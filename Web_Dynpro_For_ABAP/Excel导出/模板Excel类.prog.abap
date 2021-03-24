METHOD filedownload .

  DATA: lr_response TYPE REF TO if_http_response.
  DATA: lv_source TYPE xstring.
  DATA: mr TYPE REF TO if_mr_api.
  DATA: url   TYPE string,
        guidc TYPE guid_22.

  DATA lt_hpzf_up TYPE wd_this->elements_hpzf_up.
  DATA lo_nd_hpzf TYPE REF TO if_wd_context_node.
  DATA lt_hpzf TYPE wd_this->elements_hpzf.
  lo_nd_hpzf = wd_context->get_child_node( name = wd_this->wdctx_hpzf ).
  lo_nd_hpzf->get_static_attributes_table( IMPORTING table = lt_hpzf ).

  CREATE OBJECT lr_response TYPE cl_http_response
    EXPORTING
      add_c_msg = 1.

  cl_wd_utilities=>construct_wd_url(
               EXPORTING
                 application_name =  wd_comp_controller->wd_get_api( )->get_component_info( )->get_name( )
               IMPORTING
                 out_local_url = url ).

  CONCATENATE url  '/ZTR_KLHP_DOWN.xlsx' INTO url.
  CONDENSE url NO-GAPS.
  mr = cl_mime_repository_api=>get_api( ).
  TRY.
      mr->get( EXPORTING  i_url     = url
      IMPORTING  e_content = lv_source ).
    CATCH cx_root INTO DATA(lr_root).
  ENDTRY.

  DATA: reader          TYPE REF TO zif_excel_reader,
        lo_excel_writer TYPE REF TO zif_excel_writer.
  DATA: lv_file      TYPE xstring.

  DATA lo_exc TYPE REF TO zcl_excel.
  DATA lo_works  TYPE REF TO zcl_excel_worksheet.

  IF lt_hpzf IS NOT INITIAL.

    MOVE-CORRESPONDING lt_hpzf TO lt_hpzf_up.

    CREATE OBJECT reader TYPE zcl_excel_reader_2007.
    DATA(lo_excel) = reader->load( i_excel2007 =  lv_source ).


    DATA is_table_settings TYPE zexcel_s_table_settings.
    is_table_settings-top_left_row = 2.
    is_table_settings-nofilters = 'X'.
    is_table_settings-show_row_stripes = 'X'.
    TRY.
        lo_excel->get_active_worksheet( )->bind_table(
        EXPORTING
          ip_table          = lt_hpzf_up
*        it_field_catalog  = it_field_catalog
          is_table_settings = is_table_settings
          ).
      CATCH zcx_excel .
    ENDTRY.

    CREATE OBJECT lo_excel_writer TYPE zcl_excel_writer_2007.
    lv_file = lo_excel_writer->write_file( lo_excel ).

  ELSE.
    lv_file = lv_source.
  ENDIF.
  DATA lo_nd_header TYPE REF TO if_wd_context_node.
  DATA lo_el_header TYPE REF TO if_wd_context_element.
  DATA ls_header TYPE wd_this->element_header.
  lo_nd_header = wd_context->get_child_node( name = wd_this->wdctx_header ).
  lo_el_header = lo_nd_header->get_element( ).
  lo_el_header->get_static_attributes(
    IMPORTING
      static_attributes = ls_header ).

  CALL METHOD cl_wd_runtime_services=>attach_file_to_response
    EXPORTING
      i_filename  = |{ ls_header-docno }-{ ls_header-bukrs }-开立汇票上传模板.xlsx| "'Excel.xlsx'
      i_content   = lv_file
      i_mime_type = 'EXCEL'.
ENDMETHOD.