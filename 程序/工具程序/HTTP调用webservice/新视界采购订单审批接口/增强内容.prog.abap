  "当采购订单审批至发送OA审核时，调用接口发送到OA
    DATA:ls_ekko   TYPE ekko,
         ls_ekpo   TYPE ekpo.

    CONSTANTS:c_bukrs_default TYPE t001-bukrs VALUE '6100'.

    SELECT SINGLE * INTO ls_ekko FROM ekko WHERE ebeln = is_ekko-ebeln.
    IF sy-subrc = 0 AND ls_ekko-bukrs = c_bukrs_default.
      IF ls_ekko-frgzu = 'X' AND is_ekko-frgzu = 'XX'.
        "调用接口
*--&gt;这个proxy class是在前面定义好的

        DATA:lo_proxy TYPE REF TO ZSHXJCO_CREATEXSJCGDD_OUT_SYN.

        DATA:lo_exception TYPE REF TO cx_root.

*--&gt;这两个数据类型可以在proxy class中的方法中找到（双击ZPRECO_Z_WS_001，进入这个类的定义）.
        DATA: ls_input  TYPE zshxjp2_createxsjcgdd_request,
              ls_item   TYPE ZSHXJASJ_CGDD_MX,
              ls_output TYPE zshxjp2_createxsjcgdd_response.

*--&gt;调用web service的方法

        DATA:lv_bedat TYPE char10.
        DATA:lv_name  TYPE THEAD-TDNAME,
             lt_lines TYPE TABLE OF tline,
             ls_line  TYPE tline,
             lv_text TYPE string..

        SELECT SINGLE name1 INTO @DATA(lv_name1) FROM lfa1 WHERE lifnr = @is_ekko-lifnr.

        lv_bedat = |{ is_ekko-bedat+0(4) }-{ is_ekko-bedat+4(2) }-{ is_ekko-bedat+6(2) }|.

        ls_input-in0 = is_ekko-bukrs.
        ls_input-in1 = is_ekko-ebeln.
        ls_input-in2 = is_ekko-bsart.
        ls_input-in3 = is_ekko-lifnr.
        ls_input-in4 = lv_name1.
        ls_input-in5 = is_ekko-zterm.
        ls_input-in6 = lv_bedat.

        lv_name = is_ekko-ebeln.

        CLEAR lt_lines.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
           CLIENT                        = SY-MANDT
            id                            = 'F01'
            language                      = sy-langu
            NAME                          = lv_name
            OBJECT                        = 'EKKO'
          TABLES
            lines                         = lt_lines
         EXCEPTIONS
           ID                            = 1
           LANGUAGE                      = 2
           NAME                          = 3
           NOT_FOUND                     = 4
           OBJECT                        = 5
           REFERENCE_CHECK               = 6
           WRONG_ACCESS_TO_ARCHIVE       = 7
           OTHERS                        = 8.
        IF sy-subrc ne 0.

        ENDIF.

        CLEAR lv_text.

        LOOP AT lt_lines INTO ls_line.
          CONCATENATE  lv_text ls_line INTO lv_text.
        ENDLOOP.

        ls_input-in7 = lv_text.

            DATA:lt_ztmm_rev_log TYPE TABLE OF ztmm_rev_log,
         ls_ztmm_rev_log TYPE ztmm_rev_log.

     CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '02'
        object                  = 'ZLOGID'
      IMPORTING
        number                  = ls_ztmm_rev_log-zlogid
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

      MOVE-CORRESPONDING is_ekko to ls_ztmm_rev_log.
      ls_ztmm_rev_log-header_txt = lv_text.
      ls_ztmm_rev_log-name1      = lv_name1.

        SELECT * INTO TABLE @DATA(lt_ekpo)
        FROM ekpo
        WHERE ebeln = @is_ekko-ebeln.

        IF lt_ekpo IS NOT INITIAL.
          SELECT matnr,
                 maktx
          INTO TABLE @DATA(lt_makt)
          FROM makt FOR ALL ENTRIES IN @lt_ekpo
          WHERE matnr = @lt_ekpo-matnr
            AND spras = @sy-langu.
        ENDIF.



        LOOP AT lt_ekpo INTO ls_ekpo.

          ls_item-ddxmh = ls_ekpo-ebelp.
          ls_item-wlbh = ls_ekpo-matnr.
          ls_item-cgddsl = ls_ekpo-menge.
          ls_item-sldw = ls_ekpo-meins.
          ls_item-cgjg = ls_ekpo-netpr.
          ls_item-sm = ls_ekpo-mwskz.


          READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = ls_ekpo-matnr.
          IF sy-subrc = 0.
            ls_item-wlmc = ls_makt-maktx.
          ENDIF.

          lv_name = is_ekko-ebeln && ls_ekpo-ebelp.

          CLEAR lt_lines.

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
             CLIENT                        = SY-MANDT
              id                            = 'F01'
              language                      = sy-langu
              NAME                          = lv_name
              OBJECT                        = 'EKPO'
            TABLES
              lines                         = lt_lines
            EXCEPTIONS
              id                            = 1
              language                      = 2
              name                          = 3
              not_found                     = 4
              object                        = 5
              reference_check               = 6
              wrong_access_to_archive       = 7
              others                        = 8.
        IF sy-subrc ne 0.

        ENDIF.
          CLEAR lv_text.

          LOOP AT lt_lines INTO ls_line.
            CONCATENATE  lv_text ls_line INTO lv_text.
          ENDLOOP.

          ls_item-xmwb = lv_text.

          APPEND ls_item TO ls_input-in9-xsj_cgdd_mx.

          MOVE-CORRESPONDING ls_ekpo to ls_ztmm_rev_log .
          ls_ztmm_rev_log-item_Txt = lv_text.
          ls_ztmm_Rev_log-maktx = ls_makt-maktx.
          APPEND ls_ztmm_rev_log to lt_ztmm_Rev_log.

        ENDLOOP.

*        TRY .
*            CREATE OBJECT lo_proxy.
*        CATCH CX_AI_SYSTEM_FAULT INTO lo_exception..
*            DATA(lv_err_text) = lo_exception->get_text( ).
*        ENDTRY.
*
*
*        TRY .
*          CALL METHOD lo_proxy->CREATEXSJCGDD_OUT_SYN
*            EXPORTING
*              input = ls_input
*            IMPORTING
*              output = ls_output.
*        CATCH CX_AI_SYSTEM_FAULT INTO lo_exception.
**          DATA(lv_err_text) = lo_exception->get_text( ).
**        CATCH cx_Ai_ INTO lo_exception.
*          lv_err_text = lo_exception->get_text( ).
*        ENDTRY.


        DATA:lr_convert TYPE REF TO zcl_xlm_convert.

        CREATE OBJECT lr_convert.

*  xml 数据
        DATA: lr_ixml     type ref to if_ixml,
              lr_document type ref to if_ixml_document,
              LV_XML_AS_STRING TYPE XSTRING,
              LV_XML_SIZE      TYPE I,
              LT_XML_AS_TABLE  TYPE DCXMLLINES,
              LV_CSTRING       TYPE STRING.

        lr_ixml         = cl_ixml=>create( ).
        lr_document = lr_ixml->create_document( ).

        lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = space i_name   = 'createxsjcgdd' i_value  = space        i_repeat = 'XsjCgdd_mx' ).
        lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'createxsjcgdd' i_name   = 'in0' i_value  = ls_input-in0 i_repeat = 'XsjCgdd_mx' ).
        lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'createxsjcgdd' i_name   = 'in1' i_value  = ls_input-in1 i_repeat = 'XsjCgdd_mx' ).
        lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'createxsjcgdd' i_name   = 'in2' i_value  = ls_input-in2 i_repeat = 'XsjCgdd_mx' ).
        lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'createxsjcgdd' i_name   = 'in3' i_value  = ls_input-in3 i_repeat = 'XsjCgdd_mx' ).
        lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'createxsjcgdd' i_name   = 'in4' i_value  = ls_input-in4 i_repeat = 'XsjCgdd_mx' ).
        lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'createxsjcgdd' i_name   = 'in5' i_value  = ls_input-in5 i_repeat = 'XsjCgdd_mx' ).
        lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'createxsjcgdd' i_name   = 'in6' i_value  = ls_input-in6 i_repeat = 'XsjCgdd_mx' ).
        lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'createxsjcgdd' i_name   = 'in7' i_value  = ls_input-in7 i_repeat = 'XsjCgdd_mx' ).
        lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'createxsjcgdd' i_name   = 'in8' i_value  = ls_input-in8 i_repeat = 'XsjCgdd_mx' ).
        lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'createxsjcgdd' i_name   = 'in9' i_value  = space i_repeat = 'XsjCgdd_mx' ).
        LOOP AT ls_input-in9-xsj_cgdd_mx INTO ls_item.
          lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'in9' i_name   = 'XsjCgdd_mx' i_value  = space i_repeat = 'XsjCgdd_mx' ).
          lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'XsjCgdd_mx' i_name   = 'cgddsl' i_value  = ls_item-cgddsl i_repeat = 'XsjCgdd_mx' ).
          lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'XsjCgdd_mx' i_name   = 'cgjg'   i_value  = ls_item-cgjg   i_repeat = 'XsjCgdd_mx' ).
          lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'XsjCgdd_mx' i_name   = 'ddxmh ' i_value  = ls_item-ddxmh  i_repeat = 'XsjCgdd_mx' ).
          lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'XsjCgdd_mx' i_name   = 'sldw  ' i_value  = ls_item-sldw   i_repeat = 'XsjCgdd_mx' ).
          lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'XsjCgdd_mx' i_name   = 'sm'     i_value  = ls_item-sm     i_repeat = 'XsjCgdd_mx' ).
          lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'XsjCgdd_mx' i_name   = 'wlbh'   i_value  = ls_item-wlbh   i_repeat = 'XsjCgdd_mx' ).
          lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'XsjCgdd_mx' i_name   = 'wlmc'   i_value  = ls_item-wlmc   i_repeat = 'XsjCgdd_mx' ).
          lr_convert->create_xml_element( EXPORTING ir_document = lr_document  i_parent = 'XsjCgdd_mx' i_name   = 'xmwb'   i_value  = ls_item-xmwb   i_repeat = 'XsjCgdd_mx' ).
        ENDLOOP.

      CALL FUNCTION 'SDIXML_DOM_TO_XML'
        EXPORTING
          DOCUMENT      = LR_DOCUMENT
        IMPORTING
          XML_AS_STRING = LV_XML_AS_STRING
          SIZE          = LV_XML_SIZE
        TABLES
          XML_AS_TABLE  = LT_XML_AS_TABLE
        EXCEPTIONS
          NO_DOCUMENT   = 1
          OTHERS        = 2.
      LV_CSTRING = CL_PROXY_SERVICE=>XSTRING2CSTRING( LV_XML_AS_STRING ).
      DATA:lv_response TYPE string.

      lr_convert->REQUEST_POST( EXPORTING REQUEST = LV_XML_AS_STRING interface_name = 'ZENCH_ME28_RELEASE'  IMPORTING response =  lv_response ).
  types:
    BEGIN OF type_s_key_value.
    TYPES:fname  TYPE string.
    TYPES:fvalue TYPE string.
    TYPES:END OF type_s_key_value .
  types:
    type_t_key_value TYPE STANDARD TABLE OF type_s_key_value WITH DEFAULT KEY .
    DATA:lv_parser     TYPE REF TO if_ixml_parser.
    DATA:lv_document   TYPE REF TO if_ixml_document.
    DATA:element       TYPE REF TO if_ixml_element.
    DATA:lt_key_value  TYPE type_t_key_value.
    DATA:ls_key_value  TYPE type_s_key_value.
    DATA:lv_type TYPE bapi_mtype,
         lv_message TYPE bapi_msg.

    lr_convert->convert_xlm_to_document( EXPORTING content = lv_response IMPORTING parser = lv_parser document = lv_document ).

    CLEAR:element.
    CALL METHOD lv_document->find_from_path
      EXPORTING
        path = '/SOAP:Envelope/SOAP:Body/ns1:createxsjcgddResponse/ns1:out'
      RECEIVING
        rval = element.

    lr_convert->transformation( EXPORTING element = element IMPORTING key_value = lt_key_value ).

    READ TABLE lt_key_value INTO ls_key_value WITH KEY fname = 'type'.
    IF sy-subrc EQ 0.
      lv_type = ls_key_value-fvalue.
    ENDIF.

    READ TABLE lt_key_value INTO ls_key_value WITH KEY fname = 'message'.
    IF sy-subrc EQ 0.
      lv_message = ls_key_value-fvalue.
    ENDIF.

    LOOP AT lt_ztmm_Rev_log INTO ls_ztmm_rev_log.
      ls_ztmm_rev_log-message = lv_message.
      ls_ztmm_rev_log-type = lv_type.
      MODIFY lt_ztmm_rev_log FROM ls_ztmm_rev_log.
    ENDLOOP.

    MODIFY ztmm_Rev_log FROM TABLE lt_ztmm_Rev_log.

      ENDIF.
    ENDIF.