data: g_document    type ref to cl_bds_document_set,
      g_container   type ref to cl_gui_custom_container,
      g_control     type ref to i_oi_container_control,
      g_excel       type ref to i_oi_spreadsheet,
      g_proxy       type ref to i_oi_document_proxy,
      g_error       type ref to i_oi_error,
      g_retcode     type soi_ret_string.

data: g_okcode      type sy-ucomm,
      g_excel_line  type i.
  data: it_l_signature   type sbdst_signature,
        it_l_uris        type sbdst_uri,
        wa_l_signature   like line of it_l_signature,
        wa_l_uris        like line of it_l_uris.

 CONSTANTS: c_clssnm    type sbdst_classname    value 'ZCOOTHCOST',
            c_clstyp    type sbdst_classtype    value 'OT',
            c_objkey    type sbdst_object_key   value 'ZCOOTHCOST',
            c_default_filename type string value 'ZFSS_Other_Cost.xls'.

CALL SCREEN 9000.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS '15405'.
*  SET TITLEBAR 'xxx'.
  CHECK g_document IS INITIAL.
  PERFORM FRM_SET_EXCEL.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  case g_okcode.
    when 'BACK'
       or '%EX'
       or 'RW'.
      "Close document
      perform frm_close_document.

      leave to screen 0.
      exit.
    when 'SAVE'.
*      perform frm_download_document.

    when 'PRT'.
*      perform frm_print_document.
    when others.
  endcase.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SET_EXCEL .
 if g_document is not initial.
    exit.
  endif.

* Create container control
  call method c_oi_container_control_creator=>get_container_control
    importing
      control = g_control
      retcode = g_retcode.
  if g_retcode ne c_oi_errors=>ret_ok.
    call method c_oi_errors=>raise_message
      exporting
        type = 'E'.
  endif.

* Initialize Custom Control
  create object g_container
    exporting
      container_name = 'EXCEL_COST'. "Custom Control Name

  call method g_control->init_control
    exporting
      r3_application_name      = 'EXCEL INPLACE BDS'
      inplace_enabled          = abap_true
      inplace_scroll_documents = abap_true
      parent                   = g_container
    importing
      retcode                  = g_retcode.
  if g_retcode ne c_oi_errors=>ret_ok.
    call method c_oi_errors=>raise_message
      exporting
        type = 'E'.
  endif.

* Create object for cl_bds_document_set
  create object g_document.

* Get Document info
  clear:wa_l_signature.
  wa_l_signature-prop_name  = 'DESCRIPTION'.
  "Description of the table template in OAOR
*  wa_l_signature-prop_value = c_desc.
  append wa_l_signature to it_l_signature.

  call method g_document->get_with_url
    exporting
      classname       = c_clssnm
      classtype       = c_clstyp
      object_key      = c_objkey
    changing
      uris            = it_l_uris
               "signature       = it_l_signature
    exceptions
      nothing_found   = 1
      error_kpro      = 2
      internal_error  = 3
      parameter_error = 4
      not_authorized  = 5
      not_allowed     = 6.
  if sy-subrc ne 0.
    message e002(zfico_msg).
  endif.

  read table it_l_uris into wa_l_uris index 2.
  call method g_control->get_document_proxy
    exporting
      document_type  = 'Excel.Sheet'
    importing
      document_proxy = g_proxy
      retcode        = g_retcode.
  if g_retcode ne c_oi_errors=>ret_ok.
    call method c_oi_errors=>show_message
      exporting
        type = 'E'.
  endif.

* Open Document
  call method g_proxy->open_document
    exporting
      document_url     = wa_l_uris-uri
      open_inplace     = abap_true
      protect_document = abap_false"Protect Document initially
      open_readonly    = abap_false
    importing
      retcode          = g_retcode.
  if g_retcode ne c_oi_errors=>ret_ok.
    call method c_oi_errors=>show_message
      exporting
        type = 'E'.
  endif.

* Get Excel Interface
  call method g_proxy->get_spreadsheet_interface
    importing
      sheet_interface = g_excel
      retcode         = g_retcode.

  if g_retcode ne c_oi_errors=>ret_ok.
    call method c_oi_errors=>show_message
      exporting
        type = 'E'.
  endif.

call METHOD g_proxy->EXECUTE_MACRO
    EXPORTING
      MACRO_STRING = 'Sheet1.Add_Sheet'
    importing
        retcode          = g_retcode.
    if g_retcode ne c_oi_errors=>ret_ok.
      call method c_oi_errors=>show_message
        exporting
          type = 'E'.
    endif.

* Select area for entries to be displayed

  call METHOD g_proxy->EXECUTE_MACRO
    EXPORTING
      MACRO_STRING = 'Ä£¿é1.Sum'
      PARAM_COUNT  = 4
      param1       = 12
      param2       = 5
      param3       = 13
      param4       = 6

    importing
        retcode          = g_retcode.
    if g_retcode ne c_oi_errors=>ret_ok.
      call method c_oi_errors=>show_message
        exporting
          type = 'E'.
    endif.



ENDFORM.                    " FRM_SET_EXCEL
*&---------------------------------------------------------------------*
*&      Form  FRM_CLOSE_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CLOSE_DOCUMENT .
* Close document
  if not g_proxy is initial.
    call method g_proxy->close_document
      importing
        error   = g_error
        retcode = g_retcode.

    if g_error->has_failed = abap_true.
      call method g_error->raise_message
        exporting
          type = 'E'.
    endif.
  endif.
ENDFORM.                    " FRM_CLOSE_DOCUMENT


