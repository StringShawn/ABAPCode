*&*********************************************************************
*& PROGRAM NAME        : Z15405_003
*& Module Name         :
*& Apply Author        :
*& Author              :
*& Started on          :  2020-07-01
*& Transaction         : Z15405_003
*& Program type        : Report
*& Program ID          : Z15405_003
*& Program Description : 功能描述。。。。。。
*&*&*******************************************************************
*& REVISION LOG                                                       *
*&                                                                    *
*& LOG#    DATE       AUTHOR       DESCRIPTION                        *
*& ----    ----       ------       -----------                        *
*& 0001   2020-07-01  XXX     Initial Creation
*&*********************************************************************
REPORT z15405_003.

DATA: lv_fm_name            TYPE rs38l_fnam.
DATA: lv_devtype            TYPE rspoptype.
DATA: ls_control_parameters TYPE ssfctrlop,
      ls_output_options     TYPE ssfcompop,
      ls_job_output_info    TYPE ssfcrescl.
DATA: lt_pdf_line     TYPE STANDARD TABLE OF tline,
      lv_bin_filesize TYPE i,
      lv_bin_file     TYPE xstring.
DATA: lv_filename TYPE string.

*Call Function module to get function name by passing form name
CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
  EXPORTING
    formname           = 'ZSTR_TEST001'
    variant            = ' '
    direct_call        = ' '
  IMPORTING
    fm_name            = lv_fm_name
  EXCEPTIONS
    no_form            = 1
    no_function_module = 2
    OTHERS             = 3.
IF sy-subrc <> 0.

  RETURN.
ENDIF.

CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
  EXPORTING
    i_language             = '*'
  IMPORTING
    e_devtype              = lv_devtype
  EXCEPTIONS
    no_language            = 1
    language_not_installed = 2
    no_devtype_found       = 3
    system_error           = 4
    OTHERS                 = 5.
IF lv_devtype IS  INITIAL.
  CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
    EXPORTING
      i_language             = sy-langu
    IMPORTING
      e_devtype              = lv_devtype
    EXCEPTIONS
      no_language            = 1
      language_not_installed = 2
      no_devtype_found       = 3
      system_error           = 4
      OTHERS                 = 5.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
ENDIF.

*Call Function module passing input tables to form interface
ls_control_parameters-getotf = 'X'.
ls_control_parameters-no_dialog = 'X'.
ls_control_parameters-langu     = sy-langu.
ls_control_parameters-preview   = ' '.

ls_output_options-tdprinter = lv_devtype.

CALL FUNCTION lv_fm_name
  EXPORTING
    control_parameters = ls_control_parameters
    output_options     = ls_output_options
    user_settings      = 'X'
  IMPORTING
    job_output_info    = ls_job_output_info
  EXCEPTIONS
    formatting_error   = 1
    internal_error     = 2
    send_error         = 3
    user_canceled      = 4
    OTHERS             = 5.
IF sy-subrc <> 0.
ENDIF.

*Get PDF content
CALL FUNCTION 'CONVERT_OTF'
  EXPORTING
    format                = 'PDF'
    max_linewidth         = 132
  IMPORTING
    bin_filesize          = lv_bin_filesize
    bin_file              = lv_bin_file
  TABLES
    otf                   = ls_job_output_info-otfdata[]
    lines                 = lt_pdf_line
  EXCEPTIONS
    err_max_linewidth     = 1
    err_format            = 2
    err_conv_not_possible = 3
    err_bad_otf           = 4
    OTHERS                = 5.
IF sy-subrc <> 0.
ENDIF.

DATA: gt_bin    TYPE solix OCCURS 0,
      g_attsize TYPE int4.

CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
  EXPORTING
    buffer        = lv_bin_file
*   APPEND_TO_TABLE       = ' '
  IMPORTING
    output_length = g_attsize
  TABLES
    binary_tab    = gt_bin.


INCLUDE <cntn01>.
TYPE-POOLS: slis, abap, truxs.

DATA:l_obj      TYPE swc_object,
     g_filename TYPE string,
     gs_objb    TYPE borident,
     gs_obja    TYPE borident,
     gs_binrel  TYPE gbinrel,
     gt_binatt  TYPE STANDARD TABLE OF brelattr.

g_filename = 'TEST.PDF'.
DATA: l_seq TYPE i.
swc_container      l_cont.
swc_create_object  l_obj  'MESSAGE'       ''.
swc_set_element    l_cont 'NO_DIALOG'     'X'.
swc_set_element    l_cont 'DOCUMENTTITLE' g_filename.
swc_set_table      l_cont 'Content_Hex'   gt_bin.
swc_set_element    l_cont 'DOCUMENTTYPE'  'PDF'.
swc_set_element    l_cont 'DOCUMENTSIZE'  g_attsize.
swc_refresh_object l_obj.
swc_call_method    l_obj  'CREATE'        l_cont.
swc_get_object_key l_obj  gs_objb-objkey.


gs_objb-objtype = 'MESSAGE'.   "type of attach document
gs_obja-objtype = 'BKPF'.      "BO of SAP Document.
CONCATENATE    '2008'  "company code
               '0100000010' "FI Document
               '2018'  "fiscal year
               INTO
               gs_obja-objkey.

CALL FUNCTION 'BINARY_RELATION_CREATE_COMMIT'
  EXPORTING
    obj_rolea      = gs_obja
    obj_roleb      = gs_objb
    relationtype   = 'ATTA'
  IMPORTING
    binrel         = gs_binrel
  TABLES
    binrel_attrib  = gt_binatt
  EXCEPTIONS
    no_model       = 1
    internal_error = 2
    unknown        = 3
    OTHERS         = 4.
IF sy-subrc EQ 0.
  MESSAGE s043(sgos_msg).
ENDIF.