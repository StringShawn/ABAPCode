FUNCTION ZPOPUP_ALV1_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(LS_DIALOG_ELEMENT) TYPE  ZDIALOGBOX_ELEMENT OPTIONAL
*"  EXPORTING
*"     REFERENCE(IM_ALV) TYPE REF TO CL_GUI_ALV_GRID
*"  TABLES
*"      IT_TABLE TYPE  TABLE
*"      IT_HEAD STRUCTURE  ZALV_HEAD
*"  CHANGING
*"     REFERENCE(CL_EVENT) TYPE REF TO ZCL_ALV_EVENT_RECEIVER
*"         OPTIONAL
*"--------------------------------------------------------------------


  REFRESH lt_fcat.


  cv_event = cl_event.

  LOOP AT it_head.
    lt_fcat-fieldname = it_head-fieldname.
    lt_fcat-scrtext_s = it_head-fieldtext.
    lt_fcat-col_opt = 'X'.
    APPEND lt_fcat.
  ENDLOOP.

  ls_dialogbox_element = ls_dialog_element.

  CREATE DATA lr_data LIKE it_table[].
  ASSIGN lr_data->* TO <lt_alv_tab>.
  <lt_alv_tab> = it_table[].

  PERFORM popup_alv.

  im_alv = cl_alv.

ENDFUNCTION.
