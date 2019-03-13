FUNCTION ZPOPUP_EDIT_VIEW_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(I_TOOLBAR_TXT) TYPE  CHAR30
*"     REFERENCE(I_READONLY) TYPE  I DEFAULT '1'
*"     REFERENCE(I_TEXT_STRING) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     REFERENCE(O_TEXT_STRING) TYPE  STRING
*"     REFERENCE(O_ANSWER) TYPE  CHAR1
*"--------------------------------------------------------------------

  ls_edit_text-readonly = i_readonly.
  ls_edit_text-text_string = i_text_string.
  ls_edit_text-toolbar_txt = i_toolbar_txt.

  CALL SCREEN '0101' STARTING AT 20 1.

  IF i_readonly = 0.
    o_text_string = ls_edit_text-text_string.
    o_answer = ls_edit_text-answer.
  ENDIF.

ENDFUNCTION.
