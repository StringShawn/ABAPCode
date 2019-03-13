FUNCTION ZPOPUP_LISTBOX_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(BARTXT) TYPE  CHAR30
*"     REFERENCE(FIELDTXT) TYPE  CHAR20
*"     REFERENCE(FIELDNAME) TYPE  FIELDNAME
*"  EXPORTING
*"     REFERENCE(SELECT_VALUE) TYPE  STRING
*"  TABLES
*"      VALUE_TAB TYPE  TABLE
*"--------------------------------------------------------------------

  CLEAR ls_list_text.

  ls_list_text-toolbar_txt = bartxt.
  ls_list_text-fieldtxt = fieldtxt.
  ls_list_text-list_field = fieldname.
  ASSIGN value_tab[] TO <list_tab>.

  CALL SCREEN '0102' STARTING AT 20 1.

  select_value = ls_list_text-list_value.

  UNASSIGN <list_tab>.

ENDFUNCTION.
