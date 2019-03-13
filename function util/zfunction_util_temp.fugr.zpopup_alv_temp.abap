FUNCTION ZPOPUP_ALV_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(I_START_COL) TYPE  I
*"     REFERENCE(I_END_COL) TYPE  I OPTIONAL
*"     REFERENCE(I_START_ROW) TYPE  I
*"     REFERENCE(I_END_ROW) TYPE  I OPTIONAL
*"  TABLES
*"      IT_TAB TYPE  TABLE
*"--------------------------------------------------------------------

  DATA go_alv TYPE REF TO cl_salv_table.
  DATA go_funlst TYPE REF TO cl_salv_functions_list.
  DATA cxroot TYPE REF TO cx_root.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      =  it_tab[] ).

    CATCH  cx_root INTO cxroot.
      MESSAGE e000(oo) WITH cxroot->get_text( ).

  ENDTRY.

  go_funlst = go_alv->get_functions( ).
  go_funlst->set_all( 'X' ).

  IF go_alv IS BOUND.
    go_alv->set_screen_popup( start_column = i_start_col end_column  = i_end_col
                              start_line  = i_start_row end_line    = i_end_row ).

    go_alv->display( ).
  ENDIF.

ENDFUNCTION.
