*--------------------------------------------------------------------*
* DATA DECLARATION
*--------------------------------------------------------------------*

  DATA: lo_alv            TYPE REF TO cl_gui_alv_grid,
        lo_salv           TYPE REF TO cl_salv_table.

* ------------------------------------------
* Display ALV
* ------------------------------------------

  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = lo_salv
        CHANGING
          t_table      = gt_data ).
    CATCH cx_salv_msg .
  ENDTRY.


  lo_salv->display( ).