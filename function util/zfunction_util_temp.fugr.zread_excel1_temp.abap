FUNCTION ZREAD_EXCEL1_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(I_PATH) TYPE  RLGRAP-FILENAME
*"     REFERENCE(I_ROW) TYPE  I DEFAULT 2
*"     REFERENCE(I_COL) TYPE  I
*"  EXPORTING
*"     REFERENCE(O_TYPE) TYPE  CHAR1
*"     REFERENCE(O_TMSG) TYPE  BAPI_MSG
*"  TABLES
*"      IT_EXCEL_DATA STRUCTURE  ALSMEX_TABLINE
*"--------------------------------------------------------------------

  DATA path_str TYPE string.
  DATA pt_excel_data LIKE TABLE OF alsmex_tabline WITH HEADER LINE.

  DATA begin_row TYPE i.
  DATA end_row TYPE i.

  path_str = i_path.

  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file                 = path_str
    RECEIVING
      result               = o_type
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.

  IF sy-subrc <> 0 OR o_type <> 'X'.
    o_tmsg = '文件不存在'.
    o_type = 'E'.
    RETURN.
  ENDIF.

  begin_row = 501.
  end_row = 501.

  WHILE sy-subrc = 0.

    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename    = i_path
        i_begin_col = 1
        i_begin_row = begin_row
        i_end_col   = i_col
        i_end_row   = end_row
      TABLES
        intern      = pt_excel_data.

    IF pt_excel_data[] IS NOT INITIAL.
      begin_row = begin_row + 500.
      end_row = end_row + 500.
    ELSE.
      sy-subrc = 4.
    ENDIF.

  ENDWHILE.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename    = i_path
      i_begin_col = 1
      i_begin_row = i_row
      i_end_col   = i_col
      i_end_row   = end_row
    TABLES
      intern      = it_excel_data.

ENDFUNCTION.
