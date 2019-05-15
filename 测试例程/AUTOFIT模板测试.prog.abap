*&---------------------------------------------------------------------*
*&      Form  FRM_DOWNLOAD_TEMPLATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_download_template .

  FIELD-SYMBOLS:<fs_field> TYPE any.
  DATA: lc_filename TYPE string VALUE 'excel导入', "默认名
      lc_fullpath TYPE string ,  "   文件名
      lc_path     TYPE  string . "   不包括文件名
  DATA: lv_index TYPE i.
  DATA: lv_fieldname TYPE string.
  DATA: lv_path TYPE string.
  DATA: lv_exist TYPE abap_bool.
  DATA: lv_filename TYPE rlgrap-filename.
  DATA: lv_answer.

  REFRESH:gt_fcat.
  CLEAR: lv_index.

  CALL METHOD cl_gui_frontend_services=>get_desktop_directory
    CHANGING
      desktop_directory = l_desktopdirectory.

  CALL METHOD cl_gui_cfw=>update_view.



  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZTSDE002'
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  lc_filename = '客户零件导入模板'.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog "调用保存对话框
  EXPORTING
    default_extension    = 'xls'
    default_file_name    = lc_filename
    file_filter          = 'EXCEL(*.XLS)|*.XLS|'
  CHANGING
    filename             = lc_filename
    path                 = lc_path
    fullpath             = lc_fullpath
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    not_supported_by_gui = 3
    OTHERS               = 4.
  IF lc_fullpath = ''.
    MESSAGE '用户取消输入' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  CREATE OBJECT g_appl 'EXCEL.APPLICATION'.
  SET PROPERTY OF g_appl 'VISIBLE' = 0.

  CALL METHOD OF
      g_appl
      'WORKBOOKS' = g_work.
  CALL METHOD OF
      g_work
      'Add'  = g_work.

  GET PROPERTY OF g_appl 'ActiveSheet' = g_activesheet.
  SET PROPERTY OF g_activesheet 'Name' = '客户物料号批导模板'.

  CLEAR lv_index.

  LOOP AT gt_fcat INTO gs_fcat WHERE fieldname <> 'MANDT'
                                 AND fieldname <> 'ELIKZ'
                                 AND fieldname <> 'ERDAT'
                                 AND fieldname <> 'ERZET'
                                 AND fieldname <> 'ERNAM'.
    ADD 1 TO lv_index.
    PERFORM fill_cell USING 1 lv_index gs_fcat-reptext 23 1 2.
  ENDLOOP.

  CALL METHOD OF
      g_appl
      'Range' = gs_cells
    EXPORTING
      #1      = 'A1'
      #2      = 'F1'.

  CALL METHOD OF
      gs_cells
      'Select'.

  GET PROPERTY OF gs_cells 'Columns' = g_columns.

  CALL METHOD OF
      g_columns
      'AUTOFIT'.

  "EXCEL#####
  CALL METHOD OF
      g_work
      'SAVEAS'

    EXPORTING
      #1       = lc_fullpath.

  "####
  CALL METHOD OF
      g_work
      'CLOSE'.

  "##EXCEL
  CALL METHOD OF
      g_appl
      'QUIT'.
  FREE OBJECT: g_appl.

  MESSAGE 'Download success!' TYPE 'S'.
ENDFORM.                    " FRM_DOWNLOAD_TEMPLATE
*&---------------------------------------------------------------------*
*&      Form  fill_cell
*&---------------------------------------------------------------------*
*       ##EXCEL###
*----------------------------------------------------------------------*
*      -->FU_ROW     行
*      -->FU_COLUMN  列
*      -->FU_VALUE   值
*      -->FU_COLOR   颜色
*      -->FU_BOLD    粗体
*      -->FU_color_v 字体颜色
*----------------------------------------------------------------------*
FORM fill_cell USING fu_row
      fu_column
      fu_value
      fu_color
      fu_bold
      fu_color_v.

  DATA: l_cell  TYPE ole2_object,
        l_color TYPE ole2_object,
        l_bold  TYPE ole2_object.

  "#######
  CALL METHOD OF
      g_appl
      'Cells' = l_cell
    EXPORTING
      #1      = fu_row
      #2      = fu_column.

  SET PROPERTY OF l_cell 'Value'       =  fu_value.

  "#######
  GET PROPERTY OF l_cell 'Interior'    = l_color.
  SET PROPERTY OF l_color 'ColorIndex' = fu_color.

  "#######
  GET PROPERTY OF l_cell 'Font' = l_bold.
  SET PROPERTY OF l_bold 'Bold' = fu_bold.

  SET PROPERTY OF l_bold 'ColorIndex' = fu_color_v.

  FREE OBJECT:l_cell ,
              l_color,
              l_bold .

ENDFORM. "fill_cell