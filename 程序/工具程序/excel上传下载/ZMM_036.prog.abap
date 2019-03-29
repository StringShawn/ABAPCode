*&---------------------------------------------------------------------*
*& Program ID    : ZMM_036
*& Program Text  : 卸货地点批导
*& Overview      : 卸货地点批导
*& Created by    : HANDYXH
*& Creation Date : 2019/01/18
*&---------------------------------------------------------------------*
*& Changing No   :
*& Updated by    :
*& Update date   :
*& Changed Item Description :
*&---------------------------------------------------------------------*
REPORT zmm_036 NO STANDARD PAGE HEADING
                    LINE-SIZE   90
                    LINE-COUNT  120.
*&---------------------------------------------------------------------*
*&内表工作区的定义
*&---------------------------------------------------------------------*

DATA: gt_excel TYPE TABLE OF alsmex_tabline,
      gs_excel TYPE alsmex_tabline.
DATA: gt_fcat TYPE lvc_t_fcat,
      gs_fcat TYPE lvc_s_fcat.
DATA: gs_new_line  TYPE REF TO data,
      dy_err_table TYPE REF TO data,
      dy_table     TYPE REF TO data.
DATA: g_appl TYPE OLE2_OBJECT.
DATA: g_work TYPE OLE2_OBJECT.
DATA: g_activesheet TYPE OLE2_OBJECT.

DATA l_desktopdirectory TYPE string.
*&---------------------------------------------------------------------*
*&field-symbol
*&---------------------------------------------------------------------*
FIELD-SYMBOLS: <dyn_table>     TYPE STANDARD TABLE,
               <dyn_err_table> TYPE STANDARD TABLE,
               <dyn_field>,
               <dyn_lines>.

*-----------------------------------------------------------------------
* 选择屏幕
*-----------------------------------------------------------------------
*PARAMETERS  p_name TYPE dd02l-tabname OBLIGATORY.
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME .
PARAMETERS  p_fname TYPE rlgrap-filename MEMORY ID xls.
SELECTION-SCREEN END OF BLOCK blk1.
SELECTION-SCREEN: PUSHBUTTON 2(10) bt2 USER-COMMAND com2.

*-----------------------------------------------------------------------
* INITIALIZATION
*-----------------------------------------------------------------------
INITIALIZATION.

  CALL METHOD cl_gui_frontend_services=>get_desktop_directory
    CHANGING
      desktop_directory = l_desktopdirectory.
  CALL METHOD cl_gui_cfw=>update_view.

  bt2 = '下载模板'.

*-----------------------------------------------------------------------
* AT SELECTION-SCREEN
*-----------------------------------------------------------------------
AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'COM2'.
      PERFORM frm_download_template.
    WHEN 'ONLI'.
      IF p_fname IS INITIAL.
        MESSAGE '请选择路径' TYPE 'E'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

*-----------------------------------------------------------------------
* AT SELECTION-SCREEN ON VALUE-REQUEST
*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM fm_get_path.


*-----------------------------------------------------------------------
* START-OF-SELECTION.
*-----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM frm_input_file.

  PERFORM frm_build_table.

  PERFORM frm_isnert_data.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FM_GET_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fm_get_path .
  DATA: title   TYPE string VALUE '选择文件',
        ini_dir TYPE string ,
        l_rc    TYPE i,
        it_tab  TYPE filetable.
  ini_dir = l_desktopdirectory.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = title
      initial_directory       = ini_dir
      multiselection          = ' '
*     FILE_FILTER             = '*.TXT'
    CHANGING
      file_table              = it_tab
      rc                      = l_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc = 0 AND l_rc = 1.
    READ TABLE it_tab INTO p_fname INDEX 1.
  ENDIF.
ENDFORM.                    "FM_GET_PATH
*&---------------------------------------------------------------------*
*&      Form  FRM_INPUT_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_input_file .
  REFRESH:gt_excel.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'    "读取excel文件中的内容
    EXPORTING
      filename    = p_fname
      i_begin_col = '1'
      i_begin_row = '2'
      i_end_col   = '100' " 读取多少列
      i_end_row   = '500' "读取多少行
    TABLES
      intern      = gt_excel.
ENDFORM.                    "FRM_INPUT_FILE
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_build_table .
  REFRESH gt_fcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZTT_EDI_XHDD'
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = gt_fcat
    IMPORTING
      ep_table        = dy_table.
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = gt_fcat
    IMPORTING
      ep_table        = dy_err_table.
  ASSIGN dy_table->* TO <dyn_table>.
  ASSIGN dy_err_table->* TO <dyn_err_table>.
  CREATE DATA gs_new_line LIKE LINE OF <dyn_table>.

*建立一个与动态内表结构相同的数据对象，且数据对象为是一个结构

  ASSIGN gs_new_line->* TO <dyn_lines>." 用<dyn_wa>指针指向该结构
ENDFORM.                    "FRM_BUILD_TABLE
*&---------------------------------------------------------------------*
*&      Form  FRM_ISNERT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_isnert_data .
  DATA: lv_times TYPE i VALUE 1.
  DATA: lc_date TYPE sy-datum.
  DATA: lc_time TYPE sy-uzeit.
  LOOP AT gt_excel INTO gs_excel.
    lv_times = gs_excel-col + 1.
    READ TABLE gt_fcat INTO gs_fcat INDEX lv_times.
    ASSIGN COMPONENT gs_fcat-fieldname OF STRUCTURE <dyn_lines> TO <dyn_field>.
    <dyn_field> = gs_excel-value.
    AT END OF row.
      APPEND <dyn_lines> TO <dyn_table>.
      CLEAR <dyn_lines>.
    ENDAT.
    CLEAR:gs_fcat,gs_excel.
  ENDLOOP.
  lc_date = sy-datum.
  lc_time = sy-uzeit.
  LOOP AT <dyn_table> ASSIGNING <dyn_lines>.
    ASSIGN COMPONENT 'MANDT' OF STRUCTURE <dyn_lines> TO <dyn_field>.
    <dyn_field> = sy-mandt.
    MODIFY ztt_edi_xhdd FROM <dyn_lines>.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
      APPEND <dyn_lines> TO <dyn_err_table>.
    ENDIF.
    CLEAR <dyn_lines>.
  ENDLOOP.
  IF <dyn_err_table> IS INITIAL.
    MESSAGE '导入成功' TYPE 'S'.
  ELSE.

  ENDIF.
ENDFORM.                    "FRM_ISNERT_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_DOWNLOAD_TEMPLATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_download_template .

  FIELD-SYMBOLS:<fs_field> TYPE any.
  DATA: lv_index TYPE i.
  DATA: lv_fieldname TYPE string.
  DATA: lv_path TYPE string.
  DATA: lv_exist TYPE abap_bool.
  DATA: lv_filename TYPE rlgrap-filename.

  REFRESH:gt_fcat.
  CLEAR: lv_index.

  CALL METHOD cl_gui_frontend_services=>get_desktop_directory
    CHANGING
      desktop_directory = l_desktopdirectory.

  CALL METHOD cl_gui_cfw=>update_view.



  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZTT_EDI_XHDD'
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.


  CONCATENATE l_desktopdirectory '卸货地点批导模板.xls' INTO lv_path SEPARATED BY '\'.

  lv_exist = cl_gui_frontend_services=>file_exist( lv_path ).

  IF lv_exist IS NOT INITIAL.
    MESSAGE '模板在桌面已存在' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
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
  SET PROPERTY OF g_activesheet 'Name' = '卸货地点批导模板'.

  CLEAR lv_index.

  LOOP AT gt_fcat INTO gs_fcat WHERE fieldname <> 'MANDT'.
    ADD 1 TO lv_index.
    PERFORM fill_cell USING 1 lv_index gs_fcat-reptext 26 1.
  ENDLOOP.

  "EXCEL#####
  CALL METHOD OF
      g_work
      'SAVEAS'

    EXPORTING
      #1       = lv_path.

  "####
  CALL METHOD OF
      g_work
      'close'.

  "##EXCEL
  CALL METHOD OF
      g_appl
      'QUIT'.
  FREE OBJECT g_appl.

  MESSAGE 'Download success!' TYPE 'S'.
ENDFORM.                    " FRM_DOWNLOAD_TEMPLATE
*&---------------------------------------------------------------------*
*&      Form  fill_cell
*&---------------------------------------------------------------------*
*       ##EXCEL###
*----------------------------------------------------------------------*
*      -->FU_ROW     #
*      -->FU_COLUMN  #
*      -->FU_VALUE   #####
*      -->FU_COLOR   #####
*      -->FU_BOLD    #####
*----------------------------------------------------------------------*
FORM fill_cell USING fu_row
      fu_column
      fu_value
      fu_color
      fu_bold.

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

ENDFORM. "fill_cell