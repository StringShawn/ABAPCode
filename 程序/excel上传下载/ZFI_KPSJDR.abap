*&---------------------------------------------------------------------*
*& Program ID    : ZFI_KPSJDR
*& Program Text  : 开票数据批导
*& Overview      : 开票数据批导
*& Created by    : HANDYXH
*& Creation Date : 2019/01/18
*&---------------------------------------------------------------------*
*& Changing No   :
*& Updated by    :
*& Update date   :
*& Changed Item Description :
*&---------------------------------------------------------------------*
REPORT zfi_kpsjdr NO STANDARD PAGE HEADING
                    LINE-SIZE   90
                    LINE-COUNT  120.
*&---------------------------------------------------------------------*
*&内表工作区的定义
*&---------------------------------------------------------------------*
DATA: BEGIN OF gs_data,
        box         TYPE c,
        field_style TYPE lvc_t_styl.
    INCLUDE STRUCTURE zfi_kpsj.
DATA: END OF gs_data.
DATA: gt_data LIKE TABLE OF gs_data.
DATA: gt_excel TYPE TABLE OF alsmex_tabline,
      gs_excel TYPE alsmex_tabline.
DATA: gs_new_line  TYPE REF TO data,
      dy_err_table TYPE REF TO data,
      dy_table     TYPE REF TO data.
DATA: g_appl TYPE ole2_object.
DATA: g_work TYPE ole2_object.
DATA: g_activesheet TYPE ole2_object.
DATA: ls_stylelin TYPE lvc_s_styl.
"ALVd定义
DATA:gt_fcat    TYPE lvc_t_fcat,
     gs_fcat    TYPE lvc_s_fcat,
     gs_layo    TYPE lvc_s_layo,
     gs_setting TYPE lvc_s_glay. "ALV布局设置

DATA l_desktopdirectory TYPE string.
*&---------------------------------------------------------------------*
*&field-symbol
*&---------------------------------------------------------------------*
FIELD-SYMBOLS: <dyn_field> TYPE any.

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
      PERFORM frm_download_template USING 'ZFI_KPSJ'.
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

  PERFORM frm_fill_data.

  PERFORM frm_display_data.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FM_GET_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fm_get_path .
  DATA: title   TYPE string VALUE '选择文件',
        ini_dir TYPE string,
        l_rc    TYPE i,
        it_tab  TYPE filetable.
  ini_dir = l_desktopdirectory.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog  "获取文件路径
    EXPORTING
      window_title            = title
      initial_directory       = ini_dir
      multiselection          = ' '
      file_filter             = 'EXCEL(*.XLS)|*.XLS|'
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
      i_end_col   = '10' " 读取多少列
      i_end_row   = '9999' "读取多少行
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
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'   "获取fieldcat
    EXPORTING
      i_structure_name       = 'ZFI_KPSJ'
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  DELETE gt_fcat WHERE fieldname = 'MANDT'.
  LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    <fs_fcat>-edit = 'X'.
  ENDLOOP.
ENDFORM.                    "FRM_BUILD_TABLE
*&---------------------------------------------------------------------*
*&      Form  FRM_ISNERT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_fill_data .
  DATA: lv_times TYPE i VALUE 1.
  DATA: lc_date TYPE sy-datum.
  DATA: lc_time TYPE sy-uzeit.
  LOOP AT gt_excel INTO gs_excel.  "把excel数据导入内表
    lv_times = gs_excel-col.
    READ TABLE gt_fcat INTO gs_fcat INDEX lv_times.
    ASSIGN COMPONENT gs_fcat-fieldname OF STRUCTURE gs_data TO <dyn_field>.
    <dyn_field> = gs_excel-value.
    AT END OF row.
      APPEND gs_data TO gt_data.
      CLEAR gs_data.
    ENDAT.
    CLEAR:gs_fcat,gs_excel.
  ENDLOOP.
  SORT gt_fcat BY fieldname.
  LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).   "设置不可编辑
    LOOP AT gt_fcat INTO gs_fcat.
      CLEAR ls_stylelin.
      ls_stylelin-fieldname = gs_fcat-fieldname.
      ls_stylelin-style = cl_gui_alv_grid=>mc_style_disabled.
      APPEND ls_stylelin TO <fs_data>-field_style.
    ENDLOOP.
  ENDLOOP.
  SORT gt_fcat BY col_pos.
ENDFORM.                    "FRM_ISNERT_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_DOWNLOAD_TEMPLATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_download_template USING VALUE(p_name).

  FIELD-SYMBOLS:<fs_field> TYPE any.
  DATA: lc_filename TYPE string VALUE 'excel导入', "默认名
        lc_fullpath TYPE string ,  "   文件名
        lc_path     TYPE  string . "   不包括文件名
  DATA: lv_index TYPE i.
  DATA: lv_exist TYPE abap_bool.
  DATA: lv_filename TYPE rlgrap-filename.
  DATA: lv_answer TYPE c.
  DATA: lv_obj_name TYPE w3objid.
  DATA: lv_objdata TYPE wwwdatatab.
  DATA: lv_subrc TYPE sy-subrc.
  DATA: lv_filepath TYPE rlgrap-filename.

  DATA:g_excel    TYPE ole2_object,
       g_applica  TYPE ole2_object,
       g_sheet    TYPE ole2_object,
       g_cell     TYPE ole2_object,
       g_workbook TYPE ole2_object.


  REFRESH:gt_fcat.
  CLEAR: lv_index.

  lc_filename = '开票数据导入模板'.
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


*  CONCATENATE l_desktopdirectory '卸货地点批导模板.xls' INTO lv_path SEPARATED BY '\'.

  lv_exist = cl_gui_frontend_services=>file_exist( lc_fullpath ).

* 模版已存在，是否覆盖
*  IF lv_exist IS NOT INITIAL.
*    CALL FUNCTION 'POPUP_TO_CONFIRM'
*      EXPORTING
*        text_question  = '模版已存在，是否覆盖？'
*        text_button_1  = '是'(001)
*        text_button_2  = '否'(002)
*      IMPORTING
*        answer         = lv_answer
*      EXCEPTIONS
*        text_not_found = 1
*        OTHERS         = 2.
*    IF sy-subrc <> 0.
*    ENDIF.
*
*    IF lv_answer NE '1'. "取消
*      EXIT.
*    ENDIF.
*  ELSE.
*    lv_answer = '1'.
*  ENDIF.


  CREATE OBJECT g_excel 'EXCEL.APPLICATION'.
  GET PROPERTY OF g_excel 'Workbooks' = g_workbook .
  CALL METHOD OF
    g_workbook
    'Close'.

*  IF lv_answer EQ '1'.
  MOVE p_name TO lv_obj_name.
  SELECT relid objid
    FROM wwwdata
    INTO  CORRESPONDING FIELDS OF lv_objdata
    UP TO 1 ROWS
    WHERE srtf2 = 0 AND relid = 'MI'
      AND objid = lv_obj_name.
  ENDSELECT.
  lv_filepath = lc_fullpath.
  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      key         = lv_objdata
      destination = lv_filepath
    IMPORTING
      rc          = lv_subrc.
  IF lv_subrc = 0.
    MESSAGE '模板下载成功' TYPE 'S'.
  ENDIF.
*  ENDIF.

  CALL METHOD OF
    g_workbook
    'open'
    EXPORTING
      #1 = lc_fullpath.

  CALL METHOD OF
      g_excel
      'worksheets' = g_sheet
    EXPORTING
      #1           = 1.
  CALL METHOD OF
    g_sheet
    'activate'.

  SET PROPERTY OF g_excel 'visible' = 1.

  FREE OBJECT g_sheet.
  FREE OBJECT g_applica.
  FREE OBJECT g_workbook.
  FREE OBJECT g_excel.
ENDFORM.                    " FRM_DOWNLOAD_TEMPLATE
*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_display_data .

  PERFORM frm_build_layout."格式
  PERFORM frm_alv_output. "输出

ENDFORM.
FORM frm_build_layout .
  CLEAR gs_layo.
  gs_layo-box_fname  = 'BOX'.
  gs_layo-cwidth_opt = 'X'.
  gs_layo-zebra      = 'X'.
  gs_layo-stylefname = 'FIELD_STYLE'.
ENDFORM.
FORM frm_alv_output .
  CLEAR gs_setting.
  gs_setting-edt_cll_cb = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'PF_STATUS_SET'
      i_callback_user_command  = 'ALV_USER_COMMAND'
      i_grid_settings          = gs_setting
      is_layout_lvc            = gs_layo
      it_fieldcat_lvc          = gt_fcat
    TABLES
      t_outtab                 = gt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
FORM alv_user_command USING r_ucomm TYPE sy-ucomm
                            rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&INSERT'."新增行
      APPEND INITIAL LINE TO gt_data.
    WHEN '&CHANGE'."修改
      LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>) WHERE box = 'X'.   "设置不可编辑
*        LOOP AT gt_fcat INTO gs_fcat.
*          CLEAR ls_stylelin.
*          ls_stylelin-fieldname = gs_fcat-fieldname.
*          ls_stylelin-style = cl_gui_alv_grid=>mc_style_enabled.
*          APPEND ls_stylelin TO <fs_data>-field_style.
*        ENDLOOP.
        CLEAR <fs_data>-field_style.
      ENDLOOP.
    WHEN '&DELE'.  "删除行
      DELETE gt_data WHERE box = 'X'.
    WHEN '&EXECUTE'."数据导入
      PERFORM frm_insert_db.
  ENDCASE.
  rs_selfield-refresh = 'X'.
ENDFORM.                    "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&引用GUI状态
*&---------------------------------------------------------------------*
FORM pf_status_set USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.

ENDFORM.                    "PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  FRM_INSERT_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_insert_db .
  DATA:gt_kpsj TYPE TABLE OF zfi_kpsj,
       gs_kpsj TYPE zfi_kpsj.
  LOOP AT gt_data INTO gs_data WHERE box = 'X'.
    MOVE-CORRESPONDING gs_data TO gs_kpsj.
    APPEND gs_kpsj TO gt_kpsj.
    CLEAR:gs_kpsj,gs_data.
  ENDLOOP.

  MODIFY zfi_kpsj FROM TABLE gt_kpsj.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    MESSAGE '数据导入成功' TYPE 'S'.
    DELETE gt_data WHERE box = 'X'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE '数据导入失败' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.