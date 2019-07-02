*&---------------------------------------------------------------------*
*& Report  ZPPR037
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------**
* 事务代码
* 程序名称：ZPPR036
* 使用变式：
* 设计人：陈嘉炜
*设计时间：2017-08-15
*程序类型：ABAP/4 程序，报表
*输入文件：
*输出文件：
*应用类型：
*描   述：模拟MRP优先级导入
*（修改日志）------------------------------------------------------------*
*&  日志号      修改人      修改时间       修改说明       传输号码
*&  -----       ------     -------       -------        -------
*&  001         陈嘉炜       2017815       新建
*&---------------------------------------------------------------------*

REPORT zppr037 MESSAGE-ID zpp_msg.
************************************************************************
* Tables
************************************************************************
TABLES sscrfields.
*----------------------------------------------------------------------*
*
************************************************************************
*----------------------Corrections--------------------------*
*----------------------------------------------------------------------*
************************************************************************
*eject
************************************************************************
* DATA declaration
*--·输出内表以及工作区
DATA:BEGIN OF lw_xlsdata,
      plscn TYPE plpb-plscn, "计划方案
      matnr TYPE ztpp_priority-matnr, "物料编号
      zpriority TYPE ztpp_priority-zpriority, "优先级
    END OF lw_xlsdata,
    lt_xlsdata LIKE TABLE OF lw_xlsdata.

DATA:gv_fieldname TYPE vimsellist-viewfield,
     gt_seltab TYPE STANDARD TABLE OF vimsellist.

************************************************************************
* Parameters and Selection Options
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:r_search RADIOBUTTON GROUP rad DEFAULT 'X' USER-COMMAND flag,
            r_import RADIOBUTTON  GROUP rad .
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME .
PARAMETERS:p_file TYPE rlgrap-filename MODIF ID p.

SELECT-OPTIONS:
      s_plscn FOR lw_xlsdata-plscn NO-EXTENSION NO INTERVALS MODIF ID s, "计划方案
      s_matnr FOR lw_xlsdata-matnr MODIF ID s. "物料
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN FUNCTION KEY 1. "激活按钮
*eject
************************************************************************
* Initialization
************************************************************************
INITIALIZATION.
  sscrfields-functxt_01 = '下载模板'. "定义按钮

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM frm_f4_help USING 'Excel file (*.XLS;*.XLSX;*.XLSM)|*.XLS;*.XLSX;*.XLSM|' CHANGING p_file.  "文件浏览


AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_FILE' OR screen-name = 'S_PLSCN-LOW'.
      screen-required = '2'.
    ENDIF.

    CASE screen-group1.
      WHEN 'P'.
        IF r_search = 'X'.
          screen-active = '0'.
        ELSE.
          screen-active = '1'.
        ENDIF.
      WHEN 'S'.
        IF r_search = 'X'.
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
*eject
************************************************************************
* at selection screen
************************************************************************
AT SELECTION-SCREEN.
*--·在此可以对选择界面输入的条件进行判断
  CASE sscrfields-ucomm.
    WHEN 'FC01'. "下载模板
      PERFORM frm_export_xls.
    WHEN OTHERS.
  ENDCASE.
*eject
************************************************************************
* Event top of page
************************************************************************
TOP-OF-PAGE.
*eject
************************************************************************
* event Start of Selection
************************************************************************
START-OF-SELECTION.
*--·获取与相关业务数据
  IF r_search = 'X'.
    IF s_plscn IS INITIAL.
      MESSAGE '请输入计划方案！' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    gv_fieldname = 'PLSCN'.
    CALL FUNCTION 'VIEW_RANGETAB_TO_SELLIST'
      EXPORTING
        fieldname          = gv_fieldname
        append_conjunction = 'AND'
      TABLES
        sellist            = gt_seltab
        rangetab           = s_plscn.

    gv_fieldname = 'MATNR'.
    CALL FUNCTION 'VIEW_RANGETAB_TO_SELLIST'
      EXPORTING
        fieldname          = gv_fieldname
        append_conjunction = 'AND'
      TABLES
        sellist            = gt_seltab
        rangetab           = s_matnr.

    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action                         = 'U'
*       CORR_NUMBER                    = '          '
*       GENERATE_MAINT_TOOL_IF_MISSING = ' '
*       SHOW_SELECTION_POPUP           = ' '
        view_name                      = 'ZTPP_PRIORITY'
*       NO_WARNING_FOR_CLIENTINDEP     = ' '
*       RFC_DESTINATION_FOR_UPGRADE    = ' '
*       CLIENT_FOR_UPGRADE             = ' '
*       VARIANT_FOR_SELECTION          = ' '
*       COMPLEX_SELCONDS_USED          = ' '
*       CHECK_DDIC_MAINFLAG            = ' '
*       SUPPRESS_WA_POPUP              = ' '
      TABLES
        dba_sellist                    = gt_seltab
*       EXCL_CUA_FUNCT                 =
      EXCEPTIONS
        client_reference               = 1
        foreign_lock                   = 2
        invalid_action                 = 3
        no_clientindependent_auth      = 4
        no_database_function           = 5
        no_editor_function             = 6
        no_show_auth                   = 7
        no_tvdir_entry                 = 8
        no_upd_auth                    = 9
        only_show_allowed              = 10
        system_failure                 = 11
        unknown_field_in_dba_sellist   = 12
        view_not_found                 = 13
        maintenance_prohibited         = 14
        OTHERS                         = 15.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ELSE.
    IF p_file IS INITIAL.
      MESSAGE '请输入计划方案！' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    "从文本文件上传数据到内表中
    PERFORM frm_data_upload USING p_file CHANGING lt_xlsdata.
    PERFORM frm_save_data.
  ENDIF.


*eject
************************************************************************
*EVENT End-of selection
************************************************************************
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  FRM_F4_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0205   text
*      <--P_P_FILE  text
*----------------------------------------------------------------------*
FORM frm_f4_help USING file_filter TYPE string CHANGING o_fname TYPE rlgrap-filename.
  DATA: l_filetab TYPE filetable,
        l_waftab LIKE LINE OF l_filetab,
        l_rc TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = '打开文件'
*     DEFAULT_EXTENSION       =
*     DEFAULT_FILENAME        =
      file_filter             = file_filter
*     WITH_ENCODING           =
      initial_directory       = 'D:/'
*     MULTISELECTION          =
    CHANGING
      file_table              = l_filetab
      rc                      = l_rc
*     USER_ACTION             =
*     FILE_ENCODING           =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ELSE.
    READ TABLE l_filetab INTO l_waftab INDEX 1.
    o_fname = l_waftab-filename.
    CLEAR: l_filetab,
           l_waftab.
  ENDIF.
ENDFORM. " FRM_F4_HELP
*&---------------------------------------------------------------------*
*&      Form  FRM_DATA_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*      <--P_IT_RECORD  text
*----------------------------------------------------------------------*
FORM frm_data_upload USING fu_fname CHANGING fc_table TYPE STANDARD TABLE.
  DATA: t_raw_data TYPE truxs_t_text_data.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    =
      i_line_header        = 'X'
      i_tab_raw_data       = t_raw_data
      i_filename           = fu_fname
    TABLES
      i_tab_converted_data = fc_table
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM. " FRM_DATA_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  FRM_SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_save_data .
  DATA:lv_answer TYPE c.
  DATA:lt_priority TYPE TABLE OF ztpp_priority,
       lw_priority TYPE ztpp_priority.
  DATA:lt_priority_del TYPE TABLE OF ztpp_priority,
     lw_priority_del TYPE ztpp_priority.
  DATA:lv_error TYPE c.
  DATA:BEGIN OF lw_plsc,
         plscn TYPE plsc-plscn,
       END OF lw_plsc,
       lt_plsc LIKE SORTED TABLE OF lw_plsc WITH UNIQUE KEY plscn.
  DATA:BEGIN OF lw_mara,
         matnr TYPE mara-matnr,
       END OF lw_mara,
       lt_mara LIKE SORTED TABLE OF lw_mara WITH UNIQUE KEY matnr.
  DATA:lt_xlsdata_tmp LIKE TABLE OF lw_xlsdata.

  IF lt_xlsdata IS NOT INITIAL.
    SELECT
      plscn
    INTO TABLE lt_plsc
    FROM plsc
    FOR ALL ENTRIES IN lt_xlsdata
    WHERE plscn = lt_xlsdata-plscn.

    SELECT
      matnr
    INTO TABLE lt_mara
    FROM mara
    FOR ALL ENTRIES IN lt_xlsdata
    WHERE matnr = lt_xlsdata-matnr.

  ENDIF.
  LOOP AT lt_xlsdata INTO lw_xlsdata.
    IF lw_xlsdata-plscn IS INITIAL OR lw_xlsdata-matnr IS INITIAL OR lw_xlsdata-zpriority IS INITIAL.
      MESSAGE '导入数据存在空值！' TYPE 'S' DISPLAY LIKE 'E'.
      lv_error = 'X'.
      EXIT.
    ENDIF.

    READ TABLE lt_plsc WITH KEY plscn = lw_xlsdata-plscn TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      MESSAGE '不存在的计划方案' TYPE 'S' DISPLAY LIKE 'E'.
      lv_error = 'X'.
      EXIT.
    ENDIF.

    READ TABLE lt_mara WITH KEY matnr = lw_xlsdata-matnr TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      MESSAGE '不存在的物料' TYPE 'S' DISPLAY LIKE 'E'.
      lv_error = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  lt_xlsdata_tmp = lt_xlsdata.
  SORT lt_xlsdata_tmp BY plscn zpriority.
  DELETE ADJACENT DUPLICATES FROM lt_xlsdata_tmp COMPARING plscn zpriority.
  IF lines( lt_xlsdata ) <> lines( lt_xlsdata_tmp ).
      MESSAGE '同一计划方案下优先级不能重复' TYPE 'S' DISPLAY LIKE 'E'.
      lv_error = 'X'.
      EXIT.
  ENDIF.
  IF lv_error = 'X'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
  EXPORTING
*   TITLEBAR                    = ' '
*   DIAGNOSE_OBJECT             = ' '
    text_question               = '是否保存数据？'
text_button_1               = '是'
*   ICON_BUTTON_1               = ' '
text_button_2               = '否'
*   ICON_BUTTON_2               = ' '
*   DEFAULT_BUTTON              = '1'
display_cancel_button       = ''
*   USERDEFINED_F1_HELP         = ' '
*   START_COLUMN                = 25
*   START_ROW                   = 6
*   POPUP_TYPE                  =
*   IV_QUICKINFO_BUTTON_1       = ' '
*   IV_QUICKINFO_BUTTON_2       = ' '
 IMPORTING
   answer                      = lv_answer
* TABLES
*   PARAMETER                   =
EXCEPTIONS
   text_not_found              = 1
   OTHERS                      = 2
          .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


  IF lv_answer = '1'.
    IF lt_xlsdata IS NOT INITIAL.
      SELECT
        *
      INTO TABLE lt_priority_del
      FROM ztpp_priority
      FOR ALL ENTRIES IN lt_xlsdata
     WHERE plscn = lt_xlsdata-plscn.

    ENDIF.
    LOOP AT lt_xlsdata INTO lw_xlsdata.
      CLEAR lw_priority.
      MOVE-CORRESPONDING lw_xlsdata TO lw_priority.
      lw_priority-cdate = sy-datum.
      lw_priority-ctime = sy-uzeit.
      lw_priority-cuname = sy-uname.
      APPEND lw_priority TO lt_priority.
    ENDLOOP.

    DELETE ztpp_priority FROM TABLE lt_priority_del.
    IF sy-subrc <> 0 .
      ROLLBACK WORK.
      lv_error = 'X'.
    ENDIF.

    MODIFY ztpp_priority FROM TABLE lt_priority.
    IF sy-subrc <> 0 .
      ROLLBACK  WORK.
      lv_error = 'X'.
    ENDIF.

    IF lv_error IS INITIAL.
      COMMIT WORK AND WAIT.
      MESSAGE '保存成功！' TYPE 'S'.
    ELSE.
      MESSAGE '保存失败！' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.                    " FRM_SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_EXPORT_XLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_export_xls .
  DATA:lv_excel    TYPE ole2_object,
        lv_sheet    TYPE ole2_object,
        lv_cell     TYPE ole2_object,
        lv_workbook TYPE ole2_object,
        lv_line     TYPE i VALUE 0. "行号

  DATA: lv_init_path  TYPE string,
        lv_init_fname TYPE string,
        lv_path       TYPE string,
        lv_filename   TYPE string,
        lv_fullpath   TYPE string.
* 初始名称
  lv_init_fname = '模拟MRP计划优先级导入模板.xls'.
* 获取桌面路径
  CALL METHOD cl_gui_frontend_services=>get_desktop_directory
    CHANGING
      desktop_directory    = lv_init_path
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
    RETURN .
  ENDIF.

* 用户选择名称、路径
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = '指定保存文件名'
      default_extension    = 'XLS'
      default_file_name    = lv_init_fname
*     with_encoding        =
      file_filter          = cl_gui_frontend_services=>filetype_excel
*     initial_directory    = l_init_path
      prompt_on_overwrite  = 'X'
    CHANGING
      filename             = lv_filename
      path                 = lv_path
      fullpath             = lv_fullpath
*     user_action          =
*     file_encoding        =
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 5.
  IF sy-subrc = 0.
*   Implement suitable error handling here
  ENDIF.
  IF lv_fullpath IS INITIAL.
    RETURN.
  ENDIF.

  CREATE OBJECT lv_excel 'EXCEL.APPLICATION'."启动Excel
  IF sy-subrc NE 0.
    MESSAGE '启动Excel失败!' TYPE 'E'.
    RETURN.
  ENDIF.

  CALL METHOD OF
      lv_excel
      'WORKBOOKS' = lv_workbook.
*    SET PROPERTY OF lv_excel 'VISIBLE'   = 1.      "使excel 可视
  SET  PROPERTY OF lv_excel    'SHEETSINNEWWORKBOOK' = 1. "如果是读取excel文件中的内容 则是直接打开工作簿第一页
  CALL METHOD OF
      lv_workbook
      'ADD'.
  CALL METHOD OF
      lv_excel
      'WORKSHEETS' = lv_sheet
    EXPORTING
      #1           = 1.

  lv_line = lv_line + 1.

  CALL METHOD OF
      lv_excel
      'CELLS'  = lv_cell
    EXPORTING
      #1       = lv_line
      #2       = 1. "检查状态
  SET  PROPERTY OF lv_cell  'VALUE' = '计划方案'.                   "写入值

  CALL METHOD OF
      lv_excel
      'CELLS'  = lv_cell
    EXPORTING
      #1       = lv_line
      #2       = 2. "工厂
  SET  PROPERTY OF lv_cell  'VALUE' = '物料编号'.                   "写入值

  CALL METHOD OF
      lv_excel
      'CELLS'  = lv_cell
    EXPORTING
      #1       = lv_line
      #2       = 3. "产品编码
  SET  PROPERTY OF lv_cell  'VALUE' = '优先级'.                   "写入值

  GET PROPERTY OF lv_excel    'ACTIVESHEET'         = lv_sheet.          "激活工作簿
  GET PROPERTY OF lv_excel    'ACTIVEWORKBOOK'      = lv_workbook.       "激活工作区

  CALL METHOD OF
      lv_workbook
      'SAVEAS'

    EXPORTING
      #1          = lv_fullpath
      #2          = 1. "将excel文件保存
  CALL METHOD OF lv_workbook 'CLOSE'. "关闭工作区
  CALL METHOD OF lv_excel 'QUIT'. "退出excel

*WRITE:/ XLSNAME,'DONE'.                                          "退出成功，输出done

  FREE OBJECT lv_sheet.                                               "释放操作
  FREE OBJECT lv_workbook.
  FREE OBJECT lv_excel.


ENDFORM.                    " FRM_EXPORT_XLS
