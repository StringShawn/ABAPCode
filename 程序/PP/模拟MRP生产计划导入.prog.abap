*$*$********************************************************************
* Program ID/Name:ZPPR009_1               Date written:2017.08.14
* Author's name:HANDCJW                   Last update:2017.08.14
* Program title:模拟MRP生产计划导入
* Project Name:  EPR I
* Version:
* Function Spec ID:
*----------------------------------------------------------------------*
* Description: (Incl. Related Function Area and System)
*              用户维护好生产计划导入的excel文件，上载到SAP，
*              程序依此创建MRP的计划独立需求(拷贝ZPPR009)
*----------------------------------------------------------------------*
* Include:
*
*----------------------------------------------------------------------*
* Calls: (RFC and BPI)
*
*----------------------------------------------------------------------*
* Function Modules:
*
*----------------------------------------------------------------------*
* Table:
*   ZTPPH, ZTPPI
*----------------------------------------------------------------------*
* Result:
*
*---------------------------------------------------------------------*
* Change History
*----------------------------------------------------------------------*
*     Date   |   Programmer   |   Corr. #   |   Description
*            |                |             |
*            |                |             |
*$*$********************************************************************
REPORT  zppr009_1 MESSAGE-ID zpp_msg.

*$*$********************************************************************
*$*$    TABLES                                                         *
*$*$********************************************************************
TABLES:marc.
*$*$********************************************************************
*$*$    INCLUDES                                                       *
*$*$    (FOR INCLUDES WITH DECLARATIVE PURPOSES ONLY                   *
*$*$     I.E. BDC, ALV, ETC.)                                          *
*$*$********************************************************************

*$*$********************************************************************
*$*$    GLOBAL TYPES                                                   *
*$*$********************************************************************
TYPES: BEGIN OF ty_data,
         werks LIKE ztpiri-werks,
         matnr LIKE ztpiri-matnr,
       END OF ty_data.
*$*$********************************************************************
*$*$    GLOBAL CONSTANTS                                               *
*$*$********************************************************************

*$*$********************************************************************
*$*$    GLOBAL ELEMENTARY VARIABLES                                    *
*$*$********************************************************************

DATA: g_table TYPE REF TO data.
DATA: g_dcpfm LIKE usr01-dcpfm.
DATA:g_grid TYPE REF TO cl_gui_alv_grid.
DATA g_code LIKE sy-ucomm.
*$*$********************************************************************
*$*$    GLOBAL STRUCTURES                                              *
*$*$********************************************************************
****alv structure
DATA:  wa_layout TYPE lvc_s_layo,
       wa_alv_cat  TYPE lvc_s_fcat,
       wa_table    LIKE dntab,
       wa_week     LIKE t246,
      wa_ztpph     TYPE ztpph.
*$*$********************************************************************
*$*$    GLOBAL INTERNAL TABLES                                         *
*$*$********************************************************************
DATA:it_week TYPE STANDARD TABLE OF t246.
DATA:it_ztppi TYPE STANDARD TABLE OF ztppi WITH HEADER LINE.
DATA: BEGIN OF it_date OCCURS 0,
       col   TYPE i,
       date  LIKE sy-datum,
      END OF it_date.

DATA: BEGIN OF it_error OCCURS 0,
        z_posnr LIKE ztppi-z_posnr,
        werks   LIKE ztppi-werks,
        matnr   LIKE ztppi-matnr,
        maktx   LIKE ztppi-maktx,
        message TYPE string,
END OF it_error.
*****alv field category.
DATA: it_alv_cat TYPE lvc_t_fcat.

*DATA: G_ROW      TYPE I VALUE 104."20150528hp_sjfEXCEL上传最大列数60改为104
DATA: g_col      TYPE i VALUE 255."20150731hp_sjfEXCEL上传最大列数60改为255
*$*$********************************************************************
*$*$    MACROS                                                         *
*$*$********************************************************************

*$*$********************************************************************
*$*$    GLOBAL FIELD-SYMBOLS                                           *
*$*$********************************************************************
FIELD-SYMBOLS: <dyn_table> TYPE STANDARD TABLE,
               <dyn_wa>,
               <dyn_field>.
*$*$********************************************************************
*$*$    CLASSES                                                        *
*$*$********************************************************************

*$*$********************************************************************
*$*$    PARAMETERS & SELECT-OPTIONS                                    *
*$*$********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: p_import RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND u1,
            p_query RADIOBUTTON GROUP g1 .
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_file TYPE string MODIF ID gp1,
            p_versb1 LIKE ztpirh-z_versb MODIF ID gp1,
            p_vernr1 LIKE ztpirh-z_vernr DEFAULT '1' MODIF ID gp1
                                         NO-DISPLAY,
            p_desc LIKE ztpirh-z_descr MODIF ID gp1.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECT-OPTIONS: s_werks FOR marc-werks MODIF ID gp2,
                s_matnr FOR marc-matnr MODIF ID gp2.
PARAMETERS: p_versb2 LIKE ztpirh-z_versb MODIF ID gp2,
            p_vernr2 LIKE ztpirh-z_vernr MODIF ID gp2 DEFAULT '1'
            NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b3.
*$*$********************************************************************
*$*$    INITIALIZATION                                                 *
*$*$********************************************************************


*$*$********************************************************************
*$*$    AT SELECTION-SCREEN ON                                         *
*$*$********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM frm_choose_input_file .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_versb1.
  PERFORM frm_get_search_help."F4帮助...

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_versb2.
  PERFORM frm_get_search_help."F4帮助...

*$*$********************************************************************
*$*$    AT SELECTION-SCREEN OUTPUT                                     *
*$*$********************************************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM  frm_radiobutton_select. " 单选按钮处理
*$*$********************************************************************
*$*$    AT SELECTION-SCREEN                                            *
*$*$********************************************************************
AT SELECTION-SCREEN.
  IF sy-ucomm = 'ONLI'.
    PERFORM frm_selection_inputcheck."屏幕输入检查
    PERFORM frm_get_fieldname."获取表字段类型等信息.
    IF p_import = abap_true.
*      PERFORM FRM_CHECK_AUTH."检查权限
      PERFORM frm_upload_data. " 从Excel上载数据_
    ELSE.
      PERFORM frm_get_data. "从数据库表中取得数据.
    ENDIF.
  ENDIF.
*$*$********************************************************************
*$*$    START-OF-SELECTION                                             *
*$*$********************************************************************
START-OF-SELECTION.

  PERFORM frm_display_data. "Display log data.

*$*$********************************************************************
*$*$    END-OF-SELECTION                                               *
*$*$********************************************************************
*&---------------------------------------------------------------------*
*&      Form  FRM_CHOOSE_INPUT_FILE
*&---------------------------------------------------------------------*
*       chose the input file path
*----------------------------------------------------------------------*

FORM frm_choose_input_file .

  DATA: i_fname TYPE string,
        it_l_filetable TYPE TABLE OF file_table,
        i_rc TYPE i,
        i_title TYPE string,
        i_action TYPE i.
  i_title = text-002.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = i_title
      default_filename        = '.xlsx'
      file_filter             = '*.*'
    CHANGING
      file_table              = it_l_filetable
      rc                      = i_rc
      user_action             = i_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    CLEAR i_fname.
  ELSE.
    IF i_action = 0.
      READ TABLE it_l_filetable INDEX 1 INTO i_fname.
    ELSE.
      CLEAR i_fname.
    ENDIF.
  ENDIF.
  p_file = i_fname.

ENDFORM.                    " FRM_CHOOSE_INPUT_FILE
*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*        Upload data from Excel file..
*----------------------------------------------------------------------*

FORM frm_upload_data .
  DATA:it_l_upload  TYPE STANDARD TABLE OF alsmex_tabline
                                  WITH HEADER LINE,
       wa_l_upload TYPE alsmex_tabline,
       wa_l_line   TYPE REF TO data .
  DATA:i_filename LIKE rlgrap-filename,
       i_date     LIKE sy-datum,
*       I_COL(2)   TYPE N,
       i_col(4)   TYPE n,"20150731hp_sjf调整FIELDNAME长度
       i_value    LIKE ztppi-plnmg ,
       i_pos      TYPE i VALUE '4',
       i_wotnr    TYPE  p,
       i_dismm    LIKE marc-dismm,
       i_matnr    LIKE marc-matnr,
       i_werks    LIKE marc-werks.

  DATA: BEGIN OF wa_l_ztppi,
        z_posnr LIKE ztppi-z_posnr,
          werks LIKE ztppi-werks,
          matnr LIKE ztppi-matnr,
        END OF wa_l_ztppi.
  DATA:wa_l_data TYPE ty_data,
       it_l_data TYPE STANDARD TABLE OF ty_data.
  CLEAR i_filename.
  i_filename = p_file.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = i_filename
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = g_col
      i_end_row               = 100000
    TABLES
      intern                  = it_l_upload
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE e011.
  ELSE.
    CLEAR i_col.
***convert the excel table into data internal table.
    LOOP AT  it_l_upload .
      MOVE-CORRESPONDING it_l_upload TO wa_l_upload.
      IF wa_l_upload-row = 1 AND  wa_l_upload-col > 4.
****  检查Excel 中数据列数不超过56列，总列数不超过60列

        IF wa_l_upload-col > g_col.
          MESSAGE e026.
          EXIT.
        ENDIF.

******检查日期格式....必须为YYYYMMDD
        CLEAR i_date.
        i_date = wa_l_upload-value.
        CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
          EXPORTING
            date                      = i_date
          EXCEPTIONS
            plausibility_check_failed = 1
            OTHERS                    = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'E'  NUMBER sy-msgno
                        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ELSE.
          CLEAR it_date.
          it_date-col = wa_l_upload-col.
          it_date-date =  i_date.
          APPEND it_date.
****构造内表结构
          CLEAR i_wotnr.
          CALL FUNCTION 'DAY_IN_WEEK'
            EXPORTING
              datum = i_date
            IMPORTING
              wotnr = i_wotnr.
          CLEAR wa_week.
          READ TABLE it_week INTO wa_week WITH KEY wotnr = i_wotnr.
          IF sy-subrc = 0 .
            CONCATENATE wa_l_upload-value '(' wa_week-langt ')'
            INTO   wa_alv_cat-scrtext_l .
          ENDIF.

          i_col = i_col + 1.
          CONCATENATE 'FD' i_col INTO wa_alv_cat-fieldname .
          wa_alv_cat-inttype = wa_table-inttype.
          wa_alv_cat-intlen = wa_table-intlen.
          wa_alv_cat-no_zero = abap_true.

          i_pos = i_pos + 1.
          wa_alv_cat-col_pos = i_pos .
          APPEND wa_alv_cat TO it_alv_cat.
          CLEAR wa_alv_cat.
        ENDIF.

      ELSEIF wa_l_upload-row > 1 .


        IF wa_l_upload-col > 4.
          CLEAR i_value. "将数量列转换为数字类型格式。
          CALL FUNCTION 'UNITS_STRING_CONVERT'
            EXPORTING
              units_string = wa_l_upload-value
              dcpfm        = g_dcpfm
            IMPORTING
              units        = i_value
            EXCEPTIONS
              invalid_type = 1
              OTHERS       = 2.
          IF sy-subrc NE 0.
            MESSAGE ID sy-msgid TYPE 'E'  NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

          ENDIF.
        ENDIF.
*        ENDCASE.

        IF wa_l_upload-row = 2 AND wa_l_upload-col = 1.

          SORT it_alv_cat BY col_pos.
          CALL METHOD cl_alv_table_create=>create_dynamic_table
            EXPORTING
              it_fieldcatalog = it_alv_cat
            IMPORTING
              ep_table        = g_table.
          " 用表类型指针 <dyn_table> 指向 数据对象的内容.
          ASSIGN g_table->* TO <dyn_table>.

          " 建立一个与动态内表结构相同的数据对象，且数据对象为是一个结构
          CREATE DATA wa_l_line LIKE LINE OF <dyn_table>.
          ASSIGN wa_l_line->* TO <dyn_wa>.

        ENDIF.
        CLEAR wa_alv_cat.
        READ TABLE it_alv_cat INTO wa_alv_cat
                              WITH KEY col_pos = wa_l_upload-col.
        IF sy-subrc = 0.
          ASSIGN COMPONENT wa_alv_cat-fieldname OF STRUCTURE
           <dyn_wa> TO <dyn_field>.
          IF wa_l_upload-col > 4.
            <dyn_field> = i_value.
          ELSE.
            <dyn_field> = wa_l_upload-value.
          ENDIF.

        ENDIF.

        AT END OF row.
          APPEND <dyn_wa> TO <dyn_table>.
          CLEAR <dyn_wa> .

        ENDAT.
      ENDIF.
    ENDLOOP.
    CLEAR it_ztppi[].
    LOOP AT <dyn_table> INTO <dyn_wa>.
      SORT it_alv_cat BY col_pos.
      LOOP AT it_alv_cat INTO wa_alv_cat.
        ASSIGN COMPONENT wa_alv_cat-fieldname OF STRUCTURE
         <dyn_wa> TO <dyn_field>.

        it_ztppi-z_versb = p_versb1.
        it_ztppi-z_vernr = p_vernr1.
        CASE wa_alv_cat-col_pos .
          WHEN 1.
            it_ztppi-z_posnr = <dyn_field>.
          WHEN 2.
            it_ztppi-werks = <dyn_field>.
          WHEN 3.
            it_ztppi-matnr = <dyn_field>.
            CLEAR i_matnr.
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = it_ztppi-matnr
              IMPORTING
                output       = i_matnr
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.
            TRANSLATE i_matnr TO UPPER CASE.

            it_ztppi-matnr = i_matnr.
          WHEN 4.
            it_ztppi-maktx = <dyn_field>.
          WHEN OTHERS.
            it_ztppi-plnmg = <dyn_field>.
            CLEAR it_date.
            READ TABLE it_date WITH KEY col = wa_alv_cat-col_pos.
            IF sy-subrc = 0.
              it_ztppi-pldat = it_date-date.
              it_ztppi-z_coln = wa_alv_cat-col_pos.
            ENDIF.
            APPEND it_ztppi.
        ENDCASE.
      ENDLOOP.
      CLEAR: wa_l_ztppi,wa_l_data.
      MOVE-CORRESPONDING it_ztppi TO wa_l_ztppi.

*******必输项目的字段检查，获取更新行项目表的数据.

      IF wa_l_ztppi-z_posnr IS INITIAL.
        MESSAGE e027 WITH text-004.
        EXIT.
      ENDIF.

      IF wa_l_ztppi-werks IS INITIAL.
        MESSAGE e027 WITH text-005.
        EXIT.
      ELSE.
        PERFORM frm_check_auth USING wa_l_ztppi-werks."检查权限
      ENDIF.

      IF wa_l_ztppi-matnr IS INITIAL.
        MESSAGE e027 WITH text-007.
        EXIT.
      ELSE.
        ASSIGN COMPONENT 'MATNR' OF STRUCTURE
       <dyn_wa> TO <dyn_field>.
        <dyn_field> = wa_l_ztppi-matnr.
        MODIFY <dyn_table> FROM <dyn_wa>.
      ENDIF.
      READ TABLE it_l_data INTO wa_l_data
                           WITH KEY werks = wa_l_ztppi-werks
                                    matnr = wa_l_ztppi-matnr.
      IF sy-subrc = 0."检查数据是否重复..
        MESSAGE e039 WITH wa_l_data-werks wa_l_data-matnr.
      ELSE.
        CLEAR i_dismm.
        SELECT SINGLE dismm INTO i_dismm
        FROM marc  "检查物料主数据是否存在
        WHERE matnr = wa_l_ztppi-matnr
          AND werks = wa_l_ztppi-werks.
        IF sy-subrc = 0.
          IF ( i_dismm = 'ND' ) OR ( i_dismm = 'VD' ).
            MESSAGE e033 WITH wa_l_ztppi-matnr.
            "检查工厂、物料号对应的MRP类型
          ENDIF.
        ELSE.
          MESSAGE e032 WITH wa_l_ztppi-werks wa_l_ztppi-matnr.
        ENDIF.
      ENDIF.
      CLEAR wa_l_data.
      MOVE-CORRESPONDING wa_l_ztppi TO wa_l_data .
      APPEND wa_l_data TO it_l_data.
      CLEAR: it_ztppi,wa_l_data,wa_l_ztppi.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " FRM_UPLOAD_DATA

*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       Display log data.
*----------------------------------------------------------------------*

FORM frm_display_data .

  DATA:i_repid LIKE sy-repid.
  i_repid = sy-repid.


  wa_layout-cwidth_opt = 'X'. " set optimized column width.
*  WA_LAYOUT-STYLEFNAME = 'Z_FSTYLE'. " field style.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = i_repid
      i_callback_pf_status_set = 'STATUS_SET'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout_lvc            = wa_layout
      it_fieldcat_lvc          = it_alv_cat
      i_default                = 'X'
      i_save                   = ' '
    TABLES
      t_outtab                 = <dyn_table>
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE e022.
  ENDIF.

ENDFORM.                    " FRM_DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_RADIOBUTTON_SELECT
*&---------------------------------------------------------------------*
*       Process Radio button selection
*----------------------------------------------------------------------*
FORM frm_radiobutton_select .

  LOOP AT SCREEN.
    IF p_import EQ abap_true.
      IF screen-group1 = 'GP2'.
        screen-input = 0.
      ENDIF.

    ELSEIF p_query EQ abap_true.
      IF screen-group1 = 'GP1'.
        screen-input = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " FRM_RADIOBUTTON_SELECT
*&---------------------------------------------------------------------*
*&      Form  FRM_SELECTION_INPUTCHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_selection_inputcheck .
  DATA:i_versb LIKE ztpph-z_versb.
  IF p_import = abap_true.
    IF p_file IS INITIAL OR p_versb1 IS INITIAL.
      MESSAGE e028.
    ELSE.
      SELECT SINGLE z_versb INTO i_versb FROM ztpph
      WHERE z_versb = p_versb1
        AND z_vernr = p_vernr1.
      IF sy-subrc = 0.
        MESSAGE e037 WITH p_versb1.
      ENDIF.
    ENDIF.
  ELSE.
    IF p_versb2 IS INITIAL.
      MESSAGE e038.
    ENDIF.
  ENDIF.

ENDFORM.                    " FRM_SELECTION_INPUTCHECK
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FIELDNAME
*&---------------------------------------------------------------------*
*       设置显示字段的属性
*----------------------------------------------------------------------*

FORM frm_get_fieldname .

  DATA:it_l_table  LIKE TABLE OF dntab,
       wa_l_table  LIKE dntab.
  CLEAR wa_l_table.
  CALL FUNCTION 'NAMETAB_GET'
    EXPORTING
      langu          = sy-langu
      tabname        = 'ZTPPI'
    TABLES
      nametab        = it_l_table
    EXCEPTIONS
      no_texts_found = 1.
  CLEAR it_alv_cat[].
*根据取出的字段目录生成参考字段目录
  LOOP AT it_l_table INTO wa_l_table.

    wa_alv_cat-fieldname = wa_l_table-fieldname.
    wa_alv_cat-inttype = wa_l_table-inttype.
    wa_alv_cat-fix_column = abap_true.
    wa_alv_cat-intlen = wa_l_table-intlen.
    wa_alv_cat-scrtext_l = wa_l_table-fieldtext.

    CASE wa_l_table-position .
      WHEN '0004'.
        wa_alv_cat-col_pos = '2'.
        APPEND wa_alv_cat TO it_alv_cat.
        CLEAR wa_alv_cat.
      WHEN '0005'.
        wa_alv_cat-col_pos = '3'.
        APPEND wa_alv_cat TO it_alv_cat.
        CLEAR wa_alv_cat.
      WHEN '0008'.
        wa_alv_cat-col_pos = '1'.
        APPEND wa_alv_cat TO it_alv_cat.
        CLEAR wa_alv_cat.
      WHEN '0009'.
        wa_alv_cat-col_pos = '4'.
        APPEND wa_alv_cat TO it_alv_cat.
        CLEAR wa_alv_cat.
      WHEN '0010'."数据列类型
        MOVE-CORRESPONDING wa_l_table TO wa_table.

    ENDCASE.
  ENDLOOP.
****获取数字格式
  SELECT SINGLE dcpfm INTO g_dcpfm
        FROM usr01
        WHERE bname = sy-uname.
****获取星期描述
  CALL FUNCTION 'WEEKDAY_GET'
   EXPORTING
     language                = sy-langu
*     IMPORTING
*       RETURN_CODE             =
    TABLES
      weekday                 = it_week
     EXCEPTIONS
       weekday_not_found       = 1
       OTHERS                  = 2
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    " FRM_GET_FIELDNAME

*&---------------------------------------------------------------------*
*&      Form  STATUS_SET
*&---------------------------------------------------------------------*
*    设置按钮状态.
*----------------------------------------------------------------------*

FORM status_set USING pr_extab TYPE slis_t_extab .


  DELETE pr_extab WHERE fcode = '&DATA_SAVE'.


  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING pr_extab[]
  OF PROGRAM sy-repid.
ENDFORM.                    " STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*      点击保存后的处理
*----------------------------------------------------------------------*

FORM user_command USING pr_ucomm LIKE sy-ucomm
                        pr_selfield TYPE slis_selfield .
  DATA:i_answer TYPE c.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_grid.

  CALL METHOD g_grid->check_changed_data. "Get Refresh data
  IF pr_ucomm = '&DATA_SAVE'.
    PERFORM frm_process_data.
  ENDIF.

*  CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY. "Refresh data.

  IF it_error[] IS INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = '提示'
        text_question         = text-009
        text_button_1         = '是'
*       ICON_BUTTON_1         = ' '
        text_button_2         = '否'
*       ICON_BUTTON_2         = ' '
        default_button        = '2'
        display_cancel_button = ''
      IMPORTING
        answer                = i_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF i_answer = '1'.
      LEAVE TO SCREEN 0.
    ENDIF.
  ELSE.
    CALL SCREEN 100 STARTING AT  20 5.
  ENDIF.

ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*       处理数据，并调用BAPI创建生产计划需求
*----------------------------------------------------------------------*

FORM frm_process_data .
  DATA: i_matnr   LIKE  bapisitemr-material,
        i_werks   LIKE  bapisitemr-plant,
        i_reqno   LIKE  bapisitemr-req_number,
        i_disgr   LIKE marc-disgr,
        i_reqtype LIKE  bapisitemr-requ_type,
        i_to      TYPE i,
        flg_err   TYPE c,
        i_maxdate LIKE sy-datum.

  DATA:it_l_shin TYPE STANDARD TABLE OF bapisshdin,
       it_l_ret  TYPE STANDARD TABLE OF  bapireturn1 WITH HEADER LINE,
       wa_l_shin TYPE bapisshdin,
       wa_l_item TYPE bapisitemr.

*******构造内表IT_ZTPIRH.....

  CALL FUNCTION 'ENQUEUE_EZ_ZTPPH'
    EXPORTING
      mode_ztpph     = 'E'
      mandt          = sy-mandt
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'E'  NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ELSE.

    CALL FUNCTION 'ENQUEUE_EZ_ZTPPI'
      EXPORTING
        mode_ztppi     = 'E'
        mandt          = sy-mandt
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'E'  NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.

      CLEAR wa_ztpph.
      wa_ztpph-z_versb = p_versb1.
      wa_ztpph-z_vernr =  p_vernr1.
      wa_ztpph-z_descr = p_desc.

      wa_ztpph-andat = sy-datum.
      wa_ztpph-annam = sy-uname.
      wa_ztpph-antms = sy-uzeit.


      DESCRIBE TABLE it_alv_cat LINES i_to.
      SORT it_date BY date DESCENDING.
      READ TABLE it_date INDEX 1.
      IF sy-subrc = 0.
        i_maxdate = it_date-date.
      ENDIF.
      LOOP AT <dyn_table> INTO <dyn_wa> .

        CLEAR:i_matnr,i_werks,i_reqno.
        CLEAR: wa_alv_cat,wa_l_item,it_error.
*** 取得序号
        READ TABLE it_alv_cat INTO wa_alv_cat
                          WITH KEY col_pos = 1.
        IF sy-subrc = 0.
          ASSIGN COMPONENT wa_alv_cat-fieldname OF STRUCTURE
           <dyn_wa> TO <dyn_field>.
          it_error-z_posnr = <dyn_field>.
        ENDIF.
*** 取得工厂值
        CLEAR: wa_alv_cat.
        READ TABLE it_alv_cat INTO wa_alv_cat
                          WITH KEY col_pos = 2.
        IF sy-subrc = 0.
          ASSIGN COMPONENT wa_alv_cat-fieldname OF STRUCTURE
           <dyn_wa> TO <dyn_field>.
          i_werks = <dyn_field>.
          wa_l_item-plant = i_werks.
          i_reqno = i_werks.
          wa_l_item-req_number = i_werks.
          it_error-werks = i_werks.
        ENDIF.
*****取得物料值...
        CLEAR wa_alv_cat.
        READ TABLE it_alv_cat INTO wa_alv_cat
                          WITH KEY col_pos = 3.
        IF sy-subrc = 0.
          ASSIGN COMPONENT wa_alv_cat-fieldname OF STRUCTURE
           <dyn_wa> TO <dyn_field>.
          i_matnr = <dyn_field>.
          wa_l_item-material = i_matnr .
          it_error-matnr = i_matnr.
        ENDIF.
* *** 取得物料描述
        READ TABLE it_alv_cat INTO wa_alv_cat
                          WITH KEY col_pos = 4.
        IF sy-subrc = 0.
          ASSIGN COMPONENT wa_alv_cat-fieldname OF STRUCTURE
           <dyn_wa> TO <dyn_field>.
          it_error-maktx = <dyn_field>.
        ENDIF.

*****取得数字列所有值，并写入BAPI内表.
        LOOP AT it_alv_cat INTO wa_alv_cat FROM 5 TO i_to.
          ASSIGN COMPONENT wa_alv_cat-fieldname OF STRUCTURE
               <dyn_wa> TO <dyn_field>.

          wa_l_shin-date_type = '1'.
          wa_l_shin-req_qty = <dyn_field>.
          CLEAR it_date.
          READ TABLE it_date WITH KEY col = wa_alv_cat-col_pos.
          IF sy-subrc = 0.
            wa_l_shin-req_date =  it_date-date.
          ENDIF.
          IF wa_l_shin-req_date >= sy-datum.
            APPEND wa_l_shin TO it_l_shin.
            CLEAR wa_l_shin.
          ENDIF.
        ENDLOOP.
        CLEAR :i_disgr,i_reqtype.
        SELECT SINGLE disgr INTO i_disgr
        FROM marc
        WHERE matnr = i_matnr
          AND werks = i_werks.
        IF i_disgr = '0011'.
          i_reqtype = 'BSF'.
        ELSEIF i_disgr = '0010'.
          i_reqtype = 'LSF'.
        ELSE.
          i_reqtype = 'LSF'.
        ENDIF.
********调用修改BAPI
        CALL FUNCTION 'BAPI_REQUIREMENTS_CHANGE'
          EXPORTING
            material                 = i_matnr
            plant                    = i_werks
            requirementstype         = i_reqtype
            version                  = 'T1' "'00'
            reqmtsplannumber         = i_reqno
            vers_activ               = ''
            do_commit                = space
            update_mode              = 'X'
            delete_old               = 'X'
            no_withdr                = 'X'
          TABLES
            requirements_schedule_in = it_l_shin
*           REQUIREMENTS_CHAR_IN     =
            return                   = it_l_ret.
        CLEAR it_l_ret.
        READ TABLE it_l_ret WITH KEY type = 'E'
                                     id = '6P'
                                     number = '017'.
        IF sy-subrc = 0. "生产计划需求不存在，需要新建.
          CLEAR it_l_ret[].

          wa_l_item-requ_type = i_reqtype.
          wa_l_item-version = 'T1'. "'00'
          wa_l_item-vers_activ = ''.
          CALL FUNCTION 'BAPI_REQUIREMENTS_CREATE'
            EXPORTING
              requirements_item        = wa_l_item
              do_commit                = space
              update_mode              = 'X'
            TABLES
              requirements_schedule_in = it_l_shin
              return                   = it_l_ret.
          .
          CLEAR flg_err.
****          错误处理...
          LOOP AT  it_l_ret WHERE type = 'E' OR type = 'A'.
            it_error-message = it_l_ret-message.
            APPEND it_error.
            flg_err = abap_true.
            EXIT.
          ENDLOOP.

          IF flg_err IS INITIAL.
****            没有错误，更新行项目表的标识...
            PERFORM frm_update_dbtable USING i_matnr i_werks.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
******调用MD74删除与生产计划日期有重叠的客户需求数据 added 2013/8/14..

            PERFORM frm_delete_cureq USING i_matnr i_werks i_maxdate. "

          ENDIF.

        ELSE.
          CLEAR: flg_err,it_l_ret.
          LOOP AT  it_l_ret WHERE type = 'E' OR type = 'A'.

            it_error-message = it_l_ret-message.
            APPEND it_error.
            flg_err = abap_true.
            EXIT.
          ENDLOOP.
          IF flg_err IS INITIAL.
****            没有错误，更新外挂表...
            PERFORM frm_update_dbtable USING i_matnr i_werks.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
******调用MD74删除与生产计划日期有重叠的客户需求数据 added 2013/8/14..

            PERFORM frm_delete_cureq USING i_matnr i_werks i_maxdate.
          ENDIF.
        ENDIF.
        CLEAR:it_l_ret[],it_l_shin[],wa_l_item.
      ENDLOOP.

***** 更新外挂表数据
      MODIFY ztpph FROM wa_ztpph.
      IF sy-subrc = 0.
        MODIFY ztppi FROM TABLE it_ztppi.
        IF sy-subrc NE 0.
          MESSAGE e035 WITH 'ZTPPI'.
        ENDIF.
      ELSE.
        MESSAGE e035 WITH 'ZTPPH'.
      ENDIF.

      CALL FUNCTION 'DEQUEUE_EZ_ZTPPI'
        EXPORTING
          mode_ztppi = 'E'
          mandt      = sy-mandt.

    ENDIF.
    CALL FUNCTION 'DEQUEUE_EZ_ZTPPH'
      EXPORTING
        mode_ztpph = 'E'
        mandt      = sy-mandt.

  ENDIF.

ENDFORM.                    " FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_UPDATE_DBTABLE
*&---------------------------------------------------------------------*
*      更新内表中标识
*----------------------------------------------------------------------*

FORM frm_update_dbtable USING pr_matnr pr_werks.


  LOOP AT it_ztppi WHERE matnr = pr_matnr AND werks = pr_werks.
    it_ztppi-z_pirch = 'S'.
    MODIFY  it_ztppi.
  ENDLOOP.

ENDFORM.                    " FRM_UPDATE_DBTABLE
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       从数据库中根据屏幕输入条件查询得到报表数据
*----------------------------------------------------------------------*

FORM frm_get_data .

  DATA:i_maxcol TYPE i,
       i_pos    TYPE i VALUE 4,
       i_wotnr  TYPE  p,
       i_date   LIKE sy-datum,
       i_index  TYPE i,
       i_col(4) TYPE n VALUE 4.
  CLEAR it_ztppi[].
  DATA:BEGIN OF it_l_data OCCURS 0,
        z_posnr  LIKE ztppi-z_posnr,
        z_coln   LIKE ztppi-z_coln,
        werks    LIKE ztppi-werks,
        matnr    LIKE ztppi-matnr,
        pldat    LIKE ztppi-pldat,
        maktx    LIKE ztppi-maktx,
        plnmg    LIKE ztppi-plnmg,
       END OF it_l_data.
  DATA: wa_l_data LIKE it_l_data,
      wa_l_line   TYPE REF TO data ..
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztppi
  FROM ztppi
  WHERE z_versb = p_versb2
    AND werks IN s_werks
    AND matnr IN s_matnr.
  IF it_ztppi[] IS INITIAL.
    MESSAGE e036.
  ELSE.
    SORT it_ztppi BY z_coln DESCENDING.
    READ TABLE it_ztppi INDEX 1.
    i_maxcol = it_ztppi-z_coln.

    WHILE  i_pos < i_maxcol.
      i_col = i_col + 1.
      CONCATENATE 'FD' i_col INTO wa_alv_cat-fieldname .
      wa_alv_cat-inttype = wa_table-inttype.
      wa_alv_cat-intlen = wa_table-intlen.
      wa_alv_cat-no_zero = abap_true.
      i_pos = i_pos + 1.
      wa_alv_cat-col_pos = i_pos .
      APPEND wa_alv_cat TO it_alv_cat.
      CLEAR wa_alv_cat.
    ENDWHILE.
    LOOP AT it_ztppi.
      MOVE-CORRESPONDING it_ztppi TO it_l_data.
      APPEND it_l_data.
      CLEAR it_l_data.
    ENDLOOP.
    SORT it_alv_cat BY col_pos.
*****  构造动态内表来显示数据
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = it_alv_cat
      IMPORTING
        ep_table        = g_table.
    " 用表类型指针 <dyn_table> 指向 数据对象的内容.
    ASSIGN g_table->* TO <dyn_table>.

    " 建立一个与动态内表结构相同的数据对象，且数据对象为是一个结构
    CREATE DATA wa_l_line LIKE LINE OF <dyn_table>.
    ASSIGN wa_l_line->* TO <dyn_wa>.


    SORT it_l_data BY z_posnr z_coln.
    LOOP AT it_l_data .
      MOVE-CORRESPONDING it_l_data TO wa_l_data.
      CLEAR: wa_alv_cat,i_index.
      READ TABLE it_alv_cat INTO wa_alv_cat
                WITH KEY col_pos =  wa_l_data-z_coln.
      IF sy-subrc = 0.
        i_index = sy-tabix.
        ASSIGN COMPONENT wa_alv_cat-fieldname OF STRUCTURE
      <dyn_wa> TO <dyn_field>.

        <dyn_field> = wa_l_data-plnmg.
        CLEAR: i_wotnr,i_date,wa_week.
        i_date =  wa_l_data-pldat.

        CALL FUNCTION 'DAY_IN_WEEK'
          EXPORTING
            datum = i_date
          IMPORTING
            wotnr = i_wotnr.
        READ TABLE it_week INTO wa_week WITH KEY wotnr = i_wotnr.
        IF sy-subrc = 0 .
          CONCATENATE i_date '(' wa_week-langt ')'
          INTO   wa_alv_cat-scrtext_l .
        ENDIF.

        MODIFY it_alv_cat FROM wa_alv_cat INDEX i_index.

      ENDIF.

      AT END OF z_posnr.
        CLEAR wa_alv_cat.
        READ TABLE it_alv_cat INTO wa_alv_cat
                  WITH KEY col_pos =  1.
        IF sy-subrc = 0.
          ASSIGN COMPONENT wa_alv_cat-fieldname OF STRUCTURE
        <dyn_wa> TO <dyn_field>.
          <dyn_field> = wa_l_data-z_posnr.
        ENDIF.

        CLEAR wa_alv_cat.
        READ TABLE it_alv_cat INTO wa_alv_cat
                  WITH KEY col_pos =  2.
        IF sy-subrc = 0.
          ASSIGN COMPONENT wa_alv_cat-fieldname OF STRUCTURE
        <dyn_wa> TO <dyn_field>.
          <dyn_field> = wa_l_data-werks.
        ENDIF.

        CLEAR wa_alv_cat.
        READ TABLE it_alv_cat INTO wa_alv_cat
                  WITH KEY col_pos =  3.
        IF sy-subrc = 0.
          ASSIGN COMPONENT wa_alv_cat-fieldname OF STRUCTURE
        <dyn_wa> TO <dyn_field>.
          <dyn_field> = wa_l_data-matnr.
        ENDIF.

        CLEAR wa_alv_cat.
        READ TABLE it_alv_cat INTO wa_alv_cat
                  WITH KEY col_pos =  4.
        IF sy-subrc = 0.
          ASSIGN COMPONENT wa_alv_cat-fieldname OF STRUCTURE
        <dyn_wa> TO <dyn_field>.
          <dyn_field> = wa_l_data-maktx.
        ENDIF.
        APPEND <dyn_wa> TO <dyn_table>.
        CLEAR <dyn_wa>.

      ENDAT.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " FRM_GET_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*    屏幕输出处理
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA:  i_container TYPE scrfname VALUE 'CS_ALV',
          i_grid  TYPE REF TO cl_gui_alv_grid,
          i_custom_container TYPE REF TO cl_gui_custom_container,
          it_l_field TYPE lvc_t_fcat,
          wa_l_field TYPE lvc_s_fcat,
         wa_l_layout TYPE lvc_s_layo.
  CREATE OBJECT i_custom_container
    EXPORTING
      container_name = i_container.
  CREATE OBJECT i_grid
    EXPORTING
      i_parent = i_custom_container.

  LOOP AT it_alv_cat INTO wa_alv_cat FROM 1 TO 4.
    MOVE-CORRESPONDING wa_alv_cat TO wa_l_field.
    APPEND wa_l_field TO it_l_field.
    CLEAR wa_l_field .
  ENDLOOP.

  wa_l_field-fieldname = 'MESSAGE' .
  wa_l_field-inttype = 'C'.
  wa_l_field-intlen = 220.
  wa_l_field-scrtext_l = '文本消息'.
  wa_l_field-col_pos = 5.
  APPEND wa_l_field TO it_l_field.
  CLEAR wa_l_field.
  wa_l_layout-cwidth_opt = 'X'. " set optimized column width.
  CALL METHOD i_grid->set_table_for_first_display
    EXPORTING
      is_layout       = wa_l_layout
    CHANGING
      it_fieldcatalog = it_l_field
      it_outtab       = it_error[].


ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*      屏幕输入处理
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE g_code.
    WHEN 'ENDE'.
      LEAVE PROGRAM.
    WHEN '&F12'.
      CLEAR it_error[].
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
*     do nothing
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_SEARCH_HELP
*&---------------------------------------------------------------------*
*      F4帮助
*----------------------------------------------------------------------*

FORM frm_get_search_help .

  DATA: it_l_versb LIKE ztpph OCCURS 0 WITH HEADER LINE.

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE it_l_versb
  FROM ztpph.
  SORT it_l_versb BY z_versb.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield         = 'Z_VERSB'
      dynpprog         = sy-repid
      dynpnr           = sy-dynnr
      dynprofield      = 'P_VERSB'
      value_org        = 'S'
      callback_program = sy-repid
    TABLES
      value_tab        = it_l_versb
    EXCEPTIONS
      parameter_error  = 1
      no_values_found  = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty  NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " FRM_GET_SEARCH_HELP
*&---------------------------------------------------------------------*
*&      Form  FRM_DELETE_CUREQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PR_MATNR  text
*      -->PR_WERKS  text
*      -->PR_MAXDATE  text
*----------------------------------------------------------------------*
FORM frm_delete_cureq  USING    pr_matnr
                                pr_werks
                                pr_maxdate.
  RANGES:r_matnr FOR rm60r-matnr,
         r_werks FOR rm60r-werks,
         r_versb FOR rm60r-versb.
  r_matnr-sign = 'I'.
  r_matnr-option = 'EQ'.
  r_matnr-low = pr_matnr.
  APPEND r_matnr.

  r_werks-sign = 'I'.
  r_werks-option = 'EQ'.
  r_werks-low = pr_werks.
  APPEND r_werks.

  r_versb-sign = 'I'.
  r_versb-option = 'EQ'.
  r_versb-low = '01'.
  APPEND r_versb.



  SUBMIT rm60rr20
             WITH werks IN r_werks
             WITH matnr IN r_matnr
             WITH versb IN r_versb
             WITH date1 INCL  pr_maxdate
             WITH histflag INCL 'X'
             WITH inacflag INCL space
             WITH listflag INCL space
             WITH testflag INCL space
             AND RETURN .
ENDFORM.                    " FRM_DELETE_CUREQ
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_AUTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_check_auth USING pr_werks.

*  DATA: BEGIN OF IT_L_WERKS OCCURS 0,
*         WERKS LIKE T001W-WERKS,
*        END OF IT_L_WERKS.
*  SELECT WERKS INTO CORRESPONDING FIELDS OF TABLE IT_L_WERKS
*  FROM T001W
*  WHERE WERKS IN S_WERKS.
*  LOOP AT IT_L_WERKS.
  AUTHORITY-CHECK OBJECT 'C_PPBD' " check authorization for QP01
  ID 'WERKS' FIELD pr_werks
  ID 'AKTTYP' FIELD 'H'.
  IF sy-subrc <> 0.
    MESSAGE e063 WITH sy-uname pr_werks.
    EXIT.
  ENDIF.
*  ENDLOOP.

ENDFORM.                    " FRM_CHECK_AUTH
