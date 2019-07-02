*&---------------------------------------------------------------------*
* Program ID/Name: ZPPR008            Date written:2013/07/16
* Author's name:   HP_TC              Last update:
* Program title:  工艺路线主数据批量导入
* Project Name:  ZFSS
* Version:
* Function Spec ID:
*----------------------------------------------------------------------*
* Description: 工艺路线主数据批量导入
*
*----------------------------------------------------------------------*

REPORT  zppr008.

*$*$********************************************************************
*$*$    Global Types                                                   *
*$*$********************************************************************
TYPES:
  BEGIN OF t_up_datac,
    werks(4)     TYPE  c,   "工厂
    VERWE(3)     TYPE  c,   "用途
    matnr(18)    TYPE  c,   "物料号
    maktx(40)    TYPE  c,   "物料描述
    plnnr(8)     TYPE  c,   "工艺路线组
    plnal(4)     TYPE  c,   "工艺路线计数器
    KTEXT(40)    TYPE  c,   "工艺路线描述
    vornr(4)     TYPE  c,   "工序序号
    ARBID(8)     TYPE  c,   "工作中心
    steus(4)     TYPE  c,   "控制码
    ltxa1(40)    TYPE  c,   "工序描述
    vgw01(19)    TYPE  c,   "人工时间（分钟）
    vgw02(19)    TYPE  c,   "机器时间（分钟）
  END OF t_up_datac,
  BEGIN OF t_up_data,
    werks     TYPE  PLKO-werks,   "工厂
    VERWE     TYPE  PLKO-VERWE,   "用途
    matnr     TYPE  MARA-MATNR,   "物料号
    maktx     TYPE  makt-maktx,   "物料描述
    plnnr     TYPE  PLKO-PLNNR,   "工艺路线组
    plnal     TYPE  PLKO-PLNAL,   "工艺路线计数器
    KTEXT     TYPE  PLKO-KTEXT,   "工艺路线描述
    vornr     TYPE  PLPO-vornr,   "工序序号
    ARBID     TYPE  PLPO-ARBID,   "工作中心
    steus     TYPE  PLPO-steus,   "控制码
    ltxa1     TYPE  PLPO-LTXA1,   "工序描述
    vgw01     TYPE  PLPO-vgw01,   "人工时间（分钟）
    vgw02     TYPE  PLPO-vgw02,   "机器时间（分钟）
  END OF t_up_data,
  BEGIN OF t_msg,
    msg_typ(01)   TYPE  c,
    msg_lcd       TYPE icon_d,
    msg_txt(255),

    werks(4)     TYPE  c,   "工厂
    VERWE(3)     TYPE  c,   "用途
    matnr(18)    TYPE  c,   "物料号
    maktx(40)    TYPE  c,   "物料描述
    plnnr(8)     TYPE  c,   "工艺路线组
    plnal(4)     TYPE  c,   "工艺路线计数器
    KTEXT(40)    TYPE  c,   "工艺路线描述
    vornr(4)     TYPE  c,   "工序序号
    ARBID(8)     TYPE  c,   "工作中心
    steus(4)     TYPE  c,   "控制码
    ltxa1(40)    TYPE  c,   "工序描述
    vgw01(19)    TYPE  c,   "人工时间（分钟）
    vgw02(19)    TYPE  c,   "机器时间（分钟）
  END OF t_msg.

*$*$********************************************************************
*$*$    GLOBAL CONSTANTS                                               *
*$*$********************************************************************
CONSTANTS:
  c_x(1)      TYPE c             VALUE 'X',
  c_verwe_1   TYPE PLKO-VERWE    VALUE '1',
  c_verwe_2   TYPE PLKO-VERWE    VALUE '2'.

*$*$********************************************************************
*$*$    GLOBAL ELEMENTARY VARIABLES                                    *
*$*$********************************************************************
DATA:
  g_pos TYPE i.

*$*$********************************************************************
*$*$    GLOBAL INTERNAL TABLES                                         *
*$*$********************************************************************
DATA:
  it_up_datac   TYPE STANDARD TABLE OF t_up_datac,
  it_msg        TYPE STANDARD TABLE OF t_msg,
  it_alv_fldcat   TYPE  slis_t_fieldcat_alv.

*$*$********************************************************************
*$*$    GLOBAL STRUCTURES                                              *
*$*$********************************************************************


*$*$********************************************************************
*$*$    PARAMETERS & SELECT-OPTIONS                                    *
*$*$********************************************************************
SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:
          p_file  TYPE string OBLIGATORY, "rlgrap-filename
          p_datum TYPE sy-datum OBLIGATORY.

SELECTION-SCREEN:END OF BLOCK b1.
SELECTION-SCREEN:BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS:
          p_test AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN:END OF BLOCK b2.

*$*$********************************************************************
*$*$    Initialization                                                 *
*$*$********************************************************************
INITIALIZATION.
  p_datum = SY-DATUM.
  p_datum+4(4) = '0101'.

*$*$********************************************************************
*$*$    AT SELECTION-SCREEN ON                                         *
*$*$********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM frm_f4_file.

*$*$********************************************************************
*$*$    AT SELECTION-SCREEN  Output                                    *
*$*$********************************************************************


*$*$********************************************************************
*$*$    AT SELECTION-SCREEN                                            *
*$*$********************************************************************

*$*$********************************************************************
*$*$    START-OF-SELECTION                                             *
*$*$********************************************************************
START-OF-SELECTION.

  PERFORM frm_main_process.

*&---------------------------------------------------------------------*
*&      Form  FRM_F4_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_f4_file .


  DATA: i_fname TYPE string,
        it_l_filetable TYPE TABLE OF file_table,
        i_rc TYPE i,
        i_title TYPE string,
        i_action TYPE i.

  i_title = text-003.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = i_title
      default_filename        = '.txt'
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


ENDFORM.                    " FRM_F4_FILE
*&---------------------------------------------------------------------*
*&      Form  FRM_MAIN_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form FRM_MAIN_PROCESS .

* 上传数据至内表
  REFRESH:it_up_datac.
  PERFORM frm_upload_data.

* 检查并处理数据
  REFRESH:it_msg.
  PERFORM frm_process_data.

* 展示message
  PERFORM frm_output_msg.

endform.                    " FRM_MAIN_PROCESS
*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form FRM_UPLOAD_DATA .

  DATA:i_file TYPE  STRING.

  CLEAR:i_file.
  i_file = p_file.
  REFRESH it_up_datac.

  "上载
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME                      = i_file
*     FILETYPE                      = 'ASC'
      HAS_FIELD_SEPARATOR           = 'X'
*     HEADER_LENGTH                 = 0
*     READ_BY_LINE                  = 'X'
*     DAT_MODE                      = ' '
*   IMPORTING
*     FILELENGTH                    =
*     HEADER                        =
    TABLES
      DATA_TAB                      = it_up_datac
   EXCEPTIONS
     FILE_OPEN_ERROR               = 1
     FILE_READ_ERROR               = 2
     NO_BATCH                      = 3
     GUI_REFUSE_FILETRANSFER       = 4
     INVALID_TYPE                  = 5
     NO_AUTHORITY                  = 6
     UNKNOWN_ERROR                 = 7
     BAD_DATA_FORMAT               = 8
     HEADER_NOT_ALLOWED            = 9
     SEPARATOR_NOT_ALLOWED         = 10
     HEADER_TOO_LONG               = 11
     UNKNOWN_DP_ERROR              = 12
     ACCESS_DENIED                 = 13
     DP_OUT_OF_MEMORY              = 14
     DISK_FULL                     = 15
     DP_TIMEOUT                    = 16
     OTHERS                        = 17.
  IF sy-subrc NE 0.
    MESSAGE s011(zpp_msg) DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

endform.                    " FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form FRM_PROCESS_DATA .

  DATA:
    wa_l_up_datac   TYPE t_up_datac,
    wa_l_up_data    TYPE t_up_data,
    it_l_up_data    TYPE STANDARD TABLE OF t_up_data,
    wa_l_up_data_ok TYPE t_up_data,
    it_l_up_data_ok TYPE STANDARD TABLE OF t_up_data,
    wa_l_task       TYPE BAPI1012_TSK_C,
    it_l_task       TYPE STANDARD TABLE OF BAPI1012_TSK_C,
    wa_l_mat        TYPE BAPI1012_MTK_C,
    it_l_mat        TYPE STANDARD TABLE OF BAPI1012_MTK_C,
    wa_l_opr        TYPE BAPI1012_OPR_C,
    it_l_opr        TYPE STANDARD TABLE OF BAPI1012_OPR_C,
    i_test          TYPE BAPIFLAG,
    i_GROUP         TYPE BAPI1012_TSK_C-TASK_LIST_GROUP,
    i_GROUPCOUNTER  TYPE BAPI1012_TSK_C-GROUP_COUNTER,
    wa_l_return     TYPE BAPIRET2,
    it_l_return     TYPE STANDARD TABLE OF BAPIRET2,
    i_msgtyp(1)     TYPE c,
    i_msgtxt(255)   TYPE c,
    i_txt(255)      TYPE c,
    wa_l_msg        TYPE t_msg,
    wa_l_msg_tmp    TYPE t_msg,
    wa_l_t001w      TYPE t001w,
    i_maktx         TYPE makt-maktx,
    i_MEINS         TYPE mara-MEINS,
    i_PLNNR         TYPE mapl-PLNNR,
    wa_l_mapl       TYPE mapl,
    it_l_mapl       TYPE STANDARD TABLE OF mapl,
    wa_l_plko       TYPE plko,
    it_l_plko       TYPE STANDARD TABLE OF plko,
    i_arbpl         TYPE crhd-arbpl,
    wa_l_crhd       TYPE crhd,
    i_idx1          TYPE sy-tabix,
    i_idx2          TYPE sy-tabix,
    i_flg_dup       TYPE c.
  DATA:
    BEGIN OF wa_l_used1,
      werks     TYPE  PLKO-werks,   "工厂
      VERWE     TYPE  PLKO-VERWE,   "用途
      matnr     TYPE  MARA-MATNR,   "物料号
      PLNAL     TYPE  plko-PLNAL,
    END OF wa_l_used1,
    it_l_used1  LIKE STANDARD TABLE OF wa_l_used1,
    BEGIN OF wa_l_used2,
      werks     TYPE  PLKO-werks,   "工厂
      VERWE     TYPE  PLKO-VERWE,   "用途
      plnnr     TYPE  PLKO-PLNNR,   "工艺路线组
      PLNAL     TYPE  plko-PLNAL,
    END OF wa_l_used2,
    it_l_used2  LIKE STANDARD TABLE OF wa_l_used2.


* 依次处理上传的每一条数据
  REFRESH:it_msg.
  LOOP AT it_up_datac INTO wa_l_up_datac.

    CLEAR:i_idx1.
    i_idx1 = sy-tabix.

    CLEAR:wa_l_up_data,
          wa_l_msg.

    TRANSLATE wa_l_up_datac-matnr TO UPPER CASE.
    TRANSLATE wa_l_up_datac-plnnr TO UPPER CASE.

    "所有的外部比较都使用同样的格式
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input         = wa_l_up_datac-matnr
      IMPORTING
        OUTPUT        = wa_l_up_datac-matnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input         = wa_l_up_datac-plnnr
      IMPORTING
        OUTPUT        = wa_l_up_datac-plnnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input         = wa_l_up_datac-plnal
      IMPORTING
        OUTPUT        = wa_l_up_datac-plnal.
    CALL FUNCTION 'CONVERSION_EXIT_NUMCV_OUTPUT'
      EXPORTING
        input         = wa_l_up_datac-vornr
      IMPORTING
        OUTPUT        = wa_l_up_datac-vornr.

    "赋值
    MOVE:wa_l_up_datac-werks  to wa_l_msg-werks,
         wa_l_up_datac-VERWE  to wa_l_msg-VERWE,
         wa_l_up_datac-matnr  to wa_l_msg-matnr,
         wa_l_up_datac-maktx  to wa_l_msg-maktx,
         wa_l_up_datac-plnnr  to wa_l_msg-plnnr,
         wa_l_up_datac-plnal  to wa_l_msg-plnal,
         wa_l_up_datac-ktext  to wa_l_msg-ktext,
         wa_l_up_datac-vornr  to wa_l_msg-vornr,
         wa_l_up_datac-ARBID  to wa_l_msg-ARBID,
         wa_l_up_datac-steus  to wa_l_msg-steus,
         wa_l_up_datac-ltxa1  to wa_l_msg-ltxa1,
         wa_l_up_datac-vgw01  to wa_l_msg-vgw01,
         wa_l_up_datac-vgw02  to wa_l_msg-vgw02.

    "必须性检查
    IF wa_l_up_datac-werks IS INITIAL.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m02 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_l_up_datac-VERWE IS INITIAL.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m03 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_l_up_datac-PLNAL IS INITIAL.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m12 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_l_up_datac-vornr IS INITIAL.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m04 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_l_up_datac-ARBID IS INITIAL.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m05 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_l_up_datac-steus IS INITIAL.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m06 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_l_up_datac-ltxa1 IS INITIAL.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m07 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.

    "数据格式检查
    CATCH SYSTEM-EXCEPTIONS CONVT_NO_NUMBER = 1
                           OTHERS = 8.
      MOVE:wa_l_up_datac-werks  to wa_l_up_data-werks,
           wa_l_up_datac-VERWE  to wa_l_up_data-VERWE,
           wa_l_up_datac-matnr  to wa_l_up_data-matnr,
*           wa_l_up_datac-maktx  to wa_l_up_data-maktx,
           wa_l_up_datac-plnnr  to wa_l_up_data-plnnr,
           wa_l_up_datac-plnal  to wa_l_up_data-plnal,
           wa_l_up_datac-ktext  to wa_l_up_data-ktext,
           wa_l_up_datac-vornr  to wa_l_up_data-vornr,
           "wa_l_up_datac-ARBID  to wa_l_up_data-ARBID,
           wa_l_up_datac-steus  to wa_l_up_data-steus,
           wa_l_up_datac-ltxa1  to wa_l_up_data-ltxa1,
           wa_l_up_datac-vgw01  to wa_l_up_data-vgw01,
           wa_l_up_datac-vgw02  to wa_l_up_data-vgw02.
      WRITE wa_l_up_datac-ARBID  to wa_l_up_data-ARBID LEFT-JUSTIFIED.
    ENDCATCH.
    IF sy-subrc NE 0.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m01 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.

    "判断是否是3
    IF wa_l_up_data-VERWE = '3'.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m20 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.

    "然后判断工厂、用途、物料、组、计数器、工序6个字段是否有重复
    CLEAR:i_flg_dup.
    LOOP AT it_up_datac INTO wa_l_up_datac
      WHERE werks = wa_l_up_datac-werks
        AND VERWE = wa_l_up_datac-VERWE
        AND matnr = wa_l_up_datac-matnr
        AND plnnr = wa_l_up_datac-plnnr
        AND plnal = wa_l_up_datac-plnal
        AND vornr = wa_l_up_datac-vornr.
      CLEAR:i_idx2.
      i_idx2 = sy-tabix.
      IF i_idx1 = i_idx2.
        CONTINUE.
      ELSE.
        i_flg_dup = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF i_flg_dup = 'X'.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m18 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.


    "然后判断，这一组中是否已有错识数据
    IF wa_l_up_datac-matnr IS NOT INITIAL.
      READ TABLE it_msg INTO wa_l_msg_tmp
        WITH KEY werks = wa_l_up_datac-werks
                 VERWE = wa_l_up_datac-VERWE
                 matnr = wa_l_up_datac-matnr
                 plnal = wa_l_up_datac-plnal.
      IF sy-subrc = 0.
        "说明已有一个工序出错
        MOVE 'W' TO wa_l_msg-msg_typ.
        MOVE text-m16 TO wa_l_msg-msg_txt.
        APPEND wa_l_msg TO it_msg.
        CONTINUE.
      ENDIF.
    ENDIF.
    IF wa_l_up_datac-plnnr IS NOT INITIAL.
      READ TABLE it_msg INTO wa_l_msg_tmp
        WITH KEY werks = wa_l_up_datac-werks
                 plnnr = wa_l_up_datac-plnnr
                 plnal = wa_l_up_datac-plnal.
      IF sy-subrc = 0.
        "说明已有一个工序出错
        MOVE 'W' TO wa_l_msg-msg_typ.
        MOVE text-m16 TO wa_l_msg-msg_txt.
        APPEND wa_l_msg TO it_msg.
        CONTINUE.
      ENDIF.
    ENDIF.

    "工厂合法性检查
    CLEAR:wa_l_t001w.
    SELECT SINGLE werks
      FROM t001w
      INTO CORRESPONDING FIELDS OF wa_l_t001w
     WHERE werks = wa_l_up_data-WERKS.
    IF sy-subrc <> 0.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m00 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.

    "不论何种用途，物料号和工艺路线组不同时为空
    IF wa_l_up_data-matnr IS INITIAL
    AND ( wa_l_up_data-plnnr IS INITIal ).
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m10 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.

    "数据转换
    IF wa_l_up_data-matnr IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input              = wa_l_up_data-matnr
        IMPORTING
          OUTPUT             = wa_l_up_data-matnr
        EXCEPTIONS
          LENGTH_ERROR       = 1
          OTHERS             = 2.

      CLEAR:i_maktx.
      SELECT SINGLE maktx
        FROM makt
        INTO i_maktx
       WHERE matnr = wa_l_up_data-matnr
         AND SPRAS = sy-langu.
      MOVE: i_maktx TO wa_l_msg-maktx,
            i_maktx TO wa_l_up_data-maktx.
    ENDIF.
    IF wa_l_up_data-plnnr IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input         = wa_l_up_data-plnnr
        IMPORTING
          OUTPUT        = wa_l_up_data-plnnr.
    ENDIF.
    IF wa_l_up_data-PLNAL IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input         = wa_l_up_data-PLNAL
        IMPORTING
          OUTPUT        = wa_l_up_data-PLNAL.
    ENDIF.

    "检查工艺路线存在
    IF wa_l_up_datac-matnr IS NOT INITIAL.
      REFRESH:it_l_mapl.
      SELECT plnnr
             PLNAL
        FROM mapl
        INTO CORRESPONDING FIELDS OF TABLE it_l_mapl
       WHERE MATNR = wa_l_up_data-matnr
         AND WERKS = wa_l_up_data-WERKS
         AND PLNAL = wa_l_up_data-PLNAL.
      IF sy-subrc = 0.
        SORT it_l_mapl BY plnnr PLNAL.
        DELETE ADJACENT DUPLICATES FROM it_l_mapl COMPARING plnnr PLNAL.
        REFRESH:it_l_plko.
        SELECT plnnr
               PLNAL
               VERWE
          FROM plko
          INTO CORRESPONDING FIELDS OF TABLE it_l_plko
           FOR ALL ENTRIES IN it_l_mapl
         WHERE plnnr = it_l_mapl-plnnr
           AND PLNAL = it_l_mapl-PLNAL
           AND VERWE = wa_l_up_data-VERWE
           AND WERKS = wa_l_up_data-WERKS.
        IF sy-subrc = 0.
          MOVE 'E' TO wa_l_msg-msg_typ.
          MOVE text-m14 TO wa_l_msg-msg_txt.
          APPEND wa_l_msg TO it_msg.
          CONTINUE.
        ENDIF.

      ENDIF.
    ENDIF.
    IF wa_l_up_data-plnnr IS NOT INITIAL.
      REFRESH:it_l_plko.
      SELECT plnnr
             PLNAL
             VERWE
        FROM plko
        INTO CORRESPONDING FIELDS OF TABLE it_l_plko
       WHERE plnnr = wa_l_up_data-plnnr
         AND PLNAL = wa_l_up_data-PLNAL
         AND VERWE = wa_l_up_data-VERWE
         AND WERKS = wa_l_up_data-WERKS.
      IF sy-subrc = 0.
        MOVE 'E' TO wa_l_msg-msg_typ.
        MOVE text-m14 TO wa_l_msg-msg_txt.
        APPEND wa_l_msg TO it_msg.
        CONTINUE.
      ENDIF.

    ENDIF.

    "检查工作中心
    CLEAR:i_arbpl.
    i_arbpl = wa_l_up_data-ARBID.
    CLEAR:wa_l_crhd.
    SELECT SINGLE ARBPL
      FROM CRHD
      INTO CORRESPONDING FIELDS OF wa_l_crhd
     WHERE ARBPL = i_arbpl
       AND WERKS = wa_l_up_data-WERKS.
    IF sy-subrc <> 0.
      IF wa_l_up_data-vgw01 IS NOT INITIAL
      OR  wa_l_up_data-vgw02 IS NOT INITIAL.
        MOVE 'E' TO wa_l_msg-msg_typ.
        MOVE text-m15 TO wa_l_msg-msg_txt.
        APPEND wa_l_msg TO it_msg.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF wa_l_up_data-vornr is not INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_NUMCV_INPUT'
        EXPORTING
          input         = wa_l_up_data-vornr
        IMPORTING
          OUTPUT        = wa_l_up_data-vornr.
    ENDIF.

    "权限检查
    authority-check object 'C_ROUT'
      id 'WERKS' field wa_l_up_data-WERKS
      id 'ACTVT' FIELD '01'.
    if sy-subrc ne 0.
      move 'E' to wa_l_msg-msg_typ.
      move text-m19 to wa_l_msg-msg_txt.
      append wa_l_msg to it_msg.
      continue.
    endif.


    "形成正常数据
    APPEND wa_l_up_data TO it_l_up_data.

  ENDLOOP.

* 每组工艺路线
*  SORT it_l_up_data BY werks VERWE matnr plnnr PLNAL.
  LOOP AT it_l_up_data INTO wa_l_up_data.

    "Materal 类型：检查是否使用过
    IF wa_l_up_data-matnr IS NOT INITIAL.
      CLEAR:wa_l_used1.
      READ TABLE it_l_used1 INTO wa_l_used1
        WITH KEY werks = wa_l_up_data-werks
                 VERWE = wa_l_up_data-VERWE
                 matnr = wa_l_up_data-matnr
                 PLNAL = wa_l_up_data-PLNAL.
      IF sy-subrc = 0.
        CONTINUE. "说明该工艺路线已做!
      else.
        CLEAR:wa_l_used1.
        wa_l_used1-werks = wa_l_up_data-werks.
        wa_l_used1-VERWE = wa_l_up_data-VERWE .
        wa_l_used1-matnr = wa_l_up_data-matnr.
        wa_l_used1-PLNAL = wa_l_up_data-PLNAL.
        APPEND wa_l_used1 TO it_l_used1.
      ENDIF.

      CLEAR:i_plnnr.
      SELECT SINGLE mapl~plnnr
        FROM mapl
        INNER JOIN plko ON plko~plnnr = mapl~plnnr
                       AND plko~PLNAL = mapl~PLNAL
        INTO i_plnnr
       WHERE mapl~MATNR = wa_l_up_data-matnr
         AND mapl~WERKS = wa_l_up_data-WERKS
         AND plko~VERWE = wa_l_up_data-VERWE.

      "Unit
      SELECT SINGLE MEINS
        FROM mara
        INTO i_MEINS
       WHERE matnr = wa_l_up_data-matnr.

     "形成组文件
      REFRESH:it_l_task,
              it_l_mat,
              it_l_opr.

      LOOP AT it_l_up_data INTO wa_l_up_data_ok
        WHERE werks = wa_l_up_data-werks
          AND VERWE = wa_l_up_data-VERWE
          AND matnr = wa_l_up_data-matnr
          AND PLNAL = wa_l_up_data-PLNAL.

        "Task
        CLEAR:wa_l_task.
        wa_l_task-TASK_LIST_GROUP = i_plnnr.
        wa_l_task-GROUP_COUNTER = wa_l_up_data_ok-plnal.
        wa_l_task-VALID_FROM = p_datum.
        wa_l_task-TASK_LIST_USAGE = wa_l_up_data_ok-VERWE.
        wa_l_task-TASK_LIST_STATUS = '4'.
        IF wa_l_up_data_ok-KTEXT IS INITIAL.
          wa_l_task-DESCRIPTION = wa_l_up_data_ok-maktx.
        ELSE.
          wa_l_task-DESCRIPTION = wa_l_up_data_ok-KTEXT.
        ENDIF.
        wa_l_task-PLANT = wa_l_up_data_ok-werks.
        wa_l_task-TASK_MEASURE_UNIT = i_MEINS.
        APPEND wa_l_task TO it_l_task.

        "Material
        CLEAR:wa_l_mat.
        wa_l_mat-MATERIAL = wa_l_up_data-matnr.
        wa_l_mat-PLANT = wa_l_up_data-werks.
        wa_l_mat-TASK_LIST_GROUP  = i_plnnr.
        wa_l_mat-GROUP_COUNTER = wa_l_up_data_ok-plnal.
        wa_l_mat-VALID_FROM = p_datum.
        APPEND wa_l_mat TO it_l_mat.

       "Operation
        CLEAR:wa_l_opr.
        wa_l_opr-TASK_LIST_GROUP  = i_plnnr.
        wa_l_opr-GROUP_COUNTER = wa_l_up_data_ok-plnal.
        wa_l_opr-PLANT = wa_l_up_data-werks.
        wa_l_opr-VALID_FROM = p_datum.
        wa_l_opr-ACTIVITY = wa_l_up_data_ok-vornr.
        wa_l_opr-CONTROL_KEY = wa_l_up_data_ok-steus.
        wa_l_opr-WORK_CNTR = wa_l_up_data_ok-ARBID.
        wa_l_opr-DESCRIPTION = wa_l_up_data_ok-ltxa1.
        wa_l_opr-STD_VALUE_01 = wa_l_up_data_ok-vgw01.
        wa_l_opr-STD_VALUE_02 = wa_l_up_data_ok-vgw02.
        wa_l_opr-STD_VALUE_03 = wa_l_up_data_ok-vgw02.
        wa_l_opr-STD_VALUE_04 = wa_l_up_data_ok-vgw02.
        wa_l_opr-STD_VALUE_05 = wa_l_up_data_ok-vgw02.
        wa_l_opr-OPERATION_MEASURE_UNIT = 'EA'.
        wa_l_opr-DENOMINATOR = 1.
        wa_l_opr-NOMINATOR = 1.
        wa_l_opr-BASE_QUANTITY = 1.
        wa_l_opr-COST_RELEVANT  = 'X'.
        APPEND wa_l_opr TO it_l_opr.
      ENDLOOP.
      SORT it_l_task BY TASK_LIST_GROUP GROUP_COUNTER.
      DELETE ADJACENT DUPLICATES FROM it_l_task COMPARING TASK_LIST_GROUP GROUP_COUNTER.
      sort it_l_mat BY MATERIAL TASK_LIST_GROUP GROUP_COUNTER.
      DELETE ADJACENT DUPLICATES FROM it_l_mat COMPARING MATERIAL TASK_LIST_GROUP GROUP_COUNTER.


      "然后进行创建工艺路线
      i_test = p_test.
      CLEAR:i_GROUP,
            i_GROUPCOUNTER.
      REFRESH:it_l_return.
      CALL FUNCTION 'BAPI_ROUTING_CREATE'
        EXPORTING
          TESTRUN                      = i_test
*         PROFILE                      =
*         BOMUSAGE                     =
*         APPLICATION                  =
        IMPORTING
          GROUP                        = i_GROUP
          GROUPCOUNTER                 = i_GROUPCOUNTER
        TABLES
          task                         = it_l_task
          MATERIALTASKALLOCATION       = it_l_mat
*         SEQUENCE                     =
          OPERATION                    = it_l_opr
*         SUBOPERATION                 =
*         REFERENCEOPERATION           =
*         WORKCENTERREFERENCE          =
*         COMPONENTALLOCATION          =
*         PRODUCTIONRESOURCE           =
*         INSPCHARACTERISTIC           =
*         TEXTALLOCATION               =
*         TEXT                         =
          RETURN                       = it_l_return.

      CLEAR:i_msgtxt,
            i_msgtyp.
      LOOP AT it_l_return INTO wa_l_return
        WHERE TYPE = 'E'
           or type = 'A'.
        i_msgtyp = 'E'.
        CLEAR:i_txt.
        MESSAGE ID wa_l_return-ID
                TYPE wa_l_return-TYPE
                NUMBER wa_l_return-NUMBER
                WITH  wa_l_return-MESSAGE_V1 wa_l_return-MESSAGE_V2
                      wa_l_return-MESSAGE_V3 wa_l_return-MESSAGE_V4
                INTO i_txt.
        CONCATENATE i_msgtxt
                    i_txt
                    INTO i_msgtxt SEPARATED BY '|'.
      ENDLOOP.
      IF sy-subrc <> 0.
        IF p_test IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT          = 'X'.
        ENDIF.

        i_msgtyp = 'S'.
        CONCATENATE text-m17
                    text-t01
                    i_GROUP
                    text-t02
                    i_GROUPCOUNTER
                    INTO i_msgtxt SEPARATED BY space.
      ELSE.
         CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

      "更新Message
      LOOP AT it_l_up_data INTO wa_l_up_data_ok
        WHERE werks = wa_l_up_data-werks
          AND VERWE = wa_l_up_data-VERWE
          AND matnr = wa_l_up_data-matnr
          AND PLNAL = wa_l_up_data-PLNAL.

        CLEAR:wa_l_msg.
        wa_l_msg-msg_typ = i_msgtyp.
        MOVE:wa_l_up_data_ok-werks  to wa_l_msg-werks,
             wa_l_up_data_ok-VERWE  to wa_l_msg-VERWE,
             wa_l_up_data_ok-matnr  to wa_l_msg-matnr,
             wa_l_up_data_ok-maktx  TO wa_l_msg-maktx,
             wa_l_up_data_ok-plnnr  to wa_l_msg-plnnr,
             wa_l_up_data_ok-plnal  to wa_l_msg-plnal,
             wa_l_up_data_ok-vornr  to wa_l_msg-vornr,
             wa_l_up_data_ok-ARBID  to wa_l_msg-ARBID,
             wa_l_up_data_ok-steus  to wa_l_msg-steus,
             wa_l_up_data_ok-ltxa1  to wa_l_msg-ltxa1,
             wa_l_up_data_ok-vgw01  to wa_l_msg-vgw01,
             wa_l_up_data_ok-vgw02  to wa_l_msg-vgw02,
             i_msgtxt               to wa_l_msg-msg_txt.
        APPEND wa_l_msg TO it_msg.
      ENDLOOP.

    ENDIF.

    "组 类型：检查是否使用过
    IF wa_l_up_data-plnnr IS NOT INITIAL.
      CLEAR:wa_l_used2.
      READ TABLE it_l_used2 INTO wa_l_used2
        WITH KEY werks = wa_l_up_data-werks
                 VERWE = wa_l_up_data-VERWE
                 plnnr = wa_l_up_data-plnnr
                PLNAL = wa_l_up_data-PLNAL.
      IF sy-subrc = 0.
        CONTINUE. "说明该工艺路线已做!
      else.
        CLEAR:wa_l_used2.
        wa_l_used2-werks = wa_l_up_data-werks.
        wa_l_used2-VERWE = wa_l_up_data-VERWE .
        wa_l_used2-plnnr = wa_l_up_data-plnnr.
        wa_l_used2-PLNAL = wa_l_up_data-PLNAL.
        APPEND wa_l_used2 TO it_l_used2.
      ENDIF.


     "形成组文件
      REFRESH:it_l_task,
              it_l_mat,
              it_l_opr.


      "Operation
      LOOP AT it_l_up_data INTO wa_l_up_data_ok
        WHERE werks = wa_l_up_data-werks
          AND VERWE = wa_l_up_data-VERWE
          AND plnnr = wa_l_up_data-plnnr
          and PLNAL = wa_l_up_data-PLNAL.

        "Task
        CLEAR:wa_l_task.
        wa_l_task-TASK_LIST_GROUP = wa_l_up_data_ok-plnnr.
        wa_l_task-GROUP_COUNTER = wa_l_up_data_ok-PLNAL.
        wa_l_task-VALID_FROM = p_datum.
        wa_l_task-TASK_LIST_USAGE = wa_l_up_data_ok-VERWE.
        wa_l_task-TASK_LIST_STATUS = '4'.
        wa_l_task-DESCRIPTION = wa_l_up_data_ok-maktx.
        wa_l_task-PLANT = wa_l_up_data_ok-werks.
        wa_l_task-TASK_MEASURE_UNIT = 'EA'.
        APPEND wa_l_task TO it_l_task.

        "Operation
        CLEAR:wa_l_opr.
        wa_l_opr-TASK_LIST_GROUP  = wa_l_up_data_ok-plnnr.
        wa_l_opr-GROUP_COUNTER = wa_l_up_data_ok-PLNAL.
        wa_l_opr-PLANT = wa_l_up_data-werks.
        wa_l_opr-VALID_FROM = p_datum.
        wa_l_opr-ACTIVITY = wa_l_up_data_ok-vornr.
        wa_l_opr-CONTROL_KEY = wa_l_up_data_ok-steus.
        wa_l_opr-WORK_CNTR = wa_l_up_data_ok-ARBID.
        wa_l_opr-DESCRIPTION = wa_l_up_data_ok-ltxa1.
        wa_l_opr-STD_VALUE_01 = wa_l_up_data_ok-vgw01.
        wa_l_opr-STD_VALUE_02 = wa_l_up_data_ok-vgw02.
        wa_l_opr-STD_VALUE_03 = wa_l_up_data_ok-vgw02.
        wa_l_opr-STD_VALUE_04 = wa_l_up_data_ok-vgw02.
        wa_l_opr-STD_VALUE_05 = wa_l_up_data_ok-vgw02.
        wa_l_opr-OPERATION_MEASURE_UNIT = 'EA'.
        wa_l_opr-DENOMINATOR = 1.
        wa_l_opr-NOMINATOR = 1.
        wa_l_opr-BASE_QUANTITY = 1.
        wa_l_opr-COST_RELEVANT  = 'X'.
        APPEND wa_l_opr TO it_l_opr.
      ENDLOOP.
      SORT it_l_task BY TASK_LIST_GROUP GROUP_COUNTER.
      DELETE ADJACENT DUPLICATES FROM it_l_task COMPARING TASK_LIST_GROUP GROUP_COUNTER.

      "然后进行创建工艺路线
      i_test = p_test.
      CLEAR:i_GROUP,
            i_GROUPCOUNTER.
      REFRESH:it_l_return.
      CALL FUNCTION 'BAPI_ROUTING_CREATE'
        EXPORTING
          TESTRUN                      = i_test
*         PROFILE                      =
*         BOMUSAGE                     =
*         APPLICATION                  =
        IMPORTING
          GROUP                        = i_GROUP
          GROUPCOUNTER                 = i_GROUPCOUNTER
        TABLES
          task                         = it_l_task
*          MATERIALTASKALLOCATION       = it_l_mat
*         SEQUENCE                     =
          OPERATION                    = it_l_opr
*         SUBOPERATION                 =
*         REFERENCEOPERATION           =
*         WORKCENTERREFERENCE          =
*         COMPONENTALLOCATION          =
*         PRODUCTIONRESOURCE           =
*         INSPCHARACTERISTIC           =
*         TEXTALLOCATION               =
*         TEXT                         =
          RETURN                       = it_l_return.

      CLEAR:i_msgtxt,
            i_msgtyp.
      LOOP AT it_l_return INTO wa_l_return
        WHERE TYPE = 'E'
           or type = 'A'.
        i_msgtyp = 'E'.
        CLEAR:i_txt.
        MESSAGE ID wa_l_return-ID
                TYPE wa_l_return-TYPE
                NUMBER wa_l_return-NUMBER
                WITH  wa_l_return-MESSAGE_V1 wa_l_return-MESSAGE_V2
                      wa_l_return-MESSAGE_V3 wa_l_return-MESSAGE_V4
                INTO i_txt.
        CONCATENATE i_msgtxt
                    i_txt
                    INTO i_msgtxt SEPARATED BY '|'.
      ENDLOOP.
      IF sy-subrc <> 0.
        IF p_test IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT          = 'X'.
        ENDIF.

        i_msgtyp = 'S'.
        CONCATENATE text-m17
                    text-t01
                    i_GROUP
                    text-t02
                    i_GROUPCOUNTER
                    INTO i_msgtxt SEPARATED BY space.
      ELSE.
         CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

      "更新Message
      LOOP AT it_l_up_data INTO wa_l_up_data_ok
        WHERE werks = wa_l_up_data-werks
          AND VERWE = wa_l_up_data-VERWE
          AND plnnr = wa_l_up_data-plnnr
          and PLNAL = wa_l_up_data-PLNAL.

        CLEAR:wa_l_msg.
        wa_l_msg-msg_typ = i_msgtyp.
        MOVE:wa_l_up_data_ok-werks  to wa_l_msg-werks,
             wa_l_up_data_ok-VERWE  to wa_l_msg-VERWE,
             wa_l_up_data_ok-matnr  to wa_l_msg-matnr,
             wa_l_up_data_ok-maktx  TO wa_l_msg-maktx,
             wa_l_up_data_ok-plnnr  to wa_l_msg-plnnr,
             wa_l_up_data_ok-plnal  to wa_l_msg-plnal,
             wa_l_up_data_ok-vornr  to wa_l_msg-vornr,
             wa_l_up_data_ok-ARBID  to wa_l_msg-ARBID,
             wa_l_up_data_ok-steus  to wa_l_msg-steus,
             wa_l_up_data_ok-ltxa1  to wa_l_msg-ltxa1,
             wa_l_up_data_ok-vgw01  to wa_l_msg-vgw01,
             wa_l_up_data_ok-vgw02  to wa_l_msg-vgw02,
             i_msgtxt               to wa_l_msg-msg_txt.
        APPEND wa_l_msg TO it_msg.
      ENDLOOP.

    ENDIF.

  ENDLOOP.

  SORT it_msg BY msg_typ ASCENDING
                 WERKS ASCENDING
                 VERWE ASCENDING
                 MATNR ASCENDING.

endform.                    " FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form FRM_OUTPUT_MSG .

* Fieldcat Build
  PERFORM frm_build_fieldcat.

* Display
  PERFORM frm_output_data.

endform.                    " FRM_OUTPUT_MSG
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form FRM_BUILD_FIELDCAT .

  REFRESH: it_alv_fldcat.
  PERFORM frm_alv_fieldcat  USING:  'MSG_LCD'
                                    text-h01
                                    5
                                    ''
                                    '',

                                    'WERKS'
                                    text-h02
                                    4
                                    'PLKO'
                                    'WERKS',

                                    'VERWE'
                                    text-h03
                                    3
                                    'PLKO'
                                    'VERWE',

                                    'MATNR'
                                    text-h04
                                    18
                                    'MARA'
                                    'MATNR',


                                    'MAKTX'
                                    text-h14
                                    40
                                    'MAKT'
                                    'MAKTX',

                                    'PLNNR'
                                    text-h05
                                    8
                                    'PLKO'
                                    'PLNNR',

                                    'PLNAL'
                                    text-h06
                                    2
                                    'PLKO'
                                    'PLNAL',

                                    'VORNR'
                                    text-h07
                                    4
                                    'PLPO'
                                    'VORNR',

                                    'ARBID'
                                    text-h08
                                    8
                                    'PLPO'
                                    'ARBID',

                                    'STEUS'
                                    text-h09
                                    4
                                    'PLPO'
                                    'STEUS',

                                    'LTXA1'
                                    text-h10
                                    40
                                    'PLPO'
                                    'LTXA1',

                                    'VGW01'
                                    text-h11
                                    15
                                    'PLPO'
                                    'VGW01',

                                    'VGW02'
                                    text-h12
                                    15
                                    'PLPO'
                                    'VGW02',

                                    'MSG_TXT'
                                    text-h13
                                    255
                                    ''
                                    ''.

endform.                    " FRM_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2199   text
*      -->P_TEXT_01  text
*      -->P_8      text
*----------------------------------------------------------------------*
FORM frm_alv_fieldcat  USING   pr_fname
                             pr_selt
                             pr_outlen
                             pr_rtname
                             pr_rfname.

  DATA:wa_l_alv_fldcat  TYPE  slis_fieldcat_alv.

  CLEAR:wa_l_alv_fldcat.
  g_pos = g_pos + 1.
  wa_l_alv_fldcat-col_pos = g_pos.
  wa_l_alv_fldcat-fieldname = pr_fname.
  wa_l_alv_fldcat-seltext_l = pr_selt.
  wa_l_alv_fldcat-outputlen = pr_outlen.
  wa_l_alv_fldcat-ref_fieldname = pr_rfname.
  wa_l_alv_fldcat-ref_tabname = pr_rtname.
  APPEND wa_l_alv_fldcat TO it_alv_fldcat.


ENDFORM.                    " FRM_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form FRM_OUTPUT_DATA .


  DATA: i_repid       TYPE sy-repid,
        wa_l_layout   TYPE  SLIS_LAYOUT_ALV.
  FIELD-SYMBOLS: <fs_l_msg> TYPE t_msg.

  "数据准备
  LOOP AT it_msg  ASSIGNING <fs_l_msg>.
    "LED 展示
    CASE <fs_l_msg>-msg_typ.
      WHEN 'E'
         OR 'A'.
        <fs_l_msg>-msg_lcd = '@5C@'.
      WHEN 'W'.
        <fs_l_msg>-msg_lcd = '@5D@'.
       WHEN 'S'.
         <fs_l_msg>-msg_lcd = '@5B@'.
      WHEN OTHERS.
        <fs_l_msg>-msg_lcd = '@5C@'.
    ENDCASE.

    "外部形式
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input         = <fs_l_msg>-matnr
      IMPORTING
        OUTPUT        = <fs_l_msg>-matnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input         = <fs_l_msg>-PLNNR
      IMPORTING
        OUTPUT        = <fs_l_msg>-PLNNR.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input         = <fs_l_msg>-PLNAL
      IMPORTING
        OUTPUT        = <fs_l_msg>-PLNAL.

  ENDLOOP.


  "ALV需要
  CLEAR:i_repid,
        wa_l_layout.
  i_repid = sy-repid.
  wa_l_layout-colwidth_optimize = c_X.

  "ALV展示
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = i_repid
*      i_callback_pf_status_set = 'FRM_PF_STATUS_SET'
      IS_LAYOUT                = wa_l_layout
      it_fieldcat              = it_alv_fldcat
    TABLES
      t_outtab                 = it_msg
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


endform.                    " FRM_OUTPUT_DATA
