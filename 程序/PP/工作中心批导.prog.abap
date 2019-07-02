*&---------------------------------------------------------------------*
* Program ID/Name: ZPPR007            Date written:2013/07/12
* Author's name:   HP_TC              Last update:
* Program title:  工作中心主数据批量导入
* Project Name:  ZFSS
* Version:
* Function Spec ID:
*----------------------------------------------------------------------*
* Description: 工作中心主数据批量导入
*
*----------------------------------------------------------------------*

REPORT  zppr007.

*$*$********************************************************************
*$*$    Global Types                                                   *
*$*$********************************************************************
TYPES:
  BEGIN OF t_up_datac,
    werks(4)     TYPE  c,   "工厂
    arbpl(8)     TYPE  c,   "工作中心
    ktext(40)     TYPE  c,   "工作中心描述
    kapart1(3)   TYPE  c,  "能力类别D
    kapart2(3)   TYPE  c,  "能力类别E
    begzt(10)     TYPE  c,   "开始时间
    endzt(10)     TYPE  c,   "完成时间
    aznor1(10)    TYPE  c,   "单个的能力数量H
    aznor2(10)    TYPE  c,   "单个的能力数量I
    kostl(10)     TYPE  c,   "成本中心
  END OF t_up_datac,
  BEGIN OF t_up_data,
    werks     TYPE  crhd_api01-werks,   "工厂
    arbpl     TYPE  crhd_api01-arbpl,   "工作中心
    ktext     TYPE  crhd_api01-ktext,   "工作中心描述
    kapart1   TYPE  crhd_api05-kapart,  "能力类别D
    kapart2   TYPE  kapa_api01-kapart,  "能力类别E
    begzt     TYPE  kapa_api02-begzt,   "开始时间
    endzt     TYPE  kapa_api02-endzt,   "完成时间
    aznor1    TYPE  kapa_api02-aznor,   "单个的能力数量H
    aznor2    TYPE  kapa_api02-aznor,   "单个的能力数量I
    kostl     TYPE  crco_api01-kostl,   "成本中心
  END OF t_up_data,
  BEGIN OF t_msg,
    msg_typ(01)   TYPE  c,
    msg_lcd       TYPE icon_d,
    msg_txt(255),

    werks(4)     TYPE  c,   "工厂
    arbpl(8)     TYPE  c,   "工作中心
    ktext(40)     TYPE  c,   "工作中心描述
    kapart1(3)   TYPE  c,  "能力类别D
    kapart2(3)   TYPE  c,  "能力类别E
    begzt(10)     TYPE  c,   "开始时间
    endzt(10)     TYPE  c,   "完成时间
    aznor1(10)    TYPE  c,   "单个的能力数量H
    aznor2(10)    TYPE  c,   "单个的能力数量I
    kostl(10)     TYPE  c,   "成本中心
  END OF t_msg.


*$*$********************************************************************
*$*$    GLOBAL CONSTANTS                                               *
*$*$********************************************************************
CONSTANTS:
  c_x(1)  TYPE  c     VALUE 'X'.

*$*$********************************************************************
*$*$    GLOBAL ELEMENTARY VARIABLES                                    *
*$*$********************************************************************
DATA:
  g_okcode        TYPE  sy-ucomm,
  g_pos           TYPE  i,
  g_container     TYPE REF TO  cl_gui_custom_container, "Custom Container
  g_alv_grid      TYPE REF TO  cl_gui_alv_grid.         "Alv Grid control

*$*$********************************************************************
*$*$    GLOBAL INTERNAL TABLES                                         *
*$*$********************************************************************
DATA:
  it_up_datac   TYPE STANDARD TABLE OF t_up_datac,
  it_msg        TYPE STANDARD TABLE OF t_msg.

*$*$********************************************************************
*$*$    GLOBAL STRUCTURES                                              *
*$*$********************************************************************


*$*$********************************************************************
*$*$    PARAMETERS & SELECT-OPTIONS                                    *
*$*$********************************************************************
SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:
          p_file  TYPE string OBLIGATORY, "rlgrap-filename
          p_date  TYPE crco_api01-begda.

SELECTION-SCREEN:END OF BLOCK b1.
SELECTION-SCREEN:BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS:
          p_test AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN:END OF BLOCK b2.

*$*$********************************************************************
*$*$    Initialization                                                 *
*$*$********************************************************************
INITIALIZATION.
  p_date = sy-datum.
  p_date+4(4) = '0101'.

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

  DATA: i_path(80).
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_filename     = ' '
            def_path         = p_file
*            mask             = ',.xls,'
            mode             = '0'
*           TITLE            = ' '
       IMPORTING
            filename         = p_file
*           RC               = '
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            OTHERS           = 5.
*  CONCATENATE p_file '.xls' INTO i_path.
*  CLEAR: p_file.
*  p_file = i_path.

ENDFORM.                    " FRM_F4_FILE
*&---------------------------------------------------------------------*
*&      Form  FRM_MAIN_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_main_process .

* 上传数据至内表
  REFRESH:it_up_datac.
  PERFORM frm_upload_data.

* 检查并处理数据
  REFRESH:it_msg.
  PERFORM frm_process_data.

* 展示message
  PERFORM frm_output_msg.

ENDFORM.                    " FRM_MAIN_PROCESS
*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_upload_data .

  DATA:i_file TYPE  string.

  CLEAR:i_file.
  i_file = p_file.
  REFRESH it_up_datac.

  "上载
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                      = i_file
*     FILETYPE                      = 'ASC'
      has_field_separator           = 'X'
*     HEADER_LENGTH                 = 0
*     READ_BY_LINE                  = 'X'
*     DAT_MODE                      = ' '
*   IMPORTING
*     FILELENGTH                    =
*     HEADER                        =
    TABLES
      data_tab                      = it_up_datac
   EXCEPTIONS
     file_open_error               = 1
     file_read_error               = 2
     no_batch                      = 3
     gui_refuse_filetransfer       = 4
     invalid_type                  = 5
     no_authority                  = 6
     unknown_error                 = 7
     bad_data_format               = 8
     header_not_allowed            = 9
     separator_not_allowed         = 10
     header_too_long               = 11
     unknown_dp_error              = 12
     access_denied                 = 13
     dp_out_of_memory              = 14
     disk_full                     = 15
     dp_timeout                    = 16
     OTHERS                        = 17.
  IF sy-subrc NE 0.
    MESSAGE s011(zpp_msg) DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.                    " FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_process_data .

  DATA:
    wa_l_up_datac   TYPE t_up_datac,
    wa_l_up_data    TYPE t_up_data,
    wa_l_msg        TYPE t_msg,
    wa_l_crhd       TYPE crhd,
    wa_l_t001w      TYPE t001w,
    i_comment       TYPE crtx-ktext,
    i_exnum         TYPE balhdr-extnumber,
    i_tst           TYPE ttzdata-timestamp,
    i_date_f        TYPE sy-datum,
    i_time_f        TYPE sy-uzeit,
    i_date_t        TYPE sy-datum,
    i_time_t        TYPE sy-uzeit,
    i_len           TYPE sy-tabix,
    wa_l_log_hd     TYPE balhdr,
    it_l_log_hd     TYPE STANDARD TABLE OF balhdr,
    wa_l_log        TYPE balm,
    it_l_log        TYPE STANDARD TABLE OF balm,
    wa_l_crhd_api01 TYPE crhd_api01,
    wa_l_crhd_api02 TYPE crhd_api02,
    wa_l_crhd_api03 TYPE crhd_api03,
    wa_l_crhd_api05 TYPE crhd_api05,
    wa_l_kapa_api01 TYPE kapa_api01,
    it_l_kapa_api01 TYPE STANDARD TABLE OF kapa_api01,
    wa_l_kapa_api02 TYPE kapa_api02,
    it_l_kapa_api02 TYPE STANDARD TABLE OF kapa_api02,
    wa_l_crhd_api04 TYPE crhd_api04,
    it_l_crhd_api04 TYPE STANDARD TABLE OF crhd_api04,
    wa_l_crco_api01 TYPE crco_api01,
    it_l_crco_api01 TYPE STANDARD TABLE OF crco_api01.

* Process each
  LOOP AT it_up_datac INTO wa_l_up_datac.
    CLEAR:wa_l_up_data,
          wa_l_msg.
    "自动转大写
    TRANSLATE wa_l_up_datac-arbpl TO UPPER CASE.

    MOVE:wa_l_up_datac-werks  TO wa_l_msg-werks,
         wa_l_up_datac-arbpl  TO wa_l_msg-arbpl,
         wa_l_up_datac-ktext  TO wa_l_msg-ktext,
         wa_l_up_datac-kapart1  TO wa_l_msg-kapart1,
         wa_l_up_datac-kapart2  TO wa_l_msg-kapart2,
         wa_l_up_datac-begzt  TO wa_l_msg-begzt,
         wa_l_up_datac-endzt  TO wa_l_msg-endzt,
         wa_l_up_datac-aznor1  TO wa_l_msg-aznor1,
         wa_l_up_datac-aznor2  TO wa_l_msg-aznor2,
         wa_l_up_datac-kostl  TO wa_l_msg-kostl.

    CATCH SYSTEM-EXCEPTIONS convt_no_number = 1
                           OTHERS = 8.
      MOVE:wa_l_up_datac-werks  TO wa_l_up_data-werks,
           wa_l_up_datac-arbpl  TO wa_l_up_data-arbpl,
           wa_l_up_datac-ktext  TO wa_l_up_data-ktext,
           wa_l_up_datac-kapart1  TO wa_l_up_data-kapart1,
           wa_l_up_datac-kapart2  TO wa_l_up_data-kapart2,
           wa_l_up_datac-begzt  TO wa_l_up_data-begzt,
           wa_l_up_datac-endzt  TO wa_l_up_data-endzt,
           wa_l_up_datac-aznor1  TO wa_l_up_data-aznor1,
           wa_l_up_datac-aznor2  TO wa_l_up_data-aznor2,
           wa_l_up_datac-kostl  TO wa_l_up_data-kostl.
    ENDCATCH.
    IF sy-subrc NE 0.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m01 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.

    "Plant
    CLEAR:wa_l_t001w.
    SELECT SINGLE werks
      FROM t001w
      INTO CORRESPONDING FIELDS OF wa_l_t001w
     WHERE werks = wa_l_up_data-werks.
    IF sy-subrc <> 0.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m00 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.

    "检查存在性
    CLEAR:wa_l_crhd.
    SELECT SINGLE arbpl
      FROM crhd
      INTO CORRESPONDING FIELDS OF wa_l_crhd
     WHERE arbpl = wa_l_up_data-arbpl
       AND werks = wa_l_up_data-werks.
    IF sy-subrc = 0.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m02 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.

    "D列、E列必输其一
    IF wa_l_up_data-kapart1 IS INITIAL
    AND wa_l_up_data-kapart2 IS INITIAL.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m03 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.

    "D列H列必须同时有值或空
    IF wa_l_up_data-kapart1 IS NOT INITIAL
    AND wa_l_up_data-aznor1 IS INITIAL.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m04 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.

    "E列I列必须同时有值或空
    IF wa_l_up_data-kapart2 IS NOT INITIAL
    AND wa_l_up_data-aznor2 IS INITIAL.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m04 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.

    "权限检查
    AUTHORITY-CHECK OBJECT 'C_ARPL_WRK'
      ID 'WERKS' FIELD wa_l_up_data-werks
      ID 'ACTVT' FIELD '01'.
    IF sy-subrc NE 0.
      MOVE 'E' TO wa_l_msg-msg_typ.
      MOVE text-m08 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
      CONTINUE.
    ENDIF.

    "创建Work center
    CLEAR:wa_l_crhd_api01.
    wa_l_crhd_api01-arbpl = wa_l_up_data-arbpl.
    wa_l_crhd_api01-werks = wa_l_up_data-werks.
    wa_l_crhd_api01-verwe = '0001'.
    wa_l_crhd_api01-ktext = wa_l_up_data-ktext.
    CLEAR:wa_l_crhd_api02.
    wa_l_crhd_api02-planv = '009'.
    wa_l_crhd_api02-veran = '001'.
    wa_l_crhd_api02-vgwts = 'Z001'.
    CLEAR:wa_l_crhd_api03.
    wa_l_crhd_api03-steus = 'Z001'.
    wa_l_crhd_api03-vge01 = 'MIN'."'X'.
    wa_l_crhd_api03-vge02 = 'MIN'.
    wa_l_crhd_api03-vge03 = 'MIN'.
    wa_l_crhd_api03-vge04 = 'MIN'.
    wa_l_crhd_api03-vge05 = 'MIN'.
    CLEAR:wa_l_crhd_api05.
    IF wa_l_up_data-kapart1 IS INITIAL.
      wa_l_crhd_api05-kapart = wa_l_up_data-kapart2.
      wa_l_crhd_api05-fort2 = 'Z202'.
    ELSE.
      wa_l_crhd_api05-kapart = wa_l_up_data-kapart1.
      wa_l_crhd_api05-fort2 = 'Z201'.
    ENDIF.
    REFRESH:it_l_kapa_api01.
    IF wa_l_up_data-kapart1 IS NOT INITIAL.
      IF wa_l_up_data-kapart1 = '001'.
        CLEAR:wa_l_kapa_api01.
        wa_l_kapa_api01-kapart = wa_l_up_data-kapart1.
        wa_l_kapa_api01-canum = '0001'.
        wa_l_kapa_api01-ktext = text-003.
        wa_l_kapa_api01-werks = wa_l_up_data-werks.
        APPEND wa_l_kapa_api01 TO it_l_kapa_api01.
      ENDIF.
    ENDIF.
    IF wa_l_up_data-kapart2 IS NOT INITIAL.
      IF wa_l_up_data-kapart2 = '002'.
        CLEAR:wa_l_kapa_api01.
        wa_l_kapa_api01-kapart = wa_l_up_data-kapart2.
        wa_l_kapa_api01-canum = '0002'.
        wa_l_kapa_api01-ktext = text-004.
        wa_l_kapa_api01-werks = wa_l_up_data-werks.
        APPEND wa_l_kapa_api01 TO it_l_kapa_api01.
      ENDIF.
    ENDIF.
    REFRESH:it_l_kapa_api02.
    IF wa_l_up_data-kapart1 IS NOT INITIAL.
      IF wa_l_up_data-kapart1 = '001'.
        CLEAR:wa_l_kapa_api02.
        wa_l_kapa_api02-canum = '0001'.
        wa_l_kapa_api02-planr = '001'.
        "modify by handyxh 20190618 begin
        IF wa_l_up_data-werks CP 'SH*'.
          wa_l_kapa_api02-kalid = 'S1'.
        ENDIF.

        IF wa_l_up_data-werks CP 'SN*'.
          wa_l_kapa_api02-kalid = 'N1'.
        ENDIF.
        "modify by handyxh 20190618 end
        IF wa_l_up_data-werks CP 'Y*'.
          wa_l_kapa_api02-kalid = 'Y1'.
        ENDIF.
*        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*          EXPORTING
*            INPUT                = 'H'
*            LANGUAGE             = SY-LANGU
*          IMPORTING
**           LONG_TEXT            =
*            OUTPUT               = wa_l_KAPA_API02-MEINS
**           SHORT_TEXT           =
*         EXCEPTIONS
*            UNIT_NOT_FOUND       = 1
*            OTHERS               = 2.
        wa_l_kapa_api02-meins = 'H'.
        wa_l_kapa_api02-begzt = wa_l_up_data-begzt.
        wa_l_kapa_api02-endzt = wa_l_up_data-endzt.
        wa_l_kapa_api02-ngrad = '100'.
        wa_l_kapa_api02-aznor = wa_l_up_data-aznor1.
        wa_l_kapa_api02-kapter = 'X'.
        wa_l_kapa_api02-kaplpl = 'X'.
        APPEND wa_l_kapa_api02 TO it_l_kapa_api02.
      ENDIF.
    ENDIF.
    IF wa_l_up_data-kapart2 IS NOT INITIAL.
      IF wa_l_up_data-kapart2 = '002'.
        CLEAR:wa_l_kapa_api02.
        wa_l_kapa_api02-canum = '0002'.
        wa_l_kapa_api02-planr = '001'.

        IF wa_l_up_data-werks CP 'S*'.
          wa_l_kapa_api02-kalid = 'S1'.
        ENDIF.
        IF wa_l_up_data-werks CP 'Y*'.
          wa_l_kapa_api02-kalid = 'Y1'.
        ENDIF.

*        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*          EXPORTING
*            INPUT                = 'H'
*            LANGUAGE             = SY-LANGU
*          IMPORTING
**           LONG_TEXT            =
*            OUTPUT               = wa_l_KAPA_API02-MEINS
**           SHORT_TEXT           =
*         EXCEPTIONS
*            UNIT_NOT_FOUND       = 1
*            OTHERS               = 2.

        wa_l_kapa_api02-meins = 'H'.
        wa_l_kapa_api02-begzt = wa_l_up_data-begzt.
        wa_l_kapa_api02-endzt = wa_l_up_data-endzt.
        wa_l_kapa_api02-ngrad = '100'.
        wa_l_kapa_api02-aznor = wa_l_up_data-aznor2.
        wa_l_kapa_api02-kapter = 'X'.
        wa_l_kapa_api02-kaplpl = 'X'.
        APPEND wa_l_kapa_api02 TO it_l_kapa_api02.
      ENDIF.
    ENDIF.
    REFRESH:it_l_crhd_api04.
    IF wa_l_up_data-kapart1 IS NOT INITIAL.
      IF wa_l_up_data-kapart1 = '001'.
        CLEAR:wa_l_crhd_api04.
        wa_l_crhd_api04-canum = '0001'.
        wa_l_crhd_api04-fork2 = 'Z101'.
        APPEND wa_l_crhd_api04 TO it_l_crhd_api04.
      ENDIF.
    ENDIF.
    IF wa_l_up_data-kapart2 IS NOT INITIAL.
      IF wa_l_up_data-kapart2 = '002'.
        CLEAR:wa_l_crhd_api04.
        wa_l_crhd_api04-canum = '0002'.
        wa_l_crhd_api04-fork2 = 'Z102'.
        APPEND wa_l_crhd_api04 TO it_l_crhd_api04.
      ENDIF.
    ENDIF.
    REFRESH:it_l_crco_api01.
    CLEAR:wa_l_crco_api01.
    wa_l_crco_api01-kostl = wa_l_up_data-kostl.
    wa_l_crco_api01-begda  = p_date.
    wa_l_crco_api01-endda = '99991231'.
    wa_l_crco_api01-lstar1 = 'AT01'.
    wa_l_crco_api01-lstar2 = 'AT02'.
    wa_l_crco_api01-lstar3 = 'AT03'.
    wa_l_crco_api01-lstar4 = 'AT04'.
    wa_l_crco_api01-lstar5 = 'AT05'.
    wa_l_crco_api01-forml1 = 'Z301'.
    wa_l_crco_api01-forml2 = 'Z302'.
    wa_l_crco_api01-forml3 = 'Z302'.
    wa_l_crco_api01-forml4 = 'Z302'.
    wa_l_crco_api01-forml5 = 'Z302'.
    wa_l_crco_api01-leinh1 = 'MIN'.
    wa_l_crco_api01-leinh2 = 'MIN'.
    wa_l_crco_api01-leinh3 = 'MIN'.
    wa_l_crco_api01-leinh4 = 'MIN'.
    wa_l_crco_api01-leinh5 = 'MIN'.
    wa_l_crco_api01-lstar_ref1 = c_x.
    wa_l_crco_api01-lstar_ref2 = c_x.
    wa_l_crco_api01-lstar_ref3 = c_x.
    wa_l_crco_api01-lstar_ref4 = c_x.
    wa_l_crco_api01-lstar_ref5 = c_x.
    APPEND wa_l_crco_api01 TO it_l_crco_api01.

    CLEAR:i_comment.
    CALL FUNCTION 'TZ_UTC_SYSTEMCLOCK'
      IMPORTING
        utc_timestamp = i_tst.
    i_comment = i_tst.
    CLEAR:i_date_f,i_time_f.
    i_date_f = sy-datum.
    i_time_f = sy-uzeit.

    "Call BAPI
    CALL FUNCTION 'CRAP_WORKCENTER_CREATE'
      EXPORTING
        in_crhd_api01 = wa_l_crhd_api01
        in_crhd_api02 = wa_l_crhd_api02
        in_crhd_api03 = wa_l_crhd_api03
        in_crhd_api05 = wa_l_crhd_api05
        comment       = i_comment
        test          = p_test
      TABLES
        in_kapa_api01 = it_l_kapa_api01
        in_kapa_api02 = it_l_kapa_api02
        in_crhd_api04 = it_l_crhd_api04
        in_crco_api01 = it_l_crco_api01
*       IN_KAZY_API01 =
*       IN_KAPA_API04 =
      .
    IF p_test IS INITIAL.
      COMMIT WORK AND WAIT.
      WAIT UP TO 1 SECONDS.

      "Check成功否
      CALL FUNCTION 'CMOA_CHECK_WORKCENTER'
        EXPORTING
          i_vaplz              = wa_l_up_data-arbpl
          i_vawrk              = wa_l_up_data-werks
        EXCEPTIONS
          no_workcenter        = 1
          no_plant             = 2
          workcenter_not_found = 3
          plnty_not_allowed    = 4
          kokrs_diff           = 4
          deleted_and_locked   = 5
          deleted              = 6
          locked               = 7
          OTHERS               = 8.
      IF sy-subrc NE 0.
        CLEAR:i_date_t,i_time_t.
        i_date_t = sy-datum.
        i_time_t = sy-uzeit.
        CLEAR:i_exnum.
        i_exnum = i_comment.

        "取得Log
        REFRESH:it_l_log_hd,it_l_log.
        CALL FUNCTION 'APPL_LOG_READ_DB'
          EXPORTING
            object             = 'CRAP'
*            subobject          = '*'
            external_number    = i_exnum
            date_from          = i_date_f
            date_to            = i_date_t
            time_from          = i_time_f
            time_to            = i_time_t
*          IMPORTING
*            number_of_logs     = i_number_of_logs
          TABLES
            header_data        =  it_l_log_hd
            messages           = it_l_log.

        CLEAR:i_len.
        DESCRIBE TABLE it_l_log_hd LINES i_len.
        CLEAR:wa_l_log_hd.
        READ TABLE it_l_log_hd INTO wa_l_log_hd INDEX i_len.
        IF sy-subrc NE 0.
          MOVE 'E' TO wa_l_msg-msg_typ.
          MOVE text-m05 TO wa_l_msg-msg_txt.
          APPEND wa_l_msg TO it_msg.
          CONTINUE.
        ELSE.
          CLEAR:wa_l_log.
          READ TABLE it_l_log INTO wa_l_log
            WITH KEY lognumber = wa_l_log_hd-lognumber
                     msgty = 'E'.
          IF sy-subrc NE 0.
            MOVE 'E' TO wa_l_msg-msg_typ.
            MOVE text-m05 TO wa_l_msg-msg_txt.
            APPEND wa_l_msg TO it_msg.
            CONTINUE.
          ELSE.
            MOVE 'E' TO wa_l_msg-msg_typ.
            MESSAGE ID wa_l_log-msgid TYPE wa_l_log-msgty NUMBER wa_l_log-msgno
              WITH wa_l_log-msgv1 wa_l_log-msgv2 wa_l_log-msgv3 wa_l_log-msgv4
              INTO wa_l_msg-msg_txt.
            APPEND wa_l_msg TO it_msg.
            CONTINUE.
          ENDIF.
        ENDIF.
      ELSE.
        MOVE 'S' TO wa_l_msg-msg_typ.
        MOVE text-m06 TO wa_l_msg-msg_txt.
        APPEND wa_l_msg TO it_msg.
      ENDIF.
    ELSE.
      MOVE 'S' TO wa_l_msg-msg_typ.
      MOVE text-m07 TO wa_l_msg-msg_txt.
      APPEND wa_l_msg TO it_msg.
    ENDIF.

  ENDLOOP.
  SORT it_msg BY msg_typ ASCENDING
                 werks ASCENDING
                 arbpl ASCENDING.

ENDFORM.                    " FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_output_msg .

  FIELD-SYMBOLS: <fs_l_msg> TYPE t_msg.

  CHECK it_msg IS NOT INITIAL.

* MSG processing
  LOOP AT it_msg  ASSIGNING <fs_l_msg>.
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
  ENDLOOP.

* Call MSG show
  CALL SCREEN 9001.

ENDFORM.                    " FRM_OUTPUT_MSG
*&---------------------------------------------------------------------*
*&      Module  9001_PBO_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE 9001_pbo_status OUTPUT.

* PF status
  SET PF-STATUS 'PF_9001'.

* Title bar
  SET TITLEBAR 'TB_9001'.

* Init MSG ALV
  PERFORM frm_init_msg.

ENDMODULE.                 " 9001_PBO_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_init_msg .

  DATA: it_l_fcat       TYPE  lvc_t_fcat,
        e_l_layout      TYPE  lvc_s_layo,
        e_l_variant     TYPE  disvariant.

  IF g_container IS INITIAL.
    "instantiate the Custom Area
    CREATE OBJECT g_container
      EXPORTING
        container_name = 'CS_MSG'
      EXCEPTIONS
        others         = 1.

    "instantiate the ALV Grid
    CREATE OBJECT g_alv_grid
      EXPORTING
        i_parent          = g_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    "ALV Config
    PERFORM  frm_config_alv   CHANGING  e_l_layout
                                        e_l_variant
                                        it_l_fcat.

    "show ALV data - 1st initial
    CALL METHOD g_alv_grid->set_table_for_first_display
      EXPORTING
        i_bypassing_buffer            = 'X'
        is_variant                    = e_l_variant
        i_save                        = 'A'
        is_layout                     = e_l_layout
      CHANGING
        it_outtab                     = it_msg
        it_fieldcatalog               = it_l_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    "Set ready for input
    CALL METHOD g_alv_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 0.

  ELSE.
    CALL METHOD g_alv_grid->refresh_table_display
      EXPORTING
        i_soft_refresh = ' '.
  ENDIF.

ENDFORM.                    " FRM_INIT_MSG
*&---------------------------------------------------------------------*
*&      Form  FRM_CONFIG_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_E_L_LAYOUT  text
*      <--P_E_L_VARIANT  text
*      <--P_IT_L_FCAT  text
*----------------------------------------------------------------------*
FORM frm_config_alv  CHANGING pv_e_layout   TYPE  lvc_s_layo
                              pv_e_variant  TYPE  disvariant
                              pv_t_fcat     TYPE  lvc_t_fcat.

*"1. ALV Control: Layout
  CLEAR:pv_e_layout.
  pv_e_layout-sel_mode = 'B'.
*  pv_e_layout-stylefname = 'CELL_STYL'."cell_styl
  pv_e_layout-no_rowmove = 'X'.
  pv_e_layout-cwidth_opt = 'X'.

*"2. ALV Control: Variant
  CLEAR:pv_e_variant.
  pv_e_variant-report = sy-repid.

*"3. ALV Control: field catelog
  PERFORM frm_alv_fieldcat CHANGING pv_t_fcat.

ENDFORM.                    " FRM_CONFIG_ALV
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_PT_T_FCAT  text
*----------------------------------------------------------------------*
FORM frm_alv_fieldcat  CHANGING pv_t_fcat     TYPE  lvc_t_fcat.


  PERFORM frm_field_cat USING: 'MSG_LCD'
                                text-mh1
                                5
                                ' '
                                ' '
                                space
                     CHANGING pv_t_fcat.
  PERFORM frm_field_cat USING: 'WERKS'
                                text-mh2
                                10
                                'WERKS'
                                'T001W'
                                space
                     CHANGING pv_t_fcat.
  PERFORM frm_field_cat USING: 'ARBPL'
                                text-mh3
                                10
                                'ARBPL'
                                'CRHD_API01'
                                space
                     CHANGING pv_t_fcat.
  PERFORM frm_field_cat USING: 'KTEXT'
                                text-mh4
                                40
                                'KTEXT'
                                'CRHD_API01'
                                space
                     CHANGING pv_t_fcat.
  PERFORM frm_field_cat USING: 'KAPART1'
                                text-mh5
                                10
                                'KAPART'
                                'CRHD_API05'
                                space
                     CHANGING pv_t_fcat.
  PERFORM frm_field_cat USING: 'KAPART2'
                                text-mh5
                                10
                                'KAPART'
                                'CRHD_API05'
                                space
                     CHANGING pv_t_fcat.
  PERFORM frm_field_cat USING: 'BEGZT'
                                text-mh6
                                10
                                'BEGZT'
                                'KAPA_API02'
                                space
                     CHANGING pv_t_fcat.
  PERFORM frm_field_cat USING: 'ENDZT'
                                text-mh7
                                10
                                'ENDZT'
                                'KAPA_API02'
                                space
                     CHANGING pv_t_fcat.
  PERFORM frm_field_cat USING: 'AZNOR1'
                                text-mh8
                                10
                                'AZNOR'
                                'KAPA_API02'
                                space
                     CHANGING pv_t_fcat.
  PERFORM frm_field_cat USING: 'AZNOR2'
                                text-mh8
                                10
                                'AZNOR'
                                'KAPA_API02'
                                space
                     CHANGING pv_t_fcat.
  PERFORM frm_field_cat USING: 'KOSTL'
                                text-mh9
                                10
                                'KOSTL'
                                'CRCO_API01'
                                space
                     CHANGING pv_t_fcat.
  PERFORM frm_field_cat USING: 'MSG_TXT'
                                text-mh0
                                255
                                ' '
                                ' '
                                space
                     CHANGING pv_t_fcat.

ENDFORM.                    " FRM_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0609   text
*      -->P_TEXT_D01  text
*      -->P_4      text
*      -->P_0612   text
*      -->P_0613   text
*      <--P_PV_T_FCAT  text
*----------------------------------------------------------------------*
FORM frm_field_cat  USING   pr_fname
                             pr_selt
                             pr_outlen
                             pr_rtname
                             pr_rfname
                             pr_edit
                    CHANGING pv_t_fcat     TYPE  lvc_t_fcat.

  DATA:wa_l_fcat        TYPE  lvc_s_fcat.

  g_pos = g_pos + 1.
  MOVE: g_pos         TO wa_l_fcat-col_pos,
        pr_fname      TO wa_l_fcat-fieldname,
        'IT_MSG'      TO wa_l_fcat-tabname,
        pr_selt       TO wa_l_fcat-scrtext_l,
        pr_outlen     TO wa_l_fcat-outputlen,
        pr_rtname     TO wa_l_fcat-ref_table,
        pr_rfname     TO wa_l_fcat-ref_field,
        pr_edit       TO wa_l_fcat-edit.
  APPEND wa_l_fcat TO pv_t_fcat.


ENDFORM.                    " FRM_FIELD_CAT
*&---------------------------------------------------------------------*
*&      Module  9001_PAI_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE 9001_pai_exit INPUT.
  IF g_okcode EQ '%EX'
  OR g_okcode EQ 'RW'.
    CLEAR:g_okcode.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.                 " 9001_PAI_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  9001_PAI_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE 9001_pai_command INPUT.

  IF g_okcode = 'BACK'
  OR g_okcode = 'RW'
  OR g_okcode = '%EX'.
    CLEAR:g_okcode.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.                 " 9001_PAI_COMMAND  INPUT
