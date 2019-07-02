* Program ID/Name:  ZMMI005          Date written: 2013/07/17
* Author's name:  HP_SJF            Last update:
* Program title:
* Project Name:  ZFSS SAP Project
* Version:
* Function Spec ID:ZFSS_SAP_FS_MM_01_01_批量导入合同价格_V1.0
*----------------------------------------------------------------------*
* Description: 根据用户提供的采购信息记录模板，批量处理信息记录。
*----------------------------------------------------------------------*
* Include:
*
*----------------------------------------------------------------------*
* Calls: (RFC and BAPI)
* ZSD_COND_SAVE_A
*----------------------------------------------------------------------*
* Function Modules:
*
*----------------------------------------------------------------------*
* Table:
*
*----------------------------------------------------------------------*
* Result:
*
*---------------------------------------------------------------------*
* Change History
*----------------------------------------------------------------------*
*     Date   |   Programmer   |   Corr. #   |   Description
* 2013/07/17 |  HP_SJF        |   Init      |
* 2015/08/27 |  HP_SJF        |   Modi      |  开票清单暂作价修改

REPORT zmmi005.
TYPE-POOLS: slis, icon.
************************************************************************
*        TABLES
************************************************************************
*TABLES:
************************************************************************
*        CONSTANTS
************************************************************************
CONSTANTS: c_begin_col   TYPE i VALUE '2',     "开始列
           c_begin_row   TYPE i VALUE '6',     "开始行
           c_end_col     TYPE i VALUE '16',    "结束列
           c_end_row     TYPE i VALUE '65535', "结束行
           c_update      TYPE c VALUE 'S',
           c_me11(4)     TYPE c VALUE 'ME11',
           c_me12(4)     TYPE c VALUE 'ME12'.
************************************************************************
*        DATA
************************************************************************
DATA: g_rc        TYPE i,
      g_separator TYPE c,
      g_mode      TYPE c VALUE 'N'.
************************************************************************
*        INTERNAL TABLES
************************************************************************
TYPES:  BEGIN OF t_excel,
        text(8000) TYPE c,
  END OF t_excel.
TYPES: BEGIN OF t_msg,
        msg_typ(01)   TYPE c,
        msg_lcd       TYPE icon_d,
        msg_txt(255),
        lifnr     TYPE eina-lifnr,
        matnr     TYPE eina-matnr,
        ekorg     TYPE eine-ekorg,
        werks     TYPE eine-werks,
        esokz     TYPE eine-esokz,
        mwskz     TYPE eine-mwskz,
        netpr     TYPE eine-netpr,
        waers     TYPE eine-waers,
        peinh     TYPE eine-peinh,
        bprme     TYPE eine-bprme,
        datab     TYPE a017-datab,
        datbi     TYPE a017-datbi,
        kzust     TYPE konh-kzust,
        urztp     TYPE eina-urztp,
        urzzt     TYPE eina-urzzt,                          "20150827
  END OF t_msg.
TYPES: BEGIN OF t_orig_data,
        lifnr(10),
        matnr(18),
        ekorg(4),
        werks(4),
        esokz(1),
        mwskz(2),
        netpr(20),
        waers(5),
        peinh(5),
        bprme(3),
        datab(10),
        datbi(10),
        kzust(3),
        urztp(1),
        urzzt(16),                                          "20150827
  END OF t_orig_data.
TYPES: BEGIN OF t_main,
        lifnr     TYPE eina-lifnr,
        matnr     TYPE eina-matnr,
        ekorg     TYPE eine-ekorg,
        werks     TYPE eine-werks,
        esokz     TYPE eine-esokz,
        mwskz     TYPE eine-mwskz,
        netpr     TYPE eine-netpr,
        waers     TYPE eine-waers,
        peinh     TYPE eine-peinh,
        bprme     TYPE eine-bprme,
        datab     TYPE a017-datab,
        datbi     TYPE a017-datbi,
        kzust     TYPE konh-kzust,
        urztp     TYPE eina-urztp,
        urzzt     TYPE eina-urzzt,                          "20150827
  END OF t_main.
TYPES: BEGIN OF t_check,
        lifnr     TYPE eina-lifnr,
        matnr     TYPE eina-matnr,
        ekorg     TYPE eine-ekorg,
        werks     TYPE eine-werks,
        esokz     TYPE eine-esokz,
        datab     TYPE a017-datab,
        datbi     TYPE a017-datbi,
  END OF t_check.
DATA: it_excel    TYPE STANDARD TABLE OF t_excel,
      wa_excel    TYPE t_excel,
      it_main     TYPE STANDARD TABLE OF t_main,
      wa_main     TYPE t_main,
      it_msg      TYPE STANDARD TABLE OF t_msg,
      it_check    TYPE STANDARD TABLE OF t_check,
      it_a017     TYPE STANDARD TABLE OF a017.
DATA: it_file     TYPE filetable.
***BDC Table
DATA: it_bdcdata  TYPE TABLE OF bdcdata,
      wa_bdcdata  TYPE bdcdata,
      it_mtab     TYPE TABLE OF bdcmsgcoll,
      wa_mtab     TYPE bdcmsgcoll.
***ALV
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      g_layout    TYPE slis_layout_alv,
      g_sort      TYPE slis_t_sortinfo_alv,
      g_event     TYPE slis_t_event.
DATA: g_repid     TYPE sy-repid.
************************************************************************
*        PARAMETERS
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.
PARAMETERS: p_file     TYPE rlgrap-filename .
PARAMETERS: p_mode     TYPE c DEFAULT 'N'.
SELECTION-SCREEN END OF BLOCK b01.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CLEAR: it_file, g_rc.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = ''
    CHANGING
      file_table              = it_file
      rc                      = g_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc = 0.
    READ TABLE it_file INTO p_file INDEX 1.
  ELSE.
    EXIT.
  ENDIF.
************************************************************************
*----------------------------------------------------------------------*
*
*        MAIN PROGRAM
*
*----------------------------------------------------------------------*
************************************************************************
START-OF-SELECTION.
  PERFORM frm_get_main_data.
  PERFORM frm_deal_data.
  PERFORM frm_display_msg.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_MAIN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_main_data .
  CLEAR:g_separator.
  CHECK NOT p_file IS INITIAL.
  CALL FUNCTION 'ZZZ_UPLOADEXCEL'
    EXPORTING
      im_filename             = p_file
      im_begin_col            = c_begin_col
      im_begin_row            = c_begin_row
      im_end_col              = c_end_col
      im_end_row              = c_end_row
    IMPORTING
      ex_separator            = g_separator
    TABLES
      it_exceltab             = it_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE e899(mm) WITH '上传EXCEL出错'.
    EXIT.
  ENDIF.

  IF it_excel IS INITIAL.
    MESSAGE e899(mm) WITH '上传EXCEL数据为空'.
    EXIT.
  ELSE.
* 取数据
    SELECT a~lifnr
           a~matnr
           b~ekorg
           b~werks
           b~esokz
      INTO TABLE it_check
      FROM eina AS a
      INNER JOIN eine AS b
      ON a~infnr = b~infnr.

    SELECT * FROM a017 INTO TABLE it_a017.
  ENDIF.
ENDFORM.                    " FRM_GET_MAIN_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_deal_data .
  DATA: wa_orig_data  TYPE t_orig_data,
        wa_msg        TYPE t_msg,
        wa_check      TYPE t_check,
        wa_a017       TYPE a017.
  DATA: l_lifnr       TYPE lfa1-lifnr,
        l_matnr       TYPE mara-matnr,
        l_werks       TYPE t001w-werks,
        l_me(4)       TYPE c,
        l_flg(1)      TYPE c."A: APPEND; M: MODIFY

***形成上传数据
  LOOP AT it_excel INTO wa_excel.
    CLEAR:wa_orig_data.
    SPLIT wa_excel AT g_separator
     INTO wa_orig_data-lifnr
          wa_orig_data-matnr
          wa_orig_data-ekorg
          wa_orig_data-werks
          wa_orig_data-esokz
          wa_orig_data-mwskz
          wa_orig_data-netpr
          wa_orig_data-waers
          wa_orig_data-peinh
          wa_orig_data-bprme
          wa_orig_data-datab
          wa_orig_data-datbi
          wa_orig_data-kzust
          wa_orig_data-urztp
          wa_orig_data-urzzt.                               "20150827
    CONDENSE:wa_orig_data-netpr NO-GAPS.
* 检查上传数据
    CLEAR: wa_msg.
    MOVE: wa_orig_data-lifnr  TO wa_msg-lifnr,
          wa_orig_data-matnr  TO wa_msg-matnr,
          wa_orig_data-ekorg  TO wa_msg-ekorg,
          wa_orig_data-werks  TO wa_msg-werks,
          wa_orig_data-esokz  TO wa_msg-esokz,
          wa_orig_data-mwskz  TO wa_msg-mwskz,
          wa_orig_data-waers  TO wa_msg-waers,
          wa_orig_data-bprme  TO wa_msg-bprme,
          wa_orig_data-datab  TO wa_msg-datab,
          wa_orig_data-datbi  TO wa_msg-datbi,
          wa_orig_data-kzust  TO wa_msg-kzust,
          wa_orig_data-urztp  TO wa_msg-urztp,
          wa_orig_data-urzzt  TO wa_msg-urzzt.              "20150827
    "Convert to Program
    CATCH SYSTEM-EXCEPTIONS convt_no_number = 1
                           OTHERS = 8.
      CLEAR: wa_main.
      MOVE: wa_orig_data-lifnr  TO wa_main-lifnr,
            wa_orig_data-matnr  TO wa_main-matnr,
            wa_orig_data-ekorg  TO wa_main-ekorg,
            wa_orig_data-werks  TO wa_main-werks,
            wa_orig_data-esokz  TO wa_main-esokz,
            wa_orig_data-mwskz  TO wa_main-mwskz,
            wa_orig_data-netpr  TO wa_main-netpr,
            wa_orig_data-waers  TO wa_main-waers,
            wa_orig_data-peinh  TO wa_main-peinh,
            wa_orig_data-bprme  TO wa_main-bprme,
            wa_orig_data-datab  TO wa_main-datab,
            wa_orig_data-datbi  TO wa_main-datbi,
            wa_orig_data-kzust  TO wa_main-kzust,
            wa_orig_data-urztp  TO wa_main-urztp,
            wa_orig_data-urzzt  TO wa_main-urzzt.           "20150827
    ENDCATCH.
    IF sy-subrc <> 0.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MOVE text-m01 TO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ELSE.
      MOVE wa_main-netpr TO wa_msg-netpr.
      MOVE wa_main-peinh TO wa_msg-peinh.
    ENDIF.
*   初始化检查
    IF wa_main-lifnr IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e001(zmm_msg) WITH text-t01
        INTO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_main-matnr IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e001(zmm_msg) WITH text-t02
        INTO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_main-ekorg IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e001(zmm_msg) WITH text-t03
        INTO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_main-werks IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e001(zmm_msg) WITH text-t04
        INTO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_main-mwskz IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e001(zmm_msg) WITH text-t05
        INTO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_main-netpr IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e001(zmm_msg) WITH text-t12
        INTO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_main-waers IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e001(zmm_msg) WITH text-t06
        INTO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_main-peinh IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e001(zmm_msg) WITH text-t07
        INTO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_main-bprme IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e001(zmm_msg) WITH text-t08
        INTO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_main-datab IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e001(zmm_msg) WITH text-t09
        INTO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_main-datbi IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e001(zmm_msg) WITH text-t10
        INTO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_main-kzust IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e001(zmm_msg) WITH text-t11
        INTO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
***检查CPC零件的数据20150827
    TRANSLATE wa_main-urzzt TO UPPER CASE.
    IF NOT wa_main-urzzt IS INITIAL AND wa_main-urzzt <> 'CPC'.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e000(zmm_msg) WITH text-t13
        INTO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
* 检查数据准确性
    CLEAR: l_lifnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_main-lifnr
      IMPORTING
        output = wa_main-lifnr.
    SELECT SINGLE lifnr FROM lfa1 INTO l_lifnr WHERE lifnr = wa_main-lifnr.
    IF sy-subrc <> 0.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e071(zmm_msg) WITH wa_main-lifnr
        INTO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
    CLEAR:l_matnr.
***转化物料大小写
    TRANSLATE wa_main-matnr TO UPPER CASE.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = wa_main-matnr
      IMPORTING
        output       = wa_main-matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
    SELECT SINGLE matnr FROM mara INTO l_matnr WHERE matnr = wa_main-matnr.
    IF sy-subrc <> 0.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e002(zmm_msg) WITH wa_main-matnr
        INTO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
    CLEAR: l_werks.
    SELECT SINGLE werks FROM t001w INTO l_werks WHERE werks = wa_main-werks.
    IF sy-subrc <> 0.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e003(zmm_msg) WITH wa_main-werks
        INTO wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.

    CLEAR: it_mtab, wa_mtab.
    CLEAR: wa_check.
    READ TABLE it_check INTO wa_check WITH KEY lifnr = wa_main-lifnr
                                               matnr = wa_main-matnr
                                               ekorg = wa_main-ekorg
                                               werks = wa_main-werks
                                               esokz = wa_main-esokz.
    IF sy-subrc = 0."如果查找到，继续查找有效起始日和有效终止日
      CLEAR: wa_a017.
      READ TABLE it_a017 INTO wa_a017 WITH KEY lifnr = wa_main-lifnr
                                               matnr = wa_main-matnr
                                               ekorg = wa_main-ekorg
                                               werks = wa_main-werks
                                               esokz = wa_main-esokz
                                               datab = wa_main-datab
                                               datbi = wa_main-datbi.
      IF sy-subrc = 0."相同记录的话覆盖，修改价格时选择
        "old
*        L_FLG = 'A'.
        "new
        l_flg = 'M'.
        PERFORM frm_bdc_change_valid_data USING wa_main   "renew by HANDWJ 20181114--
                                                l_flg.
*        PERFORM FRM_BDC_CREATE_VALID_DATA USING WA_MAIN   "delete by HANDWJ 20181114--
*                                                L_FLG.
      ELSE."不相同的话，修改有效期
        l_flg = 'A'.
        PERFORM frm_bdc_create_valid_data USING wa_main
                                                l_flg.
      ENDIF.
      CALL TRANSACTION c_me12 USING it_bdcdata
                              MODE p_mode
                              UPDATE c_update
                              MESSAGES
                              INTO it_mtab.
      l_me = c_me12.
    ELSE."如果没有找到相同记录就创建
      PERFORM frm_bdc_create_info USING wa_main.
      CALL TRANSACTION c_me11 USING it_bdcdata
                              MODE p_mode
                              UPDATE c_update
                              MESSAGES
                              INTO it_mtab.
      l_me = c_me11.
    ENDIF.
    CLEAR: it_bdcdata.

    READ TABLE it_mtab INTO wa_mtab WITH KEY msgtyp = 'S' msgid = '06'.
    IF sy-subrc = 0.
      IF  wa_mtab-msgnr = '331' OR wa_mtab-msgnr = '335' OR wa_mtab-msgnr = '336'.
        CLEAR: wa_msg.
        MOVE  'S' TO wa_msg-msg_typ.
        MOVE icon_led_green TO wa_msg-msg_lcd.
        MOVE: wa_main-lifnr  TO wa_msg-lifnr,
              wa_main-matnr  TO wa_msg-matnr,
              wa_main-ekorg  TO wa_msg-ekorg,
              wa_main-werks  TO wa_msg-werks,
              wa_main-esokz  TO wa_msg-esokz,
              wa_main-mwskz  TO wa_msg-mwskz,
              wa_main-waers  TO wa_msg-waers,
              wa_main-netpr  TO wa_msg-netpr,
              wa_main-peinh  TO wa_msg-peinh,
              wa_main-bprme  TO wa_msg-bprme,
              wa_main-datab  TO wa_msg-datab,
              wa_main-datbi  TO wa_msg-datbi,
              wa_main-kzust  TO wa_msg-kzust,
              wa_main-urztp  TO wa_msg-urztp,
              wa_main-urzzt  TO wa_msg-urzzt.

        IF l_me = c_me11.
          CONCATENATE text-m04 wa_mtab-msgv1 text-m02 INTO wa_msg-msg_txt.
        ELSE.
          CONCATENATE text-m04 wa_mtab-msgv1 text-m03 INTO wa_msg-msg_txt.
        ENDIF.
        APPEND wa_msg TO it_msg.
      ELSE.
        CLEAR: wa_msg.
        MOVE  wa_mtab-msgtyp TO wa_msg-msg_typ.
        MOVE: wa_main-lifnr  TO wa_msg-lifnr,
              wa_main-matnr  TO wa_msg-matnr,
              wa_main-ekorg  TO wa_msg-ekorg,
              wa_main-werks  TO wa_msg-werks,
              wa_main-esokz  TO wa_msg-esokz,
              wa_main-mwskz  TO wa_msg-mwskz,
              wa_main-waers  TO wa_msg-waers,
              wa_main-netpr  TO wa_msg-netpr,
              wa_main-peinh  TO wa_msg-peinh,
              wa_main-bprme  TO wa_msg-bprme,
              wa_main-datab  TO wa_msg-datab,
              wa_main-datbi  TO wa_msg-datbi,
              wa_main-kzust  TO wa_msg-kzust,
              wa_main-urztp  TO wa_msg-urztp,
              wa_main-urzzt  TO wa_msg-urzzt.
        MESSAGE ID wa_mtab-msgid
                TYPE 'S'
                NUMBER wa_mtab-msgnr
                WITH wa_mtab-msgv1
                     wa_mtab-msgv2
                     wa_mtab-msgv3
                     wa_mtab-msgv4
                INTO wa_msg-msg_txt.
        MOVE icon_led_green TO wa_msg-msg_lcd.
        APPEND wa_msg TO it_msg.
      ENDIF.
    ELSE."不成功信息
      LOOP AT it_mtab INTO wa_mtab WHERE msgtyp = 'E' OR msgtyp = 'A'  .
        CLEAR: wa_msg.
        MOVE  wa_mtab-msgtyp TO wa_msg-msg_typ.
        MOVE: wa_main-lifnr  TO wa_msg-lifnr,
              wa_main-matnr  TO wa_msg-matnr,
              wa_main-ekorg  TO wa_msg-ekorg,
              wa_main-werks  TO wa_msg-werks,
              wa_main-esokz  TO wa_msg-esokz,
              wa_main-mwskz  TO wa_msg-mwskz,
              wa_main-waers  TO wa_msg-waers,
              wa_main-netpr  TO wa_msg-netpr,
              wa_main-peinh  TO wa_msg-peinh,
              wa_main-bprme  TO wa_msg-bprme,
              wa_main-datab  TO wa_msg-datab,
              wa_main-datbi  TO wa_msg-datbi,
              wa_main-kzust  TO wa_msg-kzust,
              wa_main-urztp  TO wa_msg-urztp,
              wa_main-urzzt  TO wa_msg-urzzt.
        MESSAGE ID wa_mtab-msgid
                TYPE 'S'
                NUMBER wa_mtab-msgnr
                WITH wa_mtab-msgv1
                     wa_mtab-msgv2
                     wa_mtab-msgv3
                     wa_mtab-msgv4
                INTO wa_msg-msg_txt.
        IF wa_msg-msg_typ = 'E'.
          MOVE icon_led_red TO wa_msg-msg_lcd.
        ELSE.
          MOVE icon_led_green TO wa_msg-msg_lcd.
        ENDIF.
        APPEND wa_msg TO it_msg.
        EXIT.
      ENDLOOP.
    ENDIF.
    CLEAR: l_me.
  ENDLOOP.
ENDFORM.                    " FRM_DEAL_DATA
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR wa_bdcdata.
  wa_bdcdata-program  = program.
  wa_bdcdata-dynpro   = dynpro.
  wa_bdcdata-dynbegin = 'X'.
  APPEND wa_bdcdata TO it_bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR wa_bdcdata.
  wa_bdcdata-fnam = fnam.
  wa_bdcdata-fval = fval.
  APPEND wa_bdcdata TO it_bdcdata.
ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  FRM_BDC_CREATE_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PA_MAIN  text
*----------------------------------------------------------------------*
FORM frm_bdc_create_info  USING    pa_main STRUCTURE wa_main.

  DATA: l_datab(10),
        l_datbi(10),
        l_netpr(14),
        l_peinh(5).

  IF pa_main-waers = 'JPY'.
    l_netpr = pa_main-netpr * 100 / 100.
  ELSE.
    l_netpr = pa_main-netpr.
  ENDIF.

  l_datab = pa_main-datab.
  l_peinh = pa_main-peinh.
  l_datbi = pa_main-datbi.

  PERFORM frm_call_sapmm06i_0100 USING pa_main.
  PERFORM bdc_dynpro      USING 'SAPMM06I' '0101'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EINA-URZTP'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'EINA-URZTP'
                                pa_main-urztp.
  PERFORM bdc_field       USING 'EINA-URZZT'
                                pa_main-urzzt.              "20150827

  PERFORM bdc_dynpro      USING 'SAPMM06I' '0102'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EINE-MWSKZ'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=KO'.
  PERFORM bdc_field       USING 'EINE-MWSKZ'
                                pa_main-mwskz.

  PERFORM bdc_dynpro      USING 'SAPMV13A' '0201'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KONP-KBETR(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=KDAT'.
  PERFORM bdc_field       USING 'RV13A-DATAB'
                                l_datab.
  PERFORM bdc_field       USING 'RV13A-DATBI'
                                l_datbi.
  PERFORM bdc_field       USING 'KONP-KBETR(01)'
                                l_netpr.
  PERFORM bdc_field       USING 'KONP-KMEIN(01)'
                                pa_main-bprme.
  PERFORM bdc_field       USING 'KONP-KPEIN(01)'
                                l_peinh.
  PERFORM bdc_field       USING 'KONP-KONWA(01)'
                                pa_main-waers.

  PERFORM bdc_dynpro      USING 'SAPMV13A' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KONH-KZUST'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SICH'.
  PERFORM bdc_field       USING 'RV13A-DATAB'
                                l_datab.
  PERFORM bdc_field       USING 'RV13A-DATBI'
                                l_datbi.
  PERFORM bdc_field       USING 'KONH-KZUST'
                                pa_main-kzust.

ENDFORM.                    " FRM_BDC_CREATE_INFO
*&---------------------------------------------------------------------*
*&      Form  FRM_BDC_CHANGE_VALID_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PA_MAIN  text
*----------------------------------------------------------------------*
FORM frm_bdc_change_valid_data  USING    pa_main STRUCTURE wa_main
                                          p_flg   TYPE c.
  PERFORM frm_call_sapmm06i_0100 USING pa_main.
  PERFORM frm_call_sapmm06i_0101.
  "add by HANDWJ 20181114 begin--
  DATA:lt_a017  TYPE TABLE OF a017 WITH HEADER LINE,
       lv_num   TYPE c LENGTH 4,
       lv_num1  TYPE SY-TABIX VALUE '0001',
       lv_num2  TYPE SY-TABIX,
       lv_new   TYPE SY-TABIX,
       lv_scrol TYPE SY-TABIX,
       lv_string(20).
  lt_a017[] = it_a017[].
  DELETE lt_a017 WHERE lifnr <> pa_main-lifnr
                    OR matnr <> pa_main-matnr
                    OR ekorg <> pa_main-ekorg
                    OR werks <> pa_main-werks.
  SORT lt_a017[] BY datab.
  DESCRIBE TABLE lt_a017[] LINES lv_num2.
  READ TABLE lt_a017 WITH KEY datab = pa_main-datab.
  lv_num = sy-tabix.
  lv_scrol = lv_num DIV 9.
  IF lv_num > 9.
    DO lv_scrol TIMES.
      PERFORM bdc_dynpro      USING 'SAPLV14A' '0102'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'VAKE-DATAB(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=P+'.
      "模拟翻页动作，预测并定位该有效期在价格条件弹框中的位置
      CALL FUNCTION 'SCROLLING_IN_TABLE'
        EXPORTING
          entry_act      = lv_num1
          entry_from     = 1
          entry_to       = lv_num2
          last_page_full = 'X'
          loops          = 9
          ok_code        = 'P+'
        IMPORTING
          entry_new      = lv_new.

      lv_num1 = lv_new.
    ENDDO.
    lv_num = lv_num - lv_new + 1.
  ENDIF.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_num
    IMPORTING
      output = lv_num.
  CONCATENATE 'VAKE-DATAB(' lv_num ')' INTO lv_string.
  "add by HANDWJ 20181114 end--
  PERFORM bdc_dynpro      USING 'SAPLV14A' '0102'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                "old
*                                'VAKE-DATAB(01)'.
                                "new
                                lv_string.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=PICK'.
  PERFORM frm_call_sapmv13a_0201 USING pa_main p_flg.
  PERFORM frm_call_sapmv13a_0200 USING pa_main.
ENDFORM.                    " FRM_BDC_CHANGE_VALID_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_BDC_CREATE_VALID_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PA_MAIN  text
*----------------------------------------------------------------------*
FORM frm_bdc_create_valid_data  USING    pa_main STRUCTURE wa_main
                                          p_flg  TYPE c.
  PERFORM frm_call_sapmm06i_0100 USING pa_main.
  PERFORM frm_call_sapmm06i_0101.
***20150909hp_sjf增加修改EPS大零件，CPC零件修改
  PERFORM bdc_field       USING 'EINA-URZTP'
                                pa_main-urztp.
  PERFORM bdc_field       USING 'EINA-URZZT'
                                pa_main-urzzt.              "20150909
***20150909hp_sjf
  PERFORM bdc_dynpro      USING 'SAPLV14A' '0102'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'VAKE-DATAB(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=NEWD'.
  PERFORM frm_call_sapmv13a_0201 USING pa_main p_flg.
  PERFORM frm_call_sapmv13a_0200 USING pa_main.
ENDFORM.                    " FRM_BDC_CREATE_VALID_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_CALL_SAPMM06I_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PS_MAIN  text
*----------------------------------------------------------------------*
FORM frm_call_sapmm06i_0100  USING    ps_main STRUCTURE wa_main.
  PERFORM bdc_dynpro      USING 'SAPMM06I' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EINA-LIFNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'EINA-LIFNR'
                                ps_main-lifnr.
  PERFORM bdc_field       USING 'EINA-MATNR'
                                ps_main-matnr.
  PERFORM bdc_field       USING 'EINE-EKORG'
                                ps_main-ekorg.
  PERFORM bdc_field       USING 'EINE-WERKS'
                                ps_main-werks.
  CASE ps_main-esokz.
    WHEN '0'.
      PERFORM bdc_field       USING 'RM06I-NORMB'
                                    'X'.
    WHEN '2'.
      PERFORM bdc_field       USING 'RM06I-KONSI'
                                    'X'.
    WHEN '3'.
      PERFORM bdc_field       USING 'RM06I-LOHNB'
                                    'X'.
    WHEN 'P'.
      PERFORM bdc_field       USING 'RM06I-PIPEL'
                                    'X'.
  ENDCASE.
ENDFORM.                    " FRM_CALL_SAPMM06I_0100
*&---------------------------------------------------------------------*
*&      Form  FRM_CALL_SAPMM06I_0101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_call_sapmm06i_0101 .
  PERFORM bdc_dynpro      USING 'SAPMM06I' '0101'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EINA-MAHN1'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=KO'.
ENDFORM.                    " FRM_CALL_SAPMM06I_0101
*&---------------------------------------------------------------------*
*&      Form  FRM_CALL_SAPMV13A_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PSMAIN  text
*----------------------------------------------------------------------*
FORM frm_call_sapmv13a_0200  USING    ps_main STRUCTURE wa_main.
  DATA: l_datab(10),
        l_datbi(10).

  l_datab = ps_main-datab.
  l_datbi = ps_main-datbi.
  PERFORM bdc_dynpro      USING 'SAPMV13A' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KONH-KZUST'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SICH'.
  PERFORM bdc_field       USING 'RV13A-DATAB'
                                l_datab.
  PERFORM bdc_field       USING 'RV13A-DATBI'
                                l_datbi.
  PERFORM bdc_field       USING 'KONH-KZUST'
                                ps_main-kzust.
ENDFORM.                    " FRM_CALL_SAPMV13A_0200
*&---------------------------------------------------------------------*
*&      Form  FRM_CALL_SAPMV13A_0201
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PS_MAIN  text
*----------------------------------------------------------------------*
FORM frm_call_sapmv13a_0201  USING    ps_main STRUCTURE wa_main
                                       p_fg  TYPE c.
  DATA: l_datab(10),
        l_datbi(10),
        l_netpr(14),
        l_peinh(5).

  IF ps_main-waers = 'JPY'.
    l_netpr = ps_main-netpr * 100 / 100.
  ELSE.
    l_netpr = ps_main-netpr.
  ENDIF.
  l_datab = ps_main-datab.
  l_peinh = ps_main-peinh.
  l_datbi = ps_main-datbi.

  PERFORM bdc_dynpro      USING 'SAPMV13A' '0201'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KONP-KBETR(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=KDAT'.
  PERFORM bdc_field       USING 'RV13A-DATAB'
                                l_datab.
  PERFORM bdc_field       USING 'RV13A-DATBI'
                                l_datbi.
  PERFORM bdc_field       USING 'KONP-KBETR(01)'
                                l_netpr.
  PERFORM bdc_field       USING 'KONP-KPEIN(01)'
                                l_peinh.
  IF p_fg <> 'M'.
    PERFORM bdc_field       USING 'KONP-KMEIN(01)'
                                  ps_main-bprme.
    PERFORM bdc_field       USING 'KONP-KONWA(01)'
                                  ps_main-waers.
  ENDIF.
ENDFORM.                    " FRM_CALL_SAPMV13A_0201
*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_display_msg .
  PERFORM frm_fill_fieldcat.
  g_repid = sy-repid.
  g_layout-colwidth_optimize = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = g_repid
      is_layout          = g_layout
      it_fieldcat        = gt_fieldcat[]
*     I_GRID_TITLE       = L_TITLE
      i_default          = 'X'
      i_save             = 'A'
    TABLES
      t_outtab           = it_msg.   "需要输出内容的内表
ENDFORM.                    " FRM_DISPLAY_MSG
*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_fill_fieldcat .
  PERFORM frm_get_fieldcat USING 'MSG_LCD' '状态'  '4'.
  PERFORM frm_get_fieldcat USING 'MSG_TYP' '类型'  '4'.
  PERFORM frm_get_fieldcat USING 'LIFNR' '供应商编码'  '10'.
  PERFORM frm_get_fieldcat USING 'MATNR' '物料号'  '20'.
  PERFORM frm_get_fieldcat USING 'EKORG' '采购组织'  '8'.
  PERFORM frm_get_fieldcat USING 'WERKS' '工厂'  '6'.
  PERFORM frm_get_fieldcat USING 'ESOKZ' '采购信息'      '8'.
  PERFORM frm_get_fieldcat USING 'MWSKZ' '税码'      '6'.
  PERFORM frm_get_fieldcat USING 'NETPR' '净价' '20'.
  PERFORM frm_get_fieldcat USING 'WAERS' '货币码'     '6'.
  PERFORM frm_get_fieldcat USING 'PEINH' '定价单位'  '6'.
  PERFORM frm_get_fieldcat USING 'BPRME' '订单价格单位'  '4'.
  PERFORM frm_get_fieldcat USING 'DATAB' '有效从'  '10'.
  PERFORM frm_get_fieldcat USING 'DATBI' '有效到'  '10'.
  PERFORM frm_get_fieldcat USING 'KZUST' '暂作价标识'  '6'.
  PERFORM frm_get_fieldcat USING 'URZZT' 'CPC零件'  '12'.
  PERFORM frm_get_fieldcat USING 'URZTP' 'EPS大零件'   '8'.
  PERFORM frm_get_fieldcat USING 'MSG_TXT' '消息文本'  '40'.
ENDFORM.                    " FRM_FILL_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1756   text
*      -->P_1757   text
*      -->P_1758   text
*----------------------------------------------------------------------*
FORM frm_get_fieldcat  USING    p_name
                                p_text
                                p_outputlen.
  gt_fieldcat-icon      = 'X'.
  gt_fieldcat-fieldname = p_name.
  gt_fieldcat-seltext_m = p_text.
  gt_fieldcat-outputlen = p_outputlen.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
ENDFORM.                    " FRM_GET_FIELDCAT
