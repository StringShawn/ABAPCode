*$*$********************************************************************
* Program ID/Name:  ZMMI_INIT_STOCK             Date written: 20130821
* Author's name:   HP_SJF                       Last update:
* Program title:  初始化库存
* Project Name:  EPR I
* Version:
* Function Spec ID:
*$*$********************************************************************
*----------------------------------------------------------------------*
* Description: 根据用户提供的采购信息记录模板，批量处理信息记录。
*----------------------------------------------------------------------*
* Include:
*
*----------------------------------------------------------------------*
* Calls: (RFC and BAPI)
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
* 2013/09/05 |  HP_SJF        |   Init      |


REPORT zmmi_init_stock.
*$*$********************************************************************
*$*$    TABLES                                                         *
*$*$********************************************************************
TABLES:vekp,vepo,lfa1,t001l,t001w,mara,mbew.

*$*$********************************************************************
*$*$    INCLUDES                                                       *
*$*$    (FOR INCLUDES WITH DECLARATIVE PURPOSES ONLY                   *
*$*$     I.E. BDC, ALV, ETC.)                                          *
*$*$********************************************************************


*$*$********************************************************************
*$*$    GLOBAL TYPES                                                   *
*$*$********************************************************************
TYPES: BEGIN OF t_msg,
        werks     TYPE werks_d ,
        lgort     TYPE lgort_d ,
        matnr     TYPE matnr ,
        menge     TYPE menge_d ,
        wrbtr     TYPE wrbtr,
        budat     TYPE budat,
        sobkz     TYPE sobkz,
        lifnr     TYPE lifnr ,
        exidv     TYPE exidv ,
        vemng     TYPE vemng ,
        lgpla     TYPE lgpla,
        ebeln     TYPE ebeln,
        inhalt    TYPE inhalt,
        kunnr     TYPE kunnr,
        z_shno    TYPE zz_shno,
        exidv2    TYPE exidv2,
        wdatu     TYPE lvs_wdatu,
        bestq     TYPE bestq,                               "20131122
        old_exidv TYPE exidv,                               "20131122
        bwart     TYPE bwart,
        msg_typ(1)    TYPE c,
        msg_lcd       TYPE icon_d,
        msg_txt(255),
  END OF t_msg.
TYPES: BEGIN OF t_main,
        werks     TYPE string,
        lgort     TYPE string ,
        matnr     TYPE string,
        menge     TYPE string ,
        wrbtr     TYPE string,
        budat     TYPE string,
        sobkz     TYPE string,
        lifnr     TYPE string,
        exidv     TYPE string,
        vemng     TYPE string,
        lgpla     TYPE string,
        ebeln     TYPE string,
        inhalt    TYPE string,
        kunnr     TYPE string,
        z_shno    TYPE string,
        exidv2    TYPE string,
        wdatu     TYPE string,
        bestq     TYPE string,                              "20131122
        old_exidv TYPE string,                              "20131122
  END OF t_main.
TYPES: BEGIN OF t_key,
        bwart   TYPE bwart,
        sobkz   TYPE sobkz,
        bestq   TYPE bestq,
END OF t_key.
*$*$********************************************************************
*$*$    GLOBAL CONSTANTS                                               *
*$*$********************************************************************
*CONSTANTS: C_X      TYPE C VALUE 'X'.

*$*$********************************************************************
*$*$    GLOBAL INTERNAL TABLES                                         *
*$*$********************************************************************
DATA: it_main       TYPE STANDARD TABLE OF t_main,
      wa_main       TYPE t_main,
      it_msg        TYPE STANDARD TABLE OF t_msg,
      wa_msg        TYPE t_msg,
      it_mat_menge  TYPE STANDARD TABLE OF t_msg,
      wa_mat_menge  TYPE t_msg,
      it_key        TYPE STANDARD TABLE OF t_key,
      wa_key        TYPE t_key,
      it_date       TYPE STANDARD TABLE OF t_msg,
      wa_date       TYPE t_msg,
      it_vekp       TYPE STANDARD TABLE OF vekp,
      wa_vekp       TYPE vekp,
      wa_vekp1      TYPE vekp.

DATA: it_file       TYPE filetable.
DATA: gs_layout     TYPE slis_layout_alv,
      gt_fieldcat   TYPE slis_t_fieldcat_alv,
      gt_exclude    TYPE ui_functions.

*$*$********************************************************************
*$*$    GLOBAL ELEMENTARY VARIABLES                                    *
*$*$********************************************************************
DATA: g_rc        TYPE i.
DATA: g_repid     TYPE sy-repid.

*$*$********************************************************************
*$*$    PARAMETERS & SELECT-OPTIONS                                    *
*$*$********************************************************************
SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: p_file     TYPE rlgrap-filename .

SELECTION-SCREEN:END OF BLOCK b1 .

*$*$********************************************************************
*$*$    INITIALIZATION                                                 *
*$*$********************************************************************
INITIALIZATION.
*$*$********************************************************************
*$*$    AT SELECTION-SCREEN  Output                                    *
*$*$********************************************************************
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

*$*$********************************************************************
*$*$    AT SELECTION-SCREEN                                            *
*$*$********************************************************************
AT SELECTION-SCREEN.
*$*$********************************************************************
*$*$    START-OF-SELECTION                                             *
*$*$********************************************************************
START-OF-SELECTION.

  PERFORM frm_upload_data .
  PERFORM frm_check_upload_data.
  PERFORM frm_display_alv .
*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_upload_data .
  DATA: l_file TYPE string.

  CLEAR: it_main, it_msg.

  l_file = p_file.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = l_file
      filetype                = 'ASC'
      has_field_separator     = 'X'
    TABLES
      data_tab                = it_main
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  IF sy-subrc <> 0.
    MESSAGE s027(zmm_msg) DISPLAY LIKE 'E'.
    STOP.
  ELSE.
    IF it_main IS INITIAL.
      MESSAGE s067(zmm_msg) DISPLAY LIKE 'E'.
      STOP.
    ELSE.
      DELETE it_main INDEX 1."删除表头
    ENDIF.
  ENDIF.

ENDFORM.                    " FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_check_upload_data .

  DATA: ls_main       TYPE t_main,
        ls_mbew       TYPE mbew,
        ls_marc       TYPE marc,
        ls_ekko       TYPE ekko,
        ls_zthulgor   TYPE zthulgor,
        ls_vekp       TYPE vekp.
  DATA: l_lifnr       TYPE lifnr,
        lc_lifnr      TYPE lifnr,
        l_matnr       TYPE matnr,
        l_werks       TYPE werks_d,
        l_lgort       TYPE lgort_d,
        l_exidv       TYPE string,
        l_sum_vemng   TYPE vemng,
        l_rmatp       TYPE pl_rmatp,
        l_ktokk       TYPE ktokk,
        l_flg(1)      TYPE c,
        l_count       TYPE i.
  DATA: l_err(1)      TYPE c."用于记录是否有错
  DATA: lt_main       TYPE STANDARD TABLE OF t_main."用于检查是否有相同SKU


  LOOP AT it_main INTO wa_main WHERE NOT exidv IS INITIAL.
    APPEND wa_main TO lt_main.
    CLEAR wa_main.
  ENDLOOP.

  CLEAR: wa_main, wa_msg, wa_mat_menge, wa_key, wa_date.
  CLEAR: it_mat_menge, it_key, it_date.
  LOOP AT it_main INTO wa_main.

    IF wa_main-budat IS INITIAL.
      wa_main-budat = sy-datum.
    ENDIF.
    TRANSLATE wa_main-matnr TO UPPER CASE.
    MOVE-CORRESPONDING wa_main TO wa_msg.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = wa_msg-matnr
      IMPORTING
        output = wa_msg-matnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_msg-lifnr
      IMPORTING
        output = wa_msg-lifnr.

    CLEAR: l_err.
*初始化检查，如有问题灯为红，报错误MESSAGE,
    IF wa_msg-werks IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e001(zmm_msg) WITH text-t01
        INTO wa_msg-msg_txt.
      l_err = 'X'.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_msg-lgort IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e001(zmm_msg) WITH text-t02
        INTO wa_msg-msg_txt.
      l_err = 'X'.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
    IF wa_msg-matnr IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e001(zmm_msg) WITH text-t03
        INTO wa_msg-msg_txt.
      l_err = 'X'.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
***供应商库存标记与供应商必须同时存在或同时为空
    IF wa_msg-sobkz IS INITIAL AND NOT wa_msg-lifnr IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e000(zmm_msg) WITH text-t04
        INTO wa_msg-msg_txt.
      l_err = 'X'.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ELSEIF NOT wa_msg-sobkz IS INITIAL AND wa_msg-lifnr IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e000(zmm_msg) WITH text-t04
        INTO wa_msg-msg_txt.
      l_err = 'X'.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ELSEIF NOT wa_msg-sobkz IS INITIAL AND NOT wa_msg-lifnr IS INITIAL.
***查询信息记录
      CLEAR: l_ktokk, l_flg.
      SELECT SINGLE ktokk INTO l_ktokk FROM lfa1 WHERE lifnr = wa_msg-lifnr.
      IF l_ktokk = 'Z002'.
        CLEAR: ls_ekko.
        SELECT SINGLE * INTO ls_ekko FROM ekko WHERE ebeln = wa_msg-ebeln.
        CALL FUNCTION 'ZMM_GET_INFO_RECORD'
          EXPORTING
            i_bedat     = ls_ekko-bedat
            i_matnr     = wa_msg-matnr
            i_lifnr     = wa_msg-lifnr
            i_sobkz     = wa_msg-sobkz
            i_werks     = wa_msg-werks
            i_ekorg     = ls_ekko-ekorg
            i_bsart     = ls_ekko-bsart
            i_ktokk     = l_ktokk
            i_flg_check = 'X'
          IMPORTING
            e_flg_zero  = l_flg.
        IF NOT l_flg IS INITIAL.
          MOVE 'E' TO wa_msg-msg_typ.
          MOVE icon_led_red TO wa_msg-msg_lcd.
          MESSAGE e058(zmm_msg) WITH wa_msg-matnr wa_msg-exidv ls_ekko-bedat
            INTO wa_msg-msg_txt.
          l_err = 'X'.
          APPEND wa_msg TO it_msg.
          CONTINUE.
        ENDIF.
*      ELSE.
*        CALL FUNCTION 'ZMM_GET_INFO_RECORD'
*          EXPORTING
*            I_BEDAT     = WA_MSG-BUDAT
*            I_MATNR     = WA_MSG-MATNR
*            I_LIFNR     = WA_MSG-LIFNR
*            I_SOBKZ     = WA_MSG-SOBKZ
*            I_WERKS     = WA_MSG-WERKS
*            I_EKORG     = 'ZF00'
*            I_KTOKK     = L_KTOKK
*            I_FLG_CHECK = 'X'
*          IMPORTING
*            E_FLG_ZERO  = L_FLG.
*        IF NOT L_FLG IS INITIAL.
*          MOVE 'E' TO WA_MSG-MSG_TYP.
*          MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
*          MESSAGE E163(ZMM_MSG) WITH WA_MSG-MATNR WA_MSG-WERKS WA_MSG-LIFNR
*            INTO WA_MSG-MSG_TXT.
*          L_ERR = 'X'.
*          APPEND WA_MSG TO IT_MSG.
*          CONTINUE.
*        ENDIF.
      ENDIF.
    ENDIF.

    IF wa_msg-menge IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e063(zmm_msg) WITH text-t06
        INTO wa_msg-msg_txt.
      l_err = 'X'.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
***金额不能为0或为空
*    IF WA_MSG-WRBTR IS INITIAL.
*      MOVE 'E' TO WA_MSG-MSG_TYP.
*      MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
*      MESSAGE E001(ZMM_MSG) WITH TEXT-T07
*        INTO WA_MSG-MSG_TXT.
*      L_ERR = 'X'.
*      APPEND WA_MSG TO IT_MSG.
*      CONTINUE.
*    ENDIF.
*    IF WA_MSG-WRBTR = 0.
*      MOVE 'E' TO WA_MSG-MSG_TYP.
*      MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
*      MESSAGE E068(ZMM_MSG) WITH TEXT-T07
*        INTO WA_MSG-MSG_TXT.
*      L_ERR = 'X'.
*      APPEND WA_MSG TO IT_MSG.
*      CONTINUE.
*    ENDIF.

***检查数据准确性
    CLEAR: l_werks.
    SELECT SINGLE werks FROM t001w INTO l_werks WHERE werks = wa_msg-werks.
    IF sy-subrc <> 0.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e003(zmm_msg) WITH wa_msg-werks
        INTO wa_msg-msg_txt.
      l_err = 'X'.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.

    CLEAR: l_lgort.
    SELECT SINGLE werks FROM t001l INTO l_lgort WHERE werks = wa_msg-werks AND lgort = wa_msg-lgort.
    IF sy-subrc <> 0.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e069(zmm_msg) WITH wa_msg-lgort
        INTO wa_msg-msg_txt.
      l_err = 'X'.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.

    CLEAR:l_matnr.
    SELECT SINGLE matnr FROM mara INTO l_matnr WHERE matnr = wa_msg-matnr.
    IF sy-subrc <> 0.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e002(zmm_msg) WITH wa_msg-matnr
        INTO wa_msg-msg_txt.
      l_err = 'X'.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.

    CLEAR:l_matnr, ls_marc.
    SELECT SINGLE * FROM marc INTO ls_marc WHERE matnr = wa_msg-matnr AND werks = wa_msg-werks.
    IF ls_marc-prctr IS INITIAL.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e074(zmm_msg) WITH wa_msg-matnr wa_msg-werks
        INTO wa_msg-msg_txt.
      l_err = 'X'.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ELSEIF ls_marc-mmsta = '01'.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e161(zmm_msg) WITH wa_msg-matnr
        INTO wa_msg-msg_txt.
      l_err = 'X'.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
    CLEAR:l_matnr, ls_mbew.
    SELECT SINGLE * FROM mbew INTO ls_mbew WHERE matnr = wa_msg-matnr AND bwkey = wa_msg-werks.
    IF ls_mbew-pstat NA 'B' AND ls_mbew-pstat NA 'G'.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e075(zmm_msg) WITH wa_msg-matnr
        INTO wa_msg-msg_txt.
      l_err = 'X'.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.
***供应商检查
    IF NOT wa_msg-lifnr IS INITIAL.
      CLEAR: l_lifnr, lc_lifnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_msg-lifnr
        IMPORTING
          output = wa_msg-lifnr.
      lc_lifnr = wa_msg-lifnr.
      SELECT SINGLE lifnr FROM lfa1 INTO l_lifnr WHERE lifnr = lc_lifnr.
      IF sy-subrc <> 0.
        MOVE 'E' TO wa_msg-msg_typ.
        MOVE icon_led_red TO wa_msg-msg_lcd.
        MESSAGE e071(zmm_msg) WITH wa_msg-lifnr
          INTO wa_msg-msg_txt.
        l_err = 'X'.
        APPEND wa_msg TO it_msg.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF NOT wa_msg-exidv IS INITIAL.
***检查系统中是否已经存在此SKU
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_msg-exidv
        IMPORTING
          output = wa_msg-exidv.
      CLEAR: ls_vekp.
      SELECT SINGLE * INTO ls_vekp FROM vekp WHERE exidv = wa_msg-exidv AND status NOT IN ('0050' , '0060') .
      IF sy-subrc = 0.
        MOVE 'E' TO wa_msg-msg_typ.
        MOVE icon_led_red TO wa_msg-msg_lcd.
        CONCATENATE text-t10 wa_msg-exidv '在系统中已经存在' INTO wa_msg-msg_txt.
        l_err = 'X'.
        APPEND wa_msg TO it_msg.
        CONTINUE.
      ENDIF.
      IF wa_msg-vemng IS INITIAL.
        MOVE 'E' TO wa_msg-msg_typ.
        MOVE icon_led_red TO wa_msg-msg_lcd.
        MESSAGE e001(zmm_msg) WITH text-t08
          INTO wa_msg-msg_txt.
        l_err = 'X'.
        APPEND wa_msg TO it_msg.
        CONTINUE.
      ENDIF.
      IF wa_msg-wdatu IS INITIAL.
        MOVE 'E' TO wa_msg-msg_typ.
        MOVE icon_led_red TO wa_msg-msg_lcd.
        MESSAGE e001(zmm_msg) WITH text-t05
          INTO wa_msg-msg_txt.
        l_err = 'X'.
        APPEND wa_msg TO it_msg.
        CONTINUE.
      ENDIF.
***检查包装物料
      CLEAR: l_rmatp.
      SELECT SINGLE rmatp INTO l_rmatp FROM mara WHERE matnr = wa_msg-matnr.
      IF l_rmatp IS INITIAL.
        MOVE 'E' TO wa_msg-msg_typ.
        MOVE icon_led_red TO wa_msg-msg_lcd.
        MESSAGE e073(zmm_msg) WITH wa_msg-matnr
          INTO wa_msg-msg_txt.
        l_err = 'X'.
        APPEND wa_msg TO it_msg.
        CONTINUE.
      ENDIF.
***检查如果SKU存在，如果库位不是SKU管理，则报错
      CLEAR: ls_zthulgor.
      SELECT SINGLE * INTO ls_zthulgor FROM zthulgor WHERE zwerks = wa_msg-werks AND zlgort = wa_msg-lgort AND z_hums = 'X'.
      IF sy-subrc <> 0.
        MOVE 'E' TO wa_msg-msg_typ.
        MOVE icon_led_red TO wa_msg-msg_lcd.
        MESSAGE e164(zmm_msg) WITH wa_msg-werks wa_msg-lgort
          INTO wa_msg-msg_txt.
        l_err = 'X'.
        APPEND wa_msg TO it_msg.
        CONTINUE.
      ENDIF.
    ELSE.
***检查如果SKU不存在，如果库位是SKU管理，则报错
      CLEAR: ls_zthulgor.
      SELECT SINGLE * INTO ls_zthulgor FROM zthulgor WHERE zwerks = wa_msg-werks AND zlgort = wa_msg-lgort AND z_hums = 'X'.
      IF sy-subrc = 0.
        MOVE 'E' TO wa_msg-msg_typ.
        MOVE icon_led_red TO wa_msg-msg_lcd.
        MESSAGE e165(zmm_msg) WITH wa_msg-werks wa_msg-lgort
          INTO wa_msg-msg_txt.
        l_err = 'X'.
        APPEND wa_msg TO it_msg.
        CONTINUE.
      ENDIF.
    ENDIF.

***SKU号不能重复
    CLEAR: l_count.
    LOOP AT lt_main INTO ls_main WHERE NOT exidv IS INITIAL AND exidv = wa_main-exidv.
      l_count = l_count + 1.
    ENDLOOP.
    IF l_count > 1.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MESSAGE e070(zmm_msg) WITH wa_main-exidv
        INTO wa_msg-msg_txt.
      l_err = 'X'.
      APPEND wa_msg TO it_msg.
      CONTINUE.
    ENDIF.

    IF l_err IS INITIAL.
      wa_msg-bwart = '561'.
      MOVE 'S' TO wa_msg-msg_typ.
      MOVE icon_led_green TO wa_msg-msg_lcd.
      CLEAR wa_msg-msg_txt.
      APPEND wa_msg TO it_msg.
    ENDIF.

    wa_mat_menge-werks  = wa_msg-werks.
    wa_mat_menge-lgort  = wa_msg-lgort.
    wa_mat_menge-matnr  = wa_msg-matnr.
    wa_mat_menge-menge  = wa_msg-menge.
    wa_mat_menge-budat  = wa_msg-budat.
    wa_mat_menge-bwart  = wa_msg-bwart.
    wa_mat_menge-sobkz  = wa_msg-sobkz.
    wa_mat_menge-lifnr  = wa_msg-lifnr.
    wa_mat_menge-bestq  = wa_msg-bestq.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_mat_menge-lifnr
      IMPORTING
        output = wa_mat_menge-lifnr.

    APPEND wa_mat_menge TO it_mat_menge.

    wa_key-bwart = wa_msg-bwart.
    wa_key-sobkz = wa_msg-sobkz.
    wa_key-bestq = wa_msg-bestq.
    APPEND wa_key TO it_key.

    wa_date-budat = wa_msg-budat.
    APPEND wa_date TO it_date.

    CLEAR: wa_main, wa_msg, wa_mat_menge.

  ENDLOOP.

  SORT it_date BY budat.
  DELETE ADJACENT DUPLICATES FROM it_date.
  SORT it_key BY bwart sobkz bestq.
  DELETE ADJACENT DUPLICATES FROM it_key.
  SORT it_mat_menge BY werks lgort matnr lifnr bwart sobkz bestq.
  DELETE ADJACENT DUPLICATES FROM it_mat_menge.

***判断SKU的数量等于该物料，该工厂，该库位,该特殊库存,该供应商编码，该库存状态的数量
  LOOP AT it_mat_menge INTO wa_mat_menge.
    CLEAR: l_sum_vemng.
    LOOP AT it_msg INTO wa_msg
      WHERE exidv IS NOT INITIAL AND werks = wa_mat_menge-werks AND lgort = wa_mat_menge-lgort AND matnr = wa_mat_menge-matnr
        AND sobkz = wa_mat_menge-sobkz AND lifnr = wa_mat_menge-lifnr AND bestq = wa_mat_menge-bestq.
      l_sum_vemng = l_sum_vemng + wa_msg-vemng.
    ENDLOOP.
    IF l_sum_vemng <> wa_mat_menge-menge.
      MOVE 'E' TO wa_msg-msg_typ.
      MOVE icon_led_red TO wa_msg-msg_lcd.
      MOVE text-t09 TO wa_msg-msg_txt.
      MODIFY it_msg FROM wa_msg TRANSPORTING msg_typ msg_lcd msg_txt
      WHERE exidv IS NOT INITIAL AND werks = wa_mat_menge-werks AND lgort = wa_mat_menge-lgort AND matnr = wa_mat_menge-matnr
        AND sobkz = wa_mat_menge-sobkz AND lifnr = wa_mat_menge-lifnr AND bestq = wa_mat_menge-bestq.
    ENDIF.

  ENDLOOP.

  "add by handyxh 南京子转分 begin
  SELECT * INTO TABLE it_vekp FROM vekp FOR ALL ENTRIES IN it_msg
    WHERE exidv = it_msg-exidv.
  "add by handyxh 南京子转分 end

ENDFORM.                    " FRM_CHECK_UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_display_alv .

  PERFORM frm_fill_fieldcat.
  PERFORM frm_fill_layout.
  g_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = g_repid
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat[]
      i_save                   = 'A'
    TABLES
      t_outtab                 = it_msg.   "需要输出内容的内表

ENDFORM.                    " FRM_DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_fill_fieldcat .

  PERFORM frm_get_fieldcat USING 'MSG_LCD' '状态'  '4' 'X'.
  PERFORM frm_get_fieldcat USING 'WERKS'   '工厂'    '10' 'X'.
  PERFORM frm_get_fieldcat USING 'LGORT'   '库位'  '10' 'X'.
  PERFORM frm_get_fieldcat USING 'MATNR'   '物料号'  '10' 'X'.
  PERFORM frm_get_fieldcat USING 'MENGE'   '数量'    '8' ''.
*  PERFORM FRM_GET_FIELDCAT USING 'WRBTR'   '金额'      '6' ''.
  PERFORM frm_get_fieldcat USING 'BUDAT'   '过账日期'  '10' ''.
  PERFORM frm_get_fieldcat USING 'SOBKZ'   '供应商库存标识'  '10' ''.
  PERFORM frm_get_fieldcat USING 'LIFNR'   '供应商编号'  '18' ''.
  PERFORM frm_get_fieldcat USING 'EXIDV'   'SKU编号'    '10' ''.
  PERFORM frm_get_fieldcat USING 'VEMNG'   'SKU数量'  '18' ''.
  PERFORM frm_get_fieldcat USING 'LGPLA'   '仓位'  '30' ''.
  PERFORM frm_get_fieldcat USING 'EBELN'   '采购订单'    '10' ''.
  PERFORM frm_get_fieldcat USING 'INHALT'  '供应商批次'  '10' ''.
  PERFORM frm_get_fieldcat USING 'KUNNR'   '客户编号'  '6' ''.
  PERFORM frm_get_fieldcat USING 'Z_SHNO'  '客户发运单号'  '18' ''.
  PERFORM frm_get_fieldcat USING 'EXIDV2'  'HU序号'  '8' ''.
  PERFORM frm_get_fieldcat USING 'WDATU'   '收货日期'  '8' ''.
  PERFORM frm_get_fieldcat USING 'BESTQ'   '库存类别'  '4' ''.
  PERFORM frm_get_fieldcat USING 'OLD_EXIDV'   '旧SKU号'  '20' ''.
  PERFORM frm_get_fieldcat USING 'MSG_TXT' '消息'  '50' ''.
ENDFORM.                    " FRM_FILL_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_fill_layout .

  gs_layout-zebra = 'X'.
  gs_layout-no_input          = ' '.
  gs_layout-colwidth_optimize = 'X'.

ENDFORM.                    " FRM_FILL_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_name   text
*      -->P_text   text
*      -->P_outputlen   text
*----------------------------------------------------------------------*
FORM frm_get_fieldcat  USING    p_name
                                p_text
                                p_outputlen
                                p_key.

  DATA: ls_fieldcat       TYPE slis_fieldcat_alv.

  ls_fieldcat-fieldname = p_name.
  ls_fieldcat-seltext_m = p_text.
  ls_fieldcat-outputlen = p_outputlen.
  ls_fieldcat-key       = p_key.
  IF p_name = 'VEMNG' OR p_name = 'MENGE' OR p_name = 'MATNR' OR p_name = 'EXIDV'
    OR p_name = 'KUNNR' OR p_name = 'LIFNR'.
    ls_fieldcat-decimals_out = '0'.         "去掉小数点后边0
    ls_fieldcat-no_zero = 'X'.
  ENDIF.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.

ENDFORM.                    " FRM_GET_FIELDCAT
*&---------------------------------------------------------------------*
*&      FORM  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EXTAB    text
*----------------------------------------------------------------------*
FORM set_pf_status USING p_extab TYPE slis_t_extab.
  SET PF-STATUS '1000_STAT' .
ENDFORM.                    "Pf_set_status
*&---------------------------------------------------------------------*
*&      FORM  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_COMM     text
*      -->SELFIELD   text
*----------------------------------------------------------------------*
FORM user_command USING p_ucomm TYPE sy-ucomm selfield TYPE slis_selfield.
  DATA: l_grid    TYPE REF TO cl_gui_alv_grid,
        ls_main   TYPE t_main.

  CASE p_ucomm.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE PROGRAM.
    WHEN 'CREATE'.
      CLEAR: wa_msg.
      READ TABLE it_msg INTO wa_msg WITH KEY msg_typ = 'E'.
      IF sy-subrc = 0.
        MESSAGE e072(zmm_msg).
      ENDIF.
***处理过账
      PERFORM frm_deal_post_data.
      selfield-refresh = 'X'.

    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_POST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_deal_post_data .

  DATA: ls_goodsmvt_header   TYPE bapi2017_gm_head_01,
        ls_goodsmvt_code     TYPE bapi2017_gm_code VALUE '05',
        ls_goodsmvt_headret  TYPE bapi2017_gm_head_ret,
        ls_goodsmvt_item     TYPE bapi2017_gm_item_create,
        lt_goodsmvt_item     TYPE STANDARD TABLE OF bapi2017_gm_item_create,
        lt_sku               TYPE STANDARD TABLE OF zsmmsku,
        ls_sku               TYPE zsmmsku,
        lt_vekp              TYPE STANDARD TABLE OF vekp,
        lt_vekp1             TYPE STANDARD TABLE OF vekp,
        ls_return            TYPE bapiret2,
        lt_return            TYPE STANDARD TABLE OF bapiret2,
        lt_thulgor           TYPE STANDARD TABLE OF zthulgor,
        ls_thulgor           TYPE zthulgor,
        lt_tscilog           TYPE STANDARD TABLE OF ztscilog,
        ls_mseg              TYPE mseg.
  DATA: l_meins              TYPE meins,
        l_zeile              TYPE mblpo,
        l_sonum              TYPE lvs_sonum,
        l_erfmg              TYPE mb_erfmg,
        l_mb_erfmg           TYPE mb_erfmg,
        l_zvemng             TYPE vemng."记录当前的包装数量

  DATA: lt_log               TYPE STANDARD TABLE OF ztmm999_log,
        ls_log               TYPE ztmm999_log.

***准备BAPI数据
  LOOP AT it_date INTO wa_date.

***Header Data
    ls_goodsmvt_header-pstng_date = wa_date-budat.
    ls_goodsmvt_header-doc_date   = sy-datum.
    ls_goodsmvt_header-pr_uname   = sy-uname.
    ls_goodsmvt_header-ver_gr_gi_slipx  = 'X'.
    ls_goodsmvt_header-header_txt = '期初库存'.
    CLEAR: wa_mat_menge, l_zeile.

    LOOP AT it_key INTO wa_key.

      CLEAR: l_zeile, ls_goodsmvt_item, lt_goodsmvt_item, lt_return.
      LOOP AT it_mat_menge INTO wa_mat_menge WHERE budat = wa_date-budat AND bwart = wa_key-bwart AND sobkz = wa_key-sobkz AND bestq = wa_key-bestq.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = wa_mat_menge-matnr
          IMPORTING
            output = wa_mat_menge-matnr.

*        L_ZEILE = SY-TABIX.
        l_zeile = l_zeile + 1.
        ls_goodsmvt_item-material   = wa_mat_menge-matnr.
        ls_goodsmvt_item-plant      = wa_mat_menge-werks.
        ls_goodsmvt_item-stge_loc   = wa_mat_menge-lgort.
        ls_goodsmvt_item-line_id    = l_zeile.
        ls_goodsmvt_item-move_type  = wa_mat_menge-bwart.
        ls_goodsmvt_item-stck_type  = wa_mat_menge-bestq.
        ls_goodsmvt_item-spec_stock = wa_mat_menge-sobkz.
        ls_goodsmvt_item-vendor     = wa_mat_menge-lifnr.
        ls_goodsmvt_item-entry_qnt  = wa_mat_menge-menge.
        CLEAR l_meins.
        SELECT SINGLE meins INTO l_meins FROM mara WHERE matnr = ls_goodsmvt_item-material.
        IF sy-subrc = 0.
          ls_goodsmvt_item-entry_uom  = l_meins.
        ENDIF.
        APPEND ls_goodsmvt_item TO lt_goodsmvt_item.
        CLEAR ls_goodsmvt_item.

      ENDLOOP.

***同一个凭证下多行项目多对应的SKU信息
      CLEAR: ls_sku, lt_sku,lt_vekp.
      LOOP AT lt_goodsmvt_item INTO ls_goodsmvt_item.
        LOOP AT it_msg INTO wa_msg WHERE werks = ls_goodsmvt_item-plant AND lgort = ls_goodsmvt_item-stge_loc AND matnr = ls_goodsmvt_item-material
          AND lifnr = ls_goodsmvt_item-vendor AND bwart = ls_goodsmvt_item-move_type AND sobkz = ls_goodsmvt_item-spec_stock AND bestq = ls_goodsmvt_item-stck_type
          AND budat = wa_date-budat.
          "add by handyxh 南京子转分 begin
          READ TABLE it_vekp INTO wa_vekp WITH KEY exidv = wa_msg-exidv.
          IF sy-subrc = 0.
            APPEND wa_vekp TO lt_vekp.
          ENDIF.
          "add by handyxh 南京子转分 end
***SKU信息
          ls_sku-z_chb    = 'X'.
          ls_sku-zzeile   = ls_goodsmvt_item-line_id.
          ls_sku-zexidv   = wa_msg-exidv.
          ls_sku-zvemng   = wa_msg-vemng.
          ls_sku-zvelin   = '1'.
          ls_sku-zvepos   = '1'.
          SELECT SINGLE rmatp meins INTO (ls_sku-zvhilm, ls_sku-zvemeh) FROM mara WHERE matnr = wa_msg-matnr.
          ls_sku-zmatnr   = wa_msg-matnr.
          ls_sku-zbwart   = wa_msg-bwart.
          ls_sku-zwerks   = wa_msg-werks.
          ls_sku-zlgort   = wa_msg-lgort.
          ls_sku-zlgpla   = wa_msg-lgpla.
          ls_sku-zebeln   = wa_msg-ebeln.
          ls_sku-zinhalt  = wa_msg-inhalt.
          ls_sku-zkunnr   = wa_msg-kunnr.
          ls_sku-z_shno   = wa_msg-z_shno.
          ls_sku-zexidv2  = wa_msg-exidv2.
          ls_sku-zsobkz   = wa_msg-sobkz.
          CLEAR: l_sonum.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_msg-lifnr
            IMPORTING
              output = l_sonum.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_sonum
            IMPORTING
              output = ls_sku-zsonum.
          ls_sku-zbudat   = wa_msg-wdatu.
          ls_sku-zbestq   = wa_msg-bestq.
          ls_sku-z_org_ernam = sy-uname.
          ls_sku-z_org_erdat = sy-datum.
          ls_sku-z_org_eruhr = sy-timlo.
          ls_sku-zold_exidv   = wa_msg-old_exidv."20131122新增旧SKU字段
          ls_sku-z_exidv_ref  = ls_sku-zexidv.
          ls_sku-z_org_vemng  = ls_sku-zvemng.
          ls_sku-zerfmg       = ls_sku-zvemng.
          APPEND ls_sku TO lt_sku.
          CLEAR ls_sku.
        ENDLOOP.
      ENDLOOP.

      EXPORT it_sku = lt_sku TO MEMORY ID 'SKU'.

***过账
      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = ls_goodsmvt_header
          goodsmvt_code    = ls_goodsmvt_code
        IMPORTING
          goodsmvt_headret = ls_goodsmvt_headret
        TABLES
          goodsmvt_item    = lt_goodsmvt_item
          return           = lt_return.

      IF NOT lt_return IS INITIAL.
        READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
        IF sy-subrc = 0.
          MESSAGE ls_return-message TYPE ls_return-type.
        ENDIF.

      ELSE.
***成功创建物料凭证之后
        IF lt_sku IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          MOVE 'S' TO wa_msg-msg_typ.
          MOVE icon_led_green TO wa_msg-msg_lcd.
          LOOP AT lt_goodsmvt_item INTO ls_goodsmvt_item.
            CONCATENATE ls_goodsmvt_headret-mat_doc '已经成功创建' INTO wa_msg-msg_txt.
            MODIFY it_msg FROM wa_msg TRANSPORTING msg_typ msg_lcd msg_txt
             WHERE werks = ls_goodsmvt_item-plant AND lgort = ls_goodsmvt_item-stge_loc
               AND matnr = ls_goodsmvt_item-material AND bwart = ls_goodsmvt_item-move_type
               AND sobkz = ls_goodsmvt_item-spec_stock AND bestq = ls_goodsmvt_item-stck_type AND budat = wa_date-budat.
          ENDLOOP.
        ELSE.

***准备SKU数据

          SELECT * FROM zthulgor INTO TABLE lt_thulgor WHERE z_hums = 'X'.

          LOOP AT lt_goodsmvt_item INTO ls_goodsmvt_item.
*      判断SKU
            CLEAR: l_zeile.
            l_zeile = ls_goodsmvt_item-line_id.
            CLEAR: ls_mseg.
            ls_mseg-mblnr = ls_goodsmvt_headret-mat_doc.
            ls_mseg-mjahr = ls_goodsmvt_headret-doc_year.
            ls_mseg-zeile = ls_goodsmvt_item-line_id.
            ls_mseg-bwart = ls_goodsmvt_item-move_type.
            ls_mseg-sobkz = ls_goodsmvt_item-spec_stock.
            LOOP AT lt_sku INTO ls_sku WHERE zzeile = l_zeile AND z_chb = 'X'
              AND zmatnr =  ls_goodsmvt_item-material AND zwerks = ls_goodsmvt_item-plant AND zlgort = ls_goodsmvt_item-stge_loc AND zexidv <> ''.
              ls_sku-z_exidv_ref = ls_sku-zexidv.
              READ TABLE lt_thulgor INTO ls_thulgor WITH KEY zwerks = ls_sku-zwerks zlgort = ls_sku-zlgort.
              IF sy-subrc = 0."收货库位是SKU管理
                CLEAR: lt_tscilog.
                CALL FUNCTION 'ZMM_SKU_CREATION'
                  EXPORTING
                    is_sku           = ls_sku
                    is_mseg          = ls_mseg
*                   I_FLG_UM         = ''"在本库位创建SKU
                    i_update_tscilog = 'X'
                    i_init_import    = 'X'
                  TABLES
                    it_ztscilog      = lt_tscilog
                  EXCEPTIONS
                    error_message    = 1
                    OTHERS           = 2.
***回写MSG成功内表信息
                CLEAR: wa_msg.
                IF sy-subrc = 0.
                  CALL FUNCTION 'ZMM_UPDATE_DB_ZTABLE'
                    TABLES
                      it_tscilog = lt_tscilog.

***冻结SKU
                  CALL FUNCTION 'ZMM_DEAL_BESTQ'
                    EXPORTING
                      i_exidv        = ls_sku-zexidv
                      i_bestq        = ls_sku-zbestq
                    EXCEPTIONS
                      update_failure = 1
                      OTHERS         = 2.

                  MOVE 'S' TO wa_msg-msg_typ.
                  MOVE icon_led_green TO wa_msg-msg_lcd..
                  CONCATENATE ls_mseg-mblnr '已经成功创建' INTO wa_msg-msg_txt.
                ELSE.
                  MOVE 'E' TO wa_msg-msg_typ.
                  MOVE icon_led_red TO wa_msg-msg_lcd..
                  wa_msg-msg_txt = sy-msgv1.
                ENDIF.
              ELSE.
                MOVE 'E' TO wa_msg-msg_typ.
                MOVE icon_led_red TO wa_msg-msg_lcd..
                wa_msg-msg_txt = '无法确认是否SKU库存地'.
              ENDIF.
              MODIFY it_msg FROM wa_msg TRANSPORTING msg_typ msg_lcd msg_txt
               WHERE exidv = ls_sku-zexidv AND werks = ls_goodsmvt_item-plant AND lgort = ls_goodsmvt_item-stge_loc
               AND matnr = ls_goodsmvt_item-material AND bwart = ls_goodsmvt_item-move_type
               AND sobkz = ls_goodsmvt_item-spec_stock AND bestq = ls_goodsmvt_item-stck_type AND budat = wa_date-budat.
            ENDLOOP.
            IF sy-subrc <> 0."没有查到SKU情况
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.
              "add by handyxh 南京子转分 begin
              IF sy-datum <= '20190731'.
                DO 5 TIMES.
                  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_vekp1 FROM vekp INNER JOIN vepo ON vekp~venum = vepo~venum
                    FOR ALL ENTRIES IN lt_sku
                    WHERE exidv = lt_sku-zexidv
                      AND vepo~werks = 'SN00'.
                  IF sy-subrc = 0.
                    EXIT.
                  ELSE.
                    WAIT UP TO '0.5' SECONDS.
                  ENDIF.
                ENDDO.
                LOOP AT lt_vekp1 INTO wa_vekp1.
                  READ TABLE lt_vekp INTO wa_vekp WITH KEY exidv = wa_vekp1-exidv.
                  IF sy-subrc = 0.
                    wa_vekp1-erdat = wa_vekp-erdat.
                    wa_vekp1-eruhr = wa_vekp-eruhr.
                  ENDIF.
                  MODIFY lt_vekp1 FROM wa_vekp1.
                ENDLOOP.
                MODIFY vekp FROM TABLE lt_vekp1.
                COMMIT WORK AND WAIT.
              ENDIF.
              "add by handyxh 南京子转分 end
              MOVE 'S' TO wa_msg-msg_typ.
              MOVE icon_led_green TO wa_msg-msg_lcd.
              CONCATENATE ls_goodsmvt_headret-mat_doc '已经成功创建' INTO wa_msg-msg_txt.
              MODIFY it_msg FROM wa_msg TRANSPORTING msg_typ msg_lcd msg_txt
               WHERE werks = ls_goodsmvt_item-plant AND lgort = ls_goodsmvt_item-stge_loc
                 AND matnr = ls_goodsmvt_item-material AND bwart = ls_goodsmvt_item-move_type
                 AND sobkz = ls_goodsmvt_item-spec_stock AND bestq = ls_goodsmvt_item-stck_type AND budat = wa_date-budat.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
      "add by handyxh 20190624 增加日志
      GET TIME.
      LOOP AT lt_goodsmvt_item INTO ls_goodsmvt_item.
        LOOP AT it_msg INTO wa_msg WHERE msg_txt IS NOT INITIAL AND werks = ls_goodsmvt_item-plant AND lgort = ls_goodsmvt_item-stge_loc
                 AND matnr = ls_goodsmvt_item-material AND bwart = ls_goodsmvt_item-move_type
                 AND sobkz = ls_goodsmvt_item-spec_stock AND bestq = ls_goodsmvt_item-stck_type AND budat = wa_date-budat.
          MOVE-CORRESPONDING wa_msg TO ls_log.
          ls_log-zcpudt = sy-datum.
          ls_log-zcputm = sy-uzeit.
          APPEND ls_log TO lt_log.
          CLEAR:ls_log,wa_msg.
        ENDLOOP.
      ENDLOOP.
      IF NOT lt_log IS INITIAL.
        MODIFY ztmm999_log FROM TABLE lt_log.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
        ELSE.
          ROLLBACK WORK .
        ENDIF.
      ENDIF.
      "add by handyxh 20190624 增加日志

    ENDLOOP.

  ENDLOOP.


ENDFORM.                    " FRM_DEAL_POST_DATA
