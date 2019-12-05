**************************************************
*程序名称:凭证打印
*创建日期: 2019-11-20
*创建者:XXX
*申请者:XXX
*功能描述:
*============================================
*变更记录
*修改请求号    修改日期    修改人   修改描述
*DEVK912015    2019-11-20   HANDYXH    创建程序
***************************************************
REPORT zshxjfi0007.

TYPE-POOLS:ole2.

TABLES:bkpf,bseg,ztfi_print_001.
DATA: gv_dylx TYPE ztfi_print_002-zdylx.

TYPES:BEGIN OF typ_dis,
        sel        TYPE c,               "复选框
        print_flag TYPE c,          "打印标志，是否已打印
        lock       TYPE c,          "锁定标志，是否已打印
        bukrs      TYPE bkpf-bukrs,      "公司代码
        gjahr      TYPE bkpf-gjahr,      "会计年度
        belnr      TYPE bkpf-belnr,      "凭证编号
        monat      TYPE bkpf-monat,    "期间
        budat      TYPE bkpf-budat,      "期间
        blart      TYPE bkpf-blart,      "期间
        lifnr      TYPE bseg-lifnr,      "期间
        hkont      TYPE bseg-hkont,      "期间
        usnam      TYPE bkpf-usnam,      "期间
        wrbtr      TYPE bseg-wrbtr,      "金额
        bktxt      TYPE bkpf-bktxt,      "期间
        waers      TYPE bkpf-waers,      "期间
        awkey      TYPE bkpf-awkey,      "期间
        numpg      TYPE bkpf-numpg,      "期间
        zpzbh      TYPE zzfipzbh,      "期间

        stgrd      TYPE bkpf-stgrd,
        stblg      TYPE bkpf-stblg,
        stodt      TYPE bkpf-stodt,
        xblnr      TYPE bkpf-xblnr,

        wrbtr_bb   TYPE bseg-wrbtr,
      END OF typ_dis.
TYPES:BEGIN OF ty_bseg.
        INCLUDE TYPE bseg.
TYPES:blart TYPE bkpf-blart,
      END OF ty_bseg.
DATA:gt_display TYPE TABLE OF typ_dis WITH HEADER LINE.
DATA:gt_display2 TYPE TABLE OF typ_dis WITH HEADER LINE.
DATA:gt_re TYPE TABLE OF typ_dis WITH HEADER LINE.

DATA:gt_bseg TYPE TABLE OF ty_bseg WITH HEADER LINE.
DATA:gt_bseg2 TYPE TABLE OF ty_bseg WITH HEADER LINE.

DATA:gt_bseg13 TYPE TABLE OF ty_bseg WITH HEADER LINE.
DATA:gt_bseg14 TYPE TABLE OF ty_bseg WITH HEADER LINE.

DATA:gt_print TYPE TABLE OF ztfi_print_001 WITH HEADER LINE.
DATA:gt_ztfi_print_005 TYPE TABLE OF ztfi_print_002 WITH HEADER LINE.

DATA:g_lock_num TYPE i.

TYPES: BEGIN OF typ_xml,
         field1  TYPE string,
         field2  TYPE string,
         field3  TYPE string,
         field4  TYPE string,
         field5  TYPE string,
         field6  TYPE string,
         field7  TYPE string,
         field8  TYPE string,
         field9  TYPE string,
         field10 TYPE string,
       END OF typ_xml.

DATA gt_xml TYPE TABLE OF typ_xml WITH HEADER LINE."下载数据使用

DATA: g_destination LIKE rlgrap-filename, "文件路径
      g_file        TYPE  rlgrap-filename, "文件名
      g_sign(1),
      g_disk(6)     TYPE c VALUE 'C:\'. "存储路径.
**导出EXCEL用变量
DATA: application TYPE ole2_object,
      workbook    TYPE ole2_object,
      sheet       TYPE ole2_object,
      columns     TYPE ole2_object,
      w_comment   TYPE ole2_object,
      w_shape     TYPE ole2_object,
      rows        TYPE ole2_object,
      range       TYPE ole2_object,
      h_f         TYPE ole2_object,            " font
      cells1      TYPE ole2_object,
      cells       TYPE ole2_object.


*&---------------------------------------------------------------------*
*&       ALV
*&---------------------------------------------------------------------*
DATA:
  gw_layout   TYPE lvc_s_layo,
  gv_repid    TYPE repid,
  gt_fieldcat TYPE lvc_t_fcat,
  gw_fieldcat TYPE lvc_s_fcat.



"字段目录的宏
DEFINE mcr_fieldcat.

  CLEAR gw_fieldcat.
  gw_fieldcat-fieldname   = &1.
  gw_fieldcat-scrtext_m   = &2.
  IF gw_fieldcat-fieldname = 'PRINT_FLAG'.
*    gw_fieldcat-edit = 'X'.
    gw_fieldcat-checkbox = 'X'.
  ENDIF.

  IF gw_fieldcat-fieldname = 'ZPZBH'.
    gw_fieldcat-no_zero = 'X'.
  ENDIF.

  APPEND gw_fieldcat TO gt_fieldcat.

END-OF-DEFINITION.



SELECTION-SCREEN BEGIN OF BLOCK 0100 WITH FRAME TITLE text-001.
SELECT-OPTIONS:s_bukrs FOR bkpf-bukrs MEMORY ID buk  NO-EXTENSION  NO INTERVALS.
SELECT-OPTIONS:s_belnr FOR bkpf-belnr,
               s_blart FOR bkpf-blart DEFAULT 'SA' NO-EXTENSION  NO INTERVALS,
*               s_monat FOR bkpf-monat,
               s_budat1 FOR bkpf-budat NO-EXTENSION NO INTERVALS MODIF ID m1,
               s_budat FOR bkpf-budat MODIF ID m2,
               s_usnam FOR bkpf-usnam  NO-EXTENSION  NO INTERVALS,
               s_prctr FOR bseg-prctr,
               s_waers FOR bkpf-waers,
               s_zpzbh FOR ztfi_print_001-zpzbh.

SELECTION-SCREEN END OF BLOCK 0100 .

SELECTION-SCREEN BEGIN OF BLOCK 0200 WITH FRAME TITLE text-002.
PARAMETERS:p1  TYPE c RADIOBUTTON GROUP g1 DEFAULT 'X' MODIF ID fc USER-COMMAND comm1,
           p2  TYPE c RADIOBUTTON GROUP g1,
           p3  TYPE c RADIOBUTTON GROUP g1,
           p4  TYPE c RADIOBUTTON GROUP g1,
           p5  TYPE c RADIOBUTTON GROUP g1,
           p6  TYPE c RADIOBUTTON GROUP g1,
           p7  TYPE c RADIOBUTTON GROUP g1,

           p13 TYPE c RADIOBUTTON GROUP g1,

           p9  TYPE c RADIOBUTTON GROUP g1,
           p11 TYPE c RADIOBUTTON GROUP g1,
           p12 TYPE c RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK 0200 .

SELECTION-SCREEN BEGIN OF BLOCK 0300 WITH FRAME TITLE text-003.
PARAMETERS:p8 TYPE c AS CHECKBOX .
PARAMETERS:p10 TYPE c AS CHECKBOX .
SELECTION-SCREEN END OF BLOCK 0300 .



INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  IF p12 = 'X'  .
    LOOP AT SCREEN .
      IF screen-group1 = 'M2'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    IF s_budat1[] IS INITIAL.
      CLEAR s_budat1.
      s_budat1-sign ='I'.
      s_budat1-option ='EQ'.
      s_budat1-low = sy-datum.
      APPEND s_budat1.
    ENDIF.

  ELSE.
    LOOP AT SCREEN .
      IF screen-group1 = 'M1'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    IF s_budat[] IS INITIAL .
      CLEAR s_budat.
      s_budat-sign ='I'.
      s_budat-option ='EQ'.
      s_budat-low(6) = sy-datum(6).
      s_budat-low+6(2) = '01'.
      CALL FUNCTION 'HRPAD_GET_LAST_DAY_OF_MONTH'
        EXPORTING
          iv_date     = s_budat-low
        IMPORTING
          ev_last_day = s_budat-high.

*      CALL FUNCTION 'HR_BG_LAST_DAY_OF_MONTH'
*        EXPORTING
*          day_in            = s_budat-low
*        IMPORTING
*          last_day_of_month = s_budat-high
*        EXCEPTIONS
*          day_in_no_date    = 1
*          OTHERS            = 2.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
      APPEND s_budat.
    ENDIF.
  ENDIF.
*  IF S_BUKRS[] IS INITIAL .
*    CLEAR S_BUKRS.
*    S_BUKRS-SIGN ='I'.
*    S_BUKRS-OPTION ='EQ'.
*    SELECT SINGLE PARVA INTO S_BUKRS-LOW
*    FROM USR05 WHERE BNAME = SY-UNAME AND PARID = 'BUK'.
*    APPEND S_BUKRS.
*  ENDIF.


START-OF-SELECTION.
  "权限检查
  IF s_bukrs[] IS INITIAL.
    MESSAGE '请输入公司代码' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF s_blart[] IS INITIAL.
    MESSAGE '请输入凭证类型' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF p12 EQ 'X'.
    s_budat[] = s_budat1[].
  ENDIF.
  PERFORM frm_auth_check.
  "根据屏幕选择条件，更改选择条件
  PERFORM frm_get_sel.
  "获取数据
  PERFORM frm_get_data.
  "数据展示
  PERFORM frm_display.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_data .
  DATA:BEGIN OF lt_bseg_sum OCCURS 0,
         bukrs TYPE bkpf-bukrs,      "公司代码
         gjahr TYPE bkpf-gjahr,      "会计年度
         belnr TYPE bkpf-belnr,      "凭证编号
         wrbtr TYPE bseg-wrbtr,      "金额
       END OF lt_bseg_sum.

  DATA:BEGIN OF lt_re OCCURS 0 ,
         bukrs TYPE bkpf-bukrs,      "公司代码
         gjahr TYPE bkpf-gjahr,      "会计年度
         monat TYPE bkpf-monat,      "凭证编号
         zpzbh TYPE zzfipzbh,      "凭证编号
       END OF lt_re.

  DATA:BEGIN OF lt_ys OCCURS 0 ,
         bukrs TYPE bkpf-bukrs,      "公司代码
         gjahr TYPE bkpf-gjahr,      "会计年度
         monat TYPE bkpf-monat,      "凭证编号
         zpzbh TYPE zzfipzbh,      "凭证编号
       END OF lt_ys.

  DATA: lt_ztfi_print_005 TYPE TABLE OF ztfi_print_002 WITH HEADER LINE.

  DATA:l_index TYPE sy-tabix.

  DATA:l_varkey TYPE vim_enqkey.

  REFRESH gt_display.

  IF p10 = ''.
***凭证抬头信息
    SELECT bukrs
           gjahr
           belnr
           monat
           budat
           blart
           usnam
*         wrbtr
           bktxt
           waers
           awkey
*           XREF2_HD AS ZPZBH
           numpg
           stgrd
           stblg
           stodt
           xblnr
      INTO CORRESPONDING FIELDS OF TABLE gt_display
      FROM bkpf
      WHERE bukrs IN s_bukrs
        AND belnr IN s_belnr
        AND blart IN s_blart
        AND budat IN s_budat
        AND usnam IN s_usnam
*      and prctr in s_prctr
        AND waers IN s_waers
    AND ( bstat = '' OR bstat = 'A' OR bstat = 'U' ).
  ELSE.
***凭证抬头信息
    SELECT bukrs
           gjahr
           belnr
           monat
           budat
           blart
           usnam
*         wrbtr
           bktxt
           waers
           awkey
*           XREF2_HD AS ZPZBH
           numpg
           stgrd
          stblg
           stodt
      xblnr
      INTO CORRESPONDING FIELDS OF TABLE gt_display
      FROM bkpf
      WHERE bukrs IN s_bukrs
        AND belnr IN s_belnr
        AND blart IN s_blart
        AND budat IN s_budat
        AND usnam IN s_usnam
*      and prctr in s_prctr
        AND waers IN s_waers
        AND stgrd <> '01'
    AND ( bstat = '' OR bstat = 'A' OR bstat = 'U' ).

  ENDIF.
  IF gt_display[] IS INITIAL.
    MESSAGE '没有数据!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF gt_display[] IS NOT INITIAL.
    REFRESH lt_ztfi_print_005.
    SELECT * INTO TABLE lt_ztfi_print_005
      FROM ztfi_print_002
      FOR ALL ENTRIES IN gt_display
      WHERE bukrs = gt_display-bukrs AND belnr = gt_display-belnr AND gjahr = gt_display-gjahr AND zdylx = gv_dylx.
  ENDIF.
  g_lock_num = 0.
*****对未打印的数据，进行上锁

  LOOP AT gt_display ."WHERE zpzbh <> ''.
    l_index = sy-tabix.
****对取出来的凭证数据进行上锁，已经打印过，有打印凭证号的不进行上锁
    READ TABLE lt_ztfi_print_005 WITH KEY bukrs = gt_display-bukrs
                                       belnr = gt_display-belnr
                                       gjahr = gt_display-gjahr
                                       zdylx = gv_dylx.
    IF sy-subrc = 0.
      gt_display-zpzbh = lt_ztfi_print_005-zpzbh.
      gt_display-print_flag = 'X'.
    ELSE.
      IF p1 = 'X' OR p13 = 'X'    ."合并打印不需要上锁
        IF g_lock_num < 1000.
          g_lock_num = g_lock_num + 1.
          CLEAR l_varkey.
          CONCATENATE sy-mandt gt_display-bukrs  gt_display-belnr  gt_display-gjahr INTO l_varkey.
          CALL FUNCTION 'ENQUEUE_E_TABLE'
            EXPORTING
              mode_rstable   = 'E'
              tabname        = 'BKPF'
              varkey         = l_varkey
*             X_TABNAME      = ' '
*             X_VARKEY       = ' '
*             _SCOPE         = '2'
*             _WAIT          = ' '
*             _COLLECT       = ' '
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc <> 0.
* Implement suitable error handling here
*********未锁定成功的凭证不允许打印
            DELETE gt_display INDEX l_index.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    MODIFY gt_display INDEX l_index.
  ENDLOOP.


  SORT gt_display BY bukrs gjahr belnr.


  DATA:l_num TYPE i.

  IF s_zpzbh[] IS NOT INITIAL.
    LOOP AT s_zpzbh.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = s_zpzbh-low
        IMPORTING
          output = s_zpzbh-low.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = s_zpzbh-high
        IMPORTING
          output = s_zpzbh-high.
      MODIFY s_zpzbh.
    ENDLOOP.
    DELETE gt_display[] WHERE zpzbh NOT IN s_zpzbh.
  ENDIF.
  IF gt_display[] IS INITIAL.
    MESSAGE '没有数据!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

****获取对应的屏幕行项目信息
  CLEAR gt_bseg[].
  IF gt_display[] IS NOT INITIAL.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE gt_bseg
      FROM bseg
      FOR ALL ENTRIES IN gt_display
      WHERE bukrs = gt_display-bukrs
        AND gjahr =  gt_display-gjahr
    AND belnr =  gt_display-belnr.


    "2019-03-12 BY SW
    IF p13 = 'X'.
      SORT gt_bseg BY bukrs belnr gjahr fkber hkont koart.
      LOOP AT gt_bseg.
        READ TABLE gt_display INTO DATA(ls_display) WITH KEY bukrs = gt_bseg-bukrs
                                                             gjahr = gt_bseg-gjahr
                                                             belnr = gt_bseg-belnr.
        IF sy-subrc = 0.
          gt_bseg-blart = ls_display-blart.
        ENDIF.
        gt_bseg13-bukrs =  gt_bseg-bukrs .
        gt_bseg13-belnr =    gt_bseg-belnr.
        gt_bseg13-gjahr =    gt_bseg-gjahr .
        gt_bseg13-fkber =    gt_bseg-fkber .
        gt_bseg13-hkont =    gt_bseg-hkont .
        gt_bseg13-koart =   gt_bseg-koart.

**      gt_bseg13-sgtxt =   gt_bseg-sgtxt.
*        gt_bseg13-zuonr =   gt_bseg-zuonr.
*        gt_bseg13-kunnr =   gt_bseg-kunnr.
*        gt_bseg13-lifnr =   gt_bseg-lifnr.
*        gt_bseg13-vptnr =   gt_bseg-vptnr.
*        gt_bseg13-fkber =   gt_bseg-fkber.
**       gt_bseg13-kostl =   gt_bseg-kostl.
*        gt_bseg13-projk =   gt_bseg-projk.
        gt_bseg13-shkzg =   gt_bseg-shkzg.
        gt_bseg13-xnegp =   gt_bseg-xnegp.
        gt_bseg13-blart  =  gt_bseg-blart.

        gt_bseg13-dmbtr =   gt_bseg-dmbtr.

        COLLECT gt_bseg13.
      ENDLOOP.
      CLEAR:gt_bseg14[].
      LOOP AT gt_bseg13 WHERE blart = 'SA' .
        READ TABLE gt_display WITH KEY  belnr = gt_bseg13-belnr bukrs = gt_bseg13-bukrs gjahr = gt_bseg13-gjahr.
        IF sy-subrc = 0.
          gt_bseg13-sgtxt = gt_display-bktxt.
          APPEND gt_bseg13 TO gt_bseg14.
          CLEAR:gt_bseg13.
        ENDIF.

      ENDLOOP.
      IF gt_bseg14[] IS NOT INITIAL.
        CLEAR: gt_bseg[].
        gt_bseg[] =  gt_bseg14[].

      ENDIF.
    ENDIF.

  ENDIF.
*******获取对应的行项目 打印信息
***  CLEAR gt_print[].
***  SELECT *
***    INTO CORRESPONDING FIELDS OF TABLE gt_print
***    FROM ZTFI_PRINT_001
***    FOR ALL ENTRIES IN gt_display
***    WHERE bukrs = gt_display-bukrs
***      AND gjahr =  gt_display-gjahr
***      AND belnr =  gt_display-belnr.

***累计借方金额
  CLEAR lt_bseg_sum.
  LOOP AT gt_bseg.
    CLEAR lt_bseg_sum.
    CHECK gt_bseg-shkzg = 'S'.
    MOVE-CORRESPONDING gt_bseg TO lt_bseg_sum.
    COLLECT lt_bseg_sum.
  ENDLOOP.

  SORT lt_bseg_sum BY bukrs gjahr belnr.
  SORT gt_bseg BY bukrs belnr gjahr koart.

  gt_bseg2[] = gt_bseg[].
  SORT gt_bseg2 BY bukrs belnr gjahr hkont.

  IF p1 IS INITIAL AND p11 IS INITIAL AND p12 IS INITIAL AND p13 IS INITIAL.
    READ TABLE gt_display INDEX 1.
    SELECT SINGLE zpzbh INTO @DATA(lv_zpzbh)
      FROM ztfi_print_004
     WHERE bukrs = @gt_display-bukrs
       AND gjahr = @gt_display-gjahr
       AND monat = @gt_display-monat
       AND blart = @gt_display-blart.
  ENDIF.

**借方金额填到展示的数据中
  LOOP AT   gt_display.
    l_index = sy-tabix.

*** 借方金额
    CLEAR lt_bseg_sum.
    READ TABLE lt_bseg_sum WITH KEY bukrs = gt_display-bukrs
                                gjahr = gt_display-gjahr
                                belnr = gt_display-belnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_display-wrbtr = lt_bseg_sum-wrbtr.
    ENDIF.

    ""采购发票
    IF p11 = 'X'.
      CLEAR gt_bseg.
      READ TABLE gt_bseg WITH KEY bukrs = gt_display-bukrs
                                  belnr = gt_display-belnr
                                  gjahr = gt_display-gjahr
                                  koart = 'K'
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        gt_display-lifnr = gt_bseg-lifnr.
      ENDIF.
    ENDIF.


*    """银行/票据
    IF p12 = 'X'.
      READ TABLE gt_bseg2 WITH KEY bukrs = gt_display-bukrs
                                  belnr = gt_display-belnr
                                  gjahr = gt_display-gjahr
                                  hkont = '1121010000'
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        gt_display-hkont = gt_bseg2-hkont.
      ELSE.
        LOOP AT gt_bseg WHERE bukrs = gt_display-bukrs
                          AND belnr = gt_display-belnr
                          AND gjahr = gt_display-gjahr
                          AND hkont(4) = '1002'.
          gt_display-hkont = gt_bseg-hkont.
          EXIT.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF lv_zpzbh IS NOT INITIAL.
      gt_display-zpzbh = lv_zpzbh.
    ENDIF.


    MODIFY gt_display INDEX l_index.
  ENDLOOP.
  CLEAR gt_bseg2[].


* ADD BY ZXK 20190129 新增本币金额 BEGIN
  DATA:ret TYPE bapireturn.
  DATA:lv_amount_ex TYPE bapicurr-bapicurr.
  DATA:lv_amount_in TYPE wrbtr.
  LOOP AT gt_display INTO ls_display.

    lv_amount_ex = ls_display-wrbtr.


    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
      EXPORTING
        currency             = ls_display-waers
        amount_external      = lv_amount_ex
        max_number_of_digits = 23
      IMPORTING
        amount_internal      = lv_amount_in
        return               = ret.

    IF ret IS NOT INITIAL.

    ENDIF.

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
        date             = sy-datum
        foreign_amount   = lv_amount_in
        foreign_currency = ls_display-waers
        local_currency   = 'CNY'
      IMPORTING
        local_amount     = ls_display-wrbtr_bb.

    CLEAR:lv_amount_ex,lv_amount_in.
    MODIFY gt_display FROM ls_display.
  ENDLOOP.
* ADD BY ZXK 20190129 新增本币金额 END


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display .
  "设置布局
  PERFORM frm_set_layout.
  "设置字段目录
  PERFORM frm_set_fieldcat.
  "alv显示
  PERFORM frm_alv_output.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_LAYOUT
*&---------------------------------------------------------------------*
*       设置布局
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_set_layout .
  "设置布局
  gw_layout-cwidth_opt = 'X'.
*  gw_layout-no_rowmark = 'X'.
  gw_layout-col_opt    = 'X'.
  gw_layout-zebra      = 'X'.
  gw_layout-box_fname = 'SEL'.
  gv_repid = sy-repid.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_FIELDCAT
*&---------------------------------------------------------------------*
*       设置字段目录
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_set_fieldcat .
  IF p1 = 'X' OR p13 = 'X' OR p11 = 'X' OR p12 = 'X'.
    mcr_fieldcat 'PRINT_FLAG'  '已打印'.
  ENDIF.
  mcr_fieldcat 'ZPZBH'  '打印凭证编号'.
  mcr_fieldcat 'BUKRS'  '公司代码'.
  mcr_fieldcat 'GJAHR'  '会计年度'.
  mcr_fieldcat 'BELNR'  '凭证编号'.
  mcr_fieldcat 'BUDAT'  '过账日期'.
  mcr_fieldcat 'BLART'  '凭证类型'.
  IF p11 = 'X'.
    mcr_fieldcat 'LIFNR'  '供应商'.
  ENDIF.
  mcr_fieldcat 'XBLNR'  '参考'.
  mcr_fieldcat 'USNAM'  '制证人'.
  mcr_fieldcat 'WRBTR'  '凭证金额'.
  mcr_fieldcat 'BKTXT'  '抬头文本'.
  mcr_fieldcat 'WAERS'  '币别'.
  mcr_fieldcat 'WRBTR_BB'  '本币金额'.
  mcr_fieldcat 'AWKEY'  '参考代码'.
  mcr_fieldcat 'STGRD'  '冲销原因'.
  mcr_fieldcat 'STBLG'  '冲销参考'.
  mcr_fieldcat 'STODT'  '冲销日期'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_OUTPUT
*&---------------------------------------------------------------------*
*       alv显示
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_alv_output .
  "调用函数，显示alv
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = gv_repid
      i_callback_user_command  = 'FRM_USER_COMMAND'
      i_callback_pf_status_set = 'FRM_USER_STATUS'
      is_layout_lvc            = gw_layout
      it_fieldcat_lvc          = gt_fieldcat
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_display[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_USER_STATUS
*&---------------------------------------------------------------------*
*       gui状态
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_user_status USING extab TYPE slis_t_extab.
  DATA:l_butxt TYPE butxt.
*  SELECT SINGLE butxt INTO l_butxt FROM t001 WHERE bukrs IN s_bukrs.
  "设置gui状态
  SET PF-STATUS 'STANDARD' .
  SET TITLEBAR 'STANDARD' WITH l_butxt .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_user_command USING pv_ucomm LIKE sy-ucomm
                             ps_selfield TYPE slis_selfield..

*   DATA: lo_grid TYPE REF TO cl_gui_alv_grid,
*        lw_stbl TYPE lvc_s_stbl.
*
*  "后续需要刷新屏幕时必须有以下语句
*
*
*  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*    IMPORTING
*      e_grid = lo_grid.
*
*  CALL METHOD lo_grid->check_changed_data.

  CASE pv_ucomm.
    WHEN 'PRINT'.
      CLEAR gt_ztfi_print_005[]."清除编号表
      IF p1 = 'X' OR p13 = 'X'.
        "打印
        READ TABLE gt_display WITH KEY sel = 'X' TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          "一般打印
          PERFORM frm_print.
        ELSE.
          MESSAGE '请选择数据！' TYPE 'S'.
        ENDIF.

        LOOP AT gt_display WHERE sel = 'X'.
          IF gt_display-zpzbh <> ''.
            gt_display-print_flag = 'X'.
            MODIFY gt_display.
          ENDIF.
        ENDLOOP.
      ELSEIF p11 = 'X'.
        PERFORM frm_print_re.
      ELSEIF p12 = 'X'.
        PERFORM frm_print_ys.
      ELSE.
        "汇总打印
        PERFORM frm_print_sum.
      ENDIF.

      "刷新
      ps_selfield-refresh = 'X'.

*            CALL METHOD lo_grid->refresh_table_display
*        EXPORTING
*          is_stable = lw_stbl
*        EXCEPTIONS
*          finished  = 1.
*      IF sy-subrc <> 0.
**       Implement suitable error handling here
*      ENDIF.


    WHEN '&IC1'.


      READ TABLE gt_display  INDEX ps_selfield-tabindex.
      IF sy-subrc EQ 0.
        IF ps_selfield-fieldname = 'BELNR'.
          CHECK gt_display-belnr <> ''.
          SET PARAMETER ID 'BUK' FIELD gt_display-bukrs.
          SET PARAMETER ID 'GJR' FIELD gt_display-gjahr.
          SET PARAMETER ID 'BLN' FIELD gt_display-belnr.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

        ENDIF.
      ENDIF.
*    WHEN 'BACK1' OR  'BACK2' OR  'BACK3'.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_download_data .
  DATA l_filename TYPE rlgrap-filename .
  DATA l_filename_def TYPE rlgrap-filename .
  DATA:l_flag    TYPE c,
       l_strans  TYPE char20,
       l_perform TYPE char20.

  DATA:l_butxt TYPE butxt.
  SELECT SINGLE butxt INTO l_butxt FROM t001 WHERE bukrs IN s_bukrs.

  "strans 对应的EXCEL名称
  l_strans = 'ZCORPT002'.

  CONCATENATE l_butxt  '年度'  '生产部门公用介质耗用表.XLS' INTO l_filename_def.
  PERFORM frm_convertdata_9006.

*  PERFORM saplzfzzz_001(l_perform).

  PERFORM frm_generalxml USING l_filename_def l_strans CHANGING l_filename l_flag.
  CHECK l_flag IS INITIAL.
  PERFORM frm_process_indcator USING '正在导出数据' 80 .
  PERFORM frm_openreport USING l_filename '1'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CONVERTDATA_9006
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_convertdata_9006 .
  CLEAR: gt_xml,gt_xml[].
  LOOP AT gt_display.
    CLEAR gt_xml.
*    gt_xml-field1 = gt_display-bukrs.
*    gt_xml-field2 = gt_display-gjahr.
*    gt_xml-field3 = gt_display-monat.
*    gt_xml-field4 = gt_display-kostl.
*    gt_xml-field5 = gt_display-ktext.
*    gt_xml-field6 = gt_display-kstar.
*    gt_xml-field7 = gt_display-ktext2.
*    gt_xml-field8 = gt_display-zcb.
*    gt_xml-field9 = gt_display-zyl.
*    gt_xml-field10 = gt_display-zdj.

    APPEND gt_xml.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_GENERALXML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_FILENAME_DEF  text
*      -->P_L_STRANS  text
*      <--P_L_FILENAME  text
*      <--P_L_FLAG  text
*----------------------------------------------------------------------*
FORM frm_generalxml USING  VALUE(p_def_filename) p_trans CHANGING p_filename p_flag.
  TYPES: BEGIN OF typ_line,
           data(256) TYPE x, "STRING也行
         END OF typ_line.
  DATA: lt_xml_table TYPE STANDARD TABLE OF typ_line.
  DATA: l_result     TYPE xstring,
        l_xml_result TYPE string.

  DATA:oref   TYPE REF TO cx_root,
       l_text TYPE string.
  DATA:l_filename LIKE rlgrap-filename,
       l_mask     LIKE rlgrap-filename.
  DATA:path TYPE rlgrap-filename."存储路径
  CLEAR l_filename.
  l_mask = ',*.XLS,*.XLS.'.
  IF  g_destination IS INITIAL .
    path =  'C:\'.
  ELSE.
    path = g_destination.
  ENDIF.

  "删除多余空格
  CONDENSE  p_def_filename NO-GAPS.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING                                    "相对程序输出
      def_path         = path
      mask             = l_mask
      title            = '请选择保存路径'
      def_filename     = p_def_filename
      mode             = 'S'
    IMPORTING                     "相对程序输入
      filename         = l_filename
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.
  IF sy-subrc = 0 .
    g_destination  = l_filename.
  ENDIF.

  IF sy-subrc = 0.
    "在l_filename中搜索字符串XLS
    SEARCH l_filename FOR 'XLS'.
    IF sy-subrc = 0.
      CONCATENATE l_filename '' INTO p_filename.
    ELSE.
      CONCATENATE l_filename '.XLS' INTO p_filename.
    ENDIF.
  ENDIF.
  IF l_filename IS INITIAL .
    MESSAGE '取消下载' TYPE 'S'.
    p_flag = 'X'.
    RETURN.
  ELSE.
    PERFORM frm_build_source TABLES gt_xml
                          CHANGING  l_result.

    TRY .
        CALL TRANSFORMATION (p_trans)    "把EXCEL的摸版转换成XML的代码
            SOURCE XML l_result
            RESULT XML l_xml_result.
      CATCH cx_transformation_error INTO oref.
        l_text = oref->get_text( ).
        MESSAGE l_text TYPE 'S' DISPLAY LIKE 'E'.
        p_flag = 'X'.
        RETURN.
    ENDTRY.

    CALL FUNCTION 'SCMS_STRING_TO_FTEXT' "把STIRNG类型数据的转成内表
      EXPORTING
        text      = l_xml_result
      TABLES
        ftext_tab = lt_xml_table.

    CALL FUNCTION 'WS_DOWNLOAD' " 下载生成的EXCEL
      EXPORTING
        filename      = p_filename
        filetype      = 'BIN'
      TABLES
        data_tab      = lt_xml_table
      EXCEPTIONS
        unknown_error = 2.

  ENDIF.
ENDFORM.                    " FRM_GENERALXML

*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_SOURCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_XML  text
*      <--P_L_RESULT  text
*----------------------------------------------------------------------*
FORM frm_build_source TABLES    pt_xml
                                  "插入正确名称 <...>
                       CHANGING p_result.
  TYPE-POOLS: ixml.
  DATA: l_ostream        TYPE REF TO if_ixml_ostream,
        l_pstreamfactory TYPE REF TO if_ixml_stream_factory.
  DATA: l_xml_size TYPE i,
        l_outline  TYPE i.

  DATA: l_ixml TYPE REF TO if_ixml,
        l_doc  TYPE REF TO if_ixml_document,
        l_root TYPE REF TO if_ixml_element.


*  DESCRIBE TABLE P_IT_XML LINES l_OUTLINE.

  l_ixml = cl_ixml=>create( ).               "相当与JAVA中生成一个对象
  l_doc = l_ixml->create_document( ).         "生成DOCUMENT对象
  l_root = l_doc->create_simple_element( name = 'data' parent = l_doc ). "生成根节点
  PERFORM frm_build_xml_from_table TABLES pt_xml
                                   USING  l_doc
                                          'item'
                                          39
                                          l_outline
                                          l_root.
  l_pstreamfactory = l_ixml->create_stream_factory( ).

  l_ostream = l_pstreamfactory->create_ostream_xstring( p_result ).

  CALL METHOD l_doc->render( ostream = l_ostream recursive = 'X' ).

  l_xml_size = l_ostream->get_num_written_raw( ).
ENDFORM.                    " FRM_BUILD_SOURCE
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_XML_FROM_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_XML  text
*      -->P_L_DOC  text
*      -->P_2731   text
*      -->P_39     text
*      -->P_L_OUTLINE  text
*      -->P_L_ROOT  text
*----------------------------------------------------------------------*
FORM frm_build_xml_from_table   TABLES pt_table
                                                        USING  p_doc TYPE REF TO if_ixml_document
                                                                    p_name
                                                                    p_fieldcnt TYPE i
                                                                    p_rowcnt TYPE i
                                                                    p_parent TYPE REF TO if_ixml_element.
  FIELD-SYMBOLS:<fwa> TYPE any,        "相当与定义一个任意类型指针
                <f>   TYPE any.

  DATA:

*L_TABLE TYPE REF TO IF_IXML_ELEMENT,

    l_item      TYPE REF TO if_ixml_element,
    l_col       TYPE REF TO if_ixml_element,

*             L_VALUE TYPE REF TO IF_IXML_ELEMENT,

    l_tmp_value TYPE string,
    l_value     TYPE string,
    l_typ       TYPE c,
    l_rowid(8)  TYPE n,
    l_colid(3)  TYPE n,
    idx         TYPE i.


*  L_TMP_VALUE = P_NAME.
*  L_TABLE = P_DOC->CREATE_SIMPLE_ELEMENT( NAME = L_TMP_VALUE PARENT = P_PARENT ).



  LOOP AT pt_table ASSIGNING <fwa>.   "将FWA指针PT_TABLE
    l_item = p_doc->create_simple_element( name = p_name parent = p_parent )." NAME是节点名称 PRAENT 是父节点名称
    l_tmp_value = p_rowcnt.
    idx = l_item->set_attribute( name = 'rowcount' value =  l_tmp_value ).
    DO p_fieldcnt TIMES.
      ASSIGN COMPONENT sy-index OF STRUCTURE <fwa>  TO <f>.
      l_tmp_value = sy-index.
      CONCATENATE 'field' l_tmp_value INTO l_tmp_value.
      l_value = <f>.
      CONDENSE l_value.
      l_col = p_doc->create_simple_element( name = l_tmp_value parent = l_item  value = l_value ).
    ENDDO.
  ENDLOOP.

ENDFORM.                    " FRM_BUILD_XML_FROM_TABLE
*&---------------------------------------------------------------------*
*&      Form  FRM_PROCESS_INDCATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2354   text
*      -->P_80     text
*----------------------------------------------------------------------*
FORM frm_process_indcator USING text percentage.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = percentage
      text       = text.
ENDFORM.                    " FRM_PROCESS_INDCATOR
*&---------------------------------------------------------------------*
*&      Form  FRM_OPENREPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_FILENAME  text
*      -->P_1760   text
*----------------------------------------------------------------------*
FORM frm_openreport  USING    p_filename p_sheet TYPE  i..
  CREATE OBJECT application 'EXCEL.APPLICATION'.

  CALL METHOD OF application 'WORKBOOKS' = workbook.
  CALL METHOD OF workbook 'OPEN'
    EXPORTING
      #1 = p_filename.

  CALL METHOD OF application 'WORKSHEETS' = sheet
    EXPORTING
    #1 = p_sheet.
  CALL METHOD OF sheet 'ACTIVATE'.

  SET PROPERTY OF application 'VISIBLE' = 1.

  FREE OBJECT:sheet,workbook,application.

ENDFORM.                    " FRM_OPENREPORT
*&---------------------------------------------------------------------*
*& Form FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_auth_check .
  IF s_budat IS INITIAL.
    MESSAGE '记账日期为必输项！' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
*  IF p1 = ''.
*  IF s_budat-low(6) <> s_budat-high(6).
*    MESSAGE '记账日期请选择同一期间！' TYPE 'S' DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.
*  ENDIF.

  IF p12 = 'X'.
*    IF S_BUDAT-HIGH IS NOT INITIAL.
*      IF S_BUDAT-LOW <> S_BUDAT-HIGH.
*        MESSAGE '记账日期请选择同一天！' TYPE 'S' DISPLAY LIKE 'E'.
*        STOP.
*      ENDIF.
*    ENDIF.
  ELSE.
*    IF s_budat-high IS NOT INITIAL.
    IF s_budat-low+0(6) <> s_budat-low+0(6).
      MESSAGE '记账日期请选择同一期间！' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
*    ENDIF.
  ENDIF.

  IF s_blart-low IS NOT INITIAL.
    IF     s_blart-low = 'WE'
        OR s_blart-low = 'WL'
       OR  s_blart-low = 'WA'
       OR  s_blart-low = 'ML'
       OR  s_blart-low = 'PR'
       OR  s_blart-low = 'WI'
       OR  s_blart-low = 'AF'.
      MESSAGE '需要汇总打印的凭证请不要选择一般打印！' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.



  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
   ID 'BUKRS' FIELD s_bukrs-low.
  IF sy-subrc <> 0.
    MESSAGE '您没有该公司代码的权限！' TYPE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_PRINT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_print.

  DATA wa_header TYPE zsfi_ssf_001.
  DATA lt_item TYPE TABLE OF zsfi_ssf_002 WITH HEADER LINE.
  DATA l_kunnr TYPE kunnr.
  DATA l_ts(4) TYPE c.
  DATA : lv_jexx TYPE string,
         lv_jedx TYPE string.
*  DATA l_ktext TYPE ktext_curt.
  DATA:l_txt20 TYPE txt20_skat.
  DATA:l_name1 TYPE name1_gp.
  DATA:l_post1 TYPE ps_post1.
  DATA:l_fkbtx TYPE fkbtx.

  DATA l_sum TYPE dmbtr.
  DATA l_num TYPE i.
  DATA l_zpage TYPE i.

  DATA:lwa_ztfi_print_002 TYPE ztfi_print_001.


****获取当前科目凭证需要打印哪些辅助核算信息的字段
  DATA:lt_ztfi_print_001 TYPE TABLE OF ztfi_print_003.
  DATA:wa_ztfi_print_001 TYPE ztfi_print_003.

  SELECT *
    INTO TABLE lt_ztfi_print_001
  FROM ztfi_print_003.
  SORT lt_ztfi_print_001 BY hkont1.

  DATA: lw_ztfi_print_005 TYPE ztfi_print_002.
  DATA: lt_ztfi_print_005 TYPE TABLE OF ztfi_print_002.
  CLEAR: lw_ztfi_print_005.

  DATA: control    TYPE ssfctrlop,
        out_option TYPE ssfcompop,
        fm_name    TYPE rs38l_fnam.

  "设置用户参数可用***
  out_option-tdimmed = 'X'.
  out_option-tdnewid = 'X'.
  out_option-tddelete = 'X'.
  out_option-tdfinal = 'X'.
  out_option-tdiexit = 'X'.
  out_option-tddest = 'LP01'.

  control-preview   = 'X'.
  control-no_open   = 'X'.
  control-no_close  = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
*     user_settings      = 'X'
      control_parameters = control
      output_options     = out_option
      user_settings      = ''
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

*  LOOP AT gt_ckgl_001 WHERE SEL = 'X'.
*
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZSF_FICO_001_PRINT'
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

***
  DATA l_varkey TYPE vim_enqkey.
  DATA l_index TYPE sy-tabix.

******获取描述
  CLEAR lwa_ztfi_print_002.

  LOOP AT gt_display WHERE sel = 'X'.
    l_index = sy-tabix.
    CLEAR:wa_header,lt_item[].

*****获取凭证编码
***    wa_header-ZPZBH
    IF gt_display-zpzbh <> ''.
      wa_header-zpzbh = gt_display-zpzbh.
    ELSE.
      "需要重新编号
      IF lwa_ztfi_print_002 IS NOT INITIAL.
        IF lwa_ztfi_print_002-bukrs = gt_display-bukrs
          AND lwa_ztfi_print_002-gjahr = gt_display-gjahr
          AND lwa_ztfi_print_002-monat = gt_display-monat
          .
          lwa_ztfi_print_002-zpzbh = lwa_ztfi_print_002-zpzbh + 1.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lwa_ztfi_print_002-zpzbh
            IMPORTING
              output = lwa_ztfi_print_002-zpzbh.

          gt_display-zpzbh = lwa_ztfi_print_002-zpzbh.
        ELSE.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lwa_ztfi_print_002-zpzbh
            IMPORTING
              output = lwa_ztfi_print_002-zpzbh.

          MODIFY ztfi_print_001 FROM lwa_ztfi_print_002.
          IF sy-subrc EQ 0.
            COMMIT WORK.
          ENDIF.
          CLEAR l_varkey.
          CONCATENATE lwa_ztfi_print_002-mandt lwa_ztfi_print_002-bukrs lwa_ztfi_print_002-gjahr lwa_ztfi_print_002-monat INTO l_varkey.
          CALL FUNCTION 'DEQUEUE_E_TABLE'
            EXPORTING
              mode_rstable = 'E'
              tabname      = 'ZTFI_PRINT_001'
              varkey       = l_varkey
*             X_TABNAME    = ' '
*             X_VARKEY     = ' '
*             _SCOPE       = '3'
*             _SYNCHRON    = ' '
*             _COLLECT     = ' '
            .
          CLEAR lwa_ztfi_print_002.
          SELECT SINGLE *
           INTO lwa_ztfi_print_002
           FROM ztfi_print_001
           WHERE bukrs = gt_display-bukrs
             AND gjahr = gt_display-gjahr
          AND monat = gt_display-monat.
          IF sy-subrc EQ 0 .
            CLEAR l_varkey.
            CONCATENATE lwa_ztfi_print_002-mandt lwa_ztfi_print_002-bukrs lwa_ztfi_print_002-gjahr lwa_ztfi_print_002-monat INTO l_varkey.
            CALL FUNCTION 'ENQUEUE_E_TABLE'
              EXPORTING
                mode_rstable   = 'E'
                tabname        = 'ZTFI_PRINT_001'
                varkey         = l_varkey
*               X_TABNAME      = ' '
*               X_VARKEY       = ' '
*               _SCOPE         = '2'
*               _WAIT          = ' '
*               _COLLECT       = ' '
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.
            IF sy-subrc <> 0.
*               Implement suitable error handling here
            ENDIF.
            lwa_ztfi_print_002-zpzbh = lwa_ztfi_print_002-zpzbh + 1.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lwa_ztfi_print_002-zpzbh
              IMPORTING
                output = lwa_ztfi_print_002-zpzbh.

            gt_display-zpzbh = lwa_ztfi_print_002-zpzbh.
          ELSE.
            lwa_ztfi_print_002-bukrs = gt_display-bukrs.
            lwa_ztfi_print_002-gjahr = gt_display-gjahr.
            lwa_ztfi_print_002-monat = gt_display-monat.
            lwa_ztfi_print_002-zpzbh = 1.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lwa_ztfi_print_002-zpzbh
              IMPORTING
                output = lwa_ztfi_print_002-zpzbh.
            gt_display-zpzbh = lwa_ztfi_print_002-zpzbh.
          ENDIF.
        ENDIF.
      ELSE.
        CLEAR lwa_ztfi_print_002.
        SELECT SINGLE *
         INTO lwa_ztfi_print_002
         FROM ztfi_print_001
         WHERE bukrs = gt_display-bukrs
           AND gjahr = gt_display-gjahr
        AND monat = gt_display-monat.
        IF sy-subrc EQ 0.
          CLEAR l_varkey.
          CONCATENATE lwa_ztfi_print_002-mandt lwa_ztfi_print_002-bukrs lwa_ztfi_print_002-gjahr lwa_ztfi_print_002-monat INTO l_varkey.
          CALL FUNCTION 'ENQUEUE_E_TABLE'
            EXPORTING
              mode_rstable   = 'E'
              tabname        = 'ZTFI_PRINT_001'
              varkey         = l_varkey
*             X_TABNAME      = ' '
*             X_VARKEY       = ' '
*             _SCOPE         = '2'
*             _WAIT          = ' '
*             _COLLECT       = ' '
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc <> 0.
*               Implement suitable error handling here
          ENDIF.
          lwa_ztfi_print_002-zpzbh = lwa_ztfi_print_002-zpzbh + 1.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lwa_ztfi_print_002-zpzbh
            IMPORTING
              output = lwa_ztfi_print_002-zpzbh.
          gt_display-zpzbh = lwa_ztfi_print_002-zpzbh.
        ELSE.
          lwa_ztfi_print_002-bukrs = gt_display-bukrs.
          lwa_ztfi_print_002-gjahr = gt_display-gjahr.
          lwa_ztfi_print_002-monat = gt_display-monat.
          lwa_ztfi_print_002-zpzbh = 1.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lwa_ztfi_print_002-zpzbh
            IMPORTING
              output = lwa_ztfi_print_002-zpzbh.
          gt_display-zpzbh = lwa_ztfi_print_002-zpzbh.
        ENDIF.
      ENDIF.

      wa_header-zpzbh = lwa_ztfi_print_002-zpzbh.
    ENDIF.


    CONCATENATE '第' wa_header-zpzbh '号凭证' INTO wa_header-zpzbh.
    CONDENSE wa_header-zpzbh NO-GAPS.
    wa_header-belnr = gt_display-belnr.
    wa_header-blart = gt_display-blart.
    CONCATENATE '凭证类型：' wa_header-blart INTO wa_header-blart.
    SELECT SINGLE butxt      INTO wa_header-butxt  FROM t001      WHERE bukrs = gt_display-bukrs.
    SELECT SINGLE name_textc INTO wa_header-zjzry  FROM user_addr WHERE bname = gt_display-usnam.
    SELECT SINGLE name_textc INTO wa_header-zzdr   FROM user_addr WHERE bname = sy-uname.
    CONCATENATE gt_display-budat(4) '年' gt_display-budat+4(2) '月' gt_display-budat+6(2) '日' INTO wa_header-zjzrq.
    SELECT SINGLE ktext INTO wa_header-zbz FROM tcurt WHERE spras = '1' AND waers = gt_display-waers.
    wa_header-zdw = '元'.

    l_sum = 0.
    l_num = 0.

    LOOP AT gt_bseg WHERE bukrs = gt_display-bukrs AND gjahr = gt_display-gjahr AND belnr = gt_display-belnr.
      CLEAR wa_ztfi_print_001.
      READ TABLE lt_ztfi_print_001 INTO wa_ztfi_print_001 WITH KEY hkont1 = gt_bseg-hkont BINARY SEARCH.
      IF sy-subrc EQ 0.

      ELSE.
        LOOP AT lt_ztfi_print_001 INTO wa_ztfi_print_001 WHERE hkont1 < gt_bseg-hkont AND hkont2 >= gt_bseg-hkont.
          EXIT.
        ENDLOOP.
      ENDIF.

      CLEAR lt_item.
      "摘要
      lt_item-sgtxt = gt_bseg-sgtxt.
      "会计科目 描述 及 辅助核算字段描述
      CLEAR l_txt20.
      SELECT SINGLE txt20 INTO l_txt20 FROM skat WHERE spras = '1' AND ktopl = 'CXZX' AND saknr = gt_bseg-hkont.
      IF sy-subrc EQ 0.
        CONCATENATE lt_item-zkjkm  l_txt20 INTO lt_item-zkjkm.
      ENDIF.
      "资金账号
      IF wa_ztfi_print_001-zuonr = 'X'.
        IF lt_item-zkjkm = ''.
          CONCATENATE lt_item-zkjkm  gt_bseg-zuonr INTO lt_item-zkjkm.
        ELSE.
          CONCATENATE lt_item-zkjkm '\' gt_bseg-zuonr INTO lt_item-zkjkm.
        ENDIF.
      ENDIF.
      "客户
      IF wa_ztfi_print_001-kunnr = 'X'.
        IF gt_bseg-kunnr <> ''.
          CLEAR l_name1.
          SELECT SINGLE name1 INTO l_name1 FROM kna1 WHERE kunnr = gt_bseg-kunnr..
          IF sy-subrc EQ 0.
            IF lt_item-zkjkm = ''.
              CONCATENATE lt_item-zkjkm  l_name1 INTO lt_item-zkjkm.
            ELSE.
              CONCATENATE lt_item-zkjkm '\' l_name1 INTO lt_item-zkjkm.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      "供应商
      IF wa_ztfi_print_001-lifnr = 'X'.
        IF gt_bseg-lifnr <> ''.
          CLEAR l_name1.
          SELECT SINGLE name1 INTO l_name1 FROM lfa1 WHERE lifnr = gt_bseg-lifnr..
          IF sy-subrc EQ 0.
            IF lt_item-zkjkm = ''.
              CONCATENATE lt_item-zkjkm  l_name1 INTO lt_item-zkjkm.
            ELSE.
              CONCATENATE lt_item-zkjkm '\' l_name1 INTO lt_item-zkjkm.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      "贸易伙伴
      IF wa_ztfi_print_001-vptnr = 'X'.
        IF gt_bseg-vptnr <> ''.
          CLEAR l_name1.
          SELECT SINGLE name1 INTO l_name1 FROM kna1 WHERE kunnr = gt_bseg-vptnr..
          IF sy-subrc EQ 0.
            IF lt_item-zkjkm = ''.
              CONCATENATE lt_item-zkjkm  l_name1 INTO lt_item-zkjkm.
            ELSE.
              CONCATENATE lt_item-zkjkm '\' l_name1 INTO lt_item-zkjkm.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      "功能范围
      IF p13 = 'X'.
        IF gt_bseg-fkber <> ''.
          CLEAR l_fkbtx.
          SELECT SINGLE fkbtx INTO l_fkbtx FROM tfkbt WHERE spras = '1' AND fkber = gt_bseg-fkber.
          IF lt_item-zkjkm = ''.
            CONCATENATE lt_item-zkjkm  l_fkbtx INTO lt_item-zkjkm.
          ELSE.
            CONCATENATE lt_item-zkjkm '\' l_fkbtx INTO lt_item-zkjkm.
          ENDIF.
        ENDIF.
      ELSE.
        IF wa_ztfi_print_001-fkber = 'X'.
          IF gt_bseg-fkber <> ''.
            CLEAR l_fkbtx.
            SELECT SINGLE fkbtx INTO l_fkbtx FROM tfkbt WHERE spras = '1' AND fkber = gt_bseg-fkber.
            IF lt_item-zkjkm = ''.
              CONCATENATE lt_item-zkjkm  l_fkbtx INTO lt_item-zkjkm.
            ELSE.
              CONCATENATE lt_item-zkjkm '\' l_fkbtx INTO lt_item-zkjkm.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      "成本中心
      IF wa_ztfi_print_001-kostl = 'X'.
        IF gt_bseg-kostl <> ''.
          CLEAR l_name1.
          SELECT SINGLE ktext INTO l_name1 FROM cskt WHERE spras = '1' AND kokrs = '3000' AND datbi > sy-datum AND kostl = gt_bseg-kostl.
          IF sy-subrc EQ 0.
            IF lt_item-zkjkm = ''.
              CONCATENATE lt_item-zkjkm  l_name1 INTO lt_item-zkjkm.
            ELSE.
              CONCATENATE lt_item-zkjkm '\' l_name1 INTO lt_item-zkjkm.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      "WBS元素
      IF wa_ztfi_print_001-projk = 'X'.
        IF gt_bseg-projk <> ''.
          CLEAR l_post1.
          SELECT SINGLE post1 INTO l_post1 FROM prps WHERE pspnr = gt_bseg-projk.
          IF sy-subrc EQ 0.
            IF lt_item-zkjkm = ''.
              CONCATENATE lt_item-zkjkm  l_post1 INTO lt_item-zkjkm.
            ELSE.
              CONCATENATE lt_item-zkjkm '\' l_post1 INTO lt_item-zkjkm.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.


      "借方金额 和 贷方金额
      IF gt_bseg-shkzg = 'S'.

        IF gt_bseg-xnegp = ''.
          lt_item-zjfje = gt_bseg-dmbtr.
*          L_SUM = L_SUM + GT_BSEG-DMBTR.
        ELSE.
          lt_item-zdfje = 0 - gt_bseg-dmbtr.
*          L_SUM = L_SUM - GT_BSEG-DMBTR.
        ENDIF.

      ELSE.
        IF gt_bseg-xnegp = ''.
          lt_item-zdfje = gt_bseg-dmbtr.
*          L_SUM = L_SUM + GT_BSEG-DMBTR.
        ELSE.
          lt_item-zjfje = 0 - gt_bseg-dmbtr.
*          l_sum = l_sum - gt_bseg-dmbtr.
        ENDIF.
      ENDIF.
      l_sum = l_sum + lt_item-zjfje.
      APPEND lt_item.
      l_num = l_num + 1.
    ENDLOOP.










    "总页数
    l_zpage = l_num DIV 6  + 1.
    wa_header-zpage = l_zpage.
    CONDENSE wa_header-zpage NO-GAPS.

    CLEAR:l_num,lt_item.
    l_num = gt_display-numpg.
    lt_item-sgtxt = l_num.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lt_item-sgtxt
      IMPORTING
        output = lt_item-sgtxt.

    CONCATENATE  '附单据：' lt_item-sgtxt '张' INTO lt_item-sgtxt.


    lt_item-zjfje = l_sum.
    lt_item-zdfje = l_sum.
***合计金额及大小写转换
    IF l_sum < 0.
      l_sum = 0 - l_sum.
    ENDIF.
    lv_jexx = l_sum.
    CALL FUNCTION 'ZFI_MONEY_LOWER_TO_UPPER'
      EXPORTING
        moneyin  = lv_jexx
      IMPORTING
        moneyout = lv_jedx.

    CONCATENATE '合计：' lv_jedx INTO lt_item-zkjkm.
    SORT lt_item BY zjfje DESCENDING.
    APPEND lt_item.

    CALL FUNCTION fm_name
      EXPORTING
        control_parameters = control
        output_option      = out_option
        i_header           = wa_header
      TABLES
        t_item             = lt_item[]
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


****将凭证打印编号更新到BKPF中，并对数据解锁
    IF gt_display-zpzbh <> '' AND gt_display-print_flag = ''..
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gt_display-zpzbh
        IMPORTING
          output = gt_display-zpzbh.
*      UPDATE BKPF SET XREF2_HD = GT_DISPLAY-ZPZBH
*       WHERE BUKRS = GT_DISPLAY-BUKRS
*         AND BELNR = GT_DISPLAY-BELNR
*         AND GJAHR = GT_DISPLAY-GJAHR.
*      IF SY-SUBRC EQ 0.
*        COMMIT WORK.
*      ENDIF.
      CLEAR lw_ztfi_print_005.

      lw_ztfi_print_005-madnt = sy-mandt ."集团
      lw_ztfi_print_005-bukrs = gt_display-bukrs ."公司代码
      lw_ztfi_print_005-gjahr = gt_display-gjahr ."财年
      lw_ztfi_print_005-belnr = gt_display-belnr ."会计凭证号码
      lw_ztfi_print_005-zdylx = gv_dylx ."打印类型
      lw_ztfi_print_005-zpzbh = gt_display-zpzbh ."打印凭证编号(当前期间最大号)
      lw_ztfi_print_005-zernam = sy-uname ."创建人
      lw_ztfi_print_005-zerdat = sy-datum."创建日期
      lw_ztfi_print_005-zertim = sy-uzeit ."创建时间
      IF lw_ztfi_print_005 IS NOT INITIAL.
        MODIFY ztfi_print_002 FROM lw_ztfi_print_005 .


        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.
      ENDIF.
      CLEAR l_varkey.
      CONCATENATE sy-mandt gt_display-bukrs gt_display-belnr gt_display-gjahr INTO l_varkey.
      CALL FUNCTION 'DEQUEUE_E_TABLE'
        EXPORTING
          mode_rstable = 'E'
          tabname      = 'BKPF'
          varkey       = l_varkey
*         X_TABNAME    = ' '
*         X_VARKEY     = ' '
*         _SCOPE       = '3'
*         _SYNCHRON    = ' '
*         _COLLECT     = ' '
        .
    ENDIF.


    MODIFY gt_display INDEX l_index.
  ENDLOOP.


  IF lwa_ztfi_print_002 IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_ztfi_print_002-zpzbh
      IMPORTING
        output = lwa_ztfi_print_002-zpzbh.
    MODIFY ztfi_print_001 FROM lwa_ztfi_print_002.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
    CLEAR l_varkey.
    CONCATENATE lwa_ztfi_print_002-mandt lwa_ztfi_print_002-bukrs lwa_ztfi_print_002-gjahr lwa_ztfi_print_002-monat INTO l_varkey.
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable = 'E'
        tabname      = 'ZTFI_PRINT_001'
        varkey       = l_varkey
*       X_TABNAME    = ' '
*       X_VARKEY     = ' '
*       _SCOPE       = '3'
*       _SYNCHRON    = ' '
*       _COLLECT     = ' '
      .
    CLEAR lwa_ztfi_print_002.
  ENDIF.


  CALL FUNCTION 'SSF_CLOSE'
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_SEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_sel .


  IF p2 = 'X'.
    CLEAR:s_blart[].
    CLEAR s_blart.
    s_blart-sign = 'I'.
    s_blart-option = 'EQ'.
    s_blart-low = 'WE'.
    APPEND s_blart.
  ELSEIF p3 = 'X'.
    CLEAR:s_blart[].
    CLEAR s_blart.
    s_blart-sign = 'I'.
    s_blart-option = 'EQ'.
    s_blart-low = 'WL'.
    APPEND s_blart.
  ELSEIF p4 = 'X'.
    CLEAR:s_blart[].
    CLEAR s_blart.
    s_blart-sign = 'I'.
    s_blart-option = 'EQ'.
    s_blart-low = 'WA'.
    APPEND s_blart.
  ELSEIF p5 = 'X'.
    CLEAR:s_blart[].
    CLEAR s_blart.
    s_blart-sign = 'I'.
    s_blart-option = 'EQ'.
    s_blart-low = 'ML'.
    APPEND s_blart.
  ELSEIF p6 = 'X'.
    CLEAR:s_blart[].
    CLEAR s_blart.
    s_blart-sign = 'I'.
    s_blart-option = 'EQ'.
    s_blart-low = 'PR'.
    APPEND s_blart.
  ELSEIF p7 = 'X'.
    CLEAR:s_blart[].
    CLEAR s_blart.
    s_blart-sign = 'I'.
    s_blart-option = 'EQ'.
    s_blart-low = 'AF'.
    APPEND s_blart.
  ELSEIF p9 = 'X'.
    CLEAR:s_blart[].
    CLEAR s_blart.
    s_blart-sign = 'I'.
    s_blart-option = 'EQ'.
    s_blart-low = 'WI'.
    APPEND s_blart.
  ELSEIF p11 = 'X'.
    CLEAR:s_blart[].
    CLEAR s_blart.
    s_blart-sign = 'I'.
    s_blart-option = 'EQ'.
    s_blart-low = 'RE'.
    APPEND s_blart.
  ELSEIF p12 = 'X'.
    CLEAR:s_blart[].
    CLEAR s_blart.
    s_blart-sign = 'I'.
    s_blart-option = 'EQ'.
    s_blart-low = 'YS'.
    APPEND s_blart.

  ELSEIF p13 = 'X'.
    CLEAR:s_blart[].
    CLEAR s_blart.
    s_blart-sign = 'I'.
    s_blart-option = 'EQ'.
    s_blart-low = 'SA'.
    APPEND s_blart.

  ENDIF.

  "自由凭证打印
  IF p8 = 'X'.
    CLEAR:s_usnam,s_usnam[].
    s_usnam-sign = 'I'.
    s_usnam-option = 'EQ'.
    s_usnam-low = sy-uname.
    APPEND s_usnam.
  ENDIF.
  CLEAR gv_dylx.
  IF p1 = 'X' OR p13 = 'X'.
    gv_dylx = '1'."单张打印
  ELSE.
    gv_dylx = '2'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_PRINT_SUM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_print_sum .
  DATA wa_header TYPE zsfi_ssf_001.
  DATA lt_item TYPE TABLE OF zsfi_ssf_002 WITH HEADER LINE.
  DATA l_kunnr TYPE kunnr.
  DATA l_ts(4) TYPE c.
  DATA : lv_jexx TYPE string,
         lv_jedx TYPE string.
*  DATA l_ktext TYPE ktext_curt.
  DATA:l_txt20 TYPE txt20_skat.
  DATA:l_name1 TYPE name1_gp.
  DATA:l_post1 TYPE ps_post1.
  DATA:l_fkbtx TYPE fkbtx.
  DATA l_sum TYPE dmbtr.
  DATA l_num TYPE i.
  DATA l_zpage TYPE i.
*  DATA:l_name1 TYPE name1_gp.
  DATA:lwa_ztfi_print_002 TYPE ztfi_print_001.
  DATA:lwa_ztfi_print_005 TYPE ztfi_print_002.
  DATA:lta_ztfi_print_005 TYPE TABLE OF ztfi_print_002.



**
******获取当前科目凭证需要打印哪些辅助核算信息的字段
**  DATA:lt_ZTFI_PRINT_001 TYPE TABLE OF ZTFI_PRINT_003.
**  DATA:wa_ZTFI_PRINT_001 TYPE ZTFI_PRINT_003.
**
**  SELECT *
**    INTO TABLE lt_ZTFI_PRINT_001
**    FROM ZTFI_PRINT_003.
**  SORT lt_ZTFI_PRINT_001 BY hkont1.




  DATA: control    TYPE ssfctrlop,
        out_option TYPE ssfcompop,
        fm_name    TYPE rs38l_fnam.

  "设置用户参数可用***
  out_option-tdimmed = 'X'.
  out_option-tdnewid = 'X'.
  out_option-tddelete = 'X'.
  out_option-tdfinal = 'X'.
  out_option-tdiexit = 'X'.
  out_option-tddest = 'LP01'.

  control-preview   = 'X'.
  control-no_open   = 'X'.
  control-no_close  = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
*     user_settings      = 'X'
      control_parameters = control
      output_options     = out_option
      user_settings      = ''
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

*  LOOP AT gt_ckgl_001 WHERE SEL = 'X'.
*
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZSF_FICO_001_PRINT'
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

***
  DATA l_index TYPE sy-tabix.


  l_index = sy-tabix.
  CLEAR:wa_header,lt_item[].

***凭证号排序
  SORT gt_display BY belnr.
  "最小凭证号
  READ TABLE gt_display INDEX 1.
  IF sy-subrc EQ 0.
    wa_header-zpzbh = gt_display-zpzbh.
    IF gt_display-zpzbh IS INITIAL.
      CLEAR lwa_ztfi_print_002.
      SELECT SINGLE *
       INTO lwa_ztfi_print_002
       FROM ztfi_print_001
       WHERE bukrs = gt_display-bukrs
         AND gjahr = gt_display-gjahr
         AND monat = gt_display-monat.
      IF sy-subrc EQ 0.
        DATA: l_varkey TYPE rstable-varkey.
        CLEAR l_varkey.
        CONCATENATE lwa_ztfi_print_002-mandt lwa_ztfi_print_002-bukrs lwa_ztfi_print_002-gjahr lwa_ztfi_print_002-monat INTO l_varkey.
        CALL FUNCTION 'ENQUEUE_E_TABLE'
          EXPORTING
            mode_rstable   = 'E'
            tabname        = 'ZTFI_PRINT_001'
            varkey         = l_varkey
*           X_TABNAME      = ' '
*           X_VARKEY       = ' '
*           _SCOPE         = '2'
*           _WAIT          = ' '
*           _COLLECT       = ' '
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
*               Implement suitable error handling here
        ENDIF.
        lwa_ztfi_print_002-zpzbh = lwa_ztfi_print_002-zpzbh + 1.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lwa_ztfi_print_002-zpzbh
          IMPORTING
            output = lwa_ztfi_print_002-zpzbh.
        wa_header-zpzbh = lwa_ztfi_print_002-zpzbh.
      ENDIF.

    ENDIF.
  ENDIF.

**公司代码描述 制单人 币种 单位
  SELECT SINGLE butxt      INTO wa_header-butxt  FROM t001      WHERE bukrs = gt_display-bukrs.
  SELECT SINGLE name_textc INTO wa_header-zzdr   FROM user_addr WHERE bname = sy-uname.
  SELECT SINGLE ktext INTO wa_header-zbz FROM tcurt WHERE spras = '1' AND waers = gt_display-waers.
  wa_header-zdw = '元'.


  "最大凭证号
*  L_NUM = 0.
*  L_NUM = LINES( GT_DISPLAY[] ).
*  IF L_NUM > 1.
*    READ TABLE GT_DISPLAY INDEX L_NUM.
*    IF SY-SUBRC EQ 0.
*      CONCATENATE WA_HEADER-ZPZBH '-' GT_DISPLAY-BELNR INTO WA_HEADER-ZPZBH.
*    ENDIF.
*  ENDIF.
****记账日期
  READ TABLE s_budat INDEX 1.
  CONCATENATE s_budat-low '-' s_budat-high INTO wa_header-zjzrq.

  wa_header-blart = gt_display-blart.
  CONCATENATE '凭证类型：' wa_header-blart INTO wa_header-blart.

**凭证类型描述
  SELECT SINGLE ltext INTO wa_header-zpzlx FROM t003t WHERE spras = '1' AND blart = gt_display-blart.


  l_sum = 0.

  LOOP AT gt_bseg.
    CLEAR lt_item.
    "摘要
    lt_item-zkjkm = gt_bseg-hkont.

    "借方金额 和 贷方金额
    IF gt_bseg-shkzg = 'S'.
      IF gt_bseg-xnegp = ''.
        lt_item-zjfje = gt_bseg-dmbtr.
        l_sum = l_sum + gt_bseg-dmbtr.
      ELSE.
        lt_item-zdfje = 0 - gt_bseg-dmbtr.
        l_sum = l_sum - gt_bseg-dmbtr.
      ENDIF.
    ELSE.
      IF gt_bseg-xnegp = ''.
        lt_item-zdfje = gt_bseg-dmbtr.
      ELSE.
        lt_item-zjfje = 0 - gt_bseg-dmbtr.
      ENDIF.
    ENDIF.
    "存 成本中心
    IF p1 = ''.
      IF p7 = ''.
        lt_item-sgtxt = gt_bseg-kostl.
        CONDENSE lt_item-sgtxt NO-GAPS .
      ELSE.
        CONCATENATE gt_bseg-kostl gt_bseg-fkber INTO lt_item-sgtxt.
        CONDENSE lt_item-sgtxt NO-GAPS .
      ENDIF.
    ENDIF.
    COLLECT lt_item.


    "2019-04-08 by sw 注释

*    IF p7 = 'X' AND lwa_ztfi_print_002 IS NOT INITIAL .
*      CLEAR lwa_ztfi_print_005.
*      MOVE-CORRESPONDING lwa_ztfi_print_002 TO lwa_ztfi_print_005.
*      lwa_ztfi_print_005-zdylx = gv_dylx.
*      lwa_ztfi_print_005-zpzbh = lwa_ztfi_print_002-zpzbh.
*      lwa_ztfi_print_005-belnr = gt_bseg-belnr.
*      lwa_ztfi_print_005-bukrs = gt_bseg-bukrs.
*      lwa_ztfi_print_005-gjahr = gt_bseg-gjahr.
*      lwa_ztfi_print_005-zernam = sy-uname.
*      lwa_ztfi_print_005-zerdat = sy-datum.
*      lwa_ztfi_print_005-zertim  = sy-uzeit.
*      MODIFY ZTFI_PRINT_002 FROM    lwa_ztfi_print_005  .
*
*    ENDIF.
    "2019-04-08 by sw    注释





  ENDLOOP.



****取科目描述
  SORT lt_item BY zkjkm.
  l_num = 0.
  LOOP AT lt_item.
    "摘要
**    CONCATENATE wa_header-zpzlx '汇总' INTO lt_item-sgtxt.

    "科目描述
    CLEAR l_txt20.
    SELECT SINGLE txt20 INTO l_txt20 FROM skat WHERE spras = '1' AND ktopl = 'CXZX' AND saknr = lt_item-zkjkm.
    IF sy-subrc EQ 0.
*      CONCATENATE lt_item-zkjkm  ' '  l_txt20 INTO lt_item-zkjkm.
      lt_item-zkjkm = l_txt20.
    ENDIF.

    IF lt_item-sgtxt <> ''.
      IF p7 = ''.
        CLEAR l_name1.
        SELECT SINGLE ktext INTO l_name1 FROM cskt WHERE spras = '1' AND kokrs = '3000' AND datbi > sy-datum AND kostl = lt_item-sgtxt.
        IF sy-subrc EQ 0.
          CONCATENATE  lt_item-zkjkm  '\'  l_name1 INTO lt_item-zkjkm.
        ENDIF.
      ELSE.
        "成本中心
        CLEAR l_name1.
        SELECT SINGLE ktext INTO l_name1 FROM cskt WHERE spras = '1' AND kokrs = '3000' AND datbi > sy-datum AND kostl = lt_item-sgtxt(9).
        IF sy-subrc EQ 0.
          CONCATENATE  lt_item-zkjkm  '\'  l_name1 INTO lt_item-zkjkm.
        ENDIF.
        "功能范围
        CLEAR l_fkbtx.
        SELECT SINGLE fkbtx INTO l_fkbtx FROM tfkbt WHERE spras = '1' AND fkber = lt_item-sgtxt+9(16).
        IF sy-subrc EQ 0.
          CONCATENATE  lt_item-zkjkm  '\'  l_fkbtx INTO lt_item-zkjkm.
        ENDIF.
      ENDIF.
    ENDIF.


    CLEAR lt_item-sgtxt.
    IF p2 = 'X'.
      lt_item-sgtxt = '采购入库汇总'.
    ELSEIF p3 = 'X'.
      lt_item-sgtxt = 'STO采购入库和销售出库汇总'.
    ELSEIF p4 = 'X'.
      lt_item-sgtxt = '其他出入汇总'.
    ELSEIF p5 = 'X'.
      lt_item-sgtxt = '物料帐结算汇总'.
    ELSEIF p6 = 'X'.
      lt_item-sgtxt = '物料价格变更汇总'.
    ELSEIF p7 = 'X'.
      lt_item-sgtxt = '资产折旧汇总'.
    ELSEIF p9 = 'X'.
      lt_item-sgtxt = '其他库存凭证汇总'.

*    ELSEIF p13 = 'X'.
*      lt_item-sgtxt = '总账凭证汇总'.
    ENDIF.
    MODIFY lt_item.
    l_num = l_num + 1."计数器
  ENDLOOP.

  "总页数
  l_zpage = l_num DIV 6  + 1.
  wa_header-zpage = l_zpage.
  CONDENSE wa_header-zpage NO-GAPS.

  CLEAR:lt_item.
***合计金额及大小写转换
  IF l_sum < 0.
    l_sum = 0 - l_sum.
  ENDIF.
  lv_jexx = l_sum.
  CALL FUNCTION 'ZFI_MONEY_LOWER_TO_UPPER'
    EXPORTING
      moneyin  = lv_jexx
    IMPORTING
      moneyout = lv_jedx.

  CONCATENATE '合计：' lv_jedx INTO lt_item-zkjkm.
  lt_item-zjfje = l_sum.
  lt_item-zdfje = l_sum.
  SORT lt_item BY zjfje DESCENDING.
  APPEND lt_item.


  CALL FUNCTION fm_name
    EXPORTING
      control_parameters = control
      output_option      = out_option
      i_header           = wa_header
    TABLES
      t_item             = lt_item[]
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT gt_display.
    gt_display-zpzbh = wa_header-zpzbh.
    MODIFY gt_display.
  ENDLOOP.

  DATA:lw_ztfi_print_004 TYPE ztfi_print_004.

  lw_ztfi_print_004-bukrs = gt_display-bukrs.
  lw_ztfi_print_004-gjahr = gt_display-gjahr.
  lw_ztfi_print_004-monat = gt_display-monat.
  lw_ztfi_print_004-blart = gt_display-blart.
  lw_ztfi_print_004-zpzbh = gt_display-zpzbh.

  MODIFY ztfi_print_004 FROM lw_ztfi_print_004.

  IF lwa_ztfi_print_002 IS NOT INITIAL.
    MODIFY ztfi_print_001 FROM lwa_ztfi_print_002.
    COMMIT WORK.
  ENDIF.


  CALL FUNCTION 'SSF_CLOSE'
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_PRINT_RE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_print_re .



  DATA:lwa_ztfi_print_005 TYPE ztfi_print_002.
  DATA:lta_ztfi_print_005 TYPE TABLE OF ztfi_print_002.
  DATA wa_header TYPE zsfi_ssf_001.
  DATA lt_item TYPE TABLE OF zsfi_ssf_002 WITH HEADER LINE.
  DATA l_kunnr TYPE kunnr.
  DATA l_ts(4) TYPE c.
  DATA : lv_jexx TYPE string,
         lv_jedx TYPE string.
*  DATA l_ktext TYPE ktext_curt.
  DATA:l_txt20 TYPE txt20_skat.
  DATA:l_name1 TYPE name1_gp.
  DATA:l_post1 TYPE ps_post1.
  DATA:l_fkbtx TYPE fkbtx.

  DATA l_sum TYPE dmbtr.
  DATA l_num TYPE i.
  DATA l_zpage TYPE i.

  DATA:lwa_ztfi_print_002 TYPE ztfi_print_001.


  DATA: control    TYPE ssfctrlop,
        out_option TYPE ssfcompop,
        fm_name    TYPE rs38l_fnam.
  CLEAR:lwa_ztfi_print_005.
  "设置用户参数可用***
  out_option-tdimmed = 'X'.
  out_option-tdnewid = 'X'.
  out_option-tddelete = 'X'.
  out_option-tdfinal = 'X'.
  out_option-tdiexit = 'X'.
  out_option-tddest = 'LP01'.

  control-preview   = 'X'.
  control-no_open   = 'X'.
  control-no_close  = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
*     user_settings      = 'X'
      control_parameters = control
      output_options     = out_option
      user_settings      = ''
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

*
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZSF_FICO_001_PRINT'
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.


  DATA lt_print LIKE TABLE OF gt_display WITH HEADER LINE.
  DATA l_varkey TYPE vim_enqkey.
  DATA l_index TYPE sy-tabix.

******
  CLEAR lwa_ztfi_print_002.



  lt_print[] =  gt_display[].
  SORT lt_print BY bukrs gjahr monat blart lifnr sel DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_print COMPARING bukrs gjahr monat blart lifnr sel.

  LOOP AT lt_print WHERE sel = 'X'.
    l_index = sy-tabix.
    CLEAR:wa_header,lt_item[].
    "
    IF lt_print-zpzbh <> ''.
      wa_header-zpzbh = lt_print-zpzbh.
    ELSE.
      IF lwa_ztfi_print_002 IS INITIAL.
        CLEAR lwa_ztfi_print_005.
        SELECT SINGLE *
         INTO lwa_ztfi_print_005
         FROM ztfi_print_002
         WHERE bukrs = lt_print-bukrs
           AND gjahr = lt_print-gjahr
           AND belnr = lt_print-belnr
           AND zdylx = gv_dylx.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lwa_ztfi_print_005-zpzbh
            IMPORTING
              output = lwa_ztfi_print_005-zpzbh.
          lt_print-zpzbh = lwa_ztfi_print_005-zpzbh.
        ELSE.
          CLEAR lwa_ztfi_print_002.
          SELECT SINGLE *
           INTO lwa_ztfi_print_002
           FROM ztfi_print_001
           WHERE bukrs = lt_print-bukrs
             AND gjahr = lt_print-gjahr
             AND monat = lt_print-monat.
          IF sy-subrc EQ 0.
            CLEAR l_varkey.
            CONCATENATE lwa_ztfi_print_002-mandt lwa_ztfi_print_002-bukrs lwa_ztfi_print_002-gjahr lwa_ztfi_print_002-monat INTO l_varkey.
            CALL FUNCTION 'ENQUEUE_E_TABLE'
              EXPORTING
                mode_rstable   = 'E'
                tabname        = 'ZTFI_PRINT_001'
                varkey         = l_varkey
*               X_TABNAME      = ' '
*               X_VARKEY       = ' '
*               _SCOPE         = '2'
*               _WAIT          = ' '
*               _COLLECT       = ' '
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.
            IF sy-subrc <> 0.
*               Implement suitable error handling here
            ENDIF.
            lwa_ztfi_print_002-zpzbh = lwa_ztfi_print_002-zpzbh + 1.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lwa_ztfi_print_002-zpzbh
              IMPORTING
                output = lwa_ztfi_print_002-zpzbh.
            lt_print-zpzbh = lwa_ztfi_print_002-zpzbh.
          ELSE.
            lwa_ztfi_print_002-bukrs = lt_print-bukrs.
            lwa_ztfi_print_002-gjahr = lt_print-gjahr.
            lwa_ztfi_print_002-monat = lt_print-monat.
            lwa_ztfi_print_002-zpzbh = 1.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lwa_ztfi_print_002-zpzbh
              IMPORTING
                output = lwa_ztfi_print_002-zpzbh.
            lt_print-zpzbh = lwa_ztfi_print_002-zpzbh.
          ENDIF.
          CLEAR:lwa_ztfi_print_005.
          MOVE-CORRESPONDING lwa_ztfi_print_002 TO lwa_ztfi_print_005.
          lwa_ztfi_print_005-zdylx = gv_dylx.
          lwa_ztfi_print_005-zpzbh = lwa_ztfi_print_002-zpzbh.
          lwa_ztfi_print_005-belnr = lt_print-belnr.
          lwa_ztfi_print_005-bukrs = lt_print-bukrs.
          lwa_ztfi_print_005-gjahr = lt_print-gjahr.
          lwa_ztfi_print_005-zernam = sy-uname.
          lwa_ztfi_print_005-zerdat = sy-datum.
          lwa_ztfi_print_005-zertim  = sy-uzeit.
          MODIFY ztfi_print_002 FROM lwa_ztfi_print_005 .
        ENDIF.
      ELSE.
        IF lwa_ztfi_print_002-bukrs = lt_print-bukrs
          AND lwa_ztfi_print_002-gjahr = lt_print-gjahr
          AND lwa_ztfi_print_002-monat = lt_print-monat
          .
          lwa_ztfi_print_002-zpzbh = lwa_ztfi_print_002-zpzbh + 1.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lwa_ztfi_print_002-zpzbh
            IMPORTING
              output = lwa_ztfi_print_002-zpzbh.
          lt_print-zpzbh = lwa_ztfi_print_002-zpzbh.
        ELSE.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lwa_ztfi_print_002-zpzbh
            IMPORTING
              output = lwa_ztfi_print_002-zpzbh.
          MODIFY ztfi_print_001 FROM lwa_ztfi_print_002.
          IF sy-subrc EQ 0.
            COMMIT WORK.
          ENDIF.
          CLEAR l_varkey.
          CONCATENATE lwa_ztfi_print_002-mandt lwa_ztfi_print_002-bukrs lwa_ztfi_print_002-gjahr lwa_ztfi_print_002-monat INTO l_varkey.
          CALL FUNCTION 'DEQUEUE_E_TABLE'
            EXPORTING
              mode_rstable = 'E'
              tabname      = 'ZTFI_PRINT_001'
              varkey       = l_varkey
*             X_TABNAME    = ' '
*             X_VARKEY     = ' '
*             _SCOPE       = '3'
*             _SYNCHRON    = ' '
*             _COLLECT     = ' '
            .
          CLEAR lwa_ztfi_print_002.
          SELECT SINGLE *
           INTO lwa_ztfi_print_002
           FROM ztfi_print_001
           WHERE bukrs = lt_print-bukrs
             AND gjahr = lt_print-gjahr
          AND monat = lt_print-monat.
          IF sy-subrc EQ 0 .
            CLEAR l_varkey.
            CONCATENATE lwa_ztfi_print_002-mandt lwa_ztfi_print_002-bukrs lwa_ztfi_print_002-gjahr lwa_ztfi_print_002-monat INTO l_varkey.
            CALL FUNCTION 'ENQUEUE_E_TABLE'
              EXPORTING
                mode_rstable   = 'E'
                tabname        = 'ZTFI_PRINT_001'
                varkey         = l_varkey
*               X_TABNAME      = ' '
*               X_VARKEY       = ' '
*               _SCOPE         = '2'
*               _WAIT          = ' '
*               _COLLECT       = ' '
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.
            IF sy-subrc <> 0.
*               Implement suitable error handling here
            ENDIF.
            lwa_ztfi_print_002-zpzbh = lwa_ztfi_print_002-zpzbh + 1.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lwa_ztfi_print_002-zpzbh
              IMPORTING
                output = lwa_ztfi_print_002-zpzbh.
            lt_print-zpzbh = lwa_ztfi_print_002-zpzbh.
          ELSE.
            lwa_ztfi_print_002-bukrs = lt_print-bukrs.
            lwa_ztfi_print_002-gjahr = lt_print-gjahr.
            lwa_ztfi_print_002-monat = lt_print-monat.
            lwa_ztfi_print_002-zpzbh = 1.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lwa_ztfi_print_002-zpzbh
              IMPORTING
                output = lwa_ztfi_print_002-zpzbh.
            lt_print-zpzbh = lwa_ztfi_print_002-zpzbh.
          ENDIF.
        ENDIF.
        CLEAR:lwa_ztfi_print_005 .
        MOVE-CORRESPONDING lwa_ztfi_print_002 TO lwa_ztfi_print_005.
        lwa_ztfi_print_005-zdylx = gv_dylx.
        lwa_ztfi_print_005-zpzbh = lwa_ztfi_print_002-zpzbh.
        lwa_ztfi_print_005-belnr = lt_print-belnr.
        lwa_ztfi_print_005-bukrs = lt_print-bukrs.
        lwa_ztfi_print_005-gjahr = lt_print-gjahr.
        lwa_ztfi_print_005-zernam = sy-uname.
        lwa_ztfi_print_005-zerdat = sy-datum.
        lwa_ztfi_print_005-zertim  = sy-uzeit.
        MODIFY ztfi_print_002 FROM lwa_ztfi_print_005.

      ENDIF.
      wa_header-zpzbh = lt_print-zpzbh.
    ENDIF.


*    wa_header-zpzbh = lt_print-belnr.

**公司代码描述 制单人 币种 单位
    SELECT SINGLE butxt      INTO wa_header-butxt  FROM t001      WHERE bukrs = gt_display-bukrs.
    SELECT SINGLE name_textc INTO wa_header-zzdr   FROM user_addr WHERE bname = sy-uname.
    SELECT SINGLE ktext INTO wa_header-zbz FROM tcurt WHERE spras = '1' AND waers = gt_display-waers.
    wa_header-zdw = '元'.

****记账日期
    READ TABLE s_budat INDEX 1.
    CONCATENATE s_budat-low '-' s_budat-high INTO wa_header-zjzrq.
*    CONCATENATE s_budat-low '-' s_budat-high INTO wa_header-zjzrq.

    wa_header-blart = lt_print-blart.
    CONCATENATE '凭证类型：' wa_header-blart INTO wa_header-blart.

**凭证类型描述
    SELECT SINGLE ltext INTO wa_header-zpzlx FROM t003t WHERE spras = '1' AND blart = lt_print-blart.


    l_sum = 0.
    LOOP AT gt_display WHERE bukrs = lt_print-bukrs AND gjahr = lt_print-gjahr AND lifnr = lt_print-lifnr.
      LOOP AT gt_bseg WHERE bukrs = gt_display-bukrs AND gjahr = gt_display-gjahr AND belnr = gt_display-belnr.

        CLEAR lt_item.

        lt_item-zkjkm = gt_bseg-hkont.

        "借方金额 和 贷方金额
        IF gt_bseg-shkzg = 'S'.

          IF gt_bseg-xnegp = ''.
            lt_item-zjfje = gt_bseg-dmbtr.
            l_sum = l_sum + gt_bseg-dmbtr.
          ELSE.
            lt_item-zdfje = 0 - gt_bseg-dmbtr.
            l_sum = l_sum - gt_bseg-dmbtr.
          ENDIF.

        ELSE.
          IF gt_bseg-xnegp = ''.
            lt_item-zdfje = gt_bseg-dmbtr.
          ELSE.
            lt_item-zjfje = 0 - gt_bseg-dmbtr.
          ENDIF.
        ENDIF.
        COLLECT lt_item.
      ENDLOOP.
      gt_display-zpzbh = lt_print-zpzbh.
      MODIFY gt_display.
    ENDLOOP.

    SORT lt_item BY zkjkm .

    CLEAR l_name1.
    SELECT SINGLE name1 FROM lfa1 INTO l_name1 WHERE lifnr = lt_print-lifnr.

    l_num = 0."""计算行数
    LOOP AT lt_item.
      l_num = l_num + 1."计数器

      "科目描述
      CLEAR l_txt20.
      SELECT SINGLE txt20 INTO l_txt20 FROM skat WHERE spras = '1' AND ktopl = 'CXZX' AND saknr = lt_item-zkjkm.
      IF sy-subrc EQ 0.
*      CONCATENATE lt_item-zkjkm  ' '  l_txt20 INTO lt_item-zkjkm.
        lt_item-zkjkm = l_txt20.
      ENDIF.

      CONCATENATE l_name1 '采购发票汇总' INTO lt_item-sgtxt.
*      lt_item-SGTXT =
      MODIFY lt_item.
    ENDLOOP.



    "总页数
    l_zpage = l_num DIV 6  + 1.
    wa_header-zpage = l_zpage.
    CONDENSE wa_header-zpage NO-GAPS.

    CLEAR:lt_item.
***合计金额及大小写转换
    IF l_sum < 0.
      l_sum = 0 - l_sum.
    ENDIF.
    lv_jexx = l_sum.
    CALL FUNCTION 'ZFI_MONEY_LOWER_TO_UPPER'
      EXPORTING
        moneyin  = lv_jexx
      IMPORTING
        moneyout = lv_jedx.

    CONCATENATE '合计：' lv_jedx INTO lt_item-zkjkm.
    lt_item-zjfje = l_sum.
    lt_item-zdfje = l_sum.
    SORT lt_item BY zjfje DESCENDING.
    APPEND lt_item.



    CALL FUNCTION fm_name
      EXPORTING
        control_parameters = control
        output_option      = out_option
        i_header           = wa_header
      TABLES
        t_item             = lt_item[]
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDLOOP.



  IF lwa_ztfi_print_002 IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_ztfi_print_002-zpzbh
      IMPORTING
        output = lwa_ztfi_print_002-zpzbh.
    MODIFY ztfi_print_001 FROM lwa_ztfi_print_002.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
    CLEAR l_varkey.
    CONCATENATE lwa_ztfi_print_002-mandt lwa_ztfi_print_002-bukrs lwa_ztfi_print_002-gjahr lwa_ztfi_print_002-monat INTO l_varkey.
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable = 'E'
        tabname      = 'ZTFI_PRINT_001'
        varkey       = l_varkey
*       X_TABNAME    = ' '
*       X_VARKEY     = ' '
*       _SCOPE       = '3'
*       _SYNCHRON    = ' '
*       _COLLECT     = ' '
      .
    CLEAR lwa_ztfi_print_002.
  ENDIF.


  CALL FUNCTION 'SSF_CLOSE'
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_PRINT_YS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_print_ys .


  DATA wa_header TYPE zsfi_ssf_001.
  DATA lt_item TYPE TABLE OF zsfi_ssf_002 WITH HEADER LINE.
  DATA l_kunnr TYPE kunnr.
  DATA l_ts(4) TYPE c.
  DATA : lv_jexx TYPE string,
         lv_jedx TYPE string.
*  DATA l_ktext TYPE ktext_curt.
  DATA:l_txt20 TYPE txt20_skat.
  DATA:l_name1 TYPE name1_gp.
  DATA:l_post1 TYPE ps_post1.
  DATA:l_fkbtx TYPE fkbtx.

  DATA l_sum TYPE dmbtr.
  DATA l_num TYPE i.
  DATA l_zpage TYPE i.

  DATA:lwa_ztfi_print_002 TYPE ztfi_print_001.
  DATA:lwa_ztfi_print_005 TYPE ztfi_print_002.


  DATA: control    TYPE ssfctrlop,
        out_option TYPE ssfcompop,
        fm_name    TYPE rs38l_fnam.

  "设置用户参数可用***
  out_option-tdimmed = 'X'.
  out_option-tdnewid = 'X'.
  out_option-tddelete = 'X'.
  out_option-tdfinal = 'X'.
  out_option-tdiexit = 'X'.
  out_option-tddest = 'LP01'.

  control-preview   = 'X'.
  control-no_open   = 'X'.
  control-no_close  = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
*     user_settings      = 'X'
      control_parameters = control
      output_options     = out_option
      user_settings      = ''
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

*
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZSF_FICO_001_PRINT'
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.


  DATA lt_print LIKE TABLE OF gt_display WITH HEADER LINE.
  DATA lt_print2 LIKE TABLE OF gt_display WITH HEADER LINE.
  DATA l_varkey TYPE vim_enqkey.
  DATA l_index TYPE sy-tabix.

  DATA:l_budat TYPE budat,
       l_hkont TYPE hkont,
       l_zpzbh TYPE zzfipzbh.

******
  CLEAR lwa_ztfi_print_002.

  SORT gt_display BY bukrs gjahr budat hkont zpzbh DESCENDING.


**************先打印已编号凭证
  CLEAR:l_budat,l_hkont,l_num,l_zpzbh.
  LOOP AT gt_display WHERE zpzbh <> ''.
    "记录第一条数据的过账日期和银行账号
    IF sy-tabix = 1.
      l_zpzbh = gt_display-zpzbh.
    ENDIF.
    "已有编号的，直接打印
    IF l_zpzbh = gt_display-zpzbh.
      CLEAR lt_print.
      MOVE-CORRESPONDING gt_display TO lt_print.
      APPEND lt_print.
    ELSE.

      IF lt_print[] IS NOT  INITIAL.
        PERFORM frm_print_yh TABLES lt_print[] USING fm_name.
        CLEAR lt_print[].
      ENDIF.

      l_zpzbh = gt_display-zpzbh.
      CLEAR:lt_print,lt_print[].
      MOVE-CORRESPONDING gt_display TO lt_print.
      APPEND lt_print.
    ENDIF.



  ENDLOOP.
****将剩余未打印的凭证最后一次打印

  IF lt_print[] IS NOT INITIAL.
    PERFORM frm_print_yh TABLES lt_print[] USING fm_name.
    CLEAR lt_print[].
  ENDIF.

*******打印未编号凭证 非票据
  CLEAR:l_budat,l_hkont,l_num,l_zpzbh.
  DATA: BEGIN OF lt_bank OCCURS 0,
          hkont TYPE bseg-hkont,
        END OF lt_bank.
  CLEAR lt_print[].
  LOOP AT gt_display WHERE zpzbh = '' AND hkont <> '1121010000'.
    "记录第一条数据的过账日期和银行账号

    IF l_budat IS INITIAL AND l_hkont = ''.
      l_budat = gt_display-budat.
      l_hkont = gt_display-hkont.
    ENDIF.
    "没有编号的，按照编号排序打印
*    IF gt_display-hkont <> '1121010000'.
    IF l_budat = gt_display-budat AND l_hkont = gt_display-hkont AND l_num < 70.
      l_num = l_num + 1.
      CLEAR lt_print.
      MOVE-CORRESPONDING gt_display TO lt_print.
      APPEND lt_print.
    ELSE.
*      IF gt_display-budat-hkont <> '1121010000'.
      "当银行账号不同或者同一账号超过70条时，先打印前70，后续凭证每70条单独打印
      IF lt_print[] IS NOT  INITIAL.
        PERFORM frm_print_yh2 TABLES lt_print[] USING fm_name.
        CLEAR lt_print[].
        "打印完后清除
      ENDIF.
*      记录新的过账日期和银行科目
      l_budat = gt_display-budat.
      l_hkont = gt_display-hkont.

      "重新累计
      l_num = 1.
      CLEAR:lt_print,lt_print[].
      MOVE-CORRESPONDING gt_display TO lt_print.
      APPEND lt_print.
    ENDIF.

  ENDLOOP.
  "因为每次都清除已打印，若还有剩余则一次打印完
  IF lt_print[] IS NOT INITIAL.
    PERFORM frm_print_yh2 TABLES lt_print[] USING fm_name.
    CLEAR lt_print[].
  ENDIF.

  "begin change by ibm-rkh
  CLEAR lt_print[].
  "end change by ibm-rkh
*******打印未编号凭证 票据
  CLEAR:l_budat,l_hkont,l_num,l_zpzbh.
  LOOP AT gt_display WHERE zpzbh = '' AND hkont = '1121010000'.
    "重新累计
    CLEAR:lt_print.
    MOVE-CORRESPONDING gt_display TO lt_print.
    APPEND lt_print.

****将剩余未打印的凭证最后一次打印
    AT LAST.
      IF lt_print[] IS NOT INITIAL.
        PERFORM frm_print_yh2 TABLES lt_print[] USING fm_name.

        "begin change by ibm-rkh
        CLEAR lt_print[].
        "end change by ibm-rkh
      ENDIF.
    ENDAT.
  ENDLOOP.


  IF lwa_ztfi_print_002 IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_ztfi_print_002-zpzbh
      IMPORTING
        output = lwa_ztfi_print_002-zpzbh.

    MODIFY ztfi_print_001 FROM lwa_ztfi_print_002.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
    CLEAR l_varkey.
    CONCATENATE lwa_ztfi_print_002-mandt lwa_ztfi_print_002-bukrs lwa_ztfi_print_002-gjahr lwa_ztfi_print_002-monat INTO l_varkey.
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable = 'E'
        tabname      = 'ZTFI_PRINT_001'
        varkey       = l_varkey
*       X_TABNAME    = ' '
*       X_VARKEY     = ' '
*       _SCOPE       = '3'
*       _SYNCHRON    = ' '
*       _COLLECT     = ' '
      .
    CLEAR lwa_ztfi_print_002.
  ENDIF.


  CALL FUNCTION 'SSF_CLOSE'
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  IF gt_ztfi_print_005[] IS NOT INITIAL.
    SORT gt_ztfi_print_005 BY bukrs gjahr belnr.
    LOOP AT gt_display.
      READ TABLE gt_ztfi_print_005 WITH KEY bukrs = gt_display-bukrs
                                            gjahr = gt_display-gjahr
                                            belnr = gt_display-belnr.
      IF sy-subrc EQ 0.
        gt_display-zpzbh = gt_ztfi_print_005-zpzbh.
        IF gt_display-zpzbh <>  ''.
          gt_display-print_flag = 'X'.
        ENDIF.
        MODIFY gt_display.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_PRINT_YH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_PRINT[]
*&---------------------------------------------------------------------*
FORM frm_print_yh  TABLES   pt_print STRUCTURE gt_display
                    USING fm_name    TYPE rs38l_fnam..

  DATA wa_header TYPE zsfi_ssf_001.
  DATA lt_item TYPE TABLE OF zsfi_ssf_002 WITH HEADER LINE.
  DATA l_kunnr TYPE kunnr.
  DATA l_ts(4) TYPE c.
  DATA : lv_jexx TYPE string,
         lv_jedx TYPE string.
*  DATA l_ktext TYPE ktext_curt.
  DATA:l_txt20 TYPE txt20_skat.
  DATA:l_name1 TYPE name1_gp.
  DATA:l_post1 TYPE ps_post1.
  DATA:l_fkbtx TYPE fkbtx.
  DATA l_sum TYPE dmbtr.
  DATA l_num TYPE i.
  DATA l_zpage TYPE i.

  DATA l_varkey TYPE vim_enqkey.
  DATA: control    TYPE ssfctrlop,
        out_option TYPE ssfcompop.

  "设置用户参数可用***
  out_option-tdimmed = 'X'.
  out_option-tdnewid = 'X'.
  out_option-tddelete = 'X'.
  out_option-tdfinal = 'X'.
  out_option-tdiexit = 'X'.
  out_option-tddest = 'LP01'.

  control-preview   = 'X'.
  control-no_open   = 'X'.
  control-no_close  = 'X'.


*  DATA:l_name1 TYPE name1_gp.
  DATA:lwa_ztfi_print_002 TYPE ztfi_print_001.
  DATA:lwa_ztfi_print_005 TYPE ztfi_print_002.
  DATA:lt_ztfi_print_005 TYPE TABLE OF ztfi_print_002 WITH HEADER LINE.

***凭证号排序
  SORT pt_print BY belnr.
  "最小凭证号
  READ TABLE pt_print INDEX 1.
  IF sy-subrc EQ 0.
    IF pt_print-zpzbh <> ''.
      wa_header-zpzbh = pt_print-zpzbh.
      CONDENSE wa_header-zpzbh NO-GAPS.
    ELSE.
      CLEAR lwa_ztfi_print_005.
      SELECT SINGLE *
       INTO lwa_ztfi_print_005
       FROM ztfi_print_002
       WHERE bukrs = pt_print-bukrs
         AND gjahr = pt_print-gjahr
      AND belnr = pt_print-belnr.
      IF sy-subrc EQ 0.
        pt_print-zpzbh = lwa_ztfi_print_005-zpzbh.
      ELSE.
        CLEAR lwa_ztfi_print_002.
        SELECT SINGLE *
         INTO lwa_ztfi_print_002
         FROM ztfi_print_001
         WHERE bukrs = pt_print-bukrs
           AND gjahr = pt_print-gjahr
        AND monat = pt_print-monat.
        IF sy-subrc EQ 0.
          CLEAR l_varkey.
          CONCATENATE lwa_ztfi_print_002-mandt lwa_ztfi_print_002-bukrs lwa_ztfi_print_002-gjahr lwa_ztfi_print_002-monat INTO l_varkey.
          CALL FUNCTION 'ENQUEUE_E_TABLE'
            EXPORTING
              mode_rstable   = 'E'
              tabname        = 'ZTFI_PRINT_001'
              varkey         = l_varkey
*             X_TABNAME      = ' '
*             X_VARKEY       = ' '
*             _SCOPE         = '2'
*             _WAIT          = ' '
*             _COLLECT       = ' '
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc <> 0.
*               Implement suitable error handling here
          ENDIF.
          lwa_ztfi_print_002-zpzbh = lwa_ztfi_print_002-zpzbh + 1.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lwa_ztfi_print_002-zpzbh
            IMPORTING
              output = lwa_ztfi_print_002-zpzbh.
          pt_print-zpzbh = lwa_ztfi_print_002-zpzbh.
        ELSE.
          lwa_ztfi_print_002-bukrs = pt_print-bukrs.
          lwa_ztfi_print_002-gjahr = pt_print-gjahr.
          lwa_ztfi_print_002-monat = pt_print-monat.
          lwa_ztfi_print_002-zpzbh = 1.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lwa_ztfi_print_002-zpzbh
            IMPORTING
              output = lwa_ztfi_print_002-zpzbh.
          pt_print-zpzbh = lwa_ztfi_print_002-zpzbh.
        ENDIF.
        IF lwa_ztfi_print_002 IS NOT INITIAL.

          MODIFY ztfi_print_001 FROM lwa_ztfi_print_002.
          IF sy-subrc EQ 0.
            COMMIT WORK.
          ENDIF.
          CLEAR l_varkey.
          CONCATENATE lwa_ztfi_print_002-mandt lwa_ztfi_print_002-bukrs lwa_ztfi_print_002-gjahr lwa_ztfi_print_002-monat INTO l_varkey.
          CALL FUNCTION 'DEQUEUE_E_TABLE'
            EXPORTING
              mode_rstable = 'E'
              tabname      = 'ZTFI_PRINT_001'
              varkey       = l_varkey
*             X_TABNAME    = ' '
*             X_VARKEY     = ' '
*             _SCOPE       = '3'
*             _SYNCHRON    = ' '
*             _COLLECT     = ' '
            .
          CLEAR lwa_ztfi_print_002.
        ENDIF.
*          MOVE-CORRESPONDING lwa_ztfi_print_002 TO lwa_ZTFI_PRINT_005.
*          lwa_ztfi_PRINT_005-blart = lt_print-blart.
*          lwa_ztfi_PRINT_005-lifnr = lt_print-lifnr.
*          lwa_ztfi_PRINT_005-belnr = lt_print-belnr.
*          MODIFY ZTFI_PRINT_002 FROM lwa_ztfi_PRINT_005.
      ENDIF.
    ENDIF.
    wa_header-zpzbh = pt_print-zpzbh.
  ENDIF.

**公司代码描述 制单人 币种 单位
  SELECT SINGLE butxt      INTO wa_header-butxt  FROM t001      WHERE bukrs = pt_print-bukrs.
  SELECT SINGLE name_textc INTO wa_header-zzdr   FROM user_addr WHERE bname = sy-uname.
  SELECT SINGLE ktext INTO wa_header-zbz FROM tcurt WHERE spras = '1' AND waers = pt_print-waers.
  wa_header-zdw = '元'.


  "最大凭证号
*  l_num = 0.
*  l_num = lines( pt_print[] ).
*  IF l_num > 1.
*    READ TABLE pt_print INDEX l_num.
*    IF sy-subrc EQ 0.
*      CONCATENATE wa_header-zpzbh '-' pt_print-belnr INTO wa_header-zpzbh.
*    ENDIF.
*  ENDIF.
****记账日期
  READ TABLE s_budat INDEX 1.
  CONCATENATE s_budat-low '-' s_budat-high INTO wa_header-zjzrq.

  wa_header-blart = pt_print-blart.
  CONCATENATE '凭证类型：' wa_header-blart INTO wa_header-blart.

**凭证类型描述
  SELECT SINGLE ltext INTO wa_header-zpzlx FROM t003t WHERE spras = '1' AND blart = pt_print-blart.


  l_sum = 0.
  LOOP AT pt_print.
    LOOP AT gt_bseg WHERE bukrs = pt_print-bukrs AND gjahr = pt_print-gjahr AND belnr = pt_print-belnr.
      CLEAR lt_item.
      "摘要
      lt_item-zkjkm = gt_bseg-hkont.

      "借方金额 和 贷方金额
      IF gt_bseg-shkzg = 'S'.
        IF gt_bseg-xnegp = ''.
          lt_item-zjfje = gt_bseg-dmbtr.
          l_sum = l_sum + gt_bseg-dmbtr.
        ELSE.
          lt_item-zdfje = 0 - gt_bseg-dmbtr.
          l_sum = l_sum - gt_bseg-dmbtr.
        ENDIF.
      ELSE.
        IF gt_bseg-xnegp = ''.
          lt_item-zdfje = gt_bseg-dmbtr.
        ELSE.
          lt_item-zjfje = 0 - gt_bseg-dmbtr.
        ENDIF.
      ENDIF.
      CLEAR:lwa_ztfi_print_005.
      MOVE-CORRESPONDING pt_print TO lwa_ztfi_print_005.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lwa_ztfi_print_005-zpzbh
        IMPORTING
          output = lwa_ztfi_print_005-zpzbh.
      lwa_ztfi_print_005-zdylx = gv_dylx.
      lwa_ztfi_print_005-belnr = pt_print-belnr.
      lwa_ztfi_print_005-bukrs = pt_print-bukrs.
      lwa_ztfi_print_005-gjahr = pt_print-gjahr.
      lwa_ztfi_print_005-zernam = sy-uname.
      lwa_ztfi_print_005-zerdat = sy-datum.
      lwa_ztfi_print_005-zertim  = sy-uzeit.
      APPEND lwa_ztfi_print_005 TO lt_ztfi_print_005.
      APPEND lwa_ztfi_print_005 TO gt_ztfi_print_005.
      COLLECT lt_item.

    ENDLOOP.
*    pt_print-zpzbh = wa_header-zpzbh.
  ENDLOOP.

  IF lt_ztfi_print_005[] IS NOT INITIAL.
    MODIFY ztfi_print_002 FROM TABLE lt_ztfi_print_005[].
    COMMIT WORK.
  ENDIF.
****取科目描述
  SORT lt_item BY zkjkm.
  l_num = 0.
  LOOP AT lt_item.
    "科目描述
    CLEAR l_txt20.
    SELECT SINGLE txt20 INTO l_txt20 FROM skat WHERE spras = '1' AND ktopl = 'CXZX' AND saknr = lt_item-zkjkm.
    IF sy-subrc EQ 0.
*      CONCATENATE lt_item-zkjkm  ' '  l_txt20 INTO lt_item-zkjkm.
      lt_item-zkjkm = l_txt20.
    ENDIF.

    lt_item-sgtxt = '收款凭证'.

    MODIFY lt_item.
    l_num = l_num + 1."计数器
  ENDLOOP.

  "总页数
  l_zpage = l_num DIV 6  + 1.
  wa_header-zpage = l_zpage.
  CONDENSE wa_header-zpage NO-GAPS.

  CLEAR:lt_item.
***合计金额及大小写转换
  IF l_sum < 0.
    l_sum = 0 - l_sum.
  ENDIF.
  lv_jexx = l_sum.
  CALL FUNCTION 'ZFI_MONEY_LOWER_TO_UPPER'
    EXPORTING
      moneyin  = lv_jexx
    IMPORTING
      moneyout = lv_jedx.

  CONCATENATE '合计：' lv_jedx INTO lt_item-zkjkm.
  lt_item-zjfje = l_sum.
  lt_item-zdfje = l_sum.
  SORT lt_item BY zjfje DESCENDING.
  APPEND lt_item.



  CALL FUNCTION fm_name
    EXPORTING
      control_parameters = control
      output_option      = out_option
      i_header           = wa_header
    TABLES
      t_item             = lt_item[]
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form FRM_PRINT_YH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_PRINT[]
*&---------------------------------------------------------------------*
FORM frm_print_yh2  TABLES   pt_print STRUCTURE gt_display
                    USING fm_name    TYPE rs38l_fnam..

  DATA wa_header TYPE zsfi_ssf_001.
  DATA lt_item TYPE TABLE OF zsfi_ssf_002 WITH HEADER LINE.
  DATA l_kunnr TYPE kunnr.
  DATA l_ts(4) TYPE c.
  DATA : lv_jexx TYPE string,
         lv_jedx TYPE string.
*  DATA l_ktext TYPE ktext_curt.
  DATA:l_txt20 TYPE txt20_skat.
  DATA:l_name1 TYPE name1_gp.
  DATA:l_post1 TYPE ps_post1.
  DATA:l_fkbtx TYPE fkbtx.
  DATA l_sum TYPE dmbtr.
  DATA l_num TYPE i.
  DATA l_zpage TYPE i.


  DATA: control    TYPE ssfctrlop,
        out_option TYPE ssfcompop.

  DATA l_varkey TYPE vim_enqkey.

  "设置用户参数可用***
  out_option-tdimmed = 'X'.
  out_option-tdnewid = 'X'.
  out_option-tddelete = 'X'.
  out_option-tdfinal = 'X'.
  out_option-tdiexit = 'X'.
  out_option-tddest = 'LP01'.

  control-preview   = 'X'.
  control-no_open   = 'X'.
  control-no_close  = 'X'.


*  DATA:l_name1 TYPE name1_gp.
  DATA:lwa_ztfi_print_002 TYPE ztfi_print_001.
  DATA:lwa_ztfi_print_005 TYPE ztfi_print_002.
  DATA:lt_ztfi_print_005 TYPE TABLE OF ztfi_print_002 WITH HEADER LINE.


***凭证号排序
  SORT pt_print BY belnr.
  "最小凭证号
  READ TABLE pt_print INDEX 1.
  IF sy-subrc EQ 0.
    IF pt_print-zpzbh <> ''.
      wa_header-zpzbh = pt_print-zpzbh.
      CONDENSE wa_header-zpzbh NO-GAPS.
    ELSE.
      CLEAR lwa_ztfi_print_005.
      SELECT SINGLE *
       INTO lwa_ztfi_print_005
       FROM ztfi_print_002
       WHERE bukrs = pt_print-bukrs
         AND gjahr = pt_print-gjahr
      AND belnr = pt_print-belnr.
      IF sy-subrc EQ 0.
        pt_print-zpzbh = lwa_ztfi_print_005-zpzbh.
      ELSE.
        CLEAR lwa_ztfi_print_002.
        SELECT SINGLE *
         INTO lwa_ztfi_print_002
         FROM ztfi_print_001
         WHERE bukrs = pt_print-bukrs
           AND gjahr = pt_print-gjahr
        AND monat = pt_print-monat.
        IF sy-subrc EQ 0.
          CLEAR l_varkey.
          CONCATENATE lwa_ztfi_print_002-mandt lwa_ztfi_print_002-bukrs lwa_ztfi_print_002-gjahr lwa_ztfi_print_002-monat INTO l_varkey.
          CALL FUNCTION 'ENQUEUE_E_TABLE'
            EXPORTING
              mode_rstable   = 'E'
              tabname        = 'ZTFI_PRINT_001'
              varkey         = l_varkey
*             X_TABNAME      = ' '
*             X_VARKEY       = ' '
*             _SCOPE         = '2'
*             _WAIT          = ' '
*             _COLLECT       = ' '
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc <> 0.
*               Implement suitable error handling here
          ENDIF.
          lwa_ztfi_print_002-zpzbh = lwa_ztfi_print_002-zpzbh + 1.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lwa_ztfi_print_002-zpzbh
            IMPORTING
              output = lwa_ztfi_print_002-zpzbh.
          pt_print-zpzbh = lwa_ztfi_print_002-zpzbh.
        ELSE.
          lwa_ztfi_print_002-bukrs = pt_print-bukrs.
          lwa_ztfi_print_002-gjahr = pt_print-gjahr.
          lwa_ztfi_print_002-monat = pt_print-monat.
          lwa_ztfi_print_002-zpzbh = 1.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lwa_ztfi_print_002-zpzbh
            IMPORTING
              output = lwa_ztfi_print_002-zpzbh.
          pt_print-zpzbh = lwa_ztfi_print_002-zpzbh.
        ENDIF.
        IF lwa_ztfi_print_002 IS NOT INITIAL.

          MODIFY ztfi_print_001 FROM lwa_ztfi_print_002.
          IF sy-subrc EQ 0.
            COMMIT WORK.
          ENDIF.
          CLEAR l_varkey.
          CONCATENATE lwa_ztfi_print_002-mandt lwa_ztfi_print_002-bukrs lwa_ztfi_print_002-gjahr lwa_ztfi_print_002-monat INTO l_varkey.
          CALL FUNCTION 'DEQUEUE_E_TABLE'
            EXPORTING
              mode_rstable = 'E'
              tabname      = 'ZTFI_PRINT_001'
              varkey       = l_varkey
*             X_TABNAME    = ' '
*             X_VARKEY     = ' '
*             _SCOPE       = '3'
*             _SYNCHRON    = ' '
*             _COLLECT     = ' '
            .
          CLEAR lwa_ztfi_print_002.
        ENDIF.
*          MOVE-CORRESPONDING lwa_ztfi_print_002 TO lwa_ZTFI_PRINT_005.
*          lwa_ztfi_PRINT_005-blart = lt_print-blart.
*          lwa_ztfi_PRINT_005-lifnr = lt_print-lifnr.
*          lwa_ztfi_PRINT_005-belnr = lt_print-belnr.
*          MODIFY ZTFI_PRINT_002 FROM lwa_ztfi_PRINT_005.
      ENDIF.
    ENDIF.
    wa_header-zpzbh = pt_print-zpzbh.
  ENDIF.

**公司代码描述 制单人 币种 单位
  SELECT SINGLE butxt      INTO wa_header-butxt  FROM t001      WHERE bukrs = pt_print-bukrs.
  SELECT SINGLE name_textc INTO wa_header-zzdr   FROM user_addr WHERE bname = sy-uname.
  SELECT SINGLE ktext INTO wa_header-zbz FROM tcurt WHERE spras = '1' AND waers = pt_print-waers.
  wa_header-zdw = '元'.


  "最大凭证号
*  l_num = 0.
*  l_num = lines( pt_print[] ).
*  IF l_num > 1.
*    READ TABLE pt_print INDEX l_num.
*    IF sy-subrc EQ 0.
*      CONCATENATE wa_header-zpzbh '-' pt_print-belnr INTO wa_header-zpzbh.
*    ENDIF.
*  ENDIF.
****记账日期
  READ TABLE s_budat INDEX 1.
*  CONCATENATE s_budat-low '-' s_budat-high INTO wa_header-zjzrq.
  wa_header-zjzrq = s_budat-low .

  wa_header-blart = pt_print-blart.
  CONCATENATE '凭证类型：' wa_header-blart INTO wa_header-blart.

**凭证类型描述
  SELECT SINGLE ltext INTO wa_header-zpzlx FROM t003t WHERE spras = '1' AND blart = pt_print-blart.


  l_sum = 0.
  CLEAR lt_item[].
  LOOP AT pt_print.
    LOOP AT gt_bseg WHERE bukrs = pt_print-bukrs AND gjahr = pt_print-gjahr AND belnr = pt_print-belnr.
      CLEAR lt_item.
      "摘要
      lt_item-zkjkm = gt_bseg-hkont.

      "借方金额 和 贷方金额
      IF gt_bseg-shkzg = 'S'.
        IF gt_bseg-xnegp = ''.
          lt_item-zjfje = gt_bseg-dmbtr.
          l_sum = l_sum + gt_bseg-dmbtr.
        ELSE.
          lt_item-zdfje = 0 - gt_bseg-dmbtr.
          l_sum = l_sum - gt_bseg-dmbtr.
        ENDIF.
      ELSE.
        IF gt_bseg-xnegp = ''.
          lt_item-zdfje = gt_bseg-dmbtr.
        ELSE.
          lt_item-zjfje = 0 - gt_bseg-dmbtr.
        ENDIF.
      ENDIF.

      COLLECT lt_item.

    ENDLOOP.
    pt_print-zpzbh = wa_header-zpzbh.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = pt_print-zpzbh
      IMPORTING
        output = pt_print-zpzbh.

    MODIFY pt_print TRANSPORTING zpzbh.
  ENDLOOP.


****取科目描述
  SORT lt_item BY zkjkm.
  l_num = 0.
  LOOP AT lt_item.
    "科目描述
    CLEAR l_txt20.
    SELECT SINGLE txt20 INTO l_txt20 FROM skat WHERE spras = '1' AND ktopl = 'CXZX' AND saknr = lt_item-zkjkm.
    IF sy-subrc EQ 0.
*      CONCATENATE lt_item-zkjkm  ' '  l_txt20 INTO lt_item-zkjkm.
      lt_item-zkjkm = l_txt20.
    ENDIF.

    lt_item-sgtxt = '收款凭证'.

    MODIFY lt_item.
    l_num = l_num + 1."计数器
  ENDLOOP.

  "总页数
  l_zpage = l_num DIV 6  + 1.
  wa_header-zpage = l_zpage.
  CONDENSE wa_header-zpage NO-GAPS.

  CLEAR:lt_item.
***合计金额及大小写转换
  IF l_sum < 0.
    l_sum = 0 - l_sum.
  ENDIF.
  lv_jexx = l_sum.
  CALL FUNCTION 'ZFI_MONEY_LOWER_TO_UPPER'
    EXPORTING
      moneyin  = lv_jexx
    IMPORTING
      moneyout = lv_jedx.

  CONCATENATE '合计：' lv_jedx INTO lt_item-zkjkm.
  lt_item-zjfje = l_sum.
  lt_item-zdfje = l_sum.
  SORT lt_item BY zjfje DESCENDING.
  APPEND lt_item.

  CLEAR:gt_display2[],lt_ztfi_print_005[].
  LOOP AT pt_print.
    pt_print-zpzbh = wa_header-zpzbh.
    pt_print-print_flag = 'X'.
    MODIFY pt_print TRANSPORTING zpzbh.

    CLEAR:lwa_ztfi_print_005.
    MOVE-CORRESPONDING pt_print TO lwa_ztfi_print_005.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_ztfi_print_005-zpzbh
      IMPORTING
        output = lwa_ztfi_print_005-zpzbh.
    lwa_ztfi_print_005-zdylx = gv_dylx.
    lwa_ztfi_print_005-belnr = pt_print-belnr.
    lwa_ztfi_print_005-bukrs = pt_print-bukrs.
    lwa_ztfi_print_005-gjahr = pt_print-gjahr.
    lwa_ztfi_print_005-zernam = sy-uname.
    lwa_ztfi_print_005-zerdat = sy-datum.
    lwa_ztfi_print_005-zertim  = sy-uzeit.
    APPEND lwa_ztfi_print_005 TO lt_ztfi_print_005.
    APPEND lwa_ztfi_print_005 TO gt_ztfi_print_005.
  ENDLOOP.

*
  IF lt_ztfi_print_005[] IS NOT INITIAL.
    MODIFY ztfi_print_002 FROM TABLE lt_ztfi_print_005[].
    COMMIT WORK.
  ENDIF.



  DATA: option TYPE ssfcresop."控制参数
  CLEAR option.

  CALL FUNCTION fm_name
    EXPORTING
      control_parameters = control
      output_option      = out_option
      i_header           = wa_header
      job_output_options = option "输出参数
    TABLES
      t_item             = lt_item[]
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  IF option-tdpreview = 'X'.

  ENDIF.

ENDFORM.

*GUI Texts
*----------------------------------------------------------
* STANDARD --> 会计凭证打印


*Messages
*----------------------------------------------------------
*
* Message class: Hard coded
*   请输入公司代码

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
