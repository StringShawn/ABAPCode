*$*$********************************************************************
* Program ID/Name: ZFIR029                Date written: 2013/10/09
* Author's name:   HP_TC                  Last update:
* Program title: 成本中心预算和实际费用比较报表
* Project Name:  EPR I
* Version:
* Function Spec ID:
*----------------------------------------------------------------------*
* Description: (Incl. Related Function Area and System)
*  成本中心预算和实际费用比较报表
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
*
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

REPORT ZFIR029.

*$*$********************************************************************
*$*$    TABLES                                                         *
*$*$********************************************************************
TABLES:CSKS,RGSB4.

*$*$********************************************************************
*$*$    INCLUDES                                                       *
*$*$    (FOR INCLUDES WITH DECLARATIVE PURPOSES ONLY                   *
*$*$     I.E. BDC, ALV, ETC.)                                          *
*$*$********************************************************************


*$*$********************************************************************
*$*$    GLOBAL TYPES                                                   *
*$*$********************************************************************
TYPES:
  BEGIN OF T_COND,
    COND(72) TYPE C,
  END OF T_COND,

  BEGIN OF T_DATA1,
    ABTEI     TYPE CSKS-ABTEI,    "部门代码
    KOSTL     TYPE CSKS-KOSTL,    "成本中心
    KSTAR     TYPE COSP-KSTAR,    "科目/成本要素
    KTEXT_EN  TYPE CSKU-KTEXT,    "描述(英)
    KTEXT_ZH  TYPE CSKU-KTEXT,    "描述(中)
    TXT50_EN  TYPE SKAT-TXT50,    "科目描述（英）
    TXT50_ZH  TYPE SKAT-TXT50,    "科目描述（中）
    SUBTOL_1  TYPE COSP-WKG009,   "上年累计数(合计)
    WKGXXX_1  TYPE COSP-WKG009,   "上年累计数：20**年累计费用发生额
    WKGXXX_2  TYPE COSP-WKG009,   "上年累计数：20**年累计费用分摊额
    SUBTOL_2  TYPE COSP-WKG009,   "本年预算合计(合计)
    WKGXXX_3  TYPE COSP-WKG009,   "本年预算合计：预算金额
    WKGXXX_4  TYPE COSP-WKG009,   "本年预算合计：预算分摊金额
    SUBTOL_3  TYPE COSP-WKG009,   "本期实际(合计)
    WKGXXX_5  TYPE COSP-WKG009,   "本期实际：20**年**月费用发生额
    WKGXXX_6  TYPE COSP-WKG009,   "本期实际：20**年**月费用分摊额
    SUBTOL_4  TYPE COSP-WKG009,   "今年到本期实际数(合计)
    WKGXXX_7  TYPE COSP-WKG009,   "今年到本期实际数：20**年累计费用发生额
    WKGXXX_8  TYPE COSP-WKG009,   "今年到本期实际数：20**年累计费用分摊额
    WKGXXX_9  TYPE COSP-WKG009,   "预算使用比例
    WKGXXX_10 TYPE COSP-WKG009,   "剩余可用预算
    KHINR     TYPE CSKS-KHINR,    "成本中心组  ADD BY HANDWJ  20181218
  END OF T_DATA1,

  BEGIN OF T_DATA2,
    ABTEI     TYPE CSKS-ABTEI,    "部门代码
    KOSTL     TYPE CSKS-KOSTL,    "成本中心
    KSTAR     TYPE COSP-KSTAR,    "科目/成本要素
    KTEXT_EN  TYPE CSKU-KTEXT,    "描述(英)
    KTEXT_ZH  TYPE CSKU-KTEXT,    "描述(中)
    TXT50_EN  TYPE SKAT-TXT50,    "科目描述（英）
    TXT50_ZH  TYPE SKAT-TXT50,    "科目描述（中）
    WKGXXX_1  TYPE COSP-WKG009,   "上年累计数：20**年累计费用分摊额
    WKGXXX_2  TYPE COSP-WKG009,   "本期实际：20**年**月费用分摊额
    WKGXXX_3  TYPE COSP-WKG009,   "今年到本期实际数：20**年累计费用分摊额
    KHINR     TYPE CSKS-KHINR,    "成本中心组  ADD BY HANDWJ  20181218
  END OF T_DATA2,

  BEGIN OF T_EXCEL4,
    COL1(100) TYPE C,
    COL2(50) TYPE C,
    COL3(50) TYPE C,
    COL4(50) TYPE C,
  END OF T_EXCEL4,

  BEGIN OF T_EXCEL1,
    COL1(50) TYPE C,
    COL2(50) TYPE C,
    COL3(50) TYPE C,
    COL4(50) TYPE C,
    COL5(50) TYPE C,
    COL6(50) TYPE C,
    COL7(50) TYPE C,
    COL8(50) TYPE C,
    COL9(50) TYPE C,
    COL10(50) TYPE C,
    COL11(50) TYPE C,
    COL12(50) TYPE C,
    COL13(50) TYPE C,
    COL14(50) TYPE C,
    COL15(50) TYPE C,
    COL16(50) TYPE C,
    COL17(50) TYPE C,
    COL18(50) TYPE C,
    COL19(50) TYPE C,
  END OF T_EXCEL1,

  BEGIN OF T_EXCEL2,
    COL1(50) TYPE C,
    COL2(50) TYPE C,
    COL3(50) TYPE C,
    COL4(50) TYPE C,
    COL5(50) TYPE C,
    COL6(50) TYPE C,
    COL7(50) TYPE C,
    COL8(50) TYPE C,
  END OF T_EXCEL2.

*$*$********************************************************************
*$*$    GLOBAL CONSTANTS                                               *
*$*$********************************************************************
CONSTANTS:
  C_KOKRS   TYPE  CSKS-KOKRS    VALUE 'ZF00',
  C_KTOPL   TYPE  CSKU-KTOPL    VALUE 'ZFSS',
  C_KTOPL2  TYPE  CSKU-KTOPL    VALUE 'ZFLS',
  C_CLSSNM    TYPE SBDST_CLASSNAME    VALUE 'ZCOBUDGET',
  C_CLSTYP    TYPE SBDST_CLASSTYPE    VALUE 'OT',
  C_OBJKEY    TYPE SBDST_OBJECT_KEY   VALUE 'ZCOBUDGT',
  C_DEFAULT_FILENAME TYPE STRING VALUE 'ZFSS_BUDGET_USAGE.xls'.

*$*$********************************************************************
*$*$    GLOBAL ELEMENTARY VARIABLES                                    *
*$*$********************************************************************
*-- BDS AND DOI REFERENCES PREPARING
DATA: G_DOCUMENT    TYPE REF TO CL_BDS_DOCUMENT_SET,
      G_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_CONTROL     TYPE REF TO I_OI_CONTAINER_CONTROL,
      G_EXCEL       TYPE REF TO I_OI_SPREADSHEET,
      G_PROXY       TYPE REF TO I_OI_DOCUMENT_PROXY,
      G_ERROR       TYPE REF TO I_OI_ERROR,
      G_RETCODE     TYPE SOI_RET_STRING.

DATA: G_OKCODE      TYPE SY-UCOMM,
      G_EXCEL_LINE  TYPE I,
      G_GRPNM_NUM   TYPE I.     "add by HANDWJ 20181217--

*$*$********************************************************************
*$*$    GLOBAL INTERNAL TABLES                                         *
*$*$********************************************************************
DATA:
  IT_R_KSTAR      TYPE  RANGE OF KSTAR,
  IT_R_PAROB      TYPE RANGE OF COSS-PAROB,
  IT_CSKS         TYPE  STANDARD TABLE OF CSKS,
  IT_CSKT         TYPE  STANDARD TABLE OF CSKT,
  IT_CSKU         TYPE  STANDARD TABLE OF CSKU,
  IT_SKAT_ZH      TYPE  STANDARD TABLE OF SKAT,
  IT_SKAT_EN      TYPE  STANDARD TABLE OF SKAT,
  IT_SKB1_EN      TYPE  STANDARD TABLE OF SKB1,
  IT_DATA1        TYPE  STANDARD TABLE OF T_DATA1,
  IT_DATA2        TYPE  STANDARD TABLE OF T_DATA2,
"add by HANDWJ 20181217 begin--
  IT_R_CC         TYPE  RANGE OF CSKS-KOSTL,
*  IT_DATA1_PART   TYPE  STANDARD TABLE OF T_DATA1,
*  IT_DATA2_PART   TYPE  STANDARD TABLE OF T_DATA2,
  IT_GRPNM_CSKS   TYPE  STANDARD TABLE OF CSKS WITH HEADER LINE,
  IT_KHINR        TYPE  STANDARD TABLE OF CSKS WITH HEADER LINE,

  BEGIN OF IT_SETNODE OCCURS 0,
    SETNAME    TYPE SETNODE-SETNAME,
    SUBSETNAME TYPE SETNODE-SUBSETNAME,
    KHINR1     TYPE CSKS-KHINR,
    KHINR2     TYPE CSKS-KHINR,
    END OF IT_SETNODE.

FIELD-SYMBOLS:<FS_DATA1> TYPE T_DATA1,
              <FS_DATA2> TYPE T_DATA2,
              <FS_SET>   LIKE IT_SETNODE.
"add by HANDWJ 20181217 end--

*$*$********************************************************************
*$*$    PARAMETERS & SELECT-OPTIONS                                    *
*$*$********************************************************************
PARAMETERS:P_BUKRS  TYPE CSKS-BUKRS OBLIGATORY DEFAULT 'ZFSH'.
SELECT-OPTIONS:
  S_ABTEI FOR CSKS-ABTEI      MODIF ID TY1,  "mod by HANDWJ 20181217--
  S_GRPNM FOR RGSB4-SHORTNAME MODIF ID TY2,  "add by HANDWJ 20181217--
  S_KOSTL FOR CSKS-KOSTL,
  S_PRCTR FOR CSKS-PRCTR,
  S_FUNCA FOR CSKS-FUNC_AREA.
PARAMETERS:
  P_GJAHR TYPE ZTCOCCYSBZ-GJAHR OBLIGATORY DEFAULT SY-DATUM+0(4),  "会计年度
  P_MONTH TYPE MONTH OBLIGATORY DEFAULT SY-DATUM+4(2),     "会计期间
  P_VERSN TYPE COSP-VERSN OBLIGATORY DEFAULT '000' MATCHCODE OBJECT ZH_TKA09,
  P_BOX   TYPE C AS CHECKBOX USER-COMMAND CB1.                          "add by HANDWJ 20181217--


*Selection screen: For "SAVE" Popup
SELECTION-SCREEN BEGIN OF SCREEN 9002 AS SUBSCREEN.

PARAMETERS: P_SVPATH  LIKE RLGRAP-FILENAME.

SELECTION-SCREEN END OF SCREEN 9002.

*$*$********************************************************************
*$*$    INITIALIZATION                                                 *
*$*$********************************************************************
INITIALIZATION.
  LOOP AT SCREEN.
    CASE SCREEN-GROUP1.
      WHEN 'TY1'.
        SCREEN-ACTIVE = 1.
        SCREEN-INVISIBLE = 0.
      WHEN 'TY2'.
        SCREEN-ACTIVE  = 0.
        SCREEN-INVISIBLE = 1.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

*$*$********************************************************************
*$*$    AT SELECTION-SCREEN  Output                                    *
*$*$********************************************************************
AT SELECTION-SCREEN OUTPUT.
  "add by HANDWJ 20181217 begin--
  IF P_BOX = 'X'.
    LOOP AT SCREEN.
      CASE SCREEN-GROUP1.
        WHEN 'TY1'.
          SCREEN-ACTIVE = 0.
          SCREEN-INVISIBLE = 1.
        WHEN 'TY2'.
          SCREEN-ACTIVE  = 1.
          SCREEN-INVISIBLE = 0.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      CASE SCREEN-GROUP1.
        WHEN 'TY1'.
          SCREEN-ACTIVE = 1.
          SCREEN-INVISIBLE = 0.
        WHEN 'TY2'.
          SCREEN-ACTIVE  = 0.
          SCREEN-INVISIBLE = 1.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
  "add by HANDWJ 20181217 end--

*$*$********************************************************************
*$*$    AT SELECTION-SCREEN ON                                         *
*$*$********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ABTEI-LOW.
  PERFORM FRM_F4_ABTEI CHANGING S_ABTEI-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ABTEI-HIGH.
  PERFORM FRM_F4_ABTEI CHANGING S_ABTEI-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_SVPATH.
  DATA I_FILEPATH TYPE STRING.
  DATA I_FULLPATH TYPE STRING.
  DATA I_FILENAME TYPE STRING.

  "Get the save path
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      DEFAULT_FILE_NAME    = C_DEFAULT_FILENAME
    CHANGING
      FILENAME             = I_FILENAME
      PATH                 = I_FILEPATH
      FULLPATH             = I_FULLPATH
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC <> 0.
    "Do nothing
  ENDIF.
  CHECK I_FULLPATH IS NOT INITIAL.
  P_SVPATH = I_FULLPATH.

*$*$********************************************************************
*$*$    AT SELECTION-SCREEN                                            *
*$*$********************************************************************
AT SELECTION-SCREEN.
* 权限检查
  AUTHORITY-CHECK OBJECT 'ZFI_BUK'
    ID  'BUKRS' FIELD P_BUKRS.
  IF SY-SUBRC NE 0.
    SET CURSOR FIELD 'P_BUKRS'.
    MESSAGE E078(ZFICO_MSG) WITH P_BUKRS.
  ENDIF.

  IF S_ABTEI[] IS INITIAL
  AND S_KOSTL[] IS INITIAL.
    SET CURSOR FIELD 'S_KOSTL-LOW'.
    MESSAGE TEXT-M01 TYPE 'E'.
  ENDIF.


*$*$********************************************************************
*$*$    START-OF-SELECTION                                             *
*$*$********************************************************************
START-OF-SELECTION.
  "add by HANDWJ 20181217 begin--
  IF P_BOX = 'X' AND S_GRPNM[] IS INITIAL.
    MESSAGE '您已选择按成本中心组拆分，请输入成本中心组！' TYPE 'E'.
  ENDIF.
  "add by HANDWJ 20181217 end--

*1. 取数
  PERFORM FRM_GET_DATA.

*2. 调EXCEL展示
  CALL SCREEN 9000.

*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       取数
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .

  DATA:
    I_GJAHR             TYPE COSP-GJAHR,
    WA_R_KSTAR          LIKE LINE OF IT_R_KSTAR,
    WA_L_CSKS           TYPE CSKS,
    IT_L_CSKS           TYPE STANDARD TABLE OF CSKS,
    WA_L_CSKU           TYPE CSKU,
    WA_L_DATA1          TYPE T_DATA1,
    WA_L_DATA2          TYPE T_DATA2,
    WA_L_ZTCOCCT_RULE1  TYPE ZTCOCCT_RULE1,
    IT_L_ZTCOCCT_RULE1  TYPE STANDARD TABLE OF ZTCOCCT_RULE1,
    I_PROGRESS          TYPE C LENGTH 100,
    WA_R_PAROB          LIKE LINE OF IT_R_PAROB,
    WA_L_COSS           TYPE COSS,
    WA_L_SKAT_ZH        TYPE SKAT,
    WA_L_SKAT_EN        TYPE SKAT,
    WA_L_SKB1_EN        TYPE SKB1,
    I_PERC              TYPE P DECIMALS 4 LENGTH 10.

*-1. 成本中心主数据
  REFRESH:IT_CSKS.
***add by HANDWJ 20181217 begin--
  IF P_BOX = 'X'.
    "取成本中心组
    REFRESH:IT_R_CC.
    PERFORM FRM_GET_COSTC.

    SELECT  *
    FROM  CSKS
    INTO  CORRESPONDING FIELDS OF TABLE IT_CSKS
   WHERE  KOKRS = C_KOKRS AND
          BUKRS = P_BUKRS AND
          KOSTL IN S_KOSTL AND
          KOSTL IN IT_R_CC AND
          FUNC_AREA IN S_FUNCA AND
          PRCTR IN S_PRCTR AND
          DATBI >= SY-DATUM AND
          DATAB <= SY-DATUM.
  ELSE.
***add by HANDWJ 20181217 end--
    SELECT  *
      FROM  CSKS
      INTO  CORRESPONDING FIELDS OF TABLE IT_CSKS
     WHERE  KOKRS = C_KOKRS AND
            BUKRS = P_BUKRS AND
            ABTEI IN S_ABTEI AND
            KOSTL IN S_KOSTL AND
            FUNC_AREA IN S_FUNCA AND
            PRCTR IN S_PRCTR AND
            DATBI >= SY-DATUM AND
            DATAB <= SY-DATUM.
  ENDIF.
  IF SY-SUBRC NE 0.
    MESSAGE S002(ZFICO_MSG) DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  "成本中心 描述
  REFRESH:IT_CSKT.
  SELECT *
    FROM CSKT
    INTO CORRESPONDING FIELDS OF TABLE IT_CSKT
     FOR ALL ENTRIES IN IT_CSKS
   WHERE  KOKRS = C_KOKRS AND
          KOSTL = IT_CSKS-KOSTL AND
          SPRAS = SY-LANGU AND
          DATBI >= SY-DATUM.


*2. 先取 记账费用发生额
  "CE-OTHCOST成本要素组
  REFRESH:IT_R_KSTAR.
  PERFORM FRM_GET_KSTAR USING 'CE-OTHCOST'.
  SORT IT_R_KSTAR BY LOW.
  DELETE ADJACENT DUPLICATES FROM IT_R_KSTAR COMPARING LOW.

  "成本要素 描述
  REFRESH:IT_CSKU.
  SELECT *
    FROM CSKU
    APPENDING CORRESPONDING FIELDS OF TABLE IT_CSKU
   WHERE KTOPL = C_KTOPL
     AND KSTAR IN IT_R_KSTAR
     AND ( SPRAS = 'E'
      OR SPRAS = '1').

  "科目中文描述
  REFRESH:IT_SKAT_ZH.
  SELECT SAKNR  TXT50
    FROM SKAT
    INTO CORRESPONDING FIELDS OF TABLE IT_SKAT_ZH
   WHERE SPRAS = '1'
     AND KTOPL = C_KTOPL
     AND SAKNR IN IT_R_KSTAR.

  "备选科目
  REFRESH:IT_SKB1_EN.
  SELECT SAKNR
         ALTKT
    FROM SKB1
    INTO CORRESPONDING FIELDS OF TABLE IT_SKB1_EN
   WHERE BUKRS = P_BUKRS
     AND SAKNR IN IT_R_KSTAR.
  "英文描述
  IF IT_SKB1_EN IS NOT INITIAL.
    REFRESH:IT_SKAT_EN.
    SELECT SAKNR  TXT50
      FROM SKAT
      INTO CORRESPONDING FIELDS OF TABLE IT_SKAT_EN
       FOR ALL ENTRIES IN IT_SKB1_EN
     WHERE SPRAS = 'E'
       AND KTOPL = C_KTOPL2
       AND SAKNR = IT_SKB1_EN-ALTKT.
  ENDIF.


  "记账费用发生额  - 每一个成本中心
  REFRESH:IT_L_CSKS.
  APPEND LINES OF IT_CSKS TO IT_L_CSKS.
  LOOP AT IT_L_CSKS INTO WA_L_CSKS.

    LOOP AT IT_R_KSTAR INTO WA_R_KSTAR.

      "进度条
      CONCATENATE TEXT-P01
                  TEXT-P02
                  WA_L_CSKS-KOSTL
                  TEXT-P03
                  WA_R_KSTAR-LOW
             INTO I_PROGRESS SEPARATED BY SPACE.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          PERCENTAGE = 80
          TEXT       = I_PROGRESS.

      CLEAR:WA_L_DATA1.
      WA_L_DATA1-ABTEI = WA_L_CSKS-ABTEI.
      WA_L_DATA1-KOSTL = WA_L_CSKS-KOSTL.
      WA_L_DATA1-KSTAR = WA_R_KSTAR-LOW.
      IF P_BOX = 'X'.
        WA_L_DATA1-KHINR = WA_L_CSKS-KHINR.
      ENDIF.

      "成本要素中英文描述
      CLEAR:WA_L_CSKU.
      READ TABLE IT_CSKU INTO WA_L_CSKU
        WITH KEY KSTAR = WA_R_KSTAR-LOW
                 SPRAS = 'E'.
      IF SY-SUBRC = 0.
        WA_L_DATA1-KTEXT_EN = WA_L_CSKU-KTEXT.
      ENDIF.
      CLEAR:WA_L_CSKU.
      READ TABLE IT_CSKU INTO WA_L_CSKU
        WITH KEY KSTAR = WA_R_KSTAR-LOW
                 SPRAS = '1'.
      IF SY-SUBRC = 0.
        WA_L_DATA1-KTEXT_ZH = WA_L_CSKU-KTEXT.
      ENDIF.

      "科目中英文描述
      "-->中文
      CLEAR:WA_L_SKAT_ZH.
      READ TABLE IT_SKAT_ZH INTO WA_L_SKAT_ZH
        WITH KEY SAKNR = WA_R_KSTAR-LOW.
      IF SY-SUBRC = 0.
        WA_L_DATA1-TXT50_ZH = WA_L_SKAT_ZH-TXT50.
      ENDIF.
      "-->英文
      CLEAR:WA_L_SKB1_EN.
      READ TABLE IT_SKB1_EN INTO WA_L_SKB1_EN
        WITH KEY SAKNR = WA_R_KSTAR-LOW.
      IF SY-SUBRC = 0.
        CLEAR:WA_L_SKAT_EN.
        READ TABLE IT_SKAT_EN INTO WA_L_SKAT_EN
          WITH KEY SAKNR = WA_L_SKB1_EN-ALTKT.
        IF SY-SUBRC = 0.
          WA_L_DATA1-TXT50_EN = WA_L_SKAT_EN-TXT50.
        ENDIF.
      ENDIF.

*------> 上年累计数
      "20**年累计费用发生额
      CLEAR I_GJAHR.
      I_GJAHR = P_GJAHR - 1.
      PERFORM FRM_GET_AMOUNT    USING  '3'      "全年合计
                                       WA_L_CSKS-OBJNR  "成本中心
                                       WA_R_KSTAR-LOW   "科目
                                       I_GJAHR  "年度
                                       '04'     "类型
                                       'COIN'   "业务
                              CHANGING WA_L_DATA1-WKGXXX_1.
      "20**年累计费用分摊额
      CLEAR I_GJAHR.
      I_GJAHR = P_GJAHR - 1.
      PERFORM FRM_GET_AMOUNT    USING  '3'      "全年合计
                                       WA_L_CSKS-OBJNR  "成本中心
                                       WA_R_KSTAR-LOW   "科目
                                       I_GJAHR  "年度
                                       '04'     "类型
                                       'RKIV'   "业务
                              CHANGING WA_L_DATA1-WKGXXX_2.
      WA_L_DATA1-SUBTOL_1 = WA_L_DATA1-WKGXXX_1 + WA_L_DATA1-WKGXXX_2.

*------> 本年预算合计
      "预算金额
      CLEAR I_GJAHR.
      I_GJAHR = P_GJAHR.
      PERFORM FRM_GET_AMOUNT    USING  '3'      "全年合计
                                       WA_L_CSKS-OBJNR  "成本中心
                                       WA_R_KSTAR-LOW   "科目
                                       I_GJAHR  "年度
                                       '01'     "类型
                                       'RKP1'   "业务
                              CHANGING WA_L_DATA1-WKGXXX_3.
      "预算分摊金额
      CLEAR I_GJAHR.
      I_GJAHR = P_GJAHR.
      PERFORM FRM_GET_AMOUNT    USING  '3'      "全年合计
                                       WA_L_CSKS-OBJNR  "成本中心
                                       WA_R_KSTAR-LOW   "科目
                                       I_GJAHR  "年度
                                       '01'     "类型
                                       'RKPV'   "业务
                              CHANGING WA_L_DATA1-WKGXXX_4.
      WA_L_DATA1-SUBTOL_2 = WA_L_DATA1-WKGXXX_3 + WA_L_DATA1-WKGXXX_4.

*------> 本期实际
      "20**年**月费用发生额
      CLEAR I_GJAHR.
      I_GJAHR = P_GJAHR.
      PERFORM FRM_GET_AMOUNT    USING  '1'      "当期
                                       WA_L_CSKS-OBJNR  "成本中心
                                       WA_R_KSTAR-LOW   "科目
                                       I_GJAHR  "年度
                                       '04'     "类型
                                       'COIN'   "业务
                              CHANGING WA_L_DATA1-WKGXXX_5.
      "20**年**月费用分摊额
      CLEAR I_GJAHR.
      I_GJAHR = P_GJAHR.
      PERFORM FRM_GET_AMOUNT    USING  '1'      "当期
                                       WA_L_CSKS-OBJNR  "成本中心
                                       WA_R_KSTAR-LOW   "科目
                                       I_GJAHR  "年度
                                       '04'     "类型
                                       'RKIV'   "业务
                              CHANGING WA_L_DATA1-WKGXXX_6.
      WA_L_DATA1-SUBTOL_3 = WA_L_DATA1-WKGXXX_5 + WA_L_DATA1-WKGXXX_6.

*------> 今年到本期实际数
      "20**年累计费用发生额
      CLEAR I_GJAHR.
      I_GJAHR = P_GJAHR.
      PERFORM FRM_GET_AMOUNT    USING  '2'      "至当期
                                       WA_L_CSKS-OBJNR  "成本中心
                                       WA_R_KSTAR-LOW   "科目
                                       I_GJAHR  "年度
                                       '04'     "类型
                                       'COIN'   "业务
                              CHANGING WA_L_DATA1-WKGXXX_7.
      "20**年累计费用分摊额
      CLEAR I_GJAHR.
      I_GJAHR = P_GJAHR.
      PERFORM FRM_GET_AMOUNT    USING  '2'      "至当期
                                       WA_L_CSKS-OBJNR  "成本中心
                                       WA_R_KSTAR-LOW   "科目
                                       I_GJAHR  "年度
                                       '04'     "类型
                                       'RKIV'   "业务
                              CHANGING WA_L_DATA1-WKGXXX_8.
      WA_L_DATA1-SUBTOL_4 = WA_L_DATA1-WKGXXX_7 + WA_L_DATA1-WKGXXX_8.

*------> 预算使用比例
      IF WA_L_DATA1-SUBTOL_2 IS NOT INITIAL.
        CLEAR:I_PERC.
        I_PERC = WA_L_DATA1-SUBTOL_4 / WA_L_DATA1-SUBTOL_2.
        WA_L_DATA1-WKGXXX_9 = I_PERC * 100.
      ELSE.
        WA_L_DATA1-WKGXXX_9  = 0.
      ENDIF.

*------> 剩余可用预算
      WA_L_DATA1-WKGXXX_10 = WA_L_DATA1-SUBTOL_2 - WA_L_DATA1-SUBTOL_4.

      "判断
      IF WA_L_DATA1-WKGXXX_1 IS INITIAL
      AND WA_L_DATA1-WKGXXX_2 IS INITIAL
      AND WA_L_DATA1-WKGXXX_3 IS INITIAL
      AND WA_L_DATA1-WKGXXX_4 IS INITIAL
      AND WA_L_DATA1-WKGXXX_5 IS INITIAL
      AND WA_L_DATA1-WKGXXX_6 IS INITIAL
      AND WA_L_DATA1-WKGXXX_7 IS INITIAL
      AND WA_L_DATA1-WKGXXX_8 IS INITIAL.
        CONTINUE.
      ENDIF.

      APPEND WA_L_DATA1 TO IT_DATA1.
    ENDLOOP.
  ENDLOOP.
  SORT IT_DATA1 BY  ABTEI    "部门代码
                    KOSTL     "成本中心
                    KSTAR ASCENDING.  "科目/成本要素


*3. 次级分摊费用Allocated secondary expenses
  "CE-OTHCOST成本要素组
  REFRESH:IT_R_KSTAR.
  PERFORM FRM_GET_KSTAR USING 'CE2-GT'.
  SORT IT_R_KSTAR BY LOW.
  DELETE ADJACENT DUPLICATES FROM IT_R_KSTAR COMPARING LOW.

  "成本要素 描述
  REFRESH:IT_CSKU.
  SELECT *
    FROM CSKU
    APPENDING CORRESPONDING FIELDS OF TABLE IT_CSKU
   WHERE KTOPL = C_KTOPL
     AND KSTAR IN IT_R_KSTAR
     AND ( SPRAS = 'E'
      OR SPRAS = '1').


  "科目中文描述
  REFRESH:IT_SKAT_ZH.
  SELECT SAKNR  TXT50
    FROM SKAT
    INTO CORRESPONDING FIELDS OF TABLE IT_SKAT_ZH
   WHERE SPRAS = '1'
     AND KTOPL = C_KTOPL
     AND SAKNR IN IT_R_KSTAR.

  "备选科目
  REFRESH:IT_SKB1_EN.
  SELECT SAKNR
         ALTKT
    FROM SKB1
    INTO CORRESPONDING FIELDS OF TABLE IT_SKB1_EN
   WHERE BUKRS = P_BUKRS
     AND SAKNR IN IT_R_KSTAR.
  "英文描述
  IF IT_SKB1_EN IS NOT INITIAL.
    REFRESH:IT_SKAT_EN.
    SELECT SAKNR  TXT50
      FROM SKAT
      INTO CORRESPONDING FIELDS OF TABLE IT_SKAT_EN
       FOR ALL ENTRIES IN IT_SKB1_EN
     WHERE SPRAS = 'E'
       AND KTOPL = C_KTOPL2
       AND SAKNR = IT_SKB1_EN-ALTKT.
  ENDIF.


  "排除 - 业务伙伴成本中心
  REFRESH:IT_L_ZTCOCCT_RULE1.
  REFRESH:IT_R_PAROB.
*---20131203- 暂不需要
*  SELECT *
*    FROM ZTCOCCT_RULE1
*    INTO CORRESPONDING FIELDS OF TABLE it_l_ZTCOCCT_RULE1.
*  LOOP AT it_l_ZTCOCCT_RULE1 INTO wa_l_ZTCOCCT_RULE1.
*    clear:wa_r_PAROB.
*    wa_r_PAROB-sign    = 'I'.
*    wa_r_PAROB-option  = 'EQ'.
*    CONCATENATE 'KSZF00'
*                 wa_l_ZTCOCCT_RULE1-KOSTL
*                 INTO  wa_r_PAROB-low.
*    append wa_r_PAROB to it_r_PAROB.
*  ENDLOOP.


  REFRESH:IT_L_CSKS.
  LOOP AT IT_CSKS INTO WA_L_CSKS.
    APPEND WA_L_CSKS TO IT_L_CSKS.
  ENDLOOP.

  LOOP AT IT_L_CSKS INTO WA_L_CSKS.

    "判断COSS
    CLEAR:WA_L_COSS.
    SELECT  OBJNR PAROB
      FROM  COSS
      INTO  CORRESPONDING FIELDS OF WA_L_COSS
       UP TO 1 ROWS
     WHERE  LEDNR = '00'
       AND  OBJNR = WA_L_CSKS-OBJNR
       AND  PAROB IN IT_R_PAROB.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      CONTINUE.   "合作伙伴成本中心
    ENDIF.

    LOOP AT IT_R_KSTAR INTO WA_R_KSTAR.

      "进度条
      CONCATENATE TEXT-P01
                  TEXT-P02
                  WA_L_CSKS-KOSTL
                  TEXT-P03
                  WA_R_KSTAR-LOW
             INTO I_PROGRESS SEPARATED BY SPACE.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          PERCENTAGE = 80
          TEXT       = I_PROGRESS.

      CLEAR:WA_L_DATA2.
      WA_L_DATA2-ABTEI = WA_L_CSKS-ABTEI.
      WA_L_DATA2-KOSTL = WA_L_CSKS-KOSTL.
      WA_L_DATA2-KSTAR = WA_R_KSTAR-LOW.
      IF P_BOX = 'X'.
        WA_L_DATA2-KHINR = WA_L_CSKS-KHINR.
      ENDIF.

      CLEAR:WA_L_CSKU.
      READ TABLE IT_CSKU INTO WA_L_CSKU
        WITH KEY KSTAR = WA_R_KSTAR-LOW
                 SPRAS = 'E'.
      IF SY-SUBRC = 0.
        WA_L_DATA2-TXT50_EN = WA_L_CSKU-KTEXT.
      ENDIF.
      CLEAR:WA_L_CSKU.
      READ TABLE IT_CSKU INTO WA_L_CSKU
        WITH KEY KSTAR = WA_R_KSTAR-LOW
                 SPRAS = '1'.
      IF SY-SUBRC = 0.
        WA_L_DATA2-TXT50_ZH = WA_L_CSKU-KTEXT.
      ENDIF.

*------> 上年累计数
      "20**年累计费用分摊额
      CLEAR I_GJAHR.
      I_GJAHR = P_GJAHR - 1.
      PERFORM FRM_GET_AMOUNT2    USING  '3'      "全年合计
                                       WA_L_CSKS-OBJNR  "成本中心
                                       WA_R_KSTAR-LOW   "科目
                                       I_GJAHR  "年度
                                       '04'     "类型
                                       'RKIU'   "业务
                              CHANGING WA_L_DATA2-WKGXXX_1.

*------> 本期实际
      "20**年**月费用分摊额
      CLEAR I_GJAHR.
      I_GJAHR = P_GJAHR.
      PERFORM FRM_GET_AMOUNT2    USING  '1'      "当期
                                       WA_L_CSKS-OBJNR  "成本中心
                                       WA_R_KSTAR-LOW   "科目
                                       I_GJAHR  "年度
                                       '04'     "类型
                                       'RKIU'   "业务
                              CHANGING WA_L_DATA2-WKGXXX_2.

*------> 今年到本期实际数
      "20**年累计费用分摊额
      CLEAR I_GJAHR.
      I_GJAHR = P_GJAHR.
      PERFORM FRM_GET_AMOUNT2    USING  '2'      "至当期
                                       WA_L_CSKS-OBJNR  "成本中心
                                       WA_R_KSTAR-LOW   "科目
                                       I_GJAHR  "年度
                                       '04'     "类型
                                       'RKIU'   "业务
                              CHANGING WA_L_DATA2-WKGXXX_3.

      "判断
      IF WA_L_DATA2-WKGXXX_1 IS INITIAL
      AND WA_L_DATA2-WKGXXX_2 IS INITIAL
      AND WA_L_DATA2-WKGXXX_3 IS INITIAL.
        CONTINUE.
      ENDIF.

      APPEND WA_L_DATA2 TO IT_DATA2.
    ENDLOOP.
  ENDLOOP.
  SORT IT_DATA2 BY  ABTEI    "部门代码
                    KOSTL     "成本中心
                    KSTAR ASCENDING.  "科目/成本要素

ENDFORM.                    " FRM_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_KSTAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3244   text
*----------------------------------------------------------------------*
FORM FRM_GET_KSTAR  USING   PR_CEG  TYPE  CHAR50.

  DATA:
    WA_R_KSTAR      LIKE LINE OF IT_R_KSTAR,
    I_SETNR         TYPE  CHAR100,
    WA_L_SET_VALUES TYPE  RGSB4,
    IT_L_SET_VALUES TYPE  STANDARD TABLE OF RGSB4.

  REFRESH:IT_R_KSTAR.
  CLEAR:I_SETNR.
  REFRESH IT_L_SET_VALUES[].
  I_SETNR+0(4)   = '0102'.            "Set type:Cost Element grp
  I_SETNR+4(4)   = 'ZFSS'.            "Chart of Accounts
  I_SETNR+8(10)  = PR_CEG.          "Cost Element grp
  "FM取得
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      SETNR         = I_SETNR
    TABLES
      SET_VALUES    = IT_L_SET_VALUES
    EXCEPTIONS
      SET_NOT_FOUND = 1
      OTHERS        = 2.
  LOOP AT IT_L_SET_VALUES INTO WA_L_SET_VALUES.
    IF WA_L_SET_VALUES-FROM IS NOT INITIAL.
      CLEAR:WA_R_KSTAR.
      WA_R_KSTAR-SIGN    = 'I'.
      WA_R_KSTAR-OPTION  = 'EQ'.
      WA_R_KSTAR-LOW     = WA_L_SET_VALUES-FROM.
      APPEND WA_R_KSTAR TO IT_R_KSTAR.
    ENDIF.
    IF WA_L_SET_VALUES-TO IS NOT INITIAL.
      CLEAR:WA_R_KSTAR.
      WA_R_KSTAR-SIGN    = 'I'.
      WA_R_KSTAR-OPTION  = 'EQ'.
      WA_R_KSTAR-LOW     = WA_L_SET_VALUES-TO.
      APPEND WA_R_KSTAR TO IT_R_KSTAR.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " FRM_GET_KSTAR
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_AMOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0382   text
*      -->P_I_GJAHR  text
*      -->P_0384   text
*      -->P_0385   text
*      <--P_WA_L_DATA1_WKGXXX_1  text
*----------------------------------------------------------------------*
FORM FRM_GET_AMOUNT  USING    PR_TYPE   TYPE  C
                              PR_OBJNR  TYPE COSP-OBJNR
                              PR_KSTAR  TYPE COSP-KSTAR
                              PR_GJAHR  TYPE COSP-GJAHR
                              PR_WRTTP  TYPE COSP-WRTTP
                              PR_VRGNG  TYPE COSP-VRGNG
                     CHANGING PV_WKGXXX TYPE COSP-WKG009.
  DATA:
    WA_L_CSKS     TYPE CSKS,
    WA_L_COSP     TYPE COSP,
    IT_L_COSP     TYPE STANDARD TABLE OF COSP,
    WA_L_COSS     TYPE COSS,
    IT_L_COSS     TYPE STANDARD TABLE OF COSS,
    I_NUM         TYPE N LENGTH 3,
    I_FIELDNM(50) TYPE C,
    WA_L_COND     TYPE T_COND,
    IT_L_COND     TYPE STANDARD TABLE OF T_COND.
  FIELD-SYMBOLS:<FS_L_VALUE>  TYPE COSP-WKG009.

*- 考虑公司代码
  REFRESH:IT_L_COND.
  CLEAR:WA_L_COND.
  WA_L_COND-COND = 'LEDNR = ''00'''.
  APPEND WA_L_COND TO IT_L_COND.
  CLEAR:WA_L_COND.
  WA_L_COND-COND = 'AND  GJAHR =  PR_GJAHR'.
  APPEND WA_L_COND TO IT_L_COND.
  CLEAR:WA_L_COND.
  WA_L_COND-COND = 'AND  OBJNR = PR_OBJNR'.
  APPEND WA_L_COND TO IT_L_COND.
  CLEAR:WA_L_COND.
  WA_L_COND-COND = 'AND  OBJNR LIKE ''KSZF00%'''.
  APPEND WA_L_COND TO IT_L_COND.
  CLEAR:WA_L_COND.
  WA_L_COND-COND = 'AND  KSTAR = PR_KSTAR'.
  APPEND WA_L_COND TO IT_L_COND.
  CLEAR:WA_L_COND.
  WA_L_COND-COND = 'AND  WRTTP = PR_WRTTP'.
  APPEND WA_L_COND TO IT_L_COND.
  CLEAR:WA_L_COND.
  WA_L_COND-COND = 'AND  VRGNG = PR_VRGNG'.
  APPEND WA_L_COND TO IT_L_COND.

  IF PR_WRTTP EQ '01'. "预算
    CLEAR:WA_L_COND.
    WA_L_COND-COND = 'AND  VERSN = P_VERSN'.
    APPEND WA_L_COND TO IT_L_COND.
  ELSE.
    CLEAR:WA_L_COND.
    WA_L_COND-COND = 'AND  VERSN = ''000'''.
    APPEND WA_L_COND TO IT_L_COND.
  ENDIF.

  IF PR_WRTTP NE '01'. "预算
    CLEAR:WA_L_COND.
    WA_L_COND-COND = 'AND  BUKRS = P_BUKRS'.
    APPEND WA_L_COND TO IT_L_COND.
  ENDIF.

*- 取数
  REFRESH:IT_L_COSP.
  SELECT  BEKNZ   "Debit(S)/credit indicator(H)
          WKG001  "Total Value in Controlling Area Currency 1
          WKG002  "Total Value in Controlling Area Currency 2
          WKG003  "Total Value in Controlling Area Currency 3
          WKG004  "Total Value in Controlling Area Currency 4
          WKG005  "Total Value in Controlling Area Currency 5
          WKG006  "Total Value in Controlling Area Currency 6
          WKG007  "Total Value in Controlling Area Currency 7
          WKG008  "Total Value in Controlling Area Currency 8
          WKG009  "Total Value in Controlling Area Currency 9
          WKG010  "Total Value in Controlling Area Currency 10
          WKG011  "Total Value in Controlling Area Currency 11
          WKG012  "Total Value in Controlling Area Currency 12
    FROM  COSP
    INTO  CORRESPONDING FIELDS OF TABLE IT_L_COSP
   WHERE (IT_L_COND).
  REFRESH:IT_L_COSS.
  SELECT  BEKNZ   "Debit(S)/credit indicator(H)
          WKG001  "Total Value in Controlling Area Currency 1
          WKG002  "Total Value in Controlling Area Currency 2
          WKG003  "Total Value in Controlling Area Currency 3
          WKG004  "Total Value in Controlling Area Currency 4
          WKG005  "Total Value in Controlling Area Currency 5
          WKG006  "Total Value in Controlling Area Currency 6
          WKG007  "Total Value in Controlling Area Currency 7
          WKG008  "Total Value in Controlling Area Currency 8
          WKG009  "Total Value in Controlling Area Currency 9
          WKG010  "Total Value in Controlling Area Currency 10
          WKG011  "Total Value in Controlling Area Currency 11
          WKG012  "Total Value in Controlling Area Currency 12
    FROM  COSS
    INTO  CORRESPONDING FIELDS OF TABLE IT_L_COSS
   WHERE  (IT_L_COND).
  "WKG001-WKG012
  LOOP AT IT_L_COSP INTO WA_L_COSP.

*    "贷方 -- 20131203: 不需负数，表中数据带符号！
*    IF wa_l_cosp-beknz = 'H'.
*      DO 12 TIMES.
*        i_num = SY-INDEX.
*        CLEAR:i_fieldnm.
*        CONCATENATE 'WKG' i_num INTO i_fieldnm.
*        ASSIGN COMPONENT i_fieldnm OF STRUCTURE  wa_l_cosp
*          to <fs_l_value>.
*        IF <fs_l_value> IS ASSIGNED.
*          <fs_l_value> = - <fs_l_value>.
*        ENDIF.
*      ENDDO.
*    ENDIF.

    "合计
    CASE PR_TYPE.
      WHEN '1'. "当期
        I_NUM = P_MONTH.
        CLEAR:I_FIELDNM.
        CONCATENATE 'WKG' I_NUM INTO I_FIELDNM.
        ASSIGN COMPONENT I_FIELDNM OF STRUCTURE  WA_L_COSP
          TO <FS_L_VALUE>.
        IF <FS_L_VALUE> IS ASSIGNED.
          PV_WKGXXX = PV_WKGXXX + <FS_L_VALUE>.
        ENDIF.
      WHEN '2'. "从期初至当期
        DO P_MONTH TIMES.
          I_NUM = SY-INDEX.
          CLEAR:I_FIELDNM.
          CONCATENATE 'WKG' I_NUM INTO I_FIELDNM.
          ASSIGN COMPONENT I_FIELDNM OF STRUCTURE  WA_L_COSP
            TO <FS_L_VALUE>.
          IF <FS_L_VALUE> IS ASSIGNED.
            PV_WKGXXX = PV_WKGXXX + <FS_L_VALUE>.
          ENDIF.
        ENDDO.
      WHEN '3'. "全年合计
        PV_WKGXXX = PV_WKGXXX
                  + WA_L_COSP-WKG001 + WA_L_COSP-WKG002
                  + WA_L_COSP-WKG003 + WA_L_COSP-WKG004
                  + WA_L_COSP-WKG005 + WA_L_COSP-WKG006
                  + WA_L_COSP-WKG007 + WA_L_COSP-WKG008
                  + WA_L_COSP-WKG009 + WA_L_COSP-WKG010
                  + WA_L_COSP-WKG011 + WA_L_COSP-WKG012.
    ENDCASE.
  ENDLOOP.
  LOOP AT IT_L_COSS INTO WA_L_COSS.

*    "贷方 -- 20131203: 不需负数，表中数据带符号！
*    IF wa_l_coss-beknz = 'H'.
*      DO 12 TIMES.
*        i_num = SY-INDEX.
*        CLEAR:i_fieldnm.
*        CONCATENATE 'WKG' i_num INTO i_fieldnm.
*        ASSIGN COMPONENT i_fieldnm OF STRUCTURE  wa_l_coss
*          to <fs_l_value>.
*        IF <fs_l_value> IS ASSIGNED.
*          <fs_l_value> = - <fs_l_value>.
*        ENDIF.
*      ENDDO.
*    ENDIF.

    "合计
    CASE PR_TYPE.
      WHEN '1'. "当期
        I_NUM = P_MONTH.
        CLEAR:I_FIELDNM.
        CONCATENATE 'WKG' I_NUM INTO I_FIELDNM.
        ASSIGN COMPONENT I_FIELDNM OF STRUCTURE  WA_L_COSS
          TO <FS_L_VALUE>.
        IF <FS_L_VALUE> IS ASSIGNED.
          PV_WKGXXX = PV_WKGXXX + <FS_L_VALUE>.
        ENDIF.
      WHEN '2'. "从期初至当期
        DO P_MONTH TIMES.
          I_NUM = SY-INDEX.
          CLEAR:I_FIELDNM.
          CONCATENATE 'WKG' I_NUM INTO I_FIELDNM.
          ASSIGN COMPONENT I_FIELDNM OF STRUCTURE  WA_L_COSS
            TO <FS_L_VALUE>.
          IF <FS_L_VALUE> IS ASSIGNED.
            PV_WKGXXX = PV_WKGXXX + <FS_L_VALUE>.
          ENDIF.
        ENDDO.
      WHEN '3'. "全年合计
        PV_WKGXXX = PV_WKGXXX
                  + WA_L_COSS-WKG001 + WA_L_COSS-WKG002
                  + WA_L_COSS-WKG003 + WA_L_COSS-WKG004
                  + WA_L_COSS-WKG005 + WA_L_COSS-WKG006
                  + WA_L_COSS-WKG007 + WA_L_COSS-WKG008
                  + WA_L_COSS-WKG009 + WA_L_COSS-WKG010
                  + WA_L_COSS-WKG011 + WA_L_COSS-WKG012.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FRM_GET_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_AMOUNT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1214   text
*      -->P_WA_L_CSKS_OBJNR  text
*      -->P_WA_R_KSTAR_LOW  text
*      -->P_I_GJAHR  text
*      -->P_1218   text
*      -->P_1219   text
*      <--P_WA_L_DATA2_WKGXXX_1  text
*----------------------------------------------------------------------*
FORM FRM_GET_AMOUNT2  USING    PR_TYPE   TYPE  C
                              PR_OBJNR  TYPE COSP-OBJNR
                              PR_KSTAR  TYPE COSP-KSTAR
                              PR_GJAHR  TYPE COSP-GJAHR
                              PR_WRTTP  TYPE COSP-WRTTP
                              PR_VRGNG  TYPE COSP-VRGNG
                     CHANGING PV_WKGXXX TYPE COSP-WKG009.
  DATA:
    WA_L_CSKS     TYPE CSKS,
    WA_L_COSS     TYPE COSS,
    IT_L_COSS     TYPE STANDARD TABLE OF COSS,
    I_NUM         TYPE N LENGTH 3,
    I_FIELDNM(50) TYPE C,
    WA_L_COND     TYPE T_COND,
    IT_L_COND     TYPE STANDARD TABLE OF T_COND.
  FIELD-SYMBOLS:<FS_L_VALUE>  TYPE COSP-WKG009.


*- 考虑公司代码
  REFRESH:IT_L_COND.
  CLEAR:WA_L_COND.
  WA_L_COND-COND = 'LEDNR = ''00'''.
  APPEND WA_L_COND TO IT_L_COND.
  CLEAR:WA_L_COND.
  WA_L_COND-COND = 'AND  GJAHR =  PR_GJAHR'.
  APPEND WA_L_COND TO IT_L_COND.
  CLEAR:WA_L_COND.
  WA_L_COND-COND = 'AND  OBJNR = PR_OBJNR'.
  APPEND WA_L_COND TO IT_L_COND.
  CLEAR:WA_L_COND.
  WA_L_COND-COND = 'AND  KSTAR = PR_KSTAR'.
  APPEND WA_L_COND TO IT_L_COND.
  CLEAR:WA_L_COND.
  WA_L_COND-COND = 'AND  WRTTP = PR_WRTTP'.
  APPEND WA_L_COND TO IT_L_COND.
  CLEAR:WA_L_COND.
  WA_L_COND-COND = 'AND  VRGNG = PR_VRGNG'.
  APPEND WA_L_COND TO IT_L_COND.

  IF PR_WRTTP EQ '01'. "预算
    CLEAR:WA_L_COND.
    WA_L_COND-COND = 'AND  VERSN = P_VERSN'.
    APPEND WA_L_COND TO IT_L_COND.
  ELSE.
    CLEAR:WA_L_COND.
    WA_L_COND-COND = 'AND  VERSN = ''000'''.
    APPEND WA_L_COND TO IT_L_COND.
  ENDIF.

  IF PR_WRTTP NE '01'. "预算
    CLEAR:WA_L_COND.
    WA_L_COND-COND = 'AND  BUKRS = P_BUKRS'.
    APPEND WA_L_COND TO IT_L_COND.
  ENDIF.
  CLEAR:WA_L_COND.
  WA_L_COND-COND = 'AND  PAROB IN IT_R_PAROB'.
  APPEND WA_L_COND TO IT_L_COND.
  CLEAR:WA_L_COND.
  WA_L_COND-COND = 'AND  BEKNZ = ''S'''.
  APPEND WA_L_COND TO IT_L_COND.

*- 取数
  REFRESH:IT_L_COSS.
  SELECT  BEKNZ   "Debit(S)/credit indicator(H)
          WKG001  "Total Value in Controlling Area Currency 1
          WKG002  "Total Value in Controlling Area Currency 2
          WKG003  "Total Value in Controlling Area Currency 3
          WKG004  "Total Value in Controlling Area Currency 4
          WKG005  "Total Value in Controlling Area Currency 5
          WKG006  "Total Value in Controlling Area Currency 6
          WKG007  "Total Value in Controlling Area Currency 7
          WKG008  "Total Value in Controlling Area Currency 8
          WKG009  "Total Value in Controlling Area Currency 9
          WKG010  "Total Value in Controlling Area Currency 10
          WKG011  "Total Value in Controlling Area Currency 11
          WKG012  "Total Value in Controlling Area Currency 12
    FROM  COSS
    INTO  CORRESPONDING FIELDS OF TABLE IT_L_COSS
   WHERE  (IT_L_COND).
  LOOP AT IT_L_COSS INTO WA_L_COSS.

    "合计
    CASE PR_TYPE.
      WHEN '1'. "当期
        I_NUM = P_MONTH.
        CLEAR:I_FIELDNM.
        CONCATENATE 'WKG' I_NUM INTO I_FIELDNM.
        ASSIGN COMPONENT I_FIELDNM OF STRUCTURE  WA_L_COSS
          TO <FS_L_VALUE>.
        IF <FS_L_VALUE> IS ASSIGNED.
          PV_WKGXXX = PV_WKGXXX + <FS_L_VALUE>.
        ENDIF.
      WHEN '2'. "从期初至当期
        DO P_MONTH TIMES.
          I_NUM = SY-INDEX.
          CLEAR:I_FIELDNM.
          CONCATENATE 'WKG' I_NUM INTO I_FIELDNM.
          ASSIGN COMPONENT I_FIELDNM OF STRUCTURE  WA_L_COSS
            TO <FS_L_VALUE>.
          IF <FS_L_VALUE> IS ASSIGNED.
            PV_WKGXXX = PV_WKGXXX + <FS_L_VALUE>.
          ENDIF.
        ENDDO.
      WHEN '3'. "全年合计
        PV_WKGXXX = PV_WKGXXX
                  + WA_L_COSS-WKG001 + WA_L_COSS-WKG002
                  + WA_L_COSS-WKG003 + WA_L_COSS-WKG004
                  + WA_L_COSS-WKG005 + WA_L_COSS-WKG006
                  + WA_L_COSS-WKG007 + WA_L_COSS-WKG008
                  + WA_L_COSS-WKG009 + WA_L_COSS-WKG010
                  + WA_L_COSS-WKG011 + WA_L_COSS-WKG012.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FRM_GET_AMOUNT2

*&---------------------------------------------------------------------*
*&      Module  9000_PBO_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE 9000_PBO_STATUS OUTPUT.

* PF Status and Title bar
  SET PF-STATUS 'PF_9000'.

  SET TITLEBAR 'TB_9000'.

* BDS data
  CHECK G_DOCUMENT IS INITIAL.

  "1. Open the BDS document
  PERFORM FRM_GET_DOCUMENT.

  "2. Fill data
  IF P_BOX = 'X'.
    PERFORM FRM_BUILD_DOCUMENT2.
  ELSE.
    PERFORM FRM_BUILD_DOCUMENT.
  ENDIF.

ENDMODULE.                 " 9000_PBO_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  9000_PAI_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE 9000_PAI_COMMAND INPUT.

  CASE G_OKCODE.
    WHEN 'BACK'
       OR '%EX'
       OR 'RW'.
      "Close document
      PERFORM FRM_CLOSE_DOCUMENT.

      LEAVE TO SCREEN 0.
      EXIT.
    WHEN 'SAVE'.
      PERFORM FRM_DOWNLOAD_DOCUMENT.

    WHEN 'PRT'.
      PERFORM FRM_PRINT_DOCUMENT.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " 9000_PAI_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Form  FRM_CLOSE_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_CLOSE_DOCUMENT .

* Close document
  IF NOT G_PROXY IS INITIAL.
    CALL METHOD G_PROXY->CLOSE_DOCUMENT
      IMPORTING
        ERROR   = G_ERROR
        RETCODE = G_RETCODE.

    IF G_ERROR->HAS_FAILED = ABAP_TRUE.
      CALL METHOD G_ERROR->RAISE_MESSAGE
        EXPORTING
          TYPE = 'E'.
    ENDIF.
  ENDIF.
ENDFORM.                    " FRM_CLOSE_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  FRM_DOWNLOAD_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_DOWNLOAD_DOCUMENT .
  CALL SCREEN 9001
  STARTING AT 10 10
    ENDING AT 90 13.
ENDFORM.                    " FRM_DOWNLOAD_DOCUMENT
*&---------------------------------------------------------------------*
*&      Module  9001_PBO_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE 9001_PBO_STATUS OUTPUT.
  SET PF-STATUS 'PF_9001'.
ENDMODULE.                 " 9001_PBO_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  9001_PAI_USER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE 9001_PAI_USER INPUT.

  CASE G_OKCODE.
    WHEN '9001_CAN'.    "Cancel
      SET SCREEN 0.
    WHEN '9001_SV'.     "SAVE
      PERFORM FRM_9001_SAVE.
      SET SCREEN 0.
    WHEN OTHERS.
      "Do nothing
  ENDCASE.
  CLEAR:G_OKCODE.

ENDMODULE.                 " 9001_PAI_USER  INPUT
*&---------------------------------------------------------------------*
*&      Form  FRM_9001_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_9001_SAVE .
  DATA:
    I_RETCODE TYPE SOI_RET_STRING.

  CHECK G_PROXY IS NOT INITIAL.
  CALL METHOD G_PROXY->SAVE_AS
    EXPORTING
      FILE_NAME   = P_SVPATH
      NO_FLUSH    = ' '
      PROMPT_USER = ' '
    IMPORTING
      ERROR       = G_ERROR
      RETCODE     = I_RETCODE.
  IF G_ERROR->HAS_FAILED IS INITIAL.
    MESSAGE S016(ZFICO_MSG).
  ELSE.
    MESSAGE S015(ZFICO_MSG) DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM.                    " FRM_9001_SAVE
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DOCUMENT .

  DATA: IT_L_SIGNATURE   TYPE SBDST_SIGNATURE,
        IT_L_URIS        TYPE SBDST_URI,
        WA_L_SIGNATURE   LIKE LINE OF IT_L_SIGNATURE,
        WA_L_URIS        LIKE LINE OF IT_L_URIS.

  IF G_DOCUMENT IS NOT INITIAL.
    EXIT.
  ENDIF.

* Create container control
  CALL METHOD C_OI_CONTAINER_CONTROL_CREATOR=>GET_CONTAINER_CONTROL
    IMPORTING
      CONTROL = G_CONTROL
      RETCODE = G_RETCODE.
  IF G_RETCODE NE C_OI_ERRORS=>RET_OK.
    CALL METHOD C_OI_ERRORS=>RAISE_MESSAGE
      EXPORTING
        TYPE = 'E'.
  ENDIF.

* Initialize Custom Control
  CREATE OBJECT G_CONTAINER
    EXPORTING
      CONTAINER_NAME = 'EXCEL_BUG'. "Custom Control Name

  CALL METHOD G_CONTROL->INIT_CONTROL
    EXPORTING
      R3_APPLICATION_NAME      = 'EXCEL INPLACE BDS'
      INPLACE_ENABLED          = ABAP_TRUE
      INPLACE_SCROLL_DOCUMENTS = ABAP_TRUE
      PARENT                   = G_CONTAINER
    IMPORTING
      RETCODE                  = G_RETCODE.
  IF G_RETCODE NE C_OI_ERRORS=>RET_OK.
    CALL METHOD C_OI_ERRORS=>RAISE_MESSAGE
      EXPORTING
        TYPE = 'E'.
  ENDIF.

* Create object for cl_bds_document_set
  CREATE OBJECT G_DOCUMENT.

* Get Document info
  CLEAR:WA_L_SIGNATURE.
  WA_L_SIGNATURE-PROP_NAME  = 'DESCRIPTION'.
  "Description of the table template in OAOR
*  wa_l_signature-prop_value = c_desc.
  APPEND WA_L_SIGNATURE TO IT_L_SIGNATURE.

  CALL METHOD G_DOCUMENT->GET_WITH_URL
    EXPORTING
      CLASSNAME       = C_CLSSNM
      CLASSTYPE       = C_CLSTYP
      OBJECT_KEY      = C_OBJKEY
    CHANGING
      URIS            = IT_L_URIS
                                                              "signature       = it_l_signature
    EXCEPTIONS
      NOTHING_FOUND   = 1
      ERROR_KPRO      = 2
      INTERNAL_ERROR  = 3
      PARAMETER_ERROR = 4
      NOT_AUTHORIZED  = 5
      NOT_ALLOWED     = 6.
  IF SY-SUBRC NE 0.
    MESSAGE E002(ZFICO_MSG).
  ENDIF.

  READ TABLE IT_L_URIS INTO WA_L_URIS INDEX 2.  "mod by HANDWJ 20181217--
  CALL METHOD G_CONTROL->GET_DOCUMENT_PROXY
    EXPORTING
      DOCUMENT_TYPE  = 'Excel.Sheet'
    IMPORTING
      DOCUMENT_PROXY = G_PROXY
      RETCODE        = G_RETCODE.
  IF G_RETCODE NE C_OI_ERRORS=>RET_OK.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE
      EXPORTING
        TYPE = 'E'.
  ENDIF.

* Open Document
  CALL METHOD G_PROXY->OPEN_DOCUMENT
    EXPORTING
      DOCUMENT_URL     = WA_L_URIS-URI
      OPEN_INPLACE     = ABAP_TRUE
      PROTECT_DOCUMENT = ABAP_FALSE"Protect Document initially
      OPEN_READONLY    = ABAP_FALSE
    IMPORTING
      RETCODE          = G_RETCODE.
  IF G_RETCODE NE C_OI_ERRORS=>RET_OK.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE
      EXPORTING
        TYPE = 'E'.
  ENDIF.

* Get Excel Interface
  CALL METHOD G_PROXY->GET_SPREADSHEET_INTERFACE
    IMPORTING
      SHEET_INTERFACE = G_EXCEL
      RETCODE         = G_RETCODE.

  IF G_RETCODE NE C_OI_ERRORS=>RET_OK.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE
      EXPORTING
        TYPE = 'E'.
  ENDIF.

ENDFORM.                    " FRM_GET_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BUILD_DOCUMENT .

  CLEAR:G_EXCEL_LINE.
* EXCEL Part 1: Header
  PERFORM FRM_FILL_EXCEL_HEADER USING SPACE.

* EXCEL Part 2: 初级分摊费用 & 次级分摊费用
  PERFORM FRM_FILL_EXCEL_DATA1.

* EXCEL Part 3: 次级分摊费用 - 移至 EXCEL Part 2
  "PERFORM frm_fill_excel_data2.

ENDFORM.                    " FRM_BUILD_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_EXCEL_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_FILL_EXCEL_HEADER USING VALUE(P_KHINR).

  DATA:
    I_START     TYPE I,
    WA_L_EXCEL4 TYPE T_EXCEL4,
    IT_L_EXCEL4 TYPE STANDARD TABLE OF T_EXCEL4,
    WA_L_SOI_CELL_ITEM  TYPE SOI_CELL_ITEM,
    IT_L_SOI_CELL_ITEM  TYPE STANDARD TABLE OF SOI_CELL_ITEM.

  CLEAR:WA_L_EXCEL4.
  CONCATENATE P_GJAHR '年' P_MONTH '月'
              TEXT-T01
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T02 SPACE
              P_GJAHR '-' P_MONTH
              INTO  WA_L_EXCEL4-COL1.
  APPEND WA_L_EXCEL4 TO IT_L_EXCEL4.
  CLEAR:WA_L_EXCEL4.
  "add by HANDWJ 20181219 begin--
  IF P_KHINR IS NOT INITIAL.
    WA_L_EXCEL4-COL1 = TEXT-T62.
    WA_L_EXCEL4-COL2 = P_KHINR.
  ELSE.
  "add by HANDWJ 20181219 end--
    WA_L_EXCEL4-COL1 = TEXT-T03.
    WA_L_EXCEL4-COL2 = S_ABTEI-LOW.
  ENDIF.
  APPEND WA_L_EXCEL4 TO IT_L_EXCEL4.
  CLEAR:WA_L_EXCEL4.
  WA_L_EXCEL4-COL1 = TEXT-T04.
  WA_L_EXCEL4-COL2 = S_KOSTL-LOW.
  WA_L_EXCEL4-COL3 = TEXT-T05.
  WA_L_EXCEL4-COL4 = S_KOSTL-HIGH.
  APPEND WA_L_EXCEL4 TO IT_L_EXCEL4.
  CLEAR:WA_L_EXCEL4.
  WA_L_EXCEL4-COL1 = TEXT-T06.
  WA_L_EXCEL4-COL2 = S_PRCTR-LOW.
  WA_L_EXCEL4-COL3 = TEXT-T05.
  WA_L_EXCEL4-COL4 = S_PRCTR-HIGH.
  APPEND WA_L_EXCEL4 TO IT_L_EXCEL4.
  CLEAR:WA_L_EXCEL4.
  WA_L_EXCEL4-COL1 = TEXT-T07.
  WA_L_EXCEL4-COL2 = S_FUNCA-LOW.
  WA_L_EXCEL4-COL3 = TEXT-T05.
  WA_L_EXCEL4-COL4 = S_FUNCA-HIGH.
  APPEND WA_L_EXCEL4 TO IT_L_EXCEL4.
  CLEAR:WA_L_EXCEL4.
  WA_L_EXCEL4-COL1 = TEXT-T08.
  "mod by HANDWJ 20181219 begin--
  "old
*  CONCATENATE P_GJAHR '年'
*              P_MONTH '月'
*              INTO WA_L_EXCEL4-COL2.
  "new
  WA_L_EXCEL4-COL2 = P_MONTH.
  "mod by HANDWJ 20181219 end--
  APPEND WA_L_EXCEL4 TO IT_L_EXCEL4.
  CLEAR:WA_L_EXCEL4.
  WA_L_EXCEL4-COL1 = TEXT-T09.
  WA_L_EXCEL4-COL2 = P_VERSN.
  APPEND WA_L_EXCEL4 TO IT_L_EXCEL4.

  "添加
  I_START = G_EXCEL_LINE + 1.
  PERFORM FRM_FILL_EXCEL_CELL2
                               TABLES IT_L_EXCEL4
                               USING  I_START        "Begin
                                      1         "Begin
                                      7             "No of rows reqd
                                      4             "No of cols reqd
                                      'HEAD'.       "Range name
  G_EXCEL_LINE = G_EXCEL_LINE + 7.

*-- Header Format
  CLEAR:WA_L_SOI_CELL_ITEM.
  WA_L_SOI_CELL_ITEM-TOP = 2.
  WA_L_SOI_CELL_ITEM-LEFT = 2.
  WA_L_SOI_CELL_ITEM-ROWS = 1.
  WA_L_SOI_CELL_ITEM-COLUMNS = 1.
  WA_L_SOI_CELL_ITEM-FRAMETYP = 4.  "1左；2-上；3-左上；4-下；5-左下；6-上下；7-左上下；8-右；9-左右
  "10-右上；11-左上右；12-右下；13-左下右；14-上右下；15-上下左右
  "16-空;17-左。。。
  APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.
  CLEAR:WA_L_SOI_CELL_ITEM.
  WA_L_SOI_CELL_ITEM-TOP = 3.
  WA_L_SOI_CELL_ITEM-LEFT = 2.
  WA_L_SOI_CELL_ITEM-ROWS = 1.
  WA_L_SOI_CELL_ITEM-COLUMNS = 1.
  WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
  APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.
  CLEAR:WA_L_SOI_CELL_ITEM.
  WA_L_SOI_CELL_ITEM-TOP = 4.
  WA_L_SOI_CELL_ITEM-LEFT = 2.
  WA_L_SOI_CELL_ITEM-ROWS = 1.
  WA_L_SOI_CELL_ITEM-COLUMNS = 1.
  WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
  APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.
  CLEAR:WA_L_SOI_CELL_ITEM.
  WA_L_SOI_CELL_ITEM-TOP = 5.
  WA_L_SOI_CELL_ITEM-LEFT = 2.
  WA_L_SOI_CELL_ITEM-ROWS = 1.
  WA_L_SOI_CELL_ITEM-COLUMNS = 1.
  WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
  APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.
  CLEAR:WA_L_SOI_CELL_ITEM.
  WA_L_SOI_CELL_ITEM-TOP = 6.
  WA_L_SOI_CELL_ITEM-LEFT = 2.
  WA_L_SOI_CELL_ITEM-ROWS = 1.
  WA_L_SOI_CELL_ITEM-COLUMNS = 1.
  WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
  APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.
  CLEAR:WA_L_SOI_CELL_ITEM.
  WA_L_SOI_CELL_ITEM-TOP = 7.
  WA_L_SOI_CELL_ITEM-LEFT = 2.
  WA_L_SOI_CELL_ITEM-ROWS = 1.
  WA_L_SOI_CELL_ITEM-COLUMNS = 1.
  WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
  APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.
  CLEAR:WA_L_SOI_CELL_ITEM.
  WA_L_SOI_CELL_ITEM-TOP = 3.
  WA_L_SOI_CELL_ITEM-LEFT = 4.
  WA_L_SOI_CELL_ITEM-ROWS = 1.
  WA_L_SOI_CELL_ITEM-COLUMNS = 1.
  WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
  APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.
  CLEAR:WA_L_SOI_CELL_ITEM.
  WA_L_SOI_CELL_ITEM-TOP = 4.
  WA_L_SOI_CELL_ITEM-LEFT = 4.
  WA_L_SOI_CELL_ITEM-ROWS = 1.
  WA_L_SOI_CELL_ITEM-COLUMNS = 1.
  WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
  APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.
  CLEAR:WA_L_SOI_CELL_ITEM.
  WA_L_SOI_CELL_ITEM-TOP = 5.
  WA_L_SOI_CELL_ITEM-LEFT = 4.
  WA_L_SOI_CELL_ITEM-ROWS = 1.
  WA_L_SOI_CELL_ITEM-COLUMNS = 1.
  WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
  APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.
  CLEAR:WA_L_SOI_CELL_ITEM.
  WA_L_SOI_CELL_ITEM-TOP = 6.
  WA_L_SOI_CELL_ITEM-LEFT = 4.
  WA_L_SOI_CELL_ITEM-ROWS = 1.
  WA_L_SOI_CELL_ITEM-COLUMNS = 1.
  WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
  APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.
  CLEAR:WA_L_SOI_CELL_ITEM.
  WA_L_SOI_CELL_ITEM-TOP = 7.
  WA_L_SOI_CELL_ITEM-LEFT = 4.
  WA_L_SOI_CELL_ITEM-ROWS = 1.
  WA_L_SOI_CELL_ITEM-COLUMNS = 1.
  WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
  APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.
  CALL METHOD G_EXCEL->CELL_FORMAT
    EXPORTING
*     no_flush = ''
      CELLS    = IT_L_SOI_CELL_ITEM
    IMPORTING
      ERROR    = G_ERROR
      RETCODE  = G_RETCODE.


ENDFORM.                    " FRM_FILL_EXCEL_HEADER
*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_EXCEL_CELL2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_1      text
*      -->P_7      text
*      -->P_4      text
*      -->P_1703   text
*      -->P_IT_L_EXCEL4  text
*----------------------------------------------------------------------*
FORM FRM_FILL_EXCEL_CELL2
                     TABLES PT_DATA   TYPE STANDARD TABLE
                     USING   PR_TOP    TYPE I
                            PR_LEFT   TYPE I
                            PR_ROW    TYPE I
                            PR_COLUMN TYPE I
                            PR_RANGE  TYPE CHAR255.

  DATA: IT_L_FIELDS TYPE TABLE OF RFC_FIELDS.

*--创建range
  PERFORM FRM_CREATE_RANGE USING PR_TOP             "Begin
                                 PR_LEFT             "Begin
                                 PR_ROW             "No of rows reqd
                                 PR_COLUMN             "No of cols reqd
                                 PR_RANGE.       "Range name

*--填充range
  REFRESH IT_L_FIELDS.
  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      DATA             = PT_DATA[]
      FIELDS           = IT_L_FIELDS[]
    EXCEPTIONS
      DP_INVALID_TABLE = 1
      OTHERS           = 2.
  CALL METHOD G_EXCEL->INSERT_ONE_TABLE
    EXPORTING
      FIELDS_TABLE = IT_L_FIELDS[]       "Defn of fields
      DATA_TABLE   = PT_DATA[]     "Data
      RANGENAME    = PR_RANGE         "Range Name
    IMPORTING
      ERROR        = G_ERROR
      RETCODE      = G_RETCODE.


ENDFORM.                    " FRM_FILL_EXCEL_CELL2
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PR_TOP  text
*      -->P_PR_LEFT  text
*      -->P_PR_ROW  text
*      -->P_PR_COLUMN  text
*      -->P_PR_RANGE  text
*----------------------------------------------------------------------*
FORM FRM_CREATE_RANGE  USING  PR_TOP    TYPE I
                            PR_LEFT   TYPE I
                            PR_ROW    TYPE I
                            PR_COLUMN TYPE I
                            PR_RANGE  TYPE CHAR255.

* Select area for entries to be displayed
  CALL METHOD G_EXCEL->SET_SELECTION
    EXPORTING
      TOP     = PR_TOP
      LEFT    = PR_LEFT
      ROWS    = PR_ROW
      COLUMNS = PR_COLUMN.

* Define Range
  CALL METHOD G_EXCEL->INSERT_RANGE
    EXPORTING
      NAME    = PR_RANGE
      ROWS    = PR_ROW
      COLUMNS = PR_COLUMN
    IMPORTING
      ERROR   = G_ERROR.

  IF G_ERROR->HAS_FAILED = ABAP_TRUE.
    CALL METHOD G_ERROR->RAISE_MESSAGE
      EXPORTING
        TYPE = 'E'.
  ENDIF.

ENDFORM.                    " frm_CREATE_RANGE
*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_EXCEL_DATA1
*&---------------------------------------------------------------------*
*       初级分摊费用 & 次级分摊费用
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_FILL_EXCEL_DATA1 .

  DATA:
    WA_L_EXCEL1     TYPE T_EXCEL1,
    IT_L_EXCEL1     TYPE STANDARD TABLE OF T_EXCEL1,
    I_NUM           TYPE I,
    WA_L_DATA1      TYPE T_DATA1,
    WA_L_DATA2      TYPE T_DATA2,
    WA_L_DATA1_SUB  TYPE T_DATA1,
    WA_L_DATA1_SUB2  TYPE T_DATA1.
*  hp_dxj 20160106 add
  DATA: LV_T31    TYPE STRING ,
        LV_T33    TYPE STRING ,
        LV_YEAR   TYPE GJAHR ,
        LV_YEAR_C TYPE C LENGTH 2 .  " hp_dxj 20160106 add

  CLEAR:WA_L_DATA1_SUB2.

*-->1. 初级分摊费用
  CLEAR:WA_L_EXCEL1.
  CONCATENATE TEXT-T10
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T21
              INTO   WA_L_EXCEL1-COL1.
  APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.
  "添加
  G_EXCEL_LINE = G_EXCEL_LINE + 2.
  PERFORM FRM_FILL_EXCEL_CELL2
                               TABLES IT_L_EXCEL1
                               USING  G_EXCEL_LINE   "Begin
                                      1         "Begin
                                      1             "No of rows reqd
                                      19            "No of cols reqd
                                      'D1H1'.       "Range name
  G_EXCEL_LINE = G_EXCEL_LINE + 1.

  REFRESH:IT_L_EXCEL1.
  CLEAR:WA_L_EXCEL1.
  CONCATENATE TEXT-T11
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T12
              INTO   WA_L_EXCEL1-COL1.
  CONCATENATE TEXT-T13
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T14
              INTO   WA_L_EXCEL1-COL2.
  CONCATENATE TEXT-T15
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T16
              INTO   WA_L_EXCEL1-COL3.
  CONCATENATE TEXT-T17
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T18
              INTO   WA_L_EXCEL1-COL4.
  CONCATENATE TEXT-T19
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T20
              INTO   WA_L_EXCEL1-COL5.
  CONCATENATE TEXT-T22
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T23
              INTO   WA_L_EXCEL1-COL6.
  CONCATENATE TEXT-T24
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T25
              INTO   WA_L_EXCEL1-COL7.
  CONCATENATE TEXT-T26
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T27
              INTO   WA_L_EXCEL1-COL8.
  CONCATENATE P_GJAHR
              TEXT-T28
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T29 SPACE P_GJAHR
              INTO   WA_L_EXCEL1-COL9.
*  hp_dxj 20160106 add
  IF P_VERSN = 0 .
    LV_YEAR = P_GJAHR - 1 .
    LV_YEAR_C = LV_YEAR+2(2).
    CONCATENATE 'BP11.' LV_YEAR_C INTO LV_T31 .
    CONCATENATE 'BP11.' LV_YEAR_C INTO LV_T33 .
*    text-t31 = '' .
  ELSE.
    LV_YEAR_C = P_GJAHR+2(2).
    CONCATENATE 'CF02.' LV_YEAR_C INTO LV_T31 .
    CONCATENATE 'CF02.' LV_YEAR_C INTO LV_T33 .
  ENDIF.

  CONCATENATE TEXT-T30
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              LV_T31                " hp_dxj 20160106 add
*              text-t31             " hp_dxj 20160106 marked
              INTO   WA_L_EXCEL1-COL10.
  CONCATENATE TEXT-T32
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              LV_T33              " hp_dxj 20160106 add
*              text-t33           " hp_dxj 20160106 marked
              INTO   WA_L_EXCEL1-COL11.
  CONCATENATE P_MONTH TEXT-T34
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T35 P_MONTH
              INTO   WA_L_EXCEL1-COL12.
  CONCATENATE P_MONTH TEXT-T36
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T37 P_MONTH
              INTO   WA_L_EXCEL1-COL13.
  CONCATENATE P_MONTH TEXT-T38
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T39 P_MONTH
              INTO   WA_L_EXCEL1-COL14.
  CONCATENATE P_GJAHR TEXT-T40
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T41
              INTO   WA_L_EXCEL1-COL15.
  CONCATENATE P_GJAHR TEXT-T42
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T43
              INTO   WA_L_EXCEL1-COL16.
  CONCATENATE P_GJAHR TEXT-T44
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T45
              INTO   WA_L_EXCEL1-COL17.
  CONCATENATE TEXT-T46
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T47
              INTO   WA_L_EXCEL1-COL18.
  CONCATENATE TEXT-T49
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T50
              INTO   WA_L_EXCEL1-COL19.
  APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.
  "添加
  PERFORM FRM_FILL_EXCEL_CELL2
                               TABLES IT_L_EXCEL1
                               USING  G_EXCEL_LINE   "Begin
                                      1         "Begin
                                      1             "No of rows reqd
                                      19            "No of cols reqd
                                      'D1H2'.       "Range name
  "Excel format
  PERFORM FRM_SET_COLUMN_HEAD USING G_EXCEL_LINE  '1'.

  G_EXCEL_LINE = G_EXCEL_LINE + 1.

  "数据
  CLEAR:I_NUM.
  REFRESH:IT_L_EXCEL1.
  CLEAR:WA_L_DATA1_SUB.
  LOOP AT IT_DATA1 INTO WA_L_DATA1.
    I_NUM = I_NUM + 1.

    CLEAR:WA_L_EXCEL1.
    WRITE:WA_L_DATA1-ABTEI TO  WA_L_EXCEL1-COL1 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-KOSTL TO  WA_L_EXCEL1-COL2 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-KSTAR TO  WA_L_EXCEL1-COL3 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-TXT50_EN TO  WA_L_EXCEL1-COL4 LEFT-JUSTIFIED NO-GROUPING,
          "wa_l_data1-KTEXT_EN to  wa_l_excel1-COL4 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-TXT50_ZH TO  WA_L_EXCEL1-COL5 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-SUBTOL_1 TO  WA_L_EXCEL1-COL6 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_1 TO  WA_L_EXCEL1-COL7 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_2 TO  WA_L_EXCEL1-COL8 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-SUBTOL_2 TO  WA_L_EXCEL1-COL9 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_3 TO  WA_L_EXCEL1-COL10 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_4 TO  WA_L_EXCEL1-COL11 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-SUBTOL_3 TO  WA_L_EXCEL1-COL12 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_5 TO  WA_L_EXCEL1-COL13 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_6 TO  WA_L_EXCEL1-COL14 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-SUBTOL_4 TO  WA_L_EXCEL1-COL15 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_7 TO  WA_L_EXCEL1-COL16 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_8 TO  WA_L_EXCEL1-COL17 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_10 TO  WA_L_EXCEL1-COL19 LEFT-JUSTIFIED NO-GROUPING.

    WRITE WA_L_DATA1-WKGXXX_9 TO WA_L_EXCEL1-COL18 LEFT-JUSTIFIED NO-GROUPING.
    CONCATENATE WA_L_EXCEL1-COL18 '%' INTO WA_L_EXCEL1-COL18.

    "负号提前
    "PERFORM frm_sign_front1 CHANGING: wa_l_excel1.
    CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
      CHANGING:
        VALUE         = WA_L_EXCEL1-COL6,
        VALUE         = WA_L_EXCEL1-COL7,
        VALUE         = WA_L_EXCEL1-COL8,
        VALUE         = WA_L_EXCEL1-COL9,
        VALUE         = WA_L_EXCEL1-COL10,
        VALUE         = WA_L_EXCEL1-COL11,
        VALUE         = WA_L_EXCEL1-COL12,
        VALUE         = WA_L_EXCEL1-COL13,
        VALUE         = WA_L_EXCEL1-COL14,
        VALUE         = WA_L_EXCEL1-COL15,
        VALUE         = WA_L_EXCEL1-COL16,
        VALUE         = WA_L_EXCEL1-COL17,
        VALUE         = WA_L_EXCEL1-COL18,
        VALUE         = WA_L_EXCEL1-COL19.

    APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.

    "合计
    WA_L_DATA1_SUB-SUBTOL_1 = WA_L_DATA1_SUB-SUBTOL_1
                            + WA_L_DATA1-SUBTOL_1.
    WA_L_DATA1_SUB-WKGXXX_1 = WA_L_DATA1_SUB-WKGXXX_1
                            + WA_L_DATA1-WKGXXX_1.
    WA_L_DATA1_SUB-WKGXXX_2 = WA_L_DATA1_SUB-WKGXXX_2
                            + WA_L_DATA1-WKGXXX_2.
    WA_L_DATA1_SUB-SUBTOL_2 = WA_L_DATA1_SUB-SUBTOL_2
                            + WA_L_DATA1-SUBTOL_2.
    WA_L_DATA1_SUB-WKGXXX_3 = WA_L_DATA1_SUB-WKGXXX_3
                            + WA_L_DATA1-WKGXXX_3.
    WA_L_DATA1_SUB-WKGXXX_4 = WA_L_DATA1_SUB-WKGXXX_4
                            + WA_L_DATA1-WKGXXX_4.
    WA_L_DATA1_SUB-SUBTOL_3 = WA_L_DATA1_SUB-SUBTOL_3
                            + WA_L_DATA1-SUBTOL_3.
    WA_L_DATA1_SUB-WKGXXX_5 = WA_L_DATA1_SUB-WKGXXX_5
                            + WA_L_DATA1-WKGXXX_5.
    WA_L_DATA1_SUB-WKGXXX_6 = WA_L_DATA1_SUB-WKGXXX_6
                            + WA_L_DATA1-WKGXXX_6.
    WA_L_DATA1_SUB-SUBTOL_4 = WA_L_DATA1_SUB-SUBTOL_4
                            + WA_L_DATA1-SUBTOL_4.
    WA_L_DATA1_SUB-WKGXXX_7 = WA_L_DATA1_SUB-WKGXXX_7
                            + WA_L_DATA1-WKGXXX_7.
    WA_L_DATA1_SUB-WKGXXX_8 = WA_L_DATA1_SUB-WKGXXX_8
                            + WA_L_DATA1-WKGXXX_8.
  ENDLOOP.
  CLEAR:WA_L_EXCEL1.
  CONCATENATE TEXT-T51
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T52
              INTO   WA_L_EXCEL1-COL5.
  CATCH SYSTEM-EXCEPTIONS OTHERS = 8.
    IF WA_L_DATA1_SUB-SUBTOL_2 IS NOT INITIAL.
      WA_L_DATA1_SUB-WKGXXX_9 = WA_L_DATA1_SUB-SUBTOL_4 / WA_L_DATA1_SUB-SUBTOL_2 * 100.
    ELSE.
      WA_L_DATA1_SUB-WKGXXX_9 = 0.
    ENDIF.
  ENDCATCH.
  WA_L_DATA1_SUB-WKGXXX_10 =  WA_L_DATA1_SUB-SUBTOL_2 - WA_L_DATA1_SUB-SUBTOL_4.
  WRITE:WA_L_DATA1_SUB-SUBTOL_1 TO  WA_L_EXCEL1-COL6 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_1 TO  WA_L_EXCEL1-COL7 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_2 TO  WA_L_EXCEL1-COL8 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-SUBTOL_2 TO  WA_L_EXCEL1-COL9 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_3 TO  WA_L_EXCEL1-COL10 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_4 TO  WA_L_EXCEL1-COL11 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-SUBTOL_3 TO  WA_L_EXCEL1-COL12 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_5 TO  WA_L_EXCEL1-COL13 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_6 TO  WA_L_EXCEL1-COL14 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-SUBTOL_4 TO  WA_L_EXCEL1-COL15 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_7 TO  WA_L_EXCEL1-COL16 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_8 TO  WA_L_EXCEL1-COL17 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_10 TO  WA_L_EXCEL1-COL19 LEFT-JUSTIFIED NO-GROUPING.
  WRITE WA_L_DATA1_SUB-WKGXXX_9 TO WA_L_EXCEL1-COL18 LEFT-JUSTIFIED NO-GROUPING.
  CONCATENATE WA_L_EXCEL1-COL18 '%' INTO WA_L_EXCEL1-COL18.
  "负号提前
  "PERFORM frm_sign_front1 CHANGING wa_l_excel1.
  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING:
      VALUE         = WA_L_EXCEL1-COL6,
      VALUE         = WA_L_EXCEL1-COL7,
      VALUE         = WA_L_EXCEL1-COL8,
      VALUE         = WA_L_EXCEL1-COL9,
      VALUE         = WA_L_EXCEL1-COL10,
      VALUE         = WA_L_EXCEL1-COL11,
      VALUE         = WA_L_EXCEL1-COL12,
      VALUE         = WA_L_EXCEL1-COL13,
      VALUE         = WA_L_EXCEL1-COL14,
      VALUE         = WA_L_EXCEL1-COL15,
      VALUE         = WA_L_EXCEL1-COL16,
      VALUE         = WA_L_EXCEL1-COL17,
      VALUE         = WA_L_EXCEL1-COL18,
      VALUE         = WA_L_EXCEL1-COL19.
  APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.
  I_NUM = I_NUM + 1.
  "添加
  PERFORM FRM_FILL_EXCEL_CELL2
                               TABLES IT_L_EXCEL1
                               USING  G_EXCEL_LINE   "Begin
                                      1         "Begin
                                      I_NUM             "No of rows reqd
                                      19            "No of cols reqd
                                      'DATA1'.       "Range name

  "Excel Line
  G_EXCEL_LINE = G_EXCEL_LINE + I_NUM.

  "Excel format
  PERFORM FRM_SET_COLUMN_HEAD USING G_EXCEL_LINE  '0'.

  "总额
  WA_L_DATA1_SUB2-SUBTOL_1 = WA_L_DATA1_SUB2-SUBTOL_1
                          + WA_L_DATA1_SUB-SUBTOL_1.
  WA_L_DATA1_SUB2-WKGXXX_1 = WA_L_DATA1_SUB2-WKGXXX_1
                          + WA_L_DATA1_SUB-WKGXXX_1.
  WA_L_DATA1_SUB2-WKGXXX_2 = WA_L_DATA1_SUB2-WKGXXX_2
                          + WA_L_DATA1_SUB-WKGXXX_2.
  WA_L_DATA1_SUB2-SUBTOL_2 = WA_L_DATA1_SUB2-SUBTOL_2
                          + WA_L_DATA1_SUB-SUBTOL_2.
  WA_L_DATA1_SUB2-WKGXXX_3 = WA_L_DATA1_SUB2-WKGXXX_3
                          + WA_L_DATA1_SUB-WKGXXX_3.
  WA_L_DATA1_SUB2-WKGXXX_4 = WA_L_DATA1_SUB2-WKGXXX_4
                          + WA_L_DATA1_SUB-WKGXXX_4.
  WA_L_DATA1_SUB2-SUBTOL_3 = WA_L_DATA1_SUB2-SUBTOL_3
                          + WA_L_DATA1_SUB-SUBTOL_3.
  WA_L_DATA1_SUB2-WKGXXX_5 = WA_L_DATA1_SUB2-WKGXXX_5
                          + WA_L_DATA1_SUB-WKGXXX_5.
  WA_L_DATA1_SUB2-WKGXXX_6 = WA_L_DATA1_SUB2-WKGXXX_6
                          + WA_L_DATA1_SUB-WKGXXX_6.
  WA_L_DATA1_SUB2-SUBTOL_4 = WA_L_DATA1_SUB2-SUBTOL_4
                          + WA_L_DATA1_SUB-SUBTOL_4.
  WA_L_DATA1_SUB2-WKGXXX_7 = WA_L_DATA1_SUB2-WKGXXX_7
                          + WA_L_DATA1_SUB-WKGXXX_7.
  WA_L_DATA1_SUB2-WKGXXX_8 = WA_L_DATA1_SUB2-WKGXXX_8
                          + WA_L_DATA1_SUB-WKGXXX_8.

*-->2. 次级分摊费用
  CLEAR:I_NUM.
  REFRESH:IT_L_EXCEL1.
  CLEAR:WA_L_EXCEL1.
  CONCATENATE TEXT-T60
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T61
              INTO   WA_L_EXCEL1-COL1.
  APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.
  "添加
  G_EXCEL_LINE = G_EXCEL_LINE + 2.
  PERFORM FRM_FILL_EXCEL_CELL2
                               TABLES IT_L_EXCEL1
                               USING  G_EXCEL_LINE   "Begin
                                      1         "Begin
                                      1             "No of rows reqd
                                      19            "No of cols reqd
                                      'D2H1'.       "Range name
  G_EXCEL_LINE = G_EXCEL_LINE + 1.

  REFRESH:IT_L_EXCEL1.
  CLEAR:WA_L_EXCEL1.
  CONCATENATE TEXT-T11
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T12
              INTO   WA_L_EXCEL1-COL1.
  CONCATENATE TEXT-T13
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T14
              INTO   WA_L_EXCEL1-COL2.
  CONCATENATE TEXT-T15
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T16
              INTO   WA_L_EXCEL1-COL3.
  CONCATENATE TEXT-T17
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T18
              INTO   WA_L_EXCEL1-COL4.
  CONCATENATE TEXT-T19
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T20
              INTO   WA_L_EXCEL1-COL5.
  CONCATENATE TEXT-T22
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T23
              INTO   WA_L_EXCEL1-COL6.
  CONCATENATE TEXT-T24
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T25
              INTO   WA_L_EXCEL1-COL7.
  CONCATENATE TEXT-T55
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T27
              INTO   WA_L_EXCEL1-COL8.
  CONCATENATE P_GJAHR
              TEXT-T28
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T29 SPACE P_GJAHR
              INTO   WA_L_EXCEL1-COL9.
  CONCATENATE TEXT-T30
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              LV_T31
*              text-t31
              INTO   WA_L_EXCEL1-COL10.
  CONCATENATE TEXT-T32
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
               LV_T33
*              text-t33
              INTO   WA_L_EXCEL1-COL11.
  CONCATENATE P_MONTH TEXT-T34
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T35 P_MONTH
              INTO   WA_L_EXCEL1-COL12.
  CONCATENATE P_MONTH TEXT-T36
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T37 P_MONTH
              INTO   WA_L_EXCEL1-COL13.
  CONCATENATE P_MONTH TEXT-T56
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T39 P_MONTH
              INTO   WA_L_EXCEL1-COL14.
  CONCATENATE P_GJAHR TEXT-T40
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T41
              INTO   WA_L_EXCEL1-COL15.
  CONCATENATE P_GJAHR TEXT-T42
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T43
              INTO   WA_L_EXCEL1-COL16.
  CONCATENATE P_GJAHR TEXT-T57
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T45
              INTO   WA_L_EXCEL1-COL17.
  CONCATENATE TEXT-T46
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T47
              INTO   WA_L_EXCEL1-COL18.
  CONCATENATE TEXT-T49
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T50
              INTO   WA_L_EXCEL1-COL19.
  APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.
  "添加
  PERFORM FRM_FILL_EXCEL_CELL2
                               TABLES IT_L_EXCEL1
                               USING  G_EXCEL_LINE   "Begin
                                      1         "Begin
                                      1             "No of rows reqd
                                      19            "No of cols reqd
                                      'D2H2'.       "Range name
  "Excel format
  PERFORM FRM_SET_COLUMN_HEAD USING G_EXCEL_LINE  '1'.

  G_EXCEL_LINE = G_EXCEL_LINE + 1.

  "数据
  CLEAR:I_NUM.
  REFRESH:IT_L_EXCEL1.
  CLEAR:WA_L_DATA1_SUB.
  LOOP AT IT_DATA2 INTO WA_L_DATA2.
    I_NUM = I_NUM + 1.

    CLEAR:WA_L_EXCEL1.
    WRITE:WA_L_DATA2-ABTEI TO  WA_L_EXCEL1-COL1 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-KOSTL TO  WA_L_EXCEL1-COL2 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-KSTAR TO  WA_L_EXCEL1-COL3 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-TXT50_EN TO  WA_L_EXCEL1-COL4 LEFT-JUSTIFIED NO-GROUPING,
          "wa_l_data2-ktext_en to  wa_l_excel1-col4 left-justified no-grouping,
          WA_L_DATA2-TXT50_ZH TO  WA_L_EXCEL1-COL5 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-WKGXXX_1 TO  WA_L_EXCEL1-COL8 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-WKGXXX_2 TO  WA_L_EXCEL1-COL14 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-WKGXXX_3 TO  WA_L_EXCEL1-COL17 LEFT-JUSTIFIED NO-GROUPING.
    "负号提前
    "perform frm_sign_front1 changing wa_l_excel1.
    CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
      CHANGING:
        VALUE         = WA_L_EXCEL1-COL8,
        VALUE         = WA_L_EXCEL1-COL14,
        VALUE         = WA_L_EXCEL1-COL17.

    APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.

    "合计
    WA_L_DATA1_SUB-WKGXXX_1 = WA_L_DATA1_SUB-WKGXXX_1
                            + WA_L_DATA2-WKGXXX_1.
    WA_L_DATA1_SUB-WKGXXX_2 = WA_L_DATA1_SUB-WKGXXX_2
                            + WA_L_DATA2-WKGXXX_2.
    WA_L_DATA1_SUB-WKGXXX_3 = WA_L_DATA1_SUB-WKGXXX_3
                            + WA_L_DATA2-WKGXXX_3.
  ENDLOOP.
  CLEAR:WA_L_EXCEL1.
  CONCATENATE TEXT-T53
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T54
              INTO   WA_L_EXCEL1-COL5.
  WRITE:WA_L_DATA1_SUB-WKGXXX_1 TO  WA_L_EXCEL1-COL8 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_2 TO  WA_L_EXCEL1-COL14 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_3 TO  WA_L_EXCEL1-COL17 LEFT-JUSTIFIED NO-GROUPING.
  "负号提前
  "PERFORM frm_sign_front1 CHANGING wa_l_excel1.
  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING:
      VALUE         = WA_L_EXCEL1-COL8,
      VALUE         = WA_L_EXCEL1-COL14,
      VALUE         = WA_L_EXCEL1-COL17.
  APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.
  I_NUM = I_NUM + 1.
  "添加
  PERFORM FRM_FILL_EXCEL_CELL2
                               TABLES IT_L_EXCEL1
                               USING  G_EXCEL_LINE   "Begin
                                      1         "Begin
                                      I_NUM             "No of rows reqd
                                      19           "No of cols reqd
                                      'DATA2'.       "Range name
  "Excel Line
  G_EXCEL_LINE = G_EXCEL_LINE + I_NUM.
  "Excel format
  PERFORM FRM_SET_COLUMN_HEAD USING G_EXCEL_LINE  '0'.

  "总额
  WA_L_DATA1_SUB2-SUBTOL_1 = WA_L_DATA1_SUB2-SUBTOL_1
                          + WA_L_DATA1_SUB-SUBTOL_1.
  WA_L_DATA1_SUB2-WKGXXX_1 = WA_L_DATA1_SUB2-WKGXXX_1
                          + WA_L_DATA1_SUB-WKGXXX_1.
  WA_L_DATA1_SUB2-WKGXXX_2 = WA_L_DATA1_SUB2-WKGXXX_2
                          + WA_L_DATA1_SUB-WKGXXX_2.
  WA_L_DATA1_SUB2-SUBTOL_2 = WA_L_DATA1_SUB2-SUBTOL_2
                          + WA_L_DATA1_SUB-SUBTOL_2.
  WA_L_DATA1_SUB2-WKGXXX_3 = WA_L_DATA1_SUB2-WKGXXX_3
                          + WA_L_DATA1_SUB-WKGXXX_3.
  WA_L_DATA1_SUB2-WKGXXX_4 = WA_L_DATA1_SUB2-WKGXXX_4
                          + WA_L_DATA1_SUB-WKGXXX_4.
  WA_L_DATA1_SUB2-SUBTOL_3 = WA_L_DATA1_SUB2-SUBTOL_3
                          + WA_L_DATA1_SUB-SUBTOL_3.
  WA_L_DATA1_SUB2-WKGXXX_5 = WA_L_DATA1_SUB2-WKGXXX_5
                          + WA_L_DATA1_SUB-WKGXXX_5.
  WA_L_DATA1_SUB2-WKGXXX_6 = WA_L_DATA1_SUB2-WKGXXX_6
                          + WA_L_DATA1_SUB-WKGXXX_6.
  WA_L_DATA1_SUB2-SUBTOL_4 = WA_L_DATA1_SUB2-SUBTOL_4
                          + WA_L_DATA1_SUB-SUBTOL_4.
  WA_L_DATA1_SUB2-WKGXXX_7 = WA_L_DATA1_SUB2-WKGXXX_7
                          + WA_L_DATA1_SUB-WKGXXX_7.
  WA_L_DATA1_SUB2-WKGXXX_8 = WA_L_DATA1_SUB2-WKGXXX_8
                          + WA_L_DATA1_SUB-WKGXXX_8.

*-->3. 总合计


*-->4. Excel Color
  PERFORM FRM_EXCEL_COLOR.


ENDFORM.                    " FRM_FILL_EXCEL_DATA1
*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_EXCEL_DATA2
*&---------------------------------------------------------------------*
*       次级分摊费用
*----------------------------------------------------------------------*
FORM FRM_FILL_EXCEL_DATA2 .

  DATA:
    WA_L_EXCEL2 TYPE T_EXCEL2,
    IT_L_EXCEL2 TYPE STANDARD TABLE OF T_EXCEL2,
    I_START     TYPE I,
    I_NUM       TYPE I,
    WA_L_DATA2  TYPE T_DATA2,
    WA_L_DATA2_SUB  TYPE T_DATA2,
    WA_L_SOI_CELL_ITEM  TYPE SOI_CELL_ITEM,
    IT_L_SOI_CELL_ITEM  TYPE STANDARD TABLE OF SOI_CELL_ITEM.

  CLEAR:WA_L_EXCEL2.
  CONCATENATE TEXT-T43
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T37
              INTO   WA_L_EXCEL2-COL1.
  APPEND WA_L_EXCEL2 TO IT_L_EXCEL2.
  CLEAR:WA_L_EXCEL2.
  CONCATENATE TEXT-T11
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T12
              INTO   WA_L_EXCEL2-COL1.
  CONCATENATE TEXT-T13
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T14
              INTO   WA_L_EXCEL2-COL2.
  CONCATENATE TEXT-T15
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T16
              INTO   WA_L_EXCEL2-COL3.
  CONCATENATE TEXT-T17
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T18
              INTO   WA_L_EXCEL2-COL4.
  CONCATENATE TEXT-T19
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T20
              INTO   WA_L_EXCEL2-COL5.
  CONCATENATE P_GJAHR
              TEXT-T23
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T24
              INTO   WA_L_EXCEL2-COL6.
  CONCATENATE P_GJAHR
              '年'
              P_MONTH
              '月'
              TEXT-T31
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T32
              P_GJAHR
              P_MONTH
              INTO   WA_L_EXCEL2-COL7.
  CONCATENATE P_GJAHR
              TEXT-T35
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T36
              INTO   WA_L_EXCEL2-COL8.
  APPEND WA_L_EXCEL2 TO IT_L_EXCEL2.

  "数据
  I_NUM = 2.
  CLEAR:WA_L_DATA2_SUB.
  LOOP AT IT_DATA2 INTO WA_L_DATA2.
    I_NUM = I_NUM + 1.

    CLEAR:WA_L_EXCEL2.
    WRITE:WA_L_DATA2-ABTEI TO  WA_L_EXCEL2-COL1 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-KOSTL TO  WA_L_EXCEL2-COL2 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-KSTAR TO  WA_L_EXCEL2-COL3 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-KTEXT_EN TO  WA_L_EXCEL2-COL4 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-KTEXT_ZH TO  WA_L_EXCEL2-COL5 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-WKGXXX_1 TO  WA_L_EXCEL2-COL6 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-WKGXXX_2 TO  WA_L_EXCEL2-COL7 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-WKGXXX_3 TO  WA_L_EXCEL2-COL8 LEFT-JUSTIFIED NO-GROUPING.
    "负号提前
    PERFORM FRM_SIGN_FRONT1 CHANGING WA_L_EXCEL2.
    APPEND WA_L_EXCEL2 TO IT_L_EXCEL2.

    "合计
    WA_L_DATA2_SUB-WKGXXX_1 = WA_L_DATA2_SUB-WKGXXX_1
                            + WA_L_DATA2-WKGXXX_1.
    WA_L_DATA2_SUB-WKGXXX_2 = WA_L_DATA2_SUB-WKGXXX_2
                            + WA_L_DATA2-WKGXXX_2.
    WA_L_DATA2_SUB-WKGXXX_3 = WA_L_DATA2_SUB-WKGXXX_3
                            + WA_L_DATA2-WKGXXX_3.
  ENDLOOP.
  CLEAR:WA_L_EXCEL2.
  CONCATENATE TEXT-T45
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T46
              INTO   WA_L_EXCEL2-COL5.
  WRITE:WA_L_DATA2_SUB-WKGXXX_1 TO  WA_L_EXCEL2-COL6 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA2_SUB-WKGXXX_2 TO  WA_L_EXCEL2-COL7 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA2_SUB-WKGXXX_3 TO  WA_L_EXCEL2-COL8 LEFT-JUSTIFIED NO-GROUPING.
  "负号提前
  PERFORM FRM_SIGN_FRONT1 CHANGING WA_L_EXCEL2.
  APPEND WA_L_EXCEL2 TO IT_L_EXCEL2.
  I_NUM = I_NUM + 1.

  "添加
  I_START = G_EXCEL_LINE + 2.
  PERFORM FRM_FILL_EXCEL_CELL2
                               TABLES IT_L_EXCEL2
                               USING  I_START   "Begin
                                      1         "Begin
                                      I_NUM             "No of rows reqd
                                      8           "No of cols reqd
                                      'DATA2'.       "Range name

  "Excel Line
  G_EXCEL_LINE = I_START + I_NUM - 1.

  "Format
  REFRESH:IT_L_SOI_CELL_ITEM.
  CLEAR:WA_L_SOI_CELL_ITEM.
  WA_L_SOI_CELL_ITEM-TOP = I_START.
  WA_L_SOI_CELL_ITEM-LEFT = 1.
  WA_L_SOI_CELL_ITEM-ROWS = 1.
  WA_L_SOI_CELL_ITEM-COLUMNS = 1.
  WA_L_SOI_CELL_ITEM-BOLD = 1.
  WA_L_SOI_CELL_ITEM-FRAMETYP = 4.  "1左；2-上；3-左上；4-下；5-左下；6-上下；7-左上下；8-右；9-左右
  "10-右上；11-左上右；12-右下；13-左下右；14-上右下；15-上下左右
  "16-空;17-左。。。
  APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.
  I_START = I_START + 1.
  CLEAR:WA_L_SOI_CELL_ITEM.
  WA_L_SOI_CELL_ITEM-TOP = I_START.
  WA_L_SOI_CELL_ITEM-LEFT = 1.
  WA_L_SOI_CELL_ITEM-ROWS = 1.
  WA_L_SOI_CELL_ITEM-COLUMNS = 8.
  WA_L_SOI_CELL_ITEM-BOLD = 0.
  WA_L_SOI_CELL_ITEM-FRAMETYP = 6.  "1左；2-上；3-左上；4-下；5-左下；6-上下；7-左上下；8-右；9-左右
  "10-右上；11-左上右；12-右下；13-左下右；14-上右下；15-上下左右
  "16-空;17-左。。。
  APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.

  CALL METHOD G_EXCEL->CELL_FORMAT
    EXPORTING
*     no_flush = ''
      CELLS    = IT_L_SOI_CELL_ITEM
    IMPORTING
      ERROR    = G_ERROR
      RETCODE  = G_RETCODE.



ENDFORM.                    " FRM_FILL_EXCEL_DATA2
*&---------------------------------------------------------------------*
*&      Form  FRM_SIGN_FRONT1
*&---------------------------------------------------------------------*
*       负号提前
*----------------------------------------------------------------------*
*      <--P_WA_L_EXCEL1  text
*----------------------------------------------------------------------*
FORM FRM_SIGN_FRONT1  CHANGING PV_EXCEL1 TYPE ANY.

  DATA:
    W_STRUCT     TYPE  REF TO CL_ABAP_STRUCTDESCR,
    IT_COMP_TAB  TYPE  CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE,
    WA_COMP_TAB  LIKE LINE OF IT_COMP_TAB.
  FIELD-SYMBOLS:<FS_L_ANY> TYPE ANY.

* for table
  W_STRUCT ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA( PV_EXCEL1 ).
  IT_COMP_TAB = W_STRUCT->GET_COMPONENTS( ).

* for each component
  LOOP AT IT_COMP_TAB INTO WA_COMP_TAB
    WHERE NAME NE 'ABTEI'
      AND NAME NE 'KOSTL'
      AND NAME NE 'KSTAR'
      AND NAME NE 'KTEXT_EN'
      AND NAME NE 'KTEXT_ZH'.
    ASSIGN COMPONENT WA_COMP_TAB-NAME OF STRUCTURE PV_EXCEL1
      TO <FS_L_ANY>.
    IF <FS_L_ANY> IS ASSIGNED.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          VALUE = <FS_L_ANY>.
      UNASSIGN <FS_L_ANY>.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " FRM_SIGN_FRONT1
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_COLUMN_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_START  text
*----------------------------------------------------------------------*
FORM FRM_SET_COLUMN_HEAD  USING    PR_START TYPE I
                                   PR_TYP   TYPE  C.

  DATA:
    I_START             TYPE I,
    WA_L_SOI_CELL_ITEM  TYPE SOI_CELL_ITEM,
    IT_L_SOI_CELL_ITEM  TYPE STANDARD TABLE OF SOI_CELL_ITEM,
    I_PROGRESS          TYPE C LENGTH 100.

  CLEAR:I_PROGRESS.
  I_PROGRESS = TEXT-P04.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE = 80
      TEXT       = I_PROGRESS.

  REFRESH:IT_L_SOI_CELL_ITEM.
  CASE PR_TYP.
    WHEN '1'.   "Column heading
      I_START = PR_START - 1.
      CLEAR:WA_L_SOI_CELL_ITEM.
      WA_L_SOI_CELL_ITEM-TOP = I_START.
      WA_L_SOI_CELL_ITEM-LEFT = 1.
      WA_L_SOI_CELL_ITEM-ROWS = 1.
      WA_L_SOI_CELL_ITEM-COLUMNS = 19.
      "wa_l_soi_cell_item-bold = 1.
      WA_L_SOI_CELL_ITEM-FRAMETYP = 0.  "1左；2-上；3-左上；4-下；5-左下；6-上下；7-左上下；8-右；9-左右
      "10-右上；11-左上右；12-右下；13-左下右；14-上右下；15-上下左右
      "16-空;17-左。。。
      APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.

      I_START = PR_START.
      CLEAR:WA_L_SOI_CELL_ITEM.
      WA_L_SOI_CELL_ITEM-TOP = I_START.
      WA_L_SOI_CELL_ITEM-LEFT = 1.
      WA_L_SOI_CELL_ITEM-ROWS = 1.
      WA_L_SOI_CELL_ITEM-COLUMNS = 8.
      WA_L_SOI_CELL_ITEM-BOLD = 0.
      WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
      APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.

      "“年度预算总额”，“当月费用总额”,“累计费用总额”，“预算使用比例“以及”剩余可用预算“几列的名称加粗体
      CLEAR:WA_L_SOI_CELL_ITEM.
      WA_L_SOI_CELL_ITEM-TOP = I_START.
      WA_L_SOI_CELL_ITEM-LEFT = 9.
      WA_L_SOI_CELL_ITEM-ROWS = 1.
      WA_L_SOI_CELL_ITEM-COLUMNS = 1.
      WA_L_SOI_CELL_ITEM-BOLD = 1.
      WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
      APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.

      CLEAR:WA_L_SOI_CELL_ITEM.
      WA_L_SOI_CELL_ITEM-TOP = I_START.
      WA_L_SOI_CELL_ITEM-LEFT = 10.
      WA_L_SOI_CELL_ITEM-ROWS = 1.
      WA_L_SOI_CELL_ITEM-COLUMNS = 2.
      WA_L_SOI_CELL_ITEM-BOLD = 0.
      WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
      APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.

      CLEAR:WA_L_SOI_CELL_ITEM.
      WA_L_SOI_CELL_ITEM-TOP = I_START.
      WA_L_SOI_CELL_ITEM-LEFT = 12.
      WA_L_SOI_CELL_ITEM-ROWS = 1.
      WA_L_SOI_CELL_ITEM-COLUMNS = 1.
      WA_L_SOI_CELL_ITEM-BOLD = 1.
      WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
      APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.

      WA_L_SOI_CELL_ITEM-TOP = I_START.
      WA_L_SOI_CELL_ITEM-LEFT = 13.
      WA_L_SOI_CELL_ITEM-ROWS = 1.
      WA_L_SOI_CELL_ITEM-COLUMNS = 2.
      WA_L_SOI_CELL_ITEM-BOLD = 0.
      WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
      APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.

      CLEAR:WA_L_SOI_CELL_ITEM.
      WA_L_SOI_CELL_ITEM-TOP = I_START.
      WA_L_SOI_CELL_ITEM-LEFT = 15.
      WA_L_SOI_CELL_ITEM-ROWS = 1.
      WA_L_SOI_CELL_ITEM-COLUMNS = 1.
      WA_L_SOI_CELL_ITEM-BOLD = 1.
      WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
      APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.

      WA_L_SOI_CELL_ITEM-TOP = I_START.
      WA_L_SOI_CELL_ITEM-LEFT = 16.
      WA_L_SOI_CELL_ITEM-ROWS = 1.
      WA_L_SOI_CELL_ITEM-COLUMNS = 2.
      WA_L_SOI_CELL_ITEM-BOLD = 0.
      WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
      APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.

      CLEAR:WA_L_SOI_CELL_ITEM.
      WA_L_SOI_CELL_ITEM-TOP = I_START.
      WA_L_SOI_CELL_ITEM-LEFT = 18.
      WA_L_SOI_CELL_ITEM-ROWS = 1.
      WA_L_SOI_CELL_ITEM-COLUMNS = 2.
      WA_L_SOI_CELL_ITEM-BOLD = 1.
      WA_L_SOI_CELL_ITEM-FRAMETYP = 6.
      APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.

    WHEN '2'.
      CLEAR:WA_L_SOI_CELL_ITEM.
      WA_L_SOI_CELL_ITEM-TOP = PR_START - 1.
      WA_L_SOI_CELL_ITEM-LEFT = 1.
      WA_L_SOI_CELL_ITEM-ROWS = 1.
      WA_L_SOI_CELL_ITEM-COLUMNS = 19.
      WA_L_SOI_CELL_ITEM-BOLD = 0.


      WA_L_SOI_CELL_ITEM-FRAMETYP = 2.  "1左；2-上；3-左上；4-下；5-左下；6-上下；7-左上下；8-右；9-左右
      "10-右上；11-左上右；12-右下；13-左下右；14-上右下；15-上下左右
      "16-空;17-左。。。
      APPEND WA_L_SOI_CELL_ITEM TO IT_L_SOI_CELL_ITEM.
    WHEN OTHERS.
      EXIT.
  ENDCASE.


  CALL METHOD G_EXCEL->CELL_FORMAT
    EXPORTING
*     no_flush = ''
      CELLS    = IT_L_SOI_CELL_ITEM
    IMPORTING
      ERROR    = G_ERROR
      RETCODE  = G_RETCODE.


ENDFORM.                    " FRM_SET_COLUMN_HEAD
*&---------------------------------------------------------------------*
*&      Form  FRM_EXCEL_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_EXCEL_COLOR .

* Front    Back     Meaning
* 1        1        前后均黑
* 1        2        字黑背白
* 1        3        字黑背红
* 1        4        字黑背绿
* 1        5        字黑背蓝
* 1        6        字黑背黄
* 1        7        字黑背粉
* 1        8        字黑背浅蓝
* 1        9        字黑背紫红
* 1        10       字黑背浅绿
* 1        11       字黑背深蓝
* 1        13       字黑背黄黑
* 1        14       字黑背紫
* 1        15       字黑浅灰
* 1        16       字黑中灰

*--- Sub head
  CALL METHOD G_EXCEL->SET_COLOR
    EXPORTING
      RANGENAME = 'D1H1'
      FRONT     = 5
      BACK      = 2
    IMPORTING
      ERROR     = G_ERROR
      RETCODE   = G_RETCODE.

  CALL METHOD G_EXCEL->SET_COLOR
    EXPORTING
      RANGENAME = 'D2H1'
      FRONT     = 5
      BACK      = 2
    IMPORTING
      ERROR     = G_ERROR
      RETCODE   = G_RETCODE.

*--- Column head
  CALL METHOD G_EXCEL->SET_COLOR
    EXPORTING
      RANGENAME = 'D1H2'
      FRONT     = 1
      BACK      = 6
    IMPORTING
      ERROR     = G_ERROR
      RETCODE   = G_RETCODE.

  CALL METHOD G_EXCEL->SET_COLOR
    EXPORTING
      RANGENAME = 'D2H2'
      FRONT     = 1
      BACK      = 6
    IMPORTING
      ERROR     = G_ERROR
      RETCODE   = G_RETCODE.



ENDFORM.                    " FRM_EXCEL_COLOR
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_PRINT_DOCUMENT .
* Print document
  IF NOT G_PROXY IS INITIAL.
    CALL METHOD G_PROXY->PRINT_DOCUMENT
      EXPORTING
*       no_flush    = ' '
        PROMPT_USER = 'X'
      IMPORTING
        ERROR       = G_ERROR
        RETCODE     = G_RETCODE.

    IF G_ERROR->HAS_FAILED = ABAP_TRUE.
      CALL METHOD G_ERROR->RAISE_MESSAGE
        EXPORTING
          TYPE = 'E'.
    ENDIF.
  ENDIF.
ENDFORM.                    " FRM_PRINT_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  FRM_F4_ABTEI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_S_ABTEI_LOW  text
*----------------------------------------------------------------------*
FORM FRM_F4_ABTEI  CHANGING C_ABTEI.

  TYPES:BEGIN OF TYP_CSKS,
          KHINR TYPE CSKS-KHINR,
          TEXT  TYPE CHAR200,
          ABTEI TYPE CSKS-ABTEI,
        END OF TYP_CSKS.


  DATA:LT_CSKS TYPE TABLE OF TYP_CSKS WITH HEADER LINE.
  DATA: LIT_RETURN_TAB TYPE STANDARD TABLE OF DDSHRETVAL,
        LWA_RETURN_TAB TYPE DDSHRETVAL,
        L_ZTTYPE0  TYPE DFIES-FIELDNAME VALUE 'ABTEI'.

  SELECT * FROM CSKS
    INTO CORRESPONDING FIELDS OF TABLE LT_CSKS.
  SORT LT_CSKS.
  DELETE ADJACENT DUPLICATES FROM LT_CSKS COMPARING ALL FIELDS.

  DATA:LT_GROUPLIST TYPE TABLE OF BAPI1112_LIST WITH HEADER LINE.
  CALL FUNCTION 'BAPI_COSTCENTERGROUP_GETLIST'
    EXPORTING
      CONTROLLINGAREAMASK = 'ZF00'
    TABLES
      GROUPLIST           = LT_GROUPLIST.

  LOOP AT LT_CSKS.
    READ TABLE LT_GROUPLIST WITH KEY GROUPNAME = LT_CSKS-KHINR.
    IF SY-SUBRC = 0.
      LT_CSKS-TEXT = LT_GROUPLIST-DESCRIPT.
      MODIFY LT_CSKS.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = L_ZTTYPE0
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = LT_CSKS
      RETURN_TAB      = LIT_RETURN_TAB
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.

  READ TABLE LIT_RETURN_TAB INTO LWA_RETURN_TAB INDEX 1.
  C_ABTEI =  LWA_RETURN_TAB-FIELDVAL.


ENDFORM.                    " FRM_F4_ABTEI
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_COSTC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_COSTC .

  DATA: WA_R_CC         LIKE LINE OF IT_R_CC,
        I_SETNR         TYPE  CHAR100,
        WA_L_SET_VALUES TYPE  RGSB4,
        IT_L_SET_VALUES TYPE  STANDARD TABLE OF RGSB4,
        LT_RGSB4        TYPE  STANDARD TABLE OF RGSB4. "add by HANDWJ 20181127--

  REFRESH:IT_R_CC.
  CLEAR:I_SETNR.
  REFRESH IT_L_SET_VALUES[].
  I_SETNR+0(4)   = '0101'.            "Set type:Cost Element grp
  I_SETNR+4(4)   = 'ZF00'.            "Chart of Accounts

  REFRESH IT_KHINR[].
  SELECT *
    INTO TABLE IT_KHINR
    FROM CSKS
  WHERE KHINR IN S_GRPNM
    AND BUKRS = P_BUKRS.

  SORT IT_KHINR BY KHINR.
  IF P_BOX = 'X'.
    DELETE ADJACENT DUPLICATES FROM IT_KHINR COMPARING KHINR.
    REFRESH IT_GRPNM_CSKS[].
    IT_GRPNM_CSKS[] = IT_KHINR[].
  ENDIF.

  IF S_GRPNM[] IS NOT INITIAL.
    SELECT SETNAME
      SUBSETNAME
      INTO TABLE IT_SETNODE
      FROM SETNODE
    WHERE SETNAME IN S_GRPNM.

    LOOP AT IT_SETNODE ASSIGNING <FS_SET>.
      <FS_SET>-KHINR1 = <FS_SET>-SETNAME.
      <FS_SET>-KHINR2 = <FS_SET>-SUBSETNAME.
      READ TABLE IT_GRPNM_CSKS WITH KEY KHINR = <FS_SET>-KHINR2 BINARY SEARCH.
      IF SY-SUBRC = 0.
        DELETE IT_GRPNM_CSKS[] WHERE KHINR = <FS_SET>-KHINR2.
      ENDIF.
    ENDLOOP.

    IF IT_SETNODE[] IS NOT INITIAL.
      SELECT *
        APPENDING TABLE IT_KHINR
        FROM CSKS
        FOR ALL ENTRIES IN IT_SETNODE
      WHERE KHINR = IT_SETNODE-KHINR2.
    ENDIF.
  ENDIF.

  SORT IT_KHINR BY KHINR.
  DELETE ADJACENT DUPLICATES FROM IT_KHINR COMPARING KHINR.

  LOOP AT IT_KHINR.
    I_SETNR+8(10)  = IT_KHINR-KHINR.          "Cost Element grp
    "FM取得
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        SETNR         = I_SETNR
      TABLES
        SET_VALUES    = IT_L_SET_VALUES
      EXCEPTIONS
        SET_NOT_FOUND = 1
        OTHERS        = 2.

    LOOP AT IT_L_SET_VALUES INTO WA_L_SET_VALUES WHERE SHORTNAME = IT_KHINR-KHINR.
      WA_R_CC-SIGN    = 'I'.
      IF WA_L_SET_VALUES-TO = WA_L_SET_VALUES-FROM.
        WA_R_CC-OPTION  = 'EQ'.
        WA_R_CC-LOW     = WA_L_SET_VALUES-TO.
      ELSE.
        WA_R_CC-OPTION  = 'BT'.
        WA_R_CC-LOW     = WA_L_SET_VALUES-TO.
        WA_R_CC-HIGH    = WA_L_SET_VALUES-FROM.
      ENDIF.
      APPEND WA_R_CC TO IT_R_CC.
      CLEAR:WA_L_SET_VALUES,
            WA_R_CC.
    ENDLOOP.

    CLEAR IT_L_SET_VALUES[].
  ENDLOOP.

  DESCRIBE TABLE IT_GRPNM_CSKS LINES G_GRPNM_NUM.

ENDFORM.                    " FRM_GET_COSTC
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_DOCUMENT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_BUILD_DOCUMENT2 .

  DATA:LV_TABIX      TYPE SY-TABIX,
       LC_NAME_OLD   TYPE CHAR20,
       LV_NUMBER     TYPE CHAR2,
       LV_SHEET_NAME TYPE CHAR20.

  LV_TABIX = G_GRPNM_NUM - 1.

  DO LV_TABIX TIMES.
    PERFORM FRM_ADD_SHEET.
  ENDDO.

  LV_TABIX = G_GRPNM_NUM.
  LOOP AT IT_GRPNM_CSKS.
    CLEAR:G_EXCEL_LINE.

    IF SY-TABIX = 1.
      LC_NAME_OLD = 'Sheet1'.
    ELSE.
      CLEAR LV_NUMBER.
      LV_NUMBER = LV_TABIX.
      LV_TABIX = LV_TABIX - 1.
      CONCATENATE 'Sheet1 (' LV_NUMBER ')' INTO LC_NAME_OLD.
    ENDIF.
    LV_SHEET_NAME = IT_GRPNM_CSKS-KHINR.

    PERFORM FRM_CHANGE_SHEETNAME USING LC_NAME_OLD
                                       LV_SHEET_NAME.

* EXCEL Part 1: Header
    PERFORM FRM_FILL_EXCEL_HEADER USING IT_GRPNM_CSKS-KHINR.

* EXCEL Part 2: 初级分摊费用 & 次级分摊费用
    PERFORM FRM_FILL_EXCEL_DATA3 USING IT_GRPNM_CSKS-KHINR.
  ENDLOOP.

ENDFORM.                    " FRM_BUILD_DOCUMENT2
*&---------------------------------------------------------------------*
*&      Form  FRM_ADD_SHEET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_ADD_SHEET .

  CALL METHOD G_PROXY->EXECUTE_MACRO
    EXPORTING
      MACRO_STRING = 'Sheet1.Add_Sheet'
    IMPORTING
      RETCODE      = G_RETCODE.
  IF G_RETCODE NE C_OI_ERRORS=>RET_OK.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE
      EXPORTING
        TYPE = 'E'.
  ENDIF.

ENDFORM.                    " FRM_ADD_SHEET
*&---------------------------------------------------------------------*
*&      Form  FRM_CHANGE_SHEETNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CHANGE_SHEETNAME USING P_OLDNAME TYPE CHAR20
                                P_NEWNAME TYPE CHAR20.

  CALL METHOD G_EXCEL->SET_SHEET_NAME
    EXPORTING
      NEWNAME  = P_NEWNAME
      OLDNAME  = P_OLDNAME
      NO_FLUSH = ' '
    IMPORTING
      ERROR    = G_ERROR
      RETCODE  = G_RETCODE.
  IF G_RETCODE NE C_OI_ERRORS=>RET_OK.
    CALL METHOD G_ERROR->RAISE_MESSAGE
      EXPORTING
        TYPE = 'E'.
  ENDIF.

  CALL METHOD G_EXCEL->SELECT_SHEET
    EXPORTING
      NAME     = P_NEWNAME
      NO_FLUSH = ''
    IMPORTING
      ERROR    = G_ERROR
      RETCODE  = G_RETCODE.

ENDFORM.                    " FRM_CHANGE_SHEETNAME
*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_EXCEL_DATA3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_GRPNM_CSKS_KHINR  text
*----------------------------------------------------------------------*
FORM FRM_FILL_EXCEL_DATA3  USING P_KHINR.
  DATA:
    WA_L_EXCEL1     TYPE T_EXCEL1,
    IT_L_EXCEL1     TYPE STANDARD TABLE OF T_EXCEL1,
    I_NUM           TYPE I,
    WA_L_DATA1      TYPE T_DATA1,
    WA_L_DATA2      TYPE T_DATA2,
    WA_L_DATA1_SUB  TYPE T_DATA1,
    WA_L_DATA1_SUB2  TYPE T_DATA1.
*  hp_dxj 20160106 add
  DATA: LV_T31    TYPE STRING ,
        LV_T33    TYPE STRING ,
        LV_YEAR   TYPE GJAHR ,
        LV_YEAR_C TYPE C LENGTH 2 .  " hp_dxj 20160106 add

  CLEAR:WA_L_DATA1_SUB2.

*-->1. 初级分摊费用
  CLEAR:WA_L_EXCEL1.
  CONCATENATE TEXT-T10
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T21
              INTO   WA_L_EXCEL1-COL1.
  APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.
  "添加
  G_EXCEL_LINE = G_EXCEL_LINE + 2.
  PERFORM FRM_FILL_EXCEL_CELL2
                               TABLES IT_L_EXCEL1
                               USING  G_EXCEL_LINE   "Begin
                                      1         "Begin
                                      1             "No of rows reqd
                                      19            "No of cols reqd
                                      'D1H1'.       "Range name
  G_EXCEL_LINE = G_EXCEL_LINE + 1.

  REFRESH:IT_L_EXCEL1.
  CLEAR:WA_L_EXCEL1.
  CONCATENATE TEXT-T11
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T12
              INTO   WA_L_EXCEL1-COL1.
  CONCATENATE TEXT-T13
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T14
              INTO   WA_L_EXCEL1-COL2.
  CONCATENATE TEXT-T15
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T16
              INTO   WA_L_EXCEL1-COL3.
  CONCATENATE TEXT-T17
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T18
              INTO   WA_L_EXCEL1-COL4.
  CONCATENATE TEXT-T19
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T20
              INTO   WA_L_EXCEL1-COL5.
  CONCATENATE TEXT-T22
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T23
              INTO   WA_L_EXCEL1-COL6.
  CONCATENATE TEXT-T24
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T25
              INTO   WA_L_EXCEL1-COL7.
  CONCATENATE TEXT-T26
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T27
              INTO   WA_L_EXCEL1-COL8.
  CONCATENATE P_GJAHR
              TEXT-T28
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T29 SPACE P_GJAHR
              INTO   WA_L_EXCEL1-COL9.
*  hp_dxj 20160106 add
  IF P_VERSN = 0 .
    LV_YEAR = P_GJAHR - 1 .
    LV_YEAR_C = LV_YEAR+2(2).
    CONCATENATE 'BP11.' LV_YEAR_C INTO LV_T31 .
    CONCATENATE 'BP11.' LV_YEAR_C INTO LV_T33 .
*    text-t31 = '' .
  ELSE.
    LV_YEAR_C = P_GJAHR+2(2).
    CONCATENATE 'CF02.' LV_YEAR_C INTO LV_T31 .
    CONCATENATE 'CF02.' LV_YEAR_C INTO LV_T33 .
  ENDIF.

  CONCATENATE TEXT-T30
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              LV_T31                " hp_dxj 20160106 add
*              text-t31             " hp_dxj 20160106 marked
              INTO   WA_L_EXCEL1-COL10.
  CONCATENATE TEXT-T32
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              LV_T33              " hp_dxj 20160106 add
*              text-t33           " hp_dxj 20160106 marked
              INTO   WA_L_EXCEL1-COL11.
  CONCATENATE P_MONTH TEXT-T34
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T35 P_MONTH
              INTO   WA_L_EXCEL1-COL12.
  CONCATENATE P_MONTH TEXT-T36
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T37 P_MONTH
              INTO   WA_L_EXCEL1-COL13.
  CONCATENATE P_MONTH TEXT-T38
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T39 P_MONTH
              INTO   WA_L_EXCEL1-COL14.
  CONCATENATE P_GJAHR TEXT-T40
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T41
              INTO   WA_L_EXCEL1-COL15.
  CONCATENATE P_GJAHR TEXT-T42
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T43
              INTO   WA_L_EXCEL1-COL16.
  CONCATENATE P_GJAHR TEXT-T44
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T45
              INTO   WA_L_EXCEL1-COL17.
  CONCATENATE TEXT-T46
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T47
              INTO   WA_L_EXCEL1-COL18.
  CONCATENATE TEXT-T49
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T50
              INTO   WA_L_EXCEL1-COL19.
  APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.
  "添加
  PERFORM FRM_FILL_EXCEL_CELL2
                               TABLES IT_L_EXCEL1
                               USING  G_EXCEL_LINE   "Begin
                                      1         "Begin
                                      1             "No of rows reqd
                                      19            "No of cols reqd
                                      'D1H2'.       "Range name
  "Excel format
  PERFORM FRM_SET_COLUMN_HEAD USING G_EXCEL_LINE  '1'.

  G_EXCEL_LINE = G_EXCEL_LINE + 1.

  "数据
  CLEAR:I_NUM.
  REFRESH:IT_L_EXCEL1.
  CLEAR:WA_L_DATA1_SUB.
  LOOP AT IT_DATA1 INTO WA_L_DATA1 WHERE KHINR = P_KHINR.
    I_NUM = I_NUM + 1.

    CLEAR:WA_L_EXCEL1.
    WRITE:WA_L_DATA1-ABTEI TO  WA_L_EXCEL1-COL1 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-KOSTL TO  WA_L_EXCEL1-COL2 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-KSTAR TO  WA_L_EXCEL1-COL3 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-TXT50_EN TO  WA_L_EXCEL1-COL4 LEFT-JUSTIFIED NO-GROUPING,
          "wa_l_data1-KTEXT_EN to  wa_l_excel1-COL4 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-TXT50_ZH TO  WA_L_EXCEL1-COL5 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-SUBTOL_1 TO  WA_L_EXCEL1-COL6 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_1 TO  WA_L_EXCEL1-COL7 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_2 TO  WA_L_EXCEL1-COL8 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-SUBTOL_2 TO  WA_L_EXCEL1-COL9 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_3 TO  WA_L_EXCEL1-COL10 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_4 TO  WA_L_EXCEL1-COL11 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-SUBTOL_3 TO  WA_L_EXCEL1-COL12 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_5 TO  WA_L_EXCEL1-COL13 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_6 TO  WA_L_EXCEL1-COL14 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-SUBTOL_4 TO  WA_L_EXCEL1-COL15 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_7 TO  WA_L_EXCEL1-COL16 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_8 TO  WA_L_EXCEL1-COL17 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA1-WKGXXX_10 TO  WA_L_EXCEL1-COL19 LEFT-JUSTIFIED NO-GROUPING.

    WRITE WA_L_DATA1-WKGXXX_9 TO WA_L_EXCEL1-COL18 LEFT-JUSTIFIED NO-GROUPING.
    CONCATENATE WA_L_EXCEL1-COL18 '%' INTO WA_L_EXCEL1-COL18.

    "负号提前
    "PERFORM frm_sign_front1 CHANGING: wa_l_excel1.
    CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
      CHANGING:
        VALUE         = WA_L_EXCEL1-COL6,
        VALUE         = WA_L_EXCEL1-COL7,
        VALUE         = WA_L_EXCEL1-COL8,
        VALUE         = WA_L_EXCEL1-COL9,
        VALUE         = WA_L_EXCEL1-COL10,
        VALUE         = WA_L_EXCEL1-COL11,
        VALUE         = WA_L_EXCEL1-COL12,
        VALUE         = WA_L_EXCEL1-COL13,
        VALUE         = WA_L_EXCEL1-COL14,
        VALUE         = WA_L_EXCEL1-COL15,
        VALUE         = WA_L_EXCEL1-COL16,
        VALUE         = WA_L_EXCEL1-COL17,
        VALUE         = WA_L_EXCEL1-COL18,
        VALUE         = WA_L_EXCEL1-COL19.

    APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.

    "合计
    WA_L_DATA1_SUB-SUBTOL_1 = WA_L_DATA1_SUB-SUBTOL_1
                            + WA_L_DATA1-SUBTOL_1.
    WA_L_DATA1_SUB-WKGXXX_1 = WA_L_DATA1_SUB-WKGXXX_1
                            + WA_L_DATA1-WKGXXX_1.
    WA_L_DATA1_SUB-WKGXXX_2 = WA_L_DATA1_SUB-WKGXXX_2
                            + WA_L_DATA1-WKGXXX_2.
    WA_L_DATA1_SUB-SUBTOL_2 = WA_L_DATA1_SUB-SUBTOL_2
                            + WA_L_DATA1-SUBTOL_2.
    WA_L_DATA1_SUB-WKGXXX_3 = WA_L_DATA1_SUB-WKGXXX_3
                            + WA_L_DATA1-WKGXXX_3.
    WA_L_DATA1_SUB-WKGXXX_4 = WA_L_DATA1_SUB-WKGXXX_4
                            + WA_L_DATA1-WKGXXX_4.
    WA_L_DATA1_SUB-SUBTOL_3 = WA_L_DATA1_SUB-SUBTOL_3
                            + WA_L_DATA1-SUBTOL_3.
    WA_L_DATA1_SUB-WKGXXX_5 = WA_L_DATA1_SUB-WKGXXX_5
                            + WA_L_DATA1-WKGXXX_5.
    WA_L_DATA1_SUB-WKGXXX_6 = WA_L_DATA1_SUB-WKGXXX_6
                            + WA_L_DATA1-WKGXXX_6.
    WA_L_DATA1_SUB-SUBTOL_4 = WA_L_DATA1_SUB-SUBTOL_4
                            + WA_L_DATA1-SUBTOL_4.
    WA_L_DATA1_SUB-WKGXXX_7 = WA_L_DATA1_SUB-WKGXXX_7
                            + WA_L_DATA1-WKGXXX_7.
    WA_L_DATA1_SUB-WKGXXX_8 = WA_L_DATA1_SUB-WKGXXX_8
                            + WA_L_DATA1-WKGXXX_8.
  ENDLOOP.

  LOOP AT IT_SETNODE WHERE KHINR1 = P_KHINR.
    LOOP AT IT_DATA1 INTO WA_L_DATA1 WHERE KHINR = IT_SETNODE-KHINR2.
      I_NUM = I_NUM + 1.

      CLEAR:WA_L_EXCEL1.
      WRITE:WA_L_DATA1-ABTEI TO  WA_L_EXCEL1-COL1 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-KOSTL TO  WA_L_EXCEL1-COL2 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-KSTAR TO  WA_L_EXCEL1-COL3 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-TXT50_EN TO  WA_L_EXCEL1-COL4 LEFT-JUSTIFIED NO-GROUPING,
            "wa_l_data1-KTEXT_EN to  wa_l_excel1-COL4 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-TXT50_ZH TO  WA_L_EXCEL1-COL5 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-SUBTOL_1 TO  WA_L_EXCEL1-COL6 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-WKGXXX_1 TO  WA_L_EXCEL1-COL7 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-WKGXXX_2 TO  WA_L_EXCEL1-COL8 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-SUBTOL_2 TO  WA_L_EXCEL1-COL9 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-WKGXXX_3 TO  WA_L_EXCEL1-COL10 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-WKGXXX_4 TO  WA_L_EXCEL1-COL11 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-SUBTOL_3 TO  WA_L_EXCEL1-COL12 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-WKGXXX_5 TO  WA_L_EXCEL1-COL13 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-WKGXXX_6 TO  WA_L_EXCEL1-COL14 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-SUBTOL_4 TO  WA_L_EXCEL1-COL15 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-WKGXXX_7 TO  WA_L_EXCEL1-COL16 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-WKGXXX_8 TO  WA_L_EXCEL1-COL17 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA1-WKGXXX_10 TO  WA_L_EXCEL1-COL19 LEFT-JUSTIFIED NO-GROUPING.

      WRITE WA_L_DATA1-WKGXXX_9 TO WA_L_EXCEL1-COL18 LEFT-JUSTIFIED NO-GROUPING.
      CONCATENATE WA_L_EXCEL1-COL18 '%' INTO WA_L_EXCEL1-COL18.

      "负号提前
      "PERFORM frm_sign_front1 CHANGING: wa_l_excel1.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING:
          VALUE         = WA_L_EXCEL1-COL6,
          VALUE         = WA_L_EXCEL1-COL7,
          VALUE         = WA_L_EXCEL1-COL8,
          VALUE         = WA_L_EXCEL1-COL9,
          VALUE         = WA_L_EXCEL1-COL10,
          VALUE         = WA_L_EXCEL1-COL11,
          VALUE         = WA_L_EXCEL1-COL12,
          VALUE         = WA_L_EXCEL1-COL13,
          VALUE         = WA_L_EXCEL1-COL14,
          VALUE         = WA_L_EXCEL1-COL15,
          VALUE         = WA_L_EXCEL1-COL16,
          VALUE         = WA_L_EXCEL1-COL17,
          VALUE         = WA_L_EXCEL1-COL18,
          VALUE         = WA_L_EXCEL1-COL19.

      APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.

      "合计
      WA_L_DATA1_SUB-SUBTOL_1 = WA_L_DATA1_SUB-SUBTOL_1
                              + WA_L_DATA1-SUBTOL_1.
      WA_L_DATA1_SUB-WKGXXX_1 = WA_L_DATA1_SUB-WKGXXX_1
                              + WA_L_DATA1-WKGXXX_1.
      WA_L_DATA1_SUB-WKGXXX_2 = WA_L_DATA1_SUB-WKGXXX_2
                              + WA_L_DATA1-WKGXXX_2.
      WA_L_DATA1_SUB-SUBTOL_2 = WA_L_DATA1_SUB-SUBTOL_2
                              + WA_L_DATA1-SUBTOL_2.
      WA_L_DATA1_SUB-WKGXXX_3 = WA_L_DATA1_SUB-WKGXXX_3
                              + WA_L_DATA1-WKGXXX_3.
      WA_L_DATA1_SUB-WKGXXX_4 = WA_L_DATA1_SUB-WKGXXX_4
                              + WA_L_DATA1-WKGXXX_4.
      WA_L_DATA1_SUB-SUBTOL_3 = WA_L_DATA1_SUB-SUBTOL_3
                              + WA_L_DATA1-SUBTOL_3.
      WA_L_DATA1_SUB-WKGXXX_5 = WA_L_DATA1_SUB-WKGXXX_5
                              + WA_L_DATA1-WKGXXX_5.
      WA_L_DATA1_SUB-WKGXXX_6 = WA_L_DATA1_SUB-WKGXXX_6
                              + WA_L_DATA1-WKGXXX_6.
      WA_L_DATA1_SUB-SUBTOL_4 = WA_L_DATA1_SUB-SUBTOL_4
                              + WA_L_DATA1-SUBTOL_4.
      WA_L_DATA1_SUB-WKGXXX_7 = WA_L_DATA1_SUB-WKGXXX_7
                              + WA_L_DATA1-WKGXXX_7.
      WA_L_DATA1_SUB-WKGXXX_8 = WA_L_DATA1_SUB-WKGXXX_8
                              + WA_L_DATA1-WKGXXX_8.
    ENDLOOP.
  ENDLOOP.

  CLEAR:WA_L_EXCEL1.
  CONCATENATE TEXT-T51
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T52
              INTO   WA_L_EXCEL1-COL5.
  CATCH SYSTEM-EXCEPTIONS OTHERS = 8.
    IF WA_L_DATA1_SUB-SUBTOL_2 IS NOT INITIAL.
      WA_L_DATA1_SUB-WKGXXX_9 = WA_L_DATA1_SUB-SUBTOL_4 / WA_L_DATA1_SUB-SUBTOL_2 * 100.
    ELSE.
      WA_L_DATA1_SUB-WKGXXX_9 = 0.
    ENDIF.
  ENDCATCH.
  WA_L_DATA1_SUB-WKGXXX_10 =  WA_L_DATA1_SUB-SUBTOL_2 - WA_L_DATA1_SUB-SUBTOL_4.
  WRITE:WA_L_DATA1_SUB-SUBTOL_1 TO  WA_L_EXCEL1-COL6 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_1 TO  WA_L_EXCEL1-COL7 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_2 TO  WA_L_EXCEL1-COL8 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-SUBTOL_2 TO  WA_L_EXCEL1-COL9 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_3 TO  WA_L_EXCEL1-COL10 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_4 TO  WA_L_EXCEL1-COL11 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-SUBTOL_3 TO  WA_L_EXCEL1-COL12 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_5 TO  WA_L_EXCEL1-COL13 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_6 TO  WA_L_EXCEL1-COL14 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-SUBTOL_4 TO  WA_L_EXCEL1-COL15 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_7 TO  WA_L_EXCEL1-COL16 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_8 TO  WA_L_EXCEL1-COL17 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_10 TO  WA_L_EXCEL1-COL19 LEFT-JUSTIFIED NO-GROUPING.
  WRITE WA_L_DATA1_SUB-WKGXXX_9 TO WA_L_EXCEL1-COL18 LEFT-JUSTIFIED NO-GROUPING.
  CONCATENATE WA_L_EXCEL1-COL18 '%' INTO WA_L_EXCEL1-COL18.
  "负号提前
  "PERFORM frm_sign_front1 CHANGING wa_l_excel1.
  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING:
      VALUE         = WA_L_EXCEL1-COL6,
      VALUE         = WA_L_EXCEL1-COL7,
      VALUE         = WA_L_EXCEL1-COL8,
      VALUE         = WA_L_EXCEL1-COL9,
      VALUE         = WA_L_EXCEL1-COL10,
      VALUE         = WA_L_EXCEL1-COL11,
      VALUE         = WA_L_EXCEL1-COL12,
      VALUE         = WA_L_EXCEL1-COL13,
      VALUE         = WA_L_EXCEL1-COL14,
      VALUE         = WA_L_EXCEL1-COL15,
      VALUE         = WA_L_EXCEL1-COL16,
      VALUE         = WA_L_EXCEL1-COL17,
      VALUE         = WA_L_EXCEL1-COL18,
      VALUE         = WA_L_EXCEL1-COL19.
  APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.
  I_NUM = I_NUM + 1.
  "添加
  PERFORM FRM_FILL_EXCEL_CELL2
                               TABLES IT_L_EXCEL1
                               USING  G_EXCEL_LINE   "Begin
                                      1         "Begin
                                      I_NUM             "No of rows reqd
                                      19            "No of cols reqd
                                      'DATA1'.       "Range name

  "Excel Line
  G_EXCEL_LINE = G_EXCEL_LINE + I_NUM.

  "Excel format
  PERFORM FRM_SET_COLUMN_HEAD USING G_EXCEL_LINE  '0'.

  "总额
  WA_L_DATA1_SUB2-SUBTOL_1 = WA_L_DATA1_SUB2-SUBTOL_1
                          + WA_L_DATA1_SUB-SUBTOL_1.
  WA_L_DATA1_SUB2-WKGXXX_1 = WA_L_DATA1_SUB2-WKGXXX_1
                          + WA_L_DATA1_SUB-WKGXXX_1.
  WA_L_DATA1_SUB2-WKGXXX_2 = WA_L_DATA1_SUB2-WKGXXX_2
                          + WA_L_DATA1_SUB-WKGXXX_2.
  WA_L_DATA1_SUB2-SUBTOL_2 = WA_L_DATA1_SUB2-SUBTOL_2
                          + WA_L_DATA1_SUB-SUBTOL_2.
  WA_L_DATA1_SUB2-WKGXXX_3 = WA_L_DATA1_SUB2-WKGXXX_3
                          + WA_L_DATA1_SUB-WKGXXX_3.
  WA_L_DATA1_SUB2-WKGXXX_4 = WA_L_DATA1_SUB2-WKGXXX_4
                          + WA_L_DATA1_SUB-WKGXXX_4.
  WA_L_DATA1_SUB2-SUBTOL_3 = WA_L_DATA1_SUB2-SUBTOL_3
                          + WA_L_DATA1_SUB-SUBTOL_3.
  WA_L_DATA1_SUB2-WKGXXX_5 = WA_L_DATA1_SUB2-WKGXXX_5
                          + WA_L_DATA1_SUB-WKGXXX_5.
  WA_L_DATA1_SUB2-WKGXXX_6 = WA_L_DATA1_SUB2-WKGXXX_6
                          + WA_L_DATA1_SUB-WKGXXX_6.
  WA_L_DATA1_SUB2-SUBTOL_4 = WA_L_DATA1_SUB2-SUBTOL_4
                          + WA_L_DATA1_SUB-SUBTOL_4.
  WA_L_DATA1_SUB2-WKGXXX_7 = WA_L_DATA1_SUB2-WKGXXX_7
                          + WA_L_DATA1_SUB-WKGXXX_7.
  WA_L_DATA1_SUB2-WKGXXX_8 = WA_L_DATA1_SUB2-WKGXXX_8
                          + WA_L_DATA1_SUB-WKGXXX_8.

*-->2. 次级分摊费用
  CLEAR:I_NUM.
  REFRESH:IT_L_EXCEL1.
  CLEAR:WA_L_EXCEL1.
  CONCATENATE TEXT-T60
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T61
              INTO   WA_L_EXCEL1-COL1.
  APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.
  "添加
  G_EXCEL_LINE = G_EXCEL_LINE + 2.
  PERFORM FRM_FILL_EXCEL_CELL2
                               TABLES IT_L_EXCEL1
                               USING  G_EXCEL_LINE   "Begin
                                      1         "Begin
                                      1             "No of rows reqd
                                      19            "No of cols reqd
                                      'D2H1'.       "Range name
  G_EXCEL_LINE = G_EXCEL_LINE + 1.

  REFRESH:IT_L_EXCEL1.
  CLEAR:WA_L_EXCEL1.
  CONCATENATE TEXT-T11
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T12
              INTO   WA_L_EXCEL1-COL1.
  CONCATENATE TEXT-T13
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T14
              INTO   WA_L_EXCEL1-COL2.
  CONCATENATE TEXT-T15
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T16
              INTO   WA_L_EXCEL1-COL3.
  CONCATENATE TEXT-T17
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T18
              INTO   WA_L_EXCEL1-COL4.
  CONCATENATE TEXT-T19
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T20
              INTO   WA_L_EXCEL1-COL5.
  CONCATENATE TEXT-T22
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T23
              INTO   WA_L_EXCEL1-COL6.
  CONCATENATE TEXT-T24
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T25
              INTO   WA_L_EXCEL1-COL7.
  CONCATENATE TEXT-T55
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T27
              INTO   WA_L_EXCEL1-COL8.
  CONCATENATE P_GJAHR
              TEXT-T28
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T29 SPACE P_GJAHR
              INTO   WA_L_EXCEL1-COL9.
  CONCATENATE TEXT-T30
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              LV_T31
*              text-t31
              INTO   WA_L_EXCEL1-COL10.
  CONCATENATE TEXT-T32
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
               LV_T33
*              text-t33
              INTO   WA_L_EXCEL1-COL11.
  CONCATENATE P_MONTH TEXT-T34
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T35 P_MONTH
              INTO   WA_L_EXCEL1-COL12.
  CONCATENATE P_MONTH TEXT-T36
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T37 P_MONTH
              INTO   WA_L_EXCEL1-COL13.
  CONCATENATE P_MONTH TEXT-T56
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T39 P_MONTH
              INTO   WA_L_EXCEL1-COL14.
  CONCATENATE P_GJAHR TEXT-T40
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T41
              INTO   WA_L_EXCEL1-COL15.
  CONCATENATE P_GJAHR TEXT-T42
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T43
              INTO   WA_L_EXCEL1-COL16.
  CONCATENATE P_GJAHR TEXT-T57
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T45
              INTO   WA_L_EXCEL1-COL17.
  CONCATENATE TEXT-T46
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T47
              INTO   WA_L_EXCEL1-COL18.
  CONCATENATE TEXT-T49
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T50
              INTO   WA_L_EXCEL1-COL19.
  APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.
  "添加
  PERFORM FRM_FILL_EXCEL_CELL2
                               TABLES IT_L_EXCEL1
                               USING  G_EXCEL_LINE   "Begin
                                      1         "Begin
                                      1             "No of rows reqd
                                      19            "No of cols reqd
                                      'D2H2'.       "Range name
  "Excel format
  PERFORM FRM_SET_COLUMN_HEAD USING G_EXCEL_LINE  '1'.

  G_EXCEL_LINE = G_EXCEL_LINE + 1.

  "数据
  CLEAR:I_NUM.
  REFRESH:IT_L_EXCEL1.
  CLEAR:WA_L_DATA1_SUB.
  LOOP AT IT_DATA2 INTO WA_L_DATA2 WHERE KHINR = P_KHINR.
    I_NUM = I_NUM + 1.

    CLEAR:WA_L_EXCEL1.
    WRITE:WA_L_DATA2-ABTEI TO  WA_L_EXCEL1-COL1 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-KOSTL TO  WA_L_EXCEL1-COL2 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-KSTAR TO  WA_L_EXCEL1-COL3 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-TXT50_EN TO  WA_L_EXCEL1-COL4 LEFT-JUSTIFIED NO-GROUPING,
          "wa_l_data2-ktext_en to  wa_l_excel1-col4 left-justified no-grouping,
          WA_L_DATA2-TXT50_ZH TO  WA_L_EXCEL1-COL5 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-WKGXXX_1 TO  WA_L_EXCEL1-COL8 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-WKGXXX_2 TO  WA_L_EXCEL1-COL14 LEFT-JUSTIFIED NO-GROUPING,
          WA_L_DATA2-WKGXXX_3 TO  WA_L_EXCEL1-COL17 LEFT-JUSTIFIED NO-GROUPING.
    "负号提前
    "perform frm_sign_front1 changing wa_l_excel1.
    CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
      CHANGING:
        VALUE         = WA_L_EXCEL1-COL8,
        VALUE         = WA_L_EXCEL1-COL14,
        VALUE         = WA_L_EXCEL1-COL17.

    APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.

    "合计
    WA_L_DATA1_SUB-WKGXXX_1 = WA_L_DATA1_SUB-WKGXXX_1
                            + WA_L_DATA2-WKGXXX_1.
    WA_L_DATA1_SUB-WKGXXX_2 = WA_L_DATA1_SUB-WKGXXX_2
                            + WA_L_DATA2-WKGXXX_2.
    WA_L_DATA1_SUB-WKGXXX_3 = WA_L_DATA1_SUB-WKGXXX_3
                            + WA_L_DATA2-WKGXXX_3.
  ENDLOOP.

  LOOP AT IT_SETNODE WHERE KHINR1 = P_KHINR.
    LOOP AT IT_DATA2 INTO WA_L_DATA2 WHERE KHINR = IT_SETNODE-KHINR2.
      I_NUM = I_NUM + 1.

      CLEAR:WA_L_EXCEL1.
      WRITE:WA_L_DATA2-ABTEI TO  WA_L_EXCEL1-COL1 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA2-KOSTL TO  WA_L_EXCEL1-COL2 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA2-KSTAR TO  WA_L_EXCEL1-COL3 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA2-TXT50_EN TO  WA_L_EXCEL1-COL4 LEFT-JUSTIFIED NO-GROUPING,
            "wa_l_data2-ktext_en to  wa_l_excel1-col4 left-justified no-grouping,
            WA_L_DATA2-TXT50_ZH TO  WA_L_EXCEL1-COL5 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA2-WKGXXX_1 TO  WA_L_EXCEL1-COL8 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA2-WKGXXX_2 TO  WA_L_EXCEL1-COL14 LEFT-JUSTIFIED NO-GROUPING,
            WA_L_DATA2-WKGXXX_3 TO  WA_L_EXCEL1-COL17 LEFT-JUSTIFIED NO-GROUPING.
      "负号提前
      "perform frm_sign_front1 changing wa_l_excel1.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING:
          VALUE         = WA_L_EXCEL1-COL8,
          VALUE         = WA_L_EXCEL1-COL14,
          VALUE         = WA_L_EXCEL1-COL17.

      APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.

      "合计
      WA_L_DATA1_SUB-WKGXXX_1 = WA_L_DATA1_SUB-WKGXXX_1
                              + WA_L_DATA2-WKGXXX_1.
      WA_L_DATA1_SUB-WKGXXX_2 = WA_L_DATA1_SUB-WKGXXX_2
                              + WA_L_DATA2-WKGXXX_2.
      WA_L_DATA1_SUB-WKGXXX_3 = WA_L_DATA1_SUB-WKGXXX_3
                              + WA_L_DATA2-WKGXXX_3.
    ENDLOOP.
  ENDLOOP.
  CLEAR:WA_L_EXCEL1.
  CONCATENATE TEXT-T53
              CL_ABAP_CHAR_UTILITIES=>NEWLINE
              TEXT-T54
              INTO   WA_L_EXCEL1-COL5.
  WRITE:WA_L_DATA1_SUB-WKGXXX_1 TO  WA_L_EXCEL1-COL8 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_2 TO  WA_L_EXCEL1-COL14 LEFT-JUSTIFIED NO-GROUPING,
        WA_L_DATA1_SUB-WKGXXX_3 TO  WA_L_EXCEL1-COL17 LEFT-JUSTIFIED NO-GROUPING.
  "负号提前
  "PERFORM frm_sign_front1 CHANGING wa_l_excel1.
  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING:
      VALUE         = WA_L_EXCEL1-COL8,
      VALUE         = WA_L_EXCEL1-COL14,
      VALUE         = WA_L_EXCEL1-COL17.
  APPEND WA_L_EXCEL1 TO IT_L_EXCEL1.
  I_NUM = I_NUM + 1.
  "添加
  PERFORM FRM_FILL_EXCEL_CELL2
                               TABLES IT_L_EXCEL1
                               USING  G_EXCEL_LINE   "Begin
                                      1         "Begin
                                      I_NUM             "No of rows reqd
                                      19           "No of cols reqd
                                      'DATA2'.       "Range name
  "Excel Line
  G_EXCEL_LINE = G_EXCEL_LINE + I_NUM.
  "Excel format
  PERFORM FRM_SET_COLUMN_HEAD USING G_EXCEL_LINE  '0'.

  "总额
  WA_L_DATA1_SUB2-SUBTOL_1 = WA_L_DATA1_SUB2-SUBTOL_1
                          + WA_L_DATA1_SUB-SUBTOL_1.
  WA_L_DATA1_SUB2-WKGXXX_1 = WA_L_DATA1_SUB2-WKGXXX_1
                          + WA_L_DATA1_SUB-WKGXXX_1.
  WA_L_DATA1_SUB2-WKGXXX_2 = WA_L_DATA1_SUB2-WKGXXX_2
                          + WA_L_DATA1_SUB-WKGXXX_2.
  WA_L_DATA1_SUB2-SUBTOL_2 = WA_L_DATA1_SUB2-SUBTOL_2
                          + WA_L_DATA1_SUB-SUBTOL_2.
  WA_L_DATA1_SUB2-WKGXXX_3 = WA_L_DATA1_SUB2-WKGXXX_3
                          + WA_L_DATA1_SUB-WKGXXX_3.
  WA_L_DATA1_SUB2-WKGXXX_4 = WA_L_DATA1_SUB2-WKGXXX_4
                          + WA_L_DATA1_SUB-WKGXXX_4.
  WA_L_DATA1_SUB2-SUBTOL_3 = WA_L_DATA1_SUB2-SUBTOL_3
                          + WA_L_DATA1_SUB-SUBTOL_3.
  WA_L_DATA1_SUB2-WKGXXX_5 = WA_L_DATA1_SUB2-WKGXXX_5
                          + WA_L_DATA1_SUB-WKGXXX_5.
  WA_L_DATA1_SUB2-WKGXXX_6 = WA_L_DATA1_SUB2-WKGXXX_6
                          + WA_L_DATA1_SUB-WKGXXX_6.
  WA_L_DATA1_SUB2-SUBTOL_4 = WA_L_DATA1_SUB2-SUBTOL_4
                          + WA_L_DATA1_SUB-SUBTOL_4.
  WA_L_DATA1_SUB2-WKGXXX_7 = WA_L_DATA1_SUB2-WKGXXX_7
                          + WA_L_DATA1_SUB-WKGXXX_7.
  WA_L_DATA1_SUB2-WKGXXX_8 = WA_L_DATA1_SUB2-WKGXXX_8
                          + WA_L_DATA1_SUB-WKGXXX_8.

*-->3. 总合计


*-->4. Excel Color
  PERFORM FRM_EXCEL_COLOR.
ENDFORM.                    " FRM_FILL_EXCEL_DATA3