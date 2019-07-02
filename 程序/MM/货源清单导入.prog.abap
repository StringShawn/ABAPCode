*$*$********************************************************************
* Program ID/Name:  ZMMI009                     Date written: 20150720
* Author's name:   HP_SJF                       Last update:
* Program title:  批量导入货源清单
* Project Name:  EPR I
* Version:
* Function Spec ID: ZFSS_SAP_FS_MM_02_23_批量导入货源清单_V1.0.doc
*$*$********************************************************************
*----------------------------------------------------------------------*
* Description:
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
* 2015/07/20 |  HP_SJF        |   Init      |

REPORT ZMMI009.
*$*$********************************************************************
*$*$    TABLES                                                         *
*$*$********************************************************************
************************************************************************
*        CONSTANTS
************************************************************************
CONSTANTS: C_BEGIN_COL   TYPE I VALUE '1',     "开始列
           C_BEGIN_ROW   TYPE I VALUE '2',     "开始行
           C_END_COL     TYPE I VALUE '8',    "结束列
           C_END_ROW     TYPE I VALUE '65535'. "结束行
*$*$********************************************************************
*$*$    GLOBAL TYPES                                                   *
*$*$********************************************************************
TYPES:  BEGIN OF T_EXCEL,
        TEXT(8000) TYPE C,
  END OF T_EXCEL.
TYPES: BEGIN OF T_MSG,
        MSG_TYP(01)   TYPE C,
        MSG_LCD       TYPE ICON_D,
        MSG_TXT(255),
        MATNR     TYPE EORD-MATNR,
        WERKS     TYPE EORD-WERKS,
        VDATU     TYPE EORD-VDATU,
        BDATU     TYPE EORD-BDATU,
        LIFNR     TYPE EORD-LIFNR,
        EKORG     TYPE EORD-EKORG,
        AUTET     TYPE EORD-AUTET,
  END OF T_MSG.
TYPES: BEGIN OF T_ORIG_DATA,
        MATNR(18),
        WERKS(4),
        VDATU(8),
        BDATU(8),
        LIFNR(10),
        EKORG(4),
        AUTET(1),
  END OF T_ORIG_DATA.
TYPES: BEGIN OF T_MAIN,
        MATNR     TYPE EORD-MATNR,
        WERKS     TYPE EORD-WERKS,
        VDATU     TYPE EORD-VDATU,
        BDATU     TYPE EORD-BDATU,
        LIFNR     TYPE EORD-LIFNR,
        EKORG     TYPE EORD-EKORG,
        AUTET     TYPE EORD-AUTET,
  END OF T_MAIN.
*$*$********************************************************************
*$*$    GLOBAL INTERNAL TABLES                                         *
*$*$********************************************************************
DATA: IT_EXCEL      TYPE STANDARD TABLE OF T_EXCEL,
      WA_EXCEL      TYPE T_EXCEL,
      IT_MAIN       TYPE STANDARD TABLE OF T_MAIN,
      WA_MAIN       TYPE T_MAIN,
      IT_MSG        TYPE STANDARD TABLE OF T_MSG,
      WA_MSG        TYPE T_MSG.

DATA: IT_FILE       TYPE FILETABLE.
DATA: GS_LAYOUT     TYPE SLIS_LAYOUT_ALV,
      GT_FIELDCAT   TYPE SLIS_T_FIELDCAT_ALV.

*$*$********************************************************************
*$*$    GLOBAL ELEMENTARY VARIABLES                                    *
*$*$********************************************************************
DATA: G_RC        TYPE I,
      G_SEPARATOR TYPE C,
      G_MODE      TYPE C VALUE 'N'.

DATA: G_REPID     TYPE SY-REPID,
      G_ANSWER(1) TYPE C.
*$*$********************************************************************
*$*$    PARAMETERS & SELECT-OPTIONS                                    *
*$*$********************************************************************
SELECTION-SCREEN:BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS:     P_FILE     TYPE RLGRAP-FILENAME OBLIGATORY.
SELECTION-SCREEN:END OF BLOCK B1 .

*$*$********************************************************************
*$*$    INITIALIZATION                                                 *
*$*$********************************************************************


*$*$********************************************************************
*$*$    AT SELECTION-SCREEN  Output                                    *
*$*$********************************************************************
AT SELECTION-SCREEN OUTPUT.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  CLEAR: IT_FILE, G_RC.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = ''
    CHANGING
      FILE_TABLE              = IT_FILE
      RC                      = G_RC
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.
  IF SY-SUBRC = 0.
    READ TABLE IT_FILE INTO P_FILE INDEX 1.
  ELSE.
    EXIT.
  ENDIF.

*$*$********************************************************************
*$*$    START-OF-SELECTION                                             *
*$*$********************************************************************
START-OF-SELECTION.
  PERFORM FRM_GET_MAIN_DATA.
  PERFORM FRM_DEAL_DATA.
  PERFORM FRM_DISPLAY_DATA.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_MAIN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_MAIN_DATA .

  CLEAR:G_SEPARATOR.
  CHECK NOT P_FILE IS INITIAL.
  CALL FUNCTION 'ZZZ_UPLOADEXCEL'
    EXPORTING
      IM_FILENAME             = P_FILE
      IM_BEGIN_COL            = C_BEGIN_COL
      IM_BEGIN_ROW            = C_BEGIN_ROW
      IM_END_COL              = C_END_COL
      IM_END_ROW              = C_END_ROW
    IMPORTING
      EX_SEPARATOR            = G_SEPARATOR
    TABLES
      IT_EXCELTAB             = IT_EXCEL
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
    MESSAGE S000(ZMM_MSG) WITH '上传EXCEL出错' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF IT_EXCEL IS INITIAL.
    MESSAGE S000(ZMM_MSG) WITH '上传EXCEL数据为空' DISPLAY LIKE 'E'.
    STOP.
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
FORM FRM_DEAL_DATA .
  DATA: WA_ORIG_DATA  TYPE T_ORIG_DATA,
        LS_EORD       TYPE EORD,
        LT_EORD       TYPE STANDARD TABLE OF EORD.
  DATA: L_LIFNR       TYPE LFA1-LIFNR,
        L_MATNR       TYPE MARA-MATNR,
        L_WERKS       TYPE T001W-WERKS,
        L_ERR(1)      TYPE C."用于记录是否有错

***形成上传数据
  CLEAR: WA_MSG.
  LOOP AT IT_EXCEL INTO WA_EXCEL.
    CLEAR: L_ERR.
    CLEAR:WA_ORIG_DATA.
    SPLIT WA_EXCEL AT G_SEPARATOR
     INTO WA_ORIG_DATA-MATNR
          WA_ORIG_DATA-WERKS
          WA_ORIG_DATA-VDATU
          WA_ORIG_DATA-BDATU
          WA_ORIG_DATA-LIFNR
          WA_ORIG_DATA-EKORG
          WA_ORIG_DATA-AUTET.
* 检查上传数据
    CLEAR: WA_MSG.
    MOVE: WA_ORIG_DATA-MATNR  TO WA_MSG-MATNR,
          WA_ORIG_DATA-WERKS  TO WA_MSG-WERKS,
          WA_ORIG_DATA-VDATU  TO WA_MSG-VDATU,
          WA_ORIG_DATA-BDATU  TO WA_MSG-BDATU,
          WA_ORIG_DATA-LIFNR  TO WA_MSG-LIFNR,
          WA_ORIG_DATA-EKORG  TO WA_MSG-EKORG,
          WA_ORIG_DATA-AUTET  TO WA_MSG-AUTET.
    "Convert to Program
    CATCH SYSTEM-EXCEPTIONS CONVT_NO_NUMBER = 1
                           OTHERS = 8.
      CLEAR: WA_MAIN.
      MOVE: WA_ORIG_DATA-MATNR  TO WA_MAIN-MATNR,
            WA_ORIG_DATA-WERKS  TO WA_MAIN-WERKS,
            WA_ORIG_DATA-VDATU  TO WA_MAIN-VDATU,
            WA_ORIG_DATA-BDATU  TO WA_MAIN-BDATU,
            WA_ORIG_DATA-LIFNR  TO WA_MAIN-LIFNR,
            WA_ORIG_DATA-EKORG  TO WA_MAIN-EKORG,
            WA_ORIG_DATA-AUTET  TO WA_MAIN-AUTET.
    ENDCATCH.
    IF SY-SUBRC <> 0.
      MOVE 'E' TO WA_MSG-MSG_TYP.
      MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
      MOVE TEXT-M01 TO WA_MSG-MSG_TXT.
      L_ERR = 'X'.
      APPEND WA_MSG TO IT_MSG.
      CONTINUE.
    ENDIF.
*   初始化检查
    IF WA_MSG-MATNR IS INITIAL.
      MOVE 'E' TO WA_MSG-MSG_TYP.
      MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
      MESSAGE E001(ZMM_MSG) WITH TEXT-T01
        INTO WA_MSG-MSG_TXT.
      L_ERR = 'X'.
      APPEND WA_MSG TO IT_MSG.
      CONTINUE.
    ENDIF.
    IF WA_MSG-WERKS IS INITIAL.
      MOVE 'E' TO WA_MSG-MSG_TYP.
      MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
      MESSAGE E001(ZMM_MSG) WITH TEXT-T02
        INTO WA_MSG-MSG_TXT.
      L_ERR = 'X'.
      APPEND WA_MSG TO IT_MSG.
      CONTINUE.
    ENDIF.
    IF WA_MSG-VDATU IS INITIAL.
      MOVE 'E' TO WA_MSG-MSG_TYP.
      MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
      MESSAGE E001(ZMM_MSG) WITH TEXT-T03
        INTO WA_MSG-MSG_TXT.
      L_ERR = 'X'.
      APPEND WA_MSG TO IT_MSG.
      CONTINUE.
    ENDIF.
    IF WA_MSG-BDATU IS INITIAL.
      MOVE 'E' TO WA_MSG-MSG_TYP.
      MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
      MESSAGE E001(ZMM_MSG) WITH TEXT-T04
        INTO WA_MSG-MSG_TXT.
      L_ERR = 'X'.
      APPEND WA_MSG TO IT_MSG.
      CONTINUE.
    ENDIF.
    IF WA_MSG-LIFNR IS INITIAL.
      MOVE 'E' TO WA_MSG-MSG_TYP.
      MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
      MESSAGE E001(ZMM_MSG) WITH TEXT-T05
        INTO WA_MSG-MSG_TXT.
      L_ERR = 'X'.
      APPEND WA_MSG TO IT_MSG.
      CONTINUE.
    ENDIF.
    IF WA_MSG-EKORG IS INITIAL.
      MOVE 'E' TO WA_MSG-MSG_TYP.
      MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
      MESSAGE E001(ZMM_MSG) WITH TEXT-T06
        INTO WA_MSG-MSG_TXT.
      L_ERR = 'X'.
      APPEND WA_MSG TO IT_MSG.
      CONTINUE.
    ENDIF.
    IF WA_MAIN-AUTET IS INITIAL.
      MOVE 'E' TO WA_MSG-MSG_TYP.
      MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
      MESSAGE E001(ZMM_MSG) WITH TEXT-T07
        INTO WA_MSG-MSG_TXT.
      L_ERR = 'X'.
      APPEND WA_MSG TO IT_MSG.
      CONTINUE.
    ENDIF.

* 检查数据准确性
    CLEAR:L_MATNR.
***转化物料大小写
    TRANSLATE WA_MSG-MATNR TO UPPER CASE.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT        = WA_MSG-MATNR
      IMPORTING
        OUTPUT       = WA_MSG-MATNR
      EXCEPTIONS
        LENGTH_ERROR = 1
        OTHERS       = 2.
    SELECT SINGLE MATNR FROM MARA INTO L_MATNR WHERE MATNR = WA_MSG-MATNR.
    IF SY-SUBRC <> 0.
      MOVE 'E' TO WA_MSG-MSG_TYP.
      MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
      MESSAGE E002(ZMM_MSG) WITH WA_MSG-MATNR
        INTO WA_MSG-MSG_TXT.
      L_ERR = 'X'.
      APPEND WA_MSG TO IT_MSG.
      CONTINUE.
    ENDIF.
    CLEAR: L_WERKS.
    TRANSLATE WA_MSG-WERKS TO UPPER CASE.
    SELECT SINGLE WERKS FROM T001W INTO L_WERKS WHERE WERKS = WA_MSG-WERKS.
    IF SY-SUBRC <> 0.
      MOVE 'E' TO WA_MSG-MSG_TYP.
      MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
      MESSAGE E003(ZMM_MSG) WITH WA_MSG-WERKS
        INTO WA_MSG-MSG_TXT.
      L_ERR = 'X'.
      APPEND WA_MSG TO IT_MSG.
      CONTINUE.
    ENDIF.
    CLEAR: L_LIFNR.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_MSG-LIFNR
      IMPORTING
        OUTPUT = WA_MSG-LIFNR.
    SELECT SINGLE LIFNR FROM LFA1 INTO L_LIFNR WHERE LIFNR = WA_MSG-LIFNR.
    IF SY-SUBRC <> 0.
      MOVE 'E' TO WA_MSG-MSG_TYP.
      MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
      MESSAGE E071(ZMM_MSG) WITH WA_MSG-LIFNR
        INTO WA_MSG-MSG_TXT.
      L_ERR = 'X'.
      APPEND WA_MSG TO IT_MSG.
      CONTINUE.
    ENDIF.
    TRANSLATE WA_MSG-EKORG TO UPPER CASE.

***检查货源清单是否已经存在,通过物料工厂供应商采购组织来检查
    CLEAR: LS_EORD, LT_EORD.
    SELECT * INTO TABLE LT_EORD FROM EORD
      WHERE MATNR = WA_MSG-MATNR
      AND WERKS = WA_MSG-WERKS
      AND LIFNR = WA_MSG-LIFNR
      AND EKORG = WA_MSG-EKORG.
    LOOP AT LT_EORD INTO LS_EORD.
      IF WA_MSG-VDATU BETWEEN LS_EORD-VDATU AND LS_EORD-BDATU.
        MOVE 'E' TO WA_MSG-MSG_TYP.
        MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
        MESSAGE E209(ZMM_MSG) WITH WA_MSG-VDATU
          INTO WA_MSG-MSG_TXT.
        L_ERR = 'X'.
        APPEND WA_MSG TO IT_MSG.
        EXIT.
      ENDIF.
      IF WA_MSG-BDATU BETWEEN LS_EORD-VDATU AND LS_EORD-BDATU.
        MOVE 'E' TO WA_MSG-MSG_TYP.
        MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
        MESSAGE E210(ZMM_MSG) WITH WA_MSG-BDATU
          INTO WA_MSG-MSG_TXT.
        L_ERR = 'X'.
        APPEND WA_MSG TO IT_MSG.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF L_ERR IS INITIAL.
      MOVE 'S' TO WA_MSG-MSG_TYP.
      MOVE ICON_LED_GREEN TO WA_MSG-MSG_LCD.
      CLEAR WA_MSG-MSG_TXT.
      APPEND WA_MSG TO IT_MSG.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DISPLAY_DATA .

  CLEAR: GT_FIELDCAT, GS_LAYOUT, G_REPID.

  PERFORM FRM_FILL_FIELDCAT.
  PERFORM FRM_FILL_LAYOUT.
  G_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = G_REPID
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      IS_LAYOUT                = GS_LAYOUT
      IT_FIELDCAT              = GT_FIELDCAT[]
      I_SAVE                   = 'A'
    TABLES
      T_OUTTAB                 = IT_MSG.   "需要输出内容的内表

ENDFORM.                    " FRM_DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_FILL_FIELDCAT .

  PERFORM FRM_GET_FIELDCAT USING 'MSG_LCD'  '状态'  '4' 'X' ''.
  PERFORM FRM_GET_FIELDCAT USING 'MATNR'    '物料'  '18' '' ''.
  PERFORM FRM_GET_FIELDCAT USING 'WERKS'    '工厂'  '4' '' ''.
  PERFORM FRM_GET_FIELDCAT USING 'VDATU'    '有效起始日'  '10' '' ''.
  PERFORM FRM_GET_FIELDCAT USING 'BDATU'    '有效截止日'  '10' '' ''.
  PERFORM FRM_GET_FIELDCAT USING 'LIFNR'    '供应商代码'  '10' '' ''.
  PERFORM FRM_GET_FIELDCAT USING 'EKORG'    '采购组织'  '4' '' ''.
  PERFORM FRM_GET_FIELDCAT USING 'AUTET'     'MRP'   '1' '' ''.
  PERFORM FRM_GET_FIELDCAT USING 'MSG_TXT'  '备注'    '40' '' ''.

ENDFORM.                    " FRM_FILL_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_name   text
*      -->P_text   text
*      -->P_outputlen   text
*      -->P_KEY   text
*----------------------------------------------------------------------*
FORM FRM_GET_FIELDCAT  USING    P_NAME
                                P_TEXT
                                P_OUTPUTLEN
                                P_KEY
                                P_HOT.

  DATA: LS_FIELDCAT       TYPE SLIS_FIELDCAT_ALV.

  LS_FIELDCAT-FIELDNAME = P_NAME.
  LS_FIELDCAT-SELTEXT_M = P_TEXT.
  LS_FIELDCAT-OUTPUTLEN = P_OUTPUTLEN.
  LS_FIELDCAT-KEY       = P_KEY.
  LS_FIELDCAT-HOTSPOT   = P_HOT.

  IF P_NAME = 'MATNR' OR P_NAME = 'LIFNR' .
    LS_FIELDCAT-NO_ZERO = 'X'.          "前导0不显示
  ENDIF.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.
  CLEAR LS_FIELDCAT.

ENDFORM.                    " FRM_GET_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  FRM_FILL_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_FILL_LAYOUT .

  GS_LAYOUT-ZEBRA = 'X'.
  GS_LAYOUT-NO_INPUT          = ' '.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

ENDFORM.                    " FRM_FILL_LAYOUT
*&---------------------------------------------------------------------*
*&      FORM  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_EXTAB    text
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING PT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS '1000_STAT' EXCLUDING PT_EXTAB.
ENDFORM.                    "Pf_set_status
*&---------------------------------------------------------------------*
*&      FORM  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_COMM     text
*      -->SELFIELD   text
*----------------------------------------------------------------------*
FORM USER_COMMAND USING P_UCOMM TYPE SY-UCOMM SELFIELD TYPE SLIS_SELFIELD.

  DATA: L_GRID      TYPE REF TO CL_GUI_ALV_GRID,
        L_ERR(1)    TYPE C."记录错误

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = L_GRID.

  CASE P_UCOMM.
    WHEN 'OK_SAVE'.

      CALL METHOD L_GRID->CHECK_CHANGED_DATA.

***检查是否内表已经符合要求可以导入
      READ TABLE IT_MSG INTO WA_MSG WITH KEY MSG_TYP = 'E'.
      IF SY-SUBRC = 0.
        MESSAGE E141(ZMM_MSG).
        EXIT.
      ENDIF.

***需要判断更新后不能再次进行

      CLEAR: G_ANSWER.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = '确认批量导入'
          TEXT_QUESTION         = '是否批量导入所有数据？'
          TEXT_BUTTON_1         = '是'
          TEXT_BUTTON_2         = '否'
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ''
        IMPORTING
          ANSWER                = G_ANSWER.
      IF G_ANSWER = '1'.
        PERFORM FRM_UPDATE_DATA.
        SELFIELD-REFRESH = 'X'.
***刷新ALV显示
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_UPDATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_UPDATE_DATA .

  DATA: LT_EORDU      TYPE TABLE OF EORDU,
        LS_EORDU      TYPE EORDU.
  DATA: L_MATNR       TYPE MATNR,
        L_WERKS       TYPE WERKS_D,
        L_ERR(1)      TYPE C.

  SORT IT_MSG BY MATNR WERKS.
  CLEAR: LS_EORDU, L_ERR.
  LOOP AT IT_MSG INTO WA_MSG.
    AT NEW MATNR.
      CLEAR: LT_EORDU[].
    ENDAT.
    MOVE-CORRESPONDING WA_MSG TO LS_EORDU.
    LS_EORDU-ERDAT = SY-DATUM.
    LS_EORDU-ERNAM = SY-UNAME.
    LS_EORDU-KZ = 'I'.
    APPEND LS_EORDU TO LT_EORDU.
    CLEAR: L_MATNR, L_WERKS.
    L_MATNR = WA_MSG-MATNR.
    L_WERKS = WA_MSG-WERKS.
    AT END OF MATNR.
      CALL FUNCTION 'ME_INITIALIZE_SOURCE_LIST' .

      CALL FUNCTION 'ME_DIRECT_INPUT_SOURCE_LIST'
        EXPORTING
          I_MATNR          = L_MATNR
          I_WERKS          = L_WERKS
        TABLES
          T_EORD           = LT_EORDU
        EXCEPTIONS
          PLANT_MISSING    = 1
          MATERIAL_MISSING = 2
          ERROR_MESSAGE    = 3
          OTHERS           = 4.
      IF SY-SUBRC = 0.

        CALL FUNCTION 'ME_POST_SOURCE_LIST_NEW'
          EXPORTING
            I_MATNR = L_MATNR.

      ELSE.

        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO WA_MSG-MSG_TXT.
        MOVE 'E' TO WA_MSG-MSG_TYP.
        MOVE ICON_LED_RED TO WA_MSG-MSG_LCD.
        L_ERR = 'X'.
        MODIFY IT_MSG FROM WA_MSG TRANSPORTING MSG_TYP MSG_LCD MSG_TXT.
      ENDIF.

    ENDAT.
  ENDLOOP.

  IF L_ERR IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
    LOOP AT IT_MSG INTO WA_MSG.
      WA_MSG-MSG_TXT = '货源清单已经成功导入'.
      MODIFY IT_MSG FROM WA_MSG TRANSPORTING MSG_TXT.
      CLEAR WA_MSG.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " FRM_UPDATE_DATA
