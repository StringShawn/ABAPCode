*$*$********************************************************************
* Program ID/Name:ZPPR009                 Date written:2013.07.22
* Author's name:HP_FCG                    Last update:2013.07.22
* Program title:生产计划导入
* Project Name:  EPR I
* Version:
* Function Spec ID:PP_01_09
*----------------------------------------------------------------------*
* Description: (Incl. Related Function Area and System)
*              用户维护好生产计划导入的excel文件，上载到SAP，
*              程序依此创建MRP的计划独立需求
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
REPORT  ZPPR009 MESSAGE-ID ZPP_MSG.

*$*$********************************************************************
*$*$    TABLES                                                         *
*$*$********************************************************************
TABLES:MARC.
*$*$********************************************************************
*$*$    INCLUDES                                                       *
*$*$    (FOR INCLUDES WITH DECLARATIVE PURPOSES ONLY                   *
*$*$     I.E. BDC, ALV, ETC.)                                          *
*$*$********************************************************************

*$*$********************************************************************
*$*$    GLOBAL TYPES                                                   *
*$*$********************************************************************
TYPES: BEGIN OF TY_DATA,
         WERKS LIKE ZTPIRI-WERKS,
         MATNR LIKE ZTPIRI-MATNR,
       END OF TY_DATA.
*$*$********************************************************************
*$*$    GLOBAL CONSTANTS                                               *
*$*$********************************************************************

*$*$********************************************************************
*$*$    GLOBAL ELEMENTARY VARIABLES                                    *
*$*$********************************************************************

DATA: G_TABLE TYPE REF TO DATA.
DATA: G_DCPFM LIKE USR01-DCPFM.
DATA:G_GRID TYPE REF TO CL_GUI_ALV_GRID.
DATA G_CODE LIKE SY-UCOMM.
*$*$********************************************************************
*$*$    GLOBAL STRUCTURES                                              *
*$*$********************************************************************
****alv structure
DATA:  WA_LAYOUT TYPE LVC_S_LAYO,
       WA_ALV_CAT  TYPE LVC_S_FCAT,
       WA_TABLE    LIKE DNTAB,
       WA_WEEK     LIKE T246,
      WA_ZTPPH     TYPE ZTPPH.
*$*$********************************************************************
*$*$    GLOBAL INTERNAL TABLES                                         *
*$*$********************************************************************
DATA:IT_WEEK TYPE STANDARD TABLE OF T246.
DATA:IT_ZTPPI TYPE STANDARD TABLE OF ZTPPI WITH HEADER LINE.
DATA: BEGIN OF IT_DATE OCCURS 0,
       COL   TYPE I,
       DATE  LIKE SY-DATUM,
      END OF IT_DATE.

DATA: BEGIN OF IT_ERROR OCCURS 0,
        Z_POSNR LIKE ZTPPI-Z_POSNR,
        WERKS   LIKE ZTPPI-WERKS,
        MATNR   LIKE ZTPPI-MATNR,
        MAKTX   LIKE ZTPPI-MAKTX,
        MESSAGE TYPE STRING,
END OF IT_ERROR.
*****alv field category.
DATA: IT_ALV_CAT TYPE LVC_T_FCAT.

*DATA: G_ROW      TYPE I VALUE 104."20150528hp_sjfEXCEL上传最大列数60改为104
DATA: G_COL      TYPE I VALUE 255."20150731hp_sjfEXCEL上传最大列数60改为255
*$*$********************************************************************
*$*$    MACROS                                                         *
*$*$********************************************************************

*$*$********************************************************************
*$*$    GLOBAL FIELD-SYMBOLS                                           *
*$*$********************************************************************
FIELD-SYMBOLS: <DYN_TABLE> TYPE STANDARD TABLE,
               <DYN_WA>,
               <DYN_FIELD>.
*$*$********************************************************************
*$*$    CLASSES                                                        *
*$*$********************************************************************

*$*$********************************************************************
*$*$    PARAMETERS & SELECT-OPTIONS                                    *
*$*$********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

PARAMETERS: P_IMPORT RADIOBUTTON GROUP G1 DEFAULT 'X' USER-COMMAND U1,
            P_QUERY RADIOBUTTON GROUP G1 .
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_FILE TYPE STRING MODIF ID GP1,
            P_VERSB1 LIKE ZTPIRH-Z_VERSB MODIF ID GP1,
            P_VERNR1 LIKE ZTPIRH-Z_VERNR DEFAULT '1' MODIF ID GP1
                                         NO-DISPLAY,
            P_DESC LIKE ZTPIRH-Z_DESCR MODIF ID GP1.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS: S_WERKS FOR MARC-WERKS MODIF ID GP2,
                S_MATNR FOR MARC-MATNR MODIF ID GP2.
PARAMETERS: P_VERSB2 LIKE ZTPIRH-Z_VERSB MODIF ID GP2,
            P_VERNR2 LIKE ZTPIRH-Z_VERNR MODIF ID GP2 DEFAULT '1'
            NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B3.
*$*$********************************************************************
*$*$    INITIALIZATION                                                 *
*$*$********************************************************************


*$*$********************************************************************
*$*$    AT SELECTION-SCREEN ON                                         *
*$*$********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM FRM_CHOOSE_INPUT_FILE .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VERSB1.
  PERFORM FRM_GET_SEARCH_HELP."F4帮助...

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VERSB2.
  PERFORM FRM_GET_SEARCH_HELP."F4帮助...

*$*$********************************************************************
*$*$    AT SELECTION-SCREEN OUTPUT                                     *
*$*$********************************************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM  FRM_RADIOBUTTON_SELECT. " 单选按钮处理
*$*$********************************************************************
*$*$    AT SELECTION-SCREEN                                            *
*$*$********************************************************************
AT SELECTION-SCREEN.
  IF SY-UCOMM = 'ONLI'.
    PERFORM FRM_SELECTION_INPUTCHECK."屏幕输入检查
    PERFORM FRM_GET_FIELDNAME."获取表字段类型等信息.
    IF P_IMPORT = ABAP_TRUE.
*      PERFORM FRM_CHECK_AUTH."检查权限
      PERFORM FRM_UPLOAD_DATA. " 从Excel上载数据_
    ELSE.
      PERFORM FRM_GET_DATA. "从数据库表中取得数据.
    ENDIF.
  ENDIF.
*$*$********************************************************************
*$*$    START-OF-SELECTION                                             *
*$*$********************************************************************
START-OF-SELECTION.

  PERFORM FRM_DISPLAY_DATA. "Display log data.

*$*$********************************************************************
*$*$    END-OF-SELECTION                                               *
*$*$********************************************************************
*&---------------------------------------------------------------------*
*&      Form  FRM_CHOOSE_INPUT_FILE
*&---------------------------------------------------------------------*
*       chose the input file path
*----------------------------------------------------------------------*

FORM FRM_CHOOSE_INPUT_FILE .

  DATA: I_FNAME TYPE STRING,
        IT_L_FILETABLE TYPE TABLE OF FILE_TABLE,
        I_RC TYPE I,
        I_TITLE TYPE STRING,
        I_ACTION TYPE I.
  I_TITLE = TEXT-002.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = I_TITLE
      DEFAULT_FILENAME        = '.xlsx'
      FILE_FILTER             = '*.*'
    CHANGING
      FILE_TABLE              = IT_L_FILETABLE
      RC                      = I_RC
      USER_ACTION             = I_ACTION
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    CLEAR I_FNAME.
  ELSE.
    IF I_ACTION = 0.
      READ TABLE IT_L_FILETABLE INDEX 1 INTO I_FNAME.
    ELSE.
      CLEAR I_FNAME.
    ENDIF.
  ENDIF.
  P_FILE = I_FNAME.

ENDFORM.                    " FRM_CHOOSE_INPUT_FILE
*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*        Upload data from Excel file..
*----------------------------------------------------------------------*

FORM FRM_UPLOAD_DATA .
  DATA:IT_L_UPLOAD  TYPE STANDARD TABLE OF ALSMEX_TABLINE
                                  WITH HEADER LINE,
       WA_L_UPLOAD TYPE ALSMEX_TABLINE,
       WA_L_LINE   TYPE REF TO DATA .
  DATA:I_FILENAME LIKE RLGRAP-FILENAME,
       I_DATE     LIKE SY-DATUM,
*       I_COL(2)   TYPE N,
       I_COL(4)   TYPE N,"20150731hp_sjf调整FIELDNAME长度
       I_VALUE    LIKE ZTPPI-PLNMG ,
       I_POS      TYPE I VALUE '4',
       I_WOTNR    TYPE  P,
       I_DISMM    LIKE MARC-DISMM,
       I_MATNR    LIKE MARC-MATNR,
       I_WERKS    LIKE MARC-WERKS.

  DATA: BEGIN OF WA_L_ZTPPI,
        Z_POSNR LIKE ZTPPI-Z_POSNR,
          WERKS LIKE ZTPPI-WERKS,
          MATNR LIKE ZTPPI-MATNR,
        END OF WA_L_ZTPPI.
  DATA:WA_L_DATA TYPE TY_DATA,
       IT_L_DATA TYPE STANDARD TABLE OF TY_DATA.
  CLEAR I_FILENAME.
  I_FILENAME = P_FILE.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = I_FILENAME
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = G_COL
      I_END_ROW               = 100000
    TABLES
      INTERN                  = IT_L_UPLOAD
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
    MESSAGE E011.
  ELSE.
    CLEAR I_COL.
***convert the excel table into data internal table.
    LOOP AT  IT_L_UPLOAD .
      MOVE-CORRESPONDING IT_L_UPLOAD TO WA_L_UPLOAD.
      IF WA_L_UPLOAD-ROW = 1 AND  WA_L_UPLOAD-COL > 4.
****  检查Excel 中数据列数不超过56列，总列数不超过60列

        IF WA_L_UPLOAD-COL > G_COL.
          MESSAGE E026.
          EXIT.
        ENDIF.

******检查日期格式....必须为YYYYMMDD
        CLEAR I_DATE.
        I_DATE = WA_L_UPLOAD-VALUE.
        CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
          EXPORTING
            DATE                      = I_DATE
          EXCEPTIONS
            PLAUSIBILITY_CHECK_FAILED = 1
            OTHERS                    = 2.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE 'E'  NUMBER SY-MSGNO
                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          EXIT.
        ELSE.
          CLEAR IT_DATE.
          IT_DATE-COL = WA_L_UPLOAD-COL.
          IT_DATE-DATE =  I_DATE.
          APPEND IT_DATE.
****构造内表结构
          CLEAR I_WOTNR.
          CALL FUNCTION 'DAY_IN_WEEK'
            EXPORTING
              DATUM = I_DATE
            IMPORTING
              WOTNR = I_WOTNR.
          CLEAR WA_WEEK.
          READ TABLE IT_WEEK INTO WA_WEEK WITH KEY WOTNR = I_WOTNR.
          IF SY-SUBRC = 0 .
            CONCATENATE WA_L_UPLOAD-VALUE '(' WA_WEEK-LANGT ')'
            INTO   WA_ALV_CAT-SCRTEXT_L .
          ENDIF.

          I_COL = I_COL + 1.
          CONCATENATE 'FD' I_COL INTO WA_ALV_CAT-FIELDNAME .
          WA_ALV_CAT-INTTYPE = WA_TABLE-INTTYPE.
          WA_ALV_CAT-INTLEN = WA_TABLE-INTLEN.
          WA_ALV_CAT-NO_ZERO = ABAP_TRUE.

          I_POS = I_POS + 1.
          WA_ALV_CAT-COL_POS = I_POS .
          APPEND WA_ALV_CAT TO IT_ALV_CAT.
          CLEAR WA_ALV_CAT.
        ENDIF.

      ELSEIF WA_L_UPLOAD-ROW > 1 .


        IF WA_L_UPLOAD-COL > 4.
          CLEAR I_VALUE. "将数量列转换为数字类型格式。
          CALL FUNCTION 'UNITS_STRING_CONVERT'
            EXPORTING
              UNITS_STRING = WA_L_UPLOAD-VALUE
              DCPFM        = G_DCPFM
            IMPORTING
              UNITS        = I_VALUE
            EXCEPTIONS
              INVALID_TYPE = 1
              OTHERS       = 2.
          IF SY-SUBRC NE 0.
            MESSAGE ID SY-MSGID TYPE 'E'  NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

          ENDIF.
        ENDIF.
*        ENDCASE.

        IF WA_L_UPLOAD-ROW = 2 AND WA_L_UPLOAD-COL = 1.

          SORT IT_ALV_CAT BY COL_POS.
          CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
            EXPORTING
              IT_FIELDCATALOG = IT_ALV_CAT
            IMPORTING
              EP_TABLE        = G_TABLE.
          " 用表类型指针 <dyn_table> 指向 数据对象的内容.
          ASSIGN G_TABLE->* TO <DYN_TABLE>.

          " 建立一个与动态内表结构相同的数据对象，且数据对象为是一个结构
          CREATE DATA WA_L_LINE LIKE LINE OF <DYN_TABLE>.
          ASSIGN WA_L_LINE->* TO <DYN_WA>.

        ENDIF.
        CLEAR WA_ALV_CAT.
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                              WITH KEY COL_POS = WA_L_UPLOAD-COL.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
           <DYN_WA> TO <DYN_FIELD>.
          IF WA_L_UPLOAD-COL > 4.
            <DYN_FIELD> = I_VALUE.
          ELSE.
            <DYN_FIELD> = WA_L_UPLOAD-VALUE.
          ENDIF.

        ENDIF.

        AT END OF ROW.
          APPEND <DYN_WA> TO <DYN_TABLE>.
          CLEAR <DYN_WA> .

        ENDAT.
      ENDIF.
    ENDLOOP.
    CLEAR IT_ZTPPI[].
    LOOP AT <DYN_TABLE> INTO <DYN_WA>.
      SORT IT_ALV_CAT BY COL_POS.
      LOOP AT IT_ALV_CAT INTO WA_ALV_CAT.
        ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
         <DYN_WA> TO <DYN_FIELD>.

        IT_ZTPPI-Z_VERSB = P_VERSB1.
        IT_ZTPPI-Z_VERNR = P_VERNR1.
        CASE WA_ALV_CAT-COL_POS .
          WHEN 1.
            IT_ZTPPI-Z_POSNR = <DYN_FIELD>.
          WHEN 2.
            IT_ZTPPI-WERKS = <DYN_FIELD>.
          WHEN 3.
            IT_ZTPPI-MATNR = <DYN_FIELD>.
            CLEAR I_MATNR.
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                INPUT        = IT_ZTPPI-MATNR
              IMPORTING
                OUTPUT       = I_MATNR
              EXCEPTIONS
                LENGTH_ERROR = 1
                OTHERS       = 2.
            TRANSLATE I_MATNR TO UPPER CASE.

            IT_ZTPPI-MATNR = I_MATNR.
          WHEN 4.
            IT_ZTPPI-MAKTX = <DYN_FIELD>.
          WHEN OTHERS.
            IT_ZTPPI-PLNMG = <DYN_FIELD>.
            CLEAR IT_DATE.
            READ TABLE IT_DATE WITH KEY COL = WA_ALV_CAT-COL_POS.
            IF SY-SUBRC = 0.
              IT_ZTPPI-PLDAT = IT_DATE-DATE.
              IT_ZTPPI-Z_COLN = WA_ALV_CAT-COL_POS.
            ENDIF.
            APPEND IT_ZTPPI.
        ENDCASE.
      ENDLOOP.
      CLEAR: WA_L_ZTPPI,WA_L_DATA.
      MOVE-CORRESPONDING IT_ZTPPI TO WA_L_ZTPPI.

*******必输项目的字段检查，获取更新行项目表的数据.

      IF WA_L_ZTPPI-Z_POSNR IS INITIAL.
        MESSAGE E027 WITH TEXT-004.
        EXIT.
      ENDIF.

      IF WA_L_ZTPPI-WERKS IS INITIAL.
        MESSAGE E027 WITH TEXT-005.
        EXIT.
      ELSE.
        PERFORM FRM_CHECK_AUTH USING WA_L_ZTPPI-WERKS."检查权限
      ENDIF.

      IF WA_L_ZTPPI-MATNR IS INITIAL.
        MESSAGE E027 WITH TEXT-007.
        EXIT.
      ELSE.
        ASSIGN COMPONENT 'MATNR' OF STRUCTURE
       <DYN_WA> TO <DYN_FIELD>.
        <DYN_FIELD> = WA_L_ZTPPI-MATNR.
        MODIFY <DYN_TABLE> FROM <DYN_WA>.
      ENDIF.
      READ TABLE IT_L_DATA INTO WA_L_DATA
                           WITH KEY WERKS = WA_L_ZTPPI-WERKS
                                    MATNR = WA_L_ZTPPI-MATNR.
      IF SY-SUBRC = 0."检查数据是否重复..
        MESSAGE E039 WITH WA_L_DATA-WERKS WA_L_DATA-MATNR.
      ELSE.
        CLEAR I_DISMM.
        SELECT SINGLE DISMM INTO I_DISMM
        FROM MARC  "检查物料主数据是否存在
        WHERE MATNR = WA_L_ZTPPI-MATNR
          AND WERKS = WA_L_ZTPPI-WERKS.
        IF SY-SUBRC = 0.
          IF ( I_DISMM = 'ND' ) OR ( I_DISMM = 'VD' ).
            MESSAGE E033 WITH WA_L_ZTPPI-MATNR.
            "检查工厂、物料号对应的MRP类型
          ENDIF.
        ELSE.
          MESSAGE E032 WITH WA_L_ZTPPI-WERKS WA_L_ZTPPI-MATNR.
        ENDIF.
      ENDIF.
      CLEAR WA_L_DATA.
      MOVE-CORRESPONDING WA_L_ZTPPI TO WA_L_DATA .
      APPEND WA_L_DATA TO IT_L_DATA.
      CLEAR: IT_ZTPPI,WA_L_DATA,WA_L_ZTPPI.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " FRM_UPLOAD_DATA

*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       Display log data.
*----------------------------------------------------------------------*

FORM FRM_DISPLAY_DATA .

  DATA:I_REPID LIKE SY-REPID.
  I_REPID = SY-REPID.


  WA_LAYOUT-CWIDTH_OPT = 'X'. " set optimized column width.
*  WA_LAYOUT-STYLEFNAME = 'Z_FSTYLE'. " field style.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM       = I_REPID
      I_CALLBACK_PF_STATUS_SET = 'STATUS_SET'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      IS_LAYOUT_LVC            = WA_LAYOUT
      IT_FIELDCAT_LVC          = IT_ALV_CAT
      I_DEFAULT                = 'X'
      I_SAVE                   = ' '
    TABLES
      T_OUTTAB                 = <DYN_TABLE>
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE E022.
  ENDIF.

ENDFORM.                    " FRM_DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_RADIOBUTTON_SELECT
*&---------------------------------------------------------------------*
*       Process Radio button selection
*----------------------------------------------------------------------*
FORM FRM_RADIOBUTTON_SELECT .

  LOOP AT SCREEN.
    IF P_IMPORT EQ ABAP_TRUE.
      IF SCREEN-GROUP1 = 'GP2'.
        SCREEN-INPUT = 0.
      ENDIF.

    ELSEIF P_QUERY EQ ABAP_TRUE.
      IF SCREEN-GROUP1 = 'GP1'.
        SCREEN-INPUT = 0.
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
FORM FRM_SELECTION_INPUTCHECK .
  DATA:I_VERSB LIKE ZTPPH-Z_VERSB.
  IF P_IMPORT = ABAP_TRUE.
    IF P_FILE IS INITIAL OR P_VERSB1 IS INITIAL.
      MESSAGE E028.
    ELSE.
      SELECT SINGLE Z_VERSB INTO I_VERSB FROM ZTPPH
      WHERE Z_VERSB = P_VERSB1
        AND Z_VERNR = P_VERNR1.
      IF SY-SUBRC = 0.
        MESSAGE E037 WITH P_VERSB1.
      ENDIF.
    ENDIF.
  ELSE.
    IF P_VERSB2 IS INITIAL.
      MESSAGE E038.
    ENDIF.
  ENDIF.

ENDFORM.                    " FRM_SELECTION_INPUTCHECK
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FIELDNAME
*&---------------------------------------------------------------------*
*       设置显示字段的属性
*----------------------------------------------------------------------*

FORM FRM_GET_FIELDNAME .

  DATA:IT_L_TABLE  LIKE TABLE OF DNTAB,
       WA_L_TABLE  LIKE DNTAB.
  CLEAR WA_L_TABLE.
  CALL FUNCTION 'NAMETAB_GET'
    EXPORTING
      LANGU          = SY-LANGU
      TABNAME        = 'ZTPPI'
    TABLES
      NAMETAB        = IT_L_TABLE
    EXCEPTIONS
      NO_TEXTS_FOUND = 1.
  CLEAR IT_ALV_CAT[].
*根据取出的字段目录生成参考字段目录
  LOOP AT IT_L_TABLE INTO WA_L_TABLE.

    WA_ALV_CAT-FIELDNAME = WA_L_TABLE-FIELDNAME.
    WA_ALV_CAT-INTTYPE = WA_L_TABLE-INTTYPE.
    WA_ALV_CAT-FIX_COLUMN = ABAP_TRUE.
    WA_ALV_CAT-INTLEN = WA_L_TABLE-INTLEN.
    WA_ALV_CAT-SCRTEXT_L = WA_L_TABLE-FIELDTEXT.

    CASE WA_L_TABLE-POSITION .
      WHEN '0004'.
        WA_ALV_CAT-COL_POS = '2'.
        APPEND WA_ALV_CAT TO IT_ALV_CAT.
        CLEAR WA_ALV_CAT.
      WHEN '0005'.
        WA_ALV_CAT-COL_POS = '3'.
        APPEND WA_ALV_CAT TO IT_ALV_CAT.
        CLEAR WA_ALV_CAT.
      WHEN '0008'.
        WA_ALV_CAT-COL_POS = '1'.
        APPEND WA_ALV_CAT TO IT_ALV_CAT.
        CLEAR WA_ALV_CAT.
      WHEN '0009'.
        WA_ALV_CAT-COL_POS = '4'.
        APPEND WA_ALV_CAT TO IT_ALV_CAT.
        CLEAR WA_ALV_CAT.
      WHEN '0010'."数据列类型
        MOVE-CORRESPONDING WA_L_TABLE TO WA_TABLE.

    ENDCASE.
  ENDLOOP.
****获取数字格式
  SELECT SINGLE DCPFM INTO G_DCPFM
        FROM USR01
        WHERE BNAME = SY-UNAME.
****获取星期描述
  CALL FUNCTION 'WEEKDAY_GET'
   EXPORTING
     LANGUAGE                = SY-LANGU
*     IMPORTING
*       RETURN_CODE             =
    TABLES
      WEEKDAY                 = IT_WEEK
     EXCEPTIONS
       WEEKDAY_NOT_FOUND       = 1
       OTHERS                  = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    " FRM_GET_FIELDNAME

*&---------------------------------------------------------------------*
*&      Form  STATUS_SET
*&---------------------------------------------------------------------*
*    设置按钮状态.
*----------------------------------------------------------------------*

FORM STATUS_SET USING PR_EXTAB TYPE SLIS_T_EXTAB .


  DELETE PR_EXTAB WHERE FCODE = '&DATA_SAVE'.


  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING PR_EXTAB[]
  OF PROGRAM SY-REPID.
ENDFORM.                    " STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*      点击保存后的处理
*----------------------------------------------------------------------*

FORM USER_COMMAND USING PR_UCOMM LIKE SY-UCOMM
                        PR_SELFIELD TYPE SLIS_SELFIELD .
  DATA:I_ANSWER TYPE C.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_GRID.

  CALL METHOD G_GRID->CHECK_CHANGED_DATA. "Get Refresh data
  IF PR_UCOMM = '&DATA_SAVE'.
    PERFORM FRM_PROCESS_DATA.
  ENDIF.

*  CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY. "Refresh data.

  IF IT_ERROR[] IS INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TITLEBAR              = '提示'
        TEXT_QUESTION         = TEXT-009
        TEXT_BUTTON_1         = '是'
*       ICON_BUTTON_1         = ' '
        TEXT_BUTTON_2         = '否'
*       ICON_BUTTON_2         = ' '
        DEFAULT_BUTTON        = '2'
        DISPLAY_CANCEL_BUTTON = ''
      IMPORTING
        ANSWER                = I_ANSWER
      EXCEPTIONS
        TEXT_NOT_FOUND        = 1
        OTHERS                = 2.
    IF I_ANSWER = '1'.
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

FORM FRM_PROCESS_DATA .
  DATA: I_MATNR   LIKE  BAPISITEMR-MATERIAL,
        I_WERKS   LIKE  BAPISITEMR-PLANT,
        I_REQNO   LIKE  BAPISITEMR-REQ_NUMBER,
        I_DISGR   LIKE MARC-DISGR,
        I_REQTYPE LIKE  BAPISITEMR-REQU_TYPE,
        I_TO      TYPE I,
        FLG_ERR   TYPE C,
        I_MAXDATE LIKE SY-DATUM.

  DATA:IT_L_SHIN TYPE STANDARD TABLE OF BAPISSHDIN,
       IT_L_RET  TYPE STANDARD TABLE OF  BAPIRETURN1 WITH HEADER LINE,
       WA_L_SHIN TYPE BAPISSHDIN,
       WA_L_ITEM TYPE BAPISITEMR.

*******构造内表IT_ZTPIRH.....

  CALL FUNCTION 'ENQUEUE_EZ_ZTPPH'
    EXPORTING
      MODE_ZTPPH     = 'E'
      MANDT          = SY-MANDT
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      OTHERS         = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'E'  NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ELSE.

    CALL FUNCTION 'ENQUEUE_EZ_ZTPPI'
      EXPORTING
        MODE_ZTPPI     = 'E'
        MANDT          = SY-MANDT
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE 'E'  NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.

      CLEAR WA_ZTPPH.
      WA_ZTPPH-Z_VERSB = P_VERSB1.
      WA_ZTPPH-Z_VERNR =  P_VERNR1.
      WA_ZTPPH-Z_DESCR = P_DESC.

      WA_ZTPPH-ANDAT = SY-DATUM.
      WA_ZTPPH-ANNAM = SY-UNAME.
      WA_ZTPPH-ANTMS = SY-UZEIT.


      DESCRIBE TABLE IT_ALV_CAT LINES I_TO.
      SORT IT_DATE BY DATE DESCENDING.
      READ TABLE IT_DATE INDEX 1.
      IF SY-SUBRC = 0.
        I_MAXDATE = IT_DATE-DATE.
      ENDIF.
      LOOP AT <DYN_TABLE> INTO <DYN_WA> .

        CLEAR:I_MATNR,I_WERKS,I_REQNO.
        CLEAR: WA_ALV_CAT,WA_L_ITEM,IT_ERROR.
*** 取得序号
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                          WITH KEY COL_POS = 1.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
           <DYN_WA> TO <DYN_FIELD>.
          IT_ERROR-Z_POSNR = <DYN_FIELD>.
        ENDIF.
*** 取得工厂值
        CLEAR: WA_ALV_CAT.
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                          WITH KEY COL_POS = 2.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
           <DYN_WA> TO <DYN_FIELD>.
          I_WERKS = <DYN_FIELD>.
          WA_L_ITEM-PLANT = I_WERKS.
          I_REQNO = I_WERKS.
          WA_L_ITEM-REQ_NUMBER = I_WERKS.
          IT_ERROR-WERKS = I_WERKS.
        ENDIF.
*****取得物料值...
        CLEAR WA_ALV_CAT.
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                          WITH KEY COL_POS = 3.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
           <DYN_WA> TO <DYN_FIELD>.
          I_MATNR = <DYN_FIELD>.
          WA_L_ITEM-MATERIAL = I_MATNR .
          IT_ERROR-MATNR = I_MATNR.
        ENDIF.
* *** 取得物料描述
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                          WITH KEY COL_POS = 4.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
           <DYN_WA> TO <DYN_FIELD>.
          IT_ERROR-MAKTX = <DYN_FIELD>.
        ENDIF.

*****取得数字列所有值，并写入BAPI内表.
        LOOP AT IT_ALV_CAT INTO WA_ALV_CAT FROM 5 TO I_TO.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
               <DYN_WA> TO <DYN_FIELD>.

          WA_L_SHIN-DATE_TYPE = '1'.
          WA_L_SHIN-REQ_QTY = <DYN_FIELD>.
          CLEAR IT_DATE.
          READ TABLE IT_DATE WITH KEY COL = WA_ALV_CAT-COL_POS.
          IF SY-SUBRC = 0.
            WA_L_SHIN-REQ_DATE =  IT_DATE-DATE.
          ENDIF.
          IF WA_L_SHIN-REQ_DATE >= SY-DATUM.
            APPEND WA_L_SHIN TO IT_L_SHIN.
            CLEAR WA_L_SHIN.
          ENDIF.
        ENDLOOP.
        CLEAR :I_DISGR,I_REQTYPE.
        SELECT SINGLE DISGR INTO I_DISGR
        FROM MARC
        WHERE MATNR = I_MATNR
          AND WERKS = I_WERKS.
        IF I_DISGR = '0011'.
          I_REQTYPE = 'BSF'.
        ELSEIF I_DISGR = '0010'.
          I_REQTYPE = 'LSF'.
        ELSE.
          I_REQTYPE = 'LSF'.
        ENDIF.
********调用修改BAPI
        CALL FUNCTION 'BAPI_REQUIREMENTS_CHANGE'
          EXPORTING
            MATERIAL                 = I_MATNR
            PLANT                    = I_WERKS
            REQUIREMENTSTYPE         = I_REQTYPE
            VERSION                  = '00'
            REQMTSPLANNUMBER         = I_REQNO
            VERS_ACTIV               = 'X'
            DO_COMMIT                = SPACE
            UPDATE_MODE              = 'X'
            DELETE_OLD               = 'X'
            NO_WITHDR                = 'X'
          TABLES
            REQUIREMENTS_SCHEDULE_IN = IT_L_SHIN
*           REQUIREMENTS_CHAR_IN     =
            RETURN                   = IT_L_RET.
        CLEAR IT_L_RET.
        READ TABLE IT_L_RET WITH KEY TYPE = 'E'
                                     ID = '6P'
                                     NUMBER = '017'.
        IF SY-SUBRC = 0. "生产计划需求不存在，需要新建.
          CLEAR IT_L_RET[].

          WA_L_ITEM-REQU_TYPE = I_REQTYPE.
          WA_L_ITEM-VERSION = '00'.
          WA_L_ITEM-VERS_ACTIV = 'X'.
          CALL FUNCTION 'BAPI_REQUIREMENTS_CREATE'
            EXPORTING
              REQUIREMENTS_ITEM        = WA_L_ITEM
              DO_COMMIT                = SPACE
              UPDATE_MODE              = 'X'
            TABLES
              REQUIREMENTS_SCHEDULE_IN = IT_L_SHIN
              RETURN                   = IT_L_RET.
          .
          CLEAR FLG_ERR.
****          错误处理...
          LOOP AT  IT_L_RET WHERE TYPE = 'E' OR TYPE = 'A'.
            IT_ERROR-MESSAGE = IT_L_RET-MESSAGE.
            APPEND IT_ERROR.
            FLG_ERR = ABAP_TRUE.
            EXIT.
          ENDLOOP.

          IF FLG_ERR IS INITIAL.
****            没有错误，更新行项目表的标识...
            PERFORM FRM_UPDATE_DBTABLE USING I_MATNR I_WERKS.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = 'X'.
******调用MD74删除与生产计划日期有重叠的客户需求数据 added 2013/8/14..

            PERFORM FRM_DELETE_CUREQ USING I_MATNR I_WERKS I_MAXDATE. "

          ENDIF.

        ELSE.
          CLEAR: FLG_ERR,IT_L_RET.
          LOOP AT  IT_L_RET WHERE TYPE = 'E' OR TYPE = 'A'.

            IT_ERROR-MESSAGE = IT_L_RET-MESSAGE.
            APPEND IT_ERROR.
            FLG_ERR = ABAP_TRUE.
            EXIT.
          ENDLOOP.
          IF FLG_ERR IS INITIAL.
****            没有错误，更新外挂表...
            PERFORM FRM_UPDATE_DBTABLE USING I_MATNR I_WERKS.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = 'X'.
******调用MD74删除与生产计划日期有重叠的客户需求数据 added 2013/8/14..

            PERFORM FRM_DELETE_CUREQ USING I_MATNR I_WERKS I_MAXDATE.
          ENDIF.
        ENDIF.
        CLEAR:IT_L_RET[],IT_L_SHIN[],WA_L_ITEM.
      ENDLOOP.

***** 更新外挂表数据
      MODIFY ZTPPH FROM WA_ZTPPH.
      IF SY-SUBRC = 0.
        MODIFY ZTPPI FROM TABLE IT_ZTPPI.
        IF SY-SUBRC NE 0.
          MESSAGE E035 WITH 'ZTPPI'.
        ENDIF.
      ELSE.
        MESSAGE E035 WITH 'ZTPPH'.
      ENDIF.

      CALL FUNCTION 'DEQUEUE_EZ_ZTPPI'
        EXPORTING
          MODE_ZTPPI = 'E'
          MANDT      = SY-MANDT.

    ENDIF.
    CALL FUNCTION 'DEQUEUE_EZ_ZTPPH'
      EXPORTING
        MODE_ZTPPH = 'E'
        MANDT      = SY-MANDT.

  ENDIF.

ENDFORM.                    " FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_UPDATE_DBTABLE
*&---------------------------------------------------------------------*
*      更新内表中标识
*----------------------------------------------------------------------*

FORM FRM_UPDATE_DBTABLE USING PR_MATNR PR_WERKS.


  LOOP AT IT_ZTPPI WHERE MATNR = PR_MATNR AND WERKS = PR_WERKS.
    IT_ZTPPI-Z_PIRCH = 'S'.
    MODIFY  IT_ZTPPI.
  ENDLOOP.

ENDFORM.                    " FRM_UPDATE_DBTABLE
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       从数据库中根据屏幕输入条件查询得到报表数据
*----------------------------------------------------------------------*

FORM FRM_GET_DATA .

  DATA:I_MAXCOL TYPE I,
       I_POS    TYPE I VALUE 4,
       I_WOTNR  TYPE  P,
       I_DATE   LIKE SY-DATUM,
       I_INDEX  TYPE I,
       I_COL(4) TYPE N VALUE 4.
  CLEAR IT_ZTPPI[].
  DATA:BEGIN OF IT_L_DATA OCCURS 0,
        Z_POSNR  LIKE ZTPPI-Z_POSNR,
        Z_COLN   LIKE ZTPPI-Z_COLN,
        WERKS    LIKE ZTPPI-WERKS,
        MATNR    LIKE ZTPPI-MATNR,
        PLDAT    LIKE ZTPPI-PLDAT,
        MAKTX    LIKE ZTPPI-MAKTX,
        PLNMG    LIKE ZTPPI-PLNMG,
       END OF IT_L_DATA.
  DATA: WA_L_DATA LIKE IT_L_DATA,
      WA_L_LINE   TYPE REF TO DATA ..
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTPPI
  FROM ZTPPI
  WHERE Z_VERSB = P_VERSB2
    AND WERKS IN S_WERKS
    AND MATNR IN S_MATNR.
  IF IT_ZTPPI[] IS INITIAL.
    MESSAGE E036.
  ELSE.
    SORT IT_ZTPPI BY Z_COLN DESCENDING.
    READ TABLE IT_ZTPPI INDEX 1.
    I_MAXCOL = IT_ZTPPI-Z_COLN.

    WHILE  I_POS < I_MAXCOL.
      I_COL = I_COL + 1.
      CONCATENATE 'FD' I_COL INTO WA_ALV_CAT-FIELDNAME .
      WA_ALV_CAT-INTTYPE = WA_TABLE-INTTYPE.
      WA_ALV_CAT-INTLEN = WA_TABLE-INTLEN.
      WA_ALV_CAT-NO_ZERO = ABAP_TRUE.
      I_POS = I_POS + 1.
      WA_ALV_CAT-COL_POS = I_POS .
      APPEND WA_ALV_CAT TO IT_ALV_CAT.
      CLEAR WA_ALV_CAT.
    ENDWHILE.
    LOOP AT IT_ZTPPI.
      MOVE-CORRESPONDING IT_ZTPPI TO IT_L_DATA.
      APPEND IT_L_DATA.
      CLEAR IT_L_DATA.
    ENDLOOP.
    SORT IT_ALV_CAT BY COL_POS.
*****  构造动态内表来显示数据
    CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
      EXPORTING
        IT_FIELDCATALOG = IT_ALV_CAT
      IMPORTING
        EP_TABLE        = G_TABLE.
    " 用表类型指针 <dyn_table> 指向 数据对象的内容.
    ASSIGN G_TABLE->* TO <DYN_TABLE>.

    " 建立一个与动态内表结构相同的数据对象，且数据对象为是一个结构
    CREATE DATA WA_L_LINE LIKE LINE OF <DYN_TABLE>.
    ASSIGN WA_L_LINE->* TO <DYN_WA>.


    SORT IT_L_DATA BY Z_POSNR Z_COLN.
    LOOP AT IT_L_DATA .
      MOVE-CORRESPONDING IT_L_DATA TO WA_L_DATA.
      CLEAR: WA_ALV_CAT,I_INDEX.
      READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                WITH KEY COL_POS =  WA_L_DATA-Z_COLN.
      IF SY-SUBRC = 0.
        I_INDEX = SY-TABIX.
        ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
      <DYN_WA> TO <DYN_FIELD>.

        <DYN_FIELD> = WA_L_DATA-PLNMG.
        CLEAR: I_WOTNR,I_DATE,WA_WEEK.
        I_DATE =  WA_L_DATA-PLDAT.

        CALL FUNCTION 'DAY_IN_WEEK'
          EXPORTING
            DATUM = I_DATE
          IMPORTING
            WOTNR = I_WOTNR.
        READ TABLE IT_WEEK INTO WA_WEEK WITH KEY WOTNR = I_WOTNR.
        IF SY-SUBRC = 0 .
          CONCATENATE I_DATE '(' WA_WEEK-LANGT ')'
          INTO   WA_ALV_CAT-SCRTEXT_L .
        ENDIF.

        MODIFY IT_ALV_CAT FROM WA_ALV_CAT INDEX I_INDEX.

      ENDIF.

      AT END OF Z_POSNR.
        CLEAR WA_ALV_CAT.
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                  WITH KEY COL_POS =  1.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
        <DYN_WA> TO <DYN_FIELD>.
          <DYN_FIELD> = WA_L_DATA-Z_POSNR.
        ENDIF.

        CLEAR WA_ALV_CAT.
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                  WITH KEY COL_POS =  2.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
        <DYN_WA> TO <DYN_FIELD>.
          <DYN_FIELD> = WA_L_DATA-WERKS.
        ENDIF.

        CLEAR WA_ALV_CAT.
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                  WITH KEY COL_POS =  3.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
        <DYN_WA> TO <DYN_FIELD>.
          <DYN_FIELD> = WA_L_DATA-MATNR.
        ENDIF.

        CLEAR WA_ALV_CAT.
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                  WITH KEY COL_POS =  4.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
        <DYN_WA> TO <DYN_FIELD>.
          <DYN_FIELD> = WA_L_DATA-MAKTX.
        ENDIF.
        APPEND <DYN_WA> TO <DYN_TABLE>.
        CLEAR <DYN_WA>.

      ENDAT.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " FRM_GET_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*    屏幕输出处理
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA:  I_CONTAINER TYPE SCRFNAME VALUE 'CS_ALV',
          I_GRID  TYPE REF TO CL_GUI_ALV_GRID,
          I_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
          IT_L_FIELD TYPE LVC_T_FCAT,
          WA_L_FIELD TYPE LVC_S_FCAT,
         WA_L_LAYOUT TYPE LVC_S_LAYO.
  CREATE OBJECT I_CUSTOM_CONTAINER
    EXPORTING
      CONTAINER_NAME = I_CONTAINER.
  CREATE OBJECT I_GRID
    EXPORTING
      I_PARENT = I_CUSTOM_CONTAINER.

  LOOP AT IT_ALV_CAT INTO WA_ALV_CAT FROM 1 TO 4.
    MOVE-CORRESPONDING WA_ALV_CAT TO WA_L_FIELD.
    APPEND WA_L_FIELD TO IT_L_FIELD.
    CLEAR WA_L_FIELD .
  ENDLOOP.

  WA_L_FIELD-FIELDNAME = 'MESSAGE' .
  WA_L_FIELD-INTTYPE = 'C'.
  WA_L_FIELD-INTLEN = 220.
  WA_L_FIELD-SCRTEXT_L = '文本消息'.
  WA_L_FIELD-COL_POS = 5.
  APPEND WA_L_FIELD TO IT_L_FIELD.
  CLEAR WA_L_FIELD.
  WA_L_LAYOUT-CWIDTH_OPT = 'X'. " set optimized column width.
  CALL METHOD I_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT       = WA_L_LAYOUT
    CHANGING
      IT_FIELDCATALOG = IT_L_FIELD
      IT_OUTTAB       = IT_ERROR[].


ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*      屏幕输入处理
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE G_CODE.
    WHEN 'ENDE'.
      LEAVE PROGRAM.
    WHEN '&F12'.
      CLEAR IT_ERROR[].
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

FORM FRM_GET_SEARCH_HELP .

  DATA: IT_L_VERSB LIKE ZTPPH OCCURS 0 WITH HEADER LINE.

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE IT_L_VERSB
  FROM ZTPPH.
  SORT IT_L_VERSB BY Z_VERSB.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD         = 'Z_VERSB'
      DYNPPROG         = SY-REPID
      DYNPNR           = SY-DYNNR
      DYNPROFIELD      = 'P_VERSB'
      VALUE_ORG        = 'S'
      CALLBACK_PROGRAM = SY-REPID
    TABLES
      VALUE_TAB        = IT_L_VERSB
    EXCEPTIONS
      PARAMETER_ERROR  = 1
      NO_VALUES_FOUND  = 2
      OTHERS           = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY  NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
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
FORM FRM_DELETE_CUREQ  USING    PR_MATNR
                                PR_WERKS
                                PR_MAXDATE.
  RANGES:R_MATNR FOR RM60R-MATNR,
         R_WERKS FOR RM60R-WERKS,
         R_VERSB FOR RM60R-VERSB.
  R_MATNR-SIGN = 'I'.
  R_MATNR-OPTION = 'EQ'.
  R_MATNR-LOW = PR_MATNR.
  APPEND R_MATNR.

  R_WERKS-SIGN = 'I'.
  R_WERKS-OPTION = 'EQ'.
  R_WERKS-LOW = PR_WERKS.
  APPEND R_WERKS.

  R_VERSB-SIGN = 'I'.
  R_VERSB-OPTION = 'EQ'.
  R_VERSB-LOW = '01'.
  APPEND R_VERSB.



  SUBMIT RM60RR20
             WITH WERKS IN R_WERKS
             WITH MATNR IN R_MATNR
             WITH VERSB IN R_VERSB
             WITH DATE1 INCL  PR_MAXDATE
             WITH HISTFLAG INCL 'X'
             WITH INACFLAG INCL SPACE
             WITH LISTFLAG INCL SPACE
             WITH TESTFLAG INCL SPACE
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
FORM FRM_CHECK_AUTH USING PR_WERKS.

*  DATA: BEGIN OF IT_L_WERKS OCCURS 0,
*         WERKS LIKE T001W-WERKS,
*        END OF IT_L_WERKS.
*  SELECT WERKS INTO CORRESPONDING FIELDS OF TABLE IT_L_WERKS
*  FROM T001W
*  WHERE WERKS IN S_WERKS.
*  LOOP AT IT_L_WERKS.
  AUTHORITY-CHECK OBJECT 'C_PPBD' " check authorization for QP01
  ID 'WERKS' FIELD PR_WERKS
  ID 'AKTTYP' FIELD 'H'.
  IF SY-SUBRC <> 0.
    MESSAGE E063 WITH SY-UNAME PR_WERKS.
    EXIT.
  ENDIF.
*  ENDLOOP.

ENDFORM.                    " FRM_CHECK_AUTH
