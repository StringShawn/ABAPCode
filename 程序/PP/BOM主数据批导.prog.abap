*$*$********************************************************************
* Program ID/Name:ZPPR006                 Date written:2013/7/16
* Author's name: HP_FCG                   Last update:2013/7/23
* Program title:BOM主数据批量导入
* Project Name:  EPR I
* Version:V0
* Function Spec ID:PP_01_06
*----------------------------------------------------------------------*
* Description: (Incl. Related Function Area and System)
*              用户维护好Excel文件后复制到txt文件，再上载到SAP，
*              批量创建BOM主数据
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

REPORT  ZPPR006 MESSAGE-ID ZPP_MSG.
*$*$********************************************************************
*$*$    TABLES                                                         *
*$*$********************************************************************

*$*$********************************************************************
*$*$    INCLUDES                                                       *
*$*$    (FOR INCLUDES WITH DECLARATIVE PURPOSES ONLY                   *
*$*$     I.E. BDC, ALV, ETC.)                                          *
*$*$********************************************************************

*$*$********************************************************************
*$*$    GLOBAL TYPES                                                   *
*$*$********************************************************************

*$*$********************************************************************
*$*$    GLOBAL CONSTANTS                                               *
*$*$********************************************************************
CONSTANTS: C_FIELDX     TYPE C VALUE 'X',
           C_USAGE      TYPE STLAN VALUE '1'.
*$*$*********************************************y**********************
*
*$*$    GLOBAL ELEMENTARY VARIABLES                                    *
*$*$********************************************************************
DATA:G_DATE LIKE SY-DATUM.
*$*$********************************************************************
*$*$    GLOBAL STRUCTURES                                              *
*$*$********************************************************************
****alv structure
DATA:WA_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
       WA_LAYOUT TYPE SLIS_LAYOUT_ALV.
*$*$********************************************************************
*$*$    GLOBAL INTERNAL TABLES                                         *
*$*$********************************************************************
DATA:BEGIN OF IT_DATA OCCURS 0,
      WERKS  LIKE MAST-WERKS, "工厂
      MATNR  LIKE MAST-MATNR,"父项物料号
      MAKTX  LIKE MAKT-MAKTX, "父项物料描述
      POSNR  LIKE STPO-POSNR, "  子项行项目号
      IDNRK  LIKE STPO-IDNRK, "子项物料号
      IDMKT  LIKE MAKT-MAKTX, "子项物料描述
      MENGE  LIKE STPO-MENGE," 子项数量
      RPROD  TYPE C,         "生产无关
      RCOST  TYPE C,         "成本无关
     END OF IT_DATA.

DATA:BEGIN OF IT_OUT OCCURS 0,
      WERKS  LIKE MAST-WERKS, "工厂
      MATNR  LIKE MAST-MATNR,"父项物料号
      MAKTX  LIKE MAKT-MAKTX, "父项物料描述
      POSNR  LIKE STPO-POSNR, "  子项行项目号
      IDNRK  LIKE STPO-IDNRK, "子项物料号
      IDMKT  LIKE MAKT-MAKTX, "子项物料描述
      MENGE  LIKE STPO-MENGE," 子项数量
      RPROD  TYPE C,         "生产无关
      RCOST  TYPE C,         "成本无关
      MSGTY  LIKE BAPIE1RET2-TYPE, "消息类型
     MESSAGE LIKE BAPIE1RET2-MESSAGE,  " 执行结果消息

END OF IT_OUT.

DATA:BEGIN OF IT_OUT_L OCCURS 0,
      MSGLT  LIKE D346I-ICON_ID,
      WERKS  LIKE MAST-WERKS, "工厂
      MATNR  LIKE MAST-MATNR,"父项物料号
      MAKTX  LIKE MAKT-MAKTX, "父项物料描述
      POSNR  LIKE STPO-POSNR, "  子项行项目号
      IDNRK  LIKE STPO-IDNRK, "子项物料号
      IDMKT  LIKE MAKT-MAKTX, "子项物料描述
      MENGE  LIKE STPO-MENGE," 子项数量
      RPROD  TYPE C,         "生产无关
      RCOST  TYPE C,         "成本无关
     MESSAGE LIKE BAPIE1RET2-MESSAGE,  " 执行结果消息
END OF IT_OUT_L.

*****alv field category.
DATA:IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
*$*$********************************************************************
*$*$    MACROS                                                         *
*$*$********************************************************************
DEFINE  CHECK_MANDTORY.

  IF &1 IS INITIAL.
    IT_OUT-MSGTY = 'E'.
    MESSAGE E016 WITH &2
 INTO IT_OUT-MESSAGE.
    APPEND IT_OUT.
    CLEAR IT_OUT.
    CONTINUE.
  ENDIF.

END-OF-DEFINITION.



*$*$********************************************************************
*$*$    GLOBAL FIELD-SYMBOLS                                           *
*$*$********************************************************************

*$*$********************************************************************
*$*$    CLASSES                                                        *
*$*$********************************************************************

*$*$********************************************************************
*$*$    PARAMETERS & SELECT-OPTIONS                                    *
*$*$********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_FILE    TYPE STRING OBLIGATORY,
            P_DATUV   LIKE RC29N-DATUV OBLIGATORY DEFAULT G_DATE.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS            P_TEST    AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B2.

LOAD-OF-PROGRAM.
  PERFORM FRM_GET_FIRSTDAY_OF_YEAR. "取当年第一天
*$*$********************************************************************
*$*$    INITIALIZATION                                                 *
*$*$********************************************************************

*$*$********************************************************************
*$*$    AT SELECTION-SCREEN ON                                         *
*$*$********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM FRM_CHOOSE_INPUT_FILE .
*$*$********************************************************************
*$*$    AT SELECTION-SCREEN OUTPUT                                     *
*$*$********************************************************************

*$*$********************************************************************
*$*$    AT SELECTION-SCREEN                                            *
*$*$********************************************************************

*$*$********************************************************************
*$*$    START-OF-SELECTION                                             *
*$*$********************************************************************
START-OF-SELECTION.

  PERFORM FRM_UPLOAD_DATA. " Upload data from Excel file..
  PERFORM FRM_CREATE_BOM. " Call BAP to Create Bom
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

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME                = P_FILE
      FILETYPE                = 'ASC'
      HAS_FIELD_SEPARATOR     = 'X'
*     codepage                = '8400'
*     DAT_MODE                = 'X'
    TABLES
      DATA_TAB                = IT_DATA
    EXCEPTIONS
      FILE_OPEN_ERROR         = 1
      FILE_READ_ERROR         = 2
      NO_BATCH                = 3
      GUI_REFUSE_FILETRANSFER = 4
      INVALID_TYPE            = 5
      NO_AUTHORITY            = 6
      UNKNOWN_ERROR           = 7
      BAD_DATA_FORMAT         = 8
      HEADER_NOT_ALLOWED      = 9
      SEPARATOR_NOT_ALLOWED   = 11
      HEADER_TOO_LONG         = 11
      UNKNOWN_DP_ERROR        = 12
      ACCESS_DENIED           = 13
      DP_OUT_OF_MEMORY        = 14
      DISK_FULL               = 15
      DP_TIMEOUT              = 16
      OTHERS                  = 17.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
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
**  ****** Build the fieldcat for ALV display.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = I_REPID
      I_INTERNAL_TABNAME     = 'IT_OUT_L'
*     I_STRUCTURE_NAME       =
*     I_CLIENT_NEVER_DISPLAY = 'X'
      I_INCLNAME             = I_REPID
*      I_BYPASSING_BUFFER     = 'X'
*     I_BUFFER_ACTIVE        =
    CHANGING
      CT_FIELDCAT            = IT_FIELDCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.
  IF SY-SUBRC <> 0.
    MESSAGE E007.
  ENDIF.

*
  READ TABLE IT_FIELDCAT INTO WA_FIELDCAT INDEX 1.
  WA_FIELDCAT-SELTEXT_L = TEXT-012.
  WA_FIELDCAT-DDICTXT = 'L'.
  MODIFY IT_FIELDCAT FROM WA_FIELDCAT INDEX 1.

    READ TABLE IT_FIELDCAT INTO WA_FIELDCAT INDEX 9.
  WA_FIELDCAT-SELTEXT_L = TEXT-010.
  WA_FIELDCAT-DDICTXT = 'L'.
  MODIFY IT_FIELDCAT FROM WA_FIELDCAT INDEX 9.

  READ TABLE IT_FIELDCAT INTO WA_FIELDCAT INDEX 10.
  WA_FIELDCAT-SELTEXT_L = TEXT-011.
  WA_FIELDCAT-DDICTXT = 'L'.
  MODIFY IT_FIELDCAT FROM WA_FIELDCAT INDEX 10.


  WA_LAYOUT-COLWIDTH_OPTIMIZE = 'X'. " set optimized column width.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
     I_CALLBACK_PROGRAM                = I_REPID
*    I_CALLBACK_PF_STATUS_SET          = ' '
*    I_CALLBACK_USER_COMMAND           = ' '
*    I_CALLBACK_TOP_OF_PAGE            = ' '
*    I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*    I_CALLBACK_HTML_END_OF_LIST       = ' '
*    I_STRUCTURE_NAME                  =
*    I_BACKGROUND_ID                   = ' '
*    I_GRID_TITLE                      =
*    I_GRID_SETTINGS                   =
     IS_LAYOUT                         = WA_LAYOUT
     IT_FIELDCAT                       = IT_FIELDCAT
*    IT_EXCLUDING                      =
*    IT_SPECIAL_GROUPS                 =
*    IT_SORT                           =
*    IT_FILTER                         =
*  IMPORTING
*    E_EXIT_CAUSED_BY_CALLER           =
*    ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = IT_OUT_L
   EXCEPTIONS
     PROGRAM_ERROR                     = 1
     OTHERS                            = 2 .
  IF SY-SUBRC <> 0.
    MESSAGE E007.
  ENDIF.


ENDFORM.                    " FRM_DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CREATE_BOM .

***  局部变量定义
  DATA:I_MATNR    LIKE CSAP_MBOM-MATNR,
       I_WERKS    LIKE CSAP_MBOM-WERKS,
       I_USAGE    LIKE CSAP_MBOM-STLAN,
       I_DATUV    LIKE CSAP_MBOM-DATUV,

       I_FROM     LIKE SY-TABIX,
       I_TO       LIKE SY-TABIX,
       I_MSGTY    LIKE BAPIE1RET2-TYPE,
       I_MESSAGE  LIKE BAPIE1RET2-MESSAGE,
       FLG_ERROR  TYPE C.
  DATA:WA_L_STKO  LIKE STKO_API01,
       WA_L_STPO  LIKE STPO_API01,
       IT_L_STPO  TYPE STANDARD TABLE OF STPO_API01.


  LOOP AT IT_DATA.


    AT NEW MATNR.
      CLEAR I_FROM.
      I_FROM = SY-TABIX.
      CLEAR: FLG_ERROR,I_MESSAGE.
    ENDAT.
    IF FLG_ERROR = ABAP_TRUE.
      MOVE-CORRESPONDING IT_DATA TO IT_OUT.
      IT_OUT-MSGTY = 'E'.
      IT_OUT-MESSAGE = I_MESSAGE.
      APPEND IT_OUT.
      CLEAR IT_OUT.
      CONTINUE.
    ENDIF.

*    ***convert material number...
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT        = IT_DATA-MATNR
      IMPORTING
        OUTPUT       = IT_DATA-MATNR
      EXCEPTIONS
        LENGTH_ERROR = 1
        OTHERS       = 2.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT        = IT_DATA-IDNRK
      IMPORTING
        OUTPUT       = IT_DATA-IDNRK
      EXCEPTIONS
        LENGTH_ERROR = 1
        OTHERS       = 2.
    CALL FUNCTION 'CONVERSION_EXIT_NUMCV_INPUT'
      EXPORTING
        INPUT  = IT_DATA-POSNR
      IMPORTING
        OUTPUT = IT_DATA-POSNR.

    TRANSLATE IT_DATA-WERKS TO UPPER CASE.
    TRANSLATE IT_DATA-MATNR TO UPPER CASE.
    TRANSLATE IT_DATA-IDNRK TO UPPER CASE.



    CHECK_MANDTORY IT_DATA-WERKS TEXT-003." 工厂
    CHECK_MANDTORY IT_DATA-MATNR TEXT-004."父项物料
*    CHECK_MANDTORY IT_DATA-MAKTX TEXT-005. "父项物料描述
    CHECK_MANDTORY IT_DATA-POSNR TEXT-006. "子项行项目号
    CHECK_MANDTORY IT_DATA-IDNRK TEXT-007. "子项物料号
*    CHECK_MANDTORY IT_DATA-MAKTX TEXT-008. "子项物料描述
    CHECK_MANDTORY IT_DATA-MENGE TEXT-009." 子项数量

****取父项物料描述
    SELECT SINGLE MAKTX INTO IT_DATA-MAKTX
    FROM MAKT
    WHERE MATNR = IT_DATA-MATNR
     AND SPRAS = SY-LANGU.


****取子项物料描述
    SELECT SINGLE MAKTX INTO IT_DATA-IDMKT
    FROM MAKT
    WHERE MATNR = IT_DATA-IDNRK
     AND SPRAS = SY-LANGU.
    MOVE-CORRESPONDING IT_DATA TO IT_OUT.
****检查物料是否存在.....
    CLEAR: I_MATNR.
    SELECT SINGLE MATNR INTO I_MATNR
    FROM MARC
    WHERE MATNR = IT_DATA-MATNR
      AND WERKS = IT_DATA-WERKS.
    IF I_MATNR IS INITIAL.
      IT_OUT-MSGTY = 'E'.
      MESSAGE E023 WITH IT_DATA-MATNR IT_DATA-WERKS INTO IT_OUT-MESSAGE.
      APPEND IT_OUT.
      CLEAR IT_OUT.
      CONTINUE.

    ENDIF.
    CLEAR: I_MATNR,I_MESSAGE.
    SELECT SINGLE MATNR INTO I_MATNR
    FROM MARC
    WHERE MATNR = IT_DATA-IDNRK
      AND WERKS = IT_DATA-WERKS.
    IF I_MATNR IS INITIAL.
      IT_OUT-MSGTY = 'E'.
      MESSAGE E024 WITH IT_DATA-IDNRK IT_DATA-WERKS INTO IT_OUT-MESSAGE.
      APPEND IT_OUT.
      FLG_ERROR = ABAP_TRUE.
      I_MESSAGE = IT_OUT-MESSAGE.
      CLEAR IT_OUT.
      CONTINUE.
    ENDIF.

    CLEAR WA_L_STPO.
    WA_L_STPO-ITEM_CATEG = 'L'.
    WA_L_STPO-ITEM_NO = IT_OUT-POSNR.
    WA_L_STPO-COMPONENT = IT_OUT-IDNRK.
    WA_L_STPO-COMP_QTY = IT_OUT-MENGE.

    TRANSLATE IT_OUT-RPROD TO UPPER CASE.
    IF IT_OUT-RPROD = ABAP_TRUE.

      WA_L_STPO-REL_PROD = SPACE.
    ELSE.
      WA_L_STPO-REL_PROD = C_FIELDX.
    ENDIF.

    TRANSLATE IT_OUT-RCOST TO UPPER CASE.
    IF IT_OUT-RCOST = ABAP_TRUE.
      WA_L_STPO-REL_COST = SPACE.
    ELSE.
      WA_L_STPO-REL_COST = C_FIELDX.
    ENDIF.
    WA_L_STPO-REL_ENGIN = C_FIELDX.

    APPEND WA_L_STPO TO IT_L_STPO.
    APPEND IT_OUT.
    CLEAR IT_OUT.
    AT END OF MATNR.

      CLEAR I_TO .
      I_TO = SY-TABIX.

*** 1.检查BOM是否已经存在....
      CLEAR I_MATNR.
      SELECT SINGLE MATNR INTO I_MATNR
      FROM MAST
      WHERE MATNR = IT_DATA-MATNR
        AND WERKS = IT_DATA-WERKS
        AND STLAN = C_USAGE.

      IF NOT I_MATNR IS INITIAL.
        CLEAR:I_MSGTY,I_MESSAGE.
        I_MSGTY = 'E'.
        MESSAGE E018 WITH IT_DATA-WERKS IT_DATA-MATNR
        INTO I_MESSAGE.
        LOOP AT IT_OUT FROM I_FROM TO I_TO.
          IT_OUT-MSGTY = I_MSGTY.
          IT_OUT-MESSAGE = I_MESSAGE.
          MODIFY IT_OUT.
        ENDLOOP.
*        MODIFY  IT_OUT TRANSPORTING MSGTY MESSAGE
*                       WHERE WERKS = IT_DATA-WERKS
*                         AND MATNR = IT_DATA-MATNR .
      ELSE.
        IF P_TEST IS INITIAL.
          CLEAR WA_L_STKO.
          WA_L_STKO-BASE_QUAN = '1'.
          WA_L_STKO-BOM_STATUS = '01'.
          CLEAR: I_MATNR,I_WERKS,I_USAGE,I_DATUV.
          I_MATNR = IT_DATA-MATNR.
          I_WERKS = IT_DATA-WERKS.
          I_USAGE = C_USAGE.
          I_DATUV = P_DATUV.
          CALL FUNCTION 'CSAP_MAT_BOM_CREATE'
            EXPORTING
              MATERIAL   = I_MATNR
              PLANT      = I_WERKS
              BOM_USAGE  = I_USAGE
              VALID_FROM = I_DATUV
              I_STKO     = WA_L_STKO
            TABLES
              T_STPO     = IT_L_STPO
            EXCEPTIONS
              ERROR      = 1
              OTHERS     = 2.
          CLEAR:I_MSGTY,I_MESSAGE.
          IF SY-SUBRC <> 0.
            I_MSGTY = 'E'.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
           INTO I_MESSAGE.
          ELSE.
            I_MSGTY = 'S'.
            MESSAGE E019 WITH IT_DATA-WERKS IT_DATA-MATNR
            INTO I_MESSAGE.
          ENDIF.

          LOOP AT IT_OUT FROM I_FROM TO I_TO.
            IT_OUT-MSGTY = I_MSGTY.
            IT_OUT-MESSAGE = I_MESSAGE.
            MODIFY IT_OUT.
          ENDLOOP.

*          MODIFY  IT_OUT TRANSPORTING MSGTY MESSAGE
*                       WHERE WERKS = IT_DATA-WERKS
*                         AND MATNR = IT_DATA-MATNR .

        ELSE.
          CLEAR:I_MSGTY,I_MESSAGE.
          I_MSGTY = 'S'.
          MESSAGE E020 WITH IT_DATA-WERKS IT_DATA-MATNR
          INTO I_MESSAGE.
          LOOP AT IT_OUT FROM I_FROM TO I_TO.
            IT_OUT-MSGTY = I_MSGTY.
            IT_OUT-MESSAGE = I_MESSAGE.
            MODIFY IT_OUT.
          ENDLOOP.

*
*          MODIFY  IT_OUT TRANSPORTING MSGTY MESSAGE
*                      WHERE WERKS = IT_DATA-WERKS
*                        AND MATNR = IT_DATA-MATNR .
        ENDIF.

      ENDIF.
      CLEAR: IT_OUT,IT_L_STPO[].
    ENDAT.
  ENDLOOP.

  SORT IT_OUT BY MSGTY WERKS MATNR IDNRK.
  LOOP AT IT_OUT.
    MOVE-CORRESPONDING IT_OUT TO IT_OUT_L.
    IF IT_OUT-MSGTY = 'E'.
      IT_OUT_L-MSGLT = '@5C@'.
    ELSE.
      IT_OUT_L-MSGLT = '@5B@'.
    ENDIF.
    APPEND IT_OUT_L.
    CLEAR IT_OUT_L.
  ENDLOOP.


ENDFORM.                    " FRM_CREATE_BOM
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FIRSTDAY_OF_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_FIRSTDAY_OF_YEAR .
  G_DATE = SY-DATUM.
  G_DATE+4(4) ='0101'.
ENDFORM.                    " FRM_GET_FIRSTDAY_OF_YEAR
