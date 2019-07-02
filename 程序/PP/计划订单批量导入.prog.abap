*$*$********************************************************************
* Program ID/Name:ZPPR011                 Date written:2013.07.22
* Author's name:HP_SJF                    Last update:2013.07.22
* Program title:客户需求导入
* Project Name:  ZFSS SAP PROJECT
* Version:
* Function Spec ID:ZFSS_SAP_FS_PP_01_18_计划订单批量导入_V0.2.doc
*----------------------------------------------------------------------*
* Description:
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
* 20150819   |  HP_SJF        |             |   INIT.
*            |                |             |
*$*$********************************************************************
REPORT ZPPR029 MESSAGE-ID ZPP_MSG.

*$*$********************************************************************
*$*$    TABLES                                                         *
*$*$********************************************************************
TABLES: MARC.
*$*$********************************************************************
*$*$    INCLUDES                                                       *
*$*$    (FOR INCLUDES WITH DECLARATIVE PURPOSES ONLY                   *
*$*$     I.E. BDC, ALV, ETC.)                                          *
*$*$********************************************************************

*$*$********************************************************************
*$*$    GLOBAL TYPES                                                   *
*$*$********************************************************************
TYPES: BEGIN OF TY_DATA,
         PLWRK LIKE ZTPLDORDI-PLWRK,
         PWWRK LIKE ZTPLDORDI-PWWRK,
         MATNR LIKE ZTPLDORDI-MATNR,
       END OF TY_DATA.
*$*$********************************************************************
*$*$    GLOBAL CONSTANTS                                               *
*$*$********************************************************************

*$*$********************************************************************
*$*$    GLOBAL ELEMENTARY VARIABLES                                    *
*$*$********************************************************************


DATA: G_TABLE  TYPE REF TO DATA.
DATA: G_DCPFM  LIKE USR01-DCPFM.
DATA: G_GRID   TYPE REF TO CL_GUI_ALV_GRID.
DATA  G_CODE   LIKE SY-UCOMM.
*$*$********************************************************************
*$*$    GLOBAL STRUCTURES                                              *
*$*$********************************************************************
****alv structure
DATA:  WA_LAYOUT      TYPE LVC_S_LAYO,
       WA_ALV_CAT     TYPE LVC_S_FCAT,
       WA_TABLE       LIKE DNTAB,
       WA_WEEK        LIKE T246,
       WA_ZTPLDORDH   TYPE ZTPLDORDH .
*$*$********************************************************************
*$*$    GLOBAL INTERNAL TABLES                                         *
*$*$********************************************************************
DATA: IT_WEEK      TYPE STANDARD TABLE OF T246.
DATA: IT_ZTPLDORDI TYPE STANDARD TABLE OF ZTPLDORDI WITH HEADER LINE.
DATA: IT_DATA      TYPE STANDARD TABLE OF TY_DATA,
      WA_DATA      TYPE TY_DATA.

DATA: BEGIN OF IT_DATE OCCURS 0,
       COL   TYPE I,
       DATE  LIKE SY-DATUM,
      END OF IT_DATE.

DATA: BEGIN OF IT_ERROR OCCURS 0,
        Z_POSNR LIKE ZTPLDORDI-Z_POSNR,
        PLWRK   LIKE ZTPLDORDI-PLWRK,
        PWWRK   LIKE ZTPLDORDI-PWWRK,
        MATNR   LIKE ZTPLDORDI-MATNR,
        MAKTX   LIKE ZTPLDORDI-MAKTX,
        MESSAGE TYPE STRING,
      END OF IT_ERROR.
*****alv field category.
DATA: IT_ALV_CAT TYPE LVC_T_FCAT.

DATA: G_COL      TYPE I VALUE 255.
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
            P_VERSB1 LIKE ZTPLDORDH-Z_VERSB MODIF ID GP1,
            P_VERNR1 LIKE ZTPLDORDH-Z_VERNR DEFAULT '1' MODIF ID GP1
                                         NO-DISPLAY,
            P_DESC LIKE ZTPLDORDH-Z_DESCR MODIF ID GP1.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS: S_WERKS FOR MARC-WERKS MODIF ID GP2,
                S_MATNR FOR MARC-MATNR MODIF ID GP2.
PARAMETERS: P_VERSB2 LIKE ZTPLDORDH-Z_VERSB MODIF ID GP2,
            P_VERNR2 LIKE ZTPLDORDH-Z_VERNR MODIF ID GP2 DEFAULT '1'
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

      PERFORM FRM_UPLOAD_DATA. " 从Excel上载数据
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
  DATA:I_FILENAME  LIKE RLGRAP-FILENAME,
       I_DATE      LIKE SY-DATUM,
       I_COL(4)    TYPE N,
       I_VALUE     TYPE GSMNG,
       I_POS       TYPE I VALUE '5',
       I_COL_POS   TYPE I,
       I_WOTNR     TYPE P,
       I_DISMM     LIKE MARC-DISMM,
       I_MATNR     LIKE MARC-MATNR.

  DATA: BEGIN OF WA_L_ZTPLDORDI,
        Z_POSNR LIKE ZTPLDORDI-Z_POSNR,
        PLWRK   LIKE ZTPLDORDI-PLWRK,
        PWWRK   LIKE ZTPLDORDI-PWWRK,
        MATNR   LIKE ZTPLDORDI-MATNR,
        END OF WA_L_ZTPLDORDI.

  DATA: LS_EORD   TYPE EORD.

***清空所有全局变量表
  CLEAR: IT_ZTPLDORDI, IT_DATA.

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
    LOOP AT IT_L_UPLOAD .
      MOVE-CORRESPONDING IT_L_UPLOAD TO WA_L_UPLOAD.
      IF WA_L_UPLOAD-ROW = 1 AND  WA_L_UPLOAD-COL > 5.
****  检查Excel中数据列数不超过XX列，总列数不超过255列

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
          IT_DATE-COL  = WA_L_UPLOAD-COL.
          IT_DATE-DATE = I_DATE.
          APPEND IT_DATE.
****构造内表结构
          CLEAR I_WOTNR.
          CALL FUNCTION 'DAY_IN_WEEK'
            EXPORTING
              DATUM = I_DATE
            IMPORTING
              WOTNR = I_WOTNR.
          READ TABLE IT_WEEK INTO WA_WEEK WITH KEY WOTNR = I_WOTNR.
          IF SY-SUBRC = 0 .
            CONCATENATE WA_L_UPLOAD-VALUE '(' WA_WEEK-LANGT ')'
            INTO   WA_ALV_CAT-SCRTEXT_L .
          ENDIF.

          I_COL = I_COL + 1.
          CONCATENATE 'FD' I_COL INTO WA_ALV_CAT-FIELDNAME .
          WA_ALV_CAT-INTTYPE = WA_TABLE-INTTYPE.
          WA_ALV_CAT-INTLEN  = WA_TABLE-INTLEN.
          WA_ALV_CAT-NO_ZERO = ABAP_TRUE.

          I_POS = I_POS + 1.
          WA_ALV_CAT-COL_POS = I_POS .
          APPEND WA_ALV_CAT TO IT_ALV_CAT.
          CLEAR WA_ALV_CAT.
        ENDIF.

      ELSEIF WA_L_UPLOAD-ROW > 1 .

        IF WA_L_UPLOAD-COL > 5.
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
        CLEAR:I_COL_POS, WA_ALV_CAT.
        I_COL_POS = WA_L_UPLOAD-COL .
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                              WITH KEY COL_POS = I_COL_POS.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
           <DYN_WA> TO <DYN_FIELD>.
          IF WA_L_UPLOAD-COL > 5.
            <DYN_FIELD> = I_VALUE.
          ELSE.
            <DYN_FIELD> = WA_L_UPLOAD-VALUE.
          ENDIF.
        ENDIF.
        AT END OF ROW.
          APPEND <DYN_WA> TO <DYN_TABLE>.
          CLEAR:<DYN_WA>.
        ENDAT.
      ENDIF.
    ENDLOOP.
    CLEAR IT_ZTPLDORDI[].
    LOOP AT <DYN_TABLE> INTO <DYN_WA>.
      SORT IT_ALV_CAT BY COL_POS.
      LOOP AT IT_ALV_CAT INTO WA_ALV_CAT.
        ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
         <DYN_WA> TO <DYN_FIELD>.

        IT_ZTPLDORDI-Z_VERSB = P_VERSB1.
        IT_ZTPLDORDI-Z_VERNR = P_VERNR1.
        CASE WA_ALV_CAT-COL_POS .
          WHEN 1.
            IT_ZTPLDORDI-Z_POSNR = <DYN_FIELD>.
          WHEN 2.
            IT_ZTPLDORDI-PLWRK   = <DYN_FIELD>.
          WHEN 3.
            IT_ZTPLDORDI-PWWRK   = <DYN_FIELD>.
          WHEN 4.
            IT_ZTPLDORDI-MATNR   = <DYN_FIELD>.
            CLEAR I_MATNR.
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                INPUT        = IT_ZTPLDORDI-MATNR
              IMPORTING
                OUTPUT       = I_MATNR
              EXCEPTIONS
                LENGTH_ERROR = 1
                OTHERS       = 2.
            TRANSLATE I_MATNR TO UPPER CASE.

            IT_ZTPLDORDI-MATNR = I_MATNR.
          WHEN 5.
            IT_ZTPLDORDI-MAKTX = <DYN_FIELD>.

          WHEN OTHERS.
            IT_ZTPLDORDI-GSMNG = <DYN_FIELD>.
            CLEAR IT_DATE.
            READ TABLE IT_DATE WITH KEY COL = WA_ALV_CAT-COL_POS.
            IF SY-SUBRC = 0.
              IT_ZTPLDORDI-PLDAT = IT_DATE-DATE.
              IT_ZTPLDORDI-Z_COLN = WA_ALV_CAT-COL_POS.
            ENDIF.
            APPEND IT_ZTPLDORDI.
        ENDCASE.
      ENDLOOP.
      CLEAR: WA_L_ZTPLDORDI,WA_DATA.
      MOVE-CORRESPONDING IT_ZTPLDORDI TO WA_L_ZTPLDORDI.
*******必输项目的字段检查，获取更新行项目表的数据.

      IF WA_L_ZTPLDORDI-Z_POSNR IS INITIAL.
        MESSAGE E027 WITH TEXT-004.
        EXIT.
      ENDIF.

      IF WA_L_ZTPLDORDI-PLWRK IS INITIAL.
        MESSAGE E027 WITH TEXT-005.
        EXIT.
      ELSE.

        PERFORM FRM_CHECK_AUTH USING  WA_L_ZTPLDORDI-PLWRK."检查权限.
      ENDIF.
      IF WA_L_ZTPLDORDI-PWWRK IS INITIAL.
        MESSAGE E027 WITH TEXT-006.
        EXIT.
      ENDIF.

      IF WA_L_ZTPLDORDI-MATNR IS INITIAL.
        MESSAGE E027 WITH TEXT-007.
        EXIT.
      ELSE.
        ASSIGN COMPONENT 'MATNR' OF STRUCTURE
       <DYN_WA> TO <DYN_FIELD>.
        <DYN_FIELD> = WA_L_ZTPLDORDI-MATNR.
        MODIFY <DYN_TABLE> FROM <DYN_WA>.
      ENDIF.

      READ TABLE IT_DATA INTO WA_DATA
                           WITH KEY PLWRK = WA_L_ZTPLDORDI-PLWRK
                                    PWWRK = WA_L_ZTPLDORDI-PWWRK
                                    MATNR = WA_L_ZTPLDORDI-MATNR.
      IF SY-SUBRC = 0."检查数据是否重复..
        MESSAGE E105 WITH WA_DATA-PLWRK WA_DATA-PWWRK
                          WA_DATA-MATNR.
      ELSE.
        CLEAR I_DISMM.
        SELECT SINGLE DISMM INTO I_DISMM
        FROM MARC  "检查物料主数据是否存在
        WHERE MATNR = WA_L_ZTPLDORDI-MATNR
          AND WERKS = WA_L_ZTPLDORDI-PLWRK.
        IF SY-SUBRC = 0.
          IF ( I_DISMM = 'ND' ) OR ( I_DISMM = 'VD' ).
            MESSAGE E033 WITH WA_L_ZTPLDORDI-MATNR.
            "检查工厂、物料号对应的MRP类型
          ENDIF.
        ELSE.
          MESSAGE E032 WITH WA_L_ZTPLDORDI-PLWRK WA_L_ZTPLDORDI-MATNR.
        ENDIF.
      ENDIF.
***需求工厂与供应工厂不相同的情况下检查源清单是否存在
      IF WA_L_ZTPLDORDI-PLWRK <> WA_L_ZTPLDORDI-PWWRK.
        CLEAR: LS_EORD.
        SELECT SINGLE * INTO LS_EORD FROM EORD
          WHERE MATNR = WA_L_ZTPLDORDI-MATNR
            AND WERKS = WA_L_ZTPLDORDI-PLWRK
            AND LIFNR = WA_L_ZTPLDORDI-PWWRK
            AND EKORG = 'ZF02'.
        IF SY-SUBRC <> 0.
          MESSAGE E106 WITH WA_L_ZTPLDORDI-PLWRK WA_L_ZTPLDORDI-PWWRK
                            WA_L_ZTPLDORDI-MATNR.
        ENDIF.
      ENDIF.

      CLEAR WA_DATA.
      MOVE-CORRESPONDING WA_L_ZTPLDORDI TO WA_DATA.
      APPEND WA_DATA TO IT_DATA.
      CLEAR: WA_DATA ,WA_L_ZTPLDORDI,IT_ZTPLDORDI.
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

  DATA:I_VERSB LIKE ZTPLDORDH-Z_VERSB.
  IF P_IMPORT = ABAP_TRUE.
    IF P_FILE IS INITIAL OR P_VERSB1 IS INITIAL.
      MESSAGE E028.
    ELSE.
      SELECT SINGLE Z_VERSB INTO I_VERSB FROM ZTPLDORDH
      WHERE Z_VERSB = P_VERSB1
        AND Z_VERNR = P_VERNR1.
      IF SY-SUBRC = 0.
        MESSAGE E037 WITH P_VERSB1.
      ELSEIF P_VERSB1+0(1) = 'E'.
        MESSAGE E050.
      ENDIF.
    ENDIF.
  ELSE.
    IF P_VERSB2 IS INITIAL.
      MESSAGE E038.
    ELSEIF P_VERSB2+0(1) = 'E'.
      MESSAGE E050.
    ENDIF.

  ENDIF.

ENDFORM.                    " FRM_SELECTION_INPUTCHECK
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FIELDNAME
*&---------------------------------------------------------------------*
*        设置显示字段的属性
*----------------------------------------------------------------------*

FORM FRM_GET_FIELDNAME .

  DATA:IT_L_TABLE  LIKE TABLE OF DNTAB,
       WA_L_TABLE  LIKE DNTAB.
  CLEAR WA_L_TABLE.
  CALL FUNCTION 'NAMETAB_GET'
    EXPORTING
      LANGU          = SY-LANGU
      TABNAME        = 'ZTPLDORDI'
    TABLES
      NAMETAB        = IT_L_TABLE
    EXCEPTIONS
      NO_TEXTS_FOUND = 1.
  CLEAR IT_ALV_CAT[].
*根据取出的字段目录生成参考字段目录
  LOOP AT IT_L_TABLE INTO WA_L_TABLE.

    WA_ALV_CAT-FIELDNAME = WA_L_TABLE-FIELDNAME.
    WA_ALV_CAT-INTTYPE = WA_L_TABLE-INTTYPE.
    WA_ALV_CAT-INTLEN = WA_L_TABLE-INTLEN.
    WA_ALV_CAT-FIX_COLUMN = ABAP_TRUE.
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
      WHEN '0006'.
        WA_ALV_CAT-COL_POS = '4'.
        APPEND WA_ALV_CAT TO IT_ALV_CAT.
        CLEAR WA_ALV_CAT.
      WHEN '0009'.
        WA_ALV_CAT-COL_POS = '1'.
        APPEND WA_ALV_CAT TO IT_ALV_CAT.
        CLEAR WA_ALV_CAT.
      WHEN '0010'.
        WA_ALV_CAT-COL_POS = '5'.
        APPEND WA_ALV_CAT TO IT_ALV_CAT.
        CLEAR WA_ALV_CAT.
      WHEN '0011'."数据列类型
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
*     设置按钮状态.
*----------------------------------------------------------------------*

FORM STATUS_SET USING PR_EXTAB TYPE SLIS_T_EXTAB .
  DELETE PR_EXTAB WHERE FCODE = '&DATA_SAVE'.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING PR_EXTAB[]
  OF PROGRAM SY-REPID.
ENDFORM.                    " STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*   点击保存后的处理
*----------------------------------------------------------------------*
*  -->  pr_ucomm         system comm.
*  <--  pr_selfield      select field
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
*        处理数据，并调用BAPI创建客户计划需求
*----------------------------------------------------------------------*

FORM FRM_PROCESS_DATA .
  DATA: WA_L_ZTPLDORDI LIKE IT_ZTPLDORDI.
  DATA: I_MATNR        LIKE BAPISITEMR-MATERIAL,
        I_PLWRK        LIKE BAPIPLAF_I1-PLAN_PLANT,
        I_PWWRK        LIKE BAPIPLAF_I1-PROD_PLANT,
        I_TO           TYPE I,
        FLG_ERR        TYPE C,
        I_PLANORDER    TYPE PLNUM.

  DATA: WA_L_DATA   TYPE TY_DATA.

  DATA: IT_L_PLAF   TYPE STANDARD TABLE OF PLAF,
        WA_L_PLAF   TYPE PLAF,
        WA_L_HEAD   TYPE BAPIPLAF_I1,
        WA_L_RET    TYPE BAPIRETURN1.
*******构造内表IT_ZTPLDORDH.....
  CALL FUNCTION 'ENQUEUE_EZ_ZTPLDORDH'
    EXPORTING
      MODE_ZTPLDORDH = 'E'
      MANDT          = SY-MANDT
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      OTHERS         = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'E'  NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ELSE.

    CALL FUNCTION 'ENQUEUE_EZ_ZTPLDORDI'
      EXPORTING
        MODE_ZTPLDORDI = 'E'
        MANDT          = SY-MANDT
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE 'E'  NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.

      CLEAR WA_ZTPLDORDH.

      WA_ZTPLDORDH-Z_VERSB = P_VERSB1.
      WA_ZTPLDORDH-Z_VERNR = P_VERNR1.
      WA_ZTPLDORDH-Z_DESCR = P_DESC.

      WA_ZTPLDORDH-ANDAT = SY-DATUM.
      WA_ZTPLDORDH-ANNAM = SY-UNAME.
      WA_ZTPLDORDH-ANTMS = SY-UZEIT.


***准备创建计划订单前，必须删除原来的计划订单BAPI_PLANNEDORDER_DELETE
      CLEAR: IT_L_PLAF, WA_L_PLAF.
      IF NOT IT_DATA IS INITIAL.
        SELECT PLNUM MATNR PLWRK PWWRK
          INTO CORRESPONDING FIELDS OF TABLE IT_L_PLAF
          FROM PLAF
          FOR ALL ENTRIES IN IT_DATA
          WHERE MATNR = IT_DATA-MATNR
            AND PLWRK = IT_DATA-PLWRK
            AND PWWRK = IT_DATA-PWWRK.

        LOOP AT IT_L_PLAF INTO WA_L_PLAF.
          CLEAR: WA_L_RET.
          CALL FUNCTION 'BAPI_PLANNEDORDER_DELETE'
            EXPORTING
              PLANNEDORDER = WA_L_PLAF-PLNUM
            IMPORTING
              RETURN       = WA_L_RET.

          IF WA_L_RET-TYPE = 'S' AND  WA_L_RET-ID = '61'
            AND WA_L_RET-NUMBER = '012'.
          ELSE.
            IT_ERROR-MESSAGE =  WA_L_RET-MESSAGE.
            APPEND IT_ERROR.
            FLG_ERR = ABAP_TRUE.
            EXIT.
          ENDIF.

        ENDLOOP.

      ENDIF.


***得考虑是否可以这样，因为数字为0的信息需要删除，但不需要创建
      IF FLG_ERR IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
      ENDIF.
***创建计划订单，考虑是否用同一个COMMIT

      DESCRIBE TABLE IT_ALV_CAT LINES I_TO.

      LOOP AT <DYN_TABLE> INTO <DYN_WA> .

        CLEAR:I_MATNR,I_PLWRK,I_PWWRK,IT_ERROR.
        CLEAR: WA_ALV_CAT,WA_L_DATA.
***** 取得序号...
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                       WITH KEY COL_POS = 1.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
           <DYN_WA> TO <DYN_FIELD>.
          IT_ERROR-Z_POSNR = <DYN_FIELD>.
        ENDIF.

*** 取得需求工厂
        CLEAR: WA_ALV_CAT.
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                          WITH KEY COL_POS = 2.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
           <DYN_WA> TO <DYN_FIELD>.
          I_PLWRK = <DYN_FIELD>.
          WA_L_DATA-PLWRK = I_PLWRK.
          WA_L_HEAD-PLAN_PLANT = I_PLWRK.

        ENDIF.
*** 取得供应工厂
        CLEAR WA_ALV_CAT.
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                          WITH KEY COL_POS = 3.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
           <DYN_WA> TO <DYN_FIELD>.
          I_PWWRK = <DYN_FIELD>.
          WA_L_DATA-PWWRK = I_PWWRK.
          WA_L_HEAD-PROD_PLANT = I_PWWRK.
        ENDIF.
*****取得物料值...
        CLEAR WA_ALV_CAT.
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                          WITH KEY COL_POS = 4.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
           <DYN_WA> TO <DYN_FIELD>.
          I_MATNR = <DYN_FIELD>.
          WA_L_DATA-MATNR = I_MATNR.
          WA_L_HEAD-MATERIAL = I_MATNR .
        ENDIF.

*****取得物料描述..
        CLEAR WA_ALV_CAT.
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                          WITH KEY COL_POS = 5.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
           <DYN_WA> TO <DYN_FIELD>.
          IT_ERROR-MAKTX = <DYN_FIELD>.
        ENDIF.
        MOVE-CORRESPONDING WA_L_DATA TO IT_ERROR.

***基于需求工厂以及供应工厂不同参数的不同
        IF WA_L_HEAD-PLAN_PLANT = WA_L_HEAD-PROD_PLANT.
          WA_L_HEAD-PLDORD_PROFILE = 'LA'.

        ELSE.
          WA_L_HEAD-PLDORD_PROFILE = 'UL'.
          WA_L_HEAD-FIXED_VEND     = WA_L_HEAD-PROD_PLANT.
          WA_L_HEAD-PURCH_ORG      = 'ZF02'.

        ENDIF.

        WA_L_HEAD-CONVERSION_IND = 'X'.
        WA_L_HEAD-FIRMING_IND    = 'X'.

***** 取得动态数列数据
        LOOP AT IT_ALV_CAT INTO WA_ALV_CAT FROM 6 TO I_TO.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
               <DYN_WA> TO <DYN_FIELD>.

          WA_L_HEAD-TOTAL_PLORD_QTY = <DYN_FIELD>.
          CLEAR IT_DATE.
          READ TABLE IT_DATE WITH KEY COL = WA_ALV_CAT-COL_POS.
          IF SY-SUBRC = 0.
            WA_L_HEAD-ORDER_FIN_DATE =  IT_DATE-DATE.
          ENDIF.

***如果发现数量为0时，不创建计划订单
          IF WA_L_HEAD-TOTAL_PLORD_QTY = 0.
            CONTINUE.
          ENDIF.

***进行创建计划订单
          CLEAR: FLG_ERR, WA_L_RET, I_PLANORDER.
          CALL FUNCTION 'BAPI_PLANNEDORDER_CREATE'
            EXPORTING
              HEADERDATA   = WA_L_HEAD
            IMPORTING
              RETURN       = WA_L_RET
              PLANNEDORDER = I_PLANORDER.
          IF WA_L_RET-TYPE = 'E'.
            IT_ERROR-MESSAGE =  WA_L_RET-MESSAGE.
            APPEND IT_ERROR.
            FLG_ERR = ABAP_TRUE.
          ENDIF.

        ENDLOOP.

        IF FLG_ERR IS INITIAL.
****            没有错误，更新外挂表...
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.
          PERFORM FRM_UPDATE_DBTABLE USING WA_L_DATA.
        ENDIF.

        CLEAR:WA_L_RET,WA_L_HEAD.
      ENDLOOP.
******
      MODIFY ZTPLDORDH FROM WA_ZTPLDORDH.
      IF SY-SUBRC = 0.
        MODIFY ZTPLDORDI FROM TABLE IT_ZTPLDORDI.
        IF SY-SUBRC NE 0.
          MESSAGE E035 WITH 'ZTPLDORDI'.
        ENDIF.
      ELSE.
        MESSAGE E035 WITH 'ZTPLDORDH'.
      ENDIF.

      CALL FUNCTION 'DEQUEUE_EZ_ZTPLDORDI'
        EXPORTING
          MODE_ZTPLDORDI = 'E'
          MANDT          = SY-MANDT.
    ENDIF.
    CALL FUNCTION 'DEQUEUE_EZ_ZTPLDORDH'
      EXPORTING
        MODE_ZTPLDORDH = 'E'
        MANDT          = SY-MANDT.
  ENDIF.

ENDFORM.                    " FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_UPDATE_DBTABLE
*&---------------------------------------------------------------------*
*       更新内表中的标识.
*----------------------------------------------------------------------*

FORM FRM_UPDATE_DBTABLE  USING    PR_DATA TYPE TY_DATA.

  LOOP AT IT_ZTPLDORDI WHERE PLWRK = PR_DATA-PLWRK
                        AND MATNR = PR_DATA-MATNR.
    IT_ZTPLDORDI-Z_PIRCH = 'S'.
    MODIFY IT_ZTPLDORDI.
  ENDLOOP.

ENDFORM.                    " FRM_UPDATE_DBTABLE
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       从数据库中根据屏幕输入条件查询得到报表数据
*----------------------------------------------------------------------*

FORM FRM_GET_DATA .

  DATA: I_MAXCOL   TYPE I,
        I_POS      TYPE I VALUE 5,
        I_WOTNR    TYPE P,
        I_DATE     LIKE SY-DATUM,
        I_INDEX    TYPE I,
        I_COL(4)   TYPE N VALUE 6.

  DATA: BEGIN OF IT_DATA OCCURS 0,
         Z_POSNR  LIKE ZTPLDORDI-Z_POSNR,
         Z_COLN   LIKE ZTPLDORDI-Z_COLN,
         PLWRK    LIKE ZTPLDORDI-PLWRK,
         PWWRK    LIKE ZTPLDORDI-PWWRK,
         MATNR    LIKE ZTPLDORDI-MATNR,
         PLDAT    LIKE ZTPLDORDI-PLDAT,
         MAKTX    LIKE ZTPLDORDI-MAKTX,
         GSMNG    LIKE ZTPLDORDI-GSMNG,
       END OF IT_DATA.
  DATA: BEGIN OF IT_L_MAXDATE OCCURS 0,
         ANDAT   LIKE ZTPLDORDH-ANDAT,
         ANTMS   LIKE ZTPLDORDH-ANTMS,
         PLDAT   LIKE ZTPLDORDI-PLDAT,
       END OF IT_L_MAXDATE.

  DATA: WA_L_DATA   LIKE IT_DATA,
        WA_L_LINE   TYPE REF TO DATA .

  CLEAR IT_ZTPLDORDI[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTPLDORDI
  FROM ZTPLDORDI
  WHERE Z_VERSB = P_VERSB2
    AND PLWRK IN S_WERKS
    AND MATNR IN S_MATNR.

  IF IT_ZTPLDORDI[] IS INITIAL.
    MESSAGE E036.
  ELSE.
    SORT IT_ZTPLDORDI BY Z_COLN DESCENDING.
    READ TABLE IT_ZTPLDORDI INDEX 1.
    I_MAXCOL = IT_ZTPLDORDI-Z_COLN.

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
    LOOP AT IT_ZTPLDORDI.
      MOVE-CORRESPONDING IT_ZTPLDORDI TO IT_DATA.
      APPEND IT_DATA.
      CLEAR IT_DATA.
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


    SORT IT_DATA BY Z_POSNR Z_COLN.
    LOOP AT IT_DATA .
      MOVE-CORRESPONDING IT_DATA TO WA_L_DATA.
      CLEAR: WA_ALV_CAT,I_INDEX.
      READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                WITH KEY COL_POS =  WA_L_DATA-Z_COLN.
      IF SY-SUBRC = 0.
        I_INDEX = SY-TABIX.
        ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
      <DYN_WA> TO <DYN_FIELD>.

        <DYN_FIELD> = WA_L_DATA-GSMNG.
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
          <DYN_FIELD> = WA_L_DATA-PLWRK.
        ENDIF.

        CLEAR WA_ALV_CAT.
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                  WITH KEY COL_POS =  3.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
        <DYN_WA> TO <DYN_FIELD>.
          <DYN_FIELD> = WA_L_DATA-PWWRK.
        ENDIF.

        CLEAR WA_ALV_CAT.
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                  WITH KEY COL_POS =  4.
        IF SY-SUBRC = 0.
          ASSIGN COMPONENT WA_ALV_CAT-FIELDNAME OF STRUCTURE
        <DYN_WA> TO <DYN_FIELD>.
          <DYN_FIELD> = WA_L_DATA-MATNR.
        ENDIF.

        CLEAR WA_ALV_CAT.
        READ TABLE IT_ALV_CAT INTO WA_ALV_CAT
                  WITH KEY COL_POS =  5.
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
*       屏幕输出处理
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
  WA_L_FIELD-COL_POS = 7.
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

  DATA:IT_L_VERSB LIKE ZTPLDORDH OCCURS 0 WITH HEADER LINE.

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE IT_L_VERSB
  FROM ZTPLDORDH
  WHERE Z_VERSB NOT LIKE 'E%' .
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
*&      Form  FRM_CHECK_AUTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CHECK_AUTH USING PR_WERKS.


  AUTHORITY-CHECK OBJECT 'C_PPBD' " check authorization for QP01
  ID 'WERKS' FIELD PR_WERKS
  ID 'AKTTYP' FIELD 'H'.
  IF SY-SUBRC <> 0.
    MESSAGE E063 WITH SY-UNAME PR_WERKS.
    EXIT.
  ENDIF.

ENDFORM.                    " FRM_CHECK_AUTH
