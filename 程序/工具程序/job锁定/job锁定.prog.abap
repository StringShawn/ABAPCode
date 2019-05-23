*$*$********************************************************************
* Program ID/Name:ZMMI003                 Date written:2013.8.16
* Author's name: HP_FCG                   Last update:
* Program title:SRM回传ASN接口
* Project Name:  EPR I
* Version:
* Function Spec ID:MM_01_04
*----------------------------------------------------------------------*
* Description: (Incl. Related Function Area and System)
*根据供应商的ASN在SAP中产生内向交货单并关联HU。
*SAP定期排程去文件服务器读取该部分数据，目前暂定排程的频率为每半个小时一次
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
REPORT ZMMI003 MESSAGE-ID ZMM_MSG.


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
TYPES:BEGIN OF TY_DATA ,
      VGBEL(10)    TYPE C,"采购订单
      VGPOS(6)     TYPE C,"采购订单行项目
      LIFEX(35)    TYPE C,"ASN号
      LIFEXPOS(6)  TYPE C,"行项目号
      LFDAT(10)    TYPE C, "交货日期
      MATNR(18)    TYPE C,"物料号
      LFIMG(15)    TYPE C,"交货数量
      EXIDV(20)    TYPE C,"SKU
      VEMNG(15)    TYPE C,"包装数量
      LICHN(40)    TYPE C,"供应商批次
     END OF TY_DATA.

*$*$********************************************************************
*$*$    GLOBAL CONSTANTS                                               *
*$*$********************************************************************
CONSTANTS:C_SEPARATOR TYPE C VALUE ';',
          C_ASN  TYPE ZZ_CONT VALUE 'ASN'.

*$*$********************************************************************
*$*$    GLOBAL ELEMENTARY VARIABLES                                    *
*$*$********************************************************************
DATA:G_FOLDER TYPE REF TO ZCL_SHAREFOLDER,
      G_PATH  TYPE STRING,
      G_TEXT  TYPE STRING.
****alv structure
DATA:WA_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
       WA_LAYOUT TYPE SLIS_LAYOUT_ALV.
*$*$********************************************************************
*$*$    GLOBAL INTERNAL TABLES                                         *
*$*$********************************************************************
DATA:BEGIN OF IT_DATA OCCURS 0,
      VGBEL    LIKE LIPS-VGBEL,"采购订单
      VGPOS    LIKE LIPS-VGPOS,"采购订单行项目
      LIFEX    LIKE LIKP-LIFEX, "ASN号
      LIFEXPOS LIKE LIPS-LIFEXPOS,"行项目号
      LFDAT    LIKE LIKP-LFDAT,"交货日期
      MATNR    LIKE LIPS-MATNR,"物料号
      LFIMG    LIKE LIPS-LFIMG,"交货数量
      EXIDV    LIKE VEKP-EXIDV,"SKU
      VEMNG    LIKE VEPO-VEMNG,"包装数量
      LICHN    LIKE ZTHUNUM2-ZLICHN,"LIPS-LICHN,"供应商批次
      RMATP    LIKE MARA-RMATP,
      MEINS    LIKE MARA-MEINS,
      FNAME    LIKE ZTMMASNLOG-FNAME,
     END OF IT_DATA.

DATA:BEGIN OF IT_OUT OCCURS 0,
      LIFEX    LIKE LIKP-LIFEX, "ASN号
      LIFEXPOS LIKE LIPS-LIFEXPOS,"行项目号
      LFDAT    LIKE LIKP-LFDAT,"交货日期
      VGBEL    LIKE LIPS-VGBEL,"采购订单
      VGPOS    LIKE LIPS-VGPOS,"采购订单行项目
      MATNR    LIKE LIPS-MATNR,"物料号
      LFIMG(16) TYPE P, "   LIKE LIPS-LFIMG,"交货数量
      EXIDV    LIKE VEKP-EXIDV,"SKU
      VEMNG(16) TYPE P,"    LIKE VEPO-VEMNG,"包装数量
      LICHN    LIKE ZTHUNUM2-ZLICHN,"LIPS-LICHN,"供应商批次
      VBELN    LIKE LIKP-VBELN,"SAP 内向交货单号.
      FNAME    LIKE ZTMMASNLOG-FNAME,
      TYPE     LIKE BAPIRET2-TYPE,
      MESSAGE  LIKE BAPIRET2-MESSAGE,
     END OF IT_OUT.
DATA:IT_ASNLOG TYPE STANDARD TABLE OF ZTMMASNLOG WITH HEADER LINE.
DATA:BEGIN OF IT_FILE OCCURS 0,
      NAME TYPE  EPSFILNAM ,
     END OF IT_FILE.
DATA:IT_OUT_T LIKE IT_OUT OCCURS 0 WITH HEADER LINE.
*****alv field category.
DATA:IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

DEFINE  CHECK_MANDTORY.

  IF &1 IS INITIAL.
    MOVE-CORRESPONDING WA_L_OUT TO IT_OUT.
    IT_OUT-TYPE = 'E'.
    MESSAGE E037 WITH &2 INTO IT_OUT-MESSAGE.
    APPEND IT_OUT.
    CLEAR IT_OUT.
    DELETE IT_DATA INDEX I_INDEX.
    CONTINUE.
  ENDIF.

END-OF-DEFINITION.

LOAD-OF-PROGRAM.
  PERFORM FRM_GET_PATH."获取默认路径..

*$*$********************************************************************
*$*$    PARAMETERS & SELECT-OPTIONS                                    *
*$*$********************************************************************
  SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:	P_FILE TYPE STRING  OBLIGATORY LOWER CASE,
              P_LOG AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF BLOCK B1.

*$*$********************************************************************
*$*$    INITIALIZATION                                                 *
*$*$********************************************************************

*$*$********************************************************************
*$*$    AT SELECTION-SCREEN  Output                                    *
*$*$********************************************************************

*$*$********************************************************************
*$*$    AT SELECTION-SCREEN                                            *
*$*$********************************************************************
AT SELECTION-SCREEN  .
  " CHANGE BY HANDYXH 20181022
*  IF SY-UCOMM = 'ONLI'.
*    CLEAR G_TEXT.
*    PERFORM FRM_LOCK_PROGRAM."锁住程序，独占运行.
*  ENDIF.
*$*$********************************************************************
*$*$    START-OF-SELECTION                                             *
*$*$********************************************************************

START-OF-SELECTION.
    "ADD BY HANDYXH 20181022 BEGIN
    CLEAR G_TEXT.
    PERFORM FRM_LOCK_PROGRAM."锁住程序，独占运行.
    PERFORM FRM_CHECKJOB.
    "ADD BY HANDYXH 20181022 END
  IF G_TEXT IS INITIAL.

    PERFORM FRM_GET_FILE_LIST."从共享服务器获取文件清单.

    PERFORM FRM_DISPLAY_LOG."显示处理日志.
  ENDIF.

END-OF-SELECTION.
  PERFORM FRM_UNLOCK_PROGRAM. "解锁程序..
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       从共享服务期获取ASN数据
*----------------------------------------------------------------------*

FORM FRM_GET_DATA USING PR_FILENAME "带文件路径的文件名
                        PR_FNAME.  "不带文件路径的文件名
  "ADD BY HANDYXH 20181022 BEIGN
  TYPES:BEGIN OF LY_LIKP,
       LIFEX TYPE LIKP-LIFEX,
    END OF LY_LIKP.
  DATA:LT_LIKP TYPE STANDARD TABLE OF LY_LIKP,
       LS_LIKP TYPE LY_LIKP.
  "ADD BY HANDYXH 20181022  END
  DATA:I_LICHN    TYPE STRING,"供应商批次
        I_LICHN1    TYPE XSTRING,"供应商批次
       I_CHAR1 TYPE C,
       I_CHAR2 TYPE X VALUE '0D',
       I_SPACE TYPE XSTRING .
  DATA:IT_L_DATA  TYPE ZTG01_T_DATA,
       IT_L_MSG   TYPE ZTG01_T_MSG,
       WA_L_DATA  TYPE ZTG01_DATA,
       WA_L_MSG   TYPE ZTG01_MSG,
       WA_LC_DATA TYPE TY_DATA,
       WA_L_OUT   LIKE IT_DATA.
  DATA: BEGIN OF IT_L_CHECK1 OCCURS 0,
         VGBEL    LIKE LIPS-VGBEL,"采购订单
         VGPOS    LIKE LIPS-VGPOS,"采购订单行项目
         LFDAT    LIKE LIKP-LFDAT,"交货日期
         LFIMG    LIKE LIPS-LFIMG,"交货数量
    END OF IT_L_CHECK1.
  DATA:I_FILENAME TYPE STRING,
       I_MESSAGE  TYPE STRING,
       I_SKUQTY   LIKE VEPO-VEMNG,
       I_LIFEX    LIKE LIKP-LIFEX,
       I_FROM     TYPE I,
       I_TO       TYPE I,
       I_INDEX    TYPE I,
       I_EXIDV    LIKE VEKP-EXIDV.
  I_FILENAME = PR_FILENAME.
  CREATE OBJECT G_FOLDER.
  CLEAR: IT_L_DATA[],IT_DATA[].
  CALL METHOD G_FOLDER->GET_FROM_SERVER
    EXPORTING
      IM_FILENAME = I_FILENAME
    IMPORTING
      CT_DATA     = IT_L_DATA
      CT_MSG      = IT_L_MSG.
  DATA: CL_CONV_X2C TYPE REF TO CL_ABAP_CONV_IN_CE.
  IF IT_L_MSG[] IS INITIAL.
    I_CHAR1 = CL_ABAP_CHAR_UTILITIES=>ENDIAN.

    LOOP AT IT_L_DATA INTO WA_L_DATA.
      CLEAR:WA_LC_DATA,I_MESSAGE,I_LICHN,I_LICHN1.
      SPLIT WA_L_DATA AT C_SEPARATOR
      INTO  WA_LC_DATA-LIFEX
            WA_LC_DATA-LIFEXPOS
            WA_LC_DATA-LFDAT
            WA_LC_DATA-VGBEL
            WA_LC_DATA-VGPOS
            WA_LC_DATA-MATNR
            WA_LC_DATA-LFIMG
            WA_LC_DATA-EXIDV
            WA_LC_DATA-VEMNG
            I_LICHN.
      REPLACE I_CHAR1 WITH SPACE INTO I_LICHN.
      CONDENSE I_LICHN.

      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          TEXT   = I_LICHN
        IMPORTING
          BUFFER = I_LICHN1
        EXCEPTIONS
          FAILED = 1
          OTHERS = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.

      REPLACE I_CHAR2 WITH I_SPACE INTO I_LICHN1 IN BYTE MODE.
      CL_CONV_X2C = CL_ABAP_CONV_IN_CE=>CREATE( ).
      CL_CONV_X2C->CONVERT( EXPORTING INPUT = I_LICHN1
                            IMPORTING DATA  = I_LICHN ).
      CONDENSE I_LICHN.

      WA_LC_DATA-LICHN = I_LICHN.
      CONDENSE : WA_LC_DATA-LFIMG NO-GAPS ,WA_LC_DATA-LFIMG NO-GAPS .

      MOVE-CORRESPONDING WA_LC_DATA TO IT_DATA.
      IT_DATA-FNAME = PR_FNAME.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = WA_LC_DATA-MATNR
        IMPORTING
          OUTPUT       = IT_DATA-MATNR
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.

      CONDENSE WA_LC_DATA-LFDAT.
      CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
        EXPORTING
          DATE_EXTERNAL            = WA_LC_DATA-LFDAT
        IMPORTING
          DATE_INTERNAL            = IT_DATA-LFDAT
        EXCEPTIONS
          DATE_EXTERNAL_IS_INVALID = 1
          OTHERS                   = 2.
      IF SY-SUBRC = 0.
        IF IT_DATA-LFDAT IS INITIAL.
          IT_DATA-LFDAT = SY-DATUM.
        ENDIF.
      ELSE.
        IF WA_LC_DATA-LFDAT IS INITIAL.
          IT_DATA-LFDAT = SY-DATUM.
        ELSE.
          MESSAGE E040 WITH WA_LC_DATA-LFDAT
           INTO I_MESSAGE .
        ENDIF.
      ENDIF.
***** 2014.01.07增加SKU重复检查.....changgen.
      CLEAR I_EXIDV.
      SELECT SINGLE EXIDV INTO I_EXIDV
      FROM VEKP
      WHERE EXIDV = IT_DATA-EXIDV.
      IF NOT I_EXIDV IS INITIAL.
        MESSAGE E025 WITH IT_DATA-EXIDV
                   INTO I_MESSAGE .
      ENDIF.

      IF I_MESSAGE IS INITIAL.
        APPEND IT_DATA.
        CLEAR IT_DATA.
      ELSE.
        MOVE-CORRESPONDING IT_DATA TO IT_OUT.
        IT_OUT-TYPE = 'E'.
        IT_OUT-MESSAGE = I_MESSAGE.
        APPEND IT_OUT.
        CLEAR IT_OUT.
      ENDIF.

    ENDLOOP.

  ELSE.
    CLEAR WA_L_MSG.
    READ TABLE IT_L_MSG INTO WA_L_MSG INDEX 1.
    IF SY-SUBRC = 0.
      MESSAGE E000 WITH WA_L_MSG-MSG .
    ENDIF.

  ENDIF.

****数据正确性检查.....
  SORT IT_DATA BY VGBEL VGPOS.
  CLEAR: I_SKUQTY,I_FROM,I_TO,I_INDEX.
  " ADD BY HANDYXH 20181022 BEGIN
  REFRESH LT_LIKP.
  SELECT LIFEX
  INTO CORRESPONDING FIELDS OF TABLE LT_LIKP
  FROM LIKP FOR ALL ENTRIES IN IT_DATA
  WHERE LIFEX = IT_DATA-LIFEX.
  "ADD BY HANDYXH 20181022  END
  LOOP AT IT_DATA.
    I_INDEX = SY-TABIX.
    MOVE-CORRESPONDING IT_DATA TO WA_L_OUT.
***** 检查必输字段.....
    CHECK_MANDTORY WA_L_OUT-VGBEL  TEXT-003.
    CHECK_MANDTORY WA_L_OUT-VGPOS  TEXT-004.
    CHECK_MANDTORY WA_L_OUT-LIFEX  TEXT-005.
    CHECK_MANDTORY WA_L_OUT-LIFEXPOS TEXT-006.
    CHECK_MANDTORY WA_L_OUT-MATNR  TEXT-007.
    CHECK_MANDTORY WA_L_OUT-LFIMG  TEXT-008.
    CHECK_MANDTORY WA_L_OUT-EXIDV  TEXT-009.
    CHECK_MANDTORY WA_L_OUT-VEMNG  TEXT-010.
*****检查ASN号是否已经存在,


    CLEAR I_LIFEX.
    "CHANGE BY HANDYXH 20181022 BEGIN
*    SELECT SINGLE LIFEX INTO I_LIFEX
*    FROM LIKP
*    WHERE LIFEX = WA_L_OUT-LIFEX.
    READ TABLE LT_LIKP INTO LS_LIKP WITH KEY LIFEX = WA_L_OUT-LIFEX.
    IF SY-SUBRC = 0.
      I_LIFEX = LS_LIKP-LIFEX.
    ENDIF.
    "CHANGE BY HANDYXH 20181022 END
    IF NOT I_LIFEX IS INITIAL.
      MOVE-CORRESPONDING WA_L_OUT TO IT_OUT.
      IT_OUT-TYPE = 'E'.
      MESSAGE E032 WITH WA_L_OUT-LIFEX INTO IT_OUT-MESSAGE.
      APPEND IT_OUT.
      CLEAR IT_OUT.
      DELETE IT_DATA INDEX I_INDEX.
      CONTINUE.
    ENDIF.

    "加总同一个采购订单，行项目的看板卡的数量...
    I_SKUQTY = I_SKUQTY + WA_L_OUT-VEMNG.

    AT NEW VGPOS.
      I_FROM = I_INDEX.
      MOVE-CORRESPONDING WA_L_OUT TO IT_L_CHECK1.
      APPEND IT_L_CHECK1.
      CLEAR IT_L_CHECK1.

    ENDAT.


******* 检查供应商，数量是否一致....

    CLEAR IT_L_CHECK1.
    READ TABLE IT_L_CHECK1 WITH KEY VGBEL = WA_L_OUT-VGBEL
                                    VGPOS = WA_L_OUT-VGPOS.
    IF SY-SUBRC = 0.

      "检查订单数量
      IF WA_L_OUT-LFIMG NE IT_L_CHECK1-LFIMG.


        LOOP AT IT_DATA WHERE VGBEL = WA_L_OUT-VGBEL
                           AND VGPOS = WA_L_OUT-VGPOS.
          MOVE-CORRESPONDING WA_L_OUT TO IT_OUT.
          IT_OUT-TYPE = 'E'.
          MESSAGE E029 WITH WA_L_OUT-VGBEL  WA_L_OUT-VGPOS
                            WA_L_OUT-LFIMG INTO IT_OUT-MESSAGE.
          APPEND IT_OUT.
          CLEAR IT_OUT.
        ENDLOOP.

        DELETE IT_DATA WHERE VGBEL = WA_L_OUT-VGBEL
                           AND VGPOS = WA_L_OUT-VGPOS.
        CONTINUE.
      ENDIF.
      IF WA_L_OUT-LFDAT NE IT_L_CHECK1-LFDAT.

        LOOP AT IT_DATA WHERE VGBEL = WA_L_OUT-VGBEL
                          AND VGPOS = WA_L_OUT-VGPOS.
          MOVE-CORRESPONDING IT_DATA TO IT_OUT.
          IT_OUT-TYPE = 'E'.
          MESSAGE E033 WITH WA_L_OUT-VGBEL  WA_L_OUT-LFDAT
                               INTO IT_OUT-MESSAGE.
          APPEND IT_OUT.
          CLEAR IT_OUT.
        ENDLOOP.

        DELETE IT_DATA WHERE VGBEL = WA_L_OUT-VGBEL
                           AND VGPOS = WA_L_OUT-VGPOS.

        CONTINUE.
      ENDIF.

    ENDIF.

    AT END OF VGPOS.
      I_TO = I_INDEX.
      CLEAR IT_L_CHECK1.
      READ TABLE IT_L_CHECK1 WITH KEY VGBEL = WA_L_OUT-VGBEL
                                      VGPOS = WA_L_OUT-VGPOS.
      IF SY-SUBRC = 0. "检查看板卡数量.
        IF IT_L_CHECK1-LFIMG NE I_SKUQTY.
          LOOP AT IT_DATA FROM I_FROM TO I_TO.
            MOVE-CORRESPONDING IT_DATA TO IT_OUT.
            IT_OUT-TYPE = 'E'.
            MESSAGE E034 WITH IT_DATA-VGBEL IT_DATA-VGPOS
                              IT_L_CHECK1-LFIMG I_SKUQTY
                         INTO IT_OUT-MESSAGE.
            APPEND IT_OUT.
            CLEAR IT_OUT.

          ENDLOOP.
          DELETE IT_DATA FROM I_FROM TO I_TO .
          CLEAR: I_FROM,I_TO,I_SKUQTY.
          CONTINUE.
        ELSE.
          CLEAR I_SKUQTY.
        ENDIF.
      ENDIF.
    ENDAT.

    SELECT SINGLE RMATP MEINS INTO (WA_L_OUT-RMATP,WA_L_OUT-MEINS)
    FROM MARA
    WHERE MATNR = WA_L_OUT-MATNR.
    MODIFY IT_DATA FROM WA_L_OUT.
    CLEAR: WA_L_OUT.
  ENDLOOP.


ENDFORM.                    " FRM_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_DN
*&---------------------------------------------------------------------*
*       根据数据创建内向交货单
*----------------------------------------------------------------------*

FORM FRM_CREATE_DN USING PR_FILENAME.

  DATA:WA_L_DATA    LIKE IT_DATA,
       WA_L_FDATA   TYPE ZTG01_DATA,
       WA_L_MSG     TYPE ZTG01_MSG,
       WA_L_DNITEM  TYPE ZSMMIBASN,
       WA_L_DNRET   TYPE ZSMMIBRET,
       IT_L_DNITEM  TYPE STANDARD TABLE OF ZSMMIBASN,
       IT_L_DNRET   TYPE STANDARD TABLE OF ZSMMIBRET,
       IT_L_DNMSG   TYPE STANDARD TABLE OF BDIDOCSTAT WITH HEADER LINE,
       IT_L_HUNUM   TYPE STANDARD TABLE OF ZTHUNUM WITH HEADER LINE,
       IT_L_SUCC    TYPE ZTG01_T_DATA,
       IT_L_FAIL    TYPE ZTG01_T_DATA,
       IT_L_MSG     TYPE ZTG01_T_MSG,
       IT_L_OUT     LIKE IT_OUT OCCURS 0 WITH HEADER LINE.
  DATA:FLG_MESSAGE  TYPE C,
       I_MESSAGE    TYPE STRING,
       I_LFIMG(15)  TYPE C,
       I_VEMNG(15)  TYPE C,
       I_FILENAME   TYPE STRING.
  DATA:BEGIN OF IT_L_HU OCCURS 0,
        ZEXIDV LIKE ZTHUNUM-ZEXIDV,
        ZLICHN LIKE ZTHUNUM-ZLICHN,
       END OF IT_L_HU.


  SORT IT_DATA BY VGBEL VGPOS.
  CLEAR IT_L_OUT[].
  LOOP AT IT_DATA.
    MOVE-CORRESPONDING IT_DATA TO WA_L_DATA.
    MOVE-CORRESPONDING IT_DATA TO IT_OUT.
    APPEND IT_OUT.
    CLEAR IT_OUT.
*****Delivery and HU data...
    WA_L_DNITEM-ZLIFEX = WA_L_DATA-LIFEX.
    WA_L_DNITEM-ZLIFEXPOS = WA_L_DATA-LIFEXPOS.
    WA_L_DNITEM-ZNTANF = WA_L_DATA-LFDAT.
    WA_L_DNITEM-ZLFIMG = WA_L_DATA-LFIMG.
    WA_L_DNITEM-ZVBELN = WA_L_DATA-VGBEL.
    WA_L_DNITEM-ZPOSNR = WA_L_DATA-VGPOS.
    WA_L_DNITEM-ZKDMAT = WA_L_DATA-MATNR.
    WA_L_DNITEM-ZVRKME = WA_L_DATA-MEINS.
    WA_L_DNITEM-ZLICHN = WA_L_DATA-LICHN.
    WA_L_DNITEM-ZEXIDV = WA_L_DATA-EXIDV.
    WA_L_DNITEM-ZVEMNG = WA_L_DATA-VEMNG.
    WA_L_DNITEM-ZVEMEH = WA_L_DATA-MEINS.
    WA_L_DNITEM-ZRMATP = WA_L_DATA-RMATP.
    APPEND WA_L_DNITEM TO IT_L_DNITEM.
    CLEAR WA_L_DNITEM.
    AT END OF VGBEL.
      CLEAR: FLG_MESSAGE,I_MESSAGE.
      CALL FUNCTION 'ZMM_CREATE_INBOUD_DELIVERY'
        TABLES
          IT_ITEM       = IT_L_DNITEM
          IT_RETURN     = IT_L_DNRET
          IT_STATUS     = IT_L_DNMSG
        EXCEPTIONS
          ERROR_MESSAGE = 1
          OTHERS        = 2.
*****错误处理
      IF SY-SUBRC <> 0.
        FLG_MESSAGE = ABAP_TRUE.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO I_MESSAGE .
      ELSE.
        LOOP AT IT_L_DNMSG WHERE MSGTY = 'A' OR MSGTY = 'E'.
          FLG_MESSAGE = ABAP_TRUE.
          MESSAGE ID     IT_L_DNMSG-MSGID
            TYPE   IT_L_DNMSG-MSGTY
            NUMBER IT_L_DNMSG-MSGNO
            WITH   IT_L_DNMSG-MSGV1
                   IT_L_DNMSG-MSGV2
                   IT_L_DNMSG-MSGV3
                   IT_L_DNMSG-MSGV4
            INTO I_MESSAGE..
          EXIT.
        ENDLOOP.
      ENDIF.
*****更新内向交货单和错误消息
      IF FLG_MESSAGE IS INITIAL.
        CLEAR WA_L_DNRET.
        READ TABLE IT_L_DNRET INTO WA_L_DNRET
                              WITH KEY ZVBELN = WA_L_DATA-VGBEL.
        IF SY-SUBRC = 0.
          LOOP AT IT_OUT WHERE VGBEL = WA_L_DATA-VGBEL
                           AND TYPE IS INITIAL.
            IT_OUT-VBELN = WA_L_DNRET-ZVBELN_VL.
            IT_OUT-TYPE = 'S'.
            MESSAGE E036 WITH WA_L_DNRET-ZVBELN_VL
                         INTO I_MESSAGE.
            IT_OUT-MESSAGE = I_MESSAGE.

            MOVE-CORRESPONDING IT_OUT TO IT_L_OUT.
            APPEND IT_L_OUT.
            CLEAR IT_L_OUT.
            IT_L_HU-ZEXIDV = IT_OUT-EXIDV.
            IT_L_HU-ZLICHN = IT_OUT-LICHN.
            APPEND IT_L_HU.
            CLEAR IT_L_HU.
            MODIFY IT_OUT.
            CLEAR IT_OUT.
          ENDLOOP.
        ENDIF.
      ELSE.
        LOOP AT IT_OUT WHERE VGBEL = WA_L_DATA-VGBEL.
          IF  IT_OUT-TYPE IS INITIAL.
            IT_OUT-TYPE = 'E'.
            IT_OUT-MESSAGE = I_MESSAGE.
          ENDIF.
          MOVE-CORRESPONDING IT_OUT TO IT_L_OUT.
          APPEND IT_L_OUT.
          CLEAR IT_L_OUT.
          MODIFY IT_OUT.
          CLEAR IT_OUT.
        ENDLOOP.
      ENDIF.

      CLEAR:IT_L_DNITEM[],IT_L_DNRET[],IT_L_DNMSG[].
    ENDAT.
  ENDLOOP.
********更新供应商批次号码..
  IF NOT IT_L_HU[] IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_L_HUNUM
    FROM ZTHUNUM
    FOR ALL ENTRIES IN IT_L_HU
    WHERE ZEXIDV = IT_L_HU-ZEXIDV.
    SORT IT_L_HU BY ZEXIDV.
    LOOP AT IT_L_HUNUM.
      CLEAR IT_L_HU.
      READ TABLE IT_L_HU WITH KEY ZEXIDV = IT_L_HUNUM-ZEXIDV
                         BINARY SEARCH.
      IF SY-SUBRC = 0.
        IT_L_HUNUM-ZLICHN = IT_L_HU-ZLICHN.
        MODIFY IT_L_HUNUM.
      ENDIF.
    ENDLOOP.

    MODIFY ZTHUNUM FROM TABLE IT_L_HUNUM.
  ENDIF.
  IF IT_L_OUT[] IS INITIAL.
    LOOP AT IT_OUT.
      CLEAR :I_LFIMG,I_VEMNG,WA_L_FDATA.
      WRITE: IT_OUT-LFIMG TO I_LFIMG NO-GAP NO-GROUPING,
             IT_OUT-VEMNG TO I_VEMNG NO-GAP NO-GROUPING.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          INPUT  = IT_OUT-MATNR
        IMPORTING
          OUTPUT = IT_OUT-MATNR. "去掉物料号码前导零

      CONCATENATE IT_OUT-LIFEX  "ASN号
                  IT_OUT-LIFEXPOS "行项目号
                  IT_OUT-LFDAT"交货日期
                  IT_OUT-VGBEL"采购订单
                  IT_OUT-VGPOS"采购订单行项目
                  IT_OUT-MATNR"物料号
                  I_LFIMG"交货数量
                  IT_OUT-EXIDV"SKU
                  I_VEMNG "包装数量
                  IT_OUT-LICHN"供应商批次
                  INTO WA_L_FDATA-LINE SEPARATED BY C_SEPARATOR.

      IF IT_OUT-TYPE = 'S'.
        APPEND WA_L_FDATA TO IT_L_SUCC.
        CLEAR WA_L_FDATA.
      ELSEIF IT_OUT-TYPE = 'E'.
        APPEND WA_L_FDATA TO IT_L_FAIL.
        CLEAR WA_L_FDATA.
      ENDIF.
      APPEND IT_OUT TO IT_OUT_T.
      CLEAR IT_OUT_T.
    ENDLOOP.
  ELSE.
    LOOP AT IT_L_OUT.
      CLEAR :I_LFIMG,I_VEMNG,WA_L_FDATA.
      WRITE: IT_L_OUT-LFIMG TO I_LFIMG NO-GAP NO-GROUPING,
             IT_L_OUT-VEMNG TO I_VEMNG NO-GAP NO-GROUPING.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          INPUT  = IT_OUT-MATNR
        IMPORTING
          OUTPUT = IT_OUT-MATNR. "去掉物料号码前导零

      CONCATENATE IT_L_OUT-LIFEX  "ASN号
                  IT_L_OUT-LIFEXPOS "行项目号
                  IT_L_OUT-LFDAT"交货日期
                  IT_L_OUT-VGBEL"采购订单
                  IT_L_OUT-VGPOS"采购订单行项目
                  IT_L_OUT-MATNR"物料号
                  I_LFIMG"交货数量
                  IT_L_OUT-EXIDV"SKU
                  I_VEMNG "包装数量
                  IT_L_OUT-LICHN"供应商批次
                  INTO WA_L_FDATA-LINE SEPARATED BY C_SEPARATOR.

      IF IT_L_OUT-TYPE = 'S'.
        APPEND WA_L_FDATA TO IT_L_SUCC.
        CLEAR WA_L_FDATA.
      ELSEIF IT_L_OUT-TYPE = 'E'.
        APPEND WA_L_FDATA TO IT_L_FAIL.
        CLEAR WA_L_FDATA.
      ENDIF.
      APPEND IT_L_OUT TO IT_OUT_T.
      CLEAR IT_OUT_T.
    ENDLOOP.
  ENDIF.

  IF NOT IT_L_SUCC IS INITIAL.
    CONCATENATE P_FILE  'successful/' PR_FILENAME INTO I_FILENAME .
    CREATE OBJECT G_FOLDER.

    CALL METHOD G_FOLDER->SEND_TO_SERVER
      EXPORTING
        IM_FILENAME = I_FILENAME
        CT_DATA     = IT_L_SUCC
      IMPORTING
        CT_MSG      = IT_L_MSG.

    IF NOT IT_L_MSG IS INITIAL.
      CLEAR WA_L_MSG.
      READ TABLE IT_L_MSG INTO WA_L_MSG INDEX 1.
      IF SY-SUBRC = 0.
        IT_L_OUT-TYPE = 'E'.
        IT_L_OUT-MESSAGE = WA_L_MSG-MSG.
        MODIFY IT_L_OUT TRANSPORTING TYPE MESSAGE
                        WHERE TYPE = 'S'.
      ENDIF.
      LOOP AT IT_OUT.
        CLEAR IT_L_OUT.
        READ TABLE IT_L_OUT WITH KEY LIFEX    = IT_OUT-LIFEX
                                   LIFEXPOS = IT_OUT-LIFEXPOS
                                   VGBEL    = IT_OUT-VGBEL
                                   VGPOS    = IT_OUT-VGPOS.
        IF SY-SUBRC = 0.
          IT_OUT-TYPE = IT_L_OUT-TYPE.
          IT_OUT-MESSAGE = IT_L_OUT-MESSAGE.
          MODIFY IT_OUT.
        ENDIF.

      ENDLOOP.
    ENDIF.
  ENDIF.

  IF NOT IT_L_FAIL IS INITIAL.
    CONCATENATE P_FILE  'failed/' PR_FILENAME INTO I_FILENAME .
    CREATE OBJECT G_FOLDER.

    CALL METHOD G_FOLDER->SEND_TO_SERVER
      EXPORTING
        IM_FILENAME = I_FILENAME
        CT_DATA     = IT_L_FAIL
      IMPORTING
        CT_MSG      = IT_L_MSG.

    IF NOT IT_L_MSG IS INITIAL.
      CLEAR WA_L_MSG.
      READ TABLE IT_L_MSG INTO WA_L_MSG INDEX 1.
      IF SY-SUBRC = 0.
        IT_L_OUT-TYPE = 'E'.
        IT_L_OUT-MESSAGE = WA_L_MSG-MSG.
        MODIFY IT_L_OUT TRANSPORTING TYPE MESSAGE
                        WHERE TYPE = 'E'.
      ENDIF.
      LOOP AT IT_OUT.
        CLEAR IT_L_OUT.
        READ TABLE IT_L_OUT WITH KEY LIFEX    = IT_OUT-LIFEX
                                   LIFEXPOS = IT_OUT-LIFEXPOS
                                   VGBEL    = IT_OUT-VGBEL
                                   VGPOS    = IT_OUT-VGPOS.
        IF SY-SUBRC = 0.
          IT_OUT-TYPE = IT_L_OUT-TYPE.
          IT_OUT-MESSAGE = IT_L_OUT-MESSAGE.
          MODIFY IT_OUT.
        ENDIF.

      ENDLOOP.
    ENDIF.


  ENDIF.


ENDFORM.                    " FRM_CREATE_DN
*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_LOG
*&---------------------------------------------------------------------*
*       显示处理日志.
*----------------------------------------------------------------------*

FORM FRM_DISPLAY_LOG .
  DATA:I_REPID LIKE SY-REPID.
  I_REPID = SY-REPID.
**  ****** Build the fieldcat for ALV display.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = I_REPID
      I_INTERNAL_TABNAME     = 'IT_OUT'
*     I_STRUCTURE_NAME       =
*     I_CLIENT_NEVER_DISPLAY = 'X'
      I_INCLNAME             = I_REPID
*     I_BYPASSING_BUFFER     =
*     I_BUFFER_ACTIVE        =
    CHANGING
      CT_FIELDCAT            = IT_FIELDCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*
  READ TABLE IT_FIELDCAT INTO WA_FIELDCAT INDEX 1.
  WA_FIELDCAT-SELTEXT_L = TEXT-003.
  WA_FIELDCAT-DDICTXT = 'L'.
  MODIFY IT_FIELDCAT FROM WA_FIELDCAT INDEX 1.


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
      T_OUTTAB                          = IT_OUT_T
   EXCEPTIONS
     PROGRAM_ERROR                     = 1
     OTHERS                            = 2 .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " FRM_DISPLAY_LOG
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FILE_LIST
*&---------------------------------------------------------------------*
*    从共享服务器获取文件清单.
*----------------------------------------------------------------------*

FORM FRM_GET_FILE_LIST .
  DATA:I_DIRNAME LIKE EPSF-EPSDIRNAM,
       I_NAME    TYPE STRING,
       I_EXTNAM  TYPE STRING,
       I_FILENAM TYPE STRING,
       I_DATE    LIKE SY-DATUM,
       I_TIME    LIKE SY-UZEIT,
       I_UNAME    LIKE SY-UNAME.
  DATA:IT_L_DIR  TYPE STANDARD TABLE OF EPSFILI,
       WA_L_DIR  TYPE EPSFILI .


  I_DIRNAME = P_FILE.
  CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
    EXPORTING
      DIR_NAME               = I_DIRNAME
    TABLES
      DIR_LIST               = IT_L_DIR
    EXCEPTIONS
      INVALID_EPS_SUBDIR     = 1
      SAPGPARAM_FAILED       = 2
      BUILD_DIRECTORY_FAILED = 3
      NO_AUTHORIZATION       = 4
      READ_DIRECTORY_FAILED  = 5
      TOO_MANY_READ_ERRORS   = 6
      EMPTY_DIRECTORY_LIST   = 7
      OTHERS                 = 8.
  IF SY-SUBRC <> 0.
    MESSAGE I118 WITH I_DIRNAME.
    STOP.
  ELSE.
    LOOP AT IT_L_DIR INTO WA_L_DIR.
      CLEAR :I_NAME ,I_EXTNAM.
      IT_FILE-NAME = WA_L_DIR-NAME.

      SPLIT  IT_FILE-NAME AT '.' INTO I_NAME I_EXTNAM.
      IF I_EXTNAM = 'csv' OR I_EXTNAM = 'CSV'.
        APPEND IT_FILE.
        CLEAR IT_FILE.
      ENDIF.
    ENDLOOP.
  ENDIF.
  CLEAR IT_OUT_T[].
  LOOP AT IT_FILE .
    CLEAR: I_FILENAM,IT_OUT[],IT_OUT.
    CONCATENATE P_FILE  IT_FILE-NAME INTO I_FILENAM.
    PERFORM FRM_GET_DATA USING I_FILENAM
                               IT_FILE-NAME ."从共享服务期获取ASN数据

    PERFORM FRM_CREATE_DN USING IT_FILE-NAME."根据数据创建内向交货单
    PERFORM FRM_DELETE_FILE USING I_FILENAM. "删除原始文件.
  ENDLOOP.


  I_DATE = SY-DATUM.
  I_TIME = SY-UZEIT.
  I_UNAME = SY-UNAME.

  LOOP AT IT_OUT_T.
    MOVE-CORRESPONDING IT_OUT_T TO IT_ASNLOG.
    CONCATENATE I_DATE I_TIME INTO IT_ASNLOG-TMSTP.
    CONDENSE  IT_ASNLOG-TMSTP.
    IT_ASNLOG-ERDAT = I_DATE.
    IT_ASNLOG-ERTIM = I_TIME.
    IT_ASNLOG-ERNAM = I_UNAME.
    APPEND IT_ASNLOG.
    CLEAR IT_ASNLOG.
  ENDLOOP.
  IF P_LOG = ABAP_TRUE.
    MODIFY ZTMMASNLOG FROM TABLE IT_ASNLOG.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " FRM_GET_FILE_LIST
*&---------------------------------------------------------------------*
*&      Form  FRM_DELETE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_FILENAM  text
*----------------------------------------------------------------------*
FORM FRM_DELETE_FILE  USING    PR_FILENAM.
  DATA:I_FILENAME TYPE STRING.
  DATA: WA_L_MSG TYPE ZTG01_MSG,
        IT_L_MSG TYPE ZTG01_T_MSG.
  I_FILENAME = PR_FILENAM.
  CREATE OBJECT G_FOLDER.

  CALL METHOD G_FOLDER->DELETE_FROM_SERVER
    EXPORTING
      IM_FILENAME = I_FILENAME
    IMPORTING
      CT_MSG      = IT_L_MSG.

ENDFORM.                    " FRM_DELETE_FILE
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_PATH .
  DATA: I_TTYPE   TYPE ZZ_TYPE,
        WA_L_ASN  TYPE ZTG01_S_CON.
  DATA:IT_L_MSG  TYPE ZTG01_T_MSG..
  CREATE OBJECT G_FOLDER.
  IF SY-BATCH = ABAP_TRUE.
    I_TTYPE = 'B'.
  ELSE.
    I_TTYPE = 'A'.
  ENDIF.
  CALL METHOD G_FOLDER->GET_CONNECT
    EXPORTING
      IM_TCONT     = C_ASN
      IM_TTYPE     = I_TTYPE
    IMPORTING
      LS_CONECTION = WA_L_ASN
      CT_MSG       = IT_L_MSG.

  P_FILE = WA_L_ASN-ZTPATH.

ENDFORM.                    " FRM_GET_PATH

*&---------------------------------------------------------------------*
*&      Form  FRM_LOCK_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_LOCK_PROGRAM .
*
*CALL FUNCTION 'ENQUEUE_ES_PROG'
* EXPORTING
*   MODE_TRDIR           = 'X'
*   NAME                 = SY-REPID
**   X_NAME               = ' '
**   _SCOPE               = '2'
**   _WAIT                = ' '
**   _COLLECT             = ' '
* EXCEPTIONS
*   FOREIGN_LOCK         = 1
*   SYSTEM_FAILURE       = 2
*   OTHERS               = 3
*          .
**IF SY-SUBRC <> 0.
*** Implement suitable error handling here
**ENDIF.
**
  CALL FUNCTION 'ENQUEUE_ESRDIRX'
    EXPORTING
      NAME         = SY-REPID
    EXCEPTIONS
      FOREIGN_LOCK = 1
      OTHERS       = 2.
  IF SY-SUBRC NE 0.
    " ADD BY HANDYXH 20181022 BEGIN
        WAIT UP TO '0.2' SECONDS.
*  报错前、先释放锁
    CALL FUNCTION 'DEQUEUE_ES_PROG'
      EXPORTING
        name = sy-repid.
    " ADD BY HANDYXH 20181022 END
    IF SY-BATCH IS INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

    ELSE.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO G_TEXT.
      WRITE G_TEXT.
    ENDIF.
  ENDIF.
ENDFORM.                    " FRM_LOCK_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  FRM_UNLOCK_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_UNLOCK_PROGRAM .
   WAIT UP TO '0.2' SECONDS. " ADD BY HANDYXH 20181022
  CALL FUNCTION 'DEQUEUE_ES_PROG'
    EXPORTING
      NAME = SY-REPID.

ENDFORM.                    " FRM_UNLOCK_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECKJOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CHECKJOB .
 DATA: LT_JOBLIST    TYPE STANDARD TABLE OF TBTCJOB,
        LS_JOBLIST    TYPE TBTCJOB.

  DATA: L_JOBNAME   TYPE BTCJOB,
        L_JOBCOUNT  TYPE BTCJOBCNT,
        L_RUNNING   TYPE BTCSTATUS.

  PERFORM FRM_UNLOCK_PROGRAM.

  CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
    EXPORTING
      ABAP_PROGRAM_NAME             = SY-REPID
*     ABAP_VARIANT_NAME             = ' '
*     EXTERNAL_PROGRAM_NAME         = ' '
*     DIALOG                        = ' '
      STATUS                        = '*'
    TABLES
      JOBLIST                       = LT_JOBLIST
    EXCEPTIONS
      NO_JOBS_FOUND                 = 1
      PROGRAM_SPECIFICATION_MISSING = 2
      INVALID_DIALOG_TYPE           = 3
      JOB_FIND_CANCELED             = 4
      OTHERS                        = 5.

*  IF SY-SUBRC = 0.
**  报错前、先释放锁
*    PERFORM FRM_UNLOCK_PROGRAM.
*    MESSAGE E000(ZMM_MSG) WITH '已经存在此程序的后台任务'.
*  ENDIF.

  CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
    IMPORTING
*     EVENTID                 =
*     EVENTPARM               =
*     EXTERNAL_PROGRAM_ACTIVE =
      JOBCOUNT                = L_JOBCOUNT
      JOBNAME                 = L_JOBNAME
*     STEPCOUNT               =
    EXCEPTIONS
      NO_RUNTIME_INFO         = 1
      OTHERS                  = 2.
  IF SY-SUBRC = 0.
    DELETE LT_JOBLIST WHERE JOBNAME = L_JOBNAME
                        AND JOBCOUNT = L_JOBCOUNT.
  ENDIF.

  LOOP AT LT_JOBLIST INTO LS_JOBLIST.
    CLEAR: L_RUNNING.
    CALL FUNCTION 'SHOW_JOBSTATE'
      EXPORTING
        JOBCOUNT         = LS_JOBLIST-JOBCOUNT
        JOBNAME          = LS_JOBLIST-JOBNAME
      IMPORTING
*       ABORTED          =
*       FINISHED         =
*       PRELIMINARY      =
*       READY            =
        RUNNING          = L_RUNNING
*       SCHEDULED        =
*       SUSPENDED        =
*       OTHER            =
      EXCEPTIONS
        JOBCOUNT_MISSING = 1
        JOBNAME_MISSING  = 2
        JOB_NOTEX        = 3
        OTHERS           = 4.
    IF L_RUNNING = 'X'.
*  报错前、先释放锁
      PERFORM FRM_UNLOCK_PROGRAM.
      IF SY-BATCH = 'X'.
        CONCATENATE G_TEXT '已经存在此程序的后台任务' INTO G_TEXT SEPARATED BY ','.
      ELSE.
        MESSAGE E000(ZMM_MSG) WITH '已经存在此程序的后台任务'.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " FRM_CHECKJOB