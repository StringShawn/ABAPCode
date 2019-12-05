*&---------------------------------------------------------------------*
*& Program ID    :  ZFIR00009
*& Program Text  : 货币码和汇率主数据接口
*& Overview      : 货币码和汇率主数据接口
*& Created by    : HANDLM
*& Creation Date : 2019/10/15
*&---------------------------------------------------------------------*
*& Changing No   :
*& Updated by    :
*& Update date   :
*& Changed Item Description :
*& 请求号        :
*&---------------------------------------------------------------------*
REPORT ZFIR00009.
"传输数据所用内表
TYPES:BEGIN OF TY_DATA,
        RCOMP TYPE T001-RCOMP,
        CURR  TYPE T880-CURR,
        FCURR TYPE TCURR-FCURR,
        TCURR TYPE TCURR-TCURR,
        GDATU TYPE TCURR-GDATU,
        KURST TYPE TCURR-KURST,
        UKURS TYPE TCURR-UKURS,
        ZBWB  TYPE ZTTCURR-ZBWB,
        DATUM TYPE D,
        BUKRS TYPE T001-BUKRS,
      END OF TY_DATA.
DATA:GT_DATA TYPE TABLE OF TY_DATA WITH HEADER LINE.
DATA:GT_DATAX TYPE TABLE OF TY_DATA WITH HEADER LINE.
DATA:GT_ZTTCURR TYPE TABLE OF ZTTCURR WITH HEADER LINE.
TYPES:BEGIN OF TY_CURR,
        FCURR TYPE ZTTCURR-FCURR,
        BUKRS TYPE ZTTCURR-BUKRS,
        GDATU TYPE ZTTCURR-GDATU,
        UKURS TYPE ZTTCURR-UKURS,
        ZBWB  TYPE ZTTCURR-ZBWB,
      END OF TY_CURR.

DATA:GT_ZTTCURRX TYPE TABLE OF TY_CURR WITH HEADER LINE.
"XML相关定义
*常量-重复节点名称
CONSTANTS : CON_REPEAT TYPE STRING VALUE 'Currency'.
*  xml 数据
DATA: LR_IXML          TYPE REF TO IF_IXML,
      LR_DOCUMENT      TYPE REF TO IF_IXML_DOCUMENT,
      LV_GID           TYPE I, "定位节点
      LV_XML_AS_STRING TYPE XSTRING,
      LV_XML_SIZE      TYPE I,
      LT_XML_AS_TABLE  TYPE DCXMLLINES WITH HEADER LINE,
      LV_CSTRING       TYPE STRING.
"FTP相关定义
CONSTANTS: C_KEY TYPE I VALUE 26101957."密钥
CONSTANTS: C_PWD(20) VALUE '9ol.0p;/'."密码
CONSTANTS: C_USER(15) VALUE 'sap'."用户名
CONSTANTS: C_HOST(30) VALUE '10.0.23.79'."地址
**SAPFTPA：表示以SAP服务器为目的地，上传与下载都会放在SAP服务器上；SAPFTP：以Client端为目的地
CONSTANTS: C_RFCDEST LIKE RFCDES-RFCDEST VALUE 'SAPFTPA'.

DATA:LV_PWD(40).
DATA:LV_COMMAND(99),
     LV_COMMAND1(99),
     LV_LEN          TYPE I,
     LV_HDL          TYPE I.
DATA: BEGIN OF LT_RESULT OCCURS 0,
        LINE(100) TYPE C,
      END OF LT_RESULT.

DATA:LV_FILENAME TYPE CHAR128.
DATA:LV_DIR TYPE CHAR128 .
DATA:LV_NAME_ALL TYPE CHAR128.
DATA:LV_FILENAMEX TYPE CHAR128.
DATA OREF TYPE REF TO CX_ROOT.
DATA: BEGIN OF LT_DATA_TXT OCCURS 0 ,
        LINE(500),
      END OF LT_DATA_TXT.

DATA: BEGIN OF LT_DATA_BINARY OCCURS 0,
        x(2000) TYPE X,
      END OF LT_DATA_BINARY.
DATA: LV_BINARY_LEN TYPE I.
DATA:  L_CODEPAGE(4) TYPE N .
DATA:  L_ENCODING(20).
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
PARAMETERS P_BOX TYPE C DEFAULT 'X'.
SELECTION-SCREEN END  OF BLOCK BLK1.

START-OF-SELECTION.
  PERFORM FRM_SELECT_DATA."取数
  IF GT_DATA[] IS NOT INITIAL.
    PERFORM FRM_CREATXML."生成XML
    PERFORM FRM_UPLOADFTP."上传到FTP
    PERFORM FRM_UPDATEDATA."数据更新
  ENDIF.
*&---------------------------------------------------------------------*
*& Form FRM_SELECT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_SELECT_DATA .
  "获取ZTTCURR 全部数据
  SELECT *
        INTO CORRESPONDING FIELDS OF TABLE GT_ZTTCURR
  FROM ZTTCURR.
  SELECT T001~RCOMP
         T880~CURR
         TCURR~GDATU
         TCURR~KURST
         TCURR~FCURR
         TCURR~TCURR
         TCURR~UKURS
         T001~BUKRS
         INTO CORRESPONDING FIELDS OF TABLE GT_DATA
         FROM T001 INNER JOIN T880
         ON T001~RCOMP = T880~RCOMP
         INNER JOIN TCURR
         ON TCURR~TCURR = T880~CURR
         WHERE T001~BUKRS IN ( 'GW01' , 'GW05' )
         AND  KURST = 'M' .
  DATA:GV1(10) TYPE C.
  LOOP AT GT_DATA.
    GT_DATA-GDATU = '99999999' - GT_DATA-GDATU.
    TRANSLATE GT_DATA-GDATU(5) USING ' 0'.     "bspw. Eingabe von Jahr = '0001'
    CONDENSE GT_DATA-GDATU NO-GAPS.
    GT_DATA-GDATU = GT_DATA-GDATU.
    IF GT_DATA-GDATU > SY-DATUM.
      DELETE GT_DATA.
      CONTINUE.
    ENDIF.
    GT_DATA-DATUM = SY-DATUM.
    IF GT_DATA-FCURR = GT_DATA-TCURR.
      GT_DATA-ZBWB = '1'.
    ELSE.
      GT_DATA-ZBWB = '0'.
    ENDIF.
    READ TABLE GT_ZTTCURR WITH KEY FCURR = GT_DATA-FCURR
                                    GDATU = GT_DATA-GDATU
                                    BUKRS = GT_DATA-RCOMP.
    IF SY-SUBRC <> 0."不能保存需要转存传输
      MOVE-CORRESPONDING GT_DATA TO GT_ZTTCURRX.
      MOVE-CORRESPONDING GT_DATA TO GT_DATAX.
      APPEND GT_DATAX.
      GT_ZTTCURRX-BUKRS = GT_DATA-RCOMP.
      APPEND GT_ZTTCURRX.
      CLEAR GT_ZTTCURRX.
    ELSE."能匹配上需要比对汇率是否一致，不一致的话需要传输
      IF GT_ZTTCURR-UKURS <> GT_DATA-UKURS.
        MOVE-CORRESPONDING GT_DATA TO GT_ZTTCURRX.
        GT_ZTTCURRX-BUKRS = GT_DATA-RCOMP.
        MOVE-CORRESPONDING GT_DATA TO GT_DATAX.
        APPEND GT_DATAX.
        APPEND GT_ZTTCURRX.
        CLEAR GT_ZTTCURRX.
      ENDIF.
    ENDIF.
    MODIFY GT_DATA.
    CLEAR GT_DATA.
  ENDLOOP.

  SORT GT_DATAX[]  BY RCOMP
                     CURR
                     FCURR
                     TCURR
                     GDATU DESCENDING.
  DELETE ADJACENT DUPLICATES FROM GT_DATAX[] COMPARING RCOMP
                                                      CURR
                                                      FCURR
                                                      TCURR.
  REFRESH GT_DATA[].
  MOVE-CORRESPONDING GT_DATAX[] TO GT_DATA[].

  REFRESH GT_ZTTCURR[].
  MOVE-CORRESPONDING GT_ZTTCURRX[] TO GT_ZTTCURR[].
  IF SY-SYSID = 'S4D'."开发机文件地址
    LV_DIR = '/usr/sap/hand/upload/'.
    LV_NAME_ALL = '/usr/sap/hand/upload/'.
  ELSEIF SY-SYSID = 'S4Q'."测试机文件地址
    LV_DIR = '/tmp/handsap/upload/'.
    LV_NAME_ALL = '/tmp/handsap/upload/'.
  ELSEIF SY-SYSID = 'S4P'."生产机
    LV_DIR = '/tmp/handsap/upload/'.
    LV_NAME_ALL = '/tmp/handsap/upload/'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CREATXML
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_CREATXML .
  LR_IXML         = CL_IXML=>CREATE( ).
  LR_DOCUMENT = LR_IXML->CREATE_DOCUMENT( ).

*根节点
  PERFORM FRM_CREATE_ELEMENT USING LR_DOCUMENT SPACE 'Currencies' SPACE .
*  Main
  LOOP AT GT_DATA.
    PERFORM FRM_CREATE_ELEMENT USING LR_DOCUMENT 'Currencies' CON_REPEAT                     SPACE .
    PERFORM FRM_CREATE_ELEMENT USING LR_DOCUMENT 'Currency' 'NAME'          GT_DATA-FCURR .
    PERFORM FRM_CREATE_ELEMENT USING LR_DOCUMENT 'Currency' 'DESCRIPTION'          GT_DATA-FCURR  .
    PERFORM FRM_CREATE_ELEMENT USING LR_DOCUMENT 'Currency' 'ExchangeRate'          GT_DATA-UKURS  .
    PERFORM FRM_CREATE_ELEMENT USING LR_DOCUMENT 'Currency' 'DefaultCurrency'          GT_DATA-ZBWB .
    PERFORM FRM_CREATE_ELEMENT USING LR_DOCUMENT 'Currency' 'Unit'          GT_DATA-BUKRS ."暂时默认GW01
    CLEAR GT_DATA.
  ENDLOOP.
  CALL FUNCTION 'SDIXML_DOM_TO_XML'
    EXPORTING
      DOCUMENT      = LR_DOCUMENT
    IMPORTING
      XML_AS_STRING = LV_XML_AS_STRING
      SIZE          = LV_XML_SIZE
    TABLES
      XML_AS_TABLE  = LT_XML_AS_TABLE
    EXCEPTIONS
      NO_DOCUMENT   = 1
      OTHERS        = 2.
  LV_CSTRING = CL_PROXY_SERVICE=>XSTRING2CSTRING( LV_XML_AS_STRING ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CREATE_CHILD
*&---------------------------------------------------------------------*
*& 添加子节点
*&---------------------------------------------------------------------*
*      -->P_       text
*      -->P_       text
*      -->P_P_NUMBER  text
*&---------------------------------------------------------------------*
FORM FRM_CREATE_ELEMENT  USING PR_DOCUMENT TYPE REF TO IF_IXML_DOCUMENT "XML文档
                               P_PARENT    TYPE STRING  "父节点名称
                               P_NAME      TYPE STRING  "新建节点名称
                               P_VALUE.                 "新建节点值

  DATA: LV_VALUE       TYPE STRING,
        LR_PARENT      TYPE REF TO IF_IXML_ELEMENT,
        LR_ELEMENT     TYPE REF TO IF_IXML_ELEMENT,
        LR_PARENT_NODE TYPE REF TO IF_IXML_NODE.

  LV_VALUE  = P_VALUE.
  CONDENSE LV_VALUE.

  IF P_PARENT IS INITIAL."创建根节点
    LR_ELEMENT = LR_DOCUMENT->CREATE_ELEMENT( NAME = P_NAME ).
    LR_DOCUMENT->APPEND_CHILD( NEW_CHILD = LR_ELEMENT ).
    LR_ELEMENT->SET_ATTRIBUTE( NAME = 'xmlns:xsi' VALUE = 'http://www.w3.org/2001/XMLSchema-instance' ).
  ELSE.
*    添加子节点
    LR_PARENT = PR_DOCUMENT->FIND_FROM_NAME( NAME = P_PARENT ).
    IF SY-SUBRC = 0.
      LR_ELEMENT = PR_DOCUMENT->CREATE_SIMPLE_ELEMENT( NAME   = P_NAME
                                                       PARENT = LR_PARENT
                                                       VALUE  = LV_VALUE ).

*      根据GID获取父节点
      IF LV_GID IS NOT INITIAL AND P_PARENT EQ CON_REPEAT.
        LR_PARENT_NODE = PR_DOCUMENT->FIND_FROM_GID( GID = LV_GID ).

*        根据GID添加子节点
        LR_PARENT_NODE->APPEND_CHILD( NEW_CHILD = LR_ELEMENT ).
      ELSE.
*        直接添加子节点
        LR_PARENT->APPEND_CHILD( NEW_CHILD = LR_ELEMENT ).
      ENDIF.

*        获取con_repeat的gid
      IF P_NAME EQ CON_REPEAT.
        CLEAR LV_GID.
        LV_GID =  LR_ELEMENT->GET_GID( ).
      ENDIF.

    ENDIF.
  ENDIF.
ENDFORM.                    "FRM_CREATE_ELEMENT
*&---------------------------------------------------------------------*
*& Form FRM_UPLOADFTP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_UPLOADFTP .
  GET TIME.

* 将密码转化为SAP的格式
  LV_LEN = STRLEN( C_PWD ).
  CALL FUNCTION 'HTTP_SCRAMBLE'
    EXPORTING
      SOURCE      = C_PWD
      SOURCELEN   = LV_LEN
      KEY         = C_KEY
    IMPORTING
      DESTINATION = LV_PWD. "加密密码

"连接FTP
  TRY .
      CALL FUNCTION 'FTP_CONNECT'
        EXPORTING
          USER            = C_USER
          PASSWORD        = LV_PWD
          HOST            = C_HOST
          RFC_DESTINATION = C_RFCDEST
        IMPORTING
          HANDLE          = LV_HDL.
    CATCH CX_ROOT INTO OREF.
      MESSAGE OREF->GET_TEXT( ) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

** 进入指定的FTP服务器目录

  IF SY-SYSID = 'S4P'."生产机
    LV_COMMAND = 'cd /PRD/ToAscendo'."上傳
  ELSE.
    LV_COMMAND = 'cd /upload'."上傳
  ENDIF.

  TRY .
      CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
          HANDLE  = LV_HDL
          COMMAND = LV_COMMAND
        TABLES
          DATA    = LT_RESULT.
      LOOP AT LT_RESULT.
        WRITE:/ LT_RESULT.
      ENDLOOP.
    CATCH CX_ROOT INTO OREF.
      MESSAGE OREF->GET_TEXT( ) TYPE 'S' DISPLAY LIKE 'E'.
      PERFORM FRM_DISCONNECT.
      RETURN.
  ENDTRY.


  CONCATENATE   'Currency' '_' '[' SY-DATUM  SY-UZEIT ']' '.XML' INTO LV_FILENAME.

  CALL FUNCTION 'FTP_R3_TO_SERVER'
    EXPORTING
      HANDLE      = LV_HDL
      FNAME       = LV_FILENAME
      BLOB_LENGTH = LV_XML_SIZE
*     CHARACTER_MODE       =
    TABLES
      BLOB        = LT_XML_AS_TABLE
*     TEXT        = LT_CONTENT
*   EXCEPTIONS
*     TCPIP_ERROR = 1
*     COMMAND_ERROR        = 2
*     DATA_ERROR  = 3
*     OTHERS      = 4
    .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  PERFORM FRM_DISCONNECT."关闭连接
ENDFORM.
FORM FRM_DISCONNECT.
* disconnect
* 关闭SAP与其他系统的连接
  CALL FUNCTION 'FTP_DISCONNECT'
    EXPORTING
      HANDLE = LV_HDL.

*关闭SAP与其他系统的RFC连接.
  CALL FUNCTION 'RFC_CONNECTION_CLOSE'
    EXPORTING
      DESTINATION = C_RFCDEST
    EXCEPTIONS
      OTHERS      = 1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UPDATEDATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_UPDATEDATA .
  IF GT_ZTTCURRX[] IS NOT INITIAL.
    MODIFY ZTTCURR FROM TABLE GT_ZTTCURR.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CREATFILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_CREATFILE .

  CONCATENATE 'Currency' '_' '[' SY-DATUM  SY-UZEIT  ']' '.XML' INTO LV_FILENAME.

  LV_NAME_ALL = LV_DIR && LV_FILENAME.

  OPEN DATASET LV_NAME_ALL FOR OUTPUT IN BINARY MODE.

  IF SY-SUBRC = 0.
    TRANSFER LV_CSTRING TO  LV_NAME_ALL.
    CLOSE DATASET  LV_NAME_ALL.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DELETEFILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_DELETEFILE .
  DELETE DATASET LV_NAME_ALL.
ENDFORM.