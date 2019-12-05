*&---------------------------------------------------------------------*
*& Report Z15405_002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z15405_002.

TYPE-POOLS: IXML.

TYPES: BEGIN OF T_XML_LINE,
         DATA(256) TYPE X,
       END OF T_XML_LINE.

DATA: L_IXML          TYPE REF TO IF_IXML,
      L_STREAMFACTORY TYPE REF TO IF_IXML_STREAM_FACTORY,
      L_PARSER        TYPE REF TO IF_IXML_PARSER,
      L_ISTREAM       TYPE REF TO IF_IXML_ISTREAM,
      L_DOCUMENT      TYPE REF TO IF_IXML_DOCUMENT,
      L_NODE          TYPE REF TO IF_IXML_NODE,
      L_XMLDATA       TYPE STRING.

DATA: L_ELEM      TYPE REF TO IF_IXML_ELEMENT,
      L_ROOT_NODE TYPE REF TO IF_IXML_NODE,
      L_NEXT_NODE TYPE REF TO IF_IXML_NODE,
      L_NAME      TYPE STRING,
      L_ITERATOR  TYPE REF TO IF_IXML_NODE_ITERATOR.

DATA: L_XML_TABLE      TYPE TABLE OF T_XML_LINE,
      L_XML_LINE       TYPE T_XML_LINE,
      L_XML_TABLE_SIZE TYPE I.
DATA: L_FILENAME        TYPE STRING.

TYPES:
  BEGIN OF TYPE_S_KEY_VALUE.
TYPES:FNAME  TYPE STRING.
TYPES:FVALUE TYPE STRING.
TYPES:END OF TYPE_S_KEY_VALUE .
TYPES:
  TYPE_T_KEY_VALUE TYPE STANDARD TABLE OF TYPE_S_KEY_VALUE WITH DEFAULT KEY .

DATA:KEY_VALUE    TYPE TYPE_T_KEY_VALUE,
     LS_KEY_VALUE TYPE TYPE_S_KEY_VALUE.



PARAMETERS: PA_FILE TYPE CHAR1024 DEFAULT 'C:\Users\Shawn\Desktop\1234.XML'.
* Validation of XML file: Only DTD included in xml document is supported
PARAMETERS: PA_VAL  TYPE CHAR1 AS CHECKBOX.

START-OF-SELECTION.

*   Creating the main iXML factory
  L_IXML = CL_IXML=>CREATE( ).

*   Creating a stream factory
  L_STREAMFACTORY = L_IXML->CREATE_STREAM_FACTORY( ).

  PERFORM GET_XML_TABLE CHANGING L_XML_TABLE_SIZE L_XML_TABLE.


*   wrap the table containing the file into a stream
  L_ISTREAM = L_STREAMFACTORY->CREATE_ISTREAM_ITABLE( TABLE = L_XML_TABLE
                                                  SIZE  = L_XML_TABLE_SIZE ).

*   Creating a document
  L_DOCUMENT = L_IXML->CREATE_DOCUMENT( ).

*   Create a Parser
  L_PARSER = L_IXML->CREATE_PARSER( STREAM_FACTORY = L_STREAMFACTORY
                                    ISTREAM        = L_ISTREAM
                                    DOCUMENT       = L_DOCUMENT ).

*   Validate a document
  IF PA_VAL EQ 'X'.
    L_PARSER->SET_VALIDATING( MODE = IF_IXML_PARSER=>CO_VALIDATE ).
  ENDIF.

*   Parse the stream
  IF L_PARSER->PARSE( ) NE 0.
    IF L_PARSER->NUM_ERRORS( ) NE 0.
      DATA: PARSEERROR TYPE REF TO IF_IXML_PARSE_ERROR,
            STR        TYPE STRING,
            I          TYPE I,
            COUNT      TYPE I,
            INDEX      TYPE I.

      COUNT = L_PARSER->NUM_ERRORS( ).
      WRITE: COUNT, ' parse errors have occured:'.
      INDEX = 0.
      WHILE INDEX < COUNT.
        PARSEERROR = L_PARSER->GET_ERROR( INDEX = INDEX ).
        I = PARSEERROR->GET_LINE( ).
        WRITE: 'line: ', I.
        I = PARSEERROR->GET_COLUMN( ).
        WRITE: 'column: ', I.
        STR = PARSEERROR->GET_REASON( ).
        WRITE: STR.
        INDEX = INDEX + 1.
      ENDWHILE.
    ENDIF.
  ENDIF.

*   Process the document
  IF L_PARSER->IS_DOM_GENERATING( ) EQ 'X'.
    PERFORM PROCESS_DOM USING L_DOCUMENT.
    PERFORM CONVERT_TO_TABLE.
  ENDIF.


*&--------------------------------------------------------------------*
*&      Form  get_xml_table
*&--------------------------------------------------------------------*
FORM GET_XML_TABLE CHANGING L_XML_TABLE_SIZE TYPE I
                            L_XML_TABLE      TYPE STANDARD TABLE.

*   Local variable declaration
  DATA: L_LEN     TYPE I,
        L_LEN2    TYPE I,
        L_TAB     TYPE TSFIXML,
        L_CONTENT TYPE STRING,
        L_STR1    TYPE STRING,
        C_CONV    TYPE REF TO CL_ABAP_CONV_IN_CE,
        L_ITAB    TYPE TABLE OF STRING.


  L_FILENAME = PA_FILE.
*   upload a file from the client's workstation
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
    EXPORTING
      FILENAME   = L_FILENAME
      FILETYPE   = 'BIN'
    IMPORTING
      FILELENGTH = L_XML_TABLE_SIZE
    CHANGING
      DATA_TAB   = L_XML_TABLE
    EXCEPTIONS
      OTHERS     = 19.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*   Writing the XML document to the screen
  CLEAR L_STR1.
  LOOP AT L_XML_TABLE INTO L_XML_LINE.
    C_CONV = CL_ABAP_CONV_IN_CE=>CREATE( INPUT = L_XML_LINE-DATA REPLACEMENT = SPACE  ).
    C_CONV->READ( IMPORTING DATA = L_CONTENT LEN = L_LEN ).
    CONCATENATE L_STR1 L_CONTENT INTO L_STR1.
  ENDLOOP.
  L_STR1 = L_STR1+0(L_XML_TABLE_SIZE).
  SPLIT L_STR1 AT CL_ABAP_CHAR_UTILITIES=>CR_LF INTO TABLE L_ITAB.
  WRITE: /.
  WRITE: /' XML File'.
  WRITE: /.
  LOOP AT L_ITAB INTO L_STR1.
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB IN
      L_STR1 WITH SPACE.
    WRITE: / L_STR1.
  ENDLOOP.
  WRITE: /.
ENDFORM.                    "get_xml_table

*&--------------------------------------------------------------------*
*&      Form  process_dom
*&--------------------------------------------------------------------*
FORM PROCESS_DOM USING DOCUMENT TYPE REF TO IF_IXML_DOCUMENT.

  DATA: NODE        TYPE REF TO IF_IXML_NODE,
        NODE_PARENT TYPE REF TO IF_IXML_NODE,
        ITERATOR    TYPE REF TO IF_IXML_NODE_ITERATOR,
        NODEMAP     TYPE REF TO IF_IXML_NAMED_NODE_MAP,
        ATTR        TYPE REF TO IF_IXML_NODE,
        NAME        TYPE STRING,
        NAME1       TYPE STRING,
        PREFIX      TYPE STRING,
        VALUE       TYPE STRING,
        INDENT      TYPE I,
        COUNT       TYPE I,
        INDEX       TYPE I.

  "抬头
  TYPES: BEGIN OF TY_HEAD,
           ID                   TYPE C    LENGTH 20,
           EXTERNALUNIT         TYPE C    LENGTH 50,
           INVOICENO            TYPE C    LENGTH 50,
           MATCHINGINVOICENO    TYPE C    LENGTH 50,
           INVOICEDATE          TYPE D,
           DUEDATE              TYPE D,
           ARRIVALDATE          TYPE D,
           IMPORTDATE           TYPE D,
           ACCOUNTINGDATE       TYPE D,
           PRELREGDATE          TYPE D,
           DEFREGDATE           TYPE D,
           AMOUNT               TYPE WRBTR,
           VATAMOUNT            TYPE WRBTR,
           CURRENCYCODE         TYPE C    LENGTH 50,
           ORDERNO              TYPE C    LENGTH 50,
           REFERENCE            TYPE C    LENGTH 50,
           STAMP                TYPE C    LENGTH 50,
           RECIEVERMESSAGE      TYPE C    LENGTH 250,
           EXTERNALINVOICEID    TYPE C    LENGTH 50,
           SUPPLIERIDENT        TYPE C    LENGTH 50,
           SUPPLIERNO1          TYPE C    LENGTH 50,
           SUPPLIERNO2          TYPE C    LENGTH 50,
           SUPPLIERNO3          TYPE C    LENGTH 50,
           SUPPLIERNO4          TYPE C    LENGTH 50,
           TERMSOFPAYMENT       TYPE C    LENGTH 50,
           VERIFICATIONNO       TYPE C    LENGTH 50,
           VERIFICATIONSERIE    TYPE C    LENGTH 50,
           VERIFICATIONDATE     TYPE D,
           PROCESSED            TYPE I,
           INVOICESTATUSID      TYPE I,
           UNITID               TYPE I,
           FACTORINGSUPPLIER    TYPE C    LENGTH 50,
           DATEPROCESSED        TYPE D,
           ORDERINVOICESTATUSID TYPE C,
           DOMESTICAMOUNT       TYPE WRBTR,
           OCRNO                TYPE C    LENGTH 50,
           SUPPLIEREXTRAFIELD1  TYPE C    LENGTH 50,
           SUPPLIEREXTRAFIELD2  TYPE C    LENGTH 50,
           SUPPLIEREXTRAFIELD3  TYPE C    LENGTH 50,
           SUPPLIEREXTRAFIELD4  TYPE C    LENGTH 50,
           SUPPLIEREXTRAFIELD5  TYPE C    LENGTH 50,
           SUPPLIEREXTRAFIELD6  TYPE C    LENGTH 50,
           SUPPLIEREXTRAFIELD7  TYPE C    LENGTH 50,
           SUPPLIEREXTRAFIELD8  TYPE C    LENGTH 50,
           SUPPLIEREXTRAFIELD9  TYPE C    LENGTH 50,
           SUPPLIEREXTRAFIELD10 TYPE C    LENGTH 50,
           VATCODE              TYPE C    LENGTH 50,
           PAYBLOCK             TYPE C,
         END OF TY_HEAD.
  "行项目
  TYPES:BEGIN OF TY_ITEM,
          ID                  TYPE C    LENGTH 4,
          INVOICEID           TYPE C    LENGTH 20,
          ACCOUNT             TYPE C    LENGTH 50,
          OBJECT1             TYPE C    LENGTH 50,
          OBJECT2             TYPE C    LENGTH 50,
          OBJECT3             TYPE C    LENGTH 50,
          OBJECT4             TYPE C    LENGTH 50,
          OBJECT5             TYPE C    LENGTH 50,
          OBJECT6             TYPE C    LENGTH 50,
          OBJECT7             TYPE C    LENGTH 50,
          OBJECT8             TYPE C    LENGTH 50,
          OBJECT9             TYPE C    LENGTH 50,
          OBJECT10            TYPE C    LENGTH 50,
          QUANTITY            TYPE C    LENGTH 50,
          AMOUNT              TYPE WRBTR,
          TEXT                TYPE C LENGTH 250,
          ALLOCATIONROWID     TYPE C LENGTH 4,
          ALLOCATIONROWTYPEID TYPE C,
          ORDERNO             TYPE C    LENGTH 50,
          ALLOCATIONSIGN      TYPE C    LENGTH 50,
          INSPECTIONSIGN      TYPE C    LENGTH 50,
          ATTESTSIGN          TYPE C    LENGTH 50,
          ACCRUALACCOUNT      TYPE C    LENGTH 50,
          ACCRUALDATEFROM     TYPE D,
          ACCRUALDATETO       TYPE D,
          ACCRUALEXTERNALID   TYPE C    LENGTH 50,
          ROWNO               TYPE C LENGTH 4,
        END OF TY_ITEM.

  DATA:LT_TABLE TYPE TABLE OF TY_HEAD.
  DATA:LS_TABLE TYPE  TY_HEAD.
  DATA:LS_ROW TYPE TY_ITEM.
  DATA:LT_ROW TYPE TABLE OF TY_ITEM.
  DATA:LV_ROW TYPE ABAP_BOOL.

  DATA:LV_CRLF TYPE STRING.

  FIELD-SYMBOLS <FS_ANY> TYPE ANY.

  CALL METHOD CL_ABAP_CONV_IN_CE=>UCCP
    EXPORTING
      UCCP = '0D00'
    RECEIVING
      CHAR = LV_CRLF.


  NODE ?= DOCUMENT.

  CHECK NOT NODE IS INITIAL.

  ULINE.
  WRITE: /.
  WRITE: /' DOM-TREE'.
  WRITE: /.
  IF NODE IS INITIAL. EXIT. ENDIF.
*   create a node iterator
  ITERATOR  = NODE->CREATE_ITERATOR( ).
*   get current node
  NODE = ITERATOR->GET_NEXT( ).

*   loop over all nodes
  WHILE NOT NODE IS INITIAL.
*        indent = node->get_height( ) * 2.
*        indent = indent + 20.
    INDENT = NODE->GET_HEIGHT( ).

    CASE NODE->GET_TYPE( ).
      WHEN IF_IXML_NODE=>CO_NODE_ELEMENT." ELEMENT NODE
        NAME = NODE->GET_NAME( ).
        VALUE   = NODE->GET_VALUE( ).
        NODEMAP = NODE->GET_ATTRIBUTES( ).
        TRANSLATE NAME TO UPPER CASE.

        IF NAME = 'INVOICE'.
          IF LV_ROW = ABAP_TRUE.
            APPEND LS_TABLE TO LT_TABLE.
            CLEAR LS_TABLE.
          ENDIF.
          IF LS_ROW IS NOT INITIAL.
            APPEND LS_ROW TO LT_ROW.
            CLEAR LS_ROW.
          ENDIF.
          LV_ROW = ABAP_FALSE.
        ELSEIF NAME = 'ALLOCATIONROW'.
          IF LS_ROW IS NOT INITIAL.
            APPEND LS_ROW TO LT_ROW.
            CLEAR LS_ROW.
          ENDIF.
          LV_ROW = ABAP_TRUE.
        ENDIF.

        IF NOT NODEMAP IS INITIAL."ATTRIBUTES
          COUNT = NODEMAP->GET_LENGTH( ).
          DO COUNT TIMES.
            INDEX  = SY-INDEX - 1.
            ATTR   = NODEMAP->GET_ITEM( INDEX ).
            NAME   = ATTR->GET_NAME( ).
            PREFIX = ATTR->GET_NAMESPACE_PREFIX( ).
            VALUE  = ATTR->GET_VALUE( ).
            "记录字段名、字段值
            CLEAR:LS_KEY_VALUE.
            LS_KEY_VALUE-FNAME  = NAME .
            LS_KEY_VALUE-FVALUE = VALUE.
            APPEND LS_KEY_VALUE TO KEY_VALUE .
          ENDDO.
        ENDIF.
      WHEN IF_IXML_NODE=>CO_NODE_TEXT OR
        IF_IXML_NODE=>CO_NODE_CDATA_SECTION."TEXT NODE
        VALUE = NODE->GET_VALUE( ).
        NODE_PARENT = NODE->GET_PARENT( ).
        NAME1 = NODE_PARENT->GET_NAME( ).
        TRANSLATE NAME1 TO UPPER CASE.
        IF LV_ROW = ABAP_TRUE AND VALUE NA LV_CRLF.
          ASSIGN COMPONENT NAME1 OF STRUCTURE LS_ROW TO <FS_ANY>.
          IF SY-SUBRC = 0.
            <FS_ANY> = VALUE.
          ENDIF.
        ELSEIF LV_ROW = ABAP_FALSE AND VALUE NA LV_CRLF.
          ASSIGN COMPONENT NAME1 OF STRUCTURE LS_TABLE TO <FS_ANY>.
          IF SY-SUBRC = 0.
            <FS_ANY> = VALUE.
          ENDIF.
        ENDIF.
        "记录字段名、字段值
*        CLEAR:LS_KEY_VALUE.
*        LS_KEY_VALUE-FNAME  = NAME1.
*        LS_KEY_VALUE-FVALUE = VALUE.
*        APPEND LS_KEY_VALUE TO KEY_VALUE .
    ENDCASE.

    NODE = ITERATOR->GET_NEXT( )."获取下一个节点
  ENDWHILE.

  IF LS_ROW IS NOT INITIAL.
    APPEND LS_ROW TO LT_ROW.
  ENDIF.
  IF LS_TABLE IS NOT INITIAL.
    APPEND LS_TABLE TO LT_TABLE.
  ENDIF.
ENDFORM.                    "process_dom
*&---------------------------------------------------------------------*
*& Form CONVERT_TO_TABLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CONVERT_TO_TABLE .




*  LOOP AT KEY_VALUE INTO LS_KEY_VALUE.
*    TRANSLATE LS_KEY_VALUE-FNAME TO UPPER CASE.
*    IF LS_KEY_VALUE-FNAME = 'INVOICE'.
*      IF LV_ROW = ABAP_TRUE.
*        APPEND LS_TABLE TO LT_TABLE.
*      ENDIF.
*      LV_ROW = ABAP_FALSE.
*    ELSEIF LS_KEY_VALUE-FNAME = 'ALLOCATIONROW'.
*      IF LS_ROW IS NOT INITIAL.
*        APPEND LS_ROW TO LS_TABLE-ROW.
*        CLEAR LS_ROW.
*      ENDIF.
*      LV_ROW = ABAP_TRUE.
*    ENDIF.
*    IF LV_ROW = ABAP_TRUE AND LS_KEY_VALUE-FVALUE NA LV_CRLF.
*      ASSIGN COMPONENT LS_KEY_VALUE-FNAME OF STRUCTURE LS_ROW TO <FS_ANY>.
*      IF SY-SUBRC = 0.
*        <FS_ANY> = LS_KEY_VALUE-FVALUE.
*      ENDIF.
*    ELSEIF LV_ROW = ABAP_FALSE AND LS_KEY_VALUE-FVALUE NA LV_CRLF.
*      ASSIGN COMPONENT LS_KEY_VALUE-FNAME OF STRUCTURE LS_TABLE TO <FS_ANY>.
*      IF SY-SUBRC = 0.
*        <FS_ANY> = LS_KEY_VALUE-FVALUE.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
ENDFORM.