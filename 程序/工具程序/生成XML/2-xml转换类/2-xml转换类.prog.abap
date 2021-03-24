class ZCL_TR2_XML definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF type_s_key_value.
    TYPES:fname  TYPE string.
    TYPES:fvalue TYPE string.
    TYPES:END OF type_s_key_value .
  types:
    type_t_key_value TYPE STANDARD TABLE OF type_s_key_value WITH DEFAULT KEY .
  types:
    ztr2t0190_tab TYPE STANDARD TABLE OF ztr2t0190 .

  methods CONVERT_XML_TO_ABAP
    importing
      !RESPONSE type STRING
      !BANK_IND type ZTR_BANK_INDICATOR
      !BANK_ACTION type ZTR_APP_ACTION_FOR_BANK
    exporting
      !DATA type DATA .
  methods CONVERT_VALUE_TO_XML
    importing
      !BANK_IND type ZTR_BANK_INDICATOR
      !BANK_ACTION type ZTR_APP_ACTION_FOR_BANK
      !DATA type DATA
    exporting
      !E_MSG type BAPI_MSG
      !E_TYPE type BAPI_MTYPE
      !E_XML type STRING .
  PROTECTED SECTION.
private section.

  data GT_ztr2t0190 type ztr2t0190_TAB .

  methods TRANSFORMATION
    importing
      !DOCUMENT type ref to IF_IXML_DOCUMENT
      !BANK_IND type ZTR_BANK_INDICATOR
      !BANK_ACTION type ZTR_APP_ACTION_FOR_BANK
    exporting
      !DATA type DATA .
  methods TRANSFORMATION_TABLE
    importing
      !BANK_IND type ZTR_BANK_INDICATOR
      !BANK_ACTION type ZTR_APP_ACTION_FOR_BANK
      !REPEAT_NAME type STRING
    exporting
      !LINES type I
    changing
      !NODE type ref to IF_IXML_NODE
      !TABLE type ANY TABLE .
  methods CREATE_ELEMENT
    importing
      !DOCUMENT type ref to IF_IXML_DOCUMENT
      value(PARENT) type STRING
      value(NAME) type STRING
      value(VALUE) type STRING optional
      !REPEAT type ABAP_BOOL optional
      !REPEAT_GID type I optional
      !ATTRIBUTES type TYPE_T_KEY_VALUE optional
    returning
      value(E_GID) type I .
  methods CREATE_ELEMENT_TABLE
    importing
      !TABLE type ANY TABLE
      !DOCUMENT type ref to IF_IXML_DOCUMENT
      !BANK_IND type ZTR_BANK_INDICATOR
      !BANK_ACTION type ZTR_APP_ACTION_FOR_BANK
      !PARENT type ZTR2_XMLPARENT
      !GRANDPARENT type ZTR2_XMLPARENT .
  methods CREATE_ELEMENT_STRUC
    importing
      !DATA type DATA
      !DOCUMENT type ref to IF_IXML_DOCUMENT
      !BANK_IND type ZTR_BANK_INDICATOR
      !BANK_ACTION type ZTR_APP_ACTION_FOR_BANK
      !PARENT type ZTR2_XMLPARENT
      !REPEAT_GID type I .
  methods CREATE_DEFAULT_HEADER
    importing
      !DOCUMENT type ref to IF_IXML_DOCUMENT
      !TRNCOD type ZTR2_TRNCOD
      !BANK_IND type ZTR_BANK_INDICATOR default 'BOC'
      !BANK_ACTION type ZTR_APP_ACTION_FOR_BANK default '0' .
ENDCLASS.



CLASS ZCL_TR2_XML IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TR2_XML->CONVERT_VALUE_TO_XML
* +-------------------------------------------------------------------------------------------------+
* | [--->] BANK_IND                       TYPE        ZTR_BANK_INDICATOR
* | [--->] BANK_ACTION                    TYPE        ZTR_APP_ACTION_FOR_BANK
* | [--->] DATA                           TYPE        DATA
* | [<---] E_MSG                          TYPE        BAPI_MSG
* | [<---] E_TYPE                         TYPE        BAPI_MTYPE
* | [<---] E_XML                          TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONVERT_VALUE_TO_XML.
    DATA: LR_IXML          TYPE REF TO IF_IXML,
          LR_DOCUMENT      TYPE REF TO IF_IXML_DOCUMENT,
          LV_XML_AS_STRING TYPE XSTRING,
          LV_XML_SIZE      TYPE I,
          LT_XML_AS_TABLE  TYPE DCXMLLINES,
          LV_CSTRING       TYPE STRING.

    FIELD-SYMBOLS:<FS_VALUE> TYPE ANY.

    FIELD-SYMBOLS:<FT_TABLE> TYPE ANY TABLE,
                  <FS_TABLE> TYPE ANY.

    DATA:LV_PARENT TYPE STRING,
         LV_NAME   TYPE STRING,
         LV_VALUE  TYPE STRING.

    DATA:LT_ATTRIBUTES TYPE TYPE_T_KEY_VALUE,
         LS_ATTRIBUTE  TYPE TYPE_S_KEY_VALUE.

    DATA:LS_TEST TYPE DEMO_GTT.

    DATA:REPEAT_GID  TYPE I,
         REPEAT_NAME TYPE STRING.

    SELECT * INTO TABLE @GT_ZTR2T0190 FROM ZTR2T0190
    WHERE BANK_IND = @BANK_IND
      AND BANK_ACTION = @BANK_ACTION.
    SORT GT_ZTR2T0190 BY BANK_IND BANK_ACTION ZEILE.

    LR_IXML         = CL_IXML=>CREATE( ).
    LR_DOCUMENT = LR_IXML->CREATE_DOCUMENT( ).
    IF BANK_IND = 'BOC'.
    ELSEIF BANK_IND = 'CCB'.
      LR_DOCUMENT->SET_STANDALONE( 'X' ).

      DATA(L_ENCODING) = LR_IXML->CREATE_ENCODING( CHARACTER_SET = 'GB2312'
      BYTE_ORDER = IF_IXML_ENCODING=>CO_NONE ).
      LR_DOCUMENT->SET_ENCODING( L_ENCODING ).
    ENDIF.

    IF GT_ZTR2T0190 IS NOT INITIAL.
      ME->CREATE_DEFAULT_HEADER( DOCUMENT = LR_DOCUMENT TRNCOD = GT_ZTR2T0190[ 1 ]-TRNCOD BANK_IND = BANK_IND ).
    ENDIF.

    SELECT * INTO TABLE @GT_ZTR2T0190 FROM ZTR2T0190
    WHERE BANK_IND = @BANK_IND
      AND BANK_ACTION = @BANK_ACTION.
    SORT GT_ZTR2T0190 BY BANK_IND BANK_ACTION ZEILE.

    LOOP AT GT_ZTR2T0190 INTO DATA(LS_ZRTR05450).
      LV_PARENT = LS_ZRTR05450-PARENT.
      LV_NAME = LS_ZRTR05450-NAME.
      IF LS_ZRTR05450-ATTRIBUTE1 IS NOT INITIAL.
        LS_ATTRIBUTE-FNAME = LS_ZRTR05450-ATTRIBUTE1.
        LS_ATTRIBUTE-FVALUE = LS_ZRTR05450-ATTRIBUTE_VALUE1.
        APPEND LS_ATTRIBUTE TO LT_ATTRIBUTES.
      ENDIF.
      IF LS_ZRTR05450-ATTRIBUTE2 IS NOT INITIAL.
        LS_ATTRIBUTE-FNAME = LS_ZRTR05450-ATTRIBUTE2.
        LS_ATTRIBUTE-FVALUE = LS_ZRTR05450-ATTRIBUTE_VALUE2.
        APPEND LS_ATTRIBUTE TO LT_ATTRIBUTES.
      ENDIF.
      IF LS_ZRTR05450-ATTRIBUTE3 IS NOT INITIAL.
        LS_ATTRIBUTE-FNAME = LS_ZRTR05450-ATTRIBUTE3.
        LS_ATTRIBUTE-FVALUE = LS_ZRTR05450-ATTRIBUTE_VALUE3.
        APPEND LS_ATTRIBUTE TO LT_ATTRIBUTES.
      ENDIF.
      IF LS_ZRTR05450-XML_REPEAT IS INITIAL.

        SELECT * INTO TABLE @DATA(LT_ZTR2T0190_TMP) FROM ZTR2T0190 WHERE PARENT = @LS_ZRTR05450-NAME AND BANK_ACTION = @BANK_ACTION AND BANK_IND = @BANK_IND.
        IF SY-SUBRC = 0.
          CLEAR LV_VALUE.
          IF LV_PARENT IS INITIAL.
            IF BANK_IND = 'BOC'.
              LV_PARENT = 'trans'.
            ELSEIF BANK_IND = 'CCB'.
              LV_PARENT = 'TX_INFO'.
            ENDIF.
            ME->CREATE_ELEMENT( DOCUMENT = LR_DOCUMENT
                    PARENT   = LV_PARENT
                    NAME     = LV_NAME
                    VALUE    = LV_VALUE
                    ATTRIBUTES = LT_ATTRIBUTES ).
          ELSE.
            ASSIGN COMPONENT LS_ZRTR05450-NAME OF STRUCTURE DATA TO <FS_VALUE>.
            IF SY-SUBRC NE 0.
              ASSIGN LS_TEST TO <FS_VALUE>.
            ENDIF.
            DATA(TEMP_GID) = ME->CREATE_ELEMENT( DOCUMENT = LR_DOCUMENT
                                PARENT   = LV_PARENT
                                NAME     = LV_NAME
                                VALUE    = LV_VALUE
                                ATTRIBUTES = LT_ATTRIBUTES
                                REPEAT   = ABAP_TRUE ).
            ME->CREATE_ELEMENT_STRUC( DATA = <FS_VALUE>
                                      PARENT = LS_ZRTR05450-NAME
                                      DOCUMENT = LR_DOCUMENT
                                      BANK_IND = BANK_IND
                                      BANK_ACTION = BANK_ACTION
                                      REPEAT_GID = TEMP_GID ).
          ENDIF.
        ELSE.
          ASSIGN COMPONENT LS_ZRTR05450-NAME OF STRUCTURE DATA TO <FS_VALUE>.
          IF SY-SUBRC = 0.
            LV_VALUE = <FS_VALUE>.
            IF LV_PARENT IS INITIAL.
              IF BANK_IND = 'BOC'.
                LV_PARENT = 'trans'.
              ELSEIF BANK_IND = 'CCB'.
                LV_PARENT = 'TX_INFO'.
              ENDIF.

            ENDIF.
            ME->CREATE_ELEMENT( DOCUMENT = LR_DOCUMENT
                                PARENT   = LV_PARENT
                                NAME     = LV_NAME
                                VALUE    = LV_VALUE
                                ATTRIBUTES = LT_ATTRIBUTES ).
          ENDIF.
        ENDIF.

      ELSE.

        ASSIGN COMPONENT LS_ZRTR05450-NAME OF STRUCTURE DATA TO <FT_TABLE>.

        ME->CREATE_ELEMENT_TABLE( TABLE = <FT_TABLE>
                                    DOCUMENT = LR_DOCUMENT
                                    BANK_IND = BANK_IND
                                    BANK_ACTION = BANK_ACTION
                                    PARENT = LS_ZRTR05450-NAME
                                    GRANDPARENT = LS_ZRTR05450-PARENT ).

      ENDIF.
      CLEAR:LV_VALUE,LV_PARENT,LT_ATTRIBUTES,LS_ATTRIBUTE.

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
    E_XML = CL_PROXY_SERVICE=>XSTRING2CSTRING( LV_XML_AS_STRING ).

IF BANK_IND = 'CCB'.
  E_XML = 'requestXml=' && E_XML.
ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TR2_XML->CONVERT_XML_TO_ABAP
* +-------------------------------------------------------------------------------------------------+
* | [--->] RESPONSE                       TYPE        STRING
* | [--->] BANK_IND                       TYPE        ZTR_BANK_INDICATOR
* | [--->] BANK_ACTION                    TYPE        ZTR_APP_ACTION_FOR_BANK
* | [<---] DATA                           TYPE        DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONVERT_XML_TO_ABAP.

    DATA:ixml          TYPE REF TO if_ixml.
    DATA:streamfactory TYPE REF TO if_ixml_stream_factory.
    DATA:istream       TYPE REF TO if_ixml_istream.
    DATA:c_document      TYPE REF TO cl_xml_document.
    DATA:lt_value TYPE TABLE OF type_s_key_value.
    DATA:parser   TYPE REF TO if_ixml_parser,
         document TYPE REF TO if_ixml_document.

*    CLEAR:parser,document.

    ixml          = cl_ixml=>create( ).
    streamfactory = ixml->create_stream_factory( ).
    istream       = streamfactory->create_istream_string( string = response ).
    document      =  ixml->create_document( ).
    parser        = ixml->create_parser( stream_factory = streamfactory
    istream        = istream
    document       = document ).

    IF parser->parse( ) <> 0.
      "准换xlm 错误

    ENDIF.
    "XLM 处理
    IF parser->is_dom_generating( ) = 'X'."
    ENDIF.

    me->transformation( EXPORTING bank_ind = bank_ind bank_action = bank_action document = document IMPORTING data = data ).




  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TR2_XML->CREATE_DEFAULT_HEADER
* +-------------------------------------------------------------------------------------------------+
* | [--->] DOCUMENT                       TYPE REF TO IF_IXML_DOCUMENT
* | [--->] TRNCOD                         TYPE        ZTR2_TRNCOD
* | [--->] BANK_IND                       TYPE        ZTR_BANK_INDICATOR (default ='BOC')
* | [--->] BANK_ACTION                    TYPE        ZTR_APP_ACTION_FOR_BANK (default ='0')
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CREATE_DEFAULT_HEADER.
    "中行
    TYPES:BEGIN OF TY_HEAD,
            TERMID TYPE STRING,
            TRNID  TYPE STRING,
            CUSTID TYPE STRING,
            CUSOPR TYPE STRING,
            TRNCOD TYPE STRING,
            TOKEN  TYPE STRING,
          END OF TY_HEAD.

    DATA:BEGIN OF LS_BOCB2E,
           HEAD  TYPE TY_HEAD,
           TRANS TYPE STRING,
         END OF LS_BOCB2E.
    "建行

    DATA:BEGIN OF LS_CCBGB2,
           REQUEST_SN TYPE STRING,
           CUST_ID    TYPE STRING,
           USER_ID    TYPE STRING,
           PASSWORD   TYPE STRING,
           TX_CODE    TYPE STRING,
           LANGUAGE   TYPE STRING,
           "BODY
           TXT        TYPE STRING,
         END OF LS_CCBGB2.



    DATA:LV_PARENT TYPE STRING,
         LV_NAME   TYPE STRING,
         LV_VALUE  TYPE STRING.

    DATA:LT_ATTRIBUTES TYPE TYPE_T_KEY_VALUE,
         LS_ATTRIBUTE  TYPE TYPE_S_KEY_VALUE.

    FIELD-SYMBOLS:<FS_VALUE> TYPE ANY.

    IF BANK_IND = 'BOC'."中行
      SELECT SINGLE TERMID
                    CUSTID
                    CUSOPR
      INTO CORRESPONDING FIELDS OF LS_BOCB2E-HEAD
      FROM ZTR2T0020
      WHERE BANK_IND = BANK_IND.
      LS_BOCB2E-HEAD-TRNCOD = TRNCOD.
    ELSEIF BANK_IND = 'CCB'."建行
      SELECT SINGLE CUSOPR AS USER_ID "操作员
                    CUSTID AS CUST_ID "客户编号
                    PASSWORD  "密码
                    INTO CORRESPONDING FIELDS OF LS_CCBGB2
                    FROM ZTR2T0020
                    WHERE BANK_IND = BANK_IND.
        LS_CCBGB2-REQUEST_SN = SY-DATUM && SY-UZEIT && '01'.
        LS_CCBGB2-TX_CODE = TRNCOD.
        LS_CCBGB2-LANGUAGE = 'CN'.

    ELSEIF BANK_IND = 'ICBC'."工行

    ENDIF.
    SELECT * INTO TABLE @GT_ZTR2T0190 FROM ZTR2T0190
    WHERE BANK_IND = @BANK_IND
      AND BANK_ACTION = @BANK_ACTION.
    SORT GT_ZTR2T0190 BY BANK_IND BANK_ACTION ZEILE.

    LOOP AT GT_ZTR2T0190 INTO DATA(LS_ZRTR05450).
      LV_PARENT = LS_ZRTR05450-PARENT.
      LV_NAME = LS_ZRTR05450-NAME.
      IF LS_ZRTR05450-ATTRIBUTE1 IS NOT INITIAL.
        LS_ATTRIBUTE-FNAME = LS_ZRTR05450-ATTRIBUTE1.
        LS_ATTRIBUTE-FVALUE = LS_ZRTR05450-ATTRIBUTE_VALUE1.
        APPEND LS_ATTRIBUTE TO LT_ATTRIBUTES.
      ENDIF.
      IF LS_ZRTR05450-ATTRIBUTE2 IS NOT INITIAL.
        LS_ATTRIBUTE-FNAME = LS_ZRTR05450-ATTRIBUTE2.
        LS_ATTRIBUTE-FVALUE = LS_ZRTR05450-ATTRIBUTE_VALUE2.
        APPEND LS_ATTRIBUTE TO LT_ATTRIBUTES.
      ENDIF.
      IF LS_ZRTR05450-ATTRIBUTE3 IS NOT INITIAL.
        LS_ATTRIBUTE-FNAME = LS_ZRTR05450-ATTRIBUTE3.
        LS_ATTRIBUTE-FVALUE = LS_ZRTR05450-ATTRIBUTE_VALUE3.
        APPEND LS_ATTRIBUTE TO LT_ATTRIBUTES.
      ENDIF.
      SELECT * INTO TABLE @DATA(LT_ZTR2T0190_TMP) FROM ZTR2T0190 WHERE PARENT = @LS_ZRTR05450-NAME AND BANK_ACTION = @BANK_ACTION AND BANK_IND = @BANK_IND.
      IF SY-SUBRC = 0.
        IF LV_PARENT IS INITIAL.
          ME->CREATE_ELEMENT( DOCUMENT = DOCUMENT
                              PARENT   = LV_PARENT
                              NAME     = LV_NAME
                              VALUE    = LV_VALUE
                              ATTRIBUTES = LT_ATTRIBUTES ).
        ELSE.
          IF BANK_IND = 'BOC'.
            ASSIGN COMPONENT LS_ZRTR05450-NAME OF STRUCTURE LS_BOCB2E TO <FS_VALUE>.
            DATA(TEMP_GID) = ME->CREATE_ELEMENT( DOCUMENT = DOCUMENT
                                PARENT   = LV_PARENT
                                NAME     = LV_NAME
                                VALUE    = LV_VALUE
                                ATTRIBUTES = LT_ATTRIBUTES
                                REPEAT   = ABAP_TRUE ).
            ME->CREATE_ELEMENT_STRUC( DATA = <FS_VALUE>
                                      PARENT = LS_ZRTR05450-NAME
                                      DOCUMENT = DOCUMENT
                                      BANK_IND = BANK_IND
                                      BANK_ACTION = BANK_ACTION
                                      REPEAT_GID  = TEMP_GID ).
          ELSEIF BANK_IND = 'CCB'.
            ASSIGN COMPONENT LS_ZRTR05450-NAME OF STRUCTURE LS_CCBGB2 TO <FS_VALUE>.
            TEMP_GID = ME->CREATE_ELEMENT( DOCUMENT = DOCUMENT
                                PARENT   = LV_PARENT
                                NAME     = LV_NAME
                                VALUE    = LV_VALUE
                                ATTRIBUTES = LT_ATTRIBUTES
                                REPEAT   = ABAP_TRUE ).
            ME->CREATE_ELEMENT_STRUC( DATA = <FS_VALUE>
                                      PARENT = LS_ZRTR05450-NAME
                                      DOCUMENT = DOCUMENT
                                      BANK_IND = BANK_IND
                                      BANK_ACTION = BANK_ACTION
                                      REPEAT_GID  = TEMP_GID ).

          ENDIF.

        ENDIF.

      ELSE.
        IF BANK_IND = 'BOC'.
          ASSIGN COMPONENT LS_ZRTR05450-NAME OF STRUCTURE LS_BOCB2E TO <FS_VALUE>.
          IF SY-SUBRC = 0.
            LV_VALUE = <FS_VALUE>.
          ENDIF.
          ME->CREATE_ELEMENT( DOCUMENT = DOCUMENT
                              PARENT   = LV_PARENT
                              NAME     = LV_NAME
                              VALUE    = LV_VALUE
                              ATTRIBUTES = LT_ATTRIBUTES ).
        ELSEIF BANK_IND = 'CCB'.
          ASSIGN COMPONENT LS_ZRTR05450-NAME OF STRUCTURE LS_CCBGB2 TO <FS_VALUE>.
          IF SY-SUBRC = 0.
            LV_VALUE = <FS_VALUE>.
          ENDIF.
          ME->CREATE_ELEMENT( DOCUMENT = DOCUMENT
                              PARENT   = LV_PARENT
                              NAME     = LV_NAME
                              VALUE    = LV_VALUE
                              ATTRIBUTES = LT_ATTRIBUTES ).

        ENDIF.

      ENDIF.
      CLEAR:LV_VALUE,LV_PARENT,LT_ATTRIBUTES,LS_ATTRIBUTE.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TR2_XML->CREATE_ELEMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] DOCUMENT                       TYPE REF TO IF_IXML_DOCUMENT
* | [--->] PARENT                         TYPE        STRING
* | [--->] NAME                           TYPE        STRING
* | [--->] VALUE                          TYPE        STRING(optional)
* | [--->] REPEAT                         TYPE        ABAP_BOOL(optional)
* | [--->] REPEAT_GID                     TYPE        I(optional)
* | [--->] ATTRIBUTES                     TYPE        TYPE_T_KEY_VALUE(optional)
* | [<-()] E_GID                          TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CREATE_ELEMENT.
    DATA: lv_value       TYPE string,
          lr_parent      TYPE REF TO if_ixml_element,
          lr_element     TYPE REF TO if_ixml_element,
          lr_parent_node TYPE REF TO if_ixml_node.

    lv_value  = value.
    CONDENSE lv_value.

    IF parent IS INITIAL."创建根节点
      lr_element = document->create_element( name = name ).
      document->append_child( new_child = lr_element ).
      IF attributes IS NOT INITIAL.
        LOOP AT attributes INTO DATA(ls_attribute).
          lr_element->set_attribute( name = ls_attribute-fname value = ls_attribute-fvalue ).
        ENDLOOP.
      ENDIF.

    ELSE.
*    添加子节点
      lr_parent = document->find_from_name( name = parent ).
      IF sy-subrc = 0.
        lr_element = document->create_simple_element( name   = name
                                                         parent = lr_parent
                                                         value  = lv_value ).
        IF attributes IS NOT INITIAL.
          LOOP AT attributes INTO ls_attribute.
            lr_element->set_attribute( name = ls_attribute-fname value = ls_attribute-fvalue ).
          ENDLOOP.
        ENDIF.

*      根据GID获取父节点
        IF repeat_gid IS NOT INITIAL.
          lr_parent_node = document->find_from_gid( gid = repeat_gid ).

*        根据GID添加子节点
          lr_parent_node->append_child( new_child = lr_element ).
        ELSE.
*        直接添加子节点
          lr_parent->append_child( new_child = lr_element ).
        ENDIF.

*        获取con_repeat的gid
        IF repeat = abap_true.
          CLEAR e_gid.
          e_gid =  lr_element->get_gid( ).
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TR2_XML->CREATE_ELEMENT_STRUC
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        DATA
* | [--->] DOCUMENT                       TYPE REF TO IF_IXML_DOCUMENT
* | [--->] BANK_IND                       TYPE        ZTR_BANK_INDICATOR
* | [--->] BANK_ACTION                    TYPE        ZTR_APP_ACTION_FOR_BANK
* | [--->] PARENT                         TYPE        ZTR2_XMLPARENT
* | [--->] REPEAT_GID                     TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CREATE_ELEMENT_STRUC.

    DATA:lt_ztr2t0190 TYPE TABLE OF ztr2t0190,
         ls_ztr2t0190 TYPE ztr2t0190.
    FIELD-SYMBOLS:<ft_table> TYPE ANY TABLE,
                  <fs_table> TYPE any.

    DATA:lv_parent TYPE string,
         lv_name   TYPE string,
         lv_value  TYPE string.

    FIELD-SYMBOLS:<fs_value> TYPE any.
    FIELD-SYMBOLS:<fs_value1> TYPE any.

    DATA:lt_test TYPE TABLE OF demo_gtt,
         ls_test TYPE demo_gtt.

    DATA:lt_attributes TYPE type_t_key_value,
         ls_attribute  TYPE type_s_key_value.

    ASSIGN data TO <fs_value>.

    SELECT * INTO TABLE lt_ztr2t0190 FROM ztr2t0190 WHERE bank_action = bank_action AND bank_ind = bank_ind
                                                        AND parent = parent.
    SORT lt_ztr2t0190 BY  bank_ind bank_action zeile. "ADD BY CRH
    LOOP AT lt_ztr2t0190 INTO DATA(ls_zrtr05450).
      lv_name = ls_zrtr05450-name.
      lv_parent = ls_zrtr05450-parent.

      IF ls_zrtr05450-attribute1 IS NOT INITIAL.
        ls_attribute-fname = ls_zrtr05450-attribute1.
        ls_attribute-fvalue = ls_zrtr05450-attribute_value1.
        APPEND ls_attribute TO lt_attributes.
      ENDIF.
      IF ls_zrtr05450-attribute2 IS NOT INITIAL.
        ls_attribute-fname = ls_zrtr05450-attribute2.
        ls_attribute-fvalue = ls_zrtr05450-attribute_value2.
        APPEND ls_attribute TO lt_attributes.
      ENDIF.
      IF ls_zrtr05450-attribute3 IS NOT INITIAL.
        ls_attribute-fname = ls_zrtr05450-attribute3.
        ls_attribute-fvalue = ls_zrtr05450-attribute_value3.
        APPEND ls_attribute TO lt_attributes.
      ENDIF.

      IF ls_zrtr05450-xml_repeat IS NOT INITIAL.

        ASSIGN COMPONENT ls_zrtr05450-name OF STRUCTURE <fs_value> TO <ft_table>.
        IF sy-subrc = 0.
          me->create_element_table( table = <ft_table>
                                    document = document
                                    bank_ind = bank_ind
                                    bank_action = bank_action
                                    parent = ls_zrtr05450-name
                                    grandparent = ls_zrtr05450-parent ).
        ENDIF.


      ELSE.
        SELECT * INTO TABLE @DATA(lt_ztr2t0190_tmp) FROM ztr2t0190 WHERE parent = @ls_zrtr05450-name AND bank_action = @bank_action AND bank_ind = @bank_ind.
        IF sy-subrc = 0.
          CLEAR lv_value.
          ASSIGN COMPONENT ls_zrtr05450-name OF STRUCTURE <fs_value> TO <fs_value1>.
          IF sy-subrc  NE 0.
            ASSIGN ls_test TO <fs_value1>.
          ENDIF.
          DATA(temp_gid) = me->create_element( document = document
                              parent   = lv_parent
                              name     = lv_name
                              value    = lv_value
                              attributes = lt_attributes
                              repeat_gid = repeat_gid
                              repeat   = abap_true ).
          me->create_element_struc( data = <fs_value1>
                                    parent = ls_zrtr05450-name
                                    document = document
                                    bank_ind = bank_ind
                                    bank_action = bank_action
                                    repeat_gid = temp_gid ).

        ELSE.
          ASSIGN COMPONENT ls_zrtr05450-name OF STRUCTURE <fs_value> TO <fs_value1>.
          IF sy-subrc = 0.
            lv_value = <fs_value1>.
          ENDIF.
          me->create_element( document = document
                              parent   = lv_parent
                              name     = lv_name
                              value    = lv_value
                              attributes = lt_attributes
                              repeat_gid = repeat_gid ).

        ENDIF.

      ENDIF.
      CLEAR:lv_name,lv_value.
*      CLEAR:<fs_value1>,<ft_table>.
    ENDLOOP.

    DELETE gt_ztr2t0190 WHERE parent = parent.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TR2_XML->CREATE_ELEMENT_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] TABLE                          TYPE        ANY TABLE
* | [--->] DOCUMENT                       TYPE REF TO IF_IXML_DOCUMENT
* | [--->] BANK_IND                       TYPE        ZTR_BANK_INDICATOR
* | [--->] BANK_ACTION                    TYPE        ZTR_APP_ACTION_FOR_BANK
* | [--->] PARENT                         TYPE        ZTR2_XMLPARENT
* | [--->] GRANDPARENT                    TYPE        ZTR2_XMLPARENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CREATE_ELEMENT_TABLE.

    DATA:lt_ztr2t0190 TYPE TABLE OF ztr2t0190,
         ls_ztr2t0190 TYPE ztr2t0190.
    FIELD-SYMBOLS:<ft_table>  TYPE ANY TABLE,
                  <ft_table1> TYPE ANY TABLE,
                  <fs_table>  TYPE any.

    DATA:lv_parent TYPE string,
         lv_name   TYPE string,
         lv_value  TYPE string.

    DATA:lt_test TYPE TABLE OF demo_gtt,
         ls_test TYPE demo_gtt.

    DATA:temp_ztr2t0190 TYPE TABLE OF ztr2t0190.

    FIELD-SYMBOLS:<fs_value> TYPE any.

    DATA:lt_attributes TYPE type_t_key_value,
         ls_attribute  TYPE type_s_key_value.

    DATA:lv_repeat_gid TYPE i.

    ASSIGN table TO <ft_table>.

    SELECT * INTO TABLE lt_ztr2t0190 FROM ztr2t0190 WHERE bank_action = bank_action AND bank_ind = bank_ind
                                                        AND parent = parent.
    sort lt_ztr2t0190 by bank_ind bank_action zeile."ADD BY CRH
    temp_ztr2t0190 = gt_ztr2t0190.

    LOOP AT <ft_table> ASSIGNING <fs_table>.
      lv_name = parent.
      lv_parent = grandparent.
      lv_repeat_gid = me->create_element( document = document
                      parent   = lv_parent
                      name     = lv_name
                      repeat   = abap_true ).
      gt_ztr2t0190 = temp_ztr2t0190.
      LOOP AT lt_ztr2t0190 INTO DATA(ls_zrtr05450).
        lv_name = ls_zrtr05450-name.
        lv_parent = ls_zrtr05450-parent.

        IF ls_zrtr05450-attribute1 IS NOT INITIAL.
          ls_attribute-fname = ls_zrtr05450-attribute1.
          ls_attribute-fvalue = ls_zrtr05450-attribute_value1.
          APPEND ls_attribute TO lt_attributes.
        ENDIF.
        IF ls_zrtr05450-attribute2 IS NOT INITIAL.
          ls_attribute-fname = ls_zrtr05450-attribute2.
          ls_attribute-fvalue = ls_zrtr05450-attribute_value2.
          APPEND ls_attribute TO lt_attributes.
        ENDIF.
        IF ls_zrtr05450-attribute3 IS NOT INITIAL.
          ls_attribute-fname = ls_zrtr05450-attribute3.
          ls_attribute-fvalue = ls_zrtr05450-attribute_value3.
          APPEND ls_attribute TO lt_attributes.
        ENDIF.

        IF ls_zrtr05450-xml_repeat IS NOT INITIAL.

          ASSIGN COMPONENT ls_zrtr05450-name OF STRUCTURE <fs_table> TO <ft_table1>.
          me->create_element_table( table = <ft_table1>
                                    document = document
                                    bank_ind = bank_ind
                                    bank_action = bank_action
                                    parent = ls_zrtr05450-name
                                    grandparent = ls_zrtr05450-parent ).

        ELSE.
          SELECT * INTO TABLE @DATA(lt_ztr2t0190_tmp) FROM ztr2t0190 WHERE parent = @ls_zrtr05450-name AND bank_ind = @bank_ind AND bank_action = @bank_action.
          IF sy-subrc = 0.
            CLEAR lv_value.
            ASSIGN COMPONENT ls_zrtr05450-name OF STRUCTURE <fs_table> TO <fs_value>.
            IF sy-subrc NE 0.
              ASSIGN ls_test TO <fs_value>.
            ENDIF.
            DATA(temp_repeat_gid) = me->create_element( document = document
                                parent   = lv_parent
                                name     = lv_name
                                value    = lv_value
                                attributes = lt_attributes
                                repeat   = abap_true
                                repeat_gid = lv_repeat_gid ).
            me->create_element_struc( data = <fs_value>
                                      parent = ls_zrtr05450-name
                                      document = document
                                      bank_ind = bank_ind
                                      bank_action = bank_action
                                      repeat_gid = temp_repeat_gid ).


          ELSE.
            ASSIGN COMPONENT ls_zrtr05450-name OF STRUCTURE <fs_table> TO <fs_value>.
            IF sy-subrc = 0.
              lv_value = <fs_value>.
            ENDIF.
            me->create_element( document = document
                                parent   = lv_parent
                                name     = lv_name
                                value    = lv_value
                                attributes = lt_attributes
                                repeat_gid = lv_repeat_gid ).

          ENDIF.

        ENDIF.
        CLEAR:lv_name,lv_value,lv_parent.
*        CLEAR:<fs_value>,<ft_table1>.
      ENDLOOP.
    ENDLOOP.

    DELETE gt_ztr2t0190 WHERE parent = parent.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TR2_XML->TRANSFORMATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] DOCUMENT                       TYPE REF TO IF_IXML_DOCUMENT
* | [--->] BANK_IND                       TYPE        ZTR_BANK_INDICATOR
* | [--->] BANK_ACTION                    TYPE        ZTR_APP_ACTION_FOR_BANK
* | [<---] DATA                           TYPE        DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD TRANSFORMATION.

    DATA:
      node        TYPE REF TO if_ixml_node,
      iterator    TYPE REF TO if_ixml_node_iterator,
      nodemap     TYPE REF TO if_ixml_named_node_map,
      node_parent TYPE REF TO if_ixml_node,
      attr        TYPE REF TO if_ixml_node,
      name        TYPE string,
      name1       TYPE string,
      prefix      TYPE string,
      value       TYPE string,
      indent      TYPE i,
      count       TYPE i,
      index       TYPE i.

    FIELD-SYMBOLS <fs_value> TYPE any.
    FIELD-SYMBOLS <ft_table> TYPE ANY TABLE.

*    DATA:ls_key_value TYPE type_s_key_value.
*
*    CLEAR:key_value[].

    SELECT * INTO TABLE @DATA(lt_zrtrt05430) FROM ZTR2T0010
    WHERE bank_ind = @bank_ind
      AND bank_action = @bank_action.

    node ?= document.
    CHECK NOT node IS INITIAL.
    IF node IS INITIAL.
      EXIT.
    ENDIF.

    "CREATE A NODE ITERATOR
    iterator  = node->create_iterator( ).
    "GET CURRENT NODE
    node = iterator->get_next( ).

    "LOOP OVER ALL NODES
    WHILE NOT node IS INITIAL.
*        indent = node->get_height( ) * 2.
*        indent = indent + 20.
      indent = node->get_height( ).

      CASE node->get_type( ).
        WHEN if_ixml_node=>co_node_element." ELEMENT NODE
          name = node->get_name( ).
          value   = node->get_value( ).
          READ TABLE lt_zrtrt05430 INTO DATA(ls_zrtr05430) WITH KEY name = name.
          IF sy-subrc = 0 AND ls_zrtr05430-xml_repeat = 'X'.
            ASSIGN COMPONENT name OF STRUCTURE data TO <ft_table>.
            IF sy-subrc = 0.
              transformation_table( EXPORTING bank_ind = bank_ind bank_action = bank_action  repeat_name = name IMPORTING lines = DATA(lv_times) CHANGING node = node table = <ft_table> ).
              DO lv_times - 1 TIMES.
                node = iterator->get_next( )."获取下一个节点
              ENDDO.
            ENDIF.
          ENDIF.
        WHEN if_ixml_node=>co_node_text OR
          if_ixml_node=>co_node_cdata_section."TEXT NODE
          value = node->get_value( ).
          node_parent = node->get_parent( ).
          name1 = node_parent->get_name( ).
          READ TABLE lt_zrtrt05430 INTO ls_zrtr05430 WITH KEY name = name1.
          IF sy-subrc = 0.
            ASSIGN COMPONENT name1 OF STRUCTURE data TO <fs_value>.
            IF sy-subrc = 0.
              <fs_value> = value.
            ENDIF.
          ENDIF.
      ENDCASE.

      node = iterator->get_next( )."获取下一个节点
    ENDWHILE.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TR2_XML->TRANSFORMATION_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] BANK_IND                       TYPE        ZTR_BANK_INDICATOR
* | [--->] BANK_ACTION                    TYPE        ZTR_APP_ACTION_FOR_BANK
* | [--->] REPEAT_NAME                    TYPE        STRING
* | [<---] LINES                          TYPE        I
* | [<-->] NODE                           TYPE REF TO IF_IXML_NODE
* | [<-->] TABLE                          TYPE        ANY TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD TRANSFORMATION_TABLE.

    DATA:iterator    TYPE REF TO if_ixml_node_iterator,
         nodemap     TYPE REF TO if_ixml_named_node_map,
         node_parent TYPE REF TO if_ixml_node,
         attr        TYPE REF TO if_ixml_node,
         child       TYPE REF TO if_ixml_node,
         name        TYPE string,
         name1       TYPE string,
         prefix      TYPE string,
         value       TYPE string,
         indent      TYPE i,
         count       TYPE i,
         index       TYPE i.

    DATA:first_node TYPE string.

    SELECT * INTO TABLE @DATA(lt_zrtrt05430) FROM ZTR2T0010
    WHERE bank_ind = @bank_ind
      AND bank_action = @bank_action
      AND parent = @repeat_name.

    CHECK lt_zrtrt05430 IS NOT INITIAL.

    SORT lt_zrtrt05430 BY zeile.


    FIELD-SYMBOLS:<ft_table> TYPE STANDARD TABLE,
                  <fs_table> TYPE any,
                  <fs_value> TYPE any.
    DATA:lr_line TYPE REF TO data.
    ASSIGN table TO <ft_table>.
*    CREATE DATA lr_line LIKE LINE OF <ft_table>.
*    ASSIGN lr_line->* TO <fs_table>.
    CLEAR lines.

    iterator  = node->create_iterator( ).
    "GET CURRENT NODE
    node = iterator->get_next( ).

    "LOOP OVER ALL NODES
    WHILE NOT node IS INITIAL.
*        indent = node->get_height( ) * 2.
*        indent = indent + 20.
      indent = node->get_height( ).

      CASE node->get_type( ).
        WHEN if_ixml_node=>co_node_element." ELEMENT NODE
          name = node->get_name( ).
          value   = node->get_value( ).
          IF name = repeat_name.
            APPEND INITIAL LINE TO <ft_table> ASSIGNING <fs_table>.
          ENDIF.
          READ TABLE lt_zrtrt05430 INTO DATA(ls_zrtr05430) WITH KEY name = name.
          IF sy-subrc = 0.
*            IF name = first_node.
*              APPEND INITIAL LINE TO <ft_table> ASSIGNING <fs_table>.
*            ENDIF.
            IF ls_zrtr05430-xml_repeat = 'X'.
              ASSIGN COMPONENT name OF STRUCTURE <fs_table> TO <ft_table>.
              IF sy-subrc = 0.
                me->transformation_table( EXPORTING bank_ind = bank_ind bank_action = bank_action  repeat_name = name IMPORTING lines = DATA(lv_times) CHANGING node = node table = <ft_table> ).

                DO lv_times - 1 TIMES.
                  node = iterator->get_next( )."获取下一个节点
                ENDDO.
              ENDIF.
            ENDIF.
          ENDIF.
        WHEN if_ixml_node=>co_node_text OR
          if_ixml_node=>co_node_cdata_section."TEXT NODE
          value = node->get_value( ).
          node_parent = node->get_parent( ).
          name1 = node_parent->get_name( ).
          READ TABLE lt_zrtrt05430 INTO ls_zrtr05430 WITH KEY name = name1.
          IF sy-subrc = 0.
*            IF name = first_node.
*              APPEND INITIAL LINE TO <ft_table> ASSIGNING <fs_table>.
*            ENDIF.
            ASSIGN COMPONENT name1 OF STRUCTURE <fs_table> TO <fs_value>.
            IF sy-subrc = 0.
              <fs_value> = value.
            ENDIF.
          ENDIF.
      ENDCASE.
      ADD 1 TO lines.
      node = iterator->get_next( )."获取下一个节点
    ENDWHILE.


  ENDMETHOD.
ENDCLASS.