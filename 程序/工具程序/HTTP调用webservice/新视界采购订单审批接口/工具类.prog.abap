class ZCL_XLM_CONVERT definition
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

  methods REQUEST_POST
    importing
      !REQUEST type XSTRING
      !INTERFACE_NAME type ZZMMINTERFACE
    exporting
      value(RESPONSE) type STRING
    returning
      value(ERROR) type STRING .
  methods CONVERT_XLM_TO_DOCUMENT
    importing
      !CONTENT type STRING
    exporting
      !PARSER type ref to IF_IXML_PARSER
      !DOCUMENT type ref to IF_IXML_DOCUMENT .
  methods TRANSFORMATION
    importing
      !ELEMENT type ref to IF_IXML_ELEMENT
    exporting
      !KEY_VALUE type TYPE_T_KEY_VALUE .
  methods DISPLAY_STRING_XML
    importing
      !CONTENT type STRING .
  methods DOWNLOAD_XML
    importing
      !CONTENT type STRING
      !PATH type LOCALFILE .
  methods CONVERT_XSTRING_TO_STRING
    importing
      !INPUT type XSTRING
    exporting
      !OUTPUT type STRING .
  methods CREATE_XML_ELEMENT
    importing
      !IR_DOCUMENT type ref to IF_IXML_DOCUMENT
      !I_PARENT type STRING
      !I_NAME type STRING
      !I_VALUE type STRING
      !I_REPEAT type STRING .
  PROTECTED SECTION.
private section.

  data LV_GID type I .

  methods DECODE_XSTRING_TO_STRING
    importing
      !IV_ENCODING type EPIC_XML_ENCODING
      !IV_XML_XSTRING type XSTRING
    exporting
      !EV_XML_STRING type STRING .
ENDCLASS.



CLASS ZCL_XLM_CONVERT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_XLM_CONVERT->CONVERT_XLM_TO_DOCUMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] CONTENT                        TYPE        STRING
* | [<---] PARSER                         TYPE REF TO IF_IXML_PARSER
* | [<---] DOCUMENT                       TYPE REF TO IF_IXML_DOCUMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_xlm_to_document.

    DATA:ixml          TYPE REF TO if_ixml.
    DATA:streamfactory TYPE REF TO if_ixml_stream_factory.
    DATA:istream       TYPE REF TO if_ixml_istream.
    DATA:c_document      TYPE REF TO cl_xml_document.

    CLEAR:parser,document.

    ixml          = cl_ixml=>create( ).
    streamfactory = ixml->create_stream_factory( ).
    istream       = streamfactory->create_istream_string( string = content ).
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


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_XLM_CONVERT->CONVERT_XSTRING_TO_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] INPUT                          TYPE        XSTRING
* | [<---] OUTPUT                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_xstring_to_string.

    DATA lo_conv TYPE REF TO cl_abap_conv_in_ce.
*
    CALL METHOD cl_abap_conv_in_ce=>create
      EXPORTING
        encoding    = 'UTF-8'
        endian      = 'L'
        ignore_cerr = 'X'
        replacement = '#'
        input       = input
      RECEIVING
        conv        = lo_conv.

    CALL METHOD lo_conv->read
      IMPORTING
        data = output.

*
*    call function 'HR_KR_XSTRING_TO_STRING'
*      exporting
*        in_xstring = input
*      importing
*        out_string = output.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_XLM_CONVERT->CREATE_XML_ELEMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_DOCUMENT                    TYPE REF TO IF_IXML_DOCUMENT
* | [--->] I_PARENT                       TYPE        STRING
* | [--->] I_NAME                         TYPE        STRING
* | [--->] I_VALUE                        TYPE        STRING
* | [--->] I_REPEAT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_xml_element.
    DATA: lv_value       TYPE string,
          lr_parent      TYPE REF TO if_ixml_element,
          lr_element     TYPE REF TO if_ixml_element,
          lr_parent_node TYPE REF TO if_ixml_node.

    lv_value  = i_value.
    CONDENSE lv_value.

    IF i_parent IS INITIAL."创建根节点
      lr_element = ir_document->create_element( name = i_name namespace = 'web' ).
      ir_document->append_child( new_child = lr_element ).
      CALL METHOD ir_document->if_ixml_node~set_namespace
        EXPORTING
          namespace = 'web'.
    ELSE.
*    添加子节点\
      lr_parent = ir_document->find_from_name( name = i_parent namespace = 'web' ).

      IF sy-subrc = 0.

        IF i_parent EQ i_repeat OR i_name = i_repeat.
          lr_element = ir_document->create_simple_element( name   = i_name
                                                           parent = lr_parent
                                                           value  = lv_value
                                                           namespace = 'sap' ).
        ELSE.
          lr_element = ir_document->create_simple_element( name   = i_name
                                                           parent = lr_parent
                                                           value  = lv_value
                                                           namespace = 'web' ).
        ENDIF.

*      根据GID获取父节点


        IF lv_gid IS NOT INITIAL AND i_parent EQ i_repeat.
          lr_parent_node = ir_document->find_from_gid( gid = lv_gid ).

*        根据GID添加子节点
          lr_parent_node->append_child( new_child = lr_element ).
        ELSE.
*        直接添加子节点
          lr_parent->append_child( new_child = lr_element ).
        ENDIF.

*        获取con_repeat的gid
        IF i_name EQ i_repeat.
          CLEAR lv_gid.
          lv_gid =  lr_element->get_gid( ).
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_XLM_CONVERT->DECODE_XSTRING_TO_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENCODING                    TYPE        EPIC_XML_ENCODING
* | [--->] IV_XML_XSTRING                 TYPE        XSTRING
* | [<---] EV_XML_STRING                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD decode_xstring_to_string.
    DATA lr_cx_root TYPE REF TO cx_root.


    TRY.

        CLEAR ev_xml_string.

        CALL TRANSFORMATION epic_xslt_decode_xstr2str
           SOURCE XML iv_xml_xstring
           RESULT XML ev_xml_string.


      CATCH cx_transformation_error INTO lr_cx_root  ##no_handler.

    ENDTRY.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_XLM_CONVERT->DISPLAY_STRING_XML
* +-------------------------------------------------------------------------------------------------+
* | [--->] CONTENT                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD display_string_xml.

    DATA:lv_parser     TYPE REF TO if_ixml_parser.
    DATA:lv_document   TYPE REF TO if_ixml_document.
    DATA:lc_document    TYPE REF TO cl_xml_document.

    IF NOT lc_document IS BOUND.
      CREATE OBJECT lc_document.
    ENDIF.

    me->convert_xlm_to_document( EXPORTING content = content IMPORTING parser = lv_parser document = lv_document ).

    lc_document->create_with_dom( document = lv_document ).

    CALL METHOD lc_document->display.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_XLM_CONVERT->DOWNLOAD_XML
* +-------------------------------------------------------------------------------------------------+
* | [--->] CONTENT                        TYPE        STRING
* | [--->] PATH                           TYPE        LOCALFILE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_xml.

    DATA:localfile TYPE localfile.
    DATA:lv_parser     TYPE REF TO if_ixml_parser.
    DATA:lv_document   TYPE REF TO if_ixml_document.
    DATA:lc_document    TYPE REF TO cl_xml_document.

    IF NOT lc_document IS BOUND.
      CREATE OBJECT lc_document.
    ENDIF.

    me->convert_xlm_to_document( EXPORTING content = content IMPORTING parser = lv_parser document = lv_document ).

    lc_document->create_with_dom( document = lv_document ).

    CONCATENATE path '\' sy-uname '.XML' INTO localfile.
    lc_document->export_to_file( filename = localfile ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_XLM_CONVERT->REQUEST_POST
* +-------------------------------------------------------------------------------------------------+
* | [--->] REQUEST                        TYPE        XSTRING
* | [--->] INTERFACE_NAME                 TYPE        ZZMMINTERFACE
* | [<---] RESPONSE                       TYPE        STRING
* | [<-()] ERROR                          TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD request_post.

    DATA:http_client          TYPE REF TO if_http_client.
    DATA:lv_system_fault      TYPE REF TO cx_ai_system_fault.
    DATA:lv_application_fault TYPE REF TO cx_ai_application_fault.
    DATA:lv_err_text          TYPE string.
    DATA request_str          TYPE string.

***add by hand crh 20190722  request 同时由string改为xstring start
    DATA: request1  TYPE string.
***dd by hand crh 20190722 end.

    DATA:ls_ztmm_auth_conf TYPE ztmm_auth_conf.
    DATA:ls_url TYPE string,
         ls_username TYPE string,
         ls_password TYPE string.

    CLEAR:ls_ztmm_auth_conf,ls_url,ls_username,ls_password.

    SELECT SINGLE * INTO ls_ztmm_auth_conf FROM ztmm_auth_conf WHERE zinterface = interface_name.

    ls_url = ls_ztmm_auth_conf-zwsdl.
    ls_username = ls_ztmm_auth_conf-zusername.
    ls_password = ls_ztmm_auth_conf-zpassword.

    "Create HTTP client object
*    cl_http_client=>create_by_destination( EXPORTING destination = dest IMPORTING client = http_client ).

    "SET request uri (/<path>[?<querystring>])W
*    cl_http_utility=>set_request_uri( request = http_client->request uri = '/dir/wsdl?p=ic/b8894e19071939d1a073c0521ec34e9b' ).
    cl_http_client=>create_by_url( EXPORTING url = ls_url
                                   IMPORTING client = http_client ).
    "http://podevsrv.shenghongtec.com:50000/dir/wsdl?p=ic/b8894e19071939d1a073c0521ec34e9b

    me->convert_xstring_to_string( EXPORTING input = request  IMPORTING output = request1  ).

    REPLACE ALL OCCURRENCES OF '<?xml version="1.0" encoding="utf-8"?>' IN request1 WITH ''.

    "构造查询报文
    CONCATENATE
    '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:web="webservices.services.weaver.com.cn" xmlns:sap="http://sap.sh.interfaces.weaver">'
    '<soapenv:Header/>'
    '<soapenv:Body>'
    request1
    '</soapenv:Body>'
    ' </soapenv:Envelope>'
    INTO request_str.

    http_client->authenticate(
    username             = ls_username
    password             = ls_password ).

    http_client->request->set_header_field( name = 'server_protocol' value = 'HTTP/1.2' ).
    http_client->request->set_version(
    if_http_request=>co_protocol_version_1_0 ).
    http_client->request->set_header_field( name = 'soapAction' value = 'http://sap.com/xi/WebService/soap1.1' ).
    http_client->request->set_header_field(
     EXPORTING
       name  = 'Content-Type'
       value = 'text/xml;charset=utf-8' ) .

    http_client->request->set_method( if_http_request=>co_request_method_post ).
    http_client->request->set_cdata( data =  request_str ).

    http_client->send( EXCEPTIONS http_communication_failure = 1 http_invalid_state = 2 ).
    TRY.
        http_client->receive( EXCEPTIONS http_communication_failure = 1 http_invalid_state = 2 http_processing_failed = 3 ).
      CATCH cx_ai_system_fault INTO lv_system_fault.
        lv_err_text = lv_system_fault->get_text( ).
      CATCH cx_ai_application_fault INTO lv_application_fault.
        lv_err_text = lv_application_fault->get_text( ).
    ENDTRY.

    response = http_client->response->get_cdata( ).
    http_client->close( ).

    error = lv_err_text.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_XLM_CONVERT->TRANSFORMATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] ELEMENT                        TYPE REF TO IF_IXML_ELEMENT
* | [<---] KEY_VALUE                      TYPE        TYPE_T_KEY_VALUE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD transformation.

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

    DATA:ls_key_value TYPE type_s_key_value.

    CLEAR:key_value[].



    node ?= element.
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
          nodemap = node->get_attributes( ).

          IF NOT nodemap IS INITIAL."ATTRIBUTES
            count = nodemap->get_length( ).
            DO count TIMES.
              index  = sy-index - 1.
              attr   = nodemap->get_item( index ).
              name   = attr->get_name( ).
              prefix = attr->get_namespace_prefix( ).
              value  = attr->get_value( ).
              "记录字段名、字段值
              CLEAR:ls_key_value.
              ls_key_value-fname  = name .
              ls_key_value-fvalue = value.
              APPEND ls_key_value TO key_value .
            ENDDO.
          ENDIF.
        WHEN if_ixml_node=>co_node_text OR
          if_ixml_node=>co_node_cdata_section."TEXT NODE
          value = node->get_value( ).
          node_parent = node->get_parent( ).
          name1 = node_parent->get_name( ).
          "记录字段名、字段值
          CLEAR:ls_key_value.
          ls_key_value-fname  = name1.
          ls_key_value-fvalue = value.
          APPEND ls_key_value TO key_value .
      ENDCASE.

      node = iterator->get_next( )."获取下一个节点
    ENDWHILE.


  ENDMETHOD.
ENDCLASS.