class ZCL_UTIL_XML definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  types:
    BEGIN OF type_s_key_value.
    TYPES:fname  TYPE string.
    TYPES:fvalue TYPE string.
    TYPES:END OF type_s_key_value .
  types:
    type_t_key_value TYPE STANDARD TABLE OF type_s_key_value WITH DEFAULT KEY .

  constants DESTINATION type RFCDEST value 'NBCB_API' ##NO_TEXT.

  methods REQUEST_POST
    importing
      !REQUEST type XSTRING
      !DEST type RFCDEST default DESTINATION
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
protected section.
private section.

  methods DECODE_XSTRING_TO_STRING
    importing
      !IV_ENCODING type EPIC_XML_ENCODING
      !IV_XML_XSTRING type XSTRING
    exporting
      !EV_XML_STRING type STRING .
ENDCLASS.



CLASS ZCL_UTIL_XML IMPLEMENTATION.


  METHOD CONVERT_XLM_TO_DOCUMENT.

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


  method CONVERT_XSTRING_TO_STRING.

    data lo_conv type ref to cl_abap_conv_in_ce.
*
    call method cl_abap_conv_in_ce=>create
      exporting
        encoding    = 'UTF-8'
        endian      = 'L'
        ignore_cerr = 'X'
        replacement = '#'
        input       = input
      receiving
        conv        = lo_conv.

    call method lo_conv->read
      importing
        data = output.

*
*    call function 'HR_KR_XSTRING_TO_STRING'
*      exporting
*        in_xstring = input
*      importing
*        out_string = output.


  endmethod.


  method DECODE_XSTRING_TO_STRING.
    data lr_cx_root type ref to cx_root.


  try.

      clear ev_xml_string.

      call transformation epic_xslt_decode_xstr2str
         source xml iv_xml_xstring
         result xml ev_xml_string.


    catch cx_transformation_error into lr_cx_root  ##no_handler.

  endtry.


  endmethod.


  METHOD DISPLAY_STRING_XML.

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


  METHOD DOWNLOAD_XML.

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


method IF_HTTP_EXTENSION~HANDLE_REQUEST.

  data request_str          type xstring.
  data response_str         type string.
  data:lv_err_text          type string.

  data:request_method   type string.



  "获取传递过来的参数 ~request_method
  request_method = server->request->get_header_field( '~request_method' ) .

  "POST http Method CO_REQUEST_METHOD_GET
  if request_method = if_http_request=>co_request_method_get.

    server->response->set_status( code = 200 reason = 'Ok' ).
    "server->response->set_content_type( 'application/json' ).
    server->response->set_cdata( data = response_str ).

  endif.

  "POST http method
  "IF request_method = 'POST'.

  if request_method = if_http_request=>co_request_method_post.


    " request_str = server->request->get_cdata( ).
    request_str = server->request->get_data( ).


    lv_err_text = me->request_post( exporting request = request_str importing response = response_str ).
*    lv_err_text = me->request_post( exporting request = request_str1 importing response = response_str ). "modify by hand crh 20190723

    if lv_err_text is initial.

      "Send the response back
      server->response->set_status( code = 200 reason = 'Ok' ).
      server->response->set_cdata( data = response_str ).

    else."500
      response_str = lv_err_text && response_str.
      server->response->set_status( code = 500 reason = 'Ok' ).
      server->response->set_cdata( data = response_str ).

    endif.

  endif.

  " PUT http method: modify
  if request_method = 'PUT'.

  endif.

  "DELETE http method: delete
  if request_method = 'DELETE'.

  endif.

endmethod.


  method REQUEST_POST.

    data:http_client          type ref to if_http_client.
    data:lv_system_fault      type ref to cx_ai_system_fault.
    data:lv_application_fault type ref to cx_ai_application_fault.
    data:lv_err_text          type string.
    data request_str          type string.

***add by hand crh 20190722  request 同时由string改为xstring start
    data: request1  type string.
***dd by hand crh 20190722 end.

    "Create HTTP client object
    cl_http_client=>create_by_destination( exporting destination = dest importing client = http_client ).

    "SET request uri (/<path>[?<querystring>])W
    cl_http_utility=>set_request_uri( request = http_client->request uri = '/BisOutPlatform/services/erpPlatform?wsdl' ).

    http_client->request->set_method( if_http_request=>co_request_method_post ).

***add by hand crh 20190722 start
*    me->DECODE_XSTRING_TO_STRING( exporting IV_ENCODING = 'UTF-8' IV_XML_XSTRING = request  importing EV_XML_STRING = request1  ).
    me->CONVERT_XSTRING_TO_STRING( exporting INPUT = request  importing OUTPUT = request1  ).
***dd by hand crh 20190722 end.


    "构造查询报文
    concatenate
    '<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:web="http://webservice.bedp.byttersoft.com">'
    '<soap:Header/>'
    '<soap:Body>'
    '<web:serverErpXml>'
    '<web:erpReqXml>'
    '<![CDATA['
*    request
    request1 "MODIFY BY CRH 20190723
    ']]>'
    '</web:erpReqXml>'
    '</web:serverErpXml>'
    '</soap:Body>'
    ' </soap:Envelope>'
    into request_str.

    http_client->request->set_header_field( name = 'server_protocol' value = 'HTTP/1.2' ).
    http_client->request->set_header_field( name = 'soapAction' value = 'http://172.20.0.40:8090/BisOutPlatform/services/erpPlatform/serverErpXml' ).
    http_client->request->set_cdata( data =  request_str ).
    http_client->send( exceptions http_communication_failure = 1 http_invalid_state = 2 ).
    try.
        http_client->receive( exceptions http_communication_failure = 1 http_invalid_state = 2 http_processing_failed = 3 ).
      catch cx_ai_system_fault into lv_system_fault.
        lv_err_text = lv_system_fault->get_text( ).
      catch cx_ai_application_fault into lv_application_fault.
        lv_err_text = lv_application_fault->get_text( ).
    endtry.
    response = http_client->response->get_cdata( ).
    http_client->close( ).

    "返回的SOAP 不是 最终要的报文 预处理
    data:lv_parser     type ref to if_ixml_parser.
    data:lv_document   type ref to if_ixml_document.
    data:element       type ref to if_ixml_element.
    data:lt_key_value  type type_t_key_value.
    data:ls_key_value  type type_s_key_value.

    me->convert_xlm_to_document( exporting content = response importing parser = lv_parser document = lv_document ).

    clear:element.
    call method lv_document->find_from_path
      exporting
        path = '/soapenv:Envelope/soapenv:Body/ns:serverErpXmlResponse/ns:return'
      receiving
        rval = element.

    me->transformation( exporting element = element importing key_value = lt_key_value ).

    read table lt_key_value into ls_key_value with key fname = 'return'.
    if sy-subrc eq 0.
      response = ls_key_value-fvalue.
    endif.

    error = lv_err_text.

  endmethod.


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
