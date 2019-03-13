FUNCTION ZSTRANS_XML_TO_TABLE_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(XML_STRING) TYPE  STRING
*"  TABLES
*"      OT_NODE TYPE  TABLE
*"--------------------------------------------------------------------

  DATA ixml TYPE REF TO if_ixml.
  DATA document TYPE REF TO if_ixml_document.
  DATA streamfactory TYPE REF TO if_ixml_stream_factory.
  DATA istream TYPE REF TO if_ixml_istream.
  DATA parser TYPE REF TO if_ixml_parser.
  DATA node TYPE REF TO if_ixml_node.

  DATA temp_string TYPE string.

  CHECK xml_string IS NOT INITIAL.

  REFRESH lt_node.

  ixml = cl_ixml=>create( ).

  document = ixml->create_document( ).

  streamfactory = ixml->create_stream_factory( ).

  istream = streamfactory->create_istream_string( string = xml_string ).

  parser = ixml->create_parser( stream_factory = streamfactory
                                istream        = istream
                                document       = document ).

  parser->parse( ).

  CALL METHOD istream->close( ).

  CLEAR istream.

  node = document.

  PERFORM get_xml_data USING node 0.

  ot_node[] = lt_node[].

ENDFUNCTION.

*  test data
*  temp_string = '<?xml version="1.0" encoding="GBK" ?><Result><Account>11014627730000' &&
*  '</Account><CcyCode>RMB</CcyCode><CcyType></CcyType><AccountName>通灵珠宝股份有限公司</AccountName>' &&
*  '<Balance>24121521.56</Balance><TotalAmount>24121521.56</TotalAmount><AccountType></AccountType>' &&
*  '<AccountStatus>A</AccountStatus><BankName></BankName><LastBalance>41.87</LastBalance>' &&
*  '<HoldBalance>0.00</HoldBalance></Result>' .
