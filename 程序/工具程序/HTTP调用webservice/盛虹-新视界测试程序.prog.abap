REPORT ztest_15405.

TYPES: BEGIN OF t_xml_line,
         data(256) TYPE x,
       END OF t_xml_line.

DATA: l_xml_table      TYPE TABLE OF t_xml_line,
      l_xml_line       TYPE t_xml_line,
      l_xml_table_size TYPE i.
DATA: l_filename        TYPE string.

*PARAMETERS: pa_file TYPE char1024 DEFAULT 'C:\Users\Shawn\Desktop\123.xml' LOWER CASE.
*
*PERFORM get_xml_table CHANGING l_xml_table_size l_xml_table.



DATA:http_client          TYPE REF TO if_http_client.
DATA:lv_system_fault      TYPE REF TO cx_ai_system_fault.
DATA:lv_application_fault TYPE REF TO cx_ai_application_fault.
DATA:lv_err_text          TYPE string.
DATA request_str          TYPE string.
DATA:response             TYPE string.
cl_http_client=>create_by_url( EXPORTING url =
'http://172.30.18.22:50000/XISOAPAdapter/MessageServlet?senderParty=&senderService=SH_SAPECC&receiverParty=&receiverService=&interface=CREATEXSJCGDD_OUT_SYN&interfaceNamespace=urn:shenghong.SAPECC:OA'
                               IMPORTING client = http_client ).

request_str = '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:web="webservices.services.weaver.com.cn" xmlns:sap="http://sap.sh.interfaces.weaver">' &&
         '<soapenv:Header/>' &&
         '<soapenv:Body>' &&
         '<web:createxsjcgdd>' &&
         '<web:in0>6100</web:in0>' &&
         '<web:in1>4500000002</web:in1>' &&
         '<web:in2>NB</web:in2>' &&
         '<web:in3>0020013064</web:in3>' &&
         '<web:in4>北京国网普瑞特高压输电技术有限公司</web:in4>' &&
         '<web:in5/>' &&
         '<web:in6>2019-11-12</web:in6>' &&
         '<web:in7/>' &&
         '<web:in8/>' &&
         '<web:in9>' &&
           '<sap:XsjCgdd_mx>' &&
               '<sap:cgddsl>10.000</sap:cgddsl>' &&
               '<sap:cgjg>10.00</sap:cgjg>' &&
               '<sap:ddxmh>00010</sap:ddxmh>' &&
               '<sap:sldw>KG</sap:sldw>' &&
               '<sap:sm/>' &&
               '<sap:wlbh>00000000069000002</sap:wlbh>' &&
               '<sap:wlmc>成品-新视界测试</sap:wlmc>' &&
               '<sap:xmwb/>' &&
            '</sap:XsjCgdd_mx>' &&
         '</web:in9>' &&
      '</web:createxsjcgdd>' &&
               '</soapenv:Body>' &&
'</soapenv:Envelope>'.

http_client->authenticate(
username             = 'po_sender'
password             = 'posender0807' ).

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

BREAK-POINT.
**&--------------------------------------------------------------------*
**&      Form  get_xml_table
**&--------------------------------------------------------------------*
*FORM get_xml_table CHANGING l_xml_table_size TYPE i
*                            l_xml_table      TYPE STANDARD TABLE.
*
**   Local variable declaration
*  DATA: l_len     TYPE i,
*        l_len2    TYPE i,
*        l_tab     TYPE tsfixml,
*        l_content TYPE string,
*        l_str1    TYPE string,
*        c_conv    TYPE REF TO cl_abap_conv_in_ce,
*        l_itab    TYPE TABLE OF string.
*
*
*  l_filename = pa_file.
**   upload a file from the client's workstation
*  CALL METHOD cl_gui_frontend_services=>gui_upload
*    EXPORTING
*      filename   = l_filename
*      filetype   = 'BIN'
*    IMPORTING
*      filelength = l_xml_table_size
*    CHANGING
*      data_tab   = l_xml_table
*    EXCEPTIONS
*      OTHERS     = 19.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
**   Writing the XML document to the screen
*  CLEAR l_str1.
*  LOOP AT l_xml_table INTO l_xml_line.
*    c_conv = cl_abap_conv_in_ce=>create( input = l_xml_line-data replacement = space  ).
*    c_conv->read( IMPORTING data = l_content len = l_len ).
*    CONCATENATE l_str1 l_content INTO l_str1.
*  ENDLOOP.
*  l_str1 = l_str1+0(l_xml_table_size).
*  SPLIT l_str1 AT ' ' INTO TABLE l_itab.
*  LOOP AT l_itab INTO l_str1.
**    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN
**      l_str1 WITH space.
**    WRITE: / l_str1.
*    request_str = request_str && l_str1.
*  ENDLOOP.
*  WRITE: /.
*ENDFORM.                    "get_xml_table