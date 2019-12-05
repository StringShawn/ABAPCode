*&---------------------------------------------------------------------*
*& Report Z_15405_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_15405_test.
*&---------------------------------------------------------------------*
*& Report  ZDEMO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------

TYPE-POOLS: ixml.

TYPES: BEGIN OF xml_line,
          data(255) TYPE x,
       END OF xml_line.

DATA:
      l_xml_table_forecast  TYPE TABLE OF xml_line,
      l_rc                  TYPE i,
      l_xml_size            TYPE i,
      wa_xml                TYPE xml_line,
      gs_solix              TYPE solix,
      binary_content_forecast TYPE solix_tab,
      sent_to_all   TYPE os_boolean,
      main_text     TYPE bcsy_text,
      send_request  TYPE REF TO cl_bcs,
      document      TYPE REF TO cl_document_bcs,
      recipient     TYPE REF TO if_recipient_bcs,
      bcs_exception TYPE REF TO cx_bcs,
      mailto        TYPE ad_smtpadr.

DATA: gt_sflight TYPE TABLE OF mara,
      gs_sflight TYPE mara.

CONSTANTS gc_mark VALUE 'X'.


START-OF-SELECTION.
  mailto = 'xuhang.ye@hand-china.com'.     "此处填入自己的邮箱
  PERFORM frm_get_flight.
  PERFORM frm_process_xml_data USING 1.
  PERFORM frm_send_email.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FLIGHT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_flight .
  SELECT * "UP TO 10 ROWS
    FROM mara
    INTO CORRESPONDING FIELDS OF TABLE gt_sflight.
*   WHERE carrid = s_carrid
*     AND connid = s_connid
*     AND fldate = s_fldate
    .

ENDFORM.                    " FRM_GET_FLIGHT
*&---------------------------------------------------------------------*
*&      Form  FRM_PROCESS_XML_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*----------------------------------------------------------------------*
FORM frm_process_xml_data  USING reltype TYPE i.
  DATA: l_ixml TYPE REF TO if_ixml,
        l_streamfactory TYPE REF TO if_ixml_stream_factory,
        l_ostream       TYPE REF TO if_ixml_ostream,
        l_renderer      TYPE REF TO if_ixml_renderer,
        l_document      TYPE REF TO if_ixml_document.
  DATA: l_element_root  TYPE REF TO if_ixml_element,
        r_element       TYPE REF TO if_ixml_element,
        r_worksheet     TYPE REF TO if_ixml_element,
        r_table         TYPE REF TO if_ixml_element,
        r_column        TYPE REF TO if_ixml_element,
        r_row           TYPE REF TO if_ixml_element,
        r_cell          TYPE REF TO if_ixml_element,
        r_data          TYPE REF TO if_ixml_element,
        l_value         TYPE string.
  FIELD-SYMBOLS:
        <ls_flight> TYPE mara.
*  create a ixml factory
  l_ixml = cl_ixml=>create( ).
*  create the DOM object model
  l_document = l_ixml->create_document( ).
*  create workbook
  PERFORM create_workbook USING l_document
                                r_worksheet
                                r_table.
*  column formatting
  CASE reltype.
    WHEN 1.
      PERFORM frm_forecast_column_format USING l_document r_table.
    WHEN 2.
    WHEN OTHERS.
  ENDCASE.
*  data table
  LOOP AT gt_sflight ASSIGNING <ls_flight>.
    r_row = l_document->create_simple_element( name = 'Row' parent = r_table ).
    r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
    l_value = <ls_flight>-mandt.
    r_data = l_document->create_simple_element( name = 'Data' value = l_value parent = r_cell ).
    r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).
    r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
    l_value = <ls_flight>-matnr.
    r_data = l_document->create_simple_element( name = 'Data' value = l_value parent = r_cell ).
    r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).
    r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
    l_value = <ls_flight>-ersda.
    r_data = l_document->create_simple_element( name = 'Data' value = l_value parent = r_cell ).
    r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).

  ENDLOOP.
*   creating a stream factory
  l_streamfactory = l_ixml->create_stream_factory( ).
*   connect internal xml table to stream factory
  CASE reltype.
    WHEN 1.
      l_ostream = l_streamfactory->create_ostream_itable( table = l_xml_table_forecast ).
    WHEN 2.
    WHEN OTHERS.
  ENDCASE.
*   rendering the document
  l_renderer = l_ixml->create_renderer( ostream = l_ostream document = l_document ).
  l_rc = l_renderer->render( ).
*   saving the xml document
  l_xml_size = l_ostream->get_num_written_raw( ).
ENDFORM.                    " FRM_PROCESS_XML_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_WORKBOOK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DOCUMENT  text
*      -->P_R_WORKSHEET  text
*      -->P_R_TABLE  text
*----------------------------------------------------------------------*
FORM create_workbook  USING    l_document TYPE REF TO if_ixml_document
                               r_worksheet TYPE REF TO if_ixml_element
                               r_table TYPE REF TO if_ixml_element.
  DATA: l_element_root        TYPE REF TO if_ixml_element,
        ns_attribute          TYPE REF TO if_ixml_attribute,
        r_element_properties  TYPE REF TO if_ixml_element,
        l_value               TYPE string.

*  create root node 'workbook'
  l_element_root = l_document->create_simple_element( name = 'Workbook' parent = l_document ).
  l_element_root->set_attribute( name = 'xmlns' value = 'urn:schemas-microsoft-com:office:spreadsheet' ).

  ns_attribute = l_document->create_namespace_decl( name = 'ss' prefix = 'xmlns'
                                        uri = 'urn:schemas-microsoft-com:office:spreadsheet' ).
  l_element_root->set_attribute_node( ns_attribute ).

  ns_attribute = l_document->create_namespace_decl( name = 'x' prefix = 'xmlns'
                                        uri = 'urn:schemas-microsoft-com:office:excel' ).
  l_element_root->set_attribute_node( ns_attribute ).

*  create node for document properties
  r_element_properties = l_document->create_simple_element( name = 'TEST_REPORT' parent = l_element_root ).
  l_value = sy-uname.
  l_document->create_simple_element( name = 'Author' value = l_value parent = r_element_properties ).

*  worksheet
  r_worksheet = l_document->create_simple_element( name = 'Worksheet' parent = l_element_root ).
  r_worksheet->set_attribute_ns( name = 'Name' prefix = 'ss' value = 'Sheet1' ).

*  table
  r_table = l_document->create_simple_element( name = 'Table' parent = r_worksheet ).
  r_table->set_attribute_ns( name = 'FullColumns' prefix = 'x' value = '1' ).
  r_table->set_attribute_ns( name = 'FullRows'    prefix = 'x' value = '1' ).

ENDFORM.                    " CREATE_WORKBOOK
*&---------------------------------------------------------------------*
*&      Form  FRM_FORECAST_COLUMN_FORMAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DOCUMENT  text
*      -->P_R_TABLE  text
*----------------------------------------------------------------------*
FORM frm_forecast_column_format  USING    l_document TYPE REF TO if_ixml_document
                                          r_table TYPE REF TO if_ixml_element.
  DATA: l_element_root TYPE REF TO if_ixml_element,
        r_column       TYPE REF TO if_ixml_element,
        r_row          TYPE REF TO if_ixml_element,
        r_cell         TYPE REF TO if_ixml_element,
        r_data         TYPE REF TO if_ixml_element,
        l_value        TYPE string.
  DATA: lv_short       TYPE string,
        lv_medium      TYPE string,
        lv_long        TYPE string,
        lv_exe_date    TYPE char10,
        lv_exe_time    TYPE char10,
        lv_exe_user    TYPE string,
        lt_month_names TYPE TABLE OF t247,
        ls_month_name  TYPE t247,
        lv_date_add    TYPE sy-datum,
        lv_count       TYPE i.
  lv_short = '60'.
  lv_medium = '90'.
  lv_long = '150'.
  WRITE sy-datum TO lv_exe_date.
  CONCATENATE sy-uzeit+0(2) ':' sy-uzeit+2(2) INTO lv_exe_time.
  lv_exe_user = sy-uname.
  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      language              = sy-langu
    TABLES
      month_names           = lt_month_names
    EXCEPTIONS
      month_names_not_found = 1
      OTHERS                = 2.
  IF sy-subrc = 0.
    READ TABLE lt_month_names WITH KEY mnr = sy-datum+4(2) INTO ls_month_name.
  ENDIF.
*  columns and width
*  line
  DO 1 TIMES.
    r_column = l_document->create_simple_element( name = 'Column' parent = r_table ).
    r_column->set_attribute_ns( name = 'Width' prefix = 'ss' value = lv_short ).
  ENDDO.
  DO 14 TIMES.
    r_column = l_document->create_simple_element( name = 'Column' parent = r_table ).
  ENDDO.
*  information row
  r_row = l_document->create_simple_element( name = 'Row' parent = r_table ).
  r_row->set_attribute_ns( name = 'AutoFitHeight' prefix = 'ss' value = '1' ).
*  type
  r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  l_value = 'SFLIGHT_FORECAST'.
  r_data = l_document->create_simple_element( name = 'Data' value = l_value parent = r_cell ).
  r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).
  DO 3 TIMES.
    r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  ENDDO.
*  name
  r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  l_value = sy-uname.
  r_data = l_document->create_simple_element( name = 'Data'  value = l_value   parent = r_cell ).           " Data
  r_data->set_attribute_ns( name = 'Type'  prefix = 'ss'  value = 'String' ).

*   Date
  r_cell = l_document->create_simple_element( name = 'Cell'  parent = r_row ).
  l_value = lv_exe_date.
  r_data = l_document->create_simple_element( name = 'Data'  value = l_value   parent = r_cell ).           " Data
  r_data->set_attribute_ns( name = 'Type'  prefix = 'ss'  value = 'String' ).
*   Time
  r_cell = l_document->create_simple_element( name = 'Cell'  parent = r_row ).
  l_value = lv_exe_time.
  r_data = l_document->create_simple_element( name = 'Data'  value = l_value   parent = r_cell ).           " Data
  r_data->set_attribute_ns( name = 'Type'  prefix = 'ss'  value = 'String' ).

*   Column Headers Row
  r_row = l_document->create_simple_element( name = 'Row'  parent = r_table ).
  r_row->set_attribute_ns( name = 'AutoFitHeight'  prefix = 'ss'  value = '1' ).

*  mandt
  r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  r_data = l_document->create_simple_element( name = 'Data' value = 'mandt' parent = r_cell ).
  r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).

*  carrid
  r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  r_data = l_document->create_simple_element( name = 'Data' value = 'carrid' parent = r_cell ).
  r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).

*  connid
  r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  r_data = l_document->create_simple_element( name = 'Data' value = 'connid' parent = r_cell ).
  r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).

*  fldate
  r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  r_data = l_document->create_simple_element( name = 'Data' value = 'fldate' parent = r_cell ).
  r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).

*  price
  r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  r_data = l_document->create_simple_element( name = 'Data' value = 'price' parent = r_cell ).
  r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).

*  currency
  r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  r_data = l_document->create_simple_element( name = 'Data' value = 'currency' parent = r_cell ).
  r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).

*  planetype
  r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  r_data = l_document->create_simple_element( name = 'Data' value = 'planetype' parent = r_cell ).
  r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).

*  seatsmax
  r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  r_data = l_document->create_simple_element( name = 'Data' value = 'seatsmax' parent = r_cell ).
  r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).

*  seatsocc
  r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  r_data = l_document->create_simple_element( name = 'Data' value = 'seatsocc' parent = r_cell ).
  r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).

*  paymentsum
  r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  r_data = l_document->create_simple_element( name = 'Data' value = 'paymentsum' parent = r_cell ).
  r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).

*  seatsmax_b
  r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  r_data = l_document->create_simple_element( name = 'Data' value = 'seatsmax_b' parent = r_cell ).
  r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).

*  seatsocc_b
  r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  r_data = l_document->create_simple_element( name = 'Data' value = 'seatsocc_b' parent = r_cell ).
  r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).

*  seatsmax_f
  r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  r_data = l_document->create_simple_element( name = 'Data' value = 'seatsmax_f' parent = r_cell ).
  r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).

*  seatsocc_f
  r_cell = l_document->create_simple_element( name = 'Cell' parent = r_row ).
  r_data = l_document->create_simple_element( name = 'Data' value = 'seatsocc_f' parent = r_cell ).
  r_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ).

ENDFORM.                    " FRM_FORECAST_COLUMN_FORMAT
*&---------------------------------------------------------------------*
*&      Form  FRM_SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_send_email .
  DATA: lc_xls_type TYPE so_obj_tp VALUE 'XLS',
        lc_codepage TYPE abap_encod VALUE '4103',
        lv_string   TYPE string,
        lv_size     TYPE so_obj_len,
        lc_add_attc TYPE so_obj_des VALUE 'popup',
        lt_binary_content TYPE solix_tab.
  LOOP AT l_xml_table_forecast INTO wa_xml.
    CLEAR gs_solix.
    gs_solix-line = wa_xml-data.
    APPEND gs_solix TO binary_content_forecast.
  ENDLOOP.
  TRY .
*    -------------create persistent sent request----------------
      send_request = cl_bcs=>create_persistent( ).
*    -------------create and set document with attachment-------
*    create document object from internal table with text
      APPEND 'Mail text!' TO main_text.
      document = cl_document_bcs=>create_document(
        i_type = 'HTM'
        i_text = main_text
        i_subject = 'Test created' ).
*    add the spread sheet as attachment to document object
      document->add_attachment(
        i_attachment_type = lc_xls_type
        i_attachment_subject = 'SpreadSheet'
        i_att_content_hex = binary_content_forecast ).
*    send document object to send request
      send_request->set_document( document ).
*    --------------add recipient (e-mail address)--------------
*    create recipient object
      recipient = cl_cam_address_bcs=>create_internet_address( mailto ).
*    add recipient object to send request
      send_request->add_recipient( recipient ).
*    --------------send document ------------------------------
      sent_to_all = send_request->send( i_with_error_screen = 'X' ).
      commit work.
      IF sent_to_all is INITIAL.
        MESSAGE i500(sbcoms) WITH mailto.
      else.
        MESSAGE s022(so).
      ENDIF.
*    ---------------exception handling ------------------------
    CATCH cx_bcs INTO bcs_exception.
      MESSAGE i865(so) WITH bcs_exception->error_type.
  ENDTRY.
ENDFORM.                    " FRM_SEND_EMAIL