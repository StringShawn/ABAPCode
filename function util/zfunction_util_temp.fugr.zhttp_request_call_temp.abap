FUNCTION ZHTTP_REQUEST_CALL_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(METHOD) TYPE  STRING
*"     REFERENCE(URL) TYPE  STRING
*"     REFERENCE(CONTENT_TYPE) TYPE  STRING OPTIONAL
*"     REFERENCE(HEAD_FIELDS) TYPE  TIHTTPNVP OPTIONAL
*"     REFERENCE(FORM_FIELDS) TYPE  TIHTTPNVP OPTIONAL
*"     REFERENCE(FORM_STRING) TYPE  STRING OPTIONAL
*"     REFERENCE(FORM_XSTRING) TYPE  XSTRING OPTIONAL
*"     REFERENCE(TIMEOUT) TYPE  I OPTIONAL
*"  EXPORTING
*"     REFERENCE(RETURN_STRING) TYPE  STRING
*"  EXCEPTIONS
*"      IMPORT_ELEMENT_ERROR
*"      CREATE_URL_ERROR
*"      SEND_DATA_ERROR
*"      RECEIVE_DATA_ERROR
*"      CLOSE_HTTP_ERROR
*"--------------------------------------------------------------------

  DATA http_client TYPE REF TO if_http_client.
  DATA return_xstring TYPE xstring.
  DATA temp_url TYPE string.

  IF method = 'GET' AND form_string IS INITIAL.
    RAISE import_element_error.
  ELSEIF method = 'POST' AND form_xstring IS INITIAL AND form_string IS INITIAL AND form_fields IS INITIAL.
    RAISE import_element_error.
  ELSEIF method <> 'GET' AND method <> 'POST'.
    RAISE import_element_error.
  ENDIF.

  IF method = 'GET'.
    temp_url = url && form_string.
  ELSE.
    temp_url = url.
  ENDIF.

  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = temp_url
    IMPORTING
      client             = http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.

  IF sy-subrc <> 0.
    RAISE create_url_error.
  ENDIF.

  http_client->request->set_method( method ).

  IF head_fields[] IS NOT INITIAL.
    http_client->request->set_header_fields(  fields = head_fields[] ).
  ENDIF.

  IF method = 'POST' AND content_type IS NOT INITIAL.
    http_client->request->set_content_type( content_type ).
  ENDIF.

  IF method = 'POST' AND ( form_string IS NOT INITIAL OR form_xstring IS NOT INITIAL ).

    IF form_string IS NOT INITIAL.
      DATA(form_length) = strlen( form_string ).

      CALL METHOD http_client->request->set_cdata
        EXPORTING
          data   = form_string
          length = form_length.

    ELSEIF form_xstring IS NOT INITIAL.
      DATA(form_xlength) = xstrlen( form_xstring ).

      CALL METHOD http_client->request->set_data
        EXPORTING
          data   = form_xstring
          length = form_xlength.
    ENDIF.

  ELSEIF method = 'POST' AND form_fields[] IS NOT INITIAL.

    CALL METHOD http_client->request->set_form_fields
      EXPORTING
        fields = form_fields[].
  ENDIF.

  CALL METHOD http_client->send
    EXPORTING
      timeout                    = timeout
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4
      OTHERS                     = 5.

  IF sy-subrc <> 0.
    RAISE send_data_error.
  ENDIF.

  CALL METHOD http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.

  IF sy-subrc <> 0.
    RAISE receive_data_error.
  ENDIF.

  return_xstring = http_client->response->get_data( ).

  CALL METHOD http_client->close
    EXCEPTIONS
      http_invalid_state = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    RAISE close_http_error.
  ENDIF.

  DATA lr_conv TYPE REF TO cl_abap_conv_in_ce.

  CALL METHOD cl_abap_conv_in_ce=>create
    EXPORTING
      input       = return_xstring
      encoding    = 'UTF-8' "8400
      replacement = '?'
      ignore_cerr = abap_true
    RECEIVING
      conv        = lr_conv.

  TRY.
      CALL METHOD lr_conv->read
        IMPORTING
          data = return_string.

    CATCH cx_sy_conversion_codepage .
    CATCH cx_sy_codepage_converter_init .
    CATCH cx_parameter_invalid_type .
    CATCH cx_parameter_invalid_range .

  ENDTRY.

ENDFUNCTION.
