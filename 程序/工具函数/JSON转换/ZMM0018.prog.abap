FUNCTION ZMM_0018.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_WAIT) TYPE  I DEFAULT '0.5'
*"  EXPORTING
*"     REFERENCE(EX_MSG) TYPE  CHAR255
*"  TABLES
*"      T_INPUT STRUCTURE  ZMMS0055
*"----------------------------------------------------------------------
  DATA: lx    TYPE char1  VALUE '['.
  DATA: rx    TYPE char1  VALUE ']'.
  DATA: lxx   TYPE char1  VALUE '{'.
  DATA: rxx   TYPE char1  VALUE '}'.
  DATA: yy    TYPE char1  VALUE '"'.
  DATA: d     TYPE char1  VALUE ','.
  DATA: dd    TYPE char1  VALUE ':'.
  DATA: strx          TYPE string.
  DATA: strxx         TYPE string.
  DATA: strxxn        TYPE string.
  DATA: strxxx        TYPE string.
  DATA: strxxxn       TYPE string.
  DATA: lv_sumxx      TYPE i.
  DATA: lv_dmbtr  TYPE c LENGTH 15.
  DATA: lv_dmbtr1 TYPE c LENGTH 15.
  DATA: lv_dmbtr2 TYPE c LENGTH 15.
  DATA: lv_dmbtr3 TYPE c LENGTH 15.
  DATA: lv_erfmg  TYPE c LENGTH 16.
  DATA: lv_erfmg1 TYPE c LENGTH 16.
  DATA: lv_erfmg2 TYPE c LENGTH 16.
  DATA: lv_erfmg3 TYPE c LENGTH 16.
  DATA: ls_zesb        TYPE zesb.
  data lv_matkx TYPE c LENGTH 60.
*  DATA: lt_zmmt0060 TYPE STANDARD TABLE OF zmmt0060.
*  DATA:ls_zmmt0060 TYPE zmmt0060.
  DATA ls_mseg LIKE LINE OF T_INPUT.
  WAIT UP TO i_wait SECONDS.

  LOOP AT T_INPUT INTO  ls_mseg.
    CLEAR lv_matkx.
    lv_matkx = ls_mseg-maktx.
    REPLACE ALL OCCURRENCES OF '\' IN lv_matkx WITH '\\'.
    REPLACE ALL OCCURRENCES OF '"' IN lv_matkx WITH '\"'.
    CLEAR: lv_dmbtr,lv_erfmg.
    lv_dmbtr  = ls_mseg-WRBTR." - ls_mseg-bnbtr.
    lv_dmbtr1 = ls_mseg-WRBTR1." - ls_mseg-bnbtr.
    lv_dmbtr2 = ls_mseg-WRBTR2." - ls_mseg-bnbtr.
    lv_dmbtr3 = ls_mseg-WRBTR3." - ls_mseg-bnbtr.
    lv_erfmg  = ls_mseg-MENGE.
    lv_erfmg1 = ls_mseg-MENGE1.
    lv_erfmg2 = ls_mseg-MENGE2.
    lv_erfmg3 = ls_mseg-MENGE3.
    CONDENSE:lv_dmbtr,lv_erfmg.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = ls_mseg-erfme
        language       = sy-langu
      IMPORTING
*       LONG_TEXT      =
        output         = ls_mseg-erfme
*       SHORT_TEXT     =
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      CLEAR ls_mseg-erfme.
    ENDIF.
    lv_sumxx = lv_sumxx + 1.
*第二二二二二二二二二二层LOOP
    CONCATENATE lxx
                yy 'BELNR' yy dd yy ls_mseg-BELNR yy d
                yy 'BUZEI' yy dd yy ls_mseg-BUZEI yy d
                yy 'CPUDT' yy dd yy ls_mseg-CPUDT yy d
                yy 'CPUTM' yy dd yy ls_mseg-CPUTM yy d
                yy 'BWART' yy dd yy ls_mseg-BWART yy d
                yy 'MATNR' yy dd yy ls_mseg-MATNR yy d
                yy 'MAKTX' yy dd yy lv_matkx yy d
                yy 'WERKS' yy dd yy ls_mseg-WERKS yy d
                yy 'LGORT' yy dd yy ls_mseg-LGORT yy d
                yy 'CHARG' yy dd yy ls_mseg-CHARG yy d
                yy 'SOBKZ' yy dd yy ls_mseg-SOBKZ yy d
                yy 'WAERS' yy dd yy ls_mseg-WAERS yy d
                yy 'MENGE' yy dd yy lv_erfmg      yy d
                yy 'MENGE1' yy dd yy lv_erfmg1    yy d
                yy 'MENGE2' yy dd yy lv_erfmg2    yy d
                yy 'MENGE3' yy dd yy lv_erfmg3    yy d
                yy 'ERFME' yy dd yy ls_mseg-ERFME yy d
                yy 'WRBTR' yy dd yy lv_dmbtr      yy d
                yy 'WRBTR1' yy dd yy lv_dmbtr1    yy d
                yy 'WRBTR2' yy dd yy lv_dmbtr2    yy d
                yy 'WRBTR3' yy dd yy lv_dmbtr3    yy d
                yy 'SHKZG' yy dd yy ls_mseg-SHKZG yy d
                yy 'EBELN' yy dd yy ls_mseg-EBELN yy d
                yy 'EBELP' yy dd yy ls_mseg-EBELP yy d
                yy 'LFBNR' yy dd yy ls_mseg-LFBNR yy d
                yy 'LFPOS' yy dd yy ls_mseg-LFPOS yy d
                yy 'VBELN_IM' yy dd yy ls_mseg-VBELN_IM yy d
                yy 'VBELP_IM' yy dd yy ls_mseg-VBELP_IM yy d
                yy 'USNAM_MKPF' yy dd yy ls_mseg-USNAM_MKPF yy

*                    yy 'MBLNR' yy dd yy ls_mseg-mblnr yy d
*                    yy 'ZEILE' yy dd yy ls_mseg-zeile yy d
*                    yy 'CPUDT_MKPF' yy dd yy ls_mseg-cpudt_mkpf yy d
*                    yy 'CPUTM_MKPF' yy dd yy ls_mseg-cputm_mkpf yy d
*                    yy 'BWART' yy dd yy ls_mseg-bwart yy d
*                    yy 'MATNR' yy dd yy ls_mseg-matnr yy d
*                    yy 'MAKTX' yy dd yy lv_matkx yy d
*                    yy 'WERKS' yy dd yy ls_mseg-werks yy d
*                    yy 'LGORT' yy dd yy ls_mseg-lgort yy d
*                    yy 'CHARG' yy dd yy ls_mseg-charg yy d
*                    yy 'SOBKZ' yy dd yy ls_mseg-sobkz yy d
*                    yy 'WAERS' yy dd yy ls_mseg-waers yy d
*                    yy 'DMBTR' yy dd yy lv_dmbtr yy d
*                    yy 'ERFMG' yy dd yy lv_erfmg yy d
*                    yy 'ERFME' yy dd yy ls_mseg-erfme yy d
*                    yy 'SHKZG' yy dd yy ls_mseg-shkzg yy d
*                    yy 'EBELN' yy dd yy ls_mseg-ebeln yy d
*                    yy 'EBELP' yy dd yy ls_mseg-ebelp yy d
*                    yy 'LFBNR' yy dd yy ls_mseg-lfbnr yy d
*                    yy 'LFPOS' yy dd yy ls_mseg-lfpos yy d
*                    yy 'VBELN_IM' yy dd yy ls_mseg-vbeln_im yy d
*                    yy 'VBELP_IM' yy dd yy ls_mseg-vbelp_im yy d
*                    yy 'USNAM_MKPF' yy dd yy ls_mseg-usnam_mkpf yy
                rxx
                    INTO strxxn.
    IF lv_sumxx = 1.
      strxx = strxxn.
    ELSEIF lv_sumxx > 1.
      CONCATENATE strxx d strxxn INTO strxx.              "如果超过第一次循环，就加逗号,带n的新数据，出入到后面。
    ENDIF.
*第二二二二二二二二二二层LOOP
  ENDLOOP.

  "如果循环超过一次，就加括号。
  IF lv_sumxx > 0.
    CONCATENATE lx strxx rx INTO strxx.
  ENDIF.
  CLEAR:lv_sumxx.
  "指定下发系统
  ls_zesb-vc = 'X'.

*第一一一一一一一一一一层LOOP
  CONCATENATE lxx
                  yy 'ZVBELN' yy dd yy ls_mseg-zvbeln yy d
                  yy 'CPUDT' yy dd yy ls_mseg-cpudt yy d
                  yy 'CPUTM' yy dd yy ls_mseg-cputm yy d
                  yy 'CRM' yy dd yy ls_zesb-crm yy d
                  yy 'OMS' yy dd yy ls_zesb-oms yy d
                  yy 'BI' yy dd yy ls_zesb-bi yy d
                  yy 'ESP' yy dd yy ls_zesb-esp yy d
                  yy 'GW' yy dd yy ls_zesb-gw yy d
                  yy 'EVM' yy dd yy ls_zesb-evm yy d
                  yy 'WX' yy dd yy ls_zesb-wx yy d
                  yy 'WMS' yy dd yy ls_zesb-wms yy d
                  yy 'TMS' yy dd yy ls_zesb-tms yy d
                  yy 'DX' yy dd yy ls_zesb-dx yy d
                  yy 'VC' yy dd yy ls_zesb-vc yy d
                  yy 'ITEM' yy dd
                  strxx
                  rxx
                  INTO strx.
  CLEAR:strxx.
*第一一一一一一一一一一层LOOP
  DATA: http_client TYPE REF TO if_http_client .
  DATA: l_url TYPE string .
  DATA: return_str TYPE string .

  " 获取接口地址
  DATA: lv_mes TYPE char100.
  CALL FUNCTION 'ZFM_GET_URL'
    EXPORTING
      im_mandt = sy-mandt
      im_zifid = 'ZMM_0018'
    IMPORTING
      ex_zurl  = l_url
      ex_mes   = lv_mes.

  CHECK l_url IS NOT INITIAL.
  cl_http_client=>create_by_url(
           EXPORTING url    = l_url
           IMPORTING client = http_client ).

  http_client->request->set_header_field(
          EXPORTING
               name  = '~server_protocol'
               value = 'HTTP/1.1' ).

  http_client->request->set_header_field(
    EXPORTING
      name  = 'Content-Type'
      value = 'application/json;charset=utf-8' ) . "text/xml; charset=utf-8

  http_client->request->set_header_field(
        EXPORTING
             name  = 'Accept'
             value = '*/*' ).

  http_client->request->set_method( 'POST' ).

  http_client->request->set_cdata(
    EXPORTING
      data = strx ).

  http_client->send(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2 ).

  http_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3  ).

  return_str = http_client->response->get_cdata( ).

  http_client->close( ).
  ex_msg = return_str.





ENDFUNCTION.