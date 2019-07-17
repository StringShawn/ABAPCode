REPORT  zexprot_excel.

*&---------------------------------------------------------------------*
*&      Types and Data
*&---------------------------------------------------------------------*
TABLES:varid.

CONSTANTS:
  gc_tab  TYPE c VALUE cl_bcs_convert=>gc_tab,
  gc_crlf TYPE c VALUE cl_bcs_convert=>gc_crlf.

TYPES : BEGIN OF ty_fin ,
          line TYPE string,
        END OF ty_fin .

DATA : g_email         TYPE char200 .
DATA : ascilines(4096) TYPE c OCCURS 0 WITH HEADER LINE.
DATA : list            TYPE TABLE OF abaplist WITH HEADER LINE.
DATA : i_final         TYPE TABLE OF ty_fin .

*&---------------------------------------------------------------------*
*&      Selection Screen
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME .
PARAMETERS : p_report TYPE trdir-name    OBLIGATORY,
             p_vari   TYPE rsvar-variant OBLIGATORY.
SELECTION-SCREEN SKIP 1 .

PARAMETER : p_trim TYPE c AS  CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN SKIP 1 .

SELECT-OPTIONS : s_to FOR g_email NO INTERVALS OBLIGATORY,
                 s_cc FOR g_email NO INTERVALS .
SELECTION-SCREEN END OF BLOCK b1 .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM frm_show_vairous.

*&---------------------------------------------------------------------*
*&      Start of Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION .
  PERFORM execute_report .
  PERFORM process_output .
  PERFORM send_email .

*&---------------------------------------------------------------------*
*&      Form  execute_report
*&---------------------------------------------------------------------*
FORM execute_report .
* Call report and export output in memory
  SUBMIT (p_report)
  USING SELECTION-SET p_vari
         LINE-SIZE sy-linsz
        EXPORTING LIST TO MEMORY AND RETURN.
ENDFORM.                    "execute_report


*&---------------------------------------------------------------------*
*&      Form  process_output
*&---------------------------------------------------------------------*
FORM process_output .

  TYPES : BEGIN OF ty_split ,
            token TYPE char50,
          END OF ty_split .

  DATA : li_split TYPE TABLE OF ty_split,
         lv_str   TYPE string,
         ls_final TYPE ty_fin,
         lv_token TYPE char50.

  FIELD-SYMBOLS <fs_slip> TYPE ty_split .

* Get report output from memory
  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      listobject = list
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

* Convert it to ascii
  CALL FUNCTION 'LIST_TO_ASCI'
    TABLES
      listobject         = list
      listasci           = ascilines
    EXCEPTIONS
      empty_list         = 1
      list_index_invalid = 2
      OTHERS             = 3.

* Convert ascii to csv file
  LOOP AT ascilines .
*   Skip separater lines
    CHECK ascilines+0(10) <> '----------' .
    CLEAR li_split .
    SPLIT ascilines AT '|' INTO TABLE li_split .

    CLEAR lv_str .
    LOOP AT li_split ASSIGNING <fs_slip> .

      CLEAR lv_token .
      lv_token = <fs_slip>-token .

*     Post processing
      IF p_trim = abap_true .
        CONDENSE lv_token .
      ENDIF .

*      IF p_neg = abap_true .
*        CONDENSE lv_token .
*        TRY.
*            FIND REGEX '(\d+(\,\d+)?)+(\.\d+)?-' IN lv_token .
*            IF sy-subrc = 0 .
*              REPLACE '-' IN lv_token WITH space .
*              CONCATENATE '-' lv_token INTO lv_token .
*            ENDIF.
*          CATCH cx_root .
*            MESSAGE 'Error in regular expression' TYPE 'A'.
*        ENDTRY .
*      ENDIF.

      CASE sy-tabix.
        WHEN 1 .
        WHEN 2.
          lv_str = lv_token.
        WHEN OTHERS.
          CONCATENATE lv_str lv_token INTO lv_str SEPARATED BY gc_tab.
      ENDCASE.
    ENDLOOP .
    CLEAR ls_final .
    ls_final-line = lv_str   .
    APPEND ls_final TO i_final .
  ENDLOOP.
ENDFORM.                    "process_output


*&---------------------------------------------------------------------*
*&      Form  send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_email .

  DATA lv_string TYPE string.
  DATA : ls_final TYPE ty_fin .

  DATA main_text      TYPE bcsy_text.
  DATA binary_content TYPE solix_tab.
  DATA size           TYPE so_obj_len.
  DATA : email      TYPE adr6-smtp_addr.

  DATA send_request   TYPE REF TO cl_bcs.
  DATA document       TYPE REF TO cl_document_bcs.
  DATA recipient      TYPE REF TO if_recipient_bcs.
  DATA bcs_exception  TYPE REF TO cx_bcs.
  DATA sent_to_all    TYPE os_boolean.

  DATA : lv_subject TYPE so_obj_des .


  LOOP AT i_final INTO ls_final .
    CASE sy-tabix.
      WHEN 1.
        CONCATENATE ls_final-line gc_crlf INTO lv_string .
      WHEN OTHERS.
        CONCATENATE lv_string ls_final-line gc_crlf INTO lv_string .
    ENDCASE.
  ENDLOOP.

* --------------------------------------------------------------
* convert the text string into UTF-16LE binary data including
* byte-order-mark. Mircosoft Excel prefers these settings
* all this is done by new class cl_bcs_convert (see note 1151257)

  TRY.
      cl_bcs_convert=>string_to_solix(
        EXPORTING
          iv_string   = lv_string
          iv_codepage = '4103'  "suitable for MS Excel, leave empty
          iv_add_bom  = 'X'     "for other doc types
        IMPORTING
          et_solix  = binary_content
          ev_size   = size ).
    CATCH cx_bcs.
      MESSAGE e445(so).
  ENDTRY.


  TRY.

*     -------- create persistent send request ------------------------
      send_request = cl_bcs=>create_persistent( ).

*     -------- create and set document with attachment ---------------
*     create document object from internal table with text

      CONCATENATE p_report p_vari sy-datum sy-uzeit INTO lv_subject SEPARATED BY space.

      PERFORM create_body_of_email CHANGING main_text .

*     APPEND 'Email from SAP background Job' TO main_text.  "#EC NOTEXT
      document = cl_document_bcs=>create_document(
        i_type    = 'HTM'
        i_text    = main_text
        i_subject = lv_subject ).                           "#EC NOTEXT

      DATA : lv_name TYPE sood-objdes .

      lv_name = p_report .
*     add the spread sheet as attachment to document object
      document->add_attachment(
        i_attachment_type    = 'xls'                        "#EC NOTEXT
        i_attachment_subject = lv_name                      "#EC NOTEXT
        i_attachment_size    = size
        i_att_content_hex    = binary_content ).

*     add document object to send request
      send_request->set_document( document ).

*      DATA : l_receipient_soos TYPE soos1.

*     --------- add recipient (e-mail address) -----------------------
      LOOP AT s_to .
*       create recipient object
        email = s_to-low .
        recipient = cl_cam_address_bcs=>create_internet_address( email ).
*       add recipient object to send request
        send_request->add_recipient( recipient ).
      ENDLOOP .

      CALL METHOD send_request->set_status_attributes
        EXPORTING
          i_requested_status = 'E'.

      LOOP AT s_cc .
*       create recipient object
        email = s_cc-low .
        recipient = cl_cam_address_bcs=>create_internet_address( email ).

*       add recipient object to send request
        send_request->add_recipient( EXPORTING i_recipient = recipient i_copy = abap_true ).
      ENDLOOP .


*     ---------- send document ---------------------------------------
      send_request->set_send_immediately( i_send_immediately = 'X' ).
      sent_to_all = send_request->send( i_with_error_screen = 'X' ).

      COMMIT WORK.

      IF sent_to_all IS INITIAL.
        MESSAGE i500(sbcoms) WITH s_to-low.
      ELSE.
        MESSAGE s022(so).
      ENDIF.

*   ------------ exception handling ----------------------------------
*   replace this rudimentary exception handling with your own one !!!
    CATCH cx_bcs INTO bcs_exception.
      MESSAGE i865(so) WITH bcs_exception->error_type.
  ENDTRY.



ENDFORM .                    "send_email

*&---------------------------------------------------------------------*
*&      Form  CREATE_BODY_OF_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_MAIN_TEXT  text
*----------------------------------------------------------------------*
FORM create_body_of_email  CHANGING body_html TYPE bcsy_text.

  DATA : ls_line TYPE so_text255 .

  APPEND '<html>' TO body_html .
  APPEND '<title>Email</title>' TO body_html .
  APPEND '<body>' TO body_html  .
  APPEND '<p>Data attached</p>' TO body_html .
  APPEND '</body>' TO body_html .
  APPEND '</html>' TO body_html .

ENDFORM.                    " CREATE_BODY_OF_EMAIL
*&---------------------------------------------------------------------*
*& Form FRM_SHOW_VAIROUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_show_vairous .

  DATA: BEGIN OF variant_hlp_tbl OCCURS 20.
      INCLUDE STRUCTURE btcvarhtbl.
  DATA: END OF variant_hlp_tbl.

  DATA: selected_variant LIKE tbtcp-variant.

  DATA: ttl_lines TYPE i.

  DATA: scrnnr1 LIKE sy-dynnr.

  DATA: entered_progname LIKE btch4210-progname.

  DATA: fieldtbl LIKE dfies OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF dynpfields OCCURS 5.
      INCLUDE STRUCTURE dynpread.
  DATA: END OF dynpfields.

  CONSTANTS: var_nodisplay TYPE btcoptions-btcoption
                                VALUE 'VAR_NODISPLAY'.
  DATA: show_no_display_variants TYPE btcchar1.
  DATA: var_options TYPE TABLE OF btcoptions.
  DATA: wa_varoptions TYPE btcoptions.

  FREE variant_hlp_tbl.
  REFRESH dynpfields.

  dynpfields-fieldname = 'P_REPORT'.
  APPEND dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-repid
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      OTHERS               = 99 ##FM_SUBRC_OK.

  LOOP AT dynpfields.
    entered_progname = dynpfields-fieldvalue.
  ENDLOOP.

  IF entered_progname EQ space.
    MESSAGE s079(bt).
    EXIT.
  ENDIF.

* hgk   11.12.2000
* make sure, that system variants are also displayed in
* clients other than 000

  show_no_display_variants = 'X'.

  CALL FUNCTION 'BTC_OPTION_GET'
    EXPORTING
      name         = var_nodisplay
*     IMPVALUE1    =
*     IMPVALUE2    =
*   IMPORTING
*     COUNT        =
    TABLES
      options      = var_options
    EXCEPTIONS
      invalid_name = 1
      OTHERS       = 2.
  IF sy-subrc = 0.
    READ TABLE var_options INDEX 1 INTO wa_varoptions.
    IF wa_varoptions-value1 IS NOT INITIAL.
      CLEAR show_no_display_variants.
    ENDIF.
  ENDIF.


  SELECT * FROM varid WHERE report EQ entered_progname.
    IF show_no_display_variants IS INITIAL
    AND ( varid-transport = 'X' OR varid-transport = 'N' ).
      CONTINUE.
    ENDIF.
    variant_hlp_tbl-variant = varid-variant.
    APPEND variant_hlp_tbl.
  ENDSELECT.

  SELECT * FROM varid CLIENT SPECIFIED WHERE mandt = '000'
             AND report EQ entered_progname.
    IF show_no_display_variants IS INITIAL
    AND ( varid-transport = 'X' OR varid-transport = 'N' ).
      CONTINUE.
    ENDIF.
    IF varid-variant(4) = 'SAP&' OR
                     varid-variant(4) = 'CUS&'.
      variant_hlp_tbl-variant = varid-variant.
      APPEND variant_hlp_tbl.
    ENDIF.
  ENDSELECT.


  DESCRIBE TABLE variant_hlp_tbl LINES ttl_lines.

* fix for 46D - show message when there is no variant for that report
  IF ttl_lines EQ 0.
    DATA: report_needs_variant(3).
    CALL FUNCTION 'RS_SELSCREEN_EXISTS'
      EXPORTING
        report = entered_progname
      IMPORTING
        answer = report_needs_variant
      EXCEPTIONS
        OTHERS = 99.
    IF sy-subrc EQ 0.
      IF report_needs_variant EQ 'YES'.
        MESSAGE s076(bt) WITH entered_progname.
      ELSE.
        MESSAGE s657(bt) WITH entered_progname.
      ENDIF.
    ELSE.
* must be a problem in the report
      MESSAGE s074(bt) WITH entered_progname.
    ENDIF.
  ENDIF.

  SORT variant_hlp_tbl BY variant ASCENDING.

  DATA: scrnfield_var TYPE help_info-dynprofld.

  scrnfield_var = 'P_VARI'.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'P_VARI'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
     dynprofield     = scrnfield_var
      value_org       = 'S'
    TABLES
      value_tab       = variant_hlp_tbl
      field_tab       = fieldtbl
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3 ##FM_SUBRC_OK.
  " SHOW_VARIANTS  INPUT
ENDFORM.