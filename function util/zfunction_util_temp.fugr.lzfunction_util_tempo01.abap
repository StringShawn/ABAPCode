*----------------------------------------------------------------------*
***INCLUDE LZFUNCTION_UTILO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

  IF ls_edit_text-readonly = 0.
    SET PF-STATUS '0101'.
  ELSE.
    DATA fcode TYPE TABLE OF sy-ucomm.
    APPEND 'CONF' TO fcode.
    SET PF-STATUS '0101' EXCLUDING fcode.
  ENDIF.

  SET TITLEBAR '0101' WITH ls_edit_text-toolbar_txt.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INIT_EDIT_VIEW  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_edit_view OUTPUT.

  IF edit_container IS INITIAL.
    CREATE OBJECT edit_container
      EXPORTING
        container_name              = 'EDIT_CON'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  IF edit_text IS INITIAL.
    CREATE OBJECT edit_text EXPORTING parent = edit_container.
  ENDIF.

  CALL METHOD edit_text->set_toolbar_mode
    EXPORTING
      toolbar_mode = 0.

  CALL METHOD edit_text->set_statusbar_mode
    EXPORTING
      statusbar_mode = 0.

  CALL METHOD edit_text->set_readonly_mode( readonly_mode = ls_edit_text-readonly ).

  CALL METHOD edit_text->set_textstream
    EXPORTING
      text                   = ls_edit_text-text_string
    EXCEPTIONS
      error_cntl_call_method = 1
      not_supported_by_gui   = 2
      OTHERS                 = 3.

  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2
      OTHERS            = 3.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0102 OUTPUT.

  SET PF-STATUS '0102'.

  SET TITLEBAR '0101' WITH ls_list_text-toolbar_txt.

ENDMODULE.
