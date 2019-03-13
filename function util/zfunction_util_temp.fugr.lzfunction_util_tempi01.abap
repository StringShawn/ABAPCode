*----------------------------------------------------------------------*
***INCLUDE LZFUNCTION_UTILI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.

  CASE ok_code.
    WHEN 'CONF'.

      DATA text_string TYPE string.

      CALL METHOD edit_text->get_textstream
        IMPORTING
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

      ls_edit_text-answer = 'J'.

      LEAVE TO SCREEN 0.

    WHEN ''.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101_exit INPUT.

  LEAVE TO SCREEN 0.

  ls_edit_text-answer = 'X'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102_exit INPUT.

  CLEAR ls_list_text-list_value.
  LEAVE TO SCREEN 0.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LIST_ACTION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list_action INPUT.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = ls_list_text-list_field
      value_org       = 'S'
    TABLES
      value_tab       = <list_tab>
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.

  CASE ok_code.
    WHEN 'CONF'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
