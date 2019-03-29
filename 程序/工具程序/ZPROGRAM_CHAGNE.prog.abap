*&---------------------------------------------------------------------*
*& Report  ZPROGRAM_CHANGE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZPROGRAM_CHANGE.
DATA: BEGIN OF SRC OCCURS 1,
           TXT(255) TYPE C,
        END OF SRC.
PARAMETERS: REP LIKE TRDIR-NAME.

START-OF-SELECTION.
  READ REPORT REP INTO SRC.
  EDITOR-CALL FOR SRC.
  IF SY-SUBRC = 0.
    INSERT REPORT REP FROM SRC.
  ENDIF.