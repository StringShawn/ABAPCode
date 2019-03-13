*&---------------------------------------------------------------------*
*&  Include  ZEXCEL_MACRO
*&---------------------------------------------------------------------*
DATA excel TYPE ole2_object.
DATA workbooks TYPE ole2_object.
DATA workbook TYPE ole2_object.
DATA sheets TYPE ole2_object.
DATA sheet TYPE ole2_object.
DATA cell TYPE ole2_object.
DATA copy_range TYPE ole2_object.
DATA paste_range TYPE ole2_object.
DATA column TYPE ole2_object.

DATA temp_row TYPE i.
DATA temp_col TYPE i.


DEFINE open_excel.


  CREATE OBJECT excel 'Excel.Application'.

  CALL METHOD OF excel 'Workbooks' = workbooks.

  CALL METHOD OF workbooks
     'Open'  = workbook
    EXPORTING
      #1        = &1.

END-OF-DEFINITION.

DEFINE select_sheet.

  IF &1 IS INITIAL.

    CALL METHOD OF workbook
      'worksheets' = sheet
      EXPORTING
       #1        = 1."sheet_name
  ELSE.
    CALL METHOD OF workbook
     'worksheets' = sheet
      EXPORTING
       #1        = &1."sheet_name
  ENDIF.

END-OF-DEFINITION.

DEFINE change_column_width.

  "&1-A,B,C
  "&2-width

  IF &2 > 0.
    CALL METHOD of sheet
      'COLUMNS' = column
      EXPORTING
        #1 = &1.

    SET PROPERTY OF column 'columnwidth' = &2.
  ENDIF.
END-OF-DEFINITION.

DEFINE fill_cell.

  temp_row = &1.
  temp_col = &2.

  CALL METHOD OF sheet
   'CELLS' = cell
   EXPORTING
   #1      = temp_row
   #2      = temp_col.

  SET PROPERTY OF cell 'VALUE' = &3.

END-OF-DEFINITION.

DEFINE fill_cell_with_tab.

  FIELD-SYMBOLS:<tab> TYPE STANDARD TABLE.
  FIELD-SYMBOLS:<value> TYPE any.

  DATA tabname TYPE char30.

  tabname = &1 && '[]'.
  ASSIGN (tabname) TO <tab>.
  LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<ls_tab>) .
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <ls_tab> to <value>.
      IF <value> is ASSIGNED.
        CASE sy-index.
          WHEN 1.
            temp_row = <value>.
          WHEN 2.
            temp_col = <value>.
          WHEN 3.
            CALL METHOD OF sheet
              'CELLS' = cell
            EXPORTING
              #1 = temp_row
              #2 = temp_col.

            SET PROPERTY OF cell 'value' = <value>.
            EXIT.
          WHEN OTHERS.
        ENDCASE.
      ELSE.
        EXIT.
      ENDIF.

    ENDDO.

  ENDLOOP.

END-OF-DEFINITION.

DEFINE add_sheet.

  CALL METHOD OF workbook
    'add' = sheet.

END-OF-DEFINITION.

DEFINE delete_sheet.

  CALL METHOD of sheet 'delete'.
END-OF-DEFINITION.

DEFINE copy_sheet_area.

  "&1-fron sheet
  "&2-to sheet
  "&3-begin postion eg-A1
  "&4-end position eg-L21

  CALL METHOD OF workbook
      'sheets' = sheet
    EXPORTING
      #1 = &1.

  CALL METHOD OF sheet
    'Range' = copy_range
    EXPORTING
      #1 = &3
      #2 = &4.

  CALL METHOD OF copy_range 'copy'.

  CALL METHOD OF workbook
      'sheets' = sheet
    EXPORTING
      #1 = &2.

  CALL METHOD OF sheet
    'Range' = paste_range
    EXPORTING
      #1 = &3
      #2 = &4.

  CALL METHOD OF paste_range 'pastespecial'.

END-OF-DEFINITION.

DEFINE rename_sheet.

  IF &1 is NOT INITIAL.
    SET PROPERTY OF sheet 'name ' = &1.
  ENDIF.
END-OF-DEFINITION.

DEFINE set_sheet_protect.

  "&1-password

  IF &1 IS INITIAL.
     CALL METHOD OF sheet 'protect'.
  ELSE.
     CALL METHOD OF sheet
       'protect' = sheet
      EXPORTING
        #1 = &1.
  ENDIF.

END-OF-DEFINITION.

DEFINE set_sheet_unrotect.

  "&1-password

  IF &1 IS INITIAL.
     CALL METHOD OF sheet 'unprotect'.
  ELSE.
     CALL METHOD OF sheet
       'unprotect' = sheet
      EXPORTING
        #1 = &1.
  ENDIF.

END-OF-DEFINITION.

DEFINE set_excel_autoopen.
  SET PROPERTY OF excel 'visible' = 1.
END-OF-DEFINITION.

DEFINE set_sheet_active.
  CALL METHOD OF sheet 'activate'.
END-OF-DEFINITION.

DEFINE save_excel.
  CALL METHOD OF workbook 'save'.
END-OF-DEFINITION.

DEFINE check_ole_error.

  CASE sy-subrc.
    WHEN 0.
    WHEN OTHERS.
       MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDCASE.
END-OF-DEFINITION.
