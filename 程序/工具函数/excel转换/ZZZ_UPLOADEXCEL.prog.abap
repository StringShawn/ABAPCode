FUNCTION zzz_uploadexcel.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_FILENAME) TYPE  RLGRAP-FILENAME
*"     REFERENCE(IM_BEGIN_COL) TYPE  I
*"     REFERENCE(IM_BEGIN_ROW) TYPE  I
*"     REFERENCE(IM_END_COL) TYPE  I
*"     REFERENCE(IM_END_ROW) TYPE  I
*"  EXPORTING
*"     REFERENCE(EX_SEPARATOR) TYPE  CHAR1
*"  TABLES
*"      IT_EXCELTAB
*"  EXCEPTIONS
*"      INCONSISTENT_PARAMETERS
*"      UPLOAD_OLE
*"----------------------------------------------------------------------

  CONSTANTS: c_hex_tab    TYPE x VALUE '9'.

  FIELD-SYMBOLS: <field>.

  DATA: wa_application TYPE ole2_object,
        wa_workbook    TYPE ole2_object,
        wa_range       TYPE ole2_object,
        wa_worksheet   TYPE ole2_object.

  DATA: wa_cell  TYPE ole2_object.
  DATA: wa_cell1 TYPE ole2_object.

  DEFINE m_message.
    case sy-subrc.
      when 0.
      when 1.
        message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      when others.
        raise upload_ole.
    endcase.
  END-OF-DEFINITION.

  IF im_begin_row > im_end_row. RAISE inconsistent_parameters. ENDIF.
  IF im_begin_col > im_end_col. RAISE inconsistent_parameters. ENDIF.

*  IF wa_application-header = space OR wa_application-handle = -1.
  IF wa_application-header = space OR wa_application-handle = 0.
    CREATE OBJECT wa_application 'Excel.Application'.
    m_message.
  ENDIF.
  CALL METHOD OF
      wa_application
      'Workbooks'    = wa_workbook.
  m_message.

  CALL METHOD OF
      wa_workbook
      'Open'

    EXPORTING
      #1          = im_filename.
  m_message.

  GET PROPERTY OF  wa_application 'ACTIVESHEET' = wa_worksheet.
  m_message.

  CALL METHOD OF
      wa_worksheet
      'Cells'      = wa_cell
    EXPORTING
      #1           = im_begin_row
      #2           = im_begin_col.
  m_message.

  CALL METHOD OF
      wa_worksheet
      'Cells'      = wa_cell1
    EXPORTING
      #1           = im_end_row
      #2           = im_end_col.
  m_message.

  CALL METHOD OF
      wa_worksheet
      'RANGE'      = wa_range
    EXPORTING
      #1           = wa_cell
      #2           = wa_cell1.
  m_message.
  CALL METHOD OF
      wa_range
      'SELECT'.
  m_message.

  CALL METHOD OF
      wa_range'COPY'.
  m_message.

  m_message.

* Without flush, CLPB_IMPORT does not find the data
  CALL FUNCTION 'CONTROL_FLUSH'
    EXCEPTIONS
      OTHERS = 3.

  CALL FUNCTION 'CLPB_IMPORT'
    TABLES
      data_tab   = it_exceltab
    EXCEPTIONS
      clpb_error = 1
      OTHERS     = 2.

  IF sy-subrc <> 0. MESSAGE x001(kx). ENDIF.
  ASSIGN ex_separator TO <field> TYPE 'X'.
  <field> = c_hex_tab.

  ex_separator = cl_bcs_convert=>gc_tab.

  SET PROPERTY OF wa_application 'CutCopyMode' = 0.
  m_message.

  CALL METHOD OF
      wa_application
      'QUIT'.
  m_message.

  FREE   OBJECT wa_cell.
  m_message.
  FREE   OBJECT wa_cell1.
  m_message.
  FREE   OBJECT wa_range.
  FREE   OBJECT wa_worksheet.
  FREE   OBJECT wa_workbook.
  FREE   OBJECT wa_application.
  m_message.


ENDFUNCTION.
