FUNCTION ZZZ_GENERATE_EXCEL_DATA.
*"----------------------------------------------------------------------
*"*"局部接口：
*"  IMPORTING
*"     REFERENCE(IM_FILENAME) TYPE  STRING OPTIONAL
*"  TABLES
*"      IT_EXCEL TYPE  STANDARD TABLE
*"  EXCEPTIONS
*"      DOWNLOAD_FAIL
*"----------------------------------------------------------------------

CONSTANTS c_default_filename TYPE string VALUE 'data.xlsx'.
DATA:it_struc_comp_table TYPE cl_abap_structdescr=>component_table,
     wa_struc_comp_table LIKE LINE OF it_struc_comp_table.
DATA i_o_descr TYPE REF TO cl_abap_typedescr.
DATA i_o_tab   type REF TO cl_abap_tabledescr.
DATA i_o_datae TYPE REF TO cl_abap_elemdescr.
DATA i_o_struc TYPE REF TO cl_abap_structdescr.
DATA i_dataref   TYPE REF TO data.
DATA i_filepath TYPE string.
DATA i_fullpath TYPE string.
DATA i_filename TYPE string.
DATA:i_workbooks     TYPE  ole2_object,
     i_workbook      TYPE  ole2_object,
     i_line          TYPE  i,
     i_allline       TYPE  i,
     i_column        TYPE  i,
     i_bold          TYPE  i,
     i_linen(5)      TYPE  N,
     i_txtp          TYPE  c LENGTH 255,
     i_txt(255)      type c,
     i_percent       type  I.
FIELD-SYMBOLS:<fs_s>  TYPE  any,
              <fs_t>  TYPE  any TABLE,
              <fs_f>  TYPE  any.

*- Get type
  i_o_descr =  cl_abap_typedescr=>describe_by_data( IT_EXCEL ).
  IF i_o_descr IS NOT BOUND.
    RAISE DOWNLOAD_FAIL.
  ENDIF.
  i_o_struc ?= i_o_descr.
  it_struc_comp_table = i_o_struc->get_components( ).
  i_o_tab = cl_abap_tabledescr=>create( i_o_struc ).
  "form the structure type
  CREATE DATA i_dataref TYPE HANDLE i_o_struc.
  ASSIGN i_dataref->* to <fs_s> CASTING TYPE HANDLE i_o_struc.

*- If the file name is not imported, popup the file path
  IF im_filename IS INITIAL.
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        default_file_name    = c_default_filename
      CHANGING
        filename             = i_filename
        path                 = i_filepath
        fullpath             = i_fullpath
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
      RAISE DOWNLOAD_FAIL.
    ENDIF.
    CHECK i_fullpath IS NOT INITIAL.
  ELSE.
    i_fullpath = im_filename.
  ENDIF.

*- Using standard OLE to save the data to EXCEL (XLS)
  "Start OLE excel service
  CLEAR:g_excel.
  CREATE OBJECT g_excel 'Excel.Application'.
  set property of g_excel 'Visible' = 0.
  "New Work book service
  CLEAR:i_workbooks.
  CALL METHOD  OF g_excel 'Workbooks' = i_workbooks.
  "Add new workbook
  CLEAR:i_workbook.
  CALL METHOD OF i_workbooks 'Add' = i_workbook.
  "Each line to Each Lin in EXCEL
  CLEAR:i_line,
        i_column,
        i_bold,
        i_allline.
  DESCRIBE TABLE it_excel[] LINES i_allline.
  LOOP AT it_excel[] ASSIGNING <fs_s>.

    i_line = i_line + 1.
    IF i_line = 1.    "Header
      i_bold = 1.
    ELSE.
      i_bold = 0.
    ENDIF.

    "进度条\
    i_percent = ( i_line / i_allline ) * 100.
    i_linen = i_line.
    CLEAR:i_txtp.
    CONCATENATE '正在下载Excel第'
                i_linen '条数据...' INTO i_txtp.
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        percentage = i_percent
        text       = i_txtp.

    CLEAR:i_column.
    LOOP AT it_struc_comp_table INTO wa_struc_comp_table.
      ASSIGN COMPONENT wa_struc_comp_table-name OF STRUCTURE
        <fs_s> TO <fs_f>.
      IF <fs_f> IS ASSIGNED.
        CLEAR:i_txt.
        WRITE <fs_f> to i_txt LEFT-JUSTIFIED no-GROUPING.

        IF wa_struc_comp_table-TYPE->TYPE_KIND = 'C'.  "C类型
          CONCATENATE '''' i_txt INTO i_txt.
        ENDIF.

        i_column = i_column + 1.
        PERFORM frm_fill_excel_cell USING i_line
                                          i_column
                                          i_bold "1-Bold;0-No bold
                                          i_txt
                                          .
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  "Save to LOCAL
  CALL METHOD OF i_workbook 'SAVEAS' EXPORTING  #1 = i_fullpath.
  CALL METHOD OF i_workbook 'Close'.
  CALL METHOD OF g_excel 'QUIT'.
  "Free EXCEL object
  FREE OBJECT g_excel.


ENDFUNCTION.
