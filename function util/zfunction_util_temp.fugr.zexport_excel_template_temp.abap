FUNCTION ZEXPORT_EXCEL_TEMPLATE_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(OBJID) TYPE  WWWDATATAB-OBJID
*"     REFERENCE(DIALOG_ELEMENTS) TYPE  ZDIALOG_ELEMENTS OPTIONAL
*"     REFERENCE(EXCEL_ATTRIBUTE) TYPE  ZEXCEL_ATTRIBUTE OPTIONAL
*"  TABLES
*"      EXCEL_ELEMENTS STRUCTURE  ZEXCEL_ELEMENTS OPTIONAL
*"--------------------------------------------------------------------

  DATA key TYPE wwwdatatab.

  DATA workbooks TYPE ole2_object.
  DATA workbook TYPE ole2_object.
  DATA sheets TYPE ole2_object.
  DATA sheet TYPE ole2_object.
  DATA excel TYPE ole2_object.
  DATA cell  TYPE ole2_object.

  DATA destination TYPE rlgrap-filename.
  DATA default_file_name TYPE string.
  DATA fullpath TYPE string.
  DATA filename TYPE string.
  DATA path TYPE string.
  DATA user_action TYPE i.

  DATA result TYPE abap_bool.


  IF dialog_elements-default_file_name IS INITIAL.
    SELECT SINGLE text INTO default_file_name
      FROM wwwdata
      WHERE relid = 'MI'
        AND objid = objid.
  ELSE.
    default_file_name = dialog_elements-default_file_name.
  ENDIF.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = dialog_elements-window_title
      default_extension    = dialog_elements-default_extension
      default_file_name    = default_file_name
    CHANGING
      filename             = filename
      path                 = path
      fullpath             = fullpath
      user_action          = user_action
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSEIF user_action <> 0.
    RETURN.
  ENDIF.

  destination = fullpath.

*  IF file_path IS NOT INITIAL OR file_name IS NOT INITIAL. "创建文件路径
*    destination = file_path && '\' && file_name.
*  ENDIF.
*
*  IF file_path IS NOT INITIAL.
*    DATA check_path TYPE string.
*    check_path = file_path.
*
*    CALL METHOD cl_gui_frontend_services=>directory_exist
*      EXPORTING
*        directory            = check_path
*      RECEIVING
*        result               = result
*      EXCEPTIONS
*        cntl_error           = 1
*        error_no_gui         = 2
*        wrong_parameter      = 3
*        not_supported_by_gui = 4
*        OTHERS               = 5.
*
*    IF result = abap_false.
*      CALL FUNCTION 'GUI_CREATE_DIRECTORY'
*        EXPORTING
*          dirname = file_path
*        EXCEPTIONS
*          failed  = 1
*          OTHERS  = 2.
*    ENDIF.
*  ENDIF.


  key-relid = 'MI'.
  key-objid = objid.

  "因为函数download_web_object本身代码的限制，不能支持弹出选择框，然后选择文件存放地点，所以先要弹出对话框
  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      key         = key
      destination = destination.

  CHECK excel_elements[] IS NOT INITIAL.

  open_excel destination.

  select_sheet ''.

  fill_cell_with_tab 'excel_elements'.

  IF excel_attribute-sheet_protect = 'X'.
    set_sheet_protect ''.
  ENDIF.

  IF excel_attribute-open_flag = 'X'.
    set_excel_autoopen.
  ENDIF.

  save_excel.

ENDFUNCTION.
