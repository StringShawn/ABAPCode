REPORT ztest_sendmail.

PARAMETERS p_file LIKE rlgrap-filename OBLIGATORY.
*&---------------------------------------------------------------------*
*&   Event AT SELECTION-SCREEN ON
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM frm_file_open_f4 CHANGING p_file.
*&---------------------------------------------------------------------*
*&   Event AT START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM frm_send_mail.

*&---------------------------------------------------------------------*
*&      Form  FRM_FILE_OPEN_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_FILE  text
*----------------------------------------------------------------------*
FORM frm_file_open_f4 CHANGING file.

  DATA: lit_file      TYPE filetable,
        l_file_filter TYPE string,
        l_rc          TYPE i.
  FIELD-SYMBOLS: <fs_file> LIKE LINE OF lit_file.

  l_file_filter = 'Excel Template (*.xlt)|*.xlt|Excel Workbook (*.xls)|*.xls|All files (*.*)|*.*'.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Choose template file ...'
      default_extension       = '*.*'
      "file_filter             = l_file_filter
    CHANGING
      file_table              = lit_file
      rc                      = l_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc = 0.
    READ TABLE lit_file ASSIGNING <fs_file> INDEX 1.

    IF sy-subrc = 0.
      file = <fs_file>-filename.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SEND_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_send_mail .

  DATA:lo_document      TYPE REF TO cl_document_bcs, "用来放发送的内容的类
       lit_contents     TYPE soli_tab,
       l_cc             TYPE adr6-smtp_addr,
       l_to             TYPE adr6-smtp_addr,
       l_bcs_to         TYPE REF TO if_recipient_bcs,
       l_bcs_cc         TYPE REF TO if_recipient_bcs,
       lo_sender        TYPE REF TO cl_sapuser_bcs,
       l_result         TYPE os_boolean,
*      cl_bcs发送邮件主要用到的功能类, 包括创建发送请求, 添加发送内容,添加发送地址, 到最终的发送指令发出.
       w_document       TYPE REF TO cl_bcs,
       l_file_size_char TYPE so_obj_len,
       l_filen          TYPE string,
       lit_mailhex      TYPE solix_tab,
       l_file_size      TYPE i,
       l_rc             TYPE i,
       l_string         TYPE string,
       l_subject        TYPE so_obj_des,
*      cx_bcs异常类, 捕捉发送邮件过程中出现的异常.
       lo_fail          TYPE REF TO cx_bcs.


* ATTACH THE FILE, THE ATTACHMENT TYPE SHOULD BE BIN TO ACCEPT ANY KIND OF ATTACHMENT, INCLUDING VIDEOS, AUDIO FILES ETC...

  l_filen = p_file.
* 读取附件
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = l_filen
      filetype                = 'BIN'  "
*     has_field_separator     = SPACE
*     header_length           = 0
*     read_by_line            = ''
*     dat_mode                = SPACE
*     codepage                = SPACE
*     ignore_cerr             = ABAP_TRUE
*     replacement             = '#'
*     virus_scan_profile      =
    IMPORTING
      filelength              = l_file_size
*     header                  =
    CHANGING
      data_tab                = lit_mailhex
*     isscanperformed         = SPACE
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    RETURN.
  ENDIF.
* 附件长度,这个很重要,一定要有
  l_file_size_char = l_file_size.
* 邮件正文
  l_string = '哈喽,地球人,点开附件有惊喜'.
  APPEND l_string  TO lit_contents.

  TRY.
*     CREATE THE DOCUMENT WITH CONTENTS
      CREATE OBJECT lo_document.
      lo_document = cl_document_bcs=>create_document(
          i_type       = 'HTM'
          i_subject    = '一封来自火星的测试邮件'  "邮件标题
          i_length     = l_file_size_char
          i_language   = sy-langu
          i_importance = '1'
          i_text       = lit_contents
          ).

*     hard code 附件为excel
      l_subject = '附件.xls'.
*     添加附件,可以添加多个附件
      CALL METHOD lo_document->add_attachment
        EXPORTING
          i_attachment_type    = 'BIN'
          i_attachment_subject = l_subject
          i_attachment_size    = l_file_size_char
          i_att_content_hex    = lit_mailhex.

*     CREATING PERSISTENT OBJECT WILL ALLOW YOU TO SET THE DOCUMENT IN THE MAIL
      w_document = cl_bcs=>create_persistent( ).

*     发件人
*     前提是这个邮箱地址能发邮件，并且不需要密码
      lo_sender = cl_sapuser_bcs=>create( sy-uname ).
      w_document->set_sender( lo_sender ).

*     收件人
      l_to = 'mars@mars.com'.
      l_bcs_to = cl_cam_address_bcs=>create_internet_address( l_to ).
*     Add recipient to send request
      CALL METHOD w_document->add_recipient
        EXPORTING
          i_recipient = l_bcs_to.

*    抄送人
      l_cc = 'test@test.com'.
      l_bcs_cc = cl_cam_address_bcs=>create_internet_address( l_cc ).

      CALL METHOD w_document->add_recipient
        EXPORTING
          i_recipient = l_bcs_cc
          i_copy      = 'X'.
      " i_express   = 'X'.

*     SEND THE MAIL
      w_document->set_send_immediately( 'X' ).  "设置立即发送
      w_document->send_request->set_link_to_outbox( 'X' ). "与outbox关联
      CALL METHOD w_document->set_document( lo_document ).
      CALL METHOD w_document->send(
        EXPORTING
          i_with_error_screen = 'X'
        RECEIVING
          result              = l_result ).
    CATCH cx_bcs INTO lo_fail.
  ENDTRY.
* YOU CAN VERIFY THE STATUS IN THE LIST, YOU CAN ALSO SUBMIT THIS AS A BACKGROUND JOB.

  IF l_result = 'X'.
    COMMIT WORK AND WAIT.
    MESSAGE '发送成功' TYPE 'S'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE '发送失败' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.