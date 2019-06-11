class ZCL_15405_XMTB definition
  public
  final
  create public .

public section.

  methods SET_LAYOUT
    importing
      !IS_LAYO type LVC_S_LAYO optional .
  methods SET_FIELDCAT
    importing
      !IT_FCAT type LVC_T_FCAT optional
      !IS_STRUCTURE type DD02L-TABNAME optional .
  methods DISPLAY_ALV
    importing
      !IS_REPID type SY-REPID
      !I_USER_COMMAND type SLIS_FORMNAME optional
      !I_PF_STATUS type SLIS_FORMNAME optional
      !IT_DATA type TABLE .
  methods SEND_EMAIL
    importing
      !IT_MAIL type TABLE
      !IS_TYPE type CHAR1 optional .
  methods MANTAIN_TABLE
    importing
      !TABNAME type DD02V-TABNAME .
protected section.
private section.

  data GS_LAYO type LVC_S_LAYO .
  data GT_FCAT type LVC_T_FCAT .
ENDCLASS.



CLASS ZCL_15405_XMTB IMPLEMENTATION.


  METHOD display_alv.


    FIELD-SYMBOLS:<dyn_table> TYPE STANDARD TABLE,
                  <dyn_wa>    TYPE any,
                  <dyn_field> TYPE any.

    DATA: dy_table TYPE REF TO data.

*    CALL METHOD cl_alv_table_create=>create_dynamic_table
*      EXPORTING
*        it_fieldcatalog = gt_fcat
*      IMPORTING
*        ep_table        = dy_table.
    CREATE DATA dy_table LIKE IT_DATA.
    ASSIGN dy_table->* TO <dyn_table>.
    <dyn_table> = it_data.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
*       I_INTERFACE_CHECK        = ' '
*       I_BYPASSING_BUFFER       =
*       I_BUFFER_ACTIVE          =
        i_callback_program       = is_repid
        i_callback_pf_status_set = i_pf_status
        i_callback_user_command  = i_user_command
*       I_CALLBACK_TOP_OF_PAGE   = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME         =
*       I_BACKGROUND_ID          = ' '
*       I_GRID_TITLE             =
*       I_GRID_SETTINGS          =
        is_layout_lvc            = gs_layo
        it_fieldcat_lvc          = gt_fcat
*       IT_EXCLUDING             =
*       IT_SPECIAL_GROUPS_LVC    =
*       IT_SORT_LVC              =
*       IT_FILTER_LVC            =
*       IT_HYPERLINK             =
*       IS_SEL_HIDE              =
*       I_DEFAULT                = 'X'
*       I_SAVE                   = ' '
*       IS_VARIANT               =
*       IT_EVENTS                =
*       IT_EVENT_EXIT            =
*       IS_PRINT_LVC             =
*       IS_REPREP_ID_LVC         =
*       I_SCREEN_START_COLUMN    = 0
*       I_SCREEN_START_LINE      = 0
*       I_SCREEN_END_COLUMN      = 0
*       I_SCREEN_END_LINE        = 0
*       I_HTML_HEIGHT_TOP        =
*       I_HTML_HEIGHT_END        =
*       IT_ALV_GRAPHICS          =
*       IT_EXCEPT_QINFO_LVC      =
*       IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*       E_EXIT_CAUSED_BY_CALLER  =
*       ES_EXIT_CAUSED_BY_USER   =
      TABLES
        t_outtab                 = <dyn_table>
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.
  ENDMETHOD.


  METHOD mantain_table.
    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action                       = 'U'
*       CORR_NUMBER                  = '          '
*       GENERATE_MAINT_TOOL_IF_MISSING       = ' '
*       SHOW_SELECTION_POPUP         = ' '
        view_name                    = tabname
*       NO_WARNING_FOR_CLIENTINDEP   = ' '
*       RFC_DESTINATION_FOR_UPGRADE  = ' '
*       CLIENT_FOR_UPGRADE           = ' '
*       VARIANT_FOR_SELECTION        = ' '
*       COMPLEX_SELCONDS_USED        = ' '
*       CHECK_DDIC_MAINFLAG          = ' '
*       SUPPRESS_WA_POPUP            = ' '
*   TABLES
*       DBA_SELLIST                  =
*       EXCL_CUA_FUNCT               =
      EXCEPTIONS
        client_reference             = 1
        foreign_lock                 = 2
        invalid_action               = 3
        no_clientindependent_auth    = 4
        no_database_function         = 5
        no_editor_function           = 6
        no_show_auth                 = 7
        no_tvdir_entry               = 8
        no_upd_auth                  = 9
        only_show_allowed            = 10
        system_failure               = 11
        unknown_field_in_dba_sellist = 12
        view_not_found               = 13
        maintenance_prohibited       = 14
        OTHERS                       = 15.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD send_email.
    DATA:send_request TYPE REF TO cl_bcs,
         document     TYPE REF TO cl_document_bcs,
         fail         TYPE REF TO cx_bcs,
         recipient    TYPE REF TO if_recipient_bcs,
         lo_sender    TYPE REF TO if_sender_bcs.
    DATA:ls        TYPE string,
         mailto    TYPE ad_smtpadr,
         main_text TYPE bcsy_text,
         title     TYPE so_obj_des.
    TYPES:BEGIN OF ly_mail,
            smtp_addr TYPE ad_smtpadr,
          END OF ly_mail.
    DATA:lt_mail TYPE TABLE OF ly_mail,
         ls_mail TYPE ly_mail.
    DATA:lv_string TYPE string.
    DATA binary_content TYPE solix_tab.
    DATA:lt_att_content_text TYPE soli_tab,
         ls_att_content_text TYPE soli.
    DATA size           TYPE so_obj_len.
    "邮件标题

    CASE is_type.
      WHEN '1'.
        DATA:lt_xmtb TYPE TABLE OF zt15405_xmtb,
             ls_xmtb TYPE zt15405_xmtb,
             lt_xmjl TYPE TABLE OF zt15405_xmjl,
             ls_xmjl TYPE zt15405_xmjl,
             lv_send TYPE ad_smtpadr.
        lt_xmtb = it_mail.
        READ TABLE lt_xmtb INTO ls_xmtb INDEX 1.
        SELECT SINGLE c~zxmjl
                zxmjlxm
                zxmjleml
          INTO CORRESPONDING FIELDS OF ls_xmjl
          FROM zt15405_xmjl AS c JOIN zt15405_xm AS b ON c~zxmjl = b~zxmjl
          WHERE b~zxmdm = ls_xmtb-zxmdm.
        SELECT SINGLE zsqreml
        INTO lv_send
        FROM zt15405_sqr
        WHERE zsqr = ls_xmtb-zsqr.
        title = 'TimeSheet添加'.
        CLEAR ls.
        ls = |<html><body><p>{ ls_xmjl-zxmjlxm }，你好</p>|.
        APPEND ls TO main_text.

        ls = '<p>&nbsp&nbsp&nbsp&nbsp请帮忙添加一下ts，谢谢~</p>'.
        APPEND ls TO main_text.

        ls = '<table border="1"><tr><th>项目</th><th>日期</th></tr>'.
        APPEND ls TO main_text.

        LOOP AT lt_xmtb INTO ls_xmtb .
          ls = '<tr>'.
          APPEND ls TO main_text.
          CONCATENATE '<td>' ls_xmtb-zxmms '</td><td>' ls_xmtb-datum '</td>' INTO ls.
          APPEND ls TO main_text.
          ls = '</tr>'.
          APPEND ls TO main_text.
        ENDLOOP.

        ls = '</table></body></html>'.
        APPEND ls TO main_text.
*      WHEN .
      WHEN OTHERS.
    ENDCASE.

    TRY .
        "第一步 创建发送请求
        send_request = cl_bcs=>create_persistent( ).
        "第二步 创建整理发送内容
        document = cl_document_bcs=>create_document( i_type = 'HTM' i_text = main_text i_subject = title ).
        "第三步 天健邮件内容到发送请求
        send_request->set_document( document ).
*      LOOP AT lt_mail INTO ls_mail.
        mailto = ls_xmjl-zxmjleml.
        "第四步  邮件地址转换
        recipient = cl_cam_address_bcs=>create_internet_address( mailto ).
        "第五步  添加邮件地址到发送请求
        send_request->add_recipient( recipient ).

        lo_sender = cl_cam_address_bcs=>create_internet_address( lv_send ).

        send_request->set_sender( lo_sender ).
*      ENDLOOP.
        "第六步  正式发送并提交作业
        send_request->set_send_immediately( i_send_immediately = 'X' ).
        send_request->send( i_with_error_screen = 'X' ).

        COMMIT WORK.
      CATCH cx_bcs INTO fail.

    ENDTRY.
  ENDMETHOD.


  METHOD set_fieldcat.
    REFRESH gt_fcat.
    IF it_fcat IS  NOT INITIAL.
      gt_fcat = it_fcat.
    ELSEIF is_structure IS NOT INITIAL.
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
*         I_BUFFER_ACTIVE        =
          i_structure_name       = is_structure
*         I_CLIENT_NEVER_DISPLAY = 'X'
*         I_BYPASSING_BUFFER     =
*         I_INTERNAL_TABNAME     =
        CHANGING
          ct_fieldcat            = gt_fcat
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD set_layout.
    CLEAR gs_layo.
    IF is_layo IS INITIAL.
      gs_layo-cwidth_opt = 'X'."列优化
      gs_layo-zebra      = 'X'."斑马线
    ELSE.
      gs_layo = is_layo.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
