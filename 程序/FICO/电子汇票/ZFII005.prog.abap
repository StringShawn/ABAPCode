*&---------------------------------------------------------------------*
*& Report  ZFII005
* Program ID/Name: ZFII005                Date written: 2019.03.19
* Author's name:  HAND_ABAP1
* Program title: 财务公司发票接口
* Project Name:
* Version:
* Function Spec ID
*&---------------------------------------------------------------------*
REPORT zfii005 MESSAGE-ID zfico_msg.

TYPES:BEGIN OF ty_data ,
        field01  TYPE string,"数据1
        field02  TYPE string,"数据2
        field03  TYPE string,"数据3
        field04  TYPE string,"数据4
        field05  TYPE string,"数据5
        field06  TYPE string,"数据6
        field07  TYPE string,"数据7
        field08  TYPE string,"数据8
      END OF ty_data.

DATA:gt_data TYPE TABLE OF zsdianzihpi,
     gs_data TYPE zsdianzihpi.
DATA:g_folder TYPE REF TO zcl_sharefolder,
     g_file TYPE string,
     g_backfile TYPE string .
CONSTANTS:c_ecds TYPE zz_cont VALUE 'ECDS',
          c_ecds_bak TYPE zz_cont VALUE 'ECDS_BAK',
          c_separator TYPE c VALUE '^'.

CONSTANTS:
  gc_tab  TYPE c VALUE cl_bcs_convert=>gc_tab,
  gc_crlf TYPE c VALUE cl_bcs_convert=>gc_crlf.

DATA:BEGIN OF it_file OCCURS 0,
      name TYPE  epsfilnam ,
     END OF it_file.

*$*$********************************************************************
*$*$    INITIALIZATION                                                 *
*$*$********************************************************************
INITIALIZATION.

*$*$********************************************************************
*$*$    AT SELECTION-SCREEN                                            *
*$*$********************************************************************
AT SELECTION-SCREEN.
*  IF sy-ucomm = 'ONLI'.
*
*  ENDIF.
*$*$********************************************************************
*$*$    START-OF-SELECTION                                             *
*$*$********************************************************************
START-OF-SELECTION.

  PERFORM frm_get_path.
  PERFORM frm_get_from_ftp.

*$*$********************************************************************
*$*$    END-OF-SELECTION                                             *
*$*$********************************************************************
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FROM_FTP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_get_from_ftp .
  DATA:i_dirname    LIKE epsf-epsdirnam,
         i_name     TYPE string,
         i_backfile TYPE string,
         i_extnam   TYPE string,
         i_filenam  TYPE string.
  DATA:it_l_dir  TYPE STANDARD TABLE OF epsfili,
       wa_l_dir  TYPE epsfili .
  i_dirname = g_file.
  CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
    EXPORTING
      dir_name               = i_dirname
    TABLES
      dir_list               = it_l_dir
    EXCEPTIONS
      invalid_eps_subdir     = 1
      sapgparam_failed       = 2
      build_directory_failed = 3
      no_authorization       = 4
      read_directory_failed  = 5
      too_many_read_errors   = 6
      empty_directory_list   = 7
      OTHERS                 = 8.
  IF sy-subrc <> 0.
    MESSAGE i118(zmm_msg) WITH i_dirname.
    STOP.
  ELSE.
    LOOP AT it_l_dir INTO wa_l_dir.
      CLEAR :i_name ,i_extnam.
      it_file-name = wa_l_dir-name.

      SPLIT  it_file-name AT '.' INTO i_name i_extnam.
      TRANSLATE i_extnam TO UPPER CASE.
      IF i_extnam = 'TXT'.
        APPEND it_file.
        CLEAR it_file.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT it_file .
    CLEAR: i_filenam,gt_data,i_backfile.
    CONCATENATE g_file  it_file-name INTO i_filenam.
    CONCATENATE g_backfile  it_file-name INTO i_backfile.
    PERFORM frm_get_data USING i_filenam . "从共享服务期获取EDI数据
    PERFORM frm_process_data. "生成凭证并清帐
    PERFORM frm_move_file USING i_filenam i_backfile.
    PERFORM frm_delete_file USING i_filenam. "删除原始文件..
  ENDLOOP.
ENDFORM.                    " FRM_GET_FROM_FTP
*&---------------------------------------------------------------------*
*&      Form  FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_process_data .
  DATA:lt_log TYPE TABLE OF zzt_dianzihp_log,
       ls_log TYPE zzt_dianzihp_log.

  CLEAR:ls_log,lt_log.

  CALL FUNCTION 'ZFICO_003'
    TABLES
      t_input  = gt_data
      t_output = lt_log.
  "错误日志保存底表
  IF lt_log IS NOT INITIAL.
    PERFORM frm_send_email TABLES lt_log.
    MODIFY zzt_dianzihp_log FROM TABLE lt_log.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.                    " FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_get_path .
  DATA: i_ttype   TYPE zz_type,
        wa_l_asn  TYPE ztg01_s_con.
  DATA:it_l_msg  TYPE ztg01_t_msg..
  CREATE OBJECT g_folder.
  i_ttype = 'A'.
  CALL METHOD g_folder->get_connect
    EXPORTING
      im_tcont     = c_ecds
      im_ttype     = i_ttype
    IMPORTING
      ls_conection = wa_l_asn
      ct_msg       = it_l_msg.
  g_file = wa_l_asn-ztpath.

  CALL METHOD g_folder->get_connect
    EXPORTING
      im_tcont     = c_ecds_bak
      im_ttype     = i_ttype
    IMPORTING
      ls_conection = wa_l_asn
      ct_msg       = it_l_msg.
  g_backfile = wa_l_asn-ztpath.
ENDFORM.                    " FRM_GET_PATH
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_EDI_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_FILENAM  text
*----------------------------------------------------------------------*
FORM frm_get_data  USING  pr_filename.
  DATA:lt_con TYPE TABLE OF ztedi_conr,
       ls_con LIKE LINE OF lt_con.
  DATA:
       lv_date TYPE string.
  DATA:error_flag.
  "----------------------------------------------------------------------
  DATA:it_l_data  TYPE ztg01_t_data,
        it_l_msg   TYPE ztg01_t_msg,
        wa_l_data  TYPE ztg01_data,
        wa_l_msg   TYPE ztg01_msg,
        wa_lc_data TYPE ty_data,
        lt_data   LIKE gt_data,
        lt_data_tmp LIKE gt_data,
        ls_data   LIKE gs_data.

  DATA:i_filename TYPE string.


  i_filename = pr_filename.
  CREATE OBJECT g_folder.
  CLEAR: it_l_data[],gt_data[].

  CALL METHOD g_folder->get_from_server_utf8
    EXPORTING
      im_filename = i_filename
    IMPORTING
      ct_data     = it_l_data
      ct_msg      = it_l_msg.

  IF it_l_msg[] IS INITIAL.
    LOOP AT it_l_data INTO wa_l_data.
      CLEAR:wa_lc_data.

      SPLIT wa_l_data AT c_separator
      INTO  wa_lc_data-field01
            wa_lc_data-field02
            wa_lc_data-field03
            wa_lc_data-field04
            wa_lc_data-field05
            wa_lc_data-field06
            wa_lc_data-field07
            wa_lc_data-field08.

      IF sy-subrc = 0.
        CONDENSE:wa_lc_data-field01,
                 wa_lc_data-field02,
                 wa_lc_data-field03,
                 wa_lc_data-field04,
                 wa_lc_data-field05,
                 wa_lc_data-field06,
                 wa_lc_data-field07,
                 wa_lc_data-field08.

        gs_data-bukrs = wa_lc_data-field01.
        gs_data-lifnr = wa_lc_data-field02.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_data-lifnr
          IMPORTING
            output = gs_data-lifnr.
        gs_data-butxt = wa_lc_data-field03.
        gs_data-name1 = wa_lc_data-field04.
        gs_data-zuonr = wa_lc_data-field05.
        PERFORM frm_deal_date USING wa_lc_data-field06
                              CHANGING lv_date.
        gs_data-zfbdt = lv_date.
        gs_data-wrbtr = wa_lc_data-field07.
        APPEND gs_data TO lt_data.
      ENDIF.
      CLEAR lv_date.
*      ENDIF.
    ENDLOOP.
    SORT lt_data BY bukrs lifnr.
    APPEND LINES OF lt_data TO gt_data.

  ELSE.
    CLEAR wa_l_msg.
    READ TABLE it_l_msg INTO wa_l_msg INDEX 1.
    IF sy-subrc = 0.
      MESSAGE e200 WITH wa_l_msg-msg .
    ENDIF.

  ENDIF.


ENDFORM.                    " FRM_GET_EDI_DATA
**&---------------------------------------------------------------------
*
*&      Form  FRM_DELETE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_FILENAM  text
*----------------------------------------------------------------------*
FORM frm_delete_file  USING    pr_filenam.
  DATA:i_filename TYPE string.
  DATA: wa_l_msg TYPE ztg01_msg,
        it_l_msg TYPE ztg01_t_msg.
  i_filename = pr_filenam.
  CREATE OBJECT g_folder.

  CALL METHOD g_folder->delete_from_server
    EXPORTING
      im_filename = i_filename
    IMPORTING
      ct_msg      = it_l_msg.

ENDFORM.                    " FRM_DELETE_FILE
*&---------------------------------------------------------------------*
*&      Form  FRM_MOVE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PR_FILENAM  text
*----------------------------------------------------------------------*
FORM frm_move_file  USING    pr_filenam pr_backfile.
  DATA:it_l_data  TYPE ztg01_t_data,
       it_l_msg   TYPE ztg01_t_msg,
       i_filename TYPE string.

  CLEAR: it_l_data[].
  CALL METHOD g_folder->get_from_server_utf8
    EXPORTING
      im_filename = pr_filenam
    IMPORTING
      ct_data     = it_l_data
      ct_msg      = it_l_msg.

  IF it_l_msg[] IS INITIAL.

    CALL METHOD g_folder->send_to_server
      EXPORTING
        im_filename = pr_backfile
        ct_data     = it_l_data
      IMPORTING
        ct_msg      = it_l_msg.

  ENDIF.

ENDFORM.                    " FRM_MOVE_FILE
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LC_DATA_FIELD05  text
*      <--P_LV_DATE  text
*----------------------------------------------------------------------*
FORM frm_deal_date  USING    lv_field
                    CHANGING lv_date.
  DATA:   lv_s1 TYPE string,
          lv_s2 TYPE string,
          lv_s3 TYPE string.
  IF  lv_field IS NOT INITIAL .
    SPLIT  lv_field AT '-' INTO lv_s1 lv_s2 lv_s3.
    CONDENSE:lv_s1, lv_s2, lv_s3.
    IF strlen( lv_s2 ) < 2.
      CONCATENATE '0' lv_s2 INTO lv_s2.
    ENDIF.
    IF strlen( lv_s3 ) < 2.
      CONCATENATE '0' lv_s3 INTO lv_s3.
    ENDIF.
    CONCATENATE lv_s1 lv_s2 lv_s3 INTO lv_date.
    CONDENSE lv_date.
  ENDIF.


ENDFORM.                    " FRM_DEAL_DATE
*&---------------------------------------------------------------------*
*&      Form  FRM_SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_send_email  TABLES   p_log STRUCTURE zzt_dianzihp_log.
  DATA:send_request TYPE REF TO cl_bcs,
       document TYPE REF TO cl_document_bcs,
       fail TYPE REF TO cx_bcs,
       recipient TYPE REF TO if_recipient_bcs.
  DATA:ls        TYPE string,
       mailto    TYPE ad_smtpadr,
       main_text TYPE bcsy_text,
       title     TYPE so_obj_des.

  DATA:lv_string TYPE string.
  DATA binary_content TYPE solix_tab.
  DATA:lt_att_content_text TYPE soli_tab,
       ls_att_content_text TYPE soli.
  DATA size           TYPE so_obj_len.
  DATA:lv_dmbtr TYPE string.
  DATA:lt_mail TYPE TABLE OF ztemil_ecds,
       ls_mail TYPE ztemil_ecds.
  "邮件标题
  title = '电子汇票回传及应付自动清帐'.
  CLEAR ls.
  ls = '电子汇票回传及应付自动清帐程序已经执行，执行结果如附件，请查看。'.
  APPEND ls TO main_text.

  CONCATENATE '公司代码' '供应商编码' '汇票号' '创建日期' '创建时间'
  '公司代码描述' '供应商描述' '凭证金额' '到期日' '会计凭证编号'
  '清账凭证编号' '清账日期' '消息类型' '消息' INTO lv_string SEPARATED BY gc_tab.
  CONCATENATE lv_string gc_crlf INTO lv_string.
  CLEAR lt_mail.
  SELECT * INTO TABLE lt_mail FROM ztemil_ecds.

  LOOP AT p_log.
    CLEAR lv_dmbtr.
    lv_dmbtr = p_log-wrbtr.
    CONCATENATE:  lv_string
                  p_log-bukrs gc_tab
                  p_log-lifnr gc_tab
                  p_log-zuonr gc_tab
                  p_log-cpudt gc_tab
                  p_log-cputm gc_tab
                  p_log-butxt gc_tab
                  p_log-name1 gc_tab
                  lv_dmbtr gc_tab
                  p_log-zfbdt gc_tab
                  p_log-belnr gc_tab
                  p_log-augbl gc_tab
                  p_log-augdt gc_tab
                  p_log-ztype gc_tab
                  p_log-zmsg  gc_crlf INTO lv_string.
    CLEAR:p_log.
  ENDLOOP.

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

  TRY .
      "第一步 创建发送请求
      send_request = cl_bcs=>create_persistent( ).
      "第二步 创建整理发送内容
      document = cl_document_bcs=>create_document( i_type = 'RAW' i_text = main_text i_subject = title ).
      document->add_attachment(
      EXPORTING
        i_attachment_type = 'xls'
        i_attachment_subject = title
        i_attachment_size    = size
        i_att_content_hex   = binary_content ).
      "第三步 天健邮件内容到发送请求
      send_request->set_document( document ).
      LOOP AT lt_mail INTO ls_mail.
        mailto = ls_mail-smtp_addr.
        "第四步  邮件地址转换
        recipient = cl_cam_address_bcs=>create_internet_address( mailto ).
        "第五步  添加邮件地址到发送请求
        send_request->add_recipient( recipient ).
      ENDLOOP.
      "第六步  正式发送并提交作业
      send_request->set_send_immediately( i_send_immediately = 'X' ).
      send_request->send( i_with_error_screen = 'X' ).
    CATCH cx_bcs INTO fail.

  ENDTRY.

ENDFORM.                    " FRM_SEND_EMAIL