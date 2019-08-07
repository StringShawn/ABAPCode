*&---------------------------------------------------------------------*
*& Report ZMMR014
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zmmr014.
type-pools: slis,icon.
tables:plaf,sscrfields.
data: go_grid type ref to cl_gui_alv_grid.
data: gt_fieldcat   type lvc_t_fcat,
      gs_fieldcat   type lvc_s_fcat,
      gs_layout_lvc type lvc_s_layo.
data mcr_fieldcat.
define mcr_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname  = &1.
  gs_fieldcat-scrtext_s  =
  gs_fieldcat-scrtext_l  =
  gs_fieldcat-scrtext_m  =
  gs_fieldcat-reptext    =
  gs_fieldcat-coltext    = &2.
  gs_fieldcat-edit       = &3.
  gs_fieldcat-checkbox   = &4.
  gs_fieldcat-key        = &5.
  gs_fieldcat-icon       = &6.
  gs_fieldcat-ref_table  = &7.
  gs_fieldcat-ref_field  = &8.
  gs_fieldcat-outputlen  = &9.
  APPEND gs_fieldcat TO gt_fieldcat.
end-of-definition.

constants:serv_group like rzllitab-classname value 'JOBRUN'.
constants:c_funtxt_01 type rsfunc_txt value 'ģ������'.

data:gt_mard   type standard table of zmms_014_mard,
     gt_mkol   type standard table of zmms_014_mkol,
     gt_mard_f type standard table of zmms_014_mard,
     gt_mkol_f type standard table of zmms_014_mkol,
     gt_ekpo   type standard table of zmms_014_ekpo,
     gt_plan   type standard table of zmms_014_plan,
     gt_psttr  type standard table of zmms_014 with header line,
     gt_matnr  type standard table of zmms_014 with header line,
     gt_resb   type standard table of zmms_014_resb,
     wa_resb   type zmms_014_resb,
     wa_mard   type zmms_014_mard,
     wa_mkol   type zmms_014_mkol,
     wa_mard_f type zmms_014_mard,
     wa_mkol_f type zmms_014_mkol,
     wa_plan   type zmms_014_plan,
     wa_ekpo   type zmms_014_ekpo.
data:st_flag type c,
     po_flag type c,
     re_flag type c.
data:g_error type c.
field-symbols:<ls_ekpo> type zmms_014_ekpo.
types:begin of ty_excel,
        knumv        type string,    " ˳��
        plwrk        type string,    " ����
        matnr        type string,    " ��������
        gsmng        type string,    " ��������
        psttr        type string,    " �ƻ�������ʼ����
        pedtr        type string,    " �ƻ������������
        eflag        type c,
        zicon(4)     type c,
        message(100) type c,
      end of ty_excel.
data:gt_excel type standard table of ty_excel.
data:ga_excel type ty_excel.

types:begin of ty_out,
        psttr     type plaf-psttr,    " �ƻ�������ʼ����
        pedtr     type plaf-pedtr,    " �ƻ������������
        plnum     type plaf-plnum,    " �ƻ�������
        plwrk     type plaf-plwrk,    " ����
        matnr     type plaf-matnr,    " ��������
        maktx     type makt-maktx,    " ������������
        gsmng     type plaf-gsmng,    " ��������
        idnrk     type resb-matnr,    " �������
        ojtxb     type makt-maktx,    " �������
        bdmng     type resb-bdmng,    " ��������
        meins     type resb-meins,    " ��λ
        labst     type mard-labst,    " �������
        slabs     type mkol-slabs,    " ���ϸ�Ʒ����
        labql     type mard-labst,    " ���Ƿ��
        menge     type ekpo-menge,    " PO����
        eindt     type eket-eindt,    " PO������������
        wmsdj     type ekpo-menge,    " WMS������
        wmsfp     type ekpo-menge,    " WMS���ϸ�Ʒ
        poqli     type ekpo-menge,    " POǷ��
        prmng     type ekpo-menge,    " PRδתPO����
        ekgrp     type ekko-ekgrp,    " �ɹ���
        lifnr(20) type c,             " ��Ӧ��
        qtmng     type ekpo-menge,    " ������������
      end of ty_out.
data:it_out type standard table of ty_out,
     lt_out type standard table of ty_out,
     wa_out type ty_out.

types:begin of ty_total,
        psttr type plaf-psttr,    " �ƻ�������ʼ����
        pedtr type plaf-pedtr,    " �ƻ������������
        plnum type plaf-plnum,    " �ƻ�������
        plwrk type plaf-plwrk,    " ����
        matnr type plaf-matnr,    " ��������
        maktx type makt-maktx,    " ������������
        gsmng type plaf-gsmng,    " ��������
        okmng type plaf-gsmng,    " ������������������
        yesno type c,             " �Ƿ�����
        eindt type eket-eindt,    " PO������������
      end of ty_total.
data:it_total type standard table of ty_total,
     wa_total type ty_total.
*----------------------------------------------------------------------*
*       Selection-screen
*----------------------------------------------------------------------*
selection-screen:function key 1.
selection-screen:function key 2.

selection-screen begin of block bk1 with frame title text-001.
parameters:p_fname type string.
selection-screen end of block bk1.

selection-screen begin of block bk2 with frame title text-002.
parameters:p_werks type marc-werks obligatory default '1112'.
select-options:s_psttr for plaf-psttr default sy-datum,
               s_idnrk for plaf-matnr.
selection-screen end of block bk2.

selection-screen begin of block bk3 with frame title text-003.
parameters:p_rad1 radiobutton group g1 user-command c1 default 'X',
           p_rad2 radiobutton group g1.
parameters:p_total as checkbox.
selection-screen end of block bk3.

initialization.

at selection-screen on value-request for p_fname.

  perform frm_setf4_fname.

at selection-screen output.

  perform frm_check_screen.

at selection-screen.

  perform frm_run_screen.

start-of-selection.

*&---------------------------------------------------------------------*
*&      Form  FRM_SETF4_FNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form frm_setf4_fname .
  data: lt_file type filetable,
        ls_file type file_table,
        l_rc    type i.
  call method cl_gui_frontend_services=>file_open_dialog
    exporting
      default_extension       = '.xls'
      file_filter             = '.xls'
    changing
      file_table              = lt_file
      rc                      = l_rc
    exceptions
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      others                  = 5.
  if sy-subrc eq 0.
    read table lt_file into ls_file index 1.
    p_fname = ls_file.
  endif.
endform.                    " FRM_SETF4_FNAME
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_check_screen .
  loop at screen.
    if screen-name cs 'p_fname'.
      if p_rad1 = 'X'.
        screen-input = 0.
        screen-invisible = 1.
      elseif p_rad2 = 'X'.
        screen-input = 1.
        screen-invisible = 0.
      endif.
      modify screen.
    endif.

    if screen-name cs 's_psttr' or screen-name cs 's_idnrk' or screen-name cs 'p_werks'.
      if p_rad2 = 'X'.
        screen-input = 0.
        screen-invisible = 1.
      elseif p_rad1 = 'X'.
        screen-input = 1.
        screen-invisible = 0.
      endif.
      modify screen.
    endif.
  endloop.
  if p_rad1 = 'X'.
    clear:sscrfields-functxt_01.
  else.
    sscrfields-functxt_01 = c_funtxt_01.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_run_screen .
  case sscrfields-ucomm.
    when 'FC01'.

      perform frm_downlaod_template.

    when 'ONLI'.

      if p_rad1 = 'X'.

        perform frm_get_data.

        perform frm_process_data.

        if p_total = 'X'.

          perform frm_show_totaldata.

        else.

          perform frm_show_data.

        endif.

      else.

        perform frm_check_filename.

        perform frm_upload_file.

        perform frm_check_data.

        if g_error = 'X'.

          perform frm_show_error_list.

        else.

          perform frm_get_data.

          perform frm_process_data.

          if p_total = 'X'.

            perform frm_show_totaldata.

          else.

            perform frm_show_data.

          endif.

        endif.

      endif.

    when others.

  endcase.
endform.
*&---------------------------------------------------------------------*
*& Form FRM_DOWNLAOD_TEMPLATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_downlaod_template .
  data: l_filename   type string,
        l_name       type string,
        l_path       type string,
        l_fullpath   type string,
        l_destinaton type rlgrap-filename,
        l_objid      type w3objid,
        l_object     type wwwdatatab,
        l_rc         type sy-subrc.
  l_name = 'ģ�������ƻ�'.
  call method cl_gui_frontend_services=>file_save_dialog
    exporting
      window_title         = 'Download Template'
      default_extension    = 'xls'
      default_file_name    = l_name
      file_filter          = '.xls'
    changing
      filename             = l_filename
      path                 = l_path
      fullpath             = l_fullpath
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      others               = 4.
  if sy-subrc = 0.
    l_objid = 'ZMMR014'.
*   ȡ�ô洢��������
    select single *
      into corresponding fields of l_object
      from wwwdata
     where srtf2 = 0
       and relid = 'MI'
       and objid = l_objid.

    if sy-subrc ne 0.
      message '����ʧ�ܣ����������룺SMW0�ϴ���Ӧģ��' type 'E'.
    endif.

    l_destinaton = l_fullpath.

    call function 'DOWNLOAD_WEB_OBJECT'
      exporting
        key         = l_object
        destination = l_destinaton
      importing
        rc          = l_rc.
    if l_rc ne 0.
      message '����ʧ�ܣ�����ϵ����Ա' type 'E'.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_get_data .
  call function 'SPBT_INITIALIZE'
    exporting
      group_name                     = serv_group
* IMPORTING
*     MAX_PBT_WPS                    =
*     FREE_PBT_WPS                   =
    exceptions
      invalid_group_name             = 1
      internal_error                 = 2
      pbt_env_already_initialized    = 3
      currently_no_resources_avail   = 4
      no_pbt_resources_found         = 5
      cant_init_different_pbt_groups = 6
      others                         = 7.

  loop at s_psttr.
    gt_psttr-sign   = s_psttr-sign.
    gt_psttr-option = s_psttr-option.
    gt_psttr-low    = s_psttr-low.
    gt_psttr-high   = s_psttr-high.
    append gt_psttr.
  endloop.

  loop at s_idnrk.
    gt_matnr-sign   = s_idnrk-sign.
    gt_matnr-option = s_idnrk-option.
    gt_matnr-low    = s_idnrk-low.
    gt_matnr-high   = s_idnrk-high.
    append gt_matnr.
  endloop.

  call function 'ZMMR014_GET_STOCK'
    starting new task 'FM1TASK' destination in group serv_group
    performing sub_fm1_back on end of task
    exporting
      p_werks   = p_werks
    tables
      gt_mkol   = gt_mkol
      gt_mard   = gt_mard
      gt_mkol_f = gt_mkol_f
      gt_mard_f = gt_mard_f
      s_matnr   = gt_matnr.

  call function 'ZMMR014_GET_PO'
    starting new task 'FM2TASK' destination in group serv_group
    performing sub_fm2_back on end of task
    exporting
      p_werks = p_werks
    tables
      gt_ekpo = gt_ekpo
      s_matnr = gt_matnr.

  call function 'ZMMR014_GET_RESB'
    starting new task 'FM3TASK' destination in group serv_group
    performing sub_fm3_back on end of task
    exporting
      p_werks = p_werks
    tables
      gt_resb = gt_resb
      gt_plan = gt_plan
      s_psttr = gt_psttr
      s_matnr = gt_matnr.

  wait until st_flag = 'X' and po_flag = 'X' and re_flag = 'X'.
endform.

form sub_fm1_back using  name.
  receive results from function 'ZMMR014_GET_STOCK'
      tables
        gt_mard   = gt_mard
        gt_mkol   = gt_mkol
        gt_mkol_f = gt_mkol_f
        gt_mard_f = gt_mard_f.
  st_flag = 'X'.
endform.                    "sub_fm1_back

form sub_fm2_back using  name.
  receive results from function 'ZMMR014_GET_PO'
      tables
        gt_ekpo = gt_ekpo.
  po_flag = 'X'.
endform.                    "sub_fm1_back

form sub_fm3_back using  name.
  receive results from function 'ZMMR014_GET_RESB'
      tables
        gt_resb = gt_resb.
  re_flag = 'X'.
endform.                    "sub_fm1_back
*&---------------------------------------------------------------------*
*& Form FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_process_data .
  data:p_menge type menge_d,
       p_lifnr type lifnr,
       p_pstyp type ekpo-pstyp,
       p_qtmng type menge_d,
       p_eindt type datum.

  sort gt_resb by psttr plnum.
  loop at gt_resb into wa_resb.
    clear:wa_mard,wa_mkol,wa_mard_f,wa_mkol_f,p_menge.
    wa_out-psttr = wa_resb-psttr.                              " �ƻ�������ʼ����
    wa_out-pedtr = wa_resb-pedtr.                              " �ƻ������������
    wa_out-plnum = wa_resb-plnum.                              " �ƻ�������
    wa_out-plwrk = wa_resb-plwrk.                              " ����
    wa_out-matnr = wa_resb-matnr.                              " ��������
    wa_out-maktx = wa_resb-maktx.                              " ������������
    wa_out-gsmng = wa_resb-gsmng.                              " ��������
    wa_out-idnrk = wa_resb-idnrk.                              " �������
    wa_out-ojtxb = wa_resb-ojtxb.                              " �������
    wa_out-bdmng = wa_resb-bdmng.                              " ��������
    wa_out-meins = wa_resb-meins.                              " ��λ

    if wa_resb-sobsl = ''.                                     " �������  && ���ϸ�Ʒ����
      read table gt_mard into wa_mard with key matnr = wa_out-idnrk binary search.
      wa_out-labst = wa_mard-labst.

      read table gt_mard_f into wa_mard_f with key matnr = wa_out-idnrk binary search.
      wa_out-slabs = wa_mard_f-labst.

      p_pstyp = ''.
    else.
      read table gt_mkol into wa_mkol with key matnr = wa_out-idnrk binary search.
      wa_out-labst = wa_mkol-slabs.

      read table gt_mkol_f into wa_mkol_f with key matnr = wa_out-idnrk binary search.
      wa_out-slabs = wa_mkol_f-slabs.

      p_pstyp = 'K'.
    endif.

    if wa_out-bdmng > wa_out-labst.
      wa_out-labql = wa_out-bdmng - wa_out-labst.               " ���Ƿ��
      wa_out-qtmng = wa_out-labst / ( wa_out-bdmng / wa_out-gsmng ).
      p_menge      = wa_out-labql.
      read table gt_ekpo assigning <ls_ekpo> with key matnr = wa_out-idnrk pstyp = p_pstyp binary search.
      if sy-subrc = 0.
        loop at gt_ekpo assigning <ls_ekpo> where matnr = wa_out-idnrk and pstyp = p_pstyp.
          wa_out-menge = wa_out-menge + <ls_ekpo>-menge.        " PO����
          wa_out-ekgrp = <ls_ekpo>-ekgrp.                       " �ɹ���
          if wa_out-lifnr cs <ls_ekpo>-lifnr.
          else.
            if wa_out-lifnr is initial.
              wa_out-lifnr = <ls_ekpo>-lifnr.
            else.
              wa_out-lifnr = wa_out-lifnr && '/' && <ls_ekpo>-lifnr.
            endif.
          endif.
          if p_menge > 0.
            wa_out-eindt      = <ls_ekpo>-eindt.                " PO������������
            if <ls_ekpo>-menge > p_menge.
              <ls_ekpo>-menge = <ls_ekpo>-menge - p_menge.
              p_menge = 0.
            else.
              p_menge = p_menge - <ls_ekpo>-menge.
              delete gt_ekpo.
            endif.
          endif.
        endloop.
        if p_menge > 0.
          wa_out-poqli = p_menge.                               " POǷ��
          wa_out-prmng = p_menge.                               " PRδתPO����
          wa_out-eindt = '99991231'.
        endif.
      else.
        wa_out-menge = 0.                                       " PO����
        wa_out-eindt = '99991231'.                              " PO������������
        wa_out-poqli = p_menge.                                 " POǷ��
        wa_out-prmng = p_menge.                                 " PRδתPO����
        wa_out-ekgrp = ''.                                      " �ɹ���
        wa_out-lifnr = ''.                                      " ��Ӧ��
      endif.
    else.
      wa_out-labql = 0.
      if wa_resb-sobsl = ''.                                    " ���Ƿ��
        wa_mard-labst = wa_mard-labst - wa_out-bdmng.
        modify gt_mard from wa_mard transporting labst where matnr = wa_out-idnrk.
      else.
        wa_mkol-slabs = wa_mkol-slabs - wa_out-bdmng.
        modify gt_mkol from wa_mkol transporting slabs where matnr = wa_out-idnrk.
      endif.
      wa_out-qtmng = wa_out-gsmng.                              " ��������
    endif.

*    wa_out-wmsdj = wa_resb- "               type ekpo-menge,    " WMS������
*    wa_out-wmsfp = wa_resb- "               type ekpo-menge,    " WMS���ϸ�Ʒ
    at new plnum.
      clear:p_qtmng,p_eindt.
      p_qtmng = wa_out-qtmng.
    endat.
    if wa_out-qtmng < p_qtmng.
      p_qtmng = wa_out-qtmng.
    endif.
    if wa_out-eindt > p_eindt.
      p_eindt = wa_out-eindt.
    endif.

    at end of plnum.
      wa_total-psttr = wa_out-psttr.
      wa_total-pedtr = wa_out-pedtr.
      wa_total-plnum = wa_out-plnum.
      wa_total-plwrk = wa_out-plwrk.
      wa_total-matnr = wa_out-matnr.
      wa_total-maktx = wa_out-maktx.
      wa_total-gsmng = wa_out-gsmng.
      wa_total-okmng = p_qtmng.
      if wa_total-okmng = wa_total-gsmng.
        wa_total-yesno = '��'.
      else.
        wa_total-yesno = '��'.
      endif.
      wa_total-eindt = p_eindt.
      append wa_total to it_total.
      clear wa_total.
    endat.
    append wa_out to it_out.
    clear wa_out.
  endloop.
endform.
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_FILENAME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_check_filename .
  data: l_result type abap_bool.
  if p_fname is initial.
    set cursor field 'P_FNAME'.
    message e055(00).
  else.
    call method cl_gui_frontend_services=>file_exist
      exporting
        file                 = p_fname
      receiving
        result               = l_result
      exceptions
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        others               = 5.
    if l_result is initial.
      message e173(57) with p_fname.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form FRM_UPLOAD_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_upload_file .
  data: l_filename  type rlgrap-filename,
        lt_data_tab type truxs_t_text_data.
  call function 'GUI_UPLOAD'
    exporting
      filename                = p_fname
      filetype                = 'ASC'
      has_field_separator     = cl_abap_char_utilities=>horizontal_tab
    tables
      data_tab                = lt_data_tab
    exceptions
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
      others                  = 17.
  if sy-subrc eq 0.
    l_filename = p_fname.
    refresh:gt_excel[].
    call function 'TEXT_CONVERT_XLS_TO_SAP'
      exporting
        i_field_seperator    = cl_abap_char_utilities=>horizontal_tab
*       i_line_header        = abap_true
        i_tab_raw_data       = lt_data_tab
        i_filename           = l_filename
      tables
        i_tab_converted_data = gt_excel
      exceptions
        conversion_failed    = 1
        others               = 2.
  endif.
  delete gt_excel index 1.
endform.
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_check_data.
  data:p_matnr type matnr,
       p_psttr type datum,
       p_pedtr type datum,
       wa_marc type marc.
  clear:g_error.
  loop at gt_excel into ga_excel.
    p_matnr = ga_excel-matnr.
    p_psttr = ga_excel-psttr.
    p_pedtr = ga_excel-pedtr.
    call function 'CONVERSION_EXIT_MATN1_INPUT'
      exporting
        input        = p_matnr
      importing
        output       = p_matnr
      exceptions
        length_error = 1
        others       = 2.
    ga_excel-matnr = p_matnr.
    select single * into wa_marc from marc where werks = ga_excel-plwrk and matnr = p_matnr.
    if sy-subrc ne 0.
      ga_excel-message = '�������ϲ�����'.
      ga_excel-eflag   = 'X'.
      ga_excel-zicon   = '@5C@'.
      g_error = 'X'.
    endif.

    if ga_excel-gsmng <= 0.
      ga_excel-message = 'û�мƻ�����'.
      ga_excel-eflag   = 'X'.
      ga_excel-zicon   = '@5C@'.
      g_error = 'X'.
    endif.

    if p_psttr < sy-datum or p_pedtr < sy-datum.
      ga_excel-message = '�ƻ���ʼ���ڻ���������ڹ�ȥ'.
      ga_excel-eflag   = 'X'.
      ga_excel-zicon   = '@5C@'.
      g_error = 'X'.
    endif.
    modify gt_excel from ga_excel transporting eflag zicon message.

    move-corresponding ga_excel to wa_plan.
    append wa_plan to gt_plan.
  endloop.

  if gt_plan[] is initial.
    message e001(00) with 'û�е���ƻ�����'.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form FRM_SHOW_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_show_data .
  refresh:gt_fieldcat[].
  mcr_fieldcat 'PSTTR' '�ƻ�������ʼ����' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PEDTR' '�ƻ������������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PLNUM' '�ƻ�������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PLWRK' '����' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MATNR' '��������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MAKTX' '������������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'GSMNG' '��������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'IDNRK' '�������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'OJTXB' '�������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'BDMNG' '��������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MEINS' '��λ' '' '' '' '' '' '' ''.
  mcr_fieldcat 'LABST' '�������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'SLABS' '���ϸ�Ʒ����' '' '' '' '' '' '' ''.
  mcr_fieldcat 'LABQL' '���Ƿ��' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MENGE' 'PO����' '' '' '' '' '' '' ''.
  mcr_fieldcat 'EINDT' 'PO������������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'WMSDJ' 'WMS������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'WMSFP' 'WMS���ϸ�Ʒ' '' '' '' '' '' '' ''.
  mcr_fieldcat 'POQLI' 'POǷ��' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PRMNG' 'PRδתPO����' '' '' '' '' '' '' ''.
  mcr_fieldcat 'EKGRP' '�ɹ���' '' '' '' '' '' '' ''.
  mcr_fieldcat 'LIFNR' '��Ӧ��' '' '' '' '' '' '' ''.

* ��layout��ֵ
  clear gs_layout_lvc.
  gs_layout_lvc-cwidth_opt = abap_true.
  gs_layout_lvc-zebra      = abap_true.
  gs_layout_lvc-no_rowmark = abap_true.

  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
      i_callback_program = sy-repid
*     i_callback_pf_status_set = 'FRM_PF_STATUS_SET_ALV'
*     i_callback_user_command = 'FRM_USER_COMMAND'
      it_fieldcat_lvc    = gt_fieldcat
      is_layout_lvc      = gs_layout_lvc
      i_save             = 'A'
    tables
      t_outtab           = it_out
    exceptions
      program_error      = 1
      others             = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form FRM_SHOW_ERROR_LIST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_show_error_list .
  refresh:gt_fieldcat[].

  mcr_fieldcat 'ZICON'   '��ʶ' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PLWRK'   '����' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MATNR'   '��������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'GSMNG'   '��������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PSTTR'   '�ƻ���ʼ����' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PEDTR'   '�ƻ��������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MESSAGE' '��Ϣ' '' '' '' '' '' '' ''.

* ��layout��ֵ
  clear gs_layout_lvc.
  gs_layout_lvc-cwidth_opt = abap_true.
  gs_layout_lvc-zebra      = abap_true.
  gs_layout_lvc-no_rowmark = abap_true.

  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
      i_callback_program = sy-repid
*     i_callback_pf_status_set = 'FRM_PF_STATUS_SET_ALV'
*     i_callback_user_command = 'FRM_USER_COMMAND'
      it_fieldcat_lvc    = gt_fieldcat
      is_layout_lvc      = gs_layout_lvc
      i_save             = 'A'
    tables
      t_outtab           = gt_excel
    exceptions
      program_error      = 1
      others             = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form FRM_SHOW_TOTALDATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_show_totaldata .
  refresh:gt_fieldcat[].
  mcr_fieldcat 'PSTTR'   '�ƻ�������ʼ����' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PEDTR'   '�ƻ������������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PLNUM'   '�ƻ�������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PLWRK'   '����' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MATNR'   '��������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MAKTX'   '������������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'GSMNG'   '��������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'OKMNG'   '������������������' '' '' '' '' '' '' ''.
  mcr_fieldcat 'YESNO'   '�Ƿ�����' '' '' '' '' '' '' ''.
  mcr_fieldcat 'EINDT'   'PO������������' '' '' '' '' '' '' ''.

* ��layout��ֵ
  clear gs_layout_lvc.
  gs_layout_lvc-cwidth_opt = abap_true.
  gs_layout_lvc-zebra      = abap_true.
  gs_layout_lvc-no_rowmark = abap_true.

  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
      i_callback_program = sy-repid
*     i_callback_pf_status_set = 'FRM_PF_STATUS_SET_ALV'
*     i_callback_user_command = 'FRM_USER_COMMAND'
      it_fieldcat_lvc    = gt_fieldcat
      is_layout_lvc      = gs_layout_lvc
      i_save             = 'A'
    tables
      t_outtab           = it_total
    exceptions
      program_error      = 1
      others             = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.
  endif.
endform.