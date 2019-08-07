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
constants:c_funtxt_01 type rsfunc_txt value '模板下载'.

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
        knumv        type string,    " 顺序
        plwrk        type string,    " 工厂
        matnr        type string,    " 整车编码
        gsmng        type string,    " 整车数量
        psttr        type string,    " 计划订单开始日期
        pedtr        type string,    " 计划订单完成日期
        eflag        type c,
        zicon(4)     type c,
        message(100) type c,
      end of ty_excel.
data:gt_excel type standard table of ty_excel.
data:ga_excel type ty_excel.

types:begin of ty_out,
        psttr     type plaf-psttr,    " 计划订单开始日期
        pedtr     type plaf-pedtr,    " 计划订单完成日期
        plnum     type plaf-plnum,    " 计划订单号
        plwrk     type plaf-plwrk,    " 工厂
        matnr     type plaf-matnr,    " 整车编码
        maktx     type makt-maktx,    " 整车编码描述
        gsmng     type plaf-gsmng,    " 整车数量
        idnrk     type resb-matnr,    " 组件编码
        ojtxb     type makt-maktx,    " 组件描述
        bdmng     type resb-bdmng,    " 需求数量
        meins     type resb-meins,    " 单位
        labst     type mard-labst,    " 库存数量
        slabs     type mkol-slabs,    " 不合格品数量
        labql     type mard-labst,    " 库存欠料
        menge     type ekpo-menge,    " PO数量
        eindt     type eket-eindt,    " PO最早满足日期
        wmsdj     type ekpo-menge,    " WMS待检验
        wmsfp     type ekpo-menge,    " WMS不合格品
        poqli     type ekpo-menge,    " PO欠料
        prmng     type ekpo-menge,    " PR未转PO数量
        ekgrp     type ekko-ekgrp,    " 采购组
        lifnr(20) type c,             " 供应商
        qtmng     type ekpo-menge,    " 整车齐套数量
      end of ty_out.
data:it_out type standard table of ty_out,
     lt_out type standard table of ty_out,
     wa_out type ty_out.

types:begin of ty_total,
        psttr type plaf-psttr,    " 计划订单开始日期
        pedtr type plaf-pedtr,    " 计划订单完成日期
        plnum type plaf-plnum,    " 计划订单号
        plwrk type plaf-plwrk,    " 工厂
        matnr type plaf-matnr,    " 整车编码
        maktx type makt-maktx,    " 整车编码描述
        gsmng type plaf-gsmng,    " 整车数量
        okmng type plaf-gsmng,    " 可满足整车齐套数量
        yesno type c,             " 是否满足
        eindt type eket-eindt,    " PO最早满足日期
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
  l_name = '模拟生产计划'.
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
*   取得存储对象数据
    select single *
      into corresponding fields of l_object
      from wwwdata
     where srtf2 = 0
       and relid = 'MI'
       and objid = l_objid.

    if sy-subrc ne 0.
      message '下载失败，请用事务码：SMW0上传相应模板' type 'E'.
    endif.

    l_destinaton = l_fullpath.

    call function 'DOWNLOAD_WEB_OBJECT'
      exporting
        key         = l_object
        destination = l_destinaton
      importing
        rc          = l_rc.
    if l_rc ne 0.
      message '下载失败，请联系管理员' type 'E'.
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
    wa_out-psttr = wa_resb-psttr.                              " 计划订单开始日期
    wa_out-pedtr = wa_resb-pedtr.                              " 计划订单完成日期
    wa_out-plnum = wa_resb-plnum.                              " 计划订单号
    wa_out-plwrk = wa_resb-plwrk.                              " 工厂
    wa_out-matnr = wa_resb-matnr.                              " 整车编码
    wa_out-maktx = wa_resb-maktx.                              " 整车编码描述
    wa_out-gsmng = wa_resb-gsmng.                              " 整车数量
    wa_out-idnrk = wa_resb-idnrk.                              " 组件编码
    wa_out-ojtxb = wa_resb-ojtxb.                              " 组件描述
    wa_out-bdmng = wa_resb-bdmng.                              " 需求数量
    wa_out-meins = wa_resb-meins.                              " 单位

    if wa_resb-sobsl = ''.                                     " 库存数量  && 不合格品数量
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
      wa_out-labql = wa_out-bdmng - wa_out-labst.               " 库存欠料
      wa_out-qtmng = wa_out-labst / ( wa_out-bdmng / wa_out-gsmng ).
      p_menge      = wa_out-labql.
      read table gt_ekpo assigning <ls_ekpo> with key matnr = wa_out-idnrk pstyp = p_pstyp binary search.
      if sy-subrc = 0.
        loop at gt_ekpo assigning <ls_ekpo> where matnr = wa_out-idnrk and pstyp = p_pstyp.
          wa_out-menge = wa_out-menge + <ls_ekpo>-menge.        " PO数量
          wa_out-ekgrp = <ls_ekpo>-ekgrp.                       " 采购组
          if wa_out-lifnr cs <ls_ekpo>-lifnr.
          else.
            if wa_out-lifnr is initial.
              wa_out-lifnr = <ls_ekpo>-lifnr.
            else.
              wa_out-lifnr = wa_out-lifnr && '/' && <ls_ekpo>-lifnr.
            endif.
          endif.
          if p_menge > 0.
            wa_out-eindt      = <ls_ekpo>-eindt.                " PO最早满足日期
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
          wa_out-poqli = p_menge.                               " PO欠料
          wa_out-prmng = p_menge.                               " PR未转PO数量
          wa_out-eindt = '99991231'.
        endif.
      else.
        wa_out-menge = 0.                                       " PO数量
        wa_out-eindt = '99991231'.                              " PO最早满足日期
        wa_out-poqli = p_menge.                                 " PO欠料
        wa_out-prmng = p_menge.                                 " PR未转PO数量
        wa_out-ekgrp = ''.                                      " 采购组
        wa_out-lifnr = ''.                                      " 供应商
      endif.
    else.
      wa_out-labql = 0.
      if wa_resb-sobsl = ''.                                    " 库存欠料
        wa_mard-labst = wa_mard-labst - wa_out-bdmng.
        modify gt_mard from wa_mard transporting labst where matnr = wa_out-idnrk.
      else.
        wa_mkol-slabs = wa_mkol-slabs - wa_out-bdmng.
        modify gt_mkol from wa_mkol transporting slabs where matnr = wa_out-idnrk.
      endif.
      wa_out-qtmng = wa_out-gsmng.                              " 齐套数量
    endif.

*    wa_out-wmsdj = wa_resb- "               type ekpo-menge,    " WMS待检验
*    wa_out-wmsfp = wa_resb- "               type ekpo-menge,    " WMS不合格品
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
        wa_total-yesno = '是'.
      else.
        wa_total-yesno = '否'.
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
      ga_excel-message = '工厂物料不存在'.
      ga_excel-eflag   = 'X'.
      ga_excel-zicon   = '@5C@'.
      g_error = 'X'.
    endif.

    if ga_excel-gsmng <= 0.
      ga_excel-message = '没有计划数量'.
      ga_excel-eflag   = 'X'.
      ga_excel-zicon   = '@5C@'.
      g_error = 'X'.
    endif.

    if p_psttr < sy-datum or p_pedtr < sy-datum.
      ga_excel-message = '计划开始日期或结束日期在过去'.
      ga_excel-eflag   = 'X'.
      ga_excel-zicon   = '@5C@'.
      g_error = 'X'.
    endif.
    modify gt_excel from ga_excel transporting eflag zicon message.

    move-corresponding ga_excel to wa_plan.
    append wa_plan to gt_plan.
  endloop.

  if gt_plan[] is initial.
    message e001(00) with '没有导入计划数据'.
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
  mcr_fieldcat 'PSTTR' '计划订单开始日期' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PEDTR' '计划订单完成日期' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PLNUM' '计划订单号' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PLWRK' '工厂' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MATNR' '整车编码' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MAKTX' '整车编码描述' '' '' '' '' '' '' ''.
  mcr_fieldcat 'GSMNG' '整车数量' '' '' '' '' '' '' ''.
  mcr_fieldcat 'IDNRK' '组件编码' '' '' '' '' '' '' ''.
  mcr_fieldcat 'OJTXB' '组件描述' '' '' '' '' '' '' ''.
  mcr_fieldcat 'BDMNG' '需求数量' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MEINS' '单位' '' '' '' '' '' '' ''.
  mcr_fieldcat 'LABST' '库存数量' '' '' '' '' '' '' ''.
  mcr_fieldcat 'SLABS' '不合格品数量' '' '' '' '' '' '' ''.
  mcr_fieldcat 'LABQL' '库存欠料' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MENGE' 'PO数量' '' '' '' '' '' '' ''.
  mcr_fieldcat 'EINDT' 'PO最早满足日期' '' '' '' '' '' '' ''.
  mcr_fieldcat 'WMSDJ' 'WMS待检验' '' '' '' '' '' '' ''.
  mcr_fieldcat 'WMSFP' 'WMS不合格品' '' '' '' '' '' '' ''.
  mcr_fieldcat 'POQLI' 'PO欠料' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PRMNG' 'PR未转PO数量' '' '' '' '' '' '' ''.
  mcr_fieldcat 'EKGRP' '采购组' '' '' '' '' '' '' ''.
  mcr_fieldcat 'LIFNR' '供应商' '' '' '' '' '' '' ''.

* 给layout赋值
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

  mcr_fieldcat 'ZICON'   '标识' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PLWRK'   '工厂' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MATNR'   '整车编码' '' '' '' '' '' '' ''.
  mcr_fieldcat 'GSMNG'   '整车数量' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PSTTR'   '计划开始日期' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PEDTR'   '计划完成日期' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MESSAGE' '消息' '' '' '' '' '' '' ''.

* 给layout赋值
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
  mcr_fieldcat 'PSTTR'   '计划订单开始日期' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PEDTR'   '计划订单完成日期' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PLNUM'   '计划订单号' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PLWRK'   '工厂' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MATNR'   '整车编码' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MAKTX'   '整车编码描述' '' '' '' '' '' '' ''.
  mcr_fieldcat 'GSMNG'   '整车数量' '' '' '' '' '' '' ''.
  mcr_fieldcat 'OKMNG'   '可满足整车齐套数量' '' '' '' '' '' '' ''.
  mcr_fieldcat 'YESNO'   '是否满足' '' '' '' '' '' '' ''.
  mcr_fieldcat 'EINDT'   'PO最早满足日期' '' '' '' '' '' '' ''.

* 给layout赋值
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