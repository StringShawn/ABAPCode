*&---------------------------------------------------------------------*
*& Program ID    :  ZMM_RP044
*& Program Text  : 直发流程后续单据生成平台
*& Overview      : 直发流程后续单据生成平台
*& Created by    : HANDLM
*& Creation Date : 2020/03/23
*&---------------------------------------------------------------------*
*& Changing No   :
*& Updated by    :
*& Update date   :
*& Changed Item Description :
*& 请求号        :
*&---------------------------------------------------------------------*

report  zmm_rp044.
*-----------------------------------------------------------------------
* GLOBAL DATA DECLARE
*-----------------------------------------------------------------------
"全局变量声明部分
type-pools:slis,icon.
tables:ztsd001.
types:begin of ty_alv,
      box     type c,"选择框
      vbeln   type ztsd001-vbeln,"销售订单
      posnr   type ztsd001-posnr,"销售凭证项目
      light   type char4,"状态灯
      zqsid   type ztsd001-zqsid,"客户签收单号
      zposnr  type ztsd001-zposnr,"行项目
      zqsdate type ztsd001-zqsdate,"创建日期
      zcreater type ztsd001-zcreater,"创建者
      kunnr   type ztsd001-kunnr,"客户编号
      matnr   type ztsd001-matnr,"物料
      maktx   type ztsd001-maktx,"物料描述
      kwmeng  type ztsd001-kwmeng,"销售订单数量
      lifnr   type ztsd001-lifnr,"供应商编号
      name1   type ztsd001-name1,"供应商名称
      ebeln   type ztsd001-ebeln,"采购订单
      ebelp   type ztsd001-ebelp,"采购订单行项目
      menge   type ztsd001-menge,"采购订单数量
      licha   type ztsd001-licha,"供应商批次
      zdate   type ztsd001-zdate,"效期
      zmenge  type ztsd001-zmenge,"数量
      zdhwd   type ztsd001-zdhwd,"到货温度
      zgzdate type ztsd001-zgzdate,"过账日期
      zmenge1 type ztsd001-zmenge1,"已入库数量
      charg   type ztsd001-charg,"批号
      mblnr   type ztsd001-mblnr,"入库物料凭证
      zvbeln  type ztsd001-zvbeln,"交货单
      zmblnr  type ztsd001-zmblnr,"出库物料凭证
      lfimg   type ztsd001-lfimg,"过账数量
      zmsg    type bapi_msg,"消息
      werks   type ekpo-werks,"工厂
      vstel   type vbap-vstel,"装运点
  end of ty_alv.
data:gt_alv type table of ty_alv,
     gs_alv type ty_alv.
"转存中间表
data:lt_data type table of ty_alv,
     lw_data type ty_alv.
types:begin of ty_charg,
        matnr type mchb-matnr,
        werks type mchb-werks,
        charg type mchb-charg,
end of ty_charg.
data:gt_charg type table of ty_charg,
      gs_charg type ty_charg.
data:gt_ztsd003 type table of ztsd003,
     gs_ztsd003 type ztsd003.
data:gt_ztsd001 type table of ztsd001,
     gs_ztsd001 type ztsd001.
types:begin of ty_mdbs,
  ebeln type mdbs-ebeln,
  ebelp type mdbs-ebelp,
  matnr type mdbs-matnr,
  menge type mdbs-menge,
  wemng type mdbs-wemng,
  end of ty_mdbs.
data:gt_mdbs type table of ty_mdbs,
     gs_mdbs type ty_mdbs.
"ALV定义
data:gt_fcat    type lvc_t_fcat,
     gs_fcat    type lvc_s_fcat,
     gs_layo    type lvc_s_layo.
"BAPI定义
data: gw_header type bapi2017_gm_head_01,              "过账BAPI-HEAD
      gt_item   type table of bapi2017_gm_item_create, "过账BAPI-ITEM
      gw_item   type bapi2017_gm_item_create,
      gt_return type table of bapiret2,                "返回消息
      gw_return type bapiret2,
      gt_extension type table of bapiparex,
      gw_extension type bapiparex.
data: lc_headret  type bapi2017_gm_head_ret,
      lc_document type bapi2017_gm_head_ret-mat_doc,
      lc_year     type bapi2017_gm_head_ret-doc_year.
"屏幕定义
data:ok_code  type sy-ucomm,
     gv_code  type sy-ucomm,
     gv_zdhwd type ztsd001-zdhwd."到货温度
*-----------------------------------------------------------------------
* 选择屏幕
*-----------------------------------------------------------------------
selection-screen begin of block blk1 with frame .
select-options:s_zqsid    for ztsd001-zqsid ,"客户签收单号
               s_zdate    for ztsd001-zqsdate ,"创建日期
               s_zcreat   for ztsd001-zcreater ,"创建者
               s_vbeln    for ztsd001-vbeln ,"销售订单号
               s_kunnr    for ztsd001-kunnr ,"客户号
               s_ebeln    for ztsd001-ebeln    ,"采购订单号
               s_lifnr    for ztsd001-lifnr."供应商号
selection-screen end of block blk1.
*-----------------------------------------------------------------------
* 主程序逻辑
*-----------------------------------------------------------------------
start-of-selection.
  perform frm_get_data."数据获取
  perform frm_build_layout."格式
  perform frm_build_fieldcat."字段填充
  perform frm_alv_output. "输出
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_get_data .
  "拉取自建表ZTSD001数据时，只拉取对应数据且供应商批次/效期/数量不为空的数据
  select *
       into corresponding fields of table gt_alv
       from ztsd001 inner join ekpo
       on ztsd001~ebeln = ekpo~ebeln
       and ztsd001~ebelp = ekpo~ebelp
       where zqsid in s_zqsid "客户签收单号
        and  zqsdate in s_zdate    "创建日期
        and  zcreater in  s_zcreat "创建者
        and  vbeln in  s_vbeln "销售订单号
        and  ztsd001~kunnr in  s_kunnr "客户号
        and  ztsd001~ebeln in  s_ebeln "采购订单号
        and  lifnr in  s_lifnr "供应商号
        and  licha <> ''"供应商批次
        and  zdate <> ''"效期
        and  zmenge <> ''."数量


  loop at gt_alv into gs_alv.
    "若对应数据的批次号字段有值，状态灯为黄色；若交货单字段有值，状态灯为绿色；其他皆为红色
    if gs_alv-zmblnr is not initial.
      gs_alv-light = icon_green_light.
    elseif gs_alv-charg is not initial.
      gs_alv-light = icon_yellow_light.
    else.
      gs_alv-light = icon_red_light.
    endif.

    gs_alv-zgzdate = sy-datum.

    select single vstel into gs_alv-vstel from vbap where vbeln = gs_alv-vbeln and posnr = gs_alv-posnr.

    modify gt_alv from gs_alv.
  endloop.


  if gt_alv is not initial.

    "批次检查
    select matnr
           werks
           charg
           into corresponding fields of table gt_charg
           from mchb
           for all entries in gt_alv
           where matnr = gt_alv-matnr
           and werks = gt_alv-werks.
    sort gt_charg.
    delete adjacent duplicates from gt_charg  comparing all fields.

    "数量检查
    select ebeln
           ebelp
           matnr
           menge
           wemng
           into corresponding fields of table  gt_mdbs
           from mdbs
           for all entries in gt_alv
           where ebeln = gt_alv-ebeln
           and ebelp = gt_alv-ebelp
           and matnr = gt_alv-matnr.



  endif.
endform.                    " FRM_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_build_layout .
  clear gs_layo.
  gs_layo-cwidth_opt = 'X'.
  gs_layo-zebra      = 'X'.
  gs_layo-box_fname  = 'BOX'.
endform.                    " FRM_BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_build_fieldcat .
  perform add_fieldcat using:'BOX' '复选框' '' '' 'X',
                             'LIGHT' '状态灯' '' '' ''," 状态灯
                            'ZQSID' '客户签收单号' 'ZTSD001' 'ZQSID' ''," 客户签收单号
                            'ZPOSNR' '行项目' 'ZTSD001' 'ZPOSNR' ''," 行项目

                            'ZDHWD' '到货温度' 'ZTSD001' 'ZDHWD' 'X'," 到货温度
                            'ZGZDATE' '过账日期' 'ZTSD001' 'ZGZDATE' 'X'," 过账日期

                            'ZQSDATE' '创建日期' 'ZTSD001' 'ZQSDATE' ''," 创建日期
                            'ZCREATER' '创建者' 'ZTSD001' 'ZCREATER' ''," 创建者
                            'KUNNR' '客户编号' 'ZTSD001' 'KUNNR' ''," 客户编号
                            'VBELN' '销售订单' 'ZTSD001' 'VBELN' ''," 销售订单
                            'POSNR' '销售凭证项目' 'ZTSD001' 'POSNR' ''," 销售凭证项目
                            'MATNR' '物料' 'ZTSD001' 'MATNR' ''," 物料
                            'MAKTX' '物料描述' 'ZTSD001' 'MAKTX' ''," 物料描述
                            'KWMENG' '销售订单数量' 'ZTSD001' 'KWMENG' ''," 销售订单数量
                            'LIFNR' '供应商编号' 'ZTSD001' 'LIFNR' ''," 供应商编号
                            'NAME1' '供应商名称' 'ZTSD001' 'NAME1' ''," 供应商名称
                            'EBELN' '采购订单' 'ZTSD001' 'EBELN' ''," 采购订单
                            'EBELP' '采购订单行项目' 'ZTSD001' 'EBELP' ''," 采购订单行项目
                            'MENGE' '采购订单数量' 'ZTSD001' 'MENGE' ''," 采购订单数量
                            'LICHA' '供应商批次' 'ZTSD001' 'LICHA' ''," 供应商批次
                            'ZDATE' '效期' 'ZTSD001' 'ZDATE' ''," 效期
                            'ZMENGE' '数量' 'ZTSD001' 'ZMENGE' ''," 数量

                            'ZMENGE1' '已入库数量' 'ZTSD001' 'ZMENGE1' ''," 已入库数量
                            'CHARG' '批号' 'ZTSD001' 'CHARG' ''," 批号
                            'MBLNR' '入库物料凭证' 'ZTSD001' 'MBLNR' ''," 入库物料凭证
                            'ZVBELN' '交货单' 'ZTSD001' 'ZVBELN' ''," 交货单
                            'ZMBLNR' '出库物料凭证' 'ZTSD001' 'ZMBLNR' ''," 出库物料凭证
                            'LFIMG' '过账数量' 'ZTSD001' 'LFIMG' ''," 过账数量
                            'ZMSG' '消息' '' '' ''." 消息
endform.                    " FRM_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDCAT
*&---------------------------------------------------------------------*
*       ALV字段填充逻辑
*----------------------------------------------------------------------*
form add_fieldcat  using    value(p_fieldname)
                            value(p_coltext)
                            value(p_ref_table)
                            value(p_ref_field)
                            value(p_edit).
  clear gs_fcat.
  if p_fieldname eq 'BOX'.
    gs_fcat-checkbox = 'X'.
  endif.
  gs_fcat-fieldname = p_fieldname.
  gs_fcat-coltext   = p_coltext.
  gs_fcat-ref_table = p_ref_table.
  gs_fcat-ref_field = p_ref_field.
  gs_fcat-edit      = p_edit.
  append gs_fcat to gt_fcat.
endform.                    "add_fieldcat
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_alv_output .
  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
      i_callback_program       = sy-repid
      i_callback_user_command  = 'FRM_USER_COMMAND'
      i_callback_pf_status_set = 'FRM_USER_STATUS'
      is_layout_lvc            = gs_layo
      it_fieldcat_lvc          = gt_fcat
    tables
      t_outtab                 = gt_alv
    exceptions
      program_error            = 1
      others                   = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " FRM_ALV_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  FRM_USER_STATUS
*&---------------------------------------------------------------------*
*       gui状态
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_user_status using extab type slis_t_extab.
  "设置gui状态
  set pf-status 'STANDARD'.
endform.                    "frm_user_status
*&---------------------------------------------------------------------*
*&      Form  frm_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_user_command using pv_ucomm like sy-ucomm
                             ps_selfield type slis_selfield..

  data: lo_grid type ref to cl_gui_alv_grid,
       lw_stbl type lvc_s_stbl.
  data:lv_flag.
  data:lt_data type table of ty_alv,
       lw_data type ty_alv.
  call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    importing
      e_grid = lo_grid.

  call method lo_grid->check_changed_data
    importing
      e_valid = lv_flag.
  check lv_flag <> ''.

  case pv_ucomm.
    when '&CREATE'."单据生成
      refresh lt_data.
      clear lw_data.
      lt_data = gt_alv.
      delete lt_data where box is initial.
      if lt_data is not initial."检查数据是否选中
        delete  lt_data where  zdhwd is initial.
        if lt_data is initial."检查到货温度是否维护
          message '请维护到货温度' type 'S' display like 'E'.
        else.
          "检查对应的已入库数量/批次号/入库物料凭证/交货单/出库物料凭证/过账数量字段是否为空
          "若都不为空，则不走以下创建逻辑，不执行任何操作
          loop at gt_alv into gs_alv where box is not initial
                                      and  zmenge1 is not initial
                                      and  charg   is not initial
                                      and  mblnr   is not initial
                                      and  zvbeln  is not initial
                                      and  zmblnr  is not initial
                                      and  lfimg   is not initial.
            gs_alv-box = ''.
            modify gt_alv from gs_alv.
            clear gs_alv.
          endloop.
          "检查是否还有数据进行单据创建
          refresh lt_data.
          clear lw_data.
          lt_data = gt_alv.
          delete lt_data where box is initial.
          if lt_data is not initial."检查数据是否选中
            perform frm_create."生成单据
          else.
            message '请选择正确的数据之后再生成单据！' type 'S' display like 'E'.
          endif.
        endif.
      else.
        message '请选中数据之后再生成单据！' type 'S' display like 'E'.
      endif.

    when '&ZDHWD'."到货温度维护
      perform frm_adddata."维护到货温度

    when 'SELECT_ALL'.

      loop at gt_alv into gs_alv.
        gs_alv-box = 'X'.
        modify gt_alv from gs_alv.
      endloop.

    when 'DESELECT'.

      loop at gt_alv into gs_alv.
        gs_alv-box = ''.
        modify gt_alv from gs_alv.
      endloop.

  endcase.

  lw_stbl-row = 'X'.
  lw_stbl-col = 'X'.
  call method lo_grid->refresh_table_display
    exporting
      is_stable      = lw_stbl
      i_soft_refresh = 'X'
    exceptions
      finished       = 1
      others         = 2.
endform.                    "frm_user_command
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_create .
************************************************************
  "整体逻辑：未入库先MIGO入库，入库完成之后创建DN，然后过账外向交货单,如果过账失败，那么也不创建交货单
************************************************************
  data:l_menge type mdbs-menge,"采购订单数量
       l_wemng type mdbs-wemng."已交货数量
  data:l_menge1 type mdbs-menge."剩余可入库数量
  "清空内表，之后要更新自建表
  refresh gt_ztsd003.
  refresh gt_ztsd001.
  "计算可收货数量和自建表的数量是否一致
  loop at gt_alv into gs_alv where box is not initial
    and zmenge1 is initial
    and charg is initial
    and mblnr is initial
    and zmsg is initial.
    loop at gt_mdbs into gs_mdbs
             where ebeln = gs_alv-ebeln
             and ebelp = gs_alv-ebelp
             and matnr = gs_alv-matnr.
      l_menge = l_menge + gs_mdbs-menge.
      l_wemng = l_wemng + gs_mdbs-wemng.
      clear gs_mdbs.
    endloop.
    l_menge1 = l_menge - l_wemng.
    "判断选中数据的数量字段值小于等于剩余可交货数量，即执行下面采购订单入库的逻辑
    "否则写入报错至消息列“此次收货入库数量已超过剩余可收货数量”
    if gs_alv-zmenge > l_menge1.
      gs_alv-zmsg = '此次收货入库数量已超过剩余可收货数量'.
    endif.
    modify gt_alv from gs_alv.
    clear: gs_alv,l_menge,l_wemng,l_menge1.
  endloop.

  "MIGO
  perform frm_migo_bapi.

  "创建外向交货单以及过账
  refresh lt_data.
  lt_data = gt_alv.
  delete lt_data where box is initial.
  sort lt_data by vbeln.
  perform frm_create_dn."创建交货单

  "更新ztsd001数据
  if gt_ztsd001 is not initial.
    modify ztsd001 from table gt_ztsd001.
    commit work and wait.
    refresh gt_ztsd001.
  endif.

  "更新ztsd003数据
  "所有报错数据一次性更新到底表
  if gt_ztsd003 is not initial.
    modify ztsd003 from table gt_ztsd003.
    commit work and wait.
    refresh gt_ztsd003.
  endif.
endform.                    " FRM_CREATE
*&---------------------------------------------------------------------*
*&      Form  FRM_ADDDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_adddata .
  call screen 9001  starting at 10 5 ending at 50 10."调用小屏幕批量维护温度
endform.                    " FRM_ADDDATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9001 output.
  set pf-status '9001'.
  set titlebar '9001'.
endmodule.                 " STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9001 input.
  clear ok_code.
  ok_code = sy-ucomm.
  clear sy-ucomm.
  clear gv_code.
  gv_code = ok_code.
  case gv_code.
    when '&GV_OK'."将温度回填所有选中的条目
      loop at gt_alv into gs_alv where box is not initial.
        gs_alv-zdhwd = gv_zdhwd.
        modify gt_alv from gs_alv.
        clear gs_alv.
      endloop.
      leave to screen 0.
    when '&GV_CL'."离开屏幕
      leave to screen 0.
  endcase.
endmodule.                 " USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*&      Form  FRM_MIGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_migo .
  data: lc_headret  type bapi2017_gm_head_ret,
        lc_document type bapi2017_gm_head_ret-mat_doc,
        lc_year     type bapi2017_gm_head_ret-doc_year.
  data:lw_zmkpf type zmkpf.
  data: line_id  type i.
  data: lc_error    type c.           "过账标识
  data: lc_msg      type c length 220."过账消息
  clear line_id.
  clear gw_item.
  clear gw_header.
  clear lc_headret.
  clear lc_document.
  clear lc_year.
  clear gw_extension.
  clear gw_return.
  refresh gt_item.
  refresh: gt_return.
  refresh: gt_extension.
  add 1 to line_id.
  "bapi行项目字段赋值
  gw_item-line_id    = line_id.
  gw_item-move_type  = '101'.             "移动类型
  gw_item-po_number  = gs_alv-ebeln.   "采购订单号
  gw_item-po_item    = gs_alv-ebelp.   "采购订单行项目
  gw_item-mvt_ind    = 'B'.               "移动标识
  gw_item-entry_qnt  = gs_alv-zmenge.   "数量
  gw_item-quantity   = gs_alv-zmenge.   "数量
  gw_item-stge_loc   = '1005'.   "库存地点
  gw_item-expirydate = gs_alv-zdate.   "货架寿命到期日
  gw_item-batch     = gs_alv-charg."批次
  gw_item-vendrbatch = gs_alv-licha."供应商批次
  append gw_item to gt_item.
  "一个采购订单一个行项目收一次货
  "gw_header-pstng_date = sy-datum.
  gw_header-pstng_date = gs_alv-zgzdate.
  "gw_header-doc_date   = sy-datum.
  gw_header-doc_date   = gs_alv-zgzdate.
  gw_extension-structure =  'ZMKPF_BADI'.
  gw_extension-valuepart1 = gs_alv-zdhwd.
  append gw_extension to gt_extension.
  call function 'BAPI_GOODSMVT_CREATE'   "收货
    exporting
      goodsmvt_header  = gw_header
      goodsmvt_code    = '01'
    importing
      goodsmvt_headret = lc_headret
      materialdocument = lc_document
      matdocumentyear  = lc_year
    tables
      goodsmvt_item    = gt_item
      return           = gt_return
      extensionin      = gt_extension.
  clear:lc_error,lc_msg.
  loop at gt_return into gw_return where type = 'A' or type = 'E'.
    lc_error = 'X'.
    concatenate lc_msg gw_return-message into lc_msg.
  endloop.
  if lc_error = 'X'.
    rollback work.
    "若收货入库失败，不更新对应数据的自建表ZTSD001；将具体问题写入对应数据的ALV消息列，且将具体问题日志写入日志表ZTSD003
    gs_alv-zmsg = lc_msg.
    move-corresponding gs_alv to gs_ztsd003.
    gs_ztsd003-zuser = sy-uname.
    gs_ztsd003-zdate = sy-datum.
    gs_ztsd003-ztime = sy-uzeit.
    gs_ztsd003-zmessage = gs_alv-zmsg.
    append gs_ztsd003 to gt_ztsd003."错误数据写入日志表ZTSD003
    clear gs_ztsd003.
    refresh: gt_item.
  else.
    commit work and wait.
    gs_alv-mblnr = lc_document."物料凭证
    gs_alv-zmenge1 = gs_alv-zmenge."已入库数量
    gs_alv-light = icon_yellow_light."对应数据的批次号字段有值，状态灯为黄色
    move-corresponding gs_alv to gs_ztsd001.
    append gs_ztsd001 to gt_ztsd001.
    clear lw_zmkpf.
    lw_zmkpf-mblnr = lc_document.
    lw_zmkpf-mjahr = lc_year.
    lw_zmkpf-zdhwd = gs_alv-zdhwd.
    modify zmkpf from lw_zmkpf.
    if sy-subrc = 0.
      commit work and wait.
    else.
      rollback work.
    endif.
  endif.
endform.                    " FRM_MIGO
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_DN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_create_dn .
  data: it_so type bapidlvreftosalesorder occurs 0.
  data:ls_data type ty_alv.
  data: it_return type bapiret2 occurs 0.
  data: g_vbeln type likp-vbeln.
  data: w_so type bapidlvreftosalesorder.
  data: lc_error    type c.           "过账标识
  data:lc_flag      type c.           "所选行无法交货
  data: lc_msg      type c length 220."过账消息
  data:gw_data type ty_alv.
  refresh: it_so,it_return.
  clear:g_vbeln.
  "创建DN的时候，同一张SO同一个行项目不能创建在同一张DN里面，要分开

  data:lt_dnx type table of ty_alv,
       lw_dnx type ty_alv.

  loop at lt_data into gs_alv where box is not initial and zmenge1 is not initial and charg is not initial and mblnr is not initial and zmsg is initial.

    move-corresponding gs_alv to lw_data.
    w_so-ref_doc = lw_data-vbeln.
    w_so-ref_item = lw_data-posnr.
    append w_so to it_so.
    clear w_so.
    "一张SO创建一张DN
    at end of vbeln.
      read table lt_data into ls_data with key vbeln = gs_alv-vbeln.
      if ls_data-zvbeln is not initial.
        perform frm_dn_post using ls_data-zvbeln.
      else.
        sort it_so by ref_doc ref_item.
        delete adjacent duplicates from it_so comparing all fields.

        if not lw_data-vstel is initial."lw_data-zgzdate

          call function 'BAPI_OUTB_DELIVERY_CREATE_SLS'
            exporting
              ship_point        = lw_data-vstel
              due_date          = sy-datum
            importing
              delivery          = g_vbeln
            tables
              sales_order_items = it_so
              return            = it_return.

        else.

          call function 'BAPI_OUTB_DELIVERY_CREATE_SLS'
            exporting
              ship_point        = 'S010'
              due_date          = sy-datum
            importing
              delivery          = g_vbeln
            tables
              sales_order_items = it_so
              return            = it_return.

        endif.

        clear: lc_error,lc_msg.
        if g_vbeln is initial."发货失败
          rollback work.
          lc_msg = '交货单创建失败！'.
          loop at it_so into w_so.
            loop at gt_alv into gs_alv where vbeln = w_so-ref_doc and posnr = w_so-ref_item and box is not initial.
              gs_alv-zmsg = lc_msg.
              "将错误数据存到ZTSD003里面
              move-corresponding gs_alv to gs_ztsd003.
              gs_ztsd003-zuser = sy-uname.
              gs_ztsd003-zdate = sy-datum.
              gs_ztsd003-ztime = sy-uzeit.
              gs_ztsd003-zmessage = gs_alv-zmsg.
              append gs_ztsd003 to gt_ztsd003."错误数据写入日志表ZTSD003
              "更新ALV错误信息
              modify gt_alv from gs_alv .
              clear:gs_alv.
            endloop.
          endloop.
          refresh: it_so,it_return.
          clear:g_vbeln.
        else.
          "提交处理
          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = abap_true.
          "当生成发货单成功，进行过账操作
          loop at it_so into w_so.
            "更新ALV数据
            loop at gt_alv into gs_alv where vbeln = w_so-ref_doc and posnr = w_so-ref_item and box is not initial.
*            gs_alv-zmsg  = '交货单创建成功'.
              gs_alv-zvbeln = g_vbeln."交货单
              modify gt_alv from gs_alv.
              move-corresponding gs_alv to gs_ztsd001.
              append gs_ztsd001 to gt_ztsd001.
              clear:gs_alv.
            endloop.
          endloop.

          refresh: it_so,it_return.

          "根据交货单过账
          perform frm_dn_post using g_vbeln.
          clear:g_vbeln.
        endif.
      endif.

    endat.
    clear gs_alv.
  endloop.
endform.                    " FRM_CREATE_DN
*&---------------------------------------------------------------------*
*&      Form  FRM_CHARG_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_ALV_CHARG  text
*----------------------------------------------------------------------*
form frm_charg_check  changing gs_alv_charg.
  data:lt_charg type table of ty_charg.
  "批次生成规则：年份+效期日期+过账日期
  "CONCATENATE sy-datum(2) gs_alv-zdate+4(*) sy-datum+4(*) INTO gs_alv-charg.
  concatenate gs_alv-zgzdate(2) gs_alv-zdate+4(*) gs_alv-zgzdate+4(*) into gs_alv-charg.
  read table gt_charg into gs_charg with key matnr = gs_alv-matnr werks = gs_alv-werks charg = gs_alv_charg.
  if sy-subrc = 0."能抓取到，批次存在，查看添加001的批次是否存在
    clear gs_charg.
    concatenate gs_alv_charg '001' into gs_alv_charg.
    refresh lt_charg.
    lt_charg = gt_charg.
    delete lt_charg where matnr = gs_alv-matnr and werks = gs_alv-werks and charg <> gs_alv_charg.
    if lt_charg is initial."数据为空，现在的这个001的编码已经可用
    else."不为空，数据存在，现在的批次还不能使用
      sort gt_charg by matnr werks charg descending.
      read table gt_charg into gs_charg with key matnr = gs_alv-matnr werks = gs_alv-werks.
      if sy-subrc = 0.
        gs_alv_charg = gs_charg-charg + 1."在现有的最大批次号的基础上加一
      endif.
    endif.
  else."抓取不到，判断当前屏幕里面是否有重复的批次号，有的话，在现有的最大批次号的基础上加1
    read table lt_data into lw_data with key matnr = gs_alv-matnr werks = gs_alv-werks charg = gs_alv_charg.
    if sy-subrc = 0."能获取到，批次存在，查看添加001的批次是否存在
      concatenate gs_alv_charg '001' into gs_alv_charg.
      read table lt_data into lw_data with key matnr = gs_alv-matnr werks = gs_alv-werks charg = gs_alv_charg.
      if sy-subrc <> 0."抓取不到，可以使用
      else."抓取的到，不能使用
        sort lt_data by matnr werks charg descending.
        read table lt_data into lw_data with key matnr = gs_alv-matnr werks = gs_alv-werks.
        if sy-subrc = 0.
          gs_alv_charg = lw_data-charg + 1."在现有的最大批次号的基础上加一
        endif.
      endif.
    endif.
  endif.
endform.                    " FRM_CHARG_CHECK
*&---------------------------------------------------------------------*
*&      Form  FRM_MIGO_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_migo_bapi .
  loop at gt_alv into gs_alv where box is not initial and zmenge1 is initial and charg is initial and mblnr is initial and zmsg is initial.
    refresh lt_data.
    clear lw_data.
    lt_data = gt_alv.
    "批次生成规则：年份+效期日期+过账日期
    perform frm_charg_check changing gs_alv-charg."检查批次号是否存在
    perform frm_migo."MIGO入库.
    modify gt_alv from gs_alv.
    clear gs_alv.
  endloop.
  "更新ztsd001数据
  if gt_ztsd001 is not initial.
    modify ztsd001 from table gt_ztsd001.
    commit work and wait.
    refresh gt_ztsd001.
  endif.
endform.                    " FRM_MIGO_BAPI
*&---------------------------------------------------------------------*
*&      Form  FRM_DN_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_dn_post using  lv_vbeln.
  data: ls_header type bapiobdlvhdrcon,
        ls_header_control type bapiobdlvhdrctrlcon,
        lt_return type table of bapiret2 with header line,
        fieldname type fieldname value '(SAPLV50S)EMKPF-MBLNR'.
  data:l_mblnr type mkpf-mblnr.
  data: lc_msg      type c length 220."过账消息
  clear: ls_header,ls_header_control.
  data:  it_item        type standard table of bapiobdlvitemcon,
         w_item        type bapiobdlvitemcon,
         hdl  like table of bapidlvdeadln with header line,
             "外向交货项目级别控制数据
         it_item_control     type standard table of bapiobdlvitemctrlcon,
         w_item_control     type bapiobdlvitemctrlcon,
        "确认向外交货拣配数据项目等级(SPE)
         it_item_spl    type standard table of /spe/bapiobdlvitemconf,
         w_item_spl    type /spe/bapiobdlvitemconf,
         ls_lips type lips,
         lt_lips type table of lips..

  data: lv_posnr type lips-posnr.
  refresh: lt_return,it_item,it_item_control,it_item_spl,lt_return .
  clear lv_posnr.
  ls_header-deliv_numb = lv_vbeln.
  ls_header_control-deliv_numb = lv_vbeln.
  ls_header_control-post_gi_flg = 'X'.
  "标记更更实际和计划的过账时间， 时间的值在 HEADER_DEADLINES 参数里添加（这个开始没注意看帮助，走了很多弯路，才找到）
  ls_header_control-deliv_date_flg = 'X'.
  ls_header_control-gdsi_date_flg ='X'.

  select * into table lt_lips
    from lips
    where vbeln = lv_vbeln.
  lv_posnr = 900000.
  "
  loop at lt_lips into ls_lips.

    loop at gt_alv into gs_alv where zvbeln = lv_vbeln and box is not initial and vbeln = ls_lips-vgbel and posnr = ls_lips-vgpos.
      add 1 to lv_posnr.
      w_item-deliv_numb = lv_vbeln.
      w_item-deliv_item = lv_posnr.
      w_item-material = gs_alv-matnr.
      w_item-batch = gs_alv-charg.
      w_item-hieraritem = ls_lips-posnr.
      w_item-usehieritm = '1'.
      w_item-dlv_qty = gs_alv-zmenge.
      w_item-dlv_qty_imunit = gs_alv-zmenge.
      w_item-fact_unit_denom = 1.
      w_item-fact_unit_nom = 1.
      append w_item to it_item.
      clear w_item.

      w_item_control-deliv_numb = lv_vbeln.
      w_item_control-deliv_item = lv_posnr.
      w_item_control-chg_delqty = 'X'.
      append w_item_control to it_item_control.
      clear w_item_control.

      w_item_spl-deliv_numb = lv_vbeln.
      w_item_spl-deliv_item = lv_posnr.
      w_item_spl-stge_loc = '1005'.
      append w_item_spl to it_item_spl.
      clear w_item_spl.

    endloop.
    w_item-deliv_numb = lv_vbeln.
    w_item-deliv_item = ls_lips-posnr.
    w_item-material = ls_lips-matnr.
*      w_item-dlv_qty = w_tab-bdmng1.
*      w_item-dlv_qty_imunit = w_tab-bdmng1.
    w_item-fact_unit_denom = 1.
    w_item-fact_unit_nom = 1.
    append w_item to it_item.
    clear w_item.

    w_item_control-deliv_numb = lv_vbeln.
    w_item_control-deliv_item = ls_lips-posnr.
    w_item_control-chg_delqty = 'X'.
    w_item_control-net_wt_flg = 'X'.
    w_item_control-gross_wt_flg = 'X'.
    append w_item_control to it_item_control.
    clear w_item_control.

    w_item_spl-deliv_numb = lv_vbeln.
    w_item_spl-deliv_item = ls_lips-posnr.
    w_item_spl-stge_loc = '1005'.
    append w_item_spl to it_item_spl.
    clear w_item_spl.
  endloop.


  "查 header_deadlines 说明得到下面提示
*- WSHDRLFDAT  Delivery date
*- WSHDRWADAT  Goods issue date (planned)
*- WSHDRWADTI  Goods issue date (actual)
*- WSHDRLDDAT  Loading date
*- WSHDRTDDAT  Transportation planning date
*- WSHDRKODAT  Picking date

  clear:hdl,hdl[].
  hdl-deliv_numb  =   lv_vbeln.
  hdl-timetype = 'WSHDRWADTI'. "实际过账日期
  hdl-timestamp_utc = gs_alv-zgzdate.
  append hdl.

  hdl-deliv_numb =  lv_vbeln.
  hdl-timetype = 'WSHDRWADAT'. "计划日期
  hdl-timestamp_utc = gs_alv-zgzdate.
  append hdl.

  hdl-deliv_numb =  lv_vbeln.
  hdl-timetype = 'WSHDRLFDAT'. "凭证日期
  hdl-timestamp_utc = gs_alv-zgzdate.
  append hdl.


  call function 'BAPI_OUTB_DELIVERY_CONFIRM_DEC'
    exporting
      header_data      = ls_header
      header_control   = ls_header_control
      delivery         = lv_vbeln
    tables
      header_deadlines = hdl
      item_data        = it_item
      item_control     = it_item_control
      item_data_spl    = it_item_spl
      return           = lt_return.

  field-symbols <value> type any.
  if lt_return[] is initial."过账成功，抓取系统缓存的物料凭证数据
    assign (fieldname) to <value>.
    if <value> is assigned.
      l_mblnr = <value>.
    endif.
    "更新ALV展示数据
    clear gt_ztsd001.
    loop at gt_alv into gs_alv where zvbeln = lv_vbeln and box is not initial.
      gs_alv-lfimg = gs_alv-zmenge."过账数量取自建表数量
      gs_alv-zmblnr = l_mblnr."出库物料凭证
      gs_alv-zmsg = '单据生成成功'.
      gs_alv-light = icon_green_light."交货单不为空，状态灯变为绿色
      "过账成功更新自建表
      move-corresponding gs_alv to gs_ztsd001.
      append gs_ztsd001 to gt_ztsd001.
      move-corresponding gs_alv to gs_ztsd003.
      gs_ztsd003-zuser = sy-uname.
      gs_ztsd003-zdate = sy-datum.
      gs_ztsd003-ztime = sy-uzeit.
      gs_ztsd003-zmessage = gs_alv-zmsg.
      append gs_ztsd003 to gt_ztsd003."错误数据写入日志表ZTSD003
      clear gs_ztsd003.
      modify gt_alv from gs_alv.
      clear gs_alv.
      clear gs_ztsd001.
    endloop.
    "提交处理
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = abap_true.
  else."过账失败，有报错消息
    "回滚处理
    call function 'BAPI_TRANSACTION_ROLLBACK'.
    "将错误数据存到ZTSD003里面
    clear lc_msg.
    loop at lt_return where type = 'E' or type =  'A'.
      concatenate lc_msg lt_return-message into lc_msg.
    endloop.

    loop at gt_alv into gs_alv where zvbeln = lv_vbeln and box is not initial.
      "将错误信息更新到自建表
      gs_alv-zmsg = lc_msg.
      move-corresponding gs_alv to gs_ztsd003.
      gs_ztsd003-zuser = sy-uname.
      gs_ztsd003-zdate = sy-datum.
      gs_ztsd003-ztime = sy-uzeit.
      gs_ztsd003-zmessage = gs_alv-zmsg.
      append gs_ztsd003 to gt_ztsd003."错误数据写入日志表ZTSD003
      clear gs_ztsd003.
      modify gt_alv from gs_alv.
      clear gs_alv.
      clear gs_ztsd003.
    endloop.
  endif.
endform.                    " FRM_DN_POST