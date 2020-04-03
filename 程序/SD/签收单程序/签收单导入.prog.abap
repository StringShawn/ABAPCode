*&---------------------------------------------------------------------*
*& Report  ZMM_RP042
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  zmm_rp042.
type-pools:ole2,icon.
*&---------------------------------------------------------------------*
*&内表工作区的定义
*&---------------------------------------------------------------------*
types:begin of ty_alv,
      light   type char4,
      zqsid   type ztsd001-zqsid,
      zposnr  type ztsd001-zposnr,
      zqsdate type ztsd001-zqsdate,
      zcreater type ztsd001-zcreater,
      kunnr   type ztsd001-kunnr,
      vbeln   type ztsd001-vbeln,
      posnr   type ztsd001-posnr,
      matnr   type ztsd001-matnr,
      maktx   type ztsd001-maktx,
      kwmeng  type ztsd001-kwmeng,
      lifnr   type ztsd001-lifnr,
      name1   type ztsd001-name1,
      ebeln   type ztsd001-ebeln,
      ebelp   type ztsd001-ebelp,
      menge   type ztsd001-menge,
      licha   type ztsd001-licha,
      zdate   type ztsd001-zdate,
      zmenge  type ztsd001-zmenge,
      bstkd   type vbkd-bstkd,
      zmsg    type bapi_msg,
      ebeln_n type n length 5,
      end of ty_alv.

types: begin of ty_ekko,
      ebeln type ekko-ebeln,
      ebelp type ekpo-ebelp,
      bsart type ekko-bsart,
      lifnr type ekko-lifnr,
      matnr type ekpo-matnr,
      menge type ekpo-menge,
      end of ty_ekko.

types: begin of ty_lfa1,
      lifnr type lfa1-lifnr,
      name1 type lfa1-name1,
      end of ty_lfa1.

types: begin of ty_makt,
      matnr type makt-matnr,
      maktx type makt-maktx,
      end of ty_makt.

types: begin of ty_vbap,
      vbeln type vbap-vbeln,
      posnr type vbap-posnr,
      kunnr type vbak-kunnr,
      kwmeng type vbap-kwmeng,
      end of ty_vbap.

types: begin of ty_ekkn,
      ebeln type ekkn-ebeln,
      ebelp type ekkn-ebelp,
      vbeln type ekkn-vbeln,
      vbelp type ekkn-vbelp,
      end of ty_ekkn.

types: begin of ty_mdbs,
      ebeln type mdbs-ebeln,
      ebelp type mdbs-ebelp,
      matnr type mdbs-matnr,
      menge type mdbs-menge,
      wemng type mdbs-wemng,
      end of ty_mdbs.

types: begin of ty_vbkd,
      vbeln type vbkd-vbeln,
      posnr type vbkd-posnr,
      bstkd type vbkd-bstkd,
      end of ty_vbkd.

data: gt_excel type table of alsmex_tabline,
      gs_excel type alsmex_tabline.
data: gs_new_line  type ref to data,
      dy_err_table type ref to data,
      dy_table     type ref to data.
data: g_appl type ole2_object.
data: g_work type ole2_object.
data: g_activesheet type ole2_object.
data: gt_data type table of ztsd001,
      gs_data type ztsd001.
data: gt_alv type table of ty_alv,
      gs_alv type ty_alv.

data:gt_ekko type table of ty_ekko,
     gt_lfa1 type table of ty_lfa1,
     gt_makt type table of ty_makt,
     gt_vbap type table of ty_vbap,
     gt_ekkn type table of ty_ekkn,
     gt_mdbs type table of ty_mdbs,
     gt_vbkd type table of ty_vbkd.

data:gs_ekko type ty_ekko,
     gs_lfa1 type ty_lfa1,
     gs_makt type ty_makt,
     gs_vbap type ty_vbap,
     gs_ekkn type ty_ekkn,
     gs_mdbs type ty_mdbs,
     gs_vbkd type ty_vbkd.

"ALV定义
data:gt_fcat    type lvc_t_fcat,
     gs_fcat    type lvc_s_fcat,
     gs_layo    type lvc_s_layo.

data l_desktopdirectory type string.
*&---------------------------------------------------------------------*
*&field-symbol
*&---------------------------------------------------------------------*
field-symbols: <dyn_table>     type standard table,
               <dyn_err_table> type standard table,
               <dyn_field>,
               <dyn_lines>.

*-----------------------------------------------------------------------
* 选择屏幕
*-----------------------------------------------------------------------
*PARAMETERS  p_name TYPE dd02l-tabname OBLIGATORY.
selection-screen begin of block blk1 with frame .
parameters  p_fname type rlgrap-filename memory id xls.
parameters  p_lines type c length 4 default '50'.
selection-screen end of block blk1.
selection-screen: pushbutton 2(10) bt2 user-command com2.

*-----------------------------------------------------------------------
* INITIALIZATION
*-----------------------------------------------------------------------
initialization.

  call method cl_gui_frontend_services=>get_desktop_directory
    changing
      desktop_directory = l_desktopdirectory.
  call method cl_gui_cfw=>update_view.

  bt2 = '下载模板'.

*-----------------------------------------------------------------------
* AT SELECTION-SCREEN
*-----------------------------------------------------------------------
at selection-screen.

  case sy-ucomm.
    when 'COM2'.
      perform frm_download_template.
    when 'ONLI'.
      if p_fname is initial.
        message '请选择路径' type 'E'.
      endif.
    when others.
  endcase.

*-----------------------------------------------------------------------
* AT SELECTION-SCREEN ON VALUE-REQUEST
*-----------------------------------------------------------------------
at selection-screen on value-request for p_fname.
  perform fm_get_path.


*-----------------------------------------------------------------------
* START-OF-SELECTION.
*-----------------------------------------------------------------------
start-of-selection.

  perform frm_input_file.

  perform frm_build_table.

  perform frm_process_data.

  perform frm_display_data.

end-of-selection.
*&---------------------------------------------------------------------*
*&      Form  FM_GET_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form fm_get_path .
  data: title   type string value '选择文件',
        ini_dir type string ,
        l_rc    type i,
        it_tab  type filetable.
  ini_dir = l_desktopdirectory.
  call method cl_gui_frontend_services=>file_open_dialog
    exporting
      window_title            = title
      initial_directory       = ini_dir
      multiselection          = ' '
*     FILE_FILTER             = '*.TXT'
    changing
      file_table              = it_tab
      rc                      = l_rc
    exceptions
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      others                  = 5.
  if sy-subrc = 0 and l_rc = 1.
    read table it_tab into p_fname index 1.
  endif.
endform.                    "FM_GET_PATH
*&---------------------------------------------------------------------*
*&      Form  FRM_INPUT_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form frm_input_file .
  refresh:gt_excel.
  call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'    "读取excel文件中的内容
    exporting
      filename    = p_fname
      i_begin_col = '1'
      i_begin_row = '2'
      i_end_col   = '8' " 读取多少列
      i_end_row   = '1000' "读取多少行
    tables
      intern      = gt_excel.
endform.                    "FRM_INPUT_FILE
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form frm_build_table .
  refresh gt_fcat.
  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name       = 'ZSMM042'
    changing
      ct_fieldcat            = gt_fcat
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.

  call method cl_alv_table_create=>create_dynamic_table
    exporting
      it_fieldcatalog = gt_fcat
    importing
      ep_table        = dy_table.
  call method cl_alv_table_create=>create_dynamic_table
    exporting
      it_fieldcatalog = gt_fcat
    importing
      ep_table        = dy_err_table.
  assign dy_table->* to <dyn_table>.
  assign dy_err_table->* to <dyn_err_table>.
  create data gs_new_line like line of <dyn_table>.

*建立一个与动态内表结构相同的数据对象，且数据对象为是一个结构

  assign gs_new_line->* to <dyn_lines>." 用<dyn_wa>指针指向该结构
endform.                    "FRM_BUILD_TABLE
*&---------------------------------------------------------------------*
*&      Form  FRM_DOWNLOAD_TEMPLATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form frm_download_template .

  field-symbols:<fs_field> type any.
  data: lc_filename type string , "默认名
        lc_fullpath type string ,  "   文件名
        lc_path     type  string . "   不包括文件名
  data: lv_index type i.
  data: lv_exist type abap_bool.
  data: lv_filename type rlgrap-filename.
  data: lv_answer type c.
  data: lv_obj_name type w3objid.
  data: lv_objdata type wwwdatatab.
  data: lv_subrc type sy-subrc.
  data: lv_filepath type rlgrap-filename.

  data:g_excel    type ole2_object,
       g_applica  type ole2_object,
       g_sheet    type ole2_object,
       g_cell     type ole2_object,
       g_workbook type ole2_object.


  refresh:gt_fcat.
  clear: lv_index.

  lc_filename = '客户签收单导入模板'.
  call method cl_gui_frontend_services=>file_save_dialog "调用保存对话框
    exporting
      default_extension    = 'xls'
      default_file_name    = lc_filename
      file_filter          = 'EXCEL(*.XLS)|*.XLS|'
    changing
      filename             = lc_filename
      path                 = lc_path
      fullpath             = lc_fullpath
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      others               = 4.
  if lc_fullpath = ''.
    message '用户取消输入' type 'S' display like 'E'.
    stop.
  endif.


  lv_exist = cl_gui_frontend_services=>file_exist( lc_fullpath ).


  create object g_excel 'EXCEL.APPLICATION'.
  get property of g_excel 'Workbooks' = g_workbook .
  call method of
    g_workbook
    'Close'.

*  IF lv_answer EQ '1'.
  move 'ZMM_RP043_IN' to lv_obj_name.
  select relid objid
    from wwwdata
    into  corresponding fields of lv_objdata
    up to 1 rows
    where srtf2 = 0 and relid = 'MI'
      and objid = lv_obj_name.
  endselect.
  lv_filepath = lc_fullpath.
  call function 'DOWNLOAD_WEB_OBJECT'
    exporting
      key         = lv_objdata
      destination = lv_filepath
    importing
      rc          = lv_subrc.
  if lv_subrc = 0.
    message '模板下载成功' type 'S'.
  endif.
*  ENDIF.

*  CALL METHOD OF
*    g_workbook
*    'open'
*    EXPORTING
*    #1 = lc_fullpath.
*
*  CALL METHOD OF
*    g_excel
*    'worksheets' = g_sheet
*    EXPORTING
*    #1 = 1.
*  CALL METHOD OF
*    g_sheet
*    'activate'.
*
*  SET PROPERTY OF g_excel 'visible' = 1.

  free object g_sheet.
  free object g_applica.
  free object g_workbook.
  free object g_excel.
endform.                    " FRM_DOWNLOAD_TEMPLATE
*&---------------------------------------------------------------------*
*&      Form  fill_cell
*&---------------------------------------------------------------------*
*       ##EXCEL###
*----------------------------------------------------------------------*
*      -->FU_ROW     #
*      -->FU_COLUMN  #
*      -->FU_VALUE   #####
*      -->FU_COLOR   #####
*      -->FU_BOLD    #####
*----------------------------------------------------------------------*
form fill_cell using fu_row
      fu_column
      fu_value
      fu_color
      fu_bold.

  data: l_cell  type ole2_object,
        l_color type ole2_object,
        l_bold  type ole2_object.

  "#######
  call method of
    g_appl
    'Cells' = l_cell
    exporting
    #1 = fu_row
    #2 = fu_column.

  set property of l_cell 'Value'       =  fu_value.

  "#######
  get property of l_cell 'Interior'    = l_color.
  set property of l_color 'ColorIndex' = fu_color.

  "#######
  get property of l_cell 'Font' = l_bold.
  set property of l_bold 'Bold' = fu_bold.

endform. "fill_cell
*&---------------------------------------------------------------------*
*&      Form  frm_process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_process_data .
  data: lv_times type i value 1.
  data: lc_date type sy-datum.
  data: lc_time type sy-uzeit.
  data: lt_ztsd001 type table of ztsd001,
        ls_ztsd001 type ztsd001.
  data: lv_menge1 type mdbs-menge.  "本次导入数量+之前导入数量
  data: lv_menge2 type mdbs-menge.  "剩余可交货数量
  loop at gt_excel into gs_excel.
    lv_times = gs_excel-col.
    read table gt_fcat into gs_fcat index lv_times.
    assign component gs_fcat-fieldname of structure <dyn_lines> to <dyn_field>.
    if gs_fcat-inttype = 'D'.
      call function 'CONVERT_DATE_TO_INTERNAL'
        exporting
          date_external                  = gs_excel-value
*         ACCEPT_INITIAL_DATE            =
       importing
         date_internal                  = <dyn_field>
       exceptions
         date_external_is_invalid       = 1
         others                         = 2.
    else.
      <dyn_field> = gs_excel-value.
    endif.

    at end of row.
      append <dyn_lines> to <dyn_table>.
      clear <dyn_lines>.
    endat.
    clear:gs_fcat,gs_excel.
  endloop.

  refresh:gt_ekko,gt_lfa1,gt_ekkn,gt_makt,gt_vbap.

  data:lt_ekpo type standard table of ekpo with header line.
  data:lv_lines type i.

  loop at <dyn_table> into <dyn_lines>.

    move-corresponding <dyn_lines> to gs_alv.

    call function 'CONVERSION_EXIT_MATN1_INPUT'
      exporting
        input        = gs_alv-matnr
      importing
        output       = gs_alv-matnr
      exceptions
        length_error = 1
        others       = 2.
    if sy-subrc <> 0.
      gs_alv-light = icon_red_light.
      gs_alv-zmsg  = '物料号不正确！'.
      append gs_alv to gt_alv.
      clear:gs_alv.
      continue.
    endif.


    if gs_alv-ebeln is initial.
      gs_alv-light = icon_red_light.
      gs_alv-zmsg  = '采购订单号为空！'.
      append gs_alv to gt_alv.
      clear:gs_alv.
      continue.
    endif.

    if gs_alv-matnr is initial.
      gs_alv-light = icon_red_light.
      gs_alv-zmsg  = '物料号号为空！'.
      append gs_alv to gt_alv.
      clear:gs_alv.
      continue.
    endif.

    if gs_alv-ebelp is initial.

      "依据 采购订单号 物料号号 查取 采购订单行项目号码
      "ZSD156导入前加一段逻辑，如果导入的数据里存在采购订单行项目为空的数据，但是采购订单和物料号是不为空的，根据采购订单加物料判断该条数据在EKPO表是否存在多条；若是多条的话，报错消息“该采购订单对应物料不是唯一行项目，请维护此采购订单导入数据的行项目”；若只存在单条数据的话，取出对应的行项目EKPO-EBELP，作为导入条件写入表中ZTSD001。
      clear:lt_ekpo[],lt_ekpo.
      select * from ekpo into table lt_ekpo where ebeln = gs_alv-ebeln and matnr = gs_alv-matnr .

      if sy-subrc eq 0.

        describe table lt_ekpo lines lv_lines.

        if lv_lines = 1.
          read table lt_ekpo index 1.
          gs_alv-ebelp = lt_ekpo-ebelp.
        else.

          gs_alv-light = icon_red_light.
          gs_alv-zmsg  = '该采购订单对应物料不是唯一行项目，请维护此采购订单导入数据的行项目'.
          append gs_alv to gt_alv.
          clear:gs_alv.
          continue.

        endif.

      else.

        gs_alv-light = icon_red_light.
        gs_alv-zmsg  = '采购订单行项目为空！'.
        append gs_alv to gt_alv.
        clear:gs_alv.
        continue.

      endif.

    endif.


    if gs_alv-licha is initial.
      gs_alv-light = icon_red_light.
      gs_alv-zmsg  = '供应商批次为空！'.
      append gs_alv to gt_alv.
      clear:gs_alv.
      continue.
    endif.

    if gs_alv-zdate is initial.
      gs_alv-light = icon_red_light.
      gs_alv-zmsg  = '效期为空！'.
      append gs_alv to gt_alv.
      clear:gs_alv.
      continue.
    endif.

    if gs_alv-zmenge is initial.
      gs_alv-light = icon_red_light.
      gs_alv-zmsg  = '数量为空！'.
      append gs_alv to gt_alv.
      clear:gs_alv.
      continue.
    endif.



    append gs_alv to gt_alv.
  endloop.

  check gt_alv is not initial.

  select ekko~ebeln
         ebelp
         bsart
         lifnr
         matnr
         menge
  into table gt_ekko
  from ekko inner join ekpo
    on ekko~ebeln = ekpo~ebeln
  for all entries in gt_alv
  where ekko~ebeln = gt_alv-ebeln
    and ebelp = gt_alv-ebelp.
  if sy-subrc = 0.
    select ebeln
           ebelp
           matnr
           menge
           wemng
    into table gt_mdbs
    from mdbs for all entries in gt_ekko
    where ebeln = gt_ekko-ebeln
      and ebelp = gt_ekko-ebelp
      and matnr = gt_ekko-matnr.

    select lifnr
           name1
    into table gt_lfa1
    from lfa1 for all entries in gt_ekko
    where lifnr = gt_ekko-lifnr.

  endif.

  select matnr
         maktx
  into table gt_makt
  from makt for all entries in gt_alv
  where matnr = gt_alv-matnr
    and spras = sy-langu.

  select ebeln
         ebelp
         vbeln
         vbelp
  into table gt_ekkn
  from ekkn for all entries in gt_alv
  where ebeln = gt_alv-ebeln
    and ebelp = gt_alv-ebelp.
  if sy-subrc = 0.
    select vbak~vbeln
           posnr
           kunnr
           kwmeng
    into table gt_vbap
    from vbak inner join vbap
      on vbak~vbeln = vbap~vbeln
    for all entries in gt_ekkn
    where vbak~vbeln = gt_ekkn-vbeln
      and posnr = gt_ekkn-vbelp.

    select vbeln
           posnr
           bstkd
    into table gt_vbkd
    from vbkd for all entries in gt_ekkn
    where vbeln = gt_ekkn-vbeln
      and posnr = gt_ekkn-vbelp.
    sort gt_vbkd by vbeln posnr.
  endif.



  select *
  into table lt_ztsd001
  from ztsd001 for all entries in gt_alv
  where ebeln = gt_alv-ebeln
    and ebelp = gt_alv-ebelp
    and matnr = gt_alv-matnr
    and zmenge1 = 0.

  loop at gt_alv into gs_alv where light ne icon_red_light.

    read table gt_ekko into gs_ekko with key ebeln = gs_alv-ebeln
                                             ebelp = gs_alv-ebelp.
    if sy-subrc ne 0.
      gs_alv-light = icon_red_light.
      gs_alv-zmsg  = '采购订单号不存在！'.
      modify gt_alv from gs_alv.
      clear:gs_alv.
      continue.
    else.
      if gs_ekko-bsart ne 'NB' and gs_ekko-bsart ne 'BH'.
        gs_alv-light = icon_red_light.
        gs_alv-zmsg  = '该采购订单的订单类型不支持导入！'.
        modify gt_alv from gs_alv.
        clear:gs_alv.
        continue.
      endif.
      gs_alv-menge = gs_ekko-menge.
      gs_alv-lifnr = gs_ekko-lifnr.
    endif.



    read table gt_mdbs into gs_mdbs with key ebeln = gs_alv-ebeln
                                             ebelp = gs_alv-ebelp
                                             matnr = gs_alv-matnr.
    if sy-subrc = 0.
      loop at  lt_ztsd001 into ls_ztsd001 where ebeln = gs_alv-ebeln
                                            and ebelp = gs_alv-ebelp
                                            and matnr = gs_alv-matnr.
        add ls_ztsd001-zmenge to lv_menge1.

      endloop.
      add gs_alv-zmenge to lv_menge1.
      lv_menge2 = gs_mdbs-menge - gs_mdbs-wemng.
      if lv_menge1 > lv_menge2.
        gs_alv-light = icon_red_light.
        gs_alv-zmsg  = '此次导入数量已超过该采购订单此物料剩余可交货数量，请处理'.
        modify gt_alv from gs_alv.
        clear:gs_alv,lv_menge1,lv_menge2.
        continue.
      endif.
    endif.

    read table gt_ekkn into gs_ekkn with key ebeln = gs_alv-ebeln
                                             ebelp = gs_alv-ebelp.
    if sy-subrc = 0.
      gs_alv-vbeln = gs_ekkn-vbeln.
      gs_alv-posnr = gs_ekkn-vbelp.
    endif.

    read table gt_vbap into gs_vbap with key vbeln = gs_alv-vbeln
                                             posnr = gs_alv-posnr.
    if sy-subrc = 0.
      gs_alv-kunnr = gs_vbap-kunnr.
      gs_alv-kwmeng = gs_vbap-kwmeng.
    endif.

    read table gt_vbkd into gs_vbkd with key vbeln = gs_alv-vbeln
                                             posnr = gs_alv-posnr binary search.
    if sy-subrc = 0.
      gs_alv-bstkd = gs_vbkd-bstkd.
    endif.

    read table gt_lfa1 into gs_lfa1 with key lifnr = gs_alv-lifnr.
    if sy-subrc = 0.
      gs_alv-name1 = gs_lfa1-name1.
    endif.

    read table gt_makt into gs_makt with key matnr = gs_alv-matnr.
    if sy-subrc = 0.
      gs_alv-maktx = gs_makt-maktx.
    endif.

    gs_alv-light = icon_green_light.
    gs_alv-zmsg  = '导入成功'.
    gs_alv-zqsdate = sy-datum.
    gs_alv-zcreater = sy-uname.
    modify gt_alv from gs_alv.
    clear:gs_alv,lv_menge1,lv_menge2.
  endloop.

  sort gt_alv by kunnr bstkd ebeln ebelp.

  data:lv_num type i.
  data:lv_tabix type sy-tabix.
  clear:lt_ztsd001,ls_ztsd001,lv_num,lv_tabix,gt_vbkd.

  loop at gt_alv into gs_alv where light = icon_green_light.
    add 1 to lv_tabix.
    if lv_tabix = '1'.
      gs_vbkd-bstkd = gs_alv-bstkd.
      append gs_vbkd to gt_vbkd.
    endif.
    read table lt_ztsd001 into ls_ztsd001 with key ebeln = gs_alv-ebeln.
    if sy-subrc ne 0.
      loop at gt_alv transporting no fields where ebeln = gs_alv-ebeln and light = icon_green_light.
        add 1 to gs_alv-ebeln_n.
      endloop.
      add gs_alv-ebeln_n to lv_num.
      if lv_num > p_lines.
        perform frm_generate_qsid tables lt_ztsd001.
        clear lv_num.
        add gs_alv-ebeln_n to lv_num.
      endif.
    endif.

    read table lt_ztsd001 transporting no fields with key kunnr = gs_alv-kunnr.
    if sy-subrc ne 0 and lt_ztsd001 is not initial.
      perform frm_generate_qsid tables lt_ztsd001.
      clear lv_num.
      add gs_alv-ebeln_n to lv_num.
    endif.

    read table gt_vbkd transporting no fields with key bstkd = gs_alv-bstkd.
    if sy-subrc ne 0 and lt_ztsd001 is not initial .
      perform frm_generate_qsid tables lt_ztsd001.
      clear lv_num.
      add gs_alv-ebeln_n to lv_num.
      gs_vbkd-bstkd = gs_alv-bstkd.
      append gs_vbkd to gt_vbkd.
    endif.

    move-corresponding gs_alv to ls_ztsd001.
    append ls_ztsd001 to lt_ztsd001.
    clear gs_alv.
  endloop.

  read table lt_ztsd001 transporting no fields with key zqsid = ''.
  if sy-subrc = 0.
    perform frm_generate_qsid tables lt_ztsd001.
  endif.

  loop at gt_alv into gs_alv where light = icon_green_light.
    read table lt_ztsd001 into ls_ztsd001 with key ebeln = gs_alv-ebeln
                                                   ebelp = gs_alv-ebelp.
    if sy-subrc = 0.
      gs_alv-zqsid = ls_ztsd001-zqsid.
      gs_alv-zposnr = ls_ztsd001-zposnr.
    endif.
    modify gt_alv from gs_alv.
    clear:gs_alv,ls_ztsd001.
  endloop.

  if lt_ztsd001 is not initial.
    modify ztsd001 from table lt_ztsd001.
    commit work and wait.
  endif.

endform.                    " FRM_CHECK_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form frm_display_data .

  perform frm_build_layout."格式
  perform frm_build_fieldcat.
  perform frm_alv_output. "输出

endform.                    "frm_display_data
*&---------------------------------------------------------------------*
*&      Form  frm_build_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form frm_build_layout .
  clear gs_layo.
  gs_layo-cwidth_opt = 'X'.
  gs_layo-zebra      = 'X'.
endform.                    "frm_build_layout
*&---------------------------------------------------------------------*
*&      Form  frm_alv_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form frm_alv_output .

  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
      i_callback_program = sy-repid
      is_layout_lvc      = gs_layo
      it_fieldcat_lvc    = gt_fcat
    tables
      t_outtab           = gt_alv
    exceptions
      program_error      = 1
      others             = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    "frm_alv_output
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_build_fieldcat .
  clear gt_fcat.
  perform add_fieldcat using:'LIGHT  ' '状态灯' '' ''," 状态灯
  'ZQSID  ' '客户签收单号' 'ZTSD001' 'ZQSID  '," 客户签收单号
  'ZPOSNR ' '行项目' 'ZTSD001' 'ZPOSNR '," 行项目

  'LIFNR  ' '供应商编号' 'ZTSD001' 'LIFNR  '," 供应商编号
  'NAME1  ' '供应商名称' 'ZTSD001' 'NAME1  '," 供应商名称
  'EBELN  ' '采购订单' 'ZTSD001' 'EBELN  '," 采购订单
  'EBELP  ' '采购订单行项目' 'ZTSD001' 'EBELP  '," 采购订单行项目
  'MENGE  ' '采购订单数量' 'ZTSD001' 'MENGE  '," 采购订单数量
  'LICHA  ' '供应商批次' 'ZTSD001' 'LICHA  '," 供应商批次
  'ZDATE  ' '效期' 'ZTSD001' 'ZDATE  '," 效期
  'ZMENGE ' '数量' 'ZTSD001' 'ZMENGE '," 数量

  'ZQSDATE' '创建日期' 'ZTSD001' 'ZQSDATE'," 创建日期
  'KUNNR  ' '客户编号' 'ZTSD001' 'KUNNR  '," 客户编号
  'VBELN  ' '销售订单' 'ZTSD001' 'VBELN  '," 销售订单
  'BSTKD  ' '纸质合约号' '' '  '," 合约
  'POSNR  ' '销售凭证项目' 'ZTSD001' 'POSNR  '," 销售凭证项目
  'MATNR  ' '物料' 'ZTSD001' 'MATNR  '," 物料
  'MAKTX  ' '物料描述' 'ZTSD001' 'MAKTX  '," 物料描述
  'KWMENG ' '销售订单数量' 'ZTSD001' 'KWMENG '," 销售订单数量

  'ZMSG   ' '消息' '' ''." 消息



endform.                    " FRM_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDCAT
*&---------------------------------------------------------------------*
*       增加字段
*----------------------------------------------------------------------*
form add_fieldcat  using    value(p_fieldname)
                            value(p_coltext)
                            value(p_ref_table)
                            value(p_ref_field).
  clear gs_fcat.
  gs_fcat-fieldname = p_fieldname.
  gs_fcat-coltext   = p_coltext.
  gs_fcat-ref_table = p_ref_table.
  gs_fcat-ref_field = p_ref_field.
  append gs_fcat to gt_fcat.
endform.                    "add_fieldcat
*&---------------------------------------------------------------------*
*&      Form  FRM_INSERT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZTSD001  text
*----------------------------------------------------------------------*
form frm_generate_qsid  tables   pt_ztsd001 structure ztsd001.

  data:ls_ztsd001 type ztsd001.
  data:lv_zqsid  type ztsd001-zqsid.
  data:lv_zposnr type ztsd001-zposnr.
  clear lv_zposnr.

  lv_zqsid = zcl_mm042_util=>get_zqsid( zdate = sy-datum ).
  loop at pt_ztsd001 into ls_ztsd001 where zqsid is initial.
    add 1 to  lv_zposnr.
    ls_ztsd001-zposnr = lv_zposnr.
    ls_ztsd001-zqsid  = lv_zqsid.
    modify pt_ztsd001 from ls_ztsd001.
    clear:ls_ztsd001.
  endloop.
endform.                    " FRM_INSERT_TABLE