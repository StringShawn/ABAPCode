*$*$********************************************************************
* Program ID/Name:  ZMMI004                Date written: 20130821
* Author's name:   HP_DXJ                       Last update:
* Program title:  盘点结果录入导入
* Project Name:  EPR I
* Version:
* Function Spec ID:
*----------------------------------------------------------------------*
* Description: (Incl. Related Function Area and System)
*
*----------------------------------------------------------------------*
* Include:
*
*----------------------------------------------------------------------*
* Calls: (RFC and BPI)
*
*----------------------------------------------------------------------*
* Function Modules:
*
*----------------------------------------------------------------------*
* Table:
*
*----------------------------------------------------------------------*
* Result:
*
*---------------------------------------------------------------------*
* Change History
*----------------------------------------------------------------------*
*     Date   |   Programmer   |   Corr. #   |   Description
*            |                |             |
*            |                |             |
*$*$********************************************************************



*&---------------------------------------------------------------------*
*& Report  ZMMI004
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report zmmi004.


*$*$********************************************************************
*$*$    TABLES                                                         *
*$*$********************************************************************


*$*$********************************************************************
*$*$    INCLUDES                                                       *
*$*$    (FOR INCLUDES WITH DECLARATIVE PURPOSES ONLY                   *
*$*$     I.E. BDC, ALV, ETC.)                                          *
*$*$********************************************************************


*$*$********************************************************************
*$*$    GLOBAL TYPES                                                   *
*$*$********************************************************************
types:begin of t_alv_data ,
        iblnr   type iseg-iblnr ,
        gjahr   type iseg-gjahr ,
        zeili_s type string     ,
        matnr   type iseg-matnr ,
        werks   type iseg-werks ,
        lgort   type iseg-lgort ,
        xnull   type iseg-xnull ,
        erfmg_s type string     ,
        meins   type iseg-meins ,
        zisku   type xfeld      ,
        exidv   type exidv      ,
        vemng_s type string     ,
        vemng   type vepo-vemng ,
        icon    type icon_d     ,
        zsel    type c          ,
        mesg    type string     ,
        zeili   type iseg-zeili ,
        erfmg   type iseg-erfmg ,
        xzael   type iseg-xzael ,
    end of t_alv_data.

types:begin of t_data ,
    iblnr   type iseg-iblnr ,
    gjahr   type iseg-gjahr ,
    zeili_s type string     ,
    exidv   type exidv      ,
    matnr   type iseg-matnr ,
    werks   type iseg-werks ,
    lgort   type iseg-lgort ,
    xnull   type iseg-xnull ,
    erfmg_s type string     ,
    meins   type iseg-meins ,
    zisku   type xfeld      ,
    vemng   type vepo-vemng ,
    icon    type icon_d     ,
    zsel    type c          ,
    mesg    type string     ,
    zeili   type iseg-zeili ,
    erfmg   type iseg-erfmg ,
    xzael   type iseg-xzael ,
end of t_data.




*$*$********************************************************************
*$*$    GLOBAL CONSTANTS                                               *
*$*$********************************************************************
constants:
            c_red(4)      type c          value '@0A@',
            c_yellow(4)   type c          value '@09@',
            c_green(4)    type c          value '@08@'.

*$*$********************************************************************
*$*$    GLOBAL INTERNAL TABLES                                         *
*$*$********************************************************************
data: it_raw            type  truxs_t_text_data ,
      it_alv_data       type standard table of t_alv_data ,
      it_count_data     type standard table of t_data ,
      it_nocount_data   type standard table of t_data ,
      wa_data           type t_data                   ,
      wa_alv_data       type t_alv_data,
      wa_alv_data_tmp   type t_alv_data.
data:it_ikpf type table of ikpf with header line.
data:it_iseg type table of iseg with header line.
data:it_vekp type table of vekp with header line.
data:it_vepo type table of vepo with header line.

*$*$********************************************************************
*$*$    GLOBAL ELEMENTARY VARIABLES                                    *
*$*$********************************************************************
data: gs_repid    type sy-repid,
      gs_fieldcat type slis_t_fieldcat_alv,
      gs_layout   type slis_layout_alv,
      gs_sortinfo type slis_t_sortinfo_alv,
      as_fieldcat type slis_fieldcat_alv.

*$*$********************************************************************
*$*$    PARAMETERS & SELECT-OPTIONS                                    *
*$*$********************************************************************
selection-screen:begin of block b1 with frame title text-001.
PARAMETERS:P_DATE TYPE SY-DATUM OBLIGATORY.
parameters:
            p_file type string obligatory .

selection-screen:end of block b1 .


*$*$********************************************************************
*$*$    INITIALIZATION                                                 *
*$*$********************************************************************
initialization.
*$*$********************************************************************
*$*$    AT SELECTION-SCREEN  Output                                    *
*$*$********************************************************************
at selection-screen on value-request for p_file.
  perform frm_choose_input_file .


*$*$********************************************************************
*$*$    AT SELECTION-SCREEN                                            *
*$*$********************************************************************
at selection-screen.
*$*$********************************************************************
*$*$    START-OF-SELECTION                                             *
*$*$********************************************************************
start-of-selection.

  perform frm_upload_data .

  if it_alv_data[] is initial.

    message s000(zmm_msg) display like 'E' with '文件内容为空!' .
    stop .

  endif.

  perform frm_check_upload_data.

  perform frm_get_xzael .

  perform frm_display_alv .


*&---------------------------------------------------------------------*
*&      Form  FRM_CHOOSE_INPUT_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_choose_input_file .

  data: i_fname type string,
       it_l_filetable type table of file_table,
       i_rc type i,
       i_title type string,
       i_filter type string ,
       i_action type i.
  i_title = text-001.

  i_filter = cl_gui_frontend_services=>filetype_excel.

  call method cl_gui_frontend_services=>file_open_dialog
    exporting
      window_title            = i_title
*     DEFAULT_FILENAME        = '.XLSX'
      file_filter             = i_filter
    changing
      file_table              = it_l_filetable
      rc                      = i_rc
      user_action             = i_action
    exceptions
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      others                  = 5.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    clear i_fname.
  else.
    if i_action = 0.
      read table it_l_filetable index 1 into i_fname.
    else.
      clear i_fname.
    endif.
  endif.
  p_file = i_fname.



endform.                    " FRM_CHOOSE_INPUT_FILE
*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_upload_data .

  data lv_filename type rlgrap-filename.
  lv_filename = p_file.

  call function 'TEXT_CONVERT_XLS_TO_SAP'
    exporting
*     I_FIELD_SEPERATOR    =
      i_line_header        = 'X'     " if this flag is set, first row of record will be ignored during import.
      i_tab_raw_data       = it_raw  " working buffer
      i_filename           = lv_filename   " input Excel file.
    tables
      i_tab_converted_data = it_alv_data      " internal table holding the Excel data
    exceptions
      conversion_failed    = 1
      others               = 2.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    exit.
*        leave program.
  endif.


*  上传TEXT文本文件
*  CALL METHOD cl_gui_frontend_services=>gui_upload
*    EXPORTING
*      filename                = p_file
*      filetype                = 'ASC'
*      has_field_separator     = 'X'
*      header_length           = 0
*      read_by_line            = 'X'
*      dat_mode                = space
*      codepage                = space
*      ignore_cerr             = abap_true
*      replacement             = '#'
*    CHANGING
*      data_tab                = it_alv_data
*    EXCEPTIONS
*      file_open_error         = 1
*      file_read_error         = 2
*      no_batch                = 3
*      gui_refuse_filetransfer = 4
*      invalid_type            = 5
*      no_authority            = 6
*      unknown_error           = 7
*      bad_data_format         = 8
*      header_not_allowed      = 9
*      separator_not_allowed   = 10
*      header_too_long         = 11
*      unknown_dp_error        = 12
*      access_denied           = 13
*      dp_out_of_memory        = 14
*      disk_full               = 15
*      dp_timeout              = 16
*      not_supported_by_gui    = 17
*      error_no_gui            = 18
*      others                  = 19.
*  IF sy-subrc <> 0.
*  ENDIF.




endform.                    " FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_display_alv .

  clear:gs_fieldcat,gs_fieldcat[].


  perform frm_field   using   'ICON'      ''                '' .
  perform frm_field   using   'MESG'      '消息文本'        '' .
  perform frm_field   using   'IBLNR'     '盘点凭证'        '' .
  perform frm_field   using   'GJAHR'     '年度'            '' .
  perform frm_field   using   'ZEILI'     '凭证行号 '       '' .
  perform frm_field   using   'MATNR'     '零件号'          '' .
  perform frm_field   using   'WERKS'     '工厂'            '' .
  perform frm_field   using   'LGORT'     '库位'            '' .
  perform frm_field   using   'XNULL'     '零计数'          '' .
  perform frm_field   using   'ERFMG'     '盘点数量'        '' .
  perform frm_field   using   'MEINS'     '单位'            '' .
  perform frm_field   using   'ZISKU'     '是否SKU'         '' .
  perform frm_field   using   'EXIDV'     'SKU编号'         '' .
  perform frm_field   using   'VEMNG'     'SKU数量'         '' .
  perform frm_layout  tables it_alv_data using 'PF_STATUS' 'ZSEL'.


endform.                    " FRM_DISPLAY_ALV

*&---------------------------------------------------------------------*
*&      Form  FRM_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form frm_field  using p_fieldname p_seltext p_no_zero.
  as_fieldcat-fieldname       = p_fieldname.
  as_fieldcat-seltext_l       = p_seltext.
  as_fieldcat-no_zero         = p_no_zero.

  append as_fieldcat to gs_fieldcat.
  clear as_fieldcat.
endform.                    " FRM_FIELD

*&---------------------------------------------------------------------*
*&      Form  FRM_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form frm_layout  tables   p_data using p_status p_box.
  gs_repid = sy-repid.
  gs_layout-zebra = 'X'.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-box_fieldname      =  p_box.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program       = gs_repid
      i_save                   = 'A'
      it_fieldcat              = gs_fieldcat[]
      it_sort                  = gs_sortinfo[]
      is_layout                = gs_layout
      i_callback_pf_status_set = p_status
      i_callback_user_command  = 'USER_COMMAND'
    tables
      t_outtab                 = p_data
    exceptions
      program_error            = 1
      others                   = 2.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    "frm_layout
*&---------------------------------------------------------------------*
*&      FORM  PF_STATUS
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->RT_EXTAB   TEXT
*----------------------------------------------------------------------*

form pf_status using rt_extab type slis_t_extab.
  set pf-status '1000_STATUS'.
  set titlebar  '1000_TITLE' .
endform.                    "PF_STATUS
*&---------------------------------------------------------------------*
*&      FORM  USER_COMMAND
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->R_UCOMM      TEXT
*      -->RS_SELFIELD  TEXT
*----------------------------------------------------------------------*
form user_command  using r_ucomm type sy-ucomm  rs_selfield type slis_selfield.
  data: lr_grid type ref to cl_gui_alv_grid.

* 将界面中的选择数据更新到内表中
*=====GET_GLOBALS_FROM_SLVC_FULLSCR  START==========
  call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    importing
      e_grid = lr_grid.
  call method lr_grid->check_changed_data.
*=====GET_GLOBALS_FROM_SLVC_FULLSCR  END============
  case r_ucomm.
    when 'DATA_SAVE'.
      perform frm_exec.
*      PERFORM frm_get_data01.
    when 'BACK'.
      leave to screen 0 .
    when 'EXIT' .
      leave to screen 0 .
    when  'CANCEL'.
      leave program .
  endcase.
  rs_selfield-row_stable = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-refresh = 'X'.
endform.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_EXEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_exec .

  refresh:it_nocount_data,
          it_count_data.
  break hp_tc.
  loop at it_alv_data into wa_alv_data where icon <> c_red.

    if wa_alv_data-xzael <> 'X'.

      move-corresponding wa_alv_data to wa_data.
      append wa_data to it_nocount_data .

    else.

      move-corresponding wa_alv_data to wa_data.
      append wa_data to it_count_data .

    endif.

    clear:wa_data , wa_alv_data .

  endloop.

  sort it_nocount_data by iblnr gjahr zeili_s exidv.
  sort it_count_data   by iblnr gjahr zeili_s exidv.

*  已计数
  perform frm_bapi_changecount tables it_count_data.

*  未计数
  perform frm_bapi_count tables it_nocount_data.

  loop at it_alv_data into wa_alv_data.

    read table it_count_data into wa_data with key iblnr = wa_alv_data-iblnr
                                                   gjahr = wa_alv_data-gjahr
                                                   zeili = wa_alv_data-zeili .
    if sy-subrc = 0 .

      wa_alv_data-icon = wa_data-icon .
      wa_alv_data-mesg = wa_data-mesg  .

      modify it_alv_data from wa_alv_data transporting icon mesg
                                          where iblnr = wa_alv_data-iblnr
                                          and   gjahr = wa_alv_data-gjahr
                                          and   zeili = wa_alv_data-zeili .
      clear wa_data .

      continue.

    endif.

    read table it_nocount_data into wa_data with key iblnr = wa_alv_data-iblnr
                                                     gjahr = wa_alv_data-gjahr
                                                     zeili = wa_alv_data-zeili .
    if sy-subrc = 0 .

      wa_alv_data-icon = wa_data-icon .
      wa_alv_data-mesg = wa_data-mesg .

      modify it_alv_data from wa_alv_data transporting icon mesg
                                    where iblnr = wa_alv_data-iblnr
                                    and   gjahr = wa_alv_data-gjahr
                                    and   zeili = wa_alv_data-zeili .

      clear wa_data .
      continue .

    endif.

  endloop.

  refresh : it_count_data , it_nocount_data .

endform.                    " FRM_EXEC
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_check_upload_data .

  data:
        lv_error  type c,
        i_vemng   type  vepo-vemng,
        i_flag    type c,
        wa_l_vekp type vekp,
        wa_l_vepo type vepo,
        i_idx1    type sy-tabix,
        i_idx2    type sy-tabix.

  loop at it_alv_data into wa_alv_data.
    i_idx1 = sy-tabix.

    catch system-exceptions others = 8.
      wa_alv_data-zeili = wa_alv_data-zeili_s .
    endcatch.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wa_alv_data-iblnr
      importing
        output = wa_alv_data-iblnr.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wa_alv_data-zeili
      importing
        output = wa_alv_data-zeili.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wa_alv_data-exidv
      importing
        output = wa_alv_data-exidv.

    call function 'CONVERSION_EXIT_MATN1_INPUT'
      exporting
        input        = wa_alv_data-matnr
      importing
        output       = wa_alv_data-matnr
      exceptions
        length_error = 1
        others       = 2.

    modify it_alv_data from wa_alv_data index i_idx1.
  endloop.

  sort it_alv_data by gjahr iblnr zeili.

  select * from ikpf
    into corresponding fields of table it_ikpf
    for all entries in it_alv_data
    where iblnr = it_alv_data-iblnr
      and gjahr = it_alv_data-gjahr.
  sort it_ikpf by iblnr gjahr.

  if it_ikpf[] is not initial.
    select * from iseg
      into corresponding fields of table it_iseg
      for all entries in it_ikpf
      where iblnr = it_ikpf-iblnr
        and gjahr = it_ikpf-gjahr.
    sort it_iseg by iblnr gjahr zeili.
  endif.


  select * from vekp
    into corresponding fields of table it_vekp
    for all entries in it_alv_data
    where exidv = it_alv_data-exidv.
  sort it_vekp by exidv.

  if it_vekp[] is not initial.
    select * from vepo
      into corresponding fields of table it_vepo
      for all entries in it_vekp
      where venum = it_vekp-venum.
    sort it_vepo by venum vepos.
  endif.


*" ALV check
  loop at it_alv_data into wa_alv_data.

    clear:i_idx1.
    i_idx1 = sy-tabix.

    catch system-exceptions others = 8.
      wa_alv_data-erfmg = wa_alv_data-erfmg_s .
      wa_alv_data-vemng = wa_alv_data-vemng_s .
    endcatch.
    if sy-subrc ne 0.
      wa_alv_data-mesg = '编号或数量格式有错误!'.
      wa_alv_data-icon = c_red .

      modify it_alv_data from wa_alv_data index i_idx1.

      clear: lv_error , wa_alv_data.

      continue.
    endif.

    READ TABLE it_ikpf with key IBLNR = wa_alv_data-IBLNR
                                GJAHR = wa_alv_data-GJAHR.
    IF sy-subrc = 0 and it_ikpf-GIDAT+0(6) <> p_date+0(6).
      wa_alv_data-mesg = '盘点凭证的计划盘点日期与盘点计数的盘点日期不在同一期间!'.
      wa_alv_data-icon = c_red .

      modify it_alv_data from wa_alv_data index i_idx1.

      clear: lv_error , wa_alv_data.

      continue.
    ENDIF.


*    检查盘点凭证是否存在
    perform frm_check_physical_document using wa_alv_data-iblnr wa_alv_data-gjahr
                                        changing lv_error .

    if lv_error = 'X'.

      wa_alv_data-mesg = '盘点凭证不存在!'.
      wa_alv_data-icon = c_red .

      modify it_alv_data from wa_alv_data index i_idx1.

      clear: lv_error , wa_alv_data.

      continue.

    endif.

* 检查是否已差异过帐
    perform frm_check_xdiff using wa_alv_data-iblnr wa_alv_data-zeili wa_alv_data-gjahr
                            changing lv_error.

    if lv_error = 'X'.

      wa_alv_data-mesg = '盘点凭证已差异过帐!'.
      wa_alv_data-icon = c_red .

      modify it_alv_data from wa_alv_data index i_idx1.

      clear: lv_error , wa_alv_data.

      continue.

    endif.

    if ( wa_alv_data-erfmg is initial and wa_alv_data-xnull is initial )
      or ( wa_alv_data-erfmg is not initial and wa_alv_data-xnull is not initial ).

      wa_alv_data-mesg = '盘点数量和零计数栏位不能同时设置!'.
      wa_alv_data-icon = c_red .

      modify it_alv_data from wa_alv_data index i_idx1.

      clear: lv_error , wa_alv_data.

      continue.

    endif.

    if wa_alv_data-erfmg is initial and wa_alv_data-xnull is not initial.

      wa_alv_data-icon = c_yellow .

      modify it_alv_data from wa_alv_data index i_idx1.

      clear: lv_error , wa_alv_data.

      continue.

    endif.

    if wa_alv_data-zisku is initial.

      if wa_alv_data-exidv is not initial
      or wa_alv_data-exidv is not initial.

        wa_alv_data-mesg = 'SKU编号栏位不能设值！' .
        wa_alv_data-icon = c_red .

        modify it_alv_data from wa_alv_data index i_idx1.

        clear: lv_error , wa_alv_data.

        continue.
      endif.

    else.

      if wa_alv_data-exidv is  initial.

        wa_alv_data-mesg = 'SKU编号栏位不能为空！' .
        wa_alv_data-icon = c_red .

        modify it_alv_data from wa_alv_data index i_idx1.

        clear: lv_error , wa_alv_data.

        continue.

      endif.

* 检查SKU的合法性
      clear:wa_l_vekp.
      read table it_vekp into wa_l_vekp with key exidv = wa_alv_data-exidv binary search.
      if sy-subrc ne 0.
        "Do nothing
      else.
        "检查库位是否一致
        read table it_vepo into wa_l_vepo with key venum = wa_l_vekp-venum binary search.
        if sy-subrc ne 0.
          "Do nothing
        else.
          if wa_l_vepo-matnr ne wa_alv_data-matnr
          or  wa_l_vepo-werks ne wa_alv_data-werks
          or wa_l_vepo-lgort ne wa_alv_data-lgort.
            message s207(zmm_msg) with wa_alv_data-exidv
                                       wa_l_vekp-werks
                                       wa_l_vekp-lgort
                                       into wa_alv_data-mesg.
            wa_alv_data-icon = c_red .

            modify it_alv_data from wa_alv_data index i_idx1.

            clear: lv_error , wa_alv_data.

            continue.
          endif.
        endif.
      endif.

* 检查是否有重复的SKU
      loop at it_alv_data into wa_alv_data_tmp where zisku is not initial
                                                  and iblnr = wa_alv_data-iblnr
                                                  and gjahr = wa_alv_data-gjahr
                                                  and zeili = wa_alv_data-zeili
                                                  and exidv = wa_alv_data-exidv.

        check sy-tabix ne i_idx1.
        wa_alv_data-mesg = 'SKU号重复，请检查！' .
        wa_alv_data-icon = c_red .

        modify it_alv_data from wa_alv_data index i_idx1 .

        lv_error = 'X'.

        exit.
      endloop.
      if wa_alv_data-icon = c_red .
        clear: lv_error , wa_alv_data.
        continue.
      endif.
    endif.


    modify it_alv_data from wa_alv_data index i_idx1.
    clear: lv_error , wa_alv_data.

  endloop.

  clear:it_ikpf[],it_iseg[],it_vekp[],it_vepo[].

*" ALV each group
  loop at it_alv_data into wa_alv_data
    where icon is initial.

    clear:wa_alv_data_tmp,
          i_vemng,
          i_flag.
    loop at it_alv_data into wa_alv_data_tmp
      where iblnr = wa_alv_data-iblnr
        and gjahr = wa_alv_data-gjahr
        and zeili = wa_alv_data-zeili
        and icon is initial.
      if wa_alv_data_tmp-erfmg ne wa_alv_data-erfmg.
        i_flag = '1'.
        exit.
      endif.

      if wa_alv_data_tmp-zisku is not initial.
        i_vemng = i_vemng + wa_alv_data_tmp-vemng.
      endif.
    endloop.
    if i_flag is initial
    and wa_alv_data-zisku is not initial..
      if i_vemng ne wa_alv_data-erfmg.
        i_flag = '2'.
      endif.
    endif.
    loop at it_alv_data into wa_alv_data_tmp
      where iblnr = wa_alv_data-iblnr
        and gjahr = wa_alv_data-gjahr
        and zeili = wa_alv_data-zeili
        and icon is initial.

      case i_flag.
        when '1'.
          wa_alv_data_tmp-mesg = '相同行项目的盘点数量不一致！' .
          wa_alv_data_tmp-icon = c_red .
        when '2'.
          wa_alv_data_tmp-mesg = '盘点数量与SKU数量不一致！' .
          wa_alv_data_tmp-icon = c_red .
        when others.
          wa_alv_data_tmp-icon = c_green .
      endcase.

      modify it_alv_data from wa_alv_data_tmp.
    endloop.

  endloop.

endform.                    " FRM_CHECK_UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_PHYSICAL_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ALV_DATA_IBLNR  text
*      <--P_LV_ERROR  text
*----------------------------------------------------------------------*
form frm_check_physical_document  using    p_iblnr p_gjahr
                                  changing p_error.
*  data lv_iblnr type ikpf-iblnr .
*
*  select single iblnr into lv_iblnr from ikpf
*        where iblnr = p_iblnr
*           and gjahr = p_gjahr .
*
  read table it_ikpf with key iblnr = p_iblnr gjahr = p_gjahr binary search.
  if sy-subrc <> 0.

    p_error = 'X' .

  endif.


endform.                    " FRM_CHECK_PHYSICAL_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_XDIFF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_check_xdiff using p_iblnr p_zeili p_gjahr
                      changing p_error.
*  data:
*        lv_xdiff type iseg-xdiff.
*
*  select single xdiff into lv_xdiff
*         from   iseg
*         where iblnr = p_iblnr
*         and   zeili = p_zeili
*         and   gjahr = p_gjahr .

  clear:it_iseg.
  read table it_iseg with key iblnr = p_iblnr
                              gjahr = p_gjahr
                              zeili = p_zeili
                              binary search .
  if it_iseg-xdiff = 'X'.

    p_error = 'X' .

  endif.



endform.                    " FRM_CHECK_XDIFF
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_XZAEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_get_xzael .

  data:begin of lt_xzael occurs 0 ,
          iblnr type iseg-iblnr ,
          gjahr type iseg-gjahr ,
          zeili type iseg-zeili ,
          xzael type iseg-xzael ,
       end of lt_xzael .

  select
         iblnr
         gjahr
         zeili
         xzael
    into corresponding fields of table lt_xzael
    from iseg
    for all entries in it_alv_data
    where iblnr = it_alv_data-iblnr
    and   gjahr = it_alv_data-gjahr
    and   zeili = it_alv_data-zeili .

  loop at lt_xzael.

    read table it_alv_data into wa_alv_data with key iblnr = lt_xzael-iblnr
                                                     gjahr = lt_xzael-gjahr
                                                     zeili = lt_xzael-zeili .
    if sy-subrc = 0 .

      wa_alv_data-xzael = lt_xzael-xzael .

      modify it_alv_data from wa_alv_data transporting xzael
                                          where  iblnr = lt_xzael-iblnr
                                          and    gjahr = lt_xzael-gjahr
                                          and    zeili = lt_xzael-zeili .

    endif.

  endloop.




endform.                    " FRM_GET_XZAEL
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_CHANGECOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_bapi_changecount tables pt_data like  it_count_data .

  data:begin of lt_ztinvent occurs 0.
          include structure ztinvent .
  data:end of lt_ztinvent .

  data:lt_ztinvent_del type table of ztinvent with header line.

  data:
        la_data type t_data.


  data: lv_error type c ,
        lv_mesg  type string ,
        lt_items  like bapi_physinv_count_items occurs 0 with header line,
        lt_return like bapiret2                 occurs 0 with header line.

  refresh:lt_ztinvent,
          lt_items.



  loop at pt_data into wa_data.
    la_data = wa_data.

    call function 'CONVERSION_EXIT_CUNIT_INPUT'
      exporting
        input          = la_data-meins
        language       = sy-langu
      importing
        output         = la_data-meins
      exceptions
        unit_not_found = 1
        others         = 2.

    at new zeili_s.
      clear lt_items .
      lt_items-item         = la_data-zeili.
      lt_items-material     = la_data-matnr .
      lt_items-entry_qnt    = la_data-erfmg.
      lt_items-entry_uom    = la_data-meins .
      lt_items-zero_count   = la_data-xnull .
      append lt_items .
    endat.

    if la_data-exidv is not initial.

      clear lt_ztinvent.

      lt_ztinvent-exidv = la_data-exidv .
      lt_ztinvent-iblnr = la_data-iblnr .
      lt_ztinvent-gjahr = la_data-gjahr .
      lt_ztinvent-zeili = la_data-zeili .
      lt_ztinvent-vemng = la_data-vemng .

      append lt_ztinvent.
    else.
      if la_data-xnull = '0'.
        lt_ztinvent_del-iblnr = la_data-iblnr .
        lt_ztinvent_del-gjahr = la_data-gjahr .
        lt_ztinvent_del-zeili = la_data-zeili .
        append lt_ztinvent_del.
      endif.
    endif.

    at end of iblnr .

      refresh:lt_return.

      call function 'BAPI_MATPHYSINV_CHANGECOUNT'
        exporting
          physinventory       = la_data-iblnr
          fiscalyear          = la_data-gjahr
*         PERCENTAGE_VARIANCE =
        tables
          items               = lt_items
          return              = lt_return
*         SERIALNUMBERS       =
        .

      clear:lv_mesg,lv_error.

      loop at lt_return where type = 'E' or type = 'A'.

        if lt_return-number = '724'
        and lt_return-id = 'M7'.    "排除E  M7  724
          if lv_error is initial.
            lv_error = '1'.
          endif.
        else.
          lv_error = '2'.
          message id lt_return-id type lt_return-type number lt_return-number
                 with lt_return-message_v1 lt_return-message_v2
                      lt_return-message_v3 lt_return-message_v4
                  into lv_mesg .
          exit.
        endif.
      endloop.

      if lv_error <> '2'.
*        更新自建表信息
        if lt_ztinvent[] is not initial.

          sort lt_ztinvent by exidv iblnr gjahr zeili.
          delete adjacent duplicates from lt_ztinvent comparing all fields .

          "先删除先前的SKU信息
          delete from ztinvent where iblnr = la_data-iblnr
                                 and gjahr = la_data-gjahr
                                 and zeili = la_data-zeili .

          modify ztinvent from table lt_ztinvent .
          if sy-subrc <> 0.
            lv_error = '3' .
          else.
            if lv_error = '1'.
              lv_error = '5' .
            else.
              lv_error = '4' .
            endif.
          endif.

        endif.

        if lt_ztinvent_del[] is not initial.""""""删除SKU信息
          sort lt_ztinvent_del by exidv iblnr gjahr zeili.
          delete adjacent duplicates from lt_ztinvent_del comparing all fields .
          loop at lt_ztinvent_del.
            delete from ztinvent where iblnr = lt_ztinvent_del-iblnr
                                   and gjahr = lt_ztinvent_del-gjahr
                                   and zeili = lt_ztinvent_del-zeili .
          endloop.
          clear:lt_ztinvent_del[].
        endif.

      endif.

      case lv_error.
        when '1'.
          la_data-icon = c_yellow .
          la_data-mesg = '盘点凭证数据没有变更!' .

        when '2'.
          call function 'BAPI_TRANSACTION_ROLLBACK' .
          la_data-mesg = lv_mesg .
          la_data-icon = c_red   .

        when '3'.
          call function 'BAPI_TRANSACTION_ROLLBACK' .
          rollback work.
          la_data-mesg = '盘点凭证结果导入成功!SKU保存失败!' .
          la_data-icon = c_red   .

        when '4'.
          call function 'BAPI_TRANSACTION_COMMIT' .
          commit work and wait.
          la_data-mesg = '盘点凭证结果导入成功!SKU保存成功!' .
          la_data-icon = c_green   .

        when '5'.
          commit work and wait.
          la_data-mesg = '盘点凭证未修改!SKU保存成功!' .
          la_data-icon = c_green   .

        when others."space
          call function 'BAPI_TRANSACTION_COMMIT' .
          commit work and wait.
          la_data-icon = c_green .
          la_data-mesg = '盘点凭证结果导入成功!' .
      endcase.

      modify pt_data from la_data transporting icon mesg
                                              where iblnr = la_data-iblnr.
      clear la_data .

      refresh:lt_items,
              lt_ztinvent.

    endat .

  endloop.

endform.                    " FRM_BAPI_CHANGECOUNT
*&---------------------------------------------------------------------*
*&      Form  FRM_BAPI_COUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_NOCOUNT_DATA  text
*----------------------------------------------------------------------*
form frm_bapi_count  tables   pt_data  like it_nocount_data.


  data:begin of lt_ztinvent occurs 0.
          include structure ztinvent .
  data:end of lt_ztinvent .

  data:lt_ztinvent_del type table of ztinvent with header line.

  data la_data type t_data .

  data: lv_error type c ,
      lv_mesg  type string ,
      lt_items  like bapi_physinv_count_items occurs 0 with header line,
      lt_return like bapiret2                 occurs 0 with header line.

  refresh:lt_items,lt_ztinvent.

  loop at pt_data into wa_data .
    la_data = wa_data .

    call function 'CONVERSION_EXIT_CUNIT_INPUT'
      exporting
        input          = la_data-meins
        language       = sy-langu
      importing
        output         = la_data-meins
      exceptions
        unit_not_found = 1
        others         = 2.

    at new zeili_s .
      clear lt_items .
      lt_items-item         = la_data-zeili.
      lt_items-material     = la_data-matnr .
      lt_items-entry_qnt    = la_data-erfmg.
      lt_items-entry_uom    = la_data-meins .
      lt_items-zero_count   = la_data-xnull .
      append lt_items .
    endat.

    if la_data-exidv is not initial.

      clear lt_ztinvent.
      lt_ztinvent-exidv = la_data-exidv .
      lt_ztinvent-iblnr = la_data-iblnr .
      lt_ztinvent-gjahr = la_data-gjahr .
      lt_ztinvent-zeili = la_data-zeili .
      lt_ztinvent-vemng = la_data-vemng .
      append lt_ztinvent.
    else.
      if la_data-xnull = '0'.
        lt_ztinvent_del-iblnr = la_data-iblnr .
        lt_ztinvent_del-gjahr = la_data-gjahr .
        lt_ztinvent_del-zeili = la_data-zeili .
        append lt_ztinvent_del.
      endif.
    endif.

    at end of iblnr .

      refresh:lt_return.

      call function 'BAPI_MATPHYSINV_COUNT'
        exporting
          physinventory       = la_data-iblnr
          fiscalyear          = la_data-gjahr
*         PERCENTAGE_VARIANCE =
         COUNT_DATE          = p_date
        tables
          items               = lt_items
          return              = lt_return.
*         SERIALNUMBERS             =
*         EXTENSIONIN               =


      clear:lv_mesg,lv_error.

      loop at lt_return where type = 'E' or type = 'A'.

        if lt_return-number = '724'
        and lt_return-id = 'M7'.    "排除E  M7  724
          if lv_error is initial.
            lv_error = '1'.
          endif.
        else.
          lv_error = '2'.
          message id lt_return-id type lt_return-type number lt_return-number
                 with lt_return-message_v1 lt_return-message_v2
                      lt_return-message_v3 lt_return-message_v4
                  into lv_mesg .
          exit.
        endif.
      endloop.

      if lv_error <> '2'.
*        更新自建表信息
        if lt_ztinvent[] is not initial.

          sort lt_ztinvent by exidv iblnr gjahr zeili.
          delete adjacent duplicates from lt_ztinvent comparing all fields .

          "先删除先前的SKU信息
          delete from ztinvent where iblnr = la_data-iblnr
                                 and gjahr = la_data-gjahr
                                 and zeili = la_data-zeili .

          modify ztinvent from table lt_ztinvent .
          if sy-subrc <> 0.
            lv_error = '3' .
          else.
            if lv_error = '1'.
              lv_error = '5' .
            else.
              lv_error = '4' .
            endif.
          endif.

        endif.

        if lt_ztinvent_del[] is not initial.
          sort lt_ztinvent_del by exidv iblnr gjahr zeili.
          delete adjacent duplicates from lt_ztinvent_del comparing all fields .
          loop at lt_ztinvent_del.
            delete from ztinvent where iblnr = lt_ztinvent_del-iblnr
                                   and gjahr = lt_ztinvent_del-gjahr
                                   and zeili = lt_ztinvent_del-zeili .
          endloop.
          clear:lt_ztinvent_del[].
        endif.

      endif.

      case lv_error.
        when '1'.
          la_data-icon = c_yellow .
          la_data-mesg = '盘点凭证数据没有变更!' .

        when '2'.
          call function 'BAPI_TRANSACTION_ROLLBACK' .
          la_data-mesg = lv_mesg .
          la_data-icon = c_red   .

        when '3'.
          call function 'BAPI_TRANSACTION_ROLLBACK' .
          rollback work.
          la_data-mesg = '盘点凭证结果导入成功!SKU保存失败!' .
          la_data-icon = c_red   .

        when '4'.
          call function 'BAPI_TRANSACTION_COMMIT' .
          commit work and wait.
          la_data-mesg = '盘点凭证结果导入成功!SKU保存成功!' .
          la_data-icon = c_green   .

        when '5'.
          commit work and wait.
          la_data-mesg = '盘点凭证未修改!SKU保存成功!' .
          la_data-icon = c_green   .

        when others."space
          call function 'BAPI_TRANSACTION_COMMIT' .
          commit work and wait.
          la_data-icon = c_green .
          la_data-mesg = '盘点凭证结果导入成功!' .
      endcase.

      modify pt_data from la_data transporting icon mesg
                                              where iblnr = la_data-iblnr.
      clear la_data .

      refresh:lt_items,
              lt_ztinvent.

    endat .

  endloop.

endform.                    " FRM_BAPI_COUNT
