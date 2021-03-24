*&---------------------------------------------------------------------*
*& Program ID    : ZFI_RP024
*& Program Text  : 立AR报表
*& Overview      : 立AR报表
*& Created by    : HANDYXH
*& Creation Date : 2021/03/16
*& Issue Number  : D1181
*&---------------------------------------------------------------------*

report  zfi_rp024.

tables:vbak,likp.

selection-screen begin of block blk1 with frame title text-t01.
select-options:s_bukrs for vbak-bukrs_vf ,
               s_kunnr for vbak-kunnr ,
               s_bstnk for vbak-bstnk ,
               s_vgbel for vbak-vgbel,
               s_vbeln for vbak-vbeln,
               s_wadat for likp-wadat_ist,
               s_auart for vbak-auart.
selection-screen end of block blk1.
*&---------------------------------------------------------------------*
*&数据定义模块
*&---------------------------------------------------------------------*

types begin of:ty_alv.
        include structure zsfi_rp024_alv.
types: vbeln_va type vbak-vbeln,
       fdgrv    type knb1-fdgrv,
       vsart    type likp-vsart,
       " posnr_vl TYPE lips-posnr,
      end of ty_alv.

data:gt_alv type table of ty_alv,
     gs_alv type ty_alv.

data:gt_fcat type lvc_t_fcat,
     gs_layo type lvc_s_layo.

field-symbols: <fs_alv> type ty_alv,
               <fs_fcat> type lvc_s_fcat.

start-of-selection.

  perform frm_get_data.

  perform frm_layout.

  perform frm_bulid_filedcat.

  perform frm_alv_display.

*&---------------------------------------------------------------------*
*&      Form  FRM_LAYOUT
*&---------------------------------------------------------------------*
*       ALV布局
*----------------------------------------------------------------------*
form frm_layout .
  gs_layo-cwidth_opt = 'X'."列优化
  gs_layo-zebra      = 'X'."斑马线
endform.                    " FRM_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  FRM_BULID_FILEDCAT
*&---------------------------------------------------------------------*
*       填充字段
*----------------------------------------------------------------------*
form frm_bulid_filedcat .
  clear:gt_fcat.
  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name       = 'ZSFI_RP024_ALV'
    changing
      ct_fieldcat            = gt_fcat
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  loop at gt_fcat assigning <fs_fcat>.
    case <fs_fcat>-fieldname.
      when 'FDGRV'.
        <fs_fcat>-coltext = text-f01.
      when 'BSTNK'.
        <fs_fcat>-coltext = text-f02.
      when 'AUART'.
        <fs_fcat>-coltext = text-f03.
      when 'VBELN_CO'.
        <fs_fcat>-coltext = text-f04.
      when 'VBELN_VL'.
        <fs_fcat>-coltext = text-f05.
      when 'VATNO'.
        <fs_fcat>-coltext = text-f06.
      when 'BSTZD'.
        <fs_fcat>-coltext = text-f07.
      when 'VSART_TXT'.
        <fs_fcat>-coltext = text-f08.
      when 'WADAT'.
        <fs_fcat>-coltext = text-f09.
      when 'NAME1'.
        <fs_fcat>-coltext = text-f10.
      when 'FDGRV_TXT'.
        <fs_fcat>-coltext = text-f11.
      when 'KEYIN_DATE'.
        <fs_fcat>-coltext = text-f12.
      when others.
    endcase.
  endloop.

endform.                    " FRM_BULID_FILEDCAT
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form frm_alv_display .

  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
      i_callback_program          = sy-repid
*      i_callback_pf_status_set    = 'FM_SET_STATUS'
*      i_callback_user_command     = 'FM_USER_COMMAND'
      is_layout_lvc               = gs_layo
      it_fieldcat_lvc             = gt_fcat
*      I_CALLBACK_HTML_TOP_OF_PAGE = 'FM_TOP_OF_PAGE'
      i_save                      = 'A'
    tables
      t_outtab                    = gt_alv
    exceptions
      program_error               = 1
      others                      = 2.
  if sy-subrc <> 0.
    message id sy-msgid type 'S' number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like 'E'.
    leave list-processing.
  endif.
endform.                    " FRM_ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_get_data .
  types:begin of ty_contract,
         vbeln type vbak-vbeln,
         auart type vbak-auart,
         bukrs type vbak-bukrs_vf,
         kunnr type vbak-kunnr,
         bstnk type vbak-bstnk,
         end of ty_contract,

        begin of ty_kna1,
         kunnr type kna1-kunnr,
         bukrs type knb1-bukrs,
         name1 type kna1-name1,
         fdgrv type knb1-fdgrv,
        end of ty_kna1,

        begin of ty_sales_order,
         vbeln type vbak-vbeln,
         vgbel type vbap-vgbel,
        end of ty_sales_order,

        begin of ty_delivery_note,
         vbeln type likp-vbeln,
         wadat type likp-wadat_ist,
         fkdat type likp-fkdat,
*         vsart TYPE likp-vsart,
         bstzd type lips-bstzd,
        end of ty_delivery_note,

        begin of ty_invoice,
         vbeln type vbrk-vbeln,
         posnr type vbrp-posnr,
         ar_curr type zsfi_rp024_alv-ar_curr,
         vatno type zsfi_rp024_alv-vatno,
         aubel type vbrp-aubel,
         vgbel type vbrp-vgbel,
         fkdat type vbrk-fkdat,
        end of ty_invoice,

        begin of ty_ztsd045,
         vbeln type ztsd045-vbeln,
         zqszt type ztsd045-zqszt,
        end of ty_ztsd045,

        begin of ty_vbfa,
        vbelv	type vbeln_von,
        posnv	type posnr_von,
        vbeln	type vbeln_nach,
        posnn	type posnr_nach,
        end of ty_vbfa,

        begin of ty_t035t,
        grupp type t035t-grupp,
        textl type t035t-textl,
        end of ty_t035t,

        begin of ty_t173t,
        vsart type t173t-vsart,
        bezei type t173t-bezei,
        end of ty_t173t,

        begin of ty_zsd012,
        vbeln type zsd012-vbeln,
        keyin_date type zsd012-keyin_date,
        end of ty_zsd012.

  data:lt_contract      type  table of ty_contract,
       lt_kna1          type  table of ty_kna1,
       lt_sales_order   type table of ty_sales_order,
       lt_delivery_note type table of ty_delivery_note,
       lt_invoice       type table of ty_invoice,
       lt_ztsd045       type table of ty_ztsd045,
       lt_vbfa          type table of ty_vbfa,
       lt_t035t         type table of ty_t035t,
       lt_t173t         type table of ty_t173t,
       lt_zsd012        type table of ty_zsd012,
       ls_contract      type ty_contract,
       ls_kna1          type ty_kna1,
       ls_sales_order   type ty_sales_order,
       ls_delivery_note type ty_delivery_note,
       ls_invoice       type ty_invoice,
       ls_ztsd045       type ty_ztsd045,
       ls_vbfa          type ty_vbfa,
       ls_t035t         type ty_t035t,
       ls_t173t         type ty_t173t,
       ls_zsd012        type ty_zsd012.


  select a~vbeln as vbeln_co
         a~auart
         a~bukrs_vf as bukrs
         a~kunnr
         a~bstnk
         b~vbeln as vbeln_va
         c~vbeln as vbeln_vl
         c~wadat_ist as wadat
         c~vsart
*         c~fkdat
         lips~bstzd
*         lips~posnr AS posnr_vl
  into corresponding fields of table gt_alv
  from vbak as a join vbap as vbap_va on a~vbeln = vbap_va~vgbel
                 join vbak as b on vbap_va~vbeln = b~vbeln
                 join lips on lips~vgbel = b~vbeln
                 join likp as c on c~vbeln = lips~vbeln
  where a~bukrs_vf in s_bukrs
    and a~kunnr in s_kunnr
    and a~bstnk in s_bstnk
    and a~vbeln in s_vgbel
    and b~vbeln in s_vbeln
    and c~wadat_ist in s_wadat
    and a~auart in s_auart.

  if sy-subrc ne 0.
    message s020(zmm01) display like 'E'.
    return.
  endif.

  sort gt_alv.
  delete adjacent duplicates from gt_alv comparing all fields.

  "获取客户信息
  select kna1~kunnr
         bukrs
         name1
         fdgrv
  into table lt_kna1
  from kna1 join knb1
    on kna1~kunnr = knb1~kunnr
  for all entries in gt_alv
  where kna1~kunnr = gt_alv-kunnr
    and bukrs = gt_alv-bukrs.

  sort lt_kna1 by kunnr bukrs.
  if lt_kna1 is not initial.
    select grupp
           textl
    into table lt_t035t
    from t035t for all entries in lt_kna1
    where grupp = lt_kna1-fdgrv
      and spras = sy-langu.
    sort lt_t035t by grupp.
  endif.
  "获取送货单签收状态
*  SELECT vbeln
*         zqszt
*  INTO TABLE lt_ztsd045
*  FROM ztsd045 FOR ALL ENTRIES IN gt_alv
*  WHERE vbeln = gt_alv-vbeln_vl.
*  SORT lt_ztsd045 BY vbeln.

  select vbelv
         posnv
         vbeln
         posnn
  into table lt_vbfa
  from vbfa for all entries in gt_alv
  where vbelv = gt_alv-vbeln_va
    and vbtyp_n = 'M'. "发票
  sort lt_vbfa by vbeln posnn.
  delete adjacent duplicates from lt_vbfa comparing vbeln posnn.

  if lt_vbfa is not initial.
    select vbrp~vbeln
           fkdat
           posnr
           kzwi1 as ar_curr
           vatno
           aubel
           vgbel
    into corresponding fields of table lt_invoice
    from vbrk join vbrp on vbrk~vbeln = vbrp~vbeln
              join zv_fi_vatno on vbrp~vbeln = zv_fi_vatno~vbeln
      for all entries in lt_vbfa
    where vbrp~vbeln = lt_vbfa-vbeln
      and posnr = lt_vbfa-posnn.
  endif.

  select vsart
         bezei
  into table lt_t173t
  from t173t for all entries in gt_alv
  where vsart = gt_alv-vsart
    and spras = sy-langu.
  sort lt_t173t by vsart.

  select vbeln
         keyin_date
  into table lt_zsd012
  from zsd012 for all entries in gt_alv
  where vbeln = gt_alv-vbeln_vl.
  sort lt_zsd012 by vbeln.

  data:lv_days type vtbbewe-atage.

  loop at gt_alv assigning <fs_alv>.
    read table lt_kna1 into ls_kna1 with key kunnr = <fs_alv>-kunnr bukrs = <fs_alv>-bukrs binary search.
    if sy-subrc = 0.
      <fs_alv>-name1 = ls_kna1-name1.
      <fs_alv>-fdgrv = ls_kna1-fdgrv.
    endif.

*    READ TABLE lt_ztsd045 INTO ls_ztsd045 WITH KEY vbeln = <fs_alv>-vbeln_vl BINARY SEARCH.
*    IF sy-subrc = 0.
*      <fs_alv>-zqszt = ls_ztsd045-zqszt.
*    ENDIF.

    "计算应收日期
    lv_days = <fs_alv>-bstzd.
    call function 'FIMA_DATE_CREATE'
      exporting
        i_date = <fs_alv>-wadat
        i_days = lv_days
      importing
        e_date = <fs_alv>-ar_date.

    <fs_alv>-zrlrq = <fs_alv>-wadat.

    read table lt_t035t into ls_t035t with key grupp = <fs_alv>-fdgrv binary search.
    if sy-subrc = 0.
      <fs_alv>-fdgrv_txt = ls_t035t-textl.
    endif.

    read table lt_t173t into ls_t173t with key vsart = <fs_alv>-vsart binary search.
    if sy-subrc = 0.
      <fs_alv>-vsart_txt = ls_t173t-bezei.
    endif.

    read table lt_zsd012 into ls_zsd012 with key vbeln = <fs_alv>-vbeln_vl binary search .
    if sy-subrc = 0.
      <fs_alv>-keyin_date = ls_zsd012-keyin_date.
    endif.

    loop at lt_invoice into ls_invoice where aubel = <fs_alv>-vbeln_va and vgbel = <fs_alv>-vbeln_vl.
      <fs_alv>-vatno = ls_invoice-vatno.
      <fs_alv>-vbeln_vf = ls_invoice-vbeln.
      <fs_alv>-fkdat = ls_invoice-fkdat.
      add ls_invoice-ar_curr to <fs_alv>-ar_curr.
    endloop.

  endloop.


endform.                    " FRM_GET_DATA