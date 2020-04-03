*&---------------------------------------------------------------------*
*& Report  ZMM_RP043
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  zmm_rp043.
*&---------------------------------------------------------------------*
*&内表工作区的定义
*&---------------------------------------------------------------------*
type-pools:slis.
tables:ztsd001,ekko.

types:begin of ty_alv,
      box     type c,
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
      menge1  type ztsd001-menge,
      licha   type ztsd001-licha,
      zdate   type ztsd001-zdate,
      zmenge  type ztsd001-zmenge,
      bsart   type ekko-bsart,
      aedat   type ekko-aedat,
      bstkd   type vbkd-bstkd,
      zmsg    type bapi_msg,
      ebeln_n type n length 5,
      end of ty_alv.

data: gt_alv type table of ty_alv,
      gs_alv type ty_alv.

"ALV定义
data:gt_fcat    type lvc_t_fcat,
     gs_fcat    type lvc_s_fcat,
     gs_layo    type lvc_s_layo.

include zmm_rp043_print.
include zmm_rp043_excel.
*-----------------------------------------------------------------------
* 选择屏幕
*-----------------------------------------------------------------------
"录入界面
selection-screen begin of block blk1 with frame .
select-options:s_ebeln1 for ztsd001-ebeln modif id ss1,
               s_vbeln1 for ztsd001-vbeln modif id ss1,
               s_kunnr1 for ztsd001-kunnr modif id ss1,
               s_lifnr1 for ztsd001-lifnr modif id ss1,
               s_matnr1 for ztsd001-matnr modif id ss1,
               s_erdat1 for ekko-aedat    modif id ss1.
parameters  p_lines type c length 4 default '50' modif id ss1.
selection-screen skip 1 .
selection-screen end of block blk1.
"修改界面
selection-screen begin of block blk2 with frame .
select-options:s_zqsid2 for ztsd001-zqsid    modif id ss2,
               s_zqsdt2 for ztsd001-zqsdate  modif id ss2,
               s_kunnr2 for ztsd001-kunnr    modif id ss2,
               s_lifnr2 for ztsd001-lifnr    modif id ss2,
               s_matnr2 for ztsd001-matnr    modif id ss2,
               s_ernam2 for ztsd001-zcreater modif id ss2.
selection-screen skip 1 .
selection-screen end of block blk2.
"打印界面
selection-screen begin of block blk3 with frame .
select-options:s_zqsid3 for ztsd001-zqsid    modif id ss3,
               s_zqsdt3 for ztsd001-zqsdate  modif id ss3,
               s_kunnr3 for ztsd001-kunnr    modif id ss3,
               s_lifnr3 for ztsd001-lifnr    modif id ss3,
               s_matnr3 for ztsd001-matnr    modif id ss3,
               s_ernam3 for ztsd001-zcreater modif id ss3.
parameters:p_31 radiobutton group 2 modif id ss3, "带金额
           p_32 radiobutton group 2 modif id ss3, "不带金额
           p_33 as checkbox modif id ss3.
selection-screen skip 1 .
selection-screen end of block blk3.
"控制界面
selection-screen begin of block blk4 with frame.
parameters:p_1 radiobutton group 1 default 'X' user-command uc1 modif id ss4,
           p_2 radiobutton group 1 modif id ss4,
           p_3 radiobutton group 1 modif id ss4.

selection-screen end of block blk4.

*-----------------------------------------------------------------------
* AT SELECTION-SCREEN
*-----------------------------------------------------------------------
at selection-screen output.

  case 'X'.
    when p_1.
      loop at screen.
        if screen-group1 = 'SS1' or screen-group1 = 'SS4'.
          screen-invisible = 0 .    """""组 ‘SS1’ 显示
          screen-input = 1.         """""组 ‘SS1’ 显示
        else.
          screen-invisible = 1 .    """""组 ‘SS1’ 显示
          screen-input = 0.         """""组 ‘SS1’ 显示
        endif.
        modify screen.
      endloop.
    when p_2.
      loop at screen.
        if screen-group1 = 'SS2' or screen-group1 = 'SS4'.
          screen-invisible = 0 .    """""组 ‘SS1’ 隐藏
          screen-input = 1.         """""组 ‘SS1’ 显示
        else.
          screen-invisible = 1 .    """""组 ‘SS1’ 显示
          screen-input = 0.         """""组 ‘SS1’ 显示
        endif.
        modify screen.
      endloop.
    when p_3.
      loop at screen.
        if screen-group1 = 'SS3' or screen-group1 = 'SS4'.
          screen-invisible = 0 .    """""组 ‘SS1’ 隐藏
          screen-input = 1.         """""组 ‘SS1’ 显示
        else.
          screen-invisible = 1 .    """""组 ‘SS1’ 显示
          screen-input = 0.         """""组 ‘SS1’ 显示
        endif.
        modify screen.
      endloop.
  endcase.


start-of-selection.

  case 'X'.
    when p_1.
      perform get_prepare_data.
    when p_2.
      perform get_change_data.
      perform process_change_data.
    when p_3.
      perform get_print_data.
  endcase.

  perform display_data.

end-of-selection.
*&---------------------------------------------------------------------*
*&      Form  get_prepare_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form get_prepare_data .
  types: begin of ty_vbap,
        vbeln type vbap-vbeln,
        posnr type vbap-posnr,
        kwmeng type vbap-kwmeng,
        end of ty_vbap.
  types: begin of ty_mdbs,
        ebeln type mdbs-ebeln,
        ebelp type mdbs-ebelp,
        matnr type mdbs-matnr,
        menge type mdbs-menge,
        wemng type mdbs-wemng,
        elikz type mdbs-elikz,
        loekz type mdbs-loekz,
        end of ty_mdbs.
  types: begin of ty_vbkd,
vbeln type vbkd-vbeln,
posnr type vbkd-posnr,
bstkd type vbkd-bstkd,
end of ty_vbkd.


  types: begin of ty_ekko,
      ebeln type ekko-ebeln,
      ebelp type ekpo-ebelp,
      end of ty_ekko.
  data:lt_ekko type table of ty_ekko,
       ls_ekko type ty_ekko.

  data:lt_vbap type table of ty_vbap,
       lt_mdbs type table of ty_mdbs,
       ls_vbap type ty_vbap,
       ls_mdbs type ty_mdbs.

  data:lt_vbkd type table of ty_vbkd,
     ls_vbkd type ty_vbkd.

  data: lt_ztsd001 type table of ztsd001,
        ls_ztsd001 type ztsd001.

  data: lv_menge1 type mdbs-menge.  "本次导入数量+之前导入数量

  field-symbols <fs_alv> type ty_alv.

  select ekko~ebeln
         ekko~bsart
         ekpo~ebelp
         ekpo~matnr
         ekpo~menge
         ekpo~txz01 as maktx
         ekko~lifnr
         lfa1~name1
         ekko~aedat
         ekkn~vbeln
         ekkn~vbelp as posnr
         vbak~kunnr
         vbap~kwmeng
  into corresponding fields of table gt_alv
  from ekko inner join ekpo
    on ekko~ebeln = ekpo~ebeln
            inner join lfa1
    on ekko~lifnr = lfa1~lifnr
            inner join ekkn
    on ekko~ebeln = ekkn~ebeln
   and ekpo~ebelp = ekkn~ebelp
            inner join vbak
    on ekkn~vbeln = vbak~vbeln
            inner join vbap
    on ekkn~vbeln = vbap~vbeln
   and ekkn~vbelp = vbap~posnr
  where ekko~ebeln in s_ebeln1
    and ekkn~vbeln in s_vbeln1
    and vbak~kunnr in s_kunnr1
    and ekko~lifnr in s_lifnr1
    and ekpo~matnr in s_matnr1
    and ekko~aedat in s_erdat1
    and ekko~bsart in ('NB' , 'BH') .

  if gt_alv is not initial.

    select ebeln
           ebelp
           matnr
           menge
           wemng
           elikz
           loekz
    into table lt_mdbs
    from mdbs for all entries in gt_alv
    where ebeln = gt_alv-ebeln
      and ebelp = gt_alv-ebelp
      and matnr = gt_alv-matnr.

    select *
    into table lt_ztsd001
    from ztsd001 for all entries in gt_alv
    where ebeln = gt_alv-ebeln
      and ebelp = gt_alv-ebelp
      and matnr = gt_alv-matnr
      and zmenge1 = 0.

    sort lt_mdbs by ebeln ebelp matnr.

    select vbeln
           posnr
           bstkd
    into table lt_vbkd
    from vbkd for all entries in gt_alv
    where vbeln = gt_alv-vbeln
      and posnr = gt_alv-posnr.
    sort lt_vbkd by vbeln posnr.

    loop at gt_alv assigning <fs_alv>.

      read table lt_vbkd into ls_vbkd with key vbeln = <fs_alv>-vbeln
                                               posnr = <fs_alv>-posnr binary search.
      if sy-subrc = 0.
        <fs_alv>-bstkd = ls_vbkd-bstkd.
      endif.

      read table lt_mdbs into ls_mdbs with key ebeln = <fs_alv>-ebeln
                                               ebelp = <fs_alv>-ebelp
                                               matnr = <fs_alv>-matnr binary search.
      if sy-subrc = 0.
        if ls_mdbs-elikz = 'X' or ls_mdbs-loekz = 'L'.
          delete gt_alv.
          continue.
        endif.
        loop at  lt_ztsd001 into ls_ztsd001 where ebeln = <fs_alv>-ebeln
                                              and ebelp = <fs_alv>-ebelp
                                              and matnr = <fs_alv>-matnr.
          add ls_ztsd001-zmenge to lv_menge1.

        endloop.
        <fs_alv>-menge1 = ls_mdbs-menge - ls_mdbs-wemng - lv_menge1.
        <fs_alv>-zmenge = <fs_alv>-menge1.
        if <fs_alv>-menge1 <= 0.
          delete gt_alv.
          continue.
        endif.
      endif.
      move-corresponding gs_alv to ls_ekko.
      append ls_ekko to lt_ekko.
      clear:lv_menge1.
    endloop.

    sort gt_alv by kunnr bstkd ebeln ebelp.

  endif.

  sort lt_ekko by ebeln ebelp.
  delete adjacent duplicates from lt_ekko comparing all fields.

  loop at lt_ekko into ls_ekko.
    call function 'ENQUEUE_EZMM_043_EKPO'
      exporting
        mode_ekpo      = 'E'
        mandt          = sy-mandt
        ebeln          = ls_ekko-ebeln
        ebelp          = ls_ekko-ebelp
      exceptions
        foreign_lock   = 1
        system_failure = 2
        others         = 3.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  endloop.

endform.                    " get_prepare_data
*&---------------------------------------------------------------------*
*&      Form  GET_CHANGE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form get_change_data .
  types: begin of ty_ekko,
      ebeln type ekko-ebeln,
      bsart type ekko-bsart,
      aedat type ekko-aedat,
      end of ty_ekko.

  data:lt_ekko type table of ty_ekko,
       ls_ekko type ty_ekko.

  field-symbols <fs_alv> type ty_alv.

  select *
  into corresponding fields of table gt_alv
  from ztsd001
  where zqsid    in s_zqsid2
    and zqsdate  in s_zqsdt2
    and kunnr    in s_kunnr2
    and lifnr in s_lifnr2
    and matnr in s_matnr2
    and zcreater in s_ernam2
    and zmenge1 = ''
    and charg   = ''
    and mblnr   = ''
    and zvbeln  = ''
    and zmblnr  = ''
    and lfimg   = ''.

  if gt_alv is not initial.
    select ebeln
           bsart
           aedat
    into table lt_ekko
    from ekko for all entries in gt_alv
    where ebeln = gt_alv-ebeln.
    sort lt_ekko by ebeln.

    loop at gt_alv assigning <fs_alv>.
      read table lt_ekko into ls_ekko with key ebeln = <fs_alv>-ebeln binary search.
      if sy-subrc = 0.
        <fs_alv>-bsart = ls_ekko-bsart.
        <fs_alv>-aedat = ls_ekko-aedat.
      endif.
    endloop.
  endif.
endform.                    " GET_CHANGE_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_PRINT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form get_print_data .
  types: begin of ty_ekko,
      ebeln type ekko-ebeln,
      bsart type ekko-bsart,
      aedat type ekko-aedat,
      end of ty_ekko.

  data:lt_ekko type table of ty_ekko,
       ls_ekko type ty_ekko.

  data: lt_yyckd type table of ztt_yyckd with header line.

  field-symbols <fs_alv> type ty_alv.

  select *
  into corresponding fields of table gt_alv
  from ztsd001
  where zqsid    in s_zqsid3
    and zqsdate  in s_zqsdt3
    and kunnr    in s_kunnr3
    and lifnr in s_lifnr3
    and matnr in s_matnr3
    and zcreater in s_ernam3.

  if gt_alv is not initial.
    select ebeln
           bsart
           aedat
    into table lt_ekko
    from ekko for all entries in gt_alv
    where ebeln = gt_alv-ebeln.
    sort lt_ekko by ebeln.

    loop at gt_alv assigning <fs_alv>.
      read table lt_ekko into ls_ekko with key ebeln = <fs_alv>-ebeln binary search.
      if sy-subrc = 0.
        <fs_alv>-bsart = ls_ekko-bsart.
        <fs_alv>-aedat = ls_ekko-aedat.
      endif.
    endloop.
  endif.

  if gt_alv is not initial.
    select matnr
           groes
           normt
           mfrpn
           meins
      from mara
      into corresponding fields of table t_mara
      for all entries in gt_alv
      where matnr = gt_alv-matnr.

    if sy-subrc <> 0.
      refresh t_mara.
    else.
      sort t_mara by matnr.
    endif.

    data:
    ls_t001 like line of t_t001,
    ls_t001w like line of t_t001,
    lv_butxt type char25.

    select vbak~vbeln
           auart
           bukrs_vf
           vbap~vgbel
           bstnk
           kzwi1
    into corresponding fields of table t_vbak
    from vbak inner join vbap
      on vbak~vbeln = vbap~vbeln
      for all entries in gt_alv
    where vbak~vbeln = gt_alv-vbeln.

    select  bukrs
            butxt
            adrnr
      from  t001
      into corresponding fields of table t_t001
      for all entries in t_vbak
      where bukrs = t_vbak-bukrs_vf.

*-----------------20180409 ISSUE 14987 HAND HZY---------------*
*...如果是公司代码BJ10，则从T001W取ADRNR
    if sy-subrc <> 0.
      refresh t_t001.
    else.
      read table t_t001 into ls_t001 with key bukrs = 'BJ10'.
      if sy-subrc = 0.
        lv_butxt = ls_t001-butxt.                    "把BUXTT拿出来
        delete t_t001 where bukrs = 'BJ10'.         "把原记录先删掉
        select single werks as bukrs                 "公司代码仅有一个同名工厂
*           BUTXT
               adrnr
          from t001w
          into corresponding fields of  ls_t001w
          where werks = 'BJ10'.
        ls_t001w-butxt = lv_butxt.
        append ls_t001w to t_t001.
      endif.
      sort t_t001 by bukrs.
    endif.

    select single * from usr21 where bname = sy-uname.

    select single * from adcp  where persnumber = usr21-persnumber.


    if  adcp-floor =  'ZSD044'.


      select vbpa~vbeln
       vbpa~posnr
       vbpa~kunnr
       vbpa~parvw
       vbpa~adrnr
  from vbpa
  inner join zsd096  on zsd096~kunnr = vbpa~kunnr
  into corresponding fields of table t_vbpa
  for all entries in gt_alv
  where vbpa~vbeln = gt_alv-vbeln and
        vbpa~posnr = 0             and
        ( vbpa~parvw = 'AG' or vbpa~parvw = 'WE' ).

      if t_vbpa[] is not initial.
        sort t_vbpa by vbeln kunnr parvw.
      endif.

    else.

      select vbeln
             posnr
             kunnr
             parvw
             adrnr
        from vbpa
        into corresponding fields of table t_vbpa
        for all entries in gt_alv
        where vbeln = gt_alv-vbeln and
              posnr = 0             and
              ( parvw = 'AG' or parvw = 'WE' ).

      if t_vbpa[] is not initial.
        sort t_vbpa by vbeln kunnr parvw.
      endif.

    endif.

    select kunnr
           name1
      into corresponding fields of table t_knvk
      from knvk
      for all entries in gt_alv
      where kunnr eq  gt_alv-kunnr.

    select kunnr
           name1
      from kna1
      into corresponding fields of table t_kna1
      for all entries in gt_alv
      where kunnr = gt_alv-kunnr.

    if t_kna1[] is not initial.
      sort t_kna1 by kunnr.
    endif.

    if t_t001[] is not initial.

      select addrnumber
             street
             city1
             post_code1
             tel_number
             fax_number
             mc_street
             sort2                                 "搜索项第二个
        from adrc
        into corresponding fields of table t_adrc
        for all entries in t_t001
        where addrnumber = t_t001-adrnr.

    endif.


    if t_vbpa[] is not initial.

      select addrnumber
             street
             city1
             post_code1
             tel_number
             fax_number
             mc_street
             house_num1
        from adrc
        appending corresponding fields of table t_adrc
        for all entries in t_vbpa
        where addrnumber = t_vbpa-adrnr.

    endif.

    if t_adrc[] is not initial.
      sort t_adrc  by addrnumber.
    endif.

    select auart
         bezei
    from tvakt
    into corresponding fields of table t_tvakt
    for all entries in t_vbak
    where auart = t_vbak-auart
      and spras = sy-langu.
    if sy-subrc = 0.
      sort t_tvakt  by auart.
    endif.
  endif.

  data: lw_prt_header   like line of t_prt_header,
        lt_prt_detail   like t_prt_detail.

  loop at gt_alv into gs_alv.
    "get company
    read table t_vbak with key vbeln = gs_alv-vbeln.
    if sy-subrc = 0.
      read table t_t001 with key bukrs = t_vbak-bukrs_vf binary search.
      if sy-subrc = 0.
        read table t_adrc with key addrnumber = t_t001-adrnr binary search.
      endif.
    endif.

    lw_prt_header-bstnk = t_vbak-bstnk.
    lw_prt_header-bukrs = t_vbak-bukrs_vf.
    lw_prt_header-butxt = t_t001-butxt.

    if t_vbak-bukrs_vf = 'S010' or t_vbak-bukrs_vf = 'S030'.

      lw_prt_header-gs_addr = '上海市钦州北路1001号12幢101'.

    elseif lw_prt_header-bukrs = 'BJ10'.
      lw_prt_header-gs_addr = t_adrc-sort2.
    else.
      lw_prt_header-gs_addr =  t_adrc-street.
    endif.

    lw_prt_header-gs_tel = t_adrc-tel_number.
    lw_prt_header-gs_fax = t_adrc-fax_number.
    lw_prt_header-vgbel = gs_alv-vbeln.
    lw_prt_header-vgbel1 = t_vbak-vgbel.

    data:a like proj-pspid.

    concatenate gs_alv-vbeln+0(1) gs_alv-bstkd+2(11) into a.
    select post1
      from proj
      into lw_prt_header-sh_post1
      where  pspid eq a.
    endselect.
    clear:  t_t001, t_adrc.
    clear:a.

    "get cust sp
    data: lw_vbpa like line of t_vbpa,
          lw_kna1 like line of t_kna1,
          lw_knvk like line of t_knvk.

    clear lw_vbpa.
    read table t_vbpa into lw_vbpa with key vbeln = gs_alv-vbeln
*                                          KUNNR = gs_alv-KUNNR
                                             kunnr = gs_alv-kunnr
                                            parvw = 'AG'
                                   binary search.

    clear lw_kna1.
    read table t_kna1 into lw_kna1 with key kunnr = lw_vbpa-kunnr
                                   binary search.

    clear t_adrc.
    read table t_adrc with key addrnumber = lw_vbpa-adrnr
                                   binary search.

    clear lw_knvk.
    read table t_knvk into lw_knvk with key kunnr = lw_vbpa-kunnr
                                   binary search.


    lw_prt_header-sp_kunnr = lw_kna1-kunnr.

    if lw_kna1-kunnr = '0002000269'.
      lw_kna1-name1 = '合富（中国）医疗科技股份有限公司（上药青浦医疗委托直发）'.
      lw_prt_header-sp_name1 = lw_kna1-name1.

    else.

      lw_prt_header-sp_name1 = lw_kna1-name1.
    endif.
    lw_prt_header-sp_addr =  t_adrc-street.

    lw_prt_header-sp_city1 = t_adrc-city1.
    clear: lw_vbpa, t_adrc, lw_kna1.

    read table t_vbpa into lw_vbpa with key vbeln = gs_alv-vbeln
*                                          POSNR = gs_alv-POSNR
                                              kunnr = gs_alv-kunnr
                                              parvw = 'WE'.

    clear lw_kna1.
    read table t_kna1 into lw_kna1 with key kunnr = lw_vbpa-kunnr.

    clear t_adrc.
    read table t_adrc into t_adrc with key addrnumber = lw_vbpa-adrnr.
    clear lw_knvk.
    read table t_knvk into lw_knvk with key kunnr = lw_vbpa-kunnr.
    lw_prt_header-sh_kunnr = lw_kna1-kunnr.



    lw_prt_header-sh_name1 = lw_kna1-name1.
    lw_prt_header-sh_addr =  t_adrc-street.
    lw_prt_header-sh_tel = t_adrc-tel_number.



    lw_prt_header-sh_per = lw_knvk-name1.
    lw_prt_header-sh_per = t_adrc-house_num1.
    lw_prt_header-sh_city1 = t_adrc-city1.
    lw_prt_header-sh_post  = t_adrc-post_code1.
    clear: lw_vbpa, t_adrc, lw_kna1.

    "get auart
    data: lw_tvakt like line of t_tvakt.

    clear lw_tvakt.
    read table t_tvakt into lw_tvakt with key auart = t_vbak-auart
                                   binary search.
    check sy-subrc = 0.

    lw_prt_header-auart_txt = lw_tvakt-bezei.
    lw_prt_header-zqsid = gs_alv-zqsid.

    select single name_text into lw_prt_header-zbr
  from v_username
  where bname = gs_alv-zcreater.

    move-corresponding gs_alv to wa_prt_detail.

    perform sub_get_material   using wa_prt_detail-matnr
                            changing wa_prt_detail.
    wa_prt_detail-budat = gs_alv-zqsdate.
    wa_prt_detail-xq = gs_alv-zdate. "效期
    wa_prt_detail-lfimg = gs_alv-zmenge. "数量

    if gs_alv-kwmeng ne 0.
      wa_prt_detail-sj = t_vbak-kzwi1 / gs_alv-kwmeng * gs_alv-zmenge.
    endif.

    wa_prt_detail-arktx = gs_alv-maktx.
    wa_prt_detail-posnr = gs_alv-zposnr.
    wa_prt_detail-zqsid = gs_alv-zqsid.

    append wa_prt_detail to t_prt_detail.
    append lw_prt_header to t_prt_header.
  endloop.

  sort t_prt_header by zqsid.
  delete adjacent duplicates from t_prt_header comparing zqsid.

  if t_prt_detail[] is not initial.
    select *
      into corresponding fields of table lt_yyckd[]
      from ztt_yyckd
      for all entries in t_prt_detail[]
      where zckdh = t_prt_detail-vbeln
      and zposnr = t_prt_detail-posnr
      and zmatnr = t_prt_detail-matnr.
  endif.

  data:lw_header type zmm_rp043_header.
  data:lt_detail type zmm_rp043_detail.
  data:ind1 type i.
  data: l_knumv like vbak-knumv.
  data: l_kbetr like konv-kbetr.
  data: l_vbeln like vbap-vbeln.
  data: t_class      like sclass occurs 0 with header line,
  t_objectdata like clobjdat occurs 0 with header line.

  data: l_cuobj     like ausp-objek,
        l_date(10)  type c,
        l_regi_date like mseg-hsdat.
  clear ind1.
  loop at t_prt_header into lw_header.
    loop at t_prt_detail into lt_detail where zqsid = lw_header-zqsid.
      if lt_detail-matnr = '000000002000012678'.

        lt_detail-msehl = '箱'.

      endif.

      if lw_header-sp_kunnr = '0001000550' or lw_header-sp_kunnr = '0001000620' .
        select single maktx into lt_detail-arktx from makt where matnr =  lt_detail-matnr and spras = 1.
      endif.

      ind1 = ind1 + 1 .
      clear l_knumv.
      clear l_kbetr.
      clear l_vbeln.
      select single vbelv into l_vbeln from vbfa where vbeln =  lt_detail-vbeln.

      if lw_header-sp_kunnr = '0001000300' or lw_header-sp_kunnr = '0001000610'.
        select single zzzcz into lt_detail-arktx from mara where  matnr =  lt_detail-matnr and zzzcz <> ''.

      endif.

      if  lw_header-sp_kunnr = '0001000540'.

        select single maktx into lt_detail-arktx from makt where  matnr =  lt_detail-matnr
          and spras = '1' .

      endif.


      h = '0001000563'.

      if lw_header-sp_kunnr = h.
        select single maktx into lt_detail-arktx from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and maktx <> ''.
        select single msehl into lt_detail-msehl from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and msehl <> ''.
        select single groes into lt_detail-groes from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and groes <> ''.
      endif.

      s = '0001000535'.

      if lw_header-sp_kunnr = s.
        select single maktx into lt_detail-arktx from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and maktx <> ''.
        select single msehl into lt_detail-msehl from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and msehl <> ''.
        select single groes into lt_detail-groes from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and groes <> ''.
      endif.

*出库单打印物料描述更改 20121128 evan
      a = '0001000455'.

      if lw_header-sp_kunnr = a.
        select single maktx into lt_detail-arktx from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and maktx <> ''.
        select single msehl into lt_detail-msehl from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and msehl <> ''.

        select single groes into lt_detail-groes from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and groes <> ''.

      endif.



      ab = '0001000535'.

      if lw_header-sp_kunnr =  ab.
        select single maktx into lt_detail-arktx from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and maktx <> ''.
        select single msehl into lt_detail-msehl from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and msehl <> ''.

        select single groes into lt_detail-groes from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and groes <> ''.

      endif.


      x = '0001000531'.

      if lw_header-sp_kunnr =  x.
        select single maktx into lt_detail-arktx from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and maktx <> ''.
        select single msehl into lt_detail-msehl from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and msehl <> ''.

        select single groes into lt_detail-groes from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and groes <> ''.
        select single normt into lt_detail-normt from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and normt <> ''.
      endif.

      w = '0001000423'.

      if lw_header-sp_kunnr = w.
        select single maktx into lt_detail-arktx from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and maktx <> ''.

      endif.



      v = '0001000540'.

      if lw_header-sp_kunnr = v.

        select single msehl into lt_detail-msehl from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and msehl <> ''.
        select single groes into lt_detail-groes from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and groes <> ''.
        select single maktx into lt_detail-arktx from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and maktx <> ''.
      endif.

      b = '0001000511'.

      if lw_header-sp_kunnr = b.
        select single maktx into lt_detail-arktx from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and maktx <> ''.
        select single msehl into lt_detail-msehl from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and msehl <> ''.

        select single groes into lt_detail-groes from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and groes <> ''.

      endif.


      d = '0001000300'.


      if lw_header-sp_kunnr = d.
        select single maktx into lt_detail-arktx from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and maktx <> ''.
        select single msehl into lt_detail-msehl from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and msehl <> ''.

        select single groes into lt_detail-groes from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and groes <> ''.

      endif.

*********************

*出库单打印物料描述更改 20121128 evan
      if   l_vbeln is not initial.
        select single knumv into l_knumv from vbak where vbeln = l_vbeln.
        if l_knumv is not initial.
          select single kbetr into l_kbetr from konv where knumv = l_knumv and kschl = 'ZPR1' and kposn = lt_detail-posnr.
          lt_detail-kbetr = l_kbetr.
          modify t_prt_detail from lt_detail.
        endif.
      endif.
*---------------------------------------------


* sgtxt物料文件的內文，如果沒將原本 user key的內文export給e_sgtxt就變成空的了
* 有user exit就需特別處理這段
*  e_sgtxt = i_mseg-sgtxt.

* 以料號get classification dat
      l_cuobj+0(18)   =  lt_detail-matnr.
***    物料主数据分类中的特性的读取
*  break-point.
      call function 'CLAF_CLASSIFICATION_OF_OBJECTS'
        exporting
          classtype          = '001'
          object             = l_cuobj
        tables
          t_class            = t_class
          t_objectdata       = t_objectdata
        exceptions
          no_classification  = 1
          no_classtypes      = 2
          invalid_class_type = 3
          others             = 4.

      if sy-subrc = 0.

        loop at t_objectdata.
          case t_objectdata-atnam.
            when 'REGISTRATION_NO'.

              if t_objectdata-ausp1 = '?'.

                t_objectdata-ausp1 = ''.
              endif.
              lt_detail-zhucz = t_objectdata-ausp1.
*          concatenate t_objectdata-ausp1+(4) t_objectdata-ausp1+5(2) t_objectdata-ausp1+8(2)
*                      into l_date.
            when 'TEMPERATURE_UPPER'.

              if t_objectdata-ausp1 = '?'.

                t_objectdata-ausp1 = ''.
              endif.

              lt_detail-wendh = t_objectdata-ausp1.
            when 'TEMPERATURE_LOWER'.
              if t_objectdata-ausp1 = '?'.

                t_objectdata-ausp1 = ''.
              endif.

              lt_detail-wendl = t_objectdata-ausp1.
            when 'MANUFACTURER'. " 新增制造商 add by Evan 20111111

              if t_objectdata-ausp1 = '?'.

                t_objectdata-ausp1 = ''.
              endif.
              lt_detail-shengcs = t_objectdata-ausp1.

          endcase.
          clear t_objectdata.
        endloop.
      endif.
*****ADD BY ANDY
      if lw_header-sp_kunnr = b.
        select single zzczh into lt_detail-zhucz from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzczh <> ''.
        select single zzzs into lt_detail-shengcs from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzzs <> ''.
        select single zup into lt_detail-wendh from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zup <> ''.
        select single zdp into lt_detail-wendl from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zdp <> ''.

        shift lt_detail-wendh left deleting leading '0'.
        shift lt_detail-wendl left deleting leading '0'.
***********************
      endif.



      if lw_header-sp_kunnr = h.
        select single zzczh into lt_detail-zhucz from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzczh <> ''.
        select single zzzs into lt_detail-shengcs from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzzs <> ''.
        select single zup into lt_detail-wendh from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zup <> ''.
        select single zdp into lt_detail-wendl from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zdp <> ''.

        shift lt_detail-wendh left deleting leading '0'.
        shift lt_detail-wendl left deleting leading '0'.


      endif.


      if lw_header-sp_kunnr = w.
        select single zzczh into lt_detail-zhucz from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzczh <> ''.

      endif.



      if lw_header-sp_kunnr = s.
        select single zzczh into lt_detail-zhucz from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzczh <> ''.
        select single zzzs into lt_detail-shengcs from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzzs <> ''.
        select single zup into lt_detail-wendh from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zup <> ''.
        select single zdp into lt_detail-wendl from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zdp <> ''.

        shift lt_detail-wendh left deleting leading '0'.
        shift lt_detail-wendl left deleting leading '0'.


      endif.



      if lw_header-sp_kunnr = x.
        select single zzczh into lt_detail-zhucz from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzczh <> ''.
        select single zzzs into lt_detail-shengcs from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzzs <> ''.
        select single zup into lt_detail-wendh from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zup <> ''.
        select single zdp into lt_detail-wendl from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zdp <> ''.
        select single normt into lt_detail-normt from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and normt <> ''.
        shift lt_detail-wendh left deleting leading '0'.
        shift lt_detail-wendl left deleting leading '0'.


      endif.



      if lw_header-sp_kunnr = ab.
        select single zzczh into lt_detail-zhucz from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzczh <> ''.
        select single zzzs into lt_detail-shengcs from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzzs <> ''.
        select single zup into lt_detail-wendh from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zup <> ''.
        select single zdp into lt_detail-wendl from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zdp <> ''.

        shift lt_detail-wendh left deleting leading '0'.
        shift lt_detail-wendl left deleting leading '0'.
***********************
      endif.

      if lw_header-sp_kunnr = v.
        select single zzczh into lt_detail-zhucz from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzczh <> ''.
        select single zzzs into lt_detail-shengcs from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzzs <> ''.
      endif.




      if lw_header-sp_kunnr = d.
        select single zzczh into lt_detail-zhucz from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzczh <> ''.
        select single zzzs into lt_detail-shengcs from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzzs <> ''.
        select single zup into lt_detail-wendh from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zup <> ''.
        select single zdp into lt_detail-wendl from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zdp <> ''.

        shift lt_detail-wendh left deleting leading '0'.
        shift lt_detail-wendl left deleting leading '0'.
***********************
      endif.


      f = '0001000530'.

      if lw_header-sp_kunnr = f.

        select single zzczh into lt_detail-zhucz from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzczh <> ''.

      endif.

      g = '0001000101'.

      if lw_header-sp_kunnr = g.

        select single zzczh into lt_detail-zhucz from zsd020 where kunnr = lw_header-sp_kunnr and matnr =  lt_detail-matnr and zzczh <> ''.

      endif.



      if lw_header-sp_kunnr = '0001000550' or lw_header-sp_kunnr = '0001000620' .
        select single maktx into lt_detail-arktx from makt where matnr =  lt_detail-matnr and spras = 1.
      endif.

      read table lt_yyckd with key zckdh = lt_detail-vbeln zposnr = lt_detail-posnr.
      if sy-subrc = 0.
        lt_detail-zmaktx = lt_yyckd-zmaktx.
        lt_detail-zmenge = lt_yyckd-zmenge.
        lt_detail-zmeins = lt_yyckd-zmeins.
        lt_detail-zprice = lt_yyckd-zprice.
        lt_detail-ztotal = lt_yyckd-ztotal.
        lt_detail-zplace = lt_yyckd-zplace.   "add 产地 发票号码字段 BY HAND LP
        lt_detail-zfphm = lt_yyckd-zfphm.
        if lw_header-sp_kunnr = '0001000545'.
          lt_detail-zscrq = lt_yyckd-zscrq.
        endif.
      endif.

      modify t_prt_detail from lt_detail.
    endloop.
  endloop.





endform.                    " GET_PRINT_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form display_data .

  perform frm_build_layout."格式
  perform frm_build_fieldcat.
  perform frm_alv_output. "输出

endform.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  frm_build_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form frm_build_layout .
  clear gs_layo.
  gs_layo-cwidth_opt = 'X'.
  gs_layo-zebra      = 'X'.
  gs_layo-box_fname      = 'BOX'.
endform.                    "frm_build_layout
*&---------------------------------------------------------------------*
*&      Form  frm_alv_output
*&---------------------------------------------------------------------*
*       text
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
  clear:gt_fcat.
  case 'X'.
    when p_1.
      perform add_fieldcat using:'BOX' '复选框' '' ' '        'X'," 复选框
                                 'LIGHT' '状态灯' '' '' ''," 状态灯
                                'ZQSID' '客户签收单号' 'ZTSD001' 'ZQSID' ''," 客户签收单号
                                'ZPOSNR' '行项目' 'ZTSD001' 'ZPOSNR' ''," 行项目

                                'MATNR' '物料' 'ZTSD001' 'MATNR' ''," 物料

                                 'LICHA' '供应商批次' 'ZTSD001' 'LICHA' 'X'," 供应商批次
                                'ZDATE' '效期' 'ZTSD001' 'ZDATE' 'X'," 效期
                                'ZMENGE' '数量' 'ZTSD001' 'ZMENGE' 'X'," 数量

                                'LIFNR' '供应商编号' 'ZTSD001' 'LIFNR' ''," 供应商编号
                                'NAME1' '供应商名称' 'ZTSD001' 'NAME1' ''," 供应商名称
                                'BSART' '采购订单类型' 'ZTSD001' 'BSART' ''," 采购订单类型
                                'AEDAT' '采购订单日期' 'ZTSD001' 'AEDAT' ''," 采购订单日期
                                'EBELN' '采购订单' 'ZTSD001' 'EBELN' ''," 采购订单
                                'EBELP' '采购订单行项目' 'ZTSD001' 'EBELP' ''," 采购订单行项目
                                'MENGE' '采购订单数量' 'ZTSD001' 'MENGE' ''," 采购订单数量
                                'MENGE1' '可导入数量' 'ZTSD001' 'MENGE' ''," 可导入数量

                                'ZQSDATE' '创建日期' 'ZTSD001' 'ZQSDATE' ''," 创建日期
                                'ZCREATER' '创建者' 'ZTSD001' 'ZCREATER' ''," 创建者
                                'KUNNR' '客户编号' 'ZTSD001' 'KUNNR' ''," 客户编号
                                'VBELN' '销售订单' 'ZTSD001' 'VBELN' ''," 销售订单
                                'POSNR' '销售凭证项目' 'ZTSD001' 'POSNR' ''," 销售凭证项目
                                'BSTKD' '纸质合约号' '' '' ''," 纸质合约号

                                'MAKTX' '物料描述' 'ZTSD001' 'MAKTX' ''," 物料描述
                                'KWMENG' '销售订单数量' 'ZTSD001' 'KWMENG' ''," 销售订单数量


                                'ZMSG' '消息' '' '' ''." 消息


    when p_2.
      perform add_fieldcat using:'BOX' '复选框' '' ' '        'X'," 复选框
                                  'LIGHT' '状态灯' '' '' ''," 状态灯
                                  'ZQSID' '客户签收单号' 'ZTSD001' 'ZQSID  ' ''," 客户签收单号
                                  'ZPOSNR' '行项目' 'ZTSD001' 'ZPOSNR '      ''," 行项目

                                   'MATNR' '物料' 'ZTSD001' 'MATNR  '         ''," 物料

                                   'LICHA' '供应商批次' 'ZTSD001' 'LICHA  '   'X'," 供应商批次
                                  'ZDATE' '效期' 'ZTSD001' 'ZDATE  '         'X'," 效期
                                  'ZMENGE' '数量' 'ZTSD001' 'ZMENGE '        'X'," 数量


                                  'LIFNR' '供应商编号' 'ZTSD001' 'LIFNR  '   ''," 供应商编号
                                  'NAME1' '供应商名称' 'ZTSD001' 'NAME1  '   ''," 供应商名称
                                  'EBELN' '采购订单' 'ZTSD001' 'EBELN  '     ''," 采购订单
                                  'EBELP' '采购订单行项目' 'ZTSD001' 'EBELP  ' ''," 采购订单行项目
                                  'MENGE' '采购订单数量' 'ZTSD001' 'MENGE  ' ''," 采购订单数量
                                  'MENGE1' '可修改数量' 'ZTSD001' 'MENGE  '  ''," 可修改数量

                                  'ZQSDATE' '创建日期' 'ZTSD001' 'ZQSDATE'   ''," 创建日期
                                  'KUNNR' '客户编号' 'ZTSD001' 'KUNNR  '     ''," 客户编号
                                  'VBELN' '销售订单' 'ZTSD001' 'VBELN  '     ''," 销售订单
                                  'POSNR' '销售凭证项目' 'ZTSD001' 'POSNR  ' ''," 销售凭证项目

                                  'MAKTX' '物料描述' 'ZTSD001' 'MAKTX  '     ''," 物料描述
                                  'KWMENG' '销售订单数量' 'ZTSD001' 'KWMENG ' ''," 销售订单数量


                                  'ZMSG' '消息' '' ''                        ''.              " 消息
    when p_3.
      perform add_fieldcat using:
                               'BOX' '复选框' '' ' '        'X'," 复选框
                               'ZQSID' '客户签收单号' 'ZTSD001' 'ZQSID' ''," 客户签收单号
                                'ZPOSNR' '行项目' 'ZTSD001' 'ZPOSNR' ''," 行项目

                                'LIFNR' '供应商编号' 'ZTSD001' 'LIFNR' ''," 供应商编号
                                'NAME1' '供应商名称' 'ZTSD001' 'NAME1' ''," 供应商名称
                                'BSART' '采购订单类型' 'ZTSD001' 'BSART' ''," 采购订单类型
                                'AEDAT' '采购订单日期' 'ZTSD001' 'AEDAT' ''," 采购订单日期
                                'EBELN' '采购订单' 'ZTSD001' 'EBELN' ''," 采购订单
                                'EBELP' '采购订单行项目' 'ZTSD001' 'EBELP' ''," 采购订单行项目
                                'MENGE' '采购订单数量' 'ZTSD001' 'MENGE' ''," 采购订单数量
                                'MENGE1' '可导入数量' 'ZTSD001' 'MENGE' ''," 可导入数量
                                'LICHA' '供应商批次' 'ZTSD001' 'LICHA' ''," 供应商批次

                                'ZQSDATE' '创建日期' 'ZTSD001' 'ZQSDATE' ''," 创建日期
                                'ZCREATER' '创建者' 'ZTSD001' 'ZCREATER' ''," 创建者
                                'KUNNR' '客户编号' 'ZTSD001' 'KUNNR' ''," 客户编号
                                'VBELN' '销售订单' 'ZTSD001' 'VBELN' ''," 销售订单
                                'POSNR' '销售凭证项目' 'ZTSD001' 'POSNR' ''," 销售凭证项目
                                'MATNR' '物料' 'ZTSD001' 'MATNR' ''," 物料
                                'MAKTX' '物料描述' 'ZTSD001' 'MAKTX' ''," 物料描述
                                'KWMENG' '销售订单数量' 'ZTSD001' 'KWMENG' ''," 销售订单数量

                                'ZDATE' '效期' 'ZTSD001' 'ZDATE' ''," 效期
                                'ZMENGE' '数量' 'ZTSD001' 'ZMENGE' ''." 数量
  endcase.




endform.                    " FRM_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDCAT
*&---------------------------------------------------------------------*
*       增加字段
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
*&      Form  PROCESS_CHANGE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form process_change_data .
  types: begin of ty_mdbs,
      ebeln type mdbs-ebeln,
      ebelp type mdbs-ebelp,
      matnr type mdbs-matnr,
      menge type mdbs-menge,
      wemng type mdbs-wemng,
      end of ty_mdbs.

  data:lt_mdbs type table of ty_mdbs,
       ls_mdbs type ty_mdbs.

  data:ls_alv type ty_alv.
  data:lv_sum_menge type ztsd001-menge.

  if gt_alv is not initial.
    select ebeln
           ebelp
           matnr
           menge
           wemng
    into table lt_mdbs
    from mdbs for all entries in gt_alv
    where ebeln = gt_alv-ebeln
      and ebelp = gt_alv-ebelp
      and matnr = gt_alv-matnr.
  endif.

  loop at gt_alv into gs_alv .
    loop at gt_alv into ls_alv where ebeln = gs_alv-ebeln and ebelp = gs_alv-ebelp.
      add ls_alv-zmenge to lv_sum_menge.
    endloop.
    read table lt_mdbs into ls_mdbs with key ebeln = gs_alv-ebeln
                                             ebelp = gs_alv-ebelp
                                             matnr = gs_alv-matnr.
    if sy-subrc = 0.
      gs_alv-menge1 = ls_mdbs-menge - ls_mdbs-wemng - lv_sum_menge + gs_alv-zmenge.
    endif.
    call function 'ENQUEUE_EZTSD001'
      exporting
        mode_ztsd001   = 'E'
        mandt          = sy-mandt
        zqsid          = gs_alv-zqsid
        zposnr         = gs_alv-zposnr
      exceptions
        foreign_lock   = 1
        system_failure = 2
        others         = 3.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
    modify gt_alv from gs_alv.
    clear:gs_alv,lv_sum_menge.
  endloop.

endform.                    " PROCESS_CHANGE_DATA
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
  case 'X'.
    when p_1.
      set pf-status 'STANDARD1' .
    when p_2.
      set pf-status 'STANDARD' .
    when p_3.
      set pf-status 'STANDARD2' .
  endcase.


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

  "后续需要刷新屏幕时必须有以下语句


  call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    importing
      e_grid = lo_grid.

  call method lo_grid->check_changed_data.
  case 'X'.
    when p_1.
      case pv_ucomm.
        when '&DATA_SAVE'.
          perform save_prepare_data.
      endcase.
    when p_2.
      case pv_ucomm.
        when '&DATA_SAVE'.
          perform save_change_data.
        when '&DELE_LINE'.
          perform delete_one_line.
        when '&DELE_ALL'.
          perform delete_on_zqsid.
      endcase.
    when p_3.
      case pv_ucomm.
        when '&PRINT'.
          perform print_data.
        when '&EXCEL'.
          perform excxel_output.
      endcase.
  endcase.

  case pv_ucomm.
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

  ps_selfield-refresh = 'X'.
endform.                    "frm_user_command
*&---------------------------------------------------------------------*
*&      Form  save_prepare_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_prepare_data .

  data:lt_ztsd001 type table of ztsd001,
       ls_ztsd001 type ztsd001.
  types: begin of ty_vbkd,
        bstkd type vbkd-bstkd,
        end of ty_vbkd.

  data:lt_vbkd type table of ty_vbkd,
       ls_vbkd type ty_vbkd.
  data:lv_num type i.
  data:lv_tabix type sy-tabix.
  field-symbols <fs_alv> type ty_alv.
  clear lt_ztsd001.
  loop at gt_alv assigning <fs_alv> where box = 'X'.

    if <fs_alv>-licha is initial.
      <fs_alv>-light = icon_red_light.
      <fs_alv>-zmsg  = '供应商批次为空'.
      continue.
    endif.

    if <fs_alv>-zdate is initial.
      <fs_alv>-light = icon_red_light.
      <fs_alv>-zmsg  = '效期为空'.
      continue.
    endif.

    if <fs_alv>-zmenge is initial.
      <fs_alv>-light = icon_red_light.
      <fs_alv>-zmsg  = '数量为空'.
      continue.
    endif.

    if <fs_alv>-zmenge > <fs_alv>-menge1.
      <fs_alv>-light = icon_red_light.
      <fs_alv>-zmsg  = '输入的数量大于可导入数量'.
      continue.
    endif.

    <fs_alv>-light = icon_green_light.
  endloop.



  clear:lv_tabix, lv_num.
  loop at gt_alv into gs_alv where light = icon_green_light.
    add 1 to lv_tabix.
    if lv_tabix = '1'.
      ls_vbkd-bstkd = gs_alv-bstkd.
      append ls_vbkd to lt_vbkd.
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

    read table lt_vbkd transporting no fields with key bstkd = gs_alv-bstkd.
    if sy-subrc ne 0 and lt_ztsd001 is not initial .
      perform frm_generate_qsid tables lt_ztsd001.
      clear lv_num.
      add gs_alv-ebeln_n to lv_num.
      ls_vbkd-bstkd = gs_alv-bstkd.
      append ls_vbkd to lt_vbkd.
    endif.
    move-corresponding gs_alv to ls_ztsd001.
    append ls_ztsd001 to lt_ztsd001.
    clear:ls_vbkd, gs_alv.
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
      gs_alv-zqsdate = ls_ztsd001-zqsdate.
      gs_alv-zcreater = ls_ztsd001-zcreater.
    endif.
    modify gt_alv from gs_alv.
    clear:gs_alv,ls_ztsd001.
  endloop.

  if lt_ztsd001 is not initial.
    modify ztsd001 from table lt_ztsd001.
  else.
    exit.
  endif.

  if sy-subrc = 0.
    loop at gt_alv assigning <fs_alv> where light = icon_green_light.
      <fs_alv>-light = icon_green_light.
      <fs_alv>-zmsg  = '保存成功'.
    endloop.
    commit work and wait.
  else.
    loop at gt_alv assigning <fs_alv> where light = icon_green_light.
      <fs_alv>-light = icon_red_light.
      <fs_alv>-zmsg  = '保存失败'.
    endloop.
    rollback work.
  endif.
endform.                    " save_prepare_data
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
  read table pt_ztsd001 transporting no fields with key zqsid = ''.
  if sy-subrc = 0.
    lv_zqsid = zcl_mm042_util=>get_zqsid( zdate = sy-datum ).
  endif.

  loop at pt_ztsd001 into ls_ztsd001 where zqsid is initial.
    add 1 to  lv_zposnr.
    ls_ztsd001-zposnr = lv_zposnr.
    ls_ztsd001-zqsid  = lv_zqsid.
    ls_ztsd001-zqsdate = sy-datum.
    ls_ztsd001-zcreater = sy-uname.
    modify pt_ztsd001 from ls_ztsd001.
    clear:ls_ztsd001.
  endloop.
endform.                    " FRM_INSERT_TABLE
*&---------------------------------------------------------------------*
*&      Form  SAVE_CHANGE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_change_data .
  data:lt_ztsd001 type table of ztsd001,
       ls_ztsd001 type ztsd001.
  data:lt_ztsd002 type table of ztsd002,
       ls_ztsd002 type ztsd002.
  field-symbols <fs_alv> type ty_alv.
  clear lt_ztsd001.
  loop at gt_alv assigning <fs_alv> where box = 'X'.
    if <fs_alv>-zmenge > <fs_alv>-menge1.
      <fs_alv>-light = icon_red_light.
      <fs_alv>-zmsg  = '数量必须小于可修改数量'.
      continue.
    endif.
    <fs_alv>-light = icon_green_light.
  endloop.

  loop at gt_alv into gs_alv where box = 'X' and light = icon_green_light.
    move-corresponding gs_alv to ls_ztsd001.
    move-corresponding gs_alv to ls_ztsd002.
    ls_ztsd002-zuser = sy-uname.
    ls_ztsd002-zaedat = sy-datum.
    ls_ztsd002-ztime = sy-uzeit.

    append ls_ztsd001 to lt_ztsd001.
    append ls_ztsd002 to lt_ztsd002.

    clear:ls_ztsd001,gs_alv,ls_ztsd002.
  endloop.

  if lt_ztsd001 is not initial .
    modify ztsd001 from table lt_ztsd001.

    if sy-subrc = 0.
      loop at gt_alv assigning <fs_alv> where box = 'X' and light = icon_green_light.
        <fs_alv>-zmsg  = '修改成功'.
      endloop.
      modify ztsd002 from table lt_ztsd002.
      commit work and wait.
    else.
      loop at gt_alv assigning <fs_alv> where box = 'X' and light = icon_green_light.
        <fs_alv>-light = icon_red_light.
        <fs_alv>-zmsg  = '修改失败'.
      endloop.
      rollback work.
    endif.
  endif.

endform.                    " SAVE_CHANGE_DATA
*&---------------------------------------------------------------------*
*&      Form  DELETE_ONE_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_one_line .
  data:lt_ztsd001 type table of ztsd001,
       ls_ztsd001 type ztsd001.
  data:lt_ztsd002 type table of ztsd002,
       ls_ztsd002 type ztsd002.
  clear lt_ztsd001.
  loop at gt_alv into gs_alv where box = 'X'.
    move-corresponding gs_alv to ls_ztsd001.
    move-corresponding gs_alv to ls_ztsd002.
    ls_ztsd002-zuser = sy-uname.
    ls_ztsd002-zaedat = sy-datum.
    ls_ztsd002-ztime = sy-uzeit.
    ls_ztsd002-zdelete = abap_true.

    append ls_ztsd001 to lt_ztsd001.
    append ls_ztsd002 to lt_ztsd002.
    delete gt_alv.
    clear:ls_ztsd001,gs_alv,ls_ztsd002.
  endloop.

  delete ztsd001 from table lt_ztsd001.

  if sy-subrc = 0.
    message '删除成功' type 'S'.
    modify ztsd002 from table lt_ztsd002.
    commit work and wait.

  else.
    message '删除失败' type 'S' display like 'E'.
    rollback work.
  endif.
endform.                    " DELETE_ONE_LINE
*&---------------------------------------------------------------------*
*&      Form  DELETE_ON_ZQSID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_on_zqsid .
  data:lt_ztsd001 type table of ztsd001,
       ls_ztsd001 type ztsd001.
  data:lt_ztsd002 type table of ztsd002,
       ls_ztsd002 type ztsd002.
  ranges:lr_zqsid for ztsd001-zqsid.
  clear: lt_ztsd001,lr_zqsid,lr_zqsid[].

  lr_zqsid-sign = 'I'.
  lr_zqsid-option = 'EQ'.
  loop at gt_alv into gs_alv where box = 'X'.
    lr_zqsid-low = gs_alv-zqsid.
    append lr_zqsid.
  endloop.

  sort lr_zqsid by low.
  delete adjacent duplicates from lr_zqsid comparing all fields.

  select * into table lt_ztsd001 from ztsd001 where zqsid in lr_zqsid.
  if sy-subrc = 0.
    delete ztsd001 from table lt_ztsd001.
    if sy-subrc = 0.
      loop at gt_alv into gs_alv where zqsid in lr_zqsid.
        move-corresponding gs_alv to ls_ztsd002.
        ls_ztsd002-zuser = sy-uname.
        ls_ztsd002-zaedat = sy-datum.
        ls_ztsd002-ztime = sy-uzeit.
        ls_ztsd002-zdelete = abap_true.

        append ls_ztsd002 to lt_ztsd002.
        delete gt_alv.
        clear:ls_ztsd002,gs_alv.
      endloop.
      message '删除成功' type 'S'.
      commit work and wait.
    else.
      message '删除失败' type 'S' display like 'E'.
      rollback work.
    endif.

  endif.

endform.                    " DELETE_ON_ZQSID
*&---------------------------------------------------------------------*
*&      Form  PRINT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form print_data .

  data:lt_alv type table of ty_alv.

  lt_alv = gt_alv.
  delete lt_alv where box = ''.
  sort lt_alv by zqsid.

  delete adjacent duplicates from lt_alv comparing zqsid.

  describe table lt_alv lines g_line.
  loop at lt_alv into gs_alv where box = 'X'.
    g_setting = 'X'.
    wa_controlpara-no_open = 'X'. "出现打印请求
    wa_controlpara-no_close = 'X'. "不添加打印请求

    at first.
      perform sub_print_begin.
    endat.

    at last.
      perform sub_print_end.
    endat.

    perform sub_print_one using gs_alv-zqsid.
    clear: gs_alv.
  endloop.

endform.                    " PRINT_DATA
*&---------------------------------------------------------------------*
*&      Form  EXCXEL_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form excxel_output .
  data:lt_alv type table of ty_alv.
  data:lv_lines type i.
  data:lv_type type c.

  data:ls_header type zmm_rp043_header,
       ls_detail type zmm_rp043_detail,
       lt_detail type table of zmm_rp043_detail.

  lt_alv = gt_alv.
  delete lt_alv where box = ''.
  sort lt_alv by zqsid.

  delete adjacent duplicates from lt_alv comparing zqsid.
  describe table lt_alv lines lv_lines.

  if lv_lines ne 1.
    message '请选中一条数据导出' type 'S' display like 'E'.
    exit.
  endif.

  read table lt_alv into gs_alv index 1.
  case 'X'.
    when p_31.
      lv_type = '1'.
    when p_32.
      lv_type = '2'.
    when others.
  endcase.


  perform excxel_output_one using gs_alv-zqsid lv_type .

endform.                    " EXCXEL_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SUB_PRINT_BEGIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form sub_print_begin .

  wa_controlpara-no_open = space. "出现打印请求
  wa_controlpara-no_close = 'X'. "不添加打印请求

endform.                    " SUB_PRINT_BEGIN

*&---------------------------------------------------------------------*
*&      Form  SUB_PRINT_END
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form sub_print_end .

  if g_line > 1.
    wa_controlpara-no_open = 'X'. "不出现打印请求
    wa_controlpara-no_close = space. "添加打印请求
  else.
    wa_controlpara-no_open = space. "出现打印请求
    wa_controlpara-no_close = space. "添加打印请求
  endif.

endform.                    " SUB_PRINT_END
*&---------------------------------------------------------------------*
*&      Form  SUB_PRINT_ONE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form sub_print_one  using    pr_zqsid   like ztsd001-zqsid.
  data: lt_detail like line of t_prt_detail occurs 0 with header line,
        lw_header like line of t_prt_header.
  data: fm_name type rs38l_fnam.


  data: l_fn(50).
  clear l_fn.

  if p_31 = 'X' and p_33 ne 'X'.
    call function 'SSF_FUNCTION_MODULE_NAME'
      exporting
        formname           = 'ZMM_RP043_1'
      importing
        fm_name            = fm_name
      exceptions
        no_form            = 1
        no_function_module = 2
        others             = 3.
  elseif p_32 = 'X' and p_33 ne 'X'.
    call function 'SSF_FUNCTION_MODULE_NAME'
      exporting
        formname           = 'ZMM_RP043_2'
      importing
        fm_name            = fm_name
      exceptions
        no_form            = 1
        no_function_module = 2
        others             = 3.
  elseif p_31 = 'X' and p_33 = 'X'.
    call function 'SSF_FUNCTION_MODULE_NAME'
      exporting
        formname           = 'ZMM_RP043_3'
      importing
        fm_name            = fm_name
      exceptions
        no_form            = 1
        no_function_module = 2
        others             = 3.
  elseif p_32 = 'X' and p_33 = 'X'.
    call function 'SSF_FUNCTION_MODULE_NAME'
      exporting
        formname           = 'ZMM_RP043_4'
      importing
        fm_name            = fm_name
      exceptions
        no_form            = 1
        no_function_module = 2
        others             = 3.
  endif.



  check sy-subrc = 0.

  loop at t_prt_detail into lt_detail where zqsid = pr_zqsid.
*    IF lt_detail-vbtyp_v = 'J'.
    lt_detail-zmove_type = '出库(服务）确认单'.
*    ELSEIF lt_detail-vbtyp_v = 'H' OR lt_detail-vbtyp_v = 'T'.
*      lt_detail-zmove_type = ' 出库退回单'.
*    ENDIF.
    append lt_detail.
  endloop.

  read table t_prt_header into lw_header with key zqsid = pr_zqsid.



*      tables:sclass, clobjdat.


  "aDD  单价（含税）结束

  select single * from usr21 where bname = sy-uname.
  select single * from adcp  where persnumber = usr21-persnumber.

  if adcp-department = '销售'.
    select single * from zsd016  where bstnk = lw_header-bstnk.
    if sy-uname <> zsd016-zxiaos and sy-uname <> zsd016-zzongj and sy-uname <> zsd016-zkefu and sy-uname <> zsd016-zzhuli and
    sy-uname <> zsd016-zxiaos1 and sy-uname <> zsd016-zzhuli1 and sy-uname <> zsd016-zhuli.
      message '你没有打印这家医院出库单的权限！' type 'E'.
    endif.
  endif.

*- ADD BY HAND LM 2018/05/14 SD HISMS:15779 ENDED


  call function fm_name
    exporting
      wa_header          = lw_header
      g_lines            = 15
      control_parameters = wa_controlpara
    tables
      it_detail          = lt_detail
    exceptions
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      others             = 5.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    "sub_print_one