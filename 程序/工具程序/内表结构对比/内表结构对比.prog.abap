report  ztest_15405.

data: begin of gt_saledata occurs 0,
        charg     like lips-charg,"批次
        posnr_ps  like lips-posnr,"交货单行项目类别
        vbeln_ps  like lips-vbeln,"交货单
        lfimg     like lips-lfimg,"交货单数量
        vrkme_ps  like lips-vrkme,
        ernam     like lips-ernam,"交货单创建人
        vbeln_ap  like vbap-vbeln,"销售订单号
        pstyv     like vbap-pstyv,"销售订单行项目类别
        posnr_ap  like vbap-posnr,"交货单行项目号
        matnr_ap  like vbap-matnr,"物料
        kwmeng    like vbap-kwmeng,"订单数量
        vrkme_ap  like vbap-vrkme,
        arktx     like vbap-arktx,"订单描述
        kzwi1     like vbap-kzwi1,"订单总价值
        netwr     like vbap-netwr,"订单净值
        netpr     like vbap-netpr,"订单净值
        waerk     like vbap-waerk,"订单币别
        bwtar     like vbap-bwtar,
        vgbel     like vbap-vgbel,"销售订单前序凭证
          mvgr1      like tvm1t-mvgr1,

        bstnk     like vbak-bstnk,
        kunnr     like vbak-kunnr,"客户号
        auart_ak  like vbak-auart,"销售订单类型
        vbeln_ak  like vbak-vbeln,
        bukrs_vf  like vbak-bukrs_vf,"公司代码
        vgbel_ak  like vbak-vgbel,
        knumv     like vbak-knumv,
        lifex     like likp-lifex,
        wadat_ist like likp-wadat_ist,"发货日期
        vbeln_kp  like likp-vbeln,
        vsart     like likp-vsart,"签收状态
        wbsta     like vbup-wbsta,
        posnr_up  like vbup-posnr,
        vbeln_up  like vbup-vbeln,
        fksta     like vbup-fksta,
        vbelv_fa  like vbfa-vbelv,
        posnv_fa  like vbfa-posnv,
        vbtyp_v   like vbfa-vbtyp_v,
        posnn_fa  like vbfa-posnn,
        vbeln_fa  like vbfa-vbeln,
        stufe     like vbfa-stufe,"前序凭证
        auart_con like zvbak_con-auart,
        vbeln_con like zvbak_con-vbeln,
        xchpf     like marc-xchpf,"批次管理
        matnr_rc  like marc-matnr,"物料号
        werks     like marc-werks,
        meins     like mara-meins,"单位
        groes     like mara-groes,"型号
        volum     like mara-volum,
        matnr_ra  like mara-matnr,"物料号
        normt     like mara-normt,"品牌
        matkl     like mara-matkl,"制造商
        keyin_date like zsd012-keyin_date,"key in
        kzwil1     like vbap-kzwi1,
        kzwil2     like vbap-kzwi1,
        name1      like kna1-name1,"客户描述
        rfwrt      like vbfa-rfwrt,"发货成本
        rfwrt1      like vbfa-rfwrt,"发货成本*1.17
        wgbez      like v023-wgbez,
        wgbez1      like v023-wgbez,
        netwr4    like vbap-netwr,"销售成绩
        netwr6    like vbap-netwr,"未签收
        netwr5    like vbap-netwr,"收入
        gp(7)       type c,"GP达标率
        bezei      like t173t-bezei,
        ebeln      like ekkn-ebeln,
        mblnr      like mseg-mblnr,
        actss      like lfa1-actss,
*add byandy***********

      lifnr       like lfa1-lifnr,
      name2       like lfa1-name1,
      wlfimg      like lips-lfimg,
      wgru1       like vbap-wgru1,

      knumh     like a363-knumh,
      kbetr     like konp-kbetr,
      kpein     like konp-kpein,
      konwa   like konp-konwa,
     netwr1    like vbap-netwr,"单位成本
     bezei1      like tvm1t-bezei,

      zyyjc  like zsd034-zyyjc,
      zkefu  like  zsd034-zkefu,
      zkhzr  like zsd034-zkhzr,
      fhyf(6) type c,
      fhnf(4) type c,
      sdabw   like vbkd-sdabw,
      erdat   like vbak-erdat,
      tg(6)   type c,
      zywlx        like zsd034-zywlx,"业务类型

      zxsxmdz like zsd034-zxsxmdz,
      zzhuli  like zsd034-zzhuli,
      zxiaos1 like zsd034-zxiaos1,
      zzhuli1 like zsd034-zzhuli1,
      gpd(6)  type c,

     kzwil8     like vbap-kzwi1,
     kzwil9     like vbap-kzwi1,
*************************************
      end of gt_saledata.

type-pools:slis.

data:gt_fcat type lvc_t_fcat.
data:gs_fcat type lvc_s_fcat.
data:gt_fcat1 type slis_t_fieldcat_alv.
data:gs_fcat1 type slis_fieldcat_alv.

call function 'REUSE_ALV_FIELDCATALOG_MERGE'
 exporting
      i_program_name               = sy-repid
   i_internal_tabname           = 'GT_SALEDATA'
*      I_STRUCTURE_NAME             =
*      I_CLIENT_NEVER_DISPLAY       = 'X'
      i_inclname                   = sy-repid
*      I_BYPASSING_BUFFER           =
*      I_BUFFER_ACTIVE              =
  changing
    ct_fieldcat                  = gt_fcat1
*    EXCEPTIONS
*      INCONSISTENT_INTERFACE       = 1
*      PROGRAM_ERROR                = 2
*      OTHERS                       = 3
          .
if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
endif.


call function 'LVC_FIELDCATALOG_MERGE'
 exporting
*      I_BUFFER_ACTIVE              =
   i_structure_name             = 'ZSD_XHMX4_SALEDATA'
*      I_CLIENT_NEVER_DISPLAY       = 'X'
*      I_BYPASSING_BUFFER           =
*      I_INTERNAL_TABNAME           =
  changing
    ct_fieldcat                  = gt_fcat
 exceptions
   inconsistent_interface       = 1
   program_error                = 2
   others                       = 3
          .

loop at gt_fcat into gs_fcat.
  read table gt_fcat1 into gs_fcat1 with key
  fieldname = gs_fcat-fieldname.
  if sy-subrc = 0.
    if gs_fcat-inttype ne gs_fcat1-inttype or
       gs_fcat-intlen ne gs_fcat1-intlen.
      write: / gs_fcat-fieldname.
    endif.
  endif.
endloop.