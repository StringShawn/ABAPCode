FUNCTION zfico_003_bdc2.
*"----------------------------------------------------------------------
*"*"局部接口：
*"  IMPORTING
*"     REFERENCE(IM_CTU) TYPE  APQI-PUTACTIVE DEFAULT 'X'
*"     REFERENCE(IM_MODE) TYPE  APQI-PUTACTIVE DEFAULT 'N'
*"     REFERENCE(IM_UPDATE) TYPE  APQI-PUTACTIVE DEFAULT 'L'
*"     REFERENCE(IM_HEAD) LIKE  ZSFICO_003_BDC2H STRUCTURE
*"        ZSFICO_003_BDC2H
*"     REFERENCE(IM_NODATA) TYPE  APQI-PUTACTIVE DEFAULT '/'
*"     REFERENCE(IM_GROUP) TYPE  APQI-GROUPID OPTIONAL
*"     REFERENCE(IM_USER) TYPE  APQI-USERID OPTIONAL
*"     REFERENCE(IM_KEEP) TYPE  APQI-QERASE OPTIONAL
*"     REFERENCE(IM_HOLDDATE) TYPE  APQI-STARTDATE OPTIONAL
*"  EXPORTING
*"     REFERENCE(EX_SUBRC) TYPE  SY-SUBRC
*"  TABLES
*"      TS_MESSTAB STRUCTURE  BDCMSGCOLL
*"      TS_ITEM STRUCTURE  ZSFICO_003_BDC2I
*"----------------------------------------------------------------------

  DATA l_date TYPE c LENGTH 10.
  DATA l_num  TYPE i."table length
  DATA i_scrn TYPE sy-dynnr.

* BDC Header
  ex_subrc = 0.

  PERFORM bdc_nodata      USING im_nodata.

  PERFORM open_group      USING im_group im_user
                                im_keep im_holddate im_ctu.
  PERFORM bdc_dynpro      USING 'SAPMF05A' '0122'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SL'.
  PERFORM bdc_field       USING 'BDC_CURSOR'  'RF05A-NEWKO'.
  PERFORM bdc_field       USING 'RF05A-XPOS1(4)'  'X'.
  PERFORM bdc_field       USING 'BKPF-BUKRS'  im_head-bukrs.
  PERFORM bdc_field       USING 'BKPF-BLART'  im_head-blart.
  PERFORM bdc_field       USING 'BKPF-BLDAT'  im_head-bldat.
  PERFORM bdc_field       USING 'BKPF-BUDAT'  im_head-budat.
  PERFORM bdc_field       USING 'BKPF-WAERS'  im_head-waers.


  "处理未清项
  PERFORM bdc_dynpro      USING 'SAPMF05A' '0710'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=PA'.

  PERFORM bdc_field       USING 'RF05A-AGKOA'	'K'.
  PERFORM bdc_field       USING 'RF05A-AGKON'	im_head-lifnr.
  PERFORM bdc_field       USING 'RF05A-AGBUK'	im_head-bukrs.
  PERFORM bdc_field       USING 'RF05A-XNOPS'	'X'.
  PERFORM bdc_field       USING 'BDC_CURSOR'  'RF05A-AGKON'.

  "用凭证号进行筛选
  DATA:lv_tabix TYPE n LENGTH 2.
  DATA:lv_yushu TYPE i.
  DATA:lv_field TYPE fnam_____4.
  CLEAR lv_tabix.

  "全选
  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=OMX'.
  "取消激活
  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=Z-'.
  "金额降序排列
  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=OSD'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'DF05B-PSBET(01)'.
  "会计凭证行项目升序
  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=OSO'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RFOPS_DK-BUZEI(01)'.
  "会计凭证编号升序
  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=OSO'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RFOPS_DK-BELNR(01)'.
  "会计凭证年度升序
  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=OSO'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RFOPS_DK-GJAHR(01)'.

  "激活  先按借方的顺序激活
  DATA:lv_pos TYPE string.
  DATA:lv_yields TYPE i.
  CLEAR:lv_pos, lv_tabix.

  LOOP AT ts_item.
    PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'RF05A-ABPOS'
                                  ts_item-index.
    PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=Z+'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'DF05B-PSBET(01)' .
  ENDLOOP.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=REST'.
  "填写差额
  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  READ TABLE ts_item WITH KEY shengyu = 'X'.
  PERFORM bdc_field       USING 'RF05A-ABPOS'

                              ts_item-index.
  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=AB'.
  PERFORM bdc_field       USING 'DF05B-PSDIF(01)'
                                im_head-dmbtr.
  PERFORM bdc_dynpro      USING 'SAPMF05A' '0700'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RF05A-NEWBS'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BS'.
  PERFORM bdc_dynpro      USING 'SAPMF05A' '0700'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RF05A-AZEI1(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=PI'.
  PERFORM bdc_dynpro      USING 'SAPMF05A' '0302'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'BSEG-SGTXT'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ZK'.
  IF im_head-sgtxt IS NOT INITIAL.
    PERFORM bdc_field       USING 'BSEG-SGTXT'
                                  im_head-sgtxt.
  ENDIF.

  IF im_head-zuonr IS NOT INITIAL.
    PERFORM bdc_field       USING 'BSEG-ZUONR'
                                  im_head-zuonr.
  ENDIF.


  PERFORM bdc_dynpro      USING 'SAPMF05A' '0332'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'BSEG-RSTGR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=AB'.
  IF im_head-rstgr IS NOT INITIAL.
    PERFORM bdc_field       USING 'BSEG-RSTGR'
                                  im_head-rstgr.
  ENDIF.

  PERFORM bdc_dynpro      USING 'SAPMF05A' '0700'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RF05A-NEWBS'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.


* Call BDC
  "传输

  PERFORM bdc_transaction TABLES ts_messtab
      USING                         'FB05'
                                    im_ctu
                                    im_mode
                                    im_update.
  IF sy-subrc <> 0.
    ex_subrc = sy-subrc.
    EXIT.
  ENDIF.

  PERFORM close_group USING     im_ctu.


ENDFUNCTION.