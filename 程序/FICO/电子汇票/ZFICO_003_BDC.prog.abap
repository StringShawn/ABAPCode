FUNCTION zfico_003_bdc.
*"----------------------------------------------------------------------
*"*"局部接口：
*"  IMPORTING
*"     REFERENCE(IM_CTU) TYPE  APQI-PUTACTIVE DEFAULT 'X'
*"     REFERENCE(IM_MODE) TYPE  APQI-PUTACTIVE DEFAULT 'N'
*"     REFERENCE(IM_UPDATE) TYPE  APQI-PUTACTIVE DEFAULT 'L'
*"     REFERENCE(IM_HEAD) LIKE  ZFI_F_02_HEAD STRUCTURE  ZFI_F_02_HEAD
*"     REFERENCE(IM_NODATA) TYPE  APQI-PUTACTIVE DEFAULT '/'
*"     REFERENCE(IM_GROUP) TYPE  APQI-GROUPID OPTIONAL
*"     REFERENCE(IM_USER) TYPE  APQI-USERID OPTIONAL
*"     REFERENCE(IM_KEEP) TYPE  APQI-QERASE OPTIONAL
*"     REFERENCE(IM_HOLDDATE) TYPE  APQI-STARTDATE OPTIONAL
*"  EXPORTING
*"     REFERENCE(EX_SUBRC) TYPE  SY-SUBRC
*"  TABLES
*"      TS_MESSTAB STRUCTURE  BDCMSGCOLL
*"      TS_T_ITEM STRUCTURE  ZFICO_003_BDCS
*"----------------------------------------------------------------------
  DATA l_date TYPE c LENGTH 10.
  DATA l_num  TYPE i."table length
  DATA i_scrn TYPE sy-dynnr.

  CLEAR l_num.
  DESCRIBE TABLE ts_t_item LINES l_num.
* BDC Header
  ex_subrc = 0.

  PERFORM bdc_nodata      USING im_nodata.

  PERFORM open_group      USING im_group im_user
                                im_keep im_holddate im_ctu.

  PERFORM bdc_dynpro      USING 'SAPMF05A' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RF05A-NEWBW'.
  PERFORM bdc_field       USING 'BKPF-BLDAT'
                                im_head-bldat.
  PERFORM bdc_field       USING 'BKPF-BLART'
                                im_head-blart.
  PERFORM bdc_field       USING 'BKPF-BUKRS'
                                im_head-bukrs.
  PERFORM bdc_field       USING 'BKPF-BUDAT'
                                im_head-budat.
  PERFORM bdc_field       USING 'BKPF-WAERS'
                                im_head-waers.

***20141114hp_sjf
  PERFORM bdc_field       USING 'BKPF-MONAT'
                                im_head-monat.
***20141114hp_sjf

  DATA:
    i_tabix LIKE sy-tabix,
    i_koart TYPE koart,
    i_umskz TYPE umskz,
    wa_item LIKE LINE OF ts_t_item.

  "BDC Item
  LOOP AT ts_t_item INTO wa_item.
    i_tabix = sy-tabix.
    IF i_tabix = 1.
      "最初始屏幕
      PERFORM bdc_field       USING 'RF05A-NEWBS'
                                    wa_item-newbs.
      PERFORM bdc_field       USING 'RF05A-NEWKO'
                                    wa_item-newko.
      PERFORM bdc_field       USING 'RF05A-NEWUM'
                                    wa_item-newum.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
    ELSE.
      "输入屏幕
*      PERFORM bdc_dynpro      USING 'SAPMF05A' i_scrn.
      PERFORM bdc_field       USING 'RF05A-NEWBS'
                                    wa_item-newbs.
      PERFORM bdc_field       USING 'RF05A-NEWKO'
                                    wa_item-newko.
      PERFORM bdc_field       USING 'RF05A-NEWUM'
                                    wa_item-newum.

    ENDIF.
    IF wa_item-newbs = '39'.
      i_scrn = '2320'.
      PERFORM bdc_dynpro      USING 'SAPMF05A' '2320'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
      "签发日期
      "数据
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'BSEG-WRBTR'.
      PERFORM bdc_field       USING 'BSEG-WRBTR'
                                    wa_item-wrbtr.
      PERFORM bdc_field       USING 'BSEG-ZUONR'
                                    wa_item-zuonr.
      PERFORM bdc_field       USING 'BSEG-GSBER'
                              wa_item-gsber.
      PERFORM bdc_field       USING 'BSEG-SGTXT'
                                    wa_item-sgtxt.
*      PERFORM bdc_field       USING 'BDC_OKCODE'
*                                    '/00'.
      PERFORM bdc_field       USING 'BSED-WDATE'
                                    wa_item-wdate.
      PERFORM bdc_field       USING 'BSED-WNAME'
                                    wa_item-wname.
      PERFORM bdc_field       USING 'BSED-WBZOG'
                                    wa_item-wbzog.
      PERFORM bdc_field       USING 'BSEG-ZFBDT'
                                  wa_item-zfbdt.
    ELSEIF wa_item-newbs = '21'.
      i_scrn = '0302'.
      PERFORM bdc_dynpro      USING 'SAPMF05A' '0302'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                        '/00'.
      "签发日期
      "数据
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'BSEG-WRBTR'.
      PERFORM bdc_field       USING 'BSEG-WRBTR'
                                    wa_item-wrbtr.
      PERFORM bdc_field       USING 'BSEG-SGTXT'
                                    wa_item-sgtxt.
      PERFORM bdc_field       USING 'BSEG-GSBER'
                                    wa_item-gsber.
      PERFORM bdc_field       USING 'BSEG-SKFBT'
                                    wa_item-skfbt.
      PERFORM bdc_field       USING 'BSEG-ZFBDT'
                                  wa_item-zfbdt.
      PERFORM bdc_dynpro      USING 'SAPMF05A' '0332'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                        '/00'.
      PERFORM bdc_field       USING 'BSEG-RSTGR'
                               wa_item-rstgr.
*      PERFORM bdc_field       USING 'BSEG-XNEGP'
*                         'X'.
      PERFORM bdc_dynpro      USING 'SAPMF05A' '0332'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=RW'.
    ENDIF.
  ENDLOOP.
  "最终save

  PERFORM bdc_dynpro      USING 'SAPMF05A' '0700'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.

* Call BDC
  "传输

  PERFORM bdc_transaction TABLES ts_messtab
      USING                         'F-02'
                                    im_ctu
                                    im_mode
                                    im_update.
  IF sy-subrc <> 0.
    ex_subrc = sy-subrc.
    EXIT.
  ENDIF.

  PERFORM close_group USING     im_ctu.


ENDFUNCTION.
INCLUDE bdcrecxy .