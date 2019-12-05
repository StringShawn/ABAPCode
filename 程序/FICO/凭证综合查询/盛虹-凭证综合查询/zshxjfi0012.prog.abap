**************************************************
*程序名称:凭证综合查询
*创建日期: 2019-11-20
*创建者:XXX
*申请者:XXX
*功能描述:
*============================================
*变更记录
*修改请求号    修改日期    修改人   修改描述
* DEVK912025    2019-11-20   HANDYXH    创建程序
***************************************************
REPORT zshxjfi0012.
TABLES:bkpf,bseg,prps.
RANGES:s_user FOR bkpf-usnam.
TYPES:BEGIN OF ty_data.
        INCLUDE STRUCTURE zsfirpt_012_alv.
TYPES:
*      paobjnr TYPE rkeobjnr,
*  docln TYPE acdoca-docln,
  vbeln TYPE vbeln,
  END OF ty_data.


TYPES:BEGIN OF ty_acct,
        aktbo   TYPE aktbo,
        paobjnr TYPE rkeobjnr,
        pasubnr TYPE rkesubnr,
        kaufn   TYPE kdauf,
        kdpos   TYPE kdpos,
      END OF ty_acct.

DATA:t_acct  TYPE STANDARD TABLE OF ty_acct,
     wa_acct TYPE                  ty_acct.

DATA:gt_data TYPE TABLE OF ty_data,
     gw_data TYPE ty_data.
FIELD-SYMBOLS <fs_data> TYPE ty_data.
DATA:gt_data_ce1 TYPE TABLE OF ty_data.

TYPES:BEGIN OF ty_data_tmp,
        bukrs TYPE bseg-bukrs,
        belnr TYPE bseg-belnr,
        buzei TYPE bseg-buzei,
        gjahr TYPE bseg-gjahr,
      END OF ty_data_tmp.

TYPES:BEGIN OF ty_kunnr,
        kunnr TYPE bseg-kunnr,
      END OF ty_kunnr.

TYPES:BEGIN OF ty_lifnr,
        lifnr TYPE bseg-lifnr,
      END OF ty_lifnr.

TYPES:BEGIN OF ty_auart,
        auart TYPE aufk-auart,
      END OF ty_auart.

TYPES:BEGIN OF ty_vptnr,
        vptnr TYPE bseg-vptnr,
      END OF ty_vptnr.

TYPES:BEGIN OF ty_bsec,
        bukrs TYPE bseg-bukrs,
        belnr TYPE bseg-belnr,
        buzei TYPE bseg-buzei,
        gjahr TYPE bseg-gjahr,
        name1 TYPE bsec-name1,
      END OF ty_bsec.

TYPES:BEGIN OF ty_kna1,
        kunnr TYPE bseg-kunnr,
        name1 TYPE kna1-name1,
        name2 TYPE kna1-name2,
      END OF ty_kna1.

TYPES:BEGIN OF ty_lfa1,
        lifnr TYPE bseg-lifnr,
        name1 TYPE lfa1-name1,
        name2 TYPE lfa1-name2,
      END OF ty_lfa1.

TYPES:BEGIN OF ty_t003p,
        auart TYPE aufk-auart,
        txt   TYPE t003p-txt,
      END OF ty_t003p.

TYPES: BEGIN OF ty_t007s,
         mwskz TYPE t007s-mwskz,
         text1 TYPE t007s-text1,
       END OF ty_t007s.

DATA:gt_bsec  TYPE TABLE OF ty_bsec,
     gw_bsec  TYPE ty_bsec,
     gt_kna1  TYPE TABLE OF ty_kna1,
     ht_kna1  TYPE TABLE OF ty_kna1,
     gw_kna1  TYPE ty_kna1,
     gt_lfa1  TYPE TABLE OF ty_lfa1,
     gw_lfa1  TYPE ty_lfa1,
     gt_t003p TYPE TABLE OF ty_t003p,
     gw_t003p TYPE ty_t003p,
     gt_t007s TYPE TABLE OF ty_t007s.


TYPES:BEGIN OF ty_data_yz.
        INCLUDE STRUCTURE zsfirpt_012_alv.
TYPES:ausbk TYPE vbsegs-ausbk,
      kaufn TYPE kdauf,
      kdpos TYPE kdpos,
      END OF ty_data_yz.


TYPES:BEGIN OF ty_vbsegd,
        ausbk TYPE vbsegd-ausbk,
        belnr TYPE vbsegd-belnr,
        gjahr TYPE vbsegd-gjahr,
        bzkey TYPE vbsegd-bzkey,
        umskz TYPE vbsegd-umskz,
        kunnr TYPE vbsegd-kunnr,
        kkber TYPE vbsegd-kkber,
        hbkid TYPE vbsegd-hbkid,
        hktid TYPE vbsegd-hktid,
        zbd1t TYPE vbsegd-zbd1t,
        zbd2t TYPE vbsegd-zbd2t,
        zbd3t TYPE vbsegd-zbd3t,
        zterm TYPE vbsegd-zterm,
      END OF ty_vbsegd.

TYPES:BEGIN OF ty_vbsegk,
        ausbk TYPE vbsegk-ausbk,
        belnr TYPE vbsegk-belnr,
        gjahr TYPE vbsegk-gjahr,
        bzkey TYPE vbsegk-bzkey,
        lifnr TYPE vbsegk-lifnr,
        zbd1t TYPE vbsegk-zbd1t,
        zbd2t TYPE vbsegk-zbd2t,
        zbd3t TYPE vbsegk-zbd3t,
        zterm TYPE vbsegk-zterm,
      END OF ty_vbsegk.

TYPES:BEGIN OF ty_kkber,
        kkber TYPE vbsegd-kkber,
      END OF ty_kkber.

TYPES:BEGIN OF ty_hbkid,
        " 账户标识描述
        ausbk TYPE vbsegd-ausbk,
        hbkid TYPE vbsegd-hbkid,
        hktid TYPE vbsegd-hktid,
      END OF ty_hbkid.


*ALV
DATA:gw_layout TYPE lvc_s_layo,
     gv_repid  TYPE repid.
"设置字段目录的宏
DEFINE mcr_fieldcat.

  CLEAR gw_fieldcat.
  gw_fieldcat-fieldname   = &1.
  gw_fieldcat-reptext   = &2.

  APPEND gw_fieldcat TO gt_fieldcat.

END-OF-DEFINITION.

"设置字段目录的宏
DEFINE mcr_fieldcat_mx.

  CLEAR gw_fieldcat_mx.
  gw_fieldcat_mx-fieldname   = &1.
  gw_fieldcat_mx-reptext   = &2.

  APPEND gw_fieldcat_mx TO gt_fieldcat_mx.

END-OF-DEFINITION.

"基本选择条件
SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:s_bukrs FOR bkpf-bukrs OBLIGATORY,
               s_belnr FOR bkpf-belnr,
               s_xblnr FOR bkpf-xblnr_alt,
               s_gjahr FOR bkpf-gjahr DEFAULT sy-datum(4) OBLIGATORY,
               s_monat FOR bkpf-monat.
SELECTION-SCREEN END OF BLOCK bk1.

"凭证抬头选择条件
SELECTION-SCREEN BEGIN OF BLOCK bk2 WITH FRAME TITLE text-002.
SELECT-OPTIONS:s_blart FOR bkpf-blart,
               s_budat FOR bkpf-budat,"DEFAULT sy-datum,"OBLIGATORY
               s_cpudt FOR bkpf-cpudt,
               s_ppnam FOR bkpf-ppnam,
               s_waers FOR bkpf-waers,
               s_usnam FOR bkpf-usnam,
               s_bktxt FOR bkpf-bktxt,
               s_xref1 FOR bkpf-xref1_hd,
               s_xref2 FOR bkpf-xref2_hd.
SELECTION-SCREEN END OF BLOCK bk2.

"凭证行项目选择条件
SELECTION-SCREEN BEGIN OF BLOCK bk3 WITH FRAME TITLE text-003.
SELECT-OPTIONS:s_hkont FOR bseg-hkont,
               s_koart FOR bseg-koart,
               s_bschl FOR bseg-bschl,
               s_umskz FOR bseg-umskz,
               s_xnegp FOR bseg-xnegp,
               s_prctr FOR bseg-prctr,
               s_sgtxt FOR bseg-sgtxt,
               s_kostl FOR bseg-kostl MATCHCODE OBJECT kost,
               s_aufnr FOR bseg-aufnr,
               s_kkber FOR bseg-kkber,
               s_kunnr FOR bseg-kunnr,
               s_lifnr FOR bseg-lifnr,
               s_vbund FOR bseg-vbund,
               s_fkber FOR bseg-fkber_long,
               s_matnr FOR bseg-matnr,
               s_rstgr FOR bseg-rstgr,
               s_anln1 FOR bseg-anln1,
               s_wrbtr FOR bseg-wrbtr,
               s_dmbtr FOR bseg-dmbtr,
               s_projk FOR prps-pspnr,
*               s_item1 FOR bseg-zz_item1,
*               s_item2 FOR bseg-zz_item2,
*               s_item3 FOR bseg-zz_item3,
*               s_item4 FOR bseg-zz_item4,
*               s_item5 FOR bseg-zz_item5,
*               s_item6 FOR bseg-zz_item6,
               s_hktid FOR bseg-hktid,
*               s_extvbe FOR bseg-zz_extvbeln,
*               s_matkl FOR bseg-zz_matkl,
*               s_item7 FOR bseg-zz_item7,
*               s_item8 FOR bseg-zz_item8,
               s_zuonr FOR bseg-zuonr.
SELECTION-SCREEN END OF BLOCK bk3.

"特殊处理选择条件
SELECTION-SCREEN BEGIN OF BLOCK bk4 WITH FRAME TITLE text-004.
PARAMETERS:p_bhyz AS CHECKBOX .  "包含预制凭证
SELECTION-SCREEN END OF BLOCK bk4.

SELECTION-SCREEN BEGIN OF BLOCK bk5 WITH FRAME TITLE text-005.
PARAMETERS:p_own AS CHECKBOX .  "
SELECTION-SCREEN END OF BLOCK bk5.

AT SELECTION-SCREEN OUTPUT.
  IF  s_bukrs[] IS INITIAL.
    s_bukrs-sign = 'I'.
    s_bukrs-option = 'EQ'.
    GET PARAMETER ID 'BUK' FIELD s_bukrs-low .
    APPEND s_bukrs.
  ENDIF.
  IF s_monat[] IS INITIAL.
    s_monat-sign = 'I'.
    s_monat-option = 'EQ'.
    s_monat-low = sy-datum+4(2) .
    APPEND s_monat.
  ENDIF.

START-OF-SELECTION.
  "检查权限
  PERFORM frm_check_auth.
  "查询数据
  PERFORM frm_get_data.
  "处理数据
  PERFORM frm_process_data.
  "显示数据
  PERFORM frm_display_data.

*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_AUTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_check_auth .
  DATA:lv_msg TYPE char255.
  TYPES:BEGIN OF ty_bukrs ,
          bukrs TYPE t001-bukrs,
        END OF ty_bukrs.

  DATA:lt_bukrs TYPE TABLE OF ty_bukrs,
       lw_bukrs TYPE ty_bukrs.

  REFRESH:lt_bukrs.
  "查询所有的公司代码
  SELECT bukrs
    INTO TABLE lt_bukrs
    FROM t001
    WHERE bukrs IN s_bukrs.
  "权限检查
  LOOP AT lt_bukrs INTO lw_bukrs.
    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
      ID 'BUKRS' FIELD lw_bukrs-bukrs
      ID 'ACTVT' FIELD '03'.
    "没有相关公司代码的权限，报错
    IF sy-subrc <> 0.
      CLEAR:lv_msg.
      CONCATENATE '您没有公司代码' lw_bukrs-bukrs '的权限' INTO lv_msg.
      MESSAGE  lv_msg TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING .
    ENDIF.
    CLEAR:lw_bukrs.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_data .

  DATA:lt_data_tmp  TYPE TABLE OF ty_data_tmp,
       lw_data_tmp  TYPE ty_data_tmp,
       lt_kunnr     TYPE TABLE OF ty_kunnr,
       lw_kunnr     TYPE ty_kunnr,
       lt_lifnr     TYPE TABLE OF ty_lifnr,
       lw_lifnr     TYPE ty_lifnr,
       lt_auart     TYPE TABLE OF ty_auart,
       lw_auart     TYPE ty_auart,
       lt_data      TYPE TABLE OF zsfirpt_012_alv,
       lw_data      TYPE zsfirpt_012_alv,
       lt_data_yz   TYPE TABLE OF ty_data_yz,
       lw_data_yz   TYPE ty_data_yz,
       lt_data_yz_s TYPE TABLE OF ty_data_yz,
       lw_data_yz_s TYPE ty_data_yz,
       lt_data_yz_d TYPE TABLE OF ty_data_yz,
       lw_data_yz_d TYPE ty_data_yz,
       lt_data_yz_k TYPE TABLE OF ty_data_yz,
       lw_data_yz_k TYPE ty_data_yz,
       lt_data_yz_a TYPE TABLE OF ty_data_yz,
       lw_data_yz_a TYPE ty_data_yz,
       lt_vbsegd    TYPE TABLE OF ty_vbsegd,
       lw_vbsegd    TYPE ty_vbsegd,
       lt_vbsegk    TYPE TABLE OF ty_vbsegk,
       lw_vbsegk    TYPE ty_vbsegk,
       lt_kkber     TYPE TABLE OF ty_kkber,
       lw_kkber     TYPE ty_kkber,
       lt_hbkid     TYPE TABLE OF ty_hbkid,
       lw_hbkid     TYPE ty_hbkid,
       lt_t014t     TYPE TABLE OF t014t,
       lw_t014t     TYPE t014t,
       lt_t012t     TYPE TABLE OF t012t,
       lw_t012t     TYPE t012t,
       lw_vptnr     TYPE ty_vptnr,
       lt_vptnr     TYPE TABLE OF ty_vptnr.

  DATA:lv_tabix TYPE i.

  REFRESH:gt_data,lt_data,gt_bsec,gt_kna1,gt_lfa1,gt_t003p,lt_data_tmp,lt_kunnr,lt_lifnr,lt_auart,lt_vptnr.
  IF p_own = 'X'.
    CLEAR:s_user,s_user[].
    s_user-sign = 'I'.
    s_user-option = 'EQ'.
    s_user-low = sy-uname.
    APPEND s_user.
  ENDIF.
  DATA: lt_data1 TYPE TABLE OF ty_data.
  DATA: lw_data1 TYPE ty_data.
  "从数据库表BSEG、BKPF中取出主数据，左连接描述表取出描述字段
*  SELECT
*       DISTINCT
*          bkpf~bukrs
*          bkpf~belnr
*          bkpf~xblnr_alt
*          bkpf~gjahr
*          bkpf~monat
*          acdoca~docln
*          acdoca~buzei
*          acdoca~ps_psp_pnr
*          bkpf~awtyp
*          bkpf~awkey
*          bkpf~blart
*          bkpf~budat
*          bkpf~bldat
*          bkpf~cpudt
*          bkpf~cputm
*          bkpf~xblnr
*          bkpf~bstat
*          bkpf~xreversal
*          bkpf~stblg
*          bkpf~stjah
*          bkpf~ppnam
*          bkpf~usnam
*          bkpf~tcode
*          bkpf~xref1_hd
*          bkpf~xref2_hd
*          bkpf~bktxt
*          acdoca~bschl
*          acdoca~umskz
*          acdoca~drcrk AS shkzg
*          acdoca~koart
*          acdoca~racct AS hkont
*          skat~txt50 AS hkont_ms     "总账科目描述
*          acdoca~wsl AS wrbtr
*          bkpf~waers
*          acdoca~hsl AS dmbtr
*          bkpf~hwaer
*          acdoca~zuonr
*          acdoca~sgtxt
*          acdoca~prctr
*          cepct~ktext               "利润中心描述
*          acdoca~kunnr
*          acdoca~vptnr
*          acdoca~lifnr
*          acdoca~ebeln
*          acdoca~ebelp
*          acdoca~rassc AS vbund
*          t880~name1 AS vbund_ms
*          acdoca~mwskz
*         t007s~text1 AS mwskz_ms     "税码描述
*          acdoca~hbkid
*          acdoca~hktid
*          t012t~text1 AS hktid_ms  "账户标识描述
*          acdoca~kokrs
*          acdoca~rfarea AS fkber_long
*          tfkbt~fkbtx               "功能范围描述
*          acdoca~aufnr
*          aufk~auart                  "订单类型
*          acdoca~rcntr AS kostl
*          cskt~ktext AS kostl_ms     "成本中心描述
*          acdoca~matnr
*          makt~maktx                    "物料描述
*          acdoca~msl AS menge
*          acdoca~werks
*          acdoca~runit AS meins
*          acdoca~anln1
*          acdoca~anln2
*          anla~txt50 AS anln1_ms     "资产描述
*          acdoca~anbwa
*          tabwt~bwatxt AS anbwa_ms  "业务类型描述
*          acdoca~paobjnr
*    INTO CORRESPONDING FIELDS OF TABLE lt_data1
*    FROM acdoca INNER JOIN bkpf ON bkpf~bukrs = acdoca~rbukrs AND bkpf~gjahr = acdoca~gjahr AND bkpf~belnr = acdoca~belnr
**             LEFT JOIN bseg on bseg~bukrs = acdoca~rbukrs AND bseg~gjahr = ACDOCA~gjahr AND bseg~belnr = ACDOCA~belnr and bseg~buzei = acdoca~buzei
*             LEFT JOIN skat  ON acdoca~racct = skat~saknr AND skat~spras = sy-langu
*              LEFT JOIN cepct ON acdoca~prctr = cepct~prctr AND cepct~spras = sy-langu
**              LEFT JOIN t014t ON bseg~kkber = t014t~kkber AND t014t~spras = sy-langu
*              LEFT JOIN t880 ON acdoca~rassc = t880~rcomp
*              LEFT JOIN t007s ON acdoca~mwskz = t007s~mwskz AND t007s~spras = sy-langu AND t007s~kalsm = 'TAXCN'
**              LEFT JOIN t053s ON ACDOCA~rstgr = t053s~rstgr AND ACDOCA~rbukrs = t053s~bukrs AND t053s~spras = sy-langu
*              LEFT JOIN t012t ON acdoca~hbkid = t012t~hbkid AND acdoca~hktid = t012t~hktid AND acdoca~rbukrs = t012t~bukrs AND t012t~spras = sy-langu
*              LEFT JOIN tfkbt ON acdoca~rfarea = tfkbt~fkber AND tfkbt~spras = sy-langu
*              LEFT JOIN aufk ON acdoca~aufnr = aufk~aufnr
*              LEFT JOIN cskt ON acdoca~rcntr = cskt~kostl AND cskt~spras = sy-langu AND cskt~datbi = '99991231'
*              LEFT JOIN makt ON acdoca~matnr = makt~matnr AND makt~spras = sy-langu
*              LEFT JOIN anla ON acdoca~rbukrs = anla~bukrs AND acdoca~anln1 = anla~anln1 AND acdoca~anln2 = anla~anln2
*              LEFT JOIN tabwt ON acdoca~anbwa = tabwt~bwasl AND tabwt~spras = sy-langu
**              LEFT JOIN zcofcofien01_01t ON ACDOCA~zz_item1 = zcofcofien01_01t~zz_item1 AND zcofcofien01_01t~spras = sy-langu
*
*    WHERE bkpf~bukrs IN s_bukrs
*      AND bkpf~belnr IN s_belnr
*      AND bkpf~xblnr_alt IN s_xblnr
*      AND bkpf~gjahr IN s_gjahr
*      AND bkpf~monat IN s_monat
*      AND bkpf~blart IN s_blart
*      AND bkpf~budat IN s_budat
*      AND bkpf~cpudt IN s_cpudt
*      AND bkpf~ppnam IN s_ppnam
*      AND bkpf~waers IN s_waers
*      AND bkpf~usnam IN s_usnam
*      AND bkpf~usnam IN s_user
*      AND bkpf~bktxt IN s_bktxt
*      AND bkpf~xref1_hd IN s_xref1
*      AND bkpf~xref2_hd IN s_xref2
*      AND acdoca~racct IN s_hkont
*      AND acdoca~koart IN s_koart
*      AND acdoca~bschl IN s_bschl
*      AND acdoca~umskz IN s_umskz
*      AND acdoca~prctr IN s_prctr
*      AND acdoca~sgtxt IN s_sgtxt
*      AND acdoca~rcntr IN s_kostl
*      AND acdoca~aufnr IN s_aufnr
*      AND acdoca~kunnr IN s_kunnr
*      AND acdoca~lifnr IN s_lifnr
*      AND acdoca~rassc IN s_vbund
*      AND acdoca~rfarea IN s_fkber
*      AND acdoca~matnr IN s_matnr
*      AND acdoca~anln1 IN s_anln1
*      AND acdoca~wsl IN s_wrbtr
*      AND acdoca~hsl IN s_dmbtr
*      AND acdoca~hktid IN  s_hktid
*      AND acdoca~zuonr IN s_zuonr
*      AND acdoca~ps_psp_pnr IN s_projk.

  SELECT   bkpf~bukrs
           bkpf~belnr
           bkpf~xblnr_alt
           bkpf~gjahr
           bkpf~monat
           bkpf~awtyp
           bkpf~awkey
           bkpf~blart
           bkpf~budat
           bkpf~bldat
           bkpf~cpudt
           bkpf~cputm
           bkpf~xblnr
           bkpf~bstat
           bkpf~xreversal
           bkpf~stblg
           bkpf~stjah
           bkpf~ppnam
           bkpf~usnam
           bkpf~tcode
           bkpf~xref1_hd
           bkpf~xref2_hd
           bkpf~bktxt
           bkpf~waers
           bkpf~hwaer
  INTO CORRESPONDING FIELDS OF TABLE gt_data
  FROM bkpf
  WHERE bkpf~bukrs IN s_bukrs
       AND bkpf~belnr IN s_belnr
       AND bkpf~xblnr_alt IN s_xblnr
       AND bkpf~gjahr IN s_gjahr
       AND bkpf~monat IN s_monat
       AND bkpf~blart IN s_blart
       AND bkpf~budat IN s_budat
       AND bkpf~cpudt IN s_cpudt
       AND bkpf~ppnam IN s_ppnam
       AND bkpf~waers IN s_waers
       AND bkpf~usnam IN s_usnam
       AND bkpf~usnam IN s_user
       AND bkpf~bktxt IN s_bktxt
       AND bkpf~xref1_hd IN s_xref1
       AND bkpf~xref2_hd IN s_xref2
       AND bkpf~bstat = ''.
  IF gt_data IS NOT INITIAL.
    SELECT bseg~bukrs,
           bseg~gjahr,
           bseg~belnr,
           bseg~buzei AS docln ,
           bseg~buzei,
           bseg~bschl,
           bseg~umskz,
           bseg~shkzg,
           bseg~xnegp,
           bseg~projk , "by sw
           bseg~koart,
           bseg~hkont,
           bseg~wrbtr,
           bseg~dmbtr,
           bseg~zuonr,
           bseg~sgtxt,
           bseg~prctr,
           bseg~kunnr,
           bseg~vptnr,
           bseg~kkber,
           bseg~vbel2,
           bseg~posn2,
           bseg~lifnr,
           bseg~ebeln,
           bseg~ebelp,
           bseg~zfbdt,
           bseg~zbd1t,
           bseg~zbd2t,
           bseg~zbd3t,
           bseg~zlsch,
           bseg~zterm,
           bseg~vbund,
           bseg~mwskz,
           bseg~rstgr,
           bseg~hbkid,
           bseg~hktid,
           bseg~kokrs,
           bseg~fkber_long,
           bseg~aufnr,
           bseg~kostl,
           bseg~matnr,
           bseg~menge,
           bseg~werks,
           bseg~meins,
           bseg~gsber,
           bseg~valut,
           bseg~anln1,
           bseg~anln2,
           bseg~anbwa,
           bseg~paobjnr
    INTO TABLE @DATA(lt_bseg)
    FROM bseg FOR ALL ENTRIES IN @gt_data
    WHERE bukrs = @gt_data-bukrs
      AND gjahr = @gt_data-gjahr
      AND belnr = @gt_data-belnr
      AND bseg~hkont IN @s_hkont
       AND bseg~koart IN @s_koart
       AND bseg~bschl IN @s_bschl
       AND bseg~umskz IN @s_umskz
       AND bseg~xnegp IN @s_xnegp
       AND bseg~prctr IN @s_prctr
       AND bseg~sgtxt IN @s_sgtxt
       AND bseg~kostl IN @s_kostl
       AND bseg~aufnr IN @s_aufnr
       AND bseg~kkber IN @s_kkber
       AND bseg~kunnr IN @s_kunnr
       AND bseg~lifnr IN @s_lifnr
       AND bseg~vbund IN @s_vbund
       AND bseg~fkber_long IN @s_fkber
       AND bseg~matnr IN @s_matnr
       AND bseg~rstgr IN @s_rstgr
       AND bseg~anln1 IN @s_anln1
       AND bseg~wrbtr IN @s_wrbtr
       AND bseg~dmbtr IN @s_dmbtr
       AND bseg~hktid IN @s_hktid
       AND bseg~zuonr IN @s_zuonr
       AND bseg~projk IN @s_projk.  "by sw

    SORT lt_bseg BY bukrs gjahr belnr.

    LOOP AT gt_data INTO gw_data.
      READ TABLE lt_bseg INTO DATA(ls_bseg) WITH KEY bukrs = gw_data-bukrs
                                                     gjahr = gw_data-gjahr
                                                     belnr = gw_data-belnr BINARY SEARCH.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_bseg TO gw_data.
      ELSE.
        DELETE gt_data.
        CONTINUE.
      ENDIF.
      MODIFY gt_data FROM gw_data.
    ENDLOOP.

    IF gt_data IS NOT INITIAL.
      SELECT prctr,
             ktext
      INTO TABLE @DATA(lt_cepct)
      FROM cepct FOR ALL ENTRIES IN @gt_data
      WHERE prctr = @gt_data-prctr
        AND spras = @sy-langu.
      SORT lt_cepct BY prctr.

      SELECT kkber,
             kkbtx
      INTO TABLE @lt_t014t
      FROM t014t FOR ALL ENTRIES IN @gt_data
      WHERE kkber = @gt_data-kkber
        AND spras = @sy-langu.
      SORT lt_t014t BY kkber.

      SELECT rcomp,
             name1
      INTO TABLE @DATA(lt_t880)
      FROM t880 FOR ALL ENTRIES IN @gt_data
      WHERE rcomp = @gt_data-vbund.
      SORT lt_t880 BY rcomp.

      SELECT rstgr,
             bukrs,
             txt20
      INTO TABLE @DATA(lt_t053s)
      FROM t053s FOR ALL ENTRIES IN @gt_data
      WHERE rstgr = @gt_data-rstgr
        AND bukrs = @gt_data-bukrs
        AND spras = @sy-langu.
      SORT lt_t053s BY rstgr bukrs.

      SELECT hbkid,
             hktid,
             bukrs,
             text1
      INTO TABLE @lt_t012t
      FROM t012t FOR ALL ENTRIES IN @gt_data
      WHERE hbkid = @gt_data-hbkid
        AND hktid = @gt_data-hktid
        AND bukrs = @gt_data-bukrs
        AND spras = @sy-langu.
      SORT lt_t012t BY hbkid hktid bukrs.

      SELECT fkber,
             fkbtx
      INTO TABLE @DATA(lt_tfkbt)
      FROM tfkbt FOR ALL ENTRIES IN @gt_data
      WHERE fkber = @gt_data-fkber_long
        AND spras = @sy-langu.
      SORT lt_tfkbt BY fkber.

      SELECT aufnr,
             auart
      INTO TABLE @DATA(lt_aufk)
      FROM aufk FOR ALL ENTRIES IN @gt_data
      WHERE aufnr = @gt_data-aufnr.
      SORT lt_aufk BY aufnr.

      SELECT kostl,
             ktext
      INTO TABLE @DATA(lt_cskt)
      FROM cskt FOR ALL ENTRIES IN @gt_data
      WHERE kostl = @gt_data-kostl
        AND spras = @sy-langu
        AND datbi = '99991231'.
      SORT lt_cskt BY kostl.

      SELECT matnr,
             maktx
      INTO TABLE @DATA(lt_makt)
      FROM makt FOR ALL ENTRIES IN @gt_data
      WHERE matnr = @gt_data-matnr
        AND spras = @sy-langu.
      SORT lt_makt BY matnr.

      SELECT anln1,
             anln2,
             bukrs,
             txt50
      INTO TABLE @DATA(lt_anla)
      FROM anla FOR ALL ENTRIES IN @gt_data
      WHERE anln1 = @gt_data-anln1
        AND anln2 = @gt_data-anln2
        AND bukrs = @gt_data-bukrs.
      SORT lt_anla BY anln1 anln2 bukrs.

      SELECT bwasl,
             bwatxt
      INTO TABLE @DATA(lt_tabwt)
      FROM tabwt FOR ALL ENTRIES IN @gt_data
      WHERE bwasl = @gt_data-anbwa
        AND spras = @sy-langu.
    ENDIF.

    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
      READ TABLE lt_cepct INTO DATA(ls_cepct) WITH KEY prctr = <fs_data>-prctr BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_data>-ktext = ls_cepct-ktext.
      ENDIF.

      READ TABLE lt_t014t INTO DATA(ls_t014t) WITH KEY kkber = <fs_data>-kkber BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_data>-kkbtx = ls_t014t-kkbtx.
      ENDIF.

      READ TABLE lt_t880 INTO DATA(ls_t880) WITH KEY rcomp = <fs_data>-vbund BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_data>-vbund_ms = ls_t880-name1.
      ENDIF.

      READ TABLE lt_t053s INTO DATA(ls_t053s) WITH KEY rstgr = <fs_data>-rstgr bukrs = <fs_data>-bukrs BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_data>-rstgr_ms = ls_t053s-txt20.
      ENDIF.

      READ TABLE lt_t012t INTO DATA(ls_t012t) WITH KEY hbkid = <fs_data>-hbkid hktid = <fs_data>-hktid bukrs = <fs_data>-bukrs BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_data>-hktid_ms = ls_t012t-text1.
      ENDIF.

      READ TABLE lt_tfkbt INTO DATA(ls_tfkbt) WITH KEY fkber = <fs_data>-fkber_long BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_data>-fkbtx = ls_tfkbt-fkbtx.
      ENDIF.

      READ TABLE lt_aufk INTO DATA(ls_aufk) WITH KEY aufnr = <fs_data>-aufnr BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_data>-auart = ls_aufk-auart.
      ENDIF.

      READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = <fs_data>-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_data>-maktx = ls_makt-maktx.
      ENDIF.

      READ TABLE lt_anla INTO DATA(ls_anla) WITH KEY anln1 = <fs_data>-anln1 anln2 = <fs_data>-anln2 BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_data>-anln1_ms = ls_anla-txt50.
      ENDIF.


      READ TABLE lt_tabwt INTO DATA(ls_tabwt) WITH KEY bwasl = <fs_data>-anbwa BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_data>-anbwa_ms = ls_tabwt-bwatxt.
      ENDIF.

    ENDLOOP.

  ENDIF.


*  SELECT  DISTINCT
*           bkpf~bukrs
*           bkpf~belnr
*           bkpf~xblnr_alt
*           bkpf~gjahr
*           bkpf~monat
*           bseg~buzei AS docln
*           bseg~buzei
*           bkpf~awtyp
*           bkpf~awkey
*           bkpf~blart
*           bkpf~budat
*           bkpf~bldat
*           bkpf~cpudt
*           bkpf~cputm
*           bkpf~xblnr
*           bkpf~bstat
*           bkpf~xreversal
*           bkpf~stblg
*           bkpf~stjah
*           bkpf~ppnam
*           bkpf~usnam
*           bkpf~tcode
*           bkpf~xref1_hd
*           bkpf~xref2_hd
*           bkpf~bktxt
*           bseg~bschl
*           bseg~umskz
*           bseg~shkzg
*           bseg~xnegp
*           bseg~projk  "by sw
*
*           bseg~koart
*           bseg~hkont
*           skat~txt50 AS hkont_ms     "总账科目描述
*           bseg~wrbtr
*           bkpf~waers
*           bseg~dmbtr
*           bkpf~hwaer
*           bseg~zuonr
*           bseg~sgtxt
*           bseg~prctr
*           cepct~ktext               "利润中心描述
*           bseg~kunnr
*           bseg~vptnr
*           bseg~kkber
*           t014t~kkbtx               " 贷方控制范围描述
*           bseg~vbel2
*           bseg~posn2
*           bseg~lifnr
*           bseg~ebeln
*           bseg~ebelp
*           bseg~zfbdt
*           bseg~zbd1t
*           bseg~zbd2t
*           bseg~zbd3t
*           bseg~zlsch
*           bseg~zterm
*           bseg~vbund
*           t880~name1 AS vbund_ms
*           bseg~mwskz
*          t007s~text1 AS mwskz_ms     "税码描述
*           bseg~rstgr
*           t053s~txt20 AS rstgr_ms   "原因代码描述
*           bseg~hbkid
*           bseg~hktid
*           t012t~text1 AS hktid_ms  "账户标识描述
*           bseg~kokrs
*           bseg~fkber_long
*           tfkbt~fkbtx               "功能范围描述
*           bseg~aufnr
*           aufk~auart                  "订单类型
*           bseg~kostl
*           cskt~ktext AS kostl_ms     "成本中心描述
*           bseg~matnr
*           makt~maktx                    "物料描述
*           bseg~menge
*           bseg~werks
*           bseg~meins
*           bseg~gsber
*           bseg~valut
*           bseg~anln1
*           bseg~anln2
*           anla~txt50 AS anln1_ms     "资产描述
*           bseg~anbwa
*           tabwt~bwatxt AS anbwa_ms  "业务类型描述
*           bseg~paobjnr
*     INTO CORRESPONDING FIELDS OF TABLE gt_data
*     FROM bseg INNER JOIN bkpf ON bkpf~bukrs = bseg~bukrs AND bkpf~gjahr = bseg~gjahr AND bkpf~belnr = bseg~belnr
*              LEFT JOIN skat  ON bseg~hkont = skat~saknr AND skat~spras = sy-langu
*               LEFT JOIN cepct ON bseg~prctr = cepct~prctr AND cepct~spras = sy-langu
*               LEFT JOIN t014t ON bseg~kkber = t014t~kkber AND t014t~spras = sy-langu
*               LEFT JOIN t880 ON bseg~vbund = t880~rcomp
*               LEFT JOIN t007s ON bseg~mwskz = t007s~mwskz AND t007s~spras = sy-langu AND t007s~kalsm = 'TAXCN'
*               LEFT JOIN t053s ON bseg~rstgr = t053s~rstgr AND bseg~bukrs = t053s~bukrs AND t053s~spras = sy-langu
*               LEFT JOIN t012t ON bseg~hbkid = t012t~hbkid AND bseg~hktid = t012t~hktid AND bseg~bukrs = t012t~bukrs AND t012t~spras = sy-langu
*               LEFT JOIN tfkbt ON bseg~fkber_long = tfkbt~fkber AND tfkbt~spras = sy-langu
*               LEFT JOIN aufk ON bseg~aufnr = aufk~aufnr
*               LEFT JOIN cskt ON bseg~kostl = cskt~kostl AND cskt~spras = sy-langu AND cskt~datbi = '99991231'
*               LEFT JOIN makt ON bseg~matnr = makt~matnr AND makt~spras = sy-langu
*               LEFT JOIN anla ON bseg~bukrs = anla~bukrs AND bseg~anln1 = anla~anln1 AND bseg~anln2 = anla~anln2
*               LEFT JOIN tabwt ON bseg~anbwa = tabwt~bwasl AND tabwt~spras = sy-langu
*
*     WHERE bkpf~bukrs IN s_bukrs
*       AND bkpf~belnr IN s_belnr
*       AND bkpf~xblnr_alt IN s_xblnr
*       AND bkpf~gjahr IN s_gjahr
*       AND bkpf~monat IN s_monat
*       AND bkpf~blart IN s_blart
*       AND bkpf~budat IN s_budat
*       AND bkpf~cpudt IN s_cpudt
*       AND bkpf~ppnam IN s_ppnam
*       AND bkpf~waers IN s_waers
*       AND bkpf~usnam IN s_usnam
*       AND bkpf~usnam IN s_user
*       AND bkpf~bktxt IN s_bktxt
*       AND bkpf~xref1_hd IN s_xref1
*       AND bkpf~xref2_hd IN s_xref2
*       AND bseg~hkont IN s_hkont
*       AND bseg~koart IN s_koart
*       AND bseg~bschl IN s_bschl
*       AND bseg~umskz IN s_umskz
*       AND bseg~xnegp IN s_xnegp
*       AND bseg~prctr IN s_prctr
*       AND bseg~sgtxt IN s_sgtxt
*       AND bseg~kostl IN s_kostl
*       AND bseg~aufnr IN s_aufnr
*       AND bseg~kkber IN s_kkber
*       AND bseg~kunnr IN s_kunnr
*       AND bseg~lifnr IN s_lifnr
*       AND bseg~vbund IN s_vbund
*       AND bseg~fkber_long IN s_fkber
*       AND bseg~matnr IN s_matnr
*       AND bseg~rstgr IN s_rstgr
*       AND bseg~anln1 IN s_anln1
*       AND bseg~wrbtr IN s_wrbtr
*       AND bseg~dmbtr IN s_dmbtr
*       AND bseg~hktid IN  s_hktid
*       AND bseg~zuonr IN s_zuonr
*       AND bseg~projk IN s_projk  "by sw
*        AND bkpf~bstat = ''.

*  DATA: lw_data2 TYPE ty_data.
*  LOOP AT lt_data1 INTO lw_data1.
*    READ TABLE gt_data INTO lw_data2 WITH KEY bukrs = lw_data1-bukrs gjahr = lw_data1-gjahr belnr = lw_data1-belnr buzei = lw_data1-buzei.
*    IF sy-subrc = 0.
*      lw_data1-zfbdt = lw_data2-zfbdt.
*      lw_data1-zbd1t = lw_data2-zbd1t.
*      lw_data1-zbd2t = lw_data2-zbd2t.
*      lw_data1-zbd3t = lw_data2-zbd3t.
*      lw_data1-zlsch = lw_data2-zlsch.
*      lw_data1-zterm = lw_data2-zterm.
*      lw_data1-rstgr = lw_data2-rstgr.
*      lw_data1-rstgr_ms = lw_data2-rstgr_ms.
*      lw_data1-gsber = lw_data2-gsber.
*      lw_data1-valut = lw_data2-valut.
*      MODIFY lt_data1 FROM lw_data1.
*    ENDIF.
*  ENDLOOP.
*
*  gt_data[] = lt_data1[].

  IF p_bhyz = 'X'."包含预制凭证

    REFRESH:lt_data_yz,lt_data_yz_s,lt_data_yz_d,lt_data_yz_k,lt_data_yz_a,lt_vbsegd,lt_vbsegk,lt_kkber,lt_hbkid,lt_t014t,lt_t012t.

    "从vbkpf、vbsegs中取数
    SELECT DISTINCT bkpf~bukrs
           bkpf~belnr
           bkpf~xblnr_alt
           bkpf~gjahr
           bkpf~monat
           bkpf~ausbk
           vbsegs~buzei
           bkpf~awtyp
           bkpf~awkey
           bkpf~blart
           bkpf~budat
           bkpf~bldat
           bkpf~cpudt
           bkpf~cputm
           bkpf~bstat
           bkpf~xreversal
           bkpf~stblg
           bkpf~stjah
           bkpf~ppnam
           bkpf~usnam
           bkpf~tcode
           bkpf~xref1_hd
           bkpf~xref2_hd
           bkpf~bktxt
          vbsegs~bschl
*         vbsegd~umskz
          vbsegs~shkzg
          vbsegs~xnegp
          vbsegs~koart
          vbsegs~saknr AS hkont
          skat~txt50 AS hkont_ms     "总账科目描述
          vbsegs~wrbtr
          bkpf~waers
         "WRBTR_FH
          vbsegs~dmbtr
          bkpf~hwaer
       "DMBTR_FH
          vbsegs~zuonr
          vbsegs~sgtxt
          vbsegs~prctr
          cepct~ktext               "利润中心描述
*       vbsegd~kunnr
     "   KUNNR_MS
*       vbsegd~kkber
*       t014t~kkbtx               " 贷方控制范围描述
*       vbsegs~vbel2
*       vbsegs~posn2
*      vbsegk~lifnr
     "LIFNR_MS
         vbsegs~ebeln
         vbsegs~ebelp
         vbsegs~zfbdt
*       vbsegk~zbd1t
*       vbsegk~zbd2t
*       vbsegk~zbd3t
*       vbsegk~zterm
         vbsegs~vbund
         t880~name1 AS vbund_ms
         vbsegs~mwskz
*         t007s~text1 AS mwskz_ms     "税码描述
         vbsegs~rstgr
         t053s~txt20 AS rstgr_ms   "原因代码描述
*       vbsegd~hbkid
*       vbsegd~hktid
*       t012t~text1 AS hktid_ms  "账户标识描述
        vbsegs~kokrs
        vbsegs~fkber AS fkber_long
        tfkbt~fkbtx               "功能范围描述
        vbsegs~aufnr
        aufk~auart                  "订单类型
      "AUART_MS
        vbsegs~kostl
        cskt~ktext AS kostl_ms     "成本中心描述
        vbsegs~matnr
        makt~maktx                    "物料描述
        vbsegs~menge
        vbsegs~werks
        vbsegs~meins
*        vbsegs~zz_matkl
        vbsegs~gsber
        vbsegs~valut
        vbsegs~anln1
        vbsegs~anln2
        anla~txt50 AS anln1_ms     "资产描述
        vbsegs~anbwa
        tabwt~bwatxt AS anbwa_ms  "业务类型描述
 INTO CORRESPONDING FIELDS OF TABLE lt_data_yz_s
 FROM vbsegs  INNER JOIN bkpf ON bkpf~ausbk = vbsegs~ausbk AND bkpf~gjahr = vbsegs~gjahr AND bkpf~belnr = vbsegs~belnr
           LEFT JOIN skat  ON vbsegs~saknr = skat~saknr AND skat~spras = sy-langu
           LEFT JOIN cepct ON vbsegs~prctr = cepct~prctr AND cepct~spras = sy-langu
           LEFT JOIN t880 ON vbsegs~vbund = t880~rcomp
*           LEFT JOIN t007s ON vbsegs~mwskz = t007s~mwskz AND t007s~spras = sy-langu AND t007s~kalsm = 'TAXCN'
           LEFT JOIN t053s ON vbsegs~rstgr = t053s~rstgr AND vbsegs~bukrs = t053s~bukrs AND t053s~spras = sy-langu
           LEFT JOIN tfkbt ON vbsegs~fkber = tfkbt~fkber AND tfkbt~spras = sy-langu
           LEFT JOIN aufk ON vbsegs~aufnr = aufk~aufnr
           LEFT JOIN cskt ON vbsegs~kostl = cskt~kostl AND cskt~spras = sy-langu AND cskt~datbi = '99991231'
           LEFT JOIN makt ON vbsegs~matnr = makt~matnr AND makt~spras = sy-langu
           LEFT JOIN anla ON vbsegs~ausbk = anla~bukrs AND vbsegs~anln1 = anla~anln1 AND vbsegs~anln2 = anla~anln2
           LEFT JOIN tabwt ON vbsegs~anbwa = tabwt~bwasl AND tabwt~spras = sy-langu

 WHERE bkpf~bukrs IN s_bukrs
   AND bkpf~belnr IN s_belnr
   AND bkpf~xblnr_alt IN s_xblnr
   AND bkpf~gjahr IN s_gjahr
   AND bkpf~monat IN s_monat
   AND bkpf~blart IN s_blart
   AND bkpf~budat IN s_budat
   AND bkpf~cpudt IN s_cpudt
   AND bkpf~ppnam IN s_ppnam
   AND bkpf~waers IN s_waers
   AND bkpf~usnam IN s_usnam
   AND bkpf~bktxt IN s_bktxt
   AND bkpf~xref1_hd IN s_xref1
   AND bkpf~xref2_hd IN s_xref2
   AND vbsegs~saknr IN s_hkont
   AND vbsegs~koart IN s_koart
   AND vbsegs~bschl IN s_bschl
   AND vbsegs~xnegp IN s_xnegp
   AND vbsegs~prctr IN s_prctr
   AND vbsegs~sgtxt IN s_sgtxt
   AND vbsegs~kostl IN s_kostl
   AND vbsegs~aufnr IN s_aufnr
   AND vbsegs~vbund IN s_vbund
   AND vbsegs~fkber IN s_fkber
   AND vbsegs~matnr IN s_matnr
   AND vbsegs~rstgr IN s_rstgr
   AND vbsegs~anln1 IN s_anln1
   AND vbsegs~zuonr IN s_zuonr
   AND bkpf~bstat = 'V'.."V是预制凭证

    "从vbkpf、vbsega中取数
    SELECT DISTINCT bkpf~bukrs
           bkpf~belnr
           bkpf~xblnr_alt
           bkpf~gjahr
           bkpf~monat
           bkpf~ausbk
           vbsega~buzei
           bkpf~awtyp
           bkpf~awkey
           bkpf~blart
           bkpf~budat
           bkpf~bldat
           bkpf~cpudt
           bkpf~cputm
           bkpf~bstat
           bkpf~xreversal
           bkpf~stblg
           bkpf~stjah
           bkpf~ppnam
           bkpf~usnam
           bkpf~tcode
           bkpf~xref1_hd
           bkpf~xref2_hd
           bkpf~bktxt
          vbsega~bschl
          vbsega~shkzg
          vbsega~xnegp
          vbsega~hkont
         skat~txt50 AS hkont_ms     "总账科目描述
          vbsega~wrbtr
          bkpf~waers
          vbsega~dmbtr
          bkpf~hwaer
          vbsega~zuonr
          vbsega~sgtxt
          vbsega~prctr
          cepct~ktext               "利润中心描述
         vbsega~mwskz
*         t007s~text1 AS mwskz_ms     "税码描述
        vbsega~fkber AS fkber_long
        tfkbt~fkbtx               "功能范围描述
        vbsega~aufnr
        aufk~auart                  "订单类型
        vbsega~kostl
        cskt~ktext AS kostl_ms     "成本中心描述
        vbsega~matnr
        makt~maktx                    "物料描述
        vbsega~menge
        vbsega~werks
        vbsega~meins
        vbsega~gsber
        vbsega~anln1
        vbsega~anln2
        anla~txt50 AS anln1_ms     "资产描述
        vbsega~anbwa
        tabwt~bwatxt AS anbwa_ms  "业务类型描述
 INTO CORRESPONDING FIELDS OF TABLE lt_data_yz_a
 FROM vbsega  INNER JOIN bkpf ON bkpf~ausbk = vbsega~ausbk AND bkpf~gjahr = vbsega~gjahr AND bkpf~belnr = vbsega~belnr
           LEFT JOIN skat  ON vbsega~hkont = skat~saknr AND skat~spras = sy-langu
           LEFT JOIN cepct ON vbsega~prctr = cepct~prctr AND cepct~spras = sy-langu
*           LEFT JOIN t007s ON vbsega~mwskz = t007s~mwskz AND t007s~spras = sy-langu AND t007s~kalsm = 'TAXCN'
           LEFT JOIN tfkbt ON vbsega~fkber = tfkbt~fkber AND tfkbt~spras = sy-langu
           LEFT JOIN aufk ON vbsega~aufnr = aufk~aufnr
           LEFT JOIN cskt ON vbsega~kostl = cskt~kostl AND cskt~spras = sy-langu AND cskt~datbi = '99991231'
           LEFT JOIN makt ON vbsega~matnr = makt~matnr AND makt~spras = sy-langu
           LEFT JOIN anla ON vbsega~ausbk = anla~bukrs AND vbsega~anln1 = anla~anln1 AND vbsega~anln2 = anla~anln2
           LEFT JOIN tabwt ON vbsega~anbwa = tabwt~bwasl AND tabwt~spras = sy-langu

 WHERE bkpf~bukrs IN s_bukrs
   AND bkpf~belnr IN s_belnr
   AND bkpf~xblnr_alt IN s_xblnr
   AND bkpf~gjahr IN s_gjahr
   AND bkpf~monat IN s_monat
   AND bkpf~blart IN s_blart
   AND bkpf~budat IN s_budat
   AND bkpf~cpudt IN s_cpudt
   AND bkpf~ppnam IN s_ppnam
   AND bkpf~waers IN s_waers
   AND bkpf~usnam IN s_usnam
   AND bkpf~bktxt IN s_bktxt
   AND bkpf~xref1_hd IN s_xref1
   AND bkpf~xref2_hd IN s_xref2
   AND vbsega~hkont IN s_hkont
   AND vbsega~bschl IN s_bschl
   AND vbsega~xnegp IN s_xnegp
   AND vbsega~prctr IN s_prctr
   AND vbsega~sgtxt IN s_sgtxt
   AND vbsega~kostl IN s_kostl
   AND vbsega~aufnr IN s_aufnr
   AND vbsega~fkber IN s_fkber
   AND vbsega~matnr IN s_matnr
   AND vbsega~anln1 IN s_anln1
   AND vbsega~zuonr IN s_zuonr
   AND bkpf~bstat = 'V'.."V是预制凭证


    "从bkpf vbsegd中取数
    "从vbkpf、vbsegs中取数
    SELECT DISTINCT bkpf~bukrs
           bkpf~belnr
           bkpf~xblnr_alt
           bkpf~gjahr
           bkpf~monat
           bkpf~ausbk
           vbsegd~buzei
           bkpf~awtyp
           bkpf~awkey
           bkpf~blart
           bkpf~budat
           bkpf~bldat
           bkpf~cpudt
           bkpf~cputm
           bkpf~bstat
           bkpf~xreversal
           bkpf~stblg
           bkpf~stjah
           bkpf~ppnam
           bkpf~usnam
           bkpf~tcode
           bkpf~xref1_hd
           bkpf~xref2_hd
           bkpf~bktxt
           vbsegd~bschl
           vbsegd~umskz
           vbsegd~shkzg
           vbsegd~xnegp

          vbsegd~hkont
          skat~txt50 AS hkont_ms     "总账科目描述

           vbsegd~wrbtr
           bkpf~waers
         "WRBTR_FH
           vbsegd~dmbtr
           bkpf~hwaer
       "DMBTR_FH
           vbsegd~zuonr
           vbsegd~sgtxt
           vbsegd~kunnr
          "KUNNR_MS
           vbsegd~kkber
           t014t~kkbtx               " 贷方控制范围描述
           vbsegd~zfbdt
           vbsegd~zbd1t
           vbsegd~zbd2t
           vbsegd~zbd3t
           vbsegd~zterm
*         vbsegs~vbund
*         t880~name1 AS vbund_ms
           vbsegd~mwskz
*           t007s~text1 AS mwskz_ms     "税码描述
           vbsegd~rstgr
           t053s~txt20 AS rstgr_ms   "原因代码描述
           vbsegd~hbkid
           vbsegd~hktid
           t012t~text1 AS hktid_ms  "账户标识描述
*        vbsegs~kokrs
           vbsegd~fkber AS fkber_long
           tfkbt~fkbtx               "功能范围描述
*        vbsegs~aufnr
*        aufk~auart                  "订单类型
      "AUART_MS
         vbsegd~gsber
         vbsegd~valut
         vbsegd~anbwa
         tabwt~bwatxt AS anbwa_ms  "业务类型描述

 INTO CORRESPONDING FIELDS OF TABLE lt_data_yz_d
 FROM vbsegd  INNER JOIN bkpf ON bkpf~ausbk = vbsegd~ausbk AND bkpf~gjahr = vbsegd~gjahr AND bkpf~belnr = vbsegd~belnr
              LEFT JOIN skat  ON vbsegd~hkont = skat~saknr AND skat~spras = sy-langu
              LEFT JOIN t014t ON vbsegd~kkber = t014t~kkber AND t014t~spras = sy-langu
              LEFT JOIN t012t ON vbsegd~hbkid = t012t~hbkid AND vbsegd~hktid = t012t~hktid AND vbsegd~ausbk = t012t~bukrs AND t012t~spras = sy-langu
*              LEFT JOIN t007s ON vbsegd~mwskz = t007s~mwskz AND t007s~spras = sy-langu AND t007s~kalsm = 'TAXCN'
              LEFT JOIN t053s ON vbsegd~rstgr = t053s~rstgr AND vbsegd~bukrs = t053s~bukrs AND t053s~spras = sy-langu
              LEFT JOIN tfkbt ON vbsegd~fkber = tfkbt~fkber AND tfkbt~spras = sy-langu
           LEFT JOIN tabwt ON vbsegd~anbwa = tabwt~bwasl AND tabwt~spras = sy-langu
 WHERE bkpf~bukrs IN s_bukrs
   AND bkpf~belnr IN s_belnr
   AND bkpf~xblnr_alt IN s_xblnr
   AND bkpf~gjahr IN s_gjahr
   AND bkpf~monat IN s_monat
   AND bkpf~blart IN s_blart
   AND bkpf~budat IN s_budat
   AND bkpf~cpudt IN s_cpudt
   AND bkpf~ppnam IN s_ppnam
   AND bkpf~waers IN s_waers
   AND bkpf~usnam IN s_usnam
   AND bkpf~bktxt IN s_bktxt
   AND bkpf~xref1_hd IN s_xref1
   AND bkpf~xref2_hd IN s_xref2
   AND vbsegd~hkont IN s_hkont
*   AND vbsegs~koart IN s_koart
   AND vbsegd~bschl IN s_bschl
   AND vbsegd~umskz IN s_umskz
   AND vbsegd~xnegp IN s_xnegp
*   AND vbsegs~prctr IN s_prctr
   AND vbsegd~sgtxt IN s_sgtxt
*   AND vbsegs~kostl IN s_kostl
*   AND vbsegs~aufnr IN s_aufnr
   AND vbsegd~kkber IN s_kkber
   AND vbsegd~kunnr IN s_kunnr
*   AND vbsegk~lifnr IN s_lifnr
*   AND vbsegs~vbund IN s_vbund
   AND vbsegd~fkber IN s_fkber
*   AND vbsegs~matnr IN s_matnr
   AND vbsegd~rstgr IN s_rstgr
*   AND vbsegs~anln1 IN s_anln1
*   AND vbsegs~zz_item1 IN s_item1
*   AND vbsegs~zz_item2 IN s_item2
*   AND vbsegs~zz_item3 IN s_item3
*   AND vbsegs~zz_item4 IN s_item4
*   AND vbsegs~zz_item5 IN s_item5
*   AND vbsegs~zz_item6 IN s_item6
   AND vbsegd~hktid IN  s_hktid
*   AND vbsegs~zz_extvbeln IN s_extvbe
*   AND vbsegs~zz_matkl IN s_matkl
*   AND vbsegs~zz_item7 IN s_item7
*   AND vbsegs~zz_item8 IN s_item8
   AND vbsegd~zuonr IN s_zuonr
   AND bkpf~bstat = 'V'.."V是预制凭证


    "从bkpf、vbsegk中取数

    SELECT DISTINCT bkpf~bukrs
              bkpf~belnr
              bkpf~xblnr_alt
              bkpf~gjahr
              bkpf~monat
              bkpf~ausbk
              vbsegk~buzei
              bkpf~awtyp
              bkpf~awkey
              bkpf~blart
              bkpf~budat
              bkpf~bldat
              bkpf~cpudt
              bkpf~cputm
              bkpf~bstat
              bkpf~xreversal
              bkpf~stblg
              bkpf~stjah
              bkpf~ppnam
              bkpf~usnam
              bkpf~tcode
              bkpf~xref1_hd
              bkpf~xref2_hd
              bkpf~bktxt
             vbsegk~bschl
             vbsegk~umskz
             vbsegk~shkzg
             vbsegk~xnegp
             vbsegk~hkont
             skat~txt50 AS hkont_ms     "总账科目描述
             vbsegk~wrbtr
             bkpf~waers
            "WRBTR_FH
             vbsegk~dmbtr
             bkpf~hwaer
          "DMBTR_FH
             vbsegk~zuonr
             vbsegk~sgtxt
             vbsegk~lifnr
        "LIFNR_MS
*            vbsegs~ebeln
*            vbsegs~ebelp
*            vbsegs~zfbdt
             vbsegk~zbd1t
             vbsegk~zbd2t
             vbsegk~zbd3t
             vbsegk~zterm
          "  vbsegs~vbund
          "  t880~name1 AS vbund_ms
            vbsegk~mwskz
*            t007s~text1 AS mwskz_ms     "税码描述
            vbsegk~rstgr
            t053s~txt20 AS rstgr_ms   "原因代码描述
            vbsegk~hbkid
            vbsegk~hktid
            t012t~text1 AS hktid_ms  "账户标识描述
        "   vbsegs~kokrs
           vbsegk~fkber AS fkber_long
           tfkbt~fkbtx               "功能范围描述
           vbsegk~gsber
           vbsegk~valut
           vbsegk~anbwa
           tabwt~bwatxt AS anbwa_ms  "业务类型描述

    INTO CORRESPONDING FIELDS OF TABLE lt_data_yz_k
    FROM vbsegk  INNER JOIN bkpf ON bkpf~ausbk = vbsegk~ausbk AND bkpf~gjahr = vbsegk~gjahr AND bkpf~belnr = vbsegk~belnr
              LEFT JOIN skat  ON vbsegk~hkont = skat~saknr AND skat~spras = sy-langu
*              LEFT JOIN t007s ON vbsegk~mwskz = t007s~mwskz AND t007s~spras = sy-langu AND t007s~kalsm = 'TAXCN'
              LEFT JOIN t053s ON vbsegk~rstgr = t053s~rstgr AND vbsegk~bukrs = t053s~bukrs AND t053s~spras = sy-langu
              LEFT JOIN t012t ON vbsegk~hbkid = t012t~hbkid AND vbsegk~hktid = t012t~hktid AND vbsegk~ausbk = t012t~bukrs AND t012t~spras = sy-langu
              LEFT JOIN tfkbt ON vbsegk~fkber = tfkbt~fkber AND tfkbt~spras = sy-langu
              LEFT JOIN tabwt ON vbsegk~anbwa = tabwt~bwasl AND tabwt~spras = sy-langu

    WHERE bkpf~bukrs IN s_bukrs
      AND bkpf~belnr IN s_belnr
      AND bkpf~xblnr_alt IN s_xblnr
      AND bkpf~gjahr IN s_gjahr
      AND bkpf~monat IN s_monat
      AND bkpf~blart IN s_blart
      AND bkpf~budat IN s_budat
      AND bkpf~cpudt IN s_cpudt
      AND bkpf~ppnam IN s_ppnam
      AND bkpf~waers IN s_waers
      AND bkpf~usnam IN s_usnam
      AND bkpf~bktxt IN s_bktxt
      AND bkpf~xref1_hd IN s_xref1
      AND bkpf~xref2_hd IN s_xref2
      AND vbsegk~hkont IN s_hkont
      AND vbsegk~bschl IN s_bschl
      AND vbsegk~umskz IN s_umskz
      AND vbsegk~xnegp IN s_xnegp
      AND vbsegk~sgtxt IN s_sgtxt
      AND vbsegk~lifnr IN s_lifnr
      AND vbsegk~fkber IN s_fkber
      AND vbsegk~rstgr IN s_rstgr
      AND vbsegk~hktid IN  s_hktid
      AND vbsegk~zuonr IN s_zuonr
      AND bkpf~bstat = 'V'.."V是预制凭证

    "从不同表中取的值，科目类别的值不同
    "vbsegs中取值，则科目类别为S
    LOOP AT lt_data_yz_s INTO lw_data_yz_s.
      MOVE lw_data_yz_s TO lw_data_yz.
      lw_data_yz-koart = 'S'.
      APPEND lw_data_yz TO lt_data_yz.
      CLEAR:lw_data_yz_s,lw_data_yz.
    ENDLOOP.
    "vbsega中取值，则科目类别为A
    LOOP AT lt_data_yz_a INTO lw_data_yz_a.
      MOVE lw_data_yz_a TO lw_data_yz.
      lw_data_yz-koart = 'A'.
      APPEND lw_data_yz TO lt_data_yz.
      CLEAR:lw_data_yz_a,lw_data_yz.
    ENDLOOP.
    "vbsegD中取值，则科目类别为D
    LOOP AT lt_data_yz_d INTO lw_data_yz_d.
      MOVE lw_data_yz_d TO lw_data_yz.
      lw_data_yz-koart = 'D'.
      APPEND lw_data_yz TO lt_data_yz.
      CLEAR:lw_data_yz_d,lw_data_yz.
    ENDLOOP.
    "vbsegk中取值，则科目类别为K
    LOOP AT lt_data_yz_k INTO lw_data_yz_k.
      MOVE lw_data_yz_k TO lw_data_yz.
      lw_data_yz-koart = 'K'.
      APPEND lw_data_yz TO lt_data_yz.
      CLEAR:lw_data_yz_k,lw_data_yz.
    ENDLOOP.


    LOOP AT lt_data_yz INTO lw_data_yz.

      " 贷方控制范围
      lw_kkber-kkber = lw_data_yz-kkber.
      APPEND lw_kkber TO lt_kkber.

      " 账户标识
      lw_hbkid-ausbk = lw_data_yz-ausbk.
      lw_hbkid-hbkid = lw_data_yz-hbkid.
      lw_hbkid-hktid = lw_data_yz-hktid.

      APPEND lw_hbkid TO lt_hbkid.

      CLEAR:lw_data_yz,lw_kkber,lw_hbkid.
    ENDLOOP.

    "查询贷方控制范围描述
    IF lt_kkber[] IS NOT INITIAL.
      SELECT *
        INTO TABLE lt_t014t
        FROM t014t FOR ALL ENTRIES IN lt_kkber
        WHERE kkber = lt_kkber-kkber
          AND spras = sy-langu.
      SORT lt_t014t BY kkber.
    ENDIF.
    "查询账户标识描述
    IF lt_hbkid[] IS NOT INITIAL.
      SELECT *
    INTO TABLE lt_t012t
    FROM t012t FOR ALL ENTRIES IN lt_hbkid
    WHERE bukrs = lt_hbkid-ausbk
      AND hbkid = lt_hbkid-hbkid
      AND hktid = lt_hbkid-hktid
      AND spras = sy-langu.
      SORT lt_t012t BY bukrs hbkid hktid.
    ENDIF.

    "将最终符合所有条件的数据放入内表gt_data中
    LOOP AT lt_data_yz INTO lw_data_yz.
      MOVE-CORRESPONDING lw_data_yz TO gw_data.
      "查询贷方控制范围描述
      READ TABLE lt_t014t INTO lw_t014t WITH KEY kkber = lw_data_yz-kkber BINARY SEARCH.
      IF sy-subrc = 0.
        gw_data-kkbtx = lw_t014t-kkbtx.
      ENDIF.
      "查询账户标识描述
      READ TABLE lt_t012t INTO lw_t012t WITH KEY bukrs = lw_data_yz-ausbk hbkid = lw_data_yz-hbkid hktid = lw_data_yz-hktid BINARY SEARCH.
      IF sy-subrc = 0.
        gw_data-hktid_ms = lw_t012t-text1.
      ENDIF.
      IF gw_data-shkzg = 'H'.
        gw_data-wrbtr_fh = gw_data-wrbtr * -1.
        gw_data-dmbtr_fh = gw_data-dmbtr * -1.
      ELSE.
        gw_data-wrbtr_fh = gw_data-wrbtr.
        gw_data-dmbtr_fh = gw_data-dmbtr.
      ENDIF.
      APPEND gw_data TO gt_data.
      CLEAR:gw_data,lw_data_yz,lw_t014t,lw_t012t.

    ENDLOOP.
  ENDIF.


*关于一次性供应商和客户描述：当根据“公司代码+会计凭证+行项目号+会计年度”关联表BSEC有值时，取BSEC-NAME1，否则分别从主数据文本表中取值。
  LOOP AT gt_data INTO gw_data.
    "当供应商及客户为一次性供应商及描述时，取描述
    lw_data_tmp-bukrs = gw_data-bukrs.
    lw_data_tmp-belnr = gw_data-belnr.
    lw_data_tmp-buzei = gw_data-buzei.
    lw_data_tmp-gjahr = gw_data-gjahr.
    APPEND lw_data_tmp TO lt_data_tmp.

    "客户
    IF gw_data-kunnr IS NOT INITIAL.
      lw_kunnr-kunnr = gw_data-kunnr.
      APPEND lw_kunnr TO lt_kunnr.
    ENDIF.

    "供应商
    IF gw_data-lifnr IS NOT INITIAL.
      lw_lifnr-lifnr = gw_data-lifnr.
      APPEND lw_lifnr TO lt_lifnr.
    ENDIF.
*    业务员
*    IF gw_data-vptnr IS NOT INITIAL.
*      lw_vptnr-vptnr = gw_data-vptnr.
*      APPEND lw_vptnr TO lt_vptnr.
*    ENDIF.
    "订单类型
    lw_auart-auart = gw_data-auart.
    APPEND lw_auart TO lt_auart.

    CLEAR:gw_data,lw_data_tmp,lw_kunnr,lw_lifnr,lw_auart.
  ENDLOOP.

  SORT lt_kunnr BY kunnr.
  DELETE ADJACENT DUPLICATES FROM lt_kunnr COMPARING kunnr.

  SORT lt_lifnr BY lifnr.
  DELETE ADJACENT DUPLICATES FROM lt_lifnr COMPARING lifnr.

  SORT lt_auart BY auart.
  DELETE ADJACENT DUPLICATES FROM lt_auart COMPARING auart.

  "一次性供应商及客户描述
  IF lt_data_tmp[] IS NOT INITIAL.
    SELECT bukrs
           belnr
           buzei
           gjahr
           name1
      INTO CORRESPONDING FIELDS OF TABLE gt_bsec
      FROM bsec FOR ALL ENTRIES IN lt_data_tmp
      WHERE bukrs = lt_data_tmp-bukrs
        AND belnr = lt_data_tmp-belnr
        AND buzei = lt_data_tmp-buzei
        AND gjahr = lt_data_tmp-gjahr.

  ENDIF.

  "供应商描述
  IF lt_lifnr[] IS NOT INITIAL.
    SELECT lifnr
           name1
           name2
      INTO CORRESPONDING FIELDS OF TABLE gt_lfa1
      FROM lfa1 FOR ALL ENTRIES IN lt_lifnr
      WHERE lifnr = lt_lifnr-lifnr.
  ENDIF.

  "客户描述
  IF lt_kunnr[] IS NOT INITIAL.
    SELECT kunnr
           name1
           name2
      INTO CORRESPONDING FIELDS OF TABLE gt_kna1
      FROM kna1 FOR ALL ENTRIES IN lt_kunnr
      WHERE kunnr = lt_kunnr-kunnr.
  ENDIF.
  "业务员
  IF lt_vptnr[] IS NOT INITIAL.
    SELECT kunnr
           name1
           name2
      INTO CORRESPONDING FIELDS OF TABLE ht_kna1
      FROM kna1 FOR ALL ENTRIES IN lt_vptnr
      WHERE kunnr = lt_vptnr-vptnr.

  ENDIF.

  "订单类型描述T003P
  IF lt_auart[] IS NOT INITIAL.
    SELECT auart
           txt
      INTO CORRESPONDING FIELDS OF TABLE gt_t003p
      FROM t003p FOR ALL ENTRIES IN lt_auart
      WHERE auart = lt_auart-auart
        AND spras = sy-langu.
  ENDIF.

  IF gt_data IS NOT INITIAL.
    SELECT mwskz
           text1
    INTO TABLE gt_t007s
    FROM t007s FOR ALL ENTRIES IN gt_data
    WHERE mwskz = gt_data-mwskz
      AND spras = sy-langu
      AND kalsm = 'TAXCN'.

    SORT gt_t007s BY mwskz.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_process_data .
  SORT gt_bsec BY bukrs belnr buzei gjahr.
  SORT gt_kna1 BY kunnr.
  SORT ht_kna1 BY kunnr.
  SORT gt_lfa1 BY lifnr.
  SORT gt_t003p BY auart.

  IF gt_data IS NOT INITIAL.
*    SELECT aktbo
*           paobjnr
*           pasubnr
*           kaufn
*           kdpos
*   INTO CORRESPONDING FIELDS OF TABLE t_acct
*   FROM ce4coft_acct
*   FOR ALL ENTRIES IN gt_data
*   WHERE paobjnr EQ gt_data-paobjnr
*   AND aktbo EQ 'X'.
  ENDIF.

  SORT t_acct BY paobjnr.
  LOOP AT gt_data INTO gw_data.
*    READ TABLE ht_kna1 INTO gw_kna1 WITH KEY kunnr = gw_data-vptnr BINARY SEARCH.
*    IF sy-subrc = 0.
*      CONCATENATE gw_kna1-name1 gw_kna1-name2 INTO  gw_data-name1 .
*      CONDENSE  gw_data-name1 .
*    ENDIF.

    IF gw_data-blart ='RV'.
      READ TABLE t_acct INTO wa_acct WITH KEY paobjnr = gw_data-paobjnr BINARY SEARCH.
      IF sy-subrc = 0.
        gw_data-vbel2 = wa_acct-kaufn.
        gw_data-posn2 = wa_acct-kdpos.
      ENDIF.
    ENDIF.

    "当“借/贷标识（BSEG-SHKZG）”=“H”时，按负数计算，否则直接取值；
*******    IF gw_data-shkzg = 'H'.
*******      gw_data-wrbtr_fh = gw_data-wrbtr * -1.
*******      gw_data-dmbtr_fh = gw_data-dmbtr * -1.
*******    ELSE.
*******      gw_data-wrbtr_fh = gw_data-wrbtr.
*******      gw_data-dmbtr_fh = gw_data-dmbtr.
*******    ENDIF.

    "供应商及客户描述
    READ TABLE gt_bsec INTO gw_bsec WITH KEY bukrs = gw_data-bukrs belnr = gw_data-belnr buzei = gw_data-buzei gjahr = gw_data-gjahr BINARY SEARCH.
    IF sy-subrc = 0.
      gw_data-kunnr_ms = gw_bsec-name1.
      gw_data-lifnr_ms = gw_bsec-name1.
    ELSE.
      "客户描述
      READ TABLE gt_kna1 INTO gw_kna1 WITH KEY kunnr = gw_data-kunnr BINARY SEARCH.
      IF sy-subrc = 0.
        CONCATENATE gw_kna1-name1 gw_kna1-name2 INTO  gw_data-kunnr_ms .
        CONDENSE  gw_data-kunnr_ms .
      ENDIF .

      "供应商描述
      READ TABLE gt_lfa1 INTO gw_lfa1 WITH KEY lifnr = gw_data-lifnr BINARY SEARCH.
      IF sy-subrc = 0.
        CONCATENATE gw_lfa1-name1 gw_lfa1-name2 INTO  gw_data-lifnr_ms .
        CONDENSE  gw_data-lifnr_ms .
      ENDIF .
    ENDIF.

    "税码描述
    READ TABLE gt_t007s INTO DATA(ls_t007s) WITH KEY mwskz = gw_data-mwskz BINARY SEARCH.
    IF sy-subrc = 0.
      gw_data-mwskz_ms = ls_t007s-text1.
    ENDIF.

    "订单类型描述
    READ TABLE gt_t003p INTO gw_t003p WITH KEY auart =  gw_data-auart BINARY SEARCH.
    IF sy-subrc = 0.
      gw_data-auart_ms = gw_t003p-txt.
    ENDIF.

    MODIFY gt_data FROM gw_data.

    CLEAR:gw_data,gw_bsec,gw_kna1,gw_lfa1,gw_t003p.
  ENDLOOP.
*add by liyunjian 20180927
  TYPES:BEGIN OF ty_data_ce1.
          INCLUDE STRUCTURE zsfirpt_012_alv.
  TYPES: rposn       TYPE rkerfposnr,
*         paobjnr     TYPE rkeobjnr,
         rbeln_xblnr TYPE rkerfbelnr,
         END OF ty_data_ce1.
  DATA:gt_data_ce1 TYPE TABLE OF ty_data_ce1,
       gw_data_ce1 TYPE ty_data_ce1.

*  DATA: gt_ce1coft TYPE STANDARD TABLE OF ce1coft,
*        gw_ce1coft TYPE ce1coft.
  TYPES:BEGIN OF ty_ce4coft_acct.
*      INCLUDE STRUCTURE ce4coft_acct.
  TYPES: paobjnr_ce4key TYPE rkeobjnr,
         END OF ty_ce4coft_acct.

  DATA: gt_ce4coft_acct TYPE STANDARD TABLE OF ty_ce4coft_acct,
        gw_ce4coft_acct TYPE ty_ce4coft_acct.
*  DATA: gt_ce4coft TYPE STANDARD TABLE OF ce4coft,
*        gw_ce4coft TYPE ce4coft.
  DATA: l_rposn TYPE num6.
****IDOC凭证
  CLEAR gt_data_ce1.
  LOOP AT gt_data INTO gw_data WHERE awtyp = 'IBKPF' AND paobjnr <> ''.
    MOVE-CORRESPONDING gw_data TO gw_data_ce1.
    CLEAR l_rposn.
    l_rposn = gw_data_ce1-buzei.
    gw_data_ce1-rposn = l_rposn .
    APPEND gw_data_ce1 TO gt_data_ce1.
    CLEAR gw_data.
  ENDLOOP.
**将需要的数据赋值给gt_data_ce1，用于下面筛选检查
  IF NOT gt_data_ce1[] IS INITIAL.
*    SELECT paledger
*           vrgar
*           versi
*           perio
*           paobjnr
*           pasubnr
*           belnr
*           posnr
*           kndnr
*           artnr
*           fkart
*           kaufn AS kaufn_ce1
*           kdpos AS kdpos_ce1
*           rkaufnr
*           werks AS werks_ce1
*           vkorg
*           vtweg
*           spart
*           bukrs
*           rbeln
*           budat
*           rposn
*       FROM ce1coft
*      INTO CORRESPONDING FIELDS OF TABLE gt_ce1coft
*      FOR ALL ENTRIES IN gt_data_ce1
*      WHERE bukrs = gt_data_ce1-bukrs AND
*            budat = gt_data_ce1-budat AND
*            rbeln = gt_data_ce1-belnr AND
*            rposn = gt_data_ce1-rposn AND
*            paledger = '02'.
*获取下面的获利字段，用于ALV输出
*    SORT gt_ce1coft BY bukrs budat rbeln rposn.
    LOOP AT gt_data ASSIGNING <fs_data>.
      READ TABLE gt_data_ce1 INTO gw_data_ce1 WITH KEY bukrs = <fs_data>-bukrs
                                      belnr = <fs_data>-belnr
                                      gjahr = <fs_data>-gjahr
                                      buzei = <fs_data>-buzei
                                      monat = <fs_data>-monat.
      IF sy-subrc = 0 .
*        READ TABLE gt_ce1coft INTO gw_ce1coft WITH KEY  bukrs = gw_data_ce1-bukrs
*                                                        budat = gw_data_ce1-budat
*                                                        rbeln = gw_data_ce1-belnr
*                                                        rposn = gw_data_ce1-rposn .
        IF sy-subrc = 0 .
*CE1COFT- KNDNR(客户)
*
*          <fs_data>-paobjnr = gw_ce1coft-paobjnr.
*          <fs_data>-kndnr = gw_ce1coft-kndnr.
**CE1COFT- ARTNR(生产)
*
*          <fs_data>-artnr = gw_ce1coft-artnr.
**CE1COFT- FKART (开票类型)
*
*          <fs_data>-fkart = gw_ce1coft-fkart.
**CE1COFT- KAUFN (销售订单)
*
*          <fs_data>-kaufn_ce1 = gw_ce1coft-kaufn.
**CE1COFT- KDPOS(销售订单项目)
*
*          <fs_data>-kdpos_ce1 = gw_ce1coft-kdpos.
**CE1COFT- RKAUFNR(订单)
*
*          <fs_data>-rkaufnr = gw_ce1coft-rkaufnr.
**CE1COFT- WERKS(工厂)
*
*          <fs_data>-werks_ce1 = gw_ce1coft-werks.
**CE1COFT- VKORG(销售组织)
*
*          <fs_data>-vkorg = gw_ce1coft-vkorg.
**CE1COFT- VTWEG(分销渠道)
*
*          <fs_data>-vtweg = gw_ce1coft-vtweg.
**CE1COFT- SPART(产品组)
*          <fs_data>-spart = gw_ce1coft-spart.
        ENDIF.

      ENDIF.
    ENDLOOP.

  ENDIF.

*非IDOC凭证
  CLEAR gt_data_ce1.
  LOOP AT gt_data INTO gw_data WHERE awtyp <> 'IBKPF' AND paobjnr <> ''.
    MOVE-CORRESPONDING gw_data TO gw_data_ce1.
    CLEAR l_rposn.
    l_rposn = gw_data_ce1-buzei.
    gw_data_ce1-rposn = l_rposn .
    gw_data_ce1-rbeln_xblnr = gw_data_ce1-xblnr.

    APPEND gw_data_ce1 TO gt_data_ce1.
    CLEAR gw_data.
  ENDLOOP.
*将需要的数据赋值给gt_data_ce1，用于下面筛选检查
  IF NOT gt_data_ce1[] IS INITIAL.
*    SELECT
*           paobjnr
*           ce4key AS paobjnr_ce4key
*      FROM ce4coft_acct
*      INTO CORRESPONDING FIELDS OF TABLE gt_ce4coft_acct
*      FOR ALL ENTRIES IN  gt_data_ce1
*       WHERE paobjnr = gt_data_ce1-paobjnr.
*按照BSEG-PAOBJNR = CE4COFT_ACCT-PAOBJNR时取CE4COFT_ACCT -CE4KEY值
*    SORT  gt_ce4coft_acct BY paobjnr.
    IF NOT gt_ce4coft_acct IS INITIAL.
*      SELECT paledger
*            vrgar
*            versi
*            perio
*            paobjnr
*            pasubnr
*            belnr
*            posnr
*            kndnr
*              artnr
*              fkart
*              kaufn AS kaufn_ce1
*              kdpos AS kdpos_ce1
*              rkaufnr
*              werks AS werks_ce1
*              vkorg
*              vtweg
*              spart
*            bukrs
*           rbeln
*           budat
*           rposn
*          FROM ce1coft
*         INTO CORRESPONDING FIELDS OF TABLE gt_ce1coft
*         FOR ALL ENTRIES IN gt_data_ce1
*         WHERE bukrs = gt_data_ce1-bukrs AND
*               budat = gt_data_ce1-budat AND
*               rbeln = gt_data_ce1-rbeln_xblnr AND
*               paledger = '02'.
*
*      SORT gt_ce1coft BY bukrs budat rbeln .

    ENDIF.
*获取下面的获利字段，用于ALV输出
*    LOOP AT gt_data ASSIGNING <fs_data>.
*      READ TABLE gt_data_ce1 INTO gw_data_ce1 WITH KEY bukrs = <fs_data>-bukrs
*                                      belnr = <fs_data>-belnr
*                                      gjahr = <fs_data>-gjahr
*                                      buzei = <fs_data>-buzei
*                                      monat = <fs_data>-monat.
*      IF sy-subrc = 0 .
*        READ TABLE  gt_ce4coft_acct INTO gw_ce4coft_acct WITH KEY paobjnr = gw_data_ce1-paobjnr.
*        IF sy-subrc = 0 .
*          READ TABLE gt_ce1coft INTO gw_ce1coft WITH KEY  bukrs = gw_data_ce1-bukrs
*                                                          budat = gw_data_ce1-budat
*                                                          rbeln = gw_data_ce1-rbeln_xblnr
*                                                          paobjnr = gw_ce4coft_acct-paobjnr_ce4key.
*          IF sy-subrc = 0 .
**CE1COFT- KNDNR(客户)
*
*            <fs_data>-paobjnr = gw_ce1coft-paobjnr.
*            <fs_data>-kndnr = gw_ce1coft-kndnr.
**CE1COFT- ARTNR(生产)
*
*            <fs_data>-artnr = gw_ce1coft-artnr.
**CE1COFT- FKART (开票类型)
*
*            <fs_data>-fkart = gw_ce1coft-fkart.
**CE1COFT- KAUFN (销售订单)
*
*            <fs_data>-kaufn_ce1 = gw_ce1coft-kaufn.
**CE1COFT- KDPOS(销售订单项目)
*
*            <fs_data>-kdpos_ce1 = gw_ce1coft-kdpos.
**CE1COFT- RKAUFNR(订单)
*
*            <fs_data>-rkaufnr = gw_ce1coft-rkaufnr.
**CE1COFT- WERKS(工厂)
*
*            <fs_data>-werks_ce1 = gw_ce1coft-werks.
**CE1COFT- VKORG(销售组织)
*
*            <fs_data>-vkorg = gw_ce1coft-vkorg.
**CE1COFT- VTWEG(分销渠道)
*
*            <fs_data>-vtweg = gw_ce1coft-vtweg.
**CE1COFT- SPART(产品组)
*            <fs_data>-spart = gw_ce1coft-spart.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

  ENDIF.
*end


* ADD BY ZXK 20190129 BEGIN
  IF gt_data IS NOT INITIAL.
    SELECT vbrp~vbeln,
           vbrp~vgbel
      FROM vbrp FOR ALL ENTRIES IN @gt_data
      WHERE vbeln = @gt_data-awkey+0(10)
*      JOIN @gt_data AS db
*        ON vbrp~vbeln = db~awkey
      INTO TABLE @DATA(lt_vbrp)
      .
  ENDIF.
  LOOP AT gt_data INTO DATA(ls_data).
    IF ls_data-blart = 'RV'.
      READ TABLE lt_vbrp INTO DATA(ls_vbrp) WITH KEY vbeln = ls_data-awkey.
      IF sy-subrc = 0.
        ls_data-vbeln = ls_vbrp-vbeln.
        ls_data-vgbel = ls_vbrp-vgbel.
      ENDIF.
    ENDIF.
    MODIFY gt_data FROM ls_data.
  ENDLOOP.
* ADD BY ZXK 20190129 END

  SORT gt_data BY bukrs belnr buzei gjahr monat.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_display_data .
  "设置布局
  PERFORM frm_set_layout.
  "alv显示
  PERFORM frm_alv_output.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_set_layout .
  " "设置布局
  gw_layout-cwidth_opt = 'X'.
  gw_layout-zebra      = 'X'.

  gv_repid = sy-repid.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_alv_output .
  "调用function，显示alv
  DATA: ls_layout   TYPE lvc_s_layo,
        lt_fieldcat TYPE lvc_t_fcat,
        ls_fieldcat TYPE LINE OF lvc_t_fcat.

  ls_layout-cwidth_opt = 'X'.
  ls_layout-zebra = 'X'.

*  设置FCAT
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSFIRPT_012_ALV'
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  DELETE lt_fieldcat WHERE fieldname = 'WRBTR_FH' OR fieldname = 'DMBTR_FH'.

*  ls_fieldcat-fieldname = 'VGBEL'.
*  ls_fieldcat-coltext   = '交货单号码'.
*  ls_fieldcat-col_pos   = 99.
*  APPEND ls_fieldcat TO lt_fieldcat.
*  CLEAR:ls_fieldcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = gv_repid
      i_callback_user_command  = 'FRM_USER_COMMAND'
      i_callback_pf_status_set = 'FRM_USER_STATUS'
*     i_structure_name         = 'ZSFIRPT_012_ALV'
      is_layout_lvc            = gw_layout
      it_fieldcat_lvc          = lt_fieldcat
      i_save                   = 'A'    "Modify by ZHANGZHIANG 13.11.2017 11:32:58 ERP-YW201711080067
    TABLES
      t_outtab                 = gt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_USER_STATUS
*&---------------------------------------------------------------------*
*       gui状态
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_user_status USING extab TYPE slis_t_extab.
  "设置gui状态
  SET PF-STATUS 'STATUS1' .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&IC1'."双击
      READ TABLE gt_data INTO gw_data INDEX rs_selfield-tabindex.
      SET PARAMETER ID 'BUK' FIELD gw_data-bukrs."公司代码
      SET PARAMETER ID 'BLN' FIELD gw_data-belnr. "会计凭证
      SET PARAMETER ID 'GJR' FIELD gw_data-gjahr. "会计年度
      CALL TRANSACTION 'FB03' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
