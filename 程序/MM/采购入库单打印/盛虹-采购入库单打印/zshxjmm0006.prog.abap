**************************************************
*程序名称:采购入库打印
*创建日期: 2019-11-29
*创建者:XXX
*申请者:XXX
*功能描述:
*============================================
*变更记录
*修改请求号    修改日期    修改人   修改描述
*DEVK912047    2019-11-29   HANDYXH    创建程序
***************************************************
REPORT zshxjmm0006.
TABLES:mseg,mkpf.
DATA:gv_all TYPE c.
DATA:ls_title TYPE string.
DATA : lt_tline TYPE TABLE OF tline WITH HEADER LINE  .
DATA:lv_name TYPE thead-tdname.
"特性值定义
DATA e_objek TYPE cuobn.
DATA e_objek1 TYPE bapi1003_key-object.
DATA e_obtab TYPE tabelle.
DATA e_klart TYPE klassenart.
DATA e_class TYPE klasse_d.
DATA BEGIN OF it_num OCCURS 0.
        INCLUDE STRUCTURE bapi1003_alloc_values_num.
DATA END OF it_num.
DATA BEGIN OF it_char OCCURS 0.
        INCLUDE STRUCTURE  bapi1003_alloc_values_char.
DATA END OF it_char.
DATA BEGIN OF it_curr OCCURS 0.
        INCLUDE STRUCTURE  bapi1003_alloc_values_curr.
DATA END OF it_curr.
DATA BEGIN OF bapi_return OCCURS 0.
        INCLUDE STRUCTURE bapiret2.
DATA END OF bapi_return.

DATA:print_head LIKE TABLE OF zsmm_print_001 WITH HEADER LINE.
DATA:print_body LIKE TABLE OF zsmm_print_b_001 WITH HEADER LINE.

DATA:BEGIN OF ls_tab,
       chk        TYPE c,
       mblnr      LIKE  mseg-mblnr,        "物料凭证
       mjahr      LIKE  mseg-mjahr,
       zeile      LIKE  mseg-zeile,        "物料凭证行号
       xauto      LIKE  mseg-xauto,
       matnr      LIKE  mseg-matnr,        "物料号
       sgtxt(255) TYPE c,                  "物料描述
       sgtxt_h    LIKE  mseg-sgtxt,         "物料凭证行文本
       vbeln_im   LIKE  mseg-vbeln_im,     "交货单
       vbelp_im   LIKE  mseg-vbelp_im,     "交货单行号
       vbeln      LIKE  vbap-vbeln,        "销售订单
       posnr      LIKE  vbap-posnr,        "销售订单行号
       ebeln      LIKE  mseg-ebeln,        "采购订单
       ebelp      LIKE  mseg-ebelp,        "采购订单行号
       bsart      LIKE  ekko-bsart,        "采购订单类型
       batxt      LIKE  t161t-batxt,
       rsnum      LIKE  mseg-rsnum,        "预留单
       rspos      LIKE  mseg-rspos,        "预留行号
       werks      LIKE  mseg-werks,        "工厂
       lgort      LIKE  mseg-lgort,        "库存地点
       umwrk      LIKE  mseg-umwrk,        "接收工厂
       umlgo      LIKE  mseg-umlgo,        "接收库存地点
       charg      LIKE  mseg-charg,        "批次
       meins      LIKE  mseg-meins,        "单位
       bwart      LIKE  mseg-bwart,        "移动类型
       budat      LIKE  mkpf-budat,        "过账日期
       anln1      LIKE  mseg-anln1,        "资产
       kostl      LIKE  mseg-kostl,        "成本中心
       ktext      LIKE  cskt-ktext,
       ps_psp_pnr LIKE  mseg-ps_psp_pnr,   "wbs
       menge      LIKE  mseg-menge,        "数量
       posex      LIKE vbap-posex,         "箱数
       menge_po   LIKE  ekpo-menge,        "采购订单数量
       bdmng      LIKE  resb-bdmng,        "预留申请数量
       bktxt      LIKE  mkpf-bktxt,        "制单人
       ekgrp      LIKE  ekko-ekgrp,        "采购组
       usnam      LIKE  mkpf-usnam,        "用户名
*       zwbhtid    LIKE  ekko-zwbhtid,      "合同
       matkl      LIKE  mara-matkl,        "物料组

*       lfsnr      LIKE  gohead-lfsnr,     "外向交货单
       lifnr      LIKE  mseg-lifnr,        "供应商
       kunnr      LIKE  mseg-kunnr,         "客户
       name1      LIKE  kuwev-name1,        "供应商
       name2      LIKE  kuwev-name1,     "客户
       ph         TYPE atwrt,
       qfh        TYPE atwrt,
       dj         TYPE atwrt,

     END OF ls_tab.                        "区分号、等级、批号
DATA:lt_tab LIKE TABLE OF ls_tab.
DATA:gt_tab LIKE TABLE OF ls_tab WITH HEADER LINE.
DATA:lt_ekko LIKE TABLE OF ekko WITH HEADER LINE.
DATA:lt_ekpo LIKE TABLE OF ekpo WITH HEADER LINE.
DATA:BEGIN OF ls_purchase,
       ebeln LIKE  mseg-ebeln,        "采购订单
       ebelp LIKE  mseg-ebelp,        "采购订单行号
     END OF ls_purchase.
DATA:lt_purchase LIKE TABLE OF ls_purchase.
DATA:lt_resb LIKE TABLE OF resb WITH HEADER LINE.
DATA:BEGIN OF ls_reserve,
       rsnum LIKE  mseg-rsnum,        "预留单
       rspos LIKE  mseg-rspos,        "预留行号

     END OF ls_reserve.
DATA:lt_reserve LIKE TABLE OF ls_reserve.
DATA:lt_mara LIKE TABLE OF mara WITH HEADER LINE.

DATA:BEGIN OF ls_invoice,
       vbeln LIKE likp-vbeln,
       posnr LIKE lips-posnr,
     END OF ls_invoice.
DATA:lt_invoice LIKE TABLE OF ls_invoice.
DATA:lt_likp LIKE TABLE OF likp WITH HEADER LINE.
DATA:lt_lips LIKE TABLE OF lips WITH HEADER LINE.

DATA:BEGIN OF ls_lips_vbap,
       vbeln   LIKE lips-vbeln,
       posnr   LIKE lips-posnr,
       vbeln_o LIKE vbap-vbeln,
       posnr_o LIKE vbap-posnr,
       posex   LIKE vbap-posex,
     END OF ls_lips_vbap.
DATA:lt_lips_vbap LIKE TABLE OF    ls_lips_vbap WITH HEADER LINE.

"客户
DATA:lt_kna1 LIKE TABLE OF kna1 WITH HEADER LINE.
DATA:BEGIN OF ls_kunnr ,
       kunnr LIKE kna1-kunnr,
     END OF ls_kunnr.
DATA:lt_kunnr LIKE TABLE OF ls_kunnr .

"供应商
DATA:lt_lfa1 LIKE TABLE OF lfa1 WITH HEADER LINE.
DATA:BEGIN OF ls_lifnr ,
       lifnr LIKE lfa1-lifnr,
     END OF ls_lifnr.
DATA:lt_lifnr LIKE TABLE OF ls_lifnr .


DATA:BEGIN OF ls_type,
       type(10) TYPE c,
       bwart    LIKE  mseg-bwart,
     END OF ls_type.
DATA:lt_type1 LIKE TABLE OF ls_type WITH HEADER LINE.
DATA:lt_type2 LIKE TABLE OF ls_type WITH HEADER LINE.

DATA:lt_t156t LIKE TABLE OF t156t WITH HEADER LINE.
DATA:lt_t001w LIKE TABLE OF t001w WITH HEADER LINE.

DATA:BEGIN OF ls_kostl,
       kostl LIKE cskt-kostl,
     END OF ls_kostl.
DATA:lt_kostl LIKE TABLE OF ls_kostl WITH HEADER LINE.
DATA:lt_cskt LIKE TABLE OF cskt WITH HEADER LINE.

*采购订单类型表
DATA:lt_t161t LIKE TABLE OF t161t WITH HEADER LINE.

*&---------------------------------------------------------------------*
*& ALV相关定义
*&---------------------------------------------------------------------*
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      gs_layout   TYPE slis_layout_alv,
      gv_repid    LIKE sy-repid VALUE sy-repid,
      gv_index    TYPE i,
      gv_title    TYPE char100.
DATA gv_open.

*&---------------------------------------------------------------------*
*& 宏定义
*&---------------------------------------------------------------------*
DEFINE fill_fieldcat.
  CLEAR:gt_fieldcat.
  gt_fieldcat-fieldname = &1.
  "gt_fieldcat-outputlen = &2.
  gt_fieldcat-seltext_l = &2.
  "gt_fieldcat-icon     = 'X'.      "图标功能
  IF &1 = 'MATNR'.
    gt_fieldcat-no_zero = 'X'.
  ENDIF.
  IF &1 = 'MBLNR'.
    gt_fieldcat-hotspot = 'X'.
  ENDIF.


  APPEND gt_fieldcat.
END-OF-DEFINITION.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS s_mblnr FOR mseg-mblnr.
SELECT-OPTIONS s_kostl FOR mseg-kostl.
SELECT-OPTIONS s_werks FOR mseg-werks OBLIGATORY.
SELECT-OPTIONS s_lgort FOR mseg-lgort.
SELECT-OPTIONS s_bwart FOR mseg-bwart OBLIGATORY.
SELECT-OPTIONS s_budat FOR mkpf-budat OBLIGATORY.
SELECT-OPTIONS s_usnam FOR mkpf-usnam.

SELECTION-SCREEN END OF BLOCK blk1.


START-OF-SELECTION.
  PERFORM frm_check.

  PERFORM frm_get_data.

  PERFORM frm_show_data.

*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_data .

  CLEAR:ls_type.
  ls_type-type = '采购入库单'.
  ls_type-bwart = '101'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '采购入库单'.
  ls_type-bwart = '103'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '采购入库单'.
  ls_type-bwart = '105'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '采购入库单'.
  ls_type-bwart = '511'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '采购入库单'.
  ls_type-bwart = '162'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '采购入库单'.
  ls_type-bwart = 'Z08'.
  APPEND ls_type TO lt_type1.

  "2019-04-22 by sw 增加打印类型

  CLEAR:ls_type.
  ls_type-type = '采购入库单'.
  ls_type-bwart = 'Z15'.
  APPEND ls_type TO lt_type1.

  CLEAR:ls_type.
  ls_type-type = '采购退货单'.
  ls_type-bwart = '104'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '采购退货单'.
  ls_type-bwart = '106'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '采购退货单'.
  ls_type-bwart = 'Z16'.
  APPEND ls_type TO lt_type1.



  CLEAR:ls_type.
  ls_type-type = '物料领用单'.
  ls_type-bwart = '344'.
  APPEND ls_type TO lt_type1.

  CLEAR:ls_type.
  ls_type-type = '物料退回单'.
  ls_type-bwart = '343'.
  APPEND ls_type TO lt_type1.
  "2019-04-22 by sw




  CLEAR:ls_type.
  ls_type-type = '物料领用单'.
  ls_type-bwart = '201'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '物料领用单'.
  ls_type-bwart = 'Z03'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '物料领用单'.
  ls_type-bwart = 'Z07'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '物料领用单'.
  ls_type-bwart = '221'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '物料领用单'.
  ls_type-bwart = '241'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '物料领用单'.
  ls_type-bwart = 'Z11'.
  APPEND ls_type TO lt_type1.

*  CLEAR:ls_type.
*  ls_type-type = '物料移库单'.
*  ls_type-bwart = '301'.
*  APPEND ls_type TO lt_type1.
*  CLEAR:ls_type.
*  ls_type-type = '物料移库单'.
*  ls_type-bwart = '302'.
*  APPEND ls_type TO lt_type1.
*  CLEAR:ls_type.
*  ls_type-type = '物料移库单'.
*  ls_type-bwart = '311'.
*  APPEND ls_type TO lt_type1.
*  CLEAR:ls_type.
*  ls_type-type = '物料移库单'.
*  ls_type-bwart = '312'.
*  APPEND ls_type TO lt_type1.


  CLEAR:ls_type.
  ls_type-type = '物料退回单'.
  ls_type-bwart = '202'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '物料退回单'.
  ls_type-bwart = 'Z04'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '物料退回单'.
  ls_type-bwart = '222'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '物料退回单'.
  ls_type-bwart = 'Z12'.
  APPEND ls_type TO lt_type1.

  CLEAR:ls_type.
  ls_type-type = '采购退货单'.
  ls_type-bwart = '122'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '采购退货单'.
  ls_type-bwart = '161'.
  APPEND ls_type TO lt_type1.
  CLEAR:ls_type.
  ls_type-type = '采购退货单'.
  ls_type-bwart = '102'.
  APPEND ls_type TO lt_type1.


*  CLEAR:ls_type.
*  ls_type-type = '销售出库单'.
*  ls_type-bwart = '645'.
*  APPEND ls_type TO lt_type1.
*  CLEAR:ls_type.
*  ls_type-type = '销售出库单'.
*  ls_type-bwart = '676'.
*  APPEND ls_type TO lt_type1.
*  CLEAR:ls_type.
*  ls_type-type = '销售出库单'.
*  ls_type-bwart = '601'.
*  APPEND ls_type TO lt_type1.
*  CLEAR:ls_type.
*  ls_type-type = '销售出库单'.
*  ls_type-bwart = '251'.
*  APPEND ls_type TO lt_type1.
*  CLEAR:ls_type.
*  ls_type-type = '销售出库单'.
*  ls_type-bwart = '654'.
*  APPEND ls_type TO lt_type1.
*  CLEAR:ls_type.
*  ls_type-type = '销售出库单'.
*  ls_type-bwart = 'Z05'.
*  APPEND ls_type TO lt_type1.



*  CLEAR:ls_type.
*  ls_type-type = '销售入库单'.
*  ls_type-bwart = '646'.
*  APPEND ls_type TO lt_type1.
*  CLEAR:ls_type.
*  ls_type-type = '销售入库单'.
*  ls_type-bwart = '675'.
*  APPEND ls_type TO lt_type1.
*  CLEAR:ls_type.
*  ls_type-type = '销售入库单'.
*  ls_type-bwart = '602'.
*  APPEND ls_type TO lt_type1.
*  CLEAR:ls_type.
*  ls_type-type = '销售入库单'.
*  ls_type-bwart = '252'.
*  APPEND ls_type TO lt_type1.
*  CLEAR:ls_type.
*  ls_type-type = '销售入库单'.
*  ls_type-bwart = '653'.
*  APPEND ls_type TO lt_type1.
*  CLEAR:ls_type.
*  ls_type-type = '销售入库单'.
*  ls_type-bwart = 'Z06'.
*  APPEND ls_type TO lt_type1.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_t161t
    FROM t161t
    WHERE spras = 1 AND bstyp = 'F'.

  SELECT  mseg~mblnr
          mseg~mjahr
          mseg~zeile
          mseg~matnr
          mseg~vbeln_im
          mseg~vbelp_im
          mseg~ebeln
          mseg~ebelp
          mseg~rsnum
          mseg~rspos
          mseg~xauto
          mseg~werks
          mseg~meins
          mseg~lgort
          mseg~anln1
          mseg~umwrk
          mseg~umlgo
          mseg~charg
          mseg~sgtxt AS sgtxt_h
          mseg~lifnr "供应商
          mseg~kunnr "客户
          mseg~bwart
          mkpf~budat
          mseg~kostl
          mseg~ps_psp_pnr
          mseg~menge
*          mseg~bpmng AS menge_po
          mkpf~bktxt
          mkpf~usnam
  INTO CORRESPONDING FIELDS OF TABLE lt_tab
    FROM mseg
    INNER JOIN mkpf ON mseg~mblnr = mkpf~mblnr AND mseg~mjahr = mkpf~mjahr
    WHERE mseg~mblnr IN s_mblnr AND
          mseg~kostl IN s_kostl AND
          mseg~werks IN s_werks AND
          mseg~lgort IN s_lgort AND
          mseg~bwart IN s_bwart AND
          mkpf~budat IN s_budat AND
          mkpf~usnam IN s_usnam.
  DELETE  lt_tab WHERE xauto = 'X' AND ( bwart = '301' OR bwart = '302' OR bwart = '311' OR bwart = '312' ) .
  DELETE lt_tab WHERE matnr IS INITIAL.
  LOOP AT lt_tab INTO ls_tab.
    IF ls_tab-ebeln IS NOT INITIAL.
      ls_purchase-ebeln = ls_tab-ebeln.
      ls_purchase-ebelp = ls_tab-ebelp.
      APPEND ls_purchase TO lt_purchase.
    ENDIF.
    IF ls_tab-rsnum IS NOT INITIAL.
      ls_reserve-rsnum = ls_tab-rsnum.
      ls_reserve-rspos = ls_tab-rspos.
      APPEND ls_reserve TO lt_reserve.
    ENDIF.
    IF ls_tab-vbeln_im IS NOT INITIAL.
      ls_invoice-vbeln = ls_tab-vbeln_im.
      ls_invoice-posnr = ls_tab-vbelp_im.
      APPEND ls_invoice TO lt_invoice.
    ENDIF.
    IF ls_tab-kostl IS NOT INITIAL.
      ls_kostl = ls_tab-kostl.
      APPEND ls_kostl TO lt_kostl.
    ENDIF.
    CLEAR :ls_purchase,ls_reserve,ls_invoice.
  ENDLOOP.


  IF lt_purchase[] IS NOT INITIAL.
    "去采购订单
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_ekko
      FROM ekko
      FOR ALL ENTRIES IN lt_purchase
      WHERE ekko~ebeln = lt_purchase-ebeln.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_ekpo
      FROM ekpo
      FOR ALL ENTRIES IN lt_purchase
      WHERE ekpo~ebeln = lt_purchase-ebeln AND ekpo~ebelp = lt_purchase-ebelp.

  ENDIF.
  IF lt_reserve[] IS NOT INITIAL.
    "取预留
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_resb
      FROM resb
      FOR ALL ENTRIES IN lt_reserve
      WHERE resb~rsnum = lt_reserve-rsnum AND resb~rspos = lt_reserve-rspos.
  ENDIF.
  IF lt_tab[] IS NOT INITIAL.
    "取物料
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_mara
      FROM mara
      FOR ALL ENTRIES IN lt_tab
      WHERE matnr = lt_tab-matnr.

  ENDIF.
  IF lt_invoice[] IS NOT INITIAL.

    SELECT lips~vbeln
             lips~posnr
             vbap~vbeln AS vbeln_o
             vbap~posnr AS posnr_o
             vbap~posex


        INTO CORRESPONDING FIELDS OF TABLE lt_lips
        FROM lips
        LEFT JOIN vbap ON lips~vgbel = vbap~vbeln AND lips~vgpos = vbap~posnr
         FOR ALL ENTRIES IN lt_invoice
        WHERE lips~vbeln = lt_invoice-vbeln AND lips~posnr = lt_invoice-posnr.

    "取交货单
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_likp
      FROM likp
      FOR ALL ENTRIES IN lt_invoice
      WHERE vbeln = lt_invoice-vbeln.
    LOOP AT lt_likp.
      IF lt_likp-kunnr IS NOT INITIAL.
        ls_kunnr-kunnr = lt_likp-kunnr.
        APPEND ls_kunnr TO lt_kunnr.
      ENDIF.
      IF lt_likp-lifnr IS NOT INITIAL.
        ls_lifnr-lifnr = lt_likp-lifnr.
        APPEND ls_lifnr TO lt_lifnr.
      ENDIF.

    ENDLOOP.
  ENDIF.

  LOOP AT lt_tab INTO ls_tab.
    IF ls_tab-lifnr IS NOT INITIAL.
      ls_lifnr-lifnr = ls_tab-lifnr.
      APPEND ls_lifnr TO lt_lifnr.
    ENDIF.
    IF ls_tab-kunnr IS NOT INITIAL.
      ls_kunnr-kunnr = ls_tab-kunnr.
      APPEND ls_kunnr TO lt_kunnr.
    ENDIF.
  ENDLOOP.
  IF lt_lifnr[] IS NOT INITIAL.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_lfa1
      FROM lfa1
      FOR ALL ENTRIES IN lt_lifnr
      WHERE lifnr = lt_lifnr-lifnr.

  ENDIF.

  IF lt_tab[] IS NOT INITIAL.
    "取物料描述
    SELECT matnr,
           maktx
    INTO TABLE @DATA(lt_makt)
    FROM makt FOR ALL ENTRIES IN @lt_tab
    WHERE matnr = @lt_tab-matnr
      AND spras = @sy-langu.
  ENDIF.
  IF lt_kunnr[] IS NOT INITIAL.
    "取客户
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_kna1
      FROM kna1
      FOR ALL ENTRIES IN lt_kunnr
      WHERE kunnr = lt_kunnr-kunnr.

  ENDIF.
  IF lt_kostl[] IS NOT INITIAL.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_cskt
      FROM cskt
      FOR ALL ENTRIES IN lt_kostl
      WHERE spras = '1' AND kostl = lt_kostl-kostl.

  ENDIF.
  LOOP AT lt_tab INTO ls_tab.


*    REFRESH:lt_tline.
*    CLEAR:lv_name.
*
*
*    lv_name = ls_tab-matnr.
*
*
*    CALL FUNCTION 'READ_TEXT'
*      EXPORTING
**       CLIENT                  = SY-MANDT
*        id                      = 'GRUN'
*        language                = '1'
*        name                    = lv_name
*        object                  = 'MATERIAL'
*      TABLES
*        lines                   = lt_tline
*      EXCEPTIONS
*        id                      = 1
*        language                = 2
*        name                    = 3
*        not_found               = 4
*        object                  = 5
*        reference_check         = 6
*        wrong_access_to_archive = 7
*        OTHERS                  = 8.
*    IF sy-subrc = 0.
*      LOOP AT lt_tline.
*        CONCATENATE ls_tab-sgtxt lt_tline-tdline INTO ls_tab-sgtxt.
*      ENDLOOP.
*    ENDIF.
    READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = ls_tab-matnr.
    IF sy-subrc = 0.
      ls_tab-sgtxt = ls_makt-maktx.
    ENDIF.

    READ TABLE lt_ekpo WITH KEY ebeln = ls_tab-ebeln  ebelp = ls_tab-ebelp.
    IF sy-subrc = 0.
      ls_tab-menge_po = lt_ekpo-menge.
    ENDIF.
    READ TABLE lt_resb WITH KEY rsnum = ls_tab-rsnum rspos = ls_tab-rspos.
    IF sy-subrc = 0.
      ls_tab-bdmng = lt_resb-bdmng.
    ENDIF.
    READ TABLE lt_ekko WITH KEY ebeln = ls_tab-ebeln.
    IF sy-subrc = 0.
      ls_tab-ekgrp = lt_ekko-ekgrp.
*      ls_tab-zwbhtid = lt_ekko-zwbhtid.
      ls_tab-bsart = lt_ekko-bsart.
    ENDIF.

    READ TABLE lt_t161t WITH KEY bsart = ls_tab-bsart.
    IF sy-subrc = 0.
      ls_tab-batxt = lt_t161t-batxt.
    ENDIF.

    READ TABLE lt_likp WITH KEY vbeln = ls_tab-vbeln_im.
    IF sy-subrc = 0.
      IF lt_likp-lifnr IS NOT INITIAL.
        ls_tab-lifnr = lt_likp-lifnr.
      ENDIF.
      IF lt_likp-kunnr IS NOT INITIAL.
        ls_tab-kunnr = lt_likp-kunnr.
      ENDIF.
    ENDIF.

    READ TABLE lt_lfa1 WITH KEY lifnr = ls_tab-lifnr.
    IF sy-subrc = 0.
      ls_tab-name1 = lt_lfa1-name1.
    ENDIF.

    READ TABLE lt_kna1 WITH KEY kunnr = ls_tab-kunnr.
    IF sy-subrc = 0.
      ls_tab-name2 = lt_kna1-name1.
    ENDIF.


    READ TABLE lt_mara WITH KEY matnr = ls_tab-matnr.
    IF sy-subrc = 0.
      ls_tab-matkl = lt_mara-matkl.

    ENDIF.
    READ TABLE lt_cskt WITH KEY kostl = ls_tab-kostl.
    IF sy-subrc = 0.
      ls_tab-ktext = lt_cskt-ktext.
    ENDIF.
    "取箱数
    READ TABLE lt_lips_vbap WITH KEY vbeln = ls_tab-vbeln_im posnr = ls_tab-vbelp_im.
    IF sy-subrc = 0.
      ls_tab-posex = lt_lips_vbap-posex.
    ENDIF.
    IF ls_tab-charg IS NOT INITIAL.
      CLEAR:e_objek, e_obtab, e_klart, e_class ,e_objek1.
      REFRESH: it_num ,it_char, it_curr ,bapi_return.
      CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
        EXPORTING
          i_matnr = ls_tab-matnr
          i_charg = ls_tab-charg
          i_werks = ls_tab-werks
        IMPORTING
          e_objek = e_objek
          e_obtab = e_obtab
          e_klart = e_klart
          e_class = e_class.
      IF sy-subrc = 0.
        CONDENSE e_objek NO-GAPS.
        e_objek1 = e_objek.
        "获取特性值
        CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
          EXPORTING
            objectkey       = e_objek1
            objecttable     = e_obtab
            classnum        = e_class
            classtype       = e_klart
          TABLES
            allocvaluesnum  = it_num
            allocvalueschar = it_char
            allocvaluescurr = it_curr
            return          = bapi_return.
        IF sy-subrc = 0.
          READ TABLE bapi_return WITH KEY type = 'S'.
          IF sy-subrc = 0.
            LOOP AT it_char.
              CASE it_char-charact.
                WHEN 'Z_DATA01'.
                  ls_tab-qfh =  it_char-value_neutral . "区分号
                WHEN 'Z_DATA02'.
                  ls_tab-dj =  it_char-value_neutral .   "等级
                WHEN 'Z_DATA05'.
                  ls_tab-ph =  it_char-value_neutral .  "批号
              ENDCASE.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY lt_tab FROM ls_tab.
  ENDLOOP.
  SORT lt_tab BY mblnr zeile.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SHOW_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_show_data .
  PERFORM frm_build_catalog.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_buffer_active          = 'X'
      i_callback_program       = gv_repid
      i_callback_pf_status_set = 'FRM_SET_PF_STATUS'
      i_callback_user_command  = 'FRM_ALV_USER_COMMAND'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat[]
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
*     IT_SORT                  =
*     IT_FILTER                =
*     i_default                = 'X'
*     i_save                   = 'A'
    TABLES
      t_outtab                 = lt_tab.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  .
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_build_catalog .

  REFRESH gt_fieldcat.

  fill_fieldcat 'CHK'         '选择'.
  fill_fieldcat 'MBLNR'       '物料凭证'.
  fill_fieldcat 'ZEILE'       '物料凭证行号'.
  fill_fieldcat 'MATNR'       '物料号'.
  fill_fieldcat 'SGTXT'       '物料描述'.
  fill_fieldcat 'VBELN_IM'    '交货单'.
  fill_fieldcat 'VBELP_IM'    '交货单行号'.
  fill_fieldcat 'EBELN'       '采购订单'.
  fill_fieldcat 'EBELP'       '采购订单行号'.
  fill_fieldcat 'BSART'       '采购类型'.
  fill_fieldcat 'BATXT'       '采购类型描述'.
  fill_fieldcat 'RSNUM'       '预留单'.
  fill_fieldcat 'RSPOS'       '预留行号'.
  fill_fieldcat 'WERKS'       '工厂'.
  fill_fieldcat 'LGORT'       '库存地点'.
  fill_fieldcat 'UMWRK'       '接收工厂'.
  fill_fieldcat 'UMLGO'       '接收库存地点'.
  fill_fieldcat 'CHARG'       '批次'.
  fill_fieldcat 'ANLN1'       '资产'.
  fill_fieldcat 'SGTXT_H'       '行文本'.
  fill_fieldcat 'NAME1'       '供应商'.
  fill_fieldcat 'NAME2'       '客户'.
  fill_fieldcat 'BWART'       '移动类型'.
  fill_fieldcat 'BUDAT'       '过账日期'.
  fill_fieldcat 'KOSTL'       '成本中心'.
  fill_fieldcat 'PS_PSP_PNR'  'wbs'.
  fill_fieldcat 'MENGE'       '数量'.
  fill_fieldcat 'MENGE_PO'    '采购订单数量'.
  fill_fieldcat 'BDMNG'       '预留申请数量'.
  fill_fieldcat 'BKTXT'       '制单人'.
  fill_fieldcat 'EKGRP'       '采购组'.
  fill_fieldcat 'USNAM'       '用户名'.
  fill_fieldcat 'ZWBHTID'     '合同'.
  fill_fieldcat 'MATKL'       '物料组'.
  fill_fieldcat 'MEINS'       '单位'.
*  fill_fieldcat 'lfsnr'       ''.
*  fill_fieldcat 'NAME1'       '客户'.
  fill_fieldcat 'PH'          '批号'.
  fill_fieldcat 'QFH'         '区分号'.
  fill_fieldcat 'DJ'          '等级'.



  gs_layout-colwidth_optimize = 'X'.

  READ TABLE gt_fieldcat  WITH KEY fieldname = 'CHK'.
  IF sy-subrc = 0.
    gv_index = sy-tabix.
    gt_fieldcat-edit     = 'X'.
    gt_fieldcat-checkbox = 'X'.
    MODIFY gt_fieldcat INDEX gv_index.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM frm_alv_user_command                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PV_UCOMM                                                      *
*  -->  PS_SELFIELD                                                   *
*---------------------------------------------------------------------*
FORM frm_alv_user_command USING pv_ucomm LIKE sy-ucomm
      ps_selfield TYPE slis_selfield.

  DATA:lv_ooalv   TYPE REF TO cl_gui_alv_grid.
  DATA : et_filtered TYPE lvc_t_fidx.
  DATA:es_filtered LIKE LINE OF et_filtered.

  CLEAR : et_filtered[],et_filtered.
  "获取屏幕选择行以外的全部行号到内表et_filtered.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lv_ooalv.

  CALL METHOD lv_ooalv->check_changed_data.
  CALL METHOD lv_ooalv->get_filtered_entries
    IMPORTING
      et_filtered_entries = et_filtered.

  IF gv_all = 'X'.
    IF et_filtered IS NOT INITIAL.
      LOOP AT lt_tab INTO ls_tab.
        READ TABLE et_filtered INTO es_filtered WITH KEY table_line = sy-tabix."判断显示内表行号是否在et_filtered中
        IF sy-subrc <> 0.
          ls_tab-chk = 'X'.
*              MODIFY lt_tab FROM ls_tab.
        ELSE.
          ls_tab-chk = ''.
        ENDIF.

        MODIFY lt_tab FROM ls_tab .
      ENDLOOP.
    ELSE.
      ls_tab-chk = 'X'.
      MODIFY lt_tab FROM ls_tab TRANSPORTING chk WHERE chk = ''.

    ENDIF.

  ENDIF.


  CASE pv_ucomm.
    WHEN 'ALL'.

      IF gv_all = ''.
        gv_all = 'X'.
        IF et_filtered IS NOT INITIAL.
          LOOP AT lt_tab INTO ls_tab.
            READ TABLE et_filtered INTO es_filtered WITH KEY table_line = sy-tabix."判断显示内表行号是否在et_filtered中
            IF sy-subrc <> 0.
              ls_tab-chk = 'X'.
*              MODIFY lt_tab FROM ls_tab.
            ELSE.
              ls_tab-chk = ''.
            ENDIF.

            MODIFY lt_tab FROM ls_tab .
          ENDLOOP.
        ELSE.
          ls_tab-chk = 'X'.
          MODIFY lt_tab FROM ls_tab TRANSPORTING chk WHERE chk = ''.

        ENDIF.

      ELSE.
        gv_all = ''.
        ls_tab-chk = ''.
        MODIFY lt_tab FROM ls_tab TRANSPORTING chk WHERE chk = 'X'.
      ENDIF.

  ENDCASE.





  CASE pv_ucomm.
    WHEN '&IC1'.
      IF ps_selfield-fieldname = 'MBLNR'.

        READ TABLE lt_tab INTO ls_tab INDEX  ps_selfield-tabindex . "双击所在行
        IF sy-subrc = 0.

          PERFORM frm_migo_dialog USING ls_tab-mblnr ls_tab-budat+0(4) .
        ENDIF.


      ENDIF.

    WHEN 'CHOOSEALL'.
      DATA:is_tab LIKE ls_tab.

      READ TABLE lt_tab INTO ls_tab WITH KEY chk = 'X'.
      IF sy-subrc <> 0.
        MESSAGE '请选择打印数据' TYPE 'E'.
      ENDIF.

      LOOP AT lt_tab INTO is_tab WHERE mblnr = ls_tab-mblnr  AND mjahr = ls_tab-mjahr AND bwart = ls_tab-bwart.
        is_tab-chk = 'X'.
        MODIFY lt_tab FROM is_tab.
      ENDLOOP.
    WHEN 'PRINT'.
      DATA:num1 TYPE i.
      REFRESH: gt_tab,lt_type2,print_head,print_body.
      READ TABLE lt_tab INTO ls_tab WITH KEY chk = 'X'.
      IF sy-subrc <> 0.
        MESSAGE '请选择打印数据' TYPE 'E'.
      ENDIF.
      LOOP AT lt_tab INTO ls_tab WHERE chk = 'X'.
        APPEND ls_tab TO gt_tab.
      ENDLOOP.
      LOOP AT gt_tab.
        READ TABLE lt_type1 WITH KEY bwart = gt_tab-bwart.
        IF sy-subrc = 0.
          CLEAR:ls_type.
          IF lt_type1-type = '销售入库单' OR  lt_type1-type = '销售出库单'.
            ls_type-type = '销售出/入库单'.
          ELSE.
            ls_type-type = lt_type1-type.

          ENDIF.
          APPEND ls_type TO lt_type2.
        ENDIF.

      ENDLOOP.
      SORT lt_type2 BY type.
      DELETE ADJACENT DUPLICATES FROM lt_type2 COMPARING type.
      num1 = lines( lt_type2 ).
      IF num1 > 1.
        MESSAGE '请选择同一类型的凭证打印！！！！！' TYPE 'E' DISPLAY LIKE 'I'.
      ENDIF.
      IF num1 = 0.
        MESSAGE '请选择能够打印的凭证进行打印！！！！！' TYPE 'E' DISPLAY LIKE 'I'.
      ENDIF.
      IF gt_tab[] IS NOT INITIAL.
        MOVE-CORRESPONDING gt_tab[] TO print_head[].
        SORT print_head BY mblnr mjahr vbeln_im.
        DELETE ADJACENT DUPLICATES FROM print_head COMPARING mblnr mjahr vbeln_im.
        LOOP AT print_head.
          READ TABLE gt_tab WITH KEY mblnr = print_head-mblnr mjahr = print_head-mjahr vbeln_im = print_head-vbeln_im.
          IF sy-subrc = 0.
            print_head-usnam = gt_tab-usnam.
          ENDIF.
          MODIFY print_head.
        ENDLOOP.



        READ TABLE lt_type2.
        IF sy-subrc = 0.
          LOOP AT print_head.
            print_head-title = lt_type2-type.
            MODIFY print_head.
          ENDLOOP.
        ENDIF.
        MOVE-CORRESPONDING gt_tab[] TO print_body[].
        PERFORM frm_print_data TABLES print_head print_body .
      ENDIF.



  ENDCASE.

  ps_selfield-refresh = 'X'.
  ps_selfield-col_stable = 'X'.
  ps_selfield-row_stable = 'X'.


ENDFORM.

*---------------------------------------------------------------------*
*       FORM frm_set_pf_status                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RT_EXTAB                                                      *
*---------------------------------------------------------------------*
FORM frm_set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_PRINT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PRINT_HEAD
*&      --> PRINT_BODY
*&---------------------------------------------------------------------*
FORM frm_print_data  TABLES   lt_head STRUCTURE zsmm_print_001
                              lt_body STRUCTURE zsmm_print_b_001.

  DATA:ls_head TYPE  zsmm_print_001.
  DATA:ls_ztabix TYPE syst_tabix.
  DATA:gt_body LIKE TABLE OF zsmm_print_b_001 WITH HEADER LINE.
  DATA: lv_func_module_name   TYPE rs38l_fnam,
        ls_control_parameters TYPE ssfctrlop,
        ls_job_output_info    TYPE ssfcrescl,
        ls_ssfcrespd          TYPE ssfcrespd,
        lv_index              TYPE i,
        lv_sformname          TYPE tdsfname.
  DATA:output_options TYPE ssfcompop.

  READ TABLE lt_head INDEX 1.
  IF sy-subrc = 0.
    READ TABLE lt_type1 WITH KEY bwart = lt_head-bwart.
    IF sy-subrc = 0.
      IF lt_type1-type = '采购入库单'.
        lv_sformname = 'ZSF_MM_PURCHASEORDER_PRINT'.
      ENDIF.
      IF lt_type1-type = '采购退货单'.
        lv_sformname = 'ZSF_MM_PURCHASEORDER_PRINT1'.
      ENDIF.
      IF lt_type1-type = '物料领用单'.
        lv_sformname = 'ZSF_MM_PURCHASEORDER_PRINT2'.
      ENDIF.
      IF lt_type1-type = '物料退回单'.
        lv_sformname = 'ZSF_MM_PURCHASEORDER_PRINT3'.
      ENDIF.
*      IF lt_type1-type = '销售入库单'.
*        lv_sformname = 'ZSF_MM_PURCHASEORDER_PRINT4'.
*      ENDIF.
*      IF lt_type1-type = '销售出库单'.
*        lv_sformname = 'ZSF_MM_PURCHASEORDER_PRINT4'.
*      ENDIF.
*      IF lt_type1-type = '物料移库单'.
*        lv_sformname = 'ZSF_MM_PURCHASEORDER_PRINT5'.
*      ENDIF.
    ENDIF.
*    loop at lt_head.
*      lt_head-title = lt_type1-type.
*      MODIFY lt_head.
*    ENDLOOP.
  ENDIF.
*  IF lt_type1-type = '销售入库单' OR lt_type1-type = '销售出库单'.
*    LOOP AT lt_head.
*      lt_head-title = lt_type1-type.
*      MODIFY lt_head.
*    ENDLOOP.
*
*  ENDIF.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lv_sformname
    IMPORTING
      fm_name            = lv_func_module_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  output_options-tdimmed = 'X'.
  output_options-tdnewid = 'X'.
  output_options-tddelete = 'X'.
  output_options-tdfinal = 'X'.
  output_options-tdiexit = 'X'.
  output_options-tddest = 'LP01'.

  ls_control_parameters-preview = 'X'.
  ls_control_parameters-no_open = 'X'.
  ls_control_parameters-no_close = 'X'.
  "CONTROL_PARAMETERS-NO_DIALOG = 'X'.
  "CONTROL_PARAMETERS-DEVICE = 'PRINTER'.


  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = ls_control_parameters
      output_options     = output_options
      user_settings      = ''
*    IMPORTING
*     job_output_options = outopt
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc = 0.
    gv_open = 'X'.
  ENDIF.
*  ENDIF.



  LOOP AT lt_head.
    MOVE-CORRESPONDING lt_head TO ls_head.
    REFRESH gt_body[].
    LOOP AT lt_body WHERE mblnr = lt_head-mblnr AND mjahr = lt_head-mjahr.
      APPEND lt_body TO gt_body.
    ENDLOOP.
    CLEAR:ls_ztabix.
    LOOP AT gt_body.
      ls_ztabix = ls_ztabix + 1.
      gt_body-ztabix = ls_ztabix.
      MODIFY gt_body.
    ENDLOOP.
    CALL FUNCTION lv_func_module_name
      EXPORTING
        control_parameters = ls_control_parameters
*       ARCHIVE_INDEX      =
*       ARCHIVE_INDEX_TAB  =
*       ARCHIVE_PARAMETERS =
*       CONTROL_PARAMETERS =
*       MAIL_APPL_OBJ      =
*       MAIL_RECIPIENT     =
*       MAIL_SENDER        =
*       OUTPUT_OPTIONS     =
*       user_settings      = 'X'
        i_header           = ls_head
* IMPORTING
*       DOCUMENT_OUTPUT_INFO       =
*       JOB_OUTPUT_INFO    =
*       JOB_OUTPUT_OPTIONS =
      TABLES
        i_item             = gt_body
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDLOOP.

***>Close Form
  CHECK NOT gv_open IS INITIAL.
  CALL FUNCTION 'SSF_CLOSE'
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    CLEAR gv_open.
  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_check .
  DATA:mes TYPE string.
*  SELECT *
*    INTO CORRESPONDING FIELDS OF TABLE lt_t156t
*    FROM t156t
*    WHERE bwart IN s_bwart.
*  LOOP AT lt_t156t.
*    AUTHORITY-CHECK OBJECT 'M_MRES_BWA'
*       ID 'ACTVT' FIELD '02'
*       ID 'BWART' FIELD lt_t156t-bwart.
*    IF sy-subrc <> 0.
*      CONCATENATE '无' lt_t156t-bwart '移动类型权限！！！' INTO mes.
*      MESSAGE mes TYPE 'E'.
*    ENDIF.
*  ENDLOOP.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_t001w
    FROM t001w
    WHERE werks IN s_werks.

  LOOP AT lt_t001w.


    AUTHORITY-CHECK OBJECT 'M_MSEG_WMB'
     ID 'ACTVT' DUMMY
     ID 'WERKS' FIELD lt_t001w-werks..
    IF sy-subrc <> 0.
      CONCATENATE '无' lt_t001w-werks '工厂权限！！！' INTO mes.
      MESSAGE mes TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDLOOP.

ENDFORM.

FORM frm_bp_dialog.


ENDFORM.


FORM frm_migo_dialog USING lv_mblnr TYPE mblnr
                           lv_mjahr TYPE mjahr.
*                           lv_zeile TYPE mblpo.
  DATA:lv_zeile TYPE mblpo.
  DATA: lv_action            TYPE  goaction,
        lv_refdoc            TYPE  refdoc,
        lv_notree            TYPE  char1,
        lv_no_auth_check     TYPE  char1,
        lv_skip_first_screen TYPE  char1,
        lv_deadend           TYPE  char1,
        lv_okcode            TYPE  okcode,
        lv_leave_after_post  TYPE  char1,
        lv_new_rollarea      TYPE  char1,
        lv_sytcode           TYPE  sytcode,
        lv_ebeln             TYPE  ebeln,
        lv_ebelp             TYPE  ebelp,
        lv_transport         TYPE  tknum,
        lv_order_number      TYPE  aufnr,
        lv_order_item        TYPE  co_posnr,
        lv_transport_means   TYPE  traty,
        lv_transportident    TYPE  traid,
        lv_inbound_deliv     TYPE  vbeln_vl,
        lv_outbound_deliv    TYPE  vbeln,
        lv_reservation_numb  TYPE  rsnum,
        lv_reservation_item  TYPE  rspos,
        lw_ext               TYPE  ext_migo_dialog,
        lv_move_type         TYPE  bwart,
        lv_spec_stock        TYPE  sobkz,
        lv_pstng_date        TYPE  budat,
        lv_doc_date          TYPE  bldat,
        lv_ref_doc_no        TYPE  xblnr,
        lv_header_txt        TYPE  bktxt.

  SET PARAMETER ID 'MBN' FIELD lv_mblnr.
  SET PARAMETER ID 'MJA' FIELD lv_mjahr.
*  SET PARAMETER ID 'POS' FIELD lv_zeile.

  lv_action                  = 'A04'.
  lv_refdoc                  = 'R02'.
  lv_notree                  = 'X'.
  lv_skip_first_screen       = 'X'.
  lv_deadend                 = 'X'.
  lv_okcode                  = 'OK_GO'.
  lv_new_rollarea            = 'X'.

  CALL FUNCTION 'MIGO_DIALOG'
    EXPORTING
      i_action            = lv_action
      i_refdoc            = lv_refdoc
      i_notree            = lv_notree
      i_no_auth_check     = lv_no_auth_check
      i_skip_first_screen = lv_skip_first_screen
      i_deadend           = lv_deadend
      i_okcode            = lv_okcode
      i_leave_after_post  = lv_leave_after_post
      i_new_rollarea      = lv_new_rollarea
      i_sytcode           = lv_sytcode
      i_ebeln             = lv_ebeln
      i_ebelp             = lv_ebelp
      i_mblnr             = lv_mblnr
      i_mjahr             = lv_mjahr
      i_zeile             = lv_zeile
      i_transport         = lv_transport
      i_order_number      = lv_order_number
      i_order_item        = lv_order_item
      i_transport_means   = lv_transport_means
      i_transportident    = lv_transportident
      i_inbound_deliv     = lv_inbound_deliv
      i_outbound_deliv    = lv_outbound_deliv
      i_reservation_numb  = lv_reservation_numb
      i_reservation_item  = lv_reservation_item
      ext                 = lw_ext
      i_move_type         = lv_move_type
      i_spec_stock        = lv_spec_stock
      i_pstng_date        = lv_pstng_date
      i_doc_date          = lv_doc_date
      i_ref_doc_no        = lv_ref_doc_no
      i_header_txt        = lv_header_txt
    EXCEPTIONS
      illegal_combination = 1
      OTHERS              = 2.

ENDFORM.


*Messages
*----------------------------------------------------------
*
* Message class: Hard coded
*   请选择打印数据

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
