*&---------------------------------------------------------------------*
*& Report ZJSRPT_004
*&---------------------------------------------------------------------*
*&事务代码/Transaction code     :
*&程序名称/Program Name         :ZJSRPT_004
*&程序描述/Program Des.         :金税平台
*&申请人/Applicant              :
*&申请日期/Date of App          :2019.9.18
*&开发单位/Development Company  :HAND(汉得)
*&作者/Author                   :ZXJ
*&---------------------------------------------------------------------*
*&摘要/Abstract:
*&    1).业务场景
*&       金税合并拆分
*&       金税数据再处理，运费分摊
*&       撤销合并拆分
*&       数据显示
*&---------------------------------------------------------------------*
*&变更记录/Change record
*&Date              Developer          ReqNo       Descriptions
*& ==========  ==================  ==========  ========================*
*&---------------------------------------------------------------------*
REPORT zjsrpt_004.

*----------------------------------------------------------------------*
* 数据库表声明
*----------------------------------------------------------------------*
TYPE-POOLS:slis.
TABLES:ztvat_data,ztvat_matnr,ztvat_rule,vbrk,ztvat_return,zsvat_yfcl_h,kna1.
*----------------------------------------------------------------------*
* 声明类型
*----------------------------------------------------------------------*

TYPES:BEGIN OF ty_data,
        box        TYPE c,
        zidnum     TYPE char15, "样票号
        vbeln      TYPE vbrk-vbeln, "sap发票号
        zls        TYPE i, "用于拆分临时存储变量
        ztax       TYPE ztvat_data-ztax, "税率
        zbklas     TYPE char2, "评估类前两个字符
        matnr      TYPE vbrp-matnr, "物料描述
        posnr      TYPE vbrp-posnr, "行项目

        zsuoyin    TYPE ztvat_data-zsuoyin,
        zposnr     TYPE ztvat_data-zposnr,
        zjszl      TYPE ztvat_data-zjszl, "发票种类
        zkpjh      TYPE ztvat_data-zkpjh, "开票机号
        bukrs      TYPE vbrk-bukrs, "公司代码
        fkdat      TYPE vbrk-fkdat, "发票日期
        fkart      TYPE vbrk-fkart, "发票类型
        vkorg      TYPE vbrk-vkorg, "销售组织
        vtweg      TYPE vbrk-vtweg, "分销渠道
        kunrg      TYPE vbrk-kunrg, "客户
        netwr_k    TYPE vbrk-netwr, "抬头净值
        zstatus    TYPE ztvat_data-zstatus,
        zkhbank    TYPE ztvat_data-zkhbank,
        zkhname    TYPE but000-name_org1, "客户名称
        stcd5      TYPE kna1-stcd5, "客户税号
        zkhsh      TYPE kna1-stcd5, "客户税号
        zaddress   TYPE char50, "地址电话
        zbank      TYPE char50, "客户开户行及账号
        kunnr      TYPE vbpa-kunnr, "销售员
        zsaler     TYPE kna1-kunnr, "销售员
        zwmfph     TYPE vbak-zwmfph, "外贸发票号
        bklas      TYPE mbew-bklas, "评估类
        zspmc      TYPE char50, "商品名称
        zggxh      TYPE char50, "规格型号
        atwrt      TYPE char50, "等级
        fkimg      TYPE vbrp-fkimg, "数量
        vrkme      TYPE vbrp-vrkme, "计量单位
        netwr      TYPE vbrp-netwr, "金额
        zbprice    TYPE vbrp-netwr, "不含税单价
        zprice     TYPE vbrp-netwr, "单价
        zmount     TYPE vbrp-netwr, "金额
        zbmount    TYPE vbrp-netwr, "本位币金额
        zse        TYPE vbrp-mwsbp, "税额
        waerk      TYPE vbrp-waerk, "币种
        zwaerk     TYPE vbrp-waerk, "本位币币种
        kursk      TYPE tcurr-ukurs, "汇率
        arktx      TYPE vbrp-arktx,
        charg      TYPE vbrp-charg, "批次
*        ZTAX     TYPE ZTVAT_DATA-ZTAX,"税率
        mwsbp      TYPE vbrp-mwsbp,
        werks      TYPE vbrp-werks,
        umvkz      TYPE vbrp-umvkz,
        umvkn      TYPE vbrp-umvkn,
        zbeizhu    TYPE ztvat_data-zbeizhu, "备注
        zssflbm    TYPE ztvat_matnr-zssflbm, "税收分类编码
        kunnr_z1   TYPE vbpa-kunnr,
        bstkd      TYPE vbkd-bstkd,
        bezei      TYPE tvsakt-bezei, "贸易方式
        zzss       TYPE char1, "再生丝标识
        zthcd      TYPE char1, "退货冲抵标识
        ztxbs      TYPE char1, "贴息款合并标识
        ernam      TYPE vbrp-ernam, "
        zjsdm      TYPE ztvat_invoice-zjsdm,
        zjshm      TYPE ztvat_invoice-zjsdm,
        zjsdate    TYPE ztvat_invoice-zjsdate,
        zpost      TYPE ztvat_invoice-zpost,
        zernam     TYPE ztvat_invoice-zernam,
        zerdat     TYPE ztvat_invoice-zerdat,
        zertim     TYPE ztvat_invoice-zertim,
        zaenam     TYPE ztvat_invoice-zaenam,
        zaedat     TYPE ztvat_invoice-zaedat,
        zaetim     TYPE ztvat_invoice-zaetim,
        ztsgz      TYPE char20,       "特殊规则
        tx_flag    TYPE abap_bool,
*        ZERNAM    TYPE VBRP-ERNAM, "
*        ZERDAT    TYPE VBRP-ERDAT, "
*        ZERTIM    TYPE ZTVAT_DATA-ZERTIM, "
        knumv      TYPE vbrk-knumv,
        aubel      TYPE vbrp-aubel,
        rfbsk      TYPE vbrk-rfbsk,
        dd_handle  TYPE int4,
        dd_kpjh    TYPE int4,
        vbeln_line TYPE i,
        msg        TYPE bapi_msg,
        light      TYPE char4,
*        SEL      TYPE C,
      END OF ty_data.


TYPES:BEGIN OF ty_head,
        vbeln TYPE vbrk-vbeln,
        kunrg TYPE vbrk-kunrg,
        bukrs TYPE vbrk-bukrs,
        netwr TYPE vbrk-netwr,
        fkimg TYPE vbrp-fkimg,
      END OF ty_head.


*&---------------------------------------------------------------------*
*& Field-symbols/字段串定义                                            *
*&---------------------------------------------------------------------*
FIELD-SYMBOLS: <dyn_table> TYPE table.



*&---------------------------------------------------------------------*
*& globle/全局变量声明
*&---------------------------------------------------------------------*
DATA:gt_data    TYPE TABLE OF ty_data,
     gt_data_nm TYPE TABLE OF ty_data,
     fs_wa_nm   TYPE ty_data,
     wa_data    TYPE ty_data,
     wa_data_nm TYPE ty_data,
     gs_data_nm TYPE ty_data,
     ls_data_nm TYPE ty_data.


DATA:gt_head TYPE TABLE OF ty_head,
     wa_head TYPE ty_head.

DATA:gt_yf_item TYPE TABLE OF zsvat_yfcl_i,
     gs_yf_item TYPE zsvat_yfcl_i.


DATA:gt_rule TYPE TABLE OF ztvat_rule.


DATA:lv_zidnum TYPE string.
DATA:lv_zjssy  TYPE string.
DATA:lt_tabix TYPE TABLE OF sy-tabix.
DATA:wa_ztvat_invoice TYPE ztvat_invoice,
     gt_ztvat_invoice TYPE TABLE OF ztvat_invoice,
     wa_ztvat_data    TYPE ztvat_data,
     gt_ztvat_data    TYPE TABLE OF ztvat_data.

DATA:gv_code TYPE sy-ucomm.

DATA:gv_clsj_kpjh TYPE abap_bool. "当处理数据的时候修改了 开票机号

*&SPWIZARD: DECLARATION OF TABLECONTROL 'ZTRCL_YF' ITSELF
CONTROLS: ztrcl_yf TYPE TABLEVIEW USING SCREEN 9000.

*&SPWIZARD: LINES OF TABLECONTROL 'ZTRCL_YF'
DATA:     g_ztrcl_yf_lines  LIKE sy-loopc.

DATA:     ok_code LIKE sy-ucomm.

"备注字段
DATA container TYPE REF TO cl_gui_custom_container.
DATA editor TYPE REF TO cl_gui_textedit.
DATA:BEGIN OF lt_beizhu OCCURS 0,
       zbeizhu TYPE ztvat_data-zbeizhu,
     END OF lt_beizhu.

CONSTANTS:g_tx_matnr TYPE mara-matnr VALUE '000000000069999997'.

*&---------------------------------------------------------------------*
* DEFINITION
*&---------------------------------------------------------------------*


DEFINE init_fieldcat.      "  ALV Fieldcat Setting

  gs_lvc-fieldname  = &1.
  gs_lvc-seltext_l  = &2.
  gs_lvc-outputlen  = &3.
  gs_lvc-checkbox   = &4.
  gs_lvc-edit       = &5.
  gs_lvc-hotspot    = &6.
  gs_lvc-decimals_out   = &7.
  gs_lvc-key        = &8.
  gs_lvc-intlen     = &9.

*  IF &1 = 'ZJSZL'.
*   gs_lvc-drdn_FIELD = 'DROP_DOWN_HANDLE'.
*  ENDIF.

  APPEND gs_lvc TO gt_lvc.
  CLEAR gs_lvc.

END-OF-DEFINITION.

DEFINE m_sort.
  ADD 1 TO gs_sort-spos.
  gs_sort-fieldname = &1.
  gs_sort-up        = 'X'.
  gs_sort-subtot    = &2.
  APPEND gs_sort TO gt_sort.
END-OF-DEFINITION.

DEFINE set_fieldcat.

  gt_fieldcat-fieldname = &1.
  gt_fieldcat-coltext   = &2.
  gt_fieldcat-ref_table = &3.
  gt_fieldcat-ref_field = &4.
  gt_fieldcat-edit      = &5.
  gt_fieldcat-hotspot   = &6.
  gt_fieldcat-intlen    = '40'.
*  IF &1 = 'BOX'.
*    GT_FIELDCAT-CHECKBOX = 'X'.
*  ENDIF.

  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
DATA: gt_lvc           TYPE slis_t_fieldcat_alv,
      gt_sort          TYPE slis_t_sortinfo_alv,
      gs_layout        TYPE slis_layout_alv,   "alv的格式
      wa_layout        TYPE lvc_s_layo,   "alv的格式
      gs_grid_settings TYPE lvc_s_glay,
      gs_variant       TYPE disvariant,
      gs_lvc           TYPE slis_fieldcat_alv,
      gt_fieldcat      LIKE TABLE OF lvc_s_fcat WITH HEADER LINE,

*定义存储下拉列表的数据
      gt_ddval         TYPE lvc_t_drop,
      gw_ddval         TYPE lvc_s_drop,

      gt_events        TYPE slis_t_event,
      gw_events        TYPE slis_alv_event.

*--下拉框
DATA: lt_dropdown TYPE lvc_t_drop,
      ls_dropdown TYPE lvc_s_drop.

DATA:l_grid    TYPE REF TO cl_gui_alv_grid.
*&---------------------------------------------------------------------*
* Selection Screen
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-005.
PARAMETERS: p1 TYPE c RADIOBUTTON GROUP g1 USER-COMMAND uc DEFAULT 'X',
            p2 TYPE c RADIOBUTTON GROUP g1,
            p3 TYPE c RADIOBUTTON GROUP g1,
            p4 TYPE c RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
  s_bukrs   FOR ztvat_data-bukrs MODIF ID ty1 DEFAULT '3500' OBLIGATORY."公司代码
PARAMETERS:p_vtweg TYPE vbrk-vtweg MODIF ID ty1 DEFAULT '10'."OBLIGATORY."分销渠道
SELECT-OPTIONS:
  s_fkdat    FOR vbrk-fkdat MODIF ID ty1,"出具发票日期
  s_fkart  FOR vbrk-fkart  MODIF ID ty1 DEFAULT 'ZF1',"发票类型
  s_vbeln   FOR vbrk-vbeln MODIF ID ty1,"SAP发票号
  s_zsale   FOR kna1-kunnr MODIF ID ty1,"销售员
  s_kunrg   FOR vbrk-kunrg MODIF ID ty1."客户
*  S_ZRULE   FOR ZTVAT_RULE-ZRULE MODIF ID TY1."合并拆分规则
PARAMETERS:p_zss  AS CHECKBOX USER-COMMAND gy1 MODIF ID ty1.

SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
  s_bukrs2   FOR ztvat_data-bukrs MODIF ID ty2 DEFAULT '3500'  OBLIGATORY."公司代码
PARAMETERS:p_vtweg2 TYPE vbrk-vtweg MODIF ID ty2 DEFAULT '10' OBLIGATORY."分销渠道
SELECT-OPTIONS:
*  S_FKDAT2    FOR VBRK-FKDAT MODIF ID TY2,"出具发票日期
  s_zid2      FOR ztvat_data-zidnum MODIF ID ty2,"样票号码
  s_zsale2   FOR ztvat_data-zsaler MODIF ID ty2,"销售员
  s_kunrg2   FOR vbrk-kunrg MODIF ID ty2,"客户
  s_wmfph2   FOR ztvat_data-zwmfph MODIF ID ty2. "外贸发票号
SELECTION-SCREEN END OF BLOCK blk2.

SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
  s_bukrs3   FOR ztvat_data-bukrs MODIF ID ty3 DEFAULT '3500' OBLIGATORY."公司代码
PARAMETERS:p_vtweg3 TYPE vbrk-vtweg MODIF ID ty3 DEFAULT '10' OBLIGATORY."分销渠道
SELECT-OPTIONS:
*  S_FKDAT3    FOR VBRK-FKDAT MODIF ID TY3,"出具发票日期
  s_zid3      FOR ztvat_data-zidnum MODIF ID ty3,"样票号码
  s_zsale3   FOR ztvat_data-zsaler MODIF ID ty3,"销售员
  s_kunrg3   FOR vbrk-kunrg MODIF ID ty3, "客户
  s_wmfph3   FOR ztvat_data-zwmfph MODIF ID ty3. "外贸发票号
SELECTION-SCREEN END OF BLOCK blk3.

SELECTION-SCREEN BEGIN OF BLOCK blk4 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
  s_zid4     FOR ztvat_data-zidnum MODIF ID ty4,"样票号码
  s_vbeln4   FOR vbrk-vbeln MODIF ID ty4,"SAP发票号
  s_zjsdm4   FOR ztvat_return-zjsdm MODIF ID ty4,"金税发票代码
  s_zjshm4   FOR ztvat_return-zjshm MODIF ID ty4,"金税发票号码
  s_zdate4   FOR ztvat_return-zjsdate MODIF ID ty4."金税发票日期
PARAMETERS:p_gl  AS CHECKBOX USER-COMMAND gy1 MODIF ID ty4.
SELECTION-SCREEN END OF BLOCK blk4.

*&-----------------------------------------------------------------*
*& 初始化处理
*&-----------------------------------------------------------------*
INITIALIZATION.
*&-----------------------------------------------------------------*
*& 选择屏幕控制
*&-----------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'TY1'.
        IF p1 IS  INITIAL.
          screen-active = '0'.
        ELSE.
          screen-active = '1'.
        ENDIF.

      WHEN 'TY2'.
        IF p2 IS  INITIAL.
          screen-active = '0'.
        ELSE.
          screen-active = '1'.
        ENDIF.

      WHEN 'TY3'.
        IF p3 IS  INITIAL.
          screen-active = '0'.
        ELSE.
          screen-active = '1'.
        ENDIF.

      WHEN 'TY4'.
        IF p4 IS  INITIAL.
          screen-active = '0'.
        ELSE.
          screen-active = '1'.
        ENDIF.

    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.



*&-----------------------------------------------------------------*
*& 参数输入检查
*&-----------------------------------------------------------------*
AT SELECTION-SCREEN.


*&-----------------------------------------------------------------*
*& 程序开始处理
*&-----------------------------------------------------------------*
START-OF-SELECTION.
* get output data
  IF p1 EQ 'X'."合并拆分
    PERFORM frm_getdata_hbcf.
    PERFORM frm_dealdata_hbcf.
    PERFORM frm_display_hbcf.

  ELSEIF p2 EQ 'X'."数据处理
    PERFORM frm_getdata_sjcl.
    PERFORM frm_display_sjcl.

  ELSEIF p3 EQ 'X'."撤销数据处理

    PERFORM frm_getdata_cxsj.
    PERFORM frm_display_cxsj.

  ELSEIF p4 EQ 'X'."金税发票对应报表

    PERFORM frm_getdata_jsbb.
    PERFORM frm_display_jsbb."

  ENDIF.
*&---------------------------------------------------------------------*
*& Form FRM_GETDATA_HBCF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_getdata_hbcf .

  DATA:BEGIN OF ls_ausp,
         atwrt TYPE ztpp_004-zhnl,
         matnr TYPE mch1-matnr,
         charg TYPE mch1-charg,
       END OF ls_ausp.

  DATA:  zausp LIKE TABLE OF ls_ausp WITH HEADER LINE.

  DATA:BEGIN OF ls_tpp4,
         matnr TYPE ztpp_004-matnr,
         zswh  TYPE ztpp_004-zswh,
         zhanh TYPE ztpp_004-zhanh,
         zhnl  TYPE ztpp_004-zhnl,
       END OF ls_tpp4.

  DATA:ztpp4  LIKE TABLE OF ls_tpp4 WITH HEADER LINE .
  DATA:ztpp5  LIKE TABLE OF ls_tpp4 WITH HEADER LINE.

  SELECT
    vbrk~bukrs
    vbrk~fkdat
    vbrk~fkart
    vbrk~vbeln
    vbrk~rfbsk
    vbrp~posnr
    vbrk~vkorg
    vbrk~vtweg
    vbrk~kunrg
    vbrk~ernam
    vbrk~knumv
    vbrk~netwr AS netwr_k
    vbrp~aubel
    vbrp~matnr
    vbrp~werks
    vbrp~arktx
    vbrp~charg
    vbrp~fkimg
    vbrp~vrkme
    vbrp~netwr
    vbrp~mwsbp
    vbrp~waerk
    vbrp~umvkz
    vbrp~umvkn
    vbpa~kunnr AS kunnr_z1
    INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM vbrk
    JOIN vbrp ON vbrk~vbeln = vbrp~vbeln
    JOIN vbpa ON vbrp~aubel = vbpa~vbeln
*    JOIN ZTVAT_INVOICE ON ZTVAT_INVOICE~ZIDNUM = VBRK~VBELN
    WHERE fkart IN ( 'ZF0','ZF1','ZF9','ZFA','ZG2','ZIG','ZIV','ZIV1','ZIVG','ZL2','ZRE','ZREA' )
      AND fkdat IN s_fkdat
      AND fkart IN s_fkart
      AND vbrk~vbeln IN s_vbeln
      AND bukrs IN s_bukrs
      AND kunrg IN s_kunrg
      AND vtweg = p_vtweg
      AND vbpa~parvw = 'Z1'
      AND vbpa~kunnr IN s_zsale
      AND rfbsk = 'A'.


*筛除sap发票已生成样票号且进行中的数据
*  SELECT ZTVAT_INVOICE~ZIDNUM, ZSTATUS
*    INTO TABLE @DATA(GT_INVOICE)
*    FROM ZTVAT_INVOICE
*    JOIN ZTVAT_DATA ON ZTVAT_INVOICE~ZIDNUM = ZTVAT_DATA~ZIDNUM
*   WHERE ZSTATUS NE '50'.
*
*  LOOP AT GT_INVOICE ASSIGNING FIELD-SYMBOL(<FS_INVOICE>).
*    <FS_INVOICE>-ZIDNUM =  |{ <FS_INVOICE>-ZIDNUM ALPHA = OUT }|.
*  ENDLOOP.



  IF gt_data IS NOT INITIAL.
*----筛除数据
    SELECT vbeln,zidnum
      INTO TABLE @DATA(gt_invoice)
      FROM ztvat_invoice
       FOR ALL ENTRIES IN @gt_data
     WHERE vbeln = @gt_data-vbeln.

    SORT gt_invoice BY vbeln .


*----客户名称
    SELECT
      partner,
      name_org1
      INTO TABLE @DATA(gt_but000)
      FROM but000
      FOR ALL ENTRIES IN @gt_data
      WHERE partner = @gt_data-kunrg.
    SORT gt_but000 BY partner.

*----客户税号、地址电话
    SELECT
      kna1~stcd5,
      kna1~kunnr,
      kna1~stras,
      kna1~telf1
*      kna1~faksd
      INTO TABLE @DATA(gt_kna1)
      FROM kna1
      FOR ALL ENTRIES IN @gt_data
      WHERE kunnr = @gt_data-kunrg.
    SORT gt_kna1 BY kunnr.

*-----客户冻结
    SELECT
      kunnr,
      vkorg,
      vtweg,
      spart,
      faksd
      INTO TABLE @DATA(gt_knvv)
      FROM knvv
      FOR ALL ENTRIES IN @gt_data
      WHERE kunnr = @gt_data-kunrg.
    SORT gt_knvv BY kunnr.

*----客户开户行及账号
    SELECT
      knbk~kunnr,
      knbk~bankl,
      knbk~bankn,
      bnka~banka
      INTO TABLE @DATA(gt_knbk)
      FROM knbk
      JOIN bnka ON knbk~bankl = bnka~bankl
      FOR ALL ENTRIES IN @gt_data
      WHERE kunnr = @gt_data-kunrg
        AND bnka~banks = 'CN '.
    SORT gt_knbk BY kunnr.

*----外贸发票号
    SELECT
      vbak~vbeln,
      vbak~zwmfph
      INTO TABLE @DATA(gt_vbak)
      FROM vbak
      FOR ALL ENTRIES IN @gt_data
      WHERE vbeln = @gt_data-aubel.
    SORT gt_vbak BY vbeln.

*----销售员
*    SELECT
*      vbeln,
*      vbpa~kunnr,
*      parvw
*      INTO TABLE @DATA(gt_vbpa)
*      FROM vbpa
*      FOR ALL ENTRIES IN @gt_data
*      WHERE vbeln = @gt_data-aubel
*        AND parvw = 'Z1'.
*    SORT gt_vbpa BY vbeln.
*    IF gt_vbpa IS NOT INITIAL.
    SELECT
      kunnr,
      name1
      INTO TABLE @DATA(gt_kna1sh)
      FROM kna1
      FOR ALL ENTRIES IN @gt_data
      WHERE kunnr = @gt_data-kunnr_z1.
    SORT gt_kna1sh BY kunnr.
*    ENDIF.

*----评估类
    SELECT
      mbew~matnr,
      mbew~bwkey,
      mbew~bklas
      INTO TABLE @DATA(gt_mbew)
      FROM mbew
      FOR ALL ENTRIES IN @gt_data
      WHERE matnr = @gt_data-matnr
        AND bwkey = @gt_data-werks.
    SORT gt_mbew BY matnr bwkey.

*----等级
    DATA:lv_atinn TYPE ausp-atinn.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'Z_DATA02'
      IMPORTING
        output = lv_atinn.

    SELECT
      mch1~matnr,
      mch1~charg,
      ausp~objek,
      ausp~atwrt,
      ausp~atinn
      INTO TABLE @DATA(gt_ausp)
      FROM ausp
      JOIN mch1 ON ausp~objek = mch1~cuobj_bm
      FOR ALL ENTRIES IN @gt_data
      WHERE matnr = @gt_data-matnr
        AND atinn = @lv_atinn.
    SORT gt_ausp BY matnr charg.

*----税率
*    SELECT
*      knumv,
*      kposn,
*      kbetr
*      INTO TABLE @DATA(gt_prcd_elements)
*      FROM prcd_elements
*      FOR ALL ENTRIES IN @gt_data
*      WHERE knumv = @gt_data-knumv
*        AND kposn = @gt_data-posnr
*        AND kschl = 'MWSI'.

    "含税单价
    SELECT * INTO TABLE @DATA(lt_prcd_elements)
    FROM prcd_elements
    FOR ALL ENTRIES IN @gt_data
    WHERE knumv = @gt_data-knumv.
    SORT lt_prcd_elements BY knumv.

    "规格型号
    SELECT matnr,
           zgg
    INTO TABLE @DATA(lt_zgg)
    FROM ztmdm_mat_log
    FOR ALL ENTRIES IN @gt_data
    WHERE matnr = @gt_data-matnr.
    SORT lt_zgg BY matnr.

    SELECT
      mch1~matnr,
      mch1~charg,
      ausp~atwrt
      INTO CORRESPONDING FIELDS OF TABLE @zausp
      FROM mch1
      LEFT JOIN ausp ON mch1~cuobj_bm = ausp~objek
      FOR ALL ENTRIES IN @gt_data
      WHERE  charg = @gt_data-charg
      AND matnr = @gt_data-matnr
      AND ausp~atinn = '0000000814'.
    SORT zausp BY matnr charg.
    IF zausp[] IS NOT INITIAL.
      SELECT matnr zhnl zswh zhanh
        INTO CORRESPONDING FIELDS OF TABLE ztpp4
        FROM ztpp_004
        FOR ALL ENTRIES IN zausp
        WHERE matnr = zausp-matnr
        AND zhxm = '24批号'
        AND zhnl = zausp-atwrt.
      SORT ztpp4 BY matnr zhnl.
      IF ztpp4[] IS NOT INITIAL.
        SELECT zswh zhanh  zhnl
          INTO CORRESPONDING FIELDS OF TABLE ztpp5
          FROM ztpp_004
          FOR ALL ENTRIES IN ztpp4
          WHERE zswh = ztpp4-zswh
          AND zhanh = ztpp4-zhanh
          AND zhxm = '规格'.
        SORT ztpp5 BY zswh zhanh.
      ENDIF.
    ENDIF.
  ENDIF.

*筛除再生丝
*    IF P_ZSS IS NOT INITIAL.
  SELECT
    matnr,
    charg,
    zcpmcz
    INTO TABLE @DATA(gt_ztmm_eos_001_log)
    FROM ztmm_eos_001_log
    FOR ALL ENTRIES IN @gt_data
    WHERE matnr = @gt_data-matnr
      AND charg = @gt_data-charg.
  SORT gt_ztmm_eos_001_log BY matnr charg.


  "取特殊规则
  SELECT bukrs,
         kunrg,
         zdk,
         ztxdk,
         ztxhbhk,
         zkpxe
    INTO CORRESPONDING FIELDS OF TABLE @gt_rule
    FROM ztvat_rule.
  SORT gt_rule BY bukrs kunrg.

  "取税收分类编码
  SELECT *
  INTO TABLE @DATA(gt_ssfl)
  FROM ztvat_matnr.

*    ENDIF.

*-----数据处理
  LOOP AT gt_data INTO wa_data.
*--排除数据
    READ TABLE gt_invoice ASSIGNING FIELD-SYMBOL(<fs_invoice>) WITH KEY vbeln = wa_data-vbeln BINARY SEARCH.
    IF sy-subrc = 0.
      DELETE gt_data.
      CONTINUE.
    ENDIF.
*--客户税号
    READ TABLE gt_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>) WITH KEY kunnr = wa_data-kunrg BINARY SEARCH.
    IF sy-subrc = 0.
      IF wa_data-vtweg = '10'.
        wa_data-zkhsh = <fs_kna1>-stcd5.
        wa_data-zaddress = <fs_kna1>-stras && <fs_kna1>-telf1.
      ENDIF.
    ENDIF.

*--客户冻结
    LOOP AT gt_knvv ASSIGNING FIELD-SYMBOL(<fs_knvv>) WHERE kunnr = wa_data-kunrg AND faksd IS NOT INITIAL.
      DATA(faksd_flag) = abap_true.
      EXIT.
    ENDLOOP.
    IF faksd_flag = abap_true.
      DELETE gt_data.
      CONTINUE.
    ENDIF.

*再生丝
    IF p_zss IS NOT INITIAL.
      READ TABLE gt_ztmm_eos_001_log ASSIGNING FIELD-SYMBOL(<fs_zss>) WITH KEY matnr = wa_data-matnr charg = wa_data-charg BINARY SEARCH.
      IF sy-subrc = 0.
        IF <fs_zss>-zcpmcz CS '再生'."包含再生丝
          DELETE gt_data.
          CONTINUE.
        ENDIF.
      ENDIF.
    ELSE.
      READ TABLE gt_ztmm_eos_001_log ASSIGNING <fs_zss> WITH KEY matnr = wa_data-matnr charg = wa_data-charg BINARY SEARCH  .
      IF sy-subrc = 0.
        IF <fs_zss>-zcpmcz CS '再生'."包含再生丝
          wa_data-zzss = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

*--发票种类
    IF wa_data-vtweg = '10'.
      wa_data-zjszl = '专票'.
    ELSEIF wa_data-vtweg = '20'.
      wa_data-zjszl = '普票'.
    ENDIF.

*--开票机号
    wa_data-zkpjh = '100'.

*--金额
    IF wa_data-matnr = g_tx_matnr.
      wa_data-light = icon_led_yellow.
    ENDIF.
    CASE wa_data-fkart.
      WHEN 'ZIG' OR 'ZIVG' OR 'ZRE' OR 'ZREA' OR 'ZG2' .
        wa_data-zmount = wa_data-netwr * -1.
        wa_data-zse = - wa_data-mwsbp.
        wa_data-light  = icon_led_red.
      WHEN OTHERS.
        wa_data-zse = wa_data-mwsbp.
        wa_data-zmount = wa_data-netwr.
    ENDCASE.

    IF wa_data-light IS INITIAL.
      wa_data-light = icon_led_green.
    ENDIF.

*--客户名称
    READ TABLE gt_but000 ASSIGNING FIELD-SYMBOL(<fs_but000>) WITH KEY partner = wa_data-kunrg BINARY SEARCH.
    IF sy-subrc = 0.
      wa_data-zkhname = <fs_but000>-name_org1.
    ENDIF.
*--客户开户行及账号
    READ TABLE gt_knbk ASSIGNING FIELD-SYMBOL(<fs_knbk>) WITH KEY kunnr = wa_data-kunrg BINARY SEARCH.
    IF sy-subrc = 0 AND wa_data-vtweg = '10'.
      wa_data-zbank = <fs_knbk>-banka && <fs_knbk>-bankn.
    ENDIF.

*--外贸发票号
    READ TABLE gt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>) WITH KEY vbeln = wa_data-aubel BINARY SEARCH.
    IF sy-subrc = 0.
      wa_data-zwmfph = <fs_vbak>-zwmfph.
    ENDIF.

*--销售员
*    READ TABLE gt_vbpa ASSIGNING FIELD-SYMBOL(<fs_vbpa>)  WITH KEY vbeln = wa_data-aubel BINARY SEARCH.
*    IF sy-subrc = 0.
    READ TABLE gt_kna1sh ASSIGNING FIELD-SYMBOL(<fs_kna1sh>) WITH KEY kunnr = wa_data-kunnr_z1 BINARY SEARCH.
    IF sy-subrc = 0.
      wa_data-zsaler = <fs_kna1sh>-name1.
    ENDIF.
*    ENDIF.

*--评估类
    READ TABLE gt_mbew ASSIGNING FIELD-SYMBOL(<fs_mbew>) WITH KEY matnr = wa_data-matnr bwkey = wa_data-werks BINARY SEARCH.
    IF sy-subrc = 0.
      wa_data-bklas = <fs_mbew>-bklas.
      wa_data-zbklas = <fs_mbew>-bklas+0(2).
    ENDIF.
*--商品名称
    SEARCH wa_data-arktx FOR '\'.
    IF sy-subrc = 0.
      SPLIT wa_data-arktx AT '\' INTO wa_data-zspmc wa_data-zggxh.
    ELSE.
      wa_data-zspmc = wa_data-arktx.
    ENDIF.

    SELECT SINGLE ktgrm INTO @DATA(lv_ktgrm) FROM mvke
  WHERE matnr = @wa_data-matnr
    AND vkorg = @wa_data-vkorg
    AND vtweg = @wa_data-vtweg.
    IF lv_ktgrm = '11' OR lv_ktgrm = '12' OR lv_ktgrm = '13' OR lv_ktgrm = '14' OR lv_ktgrm = '15'.
      IF wa_data-vtweg = '10'.
        IF wa_data-zzss = 'X'.
          wa_data-zspmc = '再生涤纶丝' && wa_data-zspmc.
        ELSE.
          wa_data-zspmc = '涤纶丝' && wa_data-zspmc.
        ENDIF.

      ELSEIF wa_data-vtweg = '20'.
        wa_data-zspmc = '涤纶长丝' && wa_data-zspmc.
      ENDIF.

    ENDIF.
*--规格型号
    READ TABLE lt_zgg INTO DATA(ls_zgg) WITH KEY matnr = wa_data-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      wa_data-zggxh = ls_zgg-zgg.
    ENDIF.

    IF wa_data-zggxh IS INITIAL.
      READ TABLE zausp WITH KEY matnr = wa_data-matnr
                                charg = wa_data-charg BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE ztpp4 WITH KEY matnr = zausp-matnr
                                  zhnl = zausp-atwrt BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE ztpp5 WITH KEY zswh = ztpp4-zswh
                                    zhanh = ztpp4-zhanh BINARY SEARCH.
          IF sy-subrc = 0.
            wa_data-zggxh = ztpp5-zhnl.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*--等级
    READ TABLE gt_ausp ASSIGNING FIELD-SYMBOL(<fs_ausp>) WITH KEY matnr = wa_data-matnr charg = wa_data-charg BINARY SEARCH.
    IF sy-subrc = 0.
      wa_data-atwrt = <fs_ausp>-atwrt.
    ENDIF.

*--汇率
    DATA:lv_datum_first TYPE sy-datum.
    lv_datum_first = sy-datum+0(6) && '01'.
    CALL FUNCTION 'READ_EXCHANGE_RATE'
      EXPORTING
        client           = sy-mandt
        date             = lv_datum_first
        foreign_currency = wa_data-waerk
        local_currency   = 'CNY'
      IMPORTING
        exchange_rate    = wa_data-kursk
      EXCEPTIONS
        no_rate_found    = 1
        no_factors_found = 2
        no_spread_found  = 3
        derived_2_times  = 4
        overflow         = 5
        zero_rate        = 6
        OTHERS           = 7.

*--本位币种、金额
    wa_data-zwaerk = 'CNY'.
    IF wa_data-waerk NE wa_data-zwaerk.
      wa_data-zbmount = wa_data-zmount * wa_data-kursk.
    ELSE.
      wa_data-zbmount = wa_data-zmount.
    ENDIF.
*--单价
    wa_data-zbprice = wa_data-zbmount / wa_data-fkimg.

*--单位
    IF wa_data-vrkme = 'KG'.
      wa_data-vrkme = '千克'.
    ENDIF.

    "税收分类编码
    LOOP AT gt_ssfl INTO DATA(ls_ssfl).
      IF wa_data-arktx+0(5) CS ls_ssfl-zspmc.
        wa_data-zssflbm = ls_ssfl-zssflbm.
        EXIT.
      ENDIF.
    ENDLOOP.

*--备注
    DATA:n TYPE i.
    DATA:lv_beizhu TYPE char100.
    IF wa_data-vtweg = '10'.
      SELECT
        vbkd~vbeln,
        vbkd~bstkd
        INTO TABLE @DATA(gt_vbkd)
        FROM vbkd
        WHERE vbeln = @wa_data-aubel.
      SORT gt_vbkd BY vbeln bstkd.
      DELETE ADJACENT DUPLICATES FROM gt_vbkd COMPARING ALL FIELDS.
      READ TABLE gt_vbkd ASSIGNING FIELD-SYMBOL(<fs_vbkd>) WITH KEY vbeln = wa_data-aubel.
      IF sy-subrc = 0.
        wa_data-bstkd = <fs_vbkd>-bstkd.
      ENDIF.

      IF wa_data-bstkd IS INITIAL.
        wa_data-bstkd = wa_data-aubel.
      ENDIF.
      wa_data-zbeizhu = wa_data-bstkd .
    ENDIF.
*--贸易方式
    SELECT SINGLE sdabw INTO @DATA(lv_sdabw) FROM vbkd WHERE vbeln = @wa_data-aubel.
    IF sy-subrc = 0.
      SELECT SINGLE bezei INTO wa_data-bezei FROM tvsakt WHERE sdabw = lv_sdabw AND spras = '1'.
    ENDIF.
*--税额


*--ZTAX税率

    READ TABLE lt_prcd_elements INTO DATA(ls_prcd_element) WITH KEY knumv = wa_data-knumv  kposn = wa_data-posnr kschl = 'MWSI' .
    IF sy-subrc = 0.
      wa_data-ztax = ls_prcd_element-kbetr / 100."税率
    ENDIF.

    "单价
    LOOP AT lt_prcd_elements INTO ls_prcd_element WHERE knumv = wa_data-knumv AND kposn = wa_data-posnr AND
     ( kschl = 'ZPRD'
     OR kschl = 'ZPR9'
     OR kschl = 'ZPR0'
     OR kschl = 'ZPR1'
     OR kschl = 'ZPI1'
     OR kschl = 'PBXX'
     OR kschl = 'ZPRA'
     OR kschl = 'PB00' ) AND kbetr NE 0  .
      IF ls_prcd_element-kpein NE 0.
        wa_data-zprice = ls_prcd_element-kbetr / ls_prcd_element-kpein * wa_data-umvkz / wa_data-umvkn."含税单价
      ELSE.
        wa_data-zprice = ls_prcd_element-kbetr * wa_data-umvkz / wa_data-umvkn."含税单价
      ENDIF.

      IF ls_prcd_element-kschl = 'ZPRA'."外贸时

        wa_data-zprice = wa_data-zprice * wa_data-kursk .

      ENDIF.
    ENDLOOP.

    "特殊规则
    READ TABLE gt_rule ASSIGNING FIELD-SYMBOL(<fs_rule>) WITH KEY bukrs = wa_data-bukrs kunrg = wa_data-kunrg BINARY SEARCH.
    IF sy-subrc = 0.
      CASE 'X'.
        WHEN <fs_rule>-zdk.
          wa_data-ztsgz = '单开'.
        WHEN <fs_rule>-ztxdk.
          wa_data-ztsgz = '贴息单开'.
        WHEN <fs_rule>-ztxhbhk.
          wa_data-ztsgz = '贴息合并货款'.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    "  贴息标识
    IF wa_data-matnr = g_tx_matnr.
      wa_data-tx_flag = abap_true.
    ENDIF.

    MODIFY gt_data FROM wa_data.
    CLEAR:lv_ktgrm,wa_data,lv_datum_first,lv_sdabw.
  ENDLOOP.

  SORT gt_data BY kunrg zwmfph vbeln matnr zspmc zggxh atwrt zbmount.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DEALDATA_HBCF
*&---------------------------------------------------------------------*
*& 数据处理
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_dealdata_hbcf .

  LOOP AT gt_data INTO wa_data.
    CALL FUNCTION 'ENQUEUE_EZLJS001'
      EXPORTING
*       MODE_VBRP      = 'E'
*       MANDT          = SY-MANDT
        vbeln          = wa_data-vbeln
        posnr          = wa_data-posnr
*       X_VBELN        = ' '
*       X_POSNR        = ' '
*       _SCOPE         = '2'
*       _WAIT          = ' '
*       _COLLECT       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDLOOP.





ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DISPLAY_HBCF
*&---------------------------------------------------------------------*
*& 合并拆分
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_hbcf .
  PERFORM frm_layout.             "设置输出格式
  PERFORM frm_fieldcat.
*  PERFORM FRM_SORT.               "设置排序、合计
  PERFORM frm_output.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_LAYOUT
*&---------------------------------------------------------------------*
*& 合并拆分
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_layout .
*  GS_LAYOUT-CWIDTH_OPT = 'X'.
  gs_layout-colwidth_optimize = 'X'.
*  GS_LAYOUT-SEL_MODE   = 'A'.
  gs_layout-edit_mode   = 'A'.
  gs_layout-box_fieldname  = 'BOX'.  "选择框字段名
  gs_grid_settings-edt_cll_cb  = 'X' .   "显示界面可编辑字段上修改了数据，回车后就会立即将内表的数据也修改
  gs_layout-info_fieldname = 'CLR'.
  gs_layout-cell_merge = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_FIELDCAT
*&---------------------------------------------------------------------*
*& 合并拆分
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_fieldcat .

  CLEAR:gt_lvc.

*  INIT_FIELDCAT 'SEL'    '复选框' '' 'X' 'X' 'X' '' 'X' ''.
  init_fieldcat 'LIGHT' '指示灯' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZIDNUM' '样票号码' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZJSZL'  '发票种类' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZKPJH'  '开票机号' '' '' ''  '' '' '' ''.
  init_fieldcat 'BUKRS'  '公司代码' '' '' ''  '' '' '' ''.
  init_fieldcat 'FKDAT'  '发票日期' '' '' ''  '' '' '' ''.
  init_fieldcat 'FKART'  '发票类型' '' '' ''  '' '' '' ''.
  init_fieldcat 'VBELN'  'SAP发票号' '' '' ''  '' '' '' ''.
  init_fieldcat 'POSNR'  '行项目' '' '' ''  '' '' '' ''.
  init_fieldcat 'VKORG'  '销售组织' '' '' ''  '' '' '' ''.
  init_fieldcat 'VTWEG'  '分销渠道' '' '' ''  '' '' '' ''.
  init_fieldcat 'KUNRG'  '客户' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZKHNAME'  '客户名称' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZKHSH'  '客户税号' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZADDRESS'  '客户地址电话' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZBANK'  '客户开户行及账号' '' '' ''  '' '' '' ''.
  init_fieldcat 'ERNAM'  '开票员' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZSALER'  '销售员' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZWMFPH'  '外贸发票号' '' '' ''  '' '' '' ''.
  init_fieldcat 'MATNR'  '物料编码' '' '' ''  '' '' '' ''.
  init_fieldcat 'BKLAS'  '评估类' '' '' ''  '' '' '' ''.
  init_fieldcat 'ARKTX'  '物料描述' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZSPMC'  '商品名称' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZGGXH'  '规格型号' '' '' ''  '' '' '' ''.
  init_fieldcat 'ATWRT'  '等级' '' '' ''  '' '' '' ''.
  init_fieldcat 'FKIMG'  '数量' '' '' ''  '' '' '' ''.
  init_fieldcat 'VRKME'  '计量单位' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZPRICE'  '含税单价' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZBPRICE'  '不含税单价' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZMOUNT'  '凭证货币金额' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZBMOUNT'  '本位币金额' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZSE'  '税额' '' '' ''  '' '' '' ''.
  init_fieldcat 'KURSK'  '汇率' '' '' ''  '' '' '' ''.
  init_fieldcat 'WAERK'  '币种' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZWAERK'  '本位币种' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZTAX'  '税率' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZBEIZHU'  '备注' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZSSFLBM'  '税收分类编码' '' '' ''  '' '' '' ''.
  init_fieldcat 'BEZEI'  '贸易方式' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZZSS'  '再生丝标识' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZTSGZ' '特殊规则' '' '' ''  '' '' '' ''.
  init_fieldcat 'MSG'    '消息' '' '' ''  '' '' '' ''.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_OUTPUT
*&---------------------------------------------------------------------*
*& 合并拆分
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_output .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'ALV_PF_STATUS'
      i_callback_user_command  = 'ALV_USER_COMMAND'
      i_grid_settings          = gs_grid_settings
      is_layout                = gs_layout
      it_fieldcat              = gt_lvc[]
*     IT_EXCLUDING             = GT_EXCLUDE
*     IT_SORT                  = GT_SORT
      i_save                   = 'A'
      is_variant               = gs_variant
*     IT_EVENTS                = LT_EVENT
    TABLES
      t_outtab                 = gt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM alv_pf_status USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM.                    "ALV_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALV执行查询后的事件响应
*----------------------------------------------------------------------*
*      -->R_UCOMN      响应码
*      -->RS_SELFIELD  当前行信息
*----------------------------------------------------------------------*
FORM alv_user_command USING r_ucomm LIKE sy-ucomm  "设置按钮响应
                            rs_selfield TYPE slis_selfield.

*  DATA: L_ZQHDH TYPE ZDHHSJ-ZQ HDH,
*        WA_ZDHHSJ1 TYPE ZDHHSJ,
*        LW_DATA LIKE WA_DATA.
  DATA: l_row  TYPE lvc_t_row,
        l_roid TYPE lvc_t_roid,
        lt_row TYPE lvc_t_roid,
        ls_row TYPE lvc_s_roid.

  DATA: ls_layout TYPE lvc_s_layo,
        lt_fcat   TYPE lvc_t_fcat,
        ls_fcat   TYPE LINE OF lvc_t_fcat.
*        L_GRID    TYPE REF TO CL_GUI_ALV_GRID.
  DATA: gs_stable TYPE lvc_s_stbl.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = l_grid.

  CALL METHOD l_grid->check_changed_data.
  rs_selfield-refresh = 'X'.


  CASE r_ucomm.
    WHEN 'HB'.
      LOOP AT gt_data INTO wa_data WHERE box = 'X'.
        IF wa_data-zidnum IS NOT INITIAL.
          MESSAGE e002(zjs001)."所选数据已生成样票号，请重新选择
        ENDIF.
        IF wa_data-vtweg = '10'.
          IF wa_data-zkhsh IS INITIAL OR wa_data-zaddress IS INITIAL OR wa_data-zbank IS INITIAL.
            MESSAGE e004(zjs001)."客户税务信息维护不全，请联系人员维护
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM frm_ucomm_hb.

    WHEN 'CF'.
      LOOP AT gt_data INTO wa_data WHERE box = 'X'.
        IF wa_data-zidnum IS NOT INITIAL.
          MESSAGE e002(zjs001)."所选数据已生成样票号，请重新选择
        ENDIF.
        IF wa_data-vtweg = '10'.
          IF wa_data-zkhsh IS INITIAL OR wa_data-zaddress IS INITIAL OR wa_data-zbank IS INITIAL.
            MESSAGE e004(zjs001)."客户税务信息维护不全，请联系人员维护
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM frm_ucomm_cf.

    WHEN 'HBCF'.
      LOOP AT gt_data INTO wa_data WHERE box = 'X'.
        IF wa_data-vtweg = '10'.
          IF wa_data-zkhsh IS INITIAL OR wa_data-zaddress IS INITIAL OR wa_data-zbank IS INITIAL.
            MESSAGE e004(zjs001)."客户税务信息维护不全，请联系人员维护
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM frm_ucomm_hbcf.

    WHEN OTHERS.
  ENDCASE.



  CALL METHOD l_grid->get_frontend_layout
    IMPORTING
      es_layout = ls_layout.

  CALL METHOD l_grid->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = lt_fcat.

  ls_layout-cwidth_opt = 'X'.
*  LS_LAYOUT-INFO_FNAME = 'CLR'.

*  PERFORM FRM_FIELDCAT.

  CALL METHOD l_grid->set_frontend_layout
    EXPORTING
      is_layout = ls_layout.

  CALL METHOD l_grid->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = lt_fcat.

  CALL METHOD l_grid->refresh_table_display.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UCOMM_CF
*&---------------------------------------------------------------------*
*& 拆分规则
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_ucomm_cf .
*排除alv上已生成样票号的数据
  DELETE gt_data WHERE zidnum IS NOT INITIAL .

  DATA:lv_netwr      TYPE vbrk-netwr,
       lv_zkpxe      TYPE ztvat_rule-zkpxe,
       lv_zkpxe_temp TYPE ztvat_rule-zkpxe,
       lv_zposnr     TYPE vbrp-posnr,
       n             TYPE i,
       m             TYPE i,
       ls_data       LIKE wa_data,
       lt_data       LIKE gt_data,
       lv_menge_xe   TYPE mseg-menge.

  DATA:gt_data_temp TYPE TABLE OF ty_data,
       gt_data_null TYPE TABLE OF ty_data,
       wa_data_temp TYPE ty_data.
  DATA:gt_data_cf   TYPE TABLE OF ty_data.
  DATA:lt_data_cf   TYPE TABLE OF ty_data.

  DATA:lv_error TYPE abap_bool.

  CLEAR:gt_ztvat_invoice,gt_ztvat_data,lt_data,lv_error,lt_tabix.

  IF gt_data IS INITIAL.
    MESSAGE s002(zjs001) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


  SORT gt_data BY vbeln zbklas posnr matnr." ZBKLAS  ZBMOUNT DESCENDING.

*  GT_DATA_TEMP = GT_DATA.
  LOOP AT gt_data INTO wa_data.
    MOVE-CORRESPONDING wa_data TO wa_data_temp.

    IF wa_data-box = ''.
      APPEND wa_data TO gt_data_null.
    ELSE.
      APPEND wa_data_temp TO gt_data_temp.
    ENDIF.
    CLEAR:wa_data,wa_data_temp.
  ENDLOOP.

  LOOP AT gt_data_null INTO wa_data.
    READ TABLE gt_data_temp INTO wa_data_temp WITH KEY vbeln = wa_data-vbeln.
    IF sy-subrc = 0.
      wa_data_temp = CORRESPONDING #( wa_data ).
      APPEND wa_data_temp TO gt_data_temp.
      DELETE gt_data_null.
    ENDIF.
    CLEAR:wa_data,wa_data_temp.
  ENDLOOP.

  LOOP AT gt_data_temp INTO wa_data_temp.
    IF wa_data_temp-fkart = 'ZIG'
    OR wa_data_temp-fkart = 'ZIVG'
    OR wa_data_temp-fkart = 'ZRE'
    OR wa_data_temp-fkart = 'ZREA'
    OR wa_data_temp-fkart = 'ZG2' .
      MESSAGE e022(zjs001)."退货及贷项发票数据不允许执行拆分
    ENDIF.

    CLEAR: wa_data_temp.
  ENDLOOP.

  CLEAR gt_data.

  gt_data_cf = CORRESPONDING #( gt_data_temp ).

  "如果有没超过限额和行数的数据
  LOOP AT gt_data_cf INTO DATA(gs_data_cf).
    lt_data_cf = gt_data_temp.
    DELETE lt_data_cf WHERE vbeln NE gs_data_cf-vbeln.
    gs_data_cf-vbeln_line = lines( lt_data_cf ).
    CASE gs_data_cf-vtweg.
      WHEN '10'."内贸
        IF gs_data_cf-bukrs = '3500'.
          lv_zkpxe = '350000'.
        ELSE.
          lv_zkpxe = '10000000'.
        ENDIF.
      WHEN '20'."外贸
        lv_zkpxe = '9999999999999.99'."无限额
      WHEN OTHERS.
    ENDCASE.
    READ TABLE gt_rule INTO DATA(gs_rule) WITH KEY bukrs = gs_data_cf-bukrs
                                                   kunrg = gs_data_cf-kunrg BINARY SEARCH.
    IF sy-subrc = 0 AND gs_rule-zkpxe > 0.
      lv_zkpxe = gs_rule-zkpxe.
    ENDIF.
    IF ( gs_data_cf-vbeln_line <= 8 AND gs_data_cf-netwr_k <= lv_zkpxe ) OR gs_rule-zdk = 'X'.
      IF sy-ucomm = 'CF'.
        MESSAGE s014(zjs001) INTO gs_data_cf-msg.
        APPEND gs_data_cf TO gt_data_null.
        DELETE gt_data_cf.
      ELSE.
        APPEND gs_data_cf TO gt_data_null.
        DELETE gt_data_cf.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT gt_data_cf BY vbeln.
  DELETE ADJACENT DUPLICATES FROM gt_data_cf COMPARING vbeln.
  CLEAR:lt_data_cf.

  LOOP AT gt_data_cf INTO gs_data_cf .
    LOOP AT gt_data_temp INTO wa_data_temp WHERE vbeln = gs_data_cf-vbeln.
      ADD 1 TO n.
      ADD wa_data_temp-zbmount TO lv_netwr.
      IF lv_netwr > lv_zkpxe AND lt_data_cf IS NOT INITIAL.
        PERFORM create_zidnum_list TABLES lt_data_cf lt_tabix CHANGING lv_zidnum.
        APPEND LINES OF lt_data_cf TO lt_data.
        CLEAR :lt_data_cf,lv_netwr,n.
        ADD wa_data_temp-zbmount TO lv_netwr.
      ENDIF.

      IF lv_netwr > lv_zkpxe.   "当叠加金额 大于限额
        gs_data_nm = CORRESPONDING #( wa_data_temp ).
        PERFORM frm_create_zidnum USING '2' CHANGING gs_data_nm gs_data_nm-zbmount lv_zidnum.
        CLEAR lt_data_cf.
        APPEND gs_data_nm TO lt_data.
        n = 1.
        CONTINUE.
      ENDIF.

      IF n > 8. "当超过8行时
        PERFORM create_zidnum_list TABLES lt_data_cf lt_tabix CHANGING lv_zidnum.
        APPEND LINES OF lt_data_cf TO lt_data.
        CLEAR :lt_data_cf,n.
      ENDIF.

      APPEND wa_data_temp TO lt_data_cf.
    ENDLOOP.
    IF lt_data_cf IS NOT INITIAL.
      PERFORM create_zidnum_list TABLES lt_data_cf lt_tabix CHANGING lv_zidnum.
      APPEND LINES OF lt_data_cf TO lt_data.
      CLEAR lt_data_cf.
    ENDIF.
    CLEAR:lv_netwr, n.
  ENDLOOP.



*--后续新增拆分后数据到alv屏幕
  APPEND LINES OF lt_data TO gt_data.
*--将没有选中的行加回屏幕
  APPEND LINES OF gt_data_null TO gt_data.

  MODIFY ztvat_data FROM TABLE gt_ztvat_data.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
  MODIFY ztvat_invoice FROM TABLE gt_ztvat_invoice.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
  DELETE ADJACENT DUPLICATES FROM gt_head COMPARING vbeln.
  CLEAR:gt_ztvat_data,gt_ztvat_invoice.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UCOMM_HBCF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_ucomm_hbcf .

  DATA:lv_netwr  TYPE vbap-netwr.
  DATA:lt_tabix TYPE TABLE OF sy-tabix,
       ls_tabix TYPE sy-tabix.
  DATA:lv_zposnr    TYPE ztvat_data-zposnr,
       gt_data_null TYPE TABLE OF ty_data.
  DATA:gt_data_hb   TYPE TABLE OF ty_data.
  DATA:n        TYPE i,
       lv_zkpxe TYPE vbrk-netwr.
  DATA:lv_vbeln TYPE vbrk-vbeln.
  DATA:lt_ziv1  TYPE TABLE OF ty_data.

  LOOP AT gt_data INTO wa_data WHERE box = 'X'.
    IF wa_data-fkart = 'ZIG'
    OR wa_data-fkart = 'ZIVG'
    OR wa_data-fkart = 'ZRE'
    OR wa_data-fkart = 'ZREA'
    OR wa_data-fkart = 'ZG2' .
      MESSAGE e003(zjs001)."所选数据包括退货、贷项及贴息发票数据，系统不允许自动合并，请手工选择处理这部分数据
    ENDIF.

  ENDLOOP.
*---先走拆分逻辑
  PERFORM frm_ucomm_cf.
*---后合并
  "取特殊规则
  CLEAR:gt_data_null,gt_data_nm.
  LOOP AT gt_data INTO wa_data.
    MOVE-CORRESPONDING wa_data TO wa_data_nm.
    IF wa_data_nm-box = 'X'.
      APPEND wa_data_nm TO gt_data_nm.
    ENDIF.

    IF wa_data_nm-box = ''.
      APPEND wa_data TO gt_data_null.
    ENDIF.
    CLEAR:wa_data_nm,wa_data.
  ENDLOOP.


  CLEAR:lv_netwr,gt_data,lv_zposnr,lt_ziv1.

  READ TABLE gt_data_nm TRANSPORTING NO FIELDS WITH KEY fkart = 'ZIV1'.
  IF sy-subrc = 0.
    LOOP AT gt_data_nm INTO wa_data WHERE fkart = 'ZIV1'.
      APPEND wa_data TO lt_ziv1.
    ENDLOOP.
    DELETE gt_data_nm WHERE fkart = 'ZIV1'.
  ENDIF.

  PERFORM zc_hb."正常合并

  IF lt_ziv1 IS NOT INITIAL.
    gt_data_nm = lt_ziv1.
    PERFORM zc_hb."正常合并
  ENDIF.

  APPEND LINES OF gt_data_null TO gt_data.
  SORT gt_data BY bukrs kunrg arktx zggxh atwrt zbklas ztax zprice matnr vbeln.





ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CREATE_ZIDNUM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA_NM
*&---------------------------------------------------------------------*
FORM frm_create_zidnum   USING    VALUE(p_type)
                         CHANGING  ls_data_nm STRUCTURE wa_data_nm
                                   pv_netwr
                                   pv_zidnum.

  CALL FUNCTION 'ZFM_JS_GETSERIALNUM' "样票号码
    EXPORTING
      object      = 'ZJSYP'
      nr_range_nr = '01'
    IMPORTING
      number      = lv_zidnum.

  wa_ztvat_invoice-zidnum = lv_zidnum.
  wa_ztvat_invoice-vbeln  = ls_data_nm-vbeln.
  wa_ztvat_invoice-zernam = sy-uname.
  wa_ztvat_invoice-zerdat = sy-datum.
  wa_ztvat_invoice-zertim = sy-uzeit.
  APPEND wa_ztvat_invoice TO gt_ztvat_invoice.
  CLEAR:wa_ztvat_invoice.

  CALL FUNCTION 'ZFM_JS_GETSERIALNUM'  "索引
    EXPORTING
      object      = 'ZJSSY'
      nr_range_nr = '01'
    IMPORTING
      number      = lv_zjssy.

  wa_ztvat_data-zidnum  = lv_zidnum.
  wa_ztvat_data-zsuoyin = lv_zjssy.
*  WA_ZTVAT_DATA-VBELN   = LS_DATA_NM-VBELN.
  wa_ztvat_data-matnr   = ls_data_nm-matnr.
  wa_ztvat_data-zstatus = '10'."处理中
  wa_ztvat_data-zjszl   = ls_data_nm-zjszl.
  wa_ztvat_data-zkpjh   = ls_data_nm-zkpjh.
  wa_ztvat_data-zposnr  = ls_data_nm-zposnr.
  wa_ztvat_data-bukrs   = ls_data_nm-bukrs.
*  WA_ZTVAT_DATA-FKDAT   = LS_DATA_NM-FKDAT.
*  WA_ZTVAT_DATA-FKART   = LS_DATA_NM-FKART.
  wa_ztvat_data-zposnr  = 10.
  wa_ztvat_data-vkorg   = ls_data_nm-vkorg.
  wa_ztvat_data-vtweg   = ls_data_nm-vtweg.
  wa_ztvat_data-kunrg   = ls_data_nm-kunrg.
  wa_ztvat_data-zkhname = ls_data_nm-zkhname.
  wa_ztvat_data-zkhsh   = ls_data_nm-zkhsh.
  wa_ztvat_data-zaddress = ls_data_nm-zaddress.
  wa_ztvat_data-zkhbank  = ls_data_nm-zbank.
  wa_ztvat_data-ernam    = ls_data_nm-ernam.
  wa_ztvat_data-zsaler   = ls_data_nm-zsaler.
  wa_ztvat_data-zwmfph   = ls_data_nm-zwmfph.
  wa_ztvat_data-bklas    = ls_data_nm-bklas.
  wa_ztvat_data-arktx    = ls_data_nm-arktx.
  wa_ztvat_data-zspmc    = ls_data_nm-zspmc.
  IF ls_data_nm-atwrt IS INITIAL.
    wa_ztvat_data-zggxh    = ls_data_nm-zggxh .
  ELSE.
    wa_ztvat_data-zggxh    = |{ ls_data_nm-zggxh } { ls_data_nm-atwrt }级| .
  ENDIF.

  wa_ztvat_data-atwrt    = ls_data_nm-atwrt.
  wa_ztvat_data-fkimg    = ls_data_nm-fkimg."数量
  wa_ztvat_data-vrkme    = ls_data_nm-vrkme.
  wa_ztvat_data-zmount   = ls_data_nm-zmount.
  wa_ztvat_data-zbmount  = pv_netwr."金额
  wa_ztvat_data-zse      = ls_data_nm-zse.
  wa_ztvat_data-zprice   = ls_data_nm-zprice.
  wa_ztvat_data-zbprice   = ls_data_nm-zbprice.

  wa_ztvat_data-kursk    = ls_data_nm-kursk.
  wa_ztvat_data-waerk    = ls_data_nm-waerk.
  wa_ztvat_data-zwaerk   = ls_data_nm-zwaerk.
  wa_ztvat_data-ztax     = ls_data_nm-ztax.
  IF ls_data_nm-bukrs = '3500' AND ls_data_nm-vtweg = '10'.
    wa_ztvat_data-zbeizhu  = |{ ls_data_nm-zsaler } { ls_data_nm-bstkd }|.
  ENDIF.
  wa_ztvat_data-zssflbm  = ls_data_nm-zssflbm.
  wa_ztvat_data-bezei    = ls_data_nm-bezei.
  wa_ztvat_data-zzss     = ls_data_nm-zzss.
  wa_ztvat_data-zernam   = sy-uname.
  wa_ztvat_data-zerdat   = sy-datum.
  wa_ztvat_data-zertim   = sy-uzeit.
  APPEND wa_ztvat_data TO gt_ztvat_data.

  CALL FUNCTION 'DEQUEUE_EZLJS001'
    EXPORTING
      vbeln = ls_data_nm-vbeln
      posnr = ls_data_nm-posnr.



  ls_data_nm-zidnum = lv_zidnum.
  ls_data_nm-zsuoyin = lv_zjssy.
  pv_zidnum = lv_zidnum.

  CLEAR:wa_ztvat_data,lv_zjssy,lv_zidnum.

*  MODIFY GT_DATA_NM FROM LS_DATA_NM TRANSPORTING ZIDNUM ZSUOYIN WHERE VBELN = LS_DATA_NM-VBELN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UCOMM_HB
*&---------------------------------------------------------------------*
*& 合并规则
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_ucomm_hb .
  DATA:lv_zposnr    TYPE ztvat_data-zposnr,
       gt_data_null TYPE TABLE OF ty_data.
  DATA:lv_netwr   TYPE vbap-netwr,
       lv_zbeizhu TYPE ztvat_data-zbeizhu,
       lv_amount  TYPE vbap-netwr,
       lv_se      TYPE vbap-netwr,
       lv_zkpxe   TYPE ztvat_rule-zkpxe.
  DATA:lv_menge TYPE mseg-menge.
  DATA:n TYPE i.
  DATA:gt_data_hb TYPE TABLE OF ty_data.
  DATA:gt_data_temp TYPE TABLE OF ty_data.
  DATA:lv_flag_r  TYPE abap_bool.    "判断退货是否选择正向
  DATA:tx_flag    TYPE abap_bool.    "存在贴息 falg
  DATA:lv_numb_zx TYPE i.            "正向发票数量
  DATA:ls_tabix TYPE  sy-tabix.
  DATA:lv_vbeln TYPE vbrk-vbeln.  "单开超过8行时，记录发票号

  CLEAR lv_numb_zx.

  CLEAR:gt_data_null,gt_data_nm,lt_tabix,gt_data_hb.
  LOOP AT gt_data INTO wa_data.
    MOVE-CORRESPONDING wa_data TO wa_data_nm.
    IF wa_data_nm-box = 'X'.
      APPEND wa_data_nm TO gt_data_nm.
    ENDIF.

    IF wa_data_nm-box = ''.
      APPEND wa_data TO gt_data_null.
    ENDIF.
    CLEAR:wa_data_nm,wa_data.
  ENDLOOP.

  "选择一条发票 就选中所有的
  LOOP AT gt_data_nm INTO wa_data_nm.
    LOOP AT gt_data_null INTO wa_data WHERE vbeln = wa_data_nm-vbeln.
      wa_data_nm = CORRESPONDING #( wa_data ).
      APPEND wa_data_nm TO gt_data_nm.
      DELETE gt_data_null.
    ENDLOOP.
  ENDLOOP.
*--合并
  LOOP AT gt_data_nm INTO wa_data.
    CASE wa_data-fkart.
      WHEN 'ZIG' OR 'ZIVG' OR 'ZRE' OR 'ZREA' OR 'ZG2' .
        lv_flag_r = abap_true.
      WHEN OTHERS.
        AT END OF vbeln.
          ADD 1 TO lv_numb_zx.
        ENDAT.
    ENDCASE.
    IF wa_data-matnr = g_tx_matnr AND wa_data-fkart NE 'ZIG' AND
                                      wa_data-fkart NE 'ZIVG' AND
                                      wa_data-fkart NE 'ZRE' AND
                                      wa_data-fkart NE 'ZREA' AND
                                      wa_data-fkart NE 'ZG2' .
      tx_flag = abap_true.
    ENDIF.
  ENDLOOP.

  IF lv_flag_r = abap_true AND lv_numb_zx IS INITIAL.
    MESSAGE s005(zjs001) DISPLAY LIKE 'E'."所选数据包括退货、贷项发票数据，请注意选择正向合并发票数据
    EXIT.
  ENDIF.

  IF lv_flag_r = abap_true AND tx_flag = abap_true.
    MESSAGE s025(zjs001) DISPLAY LIKE 'E'."所选数据包含贴息发票数据，请勿选择退货合并
    EXIT.
  ENDIF.

  IF tx_flag = abap_true AND lv_flag_r = abap_false.

    gt_data_temp = gt_data_nm.
    SORT gt_data_temp BY ztax.
    DELETE ADJACENT DUPLICATES FROM gt_data_temp COMPARING ztax.
    DATA(tax_line) = lines( gt_data_temp ).
    IF tax_line > 1.
      MESSAGE s018(zjs001) DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDIF.

  "如果是退货
  CLEAR:lv_netwr,lv_menge,gt_data,lv_amount.
  IF lv_flag_r = abap_true.
    LOOP AT gt_data_nm INTO wa_data_nm.
      IF wa_data_nm-zbmount < 0.
        wa_data_nm-fkimg = - wa_data_nm-fkimg.
      ENDIF.
      ADD wa_data_nm-zbmount TO lv_netwr.
      ADD wa_data_nm-fkimg   TO lv_menge.
      MODIFY gt_data_nm FROM wa_data_nm TRANSPORTING fkimg.
    ENDLOOP.
    IF lv_netwr <= 0 OR lv_menge <= 0.  "不满足退货条件
      MESSAGE s015(zjs001) DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    SORT gt_data_nm BY bukrs kunrg ztax zbmount DESCENDING zbklas zprice zwmfph DESCENDING.
    CLEAR:lv_netwr,lv_menge,lv_amount,lv_vbeln.

    LOOP AT gt_data_nm INTO wa_data_nm WHERE zidnum IS INITIAL.
      ls_tabix = sy-tabix.
      IF n > 8. "如果行数大于8
        PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.

        LOOP AT lt_tabix INTO DATA(ls_tabix_temp).
          MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
        ENDLOOP.
        CLEAR:gt_data_hb, lv_netwr,lt_tabix,lv_amount,wa_data_nm-zidnum,n.
        n = 1.
      ENDIF.

      CASE wa_data_nm-vtweg.
        WHEN '10'."内贸
          IF wa_data_nm-bukrs = '3500'.
            lv_zkpxe = '350000'.
          ELSE.
            lv_zkpxe = '10000000'.
          ENDIF.
        WHEN '20'."外贸
          lv_zkpxe = '9999999999999.99'."无限额
        WHEN OTHERS.
      ENDCASE.

      READ TABLE gt_rule INTO DATA(ls_rule) WITH KEY kunrg = wa_data_nm-kunrg bukrs = wa_data_nm-bukrs BINARY SEARCH.
      IF sy-subrc = 0."有特殊逻辑
        IF ls_rule-zkpxe IS NOT INITIAL.
          lv_zkpxe = ls_rule-zkpxe."特殊规则限额
        ENDIF.
      ENDIF.

      "如果遇到不同客户 或者不同税率
      READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                            bukrs = wa_data_nm-bukrs.
      IF sy-subrc NE 0 AND gt_data_hb IS NOT INITIAL.
        PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
        LOOP AT lt_tabix INTO ls_tabix_temp.
          MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
        ENDLOOP.
        CLEAR:gt_data_hb, lv_netwr,lt_tabix,lv_amount,wa_data_nm-zidnum,n.
      ENDIF.

      IF wa_data_nm-zbmount < 0.  "如果是退货订单
        DATA(lv_end_line) = lines( gt_data_hb ).
        IF lv_end_line = 0.
          wa_data_nm-msg = '没有正向发票参与合并'.
          MODIFY gt_data_nm FROM wa_data_nm.
          CONTINUE.
        ENDIF.
        READ TABLE gt_data_hb INTO DATA(gs_data_hb) INDEX lv_end_line.   " 与最后一行合并
*            IF SY-SUBRC = 0.
        ADD wa_data_nm-zbmount TO lv_netwr.
        ADD wa_data_nm-zmount  TO lv_amount.
        ADD wa_data_nm-fkimg   TO gs_data_hb-fkimg.
        ADD wa_data_nm-zbmount TO gs_data_hb-zbmount.
        ADD wa_data_nm-zmount  TO gs_data_hb-zmount.
        ADD wa_data_nm-zse     TO gs_data_hb-zse.
*        IF wa_data_nm-vtweg = '10'.
*          gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ wa_data_nm-bstkd }|.
*        ENDIF.

        WHILE gs_data_hb-zbmount <= 0  OR gs_data_hb-fkimg <= 0.
          DELETE gt_data_hb INDEX lv_end_line.
          SUBTRACT 1 FROM lv_end_line.
          IF lv_end_line = 0.
            EXIT.
          ENDIF.
          READ TABLE gt_data_hb INTO DATA(gs_data_temp) INDEX lv_end_line.
          lv_netwr = gs_data_hb-zbmount.
          lv_menge = gs_data_hb-fkimg.
          lv_amount = gs_data_hb-zmount.
          lv_se     = gs_data_hb-zse.
          lv_zbeizhu = gs_data_hb-zbeizhu.
          gs_data_hb = CORRESPONDING #( gs_data_temp ).
          ADD lv_netwr TO gs_data_hb-zbmount.
          ADD lv_menge TO gs_data_hb-fkimg.
          ADD lv_amount TO gs_data_hb-zmount.
          ADD lv_se TO gs_data_hb-zse.
          IF gs_data_hb-vtweg = '10'.
            gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ lv_zbeizhu }|.
          ENDIF.
          CLEAR:lv_netwr,lv_menge,lv_se,lv_amount.
        ENDWHILE.
        IF lv_end_line = 0.
          wa_data_nm-msg = '没有正向发票参与合并'.
          MODIFY gt_data_nm FROM wa_data_nm.
          CONTINUE.
        ENDIF.
        MODIFY gt_data_hb FROM gs_data_hb INDEX lv_end_line.
        APPEND ls_tabix TO lt_tabix.
      ELSE.
        CASE wa_data_nm-vtweg.
          WHEN '10'.
            IF wa_data_nm-bukrs = '3500'.
              READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = wa_data_nm-kunrg
                                                             zggxh  = wa_data_nm-zggxh
                                                             atwrt  = wa_data_nm-atwrt
                                                             zprice = wa_data_nm-zprice
                                                             matnr  = wa_data_nm-matnr.
            ELSE.
              READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = wa_data_nm-kunrg
                                                             zggxh  = wa_data_nm-zggxh
                                                             matnr  = wa_data_nm-matnr.
            ENDIF.
          WHEN '20'.
            READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = wa_data_nm-kunrg
                                                           zwmfph = wa_data_nm-zwmfph
                                                           zggxh  = wa_data_nm-zggxh
                                                           atwrt  = wa_data_nm-atwrt
                                                           zprice = wa_data_nm-zprice
                                                           matnr  = wa_data_nm-matnr.
          WHEN OTHERS.
        ENDCASE.
        IF sy-subrc = 0.   "当存在相同客户相同物料时
          ADD wa_data_nm-zbmount TO lv_netwr.
          IF lv_netwr > lv_zkpxe.
            PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
            LOOP AT lt_tabix INTO ls_tabix_temp.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
            CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
            ADD wa_data_nm-zbmount TO lv_netwr.
            APPEND wa_data_nm TO gt_data_hb.
            APPEND ls_tabix TO lt_tabix.
            n = 1.
          ELSE.
            ADD wa_data_nm-fkimg   TO gs_data_hb-fkimg.
            ADD wa_data_nm-zbmount TO gs_data_hb-zbmount.
            ADD wa_data_nm-zmount  TO gs_data_hb-zmount.
            ADD wa_data_nm-zse     TO gs_data_hb-zse.
*            IF wa_data_nm-vtweg = '10'.
*              gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ wa_data_nm-bstkd }|.
*            ENDIF.
            MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
            APPEND ls_tabix TO lt_tabix.
          ENDIF.

        ELSE.
          ADD wa_data_nm-zbmount TO lv_netwr.
          IF lv_netwr > lv_zkpxe.
            PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
            LOOP AT lt_tabix INTO ls_tabix_temp.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
            CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
            ADD wa_data_nm-zbmount TO lv_netwr.
            APPEND wa_data_nm TO gt_data_hb.
            APPEND ls_tabix TO lt_tabix.
            n = 1.
          ELSE.
            APPEND wa_data_nm TO gt_data_hb.
            APPEND ls_tabix TO lt_tabix.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF gt_data_hb IS NOT INITIAL.
      PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
      LOOP AT lt_tabix INTO ls_tabix_temp.
        MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
      ENDLOOP.
      CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
    ENDIF.
  ENDIF.

  CLEAR:lv_netwr,lv_zposnr,lv_vbeln.

  PERFORM zc_hb.

  APPEND LINES OF gt_data_null TO gt_data.
  SORT gt_data BY bukrs kunrg arktx zggxh atwrt zbklas ztax zprice matnr vbeln.





ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GETDATA_CXSJ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_getdata_cxsj .
  SELECT
    ztvat_data~zidnum
    ztvat_data~zstatus
    ztvat_data~zsuoyin
    ztvat_data~zjszl
    ztvat_data~zkpjh
    ztvat_data~zposnr
    ztvat_data~bukrs
    ztvat_data~vkorg
    ztvat_data~vtweg
    ztvat_data~kunrg
    ztvat_data~zkhname
    ztvat_data~zkhsh
    ztvat_data~zaddress
    ztvat_data~zkhbank
    ztvat_data~ernam
    ztvat_data~zsaler
    ztvat_data~zwmfph
    ztvat_data~matnr
    ztvat_data~bklas
    ztvat_data~arktx
    ztvat_data~zspmc
    ztvat_data~zggxh
    ztvat_data~atwrt
    ztvat_data~fkimg
    ztvat_data~vrkme
    ztvat_data~zprice
    ztvat_data~zbprice
    ztvat_data~zmount
    ztvat_data~zbmount
    ztvat_data~zse
    ztvat_data~kursk
    ztvat_data~waerk
    ztvat_data~zwaerk
    ztvat_data~ztax
    ztvat_data~zbeizhu
    ztvat_data~zssflbm
    ztvat_data~bezei
    ztvat_data~zzss
*    ZTVAT_DATA~ZTHCD
*    ZTVAT_DATA~ZTXBS
    INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM ztvat_data
    WHERE zidnum IN s_zid3
      AND bukrs IN s_bukrs3
      AND vtweg = p_vtweg3
      AND zsaler IN s_zsale3
      AND kunrg IN s_kunrg3
      AND zstatus IN ( '10','20' )
      AND zwmfph IN s_wmfph3.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DISPLAY_CXSJ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_cxsj .

  PERFORM frm_layout_cxsj.             "设置输出格式
  PERFORM frm_fieldcat_cxsj.
  PERFORM frm_output_cxsj.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_LAYOUT_CXSJ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_layout_cxsj .
**layout
  wa_layout-box_fname = 'BOX'.
  wa_layout-cwidth_opt = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_FIELDCAT_CXSJ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_fieldcat_cxsj .


  set_fieldcat 'ZIDNUM' '样票号码' '' '' '' ''.
  set_fieldcat 'ZSTATUS' '状态' '' '' '' ''.
  set_fieldcat 'ZSUOYIN' '索引' '' '' '' ''.
  set_fieldcat 'ZJSZL'   '发票种类' '' '' '' ''.
  set_fieldcat 'ZKPJH' '开票机号' '' '' '' ''.
  set_fieldcat 'ZPOSNR' '样票行项目' '' '' '' ''.
  set_fieldcat 'BUKRS' '公司代码' '' '' '' ''.
*  SET_FIELDCAT 'FKDAT' '发票日期' '' '' '' ''.
*  SET_FIELDCAT 'FKART' '发票类型' '' '' '' ''.
  set_fieldcat 'VKORG' '销售组织' '' '' '' ''.
  set_fieldcat 'VTWEG' '分销渠道' '' '' '' ''.
  set_fieldcat 'KUNRG' '客户' '' '' '' ''.
  set_fieldcat 'ZKHNAME' '客户名称' '' '' '' ''.
  set_fieldcat 'ZKHSH' '客户税号' '' '' '' ''.
  set_fieldcat 'ZADDRESS' '客户地址电话' '' '' '' ''.
  set_fieldcat 'ZKHBANK' '客户开户行及账号' '' '' '' ''.
  set_fieldcat 'ERNAM' '开票员' '' '' '' ''.
  set_fieldcat 'ZSALER' '销售员' '' '' '' ''.
  set_fieldcat 'ZWMFPH' '外贸发票号' '' '' '' ''.
  set_fieldcat 'MATNR' '物料编码' '' '' '' ''.
  set_fieldcat 'BKLAS' '评估类' '' '' '' ''.
  set_fieldcat 'ARKTX' '物料描述' '' '' '' ''.
  set_fieldcat 'ZSPMC' '商品名称' '' '' '' ''.
  set_fieldcat 'ZGGXH' '规格型号' '' '' '' ''.
  set_fieldcat 'ATWRT' '等级' '' '' '' ''.
  set_fieldcat 'FKIMG' '数量' '' '' '' ''.
  set_fieldcat 'VRKME' '计量单位' '' '' '' ''.
  set_fieldcat 'ZPRICE' '原币含税单价' '' '' '' ''.
  set_fieldcat 'ZBPRICE' '本位币不含税单价' '' '' '' ''.
  set_fieldcat 'ZMOUNT' '金额' '' '' '' ''.
  set_fieldcat 'ZBMOUNT' '本位币金额' '' '' '' ''.
  set_fieldcat 'ZSE' '税额' '' '' '' ''.
  set_fieldcat 'KURSK' '汇率' '' '' '' ''.
  set_fieldcat 'WAERK' '币种' '' '' '' ''.
  set_fieldcat 'ZWAERK' '本位币种' '' '' '' ''.
  set_fieldcat 'ZTAX' '税率' '' '' '' ''.
  set_fieldcat 'ZBEIZHU' '备注' '' '' '' ''.
  set_fieldcat 'ZSSFLBM' '税收分类编码' '' '' '' ''.
  set_fieldcat 'BEZEI' '贸易方式' '' '' '' ''.
  set_fieldcat 'ZZSS' '再生丝标识' '' '' '' ''.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_OUTPUT_CXSJ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_output_cxsj .

  gs_variant-handle = '1'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      is_layout_lvc            = wa_layout
      it_fieldcat_lvc          = gt_fieldcat[]
*     i_grid_title             = p_title
      i_grid_settings          = gs_grid_settings
      i_callback_pf_status_set = 'ALV_PF'
      i_callback_user_command  = 'USER_COMMAND_CXSJ'
      is_variant               = gs_variant
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PF
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM alv_pf USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_CXSJ'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  frm_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command_cxsj  USING r_ucomm LIKE sy-ucomm                      "FRM_USER_COMMAND
                              rs_selfield TYPE slis_selfield.

  DATA:n       TYPE i,
       m       TYPE i,
       ls_data TYPE ty_data.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = l_grid.

  CALL METHOD l_grid->check_changed_data.
  rs_selfield-refresh = 'X'.


  SORT gt_data BY zidnum kunrg matnr .

  CLEAR:ls_data,n,m,wa_data.
  CASE r_ucomm.
    WHEN 'CXYCL'."撤销已处理
*---校验
      LOOP AT gt_data INTO wa_data WHERE box = 'X'.
        IF wa_data-zstatus = '10'.
          MESSAGE e007(zjs001)."勾选项中存在状态为处理中的数据，请注意筛选
        ENDIF.

        READ TABLE gt_data TRANSPORTING NO FIELDS WITH KEY box = '' zidnum = wa_data-zidnum.
        IF sy-subrc = 0.
          MESSAGE e008(zjs001)."同一个样票数据存在未选中全部条目的情况，请勾选完整
        ENDIF.

      ENDLOOP.
*--撤销
      CLEAR:gt_ztvat_data,wa_ztvat_data.
      LOOP AT gt_data INTO wa_data WHERE box = 'X'.
        MOVE-CORRESPONDING wa_data TO wa_ztvat_data.
        wa_ztvat_data-zstatus = '10'.
        wa_ztvat_data-zaenam = sy-uname.
        wa_ztvat_data-zaedat = sy-datum.
        wa_ztvat_data-zaetim = sy-uzeit.

        APPEND wa_ztvat_data TO gt_ztvat_data.
        wa_data-zstatus = '10'.
        MODIFY  gt_data FROM wa_data TRANSPORTING zstatus.

      ENDLOOP.
      MODIFY ztvat_data FROM TABLE gt_ztvat_data.
      IF sy-subrc = 0.
        COMMIT WORK.
        MESSAGE s010(zjs001)."更新成功
      ELSE.
        ROLLBACK WORK.
        MESSAGE e009(zjs001)."更新失败
      ENDIF.
    WHEN 'CXHBCF'."撤销合并拆分
*---校验
      LOOP AT gt_data INTO wa_data WHERE box = 'X'.
        IF wa_data-zstatus = '20'.
          MESSAGE e020(zjs001)."勾选项中存在状态为处理中的数据，请注意筛选
        ENDIF.

        READ TABLE gt_data TRANSPORTING NO FIELDS WITH KEY box = '' zidnum = wa_data-zidnum.
        IF sy-subrc = 0.
          MESSAGE e008(zjs001)."同一个样票数据存在未选中全部条目的情况，请勾选完整
        ENDIF.
      ENDLOOP.
*--撤销
      CLEAR:gt_ztvat_data,wa_ztvat_data.
      LOOP AT gt_data INTO wa_data WHERE box = 'X'.
        MOVE-CORRESPONDING wa_data TO wa_ztvat_data.
        APPEND wa_ztvat_data TO gt_ztvat_data.
        DELETE gt_data.
      ENDLOOP.

      IF gt_ztvat_data IS NOT INITIAL.
        SELECT * INTO TABLE @DATA(lt_ztvat_invoice) FROM ztvat_invoice
        FOR ALL ENTRIES IN @gt_ztvat_data
        WHERE zidnum = @gt_ztvat_data-zidnum.
      ENDIF.


      DELETE ztvat_data FROM TABLE gt_ztvat_data.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

      DELETE ztvat_invoice  FROM TABLE lt_ztvat_invoice.
      IF sy-subrc = 0.
        COMMIT WORK.
        MESSAGE s010(zjs001)."处理成功
      ELSE.
        ROLLBACK WORK.
        MESSAGE e009(zjs001)."处理失败
      ENDIF.

    WHEN OTHERS.
  ENDCASE.






ENDFORM.                    "USER_COMMAND_HEAD     "
*&---------------------------------------------------------------------*
*& Form FRM_GETDATA_JSBB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_getdata_jsbb .
  SELECT
    ztvat_invoice~zidnum
    ztvat_invoice~vbeln
    fkart
    zjsdm
    zjshm
    zjsdate
    zpost
    ztvat_invoice~zerdat
    ztvat_invoice~zertim
    ztvat_invoice~zernam
    ztvat_invoice~zaedat
    ztvat_invoice~zaetim
    ztvat_invoice~zaenam
    INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM ztvat_invoice
    JOIN vbrk
    ON ztvat_invoice~vbeln = vbrk~vbeln
    WHERE ztvat_invoice~zidnum IN s_zid4
      AND ztvat_invoice~vbeln  IN s_vbeln4
      AND zjsdm  IN s_zjsdm4
      AND zjshm  IN s_zjshm4
      AND zjsdate IN s_zdate4.

  IF gt_data IS NOT INITIAL.
    SELECT zidnum,
           zstatus
    INTO TABLE @DATA(lt_ztvat_data)
    FROM ztvat_data
    FOR ALL ENTRIES IN @gt_data
    WHERE zidnum = @gt_data-zidnum.

    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
      READ TABLE lt_ztvat_data INTO DATA(ls_ztvat_data) WITH KEY zidnum = <fs_data>-zidnum.
      IF sy-subrc = 0.
        <fs_data>-zstatus = ls_ztvat_data-zstatus.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF p_gl = 'X'.
    LOOP AT gt_data INTO wa_data.
      IF wa_data-zjsdm IS INITIAL OR wa_data-zjshm IS INITIAL.
        DELETE gt_data.
      ENDIF.
    ENDLOOP.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DISPLAY_JSBB
*&---------------------------------------------------------------------*
*& 金税发票对应报表
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_jsbb .
  PERFORM frm_layout_jsbb.             "设置输出格式
  PERFORM frm_fieldcat_jsbb.
  PERFORM frm_output_jsbb.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_LAYOUT_JSBB
*&---------------------------------------------------------------------*
*& 金税发票对应报表
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_layout_jsbb .
**layout
  wa_layout-box_fname = 'BOX'.
  wa_layout-cwidth_opt = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_FIELDCAT_JSBB
*&---------------------------------------------------------------------*
*& 金税发票对应报表
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_fieldcat_jsbb .

  set_fieldcat 'ZIDNUM' '样票号码' '' '' '' ''.
  set_fieldcat 'VBELN' 'SAP发票号' '' '' '' ''.
  set_fieldcat 'FKART' '发票类型' '' '' '' ''.
  set_fieldcat 'ZJSDM'   '金税代码' '' '' '' ''.
  set_fieldcat 'ZJSHM' '金税号码' '' '' '' ''.
  set_fieldcat 'ZJSDATE' '金税日期' '' '' '' ''.
  set_fieldcat 'ZPOST' '已过账' '' '' '' ''.
  set_fieldcat 'ZERDAT' '创建日期' '' '' '' ''.
  set_fieldcat 'ZERTIM' '创建时间' '' '' '' ''.
  set_fieldcat 'ZERNAM' '创建者' '' '' '' ''.
  set_fieldcat 'ZAEDAT' '修改日期' '' '' '' ''.
  set_fieldcat 'ZAETIM' '修改时间' '' '' '' ''.
  set_fieldcat 'ZAENAM' '修改者' '' '' '' ''.
  set_fieldcat 'ZSTATUS' '状态' '' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_OUTPUT_JSBB
*&---------------------------------------------------------------------*
*& 金税发票对应报表
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_output_jsbb .
  gs_variant-handle = '1'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
      is_layout_lvc      = wa_layout
      it_fieldcat_lvc    = gt_fieldcat[]
*     i_grid_title       = p_title
      i_grid_settings    = gs_grid_settings
*     I_CALLBACK_PF_STATUS_SET = 'ALV_PF'
*     I_CALLBACK_USER_COMMAND  = 'USER_COMMAND_CXSJ'
      is_variant         = gs_variant
      i_save             = 'A'
    TABLES
      t_outtab           = gt_data
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GETDATA_SJCL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_getdata_sjcl .
  SELECT *
*    zidnum
*    zstatus
*    zsuoyin
*    zjszl
*    zkpjh
*    zposnr
*    bukrs
*    vkorg
*    vtweg
*    kunrg
*    zkhname
*    zkhsh
*    zaddress
*    zkhbank
*    ernam
*    zsaler
*    zwmfph
*    matnr
*    bklas
*    arktx
*    zspmc
*    zggxh
*    atwrt
*    fkimg
*    vrkme
*    zprice
*    zbprice
*    zmount
*    zbmount
*    zse
*    kursk
*    waerk
*    zwaerk
*    ztax
*    zbeizhu
*    zssflbm
*    bezei
*    zzss
    INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM ztvat_data
    WHERE zstatus = '10'
      AND vtweg  = p_vtweg2
      AND zidnum IN s_zid2
      AND bukrs  IN s_bukrs2
      AND zsaler IN s_zsale2
      AND kunrg  IN s_kunrg2
      AND zwmfph IN s_wmfph2.

  SORT gt_data BY zidnum zstatus zsuoyin.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DISPLAY_SJCL
*&---------------------------------------------------------------------*
*& 数据处理
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_sjcl .
  PERFORM frm_layout_sjcl.             "设置输出格式
  PERFORM frm_fieldcat_sjcl.
  PERFORM frm_creat_dropdown_values.
  PERFORM frm_creat_event_exits.
  PERFORM frm_output_sjcl.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_LAYOUT_SJCL
*&---------------------------------------------------------------------*
*& 数据处理
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_layout_sjcl .
**layout
  wa_layout-box_fname = 'BOX'.
  wa_layout-cwidth_opt = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_FIELDCAT_SJCL
*&---------------------------------------------------------------------*
*& 数据处理
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_fieldcat_sjcl .

  set_fieldcat 'ZIDNUM' '样票号码' '' '' '' ''.
  set_fieldcat 'ZSTATUS' '状态' '' '' '' ''.
  set_fieldcat 'ZSUOYIN' '索引' '' '' '' ''.

  gt_fieldcat-fieldname  = 'ZJSZL'.
  gt_fieldcat-coltext    = '发票种类'.
  gt_fieldcat-drdn_field = 'DD_HANDLE'.
  gt_fieldcat-edit       = 'X'.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.

  gt_fieldcat-fieldname  = 'ZKPJH'.
  gt_fieldcat-coltext    = '开票机号'.
  gt_fieldcat-drdn_field = 'DD_KPJH'.
  gt_fieldcat-edit       = 'X'.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.

  set_fieldcat 'ZPOSNR' '样票行项目' '' '' 'X' ''.
  set_fieldcat 'BUKRS' '公司代码' '' '' '' ''.
  set_fieldcat 'VKORG' '销售组织' '' '' '' ''.
  set_fieldcat 'VTWEG' '分销渠道' '' '' '' ''.
  set_fieldcat 'KUNRG' '客户' '' '' '' ''.
  set_fieldcat 'ZKHNAME' '客户名称' '' '' 'X' ''.
  set_fieldcat 'ZKHSH' '客户税号' '' '' 'X' ''.
  set_fieldcat 'ZADDRESS' '客户地址电话' '' '' 'X' ''.
  set_fieldcat 'ZKHBANK' '客户开户行及账号' '' '' 'X' ''.
  set_fieldcat 'ERNAM' '开票员' '' '' '' ''.
  set_fieldcat 'ZSALER' '销售员' '' '' '' ''.
  set_fieldcat 'ZWMFPH' '外贸发票号' '' '' '' ''.
  set_fieldcat 'MATNR' '物料编码' '' '' '' ''.
  set_fieldcat 'BKLAS' '评估类' '' '' '' ''.
  set_fieldcat 'ARKTX' '物料描述' '' '' '' ''.
  set_fieldcat 'ZSPMC' '商品名称' '' '' 'X' ''.
  set_fieldcat 'ZGGXH' '规格型号' '' '' 'X' ''.
  set_fieldcat 'ATWRT' '等级' '' '' 'X' ''.
  set_fieldcat 'FKIMG' '数量' '' '' 'X' ''.
  set_fieldcat 'VRKME' '计量单位' '' '' 'X' ''.
  set_fieldcat 'ZPRICE' '原币含税单价' '' '' 'X' ''.
  set_fieldcat 'ZBPRICE' '本位币不含税单价' '' '' 'X' ''.
  set_fieldcat 'ZMOUNT' '金额' '' '' 'X' ''.
  set_fieldcat 'ZBMOUNT' '本位币金额' '' '' 'X' ''.
  set_fieldcat 'ZSE' '税额' '' '' 'X' ''.
  set_fieldcat 'KURSK' '汇率' '' '' '' ''.
  set_fieldcat 'WAERK' '币种' '' '' '' ''.
  set_fieldcat 'ZWAERK' '本位币种' '' '' '' ''.
  set_fieldcat 'ZTAX' '税率' '' '' '' ''.
  set_fieldcat 'ZBEIZHU' '备注' '' '' 'X' ''.
  set_fieldcat 'ZSSFLBM' '税收分类编码' '' '' 'X' ''.
  set_fieldcat 'BEZEI' '贸易方式' '' '' '' ''.
  set_fieldcat 'ZZSS' '再生丝标识' '' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_OUTPUT_SJCL
*&---------------------------------------------------------------------*
*& 数据处理
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_output_sjcl .
  gs_variant-handle = '1'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      is_layout_lvc            = wa_layout
      it_fieldcat_lvc          = gt_fieldcat[]
*     i_grid_title             = p_title
      i_grid_settings          = gs_grid_settings
      i_callback_pf_status_set = 'ALV_PF_SJCL'
      i_callback_user_command  = 'USER_COMMAND_SJCL'
      is_variant               = gs_variant
      it_events                = gt_events
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PF
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM alv_pf_sjcl USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ALV_PF_SJCL'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  frm_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command_sjcl  USING r_ucomm LIKE sy-ucomm                      "FRM_USER_COMMAND
                              rs_selfield TYPE slis_selfield.


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = l_grid.

  CALL METHOD l_grid->check_changed_data.
  rs_selfield-refresh = 'X'.

  DATA(lt_yfcl) = gt_data.
  DELETE lt_yfcl WHERE box NE 'X'.
  IF lt_yfcl IS INITIAL.
    MESSAGE '请选中数据' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CASE r_ucomm.
    WHEN 'CLWC'."处理完成
      PERFORM save_sjcl USING '2'.


    WHEN 'YFCL'."运费处理

      SORT lt_yfcl BY zidnum.
      DELETE ADJACENT DUPLICATES FROM lt_yfcl COMPARING zidnum.
      DATA(yf_line) = lines( lt_yfcl ).
      IF yf_line > 1.
        MESSAGE s017(zjs001) DISPLAY LIKE 'E'. "请勿同时选择多条不同样票数据进行运费分摊
        EXIT.
      ENDIF.

      DATA(ls_yfcl) = lt_yfcl[ 1 ].
      CLEAR:gs_yf_item,gt_yf_item.
      LOOP AT gt_data INTO DATA(gs_data) WHERE box = 'X'.
        IF gs_data-vtweg NE '20'.
          MESSAGE e019(zjs001).
        ENDIF.
        gs_yf_item = CORRESPONDING #( gs_data ).
        APPEND gs_yf_item TO gt_yf_item.
      ENDLOOP.

      LOOP AT gt_data INTO gs_data WHERE box = '' AND zidnum = gs_yf_item-zidnum.
        gs_yf_item = CORRESPONDING #( gs_data ).
        APPEND gs_yf_item TO gt_yf_item.
      ENDLOOP.

      "取价格贸易条款
      SELECT SINGLE inco1 INTO zsvat_yfcl_h-zjgtk
      FROM ztvat_invoice INNER JOIN vbrp ON ztvat_invoice~vbeln = vbrp~vbeln
                         INNER JOIN vbkd ON vbrp~aubel = vbkd~vbeln
      WHERE ztvat_invoice~zidnum = gs_data-zidnum.


      CALL SCREEN '9000' STARTING AT 20 1 ENDING AT 180 30.


    WHEN '&DATA_SAVE'.

      PERFORM save_sjcl USING '1'.

    WHEN OTHERS.
  ENDCASE.

  CALL METHOD l_grid->get_frontend_layout
    IMPORTING
      es_layout = wa_layout.

  CALL METHOD l_grid->set_frontend_layout
    EXPORTING
      is_layout = wa_layout.

  CALL METHOD l_grid->refresh_table_display.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CREAT_DROPDOWN_VALUES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_creat_dropdown_values .

  CLEAR gw_ddval.
  gw_ddval-handle = 1.
  gw_ddval-value = '普票'.
  APPEND gw_ddval TO gt_ddval.

  CLEAR gw_ddval.
  gw_ddval-handle = 1.
  gw_ddval-value = '专票'.
  APPEND gw_ddval TO gt_ddval.

  CLEAR gw_ddval.
  gw_ddval-handle = 2.
  gw_ddval-value = '100'.
  APPEND gw_ddval TO gt_ddval.

  CLEAR gw_ddval.
  gw_ddval-handle = 2.
  gw_ddval-value = '200'.
  APPEND gw_ddval TO gt_ddval.

  CLEAR gw_ddval.
  gw_ddval-handle = 2.
  gw_ddval-value = '300'.
  APPEND gw_ddval TO gt_ddval.



  LOOP AT gt_data INTO wa_data.
    wa_data-dd_handle = '1'.

    wa_data-dd_kpjh   = '2'.

    IF wa_data-zkpjh IS INITIAL.
      wa_data-zkpjh = '100'.
    ENDIF.

    MODIFY gt_data FROM wa_data.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CREAT_EVENT_EXITS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_creat_event_exits .
  gw_events-name = 'CALLER_EXIT'.
  gw_events-form = 'CALLER_EXIT'.
  APPEND gw_events TO gt_events.

  gw_events-name = slis_ev_data_changed.
  gw_events-form = 'ALV_DATA_CHANGED'.
  APPEND gw_events TO gt_events.


ENDFORM.
*---------------------------------------------------------------------*
*设置下拉列表，使Grid和内表能链接上
*---------------------------------------------------------------------*
FORM caller_exit USING ls_data TYPE slis_data_caller_exit.
  DATA: l_ref_alv TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = l_ref_alv.
  CALL METHOD l_ref_alv->set_drop_down_table
    EXPORTING
      it_drop_down = gt_ddval.
ENDFORM.                    "CALLER_EXIT
*&---------------------------------------------------------------------*
*& Form CREATE_ZIDNUM_LIST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DATA_HB
*&---------------------------------------------------------------------*
FORM create_zidnum_list  TABLES   pt_data_hb LIKE gt_data_nm
                                  pt_tabix   LIKE  lt_tabix
                         CHANGING VALUE(pv_zidnum).

  DATA:zs_flag  TYPE abap_bool.  "再生丝标识
  DATA:lv_posnr TYPE ztvat_data-zposnr.
  DATA:lv_beizhu   TYPE ztvat_data-zbeizhu.
  DATA:lv_zbeizhu  TYPE ztvat_data-zbeizhu.
  DATA:lv_zbeizhu1 TYPE ztvat_data-zbeizhu.
  DATA:lv_zsalers  TYPE ztvat_data-zbeizhu.
  DATA:lv_zsaler TYPE ztvat_data-zsaler.
  TYPES:BEGIN OF ly_beizhu,
          zsaler  TYPE ztvat_data-zsaler,
          aubel   TYPE vbrk-vbeln,
          zbeizhu TYPE ztvat_data-zbeizhu,
        END OF ly_beizhu.
  DATA:lt_beizhu TYPE TABLE OF ly_beizhu,
       ls_beizhu TYPE ly_beizhu.

  CLEAR:zs_flag,lv_posnr,lv_beizhu.
  READ TABLE pt_data_hb TRANSPORTING NO FIELDS WITH KEY zzss = abap_true.
  IF sy-subrc = 0.
    zs_flag = abap_true.
  ENDIF.

  READ TABLE pt_data_hb INDEX 1.
  IF pt_data_hb-bukrs = '3500' AND pt_data_hb-vtweg = '10'.
    CLEAR lt_beizhu.
    LOOP AT pt_tabix.
      READ TABLE gt_data_nm INTO gs_data_nm INDEX pt_tabix.
      MOVE-CORRESPONDING gs_data_nm TO ls_beizhu.
      APPEND ls_beizhu TO lt_beizhu.
    ENDLOOP.
*    lt_beizhu = CORRESPONDING #( pt_data_hb[] ).
    CLEAR:lv_zsalers,lv_zbeizhu1, lv_beizhu.
    SORT lt_beizhu BY zsaler zbeizhu.
    LOOP AT lt_beizhu INTO ls_beizhu.
      lv_zsaler = ls_beizhu-zsaler.
      lv_zbeizhu = ls_beizhu-zbeizhu.

      AT NEW aubel.
        lv_zbeizhu1 = |{ lv_zbeizhu1 },{ lv_zbeizhu }|.
      ENDAT.
*
*      AT END OF zsaler.
*        lv_zbeizhu1 = |{ lv_zsaler } { lv_zbeizhu1 }|.
*        lv_beizhu = lv_beizhu && lv_zbeizhu1.
*        CLEAR lv_zbeizhu1.
*      ENDAT.

      AT END OF zsaler.
        lv_zsalers = |{ lv_zsalers } { lv_zsaler }|.
      ENDAT.
    ENDLOOP.
    SHIFT lv_zbeizhu1 LEFT DELETING LEADING ','.
    SHIFT lv_zbeizhu1 RIGHT DELETING TRAILING ','.
    SHIFT lv_zsalers LEFT DELETING LEADING ' '.
    lv_beizhu =  |{ lv_zsalers } { lv_zbeizhu1 }|.
  ENDIF.

  CALL FUNCTION 'ZFM_JS_GETSERIALNUM' "样票号码
    EXPORTING
      object      = 'ZJSYP'
      nr_range_nr = '01'
    IMPORTING
      number      = lv_zidnum.


  wa_ztvat_invoice-zidnum = lv_zidnum.
  pv_zidnum = lv_zidnum.

  wa_ztvat_invoice-zernam = sy-uname.
  wa_ztvat_invoice-zerdat = sy-datum.
  wa_ztvat_invoice-zertim = sy-uzeit.
  IF pt_tabix[] IS NOT INITIAL.
    LOOP AT pt_tabix.
      READ TABLE gt_data_nm INTO gs_data_nm INDEX pt_tabix.
      wa_ztvat_invoice-vbeln  = gs_data_nm-vbeln.
      APPEND wa_ztvat_invoice TO gt_ztvat_invoice.
    ENDLOOP.
  ENDIF.
  CLEAR lv_posnr.
  LOOP AT pt_data_hb ASSIGNING FIELD-SYMBOL(<ls_data>).
    IF pt_tabix[] IS INITIAL.
      wa_ztvat_invoice-vbeln  = <ls_data>-vbeln.
      APPEND wa_ztvat_invoice TO gt_ztvat_invoice.
    ENDIF.


    CALL FUNCTION 'ZFM_JS_GETSERIALNUM'  "索引
      EXPORTING
        object      = 'ZJSSY'
        nr_range_nr = '01'
      IMPORTING
        number      = lv_zjssy.

    "写入  data 表
    ADD 10 TO lv_posnr.
    wa_ztvat_data-zidnum  = lv_zidnum.
    wa_ztvat_data-zsuoyin = lv_zjssy.
    wa_ztvat_data-matnr   = <ls_data>-matnr.
    wa_ztvat_data-zstatus = '10'."处理中
    wa_ztvat_data-zjszl   = <ls_data>-zjszl.
    wa_ztvat_data-zkpjh   = <ls_data>-zkpjh.
    wa_ztvat_data-zposnr  = lv_posnr.
    wa_ztvat_data-bukrs   = <ls_data>-bukrs.
    wa_ztvat_data-vkorg   = <ls_data>-vkorg.
    wa_ztvat_data-vtweg   = <ls_data>-vtweg.
    wa_ztvat_data-kunrg   = <ls_data>-kunrg.
    wa_ztvat_data-zkhname = <ls_data>-zkhname.
    wa_ztvat_data-zkhsh   = <ls_data>-zkhsh.
    wa_ztvat_data-zaddress = <ls_data>-zaddress.
    wa_ztvat_data-zkhbank  = <ls_data>-zbank.
    wa_ztvat_data-ernam    = <ls_data>-ernam.
    wa_ztvat_data-zsaler   = <ls_data>-zsaler.
    wa_ztvat_data-zwmfph   = <ls_data>-zwmfph.
    wa_ztvat_data-bklas    = <ls_data>-bklas.
    wa_ztvat_data-arktx    = <ls_data>-arktx.
    wa_ztvat_data-zspmc    = <ls_data>-zspmc.
    IF <ls_data>-atwrt IS INITIAL.
      wa_ztvat_data-zggxh    = <ls_data>-zggxh.
    ELSE.
      wa_ztvat_data-zggxh    = |{ <ls_data>-zggxh } { <ls_data>-atwrt }级|.
    ENDIF.

    wa_ztvat_data-atwrt    = <ls_data>-atwrt.
    wa_ztvat_data-fkimg    = <ls_data>-fkimg."数量
    wa_ztvat_data-vrkme    = <ls_data>-vrkme.
    wa_ztvat_data-zbprice   = <ls_data>-zbmount / <ls_data>-fkimg.
    wa_ztvat_data-zprice   = ( <ls_data>-zbmount + <ls_data>-zse ) / <ls_data>-fkimg.
    wa_ztvat_data-zmount   = <ls_data>-zmount.
    wa_ztvat_data-zbmount  = <ls_data>-zbmount."金额
    wa_ztvat_data-zse      = <ls_data>-zse. "税额
    wa_ztvat_data-kursk    = <ls_data>-kursk.
    wa_ztvat_data-waerk    = <ls_data>-waerk.
    wa_ztvat_data-zwaerk   = <ls_data>-zwaerk.
    wa_ztvat_data-ztax     = <ls_data>-ztax.
    wa_ztvat_data-zbeizhu  = lv_beizhu.
    wa_ztvat_data-zssflbm  = <ls_data>-zssflbm.
    wa_ztvat_data-zernam   = sy-uname.
    wa_ztvat_data-zerdat   = sy-datum.
    wa_ztvat_data-zertim   = sy-uzeit.
    wa_ztvat_data-bezei    = <ls_data>-bezei.
    IF zs_flag = abap_true.
      wa_ztvat_data-zzss   = abap_true.
    ENDIF.
    <ls_data>-zidnum = lv_zidnum.
    <ls_data>-zsuoyin = lv_zjssy.
    APPEND wa_ztvat_data TO gt_ztvat_data.
  ENDLOOP.
  CLEAR:wa_ztvat_invoice.


ENDFORM.

INCLUDE zjsrpt_004_status_9000o01.

INCLUDE zjsrpt_004_user_command_900i01.



*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
FORM alv_data_changed USING pel_data TYPE REF TO cl_alv_changed_data_protocol.
  DATA: l_name(20),ls_cells TYPE lvc_s_modi.
  FIELD-SYMBOLS <fs_value>.
  CLEAR gv_clsj_kpjh.
  LOOP AT pel_data->mt_mod_cells INTO ls_cells.
    IF ls_cells-fieldname = 'ZKPJH' .
      READ TABLE gt_data INTO gs_data_nm INDEX ls_cells-row_id.
      LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>) WHERE zidnum = gs_data_nm-zidnum.
        <fs_data>-zkpjh = ls_cells-value.
      ENDLOOP.
      gv_clsj_kpjh = abap_true.
    ELSEIF ls_cells-fieldname = 'ZJSZL'.
      READ TABLE gt_data INTO gs_data_nm INDEX ls_cells-row_id.
      LOOP AT gt_data ASSIGNING <fs_data> WHERE zidnum = gs_data_nm-zidnum.
        <fs_data>-zjszl = ls_cells-value.
      ENDLOOP.
      gv_clsj_kpjh = abap_true.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_SJCL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM save_sjcl  USING    VALUE(p_type).
  DATA:lt_ztvat_data LIKE TABLE OF ztvat_data WITH HEADER LINE.
  DATA:lt_ztvat_null LIKE TABLE OF ztvat_data WITH HEADER LINE.
  DATA:lt_ztvat_temp LIKE TABLE OF ztvat_data WITH HEADER LINE.

  CLEAR:lt_ztvat_data[],lt_ztvat_data.


*--更新自建表
  CLEAR:wa_ztvat_data,gt_ztvat_data,wa_data.

  LOOP AT gt_data INTO wa_data .
    MOVE-CORRESPONDING wa_data TO wa_ztvat_data.
    IF wa_data-box = 'X'.
      wa_ztvat_data-zaenam = sy-uname.
      wa_ztvat_data-zaedat = sy-datum.
      wa_ztvat_data-zaetim = sy-uzeit.
      APPEND wa_ztvat_data TO gt_ztvat_data.
    ELSE.
      wa_ztvat_data-zaenam = sy-uname.
      wa_ztvat_data-zaedat = sy-datum.
      wa_ztvat_data-zaetim = sy-uzeit.
      APPEND wa_ztvat_data TO lt_ztvat_null.
    ENDIF.

    CLEAR:wa_ztvat_data,wa_data.
  ENDLOOP.

  IF gv_clsj_kpjh = abap_true OR p_type = '2'.
    LOOP AT gt_ztvat_data INTO wa_ztvat_data.
      LOOP AT lt_ztvat_null INTO DATA(wa_ztvat_null) WHERE zidnum = wa_ztvat_data-zidnum.
        APPEND wa_ztvat_null TO lt_ztvat_temp.
        DELETE lt_ztvat_null.
        CONTINUE.
      ENDLOOP.
    ENDLOOP.

    SORT lt_ztvat_temp BY zidnum zsuoyin.
    DELETE ADJACENT DUPLICATES FROM lt_ztvat_temp COMPARING zidnum zsuoyin.
    APPEND LINES OF lt_ztvat_temp TO gt_ztvat_data.
  ENDIF.

  LOOP AT gt_ztvat_data INTO wa_ztvat_data .
*--校验关键字段是否为空
    IF wa_ztvat_data-zkhname IS INITIAL.
      MESSAGE e011(zjs001)."关键字段数据遗漏，请确认已维护
    ENDIF.
    IF wa_ztvat_data-zposnr IS INITIAL.
      MESSAGE e011(zjs001)."关键字段数据遗漏，请确认已维护
    ENDIF.
    IF wa_ztvat_data-zspmc IS INITIAL.
      MESSAGE e011(zjs001)."关键字段数据遗漏，请确认已维护
    ENDIF.
    IF wa_ztvat_data-vrkme IS INITIAL.
      MESSAGE e011(zjs001)."关键字段数据遗漏，请确认已维护
    ENDIF.
    IF wa_ztvat_data-fkimg IS INITIAL.
      MESSAGE e011(zjs001)."关键字段数据遗漏，请确认已维护
    ENDIF.
    IF wa_ztvat_data-zprice IS INITIAL.
      MESSAGE e011(zjs001)."关键字段数据遗漏，请确认已维护
    ENDIF.
    IF wa_ztvat_data-zbmount IS INITIAL.
      MESSAGE e011(zjs001)."关键字段数据遗漏，请确认已维护
    ENDIF.
    IF wa_ztvat_data-zjszl IS INITIAL.
      MESSAGE e011(zjs001)."关键字段数据遗漏，请确认已维护
    ENDIF.
    IF wa_ztvat_data-zkpjh IS INITIAL.
      MESSAGE e011(zjs001)."关键字段数据遗漏，请确认已维护
    ENDIF.
    IF wa_ztvat_data-zssflbm IS INITIAL.
      MESSAGE e011(zjs001)."关键字段数据遗漏，请确认已维护
    ENDIF.
  ENDLOOP.

  IF gt_ztvat_data IS NOT INITIAL.
    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_ztvat_data
    FROM ztvat_data
    FOR ALL ENTRIES IN gt_ztvat_data
    WHERE zidnum = gt_ztvat_data-zidnum.
*        CLEAR:GT_ZTVAT_DATA.
    LOOP AT lt_ztvat_data.
      READ TABLE gt_ztvat_data INTO wa_ztvat_data WITH KEY zidnum  = lt_ztvat_data-zidnum zsuoyin = lt_ztvat_data-zsuoyin.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING wa_ztvat_data TO lt_ztvat_data.
        IF p_type = '2'.
          lt_ztvat_data-zstatus = '20'.
        ENDIF.

        lt_ztvat_data-mandt  = sy-mandt.
        lt_ztvat_data-zaenam = sy-uname.
        lt_ztvat_data-zaedat = sy-datum.
        lt_ztvat_data-zaetim = sy-uzeit.
        MODIFY lt_ztvat_data." TRANSPORTING ZSTATUS ZAENAM ZAEDAT ZAETIM ZJSZL ZKPJH.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CLEAR:gt_ztvat_data.

  MODIFY ztvat_data FROM TABLE lt_ztvat_data .
  COMMIT WORK.
  IF sy-subrc = 0.
    MESSAGE s010(zjs001)."处理成功
    CLEAR gv_clsj_kpjh.
  ELSE.
    MESSAGE e009(zjs001)."处理失败
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZC_HB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zc_hb .
  DATA:gt_data_hb TYPE TABLE OF ty_data.

  DATA:n TYPE i.
  DATA:lv_netwr TYPE vbap-netwr.
  DATA:ls_tabix TYPE  sy-tabix.
  DATA:lv_zkpxe TYPE ztvat_rule-zkpxe.
  DATA:lt_data_temp TYPE TABLE OF ty_data.

  SORT gt_data_nm BY bukrs kunrg tx_flag ztax zbklas zwmfph zprice  zggxh atwrt zspmc.

  lt_data_temp = CORRESPONDING #( gt_data_nm ).

  LOOP AT gt_data_nm INTO wa_data_nm WHERE zidnum IS INITIAL AND msg IS INITIAL.
    ls_tabix = sy-tabix.

    CASE wa_data_nm-vtweg.
      WHEN '10'."内贸
        IF wa_data_nm-bukrs = '3500'.
          lv_zkpxe = '350000'.
        ELSE.
          lv_zkpxe = '10000000'.
        ENDIF.
      WHEN '20'."外贸
        lv_zkpxe = '9999999999999.99'."无限额
      WHEN OTHERS.
    ENDCASE.

    IF wa_data_nm-zbmount > lv_zkpxe."单个SAP发票数据净价值超过限额
      IF gt_data_hb IS NOT INITIAL.
        PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
        LOOP AT lt_tabix INTO DATA(ls_tabix_temp).
          MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
        ENDLOOP.
        CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
      ENDIF.
      PERFORM frm_create_zidnum USING '2' CHANGING wa_data_nm wa_data_nm-zbmount wa_data_nm-zidnum.
      CLEAR gt_data_hb.
      MODIFY gt_data_nm FROM wa_data_nm.
      n = 1.
      CONTINUE.
    ENDIF.

    READ TABLE gt_rule INTO DATA(ls_rule) WITH KEY kunrg = wa_data_nm-kunrg bukrs = wa_data_nm-bukrs BINARY SEARCH.
*--若贴息款单开和单开字段不为空,内外贸额度大于特殊规则额度

    IF sy-subrc = 0."有特殊逻辑
      IF ls_rule-zkpxe IS NOT INITIAL.
        lv_zkpxe = ls_rule-zkpxe."特殊规则限额
      ENDIF.

      IF ls_rule-zdk = 'X'."单开
        READ TABLE gt_data_hb INTO DATA(gs_data_hb) WITH KEY bukrs = wa_data_nm-bukrs
                                                             kunrg = wa_data_nm-kunrg
                                                             vbeln = wa_data_nm-vbeln.
        IF sy-subrc NE 0 AND gt_data_hb IS NOT INITIAL.
          PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
          LOOP AT lt_tabix INTO ls_tabix_temp.
            MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
          ENDLOOP.
          CLEAR:gt_data_hb, lv_netwr,lt_tabix,n,wa_data_nm-zidnum,n.
        ENDIF.

        IF wa_data_nm-vbeln_line > 8.
          CLEAR gt_data_hb.
          wa_data_nm-msg = '单开数据超过8行，请手工处理'.
          LOOP AT lt_data_temp INTO DATA(ls_data_temp) WHERE vbeln = wa_data_nm-vbeln.
            MODIFY gt_data_nm FROM wa_data_nm INDEX sy-tabix TRANSPORTING msg .
          ENDLOOP.
        ELSE.
          LOOP AT lt_data_temp INTO ls_data_temp WHERE vbeln = wa_data_nm-vbeln.
            APPEND sy-tabix TO lt_tabix.
            APPEND ls_data_temp TO gt_data_hb.
          ENDLOOP.
          PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
          LOOP AT lt_tabix INTO ls_tabix_temp.
            MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
          ENDLOOP.
          CLEAR:gt_data_hb, lv_netwr,lt_tabix,n,wa_data_nm-zidnum.
        ENDIF.
      ENDIF.

      IF ls_rule-ztxdk = 'X'.   "贴息单开
        IF wa_data_nm-matnr = g_tx_matnr.    "如果有贴息款 走单开，其他走正常逻辑

          READ TABLE gt_data_hb INTO gs_data_hb        WITH KEY bukrs = wa_data_nm-bukrs
                                                                kunrg = wa_data_nm-kunrg
                                                                matnr = wa_data_nm-matnr.
          IF sy-subrc NE 0 AND gt_data_hb IS NOT INITIAL.
            PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
            LOOP AT lt_tabix INTO ls_tabix_temp.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
            CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
            LOOP AT lt_data_temp INTO ls_data_temp WHERE vbeln = wa_data_nm-vbeln.
              ls_tabix_temp = sy-tabix.
              APPEND ls_tabix_temp TO lt_tabix.
              APPEND ls_data_temp TO gt_data_hb.
              wa_data_nm-zidnum = '1'.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
          ELSEIF sy-subrc = 0.
            ADD wa_data_nm-fkimg   TO gs_data_hb-fkimg.
            ADD wa_data_nm-zbmount TO gs_data_hb-zbmount.
            ADD wa_data_nm-zmount  TO gs_data_hb-zmount.
            ADD wa_data_nm-zse     TO gs_data_hb-zse.
*            IF wa_data_nm-vtweg = '10'.
*              gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ wa_data_nm-bstkd }|.
*            ENDIF.
            MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
            APPEND ls_tabix TO lt_tabix.
          ELSEIF sy-subrc NE 0 AND gt_data_hb IS INITIAL.
            APPEND wa_data_nm TO gt_data_hb.
            APPEND ls_tabix TO lt_tabix.
            ADD 1 TO n.
          ENDIF.
        ELSE.

          "如果遇到不同客户的情况
          CASE wa_data_nm-vtweg.
            WHEN '10'."内贸
              READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                                    bukrs = wa_data_nm-bukrs
                                                                    ztax = wa_data_nm-ztax
                                                                    zbklas = wa_data_nm-zbklas.
            WHEN '20'."外贸
              READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                                    bukrs = wa_data_nm-bukrs
                                                                    ztax = wa_data_nm-ztax
                                                                    zbklas = wa_data_nm-zbklas
                                                                    zwmfph = wa_data_nm-zwmfph.
            WHEN OTHERS.
          ENDCASE.
          IF sy-subrc NE 0 AND gt_data_hb IS NOT INITIAL.
            PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
            LOOP AT lt_tabix INTO ls_tabix_temp.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
            CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
          ENDIF.

          ADD wa_data_nm-netwr_k TO lv_netwr.
          ADD wa_data_nm-vbeln_line TO n.
          IF lv_netwr > lv_zkpxe OR n > 8.    "如果不能合到一张样票
            PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
            LOOP AT lt_tabix INTO ls_tabix_temp.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
            CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
            ADD wa_data_nm-netwr_k TO lv_netwr.
            LOOP AT lt_data_temp INTO ls_data_temp WHERE vbeln = wa_data_nm-vbeln.
              ls_tabix_temp = sy-tabix.
              APPEND ls_tabix_temp TO lt_tabix.
              APPEND ls_data_temp TO gt_data_hb.
              wa_data_nm-zidnum = '1'.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
            n = wa_data_nm-vbeln_line.
          ELSE.
            SUBTRACT wa_data_nm-vbeln_line FROM n.
            LOOP AT lt_data_temp INTO ls_data_temp WHERE vbeln = wa_data_nm-vbeln.
              ls_tabix = sy-tabix.
              CASE wa_data_nm-vtweg.
                WHEN '10'.
                  IF wa_data_nm-bukrs = '3500'.
                    READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                                   zggxh  = ls_data_temp-zggxh
                                                                   atwrt  = ls_data_temp-atwrt
                                                                   zprice = ls_data_temp-zprice
                                                                   zspmc  = ls_data_temp-zspmc.
                  ELSE.
                    READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                                   zspmc  = ls_data_temp-zspmc
                                                                   zggxh  = ls_data_temp-zggxh.
                  ENDIF.
                WHEN '20'.
                  READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                                 zwmfph = ls_data_temp-zwmfph
                                                                 zggxh  = ls_data_temp-zggxh
                                                                 atwrt  = ls_data_temp-atwrt
                                                                 zprice = ls_data_temp-zprice
                                                                 zspmc  = ls_data_temp-zspmc.
                WHEN OTHERS.
              ENDCASE.
              IF sy-subrc = 0.
                ADD ls_data_temp-fkimg   TO gs_data_hb-fkimg.
                ADD ls_data_temp-zbmount TO gs_data_hb-zbmount.
                ADD ls_data_temp-zmount  TO gs_data_hb-zmount.
                ADD ls_data_temp-zse     TO gs_data_hb-zse.
                IF ls_data_temp-vtweg = '10'.
                  gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ ls_data_temp-bstkd }|.
                ENDIF.
                MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
                APPEND ls_tabix TO lt_tabix.
                wa_data_nm-zidnum = '1'.
                MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix TRANSPORTING zidnum .
              ELSE.
                APPEND ls_tabix TO lt_tabix.
                APPEND ls_data_temp TO gt_data_hb.
                ADD 1 TO n.
                wa_data_nm-zidnum = '1'.
                MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix TRANSPORTING zidnum .
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ls_rule-ztxhbhk = 'X'.
        IF n > 7. "如果行数大于8
          PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
          LOOP AT lt_tabix INTO ls_tabix_temp.
            MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
          ENDLOOP.
          CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
          n = 1.
        ENDIF.

        "如果遇到不同客户的情况
        CASE wa_data_nm-vtweg.
          WHEN '10'."内贸
            IF wa_data_nm-matnr = g_tx_matnr.
              READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                                    bukrs = wa_data_nm-bukrs
                                                                    ztax = wa_data_nm-ztax.
            ELSE.
              READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                                    bukrs = wa_data_nm-bukrs
                                                                    ztax = wa_data_nm-ztax
                                                                    zbklas = wa_data_nm-zbklas.
            ENDIF.

          WHEN '20'."外贸
            IF wa_data_nm-matnr = g_tx_matnr.
              READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                                    bukrs = wa_data_nm-bukrs
                                                                    ztax = wa_data_nm-ztax
                                                                    zwmfph = wa_data_nm-zwmfph.
            ELSE.
              READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                                    bukrs = wa_data_nm-bukrs
                                                                    ztax = wa_data_nm-ztax
                                                                    zbklas = wa_data_nm-zbklas
                                                                    zwmfph = wa_data_nm-zwmfph.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
        IF sy-subrc NE 0 AND gt_data_hb IS NOT INITIAL.
          PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
          LOOP AT lt_tabix INTO ls_tabix_temp.
            MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
          ENDLOOP.
          CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
        ENDIF.
        IF wa_data_nm-matnr = g_tx_matnr.  "如果是贴息物料
          DATA(lv_end_line) = lines( gt_data_hb ).
          IF lv_end_line = 0.
            wa_data_nm-msg = |客户{ wa_data_nm-kunrg }没有满足贴息合并的数据|.
            MODIFY gt_data_nm FROM wa_data_nm.
            CONTINUE.
          ENDIF.

          READ TABLE gt_data_hb INTO gs_data_hb INDEX lv_end_line.   " 与最后一行合并
          IF gs_data_hb-bukrs NE wa_data_nm-bukrs OR gs_data_hb-kunrg NE wa_data_nm-kunrg.
            wa_data_nm-msg = |客户{ wa_data_nm-kunrg }没有满足贴息合并的数据|.
            MODIFY gt_data_nm FROM wa_data_nm.
            CONTINUE.
          ENDIF.
*            IF SY-SUBRC = 0.
*          ADD wa_data_nm-zbmount TO lv_netwr.
*          IF lv_netwr > lv_zkpxe.
*            ADD wa_data_nm-fkimg   TO gs_data_hb-fkimg.
*            ADD wa_data_nm-zbmount TO gs_data_hb-zbmount.
*            ADD wa_data_nm-zmount  TO gs_data_hb-zmount.
*            ADD wa_data_nm-zse     TO gs_data_hb-zse.
*            MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
*            APPEND ls_tabix TO lt_tabix.
*            PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
*            LOOP AT lt_tabix INTO ls_tabix_temp.
*              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
*            ENDLOOP.
*            CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
*            ADD wa_data_nm-zbmount TO lv_netwr.
**            APPEND wa_data_nm TO gt_data_hb.
**            APPEND ls_tabix TO lt_tabix.
*            n = 1.
*          ELSE.
          ADD wa_data_nm-fkimg   TO gs_data_hb-fkimg.
          ADD wa_data_nm-zbmount TO gs_data_hb-zbmount.
          ADD wa_data_nm-zmount  TO gs_data_hb-zmount.
          ADD wa_data_nm-zse     TO gs_data_hb-zse.
*            IF wa_data_nm-vtweg = '10'.
*              gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ wa_data_nm-bstkd }|.
*            ENDIF.
          MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
          APPEND ls_tabix TO lt_tabix.
*          ENDIF.
        ELSE.

          ADD wa_data_nm-netwr_k TO lv_netwr.
          ADD wa_data_nm-vbeln_line TO n.
          IF lv_netwr > lv_zkpxe OR n > 8.    "如果不能合到一张样票
            PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
            LOOP AT lt_tabix INTO ls_tabix_temp.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
            CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
            LOOP AT lt_data_temp INTO ls_data_temp WHERE vbeln = wa_data_nm-vbeln.
              ls_tabix_temp = sy-tabix.
              APPEND ls_tabix_temp TO lt_tabix.
              APPEND ls_data_temp TO gt_data_hb.
              wa_data_nm-zidnum = '1'.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
            ADD wa_data_nm-netwr_k TO lv_netwr.
            n = wa_data_nm-vbeln_line.
          ELSE.
            SUBTRACT wa_data_nm-vbeln_line FROM n.
            LOOP AT lt_data_temp INTO ls_data_temp WHERE vbeln = wa_data_nm-vbeln.
              ls_tabix = sy-tabix.
              CASE wa_data_nm-vtweg.
                WHEN '10'.
                  IF wa_data_nm-bukrs = '3500'.
                    READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                                   zggxh  = ls_data_temp-zggxh
                                                                   atwrt  = ls_data_temp-atwrt
                                                                   zprice = ls_data_temp-zprice
                                                                   zspmc  = ls_data_temp-zspmc.
                  ELSE.
                    READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                                   zspmc  = ls_data_temp-zspmc
                                                                   zggxh  = ls_data_temp-zggxh.
                  ENDIF.
                WHEN '20'.
                  READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                                 zwmfph = ls_data_temp-zwmfph
                                                                 zggxh  = ls_data_temp-zggxh
                                                                 atwrt  = ls_data_temp-atwrt
                                                                 zprice = ls_data_temp-zprice
                                                                 zspmc  = ls_data_temp-zspmc.
                WHEN OTHERS.
              ENDCASE.
              IF sy-subrc = 0.
                ADD ls_data_temp-fkimg   TO gs_data_hb-fkimg.
                ADD ls_data_temp-zbmount TO gs_data_hb-zbmount.
                ADD ls_data_temp-zmount  TO gs_data_hb-zmount.
                ADD ls_data_temp-zse     TO gs_data_hb-zse.
                IF ls_data_temp-vtweg = '10'.
                  gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ ls_data_temp-bstkd }|.
                ENDIF.
                MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
                APPEND ls_tabix TO lt_tabix.
                wa_data_nm-zidnum = '1'.
                MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix TRANSPORTING zidnum .
              ELSE.
                APPEND ls_tabix TO lt_tabix.
                APPEND ls_data_temp TO gt_data_hb.
                ADD 1 TO n.
                wa_data_nm-zidnum = '1'.
                MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix TRANSPORTING zidnum .
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    IF ls_rule-ztxdk = abap_false AND ls_rule-zdk = abap_false AND ls_rule-ztxhbhk = abap_false .



      IF n > 7. "如果行数大于8
        PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
        LOOP AT lt_tabix INTO ls_tabix_temp.
          MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
        ENDLOOP.
        CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
        n = 1.
      ENDIF.

      "如果遇到不同客户的情况
      CASE wa_data_nm-vtweg.
        WHEN '10'."内贸
          IF wa_data_nm-matnr = g_tx_matnr.
            READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                                  bukrs = wa_data_nm-bukrs
                                                                  ztax = wa_data_nm-ztax.
          ELSE.
            READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                    bukrs = wa_data_nm-bukrs
                                                    ztax = wa_data_nm-ztax
                                                    zbklas = wa_data_nm-zbklas.
          ENDIF.

        WHEN '20'."外贸
          IF wa_data_nm-matnr = g_tx_matnr.
            READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                                  bukrs = wa_data_nm-bukrs
                                                                  ztax = wa_data_nm-ztax
                                                                  zwmfph = wa_data_nm-zwmfph.
          ELSE.
            READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                      bukrs = wa_data_nm-bukrs
                                                      ztax = wa_data_nm-ztax
                                                      zbklas = wa_data_nm-zbklas
                                                      zwmfph = wa_data_nm-zwmfph.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
      IF sy-subrc NE 0 AND gt_data_hb IS NOT INITIAL.
        PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.

        LOOP AT lt_tabix INTO ls_tabix_temp.
          MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
        ENDLOOP.
        CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
      ENDIF.

      IF wa_data_nm-matnr = g_tx_matnr.  "如果是贴息物料
        lv_end_line = lines( gt_data_hb ).
        IF lv_end_line = 0.
          wa_data_nm-msg = |客户{ wa_data_nm-kunrg }没有满足贴息合并的数据|.
          MODIFY gt_data_nm FROM wa_data_nm.
          CONTINUE.
        ENDIF.
        READ TABLE gt_data_hb INTO gs_data_hb INDEX lv_end_line.   " 与最后一行合并
        IF gs_data_hb-bukrs NE wa_data_nm-bukrs OR gs_data_hb-kunrg NE wa_data_nm-kunrg.
          wa_data_nm-msg = |客户{ wa_data_nm-kunrg }没有满足贴息合并的数据|.
          MODIFY gt_data_nm FROM wa_data_nm.
          CONTINUE.
        ENDIF.
        IF  gt_data_hb IS NOT INITIAL.
          READ TABLE gt_data_hb INTO gs_data_hb WITH KEY matnr = wa_data_nm-matnr.
          IF sy-subrc = 0.    "如果合并内表已经存在 贴息物料
*            ADD wa_data_nm-zbmount TO lv_netwr.
*            IF lv_netwr > lv_zkpxe.
*              ADD wa_data_nm-fkimg   TO gs_data_hb-fkimg.
*              ADD wa_data_nm-zbmount TO gs_data_hb-zbmount.
*              ADD wa_data_nm-zmount  TO gs_data_hb-zmount.
*              ADD wa_data_nm-zse     TO gs_data_hb-zse.
*              MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
*              APPEND ls_tabix TO lt_tabix.
*              PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
*
*              LOOP AT lt_tabix INTO ls_tabix_temp.
*                MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
*              ENDLOOP.
*              CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum.
*              n = 1.
*            ELSE.
            ADD wa_data_nm-fkimg   TO gs_data_hb-fkimg.
            ADD wa_data_nm-zbmount TO gs_data_hb-zbmount.
            ADD wa_data_nm-zmount  TO gs_data_hb-zmount.
            ADD wa_data_nm-zse     TO gs_data_hb-zse.
*              IF wa_data_nm-vtweg = '10'.
*                gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ wa_data_nm-bstkd }|.
*              ENDIF.
            MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
            APPEND ls_tabix TO lt_tabix.
*            ENDIF.
          ELSE.
*            ADD 1 TO n.
            ADD wa_data_nm-zbmount TO lv_netwr.
            APPEND wa_data_nm TO gt_data_hb.
            APPEND ls_tabix TO lt_tabix.
          ENDIF.
          CONTINUE.
        ELSE.
          wa_data_nm-msg = '没有其他物料合并贴息款'.
          MODIFY gt_data_nm FROM wa_data_nm.
        ENDIF.

      ELSE.
        ADD wa_data_nm-netwr_k TO lv_netwr.
        ADD wa_data_nm-vbeln_line TO n.
        IF lv_netwr > lv_zkpxe OR n > 8.    "如果不能合到一张样票
          PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
          LOOP AT lt_tabix INTO ls_tabix_temp.
            MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
          ENDLOOP.
          CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum.
          LOOP AT lt_data_temp INTO ls_data_temp WHERE vbeln = wa_data_nm-vbeln.
            ls_tabix_temp = sy-tabix.
            APPEND ls_tabix_temp TO lt_tabix.
            APPEND ls_data_temp TO gt_data_hb.
            wa_data_nm-zidnum = '1'.
            MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
          ENDLOOP.
          ADD wa_data_nm-netwr_k TO lv_netwr.
          n = wa_data_nm-vbeln_line.
        ELSE.
          SUBTRACT wa_data_nm-vbeln_line FROM n.
          LOOP AT lt_data_temp INTO ls_data_temp WHERE vbeln = wa_data_nm-vbeln.
            ls_tabix = sy-tabix.
            CASE wa_data_nm-vtweg.
              WHEN '10'.
                IF wa_data_nm-bukrs = '3500'.
                  READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                                 zggxh  = ls_data_temp-zggxh
                                                                 atwrt  = ls_data_temp-atwrt
                                                                 zprice = ls_data_temp-zprice
                                                                 zspmc  = ls_data_temp-zspmc.
                ELSE.
                  READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                                 zspmc  = ls_data_temp-zspmc
                                                                 zggxh  = ls_data_temp-zggxh.
                ENDIF.
              WHEN '20'.
                READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                               zwmfph = ls_data_temp-zwmfph
                                                               zggxh  = ls_data_temp-zggxh
                                                               atwrt  = ls_data_temp-atwrt
                                                               zprice = ls_data_temp-zprice
                                                               zspmc  = ls_data_temp-zspmc.
              WHEN OTHERS.
            ENDCASE.
            IF sy-subrc = 0.
              ADD ls_data_temp-fkimg   TO gs_data_hb-fkimg.
              ADD ls_data_temp-zbmount TO gs_data_hb-zbmount.
              ADD ls_data_temp-zmount  TO gs_data_hb-zmount.
              ADD ls_data_temp-zse     TO gs_data_hb-zse.
              IF ls_data_temp-vtweg = '10'.
                gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ ls_data_temp-bstkd }|.
              ENDIF.
              MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
              APPEND ls_tabix TO lt_tabix.
              wa_data_nm-zidnum = '1'.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix TRANSPORTING zidnum .
            ELSE.
              APPEND ls_tabix TO lt_tabix.
              APPEND ls_data_temp TO gt_data_hb.
              ADD 1 TO n.
              wa_data_nm-zidnum = '1'.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix TRANSPORTING zidnum .
            ENDIF.
          ENDLOOP.
        ENDIF.

      ENDIF.

    ENDIF.
    CLEAR ls_rule.
  ENDLOOP.

  IF gt_data_hb IS NOT INITIAL.
    PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.

    LOOP AT lt_tabix INTO ls_tabix_temp.
      MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
    ENDLOOP.
    CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
  ENDIF.

  MODIFY ztvat_data FROM TABLE gt_ztvat_data.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
  MODIFY ztvat_invoice FROM TABLE gt_ztvat_invoice.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  CLEAR: gt_ztvat_data,gt_ztvat_invoice.

  LOOP AT gt_data_nm INTO wa_data_nm.
    MOVE-CORRESPONDING wa_data_nm TO  wa_data.
    APPEND wa_data TO gt_data.
    CLEAR:wa_data,wa_data_nm.
  ENDLOOP.

ENDFORM.

*GUI Texts
*----------------------------------------------------------
* TITLE_9000 --> 运费处理


*Selection texts
*----------------------------------------------------------
* P1         合并拆分
* P2         处理数据
* P3         撤销数据处理
* P4         金税发票对应报表
* P_GL         过滤未回传的样票数据
* P_VTWEG D       .
* P_VTWEG2 D       .
* P_VTWEG3 D       .
* P_ZSS         再生丝筛选勾选框
* S_BUKRS D       .
* S_BUKRS2 D       .
* S_BUKRS3 D       .
* S_FKART D       .
* S_FKDAT         发票日期
* S_KUNRG         客户
* S_KUNRG2         客户
* S_KUNRG3         客户
* S_VBELN         SAP发票号
* S_VBELN4         SAP发票号
* S_WMFPH2         外贸发票号
* S_WMFPH3         外贸发票号
* S_ZDATE4         金税日期
* S_ZID2         样票号码
* S_ZID3         样票号码
* S_ZID4         样票号码
* S_ZJSDM4         金税发票代码
* S_ZJSHM4         金税发票代码
* S_ZSALE         销售员
* S_ZSALE2         销售员
* S_ZSALE3         销售员


*Messages
*----------------------------------------------------------
*
* Message class: Hard coded
*   请选中数据
*
* Message class: ZJS001
*002   所选数据已生成样票号，请重新选择
*003   所选数据包括退货、贷项数据，系统不允许自动合并，请手工选择处理这部分数据
*004   客户税务信息维护不全，请联系人员维护
*005   所选数据包括退货、贷项发票数据，请注意选择正向合并发票数据
*007   勾选项中存在状态为处理中的数据，请注意筛选
*008   同一个样票数据存在未选中全部条目的情况，请勾选完整
*009   处理失败
*010   处理成功
*011   关键字段数据遗漏，请确认已维护
*014   选中数据不满足拆分条件，不允许拆分生成样票数据，请执行合并功能
*015   不满足退货合并条件，请确定合并后数量金额是否满足正数
*017   请勿同时选择多条不同样票数据进行运费分摊
*018   选中数据税率不一致，不允许进行合并
*019   非外贸业务，不允许进行运费分摊
*020   勾选项中存在状态为已处理的数据，请注意筛选
*022   退货及贷项发票数据不允许执行拆分
*025   所选数据包含贴息发票数据，请勿选择退货合并

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.1 - E.G.Mellodew. 1998-2019. Sap Release 752
