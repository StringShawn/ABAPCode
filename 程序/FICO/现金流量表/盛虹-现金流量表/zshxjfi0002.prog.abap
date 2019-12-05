**************************************************
*程序名称:现金流量表
*创建日期: 2019-11-14
*创建者:XXX
*申请者:XXX
*功能描述:
*============================================
*变更记录
*修改请求号    修改日期    修改人   修改描述
*DEVK911993    2019-11-14   HANDYXH    创建程序
***************************************************
REPORT zshxjfi0002 NO STANDARD PAGE HEADING MESSAGE-ID ZFISHXJ01.


TABLES:
  bseg,                                "会计核算凭证段
  faglflexa,                           "总帐: 实际行项目
  tfin011.                             "利润中心到利润中心组的映射

TYPE-POOLS: zfi01.

CONSTANTS: c_special_space  TYPE c VALUE '　'.

*CLASS lcl_event_handler DEFINITION DEFERRED.
*CLASS lcl_event_handler DEFINITION INHERITING FROM zcl_utils_alv_event_handler.
*  PUBLIC SECTION.
*
*  PROTECTED SECTION.
*
*  PRIVATE SECTION.
*ENDCLASS.
*
*CLASS lcl_event_handler IMPLEMENTATION.
*
*ENDCLASS.

*--------------------------------------------------------------------*
*& Global Variables Declaration
*--------------------------------------------------------------------*
DATA:
  gs_layout   TYPE lvc_s_layo,
  gt_fieldcat TYPE lvc_t_fcat,
  gt_event    TYPE slis_t_event,
  go_alv_grid TYPE REF TO cl_gui_alv_grid.
*  go_event_handler TYPE REF TO lcl_event_handler.
*---used for selection-screen
TYPES: BEGIN OF ty_prctr,
         prctr TYPE prctr,
         ktext TYPE cepct-ktext,
       END OF ty_prctr.

TYPES: BEGIN OF ty_kostl,
         kostl TYPE kostl,
         ktext TYPE cskt-ktext,
       END OF ty_kostl.

TYPES: tt_prctr TYPE STANDARD TABLE OF ty_prctr,
       tt_kostl TYPE STANDARD TABLE OF ty_kostl.

*&---------------------------------------------------------------------*
*&              TYPES
*&---------------------------------------------------------------------*
TYPES:
* 总帐: 总计
  BEGIN OF typ_bseg,
    gjahr      TYPE bseg-gjahr,             "会计年度
    monat      TYPE bkpf-monat,             "月份
    bukrs      TYPE bseg-bukrs,             "公司代码
    belnr      TYPE bseg-belnr,             "会计凭证编号 add by wangjunchuan on 20180521 bpmno:ERP-YW201805090294
    buzei      TYPE bseg-buzei,             "会计凭证中的行项目数 add by wangjunchuan on 20180521 bpmno:ERP-YW201805090294
    shkzg      TYPE bseg-shkzg,             "借方/贷方标识
    dmbtr      TYPE bseg-dmbtr,             "按本位币计的金额
    rstgr      TYPE bseg-rstgr,             "付款原因代码
    prctr      TYPE bseg-prctr,             "利润中心

    xreversal  TYPE bkpf-xreversal, "是否冲销
    bldat      TYPE bkpf-bldat,    "凭证日期
    budat      TYPE bkpf-budat,    "过帐日期
    bstat      TYPE bkpf-bstat,    "凭证状态
    waers      TYPE bkpf-waers,    "货币
    hwaer      TYPE bkpf-hwaer,    "本位币
    bktxt      TYPE bkpf-bktxt,    "凭证抬头文本
    bschl      TYPE bseg-bschl,    "记账码
    xnegp      TYPE bseg-xnegp,    "反记帐
    hkont      TYPE bseg-hkont,    "总账科目
    xbilk      TYPE bseg-xbilk,    "是否资产负债表科目
    prctr_text TYPE cepct-ktext,   "利润中心描述
    kostl      TYPE bseg-kostl,    "成本中心
    kostl_text TYPE cskt-ktext,    "成本中心描述
    koart      TYPE bseg-koart,    "科目类型
    umskz      TYPE bseg-umskz,    "特殊总帐标识
    lifnr      TYPE bseg-lifnr,    "供应商
    lifnr_text TYPE lfa1-name1,    "供应商描述
    kunnr      TYPE bseg-kunnr,    "客户
    kunnr_text TYPE kna1-name1,    "客户描述
    gsber      TYPE bseg-gsber,    "业务范围
    fkber      TYPE bseg-fkber,    "功能范围
*    ZZ_FEETY   TYPE BSEG-ZZ_FEETY, "费用类型
    aufnr      TYPE bseg-aufnr   , "订单
    wrbtr      TYPE bseg-wrbtr   , "凭证货币金额
    matnr      TYPE bseg-matnr   , "物料号
    menge      TYPE bseg-menge   , "数量
    meins      TYPE bseg-meins   , "基本计量单位
    zfbdt      TYPE bseg-zfbdt   , "基准日期
    zuonr      TYPE bseg-zuonr   , "分配
    pswsl      TYPE bseg-pswsl   , "总帐货币金额
    sgtxt      TYPE bseg-sgtxt   , "行项目文本
  END OF typ_bseg.

TYPES:BEGIN OF ty_bkpf,
        bukrs     TYPE bkpf-bukrs,
        gjahr     TYPE bkpf-gjahr,
        belnr     TYPE bkpf-belnr,
        monat     TYPE bkpf-monat,
        xreversal TYPE bkpf-xreversal,
        bldat     TYPE bkpf-bldat,
        budat     TYPE bkpf-budat,
        bstat     TYPE bkpf-bstat,
        waers     TYPE bkpf-waers,
        hwaer     TYPE bkpf-hwaer,
        bktxt     TYPE bkpf-bktxt,
      END OF ty_bkpf.


TYPES:
  BEGIN OF ty_faglflext,
    ryear TYPE gjahr,
    hslvt TYPE hslvt12,
*--------修改开始 2017-03-06 IBM-ZJX------------------------------*
* 本月发生额：目前取的是年初数，需要修改为本月的月初数，          *
* 即上月的月末数。                                                *
*-----------------------------------------------------------------*
    rpmax TYPE faglflext-rpmax,        "期间
    hsl01 TYPE faglflext-hsl01,        "按本位币的期间中的业务总计
    hsl02 TYPE faglflext-hsl02,        "按本位币的期间中的业务总计
    hsl03 TYPE faglflext-hsl03,        "按本位币的期间中的业务总计
    hsl04 TYPE faglflext-hsl04,        "按本位币的期间中的业务总计
    hsl05 TYPE faglflext-hsl05,        "按本位币的期间中的业务总计
    hsl06 TYPE faglflext-hsl06,        "按本位币的期间中的业务总计
    hsl07 TYPE faglflext-hsl07,        "按本位币的期间中的业务总计
    hsl08 TYPE faglflext-hsl08,        "按本位币的期间中的业务总计
    hsl09 TYPE faglflext-hsl09,        "按本位币的期间中的业务总计
    hsl10 TYPE faglflext-hsl10,        "按本位币的期间中的业务总计
    hsl11 TYPE faglflext-hsl11,        "按本位币的期间中的业务总计
    hsl12 TYPE faglflext-hsl12,        "按本位币的期间中的业务总计
*------修改结束  2017-03-06 IBM-ZJX-------------------------------*
  END OF ty_faglflext.


DATA:
  ls_faglflext TYPE ty_faglflext,
  lt_faglflext TYPE TABLE OF ty_faglflext,
  lv_this_year TYPE hslvt,
  lv_last_year TYPE hslvt.

*" 现金流量表
TYPES: BEGIN OF zfi01_xjll_s,
         rstgr TYPE bseg-rstgr, "付款原因代码
         hcnum TYPE i,          "行次号
         hbyfs TYPE hslxx12,    "本月发生
         hqmlj TYPE hslxx12,    "期末累计
         hsntq TYPE hslxx12,    "上年同期
       END OF zfi01_xjll_s.

TYPES: zfi01_xjll_tt  TYPE STANDARD TABLE OF zfi01_xjll_s.

DATA:
  gt_bseg   TYPE STANDARD TABLE OF typ_bseg,
  gt_bkpf   TYPE STANDARD TABLE OF ty_bkpf,
  gt_xjll   TYPE STANDARD TABLE OF zfi01_xjll_s,
  gt_output TYPE STANDARD TABLE OF zfi01_xjll_s.

TYPES BEGIN OF ty_detail.
        INCLUDE  TYPE typ_bseg.
TYPES field      TYPE char20."双击字段名
TYPES END OF ty_detail.
DATA gt_bseg_detail_all TYPE TABLE OF ty_detail."add by wangjunchuan on 20180521 bpmno:ERP-YW201805090294
DATA gt_bseg_detail TYPE TABLE OF ty_detail."add by wangjunchuan on 20180521 bpmno:ERP-YW201805090294

TYPES : BEGIN OF ty_alv_out ,
          xm_txt TYPE char100,
          yydm_t TYPE char10,
          rstgr  TYPE bseg-rstgr, "付款原因代码
          hcnum  TYPE i,          "行次号
          hbyfs  TYPE hslxx12,    "本月发生
          hqmlj  TYPE hslxx12,    "期末累计
          hsntq  TYPE hslxx12,    "上年同期
        END OF ty_alv_out .

DATA: gt_alv_out  TYPE STANDARD TABLE OF ty_alv_out.

DATA : BEGIN OF wa_alv_out,
         xm_txt TYPE char100,
         yydm_t TYPE char10,
         rstgr  TYPE bseg-rstgr, "付款原因代码
         hcnum  TYPE i,          "行次号
         hbyfs  TYPE hslxx12,    "本月发生
         hqmlj  TYPE hslxx12,    "期末累计
         hsntq  TYPE hslxx12,    "上年同期
       END OF wa_alv_out .

DATA : BEGIN OF wa_alv_out2,
         xm_txt TYPE char100,
         yydm_t TYPE char10,
         rstgr  TYPE bseg-rstgr, "付款原因代码
         hcnum  TYPE i,          "行次号
         hbyfs  TYPE hslxx12,    "本月发生
         hqmlj  TYPE hslxx12,    "期末累计
         hsntq  TYPE hslxx12,    "上年同期
       END OF wa_alv_out2 .

DATA : BEGIN OF wa_alv_out3,
         xm_txt TYPE char100,
         yydm_t TYPE char10,
         rstgr  TYPE bseg-rstgr, "付款原因代码
         hcnum  TYPE i,          "行次号
         hbyfs  TYPE hslxx12,    "本月发生
         hqmlj  TYPE hslxx12,    "期末累计
         hsntq  TYPE hslxx12,    "上年同期
       END OF wa_alv_out3 .

DATA : BEGIN OF wa_alv_out4,
         xm_txt TYPE char100,
         yydm_t TYPE char10,
         rstgr  TYPE bseg-rstgr, "付款原因代码
         hcnum  TYPE i,          "行次号
         hbyfs  TYPE hslxx12,    "本月发生
         hqmlj  TYPE hslxx12,    "期末累计
         hsntq  TYPE hslxx12,    "上年同期
       END OF wa_alv_out4 .

*输出Excel相关变量
DATA: l_filename   TYPE string.

*路径
DATA: l_fullpath    TYPE string,
      l_path        TYPE string,
      l_user_action TYPE i,
      l_encoding    TYPE abap_encoding.

*输出Excel相关变量
DATA: ls_wwwdata TYPE wwwdatatab.
DATA: v_excel    TYPE ole2_object.        " Excel object
DATA: v_book     TYPE ole2_object.
DATA: v_sheet    TYPE ole2_object.
DATA: v_cell     TYPE ole2_object.

*DATA: grt_bukrs TYPE tt_rsdsselopt,
*      grs_bukrs LIKE LINE OF grt_bukrs.
*DATA: grt_pcgrp TYPE RANGE OF fagl_srep_pcgrp.


*&---------------------------------------------------------------------*
*&              SELECTION-SCREEN
*&---------------------------------------------------------------------*
* 定义选择屏幕
SELECTION-SCREEN: BEGIN OF BLOCK b1.
*    WITH FRAME TITLE text-003.
*PARAMETERS:
*  p_bukrs TYPE faglflexa-rbukrs OBLIGATORY.  "公司代码
SELECT-OPTIONS  : s_bukrs FOR faglflexa-rbukrs NO INTERVALS NO-EXTENSION  MEMORY ID buk OBLIGATORY  .

*SELECT-OPTIONS:
*s_prctr FOR  faglflexa-prctr,              "利润中心
*s_pcgrp FOR  tfin011-pcgrp  MATCHCODE OBJECT zfi009.
*"利润中心组
PARAMETERS:
  p_ryear TYPE faglflexa-ryear  OBLIGATORY DEFAULT sy-datum+0(4),
  "年度
  p_monat TYPE bkpf-monat       OBLIGATORY DEFAULT sy-datum+4(2).
"月份
SELECT-OPTIONS:
   s_gsber FOR  bseg-gsber.             "科目

PARAMETERS:
p_yzpz AS CHECKBOX DEFAULT 'X'.
"p_flag TYPE char1 NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b1.


*&---------------------------------------------------------------------*
*&              AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

* 选择画面月份的检查
  PERFORM frm_monat_check.

INITIALIZATION.

* 初期化处理
  CLEAR:
     gt_bseg,
     gt_xjll,
     gt_output.


START-OF-SELECTION.

*  PERFORM frm_get_bukrs_from_kokrs USING '3000' CHANGING grt_bukrs.

*  PERFORM frm_build_profit_center.

  PERFORM frm_author_check.

  PERFORM frm_faglflext_get.

  PERFORM frm_report_edit.

  PERFORM data_display.
  "PERFORM frm_download_proc.
*&---------------------------------------------------------------------*
*&      Form  FRM_MONAT_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_monat_check .
*modify by xubin 20180410 ERP-YW201804020027
*  if p_monat > 12.
  IF p_monat > 16.
*end modify by xubin 20180410 ERP-YW201804020027
    MESSAGE s002
    DISPLAY LIKE 'E'.
    LEAVE SCREEN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_BUKRS_FROM_KOKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0322   text
*      <--P_GRT_BUKRS  text
*----------------------------------------------------------------------*
FORM frm_get_bukrs_from_kokrs USING i_kokrs         TYPE kokrs
  CHANGING ct_range_bukrs  TYPE tt_rsdsselopt.
  DATA: BEGIN OF ls_bukrs,
          bukrs TYPE bukrs,
        END OF ls_bukrs,
        lt_bukrs LIKE TABLE OF ls_bukrs.

  DATA: lrs_bukrs TYPE LINE OF tt_rsdsselopt.

  CLEAR ct_range_bukrs.

  CALL FUNCTION 'RK_BUKRS_OF_KOKRS'
    EXPORTING
      kokrs          = i_kokrs
    TABLES
      t_bukrs        = lt_bukrs
    EXCEPTIONS
      no_bukrs_found = 1
      not_found      = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


  IF s_bukrs IS NOT INITIAL.
    LOOP AT lt_bukrs INTO ls_bukrs.
      IF ls_bukrs-bukrs IN s_bukrs.
        lrs_bukrs-sign    = 'I'.
        lrs_bukrs-option  = 'EQ'.
        lrs_bukrs-low     = ls_bukrs-bukrs.
        APPEND lrs_bukrs TO ct_range_bukrs.
        CLEAR lrs_bukrs.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT lt_bukrs INTO ls_bukrs.
      lrs_bukrs-sign    = 'I'.
      lrs_bukrs-option  = 'EQ'.
      lrs_bukrs-low     = ls_bukrs-bukrs.
      APPEND lrs_bukrs TO ct_range_bukrs.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_PROFIT_CENTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_build_profit_center .
*" Profit Center
  DATA: lrt_prctr TYPE RANGE OF prctr,
        lrs_prctr LIKE RANGE OF lrt_prctr.
  DATA: lt_pcgrp_hierarchy TYPE STANDARD TABLE OF bapiset_hier,
        ls_pcgrp_hierarchy TYPE bapiset_hier,
        lt_pcgrp_value     TYPE STANDARD TABLE OF bapi1116_values,
        ls_pcgrp_value     TYPE bapi1116_values.

  DATA: ls_return  TYPE bapiret2.

  DATA: lt_pcgrp           TYPE STANDARD TABLE OF bapiset_groupname.
*  DATA: lrs_pcgrp LIKE LINE OF grt_pcgrp.


  DATA: lt_prctr       TYPE STANDARD TABLE OF ty_prctr,
        lt_prctr_final TYPE STANDARD TABLE OF ty_prctr,
        ls_prctr       TYPE ty_prctr.

  DATA: setclass      TYPE setheader-setclass,
        subclass      TYPE setheader-subclass,
        prefix        TYPE char16,
        lv_set_id     TYPE sethier-setid,
        lt_set_hier   TYPE STANDARD TABLE OF sethier,
        ls_set_hier   TYPE sethier,
        lt_set_values TYPE STANDARD TABLE OF setvalues.

  DATA: lv_group_name TYPE bapiset_groupname.

*  CLEAR: grt_pcgrp.
*
**" Build Range Table
**  IF s_prctr IS INITIAL.
**    IF s_pcgrp IS NOT INITIAL.
**" this bapi check authority object K_PCA, Orz...
**      call function 'BAPI_PROFITCENTERGRP_GETDETAIL'
**        exporting
**          controllingarea = 'COFT'
**          groupname       = 'COFT'
***         LANGUAGE        =
**        importing
**          return          = ls_return
**        tables
**          hierarchynodes  = lt_pcgrp_hierarchy
**          hierarchyvalues = lt_pcgrp_value.
*      setclass = '0106'.
*      subclass = '3000'.
*      CONCATENATE setclass subclass INTO prefix.
*      CONCATENATE setclass subclass '3000' INTO lv_set_id.
*      CALL FUNCTION 'G_SET_TREE_IMPORT'
*        EXPORTING
**         CLIENT                    = SY-MANDT
**         FIELDNAME                 = ' '
**         LANGU                     = SY-LANGU
**         NO_DESCRIPTIONS           = ' '
**         NO_RW_INFO                = ' '
*          setid                     = lv_set_id
**         TABNAME                   = ' '
**         NO_VARIABLE_REPLACEMENT   = ' '
**         ROOT_HEADER_ONLY          = ' '
**         NO_TABLE_BUFFERING        = ' '
*          max_hier_level            = 999
**         DATE_FROM                 =
**         DATE_TO                   =
**       IMPORTING
**         SET_NOT_TRANSPARENT       =
*        TABLES
*          set_hierarchy             = lt_set_hier
*          set_values                = lt_set_values
*        EXCEPTIONS
*          set_not_found             = 1
*          illegal_field_replacement = 2
*          illegal_table_replacement = 3
*          set_is_damaged            = 4
*          OTHERS                    = 5.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
*
**      if ls_return is not initial.
**        message id ls_return-id type ls_return-type number ls_return-number
**                with ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
**      endif.
*
**      loop at lt_pcgrp_hierarchy into ls_pcgrp_hierarchy.
**        if ls_pcgrp_hierarchy-groupname in s_pcgrp.
***        append ls_pcgrp_hierarchy-groupname to lt_pcgrp.
***" Profit Center Group
**          clear lrs_pcgrp.
**          lrs_pcgrp-sign    = 'I'.
**          lrs_pcgrp-option  = 'EQ'.
**          lrs_pcgrp-low     = ls_pcgrp_hierarchy-groupname.
**          lrs_pcgrp-high    = ''.
**          append lrs_pcgrp to grt_pcgrp.
**
**          perform get_profit_center_by_group using ls_pcgrp_hierarchy-groupname
**                                          changing lt_prctr.
**          append lines of lt_prctr to lt_prctr_final.
**
**        endif.
**      endloop.
*
*      LOOP AT lt_set_hier INTO ls_set_hier.
*        lv_group_name = substring_after( val = ls_set_hier-setid
*        sub = prefix ).
*        IF lv_group_name IN s_pcgrp.
*          CLEAR lrs_pcgrp.
*          lrs_pcgrp-sign    = 'I'.
*          lrs_pcgrp-option  = 'EQ'.
*          lrs_pcgrp-low     =  lv_group_name  . " ls_pcgrp_hierarchy-groupname.
*          lrs_pcgrp-high    = ''.
*          APPEND lrs_pcgrp TO grt_pcgrp.
*
*          PERFORM get_profit_center_by_group USING lv_group_name  " ls_pcgrp_hierarchy-groupname
*          CHANGING lt_prctr.
*          APPEND LINES OF lt_prctr TO lt_prctr_final.
*        ENDIF.
*        CLEAR lt_prctr.
*        CLEAR lv_group_name.
*      ENDLOOP.
*
*      LOOP AT lt_prctr_final INTO ls_prctr.
*        CLEAR s_prctr.
*        s_prctr-sign      = 'I'.
*        s_prctr-option    = 'EQ'.
*        s_prctr-low       = ls_prctr-prctr.
**      s_prctr-sign      = ''.
*        APPEND s_prctr.
*      ENDLOOP.
*    ENDIF.
*  ELSE.
*    " do nothing...
*  ENDIF.
*
*  IF s_prctr[] IS INITIAL.
*    PERFORM get_prctr_from_cepc_bukrs USING s_bukrs-low
*                                   CHANGING s_prctr[].
*  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM get_prctr_from_cepc_bukrs USING iv_bukrs TYPE bkpf-bukrs
                  CHANGING ct_range_prctr TYPE fagl_range_t_prctr.
  DATA: lt_cepc_bukrs TYPE STANDARD TABLE OF cepc_bukrs,
        ls_cepc_bukrs TYPE cepc_bukrs.

  DATA: ls_range_prctr  TYPE fagl_range_prctr.

  SELECT *
    INTO TABLE lt_cepc_bukrs
    FROM cepc_bukrs
    WHERE kokrs = '3000'
      AND bukrs = iv_bukrs.

  LOOP AT lt_cepc_bukrs INTO ls_cepc_bukrs.

    CLEAR: ls_range_prctr.
    ls_range_prctr-sign   = 'I'.
    ls_range_prctr-option = 'EQ'.
    ls_range_prctr-low    = ls_cepc_bukrs-prctr.
    APPEND ls_range_prctr TO ct_range_prctr.

  ENDLOOP.

ENDFORM.


FORM get_profit_center_by_group USING i_pcgrp   TYPE fagl_srep_pcgrp
  CHANGING ct_prctr  TYPE tt_prctr .

  DATA: lv_group_name TYPE bapiset_groupname,
        ls_language   TYPE bapi0015_10.

  DATA: lt_hierarchy TYPE STANDARD TABLE OF bapiset_hier,
        lt_value     TYPE STANDARD TABLE OF bapi1116_values,
        ls_value     TYPE bapi1116_values,
        ls_return    TYPE bapiret2.

  DATA: ls_prctr TYPE ty_prctr,
        lt_prctr TYPE STANDARD TABLE OF ty_prctr.

  DATA: setclass      TYPE setheader-setclass,
        subclass      TYPE setheader-subclass,
        prefix        TYPE char16,
        lv_set_id     TYPE sethier-setid,
        lt_set_hier   TYPE STANDARD TABLE OF sethier,
        lt_set_values TYPE STANDARD TABLE OF setvalues,
        ls_set_values TYPE setvalues.

  CLEAR ct_prctr.

  lv_group_name      = i_pcgrp.
  ls_language-langu  = sy-langu.

*  call function 'BAPI_PROFITCENTERGRP_GETDETAIL'
*    exporting
*      controllingarea = 'COFT'
*      groupname       = lv_group_name
*      language        = ls_language
*    importing
*      return          = ls_return
*    tables
*      hierarchynodes  = lt_hierarchy
*      hierarchyvalues = lt_value.
*
*  if ls_return is not initial.
*    message id ls_return-id type ls_return-type number ls_return-number
*            with ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
*  endif.

  setclass = '0106'.
  subclass = '3000'.
  CONCATENATE setclass subclass INTO prefix.
  CONCATENATE setclass subclass i_pcgrp INTO lv_set_id.
  CALL FUNCTION 'G_SET_TREE_IMPORT'
    EXPORTING
*     CLIENT                    = SY-MANDT
*     FIELDNAME                 = ' '
*     LANGU                     = SY-LANGU
*     NO_DESCRIPTIONS           = ' '
*     NO_RW_INFO                = ' '
      setid                     = lv_set_id
*     TABNAME                   = ' '
*     NO_VARIABLE_REPLACEMENT   = ' '
*     ROOT_HEADER_ONLY          = ' '
*     NO_TABLE_BUFFERING        = ' '
      max_hier_level            = 999
*     DATE_FROM                 =
*     DATE_TO                   =
*       IMPORTING
*     SET_NOT_TRANSPARENT       =
    TABLES
      set_hierarchy             = lt_set_hier
      set_values                = lt_set_values
    EXCEPTIONS
      set_not_found             = 1
      illegal_field_replacement = 2
      illegal_table_replacement = 3
      set_is_damaged            = 4
      OTHERS                    = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*  loop at lt_value into ls_value.
*    ls_prctr-prctr  = ls_value-valfrom.
*    append ls_prctr to lt_prctr..
*  endloop.

  LOOP AT lt_set_values INTO ls_set_values.
    ls_prctr-prctr  = ls_set_values-from.
    APPEND ls_prctr TO lt_prctr.
  ENDLOOP.

  ct_prctr  = lt_prctr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTHOR_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_author_check .

  PERFORM authority_check_bukrs.

  PERFORM authority_check_prctr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM authority_check_bukrs .
  DATA: lv_bukrs  TYPE bukrs.

* 公司代码权限检查
*  LOOP AT grt_bukrs INTO grs_bukrs.
*    lv_bukrs  = grs_bukrs-low.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
        ID 'BUKRS' FIELD s_bukrs-low
  ID 'ACTVT' DUMMY.

* 检查失败的场合
  IF sy-subrc <> 0.
    MESSAGE e398(00) WITH '您没有公司代码' s_bukrs-low '的操作权限！'.
  ENDIF.
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK_PRCTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM authority_check_prctr .
  DATA: BEGIN OF ls_prctr,
          prctr TYPE prctr,
        END OF ls_prctr,
        lt_prctr LIKE TABLE OF ls_prctr.

*  DATA: lrs_pcgrp LIKE LINE OF grt_pcgrp.

  DATA: resparea  TYPE resparea.

*" Profit Center Group
*  LOOP AT grt_pcgrp INTO lrs_pcgrp.
*    CLEAR resparea.
*    CONCATENATE 'PHCOFT'
*    lrs_pcgrp-low
*    INTO resparea.
*    AUTHORITY-CHECK OBJECT 'K_PCA'
*    ID 'RESPAREA'  FIELD resparea
*    ID 'CO_ACTION' DUMMY
*    ID 'KSTAR'     DUMMY.
*    IF sy-subrc <> 0.
*      MESSAGE e004(zfirpt015) WITH '您没有利润中心组' lrs_pcgrp-low '的权限'.
*    ENDIF.
*  ENDLOOP.
*
**" Profit Center
*  SELECT prctr
*  INTO TABLE lt_prctr
*  FROM cepc
*  WHERE prctr IN s_prctr
*  AND datbi >= sy-datum
*  AND kokrs = '3000'.
*  LOOP AT lt_prctr INTO ls_prctr.
*    CLEAR resparea.
*    CONCATENATE 'PCCOFT'
*    ls_prctr-prctr
*    INTO resparea.
*    AUTHORITY-CHECK OBJECT 'K_PCA'
*    ID 'RESPAREA' FIELD resparea
*    ID 'CO_ACTION'  DUMMY
*    ID 'KSTAR' DUMMY.
*    IF sy-subrc <> 0.
*      MESSAGE e004(zfirpt015) WITH '您没有利润中心' ls_prctr-prctr '的权限'.
*    ENDIF.
*  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_FAGLFLEXT_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_faglflext_get .
* 局部定义
  DATA:
    lw_resparea(40) TYPE c,
    lw_prctr        TYPE prctr,
    lw_pcgrp        TYPE fc_pcgrp,
    lw_year         TYPE faglflexa-ryear,
    lh_ethier_co    LIKE sethier_co,
    lt_ethier_co    LIKE TABLE OF sethier_co.

  lw_year = p_ryear - 1.

* 局部定义
  DATA:
    lrt_pcgrp    TYPE RANGE OF setleaf-valfrom,
    lrh_pcgrp    LIKE LINE OF lrt_pcgrp,
    lh_pcgrp     TYPE setleaf,
    lt_pcgrp     TYPE STANDARD TABLE OF setleaf,
    lt_pcgrp_tem TYPE STANDARD TABLE OF setleaf,
    lh_bseg      TYPE typ_bseg,
    lh_bkpf      TYPE ty_bkpf.

*  LOOP AT s_pcgrp.
*
**   利润中心组-利润中心关系的取得
*    CALL FUNCTION 'ZFI_FG014_GET_PCGRP'
*      EXPORTING
*        iv_pcgrp_low  = s_pcgrp-low
*        iv_pcgrp_high = s_pcgrp-high
*      TABLES
*        et_lirzx      = lt_pcgrp_tem.
*
*    IF lt_pcgrp_tem IS NOT INITIAL.
*      APPEND LINES OF lt_pcgrp_tem TO lt_pcgrp.
*    ENDIF.
*
*  ENDLOOP.
*  LOOP AT lt_pcgrp INTO lh_pcgrp.
*    s_prctr-sign   = lh_pcgrp-valsign.
*    s_prctr-option = lh_pcgrp-valoption.
*    s_prctr-low    = lh_pcgrp-valfrom.
*    s_prctr-high   = lh_pcgrp-valto.
*    APPEND s_prctr TO s_prctr.
*  ENDLOOP.

  SELECT  t1~gjahr                       "会计年度
          t1~bukrs                       "公司代码
          t1~belnr                 "add by wangjunchuan on 20180521 bpmno:ERP-YW201805090294
          t1~buzei                 "add by wangjunchuan on 20180521 bpmno:ERP-YW201805090294
          t1~shkzg                       "借方/贷方标识
*               t1~wrbtr AS dmbtr                       "按本位币计的金额
          t1~dmbtr
          t1~rstgr                       "付款原因代码
          t1~prctr                       "利润中心
          t1~bschl
          t1~xnegp
          t1~hkont
          t1~xbilk
          t1~kostl
          t1~koart
          t1~umskz
          t1~lifnr
          t1~kunnr
          t1~gsber
          t1~fkber
*          T1~ZZ_FEETY
          t1~aufnr
          t1~wrbtr
          t1~matnr
          t1~menge
          t1~meins
          t1~zfbdt
          t1~zuonr
          t1~pswsl
          t1~sgtxt
*          t2~monat                       "月份
*          t2~xreversal
*          t2~bldat
*          t2~budat
*          t2~bstat
*          t2~waers
*          t2~hwaer
*          t2~bktxt
     FROM bseg AS t1
*    INNER JOIN bkpf AS t2
**       ON t1~belnr = t2~belnr AND t1~gjahr = t2~gjahr AND t1~bukrs = t2~bukrs
     INTO CORRESPONDING FIELDS OF TABLE gt_bseg
    WHERE t1~bukrs IN s_bukrs  "   =  p_bukrs          "公司代码
      AND ( t1~gjahr = p_ryear           "会计年度
       OR  t1~gjahr = lw_year )         "上年
*      AND t2~monat  <= p_monat           "月份
*      AND t1~prctr  IN s_prctr           "利润中心
      AND t1~rstgr  <> ''
      AND t1~gsber IN s_gsber
*     AND t1~prctr  IN lrt_pcgrp         "利润中心组-利润中心关系
      AND ( t1~hkont  >= '1001000000'    "会计科目下限
      AND   t1~hkont  <= '1012999999'    "会计科目上限
       OR   t1~hkont   = '9999000003' ).
  IF gt_bseg IS NOT INITIAL.
    SELECT   t2~bukrs
             t2~gjahr
             t2~belnr
             t2~monat                       "月份
             t2~xreversal
             t2~bldat
             t2~budat
             t2~bstat
             t2~waers
             t2~hwaer
             t2~bktxt
    INTO CORRESPONDING FIELDS OF TABLE gt_bkpf
    FROM bkpf AS t2 FOR ALL ENTRIES IN gt_bseg
    WHERE bukrs = gt_bseg-bukrs
      AND gjahr = gt_bseg-gjahr
      AND belnr = gt_bseg-belnr
      AND monat <= p_monat.
    SORT gt_bkpf BY bukrs gjahr belnr.

    LOOP AT gt_bseg INTO lh_bseg.
      READ TABLE gt_bkpf INTO lh_bkpf WITH KEY bukrs = lh_bseg-bukrs
                                               gjahr = lh_bseg-gjahr
                                               belnr = lh_bseg-belnr.
      IF sy-subrc = 0.
        lh_bseg-gjahr = lh_bkpf-gjahr.
        lh_bseg-belnr = lh_bkpf-belnr.
        lh_bseg-monat = lh_bkpf-monat.
        lh_bseg-xreversal = lh_bkpf-xreversal.
        lh_bseg-bldat = lh_bkpf-bldat.
        lh_bseg-budat = lh_bkpf-budat.
        lh_bseg-bstat = lh_bkpf-bstat.
        lh_bseg-waers = lh_bkpf-waers.
        lh_bseg-hwaer = lh_bkpf-hwaer.
        lh_bseg-bktxt = lh_bkpf-bktxt.
      ELSE.
        DELETE gt_bseg.
        CONTINUE.
      ENDIF.
      MODIFY gt_bseg FROM lh_bseg.
      CLEAR:lh_bkpf,lh_bseg.
    ENDLOOP.
  ENDIF.

  IF p_yzpz = 'X'.
    SELECT t1~gjahr                       "会计年度
           t1~bukrs                       "公司代码
           t1~belnr                 "add by wangjunchuan on 20180521 bpmno:ERP-YW201805090294
           t1~buzei                 "add by wangjunchuan on 20180521 bpmno:ERP-YW201805090294
           t1~shkzg                       "借方/贷方标识
*                      t1~wrbtr AS dmbtr                       "按本位币计的金额
           t1~dmbtr
           t1~rstgr                       "付款原因代码
           t1~prctr                       "利润中心
           t1~bschl
           t1~xnegp
           t1~saknr AS hkont
*           T1~XBILK
           t1~kostl
           t1~koart
*           T1~UMSKZ
*           T1~LIFNR
*           T1~KUNNR
           t1~gsber
           t1~fkber
*           T1~ZZ_FEETY
           t1~aufnr
           t1~wrbtr
           t1~matnr
           t1~menge
           t1~meins
           t1~zfbdt
           t1~zuonr
*           T1~PSWSL
           t1~sgtxt
           t2~monat                       "月份
*           T2~XREVERSAL
           t2~bldat
           t2~budat
           t2~bstat
           t2~waers
           t2~hwaer
           t2~bktxt
       FROM vbsegs AS t1
      INNER JOIN vbkpf AS t2
         ON t1~belnr = t2~belnr AND t1~gjahr = t2~gjahr AND t1~bukrs = t2~bukrs
  APPENDING CORRESPONDING FIELDS OF TABLE gt_bseg
      WHERE t1~bukrs IN s_bukrs  "   =  p_bukrs          "公司代码
        AND ( t1~gjahr = p_ryear           "会计年度
         OR  t1~gjahr = lw_year )         "上年
        AND t2~monat  <= p_monat           "月份
*        AND t1~prctr  IN s_prctr           "利润中心
        AND t1~rstgr  <> ''
        AND t1~gsber IN s_gsber
*     AND t1~prctr  IN lrt_pcgrp         "利润中心组-利润中心关系
        AND ( t1~saknr  >= '1001000000'    "会计科目下限
        AND   t1~saknr  <= '1012999999'    "会计科目上限
         OR   t1~saknr   = '9999000003' ).
  ENDIF.




***  SELECT ryear
***   hslvt
***   rpmax         "期间
***   hsl01         "按本位币的期间中的业务总计
***   hsl02         "按本位币的期间中的业务总计
***   hsl03         "按本位币的期间中的业务总计
***   hsl04         "按本位币的期间中的业务总计
***   hsl05         "按本位币的期间中的业务总计
***   hsl06         "按本位币的期间中的业务总计
***   hsl07         "按本位币的期间中的业务总计
***   hsl08         "按本位币的期间中的业务总计
***   hsl09         "按本位币的期间中的业务总计
***   hsl10         "按本位币的期间中的业务总计
***   hsl11         "按本位币的期间中的业务总计
***   hsl12         "按本位币的期间中的业务总计
****------修改结束  2017-03-06 IBM-ZJX-------------------------------*
***   FROM faglflext
***   INTO CORRESPONDING FIELDS OF TABLE lt_faglflext
***   WHERE ( ryear = p_ryear OR ryear = lw_year )
***   AND racct  >= '1001000000'   "会计科目下限
***   AND racct  <= '1012999999'   "会计科目上限
***   AND rbukrs IN grt_bukrs
***   AND rbusa  IN s_gsber
***   AND prctr IN s_prctr.

  SELECT faglflext~ryear
   faglflext~hslvt
   faglflext~rpmax         "期间
   faglflext~hsl01         "按本位币的期间中的业务总计
   faglflext~hsl02         "按本位币的期间中的业务总计
   faglflext~hsl03         "按本位币的期间中的业务总计
   faglflext~hsl04         "按本位币的期间中的业务总计
   faglflext~hsl05         "按本位币的期间中的业务总计
   faglflext~hsl06         "按本位币的期间中的业务总计
   faglflext~hsl07         "按本位币的期间中的业务总计
   faglflext~hsl08         "按本位币的期间中的业务总计
   faglflext~hsl09         "按本位币的期间中的业务总计
   faglflext~hsl10         "按本位币的期间中的业务总计
   faglflext~hsl11         "按本位币的期间中的业务总计
   faglflext~hsl12         "按本位币的期间中的业务总计
   FROM faglflext
    INNER JOIN skb1 ON faglflext~rbukrs = skb1~bukrs
                   AND  faglflext~racct = skb1~saknr
   INTO CORRESPONDING FIELDS OF TABLE lt_faglflext
   WHERE ( faglflext~ryear = p_ryear OR faglflext~ryear = lw_year )
   AND ( faglflext~racct LIKE '1001%'   OR  faglflext~racct LIKE '1002%' OR  faglflext~racct LIKE '1012%'  )  "会计科目下限
   AND faglflext~rbukrs IN s_bukrs
   AND faglflext~rbusa  IN s_gsber
*   AND faglflext~prctr IN s_prctr
   AND skb1~fstag = 'G005'.

  IF sy-subrc <> 0.
    MESSAGE s003
    DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REPORT_EDIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_report_edit .
* 局部定义
  DATA:
    lh_bseg TYPE typ_bseg,
    lh_xjll TYPE zfi01_xjll_s,
    ls_xjll TYPE zfi01_xjll_s.

  DATA: ls_alv_out  TYPE ty_alv_out.

* 报表数据的编辑
  LOOP AT gt_bseg INTO lh_bseg.

*-  付款原因代码
    CASE lh_bseg-rstgr.

*--   A01的场合
      WHEN 'A01'.
*---    值的计算
        PERFORM frm_value_proc USING '2'
              lh_bseg.
*--   A02的场合
      WHEN 'A02'.
*---    值的计算
        PERFORM frm_value_proc USING '3'
              lh_bseg.
*--   A03的场合
      WHEN 'A03'.
*---    值的计算
        PERFORM frm_value_proc USING '4'
              lh_bseg.
*--   A04的场合
      WHEN 'A04'.
*---    值的计算
        PERFORM frm_value_proc USING '5'
              lh_bseg.
*--   A05的场合
      WHEN 'A05'.
*---    值的计算
        PERFORM frm_value_proc USING '6'
              lh_bseg.
*--   A06的场合
      WHEN 'A06'.
*---    值的计算
        PERFORM frm_value_proc USING '7'
              lh_bseg.
*--   A07的场合
      WHEN 'A07'.
*---    值的计算
        PERFORM frm_value_proc USING '8'
              lh_bseg.
*--   A08的场合
      WHEN 'A08'.
*---    值的计算
        PERFORM frm_value_proc USING '9'
              lh_bseg.
*--   A09的场合
      WHEN 'A09'.
*---    值的计算
        PERFORM frm_value_proc USING '10'
              lh_bseg.
*--   A10的场合
      WHEN 'A10'.
*---    值的计算
        PERFORM frm_value_proc USING '11'
              lh_bseg.
*--   A11的场合
      WHEN 'A11'.
*---    值的计算
        PERFORM frm_value_proc USING '12'
              lh_bseg.
*--   A12的场合
      WHEN 'A12'.
*---    值的计算
        PERFORM frm_value_proc USING '13'
              lh_bseg.
*--   A13的场合
      WHEN 'A13'.
*---    值的计算
        PERFORM frm_value_proc USING '14'
              lh_bseg.
*--   B01的场合
      WHEN 'B01'.
*---    值的计算
        PERFORM frm_value_proc USING '16'
              lh_bseg.
*--   B02的场合
      WHEN 'B02'.
*---    值的计算
        PERFORM frm_value_proc USING '17'
              lh_bseg.
*--   B03的场合
      WHEN 'B03'.
*---    值的计算
        PERFORM frm_value_proc USING '18'
              lh_bseg.
*--   B04的场合
      WHEN 'B04'.
*---    值的计算
        PERFORM frm_value_proc USING '19'
              lh_bseg.
*--   B05的场合
      WHEN 'B05'.
*---    值的计算
        PERFORM frm_value_proc USING '20'
              lh_bseg.
*--   B06的场合
      WHEN 'B06'.
*---    值的计算
        PERFORM frm_value_proc USING '21'
              lh_bseg.
*--   B07的场合
      WHEN 'B07'.
*---    值的计算
        PERFORM frm_value_proc USING '22'
              lh_bseg.
*--   B08的场合
      WHEN 'B08'.
*---    值的计算
        PERFORM frm_value_proc USING '23'
              lh_bseg.
*--   B09的场合
      WHEN 'B09'.
*---    值的计算
        PERFORM frm_value_proc USING '24'
              lh_bseg.
*--   C01的场合
      WHEN 'C01'.
*---    值的计算
        PERFORM frm_value_proc USING '28'
              lh_bseg.
*--   C02的场合
      WHEN 'C02'.
*---    值的计算
        PERFORM frm_value_proc USING '29'
              lh_bseg.
*--   C03的场合
      WHEN 'C03'.
*---    值的计算
        PERFORM frm_value_proc USING '30'
              lh_bseg.
*--   C04的场合
      WHEN 'C04'.
*---    值的计算
        PERFORM frm_value_proc USING '31'
              lh_bseg.
*--   C05的场合
      WHEN 'C05'.
*---    值的计算
        PERFORM frm_value_proc USING '32'
              lh_bseg.
*--   D01的场合
      WHEN 'D01'.
*---    值的计算
        PERFORM frm_value_proc USING '34'
              lh_bseg.
*--   D02的场合
      WHEN 'D02'.
*---    值的计算
        PERFORM frm_value_proc USING '35'
              lh_bseg.
*--   D03的场合
      WHEN 'D03'.
*---    值的计算
        PERFORM frm_value_proc USING '36'
              lh_bseg.
*--   D04的场合
      WHEN 'D04'.
*---    值的计算
        PERFORM frm_value_proc USING '37'
              lh_bseg.
*--   D05的场合
      WHEN 'D05'.
*---    值的计算
        PERFORM frm_value_proc USING '38'
              lh_bseg.
*--   E01的场合
      WHEN 'E01'.
*---    值的计算
        PERFORM frm_value_proc USING '42'
              lh_bseg.
*--   E02的场合
      WHEN 'E02'.
*---    值的计算
        PERFORM frm_value_proc USING '44'
              lh_bseg.
*--   E03的场合
      WHEN 'E03'.
*---    值的计算
        PERFORM frm_value_proc USING '45'
              lh_bseg.
*--   E04的场合
      WHEN 'E04'.
*---    值的计算
        PERFORM frm_value_proc USING '46'
              lh_bseg.
*--   F01的场合
      WHEN 'F01'.
*---    值的计算
        PERFORM frm_value_proc USING '48'
              lh_bseg.
*--   F02的场合
      WHEN 'F02'.
*---    值的计算
        PERFORM frm_value_proc USING '49'
              lh_bseg.
*--   F03的场合
      WHEN 'F03'.
*---    值的计算
        PERFORM frm_value_proc USING '51'
              lh_bseg.
*--   G01的场合
      WHEN 'G01'.
*---    值的计算
        PERFORM frm_value_proc USING '54'
              lh_bseg.
*--   上记以外的场合
      WHEN OTHERS.

    ENDCASE.

  ENDLOOP.


  IF gt_xjll IS INITIAL.
    MESSAGE s003 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  SORT gt_xjll BY rstgr ASCENDING.

* 最终用表的编辑
  CLEAR ls_xjll.
  LOOP AT gt_xjll INTO lh_xjll.

    ls_xjll-hbyfs = ls_xjll-hbyfs + lh_xjll-hbyfs. "本月发生
    ls_xjll-hqmlj = ls_xjll-hqmlj + lh_xjll-hqmlj. "期末累计
    ls_xjll-hsntq = ls_xjll-hsntq + lh_xjll-hsntq. "上年同期
    ls_xjll-rstgr = lh_xjll-rstgr.   "付款原因代码
    ls_xjll-hcnum = lh_xjll-hcnum.   "行次号
    AT END OF rstgr.
      APPEND ls_xjll TO gt_output.
      CLEAR ls_xjll.
    ENDAT.

    CLEAR lh_xjll.

  ENDLOOP.
*--Start update by IBM-SXB 2016.09.12 Assignment #1002
  IF lt_faglflext IS NOT INITIAL.

    DATA(lv_last) = p_ryear - 1.

    LOOP AT lt_faglflext INTO ls_faglflext WHERE ryear = p_ryear.
      lv_this_year = lv_this_year + ls_faglflext-hslvt.
    ENDLOOP.

    LOOP AT lt_faglflext INTO ls_faglflext WHERE ryear = lv_last.
      lv_last_year = lv_last_year + ls_faglflext-hslvt.
    ENDLOOP.

    ls_xjll-hcnum = '56'. "行次号
    SORT lt_faglflext BY ryear ASCENDING.
    READ TABLE lt_faglflext INTO ls_faglflext WITH KEY ryear = p_ryear.
    IF sy-subrc = 0.
*--------修改开始 2017-03-06 IBM-ZJX------------------------------*
* 本月发生额：目前取的是年初数，需要修改为本月的月初数，          *
* 即上月的月末数。                                                *
*-----------------------------------------------------------------*
*       ls_xjll-hbyfs = lv_this_year. "本月发生
*------修改结束  2017-03-06 IBM-ZJX-------------------------------*
      ls_xjll-hqmlj = lv_this_year. "期末累计
    ENDIF.

*--------修改开始 2017-03-06 IBM-ZJX------------------------------*
* 本月发生额：目前取的是年初数，需要修改为本月的月初数，          *
* 即上月的月末数。                                                *
*-----------------------------------------------------------------*
*--   本月发生额
    LOOP AT lt_faglflext INTO ls_faglflext WHERE ryear = p_ryear.
      CASE p_monat.
        WHEN '01'.
          ls_xjll-hbyfs = ls_xjll-hbyfs + ls_faglflext-hslvt.
        WHEN '02'.
          ls_xjll-hbyfs = ls_xjll-hbyfs + ls_faglflext-hslvt
                        + ls_faglflext-hsl01.
        WHEN '03'.
          ls_xjll-hbyfs = ls_xjll-hbyfs + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02.
        WHEN '04'.
          ls_xjll-hbyfs = ls_xjll-hbyfs + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03.
        WHEN '05'.
          ls_xjll-hbyfs = ls_xjll-hbyfs + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03 + ls_faglflext-hsl04.
        WHEN '06'.
          ls_xjll-hbyfs = ls_xjll-hbyfs + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03 + ls_faglflext-hsl04
                        + ls_faglflext-hsl05.
        WHEN '07'.
          ls_xjll-hbyfs = ls_xjll-hbyfs + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03 + ls_faglflext-hsl04
                        + ls_faglflext-hsl05 + ls_faglflext-hsl06.
        WHEN '08'.
          ls_xjll-hbyfs = ls_xjll-hbyfs + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03 + ls_faglflext-hsl04
                        + ls_faglflext-hsl05 + ls_faglflext-hsl06
                        + ls_faglflext-hsl07.
        WHEN '09'.
          ls_xjll-hbyfs = ls_xjll-hbyfs + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03 + ls_faglflext-hsl04
                        + ls_faglflext-hsl05 + ls_faglflext-hsl06
                        + ls_faglflext-hsl07 + ls_faglflext-hsl08.
        WHEN '10'.
          ls_xjll-hbyfs = ls_xjll-hbyfs + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03 + ls_faglflext-hsl04
                        + ls_faglflext-hsl05 + ls_faglflext-hsl06
                        + ls_faglflext-hsl07 + ls_faglflext-hsl08
                        + ls_faglflext-hsl09.
        WHEN '11'.
          ls_xjll-hbyfs = ls_xjll-hbyfs + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03 + ls_faglflext-hsl04
                        + ls_faglflext-hsl05 + ls_faglflext-hsl06
                        + ls_faglflext-hsl07 + ls_faglflext-hsl08
                        + ls_faglflext-hsl09 + ls_faglflext-hsl10.
*modify by xubin 20180410 ERP-YW201804020027
*        when '12'.
        WHEN '12' OR '13' OR '14' OR '15' OR '16'.
*end modify by xubin 20180410 ERP-YW201804020027
          ls_xjll-hbyfs = ls_xjll-hbyfs + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03 + ls_faglflext-hsl04
                        + ls_faglflext-hsl05 + ls_faglflext-hsl06
                        + ls_faglflext-hsl07 + ls_faglflext-hsl08
                        + ls_faglflext-hsl09 + ls_faglflext-hsl10
                        + ls_faglflext-hsl11.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
*--   上年同期
    LOOP AT lt_faglflext INTO ls_faglflext WHERE ryear = lv_last.
      CASE p_monat.
        WHEN '01'.
          ls_xjll-hsntq = ls_xjll-hsntq + ls_faglflext-hslvt.
        WHEN '02'.
          ls_xjll-hsntq = ls_xjll-hsntq + ls_faglflext-hslvt
                        + ls_faglflext-hsl01.
        WHEN '03'.
          ls_xjll-hsntq = ls_xjll-hsntq + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02.
        WHEN '04'.
          ls_xjll-hsntq = ls_xjll-hsntq + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03.
        WHEN '05'.
          ls_xjll-hsntq = ls_xjll-hsntq + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03 + ls_faglflext-hsl04.
        WHEN '06'.
          ls_xjll-hsntq = ls_xjll-hsntq + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03 + ls_faglflext-hsl04
                        + ls_faglflext-hsl05.
        WHEN '07'.
          ls_xjll-hsntq = ls_xjll-hsntq + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03 + ls_faglflext-hsl04
                        + ls_faglflext-hsl05 + ls_faglflext-hsl06.
        WHEN '08'.
          ls_xjll-hsntq = ls_xjll-hsntq + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03 + ls_faglflext-hsl04
                        + ls_faglflext-hsl05 + ls_faglflext-hsl06
                        + ls_faglflext-hsl07.
        WHEN '09'.
          ls_xjll-hsntq = ls_xjll-hsntq + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03 + ls_faglflext-hsl04
                        + ls_faglflext-hsl05 + ls_faglflext-hsl06
                        + ls_faglflext-hsl07 + ls_faglflext-hsl08.
        WHEN '10'.
          ls_xjll-hsntq = ls_xjll-hsntq + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03 + ls_faglflext-hsl04
                        + ls_faglflext-hsl05 + ls_faglflext-hsl06
                        + ls_faglflext-hsl07 + ls_faglflext-hsl08
                        + ls_faglflext-hsl09.
        WHEN '11'.
          ls_xjll-hsntq = ls_xjll-hsntq + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03 + ls_faglflext-hsl04
                        + ls_faglflext-hsl05 + ls_faglflext-hsl06
                        + ls_faglflext-hsl07 + ls_faglflext-hsl08
                        + ls_faglflext-hsl09 + ls_faglflext-hsl10.
*modify by xubin 20180410 ERP-YW201804020027
*        when '12'.
        WHEN '12' OR '13' OR '14' OR '15' OR '16'.
*end modify by xubin 20180410 ERP-YW201804020027
          ls_xjll-hsntq = ls_xjll-hsntq + ls_faglflext-hslvt
                        + ls_faglflext-hsl01 + ls_faglflext-hsl02
                        + ls_faglflext-hsl03 + ls_faglflext-hsl04
                        + ls_faglflext-hsl05 + ls_faglflext-hsl06
                        + ls_faglflext-hsl07 + ls_faglflext-hsl08
                        + ls_faglflext-hsl09 + ls_faglflext-hsl10
                        + ls_faglflext-hsl11.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
*------修改结束  2017-03-06 IBM-ZJX-------------------------------*

    READ TABLE lt_faglflext INTO ls_faglflext WITH KEY ryear = lv_last_year.
    IF sy-subrc = 0.
*--------修改开始 2017-03-06 IBM-ZJX------------------------------*
* 本月发生额：目前取的是年初数，需要修改为本月的月初数，          *
* 即上月的月末数。                                                *
*-----------------------------------------------------------------*
*        ls_xjll-hsntq = lv_last_year. "上年同期
*------修改结束  2017-03-06 IBM-ZJX-------------------------------*
    ENDIF.

    APPEND ls_xjll TO gt_output.
    CLEAR ls_xjll.
  ENDIF.

* 业务顾问要求增加ALV,将报表模板中的公式，翻译为代码
  PERFORM frm_alv_f1.

  CLEAR ls_alv_out.
  LOOP AT gt_alv_out INTO ls_alv_out.
    LOOP AT  gt_output INTO ls_xjll WHERE hcnum = ls_alv_out-hcnum.
      ls_alv_out-hbyfs = ls_xjll-hbyfs.
      ls_alv_out-hqmlj = ls_xjll-hqmlj.    "期末累计
      ls_alv_out-hsntq = ls_xjll-hsntq."TYPE hslxx12,    "上年同期
      MODIFY gt_alv_out FROM ls_alv_out.
      CLEAR ls_alv_out.
    ENDLOOP.
    "行次 = 15 汇总  经营活动现金流入小计
    IF ls_alv_out-hcnum = 15 .
      LOOP AT  gt_output INTO ls_xjll WHERE rstgr = 'A01'
                                          OR rstgr = 'A04'
                                          OR rstgr = 'A08'
                                          OR rstgr = 'A09'
                                          OR rstgr = 'A011'
                                          OR rstgr = 'A12'
                                          OR rstgr = 'A13' .
        ls_alv_out-hbyfs = ls_alv_out-hbyfs + ls_xjll-hbyfs.
        ls_alv_out-hqmlj = ls_alv_out-hqmlj + ls_xjll-hqmlj.    "期末累计
        ls_alv_out-hsntq = ls_alv_out-hsntq + ls_xjll-hsntq."TYPE hslxx12,    "上年同期

      ENDLOOP.

      MODIFY gt_alv_out FROM ls_alv_out.
*    endif.

      " 行次 = 25  汇总   经营活动现金流出小计
    ELSEIF ls_alv_out-hcnum = 25.
      LOOP AT  gt_output INTO ls_xjll WHERE rstgr = 'B01'
                                          OR rstgr = 'B05'
                                          OR rstgr = 'B08'
                                          OR rstgr = 'B09'
                                          OR rstgr = 'B07' .
        ls_alv_out-hbyfs = ls_alv_out-hbyfs + ls_xjll-hbyfs.
        ls_alv_out-hqmlj = ls_alv_out-hqmlj + ls_xjll-hqmlj.    "期末累计
        ls_alv_out-hsntq = ls_alv_out-hsntq + ls_xjll-hsntq."TYPE hslxx12,    "上年同期

      ENDLOOP.
      MODIFY gt_alv_out FROM ls_alv_out.
*    endif.

      " 行次 = 33   投资活动现金流入小计
    ELSEIF ls_alv_out-hcnum = 33.
      LOOP AT  gt_output INTO ls_xjll WHERE rstgr = 'C01'
                                          OR rstgr = 'C02'
                                          OR rstgr = 'C03'
                                          OR rstgr = 'C04'
                                          OR rstgr = 'C05' .
        ls_alv_out-hbyfs = ls_alv_out-hbyfs + ls_xjll-hbyfs.
        ls_alv_out-hqmlj = ls_alv_out-hqmlj + ls_xjll-hqmlj.    "期末累计
        ls_alv_out-hsntq = ls_alv_out-hsntq + ls_xjll-hsntq."TYPE hslxx12,    "上年同期

      ENDLOOP.
      MODIFY gt_alv_out FROM ls_alv_out.
*    endif.

      "行次 = 39   投资活动现金流出小计
    ELSEIF ls_alv_out-hcnum = 39.
      LOOP AT  gt_output INTO ls_xjll WHERE rstgr = 'D01'
                                          OR rstgr = 'D02'
                                          OR rstgr = 'D03'
                                          OR rstgr = 'D04'
                                          OR rstgr = 'D05' .
        ls_alv_out-hbyfs = ls_alv_out-hbyfs + ls_xjll-hbyfs.
        ls_alv_out-hqmlj = ls_alv_out-hqmlj + ls_xjll-hqmlj.    "期末累计
        ls_alv_out-hsntq = ls_alv_out-hsntq + ls_xjll-hsntq."TYPE hslxx12,    "上年同期

      ENDLOOP.
      MODIFY gt_alv_out FROM ls_alv_out.
*    endif.



    ELSEIF ls_alv_out-hcnum = 47.
      LOOP AT  gt_output INTO ls_xjll WHERE rstgr = 'E01'
                                          OR rstgr = 'E02'
                                          OR rstgr = 'E03'
                                          OR rstgr = 'E04'.
        ls_alv_out-hbyfs = ls_alv_out-hbyfs + ls_xjll-hbyfs.
        ls_alv_out-hqmlj = ls_alv_out-hqmlj + ls_xjll-hqmlj.    "期末累计
        ls_alv_out-hsntq = ls_alv_out-hsntq + ls_xjll-hsntq."TYPE hslxx12,    "上年同期

      ENDLOOP.
      MODIFY gt_alv_out FROM ls_alv_out.

*    endif.

      "行次 = 52  筹资活动现金流出小计
    ELSEIF ls_alv_out-hcnum = 52.
      LOOP AT  gt_output INTO ls_xjll WHERE rstgr = 'F01'
                                          OR rstgr = 'F02'
                                          OR rstgr = 'F03' ..
        ls_alv_out-hbyfs = ls_alv_out-hbyfs + ls_xjll-hbyfs.
        ls_alv_out-hqmlj = ls_alv_out-hqmlj + ls_xjll-hqmlj.    "期末累计
        ls_alv_out-hsntq = ls_alv_out-hsntq + ls_xjll-hsntq."TYPE hslxx12,    "上年同期

      ENDLOOP.
      MODIFY gt_alv_out FROM ls_alv_out.

    ELSE.
      " do nothing...
    ENDIF.
    CLEAR ls_alv_out.
  ENDLOOP.

*" 行次 = 26, 经营活动产生的现金流量净额
  READ TABLE gt_alv_out INTO wa_alv_out WITH KEY hcnum = 15.
  READ TABLE gt_alv_out INTO wa_alv_out2 WITH KEY hcnum = 25.

  CLEAR ls_alv_out.
  ls_alv_out-hbyfs = wa_alv_out-hbyfs - wa_alv_out2-hbyfs.
  ls_alv_out-hqmlj =  wa_alv_out-hqmlj -  wa_alv_out2-hqmlj.    "期末累计
  ls_alv_out-hsntq =  wa_alv_out-hsntq -  wa_alv_out2-hsntq."TYPE hslxx12,    "上年同期

  MODIFY gt_alv_out FROM ls_alv_out TRANSPORTING hbyfs hqmlj hsntq  WHERE hcnum = 26.
  CLEAR wa_alv_out.
  CLEAR wa_alv_out2.
  CLEAR ls_alv_out.

*" 行次 = 40, 投资活动产生的现金流量净额
  READ TABLE gt_alv_out INTO wa_alv_out WITH KEY hcnum = 33.
  READ TABLE gt_alv_out INTO wa_alv_out2 WITH KEY hcnum = 39.

  ls_alv_out-hbyfs = wa_alv_out-hbyfs - wa_alv_out2-hbyfs.
  ls_alv_out-hqmlj =  wa_alv_out-hqmlj -  wa_alv_out2-hqmlj.    "期末累计
  ls_alv_out-hsntq =  wa_alv_out-hsntq -  wa_alv_out2-hsntq."TYPE hslxx12,    "上年同期

  MODIFY gt_alv_out FROM ls_alv_out TRANSPORTING hbyfs hqmlj hsntq  WHERE hcnum = 40.
  CLEAR wa_alv_out.
  CLEAR wa_alv_out2.
  CLEAR ls_alv_out.

*" 行次 = 53, 筹资活动产生的现金流量净额
  READ TABLE gt_alv_out INTO wa_alv_out WITH KEY hcnum = 47.
  READ TABLE gt_alv_out INTO wa_alv_out2 WITH KEY hcnum = 52.

  ls_alv_out-hbyfs = wa_alv_out-hbyfs - wa_alv_out2-hbyfs.
  ls_alv_out-hqmlj =  wa_alv_out-hqmlj -  wa_alv_out2-hqmlj.    "期末累计
  ls_alv_out-hsntq =  wa_alv_out-hsntq -  wa_alv_out2-hsntq."TYPE hslxx12,    "上年同期

  MODIFY gt_alv_out FROM ls_alv_out TRANSPORTING hbyfs hqmlj hsntq  WHERE hcnum = 53.
  CLEAR wa_alv_out.
  CLEAR wa_alv_out2.
  CLEAR ls_alv_out.


*" 行次 = 55, 五、现金及现金等价物净增加额
  READ TABLE gt_alv_out INTO wa_alv_out WITH KEY hcnum = 26.
  READ TABLE gt_alv_out INTO wa_alv_out2 WITH KEY hcnum = 40.
  READ TABLE gt_alv_out INTO wa_alv_out3 WITH KEY hcnum = 53.
  READ TABLE gt_alv_out INTO wa_alv_out4 WITH KEY hcnum = 54.

  ls_alv_out-hbyfs =  wa_alv_out-hbyfs + wa_alv_out2-hbyfs + wa_alv_out3-hbyfs + wa_alv_out4-hbyfs.
  ls_alv_out-hqmlj =  wa_alv_out-hqmlj +  wa_alv_out2-hqmlj + wa_alv_out3-hqmlj + wa_alv_out4-hqmlj . "期末累计
  ls_alv_out-hsntq =  wa_alv_out-hsntq +  wa_alv_out2-hsntq + wa_alv_out3-hsntq + wa_alv_out4-hsntq."TYPE hslxx12,    "上年同期

  MODIFY gt_alv_out FROM ls_alv_out TRANSPORTING hbyfs hqmlj hsntq  WHERE hcnum = 55.
  CLEAR wa_alv_out.
  CLEAR wa_alv_out2.
  CLEAR wa_alv_out3.
  CLEAR wa_alv_out4.
  CLEAR ls_alv_out.

*" 行次 =  57, 六、年末现金及现金等价物余额
  READ TABLE gt_alv_out INTO wa_alv_out WITH KEY hcnum = 55.
  READ TABLE gt_alv_out INTO wa_alv_out2 WITH KEY hcnum = 56.
  ls_alv_out-hbyfs = wa_alv_out-hbyfs + wa_alv_out2-hbyfs.
  ls_alv_out-hqmlj =  wa_alv_out-hqmlj +  wa_alv_out2-hqmlj.    "期末累计
  ls_alv_out-hsntq =  wa_alv_out-hsntq +  wa_alv_out2-hsntq."TYPE hslxx12,    "上年同期
  MODIFY gt_alv_out FROM ls_alv_out TRANSPORTING hbyfs hqmlj hsntq  WHERE hcnum = 57.
  CLEAR wa_alv_out.
  CLEAR wa_alv_out2.
  CLEAR ls_alv_out.
ENDFORM.

FORM frm_value_proc USING i_hcnum  TYPE i
        i_th_bseg TYPE typ_bseg.

* 局部定义
  DATA:
    lw_year TYPE faglflexa-ryear,
    ls_xjll TYPE zfi01_xjll_s.

* 本月发生
*modify by xubin 20180410 ERP-YW201804020027
*如果13-16月，本月发生额是12-输入月的总和
*  if  i_th_bseg-monat = p_monat and i_th_bseg-gjahr = p_ryear.
**-  借方标识的场合
*    if i_th_bseg-shkzg = 'S'.
*      ls_xjll-hbyfs = i_th_bseg-dmbtr.
**-  贷方标识的场合
*    else.
*      ls_xjll-hbyfs = 0 - i_th_bseg-dmbtr.
*    endif.
*  endif.
  IF p_monat <= 12.
    IF  i_th_bseg-monat = p_monat AND i_th_bseg-gjahr = p_ryear.
*-  借方标识的场合
      IF i_th_bseg-shkzg = 'S'.
        ls_xjll-hbyfs = i_th_bseg-dmbtr.
*-  贷方标识的场合
      ELSE.
        ls_xjll-hbyfs = 0 - i_th_bseg-dmbtr.
      ENDIF.

*add by wangjunchuan on 20180521 bpmno:ERP-YW201805090294
      PERFORM frm_collect_detail USING i_th_bseg 'HBYFS'.
*add end.
    ENDIF.
  ELSEIF p_monat >= 13 AND p_monat <= 16.
    IF i_th_bseg-monat >= 12 AND i_th_bseg-monat <= p_monat AND i_th_bseg-gjahr = p_ryear.
*-  借方标识的场合
      IF i_th_bseg-shkzg = 'S'.
        ls_xjll-hbyfs = i_th_bseg-dmbtr.
*-  贷方标识的场合
      ELSE.
        ls_xjll-hbyfs = 0 - i_th_bseg-dmbtr.
      ENDIF.
*ADD BY WANGJUNCHUAN ON 20180521 BPMNO:ERP-YW201805090294
      PERFORM frm_collect_detail USING i_th_bseg 'HBYFS'.
*add end.
    ENDIF.
  ENDIF.
*end modify by xubin 20180410 ERP-YW201804020027

  IF i_th_bseg-rstgr = 'B01' OR i_th_bseg-rstgr = 'B05' OR i_th_bseg-rstgr = 'B07' OR i_th_bseg-rstgr = 'B08' OR
  i_th_bseg-rstgr = 'B09' OR i_th_bseg-rstgr = 'D01' OR i_th_bseg-rstgr = 'D02' OR i_th_bseg-rstgr = 'D03' OR
  i_th_bseg-rstgr = 'D04' OR i_th_bseg-rstgr = 'D05' OR i_th_bseg-rstgr = 'F01' OR i_th_bseg-rstgr = 'F02' OR
  i_th_bseg-rstgr = 'F03'.
    ls_xjll-hbyfs = - ls_xjll-hbyfs.
  ENDIF.
* 期末累计
  IF i_th_bseg-gjahr = p_ryear.
*- 借方标识的场合
    IF i_th_bseg-shkzg = 'S'.
      ls_xjll-hqmlj = i_th_bseg-dmbtr.
*- 贷方标识的场合
    ELSE.
      ls_xjll-hqmlj = 0 - i_th_bseg-dmbtr.
    ENDIF.
*add by wangjunchuan on 20180521 bpmno:ERP-YW201805090294
    PERFORM frm_collect_detail USING i_th_bseg 'HQMLJ'.
*add end.
  ENDIF.

  IF i_th_bseg-rstgr = 'B01' OR
      i_th_bseg-rstgr = 'B05' OR
      i_th_bseg-rstgr = 'B07' OR
      i_th_bseg-rstgr = 'B08' OR
      i_th_bseg-rstgr = 'B09' OR
      i_th_bseg-rstgr = 'D01' OR
      i_th_bseg-rstgr = 'D02' OR
      i_th_bseg-rstgr = 'D03' OR
      i_th_bseg-rstgr = 'D04' OR
      i_th_bseg-rstgr = 'D05' OR
      i_th_bseg-rstgr = 'F01' OR
      i_th_bseg-rstgr = 'F02' OR
      i_th_bseg-rstgr = 'F03'.
    ls_xjll-hqmlj = - ls_xjll-hqmlj.
  ENDIF.

* 上年同期
  lw_year = p_ryear - 1.
  IF i_th_bseg-gjahr = lw_year.
*- 借方标识的场合
    IF i_th_bseg-shkzg = 'S'.
      ls_xjll-hsntq = i_th_bseg-dmbtr.
*- 贷方标识的场合
    ELSE.
      ls_xjll-hsntq = 0 - i_th_bseg-dmbtr.
    ENDIF.

*add by wangjunchuan on 20180521 bpmno:ERP-YW201805090294
    PERFORM frm_collect_detail USING i_th_bseg 'HSNTQ'.
*add end.
  ENDIF.

  IF i_th_bseg-rstgr = 'B01' OR i_th_bseg-rstgr = 'B05' OR i_th_bseg-rstgr = 'B07' OR i_th_bseg-rstgr = 'B08' OR
      i_th_bseg-rstgr = 'B09' OR i_th_bseg-rstgr = 'D01' OR i_th_bseg-rstgr = 'D02' OR i_th_bseg-rstgr = 'D03' OR
      i_th_bseg-rstgr = 'D04' OR i_th_bseg-rstgr = 'D05' OR i_th_bseg-rstgr = 'F01' OR i_th_bseg-rstgr = 'F02' OR
      i_th_bseg-rstgr = 'F03'.
    ls_xjll-hsntq = - ls_xjll-hsntq.
  ENDIF.

  ls_xjll-rstgr = i_th_bseg-rstgr.
  ls_xjll-hcnum = i_hcnum.
  APPEND ls_xjll TO gt_xjll.
*  COLLECT ls_xjll INTO gt_xjll.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DOWNLOAD_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_download_proc .

  DATA: l_file TYPE rlgrap-filename.

  CLEAR ls_wwwdata.
  ls_wwwdata-relid = 'MI'.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = '下载到本地文件'
*     default_extension    = 'XLS'           "缺省文件类型
      default_extension    = ',Excel Files,*.xls,All Files,*.*.'           "缺省文件类型
      default_file_name    = '现金流量表'
*     file_filter          = '文本文件(*.TXT)|*.TXT|Excel 文件 (*.XLS)|*.XLS;*.XLSX|所有文件 (*.*)|*.*|'
      file_filter          = 'Excel2003(*.xls)|*.xls|Excel2007(*.xlsx)|*.xlsx|'
      with_encoding        = 'X'
*     initial_directory    = 'C:\'
    CHANGING
      filename             = l_filename    "获得用户输入文件名
      path                 = l_path        "获得用户所选路径
      fullpath             = l_fullpath    "路径加文件名
      user_action          = l_user_action
      file_encoding        = l_encoding
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  "导出数据
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF sy-subrc <> 0 OR l_user_action <>
  cl_gui_frontend_services=>action_ok.
    RETURN.
  ENDIF.

  ls_wwwdata-objid = 'ZFIRPT15'.

  l_file = l_fullpath.
  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      key         = ls_wwwdata
      destination = l_file.

* 打开EXCEL并定位到sheet1
  CREATE OBJECT v_excel 'EXCEL.APPLICATION' . "创建EXCEL对象
*  SET PROPERTY OF V_EXCEL 'VISIBLE'  = 1.      "设置EXCEL属性可视
  CALL METHOD OF v_excel 'WORKBOOKS' = v_book. "激活工作簿

  CALL METHOD OF
    v_book
    'OPEN'
    EXPORTING
      #1 = l_fullpath. "打开指定路径EXCEL工作簿
  CALL METHOD OF
  v_excel
  'WORKSHEETS' = v_sheet
  EXPORTING
    #1           = 1.
  CALL METHOD OF
    v_sheet
    'SELECT'.

* EXCEL文件赋值
  PERFORM frm_excel_proc.

  SET PROPERTY OF v_excel 'VISIBLE' = 1.   "设置EXCEL属性可视
  CALL METHOD OF
    v_book
    'SAVE'.
  FREE : v_excel,v_book,v_sheet.

*  CREATE OBJECT v_excel 'Excel.Application'.
*
*  SET PROPERTY OF v_excel 'VISIBLE' = 1.
*
*  CALL METHOD OF v_excel 'Workbooks' = v_book.
*
*  CALL METHOD OF v_book 'Open' = v_book
*  EXPORTING
*    #1 = l_fullpath.
*
*  GET PROPERTY OF v_excel 'ACTIVECELL' = v_sheet.
*
*  CALL METHOD OF v_excel 'Worksheets' = v_sheet
*    EXPORTING
*    #1 = 1.
*
*  CALL METHOD OF v_sheet 'Activate'.
*
*
**** EXCEL文件赋值
*  PERFORM frm_excel_proc.
*
**--设置excel可见
**  CALL METHOD OF V_EXCEL 'Worksheets' = V_SHEET
**  EXPORTING
**    #1 = 1.
**  CALL METHOD OF V_SHEET 'Activate'.
*
**  CALL METHOD OF V_BOOK 'SAVEAS'
**  EXPORTING
**    #1 = L_FULLPATH.
*
**  CALL METHOD OF V_BOOK 'Exit' = V_EXCEL.
**  CALL METHOD OF V_BOOK 'QUIT'.
**  CALL METHOD OF V_EXCEL 'QUIT'.
*
*  FREE OBJECT v_cell.
*  FREE OBJECT v_sheet.
*  FREE OBJECT v_book.
*  FREE OBJECT v_excel.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_EXCEL_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_excel_proc .
* 局部定义
  DATA:
    lh_xjll     TYPE zfi01_xjll_s,
    lw_date(10) TYPE c,
    lw_x        TYPE i.

  DATA: l_butxt TYPE t001-butxt.
  CLEAR l_butxt.
  SELECT SINGLE butxt
  INTO l_butxt
  FROM t001
  WHERE bukrs =  s_bukrs-low.
  DATA: l_gsmc(50) TYPE c.
  CLEAR l_gsmc.
  CONCATENATE '编制单位:' l_butxt
  INTO l_gsmc.
  PERFORM cell_fill USING 3 1 l_gsmc.
* 日期的赋值
  CONCATENATE p_ryear
  '年'
  p_monat
  '月'
  INTO lw_date.

  PERFORM cell_fill USING 3 3 lw_date.

* 资产负债出力表的循环处理
  LOOP AT gt_output INTO lh_xjll.

    CLEAR lw_x.

*   EXCEL填充值
    lw_x = lh_xjll-hcnum + 4.                      "计算行
    PERFORM cell_fill USING lw_x 4  lh_xjll-hbyfs. "本月发生
    PERFORM cell_fill USING lw_x 5  lh_xjll-hqmlj. "期末累计
    PERFORM cell_fill USING lw_x 6  lh_xjll-hsntq. "上年同期

  ENDLOOP.
ENDFORM.


FORM cell_fill USING i j val.

* get cell
  CALL METHOD OF
  v_excel
  'Cells' = v_cell
  EXPORTING
    #1      = i
    #2      = j.

* set cell value
  SET PROPERTY OF v_cell 'Value' = val .

ENDFORM.                    "FILL_CELL
*&---------------------------------------------------------------------*
*&      Form  DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_display .
  DATA: lv_title            TYPE lvc_title,
        lv_callback_program TYPE sy-repid VALUE 'YFI00002'.
*        lv_callback_program TYPE sy-repid VALUE 'ZCOFCOFIRP_0003'.

  PERFORM alv_fieldcat_build CHANGING gt_fieldcat.
  PERFORM alv_layout_build.
  PERFORM alv_event_build.
  PERFORM alv_grid_title CHANGING lv_title.

  lv_callback_program = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = lv_callback_program
      i_callback_pf_status_set = 'ALV_PF_STATUS'
      i_callback_user_command  = 'ALV_USER_COMMAND'
      i_callback_top_of_page   = 'ALV_TOP_OF_PAGE'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
      i_grid_title             = lv_title
*     I_GRID_SETTINGS          =
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fieldcat
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              =
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = ' '
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT_LVC             =
*     IS_REPREP_ID_LVC         =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        =
*     I_HTML_HEIGHT_END        =
*     IT_ALV_GRAPHICS          =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = gt_alv_out
*   EXCEPTIONS
*     PROGRAM_ERROR            = 1
*     OTHERS                   = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM alv_fieldcat_build  CHANGING ct_fieldcat  TYPE lvc_t_fcat.
  DATA: ls_fieldcat TYPE lvc_s_fcat.

  CLEAR ct_fieldcat.

  PERFORM alv_fieldcat_fill USING 1   'XM_TXT'  '' '项目   ' CHANGING ct_fieldcat.
  PERFORM alv_fieldcat_fill USING 1   'RSTGR'  '' '原因代码   ' CHANGING ct_fieldcat.
  PERFORM alv_fieldcat_fill USING 2   'HCNUM'  '' '行次   ' CHANGING ct_fieldcat.
  PERFORM alv_fieldcat_fill USING 3   'HBYFS'  '' '本月发生         ' CHANGING ct_fieldcat.
  PERFORM alv_fieldcat_fill USING 4   'HQMLJ'  '' '期末累计         ' CHANGING ct_fieldcat.
  PERFORM alv_fieldcat_fill USING 5   'HSNTQ'  '' '上年同期            ' CHANGING ct_fieldcat.
  "perform alv_fieldcat_fill using 6   'hqmlj              '  '' '上年同期     ' changing ct_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_layout_build .
  gs_layout-zebra         = abap_true.
  gs_layout-cwidth_opt    = abap_true.
  gs_layout-smalltitle    = abap_true.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_EVENT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_event_build .
  DATA: ls_event  LIKE LINE OF gt_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
*   EXPORTING
*     I_LIST_TYPE           = 0
    IMPORTING
      et_events = gt_event
*   EXCEPTIONS
*     LIST_TYPE_WRONG       = 1
*     OTHERS    = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT gt_event INTO ls_event.
    IF ls_event-name EQ slis_ev_caller_exit_at_start.
      ls_event-form = 'ALV_CALLER_EXIT'.
    ENDIF.

    MODIFY gt_event FROM ls_event.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_TITLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_TITLE  text
*----------------------------------------------------------------------*
FORM alv_grid_title   CHANGING ch_alv_grid_title TYPE lvc_title.
  DATA: lv_lines        TYPE i,
        lv_lines_string TYPE string.

  IF gt_alv_out IS NOT INITIAL.
    lv_lines = lines( gt_alv_out ).
  ENDIF.

  lv_lines_string = lv_lines.

  ch_alv_grid_title = '条目数:' && lv_lines_string.
  CONDENSE ch_alv_grid_title.
ENDFORM.


FORM alv_fieldcat_fill USING col_pos      TYPE lvc_s_fcat-col_pos
                             fieldname    TYPE lvc_s_fcat-fieldname
                             no_out       TYPE lvc_s_fcat-no_out
                             reptext      TYPE lvc_s_fcat-reptext
                    CHANGING ct_fieldcat  TYPE lvc_t_fcat.

  DATA: ls_fieldcat TYPE lvc_s_fcat.

  ls_fieldcat-col_pos   = col_pos.
  ls_fieldcat-fieldname = to_upper( fieldname ).
  ls_fieldcat-no_out    = no_out.
  ls_fieldcat-reptext   = reptext.
  APPEND ls_fieldcat TO ct_fieldcat.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_F1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_alv_f1 .
  DATA: ls_alv_out  TYPE ty_alv_out.

  ls_alv_out-xm_txt = '一、经营活动产生的现金流量：'.
  ls_alv_out-rstgr = ''.
  ls_alv_out-hcnum  = 1.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.

  ls_alv_out-xm_txt = '    销售商品、提供劳务收到的现金'.
  ls_alv_out-rstgr = 'A01'.
  ls_alv_out-hcnum  = 2.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    客户存款和同业存放款项净增加额'.
  ls_alv_out-rstgr = 'A02'.
  ls_alv_out-hcnum  = 3.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '     向中央银行借款净增加额'.
  ls_alv_out-rstgr = 'A03'.
  ls_alv_out-hcnum  = 4.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    向其他金融机构拆入资金净增加额'.
  ls_alv_out-rstgr = 'A04'.
  ls_alv_out-hcnum  = 5.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    收到原保险合同保费取得的现金'.
  ls_alv_out-rstgr = 'A05'.
  ls_alv_out-hcnum  = 6.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    收到再保险业务现金净额'.
  ls_alv_out-rstgr = 'A06'.
  ls_alv_out-hcnum  = 7.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    保户储金及投资款净增加额'.
  ls_alv_out-rstgr = 'A07'.
  ls_alv_out-hcnum  = 8.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '        处置交易性金融资产净增加额'.
  ls_alv_out-rstgr = 'A08'.
  ls_alv_out-hcnum  = 9.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '     收取利息、手续费及佣金的现金'.
  ls_alv_out-rstgr = 'A09'.
  ls_alv_out-hcnum  = 10.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    拆入资金净增加额'.
  ls_alv_out-rstgr = 'A10'.
  ls_alv_out-hcnum  = 11.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    回购业务资金净增加额'.
  ls_alv_out-rstgr = 'A11'.
  ls_alv_out-hcnum  = 12.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    收到的税费返还'.
  ls_alv_out-rstgr = 'A12'.
  ls_alv_out-hcnum  = 13.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    收到其他与经营活动有关的现金'.
  ls_alv_out-rstgr = 'A13'.
  ls_alv_out-hcnum  = 14.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    经营活动现金流入小计'.
  ls_alv_out-rstgr = ''.
  ls_alv_out-hcnum  = 15.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '     购买商品、接受劳务支付的现金'.
  ls_alv_out-rstgr = 'B01'.
  ls_alv_out-hcnum  = 16.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    客户贷款及垫款净增加额'.
  ls_alv_out-rstgr = 'B02'.
  ls_alv_out-hcnum  = 17.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    存放中央银行和同业款项净增加额'.
  ls_alv_out-rstgr = 'B03'.
  ls_alv_out-hcnum  = 18.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    支付原保险合同赔付款项的现金'.
  ls_alv_out-rstgr = 'B04'.
  ls_alv_out-hcnum  = 19.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    支付利息、手续费及佣金的现金'.
  ls_alv_out-rstgr = 'B05'.
  ls_alv_out-hcnum  = 20.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    支付保单红利的现金'.
  ls_alv_out-rstgr = 'B06'.
  ls_alv_out-hcnum  = 21.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    支付给职工以及为职工支付的现金'.
  ls_alv_out-rstgr = 'B07'.
  ls_alv_out-hcnum  = 22.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    支付的各项税费'.
  ls_alv_out-rstgr = 'B08'.
  ls_alv_out-hcnum  = 23.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    支付其他与经营活动有关的现金'.
  ls_alv_out-rstgr = 'B09'.
  ls_alv_out-hcnum  = 24.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    经营活动现金流出小计'.
  ls_alv_out-rstgr = ''.
  ls_alv_out-hcnum  = 25.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    经营活动产生的现金流量净额'.
  ls_alv_out-rstgr = ''.
  ls_alv_out-hcnum  = 26.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    二、投资活动产生的现金流量：'.
  ls_alv_out-rstgr = ''.
  ls_alv_out-hcnum  = 27.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '        收回投资收到的现金'.
  ls_alv_out-rstgr = 'C01'.
  ls_alv_out-hcnum  = 28.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    取得投资收益收到的现金'.
  ls_alv_out-rstgr = 'C02'.
  ls_alv_out-hcnum  = 29.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '处置固定资产、无形资产和其他长期资产收回的现金净额'.
  ls_alv_out-rstgr = 'C03'.
  ls_alv_out-hcnum  = 30.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    处置子公司及其他营业单位收到的现金净额'.
  ls_alv_out-rstgr = 'C04'.
  ls_alv_out-hcnum  = 31.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    收到其他与投资活动有关的现金'.
  ls_alv_out-rstgr = 'C05'.
  ls_alv_out-hcnum  = 32.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '  投资活动现金流入小计'.
  ls_alv_out-rstgr = ''.
  ls_alv_out-hcnum  = 33.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.



  ls_alv_out-xm_txt = '      购建固定资产、无形资产和其他长期资产支付的现金'.
  ls_alv_out-rstgr = 'D01'.
  ls_alv_out-hcnum  = 34.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '         投资支付的现金'.
  ls_alv_out-rstgr = 'D02'.
  ls_alv_out-hcnum  = 35.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '          质押贷款净增加额'.
  ls_alv_out-rstgr = 'D03'.
  ls_alv_out-hcnum  = 36.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '          取得子公司及其他营业单位支付的现金净额'.
  ls_alv_out-rstgr = 'D04'.
  ls_alv_out-hcnum  = 37.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '        支付其他与投资活动有关的现金'.
  ls_alv_out-rstgr = 'D05'.
  ls_alv_out-hcnum  = 38.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.



  ls_alv_out-xm_txt = '        投资活动现金流出小计'.
  ls_alv_out-rstgr = ''.
  ls_alv_out-hcnum  = 39.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '      投资活动产生的现金流量净额'.
  ls_alv_out-rstgr = ''.
  ls_alv_out-hcnum  = 40.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '      三、筹资活动产生的现金流量：'.
  ls_alv_out-rstgr = ''.
  ls_alv_out-hcnum  = 41.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    吸收投资收到的现金'.
  ls_alv_out-rstgr = 'E01'.
  ls_alv_out-hcnum  = 42.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    其中：子公司吸收少数股东投资收到的现金'.
  ls_alv_out-rstgr = ''.
  ls_alv_out-hcnum  = 43.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '        取得借款收到的现金'.
  ls_alv_out-rstgr = 'E02'.
  ls_alv_out-hcnum  = 44.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '            发行债券收到的现金'.
  ls_alv_out-rstgr = 'E03'.
  ls_alv_out-hcnum  = 45.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.

  ls_alv_out-xm_txt = '               收到其他与筹资活动有关的现金'.
*  ls_alv_out-rstgr = 'E034'.
  ls_alv_out-rstgr = 'E04'.
  ls_alv_out-hcnum  = 46.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '   筹资活动现金流入小计'.
  ls_alv_out-rstgr = ''.
  ls_alv_out-hcnum  = 47.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '    偿还债务支付的现金'.
  ls_alv_out-rstgr = 'F01'.
  ls_alv_out-hcnum  = 48.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '  分配股利、利润或偿付利息支付的现金'.
  ls_alv_out-rstgr = 'F02'.
  ls_alv_out-hcnum  = 49.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.

  ls_alv_out-xm_txt = '   其中：子公司支付给少数股东的股利、利润'.
*  ls_alv_out-rstgr = 'F02'.
  ls_alv_out-hcnum  = 50.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '       支付其他与筹资活动有关的现金'.
  ls_alv_out-rstgr = 'F03'.
  ls_alv_out-hcnum  = 51.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.



  ls_alv_out-xm_txt = '      筹资活动现金流出小计'.
  ls_alv_out-rstgr = ''.
  ls_alv_out-hcnum  = 52.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.

  ls_alv_out-xm_txt = '筹资活动产生的现金流量净额'.
  ls_alv_out-rstgr = ''.
  ls_alv_out-hcnum  = 53.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '      四、汇率变动对现金的影响'.
  ls_alv_out-rstgr = 'G01'.
  ls_alv_out-hcnum  = 54.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '五、现金及现金等价物净增加额'.
  ls_alv_out-rstgr = ''.
  ls_alv_out-hcnum  = 55.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = '加：年初现金及现金等价物余额'.
  ls_alv_out-rstgr = ''.
  ls_alv_out-hcnum  = 56.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.


  ls_alv_out-xm_txt = ' 六、年末现金及现金等价物余额'.
  ls_alv_out-rstgr = ''.
  ls_alv_out-hcnum  = 57.
  APPEND ls_alv_out TO gt_alv_out.
  CLEAR ls_alv_out.

ENDFORM.


FORM alv_user_command USING r_ucomm TYPE sy-ucomm
                            ps_selfield TYPE slis_selfield.

  IF r_ucomm EQ '&DOWNLOAD'.
    PERFORM frm_download_proc.
  ELSEIF r_ucomm EQ '&BACK'.
    LEAVE TO SCREEN 0 .
  ELSEIF r_ucomm EQ '&F03' OR
         r_ucomm EQ '&F15'.
*" Release Excel
  ELSEIF r_ucomm EQ '&IC1'."add by wangjunchuan on 20180521 bpmno:ERP-YW201805090294
    PERFORM frm_fix_dbclick USING ps_selfield.
  ENDIF.

  ps_selfield-refresh = abap_true.

ENDFORM.


FORM alv_pf_status USING rt_excluded_fcode  TYPE slis_t_extab.

  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_excluded_fcode.

ENDFORM.


FORM alv_top_of_page .


  DATA: lt_list_commentary TYPE slis_t_listheader,
        ls_line            TYPE slis_listheader,
        l_lin              TYPE i,
        l_char(10)         TYPE c.

  DATA: lv_butxt  TYPE t001-butxt.

  SELECT SINGLE
    butxt
    INTO lv_butxt
    FROM t001
    WHERE bukrs IN s_bukrs.

  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = '编制单位：'.
  ls_line-info = s_bukrs-low && ' ' && lv_butxt.
  APPEND ls_line TO lt_list_commentary.


  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = '期间：'.
  ls_line-info = p_ryear && '年 ' &&  p_monat && '月'.
  APPEND ls_line TO lt_list_commentary.

  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = '单位：'.
  ls_line-info = '元'.
  APPEND ls_line TO lt_list_commentary.




  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_list_commentary
*     i_logo             = 'ENJOYSAP_LOGO'
      i_end_of_list_grid = space
      i_alv_form         = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_FIX_DBCLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PS_SELFIELD  text
*----------------------------------------------------------------------*
FORM frm_fix_dbclick  USING    ps_selfield TYPE slis_selfield.
  DATA ls_alv_out LIKE LINE OF gt_alv_out.
  DATA ls_bseg_detail_all LIKE LINE OF gt_bseg_detail_all.
  DATA lt_detail TYPE TABLE OF ty_detail.
  DATA BEGIN OF ls_cepct.
  DATA prctr TYPE cepct-prctr.
  DATA ktext TYPE cepct-ktext.
  DATA END OF ls_cepct.
  DATA lt_cepct LIKE TABLE OF ls_cepct.
  DATA BEGIN OF ls_cskt.
  DATA kostl TYPE cskt-kostl.
  DATA ktext TYPE cskt-ktext.
  DATA END OF ls_cskt.
  DATA lt_cskt LIKE TABLE OF ls_cskt.
  DATA BEGIN OF ls_lfa1.
  DATA lifnr TYPE lfa1-lifnr.
  DATA name1 TYPE lfa1-name1.
  DATA END OF ls_lfa1.
  DATA lt_lfa1 LIKE TABLE OF ls_lfa1.
  DATA BEGIN OF ls_kna1.
  DATA kunnr TYPE kna1-kunnr.
  DATA name1 TYPE kna1-name1.
  DATA END OF ls_kna1.
  DATA lt_kna1 LIKE TABLE OF ls_kna1.
  FIELD-SYMBOLS <l_detail> LIKE LINE OF gt_bseg_detail.

  REFRESH gt_bseg_detail.
*
  READ TABLE gt_alv_out INTO ls_alv_out INDEX ps_selfield-tabindex.
  CHECK sy-subrc = 0.
  IF ls_alv_out-rstgr IS INITIAL.
    MESSAGE s004 WITH '合计行不允许穿透' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  IF ( ls_alv_out-hbyfs = 0 AND ps_selfield-fieldname = 'HBYFS' ) OR
     ( ls_alv_out-hqmlj = 0 AND ps_selfield-fieldname = 'HQMLJ' ) OR
     ( ls_alv_out-hsntq = 0 AND ps_selfield-fieldname = 'HSNTQ' ).
    MESSAGE e004 WITH '暂无明细数据！' DISPLAY LIKE 'E'.
  ENDIF.

  LOOP AT gt_bseg_detail_all INTO ls_bseg_detail_all WHERE rstgr = ls_alv_out-rstgr
                                                       AND field = ps_selfield-fieldname.
    APPEND ls_bseg_detail_all TO gt_bseg_detail.
    CLEAR ls_bseg_detail_all.
  ENDLOOP.

  CHECK NOT gt_bseg_detail IS INITIAL.
*利润中心描述
  lt_detail = gt_bseg_detail.
  SORT lt_detail BY prctr.
  DELETE ADJACENT DUPLICATES FROM lt_detail COMPARING prctr.
  DELETE lt_detail WHERE prctr IS INITIAL.
  IF NOT lt_detail IS INITIAL.
    SELECT cepct~prctr
           cepct~ktext
      INTO TABLE lt_cepct
      FROM cepct
       FOR ALL ENTRIES IN lt_detail
     WHERE cepct~spras = sy-langu
       AND cepct~prctr = lt_detail-prctr
       AND cepct~kokrs = '3000'.
    IF sy-subrc = 0.
      SORT lt_cepct BY prctr.
    ENDIF.
  ENDIF.
*成本中心描述
  lt_detail = gt_bseg_detail.
  SORT lt_detail BY kostl.
  DELETE ADJACENT DUPLICATES FROM lt_detail COMPARING kostl.
  DELETE lt_detail WHERE kostl IS INITIAL.
  IF NOT lt_detail IS INITIAL.
    SELECT cskt~kostl
           cskt~ktext
      INTO TABLE lt_cskt
      FROM cskt
       FOR ALL ENTRIES IN lt_detail
     WHERE cskt~kostl = lt_detail-kostl
       AND cskt~spras = sy-langu
       AND cskt~kokrs = '3000'.
    IF sy-subrc = 0.
      SORT lt_cskt BY kostl.
    ENDIF.
  ENDIF.
*供应商描述
  lt_detail = gt_bseg_detail.
  SORT lt_detail BY lifnr.
  DELETE ADJACENT DUPLICATES FROM lt_detail COMPARING lifnr.
  DELETE lt_detail WHERE lifnr IS INITIAL.
  IF NOT lt_detail IS INITIAL.
    SELECT lfa1~lifnr
           lfa1~name1
      INTO TABLE lt_lfa1
      FROM lfa1
       FOR ALL ENTRIES IN lt_detail
     WHERE lfa1~lifnr = lt_detail-lifnr.
    IF sy-subrc = 0.
      SORT lt_lfa1 BY lifnr.
    ENDIF.
  ENDIF.
*客户描述
  lt_detail = gt_bseg_detail.
  SORT lt_detail BY kunnr.
  DELETE ADJACENT DUPLICATES FROM lt_detail COMPARING kunnr.
  DELETE lt_detail WHERE kunnr IS INITIAL.
  IF NOT lt_detail IS INITIAL.
    SELECT kna1~kunnr
           kna1~name1
      INTO TABLE lt_kna1
      FROM kna1
       FOR ALL ENTRIES IN lt_detail
     WHERE kna1~kunnr = lt_detail-kunnr.
    IF sy-subrc = 0.
      SORT lt_kna1 BY kunnr.
    ENDIF.
  ENDIF.
*文本赋值
  LOOP AT gt_bseg_detail ASSIGNING <l_detail>.
    READ TABLE lt_cepct INTO ls_cepct
      WITH KEY prctr = <l_detail>-prctr BINARY SEARCH.
    IF sy-subrc = 0.
      <l_detail>-prctr_text = ls_cepct-ktext.
    ENDIF.
    READ TABLE lt_cskt INTO ls_cskt
      WITH KEY kostl = <l_detail>-kostl BINARY SEARCH.
    IF sy-subrc = 0.
      <l_detail>-kostl_text = ls_cskt-ktext.
    ENDIF.
    READ TABLE lt_lfa1 INTO ls_lfa1
      WITH KEY lifnr = <l_detail>-lifnr BINARY SEARCH.
    IF sy-subrc = 0.
      <l_detail>-lifnr_text = ls_lfa1-name1.
    ENDIF.
    READ TABLE lt_kna1 INTO ls_kna1
      WITH KEY kunnr = <l_detail>-kunnr BINARY SEARCH.
    IF sy-subrc = 0.
      <l_detail>-kunnr_text = ls_kna1-name1.
    ENDIF.
*" 反记账处理
    IF <l_detail>-shkzg EQ 'S'.
      IF <l_detail>-xnegp EQ 'X'.
        <l_detail>-shkzg = 'H'.
      ENDIF.
    ELSEIF <l_detail>-shkzg EQ 'H'.
      IF <l_detail>-xnegp EQ 'X'.
        <l_detail>-shkzg = 'S'.
      ENDIF.
      <l_detail>-dmbtr = <l_detail>-dmbtr * -1.
      <l_detail>-wrbtr = <l_detail>-wrbtr * -1.
      <l_detail>-menge = <l_detail>-menge * -1.

    ELSE.
      " do nothing...
    ENDIF.
  ENDLOOP.

*
  PERFORM frm_item_display.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_COLLECT_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_TH_BSEG  text
*      -->P_3363   text
*----------------------------------------------------------------------*
FORM frm_collect_detail  USING    ps_bseg TYPE typ_bseg
                                  pv_field.
  DATA ls_bseg_detail_all LIKE LINE OF gt_bseg_detail_all.

  MOVE-CORRESPONDING ps_bseg TO ls_bseg_detail_all.
  ls_bseg_detail_all-field = pv_field.
  APPEND ls_bseg_detail_all TO gt_bseg_detail_all.
  CLEAR ls_bseg_detail_all.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ITEM_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_item_display .
  DATA: lv_callback_program TYPE sy-repid.
  DATA: lv_title        TYPE lvc_title,
        ls_grid_setting TYPE lvc_s_glay,
        ls_layout       TYPE lvc_s_layo,
        ls_variant      TYPE disvariant,
        lt_fieldcat     TYPE lvc_t_fcat,
        ls_fieldcat     TYPE lvc_s_fcat.

  DATA: lt_sort TYPE lvc_t_sort.

  DATA: lv_lines        TYPE i,
        lv_lines_string TYPE string.

*  if gt_output is not initial.
  IF gt_bseg_detail IS NOT INITIAL.
*    lv_lines = lines( gt_output ).
    lv_lines = lines( gt_bseg_detail ).
  ENDIF.

  lv_lines_string = lv_lines.

  lv_title = '条目数:' && lv_lines_string.
  CONDENSE lv_title.

*" Layout
  ls_layout-cwidth_opt    = 'X'.
  ls_layout-zebra         = 'X'.
  ls_layout-smalltitle    = 'X'.

*" Layout Variant
  ls_variant-report = 'YFI00002'.
  ls_variant-handle = 'L2ND'. " 2级ALV明细界面


  DEFINE mcr_fieldcat.
    ls_fieldcat-fieldname = &1."GJAHR'.
    ls_fieldcat-coltext   = &2."'会计年度'.
    ls_fieldcat-scrtext_l = &2."'会计年度'.
    ls_fieldcat-scrtext_m = &2."'会计年度'.
    ls_fieldcat-scrtext_s = &2."'会计年度'.
    IF ls_fieldcat-fieldname = 'BELNR'.
      ls_fieldcat-hotspot = 'X'.
    ENDIF.
    APPEND ls_fieldcat TO lt_fieldcat.
    CLEAR ls_fieldcat.
  END-OF-DEFINITION.
  mcr_fieldcat 'GJAHR' '会计年度'.
  mcr_fieldcat 'MONAT' '月份'.
  mcr_fieldcat 'BUKRS' '公司代码'.
  mcr_fieldcat 'BELNR' '会计凭证编号'.
  mcr_fieldcat 'BUZEI' '行项目数'.
  mcr_fieldcat 'SHKZG' '借贷标识'.
  mcr_fieldcat 'DMBTR' '本位币金额'.
  mcr_fieldcat 'PRCTR' '利润中心'.
  mcr_fieldcat 'RSTGR' '付款原因代码'.
  mcr_fieldcat 'XREVERSAL' '是否冲销'.
  mcr_fieldcat 'BLDAT' '凭证日期'.
  mcr_fieldcat 'BUDAT' '过帐日期'.
  mcr_fieldcat 'BSTAT' '凭证状态'.
  mcr_fieldcat 'WAERS' '货币'.
  mcr_fieldcat 'HWAER' '本位币'.
  mcr_fieldcat 'BKTXT' '凭证抬头文本'.
  mcr_fieldcat 'BSCHL' '记账码'.
  mcr_fieldcat 'XNEGP' '反记帐'.
  mcr_fieldcat 'HKONT' '总账科目'.
  mcr_fieldcat 'XBILK' '是否资产负债表科目'.
  mcr_fieldcat 'PRCTR_TEXT' '利润中心描述'.
  mcr_fieldcat 'KOSTL' '成本中心'.
  mcr_fieldcat 'KOSTL_TEXT' '成本中心描述'.
  mcr_fieldcat 'KOART' '科目类型'.
  mcr_fieldcat 'UMSKZ' '特殊总帐标识'.
  mcr_fieldcat 'LIFNR' '供应商'.
  mcr_fieldcat 'LIFNR_TEXT' '供应商描述'.
  mcr_fieldcat 'KUNNR' '客户'.
  mcr_fieldcat 'KUNNR_TEXT' '客户描述'.
  mcr_fieldcat 'GSBER' '业务范围'.
  mcr_fieldcat 'FKBER' '功能范围'.
*  mcr_fieldcat 'ZZ_FEETY' '费用类型'.
  mcr_fieldcat 'AUFNR' '订单'.
  mcr_fieldcat 'WRBTR' '凭证货币金额'.
  mcr_fieldcat 'MATNR' '物料号'.
  mcr_fieldcat 'MENGE' '数量'.
  mcr_fieldcat 'MEINS' '基本计量单位'.
  mcr_fieldcat 'ZFBDT' '基准日期'.
  mcr_fieldcat 'ZUONR' '分配'.
  mcr_fieldcat 'PSWSL' '总帐货币金额'.
  mcr_fieldcat 'SGTXT' '行项目文本'.

  lv_callback_program = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
      i_bypassing_buffer       = 'X'
*     I_BUFFER_ACTIVE          =
      i_callback_program       = lv_callback_program
      i_callback_pf_status_set = 'FRM_ALV_PF_STATUS'
      i_callback_user_command  = 'FRM_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
      i_grid_title             = lv_title
*     I_GRID_SETTINGS          = LS_GRID_SETTING
      is_layout_lvc            = ls_layout
      it_fieldcat_lvc          = lt_fieldcat
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS_LVC    =
      it_sort_lvc              = lt_sort
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      i_save                   = 'A'
      is_variant               = ls_variant
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT_LVC             =
*     IS_REPREP_ID_LVC         =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        =
*     I_HTML_HEIGHT_END        =
*     IT_ALV_GRAPHICS          =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
*     t_outtab                 = it_bseg
      t_outtab                 = gt_bseg_detail
*   EXCEPTIONS
*     PROGRAM_ERROR            = 1
*     OTHERS                   = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
FORM frm_alv_pf_status USING rt_excluded_fcode  TYPE slis_t_extab.
  DATA ls_code TYPE sy-ucomm.

  ls_code = '&DOWNLOAD'.
  APPEND ls_code TO rt_excluded_fcode.

  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_excluded_fcode.

ENDFORM.
FORM frm_user_command USING r_ucomm TYPE sy-ucomm
            ps_selfield TYPE slis_selfield.
  DATA ls_line LIKE LINE OF gt_bseg_detail.

  IF r_ucomm EQ '&IC1'.
    READ TABLE gt_bseg_detail INTO ls_line INDEX ps_selfield-tabindex.
    CHECK sy-subrc = 0.
    SET PARAMETER ID 'BLN' FIELD ls_line-belnr.
    SET PARAMETER ID 'BUK' FIELD ls_line-bukrs.
    SET PARAMETER ID 'GJR' FIELD ls_line-gjahr.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDIF.

  ps_selfield-refresh = abap_true.

ENDFORM.


*Messages
*----------------------------------------------------------
*
* Message class: 00
*398   & & & &

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
