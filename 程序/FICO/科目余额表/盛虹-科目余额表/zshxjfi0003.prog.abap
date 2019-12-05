**************************************************
*程序名称:科目余额表
*创建日期: 2019-11-14
*创建者:XXX
*申请者:XXX
*功能描述:
*============================================
*变更记录
*修改请求号    修改日期    修改人   修改描述
*DEVK911995    2019-11-14   HANDYXH    创建程序
***************************************************
REPORT zshxjfi0003 MESSAGE-ID zfishxj01.

*--------------------------------------------------------------------*
*& Tables Declaration
*--------------------------------------------------------------------*
TABLES: sscrfields.

TABLES: bkpf,
        bseg.
TABLES: faglflext,
        faglflexa.

*--------------------------------------------------------------------*
*& Constants Declaration
*--------------------------------------------------------------------*
CONSTANTS: c_light_red    TYPE c VALUE '1', " For ALV
           c_light_yellow TYPE c VALUE '2',
           c_light_green  TYPE c VALUE '3',
           c_slash        TYPE c VALUE '/'.
*--------------------------------------------------------------------*
*& Type Declaration
*--------------------------------------------------------------------*
TYPES: BEGIN OF ty_output,
         dummy TYPE dummy,
       END OF ty_output.

*---> 科目余额

TYPES: BEGIN OF ty_period_balance. " 期间维度的余额
        INCLUDE TYPE fdbl_balance_line.
TYPES:
  debit_wrbtr           TYPE fdbl_balance_line-debit,         " 借方金额->凭证货币
  credit_wrbtr          TYPE fdbl_balance_line-credit,        " 贷方金额->凭证货币
  balance_wrbtr         TYPE fdbl_balance_line-balance,       " 余额    ->凭证货币
  debit_cum             TYPE fdbl_balance_line-balance,       " 累计借方
  debit_cum_year        TYPE fdbl_balance_line-balance,       " 累计借方->本年
  debit_cum_wrbtr       TYPE fdbl_balance_line-balance,       " 累计借方->凭证货币
  debit_cum_wrbtr_year  TYPE fdbl_balance_line-balance,       " 累计借方->凭证货币->本年
  credit_cum            TYPE fdbl_balance_line-balance,       " 累计贷方
  credit_cum_year       TYPE fdbl_balance_line-balance,       " 累计贷方->本年
  credit_cum_wrbtr      TYPE fdbl_balance_line-balance,       " 累计贷方->凭证货币

  credit_cum13          TYPE fdbl_balance_line-balance,       " 累计特殊季度贷方
  debit_cum13           TYPE fdbl_balance_line-balance,       " 累计特殊季度借方



  credit_cum_wrbtr_year TYPE fdbl_balance_line-balance,       " 累计贷方->凭证货币->本年
  balance_cum_wrbtr     TYPE fdbl_balance_line-balance_cum,   " 累计余额->凭证货币
*---数量
  debit_menge           TYPE faglflext-mslvt,                 " 借方数量
  credit_menge          TYPE faglflext-mslvt,                 " 贷方数量
  debit_menge_cum       TYPE faglflext-mslvt,                 " 累计借方数量
  debit_menge_cum_year  TYPE faglflext-mslvt,                 " 累计借方数量->本年
  credit_menge_cum      TYPE faglflext-mslvt,                 " 累计贷方数量
  credit_menge_cum_year TYPE faglflext-mslvt,                 " 累计贷方数量->本年
  menge_balance         TYPE faglflext-mslvt,
  menge_balance_cum     TYPE faglflext-mslvt,                 " 期末数量
  dummy                 TYPE dummy.
TYPES: END OF ty_period_balance.

TYPES: tt_period_balance TYPE STANDARD TABLE OF ty_period_balance WITH NON-UNIQUE DEFAULT KEY.

TYPES: BEGIN OF ty_balance_line.
TYPES: rbukrs TYPE faglflext-rbukrs, "公司代码
       racct  TYPE faglflext-racct,           " 科目
       rcntr  TYPE faglflext-rcntr,           " 成本中心
       prctr  TYPE faglflext-prctr,           " 利润中心
       rbusa  TYPE faglflext-rbusa,           " 业务范围
       rfarea TYPE faglflext-rfarea.          " 功能范围
*--->!!! Note:
*--->!!! Development Procedure
*" 1. 新增维度时，请把字段添加在 货币码-RTCUR之前
*" 2. 并且：字段名称要用FAGLFLEXT表的字段，不要用BSEG的字段
*" 3. 并同时添加到结构：ZSFIRPT_003_ALV
*" 4. 并同时修改预制凭证的维度：子程序-> line_item_predoc_get
*" 5. 并同时修改双击ALV时的where条件：子程序-> alv_handle_dclick_condition,
*"    主要是做一个字段的mapping, 比如科目字段， FAGLFLEXT表中是RACCT,BSEG表中是HKONT
*"    这里做一个字段的映射转换
*" e.g.
*" types: rbusa   type faglflext-rbusa.           " 业务范围
*" types: rfarea  type faglflext-rfarea.          " 功能范围
*<---!!!
TYPES: rtcur TYPE faglflext-rtcur. " 货币码
TYPES: period_balance TYPE tt_period_balance.
TYPES: END OF ty_balance_line.
*<---

TYPES: tt_bseg TYPE STANDARD TABLE OF ZSFIRPT_003_ALV_item.

TYPES: BEGIN OF ty_ska1,
         ktopl TYPE ska1-ktopl,
         saknr TYPE ska1-saknr,
         xbilk TYPE ska1-xbilk,
         gvtyp TYPE ska1-gvtyp,
       END OF ty_ska1.


*--------------------------------------------------------------------*
*& Global Variables Declaration
*--------------------------------------------------------------------*
DATA: gt_output   TYPE STANDARD TABLE OF ty_output,
      gs_layout   TYPE lvc_s_layo,
      gt_fieldcat TYPE lvc_t_fcat,
      gt_event    TYPE slis_t_event,
      go_alv_grid TYPE REF TO cl_gui_alv_grid.

DATA: gt_bseg         TYPE STANDARD TABLE OF ZSFIRPT_003_ALV_item, " 凭证行项目
      gt_bseg_pre     TYPE STANDARD TABLE OF ZSFIRPT_003_ALV_item,  " 预制凭证行项目
      gt_bseg_2nd_lvl TYPE STANDARD TABLE OF ZSFIRPT_003_ALV_item,  " 2级明细凭证的行项目
      gv_budat_from   TYPE bkpf-budat,  " 会计期间-起始日期
      gv_period_from  TYPE bkpf-monat,
      gv_budat_to     TYPE bkpf-budat,  " 会计期间-结束日期
      gv_period_to    TYPE bkpf-monat.

DATA: gv_budat_from_year  TYPE bkpf-budat, " 本年
      gv_budat_to_year    TYPE bkpf-budat,  " 本年
      gv_period_from_year TYPE bkpf-monat,
      gv_period_to_year   TYPE bkpf-monat.

DATA: gt_faglflext TYPE STANDARD TABLE OF faglflext,
      gt_balance   TYPE STANDARD TABLE OF ty_balance_line.
DATA: gt_fagl_011qt TYPE STANDARD TABLE OF fagl_011qt. " 一级科目文本表

*" 上线日期配置表
DATA: gt_t093c TYPE STANDARD TABLE OF t093c.

*" 科目主数据表
DATA: gt_ska1 TYPE STANDARD TABLE OF ty_ska1.

*--- Dynamic Table
DATA: gt_fieldcat_dynamic_cond TYPE lvc_t_fcat.
DATA: gr_dynamic_table TYPE REF TO data,
      gr_dynamic_line  TYPE REF TO data.

DATA: gr_dynamic_period_block_table TYPE REF TO data,
      gr_dynamic_period_block_line  TYPE REF TO data.

FIELD-SYMBOLS: <fs_output_table> TYPE STANDARD TABLE,
               <fs_output_line>  TYPE any.

FIELD-SYMBOLS: <fs_period_block_table> TYPE STANDARD TABLE,
               <fs_period_block_line>  TYPE any.
RANGES g_bukrs FOR bkpf-bukrs.
RANGES g_hkont FOR bseg-hkont.
RANGES g_kostl FOR bseg-kostl.
RANGES g_prctr FOR bseg-prctr.
*--------------------------------------------------------------------*
*& Selection Screen
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS s_bukrs FOR bkpf-bukrs OBLIGATORY MEMORY ID buk.
PARAMETERS:  p_gjahr TYPE gjahr OBLIGATORY.
SELECT-OPTIONS: s_monat FOR bkpf-monat NO-EXTENSION.
PARAMETERS: p_tsqj TYPE c AS CHECKBOX. " 是否特殊区间
PARAMETERS: p_save TYPE c AS CHECKBOX. " 是否保存
SELECT-OPTIONS: s_hkont FOR bseg-hkont.

*" KOSTL
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_kostl AS CHECKBOX.
SELECTION-SCREEN COMMENT 3(26) text-c01 FOR FIELD s_kostl.
SELECT-OPTIONS: s_kostl FOR bseg-kostl MATCHCODE OBJECT kost MEMORY ID kos.
SELECTION-SCREEN END OF LINE.

*" PRCTR
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_prctr AS CHECKBOX.
SELECTION-SCREEN COMMENT 3(26) text-c02 FOR FIELD s_prctr .
SELECT-OPTIONS: s_prctr FOR bseg-prctr MEMORY ID prc.
SELECTION-SCREEN END OF LINE.

*" GSBER 业务范围
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_gsber AS CHECKBOX.
SELECTION-SCREEN COMMENT 3(26) text-c03 FOR FIELD s_gsber .
SELECT-OPTIONS: s_gsber FOR bseg-gsber MEMORY ID gsb.
SELECTION-SCREEN END OF LINE.

*" 功能范围
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_rfarea AS CHECKBOX.
SELECTION-SCREEN COMMENT 3(26) text-c04 FOR FIELD s_rfarea .
SELECT-OPTIONS: s_rfarea FOR bseg-fkber_long MEMORY ID fbe.
SELECTION-SCREEN END OF LINE.

*--->NOTE: 新增其他查询维度的字段时, 请同时在以下几个地方也添加相应的字段|||
*--->!!! Development Procedure
*" 1. 类型  : ty_balance_line 中也添加相应的字段|||
*" 2. 子程序：build_fieldcat_dynamically
*" 3. 子程序：build_fieldcat_period_dynamic
*"
*" 新增字段示例
*" e.g.
*" rfarea   " 功能范围
*" rbusa    " 业务范围
*<---NOTE: 新增其他查询维度的字段时, 请同时在以下几个地方也添加相应的字段|||

SELECTION-SCREEN END OF BLOCK b1.

*" 选项
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002. " 选项
PARAMETERS: p_hwaer TYPE c RADIOBUTTON GROUP rg1 DEFAULT 'X', " 本位币
            p_waers TYPE c RADIOBUTTON GROUP rg1 .            " 原币

PARAMETERS: p_lvl1 TYPE c AS CHECKBOX, " 是否显示一级科目
            p_sum1 TYPE c AS CHECKBOX. " 是否汇总一级科目
PARAMETERS: p_pre TYPE c AS CHECKBOX. " 是否包含预制凭证

**" 资产类科目时，累计借方/贷方，是否包含期初
*parameters: p_xbilk type c as checkbox default 'X'.

SELECTION-SCREEN END OF BLOCK b2.

*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initialize.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM sscr_pbo.
*--------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM sscr_pai.

*--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM main_program.

*--------------------------------------------------------------------*
END-OF-SELECTION.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON EXIT-COMMAND.


*&---------------------------------------------------------------------*
*&      Form  main_program
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM main_program.
  PERFORM authority_check.
  PERFORM init_data.
  PERFORM create_dynamic_table.
  PERFORM build_posting_data_range.
  PERFORM data_get.
  PERFORM data_process.
  PERFORM data_process_pre_doc.       " 预制凭证
  PERFORM data_process_filter.        " 一级科目及等于0的行项目过滤处理
  PERFORM data_process_text.          " 文本描述的处理
  PERFORM data_process_debit_credit.  " 借贷方向的处理
  PERFORM data_display.

ENDFORM. "main_program

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM authority_check .
  TYPES BEGIN OF ty_bukrs.
  TYPES bukrs TYPE bkpf-bukrs.
  TYPES END OF ty_bukrs.
  DATA lt_bukrs TYPE TABLE OF ty_bukrs.
  DATA ls_bukrs TYPE ty_bukrs.

  REFRESH g_bukrs.
  DATA: lv_message TYPE char50.
*选取所有公司代码
  SELECT t001~bukrs
    INTO TABLE lt_bukrs
    FROM t001
   WHERE bukrs IN s_bukrs.
  LOOP AT lt_bukrs INTO ls_bukrs.
    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
             ID 'BUKRS' FIELD ls_bukrs-bukrs
             ID 'ACTVT' FIELD '03'.
    IF sy-subrc <> 0.
      CLEAR lv_message.
      lv_message = '没有公司代码' && ls_bukrs-bukrs && '的权限'.
      MESSAGE  lv_message TYPE 'S'  DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ELSE.
      g_bukrs-sign   = 'I'.
      g_bukrs-option = 'EQ'.
      g_bukrs-low    = ls_bukrs-bukrs.
      APPEND g_bukrs.
    ENDIF.
  ENDLOOP.
  IF g_bukrs[] IS INITIAL.

  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM init_data .
*" 期初上线配置数据
  SELECT *
    INTO TABLE gt_t093c
    FROM t093c
    WHERE bukrs IN g_bukrs
    .

*" 科目主数据
  SELECT
    ktopl
    saknr
    xbilk
    gvtyp
    INTO TABLE gt_ska1
    FROM ska1
    WHERE ktopl = 'CXZX'.

  SORT gt_ska1 ASCENDING BY ktopl saknr.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  initialize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM initialize.
  IF p_gjahr IS INITIAL.
    p_gjahr = sy-datum+0(4).
  ENDIF.

  IF s_monat IS INITIAL.
    s_monat-sign    = 'I'.
    s_monat-option  = 'EQ'.
    s_monat-low     = sy-datum+4(2).
    APPEND s_monat.
  ENDIF.

ENDFORM. "initialize

*&---------------------------------------------------------------------*
*&      Form  selscr_pbo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sscr_pbo.
  .
ENDFORM. "selscr_pbo

*&---------------------------------------------------------------------*
*&      Form  selscr_pai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sscr_pai.



ENDFORM. "selscr_pai

*&---------------------------------------------------------------------*
*&      Form  data_get
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_get.

  SELECT *
    INTO TABLE gt_faglflext
    FROM faglflext
    WHERE rbukrs IN g_bukrs
      AND ryear  = p_gjahr
      AND racct  IN s_hkont   " 科目
      AND rcntr  IN s_kostl   " 成本中心
      AND prctr  IN s_prctr   " 利润中心
      AND rbusa  IN s_gsber   " 业务范围
      AND rfarea IN s_rfarea. " 功能范围
  IF sy-subrc <> 0.
    MESSAGE s000 WITH '未查询到任何数据' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.





*  loop at gt_faglflext into data(ls_faglflext).
*    if ls_faglflext-drcrk = 'H'.
*      if ls_faglflext-activ <> 'RFBU'.
*        delete gt_faglflext.
*      endif.
*    endif.
*  endloop.

*" 一级科目或者一级科目汇总
  IF p_lvl1 EQ abap_true
      OR p_sum1 EQ abap_true.
    SELECT *
      INTO TABLE gt_fagl_011qt
      FROM fagl_011qt
      WHERE versn = 'CXZX'
        AND spras = sy-langu.
  ENDIF.

ENDFORM. "data_get

*&---------------------------------------------------------------------*
*&      Form  data_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_process.

**--------------------------------------------------------------------*
*  types: begin of ty_period_block_nav.
*  types:
*    rbukrs type faglflext-rbukrs,
*    racct  type faglflext-racct,
*    rcntr  type faglflext-rcntr,
*    prctr  type faglflext-prctr,
*    rtcur  type faglflext-rtcur.
*          include type fagl_s_period_block. " fagl_s_period_block_rpmax
*  types: end of ty_period_block_nav.
**--------------------------------------------------------------------*

  DATA: ls_faglflext  TYPE faglflext.

*  data: ls_period_block type ty_period_block_nav,
*        lt_period_block type standard table of ty_period_block_nav.

  DATA: ls_period_block TYPE fagl_s_period_block.

  DATA: ls_balance       TYPE ty_balance_line,
        ls_balance_temp  TYPE ty_balance_line,
        ls_balance_final TYPE ty_balance_line,
        lt_balance       TYPE STANDARD TABLE OF ty_balance_line,
        lt_balance_final TYPE STANDARD TABLE OF ty_balance_line,
        lv_period        TYPE i,
        lv_tabix         TYPE sytabix.

  DATA: ls_balance_period      TYPE ty_period_balance,
        ls_balance_period_temp TYPE ty_period_balance,
        lt_balance_period      TYPE STANDARD TABLE OF ty_period_balance.


  DATA: lv_debit_sum         TYPE fdbl_de_s,    "
        lv_credit_sum        TYPE fdbl_de_h,    "
        lv_balance_sum       TYPE fdbl_de_bal,  "
        lv_balance_cum       TYPE fdbl_de_bal_cum,
        lv_carryfoward       TYPE fdbl_de_bal_cum,
        lv_carryfoward_wrbtr TYPE fdbl_de_bal_cum.

  DATA: field_name_hsl    TYPE char73,
        field_name_tsl    TYPE char73,
        field_name_msl    TYPE char73,
        field_name_amount TYPE char73,
        lv_period_char2   TYPE numc2,
        lv_period_char3   TYPE numc3.

  DATA: ls_ska1 LIKE LINE OF gt_ska1.


  FIELD-SYMBOLS: <hsl_val>   TYPE any,
                 <tsl_val>   TYPE any,
                 <msl_val>   TYPE any,
                 <fv_amount> TYPE any.


*" FAGLFLEXT
  LOOP AT gt_faglflext INTO ls_faglflext.
    CLEAR <fs_period_block_line>.

    MOVE-CORRESPONDING ls_faglflext TO <fs_period_block_line>.
    COLLECT <fs_period_block_line> INTO <fs_period_block_table>.
  ENDLOOP.

  LOOP AT <fs_period_block_table> ASSIGNING <fs_period_block_line>.
    CLEAR: ls_balance.
    MOVE-CORRESPONDING <fs_period_block_line> TO ls_period_block.
    MOVE-CORRESPONDING <fs_period_block_line> TO ls_balance.

    CLEAR: lt_balance_period.

    DO 17 TIMES.
      CLEAR: ls_balance_period,
             lv_period.

      lv_period = sy-index.
      IF sy-index = 1.
*" 余额结转行
        ls_balance_period-period = 0.
        ls_balance_period-month  = '余额结转'.
        CLEAR: ls_balance_period-debit,
               ls_balance_period-credit,
               ls_balance_period-balance.
        ls_balance_period-balance_cum       = ls_period_block-hslvt.          " 本行累计余额 = 本行结转金额
        ls_balance_period-balance_cum_wrbtr = ls_period_block-tslvt.          " 本行累计余额 = 本行结转金额
        ls_balance_period-menge_balance_cum = ls_period_block-mslvt.          "
        lv_carryfoward  = lv_carryfoward + ls_period_block-hslvt.     " 结转金额->本位币
        lv_carryfoward_wrbtr  = lv_carryfoward_wrbtr +
                                ls_period_block-tslvt.     " 结转金额->凭证货币
        COLLECT ls_balance_period INTO lt_balance_period.

      ELSE.
        lv_period       = lv_period - 1.
        lv_period_char2 = lv_period.
        lv_period_char3 = lv_period_char2.
        CONCATENATE 'LS_PERIOD_BLOCK-' 'HSL' lv_period_char2 INTO field_name_hsl.
        CONCATENATE 'LS_PERIOD_BLOCK-' 'TSL' lv_period_char2 INTO field_name_tsl.
        CONCATENATE 'LS_PERIOD_BLOCK-' 'MSL' lv_period_char2 INTO field_name_msl.
        ASSIGN (field_name_hsl) TO <hsl_val>.
        ASSIGN (field_name_tsl) TO <tsl_val>.
        ASSIGN (field_name_msl) TO <msl_val>.

        ls_balance_period-period = lv_period.
        CALL FUNCTION 'GET_PERIOD_TEXTS_BASIS'
          EXPORTING
*           I_SPRAS                       = SY-LANGU
            i_periv                       = 'K4'
            i_poper                       = lv_period_char3
*           I_BDATJ                       = '0000'
          IMPORTING
*           E_KTEXT                       =
            e_ltext                       = ls_balance_period-month
*           E_T009C                       =
          EXCEPTIONS
            period_version_not_found      = 1
            period_texts_not_found        = 2
            period_version_year_dependent = 3
            version_not_year_dependent    = 4
            OTHERS                        = 5.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

        IF ls_period_block-drcrk EQ 'S'.
          ls_balance_period-debit        = <hsl_val>.
          ls_balance_period-debit_wrbtr  = <tsl_val>.
          ls_balance_period-debit_menge  = <msl_val>.
          lv_debit_sum                   = lv_debit_sum + ls_balance_period-debit.
        ELSE.
          ls_balance_period-credit       = 0 - <hsl_val>.
          ls_balance_period-credit_wrbtr = 0 - <tsl_val>.
          ls_balance_period-credit_menge = 0 - <msl_val>.
          lv_credit_sum                  = lv_credit_sum + ls_balance_period-credit.
        ENDIF.
        ls_balance_period-balance      = ls_balance_period-balance +
                                         ls_balance_period-debit   -
                                         ls_balance_period-credit.

        ls_balance_period-balance_wrbtr = ls_balance_period-balance_wrbtr +
                                          ls_balance_period-debit_wrbtr   -
                                          ls_balance_period-credit_wrbtr.

        ls_balance_period-menge_balance = ls_balance_period-menge_balance +
                                          ls_balance_period-debit_menge   -
                                          ls_balance_period-credit_menge.

*        lv_balance_sum  = lv_balance_sum + ls_balance-balance.
        COLLECT ls_balance_period INTO lt_balance_period.
      ENDIF.
    ENDDO.

    ls_balance-period_balance = lt_balance_period.
    APPEND ls_balance TO lt_balance.
  ENDLOOP.

  CLEAR: lt_balance_final,
         ls_balance_final.
  CLEAR lt_balance_period.


  LOOP AT lt_balance INTO ls_balance.
    ls_balance_temp = ls_balance.
    LOOP AT ls_balance_temp-period_balance INTO ls_balance_period.
      COLLECT ls_balance_period INTO lt_balance_period.
    ENDLOOP.

    AT END OF rtcur.
      CLEAR ls_balance_final.
      MOVE-CORRESPONDING ls_balance_temp TO ls_balance_final.
*" 除了 period_balance，其他字段均赋值，clear  period_balance.
      CLEAR ls_balance_final-period_balance.
      ls_balance_final-period_balance = lt_balance_period.
      APPEND ls_balance_final TO lt_balance_final.

      CLEAR: lt_balance_period.
    ENDAT.
  ENDLOOP.

*--------------------------------------------------------------------*
*" !!! 最终数据输出, 动态字段请参照结构：SE11->ZSFIRPT_003_ALV_COMMON
*--------------------------------------------------------------------*
*  lv_period = p_monat.
  lv_period = s_monat-low.
  LOOP AT lt_balance_final INTO ls_balance_final.

*" 计算余额
    CLEAR ls_ska1.
    READ TABLE gt_ska1 INTO ls_ska1 WITH KEY ktopl = 'CXZX'
                                             saknr = ls_balance_final-racct
                                             BINARY SEARCH.
    IF sy-subrc = 0.

    ENDIF.

    PERFORM data_process_calc_balance USING    ls_ska1-xbilk " p_xbilk
                                               ls_balance_final-rbukrs
                                      CHANGING ls_balance_final-period_balance.

    MOVE-CORRESPONDING ls_balance_final TO <fs_output_line>.
*" 货币码, 很重要, 用于确定显示本位币还是凭证货币
    CONCATENATE '<fs_output_line>-' 'waers' INTO field_name_amount.
    ASSIGN (field_name_amount) TO <fv_amount>.
    IF sy-subrc = 0.
      <fv_amount> = ls_balance_final-rtcur.
    ENDIF.

*" 期初余额&期初数量,读取上一期间的期末余额
    READ TABLE ls_balance_final-period_balance INTO ls_balance_period WITH KEY period = lv_period - 1.
    IF sy-subrc = 0.
      CONCATENATE '<fs_output_line>-' 'opening_balance' INTO field_name_amount.
      ASSIGN (field_name_amount) TO <fv_amount>.
      IF sy-subrc = 0.
        <fv_amount> = ls_balance_period-balance_cum.
      ENDIF.

      CONCATENATE '<fs_output_line>-' 'opening_balance_wrbtr' INTO field_name_amount.
      ASSIGN (field_name_amount) TO <fv_amount>.
      IF sy-subrc = 0.
        <fv_amount> = ls_balance_period-balance_cum_wrbtr.
      ENDIF.

*" 期初数量
      CONCATENATE '<fs_output_line>-' 'opening_menge' INTO field_name_amount.
      ASSIGN (field_name_amount) TO <fv_amount>.
      IF sy-subrc = 0.
        <fv_amount> = ls_balance_period-menge_balance_cum.
      ENDIF.

    ENDIF.




    IF  p_tsqj = 'X'."为特殊区间新增判断by sw

      LOOP AT ls_balance_final-period_balance INTO ls_balance_period WHERE period IN s_monat
                                                                         OR month =  '特殊季度1' OR month =  '特殊季度2'
                                                                         OR month =  '特殊季度3' OR month =  '特殊季度4'.
        CLEAR: field_name_amount.
*" 借方金额
        CONCATENATE '<fs_output_line>-' 'dmbtr_debit' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          IF ls_balance_period-month = '特殊季度1' OR ls_balance_period-month = '特殊季度2' OR ls_balance_period-month = '特殊季度3' OR ls_balance_period-month = '特殊季度4' .
            <fv_amount> = <fv_amount> + ls_balance_period-debit.
          ENDIF.
        ENDIF.

        CONCATENATE '<fs_output_line>-' 'wrbtr_debit' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          IF ls_balance_period-month = '特殊季度1' OR ls_balance_period-month = '特殊季度2' OR ls_balance_period-month = '特殊季度3' OR ls_balance_period-month = '特殊季度4' .
            <fv_amount> = <fv_amount> + ls_balance_period-debit_wrbtr.
          ENDIF.
        ENDIF.

*" 借方数量
        CONCATENATE '<fs_output_line>-' 'menge_debit' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          IF ls_balance_period-month = '特殊季度1' OR ls_balance_period-month = '特殊季度2' OR ls_balance_period-month = '特殊季度3' OR ls_balance_period-month = '特殊季度4' .
            <fv_amount> = <fv_amount> + ls_balance_period-debit_menge.
          ENDIF.
        ENDIF.

*" 贷方金额
        CONCATENATE '<fs_output_line>-' 'dmbtr_credit' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          IF ls_balance_period-month = '特殊季度1' OR ls_balance_period-month = '特殊季度2' OR ls_balance_period-month = '特殊季度3' OR ls_balance_period-month = '特殊季度4' .
            <fv_amount> = <fv_amount> + ls_balance_period-credit.
          ENDIF.
        ENDIF.

        CONCATENATE '<fs_output_line>-' 'wrbtr_credit' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          IF ls_balance_period-month = '特殊季度1' OR ls_balance_period-month = '特殊季度2' OR ls_balance_period-month = '特殊季度3' OR ls_balance_period-month = '特殊季度4' .
            <fv_amount> = <fv_amount> + ls_balance_period-credit_wrbtr.
          ENDIF.
        ENDIF.

*" 贷方数量
        CONCATENATE '<fs_output_line>-' 'menge_credit' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          IF ls_balance_period-month = '特殊季度1' OR ls_balance_period-month = '特殊季度2' OR ls_balance_period-month = '特殊季度3' OR ls_balance_period-month = '特殊季度4' .
            <fv_amount> = <fv_amount> + ls_balance_period-credit_menge.
          ENDIF.
        ENDIF.

*" 累计借方
        IF ls_balance_period-period = gv_period_to.
          CONCATENATE '<fs_output_line>-' 'dmbtr_debit_cum' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-debit_cum.
          ENDIF.

          CONCATENATE '<fs_output_line>-' 'wrbtr_debit_cum' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-debit_wrbtr.
          ENDIF.

*" 累计借方数量
          CONCATENATE '<fs_output_line>-' 'menge_debit_cum' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-debit_menge_cum.
          ENDIF.

        ENDIF.

*" 本年累计借方金额 & 数量
        IF ls_balance_period-period >= gv_period_to_year.
          CONCATENATE '<fs_output_line>-' 'dmbtr_debit_cum_year' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            IF ls_balance_period-month = '特殊季度1' OR ls_balance_period-month = '特殊季度2' OR ls_balance_period-month = '特殊季度3' OR ls_balance_period-month = '特殊季度4' .
              <fv_amount> =   <fv_amount> + ls_balance_period-debit.
            ELSE.
              <fv_amount> = ls_balance_period-debit_cum_year.
            ENDIF.
          ENDIF.

          CONCATENATE '<fs_output_line>-' 'wrbtr_debit_cum_year' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            IF ls_balance_period-month = '特殊季度1' OR ls_balance_period-month = '特殊季度2' OR ls_balance_period-month = '特殊季度3' OR ls_balance_period-month = '特殊季度4' .
              <fv_amount> = <fv_amount> + ls_balance_period-debit_cum_wrbtr.
            ELSE.
              <fv_amount> = ls_balance_period-debit_cum_wrbtr_year.
            ENDIF.
          ENDIF.

          CONCATENATE '<fs_output_line>-' 'menge_debit_cum_year' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            IF ls_balance_period-month = '特殊季度1' OR ls_balance_period-month = '特殊季度2' OR ls_balance_period-month = '特殊季度3' OR ls_balance_period-month = '特殊季度4' .
              <fv_amount> = <fv_amount> + ls_balance_period-debit_menge.
            ELSE.
              <fv_amount> = ls_balance_period-debit_menge_cum_year.
            ENDIF.
          ENDIF.
        ENDIF.

*" 累计贷方
        IF ls_balance_period-period >= gv_period_to.
          CONCATENATE '<fs_output_line>-' 'dmbtr_credit_cum' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-credit_cum.
          ENDIF.

          CONCATENATE '<fs_output_line>-' 'wrbtr_credit_cum' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-credit_cum_wrbtr.
          ENDIF.

*" 累计贷方数量
          CONCATENATE '<fs_output_line>-' 'menge_credit_cum' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-credit_menge_cum.
          ENDIF.

        ENDIF.

*" 本年累计贷方金额 & 数量
        IF ls_balance_period-period >= gv_period_to.
          CONCATENATE '<fs_output_line>-' 'dmbtr_credit_cum_year' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            IF ls_balance_period-month = '特殊季度1' OR ls_balance_period-month = '特殊季度2' OR ls_balance_period-month = '特殊季度3' OR ls_balance_period-month = '特殊季度4' .
              <fv_amount> = <fv_amount>  +  ls_balance_period-credit.
            ELSE.
              <fv_amount> = ls_balance_period-credit_cum_year.
            ENDIF.
          ENDIF.


          CONCATENATE '<fs_output_line>-' 'wrbtr_credit_cum_year' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            IF ls_balance_period-month = '特殊季度1' OR ls_balance_period-month = '特殊季度2' OR ls_balance_period-month = '特殊季度3' OR ls_balance_period-month = '特殊季度4' .
              <fv_amount> = <fv_amount> + ls_balance_period-credit_wrbtr..
            ELSE.
              <fv_amount> = ls_balance_period-credit_cum_wrbtr_year.
            ENDIF.
          ENDIF.

*" 本年累计贷方数量
          CONCATENATE '<fs_output_line>-' 'menge_credit_cum_year' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            IF ls_balance_period-month = '特殊季度1' OR ls_balance_period-month = '特殊季度2' OR ls_balance_period-month = '特殊季度3' OR ls_balance_period-month = '特殊季度4' .

              <fv_amount> = <fv_amount> +   ls_balance_period-credit_menge.
            ELSE.
              <fv_amount> = ls_balance_period-credit_menge_cum_year.
            ENDIF.
          ENDIF.

        ENDIF.

*" 期末余额
        CONCATENATE '<fs_output_line>-' 'closing_balance' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          <fv_amount> = ls_balance_period-balance_cum.
        ENDIF.

        CONCATENATE '<fs_output_line>-' 'closing_balance_wrbtr' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          <fv_amount> = ls_balance_period-balance_cum_wrbtr.
        ENDIF.
*" 期末数量
        CONCATENATE '<fs_output_line>-' 'closing_menge' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          <fv_amount> = ls_balance_period-menge_balance_cum.
        ENDIF.
      ENDLOOP.









    ELSE.
      LOOP AT ls_balance_final-period_balance INTO ls_balance_period WHERE period IN s_monat.
        CLEAR: field_name_amount.
*" 借方金额
        CONCATENATE '<fs_output_line>-' 'dmbtr_debit' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          <fv_amount> = <fv_amount> + ls_balance_period-debit.
        ENDIF.

        CONCATENATE '<fs_output_line>-' 'wrbtr_debit' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          <fv_amount> = <fv_amount> + ls_balance_period-debit_wrbtr.
        ENDIF.

*" 借方数量
        CONCATENATE '<fs_output_line>-' 'menge_debit' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          <fv_amount> = <fv_amount> + ls_balance_period-debit_menge.
        ENDIF.

*" 贷方金额
        CONCATENATE '<fs_output_line>-' 'dmbtr_credit' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          <fv_amount> = <fv_amount> + ls_balance_period-credit.
        ENDIF.

        CONCATENATE '<fs_output_line>-' 'wrbtr_credit' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          <fv_amount> = <fv_amount> + ls_balance_period-credit_wrbtr.
        ENDIF.

*" 贷方数量
        CONCATENATE '<fs_output_line>-' 'menge_credit' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          <fv_amount> = <fv_amount> + ls_balance_period-credit_menge.
        ENDIF.

*" 累计借方
        IF ls_balance_period-period = gv_period_to.
          CONCATENATE '<fs_output_line>-' 'dmbtr_debit_cum' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-debit_cum.
          ENDIF.

          CONCATENATE '<fs_output_line>-' 'wrbtr_debit_cum' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-debit_cum_wrbtr.
          ENDIF.

*" 累计借方数量
          CONCATENATE '<fs_output_line>-' 'menge_debit_cum' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-debit_menge_cum.
          ENDIF.

        ENDIF.

*" 本年累计借方金额 & 数量
        IF ls_balance_period-period = gv_period_to_year.
          CONCATENATE '<fs_output_line>-' 'dmbtr_debit_cum_year' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-debit_cum_year.
          ENDIF.

          CONCATENATE '<fs_output_line>-' 'wrbtr_debit_cum_year' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-debit_cum_wrbtr_year.
          ENDIF.

          CONCATENATE '<fs_output_line>-' 'menge_debit_cum_year' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-debit_menge_cum_year.
          ENDIF.
        ENDIF.

*" 累计贷方
        IF ls_balance_period-period = gv_period_to.
          CONCATENATE '<fs_output_line>-' 'dmbtr_credit_cum' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-credit_cum.
          ENDIF.

          CONCATENATE '<fs_output_line>-' 'wrbtr_credit_cum' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-credit_cum_wrbtr.
          ENDIF.

*" 累计贷方数量
          CONCATENATE '<fs_output_line>-' 'menge_credit_cum' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-credit_menge_cum.
          ENDIF.

        ENDIF.

*" 本年累计贷方金额 & 数量
        IF ls_balance_period-period = gv_period_to.
          CONCATENATE '<fs_output_line>-' 'dmbtr_credit_cum_year' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-credit_cum_year.
          ENDIF.

          CONCATENATE '<fs_output_line>-' 'wrbtr_credit_cum_year' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-credit_cum_wrbtr_year.
          ENDIF.

*" 本年累计贷方数量
          CONCATENATE '<fs_output_line>-' 'menge_credit_cum_year' INTO field_name_amount.
          ASSIGN (field_name_amount) TO <fv_amount>.
          IF sy-subrc = 0.
            <fv_amount> = ls_balance_period-credit_menge_cum_year.
          ENDIF.

        ENDIF.

*" 期末余额
        CONCATENATE '<fs_output_line>-' 'closing_balance' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          <fv_amount> = ls_balance_period-balance_cum.
        ENDIF.

        CONCATENATE '<fs_output_line>-' 'closing_balance_wrbtr' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          <fv_amount> = ls_balance_period-balance_cum_wrbtr.
        ENDIF.
*" 期末数量
        CONCATENATE '<fs_output_line>-' 'closing_menge' INTO field_name_amount.
        ASSIGN (field_name_amount) TO <fv_amount>.
        IF sy-subrc = 0.
          <fv_amount> = ls_balance_period-menge_balance_cum.
        ENDIF.
      ENDLOOP.
    ENDIF.


    COLLECT <fs_output_line> INTO <fs_output_table>.
    CLEAR: <fs_output_line>.
  ENDLOOP.

  CLEAR: lt_balance.

  CLEAR gt_balance.
  gt_balance  = lt_balance_final.

ENDFORM. "data_process

*---------------------------------------------------------------------*
* Description: 计算期末余额&累计的期末余额&累计借方&累计贷方
* iv_xbilk: 标识符：是否为资产负债类科目
*
*---------------------------------------------------------------------*
FORM data_process_calc_balance USING iv_xbilk TYPE ska1-xbilk
                                         pv_bukrs TYPE bkpf-bukrs
                               CHANGING ct_balance_period  TYPE tt_period_balance.

  DATA: lv_period TYPE i,
        lv_tabix  TYPE sytabix.

  DATA: ls_balance_period      TYPE ty_period_balance,
        ls_balance_period_temp TYPE ty_period_balance.

  DATA: ls_t093c  TYPE t093c.

  READ TABLE gt_t093c INTO ls_t093c
    WITH KEY bukrs = pv_bukrs.
  CHECK sy-subrc = 0.

*" 累计余额
  READ TABLE ct_balance_period INTO ls_balance_period WITH KEY period = 0.  " 年度的期初金额
  IF sy-subrc = 0.
    ls_balance_period_temp-balance_cum       = ls_balance_period-balance_cum.
    ls_balance_period_temp-balance_cum_wrbtr = ls_balance_period-balance_cum_wrbtr.
    ls_balance_period_temp-menge_balance_cum = ls_balance_period-menge_balance_cum.
*" 本年借贷方累计不包含年初数据, 这里不赋值
  ENDIF.

  CLEAR lv_period.
  DO 16 TIMES.
    lv_period = lv_period + 1.
    READ TABLE ct_balance_period INTO ls_balance_period WITH KEY period = lv_period.
    IF sy-subrc = 0.
      lv_tabix  = sy-tabix.
*" 累计余额
      ls_balance_period-balance_cum   = ls_balance_period_temp-balance_cum +
                                        ls_balance_period-balance.
*" 累计凭证货币余额
      ls_balance_period-balance_cum_wrbtr  = ls_balance_period_temp-balance_cum_wrbtr +
                                             ls_balance_period-balance_wrbtr.
*" 累计借方
      ls_balance_period-debit_cum     = ls_balance_period_temp-debit_cum +
                                        ls_balance_period-debit.

*" 累计借方凭证货币
      ls_balance_period-debit_cum_wrbtr   = ls_balance_period_temp-debit_cum_wrbtr +
                                            ls_balance_period-debit_wrbtr.

*" 累计贷方
      ls_balance_period-credit_cum        = ls_balance_period_temp-credit_cum +
                                            ls_balance_period-credit.

*" 累计贷方凭证货币
      ls_balance_period-credit_cum_wrbtr  = ls_balance_period_temp-credit_cum_wrbtr +
                                            ls_balance_period-credit_wrbtr.







*" 累计数量
      ls_balance_period-menge_balance_cum = ls_balance_period_temp-menge_balance_cum +
                                            ls_balance_period-menge_balance.

*" 累计借方数量
      ls_balance_period-debit_menge_cum   = ls_balance_period_temp-debit_menge_cum +
                                            ls_balance_period-debit_menge.

*" 累计贷方数量
      ls_balance_period-credit_menge_cum  = ls_balance_period_temp-credit_menge_cum +
                                            ls_balance_period-credit_menge.

*" 本年累计数据(不包含本年的年初或者上线的期初数据)
      IF p_gjahr = ls_t093c-datum+0(4)
          AND lv_period <= ls_t093c-datum+4(2)
          AND iv_xbilk  EQ abap_true.
        " do nothing... 过滤掉
      ELSE.
*" 本年累计借方
        ls_balance_period-debit_cum_year  = ls_balance_period_temp-debit_cum_year +
                                            ls_balance_period-debit.
*" 本年累计贷方
        ls_balance_period-credit_cum_year   = ls_balance_period_temp-credit_cum_year +
                                              ls_balance_period-credit.

*" 本年累计借方数量
        ls_balance_period-debit_menge_cum_year   = ls_balance_period_temp-debit_menge_cum_year +
                                              ls_balance_period-debit_menge.
*" 本年累计贷方数量
        ls_balance_period-credit_menge_cum_year  = ls_balance_period_temp-credit_menge_cum_year +
                                              ls_balance_period-credit_menge.

*" 本年累计借方凭证货币
        ls_balance_period-debit_cum_wrbtr_year   = ls_balance_period_temp-debit_cum_wrbtr_year +
                                              ls_balance_period-debit_wrbtr.
*" 本年累计贷方凭证货币
        ls_balance_period-credit_cum_wrbtr_year  = ls_balance_period_temp-credit_cum_wrbtr_year +
                                              ls_balance_period-credit_wrbtr.
      ENDIF.

      ls_balance_period_temp = ls_balance_period. " Important
      MODIFY ct_balance_period INDEX lv_tabix FROM ls_balance_period
                                                           .
    ENDIF.
  ENDDO.

ENDFORM.

*---------------------------------------------------------------------*
* Description: 预制凭证处理
*---------------------------------------------------------------------*
FORM data_process_pre_doc .

  IF p_pre EQ 'X'.
    PERFORM line_item_predoc_get.
  ENDIF.

ENDFORM.

FORM data_process_filter.

  DATA: lt_field           TYPE STANDARD TABLE OF dfies,
        ls_field           TYPE dfies,
        lv_field_name      TYPE char73,
        lv_field_name_text TYPE char73,
        lv_delete_flag     TYPE flag.

  DATA: lt_sort TYPE STANDARD TABLE OF abap_sortorder,
        ls_sort TYPE abap_sortorder.

  DATA: ls_fagl_011qt     TYPE fagl_011qt,
        ls_ZSFIRPT_003_ALV TYPE ZSFIRPT_003_ALV.

  DATA: ls_lvl1_hkont TYPE ZSFIRPT_003_ALV_hkont.

  FIELD-SYMBOLS: <fs_field_value>      TYPE any,
                 <fs_field_value_text> TYPE any.

*--- Dynamic Table
  DATA: lr_dynamic_table TYPE REF TO data,
        lr_dynamic_line  TYPE REF TO data.
  FIELD-SYMBOLS: <fs_output_tablle_collect> TYPE STANDARD TABLE.

*" 过滤掉所有金额字段和数量字段等于0的记录
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = 'ZSFIRPT_003_ALV_COMMON'
*     FIELDNAME      = ' '
      langu          = sy-langu
*     LFIELDNAME     = ' '
*     ALL_TYPES      = ' '
*     GROUP_NAMES    = ' '
*     UCLEN          =
*     DO_NOT_WRITE   = ' '
*   IMPORTING
*     X030L_WA       =
*     DDOBJTYPE      =
*     DFIES_WA       =
*     LINES_DESCR    =
    TABLES
      dfies_tab      = lt_field
*     FIXED_VALUES   =
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*" 一级科目
  SORT gt_fagl_011qt ASCENDING BY versn spras ergsl.  " txtyp.

  IF <fs_output_table> IS NOT INITIAL.
    LOOP AT <fs_output_table> ASSIGNING <fs_output_line>.
      lv_delete_flag  = 'X'.
*" 过滤掉金额字段和数量字段全部等于0的记录
      LOOP AT lt_field INTO ls_field WHERE datatype EQ 'CURR' OR
                                           datatype EQ 'QUAN'.
        CLEAR lv_field_name.
        CONCATENATE '<fs_output_line>-' ls_field-fieldname INTO lv_field_name.
        ASSIGN (lv_field_name) TO <fs_field_value>.
        IF sy-subrc = 0.
          IF <fs_field_value> <> 0. " 任意一个不等于0, 则不删除此行
            lv_delete_flag  = ''.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF lv_delete_flag EQ 'X'.
        DELETE <fs_output_table>. " 删除此行
        CONTINUE.
      ENDIF.

      IF p_lvl1 EQ 'X' OR
          p_sum1 EQ 'X'.
        CLEAR lv_field_name.
*" 一级科目&一级科目文本
        MOVE-CORRESPONDING <fs_output_line> TO ls_ZSFIRPT_003_ALV.
        CONCATENATE '<fs_output_line>-' 'ergsl' INTO lv_field_name.
        ASSIGN (lv_field_name) TO <fs_field_value>.
        IF sy-subrc = 0.
          <fs_field_value> = ls_ZSFIRPT_003_ALV-racct+0(4).
          READ TABLE gt_fagl_011qt INTO ls_fagl_011qt WITH KEY versn = 'CXZX'
                                                               spras = sy-langu
                                                               ergsl = <fs_field_value>
*                                                               txtyp = 'E' " Which value ???
                                                               BINARY SEARCH.
          IF sy-subrc = 0.
            CONCATENATE '<fs_output_line>-' 'ergsl_text' INTO lv_field_name_text.
            ASSIGN (lv_field_name_text) TO <fs_field_value_text>.
            IF sy-subrc = 0.
              <fs_field_value_text> = ls_fagl_011qt-txt45.
            ENDIF.
          ELSE.
*" 在fagl_011qt 中未找到记录
            CONCATENATE '<fs_output_line>-' 'ergsl_text' INTO lv_field_name_text.
            ASSIGN (lv_field_name_text) TO <fs_field_value_text>.
            IF sy-subrc = 0.
              CASE <fs_field_value>.
                WHEN '1231'.
                  <fs_field_value_text> = '坏账准备'.
                WHEN '1301'.
                  <fs_field_value_text> = '委托贷款'.
                WHEN '1471'.
                  <fs_field_value_text> = '存货跌价准备'.
                WHEN '1702'.
                  <fs_field_value_text> = '累计摊销'.
                WHEN OTHERS.
                  " do nothing...
              ENDCASE.
            ENDIF.

          ENDIF.
        ELSE.
*" 在fagl_011qt 中未找到记录
          CASE <fs_field_value>.
            WHEN '1231'.
              <fs_field_value_text> = '坏账准备'.
            WHEN '1301'.
              <fs_field_value_text> = '委托贷款'.
            WHEN '1471'.
              <fs_field_value_text> = '存货跌价准备'.
            WHEN '1702'.
              <fs_field_value_text> = '累计摊销'.
            WHEN OTHERS.
              " do nothing...
          ENDCASE.
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDIF.


*" 科目升序排列
  CLEAR: lt_sort,
         ls_sort.
  ls_sort-name = 'RACCT'.
  APPEND ls_sort TO lt_sort.
  SORT <fs_output_table> BY (lt_sort).

  IF p_sum1 EQ 'X'.
*" 汇总一级科目
    IF <fs_output_line> IS NOT ASSIGNED.
      ASSIGN gr_dynamic_line->* TO <fs_output_line>.
    ENDIF.

    CREATE DATA lr_dynamic_table LIKE TABLE OF <fs_output_line>.
    ASSIGN lr_dynamic_table->* TO <fs_output_tablle_collect>.

    LOOP AT <fs_output_table> ASSIGNING <fs_output_line>.
      MOVE-CORRESPONDING <fs_output_line> TO ls_lvl1_hkont.
      CLEAR <fs_output_line>.
      MOVE-CORRESPONDING ls_lvl1_hkont TO <fs_output_line>.
      COLLECT <fs_output_line> INTO <fs_output_tablle_collect>.
    ENDLOOP.

    <fs_output_table> = <fs_output_tablle_collect>.

  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM data_process_document_currency .

ENDFORM.

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM data_process_local_currency .

ENDFORM.

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM data_process_text .

*" 科目文本
  TYPES: BEGIN OF ty_skat,
           saknr TYPE skat-saknr,
           txt20 TYPE  skat-txt20,
           txt50 TYPE skat-txt50,
         END OF ty_skat.

*" 成本中心文本
  TYPES: BEGIN OF ty_kostl,
           kostl TYPE cskt-kostl,
           ktext TYPE cskt-ktext,
         END OF ty_kostl.

*" 利润中心文本
  TYPES: BEGIN OF ty_prctr,
           prctr TYPE cepc-prctr,
           ktext TYPE cepct-ktext,
         END OF ty_prctr.

*"物料描述
  TYPES: BEGIN OF ty_makt,
           matnr TYPE makt-matnr,
           maktx TYPE makt-maktx,
         END OF ty_makt.

*" 业务范围
  TYPES: BEGIN OF ty_gsber,
           gsber TYPE bseg-gsber,
           gtext TYPE tgsbt-gtext,
         END OF ty_gsber.

*" 功能范围
  TYPES: BEGIN OF ty_fkber,
           fkber TYPE faglflext-rfarea,
           fkbtx TYPE tfkbt-fkbtx,
         END OF ty_fkber.

  DATA: ls_skat      TYPE ty_skat,
        lt_skat_temp TYPE STANDARD TABLE OF ty_skat,
        lt_skat      TYPE STANDARD TABLE OF ty_skat.

  DATA: ls_kostl      TYPE ty_kostl,
        lt_kostl_temp TYPE STANDARD TABLE OF ty_kostl,
        lt_kostl      TYPE STANDARD TABLE OF ty_kostl.

  DATA: ls_prctr      TYPE ty_prctr,
        lt_prctr_temp TYPE STANDARD TABLE OF ty_prctr,
        lt_prctr      TYPE STANDARD TABLE OF ty_prctr.

  DATA: ls_gsber      TYPE ty_gsber,
        lt_gsber_temp TYPE STANDARD TABLE OF ty_gsber,
        lt_gsber      TYPE STANDARD TABLE OF ty_gsber.

  DATA: ls_fkber      TYPE ty_fkber,
        lt_fkber_temp TYPE STANDARD TABLE OF ty_fkber,
        lt_fkber      TYPE STANDARD TABLE OF ty_fkber.

**" 物料描述
*  DATA: ls_makt      TYPE ty_makt,
*        lt_makt_temp TYPE STANDARD TABLE OF ty_makt,
*        lt_makt      TYPE STANDARD TABLE OF ty_makt.


  DATA: lv_field_name      TYPE char80,
        lv_field_key_name  TYPE char80, " 编码
        lv_field_key_value TYPE char80. " 编码描述

  FIELD-SYMBOLS: <fv_field_key>   TYPE any,
                 <fv_field_value> TYPE any.

  FIELD-SYMBOLS <l_bseg> LIKE LINE OF gt_bseg.

  LOOP AT <fs_output_table> ASSIGNING <fs_output_line>. " 参照结构：ZSFIRPT_003_ALV

*" 科目
    CONCATENATE '<fs_output_line>-' 'racct' INTO lv_field_name.
    ASSIGN (lv_field_name) TO <fv_field_value>.
    IF sy-subrc = 0.
      ls_skat-saknr = <fv_field_value>.
      APPEND ls_skat TO lt_skat_temp.
    ENDIF.

*" 成本中心
    CONCATENATE '<fs_output_line>-' 'RCNTR' INTO lv_field_name.
    ASSIGN (lv_field_name) TO <fv_field_value>.
    IF sy-subrc = 0.
      ls_kostl-kostl = <fv_field_value>.
      APPEND ls_kostl TO lt_kostl_temp.
    ENDIF.

*" 利润中心
    CONCATENATE '<fs_output_line>-' 'PRCTR' INTO lv_field_name.
    ASSIGN (lv_field_name) TO <fv_field_value>.
    IF sy-subrc = 0.
      ls_prctr-prctr = <fv_field_value>.
      APPEND ls_prctr TO lt_prctr_temp.
    ENDIF.

*" 业务范围
    CONCATENATE '<fs_output_line>-' 'RBUSA' INTO lv_field_name.
    ASSIGN (lv_field_name) TO <fv_field_value>.
    IF sy-subrc = 0.
      ls_gsber-gsber = <fv_field_value>.
      APPEND ls_gsber TO lt_gsber_temp.
    ENDIF.

*" 功能范围
    CONCATENATE '<fs_output_line>-' 'RFAREA' INTO lv_field_name.
    ASSIGN (lv_field_name) TO <fv_field_value>.
    IF sy-subrc = 0.
      ls_fkber-fkber = <fv_field_value>.
      APPEND ls_fkber TO lt_fkber_temp.
    ENDIF.

  ENDLOOP.

*" 查询编码的文本描述
  DELETE lt_skat_temp WHERE saknr = ''.
  SORT lt_skat_temp ASCENDING BY saknr.
  DELETE ADJACENT DUPLICATES FROM lt_skat_temp.
  IF lines( lt_skat_temp ) > 0.
    SELECT
      saknr
      txt20
      txt50
      INTO TABLE lt_skat
      FROM skat
        FOR ALL ENTRIES IN lt_skat_temp
      WHERE spras = sy-langu
        AND ktopl = 'CXZX'
        AND saknr = lt_skat_temp-saknr.
  ENDIF.

  DELETE lt_kostl_temp WHERE kostl = ''.
  SORT lt_kostl_temp ASCENDING BY kostl.
  DELETE ADJACENT DUPLICATES FROM lt_kostl_temp.
  IF lines( lt_kostl_temp ) > 0.
    SELECT
      kostl
      ktext
      INTO TABLE lt_kostl
      FROM cskt
        FOR ALL ENTRIES IN lt_kostl_temp
      WHERE spras = sy-langu
        AND kokrs = '3000'
        AND kostl = lt_kostl_temp-kostl
        AND datbi >= sy-datum
        .
  ENDIF.

  DELETE lt_prctr_temp WHERE prctr = ''.
  SORT lt_prctr_temp ASCENDING BY prctr.
  DELETE ADJACENT DUPLICATES FROM lt_prctr_temp.
  IF lines( lt_prctr_temp ) > 0.
    SELECT
      prctr
      ktext
      INTO TABLE lt_prctr
      FROM cepct
        FOR ALL ENTRIES IN lt_prctr_temp
      WHERE spras = sy-langu
        AND prctr = lt_prctr_temp-prctr
        AND datbi >= sy-datum
        AND kokrs = '3000'.
  ENDIF.

  DELETE lt_gsber_temp WHERE gsber = ''.
  SORT lt_gsber_temp ASCENDING BY gsber.
  DELETE ADJACENT DUPLICATES FROM lt_gsber_temp.
  IF lines( lt_gsber_temp ) > 0.
    SELECT
      gsber
      gtext
      INTO TABLE lt_gsber
      FROM tgsbt
        FOR ALL ENTRIES IN lt_gsber_temp
      WHERE spras = sy-langu
        AND gsber = lt_gsber_temp-gsber.
  ENDIF.

  DELETE lt_fkber_temp WHERE fkber = ''.
  SORT lt_fkber_temp ASCENDING BY fkber.
  DELETE ADJACENT DUPLICATES FROM lt_fkber_temp.
  IF lines( lt_fkber_temp ) > 0.
    SELECT
      fkber
      fkbtx
      INTO TABLE lt_fkber
      FROM tfkbt
        FOR ALL ENTRIES IN lt_fkber_temp
      WHERE spras = sy-langu
        AND fkber = lt_fkber_temp-fkber.
  ENDIF.


*" sort sort sort
  SORT lt_skat  ASCENDING BY saknr.
  SORT lt_kostl ASCENDING BY kostl.
  SORT lt_prctr ASCENDING BY prctr.
  SORT lt_gsber ASCENDING BY gsber.
  SORT lt_fkber ASCENDING BY fkber.
*  SORT lt_makt ASCENDING BY matnr.

  LOOP AT <fs_output_table> ASSIGNING <fs_output_line>.

*" 科目的文本描述
    CONCATENATE '<fs_output_line>-' 'racct' INTO lv_field_key_name.
    ASSIGN (lv_field_key_name) TO <fv_field_key>.
    IF sy-subrc = 0.
      CONCATENATE '<fs_output_line>-' 'racct_text' INTO lv_field_name.
      ASSIGN (lv_field_name) TO <fv_field_value>.
      IF sy-subrc = 0.
        READ TABLE lt_skat INTO ls_skat WITH KEY saknr = <fv_field_key> BINARY SEARCH.
        IF sy-subrc = 0.
          <fv_field_value> = ls_skat-txt50.
        ENDIF.
      ENDIF.
    ENDIF.

*" 成本中心文本描述
    CONCATENATE '<fs_output_line>-' 'rcntr' INTO lv_field_key_name.
    ASSIGN (lv_field_key_name) TO <fv_field_key>.
    IF sy-subrc = 0.
      CONCATENATE '<fs_output_line>-' 'rcntr_text' INTO lv_field_name.
      ASSIGN (lv_field_name) TO <fv_field_value>.
      IF sy-subrc = 0.
        READ TABLE lt_kostl INTO ls_kostl WITH KEY kostl = <fv_field_key> BINARY SEARCH.
        IF sy-subrc = 0.
          <fv_field_value> = ls_kostl-ktext.
        ENDIF.
      ENDIF.
    ENDIF.

    IF sy-subrc = 0.
      LOOP AT gt_bseg ASSIGNING <l_bseg> WHERE kostl = <fv_field_key>
                                            AND kostl_text = ''.
        <l_bseg>-kostl_text = ls_kostl-ktext.
      ENDLOOP.
    ENDIF.

*" 利润中心文本描述
    CONCATENATE '<fs_output_line>-' 'prctr' INTO lv_field_key_name.
    ASSIGN (lv_field_key_name) TO <fv_field_key>.
    IF sy-subrc = 0.
      CONCATENATE '<fs_output_line>-' 'prctr_text' INTO lv_field_name.
      ASSIGN (lv_field_name) TO <fv_field_value>.
      IF sy-subrc = 0.
        READ TABLE lt_prctr INTO ls_prctr WITH KEY prctr = <fv_field_key> BINARY SEARCH.
        IF sy-subrc = 0.
          <fv_field_value> = ls_prctr-ktext.
        ENDIF.
      ENDIF.
    ENDIF.

    IF sy-subrc = 0.
      LOOP AT gt_bseg ASSIGNING <l_bseg> WHERE prctr = <fv_field_key>
                                            AND prctr_text = ''.
        <l_bseg>-prctr_text = ls_prctr-ktext.
      ENDLOOP.
    ENDIF.
*" 业务范围
    CONCATENATE '<fs_output_line>-' 'rbusa' INTO lv_field_key_name.
    ASSIGN (lv_field_key_name) TO <fv_field_key>.
    IF sy-subrc = 0.
      CONCATENATE '<fs_output_line>-' 'rbusa_text' INTO lv_field_name.
      ASSIGN (lv_field_name) TO <fv_field_value>.
      IF sy-subrc = 0.
        READ TABLE lt_gsber INTO ls_gsber WITH KEY gsber = <fv_field_key> BINARY SEARCH.
        IF sy-subrc = 0.
          <fv_field_value> = ls_gsber-gtext.
        ENDIF.
      ENDIF.
    ENDIF.

*" 功能范围
    CONCATENATE '<fs_output_line>-' 'rfarea' INTO lv_field_key_name.
    ASSIGN (lv_field_key_name) TO <fv_field_key>.
    IF sy-subrc = 0.
      CONCATENATE '<fs_output_line>-' 'rfarea_text' INTO lv_field_name.
      ASSIGN (lv_field_name) TO <fv_field_value>.
      IF sy-subrc = 0.
        READ TABLE lt_fkber INTO ls_fkber WITH KEY fkber = <fv_field_key> BINARY SEARCH.
        IF sy-subrc = 0.
          <fv_field_value> = ls_fkber-fkbtx.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
* Description: 借贷方向(文本)的处理
*---------------------------------------------------------------------*
FORM data_process_debit_credit .

  DATA: ls_alv_common TYPE ZSFIRPT_003_ALV_common,
        lv_field_name TYPE char80.

  FIELD-SYMBOLS: <fv_field_value> TYPE any.

  CHECK <fs_output_table> IS NOT INITIAL.
  LOOP AT <fs_output_table> ASSIGNING <fs_output_line>.
    MOVE-CORRESPONDING <fs_output_line> TO ls_alv_common.
*" 期初方向
    lv_field_name = '<FS_OUTPUT_LINE>-OPENING_SHKZG_TEXT'.
    ASSIGN (lv_field_name) TO <fv_field_value>.
    IF sy-subrc = 0.
      IF ls_alv_common-opening_balance > 0.
        <fv_field_value>  = '借'.
      ELSEIF ls_alv_common-opening_balance < 0.
        <fv_field_value>  = '贷'.
      ELSE.
        <fv_field_value>  = '平'.
      ENDIF.
    ENDIF.

*" 期末方向
    lv_field_name = '<FS_OUTPUT_LINE>-CLOSING_SHKZG'.
    ASSIGN (lv_field_name) TO <fv_field_value>.
    IF sy-subrc = 0.
      IF ls_alv_common-closing_balance > 0.
        <fv_field_value>  = '借'.
      ELSEIF ls_alv_common-closing_balance < 0.
        <fv_field_value>  = '贷'.
      ELSE.
        <fv_field_value>  = '平'.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  data_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_display.

  DATA: lv_title            TYPE lvc_title,
        lv_callback_program TYPE sy-repid VALUE 'ZSHXJFI0003',
        ls_variant          TYPE disvariant.

  IF  p_save = 'X'.
    PERFORM save_data.
  ENDIF.

  PERFORM alv_layout_build.
  PERFORM alv_fieldcat_build CHANGING gt_fieldcat.
  PERFORM alv_event_build.
  PERFORM alv_grid_title CHANGING lv_title.

  ls_variant-report = 'ZSHXJFI0003'.
  IF p_hwaer EQ 'X'.
    ls_variant-handle = 'LHWA'.
  ELSEIF p_waers EQ 'X'.
    ls_variant-handle = 'LWAE'.
  ELSE.
    " do nothing...
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK       = ' '
*     I_BYPASSING_BUFFER      =
*     I_BUFFER_ACTIVE         =
      i_callback_program      = lv_callback_program
*     I_CALLBACK_PF_STATUS_SET          = ' '
      i_callback_user_command = 'ALV_USER_COMMAND'
      i_callback_top_of_page  = 'ALV_TOP_OF_PAGE'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME        =
*     I_BACKGROUND_ID         = ' '
      i_grid_title            = lv_title
*     I_GRID_SETTINGS         =
      is_layout_lvc           = gs_layout
      it_fieldcat_lvc         = gt_fieldcat
*     IT_EXCLUDING            =
*     IT_SPECIAL_GROUPS_LVC   =
*     IT_SORT_LVC             =
*     IT_FILTER_LVC           =
*     IT_HYPERLINK            =
*     IS_SEL_HIDE             =
*     I_DEFAULT               = 'X'
      i_save                  = 'A'
      is_variant              = ls_variant
*     IT_EVENTS               =
*     IT_EVENT_EXIT           =
*     IS_PRINT_LVC            =
*     IS_REPREP_ID_LVC        =
*     I_SCREEN_START_COLUMN   = 0
*     I_SCREEN_START_LINE     = 0
*     I_SCREEN_END_COLUMN     = 0
*     I_SCREEN_END_LINE       = 0
*     I_HTML_HEIGHT_TOP       =
*     I_HTML_HEIGHT_END       =
*     IT_ALV_GRAPHICS         =
*     IT_EXCEPT_QINFO_LVC     =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER =
*     ES_EXIT_CAUSED_BY_USER  =
    TABLES
      t_outtab                = <fs_output_table> " gt_output
*   EXCEPTIONS
*     PROGRAM_ERROR           = 1
*     OTHERS                  = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM. "data_display

*&---------------------------------------------------------------------*
*&      Form  alv_fieldcat_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_fieldcat_build CHANGING ct_fieldcat TYPE lvc_t_fcat.

  DATA: ls_fieldcat TYPE lvc_s_fcat.

  LOOP AT ct_fieldcat INTO ls_fieldcat.
    CASE ls_fieldcat-fieldname.
      WHEN 'HWAER'.
        ls_fieldcat-no_out  = abap_true.
      WHEN 'SHKZG'.
        ls_fieldcat-no_out  = abap_true.
      WHEN 'OPENING_SHKZG'.
        ls_fieldcat-no_out  = abap_true.
      WHEN 'OPENING_SHKZG_TEXT'.
*        ls_fieldcat-no_out  = abap_true.
      WHEN 'CLOSING_SHKZG'.
*        ls_fieldcat-no_out  = abap_true.
      WHEN 'CURRENCY'.
        ls_fieldcat-no_out  = abap_true.
      WHEN OTHERS.
        " do nothing...
    ENDCASE.
    MODIFY ct_fieldcat FROM ls_fieldcat.
  ENDLOOP.

ENDFORM. "alv_fieldcat_build

*&---------------------------------------------------------------------*
*&      Form  alv_fieldcat_fill
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->COL_POS      text
*      -->FIELDNAME    text
*      -->NO_OUT       text
*      -->REPTEXT      text
*      -->CT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM alv_fieldcat_fill USING col_pos TYPE lvc_s_fcat-col_pos
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


ENDFORM. "alv_fieldcat_fill

*&---------------------------------------------------------------------*
*&      Form  alv_layout_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_layout_build.
  gs_layout-zebra         = abap_true.
  gs_layout-cwidth_opt    = abap_true.
  gs_layout-smalltitle    = abap_true.
*  gs_layout-sel_mode      = ''.
*  gs_layout-excp_fname    = ''.
*  gs_layout-stylefname    = ''.

ENDFORM. "alv_layout_build


*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*-----------------------------------------------------  ----------------*
FORM alv_title_build CHANGING ch_title TYPE lvc_title.

  DATA: lv_lines        TYPE i,
        lv_lines_string TYPE string.

  lv_lines = lines( gt_output ).
  lv_lines_string = lv_lines.
  CONDENSE lv_lines_string.

  ch_title = '条目数: ' && lv_lines_string.

ENDFORM.

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM alv_pf_status USING rt_excluded_fcode TYPE slis_t_extab.

*  set pf-status 'STANDARD_FULLSCREEN' excluding rt_excluded_fcode.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_user_command USING r_ucomm TYPE sy-ucomm
                            ps_selfield TYPE slis_selfield.

  IF r_ucomm EQ '&IC1'.
    PERFORM alv_handle_double_click USING ps_selfield.
  ENDIF.

  ps_selfield-refresh = abap_true.

ENDFORM. "ALV_USER_COMMAND

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM alv_handle_dclick_condition USING is_output_line TYPE any
                                 CHANGING ct_condition   TYPE tchar255.
*" 确定用户选择的汇总的维度, 以及 双击的字段, 确定 CONDITION

  DATA: ls_fieldcat_cond TYPE lvc_s_fcat,
        lv_field_name    TYPE feldname.

  DATA: ls_condition TYPE char255.

  FIELD-SYMBOLS: <fs_field_value> TYPE any.

  LOOP AT gt_fieldcat_dynamic_cond INTO ls_fieldcat_cond.
    CLEAR: lv_field_name.

    CONCATENATE 'is_output_line-' ls_fieldcat_cond-fieldname INTO lv_field_name.
    ASSIGN (lv_field_name) TO <fs_field_value>.
    IF sy-subrc = 0.
*      if ls_condition is initial.
*" FAGLFLEXT表里的字段名和BSEG表里的字段名不一样，这里做个处理
      CASE ls_fieldcat_cond-fieldname.
        WHEN 'RBUKRS'.
          ls_fieldcat_cond-fieldname = 'BUKRS'.
        WHEN 'RACCT'.
          ls_fieldcat_cond-fieldname = 'HKONT'.
        WHEN 'RCNTR'.
          ls_fieldcat_cond-fieldname = 'KOSTL'.
        WHEN 'RBUSA'.
          ls_fieldcat_cond-fieldname = 'GSBER'.
        WHEN 'RFAREA'.
*          ls_fieldcat_cond-fieldname = 'FKBER_LONG'.
          ls_fieldcat_cond-fieldname = 'FKBER'.
        WHEN 'WAERS'.
          ls_fieldcat_cond-fieldname = 'PSWSL'. " 总账的统计货币 ???
        WHEN OTHERS.
          " do nothing...
      ENDCASE.
      IF ct_condition IS INITIAL.
        CONCATENATE ls_fieldcat_cond-fieldname
                    space
                    '='
                    space
                    ''''
                    <fs_field_value>
                    ''''
                    INTO ls_condition RESPECTING BLANKS.
        APPEND ls_condition TO ct_condition.
      ELSE.
        CONCATENATE 'AND' space
                    ls_fieldcat_cond-fieldname
                    space
                    '='
                    space space
                    ''''<fs_field_value>''''
                    INTO ls_condition RESPECTING BLANKS.
        APPEND ls_condition TO ct_condition.
      ENDIF.
    ENDIF.
    CLEAR ls_condition.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM alv_handle_double_click USING ps_selfield TYPE slis_selfield.

  DATA: ls_fieldcat_cond TYPE lvc_s_fcat,
        lv_field_name    TYPE feldname.

  DATA: lt_condition TYPE STANDARD TABLE OF char255,
        ls_condition TYPE char255.

  DATA: lt_bseg TYPE STANDARD TABLE OF ZSFIRPT_003_ALV_item.

  DATA: lv_online_time TYPE datum,
        ls_t093c       TYPE t093c,
        lv_period      TYPE numc2.

  FIELD-SYMBOLS: <fs_bseg> LIKE LINE OF gt_bseg.

  FIELD-SYMBOLS: <fs_field_value>   TYPE any,
                 <l_fs_output_line> TYPE any.

  DATA: lv_field_racct TYPE char80,
        lv_racct       TYPE faglflext-racct,
        ls_ska1        LIKE LINE OF gt_ska1.
  FIELD-SYMBOLS: <fv_racct> TYPE faglflext-racct.

  DATA: ls_maktx TYPE string,
        lt_maktx TYPE TABLE OF string.





  IF p_sum1 EQ 'X'.
    RETURN. " 一级科目汇总时不显示明细数据，意义不大
  ENDIF.



*" 取行项目明细数据
  IF ps_selfield-fieldname EQ 'OPENING_BALANCE' OR
      ps_selfield-fieldname EQ 'OPENING_BALANCE_WRBTR' OR
      ps_selfield-fieldname EQ 'DMBTR_DEBIT' OR
      ps_selfield-fieldname EQ 'WRBTR_DEBIT' OR
      ps_selfield-fieldname EQ 'DMBTR_CREDIT' OR
      ps_selfield-fieldname EQ 'WRBTR_CREDIT' OR
      ps_selfield-fieldname EQ 'DMBTR_DEBIT_CUM' OR
      ps_selfield-fieldname EQ 'WRBTR_DEBIT_CUM' OR
      ps_selfield-fieldname EQ 'DMBTR_CREDIT_CUM' OR
      ps_selfield-fieldname EQ 'WRBTR_CREDIT_CUM' OR
      ps_selfield-fieldname EQ 'CLOSING_BALANCE' OR
      ps_selfield-fieldname EQ 'CLOSING_BALANCE_WRBTR' OR
      ps_selfield-fieldname EQ 'DMBTR_DEBIT_CUM_YEAR' OR
      ps_selfield-fieldname EQ 'WRBTR_DEBIT_CUM_YEAR' OR
      ps_selfield-fieldname EQ 'MENGE_DEBIT_CUM_YEAR' OR
      ps_selfield-fieldname EQ 'DMBTR_CREDIT_CUM_YEAR' OR
      ps_selfield-fieldname EQ 'WRBTR_CREDIT_CUM_YEAR' OR
      ps_selfield-fieldname EQ 'MENGE_CREDIT_CUM_YEAR' .

*" 取明细数据
    DATA: lv_field_name11      TYPE char80,
          lv_field_key_name11  TYPE char80, " 编码
          lv_field_key_value11 TYPE char80. " 编码描述
    FIELD-SYMBOLS: <fv_field_key11>   TYPE any.
    READ TABLE <fs_output_table> ASSIGNING <fs_output_line> INDEX ps_selfield-tabindex.
    IF sy-subrc = 0.
      CONCATENATE '<fs_output_line>-' 'racct' INTO lv_field_key_name11.
      ASSIGN (lv_field_key_name11) TO <fv_field_key11>.
      IF sy-subrc = 0.
        REFRESH g_hkont.
        g_hkont-sign = 'I'.
        g_hkont-option = 'EQ'.
        g_hkont-low = <fv_field_key11>.
        APPEND g_hkont.
      ENDIF.
    ENDIF.
    PERFORM line_item_get.

*" 利润中心文本
    TYPES: BEGIN OF ty_prctr,
             prctr TYPE cepc-prctr,
             ktext TYPE cepct-ktext,
           END OF ty_prctr.

*" 成本中心文本
    TYPES: BEGIN OF ty_kostl,
             kostl TYPE cskt-kostl,
             ktext TYPE cskt-ktext,
           END OF ty_kostl.
*" 客户
    TYPES: BEGIN OF ty_kna1,
             kunnr TYPE kna1-kunnr,
             name1 TYPE kna1-name1,
             name2 TYPE kna1-name2,
           END OF ty_kna1.

*" 供应商
    TYPES: BEGIN OF ty_lfa1,
             lifnr TYPE lfa1-lifnr,
             name1 TYPE lfa1-name1,
             name2 TYPE lfa1-name2,
           END OF ty_lfa1.

*"物料描述
    TYPES: BEGIN OF ty_makt,
             matnr TYPE makt-matnr,
             maktx TYPE makt-maktx,
             vtext TYPE tvkmt-vtext,
           END OF ty_makt.


*" 成本中心
    DATA: ls_kostl TYPE ty_kostl,
          lt_kostl TYPE STANDARD TABLE OF ty_kostl.

*" 利润中心
    DATA: ls_prctr TYPE ty_prctr,
          lt_prctr TYPE STANDARD TABLE OF ty_prctr.

*" 客户
    DATA: ls_kna1 TYPE ty_kna1,
          lt_kna1 TYPE STANDARD TABLE OF ty_kna1.

*" 供应商
    DATA: ls_lfa1 TYPE ty_lfa1,
          lt_lfa1 TYPE STANDARD TABLE OF ty_lfa1.

*" 物料描述
    DATA: ls_makt TYPE ty_makt,
          lt_makt TYPE STANDARD TABLE OF ty_makt.


*利润中心文本
    lt_bseg = gt_bseg.
    DELETE lt_bseg WHERE prctr = ''.
    SORT lt_bseg BY prctr.
    DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING prctr.
    IF NOT lt_bseg IS INITIAL.
      SELECT
            prctr
            ktext
       INTO TABLE lt_prctr
       FROM cepct
        FOR ALL ENTRIES IN lt_bseg
      WHERE spras = sy-langu
        AND prctr = lt_bseg-prctr
        AND datbi >= sy-datum
        AND kokrs = '3000'.
      IF sy-subrc = 0.
        SORT lt_prctr BY prctr.
      ENDIF.
    ENDIF.

*成本中心文本
    lt_bseg = gt_bseg.
    DELETE lt_bseg WHERE kostl = ''.
    SORT lt_bseg BY kostl.
    DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING kostl.
    IF NOT lt_bseg IS INITIAL.
      SELECT
            kostl
            ktext
       INTO TABLE lt_kostl
       FROM cskt
        FOR ALL ENTRIES IN lt_bseg
      WHERE spras = sy-langu
        AND kokrs = '3000'
        AND kostl = lt_bseg-kostl
        AND datbi >= sy-datum.
      IF sy-subrc = 0.
        SORT lt_kostl BY kostl.
      ENDIF.
    ENDIF.

*客户
    lt_bseg = gt_bseg.
    DELETE lt_bseg WHERE kunnr = ''.
    SORT lt_bseg BY kunnr.
    DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING kunnr.
    IF NOT lt_bseg IS INITIAL.
      SELECT
            kunnr
            name1
       INTO TABLE lt_kna1
       FROM kna1
        FOR ALL ENTRIES IN lt_bseg
      WHERE kunnr = lt_bseg-kunnr.
      IF sy-subrc = 0.
        SORT lt_kna1 BY kunnr.
      ENDIF.
    ENDIF.

    lt_bseg = gt_bseg.
    DELETE lt_bseg WHERE vptnr = ''.
    SORT lt_bseg BY vptnr.
    DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING vptnr.
    IF NOT lt_bseg IS INITIAL.
      SELECT kunnr name1 APPENDING TABLE lt_kna1
        FROM kna1
        FOR ALL ENTRIES IN lt_bseg
        WHERE kunnr = lt_bseg-vptnr.
      IF sy-subrc = 0.
        SORT lt_kna1 BY kunnr.
      ENDIF.
    ENDIF.

*供应商
    lt_bseg = gt_bseg.
    DELETE lt_bseg WHERE lifnr = ''.
    SORT lt_bseg BY lifnr.
    DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING lifnr.
    IF NOT lt_bseg IS INITIAL.
      SELECT
            lifnr
            name1
       INTO TABLE lt_lfa1
       FROM lfa1
        FOR ALL ENTRIES IN lt_bseg
      WHERE lifnr = lt_bseg-lifnr.
      IF sy-subrc = 0.
        SORT lt_lfa1 BY lifnr.
      ENDIF.
    ENDIF.

*物料描述文本
    lt_bseg = gt_bseg.
    DELETE lt_bseg WHERE matnr = ''.
    SORT lt_bseg BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING matnr.
    IF NOT lt_bseg IS INITIAL.
      SELECT
            makt~matnr
            maktx
            tvkmt~vtext
       INTO TABLE lt_makt
       FROM makt
        INNER JOIN mvke ON mvke~matnr = makt~matnr
        INNER JOIN tvkmt ON tvkmt~ktgrm = mvke~ktgrm
        FOR ALL ENTRIES IN lt_bseg
      WHERE makt~spras = sy-langu
        AND tvkmt~spras = sy-langu
        AND makt~matnr = lt_bseg-matnr.
      IF sy-subrc = 0.
        SORT lt_makt BY matnr.
      ENDIF.
    ENDIF.


*文本赋值
    LOOP AT gt_bseg ASSIGNING <fs_bseg>.
      IF NOT <fs_bseg>-prctr IS INITIAL.
        READ TABLE lt_prctr INTO ls_prctr
          WITH KEY prctr = <fs_bseg>-prctr BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_bseg>-prctr_text = ls_prctr-ktext.
        ENDIF.
      ENDIF.

      IF NOT <fs_bseg>-kostl IS INITIAL.
        READ TABLE lt_kostl INTO ls_kostl
          WITH KEY kostl = <fs_bseg>-kostl BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_bseg>-kostl_text = ls_kostl-ktext.
        ENDIF.
      ENDIF.

      IF NOT <fs_bseg>-kunnr IS INITIAL.
        READ TABLE lt_kna1 INTO ls_kna1
          WITH KEY kunnr = <fs_bseg>-kunnr BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_bseg>-kunnr_text = ls_kna1-name1.
        ENDIF.
      ENDIF.
      IF NOT <fs_bseg>-vptnr IS INITIAL.
        READ TABLE lt_kna1 INTO ls_kna1
                 WITH KEY kunnr = <fs_bseg>-vptnr BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_bseg>-vptnr_text = ls_kna1-name1.
        ENDIF.
      ENDIF.

      IF NOT <fs_bseg>-lifnr IS INITIAL.
        READ TABLE lt_lfa1 INTO ls_lfa1
          WITH KEY lifnr = <fs_bseg>-lifnr BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_bseg>-lifnr_text = ls_lfa1-name1.
        ENDIF.
      ENDIF.

      IF NOT <fs_bseg>-matnr IS INITIAL.
        READ TABLE lt_makt INTO ls_makt
          WITH KEY matnr = <fs_bseg>-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_bseg>-matnr_text = ls_makt-maktx.
          <fs_bseg>-vtext = ls_makt-vtext.

          SEARCH ls_makt-maktx FOR '\'.
          IF sy-subrc = 0.

            SPLIT ls_makt-maktx AT '\' INTO TABLE lt_maktx."将字符串以","为分割导入内表
            READ TABLE lt_maktx INTO ls_maktx INDEX 1.
            <fs_bseg>-matnr_text1 = ls_maktx.

          ENDIF.



        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDIF.

  UNASSIGN <l_fs_output_line>.
*" Work area
  IF <l_fs_output_line> IS NOT ASSIGNED.
    ASSIGN gr_dynamic_line->* TO <l_fs_output_line>.
  ENDIF.

  READ TABLE <fs_output_table> INTO <l_fs_output_line> INDEX ps_selfield-tabindex.
  CHECK sy-subrc = 0.
  lv_field_racct = '<l_fs_output_line>-' && 'racct'.
  ASSIGN (lv_field_racct) TO <fv_racct>.
  IF sy-subrc = 0.
    lv_racct  = <fv_racct>.
    CLEAR ls_ska1.
    READ TABLE gt_ska1 INTO ls_ska1 WITH KEY ktopl = 'CXZX'
                                             saknr = lv_racct
                                             BINARY SEARCH.
  ENDIF.

*" 确定用户选择的汇总的维度, 以及 双击的字段, 确定 CONDITION
  PERFORM alv_handle_dclick_condition USING    <l_fs_output_line>
                                      CHANGING lt_condition.

*" 凭证货币

  CASE ps_selfield-fieldname.
    WHEN 'OPENING_BALANCE'. " 期初金额
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).
        IF <fs_bseg>-budat < gv_budat_from.
          APPEND <fs_bseg> TO lt_bseg.
        ENDIF.
      ENDLOOP.

      PERFORM line_item_display USING lt_bseg.

    WHEN 'OPENING_BALANCE_WRBTR'. " 期初凭证货币金额
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).
        IF <fs_bseg>-budat < gv_budat_from.
          APPEND <fs_bseg> TO lt_bseg.
        ENDIF.
      ENDLOOP.

      PERFORM line_item_display USING lt_bseg.
    WHEN 'DMBTR_DEBIT'. " 借方金额
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).
        IF gv_budat_from <= <fs_bseg>-budat AND <fs_bseg>-budat <= gv_budat_to.
          IF <fs_bseg>-shkzg EQ 'S'.
            APPEND <fs_bseg> TO lt_bseg.
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM line_item_display USING lt_bseg.
    WHEN 'WRBTR_DEBIT'. " 借方凭证货币
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).
        IF gv_budat_from <= <fs_bseg>-budat AND <fs_bseg>-budat <= gv_budat_to.
          IF <fs_bseg>-shkzg EQ 'S'.
            APPEND <fs_bseg> TO lt_bseg.
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM line_item_display USING lt_bseg.
    WHEN 'DMBTR_CREDIT'.  " 贷方金额
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).
        IF gv_budat_from <= <fs_bseg>-budat AND <fs_bseg>-budat <= gv_budat_to.
          IF <fs_bseg>-shkzg EQ 'H'.
            APPEND <fs_bseg> TO lt_bseg.
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM line_item_display USING lt_bseg.
    WHEN 'WRBTR_CREDIT'.  " 贷方凭证货币金额
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).
        IF gv_budat_from <= <fs_bseg>-budat AND <fs_bseg>-budat <= gv_budat_to.
          IF <fs_bseg>-shkzg EQ 'H'.
            APPEND <fs_bseg> TO lt_bseg.
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM line_item_display USING lt_bseg.
    WHEN 'DMBTR_DEBIT_CUM'.   " 累计借方金额
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).
        IF <fs_bseg>-shkzg EQ 'S'.
          APPEND <fs_bseg> TO lt_bseg.
        ENDIF.
      ENDLOOP.
      PERFORM line_item_display USING lt_bseg.

    WHEN 'WRBTR_DEBIT_CUM'.   " 累计借方凭证货币金额
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).
        IF <fs_bseg>-shkzg EQ 'S'.
          APPEND <fs_bseg> TO lt_bseg.
        ENDIF.
      ENDLOOP.
      PERFORM line_item_display USING lt_bseg.

    WHEN 'DMBTR_CREDIT_CUM'.  " 累计贷方金额
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).
        IF <fs_bseg>-shkzg EQ 'H'.
          APPEND <fs_bseg> TO lt_bseg.
        ENDIF.
      ENDLOOP.
      PERFORM line_item_display USING lt_bseg.

    WHEN 'WRBTR_CREDIT_CUM'.  " 累计贷方凭证货币金额
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).
        IF <fs_bseg>-shkzg EQ 'H'.
          APPEND <fs_bseg> TO lt_bseg.
        ENDIF.
      ENDLOOP.
      PERFORM line_item_display USING lt_bseg.

    WHEN 'CLOSING_BALANCE'.   " 期末余额
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).
        APPEND <fs_bseg> TO lt_bseg.
      ENDLOOP.

      PERFORM line_item_display USING lt_bseg.
    WHEN 'CLOSING_BALANCE_WRBTR'. " 期末凭证货币金额
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).
        APPEND <fs_bseg> TO lt_bseg.
      ENDLOOP.

      PERFORM line_item_display USING lt_bseg.

    WHEN 'DMBTR_DEBIT_CUM_YEAR'.  " 本年累计借方金额
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).

        IF ls_t093c-bukrs <> <fs_bseg>-bukrs.
          READ TABLE gt_t093c INTO ls_t093c
            WITH KEY bukrs = <fs_bseg>-bukrs.
          IF sy-subrc = 0.
***            lv_period       = ls_t093c-DATUM+4(2).
***            lv_online_time  = ls_t093c-DATUM+0(4) && lv_period && '01'.
***            call function 'RP_LAST_DAY_OF_MONTHS'
***              exporting
***                day_in            = lv_online_time
***              importing
***                last_day_of_month = lv_online_time
****     EXCEPTIONS
****               DAY_IN_NO_DATE    = 1
****               OTHERS            = 2
***              .
***            if sy-subrc <> 0.
**** Implement suitable error handling here
***            endif.
            lv_online_time = ls_t093c-datum.
          ENDIF.
        ENDIF.

        IF gv_budat_from_year <= <fs_bseg>-budat AND <fs_bseg>-budat <= gv_budat_to_year.
          IF <fs_bseg>-budat <= lv_online_time
             AND ls_ska1-xbilk = abap_true.
            " do nothing...过滤掉上线的期初数据
          ELSE.
            IF <fs_bseg>-shkzg EQ 'S'.
              APPEND <fs_bseg> TO lt_bseg.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM line_item_display USING lt_bseg.

    WHEN 'WRBTR_DEBIT_CUM_YEAR'.  " 本年累计借方凭证货币金额
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).

        IF ls_t093c-bukrs <> <fs_bseg>-bukrs.
          READ TABLE gt_t093c INTO ls_t093c
            WITH KEY bukrs = <fs_bseg>-bukrs.
          IF sy-subrc = 0.
***            lv_period       = ls_t093c-DATUM+4(2).
***            lv_online_time  = ls_t093c-DATUM+0(4) && lv_period && '01'.
***            call function 'RP_LAST_DAY_OF_MONTHS'
***              exporting
***                day_in            = lv_online_time
***              importing
***                last_day_of_month = lv_online_time
****     EXCEPTIONS
****               DAY_IN_NO_DATE    = 1
****               OTHERS            = 2
***              .
***            if sy-subrc <> 0.
**** Implement suitable error handling here
***            endif.
            lv_online_time = ls_t093c-datum.
          ENDIF.
        ENDIF.

        IF gv_budat_from_year <= <fs_bseg>-budat AND <fs_bseg>-budat <= gv_budat_to_year.
          IF <fs_bseg>-budat <= lv_online_time
             AND ls_ska1-xbilk = abap_true.
            " do nothing...过滤掉上线的期初数据
          ELSE.
            IF <fs_bseg>-shkzg EQ 'S'.
              APPEND <fs_bseg> TO lt_bseg.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM line_item_display USING lt_bseg.

    WHEN 'MENGE_DEBIT_CUM_YEAR'.  " 本年累计借方数量
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).

        IF ls_t093c-bukrs <> <fs_bseg>-bukrs.
          READ TABLE gt_t093c INTO ls_t093c
            WITH KEY bukrs = <fs_bseg>-bukrs.
          IF sy-subrc = 0.
***            lv_period       = ls_t093c-DATUM+4(2).
***            lv_online_time  = ls_t093c-DATUM+0(4) && lv_period && '01'.
***            call function 'RP_LAST_DAY_OF_MONTHS'
***              exporting
***                day_in            = lv_online_time
***              importing
***                last_day_of_month = lv_online_time
****     EXCEPTIONS
****               DAY_IN_NO_DATE    = 1
****               OTHERS            = 2
***              .
***            if sy-subrc <> 0.
**** Implement suitable error handling here
***            endif.
            lv_online_time = ls_t093c-datum.
          ENDIF.
        ENDIF.

        IF gv_budat_from_year <= <fs_bseg>-budat AND <fs_bseg>-budat <= gv_budat_to_year.
          IF <fs_bseg>-budat <= lv_online_time
             AND ls_ska1-xbilk = abap_true.
            " do nothing...过滤掉上线的期初数据
          ELSE.
            IF <fs_bseg>-shkzg EQ 'S'.
              APPEND <fs_bseg> TO lt_bseg.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM line_item_display USING lt_bseg.

    WHEN 'DMBTR_CREDIT_CUM_YEAR'. " 本年累计贷方金额
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).

        IF ls_t093c-bukrs <> <fs_bseg>-bukrs.
          READ TABLE gt_t093c INTO ls_t093c
            WITH KEY bukrs = <fs_bseg>-bukrs.
          IF sy-subrc = 0.
***            lv_period       = ls_t093c-DATUM+4(2).
***            lv_online_time  = ls_t093c-DATUM+0(4) && lv_period && '01'.
***            call function 'RP_LAST_DAY_OF_MONTHS'
***              exporting
***                day_in            = lv_online_time
***              importing
***                last_day_of_month = lv_online_time
****     EXCEPTIONS
****               DAY_IN_NO_DATE    = 1
****               OTHERS            = 2
***              .
***            if sy-subrc <> 0.
**** Implement suitable error handling here
***            endif.
            lv_online_time = ls_t093c-datum.
          ENDIF.
        ENDIF.

        IF gv_budat_from_year <= <fs_bseg>-budat AND <fs_bseg>-budat <= gv_budat_to_year.
          IF <fs_bseg>-budat <= lv_online_time
             AND ls_ska1-xbilk = abap_true.
            " do nothing...过滤掉上线的期初数据
          ELSE.
            IF <fs_bseg>-shkzg EQ 'H'.
              APPEND <fs_bseg> TO lt_bseg.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM line_item_display USING lt_bseg.

    WHEN 'WRBTR_CREDIT_CUM_YEAR'. " 本年累计贷方凭证货币金额
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).

        IF ls_t093c-bukrs <> <fs_bseg>-bukrs.
          READ TABLE gt_t093c INTO ls_t093c
            WITH KEY bukrs = <fs_bseg>-bukrs.
          IF sy-subrc = 0.
***            lv_period       = ls_t093c-DATUM+4(2).
***            lv_online_time  = ls_t093c-DATUM+0(4) && lv_period && '01'.
***            call function 'RP_LAST_DAY_OF_MONTHS'
***              exporting
***                day_in            = lv_online_time
***              importing
***                last_day_of_month = lv_online_time
****     EXCEPTIONS
****               DAY_IN_NO_DATE    = 1
****               OTHERS            = 2
***              .
***            if sy-subrc <> 0.
**** Implement suitable error handling here
***            endif.
            lv_online_time = ls_t093c-datum.
          ENDIF.
        ENDIF.

        IF gv_budat_from_year <= <fs_bseg>-budat AND <fs_bseg>-budat <= gv_budat_to_year.
          IF <fs_bseg>-budat <= lv_online_time
             AND ls_ska1-xbilk = abap_true.
            " do nothing...过滤掉上线的期初数据
          ELSE.
            IF <fs_bseg>-shkzg EQ 'H'.
              APPEND <fs_bseg> TO lt_bseg.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM line_item_display USING lt_bseg.

    WHEN 'MENGE_CREDIT_CUM_YEAR'. " 本年累计贷方数量
      CLEAR lt_bseg.
      LOOP AT gt_bseg ASSIGNING <fs_bseg> WHERE (lt_condition).

        IF ls_t093c-bukrs <> <fs_bseg>-bukrs.
          READ TABLE gt_t093c INTO ls_t093c
            WITH KEY bukrs = <fs_bseg>-bukrs.
          IF sy-subrc = 0.
***            lv_period       = ls_t093c-DATUM+4(2).
***            lv_online_time  = ls_t093c-DATUM+0(4) && lv_period && '01'.
***            call function 'RP_LAST_DAY_OF_MONTHS'
***              exporting
***                day_in            = lv_online_time
***              importing
***                last_day_of_month = lv_online_time
****     EXCEPTIONS
****               DAY_IN_NO_DATE    = 1
****               OTHERS            = 2
***              .
***            if sy-subrc <> 0.
**** Implement suitable error handling here
***            endif.
            lv_online_time = ls_t093c-datum.
          ENDIF.
        ENDIF.

        IF gv_budat_from_year <= <fs_bseg>-budat AND <fs_bseg>-budat <= gv_budat_to_year.
          IF <fs_bseg>-budat <= lv_online_time
             AND ls_ska1-xbilk = abap_true.
            " do nothing...过滤掉上线的期初数据
          ELSE.
            IF <fs_bseg>-shkzg EQ 'H'.
              APPEND <fs_bseg> TO lt_bseg.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM line_item_display USING lt_bseg.

    WHEN OTHERS.
      " do nothing...
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM alv_top_of_page .


  DATA: lt_list_commentary TYPE slis_t_listheader,
        ls_line            TYPE slis_listheader,
        l_lin              TYPE i,
        l_char(10)         TYPE c.

  DATA: lv_butxt  TYPE t001-butxt.


  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = '期    间：'.
  IF p_tsqj = 'X'.
    ls_line-info = p_gjahr && '特殊区间'.
  ELSE.
    IF s_monat-high IS INITIAL.
      ls_line-info = p_gjahr && s_monat-low.
    ELSE.
      ls_line-info = p_gjahr && s_monat-low && ' - ' && p_gjahr && s_monat-high.
    ENDIF.
  ENDIF.
  APPEND ls_line TO lt_list_commentary.

  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = '币    种：'.
  IF p_hwaer EQ 'X'.
    ls_line-info = '本位币'.
  ELSEIF p_waers EQ 'X'.
    ls_line-info = '凭证货币'.
  ELSE.
    " do nothing...
  ENDIF.
  APPEND ls_line TO lt_list_commentary.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_list_commentary
*     i_logo             = 'ENJOYSAP_LOGO'
      i_end_of_list_grid = space
      i_alv_form         = 'X'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  alv_handle_data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_handle_data_changed USING er_data_changed TYPE REF TO cl_alv_changed_data_protocol
                                   e_ucomm          TYPE syucomm.

  IF 1 = 1.

  ENDIF.

ENDFORM. "alv_handle_data_changed

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM alv_grid_title CHANGING ch_alv_grid_title TYPE lvc_title.

  DATA: lv_lines        TYPE i,
        lv_lines_string TYPE string.

*  if gt_output is not initial.
  IF <fs_output_table> IS NOT INITIAL.
*    lv_lines = lines( gt_output ).
    lv_lines = lines( <fs_output_table> ).
  ENDIF.

  lv_lines_string = lv_lines.

  ch_alv_grid_title = '条目数:' && lv_lines_string.
  CONDENSE ch_alv_grid_title.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  alv_event_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_event_build.

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

ENDFORM. "alv_event_build

*&---------------------------------------------------------------------*
*&      Form  alv_caller_exit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_caller_exit USING is_caller_exit TYPE slis_data_caller_exit.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     IR_SALV_FULLSCREEN_ADAPTER       =
    IMPORTING
*     ET_EXCLUDING                     =
*     E_REPID                          =
*     E_CALLBACK_PROGRAM               =
*     E_CALLBACK_ROUTINE               =
      e_grid = go_alv_grid
*     ET_FIELDCAT_LVC                  =
*     ER_TRACE                         =
*     E_FLG_NO_HTML                    =
*     ES_LAYOUT_KKBLO                  =
*     ES_SEL_HIDE                      =
*     ET_EVENT_EXIT                    =
*     ER_FORM_TOL                      =
*     ER_FORM_EOL                      =
    .



ENDFORM. "alv_caller_exit

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM create_dynamic_table .
  DATA: lt_fieldcat              TYPE lvc_t_fcat,
        lt_fieldcat_period_block TYPE lvc_t_fcat.

*" Build field catalog dynamically
  PERFORM build_fieldcat_dynamically        CHANGING lt_fieldcat.
  PERFORM build_fieldcat_period_dynamic     CHANGING lt_fieldcat_period_block.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
*     i_style_table   =
      it_fieldcatalog = lt_fieldcat
*     i_length_in_byte          =
    IMPORTING
      ep_table        = gr_dynamic_table
*     e_style_fname   =
*    exceptions
*     generate_subpool_dir_full = 1
*     others          = 2
    .
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

*" ALV output table
  IF <fs_output_table> IS NOT ASSIGNED.
    ASSIGN gr_dynamic_table->* TO <fs_output_table>.
  ENDIF.
  CREATE DATA gr_dynamic_line LIKE LINE OF <fs_output_table>.

*" ALV Work area
  IF <fs_output_line> IS NOT ASSIGNED.
    ASSIGN gr_dynamic_line->* TO <fs_output_line>.
  ENDIF.

*" 计算余额所使用的中间变量
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
*     i_style_table   =
      it_fieldcatalog = lt_fieldcat_period_block
*     i_length_in_byte          =
    IMPORTING
      ep_table        = gr_dynamic_period_block_table
*     e_style_fname   =
*    exceptions
*     generate_subpool_dir_full = 1
*     others          = 2
    .
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

*" Dynamic Table , Peroid 余额计算
  IF <fs_period_block_table> IS NOT ASSIGNED.
    ASSIGN gr_dynamic_period_block_table->* TO <fs_period_block_table>.
  ENDIF.
  CREATE DATA gr_dynamic_period_block_line LIKE LINE OF <fs_period_block_table>.

*" Work Area, 余额计算
  IF <fs_period_block_line> IS NOT ASSIGNED.
    ASSIGN gr_dynamic_period_block_line->* TO <fs_period_block_line>.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM build_posting_data_range .
  DATA: lv_budat_from TYPE sy-datum,
        lv_budat_to   TYPE sy-datum.

  CLEAR: gv_budat_from  ,
         gv_budat_to    ,
         gv_period_from ,
         gv_period_to   .

  CLEAR: gv_budat_from_year ,
         gv_budat_to_year   ,
         gv_period_from_year,
         gv_period_to_year  .


*" from date

  lv_budat_from+0(4)  = p_gjahr.
*  lv_budat_from+4(2)  = p_monat.


  lv_budat_from+4(2)  = s_monat-low.


  lv_budat_from+6(2)  = '01'.
*" Posting date->begin date

*" to date
  lv_budat_to+0(4)    = p_gjahr.
*  lv_budat_to+4(2)    = p_monat.

  IF s_monat-high IS INITIAL.
    lv_budat_to+4(2)    = s_monat-low.
  ELSE.
    lv_budat_to+4(2)    = s_monat-high.

  ENDIF.
  "2019-05-17 by sw
  IF  lv_budat_to+4(2) > 12.
    lv_budat_to+4(2) = '12'.
  ENDIF.

  lv_budat_to+6(2)    = '01'.




  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_budat_to
    IMPORTING
      last_day_of_month = lv_budat_to
*   EXCEPTIONS
*     DAY_IN_NO_DATE    = 1
*     OTHERS            = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  gv_budat_from   = lv_budat_from.
  gv_budat_to     = lv_budat_to.
  gv_period_from  = gv_budat_from+4(2).
  gv_period_to    = gv_budat_to+4(2).

  gv_budat_from_year  = p_gjahr && '01' && '01'.
  gv_budat_to_year    = gv_budat_to.
  gv_period_from_year = gv_budat_from_year+4(2).
  gv_period_to_year   = gv_budat_to_year+4(2).

ENDFORM.

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM build_fieldcat_dynamically CHANGING ct_fieldcat TYPE lvc_t_fcat.

  DATA: ls_fieldcat        TYPE lvc_s_fcat,
        lt_fieldcat_common TYPE lvc_t_fcat.

  DATA: lv_structure_name TYPE dd02l-tabname.

  CLEAR: ct_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'RBUKRS'.
  ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
  ls_fieldcat-ref_field = 'RBUKRS'.
  ls_fieldcat-key       = abap_true.
  APPEND ls_fieldcat TO ct_fieldcat.
  APPEND ls_fieldcat TO gt_fieldcat_dynamic_cond.
*add end.

*" 总账科目
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'RACCT'.
  ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
  ls_fieldcat-ref_field = 'RACCT'.
  ls_fieldcat-key       = abap_true.
  APPEND ls_fieldcat TO ct_fieldcat.
  APPEND ls_fieldcat TO gt_fieldcat_dynamic_cond.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'RACCT_TEXT'.
  ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
  ls_fieldcat-ref_field = 'RACCT_TEXT'.
  ls_fieldcat-no_out    = abap_true.  " 文本，默认不显示
  APPEND ls_fieldcat TO ct_fieldcat.

*" 成本中心
  IF p_kostl EQ 'X'.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'RCNTR'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'RCNTR'.
    APPEND ls_fieldcat TO ct_fieldcat.
    APPEND ls_fieldcat TO gt_fieldcat_dynamic_cond.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'RCNTR_TEXT'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'RCNTR_TEXT'.
    ls_fieldcat-no_out    = abap_true.
    ls_fieldcat-scrtext_l = '成本中心名称'.
    ls_fieldcat-scrtext_m = '成本中心名称'.
    ls_fieldcat-scrtext_s = '成本中心名称'.
    APPEND ls_fieldcat TO ct_fieldcat.
  ENDIF.

*" 利润中心组 & 利润中心
  IF p_prctr EQ 'X' .
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'PRCTR'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'PRCTR'.
    APPEND ls_fieldcat TO ct_fieldcat.
    APPEND ls_fieldcat TO gt_fieldcat_dynamic_cond.

*" 文本
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'PRCTR_TEXT'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'PRCTR_TEXT'.
    ls_fieldcat-scrtext_l = '利润中心名称'.
    ls_fieldcat-scrtext_m = '利润中心名称'.
    ls_fieldcat-scrtext_s = '利润中心名称'.
    ls_fieldcat-no_out    = abap_true.
    APPEND ls_fieldcat TO ct_fieldcat.

  ENDIF.

*" 业务范围
  IF p_gsber EQ 'X'.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'RBUSA'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'RBUSA'.
    APPEND ls_fieldcat TO ct_fieldcat.
    APPEND ls_fieldcat TO gt_fieldcat_dynamic_cond.

*" 文本
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'RBUSA_TEXT'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'RBUSA_TEXT'.
    ls_fieldcat-scrtext_l = '业务范围名称'.
    ls_fieldcat-scrtext_m = '业务范围名称'.
    ls_fieldcat-scrtext_s = '业务范围名称'.
    ls_fieldcat-no_out    = abap_true.
    APPEND ls_fieldcat TO ct_fieldcat.

  ENDIF.

*" 功能范围
  IF p_rfarea EQ 'X'.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'RFAREA'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'RFAREA'.
    APPEND ls_fieldcat TO ct_fieldcat.
    APPEND ls_fieldcat TO gt_fieldcat_dynamic_cond.

*" 文本
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'RFAREA_TEXT'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'RFAREA_TEXT'.
    ls_fieldcat-no_out    = abap_true.
    APPEND ls_fieldcat TO ct_fieldcat.

  ENDIF.

  IF p_lvl1 EQ 'X' OR
      p_sum1 EQ 'X'.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'ERGSL'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'ERGSL'.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'ERGSL_TEXT'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'ERGSL_TEXT'.
    APPEND ls_fieldcat TO ct_fieldcat.
  ENDIF.

*" if no fields is selected, then...
  IF ct_fieldcat IS INITIAL.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'RACCT'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'RACCT'.
    APPEND ls_fieldcat TO ct_fieldcat.
    APPEND ls_fieldcat TO gt_fieldcat_dynamic_cond.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'RACCT_TEXT'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'RACCT_TEXT'.
    ls_fieldcat-no_out    = abap_true.
    APPEND ls_fieldcat TO ct_fieldcat.

  ENDIF.

*" Add common fields
  IF p_waers EQ 'X'.      " 原币
    lv_structure_name = 'ZSFIRPT_003_ALV_COMMON'.

*" 凭证货币会用来作为双击时的 条件
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'WAERS'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV_COMMON'.
    ls_fieldcat-ref_field = 'WAERS'.
    APPEND ls_fieldcat TO gt_fieldcat_dynamic_cond.

  ELSEIF p_hwaer EQ 'X'.  " 本位币
    lv_structure_name = 'ZSFIRPT_003_ALV_COMMON_02'.
  ENDIF.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE    =
      i_structure_name   = lv_structure_name
*     I_CLIENT_NEVER_DISPLAY       = 'X'
      i_bypassing_buffer = 'X'
*     I_INTERNAL_TABNAME =
    CHANGING
      ct_fieldcat        = lt_fieldcat_common
*   EXCEPTIONS
*     INCONSISTENT_INTERFACE       = 1
*     PROGRAM_ERROR      = 2
*     OTHERS             = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  APPEND LINES OF lt_fieldcat_common TO ct_fieldcat.

*" Final Process
  IF p_sum1 EQ 'X'.
    LOOP AT ct_fieldcat INTO ls_fieldcat.
      CASE ls_fieldcat-fieldname.
        WHEN 'RACCT'.
          ls_fieldcat-no_out  = abap_true.
        WHEN 'ERGSL'.
          ls_fieldcat-key     = abap_true.
        WHEN OTHERS.
          " do nothing...
      ENDCASE.
      MODIFY ct_fieldcat FROM ls_fieldcat.
    ENDLOOP.
  ENDIF.

*" ALV Fieldcat
  gt_fieldcat = ct_fieldcat.

ENDFORM.

FORM build_fieldcat_period_dynamic CHANGING ct_fieldcat TYPE lvc_t_fcat.

  DATA: ls_fieldcat        TYPE lvc_s_fcat,
        lt_fieldcat_common TYPE lvc_t_fcat.

  CLEAR: ct_fieldcat.
*  clear: gt_fieldcat_dynamic_cond.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'RBUKRS'.
  ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
  ls_fieldcat-ref_field = 'RBUKRS'.
  ls_fieldcat-key       = abap_true.
  APPEND ls_fieldcat TO ct_fieldcat.
  APPEND ls_fieldcat TO gt_fieldcat_dynamic_cond.

*" 总账科目
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'RACCT'.
  ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
  ls_fieldcat-ref_field = 'RACCT'.
  APPEND ls_fieldcat TO ct_fieldcat.

*" 成本中心
  IF p_kostl EQ 'X'.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'RCNTR'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'RCNTR'.
    APPEND ls_fieldcat TO ct_fieldcat.
  ENDIF.

*" 利润中心组 & 利润中心
  IF p_prctr EQ 'X' .
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'PRCTR'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'PRCTR'.
    APPEND ls_fieldcat TO ct_fieldcat.
  ENDIF.

*" 业务范围
  IF p_gsber EQ 'X'.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'RBUSA'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'RBUSA'.
    APPEND ls_fieldcat TO ct_fieldcat.
  ENDIF.

*" 功能范围
  IF p_rfarea EQ 'X'.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'RFAREA'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'RFAREA'.
    APPEND ls_fieldcat TO ct_fieldcat.
  ENDIF.

*" 判断是原币还是本币
  IF p_waers EQ abap_true.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'RTCUR'.
    ls_fieldcat-ref_table = 'FAGLFLEXT'.
    ls_fieldcat-ref_field = 'RTCUR'.
    APPEND ls_fieldcat TO ct_fieldcat.
  ENDIF.

*" if no fields is selected, then...
  IF ct_fieldcat IS INITIAL.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'RACCT'.
    ls_fieldcat-ref_table = 'ZSFIRPT_003_ALV'.
    ls_fieldcat-ref_field = 'RACCT'.
    APPEND ls_fieldcat TO ct_fieldcat.
  ENDIF.

*" Add common fields
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE  =
      i_structure_name = 'FAGL_S_PERIOD_BLOCK'
*     I_CLIENT_NEVER_DISPLAY       = 'X'
*     I_BYPASSING_BUFFER           =
*     I_INTERNAL_TABNAME           =
    CHANGING
      ct_fieldcat      = lt_fieldcat_common
*   EXCEPTIONS
*     INCONSISTENT_INTERFACE       = 1
*     PROGRAM_ERROR    = 2
*     OTHERS           = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  APPEND LINES OF lt_fieldcat_common TO ct_fieldcat.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  call_line_item_report
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM call_line_item_report
                USING gt_balance TYPE fdbl_balance
                      ud_grid TYPE REF TO cl_gui_alv_grid.
  .

*" COPY FAGLB03的双击

*...get selections.....................................................*
  DATA           lt_cosel          TYPE tcosel.
  DATA           ls_cosel          TYPE cosel.
  DATA           ld_bukrs          TYPE bukrs.
  DATA           ld_grid           TYPE REF TO cl_gui_alv_grid.
  DATA: lt_cosel_original TYPE tcosel,
        ls_cosel_original TYPE cosel.
  DATA           l_length          TYPE i.                  "1055357
  DATA           l_empty(8)        TYPE c.                  "1055357



  LOOP AT lt_cosel_original INTO ls_cosel_original.         "911129
    READ TABLE lt_cosel INTO ls_cosel
    WITH KEY field = ls_cosel_original-field
    low = '*'.                                              "911129
    CHECK sy-subrc = 0.                                     "911129
    APPEND ls_cosel_original TO lt_cosel.                   "911129
  ENDLOOP.                                                  "911129
  LOOP AT lt_cosel_original INTO ls_cosel_original.         "911129
    LOOP AT lt_cosel
    INTO ls_cosel
    WHERE field = ls_cosel_original-field.                  "911129
      IF ls_cosel-low = '*'.                                "911129
        DELETE lt_cosel INDEX sy-tabix..                    "911129
      ENDIF.                                                "911129
    ENDLOOP.                                                "911129
  ENDLOOP.                                                  "911129


  DATA: ld_row    TYPE i,
        ld_col_id TYPE lvc_s_col.



  CALL METHOD cl_gui_cfw=>flush.

*...get grid type......................................................*
  DATA         ld_grid_type TYPE c.




*...fill parameters for the rep rep interface.........................*
  DATA lt_sel    TYPE TABLE OF rstisel.
  DATA lt_fields TYPE TABLE OF rstifields.
  DATA ls_sel TYPE rstisel.
  DATA ls_fields TYPE rstifields.

  DATA ls_tables TYPE fagl_tabnames.
  DATA ls_dfies TYPE dfies.
  DATA ld_curr_sel1 TYPE cosel-low.                         "1007620
  DATA ld_curr_sel2 TYPE cosel-low.                         "1007620

  DATA ls_balance TYPE fdbl_balance_line.                   "801568
  DATA lt_bukrs_hwaer TYPE fagl_t_bukrs_hwaer.              "991987
  DATA ls_bukrs_hwaer TYPE fagl_s_bukrs_hwaer.              "991987


  READ TABLE gt_balance INTO ls_balance
                   INDEX ld_row.                            "801568


*...get period and fiscal year.........................................*

*  perform get_budat tables lt_cosel
*                           lt_sel
*                    using  gt_balance
*                           'S'               "LD_KOART
*                           ld_row
*                           ld_col_id
*                           ld_grid_type.
**                          PD_CONTROLER
**                          LT_FREE_SELECTIONS.

*...get display variant................................................*

*  perform get_display_variant tables lt_sel
*                                     lt_cosel
*                              using  'S'.           "LD_KOART
**                                    PD_CONTROLER.

*...set indicator for all items/only open items........................*

  PERFORM get_all_or_open IN PROGRAM saplfagl_account_balance TABLES lt_sel
                                 lt_cosel
                          USING  'S'                "LD_KOART
                                 ld_col_id.


*...set flags to get correct kind of line items........................*

*  perform set_item_type tables lt_sel.

*...Only the Company Codes are needed related to the CURTP....."991987.*

  READ TABLE lt_cosel INTO ls_cosel WITH KEY field = 'CURTP'.
  IF NOT ls_cosel-low = '10' AND NOT ls_cosel-low = '00'.   "1095192
*    call method gi_controler->get_curr_data
*      importing
*        it_bukrs_hwaer = lt_bukrs_hwaer.
*    loop at lt_cosel into ls_cosel where field = 'RBUKRS'.
*      read table lt_bukrs_hwaer into ls_bukrs_hwaer
*                      with key bukrs = ls_cosel-low.
*      if not ls_bukrs_hwaer-curtp_group  = gd_dd_currtype   "1112980
*         and not ls_bukrs_hwaer-curtp_fourth = gd_dd_currtype. "1112980
*        delete lt_cosel.
*
*      endif.
*    endloop.
  ENDIF.

  DATA: lr_bukrs            TYPE RANGE OF bukrs WITH HEADER LINE, "1103171
        lr_saknr            TYPE RANGE OF saknr WITH HEADER LINE, "1103171
        lt_xsalh            TYPE STANDARD TABLE OF xsalh WITH HEADER LINE, "1103171
        ld_no_curr_restrict TYPE boolean.                   "1103171

  LOOP AT lt_cosel INTO ls_cosel WHERE field EQ 'RBUKRS' OR "1103171
                                       field EQ 'RACCT'.    "1103171
    IF ls_cosel-field EQ 'RACCT'.                           "1103171
      MOVE-CORRESPONDING ls_cosel TO lr_saknr.              "1103171
      APPEND lr_saknr.                                      "1103171
    ELSE.                                                   "1103171
      MOVE-CORRESPONDING ls_cosel TO lr_bukrs.              "1103171
      APPEND lr_bukrs.                                      "1103171
    ENDIF.                                                  "1103171
  ENDLOOP.                                                  "1103171
  SELECT DISTINCT xsalh FROM skb1 INTO TABLE lt_xsalh       "1103171
                  WHERE saknr IN lr_saknr AND               "1103171
                        bukrs IN lr_bukrs.                  "1103171
  READ TABLE lt_xsalh WITH KEY = space.                     "1456278
  IF sy-subrc NE 0.                                         "1456278
    ld_no_curr_restrict = 'X'.                              "1456278
  ENDIF.                                                    "1456278
  IF ld_no_curr_restrict IS INITIAL.                        "1103171
*..Begin of setting the selected document currency............."1007620*
    READ TABLE lt_cosel INTO ls_cosel WITH KEY field = 'RTCUR'.
    IF sy-subrc = 0.
      ld_curr_sel1 = ls_cosel-low.
      READ TABLE lt_cosel INTO ls_cosel WITH KEY field = 'WAERS'.
      IF sy-subrc = 0.
        ld_curr_sel2 = ls_cosel-low.
        IF ld_curr_sel1 = ld_curr_sel2.
          CLEAR ls_sel.
          ls_sel-field = 'SO_WAERS'.                        "1311254
          ls_sel-sign = 'I'.
          ls_sel-option = 'EQ'.
          ls_sel-low = ld_curr_sel2.
          APPEND ls_sel TO lt_sel.
        ELSE.                                               "1095192
          READ TABLE lt_cosel INTO ls_cosel                 "1095192
                                  WITH KEY field = 'CURTP'. "1095192
          IF NOT ls_cosel-low = '00'.                       "1095192
            CLEAR ls_sel.                                   "1095192
            ls_sel-field = 'SO_WAERS'.                      "1311254
            ls_sel-sign = 'I'.                              "1095192
            ls_sel-option = 'EQ'.                           "1095192
            ls_sel-low = ld_curr_sel1.                      "1095192
            APPEND ls_sel TO lt_sel.                        "1095192
          ENDIF.                                            "1095192
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.                                                    "1103171
*..End of setting the selected document currency..............."1007620*

  DATA: ls_fagl_tabnames TYPE fagl_tabnames,
        fielddef         TYPE REF TO data,
        wa_name(100)     TYPE c.
  FIELD-SYMBOLS: <fs_field> TYPE any.

  CALL FUNCTION 'FAGL_GET_TABLENAMES'
    EXPORTING
      i_ledger            = '0L'
    IMPORTING
      es_tabnames         = ls_fagl_tabnames
    EXCEPTIONS
      not_found           = 1
      configuration_error = 2
      OTHERS              = 3.

  LOOP AT lt_cosel INTO ls_cosel.
    CHECK ls_cosel-field NE 'CURTP'.
    CHECK ls_cosel-field NE 'WAERS'.
    CHECK ls_cosel-field NE 'RTCUR'.
    CHECK ls_cosel-field NE 'RYEAR'.
    CHECK ls_cosel-field NE 'X_GLYEC'.                      "1830344

    MOVE-CORRESPONDING ls_cosel TO ls_sel.

    DATA: lt_text TYPE TABLE OF textpool,                   "1324633
          wa_text TYPE textpool.                            "1324633
    READ TEXTPOOL 'FAGL_ACCOUNT_BALANCE' INTO lt_text       "1324633
                                     LANGUAGE sy-langu.     "1324633
    READ TABLE lt_text WITH KEY key = '010'                 "1324633
                                     INTO wa_text.          "1324633
    CHECK ls_sel-low NE '*'.                                "801568
    CONCATENATE ls_fagl_tabnames-tot_table '-' ls_sel-field "1369756
                                                 INTO wa_name. "1369756
    CREATE DATA fielddef TYPE (wa_name).                    "1369756
    ASSIGN fielddef->* TO <fs_field>.                       "1369756
    DESCRIBE FIELD <fs_field> LENGTH l_length               "1369756
                                      IN CHARACTER MODE.    "1369756
    IF l_length GT 8 OR l_length IS INITIAL.                "1129308
      l_length = 8.                                         "1055357
    ENDIF.                                                  "1055357
    l_empty = text-013.                                     "1055357
* If the filter is set for value 'empty', ls_sel-low is cleared.
    IF ( ( ls_sel-low = l_empty(l_length)                   "1055357
           OR ls_sel-low = wa_text-entry(l_length) )        "1324633
                      AND ls_sel-field NE 'RLDNR' )         "1055357
     OR ls_sel-low IS INITIAL.                              "1129308
      CLEAR ls_sel-low.                                     "801568
    ENDIF.                                                  "801568
    CLEAR: l_empty, l_length.                               "1055357

    APPEND ls_sel TO lt_sel.

    CALL FUNCTION 'FAGL_GET_TABLENAMES'
      EXPORTING
        i_ledger            = '0L'
      IMPORTING
        es_tabnames         = ls_tables
      EXCEPTIONS
        not_found           = 1
        configuration_error = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    CALL FUNCTION 'RKC_DFIES_GET'
      EXPORTING
        i_tabnm              = ls_tables-tot_table
        i_fienm              = ls_cosel-field
      IMPORTING
        e_s_dfies            = ls_dfies
      EXCEPTIONS
        no_texts_found       = 1
        no_information_found = 2
        OTHERS               = 3.

    ls_fields-field = ls_cosel-field.
    ls_fields-rfield = ls_cosel-field.
    ls_fields-kind = 'S'.
    ls_fields-rollname = ls_dfies-rollname.
    ls_fields-domname = ls_dfies-domname.
    APPEND ls_fields TO lt_fields.

  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM lt_fields.                "779743
  DELETE ADJACENT DUPLICATES FROM lt_sel.

*...add selections for row and column.

*  DATA ls_balance TYPE fdbl_balance_line.                    "801568
*
*  READ TABLE gt_balance INTO ls_balance
*                   INDEX ld_row.                             "801568

  IF ld_col_id ='PERIOD'
  AND ls_balance-period = text-014.
    MESSAGE e010(fdbl).
  ENDIF.

  IF ld_col_id = 'DEBIT'.

    ls_fields-field = 'SO_SHKZG'.
    ls_fields-rfield = 'SO_SHKZG'.
    ls_fields-kind = 'S'.
    ls_fields-rollname = 'SHKZG'.
    ls_fields-domname = 'SHKZG'.
    APPEND ls_fields TO lt_fields.

    CLEAR ls_sel.
    ls_sel-field = 'SO_SHKZG'.
    ls_sel-sign = 'I'.
    ls_sel-option = 'EQ'.
    ls_sel-low = 'S'.
    APPEND ls_sel TO lt_sel.

  ENDIF.

  IF ld_col_id = 'CREDIT'.

    ls_fields-field = 'SO_SHKZG'.
    ls_fields-rfield = 'SO_SHKZG'.
    ls_fields-kind = 'S'.
    ls_fields-rollname = 'SHKZG'.
    ls_fields-domname = 'SHKZG'.
    APPEND ls_fields TO lt_fields.

    CLEAR ls_sel.
    ls_sel-field = 'SO_SHKZG'.
    ls_sel-sign = 'I'.
    ls_sel-option = 'EQ'.
    ls_sel-low = 'H'.
    APPEND ls_sel TO lt_sel.

  ENDIF.

  CLEAR ls_fields.

  DATA: ls_fieldr TYPE rstifields,
        lt_fieldr TYPE TABLE OF rstifields.
  DATA: ld_buk_count    TYPE i.                             "991987

  LOOP AT lt_cosel INTO ls_cosel WHERE field = 'RBUKRS'.    "991987
    ld_buk_count = ld_buk_count + 1.                        "991987
  ENDLOOP.                                                  "991987
  IF ld_buk_count = 0.                                      "991987
    CLEAR lt_bukrs_hwaer.                                   "1104517
*    call method gi_controler->get_curr_data                    "1104517
*      importing                                                "1104517
*        it_bukrs_hwaer = lt_bukrs_hwaer.                       "1104517
    LOOP AT lt_bukrs_hwaer INTO ls_bukrs_hwaer.             "1104517
      ls_sel-field = 'RBUKRS'.                              "1104517
      ls_sel-sign = 'I'.                                    "1104517
      ls_sel-option = 'EQ'.                                 "1104517
      ls_sel-low = ls_bukrs_hwaer-bukrs.                    "1104517
      CLEAR ls_sel-high.                                    "1104517
      APPEND ls_sel TO lt_sel.                              "1104517
    ENDLOOP.                                                "1104517
  ENDIF.                                                    "991987

  LOOP AT lt_cosel INTO ls_cosel WHERE field = 'X_GLYEC'.   "1830344
    ls_sel-field  = 'X_GLYEC'.                              "1830344
    ls_sel-sign   = 'I'.                                    "1830344
    ls_sel-option = 'EQ'.                                   "1830344
    ls_sel-low    = 'X'.                                    "1830344
    CLEAR ls_sel-high.                                      "1830344
    APPEND ls_sel TO lt_sel.                                "1830344
  ENDLOOP.                                                  "1830344

  CALL FUNCTION 'RSTI_REPORT_FIELDS_FIND'
    EXPORTING
      e_repid   = 'FAGL_ACCOUNT_ITEMS_GL'
      e_type    = 'R'
    TABLES
      it_fields = lt_fieldr.

  LOOP AT lt_fieldr INTO ls_fieldr.
    LOOP AT lt_fields INTO ls_fields
      WHERE rfield = ls_fieldr-rfield.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      LOOP AT lt_sel INTO ls_sel
        WHERE field = ls_fieldr-rfield.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        ls_fieldr-field = ls_fieldr-rfield.
        APPEND ls_fieldr TO lt_fields.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'RSTI_APPL_STACK_INITIALIZE'
    EXPORTING
      e_tool = 'RT'
      e_onam = 'FAGL_ACCOUNT_BALANCE'.


  CALL FUNCTION 'RSTI_SELECTION_EXPORT'
    TABLES
      it_sel    = lt_sel
      it_fields = lt_fields.


  CALL FUNCTION 'RSTI_COMMUNICATION_HANDLER'
    EXPORTING
      e_fccls = ' '.

ENDFORM. " call_line_item_report

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM line_item_get .

  TYPES: BEGIN OF ty_bkpf,
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
  DATA:     lt_bseg TYPE STANDARD TABLE OF ZSFIRPT_003_ALV_item.
  DATA: ls_bseg TYPE  ZSFIRPT_003_ALV_item.
  DATA: lt_bseg1 TYPE STANDARD TABLE OF ZSFIRPT_003_ALV_item.
  DATA: ls_bseg1 TYPE  ZSFIRPT_003_ALV_item.
  DATA: lt_bseg_tmp TYPE STANDARD TABLE OF ZSFIRPT_003_ALV_item.
  DATA: lt_bkpf TYPE TABLE OF ty_bkpf,
        ls_bkpf TYPE ty_bkpf.
  FIELD-SYMBOLS: <fs_bseg> TYPE LINE OF tt_bseg.
  REFRESH gt_bseg.
*  check gt_bseg is initial. " 第一次读取了数据即可...,以免双击时每次都查询

  IF p_tsqj  = 'X'.
    SELECT bukrs
           gjahr
           belnr
           monat
           xreversal
           bldat
           budat
           bstat
           waers
           hwaer
           bktxt
    INTO TABLE lt_bkpf
    FROM bkpf
    WHERE bukrs IN g_bukrs
      AND monat IN ('13' , '14' , '15', '16'  )
      AND bstat NE 'M'
      AND budat <= gv_budat_to.

    IF lt_bkpf IS NOT INITIAL.
      SELECT bukrs
             gjahr
             belnr
             bschl
             shkzg
             xnegp
             hkont
             prctr
             kostl
             koart
             umskz
             lifnr
             kunnr
             gsber
             fkber
             dmbtr
             wrbtr
             matnr
             menge
             meins
             zfbdt
             zuonr
             pswsl
             sgtxt
             vptnr
       INTO CORRESPONDING FIELDS OF TABLE gt_bseg
       FROM bseg FOR ALL ENTRIES IN lt_bkpf
       WHERE bukrs = lt_bkpf-bukrs
         AND gjahr = lt_bkpf-gjahr
         AND belnr = lt_bkpf-belnr
         AND hkont IN s_hkont
         AND kostl IN s_kostl
         AND prctr IN s_prctr.
    ENDIF.

    LOOP AT gt_bseg ASSIGNING <fs_bseg> .
      READ TABLE lt_bkpf INTO ls_bkpf WITH KEY bukrs = <fs_bseg>-bukrs
                                               gjahr = <fs_bseg>-gjahr
                                               belnr = <fs_bseg>-belnr.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_bkpf TO <fs_bseg>.
      ENDIF.
      IF <fs_bseg>-shkzg = 'H'.
        <fs_bseg>-dmbtr = - <fs_bseg>-dmbtr.
        <fs_bseg>-wrbtr = - <fs_bseg>-wrbtr.
        <fs_bseg>-menge = - <fs_bseg>-menge.
      ENDIF.
    ENDLOOP.

*    SELECT *
*      FROM zv_fi003( gv_budat_to = @gv_budat_to )
*    INTO CORRESPONDING FIELDS OF TABLE @gt_bseg
*    WHERE bukrs  IN @g_bukrs
*          AND hkont  IN @g_hkont
*          AND kostl  IN @s_kostl
*          AND prctr  IN @s_prctr
*          AND monat  IN ('13' , '14' , '15', '16'  )
*          .
*    SELECT *
*       FROM zv_fi003_01( gv_budat_to = @gv_budat_to )
*     APPENDING CORRESPONDING FIELDS OF TABLE @lt_bseg_tmp "@gt_bseg 修改于19-3-29 替换为@lt_bseg_tmp
*     WHERE  bukrs  IN @g_bukrs
**        and bkpf~gjahr  = @p_gjahr
**        and bkpf~monat  = @p_monat
**       AND  budat  <= @gv_budat_to
*         AND  hkont  IN @g_hkont
*         AND  kostl  IN @s_kostl
*         AND  prctr  IN @s_prctr
*         AND monat   IN ('13' , '14' , '15', '16'  )


  ELSE.
*    SELECT *
*    FROM zv_fi003( gv_budat_to = @gv_budat_to )
*  INTO CORRESPONDING FIELDS OF TABLE @gt_bseg
*  WHERE bukrs  IN @g_bukrs
*        AND hkont  IN @g_hkont
*        AND kostl  IN @s_kostl
*        AND prctr  IN @s_prctr
*        AND monat NOT IN ('13' , '14' , '15', '16'  )
*        .
*    SELECT *
* FROM zv_fi003_01( gv_budat_to = @gv_budat_to )
*APPENDING CORRESPONDING FIELDS OF TABLE @lt_bseg_tmp "@gt_bseg 修改于19-3-29 替换为@lt_bseg_tmp
*WHERE  bukrs  IN @g_bukrs
**        and bkpf~gjahr  = @p_gjahr
**        and bkpf~monat  = @p_monat
**       AND  budat  <= @gv_budat_to
*   AND  hkont  IN @g_hkont
*   AND  kostl  IN @s_kostl
*   AND  prctr  IN @s_prctr
*   AND  monat  NOT IN ('13' , '14' , '15', '16'  )
*     .
    SELECT bukrs
       gjahr
       belnr
       monat
       xreversal
       bldat
       budat
       bstat
       waers
       hwaer
       bktxt
INTO TABLE lt_bkpf
FROM bkpf
WHERE bukrs IN g_bukrs
  AND monat NOT IN ('13' , '14' , '15', '16'  )
  AND bstat NE 'M'
  AND budat <= gv_budat_to.

    IF lt_bkpf IS NOT INITIAL.
      SELECT bukrs
             gjahr
             belnr
             bschl
             shkzg
             xnegp
             hkont
             prctr
             kostl
             koart
             umskz
             lifnr
             kunnr
             gsber
             fkber
             dmbtr
             wrbtr
             matnr
             menge
             meins
             zfbdt
             zuonr
             pswsl
             sgtxt
             vptnr
       INTO CORRESPONDING FIELDS OF TABLE gt_bseg
       FROM bseg FOR ALL ENTRIES IN lt_bkpf
       WHERE bukrs = lt_bkpf-bukrs
         AND gjahr = lt_bkpf-gjahr
         AND belnr = lt_bkpf-belnr
         AND hkont IN s_hkont
         AND kostl IN s_kostl
         AND prctr IN s_prctr.
    ENDIF.

    LOOP AT gt_bseg ASSIGNING <fs_bseg> .
      READ TABLE lt_bkpf INTO ls_bkpf WITH KEY bukrs = <fs_bseg>-bukrs
                                               gjahr = <fs_bseg>-gjahr
                                               belnr = <fs_bseg>-belnr.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_bkpf TO <fs_bseg>.
      ENDIF.
      IF <fs_bseg>-shkzg = 'H'.
        <fs_bseg>-dmbtr = - <fs_bseg>-dmbtr.
        <fs_bseg>-wrbtr = - <fs_bseg>-wrbtr.
        <fs_bseg>-menge = - <fs_bseg>-menge.
      ENDIF.
    ENDLOOP.
  ENDIF.





*  SELECT
*    bkpf~bukrs,
*    bkpf~belnr,
*    bkpf~gjahr,
*    ACDOCA~buzei,
*    bkpf~xreversal,
*    bkpf~bldat,
*    bkpf~budat,
*    bkpf~bstat,
*    bkpf~waers,
*    bkpf~hwaer,
*    bkpf~bktxt,
*
*    ACDOCA~bschl,
*    ACDOCA~DRCRK as shkzg,
*****    ACDOCA~xnegp,
*    ACDOCA~RACCT as hkont,
**      ACDOCA~xbilk,
*    ACDOCA~prctr,
*    ACDOCA~RCNTR as kostl,
*    ACDOCA~koart,
*    ACDOCA~umskz,
*    ACDOCA~lifnr,
*    ACDOCA~kunnr,
*    ACDOCA~RBUSA as gsber,
**      ACDOCA~fkber,
**      ACDOCA~zz_feety,
*    ACDOCA~RFAREA AS fkber,
**      ACDOCA~aufnr,
*    ACDOCA~HSL as dmbtr,
*    ACDOCA~WSL as wrbtr,
*    ACDOCA~matnr,
*    ACDOCA~MSL as menge,
*    ACDOCA~RUNIT as meins,
*****    ACDOCA~zfbdt as zfbdt,
*    ACDOCA~zuonr,
*    ACDOCA~RWCUR as pswsl,
*    ACDOCA~sgtxt,
*    ACDOCA~vptnr
*    APPENDING CORRESPONDING FIELDS OF TABLE @lt_bseg_tmp
*    FROM bkpf
*      INNER JOIN ACDOCA ON ACDOCA~RBUKRS = bkpf~bukrs
*                     AND ACDOCA~belnr = bkpf~belnr
*                     AND ACDOCA~gjahr = bkpf~gjahr
**      WHERE BKPF~BUKRS  = @P_BUKRS
*    WHERE bkpf~bukrs  IN @g_bukrs
**        and bkpf~gjahr  = @p_gjahr
**        and bkpf~monat  = @p_monat
*      AND bkpf~budat  <= @gv_budat_to
*      AND bkpf~bstat  NE 'M'            " 排除样本凭证
*      AND ACDOCA~RACCT  IN @s_hkont
*      AND ACDOCA~RCNTR  IN @s_kostl
*      AND ACDOCA~prctr  IN @s_prctr.


  lt_bseg = gt_bseg.
  SORT lt_bseg BY belnr lifnr.
  DELETE lt_bseg WHERE lifnr IS INITIAL.

  lt_bseg1 = gt_bseg.
  SORT lt_bseg1 BY belnr kunnr.
  DELETE lt_bseg1 WHERE kunnr IS INITIAL.

  LOOP AT gt_bseg ASSIGNING <fs_bseg>.
    CLEAR ls_bseg.
*    READ TABLE lt_bseg_tmp INTO ls_bseg WITH KEY bukrs = <fs_bseg>-bukrs belnr = <fs_bseg>-belnr gjahr = <fs_bseg>-gjahr buzei = <fs_bseg>-buzei.
*    IF sy-subrc = 0.
*      DELETE gt_bseg WHERE bukrs = <fs_bseg>-bukrs AND belnr = <fs_bseg>-belnr AND gjahr = <fs_bseg>-gjahr AND buzei = <fs_bseg>-buzei.
*    ELSE.

    IF <fs_bseg>-lifnr IS INITIAL
      AND (  <fs_bseg>-hkont = '1405080000'
          OR <fs_bseg>-hkont = '1405090000'
          OR <fs_bseg>-hkont = '1402000000'

          OR <fs_bseg>-hkont = '1403010000'
          OR <fs_bseg>-hkont = '1403020000'
          OR <fs_bseg>-hkont = '1403030000'
          OR <fs_bseg>-hkont = '1403040000'
          OR <fs_bseg>-hkont = '1403050000'
          OR <fs_bseg>-hkont = '1403980000'


          ) .
      CLEAR ls_bseg.
      READ TABLE lt_bseg INTO ls_bseg WITH KEY belnr = <fs_bseg>-belnr .
      IF sy-subrc = 0.
        <fs_bseg>-lifnr = ls_bseg-lifnr.
      ENDIF.
    ENDIF.

    IF <fs_bseg>-kunnr IS INITIAL
    AND (  <fs_bseg>-hkont = '6001010000'
        OR <fs_bseg>-hkont = '6001020000'
        OR <fs_bseg>-hkont = '6001030000'
        OR <fs_bseg>-hkont = '6001040000'
        OR <fs_bseg>-hkont = '6001050000'
        OR <fs_bseg>-hkont = '6001060000'
        OR <fs_bseg>-hkont = '6001080000'
        OR <fs_bseg>-hkont = '6001100000'
        OR <fs_bseg>-hkont = '6001110000'
        OR <fs_bseg>-hkont = '6051010000'
        OR <fs_bseg>-hkont = '6051020000'
        OR <fs_bseg>-hkont = '6051030000'
        OR <fs_bseg>-hkont = '6051040000'
        OR <fs_bseg>-hkont = '6051050000'
        OR <fs_bseg>-hkont = '6051060000'
        OR <fs_bseg>-hkont = '6051070000'
        OR <fs_bseg>-hkont = '6051080000'
        OR <fs_bseg>-hkont = '6051090000'
           ) .
      CLEAR: ls_bseg1.
      READ TABLE lt_bseg1 INTO ls_bseg1 WITH KEY belnr = <fs_bseg>-belnr .
      IF sy-subrc = 0.
        <fs_bseg>-kunnr = ls_bseg1-kunnr.
      ENDIF.

    ENDIF.

    IF <fs_bseg>-shkzg EQ 'S'.
      IF <fs_bseg>-xnegp EQ 'X'.
        <fs_bseg>-shkzg = 'H'.
        <fs_bseg>-dmbtr = <fs_bseg>-dmbtr * -1.
        <fs_bseg>-wrbtr = <fs_bseg>-wrbtr * -1.
        <fs_bseg>-menge = <fs_bseg>-menge * -1.
      ENDIF.
    ELSEIF <fs_bseg>-shkzg EQ 'H'.
      IF <fs_bseg>-xnegp EQ 'X'.
        <fs_bseg>-shkzg = 'S'.
        <fs_bseg>-dmbtr = -1 * <fs_bseg>-dmbtr.
        <fs_bseg>-wrbtr = -1 * <fs_bseg>-wrbtr.
        <fs_bseg>-menge = -1 * <fs_bseg>-menge.
      ELSE.
        <fs_bseg>-dmbtr = -1 * <fs_bseg>-dmbtr.
        <fs_bseg>-wrbtr = -1 * <fs_bseg>-wrbtr.
      ENDIF.
    ENDIF.
    IF <fs_bseg>-vptnr IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_bseg>-vptnr
        IMPORTING
          output = <fs_bseg>-vptnr.

    ENDIF.

*    ENDIF.
  ENDLOOP.

*  REFRESH lt_bseg.
*  REFRESH lt_bseg1.
*  lt_bseg = lt_bseg_tmp.
*  SORT lt_bseg BY belnr lifnr.
*  DELETE lt_bseg WHERE lifnr IS INITIAL.
*
*  lt_bseg1 = lt_bseg_tmp.
*  SORT lt_bseg1 BY belnr kunnr.
*  DELETE lt_bseg1 WHERE kunnr IS INITIAL.
*
*  LOOP AT lt_bseg_tmp ASSIGNING <fs_bseg>.
*
*    IF <fs_bseg>-lifnr IS INITIAL
*      AND (  <fs_bseg>-hkont = '1405080000'
*          OR <fs_bseg>-hkont = '1405090000'
*          OR <fs_bseg>-hkont = '1402000000'
*
*          OR <fs_bseg>-hkont = '1403010000'
*          OR <fs_bseg>-hkont = '1403020000'
*          OR <fs_bseg>-hkont = '1403030000'
*          OR <fs_bseg>-hkont = '1403040000'
*          OR <fs_bseg>-hkont = '1403050000'
*          OR <fs_bseg>-hkont = '1403980000'
*          ) .
*      CLEAR ls_bseg.
*      READ TABLE lt_bseg INTO ls_bseg WITH KEY belnr = <fs_bseg>-belnr .
*      IF sy-subrc = 0.
*        <fs_bseg>-lifnr = ls_bseg-lifnr.
*      ENDIF.
*    ENDIF.
*
*    IF <fs_bseg>-kunnr IS INITIAL
*    AND (  <fs_bseg>-hkont = '6001010000'
*        OR <fs_bseg>-hkont = '6001020000'
*        OR <fs_bseg>-hkont = '6001030000'
*        OR <fs_bseg>-hkont = '6001040000'
*        OR <fs_bseg>-hkont = '6001050000'
*        OR <fs_bseg>-hkont = '6001060000'
*        OR <fs_bseg>-hkont = '6001080000'
*        OR <fs_bseg>-hkont = '6001100000'
*        OR <fs_bseg>-hkont = '6001110000'
*        OR <fs_bseg>-hkont = '6051010000'
*        OR <fs_bseg>-hkont = '6051020000'
*        OR <fs_bseg>-hkont = '6051030000'
*        OR <fs_bseg>-hkont = '6051040000'
*        OR <fs_bseg>-hkont = '6051050000'
*        OR <fs_bseg>-hkont = '6051060000'
*        OR <fs_bseg>-hkont = '6051070000'
*        OR <fs_bseg>-hkont = '6051080000'
*        OR <fs_bseg>-hkont = '6051090000'
*           ) .
*      CLEAR: ls_bseg1.
*      READ TABLE lt_bseg1 INTO ls_bseg1 WITH KEY belnr = <fs_bseg>-belnr .
*      IF sy-subrc = 0.
*        <fs_bseg>-kunnr = ls_bseg1-kunnr.
*      ENDIF.
*
*    ENDIF.
*
*    "没有借贷项区别
*    <fs_bseg>-dmbtr = <fs_bseg>-dmbtr.
*    <fs_bseg>-wrbtr = <fs_bseg>-wrbtr.
*    <fs_bseg>-menge = <fs_bseg>-menge.
*
*    IF <fs_bseg>-vptnr IS NOT INITIAL.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = <fs_bseg>-vptnr
*        IMPORTING
*          output = <fs_bseg>-vptnr.
*
*    ENDIF.
*  ENDLOOP.
*
*
*  APPEND LINES OF lt_bseg_tmp TO gt_bseg.
**" append 预制凭证的数据
*  APPEND LINES OF gt_bseg_pre TO gt_bseg.

ENDFORM.

*---------------------------------------------------------------------*
* Description: 预制凭证获取
*---------------------------------------------------------------------*
FORM line_item_predoc_get .

  TYPES: BEGIN OF ty_balance.
          INCLUDE STRUCTURE ZSFIRPT_003_ALV.
          INCLUDE STRUCTURE ZSFIRPT_003_ALV_common.
  TYPES: END OF ty_balance.

  DATA: lt_bseg_pre TYPE STANDARD TABLE OF ZSFIRPT_003_ALV_item.

  DATA: ls_balance  TYPE ty_balance.

  FIELD-SYMBOLS: <fs_bseg_pre> TYPE ZSFIRPT_003_ALV_item.

*" vbsegs
  SELECT
      vbkpf~bukrs,
      vbkpf~belnr,
      vbkpf~gjahr,
*      vbkpf~xreversal,
      vbkpf~bldat,
      vbkpf~budat,
      vbkpf~bstat,
      vbkpf~waers,
      vbkpf~hwaer,
      vbkpf~bktxt,

      vbsegs~buzei,
      vbsegs~bschl,
      vbsegs~shkzg,
      vbsegs~xnegp,
*      vbsegs~hkont,
      vbsegs~saknr AS hkont,
*      bseg~xbilk,
      vbsegs~prctr,
      vbsegs~kostl,
      vbsegs~koart,
*      vbsegs~umskz,
*      vbsegs~lifnr,
*      vbsegs~kunnr,
*      bseg~zz_feety,
*      bseg~fkber_long,
*      bseg~aufnr,
      vbsegs~dmbtr,
      vbsegs~wrbtr,
      vbsegs~matnr,
      vbsegs~menge,
      vbsegs~meins,
      vbsegs~gsber,
      vbsegs~fkber,
*      bseg~menge,
      vbsegs~zuonr
*      vbsegs~pswsl
      INTO CORRESPONDING FIELDS OF TABLE @lt_bseg_pre
      FROM vbkpf
        INNER JOIN vbsegs ON vbsegs~bukrs = vbkpf~bukrs
                         AND vbsegs~belnr = vbkpf~belnr
                         AND vbsegs~gjahr = vbkpf~gjahr
*      WHERE VBKPF~BUKRS  = @P_BUKRS
      WHERE vbkpf~bukrs  IN @g_bukrs
*        and bkpf~gjahr  = @p_gjahr
*        and bkpf~monat  = @p_monat
        AND vbkpf~budat  <= @gv_budat_to
        AND vbkpf~bstat  NE 'M'            " 排除样本凭证
*        and vbsegs~hkont  in @s_hkont
        AND vbsegs~saknr  IN @s_hkont
        AND vbsegs~kostl  IN @s_kostl
        AND vbsegs~prctr  IN @s_prctr
        AND vbsegs~gsber  IN @s_gsber
        AND vbsegs~fkber  IN @s_rfarea
        .

*" VBSEGA, APPEND
  SELECT
      vbkpf~bukrs,
      vbkpf~belnr,
      vbkpf~gjahr,
*      vbkpf~xreversal,
      vbkpf~bldat,
      vbkpf~budat,
      vbkpf~bstat,
      vbkpf~waers,
      vbkpf~hwaer,
      vbkpf~bktxt,

      vbsega~buzei,
      vbsega~bschl,
      vbsega~shkzg,
      vbsega~xnegp,
*      vbsega~hkont,
      vbsega~hkont,
*      vbsega~xbilk,
      vbsega~prctr,
      vbsega~kostl,
*      vbsega~koart,
*      vbsega~umskz,
*      vbsega~lifnr,
*      vbsega~kunnr,
*      vbsega~zz_feety,
*      vbsega~fkber_long,
*      vbsega~aufnr,
      vbsega~dmbtr,
      vbsega~wrbtr,
      vbsega~matnr,
      vbsega~menge,
      vbsega~meins,
      vbsega~gsber,
      vbsega~fkber,
*      vbsega~menge,
      vbsega~zuonr
*      vbsegs~pswsl
      APPENDING CORRESPONDING FIELDS OF TABLE @lt_bseg_pre
      FROM vbkpf
        INNER JOIN vbsega ON vbsega~bukrs = vbkpf~bukrs
                         AND vbsega~belnr = vbkpf~belnr
                         AND vbsega~gjahr = vbkpf~gjahr
*      WHERE VBKPF~BUKRS  = @P_BUKRS
      WHERE vbkpf~bukrs  IN @g_bukrs
*        and bkpf~gjahr  = @p_gjahr
*        and bkpf~monat  = @p_monat
        AND vbkpf~budat  <= @gv_budat_to
        AND vbkpf~bstat  NE 'M'            " 排除样本凭证
*        and vbsegs~hkont  in @s_hkont
        AND vbsega~hkont  IN @s_hkont
        AND vbsega~kostl  IN @s_kostl
        AND vbsega~prctr  IN @s_prctr
        AND vbsega~gsber  IN @s_gsber
        AND vbsega~fkber  IN @s_rfarea
        .

*" VBSEGD
  SELECT
      vbkpf~bukrs,
      vbkpf~belnr,
      vbkpf~gjahr,
*      vbkpf~xreversal,
      vbkpf~bldat,
      vbkpf~budat,
      vbkpf~bstat,
      vbkpf~waers,
      vbkpf~hwaer,
      vbkpf~bktxt,

      vbsegd~buzei,
      vbsegd~bschl,
      vbsegd~shkzg,
      vbsegd~xnegp,
*      VBSEGD~hkont,
      vbsegd~hkont,
*      VBSEGD~xbilk,
*      vbsegd~prctr,  " 表中无此字段
*      vbsegd~kostl,  " 表中无此字段
*      VBSEGD~koart,
*      VBSEGD~umskz,
*      VBSEGD~lifnr,
*      VBSEGD~kunnr,
*      VBSEGD~zz_feety,
*      VBSEGD~fkber_long,
*      VBSEGD~aufnr,
      vbsegd~dmbtr,
      vbsegd~wrbtr,
*      vbsegd~matnr,
*      vbsegd~menge,
*      vbsegd~meins,
      vbsegd~gsber,
      vbsegd~fkber
*      VBSEGD~menge,
*      vbsega~zuonr
*      VBSEGD~pswsl
      APPENDING CORRESPONDING FIELDS OF TABLE @lt_bseg_pre
      FROM vbkpf
        INNER JOIN vbsegd ON vbsegd~bukrs = vbkpf~bukrs
                         AND vbsegd~belnr = vbkpf~belnr
                         AND vbsegd~gjahr = vbkpf~gjahr
*      WHERE VBKPF~BUKRS  = @P_BUKRS
      WHERE vbkpf~bukrs  IN @g_bukrs
*        and bkpf~gjahr  = @p_gjahr
*        and bkpf~monat  = @p_monat
        AND vbkpf~budat  <= @gv_budat_to
        AND vbkpf~bstat  NE 'M'            " 排除样本凭证
*        and vbsegs~hkont  in @s_hkont
        AND vbsegd~hkont  IN @s_hkont
*        and vbsegd~kostl  in @s_kostl      " 表中无此字段
*        and vbsegd~prctr  in @s_prctr      " 表中无此字段
        AND vbsegd~gsber  IN @s_gsber
        AND vbsegd~fkber  IN @s_rfarea
        .

*" VBSEGK
  SELECT
      vbkpf~bukrs,
      vbkpf~belnr,
      vbkpf~gjahr,
*      vbkpf~xreversal,
      vbkpf~bldat,
      vbkpf~budat,
      vbkpf~bstat,
      vbkpf~waers,
      vbkpf~hwaer,
      vbkpf~bktxt,

      vbsegk~buzei,
      vbsegk~bschl,
      vbsegk~shkzg,
      vbsegk~xnegp,
*      VBSEGK~hkont,
      vbsegk~hkont,
*      VBSEGK~xbilk,
*      vbsegk~prctr,
*      vbsegk~kostl,
*      vbsegk~koart,
*      VBSEGK~umskz,
*      VBSEGK~lifnr,
*      VBSEGK~kunnr,
*      VBSEGK~zz_feety,
*      bseg~fkber_long,
*      VBSEGK~dmbtr,
      vbsegk~wrbtr,
*      vbsegk~matnr,
*      vbsegk~menge,
*      vbsegk~meins,
      vbsegk~gsber,
      vbsegk~fkber,
*      VBSEGK~menge,
      vbsegk~zuonr
*      VBSEGK~pswsl
      APPENDING CORRESPONDING FIELDS OF TABLE @lt_bseg_pre
      FROM vbkpf
        INNER JOIN vbsegk ON vbsegk~bukrs = vbkpf~bukrs
                         AND vbsegk~belnr = vbkpf~belnr
                         AND vbsegk~gjahr = vbkpf~gjahr
*      WHERE VBKPF~BUKRS  = @P_BUKRS
      WHERE vbkpf~bukrs  IN @g_bukrs
*        and bkpf~gjahr  = @p_gjahr
*        and bkpf~monat  = @p_monat
        AND vbkpf~budat  <= @gv_budat_to
        AND vbkpf~bstat  NE 'M'            " 排除样本凭证
        AND vbsegk~hkont  IN @s_hkont
*        and vbsegk~saknr  in @s_hkont
*        and vbsegk~kostl  in @s_kostl
*        and vbsegk~prctr  in @s_prctr
        AND vbsegk~gsber  IN @s_gsber
        AND vbsegk~fkber  IN @s_rfarea
        .

  LOOP AT lt_bseg_pre ASSIGNING <fs_bseg_pre>.
    IF <fs_bseg_pre>-shkzg EQ 'S'.
      IF <fs_bseg_pre>-xnegp EQ 'X'.
        <fs_bseg_pre>-shkzg = 'H'.
        <fs_bseg_pre>-dmbtr = <fs_bseg_pre>-dmbtr * -1.
        <fs_bseg_pre>-wrbtr = <fs_bseg_pre>-wrbtr * -1.
        <fs_bseg_pre>-menge = <fs_bseg_pre>-menge * -1.
      ENDIF.
    ELSEIF <fs_bseg_pre>-shkzg EQ 'H'.
      IF <fs_bseg_pre>-xnegp EQ 'X'.
        <fs_bseg_pre>-shkzg = 'S'.
        <fs_bseg_pre>-dmbtr = -1 * <fs_bseg_pre>-dmbtr.
        <fs_bseg_pre>-wrbtr = -1 * <fs_bseg_pre>-wrbtr.
        <fs_bseg_pre>-menge = -1 * <fs_bseg_pre>-menge.
      ELSE.
*        <fs_bseg_pre>-dmbtr = -1 * <fs_bseg_pre>-dmbtr.
*        <fs_bseg_pre>-wrbtr = -1 * <fs_bseg_pre>-wrbtr.
      ENDIF.
    ENDIF.
*" 由于BSEG/VBSEGS里的字段和FAGLFLEXT表里的字段名称不一样,
*" 这里做一个mapping
    ls_balance-rbukrs = <fs_bseg_pre>-bukrs.
    ls_balance-racct  = <fs_bseg_pre>-hkont.
    ls_balance-rcntr  = <fs_bseg_pre>-kostl.
    ls_balance-prctr  = <fs_bseg_pre>-prctr.
    ls_balance-rfarea = <fs_bseg_pre>-fkber.
    ls_balance-rbusa  = <fs_bseg_pre>-gsber.

*" 期初金额 & 期初数量
    IF <fs_bseg_pre>-budat < gv_budat_from.
      IF <fs_bseg_pre>-shkzg EQ 'S'.
*" 加 借方
        ls_balance-opening_balance        = ls_balance-opening_balance +
                                            <fs_bseg_pre>-dmbtr.
        ls_balance-opening_balance_wrbtr  = ls_balance-opening_balance_wrbtr +
                                            <fs_bseg_pre>-wrbtr.
        ls_balance-opening_menge          = ls_balance-opening_menge +
                                            <fs_bseg_pre>-menge.
      ELSE.
*" 减 贷方
        ls_balance-opening_balance        = ls_balance-opening_balance -
                                            <fs_bseg_pre>-dmbtr.
        ls_balance-opening_balance_wrbtr  = ls_balance-opening_balance_wrbtr -
                                            <fs_bseg_pre>-wrbtr.
        ls_balance-opening_menge          = ls_balance-opening_menge -
                                            <fs_bseg_pre>-menge.
      ENDIF.
    ENDIF.

*" 本期借方 & *" 本期贷方
    IF gv_budat_from <= <fs_bseg_pre>-budat AND <fs_bseg_pre>-budat <= gv_budat_to.
      IF <fs_bseg_pre>-shkzg EQ 'S'.
        ls_balance-dmbtr_debit  = <fs_bseg_pre>-dmbtr.
        ls_balance-wrbtr_debit  = <fs_bseg_pre>-wrbtr.
        ls_balance-menge_debit  = <fs_bseg_pre>-menge.
      ELSE.
        ls_balance-dmbtr_credit = <fs_bseg_pre>-dmbtr.
        ls_balance-wrbtr_credit = <fs_bseg_pre>-wrbtr.
        ls_balance-menge_credit = <fs_bseg_pre>-menge.
      ENDIF.
    ENDIF.

*" 累计借方 & 累计贷方
    IF <fs_bseg_pre>-shkzg EQ 'S'.
      ls_balance-dmbtr_debit_cum  = <fs_bseg_pre>-dmbtr.
      ls_balance-wrbtr_debit_cum  = <fs_bseg_pre>-wrbtr.
      ls_balance-menge_debit_cum  = <fs_bseg_pre>-menge.
    ELSE.
      ls_balance-dmbtr_credit_cum = <fs_bseg_pre>-dmbtr.
      ls_balance-wrbtr_credit_cum = <fs_bseg_pre>-wrbtr.
      ls_balance-menge_credit_cum = <fs_bseg_pre>-menge.
    ENDIF.

*" 期末余额
    ls_balance-closing_balance        = ls_balance-opening_balance +
                                        ls_balance-dmbtr_debit     -
                                        ls_balance-dmbtr_credit.

    ls_balance-closing_balance_wrbtr  = ls_balance-opening_balance_wrbtr +
                                        ls_balance-wrbtr_debit           -
                                        ls_balance-wrbtr_credit.

    ls_balance-closing_menge          = ls_balance-opening_menge +
                                        ls_balance-menge_debit   -
                                        ls_balance-menge_credit.

    MOVE-CORRESPONDING ls_balance TO <fs_output_line>.
    COLLECT <fs_output_line> INTO <fs_output_table>.

    CLEAR: ls_balance,
           <fs_output_line>.
  ENDLOOP.

  CLEAR gt_bseg_pre.
  gt_bseg_pre = lt_bseg_pre.

ENDFORM.

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM line_item_display USING it_bseg TYPE tt_bseg.

  DATA: lv_callback_program TYPE sy-repid.
  DATA: lv_title    TYPE lvc_title,
        ls_layout   TYPE lvc_s_layo,
        ls_variant  TYPE disvariant,
        lt_fieldcat TYPE lvc_t_fcat,
        ls_fieldcat TYPE lvc_s_fcat.

  DATA: lv_lines        TYPE i,
        lv_lines_string TYPE string.

*  if gt_output is not initial.
  IF it_bseg IS NOT INITIAL.
*    lv_lines = lines( gt_output ).
    lv_lines = lines( it_bseg ).
  ENDIF.

  lv_lines_string = lv_lines.

  lv_title = '条目数:' && lv_lines_string.
  CONDENSE lv_title.

*" Layout
  ls_layout-cwidth_opt    = 'X'.
  ls_layout-zebra         = 'X'.
  ls_layout-smalltitle    = 'X'.

*" Layout Variant
  ls_variant-report = 'ZSHXJFI0003'.
  ls_variant-handle = 'L2ND'. " 2级ALV明细界面

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE  =
      i_structure_name = 'ZSFIRPT_003_ALV_ITEM'
*     I_CLIENT_NEVER_DISPLAY       = 'X'
*     I_BYPASSING_BUFFER           =
*     I_INTERNAL_TABNAME           =
    CHANGING
      ct_fieldcat      = lt_fieldcat
*   EXCEPTIONS
*     INCONSISTENT_INTERFACE       = 1
*     PROGRAM_ERROR    = 2
*     OTHERS           = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT lt_fieldcat INTO ls_fieldcat.
    IF ls_fieldcat-fieldname EQ 'BUKRS'
    OR ls_fieldcat-fieldname EQ 'BELNR'
    OR ls_fieldcat-fieldname EQ 'GJAHR'
*        or ls_fieldcat-fieldname eq 'BUZEI'
    .

      ls_fieldcat-key = abap_true.
      MODIFY lt_fieldcat FROM ls_fieldcat.
    ELSEIF ls_fieldcat-fieldname EQ 'KOSTL_TEXT'.
      ls_fieldcat-reptext   = '成本中心描述'.
      ls_fieldcat-scrtext_l = '成本中心描述'.
      ls_fieldcat-scrtext_m = '成本中心描述'.
      ls_fieldcat-scrtext_s = '成本中心描述'.
      MODIFY lt_fieldcat FROM ls_fieldcat.
    ELSEIF ls_fieldcat-fieldname EQ 'PRCTR_TEXT'.
      ls_fieldcat-reptext   = '利润中心描述'.
      ls_fieldcat-scrtext_l = '利润中心描述'.
      ls_fieldcat-scrtext_m = '利润中心描述'.
      ls_fieldcat-scrtext_s = '利润中心描述'.
      MODIFY lt_fieldcat FROM ls_fieldcat.
    ELSEIF ls_fieldcat-fieldname EQ 'KUNNR_TEXT'.
      ls_fieldcat-reptext   = '客户描述'.
      ls_fieldcat-scrtext_l = '客户描述'.
      ls_fieldcat-scrtext_m = '客户描述'.
      ls_fieldcat-scrtext_s = '客户描述'.
      MODIFY lt_fieldcat FROM ls_fieldcat.
    ELSEIF ls_fieldcat-fieldname EQ 'LIFNR_TEXT'.
      ls_fieldcat-reptext   = '供应商描述'.
      ls_fieldcat-scrtext_l = '供应商描述'.
      ls_fieldcat-scrtext_m = '供应商描述'.
      ls_fieldcat-scrtext_s = '供应商描述'.
      MODIFY lt_fieldcat FROM ls_fieldcat.
    ELSEIF ls_fieldcat-fieldname EQ 'VPTNR'.
      ls_fieldcat-reptext   = '业务员编号'.
      ls_fieldcat-scrtext_l = '业务员编号'.
      ls_fieldcat-scrtext_m = '业务员编号'.
      ls_fieldcat-scrtext_s = '业务员编号'.
      MODIFY lt_fieldcat FROM ls_fieldcat.
    ELSEIF ls_fieldcat-fieldname EQ 'VPTNR_TEXT'.
      ls_fieldcat-reptext   = '业务员名称'.
      ls_fieldcat-scrtext_l = '业务员名称'.
      ls_fieldcat-scrtext_m = '业务员名称'.
      ls_fieldcat-scrtext_s = '业务员名称'.
      MODIFY lt_fieldcat FROM ls_fieldcat.

    ELSEIF ls_fieldcat-fieldname EQ 'MATNR_TEXT'.
      ls_fieldcat-reptext   = '物料描述'.
      ls_fieldcat-scrtext_l = '物料描述'.
      ls_fieldcat-scrtext_m = '物料描述'.
      ls_fieldcat-scrtext_s = '物料描述'.
      MODIFY lt_fieldcat FROM ls_fieldcat.

    ELSEIF ls_fieldcat-fieldname EQ 'MATNR_TEXT1'.
      ls_fieldcat-reptext   = '物料名称'.
      ls_fieldcat-scrtext_l = '物料名称'.
      ls_fieldcat-scrtext_m = '物料名称'.
      ls_fieldcat-scrtext_s = '物料名称'.
      MODIFY lt_fieldcat FROM ls_fieldcat.

    ELSEIF ls_fieldcat-fieldname EQ 'VTEXT'.
      ls_fieldcat-reptext   = '物料科目组'.
      ls_fieldcat-scrtext_l = '物料科目组'.
      ls_fieldcat-scrtext_m = '物料科目组'.
      ls_fieldcat-scrtext_s = '物料科目组'.
      MODIFY lt_fieldcat FROM ls_fieldcat.

    ENDIF.
  ENDLOOP.

  lv_callback_program = sy-repid.

  CLEAR: gt_bseg_2nd_lvl.
  gt_bseg_2nd_lvl = it_bseg.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK       = ' '
*     I_BYPASSING_BUFFER      =
*     I_BUFFER_ACTIVE         =
      i_callback_program      = lv_callback_program
*     I_CALLBACK_PF_STATUS_SET          = ' '
      i_callback_user_command = 'ALV_2ND_LVL_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE  = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME        =
*     I_BACKGROUND_ID         = ' '
      i_grid_title            = lv_title
*     I_GRID_SETTINGS         =
      is_layout_lvc           = ls_layout
      it_fieldcat_lvc         = lt_fieldcat
*     IT_EXCLUDING            =
*     IT_SPECIAL_GROUPS_LVC   =
*     IT_SORT_LVC             =
*     IT_FILTER_LVC           =
*     IT_HYPERLINK            =
*     IS_SEL_HIDE             =
*     I_DEFAULT               = 'X'
      i_save                  = 'X'
      is_variant              = ls_variant
*     IT_EVENTS               =
*     IT_EVENT_EXIT           =
*     IS_PRINT_LVC            =
*     IS_REPREP_ID_LVC        =
*     I_SCREEN_START_COLUMN   = 0
*     I_SCREEN_START_LINE     = 0
*     I_SCREEN_END_COLUMN     = 0
*     I_SCREEN_END_LINE       = 0
*     I_HTML_HEIGHT_TOP       =
*     I_HTML_HEIGHT_END       =
*     IT_ALV_GRAPHICS         =
*     IT_EXCEPT_QINFO_LVC     =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER =
*     ES_EXIT_CAUSED_BY_USER  =
    TABLES
      t_outtab                = it_bseg
*   EXCEPTIONS
*     PROGRAM_ERROR           = 1
*     OTHERS                  = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

FORM alv_2nd_lvl_user_command USING r_ucomm TYPE sy-ucomm
                            ps_selfield TYPE slis_selfield.
*" detail, 2nd level alv

  IF r_ucomm EQ '&IC1'.
    PERFORM alv_2nd_lvl_handle_dclick USING ps_selfield.
  ENDIF.

  ps_selfield-refresh = abap_true.

ENDFORM. "ALV_USER_COMMAND

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM alv_2nd_lvl_handle_dclick USING ps_selfield TYPE slis_selfield.

  DATA: ls_bseg LIKE LINE OF gt_bseg.
*  read table gt_bseg_2nd_lvl into ls_bseg index ps_selfield-tabindex.
*  check sy-subrc = 0.
*
*  if ps_selfield-fieldname eq 'BUKRS'
*      or ps_selfield-fieldname eq 'BELNR'
*      or ps_selfield-fieldname eq 'GJAHR'.

  DATA g_grid TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_grid.
  CALL METHOD g_grid->check_changed_data.

  IF ps_selfield-fieldname = 'BELNR'.
    DATA: lv_belnr TYPE bseg-belnr.
    CLEAR lv_belnr.
    lv_belnr = ps_selfield-value.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_belnr
      IMPORTING
        output = lv_belnr.

    READ TABLE gt_bseg_2nd_lvl INTO ls_bseg WITH KEY belnr = lv_belnr.
    CHECK sy-subrc = 0.

    SET PARAMETER ID 'BLN' FIELD ls_bseg-belnr.
    SET PARAMETER ID 'BUK' FIELD ls_bseg-bukrs.
    SET PARAMETER ID 'GJR' FIELD ls_bseg-gjahr.
    CALL TRANSACTION 'FB03'WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_data .
  DATA:gt_bi TYPE TABLE OF zTfi_bi_001.
  DATA:ls_bi TYPE zTfi_bi_001.

  DATA:BEGIN OF  ls_output,
         rbukrs                TYPE zTfi_bi_001-bukrs,                " 公司代码
         racct                 TYPE zTfi_bi_001-racct,                " 总账科目
         racct_text            TYPE zTfi_bi_001-racct_text,           " 总账科目长文本
         opening_balance       TYPE fdbl_balance_line-balance,       " 期初金额
         opening_menge         TYPE faglflext-mslvt,                 " 期初数量
         opening_shkzg_text    TYPE zTfi_bi_001-zqqfx,                " 期初方向

         dmbtr_debit           TYPE fdbl_balance_line-balance,       " 累计借方->凭证货币->本年
         menge_debit           TYPE fdbl_balance_line-balance,       " 累计贷方
         dmbtr_credit          TYPE fdbl_balance_line-balance,       " 累计贷方->本年
         menge_credit          TYPE fdbl_balance_line-balance,       " 累计贷方->凭证货币

         dmbtr_debit_cum_year  TYPE fdbl_balance_line-balance,       " 累计特殊季度贷方
         menge_debit_cum_year  TYPE fdbl_balance_line-balance,       " 累计特殊季度借方



         dmbtr_credit_cum_year TYPE fdbl_balance_line-balance,       " 累计贷方->凭证货币->本年
         menge_credit_cum_year TYPE fdbl_balance_line-balance,    " 累计余额->凭证货币
         dmbtr_debit_cum       TYPE fdbl_balance_line-balance,
         menge_debit_cum       TYPE fdbl_balance_line-balance,
         dmbtr_credit_cum      TYPE fdbl_balance_line-balance,
         menge_credit_cum      TYPE fdbl_balance_line-balance,
         closing_balance       TYPE fdbl_balance_line-balance,       " 期末金额
         closing_menge         TYPE faglflext-mslvt,                 " 期末数量
         closing_shkzg         TYPE zTfi_bi_001-zqmfx,                "期末方向


       END OF ls_output.
  DATA: lt_output LIKE TABLE OF ls_output.

  MOVE-CORRESPONDING  <fs_output_table>  TO lt_output.

  LOOP AT lt_output INTO ls_output.

    ls_bi-bukrs = ls_output-rbukrs   .
    ls_bi-gjahr = sy-datum+0(4).  .
    ls_bi-monat = sy-datum+4(2). .
    ls_bi-racct = ls_output-racct   .
    ls_bi-racct_text = ls_output-racct_text   .
    ls_bi-zqqsl = ls_output-opening_menge   .
    ls_bi-zqqjr = ls_output-opening_balance   .
    ls_bi-zqqfx = ls_output-opening_shkzg_text   .
    ls_bi-zbqjfjr = ls_output-menge_credit_cum_year .
    ls_bi-zbqdfjr = ls_output-menge_debit_cum_year   .
    ls_bi-zbnrjjfjr =  ls_output-dmbtr_debit_cum_year .
    ls_bi-zbnrjdfjr =  ls_output-dmbtr_credit_cum_year  .
    ls_bi-zqmyr = ls_output-closing_balance   .
    ls_bi-zqmfx  = ls_output-closing_shkzg   .

    APPEND ls_bi  TO gt_bi.
    CLEAR:ls_bi.

  ENDLOOP.

  MODIFY  zTfi_bi_001 FROM TABLE gt_bi.







ENDFORM.


*Messages
*----------------------------------------------------------
*
* Message class: FDBL
*010   Line item report cannot be called up for the selected cells

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
