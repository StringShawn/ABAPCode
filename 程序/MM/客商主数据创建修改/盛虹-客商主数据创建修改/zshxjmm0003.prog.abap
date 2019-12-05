**************************************************
*程序名称:MDM客户供应商主数据创建修改
*创建日期: 2019-11-22
*创建者:XXX
*申请者:XXX
*功能描述:
*============================================
*变更记录
*修改请求号    修改日期    修改人   修改描述
*DEVK912035   2019-11-22   HAND   平移
*============================================
*原始记录
*& Program Name         <程序名称>: ZPPRPT_003
*& Purpose              <程序用途>: MDM客户供应商主数据创建修改
*& Project Name         <项目名称>: COFCO-SAP
*& Created by           <创 建 人>: IBM-GYF
*& Created on           <创建日期>: 20180620
*& Functional Consultant<功能顾问>: PP-汪怀东
*& Description          <功能描述>: .MDM客户供应商主数据创建修改
***************************************************
REPORT ZSHXJMM0003 MESSAGE-ID zfishxj01.
TABLES : bp001 .
TYPES : BEGIN OF typ_alv .
    INCLUDE TYPE ztMM_mdm_bp_log .
TYPES : e_mail(240) TYPE c,
        bukrs       TYPE bukrs,
        akont       TYPE akont,
        zterm       TYPE dzterm,
        name2       TYPE name2,
        fox         TYPE c,
        zxbs        TYPE icon-id,    "消息灯
        cbox        TYPE c ,      "    复选框
        END OF typ_alv .
DATA : GS_ALV      TYPE typ_alv,
       gt_msg      TYPE TABLE OF typ_alv,
       gt_bapi_msg TYPE TABLE OF bapiret2,
       gt_alv      TYPE TABLE OF typ_alv.

* ALV 用
DATA:
  GS_layout   TYPE lvc_s_layo,
  gv_repid    TYPE repid,
  it_fieldcat TYPE lvc_t_fcat.
DATA: g_grid TYPE REF TO cl_gui_alv_grid.
DATA: gt_alvdata LIKE bdcdata OCCURS 0 WITH HEADER LINE.
DEFINE  d_output.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
         EXPORTING
           input  = &1
         IMPORTING
           output = &1.
END-OF-DEFINITION.


*----------------------------------------------------------------------*
*  Selection Screen                                                    *
*----------------------------------------------------------------------*
*创建字段宏定义
DEFINE add_field.
  ls_fieldcat-fieldname = '&1'.   "内表字段名称
  ls_fieldcat-scrtext_m =  &2.    "字段输出名称
  ls_fieldcat-checkbox   = &3.    "是否显示零
  ls_fieldcat-just       = &4.    "是否显示零
  ls_fieldcat-emphasize  = &5.    "是否显示零
  ls_fieldcat-edit      =  &6.    "编辑
  ls_fieldcat-key       =  &7.    "固定列
  ls_fieldcat-convexit = &8.
  ls_fieldcat-icon   = &9.
*  IF ls_fieldcat-fieldname = 'MESSAGE' .
*   ls_fieldcat-OUTPUTLEN = 50 .
*  ENDIF.
  APPEND ls_fieldcat TO it_fieldcat.
  CLEAR ls_fieldcat.
END-OF-DEFINITION.

DEFINE  d_input.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
           input  = &1
         IMPORTING
           output = &1.
END-OF-DEFINITION.
"选择屏幕
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_lifnr FOR bp001-partner ,
                s_group FOR bp001-group_d .
PARAMETERS: p_file  LIKE rlgrap-filename MODIF ID m1.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS  : p_rad  RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND cmd,
              p_rad1 RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN PUSHBUTTON 1(22) but1 USER-COMMAND download. " 定义搜索按钮

FIELD-SYMBOLS: <fs_tab>  TYPE typ_alv,
               <fs_cell> TYPE kcde_cells.
DATA : gt_cells LIKE STANDARD TABLE OF kcde_cells.
*&---------------------------------------------------------------------*
*&   Event AT INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION .
  gv_repid = sy-repid.
  CALL FUNCTION 'ICON_CREATE' " 给按钮添加图标和文本
    EXPORTING
      name   = 'ICON_EXPORT'   " 按钮的图片的名字 ICON_EXPORT
      text   = '模版下载'                   "按钮的文本
      info   = '功能'
    IMPORTING
      result = but1
    EXCEPTIONS
      OTHERS = 0.
*********************************************************************
* AT SELECTION-SCREEN
*********************************************************************
AT SELECTION-SCREEN OUTPUT .
  IF p_rad = 'X'.
    LOOP AT SCREEN .
      IF screen-name = 'SSCRFIELDS-UCOMM' OR screen-name = 'BUT1' OR screen-group4 = '003' .
        screen-active = 0 .
      ELSE .
        screen-active = 1 .
      ENDIF.
      MODIFY SCREEN .
    ENDLOOP.
  ELSE .
    LOOP AT SCREEN .
      IF screen-group4 = '001' OR screen-group4 = '002' .
        screen-active = 0 .
      ELSE .
        screen-active = 1 .
      ENDIF.
      MODIFY SCREEN .
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'DOWNLOAD'.
      PERFORM frm_down_data.
    WHEN OTHERS.
  ENDCASE.
*********************************************************************
* AT SELECTION-SCREEN
*********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM frm_open_file.
*----------------------------------------------------------------------*
* Event Occurs After The Selection Screen Has Been Processed
*----------------------------------------------------------------------*
START-OF-SELECTION.

*  IF p_rb_1 = 'X'.
  PERFORM frm_auth_check.
  PERFORM frm_get_data.
  IF sy-batch = 'X'.
    PERFORM pfm_bp.
  ELSE .
    PERFORM frm_display_data.
  ENDIF.
*----------------------------------------------------------------------*
* The Last Of The Events Called By The Runtime Environment To Occur
*----------------------------------------------------------------------*
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       报表展示
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_display_data .
*定义输出模式
  CLEAR:GS_layout,it_fieldcat.
  GS_layout-cwidth_opt         = 'X'.
  GS_layout-zebra              = 'X'.
  GS_layout-box_fname              = 'CBOX'.
  gv_repid = sy-repid.
  PERFORM frm_set_fieldcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = gv_repid
      i_callback_user_command  = 'FRM_USER_COMMAND'
      i_callback_pf_status_set = 'FRM_PF_STATUS'
      is_layout_lvc            = GS_layout
      it_fieldcat_lvc          = it_fieldcat
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_alv
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_FIELDCAT
*&---------------------------------------------------------------------*
*       AVL展示绑定及显示效果
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_set_fieldcat .
  DATA:ls_fieldcat TYPE lvc_s_fcat.
  IF p_rad = 'X'.

    add_field : logid '日志ID'  ''  ''  ''  ''  '' ''  ''.  " 日志ID
    add_field :
    lifnr '单位代码'  ''  ''  ''  ''  '' 'ALPHA'  '',  " 单位代码
    bu_group  '业务伙伴分组'  ''  ''  ''  ''  '' ''  '',  " 业务伙伴分组
    zzt '数据状态'  ''  ''  ''  ''  '' ''  '',  " 数据状态
    name1 '单位全称'  ''  ''  ''  ''  '' ''  '',  " 单位全称
    sortl '单位简称'  ''  ''  ''  ''  '' ''  '',  " 单位简称
    group_d '单位性质'  ''  ''  ''  ''  '' ''  '',  " 单位性质
    zsfgys  '是否供应商' ''  ''  ''  ''  '' ''  '',  " 是否供应商
    zskf  '是否客户'  ''  ''  ''  ''  '' ''  '',  " 是否客户
    taxnum1 '统一社会信用代码'  ''  ''  ''  ''  '' ''  '',  " 统一社会信用代码
    taxnum2 '全国组织机构代码'  ''  ''  ''  ''  '' ''  '',  " 全国组织机构代码
    taxnum3 '个人有效证件号' ''  ''  ''  ''  '' ''  '',  " 个人有效证件号
    land1 '国家'  ''  ''  ''  ''  '' ''  '',  " 国家
    regio '省份/直辖市'  ''  ''  ''  ''  '' ''  '',  " 省份/直辖市
    ort01 '城市'  ''  ''  ''  ''  '' ''  '',  " 城市
    stras '地址'  ''  ''  ''  ''  '' ''  '',  " 地址
    pstlz '邮政编码'  ''  ''  ''  ''  '' ''  '',  " 邮政编码
    telf1 '电话号码'  ''  ''  ''  ''  '' ''  '',  " 电话号码
    zid '分发数据ID'  ''  ''  ''  ''  '' ''  '',  " 分发数据ID
    status  'MDM 数据状态 ' ''  ''  ''  ''  '' ''  '',  " MDM 数据状态
    creater_id  '数据创建人ID' ''  ''  ''  ''  '' ''  '',  " 数据创建人ID
    creater_name  '数据创建人名称 '  ''  ''  ''  ''  '' ''  '',  " 数据创建人名称
    creater_time  '数据创建时间'  ''  ''  ''  ''  '' ''  '',  " 数据创建时间
    dycs  '调用次数'  ''  ''  ''  ''  '' ''  '',  " 调用次数
    hczt  '回传状态 ' ''  ''  ''  ''  '' ''  '',  " 回传状态
    cjzt  '更新主数据状态 '  ''  ''  ''  ''  '' ''  '',  " 更新主数据状态
    message '状态消息 ' ''  ''  ''  ''  '' ''  '',  " 状态消息
    zernam  '创建人 '  ''  ''  ''  ''  '' ''  '',  " 创建人
    zerdat  '创建日期'  ''  ''  ''  ''  '' ''  '',  " 创建日期
    zertim  '创建时间'  ''  ''  ''  ''  '' ''  ''.  " 创建时间
  ELSE .
    add_field :
    zxbs '状态 ' ''  ''  ''  ''  '' ''  '',  " 状态消息
    message '状态消息 ' ''  ''  ''  ''  '' ''  '',  " 状态消息
    lifnr '员工编码'  ''  ''  ''  ''  '' 'ALPHA'  '',  " 单位代码
    name1 '姓名'  ''  ''  ''  ''  '' ''  '',  " 单位全称
    name2 '身份证号'  ''  ''  ''  ''  '' ''  '',  " 单位简称
    sortl ' 搜索项1/2'  ''  ''  ''  ''  '' ''  '',  " 单位简称
    land1 ' 国家/地区'  ''  ''  ''  ''  '' ''  '',  " 单位简称
    stras '街道'  ''  ''  ''  ''  '' ''  '',  " 地址
    pstlz '邮政编码'  ''  ''  ''  ''  '' ''  '',  " 邮政编码
    ort01 '城市'  ''  ''  ''  ''  '' ''  '',  " 城市
    telf1 '手机'  ''  ''  ''  ''  '' ''  '',  " 电话号码
    e_mail '电子信箱'  ''  ''  ''  ''  '' ''  '',
    bukrs '公司代码'  ''  ''  ''  ''  '' ''  '',
    akont '统驭科目'  ''  ''  ''  ''  '' ''  '',
    zterm '付款条件'  ''  ''  ''  ''  '' ''  ''
    .





*

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SUB_FRM_USER_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_pf_status USING extab TYPE slis_t_extab.
  DATA: fcode TYPE TABLE OF sy-ucomm.
  SET PF-STATUS 'STANDARD' EXCLUDING fcode[] .
*  SET TITLEBAR 'SCREEN_1000'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  frm_user_command
*&---------------------------------------------------------------------*
*       自定义用户动作
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

*自定义用户动作
FORM frm_user_command USING p_ucomm TYPE sy-ucomm rs_selfield TYPE slis_selfield.
  DATA : l_ucomm TYPE sy-ucomm  .
  IF g_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = g_grid.
  ENDIF.
  CALL METHOD g_grid->check_changed_data.
  rs_selfield-refresh     = 'X'.
  rs_selfield-col_stable  = 'X'.
  rs_selfield-row_stable  = 'X'.
  l_ucomm = p_ucomm .
  CLEAR p_ucomm .
  DATA : l_uname   TYPE sy-uname,
         l_datum   TYPE sy-datum,
         l_uzeit   TYPE sy-uzeit,
         l_partner TYPE bu_partner.

  CASE l_ucomm.
    WHEN 'INPUT'.
      PERFORM pfm_bp  .
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_auth_check .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_data .
  IF p_rad = 'X'.
    SELECT
      *
      INTO TABLE gt_alv
      FROM ztMM_MDM_BP_LOG
      WHERE lifnr IN s_lifnr
      AND bu_group IN  s_group
      AND hczt   = ''
      AND dycs   =< 3
      AND NOT cjzt   = 'S'
    .
    SORT gt_alv BY zzt lifnr DESCENDING zerdat  DESCENDING  zertim DESCENDING .
    DELETE ADJACENT DUPLICATES FROM gt_alv COMPARING lifnr .
  ELSE .

    PERFORM frm_tidy.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PFM_CREATE_BP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM pfm_create_bp TABLES pt_alv LIKE gt_alv.

  DATA:
     lw_head               TYPE bapibus1006_head
    ,lw_organ              TYPE bapibus1006_central_organ
    ,lw_central            TYPE bapibus1006_central
    ,lw_address            TYPE bapibus1006_address
    ,v_tax_num             LIKE bapibus1006tax-taxnumber
    ,l_taxtype             LIKE bapibus1006tax-taxtype
    ,lw_bankdetail         TYPE bapibus1006_bankdetail
    ,t_bapiadtel                      TYPE STANDARD TABLE OF bapiadtel
    ,lw_bapiadtel                      LIKE LINE OF t_bapiadtel

    ,t_bapiadsmtp                     TYPE STANDARD TABLE OF bapiadsmtp
    ,l_bapiadsmtp                     LIKE LINE OF t_bapiadsmtp
    ,t_bapi_ret                       TYPE STANDARD TABLE OF bapiret2
    ,lw_bapiret2                       TYPE  bapiret2
    ,t_address_duplicates             TYPE STANDARD TABLE OF bapibus1006_address_duplicates
    ,l_address_duplicates             LIKE LINE OF t_address_duplicates
    ,l_lifnr TYPE lifnr
    ,lt_bank TYPE TABLE OF ztMM_mdm_bp_bank
    ,lt_but0bk LIKE TABLE OF but0bk
    ,lw_bank TYPE ZTMM_MDM_BP_BANK
    ,lw_but0bk LIKE but0bk.

  DATA : l_group TYPE bp001-group_d .
  DATA : l_group2 TYPE bp001-group_d .
  CLEAR GS_ALV .
  LOOP AT pt_alv INTO GS_ALV WHERE zzt = '1'.
    CLEAR : GS_ALV-cjzt .
    CLEAR : GS_ALV-message .
*pc_partner_guid ,
    SELECT SINGLE partner INTO  l_lifnr FROM bp001 WHERE partner = GS_ALV-lifnr .
    IF NOT sy-subrc = 0 ."AND .
      REFRESH: t_bapiadtel ,t_bapiadsmtp ,t_bapi_ret ,t_address_duplicates .
      CLEAR:  lw_head ,lw_organ
        ,lw_central ,lw_address ,v_tax_num ,lw_bankdetail ,lw_bapiadtel
        ,l_bapiadsmtp ,l_address_duplicates
        .
*      PERFORM prm_partner .
      lw_head-bpartner = GS_ALV-lifnr .   "Business Partner Number
      lw_head-partn_cat = '2' .    " 1 人员;2  组织;3  组
      lw_head-partn_grp = GS_ALV-bu_group . "Business Partner Grouping

      lw_central-searchterm1 = GS_ALV-sortl .
      lw_central-title_key = '0003' .
*      l_group = GS_ALV-group_d .
*      EXPORT l_group = l_group TO MEMORY ID 'ZMDMRPT_001_IN'.
*
*      IMPORT l_group = l_group2 FROM  MEMORY ID 'ZMDMRPT_001_IN'.
      lw_organ-name1 = GS_ALV-name1(35) .


      lw_address-langu = '1'.  "中国
      lw_address-street = GS_ALV-stras .
      lw_address-region = GS_ALV-regio .
      lw_address-postl_cod1 = GS_ALV-pstlz .
      lw_address-city = GS_ALV-ort01 .
      lw_address-countryiso = GS_ALV-land1 .
*      lw_address-countryiso = GS_ALV-SMTP_ADDR .

      IF GS_ALV-telf1 IS NOT INITIAL .

        REFRESH: t_bapiadtel[].
        CLEAR: lw_bapiadtel.
        lw_bapiadtel-countryiso = GS_ALV-land1 .
        lw_bapiadtel-telephone = GS_ALV-telf1 .
        lw_bapiadtel-valid_from = '18000101000000' .
        lw_bapiadtel-valid_to = '99991231235959' .
        APPEND lw_bapiadtel TO t_bapiadtel[].
      ENDIF.
      IF l_bapiadsmtp-e_mail IS NOT INITIAL .

        l_bapiadsmtp-e_mail = GS_ALV-e_mail .
        l_bapiadsmtp-valid_from = '18000101000000' .
        l_bapiadsmtp-valid_to = '99991231235959' .
        APPEND l_bapiadsmtp TO t_bapiadsmtp[].
      ENDIF.


      CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
        EXPORTING
          businesspartnerextern   = lw_head-bpartner
          partnercategory         = lw_head-partn_cat
          partnergroup            = lw_head-partn_grp
          centraldata             = lw_central
*         centraldataperson       = ls_centraldataperson
          centraldataorganization = lw_organ
*         CENTRALDATAGROUP        =
          addressdata             = lw_address
        IMPORTING
          businesspartner         = l_lifnr
        TABLES
          telefondata             = t_bapiadtel[]
          e_maildata              = t_bapiadsmtp[]
          return                  = t_bapi_ret[]
          addressduplicates       = t_address_duplicates[].
      LOOP AT t_bapi_ret INTO lw_bapiret2 WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X' .
        PERFORM prm_bapimsgt_to_return  USING lw_bapiret2 CHANGING GS_ALV-message .
      ENDLOOP.
      IF 0 = sy-subrc .
*        APPEND LINES OF t_bapi_ret TO gt_bapi_msg[].

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
          .
        GS_ALV-dycs =  GS_ALV-dycs + 1 .
        GS_ALV-cjzt =  'E' .
      ELSE .
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'
*   IMPORTING
*           RETURN        =
          .

        GS_ALV-lifnr = l_lifnr .

      ENDIF.

    ENDIF .

    SELECT
      *
      INTO CORRESPONDING FIELDS OF TABLE lt_bank
      FROM ZTMM_MDM_BP_BANK
    WHERE lifnr = GS_ALV-lifnr .

    SELECT
      *
      INTO TABLE  lt_but0bk
      FROM but0bk
    WHERE partner = GS_ALV-lifnr .

    LOOP AT lt_bank INTO lw_bank.

      READ TABLE lt_but0bk INTO lw_but0bk WITH  KEY partner = lw_bank-lifnr  bkvid = lw_bank-itemno .
      IF NOT sy-subrc = 0 .

        lw_bankdetail-bank_ctry = lw_bank-banks .
        lw_bankdetail-bank_key = lw_bank-bankl .
        IF lw_bankdetail-bank_key IS INITIAL .
          lw_bankdetail-bank_key = lw_bank-bankl1 .
        ENDIF.
        lw_bankdetail-bank_acct = lw_bank-bankn(18) .
        lw_bankdetail-bank_ref = lw_bank-bankn+18(20) .

        CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_ADD'
          EXPORTING
            businesspartner = GS_ALV-lifnr
            bankdetailid    = lw_bank-itemno
            bankdetaildata  = lw_bankdetail
* IMPORTING
*           BANKDETAILIDOUT =
          TABLES
            return          = t_bapi_ret.
        LOOP AT t_bapi_ret INTO lw_bapiret2 WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X' .
          PERFORM prm_bapimsgt_to_return  USING lw_bapiret2 CHANGING GS_ALV-message .
        ENDLOOP.
        IF 0 = sy-subrc .
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
            .
          GS_ALV-dycs =  GS_ALV-dycs + 1 .
          GS_ALV-cjzt =  'E' .
          EXIT .
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'
*   IMPORTING
*             RETURN        =
            .

        ENDIF.
      ENDIF.
    ENDLOOP.
    IF NOT GS_ALV-cjzt =  'E' AND p_rad1 = '' .
      SELECT SINGLE
       taxtype INTO  l_taxtype
        FROM dfkkbptaxnum
        WHERE taxtype = 'CN0'
      AND partner = GS_ALV-lifnr .
      IF sy-subrc NE 0 AND GS_ALV-land1 = 'CN' .
        CLEAR v_tax_num .
        IF NOT GS_ALV-taxnum1 IS INITIAL .
          v_tax_num = GS_ALV-taxnum1 .
        ELSEIF NOT  GS_ALV-taxnum2 IS INITIAL  .
          v_tax_num = GS_ALV-taxnum2 .

        ELSEIF NOT GS_ALV-taxnum3 IS INITIAL.
          v_tax_num = GS_ALV-taxnum3 .

        ENDIF.
        IF NOT v_tax_num IS INITIAL  .

          CALL FUNCTION 'BAPI_BUPA_TAX_ADD'
            EXPORTING
              businesspartner = GS_ALV-lifnr
              taxtype         = 'CN0'
              taxnumber       = v_tax_num
            TABLES
              return          = t_bapi_ret.
          LOOP AT t_bapi_ret INTO lw_bapiret2 WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X' .
            PERFORM prm_bapimsgt_to_return  USING lw_bapiret2 CHANGING GS_ALV-message .
          ENDLOOP.
          IF sy-subrc = 0 .

            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
              .

            GS_ALV-dycs =  GS_ALV-dycs + 1 .
            GS_ALV-cjzt =  'E' .


*        APPEND LINES OF t_bapi_ret TO gt_bapi_msg[].
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'
*   IMPORTING
*               RETURN        =
              .
            GS_ALV-cjzt =  'S' .
            GS_ALV-message = '成功！' .
          ENDIF.
        ENDIF.
*   ENDIF.
      ENDIF.

    ENDIF.
    MODIFY  pt_alv FROM GS_ALV .
    CLEAR GS_ALV .
  ENDLOOP.
  PERFORM prm_role TABLES pt_alv .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PRM_PARTNER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM prm_partner .
  DATA : l_lifnr TYPE lfa1-lifnr .
  SELECT SINGLE partner INTO  l_lifnr FROM bp001 WHERE partner = GS_ALV-lifnr .
  IF sy-subrc = 0 .
    GS_ALV-lifnr = GS_ALV-lifnr + 1 .
    PERFORM prm_partner .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PRM_ROLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM prm_role TABLES pt_alv LIKE gt_alv .
  DATA:
    t_bapiret2  TYPE STANDARD TABLE OF bapiret2,
    lw_bapiret2 TYPE bapiret2,
    lv_role     TYPE bapibus1006_bproles-partnerrole,
    lv_lifnr    TYPE lifnr
    .

  LOOP AT pt_alv INTO GS_ALV WHERE cjzt <> 'E' .
    CLEAR GS_ALV-message .
    IF  GS_ALV-zsfgys = '1'.
      SELECT SINGLE lifnr INTO lv_lifnr FROM lfa1 WHERE lifnr = GS_ALV-lifnr  .
      IF NOT sy-subrc = 0 .

        REFRESH t_bapiret2 .
        CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
          EXPORTING
            businesspartner     = GS_ALV-lifnr
*           BUSINESSPARTNERROLECATEGORY       = 'ZV001'    "业务伙伴供应商 (FS: BP)
*           ALL_BUSINESSPARTNERROLES          = ' '
            businesspartnerrole = 'ZVN001'   "供应商
*           DIFFERENTIATIONTYPEVALUE          =
*           VALIDFROMDATE       =
*           VALIDUNTILDATE      = '99991231'
          TABLES
            return              = t_bapiret2.
        LOOP AT t_bapiret2 INTO lw_bapiret2 WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X' .
          PERFORM prm_bapimsgt_to_return  USING lw_bapiret2 CHANGING GS_ALV-message .
        ENDLOOP.
        IF sy-subrc = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
            .
          GS_ALV-dycs =  GS_ALV-dycs + 1 .
          GS_ALV-cjzt =  'E' .


*    M_COLL_MSG 'E' '000'  P_PARTNER '添加联系人角色错误' '' ''.


*      APPEND LINES OF t_bapiret2 TO gt_bapi_msg[] .

        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true
*     IMPORTING
*             RETURN        =
            .
          GS_ALV-cjzt =  'S' .
          GS_ALV-message = '成功！' .


        ENDIF.
      ENDIF.
    ENDIF.
    IF  GS_ALV-zskf = '1' .
      SELECT SINGLE KUNNR INTO lv_lifnr FROM kna1 WHERE KUNNR = GS_ALV-lifnr  .
      IF NOT sy-subrc = 0 .

        REFRESH t_bapiret2 .
        CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
          EXPORTING
            businesspartner     = GS_ALV-lifnr
*           businesspartnerrolecategory = '1001'    "业务伙伴供应商 (FS: BP)
*           ALL_BUSINESSPARTNERROLES    = ' '
            businesspartnerrole = 'ZCU001'   "客户
*           DIFFERENTIATIONTYPEVALUE    =
*           VALIDFROMDATE       =
*           VALIDUNTILDATE      = '99991231'
          TABLES
            return              = t_bapiret2.
        LOOP AT t_bapiret2 INTO lw_bapiret2 WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X' .
          PERFORM prm_bapimsgt_to_return  USING lw_bapiret2 CHANGING GS_ALV-message .
        ENDLOOP.
        IF sy-subrc = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
            .
          GS_ALV-dycs =  GS_ALV-dycs + 1 .
          GS_ALV-cjzt =  'E' .


*    M_COLL_MSG 'E' '000'  P_PARTNER '添加联系人角色错误' '' ''.


*      APPEND LINES OF t_bapiret2 TO gt_bapi_msg[] .

        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true
*     IMPORTING
*             RETURN        =
            .
          GS_ALV-cjzt =  'S' .
          GS_ALV-message = '成功！' .


        ENDIF.
      ENDIF.
    ENDIF.

    IF  GS_ALV-fox = '1'.
      SELECT SINGLE KUNNR INTO lv_lifnr FROM kna1 WHERE lifnr = GS_ALV-lifnr  .
      IF NOT sy-subrc = 0 .

        REFRESH t_bapiret2 .
        CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
          EXPORTING
            businesspartner     = GS_ALV-lifnr
*           BUSINESSPARTNERROLECATEGORY       = 'ZV001'    "业务伙伴供应商 (FS: BP)
*           ALL_BUSINESSPARTNERROLES          = ' '
            businesspartnerrole = 'ZYG001'   "供应商
*           DIFFERENTIATIONTYPEVALUE          =
*           VALIDFROMDATE       =
*           VALIDUNTILDATE      = '99991231'
          TABLES
            return              = t_bapiret2.
        LOOP AT t_bapiret2 INTO lw_bapiret2 WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X' .
          PERFORM prm_bapimsgt_to_return  USING lw_bapiret2 CHANGING GS_ALV-message .
        ENDLOOP.
        IF sy-subrc = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
            .
          GS_ALV-dycs =  GS_ALV-dycs + 1 .
          GS_ALV-cjzt =  'E' .


*    M_COLL_MSG 'E' '000'  P_PARTNER '添加联系人角色错误' '' ''.


*      APPEND LINES OF t_bapiret2 TO gt_bapi_msg[] .

        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true
*     IMPORTING
*             RETURN        =
            .
          GS_ALV-cjzt =  'S' .
          GS_ALV-message = '成功！' .


        ENDIF.
      ENDIF.
    ENDIF .
    IF  GS_ALV-fox NE '1'.
      UPDATE
        bp001
        SET group_d =  GS_ALV-group_d
        WHERE partner = GS_ALV-lifnr .
      IF sy-subrc = 0 .
        COMMIT WORK AND WAIT .
      ELSE .
        ROLLBACK WORK .
      ENDIF.
    ENDIF .
    IF GS_ALV-fox = '1' .
      IF GS_ALV-cjzt = 'E'.

        GS_ALV-zxbs = icon_red_light.
      ELSE .
        GS_ALV-zxbs = icon_green_light.

      ENDIF.
    ENDIF.
    IF GS_ALV-cjzt NE 'E'.

      GS_ALV-cjzt =  'S' .
      GS_ALV-message = '成功！' .
    ENDIF.
    MODIFY  pt_alv FROM GS_ALV .
    CLEAR GS_ALV .
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BAPIMSGT_TO_RETURN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_RETURN  text
*      -->P_GS_return_TYPE  text
*      -->P_GS_return_ID  text
*      -->P_GS_return_NUMBER  text
*      -->P_GS_return_MESSAGE_V1  text
*      -->P_GS_return_MESSAGE_V2  text
*      -->P_GS_return_MESSAGE_V3  text
*      -->P_GS_return_MESSAGE_V4  text
*----------------------------------------------------------------------*
FORM prm_bapimsgt_to_return
*                         TABLES   p_t_return STRUCTURE bapiret2
                         USING    p_returne TYPE bapiret2
                         CHANGING p_message TYPE char255.
  CLEAR p_message .
  DATA: GS_return TYPE bapiret2.

  CLEAR:GS_return.

  MESSAGE
  ID p_returne-id
  TYPE p_returne-type
  NUMBER p_returne-number
  WITH p_returne-message_v1 p_returne-message_v2 p_returne-message_v3 p_returne-message_v4
  INTO p_returne-message.
  p_message = p_message && p_returne-message .
*  APPEND GS_return TO p_t_return.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form PFM_BP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM pfm_bp .
  PERFORM pfm_create_bp  TABLES gt_alv ."创建的选项
  PERFORM pfm_change_bp TABLES gt_alv ."修改选择
  PERFORM pfm_nodel_bp TABLES gt_alv ."启用/停用
*  IF sy-batch = 'X'.
  IF p_rad1 = ''.

    PERFORM pfm_save_log ."更新日志表
  ENDIF.
*  ENDIF.
  IF p_rad1 = 'X'.

    PERFORM pfm_kzgs .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PFM_CHANGE_BP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_ALV
*&---------------------------------------------------------------------*
FORM pfm_change_bp  TABLES   pt_alv LIKE gt_alv.
  DATA : v_tax_num      LIKE bapibus1006tax-taxnumber,
         t_bapi_ret     TYPE TABLE OF bapiret2,
         lw_bapiret2    TYPE bapiret2,
         lw_bankdetail  TYPE bapibus1006_bankdetail,
         lw_bankdetailx TYPE bapibus1006_bankdetail_x
         .
  DATA : lt_bank       TYPE TABLE OF ZTMM_MDM_BP_BANK,
         lw_bank       TYPE ZTMM_MDM_BP_BANK,
         lt_but0bk     TYPE TABLE OF but0bk,
         lw_but0bk     TYPE but0bk,
         lw_head       TYPE bapibus1006_head,
         lw_organ      TYPE bapibus1006_central_organ,
         lw_organx     TYPE bapibus1006_central_organ_x,
         lw_central    TYPE bapibus1006_central,
         lw_centralx   TYPE bapibus1006_central_x,
         lw_address    TYPE bapibus1006_address,
         lw_addressx   TYPE bapibus1006_address_x,
         t_bapiadtel   TYPE STANDARD TABLE OF bapiadtel,
         lw_bapiadtel  LIKE LINE OF t_bapiadtel,
         t_bapiadtelx  TYPE STANDARD TABLE OF bapiadtelx,
         lw_bapiadtelx TYPE  bapiadtelx.

  LOOP AT pt_alv INTO GS_ALV WHERE zzt = '2' .
    CLEAR : GS_ALV-cjzt .
    CLEAR : GS_ALV-message .
    CLEAR : v_tax_num .
    IF NOT GS_ALV-taxnum1 IS INITIAL .
      v_tax_num = GS_ALV-taxnum1 .
    ELSEIF NOT  GS_ALV-taxnum2 IS INITIAL  .
      v_tax_num = GS_ALV-taxnum2 .

    ELSEIF NOT GS_ALV-taxnum3 IS INITIAL.
      v_tax_num = GS_ALV-taxnum3 .

    ENDIF.
    IF NOT   v_tax_num IS INITIAL  AND GS_ALV-land1 = 'CN' .

      CALL FUNCTION 'BAPI_BUPA_TAX_CHANGE'
        EXPORTING
          businesspartner = GS_ALV-lifnr
          taxtype         = 'CN0'
          taxnumber       = v_tax_num
        TABLES
          return          = t_bapi_ret.
      LOOP AT t_bapi_ret INTO lw_bapiret2 WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X' .
        PERFORM prm_bapimsgt_to_return  USING lw_bapiret2 CHANGING GS_ALV-message .
      ENDLOOP.
      IF sy-subrc = 0 .

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
          .

        GS_ALV-dycs =  GS_ALV-dycs + 1 .
        GS_ALV-cjzt =  'E' .


*        APPEND LINES OF t_bapi_ret TO gt_bapi_msg[].
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'
*   IMPORTING
*           RETURN        =
          .
      ENDIF.

    ENDIF.
    IF NOT  GS_ALV-cjzt =  'E' .
      SELECT
        *
        INTO TABLE lt_bank
        FROM ZTMM_MDM_BP_BANK
      WHERE lifnr = GS_ALV-lifnr .

      SELECT
        *
        INTO TABLE  lt_but0bk
        FROM but0bk
      WHERE partner = GS_ALV-lifnr .

      LOOP AT lt_bank INTO lw_bank.

        READ TABLE lt_but0bk INTO lw_but0bk WITH  KEY partner = lw_bank-lifnr  bkvid = lw_bank-itemno .
        IF NOT sy-subrc = 0 .

          lw_bankdetail-bank_ctry = 'CN'."PL_GOAL-BANKS .
          lw_bankdetail-bank_key = lw_bank-bankl .
          IF lw_bankdetail-bank_key IS INITIAL .
            lw_bankdetail-bank_key = lw_bank-bankl1 .
          ENDIF.
          lw_bankdetail-bank_acct = lw_bank-bankn(18) .
          lw_bankdetail-bank_ref = lw_bank-bankn+18(20) .

          CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_ADD'
            EXPORTING
              businesspartner = GS_ALV-lifnr
              bankdetailid    = lw_bank-itemno
              bankdetaildata  = lw_bankdetail
* IMPORTING
*             BANKDETAILIDOUT =
            TABLES
              return          = t_bapi_ret.
          LOOP AT t_bapi_ret INTO lw_bapiret2 WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X' .
            PERFORM prm_bapimsgt_to_return  USING lw_bapiret2 CHANGING GS_ALV-message .
          ENDLOOP.
          IF 0 = sy-subrc .
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
              .
            GS_ALV-dycs =  GS_ALV-dycs + 1 .
            GS_ALV-cjzt =  'E' .
            EXIT .
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'
*   IMPORTING
*               RETURN        =
              .

          ENDIF.
        ELSE .
          DELETE lt_but0bk WHERE partner = lw_bank-lifnr AND bkvid = lw_bank-itemno .


          lw_bankdetail-bank_ctry = lw_bank-BANKS .
          lw_bankdetail-bank_key = lw_bank-bankl .
          IF lw_bankdetail-bank_key IS INITIAL .
            lw_bankdetail-bank_key = lw_bank-bankl1 .
          ENDIF.
          lw_bankdetail-bank_acct = lw_bank-bankn(18) .
          lw_bankdetail-bank_ref = lw_bank-bankn+18(20) .

          lw_bankdetailx-bank_ctry = 'X'."'CN'."PL_GOAL-BANKS .
          lw_bankdetailx-bank_key = 'X'."lw_bank-bankl .
          lw_bankdetailx-bank_acct = 'X'."lw_bank-bankn(18) .
          lw_bankdetailx-bank_ref = 'X'."lw_bank-bankn+18(20) .
          CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_CHANGE'
            EXPORTING
              businesspartner  = lw_bank-lifnr
              bankdetailid     = lw_bank-itemno
              bankdetaildata   = lw_bankdetail
              bankdetaildata_x = lw_bankdetailx
            TABLES
              return           = t_bapi_ret.
          LOOP AT t_bapi_ret INTO lw_bapiret2 WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X' .
            PERFORM prm_bapimsgt_to_return  USING lw_bapiret2 CHANGING GS_ALV-message .
          ENDLOOP.
          IF 0 = sy-subrc .
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
              .
            GS_ALV-dycs =  GS_ALV-dycs + 1 .
            GS_ALV-cjzt =  'E' .
            EXIT .
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'
*   IMPORTING
*               RETURN        =
              .

          ENDIF.
        ENDIF.
      ENDLOOP.
      LOOP AT lt_but0bk INTO lw_but0bk .
        CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_REMOVE'
          EXPORTING
            businesspartner = lw_bank-lifnr
            bankdetailid    = lw_bank-itemno
          TABLES
            return          = t_bapi_ret.
        LOOP AT t_bapi_ret INTO lw_bapiret2 WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X' .
          PERFORM prm_bapimsgt_to_return  USING lw_bapiret2 CHANGING GS_ALV-message .
        ENDLOOP.
        IF 0 = sy-subrc .
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
            .
          GS_ALV-dycs =  GS_ALV-dycs + 1 .
          GS_ALV-cjzt =  'E' .
          EXIT .
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'
*   IMPORTING
*             RETURN        =
            .

        ENDIF.
      ENDLOOP.
    ENDIF .
    IF NOT  GS_ALV-cjzt =  'E' .
      CLEAR lw_head .
      CLEAR lw_central .
      CLEAR lw_centralx .
      CLEAR lw_organ .
      CLEAR lw_organx .
      CLEAR lw_address .
      lw_head-bpartner = GS_ALV-lifnr .   "Business Partner Number

      lw_central-searchterm1 = GS_ALV-sortl .
      lw_central-title_key = '0003' .

      lw_centralx-searchterm1 = 'X' ." GS_ALV-sortl .
      lw_centralx-title_key = 'X' .                         " '0003' .
      lw_organ-name1 = GS_ALV-name1(35) .
      lw_organx-name1 = 'X' ."  GS_ALV-name1(35) .



      CALL FUNCTION 'BAPI_BUPA_CENTRAL_CHANGE'
        EXPORTING
          businesspartner           = lw_head-bpartner
          centraldata               = lw_central
          centraldataorganization   = lw_organ
          centraldata_x             = lw_centralx
          centraldataorganization_x = lw_organx
*         DUPLICATE_MESSAGE_TYPE    =
          duplicate_check_address   = lw_address
        TABLES
*         telefondatanonaddress     = t_bapiadtel
*         telefondatanonaddressx    = t_bapiadtelx
          return                    = t_bapi_ret.
      LOOP AT t_bapi_ret INTO lw_bapiret2 WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X' .
        PERFORM prm_bapimsgt_to_return  USING lw_bapiret2 CHANGING GS_ALV-message .
      ENDLOOP.
      IF 0 = sy-subrc .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
          .
        GS_ALV-dycs =  GS_ALV-dycs + 1 .
        GS_ALV-cjzt =  'E' .
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'
*   IMPORTING
*           RETURN        =
          .
        GS_ALV-cjzt =  'S' .
        GS_ALV-message = '成功！' .
        REFRESH t_bapi_ret .
      ENDIF.
    ENDIF.


    IF GS_ALV-cjzt NE  'E'.



      lw_address-langu = '1'.  "中国
*      lw_address-standardaddress = 'X' ."GS_ALV-stras '.
      lw_address-street = GS_ALV-stras .
      lw_address-region = GS_ALV-regio .
      lw_address-postl_cod1 = GS_ALV-pstlz .
      lw_address-city = GS_ALV-ort01 .
      lw_address-countryiso = GS_ALV-land1 .


      lw_addressx-langu = '1'.  "中国
*      lw_addressx-standardaddress = 'X' ."GS_ALV-stras '.
      lw_addressx-street = 'X' ."GS_ALV-stras .
      lw_addressx-region = 'X' ."GS_ALV-region .
      lw_addressx-postl_cod1 = 'X' ."GS_ALV-pstlz .
      lw_addressx-city = 'X' ."GS_ALV-ort01 .
      lw_addressx-countryiso = 'X' ."GS_ALV-land1 .

      REFRESH: t_bapiadtel[].
      CLEAR: lw_bapiadtel.
      lw_bapiadtel-countryiso = GS_ALV-land1 .
      lw_bapiadtel-telephone = GS_ALV-telf1 .
*      lw_bapiadtel-valid_from = '18000101000000' .
*      lw_bapiadtel-valid_to = '99991231235959' .
      APPEND lw_bapiadtel TO t_bapiadtel[].


      REFRESH: t_bapiadtelx[].
      CLEAR: lw_bapiadtelx.
      lw_bapiadtelx-countryiso = 'X' ." GS_ALV-land1 .
      lw_bapiadtelx-telephone = 'X' ."GS_ALV-telf1 .
*      lw_bapiadtelx-valid_from = 'X' ."'18000101000000' .
*      lw_bapiadtelx-valid_to = 'X' ." '99991231235959' .
      APPEND lw_bapiadtelx TO t_bapiadtelx[].

      CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
        EXPORTING
          businesspartner = GS_ALV-lifnr
*         ADDRESSGUID     =
          addressdata     = lw_address
          addressdata_x   = lw_addressx
*         DUPLICATE_MESSAGE_TYPE       =
*         ACCEPT_ERROR    = ' '
        TABLES
          bapiadtel       = t_bapiadtel
*         bapiadfax       =
*         BAPIADTTX       =
*         BAPIADTLX       =
*         BAPIADSMTP      =
*         BAPIADRML       =
*         BAPIADX400      =
*         BAPIADRFC       =
*         BAPIADPRT       =
*         BAPIADSSF       =
*         BAPIADURI       =
*         BAPIADPAG       =
*         BAPIAD_REM      =
*         BAPICOMREM      =
*         ADDRESSUSAGE    =
*         BAPIADVERSORG   =
*         BAPIADVERSPERS  =
*         BAPIADUSE       =
          bapiadtel_x     = t_bapiadtelx
*         BAPIADFAX_X     =
*         BAPIADTTX_X     =
*         BAPIADTLX_X     =
*         BAPIADSMT_X     =
*         BAPIADRML_X     =
*         BAPIADX40_X     =
*         BAPIADRFC_X     =
*         BAPIADPRT_X     =
*         BAPIADSSF_X     =
*         BAPIADURI_X     =
*         BAPIADPAG_X     =
*         BAPIAD_RE_X     =
*         BAPICOMRE_X     =
*         ADDRESSUSAGE_X  =
*         BAPIADVERSORG_X =
*         BAPIADVERSPERS_X             =
*         BAPIADUSE_X     =
          return          = t_bapi_ret
*         ADDRESSDUPLICATES            =
        .
      LOOP AT t_bapi_ret INTO lw_bapiret2 WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X' .
        PERFORM prm_bapimsgt_to_return  USING lw_bapiret2 CHANGING GS_ALV-message .
      ENDLOOP.
      IF 0 = sy-subrc .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
          .
        GS_ALV-dycs =  GS_ALV-dycs + 1 .
        GS_ALV-cjzt =  'E' .
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'
*   IMPORTING
*           RETURN        =
          .
        GS_ALV-cjzt =  'S' .
        GS_ALV-message = '成功！' .

      ENDIF.
    ENDIF.
    UPDATE
      bp001
      SET group_d =  GS_ALV-group_d
      WHERE partner = GS_ALV-lifnr .
    IF sy-subrc = 0 .
      COMMIT WORK AND WAIT .
    ELSE .
      ROLLBACK WORK .
    ENDIF.
    MODIFY pt_alv FROM GS_ALV .
    CLEAR GS_ALV .
  ENDLOOP.
  IF sy-subrc = 0 .

    PERFORM prm_role TABLES pt_alv .

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PFM_NODEL_BP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_ALV
*&---------------------------------------------------------------------*
FORM pfm_nodel_bp  TABLES   pt_alv LIKE gt_alv.
  DATA : lw_main TYPE cmds_ei_main .
  DATA : lw_mainl TYPE vmds_ei_main .
  DATA : lw_lifnr TYPE vmds_ei_extern .
  DATA : lt_lifnr TYPE TABLE OF  vmds_ei_extern .
  DATA : lw_msgd TYPE cvis_message .
  DATA : lw_msgc TYPE cvis_message .
  DATA : lt_msgs TYPE bapiret2_t .
  DATA : lw_msgs TYPE bapiret2 .
  DATA : lw_extern TYPE cmds_ei_extern .
  DATA : lt_extern TYPE TABLE OF cmds_ei_extern .
  CLEAR GS_ALV .
  LOOP AT pt_alv INTO GS_ALV WHERE zzt = '3'  OR zzt = '4'  .


    IF GS_ALV-zskf  = '1'.
      lw_extern-header-object_instance-kunnr = GS_ALV-lifnr .
      lw_extern-header-object_task = 'M' .
      IF GS_ALV-zzt = '3'  .

        lw_extern-central_data-central-data-nodel = '' .
      ELSE .
        lw_extern-central_data-central-data-nodel = 'X' .

      ENDIF.

      lw_extern-central_data-central-datax-nodel  = 'X' .

      APPEND lw_extern TO lt_extern .
      lw_main-customers = lt_extern .
      CALL METHOD cmd_ei_api=>maintain_bapi
        EXPORTING
*         iv_test_run          = SPACE
*         iv_collect_messages  = SPACE
          is_master_data       = lw_main
        IMPORTING
*         es_master_data_correct   =
          es_message_correct   = lw_msgc
*         es_master_data_defective =
          es_message_defective = lw_msgd.
      lt_msgs[] = lw_msgd-messages[] .
      LOOP AT lt_msgs INTO lw_msgs WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X' .
        PERFORM prm_bapimsgt_to_return  USING lw_msgs CHANGING GS_ALV-message .

      ENDLOOP.
      IF sy-subrc = 0 .

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
        GS_ALV-dycs =  GS_ALV-dycs + 1 .
        GS_ALV-cjzt =  'E' .
      ELSE .

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        GS_ALV-cjzt =  'S' .
        GS_ALV-message = '成功！' .
      ENDIF.
    ENDIF.

    IF GS_ALV-zsfgys = '1' .
      lw_lifnr-header-object_instance-lifnr = GS_ALV-lifnr .
      lw_lifnr-header-object_task = 'M' .
      IF GS_ALV-zzt = '3'  .

        lw_lifnr-central_data-central-data-nodel = '' .
      ELSE .
        lw_lifnr-central_data-central-data-nodel = 'X' .

      ENDIF.

      lw_lifnr-central_data-central-datax-nodel  = 'X' .

      APPEND lw_lifnr TO lt_lifnr .
      lw_mainl-vendors = lt_lifnr .
      CALL METHOD vmd_ei_api=>maintain_bapi
        EXPORTING
*         iv_test_run          = SPACE
*         iv_collect_messages  = SPACE
          is_master_data       = lw_mainl
        IMPORTING
*         es_master_data_correct   =
          es_message_correct   = lw_msgc
*         es_master_data_defective =
          es_message_defective = lw_msgd.
      lt_msgs[] = lw_msgd-messages[] .
      LOOP AT lt_msgs INTO lw_msgs WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X' .
        PERFORM prm_bapimsgt_to_return  USING lw_msgs CHANGING GS_ALV-message .

      ENDLOOP.
      IF sy-subrc = 0 .

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
        GS_ALV-dycs =  GS_ALV-dycs + 1 .
        GS_ALV-cjzt =  'E' .
      ELSE .

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        GS_ALV-cjzt =  'S' .
        GS_ALV-message = '成功！' .
      ENDIF.
    ENDIF .
    MODIFY pt_alv FROM GS_ALV .
    CLEAR GS_ALV .
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PFM_SAVE_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM pfm_save_log .
  DATA : lt_log TYPE TABLE OF  ztMM_MDM_BP_LOG,
         lw_log TYPE ztMM_MDM_BP_LOG.
  REFRESH lt_log .
  LOOP AT gt_alv INTO GS_ALV .
    MOVE-CORRESPONDING GS_ALV TO lw_log .
    IF lw_log-CJZT = ''.
      lw_log-CJZT = 'S' .
      lw_log-MESSAGE = '更新成功!' .
    ENDIF.
    APPEND lw_log TO lt_log .
    CLEAR GS_ALV .
    CLEAR lw_log .
  ENDLOOP.

  MODIFY ztMM_MDM_BP_LOG FROM TABLE lt_log .
  IF sy-subrc = 0 .
    COMMIT WORK .
  ELSE .
    ROLLBACK WORK .
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_DOWN_DATA
*&---------------------------------------------------------------------*
*       获取模版
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_down_data .
  DATA: lv_fname TYPE rlgrap-filename, ftype TYPE rlgrap-filetype.

  DATA: lo_objdata     LIKE wwwdatatab,
        lo_mime        LIKE w3mime,
        lc_filename    TYPE string VALUE  '导入模板.xls',
        lc_fullpath    TYPE string  VALUE 'C:\',
        lc_path        TYPE  string VALUE 'C:\',
        ls_destination LIKE rlgrap-filename,
        ls_objnam      TYPE string,
        li_rc          LIKE sy-subrc,
        ls_errtxt      TYPE string.
  DATA: p_objid TYPE wwwdatatab-objid,
        p_dest  LIKE sapb-sappfad.

  p_objid = 'ZMDMRPT_001'.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_extension    = 'XLS'
      default_file_name    = lc_filename
      file_filter          = '文本文件(*.TXT)|*.TXT|Excel 文件 (*.XLSX)|*.XLS;*.XLSX|所有文件 (*.*)|*.*|'
    CHANGING
      filename             = lc_filename
      path                 = lc_path
      fullpath             = lc_fullpath
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF lc_fullpath = ''.
    MESSAGE e000 WITH '不能打开excel' ."TYPE 'E'.
  ENDIF.

  IF sy-subrc = 0.
    p_dest = lc_fullpath.
    CONDENSE ls_objnam NO-GAPS.

    SELECT SINGLE relid objid
    FROM wwwdata
    INTO CORRESPONDING FIELDS OF lo_objdata
    WHERE srtf2 = 0
    AND relid = 'MI'
    AND objid = p_objid.

    IF sy-subrc NE 0 OR lo_objdata-objid EQ space.
      CONCATENATE '模板文件' ls_objnam '不存在' INTO ls_errtxt.
      MESSAGE i000 WITH ls_errtxt ."TYPE 'I'.
    ENDIF.

    ls_destination = p_dest.

    CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
      EXPORTING
        key         = lo_objdata
        destination = ls_destination
      IMPORTING
        rc          = li_rc.
    IF li_rc NE 0.
      CONCATENATE '模板文件:' ls_objnam '下载失败' INTO ls_errtxt.
      MESSAGE e000 WITH ls_errtxt ."TYPE 'E'.
    ELSE.
      MESSAGE s000 WITH '下载成功' ."TYPE 'S'.
    ENDIF.
    lv_fname = ls_destination.
  ENDIF.
ENDFORM .
*&---------------------------------------------------------------------*
*&      Form  frm_tidy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_tidy.
  DATA : BEGIN OF typ_matnr  ,
           matnr TYPE matnr,
           werks TYPE werks_d,
           lgort TYPE lgort_d,
         END OF typ_matnr .
  DATA : l_menge TYPE menge_d .
  DATA : l_menge1 TYPE menge_d .
  DATA : l_meins TYPE meins .
  DATA : l_meins1 TYPE meins .
  DATA : lt_matnr LIKE TABLE OF typ_matnr WITH HEADER LINE  .
  DATA : it_marc TYPE TABLE OF marc WITH HEADER LINE .
  DATA : it_makt TYPE TABLE OF makt WITH HEADER LINE .
  DATA : it_mard TYPE TABLE OF mard WITH HEADER LINE .
  DATA : it_mara TYPE TABLE OF mara WITH HEADER LINE .
  DATA : it_mast TYPE TABLE OF mast WITH HEADER LINE .
  DATA : i_quantity TYPE string .
*从已知文件名读入内表
  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 7
      i_end_col               = 13
      i_end_row               = 65535
    TABLES
      intern                  = gt_cells[]
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    MESSAGE e000 WITH  '打开文件错误，请检查文件，确保关闭文件!' ."TYPE 'E'.
    STOP.
  ENDIF.
  LOOP AT gt_cells ASSIGNING <fs_cell> .
    CASE <fs_cell>-col.
      WHEN  1 .
        GS_ALV-lifnr  = <fs_cell>-value   ."
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = GS_ALV-lifnr
          IMPORTING
            output = GS_ALV-lifnr.
      WHEN  2 .
        GS_ALV-name1  = <fs_cell>-value   ."
      WHEN  3 .
        GS_ALV-name2  = <fs_cell>-value   ."
      WHEN  4 .
        GS_ALV-sortl  = <fs_cell>-value   ."
      WHEN  5 .
        GS_ALV-land1  = <fs_cell>-value   ."
        IF GS_ALV-land1 = ''.
          GS_ALV-land1 = 'CN' .
        ENDIF.
      WHEN 6 .
        GS_ALV-stras  = <fs_cell>-value   ."街道

      WHEN  7 .
        GS_ALV-pstlz  = <fs_cell>-value   ."邮政编码

      WHEN  8 .
        GS_ALV-ort01  = <fs_cell>-value   ."城市
      WHEN  9 .
        GS_ALV-telf1  = <fs_cell>-value   ."
      WHEN  10 .
        GS_ALV-e_mail  = <fs_cell>-value   ."
      WHEN  11 .
        GS_ALV-bukrs  = <fs_cell>-value   ."
      WHEN  12.
        GS_ALV-akont  = <fs_cell>-value   ."
      WHEN  13 .
        GS_ALV-zterm  = <fs_cell>-value   ."
      WHEN OTHERS.
    ENDCASE.

    AT END OF row.
      GS_ALV-zzt = '1' .
      GS_ALV-bu_group = 'Z003' .
      GS_ALV-fox = '1' .
      APPEND  GS_ALV TO gt_alv .
      CLEAR GS_ALV .
    ENDAT.
  ENDLOOP.
ENDFORM. "frm_tidy
*&---------------------------------------------------------------------*
*& Form PFM_KZGS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM pfm_kzgs .


  DATA : lw_main TYPE cmds_ei_main .
  DATA : lw_msgd TYPE cvis_message .
  DATA : lw_msgc TYPE cvis_message .
  DATA : lt_msgs TYPE bapiret2_t .
  DATA : lw_msgs TYPE bapiret2 .
  DATA : lw_extern TYPE cmds_ei_extern .
  DATA : lt_company TYPE cmds_ei_company_t .
  DATA : lw_company TYPE cmds_ei_company .
  DATA : lw_cmd TYPE cmds_ei_cmd_company .
  DATA : lt_extern TYPE TABLE OF cmds_ei_extern .
  REFRESH lt_extern .
  LOOP AT gt_alv INTO GS_ALV.
    lw_extern-header-object_instance-kunnr = GS_ALV-lifnr .
    lw_extern-header-object_task = 'U' .
    REFRESH lw_extern-company_data-company .
    CLEAR lw_company-data .
    CLEAR lw_company-data_key .
    lw_company-task = 'I' .
    lw_company-data_key-bukrs = GS_ALV-bukrs .
    lw_company-data-akont = GS_ALV-akont .
    lw_company-data-zterm = GS_ALV-zterm .
    CLEAR lw_company-datax .
    lw_company-datax-akont = 'X' . "GS_ALV-akont .
    lw_company-datax-zterm = 'X' ."GS_ALV-zterm .
    REFRESH lt_company .
    APPEND lw_company TO lt_company .
    lw_extern-company_data-company  =  lt_company .
    lw_extern-company_data-current_state  =  'X' .
    APPEND lw_extern TO lt_extern .
    lw_main-customers = lt_extern .

    CALL METHOD cmd_ei_api=>maintain_bapi
      EXPORTING
*       iv_test_run          = SPACE
*       iv_collect_messages  = SPACE
        is_master_data       = lw_main
      IMPORTING
*       es_master_data_correct   =
        es_message_correct   = lw_msgc
*       es_master_data_defective =
        es_message_defective = lw_msgd.
    lt_msgs[] = lw_msgd-messages[] .
    LOOP AT lt_msgs INTO lw_msgs WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X' .
      PERFORM prm_bapimsgt_to_return  USING lw_msgs CHANGING GS_ALV-message .
    ENDLOOP.
    IF sy-subrc = 0 .

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
*        GS_ALV-dycs =  GS_ALV-dycs + 1 .
      GS_ALV-cjzt =  'E' .
    ELSE .

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      GS_ALV-cjzt =  'S' .
      GS_ALV-message = '成功！' .
    ENDIF.
    IF GS_ALV-fox = '1' .
      IF GS_ALV-cjzt = 'E'.

        GS_ALV-zxbs = icon_red_light.
      ELSE .
        GS_ALV-zxbs = icon_green_light.

      ENDIF.
    ENDIF.
    REFRESH lt_msgs[] .
    REFRESH lw_msgd-messages[] .
    MODIFY gt_alv FROM GS_ALV .
    CLEAR GS_ALV .

  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  frm_open_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_open_file.

  DATA: l_rc        TYPE i,
        l_filetable TYPE filetable,
        lw_file     TYPE file_table.

  DATA: lv_file(1024) TYPE c.
  DATA: lv_type(8) TYPE c.
  CONSTANTS:
   cns_open_filter TYPE string VALUE 'Microsoft Excel文件(*.xls;*.xlsx)|*.xls;*.xlsx|(*.*)|*.*|'.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = '请选择导入文件'
      file_filter             = cns_open_filter
      multiselection          = space
    CHANGING
      file_table              = l_filetable
      rc                      = l_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.


  IF sy-subrc = 0 AND l_rc = 1.
    READ TABLE l_filetable INTO lw_file INDEX 1.
    p_file = lw_file-filename .
  ENDIF .
ENDFORM. "frm_open_file

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
