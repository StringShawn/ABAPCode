*----------------------------------------------------------------------*
***INCLUDE LZMM_PR_SAP2OAF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_CREATE_PR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_create_pr TABLES p_input STRUCTURE zspr_sap2oa
                   CHANGING p_msg TYPE char255
                            p_msgtype TYPE bapi_mtype.
  DATA:
    ls_header             TYPE bapimereqheader,
    ls_headerx            TYPE bapimereqheaderx,
    lt_prheadertext       TYPE STANDARD TABLE OF bapimereqheadtext,
    ls_prheadertext       TYPE bapimereqheadtext,
    lt_item               TYPE STANDARD TABLE OF bapimereqitemimp,
    ls_item               TYPE bapimereqitemimp,
    lt_itemx              TYPE STANDARD TABLE OF bapimereqitemx,
    ls_itemx              TYPE bapimereqitemx,
    lt_pritemexp          TYPE STANDARD TABLE OF bapimereqitem,
    ls_pritemexp          TYPE bapimereqitem,
    lt_pritemtext         TYPE STANDARD TABLE OF bapimereqitemtext,
    ls_pritemtext         TYPE bapimereqitemtext,
    lt_praccount          TYPE STANDARD TABLE OF bapimereqaccount,
    ls_praccount          TYPE bapimereqaccount,
    lt_praccountx         TYPE STANDARD TABLE OF bapimereqaccountx,
    ls_praccountx         TYPE bapimereqaccountx,
    lt_allversions        TYPE STANDARD TABLE OF bapimedcm_allversions,
    ls_allversions        TYPE bapimedcm_allversions,
    lt_extensionin        TYPE STANDARD TABLE OF bapiparex,
    ls_extensionin        TYPE bapiparex,
    lt_bapi_te_mereqitem  TYPE STANDARD TABLE OF bapi_te_mereqitem,
    ls_bapi_te_mereqitem  TYPE bapi_te_mereqitem,
    lt_bapi_te_mereqitemx TYPE STANDARD TABLE OF bapi_te_mereqitemx,
    ls_bapi_te_mereqitemx TYPE bapi_te_mereqitemx,
    lt_return             TYPE STANDARD TABLE OF bapiret2,
    ls_return             TYPE bapiret2.

  DATA: lv_id   LIKE icon-id,        "状态
        lv_mess TYPE bapi_msg.

*&---分割文本
  DATA:
    lv_txt    TYPE string,
    lv_length TYPE n LENGTH 4,
    lv_num    TYPE n LENGTH 4,
    lv_count  TYPE n LENGTH 4,
    lv_start  TYPE n LENGTH 4.


  DATA:
    lv_banfn TYPE eban-banfn.

  DATA:lt_input TYPE TABLE OF zspr_sap2oa.
  DATA:ls_input TYPE zspr_sap2oa.




*&---每次计算需要导入的采购申请个数
  CLEAR:lt_input.
  lt_input = p_input[].

  SORT lt_input BY  ebelp .

  LOOP AT lt_input INTO ls_input.

*&---填充行项目数据
    CLEAR:ls_item,ls_itemx.
    ls_item-preq_item   = ls_input-ebelp. "采购申请行项目
    ls_itemx-preq_item  = ls_input-ebelp.
    ls_itemx-preq_itemx = 'X'.


    IF ls_input-knttp IS NOT INITIAL.
      ls_item-acctasscat  = ls_input-knttp. "账户分配类别
      ls_itemx-acctasscat = 'X'.
    ENDIF.

    IF ls_input-pstyp IS NOT INITIAL.
      ls_item-item_cat    = ls_input-pstyp. "项目类别文本
      ls_itemx-item_cat   = 'X'.
    ENDIF.


    IF ls_input-matnr IS NOT INITIAL.
      ls_item-material    = ls_input-matnr. "物料编号
      ls_itemx-material   = 'X'.
    ENDIF.

    IF ls_input-menge IS NOT INITIAL.
      ls_item-quantity    = ls_input-menge. "数量
      ls_itemx-quantity   = 'X'.
    ENDIF.

    IF ls_input-meins IS NOT INITIAL.
      ls_item-unit        = ls_input-meins. "申请单位（基本单位）
      ls_itemx-unit       = 'X'.
    ENDIF.

    IF ls_input-lfdat IS NOT INITIAL.
      ls_item-deliv_date  = ls_input-lfdat. "交货日期
      ls_itemx-deliv_date = 'X'.
    ENDIF.

    IF ls_input-frgdt IS NOT INITIAL.
      ls_item-rel_date = ls_input-frgdt.   "批准日期
      ls_itemx-rel_date = 'X'.
    ENDIF.

*    IF ls_input-lpein IS NOT INITIAL.
*      ls_item-del_datcat_ext = ls_input-lpein. "交货日期类别
*      ls_itemx-del_datcat_ext = 'X'.
*    ENDIF.

    IF ls_input-werks IS NOT INITIAL .
      ls_item-plant       = ls_input-werks. "工厂
      ls_itemx-plant      = 'X'.
    ENDIF.

*    IF ls_input-ekorg IS NOT INITIAL.
*      ls_item-purch_org   = ls_input-ekorg. "采购组织
*      ls_itemx-purch_org  = 'X'.
*    ENDIF.

*    IF ls_input-afnam IS NOT INITIAL.
*      ls_item-preq_name   = ls_input-afnam. "申请者
*      ls_itemx-preq_name  = 'X'.
*    ENDIF.

    IF ls_input-badat IS NOT INITIAL.
      ls_item-preq_date   = ls_input-badat. "请求日期
      ls_itemx-preq_date  = 'X'.
    ENDIF.

*    IF ls_input-flief IS NOT INITIAL.
*      ls_item-fixed_vend   = ls_input-flief. "固定供应商
*      ls_itemx-fixed_vend  = 'X'.
*    ENDIF.

    IF ls_input-preis IS NOT INITIAL.
      ls_item-preq_price  = ls_input-preis. "评估价格
      ls_itemx-preq_price = 'X'.
    ENDIF.

    IF ls_input-peinh IS NOT INITIAL.
      ls_item-price_unit  = ls_input-peinh. "价格单位
      ls_itemx-price_unit = 'X'.
    ENDIF.

*    IF ls_input-waers IS NOT INITIAL.
*      ls_item-currency    = ls_input-waers. "币别
*      ls_itemx-currency   = 'X'.
*    ENDIF.

*    IF ls_input-fistl IS NOT INITIAL.
*      ls_item-funds_ctr   = ls_input-fistl. "基金中心
*      ls_itemx-funds_ctr  = 'X'.
*    ENDIF.

*    IF ls_input-fipos IS NOT INITIAL .
*      ls_item-cmmt_item   = ls_input-fipos. "承诺项目
*      ls_itemx-cmmt_item  = 'X'.
*    ENDIF.

    IF ls_input-txz01 IS NOT INITIAL.
      ls_item-short_text  = ls_input-txz01. "短文本
      ls_itemx-short_text = 'X'.
    ENDIF.

    IF ls_input-idnlf IS NOT INITIAL.
      ls_item-vend_mat = ls_input-idnlf. "供应商物料
      ls_itemx-vend_mat = 'X'.
    ENDIF.

    IF ls_input-ekgrp IS NOT INITIAL.
      ls_item-pur_group   = ls_input-ekgrp. "采购组
      ls_itemx-pur_group  = 'X'.
    ENDIF.

    IF ls_input-matkl IS NOT INITIAL.
      ls_item-matl_group  = ls_input-matkl. "物料组
      ls_itemx-matl_group = 'X'.
    ENDIF.

*&---审批策略
    IF ls_input-bednr IS NOT INITIAL.
      ls_item-trackingno  = 'X'. "需求跟踪号
      ls_itemx-trackingno = 'X'.
    ENDIF.

    IF ls_input-lgort IS NOT INITIAL.
      ls_item-store_loc  = ls_input-lgort. "库存地点
      ls_itemx-store_loc = 'X'.
    ENDIF.

    IF ls_input-reswk IS NOT INITIAL.
      ls_item-suppl_plnt = ls_input-reswk. "供货工厂
      ls_itemx-suppl_plnt = 'X'.
    ENDIF.

    IF ls_input-lifnr IS NOT INITIAL.
      ls_item-des_vendor = ls_input-lifnr. "供应商
      ls_itemx-des_vendor = 'X'.
    ENDIF.

    IF ls_input-wepos IS NOT INITIAL.
      ls_item-gr_ind = ls_input-wepos. "收货
      ls_itemx-gr_ind = 'X'.
    ENDIF.

    IF ls_input-repos IS NOT INITIAL.
      ls_item-ir_ind = ls_input-repos. "发票收据
      ls_itemx-ir_ind = 'X'.
    ENDIF.

    IF ls_input-weunb IS NOT INITIAL.
      ls_item-gr_non_val = ls_input-weunb. "无价值收货
      ls_itemx-gr_non_val = 'X'.
    ENDIF.

    APPEND ls_item  TO lt_item.
    APPEND ls_itemx TO lt_itemx.


    CLEAR:ls_praccount, ls_praccountx.

    IF ls_input-kostl IS NOT INITIAL.
      CONDENSE ls_input-kostl.
      ls_praccount-costcenter   =  ls_input-kostl."成本中心
      ls_praccountx-costcenter  = 'X'.
    ENDIF.

    IF ls_input-sakto IS NOT INITIAL.
      ls_praccount-gl_account = ls_input-sakto.   "总账科目
      ls_praccountx-gl_account = 'X'.
    ENDIF.

*    IF ls_input-vbeln IS NOT INITIAL.
*      CONDENSE ls_input-vbeln.
*      ls_praccount-sd_doc     = ls_input-vbeln.    "销售订单号
*      ls_praccount-itm_number = ls_input-vbelp.    "销售订单行项目
*
*      ls_praccountx-sd_doc     = 'X'.
*      ls_praccountx-itm_number = 'X'.
*
*    ENDIF.

*    IF ls_input-fipos IS NOT INITIAL.
*      ls_praccount-cmmt_item         =  ls_input-fipos."承诺项目
*      ls_praccountx-cmmt_item        = 'X'.
*      ls_praccount-cmmt_item_long    =  ls_input-fipos."承诺项目
*      ls_praccountx-cmmt_item_long   = 'X'.
*    ENDIF.


*    IF ls_input-fistl IS NOT INITIAL.
*      ls_praccount-funds_ctr   = ls_input-fistl. "基金中心
*      ls_praccountx-funds_ctr  = 'X'.
*    ENDIF.


*    IF ls_input-wbs_ele IS NOT INITIAL.
*      ls_praccount-wbs_element   = ls_input-wbs_ele."WBS元素
*      ls_praccountx-wbs_element  = 'X'.
*    ENDIF.

    IF ls_praccount IS NOT INITIAL.
      ls_praccount-preq_item    =  ls_input-ebelp. "采购申请行项目
      ls_praccount-serial_no    = '01'.

      ls_praccountx-preq_item   = ls_input-ebelp. "采购申请行项目
      ls_praccountx-serial_no   = '01'.

      APPEND  ls_praccount  TO lt_praccount.
      APPEND  ls_praccountx TO lt_praccountx.
    ENDIF.


*&---行项目文本处理
*&---每132字符进行分割
    IF ls_input-text1 IS NOT INITIAL."抬头文本
      CLEAR:lv_length,lv_num,lv_count,lv_start,lv_txt.
      CONDENSE ls_input-text1.
      lv_length = strlen( ls_input-text1 ).
      lv_num    = lv_length DIV 132.    "每132字符分割，需要分割多少整数次
      DO lv_num TIMES.
        lv_length = lv_length - 132.
        lv_count  = lv_count + 1.      "
        lv_start  = ( lv_count - 1 ) * 132 .
        CLEAR lv_txt.
        lv_txt = ls_input-text1+lv_start(132).
        CLEAR ls_pritemtext.

        ls_pritemtext-preq_item = ls_input-ebelp. "行项目号
        ls_pritemtext-text_id   = 'B01'. "
        ls_pritemtext-text_form = '*'. "
        ls_pritemtext-text_line = lv_txt. "
        APPEND ls_pritemtext TO lt_pritemtext.
      ENDDO.
*&---分割剩下的字符写人内表
      IF lv_length > 0.
        IF lv_num > 0."最初的字符长度大于132
          lv_start = lv_start + 132.
        ELSE.
          CLEAR lv_start.
        ENDIF.
        CLEAR lv_txt.
        lv_txt = ls_input-text1+lv_start(lv_length).
        CLEAR ls_pritemtext.

        ls_pritemtext-preq_item = ls_input-ebelp. "行项目号
        ls_pritemtext-text_id   = 'B01'. "
        ls_pritemtext-text_form = '*'. "
        ls_pritemtext-text_line = lv_txt. "
        APPEND ls_pritemtext TO lt_pritemtext.
**&---外部给号
*        IF rb_04 = 'X' .
*          ls_pritemtext-preq_no   = ls_input-banfn. "采购申请编号
*        ENDIF.

      ENDIF.
    ENDIF.


    AT LAST.

      READ TABLE lt_input INTO ls_input INDEX 1.

*&---填充抬头数据
      CLEAR:ls_header,ls_headerx.
      CLEAR:lt_prheadertext.

      ls_header-pr_type  = ls_input-bsart. "订单类型
      ls_headerx-pr_type = 'X'.



*&---抬头文本
*&---每132字符进行分割
*      IF ls_input-head_t01 IS NOT INITIAL."抬头文本
*        CLEAR:lv_length,lv_num,lv_count,lv_start,lv_txt.
*        CONDENSE ls_input-head_t01.
*        lv_length = strlen( ls_input-head_t01 ).
*        lv_num    = lv_length DIV 132.    "每132字符分割，需要分割多少整数次
*        DO lv_num TIMES.
*          lv_length = lv_length - 132.
*          lv_count  = lv_count + 1.      "
*          lv_start  = ( lv_count - 1 ) * 132 .
*          CLEAR lv_txt.
*          lv_txt = ls_input-head_t01+lv_start(132).
*          CLEAR ls_prheadertext.
*
*          ls_prheadertext-text_id   = 'B01'. "
*          ls_prheadertext-text_form = '*'. "
*          ls_prheadertext-text_line = lv_txt. "
*          APPEND ls_prheadertext TO lt_prheadertext.
*        ENDDO.
**&---分割剩下的字符写人内表
*        IF lv_length > 0.
*          IF lv_num > 0."最初的字符长度大于132
*            lv_start = lv_start + 132.
*          ELSE.
*            CLEAR lv_start.
*          ENDIF.
*          CLEAR lv_txt.
*          lv_txt = ls_input-head_t01+lv_start(lv_length).
*          CLEAR ls_prheadertext.
*
*          ls_prheadertext-text_id   = 'B01'. "
*          ls_prheadertext-text_form = '*'. "
*          ls_prheadertext-text_line = lv_txt. "
*          APPEND ls_prheadertext TO lt_prheadertext.
*        ENDIF.
*      ENDIF.


*&---调用BAPI:BAPI_PR_CREATE

      CLEAR:lv_banfn.
      CALL FUNCTION 'BAPI_PR_CREATE'
        EXPORTING
          prheader     = ls_header
          prheaderx    = ls_headerx
*         testrun      = gv_test
        IMPORTING
          number       = lv_banfn
*         PRHEADEREXP  =
        TABLES
          return       = lt_return[]
          pritem       = lt_item[]
          pritemx      = lt_itemx[]
          praccount    = lt_praccount[]
          praccountx   = lt_praccountx[]
          pritemtext   = lt_pritemtext[]
          prheadertext = lt_prheadertext[]
          extensionin  = lt_extensionin[].



      LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        p_msgtype   = c_type_success.
        lv_mess = '创建成功，凭证号为' && lv_banfn.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        p_msgtype   = c_type_error.

        LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
          CONCATENATE lv_mess ls_return-message INTO lv_mess SEPARATED BY '|'.
        ENDLOOP.

        SHIFT lv_mess LEFT DELETING LEADING '|'.

      ENDIF.

      p_msg = lv_mess.
      CLEAR:ls_header,ls_headerx,
            lt_item,lt_itemx,
            lt_praccount,
            lt_prheadertext,
            lt_pritemtext,
            lt_extensionin,
            lt_return
            .
    ENDAT.

  ENDLOOP.

  FREE lt_input.
ENDFORM.