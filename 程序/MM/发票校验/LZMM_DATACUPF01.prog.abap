*----------------------------------------------------------------------*
***INCLUDE LZMM_interfaceF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_INPUT  text
*----------------------------------------------------------------------*
FORM frm_check_input  USING    p_input STRUCTURE zsinterface_invi CHANGING p_error p_msg.

  DATA: wa_l_kpqd       TYPE ztkpqd,
      wa_l_bcqd       TYPE ztbcqd.
  DATA:lv_bukrs TYPE t001-bukrs,
       lv_lifnr TYPE lfa1-lifnr.
  DATA:l_se TYPE zsinterface_invi-z_shuie,
       l_bhsje TYPE zsinterface_invi-z_bhsje.
  DATA:lv_length TYPE i,
       lv_gysfp TYPE c LENGTH 25.
***在运行时增加对于屏幕上的检查
  IF p_input-z_shuie = 0.
    MESSAGE s063(zmm_msg) WITH '税额' INTO p_msg.
    p_error = 'X'.
    EXIT.
  ENDIF.

  CLEAR wa_l_bcqd.
  SELECT SINGLE * INTO wa_l_bcqd FROM ztbcqd WHERE z_dzdh = p_input-z_dzdh AND zbelnr = ''.
  IF wa_l_bcqd IS INITIAL.
***去开票清单去检查是否还存在未开发票
    CLEAR wa_l_kpqd.
    SELECT SINGLE * INTO wa_l_kpqd FROM ztkpqd WHERE z_dzdh = p_input-z_dzdh AND zbelnr = ''.
    IF wa_l_kpqd IS INITIAL.
      MESSAGE s066(zmm_msg) WITH p_input-z_dzdh INTO p_msg.
      p_error = 'X'.
      EXIT.
    ENDIF.

  ENDIF.

***增加对必输项判断
  IF p_input-bukrs IS INITIAL.
    MESSAGE s001(zmm_msg) WITH '公司代码' INTO p_msg.
    p_error = 'X'.
    EXIT.
  ELSE.
    SELECT SINGLE bukrs INTO lv_bukrs FROM t001 WHERE bukrs = p_input-bukrs.
    IF sy-subrc NE 0.
      CONCATENATE '公司代码' p_input-bukrs '不存在' INTO p_msg.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_input-lifn2 IS INITIAL.
    MESSAGE s001(zmm_msg) WITH '受票方' INTO p_msg.
    p_error = 'X'.
    EXIT.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_input-lifn2
      IMPORTING
        output = p_input-lifn2.
    SELECT SINGLE lifnr INTO lv_lifnr FROM lfa1 WHERE lifnr = p_input-lifn2.
    IF sy-subrc NE 0.
      MESSAGE s071(zmm_msg) WITH p_input-lifn2 INTO p_msg.
      p_error = 'X'.
      EXIT.
*    ELSE.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          input  = p_input-lifn2
*        IMPORTING
*          output = p_input-lifn2.
    ENDIF.
  ENDIF.

  IF p_input-z_dzdh IS INITIAL.
    MESSAGE s001(zmm_msg) WITH '对账单号' INTO p_msg.
    p_error = 'X'.
    EXIT.
  ENDIF.
  IF p_input-z_gysfp IS INITIAL.
    MESSAGE s001(zmm_msg) WITH '供应商发票号码' INTO p_msg.
    p_error = 'X'.
    EXIT.
  ELSE.
    CLEAR lv_length.
    lv_length = strlen( p_input-z_gysfp ).
    IF lv_length > 25.    "如果超过25位  只取前25位
      CLEAR lv_gysfp.
      lv_gysfp = p_input-z_gysfp+0(25).
      CLEAR p_input-z_gysfp.
      p_input-z_gysfp = lv_gysfp.
    ENDIF.
  ENDIF.
  IF p_input-bldat IS INITIAL.
    MESSAGE s001(zmm_msg) WITH '凭证日期' INTO p_msg.
    p_error = 'X'.
    EXIT.
  ENDIF.
  IF NOT p_input-z_shuie IS INITIAL.
    PERFORM frm_get_sum_je USING p_input l_se l_bhsje p_msg p_error.
    CHECK p_error IS INITIAL.
    IF abs( l_se - p_input-z_shuie ) > abs( l_se * 25 / 100 ) .
      MESSAGE s065(zmm_msg) INTO p_msg.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.                    " FRM_CHECK_INPUT
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_MAIN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_INPUT  text
*----------------------------------------------------------------------*
FORM frm_get_main_data  USING    p_input STRUCTURE zsinterface_invi.


***判断对帐单是否为寄售还是非寄售
  SELECT SINGLE zsobkz INTO g_flg_con FROM ztkpqd
    WHERE zlifn2 = p_input-lifn2 AND z_dzdh = p_input-z_dzdh AND zbukrs = p_input-bukrs..
  IF sy-subrc <> 0.
    SELECT SINGLE zsobkz INTO g_flg_con FROM ztbcqd
      WHERE zlifn2 = p_input-lifn2 AND z_dzdh = p_input-z_dzdh AND zbukrs = p_input-bukrs..
  ENDIF.
*
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_ztkpqd FROM ztkpqd
    WHERE zlifn2 = p_input-lifn2 AND z_dzdh = p_input-z_dzdh AND zbukrs = p_input-bukrs." AND ZBELNR = ''.
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_ztbcqd FROM ztbcqd
*    WHERE zlifn2 = p_input-lifn2 AND z_dzdh = p_input-z_dzdh AND zbukrs = p_input-bukrs." AND ZBELNR = ''.
  CLEAR :it_ztkpqd,it_ztbcqd.
***需要将开票清单里的项进行合并，同对帐单号，同物料，同工厂，进行合计。
  LOOP AT gt_ztkpqd INTO gs_ztkpqd.
    MOVE-CORRESPONDING gs_ztkpqd TO wa_ztkpqd.
    CLEAR wa_ztkpqd-zexidv.
    IF g_flg_con IS INITIAL."非寄售
      CLEAR: wa_ztkpqd-zbelnr,wa_ztkpqd-zbktxt.
      CASE gs_ztkpqd-zbwart.
        WHEN '101' OR '123' OR '161'.
          gs_ztkpqd-id = 'A'.
        WHEN '102' OR '122' OR '162'.
          gs_ztkpqd-id = 'B'.
        WHEN OTHERS.
      ENDCASE.
      MODIFY gt_ztkpqd FROM gs_ztkpqd.
    ENDIF.
    APPEND wa_ztkpqd TO it_ztkpqd.
    CLEAR wa_ztkpqd.
  ENDLOOP.
  SORT it_ztkpqd BY zmblnr zmjahr zzeile.
  DELETE ADJACENT DUPLICATES FROM it_ztkpqd.
***需要将补差清单里的项进行合并，同对帐单号，同物料，同工厂，进行合计。
  CLEAR: g_con_bcse, g_con_bcje.
  LOOP AT gt_ztbcqd INTO gs_ztbcqd.
    IF g_flg_con IS INITIAL."非寄售
      CLEAR: wa_ztkpqd-zbelnr,wa_ztkpqd-zbktxt.
    ENDIF.
    wa_ztbcqd-z_dzdh = gs_ztbcqd-z_dzdh.
    wa_ztbcqd-zbukrs = gs_ztbcqd-zbukrs.
    wa_ztbcqd-zbwaer = gs_ztbcqd-zbwaer.
    wa_ztbcqd-zlifn2 = gs_ztbcqd-zlifn2.
    wa_ztbcqd-zmatnr = gs_ztbcqd-zmatnr.
    wa_ztbcqd-zwerks = gs_ztbcqd-zwerks.
    wa_ztbcqd-zvemng = gs_ztbcqd-zvemng.
    wa_ztbcqd-zbstme = gs_ztbcqd-zbstme.
    wa_ztbcqd-z_bcje = gs_ztbcqd-z_bcje.
    wa_ztbcqd-zshkzg = gs_ztbcqd-zshkzg.
    wa_ztbcqd-zmwskz = gs_ztbcqd-zmwskz.

    COLLECT wa_ztbcqd INTO it_ztbcqd.
    CLEAR wa_ztbcqd.
  ENDLOOP.


ENDFORM.                    " FRM_GET_MAIN_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_MAiN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_deal_main_data USING p_input STRUCTURE zsinterface_invi CHANGING p_error p_msg p_belnr.
  DATA: ls_rkwa       TYPE rkwa,
        ls_block      TYPE typ_block.
  DATA: lt_ztkpqd     TYPE STANDARD TABLE OF ztkpqd,
        ls_ztkpqd     TYPE ztkpqd,
        lt_ztbcqd     TYPE STANDARD TABLE OF ztbcqd,
        ls_ztbcqd     TYPE ztbcqd.
  DATA: lv_sgtxt      TYPE bseg-sgtxt.
  DATA: l_con_wr_se   TYPE p DECIMALS 4,"开票清单中的税额
        l_con_se      TYPE p DECIMALS 2,"寄售的税额
        l_con_bcse    TYPE p DECIMALS 2,
        l_con_bcje    TYPE p DECIMALS 2,
        l_ncon_bcse   TYPE p DECIMALS 2,
        l_ncon_bcje   TYPE p DECIMALS 2,
        l_cy          TYPE p DECIMALS 2 VALUE '0.01'.

  CLEAR lv_sgtxt.
  lv_sgtxt = p_input-z_gysfp.

  IF g_flg_con = 'K'.
***寄售部分发票校验使用标准程序
***填充寄售部分的数据
    CLEAR: ls_rkwa, g_nhsje, g_hsje, l_con_wr_se,it_rkwa.
    LOOP AT it_ztkpqd INTO wa_ztkpqd WHERE zsobkz = 'K'.
      SELECT SINGLE * INTO ls_rkwa FROM rkwa
        WHERE mblnr = wa_ztkpqd-zmblnr AND mjahr = wa_ztkpqd-zmjahr AND zeile = wa_ztkpqd-zzeile.
      ls_rkwa-mblnr = wa_ztkpqd-zmblnr.
      ls_rkwa-mjahr = wa_ztkpqd-zmjahr.
      ls_rkwa-zeile = wa_ztkpqd-zzeile.
      ls_rkwa-budat = wa_ztkpqd-zbudat.
      ls_rkwa-bukrs = wa_ztkpqd-zbukrs.
      ls_rkwa-sobkz = wa_ztkpqd-zsobkz.
      ls_rkwa-lifnr = wa_ztkpqd-zlifn2.
      ls_rkwa-werks = wa_ztkpqd-zwerks.
      ls_rkwa-matnr = wa_ztkpqd-zmatnr.
      ls_rkwa-bwaer = wa_ztkpqd-zbwaer.
      ls_rkwa-wrbtr = abs( wa_ztkpqd-zwrbtr ).
      ls_rkwa-mwskz = wa_ztkpqd-zmwskz.
      l_con_wr_se   = l_con_wr_se + wa_ztkpqd-zwrbtr * wa_ztkpqd-z_sl.
      APPEND ls_rkwa TO it_rkwa.
      CLEAR ls_rkwa.
    ENDLOOP.

    SORT it_rkwa BY mblnr mjahr zeile.
    DELETE ADJACENT DUPLICATES FROM it_rkwa.
    CLEAR gt_block.
    PERFORM rkwa_partitionieren1 TABLES it_rkwa    "--->
                                       gt_block   "<---
                                 USING p_error
                                       p_msg.

    READ TABLE gt_block INTO ls_block INDEX 1.
    IF sy-subrc = 0 AND ls_block-msgty = 'E'.
      p_error = 'X'.
      MESSAGE ID ls_block-msgid TYPE 'S' NUMBER ls_block-msgno
         WITH ls_block-msgv1 ls_block-msgv2 ls_block-msgv3 ls_block-msgv4 INTO p_msg.
      EXIT.
    ENDIF.

    CLEAR ls_rkwa.

***20140321，开票金额，补差金额，税额按照不同情况来判定
    IF it_ztbcqd IS INITIAL."不存在补差的情况，则税额就是输入税额,寄售的税额与非寄售的税额为屏幕输入税额
      l_con_se = abs( p_input-z_shuie ) .
    ELSE."补差清单不为空
      IF it_ztkpqd IS INITIAL.
        "开票清单为空，则只有补差清单的情况下,界面输入税额即为税额,金额为补差金额
        l_con_bcse = abs( p_input-z_shuie ).
*        L_CON_BCJE = G_BC_JE + L_CON_BCSE.
        l_con_bcje = g_bc_je + p_input-z_shuie."20150311补差金额调整为相加
      ELSE.
        "开票清单与补差清单都不为空，按金额同向反向判断
        IF ( g_kp_je > 0 AND g_bc_je < 0 ) OR ( g_kp_je < 0 AND g_bc_je > 0 ).
          "不同正负时,寄售税额为寄售税额,补差税额为寄售税额 - 输入税额
          l_con_se = abs( g_kp_se ).
          l_con_bcse = g_kp_se - p_input-z_shuie .

        ELSEIF ( g_kp_je > 0 AND g_bc_je > 0 ) OR ( g_kp_je < 0 AND g_bc_je < 0 ).
          " 同为正同为负时，
          IF abs( p_input-z_shuie ) > abs( g_kp_se ).
            "如果输入税额>寄售税额的绝对值，寄售税额为寄售税额，补差税额为输入税额-寄售税额
            l_con_se = abs( g_kp_se ).
            l_con_bcse = p_input-z_shuie  - g_kp_se.

          ELSEIF abs( p_input-z_shuie ) <= abs( g_kp_se ).
            "如果输入税额<=寄售税额的绝对值，寄售税额为输入税额的绝对值-0.01
            l_con_se = abs( p_input-z_shuie ) - l_cy.
            l_con_bcse = l_cy.

          ENDIF.

        ENDIF.

***补差金额与补差税额的方向相同
        IF g_bc_je * l_con_bcse > 0.
          l_con_bcje = g_bc_je + l_con_bcse.
        ELSEIF g_bc_je * l_con_bcse < 0.
          l_con_bcje = g_bc_je - l_con_bcse.
        ENDIF.

      ENDIF.

    ENDIF.


    CLEAR: p_error.
    IF NOT it_rkwa IS INITIAL.
***寄售发票校验
      PERFORM abrechnen TABLES gt_block
                         USING l_con_se
                               lv_sgtxt
                               sy-datum
                               p_input-bldat.
      READ TABLE gt_block INTO ls_block INDEX 1.
***成功COMMIT
      IF sy-subrc = 0 AND ls_block-msgty = c_msgty_success.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
***回写BELNR， BKTXT
        g_flg_upd = 'KP'.
        PERFORM frm_update_belnr_bktxt USING ls_block-belnr
                                             ls_block-gjahr
                                             p_input
                                             g_flg_upd
                                             ''
                                             p_belnr.
        IF p_belnr IS NOT INITIAL.
          CONCATENATE p_belnr ls_block-belnr INTO p_belnr.
        ELSE.
          p_belnr = ls_block-belnr.
        ENDIF.

*        PERFORM FRM_UPDATE_DB_TABLE.
      ELSE.
        p_error = 'X'.
        MESSAGE ID ls_block-msgid TYPE 'S' NUMBER ls_block-msgno
           WITH ls_block-msgv1 ls_block-msgv2 ls_block-msgv3 ls_block-msgv4 INTO p_msg.
        EXIT.
      ENDIF.

    ENDIF.
***寄售补差,因为会存在开票清单里没有，但在补差清单中存在记录的情况
    IF NOT it_ztbcqd IS INITIAL.
      PERFORM frm_deal_invoice_data TABLES lt_ztkpqd
                                           it_ztbcqd
                                    USING sy-datum
                                          p_input
                                          l_con_bcse
                                          l_con_bcje
                                          p_error
                                          p_msg
                                          p_belnr.
    ENDIF.

    IF p_error IS INITIAL.                                  "20141127
      PERFORM frm_update_db_table USING p_input p_error p_msg.
    ENDIF.
  ELSE.

***非寄售发票校验
    LOOP AT it_ztkpqd INTO wa_ztkpqd WHERE zsobkz = ''.
      MOVE-CORRESPONDING wa_ztkpqd TO ls_ztkpqd.
      CLEAR: ls_ztkpqd-zexidv.
      APPEND ls_ztkpqd TO lt_ztkpqd.
      CLEAR: ls_ztkpqd.
    ENDLOOP.
    SORT lt_ztkpqd BY zmblnr zmjahr zzeile.
    DELETE ADJACENT DUPLICATES FROM lt_ztkpqd.

    IF NOT lt_ztkpqd IS INITIAL.
      PERFORM frm_deal_invoice_data TABLES lt_ztkpqd
                                           it_ztbcqd
                                    USING  sy-datum
                                           p_input
                                          l_con_bcse
                                          l_con_bcje
                                          p_error
                                          p_msg
                                          p_belnr.
    ELSE.
***存在非寄售纯补差？
      l_con_bcje = p_input-z_bhsje + p_input-z_shuie.
      l_con_bcse = p_input-z_shuie.
      PERFORM frm_deal_invoice_data TABLES lt_ztkpqd
                                           it_ztbcqd
                                    USING  sy-datum
                                           p_input
                                          l_con_bcse
                                          l_con_bcje
                                          p_error
                                          p_msg
                                          p_belnr.
      IF p_error IS INITIAL.                                "20141127
        PERFORM frm_update_db_table USING p_input p_error p_msg.
      ENDIF.
    ENDIF.

  ENDIF.
ENDFORM.                    " FRM_DEAL_MIAN_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_INVOICE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_ZTKPQD  text
*      -->PT_ZTBCQD  text
*      -->PR_BUDAT   text
*      -->P_INPUT    text
*      -->P_BCSE     text
*      -->P_BCJE     text
*      -->P_ERROR    text
*----------------------------------------------------------------------*
FORM frm_deal_invoice_data TABLES pt_ztkpqd STRUCTURE wa_ztkpqd
                                   pt_ztbcqd STRUCTURE wa_ztbcqd
                            USING  pr_budat
                                   p_input STRUCTURE zsinterface_invi
                                   p_bcse
                                   p_bcje
                                   p_error
                                   p_msg
                                   p_belnr.

  DATA: ls_headerdata           TYPE bapi_incinv_create_header,
        ls_addressdata          TYPE bapi_incinv_create_addressdata,
        ls_invoicestatus        TYPE bapi_incinv_create_status-rbstat VALUE '5',
        ls_invoicedocnumber     TYPE bapi_incinv_fld-inv_doc_no,
        ls_fiscalyear           TYPE bapi_incinv_fld-fisc_year,
        lt_accountingdata       TYPE STANDARD TABLE OF bapi_incinv_create_account,
        lt_glaccountdata        TYPE STANDARD TABLE OF bapi_incinv_create_gl_account,
        lt_itemdata             TYPE STANDARD TABLE OF bapi_incinv_create_item,
        ls_itemdata             TYPE bapi_incinv_create_item,
        lt_materialdata         TYPE STANDARD TABLE OF bapi_incinv_create_material,
        ls_materialdata         TYPE bapi_incinv_create_material,
        lt_taxdata              TYPE STANDARD TABLE OF bapi_incinv_create_tax,
        ls_taxdata              TYPE bapi_incinv_create_tax,
        lt_withtaxdata          TYPE STANDARD TABLE OF bapi_incinv_create_withtax,
        ls_withtaxdata          TYPE bapi_incinv_create_withtax,
        lt_vendoritemsplitdata  TYPE STANDARD TABLE OF bapi_incinv_create_vendorsplit,
        ls_vendoritemsplitdata  TYPE bapi_incinv_create_vendorsplit,
        lt_tkpqd                TYPE STANDARD TABLE OF t_kpqd,
        ls_tkpqd                TYPE t_kpqd,
        ls_t052                 TYPE t052,
        ls_faede_i              TYPE faede,
        ls_faede_e              TYPE faede.
***开票清单移动类型表
  DATA: lt_tab                  TYPE STANDARD TABLE OF t_tab,
        ls_tab                  TYPE t_tab.
  DATA: l_invoice_doc_item      TYPE rblgp,
        l_amount                TYPE wrbtr,
        l_tax_amount            TYPE wrbtr,
        l_s                     TYPE c,
        l_mblnr                 TYPE mblnr,
        l_mjahr                 TYPE mjahr,
        l_zeile                 TYPE mblpo,
        l_je                    TYPE p DECIMALS 3,
        l_se                    TYPE p DECIMALS 3,
        l_cy                    TYPE p DECIMALS 2,
        l_tabix                 TYPE sytabix,
        l_belnr                 TYPE belnr_d,
        l_sgtxt                 TYPE bseg-sgtxt.
  DATA: lv_flag VALUE 'X'. " 原因代码增强

  CLEAR: l_invoice_doc_item, ls_headerdata, ls_invoicedocnumber, ls_fiscalyear,
         ls_itemdata, ls_materialdata, ls_taxdata, lt_itemdata, lt_materialdata,
         lt_taxdata, it_return, l_amount, l_tax_amount,l_sgtxt.

  l_sgtxt = p_input-z_gysfp.
**非寄售时，将开票与补差清单分开发票校验
  IF NOT pt_ztkpqd[] IS INITIAL.
***判断先后做凭证顺序
    CLEAR: lt_tkpqd, ls_tkpqd,lt_tab, ls_tab.
    LOOP AT pt_ztkpqd INTO wa_ztkpqd.
      MOVE-CORRESPONDING wa_ztkpqd TO ls_tkpqd.
      CASE ls_tkpqd-zbwart.
        WHEN '101' OR '123' OR '161'.
          ls_tkpqd-id = 'A'.
        WHEN '102' OR '122' OR '162'.
          ls_tkpqd-id = 'B'.
        WHEN OTHERS.
      ENDCASE.
      APPEND ls_tkpqd TO lt_tkpqd.
      ls_tab-id = ls_tkpqd-id.
      COLLECT ls_tab INTO lt_tab.
      CLEAR ls_tkpqd.
    ENDLOOP.
    SORT lt_tkpqd BY id.
    SORT lt_tab BY id.


***填充DP，RP字段
    LOOP AT lt_tab INTO ls_tab.
      LOOP AT lt_tkpqd INTO ls_tkpqd WHERE id = ls_tab-id.
        CASE ls_tkpqd-zbwart.
          WHEN '101' OR '123'.
            ls_tab-dp = 'S'.
          WHEN '102' OR '122'.
            ls_tab-dp = 'H'.
          WHEN '161'.
            ls_tab-rp = 'H'.
          WHEN '162'.
            ls_tab-rp = 'S'.
          WHEN OTHERS.
        ENDCASE.
        MODIFY lt_tab FROM ls_tab.
      ENDLOOP.
      CLEAR ls_tab.
    ENDLOOP.
    SORT lt_tab BY id.

****按照移动类型来分类计算不同移动类型的总金额和税额
    CLEAR: l_tabix.
    LOOP AT lt_tab INTO ls_tab.
      CLEAR: l_je, l_se.
      l_tabix = l_tabix + 1.
      LOOP AT lt_tkpqd INTO ls_tkpqd WHERE id = ls_tab-id.
        l_se    = l_se + ls_tkpqd-zwrbtr * ls_tkpqd-z_sl.
        l_je    = l_je + ls_tkpqd-zwrbtr.
      ENDLOOP.
      ls_tab-z_kp_je = l_je.
      ls_tab-z_kp_se = l_se.
      IF l_tabix = 1.
        ls_tab-z_bc_je = g_bc_je.
        ls_tab-z_bc_se = g_bc_se.
      ENDIF.

      MODIFY lt_tab FROM ls_tab.
      CLEAR ls_tab.
    ENDLOOP.
***界面上输入的税额与计算的税额差异怎么解决,计算在第一笔单子中

    l_cy = p_input-z_shuie - g_kp_se - g_bc_se.

    CLEAR: l_tabix.
    LOOP AT lt_tab INTO ls_tab.
      CLEAR: l_invoice_doc_item, ls_headerdata, ls_invoicedocnumber, ls_fiscalyear,
             ls_itemdata , ls_materialdata, ls_taxdata,
             lt_itemdata, lt_materialdata, lt_taxdata,  it_return, l_amount, l_tax_amount.
***如果存在，即表示非寄售

      l_tabix = l_tabix + 1.
      IF l_tabix = 1.
        l_amount = ls_tab-z_kp_je + ls_tab-z_kp_se + ls_tab-z_bc_je + ls_tab-z_bc_se + l_cy.
        l_tax_amount = ls_tab-z_kp_se + ls_tab-z_bc_se + l_cy.
      ELSE.
        l_amount = ls_tab-z_kp_je + ls_tab-z_kp_se.
        l_tax_amount = ls_tab-z_kp_se.
      ENDIF.

      LOOP AT lt_tkpqd INTO ls_tkpqd WHERE id = ls_tab-id.
***准备BAPI数据

        SELECT SINGLE zterm INTO ls_headerdata-pmnttrms
          FROM lfb1 WHERE lifnr = ls_tkpqd-zlifn2 AND bukrs = ls_tkpqd-zbukrs."20140519增加付款条件
        IF l_amount > 0.
          ls_headerdata-invoice_ind     = x.
          ls_headerdata-bline_date      = p_input-bldat.
        ELSEIF l_amount < 0 ."20140616基准日期增加公司代码GC02345
          CLEAR: ls_t052, ls_faede_i, ls_faede_e.
          SELECT SINGLE * INTO ls_t052 FROM t052 WHERE zterm = ls_headerdata-pmnttrms.
          ls_faede_i-zbd1t = ls_t052-ztag1.
          ls_faede_i-zbd2t = ls_t052-ztag2.
          ls_faede_i-zbd3t = ls_t052-ztag3.
          ls_faede_i-bldat = p_input-bldat.
          ls_faede_i-shkzg = 'S'.
          ls_faede_i-koart = 'D'.

          CALL FUNCTION 'DETERMINE_DUE_DATE'
            EXPORTING
              i_faede    = ls_faede_i
              i_gl_faede = 'X'
            IMPORTING
              e_faede    = ls_faede_e.
          ls_headerdata-bline_date      = ls_faede_e-netdt.
        ENDIF.
        ls_headerdata-doc_type        = 'RE'.
        ls_headerdata-doc_date        = p_input-bldat.
        ls_headerdata-pstng_date      = pr_budat.
        ls_headerdata-comp_code       = ls_tkpqd-zbukrs.
        ls_headerdata-currency        = ls_tkpqd-zbwaer.
*        LS_HEADERDATA-BLINE_DATE      = p_input-bldat.
        ls_headerdata-diff_inv        = ls_tkpqd-zlifn2.
        ls_headerdata-gross_amount    = abs( l_amount ).
        ls_headerdata-header_txt      = l_sgtxt.
        ls_headerdata-deliv_posting   = ls_tab-dp.
        ls_headerdata-return_posting  = ls_tab-rp.
******************* hp_dxj 20140813  增加发票参考号  start *****************************
*        IF P_REFNO IS NOT INITIAL.
*          LS_HEADERDATA-INV_REF_NO = P_REFNO .
*        ENDIF.
*        IF P_GJAHR IS NOT INITIAL .
*          LS_HEADERDATA-INV_YEAR   = P_GJAHR .
*        ENDIF.

******************* hp_dxj 20140813  增加发票参考号  end *****************************
        IF ls_headerdata-pymt_meth IS INITIAL.
          SELECT SINGLE zwels INTO ls_headerdata-pymt_meth
            FROM lfb1 WHERE lifnr = ls_tkpqd-zlifn2 AND bukrs = ls_tkpqd-zbukrs.
        ENDIF.
***非寄售
***ITEM
        SELECT SINGLE ebeln ebelp INTO (ls_itemdata-po_number, ls_itemdata-po_item) FROM mseg
          WHERE mblnr = ls_tkpqd-zmblnr AND mjahr = ls_tkpqd-zmjahr AND zeile = ls_tkpqd-zzeile.
        ls_itemdata-tax_code          = ls_tkpqd-zmwskz.
        ls_itemdata-item_amount       = ls_tkpqd-zwrbtr.
        ls_itemdata-quantity          = ls_tkpqd-zbstmg.
        ls_itemdata-po_unit           = ls_tkpqd-zbstme.
***根据移动类型来获取其参考凭证号
        CLEAR: l_mblnr, l_mjahr, l_zeile.
***20150128hp_sjf反复冲销导致参考凭证出错，需要注释
*        IF LS_TKPQD-ZBWART = '122' OR LS_TKPQD-ZBWART = '123'
*          OR LS_TKPQD-ZBWART = '102' OR LS_TKPQD-ZBWART = '162'.
        SELECT SINGLE lfbnr lfbja lfpos INTO (l_mblnr,l_mjahr,l_zeile) FROM mseg
          WHERE mblnr = ls_tkpqd-zmblnr AND mjahr = ls_tkpqd-zmjahr AND zeile = ls_tkpqd-zzeile.
*        ELSE.
*          L_MBLNR        = LS_TKPQD-ZMBLNR.
*          L_MJAHR        = LS_TKPQD-ZMJAHR.
*          L_ZEILE        = LS_TKPQD-ZZEILE.
*        ENDIF."20150128hp_sjf
        ls_itemdata-ref_doc           = l_mblnr.
        ls_itemdata-ref_doc_year      = l_mjahr.
        ls_itemdata-ref_doc_it        = l_zeile.
        ls_itemdata-de_cre_ind        = l_s.
        COLLECT ls_itemdata INTO lt_itemdata.
        CLEAR ls_itemdata.

        ls_taxdata-tax_code           = ls_tkpqd-zmwskz.
        ls_taxdata-tax_amount         = abs( l_tax_amount ).
        APPEND ls_taxdata TO lt_taxdata.
        CLEAR ls_taxdata.

      ENDLOOP.

***调整ITEM表内数据
      LOOP AT lt_itemdata INTO ls_itemdata.
        l_invoice_doc_item            = l_invoice_doc_item + 1.
        ls_itemdata-invoice_doc_item  = l_invoice_doc_item.
        ls_itemdata-item_amount       = abs( ls_itemdata-item_amount ).
        ls_itemdata-quantity          = abs( ls_itemdata-quantity ).
        MODIFY lt_itemdata FROM ls_itemdata.
        CLEAR ls_itemdata.
      ENDLOOP.
***如果金额为0是无法做发票校验的，所以删掉
      DELETE lt_itemdata WHERE item_amount = 0.

      SORT lt_taxdata BY tax_code.
      DELETE ADJACENT DUPLICATES FROM lt_taxdata.

      IF l_tabix = 1."将补差放在第一条记录中
        CLEAR: l_invoice_doc_item.
        LOOP AT pt_ztbcqd INTO wa_ztbcqd.
***补差
          l_invoice_doc_item            = l_invoice_doc_item + 1.
          ls_materialdata-invoice_doc_item = l_invoice_doc_item.
          ls_materialdata-material      = wa_ztbcqd-zmatnr.
          ls_materialdata-val_area      = wa_ztbcqd-zwerks.
          IF wa_ztbcqd-z_bcje > 0.
            ls_materialdata-db_cr_ind     = 'S'.
          ELSEIF wa_ztbcqd-z_bcje < 0.
            ls_materialdata-db_cr_ind     = 'H'.
          ENDIF.
          ls_materialdata-item_amount   = abs( wa_ztbcqd-z_bcje ).
          ls_materialdata-quantity      = wa_ztbcqd-zvemng."数量用SKU的数量之和
          ls_materialdata-base_uom_iso  = wa_ztbcqd-zbstme.
          ls_materialdata-tax_code      = wa_ztbcqd-zmwskz.
          "HUANGGL 20170116   ADJUST DR_CR_IND ERROR(20170221 取消注释)
          IF wa_ztbcqd-z_bcje <> 0.
            APPEND ls_materialdata TO lt_materialdata.
          ENDIF.
          CLEAR ls_materialdata.

        ENDLOOP.
      ENDIF.

****检查是否已经生成财务凭证，如果没有生成则继续，否则跳出
      IF l_tabix = 1.
        CLEAR: l_belnr.
        SELECT SINGLE zbelnr INTO l_belnr FROM ztkpqd WHERE z_dzdh = p_input-z_dzdh AND zbelnr <> ''.
        IF NOT l_belnr IS INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.

      EXPORT lv_flag TO MEMORY ID 'ZMM_INTERFACE_INVOICE'.
      CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE1'
        EXPORTING
          headerdata       = ls_headerdata
          invoicestatus    = ls_invoicestatus
        IMPORTING
          invoicedocnumber = ls_invoicedocnumber
          fiscalyear       = ls_fiscalyear
        TABLES
          itemdata         = lt_itemdata
          materialdata     = lt_materialdata
          taxdata          = lt_taxdata
          return           = it_return.
      IF it_return IS INITIAL.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

***回写BELNR， BKTXT
        g_flg_upd = 'KP'.
        PERFORM frm_update_belnr_bktxt USING ls_invoicedocnumber
                                             ls_fiscalyear
                                             p_input
                                             g_flg_upd
                                             ls_tkpqd-id
                                             p_belnr.
        PERFORM frm_update_db_table USING p_input p_error p_msg.
        CLEAR: it_bseg, it_bsis.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .

        CLEAR: wa_return.
        READ TABLE it_return INTO wa_return WITH KEY type = c_msgty_error.
        IF sy-subrc = 0.
          p_error = 'X'.
          p_msg = wa_return-message.
          EXIT.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ELSE.
***寄售补差
***补差清单发票校验
    CLEAR: wa_ztbcqd.
    l_tax_amount = p_bcse.
    l_amount     = p_bcje.
    LOOP AT pt_ztbcqd INTO wa_ztbcqd WHERE z_bcje <> 0.  "huanggl 20170116 add by dr-cr-ind error
***准备BAPI数据
      SELECT SINGLE zterm INTO ls_headerdata-pmnttrms
        FROM lfb1 WHERE lifnr = wa_ztbcqd-zlifn2 AND bukrs = wa_ztbcqd-zbukrs."20140519增加付款条件
      IF l_amount > 0.
        ls_headerdata-invoice_ind     = x.
        ls_headerdata-bline_date      = p_input-bldat.
      ELSEIF l_amount < 0."20140616基准日期增加公司代码GC02345
        CLEAR: ls_t052, ls_faede_i, ls_faede_e.
        SELECT SINGLE * INTO ls_t052 FROM t052 WHERE zterm = ls_headerdata-pmnttrms.
        ls_faede_i-zbd1t = ls_t052-ztag1.
        ls_faede_i-zbd2t = ls_t052-ztag2.
        ls_faede_i-zbd3t = ls_t052-ztag3.
        ls_faede_i-bldat = p_input-bldat.
        ls_faede_i-shkzg = 'S'.
        ls_faede_i-koart = 'D'.

        CALL FUNCTION 'DETERMINE_DUE_DATE'
          EXPORTING
            i_faede    = ls_faede_i
            i_gl_faede = 'X'
          IMPORTING
            e_faede    = ls_faede_e.
        ls_headerdata-bline_date      = ls_faede_e-netdt.
      ENDIF.
      ls_headerdata-doc_type        = 'RE'.
      ls_headerdata-doc_date        = p_input-bldat.
      ls_headerdata-pstng_date      = pr_budat.
      ls_headerdata-comp_code       = wa_ztbcqd-zbukrs.
      ls_headerdata-currency        = wa_ztbcqd-zbwaer.
*      LS_HEADERDATA-BLINE_DATE      = p_input-bldat.
      ls_headerdata-diff_inv        = wa_ztbcqd-zlifn2.
      ls_headerdata-gross_amount    = abs( l_amount ).
      ls_headerdata-header_txt      = l_sgtxt.
      ls_headerdata-deliv_posting   = 'S'.
      ls_headerdata-return_posting  = 'H'.
******************* hp_dxj 20140813  增加发票参考号  start *****************************
*      IF P_REFNO IS NOT INITIAL.
*        LS_HEADERDATA-INV_REF_NO = P_REFNO .
*      ENDIF.
*      IF P_GJAHR IS NOT INITIAL .
*        LS_HEADERDATA-INV_YEAR   = P_GJAHR .
*      ENDIF.
******************* hp_dxj 20140813  增加发票参考号  end *****************************

      IF ls_headerdata-pymt_meth IS INITIAL.
        SELECT SINGLE zwels INTO ls_headerdata-pymt_meth
          FROM lfb1 WHERE lifnr = wa_ztbcqd-zlifn2 AND bukrs = wa_ztbcqd-zbukrs.
      ENDIF.

      l_invoice_doc_item            = l_invoice_doc_item + 1.
      ls_materialdata-invoice_doc_item = l_invoice_doc_item.
      ls_materialdata-material      = wa_ztbcqd-zmatnr.
      ls_materialdata-val_area      = wa_ztbcqd-zwerks.
      IF wa_ztbcqd-z_bcje > 0.
        ls_materialdata-db_cr_ind     = 'S'.
      ELSEIF wa_ztbcqd-z_bcje < 0.
        ls_materialdata-db_cr_ind     = 'H'.
      ENDIF.
      ls_materialdata-item_amount   = abs( wa_ztbcqd-z_bcje ).
      ls_materialdata-quantity      = wa_ztbcqd-zvemng."数量用SKU的数量之和
      ls_materialdata-base_uom_iso  = wa_ztbcqd-zbstme.
      ls_materialdata-tax_code      = wa_ztbcqd-zmwskz.
      "HUANGGL 20170116   ADJUST DR_CR_IND ERROR
      IF wa_ztbcqd-z_bcje = 0.
        CONTINUE.
      ENDIF.
      APPEND ls_materialdata TO lt_materialdata.
      CLEAR ls_materialdata.

      ls_taxdata-tax_code           = wa_ztbcqd-zmwskz.
      ls_taxdata-tax_amount         = abs( l_tax_amount ).
      APPEND ls_taxdata TO lt_taxdata.
      CLEAR ls_taxdata.

    ENDLOOP.

    SORT lt_taxdata BY tax_code.
    DELETE ADJACENT DUPLICATES FROM lt_taxdata.
    IF lt_materialdata IS NOT INITIAL. "huanggl 20170116 add  dr-cr-ind error
      EXPORT lv_flag TO MEMORY ID 'ZMM_INTERFACE_INVOICE'.
      CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE1'
        EXPORTING
          headerdata       = ls_headerdata
          invoicestatus    = ls_invoicestatus
        IMPORTING
          invoicedocnumber = ls_invoicedocnumber
          fiscalyear       = ls_fiscalyear
        TABLES
          itemdata         = lt_itemdata
          materialdata     = lt_materialdata
          taxdata          = lt_taxdata
          return           = it_return.

      IF it_return IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

***回写BELNR， BKTXT
        g_flg_upd = 'BC'.
        PERFORM frm_update_belnr_bktxt USING ls_invoicedocnumber
                                             ls_fiscalyear
                                             p_input
                                             g_flg_upd
                                             ''
                                             p_belnr.
*      PERFORM FRM_UPDATE_DB_TABLE.
*      CLEAR: IT_BSEG, IT_BSIS.
      ELSE.
        p_error = 'X'.
        CLEAR: wa_return.
        READ TABLE it_return INTO wa_return WITH KEY type = c_msgty_error.
        IF sy-subrc = 0.
          p_msg = wa_return-message.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " FRM_DEAL_INVOICE_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_UPDATE_BELNR_BKTXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  P_BELNR         text
*  -->  P_GJAHR         text
*  -->  p_input-z_gysfp        text
*  -->  p_input-lifn2        text
*  -->  P_FLG           text
*----------------------------------------------------------------------*
FORM frm_update_belnr_bktxt USING p_belnr
                                   p_gjahr
                                   p_input STRUCTURE zsinterface_invi
                                   p_flg
                                   p_id
                                   p_belnr_bkpf.
  DATA: ls_bseg   TYPE bseg,
        ls_bsis   TYPE bsis,
        ls_bkpf   TYPE bkpf,
        l_awkey   TYPE awkey,
        l_name1   TYPE name1,
        l_sgtxt   TYPE bseg-sgtxt.

***需要在BSEG里供应商那条记录，空的为供应商发票号码，T的为供应商中文名称

  CLEAR: l_awkey, ls_bkpf,l_sgtxt.
  l_sgtxt = p_input-z_gysfp.
  CONCATENATE p_belnr p_gjahr INTO l_awkey.
  SELECT SINGLE * INTO ls_bkpf FROM bkpf WHERE awkey = l_awkey AND bukrs = p_input-bukrs.
  IF p_belnr_bkpf IS NOT INITIAL.
    CONCATENATE p_belnr_bkpf ls_bkpf-belnr INTO p_belnr_bkpf SEPARATED BY '/'.
  ELSE.
    p_belnr_bkpf = ls_bkpf-belnr.
  ENDIF.
  IF it_bseg IS INITIAL.
    SELECT * INTO TABLE it_bseg FROM bseg
      WHERE bukrs = p_input-bukrs AND belnr = ls_bkpf-belnr AND gjahr = ls_bkpf-gjahr
      AND buzid IN ('','T').
    CLEAR: ls_bseg.
    LOOP AT it_bseg INTO ls_bseg.
      IF ls_bseg-buzid = ''.
        ls_bseg-sgtxt   = l_sgtxt.
      ELSEIF ls_bseg-buzid = 'T'.
        CLEAR: l_name1.
        SELECT SINGLE name1 INTO l_name1 FROM lfa1 WHERE lifnr = p_input-lifn2.
***格式为供应商发票号码+发票方+名称
***20140212Yanghua邮件更改为‘供应商的名字=供应商代码=发票号’
*        CONCATENATE p_input-z_gysfp '+' p_input-lifn2 '+' L_NAME1 INTO LS_BSEG-SGTXT.
        CONCATENATE l_name1 '=' p_input-lifn2 '=' l_sgtxt INTO ls_bseg-sgtxt.
      ENDIF.
      MODIFY it_bseg FROM ls_bseg.
      CLEAR ls_bseg.
    ENDLOOP.
  ENDIF.

  IF it_bsis IS INITIAL.
    SELECT * INTO TABLE it_bsis FROM bsis
      WHERE bukrs = p_input-bukrs AND belnr = ls_bkpf-belnr AND gjahr = ls_bkpf-gjahr
      AND buzid IN ('','T').
    CLEAR: ls_bsis.
    LOOP AT it_bsis INTO ls_bsis.
      IF ls_bsis-buzid = ''.
        ls_bsis-sgtxt   = l_sgtxt.
      ELSEIF ls_bsis-buzid = 'T'.
        CLEAR: l_name1.
        SELECT SINGLE name1 INTO l_name1 FROM lfa1 WHERE lifnr = p_input-lifn2.
***格式为供应商发票号码+发票方+名称
***20140212Yanghua邮件更改为‘供应商的名字=供应商代码=发票号’
*        CONCATENATE p_input-z_gysfp '+' p_input-lifn2 '+' L_NAME1 INTO LS_BSEG-SGTXT.
        CONCATENATE l_name1 '=' p_input-lifn2 '=' l_sgtxt INTO ls_bsis-sgtxt.
      ENDIF.
      MODIFY it_bsis FROM ls_bsis.
      CLEAR ls_bsis.
    ENDLOOP.
  ENDIF.

  IF p_flg = 'KP'.
    LOOP AT gt_ztkpqd INTO gs_ztkpqd WHERE id = p_id.
      IF NOT ls_bkpf IS INITIAL.
        gs_ztkpqd-zbelnr = ls_bkpf-belnr.
      ELSE.
        gs_ztkpqd-zbelnr = p_belnr.
      ENDIF.
      gs_ztkpqd-zgjahr = p_gjahr."增加会计年度
      gs_ztkpqd-zbktxt = l_sgtxt.
      MODIFY gt_ztkpqd FROM gs_ztkpqd.
      CLEAR gs_ztkpqd.
    ENDLOOP.
***如果存在补差记录，则一起更新
    LOOP AT gt_ztbcqd INTO gs_ztbcqd.
      IF NOT gs_ztbcqd-zbelnr IS INITIAL.
        CONTINUE.
      ENDIF.

      IF NOT ls_bkpf IS INITIAL.
        gs_ztbcqd-zbelnr = ls_bkpf-belnr.
      ELSE.
        gs_ztbcqd-zbelnr = p_belnr.
      ENDIF.
      gs_ztbcqd-zgjahr = p_gjahr."增加会计年度
      gs_ztbcqd-zbktxt = l_sgtxt.
      MODIFY gt_ztbcqd FROM gs_ztbcqd.
      CLEAR gs_ztbcqd.
    ENDLOOP.
  ELSEIF p_flg = 'BC'.
    LOOP AT gt_ztbcqd INTO gs_ztbcqd.
      IF NOT ls_bkpf IS INITIAL.
        gs_ztbcqd-zbelnr = ls_bkpf-belnr.
      ELSE.
        gs_ztbcqd-zbelnr = p_belnr.
      ENDIF.
      gs_ztbcqd-zgjahr = p_gjahr."增加会计年度
      gs_ztbcqd-zbktxt = l_sgtxt.
      MODIFY gt_ztbcqd FROM gs_ztbcqd.
      CLEAR gs_ztbcqd.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " FRM_UPDATE_BELNR_BKTXT
*&---------------------------------------------------------------------*
*&      Form  FRM_UPDATE_DB_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_update_db_table USING p_input STRUCTURE zsinterface_invi
                               p_error
                               p_msg.

***更新表BSEG
  DATA: lv_rebzg TYPE bseg-rebzg,
        wa_bseg  TYPE bseg.
  PERFORM frm_update_table TABLES it_bseg
                                  it_bsis
                           USING p_input
                                 p_error
                                 p_msg.
  "huanggl 2016.09.06
  CLEAR wa_bseg.
*  IF P_REFNO IS NOT INITIAL AND P_GJAHR IS NOT INITIAL.
*    READ TABLE IT_BSEG INTO WA_BSEG INDEX 1.
*    IF SY-SUBRC = 0.
*      SELECT SINGLE REBZG INTO LV_REBZG FROM BSEG
*       WHERE BUKRS = p_input-bukrs AND BELNR = WA_BSEG-BELNR AND GJAHR = WA_BSEG-GJAHR AND BUZEI = 1.
*      IF SY-SUBRC = 0 AND LV_REBZG IS INITIAL.
*        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
*          EXPORTING
*            TITEL        = '确认'
*            TEXTLINE1    = '该行发票参考号为空，请去FBL1N检查确认！'
*            TEXTLINE2    = WA_BSEG-BELNR
*            START_COLUMN = 25
*            START_ROW    = 6.
*      ENDIF.
*    ENDIF.
*  ENDIF.
  "huanggl 2016.09.06
  CLEAR: it_return.
  CALL FUNCTION 'ZMM_UPDATE_DB_ZTABLE'
    TABLES
      it_ztkpqd = gt_ztkpqd
      it_ztbcqd = gt_ztbcqd
      it_return = it_return.
  IF it_return IS INITIAL.
    COMMIT WORK.
*    MESSAGE s064(zmm_msg) WITH p_input-z_dzdh.
  ELSE.
    READ TABLE it_return INTO wa_return WITH KEY type = c_msgty_error.
    IF sy-subrc = 0.
      ROLLBACK WORK.
      p_error = 'X'.
      p_msg = wa_return-message.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " FRM_UPDATE_DB_TABLE
*&---------------------------------------------------------------------*
*&      Form  FRM_UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_BSEG  text
*      -->PT_BSIS  text
*----------------------------------------------------------------------*
FORM frm_update_table  TABLES   pt_bseg STRUCTURE bseg
                                pt_bsis STRUCTURE bsis
                        USING   p_input STRUCTURE zsinterface_invi
                                p_error
                                p_msg.
  DATA: ps_bseg   TYPE bseg,
        ps_bsis   TYPE bsis.

  LOOP AT pt_bseg INTO ps_bseg.
    "20160923 HUANGGL
*    IF PS_BSEG-BUZEI = 001 AND P_REFNO IS NOT INITIAL AND P_GJAHR IS NOT INITIAL.
*      PS_BSEG-REBZG = P_REFNO.
*      PS_BSEG-REBZJ = P_GJAHR.
*      PS_BSEG-REBZZ = 001.
*    ENDIF.
    "20160923 HUANGGL
    CALL FUNCTION 'ENQUEUE_EZ_BSEG'
      EXPORTING
        mode_bseg      = 'E'
        mandt          = sy-mandt
        bukrs          = ps_bseg-bukrs
        belnr          = ps_bseg-belnr
        gjahr          = ps_bseg-gjahr
        buzei          = ps_bseg-buzei
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      MODIFY bseg FROM ps_bseg.
    ELSE.
      ROLLBACK WORK.
*      MESSAGE 'Failure on locking table' TYPE 'S' DISPLAY LIKE 'E'.
      p_msg =  'Failure on locking table' .
      p_error = 'X'.
      EXIT.
    ENDIF.
    CALL FUNCTION 'DEQUEUE_EZ_BSEG'
      EXPORTING
        mode_bseg = 'E'
        mandt     = sy-mandt
        bukrs     = ps_bseg-bukrs
        belnr     = ps_bseg-belnr
        gjahr     = ps_bseg-gjahr
        buzei     = ps_bseg-buzei.

  ENDLOOP.

  LOOP AT pt_bsis INTO ps_bsis.
    CALL FUNCTION 'ENQUEUE_EZ_BSIS'
      EXPORTING
        mode_bsis      = 'E'
        mandt          = sy-mandt
        bukrs          = ps_bsis-bukrs
        hkont          = ps_bsis-hkont
        augdt          = ps_bsis-augdt
        augbl          = ps_bsis-augbl
        zuonr          = ps_bsis-zuonr
        gjahr          = ps_bsis-gjahr
        belnr          = ps_bsis-belnr
        buzei          = ps_bsis-buzei
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      MODIFY bsis FROM ps_bsis.
    ELSE.
      ROLLBACK WORK.
      p_msg = 'Failure on locking table'.
      EXIT.
    ENDIF.
    CALL FUNCTION 'DEQUEUE_EZ_BSIS'
      EXPORTING
        mode_bsis = 'E'
        mandt     = sy-mandt
        bukrs     = ps_bsis-bukrs
        hkont     = ps_bsis-hkont
        augdt     = ps_bsis-augdt
        augbl     = ps_bsis-augbl
        zuonr     = ps_bsis-zuonr
        gjahr     = ps_bsis-gjahr
        belnr     = ps_bsis-belnr
        buzei     = ps_bsis-buzei.

  ENDLOOP.

ENDFORM.                    " FRM_UPDATE_TABLE
*---------------------------------------------------------------------*
*      Form RKWA_PARTITIONIEREN
*---------------------------------------------------------------------*
*      Teilt die alle Warenentnahmen in IT_RKWA auf.
*      Die Bl?cke werden in IT_BLOCK abgelegt.
* ---> IT_RKWA
* <--- IT_BLOCK
*---------------------------------------------------------------------*
FORM rkwa_partitionieren1 TABLES it_rkwa  TYPE typ_tab_rkwa
                                it_block TYPE typ_tab_block
                           USING p_error
                                 p_msg.

  DATA: s_block TYPE typ_block,
        s_rkwa  LIKE rkwa,
        idx     LIKE sy-tabix,
        wrbtr   LIKE rkwa-wrbtr,
        s_msg TYPE typ_msg.
  DATA: f_waers TYPE waers,
        f_object TYPE object_curro.
  DATA: f_kursf LIKE bkpf-kursf.
  f_object = 'BKPF'.

* baue IT_BLOCK auf (sortiert nach BUKRS/lifn2)
  CLEAR it_block.

* Currency expired --> S_RKWA contains new currency (e.g. EUR) -------*
  LOOP AT it_rkwa INTO s_rkwa.
    CALL FUNCTION 'CURRENCY_EXPIRATION_CHECK'
      EXPORTING
        currency      = s_rkwa-bwaer
        date          = sy-datlo
        object        = f_object
        bukrs         = s_rkwa-bukrs
      IMPORTING
        currency_new  = f_waers
      EXCEPTIONS
        error_message = 4.
    IF sy-subrc = 0.
      f_waers = s_rkwa-bwaer.
    ELSEIF f_waers IS INITIAL.
      MESSAGE s607 WITH 'RKWA_PARTITIONIEREN' INTO p_msg.
      p_error = 'X'.
      EXIT.
    ENDIF.
    IF s_rkwa-bwaer NE f_waers.
      CLEAR: f_kursf.
      CALL FUNCTION 'READ_EXCHANGE_RATE'
        EXPORTING
          date             = sy-datlo
          foreign_currency = s_rkwa-bwaer
          local_currency   = f_waers
        IMPORTING
          exchange_rate    = f_kursf
        EXCEPTIONS
          error_message    = 1.

      IF ( sy-subrc <> 0 ).
        MESSAGE ID sy-msgid TYPE 'S'
        NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO p_msg.
        p_error = 'X'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
        EXPORTING
          foreign_currency = s_rkwa-bwaer
          local_currency   = f_waers
          foreign_amount   = s_rkwa-wrbtr
          rate             = f_kursf
          date             = sy-datlo
        IMPORTING
          local_amount     = s_rkwa-wrbtr
        EXCEPTIONS
          error_message    = 1.

      IF ( sy-subrc <> 0 ).
        MESSAGE ID sy-msgid TYPE 'S'
        NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO p_msg.
        p_error = 'X'.
        EXIT.
      ENDIF.

      IF NOT s_rkwa-navnw IS INITIAL.
        CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
          EXPORTING
            foreign_currency = s_rkwa-bwaer
            local_currency   = f_waers
            foreign_amount   = s_rkwa-navnw
            rate             = f_kursf
            date             = sy-datlo
          IMPORTING
            local_amount     = s_rkwa-navnw
          EXCEPTIONS
            error_message    = 1.

        IF ( sy-subrc <> 0 ).
          MESSAGE ID sy-msgid TYPE 'S'
          NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO p_msg.
          p_error = 'X'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
    s_rkwa-bwaer = f_waers.

    READ TABLE it_block INTO s_block
                        WITH KEY bukrs = s_rkwa-bukrs
                                 lifnr = s_rkwa-lifnr
                                 bwaer = s_rkwa-bwaer
                        BINARY SEARCH.
    idx = sy-tabix.
    IF ( sy-subrc <> 0 ).              "Block noch nicht vorhanden
      CLEAR s_block.
      s_block-bstat = c_bstat_null.
      s_block-bukrs = s_rkwa-bukrs.
      s_block-lifnr = s_rkwa-lifnr.
      s_block-bwaer = s_rkwa-bwaer.
      APPEND s_rkwa TO s_block-it_rkwa.
      INSERT s_block INTO it_block INDEX idx.  "Tab. bleibt sortiert!
    ELSE.                              "Block bereits vorhanden
      APPEND s_rkwa TO s_block-it_rkwa.
      MODIFY it_block FROM s_block INDEX idx.
    ENDIF.
  ENDLOOP.

* rkwa_pruefen
  LOOP AT it_block INTO s_block.
    CLEAR: s_rkwa.
    LOOP AT s_block-it_rkwa INTO s_rkwa.
      IF ( s_rkwa-sobkz <> c_sobkz_kons ) AND
     ( s_rkwa-sobkz <> c_sobkz_pipe ).
        CLEAR s_msg.
        s_msg-msgid = c_msgid_m8.
        s_msg-msgty = c_msgty_error.
        s_msg-msgno = '008'.
        s_msg-msgv1 = 'RKWA_PARTITIONIEREN'.
        EXIT.
      ENDIF.
    ENDLOOP.
    MOVE-CORRESPONDING s_msg TO s_block.
    MODIFY it_block FROM s_block.
  ENDLOOP.

* Partitionen sortieren
  LOOP AT it_block INTO s_block.
    SORT s_block-it_rkwa BY mjahr mblnr zeile.
    MODIFY it_block FROM s_block.
  ENDLOOP.

ENDFORM.                    "rkwa_partitionieren
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_SUM_JE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_INPUT  text
*----------------------------------------------------------------------*
FORM frm_get_sum_je  USING    p_input STRUCTURE zsinterface_invi
                              p_se
                              p_bhsje
                              p_msg
                              p_error.

  DATA: lt_kpqd       TYPE STANDARD TABLE OF ztkpqd,
      lt_bcqd       TYPE STANDARD TABLE OF ztbcqd,
      ls_kpqd       TYPE ztkpqd,
      ls_bcqd       TYPE ztbcqd.
  DATA: l_kpqd_hsje     TYPE p DECIMALS 3,"含税金额
        l_kpqd_nhsje    TYPE p DECIMALS 3,"不含税金额
        l_bcqd_bcje     TYPE p DECIMALS 3,"补差金额
        l_kpqd_se       TYPE p DECIMALS 3,"税额
        l_bcqd_se       TYPE p DECIMALS 3."税额
  DATA: l_lifn2         TYPE lifn2.

  DATA: l_bcje          TYPE p DECIMALS 5,
        l_bcse          TYPE p DECIMALS 5.

***需要增加对非寄售的判断，如果单据已经被做掉的问题

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_input-lifn2
    IMPORTING
      output = l_lifn2.

  SELECT zmjahr zmblnr zzeile z_hsje zwrbtr z_sl zwerks
    INTO CORRESPONDING FIELDS OF TABLE lt_kpqd FROM ztkpqd
    WHERE z_dzdh = p_input-z_dzdh AND zlifn2 = l_lifn2 AND zbukrs = p_input-bukrs." AND ZBELNR = ''.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_ztbcqd FROM ztbcqd
    WHERE z_dzdh = p_input-z_dzdh AND zlifn2 = l_lifn2 AND zbukrs = p_input-bukrs."AND ZBELNR = ''.



  SORT lt_kpqd BY zmjahr zmblnr zzeile.
  DELETE ADJACENT DUPLICATES FROM lt_kpqd.

  IF lt_kpqd IS INITIAL AND gt_ztbcqd IS INITIAL.
    CONCATENATE '对账单号' p_input-z_dzdh '不存在' INTO p_msg.
    p_error = 'X'.
    EXIT.
  ENDIF.

  LOOP AT lt_kpqd INTO ls_kpqd.
    l_kpqd_se = l_kpqd_se + ls_kpqd-zwrbtr * ls_kpqd-z_sl.
    l_kpqd_nhsje = l_kpqd_nhsje + ls_kpqd-zwrbtr.
  ENDLOOP.

  LOOP AT gt_ztbcqd INTO ls_bcqd.
    l_bcqd_bcje = l_bcqd_bcje + ls_bcqd-z_bcje.
    l_bcqd_se = l_bcqd_se + ls_bcqd-z_bcje * ls_bcqd-z_sl.
  ENDLOOP.

  g_kp_je = l_kpqd_nhsje.
  g_kp_se = l_kpqd_se.
  g_bc_je = l_bcqd_bcje.
  g_bc_se = l_bcqd_se.
  p_bhsje = l_kpqd_nhsje + l_bcqd_bcje.
  p_se    = l_kpqd_se + l_bcqd_se.

ENDFORM.                    " FRM_GET_SUM_JE