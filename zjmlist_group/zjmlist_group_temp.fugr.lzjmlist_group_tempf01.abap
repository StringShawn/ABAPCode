*----------------------------------------------------------------------*
***INCLUDE LZJMLIST_GROUPF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tree .

  DATA node_key_table TYPE treev_nks WITH HEADER LINE.

  CHECK g_custom_container IS NOT BOUND.

  CREATE OBJECT g_custom_container
    EXPORTING
      container_name              = 'WERKS_CT'
      repid                       = sy-repid
      dynnr                       = sy-dynnr
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CREATE OBJECT g_simple_tree
    EXPORTING
      parent                      = g_custom_container
      node_selection_mode         = cl_gui_simple_tree=>node_sel_mode_single
    EXCEPTIONS
      lifetime_error              = 1
      cntl_system_error           = 2
      create_error                = 3
      failed                      = 4
      illegal_node_selection_mode = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD g_simple_tree->add_nodes
    EXPORTING
      table_structure_name           = 'ZSIMPLE_TREENODES'
      node_table                     = gt_werks_node[]
    EXCEPTIONS
      failed                         = 1
      error_in_node_table            = 2
      dp_error                       = 3
      table_structure_name_not_found = 4
      OTHERS                         = 5.

  DATA lt_tree_event TYPE cntl_simple_events WITH HEADER LINE.
  lt_tree_event-eventid = cl_gui_simple_tree=>eventid_node_double_click.
  lt_tree_event-appl_event = 'X'.
  APPEND lt_tree_event.
  lt_tree_event-eventid = cl_gui_simple_tree=>eventid_expand_no_children.
  lt_tree_event-appl_event = 'X'.
  APPEND lt_tree_event.

  CALL METHOD g_simple_tree->set_registered_events
    EXPORTING
      events                    = lt_tree_event[]
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3
      OTHERS                    = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  g_simple_tree_event_handler = NEW zcl_simple_tree_event_handler( ).
  g_simple_tree_event_handler->double_click_perform = 'TREE_NODE_CLICK'.
  g_simple_tree_event_handler->double_click_program = sy-repid.
  g_simple_tree_event_handler->expand_nodes_perform = 'EXPAND_NODES'.
  g_simple_tree_event_handler->expand_nodes_program = sy-repid.
  g_simple_tree_event_handler->handle_event( cl_tree = g_simple_tree ).


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_NODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_node .


  DATA ls_node TYPE zsimple_treenodes.

  SELECT  b~bezei, a~vkbur_wrk INTO TABLE @DATA(lt_vkbur)
    FROM wrf1 AS a
    INNER JOIN tvkbt AS b ON a~vkbur_wrk = b~vkbur
    WHERE betrp IN ('Z003','Z004')
    GROUP BY a~vkbur_wrk,b~bezei.

  LOOP AT lt_vkbur INTO DATA(ls_vkbur).

    last_node_key = last_node_key + 1.

    ls_node-node_key = last_node_key.
    ls_node-isfolder = 'X'.
    ls_node-expander = 'X'.
    ls_node-node_txt1 = ls_vkbur-vkbur_wrk.
    ls_node-text = |{ ls_vkbur-vkbur_wrk }  { ls_vkbur-bezei }|.

    APPEND ls_node TO gt_werks_node.
    CLEAR ls_node.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXPAND_NO_CHILDREN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM expand_nodes USING node_key TYPE tv_nodekey .

  DATA ls_node TYPE zsimple_treenodes.
  DATA pt_werks_node LIKE TABLE OF zsimple_treenodes WITH HEADER LINE.

  READ TABLE gt_werks_node WITH KEY node_key = node_key.

  SELECT locnr AS werks, name1 AS werks_name INTO TABLE @DATA(lt_werks)
    FROM wrf1 AS a
    INNER JOIN t001w AS b ON a~locnr = b~werks
    WHERE a~vkbur_wrk = @gt_werks_node-node_txt1+0(4)
    ORDER BY a~locnr.

  LOOP AT lt_werks INTO DATA(ls_werks).
    last_node_key = last_node_key + 1.
    ls_node-node_key = last_node_key.
    ls_node-relatkey = node_key.
    ls_node-relatship = cl_gui_list_tree=>relat_last_child.
    ls_node-node_txt1 = ls_werks-werks.
    ls_node-text = |{ ls_werks-werks }   { ls_werks-werks_name }|.
    APPEND ls_node TO gt_werks_node.
    APPEND ls_node TO pt_werks_node.
  ENDLOOP.

  CALL METHOD g_simple_tree->add_nodes
    EXPORTING
      table_structure_name           = 'ZSIMPLE_TREENODES'
      node_table                     = pt_werks_node[]
    EXCEPTIONS
      failed                         = 1
      error_in_node_table            = 2
      dp_error                       = 3
      table_structure_name_not_found = 4
      OTHERS                         = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TREE_NODE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tree_node_click USING node_key TYPE tv_nodekey .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv .

  CASE sy-dynnr.
    WHEN 0201.

      layo1-zebra = 'X' .
      layo1-cwidth_opt = 'X' .
      layo1-no_rowmark = ' ' .
      layo1-sel_mode = 'A'.

      alv_event1 = NEW zcl_alv_event_receiver( ).
      alv_event1->g_program = sy-repid.
      alv_event1->handle_toolbar_perform = 'HANDLE_TOOLBAR1'.
      alv_event1->user_command_perform = 'USER_COMMAND1'.
      alv_event1->double_click_perform = 'DOUBLE_CLICK1'.

      CALL FUNCTION 'ZCREATE_ALV_CLASS'
        EXPORTING
          container_name = 'TAB1_CON'
          layout         = layo1
        TABLES
          lt_fcat        = fcat1
          it_data        = gt_spfh_data
        CHANGING
          cl_container   = container1
          cl_alv         = alv1
          cl_event       = alv_event1.

    WHEN 0202.

      layo2-zebra = 'X' .
      layo2-cwidth_opt = 'X' .
      layo2-no_rowmark = ' ' .
      layo2-sel_mode = 'A'.

      alv_event2 = NEW zcl_alv_event_receiver( ).
      alv_event2->g_program = sy-repid.
      alv_event2->handle_toolbar_perform = 'HANDLE_TOOLBAR2'.
      alv_event2->user_command_perform = 'USER_COMMAND2'.
      alv_event2->double_click_perform = 'DOUBLE_CLICK2'.

      CALL FUNCTION 'ZCREATE_ALV_CLASS'
        EXPORTING
          container_name = 'TAB2_CON'
          layout         = layo2
        TABLES
          lt_fcat        = fcat2
          it_data        = gt_fspfh_data
        CHANGING
          cl_container   = container2
          cl_alv         = alv2
          cl_event       = alv_event2.

    WHEN 0203.

      layo3-zebra = 'X' .
      layo3-cwidth_opt = 'X' .
      layo3-no_rowmark = ' ' .
      layo3-sel_mode = 'A'.

      alv_event3 = NEW zcl_alv_event_receiver( ).
      alv_event3->g_program = sy-repid.
      alv_event3->handle_toolbar_perform = 'HANDLE_TOOLBAR3'.
      alv_event3->user_command_perform = 'USER_COMMAND3'.
      alv_event3->double_click_perform = 'DOUBLE_CLICK3'.

      CALL FUNCTION 'ZCREATE_ALV_CLASS'
        EXPORTING
          container_name = 'TAB3_CON'
          layout         = layo3
        TABLES
          lt_fcat        = fcat3
          it_data        = gt_th_data
        CHANGING
          cl_container   = container3
          cl_alv         = alv3
          cl_event       = alv_event3.

    WHEN 0204.
    WHEN 0205.

      layo5-zebra = 'X' .
      layo5-cwidth_opt = 'X' .
      layo5-no_rowmark = ' ' .
      layo5-sel_mode = 'A'.

      alv_event5 = NEW zcl_alv_event_receiver( ).
      alv_event5->g_program = sy-repid.
      alv_event5->handle_toolbar_perform = 'HANDLE_TOOLBAR5'.
      alv_event5->user_command_perform = 'USER_COMMAND5'.
      alv_event5->double_click_perform = 'DOUBLE_CLICK5'.

      CALL FUNCTION 'ZCREATE_ALV_CLASS'
        EXPORTING
          container_name = 'TAB5_CON'
          layout         = layo5
        TABLES
          lt_fcat        = fcat5
          it_data        = gt_spyh_data
        CHANGING
          cl_container   = container5
          cl_alv         = alv5
          cl_event       = alv_event5.

    WHEN 0206.

      layo6-zebra = 'X' .
      layo6-cwidth_opt = 'X' .
      layo6-no_rowmark = ' ' .
      layo6-sel_mode = 'A'.

      alv_event6 = NEW zcl_alv_event_receiver( ).
      alv_event6->g_program = sy-repid.
      alv_event6->handle_toolbar_perform = 'HANDLE_TOOLBAR6'.
      alv_event6->user_command_perform = 'USER_COMMAND6'.
      alv_event6->double_click_perform = 'DOUBLE_CLICK6'.

      CALL FUNCTION 'ZCREATE_ALV_CLASS'
        EXPORTING
          container_name = 'TAB6_CON'
          layout         = layo6
        TABLES
          lt_fcat        = fcat6
          it_data        = gt_fspyh_data
        CHANGING
          cl_container   = container6
          cl_alv         = alv6
          cl_event       = alv_event6.

    WHEN 0207.

      layo7-zebra = 'X' .
      layo7-cwidth_opt = 'X' .
      layo7-no_rowmark = ' ' .
      layo7-sel_mode = 'A'.

      alv_event7 = NEW zcl_alv_event_receiver( ).
      alv_event7->g_program = sy-repid.
      alv_event7->handle_toolbar_perform = 'HANDLE_TOOLBAR7'.
      alv_event7->user_command_perform = 'USER_COMMAND7'.
      alv_event7->double_click_perform = 'DOUBLE_CLICK7'.

      CALL FUNCTION 'ZCREATE_ALV_CLASS'
        EXPORTING
          container_name = 'TAB7_CON'
          layout         = layo7
        TABLES
          lt_fcat        = fcat7
          it_data        = gt_belnr_sum
        CHANGING
          cl_container   = container7
          cl_alv         = alv7
          cl_event       = alv_event7.

    WHEN 0208.

      layo8-zebra = 'X' .
      layo8-cwidth_opt = 'X' .
      layo8-no_rowmark = ' ' .
      layo8-sel_mode = 'A'.

      alv_event8 = NEW zcl_alv_event_receiver( ).
      alv_event8->g_program = sy-repid.
      alv_event8->handle_toolbar_perform = 'HANDLE_TOOLBAR8'.
      alv_event8->double_click_perform = 'DOUBLE_CLICK8'.
      alv_event8->user_command_perform = 'USER_COMMAND8'.

      CALL FUNCTION 'ZCREATE_ALV_CLASS'
        EXPORTING
          container_name = 'TAB8_CON'
          layout         = layo8
        TABLES
          lt_fcat        = fcat8
          it_data        = gt_spfh_head
        CHANGING
          cl_container   = container8
          cl_alv         = alv8
          cl_event       = alv_event8.

    WHEN 0209.

      layo9-zebra = 'X' .
      layo9-cwidth_opt = 'X' .
      layo9-no_rowmark = ' ' .
      layo9-sel_mode = 'A'.

      alv_event9 = NEW zcl_alv_event_receiver( ).
      alv_event9->g_program = sy-repid.
      alv_event9->handle_toolbar_perform = 'HANDLE_TOOLBAR9'.
      alv_event9->double_click_perform = 'DOUBLE_CLICK9'.
      alv_event9->user_command_perform = 'USER_COMMAND9'.

      CALL FUNCTION 'ZCREATE_ALV_CLASS'
        EXPORTING
          container_name = 'TAB9_CON'
          layout         = layo9
        TABLES
          lt_fcat        = fcat9
          it_data        = gt_fspfh_head
        CHANGING
          cl_container   = container9
          cl_alv         = alv9
          cl_event       = alv_event9.

    WHEN 0210.

      layo10-zebra = 'X' .
      layo10-cwidth_opt = 'X' .
      layo10-no_rowmark = ' ' .
      layo10-sel_mode = 'A'.

      alv_event10 = NEW zcl_alv_event_receiver( ).
      alv_event10->g_program = sy-repid.
      alv_event10->handle_toolbar_perform = 'HANDLE_TOOLBAR10'.
      alv_event10->user_command_perform = 'USER_COMMAND10'.

      CALL FUNCTION 'ZCREATE_ALV_CLASS'
        EXPORTING
          container_name = 'TAB10_CON'
          layout         = layo10
        TABLES
          lt_fcat        = fcat10
          it_data        = gt_fh_ml
        CHANGING
          cl_container   = container10
          cl_alv         = alv10
          cl_event       = alv_event10.

    "add by handyxh 20181112
    WHEN 0211.

      layo11-zebra = 'X' .
      layo11-cwidth_opt = 'X' .
      layo11-no_rowmark = ' ' .
      layo11-sel_mode = 'A'.

      alv_event11 = NEW zcl_alv_event_receiver( ).
      alv_event11->g_program = sy-repid.
      alv_event11->handle_toolbar_perform = 'HANDLE_TOOLBAR11'.
      alv_event11->user_command_perform = 'USER_COMMAND11'.
      alv_event11->double_click_perform = 'DOUBLE_CLICK11'.

      CALL FUNCTION 'ZCREATE_ALV_CLASS'
        EXPORTING
          container_name = 'TAB11_CON'
          layout         = layo11
        TABLES
          lt_fcat        = fcat11
          it_data        = gt_kyth_data_temp
        CHANGING
          cl_container   = container11
          cl_alv         = alv11
          cl_event       = alv_event11.
     WHEN 0212.

      layo12-zebra = 'X' .
      layo12-cwidth_opt = 'X' .
      layo12-no_rowmark = ' ' .
      layo12-sel_mode = 'A'.

      alv_event12 = NEW zcl_alv_event_receiver( ).
      alv_event12->g_program = sy-repid.
      alv_event12->handle_toolbar_perform = 'HANDLE_TOOLBAR12'.
      alv_event12->user_command_perform = 'USER_COMMAND12'.

      CALL FUNCTION 'ZCREATE_ALV_CLASS'
        EXPORTING
          container_name = 'TAB12_CON'
          layout         = layo12
        TABLES
          lt_fcat        = fcat12
          it_data        = gt_kyth_data_temp1
        CHANGING
          cl_container   = container12
          cl_alv         = alv12
          cl_event       = alv_event12.
    WHEN OTHERS.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_fcat .

  CASE sy-dynnr.
    WHEN 0201.

      REFRESH fcat1.

      add_field 'JMSTU_TXT' '单据状态'.
      add_field 'STYPE_TXT' '单据类型'.
      add_field 'WERKS' '门店编号'.
      add_field 'WERKS_NAME' '门店描述'.
      add_field 'FWERK' '发货工厂'.
      add_field 'BHNUC' '发货单号'.
      add_field 'BHITEM' '发货行'.
      add_field 'CHARG' '批次'.
      add_field 'Z_JTM' '旧条码'.
      add_field 'MATNR' '物料号'.
      add_field 'MAKTX' '物料描述'.
      add_field 'FREE_TXT' '是否赠品'.
      add_field 'DB_AMOUNT' '调拨价（含税）'.
      add_field 'SG_AMOUNT' '上柜价'.
      add_field 'MENGE' '数量'.
      add_field 'ALL_AMOUNT' '总金额'.
      add_field 'CRDAT' '暂存日期'.
      add_field 'CRTIM' '暂存时间'.
      add_field 'CRNAM' '暂存人员'.
      add_field 'FHDAT' '发货日期'.

      fcat1[] = gt_fieldcat[].

      REFRESH gt_fieldcat.

    WHEN 0202.

      REFRESH fcat2.

      add_field 'JMSTU_TXT' '单据状态'.
      add_field 'STYPE_TXT' '单据类型'.
      add_field 'WERKS' '门店编号'.
      add_field 'WERKS_NAME' '门店描述'.
      add_field 'FWERK' '发货工厂'.
      add_field 'BHNUC' '发货单号'.
      add_field 'BHITEM' '发货行'.
      add_field 'ODELN' '交货单号'.
      add_field 'CHARG' '批次'.
      add_field 'MATNR' '物料号'.
      add_field 'MAKTX' '物料描述'.
      add_field 'VTEXT' '产品层次'.
      add_field 'ZMAX_MENGE' '最高要货量'.
      add_field 'ZMIN_MENGE' '最低要货量'.
      add_field 'ZMIN_PKG_MENGE' '最小包装数量'.
      add_field 'ZPP_SCOPE' '品牌使用范围'.
      add_field 'FREE_TXT' '是否赠品'.
      add_field 'DB_AMOUNT' '调拨价（含税）'.
      add_field 'MENGE' '数量'.
      add_field 'ALL_AMOUNT' '总金额'.
      add_field 'CRDAT' '暂存日期'.
      add_field 'CRTIM' '暂存时间'.
      add_field 'CRNAM' '暂存人员'.
      add_field 'FHDAT' '发货日期'.

      fcat2[] = gt_fieldcat[].

      REFRESH gt_fieldcat.

    WHEN 0203.

      REFRESH fcat3.

      add_field 'THSTU_TXT' '退货状态'.

      add_field 'ZTHLX_TXT' '单据类型'.
      add_field 'EBELN' '退货单号'.
      add_field 'EBELP' '退货行号'.
      add_field 'PATHN' '退货路径'.
      add_field 'WERKS' '门店编号'.
      add_field 'FWERK' '起始门店'.
      add_field 'WERKS_NAME' '门店描述'.
      add_field 'CHARG' '批次'.
      add_field 'Z_JTM' '旧条码'.
      add_field 'MATNR' '物料号'.
      add_field 'MAKTX' '物料描述'.
      add_field 'AMFLG_TXT' '金额来源'.
      add_field 'HISLG_TXT' '销售金额是否被更改'.
      add_field 'HISPR' '上次销售金额'.
      add_field 'LESPR' '退货折扣金额'.
      add_field 'REDPR' '货品扣减金额'.
      add_field 'MENGE' '数量'.
      add_field 'Z_PP' '品牌'.
      add_field 'Z_ZPP' '子品牌'.
      add_field 'Z_SGJ' '上柜价'.
      add_field 'MLPER' '毛利率'.
      add_field 'CRDAT' '退货创建日期'.
      add_field 'CRTIM' '退货创建时间'.

      fcat3[] = gt_fieldcat[].
      REFRESH gt_fieldcat.

    WHEN 0204.
    WHEN 0205.

      REFRESH fcat5.

      add_field 'STATUS_TXT' '单据状态'.
      add_field 'YHDLN' '发货单号'.
      add_field 'YHDLP' '发货行'.
      add_field 'WERKS' '门店编号'.
      add_field 'WERKS_NAME' '门店描述'.
      add_field 'YHDTY_TXT' '是否信贷'.
      add_field 'CHARG' '批次'.
      add_field 'MATNR' '物料号'.
      add_field 'MAKTX' '物料描述'.
      add_field 'DB_AMOUNT' '调拨价（含税）'.
      add_field 'SG_AMOUNT' '上柜价（含税）'.
      add_field 'MENGE' '数量'.
      add_field 'AMTCH_TXT' '调拨价是否发生更改'.
      add_field 'CRDAT' '创建日期'.
      add_field 'CRTIM' '创建时间'.
      add_field 'SPDAT' '审批日期'.
      add_field 'SPTIM' '审批时间'.

      fcat5[] = gt_fieldcat[].

      REFRESH gt_fieldcat.

    WHEN 0206.

      REFRESH fcat6.

      add_field 'STATUS_TXT' '单据状态'.
      add_field 'YHDLN' '发货单号'.
      add_field 'YHDLP' '发货行'.
      add_field 'WERKS' '门店编号'.
      add_field 'WERKS_NAME' '门店描述'.
      add_field 'MATNR' '物料号'.
      add_field 'MAKTX' '物料描述'.
      add_field 'DB_AMOUNT' '调拨价（含税）'.
      add_field 'MENGE' '数量'.
      add_field 'CRDAT' '创建日期'.
      add_field 'CRTIM' '创建时间'.
      add_field 'SPDAT' '审批日期'.
      add_field 'SPTIM' '审批时间'.

      fcat6[] = gt_fieldcat[].

      REFRESH gt_fieldcat.
    WHEN 0207.

      REFRESH fcat7.

      add_field 'KUNNR' '加盟商编号'.
      add_field 'KUNNR_NAME' '加盟商描述'.
      add_field 'WERKS' '门店编号'.
      add_field 'WERKS_NAME' '门店描述'.
      add_field 'BUKRS' '关联公司'.
      add_field 'BUKRS_NAME' '公司描述'.
      add_field 'SPYH_NF_AMOUNT' '商品要货未发金额'.
      add_field 'SPYH_YF_AMOUNT' '非商品要货未发货金额'.
      add_field 'FSPYH_NF_AMOUNT' '商品要货已发货金额'.
      add_field 'FSPYH_YF_AMOUNT' '非商品要货已发货金额'.
      add_field 'SPBH_AMOUNT' '商品补货已发货金额'.
      add_field 'FSPBH_AMOUNT' '非商品补货已发货金额'.
      add_field 'OA_NF_AMOUNT' 'OA非商品未发货金额'.
      add_field 'OA_YF_AMOUNT' 'OA非商品已发货金额'.

      add_field 'SPGK_AMOUNT' '商品改款金额'.
      add_field 'SPWX_AMOUNT' '商品维修金额'.

      add_field 'SPTH_AMOUNT' '商品退货金额'.

      fcat7[] = gt_fieldcat[].

      REFRESH gt_fieldcat.

    WHEN 0208.

      REFRESH fcat8.

      add_field 'JMSTU_TXT' '单据状态'.
      add_field 'STYPE_TXT' '单据类型'.
      add_field 'WERKS' '门店编号'.
      add_field 'WERKS_NAME' '门店描述'.
      add_field 'FWERK' '发货工厂'.
      add_field 'BHNUC' '发货单号'.

      add_field 'ALL_AMOUNT' '总金额'.
      add_field 'CRDAT' '暂存日期'.
      add_field 'CRTIM' '暂存时间'.
      add_field 'CRNAM' '暂存人员'.
      add_field 'FHDAT' '发货日期'.
      add_field 'FHTIM' '发货时间'.
      add_field 'FHNAM' '发货人员'.

      fcat8[] = gt_fieldcat[].

      REFRESH gt_fieldcat.
    WHEN 0209.

      REFRESH fcat9.

      add_field 'JMSTU_TXT' '单据状态'.
      add_field 'STYPE_TXT' '单据类型'.
      add_field 'WERKS' '门店编号'.
      add_field 'WERKS_NAME' '门店描述'.
      add_field 'FWERK' '发货工厂'.
      add_field 'BHNUC' '发货单号'.

      add_field 'ALL_AMOUNT' '总金额'.
      add_field 'CRDAT' '暂存日期'.
      add_field 'CRTIM' '暂存时间'.
      add_field 'CRNAM' '暂存人员'.
      add_field 'FHDAT' '发货日期'.
      add_field 'FHTIM' '发货时间'.
      add_field 'FHNAM' '发货人员'.

      fcat9[] = gt_fieldcat[].

      REFRESH gt_fieldcat.

    WHEN 0210 OR 0310.

      REFRESH fcat10.
      CASE p_sum10.
        WHEN 1.

          add_field 'WERKS' '门店编号'.
          add_field 'WERKS_NAME' '门店描述'.
          add_field 'VKBUR' '中心编码'.
          add_field 'VKBUR_TXT' '中心描述'.
        WHEN 2.

          add_field 'VKBUR' '中心编码'.
          add_field 'VKBUR_TXT' '中心描述'.
        WHEN 3.
        WHEN OTHERS.
      ENDCASE.

      add_field 'DB_AMOUNT' '调拨金额'.
      add_field 'MLPER' '毛利率'.

      fcat10[] = gt_fieldcat[].

      REFRESH gt_fieldcat.
      "add by handyxh 20181112
    WHEN 0211.
      REFRESH fcat11.
      add_field 'STATUS' '退货状态'.
      add_field 'BHNUC' '原发货单号'.
      add_field 'WERKS' '门店编号'.
      add_field 'WERKS_NAME' '门店描述'.
      add_field 'MATNR' '物料编码'.
      add_field 'MAKTX' '物料描述'.
      add_field 'CHARG' '批次'.
      add_field 'MENGE' '数量'.

      fcat11[] = gt_fieldcat[].
      LOOP AT fcat11 WHERE fieldname = 'MENGE'.
        fcat11-edit = 'X'.
        fcat11-ref_table = 'MSEG'.
        fcat11-ref_field = 'MENGE'.
        MODIFY fcat11.
      ENDLOOP.
      REFRESH gt_fieldcat.
    WHEN 0212.
      REFRESH fcat12.
      add_field 'STATUS' '退货状态'.
      add_field 'BHNUC' '原发货单号'.
      add_field 'WERKS' '门店编号'.
      add_field 'WERKS_NAME' '门店描述'.
      add_field 'MATNR' '物料编码'.
      add_field 'MAKTX' '物料描述'.
      add_field 'CHARG' '批次'.
      add_field 'MENGE' '数量'.

      fcat12[] = gt_fieldcat[].
      REFRESH gt_fieldcat.
    WHEN OTHERS.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tab .

  DATA position TYPE i.
  DATA tab_str TYPE char5.
  DATA tab_active_str TYPE char20.

  DO 12 TIMES.   "增加非商品跨月退货 handyxh 20181112
    tab_str = 'TAB' && sy-index.

    position = sy-index - 1.
    AUTHORITY-CHECK OBJECT 'ZJMTAB' ID 'TAB_AUTH' FIELD tab_str.
    IF sy-subrc <> 0.
      tab_active_str+position(1) = 'X'.
    ELSEIF g_tb_tab-pressed_tab = space.
      g_tb_tab-pressed_tab = tab_str.
    ENDIF.
  ENDDO.

  LOOP AT SCREEN.
    IF screen-name+0(3) = 'TAB'.
      position = screen-name+3(2) - 1.
      IF tab_active_str+position(1) = 'X'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
