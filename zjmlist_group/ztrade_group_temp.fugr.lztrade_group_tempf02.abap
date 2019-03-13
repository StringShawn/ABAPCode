*----------------------------------------------------------------------*
***INCLUDE LZTRADE_GROUPF02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tree1 .

  CREATE OBJECT g_custom_container1
    EXPORTING
      parent                      = cl_gui_custom_container=>screen1
      container_name              = 'TREE_CT1'
      repid                       = sy-repid
      dynnr                       = '0501'
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

  CREATE OBJECT g_simple_tree1
    EXPORTING
      parent                      = g_custom_container1
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

  DATA lt_tree_event TYPE cntl_simple_events WITH HEADER LINE.
  lt_tree_event-eventid = cl_gui_simple_tree=>eventid_node_double_click.
  lt_tree_event-appl_event = 'X'.
  APPEND lt_tree_event.

  CALL METHOD g_simple_tree1->set_registered_events
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

  g_simple_tree_event_handler1 = NEW zcl_simple_tree_event_handler( ).
  g_simple_tree_event_handler1->double_click_perform = 'TREE_NODE_CLICK1'.
  g_simple_tree_event_handler1->double_click_program = sy-repid.

  g_simple_tree_event_handler1->handle_event( cl_tree = g_simple_tree1 ).

  PERFORM add_tree_nodes.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tree2 .

  DATA node_key_table TYPE treev_nks WITH HEADER LINE.

  DATA docking TYPE REF TO cl_gui_docking_container.

  CREATE OBJECT g_custom_container2
    EXPORTING
      parent                      = cl_gui_custom_container=>screen1
      container_name              = 'TREE_CT2'
      repid                       = sy-repid
      dynnr                       = '0501'
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

  CREATE OBJECT g_simple_tree2
    EXPORTING
      parent                      = g_custom_container2
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

  DATA lt_tree_event TYPE cntl_simple_events WITH HEADER LINE.
  lt_tree_event-eventid = cl_gui_simple_tree=>eventid_node_double_click.
  lt_tree_event-appl_event = 'X'.
  APPEND lt_tree_event.

  CALL METHOD g_simple_tree2->set_registered_events
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

  g_simple_tree_event_handler2 = NEW zcl_simple_tree_event_handler( ).
  g_simple_tree_event_handler2->double_click_perform = 'TREE_NODE_CLICK2'.
  g_simple_tree_event_handler2->double_click_program = sy-repid.

  g_simple_tree_event_handler2->handle_event( cl_tree = g_simple_tree2 ).


  PERFORM add_tree_nodes.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tree3 .

  DATA node_key_table TYPE treev_nks WITH HEADER LINE.

  DATA docking TYPE REF TO cl_gui_docking_container.

  CREATE OBJECT g_custom_container3
    EXPORTING
      parent                      = cl_gui_custom_container=>screen1
      container_name              = 'TREE_CT3'
      repid                       = sy-repid
      dynnr                       = '0501'
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

  CREATE OBJECT g_simple_tree3
    EXPORTING
      parent                      = g_custom_container3
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

  DATA lt_tree_event TYPE cntl_simple_events WITH HEADER LINE.
  lt_tree_event-eventid = cl_gui_simple_tree=>eventid_node_double_click.
  lt_tree_event-appl_event = 'X'.
  APPEND lt_tree_event.

  CALL METHOD g_simple_tree3->set_registered_events
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

  g_simple_tree_event_handler3 = NEW zcl_simple_tree_event_handler( ).
  g_simple_tree_event_handler3->double_click_perform = 'TREE_NODE_CLICK3'.
  g_simple_tree_event_handler3->double_click_program = sy-repid.

  g_simple_tree_event_handler3->handle_event( cl_tree = g_simple_tree3 ).

  PERFORM add_tree_nodes.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  tree_node_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tree_node_click1 USING node_key TYPE tv_nodekey.

  READ TABLE gt_yhlist_node WITH KEY node_key = node_key.
  IF sy-subrc = 0.
    PERFORM transaction_tcode USING gt_yhlist_node-node_txt1 gt_yhlist_node-node_txt2.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  tree_node_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tree_node_click2 USING node_key TYPE tv_nodekey.


  READ TABLE gt_odlist_node WITH KEY node_key = node_key.
  IF sy-subrc = 0.

    PERFORM transaction_tcode USING gt_odlist_node-node_txt1 gt_odlist_node-node_txt2.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  tree_node_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tree_node_click3 USING node_key TYPE tv_nodekey.

  READ TABLE gt_thlist_node WITH KEY node_key = node_key.
  IF sy-subrc = 0.
    PERFORM transaction_tcode USING gt_thlist_node-node_txt1 gt_thlist_node-node_txt2.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  tree_node_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tree_node_click4 USING node_key TYPE tv_nodekey.

  READ TABLE gt_KYthlist_node WITH KEY node_key = node_key.
  IF sy-subrc = 0.
    PERFORM transaction_tcode USING gt_KYthlist_node-node_txt1 gt_KYthlist_node-node_txt2.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_JM_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OT_JM_PATH  text
*      -->P_LT_TRADE_WERKS_PATHN  text
*----------------------------------------------------------------------*
FORM handle_jm_path  TABLES ot_jm_path STRUCTURE zlsjm_path USING type TYPE char1 pathn TYPE char10 werks TYPE char4.

  DATA ls_path1 TYPE ztrade_path_t.
  DATA ls_path2 TYPE ztrade_path_t.
  DATA next_line TYPE i.
  DATA ogrup TYPE numc3.

  IF type = '1'.

    SELECT * INTO TABLE @DATA(ot_path)
     FROM ztrade_path_t WHERE pathn = @pathn ORDER BY step.

    LOOP AT ot_path INTO ls_path1.

      ogrup = ogrup + 1.

      next_line = sy-tabix + 1.
      READ TABLE ot_path INTO ls_path2 INDEX next_line.
      IF sy-subrc = 0.

        ot_jm_path-pathn = ls_path1-pathn.
        ot_jm_path-ogrup = ogrup.
        ot_jm_path-lifnr = ls_path1-lifnr.
        ot_jm_path-from_bukrs = ls_path1-bukrs.
        ot_jm_path-from_werks = ls_path1-werks.
        ot_jm_path-from_lgort = ls_path1-lgort.
        ot_jm_path-from_lgorh = ls_path1-lgorh.
        ot_jm_path-to_bukrs = ls_path2-bukrs.
        ot_jm_path-to_werks = ls_path2-werks.
        ot_jm_path-to_lgort = ls_path2-lgort.
        ot_jm_path-to_lgorh = ls_path2-lgorh.
        ot_jm_path-mlper = ls_path1-mlper.
        APPEND ot_jm_path.

      ELSE.

        ot_jm_path-ogrup = ogrup.
        ot_jm_path-lifnr = ls_path1-lifnr.
        ot_jm_path-from_bukrs = ls_path1-bukrs.
        ot_jm_path-from_werks = ls_path1-werks.
        ot_jm_path-from_lgort = ls_path1-lgort.
        ot_jm_path-from_lgorh = ls_path1-lgorh.
        ot_jm_path-to_werks = werks.
        APPEND ot_jm_path.

      ENDIF.

      CLEAR ot_jm_path.
    ENDLOOP.
  ELSEIF type = '2'.

    DATA ls_path TYPE ztrade_path_t.

    SELECT * INTO TABLE @ot_path
     FROM ztrade_path_t WHERE pathn = @pathn ORDER BY step DESCENDING.

    LOOP AT ot_path INTO ls_path1.

      ogrup = ogrup + 1.

      IF sy-tabix = 1.

        ls_path = ls_path1.
        ot_jm_path-ogrup = ogrup.
        ot_jm_path-lifnr = ls_path1-lifnr.
        ot_jm_path-from_bukrs = ls_path1-bukrs.
        ot_jm_path-from_werks = ls_path1-werks.
        ot_jm_path-from_lgort = ls_path1-lgort.
        ot_jm_path-from_lgorh = ls_path1-lgorh.
        ot_jm_path-to_werks = werks.
        APPEND ot_jm_path.
      ELSE.

        next_line = sy-tabix - 1.
        READ TABLE ot_path INTO ls_path2 INDEX next_line.
        IF sy-subrc = 0.
          ot_jm_path-pathn = ls_path2-pathn.
          ot_jm_path-ogrup = ogrup.

          ot_jm_path-from_bukrs = ls_path2-bukrs.
          ot_jm_path-from_werks = ls_path2-werks.
          ot_jm_path-from_lgort = ls_path2-lgort.
          ot_jm_path-from_lgorh = ls_path2-lgorh.
          ot_jm_path-to_bukrs = ls_path1-bukrs.
          ot_jm_path-to_werks = ls_path1-werks.
          ot_jm_path-to_lgort = ls_path1-lgort.
          ot_jm_path-to_lgorh = ls_path1-lgorh.
          ot_jm_path-lifnr = ls_path1-lifnr.
          ot_jm_path-mlper = ls_path1-mlper.
          APPEND ot_jm_path.
        ENDIF.
      ENDIF.

      CLEAR ot_jm_path.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECT_ODELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_list .

  DATA step_line TYPE i.
  DATA temp_pathn TYPE char10.

  GET CURSOR FIELD fid LINE cursor_line.

  CASE fid.
    WHEN 'GS_FH_LIST-BHNUC'.

      SELECT * INTO TABLE @DATA(lt_jmfh)
        FROM zsd_jiamengfahuo WHERE bhnuc = @gs_fh_list-bhnuc.

      CALL FUNCTION 'ZPOPUP_ALV'
        EXPORTING
          i_start_col = 10
          i_end_col   = 120
          i_start_row = 5
          i_end_row   = 15
        TABLES
          it_tab      = lt_jmfh.
    WHEN 'GS_FH_LIST-PATHN' OR 'GS_TH_LIST-PATHN'.

      CHECK gs_fh_list-pathn IS NOT INITIAL OR gs_th_list-pathn IS NOT INITIAL.
      IF gs_fh_list-pathn IS NOT INITIAL.
        temp_pathn = gs_fh_list-pathn.
      ELSEIF gs_th_list-pathn IS NOT INITIAL.
        temp_pathn = gs_th_list-pathn.
      ENDIF.

      operation_flag = 'S'.

      SELECT SINGLE * INTO sel_path_h FROM ztrade_path_h WHERE pathn = temp_pathn.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_path_t FROM ztrade_path_t WHERE pathn = sel_path_h-pathn.

      CALL SCREEN 0301 STARTING AT 40 1.
      CLEAR operation_flag.
      CLEAR sel_path_h.

    WHEN OTHERS.
  ENDCASE.




ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_TREE_NODES2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_tree_nodes .

  DATA node_key_table TYPE treev_nks WITH HEADER LINE.

  CASE sy-dynnr.
    WHEN 0601.

      CALL METHOD g_simple_tree1->add_nodes
        EXPORTING
          table_structure_name           = 'ZSIMPLE_TREENODES'
          node_table                     = gt_yhlist_node[]
        EXCEPTIONS
          failed                         = 1
          error_in_node_table            = 2
          dp_error                       = 3
          table_structure_name_not_found = 4
          OTHERS                         = 5.

      LOOP AT gt_yhlist_node WHERE expand_child = 'X'.
        node_key_table = gt_yhlist_node-node_key.
        APPEND node_key_table.
      ENDLOOP.
      g_simple_tree1->expand_nodes( node_key_table = node_key_table[] ).

    WHEN 0602.

      CALL METHOD g_simple_tree2->add_nodes
        EXPORTING
          table_structure_name           = 'ZSIMPLE_TREENODES'
          node_table                     = gt_odlist_node[]
        EXCEPTIONS
          failed                         = 1
          error_in_node_table            = 2
          dp_error                       = 3
          table_structure_name_not_found = 4
          OTHERS                         = 5.

      LOOP AT gt_odlist_node WHERE expand_child = 'X'.
        node_key_table = gt_odlist_node-node_key.
        APPEND node_key_table.
      ENDLOOP.
      g_simple_tree2->expand_nodes( node_key_table = node_key_table[] ).

    WHEN 0603.

      CALL METHOD g_simple_tree3->add_nodes
        EXPORTING
          table_structure_name           = 'ZSIMPLE_TREENODES'
          node_table                     = gt_thlist_node[]
        EXCEPTIONS
          failed                         = 1
          error_in_node_table            = 2
          dp_error                       = 3
          table_structure_name_not_found = 4
          OTHERS                         = 5.

      LOOP AT gt_thlist_node WHERE expand_child = 'X'.
        node_key_table = gt_thlist_node-node_key.
        APPEND node_key_table.
      ENDLOOP.
      g_simple_tree3->expand_nodes( node_key_table = node_key_table[] ).
    WHEN 0604.
       CALL METHOD g_simple_tree4->add_nodes
        EXPORTING
          table_structure_name           = 'ZSIMPLE_TREENODES'
          node_table                     = gt_kythlist_node[]
        EXCEPTIONS
          failed                         = 1
          error_in_node_table            = 2
          dp_error                       = 3
          table_structure_name_not_found = 4
          OTHERS                         = 5.

      LOOP AT gt_kythlist_node WHERE expand_child = 'X'.
        node_key_table = gt_kythlist_node-node_key.
        APPEND node_key_table.
      ENDLOOP.
      g_simple_tree4->expand_nodes( node_key_table = node_key_table[] ).
    WHEN OTHERS.
  ENDCASE.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TRANSACTION_TCODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_YHLIST_NODE_NODE_TXT1  text
*----------------------------------------------------------------------*
FORM transaction_tcode  USING  type TYPE char40 node_value TYPE zsimple_treenodes-node_txt2.

  CASE type.
    WHEN '1'.

      CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
        EXPORTING
          i_ebeln      = node_value+0(10)
          i_enjoy      = 'X'
        EXCEPTIONS
          not_found    = 1
          no_authority = 2
          invalid_call = 3
          OTHERS       = 4.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    WHEN '2'.

      SET PARAMETER ID 'AUN' FIELD node_value+0(10).
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
    WHEN '3'.

      SET PARAMETER ID 'VL' FIELD node_value+0(10).
      CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
    WHEN '4'.

      DATA temp_mjahr TYPE mjahr.
      temp_mjahr = node_value+10(4).
      CALL FUNCTION 'MIGO_DIALOG'
        EXPORTING
          i_action            = 'A04'
          i_refdoc            = 'R02'
          i_notree            = 'X'
          i_skip_first_screen = 'X'
          i_deadend           = 'X'
          i_mblnr             = node_value+0(10)
          i_mjahr             = temp_mjahr.

    WHEN '5'.

      SET PARAMETER ID 'BLN' FIELD node_value+0(10).
      SET PARAMETER ID 'BUK' FIELD node_value+14(4).
      SET PARAMETER ID 'GJR' FIELD node_value+10(4).

      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_TREE4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tree4 .

  DATA node_key_table TYPE treev_nks WITH HEADER LINE.

  DATA docking TYPE REF TO cl_gui_docking_container.

  CREATE OBJECT g_custom_container4
    EXPORTING
      parent                      = cl_gui_custom_container=>screen1
      container_name              = 'TREE_CT4'
      repid                       = sy-repid
      dynnr                       = '0501'
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

  CREATE OBJECT g_simple_tree4
    EXPORTING
      parent                      = g_custom_container4
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

  DATA lt_tree_event TYPE cntl_simple_events WITH HEADER LINE.
  lt_tree_event-eventid = cl_gui_simple_tree=>eventid_node_double_click.
  lt_tree_event-appl_event = 'X'.
  APPEND lt_tree_event.

  CALL METHOD g_simple_tree4->set_registered_events
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

  g_simple_tree_event_handler4 = NEW zcl_simple_tree_event_handler( ).
  g_simple_tree_event_handler4->double_click_perform = 'TREE_NODE_CLICK4'.
  g_simple_tree_event_handler4->double_click_program = sy-repid.

  g_simple_tree_event_handler4->handle_event( cl_tree = g_simple_tree4 ).

  PERFORM add_tree_nodes.
ENDFORM.
