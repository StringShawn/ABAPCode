*----------------------------------------------------------------------*
***INCLUDE LZTRADE_GROUPO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TB_TAB_ACTIVE_TAB_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tb_tab_active_tab_set OUTPUT.

  CASE sy-dynnr.
    WHEN 0100.

      main_tab-activetab = g_tb_tab-pressed_tab.
      CASE main_tab-activetab.
        WHEN c_tb_tab-tab1.
          g_tb_tab-subscreen = '0201'.
        WHEN c_tb_tab-tab2.
          g_tb_tab-subscreen = '0202'.
        WHEN c_tb_tab-tab3.
          g_tb_tab-subscreen = '0203'.
        WHEN c_tb_tab-tab4.
          g_tb_tab-subscreen = '0204'.
        WHEN OTHERS.
      ENDCASE.

    WHEN 0501.

      list_tab-activetab = g_list_tab-pressed_tab.
      CASE list_tab-activetab.
        WHEN c_list_tab-tab1.
          g_list_tab-subscreen = '0601'.
        WHEN c_list_tab-tab2.
          g_list_tab-subscreen = '0602'.
        WHEN c_list_tab-tab3.
          g_list_tab-subscreen = '0603'.
         WHEN c_list_tab-tab4.
          g_list_tab-subscreen = '0604'.
        WHEN OTHERS.
      ENDCASE.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0301  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0301 OUTPUT.
  SET PF-STATUS '0301'.
  SET TITLEBAR '0301'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_screen OUTPUT.

  CASE sy-dynnr.
    WHEN '0301'.

      CHECK operation_flag = 'M' AND sel_path_h-status+0(4) = '@5B@' OR operation_flag = 'S'.
      LOOP AT SCREEN.
        IF screen-group1 = 'G1'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN '0302'.

      CHECK operation_flag = 'M'.
      IF sel_werks-noeff = 'X'.
        LOOP AT SCREEN.
          screen-input = 0.
          MODIFY SCREEN.
        ENDLOOP.
      ELSE.
        LOOP AT SCREEN.
          IF screen-group1 = 'G1'.
            screen-input = 0.
          ELSEIF screen-name = 'SEL_WERKS-NFDAT'.
            screen-input = 1.
          ENDIF.
          MODIFY SCREEN.
        ENDLOOP.
      ENDIF.

    WHEN '0602'.
      IF lt_return[] IS INITIAL.
        LOOP AT SCREEN.
          IF screen-name = 'ERRO'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.

    WHEN '0603'.
      IF lt_return[] IS INITIAL.
        LOOP AT SCREEN.
          IF screen-name = 'ERRO'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.

    WHEN '0604'.
      IF lt_return[] IS INITIAL.
        LOOP AT SCREEN.
          IF screen-name = 'ERRO'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_lines OUTPUT.

  CASE sy-dynnr.
    WHEN '0201'.
      path_h_tc-lines = lines( gt_path_h[] ).
    WHEN '0202'.
      werks_tc-lines = lines( gt_werks[] ).
    WHEN '0203'.
      lgort_tc-lines = lines( gt_lgort[] ).
    WHEN '0204'.

    WHEN '0301'.
      path_t_tc-lines = lines( gt_path_t[] ).
    WHEN '0303'.
      excel_tc-lines = lines( gt_excel[] ).
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INIT_TC_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_tc_screen OUTPUT.

  CASE sy-dynnr.
    WHEN '0301'.
      CHECK operation_flag = 'M' AND sel_path_h-status+0(4) = '@5B@' OR operation_flag = 'S'.
      LOOP AT SCREEN.
        screen-input = 0.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN ''.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0302  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0302 OUTPUT.
  SET PF-STATUS '0302'.
  SET TITLEBAR '0302'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0303  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0303 OUTPUT.
  SET PF-STATUS '0303'.
  SET TITLEBAR '0303'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0501  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0501 OUTPUT.
  SET PF-STATUS '0501'.
  SET TITLEBAR '0501'.

  IF gt_yhlist_node[] IS NOT INITIAL.
    g_list_tab-pressed_tab = 'TAB1'.
    IF gs_yh_list-status <> 'D' AND gs_yh_list-status <> 'F'.
      LOOP AT SCREEN.
        IF screen-name <> 'TAB1'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSEIF gt_odlist_node[] IS NOT INITIAL.
    g_list_tab-pressed_tab = 'TAB2'.
    IF gs_fh_list-stype <> '2'.
      LOOP AT SCREEN.
        IF screen-name <> 'TAB2'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT SCREEN.
        IF screen-name = 'TAB3'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSEIF gt_thlist_node[] IS NOT INITIAL.
    g_list_tab-pressed_tab = 'TAB3'.
    LOOP AT SCREEN.
      IF screen-name <> 'TAB3'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSEIF gt_kythlist_node[] IS NOT INITIAL.
    g_list_tab-pressed_tab = 'TAB4'.
    LOOP AT SCREEN.
      IF screen-name <> 'TAB4'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INIT_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_data OUTPUT.

  PERFORM init_data.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0502  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0502 OUTPUT.
  SET PF-STATUS '0502'.
  SET TITLEBAR '0502'.

  IF gt_jmat_t[] IS INITIAL.
    CLEAR gt_jmat_t.
    DO 3 TIMES.
      APPEND gt_jmat_t.
    ENDDO.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INIT_TREE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_tree OUTPUT.

  CASE sy-dynnr.
    WHEN 0601.
      IF g_custom_container1 IS NOT BOUND.
        PERFORM init_tree1.
      ENDIF.
    WHEN 0602.
      IF g_custom_container2 IS NOT BOUND.
        PERFORM init_tree2.
      ENDIF.
    WHEN 0603.
      IF g_custom_container3 IS NOT BOUND.
        PERFORM init_tree3.
      ENDIF.
    WHEN 0604.
      IF g_custom_container4 IS NOT BOUND.
        PERFORM init_tree4.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
