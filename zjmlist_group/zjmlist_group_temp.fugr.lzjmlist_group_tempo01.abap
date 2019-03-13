*----------------------------------------------------------------------*
***INCLUDE LZJMLIST_GROUPO01.
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
*&      Module  INIT_TREE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_tree OUTPUT.

  PERFORM init_tree.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INIT_NODE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_node OUTPUT.

  PERFORM init_node.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  SET PF-STATUS '0101'.
  SET TITLEBAR '0101'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TB_TAB_ACTIVE_TAB_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tb_tab_active_tab_set OUTPUT.

  CASE sy-dynnr.
    WHEN 0101.

      jm_tab-activetab = g_tb_tab-pressed_tab.
      CASE jm_tab-activetab.
        WHEN c_tb_tab-tab1.
          g_tb_tab-subscreen = '0201'.
        WHEN c_tb_tab-tab2.
          g_tb_tab-subscreen = '0202'.
        WHEN c_tb_tab-tab3.
          g_tb_tab-subscreen = '0203'.
        WHEN c_tb_tab-tab4.
          g_tb_tab-subscreen = '0204'.
        WHEN c_tb_tab-tab5.
          g_tb_tab-subscreen = '0205'.
        WHEN c_tb_tab-tab6.
          g_tb_tab-subscreen = '0206'.
        WHEN c_tb_tab-tab7.
          g_tb_tab-subscreen = '0207'.
        WHEN c_tb_tab-tab8.
          g_tb_tab-subscreen = '0208'.
        WHEN c_tb_tab-tab9.
          g_tb_tab-subscreen = '0209'.
        WHEN c_tb_tab-tab10.
          g_tb_tab-subscreen = '0210'.
        "add by handyxh 20181112
        WHEN c_tb_tab-tab11.
          g_tb_tab-subscreen = '0211'.
        WHEN c_tb_tab-tab12.
          g_tb_tab-subscreen = '0212'.
        WHEN OTHERS.
      ENDCASE.
    WHEN 0207.

      stab7-activetab = g_tb_tab7-pressed_tab.
      CASE stab7-activetab.
        WHEN c_tb_tab7-tab1.
          g_tb_tab7-subscreen = '0501'.
        WHEN c_tb_tab7-tab2.
          g_tb_tab7-subscreen = '0502'.
        WHEN OTHERS.
      ENDCASE.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INIT_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_screen OUTPUT.

  CASE sy-dynnr.
    WHEN 0201.

      CHECK container1 IS NOT BOUND.

      PERFORM init_fcat.

      PERFORM init_alv.
    WHEN 0202.

      CHECK container2 IS NOT BOUND.

      PERFORM init_fcat.

      PERFORM init_alv.

    WHEN 0203.

      CHECK container3 IS NOT BOUND.

      PERFORM init_fcat.

      PERFORM init_alv.

    WHEN 0204.
    WHEN 0205.

      CHECK container5 IS NOT BOUND.

      PERFORM init_fcat.

      PERFORM init_alv.
    WHEN 0206.

      CHECK container6 IS NOT BOUND.

      PERFORM init_fcat.

      PERFORM init_alv.
    WHEN 0207.

      CHECK container7 IS NOT BOUND.

      PERFORM init_fcat.

      PERFORM init_alv.

    WHEN 0208.

      CHECK container8 IS NOT BOUND.

      PERFORM init_fcat.

      PERFORM init_alv.
    WHEN 0209.

      CHECK container9 IS NOT BOUND.

      PERFORM init_fcat.

      PERFORM init_alv.

    WHEN 0210.

      CHECK container10 IS NOT BOUND.

      PERFORM init_fcat.

      PERFORM init_alv.
    "add by handyxh 20181112
    WHEN 0211.

       CHECK container11 IS NOT BOUND.

       PERFORM init_fcat.

       PERFORM init_alv.
    WHEN 0212.

       CHECK container12 IS NOT BOUND.

       PERFORM init_fcat.

       PERFORM init_alv.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INIT_AUTH  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_tab OUTPUT.

  PERFORM init_tab.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0401  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0401 OUTPUT.
  SET PF-STATUS '0401'.
  SET TITLEBAR '0401'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0401  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0402 OUTPUT.
  SET PF-STATUS '0402'.
  SET TITLEBAR '0402'.
ENDMODULE.
