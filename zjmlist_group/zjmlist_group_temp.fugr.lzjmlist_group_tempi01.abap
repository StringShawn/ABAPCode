*----------------------------------------------------------------------*
***INCLUDE LZJMLIST_GROUPI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.

  LEAVE PROGRAM.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101_exit INPUT.

  LEAVE PROGRAM.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TB_TAB_ACTIVE_TAB_SET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tb_tab_active_tab_set INPUT.

  ok_code = sy-ucomm.

  CASE sy-dynnr.
    WHEN 0101.

      CASE ok_code.
        WHEN c_tb_tab-tab1.
          g_tb_tab-pressed_tab = c_tb_tab-tab1.
        WHEN c_tb_tab-tab2.
          g_tb_tab-pressed_tab = c_tb_tab-tab2.
        WHEN c_tb_tab-tab3.
          g_tb_tab-pressed_tab = c_tb_tab-tab3.
        WHEN c_tb_tab-tab4.
          g_tb_tab-pressed_tab = c_tb_tab-tab4.
        WHEN c_tb_tab-tab5.
          g_tb_tab-pressed_tab = c_tb_tab-tab5.
        WHEN c_tb_tab-tab6.
          g_tb_tab-pressed_tab = c_tb_tab-tab6.
        WHEN c_tb_tab-tab7.
          g_tb_tab-pressed_tab = c_tb_tab-tab7.
        WHEN c_tb_tab-tab8.
          g_tb_tab-pressed_tab = c_tb_tab-tab8.
        WHEN c_tb_tab-tab9.
          g_tb_tab-pressed_tab = c_tb_tab-tab9.
        WHEN c_tb_tab-tab10.
          g_tb_tab-pressed_tab = c_tb_tab-tab10.
         "add by handyxh 20181112
        WHEN c_tb_tab-tab11.
          g_tb_tab-pressed_tab = c_tb_tab-tab11.
        WHEN c_tb_tab-tab12.
          g_tb_tab-pressed_tab = c_tb_tab-tab12.
        WHEN OTHERS.
      ENDCASE.
    WHEN 0207.

      CASE ok_code.
        WHEN c_tb_tab7-tab1.
          g_tb_tab7-pressed_tab = c_tb_tab7-tab1.
        WHEN c_tb_tab7-tab2.
          g_tb_tab7-pressed_tab = c_tb_tab7-tab2.
        WHEN OTHERS.
      ENDCASE.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LEAVE_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE leave_screen INPUT.

  CLEAR ls_revs.
  CLEAR ls_yh.
  LEAVE TO SCREEN 0.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0401  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0401 INPUT.

  CASE ok_code.
    WHEN 'CONF'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

  CLEAR ok_code.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0401  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0402 INPUT.

  CLEAR return_flag.

  CASE ok_code.
    WHEN 'CONF'.

      PERFORM config_cancle_date.

    WHEN OTHERS.
  ENDCASE.

  CLEAR ok_code.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0207  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0207 INPUT.

  CLEAR return_flag.

  CASE ok_code.
    WHEN 'SEAR'.

      PERFORM pop_selection_screen.
    WHEN ''.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
