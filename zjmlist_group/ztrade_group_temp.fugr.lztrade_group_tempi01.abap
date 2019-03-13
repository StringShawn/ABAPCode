*----------------------------------------------------------------------*
***INCLUDE LZTRADE_GROUPI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CLEAR ok_code.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.

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

    WHEN '0100'.
      CASE ok_code.
        WHEN c_tb_tab-tab1.
          g_tb_tab-pressed_tab = c_tb_tab-tab1.
        WHEN c_tb_tab-tab2.
          g_tb_tab-pressed_tab = c_tb_tab-tab2.
        WHEN c_tb_tab-tab3.
          g_tb_tab-pressed_tab = c_tb_tab-tab3.
        WHEN c_tb_tab-tab4.
          g_tb_tab-pressed_tab = c_tb_tab-tab4.
        WHEN OTHERS.
      ENDCASE.

    WHEN '0501'.
      CASE ok_code.
        WHEN c_list_tab-tab1.
          g_list_tab-pressed_tab = c_list_tab-tab1.
        WHEN c_list_tab-tab2.
          g_list_tab-pressed_tab = c_list_tab-tab2.
        WHEN c_list_tab-tab3.
          g_list_tab-pressed_tab = c_list_tab-tab3.
        WHEN OTHERS.
      ENDCASE.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0201  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0201 INPUT.

  CLEAR return_flag.

  CASE ok_code.
    WHEN 'CREA'.

      PERFORM init_new_path.

      CALL SCREEN 0301 STARTING AT 40 1.

    WHEN 'SELE'.

      PERFORM get_select_path.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LEAVE_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE leave_screen INPUT.

  LEAVE TO SCREEN 0.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_TABLE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_table INPUT.

  CASE sy-dynnr.
    WHEN '0301'.
      MODIFY gt_path_t INDEX path_t_tc-current_line.
    WHEN '0202'.
      MODIFY gt_werks INDEX werks_tc-current_line.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STEP_ACTION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE step_action INPUT.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'TYPID'
      value_org       = 'S'
    TABLES
      value_tab       = lt_step
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PATYP_ACTION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE patyp_action INPUT.

  IF lt_patyp[] IS INITIAL.
    lt_patyp-typid = '1'.
    lt_patyp-typname = '直营路径'.
    APPEND lt_patyp.

    lt_patyp-typid = '2'.
    lt_patyp-typname = '加盟路径'.
    APPEND lt_patyp.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'TYPID'
      value_org       = 'S'
    TABLES
      value_tab       = lt_patyp
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_PATH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_path INPUT.

  IF gt_path_t-bukrs IS NOT INITIAL.

    SELECT SINGLE bukrs INTO gt_path_t-bukrs FROM t001 WHERE bukrs = gt_path_t-bukrs.
    IF sy-subrc <> 0.
      MESSAGE '公司编号不存在' TYPE 'E'.
    ENDIF.

  ENDIF.

  IF gt_path_t-werks IS NOT INITIAL AND gt_path_t-bukrs IS NOT INITIAL.

    SELECT SINGLE bwkey INTO gt_path_t-werks FROM t001k WHERE bwkey = gt_path_t-werks AND bukrs = gt_path_t-bukrs.
    IF sy-subrc <> 0.
      MESSAGE |工厂编号 { gt_path_t-werks } 在公司 { gt_path_t-bukrs } 中不存在| TYPE 'E'.
    ENDIF.

  ELSEIF gt_path_t-werks IS NOT INITIAL.
    SELECT SINGLE werks INTO gt_path_t-werks FROM t001w WHERE werks = gt_path_t-werks.
    IF sy-subrc <> 0.
      MESSAGE '工厂编号不存在' TYPE 'E'.
    ENDIF.
  ENDIF.

  IF gt_path_t-werks IS NOT INITIAL AND gt_path_t-lgort IS NOT INITIAL.

    SELECT SINGLE lgort INTO gt_path_t-lgort FROM t001l WHERE werks = gt_path_t-werks AND lgort = gt_path_t-lgort.

    IF sy-subrc <> 0.
      MESSAGE |商品库存地点 { gt_path_t-lgort } 在工厂 { gt_path_t-werks } 中不存在| TYPE 'E'.
    ENDIF.
  ELSEIF gt_path_t-lgort IS NOT INITIAL.

    SELECT SINGLE lgort INTO gt_path_t-lgort FROM t001l WHERE lgort = gt_path_t-lgort.
    IF sy-subrc <> 0.
      MESSAGE '商品库存地点不存在' TYPE 'E'.
    ENDIF.
  ENDIF.

  IF gt_path_t-werks IS NOT INITIAL AND gt_path_t-lgorh IS NOT INITIAL.

    SELECT SINGLE lgort INTO gt_path_t-lgorh FROM t001l WHERE werks = gt_path_t-werks AND lgort = gt_path_t-lgorh.

    IF sy-subrc <> 0.
      MESSAGE |非商品库存地点 { gt_path_t-lgorh } 在工厂 { gt_path_t-werks } 中不存在| TYPE 'E'.
    ENDIF.
  ELSEIF gt_path_t-lgorh IS NOT INITIAL.

    SELECT SINGLE lgort INTO gt_path_t-lgorh FROM t001l WHERE lgort = gt_path_t-lgorh.
    IF sy-subrc <> 0.
      MESSAGE '非商品库存地点不存在' TYPE 'E'.
    ENDIF.
  ENDIF.

  IF gt_path_t-lifnr IS NOT INITIAL.
    gt_path_t-lifnr = |{ gt_path_t-lifnr ALPHA = IN }|.
    SELECT SINGLE lifnr INTO gt_path_t-lifnr FROM lfa1 WHERE lifnr = gt_path_t-lifnr.
    IF sy-subrc <> 0.
      MESSAGE '供应商编号不存在' TYPE 'E'.
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0301 INPUT.

  CLEAR return_flag.

  PERFORM check_step.

  CHECK return_flag IS INITIAL.

  CASE ok_code.
    WHEN 'SAVE'.

      PERFORM check_path_data.

      CHECK return_flag IS INITIAL.

      PERFORM save_path_data.

      CHECK return_flag IS INITIAL.

      LEAVE TO SCREEN 0.

    WHEN ''.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0202  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0202 INPUT.

  CLEAR return_flag.

  CASE ok_code.
    WHEN 'ENTER'.

      PERFORM filter_werks_data.

    WHEN 'CREA'.

      operation_flag = 'C'.
      CLEAR sel_werks.
      sel_werks-endat = '99991231'.

      CALL SCREEN 0302 STARTING AT 30 1.

      IF save_flag = 'X'.


        MESSAGE '保存成功' TYPE 'S'.
        CLEAR save_flag.
        CHECK p_pathn IS NOT INITIAL OR p_werks IS NOT INITIAL.
        PERFORM filter_werks_data.

      ENDIF.

    WHEN 'SELE'.

      PERFORM get_select_werks.

    WHEN 'IMPO'.

      CALL SCREEN 0303 STARTING AT 30 1.

      IF save_flag = 'X'.

        MESSAGE '保存成功' TYPE 'I'.
        CLEAR save_flag.
        CHECK p_pathn IS NOT INITIAL OR p_werks IS NOT INITIAL.
        PERFORM filter_werks_data.

      ENDIF.

    WHEN 'NOWK'.

      PERFORM sele_cancle_data.

      CHECK return_flag IS INITIAL.

      PERFORM cancle_werks_data.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0302  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0302 INPUT.

  CLEAR return_flag.

  CASE ok_code.
    WHEN 'SAVE'.

      PERFORM check_werks_data.

      CHECK return_flag IS INITIAL.

      PERFORM save_werks_data.

      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

  CLEAR ok_code.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_PATHN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pathn INPUT.

  IF sel_werks-pathn IS INITIAL.
    CLEAR sel_werks-path_txt.
  ELSE.
    TRANSLATE sel_werks-pathn TO UPPER CASE.

    SELECT SINGLE path_txt INTO sel_werks-path_txt FROM ztrade_path_h WHERE pathn = sel_werks-pathn.
    IF sy-subrc <> 0.
      MESSAGE |路径编号 { sel_werks-pathn } 不存在| TYPE 'E'.
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_WERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_werks INPUT.

  IF sel_werks-werks IS INITIAL.
    CLEAR sel_werks-werks_name.
  ELSE.
    TRANSLATE sel_werks-werks TO UPPER CASE.

    SELECT SINGLE name1 INTO sel_werks-werks_name FROM t001w WHERE werks = sel_werks-werks.
    IF sy-subrc <> 0.
      MESSAGE |门店编号 { sel_werks-werks } 不存在| TYPE 'E'.
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0303  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0303 INPUT.

  CLEAR return_flag.

  CASE ok_code.
    WHEN 'IMPO'.

      PERFORM read_excel_data.

    WHEN 'SAVE'.

      PERFORM check_excel_data.

      CHECK return_flag IS INITIAL.

      PERFORM save_werks_data.

      LEAVE TO SCREEN 0.

    WHEN 'EXPO'.

      PERFORM export_excle_template.
    WHEN OTHERS.
  ENDCASE.

  CLEAR ok_code.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PATH_ACTION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE path_action INPUT.

  PERFORM choose_path.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0501  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0501 INPUT.

  CASE ok_code.
    WHEN 'TAB1'.
      IF gt_yhlist_node[] IS INITIAL.

        IF gs_fh_list-mtype = '01'.

          PERFORM show_yh_odlist USING gs_fh_list-yhdln '1'.
        ELSEIF gs_fh_list-mtype = '02'.

          PERFORM show_yh_odlist USING gs_fh_list-yhdln '3'.
        ENDIF.

      ENDIF.
    WHEN 'TAB2'.
      IF gt_odlist_node[] IS INITIAL.

        PERFORM show_jm_odlist TABLES lt_return USING '2' gs_yh_list-bhnuc.

      ENDIF.
    WHEN OTHERS.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0501_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE sceen_exit INPUT.

  LEAVE TO SCREEN 0.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0203  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0203 INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0502  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0502 INPUT.

  CASE ok_code.
    WHEN 'SEAR'.
      PERFORM get_jm_amount.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0602  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0602 INPUT.

  CASE ok_code.
    WHEN 'ERRO'.
      PERFORM pop_message.
    WHEN 'SELE'.

      PERFORM select_list.

    WHEN OTHERS.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0602  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0603 INPUT.

  CASE ok_code.
    WHEN 'ERRO'.
      PERFORM pop_message.
    WHEN 'SELE'.

      PERFORM select_list.

    WHEN OTHERS.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0604  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0604 INPUT.

    CASE ok_code.
    WHEN 'ERRO'.
      PERFORM pop_message.
    WHEN 'SELE'.

      PERFORM select_list.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
