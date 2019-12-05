*----------------------------------------------------------------------*
***INCLUDE ZJSRPT_004_USER_COMMAND_900I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CLEAR gv_code.
  gv_code = ok_code.
  CASE gv_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'TEST'. "模拟分摊
      PERFORM simu_allocation.
    WHEN 'CONFIRM'. "确认分摊
      PERFORM confirm_allocation.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'ZTRCL_YF'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE ztrcl_yf_modify INPUT.
  MODIFY gt_yf_item
    FROM gs_yf_item
    INDEX ztrcl_yf-current_line.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'ZTRCL_YF'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE ztrcl_yf_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'ZTRCL_YF'
                              'GT_YF_ITEM'
                              ' '
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form SIMU_ALLOCATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM simu_allocation .
  DATA:lv_total_amount TYPE zsvat_yfcl_i-zbmount.
  DATA:lv_total_fy TYPE zsvat_yfcl_h-zyf.
  DATA:lv_remain_fy TYPE zsvat_yfcl_h-zyf.
  DATA:lv_datum_first TYPE sy-datum.
  DATA:lv_kursk TYPE zsvat_yfcl_i-kursk.
  DATA:lv_kursk_c TYPE char20.
  DATA:lv_beizhu TYPE ztvat_data-zbeizhu.

  READ TABLE gt_yf_item INTO gs_yf_item INDEX 1.
  IF zsvat_yfcl_h-waers NE gs_yf_item-waerk.   "货币不为本位币时 ，给汇率赋值

    lv_datum_first = sy-datum+0(6) && '01'.
    CALL FUNCTION 'READ_EXCHANGE_RATE'
      EXPORTING
        client           = sy-mandt
        date             = lv_datum_first
        foreign_currency = zsvat_yfcl_h-waers
        local_currency   = gs_yf_item-waerk
      IMPORTING
        exchange_rate    = lv_kursk
      EXCEPTIONS
        no_rate_found    = 1
        no_factors_found = 2
        no_spread_found  = 3
        derived_2_times  = 4
        overflow         = 5
        zero_rate        = 6
        OTHERS           = 7.

*--本位币种、金额
    lv_kursk_c = lv_kursk.
    IF lv_kursk_c CS '-'.
      SHIFT lv_kursk_c RIGHT DELETING TRAILING '-'.
      lv_total_fy = ( zsvat_yfcl_h-zyf + zsvat_yfcl_h-zbf ) / lv_kursk_c.
    ELSE.
      lv_total_fy = ( zsvat_yfcl_h-zyf + zsvat_yfcl_h-zbf ) * lv_kursk_c.
    ENDIF.

  ELSE.
    lv_total_fy = zsvat_yfcl_h-zyf + zsvat_yfcl_h-zbf.
  ENDIF.
  LOOP AT gt_yf_item INTO gs_yf_item.
    ADD gs_yf_item-zmount TO lv_total_amount.
    CLEAR gs_yf_item.
  ENDLOOP.

  lv_remain_fy = lv_total_fy.

  DATA(yf_lines) = lines( gt_yf_item ).
  LOOP AT gt_yf_item INTO gs_yf_item.
    IF sy-tabix = yf_lines..
      gs_yf_item-zfob_price = gs_yf_item-zmount - lv_remain_fy.
    ELSE.
      gs_yf_item-zfob_price = gs_yf_item-zmount - ( gs_yf_item-zmount / lv_total_amount * lv_total_fy ).
      lv_remain_fy = lv_remain_fy - ( gs_yf_item-zmount / lv_total_amount * lv_total_fy ).
    ENDIF.

    MODIFY gt_yf_item FROM gs_yf_item.
    CLEAR: gs_yf_item.
  ENDLOOP.

  READ TABLE gt_yf_item INTO gs_yf_item INDEX 1.
  READ TABLE gt_data INTO DATA(gs_data) WITH KEY zidnum = gs_yf_item-zidnum.

  lv_beizhu = |1.{ gs_data-waerk } { gs_data-kursk } { gs_data-bezei } { gs_data-zwmfph }\n2.|.
  LOOP AT gt_yf_item INTO gs_yf_item.
    lv_beizhu = |{ lv_beizhu }{ zsvat_yfcl_h-zjgtk }价 { gs_yf_item-zmount } FOB价:{ gs_yf_item-zfob_price }\n|.
  ENDLOOP.

  SHIFT lv_beizhu RIGHT DELETING TRAILING '\n'.

  CLEAR lt_beizhu[].
  lt_beizhu-zbeizhu = lv_beizhu.
  APPEND lt_beizhu.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONFIRM_ALLOCATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM confirm_allocation .
  DATA:lv_beizhu TYPE ztvat_data-zbeizhu.
  DATA:lv_datum_first TYPE sy-datum.

  LOOP AT gt_yf_item INTO gs_yf_item.
    IF gs_yf_item-zfob_price IS INITIAL.
      MESSAGE '请先模拟分摊' TYPE 'S' DISPLAY LIKE 'E'.
      DATA(error_flag) = abap_true.
      EXIT.
    ENDIF.
    CLEAR:gs_yf_item.
  ENDLOOP.
  CHECK error_flag IS INITIAL.

  CALL METHOD editor->get_text_as_r3table
    IMPORTING
      table = lt_beizhu[].

  LOOP AT lt_beizhu WHERE zbeizhu IS NOT INITIAL.
    lv_beizhu = lv_beizhu && lt_beizhu-zbeizhu && '\n'.
  ENDLOOP.
  SHIFT lv_beizhu RIGHT DELETING TRAILING '\n'.

  READ TABLE gt_yf_item INTO gs_yf_item INDEX 1.
  READ TABLE gt_data INTO DATA(gs_data) WITH KEY zidnum = gs_yf_item-zidnum.
  READ TABLE lt_beizhu INDEX 1.
  LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>) WHERE zidnum = gs_data-zidnum.
    <fs_data>-zbeizhu = lv_beizhu.
  ENDLOOP.

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  WAERS_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE waers_f4 INPUT.

*  DATA:lt_waers TYPE TABLE OF zsvat_yfcl_h-waers WITH HEADER LINE.
  DATA:BEGIN OF lt_waers OCCURS 0,
         waers TYPE zsvat_yfcl_h-waers,
       END OF lt_waers .

  CLEAR:lt_waers,lt_waers[].

  APPEND INITIAL LINE TO lt_waers.

  lt_waers-waers = 'CNY'.
  APPEND lt_waers.

  lt_waers-waers = 'USD'.
  APPEND lt_waers.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield  = 'ZSVAT_YFCL_H-WAERS'
*     PVALKEY   = ' '
*     DYNPPROG  = ' '
*     DYNPNR    = ' '
*     DYNPROFIELD            = ' '
*     STEPL     = 0
*     WINDOW_TITLE           =
*     VALUE     = ' '
      value_org = 'S'
*     MULTIPLE_CHOICE        = ' '
*     DISPLAY   = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM          = ' '
*     CALLBACK_METHOD        =
*     MARK_TAB  =
* IMPORTING
*     USER_RESET             =
    TABLES
      value_tab = lt_waers
*     FIELD_TAB =
*     RETURN_TAB             =
*     DYNPFLD_MAPPING        =
* EXCEPTIONS
*     PARAMETER_ERROR        = 1
*     NO_VALUES_FOUND        = 2
*     OTHERS    = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDMODULE.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.1 - E.G.Mellodew. 1998-2019. Sap Release 752
