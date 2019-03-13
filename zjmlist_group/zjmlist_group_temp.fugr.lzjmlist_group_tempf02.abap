*----------------------------------------------------------------------*
***INCLUDE LZJMLIST_GROUPF02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM handle_toolbar1  USING p_object TYPE REF TO cl_alv_event_toolbar_set p_interactive TYPE char01.

  PERFORM change_toolbar CHANGING p_object.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM handle_toolbar2  USING p_object TYPE REF TO cl_alv_event_toolbar_set p_interactive TYPE char01.

  PERFORM change_toolbar CHANGING p_object.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM handle_toolbar3  USING p_object TYPE REF TO cl_alv_event_toolbar_set p_interactive TYPE char01.

  PERFORM change_toolbar CHANGING p_object.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM handle_toolbar5  USING p_object TYPE REF TO cl_alv_event_toolbar_set p_interactive TYPE char01.

  PERFORM change_toolbar CHANGING p_object.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM handle_toolbar6  USING p_object TYPE REF TO cl_alv_event_toolbar_set p_interactive TYPE char01.

  PERFORM change_toolbar CHANGING p_object.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM handle_toolbar7  USING p_object TYPE REF TO cl_alv_event_toolbar_set p_interactive TYPE char01.

  PERFORM change_toolbar CHANGING p_object.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM handle_toolbar8  USING p_object TYPE REF TO cl_alv_event_toolbar_set p_interactive TYPE char01.

  PERFORM change_toolbar CHANGING p_object.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM handle_toolbar9  USING p_object TYPE REF TO cl_alv_event_toolbar_set p_interactive TYPE char01.

  PERFORM change_toolbar CHANGING p_object.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM handle_toolbar10  USING p_object TYPE REF TO cl_alv_event_toolbar_set p_interactive TYPE char01.

  PERFORM change_toolbar CHANGING p_object.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM handle_toolbar11  USING p_object TYPE REF TO cl_alv_event_toolbar_set p_interactive TYPE char01.

  PERFORM change_toolbar CHANGING p_object.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM handle_toolbar12  USING p_object TYPE REF TO cl_alv_event_toolbar_set p_interactive TYPE char01.

  PERFORM change_toolbar CHANGING p_object.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM handle_fspp_toolbar  USING p_object TYPE REF TO cl_alv_event_toolbar_set p_interactive TYPE char01.

  add_toolbar 'PRIN' '打印' '打印' '@0X@' 'X' 'P'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command1 USING p_ucomm TYPE sy-ucomm.

  CLEAR return_flag.
  CASE p_ucomm.
    WHEN 'COND'.
      PERFORM pop_selection_screen.

    WHEN 'PRIN'.

      PERFORM print_spdata.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command2 USING p_ucomm TYPE sy-ucomm.

  CLEAR return_flag.
  CASE p_ucomm.
    WHEN 'COND'.
      PERFORM pop_selection_screen.

    WHEN 'PRIN'.

      PERFORM chek_sel_data.

      CHECK return_flag IS INITIAL.

      PERFORM popup_print_screen.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command3 USING p_ucomm TYPE sy-ucomm.

  CLEAR return_flag.
  CASE p_ucomm.
    WHEN 'COND'.
      PERFORM pop_selection_screen.

    WHEN 'PRIN'.

      PERFORM chek_sel_data.

      CHECK return_flag IS INITIAL.

      PERFORM popup_print_screen.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command5 USING p_ucomm TYPE sy-ucomm.

  CLEAR return_flag.
  CASE p_ucomm.
    WHEN 'COND'.
      PERFORM pop_selection_screen.
    WHEN 'DSET'.

      CALL SCREEN 0402 STARTING AT 50 10.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command6 USING p_ucomm TYPE sy-ucomm.

  CLEAR return_flag.
  CASE p_ucomm.
    WHEN 'COND'.
      PERFORM pop_selection_screen.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command7 USING p_ucomm TYPE sy-ucomm.

  CLEAR return_flag.
  CASE p_ucomm.
    WHEN 'COND'.
      PERFORM pop_selection_screen.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command8 USING p_ucomm TYPE sy-ucomm.


  CLEAR return_flag.

  CASE p_ucomm.
    WHEN 'COND'.
      PERFORM pop_selection_screen.
    WHEN 'RESE' OR 'RESI'.

      PERFORM chek_sel_data.

      CHECK return_flag IS INITIAL.

      IF p_ucomm = 'RESI'.
        CLEAR ls_revs.
        CALL SCREEN 0401 STARTING AT 60 10.
        IF ls_revs IS INITIAL.
          RETURN.
        ENDIF.
      ENDIF.

      PERFORM reverse_fh_data.

      CHECK return_flag IS INITIAL.

      PERFORM refresh_alv.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command9 USING p_ucomm TYPE sy-ucomm.

  CLEAR return_flag.

  CASE p_ucomm.
    WHEN 'COND'.
      PERFORM pop_selection_screen.
    WHEN 'RESE' OR 'RESI'.

      PERFORM chek_sel_data.

      CHECK return_flag IS INITIAL.

      IF p_ucomm = 'RESI'.
        CLEAR ls_revs.
        CALL SCREEN 0401 STARTING AT 60 10.
        IF ls_revs IS INITIAL.
          RETURN.
        ENDIF.
      ENDIF.

      PERFORM reverse_fh_data.

      CHECK return_flag IS INITIAL.

      PERFORM refresh_alv.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command10 USING p_ucomm TYPE sy-ucomm.

  CLEAR return_flag.

  CASE p_ucomm.
    WHEN 'COND'.
      PERFORM pop_selection_screen.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command11 USING p_ucomm TYPE sy-ucomm.

  CLEAR return_flag.

  CASE p_ucomm.
    WHEN 'COND'.  "查询
      PERFORM pop_selection_screen.
    WHEN 'REJE'.  "退货

      PERFORM chek_sel_data.
      CHECK return_flag IS INITIAL.
      PERFORM check_pass_data.
      CHECK return_flag IS INITIAL.
      PERFORM execute_th_data.

    WHEN 'CONT'.  "断点续传
      PERFORM chek_sel_data.
      CHECK return_flag IS INITIAL.
      PERFORM check_pass_data.
      CHECK return_flag IS INITIAL.
      PERFORM frm_continue_execute.

    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command12 USING p_ucomm TYPE sy-ucomm.

  CLEAR return_flag.

  CASE p_ucomm.
    WHEN 'COND'.
      PERFORM pop_selection_screen.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_FSPP_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_fspp_command USING p_ucomm TYPE sy-ucomm.

  CLEAR return_flag.
  CASE p_ucomm.
    WHEN 'PRIN'.

      PERFORM print_fspdata.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM double_click1  USING p_row TYPE lvc_s_row p_column TYPE lvc_s_col p_row_no TYPE lvc_s_roid.

  READ TABLE gt_spfh_data INDEX p_row-index.
  IF sy-subrc = 0.

    CALL FUNCTION 'ZGET_TRADE_ODLIST'
      EXPORTING
        i_bhnuc   = gt_spfh_data-bhnuc
        i_type    = '2'
      EXCEPTIONS
        no_odlist = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE '流向信息缺失' TYPE 'I'.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM double_click2  USING p_row TYPE lvc_s_row p_column TYPE lvc_s_col p_row_no TYPE lvc_s_roid.

  READ TABLE gt_fspfh_data INDEX p_row-index.
  IF sy-subrc = 0.

    CALL FUNCTION 'ZGET_TRADE_ODLIST'
      EXPORTING
        i_bhnuc   = gt_fspfh_data-bhnuc
        i_type    = '2'
      EXCEPTIONS
        no_odlist = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE '流向信息缺失' TYPE 'I'.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM double_click3  USING p_row TYPE lvc_s_row p_column TYPE lvc_s_col p_row_no TYPE lvc_s_roid.

  READ TABLE gt_th_data INDEX p_row-index.
  IF sy-subrc = 0.

    CALL FUNCTION 'ZGET_TRADE_ODLIST'
      EXPORTING
        i_bhnuc   = gt_th_data-ebeln
        i_pathn   = gt_th_data-pathn
        i_type    = '4'
      EXCEPTIONS
        no_odlist = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE '流向信息缺失' TYPE 'I'.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM double_click5  USING p_row TYPE lvc_s_row p_column TYPE lvc_s_col p_row_no TYPE lvc_s_roid.

  READ TABLE gt_spyh_data INDEX p_row-index.
  IF sy-subrc = 0.

    CALL FUNCTION 'ZGET_TRADE_ODLIST'
      EXPORTING
        i_bhnuc   = gt_spyh_data-yhdln
        i_type    = '1'
      EXCEPTIONS
        no_odlist = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE '流向信息缺失' TYPE 'I'.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM double_click6  USING p_row TYPE lvc_s_row p_column TYPE lvc_s_col p_row_no TYPE lvc_s_roid.

  READ TABLE gt_fspyh_data INDEX p_row-index.
  IF sy-subrc = 0.

    CALL FUNCTION 'ZGET_TRADE_ODLIST'
      EXPORTING
        i_bhnuc   = gt_fspyh_data-yhdln
        i_type    = '3'
      EXCEPTIONS
        no_odlist = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE '流向信息缺失' TYPE 'I'.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM double_click7  USING p_row TYPE lvc_s_row p_column TYPE lvc_s_col p_row_no TYPE lvc_s_roid.

  DATA where_condition TYPE string.
  DATA lt_jm_belnr LIKE TABLE OF gs_jm_belnr WITH HEADER LINE.
  DATA type TYPE char2.

  READ TABLE gt_belnr_sum INDEX p_row-index.

  CASE p_column-fieldname.
    WHEN 'SPYH_NF_AMOUNT'.
      type = '01'.
    WHEN 'SYYH_YF_AMOUNT'.
      type = '02'.
    WHEN 'FSPYH_NF_AMOUNT'.
      type = '03'.
    WHEN 'FSPYH_YF_AMOUNT'.
      type = '04'.
    WHEN 'SPBH_AMOUNT'.
      type = '05'.
    WHEN 'FSPBH_AMOUNT'.
      type = '06'.
    WHEN 'OA_NF_AMOUNT'.
      type = '07'.
    WHEN 'OA_YF_AMOUNT'.
      type = '08'.
    WHEN 'SPGK_AMOUNT'.
      type = '09'.
    WHEN 'SPWX_AMOUNT'.
      type = '10'.
    WHEN 'SPTH_AMOUNT'.
      type = '11'.
    WHEN OTHERS.
      RETURN.
  ENDCASE.

  where_condition = | werks = gt_belnr_sum-werks and kunnr = gt_belnr_sum-kunnr | &&
                    | and bukrs = gt_belnr_sum-bukrs and type = type |.

  LOOP AT gt_jm_belnr WHERE (where_condition).
    APPEND gt_jm_belnr TO lt_jm_belnr.
  ENDLOOP.
  SORT lt_jm_belnr BY crdat.

  DATA lt_head LIKE TABLE OF zalv_head WITH HEADER LINE.
  DATA ls_element TYPE zdialogbox_element.

  lt_head[] = VALUE #(
  ( fieldname = 'JMBLN_TXT' fieldtext = '关联单据类型' )
  ( fieldname = 'JMBLN' fieldtext = '关联单据号' )
  ( fieldname = 'BTYPE_TXT' fieldtext = '单据类型' )
  ( fieldname = 'BELNR' fieldtext = '会计凭证' )
  ( fieldname = 'AMOUNT' fieldtext = '凭证金额' )
  ( fieldname = 'CRDAT' fieldtext = '创建日期' ) ).

  ls_element-width = 600.
  ls_element-height = 400.
  ls_element-top = 200.
  ls_element-left = 300.
  ls_element-caption = '凭证明细'.

  CALL FUNCTION 'ZPOPUP_ALV1'
    EXPORTING
      ls_dialog_element = ls_element
    TABLES
      it_table          = lt_jm_belnr[]
      it_head           = lt_head[].

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM double_click8  USING p_row TYPE lvc_s_row p_column TYPE lvc_s_col p_row_no TYPE lvc_s_roid.

  READ TABLE gt_spfh_head INDEX p_row-index.
  IF sy-subrc = 0.

    CALL FUNCTION 'ZGET_TRADE_ODLIST'
      EXPORTING
        i_bhnuc   = gt_spfh_head-bhnuc
        i_type    = '2'
      EXCEPTIONS
        no_odlist = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE '流向信息缺失' TYPE 'I'.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM double_click9  USING p_row TYPE lvc_s_row p_column TYPE lvc_s_col p_row_no TYPE lvc_s_roid.

  READ TABLE gt_fspfh_head INDEX p_row-index.
  IF sy-subrc = 0.

    CALL FUNCTION 'ZGET_TRADE_ODLIST'
      EXPORTING
        i_bhnuc   = gt_fspfh_head-bhnuc
        i_type    = '2'
      EXCEPTIONS
        no_odlist = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE '流向信息缺失' TYPE 'I'.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM double_click11  USING p_row TYPE lvc_s_row p_column TYPE lvc_s_col p_row_no TYPE lvc_s_roid.
  DATA:lc_pathn TYPE zsd_jmfh_h-pathn.
  READ TABLE gt_kyth_data_temp INTO DATA(ls_kyth_data) INDEX p_row-index.
  DATA:lv_bhunc TYPE char10.
    lv_bhunc = ls_kyth_data-bhnuc.
    SELECT SINGLE pathn INTO lc_pathn FROM zmm_kythlist WHERE bhnuc = lv_bhunc.

    CALL FUNCTION 'ZGET_TRADE_ODLIST'
      EXPORTING
        i_bhnuc   = lv_bhunc
        i_pathn   = lc_pathn
        i_type    = '5'
      EXCEPTIONS
        no_odlist = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE '流向信息缺失' TYPE 'I'.
    ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_alv .


  CASE g_tb_tab-subscreen.
    WHEN '0201'.
      alv1->set_frontend_layout( EXPORTING is_layout = layo1 ).
      alv1->refresh_table_display( ).
    WHEN '0202'.
      alv2->set_frontend_layout( EXPORTING is_layout = layo2 ).
      alv2->refresh_table_display( ).
    WHEN '0203'.
      alv3->set_frontend_layout( EXPORTING is_layout = layo3 ).
      alv3->refresh_table_display( ).
    WHEN '0204'.
    WHEN '0205'.
      alv5->set_frontend_layout( EXPORTING is_layout = layo5 ).
      alv5->refresh_table_display( ).
    WHEN '0206'.
      alv6->set_frontend_layout( EXPORTING is_layout = layo6 ).
      alv6->refresh_table_display( ).
    WHEN '0207'.
      alv7->set_frontend_layout( EXPORTING is_layout = layo7 ).
      alv7->refresh_table_display( ).
    WHEN '0208'.
      alv8->set_frontend_layout( EXPORTING is_layout = layo8 ).
      alv8->refresh_table_display( ).
    WHEN '0209'.
      alv9->set_frontend_layout( EXPORTING is_layout = layo9 ).
      alv9->refresh_table_display( ).
    WHEN '0210'.
      PERFORM init_fcat.

      alv10->set_frontend_layout( EXPORTING is_layout = layo10 ).
      alv10->set_frontend_fieldcatalog( it_fieldcatalog = fcat10[] ).
      alv10->refresh_table_display( ).
    "ADD BY HANDYXH 20181115
    WHEN '0211'.
      alv11->set_frontend_layout( EXPORTING is_layout = layo11 ).
      alv11->set_frontend_fieldcatalog( it_fieldcatalog = fcat11[] ).
      alv11->refresh_table_display( ).
    WHEN '0212'.
      alv12->set_frontend_layout( EXPORTING is_layout = layo12 ).
      alv12->set_frontend_fieldcatalog( it_fieldcatalog = fcat12[] ).
      alv12->refresh_table_display( ).
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
