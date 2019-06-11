*&---------------------------------------------------------------------*
*& Report Z15405_XMTB
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z15405_xmtb.
TYPE-POOLS:vrm,slis.
TABLES zt15405_xmtb.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'Z_XMTB' ITSELF
CONTROLS: z_xmtb TYPE TABLEVIEW USING SCREEN 9001.

*&SPWIZARD: LINES OF TABLECONTROL 'Z_XMTB'
DATA:     g_z_xmtb_lines  LIKE sy-loopc.

TYPES:BEGIN OF ty_xmtb,
        box TYPE c.
    INCLUDE STRUCTURE zt15405_xmtb.
TYPES END OF ty_xmtb.

DATA:lo_xmtb TYPE REF TO zcl_15405_xmtb.
DATA:lr_datum TYPE RANGE OF zt15405_xmtb-datum.
DATA:gv_code TYPE sy-ucomm.
DATA:ok_code TYPE sy-ucomm.
DATA: gw_container TYPE REF TO cl_gui_docking_container. "OOALV容器
DATA:gs_name TYPE vrm_id,
     gt_list TYPE vrm_values,
     gs_list TYPE LINE OF vrm_values.
DATA:gs_layo TYPE lvc_s_layo.

DATA:gv_time TYPE i.

DATA:gt_xmtb TYPE TABLE OF ty_xmtb,
     gs_xmtb TYPE ty_xmtb.

CONSTANTS: c_sqr  TYPE tabname16 VALUE 'ZT15405_SQR',
           c_xmjl TYPE tabname16 VALUE 'ZT15405_XMJL',
           c_xmtb TYPE tabname16 VALUE 'ZT15405_XMTB',
           c_xmdm TYPE tabname16 VALUE 'ZT15405_XM'.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
PARAMETERS:p_chk1 RADIOBUTTON GROUP 1 DEFAULT 'X' USER-COMMAND ud1,   "项目查询
           p_chk2 RADIOBUTTON GROUP 1,   "自建表维护
           p_chk3 RADIOBUTTON GROUP 1.   "项目填报
SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-002.
PARAMETERS p_sqr TYPE zt15405_xmtb-zsqr DEFAULT sy-uname OBLIGATORY MODIF ID ss1.
SELECT-OPTIONS s_datum FOR zt15405_xmtb-datum MODIF ID ss1.
SELECTION-SCREEN END OF BLOCK blk2.

SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE TEXT-003.
PARAMETERS p_tabnam TYPE tabname AS LISTBOX VISIBLE LENGTH 20 MODIF ID ss2.
SELECTION-SCREEN END OF BLOCK blk3 .

INITIALIZATION.
  PERFORM frm_set_vrm_values.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'SS1'.
      IF p_chk1 = 'X'.
        screen-active = '1'.
      ELSE.
        screen-active = '0'.
      ENDIF.
    ENDIF.

    IF screen-group1 = 'SS2'.
      IF p_chk2 = 'X'.
        screen-active = '1'.
      ELSE.
        screen-active = '0'.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.
  CREATE OBJECT lo_xmtb.
  CASE 'X'.
    WHEN p_chk1.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_xmtb FROM zt15405_xmtb WHERE zsqr = p_sqr AND datum IN s_datum.
      gs_layo-cwidth_opt = 'X'."列优化
      gs_layo-zebra      = 'X'."斑马线
      gs_layo-box_fname  = 'BOX'.
      lo_xmtb->set_layout( EXPORTING is_layo = gs_layo ).
      lo_xmtb->set_fieldcat( EXPORTING is_structure = 'ZT15405_XMTB' ).
      lo_xmtb->display_alv( EXPORTING is_repid = sy-repid
                                      i_pf_status = 'ALV_STATUS'
                                      i_user_command = 'USER_COMMAND'
                                      it_data = gt_xmtb ).
    WHEN p_chk2.
      IF p_tabnam IS NOT INITIAL.
        lo_xmtb->mantain_table( EXPORTING tabname = p_tabnam ).
      ELSE.
        MESSAGE '请选择一行' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN p_chk3.
      CLEAR gv_time.
      CALL SCREEN 9001.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_VRM_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_set_vrm_values .
  CLEAR:gs_list,gt_list.
  gs_name = 'P_TABNAM'.
  PERFORM frm_add_value USING:c_sqr  '申请人表' ,
                              c_xmjl '项目经理表',
*                              c_xmtb '项目填报表'
                              c_xmdm '项目代码表'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = gs_name
      values = gt_list.


ENDFORM.                    " FRM_SET_VRM_VALUES
*&---------------------------------------------------------------------*
*&      Form  FRM_ADD_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_add_value  USING    VALUE(p_key)
                             VALUE(p_text).
  gs_list-key = p_key.
  gs_list-text = p_text.
  APPEND gs_list TO gt_list.
  CLEAR gs_list.
ENDFORM.                    " FRM_ADD_VALUE
*&---------------------------------------------------------------------*
*&引用GUI状态
*&---------------------------------------------------------------------*
FORM alv_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM.                    "PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  gv_code = ok_code.
  CLEAR:ok_code.
  CASE gv_code.
    WHEN 'BACK' OR 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.
      PERFORM frm_save.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM user_command USING r_ucomm TYPE sy-ucomm
                            rs_selfield TYPE slis_selfield.
  DATA:lt_rows TYPE lvc_t_row,
       lo_alv  TYPE REF TO cl_gui_alv_grid.
  CASE r_ucomm.
    WHEN '&EMAIL'.
      DATA:ls_email TYPE zt15405_xmtb.
      DATA:lt_email TYPE TABLE OF zt15405_xmtb.
      CLEAR :lt_email,ls_email.
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = lo_alv.
      CALL METHOD lo_alv->get_selected_rows
        IMPORTING
          et_index_rows = lt_rows.
      LOOP AT lt_rows INTO DATA(ls_row).
        READ TABLE gt_xmtb INTO gs_xmtb INDEX ls_row-index.
        IF gs_xmtb-zsend = 'X'.
          MESSAGE '该行已经发送过邮件' TYPE 'S' DISPLAY LIKE 'E'.
          STOP.
        ENDIF.
        gs_xmtb-zsend = 'X'.
        MOVE-CORRESPONDING gs_xmtb TO ls_email.
        APPEND ls_email TO lt_email.
        CLEAR:ls_email,gs_xmtb.
        MODIFY gt_xmtb FROM gs_xmtb INDEX ls_row-index.
      ENDLOOP.
      IF lt_email IS NOT INITIAL.
        lo_xmtb->send_email( is_type = '1'
                             it_mail = lt_email ).
        MODIFY zt15405_xmtb FROM TABLE lt_email.
        MESSAGE '邮件发送成功' TYPE 'S'.
      ENDIF.

  ENDCASE.

ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_save .
  DATA:lt_xmtb TYPE TABLE OF zt15405_xmtb,
       ls_xmtb TYPE zt15405_xmtb.

  CLEAR:ls_xmtb,lt_xmtb.

  IF zt15405_xmtb-zsqr IS INITIAL.
    MESSAGE '请输入申请人' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT gt_xmtb INTO gs_xmtb WHERE box = 'X'.
    IF gs_xmtb-zxmdm IS INITIAL.
      MESSAGE '请输入项目代码' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
    MOVE-CORRESPONDING gs_xmtb TO ls_xmtb.
    APPEND ls_xmtb TO lt_xmtb.
  ENDLOOP.

  IF lt_xmtb IS NOT INITIAL.
    MODIFY zt15405_xmtb FROM TABLE lt_xmtb.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      MESSAGE '保存成功' TYPE 'S'.
    ENDIF.
  ELSE.
    MESSAGE '请选择一行' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM.                    " FRM_SAVE
*&---------------------------------------------------------------------*
*& Module STATUS_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'GUI_9001'.
  SET TITLEBAR 'TITLE_9001'.


  IF gt_xmtb IS INITIAL.
    DATA:lv_last_day TYPE datum.
    DATA:lv_first_day TYPE datum.
    CLEAR:lv_first_day, lv_last_day.
    CONCATENATE sy-datum+0(6) '01' INTO lv_first_day.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = sy-datum
      IMPORTING
        last_day_of_month = lv_last_day
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    DO.
      gs_xmtb-datum = lv_first_day.
      APPEND gs_xmtb TO gt_xmtb.
      ADD 1 TO lv_first_day.
      IF lv_first_day > lv_last_day.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.
  IF zt15405_xmtb-zsqr IS NOT INITIAL.
    ADD 1 TO gv_time.
    LOOP AT gt_xmtb INTO gs_xmtb.
      gs_xmtb-zsqr = zt15405_xmtb-zsqr.
      MODIFY gt_xmtb FROM gs_xmtb.
      CLEAR gs_xmtb.
    ENDLOOP.
  ENDIF.
  IF gv_time = '1'.
    DATA:lt_xmtb TYPE TABLE OF zt15405_xmtb.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_xmtb
    FROM zt15405_xmtb FOR ALL ENTRIES IN gt_xmtb
    WHERE datum = gt_xmtb-datum
      AND zsqr  = gt_xmtb-zsqr.
  ENDIF.


  LOOP AT gt_xmtb INTO gs_xmtb.

    READ TABLE lt_xmtb INTO DATA(ls_xmtb) WITH KEY datum = gs_xmtb-datum
                                               zsqr  = gs_xmtb-zsqr.
    IF sy-subrc = 0.
      gs_xmtb-zxmdm = ls_xmtb-zxmdm.
      gs_xmtb-zxmms = ls_xmtb-zxmms.
    ENDIF.
    IF gs_xmtb-zxmdm IS NOT INITIAL AND gs_xmtb-zxmms IS INITIAL.   "取项目描述
      SELECT SINGLE zxmms INTO gs_xmtb-zxmms FROM zt15405_xm WHERE zxmdm = gs_xmtb-zxmdm.
    ENDIF.

    IF gs_xmtb-zsqr IS NOT INITIAL AND gs_xmtb-zsqrxm IS INITIAL .  "取申请人姓名
      SELECT SINGLE zsqrxm INTO gs_xmtb-zsqrxm FROM zt15405_sqr WHERE zsqr = gs_xmtb-zsqr.
    ENDIF.
    MODIFY gt_xmtb FROM gs_xmtb.
  ENDLOOP.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'Z_XMTB'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE z_xmtb_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_xmtb LINES z_xmtb-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'Z_XMTB'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE z_xmtb_get_lines OUTPUT.
  g_z_xmtb_lines = sy-loopc.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'Z_XMTB'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE z_xmtb_modify INPUT.
  MODIFY gt_xmtb
    FROM gs_xmtb
    INDEX z_xmtb-current_line.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'Z_XMTB'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE z_xmtb_mark INPUT.
  DATA: g_z_xmtb_wa2 LIKE LINE OF gt_xmtb.
  IF z_xmtb-line_sel_mode = 1
  AND gs_xmtb-box = 'X'.
    LOOP AT gt_xmtb INTO g_z_xmtb_wa2
      WHERE box = 'X'.
      g_z_xmtb_wa2-box = ''.
      MODIFY gt_xmtb
        FROM g_z_xmtb_wa2
        TRANSPORTING box.
    ENDLOOP.
  ENDIF.
  MODIFY gt_xmtb
    FROM gs_xmtb
    INDEX z_xmtb-current_line
    TRANSPORTING box.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'Z_XMTB'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE z_xmtb_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'Z_XMTB'
                              'GT_XMTB'
                              'BOX'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
