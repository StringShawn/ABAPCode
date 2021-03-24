
REPORT zcode_generator_dtel_and_doma.

TABLES sscrfields.

SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE title1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (6) text1.
SELECTION-SCREEN POSITION 10.
PARAMETERS p_devc TYPE devclass.
SELECTION-SCREEN PUSHBUTTON 45(10) tmp USER-COMMAND tmp.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN COMMENT /1(79) cmt1.
SELECTION-SCREEN COMMENT /1(79) cmt2.
SELECTION-SCREEN END OF BLOCK 001.
SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.

*----------------------------------------------------------------------*
*       CLASS lcl_alv_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_f4             FOR EVENT onf4
                            OF cl_gui_alv_grid
                            IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells.
ENDCLASS.                    "lcl_alv_event_handler DEFINITION

TYPES:
  BEGIN OF typ_dtel,
    rollname    TYPE rollname,
    domname     TYPE domname,
    datatype    TYPE datatype_d,
    leng        TYPE ddleng,
    decimals    TYPE decimals,
    ddtext      TYPE as4text,
    result      TYPE string,
  END OF typ_dtel,

  BEGIN OF typ_doma,
    domname     TYPE domname,
    datatype    TYPE datatype_d,
    leng        TYPE ddleng,
    decimals    TYPE decimals,
    lowercase   TYPE lowercase,
    signflag    TYPE signflag,
    ddtext      TYPE as4text,
    result      TYPE string,
  END OF typ_doma.

DATA:
  go_docking_con  TYPE REF TO cl_gui_docking_container,
  go_splitter_con TYPE REF TO cl_gui_splitter_container,
  go_con_dtel     TYPE REF TO cl_gui_container,
  go_con_doma     TYPE REF TO cl_gui_container,
  go_alv_dtel     TYPE REF TO cl_gui_alv_grid,
  go_event_dtel   TYPE REF TO lcl_alv_event_handler,
  go_alv_doma     TYPE REF TO cl_gui_alv_grid,
  go_event_doma   TYPE REF TO lcl_alv_event_handler.

DATA:
  gt_dtels       TYPE TABLE OF typ_dtel,
  gt_domas       TYPE TABLE OF typ_doma.

DATA:
  gt_fieldcat_dtel  TYPE lvc_t_fcat,
  gs_fieldcat_dtel  TYPE lvc_s_fcat,
  gt_fieldcat_doma  TYPE lvc_t_fcat,
  gs_fieldcat_doma  TYPE lvc_s_fcat,
  gt_f4_dtel        TYPE lvc_t_f4,
  gt_f4_doma        TYPE lvc_t_f4,
  gs_f4             TYPE lvc_s_f4,
  gt_exclude        TYPE ui_functions,
  gs_layout         TYPE lvc_s_layo.

*----------------------------------------------------------------------*
*       CLASS lcl_alv_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_event_handler IMPLEMENTATION .
  METHOD handle_f4.
    PERFORM handle_f4 USING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells.
  ENDMETHOD.                                                "HANDLE_F4
ENDCLASS.                    "lcl_alv_event_handler IMPLEMENTATION

DEFINE d_build_fieldcat_dtel.
  gs_fieldcat_dtel-fieldname  = &1.
  gs_fieldcat_dtel-edit       = &2.
  gs_fieldcat_dtel-checkbox   = &3.
  gs_fieldcat_dtel-f4availabl = &4.
  gs_fieldcat_dtel-outputlen  = &5.
  gs_fieldcat_dtel-ref_table  = &6.
  gs_fieldcat_dtel-ref_field  = &7.
  gs_fieldcat_dtel-coltext    = &8.
  append gs_fieldcat_dtel to gt_fieldcat_dtel.
  if gs_fieldcat_dtel-f4availabl = 'X'.
    gs_f4-fieldname = &1.
    gs_f4-register = 'X'.
    append gs_f4 to gt_f4_dtel.
  endif.
  clear gs_fieldcat_dtel.
END-OF-DEFINITION.

DEFINE d_build_fieldcat_doma.
  gs_fieldcat_doma-fieldname  = &1.
  gs_fieldcat_doma-edit       = &2.
  gs_fieldcat_doma-checkbox   = &3.
  gs_fieldcat_doma-f4availabl = &4.
  gs_fieldcat_doma-outputlen  = &5.
  gs_fieldcat_doma-ref_table  = &6.
  gs_fieldcat_doma-ref_field  = &7.
  gs_fieldcat_doma-coltext    = &8.
  append gs_fieldcat_doma to gt_fieldcat_doma.
  if gs_fieldcat_doma-f4availabl = 'X'.
    gs_f4-fieldname = &1.
    gs_f4-register = 'X'.
    append gs_f4 to gt_f4_doma.
  endif.
  clear gs_fieldcat_doma.
END-OF-DEFINITION.

INITIALIZATION.
  sy-title = '批量创建数据元素和域'.
  title1 = '参数设置'.
  text1 = '开发类'.
  tmp = '本地对象'.
  cmt1 = '说明：批量创建完成后，请手动激活！'.
  cmt2 = '有BUG请反馈至微信/QQ：286503700。  更多精彩请关注微信公众号：SAP亮亮'.
  sscrfields-functxt_01 = '批量创建数据元素'.
  sscrfields-functxt_02 = '批量创建域'.

AT SELECTION-SCREEN OUTPUT.
  DATA: lt_exclude TYPE TABLE OF sy-ucomm.
  APPEND 'ONLI' TO lt_exclude.
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
      p_program = sy-cprog
    TABLES
      p_exclude = lt_exclude.

  PERFORM create_screen_object.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      go_alv_dtel->check_changed_data( ).
      PERFORM check_devc.
      PERFORM check_dtel.
      PERFORM create_dtel.
    WHEN 'FC02'.
      go_alv_doma->check_changed_data( ).
      PERFORM check_devc.
      PERFORM check_doma.
      PERFORM create_doma.
    WHEN 'TMP'.
      p_devc = '$TMP'.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  CREATE_SCREEN_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_screen_object .
  CHECK go_docking_con IS INITIAL.
  "创建容器
  CREATE OBJECT go_docking_con
    EXPORTING
      ratio = 85
      side  = cl_gui_docking_container=>dock_at_bottom.

  CREATE OBJECT go_splitter_con
    EXPORTING
      parent  = go_docking_con
      rows    = 2
      columns = 1.

  go_con_dtel = go_splitter_con->get_container( row = 1 column = 1 ).
  go_con_doma = go_splitter_con->get_container( row = 2 column = 1 ).

  "创建DTEL的ALV
  CREATE OBJECT go_alv_dtel
    EXPORTING
      i_parent = go_con_dtel.

  "事件响应
  CREATE OBJECT go_event_dtel.
  SET HANDLER go_event_dtel->handle_f4 FOR go_alv_dtel.

  "工具栏和布局
  PERFORM alv_prepare_toolbar   TABLES    gt_exclude.
  PERFORM alv_prepare_layout    CHANGING  gs_layout.

  "显示ALV
  d_build_fieldcat_dtel:
    'ROLLNAME'  'X'  ' '    ' '  16    '     '  '     '     '数据元素',
    'DOMNAME'   'X'  ' '    ' '  16    'DD04V'  'DOMNAME'   '参照域',
    'DATATYPE'  'X'  ' '    'X'   9    '     '  '     '     '预定义类型',
    'LENG'      'X'  ' '    ' '   8    '     '  '     '     '长度',
    'DECIMALS'  'X'  ' '    ' '   4    '     '  '     '     '小数',
    'DDTEXT'    'X'  ' '    ' '  30    'DD04V'  'DDTEXT'    '描述',
    'RESULT'    ' '  ' '    ' '  30    '     '  '     '     '操作结果'.
  gs_layout-grid_title = '数据元素——设置'.
  CALL METHOD go_alv_dtel->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding = gt_exclude
      is_layout            = gs_layout
    CHANGING
      it_outtab            = gt_dtels
      it_fieldcatalog      = gt_fieldcat_dtel.

  "注册事件
  go_alv_dtel->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
  go_alv_dtel->register_f4_for_fields( EXPORTING it_f4 = gt_f4_dtel ).

  "创建DOMA的ALV
  CREATE OBJECT go_alv_doma
    EXPORTING
      i_parent = go_con_doma.

  "事件响应
  CREATE OBJECT go_event_doma.
  SET HANDLER go_event_doma->handle_f4 FOR go_alv_doma.

  "显示ALV
  d_build_fieldcat_doma:
    'DOMNAME'   'X'  ' '    ' '  16    '     '  '     '     '域名称',
    'DATATYPE'  'X'  ' '    'X'   9    '     '  '     '     '预定义类型',
    'LENG'      'X'  ' '    ' '   8    '     '  '     '     '长度',
    'DECIMALS'  'X'  ' '    ' '   4    '     '  '     '     '小数',
    'LOWERCASE' 'X'  'X'    ' '   6    '     '  '     '     '大小写',
    'SIGNFLAG'  'X'  'X'    ' '   4    '     '  '     '     '负数',
    'DDTEXT'    'X'  ' '    ' '  30    'DD04V'  'DDTEXT'    '描述',
    'RESULT'    ' '  ' '    ' '  30    '     '  '     '     '操作结果'.


  gs_layout-grid_title = '域——设置'.
  CALL METHOD go_alv_doma->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding = gt_exclude
      is_layout            = gs_layout
    CHANGING
      it_outtab            = gt_domas
      it_fieldcatalog      = gt_fieldcat_doma.

  "注册事件
  go_alv_doma->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
  go_alv_doma->register_f4_for_fields( EXPORTING it_f4 = gt_f4_doma ).
ENDFORM.                    " CREATE_SCREEN_OBJECT
*&---------------------------------------------------------------------*
*&      Form  handle_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_f4  USING           e_fieldname TYPE lvc_fname
                                e_fieldvalue TYPE lvc_value
                                es_row_no TYPE lvc_s_roid
                                er_event_data TYPE REF TO cl_alv_event_data
                                et_bad_cells TYPE lvc_t_modi.
  DATA: ls_row TYPE lvc_s_row,
        ls_col TYPE lvc_s_col,
        ls_modi TYPE lvc_s_modi.
  FIELD-SYMBOLS: <lt_modi> TYPE lvc_t_modi.

  er_event_data->m_event_handled = 'X'.

  CASE e_fieldname.
    WHEN 'DATATYPE'.
      PERFORM f4_datatype CHANGING e_fieldvalue.

    WHEN OTHERS.
      EXIT.
  ENDCASE.

  ASSIGN er_event_data->m_data->* TO <lt_modi>.
  ls_modi-row_id    = es_row_no-row_id."
  ls_modi-fieldname = e_fieldname.
  ls_modi-value     = e_fieldvalue.
  APPEND ls_modi TO <lt_modi>.

ENDFORM.                                                    "handle_f4
*&---------------------------------------------------------------------*
*&      Form  alv_prepare_toolbar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_prepare_toolbar  TABLES  pt_exclude TYPE ui_functions.
  REFRESH: pt_exclude.

  APPEND cl_gui_alv_grid=>mc_fc_maximum TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_minimum TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_subtot TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sum TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_average TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_sum TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_subtot TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_find TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_filter TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_print TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_print_prev TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_export TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_graph TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_export TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_view TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_detail TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_help TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_info TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_variant TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_check TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_paste TO pt_exclude.
ENDFORM.                    "alv_prepare_toolbar
*&---------------------------------------------------------------------*
*&      Form  alv_prepare_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_prepare_layout  CHANGING ps_layout TYPE lvc_s_layo.
  ps_layout-zebra = 'X'.
  ps_layout-sel_mode = 'A'.
  ps_layout-smalltitle = 'X'.
ENDFORM.                    "alv_prepare_layout
*&---------------------------------------------------------------------*
*&      Form  f4_datatype
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_datatype CHANGING p_fieldvalue.
  DATA: lt_return TYPE TABLE OF ddshretval WITH HEADER LINE,
        BEGIN OF lt_datatypes OCCURS 0,
          datatype_d TYPE datatype_d,
          as4text    TYPE as4text,
        END OF lt_datatypes.

  SELECT
    domvalue_l AS datatype_d
    ddtext AS as4text
    INTO TABLE lt_datatypes
    FROM dd07t
    WHERE domname = 'DATATYPE' AND ddlanguage = sy-langu.

* 调用F4
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield     = 'DATATYPE_D'
      window_title = '选择'
      value_org    = 'S'
    TABLES
      value_tab    = lt_datatypes[]
      return_tab   = lt_return[].

  IF lt_return[] IS NOT INITIAL.
    READ TABLE lt_return INDEX 1.
    p_fieldvalue = lt_return-fieldval.
  ENDIF.
ENDFORM.                                                    "f4_datatype
*&---------------------------------------------------------------------*
*&      Form  DYNP_VALUES_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM dynp_values_update USING p_dynnr p_fieldname p_stepl p_fieldvalue.
  DATA: lt_dynpfields TYPE TABLE OF dynpread WITH HEADER LINE.

  lt_dynpfields-stepl      = p_stepl.
  lt_dynpfields-fieldname  = p_fieldname.
  lt_dynpfields-fieldvalue = p_fieldvalue.
  APPEND lt_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid   "程序名称
      dynumb     = p_dynnr    "屏幕编号
    TABLES
      dynpfields = lt_dynpfields[].
ENDFORM.                    "DYNP_VALUES_UPDATE
*&---------------------------------------------------------------------*
*&      Form  check_devc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_devc.
  IF p_devc IS INITIAL.
    MESSAGE '开发类不能为空' TYPE 'E'.
  ENDIF.

  IF p_devc <> '$TMP'.
    SELECT SINGLE devclass INTO p_devc FROM tdevc WHERE devclass = p_devc.
    IF sy-subrc <> 0.
      MESSAGE '开发类不存在' TYPE 'E'.
    ENDIF.

    IF p_devc(1) <> 'Y' AND p_devc(1) <> 'Z'.
      MESSAGE '不能使用标准的开发类' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.                    "check_devc
*&---------------------------------------------------------------------*
*&      Form  check_dtel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_dtel.
  DATA: lt_dtels TYPE TABLE OF typ_dtel.
  DATA: l_typekind TYPE ddtypekind.
  DATA: l_flag_line TYPE c,
        l_flag_all TYPE c.
  FIELD-SYMBOLS: <ls_dtel> TYPE typ_dtel.

  CHECK gt_dtels IS NOT INITIAL.

  LOOP AT gt_dtels ASSIGNING <ls_dtel>.
    CLEAR l_flag_line.

    IF <ls_dtel>-rollname IS INITIAL.
      <ls_dtel>-result = '数据元素不能为空'.
      l_flag_line = l_flag_all = 'X'.
    ENDIF.

    CHECK l_flag_line = ''.
    IF <ls_dtel>-rollname(1) BETWEEN 'Y' AND 'Z'.
      SELECT SINGLE rollname INTO <ls_dtel>-rollname FROM dd04l WHERE rollname = <ls_dtel>-rollname.
      IF sy-subrc = 0.
        <ls_dtel>-result = '数据元素已存在'.
        l_flag_line = l_flag_all = 'X'.
      ELSE.
        SELECT SINGLE typekind INTO l_typekind FROM ddtypes WHERE typename = <ls_dtel>-rollname.
        IF sy-subrc = 0.
          CONCATENATE <ls_dtel>-rollname '已被定义为类型' l_typekind INTO <ls_dtel>-result.
          l_flag_line = l_flag_all = 'X'.
        ENDIF.
      ENDIF.
    ELSE.
      <ls_dtel>-result = '数据元素命名错误'.
      l_flag_line = l_flag_all = 'X'.
    ENDIF.

    CHECK l_flag_line = ''.
    IF <ls_dtel>-domname IS NOT INITIAL.
      SELECT SINGLE domname INTO <ls_dtel>-domname FROM dd01l WHERE domname = <ls_dtel>-domname AND as4local = 'A'.
      IF sy-subrc <> 0.
        <ls_dtel>-result = '域不存在或未激活'.
        l_flag_line = l_flag_all = 'X'.
      ENDIF.
    ENDIF.

    CHECK l_flag_line = ''.
    IF <ls_dtel>-datatype IS NOT INITIAL.
      SELECT SINGLE domvalue_l INTO <ls_dtel>-datatype FROM dd07l WHERE domvalue_l = <ls_dtel>-datatype.
      IF sy-subrc <> 0.
        <ls_dtel>-result = '预定义类型不存在'.
        l_flag_line = l_flag_all = 'X'.
      ENDIF.
    ENDIF.

    CHECK l_flag_line = ''.
    IF <ls_dtel>-domname IS INITIAL AND <ls_dtel>-datatype IS INITIAL.
      <ls_dtel>-result = '数据元素类型未指定'.
      l_flag_line = l_flag_all = 'X'.
    ENDIF.

  ENDLOOP.

  IF l_flag_all = 'X'.
    go_alv_dtel->refresh_table_display( ).
    MESSAGE '数据元素设置错误' TYPE 'E'.
  ENDIF.

  lt_dtels = gt_dtels.
  SORT lt_dtels BY rollname.
  DELETE ADJACENT DUPLICATES FROM lt_dtels COMPARING rollname.
  IF lines( lt_dtels ) <> lines( gt_dtels ).
    MESSAGE '数据元素存在重复' TYPE 'E'.
  ENDIF.
ENDFORM.                    " check_dtel
*&---------------------------------------------------------------------*
*&      Form  check_doma
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_doma.
  DATA: lt_domas TYPE TABLE OF typ_doma.
  DATA: l_flag_line TYPE c,
        l_flag_all TYPE c.
  FIELD-SYMBOLS: <ls_doma> TYPE typ_doma.

  CHECK gt_domas IS NOT INITIAL.

  LOOP AT gt_domas ASSIGNING <ls_doma>.
    CLEAR l_flag_line.

    IF <ls_doma>-domname IS INITIAL.
      <ls_doma>-result = '域名称不能为空'.
      l_flag_line = l_flag_all = 'X'.
    ENDIF.

    CHECK l_flag_line = ''.
    IF <ls_doma>-domname(1) BETWEEN 'Y' AND 'Z'.
      SELECT SINGLE domname INTO <ls_doma>-domname FROM dd01l WHERE domname = <ls_doma>-domname.
      IF sy-subrc = 0.
        <ls_doma>-result = '域名称已存在'.
        l_flag_line = l_flag_all = 'X'.
      ENDIF.
    ELSE.
      <ls_doma>-result = '域名称命名错误'.
      l_flag_line = l_flag_all = 'X'.
    ENDIF.

    CHECK l_flag_line = ''.
    IF <ls_doma>-datatype IS NOT INITIAL.
      SELECT SINGLE domvalue_l INTO <ls_doma>-datatype FROM dd07l WHERE domvalue_l = <ls_doma>-datatype.
      IF sy-subrc <> 0.
        <ls_doma>-result = '预定义类型不存在'.
        l_flag_line = l_flag_all = 'X'.
      ENDIF.
    ELSE.
      <ls_doma>-result = '域名称类型未指定'.
      l_flag_line = l_flag_all = 'X'.
    ENDIF.

  ENDLOOP.

  IF l_flag_all = 'X'.
    go_alv_doma->refresh_table_display( ).
    MESSAGE '域名称设置错误' TYPE 'E'.
  ENDIF.

  lt_domas = gt_domas.
  SORT lt_domas BY domname.
  DELETE ADJACENT DUPLICATES FROM lt_domas COMPARING domname.
  IF lines( lt_domas ) <> lines( gt_domas ).
    MESSAGE '域名称存在重复' TYPE 'E'.
  ENDIF.
ENDFORM.                    "check_doma
*&---------------------------------------------------------------------*
*&      Form  CREATE_DTEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_dtel .
  DATA: ls_dd04v TYPE dd04v,
        ls_tadir TYPE tadir,
        l_sobj_name TYPE sobj_name,
        l_obj_name TYPE trobj_name,
        l_srcsystem TYPE srcsystem.
  FIELD-SYMBOLS: <ls_dtel> TYPE typ_dtel.

  LOOP AT gt_dtels ASSIGNING <ls_dtel>.
    MOVE-CORRESPONDING <ls_dtel> TO ls_dd04v.
    ls_dd04v-ddlanguage = sy-langu.
    ls_dd04v-headlen    = 10.
    ls_dd04v-scrlen1    = 10.
    ls_dd04v-scrlen2    = 20.
    ls_dd04v-scrlen3    = 30.
    ls_dd04v-reptext    = <ls_dtel>-ddtext.
    ls_dd04v-scrtext_s  = <ls_dtel>-ddtext.
    ls_dd04v-scrtext_m  = <ls_dtel>-ddtext.
    ls_dd04v-scrtext_l  = <ls_dtel>-ddtext.

    "写入TADIR
    ls_tadir-pgmid      = 'R3TR'.
    ls_tadir-object     = 'DTEL'.
    ls_tadir-obj_name   = ls_dd04v-rollname.
    ls_tadir-srcsystem  = sy-sysid.
    ls_tadir-author     = sy-uname.
    ls_tadir-devclass   = p_devc.
    ls_tadir-masterlang = sy-langu.
    MODIFY tadir FROM ls_tadir.

    l_sobj_name = ls_dd04v-rollname.
    l_srcsystem = sy-sysid.
    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid      = 'R3TR'
        wi_tadir_object     = 'DTEL'
        wi_tadir_obj_name   = l_sobj_name
        wi_tadir_srcsystem  = l_srcsystem
        wi_tadir_author     = sy-uname
        wi_tadir_devclass   = p_devc
        wi_tadir_masterlang = sy-langu
      EXCEPTIONS
        OTHERS              = 1.

    IF sy-subrc <> 0.
      <ls_dtel>-result = '失败：写入到TADIR出错'.
      CONTINUE.
    ENDIF.

    "创建数据元素
    CALL FUNCTION 'DDIF_DTEL_PUT'
      EXPORTING
        name              = ls_dd04v-rollname
        dd04v_wa          = ls_dd04v
      EXCEPTIONS
        dtel_not_found    = 1
        name_inconsistent = 2
        dtel_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      <ls_dtel>-result = '失败：创建数据元素出错'.
      CONTINUE.
    ENDIF.

    "插入到Working Area，使得激活的时候可以选择请求
    l_obj_name = ls_dd04v-rollname.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object   = 'DTEL'
        obj_name = l_obj_name
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      <ls_dtel>-result = '失败：INSERT_INTO_WORKING_AREA出错'.
      CONTINUE.
    ENDIF.

    <ls_dtel>-result = '成功'.
  ENDLOOP.

  go_alv_dtel->refresh_table_display( ).
ENDFORM.                    " CREATE_DTEL
*&---------------------------------------------------------------------*
*&      Form  create_doma
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_doma .
  DATA: ls_dd01v TYPE dd01v,
        ls_tadir TYPE tadir,
        l_sobj_name TYPE sobj_name,
        l_obj_name TYPE trobj_name,
        l_srcsystem TYPE srcsystem.
  FIELD-SYMBOLS: <ls_doma> TYPE typ_doma.

  LOOP AT gt_domas ASSIGNING <ls_doma>.
    MOVE-CORRESPONDING <ls_doma> TO ls_dd01v.
    ls_dd01v-ddlanguage = sy-langu.

    "写入TADIR
    ls_tadir-pgmid      = 'R3TR'.
    ls_tadir-object     = 'DOMA'.
    ls_tadir-obj_name   = ls_dd01v-domname.
    ls_tadir-srcsystem  = sy-sysid.
    ls_tadir-author     = sy-uname.
    ls_tadir-devclass   = p_devc.
    ls_tadir-masterlang = sy-langu.
    MODIFY tadir FROM ls_tadir.

    l_sobj_name = ls_dd01v-domname.
    l_srcsystem = sy-sysid.
    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid      = 'R3TR'
        wi_tadir_object     = 'DOMA'
        wi_tadir_obj_name   = l_sobj_name
        wi_tadir_srcsystem  = l_srcsystem
        wi_tadir_author     = sy-uname
        wi_tadir_devclass   = p_devc
        wi_tadir_masterlang = sy-langu
      EXCEPTIONS
        OTHERS              = 1.

    IF sy-subrc <> 0.
      <ls_doma>-result = '失败：写入到TADIR出错'.
      CONTINUE.
    ENDIF.

    "创建域
    CALL FUNCTION 'DDIF_DOMA_PUT'
      EXPORTING
        name              = ls_dd01v-domname
        dd01v_wa          = ls_dd01v
      EXCEPTIONS
        doma_not_found    = 1
        name_inconsistent = 2
        doma_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      <ls_doma>-result = '失败：创建域出错'.
      CONTINUE.
    ENDIF.

    "插入到Working Area，使得激活的时候可以选择请求
    l_obj_name = ls_dd01v-domname.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object   = 'DOMA'
        obj_name = l_obj_name
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      <ls_doma>-result = '失败：INSERT_INTO_WORKING_AREA出错'.
      CONTINUE.
    ENDIF.

    <ls_doma>-result = '成功'.
  ENDLOOP.

  go_alv_doma->refresh_table_display( ).
ENDFORM.                    "create_doma