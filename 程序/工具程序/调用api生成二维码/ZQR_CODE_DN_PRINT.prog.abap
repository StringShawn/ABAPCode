*&---------------------------------------------------------------------*
*& 作    者:  周建华（4361）
*& 开发日期:  2017-11-30
*& 请 求 号:  DEVXXXXXXX
*& 申 请 者:  N/A
*& 功能/技术文档:
*& 描    述: 模拟业务背景:获取表ZQR_CODE_DN中数据，通过OOALV方式输出，然后对
*&           基于cl_gui_docking_container类的OOALV的几个常用事件进行
*&     操作，事件包括：双击事件/单击事件/用户按钮事件/数据改动事件/
*&     工具条事件/搜索帮助事件
*&---------------------------------------------------------------------*
*                           变更记录
*&---------------------------------------------------------------------*
* 修改日期: 0000-00-00  修改人:xxxxxxx  请求号:xxxxxxx
* 描述:
*
*&---------------------------------------------------------------------*

REPORT  zqr_code_dn_print.
INCLUDE ole2incl.
INCLUDE zqr_code_image.
*----------------------------------------------------------------------*
*       Type-pools                                                     *
*----------------------------------------------------------------------*
TYPE-POOLS: slis.

*----------------------------------------------------------------------*
*       Table                                                          *
*----------------------------------------------------------------------*
TABLES:sscrfields,zqr_code_dn,lips,likp.

*----------------------------------------------------------------------*
*       Type                                                           *
*----------------------------------------------------------------------*
TYPES:BEGIN OF ty_output.
TYPES: checkbox TYPE c.                "field for checkbox
TYPES: celltab TYPE lvc_t_styl.        "field to switch editability
*TYPES:icon TYPE icon-name. "图标，单击操作 测试
*TYPES:help TYPE char2.     "搜索帮助测试字段
        INCLUDE STRUCTURE zqr_code_dn.
TYPES:lfimg TYPE lips-lfimg.

TYPES:END OF ty_output.

TYPES:BEGIN OF ty_lips.
TYPES:vbeln TYPE lips-vbeln.
TYPES:posnr TYPE lips-posnr.
TYPES:charg TYPE lips-charg.
TYPES:arktx TYPE lips-arktx.
TYPES:matnr TYPE lips-matnr.
TYPES:werks TYPE lips-werks.
TYPES:lfimg TYPE lips-lfimg.
TYPES:END OF ty_lips.

TYPES:BEGIN OF ty_mcha.
TYPES:matnr TYPE mcha-matnr.
TYPES:werks TYPE mcha-werks.
TYPES:charg TYPE mcha-charg.
TYPES:lwedt TYPE mcha-lwedt.
TYPES:licha TYPE mcha-licha.
TYPES:vfdat TYPE mcha-vfdat.
TYPES:END OF ty_mcha.

TYPES:BEGIN OF ty_mara.
TYPES:matnr TYPE mara-matnr.
TYPES:normt TYPE mara-normt.
TYPES:groes TYPE mara-groes.
TYPES:END OF ty_mara.

*----------------------------------------------------------------------*
*       Internal table and Work areas
*----------------------------------------------------------------------*
DATA:gt_output TYPE STANDARD TABLE OF ty_output,
      gs_output LIKE LINE OF gt_output.



*----------------------------------------------------------------------*
*       ALV层级关系定义
*----------------------------------------------------------------------*
DATA: g_dock_container TYPE REF TO cl_gui_docking_container, "ALV容器(还有cl_gui_custom_container类)
      g_grid           TYPE REF TO cl_gui_alv_grid, "ALV容器的实例
      gs_fieldcat      TYPE lvc_s_fcat,
      gt_fieldcat      TYPE lvc_t_fcat,   "ALV 控制: 字段目录
      gs_variant       TYPE disvariant,   "格式 (外部使用)
      gs_layout        TYPE lvc_s_layo,   "ALV 控制: 布局结构
      gt_exclude       TYPE ui_functions. "按钮筛选内表(可以筛掉不想要的功能按钮)

DATA:ok_code_100 TYPE sy-ucomm, "屏幕100触发的功能代码
      save_ok_100 TYPE sy-ucomm.

*&---------------------------------------------------------------------*
*&      Define marco
*&---------------------------------------------------------------------*
DEFINE  macro_fill_fcat.

* 定义如下几种常用参数的宏
  clear gs_fieldcat.
  gs_fieldcat-fieldname = &1.   "字段名称
  gs_fieldcat-scrtext_s = &2.   "字段描述
  gs_fieldcat-scrtext_m = &2.
  gs_fieldcat-scrtext_l = &2.
  gs_fieldcat-ref_table = &3.   "参考表
  gs_fieldcat-outputlen = &4.   "设置列的输出字符宽度
  gs_fieldcat-edit      = &5.   "可编辑(X:可编辑,空值不可编辑)
  gs_fieldcat-icon      = &6.   "图标输出(X:图标,空值不是)
  gs_fieldcat-hotspot   = &7.   "单击响应(V:响应单击,空值不响应)
  gs_fieldcat-no_zero   = &8.   "为输出字段隐藏前置零(X:隐藏,空值不隐藏)
  gs_fieldcat-f4availabl = &9.  "字段是否有搜索帮助(X:有，空值没有)
  append gs_fieldcat to gt_fieldcat.

END-OF-DEFINITION.
DEFINE  macro_fill_text.
  gs_fieldcat-scrtext_s = &1.   "字段描述
  gs_fieldcat-scrtext_m = &1.
  gs_fieldcat-scrtext_l = &1.
  gs_fieldcat-reptext = &1.
END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*&      Include
*&      定义OOALV事件以及事件响应
*&---------------------------------------------------------------------*
*定义ALV事件响应类
*(可通用,其中涉及到对实际数据操作的部分，根据实际业务进行数据的操作）

*----------------------------------------------------------------------*
*       CLASS ALV_EVENT DEFINITION
*----------------------------------------------------------------------*
* 定义事件类
*----------------------------------------------------------------------*
CLASS alv_event DEFINITION.
  PUBLIC SECTION.
    METHODS:

    handle_double_click         "双击事件
    FOR EVENT double_click OF cl_gui_alv_grid
    IMPORTING e_row e_column es_row_no,

      handle_hotspot_click        "单击事件(类似与双击事件的操作)
      FOR EVENT hotspot_click OF cl_gui_alv_grid
    IMPORTING e_row_id e_column_id es_row_no,

      handle_user_command        "用户按钮事件
      FOR EVENT user_command OF cl_gui_alv_grid
    IMPORTING e_ucomm,

      handle_data_changed        "数据改动事件
      FOR EVENT data_changed OF cl_gui_alv_grid
    IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,

      handle_toolbar            "工具条事件
      FOR EVENT toolbar OF cl_gui_alv_grid
    IMPORTING e_object e_interactive,

      handle_on_f4              "搜索帮助
      FOR EVENT onf4 OF cl_gui_alv_grid
    IMPORTING sender
      e_fieldname  "字段名称
      e_fieldvalue "字段对应的值
      es_row_no    "
      er_event_data
      et_bad_cells
      e_display.

ENDCLASS.                    "ALV_EVENT DEFINITION


*----------------------------------------------------------------------*
*       CLASS BELOW_ALV_EVENT IMPLEMENTATION
*----------------------------------------------------------------------*
* 定义的事件类对应的事件响应模块
*----------------------------------------------------------------------*
CLASS alv_event IMPLEMENTATION.

  METHOD handle_double_click.
    PERFORM frm_handle_double_click CHANGING e_row e_column es_row_no.

  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING e_row_id e_column_id es_row_no.

  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

  METHOD handle_user_command.
    PERFORM frm_handle_user_command USING e_ucomm.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

  METHOD handle_data_changed.        "数据改动事件
    PERFORM frm_handle_data_changed USING er_data_changed.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

  METHOD  handle_toolbar.            "设置工具条图标
    PERFORM frm_handle_toolbar USING e_object.

  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

  METHOD handle_on_f4.
    PERFORM frm_handle_on_f4 USING e_fieldname  es_row_no.
    er_event_data->m_event_handled = 'X' .

  ENDMETHOD.                                                "HANDLE_ON_F4

ENDCLASS.                    "ALV_EVENT IMPLEMENTATION

*----------------------------------------------------------------------*
*       Selection-screen
*----------------------------------------------------------------------*
SELECTION-SCREEN FUNCTION KEY 1. "工具条按钮 最多为5个.
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001. "选择条件
SELECT-OPTIONS:s_vbeln FOR lips-vbeln MEMORY ID vl MATCHCODE OBJECT vmvl.
SELECTION-SCREEN END OF BLOCK blk1.

*initialialization
INITIALIZATION.
*  sscrfields-functxt_01 = '配置表'.

*at selection screen
AT SELECTION-SCREEN.

*start of selection
START-OF-SELECTION.

* 获取数据
  PERFORM frm_get_data.

* 创建空屏幕:用于显示ALV
  CALL SCREEN 100.        "创建屏幕的同时创建 gui状态和gui标题

*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       获取数据
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_data .


*根据交货单LIPS-VBELN号取LIPS-POSNR,判断该行项目的LIPS-CHARG是否为空。
*取出不为空的行项目（LIPS-POSNR）的LIPS-MATNR,LIPS-CHARG,LIPS-WERKS.LIPS-ARKTX.
*并根据LIPS-MATNR,LIPS-CHARG,LIPS-WERKS，联立表MCHA,取出MCHA-LWEDT,MCHA-LICHA,MCHA-VFDAT,
*根据LIPS-MATNR联立MARA取 MARA-NORMT,MARA-GROES.
  DATA:lt_lips TYPE STANDARD TABLE OF ty_lips.
  DATA:ls_lips TYPE ty_lips.

  DATA:lt_mcha TYPE STANDARD TABLE OF ty_mcha.
  DATA:ls_mcha TYPE ty_mcha.

  DATA:lt_mara TYPE STANDARD TABLE OF ty_mara.
  DATA:ls_mara TYPE ty_mara.

  "wadat_ist
  IF s_vbeln[] IS INITIAL.

    DATA:e_date TYPE sy-datum.

    CALL FUNCTION 'FIMA_DATE_CREATE'
      EXPORTING
        i_date   = sy-datum
        i_years = '-3'"
*        i_months = '-12'"
      IMPORTING
        e_date   = e_date.

    SELECT
    lips~vbeln
    lips~posnr
    lips~charg
    lips~arktx
    lips~matnr
    lips~werks
    lips~lfimg
    FROM likp
    INNER JOIN lips ON likp~vbeln = lips~vbeln
    INTO TABLE lt_lips
    WHERE lips~vbeln IN s_vbeln AND wadat_ist BETWEEN e_date AND sy-datum.

  ELSE.

    SELECT
    lips~vbeln
    lips~posnr
    lips~charg
    lips~arktx
    lips~matnr
    lips~werks
    lips~lfimg
    FROM likp
    INNER JOIN lips ON likp~vbeln = lips~vbeln
    INTO TABLE lt_lips
    WHERE lips~vbeln IN s_vbeln.

  ENDIF.

  DELETE lt_lips WHERE lfimg EQ 0.
  DELETE lt_lips WHERE charg EQ space.

  IF NOT lt_lips IS INITIAL.

    SELECT
    matnr
    werks
    charg
    lwedt
    licha
    vfdat
    FROM mcha
    INTO TABLE lt_mcha
    FOR ALL ENTRIES IN lt_lips
    WHERE matnr = lt_lips-matnr
      AND werks = lt_lips-werks
      AND charg = lt_lips-charg.

    SELECT
    matnr
    normt
    groes
    FROM mara
    INTO TABLE lt_mara
    FOR ALL ENTRIES IN lt_lips
    WHERE matnr = lt_lips-matnr.

  ENDIF.

  SORT lt_mcha.
  SORT lt_mara.

  DATA:ls_celltab TYPE lvc_s_styl.
  DATA:lt_celltab TYPE lvc_t_styl.
  DATA:l_index TYPE i.
  " * Initially, set all checkbox cells editable.
  ls_celltab-fieldname = 'CHECKBOX'.
  ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.


  LOOP AT lt_lips INTO ls_lips.

    MOVE-CORRESPONDING ls_lips TO gs_output.

    READ TABLE lt_mcha INTO ls_mcha WITH KEY matnr = ls_lips-matnr werks = ls_lips-werks charg = ls_lips-charg.
    IF sy-subrc EQ 0.
      gs_output-lwedt = ls_mcha-lwedt.
      gs_output-licha = ls_mcha-licha.
      gs_output-vfdat = ls_mcha-vfdat.
    ENDIF.

    READ TABLE lt_mara INTO ls_mara WITH KEY matnr = ls_lips-matnr.
    IF sy-subrc EQ 0.
      gs_output-normt = ls_mara-normt.
      gs_output-groes = ls_mara-groes.
    ENDIF.

*贴纸打印字段取值：
*
*入库日期：MCHA-LWEDT
*品       牌：MARA-NORMT
*物料编码：LIPS-MATNR
*原厂编码：MCHA-LICHA
*效       期：MCHA-VFDAT
*批       号：LIPS-CHARG
*名       称：LIPS-ARKTX
*规       格：MARA-GROES
    "gs_output-qr
    "新WMS二维码
    "11000781|650804292197|20150901|(01)|21000
    "物料编号|批号|效期|(01)|批次号

    CONCATENATE
    gs_output-matnr '|'
    gs_output-licha '|'
    gs_output-vfdat '|'
    '(01)' '|'
    gs_output-charg
    INTO gs_output-qr.

    SHIFT gs_output-qr LEFT DELETING LEADING '0'.

    APPEND gs_output TO gt_output.
  ENDLOOP.


  LOOP AT gt_output INTO gs_output.
    l_index = sy-tabix.
    REFRESH lt_celltab.

    ls_celltab-fieldname = 'CHECKBOX'.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
    INSERT ls_celltab INTO TABLE lt_celltab.

    INSERT LINES OF lt_celltab INTO TABLE gs_output-celltab.
    MODIFY gt_output FROM gs_output INDEX l_index.
  ENDLOOP.




*


*  SELECT *
*  UP TO 10 ROWS
*  INTO CORRESPONDING FIELDS OF TABLE gt_output
*  FROM ZQR_CODE_DN
*  WHERE ebeln IN s_ebeln.
*
*  LOOP AT gt_output INTO gs_output.
*    gs_output-icon = icon_execute_object.  "执行按钮
*    MODIFY gt_output FROM gs_output INDEX sy-tabix.
*  ENDLOOP.
*
*  MESSAGE '程序限定最多获取10条数据' TYPE 'S'.

ENDFORM.                    " FRM_GET_DATA


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       屏幕100 PAI
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  save_ok_100 = ok_code_100.
  CLEAR:ok_code_100.
  CASE save_ok_100.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.   "gui状态的功能键中定义
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       屏幕100 PBO
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ZPF_100'.  "GUI状态
  SET TITLEBAR 'ZTB_100'.   "GUI标题

  PERFORM frm_alv_display. "显示 ALV

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       显示 ALV
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_alv_display .

  DATA: lr_event_handler TYPE REF TO alv_event,
        ls_stable        TYPE lvc_s_stbl.

* ALV容器为空则创建，不为空则刷新
  IF g_dock_container IS INITIAL.
*  创建 容器
    CREATE OBJECT g_dock_container
    EXPORTING
      repid                       = sy-repid
      dynnr                       = '100'    "alv所在屏幕
      "dock_at_top:从顶部开始计算占据屏幕的比例；dock_at_bottom:从底部开始计算占据屏幕的比例
      "dock_at_left:从左部开始计算占据屏幕的比例；dock_at_right:从右部开始计算占据屏幕的比例；
      side                        = cl_gui_docking_container=>dock_at_top
      ratio                       = 95       "占屏幕的比例
*       extension                   = 260
*       caption                     = '' "标题
      lifetime                    = cntl_lifetime_dynpro   "容器实例生命周期
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*  创建容器实例
    CREATE OBJECT g_grid
      EXPORTING
        i_parent          = g_dock_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


*   下面是OOALV常用的几个参数
    gs_layout-zebra = 'X'.     "X:行项目带颜色标识
    gs_layout-no_rowins = 'X'. "X:不允许删除选定行,通过复制粘贴维护可编辑行限定在一定范围内
    gs_layout-sel_mode = 'A'.  "A:显示alv的选择行按
*    gs_layout-no_toolbar = 'X'."X:不显示系统标准工具条，该参数默认为空即显示工具条
    gs_layout-stylefname = 'CELLTAB'.
    gs_layout-cwidth_opt = 'X'.
    gs_variant-report = sy-repid.

*  删除系统标准工具条按钮，结合no_toolbar使用
    PERFORM frm_alv_exclude.

*  设置输出显示字段
    PERFORM frm_set_fieldcat.

**设置事件
    CREATE OBJECT lr_event_handler.
    SET HANDLER lr_event_handler->handle_double_click FOR g_grid. "双击事件
    SET HANDLER lr_event_handler->handle_hotspot_click FOR g_grid."单击事件
    SET HANDLER lr_event_handler->handle_user_command FOR g_grid. "按钮事件
    SET HANDLER lr_event_handler->handle_data_changed FOR g_grid. "数据改动事件
    SET HANDLER lr_event_handler->handle_toolbar      FOR g_grid. "工具条事件

* 注册为 F4 处理的字段表
    PERFORM frm_alv_f4.
    SET HANDLER lr_event_handler->handle_on_f4 FOR g_grid.        "搜索帮助事件

    CALL METHOD cl_gui_cfw=>flush. "刷新

    CALL METHOD g_grid->register_edit_event  "注册回车事件
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid->register_edit_event  "注册编辑事件
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        i_buffer_active               = 'X'
        is_variant                    = gs_variant
        i_save                        = 'A'
        i_default                     = 'X'
        is_layout                     = gs_layout
        it_toolbar_excluding          = gt_exclude
      CHANGING
        it_outtab                     = gt_output      "输出数据的内表
        it_fieldcatalog               = gt_fieldcat  "
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD cl_gui_cfw=>flush.

  ELSE.
    ls_stable-row = 'X'.  "固定行
    ls_stable-col = 'X'.  "固定列

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = ls_stable.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDFORM.                    " FRM_ALV_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  FRM_SET_FIELDCAT
*&---------------------------------------------------------------------*
*       设置输出显示字段
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_set_fieldcat .

*  call function 'LVC_FIELDCATALOG_MERGE'
*    exporting
*      i_structure_name       = 'ZZZCZ_SH'
*    changing
*      ct_fieldcat            = gt_fieldcat
*    exceptions
*      inconsistent_interface = 1
*      program_error          = 2
*      others                 = 3.

  PERFORM build_fieldcatalog USING gs_output.

  "macro_fill_text '注册证到期日'.
*  macro_fill_fcat 'ICON'  '单击测试' ''     8  ''  'X' 'X' ''  ''.
*  macro_fill_fcat 'HELP'  '搜索帮助' ''     8  'X' ''  ''  ''  'X'.
*  macro_fill_fcat 'EBELN' 'PO号'     ''     10  ''  ''  ''  'X' ''.
*  macro_fill_fcat 'EBELP' 'PO行项目' ''     10  ''  ''  ''  'X' ''.
*  macro_fill_fcat 'MATNR' '物料号'   ''     10 ''  ''  ''  'X' ''.
*  macro_fill_fcat 'BUKRS' '公司'     'ZQR_CODE_DN' 6  ''  ''  ''  'X' ''.
*  macro_fill_fcat 'MENGE' 'PO数量'   'ZQR_CODE_DN' 13 'X' ''  ''  'X' ''.
*  macro_fill_fcat 'MEINS' '单位'     ''     4  ''  ''  ''  'X' ''.
*  macro_fill_fcat 'AGDAT' '截止日期' 'ZQR_CODE_DN' 13 'X' ''  ''  'X' ''.

*入库日期：MCHA-LWEDT
*品       牌：MARA-NORMT
*物料编码：LIPS-MATNR
*原厂编码：MCHA-LICHA
*效       期：MCHA-VFDAT
*批       号：LIPS-CHARG
*名       称：LIPS-ARKTX
*规       格：MARA-GROES

*CELLTAB
  DELETE gt_fieldcat WHERE fieldname = 'CELLTAB'.

  LOOP AT gt_fieldcat INTO gs_fieldcat.

    CASE gs_fieldcat-fieldname.
      WHEN 'CHECKBOX'."checkbox
        macro_fill_text '勾选打印'.
        gs_fieldcat-checkbox = 'X'.
        gs_fieldcat-edit = 'X'.
        " optional: set column width
        gs_fieldcat-outputlen = 6.
      WHEN 'LWEDT'.
        macro_fill_text '入库日期'.
      WHEN 'NORMT'.
        macro_fill_text '品牌'.
      WHEN 'MATNR'.
        macro_fill_text '物料编码'.
        gs_fieldcat-convexit = 'ALPHA'.
      WHEN 'LICHA'.
        macro_fill_text '原厂编码'.
      WHEN 'VFDAT'.
        macro_fill_text '效期'.
      WHEN 'CHARG'.
        macro_fill_text '批号'.
      WHEN 'ARKTX'.
        macro_fill_text '名称'.
      WHEN 'GROES'.
        macro_fill_text '规格'.
      WHEN OTHERS.
    ENDCASE.

    MODIFY gt_fieldcat FROM gs_fieldcat.
  ENDLOOP.


ENDFORM.                    " FRM_SET_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  build_fieldcatalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PS_ALV     text
*----------------------------------------------------------------------*
FORM build_fieldcatalog  USING ps_alv TYPE any.

  DATA: lobj_stdesc TYPE REF TO cl_abap_structdescr.
  DATA: lt_fields   TYPE cl_abap_structdescr=>included_view.
  DATA: lw_fields   TYPE LINE OF cl_abap_structdescr=>included_view.
  DATA: lw_desc     TYPE x030l.
  DATA: lv_stname   TYPE dd02l-tabname.

* Determine structure descriptor
  TRY.
      lobj_stdesc ?= cl_abap_structdescr=>describe_by_data( ps_alv ).
    CATCH cx_root.
      RAISE no_field_catalog.
  ENDTRY.
* If it is DDIC structure, determine field catalog using ALV FM
  IF lobj_stdesc->is_ddic_type( ) IS NOT INITIAL.
    lv_stname = lobj_stdesc->get_relative_name( ).
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_buffer_active        = space
        i_structure_name       = lv_stname
        i_bypassing_buffer     = 'X'
      CHANGING
        ct_fieldcat            = gt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      CLEAR gt_fieldcat.
      RAISE no_field_catalog.
    ENDIF.
    RETURN.
  ENDIF.
* Get structure fields
  lt_fields = lobj_stdesc->get_included_view( ).
* Build field catalog
  LOOP AT lt_fields INTO lw_fields.
    CLEAR: gs_fieldcat,lw_desc.
    gs_fieldcat-col_pos   = sy-tabix.
    gs_fieldcat-fieldname = lw_fields-name.
    IF lw_fields-type->is_ddic_type( ) IS NOT INITIAL.
      lw_desc            = lw_fields-type->get_ddic_header( ).
      gs_fieldcat-rollname = lw_desc-tabname.
    ELSE.
      gs_fieldcat-inttype  = lw_fields-type->type_kind.
      gs_fieldcat-intlen   = lw_fields-type->length.
      gs_fieldcat-decimals = lw_fields-type->decimals.
    ENDIF.
    APPEND gs_fieldcat TO gt_fieldcat.
  ENDLOOP.
  IF gt_fieldcat IS INITIAL.
    RAISE no_field_catalog.
  ENDIF.

ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&  包括                YHDEMO_OALV_001_CLASS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       双击事件
*----------------------------------------------------------------------*
*      <--P_E_ROW  text
*      <--P_E_COLUMN  text
*      <--P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM frm_handle_double_click  CHANGING p_row TYPE lvc_s_row
  p_column TYPE lvc_s_col   "双击的列对应的字段名称
  p_row_no TYPE lvc_s_roid. "p_row_no-row_id双击的行号
  DATA:l_title(40) TYPE c.
  WRITE p_row_no-row_id TO l_title.
  CONDENSE l_title.
  CONCATENATE '您双击第' l_title '行的' p_column '字段' INTO l_title.
*
*  MESSAGE i000(zzsop) WITH l_title.   "ZZSOPT-000 :&&&&


ENDFORM.                    " FRM_HANDLE_DOUBLE_CLICK


*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       单击事件
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM handle_hotspot_click  USING   p_row_id   TYPE lvc_s_row
      p_column_id TYPE lvc_s_col    "单击的列对应的字段名称
      p_row_no   TYPE lvc_s_roid.   "p_row_no-index单击的行号
  DATA:l_title(40) TYPE c.
  WRITE p_row_no-row_id TO l_title.
  CONDENSE l_title.

  CONCATENATE '您单击第' l_title '行的' p_column_id '字段' INTO l_title.

  MESSAGE i000(zzsop) WITH l_title.   "ZZSOPT-000 :&&&&


ENDFORM.                    " HANDLE_HOTSPOT_CLICK


*&---------------------------------------------------------------------*
*&      Form  FRM_HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*       用户按钮事件
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM frm_handle_user_command  USING l_ucomm TYPE sy-ucomm.   "点击的按钮名称

  DATA: lt_lvc_t_cell TYPE lvc_t_cell,
        lw_lvc_s_cell TYPE lvc_s_cell,
        lt_row        TYPE lvc_t_roid,
        ls_row        TYPE lvc_s_roid.

  DATA:l_title(40) TYPE c.

  DATA:lt_qr_dn TYPE STANDARD TABLE OF zqr_code_dn.
  DATA:ls_qr_dn LIKE LINE OF lt_qr_dn.


  CASE l_ucomm.
    WHEN 'SELECT'.
      LOOP AT gt_output INTO gs_output.
        gs_output-checkbox = 'X'.
        MODIFY gt_output FROM gs_output.
      ENDLOOP.

      PERFORM frm_refresh_table_display.
    WHEN 'DESELECT'.
      LOOP AT gt_output INTO gs_output.
        gs_output-checkbox = ''.
        MODIFY gt_output FROM gs_output.
      ENDLOOP.

      PERFORM frm_refresh_table_display.
    WHEN 'PRINT'.


      LOOP AT gt_output INTO gs_output WHERE checkbox EQ 'X'.
        CLEAR:ls_qr_dn.
        MOVE-CORRESPONDING gs_output TO ls_qr_dn.
        APPEND ls_qr_dn TO lt_qr_dn.
      ENDLOOP.

      IF lt_qr_dn IS INITIAL.
        MESSAGE '请勾选内容打印' TYPE 'E'.
      ELSE.

        PERFORM show_smart_form TABLES lt_qr_dn CHANGING ls_qr_dn.

      ENDIF.


    WHEN 'SAVE'.

      CALL METHOD g_grid->get_selected_cells
        IMPORTING
          et_cell = lt_lvc_t_cell.          "获取选定单元格的行号和字段名

      CALL METHOD g_grid->get_selected_rows "获取选定行项目的行号
      IMPORTING
        et_row_no = lt_row.

      READ TABLE lt_row INTO ls_row INDEX 1.
      IF sy-subrc = 0.
        DESCRIBE TABLE lt_row LINES l_title.
        CONDENSE l_title.
        CONCATENATE '您选定了' l_title '行' INTO l_title.
      ELSE.
        l_title = '您没有选定任何行'.
      ENDIF.

      MESSAGE i000(zzsop) WITH l_title.   "ZZSOPT-000 :&&&&

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " FRM_HANDLE_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  FRM_HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*       数据改动事件
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM frm_handle_data_changed  USING ir_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  DATA:ls_mod_cell TYPE lvc_s_modi , "应用的修改的单元格
        lv_value    TYPE lvc_value ,   "单元格内容
        ls_stable   TYPE lvc_s_stbl.  "刷新稳定性

  DATA:lw_zqr_code_dn LIKE LINE OF gt_output."保存原始数据

  DATA:l_title(40) TYPE c.

  SORT ir_data_changed->mt_mod_cells BY row_id.

*  LOOP AT ir_data_changed->mt_mod_cells INTO ls_mod_cell.
*    AT NEW row_id.
*      READ TABLE gt_output INTO gs_output INDEX ls_mod_cell-row_id.
*      lw_ZQR_CODE_DN = gs_output.
*    ENDAT.
*
*    CASE ls_mod_cell-fieldname.
*      WHEN 'MENGE'.  "数量
*        "获取指定单元格改动后内容
*        CALL METHOD ir_data_changed->get_cell_value
*          EXPORTING
*            i_row_id    = ls_mod_cell-row_id
*            i_fieldname = 'MENGE'
*          IMPORTING
*            e_value     = gs_output-menge.
*
*      WHEN 'AGDAT'.  "日期
*        CALL METHOD ir_data_changed->get_cell_value
*          EXPORTING
*            i_row_id    = ls_mod_cell-row_id
*            i_fieldname = 'AGDAT'
*          IMPORTING
*            e_value     = gs_output-agdat.
*
*      WHEN OTHERS.
*    ENDCASE.
*
*
*    AT END OF row_id.
*      IF lw_ZQR_CODE_DN-menge <> gs_output-menge.
*        WRITE ls_mod_cell-row_id TO l_title.
*        CONDENSE l_title.
*        CONCATENATE '第' l_title '行数量' INTO l_title.
*        MESSAGE i000(zzsop) WITH l_title lw_ZQR_CODE_DN-menge '修改为' gs_output-menge.
*
**       将新值写到内表中
*        MODIFY gt_output FROM gs_output INDEX ls_mod_cell-row_id.
*      ENDIF.
*
*      IF lw_ZQR_CODE_DN-agdat <> gs_output-agdat.
*        WRITE ls_mod_cell-row_id TO l_title.
*        CONDENSE l_title.
*        CONCATENATE '第' l_title '行日期' INTO l_title.
*        MESSAGE i000(zzsop) WITH l_title lw_ZQR_CODE_DN-agdat '修改为' gs_output-agdat.
*
**       将新值写到内表中
*        MODIFY gt_output FROM gs_output INDEX ls_mod_cell-row_id.
*      ENDIF.
*    ENDAT.
*
*
*  ENDLOOP.

ENDFORM.                    " FRM_HANDLE_DATA_CHANGED


*&---------------------------------------------------------------------*
*&      Form  FRM_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       工具条事件
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM frm_handle_toolbar  USING l_object TYPE REF TO cl_alv_event_toolbar_set.

  PERFORM frm_handle_toolbar_pro USING:l_object 'SELECT'  '@4B@' '全选'."ICON_SELECT_ALL
  PERFORM frm_handle_toolbar_pro USING:l_object 'DESELECT'  '@4D@' '取消全选'."ICON_DESELECT_ALL
  PERFORM frm_handle_toolbar_pro USING:l_object 'PRINT'  '@0X@' '打印二维码'.
*        l_object 'SAVE'  '@2L@' '保存',
*        l_object 'BATCH_INPUT' '@HB@' '批量输入'.

ENDFORM.                    " FRM_HANDLE_TOOLBAR

*&---------------------------------------------------------------------*
*&      FORM  FRM_HANDLE_TOOLBAR_PRO
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  TEXT
*----------------------------------------------------------------------*
FORM frm_handle_toolbar_pro  USING l_object TYPE REF TO cl_alv_event_toolbar_set
      p_function TYPE stb_button-function
      p_icon TYPE stb_button-icon
      p_text TYPE stb_button-text.

  DATA: ls_toolbar  TYPE stb_button,
        object      TYPE TABLE OF stb_button,
        l_quickinfo TYPE stb_button-quickinfo.

  CLEAR ls_toolbar.
  MOVE 3 TO ls_toolbar-butn_type. " 分隔符
  APPEND ls_toolbar TO l_object->mt_toolbar.

  CLEAR ls_toolbar.
  MOVE 0 TO ls_toolbar-butn_type.   " 按钮（正常）
  MOVE p_function TO ls_toolbar-function. "功能码
  MOVE p_icon TO ls_toolbar-icon.   "图标
  MOVE p_text TO ls_toolbar-text.   "显示文本
  l_quickinfo = p_text.
  MOVE l_quickinfo  TO ls_toolbar-quickinfo.
  APPEND ls_toolbar TO l_object->mt_toolbar.

ENDFORM.                    " FRM_HANDLE_TOOLBAR_PRO


*&---------------------------------------------------------------------*
*&      Form  FRM_HANDLE_ON_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_FIELDNAME  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM frm_handle_on_f4  USING e_fieldname TYPE lvc_fname  "内部表字段的字段名称
      es_row_no TYPE lvc_s_roid.   "分配行数给行标识

  DATA: lt_ddshretval TYPE STANDARD TABLE OF ddshretval,
        lw_ddshretval TYPE ddshretval.

  DATA: BEGIN OF it_f4 OCCURS 0,
    help TYPE char2,
    text TYPE char10,
  END OF it_f4.

*  CHECK e_fieldname = 'HELP'.
*
*  REFRESH it_f4.
*  REFRESH lt_ddshretval.
*  it_f4-help = 'T1'.
*  it_f4-text = '测试值1'.
*  APPEND it_f4.
*
*  it_f4-help = 'T2'.
*  it_f4-text = '测试值2'.
*  APPEND it_f4.
*
*  it_f4-help = 'T3'.
*  it_f4-text = '测试值3'.
*  APPEND it_f4.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'HELP'    "指定ALV用F4的字段
*      dynpprog        = sy-repid
*      value_org       = 'S'
*    TABLES
*      value_tab       = it_f4
*      return_tab      = lt_ddshretval
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*
*  READ TABLE lt_ddshretval INTO lw_ddshretval INDEX 1.
*  IF sy-subrc = 0.
*    READ TABLE gt_output INTO gs_output INDEX es_row_no-row_id.
*    IF sy-subrc = 0.
*      gs_output-help = lw_ddshretval-fieldval.
*      MODIFY gt_output FROM gs_output INDEX es_row_no-row_id.
*    ENDIF.
*  ENDIF.
*
*
** 刷新ALV
*  DATA ls_stable TYPE lvc_s_stbl.
*
*  ls_stable-row = 'X'.
*  ls_stable-col = 'X'.
*
*  IF g_dock_container IS NOT INITIAL.
*    CALL METHOD g_grid->refresh_table_display
*      EXPORTING
*        is_stable = ls_stable.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDIF.


ENDFORM.                    " FRM_HANDLE_ON_F4


*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_EXCLUDE
*&---------------------------------------------------------------------*
*       去掉标准ALV的按钮（可通用）
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_alv_exclude .

  DATA: ls_exclude TYPE ui_func.
  REFRESH gt_exclude.

  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_detail.
  APPEND ls_exclude TO gt_exclude.

  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO gt_exclude.

  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO gt_exclude.

  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO gt_exclude.

  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO gt_exclude.

  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO gt_exclude.

  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO gt_exclude.

  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_exclude TO gt_exclude.

  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO gt_exclude.

  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO gt_exclude.

  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_mb_view.
  APPEND ls_exclude TO gt_exclude.

  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO gt_exclude.

  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_exclude TO gt_exclude.

  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND ls_exclude TO gt_exclude.

  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_exclude TO gt_exclude.


ENDFORM.                    " FRM_ALV_EXCLUDE

*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_F4
*&---------------------------------------------------------------------*
*       注册为 F4 处理的字段表
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_alv_f4 .

  DATA: lt_f4 TYPE lvc_t_f4 WITH HEADER LINE .

  lt_f4-fieldname = 'HELP'. "字段名称
  lt_f4-register = 'X' .  "事件已注册
  lt_f4-getbefore = 'X' . "
  APPEND lt_f4 .
  CALL METHOD g_grid->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

ENDFORM.                    " FRM_ALV_F4
*&---------------------------------------------------------------------*
*&      Form  FRM_REFRESH_TABLE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_refresh_table_display .

  DATA:ls_stable        TYPE lvc_s_stbl.

  ls_stable-row = 'X'.  "固定行
  ls_stable-col = 'X'.  "固定列

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = ls_stable.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " FRM_REFRESH_TABLE_DISPLAY