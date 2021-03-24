class ZCL_ALV_EVENT_HANDLER definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_ALV_EVENT_HANDLER
*"* do not include other source files here!!!

  methods CONSTRUCTOR
    importing
      !IO_ALV type ref to CL_GUI_ALV_GRID
      !I_REPID type PROGNAME default SY-CPROG
      !I_F4_FORM type STRING optional
      !I_TOOLBAR_FORM type STRING optional
      !I_USER_COMMAND_FORM type STRING optional
      !I_HOTSPOT_FORM type STRING optional
      !I_DATACHANGED_FORM type STRING optional
      !I_DATACHANGED_FINISHED_FORM type STRING optional
      !I_BEFORE_UCOMM_FORM type STRING optional
      !I_DOUBLE_CLICK_FORM type STRING optional
      !I_MENU_BUTTON_FORM type STRING optional .
protected section.
*"* protected components of class ZCL_ALV_EVENT_HANDLER
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_ALV_EVENT_HANDLER
*"* do not include other source files here!!!

  data M_REPID type PROGNAME .
  data M_F4_FORM type STRING .
  data M_TOOLBAR_FORM type STRING .
  data M_HOTSPOT_FORM type STRING .
  data M_USER_COMMAND_FORM type STRING .
  data M_DATACHANGED_FORM type STRING .
  data M_DATACHANGED_FINISHED_FORM type STRING .
  data M_BEFORE_UCOMM_FORM type STRING .
  data M_DOUBLE_CLICK_FORM type STRING .
  data M_MENU_BUTTON_FORM type STRING .

  methods HANDLE_F4
    for event ONF4 of CL_GUI_ALV_GRID
    importing
      !E_FIELDNAME
      !E_FIELDVALUE
      !ES_ROW_NO
      !ER_EVENT_DATA
      !ET_BAD_CELLS
      !E_DISPLAY .
  methods HANDLE_HOTSPOT
    for event HOTSPOT_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW_ID
      !E_COLUMN_ID
      !ES_ROW_NO .
  methods HANDLE_TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE .
  methods HANDLE_USER_COMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods HANDLE_DATACHANGED
    for event DATA_CHANGED of CL_GUI_ALV_GRID
    importing
      !ER_DATA_CHANGED
      !E_ONF4
      !E_ONF4_BEFORE
      !E_ONF4_AFTER
      !E_UCOMM .
  methods HANDLE_DATACHANGED_FINISHED
    for event DATA_CHANGED_FINISHED of CL_GUI_ALV_GRID
    importing
      !E_MODIFIED
      !ET_GOOD_CELLS .
  methods HANDLE_BEFORE_UCOMM
    for event BEFORE_USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods HANDLE_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW
      !E_COLUMN
      !ES_ROW_NO .
  methods HANDLE_MENU_BUTTON
    for event MENU_BUTTON of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_UCOMM .
ENDCLASS.



CLASS ZCL_ALV_EVENT_HANDLER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALV_EVENT_HANDLER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_ALV                         TYPE REF TO CL_GUI_ALV_GRID
* | [--->] I_REPID                        TYPE        PROGNAME (default =SY-CPROG)
* | [--->] I_F4_FORM                      TYPE        STRING(optional)
* | [--->] I_TOOLBAR_FORM                 TYPE        STRING(optional)
* | [--->] I_USER_COMMAND_FORM            TYPE        STRING(optional)
* | [--->] I_HOTSPOT_FORM                 TYPE        STRING(optional)
* | [--->] I_DATACHANGED_FORM             TYPE        STRING(optional)
* | [--->] I_DATACHANGED_FINISHED_FORM    TYPE        STRING(optional)
* | [--->] I_BEFORE_UCOMM_FORM            TYPE        STRING(optional)
* | [--->] I_DOUBLE_CLICK_FORM            TYPE        STRING(optional)
* | [--->] I_MENU_BUTTON_FORM             TYPE        STRING(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD constructor.
  DEFINE d_set_handler.
    if &1 is supplied.
      &2 = &1.
      set handler me->&3 for io_alv.
    endif.
  END-OF-DEFINITION.

  m_repid = i_repid.

  d_set_handler:
*   构造函数入参                  类属性                        类方法
    i_f4_form                     m_f4_form                     handle_f4,
    i_toolbar_form                m_toolbar_form                handle_toolbar,
    i_user_command_form           m_user_command_form           handle_user_command,
    i_hotspot_form                m_hotspot_form                handle_hotspot,
    i_datachanged_form            m_datachanged_form            handle_datachanged,
    i_datachanged_finished_form   m_datachanged_finished_form   handle_datachanged_finished,
    i_before_ucomm_form           m_before_ucomm_form           handle_before_ucomm,
    i_double_click_form           m_double_click_form           handle_double_click,
    i_menu_button_form            m_menu_button_form            handle_menu_button.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_EVENT_HANDLER->HANDLE_BEFORE_UCOMM
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_UCOMM                        LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD handle_before_ucomm.
  PERFORM (m_before_ucomm_form) IN PROGRAM (m_repid) IF FOUND
    USING e_ucomm.
*FORM 示例
*FORM handle_before_ucomm USING e_ucomm TYPE sy-ucomm.
*ENDFORM.                    "handle_before_ucomm
ENDMETHOD.                    "handle_before_ucomm


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_EVENT_HANDLER->HANDLE_DATACHANGED
* +-------------------------------------------------------------------------------------------------+
* | [--->] ER_DATA_CHANGED                LIKE
* | [--->] E_ONF4                         LIKE
* | [--->] E_ONF4_BEFORE                  LIKE
* | [--->] E_ONF4_AFTER                   LIKE
* | [--->] E_UCOMM                        LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD handle_datachanged.
  PERFORM (m_datachanged_form) IN PROGRAM (m_repid) IF FOUND
    USING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.
*FORM 示例
*FORM handle_datachanged_cpmx USING  er_data_changed TYPE REF TO cl_alv_changed_data_protocol
*                                    e_onf4 TYPE char01
*                                    e_onf4_before TYPE char01
*                                    e_onf4_after TYPE char01
*                                    e_ucomm TYPE sy-ucomm.
*  FIELD-SYMBOLS: <ls_cell> TYPE lvc_s_modi.
*  DATA: ls_cpmx TYPE ty_cpmx,
*        lt_mod_cells TYPE lvc_t_modi WITH HEADER LINE.
*
*  CLEAR: g_error.
*
*  DEFINE add_protocol_entry.
*    g_error = 'X'.
*    call method er_data_changed->add_protocol_entry
*      exporting
*        i_msgid     = &1
*        i_msgty     = &2
*        i_msgno     = &3
*        i_msgv1     = &4
*        i_msgv2     = &5
*        i_msgv3     = &6
*        i_msgv4     = &7
*        i_fieldname = <ls_cell>-fieldname
*        i_row_id    = <ls_cell>-row_id
*        i_tabix     = <ls_cell>-tabix.
*  END-OF-DEFINITION.
*  DEFINE modify_cell.
*    call method er_data_changed->modify_cell
*      exporting
*        i_row_id    = <ls_cell>-row_id
*        i_tabix     = <ls_cell>-tabix
*        i_fieldname = &1
*        i_value     = &2.
*  END-OF-DEFINITION.
*  DEFINE get_field_value.
*    read table er_data_changed->mt_mod_cells with key row_id = lt_mod_cells-row_id fieldname = '&1' assigning <ls_cell>.
*    if sy-subrc = 0.
*      ls_cpmx-&1 = <ls_cell>-value.
*    else.
*      read table gt_cpmx index lt_mod_cells-row_id into gs_cpmx.
*      ls_cpmx-&1 = gs_cpmx-&1.
*    endif.
*  END-OF-DEFINITION.
*
*  LOOP AT er_data_changed->mt_mod_cells ASSIGNING <ls_cell>.
*    CASE <ls_cell>-fieldname.
*      WHEN 'SCHFBM'.  "取市场划分描述
*        SET PARAMETER ID 'ZSD_SCHF' FIELD <ls_cell>-value.
*        CLEAR gs_schf.
*        READ TABLE gt_schf INTO gs_schf WITH KEY schfbm = <ls_cell>-value.
*        IF sy-subrc <> 0 AND <ls_cell>-value IS NOT INITIAL.
*          add_protocol_entry: 'Z_SD_DEV' 'E' '009' '市场划分:' <ls_cell>-value '' ''.
*        ENDIF.
*        modify_cell: 'SCHFMS' gs_schf-schfms.
*      WHEN 'SCXFBM'.  "取市场细分描述
*        CLEAR gs_scxf.
*        READ TABLE gt_scxf INTO gs_scxf WITH KEY scxfbm = <ls_cell>-value.
*        IF sy-subrc <> 0 AND <ls_cell>-value IS NOT INITIAL.
*          add_protocol_entry: 'Z_SD_DEV' 'E' '009' '市场细分' <ls_cell>-value '' ''.
*        ENDIF.
*        modify_cell: 'SCXFMS' gs_scxf-scxfms.
*    ENDCASE.
*  ENDLOOP.
*
*  "检查是否匹配
*  SORT er_data_changed->mt_mod_cells BY row_id fieldname.
*  lt_mod_cells[] = er_data_changed->mt_mod_cells.
*  DELETE ADJACENT DUPLICATES FROM lt_mod_cells COMPARING row_id.
*  LOOP AT lt_mod_cells.
*    CLEAR ls_cpmx.
*    get_field_value: schfbm, scxfbm, qyzxbm, sfbm.
*    IF ls_cpmx-scxfbm IS NOT INITIAL.
*      READ TABLE gt_scxf WITH KEY schfbm = ls_cpmx-schfbm scxfbm = ls_cpmx-scxfbm TRANSPORTING NO FIELDS.
*      IF sy-subrc <> 0.
*        add_protocol_entry: 'Z_SD_DEV' 'E' '010' '市场划分' ls_cpmx-schfbm '市场细分' ls_cpmx-scxfbm.
*      ENDIF.
*    ENDIF.
*
*  IF g_error = ''.
*    READ TABLE er_data_changed->mt_mod_cells WITH KEY error = 'X' TRANSPORTING NO FIELDS.
*    IF sy-subrc = 0.
*      g_error = 'X'.
*    ENDIF.
*  ENDIF.
*ENDFORM.
ENDMETHOD.                    "handle_datachanged


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_EVENT_HANDLER->HANDLE_DATACHANGED_FINISHED
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_MODIFIED                     LIKE
* | [--->] ET_GOOD_CELLS                  LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD handle_datachanged_finished.
  PERFORM (m_datachanged_finished_form) IN PROGRAM (m_repid) IF FOUND
    USING e_modified et_good_cells.
*FORM 示例
*FORM handle_datachanged_finished USING  e_modified TYPE char01
*                                        et_good_cells TYPE lvc_t_modi.
*ENDFORM.                    "HANDLE_DATACHANGED_FINISHED
ENDMETHOD.                    "handle_datachanged_finished


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_EVENT_HANDLER->HANDLE_DOUBLE_CLICK
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_ROW                          LIKE
* | [--->] E_COLUMN                       LIKE
* | [--->] ES_ROW_NO                      LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD handle_double_click.
  PERFORM (m_double_click_form) IN PROGRAM (m_repid) IF FOUND
    USING e_row e_column es_row_no.
*FORM 示例
*FORM handle_double_click USING  e_row TYPE lvc_s_row
*                                e_column TYPE lvc_s_col
*                                es_row_no TYPE lvc_s_roid.
*ENDFORM.                    "handle_before_ucomm
ENDMETHOD.                    "HANDLE_DOUBLE_CLICK


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_EVENT_HANDLER->HANDLE_F4
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_FIELDNAME                    LIKE
* | [--->] E_FIELDVALUE                   LIKE
* | [--->] ES_ROW_NO                      LIKE
* | [--->] ER_EVENT_DATA                  LIKE
* | [--->] ET_BAD_CELLS                   LIKE
* | [--->] E_DISPLAY                      LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD handle_f4.
  PERFORM (m_f4_form) IN PROGRAM (m_repid) IF FOUND
    USING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.
*FORM 示例
*FORM handle_f4  USING           e_fieldname TYPE lvc_fname
*                                e_fieldvalue TYPE lvc_value
*                                es_row_no TYPE lvc_s_roid
*                                er_event_data TYPE REF TO cl_alv_event_data
*                                et_bad_cells TYPE lvc_t_modi
*                                e_display TYPE char01.
*  DATA: ls_row TYPE lvc_s_row,
*        ls_col TYPE lvc_s_col,
*        ls_modi TYPE lvc_s_modi,
*        l_tabname TYPE tabname,
*        l_fieldtext TYPE fieldtext,
*        l_ref_table TYPE lvc_rtname,
*        l_ref_field TYPE lvc_rfname.
*  FIELD-SYMBOLS: <lt_modi> TYPE lvc_t_modi.
*
*  er_event_data->m_event_handled = 'X'.
*
*  CASE e_fieldname.
*    WHEN 'TABNAME'.
*      PERFORM f4_dd_table(rsaqddic) USING 'SAPLAQJD_CNTRL'
*                                          '0300'
*                                          'G_DYN_0300-TNAME'
*                                    CHANGING e_fieldvalue.  "搜索帮助代码，来于SQVI中“插入表”的搜索帮助
*
*    WHEN 'TAB1' OR 'TAB2' OR 'ASTABLE'.
*      PERFORM f4_table CHANGING e_fieldvalue.
*
*    WHEN 'FIELD1' OR 'FIELD2' OR 'FIELDNAME'.
*      go_alv_tables->check_changed_data( ).
*
*      IF e_fieldname = 'FIELD1'.
*        READ TABLE gt_joins INDEX es_row_no-row_id INTO gs_join.
*        CHECK gs_join-tab1 IS NOT INITIAL.
*        l_tabname = gs_join-tab1.
*      ELSEIF e_fieldname = 'FIELD2'.
*        READ TABLE gt_joins INDEX es_row_no-row_id INTO gs_join.
*        CHECK gs_join-tab2 IS NOT INITIAL.
*        l_tabname = gs_join-tab2.
*      ELSEIF e_fieldname = 'FIELDNAME'.
*        READ TABLE gt_fields1 INDEX es_row_no-row_id INTO gs_field1.
*        CHECK gs_field1-astable IS NOT INITIAL.
*        l_tabname = gs_field1-astable.
*        l_fieldtext = gs_field1-scrtext_l.
*        l_ref_table = gs_field1-ref_table.
*        l_ref_field = gs_field1-ref_field.
*      ENDIF.
*
*      READ TABLE gt_tables WITH KEY astable = l_tabname INTO gs_table.
*      IF sy-subrc = 0.
*        l_tabname = gs_table-tabname.
*      ENDIF.
*
*      PERFORM f4_field USING l_tabname CHANGING e_fieldvalue l_fieldtext l_ref_table l_ref_field.
*
*    WHEN 'QFIELDNAME' OR 'CFIELDNAME'.
*      PERFORM f4_field_in_itab CHANGING e_fieldvalue.
*
*    WHEN 'EMPHASIZE'.
*      PERFORM f4_color CHANGING e_fieldvalue.
*
*    WHEN 'CONVEXIT'.
*      PERFORM f4_convexit CHANGING e_fieldvalue.
*
*    WHEN OTHERS.
*      EXIT.
*  ENDCASE.
*
*  ASSIGN er_event_data->m_data->* TO <lt_modi>.
*  ls_modi-row_id    = es_row_no-row_id."
*  ls_modi-fieldname = e_fieldname.
*  ls_modi-value     = e_fieldvalue.
*  APPEND ls_modi TO <lt_modi>.
*  IF e_fieldname = 'FIELDNAME'.
*    ls_modi-row_id    = es_row_no-row_id."
*    ls_modi-fieldname = 'SCRTEXT_L'.
*    ls_modi-value     = l_fieldtext.
*    APPEND ls_modi TO <lt_modi>.
*
*    ls_modi-row_id    = es_row_no-row_id."
*    ls_modi-fieldname = 'REF_TABLE'.
*    ls_modi-value     = l_ref_table.
*    APPEND ls_modi TO <lt_modi>.
*
*    ls_modi-row_id    = es_row_no-row_id."
*    ls_modi-fieldname = 'REF_FIELD'.
*    ls_modi-value     = l_ref_field.
*    APPEND ls_modi TO <lt_modi>.
*  ENDIF.
*
*ENDFORM.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_EVENT_HANDLER->HANDLE_HOTSPOT
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_ROW_ID                       LIKE
* | [--->] E_COLUMN_ID                    LIKE
* | [--->] ES_ROW_NO                      LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method handle_hotspot.
  PERFORM (m_hotspot_form) IN PROGRAM (m_repid) IF FOUND
    USING e_row_id e_column_id es_row_no.
*FORM 示例
*FORM handle_hotspot  USING    p_row_id     TYPE lvc_s_row
*                              p_column_id  TYPE lvc_s_col
*                              p_row_no     TYPE lvc_s_roid.
*  CASE p_column_id-fieldname.
*    WHEN 'COUNT'.
*  ENDCASE.
*ENDFORM.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_EVENT_HANDLER->HANDLE_MENU_BUTTON
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_OBJECT                       LIKE
* | [--->] E_UCOMM                        LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD handle_menu_button.
  PERFORM (m_menu_button_form) IN PROGRAM (m_repid) IF FOUND USING e_object e_ucomm.
*FORM 示例
*FORM handle_menu_button  USING  e_object TYPE REF TO cl_ctmenu
*                                e_ucomm TYPE sy-ucomm.
*  CASE e_ucomm.
*    WHEN 'MENU'.
*      CALL METHOD e_object->add_function
*        EXPORTING
*          fcode = 'MENU1'
*          text  = '菜单1'.
*  ENDCASE.
*ENDFORM.                    "handle_menu_button
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_EVENT_HANDLER->HANDLE_TOOLBAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_OBJECT                       LIKE
* | [--->] E_INTERACTIVE                  LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD handle_toolbar.
  PERFORM (m_toolbar_form) IN PROGRAM (m_repid) IF FOUND USING e_object e_interactive.
*FORM 示例
*FORM handle_toolbar USING   e_object TYPE REF TO cl_alv_event_toolbar_set
*                            e_interactive TYPE char1.
*  DATA: ls_toolbar TYPE stb_button.
*
*  ls_toolbar-function = 'IMPORT'.
*  ls_toolbar-icon = icon_import.
*  ls_toolbar-text = '导入表格字段'.
*  ls_toolbar-quickinfo = '导入表格字段'.
*  APPEND ls_toolbar TO e_object->mt_toolbar.
*  CLEAR: ls_toolbar.
*
*  ls_toolbar-function = 'ALL'.
*  ls_toolbar-text = '输出：全选'.
*  ls_toolbar-quickinfo = '全部输出'.
*  APPEND ls_toolbar TO e_object->mt_toolbar.
*  CLEAR: ls_toolbar.
*
*  ls_toolbar-function = 'NONE'.
*  ls_toolbar-text = '输出：取消全选'.
*  ls_toolbar-quickinfo = '全部不输出'.
*  APPEND ls_toolbar TO e_object->mt_toolbar.
*  CLEAR: ls_toolbar.
*ENDFORM.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_EVENT_HANDLER->HANDLE_USER_COMMAND
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_UCOMM                        LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD handle_user_command.
  PERFORM (m_user_command_form) IN PROGRAM (m_repid) IF FOUND USING e_ucomm.
*FORM 示例
*FORM handle_user_command  USING    e_ucomm TYPE sy-ucomm.
*  DATA: ok_code TYPE sy-ucomm.
*  ok_code = e_ucomm.
*  CLEAR e_ucomm.
*
*  CASE ok_code.
*    WHEN 'IMPORT'.
*      go_alv->refresh_table_display( ).
*  ENDCASE.
*ENDFORM.
ENDMETHOD.
ENDCLASS.