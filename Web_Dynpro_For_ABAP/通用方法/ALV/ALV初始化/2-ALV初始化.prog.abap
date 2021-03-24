METHOD init_alv .

  DATA: lo_cmp_usage           TYPE REF TO if_wd_component_usage,
        lo_nd_alv_result       TYPE REF TO if_wd_context_node,
        lo_interfacecontroller TYPE REF TO iwci_salv_wd_table.

  "alv 相关结构
  DATA : ls_wd0001 TYPE zabs0001,
         ls_wd0002 TYPE zabs0002,
         ls_column TYPE salv_wd_s_column_ref.

  "1.创建ALV组件
  lo_cmp_usage = wd_this->wd_cpuse_query_alv( ).                      "@@@ wd_cpuse_alv，此处修改为自己标准组件定义的ALV名称，demo为alv。
  IF lo_cmp_usage->has_active_component( ) IS INITIAL .
    lo_cmp_usage->create_component( ).
  ENDIF.

  lo_nd_alv_result = wd_context->get_child_node( 'QUERY_ALV' ).
  lo_interfacecontroller = wd_this->wd_cpifc_query_alv( ).
  lo_interfacecontroller->set_data( r_node_data = lo_nd_alv_result ).

  "2.设置ALV全局属性
  "@@@ 全局设置，根据需求作相应的设置，例如宽度，行数，是否有导出按钮等。

  ls_wd0001-set_scrollable_col_count = 10.
  ls_wd0001-set_visible_row_count = 10.
  ls_wd0001-set_view_quick_save_allowed = 'X'.
  ls_wd0001-set_export_allowed = 'X'.
  ls_wd0001-set_selection_mode = '02'.
  ls_wd0001-set_width = '100%'.
  ls_wd0001-set_design = '00'.
  ls_wd0001-set_read_only = 'X'.
  ls_wd0001-set_display_empty_rows = 'X'.
*  ls_wd0001-set_fixed_table_layout = 'X'."设置ALV 列可以
  ls_wd0001-set_column_resize_mode = '01'.
  ls_wd0001-set_export_allowed = abap_false."导出按钮

  CALL METHOD zcl_wd_common=>init_alv_standard
    EXPORTING
      ii_controller = lo_interfacecontroller
      is_head       = ls_wd0001.

  "3.引用全局包含
*  INCLUDE zwdr002_top.

  "4.设置列的属性    "@@@ 设置ALV对应的模型也就是对应的列名，可以对列进行控件类别设置，例如文本框，复选框，下拉框设置

  LOOP AT zcl_wd_common=>gr_columns INTO ls_column.
    CASE ls_column-id.

      WHEN 'BUKRS'.
        zcl_wd_common=>set_alv_column_attribute(
             EXPORTING
                                         iv_name               = 'BUKRS'
*                                         iv_header             =
                                         iv_visible            = '02'
                                         iv_position           = 1
                                         iv_input              = 'X'
                                         iv_width              = '120px'
                                         iv_resizable          = 'X'
                                         ir_alv_config_table   = zcl_wd_common=>gr_alv ).

      WHEN 'BUKRS_T'.
        zcl_wd_common=>set_alv_column_attribute(
             EXPORTING
                                         iv_name               = 'BUKRS_T'
*                                         iv_header             =
                                         iv_visible            = '02'
                                         iv_position           = 2
                                         iv_input              = 'X'
                                         iv_width              = '120px'
                                         iv_resizable          = 'X'
                                         ir_alv_config_table   = zcl_wd_common=>gr_alv ).

      WHEN 'IFA_NO'.
        zcl_wd_common=>set_alv_column_attribute(
             EXPORTING
                                         iv_name               = 'IFA_NO'
                                         iv_header             = wd_assist->if_wd_component_assistance~get_text( '017' )
                                         iv_visible            = '02'
                                         iv_position           = 3
                                         iv_link               = 'X'
                                         iv_width              = '120px'
                                         iv_resizable          = 'X'
                                         ir_alv_config_table   = zcl_wd_common=>gr_alv ).


      WHEN 'DIFA_DOC_TYPE'.
        zcl_wd_common=>set_alv_column_attribute(
             EXPORTING
                                         iv_name               = 'DIFA_DOC_TYPE'
                                         iv_header             = wd_assist->if_wd_component_assistance~get_text( '018' )
                                         iv_visible            = '02'
                                         iv_position           = 4
                                         iv_drop               = 'X'
                                         iv_width              = '120px'
                                         iv_resizable          = 'X'
                                         ir_alv_config_table   = zcl_wd_common=>gr_alv ).

      WHEN 'IFA_STATUS'.
        zcl_wd_common=>set_alv_column_attribute(
             EXPORTING
                                         iv_name               = 'IFA_STATUS'
                                         iv_header             = wd_assist->if_wd_component_assistance~get_text( '019' )
                                         iv_visible            = '02'
                                         iv_position           = 5
                                         iv_drop               = 'X'
                                         iv_width              = '120px'
                                         iv_resizable          = 'X'
                                         ir_alv_config_table   = zcl_wd_common=>gr_alv ).

      WHEN 'DFAC_TYPE'.
        zcl_wd_common=>set_alv_column_attribute(
             EXPORTING
                                         iv_name               = 'DFAC_TYPE'
                                         iv_header             = wd_assist->if_wd_component_assistance~get_text( '009' )
                                         iv_visible            = '02'
                                         iv_position           = 6
                                         iv_drop               = 'X'
                                         iv_width              = '120px'
                                         iv_resizable          = 'X'
                                         ir_alv_config_table   = zcl_wd_common=>gr_alv ).

      WHEN 'FAC_ORG'.
        zcl_wd_common=>set_alv_column_attribute(
             EXPORTING
                                         iv_name               = 'FAC_ORG'
*                                         iv_header             =
                                         iv_visible            = '02'
                                         iv_position           = 7
                                         iv_input              = 'X'
                                         iv_width              = '120px'
                                         iv_resizable          = 'X'
                                         ir_alv_config_table   = zcl_wd_common=>gr_alv ).

      WHEN 'FAC_ORG_T'.
        zcl_wd_common=>set_alv_column_attribute(
             EXPORTING
                                         iv_name               = 'FAC_ORG_T'
                                         iv_header             = wd_assist->if_wd_component_assistance~get_text( '020' )
                                         iv_visible            = '02'
                                         iv_position           = 8
                                         iv_input              = 'X'
                                         iv_width              = '120px'
                                         iv_resizable          = 'X'
                                         ir_alv_config_table   = zcl_wd_common=>gr_alv ).

      WHEN 'WAERS'.
        zcl_wd_common=>set_alv_column_attribute(
             EXPORTING
                                         iv_name               = 'WAERS'
*                                         iv_header             =
                                         iv_visible            = '02'
                                         iv_position           = 9
                                         iv_input              = 'X'
                                         iv_width              = '120px'
                                         iv_resizable          = 'X'
                                         ir_alv_config_table   = zcl_wd_common=>gr_alv ).

      WHEN 'APPL_CURR'.
        zcl_wd_common=>set_alv_column_attribute(
             EXPORTING
                                         iv_name               = 'APPL_CURR'
*                                         iv_header             =
                                         iv_visible            = '02'
                                         iv_position           = 10
                                         iv_input              = 'X'
                                         iv_width              = '120px'
                                         iv_resizable          = 'X'
                                         ir_alv_config_table   = zcl_wd_common=>gr_alv ).

      WHEN 'FAC_TL'.
        zcl_wd_common=>set_alv_column_attribute(
             EXPORTING
                                         iv_name               = 'FAC_TL'
*                                         iv_header             =
                                         iv_visible            = '02'
                                         iv_position           = 11
                                         iv_input              = 'X'
                                         iv_width              = '120px'
                                         iv_resizable          = 'X'
                                         ir_alv_config_table   = zcl_wd_common=>gr_alv ).

      WHEN 'PLAN_DATE'.
        zcl_wd_common=>set_alv_column_attribute(
             EXPORTING
                                         iv_name               = 'PLAN_DATE'
*                                         iv_header             =
                                         iv_visible            = '02'
                                         iv_position           = 12
                                         iv_input              = 'X'
                                         iv_width              = '120px'
                                         iv_resizable          = 'X'
                                         ir_alv_config_table   = zcl_wd_common=>gr_alv ).

      WHEN 'RGT_AMT'.
        zcl_wd_common=>set_alv_column_attribute(
             EXPORTING
                                         iv_name               = 'RGT_AMT'
*                                         iv_header             =
                                         iv_visible            = '02'
                                         iv_position           = 13
                                         iv_input              = 'X'
                                         iv_width              = '120px'
                                         iv_resizable          = 'X'
                                         ir_alv_config_table   = zcl_wd_common=>gr_alv ).

      WHEN 'ACT_PUB_AMT'.
        zcl_wd_common=>set_alv_column_attribute(
             EXPORTING
                                         iv_name               = 'ACT_PUB_AMT'
*                                         iv_header             =
                                         iv_visible            = '02'
                                         iv_position           = 14
                                         iv_input              = 'X'
                                         iv_width              = '120px'
                                         iv_resizable          = 'X'
                                         ir_alv_config_table   = zcl_wd_common=>gr_alv ).

      WHEN 'APPLICATION_DATE'.
        zcl_wd_common=>set_alv_column_attribute(
             EXPORTING
                                         iv_name               = 'APPLICATION_DATE'
*                                         iv_header             =
                                         iv_visible            = '02'
                                         iv_position           = 15
                                         iv_input              = 'X'
                                         iv_width              = '120px'
                                         iv_resizable          = 'X'
                                         ir_alv_config_table   = zcl_wd_common=>gr_alv ).

      WHEN 'APPLICANT_T'.
        zcl_wd_common=>set_alv_column_attribute(
             EXPORTING
                                         iv_name               = 'APPLICANT_T'
                                         iv_header             = wd_assist->if_wd_component_assistance~get_text( '021' )
                                         iv_visible            = '02'
                                         iv_position           = 16
                                         iv_input              = 'X'
                                         iv_width              = '120px'
                                         iv_resizable          = 'X'
                                         ir_alv_config_table   = zcl_wd_common=>gr_alv ).

      WHEN OTHERS.
        zcl_wd_common=>gr_alv->if_salv_wd_column_settings~delete_column( ls_column-id ).
    ENDCASE.
  ENDLOOP.

  "5.设置ALV工具栏的按钮
  CLEAR : ls_wd0002.                                            "@@@ 设置ALV工具栏的按钮，例如：添加，删除等按钮。
  ls_wd0002-set_button_code = 'CREATE'.
  ls_wd0002-set_button_name = wd_assist->if_wd_component_assistance~get_text( '003' )."新增注册
  CALL METHOD zcl_wd_common=>set_alv_button EXPORTING is_button = ls_wd0002.

  CLEAR : ls_wd0002.                                            "
  ls_wd0002-set_button_code = 'EDIT'.
  ls_wd0002-set_button_name = wd_assist->if_wd_component_assistance~get_text( '023' )."修改
  CALL METHOD zcl_wd_common=>set_alv_button EXPORTING is_button = ls_wd0002.

  CLEAR : ls_wd0002.
  ls_wd0002-set_button_code = 'DELETE'.
  ls_wd0002-set_button_name = wd_assist->if_wd_component_assistance~get_text( '005' )."删除注册
  CALL METHOD zcl_wd_common=>set_alv_button EXPORTING is_button = ls_wd0002.

  CLEAR : ls_wd0002.
  ls_wd0002-set_button_code = 'SHOW_REF'.
  ls_wd0002-set_button_name = wd_assist->if_wd_component_assistance~get_text( '022' )."显示相关单据
  CALL METHOD zcl_wd_common=>set_alv_button EXPORTING is_button = ls_wd0002.

*将本视图ALV_WD_CONFIG_TABLE属性加载到component_controller
  wd_comp_controller->gr_alv_config_table = zcl_wd_common=>gr_alv.

ENDMETHOD.