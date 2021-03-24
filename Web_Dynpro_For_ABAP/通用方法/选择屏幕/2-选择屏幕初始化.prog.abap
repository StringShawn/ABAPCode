METHOD init_sel_opts .

  DATA : ls_attributes TYPE wdr_so_s_attributes,
         rt_attributes TYPE wdr_so_t_attributes,
         ls_drop       TYPE wdr_context_attr_value,
         lt_drop       TYPE wdr_context_attr_value_list.

  "公司代码
  CLEAR ls_attributes.
  ls_attributes-attribute = 'BUKRS'.
  ls_attributes-attr_type = if_wd_select_options_20=>e_attribute_types-text.
  ls_attributes-dataelement = 'BUKRS'.
  ls_attributes-value_suggest = abap_true.
  ls_attributes-value_help_mode = if_wd_context_node_info=>c_value_help_mode-automatic.
  ls_attributes-mandatory = abap_true.
  ls_attributes-disp_index = 1.
  APPEND ls_attributes TO rt_attributes.

  "直接融资单据类型
  CLEAR ls_attributes.
  ls_attributes-attribute = 'DIFA_DOC_TYPE'.
  ls_attributes-attr_type = if_wd_select_options_20=>e_attribute_types-dropdown.
  ls_attributes-dataelement = 'ZTR_DIFA_DOC_TYPE'.
  ls_attributes-disp_index = 2.
  ls_attributes-max_1_value = abap_true.
  ls_attributes-operator_read_only = abap_true.
  APPEND ls_attributes TO rt_attributes.

  "单号
  CLEAR ls_attributes.
  ls_attributes-attribute = 'IFA_NO'.
  ls_attributes-text = wd_assist->if_wd_component_assistance~get_text( '017' ).
  ls_attributes-attr_type = if_wd_select_options_20=>e_attribute_types-id.
  ls_attributes-dataelement = 'ZTR_DIFA_NO'.
  ls_attributes-disp_index = 3.
  APPEND ls_attributes TO rt_attributes.

  "单据状态
  CLEAR ls_attributes.
  ls_attributes-attribute = 'IFA_STATUS'.
  ls_attributes-text = wd_assist->if_wd_component_assistance~get_text( '019' ).
  ls_attributes-attr_type = if_wd_select_options_20=>e_attribute_types-dropdown.
  ls_attributes-dataelement = 'ZTR_DIFA_STATUS'.
  ls_attributes-disp_index = 4.
  APPEND ls_attributes TO rt_attributes.

  "融资机构
  CLEAR ls_attributes.
  ls_attributes-attribute = 'FAC_ORG'.
  ls_attributes-attr_type = if_wd_select_options_20=>e_attribute_types-text.
  ls_attributes-dataelement = 'ZTR_FAC_ORG'.
  ls_attributes-value_help_mode = if_wd_context_node_info=>c_value_help_mode-ddic.
  ls_attributes-ddic_shlp_name = 'ZSH_TR_BP_FAC_ORG'.
  ls_attributes-disp_index = 5.
  APPEND ls_attributes TO rt_attributes.

  "直接融资方式
  CLEAR ls_attributes.
  ls_attributes-attribute = 'DFAC_TYPE'.
  ls_attributes-attr_type = if_wd_select_options_20=>e_attribute_types-dropdown.
  ls_attributes-text = wd_assist->if_wd_component_assistance~get_text( '009' ).
  ls_attributes-dataelement = 'ZTR_DFAC_TYPE'.
  ls_attributes-disp_index = 6.
  APPEND ls_attributes TO rt_attributes.

  "货币码
  CLEAR ls_attributes.
  ls_attributes-attribute = 'WAERS'.
  ls_attributes-attr_type = if_wd_select_options_20=>e_attribute_types-text.
  ls_attributes-dataelement = 'WAERS'.
  ls_attributes-value_help_mode = if_wd_context_node_info=>c_value_help_mode-ddic.
  ls_attributes-ddic_shlp_name = 'FC_WAERS'.
  ls_attributes-disp_index = 7.
  APPEND ls_attributes TO rt_attributes.

  "注册有效期至
  CLEAR ls_attributes.
  ls_attributes-attribute = 'RGT_VAL_DATE'.
  ls_attributes-attr_type = if_wd_select_options_20=>e_attribute_types-date_time.
  ls_attributes-dataelement = 'ZTR_RGT_VAL_DATE'.
  ls_attributes-disp_index = 8.
  APPEND ls_attributes TO rt_attributes.

  "申请日期
  CLEAR ls_attributes.
  ls_attributes-attribute = 'APPLICATION_DATE'.
  ls_attributes-attr_type = if_wd_select_options_20=>e_attribute_types-date_time.
  ls_attributes-dataelement = 'ZTR_APPLICATION_DATE'.
  ls_attributes-disp_index = 9.
  APPEND ls_attributes TO rt_attributes.

  "申请人
  CLEAR ls_attributes.
  ls_attributes-attribute = 'APPLICANT'.
  ls_attributes-attr_type = if_wd_select_options_20=>e_attribute_types-text.
  ls_attributes-dataelement = 'ZTR_APPLICANT'.
  ls_attributes-disp_index = 10.
  APPEND ls_attributes TO rt_attributes.

  "担保人
  CLEAR ls_attributes.
  ls_attributes-attribute = 'GUARANTOR'.
  ls_attributes-attr_type = if_wd_select_options_20=>e_attribute_types-text.
  ls_attributes-dataelement = 'ZTR_GUARANTOR'.
  ls_attributes-value_help_mode =  if_wd_context_node_info=>c_value_help_mode-ddic.
  ls_attributes-ddic_shlp_name = 'ZSH_TR_BP_GUARANTOR'.
  ls_attributes-disp_index = 11.
  APPEND ls_attributes TO rt_attributes.

  zcl_wd_selopts=>init_usage( ir_usage = wd_this->wd_cpuse_query_select_options( ) ).
  zcl_wd_selopts=>init_opts(
    EXPORTING
      ir_cpifc     = wd_this->wd_cpifc_query_select_options( )
      it_attrs     = rt_attributes
      iv_rows      = 11
      ).

ENDMETHOD.