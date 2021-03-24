
  DATA : rt_bukrs            TYPE RANGE OF bukrs,
         rt_difa_doc_type    TYPE RANGE OF ztr_difa_doc_type,
         rt_ifa_no           TYPE RANGE OF ztr_ifa_no,
         rt_ifa_status       TYPE RANGE OF ztr_ifa_status,
         rt_fac_org          TYPE RANGE OF ztr_fac_org,
         rt_dfac_type        TYPE RANGE OF ztr_dfac_type,
         rt_waers            TYPE RANGE OF waers,
         rt_rgt_val_date     TYPE RANGE OF ztr_rgt_val_date,
         rt_application_date TYPE RANGE OF ztr_application_date,
         rt_applicant        TYPE RANGE OF ztr_applicant,
         rt_guarantor        TYPE RANGE OF ztr_guarantor.
  DATA: lv_difa_doc_type  TYPE ztr_difa_doc_type.
  DATA: lt_query_alv       TYPE wd_this->elements_query_alv,
        lt_query_alv_final TYPE wd_this->elements_query_alv.
  DATA: lt_ztrt4004_04 TYPE TABLE OF ztrt4004_04.
  DATA: lt_bukrs_text   TYPE TABLE OF t001,
        lt_fac_org_text TYPE TABLE OF but000,
        lt_uname_text   TYPE TABLE OF user_addrp.
  FIELD-SYMBOLS: <rs_any>        TYPE table,
                 <lfs_query_alv> TYPE wd_this->element_query_alv.

*选择条件
  zcl_wd_selopts=>gr_opts->get_input_complete_as_range( IMPORTING range_ref = DATA(lt_range_ref) ).
  zcl_wd_selopts=>gr_opts->get_input_complete( IMPORTING max_nr_of_results = DATA(iv_max) ).

  LOOP AT lt_range_ref INTO DATA(ls_range_ref).
    CASE ls_range_ref-attribute.
      WHEN 'BUKRS'."公司代码
        ASSIGN ls_range_ref-range->* TO <rs_any>.
        rt_bukrs = <rs_any>.

      WHEN 'DIFA_DOC_TYPE'."单据类型
        ASSIGN ls_range_ref-range->* TO <rs_any>.
        rt_difa_doc_type = <rs_any>.

        READ TABLE rt_difa_doc_type INTO DATA(ls_difa_doc_type) INDEX 1.
        IF sy-subrc = 0.
          lv_difa_doc_type = ls_difa_doc_type-low.
        ENDIF.

      WHEN 'IFA_NO'."单号
        ASSIGN ls_range_ref-range->* TO <rs_any>.
        rt_ifa_no = <rs_any>.

      WHEN 'IFA_STATUS'."单据状态
        ASSIGN ls_range_ref-range->* TO <rs_any>.
        rt_ifa_status = <rs_any>.

      WHEN 'FAC_ORG'."融资机构
        ASSIGN ls_range_ref-range->* TO <rs_any>.
        rt_fac_org = <rs_any>.

      WHEN 'DFAC_TYPE'."直接融资方式
        ASSIGN ls_range_ref-range->* TO <rs_any>.
        rt_dfac_type = <rs_any>.

      WHEN 'WAERS'."货币码
        ASSIGN ls_range_ref-range->* TO <rs_any>.
        rt_waers = <rs_any>.

      WHEN 'RGT_VAL_DATE'."注册有效期至
        ASSIGN ls_range_ref-range->* TO <rs_any>.
        rt_rgt_val_date = <rs_any>.

      WHEN 'APPLICATION_DATE'."申请日期
        ASSIGN ls_range_ref-range->* TO <rs_any>.
        rt_application_date = <rs_any>.

      WHEN 'APPLICANT'."申请人
        ASSIGN ls_range_ref-range->* TO <rs_any>.
        rt_applicant = <rs_any>.

      WHEN 'GUARANTOR'."担保人
        ASSIGN ls_range_ref-range->* TO <rs_any>.
        rt_guarantor = <rs_any>.

    ENDCASE.
  ENDLOOP.