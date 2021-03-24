METHOD query .

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

*权限校验
  DATA: lt_bukrs TYPE TABLE OF t001,
        lv_error TYPE wdy_boolean.
  DATA: lv_bukrs_authority     TYPE wdy_boolean,
        lv_guarantor_authority TYPE wdy_boolean,
        lv_msgv1               TYPE syst_msgv.
  IF rt_bukrs IS INITIAL.
    zcl_wd_message=>show_msg_detail( EXPORTING
       ir_controller = wd_this->wd_get_api( )
       iv_type = 'E'
       iv_msgid = 'ZTR06'
       iv_msgno = '044'"请输入"公司代码"选择条件！
       iv_msgty = 'E'
        ).
    RETURN.

  ELSE.
*    SELECT * INTO TABLE lt_bukrs FROM t001 WHERE bukrs IN rt_bukrs.
*    LOOP AT lt_bukrs INTO DATA(ls_bukrs).
*      lv_error = zcl_wd_tr_1013=>authority_check( EXPORTING
*                                                       iv_authority_object = 'Z_TR420'
*                                                       iv_bukrs  = ls_bukrs-bukrs
*                                                       iv_authority_field  = 'ZTR_ACT_ZR'
*                                                       iv_authority_value  = 'A'"直融申请单查询
*                                                       ir_controller = wd_this->wd_get_api( )
*                                           ).
*      IF lv_error = abap_true.
*        RETURN.
*      ENDIF.
*    ENDLOOP.
  ENDIF.

*查询数据
  SELECT * INTO TABLE lt_ztrt4004_04
    FROM ztrt4004_04
    WHERE guarantor IN rt_guarantor
      AND del_ind <> abap_true.
  IF sy-subrc = 0.
    SORT lt_ztrt4004_04 BY ifa_no.
    DELETE ADJACENT DUPLICATES FROM lt_ztrt4004_04 COMPARING ifa_no.
  ENDIF.

  CLEAR lt_query_alv.

  IF lv_difa_doc_type = '1' OR lv_difa_doc_type IS INITIAL."注册申请单

    SELECT a~bukrs,
           a~ifa_no,
           a~guid,
           a~ifa_status,
           a~dfac_type,
           a~fac_org,
           a~waers,
           a~appl_curr,
           a~fac_tl,
           a~plan_date,
           b~rgt_amt,
           a~application_date,
           a~applicant,
           a~ifa_no AS ifa_no_up
      APPENDING CORRESPONDING FIELDS OF TABLE @lt_query_alv
      FROM ztrt4004_01 AS a LEFT JOIN
           ztrt4004_02 AS b ON b~ifa_no = a~ifa_no
                           AND b~del_ind <> @abap_true
      WHERE a~ifa_no IN @rt_ifa_no
        AND a~change_flag = @abap_false
        AND a~ifa_status IN @rt_ifa_status
        AND a~bukrs IN @rt_bukrs
        AND a~fac_org IN @rt_fac_org
        AND a~dfac_type IN @rt_dfac_type
        AND a~waers IN @rt_waers
        AND a~application_date IN @rt_application_date
        AND a~applicant IN @rt_applicant
        AND b~rgt_val_date IN @rt_rgt_val_date
        AND a~del_ind <> @abap_true.
    IF sy-subrc = 0.
      LOOP AT lt_query_alv ASSIGNING <lfs_query_alv> WHERE difa_doc_type IS INITIAL.
        <lfs_query_alv>-difa_doc_type = '1'.
      ENDLOOP.
    ENDIF.

  ENDIF.

  IF lv_difa_doc_type = '2' OR lv_difa_doc_type IS INITIAL."注册变更单

    SELECT a~bukrs,
           a~ifa_no,
           a~guid,
           a~ifa_status,
           a~dfac_type,
           a~fac_org,
           a~waers,
           a~appl_curr,
           a~fac_tl,
           a~plan_date,
           b~rgt_amt,
           a~application_date,
           a~applicant,
           a~ifa_no AS ifa_no_up
      APPENDING CORRESPONDING FIELDS OF TABLE @lt_query_alv
      FROM ztrt4004_01 AS a LEFT JOIN
           ztrt4004_02 AS b ON b~ifa_no = a~ifa_no
                           AND b~del_ind <> @abap_true
      WHERE a~ifa_no IN @rt_ifa_no
        AND a~change_flag = @abap_true
        AND a~ifa_status IN @rt_ifa_status
        AND a~bukrs IN @rt_bukrs
        AND a~fac_org IN @rt_fac_org
        AND a~dfac_type IN @rt_dfac_type
        AND a~waers IN @rt_waers
        AND a~application_date IN @rt_application_date
        AND a~applicant IN @rt_applicant
        AND b~rgt_val_date IN @rt_rgt_val_date
        AND a~del_ind <> @abap_true.
    IF sy-subrc = 0.
      LOOP AT lt_query_alv ASSIGNING <lfs_query_alv> WHERE difa_doc_type IS INITIAL.
        <lfs_query_alv>-difa_doc_type = '2'.
      ENDLOOP.
    ENDIF.

  ENDIF.

  IF lv_difa_doc_type = '3' OR lv_difa_doc_type IS INITIAL."发行申请单

    SELECT bukrs
           dfa_pub_no AS ifa_no
           guid
           difa_p_status AS ifa_status
           ifa_no AS ifa_no_up
           dfac_type
           fac_org
           waers
           appl_curr
           fac_tl
           plan_date
           rgt_amt
           act_pub_amt
           application_date
           applicant
      APPENDING CORRESPONDING FIELDS OF TABLE lt_query_alv
      FROM ztrcdswd4002list
      WHERE ifa_no IN rt_ifa_no
        AND ifa_status IN rt_ifa_status
        AND bukrs IN rt_bukrs
        AND fac_org IN rt_fac_org
        AND dfac_type IN rt_dfac_type
        AND waers IN rt_waers
        AND application_date IN rt_application_date
        AND applicant IN rt_applicant
        AND rgt_val_date IN rt_rgt_val_date
        AND del_ind <> abap_true.
    IF sy-subrc = 0.
      LOOP AT lt_query_alv ASSIGNING <lfs_query_alv> WHERE difa_doc_type IS INITIAL.
        <lfs_query_alv>-difa_doc_type = '3'.
      ENDLOOP.
    ENDIF.

  ENDIF.

  IF lt_query_alv[] IS NOT INITIAL.
    DATA(lt_alv_bukrs) = lt_query_alv[].
    DATA(lt_alv_fac_org) = lt_query_alv[].
    DATA(lt_alv_uname) = lt_query_alv[].

    SORT lt_alv_bukrs BY bukrs.
    DELETE ADJACENT DUPLICATES FROM lt_alv_bukrs COMPARING bukrs.
    IF lt_alv_bukrs[] IS NOT INITIAL.
      SELECT bukrs butxt INTO CORRESPONDING FIELDS OF TABLE lt_bukrs_text
        FROM t001
        FOR ALL ENTRIES IN lt_alv_bukrs
        WHERE bukrs = lt_alv_bukrs-bukrs.
    ENDIF.

    SORT lt_alv_fac_org BY fac_org.
    DELETE ADJACENT DUPLICATES FROM lt_alv_fac_org COMPARING fac_org.
    IF lt_alv_fac_org[] IS NOT INITIAL.
      SELECT partner name_org1 INTO CORRESPONDING FIELDS OF TABLE lt_fac_org_text
        FROM but000
        FOR ALL ENTRIES IN lt_alv_fac_org
        WHERE partner = lt_alv_fac_org-fac_org.
    ENDIF.

    SORT lt_alv_uname BY applicant.
    DELETE ADJACENT DUPLICATES FROM lt_alv_uname COMPARING applicant.
    IF lt_alv_uname[] IS NOT INITIAL.
      SELECT bname
*             name_first
             name_text
             INTO CORRESPONDING FIELDS OF TABLE lt_uname_text
        FROM user_addrp
        FOR ALL ENTRIES IN lt_alv_uname
        WHERE bname = lt_alv_uname-applicant.
    ENDIF.

    SORT lt_bukrs_text BY bukrs.
    SORT lt_fac_org_text BY partner.
    SORT lt_uname_text BY bname.

    LOOP AT lt_query_alv ASSIGNING <lfs_query_alv>.
      READ TABLE lt_bukrs_text INTO DATA(ls_bukrs_text) WITH KEY bukrs = <lfs_query_alv>-bukrs.
      IF sy-subrc = 0.
        <lfs_query_alv>-bukrs_t = ls_bukrs_text-butxt.
      ENDIF.

      READ TABLE lt_fac_org_text INTO DATA(ls_fac_org_text) WITH KEY partner = <lfs_query_alv>-fac_org.
      IF sy-subrc = 0.
        <lfs_query_alv>-fac_org_t = ls_fac_org_text-name_org1.
      ENDIF.

      READ TABLE lt_uname_text INTO DATA(ls_uname_text) WITH KEY bname = <lfs_query_alv>-applicant.
      IF sy-subrc = 0.
        <lfs_query_alv>-applicant_t = ls_uname_text-name_text."name_first.
      ENDIF.

      "判断是否符合担保人条件
      IF rt_guarantor IS INITIAL.
*        APPEND <lfs_query_alv> TO lt_query_alv_final.
      ELSE.
        READ TABLE lt_ztrt4004_04 TRANSPORTING NO FIELDS WITH KEY ifa_no = <lfs_query_alv>-ifa_no_up.
        IF sy-subrc = 0.
*          APPEND <lfs_query_alv> TO lt_query_alv_final.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.

*权限校验
      IF zcl_wd_tr_1013=>gc_authority_check = abap_true.

        CLEAR: lv_bukrs_authority,
               lv_guarantor_authority.

        "公司代码
        AUTHORITY-CHECK OBJECT 'Z_TR420'
                 ID 'BUKRS' FIELD <lfs_query_alv>-bukrs
                 ID 'ZTR_ACT_ZR' FIELD 'A'."直融申请单查询
        IF sy-subrc = 0.
          lv_bukrs_authority = abap_true.
        ENDIF.

        "担保人
        DATA(lt_ztrt4004_04_tmp) = lt_ztrt4004_04[].
        DELETE lt_ztrt4004_04_tmp WHERE ifa_no <> <lfs_query_alv>-ifa_no_up.
        LOOP AT lt_ztrt4004_04_tmp INTO DATA(ls_ztrt4004_04_tmp).
          AUTHORITY-CHECK OBJECT 'Z_TR421'
                   ID 'BUKRS' FIELD ls_ztrt4004_04_tmp-guarantor+5(4)
                   ID 'ZTR_ACT_ZR' FIELD 'A'."直融申请单查询
          IF sy-subrc = 0.
            lv_guarantor_authority = abap_true.
          ENDIF.
        ENDLOOP.

        "公司代码和担保人权限都不满足时报错
        IF lv_bukrs_authority <> abap_true AND lv_guarantor_authority <> abap_true.
*          lv_msgv1 = <lfs_query_alv>-bukrs.
*          zcl_wd_message=>show_msg_detail( EXPORTING
*             ir_controller = wd_this->wd_get_api( )
*             iv_type = 'E'
*             iv_msgid = 'ZTR06'
*             iv_msgno = '043'"您没有公司代码&1下，此操作的权限！
*             iv_msgty = 'E'
*             iv_msgv1 = lv_msgv1
*              ).
*          RETURN.
        ELSE.
          APPEND <lfs_query_alv> TO lt_query_alv_final.
        ENDIF.

      ELSE.
        APPEND <lfs_query_alv> TO lt_query_alv_final.
      ENDIF.

    ENDLOOP.

  ENDIF.

*页面列表赋值
  SORT lt_query_alv_final BY application_date DESCENDING.

  zcl_wd_common=>set_elements(
    EXPORTING
      iv_none     = wd_context
      iv_name     = 'QUERY_ALV'
      it_item     = lt_query_alv_final ).

ENDMETHOD.