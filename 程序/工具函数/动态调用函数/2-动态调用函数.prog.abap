FUNCTION zfm_hr_oa_update_back_dt .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      CT_INDEX_NO STRUCTURE  ZHRS01700
*"----------------------------------------------------------------------
  DATA: ls_index_no LIKE LINE OF ct_index_no,
        lt_index_no LIKE ct_index_no OCCURS 0 WITH HEADER LINE,
        lt_tab1400  TYPE zhrt01400 OCCURS 0 WITH HEADER LINE,
        lt_tab010   TYPE zoat00010 OCCURS 0 WITH HEADER LINE,
        lt_tab020   TYPE zoat00020 OCCURS 0 WITH HEADER LINE,
        lt_tfdir    TYPE tfdir OCCURS 0 WITH HEADER LINE.

  DATA: ls_header TYPE header_fb,
        lt_tables TYPE rsfbpara OCCURS 0 WITH HEADER LINE,
        lt_import TYPE rsfbpara OCCURS 0 WITH HEADER LINE,
        lt_export TYPE rsfbpara OCCURS 0 WITH HEADER LINE,
        lt_change TYPE rsfbpara OCCURS 0 WITH HEADER LINE,
        ls_para   TYPE abap_func_parmbind,
        lt_para   TYPE abap_func_parmbind_tab,
        ls_excep  TYPE abap_func_excpbind,
        lt_excep  TYPE abap_func_excpbind_tab.

  DATA: func_name   TYPE string,
        lv_tabix    TYPE sy-tabix,
        lv_index_no LIKE ls_index_no-index_no,
        lv_json     TYPE string,
        lv_fname    TYPE fieldname.

  DATA: lv_return_s TYPE zoa_return,
        lv_return_u TYPE zoa_return,
        lv_return_r TYPE zoa_return,
        lv_mes_str  TYPE string.

  FIELD-SYMBOLS: <ft_tab>   TYPE STANDARD TABLE,
                 <fs_tab>   TYPE any,
                 <fs_value> TYPE any.

  CONSTANTS: c_head TYPE char04 VALUE 'HEAD',
             c_item TYPE char04 VALUE 'ITEM'.


  CHECK ct_index_no[] IS NOT INITIAL.

  lt_index_no[] = ct_index_no[].

  SORT lt_index_no[] BY index_no.
  DELETE ADJACENT DUPLICATES FROM lt_index_no[] COMPARING index_no.
  CHECK lt_index_no[] IS NOT INITIAL.

  "OA审批回传数据记录表
  SELECT *
         INTO TABLE lt_tab1400
         FROM zhrt01400
    FOR ALL ENTRIES IN lt_index_no[]
        WHERE index_no EQ lt_index_no-index_no.

  SORT lt_tab1400 BY ztype index_no.

  "SAP流程配置基础
  SELECT *
         INTO TABLE lt_tab010
         FROM zoat00010
        WHERE lc_mk EQ 'HR'.

  SORT lt_tab010 BY lc_type.

  "流程业务数据表
  SELECT *
         INTO TABLE lt_tab020
         FROM zoat00020
    FOR ALL ENTRIES IN lt_index_no[]
        WHERE lc_type LIKE 'HR%'
          AND ref_no LIKE '%'
          AND index_no EQ lt_index_no-index_no.

  SORT lt_tab020 BY index_no.

  "功能模块
  SELECT *
         INTO TABLE lt_tfdir
         FROM tfdir
        WHERE funcname LIKE 'Z%'.

  SORT lt_tfdir BY funcname.


  LOOP AT ct_index_no INTO ls_index_no.
    lv_tabix = sy-tabix.
    lv_index_no = ls_index_no-index_no.

    "获取流程数据
    READ TABLE lt_tab020 WITH KEY index_no = ls_index_no-index_no BINARY SEARCH.
    CHECK sy-subrc EQ 0 AND lt_tab020-upd_suc NE c_s.

    "获取流程类型
    READ TABLE lt_tab010 WITH KEY lc_type = lt_tab020-lc_type BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    "获取功能模块
    READ TABLE lt_tfdir WITH KEY funcname = lt_tab010-upd_fun BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    "赋值
    ls_header-name = lt_tfdir-funcname.
    ls_header-area = lt_tfdir-pname.
    ls_header-state = c_a.

    func_name = lt_tfdir-funcname.

    "获取Func.参数
    CALL METHOD cl_fb_parameter_db=>read
      IMPORTING
        tables    = lt_tables[]
        import    = lt_import[]
        export    = lt_export[]
*       change    = lt_change[]
      CHANGING
        header    = ls_header
      EXCEPTIONS
        cancelled = 1
        OTHERS    = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ls_index_no-ermsg.

      MODIFY ct_index_no FROM ls_index_no INDEX lv_tabix TRANSPORTING ermsg.
      CLEAR ls_index_no.

      CONTINUE.
    ENDIF.

    "获取IMPORT
    LOOP AT lt_import.
      CLEAR lt_tab1400.

      ls_para-kind = abap_func_exporting.
      ls_para-name = lt_import-parameter.

      "检查HEAD参数
      IF lt_import-parameter NE c_head.
        READ TABLE lt_tab1400 WITH KEY ztype = space
                                       index_no = lv_index_no BINARY SEARCH.
        IF sy-subrc NE 0.
          READ TABLE lt_tab1400 WITH KEY index_no = lv_index_no.
        ENDIF.

        CHECK sy-subrc EQ 0.

        lv_fname = lt_import-parameter+3(27).

        ASSIGN COMPONENT lv_fname OF STRUCTURE lt_tab1400 TO <fs_value>.
        IF sy-subrc EQ 0.
          GET REFERENCE OF <fs_value> INTO ls_para-value.

          INSERT ls_para INTO TABLE lt_para.
          CLEAR ls_para.
        ENDIF.

      ELSE.
        READ TABLE lt_tab1400 WITH KEY ztype = c_head
                                       index_no = lv_index_no BINARY SEARCH.
        CHECK sy-subrc EQ 0.

        "获取结构化数据VALUE
        PERFORM frm_back_dt_get_value USING lt_import-structure
                                            lt_tab1400-zdata
                                            'S'
                                   CHANGING ls_para-value.
        CHECK ls_para-value IS NOT INITIAL.

        INSERT ls_para INTO TABLE lt_para.
        CLEAR ls_para.
      ENDIF.
    ENDLOOP.

    "获取EXPORT
    LOOP AT lt_export.
      ls_para-kind = abap_func_importing.
      ls_para-name = lt_export-parameter.

      "获取动态类型VALUE
      PERFORM frm_back_dt_get_type_value USING lt_export-structure
                                      CHANGING ls_para-value.

      INSERT ls_para INTO TABLE lt_para.
      CLEAR ls_para.
    ENDLOOP.


    "获取TABLES
    LOOP AT lt_tables.

      ls_para-kind = abap_func_tables.
      ls_para-name = lt_tables-parameter.

      READ TABLE lt_tab1400 WITH KEY ztype = lt_tables-parameter
                                     index_no = lv_index_no BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      "获取结构化数据VALUE
      PERFORM frm_back_dt_get_value USING lt_tables-structure
                                          lt_tab1400-zdata
                                          'T'
                                 CHANGING ls_para-value.
      CHECK ls_para-value IS NOT INITIAL.

      INSERT ls_para INTO TABLE lt_para.
      CLEAR ls_para.
    ENDLOOP.

    "调用Func.
    CALL FUNCTION func_name
      PARAMETER-TABLE
      lt_para
      EXCEPTION-TABLE
      lt_excep.

  ENDLOOP.

ENDFUNCTION.