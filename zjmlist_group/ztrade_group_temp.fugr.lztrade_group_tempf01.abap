*----------------------------------------------------------------------*
***INCLUDE LZTRADE_GROUPF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data .

  CASE g_tb_tab-subscreen.
    WHEN '0201'.

      SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_path_h
        FROM ztrade_path_h.

      LOOP AT gt_path_h.
        CASE gt_path_h-patyp.
          WHEN '1'.
            gt_path_h-patyp_txt = '直营路径'.
          WHEN '2'.
            gt_path_h-patyp_txt = '加盟路径'.
          WHEN OTHERS.
        ENDCASE.

        SELECT SINGLE pathn INTO @DATA(temp_pathn) FROM ztrade_werks WHERE pathn = @gt_path_h-pathn.
        IF sy-subrc = 0.
          gt_path_h-status = '@5B@ 已关联门店'.
        ELSE.
          gt_path_h-status = '@5D@ 未关联门店'.
        ENDIF.

        MODIFY gt_path_h.
      ENDLOOP.

      CHECK lt_step[] IS INITIAL.

      DO 9 TIMES.
        lt_step-typid = sy-index.
        lt_step-typname = sy-index.
        APPEND lt_step.
      ENDDO.

    WHEN '0203'.

      SELECT a~*,b~lgobe,
        CASE ltype WHEN '1' THEN '@5B@ 商品要货库位'
                   WHEN '2' THEN '@5D@ 商品发货库位'
                   WHEN '3' THEN '@5B@ 非商品要货库位'
                   WHEN '4' THEN '@5D@ 非商品发货库位'
                   WHEN '5' THEN '@7Y@ 电商要货收货库位'
                   ELSE '错误' END AS ltype_txt
        INTO CORRESPONDING FIELDS OF TABLE @gt_lgort
        FROM ztrade_lgort AS a
        INNER JOIN t001l AS b ON a~lgort = b~lgort AND a~werks = b~werks
        ORDER BY a~ltype,a~werks.

    WHEN '0204'.


    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_STEP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_step .

  DATA temp_step_str TYPE char30.

  LOOP AT gt_path_t WHERE step IS NOT INITIAL.

    IF gt_path_t-step NS temp_step_str OR temp_step_str IS INITIAL.
      temp_step_str = temp_step_str && gt_path_t-step.
    ELSE.
      MESSAGE |路径步骤 { gt_path_t-step * 10 } 出现多个，请修改| TYPE 'I'.
      return_flag = 'X'.
      RETURN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_PATH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_path_data .

  DATA sum_per TYPE i.

  LOOP AT gt_path_t.
    IF gt_path_t IS INITIAL.
      CONTINUE.
    ELSE.
      sum_per = gt_path_t-mlper + sum_per.
    ENDIF.
  ENDLOOP.

  IF sum_per <> 100.
    MESSAGE |毛利总和 { sum_per } 毛利百分数之和必须等于100| TYPE 'I'.
    return_flag = 'X'.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAVE_PATH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_path_data .

  DATA temp_pathn TYPE c LENGTH 10.
  DATA temp_num TYPE n LENGTH 10.

  DATA lt_path_t LIKE TABLE OF ztrade_path_t WITH HEADER LINE.

  DATA ls_path_h LIKE ztrade_path_h.

  DELETE gt_path_t WHERE step IS INITIAL.

  ls_path_h = CORRESPONDING #( sel_path_h ).

  lt_path_t[] = CORRESPONDING #( gt_path_t[] ).

  IF operation_flag = 'C'.
    SELECT SINGLE MAX( pathn ) INTO temp_pathn FROM ztrade_path_h WHERE patyp = ls_path_h-patyp AND pathn NOT LIKE 'A%'.
  ELSE.
    temp_pathn = ls_path_h-pathn.
  ENDIF.

  SELECT SINGLE FOR UPDATE pathn INTO temp_pathn FROM ztrade_path_h
    WHERE pathn = temp_pathn AND patyp = ls_path_h-patyp.

  IF sy-subrc = 0 OR ( operation_flag = 'C' AND temp_pathn IS INITIAL ).


    SORT lt_path_t BY step.
    READ TABLE lt_path_t INDEX 1.
    ls_path_h-fwerk = lt_path_t-werks.
    ls_path_h-chdat = sy-datum.
    ls_path_h-chtim = sy-uzeit.
    ls_path_h-chnam = sy-uname.

    IF operation_flag = 'C'.
      IF temp_pathn IS INITIAL.
        temp_num = '0000000000'.
      ELSE.
        temp_num = temp_pathn.
      ENDIF.

      temp_num = temp_num + 1.
      ls_path_h-pathn = temp_num.
      ls_path_h-pathn = ls_path_h-patyp && ls_path_h-pathn+1(9).
      lt_path_t-pathn = ls_path_h-pathn.

      MODIFY lt_path_t FROM lt_path_t TRANSPORTING pathn WHERE pathn IS INITIAL.
    ENDIF.
  ELSE.
    return_flag = 'X'.
    MESSAGE '数据被锁定，请稍后重试' TYPE 'I'.
    RETURN.
  ENDIF.

  IF operation_flag = 'M'.

    DELETE FROM ztrade_path_h WHERE pathn = ls_path_h-pathn.
    DELETE FROM ztrade_path_t WHERE pathn = ls_path_h-pathn.
  ENDIF.

  TRY .

      INSERT ztrade_path_h FROM ls_path_h.

      INSERT ztrade_path_t FROM TABLE lt_path_t[].

    CATCH cx_sy_open_sql_db.

      ROLLBACK WORK.

      MESSAGE '保存数据出现错误' TYPE 'I'.
      LEAVE PROGRAM.
  ENDTRY.

  COMMIT WORK.

  MESSAGE '数据保存成功' TYPE 'S'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_SELECT_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_select_path .

  GET CURSOR FIELD fid LINE cursor_line.

  CHECK fid = 'GT_PATH_H-PATHN'.

  current_line = path_h_tc-current_line + cursor_line - 1.

  READ TABLE gt_path_h INDEX current_line.
  IF sy-subrc = 0.
    operation_flag = 'M'.

    sel_path_h = gt_path_h.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_path_t FROM ztrade_path_t WHERE pathn = sel_path_h-pathn
       ORDER BY step.

    CALL SCREEN 0301 STARTING AT 40 1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_NEW_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_new_path .

  CLEAR sel_path_h.
  REFRESH gt_path_t.
  CLEAR gt_path_t.

  operation_flag = 'C'.

  DO 12 TIMES.
    APPEND gt_path_t.
  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_WERKS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_werks_data .

  DATA lt_path LIKE TABLE OF ztrade_path_t WITH HEADER LINE.

  IF sel_werks-bedat <= sy-datum AND sy-mandt = '800'.
    MESSAGE '开始日期必须大于当前日期' TYPE 'I'.
    return_flag = 'X'.
    RETURN.
  ENDIF.

  IF sel_werks-bedat > sel_werks-endat.
    MESSAGE '结束日期必须不小于开始日期' TYPE 'I'.
    return_flag = 'X'.
    RETURN.
  ENDIF.

  IF sel_werks-noeff = 'X'.
    MESSAGE '已失效数据无法再进行修改' TYPE 'I'.
    return_flag = 'X'.
    RETURN.
  ENDIF.

  IF operation_flag = 'C'.

    CALL FUNCTION 'ZGET_WERKS_FWERK'
      EXPORTING
        i_werks = sel_werks-werks
        i_type  = '2'
      TABLES
        ot_path = lt_path
      EXCEPTIONS
        no_path = 1
        OTHERS  = 2.

    IF sy-subrc = 0.
      DELETE lt_path WHERE pathn = sel_werks-pathn.
    ENDIF.

    SELECT * INTO TABLE @DATA(pt_path) FROM ztrade_path_t WHERE pathn = @sel_werks-pathn ORDER BY step DESCENDING.
    READ TABLE pt_path INTO DATA(wa_path) INDEX 1.
    LOOP AT lt_path.
      IF wa_path-werks <> lt_path-werks.
        MESSAGE |一个门店不能同时2个不同结束工厂的路径| TYPE 'I'.
        return_flag = 'X'.
        RETURN.
      ENDIF.
    ENDLOOP.


    SELECT b~pathn, b~fwerk INTO TABLE @DATA(lt_werks)
      FROM ztrade_werks AS a
      INNER JOIN ztrade_path_h AS b ON a~pathn = b~pathn
      WHERE a~werks = @sel_werks-werks AND a~pathn <> @sel_werks-pathn.

    SELECT SINGLE fwerk INTO @DATA(temp_fwerk) FROM ztrade_path_h WHERE pathn = @sel_werks-pathn.

    READ TABLE lt_werks TRANSPORTING NO FIELDS WITH KEY fwerk = temp_fwerk.
    IF sy-subrc = 0.
      MESSAGE '一个门店不能同时关联2个相同起始工厂的路径' TYPE 'I'.
      return_flag = 'X'.
      RETURN.
    ENDIF.
  ENDIF.

  SELECT SINGLE * INTO @DATA(ls_trade_werks)
     FROM ztrade_werks
     WHERE werks = @sel_werks-werks AND pathn = @sel_werks-pathn
       AND ( ( bedat <= @sel_werks-endat AND endat >= @sel_werks-bedat AND nfdat = '00000000' ) OR
             ( bedat <= @sel_werks-endat AND nfdat >= @sel_werks-bedat AND nfdat <> '00000000' ) ).

  IF sy-subrc = 0.
    MESSAGE '该路径下门店的期间存在重复' TYPE 'I'.
    return_flag = 'X'.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAVE_WERKS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_werks_data .

  DATA lt_werks LIKE TABLE OF ztrade_werks WITH HEADER LINE.

  IF sy-dynnr = '0302'.


    IF operation_flag = 'M'.

      PERFORM lock_data USING sel_werks.

      CHECK return_flag IS INITIAL.

      DELETE FROM ztrade_werks WHERE pathn = sel_werks-pathn AND werks = sel_werks-werks
                                 AND bedat = sel_werks-bedat AND endat = sel_werks-endat.

    ELSEIF operation_flag = 'C'.

    ENDIF.

    lt_werks = CORRESPONDING #( sel_werks ).

    APPEND lt_werks.

  ELSEIF sy-dynnr = '0303'.

    lt_werks[] = CORRESPONDING #( gt_excel[] ).

    DELETE ztrade_werks FROM TABLE lt_werks[].
  ENDIF.

  TRY .

      INSERT ztrade_werks FROM TABLE lt_werks[].

    CATCH cx_sy_open_sql_db.

      ROLLBACK WORK.
      MESSAGE '保存数据出现错误' TYPE 'I'.
      LEAVE PROGRAM.

  ENDTRY.

  save_flag = 'X'.

  COMMIT WORK.

  MESSAGE '保存成功' TYPE 'S'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_SELECT_WERKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_select_werks .

  GET CURSOR FIELD fid LINE cursor_line.

  CHECK fid = 'GT_WERKS-WERKS'.

  current_line = werks_tc-current_line + cursor_line - 1.

  READ TABLE gt_werks INDEX current_line.
  IF sy-subrc = 0.
    operation_flag = 'M'.

    sel_werks = gt_werks.

    SELECT SINGLE path_txt INTO sel_werks-path_txt FROM ztrade_path_h WHERE pathn = sel_werks-pathn.

    CALL SCREEN 0302 STARTING AT 40 1.

    IF save_flag = 'X'.
      CLEAR save_flag.
      PERFORM filter_werks_data.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILTER_WERKS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM filter_werks_data .

  DATA where_condition TYPE string.

  GET CURSOR FIELD fid LINE cursor_line.

  CHECK fid = 'P_PATHN' OR fid = 'P_WERKS'.

  IF p_pathn IS INITIAL AND p_werks IS INITIAL.
    MESSAGE '路径编号与门店编号不能同时为空' TYPE 'I'.
    return_flag = 'X'.
    RETURN.
  ENDIF.

  IF p_pathn IS NOT INITIAL.
    TRANSLATE p_pathn TO UPPER CASE.
    where_condition = ' pathn = p_pathn '.
  ENDIF.

  IF p_werks IS NOT INITIAL AND where_condition IS INITIAL.
    TRANSLATE p_werks TO UPPER CASE.
    where_condition = ' werks = p_werks '.
  ELSEIF p_werks IS NOT INITIAL AND where_condition IS NOT INITIAL.
    TRANSLATE p_werks TO UPPER CASE.
    where_condition = where_condition && ' and werks = p_werks '.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_werks
    FROM ztrade_werks
    WHERE (where_condition).

  LOOP AT gt_werks.

    IF gt_werks-nfdat = '00000000' AND gt_werks-endat < sy-datum.
      gt_werks-status = '@06@'.
    ELSEIF gt_werks-nfdat <> '00000000' AND gt_werks-nfdat < sy-datum.
      gt_werks-status = '@06@'.
    ELSE.
      gt_werks-status = '@5B@'.
    ENDIF.

    MODIFY gt_werks.
  ENDLOOP.

  IF gt_werks[] IS INITIAL.
    MESSAGE '未找到数据' TYPE 'W'.
  ENDIF.

  path_t_tc-top_line = 0.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHOOSE_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM choose_path .

  DATA filen TYPE filetable.
  DATA filename LIKE LINE OF filen.
  DATA rc TYPE sy-subrc.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title      = 'Excel选择'
      default_extension = 'XLS'
    CHANGING
      file_table        = filen
      rc                = rc.

  LOOP AT filen INTO filename.
    p_path = filename-filename.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_EXCEL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_excel_data .

  DATA type TYPE char1.
  DATA msg TYPE bapi_msg.
  DATA lt_excel_data LIKE TABLE OF alsmex_tabline WITH HEADER LINE.
  DATA error_flag TYPE c.
  DATA lt_path LIKE TABLE OF ztrade_path_t WITH HEADER LINE.

  REFRESH lt_excel_data.
  REFRESH gt_excel.
  REFRESH pt_excel.

  CALL FUNCTION 'ZREAD_EXCEL1'
    EXPORTING
      i_path        = p_path
      i_row         = 2
      i_col         = 101
    IMPORTING
      o_type        = type
      o_tmsg        = msg
    TABLES
      it_excel_data = lt_excel_data.

  IF type = 'E'.
    MESSAGE msg TYPE 'I'.
    return_flag = 'X'.
    RETURN.
  ELSE.
    SORT lt_excel_data BY row.
    LOOP AT lt_excel_data.
      CASE lt_excel_data-col.
        WHEN 1.
          gt_excel-pathn = lt_excel_data-value.
        WHEN 2.
          gt_excel-werks = lt_excel_data-value.
        WHEN 3.
          gt_excel-bedat = lt_excel_data-value.
        WHEN 4.
          gt_excel-endat = lt_excel_data-value.

        WHEN OTHERS.
      ENDCASE.

      AT END OF row.
        APPEND gt_excel.
      ENDAT.

    ENDLOOP.
  ENDIF.

  LOOP AT gt_excel.

    IF gt_excel IS INITIAL.
      DELETE gt_excel.
      CONTINUE.
    ENDIF.

    gt_excel-line = sy-tabix.

    IF gt_excel-pathn IS INITIAL.
      gt_excel-msg = '路径编号为空'.
    ELSE.
      SELECT SINGLE path_txt INTO gt_excel-path_txt FROM ztrade_path_h WHERE pathn = gt_excel-pathn.
      IF sy-subrc <> 0.
        gt_excel-msg = '路径编号不存在'.
      ENDIF.
    ENDIF.

    IF gt_excel-werks IS INITIAL.
      gt_excel-msg = '门店编号不存在'.
    ELSE.
      SELECT SINGLE name1 INTO gt_excel-werks_name FROM t001w WHERE werks = gt_excel-werks.
      IF sy-subrc <> 0.
        gt_excel-msg = '门店编号不存在'.
      ENDIF.
    ENDIF.

    IF gt_excel-endat = '00000000' OR gt_excel-endat = space.
      gt_excel-endat = '99991231'.
    ENDIF.

    IF gt_excel-bedat = '00000000'.
      gt_excel-msg = '开始日期不能为空'.
    ELSEIF gt_excel-bedat > gt_excel-endat.
      gt_excel-msg = '开始日期不能大于结束日期'.
    ELSEIF gt_excel-bedat < sy-datum AND sy-mandt = '800'.
      gt_excel-msg = '开始日期不能小于当前日期'.
    ENDIF.

    IF gt_excel-msg IS INITIAL.

      SELECT b~pathn, b~fwerk INTO TABLE @DATA(lt_werks)
        FROM ztrade_werks AS a
        INNER JOIN ztrade_path_h AS b ON a~pathn = b~pathn
        WHERE a~werks = @gt_excel-werks AND a~pathn <> @gt_excel-pathn
        AND ( ( nfdat BETWEEN @gt_excel-bedat AND @gt_excel-endat AND nfdat <> '00000000' ) OR (
        nfdat = '00000000' AND bedat <= @gt_excel-endat AND endat >= @gt_excel-bedat ) ) .

      SELECT SINGLE fwerk INTO @DATA(temp_fwerk) FROM ztrade_path_h WHERE pathn = @gt_excel-pathn.

      READ TABLE lt_werks TRANSPORTING NO FIELDS WITH KEY fwerk = temp_fwerk.
      IF sy-subrc = 0.
        gt_excel-msg = '一个门店不能同时关联2个相同起始工厂的路径'.
      ENDIF.

    ENDIF.

    IF gt_excel-msg IS INITIAL.
      REFRESH lt_path.
      CLEAR lt_path.
      CALL FUNCTION 'ZGET_WERKS_FWERK'
        EXPORTING
          i_werks = gt_excel-werks
          i_type  = '2'
        TABLES
          ot_path = lt_path
        EXCEPTIONS
          no_path = 1
          OTHERS  = 2.

      IF sy-subrc = 0.
        DELETE lt_path WHERE pathn = gt_excel-pathn.
      ENDIF.

      SELECT * INTO TABLE @DATA(pt_path) FROM ztrade_path_t
        WHERE pathn = @gt_excel-pathn ORDER BY step DESCENDING.

      READ TABLE pt_path INTO DATA(wa_path) INDEX 1.
      LOOP AT lt_path.
        IF wa_path-werks <> lt_path-werks.
          gt_excel-msg = '一个门店不能同时2个不同结束工厂的路径'.
          EXIT.
        ENDIF.
      ENDLOOP.

    ENDIF.

    IF gt_excel-msg IS INITIAL.

      SELECT SINGLE * INTO @DATA(ls_trade_werks)
       FROM ztrade_werks
       WHERE werks = @gt_excel-werks AND pathn = @gt_excel-pathn
         AND ( ( bedat <= @gt_excel-endat AND endat >= @gt_excel-bedat AND nfdat = '00000000' ) OR
               ( bedat <= @gt_excel-endat AND nfdat >= @gt_excel-bedat AND nfdat <> '00000000' ) ).

      IF sy-subrc = 0.
        gt_excel-msg = '该路径下门店的期间存在重复'.
      ENDIF.

    ENDIF.

    IF gt_excel-msg IS INITIAL.
      LOOP AT pt_excel WHERE werks = gt_excel-werks AND bedat <= gt_excel-endat AND endat >= gt_excel-bedat.
        gt_excel-msg = '该门店在导入的数据中存在期间重复'.
        EXIT.
      ENDLOOP.
    ENDIF.

    IF gt_excel-msg IS NOT INITIAL.
      gt_excel-status = '@5C@'.
    ELSE.
      gt_excel-status = '@5B@'.
    ENDIF.

    MODIFY gt_excel.
    APPEND gt_excel TO pt_excel.
    CLEAR gt_excel.
  ENDLOOP.

  SORT gt_excel BY status DESCENDING.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_EXCEL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_excel_data .

  READ TABLE gt_excel WITH KEY status = '@5B@'.
  IF sy-subrc <> 0.
    MESSAGE '没有可保存的数据' TYPE 'I'.
    return_flag = 'X'.
    RETURN.
  ENDIF.

  READ TABLE gt_excel WITH KEY status = '@5C@'.
  IF sy-subrc = 0.
    MESSAGE '存在不正确的数据，此类数据系统保存时将忽略' TYPE 'I'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXPORT_EXCLE_TEMPLATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM export_excle_template .

  DATA zdialog_elements TYPE zdialog_elements.

  zdialog_elements-default_extension = 'XLS'.
  zdialog_elements-default_file_name = '门店路径关联模板'.
  CALL FUNCTION 'ZEXPORT_EXCEL_TEMPLATE'
    EXPORTING
      objid           = 'ZPATH'
      dialog_elements = zdialog_elements.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DELETE_WERKS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cancle_werks_data .

  LOOP AT gt_werks WHERE sel = 'X'.

    PERFORM lock_data USING gt_werks.

    IF return_flag = 'X'.
      RETURN.
    ELSE.
      gt_werks-status = '@5B@'.
      gt_werks-nfdat = p_nfdat.
      gt_werks-noeff = 'X'.
      MODIFY gt_werks.

      UPDATE ztrade_werks SET noeff = 'X' nfdat = p_nfdat
      WHERE pathn = gt_werks-pathn AND werks = gt_werks-werks.
    ENDIF.
  ENDLOOP.

  MESSAGE '数据更新完成' TYPE 'S'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LOCK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lock_data USING ls_werks LIKE gs_werks.

  DATA ls_trade_werks LIKE ztrade_werks.

  SELECT SINGLE FOR UPDATE * INTO ls_trade_werks
     FROM ztrade_werks
     WHERE pathn = ls_werks-pathn
       AND werks = ls_werks-werks
       AND bedat = ls_werks-bedat
       AND endat = ls_werks-endat.

  IF sy-subrc <> 0.
    MESSAGE '该数据已经被其他人操作，请刷新重新' TYPE 'I'.
    return_flag = 'X'.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SHOW_JM_ODLIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ODLIST  text
*      -->P_LT_RETURN  text
*----------------------------------------------------------------------*
FORM show_yh_odlist  USING yhdln TYPE char10 i_type TYPE c.

  DATA lt_status TYPE TABLE OF char20 WITH HEADER LINE.
  DATA temp_ogrup TYPE i.
  DATA ls_node TYPE zsimple_treenodes.
  DATA temp_node_key TYPE i.
  DATA temp_relat_key TYPE i.

  IF i_type = '1'.
    SELECT SINGLE * INTO @DATA(ls_yh_h1) FROM zmm_spyh_h WHERE yhdln = @yhdln.
    gs_yh_list = CORRESPONDING #( ls_yh_h1 ).
  ELSEIF i_type = '3'.
    SELECT SINGLE * INTO @DATA(ls_yh_h2) FROM zmm_fspyh_h WHERE yhdln = @yhdln.
    gs_yh_list = CORRESPONDING #( ls_yh_h2 ).
  ENDIF.

  CASE gs_yh_list-status.
    WHEN 'A'.
      temp_ogrup = 1.
    WHEN 'B'.
      temp_ogrup = 2.
    WHEN 'C'.
      temp_ogrup = 3.
    WHEN 'D'.
      temp_ogrup = 4.
    WHEN 'F'.
      temp_ogrup = 5.
    WHEN 'X'.
      temp_ogrup = 9.
    WHEN OTHERS.
  ENDCASE.

  CHECK temp_ogrup <> 9.

  lt_status[] = VALUE #( ( '已保存' ) ( '已审批' ) ( '已拣配' ) ( '已暂存' ) ( '已发货' )  ).

  DO temp_ogrup TIMES.

    READ TABLE lt_status INDEX sy-index.

    temp_node_key = temp_node_key + 1.
    temp_relat_key = temp_node_key.

    ls_node-isfolder = 'X'.
    ls_node-node_key = temp_node_key.
    ls_node-expand_child = 'X'.
    ls_node-text = |SETP{ sy-index }    { lt_status }|.
    APPEND ls_node TO gt_yhlist_node.

    CLEAR ls_node.

    CASE sy-index.
      WHEN '2'.
        SELECT * INTO TABLE @DATA(lt_yhbel_list) FROM zmm_yhbel_list
          WHERE jmbln = @yhdln AND jmtyp = '1'.

        LOOP AT lt_yhbel_list INTO DATA(ls_yhbel_list) WHERE btype = '01' OR btype = '02'.
          ls_node-isfolder = 'X'.
          temp_node_key = temp_node_key + 1.
          ls_node-node_key = temp_node_key.
          ls_node-n_image = '@3D@'.
          ls_node-exp_image = '@3D@'.
          ls_node-node_txt1 = '5'.
          ls_node-node_txt2 = ls_yhbel_list-belnr && ls_yhbel_list-gjahr && ls_yhbel_list-bukrs.
          ls_node-text = |会计凭证 { ls_yhbel_list-belnr }       { ls_yhbel_list-crdat }|.

          ls_node-relatkey = temp_relat_key.
          APPEND ls_node TO gt_yhlist_node.

          CLEAR ls_node.
        ENDLOOP.
      WHEN OTHERS.

    ENDCASE.

  ENDDO.

  IF gs_yh_list-status = 'D' OR gs_yh_list-status = 'F'.
    SELECT SINGLE bhnuc INTO gs_yh_list-bhnuc FROM zsd_jmfh_h WHERE yhdln = gs_yh_list-yhdln.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SHOW_JM_ODLIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ODLIST  text
*      -->P_LT_RETURN  text
*----------------------------------------------------------------------*
FORM get_odlist_head USING type TYPE c bhnuc TYPE char10 pathn TYPE char10.

  IF type = '2'.

    SELECT SINGLE * INTO @DATA(ls_jmfh_h) FROM zsd_jmfh_h WHERE bhnuc = @bhnuc.
    gs_fh_list = CORRESPONDING #( ls_jmfh_h ).

    SELECT SINGLE name1 INTO gs_fh_list-werks_name FROM t001w WHERE werks = gs_fh_list-werks.
    CASE gs_fh_list-jmstu.
      WHEN '1'.
        gs_fh_list-jmstu_txt = '@5B@ 保存未发货'.
      WHEN '2'.
        gs_fh_list-jmstu_txt = '@5D@ 发货中断'.
      WHEN '3'.
        gs_fh_list-jmstu_txt = '@06@ 已发货'.
      WHEN OTHERS.
    ENDCASE.

    CASE gs_fh_list-mtype.
      WHEN '01'.
        gs_fh_list-mtype_txt = '商品'.
      WHEN '02'.
        gs_fh_list-mtype_txt = '非商品'.
      WHEN OTHERS.
    ENDCASE.

    CASE gs_fh_list-stype.
      WHEN '1'.
        gs_fh_list-mtype_txt = gs_fh_list-mtype_txt && '补货'.
      WHEN '2'.
        gs_fh_list-mtype_txt = gs_fh_list-mtype_txt && '要货'.
      WHEN '3'.
        gs_fh_list-mtype_txt = gs_fh_list-mtype_txt && 'OA'.
      WHEN OTHERS.
    ENDCASE.

  ELSEIF type = '4'.

    SELECT SINGLE * INTO @DATA(ls_jmth_h) FROM zsd_jmth_h WHERE ebeln = @bhnuc." AND pathn = @pathn.
    gs_th_list = CORRESPONDING #( ls_jmth_h ).
    gs_th_list-pathn = pathn.
    SELECT SINGLE name1 INTO gs_th_list-werks_name FROM t001w WHERE werks = gs_th_list-werks.

    CASE gs_th_list-thstu.
      WHEN '1'.
        gs_th_list-thstu_txt = '@5B@ 待扫码'.
      WHEN '2'.
        gs_th_list-thstu_txt = '@5H@ 待退货'.
      WHEN '3'.
        gs_th_list-thstu_txt = '@5D@ 退货中'.
      WHEN '4'.
        gs_th_list-thstu_txt = '@2K@ 待退款'.
      WHEN '5'.
        gs_th_list-thstu_txt = '@06@ 已退款'.
      WHEN OTHERS.
    ENDCASE.

  ELSEIF type = '5'.

    SELECT SINGLE * INTO @DATA(ls_kyth_h) FROM zmm_kyth_h WHERE bhnuc = @bhnuc.
    gs_kyth_list = CORRESPONDING #( ls_kyth_h ).
    gs_kyth_list-werks = ls_kyth_h-fwerk.
    SELECT SINGLE name1 INTO gs_kyth_list-werks_name FROM t001w WHERE werks = gs_kyth_list-werks.
    gs_kyth_list-pathn = pathn.
    gs_kyth_list-crdat = ls_kyth_h-thdat.
    gs_kyth_list-crtim = ls_kyth_h-thtim.
    CASE gs_kyth_list-thstu.
      WHEN '1'.
        gs_kyth_list-thstu_txt = '@5D@ 退货中'.
      WHEN '2'.
        gs_kyth_list-thstu_txt = '@06@ 已退款'.
      WHEN OTHERS.
    ENDCASE.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SHOW_JM_ODLIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ODLIST  text
*      -->P_LT_RETURN  text
*----------------------------------------------------------------------*
FORM show_jm_odlist TABLES it_return STRUCTURE bapiret2 USING type TYPE c bhnuc TYPE char10.

  DATA ls_node TYPE zsimple_treenodes.
  DATA temp_node_key TYPE i.
  DATA temp_relat_key TYPE i.

  DATA lt_odlist LIKE TABLE OF ls_odlist WITH HEADER LINE.
  DATA lt_jm_path LIKE TABLE OF zlsjm_path WITH HEADER LINE.
  DATA gt_node LIKE TABLE OF zsimple_treenodes WITH HEADER LINE.
  DATA last_line TYPE c.


  IF type = '2'.

    CALL FUNCTION 'ZGET_WERKS_PATH'
      EXPORTING
        i_werks    = gs_fh_list-werks
        i_fwerk    = gs_fh_list-fwerk
        i_pdate    = gs_fh_list-fhdat
        i_pathn    = gs_fh_list-pathn
      TABLES
        ot_jm_path = lt_jm_path.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_odlist
      FROM ztrade_odlist
      WHERE bhnuc = bhnuc ORDER BY ogrup step.

  ELSEIF type = '4'.

    CALL FUNCTION 'ZGET_WERKS_PATH'
      EXPORTING
        i_werks    = gs_th_list-werks
        i_pathn    = gs_th_list-pathn
        i_type     = '2'
      TABLES
        ot_jm_path = lt_jm_path.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_odlist
      FROM ztrade_thlist
      WHERE ebeln = gs_th_list-ebeln AND ( pathn = gs_th_list-pathn OR pathn = space ) ORDER BY ogrup step.

  "add by handyxh 20181115
   ELSEIF type = '5'.

     CALL FUNCTION 'ZGET_WERKS_PATH'
      EXPORTING
        i_werks    = gs_kyth_list-werks
        i_pathn    = gs_kyth_list-pathn
        i_type     = '2'
      TABLES
        ot_jm_path = lt_jm_path.

     SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_odlist
       FROM zmm_kythlist
       WHERE bhnuc = gs_kyth_list-bhnuc and ( pathn = gs_kyth_list-pathn OR pathn = space ) ORDER BY ogrup step.

  ENDIF.

  PERFORM handle_belnr_node TABLES lt_odlist gt_node USING bhnuc temp_node_key temp_relat_key.

  IF gs_th_list IS NOT INITIAL OR gs_kyth_list IS NOT INITIAL.
    DELETE lt_jm_path INDEX 1.
    DELETE lt_odlist WHERE ogrup = 001.
  ENDIF.

  LOOP AT lt_jm_path.

    IF type = '2' AND sy-tabix = lines( lt_jm_path[] ) .
      last_line = 'X'.
    ELSE.
      last_line = space.
    ENDIF.

    temp_node_key = temp_node_key + 1.

    ls_node-text = |STEP{ lt_jm_path-ogrup }     { lt_jm_path-from_werks }-{ lt_jm_path-to_werks } 之间单据|  .
    ls_node-isfolder = 'X'.

    ls_node-node_key = temp_node_key.
    ls_node-expand_child = 'X'.
    APPEND ls_node TO gt_node.

    temp_relat_key = temp_node_key.
    CLEAR ls_node.

    DO 4 TIMES.

      temp_node_key = temp_node_key + 1.
      ls_node-isfolder = 'X'.
      ls_node-node_key = temp_node_key.
      ls_node-relatkey = temp_relat_key.

      READ TABLE lt_odlist WITH KEY ogrup = lt_jm_path-ogrup step = sy-index.
      IF sy-subrc = 0.

        ls_node-n_image = '@3D@'.
        ls_node-exp_image = '@3D@'.
        ls_node-node_txt1 = lt_odlist-odetp.
        ls_node-node_txt2 = lt_odlist-odeln && lt_odlist-odjhr && lt_odlist-bukrs.

        CASE lt_odlist-odetp.
          WHEN 1.
            ls_node-text = |采购订单 { lt_odlist-odeln }       { lt_odlist-crdat }|.
          WHEN 2.
            ls_node-text = |销售订单 { lt_odlist-odeln }       { lt_odlist-crdat }|.
          WHEN 3.
            ls_node-text = |外向交货 { lt_odlist-odeln }       { lt_odlist-crdat }|.
          WHEN 4.
            ls_node-text = |单据收货 { lt_odlist-odeln }       { lt_odlist-crdat }|.
          WHEN 5.
            ls_node-text = |会计凭证 { lt_odlist-odeln }       { lt_odlist-crdat }|.
          WHEN OTHERS.
        ENDCASE.

      ELSEIF last_line <> 'X'.

        ls_node-n_image = '@WA@'.
        ls_node-exp_image = '@WA@'.

        IF type = '2'.
          CASE sy-index.
            WHEN 1.
              ls_node-text = |采购订单   未创建|.
            WHEN 2.
              ls_node-text = |外向交货   未创建|.
            WHEN 3.
              ls_node-text = |交货收货   未创建|.
            WHEN 4.
              ls_node-text = |采购收货   未创建|.
            WHEN OTHERS.
          ENDCASE.
        ELSEIF type = '4'.
          CASE sy-index.
            WHEN 1.
              ls_node-text = |采购订单   未创建|.
            WHEN 2.
              ls_node-text = |外向交货   未创建|.
            WHEN 3.
              ls_node-text = |采购收货   未创建|.
            WHEN 4.
              ls_node-text = |交货收货   未创建|.
            WHEN OTHERS.
          ENDCASE.
        ELSEIF type = '5'. "add by handyxh 20181116
          CASE sy-index.
            WHEN 1.
              ls_node-text = |采购订单   未创建|.
            WHEN 2.
              ls_node-text = |外向交货   未创建|.
            WHEN 3.
              ls_node-text = |采购收货   未创建|.
            WHEN 4.
              ls_node-text = |交货收货   未创建|.
            WHEN OTHERS.
          ENDCASE.
        ENDIF.

      ELSEIF last_line = 'X'.

        ls_node-n_image = '@WA@'.
        ls_node-exp_image = '@WA@'.

        IF type = '2'.
          CASE sy-index.
            WHEN 1.
              ls_node-text = |销售订单   未创建|.
            WHEN 2.
              ls_node-text = |外向交货   未创建|.
            WHEN 3.
              ls_node-text = |交货收货   未创建|.
            WHEN 4.
              ls_node-text = |采购订单   未创建|.
            WHEN OTHERS.
          ENDCASE.
        ENDIF.

      ENDIF.

      APPEND ls_node TO gt_node.
      CLEAR ls_node.
    ENDDO.
  ENDLOOP.

  IF type = '2'.
    gt_odlist_node[] = gt_node[].
  ELSEIF type = '4' .
    gt_thlist_node[] = gt_node[].
  ELSEIF type = '5'.
    gt_kythlist_node[] = gt_node[].
  ENDIF.

  lt_return[] = it_return[].

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POP_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pop_message .

  DATA(cl_error) = NEW cl_isu_error_log( ).

  LOOP AT lt_return.
    cl_error->add_message( EXPORTING x_msgid = lt_return-id
                                     x_msgty = lt_return-type
                                     x_msgno = lt_return-number
                                     x_msgv1 = lt_return-message_v1
                                     x_msgv2 = lt_return-message_v2
                                     x_msgv3 = lt_return-message_v3
                                     x_msgv4 = lt_return-message_v4 ).
  ENDLOOP.

  cl_error->display_messages( EXPORTING x_single_in_status_line = space EXCEPTIONS OTHERS = 1 ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELE_CANCLE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sele_cancle_data .

  DATA lt_sval LIKE TABLE OF sval WITH HEADER LINE.
  DATA temp_date TYPE sy-datum.

  READ TABLE gt_werks WITH KEY sel = 'X'.
  IF sy-subrc <> 0.
    MESSAGE '没有数据被选择' TYPE 'I'.
    return_flag = 'X'.
    RETURN.
  ENDIF.

  lt_sval-tabname = 'VBAK'.
  lt_sval-fieldname = 'ERDAT'.
  lt_sval-fieldtext = '失效日期'.
  lt_sval-field_obl = 'X'.
  APPEND lt_sval.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = '请选择失效日期'
    TABLES
      fields          = lt_sval
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    return_flag = 'X'.
    RETURN.
  ELSE.

    IF lt_sval[] IS NOT INITIAL AND sy-ucomm = 'FURT'.
      READ TABLE lt_sval INDEX 1.
      temp_date = lt_sval-value.


      IF temp_date <= sy-datum.
        MESSAGE '失效日期必须大于今天' TYPE 'I'.
        return_flag = 'X'.
        RETURN.
      ELSE.
        p_nfdat = lt_sval-value.
      ENDIF.
    ELSE.
      return_flag = 'X'.
      RETURN.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_JM_AMOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_jm_amount .

  DATA lt_path TYPE TABLE OF ztrade_path_t WITH HEADER LINE.

  DATA temp_kunnr TYPE kunnr.

  REFRESH gt_jmat_t.

  TRANSLATE gs_jmat_h-werks TO UPPER CASE.
  TRANSLATE gs_jmat_h-bukrs TO UPPER CASE.

  SELECT SINGLE betrp INTO @DATA(temp_betrp) FROM wrf1 WHERE locnr = @gs_jmat_h-werks.
  IF sy-subrc <> 0.
    MESSAGE '门店编号不存在' TYPE 'I'.
    RETURN.
  ELSEIF temp_betrp <> 'Z003' AND temp_betrp <> 'Z004'.
    MESSAGE '门店不是加盟门店' TYPE 'I'.
    RETURN.
  ENDIF.

  SELECT SINGLE kunn2 INTO temp_kunnr FROM knvp WHERE kunnr = gs_jmat_h-werks AND parvw = 'RE'.
  IF sy-subrc <> 0.
    MESSAGE '门店所属加盟商不存在' TYPE 'I'.
  ENDIF.

  IF gs_jmat_h-bukrs IS INITIAL.
    CALL FUNCTION 'ZGET_WERKS_FWERK'
      EXPORTING
        i_werks = gs_jmat_h-werks
        i_type  = '2'
      TABLES
        ot_path = lt_path
      EXCEPTIONS
        no_path = 1
        OTHERS  = 2.
    IF sy-subrc <> 0.
      MESSAGE '请指定关联公司' TYPE 'I'.
      RETURN.
    ENDIF.

    LOOP AT lt_path.

      gt_jmat_t-bukrs = lt_path-bukrs.

      CALL FUNCTION 'ZISD003_4'
        EXPORTING
          i_kunnr     = temp_kunnr
          i_bukrs     = lt_path-bukrs
          i_type      = '1'
          i_nochk     = 'X'
        IMPORTING
          left_amount = gt_jmat_t-amount.

      APPEND gt_jmat_t.
      CLEAR gt_jmat_t.
    ENDLOOP.

  ELSE.

    gt_jmat_t-bukrs = gs_jmat_h-bukrs.

    CALL FUNCTION 'ZISD003_4'
      EXPORTING
        i_kunnr     = temp_kunnr
        i_bukrs     = gs_jmat_h-bukrs
        i_type      = '1'
        i_nochk     = 'X'
      IMPORTING
        left_amount = gt_jmat_t-amount.

    APPEND gt_jmat_t.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_BELNR_NODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM handle_belnr_node TABLES lt_odlist STRUCTURE ls_odlist gt_node STRUCTURE zsimple_treenodes
                        USING bhnuc TYPE char10 temp_node_key TYPE i temp_relat_key TYPE i.

  DATA ls_node TYPE zsimple_treenodes.

  IF gs_fh_list-stype = '1'."补货

    temp_node_key = temp_node_key + 1.

    ls_node-text = |STEP000     { bhnuc } 补货扣款|  .
    ls_node-isfolder = 'X'.
    ls_node-node_key = temp_node_key.
    ls_node-expand_child = 'X'.
    APPEND ls_node TO gt_node.
    CLEAR ls_node.

    temp_relat_key = temp_node_key.
    DO 2 TIMES.

      temp_node_key = temp_node_key + 1.
      ls_node-isfolder = 'X'.
      ls_node-node_key = temp_node_key.
      ls_node-relatkey = temp_relat_key.

      READ TABLE lt_odlist WITH KEY ogrup = 000 step = sy-index.
      IF sy-subrc = 0.
        ls_node-n_image = '@3D@'.
        ls_node-exp_image = '@3D@'.
        ls_node-node_txt1 = lt_odlist-odetp.
        ls_node-node_txt2 = lt_odlist-odeln && lt_odlist-odjhr && lt_odlist-bukrs.
        ls_node-text = |会计凭证 { lt_odlist-odeln }       { lt_odlist-crdat }|.

        APPEND ls_node TO gt_node.

      ELSEIF ( sy-index = 2 AND gs_fh_list-linkn <> space ) OR sy-index = 1.

        ls_node-n_image = '@WA@'.
        ls_node-exp_image = '@WA@'.

        CASE sy-index.
          WHEN 1.
            ls_node-text = |扣款凭证   未创建|.
          WHEN 2.
            ls_node-text = |信贷凭证   未创建|.
          WHEN OTHERS.
        ENDCASE.

        APPEND ls_node TO gt_node.

      ENDIF.
      CLEAR ls_node.

    ENDDO.

  ELSEIF gs_fh_list-stype = '3'."OA

    ls_node-text = |STEP000     { bhnuc } 采购入库|  .
    ls_node-isfolder = 'X'.
    ls_node-node_key = temp_node_key.
    ls_node-expand_child = 'X'.
    APPEND ls_node TO gt_node.
    CLEAR ls_node.

    temp_relat_key = temp_node_key.

    LOOP AT lt_odlist WHERE ogrup = 000.

      temp_node_key = temp_node_key + 1.
      ls_node-isfolder = 'X'.
      ls_node-node_key = temp_node_key.
      ls_node-relatkey = temp_relat_key.

      ls_node-n_image = '@3D@'.
      ls_node-exp_image = '@3D@'.
      ls_node-node_txt1 = lt_odlist-odetp.
      ls_node-node_txt2 = lt_odlist-odeln && lt_odlist-odjhr && lt_odlist-bukrs.

      CASE lt_odlist-odetp.
        WHEN 1.
          ls_node-text = |采购订单 { lt_odlist-odeln }       { lt_odlist-crdat }|.
        WHEN 2.
          ls_node-text = |销售订单 { lt_odlist-odeln }       { lt_odlist-crdat }|.
        WHEN 3.
          ls_node-text = |外向交货 { lt_odlist-odeln }       { lt_odlist-crdat }|.
        WHEN 4.
          ls_node-text = |单据收货 { lt_odlist-odeln }       { lt_odlist-crdat }|.
        WHEN 5.
          ls_node-text = |会计凭证 { lt_odlist-odeln }       { lt_odlist-crdat }|.
        WHEN OTHERS.
      ENDCASE.

      APPEND ls_node TO gt_node.
      CLEAR ls_node.
    ENDLOOP.

  ELSEIF gs_th_list IS NOT INITIAL.

    ls_node-text = |STEP000     { bhnuc } 总部接收|  .
    ls_node-isfolder = 'X'.
    ls_node-node_key = temp_node_key.
    ls_node-expand_child = 'X'.
    APPEND ls_node TO gt_node.
    CLEAR ls_node.

    temp_relat_key = temp_node_key.

    LOOP AT lt_odlist WHERE ogrup = 001.

      temp_node_key = temp_node_key + 1.
      ls_node-isfolder = 'X'.
      ls_node-node_key = temp_node_key.
      ls_node-relatkey = temp_relat_key.

      ls_node-n_image = '@3D@'.
      ls_node-exp_image = '@3D@'.
      ls_node-node_txt1 = lt_odlist-odetp.
      ls_node-node_txt2 = lt_odlist-odeln && lt_odlist-odjhr && lt_odlist-bukrs.

      CASE lt_odlist-odetp.
        WHEN 1.
          ls_node-text = |采购订单 { lt_odlist-odeln }       { lt_odlist-crdat }|.
        WHEN 2.
          ls_node-text = |销售订单 { lt_odlist-odeln }       { lt_odlist-crdat }|.
        WHEN 3.
          ls_node-text = |外向交货 { lt_odlist-odeln }       { lt_odlist-crdat }|.
        WHEN 4.
          ls_node-text = |单据收货 { lt_odlist-odeln }       { lt_odlist-crdat }|.
        WHEN 5.
          ls_node-text = |会计凭证 { lt_odlist-odeln }       { lt_odlist-crdat }|.
        WHEN OTHERS.
      ENDCASE.

      APPEND ls_node TO gt_node.
      CLEAR ls_node.
    ENDLOOP.

   ELSEIF gs_kyth_list IS NOT INITIAL. "add by handyxh 20181115

    READ TABLE lt_odlist with KEY ogrup = 001.
    ls_node-text = |STEP001     { gs_kyth_list-werks }-{ lt_odlist-bukrs } 之间单据|  .
    ls_node-isfolder = 'X'.
    ls_node-node_key = temp_node_key.
    ls_node-expand_child = 'X'.
    APPEND ls_node TO gt_node.
    CLEAR:lt_odlist, ls_node.

     LOOP AT lt_odlist WHERE ogrup = 001.

      temp_node_key = temp_node_key + 1.
      ls_node-isfolder = 'X'.
      ls_node-node_key = temp_node_key.
      ls_node-relatkey = temp_relat_key.

      ls_node-n_image = '@3D@'.
      ls_node-exp_image = '@3D@'.
      ls_node-node_txt1 = lt_odlist-odetp.
      ls_node-node_txt2 = lt_odlist-odeln && lt_odlist-odjhr && lt_odlist-bukrs.

      CASE lt_odlist-odetp.
        WHEN 1.
          ls_node-text = |采购订单 { lt_odlist-odeln }       { lt_odlist-crdat }|.
        WHEN 2.
          ls_node-text = |销售订单 { lt_odlist-odeln }       { lt_odlist-crdat }|.
        WHEN 3.
          ls_node-text = |外向交货 { lt_odlist-odeln }       { lt_odlist-crdat }|.
        WHEN 4.
          ls_node-text = |单据收货 { lt_odlist-odeln }       { lt_odlist-crdat }|.
        WHEN 5.
          ls_node-text = |会计凭证 { lt_odlist-odeln }       { lt_odlist-crdat }|.
        WHEN OTHERS.
      ENDCASE.

      APPEND ls_node TO gt_node.
      CLEAR ls_node.
    ENDLOOP.


  ENDIF.

ENDFORM.
