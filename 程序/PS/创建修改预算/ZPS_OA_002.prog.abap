FUNCTION zps_oa_002.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_INPUT) TYPE  ZZPSYSLX
*"  EXPORTING
*"     VALUE(E_OUTPUT) TYPE  ZSPS_2_OA_001_O
*"  TABLES
*"      I_ITEM STRUCTURE  ZSPS_2_OA_002_I OPTIONAL
*"----------------------------------------------------------------------

  DATA:lt_bpak LIKE TABLE OF bpak WITH HEADER LINE.
  DATA:lt_bpak_kbn0 LIKE TABLE OF bpak WITH HEADER LINE,
       lt_bpak_kbr0 LIKE TABLE OF bpak WITH HEADER LINE.
  DATA:lt_return LIKE TABLE OF bapiret2 WITH HEADER LINE.
  DATA:zobjnr TYPE j_objnr.

  DATA:lt_item TYPE TABLE OF zsps_2_oa_002_i WITH HEADER LINE.

  CLEAR:lt_item,lt_item[].

  lt_item[] = i_item[].

  IF i_input = '0'."预算申请
    LOOP AT lt_item.
      CLEAR:zobjnr.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
        EXPORTING
          input  = lt_item-posid
        IMPORTING
          output = lt_item-posid.


      SELECT SINGLE objnr INTO zobjnr
      FROM prps WHERE posid = lt_item-posid.
      lt_bpak-e_objnr = zobjnr.
      lt_bpak-e_vorga = 'KBUD'.
      lt_bpak-twaer = 'CNY'.
      lt_bpak-wert = lt_item-wtges.
      APPEND lt_bpak.
    ENDLOOP.
    CALL FUNCTION 'KBPP_EXTERN_UPDATE_CO'
      EXPORTING
        i_budget_activity = 'KBUD'
*       I_BUDGET_ACTIV_SUP_RET       = ' '
*       I_COMMIT_DATA     = ' '
        i_delta_amounts   = 'X'
        i_rollup_data     = 'X'
*       I_CHECK_PLAN_DATA = 'X'
*       I_APPLICATION     =
        i_commit_all      = 'X'
* IMPORTING
*       E_ERRORS_FOUND    =
      TABLES
        it_bpak           = lt_bpak[]
        it_return         = lt_return
      EXCEPTIONS
        no_update         = 1
        OTHERS            = 2.
*    CALL FUNCTION 'KBPP_EXTERN_UPDATE_CO'
*      EXPORTING
*        I_BUDGET_ACTIVITY = 'KBUD'
**       I_BUDGET_ACTIV_SUP_RET       = ' '
**       I_COMMIT_DATA     = ' '
*        I_DELTA_AMOUNTS   = ' '
*        I_ROLLUP_DATA     = ' '
**       I_CHECK_PLAN_DATA = 'X'
**       I_APPLICATION     =
*        I_COMMIT_ALL      = 'X'
** IMPORTING
**       E_ERRORS_FOUND    =
*      TABLES
*        IT_BPAK           = LT_BPAK[]
*        IT_RETURN         = LT_RETURN
*      EXCEPTIONS
*        NO_UPDATE         = 1
*        OTHERS            = 2.


  ELSE.
    TYPES:BEGIN OF ly_wtges_total,
            posid TYPE ps_posid,
            wtges TYPE bp_wgt,
          END OF ly_wtges_total.
    DATA:lt_bpge LIKE TABLE OF bpge WITH HEADER LINE.
    DATA:zwtges TYPE bp_wgt.
    DATA:swtges TYPE bp_wgt.
    DATA:zvorga TYPE bp_vorgang.
    DATA:lt_wtges_total TYPE TABLE OF ly_wtges_total.
    FIELD-SYMBOLS:<fs_wtges> TYPE ly_wtges_total.

    CLEAR:lt_bpak,lt_bpak_kbn0[],lt_bpak_kbr0[],lt_wtges_total.



    LOOP AT lt_item.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
        EXPORTING
          input  = lt_item-posid
        IMPORTING
          output = lt_item-posid.
      MODIFY lt_item.
    ENDLOOP.

    SELECT prps~posid,
           prps~objnr,
           stat
    INTO TABLE @DATA(lt_jest)
    FROM prps INNER JOIN jest
      ON prps~objnr = jest~objnr
    FOR ALL ENTRIES IN @lt_item
    WHERE prps~posid = @lt_item-posid.

    SORT lt_jest BY posid.

    LOOP AT lt_jest TRANSPORTING NO FIELDS WHERE stat = 'I0045' OR stat = 'I0076'.

    ENDLOOP.
    IF sy-subrc = 0.
      e_output-msgtyp = 'E'.
      e_output-message = '项目已关闭，不允许更改预算'.
      PERFORM save_log TABLES i_item USING e_output.
      EXIT.
    ENDIF.



    LOOP AT lt_item.
      CLEAR:zobjnr.

      READ TABLE lt_jest INTO DATA(ls_jest) WITH KEY posid = lt_item-posid BINARY SEARCH.
      IF sy-subrc = 0.
        zobjnr = ls_jest-objnr.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
        EXPORTING
          input  = lt_item-posid
        IMPORTING
          output = lt_item-posid.

      SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_bpge
      FROM bpge WHERE objnr = zobjnr AND wrttp = '41'.
      CLEAR zwtges.
      IF lt_bpge[] IS NOT INITIAL.
        LOOP AT lt_bpge.
          zwtges = zwtges + lt_bpge-wtges.
        ENDLOOP.
        IF lt_item-wtges > zwtges."增加预算
          zvorga = 'KBN0'.
        ELSE.
          zvorga = 'KBR0'.
        ENDIF.
        swtges = lt_item-wtges - zwtges.
      ENDIF.
      lt_bpak-e_objnr = zobjnr.
*      lt_bpak-e_vorga = zvorga.
      lt_bpak-e_vorga = 'KBN0'.
      lt_bpak-twaer = 'CNY'.
      lt_bpak-wert = swtges.
      CASE zvorga.
        WHEN 'KBN0'.
          APPEND lt_bpak TO lt_bpak_kbn0.
        WHEN 'KBR0'.
          APPEND lt_bpak TO lt_bpak_kbr0.
        WHEN OTHERS.
      ENDCASE.
      "汇总出第一层预算的增加和减少
      DATA:lv_posid TYPE ps_posid.
      CLEAR :lv_posid.
      SELECT SINGLE poski INTO @DATA(lv_poski) FROM prps WHERE posid = @lt_item-posid.

      SPLIT lv_poski AT '-' INTO TABLE DATA(lt_split_tab).
      LOOP AT lt_split_tab INTO DATA(ls_split) TO 2.
        CONCATENATE lv_posid '-' ls_split INTO lv_posid.
      ENDLOOP.
      CLEAR:lt_split_tab.
      SHIFT lv_posid LEFT DELETING LEADING '-'.
      IF sy-subrc = 0.
        CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
          EXPORTING
            input  = lv_posid
          IMPORTING
            output = lv_posid.
        READ TABLE lt_wtges_total ASSIGNING <fs_wtges> WITH KEY posid = lv_posid.
        IF sy-subrc = 0.
          <fs_wtges>-wtges = <fs_wtges>-wtges + lt_item-wtges - zwtges.
        ELSE.
          APPEND INITIAL LINE TO lt_wtges_total ASSIGNING <fs_wtges>.
          <fs_wtges>-posid = lv_posid.
          <fs_wtges>-wtges = lt_item-wtges - zwtges.
        ENDIF.
      ENDIF.

    ENDLOOP.
    "根据上面的计算结果计算第一层预算应该是多少
    IF lt_wtges_total IS NOT INITIAL.
      LOOP AT lt_wtges_total ASSIGNING <fs_wtges> .

        SELECT SINGLE objnr INTO zobjnr
        FROM prps WHERE posid = <fs_wtges>-posid.
*        SELECT *
*        INTO CORRESPONDING FIELDS OF TABLE lt_bpge
*        FROM bpge WHERE objnr = zobjnr AND wrttp = '41'.
*        CLEAR zwtges.
*        IF lt_bpge[] IS NOT INITIAL.
*
*          LOOP AT lt_bpge.
*            zwtges = zwtges + lt_bpge-wtges.
*          ENDLOOP.
*        ENDIF.
        lt_bpak-e_objnr = zobjnr.
*      lt_bpak-e_vorga = zvorga.
        lt_bpak-e_vorga = 'KBN0'.
        lt_bpak-twaer = 'CNY'.
        lt_bpak-wert = <fs_wtges>-wtges.
        IF <fs_wtges>-wtges < 0.
          zvorga = 'KBR0'.
        ELSE.
          zvorga = 'KBN0'.
        ENDIF.
        CASE zvorga.
          WHEN 'KBN0'.
            APPEND lt_bpak TO lt_bpak_kbn0.
          WHEN 'KBR0'.
            APPEND lt_bpak TO lt_bpak_kbr0.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
    ENDIF.

  ENDIF.

  SORT lt_bpak_kbr0 BY e_objnr DESCENDING.
  SORT lt_bpak_kbn0 BY e_objnr.

  IF lt_bpak_kbn0[] IS NOT INITIAL AND lt_bpak_kbr0[] IS NOT INITIAL.
      e_output-msgtyp = 'E'.
      e_output-message = '请保持预算调整方向一致，仅能全部WBS预算增加/全部WBS预算减少'.
      PERFORM save_log TABLES i_item USING e_output.
      EXIT.
  ENDIF.

  IF lt_bpak_kbn0[] IS NOT INITIAL.
    CALL FUNCTION 'KBPP_EXTERN_UPDATE_CO'
      EXPORTING
        i_budget_activity = 'KBN0'
*       I_BUDGET_ACTIV_SUP_RET       = ' '
*       I_COMMIT_DATA     = ' '
        i_delta_amounts   = 'X'
        i_rollup_data     = ' '
*       I_CHECK_PLAN_DATA = 'X'
*       I_APPLICATION     =
        i_commit_all      = 'X'
* IMPORTING
*       E_ERRORS_FOUND    =
      TABLES
        it_bpak           = lt_bpak_kbn0[]
        it_return         = lt_return
      EXCEPTIONS
        no_update         = 1
        OTHERS            = 2.
    LOOP AT lt_return WHERE type = 'E' OR type = 'A'.

    ENDLOOP.
*  READ TABLE lt_return WITH KEY type = 'E' .
    IF sy-subrc = 0 .
      e_output-msgtyp = 'E'.
      LOOP AT lt_return WHERE type = 'E' OR type = 'A'.
        e_output-message = |{ e_output-message }{ lt_return-message }|.
      ENDLOOP.
      PERFORM save_log TABLES i_item USING e_output.
      EXIT.
    ENDIF.

  ENDIF.

  IF lt_bpak_kbr0[] IS NOT INITIAL.
    CALL FUNCTION 'KBPP_EXTERN_UPDATE_CO'
      EXPORTING
        i_budget_activity = 'KBR0'
*       I_BUDGET_ACTIV_SUP_RET       = ' '
*       I_COMMIT_DATA     = ' '
        i_delta_amounts   = 'X'
        i_rollup_data     = ' '
*       I_CHECK_PLAN_DATA = 'X'
*       I_APPLICATION     =
        i_commit_all      = 'X'
* IMPORTING
*       E_ERRORS_FOUND    =
      TABLES
        it_bpak           = lt_bpak_kbr0[]
        it_return         = lt_return
      EXCEPTIONS
        no_update         = 1
        OTHERS            = 2.
  ENDIF..



  LOOP AT lt_return WHERE type = 'E' OR type = 'A'.

  ENDLOOP.
*  READ TABLE lt_return WITH KEY type = 'E' .
  IF sy-subrc = 0 .
    e_output-msgtyp = 'E'.
    LOOP AT lt_return WHERE type = 'E' OR type = 'A'.
      e_output-message = |{ e_output-message }{ lt_return-message }|.
    ENDLOOP.
  ELSE.
    e_output-msgtyp = 'S'.
    e_output-message = '成功'.
  ENDIF.

  PERFORM save_log TABLES i_item USING e_output.





ENDFUNCTION.
*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_log TABLES i_item STRUCTURE zsps_2_oa_002_i USING e_output TYPE zsps_2_oa_001_o.
  DATA:lt_ztoa_budget_log TYPE TABLE OF ztps_budget_log WITH HEADER LINE,
       wa_ztoa_budget_log TYPE ztps_budget_log.
  wa_ztoa_budget_log-zernam = sy-uname .
  wa_ztoa_budget_log-zerdat = sy-datum .
  wa_ztoa_budget_log-zertim = sy-uzeit .
  wa_ztoa_budget_log-msgtyp = e_output-msgtyp.
  wa_ztoa_budget_log-message = e_output-message.

  MOVE-CORRESPONDING i_item[] TO lt_ztoa_budget_log[].
  LOOP AT lt_ztoa_budget_log.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '33'
        object                  = 'ZLOGID'
      IMPORTING
        number                  = lt_ztoa_budget_log-logid
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    lt_ztoa_budget_log-zernam = wa_ztoa_budget_log-zernam.
    lt_ztoa_budget_log-zerdat = wa_ztoa_budget_log-zerdat.
    lt_ztoa_budget_log-zertim = wa_ztoa_budget_log-zertim.
    lt_ztoa_budget_log-msgtyp = wa_ztoa_budget_log-msgtyp.
    lt_ztoa_budget_log-message = wa_ztoa_budget_log-message.
    MODIFY lt_ztoa_budget_log.
  ENDLOOP.


  MODIFY ztps_budget_log FROM TABLE lt_ztoa_budget_log.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.
ENDFORM.