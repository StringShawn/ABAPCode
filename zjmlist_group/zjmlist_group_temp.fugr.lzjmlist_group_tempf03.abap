*----------------------------------------------------------------------*
***INCLUDE LZJMLIST_GROUPF03.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  POP_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pop_selection_screen .

  CASE g_tb_tab-subscreen.
    WHEN '0201'.
      CALL SELECTION-SCREEN 0301 STARTING AT 40 1.
    WHEN '0202'.
      CALL SELECTION-SCREEN 0302 STARTING AT 40 1.
    WHEN '0203'.
      CALL SELECTION-SCREEN 0303 STARTING AT 40 1.
    WHEN '0205'.
      CALL SELECTION-SCREEN 0305 STARTING AT 40 1.
    WHEN '0206'.
      CALL SELECTION-SCREEN 0306 STARTING AT 40 1.
    WHEN '0207'.
      CALL SELECTION-SCREEN 0307 STARTING AT 40 1.
    WHEN '0208'.
      CALL SELECTION-SCREEN 0308 STARTING AT 40 1.
    WHEN '0209'.
      CALL SELECTION-SCREEN 0309 STARTING AT 40 1.
    WHEN '0210'.
      CALL SELECTION-SCREEN 0310 STARTING AT 40 1.
    WHEN '0211'.
      CALL SELECTION-SCREEN 0311 STARTING AT 40 1.
    WHEN '0212'.
      CALL SELECTION-SCREEN 0312 STARTING AT 40 1.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  CASE g_tb_tab-subscreen.
    WHEN '0201'.
      PERFORM get_spfh_data.
    WHEN '0202'.
      PERFORM get_fspfh_data.
    WHEN '0203'.
      PERFORM get_th_data.
    WHEN '0204'.
    WHEN '0205'.
      PERFORM get_spyh_data.
    WHEN '0206'.
      PERFORM get_fspyh_data.
    WHEN '0207'.
      PERFORM get_belnr_data.
    WHEN '0208' OR '0209'.
      PERFORM get_fh_head USING g_tb_tab-subscreen.

    WHEN '0210'.
      PERFORM get_ml_data.
    WHEN '0211'.
      PERFORM get_kyth_data.
    WHEN '0212'.
      PERFORM query_kyth_data.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_SPYH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_spfh_data .

  DATA lt_matnr LIKE TABLE OF zls_matnr WITH HEADER LINE.
  DATA lt_werks LIKE TABLE OF zls_werks WITH HEADER LINE.

  SELECT DISTINCT b~bhnuc INTO TABLE @DATA(lt_bhnuc)
    FROM zsd_jiamengfahuo AS b
    INNER JOIN zsd_jmfh_h AS a ON a~bhnuc = b~bhnuc
    WHERE a~mtype = '01'
      AND a~bhnuc IN @s_bhnuc1
      AND a~werks IN @s_werks1
      AND b~matnr IN @s_matnr1
      AND b~z_jtm IN @s_zjtm1
      AND a~crdat IN @s_crdat1
      AND a~crnam IN @s_crusr1
      AND a~jmstu IN @s_jmstu1
      AND b~charg IN @s_charg1
      AND a~fhdat IN @s_fhdat1
      AND a~fhnam IN @s_fhnam1.

  IF lt_bhnuc[] IS NOT INITIAL.

    SELECT a~bhnuc, b~bhitem, a~werks, a~fwerk, a~flgor, b~matnr,
           b~charg, b~z_jtm, a~crdat, a~crtim, a~crnam, b~gujia,a~fhdat,a~fhtim,a~fhnam,
           b~kwmeng, b~z_sgj, b~zzpbz, b~yhdln, b~yhdlp, b~lgorf, a~stype, a~jmstu
      INTO TABLE @DATA(lt_temp_jmfh)
      FROM zsd_jmfh_h AS a
      INNER JOIN zsd_jiamengfahuo AS b ON a~bhnuc = b~bhnuc
      FOR ALL ENTRIES IN @lt_bhnuc
      WHERE a~bhnuc = @lt_bhnuc-bhnuc
        AND a~mtype = '01'.

    SORT lt_temp_jmfh BY bhnuc DESCENDING bhitem.
  ENDIF.

  REFRESH gt_spfh_data.
  LOOP AT lt_temp_jmfh INTO DATA(ls_temp_jmfh).

    gt_spfh_data = CORRESPONDING #( ls_temp_jmfh ).

    gt_spfh_data-db_amount = ls_temp_jmfh-gujia.
    gt_spfh_data-menge = ls_temp_jmfh-kwmeng.
    gt_spfh_data-all_amount = ls_temp_jmfh-gujia * ls_temp_jmfh-kwmeng.

    gt_spfh_data-sg_amount = ls_temp_jmfh-z_sgj.
    gt_spfh_data-free_flag = ls_temp_jmfh-zzpbz.

    CASE ls_temp_jmfh-stype.
      WHEN '1'.
        gt_spfh_data-stype_txt = '补货'.
      WHEN '2'.
        gt_spfh_data-stype_txt = '要货'.
      WHEN '3'.
      WHEN '4'.
        gt_spfh_data-stype_txt = '更换主体'.
      WHEN '6'.
        gt_spfh_data-stype_txt = '加盟调拨直营'.
      WHEN OTHERS.
    ENDCASE.

    CASE ls_temp_jmfh-jmstu.
      WHEN '1'.
        gt_spfh_data-jmstu_txt = '@5B@ 未发货'.
      WHEN '2'.
        gt_spfh_data-jmstu_txt = '@5C@ 发货未完成'.
      WHEN '3'.
        gt_spfh_data-jmstu_txt = '@06@ 已发货'.
      WHEN OTHERS.
    ENDCASE.

    IF gt_spfh_data-free_flag = 'X'.
      gt_spfh_data-free_txt = '是'.
      gt_spfh_data-db_amount = 0.
      gt_spfh_data-all_amount = 0.
    ELSE.
      gt_spfh_data-free_txt = '否'.
    ENDIF.

    READ TABLE lt_matnr WITH KEY matnr = gt_spfh_data-matnr.
    IF sy-subrc <> 0.

      SELECT SINGLE matnr maktx INTO lt_matnr
        FROM makt WHERE matnr = gt_spfh_data-matnr AND spras = sy-langu.

      APPEND lt_matnr.
    ENDIF.
    gt_spfh_data-maktx = lt_matnr-maktx.

    READ TABLE lt_werks WITH KEY werks = gt_spfh_data-werks.
    IF sy-subrc <> 0.
      SELECT SINGLE werks name1 AS werks_name INTO lt_werks
        FROM t001w WHERE werks = gt_spfh_data-werks.

      APPEND lt_werks.
    ENDIF.
    gt_spfh_data-werks_name = lt_werks-werks_name.

    APPEND gt_spfh_data.
    CLEAR gt_spfh_data.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_FSPYH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fspfh_data .

  DATA: BEGIN OF ls_matnr,
          matnr TYPE mara-matnr,
          maktx TYPE makt-maktx,
          vtext TYPE t179t-vtext,
        END OF ls_matnr.
  DATA: lt_matnr LIKE TABLE OF ls_matnr WITH HEADER LINE.

*  DATA lt_matnr LIKE TABLE OF zls_matnr WITH HEADER LINE.
  DATA lt_werks LIKE TABLE OF zls_werks WITH HEADER LINE.

  SELECT DISTINCT b~bhnuc INTO TABLE @DATA(lt_bhnuc)
    FROM zsd_jiamengfahuo AS b
    INNER JOIN zsd_jmfh_h AS a ON a~bhnuc = b~bhnuc
    WHERE a~mtype = '02'
      AND a~bhnuc IN @s_bhnuc2
      AND a~werks IN @s_werks2
      AND b~matnr IN @s_matnr2
      AND a~crdat IN @s_crdat2
      AND a~crnam IN @s_crusr2
      AND a~jmstu IN @s_jmstu2
      AND b~charg IN @s_charg2
      AND a~fhdat IN @s_fhdat2
      AND a~fhnam IN @s_fhnam2.

  IF lt_bhnuc[] IS NOT INITIAL.

    SELECT a~bhnuc, b~bhitem, a~werks, a~fwerk, a~flgor, b~matnr,
           b~charg, b~z_jtm, a~crdat, a~crtim, a~crnam, b~amt,a~fhdat,a~fhtim,a~fhnam,
           b~kwmeng, b~zzpbz, b~yhdln, b~yhdlp, b~lgorf, a~stype, a~jmstu, c~odeln
      INTO TABLE @DATA(lt_temp_jmfh)
      FROM zsd_jmfh_h AS a
      INNER JOIN zsd_jiamengfahuo AS b ON a~bhnuc = b~bhnuc
      LEFT JOIN ztrade_odlist AS c ON a~bhnuc = c~bhnuc AND c~step = 002 AND c~ogrup = 001
      FOR ALL ENTRIES IN @lt_bhnuc
      WHERE a~bhnuc = @lt_bhnuc-bhnuc
        AND a~mtype = '02'.

    SORT lt_temp_jmfh BY bhnuc DESCENDING bhitem.
  ENDIF.

  IF lt_temp_jmfh IS NOT INITIAL.
    SELECT *
      INTO TABLE @DATA(lt_zhqbh)
      FROM zhqbh
      FOR ALL ENTRIES IN @lt_temp_jmfh
      WHERE matnr = @lt_temp_jmfh-matnr.
  ENDIF.

  REFRESH gt_fspfh_data.
  LOOP AT lt_temp_jmfh INTO DATA(ls_temp_jmfh).

    gt_fspfh_data = CORRESPONDING #( ls_temp_jmfh ).

    gt_fspfh_data-db_amount = ls_temp_jmfh-amt.
    gt_fspfh_data-menge = ls_temp_jmfh-kwmeng.
    gt_fspfh_data-all_amount = ls_temp_jmfh-amt * ls_temp_jmfh-kwmeng.

    gt_fspfh_data-free_flag = ls_temp_jmfh-zzpbz.

    CASE ls_temp_jmfh-stype.
      WHEN '1'.
        gt_fspfh_data-stype_txt = '补货'.
      WHEN '2'.
        gt_fspfh_data-stype_txt = '要货'.
      WHEN '3'.
        gt_fspfh_data-stype_txt = 'OA'.
      WHEN OTHERS.
    ENDCASE.

    CASE ls_temp_jmfh-jmstu.
      WHEN '1'.
        gt_fspfh_data-jmstu_txt = '@5B@ 未发货'.
      WHEN '2'.
        gt_fspfh_data-jmstu_txt = '@5C@ 发货未完成'.
      WHEN '3'.
        gt_fspfh_data-jmstu_txt = '@06@ 已发货'.
      WHEN OTHERS.
    ENDCASE.

    IF gt_fspfh_data-free_flag = 'X'.
      gt_fspfh_data-free_txt = '是'.
      gt_fspfh_data-db_amount = 0.
      gt_fspfh_data-all_amount = 0.
    ELSE.
      gt_fspfh_data-free_txt = '否'.
    ENDIF.

    READ TABLE lt_matnr WITH KEY matnr = gt_fspfh_data-matnr.
    IF sy-subrc <> 0.

      SELECT SINGLE makt~matnr
                    makt~maktx
                    t179t~vtext
        INTO lt_matnr
        FROM makt INNER JOIN mara ON makt~matnr = mara~matnr
                  INNER JOIN t179t ON mara~prdha = t179t~prodh
        WHERE makt~matnr = gt_fspfh_data-matnr
          AND makt~spras = sy-langu
          AND t179t~spras = sy-langu.

      APPEND lt_matnr.
    ENDIF.
    gt_fspfh_data-maktx = lt_matnr-maktx.
    gt_fspfh_data-vtext = lt_matnr-vtext.

    READ TABLE lt_werks WITH KEY werks = gt_fspfh_data-werks.
    IF sy-subrc <> 0.
      SELECT SINGLE werks name1 AS werks_name INTO lt_werks
        FROM t001w WHERE werks = gt_fspfh_data-werks.

      APPEND lt_werks.
    ENDIF.
    gt_fspfh_data-werks_name = lt_werks-werks_name.

    READ TABLE lt_zhqbh INTO DATA(ls_zhqbh) WITH KEY matnr = gt_fspfh_data-matnr.
    IF sy-subrc = 0.
      gt_fspfh_data-zmax_menge = ls_zhqbh-zmax_menge.
      gt_fspfh_data-zmin_menge = ls_zhqbh-zmin_menge.
      gt_fspfh_data-zmin_pkg_menge = ls_zhqbh-zmin_pkg_menge.
      gt_fspfh_data-zpp_scope = ls_zhqbh-zpp_scope.
    ENDIF.

    APPEND gt_fspfh_data.
    CLEAR gt_fspfh_data.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_SPFH_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fh_head USING screen TYPE sy-dynnr.

  IF screen = '0208'.
    SELECT a~werks,a~bhnuc,a~fwerk,a~all_amount,a~crdat,a~crtim,a~crnam,a~fhdat,a~fhtim,a~fhnam,
           a~stype,b~name1 AS werks_name,a~jmstu, a~linkn
     INTO CORRESPONDING FIELDS OF TABLE @gt_fh_head
     FROM zsd_jmfh_h AS a
     INNER JOIN t001w AS b ON a~werks = b~werks
     WHERE a~bhnuc IN @s_bhnuc8
       AND a~werks IN @s_werks8
       AND a~crdat IN @s_crdat8
       AND a~fhdat IN @s_fhdat8
       AND a~crnam IN @s_crnam8
       AND a~fhnam IN @s_fhnam8
       AND a~mtype = '01'
       AND a~jmstu IN @s_jmstu8
       ORDER BY a~bhnuc DESCENDING.

  ELSEIF screen = '0209'.

    SELECT a~werks,a~bhnuc,a~fwerk,a~all_amount,a~crdat,a~crtim,a~crnam,a~fhdat,a~fhtim,a~fhnam,
           a~stype,b~name1 AS werks_name,a~jmstu, a~linkn
     INTO CORRESPONDING FIELDS OF TABLE @gt_fh_head
     FROM zsd_jmfh_h AS a
     INNER JOIN t001w AS b ON a~werks = b~werks
     WHERE a~bhnuc IN @s_bhnuc9
       AND a~werks IN @s_werks9
       AND a~crdat IN @s_crdat9
       AND a~fhdat IN @s_fhdat9
       AND a~crnam IN @s_crnam9
       AND a~fhnam IN @s_fhnam9
       AND a~mtype = '02'
       AND a~jmstu IN @s_jmstu9
       ORDER BY a~bhnuc DESCENDING.
  ENDIF.

  LOOP AT gt_fh_head.
    CASE gt_fh_head-stype.
      WHEN '1'.
        gt_fh_head-stype_txt = '补货'.
      WHEN '2'.
        gt_fh_head-stype_txt = '要货'.
      WHEN '3'.
        gt_fh_head-stype_txt = 'OA'.
      WHEN OTHERS.
    ENDCASE.

    CASE gt_fh_head-jmstu.
      WHEN '1'.
        gt_fh_head-jmstu_txt = '@5B@ 未发货'.
      WHEN '2'.
        gt_fh_head-jmstu_txt = '@5C@ 发货未完成'.
      WHEN '3'.
        gt_fh_head-jmstu_txt = '@06@ 已发货'.
      WHEN OTHERS.
    ENDCASE.

    MODIFY gt_fh_head.
  ENDLOOP.

  IF screen = '0208'.
    gt_spfh_head[] = gt_fh_head[].
  ELSEIF screen = '0209'.
    gt_fspfh_head[] = gt_fh_head[].
  ENDIF.
  REFRESH gt_fh_head.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_FSPYH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_spyh_data .

  DATA lt_matnr LIKE TABLE OF zls_matnr WITH HEADER LINE.
  DATA lt_werks LIKE TABLE OF zls_werks WITH HEADER LINE.

  SELECT b~send_status AS status, a~werks, a~kunnr, a~yhdln,b~yhdlp,b~matnr,
         b~sg_amount, b~amtch, a~crdat, a~crtim, a~spdat, a~sptim, a~rqdat,
         b~charg, b~z_jtm, b~db_amount,a~yhdty, b~need_menge AS menge
    INTO CORRESPONDING FIELDS OF TABLE @gt_spyh_data
    FROM zmm_spyh_h AS a
    INNER JOIN zmm_spyh_t AS b ON a~yhdln = b~yhdln
    WHERE a~yhdln IN @s_yhdln5
      AND a~werks IN @s_werks5
      AND b~charg IN @s_charg5
      AND b~z_jtm IN @s_zjtm5
      AND a~crdat IN @s_crdat5
      AND a~spdat IN @s_spdat5
      AND b~send_status IN @s_statu5
  ORDER BY a~yhdln DESCENDING, b~yhdlp.

  LOOP AT gt_spyh_data.

    CASE gt_spyh_data-status.
      WHEN 'A'.
        gt_spyh_data-status_txt = '@5B@ 待审批'.
      WHEN 'B'.
        gt_spyh_data-status_txt = '@5B@ 待拣货'.
      WHEN 'C'.
        gt_spyh_data-status_txt = '@5D@ 已拣配'.
      WHEN 'D'.
        gt_spyh_data-status_txt = '@5H@ 待发货'.
      WHEN 'F'.
        gt_spyh_data-status_txt = '@06@ 已发货'.
      WHEN 'X'.
        gt_spyh_data-status_txt = '@00@ 已删除'.
      WHEN 'R'.
        gt_spyh_data-status_txt = '@00@ 已退回'.
      WHEN OTHERS.
    ENDCASE.

    CASE gt_spyh_data-yhdty.
      WHEN '1'.
        gt_spyh_data-yhdty_txt = '是'.
      WHEN '2'.
        gt_spyh_data-yhdty_txt = '否'.
      WHEN OTHERS.
    ENDCASE.

    IF gt_spyh_data-amtch = 'X'.
      gt_spyh_data-amtch_txt = '是'.
    ELSE.
      gt_spyh_data-amtch_txt = '否'.
    ENDIF.

    READ TABLE lt_matnr WITH KEY matnr = gt_spyh_data-matnr.
    IF sy-subrc <> 0.

      SELECT SINGLE matnr maktx INTO lt_matnr
        FROM makt WHERE matnr = gt_spyh_data-matnr AND spras = sy-langu.

      APPEND lt_matnr.
    ENDIF.
    gt_spyh_data-maktx = lt_matnr-maktx.

    READ TABLE lt_werks WITH KEY werks = gt_spyh_data-werks.
    IF sy-subrc <> 0.
      SELECT SINGLE werks name1 AS werks_name INTO lt_werks
        FROM t001w WHERE werks = gt_spyh_data-werks.

      APPEND lt_werks.
    ENDIF.
    gt_spyh_data-werks_name = lt_werks-werks_name.

    MODIFY gt_spyh_data.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_FSPYH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fspyh_data .

  DATA lt_matnr LIKE TABLE OF zls_matnr WITH HEADER LINE.
  DATA lt_werks LIKE TABLE OF zls_werks WITH HEADER LINE.

  SELECT b~send_status AS status, a~werks, a~kunnr, a~yhdln,b~yhdlp,b~matnr,
         a~crdat, a~crtim, a~spdat, a~sptim, a~rqdat,
         b~charg, b~db_amount,b~need_menge AS menge
    INTO CORRESPONDING FIELDS OF TABLE @gt_fspyh_data
    FROM zmm_fspyh_h AS a
    INNER JOIN zmm_fspyh_t AS b ON a~yhdln = b~yhdln
    WHERE a~yhdln IN @s_yhdln6
      AND a~werks IN @s_werks6
      AND b~charg IN @s_charg6
      AND a~crdat IN @s_crdat6
      AND a~spdat IN @s_spdat6
    ORDER BY a~yhdln DESCENDING, b~yhdlp.

  LOOP AT gt_fspyh_data.

    CASE gt_fspyh_data-status.
      WHEN 'A'.
        gt_fspyh_data-status_txt = '@5B@ 待审批'.
      WHEN 'B'.
        gt_fspyh_data-status_txt = '@5B@ 待拣货'.
      WHEN 'C'.
        gt_fspyh_data-status_txt = '@5D@ 已拣配'.
      WHEN 'D'.
        gt_fspyh_data-status_txt = '@5H@ 待发货'.
      WHEN 'F'.
        gt_fspyh_data-status_txt = '@06@ 已发货'.
      WHEN 'X'.
        gt_fspyh_data-status_txt = '@00@ 已删除'.
      WHEN OTHERS.
    ENDCASE.


    READ TABLE lt_matnr WITH KEY matnr = gt_fspyh_data-matnr.
    IF sy-subrc <> 0.

      SELECT SINGLE matnr maktx INTO lt_matnr
        FROM makt WHERE matnr = gt_fspyh_data-matnr AND spras = sy-langu.

      APPEND lt_matnr.
    ENDIF.
    gt_fspyh_data-maktx = lt_matnr-maktx.

    READ TABLE lt_werks WITH KEY werks = gt_fspyh_data-werks.
    IF sy-subrc <> 0.
      SELECT SINGLE werks name1 AS werks_name INTO lt_werks
        FROM t001w WHERE werks = gt_fspyh_data-werks.

      APPEND lt_werks.
    ENDIF.
    gt_fspyh_data-werks_name = lt_werks-werks_name.

    MODIFY gt_fspyh_data.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHANGE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_OBJECT  text
*----------------------------------------------------------------------*
FORM get_th_data.

  DATA lt_werks LIKE TABLE OF zls_werks WITH HEADER LINE.
  DATA lt_matnr LIKE TABLE OF zls_matnr WITH HEADER LINE.
  DATA lt_dd07t LIKE TABLE OF dd07t WITH HEADER LINE.
  DATA mlper TYPE p DECIMALS 2.

  SELECT a~ebeln b~ebelp a~pathn a~zthlx a~werks c~matnr c~bwtar AS charg a~fwerk
         c~menge b~hispr b~zkjqt AS redpr b~amflg a~crdat a~crtim b~hislg a~thstu
      INTO CORRESPONDING FIELDS OF TABLE gt_th_data
      FROM zsd_jmth_h AS a
      INNER JOIN zsd_jmth_fj AS b ON a~ebeln = b~ebeln AND a~pathn = b~pathn
      INNER JOIN ekpo AS c ON b~ebeln = c~ebeln AND c~ebelp = b~ebelp
      WHERE a~ebeln IN s_ebeln3
        AND a~werks IN s_werks3
        AND a~crdat IN s_crdat3
        AND a~thstu IN s_thstu3
        AND a~thdat IN s_thdat3
      ORDER BY a~ebeln DESCENDING a~pathn b~ebelp.

  SELECT * INTO TABLE lt_dd07t FROM dd07t WHERE domname = 'ZTHLX' AND ddlanguage = sy-langu.

  IF gt_th_data[] IS NOT INITIAL.
    SELECT charg, z_jtm, z_pp, z_zpp, z_zcb, z_sgj INTO TABLE @DATA(lt_charg)
      FROM zmm_dpsxb FOR ALL ENTRIES IN @gt_th_data
      WHERE charg = @gt_th_data-charg.
  ENDIF.

  SORT lt_charg BY charg.

  LOOP AT gt_th_data.

    READ TABLE lt_charg INTO DATA(ls_charg) WITH KEY charg = gt_th_data-charg BINARY SEARCH.
    IF sy-subrc = 0.
      gt_th_data-z_jtm = ls_charg-z_jtm.
      gt_th_data-z_pp = ls_charg-z_pp.
      gt_th_data-z_zpp = ls_charg-z_zpp.
      gt_th_data-z_zcb = ls_charg-z_zcb.
      gt_th_data-z_sgj = ls_charg-z_sgj.
    ENDIF.

    READ TABLE lt_dd07t WITH KEY domvalue_l = gt_th_data-zthlx.
    IF sy-subrc = 0.
      gt_th_data-zthlx_txt = lt_dd07t-ddtext.
    ENDIF.

    READ TABLE lt_werks WITH KEY werks = gt_th_data-werks.
    IF sy-subrc <> 0.
      CLEAR lt_werks.
      SELECT SINGLE werks name1 AS werks_name INTO lt_werks FROM t001w WHERE werks = gt_th_data-werks.
      APPEND lt_werks.
    ENDIF.
    gt_th_data-werks_name = lt_werks-werks_name.

    READ TABLE lt_matnr WITH KEY matnr = gt_th_data-matnr.
    IF sy-subrc <> 0.
      CLEAR lt_matnr.
      SELECT SINGLE matnr maktx INTO lt_matnr FROM makt WHERE matnr = gt_th_data-matnr AND spras = sy-langu.
      APPEND lt_matnr.
    ENDIF.
    gt_th_data-maktx = lt_matnr-maktx.

    CASE gt_th_data-amflg.
      WHEN '1'.
        gt_th_data-amflg_txt = 'SAP'.
      WHEN '2'.
        gt_th_data-amflg_txt = '历史'.
      WHEN 'X'.
        gt_th_data-amflg_txt = '未找到'.
      WHEN OTHERS.
        gt_th_data-amflg_txt = '未知'.
    ENDCASE.

    IF gt_th_data-hislg = 'X'.
      gt_th_data-hislg_txt = '@01@'.
    ELSE.
      gt_th_data-hislg_txt = '@02@'.
    ENDIF.

    CASE gt_th_data-thstu.
      WHEN '1'.
        gt_th_data-thstu_txt = '@5B@ 待扫码'.
      WHEN '2'.
        gt_th_data-thstu_txt = '@5H@ 待退货'.
      WHEN '3'.
        gt_th_data-thstu_txt = '@5D@ 退货中'.
      WHEN '4'.
        gt_th_data-thstu_txt = '@2K@ 待退款'.
      WHEN '5'.
        gt_th_data-thstu_txt = '@06@ 已退款'.
      WHEN OTHERS.
    ENDCASE.

    IF gt_th_data-zthlx = 'A'.
      gt_th_data-lespr = gt_th_data-hispr * '0.05' * gt_th_data-menge.
      IF gt_th_data-lespr > 1000.
        gt_th_data-lespr = 1000.
      ENDIF.
    ENDIF.

    IF gt_th_data-hispr <> 0.
      mlper = ( gt_th_data-hispr - gt_th_data-z_zcb ) / gt_th_data-hispr * 100.
      gt_th_data-mlper = mlper && '%'.
    ENDIF.

    MODIFY gt_th_data.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_OBJECT  text
*----------------------------------------------------------------------*
FORM get_ml_data.

  DATA lt_werks LIKE TABLE OF zls_werks WITH HEADER LINE.

  DATA:BEGIN OF ls_vkbur,
         werks     TYPE werks_d,
         vkbur     TYPE vkbur,
         vkbur_txt TYPE bezei,
       END OF ls_vkbur,
       lt_vkbur LIKE TABLE OF ls_vkbur WITH HEADER LINE.

  DATA per TYPE p DECIMALS 2.

  IF s_pp10[] IS NOT INITIAL OR s_zpp10 IS NOT INITIAL.

    SELECT b~werks, a~gujia, a~kwmeng, a~zzpbz, d~verpr INTO TABLE @DATA(lt_ml_data)
      FROM zsd_jiamengfahuo AS a
      INNER JOIN zsd_jmfh_h AS b ON a~bhnuc = b~bhnuc
      INNER JOIN zmm_dpsxb AS c ON a~charg = c~charg
      INNER JOIN mbew AS d ON a~charg = d~bwtar AND a~matnr = d~matnr AND d~bwkey = b~fwerk
      WHERE b~jmstu = '1'
        AND b~werks IN @s_werk10
        AND b~bhnuc IN @s_bhnu10
        AND b~mtype = '01'
        AND c~z_pp IN @s_pp10
        AND c~z_zpp IN @s_zpp10.

  ELSE.

    SELECT b~werks, a~gujia, a~kwmeng, a~zzpbz, d~verpr INTO TABLE @lt_ml_data
      FROM zsd_jiamengfahuo AS a
      INNER JOIN zsd_jmfh_h AS b ON a~bhnuc = b~bhnuc
      INNER JOIN mbew AS d ON a~charg = d~bwtar AND a~matnr = d~matnr AND d~bwkey = b~fwerk
      WHERE b~jmstu = '1'
        AND b~werks IN @s_werk10
        AND b~bhnuc IN @s_bhnu10
        AND b~mtype = '01'.

  ENDIF.

  REFRESH gt_fh_ml.

  LOOP AT lt_ml_data INTO DATA(ls_ml_data).

    CASE p_sum10.
      WHEN 1.
        gt_fh_ml-db_amount = ls_ml_data-gujia.
        gt_fh_ml-werks = ls_ml_data-werks.

        READ TABLE lt_werks WITH KEY werks = ls_ml_data-werks.
        IF sy-subrc <> 0.
          SELECT SINGLE werks name1 AS werks_name INTO lt_werks FROM t001w WHERE werks = ls_ml_data-werks.
          APPEND lt_werks.
        ENDIF.
        gt_fh_ml-werks_name = lt_werks-werks_name.

        READ TABLE lt_vkbur WITH KEY werks = ls_ml_data-werks.
        IF sy-subrc <> 0.
          SELECT SINGLE a~locnr AS werks a~vkbur_wrk AS vkbur b~bezei AS vkbur_txt INTO lt_vkbur
            FROM wrf1 AS a
            INNER JOIN tvkbt AS b ON a~vkbur_wrk = b~vkbur AND b~spras = sy-langu
            WHERE a~locnr = ls_ml_data-werks.
          APPEND lt_vkbur.
        ENDIF.
        gt_fh_ml-vkbur = lt_vkbur-vkbur.
        gt_fh_ml-vkbur_txt = lt_vkbur-vkbur_txt.
      WHEN 2.
        READ TABLE lt_vkbur WITH KEY werks = ls_ml_data-werks.
        IF sy-subrc <> 0.
          SELECT SINGLE a~locnr AS werks a~vkbur_wrk AS vkbur b~bezei AS vkbur_txt INTO lt_vkbur
            FROM wrf1 AS a
            INNER JOIN tvkbt AS b ON a~vkbur_wrk = b~vkbur AND b~spras = sy-langu
            WHERE a~locnr = ls_ml_data-werks.
          APPEND lt_vkbur.
        ENDIF.
        gt_fh_ml-vkbur = lt_vkbur-vkbur.
        gt_fh_ml-vkbur_txt = lt_vkbur-vkbur_txt.
      WHEN 3.
      WHEN OTHERS.
    ENDCASE.

    IF ls_ml_data-zzpbz <> 'X'.
      gt_fh_ml-db_amount = ls_ml_data-gujia * ls_ml_data-kwmeng.
      gt_fh_ml-vp_amount = ls_ml_data-verpr * '1.16'.
    ENDIF.

    COLLECT gt_fh_ml.

    CLEAR gt_fh_ml.
  ENDLOOP.

  LOOP AT gt_fh_ml.
    IF gt_fh_ml-db_amount > 0.
      per = ( gt_fh_ml-db_amount - gt_fh_ml-vp_amount ) / gt_fh_ml-db_amount * 100.
    ELSE.
      per = 0.
    ENDIF.
    gt_fh_ml-mlper = per && '%'.
    MODIFY gt_fh_ml.
    CLEAR gt_fh_ml.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_OBJECT  text
*----------------------------------------------------------------------*
FORM get_belnr_data.

  DATA begin_date TYPE sy-datum.
  DATA end_date TYPE sy-datum.

  DATA r_werks TYPE RANGE OF werks_d WITH HEADER LINE.

  DATA lt_dd07t LIKE TABLE OF dd07t WITH HEADER LINE.

  SELECT * INTO TABLE lt_dd07t FROM dd07t WHERE domname = 'ZBTYPE' AND ddlanguage = sy-langu.

  IF s_werks7[] IS NOT INITIAL OR s_kunnr7 IS NOT INITIAL.

    SELECT DISTINCT 'I' AS sign, 'EQ' AS option, b~werks AS low INTO TABLE @r_werks
      FROM knvp AS a
      INNER JOIN t001w AS b ON a~kunnr = b~werks
      INNER JOIN wrf1 AS c ON b~werks = c~locnr AND c~betrp = 'Z004'
      WHERE a~kunn2 IN @s_kunnr7 AND a~kunnr IN @s_werks7 AND a~parvw = 'RE'.

  ENDIF.

  SELECT a~werks c~kunnr INTO CORRESPONDING FIELDS OF TABLE gt_belnr_sum
    FROM t001w AS a
    INNER JOIN knvp AS b ON a~werks = b~kunnr
    INNER JOIN kna1 AS c ON b~kunn2 = c~kunnr
    INNER JOIN wrf1 AS d ON a~werks = d~locnr AND d~betrp = 'Z004'
    WHERE a~werks IN r_werks AND b~parvw = 'RE' AND zcldt < begin_date
    ORDER BY c~kunnr a~werks.

  begin_date = p_bperi7 && '01'.
  end_date = p_eperi7 && '25'.
  end_date = end_date + 10.
  end_date+6(2) = '01'.
  end_date = end_date - 1.

  SELECT * INTO TABLE @DATA(lt_yhbel_list)
    FROM zmm_yhbel_list
    WHERE crdat BETWEEN @begin_date AND @end_date.

  SELECT * INTO TABLE @DATA(pt_jmfh_h)
    FROM zsd_jmfh_h
    WHERE fhdat BETWEEN @begin_date AND @end_date.

  DATA yt_jmfh_h LIKE TABLE OF zsd_jmfh_h WITH HEADER LINE.
  DATA bt_jmfh_h LIKE TABLE OF zsd_jmfh_h WITH HEADER LINE.
  yt_jmfh_h[] = pt_jmfh_h[].
  bt_jmfh_h[] = pt_jmfh_h[].
  DELETE yt_jmfh_h WHERE yhdln = space OR jmstu <> 3.
  SORT yt_jmfh_h BY yhdln.
  DELETE bt_jmfh_h WHERE yhdln <> space OR jmstu <> 3.
  SORT bt_jmfh_h BY bhnuc.

  REFRESH gt_belnr_sum.
  CLEAR gt_belnr_sum.
  REFRESH gt_jm_belnr.
  CLEAR gt_jm_belnr.
  LOOP AT lt_yhbel_list INTO DATA(ls_yhbel_list).

    IF ls_yhbel_list-btype = 13 OR ls_yhbel_list-btype = 12.
      CONTINUE.
    ENDIF.

    gt_belnr_sum-kunnr = ls_yhbel_list-kunnr.
    gt_belnr_sum-werks = ls_yhbel_list-werks.
    gt_belnr_sum-bukrs = ls_yhbel_list-bukrs.

    gt_jm_belnr = CORRESPONDING #( ls_yhbel_list ).

    CASE ls_yhbel_list-jmtyp.
      WHEN '1'."商品要货
        gt_jm_belnr-jmbln_txt = '要货单'.

        CASE ls_yhbel_list-btype.
          WHEN 01 OR 03 OR 07.
            READ TABLE yt_jmfh_h WITH KEY yhdln = ls_yhbel_list-jmbln BINARY SEARCH.
            IF sy-subrc = 0.
              gt_belnr_sum-spyh_yf_amount = ls_yhbel_list-amount.

              gt_jm_belnr-type = '01'.
            ELSE.
              gt_belnr_sum-spyh_nf_amount = ls_yhbel_list-amount.

              gt_jm_belnr-type = '02'.
            ENDIF.
          WHEN 02 OR 04 OR 08."信贷
          WHEN OTHERS.
            MESSAGE '存在异常单据类型，请联系信息中心' TYPE 'I'.
            LEAVE PROGRAM.
        ENDCASE.

      WHEN '2'."非商品要货

        CASE ls_yhbel_list-btype.
          WHEN 01 OR 07.

            READ TABLE yt_jmfh_h WITH KEY yhdln = ls_yhbel_list-jmbln BINARY SEARCH.
            IF sy-subrc = 0.
              gt_belnr_sum-fspyh_yf_amount = ls_yhbel_list-amount.

              gt_jm_belnr-type = '03'.
            ELSE.
              gt_belnr_sum-fspyh_nf_amount = ls_yhbel_list-amount.

              gt_jm_belnr-type = '04'.
            ENDIF.
          WHEN 02 OR 04 OR  08."信贷
          WHEN OTHERS.
            MESSAGE '存在异常单据类型，请联系信息中心' TYPE 'I'.
            LEAVE PROGRAM.
        ENDCASE.

      WHEN '3'."商品补货

        gt_jm_belnr-jmbln_txt = '要货单'.
        CASE ls_yhbel_list-btype.
          WHEN 05."发货
            gt_belnr_sum-spbh_amount = abs( ls_yhbel_list-amount ).

            gt_jm_belnr-type = '05'.

          WHEN 06."信贷

          WHEN OTHERS.
            MESSAGE '存在异常单据类型，请联系信息中心' TYPE 'I'.
            LEAVE PROGRAM.
        ENDCASE.

      WHEN '4'."非商品补货
        gt_jm_belnr-jmbln_txt = '发货单'.
        CASE ls_yhbel_list-btype.
          WHEN 05.
            gt_belnr_sum-fspbh_amount = abs( ls_yhbel_list-amount ).

            gt_jm_belnr-type = '06'.
          WHEN OTHERS.
            MESSAGE '存在异常单据类型，请联系信息中心' TYPE 'I'.
            LEAVE PROGRAM.
        ENDCASE.

      WHEN '5'."oa发货

        gt_jm_belnr-jmbln_txt = '发货单'.
        CASE ls_yhbel_list-btype.
          WHEN 09 OR 10.
            READ TABLE bt_jmfh_h WITH KEY bhnuc = ls_yhbel_list-jmbln BINARY SEARCH.
            IF sy-subrc = 0.
              gt_belnr_sum-oa_yf_amount = abs( ls_yhbel_list-amount ).
              gt_jm_belnr-type = '07'.
            ELSE.
              gt_jm_belnr-type = '08'.
              gt_belnr_sum-oa_nf_amount = abs( ls_yhbel_list-amount ).
            ENDIF.
          WHEN OTHERS.
            MESSAGE '存在异常单据类型，请联系信息中心' TYPE 'I'.
            LEAVE PROGRAM.
        ENDCASE.

      WHEN '6'."商品退货

        gt_jm_belnr-jmbln_txt = '退货单'.
        CASE ls_yhbel_list-btype.
          WHEN 11 OR 14.
            gt_belnr_sum-spth_amount = abs( ls_yhbel_list-amount ).
          WHEN 15.
            gt_belnr_sum-spth_amount = ls_yhbel_list-amount.
          WHEN OTHERS.
            MESSAGE '存在异常单据类型，请联系信息中心' TYPE 'I'.
            LEAVE PROGRAM.
        ENDCASE.
      WHEN '7'."改款

        gt_jm_belnr-jmbln_txt = '改款单'.
        CASE ls_yhbel_list-btype.
          WHEN 16.
            gt_jm_belnr-type = '09'.
            gt_belnr_sum-spgk_amount = abs( ls_yhbel_list-amount ).
          WHEN OTHERS.
            MESSAGE '存在异常单据类型，请联系信息中心' TYPE 'I'.
            LEAVE PROGRAM.
        ENDCASE.
      WHEN '8'."维修

        gt_jm_belnr-jmbln_txt = '维修单'.
        CASE ls_yhbel_list-btype.
          WHEN 17.
            gt_belnr_sum-spwx_amount = abs( ls_yhbel_list-amount ).
            gt_jm_belnr-type = '10'.
          WHEN OTHERS.
            MESSAGE '存在异常单据类型，请联系信息中心' TYPE 'I'.
            LEAVE PROGRAM.
        ENDCASE.

      WHEN OTHERS.
        MESSAGE '存在异常单据类型，请联系信息中心' TYPE 'I'.
        LEAVE PROGRAM.
    ENDCASE.

    READ TABLE lt_dd07t WITH KEY domvalue_l = ls_yhbel_list-btype.
    IF sy-subrc = 0.
      gt_jm_belnr-btype_txt = lt_dd07t-ddtext.
    ENDIF.

    COLLECT gt_belnr_sum.
    APPEND gt_jm_belnr.

    CLEAR gt_jm_belnr.
    CLEAR gt_belnr_sum.
  ENDLOOP.


  DATA lt_werks LIKE TABLE OF zls_werks WITH HEADER LINE.
  DATA lt_kunnr LIKE TABLE OF zls_kunnr WITH HEADER LINE.
  DATA lt_bukrs LIKE TABLE OF zls_bukrs WITH HEADER LINE.

  LOOP AT gt_belnr_sum.

    READ TABLE lt_werks WITH KEY werks = gt_belnr_sum-werks.
    IF sy-subrc <> 0.
      SELECT SINGLE werks name1 AS werks_name INTO lt_werks FROM t001w WHERE werks = gt_belnr_sum-werks.
      APPEND lt_werks.
    ENDIF.
    gt_belnr_sum-werks_name = lt_werks-werks_name.

    READ TABLE lt_kunnr WITH KEY kunnr = gt_belnr_sum-kunnr.
    IF sy-subrc <> 0.
      SELECT SINGLE kunnr name1 AS kunnr_name INTO lt_kunnr FROM kna1 WHERE kunnr = gt_belnr_sum-kunnr.
      APPEND lt_kunnr.
    ENDIF.
    gt_belnr_sum-kunnr_name = lt_kunnr-kunnr_name.

    READ TABLE lt_bukrs WITH KEY bukrs = gt_belnr_sum-bukrs.
    IF sy-subrc <> 0.
      SELECT SINGLE bukrs butxt AS bukrs_name INTO lt_bukrs FROM t001 WHERE bukrs = gt_belnr_sum-bukrs.
      APPEND lt_bukrs.
    ENDIF.
    gt_belnr_sum-bukrs_name = lt_bukrs-bukrs_name.

    MODIFY gt_belnr_sum.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_OBJECT  text
*----------------------------------------------------------------------*
FORM change_toolbar  CHANGING p_object TYPE REF TO cl_alv_event_toolbar_set.

  CASE g_tb_tab-subscreen.
    WHEN '0201'.
      add_toolbar 'COND' '查询' '查询' icon_execute_object 'X' 'I'.

      add_toolbar 'PRIN' '打印' '打印' '@4K@' 'X' 'P'.
    WHEN '0202'.
      add_toolbar 'COND' '查询' '查询' icon_execute_object 'X' 'I'.

      add_toolbar 'PRIN' '打印' '打印' '@4K@' 'X' 'P'.
    WHEN '0203'.
      add_toolbar 'COND' '查询' '查询' icon_execute_object 'X' 'I'.
    WHEN '0204'.
    WHEN '0205'.
      add_toolbar 'COND' '查询' '查询' icon_execute_object 'X' 'I'.
      add_toolbar 'DSET' '日期设置' '日期设置' '@1U@' 'X' 'P'.
    WHEN '0206'.
      add_toolbar 'COND' '查询' '查询' icon_execute_object 'X' 'I'.
    WHEN '0207'.
      add_toolbar 'COND' '查询' '查询' icon_execute_object 'X' 'I'.
    WHEN '0208' OR '0209'.

      add_toolbar 'COND' '查询' '查询' icon_execute_object 'X' 'I'.
      add_toolbar 'RESE' '冲销' '冲销' '@1J@' 'X' 'P'.

      IF sy-uname+0(2) = 'IT'.
        add_toolbar 'RESI' '高级冲销' '高级冲销' '@2D@' 'X' 'P'.
      ENDIF.

    WHEN '0210'.
      add_toolbar 'COND' '查询' '查询' icon_execute_object 'X' 'I'.
    WHEN '0211'.
      add_toolbar 'COND' '查询' '查询' icon_execute_object 'X' 'I'.
      add_toolbar 'REJE' '退货' '退货' '@1J@' 'X' 'P'.
*      add_toolbar 'CONT' '续传' '续传' '@1J@' 'X' 'P'.
    WHEN '0212'.
      add_toolbar 'COND' '查询' '查询' icon_execute_object 'X' 'I'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHEK_SEL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chek_sel_data .

  DATA lt_rows TYPE lvc_t_roid.

  CASE g_tb_tab-subscreen.
    WHEN '0201'.

    WHEN '0202'.
      gt_fspfh_data-sel = space.
      MODIFY gt_fspfh_data FROM gt_fspfh_data TRANSPORTING sel WHERE sel = 'X'.

      alv2->get_selected_rows( IMPORTING et_row_no = lt_rows ).
      LOOP AT lt_rows INTO DATA(ls_row).
        gt_fspfh_data-sel = 'X'.
        MODIFY gt_fspfh_data INDEX ls_row-row_id FROM gt_fspfh_data TRANSPORTING sel.
      ENDLOOP.

      READ TABLE gt_fspfh_data WITH KEY sel = 'X'.
      IF sy-subrc <> 0.
        MESSAGE '未选中任何数据' TYPE 'S' DISPLAY LIKE 'E'.
        return_flag = 'X'.
        RETURN.
      ENDIF.
    WHEN '0205'.

    WHEN '0206'.

    WHEN '0208'.

      gt_spfh_head-sel = space.
      MODIFY gt_spfh_head FROM gt_spfh_head TRANSPORTING sel WHERE sel = 'X'.

      alv8->get_selected_rows( IMPORTING et_row_no = lt_rows ).
      LOOP AT lt_rows INTO ls_row.
        gt_spfh_head-sel = 'X'.
        MODIFY gt_spfh_head INDEX ls_row-row_id FROM gt_spfh_head TRANSPORTING sel.
      ENDLOOP.

      READ TABLE gt_spfh_head WITH KEY sel = 'X'.
      IF sy-subrc <> 0.
        MESSAGE '未选中任何数据' TYPE 'I'.
        return_flag = 'X'.
        RETURN.
      ELSE.
        REFRESH gt_fh_head.
        LOOP AT gt_spfh_head WHERE sel = 'X' AND jmstu <> '1'.
          APPEND gt_spfh_head TO gt_fh_head.
        ENDLOOP.
      ENDIF.

    WHEN '0209'.

      gt_fspfh_head-sel = space.
      MODIFY gt_fspfh_head FROM gt_fspfh_head TRANSPORTING sel WHERE sel = 'X'.

      alv9->get_selected_rows( IMPORTING et_row_no = lt_rows ).
      LOOP AT lt_rows INTO ls_row.
        gt_fspfh_head-sel = 'X'.
        MODIFY gt_fspfh_head INDEX ls_row-row_id FROM gt_fspfh_head TRANSPORTING sel.
      ENDLOOP.

      READ TABLE gt_fspfh_head WITH KEY sel = 'X'.
      IF sy-subrc <> 0.
        MESSAGE '未选中任何数据' TYPE 'I'.
        return_flag = 'X'.
        RETURN.
      ELSE.
        REFRESH gt_fh_head.
        LOOP AT gt_fspfh_head WHERE sel = 'X' AND jmstu <> '1'.
          APPEND gt_fspfh_head TO gt_fh_head.
        ENDLOOP.
      ENDIF.
    WHEN '0211'.
      gs_kyth_data-sel = space.
      MODIFY gt_kyth_data_temp FROM gs_kyth_data TRANSPORTING sel WHERE sel = 'X'.

      alv11->get_selected_rows( IMPORTING et_row_no = lt_rows ).
      LOOP AT lt_rows INTO ls_row.
        gs_kyth_data-sel = 'X'.
        MODIFY gt_kyth_data_temp INDEX ls_row-row_id FROM gs_kyth_data TRANSPORTING sel.
      ENDLOOP.

      READ TABLE gt_kyth_data_temp INTO gs_kyth_data WITH KEY sel = 'X'.
      IF sy-subrc <> 0.
        MESSAGE '未选中任何数据' TYPE 'I'.
        return_flag = 'X'.
        RETURN.
      ELSE.
        REFRESH gt_kyth_data.
        LOOP AT gt_kyth_data_temp INTO gs_kyth_data WHERE sel = 'X' .
          APPEND gs_kyth_data TO gt_kyth_data.
        ENDLOOP.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PRINT_FSPDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_fspdata .

  DATA lt_head LIKE TABLE OF zzsmm04_1 WITH HEADER LINE.
  DATA lt_item LIKE TABLE OF zzsmm03_1 WITH HEADER LINE.

  DATA:BEGIN OF ls_bhnuc,
         bhnuc TYPE char10,
         flgor TYPE char4,
         fwerk TYPE werks_d,
         werks TYPE char4,
         vbeln TYPE vbeln,
       END OF ls_bhnuc,
       pt_bhnuc LIKE TABLE OF ls_bhnuc WITH HEADER LINE.

  DATA ittab_name TYPE char50.
  FIELD-SYMBOLS: <pt_fspfh_print> TYPE ANY TABLE.
  DATA pt_fspfh_print LIKE TABLE OF ls_jmfh_print WITH HEADER LINE.

  DATA lt_rows TYPE lvc_t_roid.

  ittab_name = '(SAPLZFUNCTION_UTIL)<LT_ALV_TAB>'.
  ASSIGN (ittab_name) TO <pt_fspfh_print>.
  lt_fspfh_print[] = <pt_fspfh_print>.

  fspp_alv->get_selected_rows( IMPORTING et_row_no = lt_rows ).

  LOOP AT lt_rows INTO DATA(ls_rows).

    READ TABLE lt_fspfh_print INDEX ls_rows-row_id.
    pt_bhnuc-bhnuc = lt_fspfh_print-bhnuc.
    pt_bhnuc-flgor = lt_fspfh_print-flgor.
    pt_bhnuc-werks = lt_fspfh_print-werks.
    pt_bhnuc-fwerk = lt_fspfh_print-fwerk.
    COLLECT pt_bhnuc.
  ENDLOOP.

  CHECK lt_rows[] IS NOT INITIAL.

  IF pt_bhnuc[] IS NOT INITIAL.

    SELECT bhnuc,odeln,crdat,crnam INTO TABLE @DATA(lt_vbeln)
      FROM ztrade_odlist
      FOR ALL ENTRIES IN @pt_bhnuc
      WHERE bhnuc = @pt_bhnuc-bhnuc
        AND ogrup = ( SELECT MAX( ogrup ) FROM ztrade_odlist WHERE bhnuc = @pt_bhnuc-bhnuc )
        AND step = 002.

    LOOP AT pt_bhnuc.

      READ TABLE lt_vbeln INTO DATA(ls_vbeln) WITH KEY bhnuc = pt_bhnuc-bhnuc.

      lt_head-vbeln = ls_vbeln-odeln.
      lt_head-ernam = ls_vbeln-crnam.
      lt_head-erdat = ls_vbeln-crdat.

      lt_head-lgort = pt_bhnuc-flgor.
      lt_head-kunnr = pt_bhnuc-werks.
      SELECT SINGLE lgobe INTO lt_head-lgobe FROM t001l
        WHERE lgort = lt_head-lgort AND werks = pt_bhnuc-fwerk.

      SELECT SINGLE name1 INTO lt_head-name1 FROM t001w WHERE werks = lt_head-kunnr.

      SELECT SINGLE remark
        INTO lt_head-note
        FROM zsd_jmfh_h
        WHERE bhnuc = pt_bhnuc-bhnuc
          AND mtype = '02'.

      APPEND lt_head.
    ENDLOOP.
  ENDIF.

  CHECK lt_head[] IS NOT INITIAL.

  DATA fm_name TYPE rs38l_fnam.
  DATA control_parameter TYPE ssfctrlop.
  DATA ls_job_output_info TYPE ssfcrescl.
  DATA ls_job_output_options TYPE ssfcresop.
  DATA output_options TYPE ssfcompop.

  DATA next_tabix TYPE i.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZTLMM1009SM001'
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  control_parameter-no_open   = 'X'.
  control_parameter-no_close  = 'X'."连续打印不中断

  output_options-tdimmed = 'X'.
  output_options-tddelete = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control_parameter
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  CHECK sy-subrc = 0.

  LOOP AT lt_head.

    REFRESH lt_item.

    SELECT a~vbeln,a~posnr,a~matnr,b~maktx,a~charg,a~meins,a~lfimg
      INTO CORRESPONDING FIELDS OF TABLE @lt_item
      FROM lips AS a
      INNER JOIN makt AS b ON a~matnr = b~matnr
      WHERE a~vbeln = @lt_head-vbeln
        AND b~spras = @sy-langu.

    CALL FUNCTION fm_name
      EXPORTING
        control_parameters = control_parameter
        output_options     = output_options
        wa_header          = lt_head
      IMPORTING
        job_output_info    = ls_job_output_info
        job_output_options = ls_job_output_options
      TABLES
        it_data            = lt_item.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDLOOP.

  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = ls_job_output_info
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  IF ls_job_output_info-outputdone = 'X'.
    LOOP AT lt_vbeln INTO ls_vbeln.
      UPDATE zsd_jmfh_h SET prinn = prinn + 1 WHERE bhnuc = ls_vbeln-bhnuc AND mtype = '02'.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POPUP_PRINT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM popup_print_screen .

  DATA lt_bhnuc TYPE TABLE OF zbhnuc WITH HEADER LINE.

  DATA lt_head LIKE TABLE OF zalv_head WITH HEADER LINE.
  DATA ls_element TYPE zdialogbox_element.
  DATA cl_event TYPE REF TO zcl_alv_event_receiver.

  LOOP AT gt_fspfh_data WHERE sel = 'X'.
    lt_bhnuc = gt_fspfh_data-bhnuc.
    COLLECT lt_bhnuc.
  ENDLOOP.

  SELECT a~bhnuc,a~fwerk,a~flgor,a~werks,a~fhdat,a~fhnam,b~name1 AS werks_name
    INTO CORRESPONDING FIELDS OF TABLE @lt_fspfh_print
    FROM zsd_jmfh_h AS a
    INNER JOIN t001w AS b ON a~werks = b~werks
    FOR ALL ENTRIES IN @lt_bhnuc
    WHERE a~bhnuc = @lt_bhnuc-table_line
      AND a~mtype = '02'
      AND a~jmstu = '3'.

  SORT lt_fspfh_print BY bhnuc.

  lt_head[] = VALUE #(
  ( fieldname = 'BHNUC' fieldtext = '发货单号' )
  ( fieldname = 'FWERK' fieldtext = '发货工厂' )
  ( fieldname = 'WERKS' fieldtext = '加盟门店' )
  ( fieldname = 'WERKS_NAME' fieldtext = '加盟门店名称' )
  ( fieldname = 'FHDAT' fieldtext = '发货日期' )
  ( fieldname = 'FHNAM' fieldtext = '发货人员' ) ).

  ls_element-width = 600.
  ls_element-height = 400.
  ls_element-top = 200.
  ls_element-left = 300.
  ls_element-caption = '加盟非商品打印'.

  cl_event = NEW zcl_alv_event_receiver( ).
  cl_event->handle_toolbar_perform = 'HANDLE_FSPP_TOOLBAR'.
  cl_event->user_command_perform = 'USER_FSPP_COMMAND'.
  cl_event->g_program = sy-repid.

  FREE fspp_alv.

  CALL FUNCTION 'ZPOPUP_ALV1'
    EXPORTING
      ls_dialog_element = ls_element
    IMPORTING
      im_alv            = fspp_alv
    TABLES
      it_table          = lt_fspfh_print[]
      it_head           = lt_head[]
    CHANGING
      cl_event          = cl_event.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PRINT_SPDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_spdata .

  DATA lt_rows TYPE lvc_t_roid.

  gt_spfh_data-sel = space.
  MODIFY gt_spfh_data FROM gt_spfh_data TRANSPORTING sel WHERE sel = 'X'.

  alv1->get_selected_rows( IMPORTING et_row_no = lt_rows ).

  LOOP AT lt_rows INTO DATA(ls_rows).
    gt_spfh_data-sel = 'X'.
    MODIFY gt_spfh_data INDEX ls_rows-row_id FROM gt_spfh_data TRANSPORTING sel.
  ENDLOOP.


  DATA lt_bhnuc TYPE TABLE OF numc10 WITH HEADER LINE.
  DATA lt_print LIKE TABLE OF zzsd_1036 WITH HEADER LINE.

  LOOP AT gt_spfh_data WHERE sel = 'X'.
    lt_bhnuc = gt_spfh_data-bhnuc.
    COLLECT lt_bhnuc.
  ENDLOOP.

  IF lt_bhnuc[] IS INITIAL.
    MESSAGE '请选择要打印的数据' TYPE 'I'.
    RETURN.
  ENDIF.

  SELECT c~crdat AS zcdat,a~werks,b~z_jtm,c~crnam AS zcusr,kwmeng,a~bhnuc,bhitem,gujia,
         a~z_sgj,z_dbj,z_pp,z_zz,z_sccd,z_zszl,
         z_fszl,z_khj,z_jzl,z_jczs,z_gjzs,z_jszs,z_sfz,z_zppl,z_ks,z_spmc,zzpbz
    INTO TABLE @DATA(lt_attr)
    FROM zsd_jiamengfahuo AS a
    INNER JOIN zmm_dpsxb AS b ON a~charg = b~charg
    INNER JOIN zsd_jmfh_h AS c ON a~bhnuc = c~bhnuc AND c~mtype = '01'
    FOR ALL ENTRIES IN @lt_bhnuc
    WHERE a~bhnuc = @lt_bhnuc-table_line.

  LOOP AT lt_attr INTO DATA(ls_attr).
    MOVE-CORRESPONDING ls_attr TO lt_print.
    lt_print-z_zz = |{ round( val = ls_attr-z_zz dec = 3 mode =  5 ) }|.
    lt_print-z_sgj = ls_attr-z_sgj * ls_attr-kwmeng.

    lt_print-z_dbj = ls_attr-gujia * ls_attr-kwmeng.
    lt_print-z_zszl = |{ round( val = ls_attr-z_zszl dec = 3 mode =  5 ) }|.
    lt_print-z_fszl = |{ round( val = ls_attr-z_fszl dec = 3 mode =  5 ) }|.

    lt_print-z_khj = |{ round( val = ls_attr-z_khj dec = 2 mode =  5 ) }|.
    lt_print-z_jzl = |{ round( val = ls_attr-z_jzl dec = 3 mode =  5 ) }|.

    IF ls_attr-zzpbz = 'X'.
      CLEAR lt_print-z_dbj.
    ENDIF.
    APPEND lt_print.
  ENDLOOP.

  lt_print-t_type = '跨公司调拨'.

  MODIFY lt_print FROM lt_print TRANSPORTING name1 t_type WHERE name1 = space.

  DATA fm_name TYPE rs38l_fnam.
  DATA control_parameter TYPE ssfctrlop.
  DATA ls_job_output_info TYPE ssfcrescl.
  DATA ls_job_output_options TYPE ssfcresop.
  DATA output_options TYPE ssfcompop.

  DATA next_tabix TYPE i.
  DATA it_print LIKE TABLE OF zzsd_1036 WITH HEADER LINE.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZTLSD_SM_1036'
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  control_parameter-no_open   = 'X'.
  control_parameter-no_close  = 'X'.

  output_options-tdimmed = 'X'.
  output_options-tddelete = 'X'.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = control_parameter
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  SORT lt_print BY bhnuc bhitem.
  LOOP AT lt_print.

    next_tabix = sy-tabix + 1.

    APPEND lt_print TO it_print.

    READ TABLE lt_print INTO DATA(ls_print) INDEX next_tabix.
    IF sy-subrc <> 0 OR lt_print-bhnuc <> ls_print-bhnuc.

      SELECT SINGLE name1 INTO it_print-name1 FROM t001w WHERE werks = lt_print-werks.
      MODIFY it_print FROM it_print TRANSPORTING name1 WHERE name1 IS INITIAL.

      CALL FUNCTION fm_name
        EXPORTING
          control_parameters = control_parameter
          output_options     = output_options
          user_settings      = 'X'
        IMPORTING
          job_output_info    = ls_job_output_info
          job_output_options = ls_job_output_options
        TABLES
          itab               = it_print[]
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.

      REFRESH it_print.
      CLEAR it_print.

    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = ls_job_output_info
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REVERSE_SPFH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reverse_fh_data .

  DATA lt_odlist LIKE TABLE OF ztrade_odlist WITH HEADER LINE.
  DATA reverse_flag TYPE c.
  DATA lines TYPE i.

  DATA(cl_reverse_od) = NEW zcl_reverse_orders( ).

  READ TABLE gt_fh_head WITH KEY stype = '3'.
  IF sy-subrc = 0.
    MESSAGE 'OA类型发货单将不会被冲销' TYPE 'I'.
    DELETE gt_fh_head WHERE stype = '3'.
  ENDIF.

  LOOP AT gt_fh_head.

    SELECT * INTO TABLE lt_odlist FROM ztrade_odlist
      WHERE bhnuc = gt_fh_head-bhnuc ORDER BY ogrup DESCENDING step DESCENDING.

    IF gt_fh_head-linkn <> space.
      SELECT SINGLE * INTO @DATA(ls_cre_list) FROM zmm_cre_list WHERE xlenr = @gt_fh_head-bhnuc AND status = 'C'.
      IF sy-subrc = 0.
        MESSAGE |发货单 { gt_fh_head-bhnuc } 信贷已经开始还款，单据冲销将不完全| TYPE 'I'.
        DELETE lt_odlist WHERE ogrup = 000.
      ENDIF.
    ENDIF.

    IF ls_revs IS NOT INITIAL.
      DELETE lt_odlist WHERE ogrup < ls_revs-ogrup OR ( ogrup = ls_revs-ogrup AND step < ls_revs-step ).
    ENDIF.

    LOOP AT lt_odlist.

      lines = sy-tabix.

      TRY .

          IF lines = 1 AND gt_fh_head-jmstu = '3'.
            SELECT SINGLE ebelp, gjahr, belnr, lfbnr INTO @DATA(ls_ekbe)
              FROM ekbe
              WHERE ebeln = @lt_odlist-odeln AND vgabe = '1'.

            IF sy-subrc = 0 AND reverse_flag = space.

              textline1 = '存在发货单门店已收货，是否冲销'.
              textline2 = '如果确认，则默认所选择的单据都按照此操作进行'.
              titel = '冲销提醒'.
              PERFORM pop_message_window.

              IF return_flag IS NOT INITIAL.
                RETURN.
              ELSE.
                reverse_flag = 'X'.
              ENDIF.

              cl_reverse_od->reverse_mblnr( ebeln = lt_odlist-odeln ).
            ELSEIF sy-subrc = 0 AND reverse_flag = 'X'.

              cl_reverse_od->reverse_mblnr( ebeln = lt_odlist-odeln ).
            ENDIF.
          ENDIF.

          CASE lt_odlist-odetp.
            WHEN '1'.
              cl_reverse_od->delete_po( lt_odlist-odeln ).
            WHEN '2'.
              cl_reverse_od->delete_so( lt_odlist-odeln ).
            WHEN '3'.
              cl_reverse_od->delete_sto( vbeln = lt_odlist-odeln vtype = '2' ).
            WHEN '4'.
              SELECT SINGLE tcode2,le_vbeln INTO @DATA(ls_mkpf) FROM mkpf
                WHERE mblnr = @lt_odlist-odeln AND mjahr = @lt_odlist-odjhr.

              IF ls_mkpf-tcode2 = 'MB01'.
                cl_reverse_od->reverse_mblnr( mblnr = lt_odlist-odeln mjahr = CONV #( lt_odlist-odjhr ) ).
              ELSEIF ls_mkpf-tcode2 = 'VL02N'.
                cl_reverse_od->reverse_sto( vbeln = ls_mkpf-le_vbeln vtype = '2' ).
              ELSE.
                MESSAGE |物料凭证 { lt_odlist-odeln } 无法识别来源| TYPE 'I'.
                return_flag = 'X'.
                RETURN.
              ENDIF.
              CLEAR ls_mkpf.
            WHEN '5'.
              cl_reverse_od->reverse_belnr( belnr = lt_odlist-odeln gjahr = CONV #( lt_odlist-odjhr ) bukrs = lt_odlist-bukrs  ).

              DELETE FROM zmm_yhbel_list WHERE jmbln = lt_odlist-bhnuc AND belnr = lt_odlist-odeln.
            WHEN OTHERS.
          ENDCASE.

        CATCH zcx_break_program INTO DATA(lr_break_program).
          IF lr_break_program->lt_return[] IS NOT INITIAL.

            PERFORM pop_message TABLES lr_break_program->lt_return[].

          ELSEIF lr_break_program->ls_msg IS NOT INITIAL.
            MESSAGE lr_break_program->ls_msg TYPE 'I'.
          ENDIF.
          return_flag = 'X'.
          RETURN.
      ENDTRY.

      DELETE FROM ztrade_odlist WHERE bhnuc = lt_odlist-bhnuc AND ogrup = lt_odlist-ogrup AND step = lt_odlist-step.

      IF lt_odlist-ogrup = 000 AND lt_odlist-step = 002 AND gt_fh_head-linkn <> space.
        DELETE FROM zmm_cre_list WHERE xlenr = gt_fh_head-bhnuc.
        UPDATE zsd_jmfh_h SET linkn = space cre_amount = 0 WHERE bhnuc = lt_odlist-bhnuc AND mtype = '01'.
      ENDIF.

      IF lines = 1.
        UPDATE zsd_jmfh_h SET jmstu = '2' fhdat = '00000000' fhtim = space fhnam = space WHERE bhnuc = lt_odlist-bhnuc.

        gt_fh_head-jmstu = '2'.
        gt_fh_head-jmstu_txt = '@5C@ 发货未完成'.
        IF g_tb_tab-pressed_tab = 'TAB8'.
          MODIFY gt_spfh_head FROM gt_fh_head TRANSPORTING jmstu jmstu_txt WHERE bhnuc = gt_spfh_head-bhnuc.
        ELSEIF g_tb_tab-pressed_tab = 'TAB9'.
          MODIFY gt_fspfh_head FROM gt_fspfh_head TRANSPORTING jmstu jmstu_txt WHERE bhnuc = gt_fspfh_head-bhnuc.
        ENDIF.
      ENDIF.
      COMMIT WORK.

    ENDLOOP.

    SELECT SINGLE * INTO lt_odlist FROM ztrade_odlist WHERE bhnuc = gt_fh_head-bhnuc.
    IF sy-subrc <> 0.
      UPDATE zsd_jmfh_h SET jmstu = '1' pathn = space WHERE bhnuc = gt_fh_head-bhnuc.
      COMMIT WORK.

      gt_fh_head-jmstu = '1'.
      gt_fh_head-jmstu_txt = '@5B@ 未发货'.
      IF g_tb_tab-pressed_tab = 'TAB8'.
        MODIFY gt_spfh_head FROM gt_fh_head TRANSPORTING jmstu jmstu_txt WHERE bhnuc = gt_fh_head-bhnuc.
        UPDATE zsd_jiamengfahuo SET ofsto = space WHERE bhnuc = gt_fh_head-bhnuc.

      ELSEIF g_tb_tab-pressed_tab = 'TAB9'.
        MODIFY gt_fspfh_head FROM gt_fh_head TRANSPORTING jmstu jmstu_txt WHERE bhnuc = gt_fh_head-bhnuc.
      ENDIF.

    ENDIF.

    REFRESH lt_odlist.

  ENDLOOP.

  REFRESH gt_fh_head.

  MESSAGE '冲销完成' TYPE 'S'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POP_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pop_message TABLES lt_return STRUCTURE bapiret2.

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
*&      Form  POP_MESSAGE_WINDOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pop_message_window .


  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      textline1      = textline1
      textline2      = textline2
      titel          = titel
      cancel_display = ''
    IMPORTING
      answer         = answer.

  CLEAR textline1.
  CLEAR textline2.
  CLEAR titel.

  IF answer <> 'J'.
    return_flag = 'X'.
  ELSE.
    CLEAR return_flag.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_period CHANGING period TYPE kmonth.

  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
    EXPORTING
      actual_month               = sy-datum+0(6)
      language                   = sy-langu
      start_column               = 8
      start_row                  = 5
    IMPORTING
      selected_month             = period
    EXCEPTIONS
      factory_calendar_not_found = 1
      holiday_calendar_not_found = 2
      month_not_found            = 3
      OTHERS                     = 4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONFIG_CANCLE_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM config_cancle_date .

  TRANSLATE ls_yh-werks TO UPPER CASE.

  SELECT SINGLE SUM( all_amount ) INTO @DATA(temp_all_amount)
    FROM zmm_spyh_h
    WHERE werks = @ls_yh-werks AND status = 'A'.

  IF temp_all_amount = 0.
    MESSAGE |未发现门店 { ls_yh-werks } 存在待审批要货单| TYPE 'I'.
    RETURN.
  ELSEIF ls_yh-cadat <= sy-datum.
    MESSAGE '取消日期必须大于当前日期' TYPE 'I'.
    RETURN.
  ENDIF.

  textline1 = |该门店有价值 { temp_all_amount } 元的要货商品未审批|.
  textline2 = |请确认是否要设置要货过期失效时间|.
  titel = '要货设置提醒'.
  PERFORM pop_message_window.

  CHECK return_flag IS INITIAL.

  UPDATE zmm_spyh_h SET cadat = ls_yh-cadat WHERE werks = ls_yh-werks AND status = 'A'.

  MESSAGE '日期设置完成' TYPE 'S'.

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_KYTH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_kyth_data .
  DATA lt_matnr LIKE TABLE OF zls_matnr WITH HEADER LINE.
  DATA lt_werks LIKE TABLE OF zls_werks WITH HEADER LINE.
  FIELD-SYMBOLS <fs_kyth> LIKE LINE OF gt_kyth_data.
  REFRESH gt_kyth_data_temp.
  SELECT DISTINCT b~bhnuc
                  b~bhitem
                  a~werks
                  b~matnr
                  b~charg
                  a~crdat
                  a~crnam
                  a~fhdat
                  a~fhnam
                  b~kwmeng AS menge
    INTO CORRESPONDING FIELDS OF TABLE gt_kyth_data_temp
    FROM zsd_jiamengfahuo AS b
    INNER JOIN zsd_jmfh_h AS a ON a~bhnuc = b~bhnuc
    WHERE a~mtype = '02'
      AND a~bhnuc IN s_bhnu11
      AND a~werks IN s_werk11
      AND a~crdat IN s_crda11
      AND a~crnam IN s_crna11
      AND a~fhdat IN s_fhda11
      AND a~fhnam IN s_fhna11.
  IF gt_kyth_data_temp IS NOT INITIAL.
    SELECT werks
           name1 AS werks_name
    INTO TABLE lt_werks
    FROM t001w FOR ALL ENTRIES IN gt_kyth_data_temp
    WHERE werks = gt_kyth_data_temp-werks
      AND spras = sy-langu.

    SELECT matnr
           maktx
    INTO TABLE lt_matnr
    FROM makt FOR ALL ENTRIES IN gt_kyth_data_temp
    WHERE matnr = gt_kyth_data_temp-matnr
      AND spras = sy-langu.

    SELECT a~bhnuc,
           bhitem,
           thstu,
           menge
    INTO TABLE @DATA(lt_kyth)
    FROM zmm_kyth_h AS a
    INNER JOIN zmm_kyth_jy AS b ON a~bhnuc = b~bhnuc
    FOR ALL ENTRIES IN @gt_kyth_data_temp
    WHERE a~bhnuc = @gt_kyth_data_temp-bhnuc
      AND b~bhitem = @gt_kyth_data_temp-bhitem.
  ENDIF.
  LOOP AT gt_kyth_data_temp ASSIGNING <fs_kyth>.

    IF <fs_kyth>-crdat+4(2) = sy-datum+4(2).
      MESSAGE '存在当月单据，请使用常规冲销' TYPE 'E'.
    ENDIF.

    READ TABLE lt_werks  WITH KEY werks = <fs_kyth>-werks.
    IF sy-subrc = 0.
      <fs_kyth>-werks_name = lt_werks-werks_name.
    ENDIF.

    READ TABLE lt_matnr WITH KEY matnr = <fs_kyth>-matnr.
    IF sy-subrc = 0.
      <fs_kyth>-maktx = lt_matnr-maktx.
    ENDIF.

    READ TABLE lt_kyth INTO DATA(ls_kyth) WITH KEY bhnuc = <fs_kyth>-bhnuc
                                                   bhitem = <fs_kyth>-bhitem.
    IF sy-subrc = 0.
      CASE ls_kyth-thstu.
        WHEN '1'.
          <fs_kyth>-status = '@5D@退货中'.
        WHEN '2'.
          <fs_kyth>-status = '@06@已退货'.
        WHEN OTHERS.
      ENDCASE.
      <fs_kyth>-menge = ls_kyth-menge.
    ENDIF.
    CLEAR:lt_matnr,lt_werks.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_TH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_th_data .

  DATA:lv_bhnuc TYPE zbhnuc.
  DATA:lt_kyth TYPE TABLE OF zsmm_kyth,
       ls_kyth TYPE zsmm_kyth.
  DATA:lt_return TYPE TABLE OF bapiret2.
  DATA:lv_change ."数量发生变化
  FIELD-SYMBOLS <fs_kyth> LIKE LINE OF gt_kyth_data_temp.
  "step1 取得路径等相关数据
  CLEAR: gs_kyth_data,
         lv_bhnuc,
         lv_change.
  READ TABLE gt_kyth_data INTO gs_kyth_data INDEX 1.
  lv_bhnuc = gs_kyth_data-bhnuc.

  SELECT bhnuc,
         bhitem,
         kwmeng
  INTO TABLE @DATA(lt_jiamengfahuo)
  FROM zsd_jiamengfahuo
  WHERE bhnuc = @lv_bhnuc.

  LOOP AT gt_kyth_data INTO gs_kyth_data.
    READ TABLE lt_jiamengfahuo INTO DATA(ls_jiamengfahuo) WITH KEY bhnuc = gs_kyth_data-bhnuc
                                                                   bhitem = gs_kyth_data-bhitem.
    IF sy-subrc = 0.
      IF ls_jiamengfahuo-kwmeng <> gs_kyth_data-menge.
        lv_change = 'X'.
      ENDIF.
    ENDIF.
    MOVE-CORRESPONDING gs_kyth_data TO ls_kyth.
    APPEND ls_kyth TO lt_kyth.
    CLEAR:ls_kyth,gs_kyth_data.
  ENDLOOP.

  CALL FUNCTION 'ZFMMM_KYTH'
    EXPORTING
      im_bhnuc        = lv_bhnuc
      im_change       = lv_change
    TABLES
      gt_data         = lt_kyth
      return          = lt_return
    EXCEPTIONS
      error_exception = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    LOOP AT gt_kyth_data_temp ASSIGNING <fs_kyth> .
      READ TABLE lt_kyth INTO ls_kyth WITH KEY bhnuc = <fs_kyth>-bhnuc
                                                     bhitem = <fs_kyth>-bhitem.
      IF sy-subrc = 0.
        <fs_kyth>-thstu = '1'.
        <fs_kyth>-status = '@5D@退货中'.
      ENDIF.
    ENDLOOP.
    CALL FUNCTION 'SUSR_DISPLAY_LOG'
      EXPORTING
        display_in_popup = 'X'
      TABLES
        it_log_bapiret2  = lt_return
      EXCEPTIONS
        parameter_error  = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    MESSAGE '退货成功' TYPE 'S'.
    LOOP AT gt_kyth_data_temp ASSIGNING <fs_kyth> .
      READ TABLE lt_kyth INTO ls_kyth WITH KEY bhnuc = <fs_kyth>-bhnuc
                                               bhitem = <fs_kyth>-bhitem.
      IF sy-subrc = 0.
        <fs_kyth>-thstu = '2'.
        <fs_kyth>-status = '@06@已退货'.
      ENDIF.
    ENDLOOP.
  ENDIF.

  PERFORM refresh_alv.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  QUERY_KYTH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM query_kyth_data .
  DATA lt_matnr LIKE TABLE OF zls_matnr WITH HEADER LINE.
  DATA lt_werks LIKE TABLE OF zls_werks WITH HEADER LINE.
  FIELD-SYMBOLS <fs_kyth> LIKE LINE OF gt_kyth_data.
  REFRESH gt_kyth_data_temp.
  SELECT DISTINCT b~bhnuc
                  a~werks
                  b~matnr
                  b~charg
                  a~crdat
                  a~crnam
                  a~fhdat
                  a~fhnam
                  c~menge
                  d~thstu
    INTO CORRESPONDING FIELDS OF TABLE gt_kyth_data_temp1
    FROM zsd_jiamengfahuo AS b
    INNER JOIN zsd_jmfh_h AS a ON a~bhnuc = b~bhnuc
    INNER JOIN zmm_kyth_jy AS c ON a~bhnuc = c~bhnuc
                                AND b~bhitem = c~bhitem
    INNER JOIN zmm_kyth_h AS d ON a~bhnuc = d~bhnuc
    WHERE a~mtype = '02'
      AND a~bhnuc IN s_bhnu12
      AND a~werks IN s_werk12
      AND a~crdat IN s_crda12
      AND a~crnam IN s_crna12
      AND a~fhdat IN s_fhda12
      AND a~fhnam IN s_fhna12.
  IF gt_kyth_data_temp1 IS NOT INITIAL.
    SELECT werks
           name1 AS werks_name
    INTO TABLE lt_werks
    FROM t001w FOR ALL ENTRIES IN gt_kyth_data_temp1
    WHERE werks = gt_kyth_data_temp1-werks
      AND spras = sy-langu.

    SELECT matnr
           maktx
    INTO TABLE lt_matnr
    FROM makt FOR ALL ENTRIES IN gt_kyth_data_temp1
    WHERE matnr = gt_kyth_data_temp1-matnr
      AND spras = sy-langu.
  ENDIF.
  LOOP AT gt_kyth_data_temp1 ASSIGNING <fs_kyth>.

    READ TABLE lt_werks  WITH KEY werks = <fs_kyth>-werks.
    IF sy-subrc = 0.
      <fs_kyth>-werks_name = lt_werks-werks_name.
    ENDIF.

    READ TABLE lt_matnr WITH KEY matnr = <fs_kyth>-matnr.
    IF sy-subrc = 0.
      <fs_kyth>-maktx = lt_matnr-maktx.
    ENDIF.
    CASE <fs_kyth>-thstu.
      WHEN '1'.
        <fs_kyth>-status = '@5D@退货中'.
      WHEN '2'.
        <fs_kyth>-status = '@06@已退货'.
      WHEN OTHERS.
    ENDCASE.
    CLEAR:lt_matnr,lt_werks.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_PASS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_pass_data .
  DATA:BEGIN OF ls_bhnuc,
         bhnuc TYPE char10,
         menge TYPE menge_d,
       END OF ls_bhnuc,
       lt_bhnuc LIKE TABLE OF ls_bhnuc WITH HEADER LINE.

  LOOP AT gt_kyth_data_temp INTO gs_kyth_data.
    IF gs_kyth_data-thstu = '2'.
      MESSAGE '已经退货成功，不能继续操作' TYPE 'I'.
      return_flag = 'X'.
      EXIT.
    ENDIF.
    ls_bhnuc-bhnuc = gs_kyth_data-bhnuc.
    ls_bhnuc-menge = gs_kyth_data-menge.
    COLLECT ls_bhnuc INTO lt_bhnuc.
    CLEAR:gs_kyth_data,ls_bhnuc.
  ENDLOOP.

  IF lines( lt_bhnuc ) > 1.
    MESSAGE '一次只能操作一个退货订单' TYPE 'I'.
    return_flag = 'X'.
    RETURN.
  ENDIF.

  READ TABLE lt_bhnuc INTO ls_bhnuc INDEX 1.
  IF ls_bhnuc-menge <= 0.
    MESSAGE '输入数量必须大于0' TYPE 'I'.
    return_flag = 'X'.
    RETURN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DELETE_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_BHNUC_ODELN  text
*----------------------------------------------------------------------*
FORM frm_delete_po  TABLES pt_return TYPE bapiret2_t
                            pt_data
                           pt_kythlist STRUCTURE zmm_kythlist
                    USING p_ebeln TYPE ekko-ebeln
                          lv_change.
  DATA:ls_header TYPE bapiekkol,
       lt_item   TYPE TABLE OF bapiekpo,
       ls_item   TYPE bapiekpo,
       lt_return TYPE TABLE OF bapiret2.

  DATA:lt_itemx    TYPE TABLE OF bapimepoitemx,
       ls_itemx    TYPE bapimepoitemx,
       lt_item_set TYPE TABLE OF bapimepoitem,
       ls_item_set TYPE bapimepoitem.

  gs_kythlist-step = gs_kythlist-step + 1.
  gs_kythlist-odetp = '1'.
  gs_kythlist-odeln = p_ebeln.

  READ TABLE pt_kythlist INTO DATA(ls_kythlist) WITH KEY ogrup = gs_kythlist-ogrup
                                                         step  = gs_kythlist-step.
  CHECK sy-subrc <> 0.

  CLEAR return_flag.

  CALL FUNCTION 'BAPI_PO_GETDETAIL'    "获取采购订单信息
    EXPORTING
      purchaseorder = p_ebeln
      items         = 'X'
    IMPORTING
      po_header     = ls_header
    TABLES
      po_items      = lt_item.

  LOOP AT lt_item INTO ls_item.
    ls_item-delete_ind = 'L'.
    MOVE-CORRESPONDING ls_item TO ls_item_set.
    APPEND ls_item_set TO lt_item_set.
    ls_itemx-po_item   = ls_item-po_item.
    ls_itemx-po_itemx  = 'X'.
    ls_itemx-delete_ind = 'X'.
    APPEND ls_itemx TO lt_itemx.
    CLEAR:ls_itemx,ls_item_set.
  ENDLOOP.

  CALL FUNCTION 'BAPI_PO_CHANGE'      "修改采购订单行项目删除标识
    EXPORTING
      purchaseorder = p_ebeln
    TABLES
      return        = lt_return
      poitem        = lt_item_set
      poitemx       = lt_itemx.

  pt_return[] = lt_return[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_BHNUC_ODELN  text
*----------------------------------------------------------------------*
FORM frm_create_po TABLES pt_return TYPE bapiret2_t
                           pt_data STRUCTURE zsmm_kyth
                           pt_kythlist STRUCTURE zmm_kythlist
                   USING  lv_change
                    CHANGING p_ebeln.
  DATA:ls_header     TYPE bapimepoheader,
       ls_header_set TYPE bapimepoheader,
       ls_headerx    TYPE bapimepoheaderx,
       lt_item       TYPE TABLE OF bapimepoitem,
       ls_item       TYPE bapimepoitem,
       lt_return     TYPE bapiret2_t,
       lt_ret        TYPE TABLE OF bapiret2,
       ls_data       TYPE zsmm_kyth.

  DATA:lt_itemx    TYPE TABLE OF bapimepoitemx,
       ls_itemx    TYPE bapimepoitemx,
       lt_item_set TYPE TABLE OF bapimepoitem,
       ls_item_set TYPE bapimepoitem.

  DATA:w_struct    TYPE REF TO cl_abap_structdescr,
       it_comp_tab TYPE cl_abap_structdescr=>component_table,
       wa_comp_tab LIKE LINE OF it_comp_tab.
  FIELD-SYMBOLS:<fs_value> TYPE any.
  FIELD-SYMBOLS:<fs_xvalue> TYPE any.

  gs_kythlist-step = gs_kythlist-step + 1.
  gs_kythlist-odetp = '1'.

  READ TABLE pt_kythlist TRANSPORTING NO FIELDS WITH KEY ogrup = gs_kythlist-ogrup step = gs_kythlist-step.
  CHECK sy-subrc <> 0.

  CLEAR return_flag.

  CALL FUNCTION 'BAPI_PO_GETDETAIL1'   "获取po信息
    EXPORTING
      purchaseorder = p_ebeln
    IMPORTING
      poheader      = ls_header
    TABLES
      return        = lt_ret
      poitem        = lt_item.

  READ TABLE lt_ret TRANSPORTING NO FIELDS WITH KEY type = 'E'.
  IF sy-subrc = 0.
    MOVE-CORRESPONDING lt_ret[] TO pt_return[].
    EXIT.
  ENDIF.
  MOVE-CORRESPONDING ls_header TO ls_header_set.
  CLEAR ls_header_set-po_number.
  w_struct ?= cl_abap_structdescr=>describe_by_data( ls_header_set ).
  it_comp_tab = w_struct->get_components( ).
  LOOP AT it_comp_tab INTO wa_comp_tab.
    ASSIGN COMPONENT wa_comp_tab-name OF STRUCTURE ls_header_set TO <fs_value>.
    IF <fs_value> IS NOT INITIAL.

      ASSIGN COMPONENT  wa_comp_tab-name OF STRUCTURE ls_headerx TO <fs_xvalue>.
      IF <fs_xvalue> IS ASSIGNED.
        <fs_xvalue> = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  REFRESH it_comp_tab.
  w_struct ?= cl_abap_structdescr=>describe_by_data( ls_item_set ).
  it_comp_tab = w_struct->get_components( ).
  LOOP AT lt_item INTO ls_item.
    ls_item-ret_item = 'X'.
    MOVE-CORRESPONDING ls_item TO ls_item_set.
    LOOP AT it_comp_tab INTO wa_comp_tab.
      ASSIGN COMPONENT wa_comp_tab-name OF STRUCTURE ls_item_set TO <fs_value>.
      IF <fs_value> IS NOT INITIAL.
        ASSIGN COMPONENT wa_comp_tab-name OF STRUCTURE ls_itemx TO <fs_xvalue>.
        IF <fs_xvalue> IS ASSIGNED.
          IF wa_comp_tab-name = 'PO_ITEM'.
            <fs_xvalue> = <fs_value>.
          ELSE.
            <fs_xvalue> = 'X'.
          ENDIF.

        ENDIF.
      ENDIF.
      CLEAR wa_comp_tab.
    ENDLOOP.
    IF lv_change IS NOT INITIAL.        "如果数量已经更改
      READ TABLE pt_data INTO ls_data WITH KEY matnr = ls_item_set-material
                                               charg = ls_item_set-batch.
      IF sy-subrc = 0.
        ls_item_set-quantity = ls_data-menge.
        ls_itemx-quantity    = 'X'.
      ENDIF.
    ENDIF.

    APPEND ls_item_set TO lt_item_set.

    ls_itemx-po_itemx = 'X'.
    APPEND ls_itemx TO lt_itemx.
    CLEAR: ls_itemx,ls_item_set,ls_item,ls_data.
  ENDLOOP.

  CALL FUNCTION 'BAPI_PO_CREATE1'  "创建一个退货PO
    EXPORTING
      poheader         = ls_header_set
      poheaderx        = ls_headerx
    IMPORTING
      exppurchaseorder = gs_kythlist-odeln
    TABLES
      return           = lt_return
      poitem           = lt_item_set
      poitemx          = lt_itemx.

  p_ebeln = gs_kythlist-odeln.
*  pt_return = lt_return.
  pt_return[] = CORRESPONDING #( lt_return[] ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_SO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RETURN  text
*      -->P_GT_DATA  text
*      -->P_LS_BHNUC_ODELN  text
*----------------------------------------------------------------------*
FORM frm_copy_so  TABLES   p_return TYPE bapiret2_t
                             p_data STRUCTURE zsmm_kyth
                             pt_kythlist STRUCTURE zmm_kythlist
                    CHANGING  p_vbeln.

  DATA:lt_return TYPE TABLE OF bapiret2,
       ls_return TYPE bapiret2.

  DATA:lv_vbeln_ex TYPE vbak-vbeln.
  DATA:lv_flag TYPE c.

  gs_kythlist-step = gs_kythlist-step + 1.
  gs_kythlist-odetp = '2'.

  READ TABLE pt_kythlist TRANSPORTING NO FIELDS WITH KEY ogrup = gs_kythlist-ogrup step = gs_kythlist-step.
  CHECK sy-subrc <> 0.

  CLEAR return_flag.
  "复制销售订单
  CALL FUNCTION 'BAPI_SALESDOCUMENT_COPY'
    EXPORTING
      salesdocument    = p_vbeln
      documenttype     = 'ZB45'
    IMPORTING
      salesdocument_ex = lv_vbeln_ex
    TABLES
      return           = lt_return.
  LOOP AT lt_return INTO ls_return WHERE type = 'E'.
    lv_flag = 'X'.
  ENDLOOP.
  IF return_flag IS NOT INITIAL.
    p_return[] = lt_return[].
    EXIT.
  ENDIF.
  gs_kythlist-odeln = lv_vbeln_ex.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DELETE_SO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RETURN  text
*      -->P_GT_DATA  text
*      -->P_LS_BHNUC_ODELN  text
*----------------------------------------------------------------------*
FORM frm_delete_so  TABLES   p_return TYPE bapiret2_t
                             p_gt_data
                             pt_kythlist STRUCTURE zmm_kythlist
                    USING    p_vbeln .



  "删除 so
  DATA: i_headr  TYPE bapisdhead1,
        i_headrx TYPE bapisdhead1x,
        i_return TYPE bapiret2 OCCURS 0 WITH HEADER LINE,
        i_item   TYPE bapisditem OCCURS 0 WITH HEADER LINE,
        i_itemx  TYPE bapisditemx  OCCURS 0 WITH HEADER LINE.

  gs_kythlist-step = gs_kythlist-step + 1.
  gs_kythlist-odetp = '2'.
  READ TABLE pt_kythlist TRANSPORTING NO FIELDS WITH KEY ogrup = gs_kythlist-ogrup step = gs_kythlist-step.
  CHECK sy-subrc <> 0.

  CLEAR return_flag.
  gs_kythlist-odeln = p_vbeln.

  REFRESH: i_item, i_itemx, i_return.
  CLEAR: i_headr, i_headrx.
  i_headr-zlsddzt  = 'X'.
  i_headrx-zlsddzt  = 'X'.
  i_headrx-updateflag = 'U'.

  SELECT posnr
   INTO TABLE @DATA(lt_vbap)
   FROM vbap WHERE vbeln = @p_vbeln.

  LOOP AT lt_vbap INTO DATA(ls_vbap).
    i_item-itm_number = ls_vbap-posnr.
    i_item-reason_rej = 'Z1'.


    i_itemx-updateflag = 'U'.
    i_itemx-itm_number = ls_vbap-posnr.
    i_itemx-reason_rej = 'X'.
    APPEND i_item.
    APPEND i_itemx.
  ENDLOOP.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument     = p_vbeln
      order_header_in   = i_headr
      order_header_inx  = i_headrx
      behave_when_error = 'P'
    TABLES
      return            = i_return
      order_item_in     = i_item
      order_item_inx    = i_itemx.

  p_return[] = i_return[].

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REVERSE_STO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RETURN  text
*      -->P_GT_DATA  text
*      -->P_LS_BHNUC_ODELN  text
*----------------------------------------------------------------------*
FORM frm_reverse_sto  TABLES   p_return TYPE bapiret2_t
                               p_gt_data
                               pt_kythlist STRUCTURE zmm_kythlist
                      USING    p_ebeln.

  DATA:ls_return TYPE bapiret2.
  DATA:ls_mkpf TYPE mkpf.

  gs_kythlist-step = gs_kythlist-step + 1.
  gs_kythlist-odetp = '4'.

  READ TABLE pt_kythlist TRANSPORTING NO FIELDS WITH KEY ogrup = gs_kythlist-ogrup step = gs_kythlist-step.
  CHECK sy-subrc <> 0.

  CLEAR return_flag.

  SELECT SINGLE vbeln
  INTO @DATA(lv_vbeln)
  FROM lips
  WHERE vgbel = @p_ebeln.
  IF sy-subrc = 0.
    SELECT SINGLE vbtyp                                                  "SD 凭证类别
    FROM likp
    INTO @DATA(lv_vbtyp)
    WHERE vbeln = @lv_vbeln.

    CALL FUNCTION 'WS_REVERSE_GOODS_ISSUE'
      EXPORTING
        i_vbeln                   = lv_vbeln
        i_budat                   = sy-datum
        i_tcode                   = 'VL09'
        i_vbtyp                   = lv_vbtyp
      IMPORTING
        es_emkpf                  = ls_mkpf
      EXCEPTIONS
        error_reverse_goods_issue = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      ls_return-type = sy-msgty.
      ls_return-id   = sy-msgid.
      ls_return-number = sy-msgno.
      ls_return-message_v1 = sy-msgv1.
      ls_return-message_v2 = sy-msgv2.
      ls_return-message_v3 = sy-msgv3.
      ls_return-message_v4 = sy-msgv4.
      APPEND ls_return TO p_return.
    ENDIF.
    gs_kythlist-odeln = ls_mkpf-mblnr.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_STO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RETURN  text
*      -->P_GT_DATA  text
*      -->P_LS_BHNUC_ODELN  text
*----------------------------------------------------------------------*
FORM frm_create_sto  TABLES   p_return TYPE bapiret2_t
                              p_gt_data
                              pt_kythlist STRUCTURE zmm_kythlist
                     USING    p_ebeln
                              p_bhnuc TYPE ztrade_odlist.
  DATA lt_request TYPE TABLE OF bapideliciousrequest.
  DATA lt_items TYPE TABLE OF bapideliciouscreateditems.
  DATA lt_return TYPE TABLE OF bapiret2.
  DATA ls_return TYPE bapiret2.

  gs_kythlist-step = gs_kythlist-step + 1.
  gs_kythlist-odetp = '3'.

  READ TABLE pt_kythlist TRANSPORTING NO FIELDS WITH KEY ogrup = gs_kythlist-ogrup step = gs_kythlist-step.
  CHECK sy-subrc <> 0.

  CLEAR return_flag.
*
*    READ TABLE it_thlist INTO DATA(ls_odlist) WITH KEY ogrup = active_thlist-ogrup step = 001.

  DO 3 TIMES.

    SELECT a~ebeln AS document_numb a~ebelp AS document_item a~matnr AS material
           a~menge AS quantity_sales_uom a~meins AS sales_unit b~charg AS batch
      INTO CORRESPONDING FIELDS OF TABLE lt_request
      FROM ekpo AS a
      INNER JOIN eket AS b ON a~ebeln = b~ebeln AND b~ebelp = a~ebelp
      WHERE a~ebeln = p_ebeln.

    IF lt_request[] IS NOT INITIAL.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.

  SELECT SINGLE lgort
  INTO @DATA(lv_lgort)
  FROM lips
  WHERE vbeln = @p_bhnuc-odeln.

  IF lt_request[] IS NOT INITIAL.

    LOOP AT lt_request INTO DATA(ls_request).
      ls_request-document_type = 'B'.
      ls_request-delivery_date = sy-datum.
      ls_request-delivery_time = sy-uzeit.

      ls_request-stge_loc = lv_lgort.
      MODIFY lt_request FROM ls_request.
    ENDLOOP.

    CALL FUNCTION 'BAPI_DELIVERYPROCESSING_EXEC'
      TABLES
        request      = lt_request
        createditems = lt_items
        return       = lt_return.

    IF lines( lt_items[] ) <> lines( lt_request[] ) AND lines( lt_items[] ) > 0.
      ls_return-id = '00'.
      ls_return-type = 'E'.
      ls_return-number = '001'.
      ls_return-message_v1 = '创建的交货单行项目数量错误，可能是物料数据被锁定导致'.
      APPEND ls_return TO lt_return.
    ELSE.

      READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
      IF sy-subrc <> 0.
        gs_kythlist-odeln = lt_items[ 1 ]-document_numb.
      ENDIF.
    ENDIF.

  ELSE.
    ls_return-id = '00'.
    ls_return-type = 'E'.
    ls_return-number = '001'.
    ls_return-message_v1 = '创建PO-STO时无行项目数据'.
    APPEND ls_return TO lt_return.
  ENDIF.
  p_return[] = lt_return[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_BILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RETURN  text
*      -->P_GT_DATA  text
*      -->P_LS_BHNUC_ODELN  text
*----------------------------------------------------------------------*
FORM frm_create_bill  TABLES   p_return TYPE bapiret2_t
                               p_gt_data
                               pt_kythlist STRUCTURE zmm_kythlist
                      USING    p_bhnuc TYPE ztrade_odlist
                               p_jmfh_h TYPE zsd_jmfh_h.

  DATA:
    BEGIN OF ls_belnr_relation,
      from_bukrs  TYPE bukrs,
      from_werks  TYPE werks_d,
      from_kunnr  TYPE kunnr,
      from_lifnr  TYPE lifnr,
      to_werks    TYPE werks_d,
      to_kunnr    TYPE kunnr,
      to_lifnr    TYPE lifnr,
      ebeln       TYPE ebeln,
      werks       TYPE werks_d,
      werks_name  TYPE name1,
      werks_kunnr TYPE kunnr,
      all_amount  TYPE dmbtr,
    END OF ls_belnr_relation .
  DATA ls_yhbel_list TYPE zmm_yhbel_list.
  DATA ls_kythlist   TYPE zmm_kythlist.
  DATA lt_return     TYPE TABLE OF bapiret2.

  DATA lt_belnr_item TYPE zbelnr_item.
  DATA lt_item TYPE TABLE OF zsd003_8.
  DATA ls_item TYPE zsd003_8.

  DATA lines TYPE i.

  DATA o_type TYPE char1.
  DATA o_tmsg TYPE bapi_msg.
  DATA belnr TYPE belnr_d.
  DATA lv_kbetr TYPE konv-kbetr.
  DATA lv_kawrt TYPE konv-kawrt.

  gs_kythlist-step = gs_kythlist-step + 1.
  gs_kythlist-odetp = '5'.

  READ TABLE pt_kythlist TRANSPORTING NO FIELDS WITH KEY ogrup = gs_kythlist-ogrup step = gs_kythlist-step.
  CHECK sy-subrc <> 0.

  CLEAR return_flag.

  CLEAR ls_belnr_relation.

  SELECT SINGLE *
  INTO @DATA(ls_bkpf)
  FROM bkpf
  WHERE belnr = @p_bhnuc-odeln
    AND bukrs = @p_bhnuc-bukrs
    AND gjahr = @p_bhnuc-odjhr.



  READ TABLE gt_kythlist INTO ls_kythlist WITH KEY ogrup = gs_kythlist-ogrup
                                                   odetp = '2'.

  SELECT * INTO TABLE @DATA(lt_kyth_data) FROM zmm_kythlist
  WHERE bhnuc = @ls_kythlist-odeln  AND ogrup = 001 ORDER BY ogrup ,step.

  SELECT  SINGLE netwr
  INTO ls_belnr_relation-all_amount
  FROM vbak
  WHERE vbak~vbeln = ls_kythlist-odeln.

  ls_belnr_relation-ebeln = ls_kythlist-odeln.

  ls_belnr_relation-werks = p_jmfh_h-werks.
  ls_belnr_relation-from_werks = p_jmfh_h-fwerk.
  ls_belnr_relation-from_bukrs = ls_bkpf-bukrs.
  ls_belnr_relation-to_werks = p_jmfh_h-werks.

  " get belnr


  SELECT SINGLE name1 INTO @ls_belnr_relation-werks_name FROM t001w WHERE werks = @ls_belnr_relation-werks.
  SELECT SINGLE kunn2 INTO @ls_belnr_relation-werks_kunnr FROM knvp WHERE kunnr = @ls_belnr_relation-werks AND parvw = 'RE'.



  SELECT SINGLE kunn2 INTO ls_belnr_relation-from_kunnr
    FROM t001w AS a INNER JOIN knvp AS b ON a~kunnr = b~kunnr
    WHERE a~werks = ls_belnr_relation-from_werks AND b~parvw = 'RE'.

  SELECT SINGLE kunn2 INTO ls_belnr_relation-to_kunnr
    FROM t001w AS a INNER JOIN knvp AS b ON a~kunnr = b~kunnr
    WHERE a~werks = ls_belnr_relation-to_werks AND b~parvw = 'RE'.

*  SELECT SINGLE lifn2 INTO ls_belnr_relation-from_lifnr
*    FROM t001w AS a INNER JOIN wyt3 AS b ON a~lifnr = b~lifnr
*    WHERE a~werks = ls_belnr_relation-from_werks AND b~parvw = 'LF'.
*
*  SELECT SINGLE lifn2 INTO ls_belnr_relation-to_lifnr
*    FROM t001w AS a INNER JOIN wyt3 AS b ON a~lifnr = b~lifnr
*    WHERE a~werks = ls_belnr_relation-to_werks AND b~parvw = 'LF'.

  ls_item-bukrs = p_bhnuc-bukrs.
  ls_item-newko = ls_belnr_relation-werks_kunnr.
  ls_item-ntype = '1'.
  ls_item-hkont = '1122020300'.
  ls_item-bschl = '01'.
  ls_item-amount = ls_belnr_relation-all_amount.
  ls_item-linet = |{ ls_belnr_relation-werks }-{ ls_belnr_relation-werks_name }-{ ls_belnr_relation-ebeln }-非商品退货 |.
  APPEND ls_item TO lt_item.

  ls_item-bukrs = p_bhnuc-bukrs.
  ls_item-newko = ls_belnr_relation-to_kunnr.
  ls_item-ntype = '1'.
  ls_item-hkont = '1122020100'.
  ls_item-bschl = '11'.
  ls_item-amount = 0 - ls_belnr_relation-all_amount.
  ls_item-linet = |{ ls_belnr_relation-werks }-{ ls_belnr_relation-werks_name }-{ ls_belnr_relation-ebeln }-非商品退货 |.
  APPEND ls_item TO lt_item.


*    ENDIF.

*  lines = lines( lt_kyth_data[] ).

*  DELETE lt_kyth_data WHERE odetp <> 005 OR fwerk <> ls_belnr_relation-from_werks.
*
*
*  READ TABLE lt_kyth_data TRANSPORTING NO FIELDS INDEX 1.
*  IF sy-subrc <> 0 AND ls_belnr_relation-all_amount > 0.

  CALL FUNCTION 'ZISD003_8'
    EXPORTING
      i_xlenr   = ls_belnr_relation-ebeln
      i_bukrs   = ls_belnr_relation-from_bukrs
    IMPORTING
      o_type    = o_type
      o_tmsg    = o_tmsg
      o_belnr   = belnr
    TABLES
      it_item   = lt_item[]
      it_return = lt_return[].

  IF o_type = 'E'.
    p_return[] = lt_return[].
    EXIT.
  ENDIF.

  ls_yhbel_list-jmbln = ls_belnr_relation-ebeln.
  ls_yhbel_list-belnr = belnr.
  ls_yhbel_list-werks = ls_belnr_relation-werks.
  ls_yhbel_list-jmtyp = '6'.
  ls_yhbel_list-bukrs = ls_belnr_relation-from_bukrs.
  ls_yhbel_list-kunnr = ls_belnr_relation-werks_kunnr.
  ls_yhbel_list-gjahr = sy-datum+0(4).
  ls_yhbel_list-btype = '11'.
  ls_yhbel_list-amount = 0 - ls_belnr_relation-all_amount.
  ls_yhbel_list-crdat = sy-datum.
  ls_yhbel_list-crtim = sy-uzeit.

*    lines = gs_kythlist-step + 1.
*    ls_kythlist-bhnuc = ls_belnr_relation-ebeln.
*    ls_kythlist-bukrs = ls_belnr_relation-from_bukrs.
*    ls_kythlist-ogrup = 001.
*    ls_kythlist-step = lines.
*    ls_kythlist-odetp = '5'.
*    ls_kythlist-odeln = belnr.
*    ls_kythlist-odjhr = sy-datum+0(4).
*    ls_kythlist-fwerk = ls_belnr_relation-from_werks.
*    ls_kythlist-crdat = sy-datum.
*    ls_kythlist-crtim = sy-uzeit.
*    ls_kythlist-crnam = sy-uname.

*    MODIFY zmm_kythlist   FROM ls_kythlist.
  MODIFY zmm_yhbel_list FROM ls_yhbel_list.

  gs_kythlist-odeln = belnr.
  gs_kythlist-bukrs = ls_belnr_relation-from_bukrs.

*    COMMIT WORK.

*  ENDIF.

*  LOOP AT lt_belnr_item INTO ls_item.
*
*    APPEND ls_item TO lt_item.
*
*    IF lines( lt_item[] ) = 2.
*
*      READ TABLE lt_kyth_data TRANSPORTING NO FIELDS INDEX 1.
*      IF sy-subrc = 0.
*        REFRESH lt_item.
*        DELETE lt_kyth_data INDEX 1.
*        CONTINUE.
*      ELSEIF ls_belnr_relation-all_amount > 0.
*
*        CALL FUNCTION 'ZISD003_8'
*          EXPORTING
*            i_xlenr   = ls_belnr_relation-ebeln
*            i_bukrs   = ls_item-bukrs
*          IMPORTING
*            o_type    = o_type
*            o_tmsg    = o_tmsg
*            o_belnr   = belnr
*          TABLES
*            it_item   = lt_item[]
*            it_return = lt_return[].
*
*        IF o_type = 'E'.
*
*          p_return[] = lt_return[].
*          EXIT.
*        ELSE.
*
*          ls_yhbel_list-jmbln = ls_belnr_relation-ebeln.
*          ls_yhbel_list-belnr = belnr.
*          ls_yhbel_list-werks = ls_belnr_relation-werks.
*          ls_yhbel_list-jmtyp = '6'.
*          ls_yhbel_list-bukrs = ls_item-bukrs.
*          ls_yhbel_list-kunnr = ls_belnr_relation-werks_kunnr.
*          ls_yhbel_list-gjahr = sy-datum+0(4).
*          ls_yhbel_list-amount = 0 - ls_belnr_relation-all_amount.
*          ls_yhbel_list-btype = 11.
*          ls_yhbel_list-crdat = sy-datum.
*          ls_yhbel_list-crtim = sy-uzeit.
*
*          lines = lines + 1.
*          ls_kythlist-bhnuc = ls_belnr_relation-ebeln.
*          ls_kythlist-bukrs = ls_item-bukrs.
*          ls_kythlist-ogrup = 001.
*          ls_kythlist-step = lines.
*          ls_kythlist-odeln = belnr.
*          ls_kythlist-odetp = '5'.
*          ls_kythlist-odjhr = sy-datum+0(4).
*          ls_kythlist-fwerk = ls_belnr_relation-from_werks.
*          ls_kythlist-crdat = sy-datum.
*          ls_kythlist-crtim = sy-uzeit.
*          ls_kythlist-crnam = sy-uname.
*
*          MODIFY zmm_kythlist FROM ls_kythlist.
*          MODIFY zmm_yhbel_list FROM ls_yhbel_list.
*
*          COMMIT WORK.
*
*          CLEAR ls_item.
*          REFRESH lt_item.
*        ENDIF.
*
*      ENDIF.
*    ENDIF.
*
*  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_DN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RETURN  text
*      -->P_GT_DATA  text
*      -->P_LV_VBELN  text
*----------------------------------------------------------------------*
FORM frm_create_dn  TABLES   p_return TYPE bapiret2_t
                             p_gt_data
                             pt_kythlist STRUCTURE zmm_kythlist
                    USING    p_vbeln.

  "创建DN
  DATA lt_request TYPE TABLE OF bapideliciousrequest.
  DATA ls_request TYPE bapideliciousrequest.
  DATA lt_createditems TYPE TABLE OF bapideliciouscreateditems.
  DATA lt_return  TYPE TABLE OF bapiret2.
  DATA ls_return  TYPE bapiret2.
  DATA lv_vbeln TYPE vbak-vbeln.

  DATA ls_kythlist TYPE zmm_kythlist.

  DATA line1 TYPE i.
  DATA line2 TYPE i.

  gs_kythlist-step = gs_kythlist-step + 1.
  gs_kythlist-odetp = '3'.

  READ TABLE pt_kythlist TRANSPORTING NO FIELDS WITH KEY ogrup = gs_kythlist-ogrup step = gs_kythlist-step.
  CHECK sy-subrc <> 0.

  READ TABLE gt_kythlist INTO ls_kythlist WITH KEY ogrup = gs_kythlist-ogrup odetp = 002.
  lv_vbeln = ls_kythlist-odeln.

  CLEAR return_flag.

  DO 3 TIMES.

    SELECT vbeln AS document_numb, posnr AS document_item, kwmeng AS quantity_sales_uom, meins AS sales_unit,
           werks AS plant, 'A' AS document_type
      INTO CORRESPONDING FIELDS OF TABLE @lt_request
      FROM vbap
      WHERE vbeln = @lv_vbeln.

    IF lt_request[] IS NOT INITIAL.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.

  SELECT vbeln,
         posnr,
         lgort
  INTO TABLE @DATA(lt_lips)
  FROM lips
  WHERE vgbel = @p_vbeln.

  LOOP AT lt_request INTO ls_request.
    READ TABLE lt_lips INTO DATA(ls_lips) WITH KEY posnr = ls_request-document_item.
    IF sy-subrc = 0.
      ls_request-stge_loc = ls_lips-lgort.
    ENDIF.
    MODIFY lt_request FROM ls_request TRANSPORTING stge_loc.
    CLEAR:ls_lips.
  ENDLOOP.

  IF lt_request[] IS NOT INITIAL.

    line1 = lines( lt_request[] ).

    CALL FUNCTION 'BAPI_DELIVERYPROCESSING_EXEC'
      TABLES
        request      = lt_request
        createditems = lt_createditems
        return       = lt_return.

    line2 = lines( lt_createditems[] ).

    IF line1 <> line2 AND line2 > 0.
      REFRESH lt_return.
      ls_return-id = '00'.
      ls_return-type = 'E'.
      ls_return-number = '001'.
      ls_return-message_v1 = '物料正在被其他人操作中，已锁定，请稍后再试'.
      APPEND ls_return TO lt_return.

    ELSE.

      READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
      IF sy-subrc <> 0.
        gs_kythlist-odeln = lt_createditems[ 1 ]-document_numb.   "创建成功
      ENDIF.

    ENDIF.
  ELSE.
    ls_return-id = '00'.
    ls_return-type = 'E'.
    ls_return-number = '001'.
    ls_return-message_v1 = '创建SO-DN时无行项目数据'.
    APPEND ls_return TO lt_return.
  ENDIF.
  p_return[] = lt_return[] .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_MIGO_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RETURN  text
*      -->P_GT_DATA  text
*      -->P_LS_BHNUC_ODELN  text
*----------------------------------------------------------------------*
FORM frm_migo_po  TABLES   p_return TYPE bapiret2_t
                           p_gt_data
                           pt_kythlist STRUCTURE zmm_kythlist
                  USING    p_bhunc TYPE ztrade_odlist.
  DATA ls_header TYPE bapi2017_gm_head_01.
  DATA lt_item TYPE TABLE OF bapi2017_gm_item_create.
  DATA ls_item TYPE bapi2017_gm_item_create.
  DATA:lt_return TYPE TABLE OF bapiret2.
  DATA ls_return TYPE bapiret2.
  DATA ls_ebeln TYPE ekko-ebeln.

  DATA temp_mblnr TYPE bapi2017_gm_head_ret.

  gs_kythlist-step = gs_kythlist-step + 1.
  gs_kythlist-odetp = '4'.

  READ TABLE pt_kythlist TRANSPORTING NO FIELDS WITH KEY ogrup = gs_kythlist-ogrup step = gs_kythlist-step.
  CHECK sy-subrc <> 0.

  READ TABLE gt_kythlist INTO DATA(ls_kythlist) WITH KEY ogrup = gs_kythlist-ogrup odetp = 1.
  ls_ebeln = ls_kythlist-odeln.

  CLEAR return_flag.

  SELECT ebeln ,ebelp ,lgort INTO TABLE @DATA(lt_lgort) FROM mseg WHERE ebeln = @p_bhunc-odeln
                                                       AND bwart = '101'.

  DO 3 TIMES.

    SELECT DISTINCT a~ebeln AS po_number a~ebelp AS po_item a~matnr AS material
           a~werks AS plant a~menge AS entry_qnt b~charg AS batch
      INTO CORRESPONDING FIELDS OF TABLE lt_item
      FROM ekpo AS a
      INNER JOIN eket AS b ON a~ebeln = b~ebeln AND a~ebelp = b~ebelp
      WHERE a~ebeln = ls_ebeln.

    IF lt_item[] IS NOT INITIAL.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.

  IF lt_item[] IS NOT INITIAL.
    LOOP AT lt_item INTO ls_item.
      READ TABLE lt_lgort INTO DATA(ls_lgort) WITH KEY ebelp = ls_item-po_item.
      IF sy-subrc = 0.
        ls_item-stge_loc = ls_lgort-lgort.
      ENDIF.
      MODIFY lt_item FROM ls_item TRANSPORTING stge_loc.
      CLEAR:ls_item.
    ENDLOOP.

    ls_header-pstng_date = sy-datum.
    ls_header-doc_date = sy-datum.

    ls_item-move_type = '101'.
    ls_item-mvt_ind = 'B'.
    MODIFY lt_item FROM ls_item TRANSPORTING move_type mvt_ind WHERE move_type IS INITIAL.

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = ls_header
        goodsmvt_code    = '01'
      IMPORTING
        goodsmvt_headret = temp_mblnr
      TABLES
        goodsmvt_item    = lt_item
        return           = lt_return.

    gs_kythlist-odeln = temp_mblnr+0(10).

  ELSE.

    ls_return-id = '00'.
    ls_return-type = 'E'.
    ls_return-number = '001'.
    ls_return-message_v1 = 'PO过账时无行项目数据'.
    APPEND ls_return TO lt_return.

  ENDIF.
  p_return[] = lt_return[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_POST_DN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RETURN  text
*      -->P_GT_DATA  text
*      -->P_LV_VBELN  text
*----------------------------------------------------------------------*
FORM frm_post_dn  TABLES   p_return TYPE bapiret2_t
                           p_gt_data
                           pt_kythlist STRUCTURE zmm_kythlist.
  "post dn
  DATA ls_header TYPE bapiobdlvhdrcon.
  DATA ls_header_control TYPE bapiobdlvhdrctrlcon.
  DATA fieldname TYPE fieldname VALUE '(SAPLV50S)EMKPF-MBLNR'.
  DATA lt_return TYPE TABLE OF bapiret2.
  DATA lv_vbeln TYPE likp-vbeln.
  DATA ls_kythlist TYPE zmm_kythlist.

  gs_kythlist-step = gs_kythlist-step + 1.
  gs_kythlist-odetp = '4'.

  READ TABLE pt_kythlist TRANSPORTING NO FIELDS WITH KEY ogrup = gs_kythlist-ogrup step = gs_kythlist-step.
  CHECK sy-subrc <> 0.

  READ TABLE gt_kythlist INTO ls_kythlist WITH KEY ogrup = gs_kythlist-ogrup odetp = 3.
  lv_vbeln = ls_kythlist-odeln.

  CLEAR return_flag.

  ls_header-deliv_numb = lv_vbeln.

  ls_header_control-deliv_numb = lv_vbeln.
  ls_header_control-post_gi_flg = 'X'.


  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CONFIRM_DEC'
    EXPORTING
      header_data    = ls_header
      header_control = ls_header_control
      delivery       = lv_vbeln
    TABLES
      return         = lt_return.


  IF lt_return[] IS INITIAL.
    ASSIGN (fieldname) TO FIELD-SYMBOL(<value>).
    IF <value> IS ASSIGNED.
      gs_kythlist-odeln = <value>.
    ENDIF.
  ENDIF.
  p_return[] = lt_return[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_POST_STO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RETURN  text
*      -->P_GT_DATA  text
*      -->P_LS_BHNUC_ODELN  text
*----------------------------------------------------------------------*
FORM frm_post_sto  TABLES   p_return TYPE bapiret2_t
                            p_gt_data
                            pt_kythlist STRUCTURE zmm_kythlist.
  DATA ls_header TYPE bapiobdlvhdrcon.
  DATA ls_header_control TYPE bapiobdlvhdrctrlcon.
  DATA fieldname TYPE fieldname VALUE '(SAPLV50S)EMKPF-MBLNR'.
  DATA lt_return TYPE TABLE OF bapiret2.
  DATA lv_vbeln_sto TYPE likp-vbeln.

  gs_kythlist-step = gs_kythlist-step + 1.
  gs_kythlist-odetp = '4'.

  READ TABLE pt_kythlist TRANSPORTING NO FIELDS WITH KEY ogrup = gs_kythlist-ogrup step = gs_kythlist-step.
  CHECK sy-subrc <> 0.
  READ TABLE gt_kythlist INTO DATA(ls_kythlist) WITH KEY ogrup = gs_kythlist-ogrup step = 002.
  lv_vbeln_sto = ls_kythlist-odeln.

  CLEAR return_flag.

  ls_header-deliv_numb = lv_vbeln_sto.

  ls_header_control-deliv_numb = lv_vbeln_sto.
  ls_header_control-post_gi_flg = 'X'.


  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CONFIRM_DEC'
    EXPORTING
      header_data    = ls_header
      header_control = ls_header_control
      delivery       = lv_vbeln_sto
    TABLES
      return         = lt_return.

  IF lt_return[] IS INITIAL.
    ASSIGN (fieldname) TO FIELD-SYMBOL(<value>).
    IF <value> IS ASSIGNED.
      gs_kythlist-odeln = <value>.

*        IF last_one = 'X'.
*          active_thlist-spflg = '1'.
*        ENDIF.
    ENDIF.
  ELSE.
    p_return[] = lt_return[].
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DELETE_DN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RETURN  text
*      -->P_GT_DATA  text
*      -->P_LV_VBELN_DN  text
*----------------------------------------------------------------------*
FORM frm_delete_dn  TABLES   p_return TYPE bapiret2_t
                             p_gt_data
                             pt_kythlist STRUCTURE zmm_kythlist
                    USING    p_vbeln_dn.
*---declaration of BAPI
  DATA: wa_hdata  LIKE bapiobdlvhdrchg,
        wa_hcont  LIKE bapiobdlvhdrctrlchg,
        d_delivy  LIKE bapiobdlvhdrchg-deliv_numb,
        lt_return TYPE bapiret2 OCCURS 0 WITH HEADER LINE.

  gs_kythlist-step = gs_kythlist-step + 1.
  gs_kythlist-odetp = '3'.
  READ TABLE pt_kythlist TRANSPORTING NO FIELDS WITH KEY ogrup = gs_kythlist-ogrup step = gs_kythlist-step.
  CHECK sy-subrc <> 0.

  CLEAR return_flag.

  IF p_vbeln_dn IS NOT INITIAL.

    gs_kythlist-odeln = p_vbeln_dn.
    " 删除 dn
    CLEAR: wa_hdata, wa_hcont, d_delivy, lt_return.
    wa_hdata-deliv_numb = p_vbeln_dn.
    wa_hcont-deliv_numb = p_vbeln_dn.
    wa_hcont-dlv_del    = abap_true.
    d_delivy            = p_vbeln_dn.
*      ---Deleting delivery doc (VL02)
    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        header_data    = wa_hdata
        header_control = wa_hcont
        delivery       = d_delivy
      TABLES
        return         = lt_return.
  ENDIF.
  p_return[] = lt_return[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHANGE_SO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RETURN  text
*      -->P_GT_DATA  text
*      -->P_LV_VBELN  text
*----------------------------------------------------------------------*
FORM frm_create_so  TABLES   p_return TYPE bapiret2_t
                             p_data STRUCTURE zsmm_kyth
                             pt_kythlist STRUCTURE zmm_kythlist
                    USING    p_bhnuc
                             p_vbeln.

  DATA ls_header TYPE bapisdhd1.
  DATA ls_headerx TYPE bapisdhd1x.
  DATA lt_partner TYPE TABLE OF bapiparnr.

  DATA lt_item TYPE TABLE OF bapisditm.
  DATA lt_schedules TYPE TABLE OF bapischdl.
  DATA lt_schedulesx TYPE TABLE OF bapischdlx.
  DATA lt_condition TYPE TABLE OF bapicond.
  DATA lt_conditionx TYPE TABLE OF bapicondx.

  DATA ls_partner TYPE bapiparnr.
  DATA ls_item TYPE bapisditm.
  DATA ls_schedules TYPE bapischdl.
  DATA ls_schedulesx TYPE bapischdlx.
  DATA ls_condition TYPE bapicond.
  DATA ls_conditionx TYPE bapicondx.

  DATA:lt_itemx TYPE TABLE OF bapisditmx,
       ls_itemx TYPE bapisditmx.

  DATA:lt_return TYPE TABLE OF bapiret2.

  DATA serv_date TYPE sy-datum.


  gs_kythlist-step = gs_kythlist-step + 1.
  gs_kythlist-odetp = '2'.

  READ TABLE pt_kythlist TRANSPORTING NO FIELDS WITH KEY ogrup = gs_kythlist-ogrup step = gs_kythlist-step.
  CHECK sy-subrc <> 0.

  CLEAR return_flag.

*    READ TABLE it_thlist TRANSPORTING NO FIELDS WITH KEY ogrup = active_thlist-ogrup step = 001.
*    CHECK sy-subrc <> 0.

  ls_header-doc_type = 'ZB45'.
  ls_header-sales_org = active_path-from_bukrs.
  ls_header-distr_chan = 'T1'.
  ls_header-division = '00'.
  SELECT SINGLE vkbur_wrk INTO ls_header-sales_off FROM wrf1 WHERE locnr = werks.

  ls_headerx-updateflag = 'I'.
  ls_headerx-doc_type = 'X'.
  ls_headerx-sales_org = 'X'.
  ls_headerx-division = 'X'.
  ls_headerx-sales_off = 'X'.

  SELECT vbak~vbeln ,
         posnr ,
         matnr ,
         charg ,
         werks ,
         vkorg ,
         lgort ,
         konv~knumv ,
         kbetr
  INTO TABLE @DATA(lt_vbap)
  FROM vbak INNER JOIN vbap
    ON vbak~vbeln = vbap~vbeln
            INNER JOIN konv
    ON vbak~knumv = konv~knumv

  WHERE vbak~vbeln = @p_vbeln
    AND konv~kschl = 'ZPR6'.


  SELECT parvw AS partn_role kunn2 AS partn_numb INTO TABLE lt_partner
    FROM knvp
    WHERE kunnr = gs_kythlist-fwerk AND vkorg = active_path-from_bukrs AND vtweg = 'T0' AND spart = '00'.


  LOOP AT lt_vbap INTO DATA(ls_vbap).

*      IF sy-tabix = 1.
*        SELECT ebelp, mwskz INTO TABLE @DATA(lt_mwskz) FROM ekpo WHERE ebeln = @ls_data-ebeln.
*      ENDIF.
    READ TABLE p_data INTO DATA(ls_data) WITH KEY matnr = ls_vbap-matnr
                                                   charg = ls_vbap-charg.
    IF sy-subrc = 0.
      ls_item-target_qty = ls_data-menge.
    ENDIF.
    ls_item-itm_number = ls_vbap-posnr.
    ls_item-material   = ls_vbap-matnr.
    ls_item-batch      = ls_vbap-charg.
    ls_item-plant      = ls_vbap-werks.
    ls_item-store_loc  = ls_vbap-lgort.
    ls_item-ref_doc    = ls_vbap-vbeln.
    ls_item-ref_doc_it = ls_vbap-posnr.
    ls_item-ref_doc_ca = 'C'.

    APPEND ls_item TO lt_item.

    ls_itemx-itm_number  = ls_vbap-posnr.
    ls_itemx-material    = 'X'.
    ls_itemx-batch       = 'X'.
    ls_itemx-plant       = 'X'.
    ls_itemx-store_loc   = 'X'.
    ls_itemx-ref_doc     = 'X'.
    ls_itemx-ref_doc_it  = 'X'.
    ls_itemx-ref_doc_ca  = 'X'.

    APPEND ls_itemx TO lt_itemx.
*      READ TABLE lt_mwskz INTO DATA(ls_mwskz) WITH KEY ebelp = ls_data-ebelp.
*      IF sy-subrc = 0 AND ( ls_mwskz-mwskz = 'J1' OR ls_mwskz = space ).
*        serv_date = '20180101'.
*        ls_item-serv_date = serv_date.
*      ELSE.
    serv_date = space.
*      ENDIF.

    APPEND ls_item TO lt_item.

    ls_schedules-itm_number = ls_vbap-posnr.
    ls_schedules-sched_line = 0001.
    ls_schedules-req_date = sy-datum.
    ls_schedules-req_qty = ls_item-target_qty.
    ls_schedules-date_type = '1'.
    ls_schedules-sched_type = 'DN'.
    ls_schedules-dlv_date = sy-datum.
    ls_schedules-load_date = sy-datum.
    ls_schedules-gi_date = sy-datum.
    ls_schedules-tp_date = sy-datum.
    ls_schedules-ms_date = sy-datum.
    ls_schedules-tp_time = sy-uzeit.
    ls_schedules-ms_time = sy-uzeit.
    ls_schedules-load_time = sy-uzeit.
    ls_schedules-gi_time = sy-uzeit.
    ls_schedules-req_time = sy-uzeit.
    ls_schedules-dlv_time = sy-uzeit.

    APPEND ls_schedules TO lt_schedules.

    ls_schedulesx-itm_number = ls_vbap-posnr.
    ls_schedulesx-sched_line = 0001.
    ls_schedulesx-updateflag = 'I'.
    ls_schedulesx-req_date = 'X'.
    ls_schedulesx-date_type = 'X'.
    ls_schedulesx-sched_type = 'X'.
    ls_schedulesx-req_qty = 'X'.
    ls_schedulesx-dlv_date = 'X'.
    ls_schedulesx-load_date = 'X'.
    ls_schedulesx-gi_date = 'X'.
    ls_schedulesx-tp_date = 'X'.
    ls_schedulesx-ms_date = 'X'.
    ls_schedulesx-load_time = 'X'.
    ls_schedulesx-gi_time = 'X'.
    ls_schedulesx-ms_time = 'X'.
    ls_schedulesx-tp_time = 'X'.
    ls_schedulesx-req_time = 'X'.
    ls_schedulesx-dlv_time = 'X'.
    APPEND ls_schedulesx TO lt_schedulesx.


    ls_condition-itm_number = ls_vbap-posnr.
    ls_condition-cond_type = 'ZPR6'.
    ls_condition-cond_st_no = '100'.
    ls_condition-cond_count = '1'.
    ls_condition-cond_value = ls_vbap-kbetr.
    ls_condition-currency = 'CNY'.
    APPEND ls_condition TO lt_condition.

    ls_conditionx-itm_number = ls_vbap-posnr.
    ls_conditionx-cond_type = 'ZPR6'.
    ls_conditionx-cond_st_no = '100'.
    ls_conditionx-cond_count = '1'.
    ls_conditionx-cond_value = 'X'.
    ls_conditionx-currency = 'X'.
    ls_conditionx-updateflag = 'U'.
    APPEND ls_conditionx TO lt_conditionx.

    CLEAR ls_item.
    CLEAR ls_schedules.
    CLEAR ls_schedulesx.
  ENDLOOP.

  IF serv_date <> 00000000.
    ls_header-serv_date = serv_date.
    ls_headerx-serv_date = 'X'.
  ENDIF.

  CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
    EXPORTING
      sales_header_in      = ls_header
      sales_header_inx     = ls_headerx
    IMPORTING
      salesdocument_ex     = gs_kythlist-odeln
    TABLES
      return               = lt_return
      sales_items_in       = lt_item
      sales_partners       = lt_partner
      sales_schedules_in   = lt_schedules
      sales_schedules_inx  = lt_schedulesx
      sales_conditions_in  = lt_condition
      sales_conditions_inx = lt_conditionx.

  p_return[] = lt_return[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CONTINUE_EXECUTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_continue_execute .

  DATA:lv_bhnuc TYPE zbhnuc.
  DATA:lt_kyth TYPE TABLE OF zsmm_kyth,
       ls_kyth TYPE zsmm_kyth.
  DATA:lt_return TYPE TABLE OF bapiret2.
  DATA:lv_change ."数量发生变化

  CLEAR: gs_kyth_data,
         lv_bhnuc,
         lv_change.
  READ TABLE gt_kyth_data INTO gs_kyth_data INDEX 1.
  lv_bhnuc = gs_kyth_data-bhnuc.

  SELECT bhnuc,
         bhitem,
         kwmeng
  INTO TABLE @DATA(lt_jiamengfahuo)
  FROM zsd_jiamengfahuo
  WHERE bhnuc = @lv_bhnuc.

  LOOP AT gt_kyth_data INTO gs_kyth_data.
    READ TABLE lt_jiamengfahuo INTO DATA(ls_jiamengfahuo) WITH KEY bhnuc = gs_kyth_data-bhnuc
                                                                   bhitem = gs_kyth_data-bhitem.
    IF sy-subrc = 0.
      IF ls_jiamengfahuo-kwmeng <> gs_kyth_data-menge.
        lv_change = 'X'.
      ENDIF.
    ENDIF.
    MOVE-CORRESPONDING gs_kyth_data TO ls_kyth.
    APPEND ls_kyth TO lt_kyth.
    CLEAR:ls_kyth,gs_kyth_data.
  ENDLOOP.

  CALL FUNCTION 'ZFMMM_KYTH'
    EXPORTING
      im_bhnuc        = lv_bhnuc
      im_change       = lv_change
    TABLES
      gt_data         = lt_kyth
      return          = lt_return
    EXCEPTIONS
      error_exception = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    LOOP AT gt_kyth_data_temp ASSIGNING FIELD-SYMBOL(<fs_kyth>) .
      READ TABLE lt_kyth INTO DATA(temp_kyth) WITH KEY bhnuc = <fs_kyth>-bhnuc
                                                     bhitem = <fs_kyth>-bhitem.
      IF sy-subrc = 0.
        <fs_kyth>-status = '退货中'.
      ENDIF.
    ENDLOOP.
    CALL FUNCTION 'SUSR_DISPLAY_LOG'
      EXPORTING
        display_in_popup = 'X'
      TABLES
        it_log_bapiret2  = lt_return
      EXCEPTIONS
        parameter_error  = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    MESSAGE '退货成功' TYPE 'S'.
    LOOP AT gt_kyth_data INTO gs_kyth_data .
      DELETE gt_kyth_data_temp WHERE bhnuc = gs_kyth_data-bhnuc.
      EXIT.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INSET_ZMM_KYTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_zmm_kyth TABLES pt_return TYPE bapiret2_t
                            pt_data STRUCTURE zsmm_kyth
                      USING p_bhnuc p_werks.
  DATA:ls_return TYPE bapiret2.
  DATA:ls_kyth_h  TYPE zmm_kyth_h,
       ls_kyth_jy TYPE zmm_kyth_jy,
       lt_kyth_jy TYPE TABLE OF zmm_kyth_jy.
  READ TABLE pt_return INTO ls_return WITH KEY type = 'E'.
  IF sy-subrc NE 0.
    SELECT SINGLE bhnuc INTO @DATA(lc_bhnuc) FROM zmm_kyth_h WHERE bhnuc = @p_bhnuc.
    IF sy-subrc <> 0.
      CLEAR:ls_kyth_h,ls_kyth_jy,lt_kyth_jy.
      ls_kyth_h-bhnuc = p_bhnuc.
      ls_kyth_h-fwerk = p_werks.
      ls_kyth_h-thdat = sy-datum.
      ls_kyth_h-thtim = sy-uzeit.
      ls_kyth_h-thnam = sy-uname.
      ls_kyth_h-thstu = '1'.

      LOOP AT pt_data INTO DATA(temp_data).
        ls_kyth_jy-bhnuc = temp_data-bhnuc.
        ls_kyth_jy-bhitem = temp_data-bhitem.
        ls_kyth_jy-matnr = temp_data-matnr.
        ls_kyth_jy-charg = temp_data-charg.
        ls_kyth_jy-menge = temp_data-menge.
        APPEND ls_kyth_jy TO lt_kyth_jy.
        CLEAR ls_kyth_jy.
      ENDLOOP.
      INSERT zmm_kyth_h FROM ls_kyth_h.
      INSERT zmm_kyth_jy FROM TABLE lt_kyth_jy.
    ENDIF.

  ENDIF.
ENDFORM.
