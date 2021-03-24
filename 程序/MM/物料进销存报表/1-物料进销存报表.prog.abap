REPORT zcpcr0004_1.
TABLES: mchbh,mara,bkpf,marc.

DATA:BEGIN OF gt_mseg OCCURS 0,
       mblnr    LIKE mseg-mblnr,
       zeile    LIKE mseg-zeile,
       matnr    LIKE mseg-matnr,
       werks    LIKE mseg-werks,
       bwart    LIKE mseg-bwart,
       mtart    LIKE mara-mtart,
       menge    LIKE mseg-menge,
       lgort    LIKE mseg-lgort,
       dmbtr    LIKE mseg-dmbtr,
       shkzg    LIKE mseg-shkzg,
       budat    LIKE mkpf-budat,
       smbln    LIKE mseg-smbln,
       smblp    LIKE mseg-smblp,
       kzbew    LIKE mseg-kzbew,
       dflag(1),
       rksl     TYPE p DECIMALS 3,
       rkje     TYPE p DECIMALS 2,
       cksl     TYPE p DECIMALS 3,
       ckje     TYPE p DECIMALS 2,
       tlsl     TYPE p DECIMALS 3,
       tlje     TYPE p DECIMALS 2,
       chsl     TYPE p DECIMALS 3,
       chje     TYPE p DECIMALS 2,
     END OF gt_mseg.

DATA: BEGIN OF it_bsim OCCURS 0,
        bukrs LIKE bkpf-bukrs,  "公司代码
        belnr LIKE bkpf-belnr,  "会计凭证
        gjahr LIKE bkpf-gjahr,  "会计年度
        budat LIKE bkpf-budat,  "过账日期
        buzei LIKE bsim-buzei,  "项目
        matnr LIKE bsim-matnr,  "物料
        shkzg LIKE bsim-shkzg,  "借/贷
        dmbtr LIKE bsim-dmbtr,  "本位币金额
      END OF it_bsim.

DATA:BEGIN OF gt_mardh OCCURS 0,
       matnr LIKE mardh-matnr,
       maktx LIKE makt-maktx,
       werks LIKE mardh-werks,
       lgort LIKE mardh-lgort,
       mtart LIKE mara-mtart,
       lfgja LIKE mardh-lfgja,
       lfmon LIKE mardh-lfmon,
       labst LIKE mardh-labst,
       qckc  TYPE p DECIMALS 3,
       qcje  TYPE p DECIMALS 3,
       qmkc  TYPE p DECIMALS 3,
       qmje  TYPE p DECIMALS 2,
     END OF gt_mardh.

DATA:BEGIN OF gt_index OCCURS 0,
       matnr LIKE mseg-matnr,
       werks LIKE mseg-werks,
     END OF gt_index.

DATA:gt_zcpcr0004 TYPE TABLE OF zcpcr0004,
     gs_zcpcr0004 TYPE zcpcr0004.

RANGES:gr_bwart FOR mseg-bwart.

DATA:lv_bgdate TYPE d,
     lv_eddata TYPE d.

DATA:where_condition TYPE string.
DATA:mtart_condtion TYPE string.

FIELD-SYMBOLS: <ft_alv> TYPE STANDARD TABLE,
               <fs_alv>.
*&---------------------------------------------------------------------*
*&  alv 定义
*&---------------------------------------------------------------------*
DATA:gs_layo  TYPE lvc_s_layo,
     gs_fcat  TYPE lvc_s_fcat,
     gt_fcat  TYPE lvc_t_fcat,
     gs_title TYPE lvc_title.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:s_matnr FOR marc-matnr MEMORY ID mat MATCHCODE OBJECT mat1,
               s_werks FOR marc-werks  MEMORY ID wrk MATCHCODE OBJECT h_t001w DEFAULT 'YZ01'.
*               S_LGORT FOR MCHBH-LGORT MEMORY ID LAG.
PARAMETERS: p_year  LIKE mseg-gjahr OBLIGATORY DEFAULT sy-datum+0(4),
            p_month LIKE bkpf-monat OBLIGATORY DEFAULT sy-datum+4(2).
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETER:p01  RADIOBUTTON GROUP type DEFAULT 'X'.
*PARAMETER:p02 RADIOBUTTON GROUP type.
PARAMETER:p03 RADIOBUTTON GROUP type.
SELECTION-SCREEN END OF BLOCK b2.


START-OF-SELECTION.

  PERFORM build_bwart_range.

  PERFORM build_where_condition.

  PERFORM set_fieldcat.

  PERFORM get_data.

  PERFORM display_alv.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  TYPES: BEGIN OF ty_makt,
           matnr TYPE makt-matnr,
           maktx TYPE makt-maktx,
         END OF ty_makt.
  TYPES: BEGIN OF ty_mbewh,
           lfgja TYPE mbewh-lfgja,
           lfmon TYPE mbewh-lfmon,
           matnr TYPE mbewh-matnr,
           bwkey TYPE mbewh-bwkey,
           lbkum TYPE mbewh-lbkum,
           salk3 TYPE mbewh-salk3,
         END OF ty_mbewh.
  TYPES:BEGIN OF ty_marc,
          matnr TYPE marc-matnr,
          werks TYPE marc-werks,
        END OF ty_marc.
  TYPES:BEGIN OF ty_marv.
          INCLUDE TYPE marv.
  TYPES:bwkey TYPE t001k-bwkey,
        END OF ty_marv.

  DATA:lt_makt TYPE TABLE OF ty_makt,
       ls_makt TYPE ty_makt.
  DATA:lt_mbewh TYPE TABLE OF ty_mbewh,
       ls_mbewh TYPE ty_mbewh.

  DATA:lt_marc TYPE TABLE OF ty_marc,
       ls_marc TYPE ty_marc.

  DATA:lv_year  TYPE char4,
       lv_month TYPE char2.

  DATA:lv_date TYPE d.

  DATA:lv_fieldname TYPE string.
  DATA:lv_index TYPE sy-tabix.

  DATA:lv_lbkum TYPE mbewh-lbkum,
       lv_salk3 TYPE mbewh-salk3.

  SELECT mara~matnr
         werks
  INTO TABLE lt_marc
  FROM marc JOIN mara
    ON marc~matnr = mara~matnr
  WHERE werks IN s_werks
    AND mara~matnr IN s_matnr
    AND (mtart_condtion).


  SELECT mseg~mblnr mseg~zeile mseg~matnr mseg~werks mseg~bwart mseg~menge mseg~dmbtr mseg~shkzg mkpf~budat mara~mtart smbln smblp mseg~lgort  mseg~kzbew
    FROM mseg INNER JOIN  mkpf ON mseg~mblnr = mkpf~mblnr AND mseg~mjahr = mkpf~mjahr INNER JOIN mara ON mseg~matnr = mara~matnr
   INTO CORRESPONDING FIELDS OF TABLE gt_mseg
  WHERE  (where_condition).

  IF lt_marc[] IS NOT INITIAL.
    SELECT matnr maktx INTO TABLE lt_makt FROM makt FOR ALL ENTRIES IN lt_marc WHERE matnr = lt_marc-matnr AND spras = sy-langu.

    SELECT lfgja
           lfmon
           matnr
           bwkey
           lbkum
           salk3
    INTO TABLE lt_mbewh
    FROM mbewh FOR ALL ENTRIES IN lt_marc
    WHERE matnr = lt_marc-matnr
      AND bwkey = lt_marc-werks
      AND ( ( lfgja = p_year AND lfmon <= p_month ) OR lfgja < p_year ).
    SORT lt_mbewh BY lfgja DESCENDING lfmon DESCENDING.

    SELECT zco001~matnr werks mara~mtart lfgja lfmon qmkc qmje
  INTO CORRESPONDING FIELDS OF TABLE gt_mardh FROM zco001 INNER JOIN mara ON zco001~matnr = mara~matnr WHERE zco001~matnr IN s_matnr
  AND werks IN s_werks AND lfgja = p_year AND lfmon = p_month.
  ENDIF.

  LOOP AT lt_marc INTO ls_marc.
    READ TABLE gt_index WITH KEY matnr = ls_marc-matnr
                                   werks = ls_marc-werks.
    IF sy-subrc = 0.
      lv_index = sy-tabix.
      READ TABLE <ft_alv> ASSIGNING <fs_alv> INDEX lv_index.
    ELSE.
      gt_index = CORRESPONDING #( ls_marc ).
      APPEND gt_index.
      APPEND INITIAL LINE TO <ft_alv> ASSIGNING <fs_alv>.
      PERFORM fill_value USING 'MATNR' ls_marc-matnr.
      PERFORM fill_value USING 'WERKS' ls_marc-werks.
      READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_marc-matnr.
      IF sy-subrc = 0.
        PERFORM fill_value USING 'MAKTX' ls_makt-maktx.
      ENDIF.
*      READ TABLE lt_marv INTO ls_marv WITH KEY bwkey = ls_marc-werks.
*      IF sy-subrc = 0.

      SELECT SINGLE qmkc qmje INTO (lv_lbkum,lv_salk3) FROM zco001
      WHERE matnr = ls_marc-matnr
        AND werks = ls_marc-werks
        AND lfgja = p_year
        AND lfmon = p_month.
      IF sy-subrc = 0.
        PERFORM fill_numerical USING 'ZQMSL' lv_lbkum.
        PERFORM fill_numerical USING 'ZQMJE' lv_salk3.

*        PERFORM fill_numerical USING 'ZQCSL' gt_mardh-qckc.
*        PERFORM fill_numerical USING 'ZQCJE' gt_mardh-qcje.
      ELSE.
        SELECT SINGLE lbkum salk3 INTO (lv_lbkum,lv_salk3) FROM mbew
WHERE matnr = ls_marc-matnr
AND bwkey = ls_marc-werks
AND ( ( lfgja = p_year AND lfmon <= p_month ) OR lfgja < p_year ).
        IF sy-subrc = 0.
          PERFORM fill_numerical USING 'ZQMSL' lv_lbkum.
          PERFORM fill_numerical USING 'ZQMJE' lv_salk3.
        ELSE.
          LOOP AT lt_mbewh INTO ls_mbewh WHERE matnr = ls_marc-matnr
                                 AND bwkey = ls_marc-werks.
            PERFORM fill_numerical USING 'ZQMSL' ls_mbewh-lbkum.
            PERFORM fill_numerical USING 'ZQMJE' ls_mbewh-salk3.
            EXIT.
          ENDLOOP.
        ENDIF.
      ENDIF.

      lv_bgdate = lv_bgdate - 1.
      lv_year = lv_bgdate+0(4).
      lv_month = lv_bgdate+4(2).
      lv_bgdate = lv_bgdate + 1.

      SELECT SINGLE qmkc qmje INTO (lv_lbkum,lv_salk3) FROM zco001
      WHERE matnr = ls_marc-matnr
        AND werks = ls_marc-werks
        AND lfgja = lv_year
        AND lfmon = lv_month.
      IF sy-subrc = 0.
        PERFORM fill_numerical USING 'ZQCSL' lv_lbkum.
        PERFORM fill_numerical USING 'ZQCJE' lv_salk3.
      ELSE.
        SELECT SINGLE lbkum salk3 INTO (lv_lbkum,lv_salk3) FROM mbew
        WHERE matnr = ls_marc-matnr
        AND bwkey = ls_marc-werks
        AND ( ( lfgja = p_year AND lfmon < p_month ) OR lfgja < p_year ).
        IF sy-subrc = 0.
          PERFORM fill_numerical USING 'ZQCSL' lv_lbkum.
          PERFORM fill_numerical USING 'ZQCJE' lv_salk3.
        ELSE.
          DELETE lt_mbewh WHERE matnr = ls_marc-matnr AND bwkey = ls_marc-werks AND lfgja = p_year AND lfmon = p_month.
          LOOP AT lt_mbewh INTO ls_mbewh WHERE matnr = ls_marc-matnr
                                           AND bwkey = ls_marc-werks.
            PERFORM fill_numerical USING 'ZQCSL' ls_mbewh-lbkum.
            PERFORM fill_numerical USING 'ZQCJE' ls_mbewh-salk3.
            EXIT.
          ENDLOOP.
        ENDIF.
      ENDIF.

*      ENDIF.
    ENDIF.
    LOOP AT gt_mseg WHERE matnr = ls_marc-matnr AND werks = ls_marc-werks.
      IF gt_mseg-shkzg = 'H'.
        gt_mseg-dmbtr = - gt_mseg-dmbtr.
        gt_mseg-menge = - gt_mseg-menge.
      ENDIF.
      LOOP AT gt_zcpcr0004 INTO gs_zcpcr0004 WHERE bwart = gt_mseg-bwart.
        IF gs_zcpcr0004-zoppo IS NOT INITIAL.
          gt_mseg-dmbtr = - gt_mseg-dmbtr.
          gt_mseg-menge = - gt_mseg-menge.
        ENDIF.
        IF gs_zcpcr0004-kzbew IS NOT INITIAL.
          CHECK gs_zcpcr0004-kzbew = gt_mseg-kzbew.
        ENDIF.
        lv_fieldname = 'MENGE' && gs_zcpcr0004-ztype_bwart.
        CONDENSE lv_fieldname NO-GAPS.
        PERFORM fill_numerical USING lv_fieldname gt_mseg-menge.
        lv_fieldname = 'DMBTR' && gs_zcpcr0004-ztype_bwart.
        CONDENSE lv_fieldname NO-GAPS.
        PERFORM fill_numerical USING lv_fieldname gt_mseg-dmbtr.
      ENDLOOP.

    ENDLOOP.
    UNASSIGN <fs_alv>.
  ENDLOOP.



***处理pr/re/z1，取bsim表数据
  IF gt_index IS NOT INITIAL.
    DATA:l_hkont TYPE skat-saknr.
    SELECT
  a~bukrs
  a~belnr
  a~gjahr
  a~budat
  b~buzei
  b~matnr
  b~shkzg
  b~dmbtr
  INTO CORRESPONDING FIELDS OF TABLE it_bsim
  FROM bkpf AS a
  JOIN bsim AS b ON a~belnr = b~belnr
                AND a~gjahr = b~gjahr
                AND a~blart = b~blart
                AND a~budat = b~budat
  FOR ALL ENTRIES IN gt_index
  WHERE a~gjahr = p_year
    AND a~budat >= lv_bgdate AND a~budat <= lv_eddata
    AND ( a~blart EQ 'RE' OR a~blart EQ 'PR' OR a~blart EQ 'Z1' )
    AND b~matnr EQ gt_index-matnr
    AND a~bukrs IN s_werks
    AND b~bwkey IN s_werks.
  ENDIF.

*"计算发票校验和价格调整
  LOOP AT gt_index.
    lv_index = sy-tabix.
    READ TABLE <ft_alv> ASSIGNING <fs_alv> INDEX lv_index.
    LOOP AT  it_bsim WHERE matnr = gt_index-matnr.
      SELECT SINGLE hkont INTO l_hkont FROM bseg WHERE belnr = it_bsim-belnr AND buzei = it_bsim-buzei AND bukrs = it_bsim-bukrs AND gjahr = it_bsim-gjahr AND hkont NE '0066031040' .
      IF sy-subrc = 0.
        CASE it_bsim-shkzg.
          WHEN 'S'. "入库
            "期末金额
            PERFORM fill_numerical USING 'ZTZJE' it_bsim-dmbtr.
          WHEN 'H'. "出库
            "期末金额
            it_bsim-dmbtr = - it_bsim-dmbtr.
            PERFORM fill_numerical USING 'ZTZJE' it_bsim-dmbtr.
        ENDCASE.
      ENDIF.
      CLEAR it_bsim.
    ENDLOOP.
    UNASSIGN <fs_alv>.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .

  PERFORM set_layout.

  PERFORM output_alv.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_layout .

  CLEAR gs_layo.
  gs_layo-zebra      = 'X'.
  gs_layo-cwidth_opt = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_fieldcat .
  DATA:lt_zcpcr0004 TYPE TABLE OF zcpcr0004.
  DATA:lv_fieldname TYPE string.
  DATA:ls_dd07v TYPE dd07v.
  DATA:lr_table    TYPE REF TO data,
       lr_new_line TYPE REF TO data.
  DATA:lv_domain_value TYPE dd07l-domvalue_l.
  CLEAR gt_fcat.
  PERFORM add_fieldcat USING:'WERKS' '工厂' 'T001W' 'WERKS' ''," 工厂
                              'MATNR' '物料编码' 'MARA' 'MATNR' ''," 物料编码
                              'MAKTX' '物料描述' 'MAKT' 'MAKTX' ''," 物料描述
                              'ZQCSL' '期初库存' 'MSEG' 'MENGE' ''," 期初库存
                              'ZQCJE' '期初金额' 'BSEG' 'DMBTR' ''." 期初金额
  lt_zcpcr0004 = gt_zcpcr0004.
  SORT lt_zcpcr0004 BY ztype_bwart.
  DELETE ADJACENT DUPLICATES FROM lt_zcpcr0004 COMPARING ztype_bwart.
  LOOP AT lt_zcpcr0004 INTO gs_zcpcr0004.

    gs_fcat-fieldname = 'MENGE' && gs_zcpcr0004-ztype_bwart.
    CONDENSE gs_fcat-fieldname NO-GAPS.
    lv_domain_value = gs_zcpcr0004-ztype_bwart.
    CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
      EXPORTING
        domname  = 'ZDTYPE_BWART'
        value    = lv_domain_value
        langu    = sy-langu
      IMPORTING
        dd07v_wa = ls_dd07v.
    gs_fcat-coltext = ls_dd07v-ddtext && '数量'.
    gs_fcat-ref_table = 'MSEG'.
    gs_fcat-ref_field = 'MENGE'.
    APPEND gs_fcat TO gt_fcat.

    gs_fcat-fieldname = 'DMBTR' && gs_zcpcr0004-ztype_bwart.
    CONDENSE gs_fcat-fieldname NO-GAPS.
    gs_fcat-coltext = ls_dd07v-ddtext && '金额'.
    gs_fcat-ref_table = 'BSEG'.
    gs_fcat-ref_field = 'DMBTR'.
    APPEND gs_fcat TO gt_fcat.
    CLEAR gs_fcat.
  ENDLOOP.

  PERFORM add_fieldcat USING:'ZQTJE' '其他调整金额' 'BSEG' 'DMBTR' ''," 其他调整金额
                             'ZQMSL' '期末库存' 'MSEG' 'MENGE' ''," 期末库存
                             'ZQMJE' '期末金额' 'BSEG' 'DMBTR' ''." 期末金额

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
*     i_style_table             =
      it_fieldcatalog           = gt_fcat
*     i_length_in_byte          =
    IMPORTING
      ep_table                  = lr_table
*     e_style_fname             =
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2.

  ASSIGN lr_table->* TO <ft_alv>.
  CREATE DATA lr_new_line LIKE LINE OF <ft_alv>.
  ASSIGN lr_new_line->* TO <fs_alv>.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM add_fieldcat  USING   VALUE(p_fieldname)
                            VALUE(p_coltext)
                            VALUE(p_ref_tab)
                            VALUE(p_ref_field)
                            VALUE(p_edit).
  CLEAR:gs_fcat.

  gs_fcat-fieldname = p_fieldname.
  gs_fcat-coltext   = p_coltext.
  gs_fcat-ref_table = p_ref_tab.
  gs_fcat-ref_field = p_ref_field.
  gs_fcat-edit      = p_edit.
  APPEND gs_fcat TO gt_fcat.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM output_alv .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program     = sy-repid
      is_layout_lvc          = gs_layo
      it_fieldcat_lvc        = gt_fcat
      i_save                 = 'A'
      i_callback_top_of_page = 'ALV_TOP_OF_PAGE'
*     i_grid_title           = gs_title
    TABLES
      t_outtab               = <ft_alv>
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
ENDFORM.
FORM alv_top_of_page.
*ALV Header declarations
  DATA: t_header      TYPE slis_t_listheader,
        gw_header     TYPE slis_listheader,
        t_line        LIKE gw_header-info,
        ld_lines      TYPE i,
        ld_linesc(10) TYPE c.

* -->>Title
  gw_header-typ  = 'H'.    " H = Header, S = Selection, A = Action
  IF p01 = 'X'.
    gw_header-info ='原材料进销存报表'.   "sy-title.
*  ELSEIF p02 = 'X'.
*    gw_header-info ='低值易耗品进销存报表'.   "sy-title.
  ELSEIF p03 = 'X'.
    gw_header-info ='成品进销存报表'.   "sy-title.
  ENDIF.
  APPEND gw_header TO t_header.
  CLEAR gw_header.

* -->>System Date   只有tye = 'S'时才设置key.
  gw_header-typ  = 'S'.
  gw_header-key = 'System Date: '.
  CONCATENATE  sy-datum(4) '/' sy-datum+4(2) '/' sy-datum+6(2)
               INTO gw_header-info.   "todays date
  APPEND gw_header TO t_header.
  CLEAR: gw_header.

* -->>Total No. of Records Selected
  DESCRIBE TABLE <ft_alv> LINES ld_lines.  "count the row number of gt_itab
  ld_linesc = ld_lines.

  CONCATENATE 'Total No. of Records: ' ld_linesc INTO t_line SEPARATED BY space.
  gw_header-typ  = 'A'.
  gw_header-info = t_line.

  APPEND gw_header TO t_header.
  CLEAR: gw_header, t_line.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header.

ENDFORM .
*&---------------------------------------------------------------------*
*&      Form  BUILD_BWART_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_bwart_range .
  SELECT * INTO TABLE gt_zcpcr0004 FROM zcpcr0004 .
  LOOP AT gt_zcpcr0004 INTO gs_zcpcr0004.
    gr_bwart = VALUE #( sign = 'I' option = 'EQ'  low = gs_zcpcr0004-bwart high = space ).
    APPEND gr_bwart.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_WHERE_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_where_condition .



  lv_bgdate = p_year && p_month && '01'.

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date             = lv_bgdate
    IMPORTING
      ev_month_begin_date = lv_bgdate
      ev_month_end_date   = lv_eddata.

  where_condition = |mseg~matnr IN s_matnr AND werks IN s_werks AND mseg~mjahr = p_year AND mkpf~budat >= lv_bgdate AND mkpf~budat <= lv_eddata AND mseg~lgort <> '' and mseg~bwart in gr_bwart|.


  CASE 'X'.
    WHEN p01.
      where_condition = |{ where_condition } and ( mtart = 'Z002' OR mtart = 'Z003' OR mtart = 'Z004' OR mtart = 'Z005' or mtart = 'Z001' ) |.
*    WHEN p02.
*      where_condition = |{ where_condition } and ( mtart = 'Z002' OR mtart = 'Z003' OR mtart = 'Z004' OR mtart = 'Z005' )|.
    WHEN p03.
      where_condition = |{ where_condition } and ( mtart = 'Z007' OR mtart = 'Z071' )|.
    WHEN OTHERS.
  ENDCASE.

  CASE 'X'.
    WHEN p01.
      mtart_condtion = |  ( mtart = 'Z002' OR mtart = 'Z003' OR mtart = 'Z004' OR mtart = 'Z005' or mtart = 'Z001' ) |.
*    WHEN p02.
*      mtart_condtion = |  ( mtart = 'Z002' OR mtart = 'Z003' OR mtart = 'Z004' OR mtart = 'Z005' )|.
    WHEN p03.
      mtart_condtion = |  ( mtart = 'Z007' OR mtart = 'Z071' )|.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0525   text
*      -->P_GT_MSEG_MATNR  text
*----------------------------------------------------------------------*
FORM fill_value  USING    VALUE(p_fieldname)
                          VALUE(p_value).
  FIELD-SYMBOLS <f_value>.
  ASSIGN COMPONENT p_fieldname OF STRUCTURE <fs_alv> TO <f_value>.
  IF sy-subrc = 0.
    <f_value> = p_value.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_NUMERICAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FIELDNAME  text
*      -->P_GT_MSEG_MENGE  text
*----------------------------------------------------------------------*
FORM fill_numerical  USING    VALUE(p_fieldname)
                          VALUE(p_value).
  FIELD-SYMBOLS <f_value>.
  ASSIGN COMPONENT p_fieldname OF STRUCTURE <fs_alv> TO <f_value>.
  IF sy-subrc = 0.
    <f_value> = p_value + <f_value>.
  ENDIF.
ENDFORM.