**************************************************
*程序名称:收发存报表
*创建日期: 2019-11-29
*创建者:XXX
*申请者:XXX
*功能描述:
*============================================
*变更记录
*修改请求号    修改日期    修改人   修改描述
*DEVK912054    2019-11-29   HANDYXH    创建程序
***************************************************

REPORT zshxjfi0014 MESSAGE-ID zfishxj01.

*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES:mseg,mkpf.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES: BEGIN OF ty_alv,
         matnr TYPE mseg-matnr,  " 物料编码
         maktx TYPE makt-maktx,  " 物料名称
         bukrs TYPE bkpf-bukrs,  " 公司代码
         werks TYPE bseg-werks,  " 工厂
         gjahr TYPE bseg-gjahr,  " 年度
         monat TYPE bkpf-monat,  " 期间
         vprsv TYPE mbew-vprsv,  " 价格控制
         bklas TYPE mbew-bklas,  " 评估类
         bkbez TYPE t025t-bkbez, " 评估类名称
         zqcsl TYPE bseg-menge,  " 期初数量
         zqcje TYPE bseg-dmbtr,  " 期初金额
         zsrsl TYPE bseg-menge,  " 收入数量
         zsrje TYPE bseg-dmbtr,  " 收入金额
         zfcsl TYPE bseg-menge,  " 发出数量
         zfcje TYPE bseg-dmbtr,  " 发出金额
         zqmsl TYPE bseg-menge,  " 期末数量
         zqmje TYPE bseg-dmbtr,  " 期末金额

       END OF ty_alv.

*-----------------------------------------------------------------------
* D A T A S
*-----------------------------------------------------------------------
DATA:gt_alv  TYPE TABLE OF ty_alv.

*-----------------------------------------------------------------------
* ALV D A T A S
*-----------------------------------------------------------------------
DATA:gt_fcat TYPE lvc_t_fcat,
     gw_fcat TYPE lvc_s_fcat,
     gw_layo TYPE lvc_s_layo.

*-----------------------------------------------------------------------
* R A N G E S
*-----------------------------------------------------------------------
RANGES:r_budat FOR bkpf-budat,
       r_blart FOR bkpf-blart,
       r_hkont FOR bseg-hkont,
       r_bwart FOR mseg-bwart.
*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS:c_default_bukrs TYPE t001-bukrs VALUE '6100',
          c_default_werks TYPE t001w-werks VALUE '6110',
          c_shkzg_h       TYPE bseg-shkzg VALUE 'H'.

*--------------------------------------------------------------------*
*定义选择屏幕参数
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.

PARAMETERS:p_bukrs TYPE t001-bukrs  OBLIGATORY DEFAULT c_default_bukrs,
           p_werks TYPE t001w-werks OBLIGATORY DEFAULT c_default_werks,
           p_mjahr TYPE mkpf-mjahr  OBLIGATORY,
           p_monat TYPE bkpf-monat  OBLIGATORY.

SELECT-OPTIONS:s_matnr FOR mseg-matnr.

SELECTION-SCREEN END OF BLOCK blk1.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM build_condition.
  PERFORM get_data.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       根据条件取数
*----------------------------------------------------------------------*
FORM get_data .

  CLEAR:gt_alv.

  "取会计凭证
  SELECT bukrs,
         gjahr,
         belnr,
         monat,
         blart
  INTO TABLE @DATA(lt_bkpf)
  FROM bkpf
  WHERE bukrs = @p_bukrs
    AND ( ( gjahr = @p_mjahr AND monat <= @p_monat ) OR gjahr < @p_mjahr )
    AND blart IN @r_blart.
  SORT lt_bkpf BY bukrs gjahr belnr.

  IF lt_bkpf IS NOT INITIAL.
    SELECT bukrs,
           gjahr,
           belnr,
           werks,
           matnr,
           shkzg,
           dmbtr,
           meins
    INTO TABLE @DATA(lt_bseg)
    FROM bseg FOR ALL ENTRIES IN @lt_bkpf
    WHERE bukrs = @lt_bkpf-bukrs
      AND gjahr = @lt_bkpf-gjahr
      AND belnr = @lt_bkpf-belnr
      AND matnr IN @s_matnr
      AND hkont IN @r_hkont
      AND werks = @p_werks.

    SORT lt_bseg BY bukrs werks matnr.


    LOOP AT lt_bseg INTO DATA(ls_bseg).
      IF ls_bseg-shkzg = c_shkzg_h.
        ls_bseg-dmbtr = - ls_bseg-dmbtr.
      ENDIF.

      READ TABLE gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WITH KEY bukrs = ls_bseg-bukrs
                                                                  werks = ls_bseg-werks
                                                                  matnr = ls_bseg-matnr.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO gt_alv ASSIGNING <fs_alv>.
        <fs_alv>-bukrs = ls_bseg-bukrs.
        <fs_alv>-werks = ls_bseg-werks.
        <fs_alv>-matnr = ls_bseg-matnr.
        <fs_alv>-gjahr = p_mjahr.
        <fs_alv>-monat = p_monat.
      ENDIF.

      READ TABLE lt_bkpf INTO DATA(ls_bkpf) WITH KEY bukrs = ls_bseg-bukrs
                                                     gjahr = ls_bseg-gjahr
                                                     belnr = ls_bseg-belnr BINARY SEARCH.
      "期初库存价值
      IF ls_bkpf-monat < p_monat.
        ADD ls_bseg-dmbtr TO <fs_alv>-zqcje.
      ENDIF.

      "期末库存价值
      IF ls_bkpf-monat <= p_monat.
        ADD ls_bseg-dmbtr TO <fs_alv>-zqmje.
      ENDIF.

      IF ls_bkpf-monat = p_monat.
        "收入金额
        IF ls_bkpf-blart = 'WE'.
          ADD ls_bseg-dmbtr TO <fs_alv>-zsrje.
        ENDIF.

        "发出金额
        IF ls_bkpf-blart = 'WA'.
          ADD ls_bseg-dmbtr TO <fs_alv>-zfcje.
        ENDIF.

      ENDIF.
    ENDLOOP.
  ENDIF.


  "取物料凭证
  SELECT mkpf~mblnr,
         mkpf~mjahr,
         mkpf~xblnr,
         mkpf~budat,
         mseg~matnr,
         mseg~zeile,
         mseg~dmbtr,
         mseg~shkzg,
         mseg~menge,
         mseg~meins,
         mseg~werks,
         mseg~bwart,
         mseg~kzvbr
  INTO TABLE @DATA(lt_mkpf)
  FROM mkpf INNER JOIN mseg
    ON mkpf~mblnr = mseg~mblnr
   AND mkpf~mjahr = mseg~mjahr
  WHERE budat <= @r_budat-high
    AND werks =  @p_werks
    AND matnr IN @s_matnr.

  DELETE lt_mkpf WHERE matnr IS INITIAL OR kzvbr = 'A'.

  SORT lt_mkpf BY mjahr werks mjahr.

  LOOP AT lt_mkpf INTO DATA(ls_mkpf).

    IF ls_mkpf-shkzg = c_shkzg_h.
      ls_mkpf-dmbtr = - ls_mkpf-dmbtr.
      ls_mkpf-menge = - ls_mkpf-menge.
    ENDIF.

    READ TABLE gt_alv ASSIGNING <fs_alv> WITH KEY werks = ls_mkpf-werks
                                                  matnr = ls_mkpf-matnr.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO gt_alv ASSIGNING <fs_alv>.
      <fs_alv>-bukrs = p_bukrs.
      <fs_alv>-werks = ls_mkpf-werks.
      <fs_alv>-matnr = ls_mkpf-matnr.
      <fs_alv>-gjahr = p_mjahr.
      <fs_alv>-monat = p_monat.
    ENDIF.

    IF ls_mkpf-budat < r_budat-low.
      "期初库存价值
      ADD ls_mkpf-dmbtr TO <fs_alv>-zqcje.
      "期初库存数量
      ADD ls_mkpf-menge TO <fs_alv>-zqcsl.
    ENDIF.

    IF ls_mkpf-budat <= r_budat-high.
      "期末库存价值
      ADD ls_mkpf-dmbtr TO <fs_alv>-zqmje.
      "期末库存数量
      ADD ls_mkpf-menge TO <fs_alv>-zqmsl.
    ENDIF.

    IF ls_mkpf-budat IN r_budat.
      "收入数量 收入金额
      CLEAR r_bwart[].
      r_bwart[] = VALUE #( sign = 'I' option = 'EQ' ( low = '561' )
                                                    ( low = '562' )
                                                    ( low = '101' )
                                                    ( low = '102' )
                                                    ( low = '105' )
                                                    ( low = '106' )
                                                    ( low = 'Z61' )
                                                    ( low = 'Z62' ) ).
      IF ls_mkpf-bwart IN r_bwart.
        ADD ls_mkpf-menge TO <fs_alv>-zsrsl.
        ADD ls_mkpf-dmbtr TO <fs_alv>-zsrje.
      ENDIF.

      "本期收货数量
      CLEAR r_bwart[].
      r_bwart[] = VALUE #( sign = 'I' option = 'EQ' ( low = '201' )
                                                    ( low = '202' )
                                                    ( low = '221' )
                                                    ( low = '222' )
                                                    ( low = '601' )
                                                    ( low = '602' )
                                                    ( low = 'Z63' )
                                                    ( low = 'Z64' ) ).
      IF ls_mkpf-bwart IN r_bwart.
        ADD ls_mkpf-menge TO <fs_alv>-zfcsl.
        ADD ls_mkpf-dmbtr TO <fs_alv>-zfcje.
      ENDIF.

    ENDIF.
  ENDLOOP.


  "取价格控制
  IF gt_alv IS NOT INITIAL.
    SELECT bwkey,
           matnr,
           vprsv,
           bklas
    INTO TABLE @DATA(lt_mbew)
    FROM mbew FOR ALL ENTRIES IN @gt_alv
    WHERE bwkey = @gt_alv-werks
      AND matnr = @gt_alv-matnr.
    SORT lt_mbew BY bwkey matnr.

    IF lt_mbew IS NOT INITIAL.
      SELECT bklas,
             bkbez
      INTO TABLE @DATA(lt_t025t)
      FROM t025t FOR ALL ENTRIES IN @lt_mbew
      WHERE bklas = @lt_mbew-bklas
        AND spras = @sy-langu.
    ENDIF.

    "取物料描述
    SELECT matnr,
           maktx
    INTO TABLE @DATA(lt_makt)
    FROM makt FOR ALL ENTRIES IN @gt_alv
    WHERE matnr = @gt_alv-matnr.
    SORT lt_makt BY matnr.
  ENDIF.


  SORT gt_alv BY bukrs werks matnr.
  DELETE ADJACENT DUPLICATES FROM gt_alv COMPARING bukrs werks matnr.

  LOOP AT gt_alv ASSIGNING <fs_alv>.
    "价格控制
    READ TABLE lt_mbew INTO DATA(ls_mbew) WITH KEY bwkey = <fs_alv>-werks matnr = <fs_alv>-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_alv>-vprsv = ls_mbew-vprsv.
    ENDIF.

    "物料描述
    READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = <fs_alv>-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_alv>-maktx = ls_makt-maktx.
    ENDIF.

  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       展示数据
*----------------------------------------------------------------------*
FORM display_data .
  PERFORM build_layout.
  PERFORM build_fieldcatlog.
  PERFORM display_alv.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       构建格式
*----------------------------------------------------------------------*
FORM build_layout .
  gw_layo-zebra        = 'X'.
  gw_layo-cwidth_opt   = 'X' .       " 自动优化列宽,根据字段值显示调节
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATLOG
*&---------------------------------------------------------------------*
*       构建字段
*----------------------------------------------------------------------*
FORM build_fieldcatlog .
  CLEAR:gt_fcat.

  PERFORM add_fieldcat USING:'MATNR' '物料编码' '' ''," 物料编码
  'MAKTX' '物料名称' '' ''," 物料名称
  'GJAHR' '年度' '' ''," 年度
  'MONAT' '期间' '' ''," 期间
  'VPRSV' '价格控制' '' ''," 价格控制
  'BLKAS' '评估类' '' ''," 评估类
  'BKBEZ' '评估类名称' '' ''," 评估类名称
  'ZQCSL' '期初数量' 'MSEG' 'MENGE'," 期初数量
  'ZQCJE' '期初金额' 'BSEG' 'DMBTR'," 期初金额
  'ZSRSL' '收入数量' 'MSEG' 'MENGE'," 收入数量
  'ZSRJE' '收入金额' 'BSEG' 'DMBTR'," 收入金额
  'ZFCSL' '发出数量' 'MSEG' 'MENGE'," 发出数量
  'ZFCJE' '发出金额' 'BSEG' 'DMBTR'," 发出金额
  'ZQMSL' '期末数量' 'MSEG' 'MENGE'," 期末数量
  'ZQMJE' '期末金额' 'BSEG' 'DMBTR'." 期末金额






ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       展示alv
*----------------------------------------------------------------------*
FORM display_alv .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
*     i_callback_pf_status_set    = l_status
*     i_callback_user_command     = l_command
*     i_callback_html_top_of_page = l_top_of_page
      is_layout_lvc      = gw_layo
      it_fieldcat_lvc    = gt_fcat
*     it_sort_lvc        = gt_sort
*     it_filter_lvc      = gt_filt
    TABLES
      t_outtab           = gt_alv
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDCAT
*&---------------------------------------------------------------------*
*       增加字段
*----------------------------------------------------------------------*
FORM add_fieldcat  USING    VALUE(p_fieldname)
                            VALUE(p_coltext)
                            VALUE(p_ref_table)
                            VALUE(p_ref_field).
  CLEAR gw_fcat.
  gw_fcat-fieldname = p_fieldname.
  gw_fcat-coltext   = p_coltext.
  gw_fcat-ref_table = p_ref_table.
  gw_fcat-ref_field = p_ref_field.
  APPEND gw_fcat TO gt_fcat.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_CONDITION
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM build_condition .

  CLEAR:r_budat ,r_budat[],r_blart,r_blart[],r_hkont,r_hkont[].

  r_budat-sign = 'I'.
  r_budat-option = 'BT'.
  r_budat-low  = p_mjahr && p_monat && '01'.
  CALL FUNCTION 'HRPAD_GET_LAST_DAY_OF_MONTH'
    EXPORTING
      iv_date     = r_budat-low
    IMPORTING
      ev_last_day = r_budat-high.

  APPEND r_budat.

  r_blart[] = VALUE #(  sign = 'I' option = 'EQ' ( low = 'WA' )
                                                 ( low = 'WE' )
                                                 ( low = 'RE' )
                                                 ( low = 'PR' )
                                                 ( low = 'WL' )
                                                 ( low = 'SA' ) ).

  r_hkont[] = VALUE #( sign = 'I' option = 'EQ' ( low = '1403980000' )
                                                ( low = '1405980000' ) ).

ENDFORM.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
