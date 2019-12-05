**************************************************
*程序名称:存货库龄表
*创建日期: 2019-11-21
*创建者:XXX
*申请者:XXX
*功能描述:
*============================================
*变更记录
*修改请求号    修改日期    修改人   修改描述
*DEVK912031    2019-11-21   HANDYXH    创建程序
***************************************************

REPORT zshxjmm0001 MESSAGE-ID zfishxj01.

*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES:mseg,mkpf,t001l.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES: BEGIN OF ty_alv,
         werks  TYPE mseg-werks,  " 工厂
         matnr  TYPE mseg-matnr,  " 物料编码
         maktx  TYPE makt-maktx,  " 物品名称
         lgort  TYPE mseg-lgort,  " 仓库号
         menge1 TYPE mseg-menge, " 当月净重
         dmbtr1 TYPE mseg-dmbtr, " 当月金额
         menge2 TYPE mseg-menge, " 1-3个月净重
         dmbtr2 TYPE mseg-dmbtr, " 1-3个月金额
         menge3 TYPE mseg-menge, " 4-6个月净重
         dmbtr3 TYPE mseg-dmbtr, " 4-6个月金额
         menge4 TYPE mseg-menge, " 7个月-1年净重
         dmbtr4 TYPE mseg-dmbtr, " 7个月-1年金额
         menge5 TYPE mseg-menge, " 1-2年净重
         dmbtr5 TYPE mseg-dmbtr, " 1-2年金额
         menge6 TYPE mseg-menge, " 2年以上净重
         dmbtr6 TYPE mseg-dmbtr, " 2年以上金额
       END OF ty_alv.

TYPES: BEGIN OF ty_mseg,
         mblnr TYPE mkpf-mblnr,
         mjahr TYPE mkpf-mjahr,
         zeile TYPE mseg-zeile,
         werks TYPE mseg-werks,
         matnr TYPE mseg-matnr,
         lgort TYPE mseg-lgort,
         budat TYPE mkpf-budat,
         menge TYPE mseg-menge,
         dmbtr TYPE mseg-dmbtr,
       END OF ty_mseg.



*-----------------------------------------------------------------------
* D A T A S
*-----------------------------------------------------------------------
DATA:  gt_alv     TYPE TABLE OF ty_alv,
       gt_mseg_s  TYPE TABLE OF ty_mseg,
       gt_mseg_h  TYPE TABLE OF ty_mseg,
       gt_age_his TYPE TABLE OF ztmm_xsj_fimbage.

DATA:  gw_mseg_s TYPE ty_mseg,
       gw_mseg_h TYPE ty_mseg.

*-----------------------------------------------------------------------
* ALV D A T A S
*-----------------------------------------------------------------------
DATA:gt_fcat TYPE lvc_t_fcat,
     gw_fcat TYPE lvc_s_fcat,
     gw_layo TYPE lvc_s_layo.

*-----------------------------------------------------------------------
* R A N G E S
*-----------------------------------------------------------------------
RANGES:r_bwart FOR mseg-bwart,
       r_budat FOR mkpf-budat.
*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS:c_default_werks TYPE t001w-werks VALUE '6110',
          c_shkzg_h       TYPE bseg-shkzg VALUE 'H'.

*--------------------------------------------------------------------*
*定义选择屏幕参数
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.

PARAMETERS:p_werks TYPE t001w-werks OBLIGATORY DEFAULT c_default_werks.

SELECT-OPTIONS:s_lgort FOR t001l-lgort,
               s_matnr FOR mseg-matnr.

PARAMETERS p_budat TYPE mkpf-budat OBLIGATORY DEFAULT sy-datum.

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

  SELECT mkpf~mjahr,
         mkpf~mblnr,
         budat,
         zeile,
         matnr,
         werks,
         lgort,
         menge,
         dmbtr,
         waers,
         shkzg,
         sjahr,
         smbln,
         smblp
  INTO TABLE @DATA(lt_mkpf)
  FROM mkpf INNER JOIN mseg
    ON mkpf~mblnr = mseg~mblnr
   AND mkpf~mjahr = mseg~mjahr
 WHERE mseg~werks = @p_werks
   AND mseg~matnr IN @s_matnr
   AND mseg~lgort IN @s_lgort
   AND mkpf~budat IN @r_budat
   AND mseg~bwart IN @r_bwart.

  DELETE lt_mkpf WHERE matnr IS INITIAL.

  "不计算冲销的数据
  LOOP AT lt_mkpf INTO DATA(ls_mkpf) WHERE smbln IS NOT INITIAL.
    DELETE lt_mkpf WHERE mblnr = ls_mkpf-smbln AND mjahr = ls_mkpf-sjahr AND zeile = ls_mkpf-smblp.
    DELETE gt_age_his WHERE mblnr = ls_mkpf-smbln AND mjahr = ls_mkpf-sjahr AND zeile = ls_mkpf-smblp.
    DELETE lt_mkpf.
  ENDLOOP.

  SORT lt_mkpf BY werks lgort matnr budat.
  LOOP AT lt_mkpf INTO ls_mkpf.
    "排除历史表中存在的物料凭证 避免同一个物料凭证计算两次
    READ TABLE gt_age_his TRANSPORTING NO FIELDS WITH KEY mjahr = ls_mkpf-mjahr mblnr = ls_mkpf-mblnr zeile = ls_mkpf-zeile.
    IF sy-subrc = 0.
      DELETE lt_mkpf.
      CONTINUE.
    ENDIF.

    CASE ls_mkpf-shkzg.
      WHEN 'S'.
        MOVE-CORRESPONDING ls_mkpf TO gw_mseg_s.
        APPEND gw_mseg_s TO gt_mseg_s.
      WHEN 'H'.
        MOVE-CORRESPONDING ls_mkpf TO gw_mseg_h.
        CLEAR: gw_mseg_h-budat,gw_mseg_h-mjahr,gw_mseg_h-mblnr,gw_mseg_h-zeile.
        COLLECT gw_mseg_h INTO gt_mseg_h.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  gt_mseg_s = CORRESPONDING #( BASE ( gt_mseg_s ) gt_age_his ).
  SORT gt_mseg_s BY werks lgort matnr budat.

  LOOP AT gt_mseg_s INTO gw_mseg_s.
    READ TABLE gt_mseg_h INTO gw_mseg_h WITH KEY werks = gw_mseg_s-werks
                                                 lgort = gw_mseg_s-lgort
                                                 matnr = gw_mseg_s-matnr.
    IF sy-subrc = 0.
      gw_mseg_h-menge = gw_mseg_h-menge - gw_mseg_s-menge.
      gw_mseg_h-dmbtr = gw_mseg_h-dmbtr - gw_mseg_s-dmbtr.
      IF gw_mseg_h-menge < 0.
        DELETE gt_mseg_h INDEX sy-tabix.
        gw_mseg_s-menge = - gw_mseg_h-menge.
        gw_mseg_s-dmbtr = - gw_mseg_h-dmbtr.
        MODIFY gt_mseg_s FROM gw_mseg_s TRANSPORTING menge dmbtr.
      ELSEIF gw_mseg_h-menge > 0.
        MODIFY gt_mseg_h FROM gw_mseg_h INDEX sy-tabix TRANSPORTING menge.
        DELETE gt_mseg_s.
        CONTINUE.
      ELSE.
        DELETE gt_mseg_h INDEX sy-tabix.
        DELETE gt_mseg_s.
      ENDIF.
    ENDIF.
  ENDLOOP.

*  "存入自建表
*  DATA:lt_xjs_fimbage TYPE TABLE OF ztmm_xsj_fimbage.
*  CLEAR :lt_xjs_fimbage.
*  lt_xjs_fimbage = CORRESPONDING #( gt_mseg_s ).
*  LOOP AT lt_xjs_fimbage ASSIGNING FIELD-SYMBOL(<fs_age>).
*    <fs_age>-cxdat = p_budat.
*  ENDLOOP.
*  MODIFY ztmm_xsj_fimbage FROM TABLE lt_xjs_fimbage.
*  COMMIT WORK AND WAIT.

  DATA:lv_month TYPE vtbbewe-atage.
  LOOP AT gt_mseg_s INTO gw_mseg_s.
    READ TABLE gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WITH KEY werks = gw_mseg_s-werks
                                                                lgort = gw_mseg_s-lgort
                                                                matnr = gw_mseg_s-matnr.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO gt_alv ASSIGNING <fs_alv>.
      <fs_alv>-werks = gw_mseg_s-werks.
      <fs_alv>-lgort = gw_mseg_s-lgort.
      <fs_alv>-matnr = gw_mseg_s-matnr.
    ENDIF.
    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        i_date_from = gw_mseg_s-budat
        i_date_to   = p_budat
      IMPORTING
        e_months    = lv_month.

    IF lv_month <= 1.
      <fs_alv>-menge1 = <fs_alv>-menge1 + gw_mseg_s-menge.
      <fs_alv>-dmbtr1 = <fs_alv>-dmbtr1 + gw_mseg_s-dmbtr.
    ELSEIF lv_month > 1 AND lv_month <= 3.
      <fs_alv>-menge2 = <fs_alv>-menge2 + gw_mseg_s-menge.
      <fs_alv>-dmbtr2 = <fs_alv>-dmbtr2 + gw_mseg_s-dmbtr.
    ELSEIF lv_month > 3 AND lv_month <= 6.
      <fs_alv>-menge3 = <fs_alv>-menge3 + gw_mseg_s-menge.
      <fs_alv>-dmbtr3 = <fs_alv>-dmbtr3 + gw_mseg_s-dmbtr.
    ELSEIF lv_month > 6 AND lv_month <= 12.
      <fs_alv>-menge4 = <fs_alv>-menge4 + gw_mseg_s-menge.
      <fs_alv>-dmbtr4 = <fs_alv>-dmbtr4 + gw_mseg_s-dmbtr.
    ELSEIF lv_month > 12 AND lv_month <= 24.
      <fs_alv>-menge5 = <fs_alv>-menge5 + gw_mseg_s-menge.
      <fs_alv>-dmbtr5 = <fs_alv>-dmbtr5 + gw_mseg_s-dmbtr.
    ELSEIF lv_month > 24.
      <fs_alv>-menge6 = <fs_alv>-menge6 + gw_mseg_s-menge.
      <fs_alv>-dmbtr6 = <fs_alv>-dmbtr6 + gw_mseg_s-dmbtr.
    ENDIF.

  ENDLOOP.

  "取物料描述
  SELECT matnr,
         maktx
  INTO TABLE @DATA(lt_makt)
  FROM makt FOR ALL ENTRIES IN @gt_alv
  WHERE matnr = @gt_alv-matnr.
  SORT lt_makt BY matnr.

  LOOP AT gt_alv ASSIGNING <fs_alv>.

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

  PERFORM add_fieldcat USING:'MAKTX' '物品名称' '' ''," 物品名称
  'LGORT' '仓库号' '' ''," 仓库号
  'MENGE1' '当月净重' 'MSEG' 'MENGE'," 当月净重
  'DMBTR1' '当月金额' 'BSEG' 'DMBTR'," 当月金额
  'MENGE2' '1-3个月净重' 'MSEG' 'MENGE'," 1-3个月净重
  'DMBTR2' '1-3个月金额' 'BSEG' 'DMBTR'," 1-3个月金额
  'MENGE3' '4-6个月净重' 'MSEG' 'MENGE'," 4-6个月净重
  'DMBTR3' '4-6个月金额' 'BSEG' 'DMBTR'," 4-6个月金额
  'MENGE4' '7个月-1年净重' 'MSEG' 'MENGE'," 7个月-1年净重
  'DMBTR4' '7个月-1年金额' 'BSEG' 'DMBTR'," 7个月-1年金额
  'MENGE5' '1-2年净重' 'MSEG' 'MENGE'," 1-2年净重
  'DMBTR5' '1-2年金额' 'BSEG' 'DMBTR'," 1-2年金额
  'MENGE6' '2年以上净重' 'MSEG' 'MENGE'," 2年以上净重
  'DMBTR6' '2年以上金额' 'BSEG' 'DMBTR'." 2年以上金额

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

  SELECT bwart AS high
  INTO CORRESPONDING FIELDS OF TABLE r_bwart
  FROM ztmm_chkl_bwart.

  LOOP AT r_bwart.
    r_bwart-option = 'EQ'.
    r_bwart-sign   = 'I'.
    MODIFY r_bwart.
  ENDLOOP.

  SELECT SINGLE MAX( cxdat )
  INTO @DATA(lv_cxdat)
  FROM ztmm_xsj_fimbage
  WHERE cxdat <= @p_budat.

  r_budat[] = VALUE #( ( sign = 'I' option = 'BT' low = lv_cxdat high = p_budat ) ).

  IF lv_cxdat IS NOT INITIAL.
    SELECT * INTO TABLE gt_age_his
    FROM ztmm_xsj_fimbage
    WHERE cxdat = lv_cxdat.
  ENDIF.
ENDFORM.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
