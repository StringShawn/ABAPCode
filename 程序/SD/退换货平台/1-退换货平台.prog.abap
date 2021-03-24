REPORT zsbip0005 MESSAGE-ID zusc_opt.

INCLUDE zsbip0005_top.   "定义 include

"退货单界面
SELECTION-SCREEN BEGIN OF SCREEN 9000 AS SUBSCREEN.
PARAMETERS: p_11 RADIOBUTTON GROUP 1 DEFAULT 'X' USER-COMMAND uc1, "创建
            p_12 RADIOBUTTON GROUP 1, "修改
            p_13 RADIOBUTTON GROUP 1. "查询
PARAMETERS:p_vkorg1 TYPE vbak-vkorg MEMORY ID vko, "采购组织
           p_vtweg1 TYPE vbak-vtweg MODIF ID s11, "分销渠道
           p_spart1 TYPE vbak-spart MODIF ID s11, "产品组
           p_auart1 TYPE vbak-auart MODIF ID s11. "销售凭证类型
SELECT-OPTIONS:s_vgbel1 FOR vbrk-vbeln MODIF ID s11."参考发票号

SELECT-OPTIONS:s_vbelr1 FOR vbak-vbeln MODIF ID s12,"退货单号
               s_vbeln1 FOR vbrk-vbeln MODIF ID s12,"发票号
               s_fkdat1 FOR vbrk-fkdat MODIF ID s12,"发票日期
               s_erdat1 FOR vbrk-erdat MODIF ID s12,"创建日期
               s_kunag1 FOR vbrk-kunag MODIF ID s12."客户
SELECTION-SCREEN END OF SCREEN 9000.

"换货单界面
SELECTION-SCREEN BEGIN OF SCREEN 9001 AS SUBSCREEN.
PARAMETERS: p_21 RADIOBUTTON GROUP 2 DEFAULT 'X' USER-COMMAND uc2, "创建
            p_22 RADIOBUTTON GROUP 2, "修改
            p_23 RADIOBUTTON GROUP 2. "查询

PARAMETERS:p_vkorg2 TYPE vbak-vkorg MEMORY ID vko, "销售组织
           p_vtweg2 TYPE vbak-vtweg MODIF ID s21. "分销渠道
SELECT-OPTIONS s_vbelr2 FOR vbak-vbeln."退货单号

SELECT-OPTIONS:s_vbelh2 FOR vbak-vbeln MODIF ID s22,
               s_vbeln2 FOR vbrk-vbeln MODIF ID s22,
*               s_vbelr2 FOR vbak-vbeln MODIF ID s22,
               s_audat2 FOR vbak-audat MODIF ID s22,
               s_kunnr2 FOR vbak-kunnr MODIF ID s22.
SELECTION-SCREEN END OF SCREEN 9001.

SELECTION-SCREEN BEGIN OF SCREEN 9002 AS SUBSCREEN.
PARAMETERS: p_31 RADIOBUTTON GROUP 3 DEFAULT 'X' USER-COMMAND uc3,
            p_32 RADIOBUTTON GROUP 3,
            p_33 RADIOBUTTON GROUP 3.
SELECT-OPTIONS:s_vkorg3 FOR vbak-vkorg MODIF ID s32,
               s_vbelh3 FOR vbak-vbeln,
               s_vbeln3 FOR likp-vbeln MODIF ID s32,
               s_audat3 FOR likp-erdat MODIF ID s32,
               s_kunnr3 FOR vbak-kunnr.
PARAMETERS p_vstel3 TYPE likp-vstel MODIF ID s31 MEMORY ID vst.
SELECTION-SCREEN END OF SCREEN 9002.

SELECTION-SCREEN: BEGIN OF TABBED BLOCK tblk FOR 10 LINES,
                  TAB (20) button1 USER-COMMAND push1
                                   DEFAULT SCREEN 9000,
                  TAB (20) button2 USER-COMMAND push2
                                   DEFAULT SCREEN 9001,
                  TAB (20) button3 USER-COMMAND push3
                                   DEFAULT SCREEN 9002,
                  END OF BLOCK tblk.

INITIALIZATION.
  button1 = '退货单页签'.
  button2 = '换货单页签'.
  button3 = '换货出货单页签'.

AT SELECTION-SCREEN OUTPUT.
  CASE 'X'.
    WHEN p_11.
      LOOP AT SCREEN.
        IF screen-group1 = 'S11'.
          screen-input = 1.
          screen-invisible = 0.
        ELSEIF screen-group1 = 'S12'.
          screen-input = 0.
          screen-invisible = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
      tblk-dynnr = 9000.
      tblk-activetab = 'PUSH1'.
    WHEN p_12 OR p_13.
      LOOP AT SCREEN.
        IF screen-group1 = 'S12'.
          screen-input = 1.
          screen-invisible = 0.
        ELSEIF screen-group1 = 'S11'.
          screen-input = 0.
          screen-invisible = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
      tblk-dynnr = 9000.
      tblk-activetab = 'PUSH1'.
    WHEN p_21.
      LOOP AT SCREEN.
        IF screen-group1 = 'S21'.
          screen-input = 1.
          screen-invisible = 0.
        ELSEIF screen-group1 = 'S22'.
          screen-input = 0.
          screen-invisible = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
      tblk-dynnr = 9001.
      tblk-activetab = 'PUSH2'.
    WHEN p_22 OR p_23.
      LOOP AT SCREEN.
        IF screen-group1 = 'S22'.
          screen-input = 1.
          screen-invisible = 0.
        ELSEIF screen-group1 = 'S21'.
          screen-input = 0.
          screen-invisible = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
      tblk-dynnr = 9001.
      tblk-activetab = 'PUSH2'.
    WHEN p_31.
      LOOP AT SCREEN.
        IF screen-group1 = 'S32'.
          screen-input = 0.
          screen-invisible = 1.
        ELSEIF screen-group1 = 'S31'.
          screen-input = 1.
          screen-invisible = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
      tblk-dynnr = 9002.
      tblk-activetab = 'PUSH3'.
    WHEN p_32 OR p_33.
      LOOP AT SCREEN.
        IF screen-group1 = 'S32'.
          screen-input = 1.
          screen-invisible = 0.
        ELSEIF screen-group1 = 'S31'.
          screen-input = 0.
          screen-invisible = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
      tblk-dynnr = 9002.
      tblk-activetab = 'PUSH3'.
  ENDCASE.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'PUSH1'.
      CLEAR:p_21,p_22,p_23,p_31,p_32,p_33.
      p_11 = 'X'.
    WHEN 'PUSH2'.
      CLEAR:p_11,p_12,p_13,p_31,p_32,p_33.
      p_21 = 'X'.
    WHEN 'PUSH3'.
      CLEAR:p_11,p_12,p_13,p_21,p_22,p_23.
      p_31 = 'X'.
    WHEN 'ONLI'.
      IF p_11 = 'X' OR p_12 = 'X' OR p_13 = 'X'.
        CLEAR:p_21,p_22,p_23,p_31,p_32,p_33.
      ELSEIF p_21 = 'X' OR p_22 = 'X' OR p_23 = 'X'.
        CLEAR:p_11,p_12,p_13,p_31,p_32,p_33.
      ELSEIF p_31 = 'X' OR p_32 = 'X'.
        CLEAR:p_11,p_12,p_13,p_21,p_22,p_23.
      ENDIF.
      CASE 'X'.
        WHEN p_11.
          zcl_usc_opt_util=>check_not_null( i_fieldname = '销售组织' i_value = p_vkorg1 ).
          zcl_usc_opt_util=>check_not_null( i_fieldname = '分销渠道' i_value = p_vtweg1 ).
          zcl_usc_opt_util=>check_not_null( i_fieldname = '产品组'   i_value = p_spart1 ).
          zcl_usc_opt_util=>check_not_null( i_fieldname = '退货单类型' i_value = p_auart1 ).
        WHEN p_12 OR p_13.
          zcl_usc_opt_util=>check_not_null( i_fieldname = '销售组织' i_value = p_vkorg1 ).
        WHEN p_21.
          zcl_usc_opt_util=>check_not_null( i_fieldname = '销售组织' i_value = p_vkorg2 ).
          zcl_usc_opt_util=>check_not_null( i_fieldname = '分销渠道' i_value = p_vtweg2 ).
          zcl_usc_opt_util=>check_not_null( i_fieldname = '退货单号' i_value = s_vbelr2 ).
        WHEN p_22 OR p_23.
          zcl_usc_opt_util=>check_not_null( i_fieldname = '销售组织' i_value = p_vkorg2 ).
        WHEN p_31.
          zcl_usc_opt_util=>check_not_null( i_fieldname = '换货单号' i_value = s_vbelh3 ).
          zcl_usc_opt_util=>check_not_null( i_fieldname = '装运点' i_value = p_vstel3 ).
      ENDCASE.
  ENDCASE.


START-OF-SELECTION.

  CASE 'X'.
    WHEN p_11.
      PERFORM prepare_return_data.
    WHEN p_12 OR p_13.
      PERFORM get_return_data.
    WHEN p_21.
      PERFORM prepare_replace_data.
    WHEN p_22.
      PERFORM get_replace_edit.
    WHEN p_23.
      PERFORM get_replace_data.
    WHEN p_31.
      PERFORM prepare_replace_delivery.
    WHEN p_32 OR p_33.
      PERFORM get_replace_delivery.
  ENDCASE.

  PERFORM display_alv.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  PREPARE_RETURN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM prepare_return_data .


  DATA: BEGIN OF ls_od,
          front_ord     TYPE  char30,  "  厂内上段制程
          front_prd_ord TYPE  char30,    "上段工单号
          front_proc    TYPE  char30,    "上段制程
          prd_ord       TYPE  char30,   " 本段工单号
          proc          TYPE  char4,   "  本段制程
          wafer_lot     TYPE  char30,   " WAFER_LOT
          mes_bp_batch  TYPE  char30,
          mes_cp_batch  TYPE  char30,
          mes_cog_batch TYPE  char30,
          mes_cof_batch TYPE  char30,
        END  OF ls_od.
  TYPES: BEGIN OF ty_vbap,
           vbeln  TYPE vbap-vbeln,
           posnr  TYPE vbap-posnr,
           vgbel  TYPE vbap-vgbel,
           vgpos  TYPE vbap-vgpos,
           kwmeng TYPE vbap-kwmeng,
         END OF ty_vbap.
  DATA:lt_alv TYPE TABLE OF ty_alv.
  DATA:lt_vbap TYPE TABLE OF ty_vbap,
       ls_vbap TYPE ty_vbap.



  SELECT vbrk~vbeln AS vbeln_vf
         posnr AS posnr_vf
         kunag
         vkorg
         matnr
         arktx
         fkimg
         vrkme
         vgbel AS vbeln_vl
         vgpos AS posnr_vl
         charg
         aubel AS vbeln_va
         aupos AS posnr_va
  INTO CORRESPONDING FIELDS OF TABLE gt_alv
  FROM vbrk JOIN vbrp
    ON vbrk~vbeln = vbrp~vbeln
  WHERE vbrk~vbeln IN s_vgbel1
    AND vbrk~vkorg = p_vkorg1
    AND vbrp~vgtyp = 'J'.

  IF gt_alv IS NOT INITIAL.
    SELECT vbeln
           posnr
           vgbel
           vgpos
           kwmeng
    INTO TABLE lt_vbap
    FROM vbap FOR ALL ENTRIES IN gt_alv
    WHERE vgbel = gt_alv-vbeln_vf
      AND vgpos = gt_alv-posnr_vf.
  ENDIF.

  LOOP AT gt_alv ASSIGNING <fs_alv>.
    CLEAR l_sn.
    CALL FUNCTION 'ZFM_READ_BAT_CHARA'
      EXPORTING
        matnr           = <fs_alv>-matnr
        charg           = <fs_alv>-charg
      IMPORTING
        front_ord       = ls_od-front_ord  "  厂内上段制程
        front_prd_ord   = ls_od-front_prd_ord    "上段工单号
        front_proc      = ls_od-front_proc    "上段制程
        prd_ord         = ls_od-prd_ord   " 本段工单号
        proc            = ls_od-proc   "  本段制程
        wafer_lot       = ls_od-wafer_lot   " WAFER_LOT
        mes_bp_batch    = ls_od-mes_bp_batch
        mes_cp_batch    = ls_od-mes_cp_batch
        mes_cog_batch   = ls_od-mes_cog_batch
        mes_cof_batch   = ls_od-mes_cof_batch
      EXCEPTIONS
        batch_not_found = 1
        OTHERS          = 2.
    IF ls_od-proc = 'BP'.
      <fs_alv>-baselotno =  ls_od-mes_bp_batch.
      <fs_alv>-sn        = '3'.
    ELSEIF ls_od-proc = 'CP'.
      <fs_alv>-baselotno = ls_od-mes_cp_batch.
      <fs_alv>-sn        = '2'.
    ELSEIF ls_od-proc = 'COG'.
      <fs_alv>-baselotno = ls_od-mes_cog_batch.
      <fs_alv>-sn        = '1'.
    ELSEIF ls_od-proc = 'COF'.
      <fs_alv>-baselotno = ls_od-mes_cof_batch.
      <fs_alv>-sn        = '0'.
    ENDIF .
    LOOP AT lt_vbap INTO ls_vbap WHERE vgbel = <fs_alv>-vbeln_vf AND
                                     vgpos = <fs_alv>-posnr_vf.
      ADD ls_vbap-kwmeng TO <fs_alv>-menge_re.
      IF ls_vbap-vbeln NE <fs_alv>-vbeln_re OR ls_vbap-posnr NE <fs_alv>-posnr_re.
        ADD ls_vbap-kwmeng TO <fs_alv>-menge_ex.
      ENDIF.
    ENDLOOP.
    <fs_alv>-proc      = ls_od-proc.
    <fs_alv>-wafer_lot = ls_od-wafer_lot.
*    <fs_alv>-menge = <fs_alv>-fkimg - <fs_alv>-menge_re.
    <fs_alv>-aufnr     = ls_od-prd_ord.
    <fs_alv>-auart = p_auart1.

  ENDLOOP.

  DELETE gt_alv WHERE fkimg = 0.
  SORT gt_alv BY vbeln_vf sn ASCENDING.

  SELECT * INTO TABLE lt_zppt001 FROM zppt001 FOR ALL ENTRIES IN gt_alv
  WHERE aufnr = gt_alv-aufnr
    AND  zlevel = '5' AND zcanc = ''.

  LOOP AT gt_alv INTO gs_alv WHERE zgroup IS INITIAL.
    ADD 1 TO lv_zgroup.
    gs_alv-zgroup = lv_zgroup.
    MODIFY gt_alv FROM gs_alv TRANSPORTING zgroup.
    PERFORM set_zgroup USING gs_alv-aufnr gs_alv-wafer_lot.

  ENDLOOP.
  SORT gt_alv BY vbeln_vf zgroup sn posnr_vf.
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
  PERFORM set_fieldcat.
  PERFORM set_alv_title.
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
  CASE 'X'.
    WHEN  p_12 OR p_21 OR p_22 OR p_31 OR p_32.
      gs_layo-box_fname  = 'BOX'.
    WHEN OTHERS.
  ENDCASE.

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
  CLEAR gt_fcat.
  CASE 'X'.
    WHEN p_11.
      PERFORM add_fieldcat USING:   'BOX'          '选择列'       '' '' 'X',
                                    'ID'           '状态灯'       '' '' '',
                                    'VBELN_VF'     'SAP发票号'    ''     ''     '',
                                     'POSNR_VF'    '行号'        ''     ''     '',
                                     'KUNAG'       '客户'        'VBAK'     'KUNNR'     '',
                                   'VKORG'         '销售组织'      'VBAK'     'VKORG'     'X',
                                   'AUART'         '退货单类型'      'VBAK'     'AUART'     'X',
                                    'MATNR'        '物料号'       ''     ''     '',
                                   'ARKTX'         '物料描述'      ''     ''     '',
                                   'FKIMG'         '发票数量'      ''     ''     '',
                                   'MENGE_EX'      '已退货数量'      ''     ''     '',
                                   'VBELN_VL'      '出货单号'      ''     ''     '',
                                  'POSNR_VL'       '出货单行号'     ''     ''     '',
                                   'CHARG'         'SAP产出批次'   ''     ''     '',
                                   'BASELOTNO'     'MES产出批次'   ''     ''     '',
                                       'WAFER_LOT' 'WAFER LOT'   ''     ''     '',
                                     'ZGROUP'      '组数'        '' '' '',
                                     'SN'          '层数'        ''      ''     '',
                                   'VBELN_VA'      '销售单号'      ''     ''     '',
                                     'POSNR_VA'    '行号'          ''     ''     '',
                                   'MENGE'         '退货数量'      'MSEG'     'MENGE'     'X',
                                   'VBELN_RE'      '退货单号'      ''     ''     '',
                                     'MESS'        '消息'        ''     ''     ''.
    WHEN p_12.
      PERFORM add_fieldcat USING: 'ID'           '状态灯'       '' '' '',
              'VKORG    ' '销售组织' '' '' ''," 销售组织
              'AUART    ' '退货单类型' '' '' ''," 退货单类型
              'VBELN_RE ' '退货单号' '' '' ''," 退货单号
              'KUNAG    ' '客户编号' 'VBAK' 'KUNNR' ''," 客户编号
              'NAME1    ' '客户名称' '' '' ''," 客户名称
              'POSNR_RE' '退货单行号' '' '' ''," 退货单行号
              'MATNR    ' '退货物料号' '' '' ''," 退货物料号
              'ARKTX' '退货物料描述' '' '' ''," 退货物料描述
              'MENGE' '退货数量' 'MSEG' 'MENGE' 'X'," 退货数量
              'VBELN_VF' '参考发票号' '' '' ''," 参考发票号
              'POSNR_VF' '发票行号' '' '' ''," 发票行号
              'FKIMG' '发票数量' 'VBAP' 'FKIMG' ''," 发票数量
              'MENGE_EX'  '非本单退货数量'      ''     ''     '',
              'CHARG' 'SAP产出批次' '' '' ''," SAP产出批次
              'BASELOTNO' 'MES产出批次' '' '' ''," MES产出批次
              'WAFER_LOT' 'WAFER LOT' '' '' ''," WAFER LOT
              'MESS'        '消息'        ''     ''     ''.
    WHEN p_13.
      PERFORM add_fieldcat USING:'VKORG    ' '销售组织' '' '' ''," 销售组织
              'AUART    ' '退货单类型' '' '' ''," 退货单类型
              'VBELN_RE ' '退货单号' '' '' ''," 退货单号
              'KUNAG    ' '客户编号' 'VBAK' 'KUNNR' ''," 客户编号
              'NAME1    ' '客户名称' '' '' ''," 客户名称
              'POSNR_RE' '退货单行号' '' '' ''," 退货单行号
              'MATNR    ' '退货物料号' '' '' ''," 退货物料号
              'ARKTX' '退货物料描述' '' '' ''," 退货物料描述
              'MENGE' '退货数量' 'MSEG' 'MENGE' ''," 退货数量
              'VBELN_VF' '参考发票号' '' '' ''," 参考发票号
              'POSNR_VF' '发票行号' '' '' ''," 发票行号
              'FKIMG' '发票数量' 'VBAP' 'FKIMG' ''," 发票数量
              'CHARG' 'SAP产出批次' '' '' ''," SAP产出批次
              'BASELOTNO' 'MES产出批次' '' '' ''," MES产出批次
              'WAFER_LOT' 'WAFER LOT' '' '' ''." WAFER LOT
    WHEN p_21.
      PERFORM add_fieldcat USING:'ID  ' '状态灯' '' '' ''," 状态灯
      'VKORG    ' '销售组织' 'VBAK' 'VKORG    ' 'X'," 销售组织
      'VTWEG    ' '分销渠道' 'VBAK' 'VTWEG    ' 'X'," 分销渠道
      'SPART    ' '产品组' 'VBAK' 'SPART    ' 'X'," 产品组
      'AUART    ' '换货单类型' 'VBAK' 'AUART    ' 'X'," 换货单类型
      'AUART_TXT' '换货单类型描述' '' '    ' ''," 换货单类型描述
      'KUNAG    ' '客户编码' 'VBAK' 'KUNNR    ' 'X'," 客户编码
      'NAME1    ' '客户名称' 'VBAK' 'NAME1    ' ''," 客户名称
      'VBELN_RE ' '退单号' 'VBAK' 'VBELN_RE ' ''," 退货单号
      'KUNAG_ZD ' '换货单送达方' 'VBAK' 'KUNNR ' 'X'," 换货单送达方
      'NAME1_SH' '换货单送达方描述' '' '' ''," 换货单送达方描述
'STREET' '换货单送达方地址' '' '' ''," 换货单送达方地址
*      'KUNAG_FZD' '非终段送达方' 'VBAK' 'KUNNR' 'X'," 非终段送达方
      'POSNR_RE' '行号' 'VBAP' 'POSNR    ' ''," 行号
      'MATNR    ' '物料编码' 'VBAP' 'MATNR    ' ''," 物料编码
      'ARKTX    ' '物料描述' 'VBAP' 'ARKTX    ' ''," 物料描述
      'LGORT    ' '库存地点' 'VBAP' 'LGORT    ' ''," 库存地点
      'KWMENG   ' '退货数量' 'VBAP' 'KWMENG   ' ''," 退货数量
      'MENGE    ' '换货数量' 'MSEG' 'MENGE    ' 'X'," 换货数量
      'VBELN_HH ' '换货单号' 'VBAK' 'VBELN ' ''," 换货单号
      'MESS' '消息' '' '     ' ''." 消息
    WHEN p_22.
      PERFORM add_fieldcat USING:'ID  ' '状态灯' '' '' ''," 状态灯
            'VKORG    ' '销售组织' '' '' ''," 销售组织
'AUART    ' '换货单类型' '' '' ''," 换货单类型
'VBELN_HH' '换货单号' '' '' ''," 换货单号
'KUNAG' '换货单售达方' 'VBAK' 'KUNNR' ''," 换货单售达方
'NAME1    ' '换货单售达方描述' '' '' ''," 换货单售达方描述
'KUNAG_ZD' '换货单送达方' 'VBAK' 'KUNNR' ''," 换货单送达方
'NAME1_SH' '换货单送达方描述' '' '' ''," 换货单送达方描述
'STREET' '换货单送达方地址' '' '' ''," 换货单送达方地址
'POSNR_HH' '换货单行号' '' '' ''," 换货单行号
'MATNR' '换货单物料号' '' '' ''," 换货单物料号
'ARKTX' '换货单物料描述' '' '' ''," 换货单物料描述
'LGORT    ' '库存地点' 'VBAP' 'LGORT    ' ''," 库存地点
'VBELN_RE' '退货单号' '' '' ''," 退货单号
'MENGE' '换货单数量' 'MSEG' 'MENGE' 'X'," 换货单数量
'MESS' '消息' '' '     ' ''." 消息.
    WHEN p_23.
      PERFORM add_fieldcat USING:'VKORG    ' '销售组织' '' '' ''," 销售组织
'AUART    ' '换货单类型' '' '' ''," 换货单类型
'VBELN_HH' '换货单号' '' '' ''," 换货单号
'KUNAG' '换货单售达方' 'VBAK' 'KUNNR' ''," 换货单售达方
'NAME1    ' '换货单售达方描述' '' '' ''," 换货单售达方描述
'KUNAG_ZD' '换货单送达方' 'VBAK' 'KUNNR' ''," 换货单送达方
'NAME1_SH' '换货单送达方描述' '' '' ''," 换货单送达方描述
'STREET' '换货单送达方地址' '' '' ''," 换货单送达方地址
'POSNR_HH' '换货单行号' 'VBAP' 'POSNR' ''," 换货单行号
'MATNR' '换货单物料号' '' '' ''," 换货单物料号
'ARKTX' '换货单物料描述' '' '' ''," 换货单物料描述
'CHARG' '批次' '' '' ''," 批次
'LGORT    ' '库存地点' 'VBAP' 'LGORT    ' ''," 库存地点
'VBELN_RE' '退货单号' 'VBAP' 'VBELN' ''," 退货单号
'POSNR_RE' '退货单行号' 'VBAP' 'POSNR' ''," 退货单行号
'MENGE' '退货单数量' '' '' ''." 换货单数量

    WHEN p_31.
      PERFORM add_fieldcat USING:'ID  ' '状态灯' '' '' ''," 状态灯
'VKORG    ' '销售组织' '' '' ''," 销售组织
'VBELN_CK' '换货出库单号' '' '' ''," 换货出库单号
'VBELN_HH ' '换货单号' '' '' ''," 换货单号
'KUNAG' '客户编码' 'VBAK' 'KUNNR' ''," 客户编码
'NAME1    ' '客户名称' '' '' ''," 客户名称
'KUNAG_ZD ' '送达方编码' 'VBAK' 'KUNNR' ''," 送达方编码
'NAME1_SH' '送达方名称' '' '' ''," 送达方名称
'STREET' '送达方地址' '' '' ''," 送达方地址
'POSNR_HH' '换货单行号' '' '' ''," 换货单行号
'MATNR    ' '物料编码' '' '' ''," 物料编码
'ARKTX    ' '物料描述' '' '' ''," 物料描述
'KWMENG   ' '换货单数量' '' '' ''," 换货单数量
'POSNR_CK' '出货单行号' '' '' ''," 出库单行号
'LGORT' '出货仓位' 'T001L' 'LGORT' 'X'," 出货仓位
'POSNR_PC' '批次行号' '' '' ''," 批次行号
'CHARG' '批次' '' '' ''," 批次
'MENGE' '换货出货单数量' 'MSEG' 'MENGE' 'X'," 换货出货单数量
'MESS' '消息' '' '' ''." 消息
    WHEN p_32.
      PERFORM add_fieldcat USING:'ID  ' '状态灯' '' '' ''," 状态灯
'VKORG    ' '销售组织' '' '' ''," 销售组织
'VBELN_CK' '换货出库单号' '' '' ''," 换货出库单号
'KUNAG' '客户编码' 'VBAK' 'KUNNR' ''," 客户编码
'NAME1    ' '客户名称' '' '' ''," 客户名称
'KUNAG_ZD ' '送达方编码' 'VBAK' 'KUNNR' ''," 送达方编码
'NAME1_SH' '送达方名称' '' '' ''," 送达方名称
'STREET' '送达方地址' '' '' ''," 送达方地址
'MATNR    ' '物料编码' '' '' ''," 物料编码
'ARKTX    ' '物料描述' '' '' ''," 物料描述
'LGORT' '出货仓位' '' '' ''," 出货仓位
'POSNR_CK' '出货单行号' '' '' ''," 出货单行号
'POSNR_PC' '批次行号' '' '' ''," 批次行号
'CHARG' '批次' '' '' ''," 批次
'MENGE' '出货单数量' 'MSEG' 'MENGE' 'X'," 出货单数量
'VBELN_HH' '换货单号' '' '' ''," 换货单号
'POSNR_HH' '换货单行号' '' '' ''," 换货单行号
'KWMENG' '换货单数量' 'MSEG' 'KWMENG' ''," 换货单数量
'MESS' '消息' '' '' ''." 消息

    WHEN p_33.
      PERFORM add_fieldcat USING:'VKORG    ' '销售组织' '' '' ''," 销售组织
'VBELN_CK' '换货出库单号' '' '' ''," 换货出库单号
'KUNAG' '客户编码' 'VBAK' 'KUNNR' ''," 客户编码
'NAME1    ' '客户名称' '' '' ''," 客户名称
'KUNAG_ZD ' '送达方编码' 'VBAK' 'KUNNR' ''," 送达方编码
'NAME1_SH' '送达方名称' '' '' ''," 送达方名称
'STREET' '送达方地址' '' '' ''," 送达方地址
'MATNR    ' '物料编码' '' '' ''," 物料编码
'ARKTX    ' '物料描述' '' '' ''," 物料描述
'LGORT' '出货仓位' '' '' ''," 出货仓位
'POSNR_CK' '出货单行号' '' '' ''," 出货单行号
'POSNR_PC' '批次行号' '' '' ''," 批次行号
'CHARG' '批次' '' '' ''," 批次
'MENGE' '出货单数量' 'MSEG' 'MENGE' ''," 出货单数量
'VBELN_HH' '换货单号' '' '' ''," 换货单号
'POSNR_HH' '换货单行号' '' '' ''," 换货单行号
'KWMENG' '换货单数量' 'MSEG' 'KWMENG' ''." 换货单数量
  ENDCASE.

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
  DATA:lt_event TYPE slis_t_event,
       ls_event TYPE slis_alv_event.

  CASE 'X'.
    WHEN p_11 OR p_21 OR p_12.
      CLEAR:ls_event,lt_event.
      ls_event-name = slis_ev_data_changed.
      ls_event-form = 'DATA_CHANGED'.
      APPEND ls_event TO lt_event.
      CLEAR:ls_event.
      ls_event-name = slis_ev_caller_exit_at_start.
      ls_event-form = 'CALLER_EXIT'.
      APPEND ls_event TO lt_event.
      CLEAR:ls_event.
  ENDCASE.
  CASE 'X'.
    WHEN p_11 OR p_12 OR p_13.
      ASSIGN gt_alv TO <ft_alv>.
    WHEN p_21 OR p_22 OR p_23.
      ASSIGN gt_alv1 TO <ft_alv>.
    WHEN p_31 OR p_32 OR p_33.
      ASSIGN gt_alv2 TO <ft_alv>.
  ENDCASE.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'USER_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout_lvc            = gs_layo
      it_fieldcat_lvc          = gt_fcat
      it_events                = lt_event
      i_grid_title             = gs_title
      i_save                   = 'A'
    TABLES
      t_outtab                 = <ft_alv>
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0798   text
*      -->P_0799   text
*      -->P_0800   text
*      -->P_0801   text
*      -->P_0802   text
*----------------------------------------------------------------------*
FORM add_fieldcat  USING   VALUE(p_fieldname)
                            VALUE(p_coltext)
                            VALUE(p_ref_tab)
                            VALUE(p_ref_field)
                            VALUE(p_edit).
  CLEAR:gs_fcat.
  IF p_11 = 'X' AND p_fieldname = 'BOX'.
    gs_fcat-checkbox = 'X'.
  ENDIF.
  gs_fcat-fieldname = p_fieldname.
  gs_fcat-coltext   = p_coltext.
  gs_fcat-ref_table = p_ref_tab.
  gs_fcat-ref_field = p_ref_field.
  gs_fcat-edit      = p_edit.
  APPEND gs_fcat TO gt_fcat.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_status USING extab TYPE slis_t_extab.
  DATA:ls_extab TYPE slis_extab.

  CASE 'X'.
*    WHEN p_11.
*      DELETE extab WHERE fcode = '&CREATE'.
*      ls_extab-fcode = '&SAVE'.
*      APPEND ls_extab TO extab.
    WHEN p_12.
      DELETE extab WHERE fcode = '&SAVE'.
      ls_extab-fcode = '&CREATE'.
      APPEND ls_extab TO extab.
    WHEN p_13.
      ls_extab-fcode = '&CREATE'.
      APPEND ls_extab TO extab.
      ls_extab-fcode = '&SAVE'.
      APPEND ls_extab TO extab.
    WHEN p_22.
      DELETE extab WHERE fcode = '&SAVE1'.
      ls_extab-fcode = '&CREATE1'.
      APPEND ls_extab TO extab.
    WHEN p_23.
      ls_extab-fcode = '&CREATE1'.
      APPEND ls_extab TO extab.
      ls_extab-fcode = '&SAVE1'.
      APPEND ls_extab TO extab.
    WHEN p_31.
      DELETE extab WHERE fcode = '&CREATE2'.
      ls_extab-fcode = '&SAVE2'.
      APPEND ls_extab TO extab.
    WHEN p_32.
      DELETE extab WHERE fcode = '&SAVE2'.
      ls_extab-fcode = '&CREATE2'.
      APPEND ls_extab TO extab.
    WHEN p_33.
      ls_extab-fcode = '&CREATE2'.
      APPEND ls_extab TO extab.
      ls_extab-fcode = '&SAVE2'.
      APPEND ls_extab TO extab.
    WHEN OTHERS.
  ENDCASE.


  CASE 'X'.
    WHEN p_11 OR p_12 OR p_13.
      SET PF-STATUS 'STANDARD1' EXCLUDING extab.
    WHEN p_21 OR p_22 OR p_23.
      SET PF-STATUS 'STANDARD2' EXCLUDING extab.
    WHEN p_31 OR p_32 OR p_33.
      SET PF-STATUS 'STANDARD3' EXCLUDING extab.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command USING pv_ucomm LIKE sy-ucomm
                        ps_selfield TYPE slis_selfield.
  DATA:lo_grid TYPE REF TO cl_gui_alv_grid.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lo_grid.

  lo_grid->check_changed_data( ).

  CASE 'X'.
    WHEN p_11.
      CASE pv_ucomm.
        WHEN '&CREATE'.
          PERFORM create_return_order.
        WHEN '&SAVE'.
          PERFORM change_return_order.
        WHEN OTHERS.
      ENDCASE.
    WHEN p_12.
      IF pv_ucomm = '&SAVE'.
        PERFORM change_return_order.
      ENDIF.
    WHEN p_21.
      CASE pv_ucomm.
        WHEN '&CREATE1'.
          PERFORM create_replace_order.
        WHEN '&SAVE1'.
          PERFORM change_replace_order.
        WHEN OTHERS.
      ENDCASE.
    WHEN p_22.
      CASE pv_ucomm.
        WHEN '&SAVE1'.
          PERFORM change_replace_order.
        WHEN OTHERS.
      ENDCASE.
    WHEN p_31.
      IF pv_ucomm = '&CREATE2'.
        PERFORM create_replace_delivery.
      ENDIF.
    WHEN p_32.
      IF pv_ucomm = '&SAVE2'.
        PERFORM change_replace_delivery.
      ENDIF.
  ENDCASE.

  ps_selfield-refresh = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_RETURN_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_return_order .
  TYPES:BEGIN OF ly_vgbel,
          vbeln_vf TYPE vbrk-vbeln,
          vbeln_re TYPE vbak-vbeln,
          mess     TYPE bapi_msg,
          id       TYPE icon-id,
        END OF ly_vgbel.

  TYPES:BEGIN OF ly_zgroup,
          zgroup TYPE i,
        END OF ly_zgroup.

  DATA:lt_vgbel TYPE TABLE OF ly_vgbel,
       ls_vgbel TYPE ly_vgbel.
  DATA:lt_alv TYPE TABLE OF ty_alv.
  DATA:lt_alv_temp TYPE TABLE OF ty_alv.
  DATA:ls_alv TYPE ty_alv.
  DATA:lv_menge TYPE bdc_fval.
  DATA:temp_posnr TYPE vbap-posnr.
  DATA lv_mode TYPE ctu_params-dismode VALUE 'N'.
  DATA:lv_mess  TYPE bapi_msg,
       lv_id    TYPE icon-id,
       g_vbeln  TYPE vbak-vbeln,
       err_flag TYPE abap_bool.

  DATA:lt_zsdt003 TYPE TABLE OF zsdt003,
       ls_zsdt003 TYPE zsdt003.

*  DATA:lt_zgroup TYPE TABLE OF ly_zgroup,
*       ls_zgroup TYPE ly_zgroup.

  READ TABLE gt_alv TRANSPORTING NO FIELDS WITH KEY box = 'X'.
  IF sy-subrc NE 0.
    MESSAGE e002.
  ENDIF.
  CLEAR:err_flag, lt_alv.
  LOOP AT gt_alv ASSIGNING <fs_alv> WHERE box = 'X'.
    CLEAR <fs_alv>-mess.
    IF <fs_alv>-menge <= 0.
      <fs_alv>-id = icon_led_red.
      <fs_alv>-mess = '数量必须大于0'.
      err_flag = abap_true.
    ENDIF.
    CHECK <fs_alv>-mess IS INITIAL.
    IF <fs_alv>-menge > <fs_alv>-fkimg - <fs_alv>-menge_re.
      <fs_alv>-id = icon_led_red.
      <fs_alv>-mess = '退货数量不能大于发票数量减退货量'.
      err_flag = abap_true.
    ENDIF.
    CHECK <fs_alv>-mess IS INITIAL.
*    ls_zgroup-zgroup = <fs_alv>-zgroup.
*    APPEND ls_zgroup TO lt_zgroup.
*    CLEAR:ls_zgroup.
    APPEND <fs_alv> TO lt_alv.
  ENDLOOP.
  CHECK err_flag = space.
*  SORT lt_zgroup BY zgroup.
*  DELETE ADJACENT DUPLICATES FROM lt_zgroup COMPARING ALL FIELDS.
*  LOOP AT lt_zgroup INTO ls_zgroup.
*    LOOP AT gt_alv ASSIGNING <fs_alv> WHERE zgroup = ls_zgroup-zgroup.
*      CLEAR <fs_alv>-mess.
*      IF <fs_alv>-menge <= 0.
*        <fs_alv>-id = icon_led_red.
*        <fs_alv>-mess = '数量必须大于0'.
*        err_flag = abap_true.
*      ENDIF.
*      CHECK <fs_alv>-mess IS INITIAL.
*      IF <fs_alv>-menge > <fs_alv>-fkimg.
*        <fs_alv>-id = icon_led_red.
*        <fs_alv>-mess = '退货订单数量不能大于开票数量'.
*        err_flag = abap_true.
*      ENDIF.
*      CHECK <fs_alv>-mess IS INITIAL.
*      APPEND <fs_alv> TO lt_alv.
*    ENDLOOP.
*  ENDLOOP.

  CHECK err_flag = space.

  SORT lt_alv BY vbeln_vf posnr_vf.
  DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING vbeln_vf posnr_vf.
  LOOP AT lt_alv INTO gs_alv  .

*&---如果该条数据已经正式导入成功，则不再导入
    IF gs_alv-id = icon_led_green.
      CONTINUE.
    ENDIF.
    APPEND gs_alv TO lt_alv_temp.

    AT END OF vbeln_vf.

      SORT lt_alv_temp BY posnr_vf.

      READ TABLE lt_alv INTO gs_alv INDEX sy-tabix.

      PERFORM bdc_dynpro      USING 'SAPMV45A' '0101'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'VBAK-VKORG'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=COPY'.
      PERFORM bdc_field       USING 'VBAK-AUART'
                                    gs_alv-auart.
      PERFORM bdc_field       USING 'VBAK-VKORG'
                                    gs_alv-vkorg.
      PERFORM bdc_field       USING 'VBAK-VTWEG'
                                    p_vtweg1.
      PERFORM bdc_field       USING 'VBAK-SPART'
                                    p_spart1.
      PERFORM bdc_dynpro      USING 'SAPLV45C' '0100'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=RFAK'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'LV45C-VBELN'.
      PERFORM bdc_dynpro      USING 'SAPLV45C' '0100'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=RUEF'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'VBRK-VBELN'.
      PERFORM bdc_field       USING 'VBRK-VBELN'
                                    gs_alv-vbeln_vf.

      PERFORM bdc_dynpro      USING 'SAPLV60P' '4413'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'VDICS-VBELN'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=MKLO'.
      LOOP AT lt_alv_temp INTO ls_alv.
        PERFORM bdc_dynpro      USING 'SAPLV60P' '4413'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'VDICS-VBELN'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=POPO'.
        PERFORM bdc_dynpro      USING 'SAPLV60P' '0251'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'RV45A-POSNR'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=POSI'.
        PERFORM bdc_field       USING 'RV45A-POSNR'
                                              ls_alv-posnr_vf.
        PERFORM bdc_dynpro      USING 'SAPLV60P' '4413'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'VDICS-VBELN'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=MARK'.
      ENDLOOP.

      PERFORM bdc_dynpro      USING 'SAPLV60P' '4413'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'VDICS-VBELN'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=RUEB'.
      CLEAR temp_posnr.
      LOOP AT lt_alv_temp INTO ls_alv.
        ADD 10 TO temp_posnr.
        PERFORM bdc_dynpro      USING 'SAPMV45A' '4001'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=POPO'.
        PERFORM bdc_dynpro      USING 'SAPMV45A' '0251'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'RV45A-POSNR'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=POSI'.
        PERFORM bdc_field       USING 'RV45A-POSNR'
                                      temp_posnr.
        PERFORM bdc_dynpro      USING 'SAPMV45A' '4001'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
        lv_menge = ls_alv-menge.
        CONDENSE lv_menge.
        PERFORM bdc_field       USING 'RV45A-KWMENG(01)'
                                      lv_menge.
*        PERFORM bdc_field       USING 'VBAP-WERKS(01)'
*                                      ls_alv-werks.
      ENDLOOP.
      PERFORM bdc_dynpro      USING 'SAPMV45A' '4001'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SICH'.
      PERFORM bdc_dynpro      USING 'SAPLSPO2' '0100'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=OPT1'.
      CALL TRANSACTION 'VA01' USING bdcdata MODE lv_mode MESSAGES INTO messtab.
      LOOP AT messtab  WHERE msgtyp = 'A' OR msgtyp = 'E'.
        err_flag = 'X'.
        <fs_alv>-id = icon_led_red.
        MESSAGE ID messtab-msgid TYPE messtab-msgtyp NUMBER messtab-msgnr
        WITH messtab-msgv1 messtab-msgv2 messtab-msgv3 messtab-msgv4 INTO lv_mess.
        CONCATENATE gs_alv-mess lv_mess INTO gs_alv-mess.
      ENDLOOP.
      IF err_flag IS NOT INITIAL.
        ROLLBACK WORK.
      ELSE.
        COMMIT WORK AND WAIT.
        READ TABLE messtab WITH KEY msgnr = '311' msgid = 'V1'.
        IF sy-subrc = 0.
          gs_alv-vbeln_re = messtab-msgv2.
          gs_alv-id = icon_led_green.
          gs_alv-mess = '创建退货订单成功，销售订单号为:' && gs_alv-vbeln_re.
        ENDIF.

      ENDIF.
      CLEAR ls_vgbel.
      MOVE-CORRESPONDING gs_alv TO ls_vgbel.
      APPEND ls_vgbel TO lt_vgbel.
      CLEAR:messtab[],bdcdata[],lt_alv_temp.
    ENDAT.
  ENDLOOP.

*  LOOP AT lt_zgroup INTO ls_zgroup.
  LOOP AT gt_alv ASSIGNING <fs_alv> WHERE box = 'X'.
    READ TABLE lt_vgbel INTO ls_vgbel WITH KEY vbeln_vf = <fs_alv>-vbeln_vf.
    IF sy-subrc = 0.
      <fs_alv>-mess = ls_vgbel-mess.
      <fs_alv>-id   = ls_vgbel-id.
      <fs_alv>-vbeln_re = ls_vgbel-vbeln_re.
      IF <fs_alv>-id = icon_led_green.
        MOVE-CORRESPONDING <fs_alv> TO ls_zsdt003.
        DO 3 TIMES.
          SELECT SINGLE posnr INTO ls_zsdt003-posnr_re FROM vbap
          WHERE vgbel = ls_zsdt003-vbeln_vf AND vgpos = ls_zsdt003-posnr_vf.
          IF sy-subrc = 0.
            EXIT.
          ELSE.
            WAIT UP TO '0.5' SECONDS.
          ENDIF.
        ENDDO.
        <fs_alv>-posnr_re = ls_zsdt003-posnr_re.
        APPEND ls_zsdt003 TO lt_zsdt003.
      ENDIF.
    ENDIF.
  ENDLOOP.
*  ENDLOOP.


  IF lt_zsdt003 IS NOT INITIAL.
    MODIFY zsdt003 FROM TABLE lt_zsdt003.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_ZGROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_zgroup  USING    p_aufnr p_wafer.
  DATA:ls_alv TYPE ty_alv.

  LOOP AT lt_zppt001 WHERE aufnr = p_aufnr.
    READ TABLE gt_alv INTO ls_alv WITH KEY vbeln_vl = lt_zppt001-vbeln_vl
                                           matnr = lt_zppt001-matnr
                                           charg = lt_zppt001-charg.
    IF sy-subrc = 0.
      CHECK ls_alv-wafer_lot = p_wafer.
      IF ls_alv-zgroup IS INITIAL.
        ls_alv-zgroup = gs_alv-zgroup.
        MODIFY gt_alv FROM ls_alv INDEX sy-tabix TRANSPORTING zgroup.
        PERFORM set_zgroup USING ls_alv-aufnr ls_alv-wafer_lot.
      ELSE.
        gs_alv-zgroup = ls_alv-zgroup.
        LOOP AT gt_alv INTO ls_alv WHERE zgroup = lv_zgroup.
          ls_alv-zgroup = gs_alv-zgroup.
          MODIFY gt_alv FROM ls_alv INDEX sy-tabix TRANSPORTING zgroup.
          lv_zgroup = gs_alv-zgroup.
        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDLOOP.
  LOOP AT gt_alv INTO ls_alv WHERE aufnr = p_aufnr AND wafer_lot = p_wafer AND zgroup IS INITIAL.
    ls_alv-zgroup = gs_alv-zgroup.
    MODIFY gt_alv FROM ls_alv TRANSPORTING zgroup.
    PERFORM set_zgroup USING ls_alv-aufnr ls_alv-wafer_lot.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.
*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREPARE_REPLACE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM prepare_replace_data .
  TYPES: BEGIN OF ty_kna1,
           kunnr TYPE kna1-kunnr,
           name1 TYPE kna1-name1,
         END OF ty_kna1.

  TYPES: BEGIN OF ty_vbpa,
           vbeln  TYPE vbpa-vbeln,
           kunnr  TYPE vbpa-kunnr,
           name1  TYPE kna1-name1,
           street TYPE adrc-street,
         END OF ty_vbpa.

  DATA:lt_kna1 TYPE TABLE OF ty_kna1,
       ls_kna1 TYPE ty_kna1.

  DATA:lt_vbpa TYPE TABLE OF ty_vbpa,
       ls_vbpa TYPE ty_vbpa.

  DATA:lv_lengeth TYPE i.
  DATA:lv_matnr_end TYPE c.
  SELECT vbak~vbeln AS vbeln_re
         kunnr AS kunag
         posnr AS posnr_re
         matnr
         arktx
         werks
         lgort
         kwmeng
  INTO CORRESPONDING FIELDS OF TABLE gt_alv1
  FROM vbak JOIN vbap
    ON vbak~vbeln = vbap~vbeln
  WHERE vbak~vbeln IN s_vbelr2.

  IF gt_alv1 IS NOT INITIAL.
    SELECT kunnr
           name1
    INTO TABLE lt_kna1
    FROM kna1 FOR ALL ENTRIES IN gt_alv1
    WHERE kunnr = gt_alv1-kunag.

    SELECT vbeln
           vbpa~kunnr
           kna1~name1
           street
    INTO TABLE lt_vbpa
    FROM vbpa JOIN kna1
      ON vbpa~kunnr = kna1~kunnr
              JOIN adrc
      ON kna1~adrnr = adrc~addrnumber
      FOR ALL ENTRIES IN gt_alv1
    WHERE vbeln = gt_alv1-vbeln_re
      AND parvw = 'AG'.   "sh  ->  we
  ENDIF.

  LOOP AT gt_alv1 ASSIGNING <fs_alv1>.
    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = <fs_alv1>-kunag.
    IF sy-subrc = 0.
      <fs_alv1>-name1 = ls_kna1-name1.
    ENDIF.

    READ TABLE lt_vbpa INTO ls_vbpa WITH KEY vbeln = <fs_alv1>-vbeln_re.
    IF sy-subrc = 0.
      <fs_alv1>-kunag_zd = ls_vbpa-kunnr.
      <fs_alv1>-name1_sh = ls_vbpa-name1.
      <fs_alv1>-street = ls_vbpa-street.
*      <fs_alv1>-kunag_fzd = ls_vbpa-kunnr.
    ENDIF.

    <fs_alv1>-vkorg = p_vkorg2.
    <fs_alv1>-vtweg = p_vtweg2.
    lv_lengeth = strlen( <fs_alv1>-matnr ).
    SUBTRACT 1 FROM lv_lengeth.
    lv_matnr_end = <fs_alv1>-matnr+lv_lengeth(1).
    CASE lv_matnr_end .
      WHEN 'F'.
        <fs_alv1>-spart = '40'.
        CASE p_vkorg2.
          WHEN 'YZ01'.
            <fs_alv1>-auart = 'ZZ28'.
          WHEN 'HF01'.
            <fs_alv1>-auart = 'ZZ24'.
        ENDCASE.
      WHEN 'G'.
        <fs_alv1>-spart = '30'.
        CASE p_vkorg2.
          WHEN 'YZ01'.
            <fs_alv1>-auart = 'ZZ27'.
          WHEN 'HF01'.
            <fs_alv1>-auart = 'ZZ23'.
        ENDCASE.
      WHEN 'P'.
        <fs_alv1>-spart = '20'.
        CASE p_vkorg2.
          WHEN 'YZ01'.
            <fs_alv1>-auart = 'ZZ26'.
          WHEN 'HF01'.
            <fs_alv1>-auart = 'ZZ22'.
        ENDCASE.
      WHEN 'A'.
        <fs_alv1>-spart = '10'.
        CASE p_vkorg2.
          WHEN 'YZ01'.
            <fs_alv1>-auart = 'ZZ25'.
          WHEN 'HF01'.
            <fs_alv1>-auart = 'ZZ21'.
        ENDCASE.
    ENDCASE.
    SELECT SINGLE bezei INTO <fs_alv1>-auart_txt FROM tvakt
    WHERE auart = <fs_alv1>-auart
      AND spras = sy-langu.
    <fs_alv1>-menge = <fs_alv1>-kwmeng.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_REPLACE_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_replace_order .
  TYPES: BEGIN OF ty_create,
           vkorg    TYPE vbak-vkorg,
           vtweg    TYPE vbak-vtweg,
           spart    TYPE vbak-spart,
           auart    TYPE vbak-auart,
           kunag    TYPE vbak-kunnr,
           matnr    TYPE vbap-matnr,
           werks    TYPE vbap-werks,
           lgort    TYPE vbap-lgort,
           kunag_zd TYPE vbak-kunnr,
           menge    TYPE mseg-menge,
           vbeln_re TYPE vbak-vbeln,
           vbeln_hh TYPE vbak-vbeln,
           id       TYPE icon-id,
           mess     TYPE bapi_msg,
         END OF ty_create.

  TYPES: BEGIN OF ty_vbpa,
           vbeln TYPE vbpa-vbeln,
           parvw TYPE vbpa-parvw,
           kunnr TYPE vbpa-kunnr,
         END OF ty_vbpa.

  DATA:lt_create TYPE TABLE OF ty_create.
  FIELD-SYMBOLS:<fs_create> TYPE ty_create.

  DATA:lt_vbpa TYPE TABLE OF ty_vbpa,
       ls_vbpa TYPE ty_vbpa.

  DATA ls_header TYPE bapisdhd1.
  DATA ls_headerx TYPE bapisdhd1x.
  DATA ls_partner TYPE bapiparnr.

  DATA lt_partner TYPE TABLE OF bapiparnr.
  DATA lt_item TYPE TABLE OF bapisditm.
  DATA lt_schedules TYPE TABLE OF bapischdl.
  DATA lt_schedulesx TYPE TABLE OF bapischdlx.

  DATA ls_item TYPE bapisditm.
  DATA ls_schedules TYPE bapischdl.
  DATA ls_schedulesx TYPE bapischdlx.

  DATA:lt_itemx TYPE TABLE OF bapisditmx,
       ls_itemx TYPE bapisditmx.
  DATA:lt_return TYPE TABLE OF bapiret2.
  DATA:ls_return TYPE bapiret2.

  DATA:temp_posnr TYPE vbap-posnr.
  DATA:err_flag    TYPE abap_bool.

  DATA:lv_vbeln_hh TYPE vbak-vbeln.

  DATA:lt_zsdt003 TYPE TABLE OF zsdt003.
  FIELD-SYMBOLS <fs_zsdt003> TYPE zsdt003.


  LOOP AT gt_alv1 ASSIGNING <fs_alv1> WHERE box = 'X'.
    CLEAR <fs_alv1>-mess.
    PERFORM check_kunnr_sh USING <fs_alv1>-kunag_zd <fs_alv1>-kunag <fs_alv1>-mess.
    IF <fs_alv1>-mess IS NOT INITIAL.
      <fs_alv1>-id = icon_led_red.
    ENDIF.
    CHECK <fs_alv1>-mess IS INITIAL.
    PERFORM check_spart USING <fs_alv1>-matnr <fs_alv1>-spart <fs_alv1>-mess.
    IF <fs_alv1>-mess IS NOT INITIAL.
      <fs_alv1>-id = icon_led_red.
    ENDIF.
    CHECK <fs_alv1>-mess IS INITIAL.
    PERFORM check_auart USING <fs_alv1>-matnr <fs_alv1>-vkorg <fs_alv1>-auart <fs_alv1>-mess.
    IF <fs_alv1>-mess IS NOT INITIAL.
      <fs_alv1>-id = icon_led_red.
    ENDIF.
    CHECK <fs_alv1>-mess IS INITIAL.

    READ TABLE lt_create ASSIGNING <fs_create> WITH KEY vkorg = <fs_alv1>-vkorg
                                                        vtweg = <fs_alv1>-vtweg
                                                        spart = <fs_alv1>-spart
                                                        auart = <fs_alv1>-auart
                                                        matnr = <fs_alv1>-matnr.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO lt_create ASSIGNING <fs_create>.
      <fs_create>-vkorg = <fs_alv1>-vkorg.
      <fs_create>-vtweg = <fs_alv1>-vtweg.
      <fs_create>-spart = <fs_alv1>-spart.
      <fs_create>-auart = <fs_alv1>-auart.
      <fs_create>-matnr = <fs_alv1>-matnr.
      <fs_create>-werks = <fs_alv1>-werks.
      <fs_create>-lgort = <fs_alv1>-lgort.
      <fs_create>-kunag = <fs_alv1>-kunag.
      <fs_create>-kunag_zd = <fs_alv1>-kunag_zd.
      <fs_create>-vbeln_re = <fs_alv1>-vbeln_re.
    ENDIF.
    ADD <fs_alv1>-menge TO <fs_create>-menge.
  ENDLOOP.

  SORT lt_create BY vkorg vtweg spart auart kunag.

  IF lt_create IS NOT INITIAL.
    SELECT vbeln
           parvw
           kunnr
    INTO TABLE lt_vbpa
    FROM vbpa FOR ALL ENTRIES IN lt_create
    WHERE vbeln = lt_create-vbeln_re.
  ENDIF.

  LOOP AT lt_create ASSIGNING <fs_create>.
    ADD 10 TO temp_posnr.
    ls_item-itm_number = temp_posnr.
    ls_item-material   = <fs_create>-matnr.
    ls_item-plant      = <fs_create>-werks.
    ls_item-store_loc  = <fs_create>-lgort.
    ls_item-target_qty = <fs_create>-menge.

    APPEND ls_item TO lt_item.

    ls_itemx-itm_number  = temp_posnr.
    ls_itemx-material    = 'X'.
    ls_itemx-plant       = 'X'.
    ls_itemx-store_loc   = 'X'.
    ls_itemx-target_qty   = 'X'.

    APPEND ls_itemx TO lt_itemx.

    ls_schedules-itm_number = temp_posnr.
    ls_schedules-sched_line = 0001.
    ls_schedules-req_date = sy-datum.
    ls_schedules-req_qty = ls_item-target_qty.
    ls_schedules-date_type = '1'.
    ls_schedules-sched_type = 'CP'.
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

    ls_schedulesx-itm_number = temp_posnr.
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

    AT END OF kunag.
      ls_header-doc_type = <fs_create>-auart.
      ls_header-sales_org = <fs_create>-vkorg.
      ls_header-distr_chan = <fs_create>-vtweg.
      ls_header-division = <fs_create>-spart.

      ls_headerx-updateflag = 'I'.
      ls_headerx-doc_type = 'X'.
      ls_headerx-sales_org = 'X'.
      ls_headerx-division = 'X'.
      CLEAR temp_posnr.

      LOOP AT lt_vbpa INTO ls_vbpa WHERE vbeln = <fs_create>-vbeln_re.
        ls_partner-partn_role = ls_vbpa-parvw.
        ls_partner-partn_numb = ls_vbpa-kunnr.
        IF ls_vbpa-parvw = 'WE'.  "送达方
          ls_partner-partn_numb = <fs_create>-kunag_zd.
        ENDIF.
        IF ls_vbpa-parvw = 'AG'.   "售达方
          ls_partner-partn_numb = <fs_create>-kunag.
        ENDIF.
        APPEND ls_partner TO lt_partner.
      ENDLOOP.

      CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
        EXPORTING
          sales_header_in     = ls_header
          sales_header_inx    = ls_headerx
        IMPORTING
          salesdocument_ex    = lv_vbeln_hh
        TABLES
          return              = lt_return
          sales_items_in      = lt_item
          sales_partners      = lt_partner
          sales_schedules_in  = lt_schedules
          sales_schedules_inx = lt_schedulesx.
      LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
        err_flag = 'X'.
        <fs_create>-id = icon_led_red.
        CONCATENATE <fs_create>-mess ls_return-message INTO <fs_create>-mess.
      ENDLOOP.
      IF err_flag IS NOT INITIAL.
        ROLLBACK WORK.
      ELSE.
        COMMIT WORK AND WAIT.
        <fs_create>-vbeln_hh = lv_vbeln_hh.
        <fs_create>-id = icon_led_green.
        <fs_create>-mess = '创建换货订单成功，换货订单号为:' && lv_vbeln_hh.
      ENDIF.
      CLEAR:ls_headerx,ls_header,lv_vbeln_hh,ls_return,ls_item,ls_partner,ls_schedulesx,ls_schedules.
      CLEAR:lt_return,lt_item,lt_partner,lt_schedules,lt_schedulesx.
    ENDAT.
  ENDLOOP.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_zsdt003
  FROM zsdt003 FOR ALL ENTRIES IN gt_alv1
  WHERE vbeln_re = gt_alv1-vbeln_re
    AND posnr_re = gt_alv1-posnr_re.

  LOOP AT gt_alv1 ASSIGNING <fs_alv1> WHERE box = 'X'.
    LOOP AT lt_create ASSIGNING <fs_create> WHERE vkorg = <fs_alv1>-vkorg
                                              AND vtweg = <fs_alv1>-vtweg
                                              AND spart = <fs_alv1>-spart
                                              AND auart = <fs_alv1>-auart
                                              AND kunag = <fs_alv1>-kunag
                                              AND mess IS NOT INITIAL.
      <fs_alv1>-id       = <fs_create>-id.
      <fs_alv1>-mess     = <fs_create>-mess.
      <fs_alv1>-vbeln_hh = <fs_create>-vbeln_hh.
      EXIT.
    ENDLOOP.
    IF <fs_alv1>-id = icon_led_green.
      READ TABLE lt_zsdt003 ASSIGNING <fs_zsdt003> WITH KEY vbeln_re = <fs_alv1>-vbeln_re
                                                            posnr_re = <fs_alv1>-posnr_re.
      IF sy-subrc = 0.
        <fs_zsdt003>-vbeln_hh = <fs_alv1>-vbeln_hh.
        DO 3 TIMES.
          SELECT SINGLE posnr INTO <fs_zsdt003>-posnr_hh FROM vbap WHERE matnr = <fs_zsdt003>-matnr
                                                                   AND vbeln = <fs_zsdt003>-vbeln_hh.
          IF sy-subrc = 0.
            EXIT.
          ELSE.
            WAIT UP TO '0.5' SECONDS.
          ENDIF.
        ENDDO.
        <fs_alv1>-posnr_hh = <fs_zsdt003>-posnr_hh.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lt_zsdt003 IS NOT INITIAL.
    MODIFY zsdt003 FROM TABLE lt_zsdt003.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_KUNNR_SH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_kunnr_sh  USING  VALUE(p_kunag)
                            VALUE(p_kunnr)
                            VALUE(p_mess).
  DATA:lv_kunnr TYPE kna1-kunnr.

  CLEAR:lv_kunnr.
  SELECT SINGLE kunnr
  INTO lv_kunnr
  FROM knvp
  WHERE kunn2 = p_kunag
    AND parvw = 'WE'.

  IF lv_kunnr NE p_kunnr.
    MESSAGE e001 WITH p_kunnr INTO p_mess.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_SPART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ALV1>_MATNR  text
*      -->P_<FS_ALV1>_SPART  text
*      -->P_<FS_ALV1>_MESS  text
*----------------------------------------------------------------------*
FORM check_spart  USING    p_matnr
                           p_spart
                           p_mess.

  DATA:lv_lengeth TYPE i.
  DATA:lv_matnr_end TYPE c.

  lv_lengeth = strlen( p_matnr ).
  SUBTRACT 1 FROM lv_lengeth.
  lv_matnr_end = p_matnr+lv_lengeth(1).

  CASE lv_matnr_end .
    WHEN 'F'.
      IF p_spart NE '40'.
        p_mess = '物料和产品组不符'.
      ENDIF.
    WHEN 'G'.
      IF p_spart NE '30'.
        p_mess = '物料和产品组不符'.
      ENDIF.
    WHEN 'P'.
      IF p_spart NE '20'.
        p_mess = '物料和产品组不符'.
      ENDIF.
    WHEN 'A'.
      IF p_spart NE '10'.
        p_mess = '物料和产品组不符'.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_auart  USING    p_matnr
                           p_vkorg
                           p_auart
                           p_mess.
  DATA:lv_lengeth TYPE i.
  DATA:lv_matnr_end TYPE c.

  lv_lengeth = strlen( p_matnr ).
  SUBTRACT 1 FROM lv_lengeth.
  lv_matnr_end = p_matnr+lv_lengeth(1).

  CASE lv_matnr_end .
    WHEN 'F'.
      CASE p_vkorg.
        WHEN 'YZ01'.
          IF p_auart NE 'ZZ28'.
            p_mess = '订单类型和物料销售组织对应的订单类型不符'.
          ENDIF.
        WHEN 'HF01'.
          IF p_auart NE 'ZZ24'.
            p_mess = '订单类型和物料销售组织对应的订单类型不符'.
          ENDIF.
      ENDCASE.
    WHEN 'G'.
      CASE p_vkorg.
        WHEN 'YZ01'.
          IF p_auart NE 'ZZ27'.
            p_mess = '订单类型和物料销售组织对应的订单类型不符'.
          ENDIF.
        WHEN 'HF01'.
          IF p_auart NE 'ZZ23'.
            p_mess = '订单类型和物料销售组织对应的订单类型不符'.
          ENDIF.
      ENDCASE.
    WHEN 'P'.
      CASE p_vkorg.
        WHEN 'YZ01'.
          IF p_auart NE 'ZZ26'.
            p_mess = '订单类型和物料销售组织对应的订单类型不符'.
          ENDIF.
        WHEN 'HF01'.
          IF p_auart NE 'ZZ22'.
            p_mess = '订单类型和物料销售组织对应的订单类型不符'.
          ENDIF.
      ENDCASE.
    WHEN 'A'.
      CASE p_vkorg.
        WHEN 'YZ01'.
          IF p_auart NE 'ZZ25'.
            p_mess = '订单类型和物料销售组织对应的订单类型不符'.
          ENDIF.
        WHEN 'HF01'.
          IF p_auart NE 'ZZ21'.
            p_mess = '订单类型和物料销售组织对应的订单类型不符'.
          ENDIF.
      ENDCASE.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_RETURN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_return_data .
  TYPES: BEGIN OF ty_kna1,
           kunnr TYPE kna1-kunnr,
           name1 TYPE kna1-name1,
         END OF ty_kna1.

  DATA:lt_kna1 TYPE TABLE OF ty_kna1,
       ls_kna1 TYPE ty_kna1.
  TYPES: BEGIN OF ty_vbap,
           vbeln  TYPE vbap-vbeln,
           posnr  TYPE vbap-posnr,
           vgbel  TYPE vbap-vgbel,
           vgpos  TYPE vbap-vgpos,
           kwmeng TYPE vbap-kwmeng,
         END OF ty_vbap.
  DATA:lt_vbap TYPE TABLE OF ty_vbap,
       ls_vbap TYPE ty_vbap.
  SELECT vbeln_vf
         posnr_vf
         vbak~vkorg
         auart
         vbeln_re
         z~kunag
         posnr_re
         matnr
         arktx
         menge
         fkimg
         charg
         baselotno
         wafer_lot
  INTO CORRESPONDING FIELDS OF TABLE gt_alv
  FROM zsdt003 AS z JOIN vbrk
    ON z~vbeln_vf = vbrk~vbeln
                    JOIN vbak
    ON z~vbeln_re = vbak~vbeln
  WHERE vbeln_re IN s_vbelr1
    AND vbeln_vf IN s_vbeln1
    AND vbak~vkorg = p_vkorg1
    AND vbrk~fkdat IN s_fkdat1
    AND vbrk~erdat IN s_erdat1
    AND z~kunag  IN s_kunag1.

  IF gt_alv IS NOT INITIAL.
    SELECT kunnr
           name1
    INTO TABLE lt_kna1
    FROM kna1 FOR ALL ENTRIES IN gt_alv
    WHERE kunnr = gt_alv-kunag.

    SELECT vbeln
       posnr
       vgbel
       vgpos
       kwmeng
INTO TABLE lt_vbap
FROM vbap FOR ALL ENTRIES IN gt_alv
WHERE vgbel = gt_alv-vbeln_vf
  AND vgpos = gt_alv-posnr_vf.
  ENDIF.



  LOOP AT gt_alv ASSIGNING <fs_alv>.
    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = <fs_alv>-kunag.
    IF sy-subrc = 0.
      <fs_alv>-name1 = ls_kna1-name1.
    ENDIF.
    LOOP AT lt_vbap INTO ls_vbap WHERE vgbel = <fs_alv>-vbeln_vf AND
                                     vgpos = <fs_alv>-posnr_vf.
      ADD ls_vbap-kwmeng TO <fs_alv>-menge_re.
      IF ls_vbap-vbeln NE <fs_alv>-vbeln_re OR ls_vbap-posnr NE <fs_alv>-posnr_re.
        ADD ls_vbap-kwmeng TO <fs_alv>-menge_ex.
      ENDIF.
    ENDLOOP.

  ENDLOOP.

  SORT gt_alv BY vbeln_re posnr_re.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_RETURN_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM change_return_order .

  DATA:ls_header    TYPE bapisdh1,
       ls_headerx   TYPE bapisdh1x,
       lt_return    TYPE bapiret2_t,
       ls_return    TYPE bapiret2,
       lt_item      TYPE TABLE OF bapisditm,
       ls_item      TYPE bapisditm,
       lt_itemx     TYPE TABLE OF bapisditmx,
       ls_itemx     TYPE bapisditmx,
       lt_schedule  TYPE TABLE OF bapischdl,
       ls_schedule  TYPE bapischdl,
       lt_schedulex TYPE TABLE OF bapischdlx,
       ls_schedulex TYPE bapischdlx.

  TYPES: BEGIN OF ty_vbak,
           vbeln_re TYPE vbak-vbeln,
           posnr_re TYPE vbap-posnr,
           menge    TYPE mseg-menge,
           id       TYPE icon-id,
           mess     TYPE bapi_msg,
         END OF ty_vbak.

  DATA:lt_vbak TYPE TABLE OF ty_vbak.
  DATA:ls_vbak TYPE ty_vbak.
  FIELD-SYMBOLS <fs_vbak> TYPE ty_vbak.

  DATA:lt_zsdt003 TYPE TABLE OF zsdt003.
  FIELD-SYMBOLS <fs_zsdt003> TYPE zsdt003.

  DATA:err_flag TYPE abap_bool.
  DATA:lv_menge TYPE mseg-menge.


  CLEAR:err_flag, lt_vbak.
  LOOP AT gt_alv ASSIGNING <fs_alv> WHERE box = 'X'.
    CLEAR <fs_alv>-mess.
    IF <fs_alv>-menge > <fs_alv>-fkimg.
      <fs_alv>-id = icon_led_red.
      <fs_alv>-mess = '退货数量不能大于发票数量'.
      err_flag = abap_true.
    ENDIF.
    CHECK <fs_alv>-mess IS INITIAL.
    IF <fs_alv>-menge <= 0.
      <fs_alv>-id = icon_led_red.
      <fs_alv>-mess = '退货数量必须大于0'.
      err_flag = abap_true.
    ENDIF.
    CHECK <fs_alv>-mess IS INITIAL.

    APPEND INITIAL LINE TO lt_vbak ASSIGNING <fs_vbak>.
    MOVE-CORRESPONDING <fs_alv> TO <fs_vbak>.
    CLEAR:lv_menge.
  ENDLOOP.
  CHECK err_flag = abap_false.

  LOOP AT lt_vbak INTO ls_vbak.
    LOOP AT gt_alv ASSIGNING <fs_alv> WHERE box = '' AND vbeln_re = ls_vbak-vbeln_re.
      CLEAR <fs_alv>-mess.
      IF <fs_alv>-menge > <fs_alv>-fkimg - <fs_alv>-menge_ex.
        <fs_alv>-id = icon_led_red.
        <fs_alv>-mess = '退货数量不能大于发票数量'.
        err_flag = abap_true.
      ENDIF.
      CHECK <fs_alv>-mess IS INITIAL.
      IF <fs_alv>-menge <= 0.
        <fs_alv>-id = icon_led_red.
        <fs_alv>-mess = '退货数量必须大于0'.
        err_flag = abap_true.
      ENDIF.
      CHECK <fs_alv>-mess IS INITIAL.
      <fs_alv>-box = 'X'.
      APPEND INITIAL LINE TO lt_vbak ASSIGNING <fs_vbak>.
      MOVE-CORRESPONDING <fs_alv> TO <fs_vbak>.
    ENDLOOP.
  ENDLOOP.
  CHECK err_flag = abap_false.

  CLEAR:lt_item,lt_return,lt_itemx.
  CLEAR:ls_item,ls_header,ls_headerx,ls_itemx,ls_return.

  SORT lt_vbak BY vbeln_re posnr_re.

  LOOP AT lt_vbak ASSIGNING <fs_vbak>.

    ls_schedule-itm_number = <fs_vbak>-posnr_re.
    ls_schedule-sched_line = 0001.
    ls_schedule-req_qty    = <fs_vbak>-menge.
    APPEND ls_schedule TO lt_schedule.

    ls_schedulex-itm_number = <fs_vbak>-posnr_re.
    ls_schedulex-sched_line = 0001.
    ls_schedulex-req_qty    = 'X'.
    ls_schedulex-updateflag = 'U'.
    APPEND ls_schedulex TO lt_schedulex.

    AT END OF vbeln_re.
      ls_headerx-updateflag = 'U'.

      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument     = <fs_vbak>-vbeln_re
          order_header_inx  = ls_headerx
          behave_when_error = 'P'
        TABLES
          return            = lt_return
          schedule_lines    = lt_schedule
          schedule_linesx   = lt_schedulex
          order_item_in     = lt_item
          order_item_inx    = lt_itemx.

      LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
        err_flag = 'X'.
        <fs_vbak>-id = icon_led_red.
        CONCATENATE <fs_vbak>-mess ls_return-message INTO <fs_vbak>-mess.
      ENDLOOP.
      IF err_flag IS NOT INITIAL.
        ROLLBACK WORK.
      ELSE.
        COMMIT WORK AND WAIT.
        <fs_vbak>-id = icon_led_green.
        <fs_vbak>-mess = '修改成功！'.
      ENDIF.
      CLEAR:ls_headerx,ls_header,ls_return,ls_item,ls_schedulex,ls_schedule.
      CLEAR:lt_return,lt_item,lt_itemx,lt_schedule,lt_schedulex.
    ENDAT.
  ENDLOOP.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_zsdt003
  FROM zsdt003 FOR ALL ENTRIES IN gt_alv1
  WHERE vbeln_re = gt_alv1-vbeln_re
    AND posnr_re = gt_alv1-posnr_re.

  LOOP AT gt_alv ASSIGNING <fs_alv> WHERE box = 'X'.
    LOOP AT lt_vbak ASSIGNING <fs_vbak>    WHERE vbeln_re = <fs_alv>-vbeln_re AND mess IS NOT INITIAL.
      <fs_alv>-id       = <fs_vbak>-id.
      <fs_alv>-mess     = <fs_vbak>-mess.
      EXIT.
    ENDLOOP.
    IF <fs_alv>-id = icon_led_green.
      READ TABLE lt_zsdt003 ASSIGNING <fs_zsdt003> WITH KEY vbeln_re = <fs_alv>-vbeln_re
                                                            posnr_re = <fs_alv>-posnr_re.
      IF sy-subrc = 0.
        <fs_zsdt003>-menge = <fs_alv>-menge.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF lt_zsdt003 IS NOT INITIAL.
    MODIFY zsdt003 FROM TABLE lt_zsdt003.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_REPLACE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_replace_edit .

  TYPES: BEGIN OF ty_kna1,
           kunnr TYPE kna1-kunnr,
           name1 TYPE kna1-name1,
         END OF ty_kna1.

  TYPES: BEGIN OF ty_vbpa,
           vbeln  TYPE vbpa-vbeln,
           kunnr  TYPE vbpa-kunnr,
           name1  TYPE kna1-name1,
           street TYPE adrc-street,
         END OF ty_vbpa.

  DATA:lt_kna1 TYPE TABLE OF ty_kna1,
       ls_kna1 TYPE ty_kna1.


  DATA:lt_vbpa TYPE TABLE OF ty_vbpa,
       ls_vbpa TYPE ty_vbpa.

  SELECT vbeln_hh
         posnr_hh
         vkorg
         auart
         kunag
         z~matnr
         z~arktx
         vbeln_re
         vbap~kwmeng AS menge
         vbap~lgort
  INTO CORRESPONDING FIELDS OF TABLE gt_alv1
  FROM zsdt003 AS z JOIN vbak
    ON z~vbeln_hh = vbak~vbeln
                    JOIN vbap
    ON z~vbeln_hh = vbap~vbeln
   AND z~posnr_hh = vbap~posnr
  WHERE vbeln_hh IN s_vbelh2
    AND vbeln_vf IN s_vbeln2
    AND vbak~audat IN s_audat2
    AND z~kunag  IN s_kunnr2
    AND vkorg = p_vkorg2.

  SORT gt_alv1 BY vbeln_hh posnr_hh.
  DELETE ADJACENT DUPLICATES FROM gt_alv1 COMPARING vbeln_hh posnr_hh.

  IF gt_alv1 IS NOT INITIAL.
    SELECT kunnr
           name1
    INTO TABLE lt_kna1
    FROM kna1 FOR ALL ENTRIES IN gt_alv1
    WHERE kunnr = gt_alv1-kunag.

    SELECT vbeln
           vbpa~kunnr
           kna1~name1
           street
    INTO TABLE lt_vbpa
    FROM vbpa JOIN kna1
      ON vbpa~kunnr = kna1~kunnr
              JOIN adrc
      ON kna1~adrnr = adrc~addrnumber
      FOR ALL ENTRIES IN gt_alv1
    WHERE vbeln = gt_alv1-vbeln_hh
      AND parvw = 'WE'.   "sh  ->  we

  ENDIF.

  LOOP AT gt_alv1 ASSIGNING <fs_alv1>.
    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = <fs_alv1>-kunag.
    IF sy-subrc = 0.
      <fs_alv1>-name1 = ls_kna1-name1.
    ENDIF.

    READ TABLE lt_vbpa INTO ls_vbpa WITH KEY vbeln = <fs_alv1>-vbeln_hh.
    IF sy-subrc = 0.
      <fs_alv1>-kunag_zd = ls_vbpa-kunnr.
      <fs_alv1>-name1_sh = ls_vbpa-name1.
      <fs_alv1>-street = ls_vbpa-street.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_REPLACE_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_replace_order .

  DATA:ls_header    TYPE bapisdh1,
       ls_headerx   TYPE bapisdh1x,
       lt_return    TYPE bapiret2_t,
       ls_return    TYPE bapiret2,
       lt_item      TYPE TABLE OF bapisditm,
       ls_item      TYPE bapisditm,
       lt_itemx     TYPE TABLE OF bapisditmx,
       ls_itemx     TYPE bapisditmx,
       lt_schedule  TYPE TABLE OF bapischdl,
       ls_schedule  TYPE bapischdl,
       lt_schedulex TYPE TABLE OF bapischdlx,
       ls_schedulex TYPE bapischdlx.

  TYPES: BEGIN OF ty_vbak,
           vbeln_hh TYPE vbak-vbeln,
           posnr_hh TYPE vbap-posnr,
           matnr    TYPE vbap-matnr,
           menge    TYPE mseg-menge,
           id       TYPE icon-id,
           mess     TYPE bapi_msg,
         END OF ty_vbak.

  DATA:lt_vbak TYPE TABLE OF ty_vbak.
  FIELD-SYMBOLS <fs_vbak> TYPE ty_vbak.

  DATA:err_flag TYPE abap_bool.
  DATA:lv_menge TYPE mseg-menge.


  CLEAR lt_vbak.
  LOOP AT gt_alv1 ASSIGNING <fs_alv1> WHERE box = 'X'.
    CLEAR <fs_alv1>-mess.
    IF <fs_alv1>-menge <= 0.
      <fs_alv1>-id = icon_led_red.
      <fs_alv1>-mess = '换货数量必须大于0'.
    ENDIF.
    CHECK <fs_alv1>-mess IS INITIAL.

    APPEND INITIAL LINE TO lt_vbak ASSIGNING <fs_vbak>.
    MOVE-CORRESPONDING <fs_alv1> TO <fs_vbak>.
    CLEAR:lv_menge.
  ENDLOOP.

  CLEAR:lt_item,lt_return,lt_itemx,lt_schedule,lt_schedulex.
  CLEAR:ls_item,ls_header,ls_headerx,ls_itemx,ls_return,ls_schedule,ls_schedulex.

  SORT lt_vbak BY vbeln_hh posnr_hh.

  LOOP AT lt_vbak ASSIGNING <fs_vbak>.

    ls_schedule-itm_number = <fs_vbak>-posnr_hh.
    ls_schedule-sched_line = 0001.
    ls_schedule-req_qty    = <fs_vbak>-menge.
    APPEND ls_schedule TO lt_schedule.

    ls_schedulex-itm_number = <fs_vbak>-posnr_hh.
    ls_schedulex-sched_line = 0001.
    ls_schedulex-req_qty    = 'X'.
    ls_schedulex-updateflag = 'U'.
    APPEND ls_schedulex TO lt_schedulex.

    AT END OF vbeln_hh.
      ls_headerx-updateflag = 'U'.

      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument     = <fs_vbak>-vbeln_hh
          order_header_inx  = ls_headerx
          behave_when_error = 'P'
        TABLES
          return            = lt_return
          schedule_lines    = lt_schedule
          schedule_linesx   = lt_schedulex
          order_item_in     = lt_item
          order_item_inx    = lt_itemx.

      LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
        err_flag = 'X'.
        <fs_vbak>-id = icon_led_red.
        CONCATENATE <fs_vbak>-mess ls_return-message INTO <fs_vbak>-mess.
      ENDLOOP.
      IF err_flag IS NOT INITIAL.
        ROLLBACK WORK.
      ELSE.
        COMMIT WORK AND WAIT.
        <fs_vbak>-id = icon_led_green.
        <fs_vbak>-mess = '修改成功！'.
      ENDIF.
      CLEAR:ls_headerx,ls_header,ls_return,ls_item,ls_schedule,ls_schedulex.
      CLEAR:lt_return,lt_item,lt_itemx,lt_schedule,lt_schedulex.
    ENDAT.
  ENDLOOP.

  LOOP AT gt_alv1 ASSIGNING <fs_alv1> WHERE box = 'X'.
    LOOP AT lt_vbak ASSIGNING <fs_vbak>    WHERE vbeln_hh = <fs_alv1>-vbeln_hh
                                              AND posnr_hh = <fs_alv1>-posnr_hh.
      <fs_alv1>-id       = <fs_vbak>-id.
      <fs_alv1>-mess     = <fs_vbak>-mess.
      EXIT.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREPARE_REPLACE_DELIVERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM prepare_replace_delivery .

  TYPES: BEGIN OF ty_kna1,
           kunnr TYPE kna1-kunnr,
           name1 TYPE kna1-name1,
         END OF ty_kna1.

  TYPES: BEGIN OF ty_vbpa,
           vbeln  TYPE vbpa-vbeln,
           kunnr  TYPE vbpa-kunnr,
           name1  TYPE kna1-name1,
           street TYPE adrc-street,
         END OF ty_vbpa.

  TYPES: BEGIN OF ty_lips,
           vgbel TYPE lips-vgbel,
           vgpos TYPE lips-vgpos,
           lgort TYPE lips-lgort,
           charg TYPE lips-charg,
           uecha TYPE lips-uecha,
         END OF ty_lips.
  TYPES:BEGIN OF ty_vbap,
          vbeln  TYPE vbap-vbeln,
          posnr  TYPE vbap-posnr,
          kwmeng TYPE vbap-kwmeng,
        END OF ty_vbap.

  DATA:lt_kna1 TYPE TABLE OF ty_kna1,
       ls_kna1 TYPE ty_kna1.

  DATA:lt_vbpa TYPE TABLE OF ty_vbpa,
       ls_vbpa TYPE ty_vbpa.

  DATA:lt_lips TYPE TABLE OF ty_lips,
       ls_lips TYPE ty_lips.

  DATA:lt_vbap TYPE TABLE OF ty_vbap,
       ls_vbap TYPE ty_vbap.

  DATA:lt_lips1 TYPE TABLE OF ty_lips.

  SELECT vbeln_hh
         posnr_hh
         vbeln_re
         posnr_re
         z~charg
         z~matnr
         z~arktx
         vbak~kunnr AS kunag
         vbak~vkorg
         vbap~kwmeng
  INTO CORRESPONDING FIELDS OF TABLE gt_alv2
  FROM zsdt003 AS z JOIN vbak
    ON z~vbeln_hh = vbak~vbeln
                    JOIN vbap
    ON z~vbeln_hh = vbap~vbeln
   AND z~posnr_hh = vbap~posnr
  WHERE vbeln_hh IN s_vbelh3
    AND vbak~kunnr IN s_kunnr3.

  IF gt_alv2 IS NOT INITIAL.
    SELECT kunnr
           name1
    INTO TABLE lt_kna1
    FROM kna1 FOR ALL ENTRIES IN gt_alv2
    WHERE kunnr = gt_alv2-kunag.

    SELECT vbeln
           vbpa~kunnr
           kna1~name1
           street
    INTO TABLE lt_vbpa
    FROM vbpa JOIN kna1
      ON vbpa~kunnr = kna1~kunnr
              JOIN adrc
      ON kna1~adrnr = adrc~addrnumber
      FOR ALL ENTRIES IN gt_alv2
    WHERE vbeln = gt_alv2-vbeln_hh
      AND parvw = 'WE'.   "sh  ->  we

    "取退货交货单对应的库存地点
    SELECT vgbel
           vgpos
           lgort
    INTO CORRESPONDING FIELDS OF TABLE lt_lips
    FROM lips FOR ALL ENTRIES IN gt_alv2
    WHERE vgbel = gt_alv2-vbeln_re
      AND vgpos = gt_alv2-posnr_re.

    SELECT vbeln
           posnr
           kwmeng
    INTO TABLE lt_vbap
    FROM vbap FOR ALL ENTRIES IN gt_alv2
    WHERE vbeln = gt_alv2-vbeln_re
      AND posnr = gt_alv2-posnr_re.

    "取出已经交完货的交货单
    SELECT vgbel
           uecha
           charg
    INTO CORRESPONDING FIELDS OF TABLE lt_lips1
    FROM lips JOIN vbup
      ON lips~vbeln = vbup~vbeln
    FOR ALL ENTRIES IN gt_alv2
    WHERE vgbel = gt_alv2-vbeln_hh
      AND uecha = gt_alv2-posnr_hh
      AND charg = gt_alv2-charg
      AND wbsta = 'C'.
  ENDIF.

  SORT gt_alv2 BY vbeln_hh posnr_hh.
  DATA:lv_posnr TYPE lips-posnr.

  LOOP AT gt_alv2 ASSIGNING <fs_alv2>.
    READ TABLE lt_lips1 TRANSPORTING NO FIELDS WITH KEY vgbel = <fs_alv2>-vbeln_hh
                                                        uecha = <fs_alv2>-posnr_hh
                                                        charg = <fs_alv2>-charg.
    IF sy-subrc = 0.
      DELETE gt_alv2.
      CONTINUE.
    ENDIF.
    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = <fs_alv2>-kunag.
    IF sy-subrc = 0.
      <fs_alv2>-name1 = ls_kna1-name1.
    ENDIF.

    READ TABLE lt_vbpa INTO ls_vbpa WITH KEY vbeln = <fs_alv2>-vbeln_hh.
    IF sy-subrc = 0.
      <fs_alv2>-kunag_zd = ls_vbpa-kunnr.
      <fs_alv2>-name1_sh = ls_vbpa-name1.
      <fs_alv2>-street = ls_vbpa-street.
    ENDIF.

    READ TABLE lt_lips INTO ls_lips WITH KEY vgbel = <fs_alv2>-vbeln_re
                                             vgpos = <fs_alv2>-posnr_re.
    IF sy-subrc = 0.
      <fs_alv2>-lgort = ls_lips-lgort.
    ENDIF.

    READ TABLE lt_vbap INTO ls_vbap WITH KEY vbeln = <fs_alv2>-vbeln_re
                                             posnr = <fs_alv2>-posnr_re.
    IF sy-subrc = 0.
      <fs_alv2>-menge = ls_vbap-kwmeng.
    ENDIF.

  ENDLOOP.

  LOOP AT gt_alv2 ASSIGNING <fs_alv2>.

    AT NEW vbeln_hh.
      lv_posnr = 900000.
    ENDAT.
    ADD 1 TO lv_posnr.
    <fs_alv2>-posnr_pc = lv_posnr.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_REPLACE_DELIVERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_replace_delivery .
  TYPES: BEGIN OF ty_kna1,
           kunnr  TYPE kna1-kunnr,
           name1  TYPE kna1-name1,
           street TYPE adrc-street,
         END OF ty_kna1.
  TYPES: BEGIN OF ty_vbap,
           vbeln  TYPE vbap-vbeln,
           posnr  TYPE vbap-posnr,
           kwmeng TYPE vbap-kwmeng,
         END OF ty_vbap.
  DATA:lt_kna1 TYPE TABLE OF ty_kna1,
       ls_kna1 TYPE ty_kna1.
  DATA:lt_vbap TYPE TABLE OF ty_vbap,
       ls_vbap TYPE ty_vbap.
  SELECT likp~vbeln AS vbeln_ck
         likp~vkorg
         likp~kunag
         likp~kunnr AS kunag_zd
         lips~uecha AS posnr_ck
         lips~matnr
         lips~arktx
         lips~lgort
         lips~lfimg AS menge
         lips~vgbel AS vbeln_hh
         lips~vgpos AS posnr_hh
         lips~posnr AS posnr_pc
         lips~charg
         kna1~name1
  INTO CORRESPONDING FIELDS OF TABLE gt_alv2
  FROM likp JOIN lips
    ON likp~vbeln = lips~vbeln
            JOIN kna1
    ON likp~kunag = kna1~kunnr
  WHERE likp~vkorg IN s_vkorg3
    AND lips~vgbel IN s_vbelh3
    AND likp~vbeln IN s_vbeln3
    AND likp~erdat IN s_audat3
    AND likp~kunag IN s_kunnr3
    AND lips~uecha NE 000000.

  IF gt_alv2 IS NOT INITIAL.
    SELECT kunnr
           kna1~name1
           street
    INTO TABLE lt_kna1
    FROM kna1 JOIN adrc
      ON kna1~adrnr = adrc~addrnumber
      FOR ALL ENTRIES IN gt_alv2
    WHERE kunnr = gt_alv2-kunag_zd.

    SELECT vbeln
           posnr
           kwmeng
    INTO TABLE lt_vbap
    FROM vbap FOR ALL ENTRIES IN gt_alv2
    WHERE vbeln = gt_alv2-vbeln_hh
      AND posnr = gt_alv2-posnr_hh.
  ENDIF.

  LOOP AT gt_alv2 ASSIGNING <fs_alv2>.

    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = <fs_alv2>-kunag_zd.
    IF sy-subrc = 0.
      <fs_alv2>-name1_sh = ls_kna1-name1.
      <fs_alv2>-street = ls_kna1-street.
    ENDIF.

    READ TABLE lt_vbap INTO ls_vbap WITH KEY vbeln = <fs_alv2>-vbeln_hh
                                             posnr = <fs_alv2>-posnr_hh.
    IF sy-subrc = 0.
      <fs_alv2>-kwmeng = ls_vbap-kwmeng.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_REPLACE_DELIVERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_replace_delivery .
  "创建交货单
  DATA: lt_so TYPE bapidlvreftosalesorder OCCURS 0.
  DATA: lt_return TYPE bapiret2 OCCURS 0 WITH HEADER LINE.
  DATA: g_vbeln TYPE likp-vbeln.
  DATA: ls_so TYPE bapidlvreftosalesorder.
  DATA: lv_msg      TYPE c LENGTH 220."过账消息
  "交货单批次拆分
  TYPES: BEGIN OF ty_lips,
           vbeln_hh TYPE lips-vbeln,
           posnr_hh TYPE lips-posnr,
           posnr_pc TYPE lips-posnr,
           matnr    TYPE lips-matnr,
           charg    TYPE lips-charg,
           menge    TYPE mseg-menge,
           lgort    TYPE lips-lgort,
           vbeln_ck TYPE lips-vbeln,
         END OF ty_lips.

  DATA: ls_header         TYPE bapiobdlvhdrchg,
        ls_header_control TYPE bapiobdlvhdrctrlchg.

  DATA:  lt_item         TYPE STANDARD TABLE OF bapiobdlvitemchg,
         ls_item         TYPE bapiobdlvitemchg,
         ls_headdeadline LIKE TABLE OF bapidlvdeadln WITH HEADER LINE,
         "外向交货项目级别控制数据
         lt_item_control TYPE STANDARD TABLE OF bapiobdlvitemctrlchg,
         ls_item_control TYPE bapiobdlvitemctrlchg,
         "确认向外交货拣配数据项目等级(SPE)
         lt_item_spl     TYPE STANDARD TABLE OF /spe/bapiobdlvitemchg,
         ls_item_spl     TYPE /spe/bapiobdlvitemchg,
         ls_lips         TYPE ty_lips,
         lt_lips         TYPE TABLE OF ty_lips.
  FIELD-SYMBOLS <fs_lips> TYPE ty_lips.
  DATA:lt_data TYPE TABLE OF ty_alv2.
  DATA:lt_data_temp TYPE TABLE OF ty_alv2.
  DATA:temp_posnr TYPE lips-posnr.

  DATA:lt_zsdt003 TYPE TABLE OF zsdt003.
  FIELD-SYMBOLS <fs_zsdt003> TYPE zsdt003.

  REFRESH: lt_so,lt_return.
  CLEAR:g_vbeln.

  LOOP AT gt_alv2 ASSIGNING <fs_alv2> WHERE box = 'X'.
    CLEAR <fs_alv2>-mess.
    IF <fs_alv2>-menge <= 0.
      <fs_alv2>-id = icon_led_red.
      <fs_alv2>-mess = '出货数量必须大于0'.
    ENDIF.
    CHECK <fs_alv2>-mess IS INITIAL.
    APPEND <fs_alv2> TO lt_data.
  ENDLOOP.

  LOOP AT lt_data ASSIGNING <fs_alv2>.
    ls_so-ref_doc = <fs_alv2>-vbeln_hh.
    ls_so-ref_item = <fs_alv2>-posnr_hh.
    APPEND ls_so TO lt_so.
    CLEAR ls_so.
    MOVE-CORRESPONDING <fs_alv2> TO ls_lips.
    APPEND ls_lips TO lt_lips.
    CLEAR:ls_lips.
    AT END OF vbeln_hh.
      SORT lt_so BY ref_doc ref_item.
      DELETE ADJACENT DUPLICATES FROM lt_so COMPARING ALL FIELDS.
      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
        EXPORTING
          ship_point        = p_vstel3
          due_date          = '99991231'
        IMPORTING
          delivery          = g_vbeln
        TABLES
          sales_order_items = lt_so
          return            = lt_return.
      IF g_vbeln IS INITIAL.
        ROLLBACK WORK.
        <fs_alv2>-id   = icon_led_red.
        <fs_alv2>-mess = '交货单创建失败！'.
        REFRESH: lt_return,lt_so .
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        DATA: lv_posnr TYPE lips-posnr.
        REFRESH: lt_return,lt_so .
        REFRESH: lt_return,lt_item,lt_item_control,lt_item_spl,lt_return .
        CLEAR lv_posnr.
        ls_header-deliv_numb = g_vbeln.
        ls_header_control-deliv_numb = g_vbeln.
        "标记更更实际和计划的过账时间， 时间的值在 HEADER_DEADLINES 参数里添加（这个开始没注意看帮助，走了很多弯路，才找到）
        ls_header_control-deliv_date_flg = 'X'.
        ls_header_control-gdsi_date_flg ='X'.

        LOOP AT lt_lips ASSIGNING <fs_lips>.
          AT NEW posnr_hh.
            ADD 10 TO temp_posnr.
            ls_item-deliv_numb = g_vbeln.
            ls_item-deliv_item = temp_posnr.
            ls_item-material = <fs_lips>-matnr.
*      ls_item-dlv_qty = w_tab-bdmng1.
*      ls_item-dlv_qty_imunit = w_tab-bdmng1.
            ls_item-fact_unit_denom = 1.
            ls_item-fact_unit_nom = 1.
            APPEND ls_item TO lt_item.
            CLEAR ls_item.

            ls_item_control-deliv_numb = g_vbeln.
            ls_item_control-deliv_item = temp_posnr.
            ls_item_control-chg_delqty = 'X'.
            ls_item_control-net_wt_flg = 'X'.
            ls_item_control-gross_wt_flg = 'X'.
            APPEND ls_item_control TO lt_item_control.
            CLEAR ls_item_control.

            ls_item_spl-deliv_numb = g_vbeln.
            ls_item_spl-deliv_item = temp_posnr.
            ls_item_spl-stge_loc = <fs_lips>-lgort.
            APPEND ls_item_spl TO lt_item_spl.
            CLEAR ls_item_spl.
          ENDAT.
          ls_item-deliv_numb = g_vbeln.
          ls_item-deliv_item = <fs_lips>-posnr_pc.
          ls_item-material = <fs_lips>-matnr.
          ls_item-batch = <fs_lips>-charg.
          ls_item-hieraritem = temp_posnr.
          ls_item-usehieritm = '1'.
          ls_item-dlv_qty = <fs_lips>-menge.
          ls_item-dlv_qty_imunit = <fs_lips>-menge.
          ls_item-fact_unit_denom = 1.
          ls_item-fact_unit_nom = 1.
          APPEND ls_item TO lt_item.
          CLEAR ls_item.

          ls_item_control-deliv_numb = g_vbeln.
          ls_item_control-deliv_item = <fs_lips>-posnr_pc.
          ls_item_control-chg_delqty = 'X'.
          APPEND ls_item_control TO lt_item_control.
          CLEAR ls_item_control.

          ls_item_spl-deliv_numb = g_vbeln.
          ls_item_spl-deliv_item = <fs_lips>-posnr_pc.
          ls_item_spl-stge_loc = <fs_lips>-lgort.
          APPEND ls_item_spl TO lt_item_spl.
          CLEAR ls_item_spl.
        ENDLOOP.

        "查 header_deadlines 说明得到下面提示
*- WSHDRLFDAT  Delivery date
*- WSHDRWADAT  Goods issue date (planned)
*- WSHDRWADTI  Goods issue date (actual)
*- WSHDRLDDAT  Loading date
*- WSHDRTDDAT  Transportation planning date
*- WSHDRKODAT  Picking date

        CLEAR:ls_headdeadline,ls_headdeadline[].
        ls_headdeadline-deliv_numb  =   g_vbeln.
        ls_headdeadline-timetype = 'WSHDRWADTI'. "实际过账日期
        ls_headdeadline-timestamp_utc = sy-datum.
        APPEND ls_headdeadline.

        ls_headdeadline-deliv_numb =  g_vbeln.
        ls_headdeadline-timetype = 'WSHDRWADAT'. "计划日期
        ls_headdeadline-timestamp_utc = sy-datum.
        APPEND ls_headdeadline.

        ls_headdeadline-deliv_numb =  g_vbeln.
        ls_headdeadline-timetype = 'WSHDRLFDAT'. "凭证日期
        ls_headdeadline-timestamp_utc = sy-datum.
        APPEND ls_headdeadline.


        CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
          EXPORTING
            header_data      = ls_header
            header_control   = ls_header_control
            delivery         = g_vbeln
          TABLES
            header_deadlines = ls_headdeadline
            item_data        = lt_item
            item_control     = lt_item_control
            item_data_spl    = lt_item_spl
            return           = lt_return.
        LOOP AT lt_return WHERE type = 'E'  OR type = 'A'.
          MESSAGE ID lt_return-id TYPE lt_return-type NUMBER lt_return-number
          WITH lt_return-message_v1 lt_return-message_v2 lt_return-message_v3 lt_return-message_v4 INTO lv_msg.
          <fs_alv2>-id = icon_led_red.
          <fs_alv2>-vbeln_ck = g_vbeln.
          CONCATENATE <fs_alv2>-mess lv_msg INTO <fs_alv2>-mess SEPARATED BY '/'.
          CLEAR:lv_msg.
        ENDLOOP.
        IF sy-subrc = 0.
          ROLLBACK WORK.
        ELSE.
          COMMIT WORK AND WAIT.
          <fs_alv2>-id = icon_led_green.
          <fs_alv2>-mess = '换货出货单创建成功'.
          <fs_alv2>-vbeln_ck = g_vbeln.
        ENDIF.
      ENDIF.
      CLEAR:lt_lips,lt_item,lt_item_control,lt_item_spl,lt_return,ls_header,ls_header_control,g_vbeln,temp_posnr.
    ENDAT.
  ENDLOOP.

  IF lt_data IS NOT INITIAL.
    SELECT vbeln AS vbeln_ck
           posnr AS posnr_pc
           charg
    INTO CORRESPONDING FIELDS OF TABLE lt_lips
    FROM lips FOR ALL ENTRIES IN lt_data
    WHERE vbeln = lt_data-vbeln_ck.

    SELECT * INTO TABLE lt_zsdt003
    FROM zsdt003 FOR ALL ENTRIES IN lt_data
    WHERE vbeln_hh = lt_data-vbeln_hh
      AND posnr_hh = lt_data-posnr_hh.
  ENDIF.

  LOOP AT gt_alv2 ASSIGNING <fs_alv2> WHERE box = 'X'.
    LOOP AT lt_data INTO gs_alv2 WHERE vbeln_hh = <fs_alv2>-vbeln_hh
                                    AND mess IS NOT INITIAL.
      <fs_alv2>-id = gs_alv2-id.
      <fs_alv2>-mess = gs_alv2-mess.
      <fs_alv2>-vbeln_ck = gs_alv2-vbeln_ck.
      EXIT.
    ENDLOOP.

    READ TABLE lt_lips INTO ls_lips WITH KEY vbeln_ck = <fs_alv2>-vbeln_ck
                                             charg = <fs_alv2>-charg.
    IF sy-subrc = 0.
      <fs_alv2>-posnr_ck = ls_lips-posnr_pc.
    ENDIF.
    IF <fs_alv2>-id = icon_led_green.
      READ TABLE lt_zsdt003 ASSIGNING <fs_zsdt003> WITH KEY vbeln_hh = <fs_alv2>-vbeln_hh
                                                            posnr_hh = <fs_alv2>-posnr_hh
                                                            charg    = <fs_alv2>-charg.
      IF sy-subrc = 0.
        <fs_zsdt003>-vbeln_ck = <fs_alv2>-vbeln_ck.
        <fs_zsdt003>-posnr_ck = <fs_alv2>-posnr_ck.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lt_zsdt003 IS NOT INITIAL.
    MODIFY zsdt003 FROM TABLE lt_zsdt003.
    COMMIT WORK AND WAIT.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_REPLACE_DELIVERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM change_replace_delivery .
  "交货单批次拆分
  DATA: ls_header         TYPE bapiobdlvhdrchg,
        ls_header_control TYPE bapiobdlvhdrctrlchg,
        lt_return         TYPE TABLE OF bapiret2 WITH HEADER LINE.
  DATA:l_mblnr TYPE mkpf-mblnr.
  CLEAR: ls_header,ls_header_control.
  DATA:  lt_item         TYPE STANDARD TABLE OF bapiobdlvitemchg,
         ls_item         TYPE bapiobdlvitemchg,
         ls_headdeadline LIKE TABLE OF bapidlvdeadln WITH HEADER LINE,
         "外向交货项目级别控制数据
         lt_item_control TYPE STANDARD TABLE OF bapiobdlvitemctrlchg,
         ls_item_control TYPE bapiobdlvitemctrlchg,
         "确认向外交货拣配数据项目等级(SPE)
         lt_item_spl     TYPE STANDARD TABLE OF /spe/bapiobdlvitemchg,
         ls_item_spl     TYPE /spe/bapiobdlvitemchg.
  DATA:lt_data TYPE TABLE OF ty_alv2.
  DATA:lt_data_temp TYPE TABLE OF ty_alv2.
  DATA:lv_msg TYPE c LENGTH 220.

  LOOP AT gt_alv2 ASSIGNING <fs_alv2> WHERE box = 'X'.
    CLEAR <fs_alv2>-mess.
    IF <fs_alv2>-menge <= 0.
      <fs_alv2>-id = icon_led_red.
      <fs_alv2>-mess = '出货数量必须大于0'.
    ENDIF.
    CHECK <fs_alv2>-mess IS INITIAL.
    APPEND <fs_alv2> TO lt_data.
  ENDLOOP.

  SORT lt_data BY vbeln_ck posnr_pc.

  LOOP AT lt_data ASSIGNING <fs_alv2>.
    ls_item-deliv_numb = <fs_alv2>-vbeln_ck.
    ls_item-deliv_item = <fs_alv2>-posnr_pc.
    ls_item-material = <fs_alv2>-matnr.
    ls_item-batch = <fs_alv2>-charg.
    ls_item-hieraritem = <fs_alv2>-posnr_hh.
    ls_item-usehieritm = '1'.
    ls_item-dlv_qty = <fs_alv2>-menge.
    ls_item-dlv_qty_imunit = <fs_alv2>-menge.
    ls_item-fact_unit_denom = 1.
    ls_item-fact_unit_nom = 1.
    APPEND ls_item TO lt_item.
    CLEAR ls_item.

    ls_item_control-deliv_numb = <fs_alv2>-vbeln_ck.
    ls_item_control-deliv_item = <fs_alv2>-posnr_pc.
    ls_item_control-chg_delqty = 'X'.
    APPEND ls_item_control TO lt_item_control.
    CLEAR ls_item_control.

    AT END OF vbeln_ck.
      ls_header-deliv_numb = <fs_alv2>-vbeln_ck.
      ls_header_control-deliv_numb = <fs_alv2>-vbeln_ck.

      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
        EXPORTING
          header_data    = ls_header
          header_control = ls_header_control
          delivery       = <fs_alv2>-vbeln_ck
        TABLES
          item_data      = lt_item
          item_control   = lt_item_control
          return         = lt_return.
      LOOP AT lt_return WHERE type = 'E'  OR type = 'A'.
        MESSAGE ID lt_return-id TYPE lt_return-type NUMBER lt_return-number
        WITH lt_return-message_v1 lt_return-message_v2 lt_return-message_v3 lt_return-message_v4 INTO lv_msg.
        <fs_alv2>-id = icon_led_red.
        CONCATENATE <fs_alv2>-mess lv_msg INTO <fs_alv2>-mess SEPARATED BY '/'.
        CLEAR:lv_msg.
      ENDLOOP.
      IF sy-subrc = 0.
        SHIFT <fs_alv2>-mess LEFT DELETING LEADING '/'.
        ROLLBACK WORK.
      ELSE.
        COMMIT WORK AND WAIT.
        <fs_alv2>-id = icon_led_green.
        <fs_alv2>-mess = '换货出货单更改成功'.
      ENDIF.
      CLEAR:lt_item,lt_item_control,lt_item_spl,lt_return,ls_header,ls_header_control.
    ENDAT.
  ENDLOOP.

  LOOP AT gt_alv2 ASSIGNING <fs_alv2> WHERE box = 'X'.
    READ TABLE lt_data INTO gs_alv2 WITH KEY vbeln_ck = <fs_alv2>-vbeln_ck
                                             posnr_ck = <fs_alv2>-posnr_ck.
    IF sy-subrc = 0.
      <fs_alv2>-id = gs_alv2-id.
      <fs_alv2>-mess = gs_alv2-mess.
    ENDIF.

  ENDLOOP.

ENDFORM.
FORM data_changed USING p_changed TYPE REF TO cl_alv_changed_data_protocol.
  DATA:ls_modi TYPE lvc_s_modi.
  FIELD-SYMBOLS:<f_new> TYPE lvc_s_modi-value,
                <f_old> TYPE any,
                <f_alv> TYPE any.
  DATA:lv_new_line TYPE REF TO data.
  DATA:lv_alv TYPE ty_alv.
  DATA:lv_kunnr TYPE kna1-kunnr.
  DATA:lv_menge TYPE mseg-menge.

  CREATE DATA lv_new_line LIKE LINE OF <ft_alv>.
  ASSIGN lv_new_line->* TO <f_alv>.
  LOOP AT p_changed->mt_mod_cells INTO ls_modi.
    READ TABLE <ft_alv>  ASSIGNING <f_alv> INDEX ls_modi-row_id.
    ASSIGN ls_modi-value TO <f_new>.
    ASSIGN COMPONENT ls_modi-fieldname OF STRUCTURE <f_alv> TO <f_old>.
    IF sy-subrc = 0.
      CASE 'X'.
        WHEN p_11.
          IF ls_modi-fieldname = 'MENGE'.
            gs_alv = <f_alv>.
*            LOOP AT gt_alv ASSIGNING <fs_alv> WHERE zgroup = gs_alv-zgroup.
*              CHECK ls_modi-row_id NE sy-tabix.
*              <fs_alv>-menge = <fs_alv>-menge * ( <f_new> / <f_old> ).
*            ENDLOOP.
            lv_menge = <f_new> - <f_old>.
            PERFORM change_front_menge USING gs_alv-matnr
                                             gs_alv-charg
                                             lv_menge
                                             gs_alv-vrkme
                                             gs_alv-wafer_lot
                                             gs_alv-aufnr
                                             gs_alv-proc.
            gs_alv-box = 'X'.
            MODIFY gt_alv FROM gs_alv INDEX ls_modi-row_id TRANSPORTING box.
            <f_old> = <f_new>.
          ELSE.
            <f_old> = <f_new>.
          ENDIF.
        WHEN p_12.
          IF ls_modi-fieldname = 'MENGE'.
            gs_alv = <f_alv>.
            LOOP AT gt_alv ASSIGNING <fs_alv> WHERE zgroup = gs_alv-zgroup AND vbeln_re = gs_alv-vbeln_re.
              CHECK ls_modi-row_id NE sy-tabix.
              <fs_alv>-menge = <fs_alv>-menge * ( <f_new> / <f_old> ).
            ENDLOOP.
          ELSE.
            <f_old> = <f_new>.
          ENDIF.
        WHEN p_21.
          IF ls_modi-fieldname = 'KUNAG_ZD'.
            gs_alv1 = <f_alv>.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <f_new>
              IMPORTING
                output = lv_kunnr.

            SELECT SINGLE kna1~kunnr
                          kna1~name1
                          street
            INTO (gs_alv1-kunag_zd , gs_alv1-name1_sh , gs_alv1-street)
            FROM kna1 JOIN adrc
              ON kna1~adrnr = adrc~addrnumber
            WHERE kunnr = lv_kunnr.
            MODIFY gt_alv1 FROM gs_alv1 INDEX ls_modi-row_id TRANSPORTING name1_sh street.
            <ft_alv> = gt_alv1.
          ELSEIF ls_modi-fieldname = 'KUNAG'.
            gs_alv1 = <f_alv>.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <f_new>
              IMPORTING
                output = lv_kunnr.

            SELECT SINGLE kna1~kunnr
                          kna1~name1
            INTO (gs_alv1-kunag , gs_alv1-name1 )
            FROM kna1
            WHERE kunnr = lv_kunnr.
            MODIFY gt_alv1 FROM gs_alv1 INDEX ls_modi-row_id TRANSPORTING name1.

          ELSEIF ls_modi-fieldname = 'AUART'.
            gs_alv1 = <f_alv>.
            SELECT SINGLE auart bezei INTO (gs_alv1-auart ,gs_alv1-auart_txt)
            FROM tvakt
            WHERE auart = <f_new>
              AND spras = sy-langu.
            MODIFY gt_alv1 FROM gs_alv1 INDEX ls_modi-row_id TRANSPORTING auart auart_txt.
          ELSE.
            <f_old> = <f_new>.
          ENDIF.
        WHEN OTHERS.
          <f_old> = <f_new>.
      ENDCASE.
    ENDIF.

  ENDLOOP.



  DATA:lv_grid TYPE REF TO cl_gui_alv_grid.
  DATA:lv_refresh TYPE lvc_s_stbl.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lv_grid.

  lv_refresh-row = 'X'.
  lv_refresh-col = 'X'.

  CALL METHOD lv_grid->refresh_table_display
    EXPORTING
      is_stable      = lv_refresh
      i_soft_refresh = 'X'
    EXCEPTIONS
      finished       = 1
      OTHERS         = 2.



ENDFORM.
FORM caller_exit USING p_data TYPE slis_data_caller_exit.
  DATA:lv_grid TYPE REF TO cl_gui_alv_grid.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lv_grid.

  CALL METHOD lv_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_ALV_TITLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_alv_title .
  CLEAR:gs_title.
  CASE 'X'.
    WHEN p_11.
      gs_title = '退货单创建'.
    WHEN p_12.
      gs_title = '退货单修改'.
    WHEN p_13.
      gs_title = '退货单查询'.
    WHEN p_21.
      gs_title = '换货单创建'.
    WHEN p_22.
      gs_title = '换货单修改'.
    WHEN p_23.
      gs_title = '换货单查询'.
    WHEN p_31.
      gs_title = '换货单出货创建'.
    WHEN p_32.
      gs_title = '换货单出货修改'.
    WHEN p_33.
      gs_title = '换货单出货查询'.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_REPLACE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_replace_data .
  TYPES: BEGIN OF ty_kna1,
           kunnr TYPE kna1-kunnr,
           name1 TYPE kna1-name1,
         END OF ty_kna1.

  TYPES: BEGIN OF ty_vbpa,
           vbeln  TYPE vbpa-vbeln,
           kunnr  TYPE vbpa-kunnr,
           name1  TYPE kna1-name1,
           street TYPE adrc-street,
         END OF ty_vbpa.

  TYPES: BEGIN OF ty_vbap,
           vbeln  TYPE vbap-vbeln,
           posnr  TYPE vbap-posnr,
           kwmeng TYPE vbap-kwmeng,
         END OF ty_vbap.

  DATA:lt_kna1 TYPE TABLE OF ty_kna1,
       ls_kna1 TYPE ty_kna1.

  DATA:lt_vbpa TYPE TABLE OF ty_vbpa,
       ls_vbpa TYPE ty_vbpa.

  DATA:   lt_vbap TYPE TABLE OF ty_vbap,
          ls_vbap TYPE ty_vbap.

  SELECT vbeln_hh
         posnr_hh
         vkorg
         auart
         kunag
         vbeln_re
         posnr_re
         z~charg
         z~matnr
         z~arktx
         vbap~kwmeng AS menge
         vbap~lgort
  INTO CORRESPONDING FIELDS OF TABLE gt_alv1
  FROM zsdt003 AS z JOIN vbak
    ON z~vbeln_hh = vbak~vbeln
                    JOIN vbap
    ON z~vbeln_hh = vbap~vbeln
   AND z~posnr_hh = vbap~posnr
  WHERE vbeln_hh IN s_vbelh2
    AND vbeln_vf IN s_vbeln2
    AND vbeln_re IN s_vbelr2
    AND vbak~audat IN s_audat2
    AND z~kunag  IN s_kunnr2
    AND vkorg = p_vkorg2.

*  SORT gt_alv1 BY vbeln_hh posnr_hh.
*  DELETE ADJACENT DUPLICATES FROM gt_alv1 COMPARING vbeln_hh posnr_hh charg.

  IF gt_alv1 IS NOT INITIAL.
    SELECT kunnr
           name1
    INTO TABLE lt_kna1
    FROM kna1 FOR ALL ENTRIES IN gt_alv1
    WHERE kunnr = gt_alv1-kunag.

    SELECT vbeln
           vbpa~kunnr
           kna1~name1
           street
    INTO TABLE lt_vbpa
    FROM vbpa JOIN kna1
      ON vbpa~kunnr = kna1~kunnr
              JOIN adrc
      ON kna1~adrnr = adrc~addrnumber
      FOR ALL ENTRIES IN gt_alv1
    WHERE vbeln = gt_alv1-vbeln_hh
      AND parvw = 'WE'.   "sh  ->  we

    SELECT vbeln
           posnr
           kwmeng
    INTO TABLE lt_vbap
    FROM vbap FOR ALL ENTRIES IN gt_alv1
    WHERE vbeln = gt_alv1-vbeln_re
      AND posnr = gt_alv1-posnr_re.

  ENDIF.

  LOOP AT gt_alv1 ASSIGNING <fs_alv1>.
    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = <fs_alv1>-kunag.
    IF sy-subrc = 0.
      <fs_alv1>-name1 = ls_kna1-name1.
    ENDIF.

    READ TABLE lt_vbpa INTO ls_vbpa WITH KEY vbeln = <fs_alv1>-vbeln_hh.
    IF sy-subrc = 0.
      <fs_alv1>-kunag_zd = ls_vbpa-kunnr.
      <fs_alv1>-name1_sh = ls_vbpa-name1.
      <fs_alv1>-street = ls_vbpa-street.
    ENDIF.

    READ TABLE lt_vbap INTO ls_vbap WITH KEY vbeln = <fs_alv1>-vbeln_re
                                             posnr = <fs_alv1>-posnr_re.
    IF sy-subrc = 0.
      <fs_alv1>-menge = ls_vbap-kwmeng.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_FRONT_MENGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM change_front_menge  USING    p_matnr
                                  p_charg
                                  p_menge
                                  p_vrkme
                                  p_wafer_lot
                                  p_aufnr
                                  p_proc.

  DATA: BEGIN OF ls_od,
          front_ord     TYPE  char30,  "  厂内上段制程
          front_prd_ord TYPE  char30,    "上段工单号
          front_proc    TYPE  char30,    "上段制程
          prd_ord       TYPE  char30,   " 本段工单号
          proc          TYPE  char4,   "  本段制程
          wafer_lot     TYPE  char30,   " WAFER_LOT
          mes_bp_batch  TYPE  char30,
          mes_cp_batch  TYPE  char30,
          mes_cog_batch TYPE  char30,
          mes_cof_batch TYPE  char30,
        END  OF ls_od.
  DATA: l_pp_grossdie TYPE d1n2z.
  DATA: l_lfimg    LIKE lips-lfimg,   " 需要数量
        l_menge    LIKE lips-lfimg,   " 实际可用数量
        l_img      LIKE lips-lfimg,   " 最终转换后数量
        l_vbeln_vl LIKE vbrp-vgbel,
        l_fkimg    LIKE vbrp-fkimg.
  DATA: l_menge1   LIKE lips-lfimg.

  DATA: lt_zppt001 LIKE TABLE OF zppt001 WITH HEADER LINE.
  DATA: ls_alv TYPE ty_alv.

  SELECT * INTO TABLE lt_zppt001
  FROM zppt001 WHERE aufnr = p_aufnr AND  zlevel = '5' AND zcanc = ''.

  LOOP AT lt_zppt001.
    READ TABLE gt_alv ASSIGNING <fs_alv> WITH KEY matnr = lt_zppt001-matnr
                                                  charg = lt_zppt001-charg.
    IF sy-subrc NE 0.
      CONTINUE.
    ELSE.
      CALL FUNCTION 'ZFM_READ_BAT_CHARA'
        EXPORTING
          matnr           = lt_zppt001-matnr
          charg           = lt_zppt001-charg
        IMPORTING
          front_ord       = ls_od-front_ord  "  厂内上段制程
          front_prd_ord   = ls_od-front_prd_ord    "上段工单号
          front_proc      = ls_od-front_proc    "上段制程
          prd_ord         = ls_od-prd_ord   " 本段工单号
          proc            = ls_od-proc   "  本段制程
          wafer_lot       = ls_od-wafer_lot   " WAFER_LOT
          mes_bp_batch    = ls_od-mes_bp_batch
          mes_cp_batch    = ls_od-mes_cp_batch
          mes_cog_batch   = ls_od-mes_cog_batch
          mes_cof_batch   = ls_od-mes_cof_batch
        EXCEPTIONS
          batch_not_found = 1
          OTHERS          = 2.

      IF ls_od-wafer_lot NE p_wafer_lot.
        CONTINUE.
      ENDIF.

      " 需要数量
      l_lfimg = p_menge.
      l_menge = p_menge.
      l_img = l_lfimg.
      DATA: ll_grossdie,
            ll_grossdie_qty TYPE d1n2z.
      CLEAR: ll_grossdie, ll_grossdie_qty.
      IF p_proc = 'COG' AND ls_od-proc = 'CP' OR p_proc = 'COG' AND ls_od-proc = 'BP'.
        IF p_vrkme = 'EA'.
          CALL FUNCTION 'ZFM_READ_MAT_CHARA'
            EXPORTING
              matnr           = p_matnr
            IMPORTING
              pp_grossdie     = l_pp_grossdie
            EXCEPTIONS
              class_not_found = 1
              OTHERS          = 2.
          IF sy-subrc <> 0 OR l_pp_grossdie = 0.
            CONTINUE.
          ENDIF.
          ll_grossdie = 'X'.
          ll_grossdie_qty = l_pp_grossdie.
          l_img = l_lfimg / l_pp_grossdie.
          IF l_img >= '0.8' AND l_img < 1.
            l_img = 1.
          ELSEIF l_img < '0.8'.
          ELSE.   " 大于1 四舍五入
          ENDIF.
          IF p_proc = 'COF'.   " add by anxian 20181022
            IF l_img >= '0.5'.
              DATA: lv_input  LIKE mseg-menge,
                    lv_output LIKE mseg-menge.
              lv_input = l_img.
              CALL FUNCTION 'ROUND'
                EXPORTING
                  decimals      = 1
                  input         = lv_input
                  sign          = '+'
                IMPORTING
                  output        = l_img
                EXCEPTIONS
                  input_invalid = 1
                  overflow      = 2
                  type_invalid  = 3
                  OTHERS        = 4.
              IF sy-subrc <> 0.
              ENDIF.
            ENDIF.
          ENDIF.
          IF l_img > 0.
            IF l_img > l_menge.
              l_img = l_menge.
            ENDIF.
          ELSEIF l_img < 0.
            IF l_img < l_menge.
              l_img = l_menge.
            ENDIF.
          ENDIF.
        ELSE.
          l_img = l_menge.
        ENDIF.
        l_menge = l_img.
      ENDIF.
      l_menge1 = l_menge.
      IF <fs_alv>-box = 'X'.
        l_menge = l_menge + <fs_alv>-menge.
      ENDIF.
      IF l_menge > <fs_alv>-fkimg - <fs_alv>-menge_re.
        l_menge = <fs_alv>-fkimg - <fs_alv>-menge_re.
        IF <fs_alv>-box = 'X'.
          l_menge1 = l_menge - <fs_alv>-menge.
        ENDIF.
      ENDIF.
      <fs_alv>-menge = l_menge.
      <fs_alv>-box = 'X'.
      IF ls_od-front_ord NE ''.
        PERFORM change_front_menge USING lt_zppt001-matnr
                                         lt_zppt001-charg
                                         l_menge1
                                         <fs_alv>-vrkme
                                         ls_od-wafer_lot
                                         ls_od-prd_ord
                                         ls_od-proc.
      ENDIF.

      IF ll_grossdie = ' '.
        ll_grossdie_qty = 1.
      ENDIF.

      l_lfimg = l_lfimg - l_menge1 * ll_grossdie_qty .
      IF l_lfimg <= 0.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
