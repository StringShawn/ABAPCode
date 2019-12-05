**************************************************
*程序名称:银行主数据创建
*创建日期: 2019-11-20
*创建者:XXX
*申请者:XXX
*功能描述:
*============================================
*变更记录
*修改请求号    修改日期    修改人   修改描述
* DEVK912017    2019-11-20   HANDYXH    创建程序
***************************************************
REPORT zshxjfi0009.

DATA:gw_sinsert LIKE rf02b-counter.
DATA:gw_einsert LIKE rf02b-counter.
DATA:gt_data LIKE TABLE OF zTfi_bank_log WITH HEADER LINE.
DATA:gt_bnka LIKE TABLE OF bnka WITH HEADER LINE .

DATA: BEGIN OF lt_insert OCCURS 0.
        INCLUDE STRUCTURE bnka.
DATA: END OF lt_insert.
DATA: BEGIN OF lt_suctab OCCURS 0.
        INCLUDE STRUCTURE suc_and_err_tabs.
DATA: END OF lt_suctab.

DATA: BEGIN OF lt_errtab OCCURS 0.
        INCLUDE STRUCTURE suc_and_err_tabs.
DATA: END OF lt_errtab.

START-OF-SELECTION.

*获取数据
  PERFORM frm_get_data.
*处理数据
  PERFORM frm_process_data.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_data .

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM zTfi_bank_log WHERE cjzt <> 1 AND dycs < 3.
  IF gt_data[] IS NOT INITIAL.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE gt_bnka
      FROM bnka
      FOR ALL ENTRIES IN gt_data
      WHERE banks = gt_data-banks AND bankl = gt_data-bankk.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_process_data .
  LOOP AT gt_data.
    CLEAR:gw_sinsert,gw_einsert.

    READ TABLE gt_bnka WITH  KEY banks = gt_data-banks bankl = gt_data-bankk.
    IF sy-subrc <> 0.
      "创建银行主数据
      gt_data-dycs = gt_data-dycs + 1.
      REFRESH lt_insert.
      MOVE-CORRESPONDING gt_data TO lt_insert.
      lt_insert-bankl = gt_data-bankk.
      lt_insert-erdat = gt_data-zerdat.
      lt_insert-ernam = gt_data-zernam.
      lt_insert-provz = gt_data-regio.
      APPEND lt_insert.
      CALL FUNCTION 'BANK_INSERT'
        EXPORTING
          i_real      = 'X'
        IMPORTING
          cnt_sinsert = gw_sinsert
          cnt_einsert = gw_einsert
        TABLES
          tab_insert  = lt_insert
          suctab      = lt_suctab
          errtab      = lt_errtab.
      IF lt_errtab[] IS INITIAL.
        UPDATE zTfi_bank_log SET dycs = gt_data-dycs  cjzt = 1 WHERE logid = gt_data-logid.
      ELSE.
        UPDATE zTfi_bank_log SET dycs = gt_data-dycs cjzt = 2 WHERE logid = gt_data-logid.
      ENDIF.

    ELSE.
      "修改银行主数据
      gt_data-dycs = gt_data-dycs + 1.
      REFRESH lt_insert.
      MOVE-CORRESPONDING gt_data TO lt_insert.
      lt_insert-bankl = gt_data-bankk.
      lt_insert-erdat = gt_data-zerdat.
      lt_insert-ernam = gt_data-zernam.
      lt_insert-provz = gt_data-regio.
      APPEND lt_insert.
      CALL FUNCTION 'BANK_MODIFY'
        EXPORTING
          i_real      = 'X'
        IMPORTING
          cnt_supdate = gw_sinsert
          cnt_eupdate = gw_einsert
        TABLES
          tab_modify  = lt_insert
          suctab      = lt_suctab
          errtab      = lt_errtab.
      IF lt_errtab[] IS INITIAL.
        UPDATE zTfi_bank_log SET dycs = gt_data-dycs  cjzt = 1 WHERE logid = gt_data-logid.
      ELSE.
        UPDATE zTfi_bank_log SET dycs = gt_data-dycs cjzt = 2 WHERE logid = gt_data-logid.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
