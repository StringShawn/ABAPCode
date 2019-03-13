*&---------------------------------------------------------------------*
*&  包含                LZJMLIST_GROUPDAT
*&---------------------------------------------------------------------*


SELECTION-SCREEN BEGIN OF SCREEN 0301 AS WINDOW.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
s_bhnuc1 FOR zsd_jiamengfahuo-bhnuc,
s_werks1 FOR zsd_jiamengfahuo-werks,
s_matnr1 FOR zsd_jiamengfahuo-matnr,
s_zjtm1 FOR zsd_jiamengfahuo-z_jtm,
s_charg1 FOR zsd_jiamengfahuo-charg,
s_crdat1 FOR zsd_jmfh_h-crdat,
s_crusr1 FOR zsd_jmfh_h-crnam,
s_fhdat1 FOR zsd_jmfh_h-fhdat,
s_fhnam1 FOR zsd_jmfh_h-fhnam,
s_jmstu1 FOR zsd_jmfh_h-jmstu.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 0301.

SELECTION-SCREEN BEGIN OF SCREEN 0302 AS WINDOW.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS:
s_bhnuc2 FOR zsd_jiamengfahuo-bhnuc,
s_werks2 FOR zsd_jiamengfahuo-werks,
s_matnr2 FOR zsd_jiamengfahuo-matnr,
s_charg2 FOR zsd_jiamengfahuo-charg,
s_crdat2 FOR zsd_jmfh_h-crdat,
s_crusr2 FOR zsd_jmfh_h-crnam,
s_fhdat2 FOR zsd_jmfh_h-fhdat,
s_fhnam2 FOR zsd_jmfh_h-fhnam,
s_jmstu2 FOR zsd_jmfh_h-jmstu.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF SCREEN 0302.


DATA ebeln TYPE ebeln.
DATA werks TYPE werks_d.
DATA thstu TYPE char1.
SELECTION-SCREEN BEGIN OF SCREEN 0303 AS WINDOW.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECT-OPTIONS:
s_ebeln3 FOR ebeln,
s_werks3 FOR werks,
s_thstu3 FOR thstu,
s_crdat3 FOR sy-datum,
s_thdat3 FOR sy-datum.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF SCREEN 0303.


SELECTION-SCREEN BEGIN OF SCREEN 0305 AS WINDOW.
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-005.
SELECT-OPTIONS:
s_yhdln5 FOR zmm_spyh_h-yhdln,
s_werks5 FOR zmm_spyh_h-werks,
s_charg5 FOR zmm_spyh_t-charg,
s_zjtm5 FOR zmm_spyh_t-z_jtm,
s_statu5 FOR zmm_spyh_h-status,
s_crdat5 FOR zmm_spyh_h-crdat,
s_spdat5 FOR zmm_spyh_h-spdat.
SELECTION-SCREEN END OF BLOCK b5.
SELECTION-SCREEN END OF SCREEN 0305.


SELECTION-SCREEN BEGIN OF SCREEN 0306 AS WINDOW.
SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE text-006.
SELECT-OPTIONS:
s_yhdln6 FOR zmm_fspyh_h-yhdln,
s_werks6 FOR zmm_fspyh_h-werks,
s_charg6 FOR zmm_fspyh_t-charg,
s_statu6 FOR zmm_fspyh_h-status,
s_crdat6 FOR zmm_fspyh_h-crdat,
s_spdat6 FOR zmm_fspyh_h-spdat.
SELECTION-SCREEN END OF BLOCK b6.
SELECTION-SCREEN END OF SCREEN 0306.


DATA kunnr TYPE kna1-kunnr.
SELECTION-SCREEN BEGIN OF SCREEN 0307 AS WINDOW.
SELECTION-SCREEN BEGIN OF BLOCK b7 WITH FRAME TITLE text-007.
PARAMETERS:
  p_bperi7 TYPE isellist-month OBLIGATORY,
  p_eperi7 TYPE isellist-month OBLIGATORY.
SELECT-OPTIONS:
s_kunnr7 FOR kunnr,
s_werks7 FOR werks.
SELECTION-SCREEN END OF BLOCK b7.
SELECTION-SCREEN END OF SCREEN 0307.

SELECTION-SCREEN BEGIN OF SCREEN 0308 AS WINDOW.
SELECTION-SCREEN BEGIN OF BLOCK b8 WITH FRAME TITLE text-008.
SELECT-OPTIONS:
s_bhnuc8 FOR zsd_jiamengfahuo-bhnuc,
s_werks8 FOR zsd_jiamengfahuo-werks,
s_crdat8 FOR zsd_jmfh_h-crdat,
s_crnam8 FOR zsd_jmfh_h-crnam,
s_fhdat8 FOR zsd_jmfh_h-fhdat,
s_fhnam8 FOR zsd_jmfh_h-fhnam,
s_jmstu8 FOR zsd_jmfh_h-jmstu.
SELECTION-SCREEN END OF BLOCK b8.
SELECTION-SCREEN END OF SCREEN 0308.

SELECTION-SCREEN BEGIN OF SCREEN 0309 AS WINDOW.
SELECTION-SCREEN BEGIN OF BLOCK b9 WITH FRAME TITLE text-009.
SELECT-OPTIONS:
s_bhnuc9 FOR zsd_jiamengfahuo-bhnuc,
s_werks9 FOR zsd_jiamengfahuo-werks,
s_crdat9 FOR zsd_jmfh_h-crdat,
s_crnam9 FOR zsd_jmfh_h-crnam,
s_fhdat9 FOR zsd_jmfh_h-fhdat,
s_fhnam9 FOR zsd_jmfh_h-fhnam,
s_jmstu9 FOR zsd_jmfh_h-jmstu.
SELECTION-SCREEN END OF BLOCK b9.
SELECTION-SCREEN END OF SCREEN 0309.

DATA pp TYPE z_pp.
DATA zpp TYPE z_zpp.
DATA lt_values TYPE vrm_values.

SELECTION-SCREEN BEGIN OF SCREEN 0310 AS WINDOW.
SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE text-010.
SELECT-OPTIONS:
s_bhnu10 FOR zsd_jiamengfahuo-bhnuc,
s_werk10 FOR zsd_jiamengfahuo-werks,
s_pp10 FOR pp,
s_zpp10 FOR zpp.
PARAMETERS:
p_sum10 AS LISTBOX VISIBLE LENGTH 20 OBLIGATORY DEFAULT '1'.
SELECTION-SCREEN END OF BLOCK b10.
SELECTION-SCREEN END OF SCREEN 0310.

SELECTION-SCREEN BEGIN OF SCREEN 0311 AS WINDOW.
  SELECTION-SCREEN BEGIN OF BLOCK b11 WITH FRAME TITLE TEXT-011.
  SELECT-OPTIONS:
  s_BHNU11  FOR zsd_jiamengfahuo-bhnuc OBLIGATORY,  "发货单号
  s_werk11  FOR zsd_jiamengfahuo-werks,  "门店编号
  s_crda11  FOR zsd_jmfh_h-crdat,        "创建日期
  S_CRNA11  FOR zsd_jmfh_h-crnam,        "暂存人员
  s_fhda11  FOR zsd_jmfh_h-fhdat,        "发货日期
  s_fhna11  FOR zsd_jmfh_h-fhnam.        "发货人员
  SELECTION-SCREEN END OF BLOCK b11.
SELECTION-SCREEN END OF SCREEN 0311.

SELECTION-SCREEN BEGIN OF SCREEN 0312 AS WINDOW.
  SELECTION-SCREEN BEGIN OF BLOCK b12 WITH FRAME TITLE TEXT-012.
  SELECT-OPTIONS:
  s_BHNU12  FOR zsd_jiamengfahuo-bhnuc,  "发货单号
  s_werk12  FOR zsd_jiamengfahuo-werks,  "门店编号
  s_crda12  FOR zsd_jmfh_h-crdat,        "创建日期
  S_CRNA12  FOR zsd_jmfh_h-crnam,        "暂存人员
  s_fhda12  FOR zsd_jmfh_h-fhdat,        "发货日期
  s_fhna12  FOR zsd_jmfh_h-fhnam.        "发货人员
  SELECTION-SCREEN END OF BLOCK b12.
SELECTION-SCREEN END OF SCREEN 0312.

AT SELECTION-SCREEN OUTPUT.

  CHECK lt_values[] IS INITIAL.

  lt_values = VALUE #( ( key = '1' text = '门店汇总')
                       ( key = '2' text = '中心汇总')
                       ( key = '3' text = '所有汇总') ).

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_SUM10'
      values = lt_values[].


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bperi7.
  PERFORM f4_period CHANGING p_bperi7.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_eperi7.
  PERFORM f4_period CHANGING p_eperi7.


AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'CRET'.
      PERFORM get_data.

      PERFORM refresh_alv.
    WHEN OTHERS.
  ENDCASE.
