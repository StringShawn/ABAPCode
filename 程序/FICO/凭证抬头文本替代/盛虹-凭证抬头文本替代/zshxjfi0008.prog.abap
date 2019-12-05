**************************************************
*程序名称:凭证抬头文本替代
*创建日期: 2019-11-20
*创建者:XXX
*申请者:XXX
*功能描述:
*============================================
*变更记录
*修改请求号    修改日期    修改人   修改描述
*DEVK911991    2019-11-20   HANDYXH    创建程序
***************************************************
REPORT zshxjfi0008.

*---------------------------------------------------------------------*
* Description   : 凭证抬头文本替代
* developer     : SH-GZY
* biz consultant:
*---------------------------------------------------------------------*
FORM u901  USING    p_bkpf TYPE bkpf
                        p_bseg TYPE bseg.

  DATA:t_zfit_td LIKE TABLE OF ztfi_td WITH HEADER LINE.
  DATA:c_menge TYPE menge_d.
  DATA:c_menge_t TYPE string.
  DATA:c_meins TYPE meins.
  DATA:c_bwart TYPE bwart.
  DATA:c_fixed TYPE string.
  DATA:c_middle TYPE string.
  DATA:c_txt25 TYPE string.
  DATA:c_txt TYPE string.
  DATA:c_name TYPE string.
  DATA:c_kunnr TYPE string.
  DATA:c_lifnr TYPE string.
  DATA:c_field_name TYPE string.
  FIELD-SYMBOLS: <fv_field_value> TYPE any,
                 <fs>             TYPE STANDARD TABLE,
                 <ws>             TYPE any.
  DATA l_field TYPE string.
  DATA: lr_new_line        TYPE REF TO data.
  DATA:i_accit TYPE accit.

  l_field = '(SAPLRWCL)T_ACCIT[]'.

  ASSIGN (l_field) TO <fs>.
*  CREATE DATA LR_NEW_LINE LIKE LINE OF <FS>.
*  ASSIGN LR_NEW_LINE->* TO <WS>.
*  取配置表数据
  IF sy-subrc = 0.


*if p_bkpf-BLART <> 'RV'. "by sw



    SELECT *  INTO CORRESPONDING FIELDS OF TABLE t_zfit_td FROM
        ztfi_td WHERE  boolclass = '001' AND blart = p_bkpf-blart .
    IF t_zfit_td[] IS NOT INITIAL.
      REFRESH t_zfit_td.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE t_zfit_td FROM
        ztfi_td WHERE ( bukrs = p_bkpf-bukrs OR bukrs = '' ) AND boolclass = '001' AND blart = p_bkpf-blart .
    ELSE.
      LOOP AT <fs> INTO i_accit.
        IF i_accit-bwart IS NOT INITIAL.
          c_bwart = i_accit-bwart.

        ENDIF.
      ENDLOOP.
      IF c_bwart IS NOT INITIAL.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE t_zfit_td FROM
                 ztfi_td WHERE ( bukrs = p_bkpf-bukrs OR bukrs = '' ) AND boolclass = '001' AND bwart = c_bwart .
      ENDIF.
    ENDIF.
    SORT t_zfit_td BY priority.
    IF t_zfit_td[] IS NOT INITIAL.

      LOOP AT t_zfit_td.
        IF t_zfit_td-fixed = 'X'.
          CONCATENATE c_txt t_zfit_td-txt25 INTO c_txt.
        ENDIF.

        IF t_zfit_td-bclfield = 'KUNNR'."客户
          LOOP AT <fs> INTO i_accit.
            IF i_accit-kunnr IS NOT INITIAL AND c_kunnr IS INITIAL.
*                CLEAR:C_NAME.
              SELECT SINGLE name1 INTO c_kunnr FROM kna1 WHERE kunnr = i_accit-kunnr.
              CONCATENATE c_txt c_kunnr INTO c_txt.
            ENDIF.
          ENDLOOP.
        ELSEIF t_zfit_td-bclfield = 'LIFNR'."供应商
          LOOP AT <fs> INTO i_accit.
            IF i_accit-lifnr IS NOT INITIAL AND c_lifnr IS INITIAL.
*                CLEAR:C_NAME.
              SELECT SINGLE name1 INTO c_lifnr FROM lfa1 WHERE lifnr = i_accit-lifnr.
              CONCATENATE c_txt c_lifnr INTO c_txt.
            ENDIF.
          ENDLOOP.
        ELSEIF t_zfit_td-bclfield = 'MEINS'."单位.
          CLEAR:c_meins.
          LOOP AT <fs> INTO i_accit.
            IF i_accit-meins IS NOT INITIAL.
              c_meins = i_accit-meins.
            ENDIF.
          ENDLOOP.
          CONCATENATE c_txt c_meins INTO c_txt.
        ELSEIF t_zfit_td-bclfield = 'MENGE'."数量
          LOOP AT <fs> INTO i_accit.
            IF i_accit-menge IS NOT INITIAL.
              c_menge = c_menge +  i_accit-menge.
            ENDIF.
          ENDLOOP.
          c_menge = c_menge / 2.
          c_menge_t = c_menge.
          CONCATENATE c_txt c_menge_t INTO c_txt.
        ENDIF.






*        ELSE.
*          IF t_zfit_td-bclfield = 'KUNNR'."客户
*            LOOP AT <fs> INTO i_accit.
*              IF i_accit-kunnr IS NOT INITIAL AND c_kunnr IS INITIAL.
**                CLEAR:C_NAME.
*                SELECT SINGLE name1 INTO c_kunnr FROM kna1 WHERE kunnr = i_accit-kunnr.
*                CONCATENATE c_txt c_kunnr INTO c_txt.
*              ENDIF.
*            ENDLOOP.
*          ELSEIF t_zfit_td-bclfield = 'LIFNR'."供应商
*            LOOP AT <fs> INTO i_accit.
*              IF i_accit-lifnr IS NOT INITIAL AND c_lifnr IS INITIAL.
**                CLEAR:C_NAME.
*                SELECT SINGLE name1 INTO c_lifnr FROM lfa1 WHERE lifnr = i_accit-lifnr.
*                CONCATENATE c_txt c_lifnr INTO c_txt.
*              ENDIF.
*            ENDLOOP.
*          ELSEIF t_zfit_td-bclfield = 'MEINS'."单位.
*            CLEAR:c_meins.
*            LOOP AT <fs> INTO i_accit.
*              IF i_accit-meins IS NOT INITIAL.
*                c_meins = i_accit-meins.
*              ENDIF.
*            ENDLOOP.
*            CONCATENATE c_txt c_meins INTO c_txt.
*          ELSEIF t_zfit_td-bclfield = 'MENGE'."数量
*            LOOP AT <fs> INTO i_accit.
*              IF i_accit-menge IS NOT INITIAL.
*                c_menge = c_menge +  i_accit-menge.
*              ENDIF.
*            ENDLOOP.
*            c_menge = c_menge / 2.
*            c_menge_t = c_menge.
*            CONCATENATE c_txt c_menge_t INTO c_txt.
*          ENDIF.
*
*
*
*
*
*
*
*
*
*
*
*
*
*        ENDIF.

*
*          CONCATENATE c_fixed t_zfit_td-fixed INTO c_fixed.
*          IF t_zfit_td-bclfield = 'KUNNR'."客户
*            CLEAR:c_name.
*            SELECT SINGLE name1 INTO c_name FROM kna1 WHERE kunnr = p_bseg-kunnr.
*            CONCATENATE c_middle c_name INTO c_middle.
*          ELSEIF t_zfit_td-bclfield = 'LIFNR'."供应商
*            CLEAR:c_name.
*            SELECT SINGLE name1 INTO c_name FROM lfa1 WHERE lifnr = p_bseg-lifnr.
*            CONCATENATE c_middle c_name INTO c_middle.
*          ELSEIF t_zfit_td-bclfield = 'MATNR'."物料
*            CLEAR:c_name.
*            SELECT SINGLE maktx INTO c_name FROM makt WHERE matnr = p_bseg-matnr.
*            CONCATENATE c_middle c_name INTO c_middle.
**    ELSEIF T_ZFIT_TD-BCLFIELD = 'KUNNR'."物料组
*
*          ELSEIF t_zfit_td-bclfield = 'KOSTL'."成本中心
*            CLEAR:c_name.
*            SELECT SINGLE ltext INTO c_name FROM cskt WHERE kostl = p_bseg-kostl.
*            CONCATENATE c_middle c_name INTO c_middle.
*          ELSE.
*            UNASSIGN <fv_field_value>.
*            CONCATENATE 'P_BSEG-' t_zfit_td-bclfield INTO c_field_name.
*            ASSIGN (c_field_name) TO <fv_field_value>.
*            IF sy-subrc = 0.
*              CONCATENATE c_middle <fv_field_value> INTO c_middle.
*            ENDIF.
*          ENDIF.
*          CONCATENATE c_txt25 t_zfit_td-txt25 INTO c_txt25.
*
*     CONCATENATE C_FIXED C_MIDDLE C_TXT25 INTO C_TXT.






      ENDLOOP.

      p_bkpf-bktxt = c_txt.


    ENDIF.






*ENDIF." by sw


  ENDIF.
  IF p_bkpf-awtyp = 'AMDP'.
    CONCATENATE '计提' p_bkpf-gjahr '年' p_bkpf-monat '月折旧和摊销' INTO p_bkpf-bktxt .
  ENDIF.
  IF p_bkpf-awtyp = 'AIBU'.
    p_bkpf-bktxt = '在建工程转固定资产'.
  ENDIF.
  IF p_bkpf-awtyp = 'AMBU'.
    p_bkpf-bktxt = '资产清理'.
  ENDIF.
  IF p_bkpf-awtyp = 'AFRU'.
    p_bkpf-bktxt = '作业报工'.
  ENDIF.
  IF p_bkpf-awtyp = 'AUAK'.
    p_bkpf-bktxt = '订单结算'.
  ENDIF.
  IF p_bkpf-awtyp = 'MLHD'.
    p_bkpf-bktxt = '物料帐结算'.
  ENDIF.

  IF p_bkpf-blart = 'AB'.
    p_bkpf-bktxt = '清账凭证'.
  ENDIF.
  IF p_bkpf-blart = 'PR'.
    p_bkpf-bktxt = '物料价格修改凭证'.
  ENDIF.
  IF p_bkpf-blart = 'CO'.
    p_bkpf-bktxt = '成本报工结算凭证'.
  ENDIF.
  IF p_bkpf-blart = 'ML'.
    p_bkpf-bktxt = '物料帐结算凭证'.
  ENDIF.


ENDFORM.

*---------------------------------------------------------------------*
* Description   : 凭证反记账替代
* developer     : SH-GZY
* biz consultant:
*---------------------------------------------------------------------*
FORM u902 USING is_bkpf TYPE bkpf
                 is_bseg TYPE bseg.

  IF is_bkpf-tcode EQ 'FB1K' AND is_bseg-koart EQ 'K'.
    IF is_bseg-shkzg EQ 'S'.
      is_bseg-xnegp = ''.
    ELSEIF is_bseg-shkzg EQ 'H'.
      is_bseg-xnegp = 'X'.
    ENDIF.
  ELSEIF is_bkpf-tcode EQ 'FB1D' AND is_bseg-koart EQ 'D'.
    IF is_bseg-shkzg EQ 'S'.
      is_bseg-xnegp = 'X'.
    ELSEIF is_bseg-shkzg EQ 'H'.
      is_bseg-xnegp = ''.
    ENDIF.
  ELSEIF is_bkpf-tcode EQ 'FB1S' AND is_bseg-koart EQ 'S'.
    IF is_bseg-shkzg EQ 'S'.
      is_bseg-xnegp = 'X'.
    ELSEIF is_bseg-shkzg EQ 'H'.
      is_bseg-xnegp = ''.
    ENDIF.
  ELSE.
    " do nothing...
  ENDIF.

  PERFORM u902_wrapper USING is_bkpf-bukrs
                              is_bkpf-blart
                              is_bkpf-tcode
                              space
                              is_bseg-hkont
                              is_bseg-shkzg
                     CHANGING is_bseg-xnegp.

ENDFORM.
FORM u902_wrapper USING VALUE(iv_bukrs) TYPE bkpf-bukrs
                         VALUE(iv_blart) TYPE bkpf-blart
                         VALUE(iv_tcode) TYPE bkpf-tcode
                         VALUE(iv_bwart) TYPE mseg-bwart
                         VALUE(iv_hkont) TYPE bseg-hkont
                         VALUE(iv_shkzg) TYPE bseg-shkzg
                   CHANGING ch_xnegp     TYPE bseg-xnegp.


*" 反记账替代逻辑，配置表：ZFIT_FJZPZ
  DATA: lt_zfit_fjzpz TYPE STANDARD TABLE OF ztfi_fjzpz,
        ls_zfit_fjzpz TYPE ztfi_fjzpz.
  DATA: lv_bwart  TYPE mseg-bwart.                                          " 移动类型

  SELECT *
    INTO TABLE lt_zfit_fjzpz
    FROM ztfi_fjzpz
    WHERE ( bukrs = iv_bukrs OR bukrs = '' )
      AND ( blart = iv_blart OR blart = '' )
      AND ( tcode = iv_tcode OR tcode = '' )
      AND ( bwart = iv_bwart OR bwart = '' )
      AND ( hkont_f = iv_hkont OR
            ( hkont_f <= iv_hkont AND hkont_t >= iv_hkont )
          )
      AND ( shkzg = iv_shkzg OR shkzg = '' )
  ORDER BY PRIMARY KEY.
  IF sy-subrc = 0.
*" 如果有多条配置, 则取最后一条
    READ TABLE lt_zfit_fjzpz INTO ls_zfit_fjzpz INDEX lines( lt_zfit_fjzpz ).
    IF sy-subrc = 0.
      ch_xnegp = ls_zfit_fjzpz-xnegp.
    ENDIF.

  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
* Description: 替换折旧科目
*---------------------------------------------------------------------*
FORM u903 USING    is_bkpf  TYPE bkpf
           CHANGING cs_bseg  TYPE bseg.
**BREAK-POINT .
*   CS_BSEG-HKONT = '5301010400'.
*   CS_BSEG-SGTXT = '1212'.
*  BREAk SH-GZY.
  IF is_bkpf-blart EQ 'AF'.
    DATA:ls_anla TYPE anla .

    SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_anla
      FROM anla WHERE bukrs = cs_bseg-bukrs.
    IF ls_anla IS NOT INITIAL AND ls_anla-ord42 = 'B02'.
      IF ls_anla-bukrs = '6100' AND cs_bseg-hkont = '6600120000'.
        cs_bseg-hkont = '5301010400'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
* Description: 合作伙伴替代
*---------------------------------------------------------------------*
FORM u904 USING    is_bkpf  TYPE bkpf
           CHANGING cs_bseg  TYPE bseg.
  IF cs_bseg-xref1 IS NOT INITIAL.

    cs_bseg-vptnr = cs_bseg-xref1.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'   "前缀补零
      EXPORTING
        input  = cs_bseg-vptnr
      IMPORTING
        output = cs_bseg-vptnr.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
* Description:销售发票会计凭证应收账款行项目的参考代码 1
*---------------------------------------------------------------------*
FORM u905 USING    is_bkpf  TYPE bkpf
           CHANGING cs_bseg  TYPE bseg.
  IF is_bkpf-awkey IS NOT INITIAL.
    DATA:ls_vbrp LIKE vbrp.
    DATA:ls_vbpa LIKE vbpa.
    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF ls_vbrp
      FROM vbrp
      WHERE vbrp~posnr = 10 AND vbrp~vbeln = is_bkpf-awkey.
    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF ls_vbpa
      FROM vbpa
      WHERE vbpa~parvw = 'Z1' AND   vbpa~vbeln = ls_vbrp-aubel.
    IF ls_vbpa IS NOT INITIAL.
      cs_bseg-vptnr = ls_vbpa-kunnr.

    ENDIF.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
* Description:销售发票会计凭证应收账款行项目的参考代码 1
*---------------------------------------------------------------------*
FORM u906 USING    is_bkpf  TYPE bkpf
           CHANGING cs_bseg  TYPE bseg.
*  IF is_bkpf-awkey IS NOT INITIAL.
*    DATA:ls_vbrp LIKE vbrp.
*    DATA:ls_vbak LIKE vbak.
*    SELECT SINGLE *
*      INTO CORRESPONDING FIELDS OF ls_vbrp
*      FROM vbrp
*      WHERE vbrp~posnr = 10 AND vbrp~vbeln = is_bkpf-awkey.
*    SELECT SINGLE *
*      INTO CORRESPONDING FIELDS OF ls_vbak
*      FROM vbak
*      WHERE    vbak~vbeln = ls_vbrp-aubel.
*    IF ls_vbak IS NOT INITIAL.
*      cs_bseg-xref3 = ls_vbak-zwmfph.
*    ENDIF.
*  ENDIF.

ENDFORM.



FORM b901 USING    is_bkpf  TYPE bkpf.
*  IF is_bkpf-awkey IS NOT INITIAL.
*    DATA:ls_vbrp LIKE vbrp.
*    DATA:ls_vbak LIKE vbak.
*    SELECT SINGLE *
*      INTO CORRESPONDING FIELDS OF ls_vbrp
*      FROM vbrp
*      WHERE vbrp~posnr = 10 AND vbrp~vbeln = is_bkpf-awkey.
*    SELECT SINGLE *
*      INTO CORRESPONDING FIELDS OF ls_vbak
*      FROM vbak
*      WHERE    vbak~vbeln = ls_vbrp-aubel.
*    IF ls_vbak IS NOT INITIAL.
*      is_bkpf-xref1_hd = ls_vbak-zyshth.
*      is_bkpf-xref2_hd = ls_vbak-zwmfph.
*    ENDIF.
*  ENDIF.

ENDFORM.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
