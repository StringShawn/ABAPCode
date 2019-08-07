*&---------------------------------------------------------------------*
*& Report ZPPR012
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr015.
TABLES:mast.
DATA: go_grid TYPE REF TO cl_gui_alv_grid.
DATA: gt_fieldcat   TYPE lvc_t_fcat,
      gs_fieldcat   TYPE lvc_s_fcat,
      gs_layout_lvc TYPE lvc_s_layo.
DATA mcr_fieldcat.
DEFINE mcr_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname  = &1.
  gs_fieldcat-scrtext_s  =
  gs_fieldcat-scrtext_l  =
  gs_fieldcat-scrtext_m  =
  gs_fieldcat-reptext    =
  gs_fieldcat-coltext    = &2.
  gs_fieldcat-edit       = &3.
  gs_fieldcat-checkbox   = &4.
  gs_fieldcat-key        = &5.
  gs_fieldcat-icon       = &6.
  gs_fieldcat-ref_table  = &7.
  gs_fieldcat-ref_field  = &8.
  gs_fieldcat-outputlen  = &9.
  APPEND gs_fieldcat TO gt_fieldcat.
END-OF-DEFINITION.
DATA:it_mast TYPE STANDARD TABLE OF mast,
     wa_mast TYPE mast.

TYPES:BEGIN OF ty_out,
        matnr1 TYPE marc-matnr,
        matnr  TYPE marc-matnr,
        datum  TYPE datum,
        idnrk  TYPE stpox_alv-idnrk,
        ojtxp  TYPE stpox_alv-ojtxp,
        mnglg  TYPE stpox_alv-mnglg,
        mmein  TYPE stpox_alv-mmein,
      END OF ty_out.
DATA:it_out TYPE STANDARD TABLE OF ty_out,
     wa_out TYPE ty_out.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:s_werks FOR mast-werks DEFAULT '1112' OBLIGATORY,
               s_matnr FOR mast-matnr.
PARAMETERS:p_datum TYPE datum OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.


START-OF-SELECTION.

  PERFORM frm_get_date.

  PERFORM frm_show_data.

*&---------------------------------------------------------------------*
*& Form FRM_GET_DATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_date .

  DATA:lt_stb   TYPE STANDARD TABLE OF stpox,
       lw_stb   TYPE stpox,
       lt_stb_h TYPE STANDARD TABLE OF stpox,
       lw_stb_h TYPE stpox.
  DATA: lv_stufe TYPE stpox-stufe.

  SELECT * INTO TABLE it_mast
    FROM mast
   WHERE matnr IN s_matnr
     AND werks IN s_werks.
  LOOP AT it_mast INTO wa_mast.
    REFRESH:lt_stb,lt_stb_h.

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
      EXPORTING
        capid                 = 'PP01'
        datuv                 = p_datum
        emeng                 = '1' " 需求数量
        mehrs                 = 'X'           " 多层展开
        mtnrv                 = wa_mast-matnr
        stlal                 = wa_mast-stlal " Alternative BOM
        stlan                 = wa_mast-stlan " BOM用途
        werks                 = wa_mast-werks
      TABLES
        stb                   = lt_stb
      EXCEPTIONS
        alt_not_found         = 1
        call_invalid          = 2
        material_not_found    = 3
        missing_authorization = 4
        no_bom_found          = 5
        no_plant_data         = 6
        no_suitable_bom_found = 7
        conversion_error      = 8
        OTHERS                = 9.

    CHECK lt_stb IS NOT INITIAL.
    lt_stb_h = lt_stb.
    wa_out-matnr1 = wa_mast-matnr.
    wa_out-matnr = wa_mast-matnr.
    LOOP AT lt_stb INTO lw_stb.

      lv_stufe = lw_stb-stufe - 1.
      READ TABLE lt_stb_h INTO lw_stb_h WITH KEY wegxx = lw_stb-vwegx
                                                 stufe = lv_stufe.
      IF sy-subrc = 0.
        wa_out-matnr = lw_stb_h-idnrk.
      ELSE.
        wa_out-matnr = wa_out-matnr.
      ENDIF.
      wa_out-idnrk = lw_stb-idnrk.
      wa_out-ojtxp = lw_stb-ojtxp.
      wa_out-mmein = lw_stb-meins.
      wa_out-mnglg = lw_stb-mnglg.
*      wa_out-menge  = lw_stb-menge.
*
*      wa_out-work = lw_stb-potx1.
*      wa_out-location = lw_stb-potx2.

      APPEND wa_out TO it_out.
    ENDLOOP.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SHOW_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_show_data .
  REFRESH:gt_fieldcat[].
  mcr_fieldcat 'MATNR1' '成品物料编号' '' '' 'X' '' '' '' '40'.
  mcr_fieldcat 'MATNR' '父项物料编号' '' '' '' '' '' '' '40'.
  mcr_fieldcat 'IDNRK' '组件物料' '' '' '' '' '' '' '40'.
  mcr_fieldcat 'OJTXP' '组件描述' '' '' '' '' '' '' '40'.
  mcr_fieldcat 'MNGLG' '单件用量' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MMEIN' '单位' '' '' '' '' '' '' ''.

* 给layout赋值
  CLEAR gs_layout_lvc.
  gs_layout_lvc-cwidth_opt = abap_true.
  gs_layout_lvc-zebra      = abap_true.
  gs_layout_lvc-no_rowmark = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
*     i_callback_pf_status_set = 'FRM_PF_STATUS_SET_ALV'
*     i_callback_user_command = 'FRM_USER_COMMAND'
      it_fieldcat_lvc    = gt_fieldcat
      is_layout_lvc      = gs_layout_lvc
      i_save             = 'A'
    TABLES
      t_outtab           = it_out
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
  ENDIF.
ENDFORM.