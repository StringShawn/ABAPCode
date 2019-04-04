*******************************************************************
* 事务代码：ZPP068
* 程序名称：ZPP068
* 程序目的：物料特性和特性值查询报表
* 程序类型：ABAP/4 程序  ，REPORT：报表
* 应用类型：PP
* 自建表：
* 开发人员：HAND_YXH
*(修改日志)--------------------------------------------------------
*
* 日志号   修改人  修改时间       修改说明              传输号码
*  ----    ----    ------         -----------
*
********************************************************************


REPORT zpp068.

TABLES: marc.
TYPE-POOLS: slis.

*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_ibin,
         in_recno TYPE ibin-in_recno,
         instance TYPE ibin-instance,
         atinn    TYPE v_ibin_syval-atinn,
         atwrt    TYPE v_ibin_syval-atwrt,
         atflv    TYPE v_ibin_syval-atflv,
       END OF ty_ibin,


       BEGIN OF ty_marc,
         matnr TYPE marc-matnr,
         stdpd TYPE marc-stdpd,
         werks TYPE marc-werks,
         cuobj TYPE marc-cuobj,
         dzeit TYPE marc-dzeit,
       END OF ty_marc,


       BEGIN OF ty_zzchar,
         matnr  TYPE marc-matnr,
         maktx  TYPE makt-maktx,
         maktx1 TYPE makt-maktx,
         stdpd  TYPE marc-stdpd,
         werks  TYPE marc-werks,
         atinn  TYPE ibsymbol-atinn,
         atbez  TYPE cabnt-atbez,
         atwrt  TYPE v_ibin_syval-atwrt,
         atwtb  TYPE cawnt-atwtb,
         dzeit  TYPE marc-dzeit,
         tians  TYPE char2,
         slbox  TYPE c,
       END OF ty_zzchar,

       BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END OF ty_makt,

       BEGIN OF ty_cabnt,
         atinn TYPE cabnt-atinn,
         atbez TYPE cabnt-atbez,
       END OF ty_cabnt,

       BEGIN OF ty_cawn,
         atinn TYPE cawn-atinn,
         atwrt TYPE cawn-atwrt,
         atwtb TYPE cawnt-atwtb,
       END OF ty_cawn ,

       BEGIN OF ty_cabn,
         atinn TYPE cabn-atinn,
         anzdz TYPE cabn-anzdz,
         msehi TYPE cabn-msehi,
       END OF ty_cabn.


*----------------------------------------------------------------------*
* 数据定义
*----------------------------------------------------------------------*

DATA: it_ibin   TYPE STANDARD TABLE OF ty_ibin,
      wa_ibin   LIKE LINE OF it_ibin,
      it_marc   TYPE STANDARD TABLE OF ty_marc,
      wa_marc   LIKE LINE OF it_marc,
      it_zzchar TYPE STANDARD TABLE OF ty_zzchar,
      wa_zzchar TYPE ty_zzchar,
      it_makt   TYPE STANDARD TABLE OF ty_makt,
      it_makt1  TYPE STANDARD TABLE OF ty_makt,
      it_cabnt  TYPE STANDARD TABLE OF ty_cabnt,
      it_cawn   TYPE STANDARD TABLE OF ty_cawn,
      it_cabn   TYPE STANDARD TABLE OF ty_cabn.

DATA: p_time  TYPE c LENGTH 14.  "ibin-valfr.

*alv
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv.
DATA: gs_fieldcat TYPE LINE OF slis_t_fieldcat_alv.
DATA: gs_layout TYPE slis_layout_alv.

*       Batchinputdata of single transaction
DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

*       error session opened (' ' or 'X')

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS:  s_stdpd FOR marc-stdpd ,
                 s_matnr FOR marc-matnr ,
                 s_werks FOR marc-werks .
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN.

  IF s_matnr IS NOT INITIAL AND s_stdpd IS NOT INITIAL.
    MESSAGE '可配置物料与标准物料二者之间只能选择其中一种作为查询条件' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.

  CONCATENATE sy-datum sy-uzeit INTO p_time.
  PERFORM f_get_data.

END-OF-SELECTION.

  PERFORM f_alv_fieldcat.
  PERFORM f_layout.
  PERFORM f_display.

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data .
  DATA: l_atinn LIKE wa_ibin-atinn.
  DATA: lv_xiaoshu TYPE cha_class_data-stellen,
        lv_fltp    TYPE cha_class_data-sollwert.
  DATA: lv_atflv_string TYPE cha_class_view-sollwert.
  "获得查询的物料的CUOBJ
  SELECT matnr
       werks
       cuobj
       stdpd
       dzeit
  INTO CORRESPONDING FIELDS OF TABLE it_marc
  FROM marc
  WHERE matnr IN s_matnr
  AND werks IN s_werks
  AND stdpd IN s_stdpd
  AND stdpd <> '' .
  SORT it_marc BY matnr werks.
  DELETE ADJACENT DUPLICATES FROM it_marc COMPARING matnr werks.

  "查出物料的特性和特性值
  IF it_marc[] IS NOT INITIAL.
    SELECT ibin~in_recno      " IB：唯一记录编号
           ibin~instance      " IB：组件（示例）与marc-cuobj 对应确定物料号
           v_ibin_syval~atinn " 特性编号
           v_ibin_syval~atwrt " 特性值
           v_ibin_syval~atflv "数字值
      INTO CORRESPONDING FIELDS OF TABLE it_ibin
      FROM ibin INNER JOIN v_ibin_syval ON ibin~in_recno = v_ibin_syval~in_recno "IB：符号数字标识符
      FOR ALL ENTRIES IN it_marc
      WHERE ibin~instance = it_marc-cuobj
        AND ibin~valfr < p_time   " IB：有效起始日
    AND ibin~valto >= p_time.  " IB：有效截止日

    SELECT matnr
           maktx
    INTO TABLE it_makt
    FROM makt FOR ALL ENTRIES IN it_marc
    WHERE matnr = it_marc-matnr
      AND spras = sy-langu.

    SELECT matnr
           maktx
    INTO TABLE it_makt1
    FROM makt FOR ALL ENTRIES IN it_marc
    WHERE matnr = it_marc-stdpd
      AND spras = sy-langu.

  ENDIF.
  SORT it_ibin BY instance atinn atwrt.

  IF it_ibin IS NOT INITIAL.
    SELECT atinn
           atbez
    INTO TABLE it_cabnt
    FROM cabnt FOR ALL ENTRIES IN it_ibin
    WHERE atinn = it_ibin-atinn.

    SELECT atinn
           anzdz
           msehi
    INTO TABLE it_cabn
    FROM cabn FOR ALL ENTRIES IN it_ibin
    WHERE atinn = it_ibin-atinn.

    SELECT cawn~atinn
           cawn~atwrt
           atwtb
    INTO TABLE it_cawn
    FROM cawn JOIN cawnt
      ON cawn~atinn = cawnt~atinn AND cawn~atzhl = cawnt~atzhl
      FOR ALL ENTRIES IN it_ibin
    WHERE cawn~atinn = it_ibin-atinn
      AND cawn~atwrt = it_ibin-atwrt .

  ENDIF.

  LOOP AT it_ibin INTO wa_ibin.
    CLEAR wa_zzchar.
*    CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
*      EXPORTING
*        INPUT  = WA_IBIN-ATINN
*      IMPORTING
*        OUTPUT = L_ATINN.
    wa_zzchar-atinn = wa_ibin-atinn.
    wa_zzchar-atwrt = wa_ibin-atwrt.
    IF wa_zzchar-atwrt IS INITIAL.
      READ TABLE it_cabn INTO DATA(wa_cabn) WITH KEY atinn = wa_ibin-atinn.
      IF sy-subrc = 0.
        CLEAR:lv_atflv_string, lv_fltp,lv_xiaoshu.
        lv_xiaoshu = wa_cabn-anzdz.
        lv_fltp = wa_ibin-atflv.
        CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
          EXPORTING
            i_number_of_digits = lv_xiaoshu
            i_fltp_value       = lv_fltp
          IMPORTING
            e_char_field       = lv_atflv_string.
        CONDENSE lv_atflv_string.
        IF wa_cabn-msehi IS NOT INITIAL.
          TRANSLATE wa_cabn-msehi TO LOWER CASE.
          wa_zzchar-atwrt = lv_atflv_string && wa_cabn-msehi.
        ELSE.
          wa_zzchar-atwrt = lv_atflv_string.
        ENDIF.
      ENDIF.

    ENDIF.
    READ TABLE it_cabnt INTO DATA(wa_cabnt) WITH KEY atinn = wa_ibin-atinn.
    IF sy-subrc = 0.
      wa_zzchar-atbez = wa_cabnt-atbez.
    ENDIF.

    READ TABLE it_cawn INTO DATA(wa_cawn) WITH KEY atinn = wa_ibin-atinn atwrt = wa_ibin-atwrt .
    IF sy-subrc = 0.
      wa_zzchar-atwtb = wa_cawn-atwtb.
    ENDIF.

    READ TABLE it_marc INTO wa_marc WITH  KEY cuobj =  wa_ibin-instance .
    IF sy-subrc = 0.
      wa_zzchar-matnr = wa_marc-matnr.
      wa_zzchar-stdpd = wa_marc-stdpd.
      wa_zzchar-werks = wa_marc-werks.
      wa_zzchar-dzeit = wa_marc-dzeit.
      wa_zzchar-tians = '天'.
*      SELECT SINGLE maktx INTO wa_zzchar-maktx FROM makt WHERE matnr = wa_zzchar-matnr AND spras = sy-langu.
      READ TABLE it_makt INTO DATA(wa_makt) WITH KEY matnr = wa_zzchar-matnr.
      IF sy-subrc = 0.
        wa_zzchar-maktx = wa_makt-maktx.
      ENDIF.

      READ TABLE it_makt1 INTO wa_makt WITH KEY matnr = wa_zzchar-stdpd.
      IF sy-subrc = 0.
        wa_zzchar-maktx1 = wa_makt-maktx.
      ENDIF.
    ENDIF.
    APPEND wa_zzchar TO it_zzchar.
  ENDLOOP.
  SORT it_zzchar BY stdpd matnr.
ENDFORM.                    " F_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  f_alv_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_alv_fieldcat .
  DATA: pos TYPE i.
  DEFINE fieldcat.
    clear gs_fieldcat.
    pos = pos + 1.
    gs_fieldcat-col_pos = pos.
    gs_fieldcat-fieldname = &1.
    gs_fieldcat-ref_tabname = &2.
    gs_fieldcat-ref_fieldname = &3.
    gs_fieldcat-seltext_l = &4.
    gs_fieldcat-no_zero = &5.
    gs_fieldcat-key = &6.
    append gs_fieldcat to gt_fieldcat.
  END-OF-DEFINITION.

  fieldcat 'WERKS' 'MARC' 'WERKS' '' '' ''.
  fieldcat 'MATNR' 'MAKT' 'MATNR' '' '' ''.
  fieldcat 'MAKTX' '' '' '物料描述' '' ''.
  fieldcat 'STDPD' 'MARC' 'STDPD' '超级BOM物料' '' ''.
  fieldcat 'MAKTX1' '' '' '超级BOM物料描述' '' ''.

  "FIELDCAT 'DZEIT' '' '' '交货所需天数' '' ''.
  "FIELDCAT 'TIANS' '' '' '单位' '' ''.
  fieldcat 'ATINN' 'IBSYMBOL' 'ATINN' '特性' '' ''.
  fieldcat 'ATBEZ' '' '' '特性描述' '' ''.
  fieldcat 'ATWRT' '' '' '特性值' '' ''.
  fieldcat 'ATWTB' '' '' '特性值描述' '' ''.

ENDFORM.                    "f_alv_fieldcat

*&---------------------------------------------------------------------*
*&      Form  f_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_layout .
  gs_layout-zebra = 'X'.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-box_fieldname = 'SLBOX'.
ENDFORM.                    " F_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  f_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_display .

*  可以保存格式
  DATA:gs_variant   TYPE disvariant.
  gs_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      i_save                  = 'A'         "可以保存格式
      is_variant              = gs_variant  "LAYOUT参数 可以保存格式
      is_layout               = gs_layout
      it_fieldcat             = gt_fieldcat
*     i_grid_title            = alv_title
*     i_callback_pf_status_set    = 'SET_PF_STATUS'
      i_callback_user_command = 'F_USER_COMMAND'
*     it_sort                 = it_sort
*     i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
*     i_html_height_top       = 15
    TABLES
      t_outtab                = it_zzchar
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "f_display
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM f_user_command USING r_ucomm TYPE sy-ucomm
                            rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&IC1'.

      IF rs_selfield-fieldname = 'MATNR' OR rs_selfield-fieldname = 'STDPD'.
        PERFORM frm_cal_tran1 USING rs_selfield.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CAL_TRAN1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM frm_cal_tran1  USING    p_rs_selfield  TYPE slis_selfield..
  READ TABLE it_zzchar INTO wa_zzchar INDEX p_rs_selfield-tabindex.
  IF sy-subrc = 0.

    IF p_rs_selfield-fieldname = 'MATNR'.
      PERFORM bdc_dynpro      USING 'SAPLMGMM' '0060'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RMMG1-MATNR'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ENTR'.
      PERFORM bdc_field       USING 'RMMG1-MATNR'
                                    wa_zzchar-matnr.
      PERFORM bdc_dynpro      USING 'SAPLMGMM' '0070'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'MSICHTAUSW-DYTXT(14)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ENTR'.
      PERFORM bdc_field       USING 'MSICHTAUSW-KZSEL(14)'
                                    'X'.
      PERFORM bdc_dynpro      USING 'SAPLMGMM' '0080'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RMMG1-WERKS'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ENTR'.
      PERFORM bdc_field       USING 'RMMG1-WERKS'
                                    wa_zzchar-werks.

      CALL TRANSACTION 'MM03' USING bdcdata MODE 'E' MESSAGES INTO messtab.
    ELSEIF p_rs_selfield-fieldname = 'STDPD'.
      SET PARAMETER ID 'MAT' FIELD wa_zzchar-stdpd.
      SET PARAMETER ID 'MXX' FIELD 'C'.
    ENDIF.

*    SET PARAMETER ID 'WRK'  FIELD GS_DATA-WERKS.

    CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
    CLEAR:   bdcdata[],wa_zzchar,messtab[].
  ENDIF.

ENDFORM.                    " FRM_CAL_TRAN1
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.

ENDFORM.                    "BDC_FIELD