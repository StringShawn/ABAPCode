**************************************************
*程序名称:采购入库单打印
*创建日期: 2019-11-26
*创建者:XXX
*申请者:XXX
*功能描述:
*============================================
*变更记录
*修改请求号    修改日期    修改人   修改描述
*DEVK912037    2019-11-26   HANDYXH    创建程序
***************************************************
REPORT zshxjmm0004.
TABLES:nast,*nast.

DATA: BEGIN OF nast_key,
        mblnr LIKE mkpf-mblnr,
        mjahr LIKE mkpf-mjahr,
        zeile LIKE mseg-zeile,
      END OF nast_key.

DATA:lt_mseg TYPE TABLE OF mseg,
     ls_mseg TYPE mseg,
     ls_mkpf TYPE mkpf.

DATA:lv_fm_name TYPE rs38l_fnam.


FORM entry_we01 USING ent_retco ent_screen.


  nast_key = nast-objky.

  CLEAR:ls_mkpf,lt_mseg.

  SELECT SINGLE * INTO ls_mkpf FROM mkpf WHERE mblnr = nast_key-mblnr
                                           AND mjahr = nast_key-mjahr.

  SELECT * INTO TABLE lt_mseg FROM mseg WHERE mblnr = nast_key-mblnr
                                          AND mjahr = nast_key-mjahr.

  IF lt_mseg IS INITIAL.  "已经生成物料凭证的
    EXIT.
  ENDIF.

  SELECT * INTO TABLE @DATA(lt_t001k) FROM t001k FOR ALL ENTRIES IN @lt_mseg
  WHERE bwkey = @lt_mseg-werks.
  LOOP AT lt_t001k TRANSPORTING NO FIELDS WHERE bukrs NE '6100'.

  ENDLOOP.
  IF sy-subrc = 0. "只打印新视界
    EXIT.
  ENDIF.

  PERFORM print_mblnr TABLES lt_mseg USING ls_mkpf.

  CLEAR ent_retco.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PRINT_MBLNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MSEG  text
*      -->P_LS_MKPF  text
*----------------------------------------------------------------------*
FORM print_mblnr  TABLES   pt_mseg STRUCTURE mseg
                  USING    ps_mkpf.

  DATA:ls_head TYPE  zsmm_print_001.
  DATA:ls_ztabix TYPE syst_tabix.
  DATA:gt_body LIKE TABLE OF zsmm_print_b_001 WITH HEADER LINE.
  DATA: lv_func_module_name   TYPE rs38l_fnam,
        ls_control_parameters TYPE ssfctrlop,
        ls_job_output_info    TYPE ssfcrescl,
        ls_ssfcrespd          TYPE ssfcrespd,
        lv_index              TYPE i,
        lv_sformname          TYPE tdsfname.
  DATA:output_options TYPE ssfcompop.
  DATA gv_open.

  lv_sformname = 'ZSF_MM_PURCHASEORDER_PRINT'.
*  IF lt_type1-type = '销售入库单' OR lt_type1-type = '销售出库单'.
*    LOOP AT lt_head.
*      lt_head-title = lt_type1-type.
*      MODIFY lt_head.
*    ENDLOOP.
*
*  ENDIF.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lv_sformname
    IMPORTING
      fm_name            = lv_func_module_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  output_options-tdimmed = 'X'.
  output_options-tdnewid = 'X'.
  output_options-tddelete = 'X'.
  output_options-tdfinal = 'X'.
  output_options-tdiexit = 'X'.
  output_options-tddest = 'LP01'.

  ls_control_parameters-preview = 'X'.
  ls_control_parameters-no_open = 'X'.
  ls_control_parameters-no_close = 'X'.
  "CONTROL_PARAMETERS-NO_DIALOG = 'X'.
  "CONTROL_PARAMETERS-DEVICE = 'PRINTER'.


  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = ls_control_parameters
      output_options     = output_options
      user_settings      = ''
*    IMPORTING
*     job_output_options = outopt
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc = 0.
    gv_open = 'X'.
  ENDIF.

  MOVE-CORRESPONDING ps_mkpf TO ls_head.
  REFRESH gt_body[].
  gt_body[] = CORRESPONDING #( pt_mseg[] ).
  CLEAR:ls_ztabix.
  LOOP AT gt_body.
    ls_ztabix = ls_ztabix + 1.
    gt_body-ztabix = ls_ztabix.
    MODIFY gt_body.
  ENDLOOP.
  CALL FUNCTION lv_func_module_name
    EXPORTING
      control_parameters = ls_control_parameters
*     ARCHIVE_INDEX      =
*     ARCHIVE_INDEX_TAB  =
*     ARCHIVE_PARAMETERS =
*     CONTROL_PARAMETERS =
*     MAIL_APPL_OBJ      =
*     MAIL_RECIPIENT     =
*     MAIL_SENDER        =
*     OUTPUT_OPTIONS     =
*     user_settings      = 'X'
      i_header           = ls_head
* IMPORTING
*     DOCUMENT_OUTPUT_INFO       =
*     JOB_OUTPUT_INFO    =
*     JOB_OUTPUT_OPTIONS =
    TABLES
      i_item             = gt_body
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

***>Close Form
  CHECK NOT gv_open IS INITIAL.
  CALL FUNCTION 'SSF_CLOSE'
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    CLEAR gv_open.
  ENDIF.


ENDFORM.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
