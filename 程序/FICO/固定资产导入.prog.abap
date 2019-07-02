*$*$********************************************************************
* Program ID/Name:ZFIR051                 Date written:
* Author's name:hp_xsq                    Last update:
* Program title:Statistics采购上传
* Project Name:  EPR I
* Version:
* Function Spec ID:
*----------------------------------------------------------------------*
* Description: (Incl. Related Function Area and System)
*
*----------------------------------------------------------------------*
* Include:
*
*----------------------------------------------------------------------*
* Calls: (RFC and BPI)
*
*----------------------------------------------------------------------*
* Function Modules:
*
*----------------------------------------------------------------------*
* Table:
*
*----------------------------------------------------------------------*
* Result:
*
*---------------------------------------------------------------------*
* Change History
*----------------------------------------------------------------------*
*     Date   |   Programmer   |   Corr. #   |   Description
*            |                |             |
*            |                |             |
*$*$********************************************************************

report zfir051.

*$*$********************************************************************
*$*$    TABLES                                                         *
*$*$********************************************************************


*$*$********************************************************************
*$*$    INCLUDES                                                       *
*$*$    (FOR INCLUDES WITH DECLARATIVE PURPOSES ONLY                   *
*$*$     I.E. BDC, ALV, ETC.)                                          *
*$*$********************************************************************


*$*$********************************************************************
*$*$    GLOBAL TYPES                                                   *
*$*$********************************************************************


*$*$********************************************************************
*$*$    GLOBAL CONSTANTS                                               *
*$*$********************************************************************


*$*$********************************************************************
*$*$    GLOBAL ELEMENTARY VARIABLES                                    *
*$*$********************************************************************

*$*$********************************************************************
*$*$    GLOBAL INTERNAL TABLES                                         *
*$*$********************************************************************
data:t_zszfir051 type table of zszfir051 with header line.

*$*$********************************************************************
*$*$    PARAMETERS & SELECT-OPTIONS                                    *
*$*$********************************************************************
parameters:p_file     type localfile modif id m1.


*$*$********************************************************************
*$*$    INITIALIZATION                                                 *
*$*$********************************************************************

*$*$********************************************************************
*$*$    AT SELECTION-SCREEN                                            *
*$*$********************************************************************
at selection-screen on value-request for p_file.
  perform frm_search_help_path  using p_file.


*$*$********************************************************************
*$*$    START-OF-SELECTION                                             *
*$*$********************************************************************
start-of-selection.
  perform frm_get_upload_data.


*$*$********************************************************************
*$*$    END-OF-SELECTION                                             *
*$*$********************************************************************
end-of-selection.
  perform frm_alv_out.

*&---------------------------------------------------------------------*
*&      Form  FRM_SEARCH_HELP_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_search_help_path using pr_path.
  data: user_action   type i,
        l_title     type string,
        l_filter    type string,
        i_filetable type filetable,
        file        like line of i_filetable,
        l_rc        type i.

  l_title = 'Select excel to upload'(s01).
  l_filter =  cl_gui_frontend_services=>filetype_excel.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title   = l_title
      file_filter    = l_filter
      multiselection = space
    CHANGING
      file_table     = i_filetable
      rc             = l_rc
      user_action    = user_action.

  if user_action <> cl_gui_frontend_services=>action_ok.
    exit.
  endif.

  read table i_filetable into file index 1.
  pr_path = file-filename.


endform.                    " FRM_SEARCH_HELP_PATH


*&---------------------------------------------------------------------*
*&      Form  FRM_GET_UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_get_upload_data .
  "Common Excel upload data
  types:begin of t_excel,
          text(8000) type c,
        end of t_excel.
  types:begin of typ_upload,
          excela type char250,
          excelb type char250,
          excelc type char250,
          exceld type char250,
          excele type char250,
          excelf type char250,
          excelg type char250,
          excelh type char250,
          exceli type char250,
          excelj type char250,
          excelk type char250,
          excell type char250,
          excelm type char250,
          exceln type char250,
        end of typ_upload.


  constants:c_rstr   type  i value '3',
            c_rend   type  i value '1000',
            c_cstr   type  i value '1',
            c_cend   type  i value '13'.

  data:i_separator       type  c,
       i_file            type  rlgrap-filename,
       wa_l_excel        type  t_excel,
       it_l_excel        type  standard table of t_excel.
  data:lt_upload type table of typ_upload with header line.


  if p_file is initial.
    message s012(zfico_msg) display like 'E'.
    leave list-processing.
  endif.

  clear:i_file.
  i_file = p_file.
*--- Upload data from Excel
  refresh:it_l_excel.
  CALL FUNCTION 'ZZZ_UPLOADEXCEL'
    EXPORTING
      im_filename             = i_file
      im_begin_col            = c_cstr
      im_begin_row            = c_rstr
      im_end_col              = c_cend
      im_end_row              = c_rend
    IMPORTING
      ex_separator            = i_separator
    TABLES
      it_exceltab             = it_l_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      others                  = 3.
  if sy-subrc <> 0.
    message id sy-msgid type 'S' number sy-msgno
           with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like sy-msgty.
    stop.
  endif.

  if it_l_excel is initial.
    message s007(zfico_msg) display like 'E'.
    stop.
  endif.



  loop at it_l_excel into wa_l_excel .
    split wa_l_excel at i_separator
       into lt_upload-excela
            lt_upload-excelb
            lt_upload-excelc
            lt_upload-exceld
            lt_upload-excele
            lt_upload-excelf
            lt_upload-excelg
            lt_upload-excelh
            lt_upload-exceli
            lt_upload-excelj
            lt_upload-excelk
            lt_upload-excell
            lt_upload-excelm
*            lt_upload-exceln
            .
    if lt_upload is initial.
      continue.
    endif.

    if lt_upload-excela is initial.
      continue.
    endif.


    replace all occurrences of ',' in lt_upload-excelh with space.
    condense lt_upload-excelh no-gaps.

    append lt_upload.
  endloop.



  loop at lt_upload.
    t_zszfir051-msg_lcd = icon_led_inactive.
    t_zszfir051-anlkl = lt_upload-excela.
    t_zszfir051-bukrs = lt_upload-excelb.
*    t_zszfir051-nassets = lt_upload-excelc.

    t_zszfir051-anln1 = lt_upload-excelc.
    t_zszfir051-txt50 = lt_upload-exceld.
    t_zszfir051-anlhtxt = lt_upload-excele.
    t_zszfir051-sernr = lt_upload-excelf.
    t_zszfir051-menge = lt_upload-excelg.
    t_zszfir051-meins = 'EA'.
*    t_zszfir051-aktiv = lt_upload-excelh.
    t_zszfir051-kostl = lt_upload-excelh.
    t_zszfir051-kostlv = lt_upload-exceli.
    t_zszfir051-ord43 = lt_upload-excelj.
    t_zszfir051-prctr = lt_upload-excelk.
    t_zszfir051-zanlkl = lt_upload-excela .
    t_zszfir051-txa50 = lt_upload-excell .

* 权限检查
    authority-check object 'ZFI_BUK'
      id  'BUKRS' field t_zszfir051-BUKRS.
    if sy-subrc ne 0.
      message S078(zfico_msg) with t_zszfir051-BUKRS DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    endif.

    append t_zszfir051.
  endloop.


endform.                    " FRM_GET_UPLOAD_DATA

*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_OUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_alv_out .
  data:lt_fieldcat    type lvc_t_fcat,
       lwa_fieldcat   TYPE lvc_s_fcat ,
       lv_repid       like sy-repid,
       lwa_layout     type lvc_s_layo,
       lc_grid        type ref to cl_gui_alv_grid.



  lv_repid                   = sy-repid."""""程序名
  lwa_layout-cwidth_opt      = 'X'.
  lwa_layout-zebra           = 'X'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSZFIR051'
    CHANGING
      ct_fieldcat            = lt_fieldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
           with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

    READ TABLE lt_fieldcat INTO lwa_fieldcat
                           WITH KEY fieldname = 'TXA50' .
    IF sy-subrc = 0 .
      lwa_fieldcat-reptext    = '英文描述' .
      lwa_fieldcat-scrtext_l = '英文描述' .
      lwa_fieldcat-scrtext_m = '英文描述' .
      lwa_fieldcat-scrtext_s = '英文描述' .
      MODIFY lt_fieldcat FROM lwa_fieldcat
            TRANSPORTING scrtext_l scrtext_m scrtext_s reptext
                         WHERE FIELDNAME = 'TXA50'.

    ENDIF.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = lv_repid
      i_callback_pf_status_set = 'FRM_STATUS'
      i_callback_user_command  = 'FRM_UCOMM'
      is_layout_lvc            = lwa_layout
      it_fieldcat_lvc          = lt_fieldcat[]
      i_save                   = 'A'
    TABLES
      t_outtab                 = t_zszfir051
    EXCEPTIONS
      program_error            = 1
      others                   = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
           with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " FRM_ALV_OUT
*&---------------------------------------------------------------------*
*&      form  FRM_STATUS
*&---------------------------------------------------------------------*
*       alv状态条
*----------------------------------------------------------------------*
*      -->rt_extab   text
*----------------------------------------------------------------------*
form frm_status using rt_extab type slis_t_extab.
  append 'ZSAVE' to rt_extab .
  set pf-status 'ZSTANDARD'.
endform. "FRM_STATUS
*&---------------------------------------------------------------------*
*&      Form  FRM_UCOMM
*&---------------------------------------------------------------------*
*       ALV事件
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
form frm_ucomm using r_ucomm     like sy-ucomm
                     rs_selfield type slis_selfield.
  data: lr_grid type ref to cl_gui_alv_grid.
  data:lv_answer type char1.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.
  CALL METHOD lr_grid->check_changed_data.
  data: l_valid type c.
  rs_selfield-refresh    = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-row_stable = 'X'.
  if r_ucomm = 'ZSAVE'.
    perform frm_save_data.
  elseif r_ucomm = '&BACK' or r_ucomm = '&EXIT' or r_ucomm = '&CANC'.
    leave to screen 0.
  endif.
endform. "FRM_UCOMM

*&---------------------------------------------------------------------*
*&      Form  FRM_SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_save_data .

  data:lwa_key type bapi1022_key,
       lwa_reference type bapi1022_reference.
  data:lwa_bapi1022_1 type bapi1022_1.
  data:lwa_bapiret2 type bapiret2.
  data:lwa_generaldata type bapi1022_feglg001,
       lwa_generaldatax type bapi1022_feglg001x,
       lwa_postinginformation type bapi1022_feglg002,
       lwa_postinginformationx type bapi1022_feglg002x,
       lwa_allocations type bapi1022_feglg004,
       lwa_allocationsx type bapi1022_feglg004x,
       lwa_timedependentdata type bapi1022_feglg003,
       lwa_timedependentdatax type bapi1022_feglg003x,
       lwa_extensionin TYPE bapiparex .
  DATA lt_extensionin TYPE STANDARD TABLE OF bapiparex .

  loop at t_zszfir051 where msg_lcd = icon_led_inactive.
    clear:lwa_key,lwa_reference,lwa_bapi1022_1,lwa_bapiret2,
          lwa_generaldata,lwa_generaldatax,lwa_postinginformation,lwa_postinginformationx,
          lwa_allocations,lwa_allocationsx,lwa_timedependentdata,lwa_timedependentdatax,
          lwa_extensionin ,lt_extensionin[] .

    lwa_key-companycode = t_zszfir051-bukrs.
    lwa_key-asset = t_zszfir051-anln1.


    lwa_generaldata-assetclass = t_zszfir051-anlkl.
    lwa_generaldatax-assetclass = 'X'.

    lwa_generaldata-descript = t_zszfir051-txt50.
    lwa_generaldatax-descript = 'X'.

    lwa_generaldata-descript2 = t_zszfir051-txa50 .
    lwa_generaldatax-descript2 = 'X' .

    lwa_generaldata-main_descript = t_zszfir051-anlhtxt.
    lwa_generaldatax-main_descript = 'X'.

    lwa_generaldata-quantity = t_zszfir051-menge.
    lwa_generaldatax-quantity = 'X'.

    lwa_generaldata-base_uom = t_zszfir051-meins.
    lwa_generaldatax-base_uom = 'X'.

    lwa_generaldata-serial_no = t_zszfir051-sernr.
    lwa_generaldatax-serial_no = 'X'.


    if t_zszfir051-ord43 is not initial.
      lwa_allocations-evalgroup3 = t_zszfir051-ord43.
      lwa_allocationsx-evalgroup3 = 'X'.
    endif.

    if t_zszfir051-prctr is not initial.
      lwa_allocations-evalgroup4 = t_zszfir051-prctr.
      lwa_allocationsx-evalgroup4 = 'X'.
    endif.

    if t_zszfir051-kostl is not initial.
      lwa_timedependentdata-costcenter = t_zszfir051-kostl.
      lwa_timedependentdatax-costcenter = 'X'.
    endif.

    if t_zszfir051-kostlv is not initial.
      lwa_timedependentdata-resp_cctr = t_zszfir051-kostlv.
      lwa_timedependentdatax-resp_cctr = 'X'.
    endif.

    IF t_zszfir051-zanlkl IS NOT INITIAL .
      lwa_extensionin-structure = 'BAPI_TE_ANLU' .
*      t_zszfir051-anln2 = '0000' .
*      CONCATENATE t_zszfir051-bukrs t_zszfir051-anln1 t_zszfir051-anln2 t_zszfir051-anlkl
*          INTO lwa_extensionin-valuepart1 .
      lwa_extensionin-valuepart1+0(4)  = t_zszfir051-bukrs .
      lwa_extensionin-valuepart1+4(12)  = t_zszfir051-anln1 .
      lwa_extensionin-valuepart1+16(4)  = t_zszfir051-anln2 .
      lwa_extensionin-valuepart1+20(8)  = t_zszfir051-anlkl .
      APPEND lwa_extensionin TO lt_extensionin .
      CLEAR lwa_extensionin.
    ENDIF.


    CALL FUNCTION 'BAPI_FIXEDASSET_CREATE1'
      EXPORTING
        key                 = lwa_key
        generaldata         = lwa_generaldata
        generaldatax        = lwa_generaldatax
*       postinginformation  = lwa_postinginformation
*       postinginformationx = lwa_postinginformationx
        allocations         = lwa_allocations
        allocationsx        = lwa_allocationsx
        timedependentdata   = lwa_timedependentdata
        timedependentdatax  = lwa_timedependentdatax
      IMPORTING
        companycode         = lwa_bapi1022_1-comp_code
        asset               = lwa_bapi1022_1-assetmaino
        subnumber           = lwa_bapi1022_1-assetsubno
        return              = lwa_bapiret2
      TABLES
        EXTENSIONIN         = lt_extensionin.

    if lwa_bapiret2-type = 'E' or lwa_bapiret2-type = 'I' or lwa_bapiret2-type = 'X'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      t_zszfir051-msg_lcd = icon_led_red.
    else.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      t_zszfir051-msg_lcd = icon_led_green.
    endif.

    if lwa_bapiret2-type is not initial.
      message id lwa_bapiret2-id type lwa_bapiret2-type number lwa_bapiret2-number
             with lwa_bapiret2-message_v1 lwa_bapiret2-message_v2 lwa_bapiret2-message_v3 lwa_bapiret2-message_v4
             into t_zszfir051-msg_txt.
    endif.

    modify t_zszfir051.
  endloop.
endform.                    " FRM_SAVE_DATA
