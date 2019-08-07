*&---------------------------------------------------------------------*
*& Report ZPPR012
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zppr014.
tables:mast.
data: go_grid type ref to cl_gui_alv_grid.
data: gt_fieldcat   type lvc_t_fcat,
      gs_fieldcat   type lvc_s_fcat,
      gs_layout_lvc type lvc_s_layo.
data mcr_fieldcat.
define mcr_fieldcat.
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
end-of-definition.
data:it_mast type standard table of mast,
     wa_mast type mast.
data: begin of alv_stb occurs 0.
    include structure stpox_alv.
data: end of alv_stb.

types:begin of ty_out,
        matnr type marc-matnr,
        datum type datum,
        idnrk type stpox_alv-idnrk,
        ojtxp type stpox_alv-ojtxp,
        mnglg type stpox_alv-mnglg,
        mmein type stpox_alv-mmein,
      end of ty_out.
data:it_out type standard table of ty_out,
     wa_out type ty_out.

selection-screen begin of block b1 with frame title text-001.
select-options:s_werks for mast-werks default '1112' obligatory,
               s_matnr for mast-matnr.
parameters:p_datum type datum obligatory.
selection-screen end of block b1.


start-of-selection.

  if sy-batch = 'X'.

    perform frm_save_date.

  else.

    perform frm_get_date.

    perform frm_show_data.

  endif.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_get_date .
  select * into table it_mast
    from mast
   where matnr in s_matnr
     and werks in s_werks.
  loop at it_mast into wa_mast.
    submit rcs13001
      with pm_mtnrv incl wa_mast-matnr
      with pm_werks incl wa_mast-werks
      with pm_stlal incl '1'
      with pm_capid incl 'PP01'
      with pm_datuv incl p_datum
      and return.

    import alv_stb from memory id 'ZPPR014'.    " RCS13001  ---4130行
    free memory id 'ZPPR014'.

    loop at alv_stb.
      move-corresponding alv_stb to wa_out.
      wa_out-matnr = wa_mast-matnr.
      append wa_out to it_out.
      clear wa_out.
    endloop.
  endloop.
endform.
*&---------------------------------------------------------------------*
*& Form FRM_SHOW_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_show_data .
  refresh:gt_fieldcat[].
  mcr_fieldcat 'MATNR' '成品物料编号' '' '' 'X' '' '' '' '40'.
  mcr_fieldcat 'IDNRK' '组建物料' '' '' '' '' '' '' '40'.
  mcr_fieldcat 'OJTXP' '组建描述' '' '' '' '' '' '' '40'.
  mcr_fieldcat 'MNGLG' '单件用量' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MMEIN' '单位' '' '' '' '' '' '' ''.

* 给layout赋值
  clear gs_layout_lvc.
  gs_layout_lvc-cwidth_opt = abap_true.
  gs_layout_lvc-zebra      = abap_true.
  gs_layout_lvc-no_rowmark = abap_true.

  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
      i_callback_program = sy-repid
*     i_callback_pf_status_set = 'FRM_PF_STATUS_SET_ALV'
*     i_callback_user_command = 'FRM_USER_COMMAND'
      it_fieldcat_lvc    = gt_fieldcat
      is_layout_lvc      = gs_layout_lvc
      i_save             = 'A'
    tables
      t_outtab           = it_out
    exceptions
      program_error      = 1
      others             = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form FRM_SAVE_DATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_save_date .
  data:it_zmmt014_bom type standard table of zmmt014_bom,
       wa_zmmt014_bom type zmmt014_bom.

  p_datum = sy-datum - 1.
  delete from zmmt014_bom where datum < sy-datum.
  select * into table it_mast
    from mast
   where matnr in s_matnr
     and werks in s_werks.

  loop at it_mast into wa_mast.
    do 30 times.
      p_datum = p_datum + 1.
      submit rcs13001
        with pm_mtnrv incl wa_mast-matnr
        with pm_werks incl wa_mast-werks
        with pm_stlal incl '1'
        with pm_capid incl 'PP01'
        with pm_datuv incl p_datum
         and return.

      import alv_stb from memory id 'ZPPR014'.    " RCS13001  ---4130行
      free memory id 'ZPPR014'.

      clear:it_zmmt014_bom[].
      loop at alv_stb.
        move-corresponding alv_stb to wa_zmmt014_bom.
        wa_zmmt014_bom-matnr = wa_mast-matnr.
        wa_zmmt014_bom-werks = wa_mast-werks.
        wa_zmmt014_bom-datum = p_datum.
        append wa_zmmt014_bom to it_zmmt014_bom.
        clear wa_zmmt014_bom.
      endloop.

      if it_zmmt014_bom[] is not initial.
        delete from zmmt014_bom where matnr = wa_mast-matnr and werks = wa_mast-werks and datum = p_datum.
        modify zmmt014_bom from table it_zmmt014_bom[].
        commit work.
      endif.
    enddo.
  endloop.
endform.