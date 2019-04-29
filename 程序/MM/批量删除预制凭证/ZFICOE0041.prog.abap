*&---------------------------------------------------------------------*
*& Report ZFICOE0041
*&---------------------------------------------------------------------*
*& Author: HAND YXH
*& Description: 预制凭证批量删除
*& Referance: ZFICOE0041
*&---------------------------------------------------------------------*
*& Ver    Date       Author    Modify Des
*& 1.    2019/04/29  HANDYXH     New program
*&---------------------------------------------------------------------*
REPORT zficoe0041.

*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES:rbkp.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES:BEGIN OF ly_data,
        box   TYPE c,
        icon  TYPE char4,
        belnr TYPE rbkp-belnr,
        gjahr TYPE rbkp-gjahr,
        bukrs TYPE rbkp-bukrs,
        mess  TYPE bapi_msg,
      END OF ly_data.
*-----------------------------------------------------------------------
* D A T A S
*-----------------------------------------------------------------------
DATA:gt_data TYPE TABLE OF ly_data.
*alv
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv.
DATA: gs_fieldcat TYPE LINE OF slis_t_fieldcat_alv.
DATA: gs_layout  TYPE slis_layout_alv.
DATA: gs_setting TYPE lvc_s_glay.

*-----------------------------------------------------------------------
* D E F I N E S
*-----------------------------------------------------------------------
DEFINE fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = &1.
  gs_fieldcat-ref_tabname = &2.
  gs_fieldcat-ref_fieldname = &3.
  gs_fieldcat-seltext_l = &4.
  gs_fieldcat-no_zero = &5.
  gs_fieldcat-key = &6.
  APPEND gs_fieldcat TO gt_fieldcat.
END-OF-DEFINITION.

*-----------------------------------------------------------------------
* S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_belnr FOR rbkp-belnr ,
                s_gjahr FOR rbkp-gjahr DEFAULT sy-datum+0(4).
SELECTION-SCREEN END OF BLOCK blk1.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM frm_get_data.
  PERFORM frm_display_data.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_data .
  SELECT belnr
         gjahr
         bukrs
  INTO CORRESPONDING FIELDS OF TABLE gt_data
  FROM rbkp
  WHERE belnr IN s_belnr
    AND gjahr IN s_gjahr
    AND rbstat IN ( 'A' , 'B' , 'D' ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_data .

  PERFORM f_alv_fieldcat.
  PERFORM f_layout.
  PERFORM f_display.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_alv_fieldcat .

  fieldcat 'ICON'  ''     ''      '状态'     '' ''.
  fieldcat 'BELNR' 'RBKP' 'BELNR' '凭证编号' '' ''.
  fieldcat 'GJAHR' 'RBKP' 'GJAHR' '财年'     '' ''.
  fieldcat 'BUKRS' 'RBKP' 'BUKRS' '公司代码' '' ''.
  fieldcat 'MESS'  ''     ''      '消息'     '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_layout .
  gs_layout-zebra = 'X'.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-box_fieldname = 'BOX'.
  gs_setting-EDT_CLL_CB   = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_display .
*  可以保存格式
  DATA:gs_variant   TYPE disvariant.
  gs_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_save                   = 'A'         "可以保存格式
      is_variant               = gs_variant  "LAYOUT参数 可以保存格式
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      i_grid_settings          = gs_setting
      i_callback_pf_status_set = 'FRM_PF_STATUS'
      i_callback_user_command  = 'F_USER_COMMAND'
    TABLES
      t_outtab                 = gt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
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
    WHEN '&ZDELE'.
      PERFORM frm_dele_invoice.
  ENDCASE.
  rs_selfield-refresh = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY                                              *
*&---------------------------------------------------------------------*
*&       ALV工具栏状态                                                 *
*&---------------------------------------------------------------------*
*&  -->  p1    text                                                    *
*&  <--  p2    text                                                    *
*&---------------------------------------------------------------------*
FORM frm_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM. "FRM_PF_STATUS
*&---------------------------------------------------------------------*
*& Form FRM_DELE_INVOICE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM frm_dele_invoice .

  DATA:lt_return     TYPE TABLE OF bapiret2,
       ls_return     TYPE bapiret2,
       ls_incinv_fld TYPE bapi_incinv_fld.

  LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>) WHERE box = 'X'.
    CALL FUNCTION 'BAPI_INCOMINGINVOICE_DELETE'
      EXPORTING
        invoicedocnumber = <fs_data>-belnr
        fiscalyear       = <fs_data>-gjahr
      TABLES
        return           = lt_return.

    IF lt_return IS INITIAL.
      <fs_data>-icon = icon_green_light.
      <fs_data>-mess = |凭证{ <fs_data>-belnr }财年{ <fs_data>-gjahr }删除成功！|.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
      <fs_data>-icon = icon_red_light.
      LOOP AT lt_return INTO ls_return.
        <fs_data>-mess = <fs_data>-mess && '|' && ls_return-message.
      ENDLOOP.
      SHIFT <fs_data>-mess LEFT DELETING LEADING '|'.
    ENDIF.
  ENDLOOP.



ENDFORM.