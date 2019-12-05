**************************************************
*程序名称:销售明细报表
*创建日期: 2019-11-13
*创建者:XXX
*申请者:XXX
*功能描述:
*============================================
*变更记录
*修改请求号    修改日期    修改人   修改描述
*DEVK911985    2019-11-13   HANDYXH    创建程序
***************************************************

REPORT zshxjfi0001 MESSAGE-ID zfishxj01.

*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES:mseg,mkpf.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES: BEGIN OF ty_alv,
         bukrs      TYPE t001-bukrs,
         werks      TYPE t001w-werks,
         werks_name TYPE t001w-name1,
         matnr      TYPE mseg-matnr,
         maktx      TYPE makt-maktx,
         budat      TYPE mkpf-budat,
         menge      TYPE mseg-menge,
         meins      TYPE mseg-meins,
         bklas      TYPE mbew-bklas,
         bkbez      TYPE v025-bkbez,
         kunnr      TYPE mseg-kunnr,
         kunnr_name TYPE kna1-name1,
         xblnr      TYPE mkpf-xblnr,
         grund      TYPE mseg-grund,
         ablad      TYPE mseg-ablad,
         sgtxt      TYPE mseg-sgtxt,
         dmbtr      TYPE mseg-dmbtr,
         unit_cost  TYPE mseg-dmbtr,
         mblnr      TYPE mseg-mblnr,
         mjahr      TYPE mkpf-mjahr,
         zeile      TYPE mseg-zeile,
         belnr      TYPE bkpf-belnr,
         gjahr      TYPE bseg-gjahr,
         bukrs_bseg TYPE bseg-bukrs,
         grtxt      TYPE t157e-grtxt,
       END OF ty_alv.

TYPES:BEGIN OF ty_mkpf,
        mblnr TYPE mkpf-mblnr,
        mjahr TYPE mkpf-mjahr,
        xblnr TYPE mkpf-xblnr,
        budat TYPE mkpf-budat,
        matnr TYPE mseg-matnr,
        zeile TYPE mseg-zeile,
        dmbtr TYPE mseg-dmbtr,
        shkzg TYPE mseg-shkzg,
        sgtxt TYPE mseg-sgtxt,
        menge TYPE mseg-menge,
        meins TYPE mseg-meins,
        kunnr TYPE mseg-kunnr,
        grund TYPE mseg-grund,
        ablad TYPE mseg-ablad,
        werks TYPE mseg-werks,
        bwart TYPE mseg-bwart,
      END OF ty_mkpf.

TYPES:BEGIN OF ty_t001w,
        werks TYPE t001w-werks,
        name1 TYPE t001w-name1,
      END OF ty_t001w.

TYPES:BEGIN OF ty_mbew,
        bwkey TYPE mbew-bwkey,
        matnr TYPE mbew-matnr,
        bklas TYPE mbew-bklas,
      END OF ty_mbew.

TYPES:BEGIN OF ty_kna1,
        kunnr TYPE kna1-kunnr,
        name1 TYPE kna1-name1,
      END OF ty_kna1.

TYPES: BEGIN OF ty_v025,
         bklas TYPE v025-bklas,
         bkbez TYPE v025-bkbez,
       END OF ty_v025.

TYPES: BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END OF ty_makt.

TYPES: BEGIN OF ty_t157e,
         bwart TYPE t157e-bwart,
         grund TYPE t157e-grund,
         grtxt TYPE t157e-grtxt,
       END OF ty_t157e.
*-----------------------------------------------------------------------
* D A T A S
*-----------------------------------------------------------------------
DATA:    gt_alv   TYPE TABLE OF ty_alv,
         gt_mkpf  TYPE TABLE OF ty_mkpf,
         gt_t001w TYPE TABLE OF ty_t001w,
         gt_mbew  TYPE TABLE OF ty_mbew,
         gt_v025  TYPE TABLE OF ty_v025,
         gt_kna1  TYPE TABLE OF ty_kna1,
         gt_makt  TYPE TABLE OF ty_makt,
         gt_t157e TYPE TABLE OF ty_t157e.

DATA:gw_alv   TYPE ty_alv,
     gw_mkpf  TYPE ty_mkpf,
     gw_t001w TYPE ty_t001w,
     gw_mbew  TYPE ty_mbew,
     gw_v025  TYPE ty_v025,
     gw_kna1  TYPE ty_kna1,
     gw_makt  TYPE ty_makt,
     gw_t157e TYPE ty_t157e.

*-----------------------------------------------------------------------
* ALV D A T A S
*-----------------------------------------------------------------------
DATA:gt_fcat TYPE lvc_t_fcat,
     gw_fcat TYPE lvc_s_fcat,
     gw_layo TYPE lvc_s_layo.

*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS:c_default_bukrs TYPE t001-bukrs VALUE '6100',
          c_default_werks TYPE t001w-werks VALUE '6110'.

*--------------------------------------------------------------------*
*定义选择屏幕参数
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.

PARAMETERS:p_bukrs TYPE t001-bukrs  OBLIGATORY DEFAULT c_default_bukrs,
           p_werks TYPE t001w-werks OBLIGATORY DEFAULT c_default_werks.

SELECT-OPTIONS:s_budat FOR mkpf-budat,
               s_matnr FOR mseg-matnr.

SELECTION-SCREEN END OF BLOCK blk1.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM get_data.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       根据条件取数
*----------------------------------------------------------------------*
FORM get_data .

  "取物料凭证
  SELECT mkpf~mblnr
         mkpf~mjahr
         mkpf~xblnr
         mkpf~budat
         mseg~matnr
         mseg~zeile
         mseg~dmbtr
         mseg~shkzg
         mseg~sgtxt
         mseg~menge
         mseg~meins
         mseg~kunnr
         mseg~grund
         mseg~ablad
         mseg~werks
         mseg~bwart
  INTO CORRESPONDING FIELDS OF TABLE gt_mkpf
  FROM mkpf INNER JOIN mseg
    ON mkpf~mblnr = mseg~mblnr
   AND mkpf~mjahr = mseg~mjahr
  WHERE budat IN s_budat
    AND werks =  p_werks
    AND matnr IN s_matnr
    AND bwart IN ('Z63' , 'Z64').

  IF gt_mkpf IS INITIAL.
    MESSAGE s001 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  "取工厂描述
  SELECT werks
         name1
  INTO TABLE gt_t001w
  FROM t001w FOR ALL ENTRIES IN gt_mkpf
  WHERE werks = gt_mkpf-werks.
  SORT gt_t001w BY werks.

  "取评估类和评估名称
  SELECT bwkey
         matnr
         bklas
  INTO TABLE gt_mbew
  FROM mbew FOR ALL ENTRIES IN gt_mkpf
  WHERE bwkey = gt_mkpf-werks
    AND matnr = gt_mkpf-matnr.
  SORT gt_mbew BY bwkey matnr.

  "取评估类描述
  IF gt_mbew IS NOT INITIAL.
    SELECT bklas
           bkbez
    INTO TABLE gt_v025
    FROM t025t FOR ALL ENTRIES IN gt_mbew
    WHERE bklas = gt_mbew-bklas
      AND spras = sy-langu.
    SORT gt_v025 BY bklas.
  ENDIF.

  "取客户描述
  SELECT kunnr
         name1
  INTO TABLE gt_kna1
  FROM kna1 FOR ALL ENTRIES IN gt_mkpf
  WHERE kunnr = gt_mkpf-kunnr.
  SORT gt_kna1 BY kunnr.

  "取物料描述
  SELECT matnr
         maktx
  INTO TABLE gt_makt
  FROM makt FOR ALL ENTRIES IN gt_mkpf
  WHERE matnr = gt_mkpf-matnr.
  SORT gt_makt BY matnr.

  "取销售类型名称
  SELECT bwart
         grund
         grtxt
  INTO TABLE gt_t157e
  FROM t157e FOR ALL ENTRIES IN gt_mkpf
  WHERE bwart = gt_mkpf-bwart
    AND grund = gt_mkpf-grund
    AND spras = sy-langu.
  SORT gt_t157e BY bwart grund.


  DATA:lv_awkey TYPE bkpf-awkey.

  LOOP AT gt_mkpf INTO gw_mkpf.
    MOVE-CORRESPONDING gw_mkpf TO gw_alv.

    CONCATENATE gw_alv-mblnr gw_alv-mjahr INTO lv_awkey.

    SELECT SINGLE belnr gjahr bukrs INTO ( gw_alv-belnr , gw_alv-gjahr , gw_alv-bukrs_bseg )
       FROM bkpf WHERE awkey = lv_awkey AND bukrs = p_bukrs.
    IF sy-subrc NE 0.
      CLEAR:gw_alv,gw_mkpf.
      CONTINUE.
    ENDIF.

    IF gw_mkpf-shkzg = 'H'.
      gw_alv-menge = - gw_mkpf-menge.
      gw_alv-dmbtr = - gw_mkpf-dmbtr.
    ENDIF.

    READ TABLE gt_t001w INTO gw_t001w WITH KEY werks = gw_mkpf-werks BINARY SEARCH.
    IF sy-subrc = 0.
      gw_alv-werks_name = gw_t001w-name1.
    ENDIF.

    READ TABLE gt_mbew INTO gw_mbew WITH KEY bwkey = gw_mkpf-werks matnr = gw_mkpf-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      gw_alv-bklas = gw_mbew-bklas.
    ENDIF.

    READ TABLE gt_v025 INTO gw_v025 WITH KEY bklas = gw_alv-bklas BINARY SEARCH.
    IF sy-subrc = 0.
      gw_alv-bkbez = gw_v025-bkbez.
    ENDIF.

    READ TABLE gt_kna1 INTO gw_kna1 WITH KEY kunnr = gw_mkpf-kunnr BINARY SEARCH.
    IF sy-subrc = 0.
      gw_alv-kunnr_name = gw_kna1-name1.
    ENDIF.

    READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mkpf-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      gw_alv-maktx = gw_makt-maktx.
    ENDIF.

    READ TABLE gt_t157e INTO gw_t157e WITH KEY bwart = gw_mkpf-bwart grund = gw_mkpf-grund BINARY SEARCH.
    IF sy-subrc = 0.
      gw_alv-grtxt = gw_t157e-grtxt.
    ENDIF.

    gw_alv-unit_cost  = gw_alv-dmbtr / gw_alv-menge.
    gw_alv-bukrs = p_bukrs.

    APPEND gw_alv TO gt_alv.
    CLEAR:gw_alv,gw_mkpf.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       展示数据
*----------------------------------------------------------------------*
FORM display_data .
  PERFORM build_layout.
  PERFORM build_fieldcatlog.
  PERFORM display_alv.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       构建格式
*----------------------------------------------------------------------*
FORM build_layout .
  gw_layo-zebra        = 'X'.
  gw_layo-cwidth_opt   = 'X' .       " 自动优化列宽,根据字段值显示调节
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATLOG
*&---------------------------------------------------------------------*
*       构建字段
*----------------------------------------------------------------------*
FORM build_fieldcatlog .
  CLEAR:gt_fcat.

  PERFORM add_fieldcat USING:'BUKRS' '公司代码' '' '',"公司代码
  'WERKS' '工厂' '' '',"工厂
  'WERKS_NAME' '工厂名称' '' '',"工厂名称
  'MATNR' '物料' 'MARA' 'MATNR',"物料
  'MAKTX' '物品名称' '' '',"物品名称
  'BUDAT' '出库日期' '' '',"出库日期
  'MENGE' '数量' '' '',"数量
  'MEINS' '单位' '' '',"单位
  'BKLAS' '评估类' '' '',"评估类
  'BKBEZ' '评估类名称' '' '',"评估类名称
  'KUNNR' '客户编号' '' '',"客户编号
  'KUNNR_NAME' '客户名称' '' '',"客户名称
  'XBLNR' '销售员' '' '',"销售员
  'GRUND' '销售类型' '' '',"销售类型
  'ABLAD' '税率' '' '',"税率
  'SGTXT' '销售价格' '' '',"销售价格
  'DMBTR' '出库金额' '' '',"出库金额
  'UNIT_COST ' '单位成本' '' '',"单位成本
  'MBLNR' '物料凭证' '' '',"物料凭证
  'ZEILE' '物料凭证行项目' '' '',"物料凭证行项目
  'BELNR' '会计凭证' '' ''."会计凭证



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       展示alv
*----------------------------------------------------------------------*
FORM display_alv .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program      = sy-repid
*     i_callback_pf_status_set    = l_status
      i_callback_user_command = 'ALV_USER_COMMAND'
*     i_callback_html_top_of_page = l_top_of_page
      is_layout_lvc           = gw_layo
      it_fieldcat_lvc         = gt_fcat
*     it_sort_lvc             = gt_sort
*     it_filter_lvc           = gt_filt
    TABLES
      t_outtab                = gt_alv
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDCAT
*&---------------------------------------------------------------------*
*       增加字段
*----------------------------------------------------------------------*
FORM add_fieldcat  USING    VALUE(p_fieldname)
                            VALUE(p_coltext)
                            VALUE(p_ref_table)
                            VALUE(p_ref_field).
  CLEAR gw_fcat.
  gw_fcat-fieldname = p_fieldname.
  gw_fcat-coltext   = p_coltext.
  gw_fcat-ref_table = p_ref_table.
  gw_fcat-ref_field = p_ref_field.
  APPEND gw_fcat TO gt_fcat.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_user_command USING r_ucomm TYPE sy-ucomm
                            ps_selfield TYPE slis_selfield.

  IF r_ucomm EQ '&IC1'.
    PERFORM alv_handle_double_click USING ps_selfield.
  ENDIF.

  ps_selfield-refresh = abap_true.

ENDFORM. "ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  ALV_HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PS_SELFIELD  text
*----------------------------------------------------------------------*
FORM alv_handle_double_click  USING    ps_selfield TYPE slis_selfield.
  CASE ps_selfield-fieldname.
    WHEN 'MBLNR'.
      READ TABLE gt_alv INTO DATA(ls_alv) INDEX ps_selfield-tabindex.
      PERFORM frm_migo_dialog USING ls_alv-mblnr ls_alv-mjahr.
    WHEN 'BELNR'.
      DATA: lv_belnr TYPE bseg-belnr.
      CLEAR lv_belnr.
      lv_belnr = ps_selfield-value.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_belnr
        IMPORTING
          output = lv_belnr.
      READ TABLE gt_alv INTO ls_alv INDEX ps_selfield-tabindex.
      SET PARAMETER ID 'BLN' FIELD ls_alv-belnr.
      SET PARAMETER ID 'BUK' FIELD ls_alv-bukrs.
      SET PARAMETER ID 'GJR' FIELD ls_alv-gjahr.
      CALL TRANSACTION 'FB03' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN .
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
FORM frm_migo_dialog USING lv_mblnr TYPE mblnr
                           lv_mjahr TYPE mjahr.
*                           lv_zeile TYPE mblpo.
  DATA:lv_zeile TYPE mblpo.
  DATA: lv_action            TYPE  goaction,
        lv_refdoc            TYPE  refdoc,
        lv_notree            TYPE  char1,
        lv_no_auth_check     TYPE  char1,
        lv_skip_first_screen TYPE  char1,
        lv_deadend           TYPE  char1,
        lv_okcode            TYPE  okcode,
        lv_leave_after_post  TYPE  char1,
        lv_new_rollarea      TYPE  char1,
        lv_sytcode           TYPE  sytcode,
        lv_ebeln             TYPE  ebeln,
        lv_ebelp             TYPE  ebelp,
        lv_transport         TYPE  tknum,
        lv_order_number      TYPE  aufnr,
        lv_order_item        TYPE  co_posnr,
        lv_transport_means   TYPE  traty,
        lv_transportident    TYPE  traid,
        lv_inbound_deliv     TYPE  vbeln_vl,
        lv_outbound_deliv    TYPE  vbeln,
        lv_reservation_numb  TYPE  rsnum,
        lv_reservation_item  TYPE  rspos,
        lw_ext               TYPE  ext_migo_dialog,
        lv_move_type         TYPE  bwart,
        lv_spec_stock        TYPE  sobkz,
        lv_pstng_date        TYPE  budat,
        lv_doc_date          TYPE  bldat,
        lv_ref_doc_no        TYPE  xblnr,
        lv_header_txt        TYPE  bktxt.

  SET PARAMETER ID 'MBN' FIELD lv_mblnr.
  SET PARAMETER ID 'MJA' FIELD lv_mjahr.
*  SET PARAMETER ID 'POS' FIELD lv_zeile.

  lv_action                  = 'A04'.
  lv_refdoc                  = 'R02'.
  lv_notree                  = 'X'.
  lv_skip_first_screen       = 'X'.
  lv_deadend                 = 'X'.
  lv_okcode                  = 'OK_GO'.
  lv_new_rollarea            = 'X'.

  CALL FUNCTION 'MIGO_DIALOG'
    EXPORTING
      i_action            = lv_action
      i_refdoc            = lv_refdoc
      i_notree            = lv_notree
      i_no_auth_check     = lv_no_auth_check
      i_skip_first_screen = lv_skip_first_screen
      i_deadend           = lv_deadend
      i_okcode            = lv_okcode
      i_leave_after_post  = lv_leave_after_post
      i_new_rollarea      = lv_new_rollarea
      i_sytcode           = lv_sytcode
      i_ebeln             = lv_ebeln
      i_ebelp             = lv_ebelp
      i_mblnr             = lv_mblnr
      i_mjahr             = lv_mjahr
      i_zeile             = lv_zeile
      i_transport         = lv_transport
      i_order_number      = lv_order_number
      i_order_item        = lv_order_item
      i_transport_means   = lv_transport_means
      i_transportident    = lv_transportident
      i_inbound_deliv     = lv_inbound_deliv
      i_outbound_deliv    = lv_outbound_deliv
      i_reservation_numb  = lv_reservation_numb
      i_reservation_item  = lv_reservation_item
      ext                 = lw_ext
      i_move_type         = lv_move_type
      i_spec_stock        = lv_spec_stock
      i_pstng_date        = lv_pstng_date
      i_doc_date          = lv_doc_date
      i_ref_doc_no        = lv_ref_doc_no
      i_header_txt        = lv_header_txt
    EXCEPTIONS
      illegal_combination = 1
      OTHERS              = 2.

ENDFORM.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
