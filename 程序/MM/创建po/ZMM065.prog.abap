*&---------------------------------------------------------------------*
*& Program ID    : ZMM065
*& Program Text  : 未清采购订单批量创建
*& Overview      : 未清采购订单批量创建
*& Created by    : HANDYXH
*& Creation Date : 2019/03/27
*&---------------------------------------------------------------------*
*& Changing No   :
*& Updated by    :
*& Update date   :
*& Changed Item Description :
*&---------------------------------------------------------------------*

REPORT zmm065.
*-----------------------------------------------------------------------
* GLOBAL DATA DECLARE
*-----------------------------------------------------------------------
TYPES:BEGIN OF ty_alv,
        num      TYPE i,
        checkbox TYPE c,
        id       LIKE icon-id,        "状态
        mess     TYPE bapi_msg.
        INCLUDE TYPE zspo_create.
TYPES   ebeln   TYPE ekko-ebeln.
TYPES END OF ty_alv.


DATA:gt_alv TYPE TABLE OF ty_alv,
     gs_alv TYPE ty_alv.

DATA:gt_data TYPE  TABLE OF zspo_create,
     gs_data TYPE zspo_create.

DATA: gt_excel TYPE TABLE OF alsmex_tabline,
      gs_excel TYPE alsmex_tabline.

DATA:gt_fcat TYPE lvc_t_fcat,
     gs_fcat TYPE lvc_s_fcat.

DATA:gs_layo TYPE lvc_s_layo.

DATA:gv_tabix TYPE sy-tabix,
     gv_index TYPE sy-index.

*&---------------------------------------------------------------------*
*&field-symbol
*&---------------------------------------------------------------------*
FIELD-SYMBOLS: <dyn_field> TYPE any.
FIELD-SYMBOLS: <fs_alv> TYPE ty_alv.

*-----------------------------------------------------------------------
* 选择屏幕
*-----------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME .
PARAMETERS p_fname TYPE rlgrap-filename MEMORY ID xls.
SELECTION-SCREEN END OF BLOCK blk1.

*-----------------------------------------------------------------------
* AT SELECTION-SCREEN ON VALUE-REQUEST
*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM fm_get_path.

*-----------------------------------------------------------------------
* START-OF-SELECTION.
*-----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM frm_input_file.

  PERFORM frm_build_table.

  PERFORM frm_fill_data.

  PERFORM frm_deal_data.

  PERFORM frm_display_data.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FM_GET_PATH
*&---------------------------------------------------------------------*
*       获取文件路径
*----------------------------------------------------------------------*
FORM fm_get_path .
  DATA: title   TYPE string VALUE '选择文件',
        ini_dir TYPE string,
        l_rc    TYPE i,
        it_tab  TYPE filetable.
  DATA: lv_filter TYPE string.
  CONCATENATE cl_gui_frontend_services=>filetype_excel
            cl_gui_frontend_services=>filetype_all INTO lv_filter.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog  "获取文件路径
    EXPORTING
      window_title            = title
      initial_directory       = ini_dir
      multiselection          = ' '
      file_filter             = lv_filter
    CHANGING
      file_table              = it_tab
      rc                      = l_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc = 0 AND l_rc = 1.
    READ TABLE it_tab INTO p_fname INDEX 1.
  ENDIF.
ENDFORM.                    "FM_GET_PATH
*&---------------------------------------------------------------------*
*&      Form  FRM_INPUT_FILE
*&---------------------------------------------------------------------*
*       读取excel 内容
*----------------------------------------------------------------------*
FORM frm_input_file .
  REFRESH:gt_excel.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'    "读取excel文件中的内容
    EXPORTING
      filename                = p_fname
      i_begin_col             = '1'
      i_begin_row             = '3'
      i_end_col               = '30' " 读取多少列
      i_end_row               = '9999' "读取多少行
    TABLES
      intern                  = gt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM. "FRM_INPUT_FILE
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_TABLE
*&---------------------------------------------------------------------*
*       创建fieldcat
*----------------------------------------------------------------------*
FORM frm_build_table .
  REFRESH gt_fcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'   "获取fieldcat
    EXPORTING
      i_structure_name       = 'ZSPO_CREATE'
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
ENDFORM.                    "FRM_BUILD_TABLE
*&---------------------------------------------------------------------*
*&      Form  FRM_ISNERT_DATA
*&---------------------------------------------------------------------*
*       excel读取入内表
*----------------------------------------------------------------------*
FORM frm_fill_data .
  DATA: lv_times TYPE i VALUE 1.
  DATA: lc_date TYPE sy-datum.
  DATA: lc_time TYPE sy-uzeit.
  LOOP AT gt_excel INTO gs_excel.  "把excel数据导入内表
    lv_times = gs_excel-col.
    READ TABLE gt_fcat INTO gs_fcat INDEX lv_times.
    ASSIGN COMPONENT gs_fcat-fieldname OF STRUCTURE gs_data TO <dyn_field>.
    IF gs_fcat-inttype = 'D'.
      CALL FUNCTION 'ZCONVERT_DATE_TO_INTERNAL'
        EXPORTING
          date_external = gs_excel-value
        IMPORTING
          date_internal = <dyn_field>.
    ELSE.
      <dyn_field> = gs_excel-value.
    ENDIF.
    AT END OF row.
      APPEND gs_data TO gt_data.
      CLEAR gs_data.
    ENDAT.
    CLEAR:gs_fcat,gs_excel.
  ENDLOOP.
ENDFORM. "FRM_ISNERT_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       展示alv
*----------------------------------------------------------------------*
FORM frm_display_data .

  PERFORM frm_build_layout."格式
  PERFORM frm_alv_output. "输出

ENDFORM.
FORM frm_build_layout .
  CLEAR gs_layo.
  gs_layo-cwidth_opt = 'X'.
  gs_layo-zebra      = 'X'.

  CLEAR gs_fcat.
  gs_fcat-fieldname = 'CHECKBOX'.
  gs_fcat-coltext = '选择框'.
  gs_fcat-edit = 'X'.
  gs_fcat-checkbox = 'X'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-fieldname = 'ID'.
  gs_fcat-coltext   = '状态'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-fieldname = 'MESS'.
  gs_fcat-coltext   = '消息'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-fieldname = 'EBELN'.
  gs_fcat-coltext   = '采购订单号'.
  APPEND gs_fcat TO gt_fcat.
ENDFORM.
FORM frm_alv_output .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'FRM_PF_STATUS'
      i_callback_user_command  = 'FRM_USER_COMMAND'
      is_layout_lvc            = gs_layo
      it_fieldcat_lvc          = gt_fcat
    TABLES
      t_outtab                 = gt_alv
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*       处理数据
*----------------------------------------------------------------------*
FORM frm_deal_data .

  DATA:lt_alv    TYPE TABLE OF ty_alv.
  DATA:lv_ebelp  TYPE ekpo-ebelp.
  DATA:lv_num    TYPE i.

*&---导入数据整理
  CLEAR lv_num.
  ADD 1 TO lv_num.
  LOOP AT gt_data INTO gs_data.
*&---初始化ALV结构/initial alv structure
    CLEAR:gs_alv.
*&---设置指示灯黄色等待导入标记/set indicate yellow light of wait
    "for import
    gs_alv-id         = '@5D@'.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = gs_data-matnr
      IMPORTING
        output       = gs_data-matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_data-vbeln
      IMPORTING
        output = gs_data-vbeln.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = gs_data-meins
        language       = sy-langu
      IMPORTING
        output         = gs_data-meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*&---结构化赋值
    MOVE-CORRESPONDING gs_data TO gs_alv.
    IF lv_ebelp < gs_alv-ebelp.
      lv_ebelp = gs_alv-ebelp.
    ELSE.
      ADD 1 TO lv_num.
    ENDIF.
    gs_alv-num = lv_num.
*&---增加到ALV显示内表
    APPEND gs_alv TO gt_alv.
  ENDLOOP.

*&---检查数据
*  IF gt_alv IS NOT INITIAL.
**&---物料组
*    CLEAR:lt_alv.
*    lt_alv = gt_alv.
*    DELETE lt_alv WHERE auart IS INITIAL.
*    SORT lt_alv BY auart.
*    DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING auart.
*
*    IF lt_alv IS NOT INITIAL.
*      SELECT auart
*      INTO TABLE @lt_tvak
*      FROM tvak FOR ALL ENTRIES IN @lt_alv
*      WHERE auart = @lt_alv-auart.
*      IF sy-subrc <> 0.
*        MESSAGE text-906 TYPE 'S' DISPLAY LIKE 'E'.
*      ENDIF.
*      SORT lt_tvak BY auart.
*    ENDIF.
*
**&---工厂
*    CLEAR:lt_alv.
*    lt_alv = gt_alv.
*    SORT lt_alv BY werks.
*    DELETE ADJACENT DUPLICATES FROM  lt_alv COMPARING werks.
*
*    IF lt_alv IS NOT INITIAL.
*      SELECT werks
*      FROM t001l
*      INTO TABLE @lt_werks
*      FOR ALL ENTRIES IN @lt_alv
*      WHERE werks = @lt_alv-werks.
*      IF sy-subrc <> 0.
*        MESSAGE text-m01 TYPE 'S' DISPLAY LIKE 'E'.
*      ENDIF.
*      SORT lt_werks BY werks.
*    ENDIF.
*
**&---销售订单
*    CLEAR:lt_alv.
*    lt_alv = gt_alv.
*    SORT lt_alv BY vgbel vgpos.
*    DELETE ADJACENT DUPLICATES FROM  lt_alv COMPARING vgbel vgpos.
*
*    IF lt_alv IS NOT INITIAL.
*      SELECT vbeln ,
*             posnr
*      FROM vbap
*      INTO TABLE @lt_vbap
*      FOR ALL ENTRIES IN @lt_alv
*      WHERE vbeln = @lt_alv-vgbel
*        AND posnr = @lt_alv-vgpos.
*      IF sy-subrc <> 0.
*        MESSAGE text-m02 TYPE 'S' DISPLAY LIKE 'E'.
*      ENDIF.
*      SORT lt_vbap BY vbeln posnr.
*    ENDIF.
*
*  ENDIF.

*&---使用完毕内表，释放内存；
*  FREE:lt_alv.
*  FREE:gt_data.
*
**&---导入前检查
*  LOOP AT gt_alv ASSIGNING <fs_alv>.
*
*    READ TABLE lt_tvak INTO DATA(ls_tavk) WITH KEY auart = <fs_alv>-auart BINARY SEARCH.
*    IF sy-subrc NE 0.
*      <fs_alv>-id    = '@5C@'.
*      CONCATENATE <fs_alv>-mess text-906 INTO <fs_alv>-mess."'销售凭证类型不存在'.
*    ENDIF.
*
*    READ TABLE lt_werks INTO DATA(ls_werks) WITH KEY werks = <fs_alv>-werks BINARY SEARCH.
*    IF sy-subrc NE 0.
*      <fs_alv>-id    = '@5C@'.
*      CONCATENATE <fs_alv>-mess text-m01 INTO <fs_alv>-mess."'工厂不存在'.
*    ENDIF.
*
*    READ TABLE lt_vbap INTO DATA(ls_vbap) WITH KEY vbeln = <fs_alv>-vgbel
*                                                   posnr = <fs_alv>-vgpos BINARY SEARCH.
*    IF sy-subrc NE 0.
*      <fs_alv>-id    = '@5C@'.
*      CONCATENATE <fs_alv>-mess text-m01 INTO <fs_alv>-mess."'工厂不存在'.
*    ENDIF.
*
*  ENDLOOP.


ENDFORM.                    " FRM_DEAL_DAT
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
*&      Form  FRM_DISPLAY                                              *
*&---------------------------------------------------------------------*
*&       ALV工具栏命令                                                 *
*&---------------------------------------------------------------------*
*&  -->  p1    text                                                    *
*&  <--  p2    text                                                    *
*&---------------------------------------------------------------------*
FORM frm_user_command USING r_ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.
*&---刷新屏幕数据到内表
  DATA: lr_grid1 TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid1.
  CALL METHOD lr_grid1->check_changed_data.

*&---按钮功能实现
  CASE r_ucomm.

    WHEN '&ZCREATE'.

      PERFORM frm_impt_data.

  ENDCASE.

*&---调用后数据保存处理
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid1.
  CALL METHOD lr_grid1->check_changed_data.
*&---刷新ALV 显示值
  rs_selfield-refresh = 'X' .
ENDFORM. "FRM_USER_COMMANDA
*&---------------------------------------------------------------------*
*&      Form  FRM_IMPT_DATA
*&---------------------------------------------------------------------*
*       生成PO
*----------------------------------------------------------------------*
FORM frm_impt_data .
  DATA: "lv_testrun TYPE bapiflag,
    lv_testrun TYPE bapiflag-bapiflag,
    lv_eblen   TYPE ekko-ebeln,
    lv_index   TYPE i,
    lv_rows    TYPE i,
    lv_shuilv  TYPE konv-kbetr,
    g_netpr    TYPE vbap-netpr.

  DATA:lv_length  TYPE i,
       lv_begin   TYPE i,
       lv_shengyu TYPE i.

  DATA:gv_error_rec   TYPE c,
       gv_success_rec TYPE c.

  DATA:lt_ftaxp TYPE TABLE OF ftaxp,
       ls_ftaxp TYPE ftaxp.

  DATA: gt_poheader      LIKE TABLE OF bapimepoheader WITH HEADER LINE,
        gt_poheaderx     LIKE TABLE OF bapimepoheaderx WITH HEADER LINE,
        gt_poitem        LIKE TABLE OF bapimepoitem WITH HEADER LINE,
        gt_poitemx       LIKE TABLE OF bapimepoitemx WITH HEADER LINE,
        gt_poschedule    LIKE TABLE OF bapimeposchedule WITH HEADER LINE,
        gt_poschedulex   LIKE TABLE OF bapimeposchedulx WITH HEADER LINE,
        gt_poaccount     LIKE TABLE OF bapimepoaccount WITH HEADER LINE,
        gt_poaccountx    LIKE TABLE OF bapimepoaccountx WITH HEADER LINE,
        gt_pocond        LIKE TABLE OF bapimepocond WITH HEADER LINE,
        gt_pocondx       LIKE TABLE OF bapimepocondx WITH HEADER LINE,
        gt_potextitem    LIKE TABLE OF bapimepotext WITH HEADER LINE,
        gt_pocomponents  LIKE TABLE OF bapimepocomponent WITH HEADER LINE,
        gt_pocomponentsx LIKE TABLE OF bapimepocomponentx WITH HEADER LINE,
        gt_extensionin   LIKE TABLE OF bapiparex WITH HEADER LINE,
        gs_te_item       LIKE bapi_te_mepoitem,
        gs_te_itemx      LIKE bapi_te_mepoitemx,
        gt_return        LIKE TABLE OF bapiret2 WITH HEADER LINE,
        gt_potext        LIKE TABLE OF bapimepotext WITH HEADER LINE.
*  IF cb_test = 'X'.
*    lv_testrun = 'X'.
*  ENDIF.

  CLEAR:  gv_error_rec,
          gv_success_rec.

  SORT gt_poitem[] BY po_item.
  SORT gt_poitemx[] BY po_item.
  SORT gt_poschedule[] BY po_item.
  SORT gt_poschedulex[] BY po_item.
  SORT gt_poaccountx[] BY po_item.

  LOOP AT gt_alv INTO gs_alv WHERE checkbox = 'X'.

    lv_index = sy-tabix.

*----->poitem, gt_poitemx

    gt_poitem-po_item    = gs_alv-ebelp."行项目
    gt_poitemx-po_item   = gs_alv-ebelp.

    IF gs_alv-matnr <> '' .
      gt_poitem-material  = gs_alv-matnr."物料
      gt_poitemx-material = 'X'.
    ENDIF.

    IF gs_alv-txz01 <> '' .
      gt_poitem-short_text  = gs_alv-txz01."短文本、物料描述
      gt_poitemx-short_text = 'X'.
    ENDIF.

    gt_poitem-quantity      = gs_alv-menge."数量
    gt_poitemx-quantity     = 'X'.

    gt_poitem-po_unit      = gs_alv-meins."单位
    gt_poitemx-po_unit = 'X'.


    gt_poitem-po_price     = '1'.            "价格采纳：1 = 总值, 2 = 净值
    gt_poitemx-po_price    = 'X'.


*    IF gs_alv-matkl IS NOT INITIAL.
*      gt_poitem-matl_group  = gs_alv-matkl."物料组
*      gt_poitemx-matl_group = 'X'.
*    ENDIF.

    IF gs_alv-werks IS NOT INITIAL.
      gt_poitem-plant        = gs_alv-werks."工厂
      gt_poitemx-plant       = 'X'.
    ENDIF.

    IF gs_alv-lgort IS NOT INITIAL.
      gt_poitem-stge_loc     = gs_alv-lgort."库存地点
      gt_poitemx-stge_loc    = 'X'.
    ENDIF.

*    IF gs_alv-bednr IS NOT INITIAL.
*      gt_poitem-trackingno   = gs_alv-bednr."需求号
*      gt_poitemx-trackingno = 'X'.
*    ENDIF.

    IF gs_alv-afnam IS NOT INITIAL.
      gt_poitem-preq_name    = gs_alv-afnam."申请者
      gt_poitemx-preq_name = 'X'.
    ENDIF.
    CLEAR lv_shuilv.
    IF gs_alv-mwskz IS NOT INITIAL.
      CALL FUNCTION 'GET_TAX_PERCENTAGE'
        EXPORTING
          aland   = 'CN'
          datab   = sy-datum
          mwskz   = gs_alv-mwskz
          txjcd   = 'TAXCN'
*         EXPORT  = ' '
        TABLES
          t_ftaxp = lt_ftaxp.
      READ TABLE lt_ftaxp INTO ls_ftaxp WITH KEY kschl = 'MWVS'.
      IF sy-subrc = 0.
        lv_shuilv = ls_ftaxp-kbetr.

      ENDIF.
      gt_poitem-tax_code     = gs_alv-mwskz."税码
      gt_poitemx-tax_code = 'X'.
    ENDIF.

    CLEAR g_netpr.
    IF lv_shuilv IS NOT INITIAL.
      g_netpr = gs_alv-netpr * ( ( lv_shuilv / 1000 ) + 1 ).
    ELSE.
      g_netpr = gs_alv-netpr * '1.13'.
    ENDIF.

    IF g_netpr IS NOT INITIAL.

      gt_poitem-net_price    = g_netpr."价格
      gt_poitemx-net_price   = 'X'.
    ENDIF.

    IF gs_alv-peinh IS NOT INITIAL.
      gt_poitem-price_unit   = gs_alv-peinh."价格单位
      gt_poitemx-price_unit  = 'X'.
    ENDIF.

    IF gs_alv-knttp <> ''.
      gt_poitem-acctasscat  = gs_alv-knttp."科目类别
      gt_poitemx-acctasscat = 'X'.
    ENDIF.

    IF gs_alv-pstyp <> ''.
      gt_poitem-item_cat   = gs_alv-pstyp."项目类别
      gt_poitemx-item_cat  = 'X'.
    ENDIF.

*    IF gs_alv-retpo <> ''.
*      TRANSLATE gs_alv-retpo TO UPPER CASE."转换大写
*      gt_poitem-ret_item   = gs_alv-retpo."退货项目
*      gt_poitemx-ret_item  = 'X'.
*    ENDIF.

*    IF gs_alv-free_item <> ''.
*      TRANSLATE gs_alv-free_item TO UPPER CASE."转换大写
*      gt_poitem-free_item  = gs_alv-free_item."免费项目
*      gt_poitemx-free_item = 'X'.
*    ENDIF.

*    IF gs_alv-anln1 <> ''.
*      TRANSLATE gs_alv-anln1 TO UPPER CASE."转换大写
*      gt_poaccount-asset_no  = gs_alv-anln1."资产编号
*      gt_poaccount-asset_no = 'X'.
*    ENDIF.

*    IF gs_alv-aufnr <> ''.
*      TRANSLATE gs_alv-aufnr TO UPPER CASE."转换大写
*      gt_poaccount-orderid = gs_alv-aufnr."内部订单
*      gt_poaccount-orderid = 'X'.
*    ENDIF.

*    IF gs_alv-kostl <> ''.
*      TRANSLATE gs_alv-kostl TO UPPER CASE."转换大写
*      gt_poaccount-costcenter  = gs_alv-kostl."成本中心
*      gt_poaccount-costcenter = 'X'.
*    ENDIF.
    APPEND gt_poitem.
    CLEAR gt_poitem.
    APPEND gt_poitemx.
    CLEAR gt_poitemx.

*----->poschedule, gt_poschedulex

    gt_poschedule-po_item          = gs_alv-ebelp."行项目
    gt_poschedulex-po_item         = gs_alv-ebelp.
    gt_poschedule-sched_line       = '1'.            "交货计划行
    gt_poschedulex-sched_line      = '1'.
    gt_poschedule-del_datcat_ext   = 'D'.            "交货日期的类别
    gt_poschedulex-del_datcat_ext  = 'X'.
    IF gs_alv-eindt <> ''."交货日期
      gt_poschedule-delivery_date    = gs_alv-eindt."交货日期
      gt_poschedulex-delivery_date   = 'X'.
    ENDIF.
    IF gs_alv-eindt <> ''.
      gt_poschedule-quantity         = gs_alv-menge."计划数量
      gt_poschedulex-quantity        = 'X'.
    ENDIF.
    APPEND gt_poschedule.
    CLEAR gt_poschedule.
    APPEND gt_poschedulex.
    CLEAR gt_poschedulex.

*----->poaccount, gt_poaccountx
    IF gs_alv-vbeln IS NOT INITIAL."销售订单
      gt_poaccount-po_item           = gs_alv-ebelp.
      gt_poaccountx-po_item          = gs_alv-ebelp.
      gt_poaccount-serial_no         = '01'.
      gt_poaccountx-serial_no        = '01'.
      gt_poaccount-gl_account        = gs_alv-sakto.
      gt_poaccountx-gl_account       = 'X'.
      gt_poaccount-sd_doc            = gs_alv-vbeln.
      gt_poaccountx-sd_doc           = 'X'.
      gt_poaccount-itm_number       = gs_alv-vbelp.
      gt_poaccountx-itm_number       = 'X'.
      APPEND gt_poaccount.
      APPEND gt_poaccountx.
    ENDIF.

    gt_pocond-cond_type    = 'PBXX'.
    gt_pocondx-cond_type   = 'X'.
    gt_pocond-change_id    = 'U'."I插入；U修改
    gt_pocondx-change_id   = 'X'.
    gt_pocond-itm_number   = gs_alv-ebelp.
    gt_pocondx-itm_number  = gs_alv-ebelp.
    gt_pocond-cond_st_no   = '001'.
    gt_pocondx-cond_st_no  = '001'.
    gt_pocond-currency     = gs_alv-waers.
    gt_pocondx-currency    = 'X'.
    gt_pocond-cond_unit    = gs_alv-meins.
    gt_pocondx-cond_unit   = 'X'.
    IF gs_alv-netpr <> ''.
      gt_pocond-cond_value   = g_netpr.
      gt_pocondx-cond_value  = 'X'.
    ENDIF.
    IF gs_alv-peinh <> ''.
      gt_pocond-cond_p_unt   = gs_alv-peinh.
      gt_pocondx-cond_p_unt  = 'X'.
    ENDIF.
    APPEND gt_pocond.
    CLEAR gt_pocond.
    APPEND gt_pocondx.
    CLEAR gt_pocondx.

    IF gs_alv-text IS NOT INITIAL.
      lv_length = strlen( gs_alv-text ).
      gt_potext-po_item =  gs_alv-ebelp. "采购凭证的项目编号
      gt_potext-text_id = 'F03'.         "物料采购单
      CLEAR:lv_length,lv_shengyu, lv_begin.
      DO.
        IF lv_length > 132.
          gt_potext-text_line = gs_alv-text+lv_begin(132)."行项目文本
          lv_length = lv_length - 132.
          lv_begin = lv_begin + 132.
          APPEND gt_potext.
        ELSE.
          lv_length = strlen( gs_alv-text ).
          lv_shengyu = lv_length - lv_begin.
          gt_potext-text_line = gs_alv-text+lv_begin(lv_shengyu).
          APPEND gt_potext.
          EXIT.
        ENDIF.
      ENDDO.

    ENDIF.

    IF gs_alv-ZEBELN IS NOT INITIAL AND gs_alv-ZEBELP IS NOT INITIAL.
      gt_potext-po_item =  gs_alv-ebelp. "采购凭证的项目编号
      gt_potext-text_id = 'F01'.         "备注
      gt_potext-text_line = 'PO' && gs_alv-ZEBELN && ' 行项目号' && gs_alv-ZEBELP.
      APPEND gt_potext.
    ENDIF.

    lv_rows = lv_rows + 1.

    AT END OF num.
      READ TABLE gt_alv INTO gs_alv INDEX lv_index.

      gt_poheader-doc_type     = gs_alv-bsart."采购订单类型
      gt_poheaderx-doc_type    = 'X'.
      gt_poheader-purch_org    = gs_alv-ekorg."采购组织
      gt_poheaderx-purch_org   = 'X'.
      gt_poheader-pur_group    = gs_alv-ekgrp."采购组
      gt_poheaderx-pur_group   = 'X'.
      gt_poheader-comp_code    = gs_alv-bukrs."公司
      gt_poheaderx-comp_code   = 'X'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_alv-lifnr
        IMPORTING
          output = gs_alv-lifnr.
      gt_poheader-vendor       = gs_alv-lifnr."供应商
      gt_poheaderx-vendor      = 'X'."供应商
      gt_poheader-currency     = gs_alv-waers.          "货币
      gt_poheaderx-currency    = 'X'.
      gt_poheader-langu        = '1'.            "语言
      gt_poheaderx-langu       = 'X'.
      gt_poheader-doc_date     = gs_alv-bedat."采购凭证日期
      gt_poheaderx-doc_date    = 'X'.
      gt_poheader-creat_date   = sy-datum.       "创建日期
      gt_poheaderx-creat_date  = 'X'.
      gt_poheader-created_by   = sy-uname.       "创建人
      gt_poheaderx-created_by  = 'X'.

      CLEAR lv_eblen.
      CALL FUNCTION 'BAPI_PO_CREATE1'
        EXPORTING
          poheader         = gt_poheader
          poheaderx        = gt_poheaderx
          testrun          = lv_testrun
        IMPORTING
          exppurchaseorder = lv_eblen
        TABLES
          return           = gt_return
          poitem           = gt_poitem
          poitemx          = gt_poitemx
          poschedule       = gt_poschedule
          poschedulex      = gt_poschedulex
          pocond           = gt_pocond
          pocondx          = gt_pocondx
          poaccount        = gt_poaccount
          poaccountx       = gt_poaccountx
          pocomponents     = gt_pocomponents
          pocomponentsx    = gt_pocomponentsx
          potextitem       = gt_potext.

      READ TABLE gt_return WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        gs_alv-id = icon_red_light.
        gs_alv-mess = gt_return-message.
        gv_error_rec = gv_error_rec + 1.
      ELSE.
*        IF lv_testrun EQ 'X'.
*          ROLLBACK WORK.
*          gs_alv-id = icon_green_light.
*          CONCATENATE  text-011 lv_eblen text-012 INTO gs_alv-mess.
*        ELSE.
        COMMIT WORK AND WAIT.
        gs_alv-ebeln = lv_eblen.
        gs_alv-id = icon_green_light.
        CONCATENATE  text-011 lv_eblen text-013 INTO gs_alv-mess.
*        ENDIF.
        gv_success_rec = gv_success_rec + 1.

      ENDIF.
      MODIFY gt_alv FROM gs_alv.


      REFRESH gt_return.

      CLEAR: gt_poheader,gt_poheaderx.
      CLEAR :   gt_poitem,
                gt_poitemx,
                gt_pocond,
                gt_pocondx,
                gt_poschedule,
                gt_poschedulex,
                gt_poaccount,
                gt_poaccountx,
                gt_pocomponents,
                gt_pocomponentsx.
      REFRESH : gt_poitem[],
                gt_poitemx[],
                gt_pocond[],
                gt_pocondx[],
                gt_poschedule[],
                gt_poschedulex[],
                gt_poaccount[],
                gt_poaccountx[],
                gt_pocomponents[],
                gt_pocomponentsx[].
    ENDAT.

    CLEAR gs_alv.
  ENDLOOP.
ENDFORM.                    " FRM_IMPT_DATA