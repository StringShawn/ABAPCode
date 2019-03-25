*&---------------------------------------------------------------------*
*& Program ID    : ZMM063
*& Program Text  : 批量采购申请导入
*& Overview      : 批量采购申请导入
*& Created by    : HANDYXH
*& Creation Date : 2019/03/22
*&---------------------------------------------------------------------*
*& Changing No   :
*& Updated by    :
*& Update date   :
*& Changed Item Description :
*&---------------------------------------------------------------------*

REPORT zmm063.
*-----------------------------------------------------------------------
* GLOBAL DATA DECLARE
*-----------------------------------------------------------------------
TYPES:BEGIN OF ty_alv,
        id   LIKE icon-id,        "状态
        mess TYPE bapi_msg.
        INCLUDE STRUCTURE zspr_change.
TYPES END OF ty_alv.

TYPES:BEGIN OF ty_mara,
        matnr TYPE mara-matnr,
        matkl TYPE mara-matkl,
      END OF ty_mara.

TYPES:BEGIN OF ty_makt,
        matnr TYPE makt-matnr,
        maktx TYPE makt-maktx,
      END OF ty_makt.

TYPES:BEGIN OF ty_marc,
        matnr TYPE marc-matnr,
        werks TYPE marc-werks,
        ekgrp TYPE marc-ekgrp,
      END OF ty_marc.

TYPES:BEGIN OF ty_t161,
        bstyp TYPE t161-bstyp,
        bsart TYPE t161-bsart,
        pincr TYPE t161-pincr,
        numki TYPE t161-numki,
        numke TYPE t161-numke,
      END OF ty_t161.

DATA:gt_alv TYPE TABLE OF ty_alv,
     gs_alv TYPE ty_alv.

DATA:gt_data TYPE  TABLE OF zspr_change,
     gs_data TYPE zspr_change.

DATA: gt_excel TYPE TABLE OF alsmex_tabline,
      gs_excel TYPE alsmex_tabline.

DATA:gt_fcat TYPE lvc_t_fcat,
     gs_fcat TYPE lvc_s_fcat.

DATA:gs_layo TYPE lvc_s_layo.

*&---物料描述
DATA:gt_makt TYPE TABLE OF ty_makt,
     gs_makt TYPE ty_makt.

*&---物料信息
DATA:gt_mara TYPE TABLE OF ty_mara,
     gs_mara TYPE ty_mara.

*&---物料工厂信息
DATA:gt_marc TYPE TABLE OF ty_marc,
     gs_marc TYPE ty_marc.

*&---物料工厂信息
DATA:gt_t161 TYPE TABLE OF ty_t161,
     gs_t161 TYPE ty_t161.

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

  PERFORM frm_change_pr.

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
      i_structure_name       = 'ZSPR_CHANGE'
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
  gs_fcat-fieldname = 'ID'.
  gs_fcat-coltext   = '状态'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-fieldname = 'MESS'.
  gs_fcat-coltext   = '消息'.
  APPEND gs_fcat TO gt_fcat.
ENDFORM.
FORM frm_alv_output .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
      is_layout_lvc      = gs_layo
      it_fieldcat_lvc    = gt_fcat
    TABLES
      t_outtab           = gt_alv
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHANGE_PR
*&---------------------------------------------------------------------*
*       修改pr之前检查
*----------------------------------------------------------------------*
FORM frm_change_pr .

  DATA:lv_banfn TYPE eban-banfn.
  DATA:gw_pritem TYPE bapimereqitemimp .
  DATA:gw_pritemx TYPE bapimereqitemx .
  DATA:gt_pritem TYPE TABLE OF bapimereqitemimp .
  DATA:gt_pritemx TYPE TABLE OF bapimereqitemx .
*  DATA:gw_head TYPE bapimereqheader  .
*  DATA:gw_headx TYPE bapimereqheaderx.
  DATA:gt_bapireturn TYPE TABLE OF bapiret2 .
  DATA:gw_bapireturn TYPE bapiret2 .
  DATA:lt_alv    TYPE TABLE OF ty_alv.
  DATA:lt_alv_temp TYPE TABLE OF ty_alv.
  DATA:lv_bnfpo  TYPE eban-bnfpo.
  DATA:lv_bstyp  TYPE eban-bstyp.
  DATA:err_flag.

  CLEAR:lv_banfn.
  CLEAR:lv_bnfpo.

*&---B 代表采购申请
  lv_bstyp = 'B'.

  SELECT bstyp
         bsart  "采购申请类型
         pincr  "项目编号间隔
         numki  "内部号码分配的号码范围
         numke  "外部号码分配的号码范围
  INTO TABLE gt_t161
  FROM t161
  WHERE bstyp = lv_bstyp.
  IF sy-subrc <> 0.
    MESSAGE text-906 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  SORT gt_t161 BY bsart.

*&---导入数据整理
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

*&---结构化赋值
    MOVE-CORRESPONDING gs_data TO gs_alv.
*&---增加到ALV显示内表
    APPEND gs_alv TO gt_alv.
  ENDLOOP.


*&---检查数据
  IF gt_alv IS NOT INITIAL.
*&---物料组
    CLEAR:lt_alv.
    lt_alv = gt_alv.
    DELETE lt_alv WHERE matnr IS INITIAL.
    SORT lt_alv BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING matnr.

    IF lt_alv IS NOT INITIAL.
      SELECT matnr
             matkl    "物料组
      FROM mara
      INTO TABLE gt_mara
      FOR ALL ENTRIES IN lt_alv
      WHERE matnr = lt_alv-matnr.
      IF sy-subrc <> 0.
        MESSAGE text-906 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
      SORT gt_mara BY matnr.

*&---物料描述
      SELECT matnr
             maktx
      INTO TABLE gt_makt
      FROM makt
      FOR ALL ENTRIES IN lt_alv
      WHERE matnr = lt_alv-matnr
        AND spras = sy-langu.
      IF sy-subrc <> 0.
        MESSAGE text-906 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
      SORT gt_makt BY matnr.
    ENDIF.

*&---采购组
    CLEAR:lt_alv.
    lt_alv = gt_alv.
    DELETE lt_alv  WHERE matnr IS INITIAL AND werks IS INITIAL.
    SORT lt_alv BY matnr werks.
    DELETE ADJACENT DUPLICATES FROM  lt_alv COMPARING matnr werks.

    IF lt_alv IS NOT INITIAL.
      SELECT matnr
             werks
             ekgrp    "采购组
      FROM marc
      INTO TABLE gt_marc
      FOR ALL ENTRIES IN lt_alv
      WHERE matnr = lt_alv-matnr
        AND werks = lt_alv-werks.
      IF sy-subrc <> 0.
        MESSAGE text-906 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
      SORT gt_marc BY matnr werks.
    ENDIF.
  ENDIF.


*&---使用完毕内表，释放内存；
  FREE:lt_alv.
  FREE:gt_data.


*&---导入前检查
  LOOP AT gt_alv ASSIGNING <fs_alv>.
*&---日志信息
*    <fs_alv>-ernam = sy-uname.
*    <fs_alv>-erdat = sy-datum.
*    <fs_alv>-erzzt = sy-uzeit.
*    <fs_alv>-rsrow = sy-tabix.

*&---系统预设定的行项目间隔
    READ TABLE gt_t161 INTO gs_t161 WITH KEY bsart = gs_data-bsart BINARY SEARCH.
    IF sy-subrc <> 0.
      gs_t161-pincr = 10.
    ENDIF.

*&---物料组
    READ TABLE gt_mara INTO gs_mara WITH KEY matnr = <fs_alv>-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_alv>-matkl = gs_mara-matkl.
    ELSE.
      <fs_alv>-id    = '@5C@'.
      err_flag = 'X'.
      CONCATENATE <fs_alv>-mess text-909 INTO <fs_alv>-mess."'物料不存在'.
    ENDIF.


*&---短文本
    READ TABLE gt_makt INTO gs_makt WITH KEY matnr = <fs_alv>-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_alv>-maktx = gs_makt-maktx.
    ELSE.
      <fs_alv>-id    = '@5C@'.
      err_flag = 'X'.
      CONCATENATE <fs_alv>-mess text-910 INTO <fs_alv>-mess."'物料描述不存在'.
    ENDIF.

*&---采购组
    READ TABLE gt_marc INTO gs_marc WITH KEY matnr = <fs_alv>-matnr
                                             werks = <fs_alv>-werks
                                             BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_alv>-ekgrp = gs_marc-ekgrp.
    ELSE.
      <fs_alv>-id    = '@5C@'.
      err_flag = 'X'.
      CONCATENATE <fs_alv>-mess text-908 INTO <fs_alv>-mess."'采购组不存在'.
    ENDIF.

*&---PR行项目累加10

    IF <fs_alv>-bnfpo > lv_bnfpo.
      lv_bnfpo = <fs_alv>-bnfpo.
      APPEND <fs_alv> TO lt_alv.
    ELSE.
      IF err_flag IS INITIAL.       "有错误不产生采购申请
        PERFORM frm_impt_data TABLES lt_alv.
      ENDIF.
      APPEND LINES OF lt_alv TO lt_alv_temp.
      CLEAR lt_alv.
      APPEND <fs_alv> TO lt_alv.
    ENDIF.
    AT LAST.
      IF err_flag IS INITIAL.
        PERFORM frm_impt_data TABLES lt_alv.
        APPEND LINES OF lt_alv TO lt_alv_temp.
      ENDIF.
    ENDAT.
  ENDLOOP.

  CLEAR gt_alv.

  gt_alv = lt_alv_temp.



ENDFORM.                    " FRM_CHANGE_PR
*&---------------------------------------------------------------------*
*&      Form  FRM_IMPT_DATA
*&---------------------------------------------------------------------*
*       创建采购申请
*----------------------------------------------------------------------*
FORM frm_impt_data TABLES p_alv like gt_alv.
  DATA:
    ls_header             TYPE bapimereqheader,
    ls_headerx            TYPE bapimereqheaderx,
    lt_prheadertext       TYPE STANDARD TABLE OF bapimereqheadtext,
    ls_prheadertext       TYPE bapimereqheadtext,
    lt_item               TYPE STANDARD TABLE OF bapimereqitemimp,
    ls_item               TYPE bapimereqitemimp,
    lt_itemx              TYPE STANDARD TABLE OF bapimereqitemx,
    ls_itemx              TYPE bapimereqitemx,
    lt_pritemexp          TYPE STANDARD TABLE OF bapimereqitem,
    ls_pritemexp          TYPE bapimereqitem,
    lt_pritemtext         TYPE STANDARD TABLE OF bapimereqitemtext,
    ls_pritemtext         TYPE bapimereqitemtext,
    lt_praccount          TYPE STANDARD TABLE OF bapimereqaccount,
    ls_praccount          TYPE bapimereqaccount,
    lt_praccountx         TYPE STANDARD TABLE OF bapimereqaccountx,
    ls_praccountx         TYPE bapimereqaccountx,
    lt_allversions        TYPE STANDARD TABLE OF bapimedcm_allversions,
    ls_allversions        TYPE bapimedcm_allversions,
    lt_extensionin        TYPE STANDARD TABLE OF bapiparex,
    ls_extensionin        TYPE bapiparex,
    lt_bapi_te_mereqitem  TYPE STANDARD TABLE OF bapi_te_mereqitem,
    ls_bapi_te_mereqitem  TYPE bapi_te_mereqitem,
    lt_bapi_te_mereqitemx TYPE STANDARD TABLE OF bapi_te_mereqitemx,
    ls_bapi_te_mereqitemx TYPE bapi_te_mereqitemx,
    lt_return             TYPE STANDARD TABLE OF bapiret2,
    ls_return             TYPE bapiret2.


  DATA:lt_alv TYPE TABLE OF ty_alv.

  DATA: lv_id   LIKE icon-id,        "状态
        lv_mess TYPE bapi_msg.

*&---分割文本
  DATA:
    lv_txt    TYPE string,
    lv_length TYPE n LENGTH 4,
    lv_num    TYPE n LENGTH 4,
    lv_count  TYPE n LENGTH 4,
    lv_start  TYPE n LENGTH 4.


  DATA:
    lv_banfn TYPE eban-banfn.




*&---每次计算需要导入的采购申请个数
  CLEAR:lt_alv.
  lt_alv = p_alv[].


  CLEAR:gv_tabix,gv_index.

  SORT lt_alv BY bsart  bnfpo .

  LOOP AT lt_alv INTO gs_alv.

*&---如果该条数据已经正式导入成功，则不再导入
    IF <fs_alv>-id = '@5B@'.
      CONTINUE.
    ENDIF.

    gv_index = sy-tabix.

*&---当前条目
    gv_tabix = gv_tabix + 1.




*&---填充行项目数据
    CLEAR:ls_item,ls_itemx.
    ls_item-preq_item   = gs_alv-bnfpo. "采购申请行项目
    ls_itemx-preq_item  = gs_alv-bnfpo.
    ls_itemx-preq_itemx = 'X'.


    IF gs_alv-knttp IS NOT INITIAL.
      ls_item-acctasscat  = gs_alv-knttp. "账户分配类别
      ls_itemx-acctasscat = 'X'.
    ENDIF.

    IF gs_alv-pstyp IS NOT INITIAL.
      ls_item-item_cat    = gs_alv-pstyp. "项目类别文本
      ls_itemx-item_cat   = 'X'.
    ENDIF.

    IF gs_alv-matnr IS NOT INITIAL.
      ls_item-material    = gs_alv-matnr. "物料编号
      ls_itemx-material   = 'X'.
    ENDIF.

    IF gs_alv-menge IS NOT INITIAL.
      ls_item-quantity    = gs_alv-menge. "数量
      ls_itemx-quantity   = 'X'.
    ENDIF.

    IF gs_alv-meins IS NOT INITIAL.
      ls_item-unit        = gs_alv-meins. "申请单位（基本单位）
      ls_itemx-unit       = 'X'.
    ENDIF.

    IF gs_alv-lfdat IS NOT INITIAL.
      ls_item-deliv_date  = gs_alv-lfdat. "交货日期
      ls_itemx-deliv_date = 'X'.
    ENDIF.

    IF gs_alv-werks IS NOT INITIAL .
      ls_item-plant       = gs_alv-werks. "工厂
      ls_itemx-plant      = 'X'.
    ENDIF.

*    IF gs_alv-ekorg IS NOT INITIAL.
*      ls_item-purch_org   = gs_alv-ekorg. "采购组织
*      ls_itemx-purch_org  = 'X'.
*    ENDIF.

    IF gs_alv-afnam IS NOT INITIAL.
      ls_item-preq_name   = gs_alv-afnam. "申请者
      ls_itemx-preq_name  = 'X'.
    ENDIF.

*    IF gs_alv-badat IS NOT INITIAL.
*      ls_item-preq_date   = gs_alv-badat. "请求日期
*      ls_itemx-preq_date  = 'X'.
*    ENDIF.

*    IF gs_alv-flief IS NOT INITIAL.
*      ls_item-fixed_vend   = gs_alv-flief. "固定供应商
*      ls_itemx-fixed_vend  = 'X'.
*    ENDIF.

    IF gs_alv-preis IS NOT INITIAL.
      ls_item-preq_price  = gs_alv-preis. "评估价格
      ls_itemx-preq_price = 'X'.
    ENDIF.

    IF gs_alv-peinh IS NOT INITIAL.
      ls_item-price_unit  = gs_alv-peinh. "价格单位
      ls_itemx-price_unit = 'X'.
    ENDIF.

    IF gs_alv-waers IS NOT INITIAL.
      ls_item-currency    = gs_alv-waers. "币别
      ls_itemx-currency   = 'X'.
    ENDIF.

*    IF gs_alv-fistl IS NOT INITIAL.
*      ls_item-funds_ctr   = gs_alv-fistl. "基金中心
*      ls_itemx-funds_ctr  = 'X'.
*    ENDIF.

*    IF gs_alv-fipos IS NOT INITIAL .
*      ls_item-cmmt_item   = gs_alv-fipos. "承诺项目
*      ls_itemx-cmmt_item  = 'X'.
*    ENDIF.

    IF gs_alv-txz01 IS NOT INITIAL.
      ls_item-short_text  = gs_alv-txz01. "短文本
      ls_itemx-short_text = 'X'.
    ENDIF.

    IF gs_alv-ekgrp IS NOT INITIAL.
      ls_item-pur_group   = gs_alv-ekgrp. "采购组
      ls_itemx-pur_group  = 'X'.
    ENDIF.

    IF gs_alv-matkl IS NOT INITIAL.
      ls_item-matl_group  = gs_alv-matkl. "物料组
      ls_itemx-matl_group = 'X'.
    ENDIF.

*&---审批策略
*    IF gs_alv-bednr IS NOT INITIAL.
*      ls_item-trackingno  = 'X'. "需求跟踪号
*      ls_itemx-trackingno = 'X'.
*    ENDIF.

    IF gs_alv-lgort IS NOT INITIAL.
      ls_item-store_loc  = gs_alv-lgort. "库存地点
      ls_itemx-store_loc = 'X'.
    ENDIF.


    APPEND ls_item  TO lt_item.
    APPEND ls_itemx TO lt_itemx.


    CLEAR:ls_praccount, ls_praccountx.

    IF gs_alv-kostl IS NOT INITIAL.
      CONDENSE gs_alv-kostl.
      ls_praccount-costcenter   =  gs_alv-kostl."成本中心
      ls_praccountx-costcenter  = 'X'.
    ENDIF.

*    IF gs_alv-fipos IS NOT INITIAL.
*      ls_praccount-cmmt_item         =  gs_alv-fipos."承诺项目
*      ls_praccountx-cmmt_item        = 'X'.
*      ls_praccount-cmmt_item_long    =  gs_alv-fipos."承诺项目
*      ls_praccountx-cmmt_item_long   = 'X'.
*    ENDIF.


*    IF gs_alv-fistl IS NOT INITIAL.
*      ls_praccount-funds_ctr   = gs_alv-fistl. "基金中心
*      ls_praccountx-funds_ctr  = 'X'.
*    ENDIF.


*    IF gs_alv-wbs_ele IS NOT INITIAL.
*      ls_praccount-wbs_element   = gs_alv-wbs_ele."WBS元素
*      ls_praccountx-wbs_element  = 'X'.
*    ENDIF.

    IF ls_praccount IS NOT INITIAL.
      ls_praccount-preq_item    =  gs_alv-bnfpo. "采购申请行项目
      ls_praccount-serial_no    = '01'.

      ls_praccountx-preq_item   = gs_alv-bnfpo. "采购申请行项目
      ls_praccountx-serial_no   = '01'.

      APPEND  ls_praccount  TO lt_praccount.
      APPEND  ls_praccountx TO lt_praccountx.
    ENDIF.


*&---行项目文本处理
*&---每132字符进行分割
*    IF gs_alv-item_t01 IS NOT INITIAL."抬头文本
*      CLEAR:lv_length,lv_num,lv_count,lv_start,lv_txt.
*      CONDENSE gs_alv-item_t01.
*      lv_length = strlen( gs_alv-item_t01 ).
*      lv_num    = lv_length DIV 132.    "每132字符分割，需要分割多少整数次
*      DO lv_num TIMES.
*        lv_length = lv_length - 132.
*        lv_count  = lv_count + 1.      "
*        lv_start  = ( lv_count - 1 ) * 132 .
*        CLEAR lv_txt.
*        lv_txt = gs_alv-item_t01+lv_start(132).
*        CLEAR ls_pritemtext.
*
*        ls_pritemtext-preq_item = gs_alv-bnfpo. "行项目号
*        ls_pritemtext-text_id   = 'B01'. "
*        ls_pritemtext-text_form = '*'. "
*        ls_pritemtext-text_line = lv_txt. "
*        APPEND ls_pritemtext TO lt_pritemtext.
*      ENDDO.
**&---分割剩下的字符写人内表
*      IF lv_length > 0.
*        IF lv_num > 0."最初的字符长度大于132
*          lv_start = lv_start + 132.
*        ELSE.
*          CLEAR lv_start.
*        ENDIF.
*        CLEAR lv_txt.
*        lv_txt = gs_alv-item_t01+lv_start(lv_length).
*        CLEAR ls_pritemtext.
**&---外部给号
*        IF rb_04 = 'X' .
*          ls_pritemtext-preq_no   = gs_alv-banfn. "采购申请编号
*        ENDIF.
*        ls_pritemtext-preq_item = gs_alv-bnfpo. "行项目号
*        ls_pritemtext-text_id   = 'B01'. "
*        ls_pritemtext-text_form = '*'. "
*        ls_pritemtext-text_line = lv_txt. "
*        APPEND ls_pritemtext TO lt_pritemtext.
*      ENDIF.
*    ENDIF.


    AT LAST.

      READ TABLE lt_alv INTO gs_alv INDEX gv_index.

*&---填充抬头数据
      CLEAR:ls_header,ls_headerx.
      CLEAR:lt_prheadertext.

      ls_header-pr_type  = gs_alv-bsart. "订单类型
      ls_headerx-pr_type = 'X'.



*&---抬头文本
*&---每132字符进行分割
*      IF gs_alv-head_t01 IS NOT INITIAL."抬头文本
*        CLEAR:lv_length,lv_num,lv_count,lv_start,lv_txt.
*        CONDENSE gs_alv-head_t01.
*        lv_length = strlen( gs_alv-head_t01 ).
*        lv_num    = lv_length DIV 132.    "每132字符分割，需要分割多少整数次
*        DO lv_num TIMES.
*          lv_length = lv_length - 132.
*          lv_count  = lv_count + 1.      "
*          lv_start  = ( lv_count - 1 ) * 132 .
*          CLEAR lv_txt.
*          lv_txt = gs_alv-head_t01+lv_start(132).
*          CLEAR ls_prheadertext.
*
*          ls_prheadertext-text_id   = 'B01'. "
*          ls_prheadertext-text_form = '*'. "
*          ls_prheadertext-text_line = lv_txt. "
*          APPEND ls_prheadertext TO lt_prheadertext.
*        ENDDO.
**&---分割剩下的字符写人内表
*        IF lv_length > 0.
*          IF lv_num > 0."最初的字符长度大于132
*            lv_start = lv_start + 132.
*          ELSE.
*            CLEAR lv_start.
*          ENDIF.
*          CLEAR lv_txt.
*          lv_txt = gs_alv-head_t01+lv_start(lv_length).
*          CLEAR ls_prheadertext.
*
*          ls_prheadertext-text_id   = 'B01'. "
*          ls_prheadertext-text_form = '*'. "
*          ls_prheadertext-text_line = lv_txt. "
*          APPEND ls_prheadertext TO lt_prheadertext.
*        ENDIF.
*      ENDIF.


*&---调用BAPI:BAPI_PR_CREATE

      CLEAR:lv_banfn.
      CALL FUNCTION 'BAPI_PR_CREATE'
        EXPORTING
          prheader     = ls_header
          prheaderx    = ls_headerx
*         testrun      = gv_test
        IMPORTING
          number       = lv_banfn
*         PRHEADEREXP  =
        TABLES
          return       = lt_return[]
          pritem       = lt_item[]
          pritemx      = lt_itemx[]
          praccount    = lt_praccount[]
          praccountx   = lt_praccountx[]
          pritemtext   = lt_pritemtext[]
          prheadertext = lt_prheadertext[]
          extensionin  = lt_extensionin[].



      LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        lv_id   = '@5B@'.
        lv_mess = '导入成功，凭证号为' && lv_banfn.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        lv_id   = '@5C@'.

        LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
          CONCATENATE gs_alv-mess ls_return-message INTO lv_mess.
        ENDLOOP.

      ENDIF.


      CLEAR:ls_header,ls_headerx,
            lt_item,lt_itemx,
            lt_praccount,
            lt_prheadertext,
            lt_pritemtext,
            lt_extensionin,
            lt_return
            .
    ENDAT.

  ENDLOOP.
  LOOP AT p_alv.
    p_alv-id = lv_id.
    p_alv-mess = lv_mess.
    MODIFY p_alv TRANSPORTING id mess.
    CLEAR p_alv.
  ENDLOOP.
  FREE lt_alv.

ENDFORM.                    " FRM_IMPT_DATA