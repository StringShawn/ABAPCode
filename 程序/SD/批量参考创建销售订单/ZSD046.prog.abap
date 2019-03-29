*&---------------------------------------------------------------------*
*& Program ID    : ZSD046
*& Program Text  : 批量参考创建销售订单
*& Overview      : 批量参考创建销售订单
*& Created by    : HANDYXH
*& Creation Date : 2019/03/25
*&---------------------------------------------------------------------*
*& Changing No   :
*& Updated by    :
*& Update date   :
*& Changed Item Description :
*&---------------------------------------------------------------------*

REPORT zsd046.
*-----------------------------------------------------------------------
* GLOBAL DATA DECLARE
*-----------------------------------------------------------------------
TYPES:BEGIN OF ty_alv.

        INCLUDE STRUCTURE zsref_so_create.
TYPES:  checkbox TYPE c,
        id       LIKE icon-id,        "状态
        mess     TYPE bapi_msg,
        vbeln    TYPE vbak-vbeln.
TYPES END OF ty_alv.


DATA:gt_alv TYPE TABLE OF ty_alv,
     gs_alv TYPE ty_alv.

DATA:gt_data TYPE  TABLE OF zsref_so_create,
     gs_data TYPE zsref_so_create.

DATA: gt_excel TYPE TABLE OF alsmex_tabline,
      gs_excel TYPE alsmex_tabline.

DATA:gt_fcat TYPE lvc_t_fcat,
     gs_fcat TYPE lvc_s_fcat.

DATA:gs_layo TYPE lvc_s_layo.

DATA:gv_tabix TYPE sy-tabix,
     gv_index TYPE sy-index.

DATA:   BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
*       error session opened (' ' or 'X')
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
      i_begin_row             = '2'
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
      i_structure_name       = 'ZSREF_SO_CREATE'
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
  gs_fcat-fieldname = 'VBELN'.
  gs_fcat-coltext   = '销售订单号'.
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
  DATA:BEGIN OF lt_werks OCCURS 0,
         werks TYPE t001w-werks,
       END OF lt_werks.

  DATA:BEGIN OF lt_tvak OCCURS 0,
         auart TYPE tvak-auart,
       END OF lt_tvak.

  DATA:BEGIN OF lt_vbap OCCURS 0,
         vbeln TYPE vbap-vbeln,
         posnr TYPE vbap-posnr,
       END OF lt_vbap.

*&---导入数据整理
  LOOP AT gt_data INTO gs_data.
*&---初始化ALV结构/initial alv structure
    CLEAR:gs_alv.
*&---设置指示灯黄色等待导入标记/set indicate yellow light of wait
    "for import
    gs_alv-id         = '@5D@'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_data-vgbel
      IMPORTING
        output = gs_data-vgbel.

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
    DELETE lt_alv WHERE auart IS INITIAL.
    SORT lt_alv BY auart.
    DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING auart.

    IF lt_alv IS NOT INITIAL.
      SELECT auart
      INTO TABLE @lt_tvak
      FROM tvak FOR ALL ENTRIES IN @lt_alv
      WHERE auart = @lt_alv-auart.
      IF sy-subrc <> 0.
        MESSAGE text-906 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
      SORT lt_tvak BY auart.
    ENDIF.

*&---工厂
    CLEAR:lt_alv.
    lt_alv = gt_alv.
    SORT lt_alv BY werks.
    DELETE ADJACENT DUPLICATES FROM  lt_alv COMPARING werks.

    IF lt_alv IS NOT INITIAL.
      SELECT werks
      FROM t001l
      INTO TABLE @lt_werks
      FOR ALL ENTRIES IN @lt_alv
      WHERE werks = @lt_alv-werks.
      IF sy-subrc <> 0.
        MESSAGE text-m01 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
      SORT lt_werks BY werks.
    ENDIF.

*&---销售订单
    CLEAR:lt_alv.
    lt_alv = gt_alv.
    SORT lt_alv BY vgbel vgpos.
    DELETE ADJACENT DUPLICATES FROM  lt_alv COMPARING vgbel vgpos.

    IF lt_alv IS NOT INITIAL.
      SELECT vbeln ,
             posnr
      FROM vbap
      INTO TABLE @lt_vbap
      FOR ALL ENTRIES IN @lt_alv
      WHERE vbeln = @lt_alv-vgbel
        AND posnr = @lt_alv-vgpos.
      IF sy-subrc <> 0.
        MESSAGE text-m02 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
      SORT lt_vbap BY vbeln posnr.
    ENDIF.

  ENDIF.

*&---使用完毕内表，释放内存；
  FREE:lt_alv.
  FREE:gt_data.

*&---导入前检查
  LOOP AT gt_alv ASSIGNING <fs_alv>.

    READ TABLE lt_tvak INTO DATA(ls_tavk) WITH KEY auart = <fs_alv>-auart BINARY SEARCH.
    IF sy-subrc NE 0.
      <fs_alv>-id    = '@5C@'.
      CONCATENATE <fs_alv>-mess text-906 INTO <fs_alv>-mess."'销售凭证类型不存在'.
    ENDIF.

    READ TABLE lt_werks INTO DATA(ls_werks) WITH KEY werks = <fs_alv>-werks BINARY SEARCH.
    IF sy-subrc NE 0.
      <fs_alv>-id    = '@5C@'.
      CONCATENATE <fs_alv>-mess text-m01 INTO <fs_alv>-mess."'工厂不存在'.
    ENDIF.

    READ TABLE lt_vbap INTO DATA(ls_vbap) WITH KEY vbeln = <fs_alv>-vgbel
                                                   posnr = <fs_alv>-vgpos BINARY SEARCH.
    IF sy-subrc NE 0.
      <fs_alv>-id    = '@5C@'.
      CONCATENATE <fs_alv>-mess text-m01 INTO <fs_alv>-mess."'工厂不存在'.
    ENDIF.

  ENDLOOP.


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
*       创建销售订单
*----------------------------------------------------------------------*
FORM frm_impt_data .

  TYPES:BEGIN OF ly_vgbel,
          auart TYPE vbak-auart,
          vgbel TYPE vbap-vgbel,
          vbeln TYPE vbak-vbeln,
          mess  TYPE bapi_msg,
          id    TYPE icon-id,
        END OF ly_vgbel.

  DATA:lt_vgbel TYPE TABLE OF ly_vgbel,
       ls_vgbel TYPE ly_vgbel.

  DATA lv_mode TYPE ctu_params-dismode VALUE 'N'.
  DATA:lv_mess  TYPE bapi_msg,
       lv_id    TYPE icon-id,
       lv_vbeln TYPE vbak-vbeln,
       err_flag.

  DATA:lv_menge TYPE BDC_FVAL.

  DATA:temp_posnr TYPE vbap-posnr.

  DATA:lt_alv TYPE TABLE OF ty_alv.
  DATA:lt_alv_temp TYPE TABLE OF ty_alv.
  DATA:ls_alv TYPE ty_alv.
  REFRESH lt_vgbel.
  lt_alv = gt_alv.
  SORT lt_alv BY vgbel vgpos.
  LOOP AT lt_alv INTO gs_alv WHERE checkbox = 'X' AND id <> '@5C@'.

*&---如果该条数据已经正式导入成功，则不再导入
    IF gs_alv-id = '@5B@'.
      CONTINUE.
    ENDIF.
    APPEND gs_alv TO lt_alv_temp.

    AT END OF vgbel.

      READ TABLE lt_alv INTO gs_alv INDEX sy-tabix.

      PERFORM bdc_dynpro      USING 'SAPMV45A' '0101'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'VBAK-VKORG'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=COPY'.
      PERFORM bdc_field       USING 'VBAK-AUART'
                                    gs_alv-auart.
      PERFORM bdc_dynpro      USING 'SAPLV45C' '0100'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=RAUF'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'LV45C-VBELN'.
      PERFORM bdc_dynpro      USING 'SAPLV45C' '0100'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=RUE1'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'LV45C-VBELN'.
      PERFORM bdc_field       USING 'LV45C-VBELN'
                                    gs_alv-vgbel.
      PERFORM bdc_dynpro      USING 'SAPMV45A' '4413'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'VBAPD-ORFMNG(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=MKLO'.
      LOOP AT lt_alv_temp INTO ls_alv.
        PERFORM bdc_dynpro      USING 'SAPMV45A' '4413'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'VBAPD-ORFMNG(01)'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=POPO'.
        PERFORM bdc_dynpro      USING 'SAPMV45A' '0251'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'RV45A-POSNR'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=POSI'.
        PERFORM bdc_field       USING 'RV45A-POSNR'
                                      ls_alv-vgpos.
        PERFORM bdc_dynpro      USING 'SAPMV45A' '4413'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'VBAPD-ORFMNG(01)'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=MARK'.
      ENDLOOP.

      PERFORM bdc_dynpro      USING 'SAPMV45A' '4413'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'VBAPD-ORFMNG(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=RUEB'.
      CLEAR temp_posnr.
      LOOP AT lt_alv_temp INTO ls_alv.
        ADD 10 TO temp_posnr.
        PERFORM bdc_dynpro      USING 'SAPMV45A' '4001'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=POPO'.
        PERFORM bdc_dynpro      USING 'SAPMV45A' '0251'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'RV45A-POSNR'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=POSI'.
        PERFORM bdc_field       USING 'RV45A-POSNR'
                                      temp_posnr.
        PERFORM bdc_dynpro      USING 'SAPMV45A' '4001'.
                PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '/00'.
        lv_menge = ls_alv-kwmeng.
        CONDENSE lv_menge.
        PERFORM bdc_field       USING 'RV45A-KWMENG(01)'
                                      lv_menge.
        PERFORM bdc_field       USING 'VBAP-WERKS(01)'
                                      ls_alv-werks.
      ENDLOOP.
      PERFORM bdc_dynpro      USING 'SAPMV45A' '4001'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SICH'.
      PERFORM bdc_dynpro      USING 'SAPLSPO2' '0100'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=OPT1'.
      CALL TRANSACTION 'VA01' USING bdcdata MODE lv_mode MESSAGES INTO messtab.
      LOOP AT messtab  WHERE msgtyp = 'A' OR msgtyp = 'E'.
        err_flag = 'X'.
        <fs_alv>-id = '@5C@'.
        MESSAGE ID messtab-msgid TYPE messtab-msgtyp NUMBER messtab-msgnr
        WITH messtab-msgv1 messtab-msgv2 messtab-msgv3 messtab-msgv4 INTO lv_mess.
        CONCATENATE gs_alv-mess lv_mess INTO gs_alv-mess.
      ENDLOOP.
      IF err_flag IS NOT INITIAL.
        ROLLBACK WORK.
      ELSE.
        COMMIT WORK AND WAIT.
        READ TABLE MESSTAB WITH KEY msgnr = '311' msgid = 'V1'.
        gs_alv-VBELN = messtab-msgv2.
        GS_ALV-ID = '@5B@'.
        gs_alv-mess = '创建销售订单成功，销售订单号为:' && gs_alv-vbeln.
      ENDIF.
      CLEAR ls_vgbel.
      MOVE-CORRESPONDING gs_alv TO ls_vgbel.
      APPEND ls_vgbel TO lt_vgbel.
    ENDAT.
  ENDLOOP.
  LOOP AT gt_alv ASSIGNING <fs_alv>.
    READ TABLE lt_vgbel INTO ls_vgbel WITH KEY vgbel = <fs_alv>-vgbel auart = <fs_alv>-auart.
    IF sy-subrc = 0.
      <fs_alv>-mess = ls_vgbel-mess.
      <fs_alv>-id   = ls_vgbel-id.
      <fs_alv>-vbeln = ls_vgbel-vbeln.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " FRM_IMPT_DATA
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
    CLEAR BDCDATA.
    BDCDATA-FNAM = FNAM.
    BDCDATA-FVAL = FVAL.
    APPEND BDCDATA.
ENDFORM.