*$*$********************************************************************
* Program ID/Name:  ZPPR021               Date written: 20131126
* Author's name:   HP_DXJ                       Last update:
* Program title:  报废批量导入
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



*&---------------------------------------------------------------------*
*& Report  ZPPR021
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZPPR021.


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
TYPES:
    "Common Excel upload data
  BEGIN OF t_excel,
    text(8000) TYPE c,
  END OF t_excel.



*$*$********************************************************************
*$*$    GLOBAL CONSTANTS                                               *
*$*$********************************************************************
CONSTANTS:
            c_red(4)      TYPE c          VALUE '@0A@',
            c_yellow(4)   TYPE c          VALUE '@09@',
            c_green(4)    TYPE c          VALUE '@08@'.

CONSTANTS:
  c_x(1)   TYPE  c VALUE 'X',
  c_rstr   TYPE  i VALUE '1',
  c_rend   TYPE  i VALUE '65535',
  c_cstr   TYPE  i VALUE '1',
  c_cend   TYPE  i VALUE '9999'.

*$*$********************************************************************
*$*$    GLOBAL INTERNAL TABLES                                         *
*$*$********************************************************************
DATA:BEGIN OF it_alv_data OCCURS 0 ,
          number        TYPE p          ,
          icon          TYPE icon_d     , " 图标
          werks         TYPE werks_d    , " 工厂
          lgort         TYPE lgort_d    , " 库存地点
          vornr         TYPE vornr      , " 工艺序号
          equno(29)     TYPE c          , " 工作站号
          equnm(80)     TYPE c          , " 工作站名
          matnr         TYPE matnr      ,  " 零件
          exidv(20)     TYPE c          , " 跟踪卡
          scqty(13)     TYPE c          ,  " 报废数量
          zdate         TYPE sy-datum   ,  " 日期
          reason(4)     TYPE c          ,   " 报废原因
          kostl         TYPE crco-kostl ,
          vgw01         TYPE vgw01      ,
          vgw02         TYPE vgw01      ,
          arbpl         TYPE crhd-arbpl ,
          message(255)  TYPE c          ,    " 消息
     END OF it_alv_data .

DATA:BEGIN OF it_upload_data OCCURS 0 ,
          number(17)    TYPE C          ,
          werks         TYPE werks_d    , " 工厂
          lgort         TYPE lgort_d    , " 库存地点
          vornr         TYPE vornr      , " 工艺序号
          equno(29)     TYPE c          , " 工作站号
          equnm(80)     TYPE c          , " 工作站名
          matnr         TYPE matnr      ,  " 零件
          exidv(20)     TYPE c          , " 跟踪卡
          scqty(13)     TYPE c          ,  " 报废数量
          zdate         TYPE ekko-bedat ,  " 日期
          reason(4)     TYPE c          ,   " 报废原因
     END OF it_upload_data .

DATA:BEGIN OF it_data OCCURS 0 ,
          number        TYPE p         ,
          werks         TYPE werks_d    , " 工厂
          lgort         TYPE lgort_d    , " 库存地点
          vornr         TYPE vornr      , " 工艺序号
          matnr         TYPE matnr      ,  " 父零件
          exidv(20)     TYPE c          , " 跟踪卡
          scqty         TYPE ekpo-menge ,  " 报废数量
          zmatnr        TYPE matnr      ,  " 子零件
          zexidv(20)    TYPE c          , " 子零件HU
     END OF it_data .


DATA:
  i_separator   TYPE  c,
  i_file        TYPE  RLGRAP-FILENAME,
  wa_l_excel    TYPE  t_excel,
  it_l_excel    TYPE  STANDARD TABLE OF t_excel.

DATA:
      it_raw            TYPE  TRUXS_T_TEXT_DATA .


*$*$********************************************************************
*$*$    GLOBAL ELEMENTARY VARIABLES                                    *
*$*$********************************************************************
DATA: gs_repid    TYPE sy-repid,
      gs_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gs_sortinfo TYPE slis_t_sortinfo_alv,
      as_sortinfo TYPE slis_sortinfo_alv ,
      as_fieldcat TYPE slis_fieldcat_alv.

*$*$********************************************************************
*$*$    PARAMETERS & SELECT-OPTIONS                                    *
*$*$********************************************************************
SELECTION-SCREEN:BEGIN OF BLOCK B1 WITH FRAME TITLE text-001.

PARAMETERS:
            p_file TYPE string OBLIGATORY .

SELECTION-SCREEN:END OF BLOCK B1 .






*$*$********************************************************************
*$*$    INITIALIZATION                                                 *
*$*$********************************************************************
INITIALIZATION.
*$*$********************************************************************
*$*$    AT SELECTION-SCREEN  Output                                    *
*$*$********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM FRM_CHOOSE_INPUT_FILE .


*$*$********************************************************************
*$*$    AT SELECTION-SCREEN                                            *
*$*$********************************************************************
AT SELECTION-SCREEN.
*$*$********************************************************************
*$*$    START-OF-SELECTION                                             *
*$*$********************************************************************
START-OF-SELECTION.

  PERFORM FRM_UPLOAD_DATA .

  IF it_upload_data[] IS INITIAL.

    MESSAGE s000(ZMM_MSG) DISPLAY LIKE 'E' WITH '文件内容为空!' .
    STOP .

  ENDIF.

  PERFORM frm_check_upload_data.

  SORT it_alv_data by icon DESCENDING .

  PERFORM frm_display_alv .






*&---------------------------------------------------------------------*
*&      Form  FRM_CHOOSE_INPUT_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CHOOSE_INPUT_FILE .

  DATA: I_FNAME TYPE STRING,
       IT_L_FILETABLE TYPE TABLE OF FILE_TABLE,
       I_RC TYPE I,
       I_TITLE TYPE STRING,
       I_FILTER TYPE string ,
       I_ACTION TYPE I.
  I_TITLE = TEXT-002.

  I_FILTER = CL_GUI_FRONTEND_SERVICES=>FILETYPE_EXCEL.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = I_TITLE
*     DEFAULT_FILENAME        = '.XLSX'
      FILE_FILTER             = I_FILTER
    CHANGING
      FILE_TABLE              = IT_L_FILETABLE
      RC                      = I_RC
      USER_ACTION             = I_ACTION
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    CLEAR I_FNAME.
  ELSE.
    IF I_ACTION = 0.
      READ TABLE IT_L_FILETABLE INDEX 1 INTO I_FNAME.
    ELSE.
      CLEAR I_FNAME.
    ENDIF.
  ENDIF.
  P_FILE = I_FNAME.



ENDFORM.                    " FRM_CHOOSE_INPUT_FILE
*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_UPLOAD_DATA .

  CLEAR:i_file.
  i_file = p_file.

  REFRESH:it_l_excel.
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
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE s011(zpp_msg) DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF it_l_excel IS INITIAL.
    MESSAGE s012(zpp_msg) DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

*--- 形成上传数据
  LOOP AT it_l_excel INTO wa_l_excel.

    SPLIT wa_l_excel AT i_separator
     INTO it_upload_data-number
          it_upload_data-werks
          it_upload_data-lgort
          it_upload_data-vornr
          it_upload_data-equno
          it_upload_data-equnm
          it_upload_data-matnr
          it_upload_data-exidv
          it_upload_data-scqty
          it_upload_data-zdate
          it_upload_data-reason .
    APPEND it_upload_data .
    CLEAR it_upload_data .

  ENDLOOP .



ENDFORM.                    " FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DISPLAY_ALV .

  CLEAR:gs_fieldcat,gs_fieldcat[].

  PERFORM frm_field   USING   'ICON'      ''                  ''  '5'.
  PERFORM frm_field   USING   'NUMBER'     '序号'              '' '3'.
  PERFORM frm_field   USING   'WERKS'     '工厂'              '' '5'.
  PERFORM frm_field   USING   'LGORT'     '库存地点'              '' '5'.
  PERFORM frm_field   USING   'VORNR'     '道序号(工艺序号)'          '' '4'.
  PERFORM frm_field   USING   'EQUNO'     '工作站号'            '' '15'.
  PERFORM frm_field   USING   'EQUNM'     '工作站名'            '' '15'.
  PERFORM frm_field   USING   'MATNR'     '零件号'          ''  '15'.
  PERFORM frm_field   USING   'EXIDV'     '跟踪卡'           '' '18'.
  PERFORM frm_field   USING   'SCQTY'     '报废数量'           '' '5' .
  PERFORM frm_field   USING   'ZDATE'     '日期'           ''  '10'.
  PERFORM frm_field   USING   'REASON'     '报废原因'           '' '4'.
  PERFORM frm_field   USING   'MESSAGE'     '消息'           '' '40' .

*  as_sortinfo-fieldname = 'ICON' .
*  as_sortinfo-down      = 'X'    .
*  APPEND as_sortinfo TO gs_sortinfo .
*  CLEAR as_sortinfo .


  PERFORM frm_layout  TABLES it_alv_data USING 'PF_STATUS' ''.


ENDFORM.                    " FRM_DISPLAY_ALV

*&---------------------------------------------------------------------*
*&      Form  FRM_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM frm_field  USING p_fieldname p_seltext p_no_zero p_outputlen.
  as_fieldcat-fieldname       = p_fieldname.
  as_fieldcat-seltext_l       = p_seltext.
  as_fieldcat-no_zero         = p_no_zero.
  as_fieldcat-outputlen       = p_outputlen .

  APPEND as_fieldcat TO gs_fieldcat.
  CLEAR as_fieldcat.
ENDFORM.                    " FRM_FIELD

*&---------------------------------------------------------------------*
*&      Form  FRM_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM frm_layout  TABLES   p_data USING p_status p_box.
  gs_repid = sy-repid.
  gs_layout-zebra = 'X'.
*  gs_layout-colwidth_optimize = 'X'.
*  gs_layout-box_fieldname      =  p_box.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = gs_repid
      i_save                   = 'A'
      it_fieldcat              = gs_fieldcat[]
      it_sort                  = gs_sortinfo[]
      is_layout                = gs_layout
      i_callback_pf_status_set = p_status
      i_callback_user_command  = 'USER_COMMAND'
    TABLES
      t_outtab                 = p_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "frm_layout
*&---------------------------------------------------------------------*
*&      FORM  PF_STATUS
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->RT_EXTAB   TEXT
*----------------------------------------------------------------------*

FORM pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS '1000_STATUS'.
  set TITLEBAR  '1000_TITLE' .
ENDFORM.                    "PF_STATUS
*&---------------------------------------------------------------------*
*&      FORM  USER_COMMAND
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->R_UCOMM      TEXT
*      -->RS_SELFIELD  TEXT
*----------------------------------------------------------------------*
FORM user_command  USING r_ucomm TYPE sy-ucomm  rs_selfield TYPE slis_selfield.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  DATA: ls_stable TYPE lvc_s_stbl.

* 将界面中的选择数据更新到内表中
*=====GET_GLOBALS_FROM_SLVC_FULLSCR  START==========
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.
  CALL METHOD lr_grid->check_changed_data.
*=====GET_GLOBALS_FROM_SLVC_FULLSCR  END============
  CASE r_ucomm.
    WHEN 'DATA_SAVE'.
      PERFORM frm_exec.
*      PERFORM frm_get_data01.
    WHEN 'BACK'.
      REFRESH:it_alv_data ,it_data ,it_upload_data .
      LEAVE TO SCREEN 0 .
    WHEN 'EXIT' .
      REFRESH:it_alv_data ,it_data ,it_upload_data .
      LEAVE TO SCREEN 0 .
    WHEN  'CANCEL'.
      REFRESH:it_alv_data ,it_data ,it_upload_data .
      LEAVE PROGRAM .
  ENDCASE.

  SORT it_alv_data by icon DESCENDING .

  rs_selfield-row_stable = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-refresh = 'X'.


ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_EXEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_EXEC .
  DATA: lwa_alv_data LIKE LINE OF it_alv_data .
  DATA: ls_headret    TYPE ZSHEADRET2,
        lt_item       TYPE STANDARD TABLE OF ZSITEM2,
        ls_item       TYPE ZSITEM2,
        lt_return     TYPE STANDARD TABLE OF BAPIRET2,
        ls_return     TYPE BAPIRET2,
        lt_ztscrp     TYPE STANDARD TABLE OF ZTSCRP,
        wa_l_ztscrp   TYPE ZTSCRP,
        ls_scrap      TYPE ZSMMSCRAP.

  LOOP AT it_alv_data INTO lwa_alv_data WHERE icon IS INITIAL
                                           OR icon = c_red.
    CLEAR: ls_item ,lt_item ,lt_return .
*   读取到子零件号，继续执行
    READ TABLE it_data WITH KEY number = lwa_alv_data-number .
    IF sy-subrc = 0 .
      ls_item-zeile       = '0001'         .
      ls_item-material    = it_data-zmatnr .
      ls_item-plant       = it_data-werks  .
      ls_item-stge_loc    = it_data-lgort  .
      ls_item-entry_qnt   = it_data-scqty  .
      ls_item-zskuno      = it_data-zexidv .
      ls_item-zsum01      = lwa_alv_data-vgw01 * lwa_alv_data-scqty .
      ls_item-zsum02      = lwa_alv_data-vgw02 * lwa_alv_data-scqty .
      ls_item-move_reas   = lwa_alv_data-reason .
      ls_item-arbpl       = lwa_alv_data-arbpl  .
      ls_item-zpro_eqp    = lwa_alv_data-equno   .
      ls_item-zpro_type    = 'EPSdp齿条'         .
      APPEND ls_item to lt_item .

      CALL FUNCTION 'ZMM_DEAL_SCRAP'
        EXPORTING
          I_REF_DOC_NO = ''
          I_PSTNG_DATE = lwa_alv_data-zdate
          I_DOC_DATE   = lwa_alv_data-zdate
          I_MATERIAL   = lwa_alv_data-matnr
          I_CL_NO      = ''
          I_VORNR      = lwa_alv_data-vornr
          I_KOSTL      = lwa_alv_data-kostl
          I_FLG        = 'X'"针对接口的控制
        IMPORTING
          EX_HEADRET   = LS_HEADRET
        TABLES
          TS_ITEM      = LT_ITEM
          RETURN       = LT_RETURN.
***将出错信息封装进表
      IF SY-SUBRC <> 0.

      ENDIF.

      READ TABLE lt_return INTO ls_return WITH KEY type = 'E' .
      IF sy-subrc = 0 .

        lwa_alv_data-icon = c_red .
        lwa_alv_data-message = ls_return-message .

      ELSE.

        lwa_alv_data-icon = c_green .
        CONCATENATE '物料凭证' ls_headret-mat_doc '已创建。' INTO lwa_alv_data-message .

        wa_l_ztscrp-zmblnr = ls_headret-mat_doc .
        wa_l_ztscrp-zmjahr = sy-datum+0(4) .
        wa_l_ztscrp-zzeile = '0001'        .
        wa_l_ztscrp-z_refno = ls_headret-mat_doc .

        UPDATE ZTSCRP SET Z_REFNO = ls_headret-mat_doc
                      WHERE zmblnr = wa_l_ztscrp-zmblnr
                      AND   zmjahr = wa_l_ztscrp-zmjahr
                      AND   zzeile = wa_l_ztscrp-zzeile.
        IF sy-subrc = 0 .

          COMMIT WORK AND WAIT .

        ENDIF.

      ENDIF.

      MODIFY it_alv_data FROM lwa_alv_data .
      CLEAR lwa_alv_data .

    ENDIF.

  ENDLOOP.

ENDFORM.                    " FRM_EXEC
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CHECK_UPLOAD_DATA .


  DATA: lv_bmeng             TYPE stko-bmeng             ,
        lwa_upload_data LIKE LINE OF it_upload_data ,
        lwa_alv_data    LIKE LINE OF it_alv_data    ,
        lv_error TYPE c .

  DATA:BEGIN OF lt_check OCCURS 0 ,
          number(13) TYPE c ,
       END OF lt_check .

  DATA: BEGIN OF lt_task OCCURS 0 ,
             matnr   TYPE mapl-matnr ,
             werks   TYPE mapl-werks ,
             plnnr   TYPE mapl-plnnr ,
        END OF   lt_task .

  DATA: BEGIN OF lt_material OCCURS 0 ,
            matnr   TYPE marc-matnr ,
        END OF lt_material .

  DATA:BEGIN OF lt_exidv OCCURS 0 ,
            matnr   TYPE vepo-matnr,
            werks   TYPE vepo-werks ,
            lgort   TYPE vepo-lgort ,
            exidv   TYPE vekp-exidv ,
            exidv2  TYPE vekp-exidv2 ,
            vemng   TYPE vepo-vemng ,
        END OF lt_exidv.

  DATA:
        lt_tsk_tab  LIKE STANDARD TABLE OF capp_tsk ,
        lt_opr_tab  LIKE STANDARD TABLE OF capp_opr WITH HEADER LINE ,
        lt_stb      LIKE STANDARD TABLE OF stpox    WITH HEADER LINE .

  LOOP AT it_upload_data INTO lwa_upload_data.

    TRANSLATE lwa_upload_data-werks TO UPPER CASE .
    TRANSLATE lwa_upload_data-matnr TO UPPER CASE .
    TRANSLATE lwa_upload_data-exidv TO UPPER CASE .

    CALL FUNCTION 'CONVERSION_EXIT_NUMCV_INPUT'
      EXPORTING
        INPUT  = lwa_upload_data-vornr
      IMPORTING
        OUTPUT = lwa_upload_data-vornr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = lwa_upload_data-exidv
      IMPORTING
        OUTPUT = lwa_upload_data-exidv.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = lwa_upload_data-reason
      IMPORTING
        OUTPUT = lwa_upload_data-reason.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT        = lwa_upload_data-matnr
      IMPORTING
        OUTPUT       = lwa_upload_data-matnr
      EXCEPTIONS
        LENGTH_ERROR = 1
        OTHERS       = 2.


    modify it_upload_data FROM lwa_upload_data .
    MOVE-CORRESPONDING lwa_upload_data to it_alv_data .
    APPEND it_alv_data .
    CLEAR: lv_error , lwa_upload_data ,it_alv_data.

  ENDLOOP.

*    取得任务清单
  IF it_alv_data[] IS NOT INITIAL.

    SELECT matnr
           werks
           plnnr
          INTO TABLE lt_task
          FROM mapl
          FOR ALL ENTRIES IN it_alv_data
          WHERE matnr = it_alv_data-matnr
          AND   werks = it_alv_data-werks
          AND   plnty = 'N'
          AND   plnal = '01' .

  ENDIF.

  SORT it_alv_data by number .

  LOOP AT it_alv_data .

    CLEAR lwa_alv_data .
    lwa_alv_data = it_alv_data .

    IF lwa_alv_data-scqty <= 0 .
      it_alv_data-icon = c_red .
      MESSAGE s071(ZPP_MSG) INTO lwa_alv_data-message.
      MODIFY it_alv_data FROM lwa_alv_data.
      CONTINUE .

    ENDIF.

*    检查序号
    READ TABLE lt_check WITH KEY number = lwa_alv_data-number .
    IF sy-subrc = 0 .

      it_alv_data-icon = c_red .
      MESSAGE s071(ZPP_MSG) INTO lwa_alv_data-message.
      MODIFY it_alv_data FROM lwa_alv_data.
      CONTINUE .

    ENDIF.


    AT NEW number.

      lt_check-number = it_alv_data-number .
      APPEND lt_check .

    ENDAT .

*    检查工序号
    READ TABLE lt_task WITH KEY matnr = lwa_alv_data-matnr
                                werks = lwa_alv_data-werks  .
    IF sy-subrc <> 0.

      lwa_alv_data-icon = c_red .
      MESSAGE s069(ZPP_MSG) WITH lwa_alv_data-werks lwa_alv_data-matnr lwa_alv_data-vornr INTO lwa_alv_data-message.
      MODIFY it_alv_data FROM lwa_alv_data.
      CONTINUE .

    ENDIF.
*    检查工序号
    CALL FUNCTION 'CARO_ROUTING_READ'
      EXPORTING
        DATE_FROM            = lwa_alv_data-zdate
        DATE_TO              = lwa_alv_data-zdate
        PLNTY                = 'N'
        PLNNR                = lt_task-plnnr
        PLNAL                = '01'
        MATNR                = lt_task-matnr
*       BUFFER_DEL_FLG       = 'X'
*       DELETE_ALL_CAL_FLG   = 'X'
*       ADAPT_FLG            = 'X'
*       IV_CREATE_ADD_CHANGE = ' '
      TABLES
        TSK_TAB              = lt_tsk_tab
*       SEQ_TAB              =
        OPR_TAB              = lt_opr_tab
*       PHASE_TAB            =
*       SUBOPR_TAB           =
*       REL_TAB              =
*       COM_TAB              =
*       REFERR_TAB           =
*       REFMIS_TAB           =
*       IT_AENR              =
      EXCEPTIONS
        NOT_FOUND            = 1
        REF_NOT_EXP          = 2
        NOT_VALID            = 3
        OTHERS               = 4.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
      lwa_alv_data-icon = c_red .
      MESSAGE s069(ZPP_MSG) WITH lwa_alv_data-werks lwa_alv_data-matnr lwa_alv_data-vornr INTO lwa_alv_data-message.
      MODIFY it_alv_data.
      CONTINUE .
    ENDIF.

    READ TABLE lt_opr_tab WITH KEY vornr = lwa_alv_data-vornr
                                   werks = lwa_alv_data-werks .
    IF sy-subrc <> 0 .

      lwa_alv_data-icon = c_red .
      MESSAGE s069(ZPP_MSG) WITH lwa_alv_data-werks lwa_alv_data-matnr lwa_alv_data-vornr INTO lwa_alv_data-message.
      MODIFY it_alv_data FROM lwa_alv_data.
      CONTINUE .

    ENDIF .

    lwa_alv_data-vgw01 = lt_opr_tab-vgw01 .
    lwa_alv_data-vgw02 = lt_opr_tab-vgw02 .
*          成本中心
    SELECT SINGLE kostl INTO lwa_alv_data-kostl
          FROM crco
          WHERE objid = lt_opr_tab-arbid
          AND   objty = 'A' .
*         工作中心
    SELECT SINGLE arbpl INTO lwa_alv_data-arbpl
          FROM crhd
          WHERE objid = lt_opr_tab-arbid
          AND   werks = lt_opr_tab-werks .


    lv_bmeng = lwa_alv_data-scqty .

*    检查子零件物料是否存在
    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
         CAPID                       = 'PP01'
         DATUV                       = lwa_alv_data-zdate
*       DELNL                       = ' '
*       DRLDT                       = ' '
         EHNDL                       = '1'
         EMENG                       = lv_bmeng
         MDMPS                       = 'X'
         MEHRS                       = 'X'
*       MKMAT                       = ' '
*       MMAPS                       = ' '
*       SALWW                       = ' '
*       SPLWW                       = ' '
         MMORY                       = 'X'
         MTNRV                       = lwa_alv_data-matnr
         STLAL                       = '01'
         WERKS                       = lwa_alv_data-werks
*     IMPORTING
*       TOPMAT                      =
*       DSTST                       =
      TABLES
        STB                         = lt_stb
*       MATCAT                      =
     EXCEPTIONS
       ALT_NOT_FOUND               = 1
       CALL_INVALID                = 2
       MATERIAL_NOT_FOUND          = 3
       MISSING_AUTHORIZATION       = 4
       NO_BOM_FOUND                = 5
       NO_PLANT_DATA               = 6
       NO_SUITABLE_BOM_FOUND       = 7
       CONVERSION_ERROR            = 8
       OTHERS                      = 9
              .

    IF lt_stb[] IS INITIAL OR sy-subrc <> 0  .

      lwa_alv_data-icon = c_red .
      MESSAGE s070(ZPP_MSG) INTO lwa_alv_data-message.
      MODIFY it_alv_data FROM lwa_alv_data.
      CONTINUE .
    ENDIF.

    SELECT
          matnr
          INTO TABLE lt_material
          FROM marc
          FOR ALL ENTRIES IN lt_stb
          WHERE z_mpj = 'Y'
          AND   matnr = lt_stb-idnrk
          AND   werks = lt_stb-werks  .
    IF lt_material[] IS INITIAL.

      lwa_alv_data-icon = c_red .
      MESSAGE s076(ZPP_MSG) INTO lwa_alv_data-message.
      MODIFY it_alv_data FROM lwa_alv_data.
      CONTINUE .

    ENDIF.

*      取出子零件的HU信息
    SELECT
          b~matnr
          b~werks
          b~lgort
          a~exidv
          a~exidv2
          b~vemng
          INTO CORRESPONDING FIELDS OF TABLE lt_exidv
          FROM vekp AS a
          INNER JOIN vepo AS b
          ON a~venum = b~venum
          FOR ALL ENTRIES IN lt_stb
          WHERE a~exidv = lwa_alv_data-exidv
          AND   b~werks  = lwa_alv_data-werks
          AND   b~lgort  = lwa_alv_data-lgort
          AND   b~matnr  = lt_stb-idnrk .

    IF lt_exidv[] IS INITIAL.
      lwa_alv_data-icon = c_red .
      MESSAGE s072(ZPP_MSG) INTO lwa_alv_data-message.

      MODIFY it_alv_data FROM lwa_alv_data.
      CONTINUE .

    ENDIF.

    LOOP AT lt_stb.

      READ TABLE lt_exidv WITH KEY matnr = lt_stb-idnrk
                                   werks = lwa_alv_data-werks
                                   lgort = lwa_alv_data-lgort
                                   exidv = lwa_alv_data-exidv.
      IF sy-subrc = 0 AND lt_exidv-vemng >= lt_stb-mnglg.

        MOVE-CORRESPONDING lwa_alv_data to it_data .
        it_data-zexidv = lt_exidv-exidv .
        it_data-zmatnr = lt_exidv-matnr .
        APPEND it_data .
        CLEAR it_data .
        EXIT .

      ENDIF.

    ENDLOOP.
*   检查是否找到Hu
    READ TABLE it_data WITH KEY number = lwa_alv_data-number .
    IF sy-subrc <> 0 .
      lwa_alv_data-icon = c_red .
      MESSAGE s072(ZPP_MSG) INTO lwa_alv_data-message.
    ENDIF.

    MODIFY it_alv_data FROM lwa_alv_data .
    CLEAR lwa_alv_data .

  ENDLOOP.


ENDFORM.                    " FRM_CHECK_UPLOAD_DATA
