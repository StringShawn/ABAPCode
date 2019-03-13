*&--------------------------------------------------------------------*
* 事务代码：ZTLSD1084
* 程序名称：定金单小宝打印
* 设 计 人：夜煦航
* 设计时间：2018-11-20
* 程序类型: ABAP/4 程序
* 应用类型:
* 描 述:
*(修改日志)------------------------------------------------------------*
*
* 日志号 修改人 修改时间 修改说明 传输号码
* ---- ---- ------ ----------------------------------------------------*
* 001
*&---------------------------------------------------------------------*
REPORT ztlsd1023 MESSAGE-ID zmsg01.
*&---------------------------------------------------------------------*
*&----Tables & Type-Pools
*&---------------------------------------------------------------------*
TABLES:zsd_frontm.
*&---------------------------------------------------------------------*
*&----类型声明
*&---------------------------------------------------------------------*
TYPES:BEGIN OF ty_print,
        zdjdh       TYPE zsd_frontm-zdjdh,   "定金单号
        sel         TYPE c,                  "选择
        zcsmd       TYPE zsd_frontm-zcsmd,   "门店编码
        name1       TYPE t001w-name1,        "门店名称
        tel_number  TYPE adr2-tel_number,    "电话号码
        zyddh       TYPE zsd_frontm-zyddh,   "预定/订单号
        zgkxm       TYPE zsd_frontm-zgkxm,   "顾客姓名
        zgksjh      TYPE zsd_frontm-zgksjh,  "顾客联系方式
        zcdate      TYPE zsd_frontm-zcdate,  "订货日期
        zsfkt       TYPE zsd_frontm-zsfkt,   "是否可退
        zsfkt_text  TYPE char4,              "选择方式
        charg       TYPE vbap-charg,         "饰品批次号
        arktx       TYPE vbap-arktx,         "饰品名称
        z_sgj       TYPE zmm_dpsxb-z_sgj,    "饰品上柜价
        zxjsk       TYPE zsd_frontm-zxjsk,   "收款金额-现金
        zpssk       TYPE zsd_frontm-zpssk,   "收款金额-POS
        zscsk       TYPE zsd_frontm-zscsk,   "收款金额-商场卡
        zskje       TYPE zsd_frontm-zscsk,   "收款金额-汇总
        zdjdzt      TYPE zsd_frontm-zdjdzt,  "订金单状态
        zdjdzt_text TYPE char10,             "订金单状态-文本
        zskfs       TYPE char10,             "收款方式
        zappid      TYPE zsd_frontm-zappid,  "订金经办人
        zgktbyq     TYPE zsd_frontm-zgktbyq, "顾客特别要求
        udate       TYPE zsd_frontm-udate,   "用(退)订日期
        zdycs       TYPE n,                  "打印次数
        zdyczr      TYPE sy-uname,           "打印操作人
      END OF ty_print.
*&---------------------------------------------------------------------*
*&----变量定义
*&---------------------------------------------------------------------*
DATA:gt_print TYPE TABLE OF ty_print.

DATA:gt_fcat    TYPE lvc_t_fcat,
     gs_fcat    TYPE lvc_s_fcat,
     gs_layo    TYPE lvc_s_layo,
     gs_setting TYPE lvc_s_glay.

*&---------------------------------------------------------------------*
*&----宏定义
*&---------------------------------------------------------------------*
DEFINE add_field.
  CLEAR gs_fcat.
  gs_fcat-fieldname  = &1. "字段名
  gs_fcat-COLTEXT    = &2. "列名称
  gs_fcat-seltext    = &2. "列标识
  APPEND gs_fcat TO gt_fcat.
END-OF-DEFINITION.


*&---------------------------------------------------------------------*
*&----选择屏幕定义
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
  s_cdate FOR zsd_frontm-zcdate, "订货日期
  s_zdjdh FOR zsd_frontm-zdjdh,  "定金单号
  s_zgkxm FOR zsd_frontm-zgkxm.  "顾客姓名

SELECTION-SCREEN END OF BLOCK blk1.

*&---------------------------------------------------------------------*
*&----屏幕事件
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&----主处理过程
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM frm_get_data.   "根据选择条件从系统抓取数据
  PERFORM frm_alv_show.   "展示到ALV
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       根据选择条件从系统抓取数据
*----------------------------------------------------------------------*
FORM frm_get_data .

  DATA:lt_dd07v TYPE TABLE OF dd07v.

  SELECT zdjdh  "订金单号
         zsfkt  "是否可退
         zcsmd  "门店编号
         zcdate "订货日期
         zyddh  "订单号
         zxjsk  "收款金额-现金
         zpssk  "收款金额-POS
         zscsk  "收款金额-商场卡
         zdjdzt "订金单状态
         zappid "订金经办人
         udate  "用(退)订日期
         zgksjh "顾客手机号
         zgkxm  "顾客姓名
         zgktbyq"顾客特别要求
         zdycs  "打印次数
         zdyczr "打印操作人
   FROM zsd_frontm
   INTO CORRESPONDING FIELDS OF TABLE gt_print
   WHERE zdjdh  IN s_zdjdh
     AND zcdate IN s_cdate
     AND zgkxm  IN s_zgkxm.

  IF gt_print IS NOT INITIAL.

    "取门店姓名
    SELECT werks ,
           name1 ,
           adrnr
    INTO TABLE @DATA(lt_werks)
    FROM t001w
    FOR ALL ENTRIES IN @gt_print
    WHERE werks = @gt_print-zcsmd.
    "取电话号码
    IF lt_werks IS NOT INITIAL.
      SELECT addrnumber ,
             tel_number
      INTO TABLE @DATA(lt_adr2)
      FROM adr2
      FOR ALL ENTRIES IN @lt_werks
      WHERE addrnumber = @lt_werks-adrnr.
    ENDIF.

    "取订金单状态描述
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZDJDZT'
        text           = 'X'
      TABLES
        dd07v_tab      = lt_dd07v
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "取销售订单相关信息
    SELECT vbeln ,
           charg ,
           arktx
    INTO TABLE @DATA(lt_vbap)
    FROM vbap FOR ALL ENTRIES IN @gt_print
    WHERE vbeln = @gt_print-zyddh.

    IF lt_vbap IS NOT INITIAL .
      "取上柜价
      SELECT charg ,
             z_sgj
      INTO TABLE @DATA(lt_dpsxb)
      FROM zmm_dpsxb FOR ALL ENTRIES IN @lt_vbap
      WHERE charg = @lt_vbap-charg.

    ENDIF.

  ENDIF.

  LOOP AT gt_print ASSIGNING FIELD-SYMBOL(<fs_print>).

    "门店名称
    READ TABLE lt_werks INTO DATA(ls_werks) WITH KEY werks = <fs_print>-zcsmd.
    IF sy-subrc = 0.
      <fs_print>-name1 = ls_werks-name1.
    ENDIF.

    "电话号码
    IF ls_werks IS NOT INITIAL.
      READ TABLE lt_adr2 INTO DATA(ls_adr2) WITH KEY addrnumber = ls_werks-adrnr.
      IF sy-subrc = 0.
        <fs_print>-tel_number = ls_adr2-tel_number.
      ENDIF.
    ENDIF.

    "批次 描述
    READ TABLE lt_vbap INTO DATA(ls_vbap) WITH KEY vbeln = <fs_print>-zyddh.
    IF sy-subrc = 0.
      <fs_print>-charg = ls_vbap-charg.
      <fs_print>-arktx = ls_vbap-arktx.
    ENDIF.
    "上柜价
    IF <fs_print>-charg IS NOT INITIAL.
      READ TABLE lt_dpsxb INTO DATA(ls_dpsxb) WITH KEY charg = <fs_print>-charg.
      IF sy-subrc = 0.
        <fs_print>-z_sgj = ls_dpsxb-z_sgj.
      ENDIF.
    ENDIF.
    "订金单状态描述
    READ TABLE lt_dd07v INTO DATA(ls_dd07v) WITH KEY domvalue_l = <fs_print>-zdjdzt.
    IF sy-subrc = 0.
      <fs_print>-zdjdzt_text = ls_dd07v-ddtext.
    ENDIF.
    "收款方式
    IF <fs_print>-zxjsk IS NOT INITIAL.
      <fs_print>-zskfs = '现金'.
    ELSEIF <fs_print>-zpssk IS NOT INITIAL.
      <fs_print>-zskfs = 'POS'.
    ELSEIF <fs_print>-zscsk IS NOT INITIAL.
      <fs_print>-zskfs = '商场卡'.
    ENDIF.

    "选择方式
    CASE <fs_print>-zsfkt.
      WHEN 'Y'.
        <fs_print>-zsfkt_text = '预订'.
      WHEN 'N'.
        <fs_print>-zsfkt_text = '预定'.
      WHEN OTHERS.
    ENDCASE.

    IF <fs_print>-zdycs IS INITIAL.
      <fs_print>-zdycs = 0.
    ENDIF.
    "收款金额
    <fs_print>-zskje = <fs_print>-zxjsk + <fs_print>-zpssk + <fs_print>-zscsk.

  ENDLOOP.

  SORT gt_print BY zdjdh.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_SHOW
*&---------------------------------------------------------------------*
*       展示到ALV
*----------------------------------------------------------------------*
FORM frm_alv_show .

  PERFORM frm_set_field.    "字段设置
  PERFORM frm_set_layout.   "布局
  PERFORM frm_alv_display.  "展示

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_FIELD
*&---------------------------------------------------------------------*
*       字段设置
*----------------------------------------------------------------------*
FORM frm_set_field .

  add_field 'ZDJDH'   '订金单号'.
  add_field 'ZCDATE'  '订货日期'.
  add_field 'ZCSMD'   '门店编码'.
  add_field 'NAME1'   '门店名称'.
  add_field 'ZGKXM'   '顾客姓名'.
  add_field 'ZSKJE'   '预付金额'.
  add_field 'ZDJDZT_TEXT'   '订金状态'.
  add_field 'ZDYCS'   '打印次数'.
  add_field 'ZDYCZR'  '打印操作人'.
  add_field 'ZGKTBYQ' '顾客特别要求'.

  LOOP AT gt_fcat INTO gs_fcat WHERE fieldname = 'ZGKTBYQ'.
    gs_fcat-edit = 'X'.
    MODIFY gt_fcat FROM gs_fcat TRANSPORTING edit.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_LAYOUT
*&---------------------------------------------------------------------*
*       布局
*----------------------------------------------------------------------*
FORM frm_set_layout .

  gs_layo-cwidth_opt    = 'X'.         "自动缩进
  gs_layo-zebra         = 'X'.         "斑马线
  gs_layo-box_fname     = 'SEL'.       "选择字段
  gs_setting-edt_cll_cb = 'X'.         "退出可编辑单元格时回调

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       展示
*----------------------------------------------------------------------*
FORM frm_alv_display .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_pf_status_set = 'FRM_SET_PF_STATUS'
      i_callback_user_command  = 'FRM_USER_COMMAND'
      i_callback_program       = sy-repid
      i_save                   = 'A'
      i_grid_settings          = gs_setting        "设置
      is_layout_lvc            = gs_layo           "输出格式
      it_fieldcat_lvc          = gt_fcat           "字段输出格式
    TABLES
      t_outtab                 = gt_print          "输出的内表
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_SET_PF_STATUS
*&---------------------------------------------------------------------*
*       SET PF STATUS
*----------------------------------------------------------------------*
FORM frm_set_pf_status USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_USER_COMMAND
*&---------------------------------------------------------------------*
*       事件
*----------------------------------------------------------------------*
FORM frm_user_command USING  r_ucomm TYPE sy-ucomm
                            rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&PRINT'.
      PERFORM frm_print_data. "打印
      rs_selfield-refresh = 'X'. " 更改后刷新

    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PRINT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_print_data .
  DATA:lt_item   TYPE TABLE OF zssd_1084_i,
       ls_item   TYPE zssd_1084_i,
       ls_header TYPE zssd_1084_h.
  DATA:lt_print TYPE TABLE OF ty_print,
       ls_print TYPE ty_print.
  DATA:lv_lines   TYPE i,
       lv_num     TYPE i,
       check_flag TYPE c,
       ls_control TYPE ssfctrlop.
  DATA:lc_fname TYPE rs38l_fnam.

  REFRESH lt_print.
  " 检查是否选中
  lt_print = gt_print.
  DELETE lt_print WHERE sel IS INITIAL.
  IF lt_print IS INITIAL.
    MESSAGE s005 DISPLAY LIKE 'E'.
    check_flag = 'X'.
  ENDIF.
  "只能打印一次
  LOOP AT lt_print INTO ls_print WHERE zdyczr IS NOT INITIAL.
    MESSAGE s000 WITH '订金单只能打印一次' DISPLAY LIKE 'E'.
    check_flag = 'X'.
    EXIT.
  ENDLOOP.


  CHECK check_flag IS INITIAL.

  ls_control-langu = '1'.
  ls_control-no_open = 'X'.
  ls_control-no_close = 'X'.

  "获取sf 名称
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZTLSD_SM_1084'
    IMPORTING
      fm_name            = lc_fname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      control_parameters = ls_control
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  LOOP AT lt_print INTO ls_print.
    MOVE-CORRESPONDING ls_print TO ls_header.
    MOVE-CORRESPONDING ls_print TO ls_item.
    ls_header-zhjje = ls_header-zhjje + ls_item-zskje.
    APPEND ls_item TO lt_item.
    AT END OF zdjdh.
      CALL FUNCTION 'ZISD_LOWER_TO_UPPER'
        EXPORTING
          amount   = ls_header-zhjje
        IMPORTING
          in_words = ls_header-zskje_text.

      "填充空行
      lv_lines = lines( lt_item ).
      lv_num   = 3 - ( lv_lines MOD 3 ) .
      DO lv_num TIMES.
        APPEND INITIAL LINE TO lt_item.
      ENDDO.
*&---------------------------------------------------------------------*
*&调用SMARTFORMS
*&---------------------------------------------------------------------*
      CALL FUNCTION lc_fname
        EXPORTING
          control_parameters = ls_control
          gs_header          = ls_header
        TABLES
          gt_item            = lt_item
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        EXIT.
      ENDIF.
    ENDAT.
  ENDLOOP.

  CALL FUNCTION 'SSF_CLOSE'
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT gt_print ASSIGNING FIELD-SYMBOL(<fs_print>) WHERE sel = 'X'.
    <fs_print>-zdyczr = sy-uname.
    <fs_print>-zdycs  = <fs_print>-zdycs + 1.
  ENDLOOP.

  REFRESH lt_print.
  lt_print = gt_print.
  DELETE lt_print WHERE sel IS INITIAL.
  LOOP AT lt_print INTO ls_print.
    UPDATE zsd_frontm SET zdyczr  = ls_print-zdyczr
                          zdycs   = ls_print-zdycs
                          zgktbyq = ls_print-zgktbyq WHERE zdjdh = ls_print-zdjdh.
  ENDLOOP.
  COMMIT WORK AND WAIT.
ENDFORM.
