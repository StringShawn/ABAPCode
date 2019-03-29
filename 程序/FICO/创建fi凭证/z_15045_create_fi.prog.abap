*&--------------------------------------------------------------------*
* 事务代码：ZFI1039
* 程序名称：月末“福利费”一次性过账
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
REPORT ztlfi1039 MESSAGE-ID zmsg01.
*&---------------------------------------------------------------------*
*&----Tables & Type-Pools
*&---------------------------------------------------------------------*
TABLES:faglflext,bkpf.
*&---------------------------------------------------------------------*
*&----类型声明
*&---------------------------------------------------------------------*
TYPES:BEGIN OF ty_data,
        racct  TYPE faglflext-racct,                     "总账科目号
*        txt50  TYPE skat-txt50,                     "总账科目名称
        rbukrs TYPE faglflext-rbukrs,                     "公司代码
        butxt  TYPE t001-butxt,                     "公司代码名称

        zqjnj  TYPE faglflext-hsl01,                "期间内借方
        zqjnd  TYPE faglflext-hsl01,                "期间内贷方
        minus  TYPE faglflext-hsl01,                "借方减贷方
        zyfxc  TYPE faglflext-hsl01,                "应付职工薪酬贷方
        hsl01	 TYPE hslxx12,
        hsl02  TYPE hslxx12,
        hsl03  TYPE hslxx12,
        hsl04  TYPE hslxx12,
        hsl05  TYPE hslxx12,
        hsl06  TYPE hslxx12,
        hsl07  TYPE hslxx12,
        hsl08  TYPE hslxx12,
        hsl09  TYPE hslxx12,
        hsl10  TYPE hslxx12,
        hsl11  TYPE hslxx12,
        hsl12  TYPE hslxx12,
        hsl13  TYPE hslxx12,
        hsl14  TYPE hslxx12,
        hsl15  TYPE hslxx12,
        hsl16  TYPE hslxx12,
        drcrk  TYPE faglflext-drcrk,
      END OF ty_data.
*&---------------------------------------------------------------------*
*&----变量定义
*&---------------------------------------------------------------------*
DATA:gt_data TYPE TABLE OF ty_data.
DATA:BEGIN OF gt_alv OCCURS 0,
       sel   TYPE c,
       belnr TYPE bkpf-belnr,
       zchae TYPE faglflext-hsl01,                "差额.
       month TYPE char6.                          "期间
        INCLUDE TYPE ty_data.
DATA END OF gt_alv .
DATA:gt_fcat    TYPE lvc_t_fcat,
     gs_fcat    TYPE lvc_s_fcat,
     gs_layo    TYPE lvc_s_layo,
     gt_events  TYPE slis_t_event, "保存AVL事件
     gs_setting TYPE lvc_s_glay.

DATA:s_racct TYPE RANGE OF faglflext-racct.
*&---------------------------------------------------------------------*
*&----宏定义
*&---------------------------------------------------------------------*
DEFINE ini_fieldcat.
  CLEAR gs_fcat.
  gs_fcat-fieldname  = &1. "字段名
  gs_fcat-COLTEXT    = &2. "列名称
  gs_fcat-seltext    = &2. "列标识
  gs_fcat-ref_table  = &3. "参考表
  gs_fcat-ref_field  = &4. "参考字段
  gs_fcat-hotspot    = &5. "单击
  APPEND gs_fcat TO gt_fcat.
END-OF-DEFINITION.


*&---------------------------------------------------------------------*
*&----选择屏幕定义
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME .

PARAMETERS:p_ryear TYPE faglflext-ryear DEFAULT sy-datum+0(4) OBLIGATORY,                   "会计年度
           p_month TYPE bkpf-monat  OBLIGATORY.                        "会计期间
SELECT-OPTIONS:
          s_bukrs FOR bkpf-bukrs.

SELECTION-SCREEN END OF BLOCK blk1.

*&---------------------------------------------------------------------*
*&----屏幕事件
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&----主处理过程
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM frm_build_racct."创建会计科目range表
  PERFORM frm_get_data.   "根据选择条件从系统抓取数据
  PERFORM frm_deal_data."汇总
  PERFORM frm_alv_show.   "展示到ALV

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       根据选择条件从系统抓取数据
*----------------------------------------------------------------------*
FORM frm_get_data .
  DATA:lv_cond TYPE hslxx12,
       lv_num  TYPE numc2,
       l_f     TYPE string.
  FIELD-SYMBOLS <fs_tab> TYPE hslxx12.

  SELECT rbukrs
         butxt
         racct
         hsl01
         hsl02
         hsl03
         hsl04
         hsl05
         hsl06
         hsl07
         hsl08
         hsl09
         hsl10
         hsl11
         hsl12
         hsl13
         hsl14
         hsl15
         hsl16
         drcrk
       INTO CORRESPONDING FIELDS OF TABLE gt_data
         FROM faglflext AS f
         JOIN t001 AS t
           ON f~rbukrs = t~bukrs
       WHERE rbukrs IN s_bukrs
         AND racct IN s_racct
         AND ryear = p_ryear
         AND t~spras = sy-langu.

  LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).

    IF p_month = 12.
      lv_num = p_month - 1.
      DO 5 TIMES.
        lv_num = lv_num + 1.
        CONCATENATE '<fs_data>-HSL' lv_num INTO l_f.
        ASSIGN (l_f) TO <fs_tab>.
        lv_cond = lv_cond + <fs_tab>.
        UNASSIGN <fs_tab>.
        CLEAR l_f.
      ENDDO.
    ELSE.
      lv_num = p_month.
      CONCATENATE '<fs_data>-HSL' lv_num INTO l_f.
      ASSIGN (l_f) TO <fs_tab>.
      lv_cond = lv_cond + <fs_tab>.
      UNASSIGN <fs_tab>.
      CLEAR l_f.
    ENDIF.
    IF <fs_data>-racct = '2211020000'.
      IF <fs_data>-drcrk = 'S'.
        <fs_data>-zyfxc = lv_cond.
      ENDIF.
    ELSE.
      IF <fs_data>-drcrk = 'S'.    "借方/贷方标识
*期间内借方
        <fs_data>-zqjnj = lv_cond.
      ELSE.
*期间内贷方
        <fs_data>-zqjnd = lv_cond.
        <fs_data>-zqjnd = 0 - <fs_data>-zqjnd.
      ENDIF.
    ENDIF.
    CLEAR:lv_num.
    CLEAR lv_cond.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_SHOW
*&---------------------------------------------------------------------*
*       展示到ALV
*----------------------------------------------------------------------*
FORM frm_alv_show .

  PERFORM frm_set_field.    "字段设置
  PERFORM frm_set_event.    "设置事件
  PERFORM frm_set_layout.   "布局
  PERFORM frm_alv_display.  "展示

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_FIELD
*&---------------------------------------------------------------------*
*       字段设置
*----------------------------------------------------------------------*
FORM frm_set_field .

  ini_fieldcat  'RBUKRS' '公司代码' '' '' ''.
  ini_fieldcat  'BUTXT' '公司代码名称' '' '' ''.
  ini_fieldcat  'MONTH' '期间' '' '' ''.

  ini_fieldcat  'ZQJNJ' '期间内借方' '' '' ''.
  ini_fieldcat  'ZQJND' '期间内贷方' '' '' ''.
  ini_fieldcat  'MINUS' '借方减贷方' '' '' ''.
  ini_fieldcat  'ZYFXC' '应付职工薪酬' '' '' ''.
  ini_fieldcat  'ZCHAE' '差额' '' '' ''.
  ini_fieldcat  'BELNR' '福利费过账凭证号' 'BKPF' 'BELNR' 'X'.
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
*  gs_setting-edt_cll_cb = 'X'.         "退出可编辑单元格时回调

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
      it_events                = gt_events         "事件
      is_layout_lvc            = gs_layo           "输出格式
      it_fieldcat_lvc          = gt_fcat           "字段输出格式
    TABLES
      t_outtab                 = gt_alv          "输出的内表
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
    WHEN '&CREATE'.
      PERFORM frm_create_document. "创建会计凭证
      rs_selfield-refresh = 'X'. " 更改后刷新
    WHEN '&IC1'.
      IF rs_selfield-fieldname = 'BELNR'.
        PERFORM frm_call_transaction USING rs_selfield.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_RACCT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_build_racct .

  s_racct = VALUE #( ( sign = 'I' option = 'EQ' low = '2211020000' )
                     ( sign = 'I' option = 'EQ' low = '8001030100' )
                     ( sign = 'I' option = 'EQ' low = '8001030200' )
                     ( sign = 'I' option = 'EQ' low = '8001030300' )
                     ( sign = 'I' option = 'EQ' low = '8001030400' )
                     ( sign = 'I' option = 'EQ' low = '8001030500' )
                     ( sign = 'I' option = 'EQ' low = '8001030900' )
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_create_document .

  DATA lt_return     TYPE TABLE OF bapiret2.
  DATA rtype  TYPE char1.
  DATA rtmsg  TYPE bapi_msg.
  DATA belnr   TYPE belnr_d.
  DATA error_flag TYPE c.
  DATA lv_datum TYPE sy-datum.
  DATA lv_name  TYPE ad_namtext.
  DATA ls_documentheader LIKE bapiache09.
  DATA lt_accountgl LIKE TABLE OF bapiacgl09 WITH HEADER LINE.
  DATA lt_extension2 LIKE TABLE OF bapiparex WITH HEADER LINE.
  DATA lt_currencyamount LIKE TABLE OF bapiaccr09 WITH HEADER LINE.
  DATA lt_accountpayable LIKE TABLE OF bapiacap09 WITH HEADER LINE.
  DATA:lt_ztfi1039 TYPE TABLE OF ztfi1039,
       ls_ztfi1039 TYPE ztfi1039.

  DATA ls_zsfi_bseg TYPE zsfi_bseg.

  DATA temp_line TYPE numc10.

  DATA obj_key LIKE  bapiache09-obj_key.
  LOOP AT gt_alv INTO DATA(ls_alv) WHERE sel = 'X'.
    IF ls_alv-zchae <= 0.
      MESSAGE '差额小于或等于0，无法生成凭证' TYPE 'S' DISPLAY LIKE 'E'.
      error_flag = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF sy-subrc NE '0'.
    MESSAGE '请选择一行' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  IF error_flag = 'X'.
    EXIT.
  ENDIF.
  CLEAR:lv_datum,lv_name.
  CALL FUNCTION 'ZIREAD_NAME'
    EXPORTING
      i_uname = sy-uname
    IMPORTING
      o_name  = lv_name
      o_type  = rtype.
  IF rtype = 'E'.
    lv_name = sy-uname.
  ENDIF.

  LOOP AT gt_alv INTO ls_alv WHERE sel = 'X'.
    CLEAR temp_line.
    lv_datum = ls_alv-month && '01'.
    CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
      EXPORTING
        iv_date           = lv_datum
      IMPORTING
        ev_month_end_date = lv_datum.
    ls_documentheader-pstng_date = lv_datum.
    ls_documentheader-doc_date =  lv_datum.
    ls_documentheader-fis_period = lv_datum+4(2).
    ls_documentheader-doc_type =  'SA'.
    ls_documentheader-comp_code =  ls_alv-rbukrs.
    ls_documentheader-ref_doc_no = lv_name. "参照
    ls_documentheader-username = sy-uname.

    temp_line = temp_line + 1.

    lt_accountgl-itemno_acc = temp_line.

    lt_accountgl-gl_account = '2211020000'.
    lt_accountgl-item_text = '一次性将当月福利费过账'.
    APPEND lt_accountgl.

    CLEAR lt_accountgl.

    lt_currencyamount-itemno_acc = temp_line.
    lt_currencyamount-currency = 'CNY'.
    lt_currencyamount-amt_doccur = ls_alv-zchae.
    APPEND lt_currencyamount.

    CLEAR lt_currencyamount.
    ls_zsfi_bseg-posnr =  temp_line .
    ls_zsfi_bseg-bschl = '40'.
    lt_extension2-structure = 'ZSFI_BSEG'.
    lt_extension2-valuepart1 = ls_zsfi_bseg.
    APPEND lt_extension2.

    CLEAR lt_extension2.

    temp_line = temp_line + 1.

    lt_accountgl-itemno_acc = temp_line.

    lt_accountgl-gl_account = '2211020000'.
    lt_accountgl-item_text = '一次性将当月福利费过账'.
    APPEND lt_accountgl.

    CLEAR lt_accountgl.

    lt_currencyamount-itemno_acc = temp_line.
    lt_currencyamount-currency = 'CNY'.
    lt_currencyamount-amt_doccur = 0 - ls_alv-zchae.
    APPEND lt_currencyamount.

    CLEAR lt_currencyamount.
    ls_zsfi_bseg-posnr =  temp_line .
    ls_zsfi_bseg-bschl = '50'.
    lt_extension2-structure = 'ZSFI_BSEG'.
    lt_extension2-valuepart1 = ls_zsfi_bseg.
    APPEND lt_extension2.

    CLEAR lt_extension2.


    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader = ls_documentheader
      IMPORTING
        obj_key        = obj_key
      TABLES
        accountgl      = lt_accountgl
        accountpayable = lt_accountpayable
        currencyamount = lt_currencyamount
        return         = lt_return
        extension2     = lt_extension2.

    READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'E'.
    IF sy-subrc <> 0.

      belnr = obj_key+0(10).
      rtype = 'S'.

      ls_ztfi1039-gjahr = p_ryear.
      ls_ztfi1039-monat = p_month.
      ls_ztfi1039-belnr = belnr.
      ls_ztfi1039-bukrs = ls_alv-rbukrs.
      ls_ztfi1039-zminus = ls_alv-minus.
      ls_ztfi1039-zchae = ls_alv-zchae.
      ls_ztfi1039-cdate = sy-datum.
      ls_ztfi1039-ctime = sy-uzeit.
      ls_ztfi1039-cname = sy-uname.
      MODIFY ztfi1039 FROM ls_ztfi1039.
      CLEAR ls_ztfi1039.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.



    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      LOOP AT lt_return INTO ls_return WHERE type = 'E'.
        rtmsg = rtmsg && ls_return-message && '；'.
      ENDLOOP.

      rtype = 'E'.

    ENDIF.

    IF rtype = 'E'.
      MESSAGE rtmsg TYPE 'S' DISPLAY LIKE 'E'.
      error_flag = 'X'.
      EXIT.
    ELSE.

      ls_alv-belnr = belnr.
      MODIFY gt_alv FROM ls_alv.
    ENDIF.
    CLEAR:ls_alv.
  ENDLOOP.
  IF error_flag IS INITIAL.
    MESSAGE '生成凭证成功' TYPE 'S'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_COLLECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_deal_data .
  DATA:ls_alv LIKE LINE OF gt_alv.
  DATA:lv_month  TYPE char6,
       lv_length TYPE i.
  "期间赋值
  CLEAR:lv_month,lv_length.
  lv_length = strlen( p_month ).
  IF lv_length = 1.
    lv_month = p_ryear && '0' && p_month.
  ELSE.
    lv_month = p_ryear && p_month.
  ENDIF.
  SORT gt_data BY rbukrs.

  IF gt_data IS NOT INITIAL.
    SELECT gjahr ,
           monat ,
           bukrs ,
           belnr
    INTO TABLE @DATA(lt_ztfi1039)
    FROM ztfi1039
    WHERE gjahr = @p_ryear
      AND monat = @p_month
      AND bukrs IN @s_bukrs.
  ENDIF.

  LOOP AT gt_data INTO DATA(ls_data) WHERE racct NE '2211020000' .
    ls_alv-rbukrs = ls_data-rbukrs.
    ls_alv-butxt  = ls_data-butxt.
    ls_alv-month  = lv_month.
    ls_alv-zqjnd  = ls_data-zqjnd.
    ls_alv-zqjnj  = ls_data-zqjnj.
    COLLECT ls_alv INTO gt_alv.
  ENDLOOP.

  LOOP AT gt_alv INTO ls_alv.
    LOOP AT gt_data INTO ls_data WHERE rbukrs = ls_alv-rbukrs AND racct = '2211020000' AND drcrk = 'S'.
      ls_alv-zyfxc = ls_data-zyfxc + ls_alv-zyfxc. "应付薪酬
    ENDLOOP.

    READ TABLE lt_ztfi1039 INTO DATA(ls_ztfi1039) WITH KEY bukrs = ls_alv-rbukrs.
    IF sy-subrc = 0.
      ls_alv-belnr = ls_ztfi1039-belnr.
    ENDIF.
    ls_alv-minus = ls_alv-zqjnj - ls_alv-zqjnd.
    ls_alv-zchae = ls_alv-minus - ls_alv-zyfxc.
    MODIFY gt_alv FROM ls_alv.
    CLEAR:ls_alv,ls_data.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_EVENT
*&---------------------------------------------------------------------*
*       设置事件
*----------------------------------------------------------------------*
FORM frm_set_event .
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = gt_events.

  READ TABLE gt_events INTO DATA(ls_events) WITH KEY name = slis_ev_top_of_page.
  IF sy-subrc = 0.
    ls_events-form = 'ALV_TOP_OF_PAGE'.
    MODIFY gt_events FROM ls_events INDEX sy-tabix.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  ALV_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       ALV标题设置
*----------------------------------------------------------------------*
FORM alv_top_of_page .
  DATA: lt_listcomm TYPE slis_t_listheader,   "保存ALV表标题
        ls_listcomm LIKE LINE OF lt_listcomm.
  DATA: l_m1(50),
        l_m2(50).

  REFRESH lt_listcomm.

  CLEAR ls_listcomm.
  ls_listcomm-typ = 'H'.
  ls_listcomm-key = ''.
  ls_listcomm-info = '月末“福利费”一次性过账'.
  APPEND ls_listcomm TO lt_listcomm.

  CLEAR ls_listcomm.
  ls_listcomm-typ = 'S'.
  ls_listcomm-key = ''.
  CONCATENATE '期间：' p_ryear '-' p_month INTO ls_listcomm-info." ls_listcomm-info.
  APPEND ls_listcomm TO lt_listcomm.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_listcomm.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       跳转FB03
*----------------------------------------------------------------------*
FORM frm_call_transaction  USING    p_selfield TYPE slis_selfield.
  READ TABLE gt_alv INTO DATA(ls_alv) INDEX p_selfield-tabindex.
  IF ls_alv-belnr NE ''.
    SET PARAMETER ID 'BLN' FIELD ls_alv-belnr.
    SET PARAMETER ID 'BUK' FIELD ls_alv-rbukrs.
    SET PARAMETER ID 'GJR' FIELD ls_alv-month+0(4).
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.
