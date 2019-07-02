*$*$********************************************************************
* Program ID/Name:ZPPR005                 Date written:2013/7/15
* Author's name: HP_FCG                   Last update:2013/8/5
* Program title:物料主数据批量维护
* Project Name:  EPR I
* Version:V0
* Function Spec ID:PP_01_05
*----------------------------------------------------------------------*
* Description: (Incl. Related Function Area and System)
*              用户维护好Excel文件后复制到txt文件，再上载到SAP，
*              程序按视图批量导入物料主数据
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

report  zppr005 message-id zpp_msg.
*$*$********************************************************************
*$*$    TABLES                                                         *
*$*$********************************************************************
tables:marc.
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
constants: c_fieldx     type c value 'X',
           c_bapi_mara  type te_struc value 'BAPI_TE_MARA',
           c_bapi_marax type te_struc value 'BAPI_TE_MARAX',
           c_bapi_marc  type te_struc value 'BAPI_TE_MARC',
           c_bapi_marcx type te_struc value 'BAPI_TE_MARCX',
           c_sline      type c value '/',
           c_country(2) type c value 'CN',
           c_taxtype(4) type c value 'MWST',
           cns_qpart_01     TYPE QPART VALUE '01',
           cns_qpart_04     TYPE QPART VALUE '04',
           cns_qpart_06     TYPE QPART VALUE '06',
           cns_qpart_08     TYPE QPART VALUE '08',
           cns_qpart_13     TYPE QPART VALUE '13',
           cns_qpart_80     TYPE QPART VALUE '80',
           cns_qpart_81     TYPE QPART VALUE '81',
           cns_qpart_82     TYPE QPART VALUE '82',
           cns_qpart_83     TYPE QPART VALUE '83',
           cns_qpart_84     TYPE QPART VALUE '84',
           cns_qpart_90     TYPE QPART VALUE '90'.
*$*$*********************************************y**********************
*
*$*$    GLOBAL ELEMENTARY VARIABLES                                    *
*$*$********************************************************************
data g_string type string.
*$*$********************************************************************
*$*$    GLOBAL STRUCTURES                                              *
*$*$********************************************************************
****alv structure
data:wa_fieldcat type slis_fieldcat_alv,
       wa_layout type slis_layout_alv.

DATA: g_tabname TYPE dd02v-tabname ,
      g_ucomm TYPE sy-ucomm .
*      p_qlty    TYPE c      .
*$*$********************************************************************
*$*$    GLOBAL INTERNAL TABLES                                         *
*$*$********************************************************************
*DATA:BEGIN OF IT_DATA OCCURS 0,
*       ITEMNO    LIKE SY-TABIX,   "序号
*       MATNR     LIKE MARA-MATNR,  "物料号码
*        MTART     LIKE MARA-MTART,  "物料类型
*       MAKTXC    LIKE MAKT-MAKTX,  "物料描述(ZH)
*       MAKTXE    LIKE MAKT-MAKTX,  "物料描述(EN)
*       MEINS     LIKE MARA-MEINS,  "基本单位
*       MATKL     LIKE MARA-MATKL,  "物料组
*       EXTWG     LIKE MARA-EXTWG,  "外部物料组
*       BRGEW     LIKE MARA-BRGEW,  "毛重
*       NTGEW     LIKE MARA-NTGEW,  "净重
*       GEWEI     LIKE MARA-GEWEI,  "重量单位
*       Z_CZ      LIKE MARA-Z_CZ,  "材质（材料）
*       Z_ATXLJ   LIKE MARA-Z_ATXLJ,"A特性零件
*       Z_CC      LIKE MARA-Z_CC,   "尺寸
*       Z_JGGY    LIKE MARA-Z_JGGY, "加工工艺
*       WERKS     LIKE MARC-WERKS,    "工厂
*       EKGRP     LIKE MARC-EKGRP,  "采购组
*       KORDB     LIKE MARC-KORDB,  "源清单
*       DISMM     LIKE MARC-DISMM,   "MRP类型
*       MINBE     LIKE MARC-MINBE,   "再订货点
*       DISPO     LIKE MARC-DISPO,  "MRP控制者
*       DISGR     LIKE MARC-DISGR,   "MRP组
*       DISLS     LIKE MARC-DISLS,   "批量大小
*       BSTFE     LIKE MARC-BSTFE,   "固定批量
*       BSTMI     LIKE MARC-BSTMI,   "最小批量大小
*       BSTMA     LIKE MARC-BSTMA,   "最大批量大小
*       MABST     LIKE MARC-MABST, "最大库存水平 "added 2013/8/6
*       BSTRF     LIKE MARC-BSTRF,   "舍入值
*       BESKZ     LIKE MARC-BESKZ,   "采购类型
*       SOBSL     LIKE MARC-SOBSL,   "特殊采购类型
*       Z_ZSWL    LIKE MARA-Z_ZSWL, "转手物料
*       LGPRO     LIKE MARC-LGPRO,   "生产仓储地点
*       FHORI     LIKE MARC-FHORI,   "计划边际码
*       DZEIT     LIKE MARC-DZEIT,   "内部自制时间
*       PLIFZ     LIKE MARC-PLIFZ,   "计划交货时间
*       WEBAZ     LIKE MARC-WEBAZ,   "收货处理时间
*       MRPPP     LIKE MARC-MRPPP,  "计划日历"added 2013/8/5
*       EISBE     LIKE MARC-EISBE,   "安全库存
*       SHZET     LIKE MARC-SHZET,   "安全时间
*       MAGRV     LIKE MARA-MAGRV,   "物料组的包装物料
*       RMATP     LIKE MARA-RMATP,   "包装的参考物料
*       Z_NMQT    LIKE MARA-Z_NMQT,   "标准包装数
*       Z_BXQT    LIKE MARA-Z_BXQT,   "包装安全库存
*       Z_JZKX     LIKE MARA-Z_JZKX, "基准开箱数
*       Z_GYL     LIKE MARA-Z_GYL,  "过溢量
*       Z_SYCX    LIKE MARA-Z_SYCX, "所用车型
*       ALTSL     LIKE MARC-ALTSL,   "BOM选择方法
*       SAUFT     LIKE MARC-SAUFT,   "标识：重复制造允许
*       SFEPR     LIKE MARC-SFEPR,   "重复制造参数文件
*       BKLAS     LIKE MBEW-BKLAS,   "评估类
*       VPRSV     LIKE MBEW-VPRSV,   "价格控制
*       PEINH     LIKE MBEW-PEINH,   "价格单位
*       STPRS     LIKE MBEW-STPRS,   "标准价格
*       EKALR     LIKE MBEW-EKALR,   "用QS的成本估算
*       HKMAT     LIKE MBEW-HKMAT,   "物料来源
*       HRKFT     LIKE MBEW-HRKFT,   "原始组
*       NCOST     LIKE MARC-NCOST,   "无成本核算
*       AWSLS     LIKE MARC-AWSLS,   "差异码
*       PRCTR     LIKE MARC-PRCTR,   "利润中心
*       LOSGR     LIKE MARC-LOSGR,  "成本核算批量
*       ZPLP1     LIKE MBEW-ZPLP1,   "计划价格1
*       ZPLD1     LIKE MBEW-ZPLD1,  "计划价格日期1
*       ZPLP2     LIKE MBEW-ZPLP2,   "计划价格2
*       ZPLD2     LIKE MBEW-ZPLD2,   "计划价格日期2
*END OF IT_DATA.
*
*DATA:BEGIN OF IT_SALE OCCURS 0,
*      ITEMNO  LIKE SY-TABIX,   "序号
*       MATNR   LIKE MARC-MATNR,  "物料号码
*       MTART   LIKE MARA-MTART,  "物料类型
*       MAKTXC  LIKE MAKT-MAKTX,  "物料描述(ZH)
*       MAKTXE  LIKE MAKT-MAKTX,  "物料描述(EN)
*       WERKS   LIKE MARC-WERKS,  "工厂
*       VKORG   LIKE MVKE-VKORG,  "销售组织
*       VTWEG   LIKE MVKE-VTWEG,  "分发渠道
*       SPART   LIKE MARA-SPART,  "产品组
*       TAXKM   LIKE TSKM-TAXKM,  "税金分类
*       MTPOS   LIKE MVKE-MTPOS,  "项目类别组
*       KTGRM   LIKE MVKE-KTGRM,  "科目设置组
*       VERSG   LIKE MVKE-VERSG,  "物料统计组
*       TRAGR   LIKE MARA-TRAGR,  "运输组
*       LADGR   LIKE MARC-LADGR,  "装载组
*END OF IT_SALE.

data:begin of it_data occurs 0,
       itemno       like sy-tabix,   "序号
       matnr(18)    type c,  "物料号码
     	 mtart(4)     type c,  "物料类型
       maktxc(40)   type c,  "物料描述(ZH)
       maktxe(40)   type c,  "物料描述(EN)
       meins(3)     type c,  "基本单位
       matkl(9)     type c,  "物料组
       extwg(18)    type c,  "外部物料组
       brgew(13)    type c,  "毛重
       ntgew(13)    type c,  "净重
       gewei(3)     type c,  "重量单位
       z_cz(10)     type c,  "材质（材料）
       z_atxlj(10)  type c,  "A特性零件
       z_cc(10)     type c,  "尺寸
       z_jggy(10)   type c,  "加工工艺
       werks(4)     type c,  "工厂
       ekgrp(3)     type c,  "采购组
       kordb(1)     type c,  "源清单
       dismm(2)     type c,  "MRP类型
       minbe(13)    type c,	 "再订货点
       dispo(3)     type c,  "MRP控制者
       disgr(4)     type c,  "MRP组
       disls(2)     type c,  "批量大小
       bstfe(13)    type c,	 "固定批量
       bstmi(13)    type c,	 "最小批量大小
       bstma(13)    type c,	 "最大批量大小
       mabst(13)    type c,  "最大库存水平 "added 2013/8/6
       bstrf(13)    type c,	 "舍入值
       beskz(1)     type c,  "采购类型
       sobsl(2)     type c,  "特殊采购类型
       z_zswl(1)    type c,  "转手物料
       z_mpj(1)     type c,  "毛坯件标识
       lgpro(4)     type c,  "生产仓储地点
       fhori(3)     type c,  "计划边际码
       dzeit(3)     type c,  "内部自制时间
       plifz(3)     type c,  "计划交货时间
       webaz(3)     type c,  "收货处理时间
       mrppp(3)     type c,  "计划日历"added 2013/8/5
       eisbe(13)    type c,	 "安全库存
       shzet(2)     type c,  "安全时间
       magrv(4)     type c,  "物料组的包装物料
       rmatp(18)    type c,	 "包装的参考物料
       z_bxty(18)   type c,  "箱型
       z_nmqt(13)   type c,  "标准包装数
       z_bxqt(13)   type c,  "包装安全库存
       z_jzkx(13)	  type c,  "基准开箱数
       z_gyl(13)    type c,  "过溢量
       z_sycx(200)  type c,  "所用车型
       altsl(1)     type c,   "BOM选择方法
       sauft(1)     type c,   "标识：重复制造允许
       sfepr(4)     type c,   "重复制造参数文件
       bklas(4)     type c,   "评估类
       vprsv(1)     type c,   "价格控制
       peinh(5)     type c,   "价格单位
       stprs(11)    type c,	 "标准价格
       ekalr(1)     type c,   "用QS的成本估算
       hkmat(1)     type c,   "物料来源
       hrkft(4)     type c,   "原始组
       ncost(1)     type c,   "无成本核算
       awsls(6)     type c,   "差异码
       prctr(10)    type c,	 "利润中心
       losgr(13)    type c,  "成本核算批量
       zplp1(11)    type c,	 "计划价格1
       zpld1(8)     type c,  "计划价格日期1
       zplp2(11)    type c,	 "计划价格2
       zpld2(8)     type c,   "计划价格日期2
end of it_data.

data:begin of it_sale occurs 0,
       itemno      like sy-tabix,   "序号
       matnr(18)   type c,  "物料号码
       mtart(4)    type c,  "物料类型
       maktxc(40)  type c,  "物料描述(ZH)
       maktxe(40)  type c,  "物料描述(EN)
       werks(4)    type c,  "工厂
       vkorg(4)    type c,  "销售组织
       vtweg(2)    type c,  "分发渠道
       spart(2)    type c,  "产品组
       taxkm(1)    type c,  "税金分类
       mtpos(4)    type c,  "项目类别组
       ktgrm(2)    type c,  "科目设置组
       versg(1)    type c,  "物料统计组
       tragr(4)    type c,  "运输组
       ladgr(4)    type c,  "装载组
end of it_sale.

DATA:BEGIN OF it_quality_upload OCCURS 0 ,
       itemno      LIKE sy-tabix ,  " 序号
       matnr(18)   TYPE c ,         " 物料号码
       mtart(4)    type c,  "物料类型
       maktxc(40)  type c,  "物料描述(ZH)
       werks(4)    type c,  "工厂
       qmata(6)    TYPE c,  " 检验业务分类
       qmpur(1)    type c , " 采购激活
       ssqss(8)    type c , " QM控制码
       art(8)      type c , " 检验类型
       apa(1)      type c , " 首选检验
       aktiv(1)    type c, " 检验类型激活
     END OF it_quality_upload .
DATA:BEGIN OF it_quality OCCURS 0 ,
       matnr(18)   TYPE c ,         " 物料号码
       mtart(4)    type c,  "物料类型
       werks(4)    type c,  "工厂
       maktx(40)  type c,  "物料描述(ZH)
       qmata(6)    TYPE c,  " 检验业务分类
       qmpur(1)    type c , " 采购激活
       ssqss(8)    type c , " QM控制码
       art(8)      type c , " 检验类型
       apa(1)      type c , " 首选检验
       aktiv(1)    type c, " 检验类型激活
       itemno      LIKE sy-tabix ,  " 序号
     END OF it_quality .


data:begin of it_out occurs 0,
  itemno  like sy-tabix,   "序号
  matnr   like marc-matnr,    "物料号码
  maktxc  like makt-maktx,    "物料描述(ZH)
  maktxe  like makt-maktx,    "物料描述(ZH)
  werks   like marc-werks,    "工厂代码
  vkorg   like mvke-vkorg,    "销售组织
  vtweg   like mvke-vtweg,    "分发渠道
  art     like qmat-art ,     "检验类型
  msgty   like bapie1ret2-type, "消息类型
  message like bapie1ret2-message,  " 执行结果消息
end of it_out.
*****alv field category.
data:it_fieldcat type slis_t_fieldcat_alv.
*$*$********************************************************************
*$*$    MACROS                                                         *
*$*$********************************************************************
define  check_mandtory.

  if &1 is initial.
    it_out-msgty = 'E'.
    message e016 with &2
 into it_out-message.
    append it_out.
    clear it_out.
    continue.
  endif.

end-of-definition.

define set_fieldx.
  clear g_string.
  g_string = &1.
  if g_string ne c_sline.
    &2 = c_fieldx."基本单位
  elseif g_string = c_sline.
    clear &1.
  endif.
end-of-definition.
define set_view_string.
  if &1 is initial.
    concatenate &2 &1  into &1 in character mode.
  else.
    concatenate &2 &1  into &1 separated by '/'
    in character mode.
  endif.
end-of-definition.

*$*$********************************************************************
*$*$    GLOBAL FIELD-SYMBOLS                                           *
*$*$********************************************************************

*$*$********************************************************************
*$*$    CLASSES                                                        *
*$*$********************************************************************

*$*$********************************************************************
*$*$    PARAMETERS & SELECT-OPTIONS                                    *
*$*$********************************************************************
SELECTION-SCREEN BEGIN OF SCREEN 100 AS SUBSCREEN .
selection-screen begin of block b1 with frame title text-001.
parameters: p_file    type string ,
            p_basic   as checkbox,
            p_sale    as checkbox,
            p_purch   as checkbox,
            p_mrp     as checkbox,
            p_finan   as checkbox,
            p_maktl   as checkbox,
            p_qlty    as checkbox .   " hp_dxj 20150409 注释
selection-screen end of block b1.
SELECTION-SCREEN END OF SCREEN 100 .

SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN .
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-044 .
PARAMETERS:
           pr_bx  AS CHECKBOX DEFAULT 'X' .
SELECTION-SCREEN END OF BLOCK b2 .
SELECTION-SCREEN END OF SCREEN 200 .

SELECTION-SCREEN BEGIN OF SCREEN 300 AS SUBSCREEN .
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-045 .
PARAMETERS:
           pr_extwg  AS CHECKBOX DEFAULT 'X' .
SELECTION-SCREEN END OF BLOCK b3 .
SELECTION-SCREEN END OF SCREEN 300 .

SELECTION-SCREEN: BEGIN OF TABBED BLOCK mytab FOR 10 LINES,
                  TAB (20) button1 USER-COMMAND push1,
                  TAB (20) button2 USER-COMMAND push2,
                  TAB (20) button3 USER-COMMAND push3,
                 END OF BLOCK mytab.
*$*$********************************************************************
*$*$    INITIALIZATION                                                 *
*$*$********************************************************************
INITIALIZATION .
  button1 = text-046.
  button2 = text-047.
  button3 = text-048 .
  mytab-prog = sy-repid.
  IF mytab-dynnr IS INITIAL .
*    g_dynrr     = sy-dynnr  .
    mytab-dynnr = 100.
  ENDIF.


*$*$********************************************************************
*$*$    AT SELECTION-SCREEN ON                                         *
*$*$********************************************************************
at selection-screen on value-request for p_file.
  perform frm_choose_input_file .
*$*$********************************************************************
*$*$    AT SELECTION-SCREEN OUTPUT                                     *
*$*$********************************************************************
AT SELECTION-SCREEN OUTPUT .

  IF mytab-dynnr = 100 .
    LOOP AT SCREEN .
      IF SCREEN-NAME = 'P_FILE'.
        SCREEN-REQUIRED = '2' .
        MODIFY SCREEN .
      ENDIF.
    ENDLOOP.
  ENDIF.

  CASE g_ucomm.
    WHEN 'PUSH2'.
      AUTHORITY-CHECK OBJECT 'Z_BXTYPE'
               ID 'ZBXTYPE' FIELD '1'.
      IF SY-SUBRC <> 0.
* Implement a suitable exception handling here
        mytab-dynnr = 100 .
        mytab-activetab = 'PUSH1'.
        message S368(00) with '无维护箱型的权限！' DISPLAY LIKE 'E'.
        STOP .
      ENDIF.

    WHEN 'PUSH3'.
      AUTHORITY-CHECK OBJECT 'Z_EXTGROUP'
               ID 'Z_EXTGROUP' FIELD '1'.
      IF SY-SUBRC <> 0.
*   Implement a suitable exception handling here
        mytab-dynnr = 100 .
        mytab-activetab = 'PUSH1'.
        message S368(00) with '无维护外部物料组的权限！' DISPLAY LIKE 'E' .
        STOP .
      ENDIF.
    WHEN OTHERS.
  ENDCASE.


*$*$********************************************************************
*$*$    AT SELECTION-SCREEN                                            *
*$*$********************************************************************
at selection-screen .
  g_ucomm = sy-ucomm .
*  CLEAR sy-ucomm .
  CASE g_ucomm.
    WHEN 'PUSH1'.
      mytab-dynnr = 100 .
      mytab-activetab = 'PUSH1' .
    WHEN 'PUSH2'.
      mytab-dynnr = 200 .
      mytab-activetab = 'PUSH2' .
      g_tabname = 'ZTMMBXTY' .
    WHEN 'PUSH3' .
      mytab-dynnr = 300 .
      mytab-activetab = 'PUSH3' .
      g_tabname = 'ZV_TWEW' .
    WHEN OTHERS.
  ENDCASE.

*$*$********************************************************************
*$*$    START-OF-SELECTION                                             *
*$*$********************************************************************
start-of-selection.

*  屏幕100，维护物料的视图
  IF mytab-dynnr = 100 AND g_ucomm = 'ONLI' .
    perform frm_checkbox_select.
    perform frm_auth_check.
    perform frm_upload_data. " Upload data from Excel file..
    perform frm_update_master.  " Call BAPI to maintain material master
    perform frm_display_data. "Display log data.
  ELSE .
    IF pr_bx = 'X' AND mytab-dynnr = 200 .
*      AUTHORITY-CHECK OBJECT 'Z_BXTYPE'
*               ID 'ZBXTYPE' FIELD '1'.
*      IF SY-SUBRC <> 0.
** Implement a suitable exception handling here
*        message e368(00) with '无维护箱型的权限！' .
*      ENDIF.
      PERFORM frm_maintenance_table_view USING g_tabname .

    ENDIF .

    IF pr_extwg = 'X' AND mytab-dynnr = 300 .
*        AUTHORITY-CHECK OBJECT 'Z_EXTGROUP'
*                 ID 'Z_EXTGROUP' FIELD '1'.
*        IF SY-SUBRC <> 0.
**   Implement a suitable exception handling here
*          message e368(00) with '无维护外部物料组的权限！' .
*        ENDIF.
      PERFORM frm_maintenance_table_view USING g_tabname .

    ENDIF.

  ENDIF.


*$*$********************************************************************
*$*$    END-OF-SELECTION                                               *
*$*$********************************************************************

*&---------------------------------------------------------------------*
*&      Form  FRM_CHOOSE_INPUT_FILE
*&---------------------------------------------------------------------*
*       chose the input file path
*----------------------------------------------------------------------*

form frm_choose_input_file .

  data: i_fname type string,
        it_l_filetable type table of file_table,
        i_rc type i,
        i_title type string,
        i_action type i.
  i_title = text-002.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = i_title
      default_filename        = '.txt'
      file_filter             = '*.*'
    CHANGING
      file_table              = it_l_filetable
      rc                      = i_rc
      user_action             = i_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      others                  = 5.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    clear i_fname.
  else.
    if i_action = 0.
      read table it_l_filetable index 1 into i_fname.
    else.
      clear i_fname.
    endif.
  endif.
  p_file = i_fname.

endform.                    " FRM_CHOOSE_INPUT_FILE
*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*        Upload data from Excel file..
*----------------------------------------------------------------------*

form frm_upload_data .

  IF p_file IS INITIAL .
    SET CURSOR FIELD 'P_FILE' .
    MESSAGE s055(00) DISPLAY LIKE 'E'  .
    STOP .
  ENDIF.

  if p_sale = abap_true."上传销售视图
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = p_file
        filetype                = 'ASC'
        has_field_separator     = 'X'
*       codepage                = '8400'
*       DAT_MODE                = 'X'
      TABLES
        data_tab                = it_sale
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 11
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        others                  = 17.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
             with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  elseif p_qlty = 'X'.   "质量管理
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = p_file
        filetype                = 'ASC'
        has_field_separator     = 'X'
*       codepage                = '8400'
*       DAT_MODE                = 'X'
      TABLES
        data_tab                = it_quality_upload
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 11
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        others                  = 17.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
             with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  ELSE. " 上传其他视图
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = p_file
        filetype                = 'ASC'
        has_field_separator     = 'X'
*       codepage                = '8400'
*       DAT_MODE                = 'X'
      TABLES
        data_tab                = it_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 11
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        others                  = 17.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
             with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.




**********检查物料组权限
******E:采购    K：基本数据
    data:lwa_mara type mara.

    authority-check object 'Z_MATGROUP'
                      id 'ZMATKL' field 'X'.
    if sy-subrc = 0 and p_basic = 'X'.
      message s368(00) with '你只能更新物料组！' display like 'E'.
      leave list-processing.
    endif.


    if p_basic = abap_true.""""""更新基本试图时

    elseif p_maktl = 'X'.
      authority-check object 'Z_MATGROUP'
                        id 'ZMATKL' field 'X'.
      if sy-subrc <> 0.
        message s368(00) with '无更新物料组的权限！' display like 'E'.
        leave list-processing.
      endif.

    endif.

  endif.

endform.                    " FRM_UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_UPDATE_MASTER
*&---------------------------------------------------------------------*
*       Call BAPI to maintain material master data.
*----------------------------------------------------------------------*

form frm_update_master .

  data:lwa_mara type mara.

  data:it_l_return     type standard table of bapi_matreturn2,
       it_l_desc       type standard table of bapi_makt,
       it_l_taxcal     type standard table of bapi_mlan,
       it_l_ext        type standard table of bapiparex,
       it_l_extx       type standard table of bapiparexx,
         wa_l_return   type bapi_matreturn2,
         wa_l_head     type bapimathead,
         wa_l_client   type bapi_mara,
         wa_l_clientx  type bapi_marax,
         wa_l_sale     type bapi_mvke,
         wa_l_salex    type bapi_mvkex,
         wa_l_plant    type bapi_marc,
         wa_l_plantx   type bapi_marcx,
         wa_l_val      type bapi_mbew,
         wa_l_valx     type bapi_mbewx,
         wa_l_desc     type bapi_makt,
         wa_l_taxcal   type bapi_mlan,
         wa_l_ext      type bapiparex,
         wa_l_extx     type bapiparexx.
  data:i_matnr like marc-matnr,
       i_mmsta like marc-mmsta,
       i_pstat like marc-pstat,
       i_status type c,
       i_string type string.

*  hp_dxj 20150324
  IF p_qlty = abap_true .

    PERFORM frm_maintenance_quality .

  ENDIF.

  if p_sale = abap_true.
    set_view_string i_string text-003.

    loop at it_sale.
*****data checking and fitering....

***convert material number...
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = it_sale-matnr
        IMPORTING
          output       = it_sale-matnr
        EXCEPTIONS
          length_error = 1
          others       = 2.
      translate it_sale-matnr to upper case. " 物料号码

      move-corresponding it_sale to it_out.
**1.检查基本视图是否存在

      if p_basic is initial.
        clear i_matnr.
        select single matnr into i_matnr
        from mara
        where matnr = it_out-matnr.
        if i_matnr is initial.
          it_out-msgty = 'E'.
          message e015 with it_out-matnr
          into it_out-message.
          append it_out.
          clear it_out.
          continue.
        endif.
      endif.
***2.检查必输字段..... " 销售视图的必输字段检查.....
      check_mandtory it_sale-matnr text-007. " 物料号码.
*      CHECK_MANDTORY IT_SALE-MTART TEXT-008. " 物料类型.
*      CHECK_MANDTORY IT_SALE-MAKTXC TEXT-009. "物料描述(ZH)
*      CHECK_MANDTORY IT_SALE-MAKTXE TEXT-010."物料描述(EN)
      check_mandtory it_sale-vkorg  text-011."  销售组织
      check_mandtory it_sale-vtweg  text-012."  分发渠道
      check_mandtory it_sale-spart text-013."  产品组
      check_mandtory it_sale-taxkm text-014."  税金分类
      check_mandtory it_sale-mtpos text-015."  项目类别组
*      CHECK_MANDTORY IT_SALE-KTGRM TEXT-016."  科目设置组
      translate it_sale-vkorg to upper case.
      translate it_sale-werks to upper case.

      wa_l_head-material = it_sale-matnr.
      wa_l_head-ind_sector = 'M'.
      wa_l_head-matl_type = it_sale-mtart.
***** 视图更新标识.

      wa_l_head-sales_view = abap_true.
*      WA_L_HEAD-BASIC_VIEW = ABAP_TRUE.
      wa_l_plant-plant = it_sale-werks.  "工厂
      wa_l_plant-loadinggrp = it_sale-ladgr.  "装载组
****** hp_dxj 20140822  start 工厂的物料状态
      select single mmsta into i_mmsta from marc
                          where matnr = it_sale-matnr
                           and werks = it_sale-werks.
****** hp_dxj 20140822  end 工厂的物料状态
      if sy-subrc ne 0.
        wa_l_plant-pur_status = '01'.    "工厂特定的物料状态
        wa_l_plantx-pur_status = c_fieldx.    "工厂特定的物料状态
      endif.


      wa_l_client-division = it_sale-spart.  "产品组
      wa_l_client-trans_grp = it_sale-tragr.  "运输组
      wa_l_sale-sales_org =  it_sale-vkorg.  "销售组织
      wa_l_sale-distr_chan = it_sale-vtweg.  "分发渠道
      wa_l_sale-item_cat = it_sale-mtpos.  "项目类别组
      wa_l_sale-acct_assgt = it_sale-ktgrm.  "科目设置组
      wa_l_sale-matl_stats = it_sale-versg.  "物料统计组
      wa_l_plant-availcheck = 'KP'.

      if it_sale-taxkm ne c_sline.
        clear wa_l_taxcal.
        wa_l_taxcal-depcountry = c_country.
        wa_l_taxcal-tax_type_1 = c_taxtype.
        wa_l_taxcal-taxclass_1 = it_sale-taxkm.  "税金分类
        append wa_l_taxcal to it_l_taxcal.
      endif.

      wa_l_plantx-plant = it_sale-werks.  "工厂
      wa_l_salex-sales_org = it_sale-vkorg ."销售组织
      wa_l_salex-distr_chan = it_sale-vtweg ."分发渠道
      wa_l_plantx-availcheck = c_fieldx.
      set_fieldx it_sale-ladgr wa_l_plantx-loadinggrp ."装载组
      set_fieldx it_sale-spart wa_l_clientx-division ."产品组
      set_fieldx it_sale-tragr wa_l_clientx-trans_grp."运输组

      set_fieldx it_sale-mtpos wa_l_salex-item_cat."项目类别组
      set_fieldx it_sale-ktgrm wa_l_salex-acct_assgt."科目设置组
      set_fieldx it_sale-versg wa_l_salex-matl_stats."物料统计组

*      CLEAR WA_L_DESC.
*      WA_L_DESC-LANGU = '1'.
*      WA_L_DESC-MATL_DESC = IT_SALE-MAKTXC.
*      APPEND WA_L_DESC TO IT_L_DESC.
*
*      CLEAR WA_L_DESC.
*      WA_L_DESC-LANGU = 'E'.
*      WA_L_DESC-MATL_DESC = IT_SALE-MAKTXE.
*      APPEND WA_L_DESC TO IT_L_DESC.



      clear:wa_l_return,it_l_return[].
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata            = wa_l_head
          clientdata          = wa_l_client
          clientdatax         = wa_l_clientx
          plantdata           = wa_l_plant
          plantdatax          = wa_l_plantx
          salesdata           = wa_l_sale
          salesdatax          = wa_l_salex
        TABLES
*         MATERIALDESCRIPTION = IT_L_DESC
          taxclassifications  = it_l_taxcal
          returnmessages      = it_l_return.

      loop at it_l_return into wa_l_return where type = 'E'
                                                   or type = 'A'.
        if it_out-message is initial.
          it_out-msgty = 'E'.
          it_out-message = wa_l_return-message.
          exit.
        endif.
      endloop.
      if it_out-message is initial.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        it_out-msgty = 'S'.
        message e021 with it_out-matnr i_string into  it_out-message.
      endif.

      append it_out.
      clear: it_out,wa_l_head,wa_l_plant,wa_l_plantx,wa_l_client,
            wa_l_clientx,wa_l_sale,wa_l_salex,it_l_desc[],it_l_taxcal[].

    endloop.

  elseif p_qlty <> abap_true .

    if p_basic = abap_true.
      set_view_string i_string text-002.
    endif.

    if p_mrp = abap_true.
      set_view_string i_string text-005.
    endif.

    if p_purch = abap_true.
      set_view_string i_string text-004.
    endif.

    if p_finan = abap_true.
      set_view_string i_string text-006.
    endif.

    loop at it_data.
*****data checking and fitering....
*****转换大写格式



***convert material number...
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = it_data-matnr
        IMPORTING
          output       = it_data-matnr
        EXCEPTIONS
          length_error = 1
          others       = 2.



*********add by hp_xsq
      IF p_basic = 'X'.
        clear:it_out.
        authority-check object 'Z_MATGROUP'
                          id 'ZMATKL' field 'X'.
        if sy-subrc <> 0."""无采购权限
          if it_data-matkl <> '' and it_data-matkl <> '99'
            and it_data-matkl <> c_sline.

            it_out-itemno = it_data-itemno.
            it_out-matnr = it_data-matnr.
            it_out-maktxc = it_data-maktxc.
            it_out-maktxe = it_data-maktxe.
            it_out-werks = it_data-werks.
*            it_out-vkorg = it_data-vkorg.
*            it_out-vtweg = it_data-vtweg.
            it_out-msgty = 'E'.
            it_out-message = 'TR只能更新物料组为99'.
            append it_out.

            CONTINUE.
          endif.

          clear:lwa_mara.
          lwa_mara-matnr = it_data-matnr.

          select single * from mara into lwa_mara
            where matnr = lwa_mara-matnr.
          if it_data-matkl = c_sline and sy-subrc = 0.
            it_data-matkl = lwa_mara-matkl.
          elseif sy-subrc <> 0.
            clear lwa_mara.
          else.
          endif.
          if lwa_mara is not initial.
            if lwa_mara-matkl <> it_data-matkl and lwa_mara-matkl <>
            '99'.
              it_out-itemno = it_data-itemno.
              it_out-matnr = it_data-matnr.
              it_out-maktxc = it_data-maktxc.
              it_out-maktxe = it_data-maktxe.
              it_out-werks = it_data-werks.
*            it_out-vkorg = it_data-vkorg.
*            it_out-vtweg = it_data-vtweg.
              it_out-msgty = 'E'.
              it_out-message = '物料组已经被维护为非99的值'.
              append it_out.
              CONTINUE.
            endif.
          endif.

        endif.
      ENDIF.









      translate it_data-matnr to upper case. " 物料号码
      translate it_data-meins to upper case. " 基本单位
      translate it_data-gewei to upper case. "“重量单位”
      translate it_data-dismm to upper case.  "“MRP类型”
      translate it_data-ekgrp to upper case. "“采购组”
      translate it_data-disls to upper case. "“批量大小”、
      translate it_data-beskz to upper case."“采购类型”
*      TRANSLATE IT_DATA-VKORG TO UPPER CASE."采购组织
      translate it_data-werks to upper case."工厂代码.

      move-corresponding it_data to it_out.
**1.检查基本视图是否存在

      if p_basic is initial.
        clear i_matnr.
        select single matnr into i_matnr
        from mara
        where matnr = it_out-matnr.
        if i_matnr is initial.
          it_out-msgty = 'E'.
          message e015 with it_out-matnr
          into it_out-message.
          append it_out.
          clear it_out.
          continue.
        endif.
      endif.

***2.检查必输字段.....

      if p_basic = abap_true. "基本视图的必输字段检查...

        check_mandtory it_data-matnr text-007. " 物料号码.
        check_mandtory it_data-mtart text-008. " 物料类型.
        check_mandtory it_data-maktxc text-009. "物料描述(ZH)
        check_mandtory it_data-maktxe text-010. "物料描述(EN)
        check_mandtory it_data-meins text-017. " 基本单位
        if it_data-mtart = 'ROH'.
          check_mandtory it_data-matkl text-018. " 物料组
        endif.
        check_mandtory it_data-extwg text-019.  "外部物料组
      endif.

      if p_mrp = abap_true. " MRP 视图的必输字段检查.....
        if it_data-rmatp = 'P001' or it_data-rmatp = '/'.
        else.
          it_out-msgty = 'E'.
          message e075  into it_out-message.
          append it_out.
          clear it_out.
          continue.
        endif.

        check_mandtory it_data-disgr text-023. "  MRP组
        check_mandtory it_data-werks text-020.  "工厂
        check_mandtory it_data-dismm text-021. " MRP类型
        if it_data-dismm ne 'ND'.
          check_mandtory it_data-dispo text-022. "  MRP控制者

          check_mandtory it_data-disls text-024. "批量大小
        endif.
        if it_data-disls = 'PB'.
          check_mandtory it_data-mrppp text-036. "计划日历
        endif.

        if it_data-disls = 'HB'.
          check_mandtory it_data-mabst text-037. "最大库存水平.
        endif.
        check_mandtory it_data-beskz text-025. "采购类型
        check_mandtory it_data-fhori text-026. "计划边际码
        check_mandtory it_data-z_mpj text-038. "毛坯件20140508检查毛坯件
      endif.

      if p_purch = abap_true. " 采购视图的必输字段检查.....

        check_mandtory it_data-werks text-020.  "工厂


*        IF IT_DATA-MTART = 'ROH'.
*          CHECK_MANDTORY IT_DATA-EKGRP TEXT-027. "采购组
*        ENDIF.


        if ( not it_data-ekgrp is initial )
                                   and ( not it_data-kordb is initial ).
          if it_data-matkl is initial.
            it_out-msgty = 'E'.
            message e058  into it_out-message.
            append it_out.
            clear it_out.
            continue.
          endif.
        endif.

      endif.


      if p_finan = abap_true. " 财务成本视图的必输字段检查.....

        check_mandtory it_data-werks text-020.  "工厂
        check_mandtory it_data-bklas text-028. "评估类
        check_mandtory it_data-vprsv text-029. "价格控制
        check_mandtory it_data-peinh text-030. " 价格单位
        check_mandtory it_data-ekalr text-031." 用QS的成本估算
        check_mandtory it_data-hkmat text-032." 物料来源
*        20140804 检查差异码  start hp_dxj
*        根据采购类型来判断差异码是否必填
        if it_data-beskz = 'E' or it_data-beskz = 'X'.
          check_mandtory it_data-awsls text-033."差异码
        endif.
        check_mandtory it_data-losgr text-034."成本核算批量
      endif.


**2.header data....
      wa_l_head-material = it_data-matnr.
      wa_l_head-ind_sector = 'M'.
      wa_l_head-matl_type = it_data-mtart.
***** 基本视图数据.....
      clear: wa_l_ext,wa_l_extx.
      wa_l_ext-structure = c_bapi_mara .
      wa_l_ext-valuepart1+0(18) =  it_data-matnr.  "物料号码
      wa_l_extx-structure = c_bapi_marax .
      wa_l_extx-valuepart1+0(18) =  it_data-matnr.  "物料号码
      if p_basic = abap_true.
        wa_l_head-basic_view = abap_true.
        if it_data-meins ne c_sline.
          wa_l_client-base_uom = it_data-meins.  "基本单位
        endif.
        wa_l_client-matl_group = it_data-matkl.  "物料组
        wa_l_client-extmatlgrp = it_data-extwg.  "外部物料组
        if it_data-ntgew is initial.
          wa_l_client-net_weight = '0.001'.  "净重
        else.
          if it_data-ntgew ne c_sline.
            wa_l_client-net_weight = it_data-ntgew.  "净重
          endif.
        endif.
        if  it_data-gewei is initial.
          wa_l_client-unit_of_wt = 'KG'.  "重量单位
        else.
          if it_data-gewei ne c_sline.
            wa_l_client-unit_of_wt = it_data-gewei.  "重量单位
          endif.
        endif.



        wa_l_ext-valuepart1+18(10) =  it_data-z_cz.  "材质（材料）
        wa_l_ext-valuepart1+28(10) =  it_data-z_atxlj."A特性零件
        wa_l_ext-valuepart1+38(10) =  it_data-z_cc.   "尺寸
        wa_l_ext-valuepart1+48(10) =  it_data-z_jggy. "加工工艺

        if it_data-brgew is initial.
          write '0.001' to   "毛重
         wa_l_ext-valuepart1+58(13) no-gap no-grouping
                                     left-justified.
        else.
          if it_data-brgew ne c_sline.
            write it_data-brgew to   "毛重
           wa_l_ext-valuepart1+58(13) no-gap no-grouping
                                       left-justified.
          endif.
        endif.

        set_fieldx it_data-meins wa_l_clientx-base_uom."基本单位

        set_fieldx it_data-matkl wa_l_clientx-matl_group."物料组

        set_fieldx it_data-extwg wa_l_clientx-extmatlgrp ."外部物料组

        set_fieldx it_data-ntgew wa_l_clientx-net_weight ."净重

        set_fieldx it_data-gewei wa_l_clientx-unit_of_wt ."重量单位

        set_fieldx it_data-z_cz wa_l_extx-valuepart1+18(1)."材质（材料）
        "A特性零件
        set_fieldx it_data-z_atxlj wa_l_extx-valuepart1+19(1).

        set_fieldx it_data-z_cc   wa_l_extx-valuepart1+20(1)."尺寸

        set_fieldx it_data-z_jggy wa_l_extx-valuepart1+21(1)."加工工艺

        set_fieldx it_data-brgew wa_l_extx-valuepart1+22(1)."毛重
        if it_data-maktxc ne c_sline.
          clear wa_l_desc.
          wa_l_desc-langu = '1'.
          wa_l_desc-matl_desc = it_data-maktxc.
          append wa_l_desc to it_l_desc.
        endif.
        if it_data-maktxe ne c_sline.
          clear wa_l_desc.
          wa_l_desc-langu = 'E'.
          wa_l_desc-matl_desc = it_data-maktxe.
          append wa_l_desc to it_l_desc.
        endif.

        append wa_l_ext to it_l_ext.
        append wa_l_extx to it_l_extx.
      endif.

*****MRP 视图数据
      if p_mrp = abap_true.
        wa_l_head-mrp_view = abap_true.
        clear: wa_l_ext,wa_l_extx.
        wa_l_ext-structure = c_bapi_marc .
        wa_l_ext-valuepart1+0(4) =  it_data-werks.  "物料号码
        wa_l_extx-structure = c_bapi_marcx .
        wa_l_extx-valuepart1+0(4) =  it_data-werks.  "物料号码
        wa_l_plant-plant =  it_data-werks.    "工厂
        wa_l_plant-mrp_type = it_data-dismm.   "MRP类型
        if it_data-minbe ne c_sline.
          wa_l_plant-reorder_pt = it_data-minbe. "再订货点
        endif.
        wa_l_plant-mrp_ctrler = it_data-dispo.  "MRP控制者
        if it_data-disgr = '0010' or  it_data-disgr = '10'.
          wa_l_plant-plan_strgp = space.
          wa_l_plant-mixed_mrp = space.
          wa_l_plantx-plan_strgp = c_fieldx.
          wa_l_plantx-mixed_mrp = c_fieldx.
          if it_data-disgr = '10'.
            concatenate '00' it_data-disgr into it_data-disgr.
            condense it_data-disgr.
          endif.
        elseif it_data-disgr = '0011' or  it_data-disgr = '11'.
*          WA_L_PLANT-MRP_GROUP = '0010'.   "MRP组
          wa_l_plant-plan_strgp = '11'.
          wa_l_plant-mixed_mrp = '2'.
          wa_l_plantx-plan_strgp = c_fieldx.
          wa_l_plantx-mixed_mrp = c_fieldx.
          if it_data-disgr = '11'.
            concatenate '00' it_data-disgr into it_data-disgr.
            condense it_data-disgr.
          endif.
        endif.

        wa_l_plant-mrp_group = it_data-disgr.   "MRP组.

        wa_l_plant-lotsizekey  = it_data-disls. "批量大小
        if it_data-bstfe ne c_sline.
          wa_l_plant-fixed_lot = it_data-bstfe.  	 "固定批量
          endif.
          if it_data-bstmi ne c_sline.
            wa_l_plant-minlotsize = it_data-bstmi.  "最小批量大小
          endif.
          if it_data-bstma ne c_sline.
            wa_l_plant-maxlotsize = it_data-bstma.  "最大批量大小
          endif.
          if it_data-mabst ne c_sline.
            wa_l_plant-max_stock = it_data-mabst. "最大库存水平
          endif.
          if it_data-bstrf ne c_sline.
            if it_data-beskz = 'E'.
              wa_l_plant-round_val = space.
            else.
              wa_l_plant-round_val = it_data-bstrf.   "舍入值
            endif.
          endif.
          wa_l_plant-proc_type = it_data-beskz.   "采购类型
          wa_l_plant-spproctype = it_data-sobsl.  "特殊采购类型

          wa_l_plant-iss_st_loc = it_data-lgpro.  "生产仓储地点
          if it_data-fhori ne c_sline.
            wa_l_plant-sm_key = it_data-fhori.      "计划边际码
          endif.
          if it_data-dzeit ne c_sline.
            wa_l_plant-inhseprodt = it_data-dzeit.   "内部自制时间
          endif.
          if it_data-plifz ne c_sline.
            wa_l_plant-plnd_delry = it_data-plifz.    "计划交货时间
          endif.
          if it_data-webaz ne c_sline.
            wa_l_plant-gr_pr_time = it_data-webaz.    "收货处理时间
          endif.
          wa_l_plant-ppc_pl_cal = it_data-mrppp."计划日历 added 2013/8/5
          if it_data-eisbe ne c_sline.
            wa_l_plant-safety_stk = it_data-eisbe.     "安全库存
          endif.
          if it_data-shzet ne c_sline.
            wa_l_plant-safetytime = it_data-shzet.     "安全时间
            if it_data-shzet is initial.
              wa_l_plant-safty_t_id = space.
            else.
              wa_l_plant-safty_t_id = '2'.
            endif.
            wa_l_plantx-safty_t_id =  c_fieldx.
          endif.
*          IF IT_DATA-ALTSL IS INITIAL.
          wa_l_plant-alt_bom_id = '2'.   "BOM选择方法
*          ELSE.
*            WA_L_PLANT-ALT_BOM_ID = IT_DATA-ALTSL.   "BOM选择方法
*          ENDIF.
*          IF IT_DATA-SAUFT IS INITIAL.
          wa_l_plant-rep_manuf = 'X'.    "标识：重复制造允许
*          ELSE.
*            WA_L_PLANT-REP_MANUF = IT_DATA-SAUFT.    "标识：重复制造允许
*          ENDIF.
*          IF IT_DATA-SFEPR IS INITIAL.
          wa_l_plant-repmanprof = 'Z001'.    "重复制造参数文件
*          ELSE.
*            WA_L_PLANT-REPMANPROF = IT_DATA-SFEPR.    "重复制造参数文件
*          ENDIF.

          wa_l_plant-availcheck = 'KP'.    "可用性检查的检查组

          if it_data-magrv is initial.
            wa_l_client-mat_grp_sm = 'Z002'.   "物料组的包装物料
          else.
            wa_l_client-mat_grp_sm = it_data-magrv.	 "物料组的包装物料
          endif.
            wa_l_client-pl_ref_mat = it_data-rmatp.   "包装的参考物料


            set_fieldx it_data-magrv wa_l_clientx-mat_grp_sm .
            "物料组的包装物料
            set_fieldx it_data-rmatp wa_l_clientx-pl_ref_mat."包装的参考物料
            set_fieldx it_data-z_bxqt wa_l_extx-valuepart1+4(1)."标准包装数
            set_fieldx it_data-z_nmqt wa_l_extx-valuepart1+5(1)."包装安全库存
            set_fieldx it_data-z_jzkx wa_l_extx-valuepart1+6(1)."基准开箱数
            set_fieldx it_data-z_gyl  wa_l_extx-valuepart1+7(1)."过溢量
            set_fieldx it_data-z_sycx wa_l_extx-valuepart1+8(1)."转手物料
            set_fieldx it_data-z_bxty wa_l_extx-valuepart1+9(1)."箱型
            set_fieldx it_data-z_mpj wa_l_extx-valuepart1+10(1)."毛胚件标识
            set_fieldx it_data-z_zswl wa_l_extx-valuepart1+11(1)."所用车型


            write it_data-z_bxqt to   "标准包装数
            wa_l_ext-valuepart1+4(13) no-gap no-grouping left-justified.
            write it_data-z_nmqt to   "包装安全库存
           wa_l_ext-valuepart1+17(13) no-gap no-grouping left-justified.

            write it_data-z_jzkx to   "基准开箱数
           wa_l_ext-valuepart1+30(13) no-gap no-grouping left-justified.
            write it_data-z_gyl to   "过溢量
           wa_l_ext-valuepart1+43(13) no-gap no-grouping left-justified.

            write it_data-z_zswl to   "转手物料
           wa_l_ext-valuepart1+56(1) no-gap no-grouping left-justified.
            write it_data-z_bxty to   "箱型
           wa_l_ext-valuepart1+57(18) no-gap no-grouping left-justified.
            write it_data-z_mpj to   "毛胚件标识
           wa_l_ext-valuepart1+75(1) no-gap no-grouping left-justified.
            write it_data-z_sycx+0(164) to   "所用车型
         wa_l_ext-valuepart1+76(164) no-gap no-grouping left-justified.
            write it_data-z_sycx+164(36) to   "所用车型
          wa_l_ext-valuepart2+0(36) no-gap no-grouping left-justified.
            wa_l_plantx-plant =  it_data-werks.    "工厂
            set_fieldx it_data-dismm wa_l_plantx-mrp_type . "MRP类型
            set_fieldx it_data-minbe wa_l_plantx-reorder_pt ."再订货点
            set_fieldx it_data-dispo wa_l_plantx-mrp_ctrler ."MRP控制者
            set_fieldx it_data-disgr wa_l_plantx-mrp_group.   "MRP组
*            WA_L_PLANTX-MRP_GROUP = C_FIELDX. "MRP组
            set_fieldx it_data-disls wa_l_plantx-lotsizekey . "批量大小
            set_fieldx it_data-bstfe wa_l_plantx-fixed_lot .  "固定批量
            set_fieldx it_data-bstmi wa_l_plantx-minlotsize."最小批量大小
            set_fieldx it_data-bstma wa_l_plantx-maxlotsize."最大批量大小
            set_fieldx it_data-mabst wa_l_plantx-max_stock."最大库存水平
            set_fieldx it_data-bstrf wa_l_plantx-round_val ."舍入值
            set_fieldx it_data-beskz wa_l_plantx-proc_type ."采购类型
            set_fieldx it_data-sobsl wa_l_plantx-spproctype."特殊采购类型
            set_fieldx it_data-lgpro wa_l_plantx-iss_st_loc."生产仓储地点
            set_fieldx it_data-fhori wa_l_plantx-sm_key  .   "计划边际码
            set_fieldx it_data-dzeit wa_l_plantx-inhseprodt."内部自制时间
            set_fieldx it_data-plifz wa_l_plantx-plnd_delry."计划交货时间
            set_fieldx it_data-webaz wa_l_plantx-gr_pr_time."收货处理时间
            set_fieldx it_data-eisbe wa_l_plantx-safety_stk ."安全库存
            set_fieldx it_data-mrppp wa_l_plantx-ppc_pl_cal ."计划日历
            set_fieldx it_data-shzet wa_l_plantx-safetytime . "安全时间
*          SET_FIELDX IT_DATA-ALTSL WA_L_PLANTX-ALT_BOM_ID .
            wa_l_plantx-alt_bom_id = c_fieldx."BOM选择方法
*          SET_FIELDX IT_DATA-SAUFT WA_L_PLANTX-REP_MANUF ."重复制造允许
*          SET_FIELDX IT_DATA-SFEPR WA_L_PLANTX-REPMANPROF."重复制造参数文件
            wa_l_plantx-rep_manuf = c_fieldx ."重复制造允许
            wa_l_plantx-repmanprof = c_fieldx."重复制造参数文件
            wa_l_plantx-availcheck = c_fieldx.    "可用性检查的检查组
            select single mmsta into i_mmsta from marc
              where matnr = it_data-matnr
               and werks = it_data-werks.
            if sy-subrc ne 0.
              wa_l_plant-pur_status = '01'.    "工厂特定的物料状态
              wa_l_plantx-pur_status = c_fieldx.    "工厂特定的物料状态
            endif.


          endif.

          append wa_l_ext to it_l_ext.
          append wa_l_extx to it_l_extx.

*****采购视图数据
*          IF P_PURCH = ABAP_TRUE AND ( NOT IT_DATA-EKGRP IS INITIAL )
*                                 AND ( NOT IT_DATA-KORDB IS INITIAL ) .
          if p_purch = abap_true and
            ( ( not it_data-ekgrp is initial )
                or ( not it_data-kordb is initial ) ) and it_data-ekgrp
                ne c_sline.
            wa_l_head-purchase_view = abap_true.
            wa_l_plant-plant =  it_data-werks.    "工厂
            wa_l_plant-pur_group = it_data-ekgrp.  "采购组
            wa_l_plant-sourcelist = it_data-kordb.  "源清单

            wa_l_plantx-plant = it_data-werks ."工厂
            set_fieldx it_data-ekgrp wa_l_plantx-pur_group."采购组
            set_fieldx it_data-kordb wa_l_plantx-sourcelist."源清单
            wa_l_head-basic_view = abap_true.
            wa_l_client-matl_group = it_data-matkl.  "物料组
            set_fieldx it_data-matkl wa_l_clientx-matl_group."物料组

          endif.

*****财务成本视图....
          if p_finan = abap_true.
            wa_l_head-account_view = abap_true.
            wa_l_head-cost_view = abap_true.
            wa_l_plant-plant =  it_data-werks.    "工厂
            wa_l_plant-no_costing = it_data-ncost.   "无成本核算
            wa_l_plant-variance_key = it_data-awsls.   "差异码
            wa_l_plant-profit_ctr = it_data-prctr.   "利润中心
            if  it_data-losgr ne c_sline.
              wa_l_plant-lot_size = it_data-losgr.  "成本核算批量
            endif.
            wa_l_val-val_area =  it_data-werks.    "工厂
            wa_l_val-val_class = it_data-bklas.   "评估类
            wa_l_val-price_ctrl = it_data-vprsv.   "价格控制
            if it_data-peinh ne c_sline.
              wa_l_val-price_unit = it_data-peinh.   "价格单位
            endif.
            if it_data-stprs ne c_sline.
              wa_l_val-std_price = it_data-stprs.   "标准价格
            endif.

            wa_l_val-qty_struct = it_data-ekalr.   "用QS的成本估算
            wa_l_val-orig_mat = it_data-hkmat.           "物料来源
            wa_l_val-orig_group = it_data-hrkft.   "原始组
            if it_data-zplp1 ne c_sline.
              wa_l_val-plndprice1 = it_data-zplp1.   "计划价格1
            endif.
            if it_data-zpld1 ne c_sline.
              wa_l_val-plndprdate1 = it_data-zpld1.  "计划价格日期1
            endif.
            if it_data-zplp2 ne c_sline.
              wa_l_val-plndprice2 = it_data-zplp2.   "计划价格2
            endif.
            if it_data-zpld2 ne c_sline.
              wa_l_val-plndprdate2 = it_data-zpld2.   "计划价格日期2
            endif.
            clear:i_status, i_pstat.
            select single mmsta pstat into
              (i_mmsta,i_pstat)
              from marc
              where matnr = it_data-matnr
                and werks = it_data-werks.
            if sy-subrc ne 0.
              wa_l_plant-pur_status = '01'.    "工厂特定的物料状态
              wa_l_plantx-pur_status = c_fieldx.    "工厂特定的物料状态
            else.
              clear i_status.
              find first occurrence of 'B' in i_pstat.
              if sy-subrc = 0.
                i_status = 'X'.
              endif.
              find first occurrence of 'G' in i_pstat.
              if sy-subrc = 0.
                i_status = 'X'.
              endif.
              if i_status is initial.
                wa_l_plant-pur_status = '01'.    "工厂特定的物料状态
                wa_l_plantx-pur_status = c_fieldx.    "工厂特定的物料状态
              endif.
            endif.

            wa_l_valx-val_area =  it_data-werks.    "工厂
            wa_l_plantx-plant =  it_data-werks.    "工厂
            set_fieldx it_data-ncost wa_l_plantx-no_costing  . "无成本核算
            set_fieldx it_data-awsls wa_l_plantx-variance_key  ."差异码
            set_fieldx it_data-prctr wa_l_plantx-profit_ctr  . "利润中心
            set_fieldx it_data-losgr wa_l_plantx-lot_size.  "成本核算批量
            set_fieldx it_data-bklas wa_l_valx-val_class .   "评估类
            set_fieldx it_data-vprsv wa_l_valx-price_ctrl.   "价格控制
            set_fieldx it_data-peinh wa_l_valx-price_unit.  "价格单位
            set_fieldx it_data-stprs wa_l_valx-std_price ."标准价格
            set_fieldx it_data-ekalr wa_l_valx-qty_struct."用QS的成本估算
            set_fieldx it_data-hkmat wa_l_valx-orig_mat. "物料来源
            set_fieldx it_data-hrkft wa_l_valx-orig_group. "原始组
            set_fieldx it_data-zplp1 wa_l_valx-plndprice1 ."计划价格1
            set_fieldx it_data-zpld1 wa_l_valx-plndprdate1."计划价格日期1
            set_fieldx it_data-zplp2 wa_l_valx-plndprice2. "计划价格2
            set_fieldx it_data-zpld2 wa_l_valx-plndprdate2. "计划价格日期2

          endif.


          if p_maktl = 'X'.
            wa_l_head-basic_view = abap_true.
            set_fieldx it_data-matkl wa_l_clientx-matl_group."物料组
            wa_l_client-matl_group = it_data-matkl.  "物料组
          endif.

          clear:wa_l_return,it_l_return[].
          CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
            EXPORTING
              headdata            = wa_l_head
              clientdata          = wa_l_client
              clientdatax         = wa_l_clientx
              plantdata           = wa_l_plant
              plantdatax          = wa_l_plantx
              valuationdata       = wa_l_val
              valuationdatax      = wa_l_valx
            TABLES
              materialdescription = it_l_desc
              extensionin         = it_l_ext
              extensioninx        = it_l_extx
              returnmessages      = it_l_return.

          loop at it_l_return into wa_l_return where type = 'E'
                                                  or type = 'A'.
            if it_out-message is initial.
              it_out-msgty = 'E'.
              it_out-message = wa_l_return-message.
              exit.
            endif.
          endloop.
          if it_out-message is initial.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
            it_out-msgty = 'S'.
            message e021 with it_out-matnr i_string into  it_out-message
            .
          endif.

          append it_out.
          clear: it_out,wa_l_head,wa_l_plant,wa_l_plantx,wa_l_client,
           wa_l_clientx,wa_l_sale,wa_l_salex,it_l_desc[],it_l_ext[],
           it_l_extx[],wa_l_val,wa_l_valx.
        endloop.

      endif.
      sort it_out by msgty itemno.

      DATA:LV_MATNR TYPE MARA-MATNR.
      IF P_MAktL = 'X'.""""""更新物料组时，修改消息内容
        LOOP AT it_out WHERE msgty = 'S'.
          READ TABLE IT_DATA WITH KEY MATNR = IT_OUT-MATNR
                                      itemno = IT_OUT-itemno.
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
            EXPORTING
              input  = IT_OUT-MATNR
            IMPORTING
              OUTPUT = LV_MATNR.

          CONCATENATE '物料号' LV_MATNR
                      '的物料组' IT_DATA-MATKL
                      '已经创建成功' INTO it_out-message.
          MODIFY IT_OUT.
        ENDLOOP.
      ENDIF.


    endform.                    " FRM_UPDATE_MASTER
*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       Display log data.
*----------------------------------------------------------------------*

form frm_display_data .

  data:i_repid like sy-repid.
  i_repid = sy-repid.
**  ****** Build the fieldcat for ALV display.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = i_repid
      i_internal_tabname     = 'IT_OUT'
*     I_STRUCTURE_NAME       =
*     I_CLIENT_NEVER_DISPLAY = 'X'
      i_inclname             = i_repid
*     I_BYPASSING_BUFFER     =
*     I_BUFFER_ACTIVE        =
    CHANGING
      ct_fieldcat            = it_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.
  if sy-subrc <> 0.
    message e022.
  endif.
*
  read table it_fieldcat into wa_fieldcat index 1.
  wa_fieldcat-seltext_l = text-035.
  wa_fieldcat-ddictxt = 'L'.
  modify it_fieldcat from wa_fieldcat index 1.

  LOOP AT it_fieldcat INTO wa_fieldcat .
    IF p_qlty = abap_true .
      CASE wa_fieldcat-fieldname.
        WHEN 'MAKTXE'.
          DELETE it_fieldcat WHERE fieldname =
                                   wa_fieldcat-fieldname .
        WHEN 'VKORG'.
          DELETE it_fieldcat WHERE fieldname =
                                   wa_fieldcat-fieldname .
        WHEN 'VTWEG'.
          DELETE it_fieldcat WHERE fieldname =
                                   wa_fieldcat-fieldname .
        WHEN OTHERS.
      ENDCASE.

    ENDIF.

  ENDLOOP.



  wa_layout-colwidth_optimize = 'X'. " set optimized column width.
  call function 'REUSE_ALV_GRID_DISPLAY'
   exporting
     i_callback_program                = i_repid
*    I_CALLBACK_PF_STATUS_SET          = ' '
*    I_CALLBACK_USER_COMMAND           = ' '
*    I_CALLBACK_TOP_OF_PAGE            = ' '
*    I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*    I_CALLBACK_HTML_END_OF_LIST       = ' '
*    I_STRUCTURE_NAME                  =
*    I_BACKGROUND_ID                   = ' '
*    I_GRID_TITLE                      =
*    I_GRID_SETTINGS                   =
     is_layout                         = wa_layout
     it_fieldcat                       = it_fieldcat
*    IT_EXCLUDING                      =
*    IT_SPECIAL_GROUPS                 =
*    IT_SORT                           =
*    IT_FILTER                         =
*  IMPORTING
*    E_EXIT_CAUSED_BY_CALLER           =
*    ES_EXIT_CAUSED_BY_USER            =
    tables
      t_outtab                          = it_out
   exceptions
     program_error                     = 1
     others                            = 2 .
  if sy-subrc <> 0.
    message e022.
  endif.


endform.                    " FRM_DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECKBOX_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_checkbox_select .
  if p_sale = abap_true.
    if p_basic = abap_true or p_purch = abap_true or
       p_mrp = abap_true or p_finan = abap_true
          or p_qlty = abap_true.
      message e013.
    endif.
  else.
    if  ( p_basic is initial ) and ( p_purch is initial ) and
        ( p_mrp is initial ) and ( p_finan is initial ) and
        ( p_sale is initial ) and ( p_maktl is initial )
         AND ( p_qlty IS INITIAL ).
      message e014.
    endif.

  endif.

  IF p_qlty = abap_true .
    if p_basic = abap_true or p_purch = abap_true or
       p_mrp = abap_true or p_finan = abap_true
          or p_sale = abap_true.
      message e102.
    endif.
  ENDIF.

endform.                    " FRM_CHECKBOX_SELECT
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frm_auth_check .
  data: i_statm    type c,
       i_status(6) type c,
       i_len       type i,
       i_pos       type i.

  if p_basic = abap_true.
    concatenate 'K' i_status into i_status.
  endif.
  if p_sale = abap_true.
    concatenate 'V' i_status into i_status.
  endif.
  if p_mrp = abap_true.
    concatenate 'D' i_status into i_status.
  endif.
  if p_purch = abap_true.
    concatenate 'E' i_status into i_status.
  endif.
  if p_finan = abap_true.
    concatenate 'B' i_status into i_status.
    concatenate 'G' i_status into i_status.
  endif.
*  HP_DXJ 20150324
  IF p_qlty = abap_true .
    CONCATENATE 'Q' i_status INTO i_status .
  ENDIF.
  i_len = strlen( i_status ).
  clear i_pos.
  while i_pos < i_len .
    i_statm = i_status+i_pos(1).
    authority-check object 'M_MATE_STA' " check authorization for QP01
    id 'ACTVT' field '01'
    id 'STATM' field i_statm.
    if sy-subrc <> 0.
      case i_statm.
        when 'K'.
          message e025 with sy-uname text-002.
        when 'V'.
          message e025 with sy-uname text-003.
        when 'D'.
          message e025 with sy-uname text-005.
        when 'E'.
          message e025 with sy-uname text-004.
        when 'B' or 'G'.
          message e025 with sy-uname text-006.
        WHEN 'Q' .
          message e025 with sy-uname text-039.
      endcase.

      exit.
    endif.
    i_pos = i_pos + 1.
  endwhile.

endform.                    " FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*&      Form  FRM_MAINTENANCE_QUALITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_MAINTENANCE_QUALITY .
  data:lt_l_return     type standard table of bapiret2,
         wa_l_return   type bapiret2,
         wa_l_head     type bapimathead,
         wa_l_client   type bapi_mara,
         wa_l_clientx  type bapi_marax,
         wa_l_plant    type bapi_marc,
         wa_l_plantx   type bapi_marcx,
         it_qmat       TYPE TABLE OF BAPI1001004_QMAT
                       WITH HEADER LINE.

  DATA: lwa_quality LIKE LINE OF it_quality ,
        i_matnr like marc-matnr,
        i_error TYPE c      ,
        i_string type string .

  set_view_string i_string text-039.
  LOOP AT it_quality_upload .

    MOVE-CORRESPONDING it_quality_upload to it_quality.
    it_quality-maktx = it_quality_upload-maktxc .
    MOVE-CORRESPONDING it_quality_upload to it_out .
***convert material number...
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = it_quality-matnr
      IMPORTING
        output       = it_quality-matnr
      EXCEPTIONS
        length_error = 1
        others       = 2.
    translate it_quality-matnr to upper case. " 物料号码
    TRANSLATE it_quality-mtart to UPPER CASE .
    TRANSLATE it_quality-werks to UPPER CASE .
    TRANSLATE it_quality-qmata to UPPER CASE .
    TRANSLATE it_quality-qmpur to UPPER CASE .
    TRANSLATE it_quality-ssqss TO UPPER CASE .
    TRANSLATE it_quality-art   TO UPPER CASE .
    TRANSLATE it_quality-apa   TO UPPER CASE .
    TRANSLATE it_quality-aktiv TO UPPER CASE .
*    检查必输字段
    check_mandtory it_quality-matnr text-007. " 物料号码.
    check_mandtory it_quality-mtart text-008. " 物料类型.
    check_mandtory it_quality-werks text-020.  "工厂
    check_mandtory it_quality-qmata text-040.  "检验业务分类
    check_mandtory it_quality-art text-041.  "检验类型
    check_mandtory it_quality-apa text-042.  "首选检验
    check_mandtory it_quality-aktiv text-043.  "检验业务分类
    APPEND it_quality .
    CLEAR it_quality .

  ENDLOOP.

  SORT it_quality by matnr mtart werks .

  LOOP AT  it_quality.
    lwa_quality = it_quality .

    it_out-maktxc = lwa_quality-maktx .
*     检查物料是否存在
    clear i_matnr.
    select single matnr into i_matnr
    from mara
    where matnr = lwa_quality-matnr.
    if i_matnr is initial.
      it_out-msgty = 'E'.
      message e015 with lwa_quality-matnr
      into it_out-message.
      append it_out.
      clear it_out.
      continue.
    endif.

    MOVE-CORRESPONDING lwa_quality TO IT_OUT .
    APPEND IT_OUT .
    CLEAR IT_OUT .

*  设置QMAT
    it_qmat-MATERIAL   = lwa_quality-matnr.
    it_qmat-FUNCTION   = '004'.
    it_qmat-INSPTYPE   = lwa_quality-ART.        "检验类型
    it_qmat-PLANT      = lwa_quality-WERKS.      "工厂

    it_qmat-IND_INSPTYPE_MAT_ACTIVE
                       = lwa_quality-aktiv.  "检验类型－物料合并已激活
    it_qmat-PREFERRED_INSPTYPE
                       = lwa_quality-APA.         "首选的检验类型

    it_qmat-qual_score_procedure = '06'.

    CASE lwa_quality-art.
      WHEN cns_qpart_01 or cns_qpart_80 or cns_qpart_81
            or cns_qpart_82 or cns_qpart_83 or cns_qpart_84
            or cns_qpart_90 .
*       有任务清单的检验
        it_qmat-ind_insp_with_tsk_list = 'X'.
*       自动规格分配
        it_qmat-ind_auto_assign = 'X'.
*       按特性检验
        it_qmat-ind_insp_by_charac = 'X'.
*       允许略过
        it_qmat-ind_skips_allowed = 'X'.
*       自动使用决策
        it_qmat-ind_automatic_ud = 'X'.
*       可能的序列号管理
        it_qmat-ind_single_units_possible = 'X'.

      WHEN cns_qpart_13 .
*       有任务清单的检验
        it_qmat-ind_insp_with_tsk_list = 'X'.

*       按特性检验
        it_qmat-ind_insp_by_charac = 'X'.
*       允许略过
        it_qmat-ind_skips_allowed = 'X'.

*       可能的序列号管理
        it_qmat-ind_single_units_possible = 'X'.


      WHEN OTHERS.
    ENDCASE.
    APPEND it_qmat.
    CLEAR it_qmat .

    AT END OF werks .
      wa_l_head-material = lwa_quality-matnr .
      wa_l_head-ind_sector   = 'M'.       "行业领域
      wa_l_head-matl_type    = lwa_quality-mtart.       "物料类型
      wa_l_head-quality_view = 'X'.            "质量管理视图

      wa_l_plant-PLANT       =  lwa_quality-WERKS.   "工厂
*      wa_l_plant-CTRL_KEY    =  lwa_quality-SSQSS.   "控制码
      wa_l_plant-qm_authgrp  =  lwa_quality-qmata . " 检验业务分类

      wa_l_plantx-PLANT      =  lwa_quality-WERKS.         "工厂
*      set_fieldx lwa_quality-ssqss wa_l_plantx-ctrl_key ."控制码
      set_fieldx lwa_quality-qmata wa_l_plantx-qm_authgrp."检验业务分类



*      wa_l_client-qm_procmnt = lwa_quality-qmpur. "QM采购激活
*      set_fieldx lwa_quality-qmpur wa_l_clientx-qm_procmnt."QM采购激活

* 建立质量管理视图
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          HEADDATA       = wa_l_head
          CLIENTDATA     = wa_l_client
          CLIENTDATAX    = wa_l_clientx
          PLANTDATA      = wa_l_plant
          PLANTDATAX     = wa_l_plantx
        TABLES
          RETURNMESSAGES = lt_l_return.

      LOOP AT  lt_l_return INTO wa_l_return
            WHERE  TYPE = 'A' OR
                    TYPE = 'E' OR
                    TYPE = 'X'.
        i_error = 'X' .
        it_out-msgty = 'E' .
        it_out-message = wa_l_return-message .
        MODIFY  it_out FROM it_out TRANSPORTING msgty message
                       WHERE  matnr = lwa_quality-matnr
                       AND    werks = lwa_quality-werks
                       AND    itemno = lwa_quality-itemno
                       AND    art   = lwa_quality-art  .
        CLEAR it_out .
        EXIT .
      ENDLOOP.

      REFRESH lt_l_return[] .
      CLEAR wa_l_return .
      IF i_error <> 'X'.

        CALL FUNCTION 'BAPI_MATINSPCTRL_SAVEREPLICA'
          TABLES
            RETURN         = lt_l_return
            INSPECTIONCTRL = it_qmat.
        LOOP AT  lt_l_return INTO wa_l_return
              WHERE  TYPE = 'A' OR
                      TYPE = 'E' OR
                      TYPE = 'X'.
          i_error = 'X' .
          it_out-msgty = 'E' .
          it_out-message = wa_l_return-message .
          MODIFY  it_out FROM it_out TRANSPORTING msgty message
                         WHERE  matnr = lwa_quality-matnr
                         AND    werks = lwa_quality-werks
                         AND    itemno = lwa_quality-itemno
                         AND    art   = lwa_quality-art  .
          CLEAR it_out .
          EXIT .
        ENDLOOP.
        IF i_error <> 'X'.


          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.
          it_out-msgty = 'S'.
          message e021 with it_out-matnr i_string into  it_out-message.
          MODIFY  it_out FROM it_out TRANSPORTING msgty message
                         WHERE  matnr = lwa_quality-matnr
                         AND    werks = lwa_quality-werks .
          CLEAR it_out .
        ELSE .
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        ENDIF.

      ELSE .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      ENDIF.
      REFRESH:it_qmat[] ,lt_l_return[] .
      CLEAR:wa_l_head ,wa_l_plant,wa_l_client,
            wa_l_plantx ,wa_l_clientx,lwa_quality,i_error .
    ENDAT .
  ENDLOOP.

ENDFORM.                    " FRM_MAINTENANCE_QUALITY
*&---------------------------------------------------------------------*
*&      Form  FRM_MAINTENANCE_TABLE_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_MAINTENANCE_TABLE_VIEW USING p_tabname .

  CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
    EXPORTING
      ACTION                               = 'U'
*     CORR_NUMBER                          = '          '
*     GENERATE_MAINT_TOOL_IF_MISSING       = ' '
*     SHOW_SELECTION_POPUP                 = ' '
      VIEW_NAME                            = p_tabname
*     NO_WARNING_FOR_CLIENTINDEP           = ' '
*     RFC_DESTINATION_FOR_UPGRADE          = ' '
*     CLIENT_FOR_UPGRADE                   = ' '
*     VARIANT_FOR_SELECTION                = ' '
*     COMPLEX_SELCONDS_USED                = ' '
*     CHECK_DDIC_MAINFLAG                  = ' '
*     SUPPRESS_WA_POPUP                    = ' '
*   TABLES
*     DBA_SELLIST                          =
*     EXCL_CUA_FUNCT                       =
   EXCEPTIONS
     CLIENT_REFERENCE                     = 1
     FOREIGN_LOCK                         = 2
     INVALID_ACTION                       = 3
     NO_CLIENTINDEPENDENT_AUTH            = 4
     NO_DATABASE_FUNCTION                 = 5
     NO_EDITOR_FUNCTION                   = 6
     NO_SHOW_AUTH                         = 7
     NO_TVDIR_ENTRY                       = 8
     NO_UPD_AUTH                          = 9
     ONLY_SHOW_ALLOWED                    = 10
     SYSTEM_FAILURE                       = 11
     UNKNOWN_FIELD_IN_DBA_SELLIST         = 12
     VIEW_NOT_FOUND                       = 13
     MAINTENANCE_PROHIBITED               = 14
     OTHERS                               = 15
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    " FRM_MAINTENANCE_TABLE_VIEW
