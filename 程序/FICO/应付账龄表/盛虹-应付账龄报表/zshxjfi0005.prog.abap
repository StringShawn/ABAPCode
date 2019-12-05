**************************************************
*程序名称:应付账龄表
*创建日期: 2019-11-20
*创建者:XXX
*申请者:XXX
*功能描述:
*============================================
*变更记录
*修改请求号    修改日期    修改人   修改描述
*DEVK912011    2019-11-20   HANDYXH    创建程序
***************************************************
REPORT ZSHXJFI0005.


************************************************************************
*            <第一部分---声明程序的参数和变量>  Declaration            *
************************************************************************
*----------------------------------------------------------------------*
* <1.1-声明表工作区> Table_Work_Areas Declaration                      *
*----------------------------------------------------------------------*
TABLES:
  kna1,
  lfa1,
  t001,
  skb1,
  bseg,
  bsad.


TYPE-POOLS: slis.

CONSTANTS: c_x      TYPE c      VALUE 'X'.      "选中标识
CONSTANTS: c_active TYPE char2  VALUE '03'.     "用于权限检查
CONSTANTS: c_h      TYPE c      VALUE 'H'.      "贷方标志
CONSTANTS: c_d000 TYPE char5  VALUE 'D000',   "账期内
           c_d001 TYPE char5  VALUE 'D001',   "账龄小于等于30天
           c_d002 TYPE char5  VALUE 'D002',   "账龄31到60天
           c_d003 TYPE char5  VALUE 'D003',   "账龄61到90天
           c_d004 TYPE char5  VALUE 'D004',   "账龄91到120天
           c_d005 TYPE char5  VALUE 'D005',   "账龄121到180天
           c_d006 TYPE char5  VALUE 'D006',   "账龄181到360天
           c_d007 TYPE char5  VALUE 'D007',   "账龄361到720天
           c_d008 TYPE char5  VALUE 'D008',   "账龄720到1440天
           c_d009 TYPE char5  VALUE 'D009'.   "账龄大于等于1440天
CONSTANTS: c_cust  TYPE char4  VALUE 'CUST',   "客户账龄
           c_vend  TYPE char4  VALUE 'VEND',   "供应商账龄
           c_total TYPE char5  VALUE 'TOTAL'.  "字段TOTAL名称
CONSTANTS: c_msg_e  TYPE c      VALUE 'E'.      "错误消息标志
CONSTANTS: c_item   TYPE char7  VALUE 'DISITEM'. "显示明细数据的功能码
CONSTANTS: c_a      TYPE c      VALUE 'A'.      "ALV执行的方式
CONSTANTS: c_box    TYPE c LENGTH 3      VALUE 'BOX'.    "ALV选择格子
CONSTANTS: c_belnr  TYPE char7  VALUE 'BELNR'.  "显示凭证的功能码

*----------------------------------------------------------------------*
* <1.4-声明用户自定义数据类型> Local Data Types in Program             *
*----------------------------------------------------------------------*
TYPES: BEGIN OF x_t001,
         bukrs TYPE t001-bukrs,  "公司代码
         waers TYPE t001-waers,  "货币
         ktopl TYPE t001-ktopl,  "帐目表
         land1 TYPE t001-land1,  "国家
       END OF x_t001.
TYPES: BEGIN OF x_knb1,
         kunnr TYPE knb1-kunnr,  "客户编码
       END OF x_knb1.
TYPES: BEGIN OF x_lfb1,
         lifnr TYPE lfa1-lifnr,  "供应商编码
       END OF x_lfb1.
TYPES: BEGIN OF x_head,
         box       TYPE char1,
         ktokd     TYPE kna1-ktokd,  "客户账户组
         kzhms     TYPE t077x-txt30, "客户账户组描述
         ktokk     TYPE lfa1-ktokk,  "供应商账户组
         gzhms     TYPE t077y-txt30,  "供应商账户组描述
         kunnr     TYPE bsid-kunnr,  "客户编码
         namec     TYPE kna1-name1,  "客户描述
         lifnr     TYPE bsik-lifnr,  "供应商编码
         namev     TYPE lfa1-name1,  "供应商描述
         ksbm      TYPE bsid-kunnr,  "客商编码
         ksms      TYPE kna1-name1,  "客商描述
         guoj      TYPE landx,      "国家
         koart     TYPE bseg-koart,  "科目类型
         klev1     TYPE char4,       "一级科目
         yjkmt     TYPE skat-txt50,  "一级科目描述
         hkont     TYPE bsik-hkont,  "会计帐目
         txt50     TYPE skat-txt50,  "科目描述
         sgl       TYPE char10,      "SGL
         bukrs     TYPE bsik-bukrs,  "公司代码
         butxt     TYPE t001-butxt,  "公司代码描述
         prctr     TYPE bseg-prctr,  "利润中心
         prctt     TYPE cepct-ltext, "利润中心描述
         gsber     TYPE bsik-gsber,  "业务范围
         gtext     TYPE tgsbt-gtext, "业务范围描述
         belnr     TYPE bsik-belnr,  "会计凭证
         buzei     TYPE bsik-buzei,  "会计凭证行
         gjahr     TYPE bsik-gjahr,  "会计年度
         xblnr_alt TYPE bkpf-xblnr_alt, "按月凭证号
         blart     TYPE bsik-blart,  "凭证类型
         tcode     TYPE bkpf-tcode,  "事务代码
         zuonr     TYPE bsik-zuonr,  "分配
         sgtxt     TYPE bsik-sgtxt,  "项目文本
         bldat     TYPE bsik-bldat,  "凭证日期
         budat     TYPE bsik-budat,  "过账日期
         dudat     TYPE bsik-budat,  "到期日
         augbl     TYPE bsik-augbl,  "清账凭证号
         augdt     TYPE bsik-augdt,  "清账日期
         ksye      TYPE bsik-dmbtr,  "客商余额
         wdqje     TYPE bsik-dmbtr,  "未到期余额
         zlje      TYPE bsik-dmbtr,  "账龄金额
         waers     TYPE bsik-waers,  "币种

         zlje_bb   TYPE bsik-dmbtr,  "账龄本位币金额
         waers_bb  TYPE bsik-waers,  "本币

         xtype     TYPE char4,       "账龄类型
         total     TYPE bsik-dmbtr,  "总计
         dmbt0     TYPE bsik-dmbtr,  "账期内
         dmbt1     TYPE bsik-dmbtr,                                 "30天以内
         dmbt2     TYPE bsik-dmbtr,                                 "31到60
         dmbt3     TYPE bsik-dmbtr,                                 "61到90
         dmbt4     TYPE bsik-dmbtr,                                 "3到6个月
         dmbt5     TYPE bsik-dmbtr,                                 "6到12个月
         dmbt6     TYPE bsik-dmbtr,                                 "1到2年
         dmbt7     TYPE bsik-dmbtr,                                 "2到3年
         dmbt8     TYPE bsik-dmbtr,                                 "3到4年
         dmbt9     TYPE bsik-dmbtr,                                 "4到5年
         dmbt10    TYPE bsik-dmbtr,                                 "5年以上
*ADD by PENGLIXUE at 20180224 BPMNO: ERP-YW201802020036
         kkber     LIKE bsid-kkber,
         kkbtx     LIKE t014t-kkbtx,
*end of ADD by PENGLIXUE
       END OF x_head,

       BEGIN OF x_item,
         bukrs TYPE bsik-bukrs,  "公司代码
         gjahr TYPE bsid-gjahr,  "会计年度
         lifnr TYPE bsik-lifnr,  "客户编码
         namev TYPE lfa1-name1,  "客户描述
         kunnr TYPE bsid-kunnr,  "客户编码
         namec TYPE kna1-name1,  "客户描述
         ktokk TYPE lfa1-ktokk,  "供应商账户组
         ktokd TYPE kna1-ktokd,  "客户账户组
         budat TYPE bsik-budat,  "过账日期
         augdt TYPE bsak-augdt,  "清帐日期
         dudat TYPE bsik-budat,  "净收付到期日
         umskz TYPE bsik-umskz,  "特殊总帐标识
         zterm TYPE bsik-zterm,  "付款条件代码
         text1 TYPE t052u-text1, "付款条款描述
         ducdt TYPE int4,        "逾期天数
         belnr TYPE bsik-belnr,  "会计凭证编号
         augbl TYPE bsak-augbl,  "清帐凭证
         buzei TYPE bsik-buzei,  "会计凭证中的行项目数
         bschl TYPE bsik-bschl,  "记帐代码
         shkzg TYPE bsik-shkzg,  "借方/贷方标识
         xnegp TYPE bsik-xnegp,  "标识: 反记帐
         klev1 TYPE char4,       " 一级科目
         hkont TYPE bsik-hkont,  "总分类帐帐目
         txt20 TYPE skat-txt20,  "科目描述
         wrbtr TYPE bsid-wrbtr,  "凭证货币金额
         waers TYPE bsid-waers,  "凭证货币类型
         dmbtr TYPE bsik-dmbtr,  "按本位币计的金额
         waerb TYPE t001-waers,  "本位币类型
         sgtxt TYPE bsid-sgtxt,  "项目文本
*    银行账号和开户行
         bankl TYPE lfbk-bankl,
         banka TYPE bnka-banka,
         banno TYPE char35,
       END OF x_item.
TYPES: BEGIN OF x_kna1,
         kunnr TYPE kna1-kunnr,  "客户编码
         name1 TYPE kna1-name1,  "客户描述
         ktokd TYPE kna1-ktokd,  "客户账户组
         land1 TYPE kna1-land1,  "国家
       END OF x_kna1.
TYPES: BEGIN OF x_t077d,
         ktokd TYPE kna1-ktokd,  "客户账户组
         kzhms TYPE t077x-txt30, "客户账户组描述
       END OF x_t077d.
TYPES: BEGIN OF x_t077k,
         ktokk TYPE lfa1-ktokk,  "客户账户组
         gzhms TYPE t077y-txt30, "客户账户组描述
       END OF x_t077k.


TYPES: BEGIN OF x_acc_cus,
         bukrs TYPE bsid-bukrs,  "公司代码
         kunnr TYPE bsid-kunnr,  "客户编码
         umskz TYPE bsid-umskz,  "特殊总帐标识
         augdt TYPE bsid-augdt, "清帐日期
         augbl TYPE bsid-augbl,  "清帐凭证
         gjahr TYPE bsid-gjahr,  "会计年度
         belnr TYPE bsid-belnr,  "会计凭证编号
         buzei TYPE bsid-buzei,  "会计凭证中的行项目数
         budat TYPE bsid-budat,  "凭证中的过帐日期
         waers TYPE bsid-waers,  "凭证货币类型
         bschl TYPE bsid-bschl,  "记帐代码
         shkzg TYPE bsid-shkzg,  "借方/贷方标识
         dmbtr TYPE bsid-dmbtr,  "按本位币计的金额
         wrbtr TYPE bsid-wrbtr,  "凭证货币金额
         sgtxt TYPE bsid-sgtxt,  "项目文本
         hkont TYPE bsid-hkont,  "总分类帐帐目
         zfbdt TYPE bsid-zfbdt,  "用于到期日计算的基准日期
         zterm TYPE bsid-zterm,  "付款条件代码
         zbd1t TYPE bsid-zbd1t,  "现金折扣天数
         zbd2t TYPE bsid-zbd2t,  "现金折扣天数
         zbd3t TYPE bsid-zbd3t,  "净支付条件期段
         rebzg TYPE bsid-rebzg,  "业务所属的发票号码
         xnegp TYPE bsid-xnegp,  "标识: 反记帐
         dudat TYPE bsik-budat,  "净收付到期日
         ducdt TYPE int4,        "逾期天数
         namec TYPE kna1-name1,  "客户描述
         ktokd TYPE kna1-ktokd,  "客户账户组
         kzhms TYPE t077x-txt30, "客户账户组描述
         waerb TYPE t001-waers,  "本位币
         ktopl TYPE t001-ktopl,  "账目表
         perid TYPE char5,       "属于区间标志
         xtype TYPE char4,       "账龄类型
         gsber TYPE bsid-gsber,   "业务范围
         zuonr TYPE bsid-zuonr,   "分配
         bldat TYPE bsid-bldat,   "凭证日期
         blart TYPE bkpf-blart,   "凭证类型
         land1 TYPE kna1-land1,   "国家
*ADD by PENGLIXUE at 20180224 BPMNO: ERP-YW201802020036
         kkber LIKE bsid-kkber,
*end of ADD by PENGLIXUE
       END OF x_acc_cus.
TYPES: BEGIN OF x_acc_ven,
         bukrs TYPE bsik-bukrs,  "公司代码
         lifnr TYPE bsik-lifnr,  "供应商编码
         umskz TYPE bsik-umskz,  "特殊总帐标识
         augdt TYPE bsik-augdt, "清帐日期
         augbl TYPE bsik-augbl,  "清帐凭证
         gjahr TYPE bsik-gjahr,  "会计年度
         belnr TYPE bsik-belnr,  "会计凭证编号
         buzei TYPE bsik-buzei,  "会计凭证中的行项目数
         budat TYPE bsik-budat,  "凭证中的过帐日期
         waers TYPE bsik-waers,  "凭证货币类型
         bschl TYPE bsik-bschl,  "记帐代码
         shkzg TYPE bsik-shkzg,  "借方/贷方标识
         dmbtr TYPE bsik-dmbtr,  "按本位币计的金额
         wrbtr TYPE bsik-wrbtr,  "凭证货币金额
         sgtxt TYPE bsik-sgtxt,  "项目文本
         hkont TYPE bsik-hkont,  "总分类帐帐目
         zfbdt TYPE bsik-zfbdt,  "用于到期日计算的基准日期
         zterm TYPE bsik-zterm,  "付款条件代码
         zbd1t TYPE bsik-zbd1t,  "现金折扣天数
         zbd2t TYPE bsik-zbd2t,  "现金折扣天数
         zbd3t TYPE bsik-zbd3t,  "净支付条件期段
         rebzg TYPE bsik-rebzg,  "业务所属的发票号码
         xnegp TYPE bsik-xnegp,  "标识: 反记帐
         dudat TYPE bsik-budat,  "净收付到期日
         ducdt TYPE int4,        "逾期天数
         namev TYPE lfa1-name1,  "供应商描述
         ktokk TYPE lfa1-ktokk,  "供应商账户组
         gzhms TYPE t077y-txt30, "供应商账户组描述
         waerb TYPE t001-waers,  "本位币
         ktopl TYPE t001-ktopl,  "账目表
         perid TYPE char5,       "属于区间标志
         xtype TYPE char4,       "账龄类型
         fbsis TYPE char1,       "数据是否来自于BSIS
         bankl TYPE lfbk-bankl,
         banka TYPE bnka-banka,
         banno TYPE char35,
         gsber TYPE bsid-gsber,   "业务范围
         zuonr TYPE bsid-zuonr,   "分配
         bldat TYPE bsid-bldat,   "凭证日期
         land1 TYPE lfa1-land1,   "国家
       END OF x_acc_ven.
TYPES: BEGIN OF x_account,
         bukrs TYPE bsik-bukrs,  "公司代码
         gjahr TYPE bsid-gjahr,  "会计年度
         lifnr TYPE bsik-lifnr,  "供应商编码
         namev TYPE lfa1-name1,  "供应商描述
         kunnr TYPE bsid-kunnr,  "客户编码
         namec TYPE kna1-name1,  "客户描述
         ktokk TYPE lfa1-ktokk,  "供应商账户组
         ktokd TYPE kna1-ktokd,  "客户账户组
         hkont TYPE bsik-hkont,  "会计科目
         txt20 TYPE skat-txt20,  "科目描述
         dmbtr TYPE bsik-dmbtr,  "按本位币计的金额
         umskz TYPE bsid-umskz,  "特殊总帐标识
         belnr TYPE bsid-belnr,  "会计凭证编号
         buzei TYPE bsid-buzei,  "会计凭证中的行项目数
         budat TYPE bsid-budat,  "凭证中的过帐日期
         waers TYPE bsid-waers,  "凭证货币类型
         bschl TYPE bsid-bschl,  "记帐代码
         shkzg TYPE bsid-shkzg,  "借方/贷方标识
         wrbtr TYPE bsid-wrbtr,  "凭证货币金额
         zfbdt TYPE bsid-zfbdt,  "用于到期日计算的基准日期
         zterm TYPE bsid-zterm,  "付款条件代码
         xnegp TYPE bsid-xnegp,  "标识: 反记帐
         waerb TYPE t001-waers,  "本位币类型
         ktopl TYPE t001-ktopl,  "账目表
         sgtxt TYPE bsid-sgtxt,  "文本
         perid TYPE char5,       "属于区间标志
         xtype TYPE char4,       "账龄类型
         dudat TYPE bsik-budat,  "净收付到期日
         ducdt TYPE int4,        "逾期天数
         augdt TYPE bsik-augdt,  "清帐日期
         augbl TYPE bsik-augbl,  "清帐凭证
         bankl TYPE lfbk-bankl,
         banka TYPE bnka-banka,
         banno TYPE char35,
       END OF x_account.

TYPES: BEGIN OF x_lfa1,
         lifnr TYPE lfa1-lifnr,  "供应商编码
         name1 TYPE lfa1-name1,  "供应商描述
         ktokk TYPE lfa1-ktokk,  "供应商账户组
         land1 TYPE lfa1-land1,  "国家
       END OF x_lfa1.

TYPES: BEGIN OF x_bsis,
         bukrs TYPE bsis-bukrs,  "公司代码
         augdt TYPE bsis-augdt,  "清帐日期
         augbl TYPE bsis-augbl,  "清帐凭证
         zuonr TYPE bsis-zuonr,  "分配编号
         gjahr TYPE bsis-gjahr,  "会计年度
         belnr TYPE bsis-belnr,  "会计凭证编号
         buzei TYPE bsis-buzei,  "会计凭证中的行项目数
         budat TYPE bsis-budat,  "凭证中的过帐日期
         waers TYPE bsis-waers,  "凭证货币类型
         bschl TYPE bsis-bschl,  "记帐代码
         shkzg TYPE bsis-shkzg,  "借方/贷方标识
         dmbtr TYPE bsis-dmbtr,  "按本位币计的金额
         wrbtr TYPE bsis-wrbtr,  "凭证货币金额
         sgtxt TYPE bsis-sgtxt,  "项目文本
         hkont TYPE bsis-hkont,  "总分类帐帐目
         zfbdt TYPE bsis-zfbdt,  "用于到期日计算的基准日期
         xnegp TYPE bsis-xnegp,  "标识: 反记帐
         gsber TYPE bsid-gsber,   "业务范围
         bldat TYPE bsid-bldat,   "凭证日期
       END OF x_bsis.

TYPES: BEGIN OF x_hkont,
         bukrs TYPE skb1-bukrs,  "公司代码
         saknr TYPE skb1-saknr,  "会计科目
         mitkz TYPE skb1-mitkz,  "科目是统驭科目
       END OF x_hkont,

       BEGIN OF x_skat,
         spras TYPE skat-spras,  "语言
         ktopl TYPE skat-ktopl,  "账目表
         saknr TYPE skat-saknr,  "总帐科目编号
         txt50 TYPE skat-txt50,  "总帐科目名称
       END OF x_skat.

TYPES: BEGIN OF x_cus,
         bukrs TYPE bsid-bukrs,  "公司代码
         kunnr TYPE bsid-kunnr,  "客户编码
         namec TYPE kna1-name1,  "客户描述
         ktokd TYPE kna1-ktokd,  "客户账户组
         hkont TYPE bsid-hkont,  "总分类帐帐目
         txt20 TYPE skat-txt20,  "科目描述
         xtype TYPE char4,       "账龄类型
         total TYPE bsid-dmbtr,  "总计
         dmbt0 TYPE bsik-dmbtr,  "账期内
         dmbt1 TYPE bsik-dmbtr,                                 "30天以内
         dmbt2 TYPE bsik-dmbtr,                                 "31到60
         dmbt3 TYPE bsik-dmbtr,                                 "61到90
         dmbt4 TYPE bsik-dmbtr,                                 "91到120天
         dmbt5 TYPE bsik-dmbtr,                                 "121到180天
         dmbt6 TYPE bsik-dmbtr,                                 "181到360天
         dmbt7 TYPE bsik-dmbtr,                                 "361到720天
         dmbt8 TYPE bsik-dmbtr,                                 "大于等于721天
       END OF x_cus.

TYPES: BEGIN OF x_ven,
         bukrs TYPE bsik-bukrs,  "公司代码
         lifnr TYPE bsik-lifnr,  "客户编码
         namev TYPE lfa1-name1,  "客户描述
         ktokk TYPE lfa1-ktokk,  "供应商账户组
         hkont TYPE bsik-hkont,  "总分类帐帐目
         txt20 TYPE skat-txt20,  "科目描述
         xtype TYPE char4,       "账龄类型
         total TYPE bsik-dmbtr,  "总计
         dmbt0 TYPE bsik-dmbtr,  "账期内
         dmbt1 TYPE bsik-dmbtr,                                 "30天以内
         dmbt2 TYPE bsik-dmbtr,                                 "31到60
         dmbt3 TYPE bsik-dmbtr,                                 "61到90
         dmbt4 TYPE bsik-dmbtr,                                 "91到120天
         dmbt5 TYPE bsik-dmbtr,                                 "121到180天
         dmbt6 TYPE bsik-dmbtr,                                 "181到360天
         dmbt7 TYPE bsik-dmbtr,                                 "361到720天
         dmbt8 TYPE bsik-dmbtr,                                 "大于等于721天
       END OF x_ven.

TYPES: BEGIN OF x_bseg,
         bukrs TYPE bseg-bukrs,   "公司代码
         belnr TYPE bseg-belnr,   "凭证编号
         gjahr TYPE bseg-gjahr,   "会计年度
         buzei TYPE bseg-buzei,   "凭证行号
         koart TYPE bseg-koart,  "科目类型
         prctr TYPE bseg-prctr,   "利润中心
       END OF x_bseg.

TYPES: BEGIN OF x_bukrs,
         bukrs TYPE t001-bukrs,  "公司代码
         butxt TYPE t001-butxt,  "公司文本
       END OF x_bukrs.

TYPES: BEGIN OF x_prctr,
         prctr TYPE cepct-prctr,  "利润中心
         ltext TYPE cepct-ltext,  "利润中心描述
       END OF x_prctr.
TYPES: BEGIN OF x_guojia,
         land1 TYPE t005t-land1,  "国家代码
         landx TYPE t005t-landx,  "国家文本
       END OF x_guojia.
TYPES: BEGIN OF x_bkpf,
         bukrs     TYPE bkpf-bukrs,
         belnr     TYPE bkpf-belnr,
         gjahr     TYPE bkpf-gjahr,
         tcode     TYPE bkpf-tcode,   "事务代码
         blart     TYPE bkpf-blart,   "凭证类型
         xblnr_alt TYPE bkpf-xblnr_alt, "按月凭证号
       END OF x_bkpf.

TYPES: BEGIN OF x_huizong,
         box      TYPE char1,
         bukrs    TYPE bsik-bukrs,  "公司代码
         butxt    TYPE t001-butxt,  "公司代码描述
         lifnr    TYPE bsik-lifnr,  "供应商编码
         namev    TYPE lfa1-name1,  "供应商描述
         kunnr    TYPE bsid-kunnr,  "客户编码
         namec    TYPE kna1-name1,  "客户描述
         ksbm     TYPE bsid-kunnr,  "客商编码
         ksms     TYPE kna1-name1,  "客商描述
         guoj     TYPE landx,      "国家
         ktokk    TYPE lfa1-ktokk,  "供应商账户组
         gzhms    TYPE t077y-txt30,  "供应商账户组描述
         ktokd    TYPE kna1-ktokd,  "客户账户组
         kzhms    TYPE t077x-txt30, "客户账户组描述
         koart    TYPE bseg-koart,  "科目类型
         klev1    TYPE char4,       "一级科目
         yjkmt    TYPE skat-txt20,  "一级科目描述
         hkont    TYPE bsik-hkont,  "会计帐目
         txt50    TYPE skat-txt50,  "科目描述
         prctr    TYPE bseg-prctr,  "利润中心
         prctt    TYPE cepct-ltext, "利润中心描述
         gsber    TYPE bsik-gsber,  "业务范围
         gtext    TYPE tgsbt-gtext, "业务范围描述
         waers    TYPE bsik-waers,  "币种
         zlje     TYPE bsik-dmbtr,  "账龄金额

         zlje_bb  TYPE bsik-dmbtr,  "账龄本位币金额
         waers_bb TYPE bsik-waers,  "本币

         total    TYPE bsik-dmbtr,  "总计
         wdqje    TYPE bsik-dmbtr,  "未到期余额
         xtype    TYPE char4,       "账龄类型
         dmbt1    TYPE bsik-dmbtr,                                 "30天以内
         dmbt2    TYPE bsik-dmbtr,                                 "31到60
         dmbt3    TYPE bsik-dmbtr,                                 "61到90
         dmbt4    TYPE bsik-dmbtr,                                 "3到6个月
         dmbt5    TYPE bsik-dmbtr,                                 "6到12个月
         dmbt6    TYPE bsik-dmbtr,                                 "1到2年
         dmbt7    TYPE bsik-dmbtr,                                 "2到3年
         dmbt8    TYPE bsik-dmbtr,                                 "3到4年
         dmbt9    TYPE bsik-dmbtr,                          "大于1140
       END OF x_huizong.








*----------------------------------------------------------------------*
* <1.5-声明内表> Global Internal Tables Declaration                    *
*----------------------------------------------------------------------*
DATA: it_t001 TYPE STANDARD TABLE OF x_t001,   "公司代码的数据
      wa_t001 TYPE x_t001.                     "公司代码的工作区
DATA: r_bukrs  TYPE RANGE OF t001-bukrs,        "用于权限检查RANGE
      wa_bukrs LIKE LINE OF r_bukrs.            "RANGE的工作区
DATA: it_head TYPE STANDARD TABLE OF x_head,   "输出汇总数据
      wa_head TYPE x_head.                     "汇总工作区
"      it_item type standard table of x_item,   "输出明细数据
"      wa_item type x_item.
DATA: it_account TYPE STANDARD TABLE OF x_account, "所有数据的内表
      wa_account TYPE x_account.                  "工作区
DATA: it_acc_cus TYPE STANDARD TABLE OF x_acc_cus, "客户的账龄数据
      wa_acc_cus TYPE x_acc_cus.
DATA: it_acc_ven TYPE STANDARD TABLE OF x_acc_ven, "供应商账龄数据
      wa_acc_ven TYPE x_acc_ven.                  "供应商账龄工作区
DATA: it_bsis    TYPE STANDARD TABLE OF x_bsis.   "BSIS内表
DATA: wa_bsis    TYPE x_bsis.
DATA: it_cus TYPE STANDARD TABLE OF x_cus,    "客户账龄的汇总
      wa_cus TYPE x_cus.                      "客户汇总工作区
DATA: it_ven TYPE STANDARD TABLE OF x_ven,    "供应商账龄的汇总
      wa_ven TYPE x_ven.                      "供应商汇总工作区
DATA: it_fieldcat TYPE slis_t_fieldcat_alv,        "显示汇总的字段集
      it_fielditm TYPE slis_t_fieldcat_alv.        "显示明细的字段集
DATA: it_huizong TYPE STANDARD TABLE OF x_huizong,  "汇总显示ALV
      wa_huizong TYPE x_huizong.                    "汇总显示ALV
DATA: it_huizong_out TYPE TABLE OF x_huizong.       "汇总显示ALV
DATA: wa_huizong_out TYPE x_huizong.               "汇总显示ALV
DATA: it_item TYPE STANDARD TABLE OF x_head,    "汇总模式双击行显示的明细内表
      wa_item TYPE x_head.                      ""汇总模式双击行显示的明细工作区

DATA: it_sort TYPE slis_t_sortinfo_alv,
      wa_sort LIKE LINE OF it_sort.
DATA:gt_bseg LIKE TABLE OF bseg WITH HEADER LINE.
*----------------------------------------------------------------------*
* <1.6-声明全局变量> Global Variants Declaration                       *
*----------------------------------------------------------------------*







*----------------------------------------------------------------------*
* <1.9-声明选择屏幕> Selection Screen                                  *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b_type WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:
p_kunnr AS CHECKBOX   MODIF ID oo.
SELECTION-SCREEN COMMENT 2(6) FOR FIELD s_kunnr .
SELECT-OPTIONS:
s_kunnr FOR kna1-kunnr  MODIF ID oo.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:
p_lifnr AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 2(6) FOR FIELD s_lifnr.
SELECT-OPTIONS:
s_lifnr FOR lfa1-lifnr.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:
p_hkont AS CHECKBOX  .
SELECTION-SCREEN COMMENT 2(6) FOR FIELD s_hkont.
SELECT-OPTIONS:
s_hkont FOR skb1-saknr .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b_type.
SELECTION-SCREEN BEGIN OF BLOCK b_paraset WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
s_bukrs FOR t001-bukrs MEMORY ID buk OBLIGATORY.
PARAMETERS:
p_budat TYPE bkpf-budat OBLIGATORY DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK b_paraset.

SELECTION-SCREEN: BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(30) TEXT-042 FOR FIELD p_date1 MODIF ID old.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: p_date1  TYPE idcn_segm DEFAULT '0030' MODIF ID old OBLIGATORY.
PARAMETERS: p_date2  TYPE idcn_segm DEFAULT '0060' MODIF ID old.
PARAMETERS: p_date3  TYPE idcn_segm DEFAULT '0090' MODIF ID old.
PARAMETERS: p_date4  TYPE idcn_segm DEFAULT '0120' MODIF ID old.
PARAMETERS: p_date5  TYPE idcn_segm DEFAULT '0180' MODIF ID old.
PARAMETERS: p_date6  TYPE idcn_segm DEFAULT '0360' MODIF ID old.
PARAMETERS: p_date7  TYPE idcn_segm DEFAULT '0720' MODIF ID old.
PARAMETERS: p_date8  TYPE idcn_segm DEFAULT '1440' MODIF ID old.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 1.
PARAMETERS: r_1 TYPE c RADIOBUTTON GROUP gr1 MODIF ID zzm USER-COMMAND fc1.
PARAMETERS: r_2 TYPE c RADIOBUTTON GROUP gr1 DEFAULT 'X' MODIF ID zzm.




SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS c_1 TYPE c AS CHECKBOX MODIF ID md1.               "明细模式
SELECTION-SCREEN COMMENT 2(6) FOR FIELD s_prctr MODIF ID md1.
SELECT-OPTIONS: s_prctr FOR bseg-prctr MODIF ID md1 MATCHCODE OBJECT prct.          "利润中心
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS c_2 TYPE c AS CHECKBOX MODIF ID md1.               "汇总模式
SELECTION-SCREEN COMMENT 2(6) FOR FIELD s_gsber MODIF ID md1.
SELECT-OPTIONS: s_gsber FOR bsad-gsber MATCHCODE OBJECT epic_cb_gsber_search_help MODIF ID md1. "业务范围
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b01.


SELECTION-SCREEN: BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.
PARAMETERS: r_3 TYPE c RADIOBUTTON GROUP gr2 DEFAULT 'X'.                               "本币
PARAMETERS: r_4 TYPE c RADIOBUTTON GROUP gr2.                                           "交易货币
SELECTION-SCREEN: END OF BLOCK b02.

SELECTION-SCREEN: BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-b03.
PARAMETERS: r_5 TYPE c RADIOBUTTON GROUP gr3 DEFAULT 'X'.                               "以基准日期计算
PARAMETERS: r_6 TYPE c RADIOBUTTON GROUP gr3 .                                          "以付款条件计算
SELECTION-SCREEN: END OF BLOCK b03.

*将汇总模式下的所有参数做成隐藏与显示切换效果
AT SELECTION-SCREEN OUTPUT.

  IF  s_hkont[] IS INITIAL.
    s_hkont-sign = 'I'.
    s_hkont-option = 'BT'.
    s_hkont-low = '2202020100'.
    s_hkont-high = '2202020200'.
    APPEND s_hkont.


  ENDIF.


  LOOP AT SCREEN.
    IF screen-group1 = 'OO'.
      screen-active = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

  IF r_1 EQ 'X'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'MD1'.
        screen-invisible = 1.
        screen-input     = 0.
        MODIFY SCREEN .
      ENDIF.
    ENDLOOP.
    CLEAR c_1.
    CLEAR c_2.
  ENDIF.
  IF r_2 EQ 'X'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'MD1'.
        screen-invisible = 0.
        screen-input     = 1.
        MODIFY SCREEN .
      ENDIF.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN.
*检查账龄段,要确定从账龄1至账龄8参数成递增序列。
  IF p_date1 LT 0 OR p_date2 LT 0
      OR p_date3 LT 0 OR p_date4 LT 0
      OR p_date5 LT 0 OR p_date6 LT 0
      OR p_date7 LT 0 OR p_date8 LT 0 .

    MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
    EXIT.
  ENDIF.
*段1等于空
  IF p_date1 EQ ''.
    MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
    EXIT.
  ENDIF.
*段2等于空
  IF p_date2 EQ ''.
    IF p_date3 NE '' OR p_date4 NE '' OR p_date5 NE '' OR p_date6 NE '' OR p_date7 NE '' OR p_date8 NE ''.
      MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
      EXIT.
    ENDIF.
  ELSE.
    IF p_date1 GE p_date2.
      MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
      EXIT.
    ENDIF.
  ENDIF.
*段3等于空
  IF p_date3 EQ ''.
    IF p_date4 NE '' OR p_date5 NE '' OR p_date6 NE '' OR p_date7 NE '' OR p_date8 NE ''.
      MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
      EXIT.
    ENDIF.
  ELSE.
    IF p_date2 GE p_date3.
      MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
      EXIT.
    ENDIF.
  ENDIF.
*段4等于空
  IF p_date4 EQ ''.
    IF p_date5 NE '' OR p_date6 NE '' OR p_date7 NE '' OR p_date8 NE ''.
      MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
      EXIT.
    ENDIF.
  ELSE.
    IF p_date3 GE p_date4.
      MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
      EXIT.
    ENDIF.
  ENDIF.
*段5等于空
  IF p_date5 EQ ''.
    IF p_date6 NE '' OR p_date7 NE '' OR p_date8 NE ''.
      MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
      EXIT.
    ENDIF.
  ELSE.
    IF p_date4 GE p_date5.
      MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
      EXIT.
    ENDIF.
  ENDIF.
*段6等于空
  IF p_date6 EQ ''.
    IF p_date7 NE '' OR p_date8 NE ''.
      MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
      EXIT.
    ENDIF.
  ELSE.
    IF p_date5 GE p_date6.
      MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
      EXIT.
    ENDIF.
  ENDIF.
*段7等于空
  IF p_date7 EQ ''.
    IF p_date8 NE ''.
      MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
      EXIT.
    ENDIF.
  ELSE.
    IF p_date6 GE p_date7.
      MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
      EXIT.
    ENDIF.
  ENDIF.
*段8等于空
  IF p_date8 EQ ''.

  ELSE.
    IF p_date7 GE p_date8.
      MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
      EXIT.
    ENDIF.
  ENDIF.


*  检查公司代码
  PERFORM f_check_company.
*  权限检查
  PERFORM f_check_authority.
*  检查客户
  PERFORM f_check_customer.
*  检查供应商
  PERFORM f_check_vendor.
*----------------------------------------------------------------------*
*       INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  CLEAR:it_head[],
  it_item[].

*----------------------------------------------------------------------*
*       START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
*取数
  PERFORM f_get_main.
*  显示数据
  PERFORM f_show_data.









*&---------------------------------------------------------------------*
*&      Form  F_CHECK_AUTHORITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_check_authority .
  CLEAR r_bukrs[].
  LOOP AT it_t001 INTO wa_t001.
    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'BUKRS' FIELD wa_t001-bukrs
    ID 'ACTVT' FIELD c_active.
    IF sy-subrc = 0.
      wa_bukrs-sign   = 'I'.
      wa_bukrs-option = 'EQ'.
      wa_bukrs-low    = wa_t001-bukrs.
      APPEND wa_bukrs TO r_bukrs.
      CLEAR wa_bukrs.
    ENDIF.
  ENDLOOP.
  IF r_bukrs IS INITIAL.
    SET CURSOR FIELD 'S_BUKRS-LOW'.
    MESSAGE e218(f4) WITH s_bukrs-low.
  ELSE.
    SORT r_bukrs.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_COMPANY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_check_company .
  CLEAR it_t001[].
  SELECT bukrs
         waers
         ktopl
         land1
    INTO TABLE it_t001
    FROM t001
   WHERE bukrs IN s_bukrs.
  IF sy-subrc <> 0.
    SET CURSOR FIELD 'S_BUKRS-LOW'.
    MESSAGE e069(f4).
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_CUSTOMER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_check_customer .
  DATA:lit_kunnr TYPE STANDARD TABLE OF x_knb1.
  IF s_kunnr IS NOT INITIAL.
    CLEAR lit_kunnr[].
    SELECT kunnr
    INTO TABLE lit_kunnr
    FROM knb1
    WHERE kunnr IN s_kunnr
    AND bukrs IN s_bukrs.
    IF sy-subrc <> 0.
      SET CURSOR FIELD 'S_KUNNR-LOW'.
      MESSAGE e246(f2).
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_check_vendor .
  DATA:lit_lifnr TYPE STANDARD TABLE OF x_lfb1.
  IF s_lifnr IS NOT INITIAL.
    CLEAR lit_lifnr[].
    SELECT lifnr
    INTO TABLE lit_lifnr
    FROM lfb1
    WHERE lifnr IN s_lifnr
    AND bukrs IN s_bukrs.
    IF sy-subrc <> 0.
      SET CURSOR FIELD 'S_LIFNR-LOW'.
      MESSAGE e163(f2).
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_MAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_main .
*当没有选择相应的复选框，则清空对应的值
  IF p_hkont <> c_x.
    CLEAR:
    s_hkont,
    s_hkont[].
  ENDIF.
  IF p_kunnr <> c_x.
    CLEAR:
    s_kunnr,
    s_kunnr[].
  ENDIF.
  IF p_lifnr <> c_x.
    CLEAR:
    s_lifnr,
    s_lifnr[].
  ENDIF.
  IF c_1 <> c_x.
    CLEAR:
    s_prctr,
    s_prctr[].
  ENDIF.
  IF c_2 <> c_x.
    CLEAR:
    s_gsber,
    s_gsber[].
  ENDIF.
*获取客户的账龄信息
  IF p_kunnr = c_x AND p_lifnr <> c_x.
    PERFORM f_get_customer.
  ENDIF.
*获取供应商的账龄信息
  IF p_lifnr = c_x AND p_kunnr <> c_x.
    PERFORM f_get_vendor.
  ENDIF.
*  当选择科目的时候，同时获取客户和供应商的数据
  IF ( p_hkont = c_x AND p_kunnr <> c_x AND p_lifnr <> c_x )
  OR ( p_hkont = c_x AND p_kunnr = c_x  AND p_lifnr = c_x )
  OR ( p_hkont <> c_x AND p_kunnr = c_x AND p_lifnr = c_x ).
    PERFORM f_get_customer.
    PERFORM f_get_vendor.
  ENDIF.
*  组合客户和供应商的账龄信息
  PERFORM f_merge_cus_ven.
  PERFORM f_merge_cus_ven_1.
  "  perform f_sum_money.
  PERFORM f_huizong_head.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_CUSTOMER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_customer .
  DATA: lit_kunnr TYPE STANDARD TABLE OF x_kna1,
        lwa_kunnr TYPE x_kna1.

  DATA: liw_t077d TYPE x_t077d.
  DATA: lit_t077d TYPE STANDARD TABLE OF x_t077d.
  DATA: lt_t001 TYPE TABLE OF t001.
  DATA: lw_t001 TYPE t001.
  DATA: lw_bseg TYPE x_bseg.
  DATA: lt_bseg TYPE TABLE OF x_bseg.
  DATA: l_line TYPE sy-tabix.
*ADD by PENGLIXUE at 20180309 BPMNO: ERP-YW201803050067
*  DATA:BEGIN OF LT_ZTYOUZSD_0004 OCCURS 0 ,
*         KUNNR  LIKE ZTYOUZSD_0004-KUNNR,
*         KKBER  LIKE ZTYOUZSD_0004-KKBER,
*         KNKLI  LIKE ZTYOUZSD_0004-KNKLI,
*         ZDESQH LIKE ZTYOUZSD_0004-ZDESQH,
*         ZDEZQ  LIKE ZTYOUZSD_0004-ZDEZQ,
*       END OF LT_ZTYOUZSD_0004,
*       LT_ZTYOUZSD_0004_T LIKE STANDARD TABLE OF LT_ZTYOUZSD_0004 WITH HEADER LINE.
*end of ADD by PENGLIXUE
  CLEAR:it_acc_cus[].
*根据客户的未清项
  SELECT bsid~bukrs
         bsid~kunnr
         bsid~umskz
         bsid~augdt
         bsid~augbl
         bsid~gjahr
         bsid~belnr
         bsid~buzei
         bsid~budat
         bsid~waers
         bsid~bschl
         bsid~shkzg
         bsid~dmbtr
         bsid~wrbtr
         bsid~sgtxt
         bsid~hkont
         bsid~zfbdt
         bsid~zterm
         bsid~zbd1t
         bsid~zbd2t
         bsid~zbd3t
         bsid~rebzg
         bsid~xnegp
         bsid~gsber
         bsid~zuonr
         bsid~bldat
*ADD by PENGLIXUE at 20180224 BPMNO: ERP-YW201802020036
         bsid~kkber
*end of ADD by PENGLIXUE
*ADD by PENGLIXUE at 20180309 BPMNO: ERP-YW201803050067
         bkpf~blart
*end of ADD by PENGLIXUE
    INTO CORRESPONDING FIELDS OF TABLE it_acc_cus
    FROM bsid
    INNER JOIN bkpf ON bsid~belnr EQ bkpf~belnr
                   AND bsid~bukrs EQ bkpf~bukrs
                   AND bsid~gjahr EQ bkpf~gjahr
   WHERE bsid~bukrs IN r_bukrs
     AND bsid~kunnr IN s_kunnr
     AND bsid~hkont IN s_hkont
     AND bsid~gsber IN s_gsber
     AND bkpf~budat LE p_budat
     AND bkpf~bstat NOT IN ('M','S','V','W','Z').

*根据客户的已清项
  SELECT bsad~bukrs
         bsad~kunnr
         bsad~umskz
         bsad~augdt
         bsad~augbl
         bsad~gjahr
         bsad~belnr
         bsad~buzei
         bsad~budat
         bsad~waers
         bsad~bschl
         bsad~shkzg
         bsad~dmbtr
         bsad~wrbtr
         bsad~sgtxt
         bsad~hkont
         bsad~zfbdt
         bsad~zterm
         bsad~zbd1t
         bsad~zbd2t
         bsad~zbd3t
         bsad~rebzg
         bsad~xnegp
         bsad~gsber
         bsad~zuonr
         bsad~bldat
*ADD by PENGLIXUE at 20180224 BPMNO: ERP-YW201802020036
         bsad~kkber
*end of ADD by PENGLIXUE
*ADD by PENGLIXUE at 20180309 BPMNO: ERP-YW201803050067
         bkpf~blart
*end of ADD by PENGLIXUE
    APPENDING CORRESPONDING FIELDS OF TABLE it_acc_cus
    FROM bsad
    INNER JOIN bkpf ON bsad~belnr EQ bkpf~belnr
                   AND bsad~bukrs EQ bkpf~bukrs
                   AND bsad~gjahr EQ bkpf~gjahr
   WHERE bsad~bukrs IN r_bukrs
     AND bsad~kunnr IN s_kunnr
     AND bsad~hkont IN s_hkont
     AND bsad~gsber IN s_gsber
     AND bkpf~budat LE p_budat
     AND bsad~augdt GT p_budat
     AND bkpf~bstat NOT IN ('M','S','V','W','Z').
*ADD by PENGLIXUE at 20180309 BPMNO: ERP-YW201803050067
  IF it_acc_cus IS NOT INITIAL.
*    SELECT KUNNR KKBER KNKLI ZDESQH ZDEZQ
*      INTO TABLE LT_ZTYOUZSD_0004_T
*      FROM ZTYOUZSD_0004
*      FOR ALL ENTRIES IN IT_ACC_CUS
*      WHERE KUNNR = IT_ACC_CUS-KUNNR"客户编号
*      AND   KKBER = IT_ACC_CUS-KKBER"贷方控制范围
*      AND   ZDEZT = '6'"状态
*      AND   ZDEXYLX IN ('1','2')."信用类型
  ENDIF.
*  LOOP AT LT_ZTYOUZSD_0004_T.
*    CLEAR:LT_ZTYOUZSD_0004_T-KNKLI,LT_ZTYOUZSD_0004_T-ZDESQH.
*    COLLECT LT_ZTYOUZSD_0004_T INTO LT_ZTYOUZSD_0004.
*  ENDLOOP.
*end of ADD by PENGLIXUE
  "*按照币种排除数据，如果选择了本币，则排除，如果选择交易货币，不做处理
  "  if it_acc_cus[] is not initial.
  "*币种为本币
  "    if r_3 eq 'X'.
  "      clear wa_t001.
  "      loop at it_t001 into wa_t001.
  "        delete it_acc_cus where bukrs eq wa_t001-bukrs and waers ne wa_t001-waers.
  "      endloop.
  "    endif.
  "*币种为货币
  "    if r_4 eq 'X'.

  "    endif.
  "  endif.

*当选择汇总模式时,排除数据
  IF r_2 EQ 'X'.
    IF it_acc_cus IS NOT INITIAL.
      IF c_1 EQ 'X' AND s_prctr[] IS NOT INITIAL.
*取科目类型和利润中心
        SELECT bukrs
               belnr
               gjahr
               buzei
               koart
               prctr
          INTO CORRESPONDING FIELDS OF TABLE lt_bseg
          FROM bseg
          FOR ALL ENTRIES IN it_acc_cus
         WHERE bukrs EQ it_acc_cus-bukrs
           AND belnr EQ it_acc_cus-belnr
           AND gjahr EQ it_acc_cus-gjahr
           AND buzei EQ it_acc_cus-buzei
           AND prctr IN s_prctr.
        SORT lt_bseg.
        DELETE ADJACENT DUPLICATES FROM lt_bseg.
*如果某条数据的利润中心不在选择屏幕的利润中心参数中，则排除掉。
        CLEAR wa_acc_cus.
        LOOP AT it_acc_cus INTO wa_acc_cus.
          l_line = sy-tabix.
          READ TABLE lt_bseg INTO lw_bseg WITH KEY bukrs = wa_acc_cus-bukrs belnr = wa_acc_cus-belnr
                                                gjahr = wa_acc_cus-gjahr buzei = wa_acc_cus-buzei.

          IF sy-subrc EQ 0.

          ELSE.
*排除不符合要求的利润中心
            DELETE it_acc_cus INDEX l_line.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.


*取客户的描述和账户组
  IF it_acc_cus[] IS NOT INITIAL.
    SORT it_acc_cus BY kunnr.
    SELECT kunnr
           name1
           ktokd
           land1
      INTO TABLE lit_kunnr
      FROM kna1
       FOR ALL ENTRIES IN it_acc_cus
     WHERE kunnr = it_acc_cus-kunnr.
*获取客户账户组描述
    IF lit_kunnr IS NOT INITIAL.
      SELECT ktokd
             txt30
        INTO TABLE lit_t077d
        FROM t077x
         FOR ALL ENTRIES IN lit_kunnr
       WHERE ktokd EQ lit_kunnr-ktokd
         AND spras EQ 1.
    ENDIF.
  ENDIF.
  IF lit_kunnr[] IS NOT INITIAL.
    SORT lit_kunnr BY kunnr.
    SORT it_t001 BY bukrs.
    LOOP AT it_acc_cus INTO wa_acc_cus.
      CLEAR lwa_kunnr.
*  读取客户描述和账户组
      READ TABLE lit_kunnr INTO lwa_kunnr
      WITH KEY kunnr = wa_acc_cus-kunnr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_acc_cus-namec = lwa_kunnr-name1.
        wa_acc_cus-ktokd = lwa_kunnr-ktokd.
        wa_acc_cus-land1 = lwa_kunnr-land1.
        READ TABLE lit_t077d INTO liw_t077d WITH KEY ktokd = lwa_kunnr-ktokd.
        IF sy-subrc EQ 0.
          wa_acc_cus-kzhms = liw_t077d-kzhms.
        ENDIF.
      ENDIF.
*  根据贷方标识计算负金额
      IF wa_acc_cus-shkzg = c_h.
        wa_acc_cus-wrbtr =  0 - wa_acc_cus-wrbtr. "本币金额
        wa_acc_cus-dmbtr =  0 - wa_acc_cus-dmbtr. "交易货币金额
      ENDIF.

*计算净收付到期日
*以基准日期计算:BSId/BSAd-ZFBDT   =  到期日期
      IF r_5 EQ 'X'.
        wa_acc_cus-dudat = wa_acc_cus-zfbdt.
      ENDIF.
*以付款条件计算:BSId/BSAd-ZFBDT  +  天数BSIK/BSAK-ZBD1T   =  到期日期
      IF r_6 EQ 'X'.
*ADD by PENGLIXUE at 20180309 BPMNO: ERP-YW201803050067
        IF wa_acc_cus-blart = 'AB'.
*          READ TABLE LT_ZTYOUZSD_0004 WITH  KEY KUNNR = WA_ACC_CUS-KUNNR KKBER = WA_ACC_CUS-KKBER.
          IF sy-subrc = 0.
*            WA_ACC_CUS-ZBD1T = LT_ZTYOUZSD_0004-ZDEZQ.
          ELSE.
            wa_acc_cus-zbd1t = 0.
          ENDIF.
        ENDIF.
*end of ADD by PENGLIXUE
        wa_acc_cus-dudat = wa_acc_cus-zfbdt + wa_acc_cus-zbd1t.
      ENDIF.

*计算区间的标志
      PERFORM f_calcu_period USING wa_acc_cus-dudat CHANGING wa_acc_cus-perid wa_acc_cus-ducdt.
*读取本位币和账目表
      READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_acc_cus-bukrs
      BINARY SEARCH.
      IF sy-subrc = 0.
        wa_acc_cus-waerb = wa_t001-waers.
        wa_acc_cus-ktopl = wa_t001-ktopl.
      ENDIF.
      wa_acc_cus-xtype = c_cust.
      MODIFY it_acc_cus FROM wa_acc_cus TRANSPORTING namec ktokd kzhms wrbtr dmbtr perid waerb ktopl xtype dudat ducdt land1.
      CLEAR wa_acc_cus.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CALCU_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ACC_CUS_DUDAT  text
*      <--P_WA_ACC_CUS_PERID  text
*      <--P_WA_ACC_CUS_DUCDT  text
*----------------------------------------------------------------------*
FORM f_calcu_period  USING    p_dudat TYPE sy-datum
                      CHANGING p_perid TYPE char5
                               p_ducdt TYPE vtbbewe-atage.

  DATA: l_days TYPE vtbbewe-atage, "相隔天数
        l_from TYPE sy-datum,     "开始日期
        l_to   TYPE sy-datum,     "结束日期
        l_int  TYPE i VALUE 1.    "日期正负
  DATA: l_stop TYPE string.


  CLEAR: l_days,
         l_from,
         l_to.


  IF p_dudat > p_budat.
    l_from = p_budat.
    l_to   = p_dudat.
    l_int  = -1.
  ELSE.
    l_from = p_dudat.
    l_to   = p_budat .
  ENDIF.

  CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
    EXPORTING
      i_date_from = l_from
      i_date_to   = l_to
    IMPORTING
      e_days      = l_days.

  p_ducdt = l_days * l_int.
*计算相隔的日期属于哪个区间
*判断账龄段在哪个账龄点参数结束
*如果账龄点在第2个参数结束
  IF p_date2 EQ ''.
    l_stop = 'P_DATE2'.
*如果账龄点在第3个参数结束
  ELSEIF p_date3 EQ ''.
    l_stop = 'P_DATE3'.
*如果账龄点在第4个参数结束
  ELSEIF p_date4 EQ ''.
    l_stop = 'P_DATE4'.
*如果账龄点在第5个参数结束
  ELSEIF p_date5 EQ ''.
    l_stop = 'P_DATE5'.
*如果账龄点在第6个参数结束
  ELSEIF p_date6 EQ ''.
    l_stop = 'P_DATE6'.
*如果账龄点在第7个参数结束
  ELSEIF p_date7 EQ ''.
    l_stop = 'P_DATE7'.
*如果账龄点在第8个参数结束
  ELSEIF p_date8 EQ ''.
    l_stop = 'P_DATE8'.
*如果账龄点参数都不为空
  ELSE.
    l_stop = ''.
  ENDIF.
*判断账龄
  IF p_dudat < p_budat.
    CASE l_stop.
*当第二个账龄点为空时
      WHEN 'P_DATE2'.
        IF l_days =< p_date1.
          p_perid = c_d001.
        ELSE.
          p_perid = c_d009.
        ENDIF.
*当第三个账龄点为空时
      WHEN 'P_DATE3'.
        IF l_days =< p_date1.
          p_perid = c_d001.
        ELSEIF l_days > p_date1 AND l_days <= p_date2.
          p_perid = c_d002.
        ELSE.
          p_perid = c_d009.
        ENDIF.
*当第四个账龄点为空时
      WHEN 'P_DATE4'.
        IF l_days =< p_date1.
          p_perid = c_d001.
        ELSEIF l_days > p_date1 AND l_days <= p_date2.
          p_perid = c_d002.
        ELSEIF l_days > p_date2 AND l_days <= p_date3.
          p_perid = c_d003.
        ELSE.
          p_perid = c_d009.
        ENDIF.
*当第五个账龄点为空时
      WHEN 'P_DATE5'.
        IF l_days =< p_date1.
          p_perid = c_d001.
        ELSEIF l_days > p_date1 AND l_days <= p_date2.
          p_perid = c_d002.
        ELSEIF l_days > p_date2 AND l_days <= p_date3.
          p_perid = c_d003.
        ELSEIF l_days > p_date3 AND l_days <= p_date4.
          p_perid = c_d004.
        ELSE.
          p_perid = c_d009.
        ENDIF.
*当第六个账龄点为空时
      WHEN 'P_DATE6'.
        IF l_days =< p_date1.
          p_perid = c_d001.
        ELSEIF l_days > p_date1 AND l_days <= p_date2.
          p_perid = c_d002.
        ELSEIF l_days > p_date2 AND l_days <= p_date3.
          p_perid = c_d003.
        ELSEIF l_days > p_date3 AND l_days <= p_date4.
          p_perid = c_d004.
        ELSEIF l_days > p_date4 AND l_days <= p_date5.
          p_perid = c_d005.
        ELSE.
          p_perid = c_d009.
        ENDIF.
*当第七个账龄点为空时
      WHEN 'P_DATE7'.
        IF l_days =< p_date1.
          p_perid = c_d001.
        ELSEIF l_days > p_date1 AND l_days <= p_date2.
          p_perid = c_d002.
        ELSEIF l_days > p_date2 AND l_days <= p_date3.
          p_perid = c_d003.
        ELSEIF l_days > p_date3 AND l_days <= p_date4.
          p_perid = c_d004.
        ELSEIF l_days > p_date4 AND l_days <= p_date5.
          p_perid = c_d005.
        ELSEIF l_days > p_date5 AND l_days <= p_date6.
          p_perid = c_d006.
        ELSE.
          p_perid = c_d009.
        ENDIF.
*当第八个账龄点为空时
      WHEN 'P_DATE8'.
        IF l_days =< p_date1.
          p_perid = c_d001.
        ELSEIF l_days > p_date1 AND l_days <= p_date2.
          p_perid = c_d002.
        ELSEIF l_days > p_date2 AND l_days <= p_date3.
          p_perid = c_d003.
        ELSEIF l_days > p_date3 AND l_days <= p_date4.
          p_perid = c_d004.
        ELSEIF l_days > p_date4 AND l_days <= p_date5.
          p_perid = c_d005.
        ELSEIF l_days > p_date5 AND l_days <= p_date6.
          p_perid = c_d006.
        ELSEIF l_days > p_date6 AND l_days <= p_date7.
          p_perid = c_d007.
        ELSE.
          p_perid = c_d009.
        ENDIF.
      WHEN OTHERS.
        IF l_days =< p_date1.
          p_perid = c_d001.
        ELSEIF l_days > p_date1 AND l_days <= p_date2.
          p_perid = c_d002.
        ELSEIF l_days > p_date2 AND l_days <= p_date3.
          p_perid = c_d003.
        ELSEIF l_days > p_date3 AND l_days <= p_date4.
          p_perid = c_d004.
        ELSEIF l_days > p_date4 AND l_days <= p_date5.
          p_perid = c_d005.
        ELSEIF l_days > p_date5 AND l_days <= p_date6.
          p_perid = c_d006.
        ELSEIF l_days > p_date6 AND l_days <= p_date7.
          p_perid = c_d007.
        ELSEIF l_days > p_date7 AND l_days <= p_date8.
          p_perid = c_d008.
        ELSE.
          p_perid = c_d009.
        ENDIF.
    ENDCASE.
  ELSE.
    p_perid = c_d000.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_vendor .
  DATA: lit_lifnr TYPE STANDARD TABLE OF x_lfa1,
        lwa_lifnr TYPE x_lfa1.
  DATA: liw_t077k TYPE x_t077k.
  DATA: lit_t077k TYPE STANDARD TABLE OF x_t077k.
  DATA lt_lfbk TYPE TABLE OF lfbk.
  DATA ls_lfbk TYPE  lfbk.

  DATA:lt_bnka TYPE TABLE OF bnka.
  DATA:ls_bnka TYPE   bnka.
  DATA: lw_bseg TYPE x_bseg.
  DATA: lt_bseg TYPE TABLE OF x_bseg.
  DATA: l_line TYPE sy-tabix.

  CLEAR:it_acc_ven[].
*根据供应商未清项
  SELECT bsik~bukrs
         bsik~lifnr
         bsik~umskz
         bsik~augdt
         bsik~augbl
         bsik~gjahr
         bsik~belnr
         bsik~buzei
         bsik~budat
         bsik~waers
         bsik~bschl
         bsik~shkzg
         bsik~dmbtr
         bsik~wrbtr
         bsik~sgtxt
         bsik~hkont
         bsik~zfbdt
         bsik~zterm
         bsik~zbd1t
         bsik~zbd2t
         bsik~zbd3t
         bsik~rebzg
         bsik~xnegp
         bsik~gsber
         bsik~zuonr
         bsik~bldat
    INTO CORRESPONDING FIELDS OF TABLE it_acc_ven
    FROM bsik
    INNER JOIN bkpf ON bsik~belnr EQ bkpf~belnr
                   AND bsik~bukrs EQ bkpf~bukrs
                   AND bsik~gjahr EQ bkpf~gjahr
   WHERE bsik~bukrs IN r_bukrs
     AND bsik~lifnr IN s_lifnr
     AND bsik~hkont IN s_hkont
     AND bsik~gsber IN s_gsber
     AND bkpf~budat LE p_budat
     AND bkpf~bstat NOT IN ('M','S','V','W','Z').

*获取供应商的已清项

  SELECT bsak~bukrs
         bsak~lifnr
         bsak~umskz
         bsak~augdt
         bsak~augbl
         bsak~gjahr
         bsak~belnr
         bsak~buzei
         bsak~budat
         bsak~waers
         bsak~bschl
         bsak~shkzg
         bsak~dmbtr
         bsak~wrbtr
         bsak~sgtxt
         bsak~hkont
         bsak~zfbdt
         bsak~zterm
         bsak~zbd1t
         bsak~zbd2t
         bsak~zbd3t
         bsak~rebzg
         bsak~xnegp
         bsak~gsber
         bsak~zuonr
         bsak~bldat
    APPENDING CORRESPONDING FIELDS OF TABLE it_acc_ven
    FROM bsak
    INNER JOIN bkpf ON bsak~belnr EQ bkpf~belnr
                   AND bsak~bukrs EQ bkpf~bukrs
                   AND bsak~gjahr EQ bkpf~gjahr
   WHERE bsak~bukrs IN r_bukrs
     AND bsak~lifnr IN s_lifnr
     AND bsak~hkont IN s_hkont
     AND bsak~gsber IN s_gsber
     AND bkpf~budat LE p_budat
     AND bsak~augdt GT p_budat
     AND bkpf~bstat NOT IN ('M','S','V','W','Z').
  "*按照币种排除数据，如果选择了本币，则排除，如果选择交易货币，不做处理
  "  if it_acc_ven[] is not initial.
  "*币种为本币
  "    if r_3 eq 'X'.
  "      clear wa_t001.
  "      loop at it_t001 into wa_t001.
  "        delete it_acc_ven where bukrs eq wa_t001-bukrs and waers ne wa_t001-waers.
  "      endloop.
  "    endif.
  "*币种为货币
  "    if r_4 eq 'X'.

  "    endif.
  "  endif.

*当选择汇总模式时,排除数据
  IF r_2 EQ 'X'.
    IF it_acc_ven[] IS NOT INITIAL.
      IF c_1 EQ 'X' AND s_prctr[] IS NOT INITIAL.
*取科目类型和利润中心
        SELECT bukrs
               belnr
               gjahr
               buzei
               koart
               prctr
          INTO CORRESPONDING FIELDS OF TABLE lt_bseg
          FROM bseg
          FOR ALL ENTRIES IN it_acc_ven
         WHERE bukrs EQ it_acc_ven-bukrs
           AND belnr EQ it_acc_ven-belnr
           AND gjahr EQ it_acc_ven-gjahr
           AND buzei EQ it_acc_ven-buzei
           AND prctr IN s_prctr.
        SORT lt_bseg.
        DELETE ADJACENT DUPLICATES FROM lt_bseg.
*如果某条数据的利润中心不在选择屏幕的利润中心参数中，则排除掉。
        CLEAR wa_acc_ven.
        LOOP AT it_acc_ven INTO wa_acc_ven.
          l_line = sy-tabix.
          READ TABLE lt_bseg INTO lw_bseg WITH KEY bukrs = wa_acc_ven-bukrs belnr = wa_acc_ven-belnr
                                                gjahr = wa_acc_ven-gjahr buzei = wa_acc_ven-buzei.

          IF sy-subrc EQ 0.

          ELSE.
*排除不符合要求的利润中心
            DELETE it_acc_ven INDEX l_line.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.


*获取表应付暂估的数据
  PERFORM f_get_bsis.
*取供应商的描述和账户组
  IF it_acc_ven[] IS NOT INITIAL.
    SORT it_acc_ven BY lifnr.
    SELECT lifnr
           name1
           ktokk
           land1
      INTO TABLE lit_lifnr
      FROM lfa1
       FOR ALL ENTRIES IN it_acc_ven
     WHERE lifnr = it_acc_ven-lifnr.
  ENDIF.

*获取供应商账户组描述
  IF lit_lifnr IS NOT INITIAL.
    SELECT ktokk
           txt30
      INTO TABLE lit_t077k
      FROM t077y
       FOR ALL ENTRIES IN lit_lifnr
     WHERE ktokk EQ lit_lifnr-ktokk
       AND spras EQ 1.
  ENDIF.

  IF lit_lifnr[] IS NOT INITIAL.
    SORT lit_lifnr BY lifnr.
    SELECT *
      FROM lfbk
      INTO TABLE lt_lfbk
      FOR ALL ENTRIES IN lit_lifnr
    WHERE lifnr = lit_lifnr-lifnr.

    IF lt_lfbk IS NOT INITIAL.
      LOOP AT lt_lfbk INTO ls_lfbk   .
        ls_bnka-banks = ls_lfbk-banks.
        ls_bnka-bankl = ls_lfbk-bankl.
        APPEND ls_bnka TO lt_bnka.
      ENDLOOP.

      SORT lt_bnka.
      DELETE ADJACENT DUPLICATES FROM lt_bnka.
      SELECT *
        INTO TABLE lt_bnka
        FROM bnka
         FOR ALL ENTRIES IN lt_bnka
       WHERE banks = lt_bnka-banks
         AND bankl = lt_bnka-bankl.
    ENDIF.


    LOOP AT it_acc_ven INTO wa_acc_ven.

* 银行帐号
      CLEAR:ls_lfbk.
      READ TABLE lt_lfbk INTO ls_lfbk WITH   KEY lifnr = wa_acc_ven-lifnr.
      wa_acc_ven-bankl =  ls_lfbk-bankl.

* 银行名称
      CLEAR:ls_bnka.
      READ TABLE lt_bnka INTO ls_bnka WITH   KEY bankl = wa_acc_ven-bankl.
      wa_acc_ven-banka = ls_bnka-banka.
* 银行帐号
      wa_acc_ven-banno = ls_lfbk-bankn && ls_lfbk-bkref.
      CLEAR lwa_lifnr.
*  读取供应商描述和账户组信息
      READ TABLE lit_lifnr INTO lwa_lifnr
      WITH KEY lifnr = wa_acc_ven-lifnr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_acc_ven-namev = lwa_lifnr-name1.
        wa_acc_ven-ktokk = lwa_lifnr-ktokk.
        wa_acc_ven-land1 = lwa_lifnr-land1.
        READ TABLE lit_t077k INTO liw_t077k WITH KEY ktokk = lwa_lifnr-ktokk.
        IF sy-subrc EQ 0.
          wa_acc_ven-gzhms = liw_t077k-gzhms.
        ENDIF.
      ENDIF.
*  根据贷方标识计算负金额
      IF wa_acc_ven-shkzg = c_h.
        wa_acc_ven-wrbtr =  0 - wa_acc_ven-wrbtr.
        wa_acc_ven-dmbtr = 0 - wa_acc_ven-dmbtr.
      ENDIF.
      "*  计算净收付到期日
      IF wa_acc_ven-fbsis = c_x.
        wa_acc_ven-dudat = wa_acc_ven-budat.
      ELSE.
*计算净收付到期日
*以基准日期计算:BSId/BSAd-ZFBDT   =  到期日期
        IF r_5 EQ 'X'.
          wa_acc_ven-dudat = wa_acc_ven-zfbdt.
        ENDIF.
*以付款条件计算:BSId/BSAd-ZFBDT  +  天数BSIK/BSAK-ZBD1T   =  到期日期
        IF r_6 EQ 'X'.
          wa_acc_ven-dudat = wa_acc_ven-zfbdt + wa_acc_ven-zbd1t.
        ENDIF.
      ENDIF.

*计算区间的标志
      PERFORM f_calcu_period USING wa_acc_ven-dudat CHANGING wa_acc_ven-perid wa_acc_ven-ducdt.
*读取本位币和账目表
      READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_acc_ven-bukrs
      BINARY SEARCH.
      IF sy-subrc = 0.
        wa_acc_ven-waerb = wa_t001-waers.
        wa_acc_ven-ktopl = wa_t001-ktopl.
      ENDIF.
      wa_acc_ven-xtype = c_vend.
      MODIFY it_acc_ven FROM wa_acc_ven TRANSPORTING namev ktokk gzhms wrbtr dmbtr perid waerb ktopl xtype dudat ducdt bankl banka banno land1.
      CLEAR wa_acc_ven.
    ENDLOOP.
  ENDIF.






ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_BSIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_bsis .
  DATA: lwa_bsis TYPE x_bsis.
  DATA: lit_bsis TYPE STANDARD TABLE OF x_bsis.
  DATA: lw_bseg TYPE x_bseg.
  DATA: lt_bseg TYPE TABLE OF x_bseg.
  DATA: l_line TYPE sy-tabix.

  DATA: BEGIN OF lit_po OCCURS 0,
          bukrs TYPE bukrs,
          belnr TYPE belnr_d,
          gjahr TYPE gjahr,
          ebeln TYPE ebeln,
        END OF lit_po.
  DATA: lit_tmp LIKE TABLE OF lit_po WITH HEADER LINE.
  DATA: BEGIN OF lit_tp OCCURS 0,
          ebeln TYPE ebeln,
          lifnr TYPE lifnr,
        END OF lit_tp.
  DATA: lv_lifnr TYPE lfb1-lifnr.


*获取应收暂估的未清项数据
  CLEAR it_bsis[].
*  增加科目
*  SELECT bsis~bukrs
*         bsis~augdt
*         bsis~augbl
*         bsis~zuonr
*         bsis~gjahr
*         bsis~belnr
*         bsis~buzei
*         bsis~budat
*         bsis~waers
*         bsis~bschl
*         bsis~shkzg
*         bsis~dmbtr
*         bsis~wrbtr
*         bsis~sgtxt
*         bsis~hkont
*         bsis~zfbdt
*         bsis~xnegp
*         bsis~gsber
*         bsis~bldat
*    INTO CORRESPONDING FIELDS OF TABLE it_bsis
*    FROM bsis
*    INNER JOIN bkpf ON bsis~belnr EQ bkpf~belnr
*                   AND bsis~bukrs EQ bkpf~bukrs
*                   AND bsis~gjahr EQ bkpf~gjahr
*   WHERE bsis~bukrs IN r_bukrs
*     AND bsis~hkont EQ '2202020100'
*     AND bsis~gsber IN s_gsber
*     AND bkpf~budat LE p_budat
*     AND bkpf~bstat NOT IN ('M','S','V','W','Z').


  SELECT bsis~bukrs
        bsis~augdt
        bsis~augbl
        bsis~zuonr
        bsis~gjahr
        bsis~belnr
        bsis~buzei
        bsis~budat
        bsis~waers
        bsis~bschl
        bsis~shkzg
        bsis~dmbtr
        bsis~wrbtr
        bsis~sgtxt
        bsis~hkont
        bsis~zfbdt
        bsis~xnegp
        bsis~gsber
        bsis~bldat
   INTO CORRESPONDING FIELDS OF TABLE it_bsis
   FROM bsis
*   INNER JOIN bseg ON bsis~belnr EQ bseg~belnr
*                  AND bsis~bukrs EQ bseg~bukrs
*                  AND bsis~gjahr EQ bseg~gjahr
*                  AND bsis~bschl EQ bseg~bschl
*                  AND bsis~hkont EQ bseg~hkont
   INNER JOIN bkpf ON bsis~belnr EQ bkpf~belnr
                  AND bsis~bukrs EQ bkpf~bukrs
                  AND bsis~gjahr EQ bkpf~gjahr
  WHERE bsis~mandt = sy-mandt
    AND bsis~bukrs IN r_bukrs
    AND bsis~hkont IN ( '2202020100','2202020200' )
    AND bsis~gsber IN s_gsber
    AND bkpf~budat LE p_budat
    AND bkpf~bstat NOT IN ('M','S','V','W','Z').

*  IF it_bsis[] IS NOT INITIAL.
*    DATA:gt_bseg LIKE TABLE OF bseg WITH HEADER LINE.
*    DATA: gt_bsis TYPE TABLE OF x_bsis WITH HEADER LINE.   "BSIS内表
*    DATA:i_bsis TYPE x_bsis.
*    SELECT *
*      INTO CORRESPONDING FIELDS OF TABLE gt_bseg
*      FROM bseg
*      FOR ALL ENTRIES IN it_bsis
*      WHERE bseg~belnr EQ it_bsis-belnr
*        AND bseg~bukrs EQ it_bsis-bukrs
*        AND bseg~gjahr EQ it_bsis-gjahr
*        AND bseg~bschl EQ it_bsis-bschl
*        AND bseg~hkont EQ it_bsis-hkont.
*    LOOP AT it_bsis INTO i_bsis.
*      READ TABLE gt_bseg WITH KEY belnr = i_bsis-belnr bukrs = i_bsis-bukrs gjahr = i_bsis-gjahr bschl = i_bsis-bschl hkont = i_bsis-hkont.
*      IF sy-subrc <> 0.
*        APPEND i_bsis TO gt_bsis.
*      ENDIF.
*    ENDLOOP.
*    LOOP AT gt_bsis.
*      DELETE it_bsis WHERE  belnr EQ gt_bsis-belnr
*                        AND bukrs EQ gt_bsis-bukrs
*                        AND gjahr EQ gt_bsis-gjahr
*                        AND bschl EQ gt_bsis-bschl
*                        AND hkont EQ gt_bsis-hkont.
*    ENDLOOP.
*    REFRESH :gt_bsis,gt_bseg.
*  ENDIF.


*获取应收暂估的已清项数据
*  SELECT bsas~bukrs
*         bsas~augdt
*         bsas~augbl
*         bsas~zuonr
*         bsas~gjahr
*         bsas~belnr
*         bsas~buzei
*         bsas~budat
*         bsas~waers
*         bsas~bschl
*         bsas~shkzg
*         bsas~dmbtr
*         bsas~wrbtr
*         bsas~sgtxt
*         bsas~hkont
*         bsas~zfbdt
*         bsas~xnegp
*         bsas~gsber
*         bsas~bldat
*    APPENDING CORRESPONDING FIELDS OF TABLE it_bsis
*    FROM bsas
*    INNER JOIN bkpf ON bsas~belnr EQ bkpf~belnr
*                   AND bsas~bukrs EQ bkpf~bukrs
*                   AND bsas~gjahr EQ bkpf~gjahr
*   WHERE bsas~bukrs IN r_bukrs
*     AND bsas~hkont EQ '2202020100'
*     AND bsas~gsber IN s_gsber
*     AND bkpf~budat LE p_budat
*     AND bsas~augdt GT p_budat
*     AND bkpf~bstat NOT IN ('M','S','V','W','Z').

  SELECT bsas~bukrs
          bsas~augdt
          bsas~augbl
          bsas~zuonr
          bsas~gjahr
          bsas~belnr
          bsas~buzei
          bsas~budat
          bsas~waers
          bsas~bschl
          bsas~shkzg
          bsas~dmbtr
          bsas~wrbtr
          bsas~sgtxt
          bsas~hkont
          bsas~zfbdt
          bsas~xnegp
          bsas~gsber
          bsas~bldat
     APPENDING CORRESPONDING FIELDS OF TABLE it_bsis
     FROM bsas
     INNER JOIN bkpf ON bsas~belnr EQ bkpf~belnr
                    AND bsas~bukrs EQ bkpf~bukrs
                    AND bsas~gjahr EQ bkpf~gjahr
    WHERE bsas~bukrs IN r_bukrs
      AND bsas~hkont IN ( '2202020100','2202020200' )
      AND bsas~gsber IN s_gsber
      AND bkpf~budat LE p_budat
      AND bsas~augdt GT p_budat
      AND bkpf~bstat NOT IN ('M','S','V','W','Z').






  "*按照币种排除数据
  "  if it_bsis[] is not initial.
  "*币种为本币
  "    if r_3 eq 'X'.
  "      clear wa_t001.
  "      loop at it_t001 into wa_t001.
  "        delete it_bsis where bukrs eq wa_t001-bukrs and waers ne wa_t001-waers.
  "      endloop.
  "    endif.
  "*币种为货币
  "    if r_4 eq 'X'.

  "    endif.
  "  endif.

*当选择汇总模式时,排除数据
  IF r_2 EQ 'X'.
    IF it_bsis[] IS NOT INITIAL.
      IF c_1 EQ 'X' AND s_prctr[] IS NOT INITIAL.
*取科目类型和利润中心
        SELECT bukrs
               belnr
               gjahr
               buzei
               koart
               prctr
          INTO CORRESPONDING FIELDS OF TABLE lt_bseg
          FROM bseg
          FOR ALL ENTRIES IN it_bsis
         WHERE bukrs EQ it_bsis-bukrs
           AND belnr EQ it_bsis-belnr
           AND gjahr EQ it_bsis-gjahr
           AND buzei EQ it_bsis-buzei
           AND prctr IN s_prctr.
        SORT lt_bseg.
        DELETE ADJACENT DUPLICATES FROM lt_bseg.

        CLEAR wa_bsis.
        LOOP AT it_bsis INTO wa_bsis.
          l_line = sy-tabix.
          READ TABLE lt_bseg INTO lw_bseg WITH KEY bukrs = wa_bsis-bukrs belnr = wa_bsis-belnr
                                                gjahr = wa_bsis-gjahr buzei = wa_bsis-buzei.

          IF sy-subrc EQ 0.

          ELSE.
*排除不符合要求的利润中心
            DELETE it_bsis INDEX l_line.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.


  lit_bsis[] = it_bsis[].
  SORT lit_bsis BY bukrs belnr gjahr.
  DELETE ADJACENT DUPLICATES FROM lit_bsis COMPARING bukrs belnr gjahr.
  CHECK NOT lit_bsis[] IS INITIAL.
  SELECT bukrs
         belnr
         gjahr
         ebeln
    INTO TABLE lit_po
    FROM bseg
     FOR ALL ENTRIES IN lit_bsis
   WHERE bukrs EQ lit_bsis-bukrs
     AND belnr EQ lit_bsis-belnr
     AND gjahr EQ lit_bsis-gjahr
     AND ebeln NE ''.
  lit_tmp[] = lit_po[].
  SORT lit_tmp BY ebeln.
  DELETE ADJACENT DUPLICATES FROM lit_tmp COMPARING ebeln.

  "check not lit_tmp[] is initial.
  IF lit_tmp[] IS NOT INITIAL.
    SELECT ebeln
         lifnr
    INTO TABLE lit_tp
    FROM ekko
     FOR ALL ENTRIES IN lit_tmp
   WHERE ebeln EQ lit_tmp-ebeln.
    REFRESH: lit_bsis,
             lit_tmp.
    SORT lit_po BY bukrs belnr gjahr.
    SORT lit_tp BY ebeln.
  ENDIF.


*将应付暂估的数据转移到BSIK中
  LOOP AT it_bsis INTO lwa_bsis.
    IF lwa_bsis-zuonr+0(1) NE '4'."lwa_bsis-zuonr+0(2) eq '92'.
      lv_lifnr = lwa_bsis-zuonr+0(10).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_lifnr
        IMPORTING
          output = lv_lifnr.

      IF lv_lifnr IN s_lifnr.
        MOVE-CORRESPONDING lwa_bsis TO wa_acc_ven.
        wa_acc_ven-fbsis = c_x.
        "wa_acc_ven-lifnr = lwa_bsis-zuonr+0(10).
        wa_acc_ven-lifnr = lv_lifnr.
        APPEND wa_acc_ven TO it_acc_ven.
        CLEAR:
        wa_acc_ven,
        lwa_bsis.
      ENDIF.
      CLEAR lv_lifnr.
    ELSE.
      CLEAR lit_po.
      READ TABLE lit_po WITH KEY bukrs = lwa_bsis-bukrs
      belnr = lwa_bsis-belnr
      gjahr = lwa_bsis-gjahr BINARY SEARCH.
      IF sy-subrc = 0.
        CLEAR lit_tp.
        READ TABLE lit_tp WITH KEY ebeln = lit_po-ebeln BINARY SEARCH.
        IF sy-subrc = 0.
          IF lit_tp-lifnr IN s_lifnr.
            MOVE-CORRESPONDING lwa_bsis TO wa_acc_ven.
            wa_acc_ven-fbsis = c_x.
            wa_acc_ven-lifnr = lit_tp-lifnr.
            APPEND wa_acc_ven TO it_acc_ven.
            CLEAR:
            wa_acc_ven,
            lwa_bsis.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MERGE_CUS_VEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_merge_cus_ven .
  DATA: lit_skat TYPE STANDARD TABLE OF x_skat,
        lwa_skat TYPE x_skat.
  DATA: lw_bseg TYPE x_bseg.
  DATA: lt_bseg TYPE TABLE OF x_bseg.
  DATA: l_num TYPE i.
  DATA: lw_bukrs TYPE x_bukrs.
  DATA: lt_bukrs TYPE TABLE OF x_bukrs.
  DATA: lt_prctr TYPE TABLE OF x_prctr.
  DATA: lw_prctr TYPE x_prctr.
  DATA: lt_tgsbt TYPE TABLE OF tgsbt.
  DATA: lw_tgsbt TYPE tgsbt.
  DATA: lw_guojia TYPE x_guojia.
  DATA: lt_guojia TYPE TABLE OF x_guojia.
  DATA: lt_bkpf TYPE TABLE OF x_bkpf.
  DATA: lw_bkpf TYPE x_bkpf.
  DATA: lw_t001 TYPE x_t001.


  CLEAR it_head.

  LOOP AT it_acc_cus INTO wa_acc_cus.
    MOVE-CORRESPONDING wa_acc_cus TO wa_account.
    APPEND wa_account TO it_account.
    CLEAR wa_account.
  ENDLOOP.
  LOOP AT it_acc_ven INTO wa_acc_ven.
    MOVE-CORRESPONDING wa_acc_ven TO wa_account.
    APPEND wa_account TO it_account.
    CLEAR wa_account.
  ENDLOOP.

  IF it_account[] IS INITIAL.
    MESSAGE s110(f0) DISPLAY LIKE c_msg_e.
    LEAVE LIST-PROCESSING.
  ENDIF.

*取会计科目描述
  SELECT spras
         ktopl
         saknr
         txt50
    INTO TABLE lit_skat
    FROM skat
     FOR ALL ENTRIES IN it_account
   WHERE spras EQ sy-langu
     AND ktopl EQ it_account-ktopl
     AND saknr EQ it_account-hkont.
  SORT lit_skat BY ktopl saknr.



  IF it_acc_cus IS NOT INITIAL.
*公司代码描述
    SELECT bukrs
           butxt
      INTO TABLE lt_bukrs
      FROM t001
      FOR ALL ENTRIES IN it_acc_cus
     WHERE bukrs EQ it_acc_cus-bukrs.
    SORT lt_bukrs.
    DELETE ADJACENT DUPLICATES FROM lt_bukrs.

*取科目类型和利润中心
    SELECT bukrs
           belnr
           gjahr
           buzei
           koart
           prctr
      INTO TABLE lt_bseg
      FROM bseg
      FOR ALL ENTRIES IN it_acc_cus
     WHERE bukrs EQ it_acc_cus-bukrs
       AND belnr EQ it_acc_cus-belnr
       AND gjahr EQ it_acc_cus-gjahr
       AND buzei EQ it_acc_cus-buzei.
    SORT lt_bseg.
    DELETE ADJACENT DUPLICATES FROM lt_bseg.
*取利润中心描述
    IF lt_bseg IS NOT INITIAL.
      SELECT prctr
             ltext
        INTO TABLE lt_prctr
        FROM cepct
         FOR ALL ENTRIES IN lt_bseg
       WHERE prctr EQ lt_bseg-prctr
         AND spras EQ 1.
      SORT lt_prctr.
      DELETE ADJACENT DUPLICATES FROM lt_prctr.
    ENDIF.
*取业务范围描述
    SELECT *
      INTO TABLE lt_tgsbt
      FROM tgsbt
       FOR ALL ENTRIES IN it_acc_cus
     WHERE gsber EQ it_acc_cus-gsber
       AND spras = 1.
*取国家描述
    IF it_t001 IS NOT INITIAL.
      SELECT land1
             landx
        INTO TABLE lt_guojia
        FROM t005t
        FOR ALL ENTRIES IN it_acc_cus
       WHERE land1 EQ it_acc_cus-land1
         AND spras EQ 1.
      SORT lt_guojia.
      DELETE ADJACENT DUPLICATES FROM lt_guojia.
    ENDIF.
*取事务代码
    SELECT bukrs
           belnr
           gjahr
           tcode
           blart
           xblnr_alt
      INTO CORRESPONDING FIELDS OF TABLE lt_bkpf
      FROM bkpf
       FOR ALL ENTRIES IN it_acc_cus
     WHERE bukrs EQ it_acc_cus-bukrs
       AND belnr EQ it_acc_cus-belnr
       AND gjahr EQ it_acc_cus-gjahr.
    SORT lt_bkpf.
    DELETE ADJACENT DUPLICATES FROM lt_bkpf.



    LOOP AT it_acc_cus INTO wa_acc_cus.
      CLEAR wa_head.
*ADD by PENGLIXUE at 20180224 BPMNO: ERP-YW201802020036
      wa_head-kkber = wa_acc_cus-kkber.   "信贷控制范围
      IF wa_head-kkber  IS NOT INITIAL.
        SELECT SINGLE kkbtx INTO wa_head-kkbtx
          FROM t014t
          WHERE kkber = wa_head-kkber AND spras  = sy-langu.
      ENDIF.
*end of ADD by PENGLIXUE
      wa_head-ktokd = wa_acc_cus-ktokd.   "客户账户组
      wa_head-kzhms = wa_acc_cus-kzhms.   "客户账户组描述
      wa_head-kunnr = wa_acc_cus-kunnr.   "客户编码
      wa_head-namec = wa_acc_cus-namec.   "客户描述
      wa_head-ksbm  = wa_acc_cus-kunnr.   "客商编码
      wa_head-ksms  = wa_acc_cus-namec.   "客商编码描述
*国家描述
      "      clear wa_t001.
      "      read table it_t001 into wa_t001 with key bukrs = wa_acc_cus-bukrs.
      "      if sy-subrc eq 0.
      "        read table lt_guojia into lw_guojia with key land1 = wa_t001-land1.
      "        wa_head-guoj = lw_guojia-landx.
      "      endif.
      READ TABLE lt_guojia INTO lw_guojia WITH KEY land1 = wa_acc_cus-land1.
      IF sy-subrc EQ 0.
        wa_head-guoj = lw_guojia-landx.
      ENDIF.

*科目类型
      READ TABLE lt_bseg INTO lw_bseg WITH KEY bukrs = wa_acc_cus-bukrs belnr = wa_acc_cus-belnr
                                                gjahr = wa_acc_cus-gjahr buzei = wa_acc_cus-buzei.

      IF sy-subrc EQ 0.
        wa_head-koart = lw_bseg-koart.
*利润中心
        wa_head-prctr = lw_bseg-prctr.
      ENDIF.
*利润中心描述
      IF wa_head-prctr NE ''.
        READ TABLE lt_prctr INTO lw_prctr WITH KEY prctr = wa_head-prctr.
        IF sy-subrc EQ 0.
          wa_head-prctt = lw_prctr-ltext.
        ENDIF.
      ENDIF.


*总账科目
      wa_head-hkont = wa_acc_cus-hkont.
*读取会计科目描述
      READ TABLE lit_skat INTO lwa_skat WITH KEY ktopl = wa_acc_cus-ktopl saknr = wa_acc_cus-hkont BINARY SEARCH.
      IF sy-subrc = 0.
        wa_head-txt50 = lwa_skat-txt50.
      ENDIF.

*一级科目
      wa_head-klev1 = wa_acc_cus-hkont+0(4).
*一级科目描述
      SEARCH wa_head-txt50 FOR '-'.
      IF sy-fdpos NE 0.
        l_num         = sy-fdpos.
        wa_head-yjkmt = wa_head-txt50+0(l_num).
      ELSE.
        wa_head-yjkmt = wa_head-txt50.
      ENDIF.
      CLEAR l_num.
*SGL
      wa_head-sgl = wa_acc_cus-umskz.
*公司代码
      wa_head-bukrs = wa_acc_cus-bukrs.
*公司描述
      READ TABLE lt_bukrs INTO lw_bukrs WITH KEY bukrs = wa_acc_cus-bukrs.
      IF sy-subrc EQ 0.
        wa_head-butxt = lw_bukrs-butxt.
      ENDIF.
*业务范围
      wa_head-gsber = wa_acc_cus-gsber.
*业务范围描述
      READ TABLE lt_tgsbt INTO lw_tgsbt WITH KEY gsber = wa_acc_cus-gsber.
      IF sy-subrc EQ 0.
        wa_head-gtext = lw_tgsbt-gtext.
      ENDIF.
*会计凭证
      wa_head-belnr = wa_acc_cus-belnr.
*会计凭证行
      wa_head-buzei = wa_acc_cus-buzei.
*会计年度
      wa_head-gjahr = wa_acc_cus-gjahr.
*事务代码
      READ TABLE lt_bkpf INTO lw_bkpf WITH KEY bukrs = wa_acc_cus-bukrs belnr = wa_acc_cus-belnr
                                                                          gjahr = wa_acc_cus-gjahr.

      IF sy-subrc EQ 0.
        wa_head-tcode = lw_bkpf-tcode.
*凭证类型
        wa_head-blart = lw_bkpf-blart.
*按月凭证编号
        wa_head-xblnr_alt = lw_bkpf-xblnr_alt.
      ENDIF.
*分配编号
      wa_head-zuonr = wa_acc_cus-zuonr.
*项目文本
      wa_head-sgtxt = wa_acc_cus-sgtxt.
*凭证日期
      wa_head-bldat = wa_acc_cus-bldat.
*过账日期
      wa_head-budat = wa_acc_cus-budat.
*到期日
      wa_head-dudat = wa_acc_cus-dudat.
*清账凭证号
      wa_head-augbl = wa_acc_cus-augbl.
*清账日期
      wa_head-augdt = wa_acc_cus-augdt.
*账龄类型
      wa_head-xtype = wa_acc_cus-xtype.
*客商余额
      IF r_3 EQ 'X'.
        wa_head-ksye = wa_acc_cus-dmbtr.
      ENDIF.
      IF r_4 EQ 'X'.
        wa_head-ksye = wa_acc_cus-wrbtr.
      ENDIF.
      CASE wa_acc_cus-perid.
*未到期余额
        WHEN c_d000.
          IF r_3 EQ 'X'.
            wa_head-wdqje = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-wdqje = wa_acc_cus-wrbtr.
          ENDIF.
*30天以内
        WHEN c_d001.
          IF r_3 EQ 'X'.
            wa_head-dmbt1 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt1 = wa_acc_cus-wrbtr.
          ENDIF.
*31到60
        WHEN c_d002.
          IF r_3 EQ 'X'.
            wa_head-dmbt2 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt2 = wa_acc_cus-wrbtr.
          ENDIF.
*61到90
        WHEN c_d003.
          IF r_3 EQ 'X'.
            wa_head-dmbt3 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt3 = wa_acc_cus-wrbtr.
          ENDIF.
*91到120
        WHEN c_d004.
          IF r_3 EQ 'X'.
            wa_head-dmbt4 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt4 = wa_acc_cus-wrbtr.
          ENDIF.
*120到180
        WHEN c_d005.
          IF r_3 EQ 'X'.
            wa_head-dmbt5 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt5 = wa_acc_cus-wrbtr.
          ENDIF.
*180到360
        WHEN c_d006.
          IF r_3 EQ 'X'.
            wa_head-dmbt6 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt6 = wa_acc_cus-wrbtr.
          ENDIF.
*360到720
        WHEN c_d007.
          IF r_3 EQ 'X'.
            wa_head-dmbt7 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt7 = wa_acc_cus-wrbtr.
          ENDIF.
*720到1440
        WHEN c_d008.
          IF r_3 EQ 'X'.
            wa_head-dmbt8 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt8 = wa_acc_cus-wrbtr.
          ENDIF.
*大于1440
        WHEN c_d009.
          IF r_3 EQ 'X'.
            wa_head-dmbt9 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt9 = wa_acc_cus-wrbtr.
          ENDIF.
      ENDCASE.

*账龄金额
      wa_head-zlje = wa_head-ksye - wa_head-dmbt0.
*币种
      IF r_3 EQ 'X'.
        READ TABLE it_t001 INTO lw_t001 WITH KEY bukrs = wa_acc_cus-bukrs.
        IF sy-subrc EQ 0.
          wa_head-waers = lw_t001-waers.
        ENDIF.
        CLEAR lw_t001.
      ENDIF.
      IF r_4 EQ 'X'.
        wa_head-waers = wa_acc_cus-waers.
      ENDIF.


      APPEND wa_head TO it_head.
      CLEAR wa_head.
    ENDLOOP.
  ENDIF.








ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_show_data .
*  组织需要显示的字段
  PERFORM f_fieldcat_init.
*  显示汇总ALV
  PERFORM f_show_alv.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fieldcat_init .
  DATA:lv_dmbt1 TYPE char40.
  DATA:lv_dmbt2 TYPE char40.
  DATA:lv_dmbt3 TYPE char40.
  DATA:lv_dmbt4 TYPE char40.
  DATA:lv_dmbt5 TYPE char40.
  DATA:lv_dmbt6 TYPE char40.
  DATA:lv_dmbt7 TYPE char40.
  DATA:lv_dmbt8 TYPE char40.
  DATA:lv_dmbt9 TYPE char40.

  DATA:p_dy1 TYPE idcn_segm.
  DATA:p_dy2 TYPE idcn_segm.
  DATA:p_dy3 TYPE idcn_segm.
  DATA:p_dy4 TYPE idcn_segm.
  DATA:p_dy5 TYPE idcn_segm.
  DATA:p_dy6 TYPE idcn_segm.
  DATA:p_dy7 TYPE idcn_segm.
  DATA:p_dy8 TYPE idcn_segm.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date1
    IMPORTING
      output = p_date1.

  p_dy1 = p_date1 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date2
    IMPORTING
      output = p_dy2.
  p_dy2 = p_date2 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date3
    IMPORTING
      output = p_dy3.

  p_dy3 = p_date3 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date4
    IMPORTING
      output = p_dy4.

  p_dy4 = p_date4 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date5
    IMPORTING
      output = p_dy5.

  p_dy5 = p_date5 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date6
    IMPORTING
      output = p_dy6.

  p_dy6 = p_date6 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date7
    IMPORTING
      output = p_dy7.

  p_dy7 = p_date7 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date8
    IMPORTING
      output = p_dy8.

  p_dy8 = p_date8 + 1.

  CONCATENATE:'账龄小于等于'p_date1 '天' INTO lv_dmbt1.
  CONCATENATE:'账龄'p_dy1 '天到' p_date2 '天' INTO lv_dmbt2.
  CONCATENATE:'账龄'p_dy2 '天到' p_date3 '天' INTO lv_dmbt3.
  CONCATENATE:'账龄'p_dy3 '天到' p_date4 '天' INTO lv_dmbt4.
  CONCATENATE:'账龄'p_dy4 '天到' p_date5 '天' INTO lv_dmbt5.
  CONCATENATE:'账龄'p_dy5 '天到' p_date6 '天' INTO lv_dmbt6.
  CONCATENATE:'账龄'p_dy6 '天到' p_date7 '天' INTO lv_dmbt7.
  CONCATENATE:'账龄'p_dy7 '天到' p_date8 '天' INTO lv_dmbt8.


  "  concatenate:'账龄大于等于' p_dy8 into lv_dmbt9.

*明细模式显示
  IF r_1 EQ 'X'.
    CLEAR it_fieldcat.
    PERFORM f_fieldcat_set TABLES it_fieldcat
    USING:  'KSBM'  TEXT-051 'X' '' '',  "客商编码
            'KSMS'  TEXT-052 'X' '' '',  "客商描述
            'BUKRS' TEXT-003 'X' '' '',  "公司代码
            'BUTXT' TEXT-057 'X' '' '',  "公司代码描述
            'GJAHR' TEXT-063 'X' '' '',  "会计年度
            'BELNR' TEXT-061 'X' 'X' '',  "会计凭证
            'BUZEI' TEXT-062 'X' 'X' '',  "会计凭证行
            'XBLNR_ALT' TEXT-082 '' '' '', "按月凭证编号
            'GUOJ'  TEXT-055 '' '' '',      "国家
            'KTOKD' TEXT-009 '' '' '',  "客户账户组
            'KZHMS' TEXT-049 '' '' '',  "客户账户组描述
            'KTOKK' TEXT-008 '' '' '',  "供应商账户组
            'GZHMS' TEXT-050 '' '' '',   "供应商账户组描述
            'KUNNR' TEXT-006 '' '' '',   "客户编码
            'NAMEC' TEXT-007 '' '' '',  "客户描述
            'LIFNR' TEXT-004 '' '' '',   "供应商编码
            'NAMEV' TEXT-005 '' '' '',   "供应商描述
            'KOART' TEXT-053 '' '' '',  "科目类型
            'KLEV1' TEXT-039 '' '' '',      "一级科目
            'YJKMT' TEXT-054 '' '' '',  "一级科目描述
            'HKONT' TEXT-010 '' '' '',  "会计帐目
*ADD by PENGLIXUE at 20180224 BPMNO: ERP-YW201802020036
            'KKBER' TEXT-083 '' '' '',  "信贷控制范围
            'KKBTX' TEXT-084 '' '' '',  "信贷范围描述
*end of ADD by PENGLIXUE
            'TXT50' TEXT-011 '' '' '', "科目描述
            'SGL'   TEXT-056 '' '' '',      "SGL
            'PRCTR' TEXT-058 '' '' '', "利润中心
            'PRCTT' TEXT-059 '' '' '', "利润中心描述
            'GSBER' TEXT-060 '' '' '', "业务范围
            'GTEXT' TEXT-080 '' '' '', "业务范围描述
            'BLART' TEXT-064 '' '' '',  "凭证类型
            'TCODE' TEXT-065 '' '' '', "事务代码
            'ZUONR' TEXT-066 '' '' '',  "分配
            'SGTXT' TEXT-067 '' '' '',  "项目文本
            'BLDAT' TEXT-068 '' '' '', "凭证日期
            'BUDAT' TEXT-069 '' '' '',  "过账日期
            'DUDAT' TEXT-070 '' '' '',  "到期日
            'AUGBL' TEXT-071 '' '' '',  "清账凭证号
            'AUGDT' TEXT-072 '' '' '',  "清账日期
            'KSYE ' TEXT-073 '' '' 'X', "客商余额
            'WDQJE' TEXT-074 '' '' 'X',  "未到期余额
            'ZLJE'  TEXT-075 '' '' 'X',  "账龄金额
            'WAERS' TEXT-076 '' '' '',  "币种
            'ZLJE_BB'  TEXT-085 '' '' 'X',  "账龄金额
            'WAERS_BB' TEXT-086 '' '' '',  "币种
            'XTYPE' TEXT-077 '' '' ''.       "账龄类型

  ENDIF.
*汇总模式显示
  IF r_2 EQ 'X'.
    CLEAR it_fieldcat.
*只勾选利润中心
    IF c_1 EQ 'X' AND c_2 NE 'X'.
      PERFORM f_fieldcat_set TABLES it_fieldcat
        USING: 'KSBM'  TEXT-051 'X' 'X' '',  "客商编码
            'KSMS'  TEXT-052 'X' '' '',  "客商描述
            'BUKRS' TEXT-003 'X' '' '',  "公司代码
            'BUTXT' TEXT-057 'X' '' '',  "公司代码描述
            'KLEV1' TEXT-039 'X' '' '',  "一级科目
            'YJKMT' TEXT-054 'X' '' '',  "一级科目描述
            'LIFNR' TEXT-004 '' '' '',   "供应商编码
            'NAMEV' TEXT-005 '' '' '',   "供应商描述
            'KUNNR' TEXT-006 '' '' '',   "客户编码
            'NAMEC' TEXT-007 '' '' '',  "客户描述
            'GUOJ'  TEXT-055 '' '' '',  "国家
            'KTOKK' TEXT-008 '' '' '',  "供应商账户组
            'GZHMS' TEXT-050 '' '' '',   "供应商账户组描述
            'KTOKD' TEXT-009 '' '' '',  "客户账户组
            'KZHMS' TEXT-049 '' '' '',  "客户账户组描述
            'KOART' TEXT-053 '' '' '',  "科目类型
            'HKONT' TEXT-010 '' '' '',  "会计帐目
            'TXT50' TEXT-011 '' '' '', "科目描述
            'PRCTR' TEXT-058 '' '' '', "利润中心
            'PRCTT' TEXT-059 '' '' '', "利润中心描述
            'ZLJE'  TEXT-075 '' '' 'X',  "总计，即时账龄金额
            'WAERS' TEXT-076 '' '' '',  "币种
            'ZLJE_BB'  TEXT-085 '' '' 'X',  "总计，即时账龄金额
            'WAERS_BB' TEXT-086 '' '' '',  "币种
            'WDQJE' TEXT-074 '' '' 'X'.  "未到期余额
      "            'XTYPE' text-077 '' '', "账龄类型

*只勾选业务范围
    ELSEIF c_1 NE 'X' AND c_2 EQ 'X'.
      PERFORM f_fieldcat_set TABLES it_fieldcat
        USING:  'KSBM'  TEXT-051 'X' 'X' '',  "客商编码
            'KSMS'  TEXT-052 'X' '' '',  "客商描述
            'BUKRS' TEXT-003 'X' '' '',  "公司代码
            'BUTXT' TEXT-057 'X' '' '',  "公司代码描述
            'KLEV1' TEXT-039 'X' '' '',  "一级科目
            'YJKMT' TEXT-054 'X' '' '',  "一级科目描述
            'LIFNR' TEXT-004 '' '' '',   "供应商编码
            'NAMEV' TEXT-005 '' '' '',   "供应商描述
            'KUNNR' TEXT-006 '' '' '',   "客户编码
            'NAMEC' TEXT-007 '' '' '',  "客户描述
            'GUOJ'  TEXT-055 '' '' '',  "国家
            'KTOKK' TEXT-008 '' '' '',  "供应商账户组
            'GZHMS' TEXT-050 '' '' '',   "供应商账户组描述
            'KTOKD' TEXT-009 '' '' '',  "客户账户组
            'KZHMS' TEXT-049 '' '' '',  "客户账户组描述
            'KOART' TEXT-053 '' '' '',  "科目类型
            'HKONT' TEXT-010 '' '' '',  "会计帐目
            'TXT50' TEXT-011 '' '' '', "科目描述
            'GSBER' TEXT-060 '' '' '', "业务范围
            'GTEXT' TEXT-080 '' '' '', "业务范围描述
            'ZLJE'  TEXT-075 '' '' 'X',  "总计，即时账龄金额
            'WAERS' TEXT-076 '' '' '',  "币种
             'ZLJE_BB'  TEXT-085 '' '' 'X',  "账龄金额
            'WAERS_BB' TEXT-086 '' '' '',  "币种
            'WDQJE' TEXT-074 '' '' 'X'.  "未到期余额
      "            'XTYPE' text-077 '' '', "账龄类型

*勾选利润中心和业务范围
    ELSEIF c_1 EQ 'X' AND c_2 EQ 'X'.
      PERFORM f_fieldcat_set TABLES it_fieldcat
        USING:  'KSBM'  TEXT-051 'X' 'X' '',  "客商编码
            'KSMS'  TEXT-052 'X' '' '',  "客商描述
            'BUKRS' TEXT-003 'X' '' '',  "公司代码
            'BUTXT' TEXT-057 'X' '' '',  "公司代码描述
            'KLEV1' TEXT-039 'X' '' '',  "一级科目
            'YJKMT' TEXT-054 'X' '' '',  "一级科目描述
            'LIFNR' TEXT-004 '' '' '',   "供应商编码
            'NAMEV' TEXT-005 '' '' '',   "供应商描述
            'KUNNR' TEXT-006 '' '' '',   "客户编码
            'NAMEC' TEXT-007 '' '' '',  "客户描述
            'GUOJ'  TEXT-055 '' '' '',  "国家
            'KTOKK' TEXT-008 '' '' '',  "供应商账户组
            'GZHMS' TEXT-050 '' '' '',   "供应商账户组描述
            'KTOKD' TEXT-009 '' '' '',  "客户账户组
            'KZHMS' TEXT-049 '' '' '',  "客户账户组描述
            'KOART' TEXT-053 '' '' '',  "科目类型
            'HKONT' TEXT-010 '' '' '',  "会计帐目
            'TXT50' TEXT-011 '' '' '', "科目描述
            'PRCTR' TEXT-058 '' '' '', "利润中心
            'PRCTT' TEXT-059 '' '' '', "利润中心描述
            'GSBER' TEXT-060 '' '' '', "业务范围
            'GTEXT' TEXT-080 '' '' '', "业务范围描述
            'ZLJE'  TEXT-075 '' '' 'X',  "总计，即时账龄金额
            'WAERS' TEXT-076 '' '' '',  "币种
             'ZLJE_BB'  TEXT-085 '' '' 'X',  "账龄金额
            'WAERS_BB' TEXT-086 '' '' '',  "币种
            'WDQJE' TEXT-074 '' '' 'X'.  "未到期余额
      "            'XTYPE' text-077 '' '', "账龄类型
*都不勾选利润中心和业务范围
    ELSEIF c_1 NE 'X' AND c_2 NE 'X'.
      PERFORM f_fieldcat_set TABLES it_fieldcat
        USING: 'KSBM'  TEXT-051 'X' 'X' '',  "客商编码
            'KSMS'  TEXT-052 'X' '' '',  "客商描述
            'BUKRS' TEXT-003 'X' '' '',  "公司代码
            'BUTXT' TEXT-057 'X' '' '',  "公司代码描述
            'KLEV1' TEXT-039 'X' '' '',  "一级科目
            'YJKMT' TEXT-054 'X' '' '',  "一级科目描述
            'LIFNR' TEXT-004 '' '' '',   "供应商编码
            'NAMEV' TEXT-005 '' '' '',   "供应商描述
            'KUNNR' TEXT-006 '' '' '',   "客户编码
            'NAMEC' TEXT-007 '' '' '',  "客户描述
            'GUOJ'  TEXT-055 '' '' '',  "国家
            'KTOKK' TEXT-008 '' '' '',  "供应商账户组
            'GZHMS' TEXT-050 '' '' '',   "供应商账户组描述
            'KTOKD' TEXT-009 '' '' '',  "客户账户组
            'KZHMS' TEXT-049 '' '' '',  "客户账户组描述
            'KOART' TEXT-053 '' '' '',  "科目类型
            'HKONT' TEXT-010 '' '' '',  "会计帐目
            'TXT50' TEXT-011 '' '' '', "科目描述
            'ZLJE'  TEXT-075 '' '' 'X',  "总计，即时账龄金额
            'WAERS' TEXT-076 '' '' '',  "币种
             'ZLJE_BB'  TEXT-085 '' '' 'X',  "账龄金额
            'WAERS_BB' TEXT-086 '' '' '',  "币种
            'WDQJE' TEXT-074 '' '' 'X'.  "未到期余额
      "            'XTYPE' text-077 '' '', "账龄类型

    ENDIF.
  ENDIF.

  IF p_date1 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT1' lv_dmbt1 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'账龄大于等于' p_dy1 INTO lv_dmbt9.
  ENDIF.
  IF p_date2 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT2' lv_dmbt2 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'账龄大于等于' p_dy2 INTO lv_dmbt9.
  ENDIF.
  IF p_date3 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT3' lv_dmbt3 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'账龄大于等于' p_dy3 INTO lv_dmbt9.
  ENDIF.
  IF p_date4 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT4' lv_dmbt4 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'账龄大于等于' p_dy4 INTO lv_dmbt9.
  ENDIF.
  IF p_date5 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT5' lv_dmbt5 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'账龄大于等于' p_dy5 INTO lv_dmbt9.
  ENDIF.
  IF p_date6 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT6' lv_dmbt6 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'账龄大于等于' p_dy6 INTO lv_dmbt9.
  ENDIF.
  IF p_date7 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT7' lv_dmbt7 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'账龄大于等于' p_dy7 INTO lv_dmbt9.
  ENDIF.
  IF p_date8 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT8' lv_dmbt8 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'账龄大于等于' p_dy8 INTO lv_dmbt9.
  ENDIF.

  PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT9' lv_dmbt9 ''  '' 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_show_alv .
  DATA: lwa_layout    TYPE slis_layout_alv, "ALV输出的控制
        l_variant     TYPE disvariant,     "ALV格式
        l_repid       TYPE sy-repid,       "程序名称
        lwa_excluding TYPE slis_t_extab.   "排除的菜单按钮

  lwa_layout-colwidth_optimize = c_x.
  lwa_layout-zebra             = c_x.
  lwa_layout-box_fieldname     = c_box.
  "  lwa_layout-f2code            = c_item.
  l_repid                      = sy-repid.

**交易货币 增加本币
*if r_4 = 'X'.
*  IF it_head[] IS NOT INITIAL.
*
*  ENDIF.
*
*
*ENDIF.



*明细模式显示
  IF r_1 EQ 'X'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = l_repid
        i_callback_user_command = 'F_USER_COMMAND'
        it_fieldcat             = it_fieldcat[]
        i_save                  = c_a
"       it_sort                 = it_sort[]
  "     is_variant              = l_variant
        is_layout               = lwa_layout
  "     it_excluding            = lwa_excluding
      TABLES
        t_outtab                = it_head
      EXCEPTIONS
        program_error           = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE s110(f0) DISPLAY LIKE c_msg_e.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
*汇总模式显示
  IF r_2 EQ 'X'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = l_repid
        i_callback_user_command = 'F1_USER_COMMAND'
        it_fieldcat             = it_fieldcat[]
        i_save                  = c_a
        is_layout               = lwa_layout
      TABLES
        t_outtab                = it_huizong_out
      EXCEPTIONS
        program_error           = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE s110(f0) DISPLAY LIKE c_msg_e.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_4441   text
*      -->P_TEXT_003  text
*      -->P_C_X  text
*      -->P_4444   text
*----------------------------------------------------------------------*
FORM f_fieldcat_set  TABLES pt_fieldcat TYPE slis_t_fieldcat_alv
                      USING p_field     TYPE char30
                            p_text      TYPE char40
                            p_fixed     TYPE c
                            p_hotspot   TYPE c
                            p_dosum     TYPE c.


  DATA:lwa_fieldcat  TYPE slis_fieldcat_alv."字段集

  lwa_fieldcat-fieldname  = p_field.
  lwa_fieldcat-seltext_l  = p_text.
  lwa_fieldcat-fix_column = p_fixed.
  lwa_fieldcat-hotspot    = p_hotspot.
  lwa_fieldcat-do_sum     = p_dosum.
  IF lwa_fieldcat-fieldname EQ 'KSBM' AND lwa_fieldcat-fix_column EQ 'X'.
    lwa_fieldcat-key = 'X'.
  ENDIF.
  IF lwa_fieldcat-fieldname EQ 'KSMS' AND lwa_fieldcat-fix_column EQ 'X'.
    lwa_fieldcat-key = 'X'.
  ENDIF.
  IF lwa_fieldcat-fieldname EQ 'BUKRS' AND lwa_fieldcat-fix_column EQ 'X'.
    lwa_fieldcat-key = 'X'.
  ENDIF.
  IF lwa_fieldcat-fieldname EQ 'BUTXT' AND lwa_fieldcat-fix_column EQ 'X'.
    lwa_fieldcat-key = 'X'.
  ENDIF.
  IF lwa_fieldcat-fieldname EQ 'GJAHR' AND lwa_fieldcat-fix_column EQ 'X'.
    lwa_fieldcat-key = 'X'.
  ENDIF.
  IF lwa_fieldcat-fieldname EQ 'BELNR' AND lwa_fieldcat-fix_column EQ 'X'.
    lwa_fieldcat-key = 'X'.
  ENDIF.
  IF lwa_fieldcat-fieldname EQ 'BUZEI' AND lwa_fieldcat-fix_column EQ 'X'.
    lwa_fieldcat-key = 'X'.
  ENDIF.
  IF lwa_fieldcat-fieldname EQ 'KLEV1' AND lwa_fieldcat-fix_column EQ 'X'.
    lwa_fieldcat-key = 'X'.
  ENDIF.
  IF lwa_fieldcat-fieldname EQ 'YJKMT' AND lwa_fieldcat-fix_column EQ 'X'.
    lwa_fieldcat-key = 'X'.
  ENDIF.





  "  lwa_fieldcat-edit       = 'X'.

  "*  只选择供应商时或者双击明细记录是供应商记录时，不显示客户相关的信息
  "  IF P_LIFNR = C_X
  "  AND P_KUNNR <> C_X
  "  AND P_HKONT <> C_X
  "  OR ( WA_HEAD-XTYPE = C_VEND ).
  "    IF P_FIELD = C_KUNNR
  "    OR P_FIELD = C_NAMEC
  "    OR P_FIELD = C_KTOKD.
  "      LWA_FIELDCAT-NO_OUT  = C_X.
  "    ENDIF.
  "  ENDIF.
  "*  只选择客户时或者双击明细记录是客户记录时，不显示供应商相关的信息
  "  IF P_KUNNR = C_X
  "  AND P_LIFNR <> C_X
  "  AND P_HKONT <> C_X
  "  OR ( WA_HEAD-XTYPE = C_CUST ).
  "    IF P_FIELD = C_LIFNR
  "    OR P_FIELD = C_NAMEV
  "    OR P_FIELD = C_KTOKK.
  "      LWA_FIELDCAT-NO_OUT  = C_X.
  "    ENDIF.
  "  ENDIF.

* ADD BY ZXK 20190128 BEGIN
  IF r_4 = 'X'.
    IF p_field = 'ZLJE'
    OR p_field = 'TOTAL'
    OR p_field = 'DMBT0'
    OR p_field = 'DMBT1'
    OR p_field = 'DMBT2'
    OR p_field = 'DMBT3'
    OR p_field = 'DMBT4'
    OR p_field = 'DMBT5'
    OR p_field = 'DMBT6'
    OR p_field = 'DMBT7'
    OR p_field = 'DMBT8'
    OR p_field = 'DMBT9'
    OR p_field = 'DMBT10'.
      lwa_fieldcat-cfieldname = 'WAERS'.
    ENDIF.
  ENDIF.
* ADD BY ZXK 20190128 END


  IF p_field = 'ZLJE_BB' OR p_field = 'WAERS_BB'.
    IF r_4 = 'X'.
      APPEND lwa_fieldcat TO pt_fieldcat.
    ENDIF.
  ELSE.
    APPEND lwa_fieldcat TO pt_fieldcat.
  ENDIF.



  CLEAR lwa_fieldcat.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_USER_COMMAND
*&---------------------------------------------------------------------*
*    设置汇总数据的用户命令
*----------------------------------------------------------------------*
*      -->  r_ucomm       用户双击汇总数据的功能码
*      <--  rs_selfield   被双击行的信息
*----------------------------------------------------------------------*
FORM f_user_command USING r_ucomm     TYPE sy-ucomm
      rs_selfield TYPE slis_selfield.

  IF r_ucomm = '&IC1'.
    IF rs_selfield-tabindex > 0.
      READ TABLE it_head INTO wa_head INDEX rs_selfield-tabindex.
      CASE rs_selfield-fieldname.
        WHEN 'BELNR'.
          SET PARAMETER ID 'BLN' FIELD wa_head-belnr.
          SET PARAMETER ID 'GJR' FIELD wa_head-gjahr.
          SET PARAMETER ID 'BUK' FIELD wa_head-bukrs.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        WHEN 'AUGBL'.
          SET PARAMETER ID 'BLN' FIELD wa_head-augbl.
          SET PARAMETER ID 'GJR' FIELD wa_head-gjahr.
          SET PARAMETER ID 'BUK' FIELD wa_head-bukrs.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDCASE.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_USER_COMMAND

FORM f1_user_command USING r_ucomm     TYPE sy-ucomm
      rs_selfield TYPE slis_selfield.

  IF r_ucomm = '&IC1'.
    IF rs_selfield-tabindex > 0.
      CLEAR wa_huizong_out.
      READ TABLE it_huizong_out INTO wa_huizong_out INDEX rs_selfield-tabindex.
      CASE rs_selfield-fieldname.
        WHEN 'KSBM'.
          PERFORM frm_display_item USING wa_huizong_out rs_selfield-fieldname.
      ENDCASE.
    ENDIF.
  ENDIF.



ENDFORM.                    " F_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  F_MERGE_CUS_VEN_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_merge_cus_ven_1 .
  DATA: lit_skat TYPE STANDARD TABLE OF x_skat,
        lwa_skat TYPE x_skat.
  DATA: lw_bseg TYPE x_bseg.
  DATA: lt_bseg TYPE TABLE OF x_bseg.
  DATA: l_num TYPE i.
  DATA: lw_bukrs TYPE x_bukrs.
  DATA: lt_bukrs TYPE TABLE OF x_bukrs.
  DATA: lt_prctr TYPE TABLE OF x_prctr.
  DATA: lw_prctr TYPE x_prctr.
  DATA: lt_tgsbt TYPE TABLE OF tgsbt.
  DATA: lw_tgsbt TYPE tgsbt.
  DATA: lw_guojia TYPE x_guojia.
  DATA: lt_guojia TYPE TABLE OF x_guojia.
  DATA: lt_bkpf TYPE TABLE OF x_bkpf.
  DATA: lw_bkpf TYPE x_bkpf.
  DATA: lw_t001 TYPE x_t001.

  LOOP AT it_acc_ven INTO wa_acc_ven.
    MOVE-CORRESPONDING wa_acc_ven TO wa_account.
    APPEND wa_account TO it_account.
    CLEAR wa_account.
  ENDLOOP.

  IF it_account[] IS INITIAL.
    MESSAGE s110(f0) DISPLAY LIKE c_msg_e.
    LEAVE LIST-PROCESSING.
  ENDIF.

*取会计科目描述
  SELECT spras
         ktopl
         saknr
         txt50
    INTO TABLE lit_skat
    FROM skat
     FOR ALL ENTRIES IN it_account
   WHERE spras EQ sy-langu
     AND ktopl EQ it_account-ktopl
     AND saknr EQ it_account-hkont.
  SORT lit_skat BY ktopl saknr.



  IF it_acc_ven IS NOT INITIAL.
*公司代码描述
    SELECT bukrs
           butxt
      INTO TABLE lt_bukrs
      FROM t001
      FOR ALL ENTRIES IN it_acc_ven
     WHERE bukrs EQ it_acc_ven-bukrs.
    SORT lt_bukrs.
    DELETE ADJACENT DUPLICATES FROM lt_bukrs.
*取科目类型和利润中心
    SELECT bukrs
           belnr
           gjahr
           buzei
           koart
           prctr
      INTO TABLE lt_bseg
      FROM bseg
      FOR ALL ENTRIES IN it_acc_ven
     WHERE bukrs EQ it_acc_ven-bukrs
       AND belnr EQ it_acc_ven-belnr
       AND gjahr EQ it_acc_ven-gjahr
       AND buzei EQ it_acc_ven-buzei.
    SORT lt_bseg.
    DELETE ADJACENT DUPLICATES FROM lt_bseg.
*取利润中心描述
    IF lt_bseg IS NOT INITIAL.
      SELECT prctr
             ltext
        INTO TABLE lt_prctr
        FROM cepct
         FOR ALL ENTRIES IN lt_bseg
       WHERE prctr EQ lt_bseg-prctr
         AND spras EQ 1.
      SORT lt_prctr.
      DELETE ADJACENT DUPLICATES FROM lt_prctr.
    ENDIF.
*取业务范围描述
    SELECT *
      INTO TABLE lt_tgsbt
      FROM tgsbt
       FOR ALL ENTRIES IN it_acc_ven
     WHERE gsber EQ it_acc_ven-gsber
       AND spras = 1.
*取国家描述
    IF it_t001 IS NOT INITIAL.
      SELECT land1
             landx
        INTO TABLE lt_guojia
        FROM t005t
        FOR ALL ENTRIES IN it_acc_ven
       WHERE land1 EQ it_acc_ven-land1
         AND spras EQ 1.
      SORT lt_guojia.
      DELETE ADJACENT DUPLICATES FROM lt_guojia.
    ENDIF.
*取事务代码
    SELECT bukrs
           belnr
           gjahr
           tcode
           blart
           xblnr_alt
      INTO CORRESPONDING FIELDS OF TABLE lt_bkpf
      FROM bkpf
       FOR ALL ENTRIES IN it_acc_ven
     WHERE bukrs EQ it_acc_ven-bukrs
       AND belnr EQ it_acc_ven-belnr
       AND gjahr EQ it_acc_ven-gjahr.
    SORT lt_bkpf.
    DELETE ADJACENT DUPLICATES FROM lt_bkpf.


    LOOP AT it_acc_ven INTO wa_acc_ven.
      CLEAR wa_head.
      wa_head-gzhms = wa_acc_ven-gzhms.   "供应商账户组
      wa_head-ktokk = wa_acc_ven-ktokk.   "供应商账户组描述
      wa_head-lifnr = wa_acc_ven-lifnr.   "供应商编码
      wa_head-namev = wa_acc_ven-namev.   "供应商描述
      wa_head-ksbm  = wa_acc_ven-lifnr.   "客商编码
      wa_head-ksms  = wa_acc_ven-namev.   "客商编码描述
*国家描述
      "      clear wa_t001.
      "      read table it_t001 into wa_t001 with key bukrs = wa_acc_ven-bukrs.
      "      if sy-subrc eq 0.
      "        read table lt_guojia into lw_guojia with key land1 = wa_t001-land1.
      "        wa_head-guoj = lw_guojia-landx.
      "      endif.
      READ TABLE lt_guojia INTO lw_guojia WITH KEY land1 = wa_acc_ven-land1.
      IF sy-subrc EQ 0.
        wa_head-guoj = lw_guojia-landx.
      ENDIF.

*科目类型
      READ TABLE lt_bseg INTO lw_bseg WITH KEY bukrs = wa_acc_ven-bukrs belnr = wa_acc_ven-belnr
                                                gjahr = wa_acc_ven-gjahr buzei = wa_acc_ven-buzei.

      IF sy-subrc EQ 0.
        wa_head-koart = lw_bseg-koart.
*利润中心
        wa_head-prctr = lw_bseg-prctr.
      ENDIF.
*利润中心描述
      IF wa_head-prctr NE ''.
        READ TABLE lt_prctr INTO lw_prctr WITH KEY prctr = wa_head-prctr.
        IF sy-subrc EQ 0.
          wa_head-prctt = lw_prctr-ltext.
        ENDIF.
      ENDIF.
*总账科目
      wa_head-hkont = wa_acc_ven-hkont.
*读取会计科目描述
      READ TABLE lit_skat INTO lwa_skat WITH KEY ktopl = wa_acc_ven-ktopl saknr = wa_acc_ven-hkont BINARY SEARCH.
      IF sy-subrc = 0.
        wa_head-txt50 = lwa_skat-txt50.
      ENDIF.

*一级科目
      wa_head-klev1 = wa_acc_ven-hkont+0(4).
*一级科目描述
      SEARCH wa_head-txt50 FOR '-'.
      IF sy-fdpos NE 0.
        l_num         = sy-fdpos.
        wa_head-yjkmt = wa_head-txt50+0(l_num).
      ELSE.
        wa_head-yjkmt = wa_head-txt50.
      ENDIF.
      CLEAR l_num.
*SGL
      wa_head-sgl = wa_acc_ven-umskz.
*公司代码
      wa_head-bukrs = wa_acc_ven-bukrs.
*公司描述
      READ TABLE lt_bukrs INTO lw_bukrs WITH KEY bukrs = wa_acc_ven-bukrs.
      IF sy-subrc EQ 0.
        wa_head-butxt = lw_bukrs-butxt.
      ENDIF.
*业务范围
      wa_head-gsber = wa_acc_ven-gsber.
*业务范围描述
      READ TABLE lt_tgsbt INTO lw_tgsbt WITH KEY gsber = wa_acc_ven-gsber.
      IF sy-subrc EQ 0.
        wa_head-gtext = lw_tgsbt-gtext.
      ENDIF.
*会计凭证
      wa_head-belnr = wa_acc_ven-belnr.
*会计凭证行
      wa_head-buzei = wa_acc_ven-buzei.
*会计年度
      wa_head-gjahr = wa_acc_ven-gjahr.
*事务代码
      READ TABLE lt_bkpf INTO lw_bkpf WITH KEY bukrs = wa_acc_ven-bukrs belnr = wa_acc_ven-belnr
                                                                          gjahr = wa_acc_ven-gjahr.

      IF sy-subrc EQ 0.
        wa_head-tcode = lw_bkpf-tcode.
*凭证类型
        wa_head-blart = lw_bkpf-blart.
*按月凭证编号
        wa_head-xblnr_alt = lw_bkpf-xblnr_alt.
      ENDIF.
*分配编号
      wa_head-zuonr = wa_acc_ven-zuonr.
*项目文本
      wa_head-sgtxt = wa_acc_ven-sgtxt.
*凭证日期
      wa_head-bldat = wa_acc_ven-bldat.
*过账日期
      wa_head-budat = wa_acc_ven-budat.
*到期日
      wa_head-dudat = wa_acc_ven-dudat.
*清账凭证号
      wa_head-augbl = wa_acc_ven-augbl.
*清账日期
      wa_head-augdt = wa_acc_ven-augdt.
*账龄类型
      wa_head-xtype = wa_acc_ven-xtype.
*客商余额
      IF r_3 EQ 'X'.
        wa_head-ksye = wa_acc_ven-dmbtr.
      ENDIF.
      IF r_4 EQ 'X'.
        wa_head-ksye = wa_acc_ven-wrbtr.
      ENDIF.
      CASE wa_acc_ven-perid.
*未到期余额
        WHEN c_d000.
          IF r_3 EQ 'X'.
            wa_head-wdqje = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-wdqje = wa_acc_ven-wrbtr.
          ENDIF.
*30天以内
        WHEN c_d001.
          IF r_3 EQ 'X'.
            wa_head-dmbt1 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt1 = wa_acc_ven-wrbtr.
          ENDIF.
*31到60
        WHEN c_d002.
          IF r_3 EQ 'X'.
            wa_head-dmbt2 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt2 = wa_acc_ven-wrbtr.
          ENDIF.
*61到90
        WHEN c_d003.
          IF r_3 EQ 'X'.
            wa_head-dmbt3 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt3 = wa_acc_ven-wrbtr.
          ENDIF.
*91到120
        WHEN c_d004.
          IF r_3 EQ 'X'.
            wa_head-dmbt4 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt4 = wa_acc_ven-wrbtr.
          ENDIF.
*120到180
        WHEN c_d005.
          IF r_3 EQ 'X'.
            wa_head-dmbt5 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt5 = wa_acc_ven-wrbtr.
          ENDIF.
*180到360
        WHEN c_d006.
          IF r_3 EQ 'X'.
            wa_head-dmbt6 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt6 = wa_acc_ven-wrbtr.
          ENDIF.
*360到720
        WHEN c_d007.
          IF r_3 EQ 'X'.
            wa_head-dmbt7 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt7 = wa_acc_ven-wrbtr.
          ENDIF.
*720到1440
        WHEN c_d008.
          IF r_3 EQ 'X'.
            wa_head-dmbt8 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt8 = wa_acc_ven-wrbtr.
          ENDIF.
*大于1440
        WHEN c_d009.
          IF r_3 EQ 'X'.
            wa_head-dmbt9 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt9 = wa_acc_ven-wrbtr.
          ENDIF.

      ENDCASE.
*账龄金额
      wa_head-zlje = wa_head-ksye - wa_head-dmbt0.
*币种
      IF r_3 EQ 'X'.
        READ TABLE it_t001 INTO lw_t001 WITH KEY bukrs = wa_acc_ven-bukrs.
        IF sy-subrc EQ 0.
          wa_head-waers = lw_t001-waers.
        ENDIF.
        CLEAR lw_t001.
      ENDIF.
      IF r_4 EQ 'X'.
        wa_head-waers = wa_acc_ven-waers.
      ENDIF.


      APPEND wa_head TO it_head.
      CLEAR wa_head.
    ENDLOOP.
  ENDIF.





ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MERGE_SOR_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_merge_sor_head .
  IF it_head IS NOT INITIAL.
    SORT it_head.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_HUIZONG_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_huizong_head .
  DATA: lt_huizong_lrzx TYPE TABLE OF x_huizong.
  DATA: lw_huizong_lrzx TYPE x_huizong.
  DATA: lt_huizong_ywfw TYPE TABLE OF x_huizong.
  DATA: lw_huizong_ywfw TYPE x_huizong.
  DATA: lt_huizong_hebing TYPE TABLE OF x_huizong.
  DATA: lw_huizong_hebing TYPE x_huizong.
  DATA: lt_huizong_kesh TYPE TABLE OF x_huizong.
  DATA: lw_huizong_kesh TYPE x_huizong.
  DATA: lt_huizong_out TYPE TABLE OF x_huizong.
  DATA: lw_huizong_out TYPE x_huizong.

  IF r_4 EQ 'X'.
    IF it_head[] IS NOT INITIAL.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE gt_bseg
        FROM bseg
        FOR ALL ENTRIES IN it_head
        WHERE  bukrs = it_head-bukrs AND belnr = it_head-belnr AND
                buzei = it_head-buzei AND
                gjahr = it_head-gjahr.
      LOOP AT it_head INTO wa_head.
        READ TABLE gt_bseg WITH KEY bukrs = wa_head-bukrs belnr = wa_head-belnr buzei = wa_head-buzei gjahr = wa_head-gjahr.
        IF sy-subrc = 0.
          IF wa_head-zlje >= 0.
            wa_head-zlje_bb  = gt_bseg-dmbtr.
          ELSE.
            wa_head-zlje_bb  = - gt_bseg-dmbtr.
          ENDIF.
*          wa_head-waers_bb = gt_bseg-h_hwaer.  "本币写死 cny  BY HAND 20191114
          wa_head-waers_bb = 'CNY'.
        ENDIF.
        MODIFY it_head FROM wa_head.
      ENDLOOP.
    ENDIF.

  ENDIF.

*汇总模式显示
  IF r_2 EQ 'X'.
    CLEAR wa_huizong.
    CLEAR it_huizong.
*对汇总内表进行综合数据赋值
    LOOP AT it_head INTO wa_head.
      MOVE-CORRESPONDING wa_head TO wa_huizong.
      APPEND wa_huizong TO it_huizong.
      CLEAR wa_huizong.
      CLEAR wa_head.
    ENDLOOP.
*-----------------------------------------------------------------------
*只勾选利润中心
    IF c_1 EQ 'X' AND c_2 NE 'X'.
      LOOP AT it_huizong INTO wa_huizong.
        lw_huizong_lrzx-bukrs  = wa_huizong-bukrs.  "公司代码
        lw_huizong_lrzx-butxt  = wa_huizong-butxt.  "公司代码描述
        lw_huizong_lrzx-lifnr  = wa_huizong-lifnr.  "供应商编码
        lw_huizong_lrzx-namev  = wa_huizong-namev.  "供应商描述
        lw_huizong_lrzx-kunnr  = wa_huizong-kunnr.  "客户编码
        lw_huizong_lrzx-namec  = wa_huizong-namec.  "客户描述
        lw_huizong_lrzx-ksbm   = wa_huizong-ksbm.   "客商编码
        lw_huizong_lrzx-ksms   = wa_huizong-ksms.   "客商描述
        lw_huizong_lrzx-guoj   = wa_huizong-guoj.   "国家
        lw_huizong_lrzx-ktokk  = wa_huizong-ktokk.  "供应商账户组
        lw_huizong_lrzx-gzhms  = wa_huizong-gzhms.  "供应商账户组描述
        lw_huizong_lrzx-ktokd  = wa_huizong-ktokd.  "客户账户组
        lw_huizong_lrzx-kzhms  = wa_huizong-kzhms.  "客户账户组描述
        lw_huizong_lrzx-koart  = wa_huizong-koart.  "科目类型
        lw_huizong_lrzx-klev1  = wa_huizong-klev1.  "一级科目
        lw_huizong_lrzx-yjkmt  = wa_huizong-yjkmt.  "一级科目描述
        lw_huizong_lrzx-hkont  = wa_huizong-hkont.  "会计帐目
        lw_huizong_lrzx-txt50  = wa_huizong-txt50.  "科目描述
        lw_huizong_lrzx-prctr  = wa_huizong-prctr.  "利润中心
        lw_huizong_lrzx-prctt  = wa_huizong-prctt.  "利润中心描述
        lw_huizong_lrzx-waers  = wa_huizong-waers.  "币种
        lw_huizong_lrzx-waers_bb  = wa_huizong-waers_bb.  "币种

        APPEND lw_huizong_lrzx TO lt_huizong_lrzx.
        CLEAR lw_huizong_lrzx.
        CLEAR wa_huizong.
      ENDLOOP.
*排序，去重
      SORT lt_huizong_lrzx.
      DELETE ADJACENT DUPLICATES FROM lt_huizong_lrzx.

*按照利润中心进行汇总
      LOOP AT lt_huizong_lrzx INTO lw_huizong_lrzx.
        MOVE-CORRESPONDING lw_huizong_lrzx TO lw_huizong_out.

        LOOP AT it_head INTO wa_head WHERE bukrs EQ lw_huizong_lrzx-bukrs AND lifnr EQ lw_huizong_lrzx-lifnr
                                        AND kunnr EQ lw_huizong_lrzx-kunnr AND ksbm  EQ lw_huizong_lrzx-ksbm
                                        AND guoj  EQ lw_huizong_lrzx-guoj  AND ktokk EQ lw_huizong_lrzx-ktokk
                                        AND ktokd EQ lw_huizong_lrzx-ktokd AND koart EQ lw_huizong_lrzx-koart
                                        AND klev1 EQ lw_huizong_lrzx-klev1 AND hkont EQ lw_huizong_lrzx-hkont
                                        AND prctr EQ lw_huizong_lrzx-prctr AND waers EQ lw_huizong_lrzx-waers.


          lw_huizong_out-zlje  = lw_huizong_out-zlje  + wa_head-zlje.
          lw_huizong_out-zlje_bb  = lw_huizong_out-zlje_bb  + wa_head-zlje_bb.
          lw_huizong_out-wdqje = lw_huizong_out-wdqje + wa_head-wdqje.
          lw_huizong_out-dmbt1 = lw_huizong_out-dmbt1 + wa_head-dmbt1.
          lw_huizong_out-dmbt2 = lw_huizong_out-dmbt2 + wa_head-dmbt2.
          lw_huizong_out-dmbt3 = lw_huizong_out-dmbt3 + wa_head-dmbt3.
          lw_huizong_out-dmbt4 = lw_huizong_out-dmbt4 + wa_head-dmbt4.
          lw_huizong_out-dmbt5 = lw_huizong_out-dmbt5 + wa_head-dmbt5.
          lw_huizong_out-dmbt6 = lw_huizong_out-dmbt6 + wa_head-dmbt6.
          lw_huizong_out-dmbt7 = lw_huizong_out-dmbt7 + wa_head-dmbt7.
          lw_huizong_out-dmbt8 = lw_huizong_out-dmbt8 + wa_head-dmbt8.
          lw_huizong_out-dmbt9 = lw_huizong_out-dmbt9 + wa_head-dmbt9.
        ENDLOOP.

        APPEND lw_huizong_out TO lt_huizong_out.
        CLEAR lw_huizong_out.
        CLEAR wa_head.
        CLEAR lw_huizong_lrzx.
      ENDLOOP.
*为输出内表填充数据
      IF lt_huizong_out IS NOT INITIAL.
        CLEAR it_huizong_out.
        APPEND LINES OF lt_huizong_out TO it_huizong_out.
      ENDIF.
*---------------------------------------------------------------------------

*只勾选业务范围
    ELSEIF c_1 NE 'X' AND c_2 EQ 'X'.
      LOOP AT it_huizong INTO wa_huizong.
        lw_huizong_ywfw-bukrs  = wa_huizong-bukrs.  "公司代码
        lw_huizong_ywfw-butxt  = wa_huizong-butxt.  "公司代码描述
        lw_huizong_ywfw-lifnr  = wa_huizong-lifnr.  "供应商编码
        lw_huizong_ywfw-namev  = wa_huizong-namev.  "供应商描述
        lw_huizong_ywfw-kunnr  = wa_huizong-kunnr.  "客户编码
        lw_huizong_ywfw-namec  = wa_huizong-namec.  "客户描述
        lw_huizong_ywfw-ksbm   = wa_huizong-ksbm.   "客商编码
        lw_huizong_ywfw-ksms   = wa_huizong-ksms.   "客商描述
        lw_huizong_ywfw-guoj   = wa_huizong-guoj.   "国家
        lw_huizong_ywfw-ktokk  = wa_huizong-ktokk.  "供应商账户组
        lw_huizong_ywfw-gzhms  = wa_huizong-gzhms.  "供应商账户组描述
        lw_huizong_ywfw-ktokd  = wa_huizong-ktokd.  "客户账户组
        lw_huizong_ywfw-kzhms  = wa_huizong-kzhms.  "客户账户组描述
        lw_huizong_ywfw-koart  = wa_huizong-koart.  "科目类型
        lw_huizong_ywfw-klev1  = wa_huizong-klev1.  "一级科目
        lw_huizong_ywfw-yjkmt  = wa_huizong-yjkmt.  "一级科目描述
        lw_huizong_ywfw-hkont  = wa_huizong-hkont.  "会计帐目
        lw_huizong_ywfw-txt50  = wa_huizong-txt50.  "科目描述
        lw_huizong_ywfw-gsber  = wa_huizong-gsber.  "业务范围
        lw_huizong_ywfw-gtext  = wa_huizong-gtext.  "业务范围描述
        lw_huizong_ywfw-waers  = wa_huizong-waers.  "币种
        lw_huizong_ywfw-waers_bb  = wa_huizong-waers_bb.  "币种

        APPEND lw_huizong_ywfw TO lt_huizong_ywfw.
        CLEAR lw_huizong_ywfw.
        CLEAR wa_huizong.
      ENDLOOP.
*排序，去重
      SORT lt_huizong_ywfw.
      DELETE ADJACENT DUPLICATES FROM lt_huizong_ywfw.

*按照业务范围进行汇总
      LOOP AT lt_huizong_ywfw INTO lw_huizong_ywfw.
        MOVE-CORRESPONDING lw_huizong_ywfw TO lw_huizong_out.

        LOOP AT it_head INTO wa_head WHERE bukrs EQ lw_huizong_ywfw-bukrs AND lifnr EQ lw_huizong_ywfw-lifnr
                                        AND kunnr EQ lw_huizong_ywfw-kunnr AND ksbm  EQ lw_huizong_ywfw-ksbm
                                        AND guoj  EQ lw_huizong_ywfw-guoj  AND ktokk EQ lw_huizong_ywfw-ktokk
                                        AND ktokd EQ lw_huizong_ywfw-ktokd AND koart EQ lw_huizong_ywfw-koart
                                        AND klev1 EQ lw_huizong_ywfw-klev1 AND hkont EQ lw_huizong_ywfw-hkont
                                        AND gsber EQ lw_huizong_ywfw-gsber AND waers EQ lw_huizong_ywfw-waers.


          lw_huizong_out-zlje  = lw_huizong_out-zlje  + wa_head-zlje.
          lw_huizong_out-zlje_bb  = lw_huizong_out-zlje_bb  + wa_head-zlje_bb.
          lw_huizong_out-wdqje = lw_huizong_out-wdqje + wa_head-wdqje.
          lw_huizong_out-dmbt1 = lw_huizong_out-dmbt1 + wa_head-dmbt1.
          lw_huizong_out-dmbt2 = lw_huizong_out-dmbt2 + wa_head-dmbt2.
          lw_huizong_out-dmbt3 = lw_huizong_out-dmbt3 + wa_head-dmbt3.
          lw_huizong_out-dmbt4 = lw_huizong_out-dmbt4 + wa_head-dmbt4.
          lw_huizong_out-dmbt5 = lw_huizong_out-dmbt5 + wa_head-dmbt5.
          lw_huizong_out-dmbt6 = lw_huizong_out-dmbt6 + wa_head-dmbt6.
          lw_huizong_out-dmbt7 = lw_huizong_out-dmbt7 + wa_head-dmbt7.
          lw_huizong_out-dmbt8 = lw_huizong_out-dmbt8 + wa_head-dmbt8.
          lw_huizong_out-dmbt9 = lw_huizong_out-dmbt9 + wa_head-dmbt9.
        ENDLOOP.

        APPEND lw_huizong_out TO lt_huizong_out.
        CLEAR lw_huizong_out.
        CLEAR wa_head.
        CLEAR lw_huizong_ywfw.
      ENDLOOP.
*为输出内表填充数据
      IF lt_huizong_out IS NOT INITIAL.
        CLEAR it_huizong_out.
        APPEND LINES OF lt_huizong_out TO it_huizong_out.
      ENDIF.
*---------------------------------------------------------------------------


*勾选利润中心和业务范围
    ELSEIF c_1 EQ 'X' AND c_2 EQ 'X'.
      LOOP AT it_huizong INTO wa_huizong.
        lw_huizong_hebing-bukrs  = wa_huizong-bukrs.  "公司代码
        lw_huizong_hebing-butxt  = wa_huizong-butxt.  "公司代码描述
        lw_huizong_hebing-lifnr  = wa_huizong-lifnr.  "供应商编码
        lw_huizong_hebing-namev  = wa_huizong-namev.  "供应商描述
        lw_huizong_hebing-kunnr  = wa_huizong-kunnr.  "客户编码
        lw_huizong_hebing-namec  = wa_huizong-namec.  "客户描述
        lw_huizong_hebing-ksbm   = wa_huizong-ksbm.   "客商编码
        lw_huizong_hebing-ksms   = wa_huizong-ksms.   "客商描述
        lw_huizong_hebing-guoj   = wa_huizong-guoj.   "国家
        lw_huizong_hebing-ktokk  = wa_huizong-ktokk.  "供应商账户组
        lw_huizong_hebing-gzhms  = wa_huizong-gzhms.  "供应商账户组描述
        lw_huizong_hebing-ktokd  = wa_huizong-ktokd.  "客户账户组
        lw_huizong_hebing-kzhms  = wa_huizong-kzhms.  "客户账户组描述
        lw_huizong_hebing-koart  = wa_huizong-koart.  "科目类型
        lw_huizong_hebing-klev1  = wa_huizong-klev1.  "一级科目
        lw_huizong_hebing-yjkmt  = wa_huizong-yjkmt.  "一级科目描述
        lw_huizong_hebing-hkont  = wa_huizong-hkont.  "会计帐目
        lw_huizong_hebing-txt50  = wa_huizong-txt50.  "科目描述
        lw_huizong_hebing-prctr  = wa_huizong-prctr.  "利润中心
        lw_huizong_hebing-prctt  = wa_huizong-prctt.  "利润中心
        lw_huizong_hebing-gsber  = wa_huizong-gsber.  "业务范围
        lw_huizong_hebing-gtext  = wa_huizong-gtext.  "业务范围
        lw_huizong_hebing-waers  = wa_huizong-waers.  "币种
        lw_huizong_hebing-waers_bb  = wa_huizong-waers_bb.  "币种
        APPEND lw_huizong_hebing TO lt_huizong_hebing.
        CLEAR lw_huizong_hebing.
        CLEAR wa_huizong.
      ENDLOOP.
*排序，去重
      SORT lt_huizong_hebing.
      DELETE ADJACENT DUPLICATES FROM lt_huizong_hebing.

*按照利润中心与业务范围合并进行汇总
      LOOP AT lt_huizong_hebing INTO lw_huizong_hebing.
        MOVE-CORRESPONDING lw_huizong_hebing TO lw_huizong_out.

        LOOP AT it_head INTO wa_head WHERE bukrs EQ lw_huizong_hebing-bukrs AND lifnr EQ lw_huizong_hebing-lifnr
                                        AND kunnr EQ lw_huizong_hebing-kunnr AND ksbm  EQ lw_huizong_hebing-ksbm
                                        AND guoj  EQ lw_huizong_hebing-guoj  AND ktokk EQ lw_huizong_hebing-ktokk
                                        AND ktokd EQ lw_huizong_hebing-ktokd AND koart EQ lw_huizong_hebing-koart
                                        AND klev1 EQ lw_huizong_hebing-klev1 AND hkont EQ lw_huizong_hebing-hkont
                                        AND prctr EQ lw_huizong_hebing-prctr AND gsber EQ lw_huizong_hebing-gsber
                                        AND waers EQ lw_huizong_hebing-waers.


          lw_huizong_out-zlje  = lw_huizong_out-zlje  + wa_head-zlje.
          lw_huizong_out-zlje_bb  = lw_huizong_out-zlje_bb  + wa_head-zlje_bb.
          lw_huizong_out-wdqje = lw_huizong_out-wdqje + wa_head-wdqje.
          lw_huizong_out-dmbt1 = lw_huizong_out-dmbt1 + wa_head-dmbt1.
          lw_huizong_out-dmbt2 = lw_huizong_out-dmbt2 + wa_head-dmbt2.
          lw_huizong_out-dmbt3 = lw_huizong_out-dmbt3 + wa_head-dmbt3.
          lw_huizong_out-dmbt4 = lw_huizong_out-dmbt4 + wa_head-dmbt4.
          lw_huizong_out-dmbt5 = lw_huizong_out-dmbt5 + wa_head-dmbt5.
          lw_huizong_out-dmbt6 = lw_huizong_out-dmbt6 + wa_head-dmbt6.
          lw_huizong_out-dmbt7 = lw_huizong_out-dmbt7 + wa_head-dmbt7.
          lw_huizong_out-dmbt8 = lw_huizong_out-dmbt8 + wa_head-dmbt8.
          lw_huizong_out-dmbt9 = lw_huizong_out-dmbt9 + wa_head-dmbt9.
        ENDLOOP.

        APPEND lw_huizong_out TO lt_huizong_out.
        CLEAR lw_huizong_out.
        CLEAR wa_head.
        CLEAR lw_huizong_hebing.
      ENDLOOP.
*为输出内表填充数据
      IF lt_huizong_out IS NOT INITIAL.
        CLEAR it_huizong_out.
        APPEND LINES OF lt_huizong_out TO it_huizong_out.
      ENDIF.
*---------------------------------------------------------------------------


*都不勾选利润中心和业务范围
    ELSEIF c_1 NE 'X' AND c_2 NE 'X'.
      LOOP AT it_huizong INTO wa_huizong.
        lw_huizong_kesh-bukrs  = wa_huizong-bukrs.  "公司代码
        lw_huizong_kesh-butxt  = wa_huizong-butxt.  "公司代码描述
        lw_huizong_kesh-lifnr  = wa_huizong-lifnr.  "供应商编码
        lw_huizong_kesh-namev  = wa_huizong-namev.  "供应商描述
        lw_huizong_kesh-kunnr  = wa_huizong-kunnr.  "客户编码
        lw_huizong_kesh-namec  = wa_huizong-namec.  "客户描述
        lw_huizong_kesh-ksbm   = wa_huizong-ksbm.   "客商编码
        lw_huizong_kesh-ksms   = wa_huizong-ksms.   "客商描述
        lw_huizong_kesh-guoj   = wa_huizong-guoj.   "国家
        lw_huizong_kesh-ktokk  = wa_huizong-ktokk.  "供应商账户组
        lw_huizong_kesh-gzhms  = wa_huizong-gzhms.  "供应商账户组描述
        lw_huizong_kesh-ktokd  = wa_huizong-ktokd.  "客户账户组
        lw_huizong_kesh-kzhms  = wa_huizong-kzhms.  "客户账户组描述
        lw_huizong_kesh-koart  = wa_huizong-koart.  "科目类型
        lw_huizong_kesh-klev1  = wa_huizong-klev1.  "一级科目
        lw_huizong_kesh-yjkmt  = wa_huizong-yjkmt.  "一级科目描述
        lw_huizong_kesh-hkont  = wa_huizong-hkont.  "会计帐目
        lw_huizong_kesh-txt50  = wa_huizong-txt50.  "科目描述
        lw_huizong_kesh-waers  = wa_huizong-waers.  "币种
        lw_huizong_kesh-waers_bb  = wa_huizong-waers_bb.  "币种
        APPEND lw_huizong_kesh TO lt_huizong_kesh.
        CLEAR lw_huizong_kesh.
        CLEAR wa_huizong.
      ENDLOOP.
*排序，去重
      SORT lt_huizong_kesh.
      DELETE ADJACENT DUPLICATES FROM lt_huizong_kesh.
*按照客商进行汇总
      LOOP AT lt_huizong_kesh INTO lw_huizong_kesh.
        MOVE-CORRESPONDING lw_huizong_kesh TO lw_huizong_out.

        LOOP AT it_head INTO wa_head WHERE bukrs EQ lw_huizong_kesh-bukrs AND lifnr EQ lw_huizong_kesh-lifnr
                                        AND kunnr EQ lw_huizong_kesh-kunnr AND ksbm  EQ lw_huizong_kesh-ksbm
                                        AND guoj  EQ lw_huizong_kesh-guoj  AND ktokk EQ lw_huizong_kesh-ktokk
                                        AND ktokd EQ lw_huizong_kesh-ktokd AND koart EQ lw_huizong_kesh-koart
                                        AND klev1 EQ lw_huizong_kesh-klev1 AND hkont EQ lw_huizong_kesh-hkont
                                        AND waers EQ lw_huizong_kesh-waers.


          lw_huizong_out-zlje  = lw_huizong_out-zlje  + wa_head-zlje.
          lw_huizong_out-zlje_bb  = lw_huizong_out-zlje_bb  + wa_head-zlje_bb.
          lw_huizong_out-wdqje = lw_huizong_out-wdqje + wa_head-wdqje.
          lw_huizong_out-dmbt1 = lw_huizong_out-dmbt1 + wa_head-dmbt1.
          lw_huizong_out-dmbt2 = lw_huizong_out-dmbt2 + wa_head-dmbt2.
          lw_huizong_out-dmbt3 = lw_huizong_out-dmbt3 + wa_head-dmbt3.
          lw_huizong_out-dmbt4 = lw_huizong_out-dmbt4 + wa_head-dmbt4.
          lw_huizong_out-dmbt5 = lw_huizong_out-dmbt5 + wa_head-dmbt5.
          lw_huizong_out-dmbt6 = lw_huizong_out-dmbt6 + wa_head-dmbt6.
          lw_huizong_out-dmbt7 = lw_huizong_out-dmbt7 + wa_head-dmbt7.
          lw_huizong_out-dmbt8 = lw_huizong_out-dmbt8 + wa_head-dmbt8.
          lw_huizong_out-dmbt9 = lw_huizong_out-dmbt9 + wa_head-dmbt9.
        ENDLOOP.

        APPEND lw_huizong_out TO lt_huizong_out.
        CLEAR lw_huizong_out.
        CLEAR wa_head.
        CLEAR lw_huizong_kesh.
      ENDLOOP.
*为输出内表填充数据
      IF lt_huizong_out IS NOT INITIAL.
        CLEAR it_huizong_out.
        APPEND LINES OF lt_huizong_out TO it_huizong_out.
      ENDIF.
*---------------------------------------------------------------------------
    ENDIF.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_HUIZONG_OUT  text
*      -->P_RS_SELFIELD_FIELDNAME  text
*----------------------------------------------------------------------*
FORM frm_display_item  USING pw_huizong_out TYPE x_huizong
      p_field  TYPE slis_fieldname.

*汇总模式显示
  IF r_2 EQ 'X'.
    IF c_1 EQ 'X' AND c_2 NE 'X'.
*获取明细数据
      PERFORM frm_get_item1 USING pw_huizong_out.
    ELSEIF c_1 NE 'X' AND c_2 EQ 'X'.
*获取明细数据
      PERFORM frm_get_item2 USING pw_huizong_out.
    ELSEIF c_1 EQ 'X' AND c_2 EQ 'X'.
*获取明细数据
      PERFORM frm_get_item3 USING pw_huizong_out.
    ELSEIF c_1 NE 'X' AND c_2 NE 'X'.
*获取明细数据
      PERFORM frm_get_item4 USING pw_huizong_out.
    ENDIF.
*做金额合计
    "    perform frm_sum_item.

*设置明细数据显示的字段
    PERFORM f_field_item.
*展示ALV
    PERFORM frm_display_alv.
  ENDIF.







ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_ITEM1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PW_HUIZONG_OUT  text
*----------------------------------------------------------------------*
FORM frm_get_item1  USING    p_pw_huizong_out TYPE x_huizong.
  DATA: lw_head TYPE x_head.
  DATA: lt_head TYPE TABLE OF x_head.
  DATA: lw_huizong TYPE x_huizong.

  MOVE-CORRESPONDING p_pw_huizong_out TO lw_huizong.
*按照只勾选利润中心的汇总模式把对应的明细数据选取出来放进内表
  LOOP AT it_head INTO wa_head WHERE bukrs EQ lw_huizong-bukrs AND butxt EQ lw_huizong-butxt
                                 AND lifnr EQ lw_huizong-lifnr AND namev EQ lw_huizong-namev
                                 AND kunnr EQ lw_huizong-kunnr AND namec EQ lw_huizong-namec
                                 AND ksbm  EQ lw_huizong-ksbm  AND ksms  EQ lw_huizong-ksms
                                 AND guoj  EQ lw_huizong-guoj  AND ktokk EQ lw_huizong-ktokk
                                 AND gzhms EQ lw_huizong-gzhms AND ktokd EQ lw_huizong-ktokd
                                 AND kzhms EQ lw_huizong-kzhms AND koart EQ lw_huizong-koart
                                 AND klev1 EQ lw_huizong-klev1 AND yjkmt EQ lw_huizong-yjkmt
                                 AND hkont EQ lw_huizong-hkont AND txt50 EQ lw_huizong-txt50
                                 AND prctr EQ lw_huizong-prctr AND prctt EQ lw_huizong-prctt
                                 AND waers EQ lw_huizong-waers.

    MOVE-CORRESPONDING wa_head TO lw_head.
    APPEND lw_head TO lt_head.
  ENDLOOP.
*明细数据装入显示行内表
  CLEAR it_item.
  CLEAR wa_item.
  APPEND LINES OF lt_head TO it_item.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FIELD_ITEM1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_field_item .
  DATA:lv_dmbt1 TYPE char40.
  DATA:lv_dmbt2 TYPE char40.
  DATA:lv_dmbt3 TYPE char40.
  DATA:lv_dmbt4 TYPE char40.
  DATA:lv_dmbt5 TYPE char40.
  DATA:lv_dmbt6 TYPE char40.
  DATA:lv_dmbt7 TYPE char40.
  DATA:lv_dmbt8 TYPE char40.
  DATA:lv_dmbt9 TYPE char40.

  DATA:p_dy1 TYPE idcn_segm.
  DATA:p_dy2 TYPE idcn_segm.
  DATA:p_dy3 TYPE idcn_segm.
  DATA:p_dy4 TYPE idcn_segm.
  DATA:p_dy5 TYPE idcn_segm.
  DATA:p_dy6 TYPE idcn_segm.
  DATA:p_dy7 TYPE idcn_segm.
  DATA:p_dy8 TYPE idcn_segm.



  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date1
    IMPORTING
      output = p_date1.

  p_dy1 = p_date1 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date2
    IMPORTING
      output = p_dy2.
  p_dy2 = p_date2 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date3
    IMPORTING
      output = p_dy3.

  p_dy3 = p_date3 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date4
    IMPORTING
      output = p_dy4.

  p_dy4 = p_date4 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date5
    IMPORTING
      output = p_dy5.

  p_dy5 = p_date5 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date6
    IMPORTING
      output = p_dy6.

  p_dy6 = p_date6 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date7
    IMPORTING
      output = p_dy7.

  p_dy7 = p_date7 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date8
    IMPORTING
      output = p_dy8.

  p_dy8 = p_date8 + 1.

  CONCATENATE:'账龄小于等于'p_date1 '天' INTO lv_dmbt1.
  CONCATENATE:'账龄'p_dy1 '天到' p_date2 '天' INTO lv_dmbt2.
  CONCATENATE:'账龄'p_dy2 '天到' p_date3 '天' INTO lv_dmbt3.
  CONCATENATE:'账龄'p_dy3 '天到' p_date4 '天' INTO lv_dmbt4.
  CONCATENATE:'账龄'p_dy4 '天到' p_date5 '天' INTO lv_dmbt5.
  CONCATENATE:'账龄'p_dy5 '天到' p_date6 '天' INTO lv_dmbt6.
  CONCATENATE:'账龄'p_dy6 '天到' p_date7 '天' INTO lv_dmbt7.
  CONCATENATE:'账龄'p_dy7 '天到' p_date8 '天' INTO lv_dmbt8.

  CLEAR it_fieldcat.
  PERFORM f_fieldcat_set TABLES it_fieldcat
  USING:  'KSBM'  TEXT-051 'X' '' '',  "客商编码
          'KSMS'  TEXT-052 'X' '' '',  "客商描述
          'BUKRS' TEXT-003 'X' '' '',  "公司代码
          'BUTXT' TEXT-057 'X' '' '',  "公司代码描述
          'GJAHR' TEXT-063 'X' '' '',  "会计年度
          'BELNR' TEXT-061 'X' 'X' '',  "会计凭证
          'BUZEI' TEXT-062 'X' 'X' '',  "会计凭证行
          'XBLNR_ALT' TEXT-082 '' '' '', "按月凭证编号
          'GUOJ'  TEXT-055 '' '' '',      "国家
          'KTOKD' TEXT-009 '' '' '',  "客户账户组
          'KZHMS' TEXT-049 '' '' '',  "客户账户组描述
          'KTOKK' TEXT-008 '' '' '',  "供应商账户组
          'GZHMS' TEXT-050 '' '' '',   "供应商账户组描述
          'KUNNR' TEXT-006 '' '' '',   "客户编码
          'NAMEC' TEXT-007 '' '' '',  "客户描述
          'LIFNR' TEXT-004 '' '' '',   "供应商编码
          'NAMEV' TEXT-005 '' '' '',   "供应商描述
          'KOART' TEXT-053 '' '' '',  "科目类型
          'KLEV1' TEXT-039 '' '' '',      "一级科目
          'YJKMT' TEXT-054 '' '' '',  "一级科目描述
          'HKONT' TEXT-010 '' '' '',  "会计帐目
          'TXT50' TEXT-011 '' '' '', "科目描述
          'SGL'   TEXT-056 '' '' '',      "SGL
          'PRCTR' TEXT-058 '' '' '', "利润中心
          'PRCTT' TEXT-059 '' '' '', "利润中心描述
          'GSBER' TEXT-060 '' '' '', "业务范围
          'GTEXT' TEXT-080 '' '' '', "业务范围描述
          'BLART' TEXT-064 '' '' '',  "凭证类型
          'TCODE' TEXT-065 '' '' '', "事务代码
          'ZUONR' TEXT-066 '' '' '',  "分配
          'SGTXT' TEXT-067 '' '' '',  "项目文本
          'BLDAT' TEXT-068 '' '' '', "凭证日期
          'BUDAT' TEXT-069 '' '' '',  "过账日期
          'DUDAT' TEXT-070 '' '' '',  "到期日
          'AUGBL' TEXT-071 '' '' '',  "清账凭证号
          'AUGDT' TEXT-072 '' '' '',  "清账日期
          'KSYE ' TEXT-073 '' '' 'X', "客商余额
          'WDQJE' TEXT-074 '' '' 'X',  "未到期余额
          'ZLJE'  TEXT-075 '' '' 'X',  "账龄金额
          'WAERS' TEXT-076 '' '' '',  "币种
          'ZLJE_BB'  TEXT-085 '' '' 'X',  "账龄金额
          'WAERS_BB' TEXT-086 '' '' '',  "币种
          'XTYPE' TEXT-077 '' '' ''.       "账龄类型

  IF p_date1 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT1' lv_dmbt1 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'账龄大于等于' p_dy1 INTO lv_dmbt9.
  ENDIF.
  IF p_date2 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT2' lv_dmbt2 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'账龄大于等于' p_dy2 INTO lv_dmbt9.
  ENDIF.
  IF p_date3 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT3' lv_dmbt3 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'账龄大于等于' p_dy3 INTO lv_dmbt9.
  ENDIF.
  IF p_date4 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT4' lv_dmbt4 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'账龄大于等于' p_dy4 INTO lv_dmbt9.
  ENDIF.
  IF p_date5 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT5' lv_dmbt5 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'账龄大于等于' p_dy5 INTO lv_dmbt9.
  ENDIF.
  IF p_date6 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT6' lv_dmbt6 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'账龄大于等于' p_dy6 INTO lv_dmbt9.
  ENDIF.
  IF p_date7 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT7' lv_dmbt7 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'账龄大于等于' p_dy7 INTO lv_dmbt9.
  ENDIF.
  IF p_date8 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT8' lv_dmbt8 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'账龄大于等于' p_dy8 INTO lv_dmbt9.
  ENDIF.

  PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT9' lv_dmbt9 ''  '' 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DISPLAY_ALV1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_display_alv .
  DATA: lwa_layout    TYPE slis_layout_alv, "ALV输出的控制
        l_variant     TYPE disvariant,     "ALV格式
        l_repid       TYPE sy-repid,       "程序名称
        lwa_excluding TYPE slis_t_extab.   "排除的菜单按钮



  lwa_layout-colwidth_optimize = c_x.
  lwa_layout-zebra             = c_x.
  lwa_layout-box_fieldname     = c_box.
  l_repid                      = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = l_repid
      i_grid_title            = TEXT-081
      i_callback_user_command = 'F2_USER_COMMAND'
      it_fieldcat             = it_fieldcat[]
      i_save                  = c_a
      is_layout               = lwa_layout
    TABLES
      t_outtab                = it_item
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE s110(f0) DISPLAY LIKE c_msg_e.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

FORM f2_user_command USING r_ucomm     TYPE sy-ucomm
      rs_selfield TYPE slis_selfield.

  IF r_ucomm = '&IC1'.
    IF rs_selfield-tabindex > 0.
      READ TABLE it_item INTO wa_item INDEX rs_selfield-tabindex.
      CASE rs_selfield-fieldname.
        WHEN 'BELNR'.
          SET PARAMETER ID 'BLN' FIELD wa_item-belnr.
          SET PARAMETER ID 'GJR' FIELD wa_item-gjahr.
          SET PARAMETER ID 'BUK' FIELD wa_item-bukrs.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        WHEN 'AUGBL'.
          SET PARAMETER ID 'BLN' FIELD wa_item-augbl.
          SET PARAMETER ID 'GJR' FIELD wa_item-gjahr.
          SET PARAMETER ID 'BUK' FIELD wa_item-bukrs.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDCASE.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_ITEM2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PW_HUIZONG_OUT  text
*----------------------------------------------------------------------*
FORM frm_get_item2  USING    p_pw_huizong_out TYPE x_huizong.
  DATA: lw_head TYPE x_head.
  DATA: lt_head TYPE TABLE OF x_head.
  DATA: lw_huizong TYPE x_huizong.

  MOVE-CORRESPONDING p_pw_huizong_out TO lw_huizong.
*按照只勾业务范围的汇总模式把对应的明细数据选取出来放进内表
  LOOP AT it_head INTO wa_head WHERE bukrs EQ lw_huizong-bukrs AND butxt EQ lw_huizong-butxt
                                 AND lifnr EQ lw_huizong-lifnr AND namev EQ lw_huizong-namev
                                 AND kunnr EQ lw_huizong-kunnr AND namec EQ lw_huizong-namec
                                 AND ksbm  EQ lw_huizong-ksbm  AND ksms  EQ lw_huizong-ksms
                                 AND guoj  EQ lw_huizong-guoj  AND ktokk EQ lw_huizong-ktokk
                                 AND gzhms EQ lw_huizong-gzhms AND ktokd EQ lw_huizong-ktokd
                                 AND kzhms EQ lw_huizong-kzhms AND koart EQ lw_huizong-koart
                                 AND klev1 EQ lw_huizong-klev1 AND yjkmt EQ lw_huizong-yjkmt
                                 AND hkont EQ lw_huizong-hkont AND txt50 EQ lw_huizong-txt50
                                 AND gsber EQ lw_huizong-gsber AND gtext EQ lw_huizong-gtext
                                 AND waers EQ lw_huizong-waers.


    MOVE-CORRESPONDING wa_head TO lw_head.
    APPEND lw_head TO lt_head.
  ENDLOOP.
*明细数据装入显示行内表
  CLEAR it_item.
  CLEAR wa_item.
  APPEND LINES OF lt_head TO it_item.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FIELD_ITEM2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_field_item2 .
  DATA:lv_dmbt1 TYPE char40.
  DATA:lv_dmbt2 TYPE char40.
  DATA:lv_dmbt3 TYPE char40.
  DATA:lv_dmbt4 TYPE char40.
  DATA:lv_dmbt5 TYPE char40.
  DATA:lv_dmbt6 TYPE char40.
  DATA:lv_dmbt7 TYPE char40.
  DATA:lv_dmbt8 TYPE char40.
  DATA:lv_dmbt9 TYPE char40.

  DATA:p_dy1 TYPE idcn_segm.
  DATA:p_dy2 TYPE idcn_segm.
  DATA:p_dy3 TYPE idcn_segm.
  DATA:p_dy4 TYPE idcn_segm.
  DATA:p_dy5 TYPE idcn_segm.
  DATA:p_dy6 TYPE idcn_segm.
  DATA:p_dy7 TYPE idcn_segm.
  DATA:p_dy8 TYPE idcn_segm.



  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date1
    IMPORTING
      output = p_date1.

  p_dy1 = p_date1 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date2
    IMPORTING
      output = p_dy2.
  p_dy2 = p_date2 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date3
    IMPORTING
      output = p_dy3.

  p_dy3 = p_date3 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date4
    IMPORTING
      output = p_dy4.

  p_dy4 = p_date4 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date5
    IMPORTING
      output = p_dy5.

  p_dy5 = p_date5 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date6
    IMPORTING
      output = p_dy6.

  p_dy6 = p_date6 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date7
    IMPORTING
      output = p_dy7.

  p_dy7 = p_date7 + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_date8
    IMPORTING
      output = p_dy8.

  p_dy8 = lv_dmbt8 + 1.

  CONCATENATE:'账龄小于等于'p_date1 '天' INTO lv_dmbt1.
  CONCATENATE:'账龄'p_dy1 '天到' p_date2 '天' INTO lv_dmbt2.
  CONCATENATE:'账龄'p_dy2 '天到' p_date3 '天' INTO lv_dmbt3.
  CONCATENATE:'账龄'p_dy3 '天到' p_date4 '天' INTO lv_dmbt4.
  CONCATENATE:'账龄'p_dy4 '天到' p_date5 '天' INTO lv_dmbt5.
  CONCATENATE:'账龄'p_dy5 '天到' p_date6 '天' INTO lv_dmbt6.
  CONCATENATE:'账龄'p_dy6 '天到' p_date7 '天' INTO lv_dmbt7.
  CONCATENATE:'账龄'p_dy7 '天到' p_date8 '天' INTO lv_dmbt8.


  CONCATENATE:'账龄大于等于' p_dy8 INTO lv_dmbt9.
  CLEAR it_fieldcat.
  PERFORM f_fieldcat_set TABLES it_fieldcat
  USING:  'KSBM'  TEXT-051 'X' '' '',  "客商编码
          'KSMS'  TEXT-052 'X' '' '',  "客商描述
          'BUKRS' TEXT-003 'X' '' '',  "公司代码
          'BUTXT' TEXT-057 'X' '' '',  "公司代码描述
          'GJAHR' TEXT-063 'X' '' '',  "会计年度
          'BELNR' TEXT-061 'X' 'X' '',  "会计凭证
          'BUZEI' TEXT-062 'X' 'X' '',  "会计凭证行
          'GUOJ'  TEXT-055 '' '' '',      "国家
          'KTOKD' TEXT-009 '' '' '',  "客户账户组
          'KZHMS' TEXT-049 '' '' '',  "客户账户组描述
          'KTOKK' TEXT-008 '' '' '',  "供应商账户组
          'GZHMS' TEXT-050 '' '' '',   "供应商账户组描述
          'KUNNR' TEXT-006 '' '' '',   "客户编码
          'NAMEC' TEXT-007 '' '' '',  "客户描述
          'LIFNR' TEXT-004 '' '' '',   "供应商编码
          'NAMEV' TEXT-005 '' '' '',   "供应商描述
          'KOART' TEXT-053 '' '' '',  "科目类型
          'KLEV1' TEXT-039 '' '' '',      "一级科目
          'YJKMT' TEXT-054 '' '' '',  "一级科目描述
          'HKONT' TEXT-010 '' '' '',  "会计帐目
          'TXT20' TEXT-011 '' '' '', "科目描述
          'SGL'   TEXT-056 '' '' '',      "SGL
          'PRCTR' TEXT-058 '' '' '', "利润中心
          'PRCTT' TEXT-059 '' '' '', "利润中心描述
          'GSBER' TEXT-060 '' '' '', "业务范围
          'GTEXT' TEXT-080 '' '' '', "业务范围描述
          'BLART' TEXT-064 '' '' '',  "凭证类型
          'TCODE' TEXT-065 '' '' '', "事务代码
          'ZUONR' TEXT-066 '' '' '',  "分配
          'SGTXT' TEXT-067 '' '' '',  "项目文本
          'BLDAT' TEXT-068 '' '' '', "凭证日期
          'BUDAT' TEXT-069 '' '' '',  "过账日期
          'DUDAT' TEXT-070 '' '' '',  "到期日
          'AUGBL' TEXT-071 '' '' '',  "清账凭证号
          'AUGDT' TEXT-072 '' '' '',  "清账日期
          'KSYE ' TEXT-073 '' '' 'X', "客商余额
          'WDQJE' TEXT-074 '' '' 'X',  "未到期余额
          'ZLJE'  TEXT-075 '' '' 'X',  "账龄金额
          'WAERS' TEXT-076 '' '' '',  "币种
          'XTYPE' TEXT-077 '' '' '',       "账龄类型
          'DMBT1' lv_dmbt1 ''  '' 'X',
          'DMBT2' lv_dmbt2 ''  '' 'X',
          'DMBT3' lv_dmbt3 ''  '' 'X',
          'DMBT4' lv_dmbt4 ''  '' 'X',
          'DMBT5' lv_dmbt5 ''  '' 'X',
          'DMBT6' lv_dmbt6 ''  '' 'X',
          'DMBT7' lv_dmbt7 ''  '' 'X',
          'DMBT8' lv_dmbt8 ''  '' 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_ITEM3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PW_HUIZONG_OUT  text
*----------------------------------------------------------------------*
FORM frm_get_item3  USING    p_pw_huizong_out TYPE x_huizong.
  DATA: lw_head TYPE x_head.
  DATA: lt_head TYPE TABLE OF x_head.
  DATA: lw_huizong TYPE x_huizong.

  MOVE-CORRESPONDING p_pw_huizong_out TO lw_huizong.
*按照同时勾选利润中心和业务范围的汇总模式把对应的明细数据选取出来放进内表
  LOOP AT it_head INTO wa_head WHERE bukrs EQ lw_huizong-bukrs AND butxt EQ lw_huizong-butxt
                                 AND lifnr EQ lw_huizong-lifnr AND namev EQ lw_huizong-namev
                                 AND kunnr EQ lw_huizong-kunnr AND namec EQ lw_huizong-namec
                                 AND ksbm  EQ lw_huizong-ksbm  AND ksms  EQ lw_huizong-ksms
                                 AND guoj  EQ lw_huizong-guoj  AND ktokk EQ lw_huizong-ktokk
                                 AND gzhms EQ lw_huizong-gzhms AND ktokd EQ lw_huizong-ktokd
                                 AND kzhms EQ lw_huizong-kzhms AND koart EQ lw_huizong-koart
                                 AND klev1 EQ lw_huizong-klev1 AND yjkmt EQ lw_huizong-yjkmt
                                 AND hkont EQ lw_huizong-hkont AND txt50 EQ lw_huizong-txt50
                                 AND prctr EQ lw_huizong-prctr AND prctt EQ lw_huizong-prctt
                                 AND gsber EQ lw_huizong-gsber AND gtext EQ lw_huizong-gtext
                                 AND waers EQ lw_huizong-waers.


    MOVE-CORRESPONDING wa_head TO lw_head.
    APPEND lw_head TO lt_head.
  ENDLOOP.
*明细数据装入显示行内表
  CLEAR it_item.
  CLEAR wa_item.
  APPEND LINES OF lt_head TO it_item.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_ITEM4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PW_HUIZONG_OUT  text
*----------------------------------------------------------------------*
FORM frm_get_item4  USING    p_pw_huizong_out TYPE x_huizong.
  DATA: lw_head TYPE x_head.
  DATA: lt_head TYPE TABLE OF x_head.
  DATA: lw_huizong TYPE x_huizong.

  MOVE-CORRESPONDING p_pw_huizong_out TO lw_huizong.
*按照同时不勾选利润中心和业务范围的汇总模式把对应的明细数据选取出来放进内表
  LOOP AT it_head INTO wa_head WHERE bukrs EQ lw_huizong-bukrs AND butxt EQ lw_huizong-butxt
                                 AND lifnr EQ lw_huizong-lifnr AND namev EQ lw_huizong-namev
                                 AND kunnr EQ lw_huizong-kunnr AND namec EQ lw_huizong-namec
                                 AND ksbm  EQ lw_huizong-ksbm  AND ksms  EQ lw_huizong-ksms
                                 AND guoj  EQ lw_huizong-guoj  AND ktokk EQ lw_huizong-ktokk
                                 AND gzhms EQ lw_huizong-gzhms AND ktokd EQ lw_huizong-ktokd
                                 AND kzhms EQ lw_huizong-kzhms AND koart EQ lw_huizong-koart
                                 AND klev1 EQ lw_huizong-klev1 AND yjkmt EQ lw_huizong-yjkmt
                                 AND hkont EQ lw_huizong-hkont AND txt50 EQ lw_huizong-txt50
                                 AND waers EQ lw_huizong-waers.


    MOVE-CORRESPONDING wa_head TO lw_head.
    APPEND lw_head TO lt_head.
  ENDLOOP.
*明细数据装入显示行内表
  CLEAR it_item.
  CLEAR wa_item.
  APPEND LINES OF lt_head TO it_item.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SUM_MONEY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_sum_money .

  DATA: lt_head  TYPE TABLE OF x_head.
  DATA: lw_head  TYPE x_head.
  DATA: lv_ksye  TYPE bsik-dmbtr,  "客商余额
        lv_wdqje TYPE bsik-dmbtr,  "未到期余额
        lv_zlje  TYPE bsik-dmbtr,  "账龄金额
        lv_dmbt0 TYPE bsik-dmbtr,  "账期内
        lv_dmbt1 TYPE bsik-dmbtr,                                 "30天以内
        lv_dmbt2 TYPE bsik-dmbtr,                                 "31到60
        lv_dmbt3 TYPE bsik-dmbtr,                                 "61到90
        lv_dmbt4 TYPE bsik-dmbtr,                                 "3到6个月
        lv_dmbt5 TYPE bsik-dmbtr,                                 "6到12个月
        lv_dmbt6 TYPE bsik-dmbtr,                                 "1到2年
        lv_dmbt7 TYPE bsik-dmbtr,                                 "2到3年
        lv_dmbt8 TYPE bsik-dmbtr,                                 "3到4年
        lv_dmbt9 TYPE bsik-dmbtr.                                 "4到5年


*明细模式
  IF r_1 EQ 'X'.
    CLEAR wa_head.
    LOOP AT it_head INTO wa_head.
      lv_ksye  = lv_ksye  + wa_head-ksye.
      lv_wdqje = lv_wdqje + wa_head-wdqje.
      lv_zlje  = lv_zlje  + wa_head-zlje.
      lv_dmbt0 = lv_dmbt0 + wa_head-dmbt0.
      lv_dmbt1 = lv_dmbt1 + wa_head-dmbt1.
      lv_dmbt2 = lv_dmbt2 + wa_head-dmbt2.
      lv_dmbt3 = lv_dmbt3 + wa_head-dmbt3.
      lv_dmbt4 = lv_dmbt4 + wa_head-dmbt4.
      lv_dmbt5 = lv_dmbt5 + wa_head-dmbt5.
      lv_dmbt6 = lv_dmbt6 + wa_head-dmbt6.
      lv_dmbt7 = lv_dmbt7 + wa_head-dmbt7.
      lv_dmbt8 = lv_dmbt8 + wa_head-dmbt8.
      lv_dmbt9 = lv_dmbt9 + wa_head-dmbt9.
    ENDLOOP.

    lw_head-ksye  = lv_ksye.
    lw_head-wdqje = lv_wdqje.
    lw_head-zlje  = lv_zlje.
    lw_head-dmbt0 = lv_dmbt0.
    lw_head-dmbt1 = lv_dmbt1.
    lw_head-dmbt2 = lv_dmbt2.
    lw_head-dmbt3 = lv_dmbt3.
    lw_head-dmbt4 = lv_dmbt4.
    lw_head-dmbt5 = lv_dmbt5.
    lw_head-dmbt6 = lv_dmbt6.
    lw_head-dmbt7 = lv_dmbt7.
    lw_head-dmbt8 = lv_dmbt8.
    lw_head-dmbt9 = lv_dmbt9.

    CLEAR wa_head.
    MOVE-CORRESPONDING lw_head TO wa_head.
    APPEND wa_head TO it_head.
    CLEAR wa_head.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SUM_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_sum_item .
  DATA: lt_item  TYPE TABLE OF x_head.
  DATA: lw_item  TYPE x_head.
  DATA: lv_ksye  TYPE bsik-dmbtr,  "客商余额
        lv_wdqje TYPE bsik-dmbtr,  "未到期余额
        lv_zlje  TYPE bsik-dmbtr,  "账龄金额
        lv_dmbt0 TYPE bsik-dmbtr,  "账期内
        lv_dmbt1 TYPE bsik-dmbtr,                                 "30天以内
        lv_dmbt2 TYPE bsik-dmbtr,                                 "31到60
        lv_dmbt3 TYPE bsik-dmbtr,                                 "61到90
        lv_dmbt4 TYPE bsik-dmbtr,                                 "3到6个月
        lv_dmbt5 TYPE bsik-dmbtr,                                 "6到12个月
        lv_dmbt6 TYPE bsik-dmbtr,                                 "1到2年
        lv_dmbt7 TYPE bsik-dmbtr,                                 "2到3年
        lv_dmbt8 TYPE bsik-dmbtr,                                 "3到4年
        lv_dmbt9 TYPE bsik-dmbtr.                                 "4到5年


  CLEAR wa_item.
  LOOP AT it_item INTO wa_item.
    lv_ksye  = lv_ksye  + wa_item-ksye.
    lv_wdqje = lv_wdqje + wa_item-wdqje.
    lv_zlje  = lv_zlje  + wa_item-zlje.
    lv_dmbt0 = lv_dmbt0 + wa_item-dmbt0.
    lv_dmbt1 = lv_dmbt1 + wa_item-dmbt1.
    lv_dmbt2 = lv_dmbt2 + wa_item-dmbt2.
    lv_dmbt3 = lv_dmbt3 + wa_item-dmbt3.
    lv_dmbt4 = lv_dmbt4 + wa_item-dmbt4.
    lv_dmbt5 = lv_dmbt5 + wa_item-dmbt5.
    lv_dmbt6 = lv_dmbt6 + wa_item-dmbt6.
    lv_dmbt7 = lv_dmbt7 + wa_item-dmbt7.
    lv_dmbt8 = lv_dmbt8 + wa_item-dmbt8.
  ENDLOOP.

  lw_item-ksye  = lv_ksye.
  lw_item-wdqje = lv_wdqje.
  lw_item-zlje  = lv_zlje.
  lw_item-dmbt0 = lv_dmbt0.
  lw_item-dmbt1 = lv_dmbt1.
  lw_item-dmbt2 = lv_dmbt2.
  lw_item-dmbt3 = lv_dmbt3.
  lw_item-dmbt4 = lv_dmbt4.
  lw_item-dmbt5 = lv_dmbt5.
  lw_item-dmbt6 = lv_dmbt6.
  lw_item-dmbt7 = lv_dmbt7.
  lw_item-dmbt8 = lv_dmbt8.

  CLEAR wa_item.
  MOVE-CORRESPONDING lw_item TO wa_item.
  APPEND wa_item TO it_item.
  CLEAR wa_item.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SUB_POPULATE_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_sub_populate_sort .
  ""排序用
  wa_sort-spos = 1 .
  wa_sort-fieldname = 'BUKRS'.
*  wa_sort-tabname = 'GT_OUTPUT'.
*  wa_sort-up = 'X'.
  wa_sort-up = 'X'.
  wa_sort-subtot = 'X'.
  APPEND wa_sort TO it_sort .
  CLEAR wa_sort.
ENDFORM.


*Messages
*----------------------------------------------------------
*
* Message class: F0
*110   Reference data does not exist
*
* Message class: F2
*163   Vendor & has not been created
*246   No customer has been created
*
* Message class: F4
*069   Not a valid company code entered
*218   No display authorization for company code &

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
