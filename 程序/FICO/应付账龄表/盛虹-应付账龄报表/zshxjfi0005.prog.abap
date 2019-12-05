**************************************************
*��������:Ӧ�������
*��������: 2019-11-20
*������:XXX
*������:XXX
*��������:
*============================================
*�����¼
*�޸������    �޸�����    �޸���   �޸�����
*DEVK912011    2019-11-20   HANDYXH    ��������
***************************************************
REPORT ZSHXJFI0005.


************************************************************************
*            <��һ����---��������Ĳ����ͱ���>  Declaration            *
************************************************************************
*----------------------------------------------------------------------*
* <1.1-����������> Table_Work_Areas Declaration                      *
*----------------------------------------------------------------------*
TABLES:
  kna1,
  lfa1,
  t001,
  skb1,
  bseg,
  bsad.


TYPE-POOLS: slis.

CONSTANTS: c_x      TYPE c      VALUE 'X'.      "ѡ�б�ʶ
CONSTANTS: c_active TYPE char2  VALUE '03'.     "����Ȩ�޼��
CONSTANTS: c_h      TYPE c      VALUE 'H'.      "������־
CONSTANTS: c_d000 TYPE char5  VALUE 'D000',   "������
           c_d001 TYPE char5  VALUE 'D001',   "����С�ڵ���30��
           c_d002 TYPE char5  VALUE 'D002',   "����31��60��
           c_d003 TYPE char5  VALUE 'D003',   "����61��90��
           c_d004 TYPE char5  VALUE 'D004',   "����91��120��
           c_d005 TYPE char5  VALUE 'D005',   "����121��180��
           c_d006 TYPE char5  VALUE 'D006',   "����181��360��
           c_d007 TYPE char5  VALUE 'D007',   "����361��720��
           c_d008 TYPE char5  VALUE 'D008',   "����720��1440��
           c_d009 TYPE char5  VALUE 'D009'.   "������ڵ���1440��
CONSTANTS: c_cust  TYPE char4  VALUE 'CUST',   "�ͻ�����
           c_vend  TYPE char4  VALUE 'VEND',   "��Ӧ������
           c_total TYPE char5  VALUE 'TOTAL'.  "�ֶ�TOTAL����
CONSTANTS: c_msg_e  TYPE c      VALUE 'E'.      "������Ϣ��־
CONSTANTS: c_item   TYPE char7  VALUE 'DISITEM'. "��ʾ��ϸ���ݵĹ�����
CONSTANTS: c_a      TYPE c      VALUE 'A'.      "ALVִ�еķ�ʽ
CONSTANTS: c_box    TYPE c LENGTH 3      VALUE 'BOX'.    "ALVѡ�����
CONSTANTS: c_belnr  TYPE char7  VALUE 'BELNR'.  "��ʾƾ֤�Ĺ�����

*----------------------------------------------------------------------*
* <1.4-�����û��Զ�����������> Local Data Types in Program             *
*----------------------------------------------------------------------*
TYPES: BEGIN OF x_t001,
         bukrs TYPE t001-bukrs,  "��˾����
         waers TYPE t001-waers,  "����
         ktopl TYPE t001-ktopl,  "��Ŀ��
         land1 TYPE t001-land1,  "����
       END OF x_t001.
TYPES: BEGIN OF x_knb1,
         kunnr TYPE knb1-kunnr,  "�ͻ�����
       END OF x_knb1.
TYPES: BEGIN OF x_lfb1,
         lifnr TYPE lfa1-lifnr,  "��Ӧ�̱���
       END OF x_lfb1.
TYPES: BEGIN OF x_head,
         box       TYPE char1,
         ktokd     TYPE kna1-ktokd,  "�ͻ��˻���
         kzhms     TYPE t077x-txt30, "�ͻ��˻�������
         ktokk     TYPE lfa1-ktokk,  "��Ӧ���˻���
         gzhms     TYPE t077y-txt30,  "��Ӧ���˻�������
         kunnr     TYPE bsid-kunnr,  "�ͻ�����
         namec     TYPE kna1-name1,  "�ͻ�����
         lifnr     TYPE bsik-lifnr,  "��Ӧ�̱���
         namev     TYPE lfa1-name1,  "��Ӧ������
         ksbm      TYPE bsid-kunnr,  "���̱���
         ksms      TYPE kna1-name1,  "��������
         guoj      TYPE landx,      "����
         koart     TYPE bseg-koart,  "��Ŀ����
         klev1     TYPE char4,       "һ����Ŀ
         yjkmt     TYPE skat-txt50,  "һ����Ŀ����
         hkont     TYPE bsik-hkont,  "�����Ŀ
         txt50     TYPE skat-txt50,  "��Ŀ����
         sgl       TYPE char10,      "SGL
         bukrs     TYPE bsik-bukrs,  "��˾����
         butxt     TYPE t001-butxt,  "��˾��������
         prctr     TYPE bseg-prctr,  "��������
         prctt     TYPE cepct-ltext, "������������
         gsber     TYPE bsik-gsber,  "ҵ��Χ
         gtext     TYPE tgsbt-gtext, "ҵ��Χ����
         belnr     TYPE bsik-belnr,  "���ƾ֤
         buzei     TYPE bsik-buzei,  "���ƾ֤��
         gjahr     TYPE bsik-gjahr,  "������
         xblnr_alt TYPE bkpf-xblnr_alt, "����ƾ֤��
         blart     TYPE bsik-blart,  "ƾ֤����
         tcode     TYPE bkpf-tcode,  "�������
         zuonr     TYPE bsik-zuonr,  "����
         sgtxt     TYPE bsik-sgtxt,  "��Ŀ�ı�
         bldat     TYPE bsik-bldat,  "ƾ֤����
         budat     TYPE bsik-budat,  "��������
         dudat     TYPE bsik-budat,  "������
         augbl     TYPE bsik-augbl,  "����ƾ֤��
         augdt     TYPE bsik-augdt,  "��������
         ksye      TYPE bsik-dmbtr,  "�������
         wdqje     TYPE bsik-dmbtr,  "δ�������
         zlje      TYPE bsik-dmbtr,  "������
         waers     TYPE bsik-waers,  "����

         zlje_bb   TYPE bsik-dmbtr,  "���䱾λ�ҽ��
         waers_bb  TYPE bsik-waers,  "����

         xtype     TYPE char4,       "��������
         total     TYPE bsik-dmbtr,  "�ܼ�
         dmbt0     TYPE bsik-dmbtr,  "������
         dmbt1     TYPE bsik-dmbtr,                                 "30������
         dmbt2     TYPE bsik-dmbtr,                                 "31��60
         dmbt3     TYPE bsik-dmbtr,                                 "61��90
         dmbt4     TYPE bsik-dmbtr,                                 "3��6����
         dmbt5     TYPE bsik-dmbtr,                                 "6��12����
         dmbt6     TYPE bsik-dmbtr,                                 "1��2��
         dmbt7     TYPE bsik-dmbtr,                                 "2��3��
         dmbt8     TYPE bsik-dmbtr,                                 "3��4��
         dmbt9     TYPE bsik-dmbtr,                                 "4��5��
         dmbt10    TYPE bsik-dmbtr,                                 "5������
*ADD by PENGLIXUE at 20180224 BPMNO: ERP-YW201802020036
         kkber     LIKE bsid-kkber,
         kkbtx     LIKE t014t-kkbtx,
*end of ADD by PENGLIXUE
       END OF x_head,

       BEGIN OF x_item,
         bukrs TYPE bsik-bukrs,  "��˾����
         gjahr TYPE bsid-gjahr,  "������
         lifnr TYPE bsik-lifnr,  "�ͻ�����
         namev TYPE lfa1-name1,  "�ͻ�����
         kunnr TYPE bsid-kunnr,  "�ͻ�����
         namec TYPE kna1-name1,  "�ͻ�����
         ktokk TYPE lfa1-ktokk,  "��Ӧ���˻���
         ktokd TYPE kna1-ktokd,  "�ͻ��˻���
         budat TYPE bsik-budat,  "��������
         augdt TYPE bsak-augdt,  "��������
         dudat TYPE bsik-budat,  "���ո�������
         umskz TYPE bsik-umskz,  "�������ʱ�ʶ
         zterm TYPE bsik-zterm,  "������������
         text1 TYPE t052u-text1, "������������
         ducdt TYPE int4,        "��������
         belnr TYPE bsik-belnr,  "���ƾ֤���
         augbl TYPE bsak-augbl,  "����ƾ֤
         buzei TYPE bsik-buzei,  "���ƾ֤�е�����Ŀ��
         bschl TYPE bsik-bschl,  "���ʴ���
         shkzg TYPE bsik-shkzg,  "�跽/������ʶ
         xnegp TYPE bsik-xnegp,  "��ʶ: ������
         klev1 TYPE char4,       " һ����Ŀ
         hkont TYPE bsik-hkont,  "�ܷ�������Ŀ
         txt20 TYPE skat-txt20,  "��Ŀ����
         wrbtr TYPE bsid-wrbtr,  "ƾ֤���ҽ��
         waers TYPE bsid-waers,  "ƾ֤��������
         dmbtr TYPE bsik-dmbtr,  "����λ�ҼƵĽ��
         waerb TYPE t001-waers,  "��λ������
         sgtxt TYPE bsid-sgtxt,  "��Ŀ�ı�
*    �����˺źͿ�����
         bankl TYPE lfbk-bankl,
         banka TYPE bnka-banka,
         banno TYPE char35,
       END OF x_item.
TYPES: BEGIN OF x_kna1,
         kunnr TYPE kna1-kunnr,  "�ͻ�����
         name1 TYPE kna1-name1,  "�ͻ�����
         ktokd TYPE kna1-ktokd,  "�ͻ��˻���
         land1 TYPE kna1-land1,  "����
       END OF x_kna1.
TYPES: BEGIN OF x_t077d,
         ktokd TYPE kna1-ktokd,  "�ͻ��˻���
         kzhms TYPE t077x-txt30, "�ͻ��˻�������
       END OF x_t077d.
TYPES: BEGIN OF x_t077k,
         ktokk TYPE lfa1-ktokk,  "�ͻ��˻���
         gzhms TYPE t077y-txt30, "�ͻ��˻�������
       END OF x_t077k.


TYPES: BEGIN OF x_acc_cus,
         bukrs TYPE bsid-bukrs,  "��˾����
         kunnr TYPE bsid-kunnr,  "�ͻ�����
         umskz TYPE bsid-umskz,  "�������ʱ�ʶ
         augdt TYPE bsid-augdt, "��������
         augbl TYPE bsid-augbl,  "����ƾ֤
         gjahr TYPE bsid-gjahr,  "������
         belnr TYPE bsid-belnr,  "���ƾ֤���
         buzei TYPE bsid-buzei,  "���ƾ֤�е�����Ŀ��
         budat TYPE bsid-budat,  "ƾ֤�еĹ�������
         waers TYPE bsid-waers,  "ƾ֤��������
         bschl TYPE bsid-bschl,  "���ʴ���
         shkzg TYPE bsid-shkzg,  "�跽/������ʶ
         dmbtr TYPE bsid-dmbtr,  "����λ�ҼƵĽ��
         wrbtr TYPE bsid-wrbtr,  "ƾ֤���ҽ��
         sgtxt TYPE bsid-sgtxt,  "��Ŀ�ı�
         hkont TYPE bsid-hkont,  "�ܷ�������Ŀ
         zfbdt TYPE bsid-zfbdt,  "���ڵ����ռ���Ļ�׼����
         zterm TYPE bsid-zterm,  "������������
         zbd1t TYPE bsid-zbd1t,  "�ֽ��ۿ�����
         zbd2t TYPE bsid-zbd2t,  "�ֽ��ۿ�����
         zbd3t TYPE bsid-zbd3t,  "��֧�������ڶ�
         rebzg TYPE bsid-rebzg,  "ҵ�������ķ�Ʊ����
         xnegp TYPE bsid-xnegp,  "��ʶ: ������
         dudat TYPE bsik-budat,  "���ո�������
         ducdt TYPE int4,        "��������
         namec TYPE kna1-name1,  "�ͻ�����
         ktokd TYPE kna1-ktokd,  "�ͻ��˻���
         kzhms TYPE t077x-txt30, "�ͻ��˻�������
         waerb TYPE t001-waers,  "��λ��
         ktopl TYPE t001-ktopl,  "��Ŀ��
         perid TYPE char5,       "���������־
         xtype TYPE char4,       "��������
         gsber TYPE bsid-gsber,   "ҵ��Χ
         zuonr TYPE bsid-zuonr,   "����
         bldat TYPE bsid-bldat,   "ƾ֤����
         blart TYPE bkpf-blart,   "ƾ֤����
         land1 TYPE kna1-land1,   "����
*ADD by PENGLIXUE at 20180224 BPMNO: ERP-YW201802020036
         kkber LIKE bsid-kkber,
*end of ADD by PENGLIXUE
       END OF x_acc_cus.
TYPES: BEGIN OF x_acc_ven,
         bukrs TYPE bsik-bukrs,  "��˾����
         lifnr TYPE bsik-lifnr,  "��Ӧ�̱���
         umskz TYPE bsik-umskz,  "�������ʱ�ʶ
         augdt TYPE bsik-augdt, "��������
         augbl TYPE bsik-augbl,  "����ƾ֤
         gjahr TYPE bsik-gjahr,  "������
         belnr TYPE bsik-belnr,  "���ƾ֤���
         buzei TYPE bsik-buzei,  "���ƾ֤�е�����Ŀ��
         budat TYPE bsik-budat,  "ƾ֤�еĹ�������
         waers TYPE bsik-waers,  "ƾ֤��������
         bschl TYPE bsik-bschl,  "���ʴ���
         shkzg TYPE bsik-shkzg,  "�跽/������ʶ
         dmbtr TYPE bsik-dmbtr,  "����λ�ҼƵĽ��
         wrbtr TYPE bsik-wrbtr,  "ƾ֤���ҽ��
         sgtxt TYPE bsik-sgtxt,  "��Ŀ�ı�
         hkont TYPE bsik-hkont,  "�ܷ�������Ŀ
         zfbdt TYPE bsik-zfbdt,  "���ڵ����ռ���Ļ�׼����
         zterm TYPE bsik-zterm,  "������������
         zbd1t TYPE bsik-zbd1t,  "�ֽ��ۿ�����
         zbd2t TYPE bsik-zbd2t,  "�ֽ��ۿ�����
         zbd3t TYPE bsik-zbd3t,  "��֧�������ڶ�
         rebzg TYPE bsik-rebzg,  "ҵ�������ķ�Ʊ����
         xnegp TYPE bsik-xnegp,  "��ʶ: ������
         dudat TYPE bsik-budat,  "���ո�������
         ducdt TYPE int4,        "��������
         namev TYPE lfa1-name1,  "��Ӧ������
         ktokk TYPE lfa1-ktokk,  "��Ӧ���˻���
         gzhms TYPE t077y-txt30, "��Ӧ���˻�������
         waerb TYPE t001-waers,  "��λ��
         ktopl TYPE t001-ktopl,  "��Ŀ��
         perid TYPE char5,       "���������־
         xtype TYPE char4,       "��������
         fbsis TYPE char1,       "�����Ƿ�������BSIS
         bankl TYPE lfbk-bankl,
         banka TYPE bnka-banka,
         banno TYPE char35,
         gsber TYPE bsid-gsber,   "ҵ��Χ
         zuonr TYPE bsid-zuonr,   "����
         bldat TYPE bsid-bldat,   "ƾ֤����
         land1 TYPE lfa1-land1,   "����
       END OF x_acc_ven.
TYPES: BEGIN OF x_account,
         bukrs TYPE bsik-bukrs,  "��˾����
         gjahr TYPE bsid-gjahr,  "������
         lifnr TYPE bsik-lifnr,  "��Ӧ�̱���
         namev TYPE lfa1-name1,  "��Ӧ������
         kunnr TYPE bsid-kunnr,  "�ͻ�����
         namec TYPE kna1-name1,  "�ͻ�����
         ktokk TYPE lfa1-ktokk,  "��Ӧ���˻���
         ktokd TYPE kna1-ktokd,  "�ͻ��˻���
         hkont TYPE bsik-hkont,  "��ƿ�Ŀ
         txt20 TYPE skat-txt20,  "��Ŀ����
         dmbtr TYPE bsik-dmbtr,  "����λ�ҼƵĽ��
         umskz TYPE bsid-umskz,  "�������ʱ�ʶ
         belnr TYPE bsid-belnr,  "���ƾ֤���
         buzei TYPE bsid-buzei,  "���ƾ֤�е�����Ŀ��
         budat TYPE bsid-budat,  "ƾ֤�еĹ�������
         waers TYPE bsid-waers,  "ƾ֤��������
         bschl TYPE bsid-bschl,  "���ʴ���
         shkzg TYPE bsid-shkzg,  "�跽/������ʶ
         wrbtr TYPE bsid-wrbtr,  "ƾ֤���ҽ��
         zfbdt TYPE bsid-zfbdt,  "���ڵ����ռ���Ļ�׼����
         zterm TYPE bsid-zterm,  "������������
         xnegp TYPE bsid-xnegp,  "��ʶ: ������
         waerb TYPE t001-waers,  "��λ������
         ktopl TYPE t001-ktopl,  "��Ŀ��
         sgtxt TYPE bsid-sgtxt,  "�ı�
         perid TYPE char5,       "���������־
         xtype TYPE char4,       "��������
         dudat TYPE bsik-budat,  "���ո�������
         ducdt TYPE int4,        "��������
         augdt TYPE bsik-augdt,  "��������
         augbl TYPE bsik-augbl,  "����ƾ֤
         bankl TYPE lfbk-bankl,
         banka TYPE bnka-banka,
         banno TYPE char35,
       END OF x_account.

TYPES: BEGIN OF x_lfa1,
         lifnr TYPE lfa1-lifnr,  "��Ӧ�̱���
         name1 TYPE lfa1-name1,  "��Ӧ������
         ktokk TYPE lfa1-ktokk,  "��Ӧ���˻���
         land1 TYPE lfa1-land1,  "����
       END OF x_lfa1.

TYPES: BEGIN OF x_bsis,
         bukrs TYPE bsis-bukrs,  "��˾����
         augdt TYPE bsis-augdt,  "��������
         augbl TYPE bsis-augbl,  "����ƾ֤
         zuonr TYPE bsis-zuonr,  "������
         gjahr TYPE bsis-gjahr,  "������
         belnr TYPE bsis-belnr,  "���ƾ֤���
         buzei TYPE bsis-buzei,  "���ƾ֤�е�����Ŀ��
         budat TYPE bsis-budat,  "ƾ֤�еĹ�������
         waers TYPE bsis-waers,  "ƾ֤��������
         bschl TYPE bsis-bschl,  "���ʴ���
         shkzg TYPE bsis-shkzg,  "�跽/������ʶ
         dmbtr TYPE bsis-dmbtr,  "����λ�ҼƵĽ��
         wrbtr TYPE bsis-wrbtr,  "ƾ֤���ҽ��
         sgtxt TYPE bsis-sgtxt,  "��Ŀ�ı�
         hkont TYPE bsis-hkont,  "�ܷ�������Ŀ
         zfbdt TYPE bsis-zfbdt,  "���ڵ����ռ���Ļ�׼����
         xnegp TYPE bsis-xnegp,  "��ʶ: ������
         gsber TYPE bsid-gsber,   "ҵ��Χ
         bldat TYPE bsid-bldat,   "ƾ֤����
       END OF x_bsis.

TYPES: BEGIN OF x_hkont,
         bukrs TYPE skb1-bukrs,  "��˾����
         saknr TYPE skb1-saknr,  "��ƿ�Ŀ
         mitkz TYPE skb1-mitkz,  "��Ŀ��ͳԦ��Ŀ
       END OF x_hkont,

       BEGIN OF x_skat,
         spras TYPE skat-spras,  "����
         ktopl TYPE skat-ktopl,  "��Ŀ��
         saknr TYPE skat-saknr,  "���ʿ�Ŀ���
         txt50 TYPE skat-txt50,  "���ʿ�Ŀ����
       END OF x_skat.

TYPES: BEGIN OF x_cus,
         bukrs TYPE bsid-bukrs,  "��˾����
         kunnr TYPE bsid-kunnr,  "�ͻ�����
         namec TYPE kna1-name1,  "�ͻ�����
         ktokd TYPE kna1-ktokd,  "�ͻ��˻���
         hkont TYPE bsid-hkont,  "�ܷ�������Ŀ
         txt20 TYPE skat-txt20,  "��Ŀ����
         xtype TYPE char4,       "��������
         total TYPE bsid-dmbtr,  "�ܼ�
         dmbt0 TYPE bsik-dmbtr,  "������
         dmbt1 TYPE bsik-dmbtr,                                 "30������
         dmbt2 TYPE bsik-dmbtr,                                 "31��60
         dmbt3 TYPE bsik-dmbtr,                                 "61��90
         dmbt4 TYPE bsik-dmbtr,                                 "91��120��
         dmbt5 TYPE bsik-dmbtr,                                 "121��180��
         dmbt6 TYPE bsik-dmbtr,                                 "181��360��
         dmbt7 TYPE bsik-dmbtr,                                 "361��720��
         dmbt8 TYPE bsik-dmbtr,                                 "���ڵ���721��
       END OF x_cus.

TYPES: BEGIN OF x_ven,
         bukrs TYPE bsik-bukrs,  "��˾����
         lifnr TYPE bsik-lifnr,  "�ͻ�����
         namev TYPE lfa1-name1,  "�ͻ�����
         ktokk TYPE lfa1-ktokk,  "��Ӧ���˻���
         hkont TYPE bsik-hkont,  "�ܷ�������Ŀ
         txt20 TYPE skat-txt20,  "��Ŀ����
         xtype TYPE char4,       "��������
         total TYPE bsik-dmbtr,  "�ܼ�
         dmbt0 TYPE bsik-dmbtr,  "������
         dmbt1 TYPE bsik-dmbtr,                                 "30������
         dmbt2 TYPE bsik-dmbtr,                                 "31��60
         dmbt3 TYPE bsik-dmbtr,                                 "61��90
         dmbt4 TYPE bsik-dmbtr,                                 "91��120��
         dmbt5 TYPE bsik-dmbtr,                                 "121��180��
         dmbt6 TYPE bsik-dmbtr,                                 "181��360��
         dmbt7 TYPE bsik-dmbtr,                                 "361��720��
         dmbt8 TYPE bsik-dmbtr,                                 "���ڵ���721��
       END OF x_ven.

TYPES: BEGIN OF x_bseg,
         bukrs TYPE bseg-bukrs,   "��˾����
         belnr TYPE bseg-belnr,   "ƾ֤���
         gjahr TYPE bseg-gjahr,   "������
         buzei TYPE bseg-buzei,   "ƾ֤�к�
         koart TYPE bseg-koart,  "��Ŀ����
         prctr TYPE bseg-prctr,   "��������
       END OF x_bseg.

TYPES: BEGIN OF x_bukrs,
         bukrs TYPE t001-bukrs,  "��˾����
         butxt TYPE t001-butxt,  "��˾�ı�
       END OF x_bukrs.

TYPES: BEGIN OF x_prctr,
         prctr TYPE cepct-prctr,  "��������
         ltext TYPE cepct-ltext,  "������������
       END OF x_prctr.
TYPES: BEGIN OF x_guojia,
         land1 TYPE t005t-land1,  "���Ҵ���
         landx TYPE t005t-landx,  "�����ı�
       END OF x_guojia.
TYPES: BEGIN OF x_bkpf,
         bukrs     TYPE bkpf-bukrs,
         belnr     TYPE bkpf-belnr,
         gjahr     TYPE bkpf-gjahr,
         tcode     TYPE bkpf-tcode,   "�������
         blart     TYPE bkpf-blart,   "ƾ֤����
         xblnr_alt TYPE bkpf-xblnr_alt, "����ƾ֤��
       END OF x_bkpf.

TYPES: BEGIN OF x_huizong,
         box      TYPE char1,
         bukrs    TYPE bsik-bukrs,  "��˾����
         butxt    TYPE t001-butxt,  "��˾��������
         lifnr    TYPE bsik-lifnr,  "��Ӧ�̱���
         namev    TYPE lfa1-name1,  "��Ӧ������
         kunnr    TYPE bsid-kunnr,  "�ͻ�����
         namec    TYPE kna1-name1,  "�ͻ�����
         ksbm     TYPE bsid-kunnr,  "���̱���
         ksms     TYPE kna1-name1,  "��������
         guoj     TYPE landx,      "����
         ktokk    TYPE lfa1-ktokk,  "��Ӧ���˻���
         gzhms    TYPE t077y-txt30,  "��Ӧ���˻�������
         ktokd    TYPE kna1-ktokd,  "�ͻ��˻���
         kzhms    TYPE t077x-txt30, "�ͻ��˻�������
         koart    TYPE bseg-koart,  "��Ŀ����
         klev1    TYPE char4,       "һ����Ŀ
         yjkmt    TYPE skat-txt20,  "һ����Ŀ����
         hkont    TYPE bsik-hkont,  "�����Ŀ
         txt50    TYPE skat-txt50,  "��Ŀ����
         prctr    TYPE bseg-prctr,  "��������
         prctt    TYPE cepct-ltext, "������������
         gsber    TYPE bsik-gsber,  "ҵ��Χ
         gtext    TYPE tgsbt-gtext, "ҵ��Χ����
         waers    TYPE bsik-waers,  "����
         zlje     TYPE bsik-dmbtr,  "������

         zlje_bb  TYPE bsik-dmbtr,  "���䱾λ�ҽ��
         waers_bb TYPE bsik-waers,  "����

         total    TYPE bsik-dmbtr,  "�ܼ�
         wdqje    TYPE bsik-dmbtr,  "δ�������
         xtype    TYPE char4,       "��������
         dmbt1    TYPE bsik-dmbtr,                                 "30������
         dmbt2    TYPE bsik-dmbtr,                                 "31��60
         dmbt3    TYPE bsik-dmbtr,                                 "61��90
         dmbt4    TYPE bsik-dmbtr,                                 "3��6����
         dmbt5    TYPE bsik-dmbtr,                                 "6��12����
         dmbt6    TYPE bsik-dmbtr,                                 "1��2��
         dmbt7    TYPE bsik-dmbtr,                                 "2��3��
         dmbt8    TYPE bsik-dmbtr,                                 "3��4��
         dmbt9    TYPE bsik-dmbtr,                          "����1140
       END OF x_huizong.








*----------------------------------------------------------------------*
* <1.5-�����ڱ�> Global Internal Tables Declaration                    *
*----------------------------------------------------------------------*
DATA: it_t001 TYPE STANDARD TABLE OF x_t001,   "��˾���������
      wa_t001 TYPE x_t001.                     "��˾����Ĺ�����
DATA: r_bukrs  TYPE RANGE OF t001-bukrs,        "����Ȩ�޼��RANGE
      wa_bukrs LIKE LINE OF r_bukrs.            "RANGE�Ĺ�����
DATA: it_head TYPE STANDARD TABLE OF x_head,   "�����������
      wa_head TYPE x_head.                     "���ܹ�����
"      it_item type standard table of x_item,   "�����ϸ����
"      wa_item type x_item.
DATA: it_account TYPE STANDARD TABLE OF x_account, "�������ݵ��ڱ�
      wa_account TYPE x_account.                  "������
DATA: it_acc_cus TYPE STANDARD TABLE OF x_acc_cus, "�ͻ�����������
      wa_acc_cus TYPE x_acc_cus.
DATA: it_acc_ven TYPE STANDARD TABLE OF x_acc_ven, "��Ӧ����������
      wa_acc_ven TYPE x_acc_ven.                  "��Ӧ�����乤����
DATA: it_bsis    TYPE STANDARD TABLE OF x_bsis.   "BSIS�ڱ�
DATA: wa_bsis    TYPE x_bsis.
DATA: it_cus TYPE STANDARD TABLE OF x_cus,    "�ͻ�����Ļ���
      wa_cus TYPE x_cus.                      "�ͻ����ܹ�����
DATA: it_ven TYPE STANDARD TABLE OF x_ven,    "��Ӧ������Ļ���
      wa_ven TYPE x_ven.                      "��Ӧ�̻��ܹ�����
DATA: it_fieldcat TYPE slis_t_fieldcat_alv,        "��ʾ���ܵ��ֶμ�
      it_fielditm TYPE slis_t_fieldcat_alv.        "��ʾ��ϸ���ֶμ�
DATA: it_huizong TYPE STANDARD TABLE OF x_huizong,  "������ʾALV
      wa_huizong TYPE x_huizong.                    "������ʾALV
DATA: it_huizong_out TYPE TABLE OF x_huizong.       "������ʾALV
DATA: wa_huizong_out TYPE x_huizong.               "������ʾALV
DATA: it_item TYPE STANDARD TABLE OF x_head,    "����ģʽ˫������ʾ����ϸ�ڱ�
      wa_item TYPE x_head.                      ""����ģʽ˫������ʾ����ϸ������

DATA: it_sort TYPE slis_t_sortinfo_alv,
      wa_sort LIKE LINE OF it_sort.
DATA:gt_bseg LIKE TABLE OF bseg WITH HEADER LINE.
*----------------------------------------------------------------------*
* <1.6-����ȫ�ֱ���> Global Variants Declaration                       *
*----------------------------------------------------------------------*







*----------------------------------------------------------------------*
* <1.9-����ѡ����Ļ> Selection Screen                                  *
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
PARAMETERS c_1 TYPE c AS CHECKBOX MODIF ID md1.               "��ϸģʽ
SELECTION-SCREEN COMMENT 2(6) FOR FIELD s_prctr MODIF ID md1.
SELECT-OPTIONS: s_prctr FOR bseg-prctr MODIF ID md1 MATCHCODE OBJECT prct.          "��������
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS c_2 TYPE c AS CHECKBOX MODIF ID md1.               "����ģʽ
SELECTION-SCREEN COMMENT 2(6) FOR FIELD s_gsber MODIF ID md1.
SELECT-OPTIONS: s_gsber FOR bsad-gsber MATCHCODE OBJECT epic_cb_gsber_search_help MODIF ID md1. "ҵ��Χ
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b01.


SELECTION-SCREEN: BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.
PARAMETERS: r_3 TYPE c RADIOBUTTON GROUP gr2 DEFAULT 'X'.                               "����
PARAMETERS: r_4 TYPE c RADIOBUTTON GROUP gr2.                                           "���׻���
SELECTION-SCREEN: END OF BLOCK b02.

SELECTION-SCREEN: BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-b03.
PARAMETERS: r_5 TYPE c RADIOBUTTON GROUP gr3 DEFAULT 'X'.                               "�Ի�׼���ڼ���
PARAMETERS: r_6 TYPE c RADIOBUTTON GROUP gr3 .                                          "�Ը�����������
SELECTION-SCREEN: END OF BLOCK b03.

*������ģʽ�µ����в���������������ʾ�л�Ч��
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
*��������,Ҫȷ��������1������8�����ɵ������С�
  IF p_date1 LT 0 OR p_date2 LT 0
      OR p_date3 LT 0 OR p_date4 LT 0
      OR p_date5 LT 0 OR p_date6 LT 0
      OR p_date7 LT 0 OR p_date8 LT 0 .

    MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
    EXIT.
  ENDIF.
*��1���ڿ�
  IF p_date1 EQ ''.
    MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
    EXIT.
  ENDIF.
*��2���ڿ�
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
*��3���ڿ�
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
*��4���ڿ�
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
*��5���ڿ�
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
*��6���ڿ�
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
*��7���ڿ�
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
*��8���ڿ�
  IF p_date8 EQ ''.

  ELSE.
    IF p_date7 GE p_date8.
      MESSAGE e000(zyouzfirp_0009) WITH TEXT-048.
      EXIT.
    ENDIF.
  ENDIF.


*  ��鹫˾����
  PERFORM f_check_company.
*  Ȩ�޼��
  PERFORM f_check_authority.
*  ���ͻ�
  PERFORM f_check_customer.
*  ��鹩Ӧ��
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
*ȡ��
  PERFORM f_get_main.
*  ��ʾ����
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
*��û��ѡ����Ӧ�ĸ�ѡ������ն�Ӧ��ֵ
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
*��ȡ�ͻ���������Ϣ
  IF p_kunnr = c_x AND p_lifnr <> c_x.
    PERFORM f_get_customer.
  ENDIF.
*��ȡ��Ӧ�̵�������Ϣ
  IF p_lifnr = c_x AND p_kunnr <> c_x.
    PERFORM f_get_vendor.
  ENDIF.
*  ��ѡ���Ŀ��ʱ��ͬʱ��ȡ�ͻ��͹�Ӧ�̵�����
  IF ( p_hkont = c_x AND p_kunnr <> c_x AND p_lifnr <> c_x )
  OR ( p_hkont = c_x AND p_kunnr = c_x  AND p_lifnr = c_x )
  OR ( p_hkont <> c_x AND p_kunnr = c_x AND p_lifnr = c_x ).
    PERFORM f_get_customer.
    PERFORM f_get_vendor.
  ENDIF.
*  ��Ͽͻ��͹�Ӧ�̵�������Ϣ
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
*���ݿͻ���δ����
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

*���ݿͻ���������
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
*      WHERE KUNNR = IT_ACC_CUS-KUNNR"�ͻ����
*      AND   KKBER = IT_ACC_CUS-KKBER"�������Ʒ�Χ
*      AND   ZDEZT = '6'"״̬
*      AND   ZDEXYLX IN ('1','2')."��������
  ENDIF.
*  LOOP AT LT_ZTYOUZSD_0004_T.
*    CLEAR:LT_ZTYOUZSD_0004_T-KNKLI,LT_ZTYOUZSD_0004_T-ZDESQH.
*    COLLECT LT_ZTYOUZSD_0004_T INTO LT_ZTYOUZSD_0004.
*  ENDLOOP.
*end of ADD by PENGLIXUE
  "*���ձ����ų����ݣ����ѡ���˱��ң����ų������ѡ���׻��ң���������
  "  if it_acc_cus[] is not initial.
  "*����Ϊ����
  "    if r_3 eq 'X'.
  "      clear wa_t001.
  "      loop at it_t001 into wa_t001.
  "        delete it_acc_cus where bukrs eq wa_t001-bukrs and waers ne wa_t001-waers.
  "      endloop.
  "    endif.
  "*����Ϊ����
  "    if r_4 eq 'X'.

  "    endif.
  "  endif.

*��ѡ�����ģʽʱ,�ų�����
  IF r_2 EQ 'X'.
    IF it_acc_cus IS NOT INITIAL.
      IF c_1 EQ 'X' AND s_prctr[] IS NOT INITIAL.
*ȡ��Ŀ���ͺ���������
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
*���ĳ�����ݵ��������Ĳ���ѡ����Ļ���������Ĳ����У����ų�����
        CLEAR wa_acc_cus.
        LOOP AT it_acc_cus INTO wa_acc_cus.
          l_line = sy-tabix.
          READ TABLE lt_bseg INTO lw_bseg WITH KEY bukrs = wa_acc_cus-bukrs belnr = wa_acc_cus-belnr
                                                gjahr = wa_acc_cus-gjahr buzei = wa_acc_cus-buzei.

          IF sy-subrc EQ 0.

          ELSE.
*�ų�������Ҫ�����������
            DELETE it_acc_cus INDEX l_line.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.


*ȡ�ͻ����������˻���
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
*��ȡ�ͻ��˻�������
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
*  ��ȡ�ͻ��������˻���
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
*  ���ݴ�����ʶ���㸺���
      IF wa_acc_cus-shkzg = c_h.
        wa_acc_cus-wrbtr =  0 - wa_acc_cus-wrbtr. "���ҽ��
        wa_acc_cus-dmbtr =  0 - wa_acc_cus-dmbtr. "���׻��ҽ��
      ENDIF.

*���㾻�ո�������
*�Ի�׼���ڼ���:BSId/BSAd-ZFBDT   =  ��������
      IF r_5 EQ 'X'.
        wa_acc_cus-dudat = wa_acc_cus-zfbdt.
      ENDIF.
*�Ը�����������:BSId/BSAd-ZFBDT  +  ����BSIK/BSAK-ZBD1T   =  ��������
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

*��������ı�־
      PERFORM f_calcu_period USING wa_acc_cus-dudat CHANGING wa_acc_cus-perid wa_acc_cus-ducdt.
*��ȡ��λ�Һ���Ŀ��
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

  DATA: l_days TYPE vtbbewe-atage, "�������
        l_from TYPE sy-datum,     "��ʼ����
        l_to   TYPE sy-datum,     "��������
        l_int  TYPE i VALUE 1.    "��������
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
*������������������ĸ�����
*�ж���������ĸ�������������
*���������ڵ�2����������
  IF p_date2 EQ ''.
    l_stop = 'P_DATE2'.
*���������ڵ�3����������
  ELSEIF p_date3 EQ ''.
    l_stop = 'P_DATE3'.
*���������ڵ�4����������
  ELSEIF p_date4 EQ ''.
    l_stop = 'P_DATE4'.
*���������ڵ�5����������
  ELSEIF p_date5 EQ ''.
    l_stop = 'P_DATE5'.
*���������ڵ�6����������
  ELSEIF p_date6 EQ ''.
    l_stop = 'P_DATE6'.
*���������ڵ�7����������
  ELSEIF p_date7 EQ ''.
    l_stop = 'P_DATE7'.
*���������ڵ�8����������
  ELSEIF p_date8 EQ ''.
    l_stop = 'P_DATE8'.
*���������������Ϊ��
  ELSE.
    l_stop = ''.
  ENDIF.
*�ж�����
  IF p_dudat < p_budat.
    CASE l_stop.
*���ڶ��������Ϊ��ʱ
      WHEN 'P_DATE2'.
        IF l_days =< p_date1.
          p_perid = c_d001.
        ELSE.
          p_perid = c_d009.
        ENDIF.
*�������������Ϊ��ʱ
      WHEN 'P_DATE3'.
        IF l_days =< p_date1.
          p_perid = c_d001.
        ELSEIF l_days > p_date1 AND l_days <= p_date2.
          p_perid = c_d002.
        ELSE.
          p_perid = c_d009.
        ENDIF.
*�����ĸ������Ϊ��ʱ
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
*������������Ϊ��ʱ
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
*�������������Ϊ��ʱ
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
*�����߸������Ϊ��ʱ
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
*���ڰ˸������Ϊ��ʱ
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
*���ݹ�Ӧ��δ����
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

*��ȡ��Ӧ�̵�������

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
  "*���ձ����ų����ݣ����ѡ���˱��ң����ų������ѡ���׻��ң���������
  "  if it_acc_ven[] is not initial.
  "*����Ϊ����
  "    if r_3 eq 'X'.
  "      clear wa_t001.
  "      loop at it_t001 into wa_t001.
  "        delete it_acc_ven where bukrs eq wa_t001-bukrs and waers ne wa_t001-waers.
  "      endloop.
  "    endif.
  "*����Ϊ����
  "    if r_4 eq 'X'.

  "    endif.
  "  endif.

*��ѡ�����ģʽʱ,�ų�����
  IF r_2 EQ 'X'.
    IF it_acc_ven[] IS NOT INITIAL.
      IF c_1 EQ 'X' AND s_prctr[] IS NOT INITIAL.
*ȡ��Ŀ���ͺ���������
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
*���ĳ�����ݵ��������Ĳ���ѡ����Ļ���������Ĳ����У����ų�����
        CLEAR wa_acc_ven.
        LOOP AT it_acc_ven INTO wa_acc_ven.
          l_line = sy-tabix.
          READ TABLE lt_bseg INTO lw_bseg WITH KEY bukrs = wa_acc_ven-bukrs belnr = wa_acc_ven-belnr
                                                gjahr = wa_acc_ven-gjahr buzei = wa_acc_ven-buzei.

          IF sy-subrc EQ 0.

          ELSE.
*�ų�������Ҫ�����������
            DELETE it_acc_ven INDEX l_line.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.


*��ȡ��Ӧ���ݹ�������
  PERFORM f_get_bsis.
*ȡ��Ӧ�̵��������˻���
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

*��ȡ��Ӧ���˻�������
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

* �����ʺ�
      CLEAR:ls_lfbk.
      READ TABLE lt_lfbk INTO ls_lfbk WITH   KEY lifnr = wa_acc_ven-lifnr.
      wa_acc_ven-bankl =  ls_lfbk-bankl.

* ��������
      CLEAR:ls_bnka.
      READ TABLE lt_bnka INTO ls_bnka WITH   KEY bankl = wa_acc_ven-bankl.
      wa_acc_ven-banka = ls_bnka-banka.
* �����ʺ�
      wa_acc_ven-banno = ls_lfbk-bankn && ls_lfbk-bkref.
      CLEAR lwa_lifnr.
*  ��ȡ��Ӧ���������˻�����Ϣ
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
*  ���ݴ�����ʶ���㸺���
      IF wa_acc_ven-shkzg = c_h.
        wa_acc_ven-wrbtr =  0 - wa_acc_ven-wrbtr.
        wa_acc_ven-dmbtr = 0 - wa_acc_ven-dmbtr.
      ENDIF.
      "*  ���㾻�ո�������
      IF wa_acc_ven-fbsis = c_x.
        wa_acc_ven-dudat = wa_acc_ven-budat.
      ELSE.
*���㾻�ո�������
*�Ի�׼���ڼ���:BSId/BSAd-ZFBDT   =  ��������
        IF r_5 EQ 'X'.
          wa_acc_ven-dudat = wa_acc_ven-zfbdt.
        ENDIF.
*�Ը�����������:BSId/BSAd-ZFBDT  +  ����BSIK/BSAK-ZBD1T   =  ��������
        IF r_6 EQ 'X'.
          wa_acc_ven-dudat = wa_acc_ven-zfbdt + wa_acc_ven-zbd1t.
        ENDIF.
      ENDIF.

*��������ı�־
      PERFORM f_calcu_period USING wa_acc_ven-dudat CHANGING wa_acc_ven-perid wa_acc_ven-ducdt.
*��ȡ��λ�Һ���Ŀ��
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


*��ȡӦ���ݹ���δ��������
  CLEAR it_bsis[].
*  ���ӿ�Ŀ
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
*    DATA: gt_bsis TYPE TABLE OF x_bsis WITH HEADER LINE.   "BSIS�ڱ�
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


*��ȡӦ���ݹ�������������
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






  "*���ձ����ų�����
  "  if it_bsis[] is not initial.
  "*����Ϊ����
  "    if r_3 eq 'X'.
  "      clear wa_t001.
  "      loop at it_t001 into wa_t001.
  "        delete it_bsis where bukrs eq wa_t001-bukrs and waers ne wa_t001-waers.
  "      endloop.
  "    endif.
  "*����Ϊ����
  "    if r_4 eq 'X'.

  "    endif.
  "  endif.

*��ѡ�����ģʽʱ,�ų�����
  IF r_2 EQ 'X'.
    IF it_bsis[] IS NOT INITIAL.
      IF c_1 EQ 'X' AND s_prctr[] IS NOT INITIAL.
*ȡ��Ŀ���ͺ���������
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
*�ų�������Ҫ�����������
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


*��Ӧ���ݹ�������ת�Ƶ�BSIK��
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

*ȡ��ƿ�Ŀ����
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
*��˾��������
    SELECT bukrs
           butxt
      INTO TABLE lt_bukrs
      FROM t001
      FOR ALL ENTRIES IN it_acc_cus
     WHERE bukrs EQ it_acc_cus-bukrs.
    SORT lt_bukrs.
    DELETE ADJACENT DUPLICATES FROM lt_bukrs.

*ȡ��Ŀ���ͺ���������
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
*ȡ������������
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
*ȡҵ��Χ����
    SELECT *
      INTO TABLE lt_tgsbt
      FROM tgsbt
       FOR ALL ENTRIES IN it_acc_cus
     WHERE gsber EQ it_acc_cus-gsber
       AND spras = 1.
*ȡ��������
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
*ȡ�������
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
      wa_head-kkber = wa_acc_cus-kkber.   "�Ŵ����Ʒ�Χ
      IF wa_head-kkber  IS NOT INITIAL.
        SELECT SINGLE kkbtx INTO wa_head-kkbtx
          FROM t014t
          WHERE kkber = wa_head-kkber AND spras  = sy-langu.
      ENDIF.
*end of ADD by PENGLIXUE
      wa_head-ktokd = wa_acc_cus-ktokd.   "�ͻ��˻���
      wa_head-kzhms = wa_acc_cus-kzhms.   "�ͻ��˻�������
      wa_head-kunnr = wa_acc_cus-kunnr.   "�ͻ�����
      wa_head-namec = wa_acc_cus-namec.   "�ͻ�����
      wa_head-ksbm  = wa_acc_cus-kunnr.   "���̱���
      wa_head-ksms  = wa_acc_cus-namec.   "���̱�������
*��������
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

*��Ŀ����
      READ TABLE lt_bseg INTO lw_bseg WITH KEY bukrs = wa_acc_cus-bukrs belnr = wa_acc_cus-belnr
                                                gjahr = wa_acc_cus-gjahr buzei = wa_acc_cus-buzei.

      IF sy-subrc EQ 0.
        wa_head-koart = lw_bseg-koart.
*��������
        wa_head-prctr = lw_bseg-prctr.
      ENDIF.
*������������
      IF wa_head-prctr NE ''.
        READ TABLE lt_prctr INTO lw_prctr WITH KEY prctr = wa_head-prctr.
        IF sy-subrc EQ 0.
          wa_head-prctt = lw_prctr-ltext.
        ENDIF.
      ENDIF.


*���˿�Ŀ
      wa_head-hkont = wa_acc_cus-hkont.
*��ȡ��ƿ�Ŀ����
      READ TABLE lit_skat INTO lwa_skat WITH KEY ktopl = wa_acc_cus-ktopl saknr = wa_acc_cus-hkont BINARY SEARCH.
      IF sy-subrc = 0.
        wa_head-txt50 = lwa_skat-txt50.
      ENDIF.

*һ����Ŀ
      wa_head-klev1 = wa_acc_cus-hkont+0(4).
*һ����Ŀ����
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
*��˾����
      wa_head-bukrs = wa_acc_cus-bukrs.
*��˾����
      READ TABLE lt_bukrs INTO lw_bukrs WITH KEY bukrs = wa_acc_cus-bukrs.
      IF sy-subrc EQ 0.
        wa_head-butxt = lw_bukrs-butxt.
      ENDIF.
*ҵ��Χ
      wa_head-gsber = wa_acc_cus-gsber.
*ҵ��Χ����
      READ TABLE lt_tgsbt INTO lw_tgsbt WITH KEY gsber = wa_acc_cus-gsber.
      IF sy-subrc EQ 0.
        wa_head-gtext = lw_tgsbt-gtext.
      ENDIF.
*���ƾ֤
      wa_head-belnr = wa_acc_cus-belnr.
*���ƾ֤��
      wa_head-buzei = wa_acc_cus-buzei.
*������
      wa_head-gjahr = wa_acc_cus-gjahr.
*�������
      READ TABLE lt_bkpf INTO lw_bkpf WITH KEY bukrs = wa_acc_cus-bukrs belnr = wa_acc_cus-belnr
                                                                          gjahr = wa_acc_cus-gjahr.

      IF sy-subrc EQ 0.
        wa_head-tcode = lw_bkpf-tcode.
*ƾ֤����
        wa_head-blart = lw_bkpf-blart.
*����ƾ֤���
        wa_head-xblnr_alt = lw_bkpf-xblnr_alt.
      ENDIF.
*������
      wa_head-zuonr = wa_acc_cus-zuonr.
*��Ŀ�ı�
      wa_head-sgtxt = wa_acc_cus-sgtxt.
*ƾ֤����
      wa_head-bldat = wa_acc_cus-bldat.
*��������
      wa_head-budat = wa_acc_cus-budat.
*������
      wa_head-dudat = wa_acc_cus-dudat.
*����ƾ֤��
      wa_head-augbl = wa_acc_cus-augbl.
*��������
      wa_head-augdt = wa_acc_cus-augdt.
*��������
      wa_head-xtype = wa_acc_cus-xtype.
*�������
      IF r_3 EQ 'X'.
        wa_head-ksye = wa_acc_cus-dmbtr.
      ENDIF.
      IF r_4 EQ 'X'.
        wa_head-ksye = wa_acc_cus-wrbtr.
      ENDIF.
      CASE wa_acc_cus-perid.
*δ�������
        WHEN c_d000.
          IF r_3 EQ 'X'.
            wa_head-wdqje = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-wdqje = wa_acc_cus-wrbtr.
          ENDIF.
*30������
        WHEN c_d001.
          IF r_3 EQ 'X'.
            wa_head-dmbt1 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt1 = wa_acc_cus-wrbtr.
          ENDIF.
*31��60
        WHEN c_d002.
          IF r_3 EQ 'X'.
            wa_head-dmbt2 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt2 = wa_acc_cus-wrbtr.
          ENDIF.
*61��90
        WHEN c_d003.
          IF r_3 EQ 'X'.
            wa_head-dmbt3 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt3 = wa_acc_cus-wrbtr.
          ENDIF.
*91��120
        WHEN c_d004.
          IF r_3 EQ 'X'.
            wa_head-dmbt4 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt4 = wa_acc_cus-wrbtr.
          ENDIF.
*120��180
        WHEN c_d005.
          IF r_3 EQ 'X'.
            wa_head-dmbt5 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt5 = wa_acc_cus-wrbtr.
          ENDIF.
*180��360
        WHEN c_d006.
          IF r_3 EQ 'X'.
            wa_head-dmbt6 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt6 = wa_acc_cus-wrbtr.
          ENDIF.
*360��720
        WHEN c_d007.
          IF r_3 EQ 'X'.
            wa_head-dmbt7 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt7 = wa_acc_cus-wrbtr.
          ENDIF.
*720��1440
        WHEN c_d008.
          IF r_3 EQ 'X'.
            wa_head-dmbt8 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt8 = wa_acc_cus-wrbtr.
          ENDIF.
*����1440
        WHEN c_d009.
          IF r_3 EQ 'X'.
            wa_head-dmbt9 = wa_acc_cus-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt9 = wa_acc_cus-wrbtr.
          ENDIF.
      ENDCASE.

*������
      wa_head-zlje = wa_head-ksye - wa_head-dmbt0.
*����
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
*  ��֯��Ҫ��ʾ���ֶ�
  PERFORM f_fieldcat_init.
*  ��ʾ����ALV
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

  CONCATENATE:'����С�ڵ���'p_date1 '��' INTO lv_dmbt1.
  CONCATENATE:'����'p_dy1 '�쵽' p_date2 '��' INTO lv_dmbt2.
  CONCATENATE:'����'p_dy2 '�쵽' p_date3 '��' INTO lv_dmbt3.
  CONCATENATE:'����'p_dy3 '�쵽' p_date4 '��' INTO lv_dmbt4.
  CONCATENATE:'����'p_dy4 '�쵽' p_date5 '��' INTO lv_dmbt5.
  CONCATENATE:'����'p_dy5 '�쵽' p_date6 '��' INTO lv_dmbt6.
  CONCATENATE:'����'p_dy6 '�쵽' p_date7 '��' INTO lv_dmbt7.
  CONCATENATE:'����'p_dy7 '�쵽' p_date8 '��' INTO lv_dmbt8.


  "  concatenate:'������ڵ���' p_dy8 into lv_dmbt9.

*��ϸģʽ��ʾ
  IF r_1 EQ 'X'.
    CLEAR it_fieldcat.
    PERFORM f_fieldcat_set TABLES it_fieldcat
    USING:  'KSBM'  TEXT-051 'X' '' '',  "���̱���
            'KSMS'  TEXT-052 'X' '' '',  "��������
            'BUKRS' TEXT-003 'X' '' '',  "��˾����
            'BUTXT' TEXT-057 'X' '' '',  "��˾��������
            'GJAHR' TEXT-063 'X' '' '',  "������
            'BELNR' TEXT-061 'X' 'X' '',  "���ƾ֤
            'BUZEI' TEXT-062 'X' 'X' '',  "���ƾ֤��
            'XBLNR_ALT' TEXT-082 '' '' '', "����ƾ֤���
            'GUOJ'  TEXT-055 '' '' '',      "����
            'KTOKD' TEXT-009 '' '' '',  "�ͻ��˻���
            'KZHMS' TEXT-049 '' '' '',  "�ͻ��˻�������
            'KTOKK' TEXT-008 '' '' '',  "��Ӧ���˻���
            'GZHMS' TEXT-050 '' '' '',   "��Ӧ���˻�������
            'KUNNR' TEXT-006 '' '' '',   "�ͻ�����
            'NAMEC' TEXT-007 '' '' '',  "�ͻ�����
            'LIFNR' TEXT-004 '' '' '',   "��Ӧ�̱���
            'NAMEV' TEXT-005 '' '' '',   "��Ӧ������
            'KOART' TEXT-053 '' '' '',  "��Ŀ����
            'KLEV1' TEXT-039 '' '' '',      "һ����Ŀ
            'YJKMT' TEXT-054 '' '' '',  "һ����Ŀ����
            'HKONT' TEXT-010 '' '' '',  "�����Ŀ
*ADD by PENGLIXUE at 20180224 BPMNO: ERP-YW201802020036
            'KKBER' TEXT-083 '' '' '',  "�Ŵ����Ʒ�Χ
            'KKBTX' TEXT-084 '' '' '',  "�Ŵ���Χ����
*end of ADD by PENGLIXUE
            'TXT50' TEXT-011 '' '' '', "��Ŀ����
            'SGL'   TEXT-056 '' '' '',      "SGL
            'PRCTR' TEXT-058 '' '' '', "��������
            'PRCTT' TEXT-059 '' '' '', "������������
            'GSBER' TEXT-060 '' '' '', "ҵ��Χ
            'GTEXT' TEXT-080 '' '' '', "ҵ��Χ����
            'BLART' TEXT-064 '' '' '',  "ƾ֤����
            'TCODE' TEXT-065 '' '' '', "�������
            'ZUONR' TEXT-066 '' '' '',  "����
            'SGTXT' TEXT-067 '' '' '',  "��Ŀ�ı�
            'BLDAT' TEXT-068 '' '' '', "ƾ֤����
            'BUDAT' TEXT-069 '' '' '',  "��������
            'DUDAT' TEXT-070 '' '' '',  "������
            'AUGBL' TEXT-071 '' '' '',  "����ƾ֤��
            'AUGDT' TEXT-072 '' '' '',  "��������
            'KSYE ' TEXT-073 '' '' 'X', "�������
            'WDQJE' TEXT-074 '' '' 'X',  "δ�������
            'ZLJE'  TEXT-075 '' '' 'X',  "������
            'WAERS' TEXT-076 '' '' '',  "����
            'ZLJE_BB'  TEXT-085 '' '' 'X',  "������
            'WAERS_BB' TEXT-086 '' '' '',  "����
            'XTYPE' TEXT-077 '' '' ''.       "��������

  ENDIF.
*����ģʽ��ʾ
  IF r_2 EQ 'X'.
    CLEAR it_fieldcat.
*ֻ��ѡ��������
    IF c_1 EQ 'X' AND c_2 NE 'X'.
      PERFORM f_fieldcat_set TABLES it_fieldcat
        USING: 'KSBM'  TEXT-051 'X' 'X' '',  "���̱���
            'KSMS'  TEXT-052 'X' '' '',  "��������
            'BUKRS' TEXT-003 'X' '' '',  "��˾����
            'BUTXT' TEXT-057 'X' '' '',  "��˾��������
            'KLEV1' TEXT-039 'X' '' '',  "һ����Ŀ
            'YJKMT' TEXT-054 'X' '' '',  "һ����Ŀ����
            'LIFNR' TEXT-004 '' '' '',   "��Ӧ�̱���
            'NAMEV' TEXT-005 '' '' '',   "��Ӧ������
            'KUNNR' TEXT-006 '' '' '',   "�ͻ�����
            'NAMEC' TEXT-007 '' '' '',  "�ͻ�����
            'GUOJ'  TEXT-055 '' '' '',  "����
            'KTOKK' TEXT-008 '' '' '',  "��Ӧ���˻���
            'GZHMS' TEXT-050 '' '' '',   "��Ӧ���˻�������
            'KTOKD' TEXT-009 '' '' '',  "�ͻ��˻���
            'KZHMS' TEXT-049 '' '' '',  "�ͻ��˻�������
            'KOART' TEXT-053 '' '' '',  "��Ŀ����
            'HKONT' TEXT-010 '' '' '',  "�����Ŀ
            'TXT50' TEXT-011 '' '' '', "��Ŀ����
            'PRCTR' TEXT-058 '' '' '', "��������
            'PRCTT' TEXT-059 '' '' '', "������������
            'ZLJE'  TEXT-075 '' '' 'X',  "�ܼƣ���ʱ������
            'WAERS' TEXT-076 '' '' '',  "����
            'ZLJE_BB'  TEXT-085 '' '' 'X',  "�ܼƣ���ʱ������
            'WAERS_BB' TEXT-086 '' '' '',  "����
            'WDQJE' TEXT-074 '' '' 'X'.  "δ�������
      "            'XTYPE' text-077 '' '', "��������

*ֻ��ѡҵ��Χ
    ELSEIF c_1 NE 'X' AND c_2 EQ 'X'.
      PERFORM f_fieldcat_set TABLES it_fieldcat
        USING:  'KSBM'  TEXT-051 'X' 'X' '',  "���̱���
            'KSMS'  TEXT-052 'X' '' '',  "��������
            'BUKRS' TEXT-003 'X' '' '',  "��˾����
            'BUTXT' TEXT-057 'X' '' '',  "��˾��������
            'KLEV1' TEXT-039 'X' '' '',  "һ����Ŀ
            'YJKMT' TEXT-054 'X' '' '',  "һ����Ŀ����
            'LIFNR' TEXT-004 '' '' '',   "��Ӧ�̱���
            'NAMEV' TEXT-005 '' '' '',   "��Ӧ������
            'KUNNR' TEXT-006 '' '' '',   "�ͻ�����
            'NAMEC' TEXT-007 '' '' '',  "�ͻ�����
            'GUOJ'  TEXT-055 '' '' '',  "����
            'KTOKK' TEXT-008 '' '' '',  "��Ӧ���˻���
            'GZHMS' TEXT-050 '' '' '',   "��Ӧ���˻�������
            'KTOKD' TEXT-009 '' '' '',  "�ͻ��˻���
            'KZHMS' TEXT-049 '' '' '',  "�ͻ��˻�������
            'KOART' TEXT-053 '' '' '',  "��Ŀ����
            'HKONT' TEXT-010 '' '' '',  "�����Ŀ
            'TXT50' TEXT-011 '' '' '', "��Ŀ����
            'GSBER' TEXT-060 '' '' '', "ҵ��Χ
            'GTEXT' TEXT-080 '' '' '', "ҵ��Χ����
            'ZLJE'  TEXT-075 '' '' 'X',  "�ܼƣ���ʱ������
            'WAERS' TEXT-076 '' '' '',  "����
             'ZLJE_BB'  TEXT-085 '' '' 'X',  "������
            'WAERS_BB' TEXT-086 '' '' '',  "����
            'WDQJE' TEXT-074 '' '' 'X'.  "δ�������
      "            'XTYPE' text-077 '' '', "��������

*��ѡ�������ĺ�ҵ��Χ
    ELSEIF c_1 EQ 'X' AND c_2 EQ 'X'.
      PERFORM f_fieldcat_set TABLES it_fieldcat
        USING:  'KSBM'  TEXT-051 'X' 'X' '',  "���̱���
            'KSMS'  TEXT-052 'X' '' '',  "��������
            'BUKRS' TEXT-003 'X' '' '',  "��˾����
            'BUTXT' TEXT-057 'X' '' '',  "��˾��������
            'KLEV1' TEXT-039 'X' '' '',  "һ����Ŀ
            'YJKMT' TEXT-054 'X' '' '',  "һ����Ŀ����
            'LIFNR' TEXT-004 '' '' '',   "��Ӧ�̱���
            'NAMEV' TEXT-005 '' '' '',   "��Ӧ������
            'KUNNR' TEXT-006 '' '' '',   "�ͻ�����
            'NAMEC' TEXT-007 '' '' '',  "�ͻ�����
            'GUOJ'  TEXT-055 '' '' '',  "����
            'KTOKK' TEXT-008 '' '' '',  "��Ӧ���˻���
            'GZHMS' TEXT-050 '' '' '',   "��Ӧ���˻�������
            'KTOKD' TEXT-009 '' '' '',  "�ͻ��˻���
            'KZHMS' TEXT-049 '' '' '',  "�ͻ��˻�������
            'KOART' TEXT-053 '' '' '',  "��Ŀ����
            'HKONT' TEXT-010 '' '' '',  "�����Ŀ
            'TXT50' TEXT-011 '' '' '', "��Ŀ����
            'PRCTR' TEXT-058 '' '' '', "��������
            'PRCTT' TEXT-059 '' '' '', "������������
            'GSBER' TEXT-060 '' '' '', "ҵ��Χ
            'GTEXT' TEXT-080 '' '' '', "ҵ��Χ����
            'ZLJE'  TEXT-075 '' '' 'X',  "�ܼƣ���ʱ������
            'WAERS' TEXT-076 '' '' '',  "����
             'ZLJE_BB'  TEXT-085 '' '' 'X',  "������
            'WAERS_BB' TEXT-086 '' '' '',  "����
            'WDQJE' TEXT-074 '' '' 'X'.  "δ�������
      "            'XTYPE' text-077 '' '', "��������
*������ѡ�������ĺ�ҵ��Χ
    ELSEIF c_1 NE 'X' AND c_2 NE 'X'.
      PERFORM f_fieldcat_set TABLES it_fieldcat
        USING: 'KSBM'  TEXT-051 'X' 'X' '',  "���̱���
            'KSMS'  TEXT-052 'X' '' '',  "��������
            'BUKRS' TEXT-003 'X' '' '',  "��˾����
            'BUTXT' TEXT-057 'X' '' '',  "��˾��������
            'KLEV1' TEXT-039 'X' '' '',  "һ����Ŀ
            'YJKMT' TEXT-054 'X' '' '',  "һ����Ŀ����
            'LIFNR' TEXT-004 '' '' '',   "��Ӧ�̱���
            'NAMEV' TEXT-005 '' '' '',   "��Ӧ������
            'KUNNR' TEXT-006 '' '' '',   "�ͻ�����
            'NAMEC' TEXT-007 '' '' '',  "�ͻ�����
            'GUOJ'  TEXT-055 '' '' '',  "����
            'KTOKK' TEXT-008 '' '' '',  "��Ӧ���˻���
            'GZHMS' TEXT-050 '' '' '',   "��Ӧ���˻�������
            'KTOKD' TEXT-009 '' '' '',  "�ͻ��˻���
            'KZHMS' TEXT-049 '' '' '',  "�ͻ��˻�������
            'KOART' TEXT-053 '' '' '',  "��Ŀ����
            'HKONT' TEXT-010 '' '' '',  "�����Ŀ
            'TXT50' TEXT-011 '' '' '', "��Ŀ����
            'ZLJE'  TEXT-075 '' '' 'X',  "�ܼƣ���ʱ������
            'WAERS' TEXT-076 '' '' '',  "����
             'ZLJE_BB'  TEXT-085 '' '' 'X',  "������
            'WAERS_BB' TEXT-086 '' '' '',  "����
            'WDQJE' TEXT-074 '' '' 'X'.  "δ�������
      "            'XTYPE' text-077 '' '', "��������

    ENDIF.
  ENDIF.

  IF p_date1 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT1' lv_dmbt1 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'������ڵ���' p_dy1 INTO lv_dmbt9.
  ENDIF.
  IF p_date2 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT2' lv_dmbt2 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'������ڵ���' p_dy2 INTO lv_dmbt9.
  ENDIF.
  IF p_date3 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT3' lv_dmbt3 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'������ڵ���' p_dy3 INTO lv_dmbt9.
  ENDIF.
  IF p_date4 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT4' lv_dmbt4 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'������ڵ���' p_dy4 INTO lv_dmbt9.
  ENDIF.
  IF p_date5 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT5' lv_dmbt5 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'������ڵ���' p_dy5 INTO lv_dmbt9.
  ENDIF.
  IF p_date6 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT6' lv_dmbt6 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'������ڵ���' p_dy6 INTO lv_dmbt9.
  ENDIF.
  IF p_date7 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT7' lv_dmbt7 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'������ڵ���' p_dy7 INTO lv_dmbt9.
  ENDIF.
  IF p_date8 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT8' lv_dmbt8 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'������ڵ���' p_dy8 INTO lv_dmbt9.
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
  DATA: lwa_layout    TYPE slis_layout_alv, "ALV����Ŀ���
        l_variant     TYPE disvariant,     "ALV��ʽ
        l_repid       TYPE sy-repid,       "��������
        lwa_excluding TYPE slis_t_extab.   "�ų��Ĳ˵���ť

  lwa_layout-colwidth_optimize = c_x.
  lwa_layout-zebra             = c_x.
  lwa_layout-box_fieldname     = c_box.
  "  lwa_layout-f2code            = c_item.
  l_repid                      = sy-repid.

**���׻��� ���ӱ���
*if r_4 = 'X'.
*  IF it_head[] IS NOT INITIAL.
*
*  ENDIF.
*
*
*ENDIF.



*��ϸģʽ��ʾ
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
*����ģʽ��ʾ
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


  DATA:lwa_fieldcat  TYPE slis_fieldcat_alv."�ֶμ�

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

  "*  ֻѡ��Ӧ��ʱ����˫����ϸ��¼�ǹ�Ӧ�̼�¼ʱ������ʾ�ͻ���ص���Ϣ
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
  "*  ֻѡ��ͻ�ʱ����˫����ϸ��¼�ǿͻ���¼ʱ������ʾ��Ӧ����ص���Ϣ
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
*    ���û������ݵ��û�����
*----------------------------------------------------------------------*
*      -->  r_ucomm       �û�˫���������ݵĹ�����
*      <--  rs_selfield   ��˫���е���Ϣ
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

*ȡ��ƿ�Ŀ����
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
*��˾��������
    SELECT bukrs
           butxt
      INTO TABLE lt_bukrs
      FROM t001
      FOR ALL ENTRIES IN it_acc_ven
     WHERE bukrs EQ it_acc_ven-bukrs.
    SORT lt_bukrs.
    DELETE ADJACENT DUPLICATES FROM lt_bukrs.
*ȡ��Ŀ���ͺ���������
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
*ȡ������������
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
*ȡҵ��Χ����
    SELECT *
      INTO TABLE lt_tgsbt
      FROM tgsbt
       FOR ALL ENTRIES IN it_acc_ven
     WHERE gsber EQ it_acc_ven-gsber
       AND spras = 1.
*ȡ��������
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
*ȡ�������
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
      wa_head-gzhms = wa_acc_ven-gzhms.   "��Ӧ���˻���
      wa_head-ktokk = wa_acc_ven-ktokk.   "��Ӧ���˻�������
      wa_head-lifnr = wa_acc_ven-lifnr.   "��Ӧ�̱���
      wa_head-namev = wa_acc_ven-namev.   "��Ӧ������
      wa_head-ksbm  = wa_acc_ven-lifnr.   "���̱���
      wa_head-ksms  = wa_acc_ven-namev.   "���̱�������
*��������
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

*��Ŀ����
      READ TABLE lt_bseg INTO lw_bseg WITH KEY bukrs = wa_acc_ven-bukrs belnr = wa_acc_ven-belnr
                                                gjahr = wa_acc_ven-gjahr buzei = wa_acc_ven-buzei.

      IF sy-subrc EQ 0.
        wa_head-koart = lw_bseg-koart.
*��������
        wa_head-prctr = lw_bseg-prctr.
      ENDIF.
*������������
      IF wa_head-prctr NE ''.
        READ TABLE lt_prctr INTO lw_prctr WITH KEY prctr = wa_head-prctr.
        IF sy-subrc EQ 0.
          wa_head-prctt = lw_prctr-ltext.
        ENDIF.
      ENDIF.
*���˿�Ŀ
      wa_head-hkont = wa_acc_ven-hkont.
*��ȡ��ƿ�Ŀ����
      READ TABLE lit_skat INTO lwa_skat WITH KEY ktopl = wa_acc_ven-ktopl saknr = wa_acc_ven-hkont BINARY SEARCH.
      IF sy-subrc = 0.
        wa_head-txt50 = lwa_skat-txt50.
      ENDIF.

*һ����Ŀ
      wa_head-klev1 = wa_acc_ven-hkont+0(4).
*һ����Ŀ����
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
*��˾����
      wa_head-bukrs = wa_acc_ven-bukrs.
*��˾����
      READ TABLE lt_bukrs INTO lw_bukrs WITH KEY bukrs = wa_acc_ven-bukrs.
      IF sy-subrc EQ 0.
        wa_head-butxt = lw_bukrs-butxt.
      ENDIF.
*ҵ��Χ
      wa_head-gsber = wa_acc_ven-gsber.
*ҵ��Χ����
      READ TABLE lt_tgsbt INTO lw_tgsbt WITH KEY gsber = wa_acc_ven-gsber.
      IF sy-subrc EQ 0.
        wa_head-gtext = lw_tgsbt-gtext.
      ENDIF.
*���ƾ֤
      wa_head-belnr = wa_acc_ven-belnr.
*���ƾ֤��
      wa_head-buzei = wa_acc_ven-buzei.
*������
      wa_head-gjahr = wa_acc_ven-gjahr.
*�������
      READ TABLE lt_bkpf INTO lw_bkpf WITH KEY bukrs = wa_acc_ven-bukrs belnr = wa_acc_ven-belnr
                                                                          gjahr = wa_acc_ven-gjahr.

      IF sy-subrc EQ 0.
        wa_head-tcode = lw_bkpf-tcode.
*ƾ֤����
        wa_head-blart = lw_bkpf-blart.
*����ƾ֤���
        wa_head-xblnr_alt = lw_bkpf-xblnr_alt.
      ENDIF.
*������
      wa_head-zuonr = wa_acc_ven-zuonr.
*��Ŀ�ı�
      wa_head-sgtxt = wa_acc_ven-sgtxt.
*ƾ֤����
      wa_head-bldat = wa_acc_ven-bldat.
*��������
      wa_head-budat = wa_acc_ven-budat.
*������
      wa_head-dudat = wa_acc_ven-dudat.
*����ƾ֤��
      wa_head-augbl = wa_acc_ven-augbl.
*��������
      wa_head-augdt = wa_acc_ven-augdt.
*��������
      wa_head-xtype = wa_acc_ven-xtype.
*�������
      IF r_3 EQ 'X'.
        wa_head-ksye = wa_acc_ven-dmbtr.
      ENDIF.
      IF r_4 EQ 'X'.
        wa_head-ksye = wa_acc_ven-wrbtr.
      ENDIF.
      CASE wa_acc_ven-perid.
*δ�������
        WHEN c_d000.
          IF r_3 EQ 'X'.
            wa_head-wdqje = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-wdqje = wa_acc_ven-wrbtr.
          ENDIF.
*30������
        WHEN c_d001.
          IF r_3 EQ 'X'.
            wa_head-dmbt1 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt1 = wa_acc_ven-wrbtr.
          ENDIF.
*31��60
        WHEN c_d002.
          IF r_3 EQ 'X'.
            wa_head-dmbt2 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt2 = wa_acc_ven-wrbtr.
          ENDIF.
*61��90
        WHEN c_d003.
          IF r_3 EQ 'X'.
            wa_head-dmbt3 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt3 = wa_acc_ven-wrbtr.
          ENDIF.
*91��120
        WHEN c_d004.
          IF r_3 EQ 'X'.
            wa_head-dmbt4 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt4 = wa_acc_ven-wrbtr.
          ENDIF.
*120��180
        WHEN c_d005.
          IF r_3 EQ 'X'.
            wa_head-dmbt5 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt5 = wa_acc_ven-wrbtr.
          ENDIF.
*180��360
        WHEN c_d006.
          IF r_3 EQ 'X'.
            wa_head-dmbt6 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt6 = wa_acc_ven-wrbtr.
          ENDIF.
*360��720
        WHEN c_d007.
          IF r_3 EQ 'X'.
            wa_head-dmbt7 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt7 = wa_acc_ven-wrbtr.
          ENDIF.
*720��1440
        WHEN c_d008.
          IF r_3 EQ 'X'.
            wa_head-dmbt8 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt8 = wa_acc_ven-wrbtr.
          ENDIF.
*����1440
        WHEN c_d009.
          IF r_3 EQ 'X'.
            wa_head-dmbt9 = wa_acc_ven-dmbtr.
          ENDIF.
          IF r_4 EQ 'X'.
            wa_head-dmbt9 = wa_acc_ven-wrbtr.
          ENDIF.

      ENDCASE.
*������
      wa_head-zlje = wa_head-ksye - wa_head-dmbt0.
*����
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
*          wa_head-waers_bb = gt_bseg-h_hwaer.  "����д�� cny  BY HAND 20191114
          wa_head-waers_bb = 'CNY'.
        ENDIF.
        MODIFY it_head FROM wa_head.
      ENDLOOP.
    ENDIF.

  ENDIF.

*����ģʽ��ʾ
  IF r_2 EQ 'X'.
    CLEAR wa_huizong.
    CLEAR it_huizong.
*�Ի����ڱ�����ۺ����ݸ�ֵ
    LOOP AT it_head INTO wa_head.
      MOVE-CORRESPONDING wa_head TO wa_huizong.
      APPEND wa_huizong TO it_huizong.
      CLEAR wa_huizong.
      CLEAR wa_head.
    ENDLOOP.
*-----------------------------------------------------------------------
*ֻ��ѡ��������
    IF c_1 EQ 'X' AND c_2 NE 'X'.
      LOOP AT it_huizong INTO wa_huizong.
        lw_huizong_lrzx-bukrs  = wa_huizong-bukrs.  "��˾����
        lw_huizong_lrzx-butxt  = wa_huizong-butxt.  "��˾��������
        lw_huizong_lrzx-lifnr  = wa_huizong-lifnr.  "��Ӧ�̱���
        lw_huizong_lrzx-namev  = wa_huizong-namev.  "��Ӧ������
        lw_huizong_lrzx-kunnr  = wa_huizong-kunnr.  "�ͻ�����
        lw_huizong_lrzx-namec  = wa_huizong-namec.  "�ͻ�����
        lw_huizong_lrzx-ksbm   = wa_huizong-ksbm.   "���̱���
        lw_huizong_lrzx-ksms   = wa_huizong-ksms.   "��������
        lw_huizong_lrzx-guoj   = wa_huizong-guoj.   "����
        lw_huizong_lrzx-ktokk  = wa_huizong-ktokk.  "��Ӧ���˻���
        lw_huizong_lrzx-gzhms  = wa_huizong-gzhms.  "��Ӧ���˻�������
        lw_huizong_lrzx-ktokd  = wa_huizong-ktokd.  "�ͻ��˻���
        lw_huizong_lrzx-kzhms  = wa_huizong-kzhms.  "�ͻ��˻�������
        lw_huizong_lrzx-koart  = wa_huizong-koart.  "��Ŀ����
        lw_huizong_lrzx-klev1  = wa_huizong-klev1.  "һ����Ŀ
        lw_huizong_lrzx-yjkmt  = wa_huizong-yjkmt.  "һ����Ŀ����
        lw_huizong_lrzx-hkont  = wa_huizong-hkont.  "�����Ŀ
        lw_huizong_lrzx-txt50  = wa_huizong-txt50.  "��Ŀ����
        lw_huizong_lrzx-prctr  = wa_huizong-prctr.  "��������
        lw_huizong_lrzx-prctt  = wa_huizong-prctt.  "������������
        lw_huizong_lrzx-waers  = wa_huizong-waers.  "����
        lw_huizong_lrzx-waers_bb  = wa_huizong-waers_bb.  "����

        APPEND lw_huizong_lrzx TO lt_huizong_lrzx.
        CLEAR lw_huizong_lrzx.
        CLEAR wa_huizong.
      ENDLOOP.
*����ȥ��
      SORT lt_huizong_lrzx.
      DELETE ADJACENT DUPLICATES FROM lt_huizong_lrzx.

*�����������Ľ��л���
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
*Ϊ����ڱ��������
      IF lt_huizong_out IS NOT INITIAL.
        CLEAR it_huizong_out.
        APPEND LINES OF lt_huizong_out TO it_huizong_out.
      ENDIF.
*---------------------------------------------------------------------------

*ֻ��ѡҵ��Χ
    ELSEIF c_1 NE 'X' AND c_2 EQ 'X'.
      LOOP AT it_huizong INTO wa_huizong.
        lw_huizong_ywfw-bukrs  = wa_huizong-bukrs.  "��˾����
        lw_huizong_ywfw-butxt  = wa_huizong-butxt.  "��˾��������
        lw_huizong_ywfw-lifnr  = wa_huizong-lifnr.  "��Ӧ�̱���
        lw_huizong_ywfw-namev  = wa_huizong-namev.  "��Ӧ������
        lw_huizong_ywfw-kunnr  = wa_huizong-kunnr.  "�ͻ�����
        lw_huizong_ywfw-namec  = wa_huizong-namec.  "�ͻ�����
        lw_huizong_ywfw-ksbm   = wa_huizong-ksbm.   "���̱���
        lw_huizong_ywfw-ksms   = wa_huizong-ksms.   "��������
        lw_huizong_ywfw-guoj   = wa_huizong-guoj.   "����
        lw_huizong_ywfw-ktokk  = wa_huizong-ktokk.  "��Ӧ���˻���
        lw_huizong_ywfw-gzhms  = wa_huizong-gzhms.  "��Ӧ���˻�������
        lw_huizong_ywfw-ktokd  = wa_huizong-ktokd.  "�ͻ��˻���
        lw_huizong_ywfw-kzhms  = wa_huizong-kzhms.  "�ͻ��˻�������
        lw_huizong_ywfw-koart  = wa_huizong-koart.  "��Ŀ����
        lw_huizong_ywfw-klev1  = wa_huizong-klev1.  "һ����Ŀ
        lw_huizong_ywfw-yjkmt  = wa_huizong-yjkmt.  "һ����Ŀ����
        lw_huizong_ywfw-hkont  = wa_huizong-hkont.  "�����Ŀ
        lw_huizong_ywfw-txt50  = wa_huizong-txt50.  "��Ŀ����
        lw_huizong_ywfw-gsber  = wa_huizong-gsber.  "ҵ��Χ
        lw_huizong_ywfw-gtext  = wa_huizong-gtext.  "ҵ��Χ����
        lw_huizong_ywfw-waers  = wa_huizong-waers.  "����
        lw_huizong_ywfw-waers_bb  = wa_huizong-waers_bb.  "����

        APPEND lw_huizong_ywfw TO lt_huizong_ywfw.
        CLEAR lw_huizong_ywfw.
        CLEAR wa_huizong.
      ENDLOOP.
*����ȥ��
      SORT lt_huizong_ywfw.
      DELETE ADJACENT DUPLICATES FROM lt_huizong_ywfw.

*����ҵ��Χ���л���
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
*Ϊ����ڱ��������
      IF lt_huizong_out IS NOT INITIAL.
        CLEAR it_huizong_out.
        APPEND LINES OF lt_huizong_out TO it_huizong_out.
      ENDIF.
*---------------------------------------------------------------------------


*��ѡ�������ĺ�ҵ��Χ
    ELSEIF c_1 EQ 'X' AND c_2 EQ 'X'.
      LOOP AT it_huizong INTO wa_huizong.
        lw_huizong_hebing-bukrs  = wa_huizong-bukrs.  "��˾����
        lw_huizong_hebing-butxt  = wa_huizong-butxt.  "��˾��������
        lw_huizong_hebing-lifnr  = wa_huizong-lifnr.  "��Ӧ�̱���
        lw_huizong_hebing-namev  = wa_huizong-namev.  "��Ӧ������
        lw_huizong_hebing-kunnr  = wa_huizong-kunnr.  "�ͻ�����
        lw_huizong_hebing-namec  = wa_huizong-namec.  "�ͻ�����
        lw_huizong_hebing-ksbm   = wa_huizong-ksbm.   "���̱���
        lw_huizong_hebing-ksms   = wa_huizong-ksms.   "��������
        lw_huizong_hebing-guoj   = wa_huizong-guoj.   "����
        lw_huizong_hebing-ktokk  = wa_huizong-ktokk.  "��Ӧ���˻���
        lw_huizong_hebing-gzhms  = wa_huizong-gzhms.  "��Ӧ���˻�������
        lw_huizong_hebing-ktokd  = wa_huizong-ktokd.  "�ͻ��˻���
        lw_huizong_hebing-kzhms  = wa_huizong-kzhms.  "�ͻ��˻�������
        lw_huizong_hebing-koart  = wa_huizong-koart.  "��Ŀ����
        lw_huizong_hebing-klev1  = wa_huizong-klev1.  "һ����Ŀ
        lw_huizong_hebing-yjkmt  = wa_huizong-yjkmt.  "һ����Ŀ����
        lw_huizong_hebing-hkont  = wa_huizong-hkont.  "�����Ŀ
        lw_huizong_hebing-txt50  = wa_huizong-txt50.  "��Ŀ����
        lw_huizong_hebing-prctr  = wa_huizong-prctr.  "��������
        lw_huizong_hebing-prctt  = wa_huizong-prctt.  "��������
        lw_huizong_hebing-gsber  = wa_huizong-gsber.  "ҵ��Χ
        lw_huizong_hebing-gtext  = wa_huizong-gtext.  "ҵ��Χ
        lw_huizong_hebing-waers  = wa_huizong-waers.  "����
        lw_huizong_hebing-waers_bb  = wa_huizong-waers_bb.  "����
        APPEND lw_huizong_hebing TO lt_huizong_hebing.
        CLEAR lw_huizong_hebing.
        CLEAR wa_huizong.
      ENDLOOP.
*����ȥ��
      SORT lt_huizong_hebing.
      DELETE ADJACENT DUPLICATES FROM lt_huizong_hebing.

*��������������ҵ��Χ�ϲ����л���
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
*Ϊ����ڱ��������
      IF lt_huizong_out IS NOT INITIAL.
        CLEAR it_huizong_out.
        APPEND LINES OF lt_huizong_out TO it_huizong_out.
      ENDIF.
*---------------------------------------------------------------------------


*������ѡ�������ĺ�ҵ��Χ
    ELSEIF c_1 NE 'X' AND c_2 NE 'X'.
      LOOP AT it_huizong INTO wa_huizong.
        lw_huizong_kesh-bukrs  = wa_huizong-bukrs.  "��˾����
        lw_huizong_kesh-butxt  = wa_huizong-butxt.  "��˾��������
        lw_huizong_kesh-lifnr  = wa_huizong-lifnr.  "��Ӧ�̱���
        lw_huizong_kesh-namev  = wa_huizong-namev.  "��Ӧ������
        lw_huizong_kesh-kunnr  = wa_huizong-kunnr.  "�ͻ�����
        lw_huizong_kesh-namec  = wa_huizong-namec.  "�ͻ�����
        lw_huizong_kesh-ksbm   = wa_huizong-ksbm.   "���̱���
        lw_huizong_kesh-ksms   = wa_huizong-ksms.   "��������
        lw_huizong_kesh-guoj   = wa_huizong-guoj.   "����
        lw_huizong_kesh-ktokk  = wa_huizong-ktokk.  "��Ӧ���˻���
        lw_huizong_kesh-gzhms  = wa_huizong-gzhms.  "��Ӧ���˻�������
        lw_huizong_kesh-ktokd  = wa_huizong-ktokd.  "�ͻ��˻���
        lw_huizong_kesh-kzhms  = wa_huizong-kzhms.  "�ͻ��˻�������
        lw_huizong_kesh-koart  = wa_huizong-koart.  "��Ŀ����
        lw_huizong_kesh-klev1  = wa_huizong-klev1.  "һ����Ŀ
        lw_huizong_kesh-yjkmt  = wa_huizong-yjkmt.  "һ����Ŀ����
        lw_huizong_kesh-hkont  = wa_huizong-hkont.  "�����Ŀ
        lw_huizong_kesh-txt50  = wa_huizong-txt50.  "��Ŀ����
        lw_huizong_kesh-waers  = wa_huizong-waers.  "����
        lw_huizong_kesh-waers_bb  = wa_huizong-waers_bb.  "����
        APPEND lw_huizong_kesh TO lt_huizong_kesh.
        CLEAR lw_huizong_kesh.
        CLEAR wa_huizong.
      ENDLOOP.
*����ȥ��
      SORT lt_huizong_kesh.
      DELETE ADJACENT DUPLICATES FROM lt_huizong_kesh.
*���տ��̽��л���
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
*Ϊ����ڱ��������
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

*����ģʽ��ʾ
  IF r_2 EQ 'X'.
    IF c_1 EQ 'X' AND c_2 NE 'X'.
*��ȡ��ϸ����
      PERFORM frm_get_item1 USING pw_huizong_out.
    ELSEIF c_1 NE 'X' AND c_2 EQ 'X'.
*��ȡ��ϸ����
      PERFORM frm_get_item2 USING pw_huizong_out.
    ELSEIF c_1 EQ 'X' AND c_2 EQ 'X'.
*��ȡ��ϸ����
      PERFORM frm_get_item3 USING pw_huizong_out.
    ELSEIF c_1 NE 'X' AND c_2 NE 'X'.
*��ȡ��ϸ����
      PERFORM frm_get_item4 USING pw_huizong_out.
    ENDIF.
*�����ϼ�
    "    perform frm_sum_item.

*������ϸ������ʾ���ֶ�
    PERFORM f_field_item.
*չʾALV
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
*����ֻ��ѡ�������ĵĻ���ģʽ�Ѷ�Ӧ����ϸ����ѡȡ�����Ž��ڱ�
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
*��ϸ����װ����ʾ���ڱ�
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

  CONCATENATE:'����С�ڵ���'p_date1 '��' INTO lv_dmbt1.
  CONCATENATE:'����'p_dy1 '�쵽' p_date2 '��' INTO lv_dmbt2.
  CONCATENATE:'����'p_dy2 '�쵽' p_date3 '��' INTO lv_dmbt3.
  CONCATENATE:'����'p_dy3 '�쵽' p_date4 '��' INTO lv_dmbt4.
  CONCATENATE:'����'p_dy4 '�쵽' p_date5 '��' INTO lv_dmbt5.
  CONCATENATE:'����'p_dy5 '�쵽' p_date6 '��' INTO lv_dmbt6.
  CONCATENATE:'����'p_dy6 '�쵽' p_date7 '��' INTO lv_dmbt7.
  CONCATENATE:'����'p_dy7 '�쵽' p_date8 '��' INTO lv_dmbt8.

  CLEAR it_fieldcat.
  PERFORM f_fieldcat_set TABLES it_fieldcat
  USING:  'KSBM'  TEXT-051 'X' '' '',  "���̱���
          'KSMS'  TEXT-052 'X' '' '',  "��������
          'BUKRS' TEXT-003 'X' '' '',  "��˾����
          'BUTXT' TEXT-057 'X' '' '',  "��˾��������
          'GJAHR' TEXT-063 'X' '' '',  "������
          'BELNR' TEXT-061 'X' 'X' '',  "���ƾ֤
          'BUZEI' TEXT-062 'X' 'X' '',  "���ƾ֤��
          'XBLNR_ALT' TEXT-082 '' '' '', "����ƾ֤���
          'GUOJ'  TEXT-055 '' '' '',      "����
          'KTOKD' TEXT-009 '' '' '',  "�ͻ��˻���
          'KZHMS' TEXT-049 '' '' '',  "�ͻ��˻�������
          'KTOKK' TEXT-008 '' '' '',  "��Ӧ���˻���
          'GZHMS' TEXT-050 '' '' '',   "��Ӧ���˻�������
          'KUNNR' TEXT-006 '' '' '',   "�ͻ�����
          'NAMEC' TEXT-007 '' '' '',  "�ͻ�����
          'LIFNR' TEXT-004 '' '' '',   "��Ӧ�̱���
          'NAMEV' TEXT-005 '' '' '',   "��Ӧ������
          'KOART' TEXT-053 '' '' '',  "��Ŀ����
          'KLEV1' TEXT-039 '' '' '',      "һ����Ŀ
          'YJKMT' TEXT-054 '' '' '',  "һ����Ŀ����
          'HKONT' TEXT-010 '' '' '',  "�����Ŀ
          'TXT50' TEXT-011 '' '' '', "��Ŀ����
          'SGL'   TEXT-056 '' '' '',      "SGL
          'PRCTR' TEXT-058 '' '' '', "��������
          'PRCTT' TEXT-059 '' '' '', "������������
          'GSBER' TEXT-060 '' '' '', "ҵ��Χ
          'GTEXT' TEXT-080 '' '' '', "ҵ��Χ����
          'BLART' TEXT-064 '' '' '',  "ƾ֤����
          'TCODE' TEXT-065 '' '' '', "�������
          'ZUONR' TEXT-066 '' '' '',  "����
          'SGTXT' TEXT-067 '' '' '',  "��Ŀ�ı�
          'BLDAT' TEXT-068 '' '' '', "ƾ֤����
          'BUDAT' TEXT-069 '' '' '',  "��������
          'DUDAT' TEXT-070 '' '' '',  "������
          'AUGBL' TEXT-071 '' '' '',  "����ƾ֤��
          'AUGDT' TEXT-072 '' '' '',  "��������
          'KSYE ' TEXT-073 '' '' 'X', "�������
          'WDQJE' TEXT-074 '' '' 'X',  "δ�������
          'ZLJE'  TEXT-075 '' '' 'X',  "������
          'WAERS' TEXT-076 '' '' '',  "����
          'ZLJE_BB'  TEXT-085 '' '' 'X',  "������
          'WAERS_BB' TEXT-086 '' '' '',  "����
          'XTYPE' TEXT-077 '' '' ''.       "��������

  IF p_date1 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT1' lv_dmbt1 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'������ڵ���' p_dy1 INTO lv_dmbt9.
  ENDIF.
  IF p_date2 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT2' lv_dmbt2 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'������ڵ���' p_dy2 INTO lv_dmbt9.
  ENDIF.
  IF p_date3 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT3' lv_dmbt3 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'������ڵ���' p_dy3 INTO lv_dmbt9.
  ENDIF.
  IF p_date4 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT4' lv_dmbt4 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'������ڵ���' p_dy4 INTO lv_dmbt9.
  ENDIF.
  IF p_date5 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT5' lv_dmbt5 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'������ڵ���' p_dy5 INTO lv_dmbt9.
  ENDIF.
  IF p_date6 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT6' lv_dmbt6 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'������ڵ���' p_dy6 INTO lv_dmbt9.
  ENDIF.
  IF p_date7 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT7' lv_dmbt7 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'������ڵ���' p_dy7 INTO lv_dmbt9.
  ENDIF.
  IF p_date8 NE ''.
    PERFORM f_fieldcat_set TABLES it_fieldcat USING 'DMBT8' lv_dmbt8 ''  '' 'X'.
    CLEAR lv_dmbt9.
    CONCATENATE:'������ڵ���' p_dy8 INTO lv_dmbt9.
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
  DATA: lwa_layout    TYPE slis_layout_alv, "ALV����Ŀ���
        l_variant     TYPE disvariant,     "ALV��ʽ
        l_repid       TYPE sy-repid,       "��������
        lwa_excluding TYPE slis_t_extab.   "�ų��Ĳ˵���ť



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
*����ֻ��ҵ��Χ�Ļ���ģʽ�Ѷ�Ӧ����ϸ����ѡȡ�����Ž��ڱ�
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
*��ϸ����װ����ʾ���ڱ�
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

  CONCATENATE:'����С�ڵ���'p_date1 '��' INTO lv_dmbt1.
  CONCATENATE:'����'p_dy1 '�쵽' p_date2 '��' INTO lv_dmbt2.
  CONCATENATE:'����'p_dy2 '�쵽' p_date3 '��' INTO lv_dmbt3.
  CONCATENATE:'����'p_dy3 '�쵽' p_date4 '��' INTO lv_dmbt4.
  CONCATENATE:'����'p_dy4 '�쵽' p_date5 '��' INTO lv_dmbt5.
  CONCATENATE:'����'p_dy5 '�쵽' p_date6 '��' INTO lv_dmbt6.
  CONCATENATE:'����'p_dy6 '�쵽' p_date7 '��' INTO lv_dmbt7.
  CONCATENATE:'����'p_dy7 '�쵽' p_date8 '��' INTO lv_dmbt8.


  CONCATENATE:'������ڵ���' p_dy8 INTO lv_dmbt9.
  CLEAR it_fieldcat.
  PERFORM f_fieldcat_set TABLES it_fieldcat
  USING:  'KSBM'  TEXT-051 'X' '' '',  "���̱���
          'KSMS'  TEXT-052 'X' '' '',  "��������
          'BUKRS' TEXT-003 'X' '' '',  "��˾����
          'BUTXT' TEXT-057 'X' '' '',  "��˾��������
          'GJAHR' TEXT-063 'X' '' '',  "������
          'BELNR' TEXT-061 'X' 'X' '',  "���ƾ֤
          'BUZEI' TEXT-062 'X' 'X' '',  "���ƾ֤��
          'GUOJ'  TEXT-055 '' '' '',      "����
          'KTOKD' TEXT-009 '' '' '',  "�ͻ��˻���
          'KZHMS' TEXT-049 '' '' '',  "�ͻ��˻�������
          'KTOKK' TEXT-008 '' '' '',  "��Ӧ���˻���
          'GZHMS' TEXT-050 '' '' '',   "��Ӧ���˻�������
          'KUNNR' TEXT-006 '' '' '',   "�ͻ�����
          'NAMEC' TEXT-007 '' '' '',  "�ͻ�����
          'LIFNR' TEXT-004 '' '' '',   "��Ӧ�̱���
          'NAMEV' TEXT-005 '' '' '',   "��Ӧ������
          'KOART' TEXT-053 '' '' '',  "��Ŀ����
          'KLEV1' TEXT-039 '' '' '',      "һ����Ŀ
          'YJKMT' TEXT-054 '' '' '',  "һ����Ŀ����
          'HKONT' TEXT-010 '' '' '',  "�����Ŀ
          'TXT20' TEXT-011 '' '' '', "��Ŀ����
          'SGL'   TEXT-056 '' '' '',      "SGL
          'PRCTR' TEXT-058 '' '' '', "��������
          'PRCTT' TEXT-059 '' '' '', "������������
          'GSBER' TEXT-060 '' '' '', "ҵ��Χ
          'GTEXT' TEXT-080 '' '' '', "ҵ��Χ����
          'BLART' TEXT-064 '' '' '',  "ƾ֤����
          'TCODE' TEXT-065 '' '' '', "�������
          'ZUONR' TEXT-066 '' '' '',  "����
          'SGTXT' TEXT-067 '' '' '',  "��Ŀ�ı�
          'BLDAT' TEXT-068 '' '' '', "ƾ֤����
          'BUDAT' TEXT-069 '' '' '',  "��������
          'DUDAT' TEXT-070 '' '' '',  "������
          'AUGBL' TEXT-071 '' '' '',  "����ƾ֤��
          'AUGDT' TEXT-072 '' '' '',  "��������
          'KSYE ' TEXT-073 '' '' 'X', "�������
          'WDQJE' TEXT-074 '' '' 'X',  "δ�������
          'ZLJE'  TEXT-075 '' '' 'X',  "������
          'WAERS' TEXT-076 '' '' '',  "����
          'XTYPE' TEXT-077 '' '' '',       "��������
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
*����ͬʱ��ѡ�������ĺ�ҵ��Χ�Ļ���ģʽ�Ѷ�Ӧ����ϸ����ѡȡ�����Ž��ڱ�
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
*��ϸ����װ����ʾ���ڱ�
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
*����ͬʱ����ѡ�������ĺ�ҵ��Χ�Ļ���ģʽ�Ѷ�Ӧ����ϸ����ѡȡ�����Ž��ڱ�
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
*��ϸ����װ����ʾ���ڱ�
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
  DATA: lv_ksye  TYPE bsik-dmbtr,  "�������
        lv_wdqje TYPE bsik-dmbtr,  "δ�������
        lv_zlje  TYPE bsik-dmbtr,  "������
        lv_dmbt0 TYPE bsik-dmbtr,  "������
        lv_dmbt1 TYPE bsik-dmbtr,                                 "30������
        lv_dmbt2 TYPE bsik-dmbtr,                                 "31��60
        lv_dmbt3 TYPE bsik-dmbtr,                                 "61��90
        lv_dmbt4 TYPE bsik-dmbtr,                                 "3��6����
        lv_dmbt5 TYPE bsik-dmbtr,                                 "6��12����
        lv_dmbt6 TYPE bsik-dmbtr,                                 "1��2��
        lv_dmbt7 TYPE bsik-dmbtr,                                 "2��3��
        lv_dmbt8 TYPE bsik-dmbtr,                                 "3��4��
        lv_dmbt9 TYPE bsik-dmbtr.                                 "4��5��


*��ϸģʽ
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
  DATA: lv_ksye  TYPE bsik-dmbtr,  "�������
        lv_wdqje TYPE bsik-dmbtr,  "δ�������
        lv_zlje  TYPE bsik-dmbtr,  "������
        lv_dmbt0 TYPE bsik-dmbtr,  "������
        lv_dmbt1 TYPE bsik-dmbtr,                                 "30������
        lv_dmbt2 TYPE bsik-dmbtr,                                 "31��60
        lv_dmbt3 TYPE bsik-dmbtr,                                 "61��90
        lv_dmbt4 TYPE bsik-dmbtr,                                 "3��6����
        lv_dmbt5 TYPE bsik-dmbtr,                                 "6��12����
        lv_dmbt6 TYPE bsik-dmbtr,                                 "1��2��
        lv_dmbt7 TYPE bsik-dmbtr,                                 "2��3��
        lv_dmbt8 TYPE bsik-dmbtr,                                 "3��4��
        lv_dmbt9 TYPE bsik-dmbtr.                                 "4��5��


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
  ""������
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
