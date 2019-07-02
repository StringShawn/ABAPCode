*$*$********************************************************************
* Program ID/Name:ZPPR005                 Date written:2013/7/15
* Author's name: HP_FCG                   Last update:2013/8/5
* Program title:��������������ά��
* Project Name:  EPR I
* Version:V0
* Function Spec ID:PP_01_05
*----------------------------------------------------------------------*
* Description: (Incl. Related Function Area and System)
*              �û�ά����Excel�ļ����Ƶ�txt�ļ��������ص�SAP��
*              ������ͼ������������������
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
*       ITEMNO    LIKE SY-TABIX,   "���
*       MATNR     LIKE MARA-MATNR,  "���Ϻ���
*        MTART     LIKE MARA-MTART,  "��������
*       MAKTXC    LIKE MAKT-MAKTX,  "��������(ZH)
*       MAKTXE    LIKE MAKT-MAKTX,  "��������(EN)
*       MEINS     LIKE MARA-MEINS,  "������λ
*       MATKL     LIKE MARA-MATKL,  "������
*       EXTWG     LIKE MARA-EXTWG,  "�ⲿ������
*       BRGEW     LIKE MARA-BRGEW,  "ë��
*       NTGEW     LIKE MARA-NTGEW,  "����
*       GEWEI     LIKE MARA-GEWEI,  "������λ
*       Z_CZ      LIKE MARA-Z_CZ,  "���ʣ����ϣ�
*       Z_ATXLJ   LIKE MARA-Z_ATXLJ,"A�������
*       Z_CC      LIKE MARA-Z_CC,   "�ߴ�
*       Z_JGGY    LIKE MARA-Z_JGGY, "�ӹ�����
*       WERKS     LIKE MARC-WERKS,    "����
*       EKGRP     LIKE MARC-EKGRP,  "�ɹ���
*       KORDB     LIKE MARC-KORDB,  "Դ�嵥
*       DISMM     LIKE MARC-DISMM,   "MRP����
*       MINBE     LIKE MARC-MINBE,   "�ٶ�����
*       DISPO     LIKE MARC-DISPO,  "MRP������
*       DISGR     LIKE MARC-DISGR,   "MRP��
*       DISLS     LIKE MARC-DISLS,   "������С
*       BSTFE     LIKE MARC-BSTFE,   "�̶�����
*       BSTMI     LIKE MARC-BSTMI,   "��С������С
*       BSTMA     LIKE MARC-BSTMA,   "���������С
*       MABST     LIKE MARC-MABST, "�����ˮƽ "added 2013/8/6
*       BSTRF     LIKE MARC-BSTRF,   "����ֵ
*       BESKZ     LIKE MARC-BESKZ,   "�ɹ�����
*       SOBSL     LIKE MARC-SOBSL,   "����ɹ�����
*       Z_ZSWL    LIKE MARA-Z_ZSWL, "ת������
*       LGPRO     LIKE MARC-LGPRO,   "�����ִ��ص�
*       FHORI     LIKE MARC-FHORI,   "�ƻ��߼���
*       DZEIT     LIKE MARC-DZEIT,   "�ڲ�����ʱ��
*       PLIFZ     LIKE MARC-PLIFZ,   "�ƻ�����ʱ��
*       WEBAZ     LIKE MARC-WEBAZ,   "�ջ�����ʱ��
*       MRPPP     LIKE MARC-MRPPP,  "�ƻ�����"added 2013/8/5
*       EISBE     LIKE MARC-EISBE,   "��ȫ���
*       SHZET     LIKE MARC-SHZET,   "��ȫʱ��
*       MAGRV     LIKE MARA-MAGRV,   "������İ�װ����
*       RMATP     LIKE MARA-RMATP,   "��װ�Ĳο�����
*       Z_NMQT    LIKE MARA-Z_NMQT,   "��׼��װ��
*       Z_BXQT    LIKE MARA-Z_BXQT,   "��װ��ȫ���
*       Z_JZKX     LIKE MARA-Z_JZKX, "��׼������
*       Z_GYL     LIKE MARA-Z_GYL,  "������
*       Z_SYCX    LIKE MARA-Z_SYCX, "���ó���
*       ALTSL     LIKE MARC-ALTSL,   "BOMѡ�񷽷�
*       SAUFT     LIKE MARC-SAUFT,   "��ʶ���ظ���������
*       SFEPR     LIKE MARC-SFEPR,   "�ظ���������ļ�
*       BKLAS     LIKE MBEW-BKLAS,   "������
*       VPRSV     LIKE MBEW-VPRSV,   "�۸����
*       PEINH     LIKE MBEW-PEINH,   "�۸�λ
*       STPRS     LIKE MBEW-STPRS,   "��׼�۸�
*       EKALR     LIKE MBEW-EKALR,   "��QS�ĳɱ�����
*       HKMAT     LIKE MBEW-HKMAT,   "������Դ
*       HRKFT     LIKE MBEW-HRKFT,   "ԭʼ��
*       NCOST     LIKE MARC-NCOST,   "�޳ɱ�����
*       AWSLS     LIKE MARC-AWSLS,   "������
*       PRCTR     LIKE MARC-PRCTR,   "��������
*       LOSGR     LIKE MARC-LOSGR,  "�ɱ���������
*       ZPLP1     LIKE MBEW-ZPLP1,   "�ƻ��۸�1
*       ZPLD1     LIKE MBEW-ZPLD1,  "�ƻ��۸�����1
*       ZPLP2     LIKE MBEW-ZPLP2,   "�ƻ��۸�2
*       ZPLD2     LIKE MBEW-ZPLD2,   "�ƻ��۸�����2
*END OF IT_DATA.
*
*DATA:BEGIN OF IT_SALE OCCURS 0,
*      ITEMNO  LIKE SY-TABIX,   "���
*       MATNR   LIKE MARC-MATNR,  "���Ϻ���
*       MTART   LIKE MARA-MTART,  "��������
*       MAKTXC  LIKE MAKT-MAKTX,  "��������(ZH)
*       MAKTXE  LIKE MAKT-MAKTX,  "��������(EN)
*       WERKS   LIKE MARC-WERKS,  "����
*       VKORG   LIKE MVKE-VKORG,  "������֯
*       VTWEG   LIKE MVKE-VTWEG,  "�ַ�����
*       SPART   LIKE MARA-SPART,  "��Ʒ��
*       TAXKM   LIKE TSKM-TAXKM,  "˰�����
*       MTPOS   LIKE MVKE-MTPOS,  "��Ŀ�����
*       KTGRM   LIKE MVKE-KTGRM,  "��Ŀ������
*       VERSG   LIKE MVKE-VERSG,  "����ͳ����
*       TRAGR   LIKE MARA-TRAGR,  "������
*       LADGR   LIKE MARC-LADGR,  "װ����
*END OF IT_SALE.

data:begin of it_data occurs 0,
       itemno       like sy-tabix,   "���
       matnr(18)    type c,  "���Ϻ���
     	 mtart(4)     type c,  "��������
       maktxc(40)   type c,  "��������(ZH)
       maktxe(40)   type c,  "��������(EN)
       meins(3)     type c,  "������λ
       matkl(9)     type c,  "������
       extwg(18)    type c,  "�ⲿ������
       brgew(13)    type c,  "ë��
       ntgew(13)    type c,  "����
       gewei(3)     type c,  "������λ
       z_cz(10)     type c,  "���ʣ����ϣ�
       z_atxlj(10)  type c,  "A�������
       z_cc(10)     type c,  "�ߴ�
       z_jggy(10)   type c,  "�ӹ�����
       werks(4)     type c,  "����
       ekgrp(3)     type c,  "�ɹ���
       kordb(1)     type c,  "Դ�嵥
       dismm(2)     type c,  "MRP����
       minbe(13)    type c,	 "�ٶ�����
       dispo(3)     type c,  "MRP������
       disgr(4)     type c,  "MRP��
       disls(2)     type c,  "������С
       bstfe(13)    type c,	 "�̶�����
       bstmi(13)    type c,	 "��С������С
       bstma(13)    type c,	 "���������С
       mabst(13)    type c,  "�����ˮƽ "added 2013/8/6
       bstrf(13)    type c,	 "����ֵ
       beskz(1)     type c,  "�ɹ�����
       sobsl(2)     type c,  "����ɹ�����
       z_zswl(1)    type c,  "ת������
       z_mpj(1)     type c,  "ë������ʶ
       lgpro(4)     type c,  "�����ִ��ص�
       fhori(3)     type c,  "�ƻ��߼���
       dzeit(3)     type c,  "�ڲ�����ʱ��
       plifz(3)     type c,  "�ƻ�����ʱ��
       webaz(3)     type c,  "�ջ�����ʱ��
       mrppp(3)     type c,  "�ƻ�����"added 2013/8/5
       eisbe(13)    type c,	 "��ȫ���
       shzet(2)     type c,  "��ȫʱ��
       magrv(4)     type c,  "������İ�װ����
       rmatp(18)    type c,	 "��װ�Ĳο�����
       z_bxty(18)   type c,  "����
       z_nmqt(13)   type c,  "��׼��װ��
       z_bxqt(13)   type c,  "��װ��ȫ���
       z_jzkx(13)	  type c,  "��׼������
       z_gyl(13)    type c,  "������
       z_sycx(200)  type c,  "���ó���
       altsl(1)     type c,   "BOMѡ�񷽷�
       sauft(1)     type c,   "��ʶ���ظ���������
       sfepr(4)     type c,   "�ظ���������ļ�
       bklas(4)     type c,   "������
       vprsv(1)     type c,   "�۸����
       peinh(5)     type c,   "�۸�λ
       stprs(11)    type c,	 "��׼�۸�
       ekalr(1)     type c,   "��QS�ĳɱ�����
       hkmat(1)     type c,   "������Դ
       hrkft(4)     type c,   "ԭʼ��
       ncost(1)     type c,   "�޳ɱ�����
       awsls(6)     type c,   "������
       prctr(10)    type c,	 "��������
       losgr(13)    type c,  "�ɱ���������
       zplp1(11)    type c,	 "�ƻ��۸�1
       zpld1(8)     type c,  "�ƻ��۸�����1
       zplp2(11)    type c,	 "�ƻ��۸�2
       zpld2(8)     type c,   "�ƻ��۸�����2
end of it_data.

data:begin of it_sale occurs 0,
       itemno      like sy-tabix,   "���
       matnr(18)   type c,  "���Ϻ���
       mtart(4)    type c,  "��������
       maktxc(40)  type c,  "��������(ZH)
       maktxe(40)  type c,  "��������(EN)
       werks(4)    type c,  "����
       vkorg(4)    type c,  "������֯
       vtweg(2)    type c,  "�ַ�����
       spart(2)    type c,  "��Ʒ��
       taxkm(1)    type c,  "˰�����
       mtpos(4)    type c,  "��Ŀ�����
       ktgrm(2)    type c,  "��Ŀ������
       versg(1)    type c,  "����ͳ����
       tragr(4)    type c,  "������
       ladgr(4)    type c,  "װ����
end of it_sale.

DATA:BEGIN OF it_quality_upload OCCURS 0 ,
       itemno      LIKE sy-tabix ,  " ���
       matnr(18)   TYPE c ,         " ���Ϻ���
       mtart(4)    type c,  "��������
       maktxc(40)  type c,  "��������(ZH)
       werks(4)    type c,  "����
       qmata(6)    TYPE c,  " ����ҵ�����
       qmpur(1)    type c , " �ɹ�����
       ssqss(8)    type c , " QM������
       art(8)      type c , " ��������
       apa(1)      type c , " ��ѡ����
       aktiv(1)    type c, " �������ͼ���
     END OF it_quality_upload .
DATA:BEGIN OF it_quality OCCURS 0 ,
       matnr(18)   TYPE c ,         " ���Ϻ���
       mtart(4)    type c,  "��������
       werks(4)    type c,  "����
       maktx(40)  type c,  "��������(ZH)
       qmata(6)    TYPE c,  " ����ҵ�����
       qmpur(1)    type c , " �ɹ�����
       ssqss(8)    type c , " QM������
       art(8)      type c , " ��������
       apa(1)      type c , " ��ѡ����
       aktiv(1)    type c, " �������ͼ���
       itemno      LIKE sy-tabix ,  " ���
     END OF it_quality .


data:begin of it_out occurs 0,
  itemno  like sy-tabix,   "���
  matnr   like marc-matnr,    "���Ϻ���
  maktxc  like makt-maktx,    "��������(ZH)
  maktxe  like makt-maktx,    "��������(ZH)
  werks   like marc-werks,    "��������
  vkorg   like mvke-vkorg,    "������֯
  vtweg   like mvke-vtweg,    "�ַ�����
  art     like qmat-art ,     "��������
  msgty   like bapie1ret2-type, "��Ϣ����
  message like bapie1ret2-message,  " ִ�н����Ϣ
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
    &2 = c_fieldx."������λ
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
            p_qlty    as checkbox .   " hp_dxj 20150409 ע��
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
        message S368(00) with '��ά�����͵�Ȩ�ޣ�' DISPLAY LIKE 'E'.
        STOP .
      ENDIF.

    WHEN 'PUSH3'.
      AUTHORITY-CHECK OBJECT 'Z_EXTGROUP'
               ID 'Z_EXTGROUP' FIELD '1'.
      IF SY-SUBRC <> 0.
*   Implement a suitable exception handling here
        mytab-dynnr = 100 .
        mytab-activetab = 'PUSH1'.
        message S368(00) with '��ά���ⲿ�������Ȩ�ޣ�' DISPLAY LIKE 'E' .
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

*  ��Ļ100��ά�����ϵ���ͼ
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
*        message e368(00) with '��ά�����͵�Ȩ�ޣ�' .
*      ENDIF.
      PERFORM frm_maintenance_table_view USING g_tabname .

    ENDIF .

    IF pr_extwg = 'X' AND mytab-dynnr = 300 .
*        AUTHORITY-CHECK OBJECT 'Z_EXTGROUP'
*                 ID 'Z_EXTGROUP' FIELD '1'.
*        IF SY-SUBRC <> 0.
**   Implement a suitable exception handling here
*          message e368(00) with '��ά���ⲿ�������Ȩ�ޣ�' .
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

  if p_sale = abap_true."�ϴ�������ͼ
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

  elseif p_qlty = 'X'.   "��������
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
  ELSE. " �ϴ�������ͼ
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




**********���������Ȩ��
******E:�ɹ�    K����������
    data:lwa_mara type mara.

    authority-check object 'Z_MATGROUP'
                      id 'ZMATKL' field 'X'.
    if sy-subrc = 0 and p_basic = 'X'.
      message s368(00) with '��ֻ�ܸ��������飡' display like 'E'.
      leave list-processing.
    endif.


    if p_basic = abap_true.""""""���»�����ͼʱ

    elseif p_maktl = 'X'.
      authority-check object 'Z_MATGROUP'
                        id 'ZMATKL' field 'X'.
      if sy-subrc <> 0.
        message s368(00) with '�޸����������Ȩ�ޣ�' display like 'E'.
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
      translate it_sale-matnr to upper case. " ���Ϻ���

      move-corresponding it_sale to it_out.
**1.��������ͼ�Ƿ����

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
***2.�������ֶ�..... " ������ͼ�ı����ֶμ��.....
      check_mandtory it_sale-matnr text-007. " ���Ϻ���.
*      CHECK_MANDTORY IT_SALE-MTART TEXT-008. " ��������.
*      CHECK_MANDTORY IT_SALE-MAKTXC TEXT-009. "��������(ZH)
*      CHECK_MANDTORY IT_SALE-MAKTXE TEXT-010."��������(EN)
      check_mandtory it_sale-vkorg  text-011."  ������֯
      check_mandtory it_sale-vtweg  text-012."  �ַ�����
      check_mandtory it_sale-spart text-013."  ��Ʒ��
      check_mandtory it_sale-taxkm text-014."  ˰�����
      check_mandtory it_sale-mtpos text-015."  ��Ŀ�����
*      CHECK_MANDTORY IT_SALE-KTGRM TEXT-016."  ��Ŀ������
      translate it_sale-vkorg to upper case.
      translate it_sale-werks to upper case.

      wa_l_head-material = it_sale-matnr.
      wa_l_head-ind_sector = 'M'.
      wa_l_head-matl_type = it_sale-mtart.
***** ��ͼ���±�ʶ.

      wa_l_head-sales_view = abap_true.
*      WA_L_HEAD-BASIC_VIEW = ABAP_TRUE.
      wa_l_plant-plant = it_sale-werks.  "����
      wa_l_plant-loadinggrp = it_sale-ladgr.  "װ����
****** hp_dxj 20140822  start ����������״̬
      select single mmsta into i_mmsta from marc
                          where matnr = it_sale-matnr
                           and werks = it_sale-werks.
****** hp_dxj 20140822  end ����������״̬
      if sy-subrc ne 0.
        wa_l_plant-pur_status = '01'.    "�����ض�������״̬
        wa_l_plantx-pur_status = c_fieldx.    "�����ض�������״̬
      endif.


      wa_l_client-division = it_sale-spart.  "��Ʒ��
      wa_l_client-trans_grp = it_sale-tragr.  "������
      wa_l_sale-sales_org =  it_sale-vkorg.  "������֯
      wa_l_sale-distr_chan = it_sale-vtweg.  "�ַ�����
      wa_l_sale-item_cat = it_sale-mtpos.  "��Ŀ�����
      wa_l_sale-acct_assgt = it_sale-ktgrm.  "��Ŀ������
      wa_l_sale-matl_stats = it_sale-versg.  "����ͳ����
      wa_l_plant-availcheck = 'KP'.

      if it_sale-taxkm ne c_sline.
        clear wa_l_taxcal.
        wa_l_taxcal-depcountry = c_country.
        wa_l_taxcal-tax_type_1 = c_taxtype.
        wa_l_taxcal-taxclass_1 = it_sale-taxkm.  "˰�����
        append wa_l_taxcal to it_l_taxcal.
      endif.

      wa_l_plantx-plant = it_sale-werks.  "����
      wa_l_salex-sales_org = it_sale-vkorg ."������֯
      wa_l_salex-distr_chan = it_sale-vtweg ."�ַ�����
      wa_l_plantx-availcheck = c_fieldx.
      set_fieldx it_sale-ladgr wa_l_plantx-loadinggrp ."װ����
      set_fieldx it_sale-spart wa_l_clientx-division ."��Ʒ��
      set_fieldx it_sale-tragr wa_l_clientx-trans_grp."������

      set_fieldx it_sale-mtpos wa_l_salex-item_cat."��Ŀ�����
      set_fieldx it_sale-ktgrm wa_l_salex-acct_assgt."��Ŀ������
      set_fieldx it_sale-versg wa_l_salex-matl_stats."����ͳ����

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
*****ת����д��ʽ



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
        if sy-subrc <> 0."""�޲ɹ�Ȩ��
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
            it_out-message = 'TRֻ�ܸ���������Ϊ99'.
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
              it_out-message = '�������Ѿ���ά��Ϊ��99��ֵ'.
              append it_out.
              CONTINUE.
            endif.
          endif.

        endif.
      ENDIF.









      translate it_data-matnr to upper case. " ���Ϻ���
      translate it_data-meins to upper case. " ������λ
      translate it_data-gewei to upper case. "��������λ��
      translate it_data-dismm to upper case.  "��MRP���͡�
      translate it_data-ekgrp to upper case. "���ɹ��顱
      translate it_data-disls to upper case. "��������С����
      translate it_data-beskz to upper case."���ɹ����͡�
*      TRANSLATE IT_DATA-VKORG TO UPPER CASE."�ɹ���֯
      translate it_data-werks to upper case."��������.

      move-corresponding it_data to it_out.
**1.��������ͼ�Ƿ����

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

***2.�������ֶ�.....

      if p_basic = abap_true. "������ͼ�ı����ֶμ��...

        check_mandtory it_data-matnr text-007. " ���Ϻ���.
        check_mandtory it_data-mtart text-008. " ��������.
        check_mandtory it_data-maktxc text-009. "��������(ZH)
        check_mandtory it_data-maktxe text-010. "��������(EN)
        check_mandtory it_data-meins text-017. " ������λ
        if it_data-mtart = 'ROH'.
          check_mandtory it_data-matkl text-018. " ������
        endif.
        check_mandtory it_data-extwg text-019.  "�ⲿ������
      endif.

      if p_mrp = abap_true. " MRP ��ͼ�ı����ֶμ��.....
        if it_data-rmatp = 'P001' or it_data-rmatp = '/'.
        else.
          it_out-msgty = 'E'.
          message e075  into it_out-message.
          append it_out.
          clear it_out.
          continue.
        endif.

        check_mandtory it_data-disgr text-023. "  MRP��
        check_mandtory it_data-werks text-020.  "����
        check_mandtory it_data-dismm text-021. " MRP����
        if it_data-dismm ne 'ND'.
          check_mandtory it_data-dispo text-022. "  MRP������

          check_mandtory it_data-disls text-024. "������С
        endif.
        if it_data-disls = 'PB'.
          check_mandtory it_data-mrppp text-036. "�ƻ�����
        endif.

        if it_data-disls = 'HB'.
          check_mandtory it_data-mabst text-037. "�����ˮƽ.
        endif.
        check_mandtory it_data-beskz text-025. "�ɹ�����
        check_mandtory it_data-fhori text-026. "�ƻ��߼���
        check_mandtory it_data-z_mpj text-038. "ë����20140508���ë����
      endif.

      if p_purch = abap_true. " �ɹ���ͼ�ı����ֶμ��.....

        check_mandtory it_data-werks text-020.  "����


*        IF IT_DATA-MTART = 'ROH'.
*          CHECK_MANDTORY IT_DATA-EKGRP TEXT-027. "�ɹ���
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


      if p_finan = abap_true. " ����ɱ���ͼ�ı����ֶμ��.....

        check_mandtory it_data-werks text-020.  "����
        check_mandtory it_data-bklas text-028. "������
        check_mandtory it_data-vprsv text-029. "�۸����
        check_mandtory it_data-peinh text-030. " �۸�λ
        check_mandtory it_data-ekalr text-031." ��QS�ĳɱ�����
        check_mandtory it_data-hkmat text-032." ������Դ
*        20140804 ��������  start hp_dxj
*        ���ݲɹ��������жϲ������Ƿ����
        if it_data-beskz = 'E' or it_data-beskz = 'X'.
          check_mandtory it_data-awsls text-033."������
        endif.
        check_mandtory it_data-losgr text-034."�ɱ���������
      endif.


**2.header data....
      wa_l_head-material = it_data-matnr.
      wa_l_head-ind_sector = 'M'.
      wa_l_head-matl_type = it_data-mtart.
***** ������ͼ����.....
      clear: wa_l_ext,wa_l_extx.
      wa_l_ext-structure = c_bapi_mara .
      wa_l_ext-valuepart1+0(18) =  it_data-matnr.  "���Ϻ���
      wa_l_extx-structure = c_bapi_marax .
      wa_l_extx-valuepart1+0(18) =  it_data-matnr.  "���Ϻ���
      if p_basic = abap_true.
        wa_l_head-basic_view = abap_true.
        if it_data-meins ne c_sline.
          wa_l_client-base_uom = it_data-meins.  "������λ
        endif.
        wa_l_client-matl_group = it_data-matkl.  "������
        wa_l_client-extmatlgrp = it_data-extwg.  "�ⲿ������
        if it_data-ntgew is initial.
          wa_l_client-net_weight = '0.001'.  "����
        else.
          if it_data-ntgew ne c_sline.
            wa_l_client-net_weight = it_data-ntgew.  "����
          endif.
        endif.
        if  it_data-gewei is initial.
          wa_l_client-unit_of_wt = 'KG'.  "������λ
        else.
          if it_data-gewei ne c_sline.
            wa_l_client-unit_of_wt = it_data-gewei.  "������λ
          endif.
        endif.



        wa_l_ext-valuepart1+18(10) =  it_data-z_cz.  "���ʣ����ϣ�
        wa_l_ext-valuepart1+28(10) =  it_data-z_atxlj."A�������
        wa_l_ext-valuepart1+38(10) =  it_data-z_cc.   "�ߴ�
        wa_l_ext-valuepart1+48(10) =  it_data-z_jggy. "�ӹ�����

        if it_data-brgew is initial.
          write '0.001' to   "ë��
         wa_l_ext-valuepart1+58(13) no-gap no-grouping
                                     left-justified.
        else.
          if it_data-brgew ne c_sline.
            write it_data-brgew to   "ë��
           wa_l_ext-valuepart1+58(13) no-gap no-grouping
                                       left-justified.
          endif.
        endif.

        set_fieldx it_data-meins wa_l_clientx-base_uom."������λ

        set_fieldx it_data-matkl wa_l_clientx-matl_group."������

        set_fieldx it_data-extwg wa_l_clientx-extmatlgrp ."�ⲿ������

        set_fieldx it_data-ntgew wa_l_clientx-net_weight ."����

        set_fieldx it_data-gewei wa_l_clientx-unit_of_wt ."������λ

        set_fieldx it_data-z_cz wa_l_extx-valuepart1+18(1)."���ʣ����ϣ�
        "A�������
        set_fieldx it_data-z_atxlj wa_l_extx-valuepart1+19(1).

        set_fieldx it_data-z_cc   wa_l_extx-valuepart1+20(1)."�ߴ�

        set_fieldx it_data-z_jggy wa_l_extx-valuepart1+21(1)."�ӹ�����

        set_fieldx it_data-brgew wa_l_extx-valuepart1+22(1)."ë��
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

*****MRP ��ͼ����
      if p_mrp = abap_true.
        wa_l_head-mrp_view = abap_true.
        clear: wa_l_ext,wa_l_extx.
        wa_l_ext-structure = c_bapi_marc .
        wa_l_ext-valuepart1+0(4) =  it_data-werks.  "���Ϻ���
        wa_l_extx-structure = c_bapi_marcx .
        wa_l_extx-valuepart1+0(4) =  it_data-werks.  "���Ϻ���
        wa_l_plant-plant =  it_data-werks.    "����
        wa_l_plant-mrp_type = it_data-dismm.   "MRP����
        if it_data-minbe ne c_sline.
          wa_l_plant-reorder_pt = it_data-minbe. "�ٶ�����
        endif.
        wa_l_plant-mrp_ctrler = it_data-dispo.  "MRP������
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
*          WA_L_PLANT-MRP_GROUP = '0010'.   "MRP��
          wa_l_plant-plan_strgp = '11'.
          wa_l_plant-mixed_mrp = '2'.
          wa_l_plantx-plan_strgp = c_fieldx.
          wa_l_plantx-mixed_mrp = c_fieldx.
          if it_data-disgr = '11'.
            concatenate '00' it_data-disgr into it_data-disgr.
            condense it_data-disgr.
          endif.
        endif.

        wa_l_plant-mrp_group = it_data-disgr.   "MRP��.

        wa_l_plant-lotsizekey  = it_data-disls. "������С
        if it_data-bstfe ne c_sline.
          wa_l_plant-fixed_lot = it_data-bstfe.  	 "�̶�����
          endif.
          if it_data-bstmi ne c_sline.
            wa_l_plant-minlotsize = it_data-bstmi.  "��С������С
          endif.
          if it_data-bstma ne c_sline.
            wa_l_plant-maxlotsize = it_data-bstma.  "���������С
          endif.
          if it_data-mabst ne c_sline.
            wa_l_plant-max_stock = it_data-mabst. "�����ˮƽ
          endif.
          if it_data-bstrf ne c_sline.
            if it_data-beskz = 'E'.
              wa_l_plant-round_val = space.
            else.
              wa_l_plant-round_val = it_data-bstrf.   "����ֵ
            endif.
          endif.
          wa_l_plant-proc_type = it_data-beskz.   "�ɹ�����
          wa_l_plant-spproctype = it_data-sobsl.  "����ɹ�����

          wa_l_plant-iss_st_loc = it_data-lgpro.  "�����ִ��ص�
          if it_data-fhori ne c_sline.
            wa_l_plant-sm_key = it_data-fhori.      "�ƻ��߼���
          endif.
          if it_data-dzeit ne c_sline.
            wa_l_plant-inhseprodt = it_data-dzeit.   "�ڲ�����ʱ��
          endif.
          if it_data-plifz ne c_sline.
            wa_l_plant-plnd_delry = it_data-plifz.    "�ƻ�����ʱ��
          endif.
          if it_data-webaz ne c_sline.
            wa_l_plant-gr_pr_time = it_data-webaz.    "�ջ�����ʱ��
          endif.
          wa_l_plant-ppc_pl_cal = it_data-mrppp."�ƻ����� added 2013/8/5
          if it_data-eisbe ne c_sline.
            wa_l_plant-safety_stk = it_data-eisbe.     "��ȫ���
          endif.
          if it_data-shzet ne c_sline.
            wa_l_plant-safetytime = it_data-shzet.     "��ȫʱ��
            if it_data-shzet is initial.
              wa_l_plant-safty_t_id = space.
            else.
              wa_l_plant-safty_t_id = '2'.
            endif.
            wa_l_plantx-safty_t_id =  c_fieldx.
          endif.
*          IF IT_DATA-ALTSL IS INITIAL.
          wa_l_plant-alt_bom_id = '2'.   "BOMѡ�񷽷�
*          ELSE.
*            WA_L_PLANT-ALT_BOM_ID = IT_DATA-ALTSL.   "BOMѡ�񷽷�
*          ENDIF.
*          IF IT_DATA-SAUFT IS INITIAL.
          wa_l_plant-rep_manuf = 'X'.    "��ʶ���ظ���������
*          ELSE.
*            WA_L_PLANT-REP_MANUF = IT_DATA-SAUFT.    "��ʶ���ظ���������
*          ENDIF.
*          IF IT_DATA-SFEPR IS INITIAL.
          wa_l_plant-repmanprof = 'Z001'.    "�ظ���������ļ�
*          ELSE.
*            WA_L_PLANT-REPMANPROF = IT_DATA-SFEPR.    "�ظ���������ļ�
*          ENDIF.

          wa_l_plant-availcheck = 'KP'.    "�����Լ��ļ����

          if it_data-magrv is initial.
            wa_l_client-mat_grp_sm = 'Z002'.   "������İ�װ����
          else.
            wa_l_client-mat_grp_sm = it_data-magrv.	 "������İ�װ����
          endif.
            wa_l_client-pl_ref_mat = it_data-rmatp.   "��װ�Ĳο�����


            set_fieldx it_data-magrv wa_l_clientx-mat_grp_sm .
            "������İ�װ����
            set_fieldx it_data-rmatp wa_l_clientx-pl_ref_mat."��װ�Ĳο�����
            set_fieldx it_data-z_bxqt wa_l_extx-valuepart1+4(1)."��׼��װ��
            set_fieldx it_data-z_nmqt wa_l_extx-valuepart1+5(1)."��װ��ȫ���
            set_fieldx it_data-z_jzkx wa_l_extx-valuepart1+6(1)."��׼������
            set_fieldx it_data-z_gyl  wa_l_extx-valuepart1+7(1)."������
            set_fieldx it_data-z_sycx wa_l_extx-valuepart1+8(1)."ת������
            set_fieldx it_data-z_bxty wa_l_extx-valuepart1+9(1)."����
            set_fieldx it_data-z_mpj wa_l_extx-valuepart1+10(1)."ë�߼���ʶ
            set_fieldx it_data-z_zswl wa_l_extx-valuepart1+11(1)."���ó���


            write it_data-z_bxqt to   "��׼��װ��
            wa_l_ext-valuepart1+4(13) no-gap no-grouping left-justified.
            write it_data-z_nmqt to   "��װ��ȫ���
           wa_l_ext-valuepart1+17(13) no-gap no-grouping left-justified.

            write it_data-z_jzkx to   "��׼������
           wa_l_ext-valuepart1+30(13) no-gap no-grouping left-justified.
            write it_data-z_gyl to   "������
           wa_l_ext-valuepart1+43(13) no-gap no-grouping left-justified.

            write it_data-z_zswl to   "ת������
           wa_l_ext-valuepart1+56(1) no-gap no-grouping left-justified.
            write it_data-z_bxty to   "����
           wa_l_ext-valuepart1+57(18) no-gap no-grouping left-justified.
            write it_data-z_mpj to   "ë�߼���ʶ
           wa_l_ext-valuepart1+75(1) no-gap no-grouping left-justified.
            write it_data-z_sycx+0(164) to   "���ó���
         wa_l_ext-valuepart1+76(164) no-gap no-grouping left-justified.
            write it_data-z_sycx+164(36) to   "���ó���
          wa_l_ext-valuepart2+0(36) no-gap no-grouping left-justified.
            wa_l_plantx-plant =  it_data-werks.    "����
            set_fieldx it_data-dismm wa_l_plantx-mrp_type . "MRP����
            set_fieldx it_data-minbe wa_l_plantx-reorder_pt ."�ٶ�����
            set_fieldx it_data-dispo wa_l_plantx-mrp_ctrler ."MRP������
            set_fieldx it_data-disgr wa_l_plantx-mrp_group.   "MRP��
*            WA_L_PLANTX-MRP_GROUP = C_FIELDX. "MRP��
            set_fieldx it_data-disls wa_l_plantx-lotsizekey . "������С
            set_fieldx it_data-bstfe wa_l_plantx-fixed_lot .  "�̶�����
            set_fieldx it_data-bstmi wa_l_plantx-minlotsize."��С������С
            set_fieldx it_data-bstma wa_l_plantx-maxlotsize."���������С
            set_fieldx it_data-mabst wa_l_plantx-max_stock."�����ˮƽ
            set_fieldx it_data-bstrf wa_l_plantx-round_val ."����ֵ
            set_fieldx it_data-beskz wa_l_plantx-proc_type ."�ɹ�����
            set_fieldx it_data-sobsl wa_l_plantx-spproctype."����ɹ�����
            set_fieldx it_data-lgpro wa_l_plantx-iss_st_loc."�����ִ��ص�
            set_fieldx it_data-fhori wa_l_plantx-sm_key  .   "�ƻ��߼���
            set_fieldx it_data-dzeit wa_l_plantx-inhseprodt."�ڲ�����ʱ��
            set_fieldx it_data-plifz wa_l_plantx-plnd_delry."�ƻ�����ʱ��
            set_fieldx it_data-webaz wa_l_plantx-gr_pr_time."�ջ�����ʱ��
            set_fieldx it_data-eisbe wa_l_plantx-safety_stk ."��ȫ���
            set_fieldx it_data-mrppp wa_l_plantx-ppc_pl_cal ."�ƻ�����
            set_fieldx it_data-shzet wa_l_plantx-safetytime . "��ȫʱ��
*          SET_FIELDX IT_DATA-ALTSL WA_L_PLANTX-ALT_BOM_ID .
            wa_l_plantx-alt_bom_id = c_fieldx."BOMѡ�񷽷�
*          SET_FIELDX IT_DATA-SAUFT WA_L_PLANTX-REP_MANUF ."�ظ���������
*          SET_FIELDX IT_DATA-SFEPR WA_L_PLANTX-REPMANPROF."�ظ���������ļ�
            wa_l_plantx-rep_manuf = c_fieldx ."�ظ���������
            wa_l_plantx-repmanprof = c_fieldx."�ظ���������ļ�
            wa_l_plantx-availcheck = c_fieldx.    "�����Լ��ļ����
            select single mmsta into i_mmsta from marc
              where matnr = it_data-matnr
               and werks = it_data-werks.
            if sy-subrc ne 0.
              wa_l_plant-pur_status = '01'.    "�����ض�������״̬
              wa_l_plantx-pur_status = c_fieldx.    "�����ض�������״̬
            endif.


          endif.

          append wa_l_ext to it_l_ext.
          append wa_l_extx to it_l_extx.

*****�ɹ���ͼ����
*          IF P_PURCH = ABAP_TRUE AND ( NOT IT_DATA-EKGRP IS INITIAL )
*                                 AND ( NOT IT_DATA-KORDB IS INITIAL ) .
          if p_purch = abap_true and
            ( ( not it_data-ekgrp is initial )
                or ( not it_data-kordb is initial ) ) and it_data-ekgrp
                ne c_sline.
            wa_l_head-purchase_view = abap_true.
            wa_l_plant-plant =  it_data-werks.    "����
            wa_l_plant-pur_group = it_data-ekgrp.  "�ɹ���
            wa_l_plant-sourcelist = it_data-kordb.  "Դ�嵥

            wa_l_plantx-plant = it_data-werks ."����
            set_fieldx it_data-ekgrp wa_l_plantx-pur_group."�ɹ���
            set_fieldx it_data-kordb wa_l_plantx-sourcelist."Դ�嵥
            wa_l_head-basic_view = abap_true.
            wa_l_client-matl_group = it_data-matkl.  "������
            set_fieldx it_data-matkl wa_l_clientx-matl_group."������

          endif.

*****����ɱ���ͼ....
          if p_finan = abap_true.
            wa_l_head-account_view = abap_true.
            wa_l_head-cost_view = abap_true.
            wa_l_plant-plant =  it_data-werks.    "����
            wa_l_plant-no_costing = it_data-ncost.   "�޳ɱ�����
            wa_l_plant-variance_key = it_data-awsls.   "������
            wa_l_plant-profit_ctr = it_data-prctr.   "��������
            if  it_data-losgr ne c_sline.
              wa_l_plant-lot_size = it_data-losgr.  "�ɱ���������
            endif.
            wa_l_val-val_area =  it_data-werks.    "����
            wa_l_val-val_class = it_data-bklas.   "������
            wa_l_val-price_ctrl = it_data-vprsv.   "�۸����
            if it_data-peinh ne c_sline.
              wa_l_val-price_unit = it_data-peinh.   "�۸�λ
            endif.
            if it_data-stprs ne c_sline.
              wa_l_val-std_price = it_data-stprs.   "��׼�۸�
            endif.

            wa_l_val-qty_struct = it_data-ekalr.   "��QS�ĳɱ�����
            wa_l_val-orig_mat = it_data-hkmat.           "������Դ
            wa_l_val-orig_group = it_data-hrkft.   "ԭʼ��
            if it_data-zplp1 ne c_sline.
              wa_l_val-plndprice1 = it_data-zplp1.   "�ƻ��۸�1
            endif.
            if it_data-zpld1 ne c_sline.
              wa_l_val-plndprdate1 = it_data-zpld1.  "�ƻ��۸�����1
            endif.
            if it_data-zplp2 ne c_sline.
              wa_l_val-plndprice2 = it_data-zplp2.   "�ƻ��۸�2
            endif.
            if it_data-zpld2 ne c_sline.
              wa_l_val-plndprdate2 = it_data-zpld2.   "�ƻ��۸�����2
            endif.
            clear:i_status, i_pstat.
            select single mmsta pstat into
              (i_mmsta,i_pstat)
              from marc
              where matnr = it_data-matnr
                and werks = it_data-werks.
            if sy-subrc ne 0.
              wa_l_plant-pur_status = '01'.    "�����ض�������״̬
              wa_l_plantx-pur_status = c_fieldx.    "�����ض�������״̬
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
                wa_l_plant-pur_status = '01'.    "�����ض�������״̬
                wa_l_plantx-pur_status = c_fieldx.    "�����ض�������״̬
              endif.
            endif.

            wa_l_valx-val_area =  it_data-werks.    "����
            wa_l_plantx-plant =  it_data-werks.    "����
            set_fieldx it_data-ncost wa_l_plantx-no_costing  . "�޳ɱ�����
            set_fieldx it_data-awsls wa_l_plantx-variance_key  ."������
            set_fieldx it_data-prctr wa_l_plantx-profit_ctr  . "��������
            set_fieldx it_data-losgr wa_l_plantx-lot_size.  "�ɱ���������
            set_fieldx it_data-bklas wa_l_valx-val_class .   "������
            set_fieldx it_data-vprsv wa_l_valx-price_ctrl.   "�۸����
            set_fieldx it_data-peinh wa_l_valx-price_unit.  "�۸�λ
            set_fieldx it_data-stprs wa_l_valx-std_price ."��׼�۸�
            set_fieldx it_data-ekalr wa_l_valx-qty_struct."��QS�ĳɱ�����
            set_fieldx it_data-hkmat wa_l_valx-orig_mat. "������Դ
            set_fieldx it_data-hrkft wa_l_valx-orig_group. "ԭʼ��
            set_fieldx it_data-zplp1 wa_l_valx-plndprice1 ."�ƻ��۸�1
            set_fieldx it_data-zpld1 wa_l_valx-plndprdate1."�ƻ��۸�����1
            set_fieldx it_data-zplp2 wa_l_valx-plndprice2. "�ƻ��۸�2
            set_fieldx it_data-zpld2 wa_l_valx-plndprdate2. "�ƻ��۸�����2

          endif.


          if p_maktl = 'X'.
            wa_l_head-basic_view = abap_true.
            set_fieldx it_data-matkl wa_l_clientx-matl_group."������
            wa_l_client-matl_group = it_data-matkl.  "������
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
      IF P_MAktL = 'X'.""""""����������ʱ���޸���Ϣ����
        LOOP AT it_out WHERE msgty = 'S'.
          READ TABLE IT_DATA WITH KEY MATNR = IT_OUT-MATNR
                                      itemno = IT_OUT-itemno.
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
            EXPORTING
              input  = IT_OUT-MATNR
            IMPORTING
              OUTPUT = LV_MATNR.

          CONCATENATE '���Ϻ�' LV_MATNR
                      '��������' IT_DATA-MATKL
                      '�Ѿ������ɹ�' INTO it_out-message.
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
    translate it_quality-matnr to upper case. " ���Ϻ���
    TRANSLATE it_quality-mtart to UPPER CASE .
    TRANSLATE it_quality-werks to UPPER CASE .
    TRANSLATE it_quality-qmata to UPPER CASE .
    TRANSLATE it_quality-qmpur to UPPER CASE .
    TRANSLATE it_quality-ssqss TO UPPER CASE .
    TRANSLATE it_quality-art   TO UPPER CASE .
    TRANSLATE it_quality-apa   TO UPPER CASE .
    TRANSLATE it_quality-aktiv TO UPPER CASE .
*    �������ֶ�
    check_mandtory it_quality-matnr text-007. " ���Ϻ���.
    check_mandtory it_quality-mtart text-008. " ��������.
    check_mandtory it_quality-werks text-020.  "����
    check_mandtory it_quality-qmata text-040.  "����ҵ�����
    check_mandtory it_quality-art text-041.  "��������
    check_mandtory it_quality-apa text-042.  "��ѡ����
    check_mandtory it_quality-aktiv text-043.  "����ҵ�����
    APPEND it_quality .
    CLEAR it_quality .

  ENDLOOP.

  SORT it_quality by matnr mtart werks .

  LOOP AT  it_quality.
    lwa_quality = it_quality .

    it_out-maktxc = lwa_quality-maktx .
*     ��������Ƿ����
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

*  ����QMAT
    it_qmat-MATERIAL   = lwa_quality-matnr.
    it_qmat-FUNCTION   = '004'.
    it_qmat-INSPTYPE   = lwa_quality-ART.        "��������
    it_qmat-PLANT      = lwa_quality-WERKS.      "����

    it_qmat-IND_INSPTYPE_MAT_ACTIVE
                       = lwa_quality-aktiv.  "�������ͣ����Ϻϲ��Ѽ���
    it_qmat-PREFERRED_INSPTYPE
                       = lwa_quality-APA.         "��ѡ�ļ�������

    it_qmat-qual_score_procedure = '06'.

    CASE lwa_quality-art.
      WHEN cns_qpart_01 or cns_qpart_80 or cns_qpart_81
            or cns_qpart_82 or cns_qpart_83 or cns_qpart_84
            or cns_qpart_90 .
*       �������嵥�ļ���
        it_qmat-ind_insp_with_tsk_list = 'X'.
*       �Զ�������
        it_qmat-ind_auto_assign = 'X'.
*       �����Լ���
        it_qmat-ind_insp_by_charac = 'X'.
*       �����Թ�
        it_qmat-ind_skips_allowed = 'X'.
*       �Զ�ʹ�þ���
        it_qmat-ind_automatic_ud = 'X'.
*       ���ܵ����кŹ���
        it_qmat-ind_single_units_possible = 'X'.

      WHEN cns_qpart_13 .
*       �������嵥�ļ���
        it_qmat-ind_insp_with_tsk_list = 'X'.

*       �����Լ���
        it_qmat-ind_insp_by_charac = 'X'.
*       �����Թ�
        it_qmat-ind_skips_allowed = 'X'.

*       ���ܵ����кŹ���
        it_qmat-ind_single_units_possible = 'X'.


      WHEN OTHERS.
    ENDCASE.
    APPEND it_qmat.
    CLEAR it_qmat .

    AT END OF werks .
      wa_l_head-material = lwa_quality-matnr .
      wa_l_head-ind_sector   = 'M'.       "��ҵ����
      wa_l_head-matl_type    = lwa_quality-mtart.       "��������
      wa_l_head-quality_view = 'X'.            "����������ͼ

      wa_l_plant-PLANT       =  lwa_quality-WERKS.   "����
*      wa_l_plant-CTRL_KEY    =  lwa_quality-SSQSS.   "������
      wa_l_plant-qm_authgrp  =  lwa_quality-qmata . " ����ҵ�����

      wa_l_plantx-PLANT      =  lwa_quality-WERKS.         "����
*      set_fieldx lwa_quality-ssqss wa_l_plantx-ctrl_key ."������
      set_fieldx lwa_quality-qmata wa_l_plantx-qm_authgrp."����ҵ�����



*      wa_l_client-qm_procmnt = lwa_quality-qmpur. "QM�ɹ�����
*      set_fieldx lwa_quality-qmpur wa_l_clientx-qm_procmnt."QM�ɹ�����

* ��������������ͼ
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
