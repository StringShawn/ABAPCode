*&---------------------------------------------------------------------*
*& Report ZJSRPT_004
*&---------------------------------------------------------------------*
*&�������/Transaction code     :
*&��������/Program Name         :ZJSRPT_004
*&��������/Program Des.         :��˰ƽ̨
*&������/Applicant              :
*&��������/Date of App          :2019.9.18
*&������λ/Development Company  :HAND(����)
*&����/Author                   :ZXJ
*&---------------------------------------------------------------------*
*&ժҪ/Abstract:
*&    1).ҵ�񳡾�
*&       ��˰�ϲ����
*&       ��˰�����ٴ����˷ѷ�̯
*&       �����ϲ����
*&       ������ʾ
*&---------------------------------------------------------------------*
*&�����¼/Change record
*&Date              Developer          ReqNo       Descriptions
*& ==========  ==================  ==========  ========================*
*&---------------------------------------------------------------------*
REPORT zjsrpt_004.

*----------------------------------------------------------------------*
* ���ݿ������
*----------------------------------------------------------------------*
TYPE-POOLS:slis.
TABLES:ztvat_data,ztvat_matnr,ztvat_rule,vbrk,ztvat_return,zsvat_yfcl_h,kna1.
*----------------------------------------------------------------------*
* ��������
*----------------------------------------------------------------------*

TYPES:BEGIN OF ty_data,
        box        TYPE c,
        zidnum     TYPE char15, "��Ʊ��
        vbeln      TYPE vbrk-vbeln, "sap��Ʊ��
        zls        TYPE i, "���ڲ����ʱ�洢����
        ztax       TYPE ztvat_data-ztax, "˰��
        zbklas     TYPE char2, "������ǰ�����ַ�
        matnr      TYPE vbrp-matnr, "��������
        posnr      TYPE vbrp-posnr, "����Ŀ

        zsuoyin    TYPE ztvat_data-zsuoyin,
        zposnr     TYPE ztvat_data-zposnr,
        zjszl      TYPE ztvat_data-zjszl, "��Ʊ����
        zkpjh      TYPE ztvat_data-zkpjh, "��Ʊ����
        bukrs      TYPE vbrk-bukrs, "��˾����
        fkdat      TYPE vbrk-fkdat, "��Ʊ����
        fkart      TYPE vbrk-fkart, "��Ʊ����
        vkorg      TYPE vbrk-vkorg, "������֯
        vtweg      TYPE vbrk-vtweg, "��������
        kunrg      TYPE vbrk-kunrg, "�ͻ�
        netwr_k    TYPE vbrk-netwr, "̧ͷ��ֵ
        zstatus    TYPE ztvat_data-zstatus,
        zkhbank    TYPE ztvat_data-zkhbank,
        zkhname    TYPE but000-name_org1, "�ͻ�����
        stcd5      TYPE kna1-stcd5, "�ͻ�˰��
        zkhsh      TYPE kna1-stcd5, "�ͻ�˰��
        zaddress   TYPE char50, "��ַ�绰
        zbank      TYPE char50, "�ͻ������м��˺�
        kunnr      TYPE vbpa-kunnr, "����Ա
        zsaler     TYPE kna1-kunnr, "����Ա
        zwmfph     TYPE vbak-zwmfph, "��ó��Ʊ��
        bklas      TYPE mbew-bklas, "������
        zspmc      TYPE char50, "��Ʒ����
        zggxh      TYPE char50, "����ͺ�
        atwrt      TYPE char50, "�ȼ�
        fkimg      TYPE vbrp-fkimg, "����
        vrkme      TYPE vbrp-vrkme, "������λ
        netwr      TYPE vbrp-netwr, "���
        zbprice    TYPE vbrp-netwr, "����˰����
        zprice     TYPE vbrp-netwr, "����
        zmount     TYPE vbrp-netwr, "���
        zbmount    TYPE vbrp-netwr, "��λ�ҽ��
        zse        TYPE vbrp-mwsbp, "˰��
        waerk      TYPE vbrp-waerk, "����
        zwaerk     TYPE vbrp-waerk, "��λ�ұ���
        kursk      TYPE tcurr-ukurs, "����
        arktx      TYPE vbrp-arktx,
        charg      TYPE vbrp-charg, "����
*        ZTAX     TYPE ZTVAT_DATA-ZTAX,"˰��
        mwsbp      TYPE vbrp-mwsbp,
        werks      TYPE vbrp-werks,
        umvkz      TYPE vbrp-umvkz,
        umvkn      TYPE vbrp-umvkn,
        zbeizhu    TYPE ztvat_data-zbeizhu, "��ע
        zssflbm    TYPE ztvat_matnr-zssflbm, "˰�շ������
        kunnr_z1   TYPE vbpa-kunnr,
        bstkd      TYPE vbkd-bstkd,
        bezei      TYPE tvsakt-bezei, "ó�׷�ʽ
        zzss       TYPE char1, "����˿��ʶ
        zthcd      TYPE char1, "�˻���ֱ�ʶ
        ztxbs      TYPE char1, "��Ϣ��ϲ���ʶ
        ernam      TYPE vbrp-ernam, "
        zjsdm      TYPE ztvat_invoice-zjsdm,
        zjshm      TYPE ztvat_invoice-zjsdm,
        zjsdate    TYPE ztvat_invoice-zjsdate,
        zpost      TYPE ztvat_invoice-zpost,
        zernam     TYPE ztvat_invoice-zernam,
        zerdat     TYPE ztvat_invoice-zerdat,
        zertim     TYPE ztvat_invoice-zertim,
        zaenam     TYPE ztvat_invoice-zaenam,
        zaedat     TYPE ztvat_invoice-zaedat,
        zaetim     TYPE ztvat_invoice-zaetim,
        ztsgz      TYPE char20,       "�������
        tx_flag    TYPE abap_bool,
*        ZERNAM    TYPE VBRP-ERNAM, "
*        ZERDAT    TYPE VBRP-ERDAT, "
*        ZERTIM    TYPE ZTVAT_DATA-ZERTIM, "
        knumv      TYPE vbrk-knumv,
        aubel      TYPE vbrp-aubel,
        rfbsk      TYPE vbrk-rfbsk,
        dd_handle  TYPE int4,
        dd_kpjh    TYPE int4,
        vbeln_line TYPE i,
        msg        TYPE bapi_msg,
        light      TYPE char4,
*        SEL      TYPE C,
      END OF ty_data.


TYPES:BEGIN OF ty_head,
        vbeln TYPE vbrk-vbeln,
        kunrg TYPE vbrk-kunrg,
        bukrs TYPE vbrk-bukrs,
        netwr TYPE vbrk-netwr,
        fkimg TYPE vbrp-fkimg,
      END OF ty_head.


*&---------------------------------------------------------------------*
*& Field-symbols/�ֶδ�����                                            *
*&---------------------------------------------------------------------*
FIELD-SYMBOLS: <dyn_table> TYPE table.



*&---------------------------------------------------------------------*
*& globle/ȫ�ֱ�������
*&---------------------------------------------------------------------*
DATA:gt_data    TYPE TABLE OF ty_data,
     gt_data_nm TYPE TABLE OF ty_data,
     fs_wa_nm   TYPE ty_data,
     wa_data    TYPE ty_data,
     wa_data_nm TYPE ty_data,
     gs_data_nm TYPE ty_data,
     ls_data_nm TYPE ty_data.


DATA:gt_head TYPE TABLE OF ty_head,
     wa_head TYPE ty_head.

DATA:gt_yf_item TYPE TABLE OF zsvat_yfcl_i,
     gs_yf_item TYPE zsvat_yfcl_i.


DATA:gt_rule TYPE TABLE OF ztvat_rule.


DATA:lv_zidnum TYPE string.
DATA:lv_zjssy  TYPE string.
DATA:lt_tabix TYPE TABLE OF sy-tabix.
DATA:wa_ztvat_invoice TYPE ztvat_invoice,
     gt_ztvat_invoice TYPE TABLE OF ztvat_invoice,
     wa_ztvat_data    TYPE ztvat_data,
     gt_ztvat_data    TYPE TABLE OF ztvat_data.

DATA:gv_code TYPE sy-ucomm.

DATA:gv_clsj_kpjh TYPE abap_bool. "���������ݵ�ʱ���޸��� ��Ʊ����

*&SPWIZARD: DECLARATION OF TABLECONTROL 'ZTRCL_YF' ITSELF
CONTROLS: ztrcl_yf TYPE TABLEVIEW USING SCREEN 9000.

*&SPWIZARD: LINES OF TABLECONTROL 'ZTRCL_YF'
DATA:     g_ztrcl_yf_lines  LIKE sy-loopc.

DATA:     ok_code LIKE sy-ucomm.

"��ע�ֶ�
DATA container TYPE REF TO cl_gui_custom_container.
DATA editor TYPE REF TO cl_gui_textedit.
DATA:BEGIN OF lt_beizhu OCCURS 0,
       zbeizhu TYPE ztvat_data-zbeizhu,
     END OF lt_beizhu.

CONSTANTS:g_tx_matnr TYPE mara-matnr VALUE '000000000069999997'.

*&---------------------------------------------------------------------*
* DEFINITION
*&---------------------------------------------------------------------*


DEFINE init_fieldcat.      "  ALV Fieldcat Setting

  gs_lvc-fieldname  = &1.
  gs_lvc-seltext_l  = &2.
  gs_lvc-outputlen  = &3.
  gs_lvc-checkbox   = &4.
  gs_lvc-edit       = &5.
  gs_lvc-hotspot    = &6.
  gs_lvc-decimals_out   = &7.
  gs_lvc-key        = &8.
  gs_lvc-intlen     = &9.

*  IF &1 = 'ZJSZL'.
*   gs_lvc-drdn_FIELD = 'DROP_DOWN_HANDLE'.
*  ENDIF.

  APPEND gs_lvc TO gt_lvc.
  CLEAR gs_lvc.

END-OF-DEFINITION.

DEFINE m_sort.
  ADD 1 TO gs_sort-spos.
  gs_sort-fieldname = &1.
  gs_sort-up        = 'X'.
  gs_sort-subtot    = &2.
  APPEND gs_sort TO gt_sort.
END-OF-DEFINITION.

DEFINE set_fieldcat.

  gt_fieldcat-fieldname = &1.
  gt_fieldcat-coltext   = &2.
  gt_fieldcat-ref_table = &3.
  gt_fieldcat-ref_field = &4.
  gt_fieldcat-edit      = &5.
  gt_fieldcat-hotspot   = &6.
  gt_fieldcat-intlen    = '40'.
*  IF &1 = 'BOX'.
*    GT_FIELDCAT-CHECKBOX = 'X'.
*  ENDIF.

  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
DATA: gt_lvc           TYPE slis_t_fieldcat_alv,
      gt_sort          TYPE slis_t_sortinfo_alv,
      gs_layout        TYPE slis_layout_alv,   "alv�ĸ�ʽ
      wa_layout        TYPE lvc_s_layo,   "alv�ĸ�ʽ
      gs_grid_settings TYPE lvc_s_glay,
      gs_variant       TYPE disvariant,
      gs_lvc           TYPE slis_fieldcat_alv,
      gt_fieldcat      LIKE TABLE OF lvc_s_fcat WITH HEADER LINE,

*����洢�����б������
      gt_ddval         TYPE lvc_t_drop,
      gw_ddval         TYPE lvc_s_drop,

      gt_events        TYPE slis_t_event,
      gw_events        TYPE slis_alv_event.

*--������
DATA: lt_dropdown TYPE lvc_t_drop,
      ls_dropdown TYPE lvc_s_drop.

DATA:l_grid    TYPE REF TO cl_gui_alv_grid.
*&---------------------------------------------------------------------*
* Selection Screen
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-005.
PARAMETERS: p1 TYPE c RADIOBUTTON GROUP g1 USER-COMMAND uc DEFAULT 'X',
            p2 TYPE c RADIOBUTTON GROUP g1,
            p3 TYPE c RADIOBUTTON GROUP g1,
            p4 TYPE c RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
  s_bukrs   FOR ztvat_data-bukrs MODIF ID ty1 DEFAULT '3500' OBLIGATORY."��˾����
PARAMETERS:p_vtweg TYPE vbrk-vtweg MODIF ID ty1 DEFAULT '10'."OBLIGATORY."��������
SELECT-OPTIONS:
  s_fkdat    FOR vbrk-fkdat MODIF ID ty1,"���߷�Ʊ����
  s_fkart  FOR vbrk-fkart  MODIF ID ty1 DEFAULT 'ZF1',"��Ʊ����
  s_vbeln   FOR vbrk-vbeln MODIF ID ty1,"SAP��Ʊ��
  s_zsale   FOR kna1-kunnr MODIF ID ty1,"����Ա
  s_kunrg   FOR vbrk-kunrg MODIF ID ty1."�ͻ�
*  S_ZRULE   FOR ZTVAT_RULE-ZRULE MODIF ID TY1."�ϲ���ֹ���
PARAMETERS:p_zss  AS CHECKBOX USER-COMMAND gy1 MODIF ID ty1.

SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
  s_bukrs2   FOR ztvat_data-bukrs MODIF ID ty2 DEFAULT '3500'  OBLIGATORY."��˾����
PARAMETERS:p_vtweg2 TYPE vbrk-vtweg MODIF ID ty2 DEFAULT '10' OBLIGATORY."��������
SELECT-OPTIONS:
*  S_FKDAT2    FOR VBRK-FKDAT MODIF ID TY2,"���߷�Ʊ����
  s_zid2      FOR ztvat_data-zidnum MODIF ID ty2,"��Ʊ����
  s_zsale2   FOR ztvat_data-zsaler MODIF ID ty2,"����Ա
  s_kunrg2   FOR vbrk-kunrg MODIF ID ty2,"�ͻ�
  s_wmfph2   FOR ztvat_data-zwmfph MODIF ID ty2. "��ó��Ʊ��
SELECTION-SCREEN END OF BLOCK blk2.

SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
  s_bukrs3   FOR ztvat_data-bukrs MODIF ID ty3 DEFAULT '3500' OBLIGATORY."��˾����
PARAMETERS:p_vtweg3 TYPE vbrk-vtweg MODIF ID ty3 DEFAULT '10' OBLIGATORY."��������
SELECT-OPTIONS:
*  S_FKDAT3    FOR VBRK-FKDAT MODIF ID TY3,"���߷�Ʊ����
  s_zid3      FOR ztvat_data-zidnum MODIF ID ty3,"��Ʊ����
  s_zsale3   FOR ztvat_data-zsaler MODIF ID ty3,"����Ա
  s_kunrg3   FOR vbrk-kunrg MODIF ID ty3, "�ͻ�
  s_wmfph3   FOR ztvat_data-zwmfph MODIF ID ty3. "��ó��Ʊ��
SELECTION-SCREEN END OF BLOCK blk3.

SELECTION-SCREEN BEGIN OF BLOCK blk4 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
  s_zid4     FOR ztvat_data-zidnum MODIF ID ty4,"��Ʊ����
  s_vbeln4   FOR vbrk-vbeln MODIF ID ty4,"SAP��Ʊ��
  s_zjsdm4   FOR ztvat_return-zjsdm MODIF ID ty4,"��˰��Ʊ����
  s_zjshm4   FOR ztvat_return-zjshm MODIF ID ty4,"��˰��Ʊ����
  s_zdate4   FOR ztvat_return-zjsdate MODIF ID ty4."��˰��Ʊ����
PARAMETERS:p_gl  AS CHECKBOX USER-COMMAND gy1 MODIF ID ty4.
SELECTION-SCREEN END OF BLOCK blk4.

*&-----------------------------------------------------------------*
*& ��ʼ������
*&-----------------------------------------------------------------*
INITIALIZATION.
*&-----------------------------------------------------------------*
*& ѡ����Ļ����
*&-----------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'TY1'.
        IF p1 IS  INITIAL.
          screen-active = '0'.
        ELSE.
          screen-active = '1'.
        ENDIF.

      WHEN 'TY2'.
        IF p2 IS  INITIAL.
          screen-active = '0'.
        ELSE.
          screen-active = '1'.
        ENDIF.

      WHEN 'TY3'.
        IF p3 IS  INITIAL.
          screen-active = '0'.
        ELSE.
          screen-active = '1'.
        ENDIF.

      WHEN 'TY4'.
        IF p4 IS  INITIAL.
          screen-active = '0'.
        ELSE.
          screen-active = '1'.
        ENDIF.

    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.



*&-----------------------------------------------------------------*
*& ����������
*&-----------------------------------------------------------------*
AT SELECTION-SCREEN.


*&-----------------------------------------------------------------*
*& ����ʼ����
*&-----------------------------------------------------------------*
START-OF-SELECTION.
* get output data
  IF p1 EQ 'X'."�ϲ����
    PERFORM frm_getdata_hbcf.
    PERFORM frm_dealdata_hbcf.
    PERFORM frm_display_hbcf.

  ELSEIF p2 EQ 'X'."���ݴ���
    PERFORM frm_getdata_sjcl.
    PERFORM frm_display_sjcl.

  ELSEIF p3 EQ 'X'."�������ݴ���

    PERFORM frm_getdata_cxsj.
    PERFORM frm_display_cxsj.

  ELSEIF p4 EQ 'X'."��˰��Ʊ��Ӧ����

    PERFORM frm_getdata_jsbb.
    PERFORM frm_display_jsbb."

  ENDIF.
*&---------------------------------------------------------------------*
*& Form FRM_GETDATA_HBCF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_getdata_hbcf .

  DATA:BEGIN OF ls_ausp,
         atwrt TYPE ztpp_004-zhnl,
         matnr TYPE mch1-matnr,
         charg TYPE mch1-charg,
       END OF ls_ausp.

  DATA:  zausp LIKE TABLE OF ls_ausp WITH HEADER LINE.

  DATA:BEGIN OF ls_tpp4,
         matnr TYPE ztpp_004-matnr,
         zswh  TYPE ztpp_004-zswh,
         zhanh TYPE ztpp_004-zhanh,
         zhnl  TYPE ztpp_004-zhnl,
       END OF ls_tpp4.

  DATA:ztpp4  LIKE TABLE OF ls_tpp4 WITH HEADER LINE .
  DATA:ztpp5  LIKE TABLE OF ls_tpp4 WITH HEADER LINE.

  SELECT
    vbrk~bukrs
    vbrk~fkdat
    vbrk~fkart
    vbrk~vbeln
    vbrk~rfbsk
    vbrp~posnr
    vbrk~vkorg
    vbrk~vtweg
    vbrk~kunrg
    vbrk~ernam
    vbrk~knumv
    vbrk~netwr AS netwr_k
    vbrp~aubel
    vbrp~matnr
    vbrp~werks
    vbrp~arktx
    vbrp~charg
    vbrp~fkimg
    vbrp~vrkme
    vbrp~netwr
    vbrp~mwsbp
    vbrp~waerk
    vbrp~umvkz
    vbrp~umvkn
    vbpa~kunnr AS kunnr_z1
    INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM vbrk
    JOIN vbrp ON vbrk~vbeln = vbrp~vbeln
    JOIN vbpa ON vbrp~aubel = vbpa~vbeln
*    JOIN ZTVAT_INVOICE ON ZTVAT_INVOICE~ZIDNUM = VBRK~VBELN
    WHERE fkart IN ( 'ZF0','ZF1','ZF9','ZFA','ZG2','ZIG','ZIV','ZIV1','ZIVG','ZL2','ZRE','ZREA' )
      AND fkdat IN s_fkdat
      AND fkart IN s_fkart
      AND vbrk~vbeln IN s_vbeln
      AND bukrs IN s_bukrs
      AND kunrg IN s_kunrg
      AND vtweg = p_vtweg
      AND vbpa~parvw = 'Z1'
      AND vbpa~kunnr IN s_zsale
      AND rfbsk = 'A'.


*ɸ��sap��Ʊ��������Ʊ���ҽ����е�����
*  SELECT ZTVAT_INVOICE~ZIDNUM, ZSTATUS
*    INTO TABLE @DATA(GT_INVOICE)
*    FROM ZTVAT_INVOICE
*    JOIN ZTVAT_DATA ON ZTVAT_INVOICE~ZIDNUM = ZTVAT_DATA~ZIDNUM
*   WHERE ZSTATUS NE '50'.
*
*  LOOP AT GT_INVOICE ASSIGNING FIELD-SYMBOL(<FS_INVOICE>).
*    <FS_INVOICE>-ZIDNUM =  |{ <FS_INVOICE>-ZIDNUM ALPHA = OUT }|.
*  ENDLOOP.



  IF gt_data IS NOT INITIAL.
*----ɸ������
    SELECT vbeln,zidnum
      INTO TABLE @DATA(gt_invoice)
      FROM ztvat_invoice
       FOR ALL ENTRIES IN @gt_data
     WHERE vbeln = @gt_data-vbeln.

    SORT gt_invoice BY vbeln .


*----�ͻ�����
    SELECT
      partner,
      name_org1
      INTO TABLE @DATA(gt_but000)
      FROM but000
      FOR ALL ENTRIES IN @gt_data
      WHERE partner = @gt_data-kunrg.
    SORT gt_but000 BY partner.

*----�ͻ�˰�š���ַ�绰
    SELECT
      kna1~stcd5,
      kna1~kunnr,
      kna1~stras,
      kna1~telf1
*      kna1~faksd
      INTO TABLE @DATA(gt_kna1)
      FROM kna1
      FOR ALL ENTRIES IN @gt_data
      WHERE kunnr = @gt_data-kunrg.
    SORT gt_kna1 BY kunnr.

*-----�ͻ�����
    SELECT
      kunnr,
      vkorg,
      vtweg,
      spart,
      faksd
      INTO TABLE @DATA(gt_knvv)
      FROM knvv
      FOR ALL ENTRIES IN @gt_data
      WHERE kunnr = @gt_data-kunrg.
    SORT gt_knvv BY kunnr.

*----�ͻ������м��˺�
    SELECT
      knbk~kunnr,
      knbk~bankl,
      knbk~bankn,
      bnka~banka
      INTO TABLE @DATA(gt_knbk)
      FROM knbk
      JOIN bnka ON knbk~bankl = bnka~bankl
      FOR ALL ENTRIES IN @gt_data
      WHERE kunnr = @gt_data-kunrg
        AND bnka~banks = 'CN '.
    SORT gt_knbk BY kunnr.

*----��ó��Ʊ��
    SELECT
      vbak~vbeln,
      vbak~zwmfph
      INTO TABLE @DATA(gt_vbak)
      FROM vbak
      FOR ALL ENTRIES IN @gt_data
      WHERE vbeln = @gt_data-aubel.
    SORT gt_vbak BY vbeln.

*----����Ա
*    SELECT
*      vbeln,
*      vbpa~kunnr,
*      parvw
*      INTO TABLE @DATA(gt_vbpa)
*      FROM vbpa
*      FOR ALL ENTRIES IN @gt_data
*      WHERE vbeln = @gt_data-aubel
*        AND parvw = 'Z1'.
*    SORT gt_vbpa BY vbeln.
*    IF gt_vbpa IS NOT INITIAL.
    SELECT
      kunnr,
      name1
      INTO TABLE @DATA(gt_kna1sh)
      FROM kna1
      FOR ALL ENTRIES IN @gt_data
      WHERE kunnr = @gt_data-kunnr_z1.
    SORT gt_kna1sh BY kunnr.
*    ENDIF.

*----������
    SELECT
      mbew~matnr,
      mbew~bwkey,
      mbew~bklas
      INTO TABLE @DATA(gt_mbew)
      FROM mbew
      FOR ALL ENTRIES IN @gt_data
      WHERE matnr = @gt_data-matnr
        AND bwkey = @gt_data-werks.
    SORT gt_mbew BY matnr bwkey.

*----�ȼ�
    DATA:lv_atinn TYPE ausp-atinn.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'Z_DATA02'
      IMPORTING
        output = lv_atinn.

    SELECT
      mch1~matnr,
      mch1~charg,
      ausp~objek,
      ausp~atwrt,
      ausp~atinn
      INTO TABLE @DATA(gt_ausp)
      FROM ausp
      JOIN mch1 ON ausp~objek = mch1~cuobj_bm
      FOR ALL ENTRIES IN @gt_data
      WHERE matnr = @gt_data-matnr
        AND atinn = @lv_atinn.
    SORT gt_ausp BY matnr charg.

*----˰��
*    SELECT
*      knumv,
*      kposn,
*      kbetr
*      INTO TABLE @DATA(gt_prcd_elements)
*      FROM prcd_elements
*      FOR ALL ENTRIES IN @gt_data
*      WHERE knumv = @gt_data-knumv
*        AND kposn = @gt_data-posnr
*        AND kschl = 'MWSI'.

    "��˰����
    SELECT * INTO TABLE @DATA(lt_prcd_elements)
    FROM prcd_elements
    FOR ALL ENTRIES IN @gt_data
    WHERE knumv = @gt_data-knumv.
    SORT lt_prcd_elements BY knumv.

    "����ͺ�
    SELECT matnr,
           zgg
    INTO TABLE @DATA(lt_zgg)
    FROM ztmdm_mat_log
    FOR ALL ENTRIES IN @gt_data
    WHERE matnr = @gt_data-matnr.
    SORT lt_zgg BY matnr.

    SELECT
      mch1~matnr,
      mch1~charg,
      ausp~atwrt
      INTO CORRESPONDING FIELDS OF TABLE @zausp
      FROM mch1
      LEFT JOIN ausp ON mch1~cuobj_bm = ausp~objek
      FOR ALL ENTRIES IN @gt_data
      WHERE  charg = @gt_data-charg
      AND matnr = @gt_data-matnr
      AND ausp~atinn = '0000000814'.
    SORT zausp BY matnr charg.
    IF zausp[] IS NOT INITIAL.
      SELECT matnr zhnl zswh zhanh
        INTO CORRESPONDING FIELDS OF TABLE ztpp4
        FROM ztpp_004
        FOR ALL ENTRIES IN zausp
        WHERE matnr = zausp-matnr
        AND zhxm = '24����'
        AND zhnl = zausp-atwrt.
      SORT ztpp4 BY matnr zhnl.
      IF ztpp4[] IS NOT INITIAL.
        SELECT zswh zhanh  zhnl
          INTO CORRESPONDING FIELDS OF TABLE ztpp5
          FROM ztpp_004
          FOR ALL ENTRIES IN ztpp4
          WHERE zswh = ztpp4-zswh
          AND zhanh = ztpp4-zhanh
          AND zhxm = '���'.
        SORT ztpp5 BY zswh zhanh.
      ENDIF.
    ENDIF.
  ENDIF.

*ɸ������˿
*    IF P_ZSS IS NOT INITIAL.
  SELECT
    matnr,
    charg,
    zcpmcz
    INTO TABLE @DATA(gt_ztmm_eos_001_log)
    FROM ztmm_eos_001_log
    FOR ALL ENTRIES IN @gt_data
    WHERE matnr = @gt_data-matnr
      AND charg = @gt_data-charg.
  SORT gt_ztmm_eos_001_log BY matnr charg.


  "ȡ�������
  SELECT bukrs,
         kunrg,
         zdk,
         ztxdk,
         ztxhbhk,
         zkpxe
    INTO CORRESPONDING FIELDS OF TABLE @gt_rule
    FROM ztvat_rule.
  SORT gt_rule BY bukrs kunrg.

  "ȡ˰�շ������
  SELECT *
  INTO TABLE @DATA(gt_ssfl)
  FROM ztvat_matnr.

*    ENDIF.

*-----���ݴ���
  LOOP AT gt_data INTO wa_data.
*--�ų�����
    READ TABLE gt_invoice ASSIGNING FIELD-SYMBOL(<fs_invoice>) WITH KEY vbeln = wa_data-vbeln BINARY SEARCH.
    IF sy-subrc = 0.
      DELETE gt_data.
      CONTINUE.
    ENDIF.
*--�ͻ�˰��
    READ TABLE gt_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>) WITH KEY kunnr = wa_data-kunrg BINARY SEARCH.
    IF sy-subrc = 0.
      IF wa_data-vtweg = '10'.
        wa_data-zkhsh = <fs_kna1>-stcd5.
        wa_data-zaddress = <fs_kna1>-stras && <fs_kna1>-telf1.
      ENDIF.
    ENDIF.

*--�ͻ�����
    LOOP AT gt_knvv ASSIGNING FIELD-SYMBOL(<fs_knvv>) WHERE kunnr = wa_data-kunrg AND faksd IS NOT INITIAL.
      DATA(faksd_flag) = abap_true.
      EXIT.
    ENDLOOP.
    IF faksd_flag = abap_true.
      DELETE gt_data.
      CONTINUE.
    ENDIF.

*����˿
    IF p_zss IS NOT INITIAL.
      READ TABLE gt_ztmm_eos_001_log ASSIGNING FIELD-SYMBOL(<fs_zss>) WITH KEY matnr = wa_data-matnr charg = wa_data-charg BINARY SEARCH.
      IF sy-subrc = 0.
        IF <fs_zss>-zcpmcz CS '����'."��������˿
          DELETE gt_data.
          CONTINUE.
        ENDIF.
      ENDIF.
    ELSE.
      READ TABLE gt_ztmm_eos_001_log ASSIGNING <fs_zss> WITH KEY matnr = wa_data-matnr charg = wa_data-charg BINARY SEARCH  .
      IF sy-subrc = 0.
        IF <fs_zss>-zcpmcz CS '����'."��������˿
          wa_data-zzss = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

*--��Ʊ����
    IF wa_data-vtweg = '10'.
      wa_data-zjszl = 'רƱ'.
    ELSEIF wa_data-vtweg = '20'.
      wa_data-zjszl = '��Ʊ'.
    ENDIF.

*--��Ʊ����
    wa_data-zkpjh = '100'.

*--���
    IF wa_data-matnr = g_tx_matnr.
      wa_data-light = icon_led_yellow.
    ENDIF.
    CASE wa_data-fkart.
      WHEN 'ZIG' OR 'ZIVG' OR 'ZRE' OR 'ZREA' OR 'ZG2' .
        wa_data-zmount = wa_data-netwr * -1.
        wa_data-zse = - wa_data-mwsbp.
        wa_data-light  = icon_led_red.
      WHEN OTHERS.
        wa_data-zse = wa_data-mwsbp.
        wa_data-zmount = wa_data-netwr.
    ENDCASE.

    IF wa_data-light IS INITIAL.
      wa_data-light = icon_led_green.
    ENDIF.

*--�ͻ�����
    READ TABLE gt_but000 ASSIGNING FIELD-SYMBOL(<fs_but000>) WITH KEY partner = wa_data-kunrg BINARY SEARCH.
    IF sy-subrc = 0.
      wa_data-zkhname = <fs_but000>-name_org1.
    ENDIF.
*--�ͻ������м��˺�
    READ TABLE gt_knbk ASSIGNING FIELD-SYMBOL(<fs_knbk>) WITH KEY kunnr = wa_data-kunrg BINARY SEARCH.
    IF sy-subrc = 0 AND wa_data-vtweg = '10'.
      wa_data-zbank = <fs_knbk>-banka && <fs_knbk>-bankn.
    ENDIF.

*--��ó��Ʊ��
    READ TABLE gt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>) WITH KEY vbeln = wa_data-aubel BINARY SEARCH.
    IF sy-subrc = 0.
      wa_data-zwmfph = <fs_vbak>-zwmfph.
    ENDIF.

*--����Ա
*    READ TABLE gt_vbpa ASSIGNING FIELD-SYMBOL(<fs_vbpa>)  WITH KEY vbeln = wa_data-aubel BINARY SEARCH.
*    IF sy-subrc = 0.
    READ TABLE gt_kna1sh ASSIGNING FIELD-SYMBOL(<fs_kna1sh>) WITH KEY kunnr = wa_data-kunnr_z1 BINARY SEARCH.
    IF sy-subrc = 0.
      wa_data-zsaler = <fs_kna1sh>-name1.
    ENDIF.
*    ENDIF.

*--������
    READ TABLE gt_mbew ASSIGNING FIELD-SYMBOL(<fs_mbew>) WITH KEY matnr = wa_data-matnr bwkey = wa_data-werks BINARY SEARCH.
    IF sy-subrc = 0.
      wa_data-bklas = <fs_mbew>-bklas.
      wa_data-zbklas = <fs_mbew>-bklas+0(2).
    ENDIF.
*--��Ʒ����
    SEARCH wa_data-arktx FOR '\'.
    IF sy-subrc = 0.
      SPLIT wa_data-arktx AT '\' INTO wa_data-zspmc wa_data-zggxh.
    ELSE.
      wa_data-zspmc = wa_data-arktx.
    ENDIF.

    SELECT SINGLE ktgrm INTO @DATA(lv_ktgrm) FROM mvke
  WHERE matnr = @wa_data-matnr
    AND vkorg = @wa_data-vkorg
    AND vtweg = @wa_data-vtweg.
    IF lv_ktgrm = '11' OR lv_ktgrm = '12' OR lv_ktgrm = '13' OR lv_ktgrm = '14' OR lv_ktgrm = '15'.
      IF wa_data-vtweg = '10'.
        IF wa_data-zzss = 'X'.
          wa_data-zspmc = '��������˿' && wa_data-zspmc.
        ELSE.
          wa_data-zspmc = '����˿' && wa_data-zspmc.
        ENDIF.

      ELSEIF wa_data-vtweg = '20'.
        wa_data-zspmc = '���ڳ�˿' && wa_data-zspmc.
      ENDIF.

    ENDIF.
*--����ͺ�
    READ TABLE lt_zgg INTO DATA(ls_zgg) WITH KEY matnr = wa_data-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      wa_data-zggxh = ls_zgg-zgg.
    ENDIF.

    IF wa_data-zggxh IS INITIAL.
      READ TABLE zausp WITH KEY matnr = wa_data-matnr
                                charg = wa_data-charg BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE ztpp4 WITH KEY matnr = zausp-matnr
                                  zhnl = zausp-atwrt BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE ztpp5 WITH KEY zswh = ztpp4-zswh
                                    zhanh = ztpp4-zhanh BINARY SEARCH.
          IF sy-subrc = 0.
            wa_data-zggxh = ztpp5-zhnl.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*--�ȼ�
    READ TABLE gt_ausp ASSIGNING FIELD-SYMBOL(<fs_ausp>) WITH KEY matnr = wa_data-matnr charg = wa_data-charg BINARY SEARCH.
    IF sy-subrc = 0.
      wa_data-atwrt = <fs_ausp>-atwrt.
    ENDIF.

*--����
    DATA:lv_datum_first TYPE sy-datum.
    lv_datum_first = sy-datum+0(6) && '01'.
    CALL FUNCTION 'READ_EXCHANGE_RATE'
      EXPORTING
        client           = sy-mandt
        date             = lv_datum_first
        foreign_currency = wa_data-waerk
        local_currency   = 'CNY'
      IMPORTING
        exchange_rate    = wa_data-kursk
      EXCEPTIONS
        no_rate_found    = 1
        no_factors_found = 2
        no_spread_found  = 3
        derived_2_times  = 4
        overflow         = 5
        zero_rate        = 6
        OTHERS           = 7.

*--��λ���֡����
    wa_data-zwaerk = 'CNY'.
    IF wa_data-waerk NE wa_data-zwaerk.
      wa_data-zbmount = wa_data-zmount * wa_data-kursk.
    ELSE.
      wa_data-zbmount = wa_data-zmount.
    ENDIF.
*--����
    wa_data-zbprice = wa_data-zbmount / wa_data-fkimg.

*--��λ
    IF wa_data-vrkme = 'KG'.
      wa_data-vrkme = 'ǧ��'.
    ENDIF.

    "˰�շ������
    LOOP AT gt_ssfl INTO DATA(ls_ssfl).
      IF wa_data-arktx+0(5) CS ls_ssfl-zspmc.
        wa_data-zssflbm = ls_ssfl-zssflbm.
        EXIT.
      ENDIF.
    ENDLOOP.

*--��ע
    DATA:n TYPE i.
    DATA:lv_beizhu TYPE char100.
    IF wa_data-vtweg = '10'.
      SELECT
        vbkd~vbeln,
        vbkd~bstkd
        INTO TABLE @DATA(gt_vbkd)
        FROM vbkd
        WHERE vbeln = @wa_data-aubel.
      SORT gt_vbkd BY vbeln bstkd.
      DELETE ADJACENT DUPLICATES FROM gt_vbkd COMPARING ALL FIELDS.
      READ TABLE gt_vbkd ASSIGNING FIELD-SYMBOL(<fs_vbkd>) WITH KEY vbeln = wa_data-aubel.
      IF sy-subrc = 0.
        wa_data-bstkd = <fs_vbkd>-bstkd.
      ENDIF.

      IF wa_data-bstkd IS INITIAL.
        wa_data-bstkd = wa_data-aubel.
      ENDIF.
      wa_data-zbeizhu = wa_data-bstkd .
    ENDIF.
*--ó�׷�ʽ
    SELECT SINGLE sdabw INTO @DATA(lv_sdabw) FROM vbkd WHERE vbeln = @wa_data-aubel.
    IF sy-subrc = 0.
      SELECT SINGLE bezei INTO wa_data-bezei FROM tvsakt WHERE sdabw = lv_sdabw AND spras = '1'.
    ENDIF.
*--˰��


*--ZTAX˰��

    READ TABLE lt_prcd_elements INTO DATA(ls_prcd_element) WITH KEY knumv = wa_data-knumv  kposn = wa_data-posnr kschl = 'MWSI' .
    IF sy-subrc = 0.
      wa_data-ztax = ls_prcd_element-kbetr / 100."˰��
    ENDIF.

    "����
    LOOP AT lt_prcd_elements INTO ls_prcd_element WHERE knumv = wa_data-knumv AND kposn = wa_data-posnr AND
     ( kschl = 'ZPRD'
     OR kschl = 'ZPR9'
     OR kschl = 'ZPR0'
     OR kschl = 'ZPR1'
     OR kschl = 'ZPI1'
     OR kschl = 'PBXX'
     OR kschl = 'ZPRA'
     OR kschl = 'PB00' ) AND kbetr NE 0  .
      IF ls_prcd_element-kpein NE 0.
        wa_data-zprice = ls_prcd_element-kbetr / ls_prcd_element-kpein * wa_data-umvkz / wa_data-umvkn."��˰����
      ELSE.
        wa_data-zprice = ls_prcd_element-kbetr * wa_data-umvkz / wa_data-umvkn."��˰����
      ENDIF.

      IF ls_prcd_element-kschl = 'ZPRA'."��óʱ

        wa_data-zprice = wa_data-zprice * wa_data-kursk .

      ENDIF.
    ENDLOOP.

    "�������
    READ TABLE gt_rule ASSIGNING FIELD-SYMBOL(<fs_rule>) WITH KEY bukrs = wa_data-bukrs kunrg = wa_data-kunrg BINARY SEARCH.
    IF sy-subrc = 0.
      CASE 'X'.
        WHEN <fs_rule>-zdk.
          wa_data-ztsgz = '����'.
        WHEN <fs_rule>-ztxdk.
          wa_data-ztsgz = '��Ϣ����'.
        WHEN <fs_rule>-ztxhbhk.
          wa_data-ztsgz = '��Ϣ�ϲ�����'.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    "  ��Ϣ��ʶ
    IF wa_data-matnr = g_tx_matnr.
      wa_data-tx_flag = abap_true.
    ENDIF.

    MODIFY gt_data FROM wa_data.
    CLEAR:lv_ktgrm,wa_data,lv_datum_first,lv_sdabw.
  ENDLOOP.

  SORT gt_data BY kunrg zwmfph vbeln matnr zspmc zggxh atwrt zbmount.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DEALDATA_HBCF
*&---------------------------------------------------------------------*
*& ���ݴ���
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_dealdata_hbcf .

  LOOP AT gt_data INTO wa_data.
    CALL FUNCTION 'ENQUEUE_EZLJS001'
      EXPORTING
*       MODE_VBRP      = 'E'
*       MANDT          = SY-MANDT
        vbeln          = wa_data-vbeln
        posnr          = wa_data-posnr
*       X_VBELN        = ' '
*       X_POSNR        = ' '
*       _SCOPE         = '2'
*       _WAIT          = ' '
*       _COLLECT       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDLOOP.





ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DISPLAY_HBCF
*&---------------------------------------------------------------------*
*& �ϲ����
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_hbcf .
  PERFORM frm_layout.             "���������ʽ
  PERFORM frm_fieldcat.
*  PERFORM FRM_SORT.               "�������򡢺ϼ�
  PERFORM frm_output.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_LAYOUT
*&---------------------------------------------------------------------*
*& �ϲ����
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_layout .
*  GS_LAYOUT-CWIDTH_OPT = 'X'.
  gs_layout-colwidth_optimize = 'X'.
*  GS_LAYOUT-SEL_MODE   = 'A'.
  gs_layout-edit_mode   = 'A'.
  gs_layout-box_fieldname  = 'BOX'.  "ѡ����ֶ���
  gs_grid_settings-edt_cll_cb  = 'X' .   "��ʾ����ɱ༭�ֶ����޸������ݣ��س���ͻ��������ڱ������Ҳ�޸�
  gs_layout-info_fieldname = 'CLR'.
  gs_layout-cell_merge = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_FIELDCAT
*&---------------------------------------------------------------------*
*& �ϲ����
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_fieldcat .

  CLEAR:gt_lvc.

*  INIT_FIELDCAT 'SEL'    '��ѡ��' '' 'X' 'X' 'X' '' 'X' ''.
  init_fieldcat 'LIGHT' 'ָʾ��' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZIDNUM' '��Ʊ����' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZJSZL'  '��Ʊ����' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZKPJH'  '��Ʊ����' '' '' ''  '' '' '' ''.
  init_fieldcat 'BUKRS'  '��˾����' '' '' ''  '' '' '' ''.
  init_fieldcat 'FKDAT'  '��Ʊ����' '' '' ''  '' '' '' ''.
  init_fieldcat 'FKART'  '��Ʊ����' '' '' ''  '' '' '' ''.
  init_fieldcat 'VBELN'  'SAP��Ʊ��' '' '' ''  '' '' '' ''.
  init_fieldcat 'POSNR'  '����Ŀ' '' '' ''  '' '' '' ''.
  init_fieldcat 'VKORG'  '������֯' '' '' ''  '' '' '' ''.
  init_fieldcat 'VTWEG'  '��������' '' '' ''  '' '' '' ''.
  init_fieldcat 'KUNRG'  '�ͻ�' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZKHNAME'  '�ͻ�����' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZKHSH'  '�ͻ�˰��' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZADDRESS'  '�ͻ���ַ�绰' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZBANK'  '�ͻ������м��˺�' '' '' ''  '' '' '' ''.
  init_fieldcat 'ERNAM'  '��ƱԱ' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZSALER'  '����Ա' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZWMFPH'  '��ó��Ʊ��' '' '' ''  '' '' '' ''.
  init_fieldcat 'MATNR'  '���ϱ���' '' '' ''  '' '' '' ''.
  init_fieldcat 'BKLAS'  '������' '' '' ''  '' '' '' ''.
  init_fieldcat 'ARKTX'  '��������' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZSPMC'  '��Ʒ����' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZGGXH'  '����ͺ�' '' '' ''  '' '' '' ''.
  init_fieldcat 'ATWRT'  '�ȼ�' '' '' ''  '' '' '' ''.
  init_fieldcat 'FKIMG'  '����' '' '' ''  '' '' '' ''.
  init_fieldcat 'VRKME'  '������λ' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZPRICE'  '��˰����' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZBPRICE'  '����˰����' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZMOUNT'  'ƾ֤���ҽ��' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZBMOUNT'  '��λ�ҽ��' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZSE'  '˰��' '' '' ''  '' '' '' ''.
  init_fieldcat 'KURSK'  '����' '' '' ''  '' '' '' ''.
  init_fieldcat 'WAERK'  '����' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZWAERK'  '��λ����' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZTAX'  '˰��' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZBEIZHU'  '��ע' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZSSFLBM'  '˰�շ������' '' '' ''  '' '' '' ''.
  init_fieldcat 'BEZEI'  'ó�׷�ʽ' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZZSS'  '����˿��ʶ' '' '' ''  '' '' '' ''.
  init_fieldcat 'ZTSGZ' '�������' '' '' ''  '' '' '' ''.
  init_fieldcat 'MSG'    '��Ϣ' '' '' ''  '' '' '' ''.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_OUTPUT
*&---------------------------------------------------------------------*
*& �ϲ����
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_output .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'ALV_PF_STATUS'
      i_callback_user_command  = 'ALV_USER_COMMAND'
      i_grid_settings          = gs_grid_settings
      is_layout                = gs_layout
      it_fieldcat              = gt_lvc[]
*     IT_EXCLUDING             = GT_EXCLUDE
*     IT_SORT                  = GT_SORT
      i_save                   = 'A'
      is_variant               = gs_variant
*     IT_EVENTS                = LT_EVENT
    TABLES
      t_outtab                 = gt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI״̬����
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI״̬����
*----------------------------------------------------------------------*
FORM alv_pf_status USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM.                    "ALV_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       ALVִ�в�ѯ����¼���Ӧ
*----------------------------------------------------------------------*
*      -->R_UCOMN      ��Ӧ��
*      -->RS_SELFIELD  ��ǰ����Ϣ
*----------------------------------------------------------------------*
FORM alv_user_command USING r_ucomm LIKE sy-ucomm  "���ð�ť��Ӧ
                            rs_selfield TYPE slis_selfield.

*  DATA: L_ZQHDH TYPE ZDHHSJ-ZQ HDH,
*        WA_ZDHHSJ1 TYPE ZDHHSJ,
*        LW_DATA LIKE WA_DATA.
  DATA: l_row  TYPE lvc_t_row,
        l_roid TYPE lvc_t_roid,
        lt_row TYPE lvc_t_roid,
        ls_row TYPE lvc_s_roid.

  DATA: ls_layout TYPE lvc_s_layo,
        lt_fcat   TYPE lvc_t_fcat,
        ls_fcat   TYPE LINE OF lvc_t_fcat.
*        L_GRID    TYPE REF TO CL_GUI_ALV_GRID.
  DATA: gs_stable TYPE lvc_s_stbl.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = l_grid.

  CALL METHOD l_grid->check_changed_data.
  rs_selfield-refresh = 'X'.


  CASE r_ucomm.
    WHEN 'HB'.
      LOOP AT gt_data INTO wa_data WHERE box = 'X'.
        IF wa_data-zidnum IS NOT INITIAL.
          MESSAGE e002(zjs001)."��ѡ������������Ʊ�ţ�������ѡ��
        ENDIF.
        IF wa_data-vtweg = '10'.
          IF wa_data-zkhsh IS INITIAL OR wa_data-zaddress IS INITIAL OR wa_data-zbank IS INITIAL.
            MESSAGE e004(zjs001)."�ͻ�˰����Ϣά����ȫ������ϵ��Աά��
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM frm_ucomm_hb.

    WHEN 'CF'.
      LOOP AT gt_data INTO wa_data WHERE box = 'X'.
        IF wa_data-zidnum IS NOT INITIAL.
          MESSAGE e002(zjs001)."��ѡ������������Ʊ�ţ�������ѡ��
        ENDIF.
        IF wa_data-vtweg = '10'.
          IF wa_data-zkhsh IS INITIAL OR wa_data-zaddress IS INITIAL OR wa_data-zbank IS INITIAL.
            MESSAGE e004(zjs001)."�ͻ�˰����Ϣά����ȫ������ϵ��Աά��
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM frm_ucomm_cf.

    WHEN 'HBCF'.
      LOOP AT gt_data INTO wa_data WHERE box = 'X'.
        IF wa_data-vtweg = '10'.
          IF wa_data-zkhsh IS INITIAL OR wa_data-zaddress IS INITIAL OR wa_data-zbank IS INITIAL.
            MESSAGE e004(zjs001)."�ͻ�˰����Ϣά����ȫ������ϵ��Աά��
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM frm_ucomm_hbcf.

    WHEN OTHERS.
  ENDCASE.



  CALL METHOD l_grid->get_frontend_layout
    IMPORTING
      es_layout = ls_layout.

  CALL METHOD l_grid->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = lt_fcat.

  ls_layout-cwidth_opt = 'X'.
*  LS_LAYOUT-INFO_FNAME = 'CLR'.

*  PERFORM FRM_FIELDCAT.

  CALL METHOD l_grid->set_frontend_layout
    EXPORTING
      is_layout = ls_layout.

  CALL METHOD l_grid->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = lt_fcat.

  CALL METHOD l_grid->refresh_table_display.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UCOMM_CF
*&---------------------------------------------------------------------*
*& ��ֹ���
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_ucomm_cf .
*�ų�alv����������Ʊ�ŵ�����
  DELETE gt_data WHERE zidnum IS NOT INITIAL .

  DATA:lv_netwr      TYPE vbrk-netwr,
       lv_zkpxe      TYPE ztvat_rule-zkpxe,
       lv_zkpxe_temp TYPE ztvat_rule-zkpxe,
       lv_zposnr     TYPE vbrp-posnr,
       n             TYPE i,
       m             TYPE i,
       ls_data       LIKE wa_data,
       lt_data       LIKE gt_data,
       lv_menge_xe   TYPE mseg-menge.

  DATA:gt_data_temp TYPE TABLE OF ty_data,
       gt_data_null TYPE TABLE OF ty_data,
       wa_data_temp TYPE ty_data.
  DATA:gt_data_cf   TYPE TABLE OF ty_data.
  DATA:lt_data_cf   TYPE TABLE OF ty_data.

  DATA:lv_error TYPE abap_bool.

  CLEAR:gt_ztvat_invoice,gt_ztvat_data,lt_data,lv_error,lt_tabix.

  IF gt_data IS INITIAL.
    MESSAGE s002(zjs001) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


  SORT gt_data BY vbeln zbklas posnr matnr." ZBKLAS  ZBMOUNT DESCENDING.

*  GT_DATA_TEMP = GT_DATA.
  LOOP AT gt_data INTO wa_data.
    MOVE-CORRESPONDING wa_data TO wa_data_temp.

    IF wa_data-box = ''.
      APPEND wa_data TO gt_data_null.
    ELSE.
      APPEND wa_data_temp TO gt_data_temp.
    ENDIF.
    CLEAR:wa_data,wa_data_temp.
  ENDLOOP.

  LOOP AT gt_data_null INTO wa_data.
    READ TABLE gt_data_temp INTO wa_data_temp WITH KEY vbeln = wa_data-vbeln.
    IF sy-subrc = 0.
      wa_data_temp = CORRESPONDING #( wa_data ).
      APPEND wa_data_temp TO gt_data_temp.
      DELETE gt_data_null.
    ENDIF.
    CLEAR:wa_data,wa_data_temp.
  ENDLOOP.

  LOOP AT gt_data_temp INTO wa_data_temp.
    IF wa_data_temp-fkart = 'ZIG'
    OR wa_data_temp-fkart = 'ZIVG'
    OR wa_data_temp-fkart = 'ZRE'
    OR wa_data_temp-fkart = 'ZREA'
    OR wa_data_temp-fkart = 'ZG2' .
      MESSAGE e022(zjs001)."�˻������Ʊ���ݲ�����ִ�в��
    ENDIF.

    CLEAR: wa_data_temp.
  ENDLOOP.

  CLEAR gt_data.

  gt_data_cf = CORRESPONDING #( gt_data_temp ).

  "�����û�����޶������������
  LOOP AT gt_data_cf INTO DATA(gs_data_cf).
    lt_data_cf = gt_data_temp.
    DELETE lt_data_cf WHERE vbeln NE gs_data_cf-vbeln.
    gs_data_cf-vbeln_line = lines( lt_data_cf ).
    CASE gs_data_cf-vtweg.
      WHEN '10'."��ó
        IF gs_data_cf-bukrs = '3500'.
          lv_zkpxe = '350000'.
        ELSE.
          lv_zkpxe = '10000000'.
        ENDIF.
      WHEN '20'."��ó
        lv_zkpxe = '9999999999999.99'."���޶�
      WHEN OTHERS.
    ENDCASE.
    READ TABLE gt_rule INTO DATA(gs_rule) WITH KEY bukrs = gs_data_cf-bukrs
                                                   kunrg = gs_data_cf-kunrg BINARY SEARCH.
    IF sy-subrc = 0 AND gs_rule-zkpxe > 0.
      lv_zkpxe = gs_rule-zkpxe.
    ENDIF.
    IF ( gs_data_cf-vbeln_line <= 8 AND gs_data_cf-netwr_k <= lv_zkpxe ) OR gs_rule-zdk = 'X'.
      IF sy-ucomm = 'CF'.
        MESSAGE s014(zjs001) INTO gs_data_cf-msg.
        APPEND gs_data_cf TO gt_data_null.
        DELETE gt_data_cf.
      ELSE.
        APPEND gs_data_cf TO gt_data_null.
        DELETE gt_data_cf.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT gt_data_cf BY vbeln.
  DELETE ADJACENT DUPLICATES FROM gt_data_cf COMPARING vbeln.
  CLEAR:lt_data_cf.

  LOOP AT gt_data_cf INTO gs_data_cf .
    LOOP AT gt_data_temp INTO wa_data_temp WHERE vbeln = gs_data_cf-vbeln.
      ADD 1 TO n.
      ADD wa_data_temp-zbmount TO lv_netwr.
      IF lv_netwr > lv_zkpxe AND lt_data_cf IS NOT INITIAL.
        PERFORM create_zidnum_list TABLES lt_data_cf lt_tabix CHANGING lv_zidnum.
        APPEND LINES OF lt_data_cf TO lt_data.
        CLEAR :lt_data_cf,lv_netwr,n.
        ADD wa_data_temp-zbmount TO lv_netwr.
      ENDIF.

      IF lv_netwr > lv_zkpxe.   "�����ӽ�� �����޶�
        gs_data_nm = CORRESPONDING #( wa_data_temp ).
        PERFORM frm_create_zidnum USING '2' CHANGING gs_data_nm gs_data_nm-zbmount lv_zidnum.
        CLEAR lt_data_cf.
        APPEND gs_data_nm TO lt_data.
        n = 1.
        CONTINUE.
      ENDIF.

      IF n > 8. "������8��ʱ
        PERFORM create_zidnum_list TABLES lt_data_cf lt_tabix CHANGING lv_zidnum.
        APPEND LINES OF lt_data_cf TO lt_data.
        CLEAR :lt_data_cf,n.
      ENDIF.

      APPEND wa_data_temp TO lt_data_cf.
    ENDLOOP.
    IF lt_data_cf IS NOT INITIAL.
      PERFORM create_zidnum_list TABLES lt_data_cf lt_tabix CHANGING lv_zidnum.
      APPEND LINES OF lt_data_cf TO lt_data.
      CLEAR lt_data_cf.
    ENDIF.
    CLEAR:lv_netwr, n.
  ENDLOOP.



*--����������ֺ����ݵ�alv��Ļ
  APPEND LINES OF lt_data TO gt_data.
*--��û��ѡ�е��мӻ���Ļ
  APPEND LINES OF gt_data_null TO gt_data.

  MODIFY ztvat_data FROM TABLE gt_ztvat_data.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
  MODIFY ztvat_invoice FROM TABLE gt_ztvat_invoice.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
  DELETE ADJACENT DUPLICATES FROM gt_head COMPARING vbeln.
  CLEAR:gt_ztvat_data,gt_ztvat_invoice.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UCOMM_HBCF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_ucomm_hbcf .

  DATA:lv_netwr  TYPE vbap-netwr.
  DATA:lt_tabix TYPE TABLE OF sy-tabix,
       ls_tabix TYPE sy-tabix.
  DATA:lv_zposnr    TYPE ztvat_data-zposnr,
       gt_data_null TYPE TABLE OF ty_data.
  DATA:gt_data_hb   TYPE TABLE OF ty_data.
  DATA:n        TYPE i,
       lv_zkpxe TYPE vbrk-netwr.
  DATA:lv_vbeln TYPE vbrk-vbeln.
  DATA:lt_ziv1  TYPE TABLE OF ty_data.

  LOOP AT gt_data INTO wa_data WHERE box = 'X'.
    IF wa_data-fkart = 'ZIG'
    OR wa_data-fkart = 'ZIVG'
    OR wa_data-fkart = 'ZRE'
    OR wa_data-fkart = 'ZREA'
    OR wa_data-fkart = 'ZG2' .
      MESSAGE e003(zjs001)."��ѡ���ݰ����˻��������Ϣ��Ʊ���ݣ�ϵͳ�������Զ��ϲ������ֹ�ѡ�����ⲿ������
    ENDIF.

  ENDLOOP.
*---���߲���߼�
  PERFORM frm_ucomm_cf.
*---��ϲ�
  "ȡ�������
  CLEAR:gt_data_null,gt_data_nm.
  LOOP AT gt_data INTO wa_data.
    MOVE-CORRESPONDING wa_data TO wa_data_nm.
    IF wa_data_nm-box = 'X'.
      APPEND wa_data_nm TO gt_data_nm.
    ENDIF.

    IF wa_data_nm-box = ''.
      APPEND wa_data TO gt_data_null.
    ENDIF.
    CLEAR:wa_data_nm,wa_data.
  ENDLOOP.


  CLEAR:lv_netwr,gt_data,lv_zposnr,lt_ziv1.

  READ TABLE gt_data_nm TRANSPORTING NO FIELDS WITH KEY fkart = 'ZIV1'.
  IF sy-subrc = 0.
    LOOP AT gt_data_nm INTO wa_data WHERE fkart = 'ZIV1'.
      APPEND wa_data TO lt_ziv1.
    ENDLOOP.
    DELETE gt_data_nm WHERE fkart = 'ZIV1'.
  ENDIF.

  PERFORM zc_hb."�����ϲ�

  IF lt_ziv1 IS NOT INITIAL.
    gt_data_nm = lt_ziv1.
    PERFORM zc_hb."�����ϲ�
  ENDIF.

  APPEND LINES OF gt_data_null TO gt_data.
  SORT gt_data BY bukrs kunrg arktx zggxh atwrt zbklas ztax zprice matnr vbeln.





ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CREATE_ZIDNUM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA_NM
*&---------------------------------------------------------------------*
FORM frm_create_zidnum   USING    VALUE(p_type)
                         CHANGING  ls_data_nm STRUCTURE wa_data_nm
                                   pv_netwr
                                   pv_zidnum.

  CALL FUNCTION 'ZFM_JS_GETSERIALNUM' "��Ʊ����
    EXPORTING
      object      = 'ZJSYP'
      nr_range_nr = '01'
    IMPORTING
      number      = lv_zidnum.

  wa_ztvat_invoice-zidnum = lv_zidnum.
  wa_ztvat_invoice-vbeln  = ls_data_nm-vbeln.
  wa_ztvat_invoice-zernam = sy-uname.
  wa_ztvat_invoice-zerdat = sy-datum.
  wa_ztvat_invoice-zertim = sy-uzeit.
  APPEND wa_ztvat_invoice TO gt_ztvat_invoice.
  CLEAR:wa_ztvat_invoice.

  CALL FUNCTION 'ZFM_JS_GETSERIALNUM'  "����
    EXPORTING
      object      = 'ZJSSY'
      nr_range_nr = '01'
    IMPORTING
      number      = lv_zjssy.

  wa_ztvat_data-zidnum  = lv_zidnum.
  wa_ztvat_data-zsuoyin = lv_zjssy.
*  WA_ZTVAT_DATA-VBELN   = LS_DATA_NM-VBELN.
  wa_ztvat_data-matnr   = ls_data_nm-matnr.
  wa_ztvat_data-zstatus = '10'."������
  wa_ztvat_data-zjszl   = ls_data_nm-zjszl.
  wa_ztvat_data-zkpjh   = ls_data_nm-zkpjh.
  wa_ztvat_data-zposnr  = ls_data_nm-zposnr.
  wa_ztvat_data-bukrs   = ls_data_nm-bukrs.
*  WA_ZTVAT_DATA-FKDAT   = LS_DATA_NM-FKDAT.
*  WA_ZTVAT_DATA-FKART   = LS_DATA_NM-FKART.
  wa_ztvat_data-zposnr  = 10.
  wa_ztvat_data-vkorg   = ls_data_nm-vkorg.
  wa_ztvat_data-vtweg   = ls_data_nm-vtweg.
  wa_ztvat_data-kunrg   = ls_data_nm-kunrg.
  wa_ztvat_data-zkhname = ls_data_nm-zkhname.
  wa_ztvat_data-zkhsh   = ls_data_nm-zkhsh.
  wa_ztvat_data-zaddress = ls_data_nm-zaddress.
  wa_ztvat_data-zkhbank  = ls_data_nm-zbank.
  wa_ztvat_data-ernam    = ls_data_nm-ernam.
  wa_ztvat_data-zsaler   = ls_data_nm-zsaler.
  wa_ztvat_data-zwmfph   = ls_data_nm-zwmfph.
  wa_ztvat_data-bklas    = ls_data_nm-bklas.
  wa_ztvat_data-arktx    = ls_data_nm-arktx.
  wa_ztvat_data-zspmc    = ls_data_nm-zspmc.
  IF ls_data_nm-atwrt IS INITIAL.
    wa_ztvat_data-zggxh    = ls_data_nm-zggxh .
  ELSE.
    wa_ztvat_data-zggxh    = |{ ls_data_nm-zggxh } { ls_data_nm-atwrt }��| .
  ENDIF.

  wa_ztvat_data-atwrt    = ls_data_nm-atwrt.
  wa_ztvat_data-fkimg    = ls_data_nm-fkimg."����
  wa_ztvat_data-vrkme    = ls_data_nm-vrkme.
  wa_ztvat_data-zmount   = ls_data_nm-zmount.
  wa_ztvat_data-zbmount  = pv_netwr."���
  wa_ztvat_data-zse      = ls_data_nm-zse.
  wa_ztvat_data-zprice   = ls_data_nm-zprice.
  wa_ztvat_data-zbprice   = ls_data_nm-zbprice.

  wa_ztvat_data-kursk    = ls_data_nm-kursk.
  wa_ztvat_data-waerk    = ls_data_nm-waerk.
  wa_ztvat_data-zwaerk   = ls_data_nm-zwaerk.
  wa_ztvat_data-ztax     = ls_data_nm-ztax.
  IF ls_data_nm-bukrs = '3500' AND ls_data_nm-vtweg = '10'.
    wa_ztvat_data-zbeizhu  = |{ ls_data_nm-zsaler } { ls_data_nm-bstkd }|.
  ENDIF.
  wa_ztvat_data-zssflbm  = ls_data_nm-zssflbm.
  wa_ztvat_data-bezei    = ls_data_nm-bezei.
  wa_ztvat_data-zzss     = ls_data_nm-zzss.
  wa_ztvat_data-zernam   = sy-uname.
  wa_ztvat_data-zerdat   = sy-datum.
  wa_ztvat_data-zertim   = sy-uzeit.
  APPEND wa_ztvat_data TO gt_ztvat_data.

  CALL FUNCTION 'DEQUEUE_EZLJS001'
    EXPORTING
      vbeln = ls_data_nm-vbeln
      posnr = ls_data_nm-posnr.



  ls_data_nm-zidnum = lv_zidnum.
  ls_data_nm-zsuoyin = lv_zjssy.
  pv_zidnum = lv_zidnum.

  CLEAR:wa_ztvat_data,lv_zjssy,lv_zidnum.

*  MODIFY GT_DATA_NM FROM LS_DATA_NM TRANSPORTING ZIDNUM ZSUOYIN WHERE VBELN = LS_DATA_NM-VBELN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UCOMM_HB
*&---------------------------------------------------------------------*
*& �ϲ�����
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_ucomm_hb .
  DATA:lv_zposnr    TYPE ztvat_data-zposnr,
       gt_data_null TYPE TABLE OF ty_data.
  DATA:lv_netwr   TYPE vbap-netwr,
       lv_zbeizhu TYPE ztvat_data-zbeizhu,
       lv_amount  TYPE vbap-netwr,
       lv_se      TYPE vbap-netwr,
       lv_zkpxe   TYPE ztvat_rule-zkpxe.
  DATA:lv_menge TYPE mseg-menge.
  DATA:n TYPE i.
  DATA:gt_data_hb TYPE TABLE OF ty_data.
  DATA:gt_data_temp TYPE TABLE OF ty_data.
  DATA:lv_flag_r  TYPE abap_bool.    "�ж��˻��Ƿ�ѡ������
  DATA:tx_flag    TYPE abap_bool.    "������Ϣ falg
  DATA:lv_numb_zx TYPE i.            "����Ʊ����
  DATA:ls_tabix TYPE  sy-tabix.
  DATA:lv_vbeln TYPE vbrk-vbeln.  "��������8��ʱ����¼��Ʊ��

  CLEAR lv_numb_zx.

  CLEAR:gt_data_null,gt_data_nm,lt_tabix,gt_data_hb.
  LOOP AT gt_data INTO wa_data.
    MOVE-CORRESPONDING wa_data TO wa_data_nm.
    IF wa_data_nm-box = 'X'.
      APPEND wa_data_nm TO gt_data_nm.
    ENDIF.

    IF wa_data_nm-box = ''.
      APPEND wa_data TO gt_data_null.
    ENDIF.
    CLEAR:wa_data_nm,wa_data.
  ENDLOOP.

  "ѡ��һ����Ʊ ��ѡ�����е�
  LOOP AT gt_data_nm INTO wa_data_nm.
    LOOP AT gt_data_null INTO wa_data WHERE vbeln = wa_data_nm-vbeln.
      wa_data_nm = CORRESPONDING #( wa_data ).
      APPEND wa_data_nm TO gt_data_nm.
      DELETE gt_data_null.
    ENDLOOP.
  ENDLOOP.
*--�ϲ�
  LOOP AT gt_data_nm INTO wa_data.
    CASE wa_data-fkart.
      WHEN 'ZIG' OR 'ZIVG' OR 'ZRE' OR 'ZREA' OR 'ZG2' .
        lv_flag_r = abap_true.
      WHEN OTHERS.
        AT END OF vbeln.
          ADD 1 TO lv_numb_zx.
        ENDAT.
    ENDCASE.
    IF wa_data-matnr = g_tx_matnr AND wa_data-fkart NE 'ZIG' AND
                                      wa_data-fkart NE 'ZIVG' AND
                                      wa_data-fkart NE 'ZRE' AND
                                      wa_data-fkart NE 'ZREA' AND
                                      wa_data-fkart NE 'ZG2' .
      tx_flag = abap_true.
    ENDIF.
  ENDLOOP.

  IF lv_flag_r = abap_true AND lv_numb_zx IS INITIAL.
    MESSAGE s005(zjs001) DISPLAY LIKE 'E'."��ѡ���ݰ����˻������Ʊ���ݣ���ע��ѡ������ϲ���Ʊ����
    EXIT.
  ENDIF.

  IF lv_flag_r = abap_true AND tx_flag = abap_true.
    MESSAGE s025(zjs001) DISPLAY LIKE 'E'."��ѡ���ݰ�����Ϣ��Ʊ���ݣ�����ѡ���˻��ϲ�
    EXIT.
  ENDIF.

  IF tx_flag = abap_true AND lv_flag_r = abap_false.

    gt_data_temp = gt_data_nm.
    SORT gt_data_temp BY ztax.
    DELETE ADJACENT DUPLICATES FROM gt_data_temp COMPARING ztax.
    DATA(tax_line) = lines( gt_data_temp ).
    IF tax_line > 1.
      MESSAGE s018(zjs001) DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDIF.

  "������˻�
  CLEAR:lv_netwr,lv_menge,gt_data,lv_amount.
  IF lv_flag_r = abap_true.
    LOOP AT gt_data_nm INTO wa_data_nm.
      IF wa_data_nm-zbmount < 0.
        wa_data_nm-fkimg = - wa_data_nm-fkimg.
      ENDIF.
      ADD wa_data_nm-zbmount TO lv_netwr.
      ADD wa_data_nm-fkimg   TO lv_menge.
      MODIFY gt_data_nm FROM wa_data_nm TRANSPORTING fkimg.
    ENDLOOP.
    IF lv_netwr <= 0 OR lv_menge <= 0.  "�������˻�����
      MESSAGE s015(zjs001) DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    SORT gt_data_nm BY bukrs kunrg ztax zbmount DESCENDING zbklas zprice zwmfph DESCENDING.
    CLEAR:lv_netwr,lv_menge,lv_amount,lv_vbeln.

    LOOP AT gt_data_nm INTO wa_data_nm WHERE zidnum IS INITIAL.
      ls_tabix = sy-tabix.
      IF n > 8. "�����������8
        PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.

        LOOP AT lt_tabix INTO DATA(ls_tabix_temp).
          MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
        ENDLOOP.
        CLEAR:gt_data_hb, lv_netwr,lt_tabix,lv_amount,wa_data_nm-zidnum,n.
        n = 1.
      ENDIF.

      CASE wa_data_nm-vtweg.
        WHEN '10'."��ó
          IF wa_data_nm-bukrs = '3500'.
            lv_zkpxe = '350000'.
          ELSE.
            lv_zkpxe = '10000000'.
          ENDIF.
        WHEN '20'."��ó
          lv_zkpxe = '9999999999999.99'."���޶�
        WHEN OTHERS.
      ENDCASE.

      READ TABLE gt_rule INTO DATA(ls_rule) WITH KEY kunrg = wa_data_nm-kunrg bukrs = wa_data_nm-bukrs BINARY SEARCH.
      IF sy-subrc = 0."�������߼�
        IF ls_rule-zkpxe IS NOT INITIAL.
          lv_zkpxe = ls_rule-zkpxe."��������޶�
        ENDIF.
      ENDIF.

      "���������ͬ�ͻ� ���߲�ͬ˰��
      READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                            bukrs = wa_data_nm-bukrs.
      IF sy-subrc NE 0 AND gt_data_hb IS NOT INITIAL.
        PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
        LOOP AT lt_tabix INTO ls_tabix_temp.
          MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
        ENDLOOP.
        CLEAR:gt_data_hb, lv_netwr,lt_tabix,lv_amount,wa_data_nm-zidnum,n.
      ENDIF.

      IF wa_data_nm-zbmount < 0.  "������˻�����
        DATA(lv_end_line) = lines( gt_data_hb ).
        IF lv_end_line = 0.
          wa_data_nm-msg = 'û������Ʊ����ϲ�'.
          MODIFY gt_data_nm FROM wa_data_nm.
          CONTINUE.
        ENDIF.
        READ TABLE gt_data_hb INTO DATA(gs_data_hb) INDEX lv_end_line.   " �����һ�кϲ�
*            IF SY-SUBRC = 0.
        ADD wa_data_nm-zbmount TO lv_netwr.
        ADD wa_data_nm-zmount  TO lv_amount.
        ADD wa_data_nm-fkimg   TO gs_data_hb-fkimg.
        ADD wa_data_nm-zbmount TO gs_data_hb-zbmount.
        ADD wa_data_nm-zmount  TO gs_data_hb-zmount.
        ADD wa_data_nm-zse     TO gs_data_hb-zse.
*        IF wa_data_nm-vtweg = '10'.
*          gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ wa_data_nm-bstkd }|.
*        ENDIF.

        WHILE gs_data_hb-zbmount <= 0  OR gs_data_hb-fkimg <= 0.
          DELETE gt_data_hb INDEX lv_end_line.
          SUBTRACT 1 FROM lv_end_line.
          IF lv_end_line = 0.
            EXIT.
          ENDIF.
          READ TABLE gt_data_hb INTO DATA(gs_data_temp) INDEX lv_end_line.
          lv_netwr = gs_data_hb-zbmount.
          lv_menge = gs_data_hb-fkimg.
          lv_amount = gs_data_hb-zmount.
          lv_se     = gs_data_hb-zse.
          lv_zbeizhu = gs_data_hb-zbeizhu.
          gs_data_hb = CORRESPONDING #( gs_data_temp ).
          ADD lv_netwr TO gs_data_hb-zbmount.
          ADD lv_menge TO gs_data_hb-fkimg.
          ADD lv_amount TO gs_data_hb-zmount.
          ADD lv_se TO gs_data_hb-zse.
          IF gs_data_hb-vtweg = '10'.
            gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ lv_zbeizhu }|.
          ENDIF.
          CLEAR:lv_netwr,lv_menge,lv_se,lv_amount.
        ENDWHILE.
        IF lv_end_line = 0.
          wa_data_nm-msg = 'û������Ʊ����ϲ�'.
          MODIFY gt_data_nm FROM wa_data_nm.
          CONTINUE.
        ENDIF.
        MODIFY gt_data_hb FROM gs_data_hb INDEX lv_end_line.
        APPEND ls_tabix TO lt_tabix.
      ELSE.
        CASE wa_data_nm-vtweg.
          WHEN '10'.
            IF wa_data_nm-bukrs = '3500'.
              READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = wa_data_nm-kunrg
                                                             zggxh  = wa_data_nm-zggxh
                                                             atwrt  = wa_data_nm-atwrt
                                                             zprice = wa_data_nm-zprice
                                                             matnr  = wa_data_nm-matnr.
            ELSE.
              READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = wa_data_nm-kunrg
                                                             zggxh  = wa_data_nm-zggxh
                                                             matnr  = wa_data_nm-matnr.
            ENDIF.
          WHEN '20'.
            READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = wa_data_nm-kunrg
                                                           zwmfph = wa_data_nm-zwmfph
                                                           zggxh  = wa_data_nm-zggxh
                                                           atwrt  = wa_data_nm-atwrt
                                                           zprice = wa_data_nm-zprice
                                                           matnr  = wa_data_nm-matnr.
          WHEN OTHERS.
        ENDCASE.
        IF sy-subrc = 0.   "��������ͬ�ͻ���ͬ����ʱ
          ADD wa_data_nm-zbmount TO lv_netwr.
          IF lv_netwr > lv_zkpxe.
            PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
            LOOP AT lt_tabix INTO ls_tabix_temp.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
            CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
            ADD wa_data_nm-zbmount TO lv_netwr.
            APPEND wa_data_nm TO gt_data_hb.
            APPEND ls_tabix TO lt_tabix.
            n = 1.
          ELSE.
            ADD wa_data_nm-fkimg   TO gs_data_hb-fkimg.
            ADD wa_data_nm-zbmount TO gs_data_hb-zbmount.
            ADD wa_data_nm-zmount  TO gs_data_hb-zmount.
            ADD wa_data_nm-zse     TO gs_data_hb-zse.
*            IF wa_data_nm-vtweg = '10'.
*              gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ wa_data_nm-bstkd }|.
*            ENDIF.
            MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
            APPEND ls_tabix TO lt_tabix.
          ENDIF.

        ELSE.
          ADD wa_data_nm-zbmount TO lv_netwr.
          IF lv_netwr > lv_zkpxe.
            PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
            LOOP AT lt_tabix INTO ls_tabix_temp.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
            CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
            ADD wa_data_nm-zbmount TO lv_netwr.
            APPEND wa_data_nm TO gt_data_hb.
            APPEND ls_tabix TO lt_tabix.
            n = 1.
          ELSE.
            APPEND wa_data_nm TO gt_data_hb.
            APPEND ls_tabix TO lt_tabix.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF gt_data_hb IS NOT INITIAL.
      PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
      LOOP AT lt_tabix INTO ls_tabix_temp.
        MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
      ENDLOOP.
      CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
    ENDIF.
  ENDIF.

  CLEAR:lv_netwr,lv_zposnr,lv_vbeln.

  PERFORM zc_hb.

  APPEND LINES OF gt_data_null TO gt_data.
  SORT gt_data BY bukrs kunrg arktx zggxh atwrt zbklas ztax zprice matnr vbeln.





ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GETDATA_CXSJ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_getdata_cxsj .
  SELECT
    ztvat_data~zidnum
    ztvat_data~zstatus
    ztvat_data~zsuoyin
    ztvat_data~zjszl
    ztvat_data~zkpjh
    ztvat_data~zposnr
    ztvat_data~bukrs
    ztvat_data~vkorg
    ztvat_data~vtweg
    ztvat_data~kunrg
    ztvat_data~zkhname
    ztvat_data~zkhsh
    ztvat_data~zaddress
    ztvat_data~zkhbank
    ztvat_data~ernam
    ztvat_data~zsaler
    ztvat_data~zwmfph
    ztvat_data~matnr
    ztvat_data~bklas
    ztvat_data~arktx
    ztvat_data~zspmc
    ztvat_data~zggxh
    ztvat_data~atwrt
    ztvat_data~fkimg
    ztvat_data~vrkme
    ztvat_data~zprice
    ztvat_data~zbprice
    ztvat_data~zmount
    ztvat_data~zbmount
    ztvat_data~zse
    ztvat_data~kursk
    ztvat_data~waerk
    ztvat_data~zwaerk
    ztvat_data~ztax
    ztvat_data~zbeizhu
    ztvat_data~zssflbm
    ztvat_data~bezei
    ztvat_data~zzss
*    ZTVAT_DATA~ZTHCD
*    ZTVAT_DATA~ZTXBS
    INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM ztvat_data
    WHERE zidnum IN s_zid3
      AND bukrs IN s_bukrs3
      AND vtweg = p_vtweg3
      AND zsaler IN s_zsale3
      AND kunrg IN s_kunrg3
      AND zstatus IN ( '10','20' )
      AND zwmfph IN s_wmfph3.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DISPLAY_CXSJ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_cxsj .

  PERFORM frm_layout_cxsj.             "���������ʽ
  PERFORM frm_fieldcat_cxsj.
  PERFORM frm_output_cxsj.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_LAYOUT_CXSJ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_layout_cxsj .
**layout
  wa_layout-box_fname = 'BOX'.
  wa_layout-cwidth_opt = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_FIELDCAT_CXSJ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_fieldcat_cxsj .


  set_fieldcat 'ZIDNUM' '��Ʊ����' '' '' '' ''.
  set_fieldcat 'ZSTATUS' '״̬' '' '' '' ''.
  set_fieldcat 'ZSUOYIN' '����' '' '' '' ''.
  set_fieldcat 'ZJSZL'   '��Ʊ����' '' '' '' ''.
  set_fieldcat 'ZKPJH' '��Ʊ����' '' '' '' ''.
  set_fieldcat 'ZPOSNR' '��Ʊ����Ŀ' '' '' '' ''.
  set_fieldcat 'BUKRS' '��˾����' '' '' '' ''.
*  SET_FIELDCAT 'FKDAT' '��Ʊ����' '' '' '' ''.
*  SET_FIELDCAT 'FKART' '��Ʊ����' '' '' '' ''.
  set_fieldcat 'VKORG' '������֯' '' '' '' ''.
  set_fieldcat 'VTWEG' '��������' '' '' '' ''.
  set_fieldcat 'KUNRG' '�ͻ�' '' '' '' ''.
  set_fieldcat 'ZKHNAME' '�ͻ�����' '' '' '' ''.
  set_fieldcat 'ZKHSH' '�ͻ�˰��' '' '' '' ''.
  set_fieldcat 'ZADDRESS' '�ͻ���ַ�绰' '' '' '' ''.
  set_fieldcat 'ZKHBANK' '�ͻ������м��˺�' '' '' '' ''.
  set_fieldcat 'ERNAM' '��ƱԱ' '' '' '' ''.
  set_fieldcat 'ZSALER' '����Ա' '' '' '' ''.
  set_fieldcat 'ZWMFPH' '��ó��Ʊ��' '' '' '' ''.
  set_fieldcat 'MATNR' '���ϱ���' '' '' '' ''.
  set_fieldcat 'BKLAS' '������' '' '' '' ''.
  set_fieldcat 'ARKTX' '��������' '' '' '' ''.
  set_fieldcat 'ZSPMC' '��Ʒ����' '' '' '' ''.
  set_fieldcat 'ZGGXH' '����ͺ�' '' '' '' ''.
  set_fieldcat 'ATWRT' '�ȼ�' '' '' '' ''.
  set_fieldcat 'FKIMG' '����' '' '' '' ''.
  set_fieldcat 'VRKME' '������λ' '' '' '' ''.
  set_fieldcat 'ZPRICE' 'ԭ�Һ�˰����' '' '' '' ''.
  set_fieldcat 'ZBPRICE' '��λ�Ҳ���˰����' '' '' '' ''.
  set_fieldcat 'ZMOUNT' '���' '' '' '' ''.
  set_fieldcat 'ZBMOUNT' '��λ�ҽ��' '' '' '' ''.
  set_fieldcat 'ZSE' '˰��' '' '' '' ''.
  set_fieldcat 'KURSK' '����' '' '' '' ''.
  set_fieldcat 'WAERK' '����' '' '' '' ''.
  set_fieldcat 'ZWAERK' '��λ����' '' '' '' ''.
  set_fieldcat 'ZTAX' '˰��' '' '' '' ''.
  set_fieldcat 'ZBEIZHU' '��ע' '' '' '' ''.
  set_fieldcat 'ZSSFLBM' '˰�շ������' '' '' '' ''.
  set_fieldcat 'BEZEI' 'ó�׷�ʽ' '' '' '' ''.
  set_fieldcat 'ZZSS' '����˿��ʶ' '' '' '' ''.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_OUTPUT_CXSJ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_output_cxsj .

  gs_variant-handle = '1'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      is_layout_lvc            = wa_layout
      it_fieldcat_lvc          = gt_fieldcat[]
*     i_grid_title             = p_title
      i_grid_settings          = gs_grid_settings
      i_callback_pf_status_set = 'ALV_PF'
      i_callback_user_command  = 'USER_COMMAND_CXSJ'
      is_variant               = gs_variant
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PF
*&---------------------------------------------------------------------*
*       GUI״̬����
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI״̬����
*----------------------------------------------------------------------*
FORM alv_pf USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_CXSJ'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  frm_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command_cxsj  USING r_ucomm LIKE sy-ucomm                      "FRM_USER_COMMAND
                              rs_selfield TYPE slis_selfield.

  DATA:n       TYPE i,
       m       TYPE i,
       ls_data TYPE ty_data.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = l_grid.

  CALL METHOD l_grid->check_changed_data.
  rs_selfield-refresh = 'X'.


  SORT gt_data BY zidnum kunrg matnr .

  CLEAR:ls_data,n,m,wa_data.
  CASE r_ucomm.
    WHEN 'CXYCL'."�����Ѵ���
*---У��
      LOOP AT gt_data INTO wa_data WHERE box = 'X'.
        IF wa_data-zstatus = '10'.
          MESSAGE e007(zjs001)."��ѡ���д���״̬Ϊ�����е����ݣ���ע��ɸѡ
        ENDIF.

        READ TABLE gt_data TRANSPORTING NO FIELDS WITH KEY box = '' zidnum = wa_data-zidnum.
        IF sy-subrc = 0.
          MESSAGE e008(zjs001)."ͬһ����Ʊ���ݴ���δѡ��ȫ����Ŀ��������빴ѡ����
        ENDIF.

      ENDLOOP.
*--����
      CLEAR:gt_ztvat_data,wa_ztvat_data.
      LOOP AT gt_data INTO wa_data WHERE box = 'X'.
        MOVE-CORRESPONDING wa_data TO wa_ztvat_data.
        wa_ztvat_data-zstatus = '10'.
        wa_ztvat_data-zaenam = sy-uname.
        wa_ztvat_data-zaedat = sy-datum.
        wa_ztvat_data-zaetim = sy-uzeit.

        APPEND wa_ztvat_data TO gt_ztvat_data.
        wa_data-zstatus = '10'.
        MODIFY  gt_data FROM wa_data TRANSPORTING zstatus.

      ENDLOOP.
      MODIFY ztvat_data FROM TABLE gt_ztvat_data.
      IF sy-subrc = 0.
        COMMIT WORK.
        MESSAGE s010(zjs001)."���³ɹ�
      ELSE.
        ROLLBACK WORK.
        MESSAGE e009(zjs001)."����ʧ��
      ENDIF.
    WHEN 'CXHBCF'."�����ϲ����
*---У��
      LOOP AT gt_data INTO wa_data WHERE box = 'X'.
        IF wa_data-zstatus = '20'.
          MESSAGE e020(zjs001)."��ѡ���д���״̬Ϊ�����е����ݣ���ע��ɸѡ
        ENDIF.

        READ TABLE gt_data TRANSPORTING NO FIELDS WITH KEY box = '' zidnum = wa_data-zidnum.
        IF sy-subrc = 0.
          MESSAGE e008(zjs001)."ͬһ����Ʊ���ݴ���δѡ��ȫ����Ŀ��������빴ѡ����
        ENDIF.
      ENDLOOP.
*--����
      CLEAR:gt_ztvat_data,wa_ztvat_data.
      LOOP AT gt_data INTO wa_data WHERE box = 'X'.
        MOVE-CORRESPONDING wa_data TO wa_ztvat_data.
        APPEND wa_ztvat_data TO gt_ztvat_data.
        DELETE gt_data.
      ENDLOOP.

      IF gt_ztvat_data IS NOT INITIAL.
        SELECT * INTO TABLE @DATA(lt_ztvat_invoice) FROM ztvat_invoice
        FOR ALL ENTRIES IN @gt_ztvat_data
        WHERE zidnum = @gt_ztvat_data-zidnum.
      ENDIF.


      DELETE ztvat_data FROM TABLE gt_ztvat_data.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

      DELETE ztvat_invoice  FROM TABLE lt_ztvat_invoice.
      IF sy-subrc = 0.
        COMMIT WORK.
        MESSAGE s010(zjs001)."����ɹ�
      ELSE.
        ROLLBACK WORK.
        MESSAGE e009(zjs001)."����ʧ��
      ENDIF.

    WHEN OTHERS.
  ENDCASE.






ENDFORM.                    "USER_COMMAND_HEAD     "
*&---------------------------------------------------------------------*
*& Form FRM_GETDATA_JSBB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_getdata_jsbb .
  SELECT
    ztvat_invoice~zidnum
    ztvat_invoice~vbeln
    fkart
    zjsdm
    zjshm
    zjsdate
    zpost
    ztvat_invoice~zerdat
    ztvat_invoice~zertim
    ztvat_invoice~zernam
    ztvat_invoice~zaedat
    ztvat_invoice~zaetim
    ztvat_invoice~zaenam
    INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM ztvat_invoice
    JOIN vbrk
    ON ztvat_invoice~vbeln = vbrk~vbeln
    WHERE ztvat_invoice~zidnum IN s_zid4
      AND ztvat_invoice~vbeln  IN s_vbeln4
      AND zjsdm  IN s_zjsdm4
      AND zjshm  IN s_zjshm4
      AND zjsdate IN s_zdate4.

  IF gt_data IS NOT INITIAL.
    SELECT zidnum,
           zstatus
    INTO TABLE @DATA(lt_ztvat_data)
    FROM ztvat_data
    FOR ALL ENTRIES IN @gt_data
    WHERE zidnum = @gt_data-zidnum.

    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
      READ TABLE lt_ztvat_data INTO DATA(ls_ztvat_data) WITH KEY zidnum = <fs_data>-zidnum.
      IF sy-subrc = 0.
        <fs_data>-zstatus = ls_ztvat_data-zstatus.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF p_gl = 'X'.
    LOOP AT gt_data INTO wa_data.
      IF wa_data-zjsdm IS INITIAL OR wa_data-zjshm IS INITIAL.
        DELETE gt_data.
      ENDIF.
    ENDLOOP.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DISPLAY_JSBB
*&---------------------------------------------------------------------*
*& ��˰��Ʊ��Ӧ����
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_jsbb .
  PERFORM frm_layout_jsbb.             "���������ʽ
  PERFORM frm_fieldcat_jsbb.
  PERFORM frm_output_jsbb.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_LAYOUT_JSBB
*&---------------------------------------------------------------------*
*& ��˰��Ʊ��Ӧ����
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_layout_jsbb .
**layout
  wa_layout-box_fname = 'BOX'.
  wa_layout-cwidth_opt = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_FIELDCAT_JSBB
*&---------------------------------------------------------------------*
*& ��˰��Ʊ��Ӧ����
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_fieldcat_jsbb .

  set_fieldcat 'ZIDNUM' '��Ʊ����' '' '' '' ''.
  set_fieldcat 'VBELN' 'SAP��Ʊ��' '' '' '' ''.
  set_fieldcat 'FKART' '��Ʊ����' '' '' '' ''.
  set_fieldcat 'ZJSDM'   '��˰����' '' '' '' ''.
  set_fieldcat 'ZJSHM' '��˰����' '' '' '' ''.
  set_fieldcat 'ZJSDATE' '��˰����' '' '' '' ''.
  set_fieldcat 'ZPOST' '�ѹ���' '' '' '' ''.
  set_fieldcat 'ZERDAT' '��������' '' '' '' ''.
  set_fieldcat 'ZERTIM' '����ʱ��' '' '' '' ''.
  set_fieldcat 'ZERNAM' '������' '' '' '' ''.
  set_fieldcat 'ZAEDAT' '�޸�����' '' '' '' ''.
  set_fieldcat 'ZAETIM' '�޸�ʱ��' '' '' '' ''.
  set_fieldcat 'ZAENAM' '�޸���' '' '' '' ''.
  set_fieldcat 'ZSTATUS' '״̬' '' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_OUTPUT_JSBB
*&---------------------------------------------------------------------*
*& ��˰��Ʊ��Ӧ����
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_output_jsbb .
  gs_variant-handle = '1'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
      is_layout_lvc      = wa_layout
      it_fieldcat_lvc    = gt_fieldcat[]
*     i_grid_title       = p_title
      i_grid_settings    = gs_grid_settings
*     I_CALLBACK_PF_STATUS_SET = 'ALV_PF'
*     I_CALLBACK_USER_COMMAND  = 'USER_COMMAND_CXSJ'
      is_variant         = gs_variant
      i_save             = 'A'
    TABLES
      t_outtab           = gt_data
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GETDATA_SJCL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_getdata_sjcl .
  SELECT *
*    zidnum
*    zstatus
*    zsuoyin
*    zjszl
*    zkpjh
*    zposnr
*    bukrs
*    vkorg
*    vtweg
*    kunrg
*    zkhname
*    zkhsh
*    zaddress
*    zkhbank
*    ernam
*    zsaler
*    zwmfph
*    matnr
*    bklas
*    arktx
*    zspmc
*    zggxh
*    atwrt
*    fkimg
*    vrkme
*    zprice
*    zbprice
*    zmount
*    zbmount
*    zse
*    kursk
*    waerk
*    zwaerk
*    ztax
*    zbeizhu
*    zssflbm
*    bezei
*    zzss
    INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM ztvat_data
    WHERE zstatus = '10'
      AND vtweg  = p_vtweg2
      AND zidnum IN s_zid2
      AND bukrs  IN s_bukrs2
      AND zsaler IN s_zsale2
      AND kunrg  IN s_kunrg2
      AND zwmfph IN s_wmfph2.

  SORT gt_data BY zidnum zstatus zsuoyin.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DISPLAY_SJCL
*&---------------------------------------------------------------------*
*& ���ݴ���
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_sjcl .
  PERFORM frm_layout_sjcl.             "���������ʽ
  PERFORM frm_fieldcat_sjcl.
  PERFORM frm_creat_dropdown_values.
  PERFORM frm_creat_event_exits.
  PERFORM frm_output_sjcl.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_LAYOUT_SJCL
*&---------------------------------------------------------------------*
*& ���ݴ���
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_layout_sjcl .
**layout
  wa_layout-box_fname = 'BOX'.
  wa_layout-cwidth_opt = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_FIELDCAT_SJCL
*&---------------------------------------------------------------------*
*& ���ݴ���
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_fieldcat_sjcl .

  set_fieldcat 'ZIDNUM' '��Ʊ����' '' '' '' ''.
  set_fieldcat 'ZSTATUS' '״̬' '' '' '' ''.
  set_fieldcat 'ZSUOYIN' '����' '' '' '' ''.

  gt_fieldcat-fieldname  = 'ZJSZL'.
  gt_fieldcat-coltext    = '��Ʊ����'.
  gt_fieldcat-drdn_field = 'DD_HANDLE'.
  gt_fieldcat-edit       = 'X'.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.

  gt_fieldcat-fieldname  = 'ZKPJH'.
  gt_fieldcat-coltext    = '��Ʊ����'.
  gt_fieldcat-drdn_field = 'DD_KPJH'.
  gt_fieldcat-edit       = 'X'.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.

  set_fieldcat 'ZPOSNR' '��Ʊ����Ŀ' '' '' 'X' ''.
  set_fieldcat 'BUKRS' '��˾����' '' '' '' ''.
  set_fieldcat 'VKORG' '������֯' '' '' '' ''.
  set_fieldcat 'VTWEG' '��������' '' '' '' ''.
  set_fieldcat 'KUNRG' '�ͻ�' '' '' '' ''.
  set_fieldcat 'ZKHNAME' '�ͻ�����' '' '' 'X' ''.
  set_fieldcat 'ZKHSH' '�ͻ�˰��' '' '' 'X' ''.
  set_fieldcat 'ZADDRESS' '�ͻ���ַ�绰' '' '' 'X' ''.
  set_fieldcat 'ZKHBANK' '�ͻ������м��˺�' '' '' 'X' ''.
  set_fieldcat 'ERNAM' '��ƱԱ' '' '' '' ''.
  set_fieldcat 'ZSALER' '����Ա' '' '' '' ''.
  set_fieldcat 'ZWMFPH' '��ó��Ʊ��' '' '' '' ''.
  set_fieldcat 'MATNR' '���ϱ���' '' '' '' ''.
  set_fieldcat 'BKLAS' '������' '' '' '' ''.
  set_fieldcat 'ARKTX' '��������' '' '' '' ''.
  set_fieldcat 'ZSPMC' '��Ʒ����' '' '' 'X' ''.
  set_fieldcat 'ZGGXH' '����ͺ�' '' '' 'X' ''.
  set_fieldcat 'ATWRT' '�ȼ�' '' '' 'X' ''.
  set_fieldcat 'FKIMG' '����' '' '' 'X' ''.
  set_fieldcat 'VRKME' '������λ' '' '' 'X' ''.
  set_fieldcat 'ZPRICE' 'ԭ�Һ�˰����' '' '' 'X' ''.
  set_fieldcat 'ZBPRICE' '��λ�Ҳ���˰����' '' '' 'X' ''.
  set_fieldcat 'ZMOUNT' '���' '' '' 'X' ''.
  set_fieldcat 'ZBMOUNT' '��λ�ҽ��' '' '' 'X' ''.
  set_fieldcat 'ZSE' '˰��' '' '' 'X' ''.
  set_fieldcat 'KURSK' '����' '' '' '' ''.
  set_fieldcat 'WAERK' '����' '' '' '' ''.
  set_fieldcat 'ZWAERK' '��λ����' '' '' '' ''.
  set_fieldcat 'ZTAX' '˰��' '' '' '' ''.
  set_fieldcat 'ZBEIZHU' '��ע' '' '' 'X' ''.
  set_fieldcat 'ZSSFLBM' '˰�շ������' '' '' 'X' ''.
  set_fieldcat 'BEZEI' 'ó�׷�ʽ' '' '' '' ''.
  set_fieldcat 'ZZSS' '����˿��ʶ' '' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_OUTPUT_SJCL
*&---------------------------------------------------------------------*
*& ���ݴ���
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_output_sjcl .
  gs_variant-handle = '1'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      is_layout_lvc            = wa_layout
      it_fieldcat_lvc          = gt_fieldcat[]
*     i_grid_title             = p_title
      i_grid_settings          = gs_grid_settings
      i_callback_pf_status_set = 'ALV_PF_SJCL'
      i_callback_user_command  = 'USER_COMMAND_SJCL'
      is_variant               = gs_variant
      it_events                = gt_events
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PF
*&---------------------------------------------------------------------*
*       GUI״̬����
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI״̬����
*----------------------------------------------------------------------*
FORM alv_pf_sjcl USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ALV_PF_SJCL'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  frm_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command_sjcl  USING r_ucomm LIKE sy-ucomm                      "FRM_USER_COMMAND
                              rs_selfield TYPE slis_selfield.


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = l_grid.

  CALL METHOD l_grid->check_changed_data.
  rs_selfield-refresh = 'X'.

  DATA(lt_yfcl) = gt_data.
  DELETE lt_yfcl WHERE box NE 'X'.
  IF lt_yfcl IS INITIAL.
    MESSAGE '��ѡ������' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CASE r_ucomm.
    WHEN 'CLWC'."�������
      PERFORM save_sjcl USING '2'.


    WHEN 'YFCL'."�˷Ѵ���

      SORT lt_yfcl BY zidnum.
      DELETE ADJACENT DUPLICATES FROM lt_yfcl COMPARING zidnum.
      DATA(yf_line) = lines( lt_yfcl ).
      IF yf_line > 1.
        MESSAGE s017(zjs001) DISPLAY LIKE 'E'. "����ͬʱѡ�������ͬ��Ʊ���ݽ����˷ѷ�̯
        EXIT.
      ENDIF.

      DATA(ls_yfcl) = lt_yfcl[ 1 ].
      CLEAR:gs_yf_item,gt_yf_item.
      LOOP AT gt_data INTO DATA(gs_data) WHERE box = 'X'.
        IF gs_data-vtweg NE '20'.
          MESSAGE e019(zjs001).
        ENDIF.
        gs_yf_item = CORRESPONDING #( gs_data ).
        APPEND gs_yf_item TO gt_yf_item.
      ENDLOOP.

      LOOP AT gt_data INTO gs_data WHERE box = '' AND zidnum = gs_yf_item-zidnum.
        gs_yf_item = CORRESPONDING #( gs_data ).
        APPEND gs_yf_item TO gt_yf_item.
      ENDLOOP.

      "ȡ�۸�ó������
      SELECT SINGLE inco1 INTO zsvat_yfcl_h-zjgtk
      FROM ztvat_invoice INNER JOIN vbrp ON ztvat_invoice~vbeln = vbrp~vbeln
                         INNER JOIN vbkd ON vbrp~aubel = vbkd~vbeln
      WHERE ztvat_invoice~zidnum = gs_data-zidnum.


      CALL SCREEN '9000' STARTING AT 20 1 ENDING AT 180 30.


    WHEN '&DATA_SAVE'.

      PERFORM save_sjcl USING '1'.

    WHEN OTHERS.
  ENDCASE.

  CALL METHOD l_grid->get_frontend_layout
    IMPORTING
      es_layout = wa_layout.

  CALL METHOD l_grid->set_frontend_layout
    EXPORTING
      is_layout = wa_layout.

  CALL METHOD l_grid->refresh_table_display.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CREAT_DROPDOWN_VALUES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_creat_dropdown_values .

  CLEAR gw_ddval.
  gw_ddval-handle = 1.
  gw_ddval-value = '��Ʊ'.
  APPEND gw_ddval TO gt_ddval.

  CLEAR gw_ddval.
  gw_ddval-handle = 1.
  gw_ddval-value = 'רƱ'.
  APPEND gw_ddval TO gt_ddval.

  CLEAR gw_ddval.
  gw_ddval-handle = 2.
  gw_ddval-value = '100'.
  APPEND gw_ddval TO gt_ddval.

  CLEAR gw_ddval.
  gw_ddval-handle = 2.
  gw_ddval-value = '200'.
  APPEND gw_ddval TO gt_ddval.

  CLEAR gw_ddval.
  gw_ddval-handle = 2.
  gw_ddval-value = '300'.
  APPEND gw_ddval TO gt_ddval.



  LOOP AT gt_data INTO wa_data.
    wa_data-dd_handle = '1'.

    wa_data-dd_kpjh   = '2'.

    IF wa_data-zkpjh IS INITIAL.
      wa_data-zkpjh = '100'.
    ENDIF.

    MODIFY gt_data FROM wa_data.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CREAT_EVENT_EXITS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_creat_event_exits .
  gw_events-name = 'CALLER_EXIT'.
  gw_events-form = 'CALLER_EXIT'.
  APPEND gw_events TO gt_events.

  gw_events-name = slis_ev_data_changed.
  gw_events-form = 'ALV_DATA_CHANGED'.
  APPEND gw_events TO gt_events.


ENDFORM.
*---------------------------------------------------------------------*
*���������б�ʹGrid���ڱ���������
*---------------------------------------------------------------------*
FORM caller_exit USING ls_data TYPE slis_data_caller_exit.
  DATA: l_ref_alv TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = l_ref_alv.
  CALL METHOD l_ref_alv->set_drop_down_table
    EXPORTING
      it_drop_down = gt_ddval.
ENDFORM.                    "CALLER_EXIT
*&---------------------------------------------------------------------*
*& Form CREATE_ZIDNUM_LIST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DATA_HB
*&---------------------------------------------------------------------*
FORM create_zidnum_list  TABLES   pt_data_hb LIKE gt_data_nm
                                  pt_tabix   LIKE  lt_tabix
                         CHANGING VALUE(pv_zidnum).

  DATA:zs_flag  TYPE abap_bool.  "����˿��ʶ
  DATA:lv_posnr TYPE ztvat_data-zposnr.
  DATA:lv_beizhu   TYPE ztvat_data-zbeizhu.
  DATA:lv_zbeizhu  TYPE ztvat_data-zbeizhu.
  DATA:lv_zbeizhu1 TYPE ztvat_data-zbeizhu.
  DATA:lv_zsalers  TYPE ztvat_data-zbeizhu.
  DATA:lv_zsaler TYPE ztvat_data-zsaler.
  TYPES:BEGIN OF ly_beizhu,
          zsaler  TYPE ztvat_data-zsaler,
          aubel   TYPE vbrk-vbeln,
          zbeizhu TYPE ztvat_data-zbeizhu,
        END OF ly_beizhu.
  DATA:lt_beizhu TYPE TABLE OF ly_beizhu,
       ls_beizhu TYPE ly_beizhu.

  CLEAR:zs_flag,lv_posnr,lv_beizhu.
  READ TABLE pt_data_hb TRANSPORTING NO FIELDS WITH KEY zzss = abap_true.
  IF sy-subrc = 0.
    zs_flag = abap_true.
  ENDIF.

  READ TABLE pt_data_hb INDEX 1.
  IF pt_data_hb-bukrs = '3500' AND pt_data_hb-vtweg = '10'.
    CLEAR lt_beizhu.
    LOOP AT pt_tabix.
      READ TABLE gt_data_nm INTO gs_data_nm INDEX pt_tabix.
      MOVE-CORRESPONDING gs_data_nm TO ls_beizhu.
      APPEND ls_beizhu TO lt_beizhu.
    ENDLOOP.
*    lt_beizhu = CORRESPONDING #( pt_data_hb[] ).
    CLEAR:lv_zsalers,lv_zbeizhu1, lv_beizhu.
    SORT lt_beizhu BY zsaler zbeizhu.
    LOOP AT lt_beizhu INTO ls_beizhu.
      lv_zsaler = ls_beizhu-zsaler.
      lv_zbeizhu = ls_beizhu-zbeizhu.

      AT NEW aubel.
        lv_zbeizhu1 = |{ lv_zbeizhu1 },{ lv_zbeizhu }|.
      ENDAT.
*
*      AT END OF zsaler.
*        lv_zbeizhu1 = |{ lv_zsaler } { lv_zbeizhu1 }|.
*        lv_beizhu = lv_beizhu && lv_zbeizhu1.
*        CLEAR lv_zbeizhu1.
*      ENDAT.

      AT END OF zsaler.
        lv_zsalers = |{ lv_zsalers } { lv_zsaler }|.
      ENDAT.
    ENDLOOP.
    SHIFT lv_zbeizhu1 LEFT DELETING LEADING ','.
    SHIFT lv_zbeizhu1 RIGHT DELETING TRAILING ','.
    SHIFT lv_zsalers LEFT DELETING LEADING ' '.
    lv_beizhu =  |{ lv_zsalers } { lv_zbeizhu1 }|.
  ENDIF.

  CALL FUNCTION 'ZFM_JS_GETSERIALNUM' "��Ʊ����
    EXPORTING
      object      = 'ZJSYP'
      nr_range_nr = '01'
    IMPORTING
      number      = lv_zidnum.


  wa_ztvat_invoice-zidnum = lv_zidnum.
  pv_zidnum = lv_zidnum.

  wa_ztvat_invoice-zernam = sy-uname.
  wa_ztvat_invoice-zerdat = sy-datum.
  wa_ztvat_invoice-zertim = sy-uzeit.
  IF pt_tabix[] IS NOT INITIAL.
    LOOP AT pt_tabix.
      READ TABLE gt_data_nm INTO gs_data_nm INDEX pt_tabix.
      wa_ztvat_invoice-vbeln  = gs_data_nm-vbeln.
      APPEND wa_ztvat_invoice TO gt_ztvat_invoice.
    ENDLOOP.
  ENDIF.
  CLEAR lv_posnr.
  LOOP AT pt_data_hb ASSIGNING FIELD-SYMBOL(<ls_data>).
    IF pt_tabix[] IS INITIAL.
      wa_ztvat_invoice-vbeln  = <ls_data>-vbeln.
      APPEND wa_ztvat_invoice TO gt_ztvat_invoice.
    ENDIF.


    CALL FUNCTION 'ZFM_JS_GETSERIALNUM'  "����
      EXPORTING
        object      = 'ZJSSY'
        nr_range_nr = '01'
      IMPORTING
        number      = lv_zjssy.

    "д��  data ��
    ADD 10 TO lv_posnr.
    wa_ztvat_data-zidnum  = lv_zidnum.
    wa_ztvat_data-zsuoyin = lv_zjssy.
    wa_ztvat_data-matnr   = <ls_data>-matnr.
    wa_ztvat_data-zstatus = '10'."������
    wa_ztvat_data-zjszl   = <ls_data>-zjszl.
    wa_ztvat_data-zkpjh   = <ls_data>-zkpjh.
    wa_ztvat_data-zposnr  = lv_posnr.
    wa_ztvat_data-bukrs   = <ls_data>-bukrs.
    wa_ztvat_data-vkorg   = <ls_data>-vkorg.
    wa_ztvat_data-vtweg   = <ls_data>-vtweg.
    wa_ztvat_data-kunrg   = <ls_data>-kunrg.
    wa_ztvat_data-zkhname = <ls_data>-zkhname.
    wa_ztvat_data-zkhsh   = <ls_data>-zkhsh.
    wa_ztvat_data-zaddress = <ls_data>-zaddress.
    wa_ztvat_data-zkhbank  = <ls_data>-zbank.
    wa_ztvat_data-ernam    = <ls_data>-ernam.
    wa_ztvat_data-zsaler   = <ls_data>-zsaler.
    wa_ztvat_data-zwmfph   = <ls_data>-zwmfph.
    wa_ztvat_data-bklas    = <ls_data>-bklas.
    wa_ztvat_data-arktx    = <ls_data>-arktx.
    wa_ztvat_data-zspmc    = <ls_data>-zspmc.
    IF <ls_data>-atwrt IS INITIAL.
      wa_ztvat_data-zggxh    = <ls_data>-zggxh.
    ELSE.
      wa_ztvat_data-zggxh    = |{ <ls_data>-zggxh } { <ls_data>-atwrt }��|.
    ENDIF.

    wa_ztvat_data-atwrt    = <ls_data>-atwrt.
    wa_ztvat_data-fkimg    = <ls_data>-fkimg."����
    wa_ztvat_data-vrkme    = <ls_data>-vrkme.
    wa_ztvat_data-zbprice   = <ls_data>-zbmount / <ls_data>-fkimg.
    wa_ztvat_data-zprice   = ( <ls_data>-zbmount + <ls_data>-zse ) / <ls_data>-fkimg.
    wa_ztvat_data-zmount   = <ls_data>-zmount.
    wa_ztvat_data-zbmount  = <ls_data>-zbmount."���
    wa_ztvat_data-zse      = <ls_data>-zse. "˰��
    wa_ztvat_data-kursk    = <ls_data>-kursk.
    wa_ztvat_data-waerk    = <ls_data>-waerk.
    wa_ztvat_data-zwaerk   = <ls_data>-zwaerk.
    wa_ztvat_data-ztax     = <ls_data>-ztax.
    wa_ztvat_data-zbeizhu  = lv_beizhu.
    wa_ztvat_data-zssflbm  = <ls_data>-zssflbm.
    wa_ztvat_data-zernam   = sy-uname.
    wa_ztvat_data-zerdat   = sy-datum.
    wa_ztvat_data-zertim   = sy-uzeit.
    wa_ztvat_data-bezei    = <ls_data>-bezei.
    IF zs_flag = abap_true.
      wa_ztvat_data-zzss   = abap_true.
    ENDIF.
    <ls_data>-zidnum = lv_zidnum.
    <ls_data>-zsuoyin = lv_zjssy.
    APPEND wa_ztvat_data TO gt_ztvat_data.
  ENDLOOP.
  CLEAR:wa_ztvat_invoice.


ENDFORM.

INCLUDE zjsrpt_004_status_9000o01.

INCLUDE zjsrpt_004_user_command_900i01.



*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
FORM alv_data_changed USING pel_data TYPE REF TO cl_alv_changed_data_protocol.
  DATA: l_name(20),ls_cells TYPE lvc_s_modi.
  FIELD-SYMBOLS <fs_value>.
  CLEAR gv_clsj_kpjh.
  LOOP AT pel_data->mt_mod_cells INTO ls_cells.
    IF ls_cells-fieldname = 'ZKPJH' .
      READ TABLE gt_data INTO gs_data_nm INDEX ls_cells-row_id.
      LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>) WHERE zidnum = gs_data_nm-zidnum.
        <fs_data>-zkpjh = ls_cells-value.
      ENDLOOP.
      gv_clsj_kpjh = abap_true.
    ELSEIF ls_cells-fieldname = 'ZJSZL'.
      READ TABLE gt_data INTO gs_data_nm INDEX ls_cells-row_id.
      LOOP AT gt_data ASSIGNING <fs_data> WHERE zidnum = gs_data_nm-zidnum.
        <fs_data>-zjszl = ls_cells-value.
      ENDLOOP.
      gv_clsj_kpjh = abap_true.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_SJCL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM save_sjcl  USING    VALUE(p_type).
  DATA:lt_ztvat_data LIKE TABLE OF ztvat_data WITH HEADER LINE.
  DATA:lt_ztvat_null LIKE TABLE OF ztvat_data WITH HEADER LINE.
  DATA:lt_ztvat_temp LIKE TABLE OF ztvat_data WITH HEADER LINE.

  CLEAR:lt_ztvat_data[],lt_ztvat_data.


*--�����Խ���
  CLEAR:wa_ztvat_data,gt_ztvat_data,wa_data.

  LOOP AT gt_data INTO wa_data .
    MOVE-CORRESPONDING wa_data TO wa_ztvat_data.
    IF wa_data-box = 'X'.
      wa_ztvat_data-zaenam = sy-uname.
      wa_ztvat_data-zaedat = sy-datum.
      wa_ztvat_data-zaetim = sy-uzeit.
      APPEND wa_ztvat_data TO gt_ztvat_data.
    ELSE.
      wa_ztvat_data-zaenam = sy-uname.
      wa_ztvat_data-zaedat = sy-datum.
      wa_ztvat_data-zaetim = sy-uzeit.
      APPEND wa_ztvat_data TO lt_ztvat_null.
    ENDIF.

    CLEAR:wa_ztvat_data,wa_data.
  ENDLOOP.

  IF gv_clsj_kpjh = abap_true OR p_type = '2'.
    LOOP AT gt_ztvat_data INTO wa_ztvat_data.
      LOOP AT lt_ztvat_null INTO DATA(wa_ztvat_null) WHERE zidnum = wa_ztvat_data-zidnum.
        APPEND wa_ztvat_null TO lt_ztvat_temp.
        DELETE lt_ztvat_null.
        CONTINUE.
      ENDLOOP.
    ENDLOOP.

    SORT lt_ztvat_temp BY zidnum zsuoyin.
    DELETE ADJACENT DUPLICATES FROM lt_ztvat_temp COMPARING zidnum zsuoyin.
    APPEND LINES OF lt_ztvat_temp TO gt_ztvat_data.
  ENDIF.

  LOOP AT gt_ztvat_data INTO wa_ztvat_data .
*--У��ؼ��ֶ��Ƿ�Ϊ��
    IF wa_ztvat_data-zkhname IS INITIAL.
      MESSAGE e011(zjs001)."�ؼ��ֶ�������©����ȷ����ά��
    ENDIF.
    IF wa_ztvat_data-zposnr IS INITIAL.
      MESSAGE e011(zjs001)."�ؼ��ֶ�������©����ȷ����ά��
    ENDIF.
    IF wa_ztvat_data-zspmc IS INITIAL.
      MESSAGE e011(zjs001)."�ؼ��ֶ�������©����ȷ����ά��
    ENDIF.
    IF wa_ztvat_data-vrkme IS INITIAL.
      MESSAGE e011(zjs001)."�ؼ��ֶ�������©����ȷ����ά��
    ENDIF.
    IF wa_ztvat_data-fkimg IS INITIAL.
      MESSAGE e011(zjs001)."�ؼ��ֶ�������©����ȷ����ά��
    ENDIF.
    IF wa_ztvat_data-zprice IS INITIAL.
      MESSAGE e011(zjs001)."�ؼ��ֶ�������©����ȷ����ά��
    ENDIF.
    IF wa_ztvat_data-zbmount IS INITIAL.
      MESSAGE e011(zjs001)."�ؼ��ֶ�������©����ȷ����ά��
    ENDIF.
    IF wa_ztvat_data-zjszl IS INITIAL.
      MESSAGE e011(zjs001)."�ؼ��ֶ�������©����ȷ����ά��
    ENDIF.
    IF wa_ztvat_data-zkpjh IS INITIAL.
      MESSAGE e011(zjs001)."�ؼ��ֶ�������©����ȷ����ά��
    ENDIF.
    IF wa_ztvat_data-zssflbm IS INITIAL.
      MESSAGE e011(zjs001)."�ؼ��ֶ�������©����ȷ����ά��
    ENDIF.
  ENDLOOP.

  IF gt_ztvat_data IS NOT INITIAL.
    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_ztvat_data
    FROM ztvat_data
    FOR ALL ENTRIES IN gt_ztvat_data
    WHERE zidnum = gt_ztvat_data-zidnum.
*        CLEAR:GT_ZTVAT_DATA.
    LOOP AT lt_ztvat_data.
      READ TABLE gt_ztvat_data INTO wa_ztvat_data WITH KEY zidnum  = lt_ztvat_data-zidnum zsuoyin = lt_ztvat_data-zsuoyin.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING wa_ztvat_data TO lt_ztvat_data.
        IF p_type = '2'.
          lt_ztvat_data-zstatus = '20'.
        ENDIF.

        lt_ztvat_data-mandt  = sy-mandt.
        lt_ztvat_data-zaenam = sy-uname.
        lt_ztvat_data-zaedat = sy-datum.
        lt_ztvat_data-zaetim = sy-uzeit.
        MODIFY lt_ztvat_data." TRANSPORTING ZSTATUS ZAENAM ZAEDAT ZAETIM ZJSZL ZKPJH.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CLEAR:gt_ztvat_data.

  MODIFY ztvat_data FROM TABLE lt_ztvat_data .
  COMMIT WORK.
  IF sy-subrc = 0.
    MESSAGE s010(zjs001)."����ɹ�
    CLEAR gv_clsj_kpjh.
  ELSE.
    MESSAGE e009(zjs001)."����ʧ��
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZC_HB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zc_hb .
  DATA:gt_data_hb TYPE TABLE OF ty_data.

  DATA:n TYPE i.
  DATA:lv_netwr TYPE vbap-netwr.
  DATA:ls_tabix TYPE  sy-tabix.
  DATA:lv_zkpxe TYPE ztvat_rule-zkpxe.
  DATA:lt_data_temp TYPE TABLE OF ty_data.

  SORT gt_data_nm BY bukrs kunrg tx_flag ztax zbklas zwmfph zprice  zggxh atwrt zspmc.

  lt_data_temp = CORRESPONDING #( gt_data_nm ).

  LOOP AT gt_data_nm INTO wa_data_nm WHERE zidnum IS INITIAL AND msg IS INITIAL.
    ls_tabix = sy-tabix.

    CASE wa_data_nm-vtweg.
      WHEN '10'."��ó
        IF wa_data_nm-bukrs = '3500'.
          lv_zkpxe = '350000'.
        ELSE.
          lv_zkpxe = '10000000'.
        ENDIF.
      WHEN '20'."��ó
        lv_zkpxe = '9999999999999.99'."���޶�
      WHEN OTHERS.
    ENDCASE.

    IF wa_data_nm-zbmount > lv_zkpxe."����SAP��Ʊ���ݾ���ֵ�����޶�
      IF gt_data_hb IS NOT INITIAL.
        PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
        LOOP AT lt_tabix INTO DATA(ls_tabix_temp).
          MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
        ENDLOOP.
        CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
      ENDIF.
      PERFORM frm_create_zidnum USING '2' CHANGING wa_data_nm wa_data_nm-zbmount wa_data_nm-zidnum.
      CLEAR gt_data_hb.
      MODIFY gt_data_nm FROM wa_data_nm.
      n = 1.
      CONTINUE.
    ENDIF.

    READ TABLE gt_rule INTO DATA(ls_rule) WITH KEY kunrg = wa_data_nm-kunrg bukrs = wa_data_nm-bukrs BINARY SEARCH.
*--����Ϣ����͵����ֶβ�Ϊ��,����ó��ȴ������������

    IF sy-subrc = 0."�������߼�
      IF ls_rule-zkpxe IS NOT INITIAL.
        lv_zkpxe = ls_rule-zkpxe."��������޶�
      ENDIF.

      IF ls_rule-zdk = 'X'."����
        READ TABLE gt_data_hb INTO DATA(gs_data_hb) WITH KEY bukrs = wa_data_nm-bukrs
                                                             kunrg = wa_data_nm-kunrg
                                                             vbeln = wa_data_nm-vbeln.
        IF sy-subrc NE 0 AND gt_data_hb IS NOT INITIAL.
          PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
          LOOP AT lt_tabix INTO ls_tabix_temp.
            MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
          ENDLOOP.
          CLEAR:gt_data_hb, lv_netwr,lt_tabix,n,wa_data_nm-zidnum,n.
        ENDIF.

        IF wa_data_nm-vbeln_line > 8.
          CLEAR gt_data_hb.
          wa_data_nm-msg = '�������ݳ���8�У����ֹ�����'.
          LOOP AT lt_data_temp INTO DATA(ls_data_temp) WHERE vbeln = wa_data_nm-vbeln.
            MODIFY gt_data_nm FROM wa_data_nm INDEX sy-tabix TRANSPORTING msg .
          ENDLOOP.
        ELSE.
          LOOP AT lt_data_temp INTO ls_data_temp WHERE vbeln = wa_data_nm-vbeln.
            APPEND sy-tabix TO lt_tabix.
            APPEND ls_data_temp TO gt_data_hb.
          ENDLOOP.
          PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
          LOOP AT lt_tabix INTO ls_tabix_temp.
            MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
          ENDLOOP.
          CLEAR:gt_data_hb, lv_netwr,lt_tabix,n,wa_data_nm-zidnum.
        ENDIF.
      ENDIF.

      IF ls_rule-ztxdk = 'X'.   "��Ϣ����
        IF wa_data_nm-matnr = g_tx_matnr.    "�������Ϣ�� �ߵ����������������߼�

          READ TABLE gt_data_hb INTO gs_data_hb        WITH KEY bukrs = wa_data_nm-bukrs
                                                                kunrg = wa_data_nm-kunrg
                                                                matnr = wa_data_nm-matnr.
          IF sy-subrc NE 0 AND gt_data_hb IS NOT INITIAL.
            PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
            LOOP AT lt_tabix INTO ls_tabix_temp.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
            CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
            LOOP AT lt_data_temp INTO ls_data_temp WHERE vbeln = wa_data_nm-vbeln.
              ls_tabix_temp = sy-tabix.
              APPEND ls_tabix_temp TO lt_tabix.
              APPEND ls_data_temp TO gt_data_hb.
              wa_data_nm-zidnum = '1'.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
          ELSEIF sy-subrc = 0.
            ADD wa_data_nm-fkimg   TO gs_data_hb-fkimg.
            ADD wa_data_nm-zbmount TO gs_data_hb-zbmount.
            ADD wa_data_nm-zmount  TO gs_data_hb-zmount.
            ADD wa_data_nm-zse     TO gs_data_hb-zse.
*            IF wa_data_nm-vtweg = '10'.
*              gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ wa_data_nm-bstkd }|.
*            ENDIF.
            MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
            APPEND ls_tabix TO lt_tabix.
          ELSEIF sy-subrc NE 0 AND gt_data_hb IS INITIAL.
            APPEND wa_data_nm TO gt_data_hb.
            APPEND ls_tabix TO lt_tabix.
            ADD 1 TO n.
          ENDIF.
        ELSE.

          "���������ͬ�ͻ������
          CASE wa_data_nm-vtweg.
            WHEN '10'."��ó
              READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                                    bukrs = wa_data_nm-bukrs
                                                                    ztax = wa_data_nm-ztax
                                                                    zbklas = wa_data_nm-zbklas.
            WHEN '20'."��ó
              READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                                    bukrs = wa_data_nm-bukrs
                                                                    ztax = wa_data_nm-ztax
                                                                    zbklas = wa_data_nm-zbklas
                                                                    zwmfph = wa_data_nm-zwmfph.
            WHEN OTHERS.
          ENDCASE.
          IF sy-subrc NE 0 AND gt_data_hb IS NOT INITIAL.
            PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
            LOOP AT lt_tabix INTO ls_tabix_temp.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
            CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
          ENDIF.

          ADD wa_data_nm-netwr_k TO lv_netwr.
          ADD wa_data_nm-vbeln_line TO n.
          IF lv_netwr > lv_zkpxe OR n > 8.    "������ܺϵ�һ����Ʊ
            PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
            LOOP AT lt_tabix INTO ls_tabix_temp.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
            CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
            ADD wa_data_nm-netwr_k TO lv_netwr.
            LOOP AT lt_data_temp INTO ls_data_temp WHERE vbeln = wa_data_nm-vbeln.
              ls_tabix_temp = sy-tabix.
              APPEND ls_tabix_temp TO lt_tabix.
              APPEND ls_data_temp TO gt_data_hb.
              wa_data_nm-zidnum = '1'.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
            n = wa_data_nm-vbeln_line.
          ELSE.
            SUBTRACT wa_data_nm-vbeln_line FROM n.
            LOOP AT lt_data_temp INTO ls_data_temp WHERE vbeln = wa_data_nm-vbeln.
              ls_tabix = sy-tabix.
              CASE wa_data_nm-vtweg.
                WHEN '10'.
                  IF wa_data_nm-bukrs = '3500'.
                    READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                                   zggxh  = ls_data_temp-zggxh
                                                                   atwrt  = ls_data_temp-atwrt
                                                                   zprice = ls_data_temp-zprice
                                                                   zspmc  = ls_data_temp-zspmc.
                  ELSE.
                    READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                                   zspmc  = ls_data_temp-zspmc
                                                                   zggxh  = ls_data_temp-zggxh.
                  ENDIF.
                WHEN '20'.
                  READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                                 zwmfph = ls_data_temp-zwmfph
                                                                 zggxh  = ls_data_temp-zggxh
                                                                 atwrt  = ls_data_temp-atwrt
                                                                 zprice = ls_data_temp-zprice
                                                                 zspmc  = ls_data_temp-zspmc.
                WHEN OTHERS.
              ENDCASE.
              IF sy-subrc = 0.
                ADD ls_data_temp-fkimg   TO gs_data_hb-fkimg.
                ADD ls_data_temp-zbmount TO gs_data_hb-zbmount.
                ADD ls_data_temp-zmount  TO gs_data_hb-zmount.
                ADD ls_data_temp-zse     TO gs_data_hb-zse.
                IF ls_data_temp-vtweg = '10'.
                  gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ ls_data_temp-bstkd }|.
                ENDIF.
                MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
                APPEND ls_tabix TO lt_tabix.
                wa_data_nm-zidnum = '1'.
                MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix TRANSPORTING zidnum .
              ELSE.
                APPEND ls_tabix TO lt_tabix.
                APPEND ls_data_temp TO gt_data_hb.
                ADD 1 TO n.
                wa_data_nm-zidnum = '1'.
                MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix TRANSPORTING zidnum .
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ls_rule-ztxhbhk = 'X'.
        IF n > 7. "�����������8
          PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
          LOOP AT lt_tabix INTO ls_tabix_temp.
            MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
          ENDLOOP.
          CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
          n = 1.
        ENDIF.

        "���������ͬ�ͻ������
        CASE wa_data_nm-vtweg.
          WHEN '10'."��ó
            IF wa_data_nm-matnr = g_tx_matnr.
              READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                                    bukrs = wa_data_nm-bukrs
                                                                    ztax = wa_data_nm-ztax.
            ELSE.
              READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                                    bukrs = wa_data_nm-bukrs
                                                                    ztax = wa_data_nm-ztax
                                                                    zbklas = wa_data_nm-zbklas.
            ENDIF.

          WHEN '20'."��ó
            IF wa_data_nm-matnr = g_tx_matnr.
              READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                                    bukrs = wa_data_nm-bukrs
                                                                    ztax = wa_data_nm-ztax
                                                                    zwmfph = wa_data_nm-zwmfph.
            ELSE.
              READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                                    bukrs = wa_data_nm-bukrs
                                                                    ztax = wa_data_nm-ztax
                                                                    zbklas = wa_data_nm-zbklas
                                                                    zwmfph = wa_data_nm-zwmfph.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
        IF sy-subrc NE 0 AND gt_data_hb IS NOT INITIAL.
          PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
          LOOP AT lt_tabix INTO ls_tabix_temp.
            MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
          ENDLOOP.
          CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
        ENDIF.
        IF wa_data_nm-matnr = g_tx_matnr.  "�������Ϣ����
          DATA(lv_end_line) = lines( gt_data_hb ).
          IF lv_end_line = 0.
            wa_data_nm-msg = |�ͻ�{ wa_data_nm-kunrg }û��������Ϣ�ϲ�������|.
            MODIFY gt_data_nm FROM wa_data_nm.
            CONTINUE.
          ENDIF.

          READ TABLE gt_data_hb INTO gs_data_hb INDEX lv_end_line.   " �����һ�кϲ�
          IF gs_data_hb-bukrs NE wa_data_nm-bukrs OR gs_data_hb-kunrg NE wa_data_nm-kunrg.
            wa_data_nm-msg = |�ͻ�{ wa_data_nm-kunrg }û��������Ϣ�ϲ�������|.
            MODIFY gt_data_nm FROM wa_data_nm.
            CONTINUE.
          ENDIF.
*            IF SY-SUBRC = 0.
*          ADD wa_data_nm-zbmount TO lv_netwr.
*          IF lv_netwr > lv_zkpxe.
*            ADD wa_data_nm-fkimg   TO gs_data_hb-fkimg.
*            ADD wa_data_nm-zbmount TO gs_data_hb-zbmount.
*            ADD wa_data_nm-zmount  TO gs_data_hb-zmount.
*            ADD wa_data_nm-zse     TO gs_data_hb-zse.
*            MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
*            APPEND ls_tabix TO lt_tabix.
*            PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
*            LOOP AT lt_tabix INTO ls_tabix_temp.
*              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
*            ENDLOOP.
*            CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
*            ADD wa_data_nm-zbmount TO lv_netwr.
**            APPEND wa_data_nm TO gt_data_hb.
**            APPEND ls_tabix TO lt_tabix.
*            n = 1.
*          ELSE.
          ADD wa_data_nm-fkimg   TO gs_data_hb-fkimg.
          ADD wa_data_nm-zbmount TO gs_data_hb-zbmount.
          ADD wa_data_nm-zmount  TO gs_data_hb-zmount.
          ADD wa_data_nm-zse     TO gs_data_hb-zse.
*            IF wa_data_nm-vtweg = '10'.
*              gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ wa_data_nm-bstkd }|.
*            ENDIF.
          MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
          APPEND ls_tabix TO lt_tabix.
*          ENDIF.
        ELSE.

          ADD wa_data_nm-netwr_k TO lv_netwr.
          ADD wa_data_nm-vbeln_line TO n.
          IF lv_netwr > lv_zkpxe OR n > 8.    "������ܺϵ�һ����Ʊ
            PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
            LOOP AT lt_tabix INTO ls_tabix_temp.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
            CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
            LOOP AT lt_data_temp INTO ls_data_temp WHERE vbeln = wa_data_nm-vbeln.
              ls_tabix_temp = sy-tabix.
              APPEND ls_tabix_temp TO lt_tabix.
              APPEND ls_data_temp TO gt_data_hb.
              wa_data_nm-zidnum = '1'.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
            ENDLOOP.
            ADD wa_data_nm-netwr_k TO lv_netwr.
            n = wa_data_nm-vbeln_line.
          ELSE.
            SUBTRACT wa_data_nm-vbeln_line FROM n.
            LOOP AT lt_data_temp INTO ls_data_temp WHERE vbeln = wa_data_nm-vbeln.
              ls_tabix = sy-tabix.
              CASE wa_data_nm-vtweg.
                WHEN '10'.
                  IF wa_data_nm-bukrs = '3500'.
                    READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                                   zggxh  = ls_data_temp-zggxh
                                                                   atwrt  = ls_data_temp-atwrt
                                                                   zprice = ls_data_temp-zprice
                                                                   zspmc  = ls_data_temp-zspmc.
                  ELSE.
                    READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                                   zspmc  = ls_data_temp-zspmc
                                                                   zggxh  = ls_data_temp-zggxh.
                  ENDIF.
                WHEN '20'.
                  READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                                 zwmfph = ls_data_temp-zwmfph
                                                                 zggxh  = ls_data_temp-zggxh
                                                                 atwrt  = ls_data_temp-atwrt
                                                                 zprice = ls_data_temp-zprice
                                                                 zspmc  = ls_data_temp-zspmc.
                WHEN OTHERS.
              ENDCASE.
              IF sy-subrc = 0.
                ADD ls_data_temp-fkimg   TO gs_data_hb-fkimg.
                ADD ls_data_temp-zbmount TO gs_data_hb-zbmount.
                ADD ls_data_temp-zmount  TO gs_data_hb-zmount.
                ADD ls_data_temp-zse     TO gs_data_hb-zse.
                IF ls_data_temp-vtweg = '10'.
                  gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ ls_data_temp-bstkd }|.
                ENDIF.
                MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
                APPEND ls_tabix TO lt_tabix.
                wa_data_nm-zidnum = '1'.
                MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix TRANSPORTING zidnum .
              ELSE.
                APPEND ls_tabix TO lt_tabix.
                APPEND ls_data_temp TO gt_data_hb.
                ADD 1 TO n.
                wa_data_nm-zidnum = '1'.
                MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix TRANSPORTING zidnum .
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    IF ls_rule-ztxdk = abap_false AND ls_rule-zdk = abap_false AND ls_rule-ztxhbhk = abap_false .



      IF n > 7. "�����������8
        PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
        LOOP AT lt_tabix INTO ls_tabix_temp.
          MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
        ENDLOOP.
        CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
        n = 1.
      ENDIF.

      "���������ͬ�ͻ������
      CASE wa_data_nm-vtweg.
        WHEN '10'."��ó
          IF wa_data_nm-matnr = g_tx_matnr.
            READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                                  bukrs = wa_data_nm-bukrs
                                                                  ztax = wa_data_nm-ztax.
          ELSE.
            READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                    bukrs = wa_data_nm-bukrs
                                                    ztax = wa_data_nm-ztax
                                                    zbklas = wa_data_nm-zbklas.
          ENDIF.

        WHEN '20'."��ó
          IF wa_data_nm-matnr = g_tx_matnr.
            READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                                  bukrs = wa_data_nm-bukrs
                                                                  ztax = wa_data_nm-ztax
                                                                  zwmfph = wa_data_nm-zwmfph.
          ELSE.
            READ TABLE gt_data_hb TRANSPORTING NO FIELDS WITH KEY kunrg = wa_data_nm-kunrg
                                                      bukrs = wa_data_nm-bukrs
                                                      ztax = wa_data_nm-ztax
                                                      zbklas = wa_data_nm-zbklas
                                                      zwmfph = wa_data_nm-zwmfph.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
      IF sy-subrc NE 0 AND gt_data_hb IS NOT INITIAL.
        PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.

        LOOP AT lt_tabix INTO ls_tabix_temp.
          MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
        ENDLOOP.
        CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
      ENDIF.

      IF wa_data_nm-matnr = g_tx_matnr.  "�������Ϣ����
        lv_end_line = lines( gt_data_hb ).
        IF lv_end_line = 0.
          wa_data_nm-msg = |�ͻ�{ wa_data_nm-kunrg }û��������Ϣ�ϲ�������|.
          MODIFY gt_data_nm FROM wa_data_nm.
          CONTINUE.
        ENDIF.
        READ TABLE gt_data_hb INTO gs_data_hb INDEX lv_end_line.   " �����һ�кϲ�
        IF gs_data_hb-bukrs NE wa_data_nm-bukrs OR gs_data_hb-kunrg NE wa_data_nm-kunrg.
          wa_data_nm-msg = |�ͻ�{ wa_data_nm-kunrg }û��������Ϣ�ϲ�������|.
          MODIFY gt_data_nm FROM wa_data_nm.
          CONTINUE.
        ENDIF.
        IF  gt_data_hb IS NOT INITIAL.
          READ TABLE gt_data_hb INTO gs_data_hb WITH KEY matnr = wa_data_nm-matnr.
          IF sy-subrc = 0.    "����ϲ��ڱ��Ѿ����� ��Ϣ����
*            ADD wa_data_nm-zbmount TO lv_netwr.
*            IF lv_netwr > lv_zkpxe.
*              ADD wa_data_nm-fkimg   TO gs_data_hb-fkimg.
*              ADD wa_data_nm-zbmount TO gs_data_hb-zbmount.
*              ADD wa_data_nm-zmount  TO gs_data_hb-zmount.
*              ADD wa_data_nm-zse     TO gs_data_hb-zse.
*              MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
*              APPEND ls_tabix TO lt_tabix.
*              PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
*
*              LOOP AT lt_tabix INTO ls_tabix_temp.
*                MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
*              ENDLOOP.
*              CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum.
*              n = 1.
*            ELSE.
            ADD wa_data_nm-fkimg   TO gs_data_hb-fkimg.
            ADD wa_data_nm-zbmount TO gs_data_hb-zbmount.
            ADD wa_data_nm-zmount  TO gs_data_hb-zmount.
            ADD wa_data_nm-zse     TO gs_data_hb-zse.
*              IF wa_data_nm-vtweg = '10'.
*                gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ wa_data_nm-bstkd }|.
*              ENDIF.
            MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
            APPEND ls_tabix TO lt_tabix.
*            ENDIF.
          ELSE.
*            ADD 1 TO n.
            ADD wa_data_nm-zbmount TO lv_netwr.
            APPEND wa_data_nm TO gt_data_hb.
            APPEND ls_tabix TO lt_tabix.
          ENDIF.
          CONTINUE.
        ELSE.
          wa_data_nm-msg = 'û���������Ϻϲ���Ϣ��'.
          MODIFY gt_data_nm FROM wa_data_nm.
        ENDIF.

      ELSE.
        ADD wa_data_nm-netwr_k TO lv_netwr.
        ADD wa_data_nm-vbeln_line TO n.
        IF lv_netwr > lv_zkpxe OR n > 8.    "������ܺϵ�һ����Ʊ
          PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.
          LOOP AT lt_tabix INTO ls_tabix_temp.
            MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
          ENDLOOP.
          CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum.
          LOOP AT lt_data_temp INTO ls_data_temp WHERE vbeln = wa_data_nm-vbeln.
            ls_tabix_temp = sy-tabix.
            APPEND ls_tabix_temp TO lt_tabix.
            APPEND ls_data_temp TO gt_data_hb.
            wa_data_nm-zidnum = '1'.
            MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
          ENDLOOP.
          ADD wa_data_nm-netwr_k TO lv_netwr.
          n = wa_data_nm-vbeln_line.
        ELSE.
          SUBTRACT wa_data_nm-vbeln_line FROM n.
          LOOP AT lt_data_temp INTO ls_data_temp WHERE vbeln = wa_data_nm-vbeln.
            ls_tabix = sy-tabix.
            CASE wa_data_nm-vtweg.
              WHEN '10'.
                IF wa_data_nm-bukrs = '3500'.
                  READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                                 zggxh  = ls_data_temp-zggxh
                                                                 atwrt  = ls_data_temp-atwrt
                                                                 zprice = ls_data_temp-zprice
                                                                 zspmc  = ls_data_temp-zspmc.
                ELSE.
                  READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                                 zspmc  = ls_data_temp-zspmc
                                                                 zggxh  = ls_data_temp-zggxh.
                ENDIF.
              WHEN '20'.
                READ TABLE gt_data_hb INTO gs_data_hb WITH KEY kunrg  = ls_data_temp-kunrg
                                                               zwmfph = ls_data_temp-zwmfph
                                                               zggxh  = ls_data_temp-zggxh
                                                               atwrt  = ls_data_temp-atwrt
                                                               zprice = ls_data_temp-zprice
                                                               zspmc  = ls_data_temp-zspmc.
              WHEN OTHERS.
            ENDCASE.
            IF sy-subrc = 0.
              ADD ls_data_temp-fkimg   TO gs_data_hb-fkimg.
              ADD ls_data_temp-zbmount TO gs_data_hb-zbmount.
              ADD ls_data_temp-zmount  TO gs_data_hb-zmount.
              ADD ls_data_temp-zse     TO gs_data_hb-zse.
              IF ls_data_temp-vtweg = '10'.
                gs_data_hb-zbeizhu = |{ gs_data_hb-zbeizhu },{ ls_data_temp-bstkd }|.
              ENDIF.
              MODIFY gt_data_hb FROM gs_data_hb INDEX sy-tabix.
              APPEND ls_tabix TO lt_tabix.
              wa_data_nm-zidnum = '1'.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix TRANSPORTING zidnum .
            ELSE.
              APPEND ls_tabix TO lt_tabix.
              APPEND ls_data_temp TO gt_data_hb.
              ADD 1 TO n.
              wa_data_nm-zidnum = '1'.
              MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix TRANSPORTING zidnum .
            ENDIF.
          ENDLOOP.
        ENDIF.

      ENDIF.

    ENDIF.
    CLEAR ls_rule.
  ENDLOOP.

  IF gt_data_hb IS NOT INITIAL.
    PERFORM create_zidnum_list TABLES gt_data_hb lt_tabix CHANGING wa_data_nm-zidnum.

    LOOP AT lt_tabix INTO ls_tabix_temp.
      MODIFY gt_data_nm FROM wa_data_nm INDEX ls_tabix_temp TRANSPORTING zidnum .
    ENDLOOP.
    CLEAR:gt_data_hb, lv_netwr,lt_tabix,wa_data_nm-zidnum,n.
  ENDIF.

  MODIFY ztvat_data FROM TABLE gt_ztvat_data.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
  MODIFY ztvat_invoice FROM TABLE gt_ztvat_invoice.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  CLEAR: gt_ztvat_data,gt_ztvat_invoice.

  LOOP AT gt_data_nm INTO wa_data_nm.
    MOVE-CORRESPONDING wa_data_nm TO  wa_data.
    APPEND wa_data TO gt_data.
    CLEAR:wa_data,wa_data_nm.
  ENDLOOP.

ENDFORM.

*GUI Texts
*----------------------------------------------------------
* TITLE_9000 --> �˷Ѵ���


*Selection texts
*----------------------------------------------------------
* P1         �ϲ����
* P2         ��������
* P3         �������ݴ���
* P4         ��˰��Ʊ��Ӧ����
* P_GL         ����δ�ش�����Ʊ����
* P_VTWEG D       .
* P_VTWEG2 D       .
* P_VTWEG3 D       .
* P_ZSS         ����˿ɸѡ��ѡ��
* S_BUKRS D       .
* S_BUKRS2 D       .
* S_BUKRS3 D       .
* S_FKART D       .
* S_FKDAT         ��Ʊ����
* S_KUNRG         �ͻ�
* S_KUNRG2         �ͻ�
* S_KUNRG3         �ͻ�
* S_VBELN         SAP��Ʊ��
* S_VBELN4         SAP��Ʊ��
* S_WMFPH2         ��ó��Ʊ��
* S_WMFPH3         ��ó��Ʊ��
* S_ZDATE4         ��˰����
* S_ZID2         ��Ʊ����
* S_ZID3         ��Ʊ����
* S_ZID4         ��Ʊ����
* S_ZJSDM4         ��˰��Ʊ����
* S_ZJSHM4         ��˰��Ʊ����
* S_ZSALE         ����Ա
* S_ZSALE2         ����Ա
* S_ZSALE3         ����Ա


*Messages
*----------------------------------------------------------
*
* Message class: Hard coded
*   ��ѡ������
*
* Message class: ZJS001
*002   ��ѡ������������Ʊ�ţ�������ѡ��
*003   ��ѡ���ݰ����˻����������ݣ�ϵͳ�������Զ��ϲ������ֹ�ѡ�����ⲿ������
*004   �ͻ�˰����Ϣά����ȫ������ϵ��Աά��
*005   ��ѡ���ݰ����˻������Ʊ���ݣ���ע��ѡ������ϲ���Ʊ����
*007   ��ѡ���д���״̬Ϊ�����е����ݣ���ע��ɸѡ
*008   ͬһ����Ʊ���ݴ���δѡ��ȫ����Ŀ��������빴ѡ����
*009   ����ʧ��
*010   ����ɹ�
*011   �ؼ��ֶ�������©����ȷ����ά��
*014   ѡ�����ݲ���������������������������Ʊ���ݣ���ִ�кϲ�����
*015   �������˻��ϲ���������ȷ���ϲ�����������Ƿ���������
*017   ����ͬʱѡ�������ͬ��Ʊ���ݽ����˷ѷ�̯
*018   ѡ������˰�ʲ�һ�£���������кϲ�
*019   ����óҵ�񣬲���������˷ѷ�̯
*020   ��ѡ���д���״̬Ϊ�Ѵ�������ݣ���ע��ɸѡ
*022   �˻������Ʊ���ݲ�����ִ�в��
*025   ��ѡ���ݰ�����Ϣ��Ʊ���ݣ�����ѡ���˻��ϲ�

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.1 - E.G.Mellodew. 1998-2019. Sap Release 752
