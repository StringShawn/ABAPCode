**************************************************
*��������:��������
*��������: 2019-11-19
*������:HAND-XLL
*������:HAND-XLL
*��������:Report  ZSHXJFI0010
*============================================
*�����¼
*�޸������    �޸�����      �޸���     �޸�����
*DEVK912021   2019-11-19   HAND-XLL   ��������
***************************************************

REPORT zshxjfi0010.

*----------------------------------------------------------------------*
*                    TYPE-POOLS Declaration                            *
*----------------------------------------------------------------------*
TYPE-POOLS: slis.

*----------------------------------------------------------------------*
*                    Table_Work_Areas Declaration                      *
*----------------------------------------------------------------------*
TABLES:bkpf,ska1,knb1,lfb1.
*----------------------------------------------------------------------*
*                    Local Data Types in Program                       *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_alv,
         saknr TYPE  ska1-saknr,  "��ƿ�Ŀ
         txt20 TYPE  skat-txt20,  "��Ŀ����
         kulif TYPE  knb1-kunnr,  "���̱��
         name1 TYPE  kna1-name1,  "��������
         waers TYPE  bsid-waers,   "����
         gqcfx TYPE  c,           "�ڳ�����
         gqcwr TYPE bsid-wrbtr,  "�ڳ�ԭ��
         gqcdm TYPE bsid-dmbtr,  "�ڳ���λ��
         gbjwr TYPE bsid-wrbtr,  "���ڽ跽ԭ��
         gbjdm TYPE bsid-dmbtr,  "���ڽ跽��λ��
         gbdwr TYPE bsid-wrbtr,  "���ڴ���ԭ��
         gbddm TYPE bsid-dmbtr,  "���ڴ�����λ��
         gljwr TYPE bsid-wrbtr,  "�跽�ۼ�ԭ��
         gljdm TYPE bsid-dmbtr,  "�跽�ۼƱ�λ��
         gldwr TYPE bsid-wrbtr,  "�����ۼ�ԭ��
         glddm TYPE bsid-dmbtr,  "�����ۼƱ�λ��
         gqmfx TYPE  c,           "��ĩ����
         gqmwr TYPE bsid-wrbtr,  "��ĩԭ��
         gqmdm TYPE bsid-dmbtr,  "��ĩ��λ��
       END OF ty_alv.
TYPES: BEGIN OF ty_detail,
         belnr TYPE bsid-belnr,                   "ƾ֤���
         saknr TYPE  ska1-saknr,                 "��ƿ�Ŀ
         txt20 TYPE  skat-txt20,                  "��Ŀ����
         blart TYPE bsid-blart,                    "ƾ֤����
         kulif TYPE bsid-kunnr,                    "�ͻ����
         name1 TYPE kna1-name1,                    "�ͻ�����
         budat TYPE bsid-budat,                    "��������
         wrbtr TYPE bsid-wrbtr,                    "ԭ�ҽ��
         waers TYPE bsid-waers,                    "����
         dmbtr TYPE bsid-dmbtr,                    "��λ�ҽ��
         umskz TYPE bsid-umskz,                    "�ر����˱�ʶ
         augbl TYPE bsid-augbl,                    "����ƾ֤
         augdt TYPE bsid-augdt,                    "��������
         sgtxt TYPE bsid-sgtxt,                    "�ı�
       END OF ty_detail.
TYPES: BEGIN OF ty_ska1,
         bukrs TYPE skb1-bukrs,
         saknr TYPE  ska1-saknr,
         mitkz TYPE skb1-mitkz,
         txt20 TYPE  skat-txt20,  "��Ŀ����
       END OF ty_ska1.
TYPES: BEGIN OF ty_khgy,
         bukrs TYPE skb1-bukrs,
         mitkz TYPE skb1-mitkz,
         saknr TYPE  ska1-saknr,  "��ƿ�Ŀ
         kulif TYPE  knb1-kunnr,  "���̱��
         name1 TYPE  kna1-name1,
       END OF ty_khgy.
TYPES: BEGIN OF ty_bsid,
         kulif TYPE bsid-kunnr,                    "�ͻ����
         waers TYPE bsid-waers, "����
         monat TYPE bsad-monat,
         gjahr TYPE bsad-gjahr,
         blart TYPE bsad-blart,  "ƾ֤����
         shkzg TYPE bsid-shkzg, "�����ʶ
         dmbtr TYPE bsid-dmbtr, "��λ�ҽ��
         wrbtr TYPE bsid-wrbtr,  "ƾ֤���ҽ��
         zdmbt TYPE bsid-dmbtr, "����-��λ�ҽ��
         zwrbt TYPE bsid-wrbtr,  "����-ƾ֤���ҽ��
         belnr TYPE bsid-belnr,  "���ƾ֤���
         budat TYPE bsid-budat, "��������
         umskz TYPE bsid-umskz, "�������˱�ʶ
         sgtxt TYPE bsid-sgtxt, "�ı�
         augbl TYPE bsid-augbl, "����ƾ֤
         augdt TYPE bsid-augdt, "��������
         xnegp TYPE bsid-xnegp,  "��ʶ��������
       END OF ty_bsid.

*----------------------------------------------------------------------*
*                Global Internal Tables Declaration                    *
*----------------------------------------------------------------------*
DATA:gt_alv TYPE STANDARD TABLE OF ty_alv,
     gs_alv TYPE ty_alv.
DATA:gt_detail TYPE STANDARD TABLE OF ty_detail,
     gs_detail TYPE ty_detail.
DATA:gt_ucom TYPE STANDARD TABLE OF ty_detail,
     gs_ucom TYPE ty_detail.
DATA:gt_knb1 TYPE STANDARD TABLE OF ty_khgy ,   "�ͻ�
     gs_knb1 TYPE ty_khgy.
DATA:gt_lfb1 TYPE STANDARD TABLE OF ty_khgy,   "��Ӧ��
     gs_lfb1 TYPE ty_khgy.
DATA:gt_ska1 TYPE STANDARD TABLE OF ty_ska1,
     gs_ska1 TYPE ty_ska1.
DATA:gt_skb1 TYPE STANDARD TABLE OF ty_ska1,
     gs_skb1 TYPE ty_ska1.

DATA:gt_bsid TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_bsid TYPE ty_bsid.
DATA:gt_bsad TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_bsad TYPE ty_bsid.
DATA:gt_bsik TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_bsik TYPE ty_bsid.
DATA:gt_bsak TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_bsak TYPE ty_bsid.
DATA:gt_khtail TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�ϸ��
     gs_khtail TYPE ty_bsid.
DATA:gt_gytail TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��ϸ��
     gs_gytail TYPE ty_bsid.

*�ڳ�
DATA:gt_qcid TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_qcid TYPE ty_bsid.
DATA:gt_qcad TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_qcad TYPE ty_bsid.
DATA:gt_qcad_sum TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_qcad_sum TYPE ty_bsid.
DATA:gt_qcik TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_qcik TYPE ty_bsid.
DATA:gt_qcak TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_qcak TYPE ty_bsid.
DATA:gt_qcak_sum TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_qcak_sum TYPE ty_bsid.
*���ڽ跽
DATA:gt_bqidj TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_bqidj TYPE ty_bsid.
DATA:gt_bqadj TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_bqadj TYPE ty_bsid.
DATA:gt_bqadj_sum TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_bqadj_sum TYPE ty_bsid.
DATA:gt_bqikj TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_bqikj TYPE ty_bsid.
DATA:gt_bqakj TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_bqakj TYPE ty_bsid.
DATA:gt_bqakj_sum TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_bqakj_sum TYPE ty_bsid.
*���ڴ���
DATA:gt_bqidd TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_bqidd TYPE ty_bsid.
DATA:gt_bqadd TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_bqadd TYPE ty_bsid.
DATA:gt_bqadd_sum TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_bqadd_sum TYPE ty_bsid.
DATA:gt_bqikd TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_bqikd TYPE ty_bsid.
DATA:gt_bqakd TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_bqakd TYPE ty_bsid.
DATA:gt_bqakd_sum TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_bqakd_sum TYPE ty_bsid.

*�跽�ۼ�
DATA:gt_ljidj TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_ljidj TYPE ty_bsid.
DATA:gt_ljadj TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_ljadj TYPE ty_bsid.
DATA:gt_ljadj_sum TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_ljadj_sum TYPE ty_bsid.
DATA:gt_ljikj TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_ljikj TYPE ty_bsid.
DATA:gt_ljakj TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_ljakj TYPE ty_bsid.
DATA:gt_ljakj_sum TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_ljakj_sum TYPE ty_bsid.

*�����ۼ�
DATA:gt_ljidd TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_ljidd TYPE ty_bsid.
DATA:gt_ljadd TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_ljadd TYPE ty_bsid.
DATA:gt_ljadd_sum TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_ljadd_sum TYPE ty_bsid.
DATA:gt_ljikd TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_ljikd TYPE ty_bsid.
DATA:gt_ljakd TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_ljakd TYPE ty_bsid.
DATA:gt_ljakd_sum TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_ljakd_sum TYPE ty_bsid.

*��ĩ
DATA:gt_qmid TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_qmid TYPE ty_bsid.
DATA:gt_qmad TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_qmad TYPE ty_bsid.
DATA:gt_qmad_sum TYPE STANDARD TABLE OF ty_bsid,   "�ͻ�
     gs_qmad_sum TYPE ty_bsid.
DATA:gt_qmik TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_qmik TYPE ty_bsid.
DATA:gt_qmak TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_qmak TYPE ty_bsid.
DATA:gt_qmak_sum TYPE STANDARD TABLE OF ty_bsid,   "��Ӧ��
     gs_qmak_sum TYPE ty_bsid.

DATA:gucom_kulif TYPE bsid-kunnr,
     gucom_waers TYPE bsid-waers.

*----------------------------------------------------------------------*
*                         CONSTANTS                                    *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                         DEFINE ALV PARAMETER                         *
*----------------------------------------------------------------------*
DATA: gt_fieldcat      TYPE lvc_t_fcat WITH HEADER LINE,
      gt_fidetail      TYPE lvc_t_fcat WITH HEADER LINE,
      gs_layout        TYPE lvc_s_layo,
      gs_layout_detail TYPE lvc_s_layo.

*----------------------------------------------------------------------*
*                    Selection Screen                                  *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b_type WITH FRAME TITLE text-001.
PARAMETERS:
  p_bukrs TYPE t001-bukrs DEFAULT '6100' MEMORY ID buk OBLIGATORY,
*  p_monat TYPE bkpf-monat MEMORY ID mon OBLIGATORY.
  p_gjahr TYPE bkpf-gjahr OBLIGATORY.

SELECT-OPTIONS:
s_monat FOR bkpf-monat MEMORY ID mon OBLIGATORY,
s_saknr FOR ska1-saknr,"OBLIGATORY
s_kunnr FOR knb1-kunnr ,
s_lifnr FOR lfb1-lifnr .
SELECTION-SCREEN END OF BLOCK b_type.


*----------------------------------------------------------------------*
*                   AT SELECTION-SCREEN OUTPUT                         *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  REFRESH s_saknr[].
  CLEAR s_saknr[].
  CLEAR s_saknr.
*----------------------------------------------------------------------*
*                         AT SELECTION-SCREEN.                         *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  DATA:lt_ska11 TYPE STANDARD TABLE OF ty_ska1,
       ls_ska11 TYPE ty_ska1.
  DATA:lt_ska12 TYPE STANDARD TABLE OF ty_ska1,
       ls_ska12 TYPE ty_ska1.
  SELECT
      ska1~saknr
      skb1~mitkz
      FROM ska1
      INNER JOIN skb1  ON skb1~saknr = ska1~saknr
      INTO CORRESPONDING FIELDS OF TABLE gt_ska1.
  IF s_saknr IS NOT INITIAL.
    lt_ska11 = gt_ska1.
    DELETE lt_ska11 WHERE saknr NOT IN s_saknr.
    LOOP AT lt_ska11 INTO ls_ska11.
      DATA l_ms TYPE string.
      IF ls_ska11-mitkz = 'D' OR ls_ska11-mitkz = 'K'.
      ELSE.
        CONCATENATE '�ÿ�Ŀ' ls_ska11-saknr '���ǿ��̿�Ŀ�����飡' INTO l_ms.
        MESSAGE l_ms TYPE 'E'.
      ENDIF.
    ENDLOOP.
  ELSE.
    DATA saknr_low TYPE ska1-saknr.
    DATA saknr_high TYPE ska1-saknr.
    lt_ska12 = gt_ska1.
    DELETE lt_ska12 WHERE mitkz <>  'D' AND  mitkz <>  'K'.
    SORT lt_ska12 BY saknr.
    READ TABLE lt_ska12 INTO ls_ska12 INDEX 1.
    saknr_low = ls_ska12-saknr.
    CLEAR ls_ska12.
    SORT lt_ska12 BY saknr DESCENDING .
    READ TABLE lt_ska12 INTO ls_ska12 INDEX 1.
    saknr_high = ls_ska12-saknr.

    s_saknr-sign     = 'I'.
    s_saknr-option = 'BT'.
    s_saknr-low = saknr_low.
    s_saknr-high = saknr_high.
    APPEND s_saknr.CLEAR s_saknr.
  ENDIF.
*----------------------------------------------------------------------*
*                         INITIALIZATION                               *
*----------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------*
*                      START-OF-SELECTION                              *
*----------------------------------------------------------------------*
START-OF-SELECTION.
* ѡ����Ļ��Ŀ�ж�
  SELECT
     skb1~bukrs      "��˾����
     skb1~saknr      "���ʿ�Ŀ���
     skb1~mitkz      "ͳԦ��Ŀ��ʶ
     skat~txt20
    FROM skb1
    INNER JOIN skat ON skat~saknr = skb1~saknr
    INTO CORRESPONDING FIELDS OF TABLE gt_skb1
    WHERE skb1~bukrs = '6100'  AND skb1~saknr IN s_saknr.

  IF gt_skb1 IS NOT INITIAL.
*�ͻ�
    SELECT
      skb1~mitkz
      knb1~bukrs
      knb1~akont AS saknr
      knb1~kunnr AS kulif
      kna1~name1
      FROM knb1
      INNER JOIN skb1 ON skb1~bukrs = knb1~bukrs  AND skb1~mitkz = 'D'
      INNER JOIN kna1 ON kna1~kunnr = knb1~kunnr
    INTO CORRESPONDING FIELDS OF TABLE gt_knb1
      FOR ALL ENTRIES IN gt_skb1
    WHERE knb1~bukrs = gt_skb1-bukrs  AND knb1~akont = gt_skb1-saknr AND skb1~mitkz = gt_skb1-mitkz  AND knb1~kunnr IN s_kunnr.

*    LOOP AT gt_knb1 INTO gs_knb1.
*      MOVE-CORRESPONDING gs_knb1 TO gs_alv.
*      APPEND gs_alv TO gt_alv.
*      CLEAR gs_alv.
*    ENDLOOP.

*��Ӧ��
    SELECT
    skb1~mitkz
    lfb1~bukrs
    lfb1~akont  AS saknr
    lfb1~lifnr  AS kulif
    lfa1~name1
    FROM lfb1
    INNER JOIN skb1 ON skb1~bukrs = lfb1~bukrs  AND skb1~mitkz = 'K'
    INNER JOIN lfa1 ON lfa1~lifnr = lfb1~lifnr
  INTO CORRESPONDING FIELDS OF TABLE gt_lfb1
    FOR ALL ENTRIES IN gt_skb1
  WHERE lfb1~bukrs = gt_skb1-bukrs  AND lfb1~akont = gt_skb1-saknr AND skb1~mitkz = gt_skb1-mitkz AND lfb1~lifnr IN s_lifnr.
  ENDIF.


  IF gt_knb1 IS NOT INITIAL.
*  BSID ��ƺ��㣺�ͻ��ĵڶ�������
    SELECT
      kunnr AS  kulif
      monat
      gjahr
      blart  "ƾ֤����
      shkzg "�����ʶ
      dmbtr "��λ�ҽ��
      wrbtr  "ƾ֤���ҽ��
      belnr  "���ƾ֤���
      budat "��������
      umskz "�������˱�ʶ
      sgtxt "�ı�
      waers "����
      augbl "����ƾ֤
      augdt "��������
      xnegp  "��ʶ��������
      FROM bsid
      INTO CORRESPONDING FIELDS OF TABLE gt_bsid
      FOR ALL ENTRIES IN gt_knb1
      WHERE kunnr = gt_knb1-kulif. "AND monat LT p_monat.
*  BSAD ��ƺ��㣺�ͻ��ĵڶ����������ѽ�����Ŀ��
    SELECT
      kunnr  AS  kulif
      monat
      gjahr
      blart  "ƾ֤����
      shkzg "�����ʶ
      dmbtr "��λ�ҽ��
      wrbtr  "ƾ֤���ҽ��
      belnr  "���ƾ֤���
      budat "��������
      umskz "�������˱�ʶ
      sgtxt "�ı�
      waers "����
      augbl "����ƾ֤
      augdt "��������
      xnegp  "��ʶ��������
        FROM bsad
        INTO CORRESPONDING FIELDS OF TABLE gt_bsad
        FOR ALL ENTRIES IN gt_knb1
        WHERE kunnr = gt_knb1-kulif. " AND monat LT p_monat.
  ENDIF.

  IF gt_lfb1 IS NOT INITIAL.
*  BSIK ��ƺ��㣺��Ӧ�̵ĵڶ�������
    SELECT
      lifnr  AS  kulif
      monat
      gjahr
      blart  "ƾ֤����
      shkzg  "�����ʶ
      dmbtr  "��λ�ҽ��
      wrbtr   "ƾ֤���ҽ��
      belnr   "���ƾ֤���
      budat  "��������
      umskz  "�������˱�ʶ
      sgtxt  "�ı�
      waers  "����
      augbl  "����ƾ֤
      augdt  "��������
      xnegp   "��ʶ��������
      FROM bsik
      INTO CORRESPONDING FIELDS OF TABLE gt_bsik
            FOR ALL ENTRIES IN gt_lfb1
            WHERE lifnr = gt_lfb1-kulif. " AND monat LT p_monat.
*  BSAK ��ƺ��㣺��Ӧ�̵ĵڶ����������ѽ�����Ŀ��
    SELECT
      lifnr  AS  kulif
      monat
      gjahr
      blart  "ƾ֤����
      shkzg  "�����ʶ
      dmbtr  "��λ�ҽ��
      wrbtr   "ƾ֤���ҽ��
      belnr   "���ƾ֤���
      budat  "��������
      umskz  "�������˱�ʶ
      sgtxt  "�ı�
      waers  "����
      augbl  "����ƾ֤
      augdt  "��������
      xnegp   "��ʶ��������
      FROM bsak
      INTO CORRESPONDING FIELDS OF TABLE gt_bsak
            FOR ALL ENTRIES IN gt_lfb1
            WHERE lifnr = gt_lfb1-kulif. " AND monat LT p_monat.
  ENDIF.
  LOOP AT gt_bsid INTO gs_bsid.
    MOVE-CORRESPONDING gs_bsid TO gs_alv.
    APPEND gs_alv TO gt_alv.
    CLEAR gs_alv.
  ENDLOOP.
  LOOP AT gt_bsad INTO gs_bsad.
    MOVE-CORRESPONDING gs_bsad TO gs_alv.
    APPEND gs_alv TO gt_alv.
    CLEAR gs_alv.
  ENDLOOP.
  LOOP AT gt_bsik INTO gs_bsik.
    MOVE-CORRESPONDING gs_bsik TO gs_alv.
    APPEND gs_alv TO gt_alv.
    CLEAR gs_alv.
  ENDLOOP.
  LOOP AT gt_bsak INTO gs_bsak.
    MOVE-CORRESPONDING gs_bsak TO gs_alv.
    APPEND gs_alv TO gt_alv.
    CLEAR gs_alv.
  ENDLOOP.
  SORT gt_alv BY kulif waers.
  DELETE ADJACENT DUPLICATES FROM gt_alv COMPARING kulif waers.

*SELECT
*      skb1~mitkz
*      knb1~bukrs
*      knb1~akont AS saknr
*      knb1~kunnr AS kulif
*      kna1~name1
*      FROM knb1
*      INNER JOIN skb1 ON skb1~bukrs = knb1~bukrs  AND skb1~mitkz = 'D'
*      INNER JOIN kna1 ON kna1~kunnr = knb1~kunnr
*    INTO CORRESPONDING FIELDS OF TABLE gt_knb1
*      FOR ALL ENTRIES IN gt_skb1
*    WHERE knb1~bukrs = gt_skb1-bukrs  AND knb1~akont = gt_skb1-saknr AND skb1~mitkz = gt_skb1-mitkz  AND knb1~kunnr IN s_kunnr.
*
*gt_knb1  gt_lfb1

  LOOP AT gt_alv INTO gs_alv.
    READ TABLE gt_knb1 INTO gs_knb1 WITH KEY kulif = gs_alv-kulif.
    IF sy-subrc = 0.
      gs_alv-name1 = gs_knb1-name1.
      gs_alv-saknr = gs_knb1-saknr.
    ENDIF.
    READ TABLE gt_lfb1 INTO gs_lfb1 WITH KEY kulif = gs_alv-kulif.
    IF sy-subrc = 0.
      gs_alv-name1 = gs_lfb1-name1.
      gs_alv-saknr = gs_lfb1-saknr.
    ENDIF.
    READ TABLE gt_skb1 INTO gs_skb1 WITH KEY saknr = gs_alv-saknr.
    IF sy-subrc = 0.
      gs_alv-txt20 = gs_skb1-txt20.
    ENDIF.
    MODIFY gt_alv FROM gs_alv.
    CLEAR gs_alv.
  ENDLOOP.
*�ڳ���С��ѡ����Ļ����ڼ�
  gt_qcid = gt_bsid.
  DELETE gt_qcid WHERE gjahr GT p_gjahr.  "ɾ��������ȵ�
  DELETE gt_qcid WHERE gjahr EQ p_gjahr AND monat GE s_monat-low. "ɾ������� ���ڵ����ڼ�lowֵ��
  SORT gt_qcid BY kulif waers.
  LOOP AT gt_qcid INTO gs_qcid.
    IF gs_qcid-shkzg = 'H'.
      gs_qcid-dmbtr = gs_qcid-dmbtr * -1. "��λ�ҽ��.
      gs_qcid-wrbtr = gs_qcid-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_qcid-zdmbt = gs_qcid-dmbtr. "��λ�ҽ��-����
*      gs_qcid-zwrbt = gs_qcid-wrbtr. "ƾ֤���ҽ��-����
**       APPEND gs_qcid TO gT_qcid_SUM.
*    ENDAT.
    MODIFY gt_qcid FROM gs_qcid.
    CLEAR gs_qcid.
  ENDLOOP.
  gt_qcad = gt_bsad.
  DELETE gt_qcad WHERE gjahr GT p_gjahr.
  DELETE gt_qcad WHERE gjahr EQ p_gjahr AND monat GE s_monat-low.
  SORT gt_qcad BY kulif  waers.
  LOOP AT gt_qcad INTO gs_qcad.
    IF gs_qcad-shkzg = 'H'.
      gs_qcad-dmbtr = gs_qcad-dmbtr * -1. "��λ�ҽ��.
      gs_qcad-wrbtr = gs_qcad-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_qcad-zdmbt = gs_qcad-dmbtr. "��λ�ҽ��-����
*      gs_qcad-zwrbt = gs_qcad-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_qcad FROM gs_qcad.
    CLEAR gs_qcad.
  ENDLOOP.

  APPEND LINES OF gt_qcid TO gt_qcad.
  SORT gt_qcad BY kulif waers.
  LOOP AT gt_qcad INTO gs_qcad.
    AT END OF waers.
      SUM.
      gs_qcad-zdmbt = gs_qcad-dmbtr. "��λ�ҽ��-����
      gs_qcad-zwrbt = gs_qcad-wrbtr. "ƾ֤���ҽ��-����
      APPEND gs_qcad TO gt_qcad_sum.
    ENDAT.
    MODIFY gt_qcad FROM gs_qcad.
    CLEAR gs_qcad.
  ENDLOOP.
  LOOP AT gt_alv INTO gs_alv.
    READ TABLE gt_qcad_sum INTO gs_qcad_sum WITH KEY kulif = gs_alv-kulif waers = gs_alv-waers.
    IF sy-subrc = 0.
      gs_alv-waers = gs_qcad_sum-waers.
      gs_alv-gqcdm = gs_qcad_sum-zdmbt.
      gs_alv-gqcwr = gs_qcad_sum-zwrbt.
    ENDIF.
    IF gs_alv-gqcdm > 0 AND gs_alv-gqcwr > 0.
      gs_alv-gqcfx = '��'.
    ELSEIF gs_alv-gqcdm < 0 AND gs_alv-gqcwr < 0.
      gs_alv-gqcfx = '��'.
    ELSEIF gs_alv-gqcdm = 0 AND gs_alv-gqcwr = 0.
      gs_alv-gqcfx = 'ƽ'.
    ELSEIF gs_alv-gqcdm IS INITIAL AND gs_alv-gqcwr IS INITIAL.
      gs_alv-gqcfx = 'ƽ'.
    ENDIF.
    MODIFY gt_alv FROM gs_alv.
    CLEAR gs_alv.

  ENDLOOP.

  gt_qcik = gt_bsik.
  DELETE gt_qcik WHERE gjahr GT p_gjahr.
  DELETE gt_qcik WHERE gjahr EQ p_gjahr AND monat GE s_monat-low.
  SORT gt_qcik BY kulif waers.
  LOOP AT gt_qcik INTO gs_qcik.
    IF gs_qcik-shkzg = 'H'.
      gs_qcik-dmbtr = gs_qcik-dmbtr * -1. "��λ�ҽ��.
      gs_qcik-wrbtr = gs_qcik-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_qcik-zdmbt = gs_qcik-dmbtr. "��λ�ҽ��-����
*      gs_qcik-zwrbt = gs_qcik-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_qcik FROM gs_qcik.
    CLEAR gs_qcik.
  ENDLOOP.
  gt_qcak = gt_bsak.
  DELETE gt_qcak WHERE gjahr GT p_gjahr.
  DELETE gt_qcak WHERE gjahr EQ p_gjahr AND monat GE s_monat-low.
  SORT gt_qcak BY kulif  waers.
  LOOP AT gt_qcak INTO gs_qcak.
    IF gs_qcak-shkzg = 'H'.
      gs_qcak-dmbtr = gs_qcak-dmbtr * -1. "��λ�ҽ��.
      gs_qcak-wrbtr = gs_qcak-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_qcak-zdmbt = gs_qcak-dmbtr. "��λ�ҽ��-����
*      gs_qcak-zwrbt = gs_qcak-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_qcak FROM gs_qcak.
    CLEAR gs_qcak.
  ENDLOOP.

  APPEND LINES OF gt_qcik TO gt_qcak.
  SORT gt_qcak BY kulif waers.
  LOOP AT gt_qcak INTO gs_qcak.
    AT END OF waers.
      SUM.
      gs_qcak-zdmbt = gs_qcak-dmbtr. "��λ�ҽ��-����
      gs_qcak-zwrbt = gs_qcak-wrbtr. "ƾ֤���ҽ��-����
      APPEND gs_qcak TO gt_qcak_sum.
    ENDAT.
    MODIFY gt_qcak FROM gs_qcak.
    CLEAR gs_qcak.
  ENDLOOP.
  LOOP AT gt_alv INTO gs_alv.
    READ TABLE gt_qcak_sum INTO gs_qcak_sum WITH KEY kulif = gs_alv-kulif waers = gs_alv-waers.
    IF sy-subrc = 0.
      gs_alv-waers = gs_qcak_sum-waers.
      gs_alv-gqcdm = gs_qcak_sum-zdmbt.
      gs_alv-gqcwr = gs_qcak_sum-zwrbt.
    ENDIF.
    IF gs_alv-gqcdm > 0 AND gs_alv-gqcwr > 0.
      gs_alv-gqcfx = '��'.
    ELSEIF gs_alv-gqcdm < 0 AND gs_alv-gqcwr < 0.
      gs_alv-gqcfx = '��'.
    ELSEIF gs_alv-gqcdm = 0 AND gs_alv-gqcwr = 0.
      gs_alv-gqcfx = 'ƽ'.
    ELSEIF gs_alv-gqcdm IS INITIAL AND gs_alv-gqcwr IS INITIAL.
      gs_alv-gqcfx = 'ƽ'.
    ENDIF.
    MODIFY gt_alv FROM gs_alv.
    CLEAR gs_alv.

  ENDLOOP.

*���ڽ跽��ѡ����Ļ ������ڼ��
*�ͻ�
  gt_bqidj = gt_bsid.
  DELETE gt_bqidj WHERE gjahr NE p_gjahr.  "ɾ���Ǳ���ȵ�
  DELETE gt_bqidj WHERE monat NOT IN s_monat OR shkzg = space. "ɾ���Ǳ��ڼ��
  DELETE gt_bqidj WHERE shkzg = 'S' AND xnegp = 'X'.
  DELETE gt_bqidj WHERE shkzg = 'H' AND  xnegp = space.
  SORT gt_bqidj BY kulif waers.
  LOOP AT gt_bqidj INTO gs_bqidj.
    IF gs_bqidj-shkzg = 'H' AND gs_bqidj-xnegp = 'X'.
      gs_bqidj-dmbtr = gs_bqidj-dmbtr * -1. "��λ�ҽ��.
      gs_bqidj-wrbtr = gs_bqidj-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_bqidj-zdmbt = gs_bqidj-dmbtr. "��λ�ҽ��-����
*      gs_bqidj-zwrbt = gs_bqidj-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_bqidj FROM gs_bqidj.
    CLEAR gs_bqidj.
  ENDLOOP.
  gt_bqadj = gt_bsad.
  DELETE gt_bqadj WHERE gjahr NE p_gjahr.
  DELETE gt_bqadj WHERE monat NOT IN s_monat OR shkzg = space.
  DELETE gt_bqadj WHERE shkzg = 'S' AND xnegp = 'X'.
  DELETE gt_bqadj WHERE shkzg = 'H' AND  xnegp = space.
  SORT gt_bqadj BY kulif waers.
  LOOP AT gt_bqadj INTO gs_bqadj.
    IF gs_bqadj-shkzg = 'H' AND gs_bqadj-xnegp = 'X'.
      gs_bqadj-dmbtr = gs_bqadj-dmbtr * -1. "��λ�ҽ��.
      gs_bqadj-wrbtr = gs_bqadj-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_bqadj-zdmbt = gs_bqadj-dmbtr. "��λ�ҽ��-����
*      gs_bqadj-zwrbt = gs_bqadj-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_bqadj FROM gs_bqadj.
    CLEAR gs_bqadj.
  ENDLOOP.

  APPEND LINES OF gt_bqidj TO gt_bqadj.
  SORT gt_bqadj BY kulif waers.
  LOOP AT gt_bqadj INTO gs_bqadj.
    AT END OF waers.
      SUM.
      gs_bqadj-zdmbt = gs_bqadj-dmbtr. "��λ�ҽ��-����
      gs_bqadj-zwrbt = gs_bqadj-wrbtr. "ƾ֤���ҽ��-����
      APPEND gs_bqadj TO gt_bqadj_sum.
    ENDAT.
    MODIFY gt_bqadj FROM gs_bqadj.
    CLEAR gs_bqadj.
  ENDLOOP.
  LOOP AT gt_alv INTO gs_alv.
    READ TABLE gt_bqadj_sum INTO gs_bqadj_sum WITH KEY kulif = gs_alv-kulif waers = gs_alv-waers.
    IF sy-subrc = 0.
      gs_alv-waers = gs_bqadj_sum-waers.
      gs_alv-gbjdm = gs_bqadj_sum-zdmbt.
      gs_alv-gbjwr = gs_bqadj_sum-zwrbt.
      MODIFY gt_alv FROM gs_alv.
      CLEAR gs_alv.
    ENDIF.
  ENDLOOP.

*��Ӧ��
  gt_bqikj = gt_bsik.
  DELETE gt_bqikj WHERE gjahr NE p_gjahr.
  DELETE gt_bqikj WHERE monat NOT IN s_monat OR shkzg = space.
  DELETE gt_bqikj WHERE shkzg = 'S' AND xnegp = 'X'.
  DELETE gt_bqikj WHERE shkzg = 'H' AND  xnegp = space.
  SORT gt_bqikj BY kulif waers.
  LOOP AT gt_bqikj INTO gs_bqikj.
    IF gs_bqikj-shkzg = 'H' AND gs_bqikj-xnegp = 'X'.
      gs_bqikj-dmbtr = gs_bqikj-dmbtr * -1. "��λ�ҽ��.
      gs_bqikj-wrbtr = gs_bqikj-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_bqikj-zdmbt = gs_bqikj-dmbtr. "��λ�ҽ��-����
*      gs_bqikj-zwrbt = gs_bqikj-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_bqikj FROM gs_bqikj.
    CLEAR gs_bqikj.
  ENDLOOP.
  gt_bqakj = gt_bsak.
  DELETE gt_bqakj WHERE gjahr NE p_gjahr.
  DELETE gt_bqakj WHERE monat NOT IN s_monat OR shkzg = space.
  DELETE gt_bqakj WHERE shkzg = 'S' AND xnegp = 'X'.
  DELETE gt_bqakj WHERE shkzg = 'H' AND  xnegp = space.
  SORT gt_bqakj BY kulif waers.
  LOOP AT gt_bqakj INTO gs_bqakj.
    IF gs_bqakj-shkzg = 'H' AND gs_bqakj-xnegp = 'X'.
      gs_bqakj-dmbtr = gs_bqakj-dmbtr * -1. "��λ�ҽ��.
      gs_bqakj-wrbtr = gs_bqakj-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_bqakj-zdmbt = gs_bqakj-dmbtr. "��λ�ҽ��-����
*      gs_bqakj-zwrbt = gs_bqakj-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_bqakj FROM gs_bqakj.
    CLEAR gs_bqakj.
  ENDLOOP.

  APPEND LINES OF gt_bqikj TO gt_bqakj.
  SORT gt_bqakj BY kulif waers.
  LOOP AT gt_bqakj INTO gs_bqakj.
    AT END OF waers.
      SUM.
      gs_bqakj-zdmbt = gs_bqakj-dmbtr. "��λ�ҽ��-����
      gs_bqakj-zwrbt = gs_bqakj-wrbtr. "ƾ֤���ҽ��-����
      APPEND gs_bqakj TO gt_bqakj_sum.
    ENDAT.
    MODIFY gt_bqakj FROM gs_bqakj.
    CLEAR gs_bqakj.
  ENDLOOP.
  LOOP AT gt_alv INTO gs_alv.
    READ TABLE gt_bqakj_sum INTO gs_bqakj_sum WITH KEY kulif = gs_alv-kulif waers = gs_alv-waers.
    IF sy-subrc = 0.
      gs_alv-waers = gs_bqakj_sum-waers.
      gs_alv-gbjdm = gs_bqakj_sum-zdmbt.
      gs_alv-gbjwr = gs_bqakj_sum-zwrbt.
      MODIFY gt_alv FROM gs_alv.
      CLEAR gs_alv.
    ENDIF.
  ENDLOOP.
*���ڴ�����monat IN s_monat
*�ͻ�
  gt_bqidd = gt_bsid.
  DELETE gt_bqidd WHERE gjahr NE p_gjahr.
  DELETE gt_bqidd WHERE monat NOT IN s_monat OR shkzg = space.
  DELETE gt_bqidd WHERE shkzg = 'H' AND xnegp = 'X'.
  DELETE gt_bqidd WHERE shkzg = 'S' AND  xnegp = space.
  SORT gt_bqidd BY kulif waers.
  LOOP AT gt_bqidd INTO gs_bqidd.
    IF gs_bqidd-shkzg = 'S' AND gs_bqidd-xnegp = 'X'.
      gs_bqidd-dmbtr = gs_bqidd-dmbtr * -1. "��λ�ҽ��.
      gs_bqidd-wrbtr = gs_bqidd-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_bqidd-zdmbt = gs_bqidd-dmbtr. "��λ�ҽ��-����
*      gs_bqidd-zwrbt = gs_bqidd-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_bqidd FROM gs_bqidd.
    CLEAR gs_bqidd.
  ENDLOOP.
  gt_bqadd = gt_bsad.
  DELETE gt_bqadd WHERE gjahr NE p_gjahr.
  DELETE gt_bqadd WHERE monat NOT IN s_monat OR shkzg = space.
  DELETE gt_bqadd WHERE shkzg = 'H' AND xnegp = 'X'.
  DELETE gt_bqadd WHERE shkzg = 'S' AND  xnegp = space.
  SORT gt_bqadd BY kulif waers.
  LOOP AT gt_bqadd INTO gs_bqadd.
    IF gs_bqadd-shkzg = 'S' AND gs_bqadd-xnegp = 'X'.
      gs_bqadd-dmbtr = gs_bqadd-dmbtr * -1. "��λ�ҽ��.
      gs_bqadd-wrbtr = gs_bqadd-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_bqadd-zdmbt = gs_bqadd-dmbtr. "��λ�ҽ��-����
*      gs_bqadd-zwrbt = gs_bqadd-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_bqadd FROM gs_bqadd.
    CLEAR gs_bqadd.
  ENDLOOP.

  APPEND LINES OF gt_bqidd TO gt_bqadd.
  SORT gt_bqadd BY kulif waers.
  LOOP AT gt_bqadd INTO gs_bqadd.
    AT END OF waers.
      SUM.
      gs_bqadd-zdmbt = gs_bqadd-dmbtr. "��λ�ҽ��-����
      gs_bqadd-zwrbt = gs_bqadd-wrbtr. "ƾ֤���ҽ��-����
      APPEND gs_bqadd TO gt_bqadd_sum.
    ENDAT.
    MODIFY gt_bqadd FROM gs_bqadd.
    CLEAR gs_bqadd.
  ENDLOOP.
  LOOP AT gt_alv INTO gs_alv.
    READ TABLE gt_bqadd_sum INTO gs_bqadd_sum WITH KEY kulif = gs_alv-kulif waers = gs_alv-waers.
    IF sy-subrc = 0.
      gs_alv-waers = gs_bqadd_sum-waers.
      gs_alv-gbddm = gs_bqadd_sum-zdmbt.
      gs_alv-gbdwr = gs_bqadd_sum-zwrbt.
      MODIFY gt_alv FROM gs_alv.
      CLEAR gs_alv.
    ENDIF.
  ENDLOOP.

*��Ӧ��
  gt_bqikd = gt_bsik.
  DELETE gt_bqikd WHERE gjahr NE p_gjahr.
  DELETE gt_bqikd WHERE monat NOT IN s_monat OR shkzg = space.
  DELETE gt_bqikd WHERE shkzg = 'H' AND xnegp = 'X'.
  DELETE gt_bqikd WHERE shkzg = 'S' AND  xnegp = space.
  SORT gt_bqikd BY kulif waers.
  LOOP AT gt_bqikd INTO gs_bqikd.
    IF gs_bqikd-shkzg = 'S' AND gs_bqikd-xnegp = 'X'.
      gs_bqikd-dmbtr = gs_bqikd-dmbtr * -1. "��λ�ҽ��.
      gs_bqikd-wrbtr = gs_bqikd-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_bqikd-zdmbt = gs_bqikd-dmbtr. "��λ�ҽ��-����
*      gs_bqikd-zwrbt = gs_bqikd-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_bqikd FROM gs_bqikd.
    CLEAR gs_bqikd.
  ENDLOOP.
  gt_bqakd = gt_bsak.
  DELETE gt_bqakd WHERE gjahr NE p_gjahr.
  DELETE gt_bqakd WHERE monat NOT IN s_monat OR shkzg = space.
  DELETE gt_bqakd WHERE shkzg = 'H' AND xnegp = 'X'.
  DELETE gt_bqakd WHERE shkzg = 'S' AND  xnegp = space.
  SORT gt_bqakd BY kulif waers.
  LOOP AT gt_bqakd INTO gs_bqakd.
    IF gs_bqakd-shkzg = 'S' AND gs_bqakd-xnegp = 'X'.
      gs_bqakd-dmbtr = gs_bqakd-dmbtr * -1. "��λ�ҽ��.
      gs_bqakd-wrbtr = gs_bqakd-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_bqakd-zdmbt = gs_bqakd-dmbtr. "��λ�ҽ��-����
*      gs_bqakd-zwrbt = gs_bqakd-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_bqakd FROM gs_bqakd.
    CLEAR gs_bqakd.
  ENDLOOP.

  APPEND LINES OF gt_bqikd TO gt_bqakd.
  SORT gt_bqakd BY kulif waers.
  LOOP AT gt_bqakd INTO gs_bqakd.
    AT END OF waers.
      SUM.
      gs_bqakd-zdmbt = gs_bqakd-dmbtr. "��λ�ҽ��-����
      gs_bqakd-zwrbt = gs_bqakd-wrbtr. "ƾ֤���ҽ��-����
      APPEND gs_bqakd TO gt_bqakd_sum.
    ENDAT.
    MODIFY gt_bqakd FROM gs_bqakd.
    CLEAR gs_bqakd.
  ENDLOOP.
  LOOP AT gt_alv INTO gs_alv.
    READ TABLE gt_bqakd_sum INTO gs_bqakd_sum WITH KEY kulif = gs_alv-kulif waers = gs_alv-waers.
    IF sy-subrc = 0.
      gs_alv-waers = gs_bqakd_sum-waers.
      gs_alv-gbddm = gs_bqakd_sum-zdmbt.
      gs_alv-gbdwr = gs_bqakd_sum-zwrbt.
      MODIFY gt_alv FROM gs_alv.
      CLEAR gs_alv.
    ENDIF.
  ENDLOOP.

*�跽�ۼƣ������1-high
*�ͻ�
  gt_ljidj = gt_bsid.
  DELETE gt_ljidj WHERE gjahr NE p_gjahr.  "ɾ���Ǳ���ȵ�
  DELETE gt_ljidj WHERE monat GT s_monat-high OR shkzg = space. "ɾ��������д���high
  DELETE gt_ljidj WHERE shkzg = 'S' AND xnegp = 'X'.
  DELETE gt_ljidj WHERE shkzg = 'H' AND  xnegp = space.
  SORT gt_ljidj BY kulif waers.
  LOOP AT gt_ljidj INTO gs_ljidj.
    IF gs_ljidj-shkzg = 'H' AND gs_ljidj-xnegp = 'X'.
      gs_ljidj-dmbtr = gs_ljidj-dmbtr * -1. "��λ�ҽ��.
      gs_ljidj-wrbtr = gs_ljidj-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_ljidj-zdmbt = gs_ljidj-dmbtr. "��λ�ҽ��-����
*      gs_ljidj-zwrbt = gs_ljidj-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_ljidj FROM gs_ljidj.
    CLEAR gs_ljidj.
  ENDLOOP.
  gt_ljadj = gt_bsad.
  DELETE gt_ljadj WHERE gjahr NE p_gjahr.
  DELETE gt_ljadj WHERE monat GT s_monat-high OR shkzg = space.
  DELETE gt_ljadj WHERE shkzg = 'S' AND xnegp = 'X'.
  DELETE gt_ljadj WHERE shkzg = 'H' AND  xnegp = space.
  SORT gt_ljadj BY kulif waers.
  LOOP AT gt_ljadj INTO gs_ljadj.
    IF gs_ljadj-shkzg = 'H' AND gs_ljadj-xnegp = 'X'.
      gs_ljadj-dmbtr = gs_ljadj-dmbtr * -1. "��λ�ҽ��.
      gs_ljadj-wrbtr = gs_ljadj-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_ljadj-zdmbt = gs_ljadj-dmbtr. "��λ�ҽ��-����
*      gs_ljadj-zwrbt = gs_ljadj-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.

    MODIFY gt_ljadj FROM gs_ljadj.
    CLEAR gs_ljadj.
  ENDLOOP.

  APPEND LINES OF gt_ljidj TO gt_ljadj.
  SORT gt_ljadj BY kulif waers.
  LOOP AT gt_ljadj INTO gs_ljadj.
    AT END OF waers.
      SUM.
      gs_ljadj-zdmbt = gs_ljadj-dmbtr. "��λ�ҽ��-����
      gs_ljadj-zwrbt = gs_ljadj-wrbtr. "ƾ֤���ҽ��-����
      APPEND gs_ljadj TO gt_ljadj_sum.
    ENDAT.
    MODIFY gt_ljadj FROM gs_ljadj.
    CLEAR gs_ljadj.
  ENDLOOP.
  LOOP AT gt_alv INTO gs_alv.
    READ TABLE gt_ljadj_sum INTO gs_ljadj_sum WITH KEY kulif = gs_alv-kulif waers = gs_alv-waers.
    IF sy-subrc = 0.
      gs_alv-waers = gs_ljadj_sum-waers.
      gs_alv-gljdm = gs_ljadj_sum-zdmbt.
      gs_alv-gljwr = gs_ljadj_sum-zwrbt.
      MODIFY gt_alv FROM gs_alv.
      CLEAR gs_alv.
    ENDIF.
  ENDLOOP.

*��Ӧ��
  gt_ljikj = gt_bsik.
  DELETE gt_ljikj WHERE gjahr NE p_gjahr.
  DELETE gt_ljikj WHERE monat GT s_monat-high OR shkzg = space.
  DELETE gt_ljikj WHERE shkzg = 'S' AND xnegp = 'X'.
  DELETE gt_ljikj WHERE shkzg = 'H' AND  xnegp = space.
  SORT gt_ljikj BY kulif waers.
  LOOP AT gt_ljikj INTO gs_ljikj.
    IF gs_ljikj-shkzg = 'H' AND gs_ljikj-xnegp = 'X'.
      gs_ljikj-dmbtr = gs_ljikj-dmbtr * -1. "��λ�ҽ��.
      gs_ljikj-wrbtr = gs_ljikj-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_ljikj-zdmbt = gs_ljikj-dmbtr. "��λ�ҽ��-����
*      gs_ljikj-zwrbt = gs_ljikj-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_ljikj FROM gs_ljikj.
    CLEAR gs_ljikj.
  ENDLOOP.
  gt_ljakj = gt_bsak.
  DELETE gt_ljakj WHERE gjahr NE p_gjahr.
  DELETE gt_ljakj WHERE monat GT s_monat-high OR shkzg = space.
  DELETE gt_ljakj WHERE shkzg = 'S' AND xnegp = 'X'.
  DELETE gt_ljakj WHERE shkzg = 'H' AND  xnegp = space.
  SORT gt_ljakj BY kulif waers.
  LOOP AT gt_ljakj INTO gs_ljakj.
    IF gs_ljakj-shkzg = 'H' AND gs_ljakj-xnegp = 'X'.
      gs_ljakj-dmbtr = gs_ljakj-dmbtr * -1. "��λ�ҽ��.
      gs_ljakj-wrbtr = gs_ljakj-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_ljakj-zdmbt = gs_ljakj-dmbtr. "��λ�ҽ��-����
*      gs_ljakj-zwrbt = gs_ljakj-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_ljakj FROM gs_ljakj.
    CLEAR gs_ljakj.
  ENDLOOP.

  APPEND LINES OF gt_ljikj TO gt_ljakj.
  SORT gt_ljakj BY kulif waers.
  LOOP AT gt_ljakj INTO gs_ljakj.
    AT END OF waers.
      SUM.
      gs_ljakj-zdmbt = gs_ljakj-dmbtr. "��λ�ҽ��-����
      gs_ljakj-zwrbt = gs_ljakj-wrbtr. "ƾ֤���ҽ��-����
      APPEND gs_ljakj TO gt_ljakj_sum.
    ENDAT.
    MODIFY gt_ljakj FROM gs_ljakj.
    CLEAR gs_ljakj.
  ENDLOOP.
  LOOP AT gt_alv INTO gs_alv.
    READ TABLE gt_ljakj_sum INTO gs_ljakj_sum WITH KEY kulif = gs_alv-kulif waers = gs_alv-waers.
    IF sy-subrc = 0.
      gs_alv-waers = gs_ljakj_sum-waers.
      gs_alv-gljdm = gs_ljakj_sum-zdmbt.
      gs_alv-gljwr = gs_ljakj_sum-zwrbt.
      MODIFY gt_alv FROM gs_alv.
      CLEAR gs_alv.
    ENDIF.
  ENDLOOP.

*�����ۼƣ�monat IN s_monat
*�ͻ�
  gt_ljidd = gt_bsid.
  DELETE gt_ljidd WHERE gjahr NE p_gjahr.
  DELETE gt_ljidd WHERE monat GT s_monat-high OR shkzg = space.
  DELETE gt_ljidd WHERE shkzg = 'H' AND xnegp = 'X'.
  DELETE gt_ljidd WHERE shkzg = 'S' AND  xnegp = space.
  SORT gt_ljidd BY kulif waers.
  LOOP AT gt_ljidd INTO gs_ljidd.
    IF gs_ljidd-shkzg = 'S' AND gs_ljidd-xnegp = 'X'.
      gs_ljidd-dmbtr = gs_ljidd-dmbtr * -1. "��λ�ҽ��.
      gs_ljidd-wrbtr = gs_ljidd-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_ljidd-zdmbt = gs_ljidd-dmbtr. "��λ�ҽ��-����
*      gs_ljidd-zwrbt = gs_ljidd-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_ljidd FROM gs_ljidd.
    CLEAR gs_ljidd.
  ENDLOOP.
  gt_ljadd = gt_bsad.
  DELETE gt_ljAdd WHERE gjahr NE p_gjahr.
  DELETE gt_ljadd WHERE monat GT s_monat-high OR shkzg = space.
  DELETE gt_ljadd WHERE shkzg = 'H' AND xnegp = 'X'.
  DELETE gt_ljadd WHERE shkzg = 'S' AND  xnegp = space.
  SORT gt_ljadd BY kulif waers.
  LOOP AT gt_ljadd INTO gs_ljadd.
    IF gs_ljadd-shkzg = 'S' AND gs_ljadd-xnegp = 'X'.
      gs_ljadd-dmbtr = gs_ljadd-dmbtr * -1. "��λ�ҽ��.
      gs_ljadd-wrbtr = gs_ljadd-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_ljadd-zdmbt = gs_ljadd-dmbtr. "��λ�ҽ��-����
*      gs_ljadd-zwrbt = gs_ljadd-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_ljadd FROM gs_ljadd.
    CLEAR gs_ljadd.
  ENDLOOP.

  APPEND LINES OF gt_ljidd TO gt_ljadd.
  SORT gt_ljadd BY kulif waers.
  LOOP AT gt_ljadd INTO gs_ljadd.
    AT END OF waers.
      SUM.
      gs_ljadd-zdmbt = gs_ljadd-dmbtr. "��λ�ҽ��-����
      gs_ljadd-zwrbt = gs_ljadd-wrbtr. "ƾ֤���ҽ��-����
      APPEND gs_ljadd TO gt_ljadd_sum.
    ENDAT.
    MODIFY gt_ljadd FROM gs_ljadd.
    CLEAR gs_ljadd.
  ENDLOOP.
  LOOP AT gt_alv INTO gs_alv.
    READ TABLE gt_ljadd_sum INTO gs_ljadd_sum WITH KEY kulif = gs_alv-kulif waers = gs_alv-waers.
    IF sy-subrc = 0.
      gs_alv-waers = gs_ljadd_sum-waers.
      gs_alv-glddm = gs_ljadd_sum-zdmbt.
      gs_alv-gldwr = gs_ljadd_sum-zwrbt.
      MODIFY gt_alv FROM gs_alv.
      CLEAR gs_alv.
    ENDIF.
  ENDLOOP.
*��Ӧ��
  gt_ljikd = gt_bsik.
  DELETE gt_ljikd WHERE gjahr NE p_gjahr.
  DELETE gt_ljikd WHERE monat GT s_monat-high OR shkzg = space.
  DELETE gt_ljikd WHERE shkzg = 'H' AND xnegp = 'X'.
  DELETE gt_ljikd WHERE shkzg = 'S' AND  xnegp = space.
  SORT gt_ljikd BY kulif waers.
  LOOP AT gt_ljikd INTO gs_ljikd.
    IF gs_ljikd-shkzg = 'S' AND gs_ljikd-xnegp = 'X'.
      gs_ljikd-dmbtr = gs_ljikd-dmbtr * -1. "��λ�ҽ��.
      gs_ljikd-wrbtr = gs_ljikd-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_ljikd-zdmbt = gs_ljikd-dmbtr. "��λ�ҽ��-����
*      gs_ljikd-zwrbt = gs_ljikd-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_ljikd FROM gs_ljikd.
    CLEAR gs_ljikd.
  ENDLOOP.
  gt_ljakd = gt_bsak.
  DELETE gt_ljakd WHERE gjahr NE p_gjahr.
  DELETE gt_ljakd WHERE monat GT s_monat-high OR shkzg = space.
  DELETE gt_ljakd WHERE shkzg = 'H' AND xnegp = 'X'.
  DELETE gt_ljakd WHERE shkzg = 'S' AND  xnegp = space.
  SORT gt_ljakd BY kulif waers.
  LOOP AT gt_ljakd INTO gs_ljakd.
    IF gs_ljakd-shkzg = 'S' AND gs_ljakd-xnegp = 'X'.
      gs_ljakd-dmbtr = gs_ljakd-dmbtr * -1. "��λ�ҽ��.
      gs_ljakd-wrbtr = gs_ljakd-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_ljakd-zdmbt = gs_ljakd-dmbtr. "��λ�ҽ��-����
*      gs_ljakd-zwrbt = gs_ljakd-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_ljakd FROM gs_ljakd.
    CLEAR gs_ljakd.
  ENDLOOP.

  APPEND LINES OF gt_ljikd TO gt_ljakd.
  SORT gt_ljakd BY kulif waers.
  LOOP AT gt_ljakd INTO gs_ljakd.
    AT END OF waers.
      SUM.
      gs_ljakd-zdmbt = gs_ljakd-dmbtr. "��λ�ҽ��-����
      gs_ljakd-zwrbt = gs_ljakd-wrbtr. "ƾ֤���ҽ��-����
      APPEND gs_ljakd TO gt_ljakd_sum.
    ENDAT.
    MODIFY gt_ljakd FROM gs_ljakd.
    CLEAR gs_ljakd.
  ENDLOOP.
  LOOP AT gt_alv INTO gs_alv.
    READ TABLE gt_ljakd_sum INTO gs_ljakd_sum WITH KEY kulif = gs_alv-kulif waers = gs_alv-waers.
    IF sy-subrc = 0.
      gs_alv-waers = gs_ljakd_sum-waers.
      gs_alv-glddm = gs_ljakd_sum-zdmbt.
      gs_alv-gldwr = gs_ljakd_sum-zwrbt.
      MODIFY gt_alv FROM gs_alv.
      CLEAR gs_alv.
    ENDIF.
  ENDLOOP.

*��ĩ��monat < = s_monat_HIGH
  gt_qmid = gt_bsid.
  DELETE gt_qmid WHERE gjahr GT p_gjahr.
  DELETE gt_qmid WHERE gjahr EQ p_gjahr AND monat GT s_monat-high.
  SORT gt_qmid BY kulif waers.
  LOOP AT gt_qmid INTO gs_qmid.
    IF gs_qmid-shkzg = 'H'.
      gs_qmid-dmbtr = gs_qmid-dmbtr * -1. "��λ�ҽ��.
      gs_qmid-wrbtr = gs_qmid-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_qmid-zdmbt = gs_qmid-dmbtr. "��λ�ҽ��-����
*      gs_qmid-zwrbt = gs_qmid-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_qmid FROM gs_qmid.
    CLEAR gs_qmid.
  ENDLOOP.
  gt_qmad = gt_bsad.
  DELETE gt_qmad WHERE gjahr GT p_gjahr.
  DELETE gt_qmad WHERE gjahr EQ p_gjahr AND monat GT s_monat-high.
  SORT gt_qmad BY kulif  waers.
  LOOP AT gt_qmad INTO gs_qmad.
    IF gs_qmad-shkzg = 'H'.
      gs_qmad-dmbtr = gs_qmad-dmbtr * -1. "��λ�ҽ��.
      gs_qmad-wrbtr = gs_qmad-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_qmad-zdmbt = gs_qmad-dmbtr. "��λ�ҽ��-����
*      gs_qmad-zwrbt = gs_qmad-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_qmad FROM gs_qmad.
    CLEAR gs_qmad.
  ENDLOOP.

  APPEND LINES OF gt_qmid TO gt_qmad.
  SORT gt_qmad BY kulif waers.
  LOOP AT gt_qmad INTO gs_qmad.
    AT END OF waers.
      SUM.
      gs_qmad-zdmbt = gs_qmad-dmbtr. "��λ�ҽ��-����
      gs_qmad-zwrbt = gs_qmad-wrbtr. "ƾ֤���ҽ��-����
      APPEND gs_qmad TO gt_qmad_sum.
    ENDAT.
    MODIFY gt_qmad FROM gs_qmad.
    CLEAR gs_qmad.
  ENDLOOP.
  LOOP AT gt_alv INTO gs_alv.
    READ TABLE gt_qmad_sum INTO gs_qmad_sum WITH KEY kulif = gs_alv-kulif waers = gs_alv-waers.
    IF sy-subrc = 0.
      gs_alv-waers = gs_qmad_sum-waers.
      gs_alv-gqmdm = gs_qmad_sum-zdmbt.
      gs_alv-gqmwr = gs_qmad_sum-zwrbt.
    ENDIF.
    IF gs_alv-gqmdm > 0 AND gs_alv-gqmwr > 0.
      gs_alv-gqmfx = '��'.
    ELSEIF gs_alv-gqmdm < 0 AND gs_alv-gqmwr < 0.
      gs_alv-gqmfx = '��'.
    ELSEIF gs_alv-gqmdm = 0 AND gs_alv-gqmwr = 0.
      gs_alv-gqmfx = 'ƽ'.
    ELSEIF gs_alv-gqmdm IS INITIAL AND gs_alv-gqmwr IS INITIAL.
      gs_alv-gqmfx = 'ƽ'.
    ENDIF.
    MODIFY gt_alv FROM gs_alv.
    CLEAR gs_alv.

  ENDLOOP.

  gt_qmik = gt_bsik.
  DELETE gt_qmik WHERE gjahr GT p_gjahr.
  DELETE gt_qmik WHERE gjahr EQ p_gjahr AND monat GT s_monat-high.
  SORT gt_qmik BY kulif waers.
  LOOP AT gt_qmik INTO gs_qmik.
    IF gs_qmik-shkzg = 'H'.
      gs_qmik-dmbtr = gs_qmik-dmbtr * -1. "��λ�ҽ��.
      gs_qmik-wrbtr = gs_qmik-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_qmik-zdmbt = gs_qmik-dmbtr. "��λ�ҽ��-����
*      gs_qmik-zwrbt = gs_qmik-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_qmik FROM gs_qmik.
    CLEAR gs_qmik.
  ENDLOOP.
  gt_qmak = gt_bsak.
  DELETE gt_qmak WHERE gjahr GT p_gjahr.
  DELETE gt_qmak WHERE gjahr EQ p_gjahr AND monat GT s_monat-high.
  SORT gt_qmak BY kulif  waers.
  LOOP AT gt_qmak INTO gs_qmak.
    IF gs_qmak-shkzg = 'H'.
      gs_qmak-dmbtr = gs_qmak-dmbtr * -1. "��λ�ҽ��.
      gs_qmak-wrbtr = gs_qmak-wrbtr * -1. "ƾ֤���ҽ��
    ENDIF.
*    AT END OF waers.
*      SUM.
*      gs_qmak-zdmbt = gs_qmak-dmbtr. "��λ�ҽ��-����
*      gs_qmak-zwrbt = gs_qmak-wrbtr. "ƾ֤���ҽ��-����
*    ENDAT.
    MODIFY gt_qmak FROM gs_qmak.
    CLEAR gs_qmak.
  ENDLOOP.

  APPEND LINES OF gt_qmik TO gt_qmak.
  SORT gt_qmak BY kulif waers.
  LOOP AT gt_qmak INTO gs_qmak.
    AT END OF waers.
      SUM.
      gs_qmak-zdmbt = gs_qmak-dmbtr. "��λ�ҽ��-����
      gs_qmak-zwrbt = gs_qmak-wrbtr. "ƾ֤���ҽ��-����
      APPEND gs_qmak TO gt_qmak_sum.
    ENDAT.
    MODIFY gt_qmak FROM gs_qmak.
    CLEAR gs_qmak.
  ENDLOOP.
  LOOP AT gt_alv INTO gs_alv.
    READ TABLE gt_qmak_sum INTO gs_qmak_sum WITH KEY kulif = gs_alv-kulif waers = gs_alv-waers.
    IF sy-subrc = 0.
      gs_alv-waers = gs_qmak_sum-waers.
      gs_alv-gqmdm = gs_qmak_sum-zdmbt.
      gs_alv-gqmwr = gs_qmak_sum-zwrbt.
    ENDIF.
    IF gs_alv-gqmdm > 0 AND gs_alv-gqmwr > 0.
      gs_alv-gqmfx = '��'.
    ELSEIF gs_alv-gqmdm < 0 AND gs_alv-gqmwr < 0.
      gs_alv-gqmfx = '��'.
    ELSEIF gs_alv-gqmdm = 0 AND gs_alv-gqmwr = 0.
      gs_alv-gqmfx = 'ƽ'.
    ELSEIF gs_alv-gqmdm IS INITIAL AND gs_alv-gqmwr IS INITIAL.
      gs_alv-gqmfx = 'ƽ'.
    ENDIF.
    MODIFY gt_alv FROM gs_alv.
    CLEAR gs_alv.

  ENDLOOP.


  LOOP AT gt_alv INTO gs_alv.
    IF s_kunnr IS INITIAL AND s_lifnr IS NOT INITIAL.      "ֻɸѡ��Ӧ�̣�ɾ�����пͻ�
      READ TABLE gt_knb1 INTO gs_knb1 WITH  KEY kulif = gs_alv-kulif.
      IF sy-subrc = 0.
        DELETE gt_alv WHERE kulif = gs_knb1-kulif.
      ENDIF.

    ELSEIF s_lifnr IS INITIAL AND s_kunnr IS NOT INITIAL.   "ֻɸѡ�ͻ���ɾ�����й�Ӧ��
      READ TABLE gt_lfb1 INTO gs_lfb1 WITH  KEY kulif = gs_alv-kulif.
      IF sy-subrc = 0.
        DELETE gt_alv WHERE kulif = gs_lfb1-kulif.
      ENDIF.
    ENDIF.
  ENDLOOP.


*----------------------------------------------------------------------*
*                       END-OF-SELECTION.                              *
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM frm_fidcat.
  PERFORM frm_build_layout.
  PERFORM frm_alyout.
*----------------------------------------------------------------------*
*                               Forms.                                 *
*----------------------------------------------------------------------*
FORM frm_fidcat .
  PERFORM frm_append_fieldcat USING 'SAKNR' '��ƿ�Ŀ' '' '' 'SKA1' 'SAKNR' '' ''.
  PERFORM frm_append_fieldcat USING 'TXT20' '��Ŀ����' '' '' 'SKAT' 'TXT20' '' ''.
  PERFORM frm_append_fieldcat USING 'KULIF' '���̱��' '' '' 'KNB1' 'KUNNR' '' ''.
  PERFORM frm_append_fieldcat USING 'NAME1' '��������' '' '' 'KNA1' 'NAME1' '' ''.
  PERFORM frm_append_fieldcat USING 'WAERS' 'ƾ֤����' '' '' 'BSID' 'WAERS' '' ''.
  PERFORM frm_append_fieldcat USING 'GQCFX' '�ڳ�����' '' '' '' '' '' ''.
  PERFORM frm_append_fieldcat USING 'GQCWR' '�ڳ���ԭ�ҽ�' '' '' 'BSID' 'WRBTR' '' ''.
  PERFORM frm_append_fieldcat USING 'GQCDM' '�ڳ�����λ�ҽ�' '' '' 'BSID' 'DMBTR' '' ''.
  PERFORM frm_append_fieldcat USING 'GBJWR' '���ڽ跽��ԭ�ҽ�' '' '' 'BSID' 'WRBTR' '' ''.
  PERFORM frm_append_fieldcat USING 'GBJDM' '���ڽ跽����λ�ҽ�' '' '' 'BSID' 'DMBTR' '' ''.
  PERFORM frm_append_fieldcat USING 'GBDWR' '���ڴ�����ԭ�ҽ�' '' '' 'BSID' 'WRBTR' '' ''.
  PERFORM frm_append_fieldcat USING 'GBDDM' '���ڴ�������λ�ҽ�' '' '' 'BSID' 'DMBTR' '' ''.
  PERFORM frm_append_fieldcat USING 'GLJWR' '�跽�ۼƣ�ԭ�ҽ�' '' '' 'BSID' 'WRBTR' '' ''.
  PERFORM frm_append_fieldcat USING 'GLJDM' '�跽�ۼƣ���λ�ҽ�' '' '' 'BSID' 'DMBTR' '' ''.
  PERFORM frm_append_fieldcat USING 'GLDWR' '�����ۼƣ�ԭ�ҽ�' '' '' 'BSID' 'WRBTR' '' ''.
  PERFORM frm_append_fieldcat USING 'GLDDM' '�����ۼƣ���λ�ҽ�' '' '' 'BSID' 'DMBTR' '' ''.
  PERFORM frm_append_fieldcat USING 'GQMFX' '��ĩ����' '' '' '' '' '' ''.
  PERFORM frm_append_fieldcat USING 'GQMWR' '��ĩ��ԭ�ҽ�' '' '' 'BSID' 'WRBTR' '' ''.
  PERFORM frm_append_fieldcat USING 'GQMDM' '��ĩ����λ�ҽ�' '' '' 'BSID' 'DMBTR' '' ''.
ENDFORM.                    " FRM_FIDCAT
*----------------------------------------------------------------------*
*                   FORM FRM_APPEND_FIELDCAT                           *
*----------------------------------------------------------------------*
*                           ALV��ʾ����
*----------------------------------------------------------------------*
FORM frm_append_fieldcat  USING VALUE(filed_name)
                                 VALUE(filed_text)
                                 VALUE(cref)
                                 VALUE(qref)
                                 VALUE(ref_tab)
                                 VALUE(ref_name)
                                 VALUE(edit)
                                 VALUE(just).

  DATA gs_fieldcat LIKE LINE OF gt_fieldcat.
  gs_fieldcat-fieldname = filed_name.
  gs_fieldcat-coltext = filed_text.
  gs_fieldcat-cfieldname = cref.
  gs_fieldcat-qfieldname = qref.
  gs_fieldcat-ref_table = ref_tab.
  gs_fieldcat-ref_field = ref_name.
  gs_fieldcat-edit = edit.
  gs_fieldcat-just = just.
  APPEND gs_fieldcat TO gt_fieldcat.
ENDFORM.                    "FRM_APPEND_FIELDCAT
*----------------------------------------------------------------------*
*                       FORM FRM_BUILD_LAYOUT                          *
*----------------------------------------------------------------------*
*                           ALV��ʽ����
*----------------------------------------------------------------------*
FORM frm_build_layout.
  gs_layout-cwidth_opt = 'X'. "����Ӧ
  gs_layout-zebra      = 'X'.
  gs_layout-detailinit   = 'X' .         " �Ƿ���Ե���ϸ����Ļ
  gs_layout-detailtitl   = '��ϸ����' .   " ϸ����Ļ����
ENDFORM.                    "FRM_BUILD_LAYOUT
*----------------------------------------------------------------------*
*                         FORM FRM_ALYOUT                              *
*----------------------------------------------------------------------*
*                             ALV���
*----------------------------------------------------------------------*
FORM frm_alyout.
  DATA ls_variant          TYPE disvariant.
  ls_variant-report = 'ZSHXJFI0010'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program      = sy-repid
      i_callback_user_command = 'FRM_UCOMM '
      i_callback_top_of_page  = 'F_TOP_OF_PAGE'
      i_grid_title            = '���Ƶ�λ���������ӽ��Ƚ�������ά�����������޹�˾'
      i_save                  = 'A'
      is_variant              = ls_variant
      is_layout_lvc           = gs_layout
      it_fieldcat_lvc         = gt_fieldcat[]
    TABLES
      t_outtab                = gt_alv[]
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
ENDFORM.                    "FRM_ALYOUT
*&---------------------------------------------------------------------*
*&      Form  FRM_UCOMM
*&---------------------------------------------------------------------*
*       ALV�û���ť
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*
FORM frm_ucomm USING r_ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&IC1'.   " �ж��û��Ķ���
      CLEAR gt_fidetail[].
      CLEAR gt_fidetail.
      CLEAR gt_detail.
      CLEAR gt_ucom.
      CLEAR gs_ucom.
      READ TABLE gt_alv INTO gs_alv INDEX rs_selfield-tabindex.
*      IF sy-subrc EQ 0.
      gucom_kulif = gs_alv-kulif.
      gucom_waers = gs_alv-waers.
      MOVE-CORRESPONDING  gs_alv TO gs_ucom.
      APPEND gs_ucom TO gt_ucom.
      CLEAR gs_ucom.
      CASE rs_selfield-fieldname.
*          ENDIF.
        WHEN 'GBJWR' OR 'GBJDM' .
          gt_khtail = gt_bqadj .
          gt_gytail = gt_bqakj.
*          LOOP AT gt_ucom INTO gs_ucom.
*            DELETE gt_khtail WHERE kulif <> gs_ucom-kulif OR WAERS <> gs_ucom-WAERS.
*            DELETE gt_gytail WHERE kulif <> gs_ucom-kulif OR WAERS <> gs_ucom-WAERS.
*          ENDLOOP.
          PERFORM get_detail.
          PERFORM show_detail.
        WHEN 'GBDWR' OR 'GBDDM'.
          gt_khtail = gt_bqadd .
          gt_gytail = gt_bqakd.
*          LOOP AT gt_ucom INTO gs_ucom.
*            DELETE gt_khtail WHERE kulif <> gs_ucom-kulif OR WAERS <> gs_ucom-WAERS.
*            DELETE gt_gytail WHERE kulif <> gs_ucom-kulif OR WAERS <> gs_ucom-WAERS.
*          ENDLOOP.
          PERFORM get_detail.
          PERFORM show_detail.
        WHEN  'GLJWR' OR 'GLJDM' .
          gt_khtail = gt_ljadj  .
          gt_gytail = gt_ljakj.
*          LOOP AT gt_ucom INTO gs_ucom.
*            DELETE gt_khtail WHERE kulif <> gs_ucom-kulif OR WAERS <> gs_ucom-WAERS.
*            DELETE gt_gytail WHERE kulif <> gs_ucom-kulif OR WAERS <> gs_ucom-WAERS.
*          ENDLOOP.
          PERFORM get_detail.
          PERFORM show_detail.
        WHEN  'GLDWR' OR  'GLDDM'.
          gt_khtail = gt_ljadd .
          gt_gytail = gt_ljakd.
*          LOOP AT gt_ucom INTO gs_ucom.
*            DELETE gt_khtail WHERE kulif <> gs_ucom-kulif OR WAERS <> gs_ucom-WAERS.
*            DELETE gt_gytail WHERE kulif <> gs_ucom-kulif OR WAERS <> gs_ucom-WAERS.
*          ENDLOOP.
          PERFORM get_detail.
          PERFORM show_detail.
        WHEN OTHERS.
      ENDCASE.
    WHEN ''.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " FRM_UCOMM
*&---------------------------------------------------------------------*
*&      Form  f_top_of_page
*&---------------------------------------------------------------------*
*       ALV��ͷ����
*----------------------------------------------------------------------*
FORM f_top_of_page .
  DATA: lt_header TYPE slis_t_listheader,
        ls_header TYPE slis_listheader.

  ls_header-typ  = 'H'.
  ls_header-info = '��������'.
  APPEND ls_header TO lt_header .
  CLEAR  ls_header .

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header
      i_alv_form         = 'X'.

ENDFORM.                    " f_top_of_page
*&---------------------------------------------------------------------*
*&      Form  GET_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_detail .
  DELETE gt_khtail WHERE kulif <> gucom_kulif OR waers <> gucom_waers.
  DELETE gt_gytail WHERE kulif <> gucom_kulif OR waers <> gucom_waers.
  LOOP AT gt_khtail INTO gs_khtail.
    MOVE-CORRESPONDING  gs_khtail TO gs_detail.
    APPEND gs_detail TO gt_detail.
    CLEAR gs_detail.
  ENDLOOP.

  LOOP AT gt_gytail INTO gs_gytail.
    MOVE-CORRESPONDING  gs_gytail TO gs_detail.
    APPEND gs_detail TO gt_detail.
    CLEAR gs_detail.
  ENDLOOP.
  LOOP AT gt_detail INTO gs_detail.
    READ TABLE gt_ucom INTO gs_ucom WITH KEY kulif = gs_detail-kulif.
    gs_detail-saknr = gs_ucom-saknr.
    gs_detail-txt20 = gs_ucom-txt20.
    gs_detail-name1 = gs_ucom-name1.
    MODIFY gt_detail FROM gs_detail.
    CLEAR gs_detail.
  ENDLOOP.

ENDFORM.                    "get_detail
*&---------------------------------------------------------------------*
*&      Form  SHOW_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM show_detail USING rs_selfield TYPE slis_selfield .
FORM show_detail.
  PERFORM frm_fidcat_detail.
  PERFORM frm_build_layout_detail.
  PERFORM frm_alyout_detail.
ENDFORM.                    "show_detail
*----------------------------------------------------------------------*
*                               Forms.                                 *
*----------------------------------------------------------------------*
FORM frm_fidcat_detail .
  PERFORM frm_append_fieldcat1 USING 'BELNR' 'ƾ֤���' '' '' 'BSID' 'BELNR' '' ''.
  PERFORM frm_append_fieldcat1 USING 'SAKNR' '��ƿ�Ŀ' '' '' 'SKA1' 'SAKNR' '' ''.
  PERFORM frm_append_fieldcat1 USING 'TXT20' '��Ŀ����' '' '' 'SKAT' 'TXT20' '' ''.
  PERFORM frm_append_fieldcat1 USING 'BLART' 'ƾ֤����' '' '' 'BSID' 'BLART' '' ''.
  PERFORM frm_append_fieldcat1 USING 'KULIF' '���̱��' '' '' 'KNB1' 'KUNNR' '' ''.
  PERFORM frm_append_fieldcat1 USING 'NAME1' '��������' '' '' 'KNA1' 'NAME1' '' ''.
  PERFORM frm_append_fieldcat1 USING 'BUDAT' '��������' '' '' 'BSID' 'BUDAT' '' ''.
  PERFORM frm_append_fieldcat1 USING 'WRBTR' 'ԭ�ҽ��' '' '' 'BSID' 'WRBTR' '' ''.
  PERFORM frm_append_fieldcat1 USING 'WAERS' '����' '' '' 'BSID' 'WAERS' '' ''.
  PERFORM frm_append_fieldcat1 USING 'DMBTR' '��λ�ҽ��' '' '' 'BSID' 'DMBTR' '' ''.
  PERFORM frm_append_fieldcat1 USING 'UMSKZ' '�ر����˱�ʶ' '' '' 'BSID' 'UMSKZ' '' ''.
  PERFORM frm_append_fieldcat1 USING 'AUGBL' '����ƾ֤' '' '' 'BSID' 'AUGBL' '' ''.
  PERFORM frm_append_fieldcat1 USING 'AUGDT' '��������' '' '' 'BSID' 'AUGDT' '' ''.
  PERFORM frm_append_fieldcat1 USING 'SGTXT' '�ı�' '' '' 'BSID' 'SGTXT' '' ''.
ENDFORM.                    " FRM_FIDCAT
*----------------------------------------------------------------------*
*                   FORM FRM_APPEND_FIELDCAT                           *
*----------------------------------------------------------------------*
*                           ALV��ʾ����
*----------------------------------------------------------------------*
FORM frm_append_fieldcat1  USING VALUE(filed_name)
                                 VALUE(filed_text)
                                 VALUE(cref)
                                 VALUE(qref)
                                 VALUE(ref_tab)
                                 VALUE(ref_name)
                                 VALUE(edit)
                                 VALUE(just).

  DATA gs_fidetail LIKE LINE OF gt_fidetail.
  gs_fidetail-fieldname = filed_name.
  gs_fidetail-coltext = filed_text.
  gs_fidetail-cfieldname = cref.
  gs_fidetail-qfieldname = qref.
  gs_fidetail-ref_table = ref_tab.
  gs_fidetail-ref_field = ref_name.
  gs_fidetail-edit = edit.
  gs_fidetail-just = just.
  APPEND gs_fidetail TO gt_fidetail.
ENDFORM.                    "FRM_APPEND_FIELDCAT
*----------------------------------------------------------------------*
*                       FORM FRM_BUILD_LAYOUT                          *
*----------------------------------------------------------------------*
*                           ALV��ʽ����
*----------------------------------------------------------------------*
FORM frm_build_layout_detail.
  gs_layout_detail-cwidth_opt = 'X'. "����Ӧ
  gs_layout_detail-zebra      = 'X'.
ENDFORM.                    "FRM_BUILD_LAYOUT
*----------------------------------------------------------------------*
*                         FORM FRM_ALYOUT                              *
*----------------------------------------------------------------------*
*                             ALV���
*----------------------------------------------------------------------*
FORM frm_alyout_detail.
  DATA ls_variant  TYPE disvariant.
  ls_variant-report = 'ZSHXJFI0010'.
  ls_variant-handle = 'L2ND'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program      = sy-repid
      i_callback_user_command = 'X_USER_COMMAND '
      i_grid_title            = '��ϸ��Ļ'
      i_save                  = 'X'
      is_variant              = ls_variant
      is_layout_lvc           = gs_layout_detail
      it_fieldcat_lvc         = gt_fidetail[]
    TABLES
      t_outtab                = gt_detail[]
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
ENDFORM.                    "FRM_ALYOUT

*&---------------------------------------------------------------------*
*&      Form  x_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM    text
*----------------------------------------------------------------------*
FORM x_user_command USING p_ucomm TYPE sy-ucomm
    p_rs_selfield TYPE slis_selfield .

  CASE p_ucomm.
    WHEN '&IC1' .   " �ж��û��Ķ���
      CASE p_rs_selfield-fieldname.
        WHEN 'BELNR'.
          SET PARAMETER ID 'BLN' FIELD p_rs_selfield-value .
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .
        WHEN OTHERS.
      ENDCASE.
  ENDCASE.
ENDFORM.                    " X_USER_COMMAND

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
