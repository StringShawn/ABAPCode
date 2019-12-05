**************************************************
*��������:�̶��ʲ���ϸ��
*��������: 2019-11-20
*������:XXX
*������:XXX
*��������:
*============================================
*�����¼
*�޸������    �޸�����    �޸���   �޸�����
*DEVK912009    2019-11-20   HANDYXH    ��������
***************************************************
REPORT zshxjfi0004.
*--------------------------------------------------------------------*
*& Tables Declaration
*--------------------------------------------------------------------*
TABLES: anla, anlz, anlb, anlbza,t001,ccss,anlp.

*alv����
*FIELD-SYMBOLS:<f_out> TYPE ty_out.
DATA: w_fieldcat TYPE lvc_s_fcat,
      i_fieldcat TYPE lvc_t_fcat,
      i_layout   TYPE lvc_s_layo.
DATA: g_col LIKE sy-cucol VALUE 1.
TYPE-POOLS:slis .
*----------------------------------------------------------------------*
* Define the Macros
*----------------------------------------------------------------------*
DEFINE get_fieldcat.
  w_fieldcat-col_pos   = g_col.
  w_fieldcat-no_zero = 'X'.                                 "Ϊ�������Ϊ0����
  w_fieldcat-fieldname =   &1.
  w_fieldcat-scrtext_l  =  &2.
  w_fieldcat-outputlen  =  &3.
  w_fieldcat-ref_field = &4.
  w_fieldcat-ref_table = &5.
  w_fieldcat-key = &6.
  IF &1 = 'ZCYZ' OR &1 = 'JZZB' OR &1 = 'LJZJ_ZC' OR &1 = 'LJZJ_JHW' OR &1 = 'ZCJZ' OR
    &1 = 'SCHRW' OR &1 = 'YEAR_1' OR &1 = 'MONTH_2' OR &1 = 'MONTH_1' OR &1 = 'ZMONTHJE'
    OR &1 = 'BQZJ' OR &1 = 'BQJS'.
    w_fieldcat-do_sum = 'X'.
  ENDIF.
  "HOTSPOT
  IF &1 = 'ANLN1'.
    w_fieldcat-hotspot = 'X'.
  ENDIF.

  APPEND w_fieldcat TO i_fieldcat.
  g_col = g_col + 1.
  CLEAR w_fieldcat.
END-OF-DEFINITION.
TYPE-POOLS: slis.

*--------------------------------------------------------------------*
*& Global Variables Declaration
*--------------------------------------------------------------------*

DATA: gt_layout TYPE slis_layout_alv.
DATA: i_events TYPE slis_t_event.
DATA: l_ls_event TYPE slis_alv_event.
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv.

DATA: alv_repid LIKE sy-repid.
DATA: alv_variant TYPE disvariant.
TYPES: BEGIN OF stype_fields,                               "n599966
         fieldname TYPE  aind_str3-fieldname,               "n599966
       END OF stype_fields,                                 "n599966
                                                            "n599966
       stab_fields TYPE STANDARD TABLE OF                   "n599966
 stype_fields                                               "n599966
 WITH DEFAULT KEY.
DATA : g_t_fields TYPE STANDARD TABLE OF                    "n921164
      stype_fields.
DATA: it_fieldcat TYPE slis_t_fieldcat_alv,
      wa_fieldcat TYPE slis_fieldcat_alv,
      wa_layout   TYPE slis_layout_alv,
      g_repid     LIKE sy-repid,

      g_firstdate LIKE sy-datum,      "���µ�һ��
      g_lastdate  LIKE sy-datum,      "�������һ��
      g_anhwsl    LIKE t090na-anhwsl, "��ֵֹ��
      g_ahproz    LIKE t091p-ahproz.  "��ֹת��ʱ�İٷ���
DATA: BEGIN OF it_list OCCURS 0,
        bukrs         LIKE anep-bukrs,   "��˾����
        anlue         LIKE anla-anlue,   "�ʲ����
*  add �ʲ��������
        txk20         LIKE ankt-txk20,   "�ʲ��������
        anln1         LIKE anep-anln1,   "�ʲ�����
*        anln2 LIKE anep-anln2,   "�ʲ��Ӻ���
        txt50         LIKE anla-txt50,   "�ʲ�����
        anlhtxt       TYPE anlh-anlhtxt, " �ʲ�����˵��
        txa50         LIKE anla-txa50,   "���/���ض�
        sernr         LIKE anla-sernr,   "���к�
        invnr         LIKE anla-invnr,   "�����/��ϵͳ�ʲ����
        lifnr         LIKE anla-lifnr,   "��Ӧ��
        liefe         LIKE anla-liefe,   "��Ӧ������
        werks         TYPE anlz-werks,    " ����
        werks_text    TYPE t001w-name1,   "
        stort         TYPE anlz-stort,    " �ʲ��ص�
        stort_text    TYPE t499s-ktext,
        menge         LIKE anla-menge,   "����
        zqingdun      LIKE anla-menge, "���������
        anlkl         LIKE anla-anlkl,        "�ʲ�����
        zzndjb        LIKE anlb-ndper,  "��ʹ���ڼ�
        meins         LIKE anla-meins,   "������λ
        ivdat         LIKE anla-ivdat,   "ԭʼ��������
        invzu         LIKE anla-invzu,   "���λ��
        aktiv         LIKE anla-aktiv,   "�ʱ�������
*        gsber LIKE anlz-gsber,   "ҵ��Χ
*        gtext LIKE tgsbt-gtext,   "ҵ��������
        kostl         LIKE anlz-kostl,   "�ɱ�����
        ltext         LIKE cskt-ltext,   "�ɱ�����˵��
        kostlv        LIKE anlz-kostlv, "���γɱ�����
        ltextv        LIKE cskt-ltext,   "���γɱ�����˵��
        ps_psp_pnr2   LIKE anlz-ps_psp_pnr2,  "WBSԪ��
        post1         LIKE prps-post1,  "WBSԪ������
        prctr         LIKE cepc-prctr,   "��������
        ltextp        LIKE cepct-ltext,   "��������˵��
        caufn         LIKE anlz-caufn,    "�ڲ�����
        ktext         TYPE auftext,       "�ڲ���������
        raumn         LIKE anlz-raumn,   "�����
        kfzkz         LIKE anlz-kfzkz,   "ִ���ƺ�
        ord41         LIKE t087t-ordtx,  "���ӷ�ʽ
        ord42         LIKE t087t-ordtx,  "ʹ����;
        ord43         TYPE anla-ord43,
        ord43_text    LIKE t087t-ordtx,  "�ʽ���Դ
        ord44         LIKE t087t-ordtx,  "��Ѻ��Ϣ
        gdlgrp        LIKE t087s-gdlgrp_txt, "�ʲ���;
        herst         LIKE anla-herst,   "������
        vbund         LIKE anla-vbund,   "ó�׻��
        typbz         LIKE anla-typbz,   "������

        fiamt         TYPE anla-fiamt,    " ˰���
        ehwnr         TYPE anla-ehwnr,    " ����֪ͨ���ƺ�
        ehwzu         TYPE anla-ehwzu,    " ����֪ͨ
        stadt         TYPE anla-stadt,    " ������
        grufl         TYPE anla-grufl,    " ��Χ
        flurn         TYPE anla-flurn,    " ���صȼ���ͼ/����

*        ZZ_ASSET1     TYPE ANLU-ZZ_ASSET1,  " �ۺ����ԭʼ�ʲ�����
*        ZZ_ASSET2     TYPE ANLU-ZZ_ASSET2,  " ���������ʲ�����

*---{ added by ibm-nqd on 20170927 start
*---" for more info, refer to @change_0004
        deakt         LIKE anla-deakt,   " ��������
*---} added by ibm-nqd on 20170927 end
        umjar         TYPE anlb-umjar,    " �л����
        umper         TYPE anlb-umper,    " �л��¶�
        ndurj         TYPE anlb-ndurj,    " ԭʼ���
        ndurp         TYPE anlb-ndurp,    " ԭʼ�¶�

        zcyz          LIKE anlc-kansw,   "ԭֵ
        qcyz          TYPE faglflext-hslvt, " �ڳ�ԭֵ
*---{ added by ibm-nqd on 20170703 start
        bqzj          TYPE faglflext-hslvt, " ��������->����ԭֵ����
        bqjs          TYPE faglflext-hslvt, " ���ڼ���->����ԭֵ���٣�����ֵ׼����
*---} added by ibm-nqd on 20170703 end
        jzzb_qc       TYPE faglflext-hslvt, " ��ֵ׼���ڳ�
        jzzb_bq       TYPE faglflext-hslvt, " ���ڼ�ֵ׼��
        jzzb          LIKE anlc-kaufw,   "��ֵ׼��
        ljzj_qc       TYPE faglflext-hslvt, " �ۼ��۾��ڳ�
        byljzj_tz     TYPE faglflext-hslvt, " �����ۼ��۾ɵ���
        ljzj_zc       LIKE anlc-knafa,   "�ۼ��۾ɣ�������-> �ۼ��۾���ĩ
        ljzj_jhw      LIKE anlc-knafa,   "�ۼ��۾ɣ��ƻ��⣩
        zcjz          LIKE anlc-knafa,   "��ֵ->��ֵ��ĩ��������ֵ׼����
        zcjz_ce       LIKE faglflext-hslvt,   "��ֵ���
        zcjz_qc       LIKE faglflext-hslvt,   "��ֵ�ڳ���������ֵ׼����
        schrw         LIKE anlb-schrw,   "��ֵ
*        add  �������۾ɶ�͡������۾ɶ
        month_1       LIKE anlp-nafaz,        "�����۾�
        byzj_jhw      TYPE faglflext-hslvt,   " �����۾ɣ��ƻ��⣩
        month_2       LIKE anlp-nafaz,        "�����۾�
        zmonthje1(13) TYPE p DECIMALS 2, "���۾ɶ�
        year_1        LIKE anlp-nafag,         "����������۾ɶ�
        zmonthje      LIKE anlb-schrw,       "���۾ɶ�
        afasl         LIKE anlb-afasl,          "�۾���
        afatxt        LIKE t090nat-afatxt,      "�۾�������
        zzndja        LIKE anlb-ndper,         "�ƻ�ʹ���ڼ�
        afabg         LIKE anlb-afabg,          "�۾ɿ�ʼ����
        afabe         LIKE anlc-afabe,   "�۾ɷ�Χ
        "ALV����һ���ֶΣ�ʣ��ʹ���ڼ䣬��ʾ����ʹ���ڼ�֮��ȡ����ʽΪ���ƻ�ʹ���ڼ�-��ʹ���ڼ䣩�����޸� Start
        zsysyqj       LIKE anlb-ndper,        "ʣ��ʹ���ڼ�
        "ALV����һ���ֶΣ�ʣ��ʹ���ڼ䣬��ʾ����ʹ���ڼ�֮��ȡ����ʽΪ���ƻ�ʹ���ڼ�-��ʹ���ڼ䣩�����޸� End
        je_qm         TYPE faglflext-hslvt, " ������ĩ
        je_ce         TYPE faglflext-hslvt, " ������
        je_qc         TYPE faglflext-hslvt, " �����ڳ�

        shzhq_yz      TYPE faglflext-hslvt, " �ۺ����ǰԭֵ
        shzhq_ljzj    TYPE faglflext-hslvt, " �ۺ����ǰ�ۼ��۾�
        shzhq_jzzb    TYPE faglflext-hslvt, " �ۺ����ǰ��ֵ׼��
        shzhq_je      TYPE faglflext-hslvt, " �ۺ����ǰ����

      END OF it_list.
FIELD-SYMBOLS: <it_list> LIKE it_list.
DATA: BEGIN OF it_afapl OCCURS 0,
        bukrs LIKE t093c-bukrs,  "��˾����
        afapl LIKE t093c-afapl,  "�й��ʲ��������۾ɱ�
      END OF it_afapl.

DATA: BEGIN OF it_anla OCCURS 0, "�ʲ�����¼
        bukrs  LIKE anla-bukrs,   "��˾����
        anlkl  LIKE anla-anlkl,   "�ʲ�����
        anln1  LIKE anla-anln1,   "�ʲ�����
*        anln2 LIKE anla-anln2,   "�ʲ��Ӻ���
        anlue  LIKE anla-anlue,   "�ʲ����
        txt50  LIKE anla-txt50,   "�ʲ�����
        txa50  LIKE anla-txa50,   "���/���ض�
*        txk20 like ankt-txk20,   "�ʲ��������
        sernr  LIKE anla-sernr,   "���к�
        invnr  LIKE anla-invnr,   "�����/��ϵͳ�ʲ����
        menge  LIKE anla-menge,   "����
        meins  LIKE anla-meins,   "������λ
        ivdat  LIKE anla-ivdat,   "�������
        invzu  LIKE anla-invzu,   "���λ��
        aktiv  LIKE anla-aktiv,   "�ʱ�������
        ord41  LIKE anla-ord41,   "���ӷ�ʽ
        ord42  LIKE anla-ord42,   "ʹ����;
        ord43  LIKE anla-ord43,   "�ʽ���Դ
        ord44  LIKE anla-ord43,   "��Ѻ��Ϣ
        gdlgrp LIKE anla-gdlgrp, "�ʲ���;
        lifnr  LIKE anla-lifnr,
        liefe  LIKE anla-liefe,
        herst  LIKE anla-herst,   "������
        vbund  LIKE anla-vbund,   "ó�׻��
        typbz  LIKE anla-typbz,   "������

        fiamt  TYPE anla-fiamt,    " ˰���
        ehwnr  TYPE anla-ehwnr,    " ����֪ͨ���ƺ�
        ehwzu  TYPE anla-ehwzu,    " ����֪ͨ
        stadt  TYPE anla-stadt,    " ������
        grufl  TYPE anla-grufl,    " ��Χ
        flurn  TYPE anla-flurn,    " ���صȼ���ͼ/����

*        ZZ_ASSET1 TYPE ANLU-ZZ_ASSET1,
*        ZZ_ASSET2 TYPE ANLU-ZZ_ASSET2,
*---{ added by ibm-nqd on 20170927 start
*---" for more info, refer to @change_0004
        deakt  TYPE anla-deakt,    " ��������
*---} added by ibm-nqd on 20170927 end
      END OF it_anla.

DATA: BEGIN OF it_anlz OCCURS 0, "ʱ������ʲ�����
        bukrs       LIKE anlz-bukrs,   "��˾����
        anln1       LIKE anlz-anln1,   "�ʲ�����
*        anln2 LIKE anlz-anln2,   "�ʲ��Ӻ���
*        gsber LIKE anlz-gsber,   "ҵ��Χ
        kostl       LIKE anlz-kostl,   "�ɱ�����
        kostlv      LIKE anlz-kostlv, "���γɱ�����
        caufn       LIKE anlz-caufn,   "�ڲ�����
        raumn       LIKE anlz-raumn,   "�����
        kfzkz       LIKE anlz-kfzkz,   "ִ���ƺ�
        ps_psp_pnr2 LIKE anlz-ps_psp_pnr2, "WBSԪ��
        werks       TYPE anlz-werks,
        stort       TYPE anlz-stort,
      END OF it_anlz.

DATA: BEGIN OF it_anek OCCURS 0, "�ʲ�����ƾ̧֤ͷ
        bukrs LIKE anek-bukrs,   "��˾����
        anln1 LIKE anek-anln1,   "�ʲ�����
*        anln2 LIKE anek-anln2,   "�ʲ��Ӻ���
        gjahr LIKE anek-gjahr,   "������
        monat LIKE anek-monat,
        lnran LIKE anek-lnran,   "������ʲ�����Ŀ�����
        budat LIKE anek-budat,   "��������
        belnr LIKE anek-belnr,   "�ο�ƾ֤���
        buzei LIKE anek-buzei,   "���ƾ֤�е�����Ŀ��
      END OF it_anek.

DATA: BEGIN OF it_anep OCCURS 0, "�ʲ�����Ŀ
        bukrs  LIKE anep-bukrs,   "��˾����
        anln1  LIKE anep-anln1,   "�ʲ�����
*        anln2 LIKE anep-anln2,   "�ʲ��Ӻ���
        gjahr  LIKE anep-gjahr,   "������
        lnran  LIKE anep-lnran,   "������ʲ�����Ŀ�����
        budat  LIKE anek-budat,   "��������
        belnr  LIKE anek-belnr,   "�ο�ƾ֤���
        buzei  LIKE anek-buzei,   "���ƾ֤�е�����Ŀ��
        afabe  LIKE anep-afabe,   "�۾ɷ�Χ
        bzdat  LIKE anep-bzdat,   "�ʲ���ֵ��
        bwasl  LIKE anep-bwasl,   "�ʲ�ҵ������
        shkzg  TYPE tabw-anshkz,
        anbtr  LIKE anep-anbtr,   "���ʽ��
        lnsan  LIKE anep-lnsan,   "�������ʲ���������к�
        xawbt  LIKE anep-xawbt,   "��ǣ���ͬ���ʽ������
        anbtra LIKE anep-anbtr,  "�ʲ�ԭֵ
        anbtrb LIKE anep-anbtr,  "��ֵ׼��
        anbtrc LIKE anep-anbtr,  "�ۼ��۾ɣ�������
        anbtrd LIKE anep-anbtr,  "�ۼ��۾ɣ�δ�ƻ���
      END OF it_anep.

DATA: BEGIN OF it_anea OCCURS 0, "����ֵ���ʲ�����Ŀ
        bukrs LIKE anea-bukrs,   "��˾����
        anln1 LIKE anea-anln1,   "�ʲ�����
*        anln2 LIKE anea-anln2,   "�ʲ��Ӻ���
        gjahr LIKE anea-gjahr,   "������
        lnran LIKE anea-lnran,   "������ʲ�����Ŀ�����
        afabe LIKE anea-afabe,   "�۾ɷ�Χ
        invzv LIKE anea-invzv,   "�����ۻ�Ͷ����Ȩ
        aufwv LIKE anea-aufwv,   "�й��滻ֵ�ı����ۻ��ع�
        aufwl LIKE anea-aufwl,   "�����й��滻ֵ�ı����ع�
        nafav LIKE anea-nafav,   "�����ۻ������۾�
        safav LIKE anea-safav,   "�����ۼ��ر��۾�
        aafav LIKE anea-aafav,   "�������ۻ��ƻ����۾�
        invzl LIKE anea-invzl,   "�йش���ı���Ͷ����Ȩ
        nafal LIKE anea-nafal,   "����ı��������۾�
        safal LIKE anea-safal,   "����ı����ر��۾�
        aafal LIKE anea-aafal,   "����ı����ƻ����۾�
      END OF it_anea.

DATA: BEGIN OF it_anlp OCCURS 0, "�ʲ��ڼ��ֵ
        bukrs   LIKE anlp-bukrs,   "��˾����
        gjahr   LIKE anlp-gjahr,   "������
        peraf   LIKE anlp-peraf,   "�۾ɼ�����
        anln1   LIKE anlp-anln1,   "�ʲ�����
*        anln2 LIKE anlp-anln2,   "�ʲ��Ӻ���
        afaber  LIKE anlp-afaber, "�۾ɷ�Χ
        bwasl   LIKE anep-bwasl,   "�ʲ�ҵ������
        aufwz   LIKE anlp-aufwz,   "�����ʵ�����ֵ�ع�
        nafaz   LIKE anlp-nafaz,   "���ʵ������۾�
        safaz   LIKE anlp-safaz,   "�����ʵ������۾�
        aafaz   LIKE anlp-aafaz,   "�����ʵļƻ����۾�
        belnr   LIKE anlp-belnr,   "ƾ֤���
*        anbtrc like anlp-nafaz,
        zanbtrb LIKE anlp-nafaz,
        zanbtrc LIKE anlp-nafaz,
        zanbtrd LIKE anlp-nafaz,
        budat   LIKE bkpf-budat,
        nafag   LIKE anlp-nafag,   "���ʵ������۾�
        safag   LIKE anlp-safag,   "�����ʵ������۾�
        aafag   LIKE anlp-aafag,   "�����ʵļƻ����۾�
      END OF it_anlp.

DATA: BEGIN OF it_t087t OCCURS 0, "����������
        ordnr LIKE t087t-ordnr,   "����С���
        ord4x LIKE t087t-ord4x,                             "����С��1-4
        ordtx LIKE t087t-ordtx,   "����
      END OF it_t087t.

DATA: BEGIN OF it_t087s OCCURS 0, "������8����
        gdlgrp     LIKE t087s-gdlgrp,         "�ʲ���;
        gdlgrp_txt LIKE t087s-gdlgrp_txt,                   "������8λ�ı�
      END OF it_t087s.

DATA: BEGIN OF it_ankt OCCURS 0, "�ʲ��������
        anlkl LIKE ankt-anlkl,
        txk20 LIKE ankt-txk20,
      END OF it_ankt.

DATA: BEGIN OF it_anlc OCCURS 0, "�ʲ�ֵ�ֶ�
        bukrs LIKE anlc-bukrs,   "��˾����
        anln1 LIKE anlc-anln1,   "�ʲ�����
        gjahr LIKE anlc-gjahr,   "������
        afabe LIKE anlc-afabe,   "�۾ɷ�Χ
        kansw LIKE anlc-kansw,   "�ۻ����ú���������
        kaufw LIKE anlc-kaufw,   "���ü�ֵ���ۼ��ع�
        knafa LIKE anlc-knafa,   "�ۼ������۾�
        ksafa LIKE anlc-ksafa,   "�ۼ������۾�
        kaafa LIKE anlc-kaafa,   "�ۻ��ƻ����۾�
        answl LIKE anlc-answl,   "�����Ӱ���ʲ�ֵ��ҵ��
        aufwb LIKE anlc-aufwb,   "���ü�ֵ���ع�����
        nafag LIKE anlc-nafag,   "�����ڵ�ǰ��������۾�
        safag LIKE anlc-safag,   "�ڵ�ǰ�ƻ�����еļ��ʵ��ر��۾�
        aafag LIKE anlc-aafag,   "�й���ļƻ����۾ɼ���
        aufwl LIKE anlc-aufwl,   "�����й��滻ֵ�ı����ع�
        nafal LIKE anlc-nafal,   "����ı��������۾�
        safal LIKE anlc-safal,   "����ı����ر��۾�
        aafal LIKE anlc-aafal,   "����ı����ƻ����۾�
        aufwv LIKE anlc-aufwv,   "�й��滻ֵ�ı����ۻ��ع�
        nafav LIKE anlc-nafav,   "�����ۻ������۾�
        safav LIKE anlc-safav,   "�����ۼ��ر��۾�
        aafav LIKE anlc-aafav,   "�������ۻ��ƻ����۾�
        aufwp LIKE anlc-aufwp,   "����ֵ�ļƻ��ع�
        nafap LIKE anlc-nafap,   "�����Ѽƻ������۾�
        safap LIKE anlc-safap,   "�����Ѽƻ��ر��۾�
        aafap LIKE anlc-aafap,   "������Ԥ����δ�ƻ��۾�
        aufng LIKE anlc-aufng,   "�й��ۻ������۾ɵļ�������
        zusna LIKE anlc-zusna,   "�������۾��ϵļ�ֵ����
        zussa LIKE anlc-zussa,   "���ر��۾��ϵļ�ֵ����
        zusaa LIKE anlc-zusaa,   "���޼ƻ��۾��ϵļ�ֵ����
        ndabj LIKE anlc-ndabj,   "�ڲƻ���ȿ�ʼʱ���ڵ���������
        ndabp LIKE anlc-ndabp,   "�������ʼʱ����ʹ������
      END OF it_anlc.

DATA: BEGIN OF it_anlb OCCURS 0, "�۾�����
        bukrs LIKE anlb-bukrs,   "��˾����
        anln1 LIKE anlb-anln1,   "�ʲ�����
        afabe LIKE anlb-afabe,   "�۾ɷ�Χ
        schrw LIKE anlb-schrw,   "�ʲ���ֵ
        afasl LIKE anlb-afasl,   "�۾���
        ndjar LIKE anlb-ndjar,   "�ƻ���ʹ����
        ndper LIKE anlb-ndper,   "�ƻ�ʹ���ڼ�
        afabg LIKE anlb-afabg,   "�۾ɼ��㿪ʼ����
        umjar TYPE anlb-umjar,    " �л����
        umper TYPE anlb-umper,    " �л��¶�
        ndurj TYPE anlb-ndurj,    " ԭʼ���
        ndurp TYPE anlb-ndurp,    " ԭʼ�¶�
      END OF it_anlb.

DATA: BEGIN OF it_anlbza OCCURS 0, "ʱ������۾�����
        bukrs LIKE anlbza-bukrs,   "��˾����
        anln1 LIKE anlbza-anln1,   "�ʲ�����
*        anln2 LIKE anlbza-anln2,   "�ʲ��Ӻ���
        afabe LIKE anlbza-afabe,   "�۾ɷ�Χ
        schrw LIKE anlbza-schrw,   "�ʲ���ֵ
        afasl LIKE anlbza-afasl,   "�۾���
        ndjar LIKE anlbza-ndjar,   "�ƻ���ʹ����
        ndper LIKE anlbza-ndper,   "�ƻ�ʹ���ڼ�
        afabg LIKE anlb-afabg,     "�۾ɼ��㿪ʼ����
      END OF it_anlbza.

TYPES:BEGIN OF ty_month1,
        bukrs   TYPE anlp-bukrs,
        month_1 TYPE anlp-nafaz,        "�����۾�
      END OF ty_month1.

TYPES:BEGIN OF ty_month2,
        bukrs  TYPE anlp-bukrs,
        anln1  TYPE anlp-anln1,
        gjahr  TYPE anlp-gjahr,
        afaber TYPE anlp-afaber,
        peraf  TYPE anlp-peraf,
        nafaz  TYPE anlp-nafaz,        "�����۾�
      END OF ty_month2.

DATA:lt_month1 TYPE TABLE OF ty_month2 WITH HEADER LINE.
DATA:lt_month1_1 TYPE TABLE OF ty_month1 WITH HEADER LINE.
DATA:lt_month1_2 TYPE TABLE OF ty_month1 WITH HEADER LINE.

DATA:lt_month2 TYPE TABLE OF ty_month2 WITH HEADER LINE.
DATA:lt_month2_1 TYPE TABLE OF ty_month1 WITH HEADER LINE.
DATA:lt_month2_2 TYPE TABLE OF ty_month1 WITH HEADER LINE.

DATA:lt_year1 TYPE TABLE OF ty_month2 WITH HEADER LINE.
DATA:lt_year1_1 TYPE TABLE OF ty_month1 WITH HEADER LINE.
DATA:lt_year1_2 TYPE TABLE OF ty_month1 WITH HEADER LINE.

DATA: it_peraf1 TYPE peraf,
      it_peraf2 TYPE peraf.
*RANGES s_bukrs FOR t001-bukrs.


DATA: it_gjahr2 TYPE gjahr.
DATA: it_month1 TYPE nafaz.



DATA: l_monat    LIKE bkpf-monat,
      l_budat(8) TYPE c,
      g_count    TYPE i.                                          "��¼����
DATA tp_msg(50).

DATA: gt_bzdat_range  TYPE RANGE OF anep-bzdat.

*--------------------------------------------------------------------*
*& Selection Screen
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-000.
SELECT-OPTIONS: s_bukrs FOR t001-bukrs MEMORY ID buk OBLIGATORY. "��˾����
PARAMETERS: p_gjahr LIKE anek-gjahr DEFAULT sy-datum+0(4) OBLIGATORY,     "������
            p_monat LIKE bkpf-monat DEFAULT sy-datum+4(2) OBLIGATORY.    "�ڼ�
SELECTION-SCREEN SKIP 1.

PARAMETERS:     p_afabe LIKE anlb-afabe DEFAULT '01' OBLIGATORY. "�۾ɷ�Χ
SELECT-OPTIONS: s_anlkl FOR anla-anlkl,            "�ʲ�����
                s_anln1 FOR anla-anln1,            "�ʲ�����
                s_invnr FOR anla-invnr,            "�����
                s_sernr FOR anla-sernr,            "���к�
                s_kostl FOR anlz-kostl,            "�ɱ�����
                s_kostlv FOR anlz-kostlv,          "���γɱ�����
                s_caufn FOR ccss-aufnr,            "�ڲ�����
                s_ivdat FOR anla-ivdat,            "ԭʼ��������
                s_aktiv FOR anla-aktiv,            "�ʱ�������
                s_ord41 FOR anla-ord41,            "ʹ��״̬
                s_ord42 FOR anla-ord42,            "�ʲ���;
                s_ord43 FOR anla-ord43,            "���ӷ�ʽ
                s_ord44 FOR anla-ord44,            "���ٷ�ʽ
                s_deakt FOR anla-deakt.            "��������
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK block9 WITH FRAME TITLE w_title.
SELECTION-SCREEN COMMENT 1(30) w_char1.
SELECTION-SCREEN COMMENT /3(77) w_char2.
SELECTION-SCREEN COMMENT /1(77) w_char3.
SELECTION-SCREEN COMMENT /3(77) w_char4.
SELECTION-SCREEN COMMENT /1(77) w_char5.
SELECTION-SCREEN COMMENT /3(77) w_char6.
SELECTION-SCREEN END OF BLOCK block9.
PARAMETERS:p_flag TYPE char1 NO-DISPLAY.

*--------------------------------------------------------------------*

INITIALIZATION.
  w_title = '��������'.
  w_char1 = '������Ŀ�ġ�'.
  w_char2 = '���ʲ���ϸչ�̶ֹ��ʲ���Ƭ������Ϣ���ʲ���ֵ��Ϣ���۾������Ϣ'.
  w_char3 = '�����÷�Χ��'.
  w_char4 = '���й�˾'.
  w_char5 = '��ע�����'.
  w_char6 = '��'.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---{ replaced by IBM-NQD on 20170921 start
*---" ���ſƽ���ʱ����Ϊ��������ⲿsubmit,�����Ȩ��У��ͻ����������
*---" ��Ҫ�� p_flag ���ж�ɾ��
*---" old start
*  if p_flag = ''.
*    perform auth_check .
*  endif.
*---" old end
  PERFORM auth_check.
*---} replaced by IBM-NQD on 20170921 end
  PERFORM check_data.

*--------------------------------------------------------------------*
START-OF-SELECTION.
  REFRESH: it_list, it_anla, it_anlz, it_anlc, it_anek, it_anep,
           it_anea, it_anlp, it_anlb, it_anlbza, it_t087t.

  PERFORM data_init.

  PERFORM get_data.

  PERFORM process_data.

  PERFORM frm_alv_output .

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM data_init .

  DATA: ls_bzdat_range LIKE LINE OF gt_bzdat_range,
        lv_bzdat_from  TYPE anep-bzdat,
        lv_bzdat_to    TYPE anep-bzdat.

  CLEAR: gt_bzdat_range.

  lv_bzdat_from = p_gjahr && p_monat && '01'.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_bzdat_from
    IMPORTING
      last_day_of_month = lv_bzdat_to
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR: ls_bzdat_range.
  ls_bzdat_range-sign   = 'I'.
  ls_bzdat_range-option = 'BT'.
  ls_bzdat_range-low    = lv_bzdat_from.
  ls_bzdat_range-high   = lv_bzdat_to.
  APPEND ls_bzdat_range TO gt_bzdat_range.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       ��ȡ����������ʲ��������
*----------------------------------------------------------------------*
FORM get_data.

* ȡ����������
  SELECT ordnr ord4x ordtx INTO TABLE it_t087t
    FROM t087t                                     "����������
    WHERE spras = '1'.

  SELECT gdlgrp gdlgrp_txt INTO TABLE it_t087s
    FROM t087s                                     "������8
    WHERE spras = '1'.

  SELECT anlkl txk20 INTO TABLE it_ankt
    FROM ankt                                        "�ʲ��������
    WHERE spras = '1'.


* ȡ�ʲ�����¼
*  SELECT bukrs anlkl anln1 anln2 anlue txt50 txa50 sernr invnr menge meins
  SELECT anla~bukrs
    anlkl
    anla~anln1 anlue txt50 txa50 sernr invnr menge meins
         ivdat invzu aktiv ord41 ord42 ord43 ord44 gdlgrp lifnr liefe herst
         vbund typbz
    fiamt
    ehwnr
    ehwzu
    stadt
    grufl
    flurn

*    ANLU~ZZ_ASSET1
*    ANLU~ZZ_ASSET2

    deakt

    INTO TABLE it_anla
    FROM anla
      LEFT OUTER JOIN anlu ON anlu~bukrs = anla~bukrs
                     AND anlu~anln1 = anla~anln1
                     AND anlu~anln2 = anla~anln2
    WHERE anla~bukrs IN s_bukrs
    AND   anlkl IN s_anlkl  "s_anlue
    AND   anla~anln1 IN s_anln1
    AND   invnr IN s_invnr   "�����
    AND   sernr IN s_sernr   "���к�
    AND   aktiv <= g_lastdate
    AND   ivdat IN s_ivdat
    AND   ord41 IN s_ord41
    AND   ord42 IN s_ord42
    AND   ord43 IN s_ord43
    AND   ord44 IN s_ord44
    AND   aktiv IN s_aktiv
    AND   deakt IN s_deakt.

*  SORT it_anla BY bukrs anln1 anln2.
  SORT it_anla BY bukrs anln1.

* ȡʱ������ʲ�����
*  SELECT bukrs anln1 anln2 gsber kostl kostlv caufn raumn kfzkz
  SELECT bukrs anln1 kostl kostlv caufn raumn kfzkz ps_psp_pnr2
    werks
    stort
    INTO TABLE it_anlz
    FROM anlz
    WHERE bukrs IN s_bukrs
    AND   anln1 IN s_anln1
    AND   kostl IN s_kostl
    AND   kostlv IN s_kostlv   "�ɱ����Ķ��ʲ�����
    AND   bdatu GE g_lastdate  "��Ч���ڽ���
    AND   adatu LE g_lastdate "��Ч����ʼ����
    AND   caufn IN s_caufn.

  SORT it_anlz BY bukrs anln1.

* ȡ�۾�����
  SELECT bukrs anln1 afabe schrw afasl ndjar ndper afabg
    umjar
    umper
    ndurj
    ndurp
    INTO TABLE it_anlb
    FROM anlb
    WHERE bukrs IN s_bukrs
    AND   anln1 IN s_anln1
    AND   afabe  = p_afabe."in s_afabe.  ʵ���۾ɷ�Χ

  SORT it_anlb BY bukrs anln1.

* ȡ�۾�����
  SELECT bukrs anln1 afabe schrw afasl ndjar ndper
    INTO TABLE it_anlbza
    FROM anlbza                        "ʱ������۾�����
    WHERE bukrs IN s_bukrs
    AND   anln1 IN s_anln1
    AND   afabe  = p_afabe   "in s_afabe
    AND   adatu <= g_lastdate
    AND   bdatu >= g_lastdate.

  SORT it_anlbza BY bukrs anln1.

  LOOP AT it_anlbza.
    READ TABLE it_anlb WITH KEY bukrs = it_anlbza-bukrs anln1 = it_anlbza-anln1
                                afabe = it_anlbza-afabe BINARY SEARCH.
    IF sy-subrc = 0.
      it_anlbza-afabg = it_anlb-afabg.
      MODIFY it_anlbza TRANSPORTING afabg.
      CLEAR it_anlb.
    ENDIF.
  ENDLOOP.


* ȡ�ʲ�ֵ�ֶ�
  SELECT bukrs anln1 gjahr afabe kansw answl kaufw aufwb
         aufwl aufwv knafa ksafa kaafa nafag safag aufng zusna
         zussa zusaa nafav safav aafav nafal safal aafal aafag
         aufwp nafap safap aafap ndabj ndabp
    INTO CORRESPONDING FIELDS OF TABLE it_anlc
    FROM anlc
    WHERE bukrs IN s_bukrs
    AND   anln1 IN s_anln1
    AND   gjahr = p_gjahr
    AND   afabe = p_afabe
    AND   afabe  = p_afabe."in s_afabe.

  SORT it_anlc BY bukrs anln1.

* ȡƾ̧֤ͷ�ʲ�����
  SELECT bukrs anln1 gjahr monat lnran budat belnr buzei
    INTO CORRESPONDING FIELDS OF TABLE it_anek
    FROM anek
    WHERE bukrs IN s_bukrs
    AND   anln1 IN s_anln1
    AND   gjahr = p_gjahr
    AND   monat > p_monat.

  SORT it_anek BY bukrs anln1 gjahr lnran.

  IF NOT it_anek[] IS INITIAL.
*   ȡ�ʲ�����Ŀ
    SELECT bukrs anln1 gjahr lnran belnr buzei afabe bzdat bwasl anbtr xawbt lnsan
      INTO CORRESPONDING FIELDS OF TABLE it_anep
      FROM anep
      FOR ALL ENTRIES IN it_anek
      WHERE bukrs = it_anek-bukrs
      AND   anln1 = it_anek-anln1
      AND   gjahr = it_anek-gjahr
      AND   lnran = it_anek-lnran
      AND   afabe  = p_afabe "in s_afabe
      AND   bwasl NE 'Z31'
      AND   bwasl NOT BETWEEN 600 AND 699.
    SORT it_anep BY bukrs anln1 gjahr lnran afabe.

*   ȡ����ֵ���ʲ�����Ŀ
    IF NOT it_anep[] IS INITIAL.
      SELECT bukrs anln1 gjahr lnran afabe invzv aufwv
             aufwl nafav safav aafav invzl nafal safal aafal
        INTO TABLE it_anea
        FROM anea
        FOR ALL ENTRIES IN it_anep
        WHERE bukrs = it_anep-bukrs
        AND   anln1 = it_anep-anln1
*        AND   anln2 = it_anep-anln2
        AND   gjahr = it_anep-gjahr
        AND   lnran = it_anep-lnran   "������ʲ�����Ŀ�����
        AND   afabe = it_anep-afabe.

      SORT it_anea BY bukrs anln1 gjahr lnran afabe.
    ENDIF.
  ENDIF.

* ȡ�ʲ��ڼ��ֵ
  SELECT bukrs gjahr peraf anln1 afaber
         aufwz nafaz safaz aafaz belnr
         nafag safag aafag
    INTO CORRESPONDING FIELDS OF TABLE it_anlp
    FROM anlp
    WHERE bukrs IN s_bukrs
    AND   anln1 IN s_anln1
    AND   gjahr = p_gjahr
    AND   afaber  = p_afabe  "in s_afabe  ʵ�ʵĻ��������۾ɷ�Χ
    AND   peraf GT p_monat.

  SORT it_anlp BY bukrs anln1 afaber.

ENDFORM. "get_data

*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       �������������������alv���ڲ���
*----------------------------------------------------------------------*
FORM process_data.

  DATA: l_flag(1),
        l_x         TYPE p15_months,
        l_datetime  LIKE sy-datum,
        l_gjahrm(6) TYPE c.

  DATA:lv_gjahr  TYPE bkpf-gjahr,
       lv_monat  TYPE bkpf-monat,
       lv_num(3) TYPE n.


  LOOP AT it_anlc.
    CLEAR it_list.
    it_list-bukrs = it_anlc-bukrs.
    it_list-anln1 = it_anlc-anln1. "�ʲ�����
    it_list-afabe = it_anlc-afabe. "�۾ɷ�Χ
    it_list-zcyz = it_anlc-kansw + it_anlc-answl.  "ԭֵ = �ۻ����ú��������� + "�����Ӱ���ʲ�ֵ��ҵ��
    it_list-jzzb = ( it_anlc-kaufw + it_anlc-aufwb + it_anlc-aufwl + it_anlc-aufwv ) * -1.
    "��ֵ׼�� = "���ü�ֵ���ۼ��ع� + "���ü�ֵ���ع����� + "�����й��滻ֵ�ı����ع� + "�й��滻ֵ�ı����ۻ��ع�

    it_list-ljzj_zc = ( it_anlc-knafa + it_anlc-ksafa + it_anlc-kaafa + it_anlc-nafag
                      + it_anlc-safag + it_anlc-aufng + it_anlc-zusna + it_anlc-zussa
                      + it_anlc-zusaa + it_anlc-nafav + it_anlc-safav + it_anlc-aafav
                      + it_anlc-nafal + it_anlc-safal ) * -1. "�ۼ��۾ɣ�������
    it_list-ljzj_jhw = ( it_anlc-aafag + it_anlc-aafal ) * -1. "�ۼ��۾ɣ��ƻ��⣩

    COLLECT it_list.
  ENDLOOP.

  LOOP AT it_anep.

    READ TABLE it_anek WITH KEY bukrs = it_anep-bukrs
                                anln1 = it_anep-anln1
                                gjahr = it_anep-gjahr
                                lnran = it_anep-lnran
                                BINARY SEARCH.


    it_anep-anbtra = it_anep-anbtr.

    READ TABLE it_anea WITH KEY bukrs = it_anep-bukrs            "����ֵ���ʲ�����Ŀ
                                anln1 = it_anep-anln1
                                gjahr = it_anep-gjahr
                                lnran = it_anep-lnran            "������ʲ�����Ŀ�����
                                afabe = it_anep-afabe
                                BINARY SEARCH.
    IF sy-subrc EQ 0.

      it_anep-anbtrb = - it_anea-aufwv - it_anea-aufwl.
      IF g_lastdate+4(4) EQ '1231'.
        it_anep-anbtrc = - it_anea-nafav - it_anea-nafal - it_anea-safav - it_anea-safal - it_anea-aafav - it_anea-aafal.
        it_anep-anbtrd = 0.
      ELSE.
        it_anep-anbtrc = - it_anea-nafav - it_anea-nafal - it_anea-safav - it_anea-aafav.
        it_anep-anbtrd = - it_anea-safal - it_anea-aafal.
      ENDIF.
    ENDIF.

    CLEAR it_list.
    it_list-bukrs = it_anep-bukrs.
    it_list-anln1 = it_anep-anln1.
    it_list-afabe = it_anep-afabe.


    it_list-zcyz = - it_anep-anbtra.        "���ʽ��

    it_list-jzzb = - it_anep-anbtrb.
    it_list-ljzj_zc = - it_anep-anbtrc.
    it_list-ljzj_jhw = - it_anep-anbtrd.
    COLLECT it_list.
*    endif.
  ENDLOOP.

  LOOP AT it_anlp.
    IF it_anlp-aufwz <> 0.
      it_anlp-bwasl = 'Z31'.
      it_anlp-zanbtrb = it_anlp-aufwz.
    ENDIF.

    IF it_anlp-nafaz <> 0.
      it_anlp-bwasl = '500'.
      it_anlp-zanbtrc = it_anlp-nafaz.
    ENDIF.

    IF it_anlp-safaz <> 0.
      it_anlp-bwasl = '500'.
      it_anlp-zanbtrd = it_anlp-safaz.
    ENDIF.

    IF it_anlp-aafaz <> 0.
      it_anlp-bwasl = '500'.
      it_anlp-zanbtrd = it_anlp-zanbtrd + it_anlp-aafaz.
    ENDIF.

    IF it_anlp-peraf > 12.
      l_monat = 12.
    ELSE.
      l_monat = it_anlp-peraf.
    ENDIF.
    CLEAR: l_datetime,l_gjahrm.
    CONCATENATE it_anlp-gjahr l_monat '01' INTO l_datetime.
    CONCATENATE it_anlp-gjahr it_anlp-peraf+1(2) INTO l_gjahrm.
    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = l_datetime
      IMPORTING
        e_date = it_anlp-budat.

    IF l_gjahrm > g_lastdate+0(6).
      CLEAR it_list.
      it_list-bukrs = it_anlp-bukrs.
      it_list-anln1 = it_anlp-anln1.
      it_list-afabe = it_anlp-afaber.
      it_list-jzzb = it_anlp-zanbtrb.
      it_list-ljzj_zc = it_anlp-zanbtrc.
      it_list-ljzj_jhw = it_anlp-zanbtrd.
      COLLECT it_list.
    ENDIF.
  ENDLOOP.



  SELECT bukrs anln1
  FROM anla
  APPENDING CORRESPONDING FIELDS OF TABLE it_list
  FOR ALL ENTRIES IN it_anla
  WHERE bukrs = it_anla-bukrs AND anln1 = it_anla-anln1 AND aktiv  = '00000000'.


  LOOP AT it_list ASSIGNING <it_list>.
    READ TABLE it_anla WITH KEY bukrs = <it_list>-bukrs
                                anln1 = <it_list>-anln1
                                BINARY SEARCH.
    IF sy-subrc NE 0.
      DELETE it_list.
    ELSEIF sy-subrc EQ 0.
      <it_list>-anlue = it_anla-anlue.
      <it_list>-txt50 = it_anla-txt50.
      <it_list>-txa50 = it_anla-txa50.
      <it_list>-invnr = it_anla-invnr.
      <it_list>-sernr = it_anla-sernr.
      <it_list>-menge = it_anla-menge.
      <it_list>-meins = it_anla-meins.
      <it_list>-ivdat = it_anla-ivdat.  "�������
      <it_list>-invzu = it_anla-invzu.
      <it_list>-aktiv = it_anla-aktiv.  "�ʱ�������
      <it_list>-deakt = it_anla-deakt.  "��������
      <it_list>-lifnr = it_anla-lifnr.
      <it_list>-liefe = it_anla-liefe.
      IF it_anla-lifnr IS NOT INITIAL AND
          it_anla-liefe IS INITIAL.
        SELECT SINGLE name1
          INTO <it_list>-liefe
            FROM lfa1
              WHERE lifnr = it_anla-lifnr.
      ENDIF.

      <it_list>-herst = it_anla-herst.
      <it_list>-vbund = it_anla-vbund.
      <it_list>-typbz = it_anla-typbz.
      <it_list>-anlkl = it_anla-anlkl.

      <it_list>-fiamt	= it_anla-fiamt.
      <it_list>-ehwnr	= it_anla-ehwnr.
      <it_list>-ehwzu	= it_anla-ehwzu.
      <it_list>-stadt	= it_anla-stadt.
      <it_list>-grufl	= it_anla-grufl.
      <it_list>-flurn	= it_anla-flurn.
*      <IT_LIST>-ZZ_ASSET1  = IT_ANLA-ZZ_ASSET1.
*      <IT_LIST>-ZZ_ASSET2  = IT_ANLA-ZZ_ASSET2.

      READ TABLE it_t087s WITH KEY gdlgrp = it_anla-gdlgrp.
      IF sy-subrc = 0.
        <it_list>-gdlgrp = it_t087s-gdlgrp_txt.
      ENDIF.

      READ TABLE it_t087t WITH KEY ordnr = '1'
                                   ord4x = it_anla-ord41.
      IF sy-subrc = 0.
        <it_list>-ord41 = it_t087t-ordtx.
      ENDIF.

      READ TABLE it_t087t WITH KEY ordnr = '2'
                                   ord4x = it_anla-ord42.
      IF sy-subrc = 0.
        <it_list>-ord42 = it_t087t-ordtx.
      ENDIF.

      <it_list>-ord43 = it_anla-ord43.
      READ TABLE it_t087t WITH KEY ordnr = '3'
                                   ord4x = it_anla-ord43.
      IF sy-subrc = 0.
        <it_list>-ord43_text = it_t087t-ordtx.
      ENDIF.

      READ TABLE it_t087t WITH KEY ordnr = '4'
                                   ord4x = it_anla-ord44.
      IF sy-subrc = 0.
        <it_list>-ord44 = it_t087t-ordtx.
      ENDIF.

      READ TABLE it_ankt WITH  KEY anlkl = it_anla-anlkl.
      IF sy-subrc = 0.
        <it_list>-txk20 = it_ankt-txk20.
      ENDIF.

      READ TABLE it_anlz WITH KEY bukrs = <it_list>-bukrs
                             anln1 = <it_list>-anln1.

      IF sy-subrc EQ 0.
        <it_list>-kostl = it_anlz-kostl.
        <it_list>-kostlv = it_anlz-kostlv.
        <it_list>-raumn = it_anlz-raumn.
        <it_list>-kfzkz = it_anlz-kfzkz.
        <it_list>-caufn = it_anlz-caufn.
        <it_list>-ps_psp_pnr2 = it_anlz-ps_psp_pnr2.
        <it_list>-werks       = it_anlz-werks.
        <it_list>-stort       = it_anlz-stort.
      ELSE.
        DELETE it_list.
        CONTINUE.
      ENDIF.

      "12��  �����û�����������BUKRS��ANLN1�� ANLN2��AFABE��GJAHR��ZMONTHH��ѡ��ANLBZAȡBUKRS��
      "ANLN1�� ANLN2��AFASL��NDJAR��NDPER��SCHRW��AFABG����ZANEPTEMP2��ȡZANEPTEMP2�����ֶ�+
      "AFASL��NDJAR��NDPER��SCHRW��AFABG�����ɱ�ZANEPTEMP3�����δ�ҵ�ANLBZA��¼��������û���
      "��������BUKRS��ANLN1�� ANLN2��AFABE����ANLB�����ɱ�ZANEPTEMP3
      READ TABLE it_anlbza WITH KEY bukrs = <it_list>-bukrs
                            anln1 = <it_list>-anln1
                            afabe = <it_list>-afabe   "�۾ɷ�Χ
                            BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF it_anlbza-schrw <> 0.
          <it_list>-schrw = it_anlbza-schrw.
        ELSE.
          READ TABLE it_afapl WITH KEY bukrs = <it_list>-bukrs.
          IF sy-subrc = 0.
            SELECT SINGLE anhwsl INTO g_anhwsl
              FROM t090na
              WHERE afapl = it_afapl-afapl
              AND   afasl = it_anlbza-afasl.

            IF sy-subrc = 0.
              SELECT SINGLE ahproz INTO g_ahproz
                FROM t091p
                WHERE anhwsl = g_anhwsl.
              IF sy-subrc = 0.
                <it_list>-schrw = ( <it_list>-zcyz - <it_list>-jzzb - <it_list>-schrw ) * g_ahproz / 100.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        <it_list>-afasl = it_anlbza-afasl.
        <it_list>-afabg = it_anlbza-afabg.  "�۾ɼ��㿪ʼ����
        <it_list>-zzndja = it_anlbza-ndjar * 12 + it_anlbza-ndper.  "�ƻ�ʹ���ڼ�=�ƻ���ʹ����*12+�ƻ�ʹ���ڼ�

        CLEAR l_x.
        IF <it_list>-afabg >= g_lastdate.  "�۾ɼ��㿪ʼ����  >="�������һ��
          CALL FUNCTION 'HR_IT_TFR_MONTHS_BETWEEN_DATES'
            EXPORTING
              begda  = g_lastdate
              endda  = <it_list>-afabg
            IMPORTING
              months = l_x.
        ELSE.
          CALL FUNCTION 'HR_IT_TFR_MONTHS_BETWEEN_DATES'
            EXPORTING
              begda  = <it_list>-afabg
              endda  = g_lastdate
            IMPORTING
              months = l_x.
        ENDIF.
        <it_list>-zzndjb = l_x.  "��ʹ���ڼ�

        SELECT SINGLE nafaz INTO it_month1 FROM anlp  "�ʲ��ڼ��ֵ
            WHERE  bukrs = <it_list>-bukrs
               AND anln1 = <it_list>-anln1
*               AND anln2 = <it_list>-anln2
               AND gjahr = p_gjahr
               AND afaber = p_afabe
               AND peraf = p_monat.  "�۾ɼ�����
        IF sy-subrc NE 0.
*          <it_list>-zzndjb =  <it_list>-zzndjb - 1.
        ENDIF.

        IF <it_list>-zzndjb > <it_list>-zzndja.
          <it_list>-zzndjb = <it_list>-zzndja.
        ENDIF.
      ELSE.
        READ TABLE it_anlb WITH KEY bukrs = <it_list>-bukrs
                              anln1 = <it_list>-anln1
                              afabe = <it_list>-afabe   ""�۾ɷ�Χ
                              BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF it_anlb-schrw <> 0.
            <it_list>-schrw = it_anlb-schrw. "�ʲ���ֵ
          ELSE.
            READ TABLE it_afapl WITH KEY bukrs = <it_list>-bukrs.
            IF sy-subrc = 0.
              SELECT SINGLE anhwsl INTO g_anhwsl
                FROM t090na   "�۾���
                WHERE afapl = it_afapl-afapl
                AND   afasl = it_anlb-afasl.

              IF sy-subrc = 0.
                SELECT SINGLE ahproz INTO g_ahproz
                  FROM t091p  "��ֵֹ�ٷ���
                  WHERE anhwsl = g_anhwsl.
                IF sy-subrc = 0.
                  <it_list>-schrw = ( <it_list>-zcyz - <it_list>-jzzb ) * g_ahproz / 100.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          <it_list>-afasl = it_anlb-afasl.
          <it_list>-afabg = it_anlb-afabg.
          <it_list>-zzndja = it_anlb-ndjar * 12 + it_anlb-ndper.

          CLEAR l_x.
          IF <it_list>-afabg >= g_lastdate.
            CALL FUNCTION 'HR_IT_TFR_MONTHS_BETWEEN_DATES'
              EXPORTING
                begda  = g_lastdate
                endda  = <it_list>-afabg
              IMPORTING
                months = l_x.
          ELSE.
            CALL FUNCTION 'HR_IT_TFR_MONTHS_BETWEEN_DATES'
              EXPORTING
                begda  = <it_list>-afabg
                endda  = g_lastdate
              IMPORTING
                months = l_x.
          ENDIF.
          <it_list>-zzndjb = l_x.


          SELECT SINGLE nafaz INTO it_month1 FROM anlp  "�ʲ��ڼ��ֵ
              WHERE  bukrs = <it_list>-bukrs
                 AND anln1 = <it_list>-anln1
*                 AND anln2 = <it_list>-anln2
                 AND afaber = p_afabe
                 AND gjahr = p_gjahr
                 AND peraf = p_monat.  "�۾ɼ�����

          IF sy-subrc NE 0.
*            <it_list>-zzndjb =  <it_list>-zzndjb - 1.
          ENDIF.


          IF <it_list>-zzndjb > <it_list>-zzndja.
            <it_list>-zzndjb = <it_list>-zzndja.
          ENDIF.
        ENDIF.

      ENDIF.


      CLEAR:lv_gjahr, lv_monat, lv_num.
      IF <it_list>-aktiv >= <it_list>-afabg.
        lv_gjahr = <it_list>-aktiv(4) - <it_list>-afabg(4).
        IF lv_gjahr IS INITIAL..
          lv_num = <it_list>-aktiv+4(2) - <it_list>-afabg+4(2).
        ELSE.
          lv_num = lv_gjahr * 12 + <it_list>-aktiv+4(2) - <it_list>-afabg+4(2).
        ENDIF.
        IF <it_list>-zzndja < lv_num.
          <it_list>-schrw = <it_list>-zcyz.
        ENDIF.
      ENDIF.


*      if g_lastdate+4(4) eq '1231'.
      <it_list>-ljzj_zc = <it_list>-ljzj_zc + <it_list>-ljzj_jhw.
*        <it_list>-ljzj_jhw = 0.
*      endif.

      IF <it_list>-zzndjb = <it_list>-zzndja.  "��ʹ���ڼ� = �ƻ�ʹ���ڼ�
        <it_list>-zmonthje1 = 0.
      ELSE.
        <it_list>-zmonthje1 = ( <it_list>-zcyz - <it_list>-jzzb - <it_list>-ljzj_zc - <it_list>-schrw ) / ( <it_list>-zzndja - <it_list>-zzndjb ).
        "  �����۾� = ( zcyz "ԭֵ - jzzb "��ֵ׼�� - ljzj_zc "�ۼ��۾ɣ�������- schrw ��ֵ )  / ( �ƻ�ʹ���ڼ� - ʵ��ʹ���ڼ� )
      ENDIF.

      <it_list>-zmonthje = <it_list>-zmonthje1. "�����۾�

*      "ZQINGDUNȡ���߼�Ϊ����ANLKLΪ100100-100299ʱ��ZQINGDUN=& KFZKZ�������ʱִ���ƺ�ֵKFZKZ��
*      "��ANLKLΪ100300-100399ʱ��ZQINGDUN=& KFZKZ*�����������ZP1_USEREXIT_002�и�ANLKL��KFZKZ��
*      "�����ʱִ���ƺ�ֵKFZKZ

      <it_list>-zcjz = <it_list>-zcyz - <it_list>-jzzb - <it_list>-ljzj_zc - <it_list>-ljzj_jhw.
    ENDIF.
  ENDLOOP.

  "ALV����һ���ֶΣ�ʣ��ʹ���ڼ䣬��ʾ����ʹ���ڼ�֮��ȡ����ʽΪ���ƻ�ʹ���ڼ�-��ʹ���ڼ䣩

  LOOP AT it_list ASSIGNING <it_list>.

    SELECT SINGLE ltext INTO <it_list>-ltext FROM cskt   "�ɱ�����˵��
      WHERE kostl = <it_list>-kostl
      AND   kokrs = '3000'
      AND   datbi GE g_lastdate.

    SELECT SINGLE ltext INTO <it_list>-ltextv FROM cskt  "���γɱ�����˵��
      WHERE kostl = <it_list>-kostlv AND
            kokrs = '3000' AND
            datbi GE g_lastdate.


*  ȡ�ڲ��������� aufk
    SELECT SINGLE ktext INTO <it_list>-ktext FROM aufk WHERE aufnr = <it_list>-caufn.

    "  ȡ�۾�������
    SELECT SINGLE afatxt INTO <it_list>-afatxt FROM t090nat WHERE afasl = <it_list>-afasl
                                                              AND spras = sy-langu
                                                              AND afapl = 'GWGK'.
*  ȡWBSԪ������ prps
    SELECT SINGLE post1 INTO <it_list>-post1 FROM prps WHERE pspnr = <it_list>-ps_psp_pnr2.

*  ȡ�������ĺ�������������

    IF <it_list>-kostl IS NOT INITIAL.
      SELECT SINGLE prctr INTO <it_list>-prctr FROM csks   "�ɱ�����˵��
            WHERE kostl = <it_list>-kostl AND
                  kokrs = '3000' AND
                  datbi GE g_lastdate.
    ELSEIF <it_list>-ps_psp_pnr2 <> '00000000'.
      SELECT SINGLE prctr INTO <it_list>-prctr FROM prps   "�ɱ�����˵��
           WHERE pspnr = <it_list>-ps_psp_pnr2.
    ENDIF.

    SELECT SINGLE ltext INTO <it_list>-ltextp FROM cepct   "���������������ı�
          WHERE prctr = <it_list>-prctr AND
                kokrs = '3000' AND
                datbi GE g_lastdate.

    <it_list>-zsysyqj = <it_list>-zzndja - <it_list>-zzndjb.
*    ENDIF.
  ENDLOOP.


  it_peraf1 = p_monat .

  IF p_monat = '1'.
    it_gjahr2 = p_gjahr - 1.
    it_peraf2 = '12'.
  ELSE.
    it_peraf2 = p_monat - 1.
  ENDIF.
  "�����۾�
  SORT it_list BY bukrs anln1.
  REFRESH:lt_month1,lt_month2,lt_year1.
  CLEAR:lt_month1,lt_month2,lt_year1.

  SELECT  bukrs anln1 gjahr afaber peraf nafaz
    INTO TABLE lt_month1
    FROM anlp  "�ʲ��ڼ��ֵ
    FOR ALL ENTRIES IN it_list
    WHERE  bukrs  = it_list-bukrs
       AND anln1  = it_list-anln1
       AND gjahr  = p_gjahr
       AND afaber = p_afabe
       AND peraf  = it_peraf1.  "�۾ɼ�����

  "�����۾�
  IF p_monat = '1'.
    SELECT  bukrs anln1 gjahr afaber peraf nafaz
       INTO TABLE lt_month2
       FROM anlp  "�ʲ��ڼ��ֵ
       FOR ALL ENTRIES IN it_list
       WHERE  bukrs = it_list-bukrs "= <it_list>-bukrs
          AND anln1 = it_list-anln1
          AND gjahr = it_gjahr2
          AND afaber = p_afabe
          AND peraf = it_peraf2.  "�۾ɼ�����
  ELSE.
    SELECT bukrs anln1 gjahr afaber peraf nafaz
      INTO TABLE lt_month2
      FROM anlp  "�ʲ��ڼ��ֵ
      FOR ALL ENTRIES IN it_list
     WHERE bukrs = it_list-bukrs "= <it_list>-bukrs
       AND anln1 = it_list-anln1
       AND gjahr = p_gjahr
       AND afaber = p_afabe
       AND peraf = it_peraf2.  "�۾ɼ�����
  ENDIF.
  "�����۾�
  SELECT bukrs anln1 gjahr afaber peraf nafaz
       INTO TABLE lt_year1
       FROM anlp
     FOR ALL ENTRIES IN it_list
     WHERE bukrs = it_list-bukrs
       AND anln1 = it_list-anln1
       AND gjahr = p_gjahr
       AND afaber = p_afabe
       AND peraf <= p_monat.
  "�����۾�&�����۾�
  LOOP AT it_list WHERE bukrs IN s_bukrs.

    it_peraf1 = p_monat .

    IF p_monat = '1'.
      it_gjahr2 = p_gjahr - 1.
      it_peraf2 = '12'.
    ELSE.
      it_peraf2 = p_monat - 1.
    ENDIF.

***xs***********************************
*�����۾�
    REFRESH:lt_month1_2,lt_month1_1.
    CLEAR:lt_month1_2,lt_month1_1.

    REFRESH:lt_month2_2,lt_month2_1.
    CLEAR:lt_month2_2,lt_month2_1.

    REFRESH:lt_year1_2,lt_year1_1.
    CLEAR:lt_year1_2,lt_year1_1.

*    SELECT  bukrs nafaz INTO TABLE lt_month1 FROM anlp  "�ʲ��ڼ��ֵ
*      WHERE  bukrs = it_list-bukrs
*         AND anln1 = it_list-anln1
*         AND gjahr = p_gjahr
*         AND afaber = p_afabe
*         AND peraf = it_peraf1.  "�۾ɼ�����
    LOOP AT lt_month1 WHERE bukrs = it_list-bukrs
                         AND anln1 = it_list-anln1
                         AND gjahr = p_gjahr
                         AND afaber = p_afabe
                         AND peraf = it_peraf1.  "�۾ɼ�����
      lt_month1_1-bukrs = lt_month1-bukrs.
      lt_month1_1-month_1 = lt_month1-nafaz.
      APPEND lt_month1_1.
      CLEAR lt_month1.
    ENDLOOP.

    LOOP AT lt_month1_1.
      COLLECT lt_month1_1 INTO lt_month1_2.
      CLEAR lt_month1_1.
    ENDLOOP.

    IF lt_month1_2[] IS NOT INITIAL .
      LOOP AT lt_month1_2.
        it_list-month_1 = lt_month1_2-month_1.
      ENDLOOP.
    ENDIF.
*    SELECT SUM( nafaz ) INTO it_list-month_1 FROM anlp  "�ʲ��ڼ��ֵ
*      WHERE  bukrs = it_list-bukrs "= <it_list>-bukrs
*         AND anln1 = it_list-anln1
*         AND gjahr = p_gjahr
*         AND afaber = p_afabe
*         AND peraf = it_peraf1.  "�۾ɼ�����
***xs***********************************

*�����۾�
    IF p_monat = '1'.
      LOOP AT lt_month2 WHERE  bukrs = it_list-bukrs "= <it_list>-bukrs
                           AND anln1 = it_list-anln1
                           AND gjahr = it_gjahr2
                           AND afaber = p_afabe
                           AND peraf = it_peraf2.  "�۾ɼ�����
        lt_month2_1-bukrs = lt_month2-bukrs.
        lt_month2_1-month_1 = lt_month2-nafaz.
        APPEND lt_month2_1.
        CLEAR lt_month2.
      ENDLOOP.

      LOOP AT lt_month2_1.
        COLLECT lt_month2_1 INTO lt_month2_2.
        CLEAR lt_month2_1.
      ENDLOOP.

      IF lt_month2_2[] IS NOT INITIAL .
        LOOP AT lt_month2_2.
          it_list-month_2 = lt_month2_2-month_1.
        ENDLOOP.
      ENDIF.
    ELSE.
      LOOP AT lt_month2 WHERE  bukrs = it_list-bukrs "= <it_list>-bukrs
                           AND anln1 = it_list-anln1
                           AND gjahr = p_gjahr
                           AND afaber = p_afabe
                           AND peraf = it_peraf2.  "�۾ɼ�����
        lt_month2_1-bukrs = lt_month2-bukrs.
        lt_month2_1-month_1 = lt_month2-nafaz.
        APPEND lt_month2_1.
        CLEAR lt_month2.
      ENDLOOP.

      LOOP AT lt_month2_1.
        COLLECT lt_month2_1 INTO lt_month2_2.
        CLEAR lt_month2_1.
      ENDLOOP.

      IF lt_month2_2[] IS NOT INITIAL .
        LOOP AT lt_month2_2.
          it_list-month_2 = lt_month2_2-month_1.
        ENDLOOP.
      ENDIF.
    ENDIF.
    it_list-month_1 = 0 - it_list-month_1.
    it_list-month_2 = 0 - it_list-month_2.

*����������۾ɶ�
    "*add by PENGLIXUE at 20180412 BPMNO:	ERP-YW201804090192
*    data: l_nafag like anlp-nafag,
*          l_safag like anlp-safag,
*          l_aafag like anlp-aafag.
*    clear:l_nafag,
*          l_safag,
*          l_aafag.

*    select  nafag safag aafag  into (l_nafag, l_safag, l_aafag ) from anlc
**              WHERE bukrs = it_list-bukrs AND anln2 = it_list-anln2
*              where bukrs = it_list-bukrs
*                and anln1 = it_list-anln1
*                and gjahr = p_gjahr
*                and afabe = '01' .
***xs***********************************
*    SELECT bukrs nafaz INTO TABLE lt_year1 FROM anlp
*               WHERE bukrs = it_list-bukrs
*                 AND anln1 = it_list-anln1
*                 AND gjahr = p_gjahr
*                 AND afaber = p_afabe
*                 AND peraf <= p_monat.
    LOOP AT lt_year1  WHERE bukrs = it_list-bukrs
                        AND anln1 = it_list-anln1
                        AND gjahr = p_gjahr
                        AND afaber = p_afabe
                        AND peraf <= p_monat.
      lt_year1_1-bukrs   = lt_year1-bukrs.
      lt_year1_1-month_1 = lt_year1-nafaz.
      APPEND lt_year1_1.
      CLEAR lt_year1.
    ENDLOOP.
    LOOP AT lt_year1_1.
      COLLECT lt_year1_1 INTO lt_year1_2.
      CLEAR lt_year1_1.
    ENDLOOP.

    IF lt_year1_2[] IS NOT INITIAL .
      LOOP AT lt_year1_2.
        it_list-year_1 = lt_year1_2-month_1.
      ENDLOOP.
    ENDIF.

*    SELECT SINGLE SUM( nafaz ) INTO it_list-year_1 FROM anlp
*       WHERE bukrs = it_list-bukrs
*              AND anln1 = it_list-anln1
*              AND gjahr = p_gjahr
*              AND afaber = p_afabe
*              AND peraf <= p_monat.
***xs***********************************
*      if sy-subrc = 0.
*        it_list-year_1 = l_nafag + l_safag + l_aafag.
    it_list-year_1 = 0 - it_list-year_1.
*      endif.

*    endselect.
    "*end of add by PENGLIXUE
    MODIFY it_list TRANSPORTING month_1 month_2 year_1.
  ENDLOOP.

  "ALV����һ���ֶΣ�ʣ��ʹ���ڼ䣬��ʾ����ʹ���ڼ�֮��ȡ����ʽΪ���ƻ�ʹ���ڼ�-��ʹ���ڼ䣩�����޸� End

*---{ added by ibm-nqd on 20170710 start @change_0002
  PERFORM process_data_additional.
*---} added by ibm-nqd on 20170710 end @change_0002

ENDFORM. "process_data

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
* ��������/���ڼ���
*---------------------------------------------------------------------*
FORM process_data_additional .

  TYPES: BEGIN OF ty_anek,
           bukrs TYPE anek-bukrs,
           anln1 TYPE anek-anln1,
           anln2 TYPE anek-anln2,
           gjahr TYPE anek-gjahr,
           lnran TYPE anek-lnran,
           belnr TYPE anek-belnr,
           monat TYPE anek-monat,
         END OF ty_anek.

  TYPES: BEGIN OF ty_anea_anek,
*" ANEA
           bukrs TYPE anea-bukrs,
           anln1 TYPE anea-anln1,
           anln2 TYPE anea-anln2,
           gjahr TYPE anea-gjahr,
           lnran TYPE anea-lnran,
           afabe TYPE anea-afabe,
           zujhr TYPE anea-zujhr,
           zucod TYPE anea-zucod,
           nafav TYPE anea-nafav,
           nafal TYPE anea-nafal,
*" ANEK
           monat TYPE anek-monat,
         END OF ty_anea_anek.

  TYPES: BEGIN OF ty_anep,
           bukrs TYPE anep-bukrs,
           anln1 TYPE anep-anln1,
           anln2 TYPE anep-anln2,
           gjahr TYPE anep-gjahr,
           lnran TYPE anep-lnran,
           afabe TYPE anep-afabe,
           zujhr TYPE anep-zujhr,
           zucod TYPE anep-zucod,
           monat TYPE anek-monat, " �ڼ�
           belnr TYPE anek-belnr,
           tcode TYPE anek-tcode,
           anbtr TYPE anep-anbtr,
           bwasl TYPE anep-bwasl,
*           belnr type anep-belnr,
           shkzg TYPE bseg-shkzg,
           lnsan TYPE anep-lnsan,
           stblg TYPE rbkp-stblg,
         END OF ty_anep.

  TYPES: BEGIN OF ty_anep_collect,
           bukrs      TYPE anep-bukrs,
           anln1      TYPE anep-anln1,
           gjahr      TYPE anep-gjahr,
           monat      TYPE anek-monat,
*           shkzg type bseg-shkzg,
           bqzj       TYPE faglflext-hslvt,
           bqjs       TYPE faglflext-hslvt,
           jzzb_bq    TYPE faglflext-hslvt,   " ���ڼ�ֵ׼��
           shzhq_yz   TYPE faglflext-hslvt,   " �ۺ����ǰԭֵ�������������
           shzhq_jzzb TYPE faglflext-hslvt,   " �ۺ����ǰ��ֵ׼���������������
         END OF ty_anep_collect.

  TYPES: BEGIN OF ty_tabw,
           bwasl  TYPE tabw-bwasl,
           anshkz TYPE tabw-anshkz,
         END OF ty_tabw.

  TYPES: BEGIN OF ty_t001w,
           werks TYPE t001w-werks,
           name1 TYPE t001w-name1,
         END OF ty_t001w.

  DATA: lt_anek      TYPE STANDARD TABLE OF ty_anek,
        lt_anek_shzh TYPE STANDARD TABLE OF ty_anek,  " �ۺ���أ������������
        ls_anek      TYPE ty_anek.

  DATA: lt_anep      TYPE STANDARD TABLE OF ty_anep,
        lt_anep_shzh TYPE STANDARD TABLE OF ty_anep,  " �ۺ���أ������������
        ls_anep      TYPE ty_anep.

  DATA: lt_anlc_shzh      TYPE STANDARD TABLE OF anlc,     " �ۺ���أ������������
        lt_anlc_shzh_temp TYPE STANDARD TABLE OF anlc,     " �ۺ���أ������������
        ls_anlc           TYPE anlc,
        ls_anlc_collect   TYPE anlc.                       " @change_0005

  DATA: lt_anep_collect      TYPE STANDARD TABLE OF ty_anep_collect,
        lt_anep_shzh_collect TYPE STANDARD TABLE OF ty_anep_collect,
        ls_anep_collect      TYPE ty_anep_collect.

  DATA: lt_anlp              TYPE STANDARD TABLE OF anlp,
        ls_anlp              LIKE LINE OF lt_anlp,
        lt_anlp_collect      TYPE STANDARD TABLE OF anlp,
        lt_anlp_collect_temp TYPE STANDARD TABLE OF anlp,
        ls_anlp_collect      LIKE LINE OF lt_anlp.

  DATA: lt_anlc_ljzj_qc         TYPE STANDARD TABLE OF anlc,   " @change_0005
        lt_anlc_ljzj_qc_collect TYPE STANDARD TABLE OF anlc.   " @change_0005

  DATA: lt_tabw_temp TYPE STANDARD TABLE OF ty_tabw,
        lt_tabw      TYPE STANDARD TABLE OF ty_tabw,
        ls_tabw      TYPE ty_tabw.

  DATA: lt_t001w      TYPE STANDARD TABLE OF ty_t001w,
        lt_t001w_temp TYPE STANDARD TABLE OF ty_t001w,
        ls_t001w      TYPE ty_t001w.

  DATA: lt_t499s      TYPE STANDARD TABLE OF t499s,
        lt_t499s_temp TYPE STANDARD TABLE OF t499s,
        ls_t499s      TYPE t499s.

  DATA: lt_ztfi_002 TYPE STANDARD TABLE OF ztfi_asset_confi WITH HEADER LINE.

  DATA: lv_current_period TYPE numc06.

  DATA: ls_anlb LIKE LINE OF it_anlb,
        lt_anlb LIKE STANDARD TABLE OF it_anlb.

  DATA: lt_anea TYPE STANDARD TABLE OF ty_anea_anek,
        ls_anea TYPE ty_anea_anek.
  DATA:       lt_anea_ad TYPE STANDARD TABLE OF ty_anea_anek.
  FIELD-SYMBOLS: <fs_anep>   TYPE ty_anep,
                 <fs_output> LIKE it_list.


  lv_current_period = p_gjahr && p_monat.
  lt_anlb = it_anlb[].

*" ���ñ�����
  SELECT *
    INTO TABLE lt_ztfi_002
    FROM ztfi_asset_confi
    WHERE bwagrp = '10'.
  IF sy-subrc = 0.
    SORT lt_ztfi_002 ASCENDING BY bwagrp bwasl.
  ENDIF.

  SELECT
    bukrs
    anln1
    anln2
    gjahr
    lnran
    belnr
    INTO TABLE lt_anek
    FROM anek
    WHERE bukrs IN s_bukrs
      AND anln1 IN s_anln1
      AND gjahr = p_gjahr
*      and monat = p_monat
      AND monat <= p_monat
    .

*"---{ �ۺ��������ֶ�, ������������
  LOOP AT it_list ASSIGNING <fs_output>.
    IF <fs_output>-ord43 EQ 'C11'.
*      IF <FS_OUTPUT>-ZZ_ASSET1 IS NOT INITIAL.
      CLEAR: ls_anek.
      ls_anek-bukrs = <fs_output>-bukrs.
*        LS_ANEK-ANLN1 = <FS_OUTPUT>-ZZ_ASSET1.
*        ls_anek-anln2 = <fs_output>-anln2.
*        ls_anek-gjahr =
*        ls_anek-belnr =
      APPEND ls_anek TO lt_anek_shzh.

      CLEAR: ls_anlc.
      ls_anlc-bukrs = <fs_output>-bukrs.
*        LS_ANLC-ANLN1 = <FS_OUTPUT>-ZZ_ASSET1.
      APPEND ls_anlc TO lt_anlc_shzh_temp.

*      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lines( lt_anek_shzh ) > 0.
    SELECT
      anep~bukrs
      anep~anln1
      anep~anln2
      anep~gjahr
      anep~lnran
      afabe
      zujhr
      zucod
      anek~monat
      anbtr
      bwasl
      lnsan
      INTO CORRESPONDING FIELDS OF TABLE lt_anep_shzh
      FROM anep
        INNER JOIN anek  ON anep~bukrs = anek~bukrs
                        AND anep~anln1 = anek~anln1
                        AND anep~anln2 = anek~anln2
                        AND anep~gjahr = anek~gjahr
                        AND anep~lnran = anek~lnran
        FOR ALL ENTRIES IN lt_anek_shzh
      WHERE anep~bukrs = lt_anek_shzh-bukrs
        AND anep~anln1 = lt_anek_shzh-anln1
        AND anep~afabe = p_afabe
*        and anln2 = lt_anek_shzh-anln2
*        and gjahr = lt_anek_shzh-gjahr
*        and belnr = lt_anek_shzh-belnr
      .
  ENDIF.

  IF lines( lt_anlc_shzh_temp ) > 0.
    SELECT *
      INTO TABLE lt_anlc_shzh
      FROM anlc
        FOR ALL ENTRIES IN lt_anlc_shzh_temp
      WHERE bukrs = lt_anlc_shzh_temp-bukrs
        AND anln1 = lt_anlc_shzh_temp-anln1
        AND afabe = p_afabe.
  ENDIF.

*"---} �ۺ��������ֶ�, ������������

  IF lines( lt_anek ) > 0.
    SELECT
      anep~bukrs
      anep~anln1
      anep~anln2
      anep~gjahr
      anep~lnran
      afabe
      zujhr
      zucod
      anek~monat
      anek~belnr
      anek~tcode
      anbtr
      anep~bwasl
*      shkzg
      lnsan
      rbkp~stblg
      INTO CORRESPONDING FIELDS OF TABLE lt_anep
      FROM anep
        INNER JOIN anek  ON anep~bukrs = anek~bukrs
                        AND anep~anln1 = anek~anln1
                        AND anep~anln2 = anek~anln2
                        AND anep~gjahr = anek~gjahr
                        AND anep~lnran = anek~lnran
        LEFT OUTER JOIN rbkp  ON anek~bukrs = rbkp~bukrs
                             AND anek~gjahr = rbkp~gjahr
                             AND anek~belnr = rbkp~belnr
*        INNER JOIN ZTCOFCOFI_0011 ON ZTCOFCOFI_0011~BWASL = ANEP~BWASL
        FOR ALL ENTRIES IN lt_anek
      WHERE anep~bukrs = lt_anek-bukrs
*        and bzdat in gt_bzdat_range
*        AND ZTCOFCOFI_0011~BWAGRP = '10'
        AND anep~anln1 = lt_anek-anln1
        AND anep~anln2 = lt_anek-anln2
        AND anep~gjahr = lt_anek-gjahr
        AND anep~belnr = lt_anek-belnr
        AND anep~afabe = p_afabe
      .

*�ų��ɹ���Ʊ������(2018-01-30)
* ֻ��ʾBKRP-STBLGΪ�յ�����Ŀ
*����
*ANEK-BUKRS=BKRP-BUKRS����˾���룩,
*ANEK-GJAHR=BKRP-GJAHR�������ȣ�,
*ANEK-BELNR=BKRP-BELNR���ο���,
*ANEK-TCODE = MIRO or MR8M
    LOOP AT lt_anep INTO ls_anep.
      IF ls_anep-tcode EQ 'MIRO' OR ls_anep-tcode EQ 'MR8M' OR ls_anep-tcode EQ 'MIR7'.
        IF ls_anep-stblg IS NOT INITIAL.  " ������
          DELETE lt_anep.
        ENDIF.
      ENDIF.
    ENDLOOP.


*" ANLP
*    select *
*      into table lt_anlp
*      from anlp
*        FOR ALL ENTRIES IN lt_anek
*      where bukrs = lt_anek-bukrs
*        and gjahr = lt_anek-gjahr
*        and
*    " ���ڱ��ڼ�ֵ׼��
*    select *
*      into table lt_anlp
*      from anlp
*        for all entries in lt_anek
*      where anlp~bukrs = lt_anek-bukrs
*        and anlp~gjahr = lt_anek-gjahr
*        and peraf <= p_monat
*        and anlp~anln1 = lt_anek-anln1
**        and anln2 = lt_anek-anln2
**        and belnr = lt_anek-belnr
**        and afaber = '01'
*      .

*" ANEA
*    select *
*      into table @lt_anea
*      from anea
*        for all entries in @lt_anek
*      where anea~bukrs = @lt_anek-bukrs
*        and anea~gjahr = @lt_anek-gjahr
*        and anea~anln1 = @lt_anek-anln1
*        and anea~anln2 = @lt_anek-anln2
*        and anea~lnran = @lt_anek-lnran.

  ENDIF.



*" get data from ANEA
*  SELECT
**" ANEA
*    ANEA~BUKRS,
*    ANEA~ANLN1,
*    ANEA~ANLN2,
*    ANEA~GJAHR,
*    ANEA~LNRAN,
*    ANEA~AFABE,
*    ANEA~ZUJHR,
*    ANEA~ZUCOD,
*    ANEA~NAFAV,
*    ANEA~NAFAL,
**" ANEK
*    ANEK~MONAT
*    INTO CORRESPONDING FIELDS OF TABLE @LT_ANEA
*    FROM ANEA
*      INNER JOIN ANEK ON  ANEA~BUKRS = ANEK~BUKRS
*                      AND ANEA~GJAHR = ANEK~GJAHR
*                      AND ANEA~ANLN1 = ANEK~ANLN1
*                      AND ANEA~ANLN2 = ANEK~ANLN2
*                      AND ANEA~LNRAN = ANEK~LNRAN
*    WHERE ANEA~BUKRS IN @S_BUKRS
*      AND ANEA~GJAHR = @P_GJAHR
*      AND ANEA~ANLN1 IN @S_ANLN1
**      AND anek~bukrs IN @s_bukrs
*      AND ANEA~AFABE = @P_AFABE
*      AND ANEK~MONAT <= @P_MONAT.

  "����ϵͳ���ڴ������������SELECT����滻Ϊ������� by 2018-12-17��sw
  REFRESH: lt_anea_ad,lt_anea.
  SELECT
*" ANEA
    bukrs,
    anln1,
    anln2,
    gjahr,
    lnran,
    afabe,
    zujhr,
    zucod,
    nafav,
    nafal
    FROM anea INTO CORRESPONDING FIELDS OF TABLE @lt_anea_ad
    FOR ALL ENTRIES IN @lt_anek
    WHERE bukrs = @lt_anek-bukrs
      AND gjahr = @lt_anek-gjahr
      AND anln1 = @lt_anek-anln1
      AND anln2 = @lt_anek-anln2
      AND lnran = @lt_anek-lnran.
  DELETE lt_anea_ad WHERE afabe NE p_afabe.
  CLEAR ls_anea.
  LOOP AT lt_anea_ad INTO ls_anea.
    READ TABLE lt_anek INTO ls_anek WITH KEY bukrs = ls_anea-bukrs gjahr = ls_anea-gjahr anln1 = ls_anea-anln1 anln2 = ls_anea-anln2 lnran = ls_anea-lnran .
    IF sy-subrc = 0.
      ls_anea-monat = ls_anek-monat.
      APPEND ls_anea TO lt_anea.
    ENDIF.
    CLEAR: ls_anea,ls_anek.
  ENDLOOP.

  REFRESH lt_anea_ad.


















*" ANLP
  SELECT *
    INTO TABLE @lt_anlp
    FROM anlp
    WHERE anlp~bukrs  IN @s_bukrs
      AND anlp~gjahr  = @p_gjahr
      AND anlp~afaber = @p_afabe
      AND peraf       <= @p_monat
      AND anln1       IN @s_anln1.

*" ���ڼ����ۼ��۾��ڳ�                                       " @change_0005
  SELECT *                                                    " @change_0005
    INTO TABLE lt_anlc_ljzj_qc                                " @change_0005
    FROM anlc                                                 " @change_0005
*      for all entries in lt_anek                              " @change_0005
    WHERE bukrs IN s_bukrs                                    " @change_0005
      AND gjahr = p_gjahr                                     " @change_0005
*        and peraf = p_monat                                     " @change_0005
      AND anln1 IN s_anln1                                    " @change_0005
      AND afabe = p_afabe
*      and anln2 = lt_anek-anln2
    .                                                         " @change_0005
  IF sy-subrc = 0.                                            " @change_0005
    LOOP AT lt_anlc_ljzj_qc INTO ls_anlc.                     " @change_0005
      ls_anlc_collect-bukrs = ls_anlc-bukrs.                  " @change_0005
      ls_anlc_collect-gjahr = ls_anlc-gjahr.                  " @change_0005
      ls_anlc_collect-anln1 = ls_anlc-anln1.                  " @change_0005
      ls_anlc_collect-anln2 = ls_anlc-anln2.                  " @change_0005
      ls_anlc_collect-knafa = ls_anlc-knafa.                  " @change_0005
      ls_anlc_collect-kaafa = ls_anlc-kaafa.                  " @change_0005
      ls_anlc_collect-kaufw = ls_anlc-kaufw.                  " @change_0005
      COLLECT ls_anlc_collect INTO lt_anlc_ljzj_qc_collect.   " @change_0005
      CLEAR: ls_anlc_collect.                                 " @change_0005
    ENDLOOP.                                                  " @change_0005
  ENDIF.                                                      " @change_0005

*" �����ʶ����
  LOOP AT lt_anep ASSIGNING <fs_anep>.
    ls_tabw-bwasl = <fs_anep>-bwasl.
    APPEND ls_tabw TO lt_tabw_temp.
  ENDLOOP.

  DELETE lt_tabw_temp WHERE bwasl = ''.
  SORT lt_tabw_temp ASCENDING BY bwasl.
  DELETE ADJACENT DUPLICATES FROM lt_tabw_temp.
  IF lines( lt_tabw_temp ) > 0.
    SELECT
      bwasl
      anshkz
      INTO TABLE lt_tabw
      FROM tabw
        FOR ALL ENTRIES IN lt_tabw_temp
      WHERE bwasl = lt_tabw_temp-bwasl.
  ENDIF.

  SORT lt_tabw ASCENDING BY bwasl.

*---{ *" �����ʶ���� & �������� & ���ڼ���
  LOOP AT lt_anep INTO ls_anep.
    MOVE-CORRESPONDING ls_anep TO ls_anep_collect.
*" ���ñ������������ݲŻ���㣬�������򲻼���
    READ TABLE lt_ztfi_002  WITH KEY bwagrp = '10'
                                     bwasl  = ls_anep-bwasl
                                     BINARY SEARCH.
    IF sy-subrc = 0.
      IF ls_anep-lnsan IS INITIAL.
        READ TABLE lt_tabw INTO ls_tabw WITH KEY bwasl = ls_anep-bwasl BINARY SEARCH.
        IF sy-subrc = 0.
          ls_anep-shkzg = ls_tabw-anshkz. " �����ʶ
          IF ls_anep-shkzg EQ 'S'.
            ls_anep_collect-bqzj = ls_anep-anbtr.
          ELSEIF ls_anep-shkzg EQ 'H'.
            ls_anep_collect-bqjs = ls_anep-anbtr * -1.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

**" ���ڼ�ֵ׼��
**" ��Ϊ ȡANLP������
*    if ls_anep-lnsan is initial.
*      if ls_anep-bwasl eq 'Z01'
**      or
**        ls_anep-bwasl eq '891' or
**        ls_anep-bwasl eq '892' or
**        ls_anep-bwasl eq '893' or
**        ls_anep-bwasl eq '897'
*        .
*
*        ls_anep_collect-jzzb_bq = ls_anep-anbtr.
*      endif.
*    endif.

    COLLECT ls_anep_collect INTO lt_anep_collect.
    CLEAR ls_anep_collect.
  ENDLOOP.

**" ���ڼ�ֵ׼��,
  LOOP AT lt_anlp INTO ls_anlp.
    ls_anlp_collect-bukrs = ls_anlp-bukrs.
    ls_anlp_collect-gjahr = ls_anlp-gjahr.
    ls_anlp_collect-peraf = ls_anlp-peraf. " �۾ɼ�����
    ls_anlp_collect-anln1 = ls_anlp-anln1.
    ls_anlp_collect-aufwz = ls_anlp-aufwz. " �������ع�, ���� -1 ��Ϊ��ֵ׼��
    ls_anlp_collect-nafag = ls_anlp-nafag. " @change_0005
    ls_anlp_collect-aafag = ls_anlp-aafag. " @change_0005
    ls_anlp_collect-aufwb = ls_anlp-aufwb. " @change_0005
    COLLECT ls_anlp_collect INTO lt_anlp_collect.
    CLEAR: ls_anlp_collect.
  ENDLOOP.

*"---{ �ۺ��������ֶ�, ������������
  LOOP AT lt_anep_shzh INTO ls_anep.
    CLEAR: ls_anep_collect.
    IF ls_anep-lnsan IS INITIAL.
      IF NOT (  ls_anep-bwasl EQ 'Z01' OR
                ls_anep-bwasl EQ '891' OR
                ls_anep-bwasl EQ '892' OR
                ls_anep-bwasl EQ '893' OR
                ls_anep-bwasl EQ '897' OR
                ls_anep-bwasl EQ '640' OR
                ls_anep-bwasl EQ '650' OR
                ls_anep-bwasl EQ 'Z64' OR
                ls_anep-bwasl EQ 'Z65' OR
                ls_anep-bwasl EQ '200' OR
                ls_anep-bwasl EQ '250' ).
        ls_anep_collect-bukrs    = ls_anep-bukrs.
        ls_anep_collect-gjahr    = ls_anep-gjahr.
        ls_anep_collect-monat    = ls_anep-monat.
        ls_anep_collect-anln1    = ls_anep-anln1.
        ls_anep_collect-shzhq_yz = ls_anep-anbtr. " �ۺ����ǰԭֵ
      ENDIF.

      IF     (  ls_anep-bwasl EQ 'Z01' OR
                ls_anep-bwasl EQ '891' OR
                ls_anep-bwasl EQ '892' OR
                ls_anep-bwasl EQ '893' OR
                ls_anep-bwasl EQ '897' ).
        ls_anep_collect-bukrs    = ls_anep-bukrs.
        ls_anep_collect-gjahr    = ls_anep-gjahr.
        ls_anep_collect-monat    = ls_anep-monat.
        ls_anep_collect-anln1    = ls_anep-anln1.
        ls_anep_collect-shzhq_jzzb = ls_anep-anbtr. " �ۺ����ǰ��ֵ׼��
      ENDIF.

      COLLECT ls_anep_collect INTO lt_anep_shzh_collect.

    ENDIF.
  ENDLOOP.
  DELETE lt_anep_shzh_collect WHERE table_line IS INITIAL.
*"---} �ۺ��������ֶ�, ������������


*" �ı��ֶ�
  LOOP AT it_list ASSIGNING <fs_output>.
    ls_t001w-werks  = <fs_output>-werks.
    APPEND ls_t001w TO lt_t001w_temp.

    ls_t499s-werks  = <fs_output>-werks.
    ls_t499s-stand  = <fs_output>-stort.
    APPEND ls_t499s TO lt_t499s_temp.

  ENDLOOP.

  SORT lt_t001w_temp ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_t001w_temp.
  IF lines( lt_t001w_temp ) > 0.
    SELECT
      werks
      name1
      INTO TABLE lt_t001w
      FROM t001w
        FOR ALL ENTRIES IN lt_t001w_temp
      WHERE werks = lt_t001w_temp-werks.
  ENDIF.

  SORT lt_t499s_temp ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_t499s_temp.
  IF lines( lt_t499s_temp ) > 0.
    SELECT *
      INTO TABLE lt_t499s
      FROM t499s
        FOR ALL ENTRIES IN lt_t499s_temp
      WHERE werks = lt_t499s_temp-werks
        AND stand = lt_t499s_temp-stand.
  ENDIF.

*" sort sort sort
  SORT lt_anep_collect      ASCENDING BY bukrs anln1 gjahr monat.
  SORT lt_anep_shzh_collect ASCENDING BY bukrs anln1 gjahr monat.
  SORT lt_anlc_shzh         BY bukrs ASCENDING
                               anln1 ASCENDING
                               gjahr DESCENDING.

  SORT lt_anlc_ljzj_qc_collect  ASCENDING BY bukrs anln1 gjahr .
  SORT lt_anlp_collect ASCENDING BY bukrs gjahr peraf anln1.

  SORT lt_t001w ASCENDING BY werks.
  SORT lt_t499s ASCENDING BY werks stand.

  SORT lt_anlb ASCENDING BY bukrs anln1.

  LOOP AT it_list ASSIGNING <fs_output>.
    READ TABLE lt_anep_collect INTO ls_anep_collect WITH KEY bukrs = <fs_output>-bukrs
                                                             anln1 = <fs_output>-anln1
                                                             gjahr = p_gjahr
                                                             monat = p_monat
                                                             BINARY SEARCH.
*" ��������&���ڼ���
    IF sy-subrc = 0.
      <fs_output>-bqzj    = ls_anep_collect-bqzj.
      <fs_output>-bqjs    = ls_anep_collect-bqjs.
*      <fs_output>-jzzb_bq = ls_anep_collect-jzzb_bq.
    ENDIF.

*" ���ڼ�ֵ׼�� ( �������ع� ANLP-AUFWZ * -1 )
    READ TABLE lt_anlp_collect INTO ls_anlp_collect WITH KEY bukrs = <fs_output>-bukrs
                                                             gjahr = p_gjahr
                                                             peraf = p_monat
                                                             anln1 = <fs_output>-anln1
                                                             BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_output>-jzzb_bq = ls_anlp_collect-aufwz * -1.
    ENDIF.

*"---{ for more info, refer to @change_0003

    READ TABLE lt_t001w INTO ls_t001w WITH KEY werks = <fs_output>-werks BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_output>-werks_text = ls_t001w-name1.
    ENDIF.

    READ TABLE lt_t499s INTO ls_t499s WITH KEY werks = <fs_output>-werks
                                               stand = <fs_output>-stort
                                               BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_output>-stort_text = ls_t499s-ktext.
    ENDIF.

*" �ڳ�ԭֵ
    <fs_output>-qcyz  = <fs_output>-zcyz + <fs_output>-bqjs - <fs_output>-bqzj.

*" �ۼ��۾��ڳ�
*    <fs_output>-ljzj_qc = <fs_output>-ljzj_zc - <fs_output>-month_1.                   " @change_0005
    READ TABLE lt_anlc_ljzj_qc_collect INTO ls_anlc WITH KEY bukrs = <fs_output>-bukrs  " @change_0005
                                                             anln1 = <fs_output>-anln1  " @change_0005
                                                             gjahr = p_gjahr            " @change_0005
                                                             BINARY SEARCH.             " @change_0005
    IF sy-subrc = 0.                                                                    " @change_0005
*" �ۼ��۾��ڳ�
      <fs_output>-ljzj_qc = <fs_output>-ljzj_qc + ( ls_anlc-knafa + ls_anlc-kaafa ) * -1.                   " @change_0005
*" ��ֵ׼���ڳ�
      <fs_output>-jzzb_qc = <fs_output>-jzzb_qc + ls_anlc-kaufw * -1.                   " @change_0005
    ENDIF.                                                                              " @change_0005

    READ TABLE lt_anlp_collect INTO ls_anlp WITH KEY bukrs = <fs_output>-bukrs          " @change_0005
                                                     gjahr = p_gjahr                    " @change_0005
                                                     peraf = p_monat                    " @change_0005
                                                     anln1 = <fs_output>-anln1          " @change_0005
                                                     BINARY SEARCH.                     " @change_0005
    IF sy-subrc = 0.                                                                    " @change_0005
*" �ۼ��۾��ڳ�
      <fs_output>-ljzj_qc = <fs_output>-ljzj_qc + ( ls_anlp-nafag + ls_anlp-aafag ) * -1.                   " @change_0005
*" ��ֵ׼���ڳ�
      <fs_output>-jzzb_qc = <fs_output>-jzzb_qc + ls_anlp-aufwb * -1.                   " @change_0005
    ELSE.
*" ����Ļ������ڼ�û�ж�Ӧ��ANLP-PERAFֵ��ȡANLP-PERAF�ж�Ӧ�����ֵ
      CLEAR: lt_anlp_collect_temp.
      LOOP AT lt_anlp_collect INTO ls_anlp WHERE bukrs = <fs_output>-bukrs
                                             AND gjahr = p_gjahr
                                             AND anln1 = <fs_output>-anln1.
        APPEND ls_anlp TO lt_anlp_collect_temp.
      ENDLOOP.

      PERFORM get_max_period_of_anlp USING lt_anlp_collect_temp
                                  CHANGING ls_anlp.
*" �ۼ��۾��ڳ�
      <fs_output>-ljzj_qc = <fs_output>-ljzj_qc + ls_anlp-nafag * -1.                   " @change_0005
*" ��ֵ׼���ڳ�
      <fs_output>-jzzb_qc = <fs_output>-jzzb_qc + ls_anlp-aufwb * -1.                   " @change_0005

    ENDIF.

    " @change_0005

    LOOP AT lt_anea INTO ls_anea  WHERE bukrs = <fs_output>-bukrs
                                    AND anln1 = <fs_output>-anln1
                                    AND gjahr = p_gjahr.
*                                    and monat = p_monat.
      IF ls_anea-monat = p_monat.
*" �ۼ��۾ɵ���
        <fs_output>-byljzj_tz = <fs_output>-byljzj_tz + ( ls_anea-nafav + ls_anea-nafal ) * -1.
      ELSEIF ls_anea-monat < p_monat.
*" �ۼ��۾��ڳ�
        <fs_output>-ljzj_qc = <fs_output>-ljzj_qc + ( ls_anea-nafav + ls_anea-nafal ) * -1.
      ELSE.
        " do nothing...
      ENDIF.
    ENDLOOP.

*" �����۾ɶ�ƻ��⣩
    LOOP AT lt_anlp INTO ls_anlp  WHERE bukrs = <fs_output>-bukrs
                                    AND gjahr = p_gjahr
                                    AND peraf = p_monat
                                    AND anln1 = <fs_output>-anln1.
      <fs_output>-byzj_jhw = <fs_output>-byzj_jhw + ( -1 * ls_anlp-aafaz ).
    ENDLOOP.




*" ������ľ�ֵ��ĩ��������ֵ׼��������ֵ�ڳ���������ֵ׼����<0ʱ��ȡֵΪ0��
*" ������ľ�����ĩ�������ڳ�<0ʱ��ȡֵΪ0��
    IF <fs_output>-zcjz < 0.
      <fs_output>-zcjz = 0.
    ENDIF.

    IF <fs_output>-zcjz_qc < 0.
      <fs_output>-zcjz_qc = 0.
    ENDIF.

    IF <fs_output>-je_qm < 0.
      <fs_output>-je_qm = 0.
    ENDIF.

    IF <fs_output>-je_qc < 0.
      <fs_output>-je_qc = 0.
    ENDIF.

*" ��ĳ�µġ������۾ɶ=0ʱ�����ۻ��۾��ڳ���=���ۻ��۾���ĩ�������ӣ�
    IF <fs_output>-month_1 = 0 AND <fs_output>-byljzj_tz = 0.
      <fs_output>-ljzj_qc = <fs_output>-ljzj_zc.
    ENDIF.

*" ���ʲ���������ANLA-DEAKT ��Ϊ����<��Ļ�������ȡ��ڼ䣬���ۼ��۾��ڳ�=0
    IF <fs_output>-deakt IS INITIAL OR <fs_output>-deakt EQ ''.
      " do nothing...
    ELSE.
      IF <fs_output>-deakt+0(6) < lv_current_period.
        <fs_output>-ljzj_qc = 0.
      ENDIF.
    ENDIF.


*"---{ �ۺ��������ֶ�, ������������
*    if <fs_output>-ord43 eq 'C11'.
*
*    endif.
    READ TABLE lt_anep_shzh_collect INTO ls_anep_collect WITH KEY bukrs = <fs_output>-bukrs
*                                                                  ANLN1 = <FS_OUTPUT>-ZZ_ASSET1
                                                                  gjahr = p_gjahr
                                                                  monat = p_monat
                                                                  BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_output>-shzhq_yz    = ls_anep_collect-shzhq_yz.
      <fs_output>-shzhq_jzzb  = ls_anep_collect-shzhq_jzzb.
    ENDIF.

    READ TABLE lt_anlc_shzh INTO ls_anlc WITH KEY bukrs = <fs_output>-bukrs
*                                                  ANLN1 = <FS_OUTPUT>-ZZ_ASSET1
                                                  BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_output>-shzhq_ljzj = ls_anlc-nafav +
                               ls_anlc-safav +
                               ls_anlc-aafav +
                               ls_anlc-nafal +
                               ls_anlc-safal +
                               ls_anlc-aafal.
    ENDIF.

*" �ۺ����ǰ���� = �ۺ����ǰԭֵ - �ۺ����ǰ�ۼ��۾� + �ۺ����ǰ��ֵ׼��
    <fs_output>-shzhq_je      = <fs_output>-shzhq_yz - <fs_output>-shzhq_ljzj + <fs_output>-shzhq_jzzb.

*"---} �ۺ��������ֶ�, ������������
*"---} refer to @change_0003

* ���ڱ�������2017.10.18������ʲ��ġ��ۼ��۾��ڳ����͡���ֵ׼���ڳ���������0
    IF <fs_output>-deakt IS INITIAL OR <fs_output>-deakt EQ ''.
      " do nothing...
    ELSE.
      IF <fs_output>-deakt IS NOT INITIAL AND
           lv_current_period > <fs_output>-deakt+0(6).
        <fs_output>-ljzj_qc = 0.
        <fs_output>-jzzb_qc = 0.
      ENDIF.
    ENDIF.

*" anlh
    SELECT SINGLE
      anlhtxt
      INTO <fs_output>-anlhtxt
      FROM anlh
      WHERE bukrs = <fs_output>-bukrs
        AND anln1 = <fs_output>-anln1.

*" ANLB
    READ TABLE lt_anlb INTO ls_anlb WITH KEY bukrs = <fs_output>-bukrs
                                             anln1 = <fs_output>-anln1
                                             BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_output>-umjar = ls_anlb-umjar.
      <fs_output>-umper = ls_anlb-umper.
      <fs_output>-ndurj = ls_anlb-ndurj.
      <fs_output>-ndurp = ls_anlb-ndurp.
    ENDIF.


**" �ڳ���ֵ׼��
*" ��ֵ׼���ڳ�                                                                         " @change_0005
*    <fs_output>-jzzb_qc = <fs_output>-jzzb - <fs_output>-jzzb_bq.                      " @change_0005

*" ��ֵ���
*    <fs_output>-zcjz_ce = <fs_output>-bqzj - <fs_output>-bqjs - <fs_output>-month_1.
    <fs_output>-zcjz_ce = <fs_output>-bqzj -
                          <fs_output>-bqjs -
                          <fs_output>-month_1 -
                          <fs_output>-byzj_jhw.

*" ��ֵ��ĩ��������ֵ׼����[ZCJZ] = ԭֵ��ĩ - �ۼ��۾���ĩ - �ۼ��۾ɣ�δ�ƻ���
*    <fs_output>-zcjz    = <fs_output>-zcjz_qc + <fs_output>-zcjz_ce - <fs_output>-ljzj_jhw.
*    <fs_output>-zcjz    = <fs_output>-zcyz + <fs_output>-ljzj_jhw - <fs_output>-ljzj_zc.
    <fs_output>-zcjz    = <fs_output>-zcyz - <fs_output>-ljzj_zc.

*--->>>
*" ��ֵ�ڳ���������ֵ׼����[ZCJZ_QC] = ԭֵ�ڳ� - �ۼ��۾��ڳ�
    <fs_output>-zcjz_qc = <fs_output>-qcyz - <fs_output>-ljzj_qc.

*--->>>
*" �����ڳ� = ��ֵ�ڳ���������ֵ׼����- �ڳ���ֵ׼��/��ֵ׼���ڳ�
    <fs_output>-je_qc = <fs_output>-zcjz_qc - <fs_output>-jzzb_qc.

*" ������ĩ = ��ֵ��ĩ��������ֵ׼����- ��ĩ��ֵ׼��
    <fs_output>-je_qm = <fs_output>-zcjz - <fs_output>-jzzb.

*" ������ = ���ھ�ֵ��� - ���ڼ�ֵ׼��
    <fs_output>-je_ce = <fs_output>-zcjz_ce - <fs_output>-jzzb_bq.


  ENDLOOP.
*---} *" �����ʶ���� & �������� & ���ڼ���

ENDFORM.

*---------------------------------------------------------------------*
* Description: the purpose of this subroutine
*---------------------------------------------------------------------*
FORM get_max_period_of_anlp USING VALUE(it_anlp_collect) TYPE aa_t_anlp
                         CHANGING cs_anlp TYPE anlp.

  CLEAR: cs_anlp.
  SORT it_anlp_collect DESCENDING BY peraf.
  READ TABLE it_anlp_collect INTO cs_anlp INDEX 1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  frm_alv_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_alv_output .
  PERFORM initialization_alv.
  PERFORM initialization_layout.
  PERFORM list_layout.
ENDFORM. " ALV_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  initialization_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM initialization_layout .
  i_layout-zebra = 'X'.
  i_layout-sel_mode   = 'A' .
  i_layout-cwidth_opt = 'X'.
  i_layout-detailinit = 'X' .
  i_layout-detailtitl = '��ϸ����' .
  i_layout-stylefname = 'FIELD_STYLE'.
  i_layout-info_fname = 'LINE_COLOR'.   "��Ԫ����ɫ����
ENDFORM. " INITIALIZATION_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  show_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM initialization_alv .
  get_fieldcat:      'ANLN1'         '�ʲ�����' '12' 'ANLN1' 'ANLA' 'X',
                    'TXT50'         '�ʲ�����'  '40' '' '' 'X',
                    'ANLHTXT'       '�ʲ������ı�' '40' '' '' 'X',
                    'BUKRS'         '��˾����'  '4' '' '' 'X',
                    'ANLKL'         '�ʲ�����' '6' '' '' 'X',
                    'TXK20'         '�ʲ���������' '20' '' '' 'X',
                    'TXA50'         '����ͺ�' '50' '' '' '',
*                    'SERNR'         '���к�' '18' 'SERNR' 'ANLA' '',
                    'SERNR'         '���к�/��ϵͳ�ʲ����' '18' 'SERNR' 'ANLA' '',
*                    'INVNR'         '�����/��ϵͳ�ʲ����' '25' '' '' '',
                    'INVNR'         '�����' '25' '' '' '',

                    'WERKS'         '����' '25' '' '' '',
                    'WERKS_TEXT'         '��������' '25' '' '' '',
                    'STORT'         '�ʲ��ص�' '25' '' '' '',
                    'STORT_TEXT'         '�ʲ��ص�����' '25' '' '' '',
                    'PS_PSP_PNR2'   'WBS' '25' '' '' '',
                    'POST1'         'WBS����' '25' '' '' '',

                    'MENGE'         '����'  '16' '' '' '',
                    'MEINS'         '������λ'  '5' '' '' '',
                    'INVZU'         '���λ��' '15' '' '' '',
                    'AKTIV'         '�ʱ�������' '10' '' '' '',
                    'DEAKT'         '��������' '10' '' '' '',

*                    'UMJAR'         '�л����' '10' '' '' '',
*                    'UMPER'         '�л��¶�' '10' '' '' '',
                    'NDURJ'         'ԭʼ���' '10' '' '' '',
                    'NDURP'         'ԭʼ�¶�' '10' '' '' '',


                    'IVDAT'         'ԭʼ��������' '10' '' '' '',
                    'KOSTL'         '�ɱ�����'  '10' '' '' '',
                    'LTEXT'         '�ɱ���������'   '40' '' '' '',
                    'KOSTLV'        '���γɱ�����'  '10' '' '' '',
                    'LTEXTV'        '���γɱ���������'   '40' '' '' '',
*                    'PRCTR'         '��������'   '10' '' '' '',
*                    'LTEXTP'        '������������'  '40' '' '' '',
*                    'CAUFN'         '�ڲ�����'   '12' '' '' '',
*                    'KTEXT'         '�ڲ���������'  '40' '' '' '',
                    'RAUMN'         '�����'  '8' '' '' '',
                    'KFZKZ'         'ִ���ƺ�'   '15' '' '' '',
                    'ORD41'         'ʹ��״̬'  '14' '' '' '',
                    'ORD42'         '�ʲ���;'  '2' '' '' '',
                    'ORD43_TEXT'         '���ӷ�ʽ'  '4' '' '' '',
                    'ORD44'         '���ٷ�ʽ' '3' '' '' '',
                    'LIFNR'         '��Ӧ�̱���'   '10' '' '' '',
                    'LIEFE'         '��Ӧ������'   '30' '' '' '',
                    'HERST'         '������'   '30' '' '' '',
*                    'VBUND'         'ó�׻��'  '6' '' '' '',

*                    'FIAMT'           '˰���'  '6' '' '' '',
*                    'EHWNR'           '����֪ͨ���ƺ�'  '6' '' '' '',
*                    'EHWZU'           '����֪ͨ'  '6' '' '' '',
*                    'STADT'           '������'  '6' '' '' '',
*                    'GRUFL'           '��Χ'  '6' '' '' '',
*                    'FLURN'           '���صȼ���ͼ/����'  '6' '' '' '',
*                    'ZZ_ASSET1'         '�ۺ����ԭʼ�ʲ�����'  '6' '' '' '',
*                    'ZZ_ASSET2'         '���������ʲ�����'  '6' '' '' '',

*                    'ZCYZ'          'ԭֵ'    '16' '' '' '',
*                    'BQZJ'          '��������'    '16' '' '' '',
                    'QCYZ'          '�ڳ�ԭֵ'  '16' '' '' '',
                    'BQZJ'          '����ԭֵ����'    '16' '' '' '',
*                    'BQJS'          '���ڼ���'    '16' '' '' '',
*                    'BQJS'          '����ԭֵ���٣�����ֵ׼����'    '16' '' '' '',
                    'BQJS'          '����ԭֵ����'    '16' '' '' '',
                    'ZCYZ'          'ԭֵ��ĩ'    '16' '' '' '',
*                    'JZZB'          '��ֵ׼��'   '16' '' '' '',

                    'JZZB_QC'          '��ֵ׼���ڳ�'   '16' '' '' '',
                    'JZZB_BQ'       '���ڼ�ֵ׼��'   '16' '' '' '',
                    'JZZB'          '��ֵ׼����ĩ'   '16' '' '' '',
*                    'LJZJ_ZC'       '�ۼ��۾ɣ�������'  '16' '' '' '',
                    'LJZJ_QC'       '�ۼ��۾��ڳ�'  '16' '' '' '',
                    'BYLJZJ_TZ'       '�����ۼ��۾ɵ���'  '16' '' '' '',
                    'LJZJ_ZC'       '�ۼ��۾���ĩ'  '16' '' '' '',
                    'LJZJ_JHW'      '�ۼ��۾ɣ�δ�ƻ���'  '16' '' '' '',
*                    'ZCJZ'          '��ֵ'    '16' '' '' '',
                    'ZCJZ'          '��ֵ��ĩ��������ֵ׼����'    '16' '' '' '',
                    'ZCJZ_CE'          '��ֵ���'    '16' '' '' '',
                    'ZCJZ_QC'          '��ֵ�ڳ���������ֵ׼����'    '16' '' '' '',
                    'SCHRW'         '��ֵ'  '16' '' '' '',
                    'ZZNDJA'        '�ƻ�ʹ���ڼ�'  '3' '' '' '',
                    'ZZNDJB'        '��ʹ���ڼ�'  '3' '' '' '',
                    'ZSYSYQJ'       'ʣ��ʹ���ڼ�'  '3' '' '' '',
                    'AFASL'         '�۾���'   '4' '' '' '',
                    'AFATXT'        '�۾�������'   '50' '' '' '',
                    'AFABG'         '�۾ɿ�ʼ����' '10'  '' '' '',
                    'YEAR_1'       '����������۾ɶ�'   '16' '' '' '',
                    'MONTH_2'       '�����۾ɶ�'   '16' '' '' '',
                    'MONTH_1'       '�����۾ɶ�'  '16' '' '' '',
*                    'BYZJ_JHW'       '�����۾ɶ�ƻ��⣩'  '16' '' '' '',
                    'BYZJ_JHW'       '�����۾ɣ�δ�ƻ���'  '16' '' '' '',
                    'ZMONTHJE'      '�����۾ɶ�'  '16' '' '' '',
                    'JE_QM'      '������ĩ'  '16' '' '' '',
                    'JE_CE'      '������'  '16' '' '' '',
                    'JE_QC'      '�����ڳ�'  '16' '' '' ''.

*                    'SHZHQ_YZ  '      '�ۺ����ǰԭֵ'  '16' '' '' '',
*                    'SHZHQ_LJZJ'      '�ۺ����ǰ�ۼ��۾�'  '16' '' '' '',
*                    'SHZHQ_JZZB'      '�ۺ����ǰ��ֵ׼��'  '16' '' '' '',
*                    'SHZHQ_JE  '      '�ۺ����ǰ����'  '16' '' '' ''.

ENDFORM. "initialization_alv
*&---------------------------------------------------------------------*
*&      Form  list_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM field_catalog USING col_head
      table
      field
      outputlen
      do_sum
      no_out
      just
      key
     reftab.

  DATA: i_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.

  i_fieldcat-reptext_ddic = col_head.
  i_fieldcat-fieldname = field.
  i_fieldcat-tabname = table.
  i_fieldcat-outputlen = outputlen.
  i_fieldcat-just = just.
  i_fieldcat-do_sum = do_sum.
  i_fieldcat-no_out = no_out.
  i_fieldcat-key = key.
  i_fieldcat-ref_tabname = reftab.

  APPEND i_fieldcat TO gt_fieldcat.

ENDFORM. "FIELD_CATALOG

*&---------------------------------------------------------------------*
*&      Form  list_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM list_layout .
  DATA :i_grid_settings TYPE lvc_s_glay .
  i_grid_settings-edt_cll_cb = 'X' .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'F_SET_PF_STATUS'
      i_callback_user_command  = 'F_USER_COMMAND'
      i_grid_settings          = i_grid_settings
      is_layout_lvc            = i_layout
      it_fieldcat_lvc          = i_fieldcat
      i_save                   = 'A'
    TABLES
      t_outtab                 = it_list
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM. "show_list
*&---------------------------------------------------------------------*
*&      Form  f_set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EXTAB    text
*----------------------------------------------------------------------*
FORM f_set_pf_status USING p_extab TYPE slis_t_extab .
  SET PF-STATUS 'STATUS1' .
ENDFORM. " f_set_pf_status


*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM        text
*      -->P_RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM f_user_command USING p_ucomm TYPE sy-ucomm
                    p_rs_selfield TYPE slis_selfield .
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.
  CALL METHOD lr_grid->check_changed_data.
  CASE p_ucomm .
    WHEN  'BEAN' OR '&IC1'.  "˫���¼�
      READ TABLE it_list INDEX p_rs_selfield-tabindex .
      IF p_rs_selfield-fieldname = 'ANLN1' .
        PERFORM okcode_referenzbeleg
            USING it_list-bukrs p_rs_selfield-value."G_OUT-BELNR." G_OUT-gjahr.
      ENDIF.
    WHEN '&F03' OR '&F12' OR '&F15'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.

  ENDCASE.
  p_rs_selfield-refresh = 'X'.

ENDFORM. " f_user_command

*----------------------------------------------------------------------*
FORM okcode_referenzbeleg USING r01_bukrs r01_anln1 . "r01_gjahr.
  SET PARAMETER ID 'BUK' FIELD r01_bukrs.
  SET PARAMETER ID 'AN1' FIELD r01_anln1.
*  SET PARAMETER ID 'GJR' FIELD r01_gjahr.
  CALL TRANSACTION 'AW01N'.
ENDFORM. "OKCODE_REFERENZBELEG
*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
*       ����û�����Ĳ�ѯ����
*----------------------------------------------------------------------*
FORM check_data.
  DATA: l_bukrs     LIKE t001-bukrs,
        l_anlkl     LIKE ankb-anlkl,
        l_rcomp     LIKE t880-rcomp,
        l_datec(10) TYPE c,
        l_afaber    LIKE t093-afaber.

* ��鹫˾����
  SELECT SINGLE bukrs INTO l_bukrs
    FROM t001
    WHERE bukrs IN s_bukrs.

  IF sy-subrc NE 0.
*   MESSAGE����˾���벻����
    MESSAGE e000(zyouzfirp_0008) WITH '��˾���벻����'.
  ENDIF.

* ����ʲ�����
  IF NOT s_anlkl[] IS INITIAL.
    SELECT SINGLE anlkl INTO l_anlkl
      FROM ankb
      WHERE anlkl IN s_anlkl
      AND afabe = p_afabe.

    IF sy-subrc NE 0.
*     MESSAGE���ʲ����಻����
      MESSAGE e000(zyouzfirp_0008) WITH '�ʲ����಻����'.
    ENDIF.
  ENDIF.

* ����۾ɷ�Χ
  "  if not s_afabe is initial.
  SELECT bukrs afapl INTO TABLE it_afapl
    FROM t093c
    WHERE bukrs IN s_bukrs.

  IF NOT it_afapl[] IS INITIAL.
    SELECT afaber INTO l_afaber
      FROM t093
      FOR ALL ENTRIES IN it_afapl
      WHERE afapl = it_afapl-afapl
      AND   xstore = 'X'
      AND   afaber = p_afabe."in s_afabe.
    ENDSELECT.
  ENDIF.

  IF sy-subrc NE 0.
*     MESSAGE���۾ɷ�Χ������
    MESSAGE e000(zyouzfirp_0008) WITH '�۾ɷ�Χ������'.
  ENDIF.
  " endif.

* ����ڼ�
  IF p_monat GT 16 OR p_monat LT 1.
*   MESSAGE���ڼ�Ӧ��1��16��Χ��
    MESSAGE e000(zyouzfirp_0008) WITH '�ڼ�Ӧ��1��16��Χ��'.
  ENDIF.

* �����ʲ�����ת�ʵ���Ϣ��
  CLEAR: l_datec, g_firstdate, g_lastdate.
*  concatenate p_gjahr p_monat '01' into l_datec.
  IF p_monat > 12.
    CONCATENATE p_gjahr  '1201' INTO l_datec.
  ELSE.
    CONCATENATE p_gjahr p_monat '01' INTO l_datec.
  ENDIF.
  g_firstdate = l_datec.

  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = g_firstdate
    IMPORTING
      e_date = g_lastdate.

  DATA: BEGIN OF ta_bukrs OCCURS 0.
          INCLUDE STRUCTURE t093c.
  DATA: END OF ta_bukrs.

  REFRESH ta_bukrs.
  CLEAR ta_bukrs.

  SELECT bukrs datum
    INTO CORRESPONDING FIELDS OF TABLE ta_bukrs
    FROM t093c
    WHERE bukrs IN s_bukrs
    AND datum < g_firstdate.
  IF sy-subrc <> 0.
*    MESSAGE e016.
*    EXIT.
  ELSE.
    IF ta_bukrs[] IS INITIAL.
*      MESSAGE e016.
*      EXIT.
    ELSE.
      REFRESH s_bukrs.
      CLEAR s_bukrs.
      LOOP AT ta_bukrs.
        s_bukrs-sign = 'I'.
        s_bukrs-option = 'EQ'.
        s_bukrs-low = ta_bukrs-bukrs.
        APPEND s_bukrs.
      ENDLOOP.
      REFRESH ta_bukrs.
      CLEAR ta_bukrs.
    ENDIF.
  ENDIF.
ENDFORM. "check_data
*&---------------------------------------------------------------------*
*&      Form  frm_amt_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LIST_KFZKZ  text
*      <--P_L_FLAG  text
*----------------------------------------------------------------------*
FORM frm_amt_check USING p_ta_pack_item_wrbtr
                    CHANGING p_l_flag.
  DATA:l_fieldvalue(30),
       l_type LIKE dd01v-datatype.
  l_fieldvalue = p_ta_pack_item_wrbtr.
  DO.
    SEARCH l_fieldvalue FOR ','.
    IF sy-subrc EQ 0.
      REPLACE ',' WITH '' INTO l_fieldvalue.
      CONDENSE l_fieldvalue NO-GAPS.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  SEARCH l_fieldvalue FOR '.'.
  REPLACE '.' WITH '' INTO l_fieldvalue.
  CONDENSE l_fieldvalue NO-GAPS.

  SEARCH l_fieldvalue FOR '-'.
  REPLACE '-' WITH '' INTO l_fieldvalue.
  CONDENSE l_fieldvalue NO-GAPS.
  CALL FUNCTION 'NUMERIC_CHECK'
    EXPORTING
      string_in = l_fieldvalue
    IMPORTING
      htype     = l_type.
  IF l_type NE 'NUMC'.
    p_l_flag = 'F'.
  ENDIF.
ENDFORM. " frm_amt_check

*&---------------------------------------------------------------------*
*&      Form  auth_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM auth_check .
  DATA: BEGIN OF tab_bukrs OCCURS 0,
          bukrs LIKE t001-bukrs,
        END OF tab_bukrs.
  DATA: l_message TYPE string.
*��鹫˾����

  IF NOT s_bukrs IS INITIAL .
    SELECT bukrs
      INTO TABLE tab_bukrs
        FROM t001            "  TVKOT
          WHERE
            bukrs IN s_bukrs.
    IF tab_bukrs[] IS NOT INITIAL.
      LOOP AT tab_bukrs.
        AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
                 ID 'BUKRS' FIELD tab_bukrs-bukrs
                 ID 'ACTVT' FIELD '03'.
        IF sy-subrc <> 0.
          MESSAGE e000(zyouzfirp_0008) WITH '�޹�˾����' tab_bukrs-bukrs 'Ȩ��'.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM. " AUTH_CHECK

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
