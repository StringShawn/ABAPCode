**************************************************
*��������:�������̯
*��������: 2019-11-20
*������:XXX
*������:XXX
*��������:
*============================================
*�����¼
*�޸������    �޸�����    �޸���   �޸�����
*DEVK912027    2019-11-20   HANDYXH    ��������
***************************************************

REPORT zshxjfi0013 MESSAGE-ID zfishxj01.

*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES:mseg,mkpf.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES: BEGIN OF ty_alv,
         bukrs   TYPE bseg-bukrs,  " ��˾����
         werks   TYPE bseg-werks,  " ����
         gjahr   TYPE bseg-gjahr,  " ���
         monat   TYPE bkpf-monat,  " �ڼ�
         matnr   TYPE bseg-matnr,  " ����
         maktx   TYPE makt-maktx,  " ��������
         meins   TYPE bseg-meins,  " ����������λ
         vprsv   TYPE mbew-vprsv,  " �۸����
         zqccy   TYPE bseg-dmbtr,  " �ڳ�����
         zqcjz   TYPE bseg-dmbtr,  " �ڳ�����ֵ
         zqcsl   TYPE mseg-menge,  " �ڳ��������
         zbqsh   TYPE mseg-menge,  " �����ջ�����
         zbqxh   TYPE mseg-menge,  " ������������
         zbqshcy TYPE bseg-dmbtr,  " ���ڲɹ��ջ�����
         zbqfpcy TYPE bseg-dmbtr,  " ���ڷ�ƱУ�����
         zbqggcy TYPE bseg-dmbtr,  " ���ڼ۸���Ĳ���
         zbqzcy  TYPE bseg-dmbtr,  " �����ܲ���
         zxhxs   TYPE bseg-dmbtr,  " ���ķ�̯����-����
         zxhll   TYPE bseg-dmbtr,  " ���ķ�̯��������-����
         zxhyf   TYPE bseg-dmbtr,  " ���ķ�̯����-�з�����
         zzftcy  TYPE bseg-dmbtr,  " �ܷ�̯����
         zqmlccy TYPE bseg-dmbtr,  " ��ĩ����������
         zqmsl   TYPE mseg-menge,  " ��ĩ�������
         zqmjz   TYPE bseg-dmbtr,  " ��ĩ����ֵ
         zxssl   TYPE mseg-menge,  " ��������
         zlysl   TYPE mseg-menge,  " ��������
         zschysl TYPE mseg-menge,  " ������������
       END OF ty_alv.

*-----------------------------------------------------------------------
* D A T A S
*-----------------------------------------------------------------------
DATA:gt_alv  TYPE TABLE OF ty_alv.

*-----------------------------------------------------------------------
* ALV D A T A S
*-----------------------------------------------------------------------
DATA:gt_fcat TYPE lvc_t_fcat,
     gw_fcat TYPE lvc_s_fcat,
     gw_layo TYPE lvc_s_layo.

*-----------------------------------------------------------------------
* R A N G E S
*-----------------------------------------------------------------------
RANGES:r_budat FOR bkpf-budat,
       r_blart FOR bkpf-blart,
       r_hkont FOR bseg-hkont,
       r_bwart FOR mseg-bwart.
*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS:c_default_bukrs TYPE t001-bukrs VALUE '6100',
          c_default_werks TYPE t001w-werks VALUE '6110',
          c_shkzg_h       TYPE bseg-shkzg VALUE 'H'.

*--------------------------------------------------------------------*
*����ѡ����Ļ����
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.

PARAMETERS:p_bukrs TYPE t001-bukrs  OBLIGATORY DEFAULT c_default_bukrs,
           p_werks TYPE t001w-werks OBLIGATORY DEFAULT c_default_werks,
           p_mjahr TYPE mkpf-mjahr  OBLIGATORY,
           p_monat TYPE bkpf-monat  OBLIGATORY.

SELECT-OPTIONS:s_matnr FOR mseg-matnr.

SELECTION-SCREEN END OF BLOCK blk1.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM build_condition.
  PERFORM get_data.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       ��������ȡ��
*----------------------------------------------------------------------*
FORM get_data .

  CLEAR:gt_alv.

  "ȡ���ƾ֤
  SELECT bukrs,
         gjahr,
         belnr,
         monat,
         blart
  INTO TABLE @DATA(lt_bkpf)
  FROM bkpf
  WHERE bukrs = @p_bukrs
    AND ( ( gjahr = @p_mjahr AND monat <= @p_monat ) OR gjahr < @p_mjahr )
    AND blart IN @r_blart.
  SORT lt_bkpf BY bukrs gjahr belnr.

  IF lt_bkpf IS NOT INITIAL.
    SELECT bukrs,
           gjahr,
           belnr,
           werks,
           matnr,
           shkzg,
           dmbtr,
           meins
    INTO TABLE @DATA(lt_bseg)
    FROM bseg FOR ALL ENTRIES IN @lt_bkpf
    WHERE bukrs = @lt_bkpf-bukrs
      AND gjahr = @lt_bkpf-gjahr
      AND belnr = @lt_bkpf-belnr
      AND matnr IN @s_matnr
      AND hkont IN @r_hkont
      AND werks = @p_werks.

    SORT lt_bseg BY bukrs werks matnr.


    LOOP AT lt_bseg INTO DATA(ls_bseg).
      IF ls_bseg-shkzg = c_shkzg_h.
        ls_bseg-dmbtr = - ls_bseg-dmbtr.
      ENDIF.

      READ TABLE gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WITH KEY bukrs = ls_bseg-bukrs
                                                                  werks = ls_bseg-werks
                                                                  matnr = ls_bseg-matnr.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO gt_alv ASSIGNING <fs_alv>.
        <fs_alv>-bukrs = ls_bseg-bukrs.
        <fs_alv>-werks = ls_bseg-werks.
        <fs_alv>-matnr = ls_bseg-matnr.
        <fs_alv>-meins = ls_bseg-meins.
        <fs_alv>-gjahr = p_mjahr.
        <fs_alv>-monat = p_monat.
      ENDIF.

      READ TABLE lt_bkpf INTO DATA(ls_bkpf) WITH KEY bukrs = ls_bseg-bukrs
                                                     gjahr = ls_bseg-gjahr
                                                     belnr = ls_bseg-belnr BINARY SEARCH.
      "�ڳ�������
      IF ls_bkpf-monat < p_monat.
        ADD ls_bseg-dmbtr TO <fs_alv>-zqccy.
      ENDIF.
      "�ڳ�����ֵ
      IF ls_bkpf-blart = 'PR' AND ls_bkpf-monat < p_monat.
        ADD ls_bseg-dmbtr TO <fs_alv>-zqcjz.
      ENDIF.

      "���ڲɹ��ջ�����
      IF ls_bkpf-monat = p_monat AND ( ls_bkpf-blart = 'WA' OR ls_bkpf-blart = 'WE' ).
        ADD ls_bseg-dmbtr TO <fs_alv>-zbqshcy.
      ENDIF.

      "���ڷ�ƱУ�����
      IF ls_bkpf-monat = p_monat AND ls_bkpf-blart = 'RE'.
        ADD ls_bseg-dmbtr TO <fs_alv>-zbqfpcy.
      ENDIF.

      "���ڼ۸�Ķ�����
      IF ls_bkpf-monat = p_monat AND ls_bkpf-blart = 'PR'.
        ADD ls_bseg-dmbtr TO <fs_alv>-zbqggcy.
      ENDIF.

      "��ĩ����ֵ
      IF ls_bkpf-blart = 'PR' AND ls_bkpf-monat <= p_monat.
        ADD ls_bseg-dmbtr TO <fs_alv>-zqmjz.
      ENDIF.
    ENDLOOP.
  ENDIF.


  "ȡ����ƾ֤
  SELECT mkpf~mblnr,
         mkpf~mjahr,
         mkpf~xblnr,
         mkpf~budat,
         mseg~matnr,
         mseg~zeile,
         mseg~dmbtr,
         mseg~shkzg,
         mseg~menge,
         mseg~meins,
         mseg~werks,
         mseg~bwart,
         mseg~kzvbr
  INTO TABLE @DATA(lt_mkpf)
  FROM mkpf INNER JOIN mseg
    ON mkpf~mblnr = mseg~mblnr
   AND mkpf~mjahr = mseg~mjahr
  WHERE budat <= @r_budat-high
    AND werks =  @p_werks
    AND matnr IN @s_matnr.

  DELETE lt_mkpf WHERE matnr IS INITIAL OR kzvbr = 'A'.

  SORT lt_mkpf BY mjahr werks mjahr.

  LOOP AT lt_mkpf INTO DATA(ls_mkpf).

    IF ls_mkpf-shkzg = c_shkzg_h.
      ls_mkpf-dmbtr = - ls_mkpf-dmbtr.
      ls_mkpf-menge = - ls_mkpf-menge.
    ENDIF.

    READ TABLE gt_alv ASSIGNING <fs_alv> WITH KEY werks = ls_mkpf-werks
                                                  matnr = ls_mkpf-matnr.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO gt_alv ASSIGNING <fs_alv>.
      <fs_alv>-bukrs = p_bukrs.
      <fs_alv>-werks = ls_mkpf-werks.
      <fs_alv>-matnr = ls_mkpf-matnr.
      <fs_alv>-meins = ls_mkpf-meins.
      <fs_alv>-gjahr = p_mjahr.
      <fs_alv>-monat = p_monat.
    ENDIF.

    IF ls_mkpf-budat < r_budat-low.
      "�ڳ�����ֵ
      ADD ls_mkpf-dmbtr TO <fs_alv>-zqcjz.
      "�ڳ��������
      ADD ls_mkpf-menge TO <fs_alv>-zqcsl.
    ENDIF.

    IF ls_mkpf-budat <= r_budat-high.
      "��ĩ����ֵ
      ADD ls_mkpf-dmbtr TO <fs_alv>-zqmjz.
      "��ĩ�������
      ADD ls_mkpf-menge TO <fs_alv>-zqmsl.
    ENDIF.

    IF ls_mkpf-budat IN r_budat.
      "������������
      CLEAR r_bwart[].
      r_bwart[] = VALUE #( sign = 'I' option = 'EQ' ( low = '601' )
                                                    ( low = '102' )
                                                    ( low = '201' )
                                                    ( low = '221' )
                                                    ( low = '241' )
                                                    ( low = 'Z05' )
                                                    ( low = 'Z07' )
                                                    ( low = 'Z09' )
                                                    ( low = 'Z11' )
                                                    ( low = 'Z63' ) ).
      IF ls_mkpf-bwart IN r_bwart.
        ADD ls_mkpf-menge TO <fs_alv>-zbqxh.
      ENDIF.

      "�����ջ�����
      CLEAR r_bwart[].
      r_bwart[] = VALUE #( sign = 'I' option = 'EQ' ( low = '101' )
                                                    ( low = '105' )
                                                    ( low = '162' ) ).
      IF ls_mkpf-bwart IN r_bwart.
        ADD ls_mkpf-menge TO <fs_alv>-zbqsh.
      ENDIF.

      "��������
      CLEAR r_bwart[].
      r_bwart[] = VALUE #( sign = 'I' option = 'EQ' ( low = '601' )
                                                    ( low = '602' ) ).
      IF ls_mkpf-bwart IN r_bwart.
        ADD ls_mkpf-menge TO <fs_alv>-zxssl.
      ENDIF.

      "��������
      CLEAR r_bwart[].
      r_bwart[] = VALUE #( sign = 'I' option = 'EQ' ( low = '201' )
                                                    ( low = '202' ) ).
      IF ls_mkpf-bwart IN r_bwart.
        ADD ls_mkpf-menge TO <fs_alv>-zlysl.
      ENDIF.

      "������������
      CLEAR r_bwart[].
      r_bwart[] = VALUE #( sign = 'I' option = 'EQ' ( low = '221' )
                                                    ( low = '222' ) ).
      IF ls_mkpf-bwart IN r_bwart.
        ADD ls_mkpf-menge TO <fs_alv>-zschysl.
      ENDIF.
    ENDIF.
  ENDLOOP.


  "ȡ�۸����
  IF gt_alv IS NOT INITIAL.
    SELECT bwkey,
           matnr,
           vprsv
    INTO TABLE @DATA(lt_mbew)
    FROM mbew FOR ALL ENTRIES IN @gt_alv
    WHERE bwkey = @gt_alv-werks
      AND matnr = @gt_alv-matnr.
    SORT lt_mbew BY bwkey matnr.

    "ȡ��������
    SELECT matnr,
           maktx
    INTO TABLE @DATA(lt_makt)
    FROM makt FOR ALL ENTRIES IN @gt_alv
    WHERE matnr = @gt_alv-matnr.
    SORT lt_makt BY matnr.
  ENDIF.


  SORT gt_alv BY bukrs werks matnr.
  DELETE ADJACENT DUPLICATES FROM gt_alv COMPARING bukrs werks matnr.

  LOOP AT gt_alv ASSIGNING <fs_alv>.
    "�۸����
    READ TABLE lt_mbew INTO DATA(ls_mbew) WITH KEY bwkey = <fs_alv>-werks matnr = <fs_alv>-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_alv>-vprsv = ls_mbew-vprsv.
    ENDIF.

    "��������
    READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = <fs_alv>-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_alv>-maktx = ls_makt-maktx.
    ENDIF.

    "�����ܲ��죺���ڼ۸���Ĳ���+�����ջ�����+���ڷ�ƱУ�����
    <fs_alv>-zbqzcy = <fs_alv>-zbqshcy + <fs_alv>-zbqfpcy + <fs_alv>-zbqggcy.

    "���ķ�̯����-���� = �ڳ�����+�����ܲ���/���ڳ��������+�����ջ�������*��������
    <fs_alv>-zxhxs = <fs_alv>-zqccy + <fs_alv>-zbqshcy / ( <fs_alv>-zqcsl + <fs_alv>-zbqsh ) * <fs_alv>-zxssl.

    "���ķ�̯����-���� = �ڳ�����+�����ܲ���/���ڳ��������+�����ջ�������*��������
    <fs_alv>-zxhll = <fs_alv>-zqccy + <fs_alv>-zbqshcy / ( <fs_alv>-zqcsl + <fs_alv>-zbqsh ) * <fs_alv>-zlysl.

    "���ķ�̯����-�з����� = �ڳ�����+�����ܲ���/���ڳ��������+�����ջ�������*������������
    <fs_alv>-zxhyf = <fs_alv>-zqccy + <fs_alv>-zbqshcy / ( <fs_alv>-zqcsl + <fs_alv>-zbqsh ) * <fs_alv>-zschysl.

    "��ĩ������� = �ܲ���-���ķ�̯���죨���ۣ�-���ķ�̯���죨���ã�-���ķ�̯���죨�з�������
    <fs_alv>-zqmlccy = <fs_alv>-zbqzcy - <fs_alv>-zxhxs - <fs_alv>-zxhll - <fs_alv>-zxhyf.

  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       չʾ����
*----------------------------------------------------------------------*
FORM display_data .
  PERFORM build_layout.
  PERFORM build_fieldcatlog.
  PERFORM display_alv.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       ������ʽ
*----------------------------------------------------------------------*
FORM build_layout .
  gw_layo-zebra        = 'X'.
  gw_layo-cwidth_opt   = 'X' .       " �Զ��Ż��п�,�����ֶ�ֵ��ʾ����
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATLOG
*&---------------------------------------------------------------------*
*       �����ֶ�
*----------------------------------------------------------------------*
FORM build_fieldcatlog .
  CLEAR:gt_fcat.

  PERFORM add_fieldcat USING:'BUKRS' '��˾����' '' '',"��˾����
  'WERKS' '����' '' '',"����
  'GJAHR' '���' '' '',"���
  'MONAT' '�ڼ�' '' '',"�ڼ�
  'MATNR' '����' 'MARA' 'MATNR',"����
  'MAKTX' '��������' '' '',"��������
  'MEINS' '����������λ' '' '',"����������λ
  'VPRSV' '�۸����' '' '',"�۸����
  'ZQCCY' '�ڳ�����' 'BSEG' 'DMBTR',"�ڳ�����
  'ZQCJZ' '�ڳ�����ֵ' 'BSEG' 'DMBTR',"�ڳ�����ֵ
  'ZQCSL' '�ڳ��������' 'MSEG' 'MENGE',"�ڳ��������
  'ZBQSH' '�����ջ�����' 'MSEG' 'MENGE',"�����ջ�����
  'ZBQXH' '������������' 'MSEG' 'MENGE',"������������
  'ZBQSHCY' '���ڲɹ��ջ�����' 'BSEG' 'DMBTR',"���ڲɹ��ջ�����
  'ZBQFQCY' '���ڷ�ƱУ�����' 'BSEG' 'DMBTR',"���ڷ�ƱУ�����
  'ZBQGGCY' '���ڼ۸���Ĳ���' 'BSEG' 'DMBTR',"���ڼ۸���Ĳ���
  'ZBQZCY' '�����ܲ���' 'BSEG' 'DMBTR',"�����ܲ���
  'ZXHXS' '���ķ�̯����-����' 'BSEG' 'DMBTR',"���ķ�̯����-����
  'ZXHLL' '���ķ�̯��������-����' 'BSEG' 'DMBTR',"���ķ�̯��������-����
  'ZXHYF' '���ķ�̯����-�з�����' 'BSEG' 'DMBTR',"���ķ�̯����-�з�����
  'ZZFTCY' '�ܷ�̯����' 'BSEG' 'DMBTR',"�ܷ�̯����
  'ZQMLCCY' '��ĩ����������' 'BSEG' 'DMBTR',"��ĩ����������
  'ZQMSL' '��ĩ�������' 'MSEG' 'MENGE',"��ĩ�������
  'ZQMJZ' '��ĩ����ֵ' 'BSEG' 'DMBTR'."��ĩ����ֵ





ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       չʾalv
*----------------------------------------------------------------------*
FORM display_alv .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
*     i_callback_pf_status_set    = l_status
*     i_callback_user_command     = l_command
*     i_callback_html_top_of_page = l_top_of_page
      is_layout_lvc      = gw_layo
      it_fieldcat_lvc    = gt_fcat
*     it_sort_lvc        = gt_sort
*     it_filter_lvc      = gt_filt
    TABLES
      t_outtab           = gt_alv
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDCAT
*&---------------------------------------------------------------------*
*       �����ֶ�
*----------------------------------------------------------------------*
FORM add_fieldcat  USING    VALUE(p_fieldname)
                            VALUE(p_coltext)
                            VALUE(p_ref_table)
                            VALUE(p_ref_field).
  CLEAR gw_fcat.
  gw_fcat-fieldname = p_fieldname.
  gw_fcat-coltext   = p_coltext.
  gw_fcat-ref_table = p_ref_table.
  gw_fcat-ref_field = p_ref_field.
  APPEND gw_fcat TO gt_fcat.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_CONDITION
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM build_condition .

  CLEAR:r_budat ,r_budat[],r_blart,r_blart[],r_hkont,r_hkont[].

  r_budat-sign = 'I'.
  r_budat-option = 'BT'.
  r_budat-low  = p_mjahr && p_monat && '01'.
  CALL FUNCTION 'HRPAD_GET_LAST_DAY_OF_MONTH'
    EXPORTING
      iv_date     = r_budat-low
    IMPORTING
      ev_last_day = r_budat-high.

  APPEND r_budat.

  r_blart[] = VALUE #(  sign = 'I' option = 'EQ' ( low = 'WA' )
                                                 ( low = 'WE' )
                                                 ( low = 'RE' )
                                                 ( low = 'PR' )
                                                 ( low = 'WL' ) ).

  r_hkont[] = VALUE #( sign = 'I' option = 'EQ' ( low = '1403980000' )
                                                ( low = '1405980000' ) ).

ENDFORM.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
