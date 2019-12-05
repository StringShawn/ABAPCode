**************************************************
*��������:���������ݴ���
*��������: 2019-11-21
*������:XXX
*������:XXX
*��������: Report ZMMRPT_MDM_TO_S4_MATERIAL
*============================================
*�����¼
*�޸������    �޸�����    �޸���   �޸�����
*DEVK912033    2019-11-21   HAND   ƽ��
***************************************************
REPORT ZSHXJMM0002.

*����
TABLES:ztMM_mdm_mat_log.

*�����ڱ�
DATA:gt_log TYPE TABLE OF ZTMM_MDM_mat_log , "WITH HEADER LINE,
     gt_mx  TYPE TABLE OF ZTMM_MDM_mat_tx,
     gt_txt TYPE TABLE OF ZTMM_MDM_mat_txt.

*����ṹ
DATA:GS_log TYPE ZTMM_MDM_mat_log,
     GS_mx  TYPE ZTMM_MDM_mat_tx,
     GS_txt TYPE ZTMM_MDM_mat_txt.

**�ӿڷ�����Ϣ
DATA:g_message TYPE string.
"������
DATA:BEGIN OF ls_tab,
       maktx LIKE makt-maktx.
    INCLUDE STRUCTURE ztMM_srm_mat_log.
DATA:END OF ls_tab.

DATA:lt_tab LIKE TABLE OF ls_tab.
DATA:ls_item TYPE ztMM_srm_mat_log.
DATA:lt_item TYPE TABLE OF ztMM_srm_mat_log.

*&---------------------------------------------------------------------*
*&       ����������          ���� BAPI������ֶδ����ڱ�
*&---------------------------------------------------------------------*
DATA GS_headdata       LIKE bapimathead.      "�������п�����Ϣ�ı�ͷ��
DATA GS_clientdata     LIKE bapi_mara.        "�����ͻ��˲����������
DATA GS_clientdatax    LIKE bapi_marax.       "����BAPI_MARA �ĸ�ѡ��ṹ
DATA GS_plantdata      LIKE bapi_marc.        "���������������������
DATA GS_plantdatax     LIKE bapi_marcx.       "����BAPI_MARC �ĸ�ѡ��ṹ
DATA GS_mard           LIKE bapi_mard.        "�洢λ�ü������������
DATA GS_mardx          LIKE bapi_mardx.       "BAPI_MARD �ĸ�ѡ��ṹ
DATA GS_valuationdata  LIKE bapi_mbew.        "������������
DATA GS_valuationdatax LIKE bapi_mbewx.       "BAPI_MBEW �ĸ�ѡ��ṹ
DATA GS_return         LIKE bapiret2.         "����RETURN
DATA GS_mvke           LIKE bapi_mvke.
DATA GS_mvkex          LIKE bapi_mvkex.
DATA gt_mater          LIKE bapi_makt OCCURS 10 WITH HEADER LINE.                "����MATERIALDESCRIPTION
DATA gt_unit           LIKE bapi_marm OCCURS 10 WITH HEADER LINE.                "����UNITSOFMEASURE
DATA gt_unitx          LIKE bapi_marmx OCCURS 10 WITH HEADER LINE.               "����UNITSOFMEASUREX
DATA gt_mlan           LIKE bapi_mlan OCCURS 10 WITH HEADER LINE.
DATA gt_internationalartnos LIKE bapi_mean OCCURS 0 WITH HEADER LINE .
DATA gt_mltx                LIKE bapi_mltx OCCURS 0 WITH HEADER LINE.
DATA gt_qmat                LIKE bapi1001004_qmat OCCURS 0 WITH HEADER LINE.
DATA gt_return              LIKE bapi_matreturn2 OCCURS 0 WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&       ����������          ���� BAPI������ֶδ����ڱ�
*&---------------------------------------------------------------------*
DATA:GS_service_data  TYPE bapisrv_asmd,
     GS_service_datax TYPE bapisrv_asmdx.

DATA:GS_service_description TYPE bapisrv_asmdt,
     GS_service_long_texts  TYPE bapisrv_text.
DATA:gt_service_description LIKE bapisrv_asmdt OCCURS 10 WITH HEADER LINE,
     gt_service_long_texts  LIKE bapisrv_text OCCURS 10 WITH HEADER LINE.


*&---------------------------------------------------------------------*
*&       ALV
*&---------------------------------------------------------------------*
DATA:
  gw_layout   TYPE lvc_s_layo,
  gv_repid    TYPE repid,
  gt_fieldcat TYPE lvc_t_fcat,
  gw_fieldcat TYPE lvc_s_fcat.

"�ֶ�Ŀ¼�ĺ�
DEFINE mcr_fieldcat.

  CLEAR gw_fieldcat.
  gw_fieldcat-fieldname   = &1.
  gw_fieldcat-scrtext_m   = &2.

  APPEND gw_fieldcat TO gt_fieldcat.

END-OF-DEFINITION.





*ѡ����Ļ
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:s_matnr  FOR ZTMM_MDM_mat_log-matnr,
               s_lbbm   FOR ZTMM_MDM_mat_log-lbbm.

SELECT-OPTIONS: s_hczt FOR ZTMM_MDM_mat_log-hczt NO-EXTENSION  NO INTERVALS ,""  DEFAULT  '',
                s_cjzt FOR ZTMM_MDM_mat_log-cjzt NO-EXTENSION  NO INTERVALS  ,
                s_zerdat FOR ZTMM_MDM_mat_log-zerdat,
                s_zertim FOR ZTMM_MDM_mat_log-zertim.
PARAMETERS: p_dycs TYPE ZTMM_MDM_mat_log-dycs DEFAULT 3.
SELECTION-SCREEN END OF BLOCK b1 .



INITIALIZATION.
  IF s_cjzt[] IS INITIAL.
    s_cjzt-sign = 'I'.
    s_cjzt-option = 'NE'.
    s_cjzt-low  = 'S'.
    APPEND s_cjzt.
  ENDIF.


START-OF-SELECTION.
*��ȡ��Ҫ���µ���������
  PERFORM frm_get_data.
*�������������bapi��
  PERFORM frm_fill_bapi.
***��־�������֮�����ݵ��ýӿڷ��͵�MDM
**  PERFORM frm_send_to_mdm.  ����DELETE BY IBM-CC �ӿڷ�ʽ���Ϊ�첽������Ҫ��MDMD������Ϣ�����ؽӿ�ȡ��
**����չʾ
  IF sy-batch = ''.
    PERFORM frm_display_alv.
  ENDIF.


*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& text  ��ȡ��Ҫ���µ���������
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_data .
*
  REFRESH:gt_log,
          gt_mx,
          gt_txt
          .

**��ȡ��Ҫ���µ���������
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE gt_log
    FROM ZTMM_MDM_mat_log
    WHERE matnr IN s_matnr
      AND lbbm IN s_lbbm
      AND hczt IN s_hczt
      AND cjzt IN s_cjzt
      AND zerdat IN s_zerdat
      AND zertim IN s_zertim
      AND dycs < p_dycs.
  IF gt_log IS INITIAL.
    MESSAGE 'û�����ݣ�' TYPE 'E'.
  ELSE.
    SORT   gt_log BY matnr logid DESCENDING.
    DELETE ADJACENT DUPLICATES FROM gt_log[] COMPARING matnr.
  ENDIF.
*��ȡ��Ӧ����ϸ����
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE gt_mx
    FROM ZTMM_MDM_mat_tx
    FOR ALL ENTRIES IN gt_log
    WHERE matnr = gt_log-matnr.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FRM_FILL_BAPI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_fill_bapi .


  "������

  IF gt_log[] IS NOT INITIAL.
    DATA:lt_log TYPE TABLE OF ZTMM_MDM_mat_log.
    LOOP AT gt_log INTO GS_log WHERE zzt = '2'  OR zzt = '3' OR zzt = '4'.
      APPEND GS_log TO lt_log.
    ENDLOOP.

    IF lt_log[] IS NOT INITIAL.
      SELECT mara~matnr
             makt~maktx
             marc~werks
             mara~meins
             mara~matkl
             mara~mstae
             marc~ekgrp
             marc~bstmi
             marc~bstrf
             marc~sobsl
             marc~plifz
             marc~lgfsb
             mara~etiar
       INTO CORRESPONDING FIELDS OF TABLE lt_tab
        FROM mara
        LEFT JOIN marc ON mara~matnr = marc~matnr
        LEFT JOIN makt ON mara~matnr = makt~matnr AND makt~spras = 1
        FOR ALL ENTRIES IN lt_log
        WHERE mara~matnr = lt_log-matnr .
    ENDIF.



  ENDIF.

**ѭ���ڱ�������������
  LOOP AT gt_log INTO GS_log.
    CLEAR:GS_headdata,
          GS_clientdata,
          GS_clientdatax,
          GS_plantdata,
          GS_plantdatax,
          GS_mard,
          GS_mardx,
          GS_valuationdata,
          GS_valuationdatax,
          GS_return,
          GS_mvke,
          GS_mvkex,
          gt_mater,
          gt_unit,
          gt_unitx,
          gt_mlan,
          gt_internationalartnos,
          gt_mltx,
          gt_qmat,
          gt_return,
          gt_mater.
    CLEAR:GS_service_data,
          GS_service_datax,
          gt_return[],
          gt_service_description[],
          gt_service_long_texts[].

    "��������������
    IF GS_log-zzt = '1' AND GS_log-matkl(2) <> '90'.

      "���̧ͷ����
      PERFORM fill_headdata.                         "���HEADDATA
      "��������ͼ��ϸ����
      PERFORM fill_mx_data.                          "���������ϸ����
      "��������������
      PERFORM run_material_bapi.                         "����BAPI

***������Ϸ���Ϊ07��09��Ĳ���Ʒ���ϣ���Ҫͬ����Ӧ�����������洢���Զ������
*      IF GS_log-matkl(2) = '07' OR GS_log-matkl(2) = '09'.
*
*      ENDIF.
    ELSEIF  GS_log-zzt = '1' AND GS_log-matkl(2) = '90'.
      "��������������

      "������������BAPI�ṹ
      PERFORM fill_service_data.
      "���з��������ݴ���bapi
      PERFORM run_service_bapi.


    ELSEIF     GS_log-zzt = '2' AND GS_log-matkl(2) <> '90'.
      "���̧ͷ����
      PERFORM fill_headdata.                         "���HEADDATA
      "��������ͼ��ϸ����
      PERFORM fill_mx_data.                          "���������ϸ����
      "��������������
      PERFORM run_material_bapi.                         "����BAPI

    ELSEIF     GS_log-zzt = '2' AND GS_log-matkl(2) = '90'.

      "������������BAPI�ṹ
      PERFORM fill_service_data.
      "���з��������ݴ���bapi
      PERFORM run_service_change.

    ELSEIF     GS_log-zzt = '3' AND GS_log-matkl(2) <> '90'.
      PERFORM frm_change_material_mstae USING ''.
    ELSEIF     GS_log-zzt = '3' AND GS_log-matkl(2) = '90'.
      PERFORM frm_change_service_mstae USING ''.
    ELSEIF     GS_log-zzt = '4' AND GS_log-matkl(2) <> '90'.
      PERFORM frm_change_material_mstae USING 'Z1'.
    ELSEIF     GS_log-zzt = '4' AND GS_log-matkl(2) = '90'.
      PERFORM frm_change_service_mstae USING 'Z1'.
    ENDIF.

    GS_log-dycs =  GS_log-dycs + 1.

    MODIFY  gt_log FROM GS_log.
    CLEAR GS_log.

  ENDLOOP.


***������ɺ������־��
  MODIFY ZTMM_MDM_mat_log  FROM TABLE gt_log.
  IF sy-subrc EQ 0.
    COMMIT WORK.
    MESSAGE '��־���³ɹ���' TYPE 'S'.
  ELSE.
    MESSAGE '��־����ʧ�ܣ�' TYPE 'S'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_HEADDATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_headdata .


***���Ϻ�
  CALL FUNCTION 'CONVERSION_EXIT_MATN5_INPUT'
    EXPORTING
      input        = GS_log-matnr
    IMPORTING
      output       = GS_headdata-material
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
*     Implement suitable error handling here
  ENDIF.

  GS_headdata-ind_sector      = 'C'.                   "��ҵ����
  GS_headdata-matl_type       = 'Z001'.                "��������

  GS_headdata-basic_view      = 'X'.                   "����������ͼΪX

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_MX_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_mx_data .
*  READ TABLE gt_mx

***  ��������
  CLEAR:gt_mater[],gt_mater.
  gt_mater-langu = '1'.
  gt_mater-matl_desc = GS_log-maktx.
  APPEND gt_mater.


*  CLEAR:gt_mltx[],gt_mltx.
*  gt_mltx-applobject = GS_log-matnr.
*  gt_mltx-text_name = 'MATERIAL'.
*  gt_mltx-text_id = 'GRUN'.
*  gt_mltx-langu = '1'.
*  gt_mltx-format_col = '01'.
*  gt_mltx-text_line = GS_log-zmaktx.
*  APPEND gt_mltx.


  GS_clientdata-matl_group     = GS_log-matkl.    "������
  GS_clientdata-base_uom       = GS_log-meins.   "����������λ
  GS_clientdata-base_uom_iso   = GS_log-meins.  "����������λ

  GS_clientdatax-matl_group    ='X'.
  GS_clientdatax-base_uom      ='X'.
  GS_clientdatax-base_uom_iso  ='X'.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form RUN_MATERIAL_BAPI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM run_material_bapi .

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      headdata            = GS_headdata
      clientdata          = GS_clientdata
      clientdatax         = GS_clientdatax
    IMPORTING
      return              = gt_return
    TABLES
      materialdescription = gt_mater.
*      materiallongtext    = gt_mltx.


  IF gt_return-type = 'E' .

    GS_log-cjzt = 'E'."ʧ��
    GS_log-message = gt_return-message.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAit = 'X'.


    "������
    IF lt_tab[] IS NOT INITIAL.
      READ TABLE lt_tab INTO ls_tab WITH KEY matnr = GS_log-matnr.
      IF sy-subrc = 0.
        DATA:zlogid TYPE zzmmlogid.
        DATA:zmaktx TYPE zzmmmaktx.
        DATA:ztdxx  TYPE zzmmtdxx.
        DATA : lt_tline TYPE TABLE OF tline WITH HEADER LINE  .
        DATA:lv_name TYPE thead-tdname.
        lv_name = GS_log-matnr.
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = '68'
            object                  = 'ZLOGID'
          IMPORTING
            number                  = zlogid
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.

        "���ı�
        REFRESH lt_tline.
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
*           CLIENT                  = SY-MANDT
            id                      = 'GRUN'
            language                = '1'
            name                    = lv_name
            object                  = 'MATERIAL'
          TABLES
            lines                   = lt_tline
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.
        IF sy-subrc = 0.
          LOOP AT lt_tline.
            CONCATENATE zmaktx lt_tline-tdline INTO zmaktx.
          ENDLOOP.
        ENDIF.

        "����ı�
        REFRESH:lt_tline.
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
*           CLIENT                  = SY-MANDT
            id                      = 'BEST'
            language                = '1'
            name                    = lv_name
            object                  = 'MATERIAL'
          TABLES
            lines                   = lt_tline
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.
        IF sy-subrc = 0.
          LOOP AT lt_tline.
            CONCATENATE ztdxx lt_tline-tdline INTO ztdxx.
          ENDLOOP.
        ENDIF.


        CLEAR:ls_item,lt_item[].
        LOOP AT lt_tab INTO ls_tab WHERE matnr = GS_log-matnr.
          ls_tab-logid = zlogid.
          ls_tab-sgtxt = zmaktx.
          ls_tab-zsgtxt = ztdxx.
          ls_tab-zernam = sy-uname.                     "������
          ls_tab-zerdat = sy-datum.                      "��������
          ls_tab-zertim = sy-uzeit.                     "����ʱ��



          IF ls_tab-maktx <> GS_log-maktx OR
             ls_tab-sgtxt <> GS_log-zmaktx OR
             ls_tab-zsgtxt <> GS_log-ztdxx OR
             ls_tab-meins <> GS_log-meins OR
             ls_tab-matkl <> GS_log-matkl.

            ls_tab-maktx = GS_log-maktx .
            ls_tab-sgtxt = GS_log-zmaktx.
            ls_tab-zsgtxt = GS_log-ztdxx.
            ls_tab-meins = GS_log-meins .
            ls_tab-matkl = GS_log-matkl.
            CLEAR:ls_item.
            MOVE-CORRESPONDING ls_tab TO ls_item.
            APPEND ls_item TO lt_item.
          ENDIF.

        ENDLOOP.
        IF lt_item[] IS NOT INITIAL.
          MODIFY ztMM_srm_mat_log FROM TABLE lt_item.
          IF sy-subrc = 0.
*            COMMIT WORK.
          ENDIF.

        ENDIF.
      ENDIF.

    ENDIF.






******���³��ı�
***
    DATA t_header LIKE thead OCCURS 0 WITH HEADER LINE.
    DATA t_lines LIKE tline OCCURS 0 WITH HEADER LINE.
    REFRESH:t_header,t_lines.
    DATA BEGIN OF t_fields1 OCCURS 0.
    DATA lines(10000).
    DATA END OF t_fields1.
    REFRESH t_fields1.


    t_header-tdname = GS_headdata-material.
    t_header-tdobject = 'MATERIAL'.
    t_header-tdid = 'GRUN'.
    t_header-tdspras = 1.
    APPEND t_header.

    t_fields1-lines = GS_log-zmaktx.
    APPEND t_fields1.


*    CLEAR T_HEADER.
*    T_HEADER-TDNAME = GS_HEADDATA-MATERIAL.
*    T_HEADER-TDOBJECT = 'MATERIAL'.
*    T_HEADER-TDID = 'BEST'.
*    T_HEADER-TDSPRAS = 1.
*    APPEND T_HEADER.
*
*    T_FIELDS1-LINES = GS_LOG-ZTDXX.
*    APPEND T_FIELDS1.


    CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
      EXPORTING
        language    = sy-langu
      TABLES
        text_stream = t_fields1
        itf_text    = t_lines.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = sy-mandt
        header          = t_header
        savemode_direct = 'X'
      TABLES
        lines           = t_lines
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAit = ''.



**********************************

    IF GS_log-zzt = '1' AND GS_log-ztdxx = ''.

    ELSE.
      REFRESH:t_fields1,t_header,t_lines.
*    T_HEADER-TDNAME = GS_HEADDATA-MATERIAL.
*    T_HEADER-TDOBJECT = 'MATERIAL'.
*    T_HEADER-TDID = 'GRUN'.
*    T_HEADER-TDSPRAS = 1.
*    APPEND T_HEADER.
*    T_FIELDS1-LINES = GS_LOG-ZMAKTX.
*    APPEND T_FIELDS1.


***    CLEAR T_HEADER.
      t_header-tdname = GS_headdata-material.
      t_header-tdobject = 'MATERIAL'.
      t_header-tdid = 'BEST'.
      t_header-tdspras = 1.
      APPEND t_header.
      t_fields1-lines = GS_log-ztdxx.
      APPEND t_fields1.


      CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
        EXPORTING
          language    = sy-langu
        TABLES
          text_stream = t_fields1
          itf_text    = t_lines.
      IF GS_log-ztdxx IS NOT INITIAL.
        CALL FUNCTION 'SAVE_TEXT'
          EXPORTING
            client          = sy-mandt
            header          = t_header
            savemode_direct = 'X'
          TABLES
            lines           = t_lines
          EXCEPTIONS
            id              = 1
            language        = 2
            name            = 3
            object          = 4
            OTHERS          = 5.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ELSE.
        CALL FUNCTION 'DELETE_TEXT'
          EXPORTING
            client          = sy-mandt
            id              = t_header-tdid
            language        = sy-langu
            name            = t_header-tdname
            object          = t_header-tdobject
            savemode_direct = 'X'
*           TEXTMEMORY_ONLY = ' '
*           LOCAL_CAT       = ' '
          EXCEPTIONS
            not_found       = 1
            OTHERS          = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = ''.
    ENDIF.
***********************************
    READ TABLE gt_return INDEX 1.
    GS_log-cjzt = 'S'."�ɹ�
    GS_log-message = gt_return-message.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form RUN_SERVICE_BAPI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM run_service_bapi .
  CALL FUNCTION 'BAPI_SERVICE_CREATE'
    EXPORTING
      im_service_data       = GS_service_data
      im_service_datax      = GS_service_datax
      no_number_range_check = 'X'
*     TESTRUN               =
* IMPORTING
*     SERVICE               =
*     EX_SERVICE_DATA       =
*     EX_SERVICE_DATAX      =
    TABLES
      return                = gt_return
      service_description   = gt_service_description
*     service_long_texts    = gt_service_long_texts
*     EXTENSION_IN          =
*     EXTENSION_OUT         =
    .


  READ TABLE gt_return INDEX 1.
  IF gt_return-type = 'E'.
    GS_log-cjzt = 'E'."ʧ��
    GS_log-message = gt_return-message.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

******���³��ı�
***
    DATA t_header LIKE thead OCCURS 0 WITH HEADER LINE.
    DATA t_lines LIKE tline OCCURS 0 WITH HEADER LINE.
    REFRESH:t_header,t_lines.
    DATA BEGIN OF t_fields1 OCCURS 0.
    DATA lines(10000).
    DATA END OF t_fields1.
    REFRESH t_fields1.
    t_header-tdname = GS_service_data-service.
    t_header-tdobject = 'ASMD'.
    t_header-tdid = 'LTXT'.
    t_header-tdspras = 1.
    APPEND t_header.

    t_fields1-lines = GS_log-zmaktx.
    APPEND t_fields1.

    CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
      EXPORTING
        language    = sy-langu
      TABLES
        text_stream = t_fields1
        itf_text    = t_lines.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = sy-mandt
        header          = t_header
        savemode_direct = 'X'
      TABLES
        lines           = t_lines
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = ''.
    GS_log-cjzt = 'S'."�ɹ�
    GS_log-message = gt_return-message.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_SERVICE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_service_data .


  "������
  GS_service_data-service = GS_log-matnr.
  GS_service_data-matl_group = GS_log-matkl.
  GS_service_data-serv_cat = 'ALL'.
  GS_service_data-base_uom = GS_log-meins.
*  GS_service_data-base_uom_iso = GS_log-meins.

  GS_service_datax-service = 'X'.
  GS_service_datax-matl_group = 'X'.
  GS_service_datax-base_uom = 'X'.
  GS_service_datax-serv_cat = 'X'.
*  GS_service_datax-base_uom_iso = 'X'.
  "�����ͳ��ı�
  gt_service_description-short_text = GS_log-maktx.
  gt_service_description-language = '1'.
  gt_service_description-change_id = 'U'.
*  gt_service_description-language_iso = '1'.
***  gt_service_long_texts-line = GS_log-zmaktx.
***  gt_service_long_texts-language = '1'.
*  gt_service_long_texts-language_iso = '1'.
***  APPEND gt_service_long_texts.
  APPEND gt_service_description.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SEND_TO_MDM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_send_to_mdm .
  DATA:gt_s4_to_mdm TYPE TABLE OF zMM_s4_to_mdm_xml WITH HEADER LINE.
  DATA:l_xml_str TYPE string.

  DATA:g_message TYPE string.

  LOOP AT gt_log INTO GS_log.
    "���δ����ɹ����ߴ���ʧ��N��������ش�MDM
    IF GS_log-cjzt = 'S' OR ( GS_log-cjzt = 'E' AND GS_log-dycs >= p_dycs ).
      CLEAR gt_s4_to_mdm.
      MOVE-CORRESPONDING GS_log TO gt_s4_to_mdm.
      APPEND gt_s4_to_mdm.
    ENDIF.
  ENDLOOP.

  SORT gt_s4_to_mdm BY zid .

  "ƴ�ӷ�����Ҫ�õ�XML�ַ���
  CALL FUNCTION 'ZFM_MDM_MATIRAL_XML'
*    EXPORTING
*      i_jklx   = 'WL'  "����������
    IMPORTING
      output   = l_xml_str
    TABLES
      in_table = gt_s4_to_mdm[].

  "����MDM�����ӿ� ��������
  CALL FUNCTION 'ZFM_MDM_MATIRAL_RESP'
    EXPORTING
      messagetype = 'WL'
      input       = l_xml_str
    IMPORTING
      output      = g_message.





  IF g_message(1) = 'S'.
    SHIFT g_message.
    LOOP AT gt_log INTO GS_log.

      "ֻ�Ե��ýӿڵ����ݸ��»ش�״̬�ͻش���Ϣ
      READ TABLE gt_s4_to_mdm WITH KEY zid = GS_log-zid BINARY SEARCH.
      CHECK sy-subrc EQ 0.
      GS_log-hczt = 'X'.
      GS_log-message = g_message.
      MODIFY gt_log FROM GS_log TRANSPORTING hczt message.
    ENDLOOP.
  ELSEIF g_message(1) = 'E'.
    SHIFT g_message.
    LOOP AT gt_log INTO GS_log.

      "ֻ�Ե��ýӿڵ����ݸ��»ش�״̬�ͻش���Ϣ
      READ TABLE gt_s4_to_mdm WITH KEY zid = GS_log-zid BINARY SEARCH.
      CHECK sy-subrc EQ 0.
      GS_log-hczt = ''.
      SHIFT g_message.
      GS_log-message = g_message.
      MODIFY gt_log FROM GS_log TRANSPORTING hczt message.
    ENDLOOP.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DISPLAY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_alv .
  "���ò���
  PERFORM frm_set_layout.
  "�����ֶ�Ŀ¼
  PERFORM frm_set_fieldcat.
  "alv��ʾ
  PERFORM frm_alv_output.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_LAYOUT
*&---------------------------------------------------------------------*
*       ���ò���
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_set_layout .
  "���ò���
  gw_layout-cwidth_opt = 'X'.
  gw_layout-no_rowmark = 'X'.
  gw_layout-col_opt    = 'X'.
  gw_layout-zebra      = 'X'.
  gv_repid = sy-repid.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_FIELDCAT
*&---------------------------------------------------------------------*
*       �����ֶ�Ŀ¼
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_set_fieldcat .
  mcr_fieldcat 'LOGID'   '��־ID'.
  mcr_fieldcat 'MATNR'   '���ϱ��'.
  mcr_fieldcat 'MAKTX'  '��������'.
  mcr_fieldcat 'ZMAKTX'  '��������(��)'.
  mcr_fieldcat 'ZID'  '�ַ�����ID'.
  mcr_fieldcat 'STATUS'  'MDM ����״̬'.
  mcr_fieldcat 'MATKL'  '������'.
  mcr_fieldcat 'MEINS'  '����������λ'.
  mcr_fieldcat 'CREATER_ID'  '���ݴ�����ID'.
  mcr_fieldcat 'CREATER_NAME'  '���ݴ���������'.
  mcr_fieldcat 'CREATER_TIME'  '���ݴ���ʱ��'.
  mcr_fieldcat 'ZZT'  '����״̬'.
  mcr_fieldcat 'LBBM'  '���Ϸ���'.
  mcr_fieldcat 'DYCS'  '���ô���'.
  mcr_fieldcat 'HCZT'  '�ش�״̬'.
  mcr_fieldcat 'CJZT'  '����������״̬'.
  mcr_fieldcat 'MESSAGE'  '״̬��Ϣ'.
  mcr_fieldcat 'ZERNAM'  '������'.
  mcr_fieldcat 'ZERDAT'  '��������'.
  mcr_fieldcat 'ZERTIM'  '����ʱ��'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_OUTPUT
*&---------------------------------------------------------------------*
*       alv��ʾ
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_alv_output .
  "���ú�������ʾalv
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = gv_repid
*     i_callback_user_command  = 'FRM_USER_COMMAND'
      i_callback_pf_status_set = 'FRM_USER_STATUS'
      is_layout_lvc            = gw_layout
      it_fieldcat_lvc          = gt_fieldcat
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_log
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_USER_STATUS
*&---------------------------------------------------------------------*
*       gui״̬
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_user_status USING extab TYPE slis_t_extab.
  "����gui״̬
  SET PF-STATUS 'STANDARD' .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form RUN_SERVICE_CHANGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM run_service_change .
  DATA:servicenumber TYPE asnum.
  servicenumber = GS_service_data-service.

  CALL FUNCTION 'BAPI_SERVICE_CHANGE'
    EXPORTING
      servicenumber       = servicenumber
      im_service_data     = GS_service_data
      im_service_datax    = GS_service_datax
*     TESTRUN             =
* IMPORTING
*     EX_SERVICE_DATA     =
*     EX_SERVICE_DATAX    =
    TABLES
      return              = gt_return
      service_description = gt_service_description
*     service_long_texts  = gt_service_long_texts
*     EXTENSION_IN        =
*     EXTENSION_OUT       =
    .
  READ TABLE gt_return INDEX 1.
  IF gt_return-type = 'E'.
    GS_log-cjzt = 'E'."ʧ��
    GS_log-message = gt_return-message.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

******���³��ı�
***
    DATA t_header LIKE thead OCCURS 0 WITH HEADER LINE.
    DATA t_lines LIKE tline OCCURS 0 WITH HEADER LINE.
    REFRESH:t_header,t_lines.
    DATA BEGIN OF t_fields1 OCCURS 0.
    DATA lines(10000).
    DATA END OF t_fields1.
    REFRESH t_fields1.
    t_header-tdname = GS_service_data-service.
    t_header-tdobject = 'ASMD'.
    t_header-tdid = 'LTXT'.
    t_header-tdspras = 1.
    APPEND t_header.

    t_fields1-lines = GS_log-zmaktx.
    APPEND t_fields1.

    CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
      EXPORTING
        language    = sy-langu
      TABLES
        text_stream = t_fields1
        itf_text    = t_lines.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = sy-mandt
        header          = t_header
        savemode_direct = 'X'
      TABLES
        lines           = t_lines
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = ''.
    GS_log-cjzt = 'S'."�ɹ�
    GS_log-message = gt_return-message.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHANGE_MATERIAL_MSTAE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM frm_change_material_mstae  USING    p1.



  "������
  IF lt_tab[] IS NOT INITIAL.
    READ TABLE lt_tab INTO ls_tab WITH KEY matnr = GS_log-matnr.
    IF sy-subrc = 0.
      DATA:zlogid TYPE zzmmlogid.
      DATA:zmaktx TYPE zzmmmaktx.
      DATA:ztdxx  TYPE zzmmtdxx.
      DATA : lt_tline TYPE TABLE OF tline WITH HEADER LINE  .
      DATA:lv_name TYPE thead-tdname.
      lv_name = GS_log-matnr.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '68'
          object                  = 'ZLOGID'
        IMPORTING
          number                  = zlogid
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      "���ı�
      REFRESH lt_tline.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*         CLIENT                  = SY-MANDT
          id                      = 'GRUN'
          language                = '1'
          name                    = lv_name
          object                  = 'MATERIAL'
        TABLES
          lines                   = lt_tline
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc = 0.
        LOOP AT lt_tline.
          CONCATENATE zmaktx lt_tline-tdline INTO zmaktx.
        ENDLOOP.
      ENDIF.

      "����ı�
      REFRESH:lt_tline.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*         CLIENT                  = SY-MANDT
          id                      = 'BEST'
          language                = '1'
          name                    = lv_name
          object                  = 'MATERIAL'
        TABLES
          lines                   = lt_tline
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc = 0.
        LOOP AT lt_tline.
          CONCATENATE ztdxx lt_tline-tdline INTO ztdxx.
        ENDLOOP.
      ENDIF.


      CLEAR:ls_item,lt_item[].
      LOOP AT lt_tab INTO ls_tab WHERE matnr = GS_log-matnr.
        ls_tab-logid = zlogid.
        ls_tab-sgtxt = zmaktx.
        ls_tab-zsgtxt = ztdxx.
        ls_tab-zernam = sy-uname.                     "������
        ls_tab-zerdat = sy-datum.                      "��������
        ls_tab-zertim = sy-uzeit.                     "����ʱ��
        ls_tab-mstae = p1.
        CLEAR:ls_item.
        MOVE-CORRESPONDING ls_tab TO ls_item.
        APPEND ls_item TO lt_item.
      ENDLOOP.
      IF lt_item[] IS NOT INITIAL.
        MODIFY ztMM_srm_mat_log FROM TABLE lt_item.
        IF sy-subrc = 0.
*            COMMIT WORK.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDIF.



***���Ϻ�
  CALL FUNCTION 'CONVERSION_EXIT_MATN5_INPUT'
    EXPORTING
      input        = GS_log-matnr
    IMPORTING
      output       = GS_headdata-material
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
*     Implement suitable error handling here
  ENDIF.

  GS_headdata-ind_sector      = 'C'.                   "��ҵ����
  GS_headdata-matl_type       = 'Z001'.                "��������

  GS_headdata-basic_view      = 'X'.                   "����������ͼΪX



  GS_clientdata-matl_group     = GS_log-matkl.    "������
  GS_clientdata-pur_status       = p1.   "�繤������״̬
*  GS_clientdata-base_uom       = GS_log-meins.   "����������λ
*  GS_clientdata-base_uom_iso   = GS_log-meins.  "����������λ

  GS_clientdatax-matl_group    ='X'.
  GS_clientdatax-pur_status    ='X'.
*  GS_clientdatax-base_uom      ='X'.
*  GS_clientdatax-base_uom_iso  ='X'.

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      headdata    = GS_headdata
      clientdata  = GS_clientdata
      clientdatax = GS_clientdatax
    IMPORTING
      return      = gt_return.
  READ TABLE gt_return INDEX 1.
  IF gt_return-type = 'E' .

    GS_log-cjzt = 'E'."ʧ��
    GS_log-message = gt_return-message.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    GS_log-cjzt = 'S'."�ɹ�
    GS_log-message = gt_return-message.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHANGE_SERVICE_MSTAE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM frm_change_service_mstae  USING    p1.

  "������
  GS_service_data-service = GS_log-matnr.
  GS_service_data-p_status = p1.
  IF p1 <> ''.
    GS_service_data-valid_from = sy-datum.
  ELSE.
    GS_service_data-valid_from = '00000000'.
  ENDIF.

  GS_service_datax-service = 'X'.
  GS_service_datax-p_status = 'X'.
  GS_service_datax-valid_from = 'X'.
  "�����ͳ��ı�
  gt_service_description-short_text = GS_log-maktx.
  gt_service_description-language = '1'.
  APPEND gt_service_description.
*  gt_service_long_texts-line = GS_log-zmaktx.



  CALL FUNCTION 'BAPI_SERVICE_CHANGE'
    EXPORTING
      servicenumber       = GS_service_data-service
      im_service_data     = GS_service_data
      im_service_datax    = GS_service_datax
*     TESTRUN             =
* IMPORTING
*     EX_SERVICE_DATA     =
*     EX_SERVICE_DATAX    =
    TABLES
      return              = gt_return
      service_description = gt_service_description
*     service_long_texts  = gt_service_long_texts
*     EXTENSION_IN        =
*     EXTENSION_OUT       =
    .
  READ TABLE gt_return INDEX 1.
  IF gt_return-type = 'E'.
    GS_log-cjzt = 'E'."ʧ��
    GS_log-message = gt_return-message.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    GS_log-cjzt = 'S'."�ɹ�
    GS_log-message = gt_return-message.
  ENDIF.
ENDFORM.


*Messages
*----------------------------------------------------------
*
* Message class: Hard coded
*   û�����ݣ�

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
