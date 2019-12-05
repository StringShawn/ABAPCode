**************************************************
*程序名称:物料主数据创建
*创建日期: 2019-11-21
*创建者:XXX
*申请者:XXX
*功能描述: Report ZMMRPT_MDM_TO_S4_MATERIAL
*============================================
*变更记录
*修改请求号    修改日期    修改人   修改描述
*DEVK912033    2019-11-21   HAND   平移
***************************************************
REPORT ZSHXJMM0002.

*声明
TABLES:ztMM_mdm_mat_log.

*定义内表
DATA:gt_log TYPE TABLE OF ZTMM_MDM_mat_log , "WITH HEADER LINE,
     gt_mx  TYPE TABLE OF ZTMM_MDM_mat_tx,
     gt_txt TYPE TABLE OF ZTMM_MDM_mat_txt.

*定义结构
DATA:GS_log TYPE ZTMM_MDM_mat_log,
     GS_mx  TYPE ZTMM_MDM_mat_tx,
     GS_txt TYPE ZTMM_MDM_mat_txt.

**接口返回消息
DATA:g_message TYPE string.
"甘泽余
DATA:BEGIN OF ls_tab,
       maktx LIKE makt-maktx.
    INCLUDE STRUCTURE ztMM_srm_mat_log.
DATA:END OF ls_tab.

DATA:lt_tab LIKE TABLE OF ls_tab.
DATA:ls_item TYPE ztMM_srm_mat_log.
DATA:lt_item TYPE TABLE OF ztMM_srm_mat_log.

*&---------------------------------------------------------------------*
*&       物料主数据          声明 BAPI所需的字段串与内表
*&---------------------------------------------------------------------*
DATA GS_headdata       LIKE bapimathead.      "声明带有控制信息的表头段
DATA GS_clientdata     LIKE bapi_mara.        "声明客户端层次物料数据
DATA GS_clientdatax    LIKE bapi_marax.       "声明BAPI_MARA 的复选框结构
DATA GS_plantdata      LIKE bapi_marc.        "声明工厂级别的物料数据
DATA GS_plantdatax     LIKE bapi_marcx.       "声明BAPI_MARC 的复选框结构
DATA GS_mard           LIKE bapi_mard.        "存储位置级别的物料数据
DATA GS_mardx          LIKE bapi_mardx.       "BAPI_MARD 的复选框结构
DATA GS_valuationdata  LIKE bapi_mbew.        "声明评估数据
DATA GS_valuationdatax LIKE bapi_mbewx.       "BAPI_MBEW 的复选框结构
DATA GS_return         LIKE bapiret2.         "声明RETURN
DATA GS_mvke           LIKE bapi_mvke.
DATA GS_mvkex          LIKE bapi_mvkex.
DATA gt_mater          LIKE bapi_makt OCCURS 10 WITH HEADER LINE.                "声明MATERIALDESCRIPTION
DATA gt_unit           LIKE bapi_marm OCCURS 10 WITH HEADER LINE.                "声明UNITSOFMEASURE
DATA gt_unitx          LIKE bapi_marmx OCCURS 10 WITH HEADER LINE.               "声明UNITSOFMEASUREX
DATA gt_mlan           LIKE bapi_mlan OCCURS 10 WITH HEADER LINE.
DATA gt_internationalartnos LIKE bapi_mean OCCURS 0 WITH HEADER LINE .
DATA gt_mltx                LIKE bapi_mltx OCCURS 0 WITH HEADER LINE.
DATA gt_qmat                LIKE bapi1001004_qmat OCCURS 0 WITH HEADER LINE.
DATA gt_return              LIKE bapi_matreturn2 OCCURS 0 WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&       服务主数据          声明 BAPI所需的字段串与内表
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

"字段目录的宏
DEFINE mcr_fieldcat.

  CLEAR gw_fieldcat.
  gw_fieldcat-fieldname   = &1.
  gw_fieldcat-scrtext_m   = &2.

  APPEND gw_fieldcat TO gt_fieldcat.

END-OF-DEFINITION.





*选择屏幕
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
*获取需要更新的物料数据
  PERFORM frm_get_data.
*填充物料主数据bapi。
  PERFORM frm_fill_bapi.
***日志更新完成之后将数据调用接口发送到MDM
**  PERFORM frm_send_to_mdm.  “”DELETE BY IBM-CC 接口方式变更为异步，不需要给MDMD返回消息，返回接口取消
**数据展示
  IF sy-batch = ''.
    PERFORM frm_display_alv.
  ENDIF.


*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& text  获取需要更新的物料数据
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

**获取需要更新的物料数据
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
    MESSAGE '没有数据！' TYPE 'E'.
  ELSE.
    SORT   gt_log BY matnr logid DESCENDING.
    DELETE ADJACENT DUPLICATES FROM gt_log[] COMPARING matnr.
  ENDIF.
*获取对应的明细数据
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


  "甘泽余

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

**循环内表创建物料主数据
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

    "创建物料主数据
    IF GS_log-zzt = '1' AND GS_log-matkl(2) <> '90'.

      "填充抬头数据
      PERFORM fill_headdata.                         "填充HEADDATA
      "填充基础视图明细数据
      PERFORM fill_mx_data.                          "填充所有明细数据
      "创建物料主数据
      PERFORM run_material_bapi.                         "调用BAPI

***如果物料分类为07、09类的产成品物料，需要同步相应的特征量并存储在自定义表中
*      IF GS_log-matkl(2) = '07' OR GS_log-matkl(2) = '09'.
*
*      ENDIF.
    ELSEIF  GS_log-zzt = '1' AND GS_log-matkl(2) = '90'.
      "创建服务主数据

      "填充服务主数据BAPI结构
      PERFORM fill_service_data.
      "运行服务主数据创建bapi
      PERFORM run_service_bapi.


    ELSEIF     GS_log-zzt = '2' AND GS_log-matkl(2) <> '90'.
      "填充抬头数据
      PERFORM fill_headdata.                         "填充HEADDATA
      "填充基础视图明细数据
      PERFORM fill_mx_data.                          "填充所有明细数据
      "创建物料主数据
      PERFORM run_material_bapi.                         "调用BAPI

    ELSEIF     GS_log-zzt = '2' AND GS_log-matkl(2) = '90'.

      "填充服务主数据BAPI结构
      PERFORM fill_service_data.
      "运行服务主数据创建bapi
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


***处理完成后更新日志表
  MODIFY ZTMM_MDM_mat_log  FROM TABLE gt_log.
  IF sy-subrc EQ 0.
    COMMIT WORK.
    MESSAGE '日志更新成功！' TYPE 'S'.
  ELSE.
    MESSAGE '日志更新失败！' TYPE 'S'.
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


***物料号
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

  GS_headdata-ind_sector      = 'C'.                   "行业领域
  GS_headdata-matl_type       = 'Z001'.                "物料类型

  GS_headdata-basic_view      = 'X'.                   "基本数据视图为X

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

***  物料描述
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


  GS_clientdata-matl_group     = GS_log-matkl.    "物料组
  GS_clientdata-base_uom       = GS_log-meins.   "基本计量单位
  GS_clientdata-base_uom_iso   = GS_log-meins.  "基本计量单位

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

    GS_log-cjzt = 'E'."失败
    GS_log-message = gt_return-message.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAit = 'X'.


    "甘泽余
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

        "长文本
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

        "替代文本
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
          ls_tab-zernam = sy-uname.                     "创建人
          ls_tab-zerdat = sy-datum.                      "创建日期
          ls_tab-zertim = sy-uzeit.                     "创建时间



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






******更新长文本
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
    GS_log-cjzt = 'S'."成功
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
    GS_log-cjzt = 'E'."失败
    GS_log-message = gt_return-message.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

******更新长文本
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
    GS_log-cjzt = 'S'."成功
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


  "服务编号
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
  "描述和长文本
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
    "本次创建成功或者创建失败N次以上则回传MDM
    IF GS_log-cjzt = 'S' OR ( GS_log-cjzt = 'E' AND GS_log-dycs >= p_dycs ).
      CLEAR gt_s4_to_mdm.
      MOVE-CORRESPONDING GS_log TO gt_s4_to_mdm.
      APPEND gt_s4_to_mdm.
    ENDIF.
  ENDLOOP.

  SORT gt_s4_to_mdm BY zid .

  "拼接返回需要用的XML字符串
  CALL FUNCTION 'ZFM_MDM_MATIRAL_XML'
*    EXPORTING
*      i_jklx   = 'WL'  "物料主数据
    IMPORTING
      output   = l_xml_str
    TABLES
      in_table = gt_s4_to_mdm[].

  "调用MDM反馈接口 返回数据
  CALL FUNCTION 'ZFM_MDM_MATIRAL_RESP'
    EXPORTING
      messagetype = 'WL'
      input       = l_xml_str
    IMPORTING
      output      = g_message.





  IF g_message(1) = 'S'.
    SHIFT g_message.
    LOOP AT gt_log INTO GS_log.

      "只对调用接口的数据更新回传状态和回传消息
      READ TABLE gt_s4_to_mdm WITH KEY zid = GS_log-zid BINARY SEARCH.
      CHECK sy-subrc EQ 0.
      GS_log-hczt = 'X'.
      GS_log-message = g_message.
      MODIFY gt_log FROM GS_log TRANSPORTING hczt message.
    ENDLOOP.
  ELSEIF g_message(1) = 'E'.
    SHIFT g_message.
    LOOP AT gt_log INTO GS_log.

      "只对调用接口的数据更新回传状态和回传消息
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
  "设置布局
  PERFORM frm_set_layout.
  "设置字段目录
  PERFORM frm_set_fieldcat.
  "alv显示
  PERFORM frm_alv_output.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_LAYOUT
*&---------------------------------------------------------------------*
*       设置布局
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_set_layout .
  "设置布局
  gw_layout-cwidth_opt = 'X'.
  gw_layout-no_rowmark = 'X'.
  gw_layout-col_opt    = 'X'.
  gw_layout-zebra      = 'X'.
  gv_repid = sy-repid.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_FIELDCAT
*&---------------------------------------------------------------------*
*       设置字段目录
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_set_fieldcat .
  mcr_fieldcat 'LOGID'   '日志ID'.
  mcr_fieldcat 'MATNR'   '物料编号'.
  mcr_fieldcat 'MAKTX'  '物料描述'.
  mcr_fieldcat 'ZMAKTX'  '物料描述(长)'.
  mcr_fieldcat 'ZID'  '分发数据ID'.
  mcr_fieldcat 'STATUS'  'MDM 数据状态'.
  mcr_fieldcat 'MATKL'  '物料组'.
  mcr_fieldcat 'MEINS'  '基本计量单位'.
  mcr_fieldcat 'CREATER_ID'  '数据创建人ID'.
  mcr_fieldcat 'CREATER_NAME'  '数据创建人名称'.
  mcr_fieldcat 'CREATER_TIME'  '数据创建时间'.
  mcr_fieldcat 'ZZT'  '物料状态'.
  mcr_fieldcat 'LBBM'  '物料分类'.
  mcr_fieldcat 'DYCS'  '调用次数'.
  mcr_fieldcat 'HCZT'  '回传状态'.
  mcr_fieldcat 'CJZT'  '更新主数据状态'.
  mcr_fieldcat 'MESSAGE'  '状态消息'.
  mcr_fieldcat 'ZERNAM'  '创建人'.
  mcr_fieldcat 'ZERDAT'  '创建日期'.
  mcr_fieldcat 'ZERTIM'  '创建时间'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_OUTPUT
*&---------------------------------------------------------------------*
*       alv显示
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_alv_output .
  "调用函数，显示alv
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
*       gui状态
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_user_status USING extab TYPE slis_t_extab.
  "设置gui状态
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
    GS_log-cjzt = 'E'."失败
    GS_log-message = gt_return-message.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

******更新长文本
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
    GS_log-cjzt = 'S'."成功
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



  "甘泽余
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

      "长文本
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

      "替代文本
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
        ls_tab-zernam = sy-uname.                     "创建人
        ls_tab-zerdat = sy-datum.                      "创建日期
        ls_tab-zertim = sy-uzeit.                     "创建时间
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



***物料号
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

  GS_headdata-ind_sector      = 'C'.                   "行业领域
  GS_headdata-matl_type       = 'Z001'.                "物料类型

  GS_headdata-basic_view      = 'X'.                   "基本数据视图为X



  GS_clientdata-matl_group     = GS_log-matkl.    "物料组
  GS_clientdata-pur_status       = p1.   "跨工厂物料状态
*  GS_clientdata-base_uom       = GS_log-meins.   "基本计量单位
*  GS_clientdata-base_uom_iso   = GS_log-meins.  "基本计量单位

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

    GS_log-cjzt = 'E'."失败
    GS_log-message = gt_return-message.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    GS_log-cjzt = 'S'."成功
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

  "服务编号
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
  "描述和长文本
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
    GS_log-cjzt = 'E'."失败
    GS_log-message = gt_return-message.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    GS_log-cjzt = 'S'."成功
    GS_log-message = gt_return-message.
  ENDIF.
ENDFORM.


*Messages
*----------------------------------------------------------
*
* Message class: Hard coded
*   没有数据！

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
