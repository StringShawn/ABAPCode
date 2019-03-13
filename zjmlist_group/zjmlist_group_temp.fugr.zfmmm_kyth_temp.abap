FUNCTION ZFMMM_KYTH_TEMP .
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     VALUE(IM_BHNUC) TYPE  ZBHNUC
*"     VALUE(IM_CHANGE) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      GT_DATA STRUCTURE  ZSMM_KYTH
*"      RETURN TYPE  BAPIRET2_T
*"  EXCEPTIONS
*"      ERROR_EXCEPTION
*"--------------------------------------------------------------------
  DATA:lt_bhnuc TYPE TABLE OF ztrade_odlist,
       ls_bhnuc TYPE ztrade_odlist.
  DATA lt_path_t TYPE TABLE OF zlsjm_path.
  DATA:lv_ogrup    TYPE n LENGTH 3.
  DATA:ls_return   TYPE bapiret2.
  DATA:lv_vbeln     TYPE vbak-vbeln, "so
       lv_vbeln_dn  TYPE likp-vbeln, "dn
       lv_vbeln_sto TYPE likp-vbeln, "sto
       lv_ebeln     TYPE ekko-ebeln. "po
  DATA rtype TYPE char1.
  DATA rtmsg TYPE bapi_msg.
  " 从ZTRADE_ODLIST 表中取出单据流数据
  REFRESH gt_kythlist.

  SELECT *
  INTO TABLE lt_bhnuc
  FROM ztrade_odlist
  WHERE bhnuc = im_bhnuc.

  SORT lt_bhnuc DESCENDING BY ogrup.
  READ TABLE lt_bhnuc INTO ls_bhnuc INDEX 1.
  lv_ogrup = ls_bhnuc-ogrup.

  "从 ZSD_JMFH_H 表中取出路径及相关数据
  SELECT SINGLE *
  INTO @DATA(ls_jmfh_h)
  FROM zsd_jmfh_h
  WHERE bhnuc = @im_bhnuc.

  gs_kythlist-bhnuc = im_bhnuc.
  gs_kythlist-fwerk = ls_jmfh_h-werks.
  gs_kythlist-pathn = ls_jmfh_h-pathn.
  gs_kythlist-step  = 0.
  gs_kythlist-ogrup = 1.

  SELECT *
  INTO TABLE gt_kythlist
  FROM zmm_kythlist
  WHERE bhnuc = im_bhnuc.

    CALL FUNCTION 'ZGET_WERKS_PATH'
      EXPORTING
        i_werks    = ls_jmfh_h-werks
        i_pathn    = ls_jmfh_h-pathn
        i_type     = '2'
      IMPORTING
        rtype      = rtype
        rtmsg      = rtmsg
      TABLES
        ot_jm_path = lt_path_t.
  IF rtype = 'E'.
    ls_return-type = 'E'.
    ls_return-id   = '00'.
    ls_return-number = '001'.
    ls_return-MESSAGE_V1 = rtmsg.
    APPEND ls_return to return.
    RAISE error_exception.
  ENDIF.

  return_flag = 'X'.

  READ TABLE  lt_path_t INTO active_path with KEY pathn = ls_jmfh_h-pathn.
    "step1 收货门店PO退货
    READ TABLE lt_bhnuc INTO ls_bhnuc WITH KEY ogrup = lv_ogrup
                                               odetp = 1.
    IF sy-subrc = 0.   "若PO存在
      gs_kythlist-bukrs = ls_bhnuc-bukrs.
      SELECT SINGLE ebeln INTO @DATA(temp_ebeln) FROM ekko
      WHERE ebeln =  @ls_bhnuc-odeln
        AND bsart NE 'ZUB'.
      IF sy-subrc = 0.
        SELECT SINGLE ebeln
        INTO @DATA(temp_ebeln_sto)
        FROM ekbe
        WHERE ebeln = @ls_bhnuc-odeln
          AND vgabe = '1'.
        IF sy-subrc = 0.  "如果存在收货 ME21N创建新的同订单类型PO单，退货项目勾选
          lv_ebeln = ls_bhnuc-odeln.
          PERFORM frm_create_po TABLES return gt_data gt_kythlist
                                USING im_change
                                CHANGING lv_ebeln .

          PERFORM insert_zmm_kyth TABLES return gt_data
                                  USING im_bhnuc ls_jmfh_h-werks.
          PERFORM frm_handle_result TABLES return.

          PERFORM frm_migo_po TABLES return gt_data gt_kythlist
                                  USING ls_bhnuc.   "po收货
          PERFORM frm_handle_result TABLES return.

        ELSE. "如果不存在收货  po打删除标记
          PERFORM frm_delete_po TABLES return gt_data gt_kythlist
                                USING ls_bhnuc-odeln im_change.

          PERFORM insert_zmm_kyth TABLES return gt_data
                                  USING im_bhnuc ls_jmfh_h-werks.

          PERFORM frm_handle_result TABLES return.
        ENDIF.


      ENDIF.

    ENDIF.

    DO lv_ogrup TIMES.

      "step2 发货公司SO退货
      READ TABLE lt_bhnuc INTO ls_bhnuc WITH KEY ogrup = lv_ogrup
                                                 odetp = 2.
      IF sy-subrc = 0.   "若SO存在
        gs_kythlist-bukrs = ls_bhnuc-bukrs.
        CLEAR :lv_vbeln.
        lv_vbeln = ls_bhnuc-odeln.
        READ TABLE lt_bhnuc INTO ls_bhnuc WITH KEY ogrup = lv_ogrup
                                                   odetp = 4.
        IF sy-subrc = 0.  "销售订单过账凭证存在


          IF im_change IS NOT INITIAL.    "如果修改了数量，用bapi创建销售订单
            PERFORM frm_create_so TABLES return gt_data gt_kythlist
                                  USING  ls_bhnuc lv_vbeln.
          ELSE.
            PERFORM frm_copy_so TABLES return gt_data gt_kythlist    "复制销售订单
                                USING  lv_vbeln.
          ENDIF.
          PERFORM frm_handle_result TABLES return.

          PERFORM frm_create_dn TABLES return gt_data gt_kythlist    "创建交货单
                                USING  lv_vbeln.
          PERFORM frm_handle_result TABLES return.

          PERFORM frm_post_dn TABLES return gt_data gt_kythlist.    "交货单过账.

          PERFORM frm_handle_result TABLES return.

        ELSE.
          READ TABLE lt_bhnuc INTO ls_bhnuc WITH KEY ogrup = lv_ogrup
                                                     odetp = 3.
          lv_vbeln_dn = ls_bhnuc-odeln.
          PERFORM frm_delete_dn TABLES return gt_data gt_kythlist
                                USING lv_vbeln_dn.
          PERFORM frm_handle_result TABLES return.

          PERFORM frm_delete_so TABLES return gt_data gt_kythlist
                                USING  lv_vbeln .
          PERFORM frm_handle_result TABLES return.
        ENDIF.
      ENDIF.

      "step3 退货会计凭证
      IF sy-index = 1.
        READ TABLE lt_bhnuc INTO ls_bhnuc WITH KEY odetp = 5.
        IF sy-subrc = 0. "存在订单类型为5的情况
          PERFORM frm_create_bill TABLES return gt_data gt_kythlist
                                   USING  ls_bhnuc ls_jmfh_h. "创建会计凭证
          PERFORM frm_handle_result TABLES return.
        ENDIF.
      ENDIF.

      "step4 公司间交易po退货

      READ TABLE lt_bhnuc INTO ls_bhnuc WITH KEY ogrup = lv_ogrup
                                                 odetp = 1.
      IF sy-subrc = 0. "如果po存在
        gs_kythlist-bukrs = ls_bhnuc-bukrs.
        SELECT SINGLE ebeln INTO @DATA(temp_ebeln_zub) FROM ekko
        WHERE ebeln =  @ls_bhnuc-odeln
          AND bsart = 'ZUB'.  "如果是跨公司调拨订单
        IF sy-subrc = 0.
          SELECT SINGLE ebeln
          INTO @DATA(temp_sto_zub)
          FROM ekbe
          WHERE ebeln = @ls_bhnuc-odeln
            AND vgabe = '1'.
          IF sy-subrc = 0. "检查po是否收货
            lv_ebeln = ls_bhnuc-odeln.
            PERFORM frm_create_po TABLES return gt_data gt_kythlist
                                  USING im_change
                                  CHANGING lv_ebeln.    "创建反向po
            PERFORM frm_handle_result TABLES return.

            READ TABLE lt_bhnuc INTO DATA(temp_bhnuc) WITH KEY ogrup = lv_ogrup
                                                       odetp = 3.

            PERFORM frm_create_sto TABLES return gt_data gt_kythlist
                                  USING lv_ebeln temp_bhnuc.     "创建交货单
            PERFORM frm_handle_result TABLES return.

            PERFORM frm_migo_po TABLES return gt_data gt_kythlist
                                  USING ls_bhnuc.     "po收货
            PERFORM frm_handle_result TABLES return.

            PERFORM frm_post_sto TABLES return gt_data gt_kythlist.                           "交货单过账
            PERFORM frm_handle_result TABLES return.

          ELSE.
            PERFORM frm_reverse_sto TABLES return gt_data gt_kythlist  "冲销交货单
                                  USING ls_bhnuc-odeln.
            PERFORM frm_handle_result TABLES return.

            PERFORM frm_delete_po TABLES return gt_data gt_kythlist    "删除po
                                  USING ls_bhnuc-odeln im_change.
            PERFORM frm_handle_result TABLES return.

          ENDIF.

        ENDIF.
      ENDIF.

      SUBTRACT 1 FROM lv_ogrup.
      ADD 1 TO  gs_kythlist-ogrup .
      gs_kythlist-step = 0.
    ENDDO.
    UPDATE zmm_kyth_h set thstu = '2' WHERE bhnuc = im_bhnuc.
    COMMIT WORK AND WAIT.
    CLEAR return_flag.
ENDFUNCTION.
FORM frm_handle_result TABLES pt_return TYPE bapiret2_t.
  DATA:ls_return TYPE bapiret2.

  CHECK return_flag IS INITIAL.

  READ TABLE pt_return INTO ls_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    CALL FUNCTION 'DEQUEUE_ALL'.
    CLEAR return_flag.
    RAISE error_exception.
  ELSE.
    gs_kythlist-odjhr = sy-datum+0(4).
    gs_kythlist-crdat = sy-datum.
    gs_kythlist-crtim = sy-uzeit.
    gs_kythlist-crnam = sy-uname.

    TRY .

        INSERT zmm_kythlist FROM gs_kythlist.
*          IF gs_kythlist-ogrup = 001 AND gs_kythlist-step = 001.
*            UPDATE zsd_jmth_h SET thstu = '3' WHERE ebeln = active_thlist-ebeln.
*          ENDIF.
        APPEND gs_kythlist TO gt_kythlist.

        COMMIT WORK AND WAIT.

      CATCH cx_sy_sql_error.

        ROLLBACK WORK.
        CLEAR ls_return.
        ls_return-id = '00'.
        ls_return-type = 'E'.
        ls_return-number = '001'.
        ls_return-message_v1 = '数据保存失败'.
        APPEND ls_return TO pt_return.
        RAISE error_exception.

    ENDTRY.
    REFRESH pt_return.
  ENDIF.
ENDFORM.
