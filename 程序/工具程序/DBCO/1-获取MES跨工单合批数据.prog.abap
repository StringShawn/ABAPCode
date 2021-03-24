REPORT zsmdi0006.

DATA : v_con TYPE dbcon-con_name . " value 'TESTDB' . " 数据库连接字符串
TYPES : BEGIN OF typ_trans.
        INCLUDE STRUCTURE zsdt008.
TYPES   END OF typ_trans.
DATA : it_trans TYPE TABLE OF typ_trans,
       wa_trans TYPE typ_trans.
DATA:gt_zsdt008 TYPE TABLE OF zsdt008,
     gs_zsdt008 TYPE zsdt008.
FIELD-SYMBOLS <fs_zsdt008> TYPE zsdt008.
SELECTION-SCREEN BEGIN OF BLOCK mm01 WITH FRAME .
PARAMETERS: p_werks TYPE aufk-werks DEFAULT 'HF01'.
SELECT-OPTIONS:p_datum FOR sy-datum NO-EXTENSION .
PARAMETERS:p_1 TYPE c RADIOBUTTON GROUP 1,
           p_2 TYPE c RADIOBUTTON GROUP 1.
SELECTION-SCREEN END OF BLOCK mm01 .

***&---------------------------------------------------------------------*
***      数据处理逻辑
***----------------------------------------------------------------------*
START-OF-SELECTION.

  CASE 'X'.
    WHEN p_1.
      PERFORM frm_connect_mes .
      PERFORM frm_get_order_data .
      PERFORM frm_close_connect .
    WHEN p_2.
      DELETE FROM zsdt008.
      COMMIT WORK AND WAIT.
    WHEN OTHERS.
  ENDCASE.


END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  FRM_CONNECT_MES
*&---------------------------------------------------------------------*
FORM frm_connect_mes .

  SELECT SINGLE con_name INTO v_con
    FROM zpp_connect_mes
  WHERE zgroup = '01'
    AND sysid = sy-sysid AND werks = p_werks.
  IF sy-subrc = 0 .
    EXEC SQL .
      CONNECT TO :V_CON
    ENDEXEC.

    IF sy-subrc = 0 .
      EXEC SQL.
        SET CONNECTION :V_CON
      ENDEXEC.
      IF sy-subrc NE 0 .
        MESSAGE 'MES数据库连接失败!' TYPE 'E' .
      ENDIF.
    ENDIF.
  ENDIF .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_CLOSE_CONNECT
*&---------------------------------------------------------------------*
FORM frm_close_connect .

  EXEC SQL.
    DISCONNECT :V_CON
  ENDEXEC.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_ORDER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_order_data .
  DATA : l_error(250) TYPE c .
  DATA : exec_ref TYPE REF TO cx_sy_native_sql_error.
  DATA:lt_ztfi012       TYPE TABLE OF ztfi012,
       lt_ztfi012_exist TYPE TABLE OF ztfi012,
       ls_ztfi012       TYPE ztfi012.
  DATA:lv_start TYPE string.
  DATA:lv_end TYPE string.
  READ TABLE p_datum INDEX 1.
  IF p_datum-high IS INITIAL.
    lv_start = |{ p_datum-low+0(4) }-{ p_datum-low+4(2) }-{ p_datum-low+6(2) } 00:00:00|.
    lv_end   = |{ p_datum-low+0(4) }-{ p_datum-low+4(2) }-{ p_datum-low+6(2) } 23:59:59|.
  ELSE.
    lv_start = |{ p_datum-low+0(4) }-{ p_datum-low+4(2) }-{ p_datum-low+6(2) } 00:00:00|.
    lv_end   = |{ p_datum-high+0(4) }-{ p_datum-high+4(2) }-{ p_datum-high+6(2) } 23:59:59|.
  ENDIF.

  REFRESH : it_trans .

  TRY.
      EXEC SQL.
        OPEN DBCUR FOR
       SELECT FROMLOTNO,
              TOLOTNO,
              FROMLOTQTY,
              FROMSCRAPQTY,
              TOLOTQTY,
              TOSCRAPQTY,
              OPNO,
              FROMMONO,
              TOMONO,
              USERNO,
              to_char(INSERT_TIME,'yyyyMMdd'),
              to_char(INSERT_TIME,'HHmiss')
            FROM ZPMDM0012
            WHERE INSERT_TIME >= to_date(:lv_start,'yyyy-MM-dd HH24:mi:ss')
              AND INSERT_TIME <= to_date(:lv_end,'yyyy-MM-dd HH24:mi:ss')
      ENDEXEC.
      DO.
        EXEC SQL.
          FETCH NEXT DBCUR INTO :WA_TRANS-FROMLOTNO,
                                :WA_TRANS-TOLOTNO,
                                :WA_TRANS-FROMLOTQTY,
                                :WA_TRANS-FROMSCRAPQTY,
                                :WA_TRANS-TOLOTQTY,
                                :WA_TRANS-TOSCRAPQTY,
                                :WA_TRANS-OPNO,
                                :WA_TRANS-FROMMONO,
                                :WA_TRANS-TOMONO,
                                :WA_TRANS-USERNO,
                                :WA_TRANS-INSERTDATE,
                                :WA_TRANS-INSERTTIME
        ENDEXEC.
        IF sy-subrc NE 0.
          EXIT.
        ELSE.
          APPEND wa_trans TO it_trans .
        ENDIF.
      ENDDO.
      EXEC SQL.
        CLOSE DBCUR
      ENDEXEC.

    CATCH cx_sy_native_sql_error INTO exec_ref .
      l_error = exec_ref->get_text( ).
      CONCATENATE '读取数据出错,连接名:' v_con  l_error INTO l_error .
      WRITE :/ l_error .
  ENDTRY.

  IF it_trans IS INITIAL .
    MESSAGE 'No data' TYPE 'S' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING .
  ENDIF .
  IF it_trans IS NOT INITIAL.

    DATA:batch_atinn TYPE ausp-atinn.
    DATA:mes_aufnr_atinn TYPE ausp-atinn.
    DATA:sap_aufnr_atinn TYPE ausp-atinn.

    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'MES_COF_BATCH'
      IMPORTING
        output = batch_atinn.

    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'RUN_CARD'
      IMPORTING
        output = mes_aufnr_atinn.

    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'PRD_ORD'
      IMPORTING
        output = sap_aufnr_atinn.

    LOOP AT it_trans INTO wa_trans.

      APPEND INITIAL LINE TO gt_zsdt008 ASSIGNING <fs_zsdt008>.
      <fs_zsdt008> = CORRESPONDING #( wa_trans ).

      SELECT SINGLE matnr
                    charg
                    b~ATWRT as to_aufnr
      INTO CORRESPONDING FIELDS OF <fs_zsdt008>
      FROM mch1 JOIN ausp as a
        ON mch1~cuobj_bm = a~objek
                JOIN ausp as b
        on mch1~CUOBJ_BM = b~OBJEK
      WHERE a~atwrt = <fs_zsdt008>-tolotno
        AND a~atinn = batch_atinn
        AND b~atinn = sap_aufnr_atinn.

      SELECT SINGLE b~atwrt
      INTO <fs_zsdt008>-from_aufnr
      FROM mch1 JOIN ausp AS a
        ON mch1~cuobj_bm = a~objek
                JOIN ausp AS b
        ON mch1~cuobj_bm = b~objek
      WHERE a~atwrt = <fs_zsdt008>-frommono
        AND a~atinn = mes_aufnr_atinn
        AND b~atinn = sap_aufnr_atinn.
    ENDLOOP.
  ENDIF.

  IF gt_zsdt008 IS NOT INITIAL.
    MODIFY zsdt008 FROM TABLE gt_zsdt008.
    COMMIT WORK AND WAIT.
  ENDIF.


ENDFORM.