REPORT zsmdi0005.

TABLES: zsdt004.

DATA:gt_zsdt004 TYPE TABLE OF zsdt004,
     gs_zsdt004 TYPE zsdt004.


DATA : v_con           TYPE char30, " value 'TESTDB',"数据库连接字符串
       v_errorstr(250) TYPE c,
       exec_ref        TYPE REF TO cx_sy_native_sql_error. "定义Native SQL Error
DATA: l_sigh(1).

*&---------------------------------------------------------------------*
*       选择屏幕
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK sd01 WITH FRAME TITLE text-001 .
PARAMETERS: p_vkorg TYPE vbak-vkorg DEFAULT 'YZ01'.
PARAMETERS: p_datum TYPE sy-datum.
SELECTION-SCREEN END OF BLOCK sd01 .

START-OF-SELECTION.
  PERFORM connect_sql.
  PERFORM select_data.
  PERFORM disconnect_sql.
  PERFORM insert_database.


FORM connect_sql .
  SELECT SINGLE con_name INTO v_con
      FROM zpp_connect_mes
     WHERE zgroup = '01'
  AND sysid = sy-sysid  AND werks = p_vkorg .
  IF v_con IS INITIAL.
    MESSAGE '请维护zpp_connect_mes表中的连接名' TYPE 'E' .
  ENDIF.

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
ENDFORM.


FORM disconnect_sql.
*  MESSAGE s001(00) WITH '传输完毕'.
  EXEC SQL.
    DISCONNECT :V_CON
  ENDEXEC.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_data .
  DATA:lv_start TYPE string.
  DATA:lv_end TYPE string.
  lv_start = |{ p_datum+0(4) }-{ p_datum+4(2) }-{ p_datum+6(2) } 00:00:00|.
  lv_end   = |{ p_datum+0(4) }-{ p_datum+4(2) }-{ p_datum+6(2) } 23:59:59|.
  EXEC SQL.
    OPEN C FOR SELECT SERVTYPE,
                      CASE ZVKORG WHEN 'YZ' THEN 'YZ01'
                                  WHEN 'HF' THEN 'HF01'
                                  ELSE ZVKORG END,
                      CASE ZVEWEG WHEN '内销' THEN '10'
                                  WHEN '外销' THEN '20'
                                  ELSE ZVEWEG END,
                      DZNO,
                      ITEMNO,
                      CONCAT('1',CUSTOMERNO),
                      to_char(SHIPPINGDATE,'yyyyMMdd'),
                      LOTNO,
                      WORKORDER,
                      SAPMONO,
                      PRODUCTNO,
                      DEVICE,
                      METHOD,
                      CLOTNO,
                      MAINSN,
                      QTY,
                      UNIT,
                      UNITPRICE,
                      AMOUNT,
                      REMARK,
                      WAFERLOTNO,
                      DNNO,
                      TPROGRAM,
                      to_char(CREATEDATE,'yyyyMMdd'),
                      CREATOR,
                      konwa
                 FROM zsdt004
                 WHERE CREATEDATE >= to_date(:lv_start,'yyyy-MM-dd HH24:mi:ss')
                   AND CREATEDATE <= to_date(:lv_end,'yyyy-MM-dd HH24:mi:ss')

  ENDEXEC.

  DO.
    TRY.
        EXEC SQL.
          FETCH NEXT C INTO :gs_zsdt004-ZYWFL,
                            :gs_zsdt004-ZVKORG,
                            :gs_zsdt004-ZVTWEG,
                            :gs_zsdt004-ZDZDH,
                            :gs_zsdt004-ZXH,
                            :gs_zsdt004-ZKUNNR,
                            :gs_zsdt004-ZZYRQ,
                            :gs_zsdt004-ZCPPH,
                            :gs_zsdt004-ZWGDH,
                            :gs_zsdt004-ZSAPGDH,
                            :gs_zsdt004-zmatnr,
                            :gs_zsdt004-ZDEVICE,
                            :gs_zsdt004-ZZC,
                            :gs_zsdt004-ZCLOT,
                            :gs_zsdt004-ZMESDN,
                            :gs_zsdt004-ZDZSL,
                            :gs_zsdt004-ZUNIT,
                            :gs_zsdt004-ZDZDJ,
                            :gs_zsdt004-ZDZZE,
                            :gs_zsdt004-ZBZ,
                            :gs_zsdt004-ZWAFER,
                            :gs_zsdt004-ZCGDN,
                            :gs_zsdt004-ZCSCS,
                            :gs_zsdt004-ZDZDRQ,
                            :gs_zsdt004-ZDZDRY,
                            :gs_zsdt004-zwaerk.
        ENDEXEC.
      CATCH cx_sy_native_sql_error INTO exec_ref.
        MESSAGE e006(zusc_opt) WITH exec_ref->get_text( ).
    ENDTRY.
    IF sy-subrc <> 0.
      EXIT.
    ELSE.
*      gs_zsdt004-zkunnr = '1' && gs_zsdt004-zkunnr.
      gs_zsdt004-ZKUNNR = |{ gs_zsdt004-ZKUNNR ALPHA = in }|.


      APPEND gs_zsdt004 TO gt_zsdt004.
      CLEAR gs_zsdt004.
    ENDIF.
  ENDDO.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INSERT_DATABASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM insert_database .
  IF gt_zsdt004 IS NOT INITIAL.
    MODIFY zsdt004 FROM TABLE gt_zsdt004.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.