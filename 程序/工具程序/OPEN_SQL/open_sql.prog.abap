*&1 定义数据库连接
CONSTANTS:
  c_dbs LIKE dbcon-con_name VALUE 'DB_CIM'.

*&2 open sql 
  DATA: sqlerr_ref TYPE REF TO cx_sql_exception.

* Connect external database
  TRY.
      EXEC SQL.
        CONNECT TO :C_DBS
      ENDEXEC.
    CATCH cx_sql_exception INTO sqlerr_ref.
      MESSAGE e018(zzz01) WITH c_dbs.
  ENDTRY.

*&2 close sql

* 关闭CIM数据库连接
  TRY.
      EXEC SQL.
        DISCONNECT :c_DBS
      ENDEXEC.
    CATCH cx_sql_exception INTO sqlerr_ref.
  ENDTRY.
  
*&3 select data
* 取出数据到游标
EXEC SQL.
  OPEN C FOR SELECT WO_ID,
                    WO_STAT
               FROM MES_RET_WO
               WHERE WO_STAT <> 'CLOS'
ENDEXEC.
DO.
  TRY.
      EXEC SQL.
        FETCH NEXT C INTO :IS_ZTPP-AUFNR,
                          :IS_ZTPP-ZFLAG.
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO sqlnativ_ref.
      MESSAGE e018(zzz01) WITH c_dbs.
  ENDTRY.
  IF sy-subrc <> 0.
    EXIT.
  ELSE.
    is_ztpp-aufnr = |{ is_ztpp-aufnr ALPHA = IN }|.
    APPEND is_ztpp TO it_ztpp.
    CLEAR is_ztpp.
  ENDIF.
ENDDO.

*&4 update database 
TRY.
        EXEC SQL.
          begin
           UPDATE	ZCIMPRODSPEC SET FACTORYNAME = TRIM(:C_CIM01_LOG-ZFACTR),
                                   PRODUCTSPECVERSION = '',
                                   DESCRIPTION = trim(:C_CIM01_LOG-ZMAKTX),
                                   PRODUCTIONTYPE = trim(:P_ZPRODT),
                                   ROOTPRODUCTSPEC = trim(:P_PTYPE),
                                   ADDITIONALDESCRIPTION = trim(:C_CIM01_LOG-ZGROES),
                                   ZGLASSTYPE = trim(:P_ZGLASSTYPE),
                                   SYSNAME = trim(:P_TXT2),
                                   CUS_NAME = trim(:P_CUSTNAME),
                                   CUS_GRADE = trim(:P_CUSTGRADE)
            WHERE PRODUCTSPECNAME = trim(:P_MATNR);
              IF SQL%FOUND THEN
                COMMIT;
              ELSE
                ROLLBACK;
              END IF;
           END;
        ENDEXEC.
      CATCH cx_sy_native_sql_error INTO exc_ref.
        c_cim01_log-msgty = 'E'.
        c_cim01_log-msgtx = exc_ref->get_text( ).
      CATCH cx_sql_exception INTO sqlerr_ref.
        IF sqlerr_ref->db_error = 'X'.
          c_cim01_log-msgty = 'E'.
          c_cim01_log-msgtx = sqlerr_ref->sql_message.
        ELSE.
          c_cim01_log-msgty = 'E'.
          c_cim01_log-msgtx = sqlerr_ref->internal_error.
        ENDIF.
    ENDTRY.
    
*&5 insert database
 TRY.
        EXEC SQL.
          begin
            INSERT INTO	ZCIMPRODSPEC (
                                      FACTORYNAME,
                                      PRODUCTSPECNAME,
                                      PRODUCTSPECVERSION,
                                      DESCRIPTION,
                                      PRODUCTIONTYPE,
                                      ROOTPRODUCTSPEC,
                                      ADDITIONALDESCRIPTION,
                                      ZGLASSTYPE,
                                      SYSNAME,
                                      CUS_NAME,
                                      CUS_GRADE
            )
                        values (    trim(:C_CIM01_LOG-ZFACTR),
                                    trim(:P_MATNR),
                                    '',
                                    trim(:C_CIM01_LOG-ZMAKTX),
                                    trim(:P_ZPRODT),
                                    trim(:P_PTYPE),
                                    trim(:C_CIM01_LOG-ZGROES),
                                    trim(:P_ZGLASSTYPE),
                                    trim(:P_TXT2),
                                    trim(:P_CUSTNAME),
                                    trim(:P_CUSTGRADE)
                                     );
              IF SQL%FOUND THEN
                COMMIT;
              ELSE
                ROLLBACK;
              END IF;
           END;
        ENDEXEC.
      CATCH cx_sy_native_sql_error INTO exc_ref.
        c_cim01_log-msgty = 'E'.
        c_cim01_log-msgtx = exc_ref->get_text( ).
      CATCH cx_sql_exception INTO sqlerr_ref.
        IF sqlerr_ref->db_error = 'X'.
          c_cim01_log-msgty = 'E'.
          c_cim01_log-msgtx = sqlerr_ref->sql_message.
        ELSE.
          c_cim01_log-msgty = 'E'.
          c_cim01_log-msgtx = sqlerr_ref->internal_error.
        ENDIF.
    ENDTRY.

*&6 delete database 
  TRY.
      EXEC SQL.
        begin
            DELETE FROM ZCIMPRODSPEC;
            IF SQL%FOUND THEN
              COMMIT;
            ELSE
              ROLLBACK;
            END IF;
         END;
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      ROLLBACK WORK.
      MESSAGE e018(zzz01) WITH c_dbs.
    CATCH cx_sql_exception INTO sqlerr_ref.
      ROLLBACK WORK.
      MESSAGE e018(zzz01) WITH c_dbs.
  ENDTRY.