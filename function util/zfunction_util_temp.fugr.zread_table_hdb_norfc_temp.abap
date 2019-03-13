FUNCTION ZREAD_TABLE_HDB_NORFC_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(SQL_STR) TYPE  STRING
*"     REFERENCE(MAX_LINE) TYPE  I OPTIONAL
*"  EXPORTING
*"     REFERENCE(RTYPE) TYPE  BAPI_MTYPE
*"     REFERENCE(RTMSG) TYPE  BAPI_MSG
*"  TABLES
*"      OT_DATA TYPE  TABLE OPTIONAL
*"--------------------------------------------------------------------

  DATA cl_hdb TYPE REF TO cl_hdb_sql_executor.

  DATA db6_system TYPE REF TO cl_db6_sys.

  DATA lr_result_descr TYPE REF TO cl_abap_structdescr.

  DATA lr_result TYPE REF TO data.

  TRY .
      db6_system = cl_db6_sys=>get_local_sys_ref( ).

    CATCH cx_db6_sys INTO DATA(lr_db6).
      rtype = 'E'.
      rtmsg = lr_db6->get_text( ).
      RETURN.
  ENDTRY.

  cl_hdb = NEW cl_hdb_sql_executor( ) .

  TRY.
      cl_hdb->exec_query_dyn( EXPORTING im_statement = sql_str
                                        im_cursor_size  = max_line
                                        im_system       = db6_system
                                        im_check        = abap_true
                              IMPORTING ex_structdescr  = lr_result_descr
                                        ex_result_ref   = lr_result ).

    CATCH cx_sql_exception INTO DATA(lr_cxsql).
      rtype = 'E'.
      rtmsg = lr_cxsql->get_text( ).
      FREE cl_hdb.
      RETURN.
    CATCH cx_dba_root INTO DATA(lr_cxdba).
      rtype = 'E'.
      rtmsg = lr_cxdba->get_text( ).
      FREE cl_hdb.
      RETURN.
  ENDTRY.

  FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.
  ASSIGN lr_result->* TO <lt_table>.

  IF <lt_table> IS ASSIGNED.
    ot_data[] = <lt_table>.
  ELSE.
    rtype = 'E'.
    rtmsg = '异常错误'.
  ENDIF.
  FREE cl_hdb.

ENDFUNCTION.
