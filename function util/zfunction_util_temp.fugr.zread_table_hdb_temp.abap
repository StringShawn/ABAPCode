FUNCTION ZREAD_TABLE_HDB_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     VALUE(SQL_STR) TYPE  STRING
*"     VALUE(PASSWORD) TYPE  CHAR32
*"     VALUE(MAX_LINE) TYPE  I OPTIONAL
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"  TABLES
*"      DATA STRUCTURE  TAB512 OPTIONAL
*"--------------------------------------------------------------------
  zfmparavalsave1 'ZREAD_TABLE_HDB'.
  zfmparavalsave2 'B'.

  DATA cl_hdb TYPE REF TO cl_hdb_sql_executor.

  DATA db6_system TYPE REF TO cl_db6_sys.

  DATA lr_result_descr TYPE REF TO cl_abap_structdescr.

  DATA lr_result TYPE REF TO data.

  DATA: l_offset TYPE i.

***检查外部调用

  DATA: l_select     TYPE string,
        l_select1    TYPE string,
        l_sel_field1 TYPE string,
        l_sel_field2 TYPE string,
        l_select2    TYPE string,
        l_sql_str    TYPE string,
        l_string     TYPE string.

  l_sql_str = sql_str.
  CONDENSE l_sql_str NO-GAPS.
  TRANSLATE l_sql_str TO UPPER CASE.

  SEARCH l_sql_str FOR 'UNIONALL'.
  IF sy-subrc = 0.
    SPLIT l_sql_str AT 'UNIONALL' INTO l_select1 l_select2.
    SPLIT l_select1 AT 'WHERE' INTO l_sel_field1 l_string.
    SPLIT l_select2 AT 'WHERE' INTO l_sel_field2 l_string.
    l_select = l_sel_field1 && l_sel_field2.
  ELSE.
    SPLIT l_sql_str AT 'WHERE' INTO l_select l_string.
  ENDIF.

  CONDENSE l_select NO-GAPS.

  SELECT *
    INTO TABLE @DATA(lt_interface_sql)
    FROM zinterface_sql
    WHERE password = @password.
  IF sy-subrc = 0.
    READ TABLE lt_interface_sql INTO DATA(ls_interface_sql) INDEX 1.
  ENDIF.

  CONDENSE ls_interface_sql-select_condition NO-GAPS.
  TRANSLATE ls_interface_sql-select_condition TO UPPER CASE.

  IF ls_interface_sql-select_condition <> l_select.
    rtype = 'E'.
    rtmsg = '没有查询权限'.
    zfmparavalsave2 'R'.
    EXIT.
  ENDIF.

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
      zfmparavalsave2 'R'.
      RETURN.
    CATCH cx_dba_root INTO DATA(lr_cxdba).
      rtype = 'E'.
      rtmsg = lr_cxdba->get_text( ).
      zfmparavalsave2 'R'.
      RETURN.
  ENDTRY.

  FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.
  ASSIGN lr_result->* TO <lt_table>.

  LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_table>) .

    CLEAR data.

    LOOP AT lr_result_descr->components INTO DATA(ls_components).

      IF ls_components-type_kind EQ 'P'.
        ASSIGN COMPONENT ls_components-name OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<f_value>) TYPE ls_components-type_kind DECIMALS ls_components-decimals.
      ELSE.
        ASSIGN COMPONENT ls_components-name OF STRUCTURE <ls_table> TO <f_value> TYPE ls_components-type_kind.
      ENDIF.

      IF <f_value> IS ASSIGNED.
        data-wa = data-wa && '@' && <f_value>.
      ENDIF.

    ENDLOOP.

    APPEND data.

  ENDLOOP.

  rtype = 'S'.
  zfmparavalsave2 'R'.

*  LOOP AT lr_result_descr->components INTO ls_components.
*
*    CLEAR fields.
*    fields-fieldname = ls_components-name.
*    fields-offset = l_offset.
*    fields-length = ls_components-length.
*    fields-type = ls_components-type_kind.
*    APPEND fields.
*
*    l_offset = l_offset + ls_components-length.
*
*  ENDLOOP.
ENDFUNCTION.
