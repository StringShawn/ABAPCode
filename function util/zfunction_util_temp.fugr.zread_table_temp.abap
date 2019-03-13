FUNCTION ZREAD_TABLE_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     VALUE(TABNAME) TYPE  TABNAME
*"     VALUE(SELECT_CONDITION) TYPE  STRING OPTIONAL
*"     VALUE(WHERE_CONDITION) TYPE  STRING OPTIONAL
*"     VALUE(DISTINCT_FLAG) TYPE  CHAR1 OPTIONAL
*"     VALUE(MAX_LINE) TYPE  I OPTIONAL
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"  TABLES
*"      FIELDS STRUCTURE  RFC_DB_FLD OPTIONAL
*"      DATA STRUCTURE  TAB512 OPTIONAL
*"--------------------------------------------------------------------
  FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.

  DATA all_data_flag TYPE c.

  DATA lt_dd03l LIKE TABLE OF dd03l WITH HEADER LINE.

  DATA ref_tab TYPE REF TO data.

  DATA field_tab TYPE TABLE OF fieldname WITH HEADER LINE.

*  SELECT SINGLE select_condition
*    INTO @DATA(l_select_condition)
*    FROM zinterface_sql
*    WHERE password = @password
*      AND select_condition = @select_condition.
*  IF sy-subrc <> 0.
*    rtype = 'E'.
*    rtmsg = '没有查询权限'.
*    EXIT.
*  ENDIF.

  CHECK tabname IS NOT INITIAL.

  IF select_condition IS INITIAL.
    select_condition = ' *  '.
    all_data_flag = 'X'.
  ELSE.
    SPLIT select_condition AT space INTO TABLE field_tab[].
  ENDIF.

  CREATE DATA ref_tab TYPE TABLE OF (tabname).
  ASSIGN ref_tab->* TO <lt_table>.

  DATA sql_type TYPE char2.
  IF where_condition IS INITIAL.
    IF max_line IS INITIAL.
      IF distinct_flag <> 'X'.
        sql_type = '01'.
      ELSE.
        sql_type = '02'.
      ENDIF.
    ELSE.
      IF distinct_flag <> 'X'.
        sql_type = '03'.
      ELSE.
        sql_type = '04'.
      ENDIF.
    ENDIF.
  ELSE.
    IF max_line IS INITIAL.
      IF distinct_flag <> 'X'.
        sql_type = '05'.
      ELSE.
        sql_type = '06'.
      ENDIF.
    ELSE.
      IF distinct_flag <> 'X'.
        sql_type = '07'.
      ELSE.
        sql_type = '08'.
      ENDIF.
    ENDIF.
  ENDIF.

  CASE sql_type.
    WHEN '01'.
      SELECT (select_condition) INTO CORRESPONDING FIELDS OF TABLE <lt_table> FROM (tabname).
    WHEN '02'.
      SELECT DISTINCT (select_condition) INTO CORRESPONDING FIELDS OF TABLE <lt_table> FROM (tabname).
    WHEN '03'.
      SELECT (select_condition) INTO CORRESPONDING FIELDS OF TABLE <lt_table>
        FROM (tabname)
        UP TO max_line ROWS.
    WHEN '04'.
      SELECT DISTINCT (select_condition) INTO CORRESPONDING FIELDS OF TABLE <lt_table>
        FROM (tabname)
        UP TO max_line ROWS.
    WHEN '05'.
      SELECT (select_condition) INTO CORRESPONDING FIELDS OF TABLE <lt_table>
        FROM (tabname)
        WHERE (where_condition).
    WHEN '06'.
      SELECT DISTINCT (select_condition) INTO CORRESPONDING FIELDS OF TABLE <lt_table>
        FROM (tabname)
        WHERE (where_condition).
    WHEN '07'.
      SELECT (select_condition) INTO CORRESPONDING FIELDS OF TABLE <lt_table> UP TO max_line ROWS
        FROM (tabname)
        WHERE (where_condition).
    WHEN '08'.
      SELECT DISTINCT (select_condition) INTO CORRESPONDING FIELDS OF TABLE <lt_table>
        UP TO max_line ROWS
        FROM (tabname)
        WHERE (where_condition).
    WHEN OTHERS.
  ENDCASE.

  IF field_tab[] IS INITIAL.
    SELECT position fieldname inttype leng decimals INTO CORRESPONDING FIELDS OF TABLE lt_dd03l
      FROM dd03l
      WHERE tabname = tabname.

    SORT lt_dd03l BY position.
  ELSE.
    DATA position TYPE i.

    SELECT fieldname inttype leng decimals INTO CORRESPONDING FIELDS OF TABLE lt_dd03l
      FROM dd03l
      FOR ALL ENTRIES IN field_tab
      WHERE tabname = tabname
        AND fieldname = field_tab-table_line.
  ENDIF.

  LOOP AT field_tab.
    position = position + 1.
    READ TABLE lt_dd03l WITH KEY fieldname = field_tab.
    IF sy-subrc = 0.
      DELETE lt_dd03l INDEX sy-tabix.
      INSERT lt_dd03l INDEX position.
    ENDIF.
  ENDLOOP.

*  IF all_data_flag = 'X'.
*    <lt_data> = <lt_table>.
*  ELSE.
  LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<fs_table>).

    CLEAR data-wa.

    LOOP AT lt_dd03l.
      IF lt_dd03l-inttype = 'P'.
        ASSIGN COMPONENT lt_dd03l-fieldname OF STRUCTURE <fs_table> TO FIELD-SYMBOL(<value>)
        TYPE lt_dd03l-inttype DECIMALS lt_dd03l-decimals.
      ELSE.
        ASSIGN COMPONENT lt_dd03l-fieldname OF STRUCTURE <fs_table> TO <value>
        TYPE lt_dd03l-inttype.
      ENDIF.

      data-wa = data-wa && '@' && <value>.
    ENDLOOP.

    APPEND data.
  ENDLOOP.
*  ENDIF.

  DATA offset TYPE i.
  LOOP AT lt_dd03l.
    fields-fieldname = lt_dd03l-fieldname.
    fields-offset = offset.
    fields-length = lt_dd03l-leng.
    fields-type = lt_dd03l-inttype.
    APPEND fields.

    offset = offset + lt_dd03l-leng.
  ENDLOOP.

ENDFUNCTION.
