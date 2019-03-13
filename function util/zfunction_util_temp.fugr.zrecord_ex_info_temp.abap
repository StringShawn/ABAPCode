FUNCTION ZRECORD_EX_INFO_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(LS_RECORD) LIKE  ZERECORD_LIST
*"  STRUCTURE  ZERECORD_LIST OPTIONAL
*"  TABLES
*"      LT_RECORD STRUCTURE  ZERECORD_LIST OPTIONAL
*"--------------------------------------------------------------------

  DATA lt_record_list LIKE TABLE OF zerecord_list WITH HEADER LINE.
  DATA tsl TYPE timestampl.
  DATA tsl_str TYPE char22.

  IF ls_record IS NOT INITIAL.

    APPEND ls_record TO lt_record_list.
  ENDIF.

  IF lt_record[] IS NOT INITIAL.

    APPEND LINES OF lt_record TO lt_record_list.
  ENDIF.

  LOOP AT lt_record_list.

    GET TIME STAMP FIELD tsl.

    tsl_str = tsl.

    REPLACE ALL OCCURRENCES OF '.' IN tsl_str WITH ``.

    lt_record_list-timtsl = tsl_str.

    MODIFY lt_record_list.

  ENDLOOP.

  MODIFY zerecord_list FROM TABLE lt_record_list[].
  COMMIT WORK.

  REFRESH lt_record.

ENDFUNCTION.
