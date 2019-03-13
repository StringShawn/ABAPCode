FUNCTION ZACTIVE_SPKEY_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(REPID) TYPE  SY-REPID
*"     REFERENCE(RGRUP) TYPE  NUMC3 DEFAULT 001
*"     REFERENCE(IGNORE) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     REFERENCE(ISUSE) TYPE  CHAR1
*"--------------------------------------------------------------------

  DATA lt_sval LIKE TABLE OF sval WITH HEADER LINE.
  DATA temp_spkey TYPE char10.

  IF ignore = space.
    lt_sval-tabname = 'ZSPEICAL_KEY'.
    lt_sval-fieldname = 'SPKEY'.
    lt_sval-fieldtext = '验证秘钥'.
    lt_sval-field_obl = 'X'.
    APPEND lt_sval.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = '请输入验证秘钥'
      TABLES
        fields          = lt_sval
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      RETURN.
    ELSEIF lt_sval[] IS NOT INITIAL AND sy-ucomm = 'FURT'.

      READ TABLE lt_sval INDEX 1.
      temp_spkey = lt_sval-value.

      SELECT SINGLE * INTO @DATA(ls_spkey) FROM zspeical_key
        WHERE repid = @repid AND spkey = @temp_spkey AND rgrup = @rgrup
          AND bedat <= @sy-datum AND endat >= @sy-datum.

      IF sy-subrc <> 0.
        MESSAGE '无效的验证' TYPE 'I'.
        RETURN.
      ELSE.
        isuse = 'X'.
      ENDIF.
    ENDIF.

  ELSE.

    SELECT SINGLE * INTO @ls_spkey FROM zspeical_key
      WHERE repid = @repid AND rgrup = @rgrup
        AND bedat <= @sy-datum AND endat >= @sy-datum.

    IF sy-subrc = 0.
      isuse = 'X'.
    ENDIF.

  ENDIF.



ENDFUNCTION.
