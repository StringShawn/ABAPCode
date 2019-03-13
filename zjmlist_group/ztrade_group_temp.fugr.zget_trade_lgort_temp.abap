FUNCTION ZGET_TRADE_LGORT_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(I_WERKS) TYPE  WERKS_D
*"     REFERENCE(F_WERKS) TYPE  WERKS_D DEFAULT 'A000'
*"  EXPORTING
*"     REFERENCE(WERKS_ATTR) TYPE  CHAR1
*"  TABLES
*"      OT_LGORT TYPE  TABLE
*"--------------------------------------------------------------------

  DATA like_condition TYPE string.
  DATA werks_type TYPE char1.
  DATA lt_lgort TYPE RANGE OF lgort_d WITH HEADER LINE.

  SELECT SINGLE zlhbs INTO @DATA(zlhbs) FROM t001w WHERE werks = @i_werks.

  IF zlhbs = 'X'.
    werks_type = '2'.
  ELSEIF i_werks+0(1) = 'E'.
    werks_type = '3'.
  ELSE.
    werks_type = '1'.
  ENDIF.

  like_condition = '%' && werks_type && '%'.

  SELECT 'I' AS sign,'EQ' AS option, lgort AS low INTO TABLE @lt_lgort
    FROM ztrade_lgort
    WHERE ltype = '1' AND werks = @f_werks AND slare LIKE @like_condition.

  ot_lgort[] = lt_lgort[].

  werks_attr = werks_type.

ENDFUNCTION.
