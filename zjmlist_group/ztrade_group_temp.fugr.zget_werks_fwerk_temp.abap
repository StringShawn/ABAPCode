FUNCTION ZGET_WERKS_FWERK_TEMP .
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(I_WERKS) TYPE  CHAR4
*"     REFERENCE(I_FWERK) TYPE  CHAR4 OPTIONAL
*"     REFERENCE(I_TYPE) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      OT_PATH STRUCTURE  ZTRADE_PATH_T
*"  EXCEPTIONS
*"      NO_PATH
*"--------------------------------------------------------------------
  DATA r_fwerk TYPE RANGE OF werks_d WITH HEADER LINE.

  IF i_type = space AND i_fwerk = space.
    RAISE no_path.
  ENDIF.

  IF i_fwerk IS NOT INITIAL.
    r_fwerk-option = 'EQ'.
    r_fwerk-sign = 'I'.
    r_fwerk-low = i_fwerk.
    APPEND r_fwerk.
  ENDIF.

  IF i_type = '1' OR i_type = space.

    SELECT b~* INTO TABLE @DATA(lt_path)
      FROM ztrade_werks AS a
      INNER JOIN ztrade_path_h AS c ON a~pathn = c~pathn
      INNER JOIN ztrade_path_t AS b ON b~pathn = c~pathn
      WHERE a~werks = @i_werks
        AND a~bedat <= @sy-datum
        AND a~endat >= @sy-datum
        AND c~fwerk IN @r_fwerk
        AND ( a~nfdat = '00000000' OR a~nfdat > @sy-datum )
        ORDER BY b~pathn, b~step.

  ELSEIF i_type = '2'.

    SELECT b~* INTO TABLE @lt_path
      FROM ztrade_werks AS a
      INNER JOIN ztrade_path_h AS c ON a~pathn = c~pathn
      INNER JOIN ztrade_path_t AS b ON c~pathn = b~pathn
      WHERE a~werks = @i_werks
        AND a~bedat <= @sy-datum
        AND a~endat >= @sy-datum
        AND c~fwerk IN @r_fwerk
        AND ( a~nfdat = '00000000' OR a~nfdat > @sy-datum )
        ORDER BY b~pathn DESCENDING, b~step DESCENDING.

  ENDIF.

  IF sy-subrc <> 0.

    RAISE no_path.

  ELSEIF i_type = '1' OR i_type = '2'.

    DATA wa_path LIKE LINE OF lt_path.
    LOOP AT lt_path INTO DATA(ls_path).
      wa_path = ls_path.
      AT NEW pathn.
        APPEND wa_path TO ot_path.
      ENDAT.
    ENDLOOP.
  ELSE.
    ot_path[] = lt_path[].
  ENDIF.

ENDFUNCTION.
