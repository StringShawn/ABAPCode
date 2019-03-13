FUNCTION ZCHECK_WERKS_BHNUC_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(I_BHNUC) TYPE  CHAR10
*"  EXCEPTIONS
*"      NO_EFFECT
*"--------------------------------------------------------------------


  SELECT SINGLE b~fwerk INTO @DATA(temp_werks)
    FROM ztrade_werks AS a
    INNER JOIN ztrade_path_h AS b ON a~pathn = b~pathn
    INNER JOIN t001w AS c ON b~fwerk = c~werks
    INNER JOIN zsd_jmfh_h AS d ON d~fwerk = b~fwerk AND d~werks = a~werks
    WHERE a~bedat <= @sy-datum
      AND a~endat >= @sy-datum
      AND d~bhnuc = @i_bhnuc
      AND ( a~nfdat = '00000000' OR nfdat > @sy-datum ).

  IF sy-subrc <> 0.
    RAISE no_effect.
  ENDIF.

ENDFUNCTION.
