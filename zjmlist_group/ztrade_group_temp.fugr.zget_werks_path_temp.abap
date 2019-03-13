FUNCTION ZGET_WERKS_PATH_TEMP .
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(I_WERKS) TYPE  WERKS_D
*"     REFERENCE(I_FWERK) TYPE  CHAR4 OPTIONAL
*"     VALUE(I_PDATE) TYPE  SY-DATUM OPTIONAL
*"     REFERENCE(I_PATHN) TYPE  CHAR10 OPTIONAL
*"     REFERENCE(I_TYPE) TYPE  CHAR1 DEFAULT '1'
*"  EXPORTING
*"     REFERENCE(RTYPE) TYPE  CHAR1
*"     REFERENCE(RTMSG) TYPE  BAPI_MSG
*"  TABLES
*"      OT_JM_PATH STRUCTURE  ZLSJM_PATH OPTIONAL
*"--------------------------------------------------------------------

  DATA lt_trade_werks LIKE TABLE OF ztrade_werks WITH HEADER LINE.
  DATA temp_pathn TYPE char10.

  IF i_pathn <> space.
    temp_pathn = i_pathn.
  ELSE.

    IF i_pdate = '00000000'.
      i_pdate = sy-datum.
    ENDIF.

    SELECT a~* INTO TABLE @lt_trade_werks
      FROM ztrade_werks AS a
      INNER JOIN ztrade_path_h AS b ON a~pathn = b~pathn
      WHERE a~werks = @i_werks
        AND b~fwerk = @i_fwerk
        AND a~bedat <= @i_pdate
        AND a~endat >= @i_pdate
        AND ( a~nfdat = '00000000' OR a~nfdat >= @i_pdate ).

    IF lines( lt_trade_werks[] ) > 1.
      rtype = 'E'.
      rtmsg = |门店 { i_werks } 存在多个发货可用路径 |.
      RETURN.
    ELSEIF lines( lt_trade_werks[] ) = 0.
      rtype = 'E'.
      rtmsg = |门店 { i_werks } 不存在发货可用路径 |.
      RETURN.
    ENDIF.

    READ TABLE lt_trade_werks INDEX 1.
    temp_pathn = lt_trade_werks-pathn.
  ENDIF.

  SELECT SINGLE betrp INTO @DATA(temp_betrp) FROM wrf1 WHERE locnr = @i_werks.
  IF temp_betrp = 'Z003' OR temp_betrp = 'Z004'.
    PERFORM handle_jm_path TABLES ot_jm_path USING i_type temp_pathn i_werks.
  ENDIF.

  IF ot_jm_path[] IS INITIAL.
    rtype = 'E'.
    rtmsg = |路径 { lt_trade_werks-pathn } 存在问题，请核查|.
  ENDIF.

ENDFUNCTION.
