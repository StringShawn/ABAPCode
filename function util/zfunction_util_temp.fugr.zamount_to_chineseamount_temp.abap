FUNCTION ZAMOUNT_TO_CHINESEAMOUNT_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(I_AMOUNT) TYPE  HSLVT12
*"  EXPORTING
*"     REFERENCE(O_CHINESE_AMOUNT) TYPE  STRING
*"--------------------------------------------------------------------

  DATA: ipos    TYPE i,
        iwan    TYPE i,
        imod    TYPE i,
        pin     TYPE p DECIMALS 2,
        strint  TYPE c LENGTH 10,
        strdec  TYPE c LENGTH 3,
        strintt TYPE c,
        strintd TYPE c,
        strintu TYPE c LENGTH 40,
        strdect TYPE c,
        strdecd TYPE c,
        strdecu TYPE c LENGTH 4.

  CHECK i_amount >= 0.

  pin = i_amount.

  IF pin = 0.
    ipos = 0.
  ELSE.
    ipos = floor( log10( pin ) ) + 1. "计算数值pin的整数位数。
  ENDIF.
*write / ipos.
*计算小数部分
  pin = pin * 100. "将数值的百分位变为个位，便于处理。

  strdect = pin - pin DIV 10 * 10. "获取数值的百分位，也就是分

  IF strdect <> '0'. "如果值为0，则不显示该值，直接跳过。

    PERFORM change_digital_to_chinese USING strdect CHANGING strdecd.

    CONCATENATE strdecd '分' INTO strdecu. "生成金额的“分”。
  ENDIF.

  pin = pin DIV 10. "将十分位变为个位。
  strdect =  pin - pin DIV 10 * 10. "获取数值的十分位，也就是角。
  IF strdect EQ '0' AND strdecu EQ ''. "如果角为0，分也为0，则金额的小数部分就没有。
    strdecu = ''.

  ELSEIF strdect EQ '0' AND strdecu NE ''. "如果角为0，分不为0，则角位只写“零”不写“角”。

    CONCATENATE '零' strdecu INTO strdecu.
  ELSE. "如果角不为0，则直接与前面生成的分进行拼接。

    PERFORM change_digital_to_chinese USING strdect CHANGING strdecd.
    CONCATENATE strdecd '角' strdecu INTO strdecu. "生成金额的“角”和“分”
  ENDIF.
  pin = pin DIV 10. "恢复原来的整数位。
  iwan = 0. "万位标记，四个数值位为一“万”，从个位开始，所以iwan的初值为0

*计算整数部分
  DO ipos TIMES.
    strintt = pin - pin DIV 10 * 10.
    imod = iwan MOD 4.
    IF imod EQ 0.
      IF iwan = 0.
        IF strintt NE '0'.
          PERFORM change_digital_to_chinese USING strintt CHANGING strintd.
          strintu = strintd.
        ENDIF.
      ELSEIF iwan = 4.
        IF strintt NE '0'.
          PERFORM change_digital_to_chinese USING strintt CHANGING strintd.
        ELSE.
          strintd = ''.
        ENDIF.
        CONCATENATE strintd '万' strintu INTO strintu.
      ELSEIF iwan = 8.
        IF strintt NE '0'.
          PERFORM change_digital_to_chinese USING strintt CHANGING strintd.
        ELSE.
          strintd = ''.
        ENDIF.
        CONCATENATE strintd '亿' strintu INTO strintu.
      ENDIF.
    ENDIF.
    IF imod EQ 1.
      IF strintt NE '0'.
        PERFORM change_digital_to_chinese USING strintt CHANGING strintd.
        CONCATENATE strintd '拾' strintu INTO strintu.
      ELSEIF strintd NE '零'.
        CONCATENATE '零' strintu INTO strintu.
      ENDIF.
    ENDIF.
    IF imod EQ 2.
      IF strintt NE '0'.
        PERFORM change_digital_to_chinese USING strintt CHANGING strintd.
        CONCATENATE strintd '佰' strintu INTO strintu.
      ELSEIF strintd NE '零'.
        CONCATENATE '零' strintu INTO strintu.
      ENDIF.
    ENDIF.
    IF imod EQ 3.
      IF strintt NE '0'.
        PERFORM change_digital_to_chinese USING strintt CHANGING strintd.
        CONCATENATE strintd '仟' strintu INTO strintu.
      ELSEIF strintd NE '零'.
        CONCATENATE '零' strintu INTO strintu.
      ENDIF.
    ENDIF.
    pin = pin DIV 10.
    iwan = iwan + 1.

  ENDDO.

*连接整数位和小数位。
  CONCATENATE strintu '元' strdecu INTO o_chinese_amount.

*把“零亿”、“零万”、“零元”都替换掉。
  REPLACE ALL OCCURRENCES OF '零零' IN o_chinese_amount WITH '零' .
  REPLACE ALL OCCURRENCES OF '零零' IN o_chinese_amount WITH '零' .
  REPLACE ALL OCCURRENCES OF '零亿' IN o_chinese_amount WITH '亿' .
  REPLACE ALL OCCURRENCES OF '零万' IN o_chinese_amount WITH '万' .
  REPLACE ALL OCCURRENCES OF '零元' IN o_chinese_amount WITH '元' .

  IF ipos = 0.
    o_chinese_amount = '零元'.
  ENDIF.

ENDFUNCTION.
