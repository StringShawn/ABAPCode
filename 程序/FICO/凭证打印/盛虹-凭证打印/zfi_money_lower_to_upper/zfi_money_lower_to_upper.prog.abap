FUNCTION ZFI_MONEY_LOWER_TO_UPPER.
*"----------------------------------------------------------------------
*"*"���ؽӿڣ�
*"  IMPORTING
*"     REFERENCE(MONEYIN) TYPE  STRING
*"  EXPORTING
*"     REFERENCE(MONEYOUT) TYPE  STRING
*"----------------------------------------------------------------------

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
  DATA : l_zs TYPE c .
  clear  l_zs .
  pin = moneyin.
  IF pin EQ 0.
    ipos = 0.
  ELSE.
    ipos = floor( log10( pin ) ) + 1. "������ֵpin������λ����
  ENDIF.
*write / ipos.
*����С������
  pin = pin * 100. "����ֵ�İٷ�λ��Ϊ��λ�����ڴ���
  strdect = pin - pin DIV 10 * 10. "��ȡ��ֵ�İٷ�λ��Ҳ���Ƿ֡�
  IF strdect NE '0'. "���ֵΪ0������ʾ��ֵ��ֱ��������
    CALL FUNCTION 'ZFI_DIGTAL_LOWER_TO_UPPER' "������ֵСдת��д�ĺ�������ô�д�����֡�
      EXPORTING
        digitalin  = strdect
      IMPORTING
        digitalout = strdecd.
    CONCATENATE strdecd '��' INTO strdecu. "���ɽ��ġ��֡���
  ENDIF.
  pin = pin DIV 10. "��ʮ��λ��Ϊ��λ��
  strdect =  pin - pin DIV 10 * 10. "��ȡ��ֵ��ʮ��λ��Ҳ���ǽǡ�
  IF strdect EQ '0' AND strdecu EQ ''. "�����Ϊ0����ҲΪ0�������С�����־�û�С�
    strdecu = ''.
    l_zs = '��' .
  ELSEIF strdect EQ '0' AND strdecu NE ''. "�����Ϊ0���ֲ�Ϊ0�����λֻд���㡱��д���ǡ���
    CONCATENATE '��' strdecu INTO strdecu.
  ELSE. "����ǲ�Ϊ0����ֱ����ǰ�����ɵķֽ���ƴ�ӡ�
    CALL FUNCTION 'ZFI_DIGTAL_LOWER_TO_UPPER' "������ֵСдת��д�ĺ�����
      EXPORTING
        digitalin  = strdect
      IMPORTING
        digitalout = strdecd.
    CONCATENATE strdecd '��' strdecu INTO strdecu. "���ɽ��ġ��ǡ��͡��֡�

  ENDIF.
  pin = pin DIV 10. "�ָ�ԭ��������λ��
  iwan = 0. "��λ��ǣ��ĸ���ֵλΪһ���򡱣��Ӹ�λ��ʼ������iwan�ĳ�ֵΪ0��
*������������
  DO ipos TIMES.
    strintt = pin - pin DIV 10 * 10.
    imod = iwan MOD 4.
    IF imod EQ 0.
      IF iwan = 0.
        IF strintt NE '0'.
          CALL FUNCTION 'ZFI_DIGTAL_LOWER_TO_UPPER'
            EXPORTING
              digitalin  = strintt
            IMPORTING
              digitalout = strintd.
          strintu = strintd.
        ENDIF.
      ELSEIF iwan = 4.
        IF strintt NE '0'.
          CALL FUNCTION 'ZFI_DIGTAL_LOWER_TO_UPPER'
            EXPORTING
              digitalin  = strintt
            IMPORTING
              digitalout = strintd.
        ELSE.
          strintd = ''.
        ENDIF.
        CONCATENATE strintd '��' strintu INTO strintu.
      ELSEIF iwan = 8.
        IF strintt NE '0'.
          CALL FUNCTION 'ZFI_DIGTAL_LOWER_TO_UPPER'
            EXPORTING
              digitalin  = strintt
            IMPORTING
              digitalout = strintd.
        ELSE.
          strintd = ''.
        ENDIF.
        CONCATENATE strintd '��' strintu INTO strintu.
      ENDIF.
    ENDIF.
    IF imod EQ 1.
      IF strintt NE '0'.
        CALL FUNCTION 'ZFI_DIGTAL_LOWER_TO_UPPER'
          EXPORTING
            digitalin  = strintt
          IMPORTING
            digitalout = strintd.
        CONCATENATE strintd 'ʰ' strintu INTO strintu.
      ELSEIF strintd NE '��'.
        CONCATENATE '��' strintu INTO strintu.
      ENDIF.
    ENDIF.
    IF imod EQ 2.
      IF strintt NE '0'.
        CALL FUNCTION 'ZFI_DIGTAL_LOWER_TO_UPPER'
          EXPORTING
            digitalin  = strintt
          IMPORTING
            digitalout = strintd.
        CONCATENATE strintd '��' strintu INTO strintu.
      ELSEIF strintd NE '��'.
        CONCATENATE '��' strintu INTO strintu.
      ENDIF.
    ENDIF.
    IF imod EQ 3.
      IF strintt NE '0'.
        CALL FUNCTION 'ZFI_DIGTAL_LOWER_TO_UPPER'
          EXPORTING
            digitalin  = strintt
          IMPORTING
            digitalout = strintd.
        CONCATENATE strintd 'Ǫ' strintu INTO strintu.
      ELSEIF strintd NE '��'.
        CONCATENATE '��' strintu INTO strintu.
      ENDIF.
    ENDIF.
    pin = pin DIV 10.
    iwan = iwan + 1.
*    write: / iwan, strintt, strintu, imod.
  ENDDO.
*��������λ��С��λ��
  CONCATENATE strintu 'Բ' strdecu INTO moneyout.
*�ѡ����ڡ��������򡱡�����Բ�����滻����
  REPLACE ALL OCCURRENCES OF '����' IN moneyout WITH '��' .
  REPLACE ALL OCCURRENCES OF '����' IN moneyout WITH '��' .
  REPLACE ALL OCCURRENCES OF '����' IN moneyout WITH '��' .
  REPLACE ALL OCCURRENCES OF '����' IN moneyout WITH '��' .
  REPLACE ALL OCCURRENCES OF '��Բ' IN moneyout WITH 'Բ' .
  IF ipos EQ 0.
    moneyout = '��Բ'.
  ENDIF.

  IF l_zs IS NOT INITIAL .
    CONCATENATE MONEYOUT l_zs  INTO MONEYOUT .
    CONDENSE MONEYOUT NO-GAPS .
  ENDIF.





ENDFUNCTION.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
