FUNCTION ZFI_DIGTAL_LOWER_TO_UPPER.
*"----------------------------------------------------------------------
*"*"���ؽӿڣ�
*"  IMPORTING
*"     REFERENCE(DIGITALIN) TYPE  C
*"  EXPORTING
*"     REFERENCE(DIGITALOUT) TYPE  C
*"----------------------------------------------------------------------


 IF digitalin = '0'.
    digitalout = '��'.
  ELSEIF digitalin = '1'.
    digitalout = 'Ҽ'.
  ELSEIF digitalin = '2'.
    digitalout = '��'.
  ELSEIF digitalin = '3'.
    digitalout = '��'.
  ELSEIF digitalin = '4'.
    digitalout = '��'.
  ELSEIF digitalin = '5'.
    digitalout = '��'.
  ELSEIF digitalin = '6'.
    digitalout = '½'.
  ELSEIF digitalin = '7'.
    digitalout = '��'.
  ELSEIF digitalin = '8'.
    digitalout = '��'.
  ELSEIF digitalin = '9'.
    digitalout = '��'.
  ELSE.
    digitalout = '��'.
  ENDIF.




ENDFUNCTION.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
