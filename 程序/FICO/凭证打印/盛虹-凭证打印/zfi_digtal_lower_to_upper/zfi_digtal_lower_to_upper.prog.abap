FUNCTION ZFI_DIGTAL_LOWER_TO_UPPER.
*"----------------------------------------------------------------------
*"*"±¾µØ½Ó¿Ú£º
*"  IMPORTING
*"     REFERENCE(DIGITALIN) TYPE  C
*"  EXPORTING
*"     REFERENCE(DIGITALOUT) TYPE  C
*"----------------------------------------------------------------------


 IF digitalin = '0'.
    digitalout = 'Áã'.
  ELSEIF digitalin = '1'.
    digitalout = 'Ò¼'.
  ELSEIF digitalin = '2'.
    digitalout = '·¡'.
  ELSEIF digitalin = '3'.
    digitalout = 'Èþ'.
  ELSEIF digitalin = '4'.
    digitalout = 'ËÁ'.
  ELSEIF digitalin = '5'.
    digitalout = 'Îé'.
  ELSEIF digitalin = '6'.
    digitalout = 'Â½'.
  ELSEIF digitalin = '7'.
    digitalout = 'Æâ'.
  ELSEIF digitalin = '8'.
    digitalout = '°Æ'.
  ELSEIF digitalin = '9'.
    digitalout = '¾Á'.
  ELSE.
    digitalout = '¡Á'.
  ENDIF.




ENDFUNCTION.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
