
DATA CT_DATA TYPE TABLE OF ZTG01_DATA.
DATA:WA_L_DATA TYPE ZTG01_DATA.
DATA:im_filename TYPE string.
 IM_FILENAME =  '/CsvUpload/3456.txt'.

  OPEN DATASET '/CsvUpload/345.txt'  FOR INPUT
                            IN text MODE ENCODING NON-UNICODE.

      IF SY-SUBRC <> 0.

  ELSE.
 BREAK-POINT.
    DO.
      CLEAR WA_L_DATA.
      READ DATASET '/CsvUpload/345.txt' INTO WA_L_DATA .
      IF SY-SUBRC = 0.
        APPEND WA_L_DATA TO CT_DATA.
        CLEAR WA_L_DATA.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.
  CLOSE DATASET  '/CsvUpload/345.txt'.

    DATA:WA_L_MSG   TYPE ZTG01_MSG.


  DATA:i_string type string,
       i_newline TYPE xstring VALUE '0A',
       I_XSTRING TYPE XSTRING.


*  OPEN DATASET IM_FILENAME FOR OUTPUT IN LEGACY TEXT MODE
*                           CODE PAGE '8400'.
    OPEN DATASET IM_FILENAME FOR OUTPUT IN BINARY MODE.
*  OPEN DATASET IM_FILENAME FOR OUTPUT IN LEGACY BINARY MODE
*                           CODE PAGE '8400'.
*                           ENCODING UTF-8 WITH BYTE-ORDER MARK
*                                          WITH WINDOWS LINEFEED.
  IF SY-SUBRC = 0.
    LOOP AT CT_DATA INTO WA_L_DATA.
      CLEAR:i_string, I_XSTRING.
      i_string = WA_L_DATA-LINE.
      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          TEXT   = i_string
        IMPORTING
          BUFFER = I_XSTRING
        EXCEPTIONS
          FAILED = 1
          OTHERS = 2.
      IF SY-SUBRC = 0.
      CONCATENATE i_xstring i_newline INTO i_xstring in BYTE MODE.
       TRANSFER I_XSTRING TO IM_FILENAME.
      ENDIF.

*      TRANSFER WA_L_DATA-LINE TO IM_FILENAME.

    ENDLOOP.
  ELSE.
*    MESSAGE E039 INTO WA_L_MSG-MSG.
*    APPEND WA_L_MSG TO CT_MSG.
  ENDIF.
  CLOSE DATASET IM_FILENAME.