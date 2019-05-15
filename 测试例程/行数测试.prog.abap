DATA:gt_data TYPE TABLE OF string,
     gs_data TYPE string.
READ REPORT 'ZPPR010_NEW2' INTO gt_data.
LOOP AT gt_data INTO gs_data.
  IF strlen( gs_data ) > 72.
    WRITE:/ gs_data.
  ENDIF.
ENDLOOP.