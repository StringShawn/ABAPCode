FUNCTION zconvert_date_to_internal.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(DATE_EXTERNAL)
*"  EXPORTING
*"     VALUE(DATE_INTERNAL)
*"----------------------------------------------------------------------

  DATA:lv_year  TYPE string,
       lv_month TYPE string,
       lv_day   TYPE string.

  DATA:lv_separated TYPE string.

  IF date_external CA '.'.
    lv_separated = '.'.
  ELSEIF date_external CA '/'.
    lv_separated = '/'.
  ELSEIF date_external CA '-'.
    lv_separated = '-'.
  ENDIF.
  IF lv_separated IS NOT INITIAL.
    SPLIT date_external AT lv_separated INTO lv_year lv_month lv_day.
    CONCATENATE lv_year lv_month lv_day INTO date_internal.
  ELSE.
    date_internal = date_external.
  ENDIF.




ENDFUNCTION.