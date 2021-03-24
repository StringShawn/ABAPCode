    DATA:lv_date TYPE d.
    IF i_fromdate > i_todate.
      e_days = 0.
    ELSE.
      lv_date = i_fromdate.
      WHILE lv_date NE i_todate.
        ADD 1 TO e_days.
        CALL FUNCTION 'BKK_ADD_WORKINGDAY'
          EXPORTING
            i_date      = i_fromdate
            i_days      = e_days
            i_calendar1 = 'ZM'
          IMPORTING
            e_date      = lv_date.

      ENDWHILE.
    ENDIF.
