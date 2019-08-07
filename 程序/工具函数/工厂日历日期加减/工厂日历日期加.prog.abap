  METHOD add_factory_days.
    DATA:lv_days  LIKE i_days.
    DATA:lv_index LIKE i_days,
         lv_work  TYPE scal-indicator.

    DATA:lv_date_in  LIKE i_date,
         lv_date_out LIKE e_date.

    CLEAR:lv_days,lv_date_in,lv_date_out.
    lv_date_in = i_date.
    lv_days    = i_days.
    WHILE lv_days > 0.
      ADD 1 TO lv_index.
      CALL FUNCTION 'FIMA_DATE_CREATE'
        EXPORTING
          i_date = lv_date_in
          i_days = lv_index
        IMPORTING
          e_date = lv_date_out.

      CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
        EXPORTING
          correct_option               = '+'
          date                         = lv_date_out
          factory_calendar_id          = gc_fcalid
        IMPORTING
          workingday_indicator         = lv_work
        EXCEPTIONS
          calendar_buffer_not_loadable = 1
          correct_option_invalid       = 2
          date_after_range             = 3
          date_before_range            = 4
          date_invalid                 = 5
          factory_calendar_not_found   = 6
          OTHERS                       = 7.

      IF lv_work IS INITIAL.
        lv_days = lv_days - 1.
      ENDIF.
      CLEAR:lv_work.
    ENDWHILE.
    IF i_days = 0.
      e_date = i_Date.
    ELSE.
      e_date = lv_date_out.
    ENDIF.
  ENDMETHOD.