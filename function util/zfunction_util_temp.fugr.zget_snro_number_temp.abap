FUNCTION ZGET_SNRO_NUMBER_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(OBJECT) TYPE  INRI-OBJECT
*"     REFERENCE(NR_RANGE_NR) TYPE  INRI-NRRANGENR
*"  EXPORTING
*"     REFERENCE(O_TYPE) TYPE  CHAR1
*"     REFERENCE(O_TMSG)
*"     REFERENCE(NUMBER)
*"--------------------------------------------------------------------

  DATA temp_subrc TYPE c.

  DO 3 TIMES.
    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = object
      EXCEPTIONS
        forgein_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.

    IF sy-subrc = 0.
      EXIT.
    ELSE.
      temp_subrc = sy-subrc.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.

  IF temp_subrc = 0 .
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = nr_range_nr
        object                  = object
      IMPORTING
        number                  = number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    temp_subrc = sy-subrc.

    CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
      EXPORTING
        object           = object
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    IF temp_subrc <> 0.
      o_type = 'E'.
      o_tmsg = '生成编码失败'.
      RETURN.
    ENDIF.

  ELSE.
    o_type = 'E'.
    o_tmsg = '编码被锁定，请稍后再试'.
  ENDIF.

ENDFUNCTION.
