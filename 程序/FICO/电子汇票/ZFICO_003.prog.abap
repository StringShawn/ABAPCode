FUNCTION zfico_003.
*"----------------------------------------------------------------------
*"*"局部接口：
*"  TABLES
*"      T_INPUT STRUCTURE  ZSDIANZIHPI
*"      T_OUTPUT STRUCTURE  ZZT_DIANZIHP_LOG OPTIONAL
*"----------------------------------------------------------------------
  zfmparavalsave1.
  zfmparavalsave2 'START'.

  DATA:err_flag TYPE c,
       gv_msg   TYPE string.
  DATA:lt_log TYPE TABLE OF zzt_dianzihp_log,
       ls_log TYPE zzt_dianzihp_log.

  DATA:lt_input TYPE TABLE OF zsdianzihpi.
  DATA:lt_input_temp TYPE TABLE OF zsdianzihpi.
  DATA:lt_input_clear TYPE TABLE OF zsdianzihpi.
  DATA:lv_lines TYPE i,
       lv_lines2 TYPE i.
  CLEAR: lt_input,lt_input_clear.
  SORT t_input BY bukrs.
  lv_lines = lines( t_input[] ).
  LOOP AT t_input.
    "检查输入的数据
    CLEAR:t_output.
    PERFORM frm_check_input USING t_input CHANGING t_output-ztype t_output-zmsg.

    "检查汇票号是否重复
    MOVE-CORRESPONDING t_input TO t_output.
    lt_input_temp = t_input[].
    DELETE lt_input_temp WHERE zuonr = t_input-zuonr.
    lv_lines2 = lines( lt_input_temp[] ).
    IF lv_lines - lv_lines2 > 1.
      t_output-ztype = 'E'.
      t_output-zmsg = '汇票号重复'.
    ENDIF.
    t_output-cpudt = sy-datum.
    t_output-cputm = sy-uzeit.
    t_output-usnam = sy-uname.
    APPEND t_input TO lt_input.
    err_flag = t_output-ztype.
    APPEND t_output.
    CLEAR: t_output-ztype,t_output-zmsg.
    AT END OF bukrs.
      IF err_flag IS INITIAL.
        PERFORM frm_acc_document_post TABLES lt_input lt_log USING err_flag. "F-02 生成凭证
        IF err_flag IS INITIAL.
          APPEND LINES OF lt_input TO lt_input_clear.
        ENDIF.
        LOOP AT t_output.
          READ TABLE lt_log INTO ls_log WITH KEY zuonr = t_output-zuonr.
          IF sy-subrc = 0.
            t_output-zmsg    =   ls_log-zmsg   .
            t_output-belnr   =   ls_log-belnr  .
            t_output-ztype   =   ls_log-ztype  .
          ENDIF.
          MODIFY t_output.
        ENDLOOP.
*        APPEND LINES OF lt_log TO t_output.
        CLEAR:err_flag, lt_input,lt_log
         .
      ELSE.
        CLEAR:err_flag,lt_input.
      ENDIF.
    ENDAT.
  ENDLOOP.
  IF lt_input_clear IS NOT INITIAL.
    PERFORM frm_clear TABLES lt_input_clear t_output. "清账
  ENDIF.
*  MODIFY zzt_dianzihp_log FROM TABLE lt_log.
  COMMIT WORK AND WAIT.
  zfmparavalsave2 'END'.


ENDFUNCTION.