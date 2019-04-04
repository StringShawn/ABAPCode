
FUNCTION zmm_interface_invoice.
*"----------------------------------------------------------------------
*"*"�ֲ��ӿڣ�
*"  TABLES
*"      T_INPUT STRUCTURE  ZSINTERFACE_INVI
*"      T_OUTPUT STRUCTURE  ZSINTERFACE_INVO OPTIONAL
*"----------------------------------------------------------------------
  zfmparavalsave1 .
  zfmparavalsave2 'START'.

  refresh t_output.

  LOOP AT t_input.

    MOVE-CORRESPONDING t_input TO t_output.
    PERFORM frm_check_input USING t_input CHANGING t_output-flag t_output-msg.  "�������

    IF t_output-flag = 'X'.
      t_output-flag = 'E'.
      APPEND t_output.
      CLEAR:t_input, t_output.
      CONTINUE.
    ENDIF.

    PERFORM frm_get_main_data USING t_input .   "��ȡ����

    PERFORM frm_deal_main_data USING t_input CHANGING t_output-flag t_output-msg t_output-belnr.  "��������

    IF t_output-flag = 'X'.
      t_output-flag = 'E'.
      APPEND t_output.
      CLEAR:t_input, t_output.
      CONTINUE.

    ELSE.
      t_output-flag = 'S'.
      CONCATENATE '�����ɹ�������Ϊ��' t_output-belnr INTO t_output-msg.
    ENDIF.

    APPEND t_output.
    CLEAR:t_output,t_input.

  ENDLOOP.



  zfmparavalsave2 'END'.




ENDFUNCTION.