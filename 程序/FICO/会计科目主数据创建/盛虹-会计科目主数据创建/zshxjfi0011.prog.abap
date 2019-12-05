**************************************************
*��������:��ƿ�Ŀ�����ݴ���
*��������: 2019-11-20
*������:XXX
*������:XXX
*��������:
*============================================
*�����¼
*�޸������    �޸�����    �޸���   �޸�����
* DEVK912017    2019-11-20   HANDYXH    ��������
***************************************************
REPORT zshxjfi0011.

*------------------------------------------------------------*
* Program ID/Name:       ZFICOINF_0002
* Author's name:          ���
* Date written:           20180619
* Last update:            YYYYMMDD
* Program title:          ���˻�ƿ�Ŀ�����ݴ����ӿ�
*------------------------�޸���־-----------------------------*
* Date            Userid             Reason/Description of Change
*  YYYYMMDD      ****               ����*** ��ʾ�ֶ�
*  YYYYMMDD      ****               �޸�********
*                                                                        *
*------------------------------------------------------------*
*------------------------------------------------------------*
*   �������� �� ��ʼ
*------------------------------------------------------------*

*------------------------------------------------------------*
*  DESC:INCLUDES�ļ�                                         *
*------------------------------------------------------------*

*------------------------------------------------------------*
*   DESC: ��/ �ṹ / ��ͼ������                              *
*------------------------------------------------------------*
TABLES:ska1,si_ska1,skat.

*------------------------------------------------------------*
* DESC:��������Ҫ���ڱ�                                      *
*------------------------------------------------------------*
DATA: wa_ztmdm TYPE ztfi_ska1_log,
      wa_ska1  LIKE  ska1,
      wa_ska2  LIKE ska1,
      wa_skat  LIKE  skat.
*------------------------------------------------------------*
*   DESC: ����ṹ�͹�����                                   *
*------------------------------------------------------------*
DATA:t_ska12 TYPE TABLE OF ska1,
     t_ska1  LIKE ska1,

     t_skat  LIKE skat,
     t_ztmdm TYPE TABLE OF ztfi_ska1_log.

*------------------------------------------------------------*
*    DESC: �����/����                                       *
*------------------------------------------------------------*

*------------------------------------------------------------*
*    ���ݶ��� �� ����                                        *
*------------------------------------------------------------*

*------------------------------------------------------------*
*   DESC: ����ѡ����Ļ                                       *
*------------------------------------------------------------*

*------------------------------------------------------------*
*    DESC: ����ѡ����Ļ��PBO                                 *
*------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*------------------------------------------------------------*
*    DESC: INITIALIZATION�¼�                                *
*------------------------------------------------------------*
INITIALIZATION.
*------------------------------------------------------------*
*    DESC:START-OF-SELECTION �¼�                            *
*------------------------------------------------------------*
START-OF-SELECTION.
*��ȡ����
  PERFORM frm_get_data.
*��������
  PERFORM frm_process_data.
*------------------------------------------------------------*
*   DESC: END-OF-SELECTION  �¼�                             *
*------------------------------------------------------------*
END-OF-SELECTION.

*------------------------------------------------------------*
*   DESC: TOP-OF-PAGE  �¼�                                  *
*------------------------------------------------------------*
TOP-OF-PAGE.
*------------------------------------------------------------*
*   DESC: END-OF-PAGE �¼�                                   *
*------------------------------------------------------------*
END-OF-PAGE.
*--------------------------------------------------------------*
*  NAME: �Ӻ�������                    FRM_GET_DATA             *
*  DESC: �Ӻ�����������                  ��ȡ����                *
*--------------------------------------------------------------*

FORM frm_get_data .
  SELECT    *
    INTO CORRESPONDING FIELDS OF TABLE t_ztmdm
    FROM ztfi_ska1_log
   WHERE ( status = '1' OR status = '2')
     AND dycs < 3
     AND ( cjzt = 'E' OR cjzt = '').
*     AND hczt = ''.

ENDFORM.
*--------------------------------------------------------------*
*  NAME: �Ӻ�������                    FRM_PROCESS_DATA         *
*  DESC: �Ӻ�����������                  ��������               *
*--------------------------------------------------------------*

"����BAPI����
DATA:lt_return TYPE TABLE OF bapiret2,
     ls_return TYPE bapiret2.

DATA:ls_glaccount_coa_key  TYPE glaccount_coa_key,
     ls_glaccount_coa_data TYPE glaccount_coa_data,
     ls_glaccount_coa_info TYPE glaccount_coa_info,
     ls_glaccount_coa      TYPE glaccount_coa.

DATA:ls_glaccount_name_key  TYPE glaccount_name_key,
     ls_glaccount_name_data TYPE glaccount_name_data.
DATA:lt_glaccount_name_table TYPE TABLE OF glaccount_name,
     ls_glaccount_name_table TYPE glaccount_name.



FORM frm_process_data .
  LOOP AT t_ztmdm INTO wa_ztmdm.
    CLEAR:ls_return,ls_glaccount_coa_key,ls_glaccount_coa_data,ls_glaccount_coa_info,
          ls_glaccount_coa,ls_glaccount_name_key,ls_glaccount_name_data,ls_glaccount_name_table.
    REFRESH:lt_glaccount_name_table,lt_return.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_ztmdm-saknr
      IMPORTING
        output = wa_ztmdm-saknr.

    ls_glaccount_coa_key-ktopl = wa_ztmdm-ktopl.      "��Ŀ��
    ls_glaccount_coa_key-saknr = wa_ztmdm-saknr.      "���˿�Ŀ���

    IF wa_ztmdm-xbilk IS INITIAL.
      ls_glaccount_coa_data-gvtyp = 'X'.                    "���汨���Ŀ����
    ELSE.
      ls_glaccount_coa_data-xbilk = wa_ztmdm-xbilk.         "���˿�Ŀ����
    ENDIF.
    ls_glaccount_coa_data-ktoks = wa_ztmdm-ktoks.         "���˿�Ŀ��


    ls_glaccount_coa_info-erdat = sy-datum.               "��¼��������
    ls_glaccount_coa_info-ernam = sy-uname.               "��¼������Ա

    ls_glaccount_coa-keyy   = ls_glaccount_coa_key.
    ls_glaccount_coa-data   = ls_glaccount_coa_data.
    ls_glaccount_coa-info   = ls_glaccount_coa_info.
    IF wa_ztmdm-status = '1'.
      ls_glaccount_coa-action = 'I'.                      "I �Ǹ���
    ELSEIF wa_ztmdm-status = '2'.
      ls_glaccount_coa-action = 'U'.                      "U ���޸�
    ENDIF.

    ls_glaccount_name_key-ktopl = wa_ztmdm-ktopl.         "��Ŀ��
    ls_glaccount_name_key-saknr = wa_ztmdm-saknr.         "���˿�Ŀ���
    ls_glaccount_name_key-spras = '1'.                    "����

    ls_glaccount_name_data-txt20 = wa_ztmdm-txt20.        "���ı�
    ls_glaccount_name_data-txt50 = wa_ztmdm-txt50.        "���ı�

    ls_glaccount_name_table-keyy = ls_glaccount_name_key.
    ls_glaccount_name_table-data = ls_glaccount_name_data.
    IF wa_ztmdm-status = '1'.
      ls_glaccount_name_table-action = 'I'.               "I �Ǹ���
    ELSEIF wa_ztmdm-status = '2'.
      ls_glaccount_name_table-action = 'U'.               "U ���޸�
    ENDIF.
    APPEND ls_glaccount_name_table TO lt_glaccount_name_table.
    CLEAR ls_glaccount_name_table.

    "�������˿�ĿBAPI
    CALL FUNCTION 'GL_ACCT_MASTER_SAVE_RFC'
*     EXPORTING
*       TESTMODE            =
*       NO_SAVE_AT_WARNING  =
*       NO_AUTHORITY_CHECK  =
      CHANGING
        account_coa   = ls_glaccount_coa
        account_names = lt_glaccount_name_table
*       ACCOUNT_KEYWORDS    =
*       ACCOUNT_CCODES      =
        return        = lt_return
      EXCEPTIONS
        logon_error   = 1
        error         = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.

    READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      wa_ztmdm-dycs = wa_ztmdm-dycs + 1.
      IF wa_ztmdm-status = '1'.
        wa_ztmdm-cjzt = 'E'.
        wa_ztmdm-message = ls_return-message.
      ELSEIF wa_ztmdm-status = '2'.
        wa_ztmdm-cjzt = 'E'.
        wa_ztmdm-message = ls_return-message.
      ENDIF.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      wa_ztmdm-dycs = wa_ztmdm-dycs + 1.
*      wa_ztmdm-hczt = 'X'.
      IF wa_ztmdm-status = '1'.
        wa_ztmdm-cjzt = 'S'.
        wa_ztmdm-message = '�����ɹ�'.
      ELSEIF wa_ztmdm-status = '2'.
        wa_ztmdm-cjzt = 'S'.
        wa_ztmdm-message = '�޸ĳɹ�'.
      ENDIF.
    ENDIF.

    MODIFY ztfi_ska1_log FROM wa_ztmdm.
    CLEAR wa_ztmdm.
  ENDLOOP.
ENDFORM.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
