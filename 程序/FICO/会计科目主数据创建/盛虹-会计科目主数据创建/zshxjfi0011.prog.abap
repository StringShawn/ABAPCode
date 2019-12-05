**************************************************
*程序名称:会计科目主数据创建
*创建日期: 2019-11-20
*创建者:XXX
*申请者:XXX
*功能描述:
*============================================
*变更记录
*修改请求号    修改日期    修改人   修改描述
* DEVK912017    2019-11-20   HANDYXH    创建程序
***************************************************
REPORT zshxjfi0011.

*------------------------------------------------------------*
* Program ID/Name:       ZFICOINF_0002
* Author's name:          沈炜
* Date written:           20180619
* Last update:            YYYYMMDD
* Program title:          总账会计科目主数据创建接口
*------------------------修改日志-----------------------------*
* Date            Userid             Reason/Description of Change
*  YYYYMMDD      ****               增加*** 显示字段
*  YYYYMMDD      ****               修改********
*                                                                        *
*------------------------------------------------------------*
*------------------------------------------------------------*
*   数据声明 － 开始
*------------------------------------------------------------*

*------------------------------------------------------------*
*  DESC:INCLUDES文件                                         *
*------------------------------------------------------------*

*------------------------------------------------------------*
*   DESC: 表/ 结构 / 视图的声明                              *
*------------------------------------------------------------*
TABLES:ska1,si_ska1,skat.

*------------------------------------------------------------*
* DESC:定义所需要的内表                                      *
*------------------------------------------------------------*
DATA: wa_ztmdm TYPE ztfi_ska1_log,
      wa_ska1  LIKE  ska1,
      wa_ska2  LIKE ska1,
      wa_skat  LIKE  skat.
*------------------------------------------------------------*
*   DESC: 定义结构和工作区                                   *
*------------------------------------------------------------*
DATA:t_ska12 TYPE TABLE OF ska1,
     t_ska1  LIKE ska1,

     t_skat  LIKE skat,
     t_ztmdm TYPE TABLE OF ztfi_ska1_log.

*------------------------------------------------------------*
*    DESC: 定义变/常量                                       *
*------------------------------------------------------------*

*------------------------------------------------------------*
*    数据定义 － 结束                                        *
*------------------------------------------------------------*

*------------------------------------------------------------*
*   DESC: 定义选择屏幕                                       *
*------------------------------------------------------------*

*------------------------------------------------------------*
*    DESC: 定义选择屏幕的PBO                                 *
*------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*------------------------------------------------------------*
*    DESC: INITIALIZATION事件                                *
*------------------------------------------------------------*
INITIALIZATION.
*------------------------------------------------------------*
*    DESC:START-OF-SELECTION 事件                            *
*------------------------------------------------------------*
START-OF-SELECTION.
*获取数据
  PERFORM frm_get_data.
*处理数据
  PERFORM frm_process_data.
*------------------------------------------------------------*
*   DESC: END-OF-SELECTION  事件                             *
*------------------------------------------------------------*
END-OF-SELECTION.

*------------------------------------------------------------*
*   DESC: TOP-OF-PAGE  事件                                  *
*------------------------------------------------------------*
TOP-OF-PAGE.
*------------------------------------------------------------*
*   DESC: END-OF-PAGE 事件                                   *
*------------------------------------------------------------*
END-OF-PAGE.
*--------------------------------------------------------------*
*  NAME: 子函数名称                    FRM_GET_DATA             *
*  DESC: 子函数功能描述                  获取数据                *
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
*  NAME: 子函数名称                    FRM_PROCESS_DATA         *
*  DESC: 子函数功能描述                  处理数据               *
*--------------------------------------------------------------*

"定义BAPI参数
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

    ls_glaccount_coa_key-ktopl = wa_ztmdm-ktopl.      "帐目表
    ls_glaccount_coa_key-saknr = wa_ztmdm-saknr.      "总账科目编号

    IF wa_ztmdm-xbilk IS INITIAL.
      ls_glaccount_coa_data-gvtyp = 'X'.                    "损益报表科目类型
    ELSE.
      ls_glaccount_coa_data-xbilk = wa_ztmdm-xbilk.         "总账科目类型
    ENDIF.
    ls_glaccount_coa_data-ktoks = wa_ztmdm-ktoks.         "总账科目组


    ls_glaccount_coa_info-erdat = sy-datum.               "记录创建日期
    ls_glaccount_coa_info-ernam = sy-uname.               "记录创建人员

    ls_glaccount_coa-keyy   = ls_glaccount_coa_key.
    ls_glaccount_coa-data   = ls_glaccount_coa_data.
    ls_glaccount_coa-info   = ls_glaccount_coa_info.
    IF wa_ztmdm-status = '1'.
      ls_glaccount_coa-action = 'I'.                      "I 是更新
    ELSEIF wa_ztmdm-status = '2'.
      ls_glaccount_coa-action = 'U'.                      "U 是修改
    ENDIF.

    ls_glaccount_name_key-ktopl = wa_ztmdm-ktopl.         "帐目表
    ls_glaccount_name_key-saknr = wa_ztmdm-saknr.         "总账科目编号
    ls_glaccount_name_key-spras = '1'.                    "语言

    ls_glaccount_name_data-txt20 = wa_ztmdm-txt20.        "短文本
    ls_glaccount_name_data-txt50 = wa_ztmdm-txt50.        "长文本

    ls_glaccount_name_table-keyy = ls_glaccount_name_key.
    ls_glaccount_name_table-data = ls_glaccount_name_data.
    IF wa_ztmdm-status = '1'.
      ls_glaccount_name_table-action = 'I'.               "I 是更新
    ELSEIF wa_ztmdm-status = '2'.
      ls_glaccount_name_table-action = 'U'.               "U 是修改
    ENDIF.
    APPEND ls_glaccount_name_table TO lt_glaccount_name_table.
    CLEAR ls_glaccount_name_table.

    "创建总账科目BAPI
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
        wa_ztmdm-message = '创建成功'.
      ELSEIF wa_ztmdm-status = '2'.
        wa_ztmdm-cjzt = 'S'.
        wa_ztmdm-message = '修改成功'.
      ENDIF.
    ENDIF.

    MODIFY ztfi_ska1_log FROM wa_ztmdm.
    CLEAR wa_ztmdm.
  ENDLOOP.
ENDFORM.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.3 - E.G.Mellodew. 1998-2019. Sap Release 740
