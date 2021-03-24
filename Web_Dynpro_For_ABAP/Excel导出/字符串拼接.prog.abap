  DATA:LO_NODE      TYPE REF TO IF_WD_CONTEXT_NODE,
       LO_ELEM      TYPE REF TO IF_WD_CONTEXT_ELEMENT,
       LS_CONTEXT   TYPE WD_THIS->ELEMENT_YFZK,
       LT_CONTEXT   TYPE WD_THIS->ELEMENTS_YFZK,
       LV_STRING    TYPE STRING,
       LV_XSTRING   TYPE XSTRING,
       LV_JE1       TYPE STRING,
       LV_JE2       TYPE STRING,
       LV_JE3       TYPE STRING,
       LV_ENCODING  TYPE ABAP_ENCODING VALUE '8404'.
* get the table's context
  LO_NODE = WD_CONTEXT->GET_CHILD_NODE( NAME = 'YFZK' ).
  LO_NODE->GET_STATIC_ATTRIBUTES_TABLE( IMPORTING  TABLE = LT_CONTEXT ).
  IF LT_CONTEXT IS INITIAL.
    ZCL_WD_MESSAGE=>SHOW_MSG_DETAIL(
     EXPORTING
       IV_TYPE       = 'E'
       IV_MSGID      = 'ZTR01'
       IV_MSGNO      = '000'
       IV_MSGTY      = 'S'
       IV_MSGV1      = '无可导出数据'
       IR_CONTROLLER =  WD_COMP_CONTROLLER->WD_GET_API( ) " "
       ).
     RETURN.
  ENDIF.

* create the String(Line)
  LOOP AT LT_CONTEXT INTO LS_CONTEXT.
   AT FIRST.
    CONCATENATE LV_STRING
                '年度'
*                '申请单号'
                '凭证编号'
                '行项目'
                '凭证类型'
                '过帐日期'
                '供应商'
                 '名称'
                '货币'
                '凭证金额'
                '已支付金额'
                '剩余可支付金额'
                '到期日 '
                '付款条件'
                '付款方式'
                '行项目文本'
               CL_ABAP_CHAR_UTILITIES=>NEWLINE
          INTO LV_STRING
         SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
   ENDAT.
   LV_JE1 = LS_CONTEXT-WRBTR.
   LV_JE2 = LS_CONTEXT-ZPAID.
   LV_JE3 = LS_CONTEXT-ZLEFT.
   CONCATENATE LV_STRING
               LS_CONTEXT-GJAHR
               LS_CONTEXT-BELNR
               LS_CONTEXT-BUZEI
               LS_CONTEXT-BLART
               LS_CONTEXT-BUDAT
               LS_CONTEXT-LIFNR
               LS_CONTEXT-NAME1
               LS_CONTEXT-WAERS
               LV_JE1
               LV_JE2
               LV_JE3
               LS_CONTEXT-ZFBDT
               LS_CONTEXT-ZLSCH
               LS_CONTEXT-ZBFIX
               LS_CONTEXT-SGTXT
               CL_ABAP_CHAR_UTILITIES=>NEWLINE
          INTO LV_STRING
         SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
  ENDLOOP.
* convert the string => Xstring
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      TEXT           = LV_STRING
*      MIMETYPE       = ' '
      ENCODING       = LV_ENCODING
    IMPORTING
      BUFFER         = LV_XSTRING
    EXCEPTIONS
      FAILED         = 1
      OTHERS         = 2 .

* export the XString to Excel
  WDR_TASK=>CLIENT_WINDOW->CLIENT->ATTACH_FILE_TO_RESPONSE(
**path to the word file
    I_FILENAME = 'TEST.xls'
** String Variable
    I_CONTENT =  LV_XSTRING
** File Type
    I_MIME_TYPE = 'EXCEL' ).
