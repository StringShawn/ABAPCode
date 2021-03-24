**********************************************************************
* 模块：       Web dynpro 公共类
* 作者：       yuanfeng
* 创建日期：   2017-11-15
* 描述：       ALV控件,上下文节点，ALV下拉等的封装
* From Project Vanke
**********************************************************************/
class ZCL_WD_COMMON definition
  public
  inheriting from CL_WD_COMPONENT_ASSISTANCE
  create public .

public section.

  types:
    begin of t_check_result_message,
      t100_message      type symsg,
      context_element   type ref to if_wd_context_element,
      attribute_name    type string,
    end of t_check_result_message .
  types:
    t_check_result_message_tab type standard table of t_check_result_message .

  class-data GR_ALV type ref to CL_SALV_WD_CONFIG_TABLE .
  class-data GV_WIDTH type CHAR10 value '142px' ##NO_TEXT.
  class-data GR_COLUMNS type SALV_WD_T_COLUMN_REF .
  class-data GV_WIDTH_F4 type CHAR10 value '120px' ##NO_TEXT.
  constants CV_WIDTH type STRING value '1072' ##NO_TEXT.
  constants CV_HEIGHT type STRING value '600' ##NO_TEXT.
  class-data GV_COUNT type INT1 value 10 ##NO_TEXT.
  constants CV_TABLE_WIDTH type STRING value '100%' ##NO_TEXT.

  class-methods INIT_ALV_STANDARD
    importing
      !II_CONTROLLER type ref to IWCI_SALV_WD_TABLE
      !IS_HEAD type ZABS0001 .
  class-methods SET_ALV_COLUMN_ATTRIBUTE
    importing
      value(IR_ALV_CONFIG_TABLE) type ref to CL_SALV_WD_CONFIG_TABLE
      !IV_CHECKBOX type STRING optional
      !IV_HEADER type STRING optional
      value(IV_NAME) type STRING
      !IV_POSITION type I optional
      !IV_RESIZABLE type WDY_BOOLEAN optional
      !IV_VISIBLE type WDY_UIE_LIBRARY_ENUM_TYPE optional
      !IV_WIDTH type STRING optional
      !IV_ALIGN type WDUI_TABLE_COLUMN_HALIGN optional
      !IV_BUTTON type CHAR1 optional
      !IV_LINK type CHAR1 optional
      !IV_URL type STRING optional
      !IV_INPUT type STRING optional
      !IV_DROP type STRING optional
      !IV_READONLY_NAME type STRING optional
      !IV_COLOR_NAME type STRING optional
      !IV_TEXTVIEW type STRING optional
      !IV_IMAGE type STRING optional
      !IV_READONLY type CHAR1 optional
      !IV_DROP_BY_INX type STRING optional
      !IV_SET_RESIZABLE type CHAR1 optional
      !IV_FIXED_POSITION type ANY optional
      !IV_SORT type CHAR1 optional
      !IV_VALUES_NAME type STRING optional
      !IV_BUTTON_TEXT type STRING optional
      !IV_STATE type WDY_UIE_LIBRARY_ENUM_TYPE optional .
  class-methods SET_ALV_BUTTON
    importing
      !IS_BUTTON type ZABS0002 .
  class-methods SET_ALV_BUTTON_ENABLED
    importing
      !IR_CONTROLLER type ref to IWCI_SALV_WD_TABLE optional
      !IV_BTTON_VISBLE type WDY_UIE_LIBRARY_ENUM_TYPE optional
      !IV_ALV_READONLY type CHAR1 optional .
  class-methods SET_ALV_BUTTON_VISABLE
    importing
      !IR_CONTROLLER type ref to IWCI_SALV_WD_TABLE optional
      !IV_BTTON_VISBLE type WDY_UIE_LIBRARY_ENUM_TYPE optional
      !IV_ALV_READONLY type CHAR1 optional
      !IV_ID type STRING optional .
  class-methods GET_ELEMENTS
    importing
      !IV_NONE type ref to IF_WD_CONTEXT_NODE
      !IV_NAME type STRING
    exporting
      !ET_ITEM type ANY TABLE
      !ER_MODEL type ref to IF_WD_CONTEXT_NODE .
  class-methods GET_ELEMENT
    importing
      !IV_NONE type ref to IF_WD_CONTEXT_NODE
      !IV_NAME type STRING
    exporting
      value(ES_STRU) type ANY
      !ER_MODEL type ref to IF_WD_CONTEXT_NODE
      !ER_ELEMENT type ref to IF_WD_CONTEXT_ELEMENT .
  class-methods SET_ELEMENT
    importing
      !IV_NONE type ref to IF_WD_CONTEXT_NODE
      !IV_NAME type STRING
      !IS_STRU type ANY .
  class-methods SET_ELEMENTS
    importing
      !IV_NONE type ref to IF_WD_CONTEXT_NODE
      !IV_NAME type STRING
      !IT_ITEM type TABLE optional
      !IV_ISCTAB type CHAR1 optional
      !IR_CONTROLL type ref to IF_WD_CONTROLLER optional
      !IR_ALV type ref to IWCI_SALV_WD_TABLE optional
    returning
      value(ER_NODE) type ref to IF_WD_CONTEXT_NODE .
  class-methods SET_ATTRIBUTE
    importing
      !IV_NONE type ref to IF_WD_CONTEXT_NODE
      !IV_NAME type STRING
      !IV_ATTR type ANY .
  class-methods GET_ATTRIBUTE
    importing
      !IV_NONE type ref to IF_WD_CONTEXT_NODE
      !IV_NAME type STRING
    exporting
      value(EV_ATTR) type ANY .
  class-methods GET_ELEMENT_FOR_NODE
    importing
      !IV_NONE type ref to IF_WD_CONTEXT_NODE
      !IV_NAME type STRING
      !IV_NAME_ATTR type STRING
    exporting
      !EV_ATTR type ANY
      !ER_MODEL type ref to IF_WD_CONTEXT_NODE .
  class-methods SET_DROP_LIST_FOR_ALV
    importing
      value(IV_NODE_NAME) type STRING
      value(IT_LIST) type WDR_CONTEXT_ATTR_VALUE_LIST
      !IR_NODE type ref to IF_WD_CONTEXT_NODE
      value(IV_ATTR_NAME) type STRING .
  class-methods SET_ATTR_FOR_NODE
    importing
      !IV_ATTR_VALUE type ANY
      !IV_ATTR_NAME type STRING
      !IV_NAME type STRING
      !IV_NODE type ref to IF_WD_CONTEXT_NODE
    exporting
      !ER_NODE type ref to IF_WD_CONTEXT_NODE
      !ER_ELEMENT type ref to IF_WD_CONTEXT_ELEMENT .
  class-methods CHK_MANDATORY_ATTR
    importing
      !IM_API_VIEW type ref to IF_WD_VIEW_CONTROLLER
    returning
      value(RT_RC) type SY-SUBRC .
  class-methods SET_PERSONALIZATION
    importing
      !IM_API_CTRL type ref to IF_WD_CONTROLLER
      !IM_CONFIG_ID type WDY_CONFIG_ID .
  class-methods GET_DOM_VALUE
    importing
      !IM_DOMNAM type ANY
      !IM_DOMVAL type ANY
    returning
      value(EX_DESC) type STRING .
  class-methods SET_DDL_VALUE
    importing
      !CONTEXT type ref to IF_WD_CONTEXT_NODE
      !ATT_NAME type STRING
      !CONTEXT_NAME type STRING
      !VALUE_SET type WDR_CONTEXT_ATTR_VALUE_LIST .
  class-methods GUID_CREATE
    returning
      value(RT_GUID) type STRING .
  class-methods GET_VENDOR_NAME
    importing
      !IM_LIFNR type ANY
      !IM_BUKRS type ANY optional
    exporting
      !EX_SUBRC type SY-SUBRC
    returning
      value(RT_NAME) type STRING .
  class-methods OP_CONTEXT
    importing
      !IO_CONTEXT type ref to IF_WD_CONTEXT_NODE
      value(IV_PATH) type STRING
      value(IV_MODE) type CHAR1 default 'G'
      value(IV_TYPE) type CHAR1 default 'L'
      !IS_RESULT type ANY optional
      !IT_RESULT type ANY TABLE optional
    exporting
      !ET_RESULT type ANY TABLE
      !ES_RESULT type ANY .
  class-methods SHOW_MESSAGE
    importing
      !IV_COMP type ref to OBJECT
      !IV_MSG type CSEQUENCE
      !IV_TYPE type CHAR1 .
  class-methods LOAD_CONFIG
    importing
      !IO_COMP type ref to OBJECT
      !IV_CONFIG type WDY_CONFIG_ID .
  class-methods GET_SELECT_LINES
    importing
      !IO_CONTEXT type ref to IF_WD_CONTEXT_NODE
      !IV_PATH type STRING
    exporting
      !ET_RESULT type ANY TABLE .
  class-methods SET_VALUE_LIST
    importing
      !IO_CONTEXT type ref to IF_WD_CONTEXT_NODE
      value(IV_PATH) type STRING
      value(IV_ATTR_NM) type STRING
      value(IV_DOMAIN_NM) type DOMNAME optional
      !IT_VALUE_SET type WDY_KEY_VALUE_TABLE optional
      value(IS_SELF_DEFINE) type CHAR1 default 'X' .
  class-methods POP_UP_WINDOW
    importing
      !IO_COMP type ref to OBJECT
      !IV_WIND_NAME type STRING
      !IV_WIDTH type STRING default CV_WIDTH
      !IV_HEIGHT type STRING default CV_HEIGHT
      !IV_TITLE type STRING optional
    returning
      value(RO_WINDOW) type ref to IF_WD_WINDOW .
  class-methods EDIT_DATE_RANGE
    importing
      !IM_F type ANY
      !IM_T type ANY
    exporting
      value(RT_RANGE) type TABLE .
  class-methods LOCK_UNLOCK
    importing
      !IM_LOCK type ANY
      !IM_TABLE type ANY
      !IM_VARKEY type ANY
    returning
      value(RT_MSG) type STRING .
  class-methods CLOSE_WINDOW
    importing
      !VIEW_CTRL type ref to IF_WD_VIEW_CONTROLLER optional .
  class-methods CALC_TAX
    importing
      !I_BUKRS type ANY
      !I_MWSKZ type ANY
      !I_WAERS type ANY
      !I_WRBTR type ANY
    exporting
      !E_TAXJE type ANY .
  class-methods POPUP_CONFIRM
    importing
      !ACTION_NAME type STRING
      !TEXT_TABLE type STRING_TABLE optional
      !VIEW_CTRL type ref to IF_WD_VIEW_CONTROLLER
      !WINDOW_TITLE type STRING
      !ACTION_NAME_CANCEL type STRING optional .
  class-methods GET_DATIME
    importing
      !IM_DATUM type SY-DATUM default SY-DATUM
      !IM_UZEIT type SY-UZEIT default SY-UZEIT
    returning
      value(RT_DATIME) type STRING .
  class-methods GET_CHARACTOR_PY
    importing
      !IV_NAME type STRING
    returning
      value(RV_PY) type STRING .
  class-methods GET_LOGON_NAME
    importing
      !IM_UNAME type ANY
    returning
      value(EX_NAME) type STRING .
  class-methods SET_BTN_PROPERTY
    importing
      !IV_BUTTON type STRING
      !IV_INTERFACE_CONTROLLER type ref to IWCI_SALV_WD_TABLE
      !IV_ENABLE type BOOLE_D default 'X'
      !IV_VISIBLE like CL_WD_UIELEMENT=>T_VISIBLE default '02' .
  class-methods SET_TABLE_RESET
    importing
      value(IR_COMPONENT) type ref to IF_WD_COMPONENT .
  class-methods GET_WDA_URL
    importing
      !IV_APPNAME type STRING
      !IT_PARS type WDY_KEY_VALUE_TABLE
    returning
      value(EV_URL) type STRING .
  class-methods GET_SELECTED_ELEMENTS
    importing
      !IR_CONTEXT type ref to IF_WD_CONTEXT_NODE
      !IV_CONTEXT_NAME type STRING
    returning
      value(ET_ELEMENTS) type WDR_CONTEXT_ELEMENT_SET .
  class-methods GET_SYSTEM_INFO
    returning
      value(ES_INFO) type ZABS_SYSTEM_INFO .
  class-methods SET_LIST_FOR_STATUS
    importing
      !IV_NODE type ref to IF_WD_CONTEXT_NODE
      !IV_NAME type STRING
      !IV_DOCTYPE type ZTR_DOCTYPE .
  class-methods GET_LIST_STATUS
    importing
      !IV_DOCTYPE type ZTR_DOCTYPE
    returning
      value(RT_LIST) type WDR_CONTEXT_ATTR_VALUE_LIST .
  class-methods CHECK_MANDATORY
    importing
      !VIEW_CONTROLLER type ref to IF_WD_VIEW_CONTROLLER
    returning
      value(ERROR) type CHAR1 .
  class-methods CHECK_MANDATORY_ATTR_ON_VIEW
    importing
      !VIEW_CONTROLLER type ref to IF_WD_VIEW_CONTROLLER
      !DISPLAY_MESSAGES type WDY_BOOLEAN optional .
  PROTECTED SECTION.
private section.

  class-methods GET_LINE_CONTEXT
    importing
      !IO_CONTEXT type ref to IF_WD_CONTEXT_NODE
      value(IV_PATH) type STRING
    exporting
      !ES_RESULT type ANY .
  class-methods SET_LINE_CONTEXT
    importing
      !IO_CONTEXT type ref to IF_WD_CONTEXT_NODE
      value(IV_PATH) type STRING
      !IS_RESULT type ANY .
  class-methods GET_TABLE_CONTEXT
    importing
      !IO_CONTEXT type ref to IF_WD_CONTEXT_NODE
      value(IV_PATH) type STRING
    exporting
      !ET_RESULT type ANY .
  class-methods SET_TABLE_CONTEXT
    importing
      !IO_CONTEXT type ref to IF_WD_CONTEXT_NODE
      value(IV_PATH) type STRING
      !IT_RESULT type ANY .
ENDCLASS.



CLASS ZCL_WD_COMMON IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>CALC_TAX
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_BUKRS                        TYPE        ANY
* | [--->] I_MWSKZ                        TYPE        ANY
* | [--->] I_WAERS                        TYPE        ANY
* | [--->] I_WRBTR                        TYPE        ANY
* | [<---] E_TAXJE                        TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD calc_tax.
    DATA lv_wrbtr TYPE bseg-wrbtr.
    DATA lt_mwdat TYPE STANDARD TABLE OF rtax1u15.


    CLEAR e_taxje.

    lv_wrbtr = i_wrbtr.


    CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
      EXPORTING
        i_bukrs           = i_bukrs
        i_mwskz           = i_mwskz
        i_waers           = i_waers
        i_wrbtr           = lv_wrbtr
      IMPORTING
        e_fwste           = lv_wrbtr
      TABLES
        t_mwdat           = lt_mwdat
      EXCEPTIONS
        bukrs_not_found   = 1
        country_not_found = 2
        mwskz_not_defined = 3
        mwskz_not_valid   = 4
        ktosl_not_found   = 5
        kalsm_not_found   = 6
        parameter_error   = 7
        knumh_not_found   = 8
        kschl_not_found   = 9
        unknown_error     = 10
        account_not_found = 11
        txjcd_not_valid   = 12
        OTHERS            = 13.

    e_taxje = lv_wrbtr.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>CHECK_MANDATORY
* +-------------------------------------------------------------------------------------------------+
* | [--->] VIEW_CONTROLLER                TYPE REF TO IF_WD_VIEW_CONTROLLER
* | [<-()] ERROR                          TYPE        CHAR1
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_mandatory.
    DATA lt_msg TYPE cl_wd_dynamic_tool=>t_check_result_message_tab.
    CALL METHOD cl_wd_dynamic_tool=>check_mandatory_attr_on_view(
      EXPORTING
        view_controller = view_controller
      IMPORTING
        messages        = lt_msg
                          ).
    IF lt_msg IS NOT INITIAL .
      error = 'X'.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>CHECK_MANDATORY_ATTR_ON_VIEW
* +-------------------------------------------------------------------------------------------------+
* | [--->] VIEW_CONTROLLER                TYPE REF TO IF_WD_VIEW_CONTROLLER
* | [--->] DISPLAY_MESSAGES               TYPE        WDY_BOOLEAN(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method check_mandatory_attr_on_view.


*  data l_mandatory_elements           type cl_wd_dynamic_tool=>tt_ext_check_mandatory.
*  data l_mandatory_element            type cl_wd_dynamic_tool=>t_ext_check_mandatory.
*  data l_bound_primary_property       type string.
*  data l_context                      type ref to if_wd_context.
*  data l_has_errors                   type wdy_boolean.
*  data l_last_point                   type i.
*  data l_message                      type t_check_result_message.
*  data l_context_node                 type ref to if_wd_context_node.
*  data l_context_element              type ref to if_wd_context_element.
*  data l_node_path                    type string.
*  data l_attribute_name               type string.
*  data l_context_element_set          type wdr_context_element_set.
*  data l_message_manager              type ref to if_wd_message_manager.
*  data l_view                         type ref to if_wd_view.
*  data l_element_empty_ok             type wdy_boolean.
*  data l_text                         type string.
*
**  l_view ?= cl_wdr_proxy_cast_tool=>get_instance( view_controller ).
*   l_view ?= view_controller.
*  lcl_mandatory_check=>get_mandatory_elements( exporting view = l_view  importing view_elements = l_mandatory_elements ).
*  check l_mandatory_elements is not initial.
*  l_context = view_controller->get_context( ).
*  if display_messages = abap_true.
*    l_message_manager = view_controller->get_message_manager( ).
*  endif.
*
*  loop at l_mandatory_elements into l_mandatory_element.
*    l_bound_primary_property = l_mandatory_element-view_element->bound__primary_property( ).
*    check l_bound_primary_property is not initial. "if no primary property is defined (fileupload) this is initial
*    find all occurrences of '.' in l_bound_primary_property match offset l_last_point.
*    if sy-subrc = 0.
*      l_node_path = l_bound_primary_property(l_last_point).
*      add 1 to l_last_point.
*      l_attribute_name = l_bound_primary_property+l_last_point.
*      l_context_node = l_context->root_node->path_get_node( l_node_path ).
*    else.
*      l_attribute_name = l_bound_primary_property.
*      l_context_node = l_context->root_node.
*    endif.
*    l_element_empty_ok = abap_false.
*    clear l_context_element_set.
*    l_context_element_set = lcl_mandatory_check=>get_ctxt_elements_to_check(
*      mandatory_element   = l_mandatory_element
*      context_root        = l_context->root_node
*      context_node        = l_context_node
*      node_path           = l_node_path
*      attribute_name      = l_attribute_name ).
*    loop at l_context_element_set into l_context_element.
*      l_has_errors = int_check_mandatory_attribute(
*        element          = l_context_element
*        attribute        = l_attribute_name
*        element_empty_ok = l_element_empty_ok  ).
*
*      if l_has_errors = abap_true.
*        l_message-context_element = l_context_element.
*        l_message-attribute_name = l_attribute_name.
*        l_message-t100_message-msgty = 'E'.
*        l_message-t100_message-msgid = 'WEBDYNPRO_RT'.
*        l_message-t100_message-msgno = '014'.
*        insert l_message into table messages.
*        if l_message_manager is bound.
*          l_message_manager->report_attribute_t100_message(
*            EXPORTING
*              msg                       =   l_message-t100_message
*              element                   =   l_context_element
*              attribute_name            =   l_attribute_name ).
*        endif.
*      endif.
*      clear: l_has_errors.
*    endloop.
*    clear: l_context_element_set, l_element_empty_ok, l_last_point, l_node_path.
*  endloop.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>CHK_MANDATORY_ATTR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_API_VIEW                    TYPE REF TO IF_WD_VIEW_CONTROLLER
* | [<-()] RT_RC                          TYPE        SY-SUBRC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD chk_mandatory_attr.
    DATA lt_msg TYPE cl_wd_dynamic_tool=>t_check_result_message_tab.

    CLEAR:lt_msg,rt_rc.

    "mandatory check
    CALL METHOD cl_wd_dynamic_tool=>check_mandatory_attr_on_view
      EXPORTING
        view_controller  = im_api_view
        display_messages = abap_true
      IMPORTING
        messages         = lt_msg.

    IF lt_msg IS NOT INITIAL.
      rt_rc = 4.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>CLOSE_WINDOW
* +-------------------------------------------------------------------------------------------------+
* | [--->] VIEW_CTRL                      TYPE REF TO IF_WD_VIEW_CONTROLLER(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD close_window.
    DATA:lo_window    TYPE REF TO if_wd_window,
         lo_win_cntr  TYPE REF TO if_wd_window_controller,
         ls_parameter TYPE wdr_event_parameter,
         lt_parameter TYPE wdr_event_parameter_list,
         lr_val       TYPE REF TO data.
    FIELD-SYMBOLS <fs> TYPE any.


    "取得绑定窗口的控制器
    CALL METHOD view_ctrl->get_embedding_window_ctlr
      RECEIVING
        result = lo_win_cntr.

    lo_window = lo_win_cntr->get_window( ).

    "fire plug
    IF lo_window IS NOT INITIAL.
      lo_window->close( ).
    ELSE.
      "设置fire plug参数为关闭窗口
      ls_parameter-name = 'CLOSE_WINDOW'.
      CREATE DATA lr_val TYPE c.
      ASSIGN lr_val->* TO <fs>.
      <fs> = 'X'.

      ls_parameter-value = lr_val.
      INSERT ls_parameter INTO TABLE lt_parameter.

      CALL METHOD lo_win_cntr->if_wd_view_controller~fire_plug
        EXPORTING
          parameters = lt_parameter
          plug_name  = 'EXIT'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>EDIT_DATE_RANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_F                           TYPE        ANY
* | [--->] IM_T                           TYPE        ANY
* | [<---] RT_RANGE                       TYPE        TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD edit_date_range.
    DATA lv_str TYPE string.

    IF  im_f IS NOT INITIAL
    AND im_t IS NOT INITIAL.
      lv_str = 'IBT' && im_f && im_t.
      APPEND lv_str TO rt_range.
    ELSEIF im_f IS NOT INITIAL
       AND im_t IS INITIAL.
      lv_str = 'IEQ' && im_f.
      APPEND lv_str TO rt_range.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>GET_ATTRIBUTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NONE                        TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_NAME                        TYPE        STRING
* | [<---] EV_ATTR                        TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_attribute.
    DATA : lo_el_context TYPE REF TO if_wd_context_element.
    lo_el_context = iv_none->get_element( ).

    "获取属性
    lo_el_context->get_attribute(
    EXPORTING
      name =  iv_name
    IMPORTING
      value = ev_attr ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>GET_CHARACTOR_PY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        STRING
* | [<-()] RV_PY                          TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_charactor_py.

    DATA:
      lv_xs  TYPE xstring,
      lv_i   TYPE i,
      lv_is  TYPE i,
      lv_str TYPE string,
      lv_c   TYPE char1.


    WHILE 1 = 1.
      TRY .
          lv_c = iv_name+lv_i(1).
          ADD 1 TO lv_i.
          lv_str = lv_c.
          CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
            EXPORTING
              text     = lv_str
*             MIMETYPE = ' '
              encoding = '8401'
            IMPORTING
              buffer   = lv_xs.

          IF    lv_xs < 'B0A1'   .
            DATA(lv_pinyin) = lv_c+0(1) .
          ELSEIF    lv_xs < 'B0C5'   .
            lv_pinyin = 'A'.
          ELSEIF    lv_xs < 'B2C1'   .
            lv_pinyin = 'B'.
          ELSEIF    lv_xs < 'B4EE'   .
            lv_pinyin = 'C'.
          ELSEIF    lv_xs < 'B6EA'   .
            lv_pinyin = 'D'.
          ELSEIF    lv_xs < 'B7A2'   .
            lv_pinyin = 'E'.
          ELSEIF    lv_xs < 'B8C1'   .
            lv_pinyin = 'F'.
          ELSEIF    lv_xs < 'B9FE'   .
            lv_pinyin = 'G'.
          ELSEIF    lv_xs < 'BBF7'   .
            lv_pinyin = 'H'.
          ELSEIF    lv_xs < 'BFA6'   .
            lv_pinyin = 'J'.
          ELSEIF    lv_xs < 'C0AC'   .
            lv_pinyin = 'K'.
          ELSEIF    lv_xs < 'C2E8'   .
            lv_pinyin = 'L'.
          ELSEIF    lv_xs < 'C4C3'   .
            lv_pinyin = 'M'.
          ELSEIF    lv_xs < 'C5B6'   .
            lv_pinyin = 'N'.
          ELSEIF    lv_xs < 'C5BE'   .
            lv_pinyin = 'O'.
          ELSEIF    lv_xs < 'C6DA'   .
            lv_pinyin = 'P'.
          ELSEIF    lv_xs < 'C8BB'   .
            lv_pinyin = 'Q'.
          ELSEIF    lv_xs < 'C8F6'   .
            lv_pinyin = 'R'.
          ELSEIF    lv_xs < 'CBFA'   .
            lv_pinyin = 'S'.
          ELSEIF    lv_xs < 'CDDA'   .
            lv_pinyin = 'T'.
          ELSEIF    lv_xs < 'CEF4'   .
            lv_pinyin = 'W'.
          ELSEIF    lv_xs < 'D1B9'   .
            lv_pinyin = 'X'.
          ELSEIF    lv_xs < 'D4D1'   .
            lv_pinyin = 'Y'.
          ELSEIF    lv_xs < 'D7FA'   .
            lv_pinyin = 'Z'.
          ENDIF .
          rv_py = rv_py && lv_pinyin.
        CATCH cx_root.
          EXIT.
      ENDTRY.
    ENDWHILE.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>GET_DATIME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_DATUM                       TYPE        SY-DATUM (default =SY-DATUM)
* | [--->] IM_UZEIT                       TYPE        SY-UZEIT (default =SY-UZEIT)
* | [<-()] RT_DATIME                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_datime.

    rt_datime = im_datum+0(4) && im_datum+4(2) &&
                im_datum+6(2) && im_uzeit+0(2) &&
                im_uzeit+2(2) && im_uzeit+4(2).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>GET_DOM_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_DOMNAM                      TYPE        ANY
* | [--->] IM_DOMVAL                      TYPE        ANY
* | [<-()] EX_DESC                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_dom_value.
    SELECT SINGLE ddtext
      INTO ex_desc
      FROM dd07v
     WHERE domname = im_domnam
       AND ddlanguage = sy-langu
       AND domvalue_l = im_domval.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>GET_ELEMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NONE                        TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_NAME                        TYPE        STRING
* | [<---] ES_STRU                        TYPE        ANY
* | [<---] ER_MODEL                       TYPE REF TO IF_WD_CONTEXT_NODE
* | [<---] ER_ELEMENT                     TYPE REF TO IF_WD_CONTEXT_ELEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_element.
* by yuanfeng
    DATA lo_nd_stru TYPE REF TO if_wd_context_node.
    DATA lo_el_stru TYPE REF TO if_wd_context_element.
    "节点名
    lo_nd_stru = iv_none->get_child_node( name = iv_name ).
    lo_el_stru = lo_nd_stru->get_element( ).
    lo_el_stru->get_static_attributes(
      IMPORTING
        static_attributes = es_stru ).

    er_model = lo_nd_stru.
    er_element = lo_el_stru.
    CLEAR : lo_nd_stru,lo_el_stru.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>GET_ELEMENTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NONE                        TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_NAME                        TYPE        STRING
* | [<---] ET_ITEM                        TYPE        ANY TABLE
* | [<---] ER_MODEL                       TYPE REF TO IF_WD_CONTEXT_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_elements.
*    FIELD-SYMBOLS : <lr_view> TYPE any.
*    ASSIGN iv_any TO  <lr_view>.
* by yuanfeng
    DATA lo_nd_item TYPE REF TO if_wd_context_node.
    "节点名
    lo_nd_item = iv_none->get_child_node( name = iv_name ).
    lo_nd_item->get_static_attributes_table( IMPORTING table = et_item ).

    er_model = lo_nd_item.
    CLEAR : lo_nd_item.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>GET_ELEMENT_FOR_NODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NONE                        TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_NAME                        TYPE        STRING
* | [--->] IV_NAME_ATTR                   TYPE        STRING
* | [<---] EV_ATTR                        TYPE        ANY
* | [<---] ER_MODEL                       TYPE REF TO IF_WD_CONTEXT_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_element_for_node.
* by yuanfeng
    DATA lo_nd_stru TYPE REF TO if_wd_context_node.
    DATA lo_el_stru TYPE REF TO if_wd_context_element.
    "节点名
    lo_nd_stru = iv_none->get_child_node( name = iv_name ).
    lo_el_stru = lo_nd_stru->get_element( ).
    lo_el_stru->get_attribute(
      EXPORTING
        name =  iv_name_attr
      IMPORTING
        value = ev_attr ).

    er_model = lo_nd_stru.
    CLEAR : lo_nd_stru.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_WD_COMMON=>GET_LINE_CONTEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_CONTEXT                     TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_PATH                        TYPE        STRING
* | [<---] ES_RESULT                      TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_line_context.

    DATA:
      lo_node    TYPE REF TO if_wd_context_node,
      lo_element TYPE REF TO if_wd_context_element,
      lv_str     TYPE string,
      lv_str1    TYPE string.

    SPLIT iv_path AT '.' INTO lv_str lv_str1.
    IF lv_str1 NE space." EQ 0.
      lo_node ?= io_context->path_get_node( path = iv_path ).
    ELSE.
      lo_node ?= io_context->get_child_node( name = iv_path ).
    ENDIF.
    CHECK lo_node IS BOUND.

    lo_element ?= lo_node->get_element( ).

    CHECK lo_element IS BOUND.

    lo_element->get_static_attributes(
      IMPORTING
        static_attributes = es_result ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>GET_LIST_STATUS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DOCTYPE                     TYPE        ZTR_DOCTYPE
* | [<-()] RT_LIST                        TYPE        WDR_CONTEXT_ATTR_VALUE_LIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_list_status.
    DATA: lt_domain TYPE STANDARD TABLE OF dd07v,
          ls_domain TYPE dd07v,
          lt_t9036  TYPE STANDARD TABLE OF ztrt9036,
          ls_t9036  TYPE ztrt9036,
          ls_set    TYPE wdr_context_attr_value.

*单据状态
    SELECT * INTO TABLE lt_t9036 FROM ztrt9036 WHERE doctype = iv_doctype.
    CHECK sy-subrc = 0.

*获取所有的审批状态藐视
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'ZTRDM_APPRO'
      TABLES
        values_tab      = lt_domain
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    SORT lt_t9036 BY zappro.

    LOOP AT lt_t9036 INTO ls_t9036.
      ls_set-value = ls_t9036-zappro.
      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = ls_t9036-zappro.
      IF sy-subrc = 0.
        ls_set-text = ls_domain-ddtext.
      ENDIF.
      APPEND ls_set TO rt_list.
      CLEAR ls_set.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>GET_LOGON_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_UNAME                       TYPE        ANY
* | [<-()] EX_NAME                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_logon_name.
    DATA lt_userid TYPE fdm_t_user_id.
    DATA ls_userid LIKE LINE OF lt_userid.
    DATA lt_name TYPE fdm_t_user_name.
    DATA ls_name LIKE LINE OF lt_name.


    CLEAR:lt_userid,lt_name,ex_name.

    ls_userid = im_uname.
    APPEND ls_userid TO lt_userid.


    CALL FUNCTION 'FDM_CUST_USER_NAMES_READ'
      EXPORTING
        it_user_id   = lt_userid
      IMPORTING
        et_user_name = lt_name.


    READ TABLE lt_name INTO ls_name INDEX 1.
    ex_name = ls_name-lastname && ls_name-firstname.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>GET_SELECTED_ELEMENTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_CONTEXT                     TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_CONTEXT_NAME                TYPE        STRING
* | [<-()] ET_ELEMENTS                    TYPE        WDR_CONTEXT_ELEMENT_SET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_selected_elements.
    et_elements = ir_context->get_child_node( name = iv_context_name )->get_selected_elements( 'X' ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>GET_SELECT_LINES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_CONTEXT                     TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_PATH                        TYPE        STRING
* | [<---] ET_RESULT                      TYPE        ANY TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_select_lines.

    DATA:
      lo_node TYPE REF TO if_wd_context_node,
      lv_str  TYPE string,
      lv_str1 TYPE string,
      lr_set  TYPE wdr_context_element_set,
      lo_el   TYPE REF TO if_wd_context_element.

    FIELD-SYMBOLS:<fs_line> TYPE any.

    SPLIT iv_path AT '.' INTO lv_str lv_str1.
    IF lv_str1 NE space."
      lo_node ?= io_context->path_get_node( path = iv_path ).
    ELSE.
      lo_node ?= io_context->get_child_node( name = iv_path ).
    ENDIF.
    lr_set = lo_node->get_selected_elements( ).

    LOOP AT lr_set INTO lo_el.
      INSERT INITIAL LINE INTO TABLE et_result ASSIGNING <fs_line>.
      lo_el->get_static_attributes( IMPORTING static_attributes = <fs_line> ).
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>GET_SYSTEM_INFO
* +-------------------------------------------------------------------------------------------------+
* | [<-()] ES_INFO                        TYPE        ZABS_SYSTEM_INFO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_system_info.
*time stamp
    GET TIME STAMP FIELD es_info-timestamp.

*time zone
    SELECT SINGLE tzonesys
           INTO es_info-timezone
           FROM ttzcu.

*date and time
    CONVERT TIME STAMP es_info-timestamp
            TIME ZONE es_info-timezone
            INTO DATE es_info-datum
                 TIME es_info-uzeit.

*user name
    es_info-uname = sy-uname.

*transaction code
    es_info-tcode = sy-tcode.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_WD_COMMON=>GET_TABLE_CONTEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_CONTEXT                     TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_PATH                        TYPE        STRING
* | [<---] ET_RESULT                      TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_table_context.

    DATA:
      lo_node TYPE REF TO if_wd_context_node,
      lv_str  TYPE string,
      lv_str1 TYPE string.

    SPLIT iv_path AT '.' INTO lv_str lv_str1.
    IF lv_str1 NE space."
      lo_node ?= io_context->path_get_node( path = iv_path ).
    ELSE.
      lo_node ?= io_context->get_child_node( name = iv_path ).
    ENDIF.
    CHECK lo_node IS BOUND.

    lo_node->get_static_attributes_table( IMPORTING table = et_result ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>GET_VENDOR_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_LIFNR                       TYPE        ANY
* | [--->] IM_BUKRS                       TYPE        ANY(optional)
* | [<---] EX_SUBRC                       TYPE        SY-SUBRC
* | [<-()] RT_NAME                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_vendor_name.
    CLEAR rt_name.

    CHECK im_lifnr IS NOT INITIAL.

    IF im_bukrs IS INITIAL.
      SELECT SINGLE name1
        INTO rt_name
        FROM lfa1
       WHERE lifnr = im_lifnr.

    ELSE.
      SELECT SINGLE name1
        INTO rt_name
        FROM lfa1
       INNER JOIN lfb1
               ON lfb1~lifnr = lfa1~lifnr
       WHERE lfa1~lifnr = im_lifnr
         AND lfb1~bukrs = im_bukrs.
    ENDIF.

    ex_subrc = sy-subrc.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>GET_WDA_URL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_APPNAME                     TYPE        STRING
* | [--->] IT_PARS                        TYPE        WDY_KEY_VALUE_TABLE
* | [<-()] EV_URL                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_wda_url.

    cl_wd_utilities=>construct_wd_url(
      EXPORTING
        application_name              = iv_appname
      IMPORTING
        out_absolute_url              = DATA(lv_url) ).

    LOOP AT it_pars INTO DATA(ls_pars).

      cl_http_server=>append_field_url(
       EXPORTING
         name  = ls_pars-key
         value = ls_pars-value
       CHANGING
         url   = lv_url ).

      CLEAR : ls_pars.
    ENDLOOP.

    ev_url = lv_url.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>GUID_CREATE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_GUID                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD guid_create.
    DATA lv_guid32 TYPE guid_32..

    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
*       EV_GUID_16 =
*       EV_GUID_22 =
        ev_guid_32 = lv_guid32.

    rt_guid = lv_guid32.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>INIT_ALV_STANDARD
* +-------------------------------------------------------------------------------------------------+
* | [--->] II_CONTROLLER                  TYPE REF TO IWCI_SALV_WD_TABLE
* | [--->] IS_HEAD                        TYPE        ZABS0001
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD init_alv_standard.
    DATA : lo_alv TYPE REF TO cl_salv_wd_config_table.

    lo_alv = ii_controller->get_model( ).

    lo_alv->if_salv_wd_table_settings~set_read_only( is_head-set_read_only ).
    IF is_head-set_width IS INITIAL.
      lo_alv->if_salv_wd_table_settings~set_width( '100%' ).
    ELSE.
      lo_alv->if_salv_wd_table_settings~set_width( is_head-set_width ).
    ENDIF.

    IF is_head-set_design IS NOT INITIAL.
      lo_alv->if_salv_wd_table_settings~set_design( is_head-set_design ).
    ELSE.
*      lo_alv->if_salv_wd_table_settings~set_design( |00| ).
    ENDIF.

    lo_alv->if_salv_wd_table_settings~set_c_table_allow_h_scrollbar( abap_true ).
    lo_alv->if_salv_wd_table_settings~set_data_check( is_head-set_data_check ).
    lo_alv->if_salv_wd_table_settings~set_selection_mode( is_head-set_selection_mode ).
    lo_alv->if_salv_wd_table_settings~set_display_empty_rows( is_head-set_display_empty_rows ).

    IF is_head-set_scrollable_col_count = 0.
      lo_alv->if_salv_wd_table_settings~set_scrollable_col_count( 20 ).
    ELSE.
      lo_alv->if_salv_wd_table_settings~set_scrollable_col_count( is_head-set_scrollable_col_count ).
    ENDIF.

    IF is_head-set_visible_row_count = 0.
      lo_alv->if_salv_wd_table_settings~set_visible_row_count( 20 ).  "UI组要求默认20行
    ELSE.
      lo_alv->if_salv_wd_table_settings~set_visible_row_count( is_head-set_visible_row_count ).  "UI组要求默认20行
    ENDIF.

    lo_alv->if_salv_wd_table_settings~set_grid_mode( is_head-set_grid_mode )."ADD WEIKAI 20170306
    lo_alv->if_salv_wd_table_settings~set_fixed_table_layout( is_head-set_fixed_table_layout )."ADD WEIKAI 20170318
    lo_alv->if_salv_wd_table_settings~set_multi_column_sort( is_head-set_multi_column_sort ). "add by yexiang 20170330
    lo_alv->if_salv_wd_std_functions~set_view_quick_save_allowed( is_head-set_view_quick_save_allowed ).
    lo_alv->if_salv_wd_std_functions~set_export_allowed( is_head-set_export_allowed ) .           "
    lo_alv->if_salv_wd_std_functions~set_pdf_allowed( is_head-set_pdf_allowed ).

    IF is_head-set_display_settings_allowed EQ abap_true . "add by yuanfeng 20170925
      lo_alv->if_salv_wd_std_functions~set_display_settings_allowed( 'X' ).
    ELSE.
      lo_alv->if_salv_wd_std_functions~set_aggregation_allowed( abap_true ).
    ENDIF.

    lo_alv->if_salv_wd_std_functions~set_edit_append_row_allowed( is_head-set_edit_append_row_allowed ).
    lo_alv->if_salv_wd_std_functions~set_edit_insert_row_allowed( is_head-set_edit_insert_row_allowed ).
    lo_alv->if_salv_wd_std_functions~set_edit_check_available( is_head-set_edit_check_available ).
    lo_alv->if_salv_wd_std_functions~set_edit_delete_row_allowed( is_head-set_edit_delete_row_allowed ).
    lo_alv->if_salv_wd_std_functions~set_view_list_allowed( is_head-set_view_list_allowed ).         "
    lo_alv->if_salv_wd_std_functions~set_filter_complex_allowed( 'X' ).
    lo_alv->if_salv_wd_table_settings~set_empty_table_text( is_head-set_empty_table_text )."add weikai 20170116

    lo_alv->if_salv_wd_function_settings~set_enabled( abap_true ) .

    IF is_head-set_dialog_settings_allowed EQ abap_true .
      lo_alv->if_salv_wd_std_functions~set_dialog_settings_allowed( abap_true ).
    ELSE.
      lo_alv->if_salv_wd_std_functions~set_dialog_settings_allowed( abap_false )."add chenhao 20170220
    ENDIF.

    lo_alv->if_salv_wd_std_functions~set_fixed_cols_left_allowed(      value = abap_true
    ).

    IF is_head-set_column_resize_mode IS INITIAL.
      lo_alv->if_salv_wd_table_settings~set_column_resize_mode( '99' ).
    ELSE.
      lo_alv->if_salv_wd_table_settings~set_column_resize_mode( |{ is_head-set_column_resize_mode }| ).
    ENDIF.

*    lo_alv->if_salv_wd_table_settings~set_cell_action_event_enabled( is_head-set_cell_action_event_enabled )."add zhangcs 20170720


    gr_alv = lo_alv.
    gr_columns = gr_alv->if_salv_wd_column_settings~get_columns( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>LOAD_CONFIG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_COMP                        TYPE REF TO OBJECT
* | [--->] IV_CONFIG                      TYPE        WDY_CONFIG_ID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD load_config.

    DATA ls_conf_key TYPE wdy_config_key.
    DATA lo_api_controller TYPE REF TO if_wd_controller.
    DATA lo_pers_manager   TYPE REF TO if_wd_personalization.

    ls_conf_key-config_id = iv_config.
    ls_conf_key-config_type = 00.

*   get personalization manager
    TRY .
        CALL METHOD io_comp->('WD_GET_API')
          RECEIVING
            result = lo_api_controller.

      CATCH cx_root.
    ENDTRY.
*  lo_api_controller = wd_this->wd_get_api( ).
    CHECK lo_api_controller IS BOUND.
    lo_pers_manager = lo_api_controller->get_personalization_manager( ).

    CALL METHOD lo_pers_manager->load_config_by_key
      EXPORTING
        config_key = ls_conf_key.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>LOCK_UNLOCK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_LOCK                        TYPE        ANY
* | [--->] IM_TABLE                       TYPE        ANY
* | [--->] IM_VARKEY                      TYPE        ANY
* | [<-()] RT_MSG                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD lock_unlock.
    DATA lv_tabname TYPE rstable-tabname.
    DATA lv_varkey TYPE rstable-varkey.

    CLEAR rt_msg.
    lv_tabname = im_table.
    lv_varkey = im_varkey.

    IF im_lock IS NOT INITIAL.
      CALL FUNCTION 'ENQUEUE_E_TABLE'
        EXPORTING
          mode_rstable   = 'E'
          tabname        = lv_tabname
          varkey         = lv_varkey
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid
             TYPE sy-msgty
             NUMBER sy-msgno
             INTO rt_msg
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        rt_msg = rt_msg && '.' && im_varkey.
      ENDIF.

    ELSE.
      CALL FUNCTION 'DEQUEUE_E_TABLE'
        EXPORTING
          mode_rstable = 'E'
          tabname      = lv_tabname
          varkey       = lv_varkey.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>OP_CONTEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_CONTEXT                     TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_PATH                        TYPE        STRING
* | [--->] IV_MODE                        TYPE        CHAR1 (default ='G')
* | [--->] IV_TYPE                        TYPE        CHAR1 (default ='L')
* | [--->] IS_RESULT                      TYPE        ANY(optional)
* | [--->] IT_RESULT                      TYPE        ANY TABLE(optional)
* | [<---] ET_RESULT                      TYPE        ANY TABLE
* | [<---] ES_RESULT                      TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD op_context.

    CASE iv_type.
      WHEN 'L'.
        CASE iv_mode.
          WHEN 'S'.
            CALL METHOD zcl_wd_common=>set_line_context
              EXPORTING
                io_context = io_context
                iv_path    = iv_path
                is_result  = is_result.
          WHEN 'G'.
            CALL METHOD zcl_wd_common=>get_line_context
              EXPORTING
                io_context = io_context
                iv_path    = iv_path
              IMPORTING
                es_result  = es_result.
          WHEN OTHERS.
        ENDCASE.
      WHEN 'T'.
        CASE iv_mode.
          WHEN 'S'.
            CALL METHOD zcl_wd_common=>set_table_context
              EXPORTING
                io_context = io_context
                iv_path    = iv_path
                it_result  = it_result.
          WHEN 'G'.
            CALL METHOD zcl_wd_common=>get_table_context
              EXPORTING
                io_context = io_context
                iv_path    = iv_path
              IMPORTING
                et_result  = et_result.
          WHEN OTHERS.
        ENDCASE.
      WHEN OTHERS.
    ENDCASE.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>POPUP_CONFIRM
* +-------------------------------------------------------------------------------------------------+
* | [--->] ACTION_NAME                    TYPE        STRING
* | [--->] TEXT_TABLE                     TYPE        STRING_TABLE(optional)
* | [--->] VIEW_CTRL                      TYPE REF TO IF_WD_VIEW_CONTROLLER
* | [--->] WINDOW_TITLE                   TYPE        STRING
* | [--->] ACTION_NAME_CANCEL             TYPE        STRING(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method popup_confirm.
    data:lo_wd_mgr type ref to if_wd_window_manager,
         lo_wd_cmp type ref to if_wd_component,
         lo_window type ref to if_wd_window.

* 设置pupop
    lo_wd_cmp = view_ctrl->if_wd_controller~get_component( ).
    lo_wd_mgr = lo_wd_cmp->get_window_manager( ).
    lo_window = lo_wd_mgr->create_popup_to_confirm(
                  text         = text_table
                  message_type = if_wd_window=>co_msg_type_question
                  button_kind  = 3 "Ok., Cancel
                  window_title = window_title
                  close_button = abap_true ).

* 设置ok按钮的接收action
    lo_window->subscribe_to_button_event(
                 button            = 4 "Ok
                 action_name       = action_name
                 action_view       = view_ctrl
                 is_default_button = abap_false ).

    if action_name_cancel is supplied .
      lo_window->subscribe_to_button_event(
                   button            = 6 "Cancel
                   action_name       = action_name_cancel
                   action_view       = view_ctrl
                   is_default_button = abap_false ).
    endif.

* 打开pupop
    lo_window->open( ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>POP_UP_WINDOW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_COMP                        TYPE REF TO OBJECT
* | [--->] IV_WIND_NAME                   TYPE        STRING
* | [--->] IV_WIDTH                       TYPE        STRING (default =CV_WIDTH)
* | [--->] IV_HEIGHT                      TYPE        STRING (default =CV_HEIGHT)
* | [--->] IV_TITLE                       TYPE        STRING(optional)
* | [<-()] RO_WINDOW                      TYPE REF TO IF_WD_WINDOW
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD pop_up_window.

    DATA:
      lr_view       TYPE REF TO if_wd_view_controller,
      lr_api_main   TYPE REF TO if_wd_component,
      lr_window_man TYPE REF TO if_wd_window_manager,
      comp_usage    TYPE REF TO if_wd_component_usage,
*    l_title       TYPE string,  张伯兴 2017/5/10 注释  ED1K908414
      lr_window     TYPE REF TO if_wd_window.

    TRY .
        CALL METHOD io_comp->('WD_GET_API')
          RECEIVING
            result = lr_api_main.

      CATCH cx_root.
    ENDTRY.
*    lr_api_main = wd_comp_controller->wd_get_api( ).
    lr_window_man = lr_api_main->get_window_manager( ).
*create window
    CALL METHOD lr_window_man->create_window
      EXPORTING
        modal        = abap_true
        window_name  = iv_wind_name
*       title        = l_title 张伯兴 2017/5/10 替换  ED1K908414
        title        = iv_title
        close_button = abap_true
      RECEIVING
        window       = ro_window.

    ro_window->set_window_size( width = iv_width height = iv_height ).

*open window
    ro_window->open( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>SET_ALV_BUTTON
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_BUTTON                      TYPE        ZABS0002
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_alv_button.
    DATA:  lr_btn TYPE REF TO cl_salv_wd_fe_button.
    DATA:  fn_btn TYPE REF TO cl_salv_wd_function.

    CLEAR : lr_btn,fn_btn.
    fn_btn = zcl_wd_common=>gr_alv->if_salv_wd_function_settings~create_function( id = is_button-set_button_code ).
    CREATE OBJECT lr_btn.
    lr_btn->set_text( is_button-set_button_name ).

    CASE is_button-set_button_code.
      WHEN 'ADD'.
        lr_btn->set_image_source( 'ICON_INSERT_ROW' ).
      WHEN 'ADD_T'."LIAOLF 20170227 ADD
        lr_btn->set_image_source( 'ICON_INSERT_ROW' ).
      WHEN 'EDIT'.
        lr_btn->set_image_source( '~Icon/EditChangedItem' ).
      WHEN 'ALLOW'.
        lr_btn->set_image_source( 'ICON_ALLOW' ).
      WHEN 'CHANGE'.
        lr_btn->set_image_source( 'ICON_CHANGE' ).
      WHEN 'CREATE' OR 'ZXJ' OR 'CREATE_E001' OR 'CREATE_E002'  OR 'CREATE_PD02' OR 'CREATE_PD03'  or 'CRT_ZF'.
        lr_btn->set_image_source( 'ICON_CREATE' ).
      WHEN 'DELETE'.
        lr_btn->set_image_source( 'ICON_DELETE_ROW' ).
      WHEN 'IMPORT'.
        lr_btn->set_image_source( 'ICON_IMPORT' ).
      WHEN 'REJECT'.
        lr_btn->set_image_source( 'ICON_REJECT' ).
      WHEN 'HISTORY'.
        lr_btn->set_image_source( 'ICON_HISTORY' ).
      WHEN 'SEND'.
        lr_btn->set_image_source( 'ICON_OUTBOX' ).
      WHEN 'SAVE'.
        lr_btn->set_image_source( 'ICON_SYSTEM_SAVE' ).
      WHEN 'STOP' OR 'ZCXSP'.
        lr_btn->set_image_source( 'ICON_BREAKPOINT' ).
      WHEN 'CREATE_JY'.
        lr_btn->set_image_source( 'ICON_CREATE' ).
      WHEN 'SETTLEMENT'.
        lr_btn->set_image_source( 'ICON_CASHING_UP' ).
      WHEN 'RELEASE' OR 'ZCLS'.
        lr_btn->set_image_source( 'ICON_RELEASE' ).
      WHEN 'SELALL'.
        lr_btn->set_image_source( 'ICON_SELECT_ALL' ).
      WHEN 'DESEL'.
        lr_btn->set_image_source( 'ICON_DESELECT_ALL' ).
      WHEN 'FEEDBACK' OR 'ZFIN'.
        lr_btn->set_image_source( '~Icon/Complete' ).
      WHEN 'FBCAN'.    "add by yisz 2017/6/7
        lr_btn->set_image_source( 'ICON_ADJUST_CONFIGURATION' ).
      WHEN 'JHPC'.    "add by yisz 2017/7/7
        lr_btn->set_image_source( 'ICON_CALCULATION' ).
      WHEN 'CREATE_ZX'.
        lr_btn->set_image_source( '~Icon/CreateNewReport' ).
      WHEN 'PLXG'.
        lr_btn->set_image_source( '~Icon/Material' ).
      WHEN 'EXPORT'.
        lr_btn->set_image_source( '~Icon/ExportToSpreadsheet' ).
      WHEN 'ZFQSP'.
        lr_btn->set_image_source( '~Icon/Done' ).
      WHEN 'ZDY'.
        lr_btn->set_image_source( '~Icon/Print' ).
      WHEN 'COPY'.
        lr_btn->set_image_source( '~Icon/Copy' ).

      WHEN 'F5'.
        lr_btn->set_image_source( '~Icon/TbRefresh' ).

      WHEN 'SUBMIT'.
        lr_btn->set_image_source( '~Icon/Approve' ).
      WHEN 'BLWH'.
        lr_btn->set_image_source( '~Icon/DefaultOverwritten' ).
      WHEN 'ZSC'.
        lr_btn->set_image_source( '~Icon/Delete' ).
      WHEN 'ZSP' .
        lr_btn->set_image_source( '~Icon/Approve' ).
    ENDCASE.

    "控制按钮是否显示~
    fn_btn->set_editor( lr_btn ).
    IF is_button-set_visible IS INITIAL.
      fn_btn->set_visible( '02' ).
    ELSE.
      fn_btn->set_visible( is_button-set_visible ).
    ENDIF.

    "控制按钮是否激活
    IF is_button-set_enabled = ''.
      lr_btn->set_enabled( abap_true ).
    ELSE.
      lr_btn->set_enabled( abap_false ).
    ENDIF.

    "控制按钮对齐方式~
    IF is_button-set_alignment IS NOT INITIAL.
      fn_btn->set_alignment( is_button-set_alignment ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>SET_ALV_BUTTON_ENABLED
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_CONTROLLER                  TYPE REF TO IWCI_SALV_WD_TABLE(optional)
* | [--->] IV_BTTON_VISBLE                TYPE        WDY_UIE_LIBRARY_ENUM_TYPE(optional)
* | [--->] IV_ALV_READONLY                TYPE        CHAR1(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_alv_button_enabled.


    DATA   lr_btn_add TYPE REF TO cl_salv_wd_function.
    DATA   lo_alv TYPE REF TO cl_salv_wd_config_table.

    "add weikai 20170309
    lo_alv = ir_controller->get_model( ).
    lo_alv->if_salv_wd_table_settings~set_read_only( iv_alv_readonly ).
    lr_btn_add = lo_alv->if_salv_wd_function_settings~get_function( id = 'ADD'  ).
    IF lr_btn_add IS BOUND.
      lr_btn_add->set_visible( iv_btton_visble ).
    ENDIF.

    lr_btn_add = lo_alv->if_salv_wd_function_settings~get_function( id = 'DELETE').
    IF lr_btn_add IS BOUND.
      lr_btn_add->set_visible( iv_btton_visble ).
    ENDIF.


    lr_btn_add = lo_alv->if_salv_wd_function_settings~get_function( id = 'EXPORT').
    IF lr_btn_add IS BOUND.
      lr_btn_add->set_visible( iv_btton_visble ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>SET_ALV_BUTTON_VISABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_CONTROLLER                  TYPE REF TO IWCI_SALV_WD_TABLE(optional)
* | [--->] IV_BTTON_VISBLE                TYPE        WDY_UIE_LIBRARY_ENUM_TYPE(optional)
* | [--->] IV_ALV_READONLY                TYPE        CHAR1(optional)
* | [--->] IV_ID                          TYPE        STRING(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_alv_button_visable.


    DATA   lr_btn_add TYPE REF TO cl_salv_wd_function.
    DATA   lo_alv TYPE REF TO cl_salv_wd_config_table.

    lo_alv = ir_controller->get_model( ).
    lo_alv->if_salv_wd_table_settings~set_read_only( iv_alv_readonly ).
    lr_btn_add = lo_alv->if_salv_wd_function_settings~get_function( id = iv_id  ).
    IF lr_btn_add IS BOUND.
      lr_btn_add->set_visible( iv_btton_visble ).
    ENDIF.




  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>SET_ALV_COLUMN_ATTRIBUTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_ALV_CONFIG_TABLE            TYPE REF TO CL_SALV_WD_CONFIG_TABLE
* | [--->] IV_CHECKBOX                    TYPE        STRING(optional)
* | [--->] IV_HEADER                      TYPE        STRING(optional)
* | [--->] IV_NAME                        TYPE        STRING
* | [--->] IV_POSITION                    TYPE        I(optional)
* | [--->] IV_RESIZABLE                   TYPE        WDY_BOOLEAN(optional)
* | [--->] IV_VISIBLE                     TYPE        WDY_UIE_LIBRARY_ENUM_TYPE(optional)
* | [--->] IV_WIDTH                       TYPE        STRING(optional)
* | [--->] IV_ALIGN                       TYPE        WDUI_TABLE_COLUMN_HALIGN(optional)
* | [--->] IV_BUTTON                      TYPE        CHAR1(optional)
* | [--->] IV_LINK                        TYPE        CHAR1(optional)
* | [--->] IV_URL                         TYPE        STRING(optional)
* | [--->] IV_INPUT                       TYPE        STRING(optional)
* | [--->] IV_DROP                        TYPE        STRING(optional)
* | [--->] IV_READONLY_NAME               TYPE        STRING(optional)
* | [--->] IV_COLOR_NAME                  TYPE        STRING(optional)
* | [--->] IV_TEXTVIEW                    TYPE        STRING(optional)
* | [--->] IV_IMAGE                       TYPE        STRING(optional)
* | [--->] IV_READONLY                    TYPE        CHAR1(optional)
* | [--->] IV_DROP_BY_INX                 TYPE        STRING(optional)
* | [--->] IV_SET_RESIZABLE               TYPE        CHAR1(optional)
* | [--->] IV_FIXED_POSITION              TYPE        ANY(optional)
* | [--->] IV_SORT                        TYPE        CHAR1(optional)
* | [--->] IV_VALUES_NAME                 TYPE        STRING(optional)
* | [--->] IV_BUTTON_TEXT                 TYPE        STRING(optional)
* | [--->] IV_STATE                       TYPE        WDY_UIE_LIBRARY_ENUM_TYPE(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_alv_column_attribute.

    DATA:
      lr_column_settings TYPE REF TO if_salv_wd_column_settings,
      lr_column          TYPE REF TO cl_salv_wd_column,
      lr_column_header   TYPE REF TO cl_salv_wd_column_header.

    DATA:
      lr_checkbox TYPE REF TO cl_salv_wd_uie_checkbox,
      lr_textview TYPE REF TO cl_salv_wd_uie_text_view,
      lr_button   TYPE REF TO cl_salv_wd_uie_button,
      lr_image    TYPE REF TO cl_salv_wd_uie_image,
      lr_link     TYPE REF TO cl_salv_wd_uie_link_to_action,
      lr_input    TYPE REF TO cl_salv_wd_uie_input_field,
      lr_drop_inx TYPE REF TO cl_salv_wd_uie_dropdown_by_idx,
      lr_drop     TYPE REF TO cl_salv_wd_uie_dropdown_by_key.

    lr_column_settings ?= ir_alv_config_table.
    lr_column = lr_column_settings->get_column( iv_name ).
    CHECK lr_column IS BOUND .

    lr_column_header = lr_column->create_header( ).

    IF NOT iv_header IS INITIAL .
      lr_column_header->set_text( iv_header ).
    ENDIF.
    IF NOT iv_visible IS INITIAL .
      lr_column->set_visible( iv_visible ).
    ENDIF.
    IF NOT iv_position IS INITIAL .
      lr_column->set_position( iv_position ).
    ENDIF.
    IF NOT iv_width IS INITIAL .
      lr_column->set_width( iv_width ).
    ENDIF.
    IF NOT iv_resizable IS INITIAL .
      lr_column->set_resizable( iv_resizable ).
    ENDIF.

    IF NOT iv_color_name IS INITIAL.
      lr_column->set_cell_design_fieldname( value = iv_color_name ).
    ENDIF.

    IF NOT iv_fixed_position IS INITIAL.
      lr_column->set_fixed_position( iv_fixed_position )."列固定左边
    ENDIF.

    IF NOT iv_checkbox IS INITIAL .
      CREATE OBJECT lr_checkbox
        EXPORTING
          checked_fieldname = iv_name.

      IF iv_readonly_name IS NOT INITIAL.
        lr_checkbox->set_read_only_fieldname( iv_readonly_name ).
      ELSE.
        IF iv_checkbox = '01'.
          lr_checkbox->set_read_only( abap_true ).
        ENDIF.
      ENDIF.

*Added by chenhao 20170227.
      IF iv_readonly IS NOT INITIAL.
        lr_checkbox->set_read_only( abap_true ).
      ENDIF.
*Added by chenhao 20170227 end.

      lr_column->set_cell_editor( lr_checkbox ).

    ENDIF.
    IF NOT iv_align IS INITIAL .
      lr_column->set_h_align( iv_align ).
    ENDIF.

    IF NOT iv_button IS INITIAL .
      CREATE OBJECT lr_button.
      IF iv_button_text IS INITIAL."changed by wenruihao 20180112
        lr_button->set_text(  iv_header ).
      ELSE.
        lr_button->set_text(  iv_button_text ).
      ENDIF.
      lr_column->set_cell_editor( lr_button ).
    ENDIF.

    IF NOT iv_link IS INITIAL .
      CREATE OBJECT lr_link.
      lr_link->set_text_fieldname( iv_name ).
      lr_column->set_cell_editor( lr_link ).
    ENDIF.

    IF NOT iv_input IS INITIAL .
      CREATE OBJECT lr_input
        EXPORTING
          value_fieldname = iv_name.
      lr_column->set_cell_editor( lr_input ).

      IF iv_readonly_name IS NOT INITIAL.
        IF iv_readonly_name = 'X'.
          lr_input->set_read_only( abap_true ).
        ELSE.
          lr_input->set_read_only_fieldname( iv_readonly_name ).
        ENDIF.
      ENDIF.

*Added by chenhao 20170227.
      IF iv_readonly IS NOT INITIAL.
        lr_input->set_read_only( abap_true ).
      ENDIF.
*Added by chenhao 20170227 end.

      IF iv_state IS NOT INITIAL.
        lr_input->set_state( iv_state ).
      ENDIF.
    ENDIF.

    IF NOT iv_textview IS INITIAL.
      CREATE OBJECT lr_textview.
      lr_column->set_cell_editor( lr_textview ).

      IF iv_readonly_name IS NOT INITIAL.
        IF iv_readonly_name = 'X'.
          lr_textview->set_enabled( abap_true ).
        ELSE.
          lr_textview->set_enabled_fieldname( iv_readonly_name ).
        ENDIF.
      ELSE.
        lr_textview->set_enabled( abap_true ).
      ENDIF.
    ENDIF.

    IF NOT iv_drop IS INITIAL .
      CREATE OBJECT lr_drop
        EXPORTING
          selected_key_fieldname = iv_name.
      lr_drop->set_key_visible( abap_false ).

      IF iv_readonly_name IS NOT INITIAL.
        IF iv_readonly_name = 'X'.
          lr_drop->set_read_only( abap_false ).
        ELSE.
          lr_drop->set_read_only_fieldname( iv_readonly_name ).
        ENDIF.
      ELSE.
        lr_drop->set_read_only( abap_false ).
      ENDIF.

      IF iv_readonly IS NOT INITIAL.
        lr_drop->set_read_only( abap_true ).
      ENDIF.

      IF iv_state IS NOT INITIAL.
        lr_drop->set_state( iv_state ).
      ENDIF.

      lr_column->set_cell_editor( lr_drop ).

    ENDIF.

    IF iv_drop_by_inx IS NOT INITIAL.
      CREATE OBJECT lr_drop_inx
        EXPORTING
          selected_key_fieldname = iv_name.

      IF iv_readonly_name IS NOT INITIAL.
        IF iv_readonly_name = 'X'.
          lr_drop_inx->set_read_only( abap_false ).
        ELSE.
          lr_drop_inx->set_read_only_fieldname( iv_readonly_name ).
        ENDIF.
      ELSE.
        lr_drop_inx->set_read_only( abap_false ).
      ENDIF.

      IF iv_readonly IS NOT INITIAL.
        lr_drop_inx->set_read_only( abap_true ).
      ENDIF.

      IF iv_values_name IS INITIAL.
        lr_drop_inx->set_valueset_fieldname( 'VALUES' ).
      ELSE.
        lr_drop_inx->set_valueset_fieldname( iv_values_name ).
      ENDIF.

      lr_drop_inx->set_type( if_salv_wd_c_data_table=>type_key_value ).

      IF iv_state IS NOT INITIAL.
        lr_drop_inx->set_state( iv_state ).
      ENDIF.

      lr_column->set_cell_editor( lr_drop_inx ).
    ENDIF.

    IF iv_image IS NOT INITIAL.
      CREATE OBJECT lr_image.
      lr_image->set_source_fieldname( iv_image ).
      lr_column->set_cell_editor( lr_image ).
    ENDIF.

    "add weikai 20170318
    lr_column->set_resizable( value = iv_set_resizable ).

    "add yexiang 20170303
    IF iv_sort IS NOT INITIAL.
      lr_column->if_salv_wd_column_service_ref~set_sort_fieldname( iv_name ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>SET_ATTRIBUTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NONE                        TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_NAME                        TYPE        STRING
* | [--->] IV_ATTR                        TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_attribute.
    DATA : lo_el_context TYPE REF TO if_wd_context_element.
    lo_el_context = iv_none->get_element( ).

    "设置属性
    lo_el_context->set_attribute(
    EXPORTING
      name =  iv_name
      value = iv_attr ).


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>SET_ATTR_FOR_NODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ATTR_VALUE                  TYPE        ANY
* | [--->] IV_ATTR_NAME                   TYPE        STRING
* | [--->] IV_NAME                        TYPE        STRING
* | [--->] IV_NODE                        TYPE REF TO IF_WD_CONTEXT_NODE
* | [<---] ER_NODE                        TYPE REF TO IF_WD_CONTEXT_NODE
* | [<---] ER_ELEMENT                     TYPE REF TO IF_WD_CONTEXT_ELEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_attr_for_node.
    DATA lo_nd_head TYPE REF TO if_wd_context_node.
    DATA lo_el_head TYPE REF TO if_wd_context_element.


    lo_nd_head = iv_node->get_child_node( name = iv_name ).

    lo_el_head = lo_nd_head->get_element( ).
    TRY .
        lo_el_head->set_attribute(
          name =  iv_attr_name
          value = iv_attr_value ).
      CATCH cx_root.

    ENDTRY.



    er_node = lo_nd_head.
    er_element = lo_el_head.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>SET_BTN_PROPERTY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BUTTON                      TYPE        STRING
* | [--->] IV_INTERFACE_CONTROLLER        TYPE REF TO IWCI_SALV_WD_TABLE
* | [--->] IV_ENABLE                      TYPE        BOOLE_D (default ='X')
* | [--->] IV_VISIBLE                     LIKE        CL_WD_UIELEMENT=>T_VISIBLE (default ='02')
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_btn_property.
    DATA: lr_config   TYPE REF TO cl_salv_wd_config_table,
          lr_function TYPE REF TO cl_salv_wd_function,
          lr_buttonui TYPE REF TO cl_salv_wd_fe_button.

    lr_config = iv_interface_controller->get_model( ).
    lr_function = lr_config->if_salv_wd_function_settings~get_function( id = iv_button ).

    lr_function->set_visible( value = iv_visible ).
    lr_buttonui ?= lr_function->get_editor( ).
    lr_buttonui->set_enabled( value = iv_enable ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>SET_DDL_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] CONTEXT                        TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] ATT_NAME                       TYPE        STRING
* | [--->] CONTEXT_NAME                   TYPE        STRING
* | [--->] VALUE_SET                      TYPE        WDR_CONTEXT_ATTR_VALUE_LIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_ddl_value.
    DATA:lo_node      TYPE REF TO if_wd_context_node,
         lo_node_info TYPE REF TO if_wd_context_node_info.

* get context infor
    lo_node = context->get_child_node( name = context_name ).

    lo_node_info = lo_node->get_node_info( ).

* set value set
    lo_node_info->set_attribute_value_set(
      name      = att_name
      value_set = value_set ) .

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>SET_DROP_LIST_FOR_ALV
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_NAME                   TYPE        STRING
* | [--->] IT_LIST                        TYPE        WDR_CONTEXT_ATTR_VALUE_LIST
* | [--->] IR_NODE                        TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_ATTR_NAME                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_drop_list_for_alv.

    DATA: lr_root_info TYPE REF TO if_wd_context_node_info,
          lr_node_info TYPE REF TO if_wd_context_node_info.

    DATA  lr_procode_info TYPE wdr_context_attribute_info.
    lr_root_info = ir_node->get_node_info( ).

    CALL METHOD lr_root_info->get_child_node
      EXPORTING
        name       = iv_node_name
      RECEIVING
        child_node = lr_node_info.

    CALL METHOD lr_node_info->set_attribute_value_set
      EXPORTING
        name      = iv_attr_name
        value_set = it_list.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>SET_ELEMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NONE                        TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_NAME                        TYPE        STRING
* | [--->] IS_STRU                        TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_element.
* by yuanfeng
    DATA lo_nd_stru TYPE REF TO if_wd_context_node.
    DATA lo_el_stru TYPE REF TO if_wd_context_element.
    "节点名
    lo_nd_stru = iv_none->get_child_node( name = iv_name ).
    lo_el_stru = lo_nd_stru->get_element( ).
    lo_el_stru->set_static_attributes(
        static_attributes = is_stru ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>SET_ELEMENTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NONE                        TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_NAME                        TYPE        STRING
* | [--->] IT_ITEM                        TYPE        TABLE(optional)
* | [--->] IV_ISCTAB                      TYPE        CHAR1(optional)
* | [--->] IR_CONTROLL                    TYPE REF TO IF_WD_CONTROLLER(optional)
* | [--->] IR_ALV                         TYPE REF TO IWCI_SALV_WD_TABLE(optional)
* | [<-()] ER_NODE                        TYPE REF TO IF_WD_CONTEXT_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_elements.
    DATA : lo_nd_item  TYPE REF TO if_wd_context_node,
           lr_nodeinfo TYPE REF TO if_wd_context_node_info,
           lr_element  TYPE REF TO if_wd_context_element.

    "节点名
    lo_nd_item = iv_none->get_child_node( name = iv_name ).

    IF it_item IS SUPPLIED AND lines( it_item ) = 0.
      DATA lv_item TYPE REF TO data.
      FIELD-SYMBOLS <lt_item> TYPE table.

      CREATE DATA lv_item LIKE it_item.
      ASSIGN lv_item->* TO <lt_item>.
    ENDIF.

    IF iv_isctab = 'X'.
      DATA : lr_alv_nodeinfo_settings TYPE REF TO if_wd_context_node_info,
             context_path             TYPE string_table.

      lr_alv_nodeinfo_settings = ir_alv->wd_get_api( )->get_context( )->root_node->get_child_node('SETTINGS')->get_node_info( ).
      CLEAR context_path.
      APPEND 'CONTEXT' TO context_path.                     "#EC NOTEXT
      APPEND iv_name TO context_path.

      lr_alv_nodeinfo_settings->set_mapping_complete( mapped_controller = ir_controll
                                                      context_path = context_path ). "#EC NOTEXT

      DATA s_attr_info   TYPE wdr_context_attribute_info.
      s_attr_info-name      = 'TABLE_CONTROL'.
      s_attr_info-type_name = 'SALV_WD_TABLE_CONTROL'.
      s_attr_info-default_value = if_salv_wd_c_table_settings=>table_control_c_table.

      lr_nodeinfo = lo_nd_item->get_node_info( ).
      lr_nodeinfo->add_attribute( attribute_info = s_attr_info ).

      TRY .
          lo_nd_item->set_attribute(
           EXPORTING
            name = `TABLE_CONTROL`
            value = if_salv_wd_c_table_settings=>table_control_c_table ).
        CATCH cx_root .

          APPEND INITIAL LINE TO <lt_item>.
          lo_nd_item->bind_table( new_items = <lt_item> set_initial_elements = abap_true ).
          lo_nd_item->set_attribute(
           EXPORTING
            name = `TABLE_CONTROL`
            value = if_salv_wd_c_table_settings=>table_control_c_table ).
          RETURN.
      ENDTRY.
    ENDIF.

    IF lines( it_item ) > 0 OR iv_isctab IS INITIAL.
      lo_nd_item->bind_table( new_items = it_item set_initial_elements = abap_true ).
    ELSE.
      IF it_item IS SUPPLIED AND lines( it_item ) = 0 AND  ( iv_isctab = 'X' OR iv_isctab = 'C ' ).
        APPEND INITIAL LINE TO <lt_item>.
        lo_nd_item->bind_table( new_items = <lt_item> set_initial_elements = abap_true ).
      ENDIF.
    ENDIF.

    er_node = lo_nd_item.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_WD_COMMON=>SET_LINE_CONTEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_CONTEXT                     TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_PATH                        TYPE        STRING
* | [--->] IS_RESULT                      TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_line_context.


    DATA:
      lo_node    TYPE REF TO if_wd_context_node,
      lo_element TYPE REF TO if_wd_context_element,
      lv_str     TYPE string,
      lv_str1    TYPE string.

    SPLIT iv_path AT '.' INTO lv_str lv_str1.
    IF lv_str1 NE space.".
      lo_node ?= io_context->path_get_node( path = iv_path ).
    ELSE.
      lo_node ?= io_context->get_child_node( name = iv_path ).
    ENDIF.
    CHECK lo_node IS BOUND.

    lo_element ?= lo_node->get_element( ).

    CHECK lo_element IS BOUND.

    lo_element->set_static_attributes(
      EXPORTING
        static_attributes = is_result ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>SET_LIST_FOR_STATUS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE                        TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_NAME                        TYPE        STRING
* | [--->] IV_DOCTYPE                     TYPE        ZTR_DOCTYPE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_list_for_status.
    DATA: lt_domain    TYPE STANDARD TABLE OF dd07v,
          ls_domain    TYPE dd07v,
          lt_t9036     TYPE STANDARD TABLE OF ztrt9036,
          ls_t9036     TYPE ztrt9036,
          lo_node_info TYPE REF TO if_wd_context_node_info,
          lt_set       TYPE STANDARD TABLE OF wdr_context_attr_value,
          ls_set       TYPE wdr_context_attr_value.

*单据状态
    SELECT * INTO TABLE lt_t9036 FROM ztrt9036 WHERE doctype = iv_doctype.
    CHECK sy-subrc = 0.

*获取所有的审批状态藐视
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'ZTRDM_APPRO'
      TABLES
        values_tab      = lt_domain
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    SORT lt_t9036 BY zappro.
    LOOP AT lt_t9036 INTO ls_t9036.
      ls_set-value = ls_t9036-zappro.
      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = ls_t9036-zappro.
      IF sy-subrc = 0.
        ls_set-text = ls_domain-ddtext.
      ENDIF.
      APPEND ls_set TO lt_set.
      CLEAR ls_set.
    ENDLOOP.

    lo_node_info = iv_node->get_node_info( ).
    LO_NODE_INFO->set_attribute_value_set(
      EXPORTING
        name      = iv_name    " Web Dynpro: Name of Context Element
        value_set = lt_set    " All Fixed Values of an Attribute with Texts
    ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>SET_PERSONALIZATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_API_CTRL                    TYPE REF TO IF_WD_CONTROLLER
* | [--->] IM_CONFIG_ID                   TYPE        WDY_CONFIG_ID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_personalization.
    DATA:
      lo_pers_mgr   TYPE REF TO if_wd_personalization,
      lv_config_key TYPE wdy_config_key.

    lv_config_key-config_id = im_config_id.
    lo_pers_mgr = im_api_ctrl->get_personalization_manager( ).

    CALL METHOD lo_pers_mgr->load_config_by_key
      EXPORTING
        config_key = lv_config_key.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_WD_COMMON=>SET_TABLE_CONTEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_CONTEXT                     TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_PATH                        TYPE        STRING
* | [--->] IT_RESULT                      TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_table_context.

    DATA:
      lo_node TYPE REF TO if_wd_context_node,
      lv_str  TYPE string,
      lv_str1 TYPE string.

    SPLIT iv_path AT '.' INTO lv_str lv_str1.
    IF lv_str1 NE space."
      lo_node ?= io_context->path_get_node( path = iv_path ).
    ELSE.
      lo_node ?= io_context->get_child_node( name = iv_path ).
    ENDIF.
    CHECK lo_node IS BOUND.

    lo_node->bind_table( new_items = it_result set_initial_elements = abap_true  ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>SET_TABLE_RESET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_COMPONENT                   TYPE REF TO IF_WD_COMPONENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_table_reset.

    DATA l_p13n_components        TYPE if_wd_config_runtime=>tt_personalized_components.
    DATA l_p13n_component         TYPE if_wd_config_runtime=>t_personalized_components.
    DATA lr_component             TYPE REF TO cl_wdr_delegating_component.
    DATA l_adaptation_handler     TYPE REF TO if_wdr_adaptation.
    DATA l_sim_view_el_interface  TYPE REF TO iwci_iwd_simulate_view_element.

    cl_wdr_personalization_helper=>get_personalized_components( EXPORTING include_sim_view_elemts = abap_true
                                                                IMPORTING personalized_components = l_p13n_components  ).
    IF l_p13n_components IS INITIAL.
      lr_component ?= ir_component.
      lr_component->pers_service->delete( delete_appl_data = abap_true ).
    ELSE.
      LOOP AT l_p13n_components INTO l_p13n_component .
        lr_component ?= l_p13n_component-component.
        IF lr_component->pers_service IS BOUND.
          lr_component->pers_service->delete( delete_appl_data = abap_true ).

          READ TABLE lr_component->component_info->implemented_interfaces WITH KEY component_name = 'IWD_SIMULATE_VIEW_ELEMENT' TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            l_sim_view_el_interface ?= lr_component->get_delegate( ).
            l_sim_view_el_interface->discard_changes_for_appl( ).
          ELSE.
            l_adaptation_handler ?= lr_component->if_wd_component~get_personalization_manager( ).
            TRY.
                l_adaptation_handler->save_implicit_data( pers_scope = 1 ).
              CATCH cx_wd_personalization.
                CONTINUE.
            ENDTRY.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>SET_VALUE_LIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_CONTEXT                     TYPE REF TO IF_WD_CONTEXT_NODE
* | [--->] IV_PATH                        TYPE        STRING
* | [--->] IV_ATTR_NM                     TYPE        STRING
* | [--->] IV_DOMAIN_NM                   TYPE        DOMNAME(optional)
* | [--->] IT_VALUE_SET                   TYPE        WDY_KEY_VALUE_TABLE(optional)
* | [--->] IS_SELF_DEFINE                 TYPE        CHAR1 (default ='X')
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_value_list.


    DATA:
      lo_node      TYPE REF TO if_wd_context_node,
      lo_element   TYPE REF TO if_wd_context_element,
      lv_str       TYPE string,
      lv_str1      TYPE string,
      lt_value_set TYPE wdy_key_value_table,
      l_node_info  TYPE REF TO if_wd_context_node_info.


    SPLIT iv_path AT '.' INTO lv_str lv_str1.
    IF lv_str1 NE space." EQ 0.
      lo_node ?= io_context->path_get_node( path = iv_path ).
    ELSE.
      lo_node ?= io_context->get_child_node( name = iv_path ).
    ENDIF.
    CHECK lo_node IS BOUND.

    l_node_info = lo_node->get_node_info( ).

*==获取下拉值
    IF it_value_set IS NOT INITIAL.
      lt_value_set = it_value_set.
    ELSE.
*    CALL METHOD zcl_crm_common=>get_value_list
*      EXPORTING
*        iv_domain_nm   = iv_domain_nm
*        is_self_define = is_self_define
*      IMPORTING
*        et_value_set   = lt_value_set.
    ENDIF.

    l_node_info->set_attribute_value_set( name = iv_attr_nm value_set = lt_value_set ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_COMMON=>SHOW_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_COMP                        TYPE REF TO OBJECT
* | [--->] IV_MSG                         TYPE        CSEQUENCE
* | [--->] IV_TYPE                        TYPE        CHAR1
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD show_message.

* get message manager
    DATA lo_api_controller     TYPE REF TO if_wd_controller.
    DATA lo_message_manager    TYPE REF TO if_wd_message_manager.
    DATA lv_type TYPE i.

    CASE iv_type.
      WHEN 'E'.
        lv_type = 2.
      WHEN 'S'.
        lv_type = 0.
      WHEN 'W'.
        lv_type = 1.
      WHEN OTHERS.
    ENDCASE.

    TRY .
        CALL METHOD iv_comp->('WD_GET_API')
          RECEIVING
            result = lo_api_controller.

      CATCH cx_root.
    ENDTRY.
*    lo_api_controller ?= iv_comp->wd_get_api( ).

    CALL METHOD lo_api_controller->get_message_manager
      RECEIVING
        message_manager = lo_message_manager.

* report message
    CALL METHOD lo_message_manager->report_message
      EXPORTING
        message_text = iv_msg
        message_type = lv_type
*       params       =
*       msg_user_data             =
*       is_permanent = ABAP_FALSE
*       scope_permanent_msg       = CO_MSG_SCOPE_CONTROLLER
*       view         =
*       show_as_popup             =
*       controller_permanent_msg  =
*       msg_index    =
*       cancel_navigation         =
*       enable_message_navigation =
*       component    =
*  receiving
*       message_id   =
      .

  ENDMETHOD.
ENDCLASS.