CLASS zcl_wd_message DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  CONSTANTS gv_required TYPE string VALUE '必须输入' ##NO_TEXT.

  CLASS-METHODS show_msg
    IMPORTING
      !ir_if_wd_controller TYPE REF TO if_wd_controller
      !iv_type TYPE char1
      !iv_message TYPE string .
  CLASS-METHODS show_msg_detail
    IMPORTING
      !iv_papr1 TYPE csequence OPTIONAL
      !iv_type TYPE char1 OPTIONAL
      !ir_element TYPE REF TO if_wd_context_element OPTIONAL
      !iv_attribute TYPE string OPTIONAL
      !iv_msgid TYPE sy-msgid DEFAULT '00'
      !iv_msgno TYPE sy-msgno DEFAULT '001'
      !iv_msgty TYPE sy-msgty OPTIONAL
      !iv_msgv1 TYPE sy-msgv1 OPTIONAL
      !iv_msgv2 TYPE sy-msgv2 OPTIONAL
      !iv_msgv3 TYPE sy-msgv3 OPTIONAL
      !iv_msgv4 TYPE sy-msgv4 OPTIONAL
      !ir_controller TYPE REF TO if_wd_controller OPTIONAL .
PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WD_MESSAGE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_MESSAGE=>SHOW_MSG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_IF_WD_CONTROLLER            TYPE REF TO IF_WD_CONTROLLER
* | [--->] IV_TYPE                        TYPE        CHAR1
* | [--->] IV_MESSAGE                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD show_msg.
    DATA lo_message_manager    TYPE REF TO if_wd_message_manager.

    CALL METHOD ir_if_wd_controller->get_message_manager
      RECEIVING
        message_manager = lo_message_manager.

    CASE iv_type.
        "成功
      WHEN 'S'.
        CALL METHOD lo_message_manager->report_success
          EXPORTING
            message_text = iv_message.
        "常规
      WHEN ''.
        CALL METHOD lo_message_manager->report_message
          EXPORTING
            message_text = iv_message.
        "警告
      WHEN 'W'.
        CALL METHOD lo_message_manager->report_warning
          EXPORTING
            message_text = iv_message.
        "错误
      WHEN 'E'.
        CALL METHOD lo_message_manager->report_error_message
          EXPORTING
            message_text = iv_message.
    ENDCASE.
*    MESSAGE ID 'ZTR01' TYPE 'E' NUMBER 002 INTO DATA(lv_msg).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_MESSAGE=>SHOW_MSG_DETAIL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PAPR1                       TYPE        CSEQUENCE(optional)
* | [--->] IV_TYPE                        TYPE        CHAR1(optional)
* | [--->] IR_ELEMENT                     TYPE REF TO IF_WD_CONTEXT_ELEMENT(optional)
* | [--->] IV_ATTRIBUTE                   TYPE        STRING(optional)
* | [--->] IV_MSGID                       TYPE        SY-MSGID (default ='00')
* | [--->] IV_MSGNO                       TYPE        SY-MSGNO (default ='001')
* | [--->] IV_MSGTY                       TYPE        SY-MSGTY(optional)
* | [--->] IV_MSGV1                       TYPE        SY-MSGV1(optional)
* | [--->] IV_MSGV2                       TYPE        SY-MSGV2(optional)
* | [--->] IV_MSGV3                       TYPE        SY-MSGV3(optional)
* | [--->] IV_MSGV4                       TYPE        SY-MSGV4(optional)
* | [--->] IR_CONTROLLER                  TYPE REF TO IF_WD_CONTROLLER(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD show_msg_detail.

    DATA lo_api_controller     TYPE REF TO if_wd_controller.
    DATA lo_message_manager    TYPE REF TO if_wd_message_manager.
    DATA lv_cancel_navigation  TYPE c.
    DATA ls_symsg              TYPE symsg.

    ls_symsg-msgid = iv_msgid.
    ls_symsg-msgno = iv_msgno.
    IF iv_msgty IS NOT INITIAL.
      ls_symsg-msgty = iv_msgty.
    ELSE.
      ls_symsg-msgty = iv_type.
    ENDIF.
    ls_symsg-msgv1 = iv_msgv1.
    ls_symsg-msgv2 = iv_msgv2.
    ls_symsg-msgv3 = iv_msgv3.
    ls_symsg-msgv4 = iv_msgv4.

    IF iv_type = 'E'.
      lv_cancel_navigation = 'X'.
    ENDIF.

    "get message manager
    lo_api_controller = ir_controller.
    CALL METHOD lo_api_controller->get_message_manager( RECEIVING message_manager = lo_message_manager ).

    "report message
    IF ir_element IS INITIAL.
      CALL METHOD lo_message_manager->report_t100_message
        EXPORTING
          msgid             = iv_msgid
          msgno             = iv_msgno
          msgty             = ls_symsg-msgty
          p1                = iv_msgv1
          p2                = iv_msgv2
          p3                = iv_msgv3
          p4                = iv_msgv4
          cancel_navigation = lv_cancel_navigation.
    ELSE.
      CALL METHOD lo_message_manager->report_attribute_t100_message
        EXPORTING
          msg               = ls_symsg
          element           = ir_element
          attribute_name    = iv_attribute
          cancel_navigation = lv_cancel_navigation.
    ENDIF.
  ENDMETHOD.
ENDCLASS.