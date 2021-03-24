DATA:lv_wind_name      TYPE string,
        lv_usage_name     TYPE string,
lo_window_manager TYPE REF TO if_wd_window_manager,
        lo_api_component  TYPE REF TO if_wd_component,
ls_canc_action    TYPE wdr_popup_button_action,
lv_title          TYPE string,

  lv_wind_name = 'W_MAIN_JK'.
    lv_usage_name  = 'COM_2004'.

    lo_api_component           = wd_comp_controller->wd_get_api( ).
    lo_window_manager          = lo_api_component->get_window_manager( ).
    ls_canc_action-action_name = '*'.

    wd_comp_controller->lo_window = lo_window_manager->create_and_open_popup(
        window_name          = lv_wind_name
        component_usage_name = lv_usage_name
        title                = lv_title
        message_type         = if_wd_window=>co_msg_type_none
        message_display_mode = if_wd_window=>co_msg_display_mode_selected
        cancel_action        = ls_canc_action
    ).

*    DATA: lo_component_usage TYPE REF TO if_wd_component_usage,
*          lo_view_controller TYPE REF TO if_wd_view_controller.

*    TRY.
*    CALL METHOD lo_component_usage->create_component
*      EXPORTING
*        component_name = lv_usage_name
*       configuration_id =
*       model_usage    =
*       assistance_class =
*       model_usage_name =
    .
*     CATCH cx_wd_runtime_api .
*    ENDTRY.
*
*  CALL METHOD lo_view_controller->if_wd_controller~get_component
*    RECEIVING
*      component = lv_usage_name
*      .
*
    DATA lo_cmp_usage TYPE REF TO if_wd_component_usage.
    DATA lo_listener TYPE REF TO if_wd_controller.

    lo_cmp_usage =   wd_this->wd_cpuse_com_2004( ).
    IF lo_cmp_usage->has_active_component( ) IS INITIAL.
      lo_cmp_usage->create_component( ).
    ENDIF.

    lo_listener = wd_this->wd_get_api( ).

    CALL METHOD lo_cmp_usage->add_event_handler
      EXPORTING
        listener        = lo_listener
        handler_name    = 'ONACTION_POPUP_CLOSE'
        controller_name = 'INTERFACECONTROLLER'
        event_name      = 'POPUP_CLOSE'.



    CALL METHOD wd_comp_controller->lo_window->set_window_maximize
      EXPORTING
        is_maximized = 'X'.
    wd_comp_controller->lo_window->open( ).