
  DATA: lo_api_controller TYPE REF TO if_wd_controller,
        lr_component      TYPE REF TO if_wd_component,
        lr_window_manager TYPE REF TO if_wd_window_manager,
        lo_window         TYPE REF TO if_wd_window.

* 弹出输入框
  lr_component = wd_comp_controller->wd_get_api( )."?= wd_this->wd_get_api( ).
  lr_window_manager = lr_component->get_window_manager( ).
  lo_window = lr_window_manager->create_window( window_name = 'W_SELECT_AR'
                                                button_kind = if_wd_window=>co_buttons_none
*                                                title = wd_assist->if_wd_component_assistance~get_text( '033' )
                                               ).
  lo_window->set_window_size( width = '1200px' height = '750px' size_is_fix = 'X').
  lo_window->set_close_in_any_case( abap_true ).
  lo_window->open( ).
