*----Get URL
    CLEAR lv_url.
    CALL METHOD cl_wd_utilities=>construct_wd_url
      EXPORTING
        application_name = 'ZWDC_TR_4005_MAIN'
      IMPORTING
        out_absolute_url = lv_url.

    CALL METHOD cl_http_server=>if_http_server~append_field_url
      EXPORTING
        name  = 'IV_SHOW_MODE' "模式
        value = lv_show_mode
      CHANGING
        url   = lv_url.

    CALL METHOD cl_http_server=>if_http_server~append_field_url
      EXPORTING
        name  = 'IV_DFA_RGT_NO' "注册直接融资申请单号
        value = lv_ifa_no
      CHANGING
        url   = lv_url.

    CALL METHOD cl_http_server=>if_http_server~append_field_url
      EXPORTING
        name  = 'IV_SEQNO' "序号
        value = lv_seqno
      CHANGING
        url   = lv_url.

    lr_component ?= wd_comp_controller->wd_get_api( ).
    lr_window_manager = lr_component->get_window_manager( ).

    CALL METHOD lr_window_manager->create_external_window
      EXPORTING
        url            = lv_url
        modal          = abap_true
        has_menubar    = abap_true
        is_resizable   = abap_true
        has_scrollbars = abap_true
        has_statusbar  = abap_true
        has_toolbar    = abap_true
        has_location   = abap_true
      RECEIVING
        window         = lo_window.
    lo_window->open( ).