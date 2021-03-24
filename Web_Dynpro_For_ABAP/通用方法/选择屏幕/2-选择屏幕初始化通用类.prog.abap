CLASS zcl_wd_selopts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-DATA gr_usage TYPE REF TO if_wd_component_usage .
    CLASS-DATA gr_opts TYPE REF TO if_wd_select_options_20 .

    CLASS-METHODS init_opts
      IMPORTING
        !ir_cpifc       TYPE REF TO iwci_wd_select_options_20 OPTIONAL
        VALUE(it_attrs) TYPE wdr_so_t_attributes OPTIONAL
        !iv_rows        TYPE int4 OPTIONAL
        !it_init_data   TYPE wdr_so_t_values OPTIONAL .
    CLASS-METHODS init_usage
      IMPORTING
        !ir_usage TYPE REF TO if_wd_component_usage .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WD_SELOPTS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_SELOPTS=>INIT_OPTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_CPIFC                       TYPE REF TO IWCI_WD_SELECT_OPTIONS_20(optional)
* | [--->] IT_ATTRS                       TYPE        WDR_SO_T_ATTRIBUTES(optional)
* | [--->] IV_ROWS                        TYPE        INT4(optional)
* | [--->] IT_INIT_DATA                   TYPE        WDR_SO_T_VALUES(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD init_opts.

    DATA lo_interfacecontroller TYPE REF TO iwci_wd_select_options_20.

    DATA ls_vis TYPE wdr_so_s_general_visibility.
    DATA ls_global TYPE wdr_so_s_global_options.

    lo_interfacecontroller =  ir_cpifc.

    IF iv_rows = 0.
      ls_global-nr_default_rows = 15.
    ELSE.
      ls_global-nr_default_rows = iv_rows .
    ENDIF.

    ls_vis-show_button_search = abap_true.
    ls_vis-show_button_clear = abap_true.
    ls_vis-img_button-img_btn_search = '~Icon/Search'.
    ls_vis-img_button-img_btn_clear = '~Icon/Delete'.
    ls_vis-show_max_nr_rows = abap_true.
    ls_global-default_max_nr_rows = '200' .
*    ls_global-allow_personalization = 'X'.


    DATA : ls_per TYPE wdr_so_s_personalization .
*    ls_per-default_search
*    ls_per-non_deletable
*    ls_per-start_default_search
*    ls_per-start_any_search
*    ls_per-show_collapsed = 'X'.
*    ls_per-show_all_initially  = 'X'.

        gr_opts = lo_interfacecontroller->init_select_options(
                                            exporting  general_visibility   = ls_vis
                                                       global_options       = ls_global
*                                                       personalization       =  ls_per
                                                        ).
    gr_opts->add_attributes( EXPORTING attributes = it_attrs initial_data = it_init_data ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WD_SELOPTS=>INIT_USAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IR_USAGE                       TYPE REF TO IF_WD_COMPONENT_USAGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD init_usage.
    gr_usage =   ir_usage.
    gr_usage->delete_component( ).
    IF gr_usage->has_active_component( ) IS INITIAL.
      gr_usage->create_component( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.