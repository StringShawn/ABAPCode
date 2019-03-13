FUNCTION ZSAVE_ELEMENT_VALUE_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     VALUE(LS_ELEMENT) TYPE  ZELEMENT_LIST
*"     VALUE(CHANGE_FLAG) TYPE  C DEFAULT 'X'
*"  TABLES
*"      RANGE_TAB TYPE  TABLE OPTIONAL
*"--------------------------------------------------------------------

  DATA:BEGIN OF ls_id,
         code_source TYPE char40,
         element_id  TYPE zelement_id,
       END OF ls_id.


  FIELD-SYMBOLS:<range_tab> TYPE STANDARD TABLE.
  FIELD-SYMBOLS:<range_wa> TYPE any.
  DATA gr_tab TYPE REF TO data.
  DATA gr_wa TYPE REF TO data.

  DATA gt_components TYPE abap_component_tab WITH HEADER LINE.

  DATA gr_structdescr TYPE REF TO cl_abap_structdescr.
  DATA gr_tabledescr TYPE REF TO cl_abap_tabledescr.

  CLEAR ls_element-relid.
  CLEAR ls_element-srtf2.
  CLEAR ls_element-clustr.
  CLEAR ls_element-clustd.

  gt_components-name = 'SIGN'.
  gt_components-type ?= cl_abap_elemdescr=>get_c( p_length = 1 ).
  APPEND gt_components.

  gt_components-name = 'OPTION'.
  gt_components-type ?= cl_abap_elemdescr=>get_c( p_length = 2 ).
  APPEND gt_components.

  FIND FIRST OCCURRENCE OF '-' IN ls_element-fieldname.
  IF sy-subrc = 0.
    DATA temp_data TYPE REF TO data.

    CREATE DATA temp_data TYPE (ls_element-fieldname).

    gt_components-name = 'LOW'.
    gt_components-type ?= cl_abap_elemdescr=>describe_by_data_ref( temp_data ).
    APPEND gt_components.

    gt_components-name = 'HIGH'.
    gt_components-type ?= cl_abap_elemdescr=>describe_by_data_ref( temp_data ).
    APPEND gt_components.
  ELSE.

    gt_components-name = 'LOW'.
    gt_components-type ?= cl_abap_elemdescr=>describe_by_name( ls_element-fieldname ).
    APPEND gt_components.

    gt_components-name = 'HIGH'.
    gt_components-type ?= cl_abap_elemdescr=>describe_by_name( ls_element-fieldname ).
    APPEND gt_components.
  ENDIF.

  gr_structdescr ?= cl_abap_structdescr=>create( gt_components[] ).
  gr_tabledescr ?= cl_abap_tabledescr=>create( gr_structdescr ).

  CREATE DATA gr_wa TYPE HANDLE gr_structdescr.
  CREATE DATA gr_tab TYPE HANDLE gr_tabledescr.

  ASSIGN gr_wa->* TO <range_wa>.
  ASSIGN gr_tab->* TO <range_tab>.

  ls_id-code_source = ls_element-code_source.
  ls_id-element_id = ls_element-element_id.

  <range_tab> = CORRESPONDING #( range_tab[] ).

  IF change_flag = 'X'.
    ls_element-chdat = sy-datum.
    ls_element-chtim = sy-uzeit.
  ENDIF.

  EXPORT <range_tab> TO DATABASE zelement_list(el) ID ls_id FROM ls_element.

ENDFUNCTION.
