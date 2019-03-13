FUNCTION ZGET_ELEMENT_VALUE_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     VALUE(CODE_SOURCE) TYPE  CHAR40
*"     VALUE(ELEMENT_ID) TYPE  ZELEMENT_ID
*"  EXPORTING
*"     REFERENCE(LS_ELEMENT) TYPE  ZELEMENT_LIST
*"  TABLES
*"      RANGE_TAB TYPE  TABLE
*"  EXCEPTIONS
*"      NO_ELEMENT_ID
*"--------------------------------------------------------------------

  DATA fieldname TYPE name_feld.

  DATA:BEGIN OF ls_id,
         code_source TYPE char40,
         element_id  TYPE zelement_id,
       END OF ls_id.

  SELECT SINGLE fieldname INTO fieldname FROM zelement_list
    WHERE code_source = code_source AND element_id = element_id.

  IF sy-subrc <> 0.
    RAISE no_element_id.
  ELSE.
    ls_id-code_source = code_source.
    ls_id-element_id = element_id.
  ENDIF.

  FIELD-SYMBOLS:<range_tab> TYPE STANDARD TABLE.
  FIELD-SYMBOLS:<range_wa> TYPE any.
  DATA gr_tab TYPE REF TO data.
  DATA gr_wa TYPE REF TO data.

  DATA gt_components TYPE abap_component_tab WITH HEADER LINE.

  DATA gr_structdescr TYPE REF TO cl_abap_structdescr.
  DATA gr_tabledescr TYPE REF TO cl_abap_tabledescr.

  gt_components-name = 'SIGN'.
  gt_components-type ?= cl_abap_elemdescr=>get_c( p_length = 1 ).
  APPEND gt_components.

  gt_components-name = 'OPTION'.
  gt_components-type ?= cl_abap_elemdescr=>get_c( p_length = 2 ).
  APPEND gt_components.

  FIND FIRST OCCURRENCE OF '-' IN fieldname.
  IF sy-subrc = 0.
    DATA temp_data TYPE REF TO data.

    CREATE DATA temp_data TYPE (fieldname).

    gt_components-name = 'LOW'.
    gt_components-type ?= cl_abap_elemdescr=>describe_by_data_ref( temp_data ).
    APPEND gt_components.

    gt_components-name = 'HIGH'.
    gt_components-type ?= cl_abap_elemdescr=>describe_by_data_ref( temp_data ).
    APPEND gt_components.
  ELSE.

    gt_components-name = 'LOW'.
    gt_components-type ?= cl_abap_elemdescr=>describe_by_name( fieldname ).
    APPEND gt_components.

    gt_components-name = 'HIGH'.
    gt_components-type ?= cl_abap_elemdescr=>describe_by_name( fieldname ).
    APPEND gt_components.
  ENDIF.

  gr_structdescr ?= cl_abap_structdescr=>create( gt_components[] ).
  gr_tabledescr ?= cl_abap_tabledescr=>create( gr_structdescr ).

  CREATE DATA gr_wa TYPE HANDLE gr_structdescr.
  CREATE DATA gr_tab TYPE HANDLE gr_tabledescr.

  ASSIGN gr_wa->* TO <range_wa>.
  ASSIGN gr_tab->* TO <range_tab>.

  IMPORT <range_tab> FROM DATABASE zelement_list(el) ID ls_id TO ls_element.

  range_tab[] = CORRESPONDING #( <range_tab> ).
ENDFUNCTION.
