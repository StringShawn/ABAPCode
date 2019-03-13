FUNCTION-POOL ZFUNCTION_UTIL_TEMP.               "MESSAGE-ID ..

DATA ok_code TYPE sy-ucomm.

DATA:BEGIN OF ls_node,
       name  TYPE string,
       value TYPE string,
     END OF ls_node,
     lt_node LIKE STANDARD TABLE OF ls_node WITH HEADER LINE.


DATA edit_container TYPE REF TO cl_gui_custom_container.
DATA edit_text TYPE REF TO cl_gui_textedit.

DATA:BEGIN OF ls_edit_text,
       readonly    TYPE i,
       text_string TYPE string,
       toolbar_txt TYPE char30,
       answer      TYPE c,
     END OF ls_edit_text.

DATA:BEGIN OF ls_list_text,
       toolbar_txt  TYPE string,
       fieldtxt     TYPE char20,
       list_value TYPE string,
       list_field   TYPE fieldname,
     END OF ls_list_text.
FIELD-SYMBOLS <list_tab> TYPE STANDARD TABLE.

DATA ls_dialogbox_element TYPE zdialogbox_element.
DATA lt_fcat TYPE lvc_t_fcat WITH HEADER LINE.

DATA cl_alv TYPE REF TO cl_gui_alv_grid.
DATA cv_event TYPE REF TO zcl_alv_event_receiver.

DATA cl_dialogbox_container TYPE REF TO cl_gui_dialogbox_container.
DATA cl_dialogbox_event TYPE REF TO zcl_dialogbox_event_receiver.
DATA lr_data TYPE REF TO data.
FIELD-SYMBOLS <lt_alv_tab> TYPE ANY TABLE.

INCLUDE zexcel_macro.

INCLUDE zfmparavalsave.
* INCLUDE LZFUNCTION_UTILD...                " Local class definition
