DATA:BEGIN OF lt_item OCCURS 0.
    INCLUDE STRUCTURE zspr_sap2oa.
DATA END OF lt_item.

DATA lt_item1 LIKE STANDARD TABLE OF lt_item.

DATA:BEGIN OF lt_head ,
       lcbh TYPE char25.
DATA END OF lt_head.

DATA: json_ser TYPE REF TO cl_trex_json_serializer,
      json_des TYPE REF TO cl_trex_json_deserializer.
DATA:ls_string TYPE string.
DATA:lv_begin TYPE sy-fdpos.
DATA:lv_end TYPE i.
DATA:lv_length TYPE i.
DATA:ls_string_head TYPE string,
     ls_string_item TYPE string.
PARAMETERS jsonstr TYPE string.
ls_string = jsonstr.
SEARCH jsonstr FOR '"HEAD":' IN CHARACTER MODE.
lv_begin = sy-fdpos + 7.
SEARCH jsonstr FOR ',' IN CHARACTER MODE.
lv_end  = sy-fdpos - lv_begin.

lv_length = strlen( ls_string ).
ls_string_head = ls_string+lv_begin(lv_end).
SEARCH jsonstr FOR '"ITEM":' IN CHARACTER MODE.
lv_end = sy-fdpos + 7.
lv_length = lv_length - lv_end.
ls_string_item = ls_string+lv_end(lv_length).
SHIFT ls_string_item RIGHT DELETING TRAILING '}'.

/ui2/cl_json=>deserialize( EXPORTING json = ls_string_head
                           CHANGING  data = lt_head ).
BREAK-POINT.
/ui2/cl_json=>deserialize( EXPORTING json = ls_string_item
                           CHANGING  data = lt_item1 ).