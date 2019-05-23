TYPES:BEGIN OF ly_item,
        werks TYPE ekpo-werks,
        menge TYPE CHAR20,
      END OF ly_item.

TYPES:BEGIN OF ly_head,
        lcbh TYPE char20,
      END OF ly_head.

TYPES:BEGIN OF ly_data,
        head TYPE ly_head,
        item TYPE ly_item OCCURS 0,
      END OF ly_data.

DATA:lt_data TYPE  ly_data.

DATA:lv_string TYPE string VALUE '{"HEAD":{"LCBH":"WZ-2019-05-0004"},"ITEM":[{"WERKS":"1000","MENGE":"1"},{"WERKS":"1000","MENGE":"2"},{"WERKS":"1000","MENGE":"3"}]}'.
DATA:LV_JSON TYPE STRING.
/ui2/cl_json=>deserialize( EXPORTING json = lv_string
                           CHANGING  data = lt_data ).
lv_json = /ui2/cl_json=>serialize( data = lt_data ).
BREAK-POINT.