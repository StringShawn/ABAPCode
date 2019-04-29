FUNCTION ZFICO0003_RFC.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(INPUT) TYPE  STRING
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  STRING
*"----------------------------------------------------------------------

  DATA:ls_data TYPE zsfico0003i.
  DATA:ls_output TYPE zsfico0003o.

  "去掉开头的多余字段
  SHIFT input LEFT DELETING LEADING '{"ITEM":'.
  "去掉结尾的多余字段
  SHIFT input RIGHT DELETING TRAILING '}'.
  "解析json传过来的内表套内表结构
  /ui2/cl_json=>deserialize( EXPORTING json = input
                             CHANGING  data = ls_data ).

  CALL FUNCTION 'ZFICO0003'
    EXPORTING
      im_data       = ls_data
   IMPORTING
     EX_DATA       = ls_output
            .

"  abap 转json
  DATA(json_writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
  CALL TRANSFORMATION id
                      SOURCE item  = ls_output
                      RESULT XML json_writer.
  DATA(json) = json_writer->get_output( ).
  DATA: lv_json TYPE string .
  CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
    EXPORTING
      im_xstring  = json
      im_encoding = 'UTF-8'
    IMPORTING
      ex_string   = output.


ENDFUNCTION.