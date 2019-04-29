FUNCTION zcs_material_data.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(INPUT) TYPE  STRING
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  STRING
*"----------------------------------------------------------------------
  DATA: lt_crm TYPE TABLE OF zmms0001,
        ls_crm LIKE LINE OF lt_crm.
  DATA: lt_return LIKE TABLE OF zmms0007.
*--start of update huangzh 20180424
  "  json 转 abap
*  CALL TRANSFORMATION id
*                      SOURCE XML input
*                      RESULT item = lt_crm.
  DATA: lt_area TYPE TABLE OF zmms_area WITH HEADER LINE.

  TYPES: BEGIN OF ty_area,
           berid TYPE zmms0003-berid,
           wglif TYPE zmms0003-wglif,
         END OF ty_area.
  TYPES: ty_t_area TYPE ty_area OCCURS 0.
  DATA: ls_area TYPE ty_area.

  DATA: BEGIN OF ls_data.
      INCLUDE STRUCTURE zmms0001.
  DATA: area TYPE ty_t_area.
  DATA: END OF ls_data.
  DATA: lt_data LIKE TABLE OF ls_data.

  "去掉开头的多余字段
  SHIFT input LEFT DELETING LEADING '{"ITEM":'.
  "去掉结尾的多余字段
  SHIFT input RIGHT DELETING TRAILING '}'.
  "解析json传过来的内表套内表结构
  /ui2/cl_json=>deserialize( EXPORTING json = input
                             CHANGING  data = lt_data ).

  LOOP AT lt_data INTO ls_data.
    CLEAR ls_crm.
    MOVE-CORRESPONDING ls_data TO ls_crm.

    REFRESH: lt_area[].
    LOOP AT ls_data-area INTO ls_area.
      lt_area-matnr = ls_data-matnr.
      lt_area-lifnr = ls_data-lifnr.
      lt_area-berid = ls_area-berid.
      lt_area-wglif = ls_area-wglif.
      APPEND lt_area.
    ENDLOOP.
    IF ls_data-area[] IS INITIAL.
      lt_area-matnr = ls_data-matnr.
      lt_area-lifnr = ls_data-lifnr.
      lt_area-berid = ''.
      lt_area-wglif = ''.
      APPEND lt_area.
    ENDIF.

    APPEND ls_crm TO lt_crm.
  ENDLOOP.
*--end of update huangzh 20180424
  LOOP AT lt_crm INTO ls_crm.
    REPLACE ALL OCCURRENCES OF '々' IN ls_crm-maktx  WITH '\'.
    REPLACE ALL OCCURRENCES OF '♂' IN ls_crm-maktx  WITH '"'.
    REPLACE ALL OCCURRENCES OF '♀' IN ls_crm-maktx  WITH '<'.
    REPLACE ALL OCCURRENCES OF '∮' IN ls_crm-maktx  WITH '&'.
    REPLACE ALL OCCURRENCES OF '々' IN ls_crm-maktxlong  WITH '\'.
    REPLACE ALL OCCURRENCES OF '♂' IN ls_crm-maktxlong  WITH '"'.
    REPLACE ALL OCCURRENCES OF '♀' IN ls_crm-maktxlong  WITH '<'.
    REPLACE ALL OCCURRENCES OF '∮' IN ls_crm-maktxlong  WITH '&'.
    REPLACE ALL OCCURRENCES OF '々' IN ls_crm-text01  WITH '\'.
    REPLACE ALL OCCURRENCES OF '♂' IN ls_crm-text01  WITH '"'.
    REPLACE ALL OCCURRENCES OF '♀' IN ls_crm-text01  WITH '<'.
    REPLACE ALL OCCURRENCES OF '∮' IN ls_crm-text01  WITH '&'.
    REPLACE ALL OCCURRENCES OF '々' IN ls_crm-text02  WITH '\'.
    REPLACE ALL OCCURRENCES OF '♂' IN ls_crm-text02  WITH '"'.
    REPLACE ALL OCCURRENCES OF '♀' IN ls_crm-text02  WITH '<'.
    REPLACE ALL OCCURRENCES OF '∮' IN ls_crm-text02  WITH '&'.
    REPLACE ALL OCCURRENCES OF '々' IN ls_crm-z001  WITH '\'.
    REPLACE ALL OCCURRENCES OF '♂' IN ls_crm-z001  WITH '"'.
    REPLACE ALL OCCURRENCES OF '♀' IN ls_crm-z001  WITH '<'.
    REPLACE ALL OCCURRENCES OF '∮' IN ls_crm-z001  WITH '&'.
    REPLACE ALL OCCURRENCES OF '々' IN ls_crm-z002  WITH '\'.
    REPLACE ALL OCCURRENCES OF '♂' IN ls_crm-z002  WITH '"'.
    REPLACE ALL OCCURRENCES OF '♀' IN ls_crm-z002  WITH '<'.
    REPLACE ALL OCCURRENCES OF '∮' IN ls_crm-z002  WITH '&'.
    REPLACE ALL OCCURRENCES OF '々' IN ls_crm-z003  WITH '\'.
    REPLACE ALL OCCURRENCES OF '♂' IN ls_crm-z003  WITH '"'.
    REPLACE ALL OCCURRENCES OF '♀' IN ls_crm-z003  WITH '<'.
    REPLACE ALL OCCURRENCES OF '∮' IN ls_crm-z003  WITH '&'.
    REPLACE ALL OCCURRENCES OF '々' IN ls_crm-z004  WITH '\'.
    REPLACE ALL OCCURRENCES OF '♂' IN ls_crm-z004  WITH '"'.
    REPLACE ALL OCCURRENCES OF '♀' IN ls_crm-z004  WITH '<'.
    REPLACE ALL OCCURRENCES OF '∮' IN ls_crm-z004  WITH '&'.
    REPLACE ALL OCCURRENCES OF '々' IN ls_crm-groes  WITH '\'.
    REPLACE ALL OCCURRENCES OF '♂' IN ls_crm-groes  WITH '"'.
    REPLACE ALL OCCURRENCES OF '♀' IN ls_crm-groes  WITH '<'.
    REPLACE ALL OCCURRENCES OF '∮' IN ls_crm-groes  WITH '&'.


    MODIFY lt_crm FROM ls_crm.

  ENDLOOP.
  DATA: wa_return LIKE zmms0007.
  DATA: wa_len type i.
  read TABLE lt_crm into ls_crm index 1.
  wa_len = strlen( ls_crm-matnr ).
  IF wa_len > 6.
    CLEAR wa_return.
    wa_return-type = 'ERROR'.
    wa_return-msg = '物料超过6位'.
   append wa_return to lt_return.
    else.

    CALL FUNCTION 'ZCS0001_MATERIAL'
      TABLES
        it_matnr  = lt_crm
        it_area   = lt_area
        et_return = lt_return.
  ENDIF.

  "  abap 转json
  DATA(json_writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
  CALL TRANSFORMATION id
                      SOURCE item  = lt_return
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