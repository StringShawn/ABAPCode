*----------------------------------------------------------------------*
***INCLUDE LZFUNCTION_UTILF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_XML_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE  text
*      -->P_0      text
*----------------------------------------------------------------------*
FORM get_xml_data USING p_node TYPE REF TO if_ixml_node deep TYPE i.

  DATA nodetype TYPE i.
  DATA attrslen TYPE i.
  DATA attrs TYPE REF TO if_ixml_named_node_map.
  DATA attr TYPE REF TO if_ixml_node.

  nodetype = p_node->get_type( ).
  CASE p_node->get_type( ).

    WHEN if_ixml_node=>co_node_element."处理元素节点

      lt_node-name = p_node->get_name( ).
      IF p_node->get_depth( ) = 1."1 表示最底层 以此往上类推
        lt_node-value = p_node->get_value( ).
      ELSE.
        lt_node-value = space.
      ENDIF.

      APPEND lt_node.

      attrs = p_node->get_attributes( ).
      attrslen = attrs->get_length( ).
      DO attrslen TIMES.
        attr = attrs->get_item( sy-index - 1 ).

        lt_node-name = p_node->get_name( ).
        lt_node-value = p_node->get_value( ).
        APPEND lt_node.
      ENDDO.
  ENDCASE.

  DATA childs TYPE REF TO if_ixml_node_list.
  DATA child TYPE REF TO if_ixml_node.
  DATA childslen TYPE i.
  DATA deep2 TYPE i.

  childs = p_node->get_children( ).
  childslen = childs->get_length( ).

  deep2 =  deep + 1.

  DO childslen  TIMES.

    child =  childs->get_item( sy-index - 1 ).

    PERFORM get_xml_data USING child deep2.

  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DIGITAL_TO_CHINESE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_STRDECT  text
*      <--P_STRDECD  text
*----------------------------------------------------------------------*
FORM change_digital_to_chinese  USING digitalin TYPE c CHANGING digitalout TYPE c.

  IF digitalin = '0'.
    digitalout = '零'.
  ELSEIF digitalin = '1'.
    digitalout = '壹'.
  ELSEIF digitalin = '2'.
    digitalout = '贰'.
  ELSEIF digitalin = '3'.
    digitalout = '叁'.
  ELSEIF digitalin = '4'.
    digitalout = '肆'.
  ELSEIF digitalin = '5'.
    digitalout = '伍'.
  ELSEIF digitalin = '6'.
    digitalout = '陆'.
  ELSEIF digitalin = '7'.
    digitalout = '柒'.
  ELSEIF digitalin = '8'.
    digitalout = '捌'.
  ELSEIF digitalin = '9'.
    digitalout = '玖'.
  ELSE.
    digitalout = '×'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POPUP_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM popup_alv .

  DATA ls_layout TYPE lvc_s_layo.

  IF ls_dialogbox_element IS INITIAL.
    ls_dialogbox_element-width = 600.
    ls_dialogbox_element-height = 400.
    ls_dialogbox_element-top = 200.
    ls_dialogbox_element-left = 300.
  ENDIF.

  CREATE OBJECT cl_dialogbox_container
    EXPORTING
      width    = ls_dialogbox_element-width
      height   = ls_dialogbox_element-height
      lifetime = 0
      top      = ls_dialogbox_element-top
      left     = ls_dialogbox_element-left
      caption  = ls_dialogbox_element-caption
      metric   = cl_gui_container=>metric_pixel
    EXCEPTIONS
      OTHERS   = 1.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.

    cl_dialogbox_event = NEW zcl_dialogbox_event_receiver( ).
    cl_dialogbox_event->screen_close_perform = 'CLOSE_DIALOGBOX'.
    cl_dialogbox_event->g_program = 'SAPLZFUNCTION_UTIL'.

    cl_dialogbox_event->handle_event( cl_dialogbox_container ).
  ENDIF.

  cl_alv = NEW cl_gui_alv_grid( i_parent = cl_dialogbox_container i_appl_events = 'X' ).

  IF cv_event IS BOUND.
    cv_event->handle_event( CHANGING cl_alv = cl_alv ).
  ENDIF.

  ls_layout-zebra = 'X' .
  ls_layout-cwidth_opt = 'X' .
  ls_layout-no_rowmark = ' ' .
  ls_layout-sel_mode = 'A'.

  CALL METHOD cl_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = ls_layout
    CHANGING
      it_outtab                     = <lt_alv_tab>
      it_fieldcatalog               = lt_fcat[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CLOSE_DIALOGBOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM close_dialogbox .

  cl_alv->free( ).
  FREE cl_alv.
  FREE cv_event.

  cl_dialogbox_container->free( ).
  FREE cl_dialogbox_container.
  FREE cl_dialogbox_event.

ENDFORM.
