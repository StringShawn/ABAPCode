FUNCTION ZGET_TRADE_ODLIST_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(I_BHNUC) TYPE  CHAR10
*"     REFERENCE(I_PATHN) TYPE  CHAR10 OPTIONAL
*"     REFERENCE(I_TYPE) TYPE  CHAR1
*"  TABLES
*"      LT_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      NO_ODLIST
*"--------------------------------------------------------------------

  REFRESH gt_odlist_node.
  REFRESH gt_yhlist_node.
  REFRESH gt_thlist_node.
  REFRESH gt_kythlist_node.
  CLEAR gs_yh_list.
  CLEAR gs_fh_list.
  CLEAR gs_th_list.
  CLEAR gs_kyth_list.

  IF i_type = '1' OR i_type = '3'.

    PERFORM show_yh_odlist USING i_bhnuc i_type.

    IF gs_yh_list IS INITIAL.
      RAISE no_odlist.
    ENDIF.

  ELSEIF i_type = '2' OR i_type = '4' OR i_type = '5'. " add type 5 handyxh 20181115

    PERFORM get_odlist_head USING i_type i_bhnuc i_pathn.

    PERFORM show_jm_odlist TABLES lt_return USING i_type i_bhnuc.

    IF gs_fh_list IS INITIAL AND gs_th_list IS INITIAL AND gs_kyth_list IS INITIAL.
      RAISE no_odlist.
    ENDIF.

  ENDIF.

  CALL SCREEN 0501 STARTING AT 30 1.

  REFRESH lt_return.
  REFRESH gt_odlist_node.
  REFRESH gt_yhlist_node.
  REFRESH gt_thlist_node.
  REFRESH gt_kythlist_node.
  CLEAR gs_fh_list.
  CLEAR gs_th_list.
  CLEAR gs_yh_list.
  CLEAR gs_kyth_list.

  IF g_simple_tree1 IS BOUND.
    g_simple_tree1->free( ).
    FREE g_simple_tree1.
  ENDIF.

  IF g_simple_tree2 IS BOUND.
    g_simple_tree2->free( ).
    FREE g_simple_tree2.
  ENDIF.

  IF g_simple_tree3 IS BOUND.
    g_simple_tree3->free( ).
    FREE g_simple_tree3.
  ENDIF.

  IF g_simple_tree4 IS BOUND.
    g_simple_tree4->free( ).
    FREE g_simple_tree4.
  ENDIF.

  IF g_custom_container1 IS BOUND.
    g_custom_container1->free( ).
    FREE g_custom_container1.
  ENDIF.

  IF g_custom_container2 IS BOUND.
    g_custom_container2->free( ).
    FREE g_custom_container2.
  ENDIF.

  IF g_custom_container3 IS BOUND.
    g_custom_container3->free( ).
    FREE g_custom_container3.
  ENDIF.

  IF g_custom_container4 IS BOUND.
    g_custom_container4->free( ).
    FREE g_custom_container4.
  ENDIF.
ENDFUNCTION.
