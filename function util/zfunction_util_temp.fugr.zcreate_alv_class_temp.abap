FUNCTION ZCREATE_ALV_CLASS_TEMP.
*"--------------------------------------------------------------------
*"*"本地接口:
*"  IMPORTING
*"     REFERENCE(CONTAINER_NAME) TYPE  C
*"     REFERENCE(LAYOUT) TYPE  LVC_S_LAYO
*"     REFERENCE(SCREEN_LEVEL) TYPE REF TO CL_GUI_CONTAINER OPTIONAL
*"  TABLES
*"      LT_FCAT TYPE  LVC_T_FCAT
*"      IT_DATA TYPE  TABLE
*"  CHANGING
*"     REFERENCE(CL_CONTAINER) TYPE REF TO CL_GUI_CUSTOM_CONTAINER
*"     REFERENCE(CL_ALV) TYPE REF TO CL_GUI_ALV_GRID
*"     REFERENCE(CL_EVENT) TYPE REF TO ZCL_ALV_EVENT_RECEIVER
*"         OPTIONAL
*"--------------------------------------------------------------------

  DATA variant TYPE disvariant.

  DATA parent TYPE REF TO cl_gui_container.


  variant-report = 'CONTAINER_NAME'.

  IF cl_container IS NOT BOUND.

    IF screen_level IS NOT BOUND.
      parent = cl_gui_custom_container=>screen0.
    ELSE.
      parent = screen_level.
    ENDIF.

    cl_container = NEW cl_gui_custom_container( parent = parent
                                                container_name = container_name ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  IF cl_alv IS NOT BOUND.

    cl_alv = NEW cl_gui_alv_grid( i_parent = cl_container ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  IF cl_event IS BOUND.
    cl_event->handle_event( CHANGING cl_alv = cl_alv ).
  ENDIF.

  CALL METHOD cl_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = layout
      is_variant                    = variant
      i_default                     = 'X'
      i_save                        = 'A'
    CHANGING
      it_outtab                     = it_data[]
      it_fieldcatalog               = lt_fcat[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFUNCTION.
