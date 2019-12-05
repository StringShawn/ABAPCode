*----------------------------------------------------------------------*
***INCLUDE ZJSRPT_004_STATUS_9000O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'STATUS_9000'.
  SET TITLEBAR 'TITLE_9000'.

  IF container IS INITIAL.
    CREATE OBJECT: container EXPORTING container_name = 'ZBEIZHU'.

    CREATE OBJECT editor
      EXPORTING
        parent                     = container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position          = 256
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
  ENDIF.

  CALL METHOD editor->set_text_as_r3table
    EXPORTING
      table = lt_beizhu[].

ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'ZTRCL_YF'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE ztrcl_yf_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_yf_item LINES ztrcl_yf-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'ZTRCL_YF'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE ztrcl_yf_get_lines OUTPUT.
  g_ztrcl_yf_lines = sy-loopc.
ENDMODULE.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.4.1 - E.G.Mellodew. 1998-2019. Sap Release 752
