FUNCTION-POOL ZTRADE_GROUP_TEMP.                 "MESSAGE-ID ..


DATA ok_code TYPE sy-ucomm.
DATA return_flag TYPE c.
DATA operation_flag TYPE c.
DATA save_flag TYPE c.

DATA fid TYPE char30.
DATA cursor_line TYPE i.
DATA current_line TYPE i.
DATA top_line TYPE i.

CONTROLS main_tab TYPE TABSTRIP.
CONTROLS path_h_tc TYPE TABLEVIEW USING SCREEN 0201.
CONTROLS werks_tc TYPE TABLEVIEW USING SCREEN 0202.
CONTROLS lgort_tc TYPE TABLEVIEW USING SCREEN 0203.
CONTROLS lifnr_tc TYPE TABLEVIEW USING SCREEN 0204.
CONTROLS path_t_tc TYPE TABLEVIEW USING SCREEN 0301.
CONTROLS excel_tc TYPE TABLEVIEW USING SCREEN 0303.


CONSTANTS:BEGIN OF c_tb_tab,
            tab1 TYPE sy-ucomm VALUE 'TAB1',
            tab2 TYPE sy-ucomm VALUE 'TAB2',
            tab3 TYPE sy-ucomm VALUE 'TAB3',
            tab4 TYPE sy-ucomm VALUE 'TAB4',
          END OF c_tb_tab.

DATA:BEGIN OF g_tb_tab,
       subscreen   TYPE sy-dynnr,
       prog        TYPE sy-repid VALUE 'SAPLZTRADE_GROUP',
       pressed_tab TYPE sy-ucomm VALUE 'TAB1',
     END OF g_tb_tab.


DATA:BEGIN OF ls_list,
       typid   TYPE char1,
       typname TYPE char20,
     END OF ls_list,
     lt_step  LIKE TABLE OF ls_list WITH HEADER LINE,
     lt_patyp LIKE TABLE OF ls_list WITH HEADER LINE.


DATA:BEGIN OF gs_path_h.
        INCLUDE STRUCTURE ztrade_path_h.
DATA:sel       TYPE c,
     status    TYPE char20,
     patyp_txt TYPE char10,
     END OF gs_path_h,
     gt_path_h  LIKE TABLE OF gs_path_h WITH HEADER LINE,
     sel_path_h LIKE gs_path_h.


DATA:BEGIN OF gs_path_t.
        INCLUDE STRUCTURE ztrade_path_t.
DATA:sel TYPE c,
     END OF gs_path_t,
     gt_path_t LIKE TABLE OF gs_path_t WITH HEADER LINE.


DATA p_werks TYPE werks_d.
DATA p_pathn TYPE char10.
DATA p_path TYPE rlgrap-filename.
DATA p_nfdat TYPE sy-datum.
DATA:BEGIN OF gs_werks.
        INCLUDE STRUCTURE ztrade_werks.
DATA:sel      TYPE c,
     status   TYPE char10,
     path_txt TYPE char30,
     END OF gs_werks,
     gt_werks  LIKE TABLE OF gs_werks WITH HEADER LINE,
     sel_werks LIKE gs_werks.


DATA:BEGIN OF gs_excel.
        INCLUDE STRUCTURE ztrade_werks.
DATA:sel      TYPE c,
     line     TYPE n LENGTH 4,
     path_txt TYPE char30,
     status   TYPE char10,
     msg      TYPE bapi_msg,
     END OF gs_excel,
     gt_excel LIKE TABLE OF gs_excel WITH HEADER LINE,
     pt_excel LIKE TABLE OF gs_excel WITH HEADER LINE.

DATA:BEGIN OF gs_lgort.
        INCLUDE STRUCTURE ztrade_lgort.
DATA:sel       TYPE c,
     ltype_txt TYPE char20,
     lgobe     TYPE lgobe,
     END OF gs_lgort,
     gt_lgort LIKE TABLE OF gs_lgort WITH HEADER LINE.

DATA lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.


DATA g_simple_tree1 TYPE REF TO cl_gui_simple_tree.
DATA g_custom_container1 TYPE REF TO cl_gui_custom_container.
DATA g_simple_tree_event_handler1 TYPE REF TO zcl_simple_tree_event_handler.
DATA gt_yhlist_node LIKE TABLE OF zsimple_treenodes WITH HEADER LINE.

DATA g_simple_tree2 TYPE REF TO cl_gui_simple_tree.
DATA g_custom_container2 TYPE REF TO cl_gui_custom_container.
DATA g_simple_tree_event_handler2 TYPE REF TO zcl_simple_tree_event_handler.
DATA gt_odlist_node LIKE TABLE OF zsimple_treenodes WITH HEADER LINE.

DATA g_simple_tree3 TYPE REF TO cl_gui_simple_tree.
DATA g_custom_container3 TYPE REF TO cl_gui_custom_container.
DATA g_simple_tree_event_handler3 TYPE REF TO zcl_simple_tree_event_handler.
DATA gt_thlist_node LIKE TABLE OF zsimple_treenodes WITH HEADER LINE.

"add by handyxh 20181116
DATA g_simple_tree4 TYPE REF TO cl_gui_simple_tree.
DATA g_custom_container4 TYPE REF TO cl_gui_custom_container.
DATA g_simple_tree_event_handler4 TYPE REF TO zcl_simple_tree_event_handler.
DATA gt_kythlist_node LIKE TABLE OF zsimple_treenodes WITH HEADER LINE.


CONTROLS list_tab TYPE TABSTRIP.

CONSTANTS:BEGIN OF c_list_tab,
            tab1 LIKE sy-ucomm VALUE 'TAB1',
            tab2 LIKE sy-ucomm VALUE 'TAB2',
            tab3 LIKE sy-ucomm VALUE 'TAB3',
            tab4 LIKE sy-ucomm VALUE 'TAB4',  "ADD BY HANDYXH 20181116
          END OF c_list_tab.

DATA:BEGIN OF g_list_tab,
       subscreen   TYPE sy-dynnr,
       prog        LIKE sy-repid VALUE 'SAPLZTRADE_GROUP',
       pressed_tab LIKE sy-ucomm VALUE c_tb_tab,
     END OF g_list_tab.


DATA:BEGIN OF gs_yh_list,
       yhdln      TYPE char10,
       bhnuc      TYPE char10,
       linkn      TYPE char10,
       status     TYPE char1,
       status_txt TYPE char10,
       crdat      TYPE sy-datum,
       crtim      TYPE sy-uzeit,
       spdat      TYPE sy-datum,
       sptim      TYPE sy-uzeit,
       werkf      TYPE werks_d,
       werks      TYPE werks_d,
       werks_name TYPE name1,
       all_amount TYPE dmbtr,
       cre_amount TYPE dmbtr,
     END OF gs_yh_list.

DATA:BEGIN OF gs_fh_list,
       bhnuc      TYPE char10,
       stype      TYPE char1,
       pathn      TYPE char10,
       linkn      TYPE char10,
       crdat      TYPE sy-datum,
       crtim      TYPE sy-uzeit,
       crnam      TYPE sy-uname,
       werks      TYPE werks_d,
       werks_name TYPE name1,
       fwerk      TYPE werks_d,
       jmstu      TYPE char1,
       jmstu_txt  TYPE char20,
       mtype      TYPE char2,
       mtype_txt  TYPE char10,
       yhdln      TYPE char10,
       all_amount TYPE dmbtr,
       cre_amount TYPE dmbtr,
       fhdat      TYPE sy-datum,
     END OF gs_fh_list.

DATA:BEGIN OF gs_th_list,
       ebeln      TYPE ebeln,
       pathn      TYPE char10,
       werks      TYPE werks_d,
       werks_name TYPE name1,
       zthlx      TYPE char1,
       zthlx_txt  TYPE char12,
       thstu      TYPE char1,
       thstu_txt  TYPE char10,
       crdat      TYPE sy-datum,
       crtim      TYPE sy-uzeit,
     END OF gs_th_list.

"add by handyxh 20181115
DATA:BEGIN OF gs_kyth_list,
      bhnuc  TYPE zbhnuc,
      pathn  TYPE char10,
      werks  TYPE werks_d,
      werks_name TYPE name1,
      thstu  TYPE char1,
      thstu_txt TYPE char10,
      crdat  TYPE sy-datum,
      crtim      TYPE sy-uzeit,
     END OF gs_kyth_list.
"end

DATA:BEGIN OF ls_odlist,
       bukrs TYPE bukrs,
       ogrup TYPE numc3,
       step  TYPE numc3,
       odjhr TYPE char4,
       odeln TYPE char10,
       odetp TYPE char1,
       crdat TYPE sy-datum,
     END OF ls_odlist.

DATA:BEGIN OF gs_jmat_h,
       werks TYPE werks_d,
       bukrs TYPE bukrs,
       werkn TYPE i,
     END OF gs_jmat_h.

DATA:BEGIN OF gs_jmat_t,
       bukrs  TYPE bukrs,
       amount TYPE hslxx12,
     END OF gs_jmat_t,
     gt_jmat_t LIKE TABLE OF gs_jmat_t WITH HEADER LINE.
* INCLUDE LZTRADE_GROUPD...                  " Local class definition
