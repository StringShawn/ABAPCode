FUNCTION-POOL ZJMLIST_GROUP_TEMP.                "MESSAGE-ID ..

TABLES:zsd_jmfh_h,zsd_jiamengfahuo,zmm_spyh_h,zmm_spyh_t,zmm_fspyh_t,zmm_fspyh_h.

DATA ok_code TYPE sy-ucomm.
DATA return_flag TYPE c.

DATA textline1 TYPE string.
DATA textline2 TYPE string.
DATA titel TYPE string.
DATA answer TYPE c.

DATA g_simple_tree TYPE REF TO cl_gui_simple_tree.
DATA g_custom_container TYPE REF TO cl_gui_custom_container.
DATA g_simple_tree_event_handler TYPE REF TO zcl_simple_tree_event_handler.

DATA gt_werks_node LIKE TABLE OF zsimple_treenodes WITH HEADER LINE.
DATA last_node_key TYPE i.

CONTROLS jm_tab TYPE TABSTRIP.
CONTROLS stab7 TYPE TABSTRIP.

CONSTANTS:BEGIN OF c_tb_tab,
            tab1  LIKE sy-ucomm VALUE 'TAB1',
            tab2  LIKE sy-ucomm VALUE 'TAB2',
            tab3  LIKE sy-ucomm VALUE 'TAB3',
            tab4  LIKE sy-ucomm VALUE 'TAB4',
            tab5  LIKE sy-ucomm VALUE 'TAB5',
            tab6  LIKE sy-ucomm VALUE 'TAB6',
            tab7  LIKE sy-ucomm VALUE 'TAB7',
            tab8  LIKE sy-ucomm VALUE 'TAB8',
            tab9  LIKE sy-ucomm VALUE 'TAB9',
            tab10 LIKE sy-ucomm VALUE 'TAB10',
            tab11 LIKE sy-ucomm VALUE 'TAB11',  "add by handyxh 20181112
            tab12 LIKE sy-ucomm VALUE 'TAB12',
          END OF c_tb_tab.

DATA:BEGIN OF g_tb_tab,
       subscreen   TYPE sy-dynnr,
       prog        LIKE sy-repid VALUE 'SAPLZJMLIST_GROUP',
       pressed_tab LIKE sy-ucomm,
     END OF g_tb_tab.

CONSTANTS:BEGIN OF c_tb_tab7,
            tab1 LIKE sy-ucomm VALUE 'TAB7_1',
            tab2 LIKE sy-ucomm VALUE 'TAB7_2',
          END OF c_tb_tab7.

DATA:BEGIN OF g_tb_tab7,
       subscreen   TYPE sy-dynnr,
       prog        LIKE sy-repid VALUE 'SAPLZJMLIST_GROUP',
       pressed_tab LIKE sy-ucomm VALUE c_tb_tab7,
     END OF g_tb_tab7.

DATA ls_toolbar TYPE stb_button.
DEFINE add_toolbar.

  IF &5 = 'X'.
     CLEAR ls_toolbar.
    ls_toolbar-butn_type = 3.
   IF &6 = 'I'.
    insert ls_toolbar into p_object->mt_toolbar index 1.
   ELSE.
     APPEND ls_toolbar to p_object->mt_toolbar.
   ENDIF.
  ENDIF.
  CLEAR ls_toolbar.

  ls_toolbar-butn_type = '0'.
  ls_toolbar-function = &1.
  ls_toolbar-quickinfo = &2.
  ls_toolbar-text = &3.
  ls_toolbar-icon = &4.


  clear ls_toolbar-checked .
  clear ls_toolbar-disabled.
  IF &6 = 'I'.
    insert ls_toolbar into p_object->mt_toolbar index 1.
  ELSE.
    APPEND ls_toolbar to p_object->mt_toolbar.
  ENDIF.
END-OF-DEFINITION.

DEFINE append_toolbar.

END-OF-DEFINITION.


DATA container1 TYPE REF TO cl_gui_custom_container.
DATA alv1 TYPE REF TO cl_gui_alv_grid.
DATA layo1 TYPE lvc_s_layo.
DATA fcat1 TYPE lvc_t_fcat WITH HEADER LINE.
DATA alv_event1 TYPE REF TO zcl_alv_event_receiver.

DATA container2 TYPE REF TO cl_gui_custom_container.
DATA alv2 TYPE REF TO cl_gui_alv_grid.
DATA layo2 TYPE lvc_s_layo.
DATA fcat2 TYPE lvc_t_fcat WITH HEADER LINE.
DATA alv_event2 TYPE REF TO zcl_alv_event_receiver.

DATA container3 TYPE REF TO cl_gui_custom_container.
DATA alv3 TYPE REF TO cl_gui_alv_grid.
DATA layo3 TYPE lvc_s_layo.
DATA fcat3 TYPE lvc_t_fcat WITH HEADER LINE.
DATA alv_event3 TYPE REF TO zcl_alv_event_receiver.

DATA container5 TYPE REF TO cl_gui_custom_container.
DATA alv5 TYPE REF TO cl_gui_alv_grid.
DATA layo5 TYPE lvc_s_layo.
DATA fcat5 TYPE lvc_t_fcat WITH HEADER LINE.
DATA alv_event5 TYPE REF TO zcl_alv_event_receiver.

DATA container6 TYPE REF TO cl_gui_custom_container.
DATA alv6 TYPE REF TO cl_gui_alv_grid.
DATA layo6 TYPE lvc_s_layo.
DATA fcat6 TYPE lvc_t_fcat WITH HEADER LINE.
DATA alv_event6 TYPE REF TO zcl_alv_event_receiver.

DATA container7 TYPE REF TO cl_gui_custom_container.
DATA alv7 TYPE REF TO cl_gui_alv_grid.
DATA layo7 TYPE lvc_s_layo.
DATA fcat7 TYPE lvc_t_fcat WITH HEADER LINE.
DATA alv_event7 TYPE REF TO zcl_alv_event_receiver.

DATA container8 TYPE REF TO cl_gui_custom_container.
DATA alv8 TYPE REF TO cl_gui_alv_grid.
DATA layo8 TYPE lvc_s_layo.
DATA fcat8 TYPE lvc_t_fcat WITH HEADER LINE.
DATA alv_event8 TYPE REF TO zcl_alv_event_receiver.


DATA container9 TYPE REF TO cl_gui_custom_container.
DATA alv9 TYPE REF TO cl_gui_alv_grid.
DATA layo9 TYPE lvc_s_layo.
DATA fcat9 TYPE lvc_t_fcat WITH HEADER LINE.
DATA alv_event9 TYPE REF TO zcl_alv_event_receiver.


DATA container10 TYPE REF TO cl_gui_custom_container.
DATA alv10 TYPE REF TO cl_gui_alv_grid.
DATA layo10 TYPE lvc_s_layo.
DATA fcat10 TYPE lvc_t_fcat WITH HEADER LINE.
DATA alv_event10 TYPE REF TO zcl_alv_event_receiver.


"add by handyxh 20181112
DATA container11 TYPE REF TO cl_gui_custom_container.
DATA alv11 TYPE REF TO cl_gui_alv_grid.
DATA layo11 TYPE lvc_s_layo.
DATA fcat11 TYPE lvc_t_fcat WITH HEADER LINE.
DATA alv_event11 TYPE REF TO zcl_alv_event_receiver.

DATA container12 TYPE REF TO cl_gui_custom_container.
DATA alv12 TYPE REF TO cl_gui_alv_grid.
DATA layo12 TYPE lvc_s_layo.
DATA fcat12 TYPE lvc_t_fcat WITH HEADER LINE.
DATA alv_event12 TYPE REF TO zcl_alv_event_receiver.


DATA fspp_alv TYPE REF TO cl_gui_alv_grid.

DATA gt_fieldcat TYPE lvc_t_fcat WITH HEADER LINE.


DEFINE add_field.
  gt_fieldcat-fieldname = &1.
  gt_fieldcat-scrtext_s = &2.
  gt_fieldcat-col_opt = 'X'.
  append gt_fieldcat.
  clear gt_fieldcat.
END-OF-DEFINITION.


DATA:BEGIN OF gs_fh_data,
       sel            TYPE c,
       jmstu          TYPE char1,
       jmstu_txt      TYPE char10,
       stype          TYPE char1,
       stype_txt      TYPE char10,
       fwerk          TYPE werks_d,
       flgor          TYPE lgort_d,
       werks          TYPE werks_d,
       werks_name     TYPE name1,
       bhnuc          TYPE char10,
       bhitem         TYPE posnr,
       odeln          TYPE char10,
       z_jtm          TYPE zjtm,
       charg          TYPE charg_d,
       matnr          TYPE matnr,
       maktx          TYPE maktx,
       free_flag      TYPE c,
       free_txt       TYPE char10,
       db_amount      TYPE dmbtr,
       sg_amount      TYPE dmbtr,
       all_amount     TYPE dmbtr,
       menge          TYPE menge_d,
       crdat          TYPE sy-datum,
       crtim          TYPE sy-uzeit,
       fhdat          TYPE sy-datum,
       fhtim          TYPE sy-uzeit,
       fhnam          TYPE sy-uname,
       crnam          TYPE sy-uname,
       vtext          TYPE t179t-vtext,
       zmax_menge     TYPE zhqbh-zmax_menge,
       zmin_menge     TYPE zhqbh-zmin_menge,
       zmin_pkg_menge TYPE zhqbh-zmin_pkg_menge,
       zpp_scope      TYPE zhqbh-zpp_scope,

     END OF gs_fh_data,
     gt_spfh_data  LIKE TABLE OF gs_fh_data WITH HEADER LINE,
     gt_fspfh_data LIKE TABLE OF gs_fh_data WITH HEADER LINE.

DATA:BEGIN OF gs_yh_data,
       sel        TYPE c,
       status     TYPE char1,
       status_txt TYPE char10,
       yhdty      TYPE char1,
       yhdty_txt  TYPE char6,
       werks      TYPE werks_d,
       werks_name TYPE name1,
       kunnr      TYPE kunnr,
       yhdln      TYPE char10,
       yhdlp      TYPE n LENGTH 6,
       matnr      TYPE matnr,
       z_jtm      TYPE zjtm,
       charg      TYPE charg_d,
       maktx      TYPE maktx,
       db_amount  TYPE dmbtr,
       sg_amount  TYPE dmbtr,
       menge      TYPE menge_d,
       amtch      TYPE char1,
       amtch_txt  TYPE char6,
       crdat      TYPE sy-datum,
       crtim      TYPE sy-uzeit,
       spdat      TYPE sy-datum,
       sptim      TYPE sy-uzeit,
       rqdat      TYPE sy-datum,
     END OF gs_yh_data,
     gt_spyh_data  LIKE TABLE OF gs_yh_data WITH HEADER LINE,
     gt_fspyh_data LIKE TABLE OF gs_yh_data WITH HEADER LINE.

DATA:BEGIN OF gs_fh_head,
       sel        TYPE c,
       jmstu      TYPE char1,
       jmstu_txt  TYPE char10,
       stype      TYPE char1,
       stype_txt  TYPE char10,
       fwerk      TYPE werks_d,
       werks      TYPE werks_d,
       werks_name TYPE name1,
       bhnuc      TYPE char10,
       linkn      TYPE char10,
       all_amount TYPE dmbtr,
       crdat      TYPE sy-datum,
       crtim      TYPE sy-uzeit,
       fhdat      TYPE sy-datum,
       fhtim      TYPE sy-uzeit,
       fhnam      TYPE sy-uname,
       crnam      TYPE sy-uname,
     END OF gs_fh_head,
     gt_fh_head    LIKE TABLE OF gs_fh_head WITH HEADER LINE,
     gt_spfh_head  LIKE TABLE OF gs_fh_head WITH HEADER LINE,
     gt_fspfh_head LIKE TABLE OF gs_fh_head WITH HEADER LINE.

DATA:BEGIN OF gs_th_data,

       ebeln      TYPE ebeln,
       ebelp      TYPE ebelp,
       zthlx      TYPE char1,
       zthlx_txt  TYPE char20,
       werks      TYPE werks_d,
       werks_name TYPE name1,
       fwerk      TYPE werks_d,
       pathn      TYPE char10,
       matnr      TYPE matnr,
       maktx      TYPE maktx,
       charg      TYPE charg_d,
       z_jtm      TYPE char20,
       z_pp       TYPE z_pp,
       z_zpp      TYPE z_zpp,
       z_zcb      TYPE z_zcb,
       z_sgj      TYPE dmbtr,
       menge      TYPE menge_d,
       hispr      TYPE dmbtr,
       lespr      TYPE dmbtr,
       redpr      TYPE dmbtr,
       amflg      TYPE char1,
       amflg_txt  TYPE char6,
       hislg      TYPE char1,
       hislg_txt  TYPE char6,

       crdat      TYPE sy-datum,
       crtim      TYPE sy-uzeit,

       mlper      TYPE char10,

       sel        TYPE c,
       thstu      TYPE char1,
       thstu_txt  TYPE char10,

     END OF gs_th_data,
     gt_th_data LIKE TABLE OF gs_th_data WITH HEADER LINE.

DATA:BEGIN OF ls_fh_ml,
       werks      TYPE werks_d,
       werks_name TYPE name1,
       vkbur      TYPE vkbur,
       vkbur_txt  TYPE bezei,
       db_amount  TYPE dmbtr,
       vp_amount  TYPE dmbtr,
       mlper      TYPE char10,
     END OF ls_fh_ml,
     gt_fh_ml LIKE TABLE OF ls_fh_ml WITH HEADER LINE.

DATA:BEGIN OF ls_belnr_sum,
       kunnr           TYPE kunnr,
       kunnr_name      TYPE name1,
       werks           TYPE werks_d,
       werks_name      TYPE name1,
       bukrs           TYPE bukrs,
       bukrs_name      TYPE butxt,

       spyh_nf_amount  TYPE dmbtr,
       spyh_yf_amount  TYPE dmbtr,
       fspyh_nf_amount TYPE dmbtr,
       fspyh_yf_amount TYPE dmbtr,

       spbh_amount     TYPE dmbtr,
       fspbh_amount    TYPE dmbtr,

       oa_nf_amount    TYPE dmbtr,
       oa_yf_amount    TYPE dmbtr,

       spgk_amount     TYPE dmbtr,
       spwx_amount     TYPE dmbtr,
       spth_amount     TYPE dmbtr,

     END OF ls_belnr_sum,
     gt_belnr_sum LIKE TABLE OF ls_belnr_sum WITH HEADER LINE.

DATA:BEGIN OF gs_jm_belnr.
        INCLUDE STRUCTURE zmm_yhbel_list.
DATA:type      TYPE char2,
     btype_txt TYPE char20,
     jmbln_txt TYPE char10,
     END OF gs_jm_belnr,
     gt_jm_belnr LIKE TABLE OF gs_jm_belnr WITH HEADER LINE.

DATA:BEGIN OF ls_jmfh_print,
       sel        TYPE c,
       bhnuc      TYPE char10,
       fwerk      TYPE char4,
       flgor      TYPE char4,
       werks      TYPE char4,
       fhdat      TYPE sy-datum,
       fhnam      TYPE sy-uname,
       werks_name TYPE name1,
     END OF ls_jmfh_print,
     lt_fspfh_print LIKE TABLE OF ls_jmfh_print WITH HEADER LINE.

DATA:BEGIN OF ls_revs,
       ogrup TYPE numc3,
       step  TYPE numc3,
     END OF ls_revs.

DATA:BEGIN OF ls_yh,
       werks TYPE werks_d,
       cadat TYPE sy-datum,
     END OF ls_yh.
"add by handyxh 20181112
DATA:BEGIN OF gt_kyth_data OCCURS 0,
       sel        TYPE c,
       status     TYPE char10,
       bhnuc      TYPE zsd_jmfh_h-bhnuc,
       bhitem     TYPE zsd_jiamengfahuo-bhitem,
       werks      TYPE werks_d,
       werks_name TYPE name1,
       matnr      TYPE matnr,
       charg      TYPE charg_d,
       maktx      TYPE maktx,
       menge      TYPE menge_d,
       crdat      TYPE zsd_jmfh_h-crdat,
       thstu      TYPE char1,
     END OF gt_kyth_data.
DATA:gs_kyth_data LIKE LINE OF gt_kyth_data.
DATA:gt_kyth_data_temp LIKE gt_kyth_data OCCURS 0.
DATA:gt_kyth_data_temp1 LIKE gt_kyth_data OCCURS 0.
DATA:gs_kythlist  TYPE zmm_kythlist.  "正在执行的订单
DATA:gt_kythlist  TYPE TABLE OF ZMM_KYTHLIST.
DATA:active_path  TYPE zlsjm_path.    "正在执行的路径
INCLUDE lzjmlist_groupdat.

* INCLUDE LZJMLIST_GROUPD...                 " Local class definition
