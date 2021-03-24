*&---------------------------------------------------------------------*
*& Report:       ZCODE_GENERATOR_DTEL_AND_DOMA
*& Description:  Generate data elements and domains
*&=====================================================================*
*& Create date:  2020.01.02
*& Created by:   Leo-SDU ����(΢�Ź��ںţ�SAP����)
*&---------------------------------------------------------------------*
REPORT zcode_generator_dtel_and_doma NO STANDARD PAGE HEADING.

"INCLUDE zlcl_alv_event_handler.

TABLES sscrfields.
TYPE-POOLS: vrm, cntb.

SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE title1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (6) text1 FOR FIELD p_devc.
SELECTION-SCREEN POSITION 10.
PARAMETERS p_devc TYPE devclass.
SELECTION-SCREEN PUSHBUTTON 45(10) tmp USER-COMMAND tmp.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (6) text2 FOR FIELD p_trkorr.
SELECTION-SCREEN POSITION 10.
PARAMETERS p_trkorr TYPE trkorr VISIBLE LENGTH 10.
SELECTION-SCREEN COMMENT (60) t_trkorr.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN COMMENT /1(79) cmt2.
SELECTION-SCREEN END OF BLOCK 001.

SELECTION-SCREEN BEGIN OF SCREEN 9100 AS WINDOW.
SELECTION-SCREEN COMMENT /1(70) t_9100_1.
SELECTION-SCREEN COMMENT /1(70) t_9100_2.
PARAMETERS p_yes TYPE c LENGTH 3.
SELECTION-SCREEN END OF SCREEN 9100.

SELECTION-SCREEN BEGIN OF SCREEN 9200 AS WINDOW.
SELECTION-SCREEN COMMENT /1(70) t_9200_1.
PARAMETERS p_prefix TYPE string.  "New Prefix
PARAMETERS p_preold TYPE string.  "OLD Prefix
SELECTION-SCREEN END OF SCREEN 9200.

SELECTION-SCREEN BEGIN OF SCREEN 9300 AS WINDOW.
PARAMETERS p_tabnam TYPE tabname16 OBLIGATORY.
PARAMETERS p_tabtxt TYPE as4text OBLIGATORY.
PARAMETERS p_conflg TYPE contflag AS LISTBOX VISIBLE LENGTH 50 OBLIGATORY.
PARAMETERS p_mntflg TYPE maintflag AS LISTBOX VISIBLE LENGTH 50.
PARAMETERS p_tabart TYPE tabart AS LISTBOX VISIBLE LENGTH 50 OBLIGATORY.
PARAMETERS p_tabkat TYPE tabkat AS LISTBOX VISIBLE LENGTH 50 OBLIGATORY.
SELECTION-SCREEN END OF SCREEN 9300.

SELECTION-SCREEN BEGIN OF SCREEN 9400 AS WINDOW.
SELECTION-SCREEN END OF SCREEN 9400.

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.
SELECTION-SCREEN FUNCTION KEY 3.
SELECTION-SCREEN FUNCTION KEY 4.

TYPES:
  BEGIN OF typ_field,
    fieldname TYPE fieldname,
    rollname  TYPE rollname,
    create    TYPE c,  "IF NEED TO CREATE
    domname   TYPE domname,
    datatype  TYPE datatype_d,
    leng      TYPE ddleng,
    decimals  TYPE decimals,
    ddtext    TYPE as4text,
    result    TYPE string,
  END OF typ_field,

  BEGIN OF typ_doma,
    domname   TYPE domname,
    datatype  TYPE datatype_d,
    leng      TYPE ddleng,
    decimals  TYPE decimals,
    lowercase TYPE lowercase,
    signflag  TYPE signflag,
    ddtext    TYPE as4text,
    result    TYPE string,
  END OF typ_doma.

DATA:
  go_docking_con  TYPE REF TO cl_gui_docking_container,
  go_splitter_con TYPE REF TO cl_gui_splitter_container,
  go_con_field    TYPE REF TO cl_gui_container,
  go_con_doma     TYPE REF TO cl_gui_container,
  go_alv_field    TYPE REF TO cl_gui_alv_grid,
  go_event_field  TYPE REF TO zcl_alv_event_handler,
  go_alv_doma     TYPE REF TO cl_gui_alv_grid,
  go_event_doma   TYPE REF TO zcl_alv_event_handler.

DATA:
  gt_fields TYPE TABLE OF typ_field,
  gt_domas  TYPE TABLE OF typ_doma,
  g_task    TYPE trkorr.

DATA:
  gt_fieldcat_field TYPE lvc_t_fcat,
  gs_fieldcat_field TYPE lvc_s_fcat,
  gt_fieldcat_doma  TYPE lvc_t_fcat,
  gs_fieldcat_doma  TYPE lvc_s_fcat,
  gt_f4_field       TYPE lvc_t_f4,
  gt_f4_doma        TYPE lvc_t_f4,
  gs_f4             TYPE lvc_s_f4,
  gt_exclude        TYPE ui_functions,
  gs_layout         TYPE lvc_s_layo.

DATA:
  g_typekind        TYPE ddtypekind.

DATA:
  g_ucomm   TYPE sy-ucomm,
  g_error   TYPE c,
  g_9100_ok TYPE c,
  g_9200_ok TYPE c,
  g_9300_ok TYPE c.

DEFINE d_build_fieldcat_field.
  gs_fieldcat_field-fieldname  = &1.
  gs_fieldcat_field-edit       = &2.
  gs_fieldcat_field-checkbox   = &3.
  gs_fieldcat_field-f4availabl = &4.
  gs_fieldcat_field-outputlen  = &5.
  gs_fieldcat_field-ref_table  = &6.
  gs_fieldcat_field-ref_field  = &7.
  gs_fieldcat_field-coltext    = &8.
  append gs_fieldcat_field to gt_fieldcat_field.
  if gs_fieldcat_field-f4availabl = 'X'.
    gs_f4-fieldname = &1.
    gs_f4-register = 'X'.
    append gs_f4 to gt_f4_field.
  endif.
  clear gs_fieldcat_field.
END-OF-DEFINITION.

DEFINE d_build_fieldcat_doma.
  gs_fieldcat_doma-fieldname  = &1.
  gs_fieldcat_doma-edit       = &2.
  gs_fieldcat_doma-checkbox   = &3.
  gs_fieldcat_doma-f4availabl = &4.
  gs_fieldcat_doma-outputlen  = &5.
  gs_fieldcat_doma-ref_table  = &6.
  gs_fieldcat_doma-ref_field  = &7.
  gs_fieldcat_doma-coltext    = &8.
  append gs_fieldcat_doma to gt_fieldcat_doma.
  if gs_fieldcat_doma-f4availabl = 'X'.
    gs_f4-fieldname = &1.
    gs_f4-register = 'X'.
    append gs_f4 to gt_f4_doma.
  endif.
  clear gs_fieldcat_doma.
END-OF-DEFINITION.

DEFINE show_error_message.
  message &1 type 'S' display like 'E'.
  g_error = 'X'.
  return.
END-OF-DEFINITION.

INITIALIZATION.
  sy-title = '���ٽ���������������Ԫ�غ���'.
  sscrfields-functxt_01 = '@0S@ʹ��˵��'.
  sscrfields-functxt_02 = '@OD@������'.
  sscrfields-functxt_03 = '@HO@��������Ԫ��'.
  sscrfields-functxt_04 = '@PO@������'.
  title1 = '��������'.
  text1 = '������'.
  text2 = '�����'.
  tmp = '���ض���'.
  cmt2 = '��BUG�뷴����΢��/QQ��286503700��  ���ྫ�����ע΢�Ź��ںţ�SAP����'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_trkorr.
  PERFORM f4_trkorr.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tabart.
  PERFORM f4_tabart.

AT SELECTION-SCREEN OUTPUT.
  CASE sy-dynnr.
    WHEN '1000'.
      PERFORM selection_screen_pbo_1000.
      PERFORM create_screen_object.
    WHEN '9100'.
      PERFORM selection_screen_pbo_9100.
    WHEN '9200'.
      PERFORM selection_screen_pbo_9200.
    WHEN '9300'.
      PERFORM selection_screen_pbo_9300.
    WHEN '9400'.
      PERFORM selection_screen_pbo_9400.
  ENDCASE.

AT USER-COMMAND.
  IF sy-ucomm = 'ECAN'.
    LEAVE TO SCREEN 0.
  ENDIF.

AT SELECTION-SCREEN.
  CASE sy-dynnr.
    WHEN '1000'.
      PERFORM selection_screen_pai_1000.
    WHEN '9100'.
      PERFORM selection_screen_pai_9100.
    WHEN '9200'.
      PERFORM selection_screen_pai_9200.
    WHEN '9300'.
      PERFORM selection_screen_pai_9300.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
FORM selection_screen_pf_status.
  DATA: lt_exclude TYPE TABLE OF sy-ucomm.

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = 'SWITCH_CLIENT'
      p_program = 'SAPLBTCH'
    TABLES
      p_exclude = lt_exclude.
ENDFORM.                    "set_pf_status
*&---------------------------------------------------------------------*
*&      FORM  SELECTION_SCREEN_PBO_1000
*&---------------------------------------------------------------------*
FORM selection_screen_pbo_1000.
  DATA: lt_exclude TYPE TABLE OF sy-ucomm.

  APPEND 'ONLI' TO lt_exclude.
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
      p_program = sy-cprog
    TABLES
      p_exclude = lt_exclude.

  IF p_devc = '$TMP'.
    tmp = '�������'.
    p_trkorr = ''.
  ELSE.
    tmp = '���ض���'.
  ENDIF.
  LOOP AT SCREEN.
    IF p_devc = '$TMP' AND ( screen-name = 'P_DEVC' OR screen-name = 'P_TRKORR' ).
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  "�������������
  CLEAR t_trkorr.
  IF p_trkorr <> ''.
    SELECT SINGLE as4text INTO t_trkorr FROM e07t WHERE trkorr = p_trkorr AND langu = sy-langu.
  ENDIF.
ENDFORM.                    "SELECTION_SCREEN_PBO_1000
*&---------------------------------------------------------------------*
*&      Form  selection_screen_pbo_9100
*&---------------------------------------------------------------------*
FORM selection_screen_pbo_9100.
  PERFORM selection_screen_pf_status.
  CONCATENATE '������: ' p_devc INTO t_9100_1 SEPARATED BY space.
  IF p_devc = '$TMP'.
    t_9100_2 = ''.
  ELSE.
    CONCATENATE '����: ' p_trkorr INTO t_9100_2 SEPARATED BY space.
    CONCATENATE t_9100_2 '��' t_trkorr '��' INTO t_9100_2.
  ENDIF.
  %_p_yes_%_app_%-text = '�����롰YES��ȷ����������'.
  g_9100_ok = ''.
ENDFORM.                    "selection_screen_pbo_9100
*&---------------------------------------------------------------------*
*&      Form  selection_screen_pbo_9200
*&---------------------------------------------------------------------*
FORM selection_screen_pbo_9200.
  PERFORM selection_screen_pf_status.
  t_9200_1 = '����/�滻ǰ׺'.
  %_p_prefix_%_app_%-text = '��ǰ׺'.
  %_p_preold_%_app_%-text = '��ǰ׺'.
  g_9200_ok = ''.
ENDFORM.                    "selection_screen_pbo_9200
*&---------------------------------------------------------------------*
*&      Form  selection_screen_pbo_9300
*&---------------------------------------------------------------------*
FORM selection_screen_pbo_9300.
  PERFORM selection_screen_pf_status.
  %_p_tabnam_%_app_%-text = 'Table Name'.
  %_p_tabtxt_%_app_%-text = 'Table Description'.
  %_p_conflg_%_app_%-text = 'Delivery class'.
  %_p_mntflg_%_app_%-text = 'Data Browser/Table view maint.'.
  %_p_tabart_%_app_%-text = 'Data class'.
  %_p_tabkat_%_app_%-text = 'Size category'.
  g_9300_ok = ''.
ENDFORM.                    "selection_screen_pbo_9300
*&---------------------------------------------------------------------*
*&      Form  selection_screen_pbo_9400
*&---------------------------------------------------------------------*
FORM selection_screen_pbo_9400.
  SET PF-STATUS 'SHBTCEVT' OF PROGRAM 'SAPLBTCH'.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 9400.
  WRITE: 'ʹ��˵����'.
  ULINE.
  WRITE: '1.ʹ��ǰ�����ȴ���һ�����ض������һ�£�ȷ��û������ʹ��', /.
  WRITE: '2.�ֶ���->����Ԫ�أ����[���ֶ�]�����ġ�����Ԫ�ء�Ϊ�գ���ѡ��ֶ��������Ƶ�����', /.
  WRITE: '3.��Ҫ������ָ����Ԫ���Ƿ���Ҫ����������ǰ׺ǰ����Ҫ�ȹ�ѡ����', /.
  WRITE: '4.����Ԫ��->���������[���ֶ�]�����ġ�����Ԫ�ء���Ҫ�������ҡ�������Ϊ�գ���ѡ�����Ԫ�ء����Ƶ���������', /.
  WRITE: '5.������->�����ƣ����[���ֶ�]�����ġ�����Ԫ�ء���Ҫ�������ҡ������򡱲�Ϊ�գ����ơ������򡱼������õ�[��]���', /.
  WRITE: '6.˫������Ҫ���������������򡱡��������ơ���ALV�б��⣬������Ч', /.
  WRITE: '7.ͳһ����ǰ׺������Ϊ������Ԫ�ء��򡰲���������ͳһ����/�滻ǰ׺����:Z��ZMM��', /.
  WRITE: '8.˫��������Ԫ�ء����ߡ��򡱣�������ת��SE11�Ķ�Ӧ���棬���㴴���ɹ�����б�Ҫ�ĸ��Ĳ���', /.
  WRITE: '9.ע�ⴴ��˳�򣺴����� -> ��������Ԫ�� -> �������ݿ��', /.
  WRITE: '10.�������ݿ��󣬻��Զ���ת��SE11�ĸ��Ľ��棬�����й�ѡ��������������������ֶΡ�����ֶε���Ϣ��֮�����м���', /.
  ULINE.
  WRITE: 'GitHub��ַ��http://github.com/Leo-SDU', /.
  WRITE: '΢�Ź��ںţ�SAP����'.
  ULINE.
  WRITE: '�˹���������BUG��������©�����˶���ʹ�ô˹�����ɵ��κβ���������е��κ����Σ�', /.
  WRITE: '����BUG�뷴����΢��/QQ:286503700��', /.
  WRITE: 'δ��������ɣ����������κ���ҵ��;��'.
  LEAVE SCREEN.
ENDFORM.                    "selection_screen_pbo_9400
*&---------------------------------------------------------------------*
*&      Form  selection_screen_pai_1000
*&---------------------------------------------------------------------*
FORM selection_screen_pai_1000.
  g_ucomm = sscrfields-ucomm.
  CLEAR sscrfields-ucomm.

  go_alv_field->check_changed_data( ).
  go_alv_doma->check_changed_data( ).

  CASE g_ucomm.
    WHEN 'FC01'.
      CALL SELECTION-SCREEN 9400 STARTING AT 10 1 ENDING AT 150 25.
    WHEN 'FC02'.
      CLEAR g_error.
      PERFORM check_devc.
      CHECK g_error = ''.
      PERFORM check_doma.
      CHECK g_error = ''.
      PERFORM create_doma.
    WHEN 'FC03'.
      CLEAR g_error.
      PERFORM check_devc.
      CHECK g_error = ''.
      PERFORM check_field.
      CHECK g_error = ''.
      PERFORM create_dtel.
    WHEN 'FC04'.
      CLEAR g_error.
      PERFORM check_devc.
      CHECK g_error = ''.
      PERFORM create_tabl.
    WHEN 'FC05'.
      CALL SELECTION-SCREEN 9500 STARTING AT 20 10.
    WHEN 'TMP'.
      IF p_devc <> '$TMP'.
        p_devc = '$TMP'.
      ELSE.
        p_devc = ''.
      ENDIF.
  ENDCASE.
ENDFORM.                    "selection_screen_pai_1000
*&---------------------------------------------------------------------*
*&      Form  selection_screen_pai_9100
*&---------------------------------------------------------------------*
FORM selection_screen_pai_9100.
  g_ucomm = sscrfields-ucomm.
  CLEAR sscrfields-ucomm.
  IF g_ucomm = 'GOON'.
    IF p_yes = 'YES'.
      g_9100_ok = 'X'.
      LEAVE TO SCREEN 0.
    ENDIF.
  ELSEIF g_ucomm = 'BACK'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.                    "selection_screen_pai_9100
*&---------------------------------------------------------------------*
*&      Form  selection_screen_pai_9200
*&---------------------------------------------------------------------*
FORM selection_screen_pai_9200.
  g_ucomm = sscrfields-ucomm.
  CLEAR sscrfields-ucomm.
  IF g_ucomm = 'GOON'.
    g_9200_ok = 'X'.
    LEAVE TO SCREEN 0.
  ELSEIF g_ucomm = 'BACK'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.                    "selection_screen_pai_9200
*&---------------------------------------------------------------------*
*&      Form  selection_screen_pai_9300
*&---------------------------------------------------------------------*
FORM selection_screen_pai_9300.
  g_ucomm = sscrfields-ucomm.
  CLEAR sscrfields-ucomm.
  IF g_ucomm = 'GOON'.
    IF p_tabnam(1) NA 'YZ'.
      MESSAGE '����������Y��Z��ͷ' TYPE 'E'.
    ELSE.
      SELECT SINGLE tabname INTO p_tabnam FROM dd02l WHERE tabname = p_tabnam.
      IF sy-subrc = 0.
        MESSAGE '�ñ��Ѿ�����' TYPE 'E'.
      ELSE.
        SELECT SINGLE typekind INTO g_typekind FROM ddtypes WHERE typename = p_tabnam.
        IF sy-subrc = 0.
          MESSAGE e001(00) WITH p_tabnam '�ѱ�����Ϊ����' g_typekind.
        ENDIF.
        g_9300_ok = 'X'.
        LEAVE TO SCREEN 0.
      ENDIF.
    ENDIF.
  ELSEIF g_ucomm = 'BACK'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.                    "selection_screen_pai_9300
*&---------------------------------------------------------------------*
*&      FORM  CREATE_SCREEN_OBJECT
*&---------------------------------------------------------------------*
FORM create_screen_object .
  CHECK go_docking_con IS INITIAL.
  "��������
  CREATE OBJECT go_docking_con
    EXPORTING
      ratio = 85
      side  = cl_gui_docking_container=>dock_at_bottom.

  CREATE OBJECT go_splitter_con
    EXPORTING
      parent  = go_docking_con
      rows    = 2
      columns = 1.

  go_con_field = go_splitter_con->get_container( row = 1 column = 1 ).
  go_con_doma = go_splitter_con->get_container( row = 2 column = 1 ).

  "����DTEL��ALV
  CREATE OBJECT go_alv_field
    EXPORTING
      i_parent = go_con_field.

  "�¼���Ӧ
  CREATE OBJECT go_event_field
    EXPORTING
      io_alv              = go_alv_field
      i_f4_form           = 'HANDLE_F4'
      i_toolbar_form      = 'HANDLE_TOOLBAR_FIELD'
      i_user_command_form = 'HANDLE_USER_COMMAND_FIELD'
      i_double_click_form = 'HANDLE_DOUBLE_CLICK_FIELD'.

  "�������Ͳ���
  PERFORM alv_prepare_toolbar   TABLES    gt_exclude.
  PERFORM alv_prepare_layout    CHANGING  gs_layout.

  "��ʾALV
  d_build_fieldcat_field:
    'FIELDNAME' 'X'  ' '    ' '  16    '     '  '     '     '�ֶ���(��󳤶�16)',
    'ROLLNAME'  'X'  ' '    ' '  30    'DD04L'  'ROLLNAME'  '����Ԫ��(˫���鿴)',
    'CREATE'    'X'  'X'    ' '  20    '     '  '     '     '��Ҫ����(˫����������Ч)',
    'DOMNAME'   'X'  ' '    ' '  30    '     '  '     '     '������(˫����������Ч)',
    'DATATYPE'  'X'  ' '    'X'   9    '     '  '     '     'Ԥ��������',
    'LENG'      'X'  ' '    ' '   8    '     '  '     '     '����',
    'DECIMALS'  'X'  ' '    ' '   4    '     '  '     '     'С��',
    'DDTEXT'    'X'  ' '    ' '  30    'DD04V'  'DDTEXT'    '����',
    'RESULT'    ' '  ' '    ' '  30    '     '  '     '     '�������'.
  gs_layout-grid_title = '���ֶΡ�������'.
  CALL METHOD go_alv_field->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding = gt_exclude
      is_layout            = gs_layout
    CHANGING
      it_outtab            = gt_fields
      it_fieldcatalog      = gt_fieldcat_field.

  "ע���¼�
  go_alv_field->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
  go_alv_field->register_f4_for_fields( EXPORTING it_f4 = gt_f4_field ).

  "����DOMA��ALV
  CREATE OBJECT go_alv_doma
    EXPORTING
      i_parent = go_con_doma.

  "�¼���Ӧ
  CREATE OBJECT go_event_doma
    EXPORTING
      io_alv              = go_alv_doma
      i_f4_form           = 'HANDLE_F4'
      i_toolbar_form      = 'HANDLE_TOOLBAR_DOMA'
      i_user_command_form = 'HANDLE_USER_COMMAND_DOMA'
      i_double_click_form = 'HANDLE_DOUBLE_CLICK_DOMA'.

  "��ʾALV
  d_build_fieldcat_doma:
    'DOMNAME'   'X'  ' '    ' '  30    '     '  '     '     '������(˫���в鿴��˫��������Ч)',
    'DATATYPE'  'X'  ' '    'X'   9    '     '  '     '     'Ԥ��������',
    'LENG'      'X'  ' '    ' '   8    '     '  '     '     '����',
    'DECIMALS'  'X'  ' '    ' '   4    '     '  '     '     'С��',
    'LOWERCASE' 'X'  'X'    ' '   6    '     '  '     '     '��Сд',
    'SIGNFLAG'  'X'  'X'    ' '   4    '     '  '     '     '����',
    'DDTEXT'    'X'  ' '    ' '  30    'DD04V'  'DDTEXT'    '����',
    'RESULT'    ' '  ' '    ' '  30    '     '  '     '     '�������'.


  gs_layout-grid_title = '�򡪡�����'.
  CALL METHOD go_alv_doma->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding = gt_exclude
      is_layout            = gs_layout
    CHANGING
      it_outtab            = gt_domas
      it_fieldcatalog      = gt_fieldcat_doma.

  "ע���¼�
  go_alv_doma->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
  go_alv_doma->register_f4_for_fields( EXPORTING it_f4 = gt_f4_doma ).
ENDFORM.                    " CREATE_SCREEN_OBJECT
*&---------------------------------------------------------------------*
*&      FORM  F4_TRKORR
*&---------------------------------------------------------------------*
FORM f4_trkorr.
  DATA: lt_e071  TYPE TABLE OF e071,
        lt_e071k TYPE TABLE OF e071k.

  CALL FUNCTION 'TRINT_ORDER_CHOICE'
    EXPORTING
      wi_order_type = 'K'
      wi_task_type  = 'S'
    IMPORTING
      we_order      = p_trkorr
      we_task       = g_task
    TABLES
      wt_e071       = lt_e071
      wt_e071k      = lt_e071k
    EXCEPTIONS
      OTHERS        = 1.
ENDFORM.                                                    "F4_TRKORR
*&---------------------------------------------------------------------*
*&      Form  f4_tabart
*&---------------------------------------------------------------------*
FORM f4_tabart.
  DATA: lt_values TYPE vrm_values.

  SELECT
    tabart AS key
    darttext AS text
    INTO TABLE lt_values
    FROM dartt
    WHERE ddlangu = sy-langu.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_TABART'
      values = lt_values.
ENDFORM.                                                    "f4_tabart
*&---------------------------------------------------------------------*
*&      FORM  HANDLE_TOOLBAR_FIELD
*&---------------------------------------------------------------------*
FORM handle_toolbar_field USING   e_object TYPE REF TO cl_alv_event_toolbar_set
                                  e_interactive TYPE char1.
  PERFORM add_button_to_toolbar USING     'SALL'
                                          icon_select_all
                                          cntb_btype_button
                                          ''
                                          'ȫ����Ҫ��������Ԫ��'
                                CHANGING  e_object.

  PERFORM add_button_to_toolbar USING     'DSAL'
                                          icon_deselect_all
                                          cntb_btype_button
                                          ''
                                          'ȡ��ȫѡ'
                                CHANGING  e_object.

  PERFORM add_button_to_toolbar USING     ''
                                          ''
                                          cntb_btype_sep
                                          ''
                                          ''
                                CHANGING  e_object.

  PERFORM add_button_to_toolbar USING     'DTEL'
                                          icon_transfer
                                          cntb_btype_button
                                          '�ֶ���->����Ԫ��'
                                          'Ϊ������Ԫ�ء�Ϊ�յ���ͬ���ֶ���'
                                CHANGING  e_object.

  PERFORM add_button_to_toolbar USING     'PREFIX_DTEL'
                                          icon_change
                                          cntb_btype_button
                                          '����ǰ׺(����Ԫ��)'
                                          'Ϊ����Ԫ��ͳһ����ǰ׺'
                                CHANGING  e_object.

  PERFORM add_button_to_toolbar USING     'DOMA'
                                          icon_transfer
                                          cntb_btype_button
                                          '����Ԫ��->������'
                                          'Ϊ��������Ϊ�յ�����Ԫ��ͬ��������'
                                CHANGING  e_object.

  PERFORM add_button_to_toolbar USING     'PREFIX_DOMA'
                                          icon_change
                                          cntb_btype_button
                                          '����ǰ׺(��)'
                                          'Ϊ��ͳһ����ǰ׺'
                                CHANGING  e_object.

  PERFORM add_button_to_toolbar USING     ''
                                          ''
                                          cntb_btype_sep
                                          ''
                                          ''
                                CHANGING  e_object.

  PERFORM add_button_to_toolbar USING     'CHECK'
                                          icon_check
                                          cntb_btype_button
                                          '���'
                                          '���'
                                CHANGING  e_object.
ENDFORM.                    "HANDLE_TOOLBAR_FIELD
*&---------------------------------------------------------------------*
*&      FORM  HANDLE_TOOLBAR_DOMA
*&---------------------------------------------------------------------*
FORM handle_toolbar_doma USING   e_object TYPE REF TO cl_alv_event_toolbar_set
                                 e_interactive TYPE char1.
  PERFORM add_button_to_toolbar USING     'TRANSFER'
                                          icon_transfer
                                          cntb_btype_button
                                          '������->������'
                                          'ͬ���ϱ�Ĳ����������õ�������'
                                CHANGING  e_object.

  PERFORM add_button_to_toolbar USING     ''
                                          ''
                                          cntb_btype_sep
                                          ''
                                          ''
                                CHANGING  e_object.

  PERFORM add_button_to_toolbar USING     'CHECK'
                                          icon_check
                                          cntb_btype_button
                                          '���'
                                          '���'
                                CHANGING  e_object.
ENDFORM.                    "HANDLE_TOOLBAR_DOMA
*&---------------------------------------------------------------------*
*&      FORM  ADD_BUTTON_TO_TOOLBAR
*&---------------------------------------------------------------------*
FORM add_button_to_toolbar USING p_function p_icon p_butn_type p_text p_quickinfo
                           CHANGING p_object TYPE REF TO cl_alv_event_toolbar_set.
  DATA: ls_toolbar TYPE stb_button.

  ls_toolbar-function = p_function.
  ls_toolbar-icon = p_icon.
  ls_toolbar-butn_type = p_butn_type.
  ls_toolbar-text = p_text.
  ls_toolbar-quickinfo = p_quickinfo.
  APPEND ls_toolbar TO p_object->mt_toolbar.
ENDFORM.                    "add_button_to_toolbar
*&---------------------------------------------------------------------*
*&      FORM  HANDLE_USER_COMMAND_FIELD
*&---------------------------------------------------------------------*
FORM handle_user_command_field  USING    e_ucomm TYPE sy-ucomm.
  DATA: ok_code TYPE sy-ucomm,
        ls_doma TYPE typ_doma,
        l_flag TYPE c.
  FIELD-SYMBOLS: <ls_field> TYPE typ_field.

  ok_code = e_ucomm.
  CLEAR e_ucomm.

  CASE ok_code.
    WHEN 'SALL'.
      LOOP AT gt_fields ASSIGNING <ls_field>.
        <ls_field>-create = 'X'.
      ENDLOOP.
      go_alv_field->refresh_table_display( ).
    WHEN 'DSAL'.
      LOOP AT gt_fields ASSIGNING <ls_field>.
        <ls_field>-create = ''.
      ENDLOOP.
      go_alv_field->refresh_table_display( ).
    WHEN 'DTEL'.
      LOOP AT gt_fields ASSIGNING <ls_field> WHERE rollname = ''.
        <ls_field>-rollname = <ls_field>-fieldname.
      ENDLOOP.
      go_alv_field->refresh_table_display( ).
    WHEN 'PREFIX_DTEL'.
      CHECK gt_fields IS NOT INITIAL.
      READ TABLE gt_fields WITH KEY create = 'X' TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        show_error_message '������Ϊһ������Ԫ�ع�ѡ����Ҫ������'.
      ENDIF.
      CALL SELECTION-SCREEN 9200 STARTING AT 20 10.
      CHECK g_9200_ok = 'X'.
      LOOP AT gt_fields ASSIGNING <ls_field> WHERE create = 'X'.
        PERFORM change_prefix CHANGING <ls_field>-rollname.
      ENDLOOP.
      go_alv_field->refresh_table_display( ).
    WHEN 'DOMA'.
      LOOP AT gt_fields ASSIGNING <ls_field> WHERE rollname(1) CA 'YZ' AND create = 'X' AND domname = ''.
        <ls_field>-domname = <ls_field>-rollname.
      ENDLOOP.
      go_alv_field->refresh_table_display( ).
    WHEN 'PREFIX_DOMA'.
      CHECK gt_fields IS NOT INITIAL.
      LOOP AT gt_fields ASSIGNING <ls_field> WHERE create = 'X' AND domname <> ''.
        l_flag = 'X'.
        EXIT.
      ENDLOOP.
      IF l_flag = ''.
        show_error_message: 'û����Ҫ����ǰ׺�Ĳ�����'.
      ENDIF.
      CALL SELECTION-SCREEN 9200 STARTING AT 20 10.
      CHECK g_9200_ok = 'X'.
      LOOP AT gt_fields ASSIGNING <ls_field> WHERE create = 'X' AND domname <> ''.
        PERFORM change_prefix CHANGING <ls_field>-domname.
      ENDLOOP.
      go_alv_field->refresh_table_display( ).
    WHEN 'CHECK'.
      CLEAR g_error.
      PERFORM check_field.
  ENDCASE.
ENDFORM.                    "HANDLE_USER_COMMAND_FIELD
*&---------------------------------------------------------------------*
*&      FORM  HANDLE_USER_COMMAND_DOMA
*&---------------------------------------------------------------------*
FORM handle_user_command_doma  USING    e_ucomm TYPE sy-ucomm.
  DATA: ok_code TYPE sy-ucomm,
        ls_doma TYPE typ_doma.
  FIELD-SYMBOLS: <ls_field> TYPE typ_field.

  ok_code = e_ucomm.
  CLEAR e_ucomm.

  CASE ok_code.
    WHEN 'TRANSFER'.
      go_alv_field->check_changed_data( ).
      LOOP AT gt_fields ASSIGNING <ls_field> WHERE create = 'X' AND domname <> ''.
        CLEAR ls_doma.
        ls_doma-domname   = <ls_field>-domname.
        ls_doma-datatype  = <ls_field>-datatype.
        ls_doma-leng      = <ls_field>-leng.
        ls_doma-decimals  = <ls_field>-decimals.
        CASE ls_doma-datatype.
          WHEN 'FLTP' OR 'QUAN' OR 'CURR' OR 'DEC' OR 'INT2' OR 'INT4'.
            ls_doma-signflag  = 'X'.
          WHEN 'CHAR' OR 'STRG'.
            ls_doma-lowercase = 'X'.
        ENDCASE.
        ls_doma-ddtext    = <ls_field>-ddtext.
        APPEND ls_doma TO gt_domas.
      ENDLOOP.
      go_alv_doma->refresh_table_display( ).
    WHEN 'CHECK'.
      CLEAR g_error.
      PERFORM check_doma.
  ENDCASE.
ENDFORM.                    "HANDLE_USER_COMMAND_DOMA
*&---------------------------------------------------------------------*
*&      FORM  HANDLE_DOUBLE_CLICK_FIELD
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM handle_double_click_field USING  e_row TYPE lvc_s_row
                                      e_column TYPE lvc_s_col
                                      es_row_no TYPE lvc_s_roid.
  DATA: l_rollname TYPE dd04l-rollname,
        l_answer TYPE c.
  FIELD-SYMBOLS: <ls_field> TYPE typ_field.

  IF e_row-index IS INITIAL.
    CASE e_column-fieldname.
      WHEN 'CREATE'.
        MESSAGE '��ʾ�����������Ԫ�ء��Ѿ����ڣ��ҹ�ѡ�ˡ���Ҫ����������ȡ������Ҫ�������ı�ʶ' TYPE 'I'.

        LOOP AT gt_fields ASSIGNING <ls_field> WHERE create = 'X' AND rollname <> ''.
          SELECT SINGLE rollname INTO l_rollname FROM dd04l WHERE rollname = <ls_field>-rollname.
          IF sy-subrc = 0.
            <ls_field>-create = ''.
          ENDIF.
        ENDLOOP.
        go_alv_field->refresh_table_display( ).
      WHEN 'DOMNAME'.
        MESSAGE '��ʾ���������Ԫ�صġ���������[��]��񲻴��ڣ�������䡰�������ֶ�' TYPE 'I'.
        go_alv_doma->check_changed_data( ).
        LOOP AT gt_fields ASSIGNING <ls_field> WHERE create = 'X' AND domname <> ''.
          READ TABLE gt_domas WITH KEY domname = <ls_field>-domname TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            <ls_field>-domname = ''.
          ENDIF.
        ENDLOOP.
        go_alv_field->refresh_table_display( ).
    ENDCASE.

    RETURN.
  ENDIF.

  CHECK e_column-fieldname = 'ROLLNAME'.

  READ TABLE gt_fields ASSIGNING <ls_field> INDEX e_row-index.

  CALL FUNCTION 'RS_TOOL_ACCESS'
    EXPORTING
      operation           = 'SHOW'
      object_name         = <ls_field>-rollname
      object_type         = 'DTEL'
    EXCEPTIONS
      not_executed        = 1
      invalid_object_type = 2
      OTHERS              = 3.
ENDFORM.                    "HANDLE_BEFORE_UCOMM
*&---------------------------------------------------------------------*
*&      FORM  HANDLE_DOUBLE_CLICK_DOMA
*&---------------------------------------------------------------------*
FORM handle_double_click_doma USING  e_row TYPE lvc_s_row
                                     e_column TYPE lvc_s_col
                                     es_row_no TYPE lvc_s_roid.
  DATA: l_domname TYPE domname.
  FIELD-SYMBOLS: <ls_doma> TYPE typ_doma.

  CHECK e_column-fieldname = 'DOMNAME'.
  IF e_row-index IS INITIAL.
    MESSAGE '��ʾ������������ơ��Ѿ����ڣ���ɾ��֮' TYPE 'I'.
    LOOP AT gt_domas ASSIGNING <ls_doma> WHERE domname <> ''.
      SELECT SINGLE domname INTO l_domname FROM dd01l WHERE domname = <ls_doma>-domname.
      IF sy-subrc = 0.
        DELETE gt_domas.
      ENDIF.
    ENDLOOP.
    go_alv_doma->refresh_table_display( ).
    RETURN.
  ENDIF.

  READ TABLE gt_domas ASSIGNING <ls_doma> INDEX e_row-index.

  CALL FUNCTION 'RS_TOOL_ACCESS'
    EXPORTING
      operation           = 'SHOW'
      object_name         = <ls_doma>-domname
      object_type         = 'DOMA'
    EXCEPTIONS
      not_executed        = 1
      invalid_object_type = 2
      OTHERS              = 3.
ENDFORM.                    "HANDLE_BEFORE_UCOMM
*&---------------------------------------------------------------------*
*&      FORM  HANDLE_F4
*&---------------------------------------------------------------------*
FORM handle_f4  USING          e_fieldname TYPE lvc_fname
                               e_fieldvalue TYPE lvc_value
                               es_row_no TYPE lvc_s_roid
                               er_event_data TYPE REF TO cl_alv_event_data
                               et_bad_cells TYPE lvc_t_modi
                               e_display TYPE char01.
  DATA: ls_row  TYPE lvc_s_row,
        ls_col  TYPE lvc_s_col,
        ls_modi TYPE lvc_s_modi.
  FIELD-SYMBOLS: <lt_modi> TYPE lvc_t_modi.

  er_event_data->m_event_handled = 'X'.

  CASE e_fieldname.
    WHEN 'DATATYPE'.
      PERFORM f4_datatype CHANGING e_fieldvalue.

    WHEN OTHERS.
      EXIT.
  ENDCASE.

  ASSIGN er_event_data->m_data->* TO <lt_modi>.
  ls_modi-row_id    = es_row_no-row_id."
  ls_modi-fieldname = e_fieldname.
  ls_modi-value     = e_fieldvalue.
  APPEND ls_modi TO <lt_modi>.
ENDFORM.                                                    "HANDLE_F4
*&---------------------------------------------------------------------*
*&      FORM  ALV_PREPARE_TOOLBAR
*&---------------------------------------------------------------------*
FORM alv_prepare_toolbar  TABLES  pt_exclude TYPE ui_functions.
  REFRESH: pt_exclude.

  APPEND cl_gui_alv_grid=>mc_fc_maximum TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_minimum TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_subtot TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sum TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_average TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_sum TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_subtot TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_find TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_filter TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_print TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_print_prev TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_export TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_graph TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_export TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_view TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_detail TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_help TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_info TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_variant TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_check TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO pt_exclude.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW TO PT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW TO PT_EXCLUDE.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO pt_exclude.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW TO PT_EXCLUDE.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_paste TO pt_exclude.
ENDFORM.                    "ALV_PREPARE_TOOLBAR
*&---------------------------------------------------------------------*
*&      FORM  ALV_PREPARE_LAYOUT
*&---------------------------------------------------------------------*
FORM alv_prepare_layout  CHANGING ps_layout TYPE lvc_s_layo.
  ps_layout-sel_mode = 'A'.
  ps_layout-smalltitle = 'X'.
ENDFORM.                    "ALV_PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*&      FORM  F4_DATATYPE
*&---------------------------------------------------------------------*
FORM f4_datatype CHANGING p_fieldvalue.
  DATA: lt_return TYPE TABLE OF ddshretval WITH HEADER LINE,
        BEGIN OF lt_datatypes OCCURS 0,
          datatype_d TYPE datatype_d,
          as4text    TYPE as4text,
        END OF lt_datatypes.

  SELECT
    domvalue_l AS datatype_d
    ddtext AS as4text
    INTO TABLE lt_datatypes
    FROM dd07t
    WHERE domname = 'DATATYPE' AND ddlanguage = sy-langu.

* ����F4
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield     = 'DATATYPE_D'
      window_title = 'ѡ��'
      value_org    = 'S'
    TABLES
      value_tab    = lt_datatypes[]
      return_tab   = lt_return[].

  IF lt_return[] IS NOT INITIAL.
    READ TABLE lt_return INDEX 1.
    p_fieldvalue = lt_return-fieldval.
  ENDIF.
ENDFORM.                                                    "F4_DATATYPE
*&---------------------------------------------------------------------*
*&      FORM  DYNP_VALUES_UPDATE
*&---------------------------------------------------------------------*
FORM dynp_values_update USING p_dynnr p_fieldname p_stepl p_fieldvalue.
  DATA: lt_dynpfields TYPE TABLE OF dynpread WITH HEADER LINE.

  lt_dynpfields-stepl      = p_stepl.
  lt_dynpfields-fieldname  = p_fieldname.
  lt_dynpfields-fieldvalue = p_fieldvalue.
  APPEND lt_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid   "��������
      dynumb     = p_dynnr    "��Ļ���
    TABLES
      dynpfields = lt_dynpfields[].
ENDFORM.                    "DYNP_VALUES_UPDATE
*&---------------------------------------------------------------------*
*&      FORM  CHECK_DEVC
*&---------------------------------------------------------------------*
FORM check_devc.
  DATA: l_user TYPE e070-as4user.
  IF p_devc IS INITIAL.
    show_error_message '�����಻��Ϊ��'.
  ENDIF.

  IF p_devc <> '$TMP'.
    SELECT SINGLE devclass INTO p_devc FROM tdevc WHERE devclass = p_devc.
    IF sy-subrc <> 0.
      show_error_message '�����಻����'.
    ENDIF.

    IF p_devc(1) NA 'YZ'.
      show_error_message '����ʹ�ñ�׼�Ŀ�����'.
    ENDIF.

    IF p_trkorr IS INITIAL.
      show_error_message '����Ų���Ϊ��'.
    ELSE.
      SELECT SINGLE as4user INTO l_user FROM e070 WHERE trkorr = p_trkorr AND trfunction = 'K' AND trstatus IN ('D', 'R').
      IF sy-subrc <> 0.
        show_error_message '�������Ϊδ�ͷŵĹ���̨����'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "CHECK_DEVC
*&---------------------------------------------------------------------*
*&      FORM  CHECK_FIELD
*&---------------------------------------------------------------------*
FORM check_field.
  DATA: lt_fields1 TYPE TABLE OF typ_field,
        lt_fields2 TYPE TABLE OF typ_field.
  DATA: l_typekind TYPE ddtypekind.
  DATA: l_flag_line TYPE c, "���������д���
        l_flag_all  TYPE c. "�����������д���
  FIELD-SYMBOLS: <ls_field> TYPE typ_field.

  CHECK gt_fields IS NOT INITIAL.

  LOOP AT gt_fields ASSIGNING <ls_field>.
    IF <ls_field>-create = ''.
      <ls_field>-result = ''.
      CONTINUE.
    ENDIF.

    CLEAR l_flag_line.

    IF <ls_field>-rollname IS INITIAL.
      <ls_field>-result = '����Ԫ�ز���Ϊ��'.
      l_flag_line = l_flag_all = 'X'.
    ENDIF.

    CHECK l_flag_line = ''.
    IF <ls_field>-rollname(1) CA 'YZ'.
      SELECT SINGLE rollname INTO <ls_field>-rollname FROM dd04l WHERE rollname = <ls_field>-rollname.
      IF sy-subrc = 0.
        <ls_field>-result = '����Ԫ���Ѵ���'.
        l_flag_line = l_flag_all = 'X'.
      ELSE.
        SELECT SINGLE typekind INTO l_typekind FROM ddtypes WHERE typename = <ls_field>-rollname.
        IF sy-subrc = 0.
          CONCATENATE <ls_field>-rollname '�ѱ�����Ϊ����' l_typekind INTO <ls_field>-result.
          l_flag_line = l_flag_all = 'X'.
        ENDIF.
      ENDIF.
    ELSE.
      <ls_field>-result = '����Ԫ����������'.
      l_flag_line = l_flag_all = 'X'.
    ENDIF.

    CHECK l_flag_line = ''.
    IF <ls_field>-domname IS NOT INITIAL.
      SELECT SINGLE domname INTO <ls_field>-domname FROM dd01l WHERE domname = <ls_field>-domname AND as4local = 'A'.
      IF sy-subrc <> 0.
        <ls_field>-result = '�򲻴��ڻ�δ����'.
        l_flag_line = l_flag_all = 'X'.
      ENDIF.
    ENDIF.

    CHECK l_flag_line = ''.
    IF <ls_field>-datatype IS NOT INITIAL.
      SELECT SINGLE domvalue_l INTO <ls_field>-datatype FROM dd07l WHERE domvalue_l = <ls_field>-datatype.
      IF sy-subrc <> 0.
        <ls_field>-result = 'Ԥ�������Ͳ�����'.
        l_flag_line = l_flag_all = 'X'.
      ENDIF.
    ENDIF.

    CHECK l_flag_line = ''.
    IF <ls_field>-domname IS INITIAL AND <ls_field>-datatype IS INITIAL.
      <ls_field>-result = '����Ԫ������δָ��'.
      l_flag_line = l_flag_all = 'X'.
    ENDIF.
  ENDLOOP.

  IF l_flag_all = 'X'.
    go_alv_field->refresh_table_display( ).
    show_error_message '����Ԫ�����ô���'.
  ENDIF.

  "��鹴ѡ���У������ظ�������Ԫ��
  lt_fields1 = gt_fields.
  DELETE lt_fields1 WHERE create = ''.
  lt_fields2 = lt_fields1.
  SORT lt_fields2 BY rollname.
  DELETE ADJACENT DUPLICATES FROM lt_fields2 COMPARING rollname.
  IF lines( lt_fields2 ) <> lines( lt_fields1 ).
    show_error_message '����Ԫ�ش����ظ�'.
  ENDIF.
ENDFORM.                    " CHECK_FIELD
*&---------------------------------------------------------------------*
*&      FORM  CHECK_DOMA
*&---------------------------------------------------------------------*
FORM check_doma.
  DATA: lt_domas TYPE TABLE OF typ_doma.
  DATA: l_flag_line TYPE c,
        l_flag_all  TYPE c.
  FIELD-SYMBOLS: <ls_doma> TYPE typ_doma.

  CHECK gt_domas IS NOT INITIAL.

  LOOP AT gt_domas ASSIGNING <ls_doma>.
    CLEAR l_flag_line.

    IF <ls_doma>-domname IS INITIAL.
      <ls_doma>-result = '�����Ʋ���Ϊ��'.
      l_flag_line = l_flag_all = 'X'.
    ENDIF.

    CHECK l_flag_line = ''.
    IF <ls_doma>-domname(1) CA 'YZ'.
      SELECT SINGLE domname INTO <ls_doma>-domname FROM dd01l WHERE domname = <ls_doma>-domname.
      IF sy-subrc = 0.
        <ls_doma>-result = '�������Ѵ���'.
        l_flag_line = l_flag_all = 'X'.
      ENDIF.
    ELSE.
      <ls_doma>-result = '��������������'.
      l_flag_line = l_flag_all = 'X'.
    ENDIF.

    CHECK l_flag_line = ''.
    IF <ls_doma>-datatype IS NOT INITIAL.
      SELECT SINGLE domvalue_l INTO <ls_doma>-datatype FROM dd07l WHERE domvalue_l = <ls_doma>-datatype.
      IF sy-subrc <> 0.
        <ls_doma>-result = 'Ԥ�������Ͳ�����'.
        l_flag_line = l_flag_all = 'X'.
      ENDIF.
    ELSE.
      <ls_doma>-result = '����������δָ��'.
      l_flag_line = l_flag_all = 'X'.
    ENDIF.

  ENDLOOP.

  IF l_flag_all = 'X'.
    go_alv_doma->refresh_table_display( ).
    show_error_message '���������ô���'.
  ENDIF.

  lt_domas = gt_domas.
  SORT lt_domas BY domname.
  DELETE ADJACENT DUPLICATES FROM lt_domas COMPARING domname.
  IF lines( lt_domas ) <> lines( gt_domas ).
    show_error_message '�����ƴ����ظ�'.
  ENDIF.
ENDFORM.                    "CHECK_DOMA
*&---------------------------------------------------------------------*
*&      FORM  CREATE_DTEL
*&---------------------------------------------------------------------*
FORM create_dtel .
  DATA: ls_object TYPE ddenqs,
        l_failed  TYPE c,
        l_message TYPE string,
        l_rc      TYPE i,
        ls_dd04v  TYPE dd04v.
  FIELD-SYMBOLS: <ls_field> TYPE typ_field.

  IF gt_fields IS INITIAL.
    MESSAGE '������ά��һ������Ԫ��' TYPE 'S'.
    RETURN.
  ENDIF.

  READ TABLE gt_fields WITH KEY create = 'X' TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    MESSAGE '��ѡ����Ҫ����������Ԫ��' TYPE 'S'.
    RETURN.
  ENDIF.

  p_yes = ''.
  CALL SELECTION-SCREEN 9100 STARTING AT 20 10.
  IF g_9100_ok = ''.
    MESSAGE 'û��ִ�д�������' TYPE 'S'.
    RETURN.
  ENDIF.

  LOOP AT gt_fields ASSIGNING <ls_field> WHERE create = 'X'.
    ls_object-objtype = 'DTEL'.
    ls_object-objname = <ls_field>-rollname.

    PERFORM rs_corr_insert USING 'DICT' ls_object CHANGING l_failed l_message.
    IF l_failed = 'X'.
      <ls_field>-result = 'ʧ�ܣ�RS_CORR_INSERT����'.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING <ls_field> TO ls_dd04v.
    ls_dd04v-ddlanguage = sy-langu.
    ls_dd04v-headlen = 10.
    ls_dd04v-scrlen1 = 10.
    ls_dd04v-scrlen2 = 20.
    ls_dd04v-scrlen3 = 30.
    ls_dd04v-reptext = <ls_field>-ddtext.
    ls_dd04v-scrtext_s = <ls_field>-ddtext.
    ls_dd04v-scrtext_m = <ls_field>-ddtext.
    ls_dd04v-scrtext_l = <ls_field>-ddtext.

    CALL FUNCTION 'DDIF_DTEL_PUT'
      EXPORTING
        name              = <ls_field>-rollname
        dd04v_wa          = ls_dd04v
      EXCEPTIONS
        dtel_not_found    = 1
        name_inconsistent = 2
        dtel_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      <ls_field>-result = 'ʧ�ܣ�DDIF_DTEL_PUT����'.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'DDIF_DTEL_ACTIVATE'
      EXPORTING
        name        = <ls_field>-rollname
        auth_chk    = ''
*       PRID        = -1
      IMPORTING
        rc          = l_rc
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0 OR l_rc > 4.                           "0:�ɹ���4:����
      <ls_field>-result = 'ʧ�ܣ�DDIF_DTEL_ACTIVATE����'.
      CONTINUE.
    ENDIF.

    <ls_field>-result = '�ɹ�'.
  ENDLOOP.

  go_alv_field->refresh_table_display( ).
ENDFORM.                    " CREATE_DTEL
*&---------------------------------------------------------------------*
*&      FORM  CREATE_DOMA
*&---------------------------------------------------------------------*
FORM create_doma .
  DATA: ls_object TYPE ddenqs,
        l_failed  TYPE c,
        l_message TYPE string,
        l_rc      TYPE i,
        ls_dd01v  TYPE dd01v.
  FIELD-SYMBOLS: <ls_doma> TYPE typ_doma.

  IF gt_domas IS INITIAL.
    MESSAGE '������ά��һ����' TYPE 'S'.
    RETURN.
  ENDIF.

  p_yes = ''.
  CALL SELECTION-SCREEN 9100 STARTING AT 20 10.
  IF g_9100_ok = ''.
    MESSAGE 'û��ִ�д�������' TYPE 'S'.
    RETURN.
  ENDIF.

  LOOP AT gt_domas ASSIGNING <ls_doma>.
    ls_object-objtype = 'DOMA'.
    ls_object-objname = <ls_doma>-domname.

    PERFORM rs_corr_insert USING 'DICT' ls_object CHANGING l_failed l_message.
    IF l_failed = 'X'.
      <ls_doma>-result = 'ʧ�ܣ�RS_CORR_INSERT����'.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING <ls_doma> TO ls_dd01v.
    ls_dd01v-ddlanguage = sy-langu.

    CALL FUNCTION 'DDIF_DOMA_PUT'
      EXPORTING
        name              = <ls_doma>-domname
        dd01v_wa          = ls_dd01v
      EXCEPTIONS
        doma_not_found    = 1
        name_inconsistent = 2
        doma_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      <ls_doma>-result = 'ʧ�ܣ�DDIF_DOMA_PUT����'.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'DDIF_DOMA_ACTIVATE'
      EXPORTING
        name        = <ls_doma>-domname
        auth_chk    = ''
*       PRID        = -1
      IMPORTING
        rc          = l_rc
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0 OR l_rc > 4.                           "0:�ɹ���4:����
      <ls_doma>-result = 'ʧ�ܣ�DDIF_DOMA_ACTIVATE����'.
      CONTINUE.
    ENDIF.

    <ls_doma>-result = '�ɹ�'.
  ENDLOOP.

  go_alv_doma->refresh_table_display( ).
ENDFORM.                    "CREATE_DOMA
*&---------------------------------------------------------------------*
*&      FORM  CREATE_TABL
*&---------------------------------------------------------------------*
FORM create_tabl .
  DATA: ls_object  TYPE ddenqs,
        l_failed   TYPE c,
        l_message  TYPE string,
        l_position TYPE i,
        ls_dd02v   TYPE dd02v,
        ls_dd09v   TYPE dd09v,
        ls_dd03p   TYPE dd03p,
        lt_dd03p   TYPE TABLE OF dd03p.
  FIELD-SYMBOLS: <ls_field> TYPE typ_field.

  IF gt_fields IS INITIAL.
    MESSAGE '������ά��һ���ֶ�' TYPE 'S'.
    RETURN.
  ENDIF.

  p_yes = ''.
  CALL SELECTION-SCREEN 9100 STARTING AT 20 10.
  IF g_9100_ok = ''.
    MESSAGE 'û��ִ�д�������' TYPE 'S'.
    RETURN.
  ENDIF.

  CALL SELECTION-SCREEN 9300 STARTING AT 20 10.
  IF g_9300_ok = ''.
    MESSAGE 'û��ִ�д�������' TYPE 'S'.
    RETURN.
  ENDIF.

  ls_object-objtype = 'TABL'.
  ls_object-objname = p_tabnam.

  PERFORM rs_corr_insert USING 'DICT' ls_object CHANGING l_failed l_message.
  IF l_failed = 'X'.
    MESSAGE 'ʧ�ܣ�RS_CORR_INSERT����' TYPE 'I' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  ls_dd02v-tabname = p_tabnam.
  ls_dd02v-ddtext = p_tabtxt.
  ls_dd02v-ddlanguage = sy-langu.
  ls_dd02v-tabclass = 'TRANSP'.
  ls_dd02v-contflag = p_conflg.
  ls_dd02v-mainflag = p_mntflg.

  ls_dd09v-tabname = p_tabnam.
  ls_dd09v-tabart = p_tabart.
  ls_dd09v-tabkat = p_tabkat.

  ADD 1 TO l_position.
  ls_dd03p-tabname = p_tabnam.
  ls_dd03p-fieldname = 'MANDT'.
  ls_dd03p-keyflag = 'X'.
  ls_dd03p-rollname = 'MANDT'.
  ls_dd03p-position = l_position.
  APPEND ls_dd03p TO lt_dd03p.

  LOOP AT gt_fields ASSIGNING <ls_field> WHERE fieldname <> 'MANDT'.
    ADD 1 TO l_position.

    CLEAR ls_dd03p.
    ls_dd03p-tabname = p_tabnam.
    ls_dd03p-fieldname = <ls_field>-fieldname.
    ls_dd03p-ddlanguage = sy-langu.
    ls_dd03p-position = l_position.
    IF <ls_field>-rollname IS NOT INITIAL.
      ls_dd03p-rollname = <ls_field>-rollname.
    ELSE.
      ls_dd03p-datatype  = <ls_field>-datatype.
      ls_dd03p-leng      = <ls_field>-leng.
      ls_dd03p-decimals  = <ls_field>-decimals.
      ls_dd03p-ddtext = <ls_field>-ddtext.
    ENDIF.
    APPEND ls_dd03p TO lt_dd03p.
  ENDLOOP.

  CALL FUNCTION 'DDIF_TABL_PUT'
    EXPORTING
      name              = ls_dd02v-tabname
      dd02v_wa          = ls_dd02v
      dd09l_wa          = ls_dd09v
    TABLES
      dd03p_tab         = lt_dd03p
    EXCEPTIONS
      tabl_not_found    = 1
      name_inconsistent = 2
      tabl_inconsistent = 3
      put_failure       = 4
      put_refused       = 56
      OTHERS            = 6.
  IF sy-subrc <> 0.
    show_error_message 'ʧ�ܣ�DDIF_DOMA_PUT����'.
  ENDIF.

  SET PARAMETER ID 'DTB' FIELD p_tabnam.  "����Parameter ID����������SE11��ʱ�򣬻��Զ���ʾ������

  CALL FUNCTION 'RS_TOOL_ACCESS'
    EXPORTING
      operation           = 'EDIT'
      object_name         = p_tabnam
      object_type         = 'TABL'
    EXCEPTIONS
      not_executed        = 1
      invalid_object_type = 2
      OTHERS              = 3.
ENDFORM.                    "create_tabl
*&---------------------------------------------------------------------*
*&      FORM  RS_CORR_INSERT
*&---------------------------------------------------------------------*
FORM rs_corr_insert USING    p_type
                             p_object TYPE ddenqs
                    CHANGING p_failed TYPE c
                             p_message TYPE string.
  CLEAR: p_failed, p_message.

  CALL FUNCTION 'RS_CORR_INSERT'
    EXPORTING
      object                   = p_object
      object_class             = p_type
      mode                     = 'I'
      global_lock              = 'X'
      korrnum                  = p_trkorr
      devclass                 = p_devc
*     OBJECT_CLASS_SUPPORTS_MA = ' '
    EXCEPTIONS
      cancelled                = 1
      permission_failure       = 2
      unknown_objectclass      = 3
      OTHERS                   = 4.
  IF sy-subrc <> 0.
    p_failed = 'X'.
    p_message = '����ʧ��'.
  ENDIF.
ENDFORM.                    "RS_CORR_INSERT
*&---------------------------------------------------------------------*
*&      Form  change_prefix
*&---------------------------------------------------------------------*
FORM change_prefix CHANGING p_value.
  DATA: l_prelen TYPE i,
        l_len TYPE i.

  IF p_preold = ''.
    CONCATENATE p_prefix p_value INTO p_value.
    EXIT.
  ENDIF.

  l_prelen = strlen( p_preold ).
  l_len = strlen( p_value ).
  CHECK l_len >= l_prelen.
  CONCATENATE p_prefix p_value+l_prelen INTO p_value.
ENDFORM.                    "change_prefix