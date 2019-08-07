*&---------------------------------------------------------------------*
*& Program ID    : ZMMR015
*& Program Text  : 物料短缺报表
*& Overview      : 物料短缺报表
*& Created by    : HANDYXH
*& Creation Date : 2019/08/05
*&---------------------------------------------------------------------*
*& Changing No   :
*& Updated by    :
*& Update date   :
*& Changed Item Description :
*&---------------------------------------------------------------------*
REPORT zmmr015 MESSAGE-ID zmm_mess.

*&---------------------------------------------------------------------*
*&数据定义模块
*&---------------------------------------------------------------------*
TABLES:mara.

TYPES:BEGIN OF ty_alv,
        index     TYPE i,                  "序号
        matnr     TYPE mara-matnr,         "零件号
        maktx     TYPE makt-maktx,         "零件名称
        menge     TYPE stpo-menge,         "单车用量
        zmline    TYPE zmmt036-zmline,     "主线体
        zsline    TYPE zmmt036-zsline,     "线体
        zlocation TYPE zmmt036-zlocation,  "工位
        zline_txt TYPE zmmt036-zline_txt,  "线体描述
        lifnr     TYPE lfa1-lifnr,         "供应商编码
        name1     TYPE lfa1-name1,         "供应商
        ekgrp     TYPE t024-ekgrp,         "采购组编码
        eknam     TYPE t024-eknam,         "PSM采购组
        matkl     TYPE t023t-matkl,        "物料组编码
        wgbez     TYPE t023t-wgbez,        "物料组描述
        labst     TYPE mard-labst,         "库存
        knobj     TYPE stpo-knobj,         "相关性对象号
        knnum     TYPE cuob-knnum,         "相关性内部号
        color     TYPE atwrt,              "颜色备注
        psmbz     TYPE char20,             "PSM备注
        urequ     TYPE stpo-menge,         "未清需求
        datum1    TYPE syst-datum,         "N+1日期
        trequ1    TYPE stpo-menge,         "N+1总需求
        crequ1    TYPE stpo-menge,         "N+1整车需求
        tlack1    TYPE stpo-menge,         "N+1总缺料
        clack1    TYPE stpo-menge,         "N+1整车缺料
        aplan1    TYPE char10,             "N+1到货计划
        trequ2    TYPE stpo-menge,         "N+2总需求
        datum2    TYPE syst-datum,         "N+2日期
        crequ2    TYPE stpo-menge,         "N+2整车需求
        tlack2    TYPE stpo-menge,         "N+2总缺料
        clack2    TYPE stpo-menge,         "N+2整车缺料
        aplan2    TYPE char10,             "N+2到货计划
        datum3    TYPE syst-datum,         "N+3日期
        trequ3    TYPE stpo-menge,         "N+3总需求
        crequ3    TYPE stpo-menge,         "N+3整车需求
        tlack3    TYPE stpo-menge,         "N+3总缺料
        clack3    TYPE stpo-menge,         "N+3整车缺料
        aplan3    TYPE char10,             "N+3到货计划
        datum4    TYPE syst-datum,         "N+4日期
        trequ4    TYPE stpo-menge,         "N+4总需求
        crequ4    TYPE stpo-menge,         "N+4整车需求
        tlack4    TYPE stpo-menge,         "N+4总缺料
        clack4    TYPE stpo-menge,         "N+4整车缺料
        aplan4    TYPE char10,             "N+4到货计划
        datum5    TYPE syst-datum,         "N+5日期
        trequ5    TYPE stpo-menge,         "N+5总需求
        crequ5    TYPE stpo-menge,         "N+5整车需求
        tlack5    TYPE stpo-menge,         "N+5总缺料
        clack5    TYPE stpo-menge,         "N+5整车缺料
        aplan5    TYPE char10,             "N+5到货计划
        datum6    TYPE syst-datum,         "N+6日期
        trequ6    TYPE stpo-menge,         "N+6总需求
        crequ6    TYPE stpo-menge,         "N+6整车需求
        tlack6    TYPE stpo-menge,         "N+6总缺料
        clack6    TYPE stpo-menge,         "N+6整车缺料
        aplan6    TYPE char10,             "N+6到货计划
        datum7    TYPE syst-datum,         "N+7日期
        trequ7    TYPE stpo-menge,         "N+7总需求
        crequ7    TYPE stpo-menge,         "N+7整车需求
        tlack7    TYPE stpo-menge,         "N+7总缺料
        clack7    TYPE stpo-menge,         "N+7整车缺料
        aplan7    TYPE char10,             "N+7到货计划
      END OF ty_alv.

DATA: gt_alv TYPE TABLE OF ty_alv,
      gs_alv TYPE ty_alv.

DATA: gt_fcat TYPE lvc_t_fcat,
      gs_fcat TYPE lvc_s_fcat,
      gs_layo TYPE lvc_s_layo.

DATA(lr_date) = NEW zcl_process_date( ).

CONSTANTS:c_werks     TYPE t001w-werks VALUE '1112',
          c_matkl_car TYPE mara-matkl  VALUE '1001'.   "整车物料组

RANGES:   gr_pedtr FOR plaf-pedtr,
          gr_matkl FOR mara-matkl,
          gr_lgort FOR mard-lgort.

DEFINE mcr_fieldcat.
  CLEAR gs_fcat.
  gs_fcat-fieldname  = &1.
  gs_fcat-scrtext_s  =
  gs_fcat-scrtext_l  =
  gs_fcat-scrtext_m  =
  gs_fcat-reptext    =
  gs_fcat-coltext    = &2.
  gs_fcat-edit       = &3.
  gs_fcat-checkbox   = &4.
  gs_fcat-key        = &5.
  gs_fcat-icon       = &6.
  gs_fcat-ref_table  = &7.
  gs_fcat-ref_field  = &8.
  gs_fcat-outputlen  = &9.
  APPEND gs_fcat TO gt_fcat.
END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*&选择屏幕模块
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
PARAMETERS     p_pedtr TYPE plaf-pedtr OBLIGATORY DEFAULT sy-datum.
SELECT-OPTIONS s_matnr FOR  mara-matnr.
SELECTION-SCREEN END OF BLOCK blk1.


*&---------------------------------------------------------------------*
*& START-OF-SELECTION.
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM build_coditons.
  PERFORM get_data.
  PERFORM display_data.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*& Form BUILD_CODITONS
*&---------------------------------------------------------------------*
*& build select conditions
*&---------------------------------------------------------------------*
FORM build_coditons .
  DATA:lv_span TYPE vtbbewe-atage.

  CLEAR:gr_pedtr,gr_pedtr[].

  SELECT SUM( zspan ) INTO @lv_span FROM zmmt034.
  IF sy-subrc NE 0.
    MESSAGE s001 WITH '线体时间跨度表为空，请先配置ZMMT034' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  "需要展示7天的数据 但是时间跨度第一天是最后一天 lv_span + 6 - 1
  ADD 6 TO lv_span.

  gr_pedtr[] = VALUE #( ( sign = 'I' option = 'BT' low  = lr_date->add_factory_days( i_date = p_pedtr i_days = '1')
                                                   high = lr_date->add_factory_days( i_date = p_pedtr i_days = lv_span ) ) ).


  gr_matkl[] = VALUE #( ( sign = 'I' option = 'EQ' low = '1002')
                        ( sign = 'I' option = 'EQ' low = '1003')
                        ( sign = 'I' option = 'EQ' low = '1004')
                        ( sign = 'I' option = 'EQ' low = '1005')
                        ( sign = 'I' option = 'EQ' low = '1009') ).

  gr_lgort[] = VALUE #( ( sign = 'I' option = 'EQ' low = '1201')
                        ( sign = 'I' option = 'EQ' low = '1202')
                        ( sign = 'I' option = 'EQ' low = '1203')
                        ( sign = 'I' option = 'EQ' low = '1209')
                        ( sign = 'I' option = 'EQ' low = '1301')
                        ( sign = 'I' option = 'EQ' low = '1302')
                        ( sign = 'I' option = 'EQ' low = '1303')
                        ( sign = 'I' option = 'EQ' low = '1401')
                        ( sign = 'I' option = 'EQ' low = '1402')
                        ( sign = 'I' option = 'EQ' low = '1403')
                        ( sign = 'I' option = 'EQ' low = '1409')
                        ( sign = 'I' option = 'EQ' low = '1501')
                        ( sign = 'I' option = 'EQ' low = '1601')
                        ( sign = 'I' option = 'EQ' low = '1602')
                        ( sign = 'I' option = 'EQ' low = '1603') ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& GET MAIN DATA
*&---------------------------------------------------------------------*
FORM get_data .

  TYPES:BEGIN OF ly_pedtr,
          idnrk TYPE resb-matnr,
          pedtr TYPE plaf-pedtr,
        END OF ly_pedtr.
  DATA:lt_pedtr TYPE TABLE OF ly_pedtr,
       ls_pedtr TYPE ly_pedtr.
  DATA:lt_stb TYPE TABLE OF stpox.
  DATA:lt_kntab TYPE TABLE OF string,
       ls_kntab TYPE string.


  "取需求量
  SELECT plaf~plnum,   "计划订单号
         plaf~rsnum,   "预留单号
         plaf~matnr,   "成品物料号
         plaf~pedtr,   "计划结束日期
         resb~matnr AS idnrk,   "组件物料号
         resb~bdmng,   "需求量
         mara~matkl
  INTO TABLE @DATA(lt_plaf)
  FROM plaf INNER JOIN resb
    ON plaf~rsnum = resb~rsnum
            INNER JOIN mara
    ON plaf~matnr = mara~matnr
  WHERE pedtr IN @gr_pedtr
    AND plwrk =  @c_werks.
  IF sy-subrc  <> 0.
    MESSAGE s032 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SORT lt_plaf BY pedtr ASCENDING.
  lt_pedtr = CORRESPONDING #( lt_plaf ).
  SORT lt_pedtr BY idnrk pedtr.
  DELETE ADJACENT DUPLICATES FROM lt_pedtr COMPARING ALL FIELDS.

  "取未清
  SELECT plaf~plnum,   "计划订单号
         plaf~rsnum,   "预留单号
         plaf~pedtr,   "计划结束日期
         resb~matnr,   "物料号
         resb~bdmng    "需求量
  INTO TABLE @DATA(lt_plaf_open)
  FROM plaf INNER JOIN resb
    ON plaf~rsnum = resb~rsnum
  WHERE pedtr <= @p_pedtr
    AND plwrk =  @c_werks.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      capid                 = 'PP01'           " BOM应用
      datuv                 = sy-datum         " 有效起始日
      ehndl                 = '1'
      emeng                 = '1'              " 需求数量
      mehrs                 = 'X'              " 多层展开
      mmory                 = '1'              " 是否使用缓存
      mtnrv                 = 'M01'            " 展开物料号
      stlan                 = '1'              " BOM用途
      werks                 = '1112'          " 物料所在工厂
    TABLES
      stb                   = lt_stb
    EXCEPTIONS
      alt_not_found         = 1
      call_invalid          = 2
      material_not_found    = 3
      missing_authorization = 4
      no_bom_found          = 5
      no_plant_data         = 6
      no_suitable_bom_found = 7
      conversion_error      = 8
      OTHERS                = 9.

  DELETE lt_stb WHERE idnrk NOT IN s_matnr.
  DELETE lt_stb WHERE matmk NOT IN gr_matkl.
  SORT lt_stb BY idnrk .
  CLEAR:gt_alv.
  LOOP AT lt_stb INTO DATA(ls_stb).
    gs_alv = VALUE #( matnr = ls_stb-idnrk
                      maktx = ls_stb-ojtxp
                      menge = ls_stb-mnglg
                      matkl = ls_stb-matmk ).
    COLLECT gs_alv INTO gt_alv.
    CLEAR:gs_alv.
  ENDLOOP.

  SORT gt_alv.
  DELETE ADJACENT DUPLICATES FROM gt_alv COMPARING ALL FIELDS.

  IF gt_alv IS NOT INITIAL.
    SELECT matnr,
           lifnr,
           quote
    INTO TABLE @DATA(lt_euqk)
    FROM equk INNER JOIN equp
      ON equk~qunum = equp~qunum
    FOR ALL ENTRIES IN @gt_alv
    WHERE equk~matnr = @gt_alv-matnr
      AND equk~werks = @c_werks
      AND equk~bdatu >= @sy-datum
      AND equk~vdatu <= @sy-datum.

    SORT lt_euqk BY quote DESCENDING.

    IF lt_euqk IS NOT INITIAL.
      "供应商名称
      SELECT lifnr,
             name1
      INTO TABLE @DATA(lt_lfa1)
      FROM lfa1 FOR ALL ENTRIES IN @lt_euqk
      WHERE lifnr = @lt_euqk-lifnr.
    ENDIF.

    SELECT matnr,
           ekgrp
    INTO TABLE @DATA(lt_marc)
    FROM marc FOR ALL ENTRIES IN @gt_alv
    WHERE matnr = @gt_alv-matnr
      AND werks = @c_werks.
    IF sy-subrc = 0.
      "采购组名称
      SELECT ekgrp,
             eknam
      INTO TABLE @DATA(lt_t024)
      FROM t024 FOR ALL ENTRIES IN @lt_marc
      WHERE ekgrp = @lt_marc-ekgrp.
    ENDIF.


    "物料组描述
    SELECT matkl,
           wgbez
    INTO TABLE @DATA(lt_t023t)
    FROM t023t FOR ALL ENTRIES IN @gt_alv
    WHERE matkl = @gt_alv-matkl.

    "线体描述
    SELECT zindex,
           zmline,
           zlocation,
           zsline,
           zline_txt
    INTO TABLE @DATA(lt_zmmt036)
    FROM zmmt036.

    "时间跨度
    SELECT *
    INTO TABLE @DATA(lt_zmmt034)
    FROM zmmt034.

    "颜色备注
    SELECT matnr,
           atwrt
    INTO TABLE @DATA(lt_zmmt035)
    FROM zmmt035 FOR ALL ENTRIES IN @gt_alv
    WHERE matnr = @gt_alv-matnr.

    "取非限制库存
    SELECT matnr,
           lgort,
           labst
    INTO TABLE @DATA(lt_mard)
    FROM mard FOR ALL ENTRIES IN @gt_alv
    WHERE matnr =  @gt_alv-matnr
      AND werks =  @c_werks
      AND lgort IN @gr_lgort.

    "取寄售库存
    SELECT matnr,
           lgort,
           slabs
    INTO TABLE @DATA(lt_mkol)
    FROM mkol FOR ALL ENTRIES IN @gt_alv
    WHERE matnr =  @gt_alv-matnr
      AND werks =  @c_werks
      AND lgort IN @gr_lgort
      AND sobkz = 'K'.

    "取供应商库存
    SELECT matnr,
           lblab
    INTO TABLE @DATA(lt_mslb)
    FROM mslb FOR ALL ENTRIES IN @gt_alv
    WHERE matnr = @gt_alv-matnr
      AND werks = @c_werks
      AND sobkz = 'O'.
  ENDIF.

  "处理时间跨度
  SORT lt_zmmt034 BY zindex DESCENDING.
  DATA:lv_span  TYPE zmmt034-zspan.
  LOOP AT lt_zmmt034 INTO DATA(ls_zmmt034).
    IF sy-tabix = 1.
      lv_span = ls_zmmt034-zspan - 1.
    ELSE.
      lv_span = ls_zmmt034-zspan + lv_span.
    ENDIF.
    ls_zmmt034-zspan = lv_span.
    MODIFY lt_zmmt034 FROM ls_zmmt034.
    CLEAR:ls_zmmt034.
  ENDLOOP.

  DATA:lv_fieldname TYPE string,
       lv_index     TYPE n LENGTH 1,
       lv_zindex    TYPE zmmt036-zindex,
       lv_days      TYPE vtbbewe-atage.
  FIELD-SYMBOLS <fs_field>.
  FIELD-SYMBOLS <fs_trequ> TYPE stpo-menge.
  FIELD-SYMBOLS <fs_crequ> TYPE stpo-menge.
  FIELD-SYMBOLS <fs_tlack> TYPE stpo-menge.
  FIELD-SYMBOLS <fs_clack> TYPE stpo-menge.

  LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).
    <fs_alv>-index = sy-tabix.
    CLEAR:lv_index.
    DO 7 TIMES.
      ADD 1 TO lv_index.
      lv_fieldname = 'DATUM' && lv_index.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_alv> TO <fs_field>.
      IF sy-subrc = 0.
        lv_days = lv_index.
        <fs_field> = lr_date->add_factory_days( i_date = p_pedtr i_days = lv_days ).
      ENDIF.
    ENDDO.

    CLEAR lv_zindex.
    LOOP AT lt_stb INTO ls_stb WHERE idnrk = <fs_alv>-matnr.


      READ TABLE lt_zmmt036 INTO DATA(ls_zmmt036) WITH KEY zlocation = ls_stb-potx2.
      IF sy-subrc = 0.
        IF lv_zindex = 0.
          lv_zindex = ls_zmmt036-zindex.
          <fs_alv>-zlocation = ls_stb-potx2.
        ENDIF.
        IF lv_zindex > ls_zmmt036-zindex.
          lv_zindex = ls_zmmt036-zindex.
          <fs_alv>-zlocation = ls_stb-potx2.
        ENDIF.
      ENDIF.
    ENDLOOP.

    READ TABLE lt_euqk INTO DATA(ls_equk) WITH KEY matnr = <fs_alv>-matnr.
    IF sy-subrc = 0.
      <fs_alv>-lifnr = ls_equk-lifnr.
      READ TABLE lt_lfa1 INTO DATA(ls_lfa1) WITH KEY lifnr = <fs_alv>-lifnr.
      IF sy-subrc = 0.
        <fs_alv>-name1 = ls_lfa1-name1.       "供应商名称
      ENDIF.
    ENDIF.

    READ TABLE lt_marc INTO DATA(ls_marc) WITH KEY matnr = <fs_alv>-matnr.
    IF sy-subrc = 0.
      <fs_alv>-ekgrp = ls_marc-ekgrp.
      READ TABLE lt_t024 INTO DATA(ls_t024) WITH KEY ekgrp = <fs_alv>-ekgrp.
      IF sy-subrc = 0.
        <fs_alv>-eknam = ls_t024-eknam.       "采购组名称
      ENDIF.
    ENDIF.


    READ TABLE lt_t023t INTO DATA(ls_t023t) WITH KEY matkl = <fs_alv>-matkl.
    IF sy-subrc = 0.
      <fs_alv>-wgbez = ls_t023t-wgbez.      "物料组描述
    ENDIF.

    READ TABLE lt_zmmt036 INTO ls_zmmt036 WITH KEY zlocation = <fs_alv>-zlocation.
    IF sy-subrc = 0.
      <fs_alv>-zsline    = ls_zmmt036-zsline.    "线体
      <fs_alv>-zmline    = ls_zmmt036-zmline.    "主线体
      <fs_alv>-zline_txt = ls_zmmt036-zline_txt. "线体描述
      READ TABLE lt_zmmt034 INTO ls_zmmt034 WITH KEY zmline = <fs_alv>-zmline.
      IF sy-subrc = 0.
        LOOP AT lt_pedtr INTO ls_pedtr WHERE idnrk = <fs_alv>-matnr.
          CLEAR lv_days.
          lv_days = ls_zmmt034-zspan.
          DATA(lv_pedtr) = lr_date->minus_factory_days( i_date = ls_pedtr-pedtr i_days = lv_days ).
          IF lv_pedtr IN gr_pedtr.
            CLEAR lv_index.
            DO 7 TIMES.
              ADD 1 TO lv_index.
              lv_fieldname = 'DATUM' && lv_index.
              ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_alv> TO <fs_field>.
              IF sy-subrc = 0 AND <fs_field> = lv_pedtr.
                lv_fieldname = 'TREQU' && lv_index.
                ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_alv> TO <fs_trequ>.
                lv_fieldname = 'CREQU' && lv_index.
                ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_alv> TO <fs_crequ>.
                LOOP AT lt_plaf INTO DATA(ls_plaf) WHERE idnrk = <fs_alv>-matnr AND pedtr = ls_pedtr-pedtr.
                  IF ls_plaf-matkl = c_matkl_car.
                    <fs_crequ> = <fs_crequ> + ls_plaf-bdmng.   "整车需求
                  ENDIF.
                  <fs_trequ> = <fs_trequ> + ls_plaf-bdmng.     "总需求
                  CLEAR:ls_plaf.
                ENDLOOP.
              ENDIF.
            ENDDO.
          ELSE.
            LOOP AT lt_plaf INTO ls_plaf WHERE idnrk = <fs_alv>-matnr AND pedtr = ls_pedtr-pedtr.
              <fs_alv>-urequ = ls_plaf-bdmng + <fs_alv>-urequ.      "未清需求
            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    READ TABLE lt_zmmt035 INTO DATA(ls_zmmt035) WITH KEY matnr = <fs_alv>-matnr.
    IF sy-subrc = 0.
      <fs_alv>-color = ls_zmmt035-atwrt.    "颜色备注
    ENDIF.

    LOOP AT lt_plaf_open INTO DATA(ls_plaf_open) WHERE matnr = <fs_alv>-matnr.
      <fs_alv>-urequ = ls_plaf_open-bdmng + <fs_alv>-urequ.      "未清需求
    ENDLOOP.

    LOOP AT lt_mard INTO DATA(ls_mard) WHERE matnr = <fs_alv>-matnr.
      <fs_alv>-labst = ls_mard-labst + <fs_alv>-labst.      "非限制库存
    ENDLOOP.

    LOOP AT lt_mkol INTO DATA(ls_mkol) WHERE matnr = <fs_alv>-matnr.
      <fs_alv>-labst = ls_mkol-slabs + <fs_alv>-labst.      "寄售库存
    ENDLOOP.

    LOOP AT lt_mslb INTO DATA(ls_mslb) WHERE matnr = <fs_alv>-matnr.
      <fs_alv>-labst = ls_mslb-lblab + <fs_alv>-labst.      "供应商库存
    ENDLOOP.

    CLEAR lv_index.

    DO 7 TIMES.
      ADD 1 TO lv_index.
      lv_fieldname = 'TREQU' && lv_index.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_alv> TO <fs_trequ>.
      lv_fieldname = 'TLACK' && lv_index.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_alv> TO <fs_tlack>.
      <fs_tlack> = <fs_trequ> - <fs_alv>-labst.   "总缺料
      IF <fs_tlack> < 0.
        <fs_tlack> = 0.
      ENDIF.
      lv_fieldname = 'CREQU' && lv_index.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_alv> TO <fs_crequ>.
      lv_fieldname = 'CLACK' && lv_index.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_alv> TO <fs_clack>.
      <fs_clack> = <fs_crequ> - <fs_alv>-labst.   "整车缺料
      IF <fs_clack> < 0.
        <fs_clack> = 0.
      ENDIF.
    ENDDO.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM display_data .

  PERFORM set_layout.
  PERFORM set_fieldcat.
  PERFORM display_alv.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_layout .
  CLEAR:gs_layo.
  gs_layo-cwidth_opt = 'X'."列优化
  gs_layo-zebra      = 'X'."斑马线
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELDCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_fieldcat .
  CLEAR:gt_fcat.
  mcr_fieldcat 'INDEX' '序号' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MATNR' '零件号' '' '' '' '' 'MARA' 'MATNR' ''.
  mcr_fieldcat 'MAKTX' '零件名称' '' '' '' '' '' '' ''.
  mcr_fieldcat 'MENGE' '单车用量' '' '' '' '' '' '' ''.
  mcr_fieldcat 'ZLINE_TXT' '线体' '' '' '' '' '' '' ''.
  mcr_fieldcat 'NAME1' '供应商' '' '' '' '' '' '' ''.
  mcr_fieldcat 'EKNAM' 'PSM采购组' '' '' '' '' '' '' ''.
  mcr_fieldcat 'WGBEZ' '物料组描述' '' '' '' '' '' '' ''.
  mcr_fieldcat 'LABST' '库存' '' '' '' '' '' '' ''.
  mcr_fieldcat 'COLOR' '颜色备注' '' '' '' '' '' '' ''.
  mcr_fieldcat 'PSMBZ' 'PSM备注' '' '' '' '' '' '' ''.
  mcr_fieldcat 'UREQU' '未清需求' '' '' '' '' '' '' ''.
  mcr_fieldcat 'TREQU1' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'CREQU1' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'TLACK1' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'CLACK1' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'APLAN1' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'TREQU2' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'CREQU2' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'TLACK2' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'CLACK2' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'APLAN2' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'TREQU3' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'CREQU3' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'TLACK3' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'CLACK3' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'APLAN3' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'TREQU4' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'CREQU4' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'TLACK4' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'CLACK4' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'APLAN4' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'TREQU5' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'CREQU5' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'TLACK5' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'CLACK5' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'APLAN5' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'TREQU6' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'CREQU6' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'TLACK6' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'CLACK6' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'APLAN6' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'TREQU7' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'CREQU7' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'TLACK7' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'CLACK7' 'N' '' '' '' '' '' '' ''.
  mcr_fieldcat 'APLAN7' 'N' '' '' '' '' '' '' ''.

  DATA:lv_days TYPE vtbbewe-atage.
  DATA:lv_date LIKE sy-datum.
  CLEAR:lv_days.
  LOOP AT gt_fcat INTO gs_fcat WHERE coltext = 'N'.
    lv_days = gs_fcat-fieldname+5(1).
    lv_date = lr_date->add_factory_days( i_date = p_pedtr i_days = lv_days ).
    CASE gs_fcat-fieldname+0(5).
      WHEN 'TREQU'.
        gs_fcat-coltext = lv_date && '总需求'.
      WHEN 'CREQU'.
        gs_fcat-coltext = lv_date && '整车需求'.
      WHEN 'TLACK'.
        gs_fcat-coltext = lv_date && '总缺料'.
      WHEN 'CLACK'.
        gs_fcat-coltext = lv_date && '整车缺料'.
      WHEN 'APLAN'.
        gs_fcat-coltext = lv_date && '到货计划'.
      WHEN OTHERS.
    ENDCASE.
    gs_fcat-reptext = gs_fcat-scrtext_l = gs_fcat-scrtext_s = gs_fcat-scrtext_m = gs_fcat-coltext.
    MODIFY gt_fcat FROM gs_fcat.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*、
FORM display_alv .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat_lvc    = gt_fcat
      is_layout_lvc      = gs_layo
      i_save             = 'A'
    TABLES
      t_outtab           = gt_alv
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
  ENDIF.
ENDFORM.