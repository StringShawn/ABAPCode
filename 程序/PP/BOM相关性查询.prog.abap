*&---------------------------------------------------------------------*
*& Program ID    : ZMMR016
*& Program Text  : 物料颜色备注更新
*& Overview      : 物料颜色备注更新
*& Created by    : HANDYXH
*& Creation Date : 2019/08/05
*&---------------------------------------------------------------------*
*& Changing No   :
*& Updated by    :
*& Update date   :
*& Changed Item Description :
*&---------------------------------------------------------------------*
REPORT zmmr016.

TABLES:mara.

DATA:lt_stb      TYPE TABLE OF stpox.
DATA:lt_kntab    TYPE TABLE OF string.
DATA:lt_zmmt035  TYPE TABLE OF zmmt035.
DATA:ls_zmmt035  TYPE          zmmt035.
DATA:lv_atinn    TYPE cawnt-atinn.
DATA:lv_atwrt    TYPE zmmt035-atwrt.
DATA:lt_results  TYPE match_result_tab.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS s_matnr FOR mara-matnr.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) TEXT-002.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk1.

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

IF lt_stb IS NOT INITIAL.
  SELECT knobj,
         knnum
  INTO TABLE @DATA(lt_cuob)
  FROM cuob FOR ALL ENTRIES IN @lt_stb
  WHERE knobj = @lt_stb-knobj
    AND kntab = 'STPO'.
ENDIF.

LOOP AT lt_stb INTO DATA(ls_stb).
  READ TABLE lt_cuob INTO DATA(ls_cuob) WITH KEY knobj = ls_stb-knobj.
  IF sy-subrc = 0.
    CALL FUNCTION 'CUKD_GET_KNOWLEDGE'
      EXPORTING
        knowledge_type     = ''
        relation_nr        = ls_cuob-knnum
        date               = sy-datum
      TABLES
        knowledge_tab      = lt_kntab
      EXCEPTIONS
        no_knowledge_found = 1
        no_relation_found  = 2
        OTHERS             = 3.
    READ TABLE lt_kntab INTO DATA(ls_kntab) INDEX 1.
    IF sy-subrc = 0.
      CLEAR:lv_atinn,lv_atwrt,ls_zmmt035.
      FIND ALL OCCURRENCES OF REGEX '(DZ|BW|FA)\d' IN ls_kntab
      RESULTS lt_results.
      IF sy-subrc = 0.
        LOOP AT lt_results INTO DATA(ls_result).
          DATA(lv_match) = ls_kntab+ls_result-offset(ls_result-length).
          DATA(lv_atinn_i) = lv_match+0(2).
          CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
            EXPORTING
              input  = lv_atinn_i
            IMPORTING
              output = lv_atinn.
          SELECT SINGLE atwtb INTO lv_atwrt FROM cawnt
          WHERE atinn = lv_atinn
            AND atzhl = lv_match+2(1).
          IF sy-subrc = 0.
*            "存在一个组件有多个相关性
*            LOOP AT lt_zmmt035 INTO DATA(ls_zmmt035_temp) WHERE matnr = ls_stb-idnrk.
*              ls_zmmt035_temp-atwrt = lv_atwrt && '|' && ls_zmmt035_temp-atwrt.
*              MODIFY lt_zmmt035 FROM ls_zmmt035_temp.
*              CLEAR :ls_zmmt035_temp.
*            ENDLOOP.
            IF ls_zmmt035-atwrt IS NOT INITIAL.
              ls_zmmt035-atwrt = ls_zmmt035-atwrt && '|' && lv_atwrt.
            ELSE.
              ls_zmmt035-atwrt = lv_atwrt.
            ENDIF.
          ENDIF.
        ENDLOOP.
        ls_zmmt035-matnr = ls_stb-idnrk.
        INSERT ls_zmmt035 INTO TABLE lt_zmmt035.
        CLEAR ls_zmmt035.
      ENDIF.
    ENDIF.
  ENDIF.
ENDLOOP.

IF lt_zmmt035 IS NOT INITIAL.
  DELETE ADJACENT DUPLICATES FROM lt_zmmt035 COMPARING ALL FIELDS.
  MODIFY zmmt035 FROM TABLE lt_zmmt035.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT .
    MESSAGE '更新成功' TYPE 'S'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE '更新失败，请稍候再试' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ELSE.
  MESSAGE '没有要更新的数据' TYPE 'I'.
ENDIF.