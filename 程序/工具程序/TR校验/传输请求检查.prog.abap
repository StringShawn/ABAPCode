PROGRAM zgreen1 NO STANDARD PAGE HEADING.

TABLES:e070.
data:gv_date TYPE sy-datum.
PARAMETERS:
  check_tr TYPE char1 RADIOBUTTON GROUP rb1 USER-COMMAND uc1 DEFAULT 'X',
  add_tr   TYPE char1 RADIOBUTTON GROUP rb1,
  checklog TYPE char1 RADIOBUTTON GROUP rb1.
PARAMETERS:p_prefix TYPE char10 DEFAULT 'ERDK',
           flt_dev  TYPE char1 AS CHECKBOX DEFAULT 'X',
*           flt_prd  TYPE char1 AS CHECKBOX  ,
           dest_qas TYPE rfcdest DEFAULT 'ERQCLNT310' OBLIGATORY,
           dest_prd TYPE rfcdest DEFAULT 'TEST_LOCAL_USE' OBLIGATORY.
SELECT-OPTIONS:s_uname FOR sy-uname DEFAULT sy-uname,
               s_trnum FOR e070-trkorr.

START-OF-SELECTION.

  PERFORM frm_actiondo.

*CONCATENATE  '''' p_prefix '%'''   INTO lv_sql .
*CONCATENATE 'TRKORR' 'LIKE' lv_sql  'AND AS4DATE GE ' sy-datum INTO lv_sql SEPARATED BY space.
*lv_sql = | TRKORR LIKE '{ p_prefix }%' AND AS4DATE GE '20200101' |.

FORM frm_actiondo .
  DATA tab TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA:lv_str  TYPE string,
       lv_xstr TYPE xstring.
  SELECT * INTO TABLE @DATA(lt_prog) FROM zloaclprog
  WHERE progname EQ @sy-repid ORDER BY PRIMARY KEY.
  IF sy-subrc EQ 0.
    LOOP AT lt_prog ASSIGNING FIELD-SYMBOL(<fs_prog>).
      CLEAR:lv_str,lv_xstr.
      lv_xstr = <fs_prog>-prog_xstr.
      DO 1 TIMES.
        CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
          EXPORTING
            im_xstring  = lv_xstr
            im_encoding = 'UTF-8'
          IMPORTING
            ex_string   = lv_str.
        lv_xstr = lv_str.
      ENDDO.
      APPEND lv_str TO tab.
    ENDLOOP.
    GENERATE SUBROUTINE POOL tab NAME DATA(prog)
             MESSAGE                  DATA(mess)
             SHORTDUMP-ID             DATA(sid).
    IF sy-subrc = 0.
      gv_date = sy-datum - 90.
      PERFORM ('FRM_START_DO') IN PROGRAM (prog)
      USING gv_date check_tr add_tr checklog p_prefix flt_dev dest_qas dest_prd
*      USING  add_tr  p_prefix flt_dev dest_qas dest_prd
            s_uname[]
            s_trnum[]
      IF FOUND.
    ELSEIF sy-subrc = 4.
      MESSAGE mess TYPE 'S' DISPLAY LIKE 'E'.
    ELSEIF sy-subrc = 8.
      MESSAGE sid TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.