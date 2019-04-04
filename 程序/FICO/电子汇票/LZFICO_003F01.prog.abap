*----------------------------------------------------------------------*
***INCLUDE LZFICO_003F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_INPUT  text
*      <--P_ERR_FLAG  text
*      <--P_GV_MSG  text
*----------------------------------------------------------------------*
FORM frm_check_input  USING    p_input STRUCTURE zsdianzihpi
                      CHANGING p_err p_msg.

  DATA:lt_fieldcat TYPE lvc_t_fcat,
       ls_fieldcat TYPE lvc_s_fcat.
  FIELD-SYMBOLS <fs_field> TYPE any.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSDIANZIHPI'
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_fieldcat INTO ls_fieldcat.
    ASSIGN COMPONENT ls_fieldcat-fieldname OF STRUCTURE p_input TO <fs_field>.
    IF <fs_field> IS ASSIGNED.
      IF <fs_field> IS INITIAL.
        p_err = 'E'.
        CONCATENATE '字段' ls_fieldcat-reptext '不能为空！' INTO p_msg.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CHECK p_err IS INITIAL.

*  DATA:lv_zuonr TYPE bseg-zuonr.
  TYPES:BEGIN OF ly_belnr,
        bukrs TYPE bkpf-bukrs,
        belnr TYPE bkpf-belnr,
        cpudt TYPE bkpf-cpudt,
        END OF ly_belnr.
  DATA:lt_belnr TYPE TABLE OF ly_belnr,
       ls_belnr TYPE ly_belnr.

  DATA:lt_bkpf TYPE TABLE OF bkpf.

  SELECT bukrs belnr cpudt INTO TABLE lt_belnr FROM zzt_dianzihp_log WHERE zuonr = p_input-zuonr AND belnr NE ''.

  IF sy-subrc = 0.
    SELECT * INTO TABLE lt_bkpf FROM bkpf FOR ALL ENTRIES IN lt_belnr WHERE belnr = lt_belnr-belnr
                                                                        AND gjahr = lt_belnr-cpudt+0(4)
                                                                        AND bukrs = lt_belnr-bukrs
                                                                        AND stblg = ''.
    IF sy-subrc = 0.
      p_err = 'E'.
      p_msg = '该汇票已经传输过！'.
    ENDIF.
  ENDIF.
ENDFORM.                    " FRM_CHECK_INPUT
*&---------------------------------------------------------------------*
*&      Form  FRM_ACC_DOCUMENT_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_INPUT  text
*----------------------------------------------------------------------*
FORM frm_acc_document_post  TABLES   p_input STRUCTURE zsdianzihpi
                                     p_log STRUCTURE zzt_dianzihp_log
                             USING  p_flag  TYPE c.
  DATA:  i_tabix       LIKE sy-tabix,
         i_shkzg       TYPE tbsl-shkzg,
         i_koart       TYPE tbsl-koart,
         wa_l_head     TYPE zfi_f_02_head,
         wa_l_item     TYPE zfico_003_bdcs,
         it_l_item     TYPE TABLE OF zfico_003_bdcs WITH HEADER LINE,
         lv_dmbtr      TYPE bseg-dmbtr,
         ls_input      TYPE zsdianzihpi,
         i_err TYPE c,
         i_msg TYPE c LENGTH 500,
         i_message TYPE c LENGTH 500,
         i_mode TYPE apqi-putactive VALUE 'N',
         i_subrc TYPE syst-subrc,
         it_l_mes TYPE TABLE OF bdcmsgcoll,
         wa_l_mes LIKE LINE OF it_l_mes,
         i_msgtxt(200).
  DATA:p_test.
  DATA:ls_log TYPE zzt_dianzihp_log.
  DATA:lv_belnr TYPE bkpf-belnr.

  DATA:lv_length TYPE i,
       lv_begin  TYPE i.

*    i_mode = p_mode.

  "bdc抬头信息
  "凭证头信息
  CLEAR p_input.
  READ TABLE p_input INDEX 1.

  CLEAR wa_l_head.
  wa_l_head-bldat = sy-datum.       "凭证日期
  wa_l_head-blart = 'AP'.           "凭证类型
  wa_l_head-bukrs = p_input-bukrs.  "公司代码
  wa_l_head-budat = sy-datum.       "过账日期
  wa_l_head-waers = 'RMB'.          "货币
  wa_l_head-monat = sy-datum+4(2).  "期间
  REFRESH it_l_item.
  CLEAR i_tabix.
  "凭证行信息
  LOOP AT p_input.
    i_tabix = i_tabix + 1.
    CLEAR:lv_length,lv_begin.
    lv_length = strlen( p_input-zuonr ).
    lv_begin = lv_length - 10.
    "准备BDC行项目信息
    CLEAR wa_l_item.
    wa_l_item-newbs = '39'.         "记账码
    wa_l_item-newum = 'D'.          "特别总账标识
    wa_l_item-newko = p_input-lifnr."科目
    wa_l_item-wrbtr = p_input-wrbtr."金额
    wa_l_item-wdate = sy-datum.     "签发日期
    wa_l_item-zfbdt = p_input-zfbdt."到期日
    wa_l_item-wname = p_input-butxt."出票人
    wa_l_item-wbzog = p_input-name1."受票人
    IF lv_length > 10.
      wa_l_item-zuonr = p_input-zuonr+lv_begin(10)."分配
    ELSE.
      wa_l_item-zuonr = p_input-zuonr.
    ENDIF.

    CASE p_input-bukrs.
      WHEN 'ZFSH'.
        wa_l_item-gsber = 'SH30'.       "业务范围
      WHEN 'ZFYT'.
        wa_l_item-gsber = 'YT30'.       "业务范围
      WHEN 'ZFWH'.
        wa_l_item-gsber = 'WH30'.       "业务范围
      WHEN 'ZFNJ' OR 'ZFSN'.
        wa_l_item-gsber = 'NJ60'.       "业务范围
      WHEN OTHERS.
    ENDCASE.

    CONCATENATE sy-datum+0(4) '年' sy-datum+4(2) '月开票' INTO wa_l_item-sgtxt.

***20141114hp_sjf
    CONDENSE: wa_l_item-newbs,
              wa_l_item-newum,
              wa_l_item-newko,
              wa_l_item-wrbtr,
              wa_l_item-gsber,
              wa_l_item-zuonr.
    APPEND wa_l_item TO it_l_item.
    lv_dmbtr = lv_dmbtr + wa_l_item-wrbtr.
    AT END OF lifnr.
      CLEAR ls_input.
      READ TABLE p_input INTO ls_input INDEX i_tabix.

      CLEAR wa_l_item.
      wa_l_item-newbs = '21'.         "记账码
      wa_l_item-newko = ls_input-lifnr."科目
      wa_l_item-wrbtr = lv_dmbtr."金额
      wa_l_item-zfbdt = sy-datum."到期日
      wa_l_item-wdate = sy-datum.     "签发日期
      wa_l_item-wname = ls_input-butxt."出票人
      wa_l_item-wbzog = ls_input-name1."受票人
      wa_l_item-zuonr = ls_input-zuonr."分配
      CASE ls_input-bukrs.
        WHEN 'ZFSH'.
          wa_l_item-gsber = 'SH30'.       "业务范围
        WHEN 'ZFYT'.
          wa_l_item-gsber = 'YT30'.       "业务范围
        WHEN 'ZFWH'.
          wa_l_item-gsber = 'WH30'.       "业务范围
        WHEN 'ZFNJ' OR 'ZFSN'.
          wa_l_item-gsber = 'NJ60'.       "业务范围
        WHEN OTHERS.
      ENDCASE.
*      wa_l_item-gsber = 'SH30'.       "业务范围
      wa_l_item-rstgr = '113'.        "原因代码
      CONCATENATE sy-datum+0(4) '年' sy-datum+4(2) '月开票' INTO wa_l_item-sgtxt.
      CONDENSE: wa_l_item-newbs,
          wa_l_item-newum,
          wa_l_item-newko,
          wa_l_item-wrbtr,
          wa_l_item-gsber,
          wa_l_item-zuonr,
          wa_l_item-rstgr.
      CLEAR lv_dmbtr.
      APPEND wa_l_item TO it_l_item.
    ENDAT.
  ENDLOOP.



  "F-02创建 凭证
  CLEAR:i_subrc.
  REFRESH:it_l_mes.
  CALL FUNCTION 'ZFICO_003_BDC'
    EXPORTING
      im_ctu     = 'X'
      im_mode    = i_mode
      im_head    = wa_l_head
      im_update  = 'S'
*     im_1txt    = ''
    IMPORTING
      ex_subrc   = i_subrc
    TABLES
      ts_messtab = it_l_mes
      ts_t_item  = it_l_item.

  CLEAR: i_err,
         i_msg.
  LOOP AT it_l_mes INTO wa_l_mes
    WHERE msgtyp = 'E'
       OR msgtyp = 'A'.
    CLEAR:i_message.
    MESSAGE ID wa_l_mes-msgid TYPE wa_l_mes-msgtyp
            NUMBER wa_l_mes-msgnr WITH wa_l_mes-msgv1
                                     wa_l_mes-msgv2
                                     wa_l_mes-msgv3
                                     wa_l_mes-msgv4
            INTO i_message.
    IF i_msg IS INITIAL.
      i_msg = i_message.
    ELSE.
      CONCATENATE i_msg i_message INTO i_msg SEPARATED BY '|'.
    ENDIF.
  ENDLOOP.
  IF sy-subrc = 0.
    p_flag = 'X'.
    i_err = 'X'.
    CONCATENATE  '会计凭证创建失败：' i_msg
           INTO i_msg SEPARATED BY space.
  ELSE.
    READ TABLE it_l_mes INTO wa_l_mes
      WITH KEY msgid = 'F5'
               msgnr = '312'.
    IF sy-subrc = 0.
      CONCATENATE  '会计凭证' '(' wa_l_mes-msgv1 ')' '创建成功！'
             INTO i_msg SEPARATED BY space.
      COMMIT WORK AND WAIT.
      lv_belnr = wa_l_mes-msgv1.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_belnr
        IMPORTING
          output = lv_belnr.
      DO .
        SELECT SINGLE belnr FROM bkpf INTO lv_belnr WHERE belnr = lv_belnr AND bukrs = wa_l_head-bukrs AND gjahr = wa_l_head-budat+0(4).
        IF sy-subrc = 0.
          EXIT.
        ENDIF.
      ENDDO.
    ELSE.
      i_err = 'X'.
      i_msg = '会计凭证创建失败'.
    ENDIF.

  ENDIF.

  "更新
  LOOP AT p_input.
    CLEAR ls_log.
    MOVE-CORRESPONDING p_input TO ls_log.
    IF i_err = 'X'.
      ls_log-ztype = 'E'.
      ls_log-zmsg = i_msg.
    ELSE.
      ls_log-zmsg = i_msg.
      ls_log-belnr = wa_l_mes-msgv1 .
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_log-belnr
        IMPORTING
          output = ls_log-belnr.
      ls_log-ztype = 'S'.
    ENDIF.
    APPEND ls_log TO p_log.
  ENDLOOP.
ENDFORM.                    " FRM_ACC_DOCUMENT_POST
*&---------------------------------------------------------------------*
*&      Form  FRM_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_INPUT  text
*----------------------------------------------------------------------*
FORM frm_clear  TABLES    p_input STRUCTURE zsdianzihpi
                          p_log STRUCTURE zzt_dianzihp_log.
  TYPES:BEGIN OF ly_bsik.
          INCLUDE STRUCTURE bsik.
  TYPES:netdt TYPE netdt,
        END OF ly_bsik.

  TYPES:BEGIN OF    ly_clear,
        bukrs TYPE bsik-bukrs,
        lifnr TYPE bsik-lifnr,
        shkzg TYPE bsik-shkzg,
        xnegp TYPE bsik-xnegp,
        budat TYPE bsik-budat,
        wrbtr TYPE bsik-wrbtr,
        belnr TYPE bsik-belnr,
        buzei TYPE bsik-buzei,
        gjahr TYPE bsik-gjahr,
        blart TYPE bsik-blart,
        sgtxt TYPE bsik-sgtxt,
        END OF ly_clear.

  TYPES:BEGIN OF ly_header_bean,
         blart TYPE bkpf-blart,
         gjahr TYPE bkpf-gjahr,
         sgtxt TYPE bseg-sgtxt,
         END OF ly_header_bean.
  TYPES:BEGIN OF ly_index,
         index TYPE sy-tabix,
         shengyu TYPE c,
        END OF ly_index.

  TYPES:BEGIN OF ly_kunnr,
        bukrs TYPE lfb1-bukrs,
        lifnr TYPE lfa1-lifnr,
        kunnr TYPE kna1-kunnr,
        END OF ly_kunnr.

  TYPES:BEGIN OF ly_input_tmp.
          INCLUDE TYPE zsdianzihpi.
  TYPES:kunnr TYPE kna1-kunnr,
  END OF ly_input_tmp.

  DATA:lt_bsik TYPE TABLE OF ly_bsik,       "存放满足条件的未清项
       ls_bsik TYPE ly_bsik.
  DATA:lt_bsik_all TYPE TABLE OF ly_bsik,   "存放所有的未清项
       lt_bsik_all_tmp TYPE TABLE OF ly_bsik,   "存放所有的未清项
       ls_bsik_all TYPE ly_bsik.
  DATA:lt_input_tmp TYPE TABLE OF ly_input_tmp,
       ls_input_tmp TYPE ly_input_tmp.
  DATA:lt_kunnr TYPE TABLE OF ly_kunnr,
       ls_kunnr TYPE ly_kunnr.
  DATA:lt_clear TYPE TABLE OF ly_clear,
       lt_clear_h TYPE TABLE OF ly_clear,
       ls_clear TYPE ly_clear,
       ls_clear_temp TYPE ly_clear.
  DATA:ls_header TYPE zsfico_003_bdc2h.
  DATA:ls_header_bean TYPE ly_header_bean.
  DATA:ls_faede_i TYPE faede.
  DATA:ls_faede_o TYPE faede.
  DATA:lt_item TYPE TABLE OF zsfico_003_bdc2i,
       ls_item TYPE zsfico_003_bdc2i.
  DATA:lt_index TYPE TABLE OF ly_index,
       ls_index TYPE ly_index.
  DATA:lv_length TYPE i,
       lv_begin  TYPE i.
  DATA:lv_kunnr TYPE lfa1-kunnr.
  DATA:lv_tabix TYPE sy-tabix.


  DATA:  i_mode TYPE apqi-putactive VALUE 'N',
         i_subrc TYPE syst-subrc,
         it_l_mes TYPE TABLE OF bdcmsgcoll,
         wa_l_mes LIKE LINE OF it_l_mes,
         i_err TYPE c,
         i_msg TYPE c LENGTH 500,
         i_message TYPE c LENGTH 500.
  FIELD-SYMBOLS <fs_bsik> TYPE ly_bsik.
  DATA:lv_last_day TYPE d,
       lv_dmbtr_s TYPE bsik-wrbtr,
       lv_dmbtr_h TYPE bsik-wrbtr.

  CLEAR lt_bsik.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_bsik
  FROM bsik FOR ALL ENTRIES IN p_input[]
  WHERE bukrs = p_input-bukrs
    AND lifnr = p_input-lifnr
    AND waers = 'RMB'
    AND umskz = ''
    AND zterm NE 'Z000'
    AND rstgr NOT LIKE '2%'.

  CLEAR:lt_bsik_all,lt_input_tmp.
  LOOP AT p_input.
    MOVE-CORRESPONDING p_input TO ls_input_tmp.
    APPEND ls_input_tmp TO lt_input_tmp.
    CLEAR:p_input,ls_input_tmp.
  ENDLOOP.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_bsik_all
FROM bsik FOR ALL ENTRIES IN p_input[]
WHERE bukrs = p_input-bukrs
  AND lifnr = p_input-lifnr
  AND umskz = ''.

  IF p_input[] IS NOT INITIAL.
    SELECT bukrs kunnr lfa1~lifnr INTO CORRESPONDING FIELDS OF TABLE lt_kunnr FROM lfa1 INNER JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
      FOR ALL ENTRIES IN p_input WHERE lfa1~lifnr = p_input-lifnr AND bukrs = p_input-bukrs AND XVERR = 'X'.
  ENDIF.

  LOOP AT lt_input_tmp INTO ls_input_tmp.
    READ TABLE lt_kunnr INTO ls_kunnr WITH KEY lifnr = ls_input_tmp-lifnr bukrs = ls_input_tmp-bukrs.
    IF sy-subrc = 0.
      ls_input_tmp-kunnr = ls_kunnr-kunnr.
    ENDIF.
    MODIFY lt_input_tmp FROM ls_input_tmp.
    CLEAR:ls_kunnr,ls_input_tmp.
  ENDLOOP.

  SELECT bukrs
         kunnr AS lifnr
         shkzg
         xnegp
         budat
         dmbtr
         belnr
         buzei
         gjahr
         blart
         sgtxt
     APPENDING CORRESPONDING FIELDS OF TABLE lt_bsik_all
  FROM bsid FOR ALL ENTRIES IN lt_input_tmp
  WHERE bukrs = lt_input_tmp-bukrs
    AND kunnr = lt_input_tmp-kunnr
    AND umskz = ''.

  CLEAR lt_input_tmp.

  "负数处理
  LOOP AT lt_bsik_all INTO ls_bsik_all.
    IF ls_bsik_all-shkzg = 'H'.
      ls_bsik_all-dmbtr = - ls_bsik_all-dmbtr.
    ENDIF.
    MODIFY lt_bsik_all FROM ls_bsik_all TRANSPORTING dmbtr.
  ENDLOOP.


  CLEAR:lv_last_day.
  CALL FUNCTION 'FKK_GET_LAST_DAY_OF_MONTH'
    EXPORTING
      i_date_in  = sy-datum
    IMPORTING
      e_date_out = lv_last_day.

  LOOP AT lt_bsik ASSIGNING <fs_bsik>.
    CLEAR: ls_faede_i,ls_faede_o.
    MOVE-CORRESPONDING <fs_bsik> TO ls_faede_i.
    ls_faede_i-koart = 'K'.   "科目类型
    CALL FUNCTION 'DETERMINE_DUE_DATE'
      EXPORTING
        i_faede                    = ls_faede_i
      IMPORTING
        e_faede                    = ls_faede_o
      EXCEPTIONS
        account_type_not_supported = 1
        OTHERS                     = 2.
    <fs_bsik>-netdt = ls_faede_o-netdt.
    IF <fs_bsik>-netdt > lv_last_day.
      DELETE lt_bsik.
    ELSE.
      MOVE-CORRESPONDING <fs_bsik> TO ls_clear.
      APPEND ls_clear TO lt_clear.
      CLEAR ls_clear.
    ENDIF.

  ENDLOOP.

  SORT lt_clear BY bukrs lifnr gjahr belnr buzei ASCENDING shkzg DESCENDING wrbtr DESCENDING.
  SORT lt_bsik_all BY bukrs lifnr gjahr belnr buzei dmbtr DESCENDING.

  CLEAR lv_tabix.
  LOOP AT lt_clear INTO ls_clear.
    ADD 1 TO lv_tabix.
    AT NEW lifnr.
      CLEAR lt_bsik_all_tmp.
      lt_bsik_all_tmp = lt_bsik_all.
      READ TABLE lt_clear INTO ls_clear_temp INDEX lv_tabix.
      DELETE lt_bsik_all_tmp WHERE bukrs NE ls_clear_temp-bukrs OR lifnr NE ls_clear_temp-lifnr.
    ENDAT.
    CASE ls_clear-shkzg.
      WHEN 'S'.
        lv_dmbtr_s = lv_dmbtr_s + ls_clear-wrbtr.
        READ TABLE lt_bsik_all_tmp TRANSPORTING NO FIELDS WITH KEY bukrs = ls_clear-bukrs
                                                               belnr = ls_clear-belnr
                                                               gjahr = ls_clear-gjahr
                                                               buzei = ls_clear-buzei.
        IF sy-subrc = 0.
          ls_index-index = sy-tabix.
          APPEND ls_index TO lt_index.
        ENDIF.
      WHEN 'H'.
        APPEND ls_clear TO lt_clear_h.
        lv_dmbtr_h = lv_dmbtr_h + ls_clear-wrbtr.
    ENDCASE.


    AT END OF lifnr.
      IF lv_dmbtr_h > 0.     "贷方总金额大于0
        IF lv_dmbtr_s <= lv_dmbtr_h.
          CLEAR lv_dmbtr_h.
          SORT lt_clear_h BY wrbtr ASCENDING.
          LOOP AT lt_clear_h INTO ls_clear.
            lv_dmbtr_h = ls_clear-wrbtr + lv_dmbtr_h.
            READ TABLE lt_bsik_all_tmp TRANSPORTING NO FIELDS WITH KEY bukrs = ls_clear-bukrs
                                                                   belnr = ls_clear-belnr
                                                                   gjahr = ls_clear-gjahr
                                                                   buzei = ls_clear-buzei.
            IF sy-subrc = 0.
              ls_index-index = sy-tabix.
              IF lv_dmbtr_h >= lv_dmbtr_s.
                ls_index-shengyu = 'X'.
              ENDIF.
              APPEND ls_index TO lt_index.
              CLEAR ls_index.
            ENDIF.
*            ADD 1 TO ls_header-shkzg_h.
            IF lv_dmbtr_h >= lv_dmbtr_s.
              EXIT.
            ENDIF.
          ENDLOOP.
          SORT lt_index ASCENDING.
          CLEAR : lt_item.
          LOOP AT lt_index INTO ls_index.
            ls_item-index = ls_index-index.
            ls_item-shengyu = ls_index-shengyu.
            CONDENSE ls_item-index.
            APPEND ls_item TO lt_item.
            CLEAR:ls_item,ls_index.
          ENDLOOP.
          ls_header-blart = ls_clear-blart.
          READ TABLE lt_clear INTO ls_clear_temp INDEX lv_tabix.
          ls_header-bukrs = ls_clear_temp-bukrs.
          ls_header-lifnr = ls_clear_temp-lifnr.
          ls_header-bldat = sy-datum.
          ls_header-budat = sy-datum.
          ls_header-waers = 'RMB'.
          ls_header-dmbtr =  lv_dmbtr_s - lv_dmbtr_h.
          CONDENSE: ls_header-dmbtr,ls_header-shkzg_s,ls_header-shkzg_h.
          CLEAR:lv_length,lv_begin.
          lv_length = strlen( ls_clear-sgtxt ).
          IF ls_clear-sgtxt IS INITIAL.
            CONCATENATE ' =' sy-datum+2(4) '票据清账剩余' INTO ls_header-sgtxt.
          ELSE.
            SEARCH ls_clear-sgtxt FOR   '票据清账剩余'  .
            IF sy-subrc = 0.
              lv_begin = sy-fdpos - 4.
              IF lv_begin < 0.
                CONCATENATE '=' sy-datum+2(4) '票据清账剩余' INTO ls_header-sgtxt.
              ENDIF.
              CONCATENATE ls_clear-sgtxt+0(lv_begin) sy-datum+2(4) '票据清账剩余' INTO ls_header-sgtxt.
            ELSEIF lv_length > 39.
              lv_begin = lv_length - 30.
              CONCATENATE ls_clear-sgtxt+lv_begin(30) '=' sy-datum+2(4) '票据清账剩余' INTO ls_header-sgtxt.
            ELSE.
              CONCATENATE ls_clear-sgtxt '=' sy-datum+2(4) '票据清账剩余' INTO ls_header-sgtxt.
            ENDIF.
          ENDIF.
          ls_header-rstgr = '113'.
          CONCATENATE ls_clear-belnr ls_clear-gjahr INTO ls_header-zuonr SEPARATED BY '/'.
          IF lv_dmbtr_h = lv_dmbtr_s.   "如果贷方金额和借方金额相等，不填充原因代码 文本 和分配
            CLEAR:ls_header-zuonr,ls_header-rstgr,ls_header-sgtxt.
            CONCATENATE sy-datum+2(4) '票据清账全清' INTO ls_header-sgtxt.
          ENDIF.
          CALL FUNCTION 'ZFICO_003_BDC2'
            EXPORTING
              im_mode    = i_mode
              im_head    = ls_header
            IMPORTING
              ex_subrc   = i_subrc
            TABLES
              ts_messtab = it_l_mes
              ts_item    = lt_item.
          LOOP AT it_l_mes INTO wa_l_mes WHERE msgtyp = 'E'
                                             OR msgtyp = 'A'.
            CLEAR:i_message.
            MESSAGE ID wa_l_mes-msgid TYPE wa_l_mes-msgtyp
                    NUMBER wa_l_mes-msgnr WITH wa_l_mes-msgv1
                                             wa_l_mes-msgv2
                                             wa_l_mes-msgv3
                                             wa_l_mes-msgv4
                    INTO i_message.
            IF i_msg IS INITIAL.
              i_msg = i_message.
            ELSE.
              CONCATENATE i_msg i_message INTO i_msg SEPARATED BY '|'.
            ENDIF.
          ENDLOOP.
          IF sy-subrc = 0.
            i_err = 'X'.
            CONCATENATE  '会计凭证创建失败：' i_msg
                   INTO i_msg SEPARATED BY space.
          ELSE.
            READ TABLE it_l_mes INTO wa_l_mes
              WITH KEY msgid = 'F5'
                       msgnr = '312'.
            IF sy-subrc = 0.
              CONCATENATE  '会计凭证' '(' wa_l_mes-msgv1 ')' '创建成功！'
                                 INTO i_msg SEPARATED BY space.
            ELSE.
              i_err = 'X'.
              CONCATENATE  '会计凭证创建失败：' i_msg INTO i_msg SEPARATED BY space.
            ENDIF.
          ENDIF.
        ELSE.
          i_err = 'X'.
          i_msg = '借方总金额大于贷方总金额，无法清帐'.
        ENDIF.

      ENDIF.
      LOOP AT p_log WHERE bukrs = ls_clear_temp-bukrs AND lifnr = ls_clear_temp-lifnr.
        IF i_err IS INITIAL .
          p_log-ztype = 'S'.
          p_log-augbl = wa_l_mes-msgv1.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = p_log-augbl
            IMPORTING
              output = p_log-augbl.
        ELSE.
          p_log-ztype = 'E'.
        ENDIF.

        p_log-augdt = sy-datum.

        CONCATENATE p_log-zmsg i_msg INTO p_log-zmsg SEPARATED BY '|'.
        MODIFY p_log.
        CLEAR:p_log.
      ENDLOOP.
      CLEAR:lv_dmbtr_s,ls_header,it_l_mes,lt_item,lt_index,lt_clear_h,i_msg,i_err,i_message,wa_l_mes,lv_dmbtr_h.
    ENDAT.
  ENDLOOP.



ENDFORM.                    " FRM_CLEAR