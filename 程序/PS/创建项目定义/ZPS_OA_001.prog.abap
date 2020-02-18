FUNCTION zfm_oa_013 .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_INPUT) TYPE  ZSPS_001_I OPTIONAL
*"  EXPORTING
*"     VALUE(E_OUTPUT) TYPE  ZSPS_001_O
*"----------------------------------------------------------------------
* 说明：
*      OA系统通过PO将项目信息传输到SAP进行项目编码生成
*     及WBS结构/网络活动创建
*     日志号码段：32*
*
*  2018/06/19 Create by SH-LYP
*
*"----------------------------------------------------------------------

** 数据对象声明
  DATA:wa_project_definition TYPE bapi_bus2001_new,
       lt_return             TYPE TABLE OF bapiret2,
       wa_return             TYPE bapiret2,

       lt_wbs_element        TYPE TABLE OF bapi_bus2054_new,
       wa_wbs_element        TYPE bapi_bus2054_new,

       wa_network            TYPE bapi_bus2002_new,

       lt_activity           TYPE TABLE OF bapi_bus2002_act_new,
       wa_activity           TYPE bapi_bus2002_act_new,

       lv_werks              TYPE werks,
       lv_objno              TYPE string,
       lv_cocode             TYPE char01,
       lv_seqno              TYPE zde_serno,
       lv_pspid              TYPE string,
       lv_pspid_edit         TYPE ps_pspid_edit,
       lv_prart              TYPE char1,
       lv_index              TYPE i,
       lv_plnnr              TYPE plnnr,

       wa_ztps_pro_resp      TYPE ztps_pro_resp,
       lt_ztps_pro_resp      TYPE TABLE OF ztps_pro_resp,

       lt_plpo               TYPE TABLE OF plpo,
       ls_plpo               TYPE plpo.

** 初始化
  CLEAR:wa_project_definition,
        wa_return,
        wa_network,
        wa_wbs_element,
        wa_activity,
        g_networkno,
        lv_werks,
        wa_ztps_pro_resp,
        lv_objno,
        lv_cocode,
        lv_seqno,
        lv_pspid,
        lv_pspid_edit,
        lv_prart,
        lv_index,
        g_output,
        ls_plpo,
        lv_plnnr.

  REFRESH:lt_return,
          lt_wbs_element,
          lt_activity,
          lt_ztps_pro_resp,
          lt_plpo.

** 项目编码生成 （项目类别 + 公司代码 + 部门/产品 + 立项年度 + 流水号）
* 项目类别 ：
*           X：新建项目； J：技改项目；Y：研发项目；C：测试项目

* 公司代码：
*          X：盛虹纤维有限公司  3200
*          G：国望高科          3100
*          Z: 中鲈科技          3300
*          H：港虹              3400
*          S：苏震              3900
* 部门/产品：
*         2位
*         新建/技改：部门
*         研发项目：部门/产品  部门：1.研发中心 2.研发部
*                   产品：0.PET 1.POY 2.FDY 3.DTY 4.其他

* 立项年度：
*        2位，2018->18

* 流水号：
*        3位，001-999   可以考虑创建一个No. Range

* 项目类型：
*          01.新建项目  02.技改项目  03.研发项目  04.测试项目


* 公司代码转换
  IF i_input-vbukr EQ '3100'.
    lv_cocode = 'G'.
  ELSEIF i_input-vbukr EQ '3200'.
    lv_cocode = 'X'.
  ELSEIF i_input-vbukr EQ '3300'.
    lv_cocode = 'Z'.
  ELSEIF i_input-vbukr EQ '3400'.
    lv_cocode = 'H'.
  ELSEIF i_input-vbukr EQ '3900'."2019-06-05 BY SW
    lv_cocode = 'S'.
  ENDIF.

* 项目类型转换
  CASE i_input-prart.
    WHEN '01'."新建
      lv_prart = 'X'.
    WHEN '02'."技改
      lv_prart = 'J'.
    WHEN '03'."研发
      lv_prart = 'Y'.
    WHEN '04'."测试
      lv_prart = 'C'.
  ENDCASE.

* 立项年份
  IF i_input-zlxnf IS INITIAL.
    g_output-msgtyp  = 'E'.
    g_output-message = TEXT-004.
  ENDIF.

* 根据项目负责人编码获取相关信息
  SELECT SINGLE *
    INTO wa_ztps_pro_resp
    FROM ztps_pro_resp
  WHERE responsible_no EQ i_input-vernr.
  IF sy-subrc NE 0.
    g_output-msgtyp  = 'E'.

    IF g_output-message IS INITIAL.
      g_output-message = TEXT-002 && i_input-vernr && TEXT-003.
    ELSE.
      g_output-message = g_output-message && ';' && TEXT-002 && i_input-vernr && TEXT-003.
    ENDIF.
  ENDIF.

  IF g_output IS NOT INITIAL.
    " 记录日志
    PERFORM frm_write_log USING lv_objno i_input.

    RETURN.
  ENDIF.

* 拼接生成项目编号
  IF i_input-prart EQ '01' OR i_input-prart EQ '02'.
    lv_objno = |{ lv_prart }{ lv_cocode }{ wa_ztps_pro_resp-tfcode }{ i_input-zlxnf+2(2) }|.
    lv_pspid = |{ lv_prart }{ '-' }{ lv_cocode }{ wa_ztps_pro_resp-tfcode }{ i_input-zlxnf+2(2) }|.
  ELSEIF i_input-prart EQ '03' OR i_input-prart EQ '04'.
    lv_objno = |{ lv_prart }{ lv_cocode }{ wa_ztps_pro_resp-tfcode }{ i_input-zcplx }{ i_input-zlxnf+2(2) }|.
    lv_pspid = |{ lv_prart }{ '-' }{ lv_cocode }{ wa_ztps_pro_resp-tfcode }{ i_input-zcplx }{ i_input-zlxnf+2(2) }|.
  ENDIF.

  lv_pspid = lv_pspid && '%'.
  CONDENSE lv_pspid NO-GAPS.

* 获取最大顺序号
  SELECT MAX( pspid_edit )
    INTO lv_pspid_edit
    FROM proj
  WHERE pspid_edit LIKE lv_pspid.
  IF sy-subrc EQ 0 AND lv_pspid_edit NE space.
    "获取字段长度
    lv_index = strlen( lv_pspid_edit ).

    "只保留最后3位
    lv_index = lv_index - 3.

    "顺序号加1
    lv_seqno = lv_pspid_edit+lv_index(3) + 1.

    "加前导0
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_seqno
      IMPORTING
        output = lv_seqno.
  ELSE.
    lv_seqno = '001'.
  ENDIF.

* 新的项目号
  lv_objno = lv_objno && lv_seqno.
  CONDENSE lv_objno NO-GAPS.

* 检查新的项目号是否正确
  IF strlen( lv_objno ) NE 9.

    CLEAR:lv_objno.
    g_output-msgtyp  = 'E'.
    g_output-message = TEXT-005.

    " 记录日志
    PERFORM frm_write_log USING lv_objno i_input.

    RETURN.
  ENDIF.

* 工厂赋值
  IF i_input-vbukr EQ '3100' or i_input-vbukr EQ '3900'."2019-06-05 by sw 增加3900苏震
    if i_input-vbukr EQ '3100'.
          lv_werks = '3110'.
          else.
            lv_werks = '3910'.
            endif.
  ELSE.
    lv_werks = i_input-vbukr.
  ENDIF.

** 项目定义创建
* 参数构建
  wa_project_definition-project_definition = lv_objno.       " 项目定义
  wa_project_definition-description        = i_input-post1.  " 描述

  "根据项目类型 进行项目参考文件赋值
  CASE i_input-prart.
    WHEN '01'.
      wa_project_definition-project_profile    = 'PS00001'.  " 项目参考文件
      lv_plnnr = '10000000'.
    WHEN '02'.
      wa_project_definition-project_profile    = 'PS00002'.  " 项目参考文件
      lv_plnnr = '10000001'.
    WHEN '03'.
      wa_project_definition-project_profile    = 'PS00003'.  " 项目参考文件
      lv_plnnr = '10000002'.
    WHEN '04'.
      wa_project_definition-project_profile    = 'PS00004'.  " 项目参考文件
      lv_plnnr = '10000003'.
  ENDCASE.

  wa_project_definition-responsible_no     = i_input-vernr.  " 负责人编号
  wa_project_definition-start              = i_input-plfaz.  " 项目计划开始日期
  wa_project_definition-finish             = i_input-plsez.  " 项目计划完成日期
  wa_project_definition-company_code       = i_input-vbukr.  " 公司代码
  wa_project_definition-plant              = lv_werks.       " 工厂

  " 利润中心
  IF i_input-prart EQ '01' OR i_input-prart EQ '02'.
    wa_project_definition-profit_ctr       = |{ 'P' }{ wa_project_definition-plant }|.
  ENDIF.

  REFRESH:lt_return.

* BAPI调用 进行项目定义创建
  CALL FUNCTION 'BAPI_PS_INITIALIZATION'.

  CALL FUNCTION 'BAPI_BUS2001_CREATE'
    EXPORTING
      i_project_definition = wa_project_definition
    TABLES
      et_return            = lt_return.

* 处理BAPI执行结果
  PERFORM frm_check_commit TABLES lt_return CHANGING e_output.

* 存在错误返回
  IF e_output-msgtyp EQ 'E'.

    " 记录日志
    PERFORM frm_write_log USING lv_objno i_input.

    RETURN.
  ENDIF.

* 新建项目 只进行项目定义及首层WBS创建
* 技改/研发/测试项目 项目定义之后，调用标准的模板进行创建
  IF i_input-prart EQ '01'.

* WBS创建
    wa_wbs_element-wbs_element                    = lv_objno.          " WBS元素
    wa_wbs_element-description                    = i_input-post1.     " 描述
    wa_wbs_element-wbs_account_assignment_element = abap_true.         " 科目分配元素
    wa_wbs_element-proj_type                      = i_input-prart.     " 项目类型
    wa_wbs_element-responsible_no                 = i_input-vernr.     " 负责人编号

    APPEND wa_wbs_element TO lt_wbs_element.
    CLEAR:wa_wbs_element.

    REFRESH:lt_return.

* 调用BAPI进行WBS创建
    CALL FUNCTION 'BAPI_PS_INITIALIZATION'.

    CALL FUNCTION 'BAPI_BUS2054_CREATE_MULTI'
      EXPORTING
        i_project_definition = wa_project_definition-project_definition
      TABLES
        it_wbs_element       = lt_wbs_element
        et_return            = lt_return.

* 处理BAPI执行结果| BAPI返回消息/接口返回消息
    PERFORM frm_check_commit TABLES lt_return CHANGING e_output.

* 存在错误 返回
    IF e_output-msgtyp EQ 'E'.
      " 记录日志
      PERFORM frm_write_log USING lv_objno i_input.

      RETURN.
    ENDIF.
  ELSE.

** 通过复制标准的模板 进行WBS/网络/活动创建 采用BDC的方式

* 1.进行WBS复制创建 |  OA传入参数/生成的项目号/工厂 /返回信息
    PERFORM frm_create_wbs_copy_system USING i_input
                                             lv_objno
                                             lv_werks
                                       CHANGING e_output.

* 存在错误 返回
    IF e_output-msgtyp EQ 'E'.
      " 记录日志
      PERFORM frm_write_log USING lv_objno i_input.

      RETURN.
    ENDIF.

  ENDIF.

* 2.进行网络复制创建 |  OA传入参数/生成的项目号/工厂 /返回信息
  wa_network-network_type       = 'PS02'.         " 网络类型
  wa_network-short_text         = '项目进度计划'. " 网络描述
  wa_network-profile            = 'PS00001'.      " 网络参数文件
  wa_network-plant              = lv_werks.       " 工厂
  wa_network-mrp_controller     = '101'.          " MRP控制者
  wa_network-start_date         = i_input-plfaz.  " 开始日期
  wa_network-finish_date        = i_input-plsez.  " 完成日期
  wa_network-project_definition = lv_objno.       " 项目定义
  wa_network-wbs_element        = lv_objno.       " WBS元素
  wa_network-short_text         = '项目进度计划'. " 描述

  CALL FUNCTION 'BAPI_PS_INITIALIZATION'.

  CALL FUNCTION 'BAPI_BUS2002_CREATE'
    EXPORTING
      i_network = wa_network
    TABLES
      et_return = lt_return.

* 处理BAPI执行结果| BAPI返回消息/接口返回消息
  PERFORM frm_check_commit TABLES lt_return CHANGING e_output.

* 存在错误 返回
  IF e_output-msgtyp EQ 'E'.
    "记录日志
    PERFORM frm_write_log USING lv_objno i_input.

    RETURN.
  ENDIF.

* 3.为网络创建活动
  IF g_networkno IS NOT INITIAL.
    " 获取任务清单 - 工序/作业选择
    SELECT plnty,
           plnnr,
           plnal,
           plnfl,
           plnkn,
           zaehl
      INTO TABLE @DATA(lt_plas)
      FROM plas
    WHERE plnty EQ '0'
      AND plnnr EQ @lv_plnnr
      AND plnal EQ '01'.

    " 获取标准的网络模板信息
    IF lt_plas IS NOT INITIAL.
      SELECT plnty
             plnnr
             plnkn
             zaehl
             vornr
             steus
             ltxa1
        INTO CORRESPONDING FIELDS OF TABLE lt_plpo
        FROM plpo
        FOR ALL ENTRIES IN lt_plas
      WHERE plnty EQ '0'
        AND plnnr EQ lv_plnnr
        AND plnkn EQ lt_plas-plnkn.
      IF sy-subrc EQ 0.

        LOOP AT lt_plpo INTO ls_plpo.
          wa_activity-plant                  = lv_werks.       " 工厂
          wa_activity-activity               = ls_plpo-vornr.  " 作业编号
          wa_activity-description            = ls_plpo-ltxa1.  " 作业描述
          wa_activity-wbs_element            = lv_objno.       " WBS元素
          wa_activity-control_key            = ls_plpo-steus.  " 控制码
          wa_activity-constraint_type_finish = '1'.            " 完成日期约束
          wa_activity-constraint_finish_date = i_input-plsez.  " 完成日期
          wa_activity-objectclass            = 'PRODT'.        " 对象类

          APPEND wa_activity TO lt_activity.
          CLEAR:wa_activity.
        ENDLOOP.

        CALL FUNCTION 'BAPI_PS_INITIALIZATION'.

        CALL FUNCTION 'BAPI_BUS2002_ACT_CREATE_MULTI'
          EXPORTING
            i_number    = g_networkno
          TABLES
            it_activity = lt_activity
            et_return   = lt_return.

* 处理BAPI执行结果| BAPI返回消息/接口返回消息
        PERFORM frm_check_commit TABLES lt_return CHANGING e_output.

* 存在错误 返回
        IF e_output-msgtyp EQ 'E'.
          "记录日志
          PERFORM frm_write_log USING lv_objno i_input.

          RETURN.
        ENDIF.
      ENDIF.

      CLEAR:lt_plas.
    ENDIF.
  ENDIF.

* 若不存在错误 代表成功
  e_output-msgtyp  = 'S'.
  e_output-message = TEXT-006 && lv_objno && TEXT-001.

  g_output = e_output.

* 日志记录
  PERFORM frm_write_log USING lv_objno i_input.

** 文本元素清单
* 001 Create Success
* 002	项目负责人
* 003	在SAP系统中不存在
* 004	立项年份为空
* 005	请检查项目部门是否正确
* 006 项目定义：

ENDFUNCTION.