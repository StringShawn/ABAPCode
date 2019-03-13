*&---------------------------------------------------------------------*
*& Report  Z_15405_CREATE_PO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT Z_15405_CREATE_PO.

 FORM frm_create_po TABLES p_poitem STRUCTURE zsmm054_poitem
                   USING  p_poheader TYPE zsmm054_poheader
                   CHANGING p_type
                            p_rmsg
                            p_ebeln.
  DATA ls_header TYPE bapimepoheader.
  DATA ls_headerx TYPE bapimepoheaderx.
  DATA lt_item TYPE TABLE OF bapimepoitem.
  DATA lt_itemx TYPE TABLE OF bapimepoitemx.
  DATA lt_pocond TYPE TABLE OF bapimepocond.
  DATA lt_pocondx TYPE TABLE OF bapimepocondx.
  DATA lt_return TYPE TABLE OF bapiret2.
  DATA lt_account TYPE TABLE OF bapimepoaccount.
  DATA lt_accountx TYPE TABLE OF bapimepoaccountx.

  DATA ls_item TYPE bapimepoitem.
  DATA ls_itemx TYPE bapimepoitemx.
  DATA ls_pocond TYPE bapimepocond.
  DATA ls_pocondx TYPE bapimepocondx.
  DATA ls_return TYPE bapiret2.
  DATA ls_account TYPE bapimepoaccount.
  DATA ls_accountx TYPE bapimepoaccountx.

  DATA:lt_text TYPE TABLE OF bapimepotextheader,
       ls_text TYPE bapimepotextheader.
  DATA:lt_ftaxp TYPE TABLE OF ftaxp,
       ls_ftaxp TYPE ftaxp.

  DATA temp_ebelp TYPE ekpo-ebelp.

  ls_header-doc_type = 'ZNB'.
  ls_header-comp_code = p_poheader-bukrs.
  ls_header-vendor = p_poheader-lifnr.
  ls_header-purch_org = '1000'. "采购组织
  "采购组
  ls_header-pur_group = '002'.

  ls_headerx-doc_type = 'X'.
  ls_headerx-comp_code = 'X'.
  ls_headerx-vendor = 'X'.
  ls_headerx-purch_org = 'X'.
  ls_headerx-pur_group = 'X'.

  CLEAR:lt_text, ls_text.
  ls_text-text_id = 'F01'.
  ls_text-text_line = p_poheader-oanum.
  APPEND ls_text TO lt_text.

  CLEAR temp_ebelp.

  LOOP AT p_poitem INTO DATA(ls_data).

    ADD 10 TO temp_ebelp.
    ls_item-po_item = temp_ebelp.    "采购订单行项目号
    ls_item-quantity = ls_data-menge."数量
    ls_item-plant = ls_data-werks.   "工厂
    ls_item-tax_code = ls_data-mwskz."税码
    ls_item-short_text = ls_data-txz01."短文本
    ls_item-acctasscat = 'L'.    "服务类订单
    ls_item-po_unit    = 'ST'.   "单位 pc
    ls_item-matl_group = 'Z001'. "物料组
    APPEND ls_item TO lt_item.

    ls_itemx-po_item   = temp_ebelp.
    ls_itemx-short_text = 'X'.
    ls_itemx-acctasscat = 'X'.
    ls_itemx-po_itemx  = 'X'.
    ls_itemx-plant     = 'X'.
    ls_itemx-quantity  = 'X'.
    ls_itemx-tax_code  = 'X'.
    ls_itemx-po_unit   = 'X'.
    ls_itemx-matl_group = 'X'.
    APPEND ls_itemx TO lt_itemx.

    ls_pocond-itm_number = temp_ebelp. "
    ls_pocond-cond_type = 'PBXX'.      "
    ls_pocond-currency = 'CNY'.        "
    ls_pocond-change_id = 'U'.
    REFRESH lt_ftaxp.
    CLEAR ls_ftaxp.
    CALL FUNCTION 'GET_TAX_PERCENTAGE'
      EXPORTING
        aland   = 'CN'
        datab   = sy-datum
        mwskz   = ls_item-tax_code
        txjcd   = ''
      TABLES
        t_ftaxp = lt_ftaxp.
    READ TABLE lt_ftaxp INTO ls_ftaxp INDEX 1.
    ls_pocond-cond_value = ls_data-dmbtr * ( 1 + ls_ftaxp-kbetr / 1000 ).
    APPEND ls_pocond TO lt_pocond.

    ls_pocondx-itm_number = temp_ebelp.
    ls_pocondx-cond_type = 'X'.
    ls_pocondx-currency = 'X'.
    ls_pocondx-change_id = 'X'.
    ls_pocondx-cond_value = 'X'.
    APPEND ls_pocondx TO lt_pocondx.

    ls_account-po_item = temp_ebelp.
    ls_account-serial_no = '1'.
    ls_account-quantity = ls_data-menge.
    ls_account-gl_account = '1123040100'.
    APPEND ls_account TO lt_account.
    ls_accountx-po_item = temp_ebelp.
    ls_accountx-serial_no = '1'.
    ls_accountx-quantity = 'X'.
    ls_accountx-gl_account = 'X'.
    APPEND ls_accountx TO lt_accountx.

  ENDLOOP.

  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader         = ls_header
      poheaderx        = ls_headerx
      no_price_from_po = 'X'
    IMPORTING
      exppurchaseorder = p_ebeln
    TABLES
      return           = lt_return
      poitem           = lt_item
      poitemx          = lt_itemx
      pocond           = lt_pocond
      pocondx          = lt_pocondx
      poaccount        = lt_account
      poaccountx       = lt_accountx
      potextheader     = lt_text.

  READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    p_type = 'E'.
    LOOP AT lt_return INTO ls_return WHERE type = 'E'.
      CONCATENATE p_rmsg ls_return-message INTO p_rmsg SEPARATED BY ';'.
    ENDLOOP.
    SHIFT p_rmsg LEFT DELETING LEADING ';'.
  ELSE.
    p_type = 'S'.
    p_rmsg = '创建成功，采购订单号为：' && p_ebeln.
  ENDIF.


ENDFORM.
