FUNCTION zrfc_miro.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_ZCZLX) TYPE  ZECZLX
*"     VALUE(I_ZFPDM) TYPE  ZEFPDM
*"     VALUE(I_ZFPHM) TYPE  ZEFPHM
*"     VALUE(I_BLDAT) TYPE  BLDAT
*"     VALUE(I_BUDAT) TYPE  BUDAT
*"     VALUE(I_XBLNR) TYPE  XBLNR OPTIONAL
*"     VALUE(I_BUKRS) TYPE  BUKRS
*"     VALUE(I_WAERS) TYPE  WAERS
*"     VALUE(I_RMWWR) TYPE  RMWWR
*"     VALUE(I_BKTXT) TYPE  BKTXT OPTIONAL
*"     VALUE(I_ZUONR) TYPE  DZUONR OPTIONAL
*"     VALUE(I_WMWST) TYPE  WMWST
*"  EXPORTING
*"     VALUE(EX_MTYPE) TYPE  BAPI_MTYPE
*"     VALUE(EX_MSG) TYPE  BAPI_MSG
*"     VALUE(EX_BELNR) TYPE  RBKP-BELNR
*"     VALUE(EX_GJAHR) TYPE  RBKP-GJAHR
*"     VALUE(EX_BELNR_BKPF) TYPE  BKPF-BELNR
*"     VALUE(EX_GJAHR_BKPF) TYPE  BKPF-GJAHR
*"  TABLES
*"      IT_ITEM STRUCTURE  ZSMIRO_ITEM
*"----------------------------------------------------------------------

  " local variable define
  DATA:lt_item TYPE TABLE OF zsmiro_item,
       ls_item TYPE zsmiro_item.

  DATA:lt_ekbe TYPE TABLE OF ekbe,
       ls_ekbe TYPE ekbe.

  DATA:lt_mdbs TYPE TABLE OF mdbs,
       ls_mdbs TYPE mdbs.

  DATA:lv_buzei TYPE rseg-buzei,
       lv_belnr TYPE rbkp-belnr,
       lv_sgtxt TYPE rbkp-sgtxt,
       lv_awkey TYPE bkpf-awkey.
  DATA:lv_menge_bill TYPE mseg-menge,
       lv_menge_rece TYPE mseg-menge,
       lv_index TYPE c LENGTH 3.

  " bapi variable difine
  DATA:ls_headerdata           TYPE bapi_incinv_create_header,
       ls_invoicedocnumber     TYPE bapi_incinv_fld-inv_doc_no,
       ls_fiscalyear           TYPE bapi_incinv_fld-fisc_year,
       lt_itemdata             TYPE STANDARD TABLE OF bapi_incinv_create_item,
       ls_itemdata             TYPE bapi_incinv_create_item,
       lt_materialdata         TYPE STANDARD TABLE OF bapi_incinv_create_material,
       ls_materialdata         TYPE bapi_incinv_create_material,
       lt_taxdata              TYPE STANDARD TABLE OF bapi_incinv_create_tax,
       ls_taxdata              TYPE bapi_incinv_create_tax,
       lt_return               TYPE STANDARD TABLE OF bapiret2,
       ls_return               TYPE bapiret2.
  " Constants define
  CONSTANTS:c_fieldx TYPE c VALUE 'X',
            c_blart  TYPE bkpf-blart VALUE 'RE',
            c_mtype_e TYPE bapi_mtype VALUE 'E',
            c_mtype_a TYPE bapi_mtype VALUE 'A',
            c_mtype_s TYPE bapi_mtype VALUE 'S',
            c_separated TYPE c VALUE '|'.

  " Refresh variable

  REFRESH: lt_item,
           lt_itemdata,
           lt_materialdata,
           lt_taxdata.

  CLEAR:  ls_item,
           ls_headerdata,
           ls_itemdata,
           ls_materialdata,
           ls_taxdata,
           lv_buzei.
  " Variable assignment

  lt_item   = it_item[].

  IF i_zczlx IS INITIAL.
    ex_msg = '处理类型为空'.
    ex_mtype = c_mtype_e.
    EXIT.
  ENDIF.

  IF i_zfpdm IS INITIAL.
    ex_msg = '发票代码为空'.
    ex_mtype = c_mtype_e.
    EXIT.
  ENDIF.

  IF i_zfphm IS INITIAL.
    ex_msg = '发票号码为空'.
    ex_mtype = c_mtype_e.
    EXIT.
  ENDIF.

  CLEAR:lv_sgtxt.
  CONCATENATE i_zfpdm i_zfphm INTO lv_sgtxt SEPARATED BY '-'. "Item Text
  SELECT SINGLE belnr INTO lv_belnr FROM rbkp
    WHERE sgtxt = lv_sgtxt
      AND vgart = 'RD'
      AND stblg = ''
      AND rbstat NE '2'.

  IF sy-subrc = 0.
    CONCATENATE '发票号码' lv_sgtxt '已存在SAP发票' lv_belnr '!' INTO ex_msg.
    ex_mtype = c_mtype_e.
    EXIT.
  ENDIF.



  "check invoice quantity
  IF lt_item IS NOT INITIAL.
    SELECT ebeln
           ebelp
           zekkn
           vgabe
           gjahr
           belnr
           buzei
           shkzg
           menge
    INTO CORRESPONDING FIELDS OF TABLE lt_ekbe
    FROM ekbe FOR ALL ENTRIES IN lt_item
    WHERE ebeln = lt_item-ebeln
      AND ebelp = lt_item-ebelp
      AND ( vgabe = '2' OR vgabe = 'P' ).

    SELECT ebeln
           ebelp
           etenr
           wemng
    INTO CORRESPONDING FIELDS OF TABLE lt_mdbs
    FROM mdbs FOR ALL ENTRIES IN lt_item
    WHERE ebeln = lt_item-ebeln
      AND ebelp = lt_item-ebelp.
  ENDIF.
  CLEAR:lv_index.
  LOOP AT lt_item INTO ls_item.
    ADD 1 TO lv_index.
    CLEAR:lv_menge_bill,lv_menge_rece.
    "billing  quantity
    LOOP AT lt_ekbe INTO ls_ekbe WHERE ebeln = ls_item-ebeln
                                   AND ebelp = ls_item-ebelp.
      IF ls_ekbe-shkzg = 'H'.
        ls_ekbe-menge = - ls_ekbe-menge.
      ENDIF.
      lv_menge_bill = lv_menge_bill + ls_ekbe-menge.
    ENDLOOP.
    "receiving quantity
    LOOP AT lt_mdbs INTO ls_mdbs WHERE ebeln = ls_item-ebeln
                                   AND ebelp = ls_item-ebelp.
      lv_menge_rece = lv_menge_rece + ls_mdbs-wemng.
    ENDLOOP.

    lv_menge_bill = lv_menge_bill + ls_item-menge.
    IF lv_menge_bill > lv_menge_rece.
      CONCATENATE '第' lv_index '行采购订单' ls_item-ebeln '行项目' ls_item-ebelp '发票数量大于收货数量' INTO ex_msg.
      ex_mtype = 'E'.
      EXIT.
    ENDIF.

  ENDLOOP.

  CHECK ex_mtype NE 'E'.

  ls_headerdata-invoice_ind = c_fieldx.   "Indicator: post invoice
  ls_headerdata-doc_date     = i_bldat. "Document Date
  ls_headerdata-pstng_date   = i_budat. "Posting Date
  ls_headerdata-ref_doc_no   = i_xblnr. "Reference Document Number
  ls_headerdata-comp_code    = i_bukrs. "Company Code
  ls_headerdata-currency     = i_waers. "Currency Key
  ls_headerdata-gross_amount = i_rmwwr. "Gross Invoice Amount in Document Currency
  ls_headerdata-header_txt   = i_bktxt. "Document Header Text
*  ls_headerdata-inv_rec_date = i_reindat."Invioce receipt date
  ls_headerdata-alloc_nmbr   = i_zuonr. "Assignment Number
  CONCATENATE i_zfpdm i_zfphm INTO ls_headerdata-item_text SEPARATED BY '-'. "Item Text
*  ls_headerdata-calc_tax_ind = c_fieldx.

  READ TABLE lt_item INTO ls_item INDEX 1.
  ls_taxdata-tax_code   = ls_item-mwskz.           "Sales Tax Code
  ls_taxdata-tax_amount      = i_wmwst.     "Tax Amount in Document Currency
  APPEND ls_taxdata TO lt_taxdata.
  CLEAR ls_taxdata.

  LOOP AT lt_item INTO ls_item.
    ADD 1 TO lv_buzei.
    ls_itemdata-invoice_doc_item = lv_buzei.         "Document Item in Invoice Document
    ls_itemdata-po_number        = ls_item-ebeln.    "Purchase Order Number
    ls_itemdata-po_item          = ls_item-ebelp.    "Item Number of Purchasing Document
    ls_itemdata-ref_doc          = ls_item-lfbnr.    "Document No. of a Reference Document
    ls_itemdata-ref_doc_year     = ls_item-lfgja.    "Fiscal Year of Current Period
    ls_itemdata-ref_doc_it       = ls_item-lfpos.    "Item of a Reference Document
    ls_itemdata-item_amount      = ls_item-wrbtr.    "Amount in document currency
    ls_itemdata-quantity         = ls_item-menge.    "Quantity
    ls_itemdata-po_unit          = ls_item-bstme.    "Purchase Order Unit of Measure
    ls_itemdata-po_pr_qnt        = ls_item-bpmng.    "Quantity in Purchase Order Price Unit
    ls_itemdata-po_pr_uom        = ls_item-bprme.    "Order Price Unit (Purchasing)
    ls_itemdata-tax_code         = ls_item-mwskz.    "Tax Code
    ls_itemdata-item_text        = ls_item-sgtxt.    "Item Text

    APPEND ls_itemdata TO lt_itemdata.
    CLEAR:ls_itemdata.

  ENDLOOP.

  IF i_zczlx = '1'.
    CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
      EXPORTING
        headerdata       = ls_headerdata
      IMPORTING
        invoicedocnumber = ls_invoicedocnumber
        fiscalyear       = ls_fiscalyear
      TABLES
        itemdata         = lt_itemdata
        taxdata          = lt_taxdata
        return           = lt_return.
  ELSEIF i_zczlx = '2'.
    CALL FUNCTION 'BAPI_INCOMINGINVOICE_PARK'
      EXPORTING
        headerdata       = ls_headerdata
      IMPORTING
        invoicedocnumber = ls_invoicedocnumber
        fiscalyear       = ls_fiscalyear
      TABLES
        itemdata         = lt_itemdata
        taxdata          = lt_taxdata
        return           = lt_return.
  ELSE.
    ex_msg = '处理类型值错误'.
    ex_mtype = c_mtype_e.
    EXIT.
  ENDIF.



  LOOP AT lt_return INTO ls_return WHERE type = c_mtype_a OR type = c_mtype_e.
    CONCATENATE ex_msg ls_return-message INTO ex_msg SEPARATED BY c_separated.
    ex_mtype = c_mtype_e.
  ENDLOOP.
  IF sy-subrc NE 0.
    COMMIT WORK AND WAIT.
    IF i_zczlx = '1'.
      ex_msg = '发票校验成功'.
    ELSE.
      ex_msg = '发票预制成功'.
    ENDIF.
    ex_mtype = c_mtype_s.
    ex_belnr = ls_invoicedocnumber.
    ex_gjahr = ls_fiscalyear.
    CLEAR:lv_awkey.
    CONCATENATE ex_belnr ex_gjahr INTO lv_awkey.
    SELECT SINGLE belnr gjahr
    FROM bkpf
    INTO (ex_belnr_bkpf , ex_gjahr_bkpf)
    WHERE awkey = lv_awkey.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
  SHIFT ex_msg LEFT DELETING LEADING c_separated.



ENDFUNCTION.