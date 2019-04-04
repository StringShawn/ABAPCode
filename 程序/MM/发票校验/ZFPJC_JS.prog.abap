*&---------------------------------------------------------------------*
*&  包含文件              ZMMR003_IN_001
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*   tables                                                            *
*---------------------------------------------------------------------*

TABLES:
* benutzt für SELECT
  RKWA,

* benutzt bei externem Perform
  BKPF,
  BSEG,
  BSET,
  ITXDAT,
  T001W.

TYPE-POOLS: MRM, SHLP.

INCLUDE: MRM_CONST_COMMON,             "allgemeine Konstanten
         MRM_CONST_NAST,               " Nachrichten
         MRM_CONST_MRM,
         MRM_CONST_ME,                 "aus dem Einkauf
         MRM_CONST_MB,                 "aus der Bestandsführung
         MRM_CONST_FI.                 "aus dem FI

*---------------------------------------------------------------------*
*   Typen (TYP_*)
*---------------------------------------------------------------------*

* Meldung
TYPES: BEGIN OF TYP_MSG,
         MSGID LIKE SY-MSGID,
         MSGTY LIKE SY-MSGTY,
         MSGNO LIKE SY-MSGNO,
         MSGV1 LIKE SY-MSGV1,
         MSGV2 LIKE SY-MSGV2,
         MSGV3 LIKE SY-MSGV3,
         MSGV4 LIKE SY-MSGV4,
       END OF TYP_MSG.

* Block von Warenentnahmen (zu Buchungskreis/Lieferant)
TYPES: BEGIN OF TYP_BLOCK,
         BUKRS   LIKE RKWA-BUKRS,                           "key
         LIFNR   LIKE RKWA-LIFNR,                           "key
         BWAER   LIKE RKWA-BWAER,                           "key
         IT_RKWA LIKE RKWA    OCCURS 1,"Warenentnahmen
         BSTAT   TYPE C,               "Status -> C_BSTAT_*
         BELNR   LIKE RKWA-BELNR,      "Rechnungsbeleg
         GJAHR   LIKE RKWA-GJAHR,      "Rechnungsbeleg
         MSGID   LIKE SY-MSGID,
         MSGTY   LIKE SY-MSGTY,
         MSGNO   LIKE SY-MSGNO,
         MSGV1   LIKE SY-MSGV1,
         MSGV2   LIKE SY-MSGV2,
         MSGV3   LIKE SY-MSGV3,
         MSGV4   LIKE SY-MSGV4,
       END OF TYP_BLOCK.

* zum Berechnen der Steuern
TYPES: BEGIN OF TYP_BSET,
         WRBTR LIKE BSEG-WRBTR,        "CURR statt CHAR (Vorzeichen!)
         FWSTE LIKE BSET-FWSTE,        "CURR statt CHAR (Vorzeichen!)
         MWSKZ LIKE RBSET-MWSKZ,
         TXJCD LIKE T001W-TXJCD,
       END OF TYP_BSET.

* Puffer für MWSKZ/TXJCD Abh?ngigkeit von Material, Lieferant und Werk
TYPES: BEGIN OF TYP_TAX,
         MATNR LIKE BSEG-MATNR,                             "key
         WERKS LIKE T001W-WERKS,                            "key
         LIFNR LIKE VF_KRED-LIFNR,                          "key
         MWSKZ LIKE RBSET-MWSKZ,
         TXJCD LIKE T001W-TXJCD,
       END OF TYP_TAX.

* Tabellentypen (TYP_TAB_*)
TYPES: TYP_TAB_BKPF  LIKE BKPF      OCCURS 1,
       TYP_TAB_BSEG  LIKE BSEG      OCCURS 1,
       TYP_TAB_BSET  LIKE BSET      OCCURS 1,
       TYP_TAB_RKWA  LIKE RKWA      OCCURS 1,
       TYP_TAB_BLOCK TYPE TYP_BLOCK OCCURS 1,
       TYP_TAB_TAX   TYPE TYP_TAX   OCCURS 1,
       TYP_TAB_RBWS  LIKE RBWS      OCCURS 1.

DATA: S_VARIANT      LIKE  DISVARIANT,
      S_VAR_USR      LIKE  DISVARIANT.

*---------------------------------------------------------------------*
*   Konstanten (C_*)                                                  *
*---------------------------------------------------------------------*

CONSTANTS:

      C_SAVE(1)      TYPE  C               VALUE  'A',
      C_REPID        TYPE  SY-REPID        VALUE  'RMVKON00',
      C_TOP_OF_LIST  TYPE  SLIS_FORMNAME   VALUE  'TOP_OF_LIST',
      C_DOKCLASS_NA  TYPE  DSYSH-DOKCLASS  VALUE  'NA',

* Transaktionscodes
  C_TCODE_MRKO LIKE SY-TCODE VALUE 'MRKO',

* RKWA-Status
  C_STATUS_NINV LIKE RKWA-STATUS VALUE '00',   "nicht abgerechnet
  C_STATUS_INV  LIKE RKWA-STATUS VALUE '01',    "abgerechnet

* Block-Abrechnungs-Status
  C_BSTAT_NULL   TYPE C VALUE SPACE,   "nicht verarbeitet
  C_BSTAT_OK     TYPE C VALUE '1',     "abgerechnet
  C_BSTAT_FEHLER TYPE C VALUE '2',     "Fehler bei Abrechnung

* Sonstiges
  C_STYPE_N LIKE BBKPF-STYPE VALUE 'N'."batch-input: no


*---------------------------------------------------------------------*
*  Ranges
*---------------------------------------------------------------------*

RANGES: R_STATUS FOR RKWA-STATUS OCCURS 1,
        R_SOBKZ  FOR RKWA-SOBKZ  OCCURS 1.

***********************************************************************
*    allgemeine Form-Routinen
***********************************************************************

*---------------------------------------------------------------------*
*       FORM MSG_FUELLEN_SYSVAR                                       *
*---------------------------------------------------------------------*
* <---  S_MSG                                                         *
*---------------------------------------------------------------------*
FORM MSG_FUELLEN_SYSVAR CHANGING S_MSG TYPE TYP_MSG.

  CLEAR S_MSG.
  S_MSG-MSGID = SY-MSGID.
  S_MSG-MSGTY = SY-MSGTY.
  S_MSG-MSGNO = SY-MSGNO.
  S_MSG-MSGV1 = SY-MSGV1.
  S_MSG-MSGV2 = SY-MSGV2.
  S_MSG-MSGV3 = SY-MSGV3.
  S_MSG-MSGV4 = SY-MSGV4.

ENDFORM.                    "msg_fuellen_sysvar
*---------------------------------------------------------------------*
*      Form RKWA_PARTITIONIEREN
*---------------------------------------------------------------------*
*      Teilt die alle Warenentnahmen in IT_RKWA auf.
*      Die Bl?cke werden in IT_BLOCK abgelegt.
* ---> IT_RKWA
* <--- IT_BLOCK
*---------------------------------------------------------------------*
FORM RKWA_PARTITIONIEREN TABLES IT_RKWA  TYPE TYP_TAB_RKWA
                                IT_BLOCK TYPE TYP_TAB_BLOCK.

  DATA: S_BLOCK TYPE TYP_BLOCK,
        S_RKWA  LIKE RKWA,
        IDX     LIKE SY-TABIX,
        WRBTR   LIKE RKWA-WRBTR,
        S_MSG TYPE TYP_MSG.
  DATA: F_WAERS TYPE WAERS,
        F_OBJECT TYPE OBJECT_CURRO.
  DATA: F_KURSF LIKE BKPF-KURSF.
  F_OBJECT = 'BKPF'.

* baue IT_BLOCK auf (sortiert nach BUKRS/LIFNR)
  CLEAR IT_BLOCK.

* Currency expired --> S_RKWA contains new currency (e.g. EUR) -------*
  LOOP AT IT_RKWA INTO S_RKWA.
    CALL FUNCTION 'CURRENCY_EXPIRATION_CHECK'
      EXPORTING
        CURRENCY      = S_RKWA-BWAER
        DATE          = SY-DATLO
        OBJECT        = F_OBJECT
        BUKRS         = S_RKWA-BUKRS
      IMPORTING
        CURRENCY_NEW  = F_WAERS
      EXCEPTIONS
        ERROR_MESSAGE = 4.
    IF SY-SUBRC = 0.
      F_WAERS = S_RKWA-BWAER.
    ELSEIF F_WAERS IS INITIAL.
      MESSAGE E607 WITH 'RKWA_PARTITIONIEREN'.
    ENDIF.
    IF S_RKWA-BWAER NE F_WAERS.
      CLEAR: F_KURSF.
      CALL FUNCTION 'READ_EXCHANGE_RATE'
        EXPORTING
          DATE             = SY-DATLO
          FOREIGN_CURRENCY = S_RKWA-BWAER
          LOCAL_CURRENCY   = F_WAERS
        IMPORTING
          EXCHANGE_RATE    = F_KURSF
        EXCEPTIONS
          ERROR_MESSAGE    = 1.

      IF ( SY-SUBRC <> 0 ).
        MESSAGE ID SY-MSGID TYPE 'E'
        NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
        EXPORTING
          FOREIGN_CURRENCY = S_RKWA-BWAER
          LOCAL_CURRENCY   = F_WAERS
          FOREIGN_AMOUNT   = S_RKWA-WRBTR
          RATE             = F_KURSF
          DATE             = SY-DATLO
        IMPORTING
          LOCAL_AMOUNT     = S_RKWA-WRBTR
        EXCEPTIONS
          ERROR_MESSAGE    = 1.

      IF ( SY-SUBRC <> 0 ).
        MESSAGE ID SY-MSGID TYPE 'E'
        NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      IF NOT S_RKWA-NAVNW IS INITIAL.
        CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
          EXPORTING
            FOREIGN_CURRENCY = S_RKWA-BWAER
            LOCAL_CURRENCY   = F_WAERS
            FOREIGN_AMOUNT   = S_RKWA-NAVNW
            RATE             = F_KURSF
            DATE             = SY-DATLO
          IMPORTING
            LOCAL_AMOUNT     = S_RKWA-NAVNW
          EXCEPTIONS
            ERROR_MESSAGE    = 1.

        IF ( SY-SUBRC <> 0 ).
          MESSAGE ID SY-MSGID TYPE 'E'
          NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.
    ENDIF.
    S_RKWA-BWAER = F_WAERS.

    READ TABLE IT_BLOCK INTO S_BLOCK
                        WITH KEY BUKRS = S_RKWA-BUKRS
                                 LIFNR = S_RKWA-LIFNR
                                 BWAER = S_RKWA-BWAER
                        BINARY SEARCH.
    IDX = SY-TABIX.
    IF ( SY-SUBRC <> 0 ).              "Block noch nicht vorhanden
      CLEAR S_BLOCK.
      S_BLOCK-BSTAT = C_BSTAT_NULL.
      S_BLOCK-BUKRS = S_RKWA-BUKRS.
      S_BLOCK-LIFNR = S_RKWA-LIFNR.
      S_BLOCK-BWAER = S_RKWA-BWAER.
      APPEND S_RKWA TO S_BLOCK-IT_RKWA.
      INSERT S_BLOCK INTO IT_BLOCK INDEX IDX.  "Tab. bleibt sortiert!
    ELSE.                              "Block bereits vorhanden
      APPEND S_RKWA TO S_BLOCK-IT_RKWA.
      MODIFY IT_BLOCK FROM S_BLOCK INDEX IDX.
    ENDIF.
  ENDLOOP.

* rkwa_pruefen
  LOOP AT IT_BLOCK INTO S_BLOCK.
    CLEAR: S_RKWA.
    LOOP AT S_BLOCK-IT_RKWA INTO S_RKWA.
      IF ( S_RKWA-SOBKZ <> C_SOBKZ_KONS ) AND
     ( S_RKWA-SOBKZ <> C_SOBKZ_PIPE ).
        CLEAR S_MSG.
        S_MSG-MSGID = C_MSGID_M8.
        S_MSG-MSGTY = C_MSGTY_ERROR.
        S_MSG-MSGNO = '008'.
        S_MSG-MSGV1 = 'RKWA_PARTITIONIEREN'.
        EXIT.
      ENDIF.
    ENDLOOP.
    MOVE-CORRESPONDING S_MSG TO S_BLOCK.
    MODIFY IT_BLOCK FROM S_BLOCK.
  ENDLOOP.

* Partitionen sortieren
  LOOP AT IT_BLOCK INTO S_BLOCK.
    SORT S_BLOCK-IT_RKWA BY MJAHR MBLNR ZEILE.
    MODIFY IT_BLOCK FROM S_BLOCK.
  ENDLOOP.

ENDFORM.                    "rkwa_partitionieren
*eject
*---------------------------------------------------------------------*
*       FORM LONG_MESSAGE_SHOW                                        *
*---------------------------------------------------------------------*
*   Langtext der Meldungen in der Ausgabeliste anzeigen               *
*---------------------------------------------------------------------*
FORM LONG_TEXT_MESSAGE_SHOW USING I_ALV_LIST TYPE MRM_CON_ALV.

  DATA:    T_DUMMY LIKE TLINE OCCURS 1,
           DOKNAME LIKE DOKIL-OBJECT,                       "CHAR60
           TITLE   LIKE DSYST-DOKTITLE,
           DYNNR   LIKE HELP_INFO-DYNPRO,
           PFKEY   LIKE HELP_INFO-PFKEY.

* System-Felder kopieren/konvertieren
  TITLE = SY-TITLE.
  DYNNR = SY-DYNNR.
  PFKEY = SY-PFKEY.

* Dokumentname konstuieren
  CALL FUNCTION 'DOCU_OBJECT_NAME_CONCATENATE'
    EXPORTING
      DOCU_ID  = C_DOKCLASS_NA
      ELEMENT  = I_ALV_LIST-ARBGB
      ADDITION = I_ALV_LIST-TXTNR
    IMPORTING
      OBJECT   = DOKNAME.

* Aufruf Popup mit dem Langtext der Meldung
  CALL FUNCTION 'HELP_OBJECT_SHOW'
    EXPORTING
      DOKCLASS          = C_DOKCLASS_NA
      DOKNAME           = DOKNAME
      DOKTITLE          = TITLE
      CALLED_BY_PROGRAM = C_REPID
      CALLED_BY_DYNP    = DYNNR
      MSG_VAR_1         = I_ALV_LIST-MSGV1
      MSG_VAR_2         = I_ALV_LIST-MSGV2
      MSG_VAR_3         = I_ALV_LIST-MSGV3
      MSG_VAR_4         = I_ALV_LIST-MSGV4
      CALLED_BY_CUAPROG = C_REPID
      CALLED_BY_CUASTAT = PFKEY
    TABLES
      LINKS             = T_DUMMY.

ENDFORM.                    "long_text_message_show

*---------------------------------------------------------------------*
*       FORM LONG_TEXT_MESSAGE_PREPARE                                *
*---------------------------------------------------------------------*
*  ermittelt den Text zu einer von einem Funktionsbaustein zurückge-  *
*  lieferten Fehlermeldung                                            *
*---------------------------------------------------------------------*
* Parameter:                                                          *
* <-> px_mesg: Struktur MESG; enth?lt die aufzubereitende Fehlermel-  *
*              dung; im Fehlerfall wird eine Default-Meldung abgelegt *
*---------------------------------------------------------------------*
FORM LONG_TEXT_MESSAGE_PREPARE CHANGING I_MESG STRUCTURE MESG.

  DATA: H_MTEXT LIKE SY-LISEL,
        H_MSGNO LIKE SY-MSGNO.

  MOVE I_MESG-TXTNR TO H_MSGNO.

  CALL FUNCTION 'RPY_MESSAGE_COMPOSE'
    EXPORTING
      MESSAGE_ID        = I_MESG-ARBGB
      MESSAGE_NUMBER    = H_MSGNO
      MESSAGE_VAR1      = I_MESG-MSGV1
      MESSAGE_VAR2      = I_MESG-MSGV2
      MESSAGE_VAR3      = I_MESG-MSGV3
      MESSAGE_VAR4      = I_MESG-MSGV4
    IMPORTING
      MESSAGE_TEXT      = H_MTEXT
    EXCEPTIONS
      MESSAGE_NOT_FOUND = 1
      OTHERS            = 2.

  IF SY-SUBRC = 0.
    MOVE H_MTEXT(73) TO I_MESG-TEXT.
  ELSE.
    CALL FUNCTION 'RPY_MESSAGE_COMPOSE'
      EXPORTING
        MESSAGE_ID        = SY-MSGID
        MESSAGE_NUMBER    = SY-MSGNO
        MESSAGE_VAR1      = SY-MSGV1
        MESSAGE_VAR2      = SY-MSGV2
        MESSAGE_VAR3      = SY-MSGV3
        MESSAGE_VAR4      = SY-MSGV4
      IMPORTING
        MESSAGE_TEXT      = H_MTEXT
      EXCEPTIONS
        MESSAGE_NOT_FOUND = 1
        OTHERS            = 2.
    IF SY-SUBRC EQ 0.
      I_MESG-TEXT = H_MTEXT(73).
    ELSE.
* Ooops, no text!.
*   message x062 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4. ?
      CLEAR I_MESG-TEXT .
    ENDIF.
  ENDIF.

ENDFORM.                               "LONG_TEXT_MESSAGE_PREPARE
***********************************************************************
*    Form-Routinen zum ABRECHNEN
***********************************************************************


*---------------------------------------------------------------------*
*      Form ABRECHNEN
*---------------------------------------------------------------------*
*      Rechnet alle selektierten Warenentnahmen ab.
* ---> IT_BLOCK
*---------------------------------------------------------------------*
FORM ABRECHNEN TABLES IT_BLOCK TYPE TYP_TAB_BLOCK
                USING  P_TAX
                       P_SGTXT
                       P_BUDAT
                       P_BLDAT.

  DATA: S_BLOCK  TYPE TYP_BLOCK,
        S_MSG    TYPE TYP_MSG,
        BLART    LIKE T169F-BLART,
        X_FEHLER TYPE C.

  PERFORM BELEGART_BESTIMMEN CHANGING BLART
                                      S_MSG
                                      X_FEHLER.

  IF X_FEHLER = X.
    LOOP AT IT_BLOCK INTO S_BLOCK.
      MOVE-CORRESPONDING S_MSG TO S_BLOCK.
      MODIFY IT_BLOCK FROM S_BLOCK.
    ENDLOOP.
    EXIT.
  ENDIF.

  LOOP AT IT_BLOCK INTO S_BLOCK.
    PERFORM BLOCK_ABRECHNEN TABLES S_BLOCK-IT_RKWA   "--->
                             USING S_BLOCK-BUKRS     "--->
                                   S_BLOCK-BWAER     "--->
                                   P_TAX"税额
                                   P_SGTXT"供应商发票号码
                                   P_BUDAT
                                   P_BLDAT"凭证日期
                          CHANGING S_MSG
                                   X_FEHLER          "<---
                                   BLART
                                   S_BLOCK-LIFNR
                                   S_BLOCK-BELNR     "<---
                                   S_BLOCK-GJAHR.    "<---
*   Block-Status setzen
    IF ( X_FEHLER = X ).
      S_BLOCK-BSTAT = C_BSTAT_FEHLER.
      CLEAR: S_BLOCK-BELNR, S_BLOCK-GJAHR.
      MOVE-CORRESPONDING S_MSG TO S_BLOCK.
      ROLLBACK WORK.
    ELSE.
      CLEAR S_MSG.
      S_BLOCK-MSGID = C_MSGID_M8.
      S_BLOCK-MSGTY = C_MSGTY_SUCCESS.
      S_BLOCK-MSGNO = '327'.
      S_BLOCK-BSTAT = C_BSTAT_OK.
    ENDIF.

    MODIFY IT_BLOCK FROM S_BLOCK.
  ENDLOOP.

ENDFORM.                    "abrechnen

*---------------------------------------------------------------------*
*       FORM BELEGART_BESTIMMEN                                       *
*---------------------------------------------------------------------*
*       bestimmt Belegart des Rechnungsbeleges anhand der Transaktion *
*---------------------------------------------------------------------*
*  <--- BLART Belegart
*---------------------------------------------------------------------*
FORM BELEGART_BESTIMMEN CHANGING BLART    LIKE T169F-BLART
                                 S_MSG    TYPE TYP_MSG
                                 X_FEHLER TYPE C.
  DATA: S_T169F LIKE T169F.

* lese Belegart aus T169F
  CALL FUNCTION 'MRM_DBTAB_T169F_READ'
    EXPORTING
      I_TCODE       = C_TCODE_MRKO
    IMPORTING
      E_T169F       = S_T169F
    EXCEPTIONS
      ERROR_MESSAGE = 1.

  IF ( SY-SUBRC <> 0 ).
    PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
    X_FEHLER = X.
    EXIT.
  ENDIF.
  BLART = S_T169F-BLART.

ENDFORM.                    "belegart_bestimmen

*---------------------------------------------------------------------*
*      Form BLOCK_ABRECHNEN
*---------------------------------------------------------------------*
*      Bucht für einen 'Block' von Warenentnahmen genau eine
*      Rechnung. Alle WEs befinden sich in IT_RKWA.
*      Alle WEs sind demselben Buchungskreis/Kreditor BUKRS/LIFNR
*      zugeordnet.
*      Auf die RKWA wird ein DB-Update durchgeführt (-> MR01)
* ---> BUKRS, LIFNR, BWAER: Block
*      BLART:        Belegart
* <--> IT_RKWA:      Warenentnahmen (es werden BELNR und BUZEI gefüllt)
* <--- IT_MSG:       Meldungen
*      BELNR, GJAHR: Rechnungsbeleg
*---------------------------------------------------------------------*
FORM BLOCK_ABRECHNEN TABLES IT_RKWA  TYPE TYP_TAB_RKWA  "<-->
                      USING BUKRS    LIKE RKWA-BUKRS    "--->
                            BWAER    LIKE RKWA-BWAER    "--->
                            PR_TAX
                            PR_SGTXT TYPE SGTXT
                            PR_BUDAT TYPE BUDAT
                            PR_BLDAT  TYPE BLDAT
                   CHANGING S_MSG    TYPE TYP_MSG
                            X_FEHLER TYPE C
                            BLART    LIKE T169F-BLART
                            LIFNR    LIKE RKWA-LIFNR
                            BELNR    LIKE RKWA-BELNR    "<---
                            GJAHR    LIKE RKWA-GJAHR.   "<---

  DATA: TAB_BKPF       TYPE TYP_TAB_BKPF,
        TAB_BSEG       TYPE TYP_TAB_BSEG,
        TAB_BSET       TYPE TYP_TAB_BSET,
        IT_TAX         TYPE TYP_TAB_TAX,
        TAB_RBWS       TYPE TYP_TAB_RBWS,
        S_BKPF      LIKE BKPF,
        S_BSEG      LIKE BSEG,
        S_RKWA      LIKE RKWA,
        S_TAX       TYPE TYP_TAX,
        S_T001      LIKE T001,
        GSBER         LIKE BSEG-GSBER,
        SGTXT         LIKE RBSEG-SGTXT,
        WRBTR_TMP     LIKE BSEG-WRBTR, "tempor?r benutzter Betrag
        X_UEBERNEHMEN TYPE C,
        F_MONAT       LIKE BKPF-MONAT,
        S_T003        LIKE T003,
        LIFNR_RKWA    LIKE RKWA-LIFNR.

*----- Vorbereitungen -------------------------------------------------

  PERFORM LOCK_RKWA TABLES    IT_RKWA
                    CHANGING  X_FEHLER
                              S_MSG.

  IF X_FEHLER EQ 'X'.
    EXIT.
  ENDIF.

* re-read RKWA after the lock was set
* Is the RKWA entry still not settled?
  LOOP AT IT_RKWA INTO S_RKWA.
    SELECT SINGLE * FROM RKWA
      WHERE MBLNR = S_RKWA-MBLNR
        AND MJAHR = S_RKWA-MJAHR
        AND ZEILE = S_RKWA-ZEILE
        AND STATUS = C_STATUS_NINV. "not settled
    IF SY-SUBRC NE 0.
      IF 1 = 2.
        MESSAGE E863(M8).
*       The material document is already settled.
      ENDIF.
      CLEAR S_MSG.
      S_MSG-MSGID = C_MSGID_M8.
      S_MSG-MSGTY = C_MSGTY_ERROR.
      S_MSG-MSGNO = '863'.
      S_MSG-MSGV1 = S_RKWA-MBLNR.
      S_MSG-MSGV2 = S_RKWA-MJAHR.
      X_FEHLER = X.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF X_FEHLER EQ 'X'.
    EXIT.
  ENDIF.

* bestimme W?hrungsschlüssel aus T001
  CALL FUNCTION 'FI_COMPANY_CODE_DATA'
    EXPORTING
      I_BUKRS = BUKRS
    IMPORTING
      E_T001  = S_T001
    EXCEPTIONS
      OTHERS  = 1.
  IF ( SY-SUBRC <> 0 ).
    PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
    X_FEHLER = X.
    EXIT.                              "---------->
  ENDIF.

* table IT_TAX fill
  PERFORM MWSKZ_TXJCD_PREFETCH TABLES IT_RKWA   "--->
                                      IT_TAX    "<---
                                USING LIFNR     "--->
                             CHANGING X_FEHLER  "<---
                                      S_MSG.    "<---
  IF ( X_FEHLER = X ).
    EXIT.                              "---------->
  ENDIF.


*----- Rechnungsdaten füllen -------------------------------------------

  CLEAR: TAB_BKPF,
         TAB_BSEG,
         TAB_BSET,
         TAB_RBWS,
         S_T003,
         LIFNR_RKWA.

  LIFNR_RKWA = LIFNR.

  IF ( X_FEHLER = X ).
    EXIT.
  ENDIF.


*--- check Document type
  PERFORM DOCUMENT_TYPE_CHECK USING BLART
                           CHANGING S_T003
                                    X_FEHLER
                                    S_MSG.

  IF ( X_FEHLER = X ).
    EXIT.
  ENDIF.

* header data
  PERFORM HEADER_DATA_FILL TABLES TAB_BKPF
                            USING BLART
                                  BUKRS
                                  BWAER
                                  PR_BUDAT
                                  PR_BLDAT
                                  S_T001
                         CHANGING GJAHR
                                  F_MONAT.

* item data
  PERFORM ITEM_DATA_FILL TABLES TAB_BKPF
                                TAB_BSEG
                                TAB_BSET
                                IT_RKWA
                                IT_TAX
                                TAB_RBWS
                          USING BUKRS
                                GJAHR
                                LIFNR_RKWA
                                BWAER
                                S_T001
                                S_T003
                                PR_TAX"税额
                                PR_SGTXT"供应商发票号码
                       CHANGING X_FEHLER
                                S_MSG
                                LIFNR. " Zentrale als Rechnungssteller
  IF ( X_FEHLER = X ).
    EXIT.
  ENDIF.

  PERFORM VENDOR_TAXNUM_CHECK TABLES IT_RKWA
                               USING S_T001
                                     LIFNR
                            CHANGING X_FEHLER
                                     S_MSG.

  IF ( X_FEHLER = X ).
    EXIT.
  ENDIF.

  PERFORM NULL_LINES_CHECK TABLES TAB_BSEG
                         CHANGING X_FEHLER
                                  S_MSG.

  IF ( X_FEHLER = X ).
    EXIT.
  ENDIF.

  PERFORM INVOICE_POST TABLES TAB_BKPF
                              TAB_BSEG
                              TAB_BSET
                              IT_RKWA
                              IT_TAX
                              TAB_RBWS
                        USING BLART
                              GJAHR
                              F_MONAT
                              BUKRS
                              LIFNR_RKWA
                              S_T003
                     CHANGING BELNR
                              X_FEHLER
                              S_MSG.

  IF ( X_FEHLER = X ).
    EXIT.
  ENDIF.

ENDFORM.                    "block_abrechnen


*---------------------------------------------------------------------*
*       FORM EKORG_BESTIMMEN                                          *
*---------------------------------------------------------------------*
*  -->  LIFNR, WERKS, MATNR, SOBKZ (Sonderbestandskennzeichen)
*  <--  EKORG: ermittelte Einkaufsorganisation,
*              wurde keines ermittelt, so ist es SPACE
*---------------------------------------------------------------------*
FORM EKORG_BESTIMMEN USING WERKS LIKE RKWA-WERKS
                           SOBKZ LIKE RKWA-SOBKZ
                  CHANGING EKORG LIKE EINE-EKORG.

  DATA: X_KONSI TYPE C,                "->ME_PRICING_TAX_INDICATOR
        X_PIPEL TYPE C.                "dito

  IF ( SOBKZ = C_SOBKZ_KONS ).
    X_KONSI = X.
    X_PIPEL = SPACE.
  ELSE.
    X_KONSI = SPACE.
    X_PIPEL = X.
  ENDIF.

* berechne EKORG
  CALL FUNCTION 'ME_SELECT_EKORG_FOR_PLANT'
    EXPORTING
      I_WERKS          = WERKS
      I_PIPEL          = X_PIPEL
      I_KONSI          = X_KONSI
    IMPORTING
      E_EKORG          = EKORG
    EXCEPTIONS
      NO_ENTRY_FOUND   = 1
      NO_DEFAULT_FOUND = 2.

  IF ( SY-SUBRC <> 0 ).
    CLEAR EKORG.
  ENDIF.

ENDFORM.                    "ekorg_bestimmen
*---------------------------------------------------------------------*
*       FORM MWSKZ_BESTIMMEN                                          *
*---------------------------------------------------------------------*
*       Bestimmt das Steuerkennzeichen über den Einkaufsinfosatz      *
*---------------------------------------------------------------------*
*  -->  LIFNR, WERKS, MATNR, SOBKZ (Sonderbestandskennzeichen)
*  <--  MWSKZ: ermitteltes SteuerKz, wurde keines ermittelt, so ist
*              es SPACE
*---------------------------------------------------------------------*
FORM MWSKZ_BESTIMMEN USING LIFNR LIKE RKWA-LIFNR
                           WERKS LIKE RKWA-WERKS
                           MATNR LIKE RKWA-MATNR
                           SOBKZ LIKE RKWA-SOBKZ
                           EKORG LIKE EINE-EKORG
                  CHANGING MWSKZ LIKE EINE-MWSKZ.

  DATA: S_MEICO LIKE MEICO,
        S_EINE  LIKE EINE,
        ESOKZ   LIKE EINE-ESOKZ.       "Typ des Infosatzes

  IF ( SOBKZ = C_SOBKZ_KONS ).
    ESOKZ = C_ESOKZ_KONS.
  ELSE.
    ESOKZ = C_ESOKZ_PIPE.
  ENDIF.

* Konsi-/Pipeline-Infosatz lesen
  CLEAR S_MEICO.
  S_MEICO-ESOKZ = ESOKZ.
  S_MEICO-EKORG = EKORG.
  S_MEICO-WERKS = WERKS.
  S_MEICO-LIFNR = LIFNR.
  S_MEICO-MATNR = MATNR.
  S_MEICO-ONLYE = SPACE.               "sonst geht's nicht!

*{   INSERT
*Note 663537: Variants don't have info records! Look for generic article
  PERFORM WRF_READ_INFORECORD_SA_PREPARE CHANGING S_MEICO.
*}   INSERT

  CALL FUNCTION 'ME_READ_INFORECORD'
    EXPORTING
      INCOM         = S_MEICO
    IMPORTING
      EINEDATEN     = S_EINE
    EXCEPTIONS
      ERROR_MESSAGE = 1          "nicht auswerten!
      OTHERS        = 2.

  IF ( SY-SUBRC = 0 ).
    MWSKZ = S_EINE-MWSKZ.
  ELSE.
    CLEAR MWSKZ.
  ENDIF.

ENDFORM.                    "mwskz_bestimmen
*---------------------------------------------------------------------*
*   Form MWSKZ_TXJCD_PREFETCH
*---------------------------------------------------------------------*
*   berechne vorsorglich SteuerKz & Tax Jurisdiction Code für alle
*   vorkommenden Kombinationen von Material, Werk & Lieferant.
*   Die StKz werden in IT_tax abgelegt
*   ---> BUKRS, LIFNR, IT_RKWA: Block
*   <--- IT_TAX:   Steuerkennzeichen
*        X_FEHLER: Fehler aufgetreten?
*        S_MSG:    Meldung
*---------------------------------------------------------------------*
FORM MWSKZ_TXJCD_PREFETCH TABLES IT_RKWA  TYPE TYP_TAB_RKWA
                                 IT_TAX   TYPE TYP_TAB_TAX
                           USING LIFNR    LIKE BSEG-LIFNR
                        CHANGING X_FEHLER TYPE C
                                 S_MSG    TYPE TYP_MSG.

  DATA: S_T001W LIKE T001W,
        S_RKWA  LIKE RKWA,
        S_TAX   TYPE TYP_TAX,
        EKORG   LIKE EINE-EKORG.

  X_FEHLER = SPACE.
  CLEAR IT_TAX.

* für alle RKWA-Eintr?ge: bestimme StKz + TaxJcd
  LOOP AT IT_RKWA INTO S_RKWA.

    CLEAR S_TAX.
    S_TAX-MATNR = S_RKWA-MATNR.
    S_TAX-WERKS = S_RKWA-WERKS.
    S_TAX-LIFNR = LIFNR.

*   bestimme Tax Jurisdiction Code über das Werk
    CALL FUNCTION 'LOCATION_SELECT_PLANT'
      EXPORTING
        I_WERKS       = S_RKWA-WERKS
      IMPORTING
        O_T001W       = S_T001W
      EXCEPTIONS
        ERROR_MESSAGE = 1
        OTHERS        = 2.

    IF ( SY-SUBRC <> 0 ).
      PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
      X_FEHLER = X.
      EXIT.                            "---------->
    ENDIF.
    S_TAX-TXJCD = S_T001W-TXJCD.

*   bestimme Steuerkennzeichen ...
    IF S_RKWA-MWSKZ IS INITIAL.        "vor 4.5A nicht in der RKWA
*   ... über Infosatz
      PERFORM EKORG_BESTIMMEN USING S_RKWA-WERKS
                                    S_RKWA-SOBKZ
                           CHANGING EKORG.

      IF ( EKORG = SPACE ).
        S_TAX-MWSKZ = SPACE.
      ELSE.
        PERFORM MWSKZ_BESTIMMEN USING LIFNR
                                      S_RKWA-WERKS
                                      S_RKWA-MATNR
                                      S_RKWA-SOBKZ
                                      EKORG
                             CHANGING S_TAX-MWSKZ.
      ENDIF.

      IF ( S_TAX-MWSKZ = SPACE ).      "kein StKz bestimmt
        CLEAR S_MSG.
        S_MSG-MSGID = C_MSGID_M8.
        S_MSG-MSGTY = C_MSGTY_ERROR.
        S_MSG-MSGNO = '732'.
        S_MSG-MSGV1 = LIFNR.
        S_MSG-MSGV2 = S_RKWA-MATNR.
        S_MSG-MSGV3 = S_RKWA-WERKS.
        S_MSG-MSGV4 = EKORG.
        X_FEHLER = X.
        EXIT.                          "---------->
      ENDIF.
      S_RKWA-MWSKZ = S_TAX-MWSKZ.
      MODIFY IT_RKWA FROM S_RKWA TRANSPORTING MWSKZ.
    ELSE.
*   ... über Eintrag in der RKWA
      S_TAX-MWSKZ = S_RKWA-MWSKZ.      "ab 4.5A in der RKWA
    ENDIF.

    COLLECT S_TAX INTO IT_TAX.
  ENDLOOP.                             "at it_rkwa

ENDFORM.                    "mwskz_txjcd_prefetch
*&---------------------------------------------------------------------*
*&      Form  INVOICE_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM INVOICE_POST TABLES    TI_BKPF  TYPE TYP_TAB_BKPF
                            TI_BSEG  TYPE TYP_TAB_BSEG
                            TI_BSET  TYPE TYP_TAB_BSET
                            TI_RKWA  TYPE TYP_TAB_RKWA
                            TI_TAX   TYPE TYP_TAB_TAX
                            TI_RBWS TYPE TYP_TAB_RBWS
                   USING    I_BLART  LIKE T169F-BLART
                            I_GJAHR  LIKE RKWA-GJAHR
                            I_MONAT  LIKE BKPF-MONAT
                            I_BUKRS  LIKE RKWA-BUKRS
                            I_LIFNR_RKWA  LIKE RKWA-LIFNR
                            S_T003   LIKE T003
                   CHANGING E_BELNR  LIKE RKWA-BELNR
                            X_FEHLER TYPE C
                            S_MSG    TYPE TYP_MSG.

  DATA: TAB_ACCHD LIKE ACCHD OCCURS 0 WITH HEADER LINE,
        TAB_ACCIT LIKE ACCIT OCCURS 0 WITH HEADER LINE,
        TAB_ACCCR LIKE ACCCR OCCURS 0 WITH HEADER LINE,
        TAB_ACCBSET LIKE ACCBSET OCCURS 0 WITH HEADER LINE,
        TAB_ACCIT_WT LIKE ACCIT_WT OCCURS 0 WITH HEADER LINE,
        S_BSET    LIKE BSET,
        S_ACCBSET LIKE ACCBSET,
        LS_RKWA_KEY      TYPE RKWA_KEY,                     "1731546
        LS_RKWA_KEY_OLD  TYPE RKWA_KEY,                     "1731546
        F_ZEILE          TYPE ACCTIT-ZEILE,                 "1731546
        F_LENGTH         TYPE I,                            "1731546
        F_AWTYP   LIKE ACCHD-AWTYP,
        F_AWREF   LIKE ACCHD-AWREF,
        F_AWORG   LIKE ACCHD-AWORG,
        F_AWSYS   LIKE ACCHD-AWSYS,
        F_HWAE1   LIKE BOOLE,
        F_VENDOR_LINE TYPE C.
*ENHANCEMENT-POINT RMVKON00_02 SPOTS ES_RMVKON00.

* convert document into accounting structure
* hier werden nur Zeilen mit Curtp = 00 Belegw?hrung erzeugt *
* die restlichen W?hrungszeilen wie Hausw?rhung werden durch *
* ac_doc_create erzeugt
  CALL FUNCTION 'FI_DOC_TO_ACC_TRANSFORM'
    EXPORTING
      X_HWAE1      = F_HWAE1
    TABLES
      T_BKPF       = TI_BKPF
      T_BSEG       = TI_BSEG
      T_ACCHD      = TAB_ACCHD
      T_ACCIT      = TAB_ACCIT
      T_ACCCR      = TAB_ACCCR
    EXCEPTIONS
      SYSTEM_ERROR = 1.

  IF SY-SUBRC <> 0.
    PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
    X_FEHLER = X.
    EXIT.
  ENDIF.

* Fill ZEILE in TAB_ACCIT in order to enable the FI document split in
* subroutine DOCUMENT_SPLIT_INVOICE_RECEIPT (include LFACIFSP).
* The document split is performed for the following account types:
* material (M), G/L accounts (S) and Assets (A).
  LOOP AT TAB_ACCIT WHERE KOART CA 'MSA'                    "1731546
                    AND   ZUONR IS NOT INITIAL              "1731546
                    AND   SGTXT IS NOT INITIAL.             "1731546

    LS_RKWA_KEY-MBLNR = TAB_ACCIT-ZUONR(10).                "1731546
    LS_RKWA_KEY-MJAHR = TAB_ACCIT-ZUONR+10(4).              "1731546

*   Last 4 character of SGTXT contain RKWA-ZEILE
    F_LENGTH = STRLEN( TAB_ACCIT-SGTXT ) - 4.               "1731546
    LS_RKWA_KEY-ZEILE = TAB_ACCIT-SGTXT+F_LENGTH(4).        "1731546

*   Check whether the material document reference is valid      "1731546
    READ TABLE TI_RKWA WITH KEY MJAHR = LS_RKWA_KEY-MJAHR   "1818133
                                MBLNR = LS_RKWA_KEY-MBLNR   "1818133
                                ZEILE = LS_RKWA_KEY-ZEILE   "1731546
                                BINARY SEARCH               "1731546
                       TRANSPORTING NO FIELDS.              "1731546
    IF SY-SUBRC = 0.                                        "1731546
      IF LS_RKWA_KEY NE LS_RKWA_KEY_OLD.                    "1731546
        LS_RKWA_KEY_OLD = LS_RKWA_KEY.                      "1731546
        F_ZEILE = F_ZEILE + 1.                              "1731546
      ENDIF.                                                "1731546
      TAB_ACCIT-ZEILE = TAB_ACCIT-URZEILE = F_ZEILE.        "1731546
      MODIFY TAB_ACCIT TRANSPORTING ZEILE URZEILE.          "1731546
    ENDIF.                                                  "1731546
  ENDLOOP.                                                  "1731546

* convert tax data into accounting structure
  LOOP AT TI_BSET INTO S_BSET.
    MOVE-CORRESPONDING S_BSET TO S_ACCBSET.
    S_ACCBSET-MONAT = I_MONAT.
    S_ACCBSET-BLART = I_BLART.
    CLEAR: S_ACCBSET-HWBAS,
           S_ACCBSET-HWSTE.
    APPEND S_ACCBSET TO TAB_ACCBSET.
  ENDLOOP.

  LOOP AT TAB_ACCIT WHERE POSNR = 1.
*-- withholding tax flag in table accit
    IF NOT TI_RBWS[] IS INITIAL.
      TAB_ACCIT-WT_KEY = '000001'.
    ENDIF.

    MODIFY TAB_ACCIT.
  ENDLOOP.

  PERFORM WITHHOLDING_TAX_CREATE TABLES TAB_ACCIT_WT
                                        TI_RBWS.

* alle Information aus den Sachkonten, Stamms?tzen, Customizingseins
* tellungen werden hier abgeleitet, wir brauchen es nicht zu machen

  CALL FUNCTION 'AC_DOCUMENT_CREATE'
    TABLES
      T_ACCHD       = TAB_ACCHD
      T_ACCIT       = TAB_ACCIT
      T_ACCCR       = TAB_ACCCR
      T_ACCTX       = TAB_ACCBSET
      T_ACCWT       = TAB_ACCIT_WT
    EXCEPTIONS
      ERROR_MESSAGE = 1.

  IF SY-SUBRC <> 0.
    PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
    X_FEHLER = X.
    EXIT.
  ENDIF.

  PERFORM DOCUMENT_NUMBER_GET USING I_BLART
                                    I_BUKRS
                                    I_GJAHR
                                    S_T003
                           CHANGING E_BELNR
                                    X_FEHLER
                                    S_MSG.

  CALL FUNCTION 'FI_REFERENCE_CREATE'
    EXPORTING
      I_AWTYP = 'BKPFF'
      I_BUKRS = I_BUKRS
      I_GJAHR = I_GJAHR
      I_BELNR = E_BELNR
    IMPORTING
      E_AWTYP = F_AWTYP
      E_AWREF = F_AWREF
      E_AWORG = F_AWORG
      E_AWSYS = F_AWSYS.


  CALL FUNCTION 'AC_DOCUMENT_POST'
    EXPORTING
      I_AWTYP       = F_AWTYP
      I_AWREF       = F_AWREF
      I_AWORG       = F_AWORG
      I_AWSYS       = F_AWSYS
    EXCEPTIONS
      ERROR_MESSAGE = 1.

  IF SY-SUBRC <> 0.
    PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
    X_FEHLER = X.
    EXIT.
  ENDIF.

  READ TABLE TI_BSEG WITH KEY KOART = C_KOART_KRED.
  IF TI_BSEG-WRBTR > 0.
    F_VENDOR_LINE = X.
  ELSE.
    CLEAR F_VENDOR_LINE.
  ENDIF.

  PERFORM RKWA_UPDATE TABLES    TI_RKWA
                                TI_TAX
                       USING    E_BELNR
                                I_GJAHR
                                I_LIFNR_RKWA
                                F_VENDOR_LINE
                       CHANGING X_FEHLER
                                S_MSG.

  IF X_FEHLER = X.
    EXIT.
  ENDIF.

  PERFORM HEAD_MESSAGE_CREATE TABLES TI_BKPF
                                     TI_BSEG
                            USING    I_BLART
                                     E_BELNR
                                     I_BUKRS
                            CHANGING X_FEHLER
                                     S_MSG.

  IF X_FEHLER = X.
    EXIT.
  ENDIF.

ENDFORM.                               " INVOICE_POST

*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_NUMBER_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DOCUMENT_NUMBER_GET USING I_BLART  LIKE T169F-BLART
                               I_BUKRS  LIKE RKWA-BUKRS
                               I_GJAHR  LIKE RKWA-GJAHR
                               S_T003   LIKE T003
                      CHANGING E_BELNR  LIKE RKWA-BELNR
                               X_FEHLER TYPE C
                               S_MSG    TYPE TYP_MSG.


  CALL FUNCTION 'RF_GET_DOCUMENT_NUMBER'
    EXPORTING
      COMPANY         = I_BUKRS
      RANGE           = S_T003-NUMKR
      YEAR            = I_GJAHR
    IMPORTING
      DOCUMENT_NUMBER = E_BELNR.

  IF SY-DBSYS NE 'ORACLE'.
    CALL FUNCTION 'ENQUEUE_EFBKPF'
      EXPORTING
        BELNR          = E_BELNR
        BUKRS          = I_BUKRS
        GJAHR          = I_GJAHR
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2.
    CASE SY-SUBRC.

*------- Beleg von anderem Benutzer gesperrt ---------------------------
      WHEN 1.
*        MESSAGE A127 WITH E_BELNR I_BUKRS.
        PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
        X_FEHLER = X.
        EXIT.

*------- Systemfehler --------------------------------------------------
      WHEN 2.
*        MESSAGE A128 WITH E_BELNR I_BUKRS.
        PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
        X_FEHLER = X.
        EXIT.
    ENDCASE.
  ENDIF.

ENDFORM.                               " DOCUMENT_NUMBER_GET
*&---------------------------------------------------------------------*
*&      Form  HEADER_DATA_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HEADER_DATA_FILL TABLES TI_BKPF  TYPE TYP_TAB_BKPF
                       USING I_BLART  LIKE T169F-BLART
                             I_BUKRS  LIKE RKWA-BUKRS
                             I_BWAER  LIKE RKWA-BWAER
                             I_BUDAT  TYPE BUDAT
                             I_BLDAT  TYPE BLDAT
                             S_T001   LIKE T001
                    CHANGING E_GJAHR  LIKE RKWA-GJAHR
                             E_MONAT  LIKE BKPF-MONAT.


  DATA: S_BKPF LIKE BKPF.

  CLEAR: S_BKPF.
  S_BKPF-MANDT = SY-MANDT.
  S_BKPF-BLART = I_BLART.
  S_BKPF-BUKRS = I_BUKRS.
  S_BKPF-TCODE = TCODE_MRKO.
  S_BKPF-WAERS = I_BWAER.
  S_BKPF-HWAER = S_T001-WAERS.
  S_BKPF-XMWST = 'X'.                  "Steuer rechnen
*  S_BKPF-BLDAT = SY-DATLO.             "lokales Datum
  S_BKPF-BLDAT = I_BLDAT.             "传入凭证日期
*  S_BKPF-BUDAT = SY-DATLO.             "lokales Datum
  S_BKPF-BUDAT = I_BUDAT.             "传入过帐日期
  S_BKPF-WWERT = SY-DATLO.             "lokales Datum

  S_BKPF-AWTYP = C_AWTYP_BKPFF.
  S_BKPF-GLVOR = C_AWTYP_RMRP.
  S_BKPF-USNAM = SY-UNAME.
  S_BKPF-XSNET = 'X'.

* month, year get
  IF NOT S_BKPF-BUKRS IS INITIAL.

    CALL FUNCTION 'FI_PERIOD_DETERMINE'
      EXPORTING
        I_BUDAT = S_BKPF-BUDAT
        I_BUKRS = S_BKPF-BUKRS
        I_MONAT = S_BKPF-MONAT
      IMPORTING
        E_GJAHR = S_BKPF-GJAHR
        E_MONAT = S_BKPF-MONAT.

  ENDIF.

  E_GJAHR = S_BKPF-GJAHR.
  E_MONAT = S_BKPF-MONAT.

*-------- Steuerkurs -------------------------------------------------*
  CALL FUNCTION 'FI_TAX_GET_TXKRS'
    EXPORTING
      I_BUKRS      = I_BUKRS
      I_CURR_FORGN = I_BWAER
      I_CURR_LOCAL = S_T001-WAERS
      I_BLDAT      = SY-DATLO
      I_BUDAT      = SY-DATLO
*     i_vatdate    = ????                                   "N1023317
    IMPORTING
      E_TXKRS      = S_BKPF-TXKRS.

  APPEND S_BKPF TO TI_BKPF.

ENDFORM.                               " HEADER_DATA_FILL

*&---------------------------------------------------------------------*
*&      Form  ITEM_DATA_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ITEM_DATA_FILL TABLES TI_BKPF TYPE TYP_TAB_BKPF
                           TI_BSEG TYPE TYP_TAB_BSEG
                           TI_BSET TYPE TYP_TAB_BSET
                           TI_RKWA TYPE TYP_TAB_RKWA
                           TI_TAX  TYPE TYP_TAB_TAX
                           TE_RBWS TYPE TYP_TAB_RBWS
                     USING I_BUKRS LIKE RKWA-BUKRS
                           I_GJAHR LIKE RKWA-GJAHR
                           I_LIFNR_RKWA LIKE RKWA-LIFNR
                           I_BWAER LIKE RKWA-BWAER
                           S_T001 LIKE T001
                           S_T003 LIKE T003
                           I_TAX  "税额
                           I_SGTXT  TYPE SGTXT  "供应商发票号码
                  CHANGING X_FEHLER TYPE C
                           S_MSG  TYPE TYP_MSG
                           E_LIFNR LIKE RKWA-LIFNR.

  DATA: T_BSEGT           LIKE BSEGT OCCURS 0,
        S_BSEGT           LIKE BSEGT,
        S_BSEG            LIKE BSEG,
        S_BSET            LIKE BSET,
        S_RKWA            LIKE RKWA,
        S_TAX             TYPE TYP_TAX,
        S_BKPF            LIKE BKPF,
        S_T169            LIKE T169,
        H_SGTXT           LIKE BSEG-SGTXT,
        H_GSBER           LIKE RKWA-GSBER,
        F_BUZEI           LIKE BSEG-BUZEI,
        F_FWSTE           LIKE ITXDAT-FWSTE,
        F_WRBTR           LIKE RKWA-WRBTR,
        F_ISPS_NOT_ACTIVE LIKE FMISPS-PS_ACTIVE.
  DATA: L_WRBTR           TYPE WRBTR,
        L_NAME1           TYPE NAME1.

  F_BUZEI = 1.

  CONSTANTS: C_T169(4)    VALUE 'T169'.

  CLEAR: X_FEHLER.
  CLEAR: S_MSG.

* selections
  SELECT SINGLE * FROM T169 INTO S_T169
                  WHERE TCODE = C_TCODE_MRKO.
  IF SY-SUBRC NE 0.
    MESSAGE E100 WITH C_T169 C_TCODE_MRKO.
  ENDIF.

* check if IS-PS active
  CALL FUNCTION 'FM00_CHECK_ISPS'
    IMPORTING
      E_FLG_ISPS_NOT_ACTIVE = F_ISPS_NOT_ACTIVE.

* header data
  READ TABLE TI_BKPF INDEX 1 INTO S_BKPF.

* bseg fill vendor line
  CLEAR: S_BSEG, S_RKWA, F_WRBTR.
  LOOP AT TI_RKWA INTO S_RKWA.
    IF S_RKWA-SHKZG = C_SHKZG_HABEN.
      F_WRBTR = F_WRBTR + ( S_RKWA-WRBTR - S_RKWA-NAVNW ).
    ELSE.
      F_WRBTR = F_WRBTR - ( S_RKWA-WRBTR - S_RKWA-NAVNW ).
    ENDIF.
  ENDLOOP.

  S_BSEG-MANDT = SY-MANDT.
  S_BSEG-BUKRS = I_BUKRS.
  S_BSEG-GJAHR = S_BKPF-GJAHR.
  S_BSEG-BUZEI = C_BUZEI_KRED.
  S_BSEG-LIFNR = E_LIFNR.
  S_BSEG-ZFBDT = SY-DATLO.
  S_BSEG-WRBTR = ABS( F_WRBTR ).

* posting key
  IF F_WRBTR >= 0.
    S_BSEG-BSCHL = C_BSLKH.
  ELSE.
    S_BSEG-BSCHL = C_BSLKS.
  ENDIF.
  PERFORM POSTING_KEY_DATA_GET USING    S_T003
                                        S_BSEG-BSCHL
                               CHANGING S_BSEG-SHKZG
                                        S_BSEG-KOART
                                        X_FEHLER
                                        S_MSG.
  IF ( X_FEHLER = X ).
    EXIT.
  ENDIF.

  PERFORM VENDOR_DATA_GET USING    I_BUKRS
                                   E_LIFNR
                          CHANGING X_FEHLER
                                   S_MSG
                                   S_BSEG-LIFNR
                                   S_BSEG-FILKD
                                   S_BSEG-ZTERM
                                   S_BSEG-QSSKZ.

  IF ( X_FEHLER = X ).
    EXIT.
  ENDIF.
  E_LIFNR = S_BSEG-LIFNR.
  PERFORM BUZID_GET USING    S_T169-VGART
                             S_BSEG-KOART
                    CHANGING S_BSEG-BUZID
                             S_BSEG-BUSTW.

  PERFORM KTOSL_GET USING    S_BSEG-BUSTW
                             S_BSEG-BUKRS
                    CHANGING S_BSEG-KTOSL.

  PERFORM PAYMENT_CONDITIONS_GET USING    S_BSEG-ZTERM
                                          S_BKPF-BLDAT
                                          S_BKPF-BUDAT
                                 CHANGING S_BSEG-ZFBDT
                                          S_BSEG-ZLSPR
                                          S_BSEG-ZLSCH
                                          S_BSEG-ZBD1T
                                          S_BSEG-ZBD1P
                                          S_BSEG-ZBD2T
                                          S_BSEG-ZBD2P
                                          S_BSEG-ZBD3T
                                          X_FEHLER
                                          S_MSG.
  IF ( X_FEHLER = X ).
    EXIT.
  ENDIF.

  APPEND S_BSEG TO TI_BSEG.

*   withholding tax initialization
  IF TE_RBWS[] IS INITIAL.
    CALL FUNCTION 'FI_WT_ERS_INITIALIZATION'
      EXPORTING
        I_BUKRS = S_BKPF-BUKRS
        I_GJAHR = S_BKPF-GJAHR
        I_BLDAT = S_BKPF-BLDAT
        I_BUDAT = S_BKPF-BUDAT
        I_WWERT = S_BKPF-WWERT
        I_WAERS = S_BKPF-WAERS
        I_KURSF = S_BKPF-KURSF
        I_SHKZG = S_BSEG-SHKZG
        I_LIFNR = S_BSEG-LIFNR
      TABLES
        I_RBWS  = TE_RBWS.
  ENDIF.

* bseg fill (goods extract)
  LOOP AT TI_RKWA INTO S_RKWA.
    F_BUZEI = F_BUZEI + 1.
    READ TABLE TI_TAX INTO S_TAX
                      WITH KEY MATNR = S_RKWA-MATNR
                               WERKS = S_RKWA-WERKS
                               MWSKZ = S_RKWA-MWSKZ
                               LIFNR = I_LIFNR_RKWA.
    IF ( SY-SUBRC <> 0 ).
      S_MSG-MSGID = 'XX'.
      S_MSG-MSGTY =  C_MSGTY_ERROR.
      S_MSG-MSGNO = '300'.             "Verbuchungsabbruch
      S_MSG-MSGV1 = TEXT-300.
      X_FEHLER = X.
      EXIT.
    ENDIF.

    CLEAR S_BSEG.
    S_BSEG-MANDT = SY-MANDT.
    S_BSEG-BUKRS = I_BUKRS.
    S_BSEG-GJAHR = S_BKPF-GJAHR.
    S_BSEG-BUZEI = F_BUZEI.
    S_BSEG-VORGN = C_AWTYP_RMRP.
    S_BSEG-BUZID = C_BUZID_SACH.
    S_BSEG-MWSKZ = S_TAX-MWSKZ.
    S_BSEG-TXJCD = S_TAX-TXJCD.
    S_BSEG-SAKNR = S_RKWA-HKONT.
    S_BSEG-HKONT = S_RKWA-HKONT.
    S_BSEG-GSBER = S_RKWA-GSBER.
    S_BSEG-PRCTR = S_RKWA-PRCTR.       "ab 4.5A Eintrag in der RKWA
    S_BSEG-MENGE = S_RKWA-BSTMG.
    S_BSEG-MEINS = S_RKWA-BSTME.
    S_BSEG-MATNR = S_RKWA-MATNR.   " matnr and werks are important for
    S_BSEG-WERKS = S_RKWA-WERKS.       " external tax system

* posting key in
    IF S_RKWA-SHKZG = C_SHKZG_HABEN.
      S_BSEG-BSCHL = C_BSLSS.
    ELSEIF S_RKWA-SHKZG = C_SHKZG_SOLL.
      S_BSEG-BSCHL = C_BSLSH.
    ENDIF.

    PERFORM POSTING_KEY_DATA_GET USING    S_T003
                                          S_BSEG-BSCHL
                                 CHANGING S_BSEG-SHKZG
                                          S_BSEG-KOART
                                          X_FEHLER
                                          S_MSG.
    IF ( X_FEHLER = X ).
      EXIT.
    ENDIF.

    PERFORM BUZID_GET USING    S_T169-VGART
                               S_BSEG-KOART
                      CHANGING S_BSEG-BUZID
                               S_BSEG-BUSTW.

    PERFORM KTOSL_GET USING    S_BSEG-BUSTW
                               S_BSEG-BUKRS
                      CHANGING S_BSEG-KTOSL.

*   ZuordnungsNr: aus MaterialBelegnr + Jahr zusammenbauen
    CONCATENATE S_RKWA-MBLNR S_RKWA-MJAHR INTO S_BSEG-ZUONR.
*ENHANCEMENT-POINT RMVKON00_01 SPOTS ES_RMVKON00.
*   Fill XREF3 if IS-PS is active
    IF F_ISPS_NOT_ACTIVE IS INITIAL.
      MOVE S_BSEG-ZUONR TO S_BSEG-XREF3.
    ENDIF.
*   Positionstext: 'Abrechnung zu Materialbeleg &1 &2'
    H_SGTXT = TEXT-011.
    REPLACE '&1' WITH S_RKWA-MBLNR INTO H_SGTXT.
    REPLACE '&2' WITH S_RKWA-ZEILE INTO H_SGTXT.
    S_BSEG-SGTXT = H_SGTXT.

*   Steuerbetrag ist Null
    S_BSEG-WMWST = 0.
*   Rechnungsbetrag
    S_BSEG-WRBTR = S_RKWA-WRBTR - S_RKWA-NAVNW.
    S_BSEG-TXBFW = S_BSEG-WRBTR.
    S_BSEG-REWWR = S_BSEG-WRBTR.
    S_BSEG-NAVFW = S_RKWA-NAVNW.

* if EA-PS is active take over account assignments
    PERFORM FM_ACCOUNT_ASSIGNMENT_GET USING S_RKWA
                                   CHANGING S_BSEG
                                            X_FEHLER
                                            S_MSG.
    IF ( X_FEHLER = X ).
      EXIT.
    ENDIF.

    APPEND S_BSEG TO TI_BSEG.
    CLEAR S_RKWA.
  ENDLOOP.


* tax data
  PERFORM BSET_FILL TABLES   TI_BKPF
                             TI_BSEG
                             TI_BSET
                    USING    I_BUKRS
                    CHANGING S_MSG
                             X_FEHLER.

  IF ( X_FEHLER = X ).
    EXIT.
  ENDIF.

* bseg fill (tax line)
  CALL FUNCTION 'CREATE_TAX_ITEM'
    TABLES
      T_BSEGT = T_BSEGT.

*—删除多余发票税额行
* START ADD BY HAND_ABAP1 2018/5/24
  DATA:lv_lines TYPE i.
  DESCRIBE TABLE t_bsegt LINES lv_lines.
  IF lv_lines > 1.
    DELETE t_bsegt WHERE mwskz <> 'JC'.
  ENDIF.
* END ADD BY HAND_ABAP1 2018/5/24

***税修改
  READ TABLE T_BSEGT INTO S_BSEGT INDEX 1.
  IF SY-SUBRC = 0.
    S_BSEGT-DMBTR = I_TAX.
    S_BSEGT-WRBTR = I_TAX.
    MODIFY T_BSEGT FROM S_BSEGT INDEX 1.
  ENDIF.
***税修改

  LOOP AT T_BSEGT INTO S_BSEGT.
    S_BSEGT-HWBAS = ABS( S_BSEGT-HWBAS ).
    S_BSEGT-FWBAS = ABS( S_BSEGT-FWBAS ).

    CLEAR: S_BSEG.
    MOVE-CORRESPONDING S_BSEGT TO S_BSEG.
    S_BSEG-VORGN = C_AWTYP_RMRP.
    S_BSEG-BUZID = C_BUZID_TAX.
    S_BSEG-KOART = 'S'.

    APPEND S_BSEG TO TI_BSEG.
  ENDLOOP.

***然后修改T_BSEG表，将金额修改,计算除BUZID = K之外的总值再进行替换
  CLEAR: L_WRBTR.
  LOOP AT TI_BSEG INTO S_BSEG WHERE BUZID <> 'K'.
    IF S_BSEG-SHKZG = 'H'.
      L_WRBTR = L_WRBTR - S_BSEG-WRBTR.
    ELSEIF S_BSEG-SHKZG = 'S'.
      L_WRBTR = L_WRBTR + S_BSEG-WRBTR.
    ENDIF.
  ENDLOOP.

  READ TABLE TI_BSEG INTO S_BSEG WITH KEY BUZID = 'K'.
  IF SY-SUBRC = 0.
    S_BSEG-WRBTR = L_WRBTR.
    S_BSEG-TXBFW = L_WRBTR.
***为K时，将供应商发票号码写入文本中
    S_BSEG-SGTXT = I_SGTXT.
    S_BSEG-WRBTR = ABS( S_BSEG-WRBTR )."将最后的值以正数存储
    S_BSEG-TXBFW = ABS( S_BSEG-TXBFW ).
***20140509CHANGE ORDER原因代码增强
    S_BSEG-RSTGR = '113'.
    MODIFY TI_BSEG FROM S_BSEG INDEX SY-TABIX.
  ENDIF.

***为T时，将供应商发票号码，发票方以及中文描述写入文本中
***格式为供应商发票号码+发票方+名称
***20140212Yanghua邮件更改为‘供应商的名字=供应商代码=发票号’
  READ TABLE TI_BSEG INTO S_BSEG WITH KEY BUZID = 'T'.
  IF SY-SUBRC = 0.
    CLEAR: L_NAME1.
    SELECT SINGLE NAME1 INTO L_NAME1 FROM LFA1 WHERE LIFNR = E_LIFNR.
*    CONCATENATE I_SGTXT '+' E_LIFNR '+' L_NAME1 INTO S_BSEG-SGTXT.
    CONCATENATE  L_NAME1 '=' E_LIFNR '=' I_SGTXT INTO S_BSEG-SGTXT.
***为K时，将供应商发票号码写入文本中
    MODIFY TI_BSEG FROM S_BSEG INDEX SY-TABIX.
  ENDIF.
***

  PERFORM NAV_VERTEILEN TABLES TI_BSEG.

  PERFORM BSEG_LINES_NUMBERING CHANGING TI_BSEG[].

ENDFORM.                               " ITEM_DATA_FILL

*&---------------------------------------------------------------------*
*&      Form  POSTING_KEY_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM POSTING_KEY_DATA_GET USING    S_T003  LIKE T003
                                   E_BSCHL  LIKE BSEG-BSCHL
                          CHANGING E_SHKZG  LIKE BSEG-SHKZG
                                   E_KOART  LIKE BSEG-KOART
                                   X_FEHLER TYPE C
                                   S_MSG  TYPE TYP_MSG.

  DATA: S_TBSL      LIKE TBSL,
        S_TBSLT     LIKE TBSLT.

  CALL FUNCTION 'FI_POSTING_KEY_DATA'
    EXPORTING
      I_BSCHL = E_BSCHL
    IMPORTING
      E_TBSL  = S_TBSL
      E_TBSLT = S_TBSLT.

  IF S_T003-KOARS NA S_TBSL-KOART.
    CLEAR S_MSG.
    S_MSG-MSGID = C_MSGID_M8.
    S_MSG-MSGTY = C_MSGTY_ERROR.
    S_MSG-MSGNO = '161'.
    X_FEHLER = X.
    EXIT.
  ELSE.
    E_SHKZG = S_TBSL-SHKZG.
    E_KOART = S_TBSL-KOART.
  ENDIF.

ENDFORM.                               " POSTING_KEY_DATA_GET

*&---------------------------------------------------------------------*
*&      Form  VENDOR_DATA_GET
*&---------------------------------------------------------------------*
FORM VENDOR_DATA_GET USING    I_BUKRS LIKE RKWA-BUKRS
                              I_LIFNR LIKE RKWA-LIFNR
                     CHANGING X_FEHLER TYPE C
                              S_MSG TYPE TYP_MSG
                              E_LIFNR LIKE BSEG-LIFNR
                              E_FILKD LIKE BSEG-FILKD
                              E_ZTERM LIKE BSEG-ZTERM
                              E_QSSKZ LIKE BSEG-QSSKZ.

  DATA: S_KRED LIKE VF_KRED.
  CLEAR: S_KRED.

*- read the vendor data
  CALL FUNCTION 'FI_VENDOR_DATA'
    EXPORTING
      I_BUKRS       = I_BUKRS
      I_LIFNR       = I_LIFNR
    IMPORTING
      E_KRED        = S_KRED
    EXCEPTIONS
      ERROR_MESSAGE = 01.

  IF SY-SUBRC NE 0.
    PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
    X_FEHLER = X.
    EXIT.
  ENDIF.

  E_QSSKZ = S_KRED-QSSKZ.
  E_ZTERM = S_KRED-ZTERM.

  IF NOT S_KRED-LNRZE IS INITIAL.      " headquater
    E_LIFNR = S_KRED-LNRZE.
    E_FILKD = I_LIFNR.
    CLEAR: S_KRED, E_QSSKZ, E_ZTERM.

    CALL FUNCTION 'FI_VENDOR_DATA'
      EXPORTING
        I_BUKRS       = I_BUKRS
        I_LIFNR       = E_LIFNR
      IMPORTING
        E_KRED        = S_KRED
      EXCEPTIONS
        ERROR_MESSAGE = 01.

    IF SY-SUBRC NE 0.
      PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
      X_FEHLER = X.
      EXIT.
    ENDIF.
    E_QSSKZ = S_KRED-QSSKZ.
    E_ZTERM = S_KRED-ZTERM.
  ENDIF.

  IF S_KRED-BEGRU NE SPACE.
    PERFORM AUTHORITY_CHECK USING 'BEK' 'BRGRU' S_KRED-BEGRU
                            CHANGING S_MSG
                                     X_FEHLER.
    IF X_FEHLER = X.
      EXIT.
    ENDIF.
  ENDIF.

  IF S_KRED-BEGRU_B NE SPACE.
    PERFORM AUTHORITY_CHECK USING 'BEK' 'BRGRU' S_KRED-BEGRU_B
                            CHANGING S_MSG
                                     X_FEHLER.
    IF X_FEHLER = X.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                               " VENDOR_DATA_GET

*&---------------------------------------------------------------------*
*&      Form  BUZID_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUZID_GET USING    I_VGART
                        I_KOART
               CHANGING E_BUZID
                        E_BUSTW.

*------- Buchungsstring ermitteln, BUZID setzen ----------------------*
  CALL FUNCTION 'MR_STRING_DETERMINE'
    EXPORTING
      I_KNTTP = ' '
      I_KOART = I_KOART
      I_PSTYP = ' '
      I_VGART = I_VGART
    IMPORTING
      E_BUSTW = E_BUSTW
      E_BUZID = E_BUZID.

ENDFORM.                               " BUZID_GET

*&---------------------------------------------------------------------*
*&      Form  KTOSL_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM KTOSL_GET USING    I_BUSTW
                        I_BUKRS
               CHANGING E_KTOSL.

  DATA: TAB_T169C LIKE T169C OCCURS 0,
        S_T169C   LIKE T169C,
        S_SGNBU   LIKE SGNBU.

  S_SGNBU-BUKRS = I_BUKRS.

  CALL FUNCTION 'MR_POSTING_GENERATE'
    EXPORTING
      I_BUSTW = I_BUSTW
      I_SGNBU = S_SGNBU
    TABLES
      T_T169C = TAB_T169C.


  LOOP AT TAB_T169C INTO S_T169C
                    WHERE BUSTW = I_BUSTW.
    E_KTOSL = S_T169C-VORSL.
  ENDLOOP.


ENDFORM.                               " KTOSL_GET

*&---------------------------------------------------------------------*
*&      Form  PAYMENT_CONDITIONS_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PAYMENT_CONDITIONS_GET USING    I_ZTERM LIKE BSEG-ZTERM
                                     I_BLDAT LIKE BKPF-BLDAT
                                     I_BUDAT LIKE BKPF-BUDAT
                            CHANGING E_ZFBDT LIKE BSEG-ZFBDT
                                     E_ZLSPR LIKE BSEG-ZLSPR
                                     E_ZLSCH LIKE BSEG-ZLSCH
                                     E_ZBD1T LIKE BSEG-ZBD1T
                                     E_ZBD1P LIKE BSEG-ZBD1P
                                     E_ZBD2T LIKE BSEG-ZBD2T
                                     E_ZBD2P LIKE BSEG-ZBD2P
                                     E_ZBD3T LIKE BSEG-ZBD3T
                                     X_FEHLER TYPE C
                                     S_MSG  TYPE TYP_MSG.

  DATA: S_T052       LIKE T052,
        S_SKLIN      LIKE SKLIN.

  CONSTANTS: C_T052(4) VALUE 'T052'.

* ZTERM (*) is needed for substitution paymnt term from headquater
  CHECK I_ZTERM NE '*'.

  CALL FUNCTION 'FI_FIND_PAYMENT_CONDITIONS'
    EXPORTING
      I_ZTERM       = I_ZTERM
      I_BLDAT       = I_BLDAT
      I_BUDAT       = I_BUDAT
    IMPORTING
      E_T052        = S_T052
      E_ZFBDT       = E_ZFBDT
      E_SKLIN       = S_SKLIN
    EXCEPTIONS
      ERROR_MESSAGE = 1.

  IF SY-SUBRC NE 0.
    PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
    X_FEHLER = X.
    EXIT.
  ELSE.

*------- Zahlungsschlüssel und Zahlungssperre vorschlagen --------------
    E_ZLSPR = S_T052-ZSCHF.
    E_ZLSCH = S_T052-ZLSCH.

*----------- Tage und Prozente setzen ----------------------------------
    E_ZBD1T = S_SKLIN-ZTAG1.
    E_ZBD1P = S_SKLIN-ZPRZ1.
    E_ZBD2T = S_SKLIN-ZTAG2.
    E_ZBD2P = S_SKLIN-ZPRZ2.
    E_ZBD3T = S_SKLIN-ZTAG3.
  ENDIF.

ENDFORM.                               " PAYMENT_CONDITIONS_GET


*&---------------------------------------------------------------------*
*&      Form  BSET_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BSET_FILL TABLES   TI_BKPF  TYPE TYP_TAB_BKPF
                        TI_BSEG  TYPE TYP_TAB_BSEG
                        TI_BSET  TYPE TYP_TAB_BSET
               USING    I_BUKRS  LIKE RKWA-BUKRS
               CHANGING S_MSG    TYPE TYP_MSG
                        X_FEHLER TYPE C.

  DATA: T_DBSEG    LIKE DBSEG OCCURS 0 WITH HEADER LINE,
        S_BSEG     LIKE BSEG,
        S_DBSEG    LIKE DBSEG,
        S_BKPF     LIKE BKPF,
        TAB_BKPF   LIKE BKPF OCCURS 0 WITH HEADER LINE,
        F_KALSM    LIKE T005-KALSM.

  READ TABLE TI_BKPF INDEX 1 INTO S_BKPF.

  S_BKPF-AWTYP = C_AWTYP_RMRP.
  APPEND S_BKPF TO TAB_BKPF.

* dbseg fill for cash discount
  LOOP AT TI_BSEG INTO S_BSEG.
    IF S_BSEG-KOART = C_KOART_KRED.
      T_DBSEG-XSKFB = X.
    ELSE.
      T_DBSEG-XSKFB = SPACE.
    ENDIF.
    APPEND T_DBSEG.
  ENDLOOP.

  CALL FUNCTION 'FIND_TAX_SPREADSHEET'
    EXPORTING
      BUCHUNGSKREIS = I_BUKRS
    IMPORTING
      SCHEMA        = F_KALSM
    EXCEPTIONS
      ERROR_MESSAGE = 1.

  IF SY-SUBRC NE 0.
    PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
    X_FEHLER = X.
    EXIT.
  ENDIF.

  CALL FUNCTION 'CALCULATE_TAX_DOCUMENT'
    EXPORTING
      I_BUKRS       = I_BUKRS
    IMPORTING
      E_ITXDAT      = ITXDAT
    TABLES
      T_BKPF        = TAB_BKPF
      T_BSEG        = TI_BSEG
      T_DBSEG       = T_DBSEG
    EXCEPTIONS
      ERROR_MESSAGE = 1.

  IF SY-SUBRC NE 0.
    PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
    X_FEHLER = X.
    EXIT.
  ENDIF.
* amount eligible for cash discount
  LOOP AT TI_BSEG INTO S_BSEG WHERE BUZID = C_BUZID_KRED.
    READ TABLE T_DBSEG INDEX 1 INTO S_DBSEG.
    S_BSEG-SKFBT = S_DBSEG-SKFBT.
    MODIFY TI_BSEG FROM S_BSEG.
  ENDLOOP.

* bseg fill
  CALL FUNCTION 'CREATE_BSET_ITEM'
    TABLES
      T_BKPF = TI_BKPF
      T_BSEG = TI_BSEG
      T_BSET = TI_BSET.

* vendor amount incl. taxes
  CLEAR: S_BSEG.
  LOOP AT TI_BSEG INTO S_BSEG WHERE BUZID = C_BUZID_KRED.
    IF ( ( S_BSEG-SHKZG = 'S' ) AND ( ITXDAT-SHKZG = 'S' ) )
    OR ( ( S_BSEG-SHKZG = 'H' ) AND ( ITXDAT-SHKZG = 'H' ) ).
      S_BSEG-WRBTR = S_BSEG-WRBTR - ITXDAT-FWSTE.
    ELSE.
      S_BSEG-WRBTR = S_BSEG-WRBTR + ITXDAT-FWSTE.
    ENDIF.
    S_BSEG-REWWR = S_BSEG-WRBTR.
    S_BSEG-TXBFW = S_BSEG-WRBTR.
    MODIFY TI_BSEG FROM S_BSEG.
    CLEAR S_BSEG.
  ENDLOOP.

ENDFORM.                               " BSET_FILL

*&---------------------------------------------------------------------*
*&      Form  RKWA_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM RKWA_UPDATE TABLES TI_RKWA TYPE TYP_TAB_RKWA
                        TI_TAX  TYPE TYP_TAB_TAX
                  USING I_BELNR LIKE RKWA-BELNR
                        I_GJAHR LIKE RKWA-GJAHR
                        I_LIFNR LIKE RKWA-LIFNR
                        I_VENDOR_LINE TYPE C
               CHANGING X_FEHLER TYPE C
                        S_MSG    TYPE TYP_MSG.

  DATA: S_RKWA LIKE RKWA,
        S_TAX  TYPE TYP_TAX.


  LOOP AT TI_RKWA INTO S_RKWA.
    S_RKWA-STATUS = '01'.
    S_RKWA-BELNR  = I_BELNR.
    S_RKWA-GJAHR  = I_GJAHR.
    IF NOT I_VENDOR_LINE IS INITIAL.
      S_RKWA-BUZEI  = SY-TABIX + 1.   "erste Zeile ist Kreditorenzeile
    ELSE.
      S_RKWA-BUZEI  = SY-TABIX.
    ENDIF.

* sales tax flag
    READ TABLE TI_TAX INTO S_TAX
                      WITH KEY MATNR = S_RKWA-MATNR
                               WERKS = S_RKWA-WERKS
                               LIFNR = I_LIFNR.
    IF SY-SUBRC NE 0.
      CLEAR S_MSG.
      S_MSG-MSGID = C_MSGID_M8.
      S_MSG-MSGTY = C_MSGTY_ERROR.
      S_MSG-MSGNO = '008'.
      S_MSG-MSGV1 = 'RKWA_UPDATE'.
      X_FEHLER = X.
      EXIT.
    ENDIF.

    IF S_RKWA-MWSKZ IS INITIAL.        "ab 4.5A Eintrag in der RKWA
      S_RKWA-MWSKZ = S_TAX-MWSKZ.
    ENDIF.

    MODIFY TI_RKWA FROM S_RKWA.
  ENDLOOP.

*------ Abrechnung Konsi-Verbindlichkeiten ---------------------------*
  DESCRIBE TABLE TI_RKWA LINES SY-TFILL.
  IF SY-TFILL GT 0.

    CALL FUNCTION 'MR_RKWA_UPDATE' IN UPDATE TASK
      EXPORTING
        I_UPDKZ = 'U'
      TABLES
        T_RKWA  = TI_RKWA.
  ENDIF.

ENDFORM.                               " RKWA_UPDATE

*&---------------------------------------------------------------------*
*&      Form  VARIANT_INIT
*&---------------------------------------------------------------------*
FORM VARIANT_INIT.

  CLEAR S_VARIANT.
  S_VARIANT-REPORT = C_REPID.

ENDFORM.                               " VARIANT_INIT
*&---------------------------------------------------------------------*
*&      Form  F4_VARIA
*&---------------------------------------------------------------------*
FORM F4_VARIA CHANGING CF_VARIA LIKE DISVARIANT-VARIANT.

  DATA: F_EXIT(1)   TYPE C.

  PERFORM VARIANT_INIT.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT = S_VARIANT
      I_SAVE     = C_SAVE
    IMPORTING
      E_EXIT     = F_EXIT
      ES_VARIANT = S_VAR_USR
    EXCEPTIONS
      NOT_FOUND  = 2.

  IF SY-SUBRC = 2.
    MESSAGE ID SY-MSGID TYPE 'S'      NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF F_EXIT = SPACE.
      CF_VARIA = S_VAR_USR-VARIANT.
    ENDIF.
  ENDIF.

ENDFORM.                                                    " F4_VARIA
*&---------------------------------------------------------------------*
*&      Form HEAD_MESSAGE_CREATE
*&---------------------------------------------------------------------*
FORM HEAD_MESSAGE_CREATE TABLES   TI_BKPF TYPE TYP_TAB_BKPF
                                   TI_BSEG TYPE TYP_TAB_BSEG
                          USING    I_BLART LIKE T169F-BLART
                                   I_BELNR LIKE RKWA-BELNR
                                   I_BUKRS LIKE RKWA-BUKRS
                          CHANGING X_FEHLER TYPE C
                                   S_MSG    TYPE TYP_MSG.


  DATA: LF_UPDATE   LIKE  BOOLE-BOOLE.

  DATA: H_RBKPV TYPE MRM_RBKPV,
        LS_KOMKBMR  LIKE  KOMKBMR.

  CLEAR: H_RBKPV, S_MSG.

  MOVE-CORRESPONDING TI_BKPF TO H_RBKPV.
  READ TABLE TI_BSEG INDEX 1.
  H_RBKPV-BELNR  =  I_BELNR.
  H_RBKPV-LIFNR  =  TI_BSEG-LIFNR.
* create messages
  LF_UPDATE = 'X'.

* Kommunikationsstruktur füllen
  CLEAR LS_KOMKBMR.
  LS_KOMKBMR-BUKRS = I_BUKRS.
  LS_KOMKBMR-BELNR = I_BELNR.
  LS_KOMKBMR-BLART = I_BLART.
  LS_KOMKBMR-USNAM = H_RBKPV-USNAM.
  IF NOT TI_BSEG-FILKD IS INITIAL.
    LS_KOMKBMR-LIFNR = TI_BSEG-FILKD.  " Partner LF
  ELSE.
    LS_KOMKBMR-LIFNR = TI_BSEG-LIFNR.  " Partner LF
  ENDIF.
  LS_KOMKBMR-LIFRE = TI_BSEG-LIFNR.          " Partner RS
  LS_KOMKBMR-IDENT = C_IDENT_KONS.

  CALL FUNCTION 'MRM_HEAD_MESSAGE_CREATE'
    EXPORTING
      IF_KAPPL      = C_KAPPL_MR
      IF_KALSM      = C_KALSM_0003
      IS_RBKPV      = H_RBKPV
      IS_KOMKBMR    = LS_KOMKBMR
      IF_UPDATE     = LF_UPDATE
    EXCEPTIONS
      ERROR_MESSAGE = 2
      OTHERS        = 3.

  IF SY-SUBRC <> 0.
    PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
    IF SY-MSGTY = 'A'.   "Ausgabe der A-Meldung als E-Meldung:
      S_MSG-MSGTY = 'E'.
    ENDIF.
    X_FEHLER = X.
    EXIT.
  ENDIF.

ENDFORM.                               " HEAD_MESSAGE_CREATE
*&---------------------------------------------------------------------*
*&      Form  authority_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM AUTHORITY_CHECK USING    AUTH_OBJ
                              AUTH_ID
                              AUTH_VAL TYPE LFA1-BEGRU
                     CHANGING S_MSG    TYPE TYP_MSG
                              X_FEHLER TYPE C.
  CASE AUTH_OBJ.
    WHEN 'BEK'.
      AUTHORITY-CHECK OBJECT 'F_BKPF_BEK'
                      ID 'ACTVT' FIELD '01'
                      ID AUTH_ID FIELD AUTH_VAL.
      IF SY-SUBRC NE 0.
        CLEAR S_MSG.
        S_MSG-MSGID = C_MSGID_M8.
        S_MSG-MSGTY = C_MSGTY_ERROR.
        S_MSG-MSGNO = '120'.
        S_MSG-MSGV1 = AUTH_VAL.
        X_FEHLER = X.
        EXIT.
      ENDIF.
  ENDCASE.
ENDFORM.                               " authority_check
*&---------------------------------------------------------------------*
*&      Form  bseg_lines_numbering
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_TI_BSEG[]  text
*----------------------------------------------------------------------*
FORM BSEG_LINES_NUMBERING CHANGING TI_BSEG TYPE TYP_TAB_BSEG.

  DATA: S_BSEG  LIKE BSEG,
        F_BUZEI LIKE BSEG-BUZEI.

  CLEAR: F_BUZEI.
  LOOP AT TI_BSEG INTO S_BSEG.
    F_BUZEI = F_BUZEI + 1.
    S_BSEG-BUZEI = F_BUZEI.
    MODIFY TI_BSEG FROM S_BSEG TRANSPORTING BUZEI.
  ENDLOOP.

ENDFORM.                               " bseg_lines_numbering
*&---------------------------------------------------------------------*
*&      Form  nav_verteilen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_BSEG  text
*----------------------------------------------------------------------*
FORM NAV_VERTEILEN TABLES  TI_BSEG STRUCTURE BSEG.

  DATA: LT_BSEG_OUTPUT TYPE STANDARD TABLE OF BSEG,
        LT_BSEG_INPUT TYPE STANDARD TABLE OF BSEG.

  DATA: LS_BSEG LIKE BSEG.                          "1824213 start
  DATA: LV_COUNTER TYPE I.

  FIELD-SYMBOLS: <LS_BSEG> TYPE BSEG,
                 <LS_BSEG_OUTPUT> TYPE BSEG.

  LOOP AT TI_BSEG ASSIGNING <LS_BSEG>
                      WHERE BUZID = C_BUZID_SACH
                        AND KOART = C_KOART_SACH.

    LV_COUNTER = LV_COUNTER + 1.
    <LS_BSEG>-XREF1 = LV_COUNTER.
    LS_BSEG = <LS_BSEG>.
    APPEND LS_BSEG TO LT_BSEG_INPUT.
  ENDLOOP.

  CLEAR: LS_BSEG.

  CALL FUNCTION 'NDVAT_ADJUSTMENT'
    EXPORTING
      I_BSEG        = LS_BSEG
      READ_BSEG     = 'X'
      CALCULATE     = 'M'
    IMPORTING
      E_BSEG        = LS_BSEG
    TABLES
      T_BSEG_INPUT  = LT_BSEG_INPUT
      T_BSEG_OUTPUT = LT_BSEG_OUTPUT.

  LOOP AT TI_BSEG ASSIGNING <LS_BSEG>
                      WHERE BUZID = C_BUZID_SACH
                        AND KOART = C_KOART_SACH.

    READ TABLE LT_BSEG_OUTPUT ASSIGNING <LS_BSEG_OUTPUT>
                              WITH KEY XREF1 = <LS_BSEG>-XREF1.
    IF <LS_BSEG_OUTPUT> IS ASSIGNED.
      <LS_BSEG>-WRBTR = <LS_BSEG_OUTPUT>-WRBTR.
      DELETE TABLE LT_BSEG_OUTPUT FROM <LS_BSEG_OUTPUT>.
      CLEAR <LS_BSEG>-XREF1.
    ENDIF.
  ENDLOOP.                                           "1824213 end

ENDFORM.                               " nav_verteilen
*&---------------------------------------------------------------------*
*&      Form  null_lines_check                                         *
*&---------------------------------------------------------------------*
*  Prüft ob der Beleg nur 0-Zeilen beinhaltet                          *
*----------------------------------------------------------------------*
*      -->P_TAB_BSEG  text
*      <--P_X_FEHLER  text
*      <--P_S_MSG  text
*----------------------------------------------------------------------*
FORM NULL_LINES_CHECK TABLES   TI_BSEG STRUCTURE BSEG
                      CHANGING X_FEHLER TYPE C
                               S_MSG    TYPE TYP_MSG.

  DATA: XNULLPOS LIKE BOOLE-BOOLE.

*--- überprüfung, ob der zu erzeugende FI-Beleg nicht nur Nullzeilen --*
*--- enth?lt:                                                        --*
  MOVE 'X' TO XNULLPOS.
  LOOP AT TI_BSEG.
    IF XNULLPOS = SPACE.
      EXIT.
    ELSE.
      IF TI_BSEG-WRBTR <> 0 OR TI_BSEG-FWBAS <> 0.
        CLEAR XNULLPOS.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF XNULLPOS = 'X'.
    CLEAR: S_MSG.
    S_MSG-MSGID = C_MSGID_M8.
    S_MSG-MSGTY = C_MSGTY_ERROR.
    S_MSG-MSGNO = '278'.
    X_FEHLER = X.
    EXIT.
  ENDIF.

ENDFORM.                               " null_lines_check
*eject
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_TYPE_CHECK
*&---------------------------------------------------------------------*
*  netto document type is not allowed
*----------------------------------------------------------------------*
FORM DOCUMENT_TYPE_CHECK USING    BLART    LIKE T169F-BLART
                         CHANGING S_T003   LIKE T003
                                  X_FEHLER TYPE C
                                  S_MSG    TYPE TYP_MSG.
  CLEAR: S_T003.

* gefundene Belegart darf nicht NettoBelegart sein
  CALL FUNCTION 'FI_DOCUMENT_TYPE_DATA'
    EXPORTING
      I_BLART       = BLART
    IMPORTING
      E_T003        = S_T003
    EXCEPTIONS
      ERROR_MESSAGE = 1
      OTHERS        = 2.
  IF ( SY-SUBRC <> 0 ).
    PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
    X_FEHLER = X.
    EXIT.
  ENDIF.

  IF ( S_T003-XNETB = X ).
    CLEAR: S_MSG.
    S_MSG-MSGID = C_MSGID_M8.
    S_MSG-MSGTY = C_MSGTY_ERROR.
    S_MSG-MSGNO = '733'.
    S_MSG-MSGV1 = C_TCODE_MRKO.
    S_MSG-MSGV2 = BLART.
    X_FEHLER = X.
    EXIT.
  ENDIF.

ENDFORM.                    "document_type_check
*&--------------------------------------------------------------------*
*&      Form  fm_account_assignment_get
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->S_RKWA     text
*      -->S_BSEG     text
*      -->X_FEHLER   text
*      -->S_MSG      text
*---------------------------------------------------------------------*
FORM FM_ACCOUNT_ASSIGNMENT_GET USING S_RKWA   STRUCTURE RKWA
                            CHANGING S_BSEG   STRUCTURE BSEG
                                     X_FEHLER TYPE      C
                                     S_MSG    TYPE      TYP_MSG.
  INCLUDE: IFIFMCON_APPL,
           IFIFMCON_BOOL.

  DATA: L_F_MSEG              LIKE ACCTIT,
        L_FLAG_FMAKT          TYPE C,
        L_FLG_ISPS_NOT_ACTIVE TYPE C.
* definitions for reading archived documents from AIS
  DATA: LT_ARCMKPF            TYPE  TY_T_MKPF,
        LT_ARCMSEG            TYPE  TY_T_MSEG,
        LS_ARCMSEG            TYPE  MSEG,
        LS_BORIDENT           TYPE  BORIDENT.

*----- Nur Aufrufen wenn IS-PS aktiv ist
  CALL FUNCTION 'FM00_CHECK_ISPS'
    IMPORTING
      E_FLG_ISPS_NOT_ACTIVE = L_FLG_ISPS_NOT_ACTIVE.
  CHECK L_FLG_ISPS_NOT_ACTIVE IS INITIAL.

  CALL FUNCTION 'FMFK_BUKRS_CHECK_FMAKTIV'
    EXPORTING
      IP_BUKRS           = S_RKWA-BUKRS
      IP_APPLC           = APPLC_CA
    IMPORTING
      OP_IS_ACTIVE       = L_FLAG_FMAKT
    EXCEPTIONS
      NO_FIKRS_FOR_BUKRS = 01
      WRONG_INPUT_FLAG   = 02.                              "#EC *
  CHECK L_FLAG_FMAKT = CON_JA AND SY-SUBRC IS INITIAL.

*----- HHM Kontierung aus dem Materialbeleg holen
  SELECT SINGLE FIPOS FISTL FKBER GEBER BUDGET_PD GRANT_NBR
    FROM ACCTIT
    INTO CORRESPONDING FIELDS OF L_F_MSEG
   WHERE AWTYP = 'MKPF'
     AND AWREF = S_RKWA-MBLNR
     AND AWORG = S_RKWA-MJAHR
     AND ZEILE = S_RKWA-ZEILE
     AND KTOSL = 'GBB'.
  IF SY-SUBRC <> 0 OR L_F_MSEG-FIPOS IS INITIAL.
    SELECT SINGLE FIPOS FISTL GEBER FKBER GRANT_NBR
      FROM MSEG
      INTO CORRESPONDING FIELDS OF L_F_MSEG
     WHERE MBLNR = S_RKWA-MBLNR
       AND MJAHR = S_RKWA-MJAHR
       AND ZEILE = S_RKWA-ZEILE.
    IF SY-SUBRC <> 0.
* read MM doc data from AIS (archive)
      REFRESH LT_ARCMKPF.
      REFRESH LT_ARCMSEG.
      CLEAR   LS_BORIDENT.
      CONCATENATE  S_RKWA-MBLNR S_RKWA-MJAHR
                          INTO  LS_BORIDENT-OBJKEY.
      CALL FUNCTION 'ASH_MM_MATBEL_READ'
        EXPORTING
          I_BORIDENT    = LS_BORIDENT
        TABLES
          ET_MKPF       = LT_ARCMKPF
          ET_MSEG       = LT_ARCMSEG
        EXCEPTIONS
          ERROR_MESSAGE = 1
          OTHERS        = 4.
      IF SY-SUBRC = 0.
        LOOP AT LT_ARCMSEG INTO LS_ARCMSEG
           WHERE ZEILE     = S_RKWA-ZEILE.
          L_F_MSEG-FIPOS     = LS_ARCMSEG-FIPOS.
          L_F_MSEG-FISTL     = LS_ARCMSEG-FISTL.
          L_F_MSEG-GEBER     = LS_ARCMSEG-GEBER.
          L_F_MSEG-FKBER     = LS_ARCMSEG-FKBER.
          L_F_MSEG-GRANT_NBR = LS_ARCMSEG-GRANT_NBR.
        ENDLOOP.
      ELSE.
        IF 1 = 2.
          MESSAGE E298(FI).
*   MSEG Zeile nicht gefunden! Ableitung der HHM Kontierung nicht m?glic
        ENDIF.

        CLEAR: S_MSG.
        S_MSG-MSGID = 'FI'.
        S_MSG-MSGTY = C_MSGTY_ERROR.
        S_MSG-MSGNO = '298'.
        X_FEHLER = 'X'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  S_BSEG-GRANT_NBR  = L_F_MSEG-GRANT_NBR.
  S_BSEG-FKBER_LONG = L_F_MSEG-FKBER.
  S_BSEG-FIPOS = L_F_MSEG-FIPOS.
  S_BSEG-GEBER = L_F_MSEG-GEBER.
* Budget Period EhP4
  IF CL_OPS_SWITCH_CHECK=>OPS_SFWS_BUD_PER( ) IS NOT INITIAL.
    S_BSEG-BUDGET_PD = L_F_MSEG-BUDGET_PD.
  ENDIF.
  S_BSEG-FISTL = L_F_MSEG-FISTL.
ENDFORM.                    "fm_account_assignment_get
*&---------------------------------------------------------------------*
*&      Form  withholding_tax_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TAB_ACCIT_WT  text
*      -->P_TI_RBWS  text
*----------------------------------------------------------------------*
FORM WITHHOLDING_TAX_CREATE TABLES TE_ACCIT_WT STRUCTURE ACCIT_WT
                                   TI_RBWS     TYPE TYP_TAB_RBWS.

  CHECK NOT TI_RBWS[] IS INITIAL.

  DATA: S_RBWS LIKE RBWS.

  LOOP AT TI_RBWS INTO S_RBWS.
    CLEAR TE_ACCIT_WT.
    MOVE-CORRESPONDING S_RBWS TO TE_ACCIT_WT.
    TE_ACCIT_WT-WT_KEY = S_RBWS-SPLIT_KEY.
    APPEND TE_ACCIT_WT.
  ENDLOOP.

ENDFORM.                    " withholding_tax_create
*&---------------------------------------------------------------------*
*&      Form  vendor_taxnum_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RKWA  text
*      -->P_S_T001  text
*      -->P_LIFNR  text
*      <--P_X_FEHLER  text
*      <--P_S_MSG  text
*----------------------------------------------------------------------*
FORM VENDOR_TAXNUM_CHECK TABLES   TI_RKWA TYPE TYP_TAB_RKWA
                          USING    S_T001  LIKE T001
                                   I_LIFNR LIKE RKWA-LIFNR
                          CHANGING X_FEHLER TYPE C
                                   S_MSG   TYPE TYP_MSG.
  DATA: S_TAXDATA      TYPE MRM_TAX_2,
        S_RKWA         LIKE RKWA,
        S_KRED         LIKE VF_KRED,
        TAB_TAXDATA    TYPE MRM_TAB_TAXES,
        S_T005         LIKE T005.

  CALL FUNCTION 'READ_CUSTOMIZED_MESSAGE'
    EXPORTING
      I_ARBGB = 'M8'
      I_DTYPE = 'W'
      I_MSGNR = '759'
    IMPORTING
      E_MSGTY = SY-MSGTY.
  CHECK SY-MSGTY = 'E'.

* read vendor
  CALL FUNCTION 'FI_VENDOR_DATA'
    EXPORTING
      I_LIFNR        = I_LIFNR
    IMPORTING
      E_KRED         = S_KRED
    EXCEPTIONS
      VENDOR_MISSING = 1
      OTHERS         = 2.
  IF SY-SUBRC NE 0.
    PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
    X_FEHLER = X.
    EXIT.
  ENDIF.

* check country of vendor
  CALL FUNCTION 'T005_SINGLE_READ'
    EXPORTING
      T005_LAND1 = S_KRED-LAND1
    IMPORTING
      WT005      = S_T005
    EXCEPTIONS
      NOT_FOUND  = 1
      OTHERS     = 2.
  IF SY-SUBRC <> 0.
    PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
    X_FEHLER = X.
    EXIT.
  ELSEIF S_T005-XEGLD IS INITIAL.
    EXIT.
  ENDIF.

* fill table taxdata
  LOOP AT TI_RKWA INTO S_RKWA WHERE MWSKZ NE SPACE.
    S_TAXDATA-MWSKZ = S_RKWA-MWSKZ.
    COLLECT S_TAXDATA INTO TAB_TAXDATA.
  ENDLOOP.

* read country data of company code
  CALL FUNCTION 'MRM_T005_READ'
    EXPORTING
      TI_T001    = S_T001
    IMPORTING
      TE_T005    = S_T005
    TABLES
      TI_TAXDATA = TAB_TAXDATA.                            "#EC FB_NORC

  IF SY-SUBRC <> 0.                                        "#EC FB_NORC
    PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
    X_FEHLER = X.
    EXIT.
  ELSEIF S_T005-XEGLD IS INITIAL.
    EXIT.
  ENDIF.

* check tax number
  IF  ( ( S_T005-INTCA <> 'DE' AND S_KRED-STENR IS INITIAL )
      OR ( S_T005-INTCA =  'DE' AND S_KRED-STENR IS INITIAL
          AND S_KRED-STCD1 IS INITIAL ) ) AND S_KRED-STCEG IS INITIAL .
    SY-MSGTY = 'E'.
    SY-MSGID = 'M8'.
    SY-MSGNO = '759'.
    PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
    X_FEHLER = X.
    EXIT.
  ENDIF.

ENDFORM.                    " vendor_taxnum_check

*&---------------------------------------------------------------------*
*&      Form  LOCK_RKWA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RKWA  text
*      <--P_X_FEHLER  text
*      <--P_S_MSG  text
*----------------------------------------------------------------------*
FORM LOCK_RKWA TABLES   IT_RKWA TYPE TYP_TAB_RKWA
               CHANGING X_FEHLER
                        S_MSG   TYPE TYP_MSG.

  DATA: LS_RKWA LIKE RKWA.

  CLEAR X_FEHLER.

  LOOP AT IT_RKWA INTO LS_RKWA.
    CALL FUNCTION 'ENQUEUE_E_RKWA'
      EXPORTING
        MBLNR          = LS_RKWA-MBLNR
        MJAHR          = LS_RKWA-MJAHR
        ZEILE          = LS_RKWA-ZEILE
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.
    IF SY-SUBRC <> 0.
      PERFORM MSG_FUELLEN_SYSVAR CHANGING S_MSG.
      X_FEHLER = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " lock_rkwa


*{   INSERT
INCLUDE WRF_RMVKON00F01.    "Note 663537
*}   INSERT