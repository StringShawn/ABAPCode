*&---------------------------------------------------------------------*
*&  包括                ZQR_CODE_IMAGE
*&---------------------------------------------------------------------*

TYPE-POOLS:sbdst.

DATA : bds_description LIKE bapisignat-prop_value.


* BDS handling
CONSTANTS:
c_bds_classname TYPE sbdst_classname VALUE 'DEVC_STXD_BITMAP',
c_bds_classtype TYPE sbdst_classtype VALUE 'OT',          " others
c_bds_mimetype  TYPE bds_mimetp      VALUE 'application/octet-stream',
c_bds_original  TYPE sbdst_doc_var_tg VALUE 'OR'.



* Graphic handling
CONSTANTS:
c_stdtext  LIKE thead-tdobject VALUE 'TEXT',
c_graphics LIKE thead-tdobject VALUE 'GRAPHICS',
c_bmon     LIKE thead-tdid     VALUE 'BMON',
c_bcol     LIKE thead-tdid     VALUE 'BCOL'.


DATA: gi_filename TYPE rlgrap-filename,
      gi_name        TYPE stxbitmaps-tdname,
      gi_object      TYPE stxbitmaps-tdobject,
      gi_id          TYPE stxbitmaps-tdid,
      gi_btype       TYPE stxbitmaps-tdbtype,
      gi_resident    TYPE stxbitmaps-resident,
      gi_autoheight  TYPE stxbitmaps-autoheight,
      gi_bmcomp      TYPE stxbitmaps-bmcomp,
      gi_resolution  TYPE stxbitmaps-resolution,
      l_extension TYPE rlgrap-filename,
      l_docid     TYPE stxbitmaps-docid.

"Picture Control
DATA: picture_container TYPE REF TO cl_gui_custom_container,
      picture_control   TYPE REF TO cl_gui_picture.


DATA: l_img_url TYPE w3url.
DATA :l_img_subtype TYPE w3param-cont_type.
DATA : l_str_length TYPE i.
DATA : url TYPE string.
DATA : l_content_length TYPE i.

DATA : mime TYPE w3mimetabtype.
DATA: blob TYPE w3mimetabtype,
      blob_size TYPE w3param-cont_len,
      blob_type TYPE w3param-cont_type.

DATA : i_igs_image_converter TYPE REF TO cl_igs_image_converter.
DATA: content TYPE xstring.
DATA : http_client TYPE REF TO if_http_client.


TYPES : BEGIN OF ty_binary,
  binary_field(1000) TYPE c,
END OF ty_binary.

DATA : hex_tab1 TYPE TABLE OF ty_binary WITH HEADER LINE.

DATA:qr_text TYPE string.

DATA:width TYPE int3 VALUE 50.
DATA:height TYPE int3 VALUE 50.

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_QRCODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM download_qrcode  USING pv_text TYPE string.

  qr_text = pv_text.

  DATA: xstr TYPE string,
        str  TYPE string,
        l_codepage(4) TYPE n.

  DATA:outbuf TYPE xstring.

  CALL FUNCTION 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
    EXPORTING
      external_name = 'UTF-8'
    IMPORTING
      sap_codepage  = l_codepage.
  "解码
*   xstr = cl_abap_codepage=>convert_to(
*     source      = qr_text
*     codepage    = `UTF-8` ).

  CALL FUNCTION 'TERM_CONVERT_CODEPAGE'
    EXPORTING
      intext     = qr_text
      langu      = sy-langu
    IMPORTING
      outbuf     = outbuf
    EXCEPTIONS
      conv_error = 1
      OTHERS     = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  xstr = outbuf.

  DATA: lv_str2 TYPE string,
        lv_line TYPE i,
        lv_pos TYPE i.

  lv_line = STRLEN( xstr ) / 2.
  lv_pos = 0.
  DO lv_line TIMES.
    CONCATENATE lv_str2  '%' xstr+lv_pos(2)  INTO lv_str2.
    lv_pos = lv_pos + 2.
  ENDDO.

*  CONCATENATE
*
*  'http://qr.udee.cn/index.php?m=Qrcode&a=aip&f=%23000000&b=%23FFFFFF&'
*
*  'pt=%23000000&inpt=%23000000&wh=350&t='  lv_str2   INTO url.

  CONCATENATE 'http://qr.liantu.com/api.php?text=' lv_str2   INTO url.

  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = url
    IMPORTING
      client             = http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.


  IF sy-subrc = 0.

    http_client->send( ).

    http_client->receive( ).

    content = http_client->response->get_data( ).

    http_client->close( ).

    l_str_length = XSTRLEN( content ).

    CALL FUNCTION 'RSFO_XSTRING_TO_MIME'
      EXPORTING
        c_xstring = content
        i_length  = l_str_length
      TABLES
        c_t_mime  = mime.

  ENDIF.

ENDFORM. " DOWNLOAD_QRCODE

*&---------------------------------------------------------------------*
*&      Form  CONVERT_IMAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM convert_image USING pv_name TYPE stxbitmaps-tdname.

  CREATE OBJECT i_igs_image_converter .

  i_igs_image_converter->input = 'image/png'.
  i_igs_image_converter->output = 'image/bmp'.
  i_igs_image_converter->width = width.
  i_igs_image_converter->height = height.

  CALL METHOD i_igs_image_converter->set_image
    EXPORTING
      blob      = mime
      blob_size = l_content_length.


  CALL METHOD i_igs_image_converter->execute
    EXCEPTIONS
      communication_error = 1
      internal_error      = 2
      external_error      = 3
      OTHERS              = 4.

  IF sy-subrc = 0.

    CALL METHOD i_igs_image_converter->get_image
      IMPORTING
        blob      = blob
        blob_size = blob_size
        blob_type = blob_type.

  ENDIF.

*  gi_name = 'QRCODE20'.         "name of the qrcode will be in se78
  gi_name = pv_name.
  "after one time running this program
  gi_object = 'GRAPHICS'.
  gi_id = 'BMAP'.
  gi_btype = 'BCOL'. "If u want black and white pass BMON
  gi_resident = ' '.
  gi_autoheight =  'X'.
  gi_bmcomp = 'X'.
  l_extension = 'BMP'.

  "importing the image into se78 before displaying it in the smartform.

  PERFORM import_bitmap_bds    USING blob
        gi_name
        gi_object
        gi_id
        gi_btype
        l_extension
        ' '
        gi_resident
        gi_autoheight
        gi_bmcomp
  CHANGING l_docid
    gi_resolution.

ENDFORM. " CONVERT_IMAGE


*&---------------------------------------------------------------------*
*&      Form  SHOW_SMART_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM show_smart_form TABLES pt_qr_dn STRUCTURE zqr_code_dn CHANGING qr_dn TYPE zqr_code_dn.


* DATA:qr_dn TYPE zqr_code_dn.
  DATA:lv_tabix TYPE string.

  DATA :gv_formname TYPE tdsfname VALUE 'ZQR_CODE_DN_PRINT'.
  DATA :gv_fm_name  TYPE rs38l_fnam.
  DATA :gs_cont TYPE ssfctrlop.


  DATA:lt_qr_dn TYPE STANDARD TABLE OF zqr_code_dn.
  DATA:ls_qr_dn LIKE LINE OF lt_qr_dn.

  gs_cont-no_open  = 'X'.
  gs_cont-no_close = 'X'.


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
  EXPORTING
    formname                 = gv_formname
*   VARIANT                  = ' '
*   DIRECT_CALL              = ' '
  IMPORTING
    fm_name                  = gv_fm_name
* EXCEPTIONS
*   NO_FORM                  = 1
*   NO_FUNCTION_MODULE       = 2
*   OTHERS                   = 3
    .

  CALL FUNCTION 'SSF_OPEN'
  EXPORTING
*     ARCHIVE_PARAMETERS       =
*     USER_SETTINGS            = 'X'
*     MAIL_SENDER              =
*     MAIL_RECIPIENT           =
*     MAIL_APPL_OBJ            =
*     OUTPUT_OPTIONS           =
    control_parameters       = gs_cont
  EXCEPTIONS
    formatting_error         = 1
    internal_error           = 2
    send_error               = 3
    user_canceled            = 4
    OTHERS                   = 5.

  LOOP AT pt_qr_dn INTO ls_qr_dn.
    lv_tabix = sy-tabix.

    qr_text = ls_qr_dn-qr.

    IF NOT qr_text IS INITIAL.

      PERFORM download_qrcode USING qr_text.
      "gi_name = 'QRCODE20'.
      CONCATENATE 'QRCODE' sy-uname  lv_tabix INTO gi_name.
      CONDENSE gi_name NO-GAPS.
      PERFORM convert_image USING gi_name.

      ls_qr_dn-gi_name = gi_name.


      CALL FUNCTION gv_fm_name
      EXPORTING
*   ARCHIVE_INDEX              =
*   ARCHIVE_INDEX_TAB          =
*   ARCHIVE_PARAMETERS         =
        control_parameters         = gs_cont
*   MAIL_APPL_OBJ              =
*   MAIL_RECIPIENT             =
*   MAIL_SENDER                =
*   OUTPUT_OPTIONS             =
*   USER_SETTINGS              = 'X'
      qr = ls_qr_dn
* IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
*      TABLES

      EXCEPTIONS
        formatting_error           = 1
        internal_error             = 2
        send_error                 = 3
        user_canceled              = 4
        OTHERS                     = 5
        .


    ENDIF.

  ENDLOOP.

  CALL FUNCTION 'SSF_CLOSE'
*   IMPORTING
*     JOB_OUTPUT_INFO        =
  EXCEPTIONS
    formatting_error       = 1
    internal_error         = 2
    send_error             = 3
    OTHERS                 = 4.



*  "gettingt the name FM of the smartform
*  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*    EXPORTING
**      formname = 'ZQR_TEST'
*      formname = 'ZQR_CODE_DN_PRINT'
*    IMPORTING
*      fm_name  = gv_fm_name.
*
*  CALL FUNCTION gv_fm_name
*    EXPORTING
*      qr = qr_dn.


ENDFORM. " SHOW_SMART_FORM

*&---------------------------------------------------------------------*
*&      Form  IMPORT_BITMAP_BDS (Copied from standard program and
"modified it as per the requirement)
*&---------------------------------------------------------------------*
FORM import_bitmap_bds
USING    p_blob       TYPE w3mimetabtype
      p_name           TYPE stxbitmaps-tdname
      p_object         TYPE stxbitmaps-tdobject
      p_id             TYPE stxbitmaps-tdid
      p_btype          TYPE stxbitmaps-tdbtype
      p_format         TYPE c
      p_title          LIKE bds_description
      p_resident       TYPE stxbitmaps-resident
      p_autoheight     TYPE stxbitmaps-autoheight
      p_bmcomp         TYPE stxbitmaps-bmcomp
CHANGING p_docid          TYPE stxbitmaps-docid
  p_resolution     TYPE stxbitmaps-resolution.


  DATA: l_object_key TYPE sbdst_object_key.
  DATA: l_tab        TYPE ddobjname.

  DATA: BEGIN OF l_bitmap OCCURS 0,
    l(64) TYPE x,
  END OF l_bitmap.

  DATA: l_filename        TYPE string,
        l_bytecount       TYPE i,
        l_bds_bytecount   TYPE i.
  DATA: l_color(1)        TYPE c,

        l_width_tw        TYPE stxbitmaps-widthtw,
        l_height_tw       TYPE stxbitmaps-heighttw,
        l_width_pix       TYPE stxbitmaps-widthpix,
        l_height_pix      TYPE stxbitmaps-heightpix.
  DATA: l_bds_object      TYPE REF TO cl_bds_document_set,
        l_bds_content     TYPE sbdst_content,
        l_bds_components  TYPE sbdst_components,
        wa_bds_components TYPE LINE OF sbdst_components,
        l_bds_signature   TYPE sbdst_signature,
        wa_bds_signature  TYPE LINE OF sbdst_signature,
        l_bds_properties  TYPE sbdst_properties,
        wa_bds_properties TYPE LINE OF sbdst_properties.

  DATA  wa_stxbitmaps TYPE stxbitmaps.



* Enqueue
  PERFORM enqueue_graphic USING p_object
        p_name
        p_id
        p_btype.


* Bitmap conversion
  CALL FUNCTION 'SAPSCRIPT_CONVERT_BITMAP_BDS'
    EXPORTING
      color                    = 'X'
      format                   = p_format
      resident                 = p_resident
      bitmap_bytecount         = l_bytecount
      compress_bitmap          = p_bmcomp
    IMPORTING
      width_tw                 = l_width_tw
      height_tw                = l_height_tw
      width_pix                = l_width_pix
      height_pix               = l_height_pix
      dpi                      = p_resolution
      bds_bytecount            = l_bds_bytecount
    TABLES
      bitmap_file              = p_blob
      bitmap_file_bds          = l_bds_content
    EXCEPTIONS
      format_not_supported     = 1
      no_bmp_file              = 2
      bmperr_invalid_format    = 3
      bmperr_no_colortable     = 4
      bmperr_unsup_compression = 5
      bmperr_corrupt_rle_data  = 6
      OTHERS                   = 7.

  IF sy-subrc <> 0.

    PERFORM dequeue_graphic USING p_object
          p_name
          p_id
          p_btype.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING conversion_failed.

  ENDIF.

* Save bitmap in BDS
  CREATE OBJECT l_bds_object.

  wa_bds_components-doc_count  = '1'.
  wa_bds_components-comp_count = '1'.
  wa_bds_components-mimetype   = c_bds_mimetype.
  wa_bds_components-comp_size  = l_bds_bytecount.
  APPEND wa_bds_components TO l_bds_components.

  IF p_docid IS INITIAL.          " graphic is new

    wa_bds_signature-doc_count = '1'.
    APPEND wa_bds_signature TO l_bds_signature.


    CALL METHOD l_bds_object->create_with_table
      EXPORTING
        classname  = c_bds_classname
        classtype  = c_bds_classtype
        components = l_bds_components
        content    = l_bds_content
      CHANGING
        signature  = l_bds_signature
        object_key = l_object_key
      EXCEPTIONS
        OTHERS     = 1.

    IF sy-subrc <> 0.

      PERFORM dequeue_graphic USING p_object
            p_name
            p_id
            p_btype.
*      message e285 with p_name  'BDS'.

    ENDIF.

    READ TABLE l_bds_signature INDEX 1 INTO wa_bds_signature
    TRANSPORTING doc_id.

    IF sy-subrc = 0.

      p_docid = wa_bds_signature-doc_id.

    ELSE.

      PERFORM dequeue_graphic USING p_object
            p_name
            p_id
            p_btype.
*      message e285 with p_name 'BDS'.

    ENDIF.

  ELSE.                " graphic already exists

********* read object_key for faster access *****
    CLEAR l_object_key.
    SELECT SINGLE * FROM stxbitmaps INTO wa_stxbitmaps
    WHERE tdobject = p_object
    AND tdid     = p_id
    AND tdname   = p_name
    AND tdbtype  = p_btype.

    SELECT SINGLE tabname FROM bds_locl INTO l_tab
    WHERE classname = c_bds_classname
    AND classtype = c_bds_classtype.

    IF sy-subrc = 0.

      SELECT SINGLE object_key FROM (l_tab) INTO l_object_key
      WHERE loio_id = wa_stxbitmaps-docid+10(32)
      AND classname = c_bds_classname
      AND classtype = c_bds_classtype.

    ENDIF.

******** read object_key end ********************

    CALL METHOD l_bds_object->update_with_table
      EXPORTING
        classname     = c_bds_classname
        classtype     = c_bds_classtype
        object_key    = l_object_key
        doc_id        = p_docid
        doc_ver_no    = '1'
        doc_var_id    = '1'
      CHANGING
        components    = l_bds_components
        content       = l_bds_content
      EXCEPTIONS
        nothing_found = 1
        OTHERS        = 2.

    IF sy-subrc = 1.   " inconsistency STXBITMAPS - BDS; repeat check in

      wa_bds_signature-doc_count = '1'.
      APPEND wa_bds_signature TO l_bds_signature.

      CALL METHOD l_bds_object->create_with_table
        EXPORTING
          classname  = c_bds_classname
          classtype  = c_bds_classtype
          components = l_bds_components
          content    = l_bds_content
        CHANGING
          signature  = l_bds_signature
          object_key = l_object_key
        EXCEPTIONS
          OTHERS     = 1.

      IF sy-subrc <> 0.
        PERFORM dequeue_graphic USING p_object
              p_name
              p_id
              p_btype.
*        message e285 with p_name 'BDS'.

      ENDIF.

      READ TABLE l_bds_signature INDEX 1 INTO wa_bds_signature
      TRANSPORTING doc_id.
      IF sy-subrc = 0.
        p_docid = wa_bds_signature-doc_id.
      ELSE.

        PERFORM dequeue_graphic USING p_object
              p_name
              p_id
              p_btype.

      ENDIF.

    ELSEIF sy-subrc = 2.

      PERFORM dequeue_graphic USING p_object
            p_name
            p_id
            p_btype.

*      message e285 with p_name 'BDS'.

    ENDIF.

  ENDIF.

* Save bitmap header in STXBITPMAPS
  wa_stxbitmaps-tdname     = p_name.
  wa_stxbitmaps-tdobject   = p_object.
  wa_stxbitmaps-tdid       = p_id.
  wa_stxbitmaps-tdbtype    = p_btype.
  wa_stxbitmaps-docid      = p_docid.
  wa_stxbitmaps-widthpix   = l_width_pix.
  wa_stxbitmaps-heightpix  = l_height_pix.
  wa_stxbitmaps-widthtw    = l_width_tw.
  wa_stxbitmaps-heighttw   = l_height_tw.
  wa_stxbitmaps-resolution = p_resolution.
  wa_stxbitmaps-resident   = p_resident.
  wa_stxbitmaps-autoheight = p_autoheight.
  wa_stxbitmaps-bmcomp     = p_bmcomp.
  INSERT INTO stxbitmaps VALUES wa_stxbitmaps.

  IF sy-subrc <> 0.

    UPDATE stxbitmaps FROM wa_stxbitmaps.

    IF sy-subrc <> 0.

    ENDIF.

  ENDIF.



* Set description in BDS attributes

  wa_bds_properties-prop_name  = 'DESCRIPTION'.
  wa_bds_properties-prop_value = p_title.
  APPEND wa_bds_properties TO l_bds_properties.


  CALL METHOD l_bds_object->change_properties
    EXPORTING
      classname  = c_bds_classname
      classtype  = c_bds_classtype
      object_key = l_object_key
      doc_id     = p_docid
      doc_ver_no = '1'
      doc_var_id = '1'
    CHANGING
      properties = l_bds_properties
    EXCEPTIONS
      OTHERS     = 1.

  PERFORM dequeue_graphic USING p_object
        p_name
        p_id
        p_btype.



ENDFORM. "import_bitmap_bds

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_GRAPHIC
*&---------------------------------------------------------------------*
* Enqueue of graphics stored in BDS
*----------------------------------------------------------------------*

FORM enqueue_graphic USING p_object
      p_name
      p_id
      p_btype.


  CALL FUNCTION 'ENQUEUE_ESSGRABDS'
    EXPORTING
      tdobject     = p_object
      tdname       = p_name
      tdid         = p_id
      tdbtype      = p_btype
    EXCEPTIONS
      foreign_lock = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING enqueue_failed.
  ENDIF.

ENDFORM. " ENQUEUE_GRAPHIC



*&---------------------------------------------------------------------*
*&      Form  DEQUEUE_GRAPHIC
*&---------------------------------------------------------------------*
* Dequeue of graphics stored in BDS
*----------------------------------------------------------------------*

FORM dequeue_graphic USING p_object
      p_name
      p_id
      p_btype.

  CALL FUNCTION 'DEQUEUE_ESSGRABDS'
    EXPORTING
      tdobject = p_object
      tdname   = p_name
      tdid     = p_id
      tdbtype  = p_btype.

ENDFORM. " DEQUEUE_GRAPHIC