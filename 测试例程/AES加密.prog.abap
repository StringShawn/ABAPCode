DATA: s         TYPE string,
      h(1)      TYPE x,
      c(1)      TYPE c,
      byte(2)   TYPE c,
      length    TYPE i,
      l_bindata TYPE xstring,
      l_cntbin  TYPE sdokcntbins.

"STRING 转XSTRING
DATA(lr_conv_sec) = cl_abap_conv_out_ce=>create( ).
lr_conv_sec->write( data = '123456' ).

*  " create key  STRING 转XSTRING
DATA(lr_conv_key) = cl_abap_conv_out_ce=>create( ).
lr_conv_key->write( data = 'ABCDEFGHIJKLMNOP' )."AES加密规则

" encrypt using AES256（加密）
cl_sec_sxml_writer=>encrypt(
  EXPORTING
    plaintext =  lr_conv_sec->get_buffer( )
    key =        lr_conv_key->get_buffer( )
    algorithm =  cl_sec_sxml_writer=>co_aes256_algorithm
  IMPORTING
    ciphertext = DATA(iv_xml_request_aes) ).

WRITE :iv_xml_request_aes.

cl_sec_sxml_writer=>encrypt(
  EXPORTING
    plaintext =  lr_conv_sec->get_buffer( )
    key =        lr_conv_key->get_buffer( )
    algorithm =  cl_sec_sxml_writer=>CO_AES128_ALGORITHM
  IMPORTING
    ciphertext = iv_xml_request_aes ).
WRITE :/ iv_xml_request_aes.

cl_sec_sxml_writer=>encrypt(
  EXPORTING
    plaintext =  lr_conv_sec->get_buffer( )
    key =        lr_conv_key->get_buffer( )
    algorithm =  cl_sec_sxml_writer=>CO_AES192_ALGORITHM
  IMPORTING
    ciphertext = iv_xml_request_aes ).
WRITE :/ iv_xml_request_aes.

cl_sec_sxml_writer=>encrypt(
  EXPORTING
    plaintext =  lr_conv_sec->get_buffer( )
    key =        lr_conv_key->get_buffer( )
    algorithm =  cl_sec_sxml_writer=>CO_AES128_ALGORITHM_PEM
  IMPORTING
    ciphertext = iv_xml_request_aes ).
WRITE :/ iv_xml_request_aes.

cl_sec_sxml_writer=>encrypt(
  EXPORTING
    plaintext =  lr_conv_sec->get_buffer( )
    key =        lr_conv_key->get_buffer( )
    algorithm =  cl_sec_sxml_writer=>CO_AES192_ALGORITHM_PEM
  IMPORTING
    ciphertext = iv_xml_request_aes ).
WRITE :/ iv_xml_request_aes.

cl_sec_sxml_writer=>encrypt(
  EXPORTING
    plaintext =  lr_conv_sec->get_buffer( )
    key =        lr_conv_key->get_buffer( )
    algorithm =  cl_sec_sxml_writer=>CO_AES256_ALGORITHM_PEM
  IMPORTING
    ciphertext = iv_xml_request_aes ).
WRITE :/ iv_xml_request_aes.