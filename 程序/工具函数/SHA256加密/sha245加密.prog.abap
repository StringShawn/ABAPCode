DATA:
  lv_sign_key_x  TYPE xstring,
  lv_result      TYPE string,
  lv_hmac_result TYPE string.

DATA(lv_binary_secret) = cl_abap_hmac=>string_to_xstring('').

cl_abap_hmac=>calculate_hmac_for_char(
  EXPORTING
    if_algorithm           = 'SHA256'           "Hash Algorithm
    if_key                 = lv_binary_secret   "HMAC Key
    if_data                = 'accessToken=12af37987cd64e928184b2de30d7b578&calendarId=da1ae403ef1c4da0b1283b08c5130102&productId=1233&requestedAppId=12345a6057acd1de84e90845c086bd27f01e0'   "Data
  IMPORTING
    ef_hmacstring          = lv_result
    ef_hmacb64string       = lv_hmac_result  "HMAC value as base64-encoded string
).
TRANSLATE lv_result to LOWER CASE.
WRITE lv_result.