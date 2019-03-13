*@#@@[SAP]
*&---------------------------------------------------------------------*
*&  Include           ZFMPARAVALSAVE                                   *
*&---------------------------------------------------------------------*
DEFINE zfmparavalsave1.
  data: header_gd   type header_fb,
        tables_gd   type rsfb_para with header line,
        import_gd   type rsfb_para with header line,
        export_gd   type rsfb_para with header line,
        change_gd   type rsfb_para with header line,
        pname_gd    type tfdir-pname.
  data: begin of keystr,
          name   like zfmparavalsave-name  ,
          erdat  like zfmparavalsave-erdat ,
          stamp  like zfmparavalsave-stamp ,
          indx   like zfmparavalsave-indx  ,
          para   like zfmparavalsave-para,
        end of keystr.
  data: wa_data type zfmparavalsave ,
        wa_datal type zfmparavalsavel ,
        tsl type timestampl ,tsstr(30),
        indx type numc2 ,
        fsstr type string .
  field-symbols: <fs> type any .

  header_gd-name = &1 .
  get time stamp field tsl.
  keystr-name = header_gd-name.
*  tsstr = |{ tsl  TIMEZONE = sy-zonlo }|.
  write tsl time zone sy-zonlo to tsstr .
  keystr-erdat = sy-datum.
  keystr-stamp = tsstr+11(15).

  select single pname into pname_gd from tfdir
    where funcname =  header_gd-name.
  call function 'FUNCTION_INCLUDE_SPLIT'
    exporting
      program   = pname_gd
    importing
      group     = header_gd-area
      namespace = header_gd-namespace
    exceptions
      othe      = 12.
  if sy-subrc = 0.
    concatenate header_gd-namespace header_gd-area
            into header_gd-area.
    call method cl_fb_parameter_db=>read
      importing
        tables = tables_gd[]
        import = import_gd[]
        export = export_gd[]
        change = change_gd[]
      changing
        header = header_gd.
  endif.
END-OF-DEFINITION.

DEFINE zfmparavalsave2.
  select single pname into pname_gd from tfdir
    where funcname =  header_gd-name.
  call function 'FUNCTION_INCLUDE_SPLIT'
    exporting
      program   = pname_gd
    importing
      group     = header_gd-area
      namespace = header_gd-namespace
    exceptions
      othe      = 12.
  if sy-subrc = 0.
    concatenate header_gd-namespace header_gd-area
            into header_gd-area.

    call method cl_fb_parameter_db=>read
      importing
        tables = tables_gd[]
        import = import_gd[]
        export = export_gd[]
        change = change_gd[]
      changing
        header = header_gd.
  endif.

  select single * into wa_datal from zfmparavalsavel
    where name = header_gd-name and
            memo = &1 .
  if sy-subrc <> 0 and indx < 99.
    indx = indx + 1 .
    keystr-indx = indx.

    wa_data-area = header_gd-area.
    wa_data-ernam = sy-uname.
    wa_data-memo = &1 .
    wa_data-erdat = sy-datum.

    assign ('RTYPE') to <fs>.
    if sy-subrc = 0.
      wa_data-rtype = <fs>.
    endif.

    assign ('RTMSG') to <fs>.
    if sy-subrc = 0.
      wa_data-rtmsg = <fs>.
    endif.

    loop at import_gd.
      assign (import_gd-parameter) to <fs>.
      check sy-subrc = 0 .
      keystr-para = import_gd-parameter.
      export <fs> to database zfmparavalsave(fl) id keystr from wa_data.
    endloop.

    loop at change_gd.
      assign (change_gd-parameter) to <fs>.
      check sy-subrc = 0 .
      keystr-para = change_gd-parameter.
      export <fs> to database zfmparavalsave(fl) id keystr from wa_data.
    endloop.

    loop at export_gd.
      assign (export_gd-parameter) to <fs>.
      check sy-subrc = 0 .
      keystr-para = export_gd-parameter.
      export <fs> to database zfmparavalsave(fl) id keystr from wa_data.
    endloop.

    loop at tables_gd.
      concatenate tables_gd-parameter '[]' into fsstr.
      assign (fsstr) to <fs>.
      check sy-subrc = 0 .
      keystr-para = tables_gd-parameter.
      export <fs> to database zfmparavalsave(fl) id keystr from wa_data.
    endloop.
  endif.
  IF &1 = 'B'.
    COMMIT WORK.
  ENDIF.
*
END-OF-DEFINITION.
