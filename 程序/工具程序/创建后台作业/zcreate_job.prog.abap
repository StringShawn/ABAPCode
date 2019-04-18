*&---------------------------------------------------------------------*
*& Report  ZALV_BACKGROUND
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zalv_background1.

TABLES : mara,
  tsp01.
TYPE-POOLS:slis.
TYPES : BEGIN OF t_mara,
  matnr   TYPE mara-matnr,
  ersda   TYPE mara-ersda,
  ernam   TYPE mara-ernam,
  laeda   TYPE mara-laeda,
END OF t_mara.
DATA : i_mara       TYPE STANDARD TABLE OF t_mara,
      wa_mara      TYPE t_mara,
      wa_index     TYPE indx,        " For Index details
      wa_index_key TYPE indx-srtfd VALUE 'PRG_ONE',
      i_jobsteplist     TYPE STANDARD TABLE OF tbtcstep, " For spool number
      wa_params         TYPE pri_params,  " To Get Print Parameters
      wa_jobhead        TYPE tbtcjob,     " To know the status of job
      wa_jobsteplist    TYPE tbtcstep,    " To know the spool
      w_jobname         TYPE tbtco-jobname,  " Job name for bckgrnd job
      w_jobcount        TYPE tbtco-jobcount, " Unique id for bckgrd job
      w_path            TYPE string,         " Upload path
      w_lsind           TYPE sy-lsind,       " Index
      wa_seltab         TYPE rsparams,
      i_seltab          TYPE STANDARD TABLE OF rsparams,
      wa_index1         TYPE indx,        " For Index details
      wa_index_key1     TYPE indx-srtfd VALUE 'PRG_TWO',
      i_fieldcat        TYPE slis_t_fieldcat_alv,
      wa_fieldcat       LIKE LINE OF i_fieldcat.
*----------------------------------------------------------------------*
*         CONSTANTS DECLARATION                                        *
*----------------------------------------------------------------------*
CONSTANTS :
c_a(1) TYPE c VALUE 'A',
c_m(1) TYPE c VALUE 'M',
c_l(1) TYPE c VALUE 'L',
c_c(1) TYPE c VALUE 'C',
c_zfdr(4) TYPE c VALUE 'ZFDR',
c_x(1)    TYPE c VALUE 'X',
c_locl(4) TYPE c VALUE 'ZCOW', " Destination is LOCAL
c_f(1)    TYPE c VALUE 'F',   " Job Status - Failed
c_s(1)    TYPE c VALUE 'S',
c_p(1)    TYPE c VALUE 'P'.
*----------------------------
* SELECTION SCREEN PARAMETERS
*----------------------------
SELECT-OPTIONS : s_matnr FOR mara-matnr.

START-OF-SELECTION.
*-------------------------------------------------------
* Before the export, fill the data fields before CLUSTR
*-------------------------------------------------------
  wa_index-aedat = sy-datum.
  wa_index-usera = sy-uname.
  EXPORT s_matnr
  TO DATABASE indx(st) FROM wa_index ID wa_index_key.
*-----------------------------------------------
* To Open the Job for background processing
*-----------------------------------------------
  PERFORM open_job.
*-----------------------------------------------
* To get the print parameters
*-----------------------------------------------
  PERFORM get_print_parameters.
*------------------------------
* Submit the job in background
*------------------------------
  PERFORM job_submit.
*---------------------------
* Close the background job
*---------------------------
  PERFORM job_close.
***************************************************************
****  This is the output screen with the buttons *******
***************************************************************
* Create 3 buttons DISPLAY SPOOL, STATUS, JOBLOG
  SET PF-STATUS 'ZS001'.
  WRITE: / 'The program is submitted in Background'.
  WRITE: / 'Press DISPLAY SPOOL to see the spool'.
  WRITE: / 'Press STATUS to see the status of the background'.

AT USER-COMMAND.
*-------------------------------------
* If user presses the 'BACK' button
*-------------------------------------
  IF sy-ucomm = 'BAK'.
    IF  wa_jobhead-status = c_f OR
    wa_jobhead-status = c_a.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.
*-------------------------------------------------
* If the user presses the 'DISPLAY SPOOL' Button
*-------------------------------------------------
  IF sy-ucomm = 'DISPLAY'.
    PERFORM display_spool.
  ENDIF.
*-----------------------------------------------
* If the user presses the 'JOB STATUS' Button
*-----------------------------------------------
  IF sy-ucomm = 'STATUS'.
    PERFORM display_status.
  ENDIF.
*----------------------------------------------
* If the user presses the 'JOB LOG' Button
*----------------------------------------------
  IF sy-ucomm = 'JOBLOG'.
    PERFORM display_job_log.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  open_job
*&---------------------------------------------------------------------*
FORM open_job .
*-----------------------------------------------------------------------
* This is to Create a new job which is to be submitted in background to
* process sales order/delivery/invoice
* Here we would get a unique id ( Jobcount ) which identifies our job
* along with the job name which we have assigned to our job
*-----------------------------------------------------------------------
  CONCATENATE sy-uname
  sy-datum
  sy-uzeit
  INTO w_jobname .  " Assign unique jobname
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = w_jobname
    IMPORTING
      jobcount         = w_jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " open_job
*&---------------------------------------------------------------------*
*&      Form  get_print_parameters
*&---------------------------------------------------------------------*
FORM get_print_parameters .
  DATA : l_valid TYPE c.
*-----------------------------------------------------------------
* This is to get the Print Parameters for the job which is to be
* submitted in background to process sales order/delivery/invoice
*-----------------------------------------------------------------
  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      destination            = c_locl " LOCL
      immediately            = space
      new_list_id            = c_x
      no_dialog              = c_x
      user                   = sy-uname
    IMPORTING
      out_parameters         = wa_params
      valid                  = l_valid
    EXCEPTIONS
      archive_info_not_found = 1
      invalid_print_params   = 2
      invalid_archive_params = 3
      OTHERS                 = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " get_print_parameters
*&---------------------------------------------------------------------*
*&      Form  job_submit
*&---------------------------------------------------------------------*
FORM job_submit .
*-----------------------------------------------------------------------
* The job which we have created & the unique id ( jobcount ) which we
* have got identifies our job. Hence those parameters are passed along
* with the name of the background program "ZPROGRAM_TWO"
* The job is submitted in background.
*-----------------------------------------------------------------------
  CALL FUNCTION 'JOB_SUBMIT'
    EXPORTING
      authcknam               = sy-uname
      jobcount                = w_jobcount
      jobname                 = w_jobname
      priparams               = wa_params
      report                  = 'ZALV_BACKGROUND1'
    EXCEPTIONS
      bad_priparams           = 1
      bad_xpgflags            = 2
      invalid_jobdata         = 3
      jobname_missing         = 4
      job_notex               = 5
      job_submit_failed       = 6
      lock_failed             = 7
      program_missing         = 8
      prog_abap_and_extpg_set = 9
      OTHERS                  = 10.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " job_submit
*&---------------------------------------------------------------------*
*&      Form  job_close
*&---------------------------------------------------------------------*
FORM job_close .
*-----------------------------------------------------------------
* Once the job is submitted in background then the job is closed
*-----------------------------------------------------------------
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount             = w_jobcount
      jobname              = w_jobname
      strtimmed            = c_x
    EXCEPTIONS
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      job_nosteps          = 5
      job_notex            = 6
      lock_failed          = 7
      invalid_target       = 8
      OTHERS               = 9.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " job_close
*&---------------------------------------------------------------------*
*&      Form  display_spool
*&---------------------------------------------------------------------*
FORM display_spool .
*-------------------------------------------
* To Read the Job to get the spool details
*-------------------------------------------
  DATA : l_rqident TYPE tsp01-rqident, " Spool Number
        l_spoolno TYPE tsp01_sp0r-rqid_char.
  CLEAR : l_rqident,
  w_lsind,
  wa_jobsteplist.
  REFRESH : i_jobsteplist.
  SET PF-STATUS 'ZAR02'.
*--------------------------------
* Get the Spool Number
*--------------------------------
  CALL FUNCTION 'BP_JOB_READ'
    EXPORTING
      job_read_jobcount     = w_jobcount
      job_read_jobname      = w_jobname
      job_read_opcode       = '20'
    IMPORTING
      job_read_jobhead      = wa_jobhead
    TABLES
      job_read_steplist     = i_jobsteplist
    EXCEPTIONS
      invalid_opcode        = 1
      job_doesnt_exist      = 2
      job_doesnt_have_steps = 3
      OTHERS                = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*------------------------------------------------
* Read the Job Step list to get the spool number
*------------------------------------------------
  READ TABLE i_jobsteplist INTO wa_jobsteplist INDEX 1.
  CHECK wa_jobsteplist-listident <> space.
*-----------------
* Spool Number
*-----------------
  l_rqident = wa_jobsteplist-listident.
  MOVE l_rqident TO l_spoolno.
*---------------------------
* Check the spool in TSP01
*---------------------------
  SELECT SINGLE * FROM tsp01 WHERE rqident = l_rqident.
  IF  sy-subrc = 0.
    LEAVE TO LIST-PROCESSING.
    CALL FUNCTION 'RSPO_R_RDELETE_SPOOLREQ'
      EXPORTING
        spoolid = l_spoolno.
    PERFORM show_alv.
  ENDIF.
  w_lsind = sy-lsind.
  IF sy-lsind GE 19.
    sy-lsind = 1.
  ENDIF.
ENDFORM.                    " display_spool
*&---------------------------------------------------------------------*
*&      Form  show_alv
*&---------------------------------------------------------------------*
FORM show_alv .
*---------------------------------------------------------
* Before the import, fill the data fields before CLUSTR.
*---------------------------------------------------------
  wa_index1-aedat = sy-datum.
  wa_index1-usera = sy-uname.
*----------------------------------------------------------
* To Import the selection screen data from Calling Program
*----------------------------------------------------------
  IMPORT i_mara
  FROM DATABASE indx(st) ID wa_index_key1 TO wa_index1.
  FREE MEMORY ID wa_index_key1.
* This prepares the field-catalog for ALV.
  PERFORM prepare_fieldcatalog.
* This displays the output in  ALV format .
  PERFORM display_alv.
ENDFORM.                    " show_alv
*&---------------------------------------------------------------------*
*&      Form  display_status
*&---------------------------------------------------------------------*
FORM display_status .
*------------------------------------------------------------------
* To Display the STATUS of the JOB which is exectued in background
*------------------------------------------------------------------
  CLEAR : wa_jobsteplist.
  REFRESH : i_jobsteplist.
  WRITE:/ 'DISPLAYING JOB STATUS'.
  CALL FUNCTION 'BP_JOB_READ'
    EXPORTING
      job_read_jobcount     = w_jobcount
      job_read_jobname      = w_jobname
      job_read_opcode       = '20'
    IMPORTING
      job_read_jobhead      = wa_jobhead
    TABLES
      job_read_steplist     = i_jobsteplist
    EXCEPTIONS
      invalid_opcode        = 1
      job_doesnt_exist      = 2
      job_doesnt_have_steps = 3
      OTHERS                = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*----------------------------------------------------
* To Display the status text as per the status type
*----------------------------------------------------
  CASE wa_jobhead-status.
    WHEN 'S'. WRITE: / 'Scheduled'.
    WHEN 'R'. WRITE: / 'Released'.
    WHEN 'F'. WRITE: / 'Completed'.
    WHEN 'A'. WRITE: / 'Cancelled'.
    WHEN OTHERS.
  ENDCASE.
  IF sy-lsind GE 19.
    sy-lsind = 1.
  ENDIF.
ENDFORM.                    " display_status
*&---------------------------------------------------------------------*
*&      Form  display_job_log
*&---------------------------------------------------------------------*
FORM display_job_log .
*-----------------------------------------------
* To display the log of the background program
*-----------------------------------------------
  LEAVE TO LIST-PROCESSING.
  CALL FUNCTION 'BP_JOBLOG_SHOW_SM37B'
    EXPORTING
      client                    = sy-mandt
      jobcount                  = w_jobcount
      joblogid                  = ' '
      jobname                   = w_jobname
    EXCEPTIONS
      error_reading_jobdata     = 1
      error_reading_joblog_data = 2
      jobcount_missing          = 3
      joblog_does_not_exist     = 4
      joblog_is_empty           = 5
      joblog_show_canceled      = 6
      jobname_missing           = 7
      job_does_not_exist        = 8
      no_joblog_there_yet       = 9
      no_show_privilege_given   = 10
      OTHERS                    = 11.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " display_job_log
*&---------------------------------------------------------------------*
*&      Form  prepare_fieldcatalog
*&---------------------------------------------------------------------*
FORM prepare_fieldcatalog .
  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname    = 'MATNR'.
  wa_fieldcat-tabname      = 'I_MARA'.
  wa_fieldcat-reptext_ddic = 'Material no.'.
  wa_fieldcat-outputlen    = '18'.
  APPEND wa_fieldcat TO i_fieldcat.
  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname    = 'ERSDA'.
  wa_fieldcat-tabname      = 'I_MARA'.
  wa_fieldcat-reptext_ddic = 'Creation date'.
  wa_fieldcat-outputlen    = '10'.
  APPEND  wa_fieldcat TO i_fieldcat.
  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname    = 'ERNAM'.
  wa_fieldcat-tabname      = 'I_MARA'.
  wa_fieldcat-reptext_ddic = 'Name of Person'.
  wa_fieldcat-outputlen    = '10'.
  APPEND wa_fieldcat TO i_fieldcat.
  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname    = 'LAEDA'.
  wa_fieldcat-tabname      = 'I_MARA'.
  wa_fieldcat-reptext_ddic = ' Last Change'.
  wa_fieldcat-outputlen    = '10'.
  APPEND  wa_fieldcat TO i_fieldcat.
ENDFORM.                    " prepare_fieldcatalog
*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
FORM display_alv .
* Call ABAP List Viewer (ALV)
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat = i_fieldcat
    TABLES
      t_outtab    = i_mara.
ENDFORM.                    " display_alv