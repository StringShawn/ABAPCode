*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2019/06/11 at 09:02:54
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZT15405_SQR.....................................*
DATA:  BEGIN OF STATUS_ZT15405_SQR                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZT15405_SQR                   .
CONTROLS: TCTRL_ZT15405_SQR
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZT15405_XM......................................*
DATA:  BEGIN OF STATUS_ZT15405_XM                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZT15405_XM                    .
CONTROLS: TCTRL_ZT15405_XM
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZT15405_XMJL....................................*
DATA:  BEGIN OF STATUS_ZT15405_XMJL                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZT15405_XMJL                  .
CONTROLS: TCTRL_ZT15405_XMJL
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZT15405_SQR                   .
TABLES: *ZT15405_XM                    .
TABLES: *ZT15405_XMJL                  .
TABLES: ZT15405_SQR                    .
TABLES: ZT15405_XM                     .
TABLES: ZT15405_XMJL                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
