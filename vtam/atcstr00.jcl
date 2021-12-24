* $$ JOB JNM=ATCSTR00,DISP=D,CLASS=0
* $$ LST FNO=A,CLASS=A
// JOB ATCSTR00 CATALOG B.ATCSTR00
// ASSGN SYSLST,X'00E'
// EXEC MAINT
 CATALS B.ATCSTR00
 BKEND
CONFIG=00,                                                             +
SSCPID=02,   /*ID IN NETWORK  */                                       +
NETSOL=YES, /*NETWORK SOLICITOR */                                     +
MAXSUBA=31, /*MAX NETWORK SUBAREAS */                                  +
NOPROMPT,                                                              +
COLD,                                                                  +
APBUF=(128,,064),                                                      +
LFBUF=(016,,16),                                                       +
LPBUF=(032,,32),                                                       +
NPBUF=(32,,08),                                                        +
PPBUF=(032,256,08),                                                    +
SFBUF=(032,,32),                                                       +
SPBUF=(032,,32),                                                       +
UECBUF=(032,,16)
 BKEND
/*
/&
* $$ EOJ