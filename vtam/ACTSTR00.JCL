* $$ JOB JNM=ATCSTR00,DISP=D,CLASS=0
* $$ LST LST=SYSLST,FNO=A,CLASS=A,JSEP=1
// JOB ATCSTR00 CATALOG B.ATCSTR00
// ASSGN SYSIPT,X'00C'
// ASSGN SYSLST,X'00E'
// EXEC MAINT
 CATALS B.ATCSTR00
 BKEND
CONFIG=00,                                                             +
SSCPID=01, /*ID IN NETWORK*/                                           +
NOPROMPT,                                                              +
NETSOL=YES,                                                            +
SUPP=NOSUP,                                                            +
MAXSUBA=31, /*MAX NETWORK SUBAREAS*/                                   +
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
