* $$ JOB JNM=ATCCON00,DISP=D,CLASS=0
* $$ LST LST=00E,FNO=A
// JOB ATCCON00 CATALOG B.ATCCON00
// ASSGN SYSLST,X'00E'
// EXEC MAINT
 CATALS B.ATCCON00
 BKEND
APPLPWR,                                                               X
LCLMAJ00
 BKEND
/*
/&
* $$ EOJ