* $$ JOB JNM=LCLMAJ00,DISP=D,CLASS=0
* $$ LST LST=00E,FNO=A
// JOB LCLMAJ00 CATALOG B.LCLMAJ00
// ASSGN SYSLST,X'00E'
// EXEC MAINT
 CATALS B.LCLMAJ00
 BKEND
LCLMAJ00 LBUILD
CUU080 LOCAL TERM=3277,CUADDR=080,LOGAPPL=NETSOL
CUU081 LOCAL TERM=3277,CUADDR=081,LOGAPPL=NETSOL
CUU082 LOCAL TERM=3277,CUADDR=082,LOGAPPL=NETSOL
 BKEND
/*
/&
* $$ EOJ
