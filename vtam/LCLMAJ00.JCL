* $$ JOB JNM=LCLMAJ00,DISP=D,CLASS=0
* $$ LST LST=SYSLST,FNO=A,JSEP=1
// JOB LCLMAJ00 CATALOG B.LCLMAJ00
// ASSGN SYSIPT,X'00C'
// ASSGN SYSLST,X'00E'
// EXEC MAINT
 CATALS B.LCLMAJ00
 BKEND
LCLMAJ00 LBUILD
CUU080 LOCAL TERM=3277,CUADDR=080,LOGTAB=BUDINTAB,LOGAPPL=NETSOL,      X
               FEATUR2=(MODEL2,PFK),ISTATUS=ACTIVE
 BKEND
/*
/&
* $$ EOJ
