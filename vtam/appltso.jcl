* $$ JOB JNM=APPLTSO,DISP=D,CLASS=0
* $$ LST LST=00E,FNO=A
// JOB APPLTSO CATALOG B.APPLTSO
// ASSGN SYSLST,X'00E'
// EXEC MAINT
 CATALS B.APPLTSO
 BKEND
 TSO APPL AUTH=(ACQ,PASS),BUFFACT=5
 TSO0001 APPL AUTH=(ACQ,PASS),BUFFACT=5
 TSO0002 APPL AUTH=(ACQ,PASS),BUFFACT=5
 TSO0003 APPL AUTH=(ACQ,PASS),BUFFACT=5
 TSO0004 APPL AUTH=(ACQ,PASS),BUFFACT=5
 TSO0005 APPL AUTH=(ACQ,PASS),BUFFACT=5
 TSO0006 APPL AUTH=(ACQ,PASS),BUFFACT=5
 TSO0007 APPL AUTH=(ACQ,PASS),BUFFACT=5
 BKEND
/*
/&
* $$ EOJ