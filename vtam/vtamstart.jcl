* $$ JOB JNM=VTAM,CLASS=2,DISP=L
* $$ LST LST=00E,FNO=A,CLASS=A
// JOB VTAM
// ASSGN SYS000,UA
// ASSGN SYS001,X'362'
// DLBL TRFILE,'DOS/VS.WORK-FILE.2',,SD
// EXTENT SYS001,WORK01,1,0,3651,300
// EXEC ISTINCVT,SIZE=512K
/&
* $$ EOJ