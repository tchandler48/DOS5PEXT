* $$ JOB JNM=SPFINST,CLASS=0,DISP=D,USER='NELSON DHEGAS'
* $$ LST CLASS=A,DISP=D,JSEP=1
// JOB CATMAPS INSTALL THE SPF-PACKET
// PAUSE Mount SPF Installation Tape on 280 ...
// OPTION LOG,CATAL
// ASSGN SYSIPT,X'280'
// MTC REW,SYSIPT
 INCLUDE
/*
// EXEC LNKEDT
/*
/&
// JOB CATALR OBJECT MODULS
// ASSGN SYSIPT,X'280'
// EXEC MAINT
/*
/&
// JOB CATALC VSTSO
// OPTION CATAL,LOG
// ASSGN SYSIPT,X'280'
 PHASE VSTSO,*
 INCLUDE VSTSO
// EXEC LNKEDT
/*
/&
// JOB CATALS
// ASSGN SYSIPT,X'280'
// EXEC MAINT
/*
/&
* $$ EOJ