* $$ JOB JNM=DSERVALL,CLASS=0,USER='DSERV'
* $$ LST CLASS=A
// JOB DSERVALL
// EXEC DSERV
 DSPLYS ALL
/*
// DLBL IJSYSSL,'DOSVS.OPTIONAL.DOSVS'
// EXTENT SYSSLB,OPTLB1
// ASSGN SYSSLB,DISK,VOL=OPTLB1,SHR
// EXEC DSERV
 DSPLYS SD
/*
// DLBL IJSYSSL,'DOSVS.OPTIONAL.POWER.BTAM.QTAM'
// EXTENT SYSSLB,OPTLB1
// ASSGN SYSSLB,DISK,VOL=OPTLB1,SHR
// EXEC DSERV
 DSPLYS SD
/*
// DLBL IJSYSSL,'DOSVS.OPTIONAL.VSAM'
// EXTENT SYSSLB,OPTLB1
// ASSGN SYSSLB,DISK,VOL=OPTLB1,SHR
// EXEC DSERV
 DSPLYS SD
/*
// DLBL IJSYSSL,'DOSVS.OPTIONAL.VTAM'
// EXTENT SYSSLB,OPTLB1
// ASSGN SYSSLB,DISK,VOL=OPTLB1,SHR
// EXEC DSERV
 DSPLYS SD
/*
/&
* $$ EOJ