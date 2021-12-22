* $$ JOB JNM=POWERGEN,USER='OPER',CLASS=0,DISP=D
* $$ LST LST=00E,FNO=0001,CLASS=A
// JOB POWERGEN - ASSEMBLE POWER
// OPTION  CATAL
   ACTION  CLEAR
// EXEC    PGM=ASSEMBLY,SIZE=512K
         TITLE 'POWER/VS SYSTEM GENERATION'
         POWER                                                         X
               DBLK=2008,                                              X
               TRACKGP=1,                                              X
               LTAB=(10,00,05,10,15,20,25,30,35,40,45,50,56),          X
               PRI=3,                                                  X
               SUBLIB=S,                                               X
               ACCOUNT=YES,                                            X
               STDLINE=(50000,10000),                                  X
               STDCARD=(5000,1000),                                    X
               JLOG=YES,                                               X
               JSEP=(2,0),                                             X
               RBS=(0,0),                                              X
               RDREXIT=NO,                                             X
               PAUSE=NO,                                               X
               SPOOL=YES,                                              X
               FEED=NO,                                                X
               MULT12=YES
         PLINE                                                         X
               ADDR=X'070',                                            X
               TRNSP=YES,                                              X
               CODE=EBCDIC,                                            X
               PSWRD=SECRET,                                           X
               TIMEOUT=NO,                                             X
               SWITCH=NO,                                              X
               MODSET=AA                                             
         PRMT                                                          X
               REMOTE=1,                                               X
               TYPE=3780,                                              X
               PUNROUT=1,                                              X
               LSTROUT=1,                                              X
               LIST=132
         END
/*
// EXEC    PGM=LNKEDT
/*
/&
* $$ EOJ
