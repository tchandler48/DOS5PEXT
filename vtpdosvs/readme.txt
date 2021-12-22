
That version runs on DOS/VS R34 will not work on DOS/VSE library differences!

To install:

   1, copy sort01.2314.cckd to /dasd/ directory

    2. Add 130 2314 to config file

    3. Add 080 3270 to config file

    4. Submit install.jcl to rdr,00c

    5. Submit Rvtp.jcl to rdr,00c

        You may have to insert in Rvtp.jcl the following:

// ASSGN SYSSLB,X'364'
// DLBL IJSYSSL,'DOSVS.OPTIONAL.DOSVS',99/365
// EXTENT SYSSLB,OPTLB1,1,0

Also be advised, will not run under VM, does not support cross part communications! 

