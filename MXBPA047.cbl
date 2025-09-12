000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID.  MXBPA047.                                           00020000
000300 DATE-COMPILED.                                                   00030000
000400*                                                                 00040000
000500*================================================================*00050000
000600* MAX SYSTEM STANDARD PROGRAM DESCRIPTION AREA                   |00060000
000700*================================================================*00070000
000800* PROGRAM  | MXBPA047 | TRANS ID | N/A  |                        |00080000
000900*----------+----------+----------+------+------------------------*00090000
001000* DESCRIPTION:                                                   |00100000
001100*   THIS IS THE APPLIED CREDIT STATEMENT PROGRAM.  IT TAKES THE  |00110000
001200*   SORTED OUTPUT FROM THE EXTRACT PROGRAM AND FORMATS IT INTO   |00120000
001300*   THE REPORT LAYOUT NEEDED FOR THE XEROX COLOR LASER PRINTER.  |00130000
001400*                                                                |00140000
001500*   THE 80 BYTE I/O FILE IS USED TO RUN THIS PROGRAM IN          |00150000
001600*   REPRINT MODE.  IF THE FILE IS EMPTY, THEN ALL THE DEALERS    |00160000
001700*   ARE PROCESSED.  IF NOT, THE FILE SHOULD CONTAIN ONE RECORD.  |00170000
001800*   ON THIS RECORD WILL BE THE DEALER NUMBERS OF THE DEALERS'    |00180000
001900*   STATEMENT OF CREDIT THAT ARE TO BE REPRINTED.  THE FORMAT    |00190000
002000*   IS AS FOLLOWS:                                               |00200000
002100*   000001 000342 006583 000023 .....                            |00210000
002200*   THE COMPLETE 6 BYTE DEALER NUMBER MUST BE ENTERED (THIS      |00220000
002300*   INCLUDES THE LEADING ZEROES) FOLLOWED BY A SPACE.  ANY       |00230000
002400*   NUMBER WITH AN ERROR WILL NOT BE PROCESSED.                  |00240000
002500*-----------------------------------------------------------------00250000
002600*                                                                 00260000
002700*================================================================*00270000
002800*   CALL   | PROGRAM DESCRIPTION                                 |00280000
002900*----------+-----------------------------------------------------*00290000
003000* MXBPCABT | MAX STANDARD ABEND ROUTINE                          |00300000
003100*-----------------------------------------------------------------00310000
003200* **** INPUT ****                                                 00320000
003300*================================================================*00330000
003400*  VIEW    |   TABLE  |     DESCRIPTION                          |00340000
003500*----------+-----------------------------------------------------*00350000
003600* N/A      | N/A      |                                          |00360000
003700*-----------------------------------------------------------------00370000
003800* **** OUTPUT ****                                                00380000
003900*================================================================*00390000
004000*  VIEW    |   TABLE  |     DESCRIPTION                          |00400000
004100*----------+-----------------------------------------------------*00410000
004200* N/A      | N/A      |                                          |00420000
004300*-----------------------------------------------------------------00430000
004400*                                                                 00440000
004500*================================================================*00450000
004600*   PLAN   | BIND INCLUDE MEMBERS                                |00460000
004700*----------+-----------------------------------------------------*00470000
004800* PBMSA047 | PBMSA047, PBMSCABT                                  |00480000
004900*-----------------------------------------------------------------00490000
005000*                                                                 00500000
011500/                                                                 01150000
011600 ENVIRONMENT DIVISION.                                            01160000
011700 CONFIGURATION SECTION.                                           01170000
011800 SOURCE-COMPUTER.   IBM-3090.                                     01180000
011900 OBJECT-COMPUTER.   IBM-3090.                                     01190000
012000 INPUT-OUTPUT SECTION.                                            01200000
012100 FILE-CONTROL.                                                    01210000
012200                                                                  01220000
012300     SELECT CREDIT-INFILE                                         01230000
012400            ASSIGN TO UT-S-MXA047I1                               01240000
012500            ORGANIZATION IS SEQUENTIAL                            01250000
012600            ACCESS IS SEQUENTIAL.                                 01260000
012700                                                                  01270000
012800     SELECT REPRINT-FILE                                          01280000
012900            ASSIGN TO UT-S-MXA047I2.                              01290000
013000                                                                  01300000
013100     SELECT XEROX-OUTFILE                                         01310000
013200            ASSIGN TO UT-S-MXA047X1.                              01320000
013300                                                                  01330000
013400 DATA DIVISION.                                                   01340000
013500 FILE SECTION.                                                    01350000
013600 FD  CREDIT-INFILE                                                01360000
013700     RECORDING MODE IS F                                          01370000
013800     LABEL RECORDS ARE STANDARD                                   01380000
013900     BLOCK CONTAINS 0 RECORDS                                     01390000
014000     DATA RECORD IS CREDIT-REC.                                   01400000
014100**** P0516718 S                                                   01410000
014100*01  CREDIT-REC                          PIC X(140).              01410100
014100 01  CREDIT-REC                          PIC X(162).              01410200
014100**** P0516718 E                                                   01410300
014200                                                                  01420000
014300 FD  REPRINT-FILE                                                 01430000
014400     RECORDING MODE IS F                                          01440000
014500     LABEL RECORDS ARE STANDARD                                   01450000
014600     BLOCK CONTAINS 0 RECORDS                                     01460000
014700     DATA RECORD IS REPRINT-REC.                                  01470000
014800 01  REPRINT-REC                         PIC X(80).               01480000
014900                                                                  01490000
015000 FD  XEROX-OUTFILE                                                01500000
015100     RECORDING MODE IS V                                          01510000
015200     LABEL RECORDS ARE STANDARD                                   01520000
015300     BLOCK CONTAINS 0 RECORDS.                                    01530000
015310*02.791-S                                                         01531000
015401 01  XEROX-REC-000                       PIC X(292).              01540100
015410*02.791-E                                                         01541000
015500 01  XEROX-REC-T01                       PIC X(38).               01550000
015600 01  XEROX-REC-T02                       PIC X(66).               01560000
015700 01  XEROX-REC-H01                       PIC X(13).               01570000
015800 01  XEROX-REC-H02                       PIC X(23).               01580000
015900 01  XEROX-REC-H03                       PIC X(18).               01590000
016000 01  XEROX-REC-D03                       PIC X(18).               01600000
016100 01  XEROX-REC-H04                       PIC X(23).               01610000
016200 01  XEROX-REC-D04                       PIC X(51).               01620000
016300 01  XEROX-REC-H05                       PIC X(23).               01630000
016400 01  XEROX-REC-D05                       PIC X(53).               01640000
016500 01  XEROX-REC-H06                       PIC X(79).               01650000
016600 01  XEROX-REC-H07                       PIC X(102).              01660000
016700 01  XEROX-REC-D07                       PIC X(68).               01670000
016800 01  XEROX-REC-D08                       PIC X(67).               01680000
016800**** P0516718 S                                                   01680100
016800 01  XEROX-REC-D09                       PIC X(68).               01680200
016800**** P0516718 E                                                   01680210
016900 01  XEROX-REC-D17                       PIC X(67).               01690000
016901*** 03.857 S                                                      01690100
016910 01  XEROX-REC-D18                       PIC X(293).              01691000
016920*** 03.857 S                                                      01692000
017000 01  XEROX-REC-ZZZ                       PIC X(03).               01700000
017100 EJECT                                                            01710000
017200 WORKING-STORAGE SECTION.                                         01720000
017300****************************************************************  01730000
017400*    MISC. VARIABLES                                           *  01740000
017500****************************************************************  01750000
017600 01  WS-REPRINT-REC-LAYOUT.                                       01760000
017700     05  WS-REPRINT-REC-ITEM OCCURS 11 TIMES.                     01770000
017800         10  WS-REPRINT-DLR-INP PIC  X(06).                       01780000
017900         10  FILLER             PIC  X(01).                       01790000
018000 01  WS-REPRINT-DLR-TABLE.                                        01800000
018100     05  WS-REPRINT-DLR-ITEM OCCURS 11 TIMES.                     01810000
018200         10  WS-REPRINT-DLR     PIC S9(09) COMP.                  01820000
018300 01  WS-MISC-VARS.                                                01830000
018400     05  WS-DB2DATE-ALL-ZEROES  PIC  X(10) VALUE '0000000000'.    01840000
018500     05  WS-DB2DATE-ZEROES-6    PIC  X(06) VALUE '000000'.        01850000
018600     05  WS-DB2DATE-ZEROES-8    PIC  X(08) VALUE '00000000'.      01860000
018700     05  WS-DB2DATE-ZEROES-10   PIC  X(10) VALUE '00/00/0000'.    01870000
018800     05  WS-PROGRAM-ID          PIC  X(08) VALUE 'MXBPA047'.      01880000
018900     05  WS-DATEPGM-ID          PIC  X(08) VALUE 'MXBPW003'.      01890000
019000     05  WS-FORM-DETERMINE-PGM  PIC  X(08) VALUE 'MXBPW026'.      01900000
019100     05  WS-CNTL-REPORT-PGM     PIC  X(08) VALUE 'MXBPB510'.      01910000
      * FGTN-195 - S
           05  WS-ENVIRONMENT-RTN     PIC X(08)  VALUE 'MXBPW030'.
      * FGTN-195 - E
019200     05  SUB3                   PIC S9(04) COMP VALUE ZEROES.     01920000
019300     05  SUB4                   PIC S9(04) COMP VALUE ZEROES.     01930000
019400     05  MAX-SUB                PIC S9(04) COMP VALUE ZEROES.     01940000
019500     05  WS-CURRENT-DLR         PIC S9(09) COMP VALUE ZEROES.     01950000
019600     05  WS-DLR-CNTL-ENT        PIC S9(04) COMP.                  01960000
019700     05  WS-CURRENT-DIST-NAME   PIC  X(35) VALUE SPACES.          01970000
019800     05  WS-CURRENT-MFG-NAME    PIC  X(35) VALUE SPACES.          01980000
019900     05  WS-CURRENT-CREDIT-MEMO PIC  X(11) VALUE SPACES.          01990000
020000     05  WS-CURRENT-APPLIED-DATE                                  02000000
020100                                PIC  X(10) VALUE SPACES.          02010000
020200     05  WS-CURR-LANG-IND       PIC  X(06) VALUE SPACES.          02020000
020300     05  WS-HOLD-BILL-DATE      PIC  X(10).                       02030000
020400     05  WS-NUM-STAR            PIC *(8)9  VALUE ZEROS.           02040000
020500     05  WS-STAR-CNT            PIC S9(4)  VALUE +0 COMP.         02050000
020600     05  WS-CUST-NO-LJ          PIC X(06).                        02060000
020700     05  WS-UNIQUE-KEY-BUILD.                                     02070000
020800         10  WS-UNIQUEKEY-CUST  PIC 9(06).                        02080000
020900         10  FILLER             PIC X(19)  VALUE SPACES.          02090000
021000     05  WS-PROC-DATE-NN        PIC S9(4)  VALUE +0 COMP.         02100000
      * FGTN-195 - S
           05  WS-COMPANY-NO          PIC S9(4)  VALUE +0  COMP.
           05  WS-EMAIL-LEN           PIC S9(4)  USAGE COMP.
      * FGTN-195 - E
021100/***************************************************************  02110000
021200*    LINES FOR XEROX PRINTER                                   *  02120000
021300****************************************************************  02130000
021400*** HEADER LINE (000)                                             02140000
021500 01  WS-XEROX-000-LINE.                                           02150000
021600     05  FILLER                 PIC X(03) VALUE '000'.            02160000
021610*02.791-S                                                         02161000
021710     05  FILLER                 PIC X(289) VALUE SPACES.          02171000
021720*02.791-E                                                         02172000
021800*** TITLE LINE                                                    02180000
021900 01  WS-XEROX-T01-LINE.                                           02190000
022000     05  FILLER                 PIC X(03) VALUE 'T01'.            02200000
022100     05  WS-T01-TITLE-LIT       PIC X(35) VALUE SPACES.           02210000
022200*** TITLE LINE PAGES 2-N                                          02220000
022300 01  WS-XEROX-T02-LINE.                                           02230000
022400     05  FILLER                 PIC X(03) VALUE 'T02'.            02240000
022500     05  WS-T02-TITLE-LIT-1     PIC X(14) VALUE SPACES.           02250000
022600     05  WS-T02-TITLE-LIT-2     PIC X(08) VALUE SPACES.           02260000
022700     05  WS-T02-CUST            PIC X(06) VALUE ZEROES.           02270000
022800     05  WS-T02-CUST-NAME       PIC X(35) VALUE SPACES.           02280000
022900*** HEADING 1 LINE (PAGE)                                         02290000
023000 01  WS-XEROX-H01-LINE.                                           02300000
023100     05  FILLER                 PIC X(03) VALUE 'H01'.            02310000
023200     05  WS-H01-LITERAL         PIC X(10) VALUE SPACES.           02320000
023300*** HEADING 2 LINE (NOT USED)                                     02330000
023400 01  WS-XEROX-H02-LINE.                                           02340000
023500     05  FILLER                 PIC X(03) VALUE 'H02'.            02350000
023600     05  WS-H02-LITERAL         PIC X(20) VALUE SPACES.           02360000
023700*** HEADING 3 LINE                                                02370000
023800 01  WS-XEROX-H03-LINE.                                           02380000
023900     05  FILLER                 PIC X(03) VALUE 'H03'.            02390000
024000     05  WS-H03-LITERAL         PIC X(15) VALUE SPACES.           02400000
024100*** DATA 3 LINES                                                  02410000
024200 01  WS-XEROX-D03-LINE-1.                                         02420000
024300     05  FILLER                 PIC X(03) VALUE 'D03'.            02430000
024400     05  WS-D03-CUST            PIC X(06) VALUE ZEROES.           02440000
024500     05  FILLER                 PIC X(09) VALUE SPACES.           02450000
024600 01  WS-XEROX-D03-LINE-2.                                         02460000
024700     05  FILLER                 PIC X(03) VALUE 'D03'.            02470000
024800     05  WS-D03-ADDL-CUST-NO    PIC X(13) VALUE SPACES.           02480000
024900     05  FILLER                 PIC X(02) VALUE SPACES.           02490000
025000*** HEADING 4 LINE                                                02500000
025100 01  WS-XEROX-H04-LINE.                                           02510000
025200     05  FILLER                 PIC X(03) VALUE 'H04'.            02520000
025300     05  WS-H04-LITERAL         PIC X(20) VALUE SPACES.           02530000
025400*** DATA 4 LINES                                                  02540000
025500 01  WS-XEROX-D04-LINE-1.                                         02550000
025600     05  FILLER                 PIC X(03) VALUE 'D04'.            02560000
025700     05  FILLER                 PIC X(48) VALUE SPACES.           02570000
025800 01  WS-XEROX-D04-LINE-2.                                         02580000
025900     05  FILLER                 PIC X(03) VALUE 'D04'.            02590000
026000     05  WS-D04-DLR-REP-NAME    PIC X(48) VALUE SPACES.           02600000
026100 01  WS-XEROX-D04-LINE-3.                                         02610000
026200     05  FILLER                 PIC X(03) VALUE 'D04'.            02620000
026300     05  WS-D04-DLR-REP-PHONE   PIC X(48) VALUE SPACES.           02630000
026400*** HEADING 5 LINE (ADDRESS)                                      02640000
026500 01  WS-XEROX-H05-LINE.                                           02650000
026600     05  FILLER                 PIC X(03) VALUE 'H05'.            02660000
026700     05  WS-H05-LITERAL         PIC X(20) VALUE SPACES.           02670000
026800*** DATA 5 LINES (ADDRESS)                                        02680000
026900 01  WS-XEROX-D05-LINE.                                           02690000
027000     05  FILLER                 PIC X(03) VALUE 'D05'.            02700000
027100     05  WS-D05-NAME-ADDR       PIC X(50) VALUE SPACES.           02710000
027200*** HEADING 6 LINE                                                02720000
027300 01  WS-XEROX-H06-LINE-1.                                         02730000
027400     05  FILLER                 PIC X(03) VALUE 'H06'.            02740000
027500     05  WS-H06-LITERAL-1       PIC X(16) VALUE SPACES.           02750000
027600     05  WS-H06-CUST-NAME       PIC X(35) VALUE SPACES.           02760000
027700     05  FILLER                 PIC X(25) VALUE SPACES.           02770000
027800 01  WS-XEROX-H06-LINE-2.                                         02780000
027900     05  FILLER                 PIC X(03) VALUE 'H06'.            02790000
028000     05  WS-H06-LITERAL-2       PIC X(16) VALUE SPACES.           02800000
028100     05  WS-H06-CREDIT-MEMO     PIC X(35) VALUE SPACES.           02810000
028200     05  WS-H06-LITERAL-3       PIC X(17) VALUE SPACES.           02820000
028300     05  WS-H06-APPLIED-DATE    PIC X(08) VALUE SPACES.           02830000
027800**** P0516718 S                                                   02830100
027800 01  WS-XEROX-H06-LINE-3.                                         02830110
027900     05  FILLER                 PIC X(03) VALUE 'H06'.            02830200
028000     05  WS-H06-LITERAL-31      PIC X(16) VALUE SPACES.           02830300
028100     05  WS-H06-ORG-CREDIT-MEMO PIC X(22) VALUE SPACES.           02830400
028200     05  FILLER                 PIC X(38) VALUE SPACES.           02830500
027800**** P0516718 E                                                   02830700
028400*** HEADING 7 LINES (COLUMN HEADINGS)                             02840000
028500 01  WS-XEROX-H07-LINE.                                           02850000
028600     05  FILLER                 PIC X(03) VALUE 'H07'.            02860000
028700     05  WS-H07-LITERAL-1       PIC X(14) VALUE SPACES.           02870000
028800     05  WS-H07-LITERAL-2       PIC X(09) VALUE SPACES.           02880000
028900     05  WS-H07-LITERAL-3       PIC X(18) VALUE SPACES.           02890000
029000     05  WS-H07-LITERAL-4       PIC X(30) VALUE SPACES.           02900000
029100     05  WS-H07-LITERAL-5       PIC X(17) VALUE SPACES.           02910000
029200     05  WS-H07-LITERAL-6       PIC X(11) VALUE SPACES.           02920000
029300*** DATA 7 LINE                                                   02930000
029400 01  WS-XEROX-D07-LINE.                                           02940000
029500     05  FILLER                 PIC X(03) VALUE 'D07'.            02950000
029600     05  WS-D07-INVOICE-NBR     PIC X(11) VALUE SPACES.           02960000
029700     05  WS-D07-BILL-DATE       REDEFINES                         02970000
029800         WS-D07-INVOICE-NBR     PIC X(11).                        02980000
029900     05  WS-D07-LINE-NBR        PIC 9(04).                        02990000
030000     05  WS-D07-LINE-NBR-X      REDEFINES                         03000000
030100         WS-D07-LINE-NBR        PIC X(04).                        03010000
030200     05  WS-D07-MODEL-NBR       PIC X(12) VALUE SPACES.           03020000
030300     05  WS-D07-SERIAL-NBR      PIC X(20) VALUE SPACES.           03030000
030400     05  WS-D07-APPLIED-AMT     PIC ---,---,--9.99                03040000
030500                                          VALUE ZEROES.           03050000
030600     05  WS-D07-CHARGE-TYPE     PIC X(04) VALUE SPACES.           03060000
030700*** DATA 8 LINE                                                   03070000
030800 01  WS-XEROX-D08-LINE.                                           03080000
030900     05  FILLER                 PIC X(03) VALUE 'D08'.            03090000
031000     05  WS-D08-CM-TYPE         PIC X(50) VALUE SPACES.           03100000
031000     05  WS-D08-APPLIED-AMT     PIC ---,---,--9.99                03100100
031000                                          VALUE ZEROES.           03100200
031100     05  WS-D08-APPLIED-AMT-X   REDEFINES                         03110000
031300         WS-D08-APPLIED-AMT     PIC X(14).                        03130000
027800**** P0516718 S                                                   03140100
030800 01  WS-XEROX-D09-LINE.                                           03140200
030900     05  FILLER                 PIC X(03) VALUE 'D09'.            03140210
031000     05  WS-D09-LITERAL         PIC X(16) VALUE SPACES.           03140220
031100     05  WS-D09-ORGINV-NO       PIC X(22) VALUE SPACES.           03140230
031300     05  FILLER                 PIC X(27) VALUE SPACES.           03140240
027800**** P0516718 E                                                   03140900
031500*** DATA 17 LINE                                                  03150000
031600 01  WS-XEROX-D17-LINE.                                           03160000
031700     05  FILLER                 PIC X(03) VALUE 'D17'.            03170000
031800     05  WS-D17-LITERAL         PIC X(50) VALUE SPACES.           03180000
031900     05  WS-D17-APPLIED-AMT     PIC ---,---,--9.99                03190000
031901                                          VALUE ZEROES.           03190100
031910*** 03.857 S-  DATA 18 LINE                                       03191000
031920 01  WS-XEROX-D18-LINE.                                           03192000
031930     05  FILLER                 PIC X(03) VALUE 'D18'.            03193000
031931     05  WS-D18-LITERAL1        PIC X(70) VALUE SPACES.           03193100
031932     05  WS-D18-LITERAL2        PIC X(70) VALUE SPACES.           03193200
031933     05  WS-D18-LITERAL3        PIC X(70) VALUE SPACES.           03193300
031934     05  WS-D18-LITERAL4        PIC X(70) VALUE SPACES.           03193400
031935     05  WS-D18-LITERAL5        PIC X(10) VALUE SPACES.           03193500
031940*** 03.857 E                                                      03194000
032100*** FILE TRAILER (ZZZ)                                            03210000
032200 01  WS-XEROX-ZZZ-LINE.                                           03220000
032300     05  FILLER                 PIC X(03) VALUE 'ZZZ'.            03230000
032400                                                                  03240000
032500****************************************************************  03250000
032600*    FLAGS & SWITCHES                                          *  03260000
032700****************************************************************  03270000
032800 01  WS-MISC-FLAGS.                                               03280000
032900     05  CREDIT-INFILE-FLAG          PIC X(01) VALUE SPACE.       03290000
033000         88 EOF-CREDIT-INFILE        VALUE 'Y'.                   03300000
033100                                                                  03310000
033200     05  REPRINT-FILE-FLAG           PIC X(01) VALUE SPACE.       03320000
033300         88 EOF-REPRINT-FILE         VALUE 'Y'.                   03330000
033400                                                                  03340000
033500     05  WS-REPRINT-FLAG             PIC X(01) VALUE 'N'.         03350000
033600         88 WS-CREDIT-REPRINT        VALUE 'Y'.                   03360000
033700         88 WS-NO-CREDIT-REPRINT     VALUE 'N'.                   03370000
033800                                                                  03380000
033900     05  WS-OUTPUT-FLAG           PIC X(01) VALUE 'N'.            03390000
034000         88  OUTPUT-CREATED           VALUE 'Y'.                  03400000
034100                                                                  03410000
034200     05  WS-FIRST-CREDIT-MEMO-FLAG                                03420000
034300                                  PIC X(01) VALUE 'N'.            03430000
034400         88  FIRST-CREDIT-MEMO        VALUE 'Y'.                  03440000
034500         88  NOT-FIRST-CREDIT-MEMO    VALUE 'N'.                  03450000
034600                                                                  03460000
034700     05  WS-FIRST-DIST-OF-PAGE-FLAG                               03470000
034800                                  PIC X(01) VALUE 'N'.            03480000
034900         88  FIRST-DIST-OF-PAGE       VALUE 'Y'.                  03490000
035000         88  NOT-FIRST-DIST-OF-PAGE   VALUE 'N'.                  03500000
035100                                                                  03510000
035200     05  TYPE-3-SW                PIC X(01) VALUE 'N'.            03520000
035300                                                                  03530000
035400     05  WS-DTL-HDGS-FLAG            PIC X(01) VALUE 'N'.         03540000
035500         88 YES-DTL-HDGS-WRITTEN     VALUE 'Y'.                   03550000
035600         88 NO-DTL-HDGS-WRITTEN      VALUE 'N'.                   03560000
035700     05  WS-T02-WRITTEN-FLAG         PIC X(01) VALUE 'N'.         03570000
035800         88 YES-T02-WRITTEN          VALUE 'Y'.                   03580000
035900         88 NO-T02-WRITTEN           VALUE 'N'.                   03590000
      * FGTN-195 - S
           05  WS-SHAW-CUSTOMER            PIC X(01) VALUE SPACES.
               88  SHAW-CUST-YES           VALUE 'Y'.
               88  SHAW-CUST-NO            VALUE 'N'.
           05  WS-REP-NAME                 PIC X(01) VALUE SPACES.
               88  REP-NAME-FOUND          VALUE 'Y'.
               88  REP-NAME-NOTFOUND       VALUE 'N'.
           05  WS-REP-EMAIL                PIC X(01) VALUE SPACES.
               88  REP-EMAIL-FOUND         VALUE 'Y'.
               88  REP-EMAIL-NOTFOUND      VALUE 'N'.
      * FGTN-195 - E
036000*                                                                 03600000
036100****************************************************************  03610000
036200*    COUNTERS & TOTALS                                         *  03620000
036300****************************************************************  03630000
036400 01  WS-MISC-COUNTERS.                                            03640000
036500                                                                  03650000
036600     05  TOT-CREDIT-MEMO           PIC S9(09)V99 VALUE ZEROES.    03660000
036700     05  TOT-CREDIT-DLR            PIC S9(09)V99 VALUE ZEROES.    03670000
036800     05  ACCUM-CHARGES             PIC S9(11)V99 VALUE ZEROES.    03680000
036900                                                                  03690000
037000****************************************************************  03700000
037100*    DB2 CONTROL AREAS                                         *  03710000
037200****************************************************************  03720000
037300 01  DB2-STATUS-CODES.                                            03730000
037400     05  VWMCN007-STATUS             PIC S9(9) COMP-4 VALUE +0.   03740000
037500                                                                  03750000
037600 01  DB2-STATUS-VALUES.                                           03760000
037700     05  SQL-OK                      PIC S9(9) COMP-4 VALUE +0.   03770000
037800     05  SQL-NOT-FND                 PIC S9(9) COMP-4 VALUE +100. 03780000
037900     05  SQL-CRSR-CLOSE              PIC S9(9) COMP-4 VALUE -501. 03790000
038000     05  SQL-CRSR-OPEN               PIC S9(9) COMP-4 VALUE -502. 03800000
038100/***************************************************************  03810000
038200*    XEROX 00 RECORD LAYOUT                                    *  03820000
038300****************************************************************  03830000
038400     EXEC SQL                                                     03840000
038500          INCLUDE MXBW510                                         03850000
038600     END-EXEC.                                                    03860000
038700/                                                                 03870000
038800     EXEC SQL                                                     03880000
038900          INCLUDE MXWW01                                          03890000
039000     END-EXEC.                                                    03900000
039100/***************************************************************  03910000
039200*    DB2 COMMUNICATION AREA                                    *  03920000
039300****************************************************************  03930000
039400     EXEC SQL                                                     03940000
039500         INCLUDE SQLCA                                            03950000
039600     END-EXEC.                                                    03960000
039700/                                                                 03970000
039800*    EXEC SQL                                                     03980000
039900*        INCLUDE MXWW23                                           03990000
040000*    END-EXEC.                                                    04000000
040100/***************************************************************  04010000
040200* INPUT RECORD LAYOUT.                                         *  04020000
040300****************************************************************  04030000
040400     EXEC SQL                                                     04040000
040500          INCLUDE MXAW21                                          04050000
040600     END-EXEC.                                                    04060000
040700/***************************************************************  04070000
040800* LITERALS TABLE COPYBOOK                                      *  04080000
040900****************************************************************  04090000
041000     EXEC SQL                                                     04100000
041100          INCLUDE MXLTA047                                        04110000
041200     END-EXEC.                                                    04120000
041300/********************************************                     04130000
041400*   FORM DETERMINATION WORK AREA            *                     04140000
041500*********************************************                     04150000
041600                                                                  04160000
041700     EXEC SQL                                                     04170000
041800        INCLUDE MXWW26                                            04180000
041900     END-EXEC.                                                    04190000
042000                                                                  04200000
042100/********************************************                     04210000
042200*                 ENVIRONMENT DETERMINATION *                     04220000
042300*********************************************                     04230000
042400                                                                  04240000
042500* RIS 00.636 START                                                04250000
042600     EXEC SQL                                                     04260000
042700        INCLUDE MXWW31                                            04270000
042800     END-EXEC.                                                    04280000
042900                                                                  04290000
043000     EXEC SQL                                                     04300000
043100        INCLUDE MXWW30                                            04310000
043200     END-EXEC.                                                    04320000
043300* RIS 00.636 END                                                  04330000
043400                                                                  04340000
043500****************************************************************  04350000
043600*    DB2 DCLGEN AREAS                                          *  04360000
043700****************************************************************  04370000
043800     EXEC SQL                                                     04380000
043900          INCLUDE VWMJ096                                         04390000
044000     END-EXEC.                                                    04400000
044100                                                                  04410000
044200     EXEC SQL                                                     04420000
044300          INCLUDE VWMCTUPD                                        04430000
044400     END-EXEC.                                                    04440000
044500                                                                  04450000
      * FGTN-195 - S
           EXEC SQL
                INCLUDE VWMCU00
           END-EXEC.

           EXEC SQL
                INCLUDE VWMCN00
           END-EXEC.

           EXEC SQL
                INCLUDE VWMCTRPA
           END-EXEC.

           EXEC SQL
                INCLUDE VWMRP00
           END-EXEC.

      * FGTN-195 - E
044600****************************************************************  04460000
044700*    ABNORMAL TERMINATION WORK AREA.                           *  04470000
044800****************************************************************  04480000
044900     EXEC SQL                                                     04490000
045000          INCLUDE MXWW03                                          04500000
045100     END-EXEC.                                                    04510000
045200                                                                  04520000
045300 LINKAGE SECTION.                                                 04530000
045400                                                                  04540000
045500 PROCEDURE DIVISION.                                              04550000
045600     EXIT.                                                        04560000
045700****************************************************************  04570000
045800*    PROGRAM MAIN CONTROL ROUTINE FOR APPLIED CREDIT STATEMENT *  04580000
045900****************************************************************  04590000
046000 0000-MAINLINE.                                                   04600000
046100                                                                  04610000
046200     PERFORM 0100-INITIALIZE THRU 0100-EXIT.                      04620000
046300                                                                  04630000
046400     PERFORM 1000-PROCESS-RECORDS THRU 1000-EXIT                  04640000
046500         UNTIL EOF-CREDIT-INFILE.                                 04650000
046600                                                                  04660000
046700     PERFORM 9900-TERMINATE THRU 9900-EXIT.                       04670000
046800                                                                  04680000
046900     GOBACK.                                                      04690000
047000                                                                  04700000
047100 0000-MAINLINE-EXIT.  EXIT.                                       04710000
047200/***************************************************************  04720000
047300*  OPEN FILES, INITIALIZE VARIABLES, LOAD XEROX FORMS TABLE,   *  04730000
047400*  READ INPUT RECORDS UNTIL A DEALER RECORD IS FOUND.          *  04740000
047500*  IF THERE IS DATA, SETUP THE FIRST DEALER.                   *  04750000
047600****************************************************************  04760000
047700 0100-INITIALIZE.                                                 04770000
047800                                                                  04780000
047900     OPEN INPUT  CREDIT-INFILE                                    04790000
048000                 REPRINT-FILE                                     04800000
048100          OUTPUT XEROX-OUTFILE.                                   04810000
048200                                                                  04820000
048300     SET  MXBW510-FIRST-TIME         TO TRUE.                     04830000
048400     MOVE 'N'                        TO MXBW510-EOJ-FLAG.         04840000
048500                                                                  04850000
048600     PERFORM 0110-GET-DATE               THRU 0110-EXIT.          04860000
048700                                                                  04870000
048800     INITIALIZE MXAW21-CREDIT-GENERAL-RECORD.                     04880000
048900                                                                  04890000
049000     READ REPRINT-FILE                                            04900000
049100         INTO WS-REPRINT-REC-LAYOUT                               04910000
049200             AT END SET EOF-REPRINT-FILE TO TRUE.                 04920000
049300                                                                  04930000
049400     IF EOF-REPRINT-FILE                                          04940000
049500         SET WS-NO-CREDIT-REPRINT TO TRUE                         04950000
049600         DISPLAY 'NORMAL PROCESSING'                              04960000
049700     ELSE                                                         04970000
049800         PERFORM 8600-LOAD-REPRINT-TABLE THRU 8600-EXIT           04980000
049900         DISPLAY 'REPRINT PROCESSING'                             04990000
050000         DISPLAY 'REPRINT DEALERS = '                             05000000
050100         DISPLAY WS-REPRINT-REC-LAYOUT                            05010000
050200     END-IF.                                                      05020000
050300                                                                  05030000
050400     CLOSE REPRINT-FILE.                                          05040000
050500                                                                  05050000
050600     PERFORM 8000-READ-INPUT THRU 8000-EXIT                       05060000
050700         UNTIL (MXAW21-SK-RECORD-TYPE = '00' AND                  05070000
050800               MXAW21-SK-RECORD-TYPE-SEQ = 01)                    05080000
050900            OR EOF-CREDIT-INFILE.                                 05090000
051000                                                                  05100000
051100     IF NOT EOF-CREDIT-INFILE                                     05110000
051200         SET OUTPUT-CREATED TO TRUE                               05120000
051300         PERFORM 1100-SETUP-DEALER THRU 1100-EXIT                 05130000
051400         PERFORM 8000-READ-INPUT THRU 8000-EXIT                   05140000
051500     ELSE                                                         05150000
051600         DISPLAY '  '                                             05160000
051700         DISPLAY '  '                                             05170000
051800         DISPLAY '**************************************'         05180000
051900         DISPLAY 'THERE WAS NO DATA TO PRINT FOR'                 05190000
052000         DISPLAY 'THIS RUN OF STATEMENT OF CREDIT.'               05200000
052100         DISPLAY 'DUMMY FILE CONTENTS WILL BE WRITTEN TO'         05210000
052200         DISPLAY 'DA045060.CNTL & XEROX FILES IN FOLLOWING'       05220000
052300         DISPLAY 'JOB STEPS.'                                     05230000
052400         DISPLAY '**************************************'         05240000
052500         DISPLAY '  '                                             05250000
052600         DISPLAY '  '                                             05260000
052700         MOVE 05 TO RETURN-CODE                                   05270000
052800     END-IF.                                                      05280000
052900                                                                  05290000
053000 0100-EXIT. EXIT.                                                 05300000
053100/***************************************************************  05310000
053200*  GET THE CASH PROCESS DATE                                   *  05320000
053300****************************************************************  05330000
053400 0110-GET-DATE.                                                   05340000
053500                                                                  05350000
053600     EXEC SQL                                                     05360000
053700       SELECT PROC_DATE                                           05370000
053800         INTO :DCLVWMCTUPD.PROC-DATE     INDICATOR                05380000
053900                                        :WS-PROC-DATE-NN          05390000
054000         FROM VWMCTUPD                                            05400000
054100         WHERE SUBSYSTEM_ID_IND = 'A'                             05410000
054200           AND SUBFUNCTION_CODE = '        '                      05420000
054300     END-EXEC.                                                    05430000
054400                                                                  05440000
054500     EVALUATE TRUE                                                05450000
054600       WHEN SQLCODE = +0                                          05460000
054700         CONTINUE                                                 05470000
054800       WHEN SQLCODE = +100                                        05480000
054900         EXEC SQL                                                 05490000
055000           SELECT PROC_DATE                                       05500000
055100             INTO :DCLVWMCTUPD.PROC-DATE     INDICATOR            05510000
055200                                            :WS-PROC-DATE-NN      05520000
055300             FROM VWMCTUPD                                        05530000
055400             WHERE SUBSYSTEM_ID_IND = ' '                         05540000
055500               AND SUBFUNCTION_CODE = '        '                  05550000
055600         END-EXEC                                                 05560000
055700         IF  SQLCODE NOT = +0                                     05570000
055800             SET  ABT-ERROR-IS-DB2  TO TRUE                       05580000
055900             SET  ABT-DO-ABEND      TO TRUE                       05590000
056000             MOVE SQLCODE           TO ABT-ERROR-ABEND-CODE       05600000
056100             MOVE 'SELECT  '        TO ABT-DA-FUNCTION            05610000
056200             MOVE 'VWMCTUPD'        TO ABT-DA-ACCESS-NAME         05620000
056300             MOVE '0110-   '        TO ABT-ERROR-SECTION          05630000
056400             PERFORM Z-980-ABNORMAL-TERM                          05640000
056500         END-IF                                                   05650000
056600       WHEN OTHER                                                 05660000
056700         SET  ABT-ERROR-IS-DB2      TO TRUE                       05670000
056800         SET  ABT-DO-ABEND          TO TRUE                       05680000
056900         MOVE SQLCODE               TO ABT-ERROR-ABEND-CODE       05690000
057000         MOVE 'SELECT  '            TO ABT-DA-FUNCTION            05700000
057100         MOVE 'VWMCTUPD'            TO ABT-DA-ACCESS-NAME         05710000
057200         MOVE '0110-   '            TO ABT-ERROR-SECTION          05720000
057300         PERFORM Z-980-ABNORMAL-TERM                              05730000
057400     END-EVALUATE.                                                05740000
057500                                                                  05750000
057600 0110-EXIT. EXIT.                                                 05760000
057700/***************************************************************  05770000
057800*  ROUTINE PROCESSES THE DATA RECORDS FROM THE EXTRACT PROGRAM *  05780000
057900*  TO CREATE THE XEROX DATA RECORDS.                           *  05790000
057910* 03.857 -       - 12/31/03 - ADDED ROUTINE 1600- TO WRITE NEW *  05791000
057920*                  D18 RECORD WHICH CONTAINS BRIDGE TEXT.      *  05792000
058000****************************************************************  05800000
058100 1000-PROCESS-RECORDS.                                            05810000
058200                                                                  05820000
058300* IF NEW DEALER, WRITE OUT THE CREDIT MEMO AND DEALER TOTALS,  *  05830000
058400     IF MXAW21-SK-DLR-NBR NOT = WS-CURRENT-DLR                    05840000
058500         PERFORM 3000-WRITE-CREDIT-MEMO-TOTAL THRU 3000-EXIT      05850000
058600**       ** 'GRAND TOTAL CREDITS'                                 05860000
058700         MOVE 37                 TO TABLE-ENTRY-WANTED            05870000
058800         MOVE WS-CURR-LANG-IND   TO MXCW021-LANG-CODE             05880000
058900         PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT         05890000
059000         MOVE MXCW021-LANG-TEXT  TO WS-D17-LITERAL                05900000
059100         MOVE TOT-CREDIT-DLR     TO WS-D17-APPLIED-AMT            05910000
059200         MOVE ZEROES             TO TOT-CREDIT-DLR                05920000
059300         WRITE XEROX-REC-D17                                      05930000
059400             FROM WS-XEROX-D17-LINE                               05940000
059410         PERFORM 1600-WRITE-D18 THRU 1600-EXIT                    05941000
059500         SET  NO-T02-WRITTEN     TO TRUE                          05950000
059600         SET NO-DTL-HDGS-WRITTEN TO TRUE                          05960000
059700     END-IF.                                                      05970000
059800                                                                  05980000
059900* CHECK THE RECORD CODES FOR PROCESSING.                       *  05990000
060000                                                                  06000000
060100     EVALUATE MXAW21-SK-RECORD-TYPE                               06010000
060200         ALSO MXAW21-SK-RECORD-TYPE-SEQ                           06020000
060300                                                                  06030000
060400* FIRST RECORD, CONTAINS DEALER NUMBER AND ENTITY                 06040000
060500       WHEN '00' ALSO 01                                          06050000
060600         PERFORM 1100-SETUP-DEALER THRU 1100-EXIT                 06060000
060700                                                                  06070000
060800* WRITE OUT ADDITIONAL CUSTOMER NUMBER AND LITERAL                06080000
060900       WHEN '00' ALSO 02                                          06090000
061000         IF  MXAW21-0002-ADDL-CUST-NO NOT = SPACES                06100000
061100             PERFORM 1200-WRITE-ADDL-CUST THRU 1200-EXIT          06110000
061200         END-IF                                                   06120000
061300                                                                  06130000
061400* DEALER REP AND REP PHONE                                        06140000
061500       WHEN '00' ALSO 03                                          06150000
061600         PERFORM 1300-WRITE-REP THRU 1300-EXIT                    06160000
061700                                                                  06170000
061800* DEALER NAME                                                     06180000
061900       WHEN '00' ALSO 04                                          06190000
062000         MOVE 8                  TO TABLE-ENTRY-WANTED            06200000
062100         MOVE WS-CURR-LANG-IND   TO MXCW021-LANG-CODE             06210000
062200         PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT         06220000
062300         MOVE MXCW021-LANG-TEXT  TO WS-H05-LITERAL                06230000
062400         WRITE XEROX-REC-H05                                      06240000
062500             FROM WS-XEROX-H05-LINE                               06250000
062600         MOVE MXAW21-0004-DLR-NAME-ADDR TO WS-D05-NAME-ADDR       06260000
062700                                           WS-T02-CUST-NAME       06270000
062800         IF  NO-T02-WRITTEN                                       06280000
062900             PERFORM 1500-WRITE-T02-REC    THRU 1500-EXIT         06290000
063000             SET  YES-T02-WRITTEN       TO TRUE                   06300000
063100         END-IF                                                   06310000
063200                                                                  06320000
063300         WRITE XEROX-REC-D05                                      06330000
063400             FROM WS-XEROX-D05-LINE                               06340000
063500                                                                  06350000
063600* DEALER ADDRESS1                                                 06360000
063700       WHEN '00' ALSO 05                                          06370000
063800         IF  MXAW21-0004-DLR-NAME-ADDR NOT = SPACES               06380000
063900             MOVE MXAW21-0004-DLR-NAME-ADDR TO WS-D05-NAME-ADDR   06390000
064000             WRITE XEROX-REC-D05                                  06400000
064100                 FROM WS-XEROX-D05-LINE                           06410000
064200         END-IF                                                   06420000
064300                                                                  06430000
064400* DEALER ADDRESS2                                                 06440000
064500       WHEN '00' ALSO 06                                          06450000
064600         IF  MXAW21-0004-DLR-NAME-ADDR NOT = SPACES               06460000
064700             MOVE MXAW21-0004-DLR-NAME-ADDR TO WS-D05-NAME-ADDR   06470000
064800             WRITE XEROX-REC-D05                                  06480000
064900                 FROM WS-XEROX-D05-LINE                           06490000
065000         END-IF                                                   06500000
065100                                                                  06510000
065200* DEALER ADDRESS3                                                 06520000
065300       WHEN '00' ALSO 07                                          06530000
065400         IF  MXAW21-0004-DLR-NAME-ADDR NOT = SPACES               06540000
065500             MOVE MXAW21-0004-DLR-NAME-ADDR TO WS-D05-NAME-ADDR   06550000
065600             WRITE XEROX-REC-D05                                  06560000
065700                 FROM WS-XEROX-D05-LINE                           06570000
065800         END-IF                                                   06580000
065900                                                                  06590000
066000* DEALER ADDRESS4                                                 06600000
066100       WHEN '00' ALSO 08                                          06610000
066200         IF  MXAW21-0004-DLR-NAME-ADDR NOT = SPACES               06620000
066300             MOVE MXAW21-0004-DLR-NAME-ADDR TO WS-D05-NAME-ADDR   06630000
066400             WRITE XEROX-REC-D05                                  06640000
066500                 FROM WS-XEROX-D05-LINE                           06650000
066600         END-IF                                                   06660000
066700                                                                  06670000
066800* DEALER ADDRESS5                                                 06680000
066900       WHEN '00' ALSO 09                                          06690000
067000         IF  MXAW21-0004-DLR-NAME-ADDR NOT = SPACES               06700000
067100             MOVE MXAW21-0004-DLR-NAME-ADDR TO WS-D05-NAME-ADDR   06710000
067200             WRITE XEROX-REC-D05                                  06720000
067300                 FROM WS-XEROX-D05-LINE                           06730000
067400         END-IF                                                   06740000
067500                                                                  06750000
067600* DISTRIBUTOR NAME WITH DETAIL COUNT                              06760000
067700       WHEN '01' ALSO 01                                          06770000
067800         IF  TOT-CREDIT-MEMO NOT = ZERO                           06780000
067900**       ** PRINT TOTAL FROM PREVIOUS CREDIT MEMO                 06790000
068000             PERFORM 3000-WRITE-CREDIT-MEMO-TOTAL THRU 3000-EXIT  06800000
068100         END-IF                                                   06810000
068200         IF  NO-DTL-HDGS-WRITTEN                                  06820000
068300**       ** PRINT DETAIL COLUMN HEADINGS                          06830000
068400             PERFORM 1400-DETAIL-HEADINGS  THRU 1400-EXIT         06840000
068500             SET  YES-DTL-HDGS-WRITTEN  TO TRUE                   06850000
068600         END-IF                                                   06860000
068700**       ** 'DIST NAME:'                                          06870000
068800         MOVE 9                       TO TABLE-ENTRY-WANTED       06880000
068900         MOVE WS-CURR-LANG-IND        TO MXCW021-LANG-CODE        06890000
069000         PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT         06900000
069100         MOVE MXCW021-LANG-TEXT       TO WS-H06-LITERAL-1         06910000
069200         MOVE MXAW21-0101-DIST-NAME   TO WS-CURRENT-DIST-NAME     06920000
069300         MOVE SPACES                  TO WS-CURRENT-MFG-NAME      06930000
069400                                         WS-CURRENT-CREDIT-MEMO   06940000
069500                                         WS-CURRENT-APPLIED-DATE  06950000
069600         MOVE WS-CURRENT-DIST-NAME    TO WS-H06-CUST-NAME         06960000
069700         WRITE XEROX-REC-H06                                      06970000
069800             FROM WS-XEROX-H06-LINE-1                             06980000
069900                                                                  06990000
070000* CREDIT MEMO                                                     07000000
070100       WHEN '01' ALSO 03                                          07010000
070200**       ** 'CREDIT NO:'                                          07020000
070300         MOVE 11                        TO TABLE-ENTRY-WANTED     07030000
070400         MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE      07040000
070500         PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT         07050000
070600         MOVE MXCW021-LANG-TEXT         TO WS-H06-LITERAL-2       07060000
070700         MOVE MXAW21-0103-CREDIT-NBR    TO WS-CURRENT-CREDIT-MEMO 07070000
070800                                           WS-H06-CREDIT-MEMO     07080000
070900**       ** 'APPLIED DATE:'                                       07090000
071000         MOVE 12                        TO TABLE-ENTRY-WANTED     07100000
071100         MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE      07110000
071200         PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT         07120000
071300         MOVE MXCW021-LANG-TEXT         TO WS-H06-LITERAL-3       07130000
071400* RIS 00.636 START                                                07140000
071500         MOVE 8                         TO MXWW31-FIELD-LENGTH    07150000
071600         MOVE MXAW21-0103-APPLIED-DATE  TO MXWW31-DB2-DATE        07160000
071700         MOVE WS-DLR-CNTL-ENT           TO                        07170000
071800                                           MXWW31-CNTL-ENT-NO     07180000
071900         PERFORM MXWP31-ENVIRONMENT-DATE   THRU MXWP31-EXIT       07190000
072000**IF RETURNED DATE IS SPACES, ZERO FILL BASED ON LENGTH.          07200000
072100         EVALUATE TRUE                                            07210000
072200             WHEN (MXWW31-FIELD-LENGTH NOT = 6  AND               07220000
072300                   MXWW31-FIELD-LENGTH NOT = 8  AND               07230000
072400                   MXWW31-FIELD-LENGTH NOT = 10)                  07240000
072500                 MOVE WS-DB2DATE-ALL-ZEROES TO                    07250000
072600                      MXWW31-RETURNED-DATE                        07260000
072700             WHEN MXWW31-RETURNED-DATE = SPACES                   07270000
072800                 DISPLAY '*********************************'      07280000
072900                 DISPLAY '* BLANK DATE RETURNED BY MXWP31 *'      07290000
073000                 DISPLAY '* THIS DATE WILL BE ZERO FILLED *'      07300000
073100                 DISPLAY '*********************************'      07310000
073200                 DISPLAY '* DATE PASSED:   '                      07320000
073300                               MXAW21-0103-APPLIED-DATE           07330000
073400                 DISPLAY '* DATE RETURNED: ' MXWW31-RETURNED-DATE 07340000
073500                 DISPLAY '*********************************'      07350000
073600                 EVALUATE TRUE                                    07360000
073700                     WHEN MXWW31-FIELD-LENGTH = 6                 07370000
073800                         MOVE WS-DB2DATE-ZEROES-6  TO             07380000
073900                              MXWW31-RETURNED-DATE                07390000
074000                     WHEN MXWW31-FIELD-LENGTH = 8                 07400000
074100                         MOVE WS-DB2DATE-ZEROES-8  TO             07410000
074200                              MXWW31-RETURNED-DATE                07420000
074300                     WHEN MXWW31-FIELD-LENGTH = 10                07430000
074400                         MOVE WS-DB2DATE-ZEROES-10 TO             07440000
074500                              MXWW31-RETURNED-DATE                07450000
074600                 END-EVALUATE                                     07460000
074700         END-EVALUATE                                             07470000
074800         MOVE MXWW31-RETURNED-DATE      TO WS-H06-APPLIED-DATE    07480000
074900* RIS 00.636 END                                                  07490000
075000         WRITE XEROX-REC-H06                                      07500000
075100             FROM WS-XEROX-H06-LINE-2                             07510000
075200                                                                  07520000
075200**** P0516718 S                                                   07520100
075200**** 'ORIGINAL CREDIT MEMO NO:'                                   07520200
075200         IF MXAW21-0103-ORG-CREDIT-MEMO NOT = SPACES              07520210
075200                                                                  07520220
075200            INITIALIZE WS-H06-LITERAL-31                          07520230
075200                       WS-H06-ORG-CREDIT-MEMO                     07520240
075200                                                                  07520250
075200            MOVE 45                 TO TABLE-ENTRY-WANTED         07520260
075200            MOVE WS-CURR-LANG-IND   TO MXCW021-LANG-CODE          07520270
075200            PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT      07520280
075200            MOVE MXCW021-LANG-TEXT  TO WS-H06-LITERAL-31          07520290
075200            MOVE MXAW21-0103-ORG-CREDIT-MEMO                      07520291
075200                                    TO WS-H06-ORG-CREDIT-MEMO     07520292
075200            WRITE XEROX-REC-H06                                   07520293
075200                  FROM WS-XEROX-H06-LINE-3                        07520294
075200         END-IF                                                   07520295
075200                                                                  07520400
075200**** P0516718 E                                                   07520500
075300* CREDIT MEMO APPLIED TO AN INVOICE                               07530000
075400       WHEN '02' ALSO ANY                                         07540000
075500         ADD MXAW21-0200-APPLIED-AMT TO TOT-CREDIT-MEMO           07550100
075600                                         TOT-CREDIT-DLR           07560000
075700                                                                  07570000
075800         MOVE MXAW21-0200-INVOICE-NBR TO WS-D07-INVOICE-NBR       07580000
075900         IF MXAW21-0200-LINE-NBR > +0                             07590000
076000            MOVE MXAW21-0200-LINE-NBR TO WS-D07-LINE-NBR          07600000
076100         ELSE                                                     07610000
076200            MOVE SPACES               TO WS-D07-LINE-NBR-X        07620000
076300         END-IF                                                   07630000
076400         MOVE MXAW21-0200-MODEL-NBR TO WS-D07-MODEL-NBR           07640000
076500         MOVE MXAW21-0200-SERIAL-NBR TO WS-D07-SERIAL-NBR         07650000
076600         MOVE MXAW21-0200-APPLIED-AMT TO WS-D07-APPLIED-AMT       07660000
076700         MOVE SPACES                  TO WS-D07-CHARGE-TYPE       07670000
076800         WRITE XEROX-REC-D07                                      07680000
076900             FROM WS-XEROX-D07-LINE                               07690000
075500**** P0516718 S                                                   07690400
075200**** 'ORIGINAL CUSTOMER INVOICE NO:'                              07690500
075500         IF MXAW21-0200-ORG-INV-NO NOT = SPACES                   07690610
075500            INITIALIZE WS-D09-LITERAL                             07690700
075500                       WS-D09-ORGINV-NO                           07690800
075500            MOVE 46                 TO TABLE-ENTRY-WANTED         07690900
075500            MOVE WS-CURR-LANG-IND   TO MXCW021-LANG-CODE          07691000
075500            PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT      07691100
075500            MOVE MXCW021-LANG-TEXT  TO WS-D09-LITERAL             07691200
075500            MOVE MXAW21-0200-ORG-INV-NO                           07691300
075500                                    TO WS-D09-ORGINV-NO           07691400
075500            WRITE XEROX-REC-D09                                   07691500
075500                FROM WS-XEROX-D09-LINE                            07691600
075500         END-IF                                                   07691700
075500**** P0516718 E                                                   07691900
077000                                                                  07700000
077100* CREDIT MEMO APPLIED TO A CHARGE                                 07710000
077200       WHEN '03' ALSO ANY                                         07720000
077300         MOVE 'Y'                            TO TYPE-3-SW         07730000
077400         ADD MXAW21-0300-APPLIED-AMT    TO TOT-CREDIT-MEMO        07740000
077500                                           TOT-CREDIT-DLR         07750000
077600         MOVE MXAW21-0300-APPLIED-AMT   TO ACCUM-CHARGES          07760000
077700                                                                  07770000
077800         INITIALIZE WS-D07-INVOICE-NBR                            07780000
077900                    WS-D07-LINE-NBR                               07790000
078000                    WS-D07-MODEL-NBR                              07800000
078100                    WS-D07-SERIAL-NBR                             07810000
078200* RIS 00.636 START                                                07820000
078300         MOVE 8                         TO MXWW31-FIELD-LENGTH    07830000
078400         MOVE MXAW21-0300-BILL-DATE     TO MXWW31-DB2-DATE        07840000
078500                                           WS-HOLD-BILL-DATE      07850000
078600         MOVE WS-DLR-CNTL-ENT           TO                        07860000
078700                                           MXWW31-CNTL-ENT-NO     07870000
078800         PERFORM MXWP31-ENVIRONMENT-DATE   THRU MXWP31-EXIT       07880000
078900**IF RETURNED DATE IS SPACES, ZERO FILL BASED ON LENGTH.          07890000
079000         EVALUATE TRUE                                            07900000
079100             WHEN (MXWW31-FIELD-LENGTH NOT = 6  AND               07910000
079200                   MXWW31-FIELD-LENGTH NOT = 8  AND               07920000
079300                   MXWW31-FIELD-LENGTH NOT = 10)                  07930000
079400                 MOVE WS-DB2DATE-ALL-ZEROES TO                    07940000
079500                      MXWW31-RETURNED-DATE                        07950000
079600             WHEN MXWW31-RETURNED-DATE = SPACES                   07960000
079700                 DISPLAY '*********************************'      07970000
079800                 DISPLAY '* BLANK DATE RETURNED BY MXWP31 *'      07980000
079900                 DISPLAY '* THIS DATE WILL BE ZERO FILLED *'      07990000
080000                 DISPLAY '*********************************'      08000000
080100                 DISPLAY '* DATE PASSED:   ' MXAW21-0300-BILL-DATE08010000
080200                 DISPLAY '* DATE RETURNED: ' MXWW31-RETURNED-DATE 08020000
080300                 DISPLAY '*********************************'      08030000
080400                 EVALUATE TRUE                                    08040000
080500                     WHEN MXWW31-FIELD-LENGTH = 6                 08050000
080600                         MOVE WS-DB2DATE-ZEROES-6  TO             08060000
080700                              MXWW31-RETURNED-DATE                08070000
080800                     WHEN MXWW31-FIELD-LENGTH = 8                 08080000
080900                         MOVE WS-DB2DATE-ZEROES-8  TO             08090000
081000                              MXWW31-RETURNED-DATE                08100000
081100                     WHEN MXWW31-FIELD-LENGTH = 10                08110000
081200                         MOVE WS-DB2DATE-ZEROES-10 TO             08120000
081300                              MXWW31-RETURNED-DATE                08130000
081400                 END-EVALUATE                                     08140000
081500         END-EVALUATE                                             08150000
081600         MOVE MXWW31-RETURNED-DATE      TO WS-D07-BILL-DATE       08160000
081700* RIS 00.636 END                                                  08170000
081800         MOVE MXAW21-0300-CHARGE-TYPE   TO WS-D07-CHARGE-TYPE     08180000
081900                                                                  08190000
082000         PERFORM 3001-ACCUMULATE-CHARGES THRU 3001-EXIT           08200000
082100                                                                  08210000
082200         MOVE ACCUM-CHARGES             TO WS-D07-APPLIED-AMT     08220000
082300                                                                  08230000
082400         WRITE XEROX-REC-D07                                      08240000
082500             FROM WS-XEROX-D07-LINE                               08250000
082600                                                                  08260000
082700* CREDIT MEMO APPLIED TO UNIDENTIFIED CASH                        08270000
082800       WHEN '04' ALSO 01                                          08280000
082900         ADD MXAW21-0400-APPLIED-AMT TO TOT-CREDIT-MEMO           08290000
083000                                        TOT-CREDIT-DLR            08300000
083100**       ** 'PENDING APPLICATION - ON ACCOUNT'                    08310000
083200         MOVE 32                        TO TABLE-ENTRY-WANTED     08320000
083300         MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE      08330000
083400         PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT         08340000
083500         MOVE MXCW021-LANG-TEXT         TO WS-D08-CM-TYPE         08350000
083600         MOVE MXAW21-0400-APPLIED-AMT   TO WS-D08-APPLIED-AMT     08360000
083700         WRITE XEROX-REC-D08                                      08370000
083800             FROM WS-XEROX-D08-LINE                               08380000
083900                                                                  08390000
084000* CREDIT MEMO APPLIED TO PAYABLE TRANSFER                         08400000
084100       WHEN '04' ALSO 02                                          08410000
084200         ADD MXAW21-0400-APPLIED-AMT TO TOT-CREDIT-MEMO           08420000
084300                                        TOT-CREDIT-DLR            08430000
084400**       ** 'CHECK TO BE ISSUED'                                  08440000
084500         MOVE 33                        TO TABLE-ENTRY-WANTED     08450000
084600         MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE      08460000
084700         PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT         08470000
084800         MOVE MXCW021-LANG-TEXT         TO WS-D08-CM-TYPE         08480000
084900         MOVE MXAW21-0400-APPLIED-AMT   TO WS-D08-APPLIED-AMT     08490000
085000         WRITE XEROX-REC-D08                                      08500000
085100             FROM WS-XEROX-D08-LINE                               08510000
085200                                                                  08520000
085300* CREDIT MEMO APPLIED TO MISC                                     08530000
085400       WHEN '04' ALSO 03                                          08540000
085500         ADD MXAW21-0400-APPLIED-AMT TO TOT-CREDIT-MEMO           08550000
085600                                        TOT-CREDIT-DLR            08560000
085700**       ** 'MISCELLANEOUS'                                       08570000
085800         MOVE 34                        TO TABLE-ENTRY-WANTED     08580000
085900         MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE      08590000
086000         PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT         08600000
086100         MOVE MXCW021-LANG-TEXT         TO WS-D08-CM-TYPE         08610000
086200         MOVE MXAW21-0400-APPLIED-AMT   TO WS-D08-APPLIED-AMT     08620000
086300         WRITE XEROX-REC-D08                                      08630000
086400             FROM WS-XEROX-D08-LINE                               08640000
086500                                                                  08650000
086600* CREDIT MEMO APPLIED TO FINANCE CHARGE                           08660000
086700       WHEN '04' ALSO 04                                          08670000
086800         ADD MXAW21-0400-APPLIED-AMT TO TOT-CREDIT-MEMO           08680000
086900                                        TOT-CREDIT-DLR            08690000
087000**       ** 'FINANCE CHARGE'                                      08700000
087100         MOVE 35                        TO TABLE-ENTRY-WANTED     08710000
087200         MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE      08720000
087300         PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT         08730000
087400         MOVE MXCW021-LANG-TEXT         TO WS-D08-CM-TYPE         08740000
087500         MOVE MXAW21-0400-APPLIED-AMT   TO WS-D08-APPLIED-AMT     08750000
087600         WRITE XEROX-REC-D08                                      08760000
087700             FROM WS-XEROX-D08-LINE                               08770000
087800     END-EVALUATE.                                                08780000
087900                                                                  08790000
088000     IF TYPE-3-SW = 'Y'                                           08800000
088100        MOVE 'N'             TO TYPE-3-SW                         08810000
088200        GO TO 1000-EXIT                                           08820000
088300     ELSE                                                         08830000
088400        PERFORM 8000-READ-INPUT THRU 8000-EXIT.                   08840000
088500                                                                  08850000
088600 1000-EXIT. EXIT.                                                 08860000
088700/***************************************************************  08870000
088800*  GET XEROX FORM NUMBER FOR DEALER.  WRITE OUT FIRST          *  08880000
088900*  RECORD AND DEALER DATA RECORDS.                             *  08890000
089000****************************************************************  08900000
089100 1100-SETUP-DEALER.                                               08910000
089200                                                                  08920000
089300     SET FIRST-CREDIT-MEMO          TO TRUE.                      08930000
089400     SET FIRST-DIST-OF-PAGE         TO TRUE.                      08940000
089500     MOVE MXAW21-0001-DLR-NBR       TO WS-CURRENT-DLR.            08950000
089600     MOVE MXAW21-SK-LANG-IND        TO WS-CURR-LANG-IND.          08960000
089700* RIS 00.636 START                                                08970000
089800     MOVE MXAW21-0001-DLR-CNTL-ENT  TO WS-DLR-CNTL-ENT.           08980000
089900* RIS 00.636 END                                                  08990000
090000                                                                  09000000
090100     PERFORM 1110-GET-COMPANY          THRU 1110-EXIT.            09010000
090200     PERFORM 1120-GET-XEROX-FORM-DATA  THRU 1120-EXIT.            09020000
090300     PERFORM 1130-WRITE-TRIPLE-ZERO    THRU 1130-EXIT.            09030000
090400                                                                  09040000
090500**   ** 'APPLIED CREDIT DETAIL'                                   09050000
090600     MOVE 1                         TO TABLE-ENTRY-WANTED.        09060000
090700     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         09070000
090800     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            09080000
090900     MOVE MXCW021-LANG-TEXT         TO WS-T01-TITLE-LIT.          09090000
091000     WRITE XEROX-REC-T01                                          09100000
091100         FROM WS-XEROX-T01-LINE.                                  09110000
091200**   ** 'PAGE'                                                    09120000
091300     MOVE 2                         TO TABLE-ENTRY-WANTED.        09130000
091400     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         09140000
091500     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            09150000
091600     MOVE MXCW021-LANG-TEXT         TO WS-H01-LITERAL.            09160000
091700     WRITE XEROX-REC-H01                                          09170000
091800         FROM WS-XEROX-H01-LINE.                                  09180000
091900                                                                  09190000
092000**   ** 'CUSTOMER NO:'                                            09200000
092100     MOVE 4                         TO TABLE-ENTRY-WANTED.        09210000
092200     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         09220000
092300     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            09230000
092400     MOVE MXCW021-LANG-TEXT         TO WS-H03-LITERAL.            09240000
092500     WRITE XEROX-REC-H03                                          09250000
092600         FROM WS-XEROX-H03-LINE.                                  09260000
092700                                                                  09270000
092800**** LEFT JUSTIFY CUSTOMER NUMBER                                 09280000
092900     MOVE MXAW21-0001-DLR-NBR       TO WS-NUM-STAR.               09290000
093000     MOVE +0                        TO WS-STAR-CNT.               09300000
093100     INSPECT WS-NUM-STAR TALLYING WS-STAR-CNT FOR ALL '*'.        09310000
093200     MOVE WS-NUM-STAR (WS-STAR-CNT + 1 :                          09320000
093300                       LENGTH OF WS-NUM-STAR - WS-STAR-CNT)       09330000
093400                                    TO WS-CUST-NO-LJ.             09340000
093500                                                                  09350000
093600     MOVE WS-CUST-NO-LJ             TO WS-D03-CUST                09360000
093700                                       WS-T02-CUST.               09370000
093800     WRITE XEROX-REC-D03                                          09380000
093900         FROM WS-XEROX-D03-LINE-1.                                09390000
094000                                                                  09400000
094100 1100-EXIT. EXIT.                                                 09410000
094200/***************************************************************  09420000
094300*  GET THE CONTROL ENTITY'S COMPANY                            *  09430000
094400****************************************************************  09440000
094500 1110-GET-COMPANY.                                                09450000
094600                                                                  09460000
094700     MOVE MXAW21-0001-DLR-CNTL-ENT  TO CNTL-ENT-NO OF DCLVWMJ096. 09470000
094800                                                                  09480000
094900     EXEC SQL                                                     09490000
095000         SELECT COMPANY_NO                                        09500000
095100         INTO   :DCLVWMJ096.COMPANY-NO                            09510000
095200         FROM VWMJ096                                             09520000
095300         WHERE CNTL_ENT_NO      = :DCLVWMJ096.CNTL-ENT-NO         09530000
095400     END-EXEC.                                                    09540000
095500                                                                  09550000
095600     IF SQLCODE NOT = +0                                          09560000
095700        SET  ABT-ERROR-IS-DB2     TO TRUE                         09570000
095800        SET  ABT-DO-ABEND         TO TRUE                         09580000
095900        MOVE SQLCODE              TO ABT-ERROR-ABEND-CODE         09590000
096000        MOVE 'SELECT  '           TO ABT-DA-FUNCTION              09600000
096100        MOVE 'VWMJ096 '           TO ABT-DA-ACCESS-NAME           09610000
096200        MOVE '1110-   '           TO ABT-ERROR-SECTION            09620000
096300        PERFORM Z-980-ABNORMAL-TERM                               09630000
096400     END-IF.                                                      09640000
096500                                                                  09650000
096600 1110-EXIT. EXIT.                                                 09660000
096700/**************************************************************** 09670000
096800*  THIS ROUTINE WILL BUILD THE RECORD.                          * 09680000
096900***************************************************************** 09690000
097000 1120-GET-XEROX-FORM-DATA.                                        09700000
097100                                                                  09710000
097200     INITIALIZE MXWW26-WORK-AREA.                                 09720000
097300                                                                  09730000
097400     MOVE MXAW21-0001-DLR-CNTL-ENT TO MXWW26-CNTL-ENT-NO.         09740000
097500     MOVE WS-PROGRAM-ID            TO MXWW26-RPT-PGM-ID.          09750000
097600     MOVE WS-CURR-LANG-IND         TO MXWW26-LANG-CODE.           09760000
097700     MOVE MXAW21-SK-COUNTRY-CODE   TO MXWW26-CNTRY-CODE.          09770000
097800     MOVE MXAW21-0001-DLR-NBR      TO MXWW26-CUST-NO.             09780000
097900                                                                  09790000
098000     CALL WS-FORM-DETERMINE-PGM USING MXWW26-WORK-AREA.           09800000
098100                                                                  09810000
098200 1120-EXIT. EXIT.                                                 09820000
098300/***************************************************************  09830000
098400*  WRITE OUT THE TRIPLE ZERO RECORD                            *  09840000
098500****************************************************************  09850000
098600 1130-WRITE-TRIPLE-ZERO.                                          09860000
098700                                                                  09870000
098800     INITIALIZE TRIPLE-ZERO-RECORD.                               09880000
098900                                                                  09890000
099000     MOVE ZEROS                     TO CC-REC-TYPE.               09900000
099100     MOVE 'APPLCRED'                TO FORM-NAME                  09910000
099200                                    OF TRIPLE-ZERO-RECORD.        09920000
099300     MOVE MXAW21-0001-DLR-NBR       TO DLR-NO-000                 09930000
099400                                       CUST-NO-000                09940000
099500                                       WS-UNIQUEKEY-CUST.         09950000
099600     MOVE COMPANY-NO OF DCLVWMJ096  TO COMPANY-NO-000.            09960000
099700     MOVE MXWW26-XEROX-FORM-ID      TO XEROX-FORM-ID.             09970000
099800     MOVE MXWW26-XEROX-LOGO-ID      TO XEROX-LOGO-ID.             09980000
099900     MOVE MXWW26-FORM-COPY-CNT      TO XEROX-FORM-COPY-CNT.       09990000
100000     MOVE MXWW26-SPECIAL-HANDLING   TO SPECIAL-HANDLING.          10000000
100100     MOVE WS-UNIQUE-KEY-BUILD       TO UNIQUE-KEY.                10010000
100200     MOVE WS-CURR-LANG-IND          TO LANGUAGE-CODE-000.         10020000
100300     MOVE MXAW21-SK-COUNTRY-CODE    TO COUNTRY-CODE-000.          10030000
100400     MOVE PROC-DATE OF DCLVWMCTUPD  (1:4)                         10040000
100500                                    TO DATE-000 (1:4).            10050000
100600     MOVE PROC-DATE OF DCLVWMCTUPD  (6:2)                         10060000
100700                                    TO DATE-000 (5:2).            10070000
100800     MOVE PROC-DATE OF DCLVWMCTUPD  (9:2)                         10080000
100900                                    TO DATE-000 (7:2).            10090000
101000     MOVE SPACES                    TO TRUST-NO-000.              10100000
101100     MOVE ZEROS                     TO DIST-NO-000                10110000
101200                                       MFG-NO-000                 10120000
101300                                       DIST-LOC-NO-000            10130000
101400                                       MFG-LOC-NO-000             10140000
101500                                       CUST-LOC-NO-000            10150000
101600                                       DLR-LOC-NO-000.            10160000
101700                                                                  10170000
101800     MOVE TRIPLE-ZERO-RECORD        TO WS-XEROX-000-LINE.         10180000
101900                                                                  10190000
102000**   ** WRITE THE HEADER (000) RECORD                             10200000
102100     WRITE XEROX-REC-000                                          10210000
102200         FROM WS-XEROX-000-LINE.                                  10220000
102300                                                                  10230000
102400     PERFORM 1132-CONTROL-REPORT       THRU 1132-EXIT.            10240000
102500                                                                  10250000
102600 1130-EXIT. EXIT.                                                 10260000
102700/***************************************************************  10270000
102800*    CREATE XEROX CONTROL REPORT                               *  10280000
102900****************************************************************  10290000
103000 1132-CONTROL-REPORT.                                             10300000
103100                                                                  10310000
103200     CALL WS-CNTL-REPORT-PGM      USING TRIPLE-ZERO-RECORD,       10320000
103300                                        MXBW510-LINK.             10330000
103400                                                                  10340000
103500     IF  MXBW510-SUCCESSFUL                                       10350000
103600         CONTINUE                                                 10360000
103700     ELSE                                                         10370000
103800        DISPLAY '1132- PROBLEM WITH CONTROL REPORT MODULE'        10380000
103900        SET ABT-DO-ABEND     TO TRUE                              10390000
104000        SET ABT-ERROR-IS-DB2 TO TRUE                              10400000
104100        MOVE 'SUBRTN  '      TO ABT-DA-FUNCTION                   10410000
104200        MOVE '1132-    '     TO ABT-ERROR-SECTION                 10420000
104300        MOVE 3605            TO ABT-ERROR-ABEND-CODE              10430000
104400        MOVE 'CNTLREPT'      TO ABT-DA-ACCESS-NAME                10440000
104500        PERFORM Z-980-ABNORMAL-TERM THRU                          10450000
104600                Z-980-ABNORMAL-TERM-RETURN                        10460000
104700     END-IF.                                                      10470000
104800                                                                  10480000
104900     MOVE 'N'                     TO MXBW510-RUN-FLAG.            10490000
105000                                                                  10500000
105100 1132-EXIT. EXIT.                                                 10510000
105200/***************************************************************  10520000
105300*  WRITE OUT THE ADDITIONAL CUSTOMER LITERAL AND NUMBER.       *  10530000
105400****************************************************************  10540000
105500 1200-WRITE-ADDL-CUST.                                            10550000
105600                                                                  10560000
105700**   ** 'ADDL CUST NO:'                                           10570000
105800     MOVE 5                         TO TABLE-ENTRY-WANTED.        10580000
105900     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         10590000
106000     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            10600000
106100     MOVE MXCW021-LANG-TEXT         TO WS-H03-LITERAL.            10610000
106200     WRITE XEROX-REC-H03                                          10620000
106300         FROM WS-XEROX-H03-LINE.                                  10630000
106400     MOVE MXAW21-0002-ADDL-CUST-NO  TO WS-D03-ADDL-CUST-NO.       10640000
106500     WRITE XEROX-REC-D03                                          10650000
106600         FROM WS-XEROX-D03-LINE-2.                                10660000
106700                                                                  10670000
106800 1200-EXIT. EXIT.                                                 10680000
106900/***************************************************************  10690000
107000*  WRITE OUT THE DEALER REP NAME AND PHONE NUMBER.             *  10700000
107100****************************************************************  10710000
107200 1300-WRITE-REP.                                                  10720000
107300                                                                  10730000
107400**   ** 'STATEMENT INQUIRIES'                                     10740000
107500     MOVE 6                         TO TABLE-ENTRY-WANTED.        10750000
107600     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         10760000
107700     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            10770000
107800     MOVE MXCW021-LANG-TEXT         TO WS-H04-LITERAL.            10780000
107900     WRITE XEROX-REC-H04                                          10790000
108000         FROM WS-XEROX-H04-LINE.                                  10800000
108100                                                                  10810000
      * FGTN-195 - S
      ***********************************************************
      * CHECK IF THE CUSTOMER IS SHAW CUSTOMER                  *
      ***********************************************************
           SET SHAW-CUST-NO TO TRUE
           INITIALIZE WS-COMPANY-NO
           PERFORM 1310-CHECK-SHAW-CUST   THRU
                   1310-EXIT
           PERFORM 1320-GET-COMPANY       THRU
                   1320-EXIT
      * FGTN-195 - E
108200**   ** 'PLEASE CALL'                                             10820000
108300     MOVE 7                         TO TABLE-ENTRY-WANTED.        10830000
108400     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         10840000
      * FGTN-195 - S
           MOVE WS-COMPANY-NO             TO MXCW021-ENTITY-NO.
      * FGTN-195 - E
108500     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            10850000
108600     MOVE MXCW021-LANG-TEXT         TO WS-H04-LITERAL.            10860000
108700     WRITE XEROX-REC-H04                                          10870000
108800         FROM WS-XEROX-H04-LINE.                                  10880000
108900                                                                  10890000
      * FGTN-195 - S
           IF SHAW-CUST-YES
      * POPULATE REP NAME
              SET REP-NAME-NOTFOUND           TO TRUE
              PERFORM 1340-SELECT-VWMRP00     THRU 1340-EXIT

              IF REP-NAME-FOUND
                 MOVE DLR-REP-NAME            OF DCLVWMRP00
                                              TO WS-D04-DLR-REP-NAME
              ELSE
                 MOVE SPACES                  TO WS-D04-DLR-REP-NAME
              END-IF

      * POPULATE EMAIL ADDRESS
              MOVE 1            TO PARM-ID    OF DCLVWMCTRPA
              MOVE 0            TO INTEGER-NO OF DCLVWMCTRPA

              SET REP-EMAIL-NOTFOUND          TO TRUE
              PERFORM 1350-GET-CTRPA-INFO     THRU 1350-EXIT

              IF REP-EMAIL-FOUND
                 COMPUTE WS-EMAIL-LEN  = CHAR-DATA-LEN - 3
                 MOVE CHAR-DATA-TEXT(4:WS-EMAIL-LEN)
                                              TO WS-D04-DLR-REP-PHONE
              ELSE
                 MOVE SPACE                   TO WS-D04-DLR-REP-PHONE
              END-IF
           ELSE
      * FGTN-195 - E
109000        MOVE MXAW21-0003-DLR-REP-NAME   TO WS-D04-DLR-REP-NAME    10900000
109100        MOVE MXAW21-0003-DLR-REP-PHONE  TO WS-D04-DLR-REP-PHONE   10910000
      * FGTN-195 - S
           END-IF
      * FGTN-195 - E
109200                                                                  10920000
109300     WRITE XEROX-REC-D04                                          10930000
109400         FROM WS-XEROX-D04-LINE-1.                                10940000
109500     WRITE XEROX-REC-D04                                          10950000
109600         FROM WS-XEROX-D04-LINE-2.                                10960000
109700     WRITE XEROX-REC-D04                                          10970000
109800         FROM WS-XEROX-D04-LINE-3.                                10980000
109900                                                                  10990000
110000 1300-EXIT. EXIT.                                                 11000000
      *FGTN-195 - S
      /*****************************************************************
      * THIS PROCESS CALLS THE ENVIRONMENTAL RULE ROUTINE TO DETERMINE
      * THE SHAW CUSTOMER
      ******************************************************************
       1310-CHECK-SHAW-CUST.

           INITIALIZE MXWW30-INPUT-OUTPUT-VARS.
           MOVE 'TRUE'                       TO MXWW30-FUNCTION.
           MOVE 'COMMERCIAL AUTO GROUP'      TO MXWW30-BUS-RULE-NAME.

           PERFORM 1330-GET-CNTL-INFO        THRU 1330-EXIT.

           MOVE CNTL-ENT-NO OF DCLVWMCU00    TO MXWW30-CNTL-ENT-NO.

           CALL WS-ENVIRONMENT-RTN     USING MXWW30-INPUT-OUTPUT-VARS.

           IF  MXWW30-STATUS-ROW-FND
               SET  SHAW-CUST-YES TO TRUE
           ELSE
           IF  MXWW30-STATUS-ROW-NFD
               CONTINUE
           ELSE
               MOVE MXWW30-SQLCODE           TO SQLCODE
               DISPLAY '**********************************************'
               DISPLAY ' P R O G R A M   M X B P B 1 7 1   E R R O R  '
               DISPLAY ' '
               DISPLAY ' CALL ENVIRONMENTAL RULE, SQLCODE: ' SQLCODE
               DISPLAY ' VERIFY SHAW CUSTOMER             '
               DISPLAY '**********************************************'
               DISPLAY ' '
               DISPLAY 'MXWW30-STATUS-CODE    = ' MXWW30-STATUS-CODE
               DISPLAY 'FUNCTION              = ' MXWW30-FUNCTION
               DISPLAY 'MXWW30-BUS-RUL-NAME   = ' MXWW30-BUS-RULE-NAME
               DISPLAY 'MXWW30-CNTL-END-NO    = ' MXWW30-CNTL-ENT-NO
               DISPLAY 'MXWW30-PARA-ID        = '  MXWW30-PARA-ID
               SET ABT-ERROR-IS-DB2        TO TRUE
               SET ABT-DO-ABEND            TO TRUE
               MOVE MXWW30-STATUS-CODE     TO ABT-ERROR-ABEND-CODE
               MOVE MXWW30-FUNCTION        TO ABT-DA-FUNCTION
               MOVE MXWW30-TBL-NAME        TO ABT-DA-ACCESS-NAME
               MOVE '1310-'                TO ABT-ERROR-SECTION
               MOVE +72                    TO BATCH-ERROR-CMNT-L
               MOVE 'LOOK FOR RULE COMMERCIAL AUTO GROUP '
                                           TO BATCH-ERROR-CMNT
               PERFORM Z-980-ABNORMAL-TERM
           END-IF
           END-IF.

       1310-EXIT. EXIT.
      /*****************************************************************
      * THIS PROCESS CALLS THE ENVIRONMENTAL RULE ROUTINE TO DETERMINE
      * THE SHAW CUSTOMER
      ******************************************************************
       1320-GET-COMPANY.

           MOVE CNTL-ENT-NO OF DCLVWMCU00
            TO  CNTL-ENT-NO OF DCLVWMCN00

           EXEC SQL
                SELECT    COMPANY_NO
                  INTO   :WS-COMPANY-NO
                  FROM   VWMCN00
                  WHERE  CNTL_ENT_NO   = :DCLVWMCN00.CNTL-ENT-NO
             WITH UR
           END-EXEC.

           EVALUATE SQLCODE
             WHEN +0
               CONTINUE
             WHEN OTHER
               SET  ABT-ERROR-IS-DB2        TO TRUE
               SET  ABT-DO-ABEND            TO TRUE
               MOVE SQLCODE                 TO ABT-ERROR-ABEND-CODE
               MOVE '1320    '              TO ABT-ERROR-SECTION
               MOVE 'SELECT  '              TO ABT-DA-FUNCTION
               MOVE 'VWMCN00 '              TO ABT-DA-ACCESS-NAME
               DISPLAY 'CNTL ENT      = ' CNTL-ENT-NO OF DCLVWMCN00
               PERFORM Z-980-ABNORMAL-TERM
           END-EVALUATE.

       1320-EXIT. EXIT.
      /*****************************************************************
      * THIS PROCESS CALLS THE ENVIRONMENTAL RULE ROUTINE TO DETERMINE
      * THE SHAW CUSTOMER
      ******************************************************************
       1330-GET-CNTL-INFO.

           MOVE MXAW21-SK-DLR-NBR       TO CUST-NO OF DCLVWMCU00.

           EXEC SQL
                SELECT    CNTL_ENT_NO
                         ,VALUE(DLR_REP_CODE,' ')
                  INTO   :DCLVWMCU00.CNTL-ENT-NO
                        ,:DCLVWMCU00.DLR-REP-CODE
                  FROM   VWMCU00
                  WHERE  CUST_NO       = :DCLVWMCU00.CUST-NO
             WITH UR
           END-EXEC.

           IF SQLCODE NOT = +0
              SET ABT-ERROR-IS-DB2       TO TRUE
              SET ABT-DO-ABEND           TO TRUE
              MOVE SQLCODE               TO ABT-ERROR-ABEND-CODE
              MOVE 'SELECT  '            TO ABT-DA-FUNCTION
              MOVE 'VWMCU00 '            TO ABT-DA-ACCESS-NAME
              MOVE '1330-   '            TO ABT-ERROR-SECTION
              PERFORM Z-980-ABNORMAL-TERM
           END-IF.

       1330-EXIT. EXIT.
      /*****************************************************************
      * RETREIVE THE DEALER REP INFORMATION FROM THE VWMRP00 TABLE     *
      ******************************************************************
       1340-SELECT-VWMRP00.

           MOVE DLR-REP-CODE             OF DCLVWMCU00
             TO DLR-REP-CODE             OF DCLVWMRP00
           EXEC SQL
                SELECT   DLR_REP_NAME
                  INTO   :DCLVWMRP00.DLR-REP-NAME
                  FROM  VWMRP00
                  WHERE DLR_REP_CODE = :DCLVWMRP00.DLR-REP-CODE
                   WITH UR
           END-EXEC.

           EVALUATE SQLCODE
               WHEN +0
                    SET REP-NAME-FOUND TO TRUE
               WHEN +100
                    DISPLAY '**************************'
                    DISPLAY 'REP NAME NOT FOUND IN RP00'
                    DISPLAY 'CUST-NO :' CUST-NO OF DCLVWMCU00
                    DISPLAY 'REP-CODE:' DLR-REP-CODE OF DCLVWMRP00
                    DISPLAY '***************************'
               WHEN OTHER
                    SET ABT-ERROR-IS-DB2       TO TRUE
                    SET ABT-DO-ABEND           TO TRUE
                    MOVE SQLCODE               TO ABT-ERROR-ABEND-CODE
                    MOVE 'SELECT  '            TO ABT-DA-FUNCTION
                    MOVE 'VWMRP00 '            TO ABT-DA-ACCESS-NAME
                    MOVE '1340-   '            TO ABT-ERROR-SECTION
                    PERFORM Z-980-ABNORMAL-TERM
           END-EVALUATE.

       1340-EXIT. EXIT.
      ******************************************************************
      *  RETRIVE EMAIL ID FROM CTRPA TABLE.                            *
      ******************************************************************
       1350-GET-CTRPA-INFO.

           EXEC SQL
             SELECT CHAR_DATA
              INTO :DCLVWMCTRPA.CHAR-DATA
               FROM VWMCTRPA
              WHERE PGM_NAME = 'CNTEMAIL'
                AND PARM_ID  = :DCLVWMCTRPA.PARM-ID
                AND INTEGER_NO = :DCLVWMCTRPA.INTEGER-NO
                AND SUBSTR(CHAR_DATA,1,3)=
                                         :DCLVWMRP00.DLR-REP-CODE
           END-EXEC

           EVALUATE SQLCODE
               WHEN +0
                    SET REP-EMAIL-FOUND TO TRUE
               WHEN +100
                    DISPLAY '****************************'
                    DISPLAY 'REP EMAIL NOT FOUND IN CTRPA'
                    DISPLAY 'CUST-NO :' CUST-NO OF DCLVWMCU00
                    DISPLAY 'REP-CODE:' DLR-REP-CODE OF DCLVWMRP00
                    DISPLAY '***************************'
               WHEN OTHER
                    SET ABT-ERROR-IS-DB2       TO TRUE
                    SET ABT-DO-ABEND           TO TRUE
                    MOVE SQLCODE               TO ABT-ERROR-ABEND-CODE
                    MOVE 'SELECT  '            TO ABT-DA-FUNCTION
                    MOVE 'VWMCTRPA'            TO ABT-DA-ACCESS-NAME
                    MOVE '1350-   '            TO ABT-ERROR-SECTION
                    PERFORM Z-980-ABNORMAL-TERM
           END-EVALUATE.
       1350-EXIT.
           EXIT.
      *FGTN-195 - E
110100/***************************************************************  11010000
110200*  WRITE OUT THE DETAIL COLUMN HEADINGS                        *  11020000
110300****************************************************************  11030000
110400 1400-DETAIL-HEADINGS.                                            11040000
110500                                                                  11050000
110600* DETAIL COLUMN HEADINGS (LINE 1)                                 11060000
110700**       ** 'INVOICE'                                             11070000
110800     MOVE 13                        TO TABLE-ENTRY-WANTED.        11080000
110900     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11090000
111000     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11100000
111100     MOVE MXCW021-LANG-TEXT         TO WS-H07-LITERAL-1.          11110000
111200**       ** 'LINE'                                                11120000
111300     MOVE 15                        TO TABLE-ENTRY-WANTED.        11130000
111400     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11140000
111500     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11150000
111600     MOVE MXCW021-LANG-TEXT         TO WS-H07-LITERAL-2.          11160000
111700**       ** 'MODEL'                                               11170000
111800     MOVE 17                        TO TABLE-ENTRY-WANTED.        11180000
111900     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11190000
112000     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11200000
112100     MOVE MXCW021-LANG-TEXT         TO WS-H07-LITERAL-3.          11210000
112200**       ** 'SERIAL'                                              11220000
112300     MOVE 19                        TO TABLE-ENTRY-WANTED.        11230000
112400     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11240000
112500     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11250000
112600     MOVE MXCW021-LANG-TEXT         TO WS-H07-LITERAL-4.          11260000
112700**       ** 'APPLIED'                                             11270000
112800     MOVE 21                        TO TABLE-ENTRY-WANTED.        11280000
112900     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11290000
113000     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11300000
113100     MOVE MXCW021-LANG-TEXT         TO WS-H07-LITERAL-5.          11310000
113200**       ** 'CHARGE'                                              11320000
113300     MOVE 23                        TO TABLE-ENTRY-WANTED.        11330000
113400     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11340000
113500     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11350000
113600     MOVE MXCW021-LANG-TEXT         TO WS-H07-LITERAL-6.          11360000
113700                                                                  11370000
113800     WRITE XEROX-REC-H07                                          11380000
113900         FROM WS-XEROX-H07-LINE.                                  11390000
114000                                                                  11400000
114100* DETAIL COLUMN HEADINGS (LINE 2)                                 11410000
114200**       ** 'NUMBER'                                              11420000
114300     MOVE 14                        TO TABLE-ENTRY-WANTED.        11430000
114400     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11440000
114500     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11450000
114600     MOVE MXCW021-LANG-TEXT         TO WS-H07-LITERAL-1.          11460000
114700**       ** 'NO.'                                                 11470000
114800     MOVE 16                        TO TABLE-ENTRY-WANTED.        11480000
114900     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11490000
115000     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11500000
115100     MOVE MXCW021-LANG-TEXT         TO WS-H07-LITERAL-2.          11510000
115200**       ** 'NUMBER'                                              11520000
115300     MOVE 18                        TO TABLE-ENTRY-WANTED.        11530000
115400     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11540000
115500     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11550000
115600     MOVE MXCW021-LANG-TEXT         TO WS-H07-LITERAL-3.          11560000
115700**       ** 'NUMBER'                                              11570000
115800     MOVE 20                        TO TABLE-ENTRY-WANTED.        11580000
115900     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11590000
116000     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11600000
116100     MOVE MXCW021-LANG-TEXT         TO WS-H07-LITERAL-4.          11610000
116200**       ** 'AMOUNT'                                              11620000
116300     MOVE 22                        TO TABLE-ENTRY-WANTED.        11630000
116400     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11640000
116500     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11650000
116600     MOVE MXCW021-LANG-TEXT         TO WS-H07-LITERAL-5.          11660000
116700**       ** 'TYPE'                                                11670000
116800     MOVE 24                        TO TABLE-ENTRY-WANTED.        11680000
116900     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11690000
117000     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11700000
117100     MOVE MXCW021-LANG-TEXT         TO WS-H07-LITERAL-6.          11710000
117200                                                                  11720000
117300     WRITE XEROX-REC-H07                                          11730000
117400         FROM WS-XEROX-H07-LINE.                                  11740000
117500                                                                  11750000
117600 1400-EXIT. EXIT.                                                 11760000
117700/***************************************************************  11770000
117800*  WRITE OUT THE T02 (PAGE 2-N HEADER) RECORD.                 *  11780000
117900****************************************************************  11790000
118000 1500-WRITE-T02-REC.                                              11800000
118100                                                                  11810000
118200     MOVE 38                      TO TABLE-ENTRY-WANTED.          11820000
118300     MOVE WS-CURR-LANG-IND        TO MXCW021-LANG-CODE.           11830000
118400     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11840000
118500     MOVE MXCW021-LANG-TEXT       TO WS-T02-TITLE-LIT-1.          11850000
118600                                                                  11860000
118700     MOVE 39                      TO TABLE-ENTRY-WANTED.          11870000
118800     MOVE WS-CURR-LANG-IND        TO MXCW021-LANG-CODE.           11880000
118900     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11890000
119000     MOVE MXCW021-LANG-TEXT       TO WS-T02-TITLE-LIT-2.          11900000
119100                                                                  11910000
119200     WRITE XEROX-REC-T02                                          11920000
119300         FROM WS-XEROX-T02-LINE.                                  11930000
119400                                                                  11940000
119500 1500-EXIT. EXIT.                                                 11950000
119501*** 03.857 S                                                      11950100
119510/***************************************************************  11951000
119520* 03.857 -       - 12/31/03 - ADDED ROUTINE TO WRITE OUT NEW   *  11952000
119521*                  D18 RECORD WHICH CONTAINS BRIDGE TEXT.      *  11952100
119530****************************************************************  11953000
119540 1600-WRITE-D18.                                                  11954000
119550     MOVE SPACES TO WS-D18-LITERAL1                               11955000
119551                    WS-D18-LITERAL2                               11955100
119552                    WS-D18-LITERAL3                               11955200
119553                    WS-D18-LITERAL4                               11955300
119554                    WS-D18-LITERAL5.                              11955400
119555                                                                  11955500
119556     MOVE 40                        TO TABLE-ENTRY-WANTED.        11955600
119557     MOVE WS-DLR-CNTL-ENT           TO MXCW021-ENTITY-NO.         11955700
119558     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11955800
119559     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11955900
119560     MOVE MXCW021-LANG-TEXT         TO WS-D18-LITERAL1.           11956000
119561                                                                  11956100
119562     MOVE 41                        TO TABLE-ENTRY-WANTED.        11956200
119563     MOVE WS-DLR-CNTL-ENT           TO MXCW021-ENTITY-NO.         11956300
119564     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11956400
119565     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11956500
119566     MOVE MXCW021-LANG-TEXT         TO WS-D18-LITERAL2.           11956600
119567                                                                  11956700
119568     MOVE 42                        TO TABLE-ENTRY-WANTED.        11956800
119569     MOVE WS-DLR-CNTL-ENT           TO MXCW021-ENTITY-NO.         11956900
119570     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11957000
119571     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11957100
119572     MOVE MXCW021-LANG-TEXT         TO WS-D18-LITERAL3.           11957200
119573                                                                  11957300
119574     MOVE 43                        TO TABLE-ENTRY-WANTED.        11957400
119575     MOVE WS-DLR-CNTL-ENT           TO MXCW021-ENTITY-NO.         11957500
119576     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11957600
119577     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11957700
119578     MOVE MXCW021-LANG-TEXT         TO WS-D18-LITERAL4.           11957800
119579                                                                  11957900
119580     MOVE 44                        TO TABLE-ENTRY-WANTED.        11958000
119581     MOVE WS-DLR-CNTL-ENT           TO MXCW021-ENTITY-NO.         11958100
119582     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         11958200
119583     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            11958300
119584     MOVE MXCW021-LANG-TEXT         TO WS-D18-LITERAL5.           11958400
119585                                                                  11958500
119586     WRITE XEROX-REC-D18                                          11958600
119587         FROM WS-XEROX-D18-LINE.                                  11958700
119590 1600-EXIT. EXIT.                                                 11959000
119591*03.857 E                                                         11959100
119600/***************************************************************  11960000
119700*  WRITE OUT THE LINES NEEDED FOR THE CREDIT MEMO TOTAL.       *  11970000
119800****************************************************************  11980000
119900 3000-WRITE-CREDIT-MEMO-TOTAL.                                    11990000
120000                                                                  12000000
120100**   ** 'CREDIT MEMO TOTAL'                                       12010000
120200     MOVE 36                        TO TABLE-ENTRY-WANTED.        12020000
120300     MOVE WS-CURR-LANG-IND          TO MXCW021-LANG-CODE.         12030000
120400     PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT.            12040000
120500     MOVE MXCW021-LANG-TEXT         TO WS-D17-LITERAL.            12050000
120600     MOVE TOT-CREDIT-MEMO           TO WS-D17-APPLIED-AMT.        12060000
120700     WRITE XEROX-REC-D17                                          12070000
120800         FROM WS-XEROX-D17-LINE.                                  12080000
120900                                                                  12090000
121000     MOVE ZEROES TO TOT-CREDIT-MEMO.                              12100000
121100                                                                  12110000
121200 3000-EXIT. EXIT.                                                 12120000
121300/***************************************************************  12130000
121400*  ACCUMULATE CHARGES BY BILL DATE AND CHARGE TYPE.            *  12140000
121500****************************************************************  12150000
121600 3001-ACCUMULATE-CHARGES.                                         12160000
121700                                                                  12170000
121800     PERFORM 8000-READ-INPUT THRU 8000-EXIT.                      12180000
121900                                                                  12190000
122000     IF  EOF-CREDIT-INFILE                                        12200000
122100         GO TO 3001-EXIT                                          12210000
122200     END-IF.                                                      12220000
122300                                                                  12230000
122400     IF  MXAW21-SK-RECORD-TYPE = '03'                             12240000
122500         NEXT SENTENCE                                            12250000
122600     ELSE                                                         12260000
122700         GO TO 3001-EXIT                                          12270000
122800     END-IF.                                                      12280000
122900                                                                  12290000
123000     IF  MXAW21-0300-BILL-DATE     = WS-HOLD-BILL-DATE            12300000
123100     AND MXAW21-0300-CHARGE-TYPE   = WS-D07-CHARGE-TYPE           12310000
123200         ADD MXAW21-0300-APPLIED-AMT TO TOT-CREDIT-MEMO           12320000
123300                                        TOT-CREDIT-DLR            12330000
123400                                        ACCUM-CHARGES             12340000
123500         GO TO 3001-ACCUMULATE-CHARGES                            12350000
123600     END-IF.                                                      12360000
123700                                                                  12370000
123800 3001-EXIT. EXIT.                                                 12380000
123900/***************************************************************  12390000
124000*  READ APPLIED CREDIT STATEMENT INPUT FILE.                   *  12400000
124100****************************************************************  12410000
124200 8000-READ-INPUT.                                                 12420000
124300     EXIT.                                                        12430000
124400                                                                  12440000
124500 8000-READ-AGAIN.                                                 12450000
124600                                                                  12460000
124700     READ CREDIT-INFILE                                           12470000
124800         INTO MXAW21-CREDIT-GENERAL-RECORD                        12480000
124900             AT END SET EOF-CREDIT-INFILE TO TRUE.                12490000
125000                                                                  12500000
125100     IF NOT EOF-CREDIT-INFILE AND WS-CREDIT-REPRINT               12510000
125200         IF MXAW21-SK-DLR-NBR = WS-REPRINT-DLR (1) OR             12520000
125300            MXAW21-SK-DLR-NBR = WS-REPRINT-DLR (2) OR             12530000
125400            MXAW21-SK-DLR-NBR = WS-REPRINT-DLR (3) OR             12540000
125500            MXAW21-SK-DLR-NBR = WS-REPRINT-DLR (4) OR             12550000
125600            MXAW21-SK-DLR-NBR = WS-REPRINT-DLR (5) OR             12560000
125700            MXAW21-SK-DLR-NBR = WS-REPRINT-DLR (6) OR             12570000
125800            MXAW21-SK-DLR-NBR = WS-REPRINT-DLR (7) OR             12580000
125900            MXAW21-SK-DLR-NBR = WS-REPRINT-DLR (8) OR             12590000
126000            MXAW21-SK-DLR-NBR = WS-REPRINT-DLR (9) OR             12600000
126100            MXAW21-SK-DLR-NBR = WS-REPRINT-DLR (10) OR            12610000
126200            MXAW21-SK-DLR-NBR = WS-REPRINT-DLR (11)               12620000
126300             NEXT SENTENCE                                        12630000
126400         ELSE                                                     12640000
126500             GO TO 8000-READ-AGAIN                                12650000
126600         END-IF                                                   12660000
126700     END-IF.                                                      12670000
126800                                                                  12680000
126900 8000-EXIT. EXIT.                                                 12690000
127000/**************************************************************** 12700000
127100*  FILL REPRINT TABLE WITH DEALER NUMBERS.                      * 12710000
127200*********************************************************** ***** 12720000
127300 8600-LOAD-REPRINT-TABLE.                                         12730000
127400                                                                  12740000
127500     MOVE +1 TO SUB4.                                             12750000
127600     MOVE +0 TO WS-REPRINT-DLR (1)                                12760000
127700                WS-REPRINT-DLR (2)                                12770000
127800                WS-REPRINT-DLR (3)                                12780000
127900                WS-REPRINT-DLR (4)                                12790000
128000                WS-REPRINT-DLR (5)                                12800000
128100                WS-REPRINT-DLR (6)                                12810000
128200                WS-REPRINT-DLR (7)                                12820000
128300                WS-REPRINT-DLR (8)                                12830000
128400                WS-REPRINT-DLR (9)                                12840000
128500                WS-REPRINT-DLR (10)                               12850000
128600                WS-REPRINT-DLR (11).                              12860000
128700                                                                  12870000
128800     PERFORM 8610-PROCESS-REPRINT-REC THRU 8610-EXIT              12880000
128900       VARYING SUB3 FROM 1 BY 1                                   12890000
129000         UNTIL SUB3 > 11.                                         12900000
129100                                                                  12910000
129200     COMPUTE MAX-SUB = SUB4 - 1.                                  12920000
129300                                                                  12930000
129400 8600-EXIT. EXIT.                                                 12940000
129500***************************************************************** 12950000
129600*  FILL REPRINT TABLE WITH DEALER NUMBERS.                      * 12960000
129700***************************************************************** 12970000
129800 8610-PROCESS-REPRINT-REC.                                        12980000
129900                                                                  12990000
130000     IF WS-REPRINT-DLR-INP (SUB3) > SPACES AND                    13000000
130100        WS-REPRINT-DLR-INP (SUB3) IS NUMERIC                      13010000
130200         MOVE WS-REPRINT-DLR-INP (SUB3)                           13020000
130300             TO WS-REPRINT-DLR (SUB4)                             13030000
130400         ADD +1 TO SUB4                                           13040000
130500         SET WS-CREDIT-REPRINT TO TRUE                            13050000
130600     END-IF.                                                      13060000
130700                                                                  13070000
130800 8610-EXIT. EXIT.                                                 13080000
130900/***************************************************************  13090000
131000*  WRITE OUT TOTALS FOR LAST CREDIT MEMO AND DEALER.           *  13100000
131100*  WRITE OUT DEFAULT XEROX DJDE COMMANDS TO OUTPUT FILE, CLOSE *  13110000
131200*  FILES.                                                      *  13120000
131210*  WRITE OUT TOTALS FOR LAST CREDIT MEMO AND DEALER.           *  13121000
131220* 03.857 -       - 12/31/03 - ADDED ROUTINE 1600- TO WRITE NEW *  13122000
131230*                  D18 RECORD WHICH CONTAINS BRIDGE TEXT.      *  13123000
131300****************************************************************  13130000
131400 9900-TERMINATE.                                                  13140000
131500                                                                  13150000
131600     IF OUTPUT-CREATED                                            13160000
131700         PERFORM 3000-WRITE-CREDIT-MEMO-TOTAL THRU 3000-EXIT      13170000
131800**       ** 'GRAND TOTAL CREDITS'                                 13180000
131900         MOVE 37                TO TABLE-ENTRY-WANTED             13190000
132000         MOVE WS-CURR-LANG-IND   TO MXCW021-LANG-CODE             13200000
132100         PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT         13210000
132200         MOVE MXCW021-LANG-TEXT TO WS-D17-LITERAL                 13220000
132300         MOVE TOT-CREDIT-DLR    TO WS-D17-APPLIED-AMT             13230000
132400         MOVE ZEROES            TO TOT-CREDIT-DLR                 13240000
132500         WRITE XEROX-REC-D17                                      13250000
132600             FROM WS-XEROX-D17-LINE                               13260000
132610         PERFORM 1600-WRITE-D18 THRU 1600-EXIT                    13261000
132700         WRITE XEROX-REC-ZZZ                                      13270000
132800             FROM WS-XEROX-ZZZ-LINE                               13280000
132900         INITIALIZE TRIPLE-ZERO-RECORD                            13290000
133000         MOVE ZEROS             TO CC-REC-TYPE                    13300000
133100         SET  MXBW510-EOJ       TO TRUE                           13310000
133200         PERFORM 1132-CONTROL-REPORT        THRU 1132-EXIT        13320000
133300     END-IF.                                                      13330000
133400                                                                  13340000
133500     CLOSE CREDIT-INFILE                                          13350000
133600           XEROX-OUTFILE.                                         13360000
133700                                                                  13370000
133800     OPEN OUTPUT REPRINT-FILE.                                    13380000
133900     CLOSE REPRINT-FILE.                                          13390000
134000                                                                  13400000
134100 9900-EXIT. EXIT.                                                 13410000
134200/**************************************************************** 13420000
134300*  INCLUDE ENVIRONMENT ROUTINE -                                  13430000
134400***************************************************************** 13440000
134500* RIS 00.636 START                                                13450000
134600     EXEC SQL                                                     13460000
134700          INCLUDE MXWP31                                          13470000
134800     END-EXEC.                                                    13480000
134900* RIS 00.636 END                                                  13490000
135000/**************************************************************** 13500000
135100*9000-LANGUAGE-TRANSLATION. PARAGRAPH IS IN FOLLOWING COPYBOOK    13510000
135200***************************************************************** 13520000
135300     EXEC SQL                                                     13530000
135400         INCLUDE MXWP35                                           13540000
135500     END-EXEC.                                                    13550000
135600/**************************************************************** 13560000
135700*  INCLUDE ABEND ROUTINE                                        * 13570000
135800***************************************************************** 13580000
135900     EXEC SQL                                                     13590000
136000          INCLUDE MXWP02                                          13600000
136100     END-EXEC.                                                    13610000