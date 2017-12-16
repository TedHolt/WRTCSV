000100070426      *========================================================================*
000200111109      * Module...: WRTCSV     - Write data to a CSV file in the IFS.           *
000300070426      * Author...: Ted Holt                                                    *
000400111109      * Date.....: 11/09/2011                                                  *
000500111109      *                                                                        *
000600111109      * This is the only module in service program WRTCSV.                     *
000700070426      *========================================================================*
000800130206
000900111109      * ==============================================================
001000130206     D SetCSVSep       pr
001100111103     D  inSep                         8a   varying const
001200111103      * ==============================================================
001300140610     D SetNumericFormat...
001400140610     D                 pr
001500140610     D  inFormat                      1a   const
001600140610      * ==============================================================
001700130206     D OpenCSVFile     pr            10i 0
001800111109     D  inPath                      256a   varying const
001900111109     D  ouFileDes                    10i 0
002000111102      * ==============================================================
002100130206     D WriteCSVData    pr            10i 0
002200111109     D  inFileDes                    10i 0 const
002300111102     D  inData01                    256a   varying const
002400111102     D  inData02                    256a   varying const options(*nopass)
002500111102     D  inData03                    256a   varying const options(*nopass)
002600111102     D  inData04                    256a   varying const options(*nopass)
002700111102     D  inData05                    256a   varying const options(*nopass)
002800111102     D  inData06                    256a   varying const options(*nopass)
002900111102     D  inData07                    256a   varying const options(*nopass)
003000111102     D  inData08                    256a   varying const options(*nopass)
003100111102     D  inData09                    256a   varying const options(*nopass)
003200111102     D  inData10                    256a   varying const options(*nopass)
003300111102     D  inData11                    256a   varying const options(*nopass)
003400111102     D  inData12                    256a   varying const options(*nopass)
003500111104     D  inData13                    256a   varying const options(*nopass)
003600111104     D  inData14                    256a   varying const options(*nopass)
003700111104     D  inData15                    256a   varying const options(*nopass)
003800111104     D  inData16                    256a   varying const options(*nopass)
003900111104     D  inData17                    256a   varying const options(*nopass)
004000111104     D  inData18                    256a   varying const options(*nopass)
004100111104     D  inData19                    256a   varying const options(*nopass)
004200111104     D  inData20                    256a   varying const options(*nopass)
004300111104     D  inData21                    256a   varying const options(*nopass)
004400111104     D  inData22                    256a   varying const options(*nopass)
004500111104     D  inData23                    256a   varying const options(*nopass)
004600111104     D  inData24                    256a   varying const options(*nopass)
004700111104     D  inData25                    256a   varying const options(*nopass)
004800111109     D  inData26                    256a   varying const options(*nopass)
004900111109     D  inData27                    256a   varying const options(*nopass)
005000111109     D  inData28                    256a   varying const options(*nopass)
005100111109     D  inData29                    256a   varying const options(*nopass)
005200111109     D  inData30                    256a   varying const options(*nopass)
005300111109     D  inData31                    256a   varying const options(*nopass)
005400111109     D  inData32                    256a   varying const options(*nopass)
005500111109     D  inData33                    256a   varying const options(*nopass)
005600111109     D  inData34                    256a   varying const options(*nopass)
005700111109     D  inData35                    256a   varying const options(*nopass)
005800111102      * ==============================================================
005900130206     D CloseCSVFile    pr            10i 0
006000111102     D  inFileID                     10i 0 const
