000100171216      * =======================================================================*
000200171216      * Module...: WRTCSV     - Write data to a CSV file in the IFS.           *
000300171216      * Author...: Ted Holt                                                    *
000400171216      * Date.....: 11/09/2011                                                  *
000500171216      *                                                                        *
000600171216      * This is the only module in service program WRTCSV.                     *
001400171216      * =======================================================================*
001500171216      *                                                                        *
001600171216      * To compile:
001700171216      *    (1) CRTRPGMOD MODULE(<lib>/WRTCSV)
001800171216      *                  SRCFILE(<lib>/QRPGLESRC) SRCMBR(WRTCSV)
001900171216      *    (2) CRTSRVPGM SRVPGM(<lib>/WRTCSV)
002000171216      *                  MODULE(<lib>/WRTCSV)
002100171216      *                  SRCFILE(<lib>/QSRVSRC) SRCMBR(WRTCSV)
002200171216      *                  TEXT('Write to a CSV file in the IFS')
002300171216      *                  BNDSRVPGM((IFSIORTNS))
002400171216      *                                                                        *
002500171216      * =======================================================================*
002600130906
002700111102     H nomain
002800070426     H option(*srcstmt: *nodebugio)
002900070426
003000111102      /copy prototypes,wrtcsv
003100111109      /copy prototypes,ifsiortns
003200111103
003300111103     D Prep            pr           256a   varying
003400111103     D  inString                    256a   const varying
003500111103
003600111103     D TryNumeric      pr            10i 0
003700111103     D  inString                    256a   const varying
003800111103     D  ouString                    256a         varying
003900111102
004000111104     D AssignColumn    pr            10i 0
004100111104     D  inChar                        1a   const
004200111104
004300111104     D PrepString      pr           256a   varying
004400111104     D  inString                    256a   varying const
004500111104
004600171216      * Globals
004700111103     D gSep            s              8a   varying inz(',')
004800140610     D gNumericFormat  s              1a   varying inz('V')
004900140610      ***? V = variable length (default)
005000140610      ***? F = fixed length
005100111103
005200111103      * ==============================================================
005300111103      * State table
005400111103      * ==============================================================
005500111103     D NbrOfRows       c                   const(6)
005600111103     D NbrOfColumns    c                   const(6)
005700111103
005800111103     D StateTable      ds
005900111103     D   Row                         48a   dim(NbrOfRows) ctdata perrcd(1)
006000111103     D** size of Row must be NbrOfColumns * size of Action
006100111103
006200111103     D ptr             s               *   inz(%addr(StateTable))
006300111103
006400111103     D State           ds                  dim(NbrOfRows) qualified based(ptr)
006500111103     D   Action                       8a   dim(NbrOfColumns)
006600111103
006700111102      * ==============================================================
006800111109      * The default field separator is a single comma.  This procedure
006900111109      * provides a way to change it to another set of 1-8 characters.
007000111109      * ==============================================================
007100111103     P SetCSVSep       b                   export
007200111103     D                 pi
007300111103     D  inSep                         8a   varying const
007400111103      /free
007500111103          gSep = inSep;
007600111103      /end-free
007700111103     P                 e
007800140610      * ==============================================================
007900140610      * Numeric values are reduced to simplest format by default.
008000140610      * This procedure provides a way to make them fixed length.
008100140610      * ==============================================================
008200140610     P SetNumericFormat...
008300140610     P                 b                   export
008400140610     D                 pi
008500140610     D  inFormat                      1a   const
008600140610      /free
008700140610          if inFormat = 'F'
008800140610          or informat = 'f';
008900140610             gNumericFormat = 'F';
009000140610          else;
009100140610             gNumericFormat = 'V';
009200140610          endif;
009300140610      /end-free
009400140610     P                 e
009500111103      * ==============================================================
009600111102     P OpenCSVFile     b                   export
009700111102     D                 pi            10i 0
009800111109     D  inPath                      256a   varying const
009900111109     D  ouFileDes                    10i 0
010000111102     D*** locals
010100111109
010200111102      /free
010300111102          monitor;
010400111109             ouFileDes = OpenTextFile (inPath: rwWrite: 'T':
010500111109                             *on: *off: '2': 1252);
010600111102          on-error;
010700111102             return 199;
010800111102          endmon;
010900111102          return 0;
011000111102      /end-free
011100111102     P                 e
011200111102      * ==============================================================
011300111102     P WriteCSVData    b                   export
011400111102     D                 pi            10i 0
011500111109     D  inFileDes                    10i 0 const
011600111102     D  inData01                    256a   varying const
011700111102     D  inData02                    256a   varying const options(*nopass)
011800111102     D  inData03                    256a   varying const options(*nopass)
011900111102     D  inData04                    256a   varying const options(*nopass)
012000111102     D  inData05                    256a   varying const options(*nopass)
012100111102     D  inData06                    256a   varying const options(*nopass)
012200111102     D  inData07                    256a   varying const options(*nopass)
012300111102     D  inData08                    256a   varying const options(*nopass)
012400111102     D  inData09                    256a   varying const options(*nopass)
012500111102     D  inData10                    256a   varying const options(*nopass)
012600111102     D  inData11                    256a   varying const options(*nopass)
012700111102     D  inData12                    256a   varying const options(*nopass)
012800111104     D  inData13                    256a   varying const options(*nopass)
012900111104     D  inData14                    256a   varying const options(*nopass)
013000111104     D  inData15                    256a   varying const options(*nopass)
013100111104     D  inData16                    256a   varying const options(*nopass)
013200111104     D  inData17                    256a   varying const options(*nopass)
013300111104     D  inData18                    256a   varying const options(*nopass)
013400111104     D  inData19                    256a   varying const options(*nopass)
013500111104     D  inData20                    256a   varying const options(*nopass)
013600111104     D  inData21                    256a   varying const options(*nopass)
013700111104     D  inData22                    256a   varying const options(*nopass)
013800111104     D  inData23                    256a   varying const options(*nopass)
013900111104     D  inData24                    256a   varying const options(*nopass)
014000111104     D  inData25                    256a   varying const options(*nopass)
014100111109     D  inData26                    256a   varying const options(*nopass)
014200111109     D  inData27                    256a   varying const options(*nopass)
014300111109     D  inData28                    256a   varying const options(*nopass)
014400111109     D  inData29                    256a   varying const options(*nopass)
014500111109     D  inData30                    256a   varying const options(*nopass)
014600111109     D  inData31                    256a   varying const options(*nopass)
014700111109     D  inData32                    256a   varying const options(*nopass)
014800111109     D  inData33                    256a   varying const options(*nopass)
014900111109     D  inData34                    256a   varying const options(*nopass)
015000111109     D  inData35                    256a   varying const options(*nopass)
015100111102     D*** locals
015200111109     D*OutputData      ds          4096
015300111102     D LineOut         s           4096a   varying
015400111102      /free
015500111103
015600111109        //if %parms()> 13;
015700111109        //      return 398;
015800111109        //endif;
015900111103
016000111103          LineOut = Prep(inData01);
016100111109          // the following if's are nested, but not indented due to lack of space
016200111102          if %parms()>=3;
016300111103             LineOut = LineOut + gSep + Prep(inData02);
016400111102          if %parms()>=4;
016500111103             LineOut = LineOut + gSep + Prep(inData03);
016600111103          if %parms()>=5;
016700111103             LineOut = LineOut + gSep + Prep(inData04);
016800111103          if %parms()>=6;
016900111103             LineOut = LineOut + gSep + Prep(inData05);
017000111103          if %parms()>=7;
017100111103             LineOut = LineOut + gSep + Prep(inData06);
017200111103          if %parms()>=8;
017300111103             LineOut = LineOut + gSep + Prep(inData07);
017400111103          if %parms()>=9;
017500111103             LineOut = LineOut + gSep + Prep(inData08);
017600111103          if %parms()>=10;
017700111103             LineOut = LineOut + gSep + Prep(inData09);
017800111103          if %parms()>=11;
017900111103             LineOut = LineOut + gSep + Prep(inData10);
018000111103          if %parms()>=12;
018100111103             LineOut = LineOut + gSep + Prep(inData11);
018200111103          if %parms()>=13;
018300111103             LineOut = LineOut + gSep + Prep(inData12);
018400111104          if %parms()>=14;
018500111104             LineOut = LineOut + gSep + Prep(inData13);
018600111104          if %parms()>=15;
018700111104             LineOut = LineOut + gSep + Prep(inData14);
018800111104          if %parms()>=16;
018900111104             LineOut = LineOut + gSep + Prep(inData15);
019000111104          if %parms()>=17;
019100111104             LineOut = LineOut + gSep + Prep(inData16);
019200111104          if %parms()>=18;
019300111104             LineOut = LineOut + gSep + Prep(inData17);
019400111104          if %parms()>=19;
019500111104             LineOut = LineOut + gSep + Prep(inData18);
019600111104          if %parms()>=20;
019700111104             LineOut = LineOut + gSep + Prep(inData19);
019800111104          if %parms()>=21;
019900111104             LineOut = LineOut + gSep + Prep(inData20);
020000111104          if %parms()>=22;
020100111104             LineOut = LineOut + gSep + Prep(inData21);
020200111104          if %parms()>=23;
020300111104             LineOut = LineOut + gSep + Prep(inData22);
020400111104          if %parms()>=24;
020500111104             LineOut = LineOut + gSep + Prep(inData23);
020600111104          if %parms()>=25;
020700111104             LineOut = LineOut + gSep + Prep(inData24);
020800111104          if %parms()>=26;
020900111104             LineOut = LineOut + gSep + Prep(inData25);
021000111109          if %parms()>=27;
021100111109             LineOut = LineOut + gSep + Prep(inData26);
021200111109          if %parms()>=28;
021300111109             LineOut = LineOut + gSep + Prep(inData27);
021400111109          if %parms()>=29;
021500111109             LineOut = LineOut + gSep + Prep(inData28);
021600111109          if %parms()>=30;
021700111109             LineOut = LineOut + gSep + Prep(inData29);
021800111109          if %parms()>=31;
021900111109             LineOut = LineOut + gSep + Prep(inData30);
022000111109          if %parms()>=32;
022100111109             LineOut = LineOut + gSep + Prep(inData31);
022200111109          if %parms()>=33;
022300111109             LineOut = LineOut + gSep + Prep(inData32);
022400111109          if %parms()>=34;
022500111109             LineOut = LineOut + gSep + Prep(inData33);
022600111109          if %parms()>=35;
022700111109             LineOut = LineOut + gSep + Prep(inData34);
022800111109          if %parms()>=36;
022900111109             LineOut = LineOut + gSep + Prep(inData35);
023000111109          if %parms()>=37;
023100111104             return 391;
023200111102          endif;
023300111109          endif;
023400111109          endif;
023500111109          endif;
023600111109          endif;
023700111109          endif;
023800111109          endif;
023900111109          endif;
024000111109          endif;
024100111109          endif;
024200111109          endif;
024300111104          endif;
024400111103          endif;
024500111104          endif;
024600111104          endif;
024700111104          endif;
024800111104          endif;
024900111104          endif;
025000111104          endif;
025100111104          endif;
025200111104          endif;
025300111104          endif;
025400111104          endif;
025500111115          endif;
025600111104          endif;
025700111104          endif;
025800111103          endif;
025900111103          endif;
026000111103          endif;
026100111103          endif;
026200111103          endif;
026300111103          endif;
026400111103          endif;
026500111103          endif;
026600111102          endif;
026700111103
026800140610          LineOut = %trimr(LineOut:','); // remove trailing commas
026900140610          WriteTextRecord (inFileDes: %addr(LineOut:*DATA):
027000140610                             %len(LineOut): '*CRLF');
027100111102          return 0;
027200111102      /end-free
027300111102     P                 e
027400111102      * ==============================================================
027500111102     P CloseCSVFile    b                   export
027600111102     D                 pi            10i 0
027700111102     D  inFileID                     10i 0 const
027800111102     D*** locals
027900111102      /free
028000111102          monitor;
028100111109             CloseTextFile (inFileID);
028200111102          on-error;
028300111102             return 399;
028400111102          endmon;
028500111102          return 0;
028600111102      /end-free
028700111102     P                 e
028800111102      * ==============================================================
028900111109      * Prepare a parameter to be written to a CSV file
029000111109      * Numeric values will not be delimited.
029100111109      * Non-numeric values will be delimited if they contain commas
029200111109      * or quotes. Quotes will be doubled.
029300111109      * ==============================================================
029400111103     P Prep            b
029500111103     D                 pi           256a   varying
029600111103     D  inString                    256a   const varying
029700111103     D*** locals
029800111103     D WorkString      s            256a   varying
029900111103     D Status          s             10i 0
030000111103      /free
030100111109          // if the string contains a number, treat it as numeric.
030200111103          Status = TryNumeric(%trim(inString): WorkString);
030300140610          if Status = *zero;  // it's numeric
030400140610             if gNumericFormat = 'V';
030500140610                return WorkString;
030600140610             else;
030700140610                return inString;
030800140610             endif;
030900140610          else;
031000140610             return PrepString (inString);
031100111103          endif;
031200111103      /end-free
031300111103     P                 e
031400111103      * ==============================================================
031500111109      * Determine if the string contains a number or not.
031600111109      * The following rules are in effect.
031700111109      * (1) Leading and trailing blanks are ignored.
031800111109      * (2) A leading hyphen is allowed for negative numbers.
031900111109      * (3) Blanks are permitted between the hyphen and the first digit r
032000111109      *     or decimal point.
032100111109      * (4) Digits are optional before and after the decimal, but the string
032200111109      *     must contain at least one digit.
032300111109      * (5) No embedded blanks are allowed.
032400111109      * (6) The only permitted characters are digits, hyphen, and period.
032500111109      * (7) Numbers will be reformatted to their simplest form.
032600111109      *
032700111109      * Examples of valid values:
032800111109      *
032900111109      *        -----  -----  -----  -----
033000111109      *        1      1.     1.0
033100111109      *        - 1    -1     -1.    -1.0   etc.
033200111109      *        1.2
033300111109      *        .2     0.2
033400111109      * ==============================================================
033500111103     P TryNumeric      b
033600111103     D                 pi            10i 0
033700111103     D  inString                    256a   const varying
033800111103     D  ouString                    256a         varying
033900111103     D*** locals
034000111103     D Column          s             10u 0
034100111103     D CurrAction      s              5a
034200111103     D CurrentChar     s              1a
034300111103     D CurrState       s             10u 0
034400111103     D EOL             s              1a   inz(*loval)
034500111103     D CharNdx         s             10u 0
034600111103     D StringLength    s             10u 0
034700111103     D*Valu            s                   like(ouString)
034800111103     D Status          s             10i 0
034900111103     D*WorkString      s            256a   varying
035000111103
035100111103     D Hyphen          c                   const('-')
035200111103     D Period          c                   const('.')
035300111103
035400111103     D MinusStr        s              1a   varying
035500111103     D PeriodStr       s              1a   varying
035600111103     D LeftStr         s            256a   varying
035700111103     D RightStr        s            256a   varying
035800111103
035900111103     D DataType        s              1a   inz(cChar)
036000111103     D  cInt           c                   'I'
036100111103     D  cReal          c                   'R'
036200111103     D  cChar          c                   'C'
036300111103
036400111103      /free
036500111103         monitor;
036600111103            CharNdx = *zero;
036700111103            StringLength = %len(%trimr(inString));
036800111103            CurrState = 1;
036900111103
037000111103            dou (CurrState = *zero);
037100111103               // get next character of input string
037200111103               // Access the next character of the string
037300111103
037400111103               CharNdx += 1;
037500111103               if (CharNdx <= StringLength);
037600111103                  CurrentChar = %subst(inString:CharNdx);
037700111103               else;
037800111103                  CurrentChar = EOL; // End of line
037900111103               endif;
038000111103
038100111103               // Determine which column of the state table to use
038200111104               Column = AssignColumn (CurrentChar);
038300111104
038400111103               // retrieve the action from the state table
038500111103               CurrAction = %subst(State(CurrState).Action(Column):1:5);
038600111103               // carry out the action
038700111103               select;
038800111103                  when CurrAction = 'minus';
038900111103                     MinusStr = Hyphen;
039000111103                  when CurrAction = 'per  ';
039100111103                     PeriodStr = Period;
039200111103                  when CurrAction = 'left ';
039300111104                     LeftStr += CurrentChar;
039400111103                  when CurrAction = 'right';
039500111104                     RightStr += CurrentChar;
039600111103                  when CurrAction = 'int  ';
039700111103                     DataType = cInt;
039800111103                  when CurrAction = 'real ';
039900111103                     DataType = cReal;
040000111103               endsl;
040100111103               // set new state
040200111103               CurrState =
040300111103                  %uns(%subst(State(CurrState).Action(Column):7:1));
040400111103            enddo;
040500111109
040600111109            // The number has been extracted to parts.
040700111109            // Simplify each one as much as possible.
040800111109            if DataType = cInt
040900111109            or DataType = cReal;
041000111109               LeftStr = %triml(LeftStr: '0');   // remove leading zeros
041100111109               if LeftStr = *blanks;
041200111115                  LeftStr = '0';    // ensure at least one digit left of decimal point
041300111109               endif;
041400111109            endif;
041500111109            // Put the pieces together to get the final number.
041600111103            select;
041700111103               when DataType = cInt;
041800111103                 ouString = MinusStr + LeftStr;
041900111103               when DataType = cReal;
042000111109                 // don't return decimal & right if a whole number
042100111103                 if RightStr = *blanks
042200111103                 or RightStr = *zeros;
042300111103                    clear PeriodStr;
042400111103                    clear RightStr;
042500111103                 endif;
042600111103                 ouString = MinusStr + LeftStr +
042700111103                            PeriodStr + RightStr;
042800111103               other;
042900111103                  Status = 491;
043000111103            endsl;
043100111103         on-error;
043200111103            Status = 499;
043300111103         endmon;
043400111103         return Status;
043500111103      /end-free
043600111103     P                 e
043700111103      * ==============================================================
043800111109      * State machine -- assign the column according to the input character
043900111109      * ==============================================================
044000111104     P AssignColumn    b
044100111104     D                 pi            10i 0
044200111104     D  inChar                        1a   const
044300111104     D*** locals
044400111104     D EOL             s              1a   inz(*loval)
044500111104     D Hyphen          c                   const('-')
044600111104     D Period          c                   const('.')
044700111104
044800111104      /free
044900111104          select;
045000111104             when (inChar = EOL);
045100111104                return 1;
045200111104             when (inChar = *blank);
045300111104                return 2;
045400111104             when (inChar = Hyphen);
045500111104                return 3;
045600111104             when (inChar = Period);
045700111104                return 4;
045800111104             when (inChar >= '0'
045900111104              and  inChar <= '9');
046000111104                return 5;
046100111104             other;
046200111104                return 6;
046300111104          endsl;
046400111104      /end-free
046500111104     P                 e
046600111104      * ==============================================================
046700111109      * Prepare a non-numeric string for inclusion in a CSV file
046800111109      * by addding delimiters if needed and doubling quotation marks.
046900111109      * ==============================================================
047000111104     P PrepString      b
047100111104     D                 pi           256a   varying
047200111104     D  inString                    256a   varying const
047300111104     D*** locals
047400111104     D Quote           c                   const('"')
047500111104     D Comma           c                   const(',')
047600111104     D WorkString      s            256a   varying
047700111104     D Ndx             s             10i 0
047800111104      /free
047900111104          WorkString = %trimr(inString);
048000111104          if WorkString = *blanks;
048100111104              return WorkString;
048200111104          endif;
048300111104
048400111104          // strings that do not contain commas or quotes do not
048500111104          // have to be delimited
048600111104          if %scan(Comma: WorkString) <= *zero and
048700111104             %scan(Quote: WorkString) <= *zero;
048800111104             return WorkString;
048900111104          endif;
049000111104
049100111104          // double the quotation marks
049200111104          Ndx = %scan(Quote: WorkString:1);
049300111104          dow Ndx > *zero;
049400111104             WorkString = %replace('""': WorkString: Ndx: 1);
049500111104             if Ndx + 2 <= %len(WorkString);
049600111104                Ndx = %scan(Quote: WorkString: Ndx+2);
049700111104             else;
049800111104                Ndx = *zero;
049900111104             endif;
050000111104          enddo;
050100111104          return Quote + WorkString + Quote;
050200111104      /end-free
050300111104     P                 e
050400111104      * ==============================================================
050500070426
050600070426      * State table
050700070426
050800111103eol   * blank   minus   period  digit   other
050900070725======* ======= ======= ======= ======= =======
051000070501**
051100111103     /0      /1 minus/2 per  /4 left /3      /0
051200111103     /0      /2      /0 per  /4 left /3      /0
051300111103int  /0      /0      /0 per  /5 left /3      /0
051400111103     /0      /0      /0      /0 right/6      /0
051500111103real /0      /0      /0      /0 right/6      /0
051600111103real /0      /0      /0      /0 right/6      /0
