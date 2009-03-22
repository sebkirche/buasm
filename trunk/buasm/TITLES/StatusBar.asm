TITLE StatusBar          ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;                                   Status Bar Job.
;
; Mainly done by Pit (Peter Tuente).
____________________________________________________________________________________________
____________________________________________________________________________________________

[H.StatusBar: D$ ?]

[STATUSBAR_ID              999
 ID_STATUS_BAR_TITLE        01
 ID_STATUS_BAR_TOTAL_LINES  02
 ID_STATUS_BAR_TOTAL_BYTES  03
 ID_STATUS_BAR_LINE         04
 ID_STATUS_BAR_ROW          05
 ID_STATUS_BAR_STATE        06
 ID_STATUS_BAR_NA7          07
 ID_STATUS_BAR_NA8          08
 ID_STATUS_BAR_NA9          09
 ID_STATUS_BAR_NA10         10]

[@: D$ len
 TABLE.StatusPartsPos:
 D$    0
 D$  100 ; 01 TITLE courant
 D$  176 ; 02 Total lines
 D$  264 ; 03 Bytes
 D$  340 ; 04 Line
 D$  396 ; 05 Row
 D$  472 ; 06 Modified/read to run
 D$  700
 D$  800
 D$  900
 D$ 1024]

[STR.A.StatusBar: B$ ? # 40]

[STR.A.StatusBarLineTotalLineText: B$ ' Lines' EOS
 STR.A.StatusBarLineOctetsText: B$ ' Bytes' EOS
 STR.A.StatusBarLineModified: B$ ' Modified' EOS
 STR.A.StatusBarLineUnModified: B$ ' Ready to run' EOS
 STR.A.StatusBarLineLinePos: B$ ' Line: ' STR.A.StatusBarLineLinePosNumber: B$ '           ' EOS
 STR.A.StatusBarLineRowPos: B$ ' Row: ' STR.A.StatusBarLineRowPosNumber: B$ '           ' EOS]

DecimalToAscii:

    ; input: eax = Number / edi = String Pointer

    Mov ecx 10,
        edx NA

L1: Push edx | xor edx edx | div ecx | test eax eax NOT_ZERO L1<

    Mov al dl

L2: add eax '0' | stosb | Pop eax | test eax eax POSITIVE L2<

    Mov al EOS | stosb

ret


StatusBarLineAppend:

    Mov edi STR.A.StatusBar,
        al EOS,
        ecx 40

    repne scasb | sub edi ASCII

L0: movsb | test B$esi-ASCII 0_FF NOT_ZERO L0<

ret
____________________________________________________________________________________________

[PointZeroed: D$ ?]

StatusBar:

    Mov eax D$STRUCT.EditData@CaretRowValue | shr eax 3 | add eax D$STRUCT.EditData@RightScroll | Mov D$StatusCol eax

  ; Name of the File, or of the TITLE:
    Mov esi D$LP.ActualTitle

    ..If esi <> 0

        On D$esi <> 'TITL', jmp L2>>

        While B$esi > SPC | inc esi | End_While
        While B$esi = SPC | inc esi | End_While
        Mov edi esi
        While B$edi > SPC | inc edi | End_While

        Push D$edi, edi

            Mov B$edi 0

            Call 'USER32.SendMessageA' D$H.StatusBar,
                                       &SB_SETTEXT,
                                       ID_STATUS_BAR_TITLE,
                                       esi

        Pop edi, D$edi

    ..Else
      ; eax: Simple flag for '.', because under some OS the .ext may be missing:
        Mov eax &FALSE, B$PointZeroed &FALSE
        Mov esi SaveFilter | On D$esi = 0, ret

        While B$esi <> 0 | inc esi | End_While
        While B$esi-1 <> '\'
            dec esi | On esi = SaveFilter, jmp L1>
            .If B$esi = '.'
                If B$PointZeroed = &FALSE
                    Push D$esi, esi
                    Mov B$esi 0
                    Mov eax &TRUE, B$PointZeroed &TRUE
                End_If
            .End_If
        End_While
L1:
        Push eax

            Call 'USER32.SendMessageA' D$H.StatusBar,
                                       &SB_SETTEXT,
                                       ID_STATUS_BAR_TITLE,
                                       esi

        Pop eax

        If eax = &TRUE

            Pop edi,
                D$edi

        End_If
    ..End_If
L2:
    ; Total Number of Sources Lines:
    Mov eax D$TotalNumberOfLines,
        edi STR.A.StatusBar

    Call DecimalToAscii

    Mov esi STR.A.StatusBarLineTotalLineText | Call StatusBarLineAppend

    Call 'USER32.SendMessageA' D$H.StatusBar,
                               &SB_SETTEXT,
                               ID_STATUS_BAR_TOTAL_LINES,
                               STR.A.StatusBar

    ; Total Number of Bytes:
    Mov eax D$STRUCT.EditData@SourceEnd | sub eax D$CodeSource

    Mov edi STR.A.StatusBar | Call DecimalToAscii

    Mov esi STR.A.StatusBarLineOctetsText | Call StatusBarLineAppend

    Call 'USER32.SendMessageA' D$H.StatusBar,
                               &SB_SETTEXT,
                               ID_STATUS_BAR_TOTAL_BYTES,
                               STR.A.StatusBar

    ; Line Pos:
    Mov eax D$StatusLine,
        edi STR.A.StatusBarLineLinePosNumber

    Call DecimalToAscii

    Call 'USER32.SendMessageA' D$H.StatusBar,
                               &SB_SETTEXT,
                               ID_STATUS_BAR_LINE,
                               STR.A.StatusBarLineLinePos

    ; Row Pos:
    Mov eax D$StatusCol,
        edi STR.A.StatusBarLineRowPosNumber

    Call DecimalToAscii

    Call 'USER32.SendMessageA' D$H.StatusBar,
                               &SB_SETTEXT,
                               ID_STATUS_BAR_ROW,
                               STR.A.StatusBarLineRowPos

    ; Source modified of ready to run:
    test D$FL.ReadyToRun &TRUE ZERO S1>

        mov eax STR.A.StatusBarLineUnModified

    jmp S2>

S1: mov eax STR.A.StatusBarLineModified

S2: Call 'USER32.SendMessageA' D$H.StatusBar,
                               &SB_SETTEXT,
                               ID_STATUS_BAR_STATE,
                               eax

ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT

