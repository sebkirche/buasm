TITLE Status
____________________________________________________________________________________________
____________________________________________________________________________________________

;                                   Status Bar Job.
;
; Mainly done by Pit (Peter Tuente).
____________________________________________________________________________________________
____________________________________________________________________________________________

[StatusbarHandle: ?    OnStatusBar: ?]

[STATUSBAR_ID 999]

[StatusPartsPos: 0 100 200 300 400 500 600 700 800 900 1000]

[SbLine: ? #10]
[SbLineTotalLineText: B$ ' Total Lines', 0
 SbLineOctetsText:       ' Bytes', 0
 SbLineModified:         'Modified', 0
 SbLineUnModified:       'Ready to Run', 0
 SbLineLinePos:          'Line: '
 SbLineLinePosNumber:    '           '
 SbLineRowPos:           'Row: '
 SbLineRowPosNumber:     '           '
 SbF9Off:                '[F9] Inspector Off', 0
 SbF9On:                 '[F9] Inspector On', 0 ]
[SbLine2: '12345', 0]


; > input: eax = Number / edi = String Pointer

FromBinaryToDecimalAscii:
    If eax = 0
        Mov B$edi '0', B$edi+1 0 | ret
    Else
        Mov ecx 10, edx 0-1
L0:     push edx | Mov edx 0 | div ecx | cmp eax 0 | ja L0<
        Mov al dl
L0:     add eax '0' | stosb | pop eax | cmp eax 0-1 | jne L0<
        Mov al 0 | stosb
    End_If
ret


SbLineAppend:
    Mov edi sbLine, al 0, ecx 40 | repne scasb | dec edi
    While B$esi <> 0
        movsb
    End_While
    Mov al 0 | stosb
ret

____________________________________________________________________________________________

[PointZeroed: ?]

StatusBar:
    Mov eax D$CaretRowValue | shr eax 3 | add eax D$RightScroll | Mov D$StatusCol eax

  ; Name of the File, or of the TITLE:
    Mov esi D$ActualTitle

    ..If esi <> 0
        On D$esi <> 'TITL', jmp L2>>

        While B$esi > ' ' | inc esi | End_While
        While B$esi = ' ' | inc esi | End_While
        Mov edi esi
        While B$edi > ' ' | inc edi | End_While
        push D$edi, edi
            Mov B$edi 0
            Call 'USER32.SendMessageA' D$StatusbarHandle, &SB_SETTEXT, 1, esi
        pop edi, D$edi
    ..Else
      ; eax: Simple flag for '.', because under some OS the .ext may be missing:
        Mov eax &FALSE, B$PointZeroed &FALSE
        Mov esi SaveFilter | On D$esi = 0, ret

        While B$esi <> 0 | inc esi | End_While
        While B$esi-1 <> '\'
            dec esi | On esi = SaveFilter, jmp L1>
            .If B$esi = '.'
                If B$PointZeroed = &FALSE
                    push D$esi, esi
                    Mov B$esi 0
                    Mov eax &TRUE, B$PointZeroed &TRUE
                End_If
            .End_If
        End_While
L1:     push eax
            Call 'USER32.SendMessageA' D$StatusbarHandle, &SB_SETTEXT, 1, esi
        pop eax
        If eax = &TRUE
            pop edi, D$edi
        End_If
    ..End_If
L2:
  ; Total Number of Sources Lines:
    Mov eax D$TotalNumberOfLines, edi SbLine | Call FromBinaryToDecimalAscii
    Mov esi SbLineTotalLineText | Call SbLineAppend
    Call 'USER32.SendMessageA' D$StatusbarHandle &SB_SETTEXT 2 SbLine

  ; Total Number of Bytes:
    Mov eax D$SourceEnd | sub eax D$CodeSource
    Mov edi SbLine | Call FromBinaryToDecimalAscii
    Mov esi SbLineOctetsText | Call SbLineAppend
    Call 'USER32.SendMessageA' D$StatusbarHandle &SB_SETTEXT 3 SbLine

  ; Line Pos:
    Mov eax D$StatusLine, edi SbLineLinePosNumber | Call FromBinaryToDecimalAscii
    Call 'USER32.SendMessageA' D$StatusbarHandle, &SB_SETTEXT, 4, SbLineLinePos

  ; Row Pos:
    Mov eax D$StatusCol, edi SbLineRowPosNumber | Call FromBinaryToDecimalAscii
    Call 'USER32.SendMessageA' D$StatusbarHandle, &SB_SETTEXT, 5, SbLineRowPos

  ; Source modified of ready to run:
    If B$ReadyToRun = &TRUE
        Call 'USER32.SendMessageA' D$StatusbarHandle &SB_SETTEXT 6 SbLineUnModified
    Else
        Call 'USER32.SendMessageA' D$StatusbarHandle, &SB_SETTEXT, 6, SbLineModified
    End_If
ret


[ModifiedRect: ? ? ? ?]














