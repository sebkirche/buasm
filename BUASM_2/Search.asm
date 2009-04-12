TITLE Search
 _______________________________________________________________________________________
 _______________________________________________________________________________________

;                              Search / replace routines:
 _______________________________________________________________________________________
 _______________________________________________________________________________________


; Dialog Box for Find/Replace and simple search. writing
; "U$ 04 0 0 0CA 03D" at FRdialog+4 turns it simple search

[FRdialog: D$ 090C008C2 0      ; Style
 U$ 0C 0 0 0CB 06A             ; Dim  >>>   U$ 04 0 0 0CA 03D  ; Dim for search only
 0                             ; Menu (not yet)
 0                             ; Class(not yet)
 'Search what' 0               ; Title
 08 'Helv' 0]                  ; Font

[FRC0: D$ 050000202 0          ; Style
 U$ 02 0 0C8 093               ; Dim
 012C                          ; ID
 0FFFF 085                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data

[FRC1: D$ 050000001 0          ; Style
 U$ 089 02A 03F 011            ; Dim
 &FR_FINDNEXT                   ; ID
 0FFFF 080                     ; Class
 'Search' 0                    ; Title
 0]                            ; No creation data

[FRC2: D$ 050000000 0          ; Style
 U$ 089 013 03F 011            ; Dim
 &FR_DIALOGTERM                 ; ID
 0FFFF 080                     ; Class
 'Close' 0                     ; Title
 0]                            ; No creation data

[FRC3: D$ 050000003 0          ; Style
 U$ 047 029 036 010            ; Dim
 FR_WHOLEWORD                  ; ID
 0FFFF 080                     ; Class
 'Whole word' 0                 ; Title
 0]                            ; No creation data

[FRC4: D$ 050000007 0          ; Style
 U$ 02 0F 080 02C              ; Dim
 041                           ; ID
 0FFFF 080                     ; Class
 'Search Flags' 0              ; Title
 0]                            ; No creation data

[FRC5: D$ 050000009 0          ; Style
 U$ 07 019 038 010             ; Dim
 FR_UP                         ; ID
 0FFFF 080                     ; Class
 'Upward' 0                    ; Title
 0]                            ; No creation data

[FRC6: D$ 050000009 0          ; Style
 U$ 07 029 038 010             ; Dim
 FR_DOWN                       ; ID
 0FFFF 080                     ; Class
 'Downward' 0                  ; Title
 0]                            ; No creation data

[FRC7: D$ 050000003 0          ; Style
 U$ 047 01B 032 0E             ; Dim
 FR_MATCHCASE                  ; ID
 0FFFF 080                     ; Class
 'Case' 0                      ; Title
 0]                            ; No creation data    <<<<<<<<<<<< end of simple search

[FRC8: D$ 050000027 0          ; Style
 U$ 0 049 0CC 07               ; Dim
 0133                          ; ID
 0FFFF 080                     ; Class
 'Replace with' 0              ; Title
 0]                            ; No creation data

[FRC9: D$ 050000000 0          ; Style
 U$ 042 046 040 011            ; Dim
 FR_REPLACEALL                 ; ID
 0FFFF 080                     ; Class
 'Replace all' 0               ; Title
 0]                            ; No creation data

[FRC10: D$ 050000000 0         ; Style
 U$ 089 046 03F 011            ; Dim
 FR_REPLACE                    ; ID
 0FFFF 080                     ; Class
 'Find/Replace' 0              ; Title
 0]                            ; No creation data

[FRC11: D$ 050000202 0         ; Style
 U$ 01 05B 0C8 093             ; Dim
 0136                          ; ID
 0FFFF 085                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data



[FR_UP 150    FR_DOWN 151    FR_WHOLEWORD 154    FR_MATCHCASE 155
 FR_REPLACEALL 156    FR_REPLACE 157]

________________________________________________________________________________________

; these setting use the same Dialog Box data for both options:

SetFindReplaceBox:
    Mov B$Replace &TRUE
    Mov edi FRdialog | add edi 8
    Mov ax 0C | stosw | Mov eax 0 | stosd | Mov eax 06A_00CB | stosd  ; U$ 0C 0 0 0CB 06A
    jmp L9>

SetSimpleSearchBox:
    Mov B$Replace &FALSE
    Mov edi FRdialog | add edi 8
    Mov ax 08 | stosw | Mov eax 0 | stosd | Mov eax 0_3D_00CB | stosd  ; U$ 04 0 0 0CB 03D

L9: If D$H.FindReplace = 0
        Call 'User32.CreateDialogIndirectParamA' D$H.Instance, FRdialog, D$H.MainWindow, FRproc, 0
    Else
        Beep
    End_If
ret


[UserSearchStringHandle: ?  UserReplaceStringHandle: ?  OnReplace: ?
 LenOfSearchedString: ?     LenOfReplaceString: ?]

[StringChange: ?]

GetUserSearchString:
    Mov edi ControlString, esi SearchString, ecx 120 | rep movsb
    Mov edi SearchString, al 0, ecx 120 | rep stosb
    Call 'User32.SendMessageA' D$UserSearchStringHandle &WM_GETTEXTLENGTH 0 0
    Mov D$LenOfSearchedString eax
    On eax = 0, ret
    Mov D$LenOfSearchedString eax
 ; "WM_GETTEXTLENGTH" message does not include end mark. "WM_GETTEXT" does >>> inc eax
    inc eax
    Call 'User32.SendMessageA' D$UserSearchStringHandle &WM_GETTEXT eax SearchString
    Mov B$OnReplace &FALSE

  ; Control if user changed the string for FindOrReplace flag in FRproc. (if he changed
  ; between a Find and a Replace, we have to reset to search again):
    Mov B$StringChange &FALSE
    Mov edi ControlString, esi SearchString, ecx 120 | rep cmpsb | je L5>
      Mov B$StringChange &TRUE

  ; add (only new) string to List and delete last one (limit=15 strings):

L5: Call 'User32.SendMessageA' D$UserSearchStringHandle  &CB_FINDSTRINGEXACT 0-1 SearchString
    If eax = &CB_ERR
        Call 'User32.SendMessageA' D$UserSearchStringHandle &CB_INSERTSTRING 0  SearchString
        Call 'User32.SendMessageA' D$UserSearchStringHandle &CB_DELETESTRING 15 0
    End_If
ret

GetUserReplaceString:
    Mov edi ReplaceWithString, al 0, ecx 120 | rep stosb
    Call 'User32.SendMessageA' D$UserReplaceStringHandle, &WM_GETTEXTLENGTH, 0, 0
    Mov D$LenOfReplaceString eax
    On eax = 0, ret
    inc eax
    Call 'User32.SendMessageA' D$UserReplaceStringHandle, &WM_GETTEXT, eax, ReplaceWithString
    Mov B$OnReplace &TRUE

  ; add (only new) string to List and delete last one (limit=15 strings):
    Call 'User32.SendMessageA' D$UserReplaceStringHandle, &CB_FINDSTRINGEXACT, 0-1,
                               ReplaceWithString
    If eax = &CB_ERR
      Call 'User32.SendMessageA' D$UserReplaceStringHandle, &CB_INSERTSTRING, 0,
                                 ReplaceWithString
      Call 'User32.SendMessageA' D$UserReplaceStringHandle, &CB_DELETESTRING, 15, 0
    End_If
ret


; Set them all to previous state (third parameter) at initialisation time:

SetSearchFlagButtons:
    Call 'User32.GetDlgItem' D$H.FindReplace, FR_DOWN
    Call 'User32.SendMessageA' eax, &BM_SETCHECK, D$DownSearch, 0

    Call 'User32.GetDlgItem' D$H.FindReplace, FR_UP
    Mov ebx D$DownSearch | xor ebx &TRUE
    Call 'User32.SendMessageA' eax, &BM_SETCHECK, ebx, 0

    Call 'User32.GetDlgItem' D$H.FindReplace, FR_MATCHCASE
    Call 'User32.SendMessageA' eax, &BM_SETCHECK, D$CaseSearch, 0

    Call 'User32.GetDlgItem' D$H.FindReplace, FR_WHOLEWORD
    Call 'User32.SendMessageA' eax, &BM_SETCHECK, D$WholeWordSearch, 0
ret


[STR01: B$ ? #120] [STR02: B$ ? #120] [STR03: B$ ? #120] [STR04: B$ ? #120] [STR05: B$ ? #120]
[STR06: B$ ? #120] [STR07: B$ ? #120] [STR08: B$ ? #120] [STR09: B$ ? #120] [STR10: B$ ? #120]
[STR11: B$ ? #120] [STR12: B$ ? #120] [STR13: B$ ? #120] [STR14: B$ ? #120] [STR15: B$ ? #120]
[STR16: ?]
[RTR01: B$ ? #120] [RTR02: B$ ? #120] [RTR03: B$ ? #120] [RTR04: B$ ? #120] [RTR05: B$ ? #120]
[RTR06: B$ ? #120] [RTR07: B$ ? #120] [RTR08: B$ ? #120] [RTR09: B$ ? #120] [RTR10: B$ ? #120]
[RTR11: B$ ? #120] [RTR12: B$ ? #120] [RTR13: B$ ? #120] [RTR14: B$ ? #120] [RTR15: B$ ? #120]
[RTR16: ?] [STR_RTR_len: RTR16-STR01]

StoreSearchStrings:
  ; ecx = [(120 bytes * 30) / 4] + 2 (edges) = 902
    Mov edi STR01, eax 0, ecx D$STR_RTR_len | rep stosb                  ; clear all tables
    Call 'User32.SendMessageA' D$UserSearchStringHandle, &CB_GETCOUNT, 0, 0
    On eax = &CB_ERR, ret

    Mov edi STR01, ebx 0

    While eax > 0
      push eax, ebx, edi
        Call 'User32.SendMessageA' D$UserSearchStringHandle, &CB_GETLBTEXT, ebx, edi
      pop edi, ebx, eax
      dec eax | inc ebx | add edi 120
    End_While

    Call 'User32.SendMessageA' D$UserReplaceStringHandle, &CB_GETCOUNT, 0, 0
    On eax = &CB_ERR, ret

    Mov edi RTR01, ebx 0

    While eax > 0
      push eax, ebx, edi
        Call 'User32.SendMessageA' D$UserReplaceStringHandle, &CB_GETLBTEXT, ebx, edi
      pop edi, ebx, eax
      dec eax | inc ebx | add edi 120
    End_While
ret


RestoreSearchStrings:
    Mov esi STR01, ebx 0
    While B$esi > 0
        push ebx, esi
            Call 'User32.SendMessageA' D$UserSearchStringHandle, &CB_INSERTSTRING, ebx, esi
        pop esi, ebx
        inc ebx | add esi 120
    End_While

    Mov esi RTR01, ebx 0
    While B$esi > 0
        push ebx, esi
            Call 'User32.SendMessageA' D$UserReplaceStringHandle, &CB_INSERTSTRING, ebx, esi
        pop esi, ebx
        inc ebx | add esi 120
    End_While
ret

_____________________________________________________________________________________

[FinfOrReplace: D$ ?
 H.FindReplace: D$ ?]

Proc FRproc:
    Arguments @hwnd, @msg, @wParam, @lParam

    ...If D@msg  = &WM_INITDIALOG
        Mov B$ShiftBlockInside &FALSE

        move D$H.FindReplace D@hwnd
        Mov B$FinfOrReplace &FALSE                  ; flag 0 for Search / 1 Replace
        Call 'User32.GetDlgItem' D@hwnd 012C     ; 012C = our Find Edit Box
        Mov D$UserSearchStringHandle eax
        Call 'User32.GetDlgItem' D@hwnd 0136     ; 0136 = our Replace Edit Box
        Mov D$UserReplaceStringHandle eax
        Call RestoreSearchStrings
        Call SetSearchFlagButtons
        Call 'User32.SetFocus' D$UserSearchStringHandle  ; return 0  to
        Call SetIconDialog
        jmp L8>>                                        ; keep focus to first edit control

    ...Else_If D@msg = &WM_COMMAND
        Mov eax D@wParam
        .If eax = &FR_DIALOGTERM
L0:         Call StoreSearchStrings
            Mov D$H.FindReplace 0
            Call 'User32.DestroyWindow' D@hwnd
        .Else_If eax = &FR_FINDNEXT
L1:         Call GetUserSearchString
            On D$LenOfSearchedString = 0, jmp L8>>
                Call RestoreRealSource
                    Call StringSearch
                    On B$BlockInside = &TRUE, Mov B$FinfOrReplace &TRUE ; ready for Replace if whished
                Call SetPartialEditionFromPos
        .Else_If eax = &IDCANCEL
            jmp L0<
        .Else_If eax = &IDOK
            jmp L1<
        .Else_If eax = FR_DOWN
            Mov B$DownSearch &TRUE, B$FinfOrReplace &FALSE
        .Else_If eax = FR_UP
           Mov B$DownSearch &FALSE, B$FinfOrReplace &FALSE
        .Else_If eax = FR_MATCHCASE
           xor B$CaseSearch &TRUE | Mov B$FinfOrReplace &FALSE
        .Else_If eax = FR_WHOLEWORD
           xor B$WholeWordSearch &TRUE | Mov B$FinfOrReplace &FALSE
        .Else_If eax = FR_REPLACE
           Call GetUserSearchString | Call GetUserReplaceString
           ..If B$FinfOrReplace = &TRUE
                If B$StringChange = &TRUE   ; see "StringChange" comment in GetUserSearchString
                    Mov B$FinfOrReplace &FALSE
                    Call RestoreRealSource
                        Call StringSearch
                    Call SetPartialEditionFromPos
                Else
                  On D$LenOfSearchedString > 0, Call StringReplace  ; &TRUE > Replace
                End_If
           ..Else
                Call RestoreRealSource
                    Call StringSearch                               ; false > Search
                Call SetPartialEditionFromPos
           ..End_If
           xor B$FinfOrReplace &TRUE
        .Else_If eax = FR_REPLACEALL
           Call RestoreRealSource
                Call GetUserReplaceString | Call GetUserSearchString
                On D$LenOfSearchedString > 0,  Call StringReplaceAll
           Call SetPartialEditionFromPos
        .Else
           jmp L8>
        .End_If

        Call 'USER32.SetFocus' D@hwnd

    ...Else_If D@msg = &WM_CTLCOLOREDIT

        Call WM_CTLCOLOREDIT | Return

    ...Else
L8:     Return &FALSE

    ...End_If

    Mov eax &TRUE

L9: EndP



[SearchString: B$ ? #120]  [ReplaceWithString: B$ ? #120]  [ControlString: B$ ? #120]

[DownSearch:  ?  CaseSearch: ?  WholeWordSearch: ?   SkipDashLines: ?
 Replace: ?   StringFound: ?]
[StringNotFound: B$ 'String not found', 0]

[NextSearchPos: ?    SearchFirstChar: ?]


[Lowcase | cmp #1 'A' | jb C9> | cmp #1 'Z' | ja C9> | or #1 020 | C9: ]

; If user change the searched string inside a Find/Replace process, we have to reset
; the "FindOrReplace" flag in FRproc:

StringSearch:
    Mov B$ShiftBlockInside &FALSE, B$StringFound &TRUE

    On D$LenOfSearchedString = 0, ret
    Mov B$BlockInside &FALSE

L0: Mov edi SearchString, esi edi, edx D$LenOfSearchedString
    dec edx                                                     ; - first tested char
    If B$DownSearch = &TRUE
        Mov ah B$edi | inc edi | Mov ebx 1
    Else
        add edi edx | Mov ah B$edi | dec edi | std | Mov ebx 0-1; ebx > inc/dec edi in L4 loop
    End_If

    On B$CaseSearch = &FALSE, LowCase ah

    If D$NextSearchPos = 0
        Mov esi D$CurrentWritingPos
    Else
        Mov esi D$NextSearchPos
        On esi > D$SourceEnd, Mov esi D$CurrentWritingPos       ; case of massive block delete
    End_If

L2: cmp esi D$SourceEnd | ja L8>>                               ; search for fitting first Char:
    cmp esi D$CodeSource | jb L8>>
        lodsb | On B$CaseSearch = &FALSE, LowCase al
        cmp al ah | jne L2<

    ..If edx = 0                                                ; len = 1 > string found
        Mov D$NextSearchPos esi
    ..Else
       Mov ecx edx                                              ; first letter found:
       push eax, esi, edi
L4:      .If esi > D$SourceEnd
             pop edi, esi, eax | jmp L8>>
         .Else_If esi < D$CodeSource
             pop edi, esi, eax | jmp L8>>
         .Else
K0:          lodsb | Mov ah al
             If B$SkipDashLines = &TRUE
                cmp ah '_' | je K0<
             End_If
K0:          Mov al B$edi | add edi ebx
             If B$SkipDashLines = &TRUE
                cmp al '_' | je K0<
             End_If
             If B$CaseSearch = &FALSE
                LowCase ah
                LowCase al
             End_If
             If ah = al
                loop L4<
             Else
                 pop edi, esi, eax | jmp L2<<
             End_If
             Mov D$NextSearchPos esi
         .End_If
L5:   pop edi, esi, eax
    ..End_If

    Mov eax D$NextSearchPos                                 ; string found
    If B$DownSearch = &TRUE
        Mov bl B$eax | dec eax | Mov D$BlockEndTextPtr eax
        sub eax edx | Mov D$CurrentWritingPos eax | Mov D$BlockStartTextPtr eax
        Mov bh B$eax-1
    Else
        Mov bl B$eax | inc eax | Mov D$CurrentWritingPos eax | Mov D$BlockStartTextPtr eax
        Mov D$NextSearchPos eax
        add eax edx | Mov D$BlockEndTextPtr eax
        Mov bh B$eax+1
    End_If

    .If B$WholeWordSearch = &TRUE
        Mov al bl | Call WordEdge
        If B$Edge = &TRUE
            Mov al bh | Call WordEdge
        End_If
        If B$Edge = &FALSE
            jmp L0<<
        End_If
    .End_If

    std | Mov B$BlockInside &TRUE, D$RightScroll 0
L6: lodsb | cmp al LF | ja L6<
        dec esi | Mov ebx esi | add ebx 2                     ; for caret h. Pos. count
L6: lodsb | cmp al LF | ja L6<                              ; start printing 2 lines upper
        dec esi
L6: lodsb | cmp al LF | ja L6<
        add esi 2

    If esi >= D$CodeSource
        Mov D$UpperLine esi
    Else
        move D$UpperLine D$CodeSource
    End_If

    Call SetCaret D$BlockEndtextPtr | jmp L9>

L8: cld
    If B$OnReplaceAll = &FALSE

        Call MessageBox D$STR.A.MessageWindowTitleError,
                        StringNotFound,
                        &MB_USERICON+&MB_SYSTEMMODAL

        Mov B$StringFound &FALSE

    End_If

    Mov D$NextSearchPos 0

L9: cld | On B$Disassembling = &FALSE, Call AskForRedrawNow
ret


[ReplaceStart: ?]

StringReplace:
    Mov B$ShiftBlockInside &FALSE

    .If B$BlockInside = &TRUE
        Call ControlX
        Mov ecx D$BlockEndTextPtr
        Mov D$ReplaceStart ecx
        sub ecx D$BlockStartTextPtr | inc ecx
        dec D$CaretRow | dec D$PhysicalCaretRow
        Mov esi ReplaceWithString
        While B$esi <> 0
            lodsb
            pushad
                movzx eax al | Call InsertSource
            popad
            inc D$CaretRow | inc D$PhysicalCaretRow
            If al = LF
                Mov D$CaretRow 1
            End_If
        End_While
    .Else
       jmp StringSearch
    .End_If

    If B$DownSearch = &TRUE
        move D$NextSearchPos D$CurrentWritingPos
    Else
        move D$NextSearchPos D$ReplaceStart
    End_If

    Call AskForRedrawNow
ret


[AllDanger: B$ "

  Are you sure you want 'replace all' ?

" EOS]

[AllTitle: B$ "DANGER:" EOS]

[OnReplaceAll: SilentSearch: ?]

StringReplaceAll:

    Call MessageBox AllTitle,
                    AllDanger,
                    &MB_SYSTEMMODAL+&MB_ICONQUESTION++&MB_YESNO

    ..If D$FL.MsgBoxReturn = &IDYES
        Mov B$OnReplaceAll &TRUE

L0:     Call RestoreRealSource
            Call StringSearch
        Call SetPartialEditionFromPos

        cmp B$BlockInside &TRUE | jne L9>

        Call AskForRedrawNow | Call StringReplace | jmp L0<

L9:     Mov B$OnReplaceAll &FALSE
    ..End_If
ret

;StringReplaceAll:
    Mov B$ShiftBlockInside &FALSE

    Call MessageBox AllTitle,
                    AllDanger,
                    &MB_SYSTEMMODAL+&MB_ICONQUESTION++&MB_YESNO

    ..If D$FL.MsgBoxReturn = &IDYES
        Mov B$OnReplaceAll &TRUE
L0:     Call RestoreRealSource
        Call StringSearch | cmp B$BlockInside &TRUE | jne L9>
        Call StringReplace
        Call SetPartialEditionFromPos | jmp L0<

L9:     Mov B$OnReplaceAll &FALSE
        Call SetPartialEditionFromPos
    ..End_If
ret








