TITLE Complete        ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;;
  Flag in [Configuration]: 'CompletionWanted'.

  'BuildCompletionTable' is called after each Compilation. It builds a Table of
  symbols (Labels, Macros and Equates), form the Assembler Lists: 'CompletionTable',
  Where each Record is a zero ended String.
  
  'CompletionPointers' is a Table of Pointers to the 'CompletionTable' Strings Table.
  
  The 'CodeComplete' Routines set the 'Underline' Flag on if one (and only one)
  fitting Symbol has been found.
  
  Once the 'Underline' Flag is set on, if the user hit [Ctrl]/[Space], the 'Completion'
  Routine is called.
  
  
  General Completion Checking: 'CodeComplete'
  
  'Completion' is the default (User symbols) and route to api or Equates when wanted.
  
  User symbols Routines:
  
  'UserSymbolsComplete' > 'CompletionCompare'  >'IsTheSmallerOnePartOfOthers'
  'Completion' > 'TakeCompleteModel' > 'CompleteSubstitute'
  
  Api Routine:
  
  'ApiComplete' > 'ApiCompletion' > 'CompletionListProc' > 'ApiSubstitute' > 'ReadApiForCompletion'

  OS Equates:
  
  'WinEquatesComplete'
  'EquatesCompletion'  > 'CompletionListProc' > 'InitListForEquates' > 'Win32EquatesSubstitute'
  
  'ListAllEquates'
;;
____________________________________________________________________________________________

[CompletionMode: D$ ?] ; either 'Api', 'User', or 'Equ'

[CompletionTable: D$ ?
 CompletionPointers: D$ ?]

BuildCompletionTable:

    Call VirtualFree CompletionTable

    Call VirtualFree CompletionPointers

  ; Count how many Symbols:
    Mov ecx 0, edx 0
    Mov esi D$LabelList | add esi 5
    .While B$esi <> 0
        inc ecx | While B$esi > SPC | inc esi | End_While | add esi 7
    .End_While
    add edx esi | sub edx D$LabelList

    Mov esi D$MacroList | add esi 5
    .While B$esi <> 0
        inc ecx | While B$esi > SPC | inc esi | End_While | add esi 11
    .End_While
    add edx esi | sub edx D$MacroList

    Mov esi D$EquateList | add esi 5
    .While B$esi <> 0
        inc ecx | While B$esi > SPC | inc esi | End_While | add esi 11
    .End_While
    add edx esi | sub edx D$EquateList

  ; Allocate mem for Strings ('CompletionTable')
  ; and for list of pointers to Strings ('CompletionPointers'):

    inc ecx | shl ecx 2 | or ecx 1

    Call VirtualAlloc CompletionPointers,
                      ecx

    Call VirtualAlloc CompletionTable,
                      edx

    add D$CompletionTable 1

  ; Fill the Tables:
    Mov edi D$CompletionTable, ebx D$CompletionPointers

    Mov esi D$LabelList | add esi 5
    .While B$esi > SPC
        Mov D$ebx edi | add ebx 4
        Mov ecx 0
        While B$esi > SPC
            and B$esi 07F
            If B$esi = '@'
                sub ebx 4 | Mov D$ebx 0
L0:             dec edi | cmp B$edi 0 | jne L0<
L0:             inc esi | cmp B$esi SPC | ja L0<
                jmp L2>

            End_If
            movsb | inc ecx

        End_While
      ; Do not store all of Local Labels, and smaller than 4 Chars Labels
        If ecx < 4
            sub ebx 4 | Mov D$ebx 0
L0:         dec edi | cmp B$edi 0 | jne L0<
        End_If

L2:     add esi 7
        Mov B$edi 0 | inc edi
    .End_While

    Mov esi D$MacroList | add esi 5
    .While B$esi > SPC
        Mov D$ebx edi | add ebx 4
        While B$esi > SPC | and B$esi 07F |  movsb | End_While | add esi 11
        Mov B$edi 0 | inc edi
    .End_While

    Mov esi D$EquateList | add esi 5
    .While B$esi > SPC
        Mov D$ebx edi | add ebx 4
        While B$esi > SPC
            and B$esi 07F
            If B$esi = '@'
                sub ebx 4 | Mov D$ebx 0
L0:             dec edi | cmp B$edi 0 | jne L0<
L0:             inc esi | cmp B$esi SPC | ja L0<
                jmp L1>
            End_If
            movsb
        End_While
L1:     add esi 11
        Mov B$edi 0 | inc edi
    .End_While
ret

____________________________________________________________________________________________
____________________________________________________________________________________________


[CompletionWanted: D$ ?
 CompletionFound: D$ ?
 CompletionRuning: D$ ?]

[CompletionMinimumInput: D$ 3]

; Check what the user is typing in:

CodeComplete:
    Mov D$CompletionFound 0, B$Underline &FALSE, B$CompletionRuning &TRUE

    Mov eax D$STRUCT.EditData@CurrentWritingPos

    If B$eax <= SPC
        ; OK
    Else_If B$eax = ','
        ; OK
    Else_If B$eax = "'"
        ; OK
    Else_If B$eax = '"'
        ; OK
    Else_If B$eax = ';'
        ; OK
    Else_If B$eax = '|'
        ; OK
    Else
        jmp L9>>
    End_If

    Mov esi D$STRUCT.EditData@CurrentWritingPos
    Push D$esi, esi
        Mov B$esi 0 | dec esi
        Mov ecx 0 | While B$esi > SPC | dec esi | inc ecx | End_While | inc esi
        On ecx < D$CompletionMinimumInput, jmp L5>

        If B$esi+1 = '$'
            add esi 2 | On B$esi = '@', inc esi
            Call UserSymbolsComplete
        Else_If B$esi+1 = '@'
            add esi 2 | Call UserSymbolsComplete
        Else_If B$esi = "'"
            Call ApiComplete
        Else_If B$esi = '"'
            Call ApiComplete
        Else_If B$esi = '&'
            Call WinEquatesComplete
        Else_If B$esi > SPC
            Call UserSymbolsComplete
        End_If
L5: Pop esi, D$esi

    ..If D$CompletionFound = 1
        Mov B$Underline &TRUE

    ..Else_If D$CompletionFound > 20
      ; Too much > nop

    ..Else_If D$CompletionFound > 1
        .If D$CompletionMode = 'User'
            Call IsTheSmallerOnePartOfOthers

            If eax = &TRUE
                Move D$CompletionPointer D$ShorterSymbolPointer
                Mov B$Underline al
            End_If
        .End_If

    ..End_If

L9: ..If D$H.CompletionList <> 0
        .If B$Underline = &TRUE
            If D$CompletionMode = 'Api'
                Call ApiCompletion
            Else_If D$CompletionMode = 'Equ'
                Call EquatesCompletion
            End_If
        .Else
            Call 'USER32.DestroyWindow' D$H.CompletionList
            Mov D$H.CompletionList 0
        .End_If
    ..End_If
ret


; Search if the user entered text fits with some registred compiled Symbol:

UserSymbolsComplete:
    On D$CompletionTable = 0, ret ; jmp L5>>

    Mov ebx D$CompletionPointers, D$CompletionMode 'User'
    While D$ebx <> 0

        Mov edi D$ebx | add ebx 4
        Call CompletionCompare
        .If al = 0
            If D$CompletionFound = 1
                Move D$CompletionPointersList D$CompletionPointer
            Else_If D$CompletionFound > 20
              ; Too much > nop
            Else_If D$CompletionFound > 1
                Push ebx
                    Mov eax D$CompletionFound, ebx D$CompletionPointer
                    dec eax | Mov D$CompletionPointersList+eax*4 ebx
                Pop ebx
            End_If
        .End_If

    End_While
ret


CompletionCompare:
  ; esi > User Source // edi > CompletionTable
  ; (At esi, the user's text has been temporary zero ended)
    Push esi, edi, ebx
        Mov ecx 0
L0:     Mov bl B$edi | inc edi
L1:     Mov al B$esi | inc esi | cmp al '_' | je L1<

        .If al >= 'a'
            On al <= 'z', sub al SPC
        .Else_If al = 0
            If bl > SPC
                inc D$CompletionFound
                Pop ebx, edi, esi
                Mov D$CompletionPointer edi | ret
            End_If
        .End_If

        cmp al bl | jne L9>

        jmp L0<
L9: Pop ebx, edi, esi
ret


; Several fiting Symbols found:
; Take the smaller one if all others bigger ones have the same prefix:

[ShorterSymbolPointer: D$ ?
 ShorterSymbolSize: D$ ?]

IsTheSmallerOnePartOfOthers:
  ; get the smaller one:
    Mov esi CompletionPointersList

    Mov edx D$CompletionFound, ebx 0, D$ShorterSymbolSize 0-1

    .While ebx < edx
        Mov eax D$esi+ebx*4, ecx 0

        While B$eax > SPC | inc eax | inc ecx | End_While
        If ecx < D$ShorterSymbolSize
            Mov D$ShorterSymbolSize ecx
            Move D$ShorterSymbolPointer D$esi+ebx*4
        End_If

        inc ebx
    .End_While

  ; Now, is this smaller one a valid Prefix for all fitting Symbols:
    Mov ebx 0, edx CompletionPointersList

    .While ebx < D$CompletionFound
        Mov edi D$ShorterSymbolPointer, esi D$edx, ecx D$ShorterSymbolSize
        repe cmpsb
        If ecx <> 0
            Mov eax &FALSE | ret
        End_If
        inc ebx | add edx 4
    .End_While

    Mov eax &TRUE
ret


[CompletionPointer: D$ ?]

; 'Completion' is called when user hit [Ctrl][Space]

Completion:
    If D$CompletionMode = 'Api'
        jmp ApiCompletion
    Else_If D$CompletionMode = 'Equ'
        jmp EquatesCompletion
    End_If

  ; Default case for User Symbols Completion:

    Mov D$CompletionMode 'User'

    Call RestoreRealSource

    Mov esi D$CodeSource, edx D$STRUCT.EditData@SourceEnd

    .While esi < edx
        ...If D$esi = MLC
            add esi 4
            While D$esi <> MLC
                inc esi | On esi >= edx, jmp L9>>
            End_While
            add esi 4

        ...Else_If B$esi = ';'
            While B$esi > CR | inc esi | End_While

        ...Else_If B$esi = '"'
            inc esi
            While B$esi <> '"'
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ...Else_If B$esi = "'"
            inc esi
            While B$esi <> "'"
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ...Else
          ; Not necesserary the Declaration. Any previous evocation will do as well:
            Mov al B$esi-1
            If al = '['
                ; Good
            Else_If al <= SPC
                ; Good
            Else_If al = ','
                ; Good
            Else_If al = '$'
                ; Good
            Else_If al = '|'
                ; Good
            Else
                jmp L5>>
            End_If

          ; > edi > ebx > Edition Pointer  //  esi > ecx > Search Pointer
            Mov edi D$CompletionPointer, ecx esi

L0:         Mov al B$esi
            If al >= 'a'
                On al <= 'z', sub al SPC
            Else_If al = '_'
                inc esi | jmp L0<
            End_If

            If al <> B$edi
                Mov esi ecx | jmp L5>
            End_If

            inc esi | inc edi

            .If B$edi <= SPC
                If B$esi <= SPC
                    ; Good
                Else_If B$esi = ','
                    ; Good
                Else_If B$esi = ':'
                    ; Good
                Else_If B$esi = ';'
                    ; Good
                Else_If B$esi = '|'
                    ; Good
                Else
                    Mov esi ecx | jmp L5>
                End_If

                xchg esi ecx | Mov edi CompletionModel
                While esi < ecx | movsb | End_While | Mov B$edi 0 | jmp L9>

            .Else
                jmp L0<

            .End_If
        ...End_If

L5:     inc esi
    .End_While

L9: Call SetPartialEditionFromPos

    Call CompleteSubstitute
ret


[CompletionPointersList: D$ ? # 20]
[CompletionModel: D$ ? # 20]

; Substitution for User defined Symbols

CompleteSubstitute:
    Mov B$Underline &FALSE, B$Keys+&VK_CONTROL &FALSE
    Mov D$CompletionMode 0, B$CompletionRuning &TRUE

    Mov edi D$STRUCT.EditData@CurrentWritingPos
    dec edi | While B$edi > SPC | dec edi | End_While | inc edi
    If B$edi+1 = '$'
        add edi 2 | On B$edi = '@', inc edi
    Else_If B$edi+1 = '@'
        add edi 2
    End_If

    Mov B$OldBlockInside &FALSE
    While D$STRUCT.EditData@CurrentWritingPos > edi
        Push edi
            Call EraseLastChar
            Call AskForRedrawNow
        Pop edi
    End_While

    Mov esi CompletionModel

    While B$esi <> 0
        Push esi
            movzx eax B$esi | Call InsertSource
            Call AskForRedrawNow
        Pop esi
        inc esi
    End_While

    Mov B$CompletionRuning &FALSE
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; Api Completion

[ApiInput: D$ ? # 20]

; Search if the user entered text fits with some Api Name:

ApiComplete: ; Win32Functions  WinApiFirstPass
    Mov D$CompletionFound 0, D$CompletionMode 'Api'

    Mov eax D$esi-5 | or eax 020202020 | On eax <> 'Call', ret

    inc esi

  ; esi points now after the Text Separator. Is there some '.'?
    Mov ebx D$STRUCT.EditData@CurrentWritingPos
    While ebx > esi
        dec ebx
        If B$ebx = '.'
            inc ebx | Mov esi ebx | jmp L0>
        End_If
    End_While

  ; Make a Copy for the ListBox Edit: ;;; ???!!! What use now?
L0: Push esi
        Mov edi ApiInput
        While esi < D$STRUCT.EditData@CurrentWritingPos | movsb | End_While | Mov B$edi 0
    Pop esi

    Mov ecx D$STRUCT.EditData@CurrentWritingPos | sub ecx esi | On ecx < D$CompletionMinimumInput, ret

    Mov al B$esi, edi Win32Functions, ecx 0

    .While edi < EndOfFunctionsList
        ..If B$edi < al
          ; Next Line:
L2:         While B$edi <> LF | inc edi | End_While | inc edi

        ..Else_If B$edi > al
            ret

        ..Else
            Mov D$CompletionPointer edi
            Mov ebx esi | inc ebx | inc edi
            While ebx < D$STRUCT.EditData@CurrentWritingPos
                Mov cl B$ebx | cmp cl B$edi | jne L2<
                inc ebx | inc edi
            End_While
            Mov D$CompletionFound 1 | ret

        ..End_If

    .End_While
L9: ret

____________________________________________________________________________________________

ApiCompletion:
    Mov D$CompletionMode 'Api'

    If D$H.CompletionList <> 0
        Call 'USER32.DestroyWindow' D$H.CompletionList
    End_If

    Call 'USER32.CreateDialogParamA' D$H.Instance, 1200, D$H.MainWindow, CompletionListProc, 0

    Call 'USER32.SetFocus' D$H.MainWindow

    Mov B$Keys+&VK_CONTROL &FALSE
ret

____________________________________________________________________________________________

Proc ToCompletionList:
    Argument @Key

    Call 'USER32.GetDlgItem' D$H.CompletionList, 10
    Call 'USER32.SetFocus' eax

    Call 'USER32.SendDlgItemMessageA' D$H.CompletionList, 10, &LB_GETCURSEL, 0, 0

    .If D@Key = &VK_UP
        If eax > 0
            dec eax
            Call 'USER32.SendDlgItemMessageA' D$H.CompletionList, 10,
                                              &LB_SETCURSEL, eax, 0
        End_If

    .Else_If D@Key = &VK_DOWN
        inc eax
        Call 'USER32.SendDlgItemMessageA' D$H.CompletionList, 10, &LB_SETCURSEL, eax, 0

    .Else_If D@Key = CR
        Call CompletionListProc D$H.CompletionList, &WM_COMMAND, &IDOK, 0

    .End_If
EndP


[H.CompletionList: D$ ?]

; Tag Dialog 1200

Proc CompletionListProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_INITDIALOG
        On D$H.CompletionList <> 0,
            Call 'USER32.EndDialog' D$H.CompletionList, 0

        Move D$H.CompletionList D@hwnd | Call SetCompletionPos

        .If D$CompletionMode = 'Api'
            Call InitListForApi
        .Else_If D$CompletionMode = 'User'
            Mov ecx 0
        .Else_If D$CompletionMode = 'Equ'
            Call InitListForEquates
            If ecx = 0-1
                Call 'USER32.EndDialog' D$H.CompletionList, 0
                Mov D$H.CompletionList 0
            Else_If ecx = 0
                Call 'USER32.SendDlgItemMessageA' D$H.CompletionList, 10,
                                                  &LB_GETTEXT, 0, CompletionModel
                Mov ecx 0
            End_If
        .End_If

L1:     If ecx = 0
            Mov D$CompletionPointer CompletionModel
            Call AskForRedrawNow
            Call SubstituteFromEitherList
            Call 'USER32.EndDialog' D$H.CompletionList, 0
            Mov B$Underline &FALSE, D$H.CompletionList, 0
        Else
            Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &LB_SETCURSEL, 0, 0
        End_If

        jmp L8>>

    ...Else_If D@msg = &WM_COMMAND
        If D@wParam = &IDCANCEL
            Call 'USER32.EndDialog' D$H.CompletionList, 0
            Mov D$H.CompletionList 0

        Else_If D@wParam = &IDOK
            Call ListOk

        Else_If W@wParam+2 = &LBN_DBLCLK
            Call ListOk

        End_If

    ...Else_If D@msg = &WM_VKEYTOITEM
        If W@wParam = &VK_RETURN
            Call ListOk
            popad | Mov eax 0-2 | jmp L9>

        Else_If W@wParam = &VK_ESCAPE
            Call 'USER32.EndDialog' D$H.CompletionList, 0
            Mov D$H.CompletionList 0
            popad | Mov eax 0-2 | jmp L9>

        Else
            popad | Mov eax 0-1 | jmp L9>

        End_If

    ...Else_If D@msg = &WM_CHARTOITEM
      ; For Chars inputs to the User's Source. Does not work under 95:
        Call 'USER32.SetFocus' D$H.MainWindow

    ...Else
L8:     popad | Mov eax &FALSE | jmp L9>

    ...End_If

    popad | Mov eax &TRUE
L9: EndP

[ListRectangle:
 @X1: D$ ?
 @Y1: D$ ?
 @X2: @W: D$ ?
 @Y2: @H: D$ ?]

[HwndTitleHight: D$ ?]

SetCompletionPos:

    Call 'USER32.GetWindowRect' D$H.MainWindow,
                                STRUC.RECT.MainWindow

    Mov eax D$WindowX | sub D$WindowW eax

    Mov eax D$WindowY | sub D$WindowH eax

    Call 'USER32.GetClientRect' D$H.MainWindow,
                                ListRectangle

    Mov eax D$WindowH | sub eax D$ListRectangle@H | Mov D$HwndTitleHight eax

    Call 'USER32.GetWindowRect' D$H.CompletionList, ListRectangle

    ; Write the ApiList Window Width and hight, instead of RECTright, RECTbottom:
    Mov eax D$ListRectangle@X2 | sub eax D$ListRectangle@X1

    Mov D$ListRectangle@W eax

    Mov eax D$ListRectangle@Y2 | sub eax D$ListRectangle@Y1

    Mov D$ListRectangle@H eax

    ; Count how many Chars in the Underlinement:
    Mov esi D$STRUCT.EditData@CurrentWritingPos, ecx 0 | dec esi

    While B$esi > SPC | dec esi | inc ecx | End_While

    ; Translate Caret Pos into Pixels Pos:
    Mov eax D$STRUCT.EditData@CaretRow | sub eax ecx | Call RowToX eax | add eax D$WindowX

    Push eax

        Call LineToY D$STRUCT.EditData@CaretLine

        add eax D$WindowY | add eax D$HwndTitleHight

        Push eax

            ; Is the Caret in the upper of lower part of the Screen?
            Call 'USER32.GetSystemMetrics' &SM_CYSCREEN

            Mov ecx eax | shr ecx 1

    Pop ebx, eax

    If ebx < ecx

        ; Upper half: Set ListBox Pos under the user writing Pos:
        add ebx  D$FontHeight

    Else

        ; Lower half: Set ListBox Pos above the user writing Pos:
        sub ebx D$FontHeight | sub ebx D$ListRectangle@H

        On ebx > 0FFFF, Mov ebx 0

    End_If

    Call 'USER32.MoveWindow' D$H.CompletionList,
                             eax,
                             ebx,
                             D$ListRectangle@W,
                             D$ListRectangle@H,
                             &FALSE
ret


InitListForApi:
    Mov esi D$CompletionPointer, ecx 0
L0: Mov edi CompletionModel
    While B$esi > SPC | movsb | End_While | Mov B$edi 0
    Push esi, ecx
        Call 'USER32.SendDlgItemMessageA' D$H.CompletionList, 10, &LB_ADDSTRING,
                                          0, CompletionModel
    Pop ecx esi
    While B$esi <= SPC | inc esi | End_While
    Mov ebx esi, edi ApiInput
    While B$edi <> 0
        Mov al B$ebx | cmp al B$edi | jne L9>
        inc ebx | inc edi
    End_While
    inc ecx | Mov B$edi 0 | jmp L0<
L9: ret


ListOk:
    Call 'USER32.SendDlgItemMessageA' D$H.CompletionList, 10, &LB_GETCURSEL, 0, 0
    Call 'USER32.SendDlgItemMessageA' D$H.CompletionList, 10, &LB_GETTEXT, eax,
                                      CompletionModel
    Mov D$CompletionPointer CompletionModel
    Call SubstituteFromEitherList
    Call 'USER32.EndDialog' D$H.CompletionList, 0
    Mov B$Underline &FALSE, D$H.CompletionList, 0
ret


SubstituteFromEitherList:
    If D$CompletionMode = 'Api'
        Call ApiSubstitute
    Else_If D$CompletionMode = 'User'

    Else_If D$CompletionMode = 'Equ'
        Call Win32EquatesSubstitute
    End_If
Ret


ApiSubstitute:
    Mov B$CompletionRuning &TRUE, B$Underline &FALSE
    Mov esi D$STRUCT.EditData@CurrentWritingPos

    Call ReadApiForCompletion | On B$ApiFileOK = &FALSE, ret

L0: Call EraseLastChar | Call AskForRedrawNow
    Mov eax D$STRUCT.EditData@CurrentWritingPos
    .If B$ApiFound = &FALSE
        If B$eax-1 = '.'
            While B$eax-1 <> "'"
                dec eax | cmp B$eax-1 '"' | je L1>
            End_While
            jmp L1>
        End_If
    .End_If
    cmp B$eax-1 "'" | je L1>
    cmp B$eax-1 '"' | jne L0<

L1: movzx eax B$eax-1
    Push eax
        Mov esi D$CompletionPointer
        While B$esi > SPC
            Push esi
                movzx eax B$esi | Call InsertSource
                Move D$STRUCT.EditData@PhysicalCaretRow D$STRUCT.EditData@CaretRow
            Pop esi
            inc esi
        End_While
    Pop eax

    Mov esi D$STRUCT.EditData@CurrentWritingPos | On B$esi <> al, Call InsertSource
    Call AskForRedrawNow

    Mov B$ApiFound &FALSE, B$CompletionRuning &FALSE, D$CompletionMode 0
ret


ReadApiForCompletion:
    Mov B$ApiFound &FALSE
    Call OpenApiFunctionsFile | On B$ApiFileOK = &FALSE, ret

    Mov esi D$Win32ApiList, ebx D$CompletionPointer

    Mov edx D$ApiFileSize | add edx esi

L1: .While esi < edx
        inc esi
        ...If B$esi = '.'
            Mov edi ebx                 ; edi > Our model  // esi > Win32ApiList
            inc esi

            Do
                Mov al B$esi, ah B$edi

                .If al = '('
                    If ah <= SPC
                        Mov B$ApiFound &TRUE
                        While B$esi-1 > LF | dec esi | End_While
                        Mov edi ebx

                        While B$esi <> '(' | movsb | End_While | Mov B$edi 0 | jmp L9>
                    End_If
                .End_If

                inc esi | inc edi
            Loop_Until al <> ah

        ...End_If
    .End_While

L9:
    Call VirtualFree Win32ApiList

ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; OS Equates Completion

____________________________________________________________________________________________
____________________________________________________________________________________________


; Search if the user entered text fits with some Win32 Equate:

WinEquatesComplete:
    Mov D$CompletionFound 1, D$CompletionMode 'Equ'
ret


EquatesCompletion:
    Call IsEquatesEquThere

    If D$FL.Includes = &TRUE
        Call GetEquFilesMemory
        Call ReadEquatesEqu
        Call 'USER32.CreateDialogParamA' D$H.Instance, 1200, D$H.MainWindow, CompletionListProc, 0
        Call 'USER32.SetFocus' D$H.MainWindow
    End_If

    Call VirtualFree EquateIncMemory

    Mov B$Keys+&VK_CONTROL &FALSE
ret


InitListForEquates:
    Mov esi D$STRUCT.EditData@CurrentWritingPos, ecx 0

  ; Take an Upper Case copy of user input:
    While B$esi-1 > '&' | dec esi | End_While | Mov edi CompletionModel
    While esi < D$STRUCT.EditData@CurrentWritingPos
        lodsb | On al > 'Z', and al 00_11011111
        stosb
    End_While
    Mov B$edi 0

  ; Search fitting Items in Equates.equ:
    Mov edi D$EquateIncMemory, ecx 0, edx edi | add edx D$EquatesIncFileSize

L0: Mov esi CompletionModel, al B$esi
    .If B$edi = al
        While B$edi = al | inc esi | inc edi | Mov al B$esi | End_While
        If al = 0
            While B$edi > SPC | inc edi | End_While
            Push edx, ecx, D$edi, edi
                Mov B$edi 0 | dec edi
                While B$edi > LF
                    dec edi | On edi = D$EquateIncMemory, jmp L1>
                End_While
                inc edi
L1:             Call 'USER32.SendDlgItemMessageA' D$H.CompletionList, 10, &LB_ADDSTRING, 0, edi
            Pop edi, D$edi, ecx, edx
            inc ecx
            jmp L2>

        Else
L2:         While B$edi > LF | inc edi | End_While | inc edi
            On edi >= edx, jmp L9>
            Mov al B$CompletionModel
            On al >= B$edi, jmp L0<

        End_If

    .Else_If B$edi <= al
        jmp L2<

    .End_If

L9: dec ecx | On ecx = 0-1, Call ListAllEquates
ret


; No matching Equate found > propose the full List:

ListAllEquates:
    Call 'USER32.MessageBoxA' D$H.MainWindow, {B$ "Do you want the complete List?

(This may take some time to build the list)" EOS}, {B$ 'No such OS Equate' EOS},
&MB_YESNO__&MB_SYSTEMMODAL

    If eax = &IDNO
        Mov ecx 0-1 | ret
    End_If

    On D$EquateIncMemory = 0, ret
    On D$EquatesIncFileSize = 0, ret

    Mov esi D$EquateIncMemory, edx esi | add edx D$EquatesIncFileSize

L0: Mov edi esi | While B$edi > SPC | inc edi | End_While

    Push edx, esi D$edi, edi
        Mov B$edi 0
        Call 'USER32.SendDlgItemMessageA' D$H.CompletionList, 10, &LB_ADDSTRING, 0, esi
    Pop edi, D$edi, esi, edx

    If eax <> &LB_ERR
        Mov esi edi |  While B$esi-1 <> LF | inc esi | End_While
        On esi < edx, jmp L0<
    End_If
ret


Win32EquatesSubstitute:
    Mov B$CompletionRuning &TRUE, B$Underline &FALSE, D$CompletionMode 0

    Mov esi D$STRUCT.EditData@CurrentWritingPos

L0: Call EraseLastChar | ;Call AskForRedrawNow
    Mov eax D$STRUCT.EditData@CurrentWritingPos
    cmp B$eax-1 '&' | jne L0<

L1: Mov esi D$CompletionPointer
    While B$esi > SPC
        Push esi
            movzx eax B$esi | Call InsertSource
            Move D$STRUCT.EditData@PhysicalCaretRow D$STRUCT.EditData@CaretRow
        Pop esi
        inc esi
    End_While

    Call AskForRedrawNow

    Mov B$CompletionRuning &FALSE
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

KillCompletionList:

    ; While the Completion ListBox is runing, there are many user actions that may
    ; require to close this ListBox (moves of Caret by KeyBoard or Mouse)
    test D$H.CompletionList NA ZERO S1>

        Call 'USER32.SendMessageA' D$H.CompletionList,
                                   &WM_COMMAND,
                                   &IDCANCEL,
                                   0

S1: Mov D$Underline &FALSE

    Call AskForRedraw

ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
