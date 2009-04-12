TITLE UnusedSymbols
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
writer :  /\\o//\      - December the 31, 2004 -

 'LabelList'
 'SearchSortedRegularLabel'
 'LowSigns'
 'DoneFlag'
 'BuildPlainLabelList'

;;
____________________________________________________________________________________________

____________________________________________________________________________________________
____________________________________________________________________________________________

RemoveLocalLabels:
    VirtualFree D$PlainLabelList

    Mov eax D$LabelList, eax D$eax | inc eax

    VirtualAlloc PlainLabelList eax

    Mov edx D$LabelList | add edx D$edx
    Mov edi D$PlainLabelList | add edi 5
    Mov esi D$LabelList | add esi 5

    .While esi < edx
        cmp B$esi+2 EOI | jne L1>
            cmp B$esi 'A' | jb L1>
            cmp B$esi 'Z' | ja L1>
                cmp B$esi+1 '0' | jb L1>
                cmp B$esi+1 '9' | ja L1>
                  ; |L0|dWord Byte| >>> 9
                    add esi 9 | jmp L2>

L1:     While B$esi <> EOI
            movsb
        End_While
        movsb   ; |
        movsd   ; Ptr
        movsb   ; Flag
        movsb   ; |
L2: .End_While

    Mov eax edi | Mov D$EndOfPlainLabelList eax | sub eax D$PlainLabelList
    Mov edi D$PlainLabelList | stosd | Mov al EOI | stosb
ret
____________________________________________________________________________________________

[WorkBuffer: ? #256]

[CodeLabelNameList: ?
 CodeLabelNameList.Current: ?
 DataLabelNameList: ?
 DataLabelNameList.Current: ?
 UnusedCodeAndDataDialogHandle: ?
 RecompileWanted: ?
 NumberOfUnusedDataLabels: ?
 NumberOfUnusedCodeLabels: ?]


;Task : Seperate CodeLabels from DataLabel, (Put into a list of their own)
Proc DisplayUnusedSymbolsDialog:
    Argument @hwnd
        Call RemoveLocalLabels
        Mov eax D$PlainLabelList | Mov eax D$eax
        push eax
            VirtualAlloc CodeLabelNameList eax
        pop eax
            VirtualAlloc DataLabelNameList eax

        Mov edi D$CodeLabelNameList, D$CodeLabelNameList.Current edi
        Mov edi D$DataLabelNameList, D$DataLabelNameList.Current edi

        Mov ecx D$PlainLabelList | add ecx D$ecx
        Mov esi D$PlainLabelList | add esi 5
        Mov edi WorkBuffer
        Mov D$NumberOfUnusedCodeLabels 0, D$NumberOfUnusedDataLabels 0

        .While esi < ecx
L1:         While B$esi <> EOI
                movsb
            End_While

            Test B$esi+5 DoneFlag | jnz L1>>

            Mov B$edi 0 | inc edi

            Test B$esi+5 CodeLabelFlag | jz L2>
                push esi
                    Mov edi D$CodeLabelNameList.Current
                    Mov esi WorkBuffer
                    While B$esi <> 0 | movsb | End_While
                    Mov B$edi 0 | inc edi
                    Mov D$CodeLabelNameList.Current edi
                    inc D$NumberOfUnusedCodeLabels
                pop esi
            jmp L1>

            L2: Test B$esi+5 DataLabelFlag | jz L1>
                push esi
                    Mov edi D$DataLabelNameList.Current esi WorkBuffer
                    While B$esi <> 0 | movsb | End_While
                    Mov B$edi 0 | inc edi
                    Mov D$DataLabelNameList.Current edi
                    inc D$NumberOfUnusedDataLabels
                pop esi
            Jmp L1>
L1:         add esi 7
            Mov edi WorkBuffer
L2:     .End_While
; Tag Dialog 4
        Call 'USER32.EndDialog' D@hwnd, 0
        Call 'USER32.DialogBoxParamA' D$hInstance, 4, 0, UnusedCodeAndDataDialogCallBack, 0

        VirtualFree D$CodeLabelNameList
        VirtualFree D$DataLabelNameList
EndP
____________________________________________________________________________________________

[CodeListBox 101
 DataListbox 102
 UnUsedSymbolsEditBox 111
 FindDeclaration 4
 FreeSearchCheckBox 5
 UnusedSymbolHelpbutton 9]

[TextToRetrive: B$ ? #256]
[FocusedChild: D$ ?
CurrentListBox: ?

CodeListBox.Handle: ?
DataListBox.Handle: ?
UnUsedSymbolsEditBox.Handle: ?
FindDeclaration.Handle: ?
FreeSearchCheckBox.Handle: ?
UnusedSymbolHelpbutton.Handle: ?]
DataAndCodeLabelListBoxNotification:
   movzx ebx ax | Mov D$CurrentListBox ebx
   shr eax 16
   If ax = &LBN_DBLCLK
      Call 'user32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, D$CurrentListBox,
                                        &LB_GETCARETINDEX 0 0
      Call 'user32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, D$CurrentListBox,
                                        &LB_GETTEXT eax TextToRetrive
      Call 'user32.SetDlgItemTextA' D$UnusedCodeAndDataDialogHandle, UnUsedSymbolsEditBox,
                                    TextToRetrive
      Call SearchForUnusedSymbol
   Else_If ax = &LBN_SETFOCUS
        move D$FocusedChild D$CurrentListBox
   End_If
ret

[EM_SETFOCUS 0100]
UnUsedSymbolsEditBoxNotification:
   shr eax 16
   If ax = EM_SETFOCUS
        Mov D$FocusedChild UnUsedSymbolsEditBox
   End_If
ret

[CharDistance 020]
SearchForTheSymbolInUnUsedSymbolsEditBox:
   Call 'user32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, UnUsedSymbolsEditBox,
                                        &WM_GETTEXTLENGTH 0 0
   inc eax
   Mov D$LenOfSearchedString eax



   Call 'user32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, UnUsedSymbolsEditBox,
                                        &WM_GETTEXT eax TextToRetrive


   Mov eax TextToRetrive

   While B$eax <> 0
      cmp B$eax 'Z' | jbe L0>
      cmp B$eax 'A' | ja L0>
      sub B$eax CharDistance
     L0:
     inc eax
   End_While

    Mov eax TextToRetrive
    Mov ecx D$LenOfSearchedString


    Mov esi TextToRetrive, edi ControlString | rep movsb
    Mov esi TextToRetrive, edi SearchString ecx D$LenOfSearchedString | rep movsb

    dec D$LenOfSearchedString
    push D$DownSearch, D$WholeWordSearch, D$SkipDashLines
        Mov B$DownSearch &TRUE, B$WholeWordSearch &FALSE, B$SkipDashLines &TRUE
        Mov D$NextSearchPos 0
        On B$RealSourceRestored = &FALSE, Call RestoreRealSource
            move D$CurrentWritingPos D$CodeSource
            Call StringSearch
            push D$CodeSourceA D$CodeSourceB
                Mov esi D$CurrentWritingPos | sub esi 2 | Call InternalRightClick
            pop D$CodeSourceB D$CodeSourceA
            Mov B$FinfOrReplace &FALSE
        Call SetPartialEditionFromPos
    pop D$SkipDashLines, D$WholeWordSearch, D$DownSearch
    On B$StringFound = &FALSE, Call 'USER32.SetForegroundWindow' D$UnusedCodeAndDataDialogHandle


ret



____________________________________________________________________________________________

[FreeSearchActive: &FALSE]
SearchForUnusedSymbol:
    Mov eax TextToRetrive, ecx 0

    While b$eax <> 0
        inc ecx | inc eax
    End_While
    inc ecx

    push eax ecx
        Call 'USER32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, FreeSearchCheckBox,
                                     &BM_GETCHECK, 0, 0

        Mov D$FreeSearchActive eax
        cmp eax &TRUE
    pop ecx eax | je L0>

            Mov B$eax ':', B$eax + 1 0 | inc ecx
    L0:
    Mov D$LenOfSearchedString ecx

    Mov eax TextToRetrive
    While B$eax <> 0
        cmp B$eax 'Z' | jbe L0>
        cmp B$eax 'A' | ja L0>
        sub B$eax CharDistance
     L0:
        inc eax
    End_While


PerformSearch:

    Mov esi TextToRetrive, edi ControlString | rep movsb
    Mov esi TextToRetrive, edi SearchString ecx D$LenOfSearchedString | rep movsb

    dec D$LenOfSearchedString
    push D$DownSearch, D$WholeWordSearch, D$SkipDashLines
        Mov eax D$FreeSearchActive | sub eax 1 | neg eax
        Mov B$DownSearch &TRUE, B$WholeWordSearch al, B$SkipDashLines &TRUE
        Mov D$NextSearchPos 0
        On B$RealSourceRestored = &FALSE, Call RestoreRealSource
            move D$CurrentWritingPos D$CodeSource
            Call StringSearch
            push D$CodeSourceA D$CodeSourceB
                Mov esi D$CurrentWritingPos | sub esi 2 | Call InternalRightClick
            pop D$CodeSourceB D$CodeSourceA
            Mov B$FinfOrReplace &FALSE
        Call SetPartialEditionFromPos

    pop D$SkipDashLines, D$WholeWordSearch, D$DownSearch

    On B$StringFound = &FALSE, Call 'USER32.SetForegroundWindow' D$UnusedCodeAndDataDialogHandle
ret


DesideSearchForUnusedSymbols:
  If D$FocusedChild = UnUsedSymbolsEditBox
      Call SearchForTheSymbolInUnUsedSymbolsEditBox
  Else
     Mov ax &LBN_DBLCLK | shl eax 16 | Mov ax w$FocusedChild | Call DataAndCodeLabelListBoxNotification
  End_If

  On B$StringFound = &FALSE, Call 'USER32.SetForegroundWindow' D$UnusedCodeAndDataDialogHandle
ret
____________________________________________________________________________________________

[NoteForWorkerBee:"You can press F5 to compile, without losing this list" 0]

; Tag Dialog 4


[OldUnusedSymbolDialogSubClassProc:  ?
 OldUnusedSymbolDialogSubClassProc1: ?
 OldUnusedSymbolDialogSubClassProc2: ?
 OldUnusedSymbolDialogSubClassProc3: ?
 OldUnusedSymbolDialogSubClassProc4: ?
 OldUnusedSymbolDialogSubClassProc5: ?
 OldUnusedSymbolDialogSubClassProc6: ?]

Proc UnusedSymbolDialogSubClassProc:
Arguments @hwnd, @msg, @wParam, @lParam

    .if D@msg = &WM_KEYDOWN
        if D@Wparam = &VK_F5
            Mov B$RecompileWanted &TRUE
            Mov B$ShowStats &FALSE
            Call SaveUnusedIndex
            push D$UnusedCodeAndDataDialogHandle
                Mov D$UnusedCodeAndDataDialogHandle 0
                Mov B$UnusedSymbolsWanted &TRUE
            pop eax
            Call 'USER32.EndDialog' eax, 0
            ExitP
        end_if
    .end_if


   @DefaultProcessing:
   Mov eax D@hwnd
   .if eax = D$CodeListBox.Handle
       Call D$OldUnusedSymbolDialogSubClassProc1 D@hwnd, D@msg, D@wParam, D@lParam
   .else_if eax = D$DataListBox.Handle
       Call D$OldUnusedSymbolDialogSubClassProc2 D@hwnd, D@msg, D@wParam, D@lParam
   .else_if eax = D$UnUsedSymbolsEditBox.Handle
       Call D$OldUnusedSymbolDialogSubClassProc3 D@hwnd, D@msg, D@wParam, D@lParam
   .else_if eax = D$FindDeclaration.Handle
       Call D$OldUnusedSymbolDialogSubClassProc4 D@hwnd, D@msg, D@wParam, D@lParam
   .else_if eax = D$UnusedSymbolHelpbutton.Handle
       Call D$OldUnusedSymbolDialogSubClassProc5 D@hwnd, D@msg, D@wParam, D@lParam
   .else
       Call D$OldUnusedSymbolDialogSubClassProc6 D@hwnd, D@msg, D@wParam, D@lParam
   .end_if
EndP

Proc InstallSubClassCallbacks:
Argument @hwnd
            Call 'USER32.GetDlgItem' D@hwnd CodeListBox
            if eax <> 0
                Mov D$CodeListBox.Handle eax
                Call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                Mov D$OldUnusedSymbolDialogSubClassProc1 eax
            end_if

            Call 'USER32.GetDlgItem' D@hwnd DataListbox
            if eax <> 0
                Mov D$DataListBox.Handle eax
                Call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                Mov D$OldUnusedSymbolDialogSubClassProc2 eax
            end_if

            Call 'USER32.GetDlgItem' D@hwnd UnUsedSymbolsEditBox
            if eax <> 0
                Mov D$UnUsedSymbolsEditBox.Handle eax
                Call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                Mov D$OldUnusedSymbolDialogSubClassProc3 eax
            end_if

            Call 'USER32.GetDlgItem' D@hwnd FindDeclaration
            if eax <> 0
                Mov D$FindDeclaration.Handle eax
                Call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                Mov D$OldUnusedSymbolDialogSubClassProc4 eax
            end_if
            Mov D$FindDeclaration.Handle eax

            Call 'USER32.GetDlgItem' D@hwnd UnusedSymbolHelpbutton
            if eax <> 0
                Mov D$UnusedSymbolHelpbutton.Handle eax
                Call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                Mov D$OldUnusedSymbolDialogSubClassProc5 eax
            end_if

            Call 'USER32.GetDlgItem' D@hwnd FreeSearchCheckBox
            if eax <> 0
                Call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                Mov D$OldUnusedSymbolDialogSubClassProc6 eax
            end_if
EndP

Proc UnusedCodeAndDataDialogCallBack:
    Arguments @hwnd, @msg, @wParam, @lParam

       pushad

        Mov eax &FALSE
        ..If D@msg = &WM_COMMAND
            If D@wParam = &IDHELP
                Call Help, B_U_AsmName, UnusedSymbolsHelp, ContextHlpMessage
            End_if

            Mov B$StringFound &TRUE
            Mov eax D@wParam

            If ax = CodeListBox
                Call DataAndCodeLabelListBoxNotification

            Else_If ax = DataListBox
                Call DataAndCodeLabelListBoxNotification

            Else_If ax = FindDeclaration
                shr eax 16
                cmp ax &BN_CLICKED | jne L0>
                    Call DesideSearchForUnusedSymbols
                L0:
            Else_If ax = &IDOK
                  Call DesideSearchForUnusedSymbols

            Else_If ax = &IDCANCEL
                    Mov B$UnusedSymbolsWanted &FALSE
                    Mov D$UnusedSymbolsDialogWanted &FALSE
                    Mov D$UnusedCodeAndDataDialogHandle 0
                    Call 'USER32.EndDialog' D@hwnd, 0

            Else_If ax = UnUsedSymbolsEditBox

                Call UnUsedSymbolsEditBoxNotification
            End_If

        ..Else_If D@msg = &WM_MOUSEMOVE

        ..Else_If D@msg = &WM_HELP
             Call Help B_U_AsmName, UnusedSymbolsHelp, ContextHlpMessage

        ..Else_If D@msg = &WM_KEYDOWN
             Call Help, B_U_AsmName, UnusedSymbolsHelp, ContextHlpMessage

        ..Else_If D@msg = &WM_INITDIALOG
            Call InstallSubClassCallbacks D@hwnd

            Mov eax D@hwnd
            Mov D$UnusedCodeAndDataDialogHandle eax
            Call SetUnusedDialogPos D@hwnd
            Mov ecx D$NumberOfUnusedCodeLabels | jecxz L9>
            Mov eax D$CodeLabelNameList

L0:         push eax ecx
                push eax
                    While B$eax <> 0
                        cmp B$eax '@' | je L1>
                        inc eax
                    End_While
                pop eax
                push eax
                    Call 'user32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle,
                                                      CodeListBox, &LB_ADDSTRING, 0, eax
L1:             pop eax
            pop ecx eax
            While B$eax <> 0 | inc eax | End_While
            inc eax | dec ecx | jnz L0<

L9:         Mov ecx D$NumberOfUnusedDataLabels | jecxz L9>
            Mov eax D$DataLabelNameList

L0:         push eax ecx
                push eax
                    While B$eax <> 0
                        cmp B$eax '@' | je L1>
                        inc eax
                    End_While
                pop eax
                push eax
                    Call 'user32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle,
                                                      DataListbox, &LB_ADDSTRING, 0, eax
L1:             pop eax
            pop ecx eax
            While B$eax <> 0 | inc eax | End_While
            inc eax | dec ecx | jnz L0<

L9:         Call 'user32.SetDlgItemTextA' D$UnusedCodeAndDataDialogHandle,
                                          UnUsedSymbolsEditBox, NoteForWorkerBee

            Mov eax D$UnusedCodeIndex | or eax D$UnusedDataIndex
            On eax <> 0 Call RestoreUnusedIndex

        ..Else
            popad | Mov eax &FALSE | ExitP

        ..End_If

        popad | Mov eax &TRUE
EndP
____________________________________________________________________________________________

[UnusedCodeIndex: ?   UnusedDataIndex: ?]

SaveUnusedIndex:
    Call 'USER32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, CODELISTBOX,
                                      &LB_GETTOPINDEX, 0, 0
    Mov D$UnusedCodeIndex eax

    Call 'USER32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, DATALISTBOX,
                                      &LB_GETTOPINDEX, 0, 0
    Mov D$UnusedDataIndex eax
ret

RestoreUnusedIndex:
    Call 'USER32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, CODELISTBOX,
                                      &LB_SETTOPINDEX, D$UnusedCodeIndex, 0

    Call 'USER32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, DATALISTBOX,
                                      &LB_SETTOPINDEX, D$UnusedDataIndex, 0
ret
____________________________________________________________________________________________

Proc SetUnusedDialogPos:
    Argument @hwnd
    Structure @WINDOWPLACEMENT 44,
              WINDOWPLACEMENT.iLengthDis 0,
              WINDOWPLACEMENT.flagsDis 4,
              WINDOWPLACEMENT.showCmdDis 8,
              WINDOWPLACEMENT.ptMinPosition.xDis 12,
              WINDOWPLACEMENT.ptMinPosition.yDis 16,
              WINDOWPLACEMENT.ptMaxPosition.xDis 20,
              WINDOWPLACEMENT.ptMaxPosition.yDis 24,
              WINDOWPLACEMENT.rcNormalPosition.leftDis 28,
              WINDOWPLACEMENT.rcNormalPosition.topDis 32,
              WINDOWPLACEMENT.rcNormalPosition.rightDis 36,
              WINDOWPLACEMENT.rcNormalPosition.bottomDis 40

        Mov D$WINDOWPLACEMENT.iLengthDis 44

        Call 'USER32.GetWindowPlacement' D$UnusedCodeAndDataDialogHandle, D@WINDOWPLACEMENT

        Call 'USER32.GetSystemMetrics' &SM_CXSCREEN | sub eax 5
        Mov ecx D$WINDOWPLACEMENT.rcNormalPosition.rightDis
        sub ecx D$WINDOWPLACEMENT.rcNormalPosition.leftDis
        Mov D$WINDOWPLACEMENT.rcNormalPosition.rightDis eax
        sub eax ecx | Mov D$WINDOWPLACEMENT.rcNormalPosition.leftDis eax

        Call 'USER32.GetSystemMetrics' &SM_CYSCREEN | sub eax 30
        Mov ecx D$WINDOWPLACEMENT.rcNormalPosition.bottomDis
        sub ecx D$WINDOWPLACEMENT.rcNormalPosition.topDis
        Mov D$WINDOWPLACEMENT.rcNormalPosition.bottomDis eax
        sub eax ecx | Mov D$WINDOWPLACEMENT.rcNormalPosition.topDis eax

        Call 'USER32.SetWindowPlacement' D$UnusedCodeAndDataDialogHandle, D@WINDOWPLACEMENT
EndP

____________________________________________________________________________________________

ReInitUnusedDialog:
    Call SaveUnusedIndex
    Call 'USER32.EndDialog' D$UnusedCodeAndDataDialogHandle, 0
    Mov D$UnusedCodeAndDataDialogHandle 0
    Mov B$UnusedSymbolsWanted &TRUE
    Mov B$RecompileWanted &TRUE
    Mov B$ShowStats &FALSE
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
