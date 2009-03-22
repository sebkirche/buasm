TITLE UnusedSymbols   ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;;
writer :  /\\o//\      - December the 31, 2004 -

 'LabelList'
 'SearchSortedRegularLabel'
 'LowSigns'
 'FLAG_DONE'
 'BuildPlainLabelList'

;;
____________________________________________________________________________________________

____________________________________________________________________________________________
____________________________________________________________________________________________

RemoveLocalLabels:

    Call VirtualFree PlainLabelList

    Mov eax D$LabelList, eax D$eax | inc eax

    Call VirtualAlloc PlainLabelList,
                      eax

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

[WorkBuffer: D$ ? # 256]

[CodeLabelNameList: D$ ?
 CodeLabelNameList.Current: D$ ?
 DataLabelNameList: D$ ?
 DataLabelNameList.Current: D$ ?
 H.UnusedCodeAndDataDialog: D$ ?
 RecompileWanted: D$ ?
 NumberOfUnusedDataLabels: D$ ?
 NumberOfUnusedCodeLabels: D$ ?]


;Task : Seperate CodeLabels from DataLabel, (Put into a list of their own)
Proc DisplayUnusedSymbolsDialog:
    Argument @hwnd
        Call RemoveLocalLabels
        Mov eax D$PlainLabelList | Mov eax D$eax

        Call VirtualAlloc CodeLabelNameList,
                          eax

        Call VirtualAlloc DataLabelNameList,
                          eax

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

            Test B$esi+5 FLAG_DONE NOT_ZERO L1>>

            Mov B$edi 0 | inc edi

            Test B$esi+5 FLAG_CODE_LABEL ZERO L2>
                Push esi
                    Mov edi D$CodeLabelNameList.Current
                    Mov esi WorkBuffer
                    While B$esi <> 0 | movsb | End_While
                    Mov B$edi 0 | inc edi
                    Mov D$CodeLabelNameList.Current edi
                    inc D$NumberOfUnusedCodeLabels
                Pop esi
            jmp L1>

            L2: Test B$esi+5 FLAG_DATA_LABEL ZERO L1>
                Push esi
                    Mov edi D$DataLabelNameList.Current esi WorkBuffer
                    While B$esi <> 0 | movsb | End_While
                    Mov B$edi 0 | inc edi
                    Mov D$DataLabelNameList.Current edi
                    inc D$NumberOfUnusedDataLabels
                Pop esi
            Jmp L1>
L1:         add esi 7
            Mov edi WorkBuffer
L2:     .End_While
; Tag Dialog 4
        Call 'USER32.EndDialog' D@hwnd, 0
        Call 'USER32.DialogBoxParamA' D$H.Instance, 4, 0, UnusedCodeAndDataDialogCallBack, 0

        Call VirtualFree CodeLabelNameList

        Call VirtualFree DataLabelNameList

EndP
____________________________________________________________________________________________

[CodeListBox 101
 DataListbox 102
 UnUsedSymbolsEditBox 111
 FindDeclaration 4
 FreeSearchCheckBox 5
 UnusedSymbolHelpbutton 9]

[TextToRetrive: B$ ? # 256]
[FocusedChild: D$ ?
CurrentListBox: D$ ?

H.CodeListBox: D$ ?
H.DataListBox: D$ ?
H.UnUsedSymbolsEditBox: D$ ?
H.FindDeclaration: D$ ?
H.UnusedSymbolHelpbutton: D$ ?]
DataAndCodeLabelListBoxNotification:
   movzx ebx ax | Mov D$CurrentListBox ebx
   shr eax 16
   If ax = &LBN_DBLCLK
      Call 'USER32.SendDlgItemMessageA' D$H.UnusedCodeAndDataDialog, D$CurrentListBox,
                                        &LB_GETCARETINDEX 0 0
      Call 'USER32.SendDlgItemMessageA' D$H.UnusedCodeAndDataDialog, D$CurrentListBox,
                                        &LB_GETTEXT eax TextToRetrive
      Call 'USER32.SetDlgItemTextA' D$H.UnusedCodeAndDataDialog, UnUsedSymbolsEditBox,
                                    TextToRetrive
      Call SearchForUnusedSymbol
   Else_If ax = &LBN_SETFOCUS
        Move D$FocusedChild D$CurrentListBox
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
   Call 'USER32.SendDlgItemMessageA' D$H.UnusedCodeAndDataDialog, UnUsedSymbolsEditBox,
                                        &WM_GETTEXTLENGTH 0 0
   inc eax
   Mov D$LenOfSearchedString eax



   Call 'USER32.SendDlgItemMessageA' D$H.UnusedCodeAndDataDialog, UnUsedSymbolsEditBox,
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
    Push D$DownSearch, D$WholeWordSearch, D$SkipDashLines
        Mov B$DownSearch &TRUE, B$WholeWordSearch &FALSE, B$SkipDashLines &TRUE
        Mov D$NextSearchPos 0
        On D$FL.RealSourceRestored = &FALSE Call RestoreRealSource
            Move D$STRUCT.EditData@CurrentWritingPos D$CodeSource
            Call StringSearch
            Push D$CodeSourceA D$CodeSourceB
                Mov esi D$STRUCT.EditData@CurrentWritingPos | sub esi 2 | Call InternalRightClick
            Pop D$CodeSourceB D$CodeSourceA
            Mov B$FinfOrReplace &FALSE
        Call SetPartialEditionFromPos
    Pop D$SkipDashLines, D$WholeWordSearch, D$DownSearch
    On B$StringFound = &FALSE, Call 'USER32.SetForegroundWindow' D$H.UnusedCodeAndDataDialog


ret



____________________________________________________________________________________________

[FreeSearchActive: &FALSE]
SearchForUnusedSymbol:
    Mov eax TextToRetrive, ecx 0

    While b$eax <> 0
        inc ecx | inc eax
    End_While
    inc ecx

    Push eax ecx
        Call 'USER32.SendDlgItemMessageA' D$H.UnusedCodeAndDataDialog, FreeSearchCheckBox,
                                     &BM_GETCHECK, 0, 0

        Mov D$FreeSearchActive eax
        cmp eax &TRUE
    Pop ecx eax | je L0>

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



DesideSearchForUnusedSymbols:
  If D$FocusedChild = UnUsedSymbolsEditBox
      Call SearchForTheSymbolInUnUsedSymbolsEditBox
  Else
     Mov ax &LBN_DBLCLK | shl eax 16 | Mov ax w$FocusedChild | Call DataAndCodeLabelListBoxNotification
  End_If

  On B$StringFound = &FALSE, Call 'USER32.SetForegroundWindow' D$H.UnusedCodeAndDataDialog
ret
____________________________________________________________________________________________

[NoteForWorkerBee:"You can press F5 to compile, without losing this list" EOS]

; Tag Dialog 4


[OldUnusedSymbolDialogSubClassProc1: D$ ?
 OldUnusedSymbolDialogSubClassProc2: D$ ?
 OldUnusedSymbolDialogSubClassProc3: D$ ?
 OldUnusedSymbolDialogSubClassProc4: D$ ?
 OldUnusedSymbolDialogSubClassProc5: D$ ?
 OldUnusedSymbolDialogSubClassProc6: D$ ?]

Proc UnusedSymbolDialogSubClassProc:
Arguments @hwnd, @msg, @wParam, @lParam

    .If D@msg = &WM_KEYDOWN
        If D@Wparam = &VK_F5
            Mov B$RecompileWanted &TRUE
            Mov D$FL.ShowStats &FALSE
            Call SaveUnusedIndex
            Push D$H.UnusedCodeAndDataDialog
                Mov D$H.UnusedCodeAndDataDialog 0
                Mov B$UnusedSymbolsWanted &TRUE
            Pop eax
            Call 'USER32.EndDialog' eax, 0

EndP

        End_If
    .End_If


   @DefaultProcessing:
   Mov eax D@hwnd
   .If eax = D$H.CodeListBox
       Call D$OldUnusedSymbolDialogSubClassProc1 D@hwnd, D@msg, D@wParam, D@lParam
   .Else_if eax = D$H.DataListBox
       Call D$OldUnusedSymbolDialogSubClassProc2 D@hwnd, D@msg, D@wParam, D@lParam
   .Else_if eax = D$H.UnUsedSymbolsEditBox
       Call D$OldUnusedSymbolDialogSubClassProc3 D@hwnd, D@msg, D@wParam, D@lParam
   .Else_if eax = D$H.FindDeclaration
       Call D$OldUnusedSymbolDialogSubClassProc4 D@hwnd, D@msg, D@wParam, D@lParam
   .Else_if eax = D$H.UnusedSymbolHelpbutton
       Call D$OldUnusedSymbolDialogSubClassProc5 D@hwnd, D@msg, D@wParam, D@lParam
   .Else
       Call D$OldUnusedSymbolDialogSubClassProc6 D@hwnd, D@msg, D@wParam, D@lParam
   .End_If
EndP

Proc InstallSubClassCallbacks:
Argument @hwnd
            Call 'USER32.GetDlgItem' D@hwnd CodeListBox
            If eax <> 0
                Mov D$H.CodeListBox eax
                Call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                Mov D$OldUnusedSymbolDialogSubClassProc1 eax
            End_If

            Call 'USER32.GetDlgItem' D@hwnd DataListbox
            If eax <> 0
                Mov D$H.DataListBox eax
                Call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                Mov D$OldUnusedSymbolDialogSubClassProc2 eax
            End_If

            Call 'USER32.GetDlgItem' D@hwnd UnUsedSymbolsEditBox
            If eax <> 0
                Mov D$H.UnUsedSymbolsEditBox eax
                Call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                Mov D$OldUnusedSymbolDialogSubClassProc3 eax
            End_If

            Call 'USER32.GetDlgItem' D@hwnd FindDeclaration
            If eax <> 0
                Mov D$H.FindDeclaration eax
                Call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                Mov D$OldUnusedSymbolDialogSubClassProc4 eax
            End_If
            Mov D$H.FindDeclaration eax

            Call 'USER32.GetDlgItem' D@hwnd UnusedSymbolHelpbutton
            If eax <> 0
                Mov D$H.UnusedSymbolHelpbutton eax
                Call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                Mov D$OldUnusedSymbolDialogSubClassProc5 eax
            End_If

            Call 'USER32.GetDlgItem' D@hwnd FreeSearchCheckBox
            If eax <> 0
                Call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                Mov D$OldUnusedSymbolDialogSubClassProc6 eax
            End_If
EndP

Proc UnusedCodeAndDataDialogCallBack:
    Arguments @hwnd, @msg, @wParam, @lParam

       pushad

        Mov eax &FALSE
        ..If D@msg = &WM_COMMAND
            If D@wParam = &IDHELP
                Call Help, B_U_AsmName, UnusedSymbolsHelp, ContextHlpMessage
            End_If

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
                    Mov D$FL.UnusedSymbolsDialogWanted &FALSE
                    Mov D$H.UnusedCodeAndDataDialog 0
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
            Mov D$H.UnusedCodeAndDataDialog eax
            Call SetUnusedDialogPos D@hwnd
            Mov ecx D$NumberOfUnusedCodeLabels | jecxz L9>
            Mov eax D$CodeLabelNameList

L0:         Push eax ecx
                Push eax
                    While B$eax <> 0
                        cmp B$eax '@' | je L1>
                        inc eax
                    End_While
                Pop eax
                Push eax
                    Call 'USER32.SendDlgItemMessageA' D$H.UnusedCodeAndDataDialog,
                                                      CodeListBox, &LB_ADDSTRING, 0, eax
L1:             Pop eax
            Pop ecx eax
            While B$eax <> 0 | inc eax | End_While
            inc eax | dec ecx | jnz L0<

L9:         Mov ecx D$NumberOfUnusedDataLabels | jecxz L9>
            Mov eax D$DataLabelNameList

L0:         Push eax ecx
                Push eax
                    While B$eax <> 0
                        cmp B$eax '@' | je L1>
                        inc eax
                    End_While
                Pop eax
                Push eax
                    Call 'USER32.SendDlgItemMessageA' D$H.UnusedCodeAndDataDialog,
                                                      DataListbox, &LB_ADDSTRING, 0, eax
L1:             Pop eax
            Pop ecx eax
            While B$eax <> 0 | inc eax | End_While
            inc eax | dec ecx | jnz L0<

L9:         Call 'USER32.SetDlgItemTextA' D$H.UnusedCodeAndDataDialog,
                                          UnUsedSymbolsEditBox, NoteForWorkerBee

            Mov eax D$UnusedCodeIndex | or eax D$UnusedDataIndex
            On eax <> 0 Call RestoreUnusedIndex

        ..Else
            popad | Mov eax &FALSE | ExitP

        ..End_If

        popad | Mov eax &TRUE
EndP
____________________________________________________________________________________________

[UnusedCodeIndex: D$ ?
 UnusedDataIndex: D$ ?]

SaveUnusedIndex:
    Call 'USER32.SendDlgItemMessageA' D$H.UnusedCodeAndDataDialog, CODELISTBOX,
                                      &LB_GETTOPINDEX, 0, 0
    Mov D$UnusedCodeIndex eax

    Call 'USER32.SendDlgItemMessageA' D$H.UnusedCodeAndDataDialog, DATALISTBOX,
                                      &LB_GETTOPINDEX, 0, 0
    Mov D$UnusedDataIndex eax
ret

RestoreUnusedIndex:
    Call 'USER32.SendDlgItemMessageA' D$H.UnusedCodeAndDataDialog, CODELISTBOX,
                                      &LB_SETTOPINDEX, D$UnusedCodeIndex, 0

    Call 'USER32.SendDlgItemMessageA' D$H.UnusedCodeAndDataDialog, DATALISTBOX,
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

        Call 'USER32.GetWindowPlacement' D$H.UnusedCodeAndDataDialog, D@WINDOWPLACEMENT

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

        Call 'USER32.SetWindowPlacement' D$H.UnusedCodeAndDataDialog, D@WINDOWPLACEMENT
EndP

____________________________________________________________________________________________

ReInitUnusedDialog:
    Call SaveUnusedIndex
    Call 'USER32.EndDialog' D$H.UnusedCodeAndDataDialog, 0
    Mov D$H.UnusedCodeAndDataDialog 0
    Mov B$UnusedSymbolsWanted &TRUE
    Mov B$RecompileWanted &TRUE
    Mov D$FL.ShowStats &FALSE
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
