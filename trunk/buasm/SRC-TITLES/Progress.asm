TITLE Progress        ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

; Progress Bar

[ProgressTitle: B$ 'Done' EOS]

[ProgressClassName: B$ 'msctls_progress32' EOS] ; ProgressName: 'Compiling...' EOS
[PWindowX: 10 PWindowY: 5 PWindowW: 300 PWindowH: 10
 PBarWindow: PBarWindowX: 0   PBarWindowY: 0   PBarWindowW: 340  PBarWindowH: 45
 ProgressInst: 0   H.ForBar: 0]

 Call 'Comctl32.InitCommonControls'

[Writing: B$ ' Writing PE' EOS]
[Fixing: B$ ' Resolving' EOS]
[BuildingHeader: B$ ' PE headers' EOS]
[Encoding: B$ ' Encoding' EOS]
[BuildingData: B$ ' Data job' EOS]
[BuildingRsrc: B$ ' Resources' EOS]
[BuildingImport: B$ ' Import Section' EOS]
[Replacing: B$ ' Replacing' EOS]
[Storing: B$ ' Storing Macros/Equates' EOS]
[Cleaning: B$ ' Cleaning' EOS]

[PROGRESS_BAR_WIDTH 340    PROGRESS_BAR_HIGHT 45]

InitProgressBar:
  ; Center the bar:
    Mov eax D$STRUC.RECT.EditWindow+RIGHT | sub eax PROGRESS_BAR_WIDTH
    shr eax 1 | Mov D$PBarWindowX eax

    Mov ebx D$STRUC.RECT.EditWindow+BOTTOM | sub ebx PROGRESS_BAR_HIGHT
    shr ebx 1 | Mov D$PBarWindowY eax

    Mov D$PBarWindowW PROGRESS_BAR_WIDTH, D$PBarWindowH, PROGRESS_BAR_HIGHT

    Call 'USER32.ClientToScreen' D$H.EditWindow, PBarWindow

    Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  STR.A.BUAWindowClass,
                                  Cleaning,
                                  &WS_OVERLAPPED+&WS_CAPTION+&WS_THICKFRAME+&WS_VISIBLE,
                                  D$PBarWindowX,
                                  D$PBarWindowY,
                                  D$PBarWindowW,
                                  D$PBarWindowH,
                                  0,
                                  &NULL,
                                  D$H.Instance,
                                  0
    Mov D$H.ForBar eax

 ;   Call 'USER32.ShowWindow' D$hwndForBar, &SW_SHOW ;&SW_SHOWNORMAL

 ;   Call 'USER32.UpdateWindow' D$hwndForBar
_____________________________

    Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  ProgressClassName,
                                  &NULL,
                                  &WS_CHILD+&WS_VISIBLE,
                                  D$PWindowX,
                                  D$PWindowY,
                                  D$PWindowW,
                                  D$PWindowH,
                                  D$H.ForBar,
                                  &NULL,
                                  D$H.Instance,
                                  0
    Mov D$ProgressInst eax
ret


Proc InitProgressSteps:
    Arguments @Range, @Step
      ; Low Word is 'Min', High Word is 'Max':
        shl D@Range 16
        Call 'USER32.SendMessageA'  D$ProgressInst, &PBM_SETRANGE, 0, D@Range
        Call 'USER32.SendMessageA'  D$ProgressInst, &PBM_SETSTEP, D@Step, 0
EndP


BarProgress:

    On D$FL.WeAreUnfolding = &TRUE ret

    pushad

        Call 'USER32.SendMessageA' D$ProgressInst,
                                   &PBM_STEPIT,
                                   &NULL,
                                   &NULL

    popad
ret


[CompileInfos: ]
;;
"



      code instructions:                 "
 InstNumber: "                                               

              Data Labels:                 "
 LabsNumber: "                              
              Code Labels:                  "
 CodeLabelsNumber: "


             
             Parsing Time:                "
 ParsingTime: "           ms
             Compile time:                 "
 CompileTime: "           ms
            Creating time:                  "
 CreatingTime:"           ms

                Total time:                  "
 TotalTime:   "           ms


              Source size:                "
 SourceSize:"             octets
                    PE size:                "
     PEsize:"             octets
                Total Size:                "
  TotalSize:"             octets



 "

 0]
;;

StatDecimalWritting:

    Push eax

        Mov eax '    ' | stosd | stosd

    Pop eax

L0: Mov edx 0

    div ebx

    add dl '0' | Mov B$edi dl | dec edi | cmp eax 0 | ja L0<

ret


; Tag Dialog 1

[TotalTime: D$ ?
 UnusedSymbolsWanted: D$ ?]

Proc Statistics:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ..If D@msg = &WM_COMMAND
        If D@wParam = &IDCANCEL
            Call 'USER32.EndDialog' D@hwnd, 0
        Else_If D@wParam = 3
            ;Call DisplayUnusedSymbolsDialog D@hwnd
            Call 'USER32.EndDialog' D@hwnd, 0
            Mov D$FL.UnusedSymbolsDialogWanted &TRUE
        End_If

    ..Else_If D@msg = &WM_INITDIALOG

        Call 'USER32.SetClassLongA' D@hwnd, &GCL_HICON, D$STRUC.WINDOWCLASS@hIcon

      ; Number of Instructions:
        Call 'USER32.SetDlgItemInt' D@hwnd, 110, D$InstructionsCounter, 0
      ; Number of Code and Data Labels:
        Call 'USER32.SetDlgItemInt' D@hwnd, 111, D$DataLabelsCounter, 0
        Call 'USER32.SetDlgItemInt' D@hwnd, 112, D$CodeLabelsCounter, 0

      ; Parsing Time:
        Mov eax D$Time2 | sub eax D$Time1 | Mov D$TotalTime eax
        Call 'USER32.SetDlgItemInt' D@hwnd, 120, eax, 0
      ; Compile Time:
        Mov eax D$Time3 | sub eax D$Time2 | add D$TotalTime eax
        Call 'USER32.SetDlgItemInt' D@hwnd, 121, eax, 0
      ; Creation Time:
        Mov eax D$Time4 | sub eax D$Time3 | add D$TotalTime eax
        Call 'USER32.SetDlgItemInt' D@hwnd, 122, eax, 0
      ; Total Time:
        Call 'USER32.SetDlgItemInt' D@hwnd, 129, D$TotalTime, 0

      ; Source Size:
        Call 'USER32.SetDlgItemInt' D@hwnd, 130, D$SourceLen, 0
      ; Code Size:
        Call 'USER32.SetDlgItemInt' D@hwnd, 131, D$LenOfCode, 0
      ; Total Size:
        Mov eax D$LenOfCode | add eax D$SourceLen
        Call 'USER32.SetDlgItemInt' D@hwnd, 139, eax, 0

        Call WritePE | Call RecordMRU | On D$BookMarks > 0, Call SaveBookMarks

        If B$UnusedSymbolsWanted = &TRUE
            Call DisplayUnusedSymbolsDialog D@hwnd
        Else
            Mov D$UnusedCodeIndex 0, D$UnusedDataIndex 0
        End_If

    ..Else
        popad | Mov eax &FALSE | ExitP

    ..End_If

    popad | Mov eax &TRUE
EndP


CloseProgressBar:

    Call 'USER32.SendMessageA' D$ProgressInst,
                               &PBM_SETPOS,
                               0,
                               &NULL

    Call 'USER32.DestroyWindow' D$H.ForBar

ret

____________________________________________________________________________________________
____________________________________________________________________________________________

AboutBox:

    Call 'USER32.MessageBoxA' D$H.MainWindow,
                              {B$ "BUA, the Bottom-Up Assembler

Download last version at:

http://code.google.com/p/rosasm64/downloads/list" EOS},
                             {B$ ' About:' EOS},
                             &MB_ICONINFORMATION+&MB_SYSTEMMODAL

ret
____________________________________________________________________________________________

About_ToolBar:

    Call 'USER32.MessageBoxA' D$H.MainWindow,
                              {B$ "Double-Click on the ToolBar for customization

You can Move the ToolBar Buttons by [Alt]+Drag" EOS},
                              {B$ 'ToolBar Info:' EOS},
                              &MB_ICONINFORMATION__&MB_SYSTEMMODAL

ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
