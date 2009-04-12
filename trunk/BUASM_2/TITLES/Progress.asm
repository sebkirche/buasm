TITLE Progress
 _______________________________________________________________________________________
 _______________________________________________________________________________________

; Progress Bar

[ProgressTitle: B$ 'Done' 0]

[ProgressClassName: 'msctls_progress32' 0 ; ProgressName: 'Compiling...' 0
 PWindowX: 10 PWindowY: 5 PWindowW: 300 PWindowH: 10
 PBarWindow: PBarWindowX: 0   PBarWindowY: 0   PBarWindowW: 340  PBarWindowH: 45
 ProgressInst: 0   hwndForBar: 0]

 Call 'Comctl32.InitCommonControls'

[Writing:       '[Writing PE__<<<<<__'
 Fixing:        'Resolving__<<<<<__'
 BuildingHeader:'PE headers__<<<<<__'
 Encoding:      'Encoding__<<<<<__'
 BuildingData:  'Data job__<<<<<__'
 BuildingRsrc:  'Resources__<<<<<__'
 BuildingImport:'Import Section__<<<<<__'
 Replacing:     'Replacing__<<<<<__'
 Storing:       'Storing Mac/Equ__<<<<<__'
 Cleaning:      'Cleaning]', 0]

[PROGRESS_BAR_WIDTH 340    PROGRESS_BAR_HIGHT 45]


InitProgressBar:
  ; Center the bar:
    Mov eax D$EditWindowW | sub eax PROGRESS_BAR_WIDTH
    shr eax 1 | Mov D$PBarWindowX eax

    Mov ebx D$EditWindowH | sub ebx PROGRESS_BAR_HIGHT
    shr ebx 1 | Mov D$PBarWindowY eax

    Mov D$PBarWindowW PROGRESS_BAR_WIDTH, D$PBarWindowH, PROGRESS_BAR_HIGHT

    Call 'USER32.ClientToScreen' D$EditWindowHandle, PBarWindow

    Call 'USER32.CreateWindowExA' 0, ClassName, Cleaning,
                                  &WS_OVERLAPPED__&WS_CAPTION__&WS_THICKFRAME_&WS_VISIBLE,
                                  D$PBarWindowX, D$PBarWindowY, D$PBarWindowW, D$PBarWindowH,
                                  0, 0, D$hInstance, 0
    Mov D$hwndForBar eax

    Call 'USER32.ShowWindow' D$hwndForBar, &SW_SHOW ;&SW_SHOWNORMAL

    Call 'USER32.UpdateWindow' D$hwndForBar
_____________________________

    Call 'USER32.CreateWindowExA' 0, ProgressClassName, 0, &WS_CHILD__&WS_VISIBLE,
                                  D$PWindowX, D$PWindowY, D$PWindowW, D$PWindowH,
                                  D$hwndForBar, 0, D$hInstance, 0
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
    On B$WeAreUnfolding = &TRUE, ret

    pushad
        Call 'USER32.SendMessageA' D$ProgressInst &PBM_STEPIT 0 0
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
    push eax
        Mov eax '    ' | stosd | stosd
    pop eax

L0: Mov edx 0
    div ebx
    add dl '0' | Mov B$edi dl | dec edi | cmp eax 0 | ja L0<
ret


; Tag Dialog 1

[StatsHandle: ?   TotalTime: ?   UnusedSymbolsWanted: ?]

Proc Statistics:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ..If D@msg = &WM_COMMAND
        If D@wParam = &IDCANCEL
            Call 'USER32.EndDialog' D@hwnd, 0
        Else_If D@wParam = 3
            ;Call DisplayUnusedSymbolsDialog D@hwnd
            Call 'USER32.EndDialog' D@hwnd, 0
            Mov D$UnusedSymbolsDialogWanted &TRUE
        End_If

    ..Else_If D@msg = &WM_INITDIALOG
        move D$StatsHandle D@hwnd

        Call 'USER32.SetClassLongA' D@hwnd, &GCL_HICON, D$wc_hIcon

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
    Call 'User32.SendMessageA' D$ProgressInst &PBM_SETPOS 0 0
    Call 'User32.DestroyWindow' D$hwndForBar
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

[AboutMessage: B$ "
         RosAsm, the Specific Assembler

            is free, open source, GPL.

Main Author is René Tournois < betov@free.fr>        

            Download last version at:

        http://betov.free.fr/RosAsm.html

", 0

AboutTitle:  ' About:', 0]


AboutBox:
   Call 'USER32.MessageBoxA' D$H.MainWindow,                ; handle
                            AboutMessage ,         ; Message
                            AboutTitle,            ; Message-Window-Title
                            &MB_ICONINFORMATION__&MB_SYSTEMMODAL         ; Style (0 to 4) 0 > 'OK'
ret


About_ToolBar:
    Call 'USER32.MessageBoxA' D$H.MainWindow, {"
    
    Double-Click on the ToolBar for customization
    
    You can move the ToolBar Buttons by [Alt]+Drag             
    ", 0}, {'ToolBar Info:', 0}, &MB_ICONINFORMATION__&MB_SYSTEMMODAL
ret


____________________________________________________________________________________________
____________________________________________________________________________________________




