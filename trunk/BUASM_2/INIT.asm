TITLE INIT            ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

INIT:

    ; For the Resurces Editor:
    Call 'KERNEL32.LoadLibraryA' {B$ "RICHED20.DLL" EOS}

    Mov D$H.RichEdit eax

    ; Install exception handler
    Call 'KERNEL32.SetUnhandledExceptionFilter' FinalExceptionHandler

    ; Ensure mono-instance:
    Call 'KERNEL32.CreateMutexA' &NULL,
                                 &TRUE,
                                 STR.A.BUAsmMutexName

    Call 'KERNEL32.GetLastError'

    On eax = &ERROR_ALREADY_EXISTS Mov D$FL.MultiInstance &TRUE

    Call INIT_Instance

    Call WineKey

    Call WhateverConfig

    Mov D$WriteCheckerWanted 0 ; Temporary locked. ('WriteChecker' ---> toDo as a Hook).

    Call INIT_Colors

    Call INIT_MainWindow

    Call AddUserMenu

    Call NewBuildWin32Equates

    If B$IncludesOK = &TRUE
        On D$EquatesName = 'Equa', Call AppendToCurrentDirectory
        Call PrepareStructuresFiles
    End_If

    On D$StringsLanguage <> '.en', Call OpenStringsFile

    Call EnableMenutems | Call EnableHelpMenutems
    Call EnableVisualTutsMenu | Call EnableWizardsMenu
    Call EnableClipMenu

    Call CreateStatusBar
    On B$ToolBarWanted = &TRUE, Call CreateToolBar
    On B$ScrollBarWanted = &TRUE, Call CreateScrollBar
    Call CreateEditWindow

  ; (D$IsMaximizedFlag = &SW_SHOWNORMAL or &SW_MAXIMIZE):
    Call 'USER32.ShowWindow'  D$H.MainWindow, D$IsMaximizedFlag

    Call 'USER32.UpdateWindow' D$H.MainWindow

    Call 'USER32.GetClipCursor' FullRECT

    Call 'USER32.RegisterWindowMessageA' FindString | Mov D$FindStringMessage eax

  ; Rotary table for moving inside text:
    Call SetBackTableMemory | Call InitUndo

    Call CreateFontForDialogEdition | Call LoadFont

    On D$NATION_LOGFONT@lfWeight <> 0, Call LoadNationalFont

    Call SetUndoDirectory
    Call DeleteOldUndoFiles

    Call CheckAllMRUFile | Call SetMRUmenu | On B$LoadMRU = &TRUE, Call LoadLastMRUFile


  ; copying compilable version of icon in case user compiles vithout defining any icon:
    Call StoreIcon

    On B$BlinkingCaretWanted = &TRUE, Call InitBlinkCursor

    Call InitExpressionBuffers

;    Call 'USER32.CreateAcceleratorTableA' ACCELERATORS ACCELNUMBER
;    Mov D$AccelHandle eax


ret
____________________________________________________________________________________________

[H.Instance: D$ ?]

INIT_Instance:

   Call 'KERNEL32.GetModuleHandleA' &NULL

    Mov D$H.Instance eax,
        D$WINDOWCLASS.Main+WINDOWCLASS_hInstance eax,
        D$OSSInstance eax,
        D$OPESInstance eax,
        D$BmOpenInstance eax,
        D$STRUC.MSGBOXPARAMS@hInstance eax

ret
____________________________________________________________________________________________

INIT_Colors:

    On D$H.BackGroundBrush <> 0, Call 'KERNEL32.CloseHandle' D$H.BackGroundBrush
    Call 'GDI32.CreateSolidBrush' D$NormalBackColor
    Mov D$H.BackGroundBrush eax

    On D$H.DialogsBackGroundBrush, <> 0, Call 'KERNEL32.CloseHandle' D$H.DialogsBackGroundBrush
    Call 'GDI32.CreateSolidBrush' D$RVBA.DialogsBackgnd
    Mov D$H.DialogsBackGroundBrush eax

    On D$CaretBrushHandle, <> 0, Call 'KERNEL32.CloseHandle' D$CaretBrushHandle
    Call 'GDI32.CreateSolidBrush' D$ RVBA.Normal
    Mov D$CaretBrushHandle eax

    On D$RedBrushHandle, <> 0, Call 'KERNEL32.CloseHandle' D$RedBrushHandle
    Call 'GDI32.CreateSolidBrush' D$BracketColor
    Mov D$RedBrushHandle eax
ret
____________________________________________________________________________________________

[STR.A.WindowClassMain: B$ "Main" EOS]

[WINDOWCLASS.Main: B$ ? # (10*DWORD)]

[H.MainWindow: ?
 hwndEdit: D$ ?
 FindHandle: D$ ?]

[H.Menu: D$ ?]
[H.BackGroundBrush: D$ ?]

[H.MainIcon: D$ ?]

INIT_MainWindow:

    Call INIT_Cursors

    Call 'User32.LoadIconA' D$H.Instance,
                            1

    Mov D$H.MainIcon eax

    Mov D$WINDOWCLASS.Main+WINDOWCLASS_hIcon eax,
        D$WINDOWCLASS.Main+WINDOWCLASS_style &CS_DBLCLKS+&CS_BYTEALIGNCLIENT+&CS_BYTEALIGNWINDOW+&CS_PARENTDC,
        D$WINDOWCLASS.Main+WINDOWCLASS_lpfnWndProc MainWindowProc,
        D$WINDOWCLASS.Main+WINDOWCLASS_lpszClassName STR.A.WindowClassMain,
        eax D$H.BackGroundBrush D$WINDOWCLASS.Main+WINDOWCLASS_hbrBackground eax

    Call 'USER32.RegisterClassA' WINDOWCLASS.Main

    Call 'User32.LoadMenuA' D$H.Instance,
                            M00_Menu

    Mov D$H.Menu eax

    Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  STR.A.WindowClassMain,
                                  STR.A.AppName,
                                  &WS_POPUP+&WS_CAPTION+&WS_SYSMENU+&WS_MINIMIZEBOX+&WS_MAXIMIZEBOX+&WS_THICKFRAME,
                                  D$WindowX,
                                  D$WindowY,
                                  D$WindowW,
                                  D$WindowH,
                                  &NULL,
                                  D$H.Menu,
                                  D$H.Instance,
                                  &NULL

    Mov D$H.MainWindow eax,
        D$hwndFileOwner eax,
        D$hwndPEFileOwner eax,
        D$PD_hWndOwner eax,
        D$BmhwndFileOwner eax,
        D$STRUC.MSGBOXPARAMS@hwndOwner eax

ret
____________________________________________________________________________________________

[H.CursorARROW: D$ ?
 H.CursorIBEAM: D$ ?
 H.CursorWAIT: D$ ?]

[ActualCursor: ?]

INIT_Cursors:

    Call 'USER32.LoadCursorA' 0,
                              &IDC_ARROW

    Mov D$H.CursorARROW eax

    Call 'USER32.LoadCursorA' 0,
                              &IDC_WAIT

    Mov D$H.CursorWAIT eax

    Call 'USER32.LoadCursorA' 0,
                              &IDC_IBEAM

    Mov D$H.CursorIBEAM eax

ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; EOT


