TITLE INIT            ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

INIT:

    ; For the Resurces Editor:
    Call 'KERNEL32.LoadLibraryA' {B$ 'riched20.dll' EOS}

    Mov D$H.RichEdit eax

    ; Install exception handler
    Call 'KERNEL32.SetUnhandledExceptionFilter' FinalExceptionHandler

    ; Ensure mono-instance:
    Call 'KERNEL32.CreateMutexA' &NULL,
                                 &TRUE,
                                 {B$ 'BUA IsRuning' EOS}

    Call 'KERNEL32.GetLastError'

    On eax = &ERROR_ALREADY_EXISTS Mov D$FL.MultiInstance &TRUE

    Call INIT_WineKey

    Call INIT_WhateverConfig

    Mov D$WriteCheckerWanted 0 ; Temporary locked. ('WriteChecker' ---> toDo as a Hook).

    Call INIT_MainWindow
ret
____________________________________________________________________________________________

INIT_WineKey:

    Call 'ADVAPI32.RegCreateKeyExA' &HKEY_LOCAL_MACHINE,
                                    {B$ 'Software\Windows NT\CurrentVersion\AeDebug' EOS},
                                    0,
                                    0,
                                    0,
                                    &KEY_READ+&KEY_WRITE+&KEY_QUERY_VALUE,
                                    0,
                                    hRegKey,
                                    Result

    Call 'ADVAPI32.RegSetValueExA' D$hRegKey,
                                   {B$ 'Debugger' EOS},
                                   0,
                                   &REG_SZ,
                                   {B$ 'winedbg %ld %ld' EOS},
                                   15

ret
____________________________________________________________________________________________

[ConfigMessage: B$ "You can choose to save your Configuration either as a Registry Key, or under the form of a File: config.bin

You should choose the first option for regular work on your home computer.

The second option may be of interrest, for example, when working from an USB memory stick.

If you choose the second option, when doing a Copy of RosAsm to another Folder, you will have to also copy your 'config.bin' aside" EOS]

[UserConfig: D$ ?]

[REGISTRY 1
 CONFIGFILE 2]
; Regedit.exe: HKEY_CURRENT_USER / Software / RosAsm

INIT_WhateverConfig:

    Call SetMainConfigFilePath

    Call 'KERNEL32.FindFirstFileA' ConfigFilePath,
                                   STRUC.FindFile
    Push eax

        Call 'KERNEL32.FindClose' eax

    Pop eax

    ..If eax = &INVALID_HANDLE_VALUE

        Call OpenRegistry

        .If D$Result = &REG_CREATED_NEW_KEY

            ; Tag Dialog 3
            Call 'USER32.DialogBoxParamA' D$H.Instance,
                                          3,
                                          &NULL,
                                          ConfigProc,
                                          &NULL

            If D$UserConfig = 0FF

                Call CloseRegistry

                Call 'ADVAPI32.RegDeleteKeyA' &HKEY_CURRENT_USER,
                                              BUAsmKey

                Call 'KERNEL32.ExitProcess' 0

            Else_If D$UserConfig = REGISTRY

                ; "Read" assumes create when none:
                Call ReadRegistry

                Mov D$UserConfig REGISTRY

            Else

                Call CloseRegistry

                Call 'ADVAPI32.RegDeleteKeyA' &HKEY_CURRENT_USER,
                                              BUAsmKey

                Mov D$UserConfig CONFIGFILE

                Call AutoInit

                Call WriteConfigFile

            End_If

        .Else

            Call ReadRegistry

            Mov D$UserConfig REGISTRY

        .End_If

    ..Else

        Call ReadConfigFile

        Call CheckPaths

        Mov D$UserConfig CONFIGFILE

    ..End_If
ret
____________________________________________________________________________________________

INIT_MainWindow:

    Call 'KERNEL32.GetModuleHandleA' &NULL

    Mov D$H.Instance eax,
        D$STRUC.WINDOWCLASS@hInstance eax,
        D$OSSInstance eax,
        D$STRUC.OPENFILENAME@hInstance eax,
        D$BmOpenInstance eax

    Call 'USER32.LoadIconA' eax,
                            1

    Mov D$STRUC.WINDOWCLASS@hIcon eax

    Call INIT_Cursors

    Call 'USER32.RegisterClassA' STRUC.WINDOWCLASS

    Call 'USER32.LoadMenuA' D$H.Instance,
                            M00_Menu

    Mov D$H.MenuMain eax

    Call AddUserMenu

    Call NewBuildWin32Equates

    test D$FL.Includes &TRUE ZERO S1>

        On D$EquatesName = 'Equa' Call AppendToCurrentDirectory

        Call PrepareStructuresFiles

S1: Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  STR.A.BUAWindowClass,
                                  STR.A.ApplicationName,
                                  &WS_POPUP+&WS_CAPTION+&WS_SYSMENU+&WS_MINIMIZEBOX+&WS_MAXIMIZEBOX+&WS_THICKFRAME,
                                  D$WindowX,
                                  D$WindowY,
                                  D$WindowW,
                                  D$WindowH,
                                  &NULL,
                                  D$H.MenuMain,
                                  D$H.Instance,
                                  &NULL

    Mov D$H.MainWindow eax,
        D$hwndFileOwner eax,
        D$STRUC.OPENFILENAME@hwndOwner eax,
        D$PRINTDLG@PD_hWndOwner eax,
        D$BmhwndFileOwner eax

    Call 'SHELL32.DragAcceptFiles' eax,
                                   &TRUE

    Call INIT_BackGroundColors

 ret
____________________________________________________________________________________________

INIT_Cursors:

    Call 'USER32.LoadCursorA' 0,
                              &IDC_ARROW

    Mov D$H.CursorARROW eax

    Call 'USER32.LoadCursorA' 0,
                              &IDC_IBEAM

    Mov D$STRUC.WINDOWCLASS@hCursor eax

    Mov D$H.CurrentCursor eax

    Call 'USER32.LoadCursorA' 0,
                              &IDC_WAIT

    Mov D$H.CursorWAIT eax

ret
____________________________________________________________________________________________

INIT_BackGroundColors:

    Call 'GDI32.DeleteObject' D$H.BackGroundBrush | Call 'GDI32.CreateSolidBrush' D$ARVB.BackColor

    Mov D$H.BackGroundBrush eax

    Call 'USER32.SetClassLongA' D$H.Mainwindow,
                                &GCL_HBRBACKGROUND,
                                eax

    Call 'GDI32.DeleteObject' D$H.DialogsBackGroundBrush

    Call 'GDI32.CreateSolidBrush' D$ARVB.DialogsBackColor

    Mov D$H.DialogsBackGroundBrush eax

    Call 'GDI32.DeleteObject' D$H.CaretBrush | Call 'GDI32.CreateSolidBrush' D$StatementColor

    Mov D$H.CaretBrush eax

    Call 'GDI32.DeleteObject' D$H.RedBrush | Call 'GDI32.CreateSolidBrush' D$BracketColor

    Mov D$H.RedBrush eax

ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; EOT
