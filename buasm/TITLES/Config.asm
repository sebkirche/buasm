TITLE Config          ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________
 _______________________________________________________________________________________
 _______________________________________________________________________________________

; Configuration TAB:

[StaticClassName: B$ 'STATIC' EOS
 ButtonClassName: B$ 'BUTTON' EOS]

[H.ConfigTab: D$ ?]

[FirstTab: B$ 'Tree View' EOS];       FirstTabLen: D$ len ; !!! B$ -> D$ non aligned
[SecondTab: B$ 'Sources Editor' EOS];  SecondTabLen: D$ len
[ThirdTab: B$ 'Bad Habits' EOS];       ThirdTabLen: D$ len
[FourthTab: B$ 'Colors' EOS];          FourthTabLen: D$ len
[FifthTab: B$ 'Help Files' EOS];        FifthTabLen: D$ len
[SixthTab: B$ 'Companion Files' EOS];  SixthTabLen: D$ len
[SeventhTab: B$ 'User Menu' EOS];      SeventhTabLen: D$ len
[HeigthTab: B$ 'Pos' EOS];               HeigthTabLen: D$ len
[NinethTab: B$ 'Lang' EOS];              NinethTabLen: D$ len
[DBPMenuTab: B$ 'DBP' EOS];              DBPMenuLen: D$ len
[APItab: B$ 'API' EOS];             APItabLen: D$ len]

[CalcName: B$ 'Calc.exe' EOS @tail: 0 # &MAX_PATH]                       ; If same dir
[EquatesName: B$ 'Equates.equ' EOS @tail: 0 # &MAX_PATH]
[F2Name: D$ ? # &MAX_PATH]
[CalcLinkName: B$ 'C:\Program Files\BaseCalc\BASECALC.EXE' EOS  OnCalc: 0] ; If Link to...

;[Asm32TutName: 'Asm32Help.exe' EOS @tail: 0 # &MAX_PATH]
[Win32HlpName: B$ 'Win32.hlp' EOS @tail: 0 # &MAX_PATH]
[MmediaHlpName: B$ 'Mmedia.hlp' EOS @tail: 0 # &MAX_PATH]
[OpenGlHlpName: B$ 'OpenGl.hlp' EOS @tail: 0 # &MAX_PATH]
;[OpcodeHlpName: B$ 'x86eas.hlp' EOS @tail: 0 # &MAX_PATH]
;[WinDataFileName: B$ 'WindowsData.inc' EOS @tail: 0 # &MAX_PATH]
[DxHlpName:    'directx.chm' EOS @tail: 0 # &MAX_PATH]
[WinsockHlpName: B$ 'Winsock.hlp' EOS @tail: 0 # &MAX_PATH]
[SDLRefName: B$ 'SDLRef.chm' EOS @tail: 0 # &MAX_PATH]
[sqliteName: B$ 'sqlite.hlp' EOS @tail: 0 # &MAX_PATH]
[DevILName: B$ 'DevIL.html' EOS @tail: 0 # &MAX_PATH]
[ClipName: B$ 'Clip.txt' EOS @tail: 0 # &MAX_PATH]
[B_U_AsmName: B$ 'B_U_Asm.exe' EOS @tail: 0 # &MAX_PATH]

[EntryPointLabel: B$ 'MAIN' EOS 0 0 0 0 0 0 0 0 0
 EntryPointLabelLen: len]       ; upper case needed
;;
  Take care, that since the implementation of the "Tag Main NewName" Feature, this 
  'EntryPointLabelLen' can no more be used as such, and must be re-computed:
;;
[CallBackName: B$ 'MainWindowProc'
 CallBackNameLen: len]

[TC_ITEM:
 @TC_imask: &TCIF_TEXT+&TCIF_IMAGE
 @TC_lpReserved1: 0
 @TC_lpReserved2: 0
 TC_pszText: 0
 @TC_cchTextMax: 0
 @TC_iImage: 0-1
 @TC_lParam: 0]

[ITB1: B$ "

Tree view is build by analyze of main labels and of calls to these main labels® If you validate [List orphans], the list will be added all 'non-called' main labels.

If you validate [First Call only],  and some label is called many times, it will be listed at first Call occurence only." EOS ]

 ________________________________________________________________________

[H.ConfigTabbedDialog: D$ ?
 ConfigTabIndex: D$ ?
 CtrlYflag: D$ ?
 FL.SecurityWanted: D$ ?]

[TabIs: D$ 4
 FL.SoundsWanted: D$ &TRUE]
____________________________________________________________________________________________
____________________________________________________________________________________________

; Providing the required Path for "BUAsm Files" (Equates.equ and friends).

[FolderPath: B$ ? # &MAX_PATH]
[PathInfo: B$ 'Choose a Path' EOS]
[PathTitle: B$ 'For BUAsm Files' EOS]

[STRUC.BROWSEINFOA:
 @hwndOwner: D$ 0
 @pidlRoot: D$ 0
 @pszDisplayName: D$ 0
 @lpszTitle: D$ 0
 @ulFlags: D$ &BIF_RETURNONLYFSDIRS+&BIF_DONTGOBELOWDOMAIN+&BIF_USENEWUI
 @lpfn: D$ cbBrowse
 @lParam: D$ PathInfo
 @iImage: D$ 0]

[BrowseForFolderAborted: D$ ?]

Proc BrowseForFolder:

    Arguments @hParent,
              @PathTitle

    Mov D$FolderPath 0

    Move D$STRUC.BROWSEINFOA@hwndOwner D@hParent,
         D$STRUC.BROWSEINFOA@lpszTitle D@PathTitle

    Call 'SHELL32.SHBrowseForFolder' STRUC.BROWSEINFOA

    If eax <> &FALSE

        Push eax

            Call 'SHELL32.SHGetPathFromIDList' eax,
                                               FolderPath
        Pop eax

        Mov B$BrowseForFolderAborted &FALSE

    Else

        Mov B$BrowseForFolderAborted &TRUE

    End_If

    Call 'OLE32.CoTaskMemFree' eax

EndP


Proc cbBrowse:

    Arguments @hWin,
              @uMsg,
              @lParam,
              @lpData

    Call 'USER32.SetWindowTextA' D@hWin,
                                 D@lpData

EndP


; User has selected a Path for BUsmFiles (Equates.equ *must* be there).

[MustHaveEquatesPath: B$ "
You have not provided a Path to Equates.equ.

BUAsm is going to shut down, because it can't do anything       
whithout the OS Equates List.

" EOS]


[BadEquatesPath: B$ "
You have provided a Path for 'Equates.equ',
but this File is not there.

           Try again?


" EOS]


VerifyEquatesPath:
    ..If D$FolderPath = 0
        If D$FL.Includes = &FALSE
L1:         Call 'USER32.MessageBoxA' D$H.MainWindow, MustHaveEquatesPath, Argh, &MB_SYSTEMMODAL
            Call 'KERNEL32.ExitProcess' 0
        End_If

        Mov eax 0-1

    ..Else
        Mov eax FolderPath
        While B$eax <> 0 | inc eax | End_While
        Mov D$eax '\Equ', D$eax+4 'ates', D$eax+8 '.equ', B$eax+12 0
        Mov eax FolderPath

        Call 'KERNEL32.FindFirstFileA' FolderPath, STRUC.FindFile

        .If eax = &INVALID_HANDLE_VALUE
            Call 'USER32.MessageBoxA' D$H.MainWindow, BadEquatesPath, Argh,
                                      &MB_YESNO+&MB_SYSTEMMODAL+&MB_ICONQUESTION
            If eax = &IDNO
                On D$FL.Includes = &FALSE, jmp L1<<
                Mov eax 0-1

            Else
                Mov eax 0

            End_If

        .Else
            Call 'KERNEL32.FindClose' eax

            Mov eax FolderPath

        .End_If

    ..End_If
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; Tag Dialog 3000 >>> Main
; Tag Dialog 4000 >>> ListView
; Tag Dialog 4100 >>> Sources Editor
; Tag Dialog 4200 >>> Bad Habit
; Tag Dialog 4300 >>> Colors
; Tag Dialog 4400 >>> Help Files
; Tag Dialog 4500 >>> Companion Files
; Tag Dialog 4600 >>> User Menu
; Tag Dialog 4700 >>> Pos
; Tag Dialog 4800 >>> National Language
; Tag Dialog 4900 >>> Definition of the BP Menu

Configuration:

    If D$H.ConfigDialog = 0

        Call 'USER32.DialogBoxParamA' D$H.Instance, 3000, &NULL, ConfigTabProc, &NULL

    Else

        Call Beep

    End_If

ret


InitConfigDialogTab:
    Call 'USER32.GetDlgItem' D$H.ConfigDialog, 100 | Mov D$H.ConfigTab eax

    Mov D$TC_pszText FirstTab ;| Move D$TC_cchTextMax, D$FirstTabLen
    Call 'USER32.SendMessageA' D$H.ConfigTab, &TCM_INSERTITEM, 0, TC_ITEM

    Move D$TC_pszText SecondTab ;| Move D$TC_cchTextMax D$SecondTabLen
    Call 'USER32.SendMessageA' D$H.ConfigTab, &TCM_INSERTITEM, 1, TC_ITEM

    Move D$TC_pszText ThirdTab ;| Move D$TC_cchTextMax, D$ThirdTabLen
    Call 'USER32.SendMessageA' D$H.ConfigTab, &TCM_INSERTITEM, 2, TC_ITEM

    Move D$TC_pszText FourthTab ;| Move D$TC_cchTextMax D$FourthTabLen
    Call 'USER32.SendMessageA' D$H.ConfigTab, &TCM_INSERTITEM, 3, TC_ITEM

    Move D$TC_pszText FifthTab ;| Move D$TC_cchTextMax D$FifthTabLen
    Call 'USER32.SendMessageA' D$H.ConfigTab, &TCM_INSERTITEM, 4, TC_ITEM

    Move D$TC_pszText SixthTab ;| Move D$TC_cchTextMax D$SixthTabLen
    Call 'USER32.SendMessageA' D$H.ConfigTab, &TCM_INSERTITEM, 5, TC_ITEM

    Move D$TC_pszText SeventhTab ;| Move D$TC_cchTextMax, D$SeventhTabLen
    Call 'USER32.SendMessageA' D$H.ConfigTab, &TCM_INSERTITEM, 6, TC_ITEM

    Move D$TC_pszText HeigthTab ;| Move D$TC_cchTextMax D$HeigthTabLen
    Call 'USER32.SendMessageA' D$H.ConfigTab, &TCM_INSERTITEM, 7, TC_ITEM

    Move D$TC_pszText NinethTab ;| Move D$TC_cchTextMax D$NinethTabLen
    Call 'USER32.SendMessageA' D$H.ConfigTab, &TCM_INSERTITEM, 8, TC_ITEM

    Move D$TC_pszText DBPMenuTab ;| Move D$TC_cchTextMax D$DBPMenuLen
    Call 'USER32.SendMessageA' D$H.ConfigTab, &TCM_INSERTITEM, 9, TC_ITEM

    Move D$TC_pszText APItab ;| Move D$TC_cchTextMax D$APItabLen
    Call 'USER32.SendMessageA' D$H.ConfigTab, &TCM_INSERTITEM, 10, TC_ITEM
ret

____________________________________________________________________________________________

; The Main Proc for Configuration:

[H.ConfigDialog: D$ ?]

; Tag Dialog 4100 [Text Editor]

Proc ConfigTabProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_INITDIALOG
        .If D$H.ConfigDialog = 0
            Move D$H.ConfigDialog D@hwnd
            Call InitConfigDialogTab
            Call 'USER32.CreateDialogParamA' D$H.Instance, 4000, D$H.ConfigTab,
                                             ConfigTabProc, &NULL
            Mov D$H.ConfigTabbedDialog eax, D$ConfigTabIndex 0
            Call InittabTreeView
            Call 'USER32.SetClassLongA' D$H.ConfigDialog &GCL_HICON D$STRUC.WINDOWCLASS@hIcon
        .End_If
        jmp L8>>

    ...Else_If D@msg = &WM_NOTIFY
        Mov esi D@lParam, eax D$esi+8
        ..If eax = &TCN_SELCHANGE
            Call 'USER32.SendMessageA' D$H.ConfigTab, &TCM_GETCURSEL, 0, 0
            Mov D$ConfigTabIndex eax

            Call 'USER32.DestroyWindow' D$H.ConfigTabbedDialog
            Mov eax D$ConfigTabIndex, ecx 100 | mul ecx | add eax 4000
Push D$StringsLanguage ; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            Call 'USER32.CreateDialogParamA' D$H.Instance, eax, D$H.ConfigTab,
                                             ConfigTabProc, &NULL
            Mov D$H.ConfigTabbedDialog eax
Pop D$StringsLanguage  ; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            .If D$ConfigTabIndex = 0
                Call InittabTreeView
            .Else_If D$ConfigTabIndex = 1
                Call InitTabTextEditor
            .Else_If D$ConfigTabIndex = 2
                Call InitBadHabits
            .Else_If D$ConfigTabIndex = 3
                Call InitTabColors
            .Else_If D$ConfigTabIndex = 4
                Call InitTabHelpFiles
            .Else_If D$ConfigTabIndex = 5
                Call InitTabOtherFiles
            .Else_If D$ConfigTabIndex = 6
                Call InitTabUserMenu
            .Else_If D$ConfigTabIndex = 7
                Call InitTabPos
            .Else_If D$ConfigTabIndex = 8
                Call InitTabLang
            .Else_If D$ConfigTabIndex = 9
                Call InitTabdbp
            .Else_If D$ConfigTabIndex = 10
; Tag Dialog 5000
                Call InitTabAPI
            .End_If

        ..End_If

    ...Else_If D@msg = &WM_COMMAND
        Mov eax D@wParam | and D@wParam 0FFFF | shr eax 16

        ..If D@wParam = &IDCANCEL
            jmp L1>

        ..Else_If D@wParam = &IDOK
            Call UpdateRegistry

            If D$H.UserPopUp > 0
                Call 'USER32.DeleteMenu' D$H.MenuMain 7 &MF_BYPOSITION
            End_If
            Call AddUserMenu | Call EnableMenutems | Call EnableHelpMenutems
            Call 'USER32.DrawMenuBar' D$H.MainWindow
L1:         Call 'USER32.DestroyWindow' D@hwnd
            Mov D$H.ConfigDialog 0

        ..Else_If D@wParam = &IDHELP
            Call Help, B_U_AsmName, ConfigHelp, ContextHlpMessage

        ..Else_If eax = &BN_CLICKED
            Mov eax D@wParam
            .If D$ConfigTabIndex = 0                ; Tree View.
                If eax = 11                         ; Check Orphans
                    xor B$ShowOrphan &TRUE
                Else_If eax = 12                    ; Check First Call only
                    xor B$ShowLabelsOnce &TRUE
                Else_If eax = 13                    ; Check Tree View Auto-Hide
                    Call CloseTree
                    xor B$AutoHideTreeView &TRUE
                Else_If eax = 14                    ; Rebuild tree after each Compilation
                    xor B$AutoRebuildTreeView &TRUE
                End_If

            .Else_If D$ConfigTabIndex = 1           ; Text Editor.
                Call GetEditorConfig D@hwnd


            .Else_If D$ConfigTabIndex = 2           ; Bad Habits.
                If eax = 20
                    xor B$CtrlYFlag &TRUE
                Else_If eax = 21
                    xor B$BlockAutoDelete &TRUE
                Else_If eax = 22
                    xor B$NoVirtualLimit &TRUE
                Else_If eax = 23
                    xor B$WithControlA &TRUE
                End_If

L2:         .Else_If D$ConfigTabIndex = 3           ; Colors.
                pushad
                    If eax = 20                     ;D$Color1Handle
                        Mov eax D$ARVB.BackColor | Call SetColor
                        Mov D$ARVB.BackColor eax | Call INIT_BackGroundColors
                    Else_If eax = 10                ; Color 2
                        Mov eax D$StatementColor | Call SetColor | Mov D$StatementColor eax
                    Else_If eax = 11                ; Color 3
                        Mov eax D$CommentColor | Call SetColor | Mov D$CommentColor eax
                    Else_If eax = 12                ; Color 4
                        Mov eax D$TextColor | Call SetColor | Mov D$TextColor eax
                    Else_If eax = 13                ; Color 5
                        Mov eax D$BracketColor | Call SetColor | Mov D$BracketColor eax
                    Else_If eax = 21                ; Color of Edit Controls, List Boxes,...
                        Mov eax D$ARVB.DialogsBackColor | Call SetColor
                        Mov D$ARVB.DialogsBackColor eax | Call INIT_BackGroundColors
                    Else_If eax = 30
                        xor B$WantSizeMarkerColor &TRUE
                    End_If

                    Call AskForRedrawNow
                popad

            .Else_If D$ConfigTabIndex = 4           ; Help Files.
                If eax = 500
                    Call Help, B_U_AsmName, HelPFiles, ContextHlpMessage
                Else
                    Call ConfigHelpFiles
                End_If

            .Else_If D$ConfigTabIndex = 5           ; Other Files

                pushad
                    Push eax
                        If eax = 14
L1:                         Call BrowseForFolder D@hwnd, PathTitle
                            Call VerifyEquatesPath | On eax = &FALSE, jmp L1<
                                                     On eax <> 0-1, jmp L1>
                            Pop eax | jmp L2>
                        Else
                            Call ConfigSearchFile       ; returns Ptr or 0 in eax
                        End_If
L1:                 Pop ebx
                    On eax = 0, jmp L2>>
                    Mov esi eax
                    If ebx = 10
                        Mov edi CalcName
                    Else_If ebx = 11
                        Mov edi ClipName
                    Else_If ebx = 12
                        Mov edi B_U_AsmName
                    Else_If ebx = 13
                        Mov edi F2Name
                    Else_If ebx = 14
                        Mov edi EquatesName
                    Else
                        jmp L2>
                    End_If

                    Do
                        lodsb | stosb
                    Loop_Until al = 0

                    Call InitTabOtherFiles
L2:             popad

            .Else_If D$ConfigTabIndex = 6           ; User Menu
                On eax < 310, jmp L1>
                    On eax > 320, jmp L1>
                        Call StripOneMenuUserItem | jmp L7>>

L1:             pushad
                    Call CheckUserMenuItemText | On eax = 0, jmp L2>>
                popad
                pushad
                    Push eax
                        Call ConfigSearchFile       ; returns Ptr or 0 in eax
                    Pop ebx
                    On eax = 0, jmp L2>>
                    Mov esi eax

                    If ebx = 110
                        Mov edi UserMenu0Path
                    Else_If ebx = 111
                        Mov edi UserMenu1Path
                    Else_If ebx = 112
                        Mov edi UserMenu2Path
                    Else_If ebx = 113
                        Mov edi UserMenu3Path
                    Else_If ebx = 114
                        Mov edi UserMenu4Path | I9: jmp I9>    ; just because no more '.'If
                    Else_If ebx = 115
                        Mov edi UserMenu5Path
                    Else_If ebx = 116
                        Mov edi UserMenu6Path
                    Else_If ebx = 117
                        Mov edi UserMenu7Path
                    Else_If ebx = 118
                        Mov edi UserMenu8Path
                    Else_If ebx = 119
                        Mov edi UserMenu9Path
                    Else
                        jmp L2>
                    End_If

                    Do
                        lodsb | stosb
                    Loop_Until al = 0
                    Call SaveUserMenuTab | Call InitTabUserMenu
L2:             popad

            .Else_If D$ConfigTabIndex = 7           ; Pos
                If eax = 50
                    On D$FL.SaveMainPos = &FALSE Call GetUserPosTab
                Else_If eax = 51
                    On D$FL.SaveMainPos = &FALSE Call SaveUserPosTab
                Else_If eax = 52
                    Call SetMaximized
                Else_If eax = 20
                    xor D$FL.SaveMainPos &TRUE | Call InitTabPos
                End_If

          ; Tag Dialog 4800

             .Else_If D$ConfigTabIndex = 8           ; Lang
                Call SetNationalLanguage

             .Else_If D$ConfigTabIndex = 9           ; DBPMenu
                Call SetDBPMenu

             .Else_If D$ConfigTabIndex = 10          ; API verifications
                sub eax 10 | Mov D$ApiCheckFlag eax

             .End_If

        ..End_If

    ...Else_If D@msg = &WM_CTLCOLOREDIT
        jmp L1>

    ...Else_If D@msg = &WM_CTLCOLORLISTBOX
L1:     Call 'GDI32.SetBkColor' D@wParam D$ARVB.DialogsBackColor
        popad | Mov eax D$H.DialogsBackGroundBrush | jmp L9>

    ...Else
L8:     popad | Mov eax &FALSE | jmp L9>

    ...End_If

L7: popad | Mov eax &TRUE

L9: EndP
____________________________________________________________________________________________

[NATION_CHOOSEFONT: ; EditorCHOOSEFONT
 @lStructSize: D$ len
 @hwndOwner: D$ 0
 @hDC: D$ 0
 @lpLogFont: D$ NATION_LOGFONT
 @iPointSize: D$ 0
 @Flags: D$ &CF_SCREENFONTS__&CF_INITTOLOGFONTSTRUCT
 @rgbColors: D$ 0
 @lCustData: D$ 0
 @lpfnHook: D$ 0
 @lpTemplateName: D$ 0
 @hInstance: D$ 0
 @lpszStyle: D$ &SCREEN_FONTTYPE
 @nFontType: W$ 0
 @Alignment: W$ 0
 @nSizeMin: D$ 0
 @nSizeMax: D$ 0]

[NATION_LOGFONT: ; EditorLOGFONT
 @lfHeight: D$ 0
 @lfWidth: D$ 0
 @lfEscapement: D$ 0
 @lfOrientation: D$ 0
 @lfWeight: D$ 0
 @lfItalic: B$ 0
 @lfUnderline: B$ 0
 @lfStrikeOut: B$ 0
 @lfCharSet: B$ 0
 @lfOutPrecision: B$ 0
 @lfClipPrecision: B$ 0
 @lfQuality: B$ 0
 @lfPitchAndFamily: B$ 0
 @lfFaceName: B$ 0 # &LF_FACESIZE
 NATION_LOGFONTlen: D$ len] ; NATION_LOGFONT@lfFaceName

[H.NationalFont: D$ ?]

GetNationalFont:
    Move D$NATION_CHOOSEFONT@hwndOwner D$H.ConfigDialog
    Call 'Comdlg32.ChooseFontA' NATION_CHOOSEFONT

    If eax = &TRUE
        On D$H.NationalFont <> 0, Call 'GDI32.DeleteObject' D$H.NationalFont
        Call 'GDI32.CreateFontIndirectA' NATION_LOGFONT | Mov D$H.NationalFont eax
    End_If
ret


ConfigHelpFiles:
    pushad
        Push eax
            Call ConfigSearchFile       ; returns Ptr or 0 in eax
        Pop ebx
        On eax = 0, jmp L2>>
        Mov esi eax
        .If ebx = 10
        ;  Mov edi Asm32TutName
        .Else_If ebx = 11
            Mov edi Win32HlpName
        .Else_If ebx = 12
            Mov edi MmediaHlpName
        .Else_If ebx = 13
            Mov edi OpenGlHlpName
        .Else_If ebx = 14
        ; Mov edi OpcodeHlpName
        .Else_If ebx = 15
        ;   Mov edi WinDataFileName ;| I9: jmp I9>
    ; .Else_If ebx = 16
    ;     Mov edi B_U_AsmName    ; Now only in [Other Files]
        .Else_If ebx = 17
            Mov edi DxHlpName
        .Else_If ebx = 18
            Mov edi WinsockHlpName
        .Else_If ebx = 19
            Mov edi SDLRefName
        .Else_If ebx = 20
            Mov edi sqliteName
        .Else_If ebx = 21
            Mov edi DevILName
        .End_If

        Do
            lodsb | stosb
        Loop_Until al = 0
        Call InitTabHelpFiles
L2: popad
ret


Proc GetEditorConfig:
    Argument @Parent

        .If eax = 11                    ; TAB is 2 spaces
            Mov D$TabIs 2
        .Else_If eax = 12                    ; TAB is 4 spaces
            Mov D$TabIs 4
        .Else_If eax = 13                    ; TAB is 8 spaces
            Mov D$TabIs 8

        .Else_If eax = 50
            Call XorScrollBar
        .Else_If eax = 51
            xor D$FL.SecurityWanted &TRUE
        .Else_If eax = 52
            xor B$DollarOnly &TRUE
        .Else_If eax = 53                         ; Indent
            xor D$AutoIndentFlag &TRUE
            On B$AutoIndentFlag = &FALSE, Mov D$AutoIndent 0
        .Else_If eax = 54
            xor B$LoadMRU &TRUE
           ; Call 'USER32.CheckDlgButton' D@Parent 54 D$LoadMRU
        .Else_If eax = 55
            Call XorToolBar
        .Else_If eax = 56
            xor D$FL.SoundsWanted &TRUE
        .Else_If eax = 57
            xor B$CompletionWanted &TRUE
       ; .Else_If eax = 58
       ;     xor B$WriteCheckerWanted &TRUE
        .Else_If eax = 58
            If B$ParagraphChar = 167    ; Paragraph
                Mov B$ParagraphChar 36  ; Dollar
            Else
                Mov B$ParagraphChar 167
            End_If
        .Else_If eax = 201
            Call GetMaxBackUp
        .Else
            Call CheckFontAndBlink
        .End_If
EndP


GetMaxBackUp:
    Mov D$MaxBackUp 0
    Call 'USER32.GetDlgItemTextA' D$H.ConfigTabbedDialog, 200,
                                  MaxBackUp, 5

    On B$MaxBackUp = 0, Mov D$MaxBackUp '0000'

    While B$MaxBackUp+3 = 0
        shl D$MaxBackUp 8 | Mov B$MaxBackUp '0'
    End_While

    Call UpDateRegistry
ret


XorScrollBar:

    xor D$FL.ScrollBarWanted &TRUE | Call RedrawInterface

ret


XorToolBar:

    xor D$FL.ToolBarWanted &TRUE | Call RedrawInterface

ret


InittabTreeView:

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      10,
                                      &WM_SETTEXT,
                                      0,
                                      ITB1

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      11,
                                      &BM_SETCHECK,
                                      D$ShowOrphan,
                                      0

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      12,
                                      &BM_SETCHECK,
                                      D$ShowLabelsOnce,
                                      0

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      13,
                                      &BM_SETCHECK,
                                      D$AutoHideTreeView,
                                      0

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      14,
                                      &BM_SETCHECK,
                                      D$AutoRebuildTreeView,
                                      0

ret

InitTabTextEditor:

    Mov ebx D$TabIs | and ebx 2 | shr ebx 1

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      11,
                                      &BM_SETCHECK,
                                      ebx,
                                      0

    Mov ebx D$TabIs | and ebx 4 | shr ebx 2

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      12,
                                      &BM_SETCHECK,
                                      ebx,
                                      0

    Mov ebx D$TabIs | and ebx 8 | shr ebx 3

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      13,
                                      &BM_SETCHECK,
                                      ebx,
                                      0

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      50,
                                      &BM_SETCHECK,
                                      D$FL.ScrollBarWanted,
                                      0

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      51,
                                      &BM_SETCHECK,
                                      D$FL.SecurityWanted,
                                      0

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      52,
                                      &BM_SETCHECK,
                                      D$DollarOnly,
                                      0

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      53,
                                      &BM_SETCHECK,
                                      D$AutoIndentFlag,
                                      0

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      54,
                                      &BM_SETCHECK,
                                      D$LoadMRU,
                                      0

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      55,
                                      &BM_SETCHECK,
                                      D$FL.ToolBarWanted,
                                      0

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      56,
                                      &BM_SETCHECK,
                                      D$FL.SoundsWanted,
                                      0

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      57,
                                      &BM_SETCHECK,
                                      D$CompletionWanted,
                                      0

   ; Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 58, &BM_SETCHECK,
   ;                                   D$WriteCheckerWanted, 0

    If B$ParagraphChar = 167

        Mov eax &TRUE

    Else

        Mov eax &FALSE

    End_If

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      58,
                                      &BM_SETCHECK,
                                      eax,
                                      0

    Mov eax MaxBackUp

    While B$eax = '0'

        add eax ASCII

    End_While

    On B$eax = 0 sub eax ASCII

    Call 'USER32.SetDlgItemTextA' D$H.ConfigTabbedDialog,
                                  200,
                                  eax

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      200,
                                      &EM_SETLIMITTEXT,
                                      4,
                                      0

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                      301,
                                      &BM_SETCHECK,
                                      D$FL.BlinkingCaretWanted,
                                      0

    Call 'USER32.SetDlgItemInt' D$H.ConfigTabbedDialog,
                                302,
                                D$DU.CaretTime,
                                0

ret
____________________________________________________________________________________________

[BadHabittext: B$ "

A Sources Editor is not a Text Editor. 


This is why i spend much work-time with writing the Sources Editor from scratch instead of doing what most other IDEs Authors do:

Making use of an Edit or Richedit Control.

If you disagree with my choices these Flags are under your own decision.


Again, my recommandation: 

Do not use these Flags. 
These actions are incompatible with Assembly Edition, because they are going to help to corrupt your Sources." EOS]

InitBadHabits:
    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 10, &WM_SETTEXT,
                                      0, BadHabittext

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 20, &BM_SETCHECK,
                                      D$CtrlYFlag, 0

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 21, &BM_SETCHECK,
                                      D$BlockAutoDelete, 0

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 22, &BM_SETCHECK,
                                      D$NoVirtualLimit, 0

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 23, &BM_SETCHECK,
                                      D$WithControlA, 0
ret
____________________________________________________________________________________________


CheckFontAndBlink:
    .If eax > 13
        If eax = 20
            Call SelectFont

        Else
            jmp CheckBlinkCaret

        End_If

    .End_If
ret

CheckBlinkCaret:
    If eax = 301
        Call KillBlinkCursor
        xor D$FL.BlinkingCaretWanted &TRUE
        On B$FL.BlinkingCaretWanted = &TRUE, Call InitBlinkCursor
        Call AskForRedraw

    Else_If eax = 303
        Call 'USER32.GetDlgItemInt' D$H.ConfigTabbedDialog, 302, 0, 0
        Mov D$DU.CaretTime eax
        Call ResetBlinkCursor

    Else_If eax = 320
        xor B$CompletionWanted &TRUE

    End_If
ret


InitTabColors:
    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 30, &BM_SETCHECK,
                                      D$WantSizeMarkerColor, 0
ret


InitTabHelpFiles:
;    Mov esi HelpFilesPathsPtrs, ebx 110
;L0: Push ebx esi
;        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog ebx &WM_SETTEXT 0 D$esi
;    Pop esi ebx
;    add esi 4 | inc ebx | cmp D$esi 0 | ja L0<
    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 111, &WM_SETTEXT 0,
                                      Win32HlpName
    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 112, &WM_SETTEXT 0,
                                      MmediaHlpName
    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 113, &WM_SETTEXT 0,
                                      OpenGlHlpName
    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 117, &WM_SETTEXT 0,
                                      DxHlpName
    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 118, &WM_SETTEXT 0,
                                      WinsockHlpName
    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 119, &WM_SETTEXT 0,
                                      SDLRefName
    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 120, &WM_SETTEXT 0,
                                      sqliteName
    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 121, &WM_SETTEXT 0,
                                      DevILName
ret


[OthersFilesPathsPtrs: CalcName ClipName B_U_AsmName F2Name EquatesName 0]

InitTabOtherFiles:
    Mov esi OthersFilesPathsPtrs, ebx 100
L0: Push ebx esi
        If D$esi = EquatesName
            Push esi
                Mov esi EquatesName
                While B$esi <> 0 | inc esi | End_While
                While D$esi <> 'Equa' | dec esi | End_While
                Mov B$esi 0
            Pop esi
        End_If
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, ebx, &WM_SETTEXT, 0,
                                          D$esi
    Pop esi ebx
    add esi 4 | inc ebx | cmp D$esi 0 | ja L0<

    Mov esi EquatesName
    While B$esi <> 0 | inc esi | End_While
    On D$esi+1 = 'quat', Mov B$esi 'E'
ret

;;
  Note: a Durty hack up there, because i don't want to rewrite all of the Routines
  concerned by 'EquatesName' (providing the Path to all RosAsm Files, now). Previously
  i did not know how to do what 'BrowseForFolder' actually does, and i was retrieving
  the Path by runing a usual 'GetOpenFileName' pointing to Equates.equ.
;;


[UserMenuItemsPtrs: UserMenu0String UserMenu1String UserMenu2String UserMenu3String
                    UserMenu4String UserMenu5String UserMenu6String UserMenu7String
                    UserMenu8String UserMenu9String 0

 UserMenuPathsPtrs: UserMenu0Path UserMenu1Path UserMenu2Path UserMenu3Path
                    UserMenu4Path UserMenu5Path UserMenu6Path UserMenu7Path
                    UserMenu8Path UserMenu9Path 0]

InitTabUserMenu:
    Mov esi UserMenuItemsPtrs, edi UserMenuPathsPtrs, ebx 10, ecx 210

L0: Push edi, ecx, esi, ebx
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, ecx, &WM_SETTEXT,
                                          0, D$edi
        Pop ebx, esi | Push esi, ebx
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, ebx, &WM_SETTEXT,
                                          0, D$esi
    Pop ebx, esi, ecx, edi
    add esi 4 | add edi 4 | inc ebx | inc ecx
    cmp D$esi 0 | ja L0<
ret


; ebx = 310 Based ID of [x] Button.

StripOneMenuUserItem:
    sub eax 310
  ; 'UserMenu0String' length is 8 dWords (32 Bytes per Record)
  ; 'UserMenu0Path' is 64 dWords (256 Bytes per Record)
  ; (sizes for ease of pointing).

    shl eax 5 ; * 32  (8 * dWord)

    lea edi D$UserMenu0String+eax | Mov esi edi | add esi (4*8)
    Mov ecx (8*8) | rep movsd

    shl eax 3  ; this is 3 more times > * 256 (64 * dWord)

    lea edi D$UserMenu0Path+eax | Mov esi edi | add esi (4*64)
    Mov ecx (64*8) | rep movsd

    Call InitTabUserMenu
ret


; Tag Dialog 4700 >>> Pos

InitTabPos:
     Call 'USER32.SetDlgItemInt' D$H.ConfigTabbedDialog, 10, D$WindowX, &NULL
     Call 'USER32.SetDlgItemInt' D$H.ConfigTabbedDialog, 11, D$WindowY, &NULL
     Call 'USER32.SetDlgItemInt' D$H.ConfigTabbedDialog, 12, D$WindowW, &NULL
     Call 'USER32.SetDlgItemInt' D$H.ConfigTabbedDialog, 13, D$WindowH, &NULL

     If B$IsMaximizedFlag = &SW_MAXIMIZE
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 52,
                                          &BM_SETCHECK &TRUE, 0
        Mov eax &FALSE

     Else_If D$FL.SaveMainPos = &TRUE

        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog,
                                          20,
                                          &BM_SETCHECK &TRUE,
                                          0

        Mov eax &FALSE

     End_If

     Push eax
     Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 10, &WM_ENABLE, eax, 0
     Pop eax | Push eax
     Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 11, &WM_ENABLE, eax, 0
     Pop eax | Push eax
     Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 12, &WM_ENABLE, eax, 0
     Pop eax
     Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 13, &WM_ENABLE, eax, 0
ret


SetMaximized:
     If B$IsMaximizedFlag = &SW_SHOWNORMAL
        Mov B$IsMaximizedFlag &SW_MAXIMIZE
     Else
        Mov B$IsMaximizedFlag &SW_SHOWNORMAL
     End_If

     Call 'USER32.ShowWindow'  D$H.MainWindow, D$IsMaximizedFlag
ret

; Tag Dialog 4800

InitTabLang:
    .If D$StringsLanguage = '.en'
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 10, &BM_SETCHECK, 1, 0
    .Else_If D$StringsLanguage = '.fr'
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 11, &BM_SETCHECK, 1, 0
    .Else_If D$StringsLanguage = '.br'
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 12, &BM_SETCHECK, 1, 0
    .Else_If D$StringsLanguage = '.sp'
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 13, &BM_SETCHECK, 1, 0
    .Else_If D$StringsLanguage = '.zh'
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 14, &BM_SETCHECK, 1, 0
    .Else_If D$StringsLanguage = '.it'
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 15, &BM_SETCHECK, 1, 0
    .Else_If D$StringsLanguage = '.de'
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 16, &BM_SETCHECK, 1, 0
    .Else_If D$StringsLanguage = '.no'
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 17, &BM_SETCHECK, 1, 0
    .Else_If D$StringsLanguage = '.ca'
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 18, &BM_SETCHECK, 1, 0
    .Else
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 10, &BM_SETCHECK, 1, 0
    .End_If

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 100, &EM_SETMARGINS,
                                      &EC_LEFTMARGIN__&EC_RIGHTMARGIN, 0100010

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 100, &WM_SETTEXT, 0,
    {B$ "
This Tab is for the definition of the Language to be used for displaying the Errors Messages given at Compile-Time, by the Assembler. The method will be generalized to the other Messages progressively.

Before using, you must have the National Strings Files, in a '.../RosAsmFiles/Lang/' Folder. If you downloaded 'RosAsmFull.zip', you have nothing to do. If you dowloaded the partial Files, unzip 'LangFiles.zip', in such a Folder.

The [National Font] Button is for choosing the Font to be used for dispaying the Error Messages. For the 'Ascii Users', this is just an option, but for the 'Unicode Users', - actualy only the [Chinese] ones -, this Font MUST be defined.

If your Language is missing, in this list, and if you wish to do the translation work, go to your Directory of 'RosAsm Files', and, in the [Lang] Folder, take the 'RosAsmStrings.en' Files, in any Editor.

In this File:

The '$$$$1', '$$$$2', ... Markers are simple Strings IDs, that you have to leave the way they are. These IDs are used by RosAsm to point out what National String subsitution matches with the default one.

In case you would see some '#1', '#2', ... inside a String, let these special Markers, as they are. These ones are used by RosAsm to mark the Positions of some Components of the Strings. For example, if an Error Message includes a Variable Number, these Markers are replaced on the fly, by the wished Numbers.

When you are over with a Translation, send me your work by Mail, at < betov@free.fr >, so that i could include it, and implement the needed Radio Button is this Tab.

" EOS}
ret
____________________________________________________________________________________________

[DOUBLE_CLICK_ACTION 2  RIGHT_CLICK_ACTION 3]

[DBPMenuOn: DOUBLE_CLICK_ACTION]

InitTabdbp:
    If D$DBPMenuOn = DOUBLE_CLICK_ACTION
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 11, &BM_SETCHECK, 1, 0
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 12, &BM_SETCHECK, 0, 0
    Else
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 11, &BM_SETCHECK, 0, 0
        Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 12, &BM_SETCHECK, 1, 0
    End_If
ret


[ApiCheckFlag: D$ ?] ; 0 // &DONT_RESOLVE_DLL_REFERENCES // No Verifiction

[ApiCheckInfo: B$ "
Beginners: Let this Flag [Normal].

When compiling a Source, RosAsm does several verifications on your API calls, in order to point you to typos, missing DLLs, and so on...

In some exeptional cases (WDM drivers, General Hocks, ...) this may be a problem. So, if you perfectly know what you are doing, two Flags let you define the behaviour of RosAsm:

With the DONT_RESOLVE_DLL_REFERENCES Flag, the verifications of your calls will not address the references inside the concerned DLLS.

The last Flag will completely skip your API calls verifications. You do not need to set it for WDM Drivers, because, in the case of .SYS files, no such verification, is done, anyway." EOS]

InitTabAPI:
    Mov eax 10 | add eax D$ApiCheckFlag

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, eax, &BM_SETCHECK, 1, 0

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 100, &WM_SETTEXT, 0, ApiCheckInfo
ret


SetDBPMenu:
    If eax = 11
        Mov D$DBPMenuOn DOUBLE_CLICK_ACTION
    Else
        Mov D$DBPMenuOn RIGHT_CLICK_ACTION
    End_If
ret
____________________________________________________________________________________________

[ConfigFilesFilters: B$ 'All' EOS  '*.*' EOS 0]
[FullPathUserChoice: B$ ? # &MAX_PATH]
[ConfigInitialDir: B$ ? # &MAX_PATH]
[ConfigFileTitle: B$ 'Choose the file to asign to the Menu' EOS]

[CONFIGOPENFILENAME:
 @lStructSize: D$ LEN
 @hWndOwner: D$ 0
 @hInstance: D$ 0
 @lpstrFilter: D$ ConfigFilesFilters
 @lpstrCustomFilter: D$ 0
 @nMaxCustFilter: D$ 0
 @nFilterIndex: D$ 0
 @lpstrFile: D$ FullPathUserChoice
 @nMaxFile: D$ 260
 @lpstrFileTitle: D$ 0
 @nMaxFileTitle: D$ 0
 @lpstrInitialDir: D$ ConfigInitialDir
 @lpstrTitle: D$ ConfigFileTitle
 @Flags: D$ 0281804
 @nFileOffset: W$ 0
 @nFileExtension: W$ 0
 @lpstrDefExt: D$ 0
 @lCustData: D$ 0
 @lpfnHook: D$ 0
 @lpTemplateName: D$ 0]

ConfigSearchFile:
    Move D$CONFIGOPENFILENAME@hWndOwner D$H.ConfigDialog
    Move D$CONFIGOPENFILENAME@hInstance D$H.Instance
    Mov edi FullPathUserChoice, al 0, ecx 260 | rep stosb
    Call 'COMDLG32.GetOpenFileNameA' CONFIGOPENFILENAME
    If B$FullPathUserChoice <> 0
        Mov eax FullPathUserChoice
    Else
        Mov eax 0
    End_If
ret

____________________________________________________________________________________________

; User ask for a 'ConfigSearchFile'. He must first set the Menu Item text:

[TitleErrorUserMenu: B$ 'User Menu:' EOS]
[ErrorUserMenu: B$ 'Enter the Menu item Text in the EditBox first.' EOS]

CheckUserMenuItemText:
  ; > in: eax = ID of [File...] Button (first one = 110
    Push eax
        sub eax 110 | shl eax 5
        lea edi D$UserMenu0String+eax
    Pop eax
    Push edi
        sub eax 100                             ; Menu Item Edit Control (First one = 10)
        Call 'USER32.GetDlgItem' D$H.ConfigTabbedDialog eax
    Pop edi
    Call 'USER32.SendMessageA' eax &WM_GETTEXT 30 edi
    If eax = 0
        Call 'USER32.MessageBoxA' D$H.MainWindow, ErrorUserMenu, TitleErrorUserMenu, &MB_SYSTEMMODAL
        Mov eax &FALSE
    Else
        Mov eax &TRUE
    End_If
ret


SaveUserMenuTab:
    pushad
        Mov edi UserMenu0String, eax 10
L1:     Push edi, eax
            Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, eax,
                                              &WM_GETTEXT, 30, edi
        Pop eax, edi
        inc eax | add edi 32 | cmp edi UserMenu9String | jbe L1<

        Mov esi UserMenu0String, edi UserMenu0Path
L1:     On B$esi = 0, Mov B$edi 0
        add esi 32 | add edi 256 | cmp edi UserMenu9Path | jbe L1<
    popad
ret



GetUserPosTab:

    Call 'USER32.GetWindowRect' D$H.MainWindow,
                                STRUC.RECT.MainWindow

    If D$WindowX > 0FFF_FFFF

        Mov D$WindowX 0

        Mov D$WindowY 0

        Call 'USER32.GetSystemMetrics' &SM_CXSCREEN | Mov D$WindowW eax

        Call 'USER32.GetSystemMetrics' &SM_CYSCREEN | Mov D$WindowH eax

    Else

        Mov eax D$WindowX | sub D$WindowW eax

        Mov eax D$WindowY | sub D$WindowH eax

    End_If

    Call InitTabPos

ret

SaveUserPosTab:

    Call 'USER32.SendDlgItemMessageA' D$H.ConfigTabbedDialog, 20, &BM_GETCHECK, 0, 0

    Mov D$FL.SaveMainPos eax

    Call 'USER32.GetDlgItemInt' D$H.ConfigTabbedDialog, 10, &NULL, &NULL

    Mov D$WindowX eax

    Call 'USER32.GetDlgItemInt' D$H.ConfigTabbedDialog, 11, &NULL, &NULL

    Mov D$WindowY eax

    Call 'USER32.GetDlgItemInt' D$H.ConfigTabbedDialog, 12, &NULL, &NULL

    Mov D$WindowW eax

    Call 'USER32.GetDlgItemInt' D$H.ConfigTabbedDialog, 13, &NULL, &NULL

    Mov D$WindowH eax

    Call 'USER32.MoveWindow' D$H.MainWindow, D$WindowX, D$WindowY, D$WindowW, D$WindowH, &FALSE

    Mov eax D$WindowX | or eax D$WindowY

    ..If eax = 0

        Call 'USER32.GetSystemMetrics' &SM_CXSCREEN

        .If eax = D$WindowW

            Call 'USER32.GetSystemMetrics' &SM_CYSCREEN

            If eax = D$WindowH

                Call 'USER32.ShowWindow' D$H.MainWindow, &SW_MAXIMIZE

            End_If

        .End_If

    ..End_If

ret
____________________________________________________________________________________________


SetColor:

[@CustomColorsSet: D$ ? # 16]; TODO Structure sur pile

[@STRUC.CHOOSECOLORAPI: ; TODO Structure sur pile
 @lStructSize: D$ len
 @hwndOwner: D$ 0
 @hInstance: D$ 0
 @rgbResult: D$ 0
 @lpCustColors: D$ @CustomColorsSet
 @Flags: D$ &CC_RGBINIT+&CC_FULLOPEN
 @lCustData: D$ 0
 @lpfnHook: D$ 0
 @lpTemplateName: D$ 0]

    Move D@hwndOwner D$H.ConfigDialog,
         D@hInstance D$H.Instance

    Mov D@rgbResult eax

    Push eax ; old color set by caller in eax

            Call 'COMDLG32.ChooseColorA' @STRUC.CHOOSECOLORAPI

    Pop eax ; restore old color if no choice

    If eax <> &FALSE

        Mov eax D@rgbResult

    End_If

ret
____________________________________________________________________________________________

; Registry jobs:

[hRegKey: D$ 0
 Result: D$ 0
 @Datatype: D$ 0
 RegistryDataSize: D$ DWORD]

[BUAsmConfigClass: B$ 'BUAsmConfigClass' EOS]
[BUAsmKey: B$ 'Software\BUAsm' EOS]
[Color1: B$ 'Color1' EOS]
[Color2: B$ 'Color2' EOS]
[Color3: B$ 'Color3' EOS]
[Color4: B$ 'Color4' EOS]
[Color5: B$ 'Color5' EOS]
[Color10: B$ 'Color10' EOS]
[SizeColorFlag: B$ 'SizeMarker Color Flag' EOS]

[UserLanguage: B$ 'Lang' EOS]

[Blink: B$ 'Blink' EOS]
[BlinkTime: B$ 'BlinkTime' EOS]

[Complete: B$ 'Complete' EOS]

[PrinterFont: B$ 'PrinterFont' EOS]
[ScrollBar: B$ 'ScrollBar' EOS]
[ToolBar: B$ 'ToolBar' EOS]

[SourceEditorFont: B$ 'SourceEditorFont' EOS]
[NationalLanguageFont: B$ 'NationalLanguageFont' EOS]
[UnicodeEditorFont: B$ 'UnicodeEditorFont' EOS]

[BackUpLimit: B$ 'MaxBackUp' EOS]

[MainPosX: B$ 'MainPosX' EOS]
[MainPosY: B$ 'MainPosY' EOS]
[MainPosW: B$ 'MainPosW' EOS]
[MainPosH: B$ 'MainPosH' EOS]

[SaveMainPos: B$ 'SaveMainPos' EOS]
[IsMaximized: B$ 'IsMaximized' EOS]

[TreeViewReg: B$ 'TreeViewReg' EOS]
[TreeViewReg2: B$ 'TreeViewReg2' EOS]
[TreeViewAutoHide: B$ 'TreeViewAutoHide' EOS]
[TreeRebuild: B$ 'TreeRebuild' EOS]
[TreeW: B$ 'Tree Width' EOS]

[AutoIndentReg: B$ 'AutoIndentReg' EOS]
[TabReg: B$ 'TabReg' EOS]
; Font: B$ 'Font' EOS
[CtrlYis: B$ 'CtrlYis' EOS]
[LoadMRUis: B$ 'LoadLastMRU' EOS]
[BlockAutoDeleteIs: B$ 'BlockAutoDeleteIs' EOS]
[VirtualLimit: B$ 'VirtualLimit' EOS]
[WithCtrlA: B$ 'With Ctrl A' EOS]
[WithSecurity: B$ 'WithSecurity' EOS]
[WithSounds: B$ 'WithSounds' EOS]
[WithDollarOnly: B$ 'Dollar only' EOS]
[WithChecker: B$ 'Write Checker' EOS]
[ParagrafChar: B$ 'ParagrafChar' EOS]

[CalcPath: B$ 'CalcPath' EOS]
[EquatesPath: B$ 'EquatesPath' EOS]
[F2Path: B$ 'F2Path' EOS]
[RosAsmHelpPath: B$ 'RosAsmHelpPath' EOS]

[WinApiPath: B$ 'WinApiPath' EOS]

[MmediaPath: B$ 'MmediaPath' EOS]
[OpenGlPath: B$ 'OpenGlPath' EOS]
[DxPath: B$ 'DxPath' EOS]
[WinSockPath: B$ 'WinSockPath' EOS]
[SDLRefPath: B$ 'SDLRefPath' EOS]
[sqlitePath: B$ 'sqlitePath' EOS]
[DevILPath: B$ 'DevILPath' EOS]
[ClipPath: B$ 'ClipPath' EOS]

[UserMenu0: B$ 'UserMenu0' EOS]
[UserMenu1: B$ 'UserMenu1' EOS]
[UserMenu2: B$ 'UserMenu2' EOS]
[UserMenu3: B$ 'UserMenu3' EOS]
[UserMenu4: B$ 'UserMenu4' EOS]
[UserMenu5: B$ 'UserMenu5' EOS]
[UserMenu6: B$ 'UserMenu6' EOS]
[UserMenu7: B$ 'UserMenu7' EOS]
[UserMenu8: B$ 'UserMenu8' EOS]
[UserMenu9: B$ 'UserMenu9' EOS]
[UserPath0: B$ 'UserMenuPath0' EOS]
[UserPath1: B$ 'UserMenuPath1' EOS]
[UserPath2: B$ 'UserMenuPath2' EOS]
[UserPath3: B$ 'UserMenuPath3' EOS]
[UserPath4: B$ 'UserMenuPath4' EOS]
[UserPath5: B$ 'UserMenuPath5' EOS]
[UserPath6: B$ 'UserMenuPath6' EOS]
[Userpath7: B$ 'UserMenuPath7' EOS]
[UserPath8: B$ 'UserMenuPath8' EOS]
[UserPath9: B$ 'UserMenuPath9' EOS]

[STR.A.MRU1Title: B$ 'MRU1' EOS]
[STR.A.MRU2Title: B$ 'MRU2' EOS]
[STR.A.MRU3Title: B$ 'MRU3' EOS]
[STR.A.MRU4Title: B$ 'MRU4' EOS]
[DBPMenuString: B$ 'DBPMenu' EOS]
[ApiCheckString: B$ 'ApiCheck' EOS]

[UserMenu0String: D$ ? # 8] [UserMenu1String: D$ ? # 8] [UserMenu2String: D$ ? # 8]
[UserMenu3String: D$ ? # 8] [UserMenu4String: D$ ? # 8] [UserMenu5String: D$ ? # 8]
[UserMenu6String: D$ ? # 8] [UserMenu7String: D$ ? # 8] [UserMenu8String: D$ ? # 8]
[UserMenu9String: D$ ? # 8]
;&MAX_PATH = 0104   (0104/4) = 041 = 65
[UserMenu0Path: D$ ? # 64] [UserMenu1Path: D$ ? # 64] [UserMenu2Path: D$ ? # 64] [UserMenu3Path: D$ ? # 64]
[UserMenu4Path: D$ ? # 64] [UserMenu5Path: D$ ? # 64] [UserMenu6Path: D$ ? # 64] [UserMenu7Path: D$ ? # 64]
[UserMenu8Path: D$ ? # 64] [UserMenu9Path: D$ ? # 64]

[RegistryData:
 Color1          &REG_DWORD  ARVB.BackColor
 ; NEVER INSERT ANYTHING !!!
 Color2          &REG_DWORD  StatementColor
 Color3          &REG_DWORD  CommentColor
 Color4          &REG_DWORD  TextColor
 Color5          &REG_DWORD  BracketColor
 Color10         &REG_DWORD  ARVB.DialogsBackColor
 SizeColorFlag   &REG_DWORD  WantSizeMarkerColor
 ; NEVER INSERT ANYTHING !!!
 UserLanguage    &REG_DWORD  StringsLanguage
 NationalLanguageFont &REG_BINARY NATION_LOGFONT
 UnicodeEditorFont &REG_BINARY UNICODE_EDITION_LOGFONT
 ; NEVER INSERT ANYTHING !!!
 Blink           &REG_DWORD  FL.BlinkingCaretWanted
 BlinkTime       &REG_DWORD  DU.CaretTime
 ; NEVER INSERT ANYTHING !!!
 Complete        &REG_DWORD  CompletionWanted
 ; NEVER INSERT ANYTHING !!!
 PrinterFont     &REG_BINARY cbbuffer
 ScrollBar       &REG_DWORD  FL.ScrollBarWanted
 ToolBar         &REG_DWORD  FL.ToolBarWanted
 ; NEVER INSERT ANYTHING !!!
 SourceEditorFont &REG_BINARY EditorLOGFONT
 ; NEVER INSERT ANYTHING !!!
 BackUpLimit     &REG_DWORD  MaxBackUp
 ; NEVER INSERT ANYTHING !!!
 MainPosX        &REG_DWORD  WindowX
 MainPosY        &REG_DWORD  WindowY
 MainPosW        &REG_DWORD  WindowW
 MainPosH        &REG_DWORD  WindowH
 ; NEVER INSERT ANYTHING !!!
 SaveMainPos     &REG_DWORD  FL.SaveMainPos
 IsMaximized     &REG_DWORD  IsMaximizedFlag
 ; NEVER INSERT ANYTHING !!!
 TreeViewReg     &REG_DWORD  ShowOrphan
 TreeViewReg2    &REG_DWORD  ShowLabelsOnce
 TreeViewAutoHide &REG_DWORD AutoHideTreeView
 TreeRebuild     &REG_DWORD  AutoRebuildTreeView
 TreeW           &REG_DWORD  TreeWidth
 ; NEVER INSERT ANYTHING !!!
 AutoIndentReg   &REG_DWORD  AutoIndentFlag
 TabReg          &REG_DWORD  TabIs
 ; Font            &REG_DWORD  UserFont
 CtrlYis         &REG_DWORD  CtrlYflag
 LoadMRUis       &REG_DWORD  LoadMRU
 BlockAutoDeleteIs &REG_DWORD  BlockAutoDelete
 VirtualLimit    &REG_DWORD  NoVirtualLimit
 WithCtrlA       &REG_DWORD  WithControlA
 WithSecurity    &REG_DWORD  FL.SecurityWanted
 WithSounds      &REG_DWORD  FL.SoundsWanted
 WithDollarOnly  &REG_DWORD  DollarOnly
 WithChecker     &REG_DWORD  WriteCheckerWanted
 ParagrafChar    &REG_DWORD  ParagraphChar
 ; NEVER INSERT ANYTHING !!!
 CalcPath        &REG_SZ     CalcName
 EquatesPath     &REG_SZ     EquatesName
 ; NEVER INSERT ANYTHING !!!
 F2Path          &REG_SZ     F2Name
 ; NEVER INSERT ANYTHING !!!
 RosAsmHelpPath   &REG_SZ    B_U_AsmName
 ; NEVER INSERT ANYTHING !!!
 WinApiPath      &REG_SZ     Win32HlpName
 ;  AsmHelPath      &REG_SZ     OpcodeHlpName
 MmediaPath      &REG_SZ     MmediaHlpName
 OpenGlPath      &REG_SZ     OpenGlHlpName
 DxPath          &REG_SZ     DxHlpName
 WinSockPath     &REG_SZ     WinsockHlpName
 SDLRefPath      &REG_SZ     SDLRefName
 sqlitePath      &REG_SZ     sqliteName
 DevILPath       &REG_SZ     DevILName
 ClipPath        &REG_SZ     ClipName
 ; NEVER INSERT ANYTHING !!!
 UserMenu0       &REG_SZ     UserMenu0String
 UserMenu1       &REG_SZ     UserMenu1String
 UserMenu2       &REG_SZ     UserMenu2String
 UserMenu3       &REG_SZ     UserMenu3String
 UserMenu4       &REG_SZ     UserMenu4String
 UserMenu5       &REG_SZ     UserMenu5String
 UserMenu6       &REG_SZ     UserMenu6String
 UserMenu7       &REG_SZ     UserMenu7String
 UserMenu8       &REG_SZ     UserMenu8String
 UserMenu9       &REG_SZ     UserMenu9String
 UserPath0       &REG_SZ     UserMenu0Path
 UserPath1       &REG_SZ     UserMenu1Path
 UserPath2       &REG_SZ     UserMenu2Path
 UserPath3       &REG_SZ     UserMenu3Path
 UserPath4       &REG_SZ     UserMenu4Path
 UserPath5       &REG_SZ     UserMenu5Path
 UserPath6       &REG_SZ     UserMenu6Path
 UserPath7       &REG_SZ     UserMenu7Path
 UserPath8       &REG_SZ     UserMenu8Path
 UserPath9       &REG_SZ     UserMenu9Path
 ; NEVER INSERT ANYTHING !!!
 STR.A.MRU1Title &REG_SZ     STR.A.MRU1
 STR.A.MRU2Title &REG_SZ     STR.A.MRU2
 STR.A.MRU3Title &REG_SZ     STR.A.MRU3
 STR.A.MRU4Title &REG_SZ     STR.A.MRU4
 ; NEVER INSERT ANYTHING !!!
 DBPMenuString   &REG_DWORD  DBPMenuOn
 ; NEVER INSERT ANYTHING !!!
 ApiCheckString  &REG_DWORD  ApiCheckFlag
 ; ALWAYS ADD AT THE END !!!!!!!!!!!!!!!!!!!!
 0]

OpenRegistry:

    Call 'ADVAPI32.RegCreateKeyExA' &HKEY_CURRENT_USER,
                                    BUAsmKey,
                                    0,
                                    BUAsmConfigClass,
                                    0,
                                    &KEY_READ+&KEY_WRITE+&KEY_QUERY_VALUE,
                                    0,
                                    hRegKey,
                                    Result

ret


ReadRegistry:

    Mov esi RegistryData

    ...If D$Result = &REG_CREATED_NEW_KEY
      ; Case of new installation of the Registry:
L1:     Push esi
            Call GetFieldDataSize esi
            Call 'ADVAPI32.RegSetValueExA' D$hRegKey, D$esi, 0, D$esi+4, D$esi+8, eax
        Pop esi
        On eax <> &ERROR_SUCCESS, jmp AutoInit
        add esi 12 | cmp D$esi 0 | ja L1<<

        Call AutoInit

    ...Else
      ; Case of existing Registry to be read:
L1:     Push esi
            lea ebx D$esi+4 | Mov D$RegistryDataSize 0FF
            Call 'ADVAPI32.RegQueryValueExA' D$hRegKey, D$esi, 0, ebx, D$esi+8,
                                             RegistryDataSize
            .If eax <> &ERROR_SUCCESS
                Pop esi | Push esi
                Call GetFieldDataSize esi
                Call 'ADVAPI32.RegSetValueExA' D$hRegKey, D$esi, 0, D$esi+4, D$esi+8, eax
            .End_If
        Pop esi
        add esi 12 | cmp D$esi 0 | ja L1<<
    ...End_If

    Call CloseRegistry
ret


UpdateRegistry:
    On D$UserConfig = CONFIGFILE, jmp WriteConfigFile

    Call OpenRegistry

    Mov esi RegistryData
  ; either &REG_BINARY // &REG_DWORD // &REG_SZ
L1: Push esi
        Call GetFieldDataSize esi
        Call 'ADVAPI32.RegSetValueExA' D$hRegKey, D$esi, 0, D$esi+4, D$esi+8, eax
    Pop esi
    add esi 12 | cmp D$esi 0 | ja L1<<

    Call CloseRegistry
ret


Proc GetFieldDataSize:

    Argument @Pointer

    Uses esi

        Mov esi D@Pointer
      ; esi >>> One Key in 'RegistryData': PointerToName // &FLAG // RosAsmData
      ; &FLAG can be: &REG_BINARY // &REG_DWORD // &REG_SZ
        If D$esi+4 = &REG_DWORD
            Mov eax 4
      ; cases of Fonts LOGFONT Structures:
        Else_If D$esi = PrinterFont
          ; Static size of cbbuffer (&REG_BINARY):
            Mov eax 60
        Else_If D$esi = SourceEditorFont
            Mov eax D$EditorLOGFONT.len
        Else_If D$esi = NationalLanguageFont
            Mov eax D$NATION_LOGFONTlen
        Else_If D$esi = UnicodeEditorFont
            Move eax D$UNICODE_EDITION_LOGFONTlen
      ; Cases of Strings (&REG_SZ):
        Else
            Push edi
                Mov edi D$esi+8, ecx 0FFFF, al 0 | repne scasb
                sub ecx 0FFFF | neg ecx | Mov eax ecx
            Pop edi
        End_If
      ; eax = Size of the Data to be saved into the Registry Key:
EndP


CloseRegistry:
    Call 'ADVAPI32.RegCloseKey' D$hRegKey
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  Auto initialize the companion Files registries if this is a first install done
  after unzipping the whole Package.
;;


AutoInit:  ; 'RegistryData'

    Call GetDirectory ActualDir

    Mov edi ActualDir | While B$edi <> 0 | inc edi | End_While

    On B$edi-1 <> '\', Mov B$edi '\' | inc edi
  ; ...\RosAsmFiles\
    Mov D$edi 'RosA', D$edi+4 'smFi', D$edi+8 'les\' | add edi 12
    Mov D$ActualDirPointer edi

  ; 'Equates.inc'
    Mov D$edi 'Equa', D$edi+4 'tes.', D$edi+8 'equ'
    Call 'KERNEL32.FindFirstFileA' ActualDir, WIN32_FIND_DATA

    .If eax <> &INVALID_HANDLE_VALUE
        Call 'KERNEL32.FindClose' eax
        Mov esi ActualDir, edi EquatesName
        While B$esi <> 0 | movsb | End_While | Mov B$edi 0

      ; 'B_U_Asm.exe'
        Mov edi D$ActualDirPointer
        Mov D$edi 'B_U_', D$edi+4 'Asm.', D$edi+8 'exe', B$edi+12 0
        Call 'KERNEL32.FindFirstFileA' ActualDir, WIN32_FIND_DATA

        If eax <> &INVALID_HANDLE_VALUE
            Call 'KERNEL32.FindClose' eax
            Mov esi ActualDir, edi B_U_AsmName
            While B$esi <> 0 | movsb | End_While | Mov B$edi 0
        End_If

      ; 'Clip.txt
        Mov edi D$ActualDirPointer
        Mov D$edi 'Clip', D$edi+4 '.txt', B$edi+8 0
        Call 'KERNEL32.FindFirstFileA' ActualDir, WIN32_FIND_DATA
        If eax <> &INVALID_HANDLE_VALUE
            Call 'KERNEL32.FindClose' eax
            Mov esi ActualDir, edi ClipName
            While B$esi <> 0 | movsb | End_While | Mov B$edi 0
        End_If

    .End_If
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; add a User Menu if any.

[H.UserPopUp: D$ ?]
[UserPopMenu: B$ 'User' EOS]

AddUserMenu:
    On B$UserMenu0Path = EOS ret

    On B$UserMenu0String = EOS ret


    Call 'USER32.CreatePopupMenu' | Mov D$H.UserPopUp eax

    Call 'USER32.AppendMenuA' D$H.UserPopUp,
                              &MF_STRING,
                              2000,
                              UserMenu0String

    On B$UserMenu1Path = EOS jmp S1>>

    On B$UserMenu1String = EOS jmp S1>>

        Call 'USER32.AppendMenuA' D$H.UserPopUp,
                                  &MF_STRING,
                                  2001,
                                  UserMenu1String

    On B$UserMenu2Path = EOS jmp S1>>

    On B$UserMenu2String = EOS jmp S1>>

        Call 'USER32.AppendMenuA' D$H.UserPopUp,
                                  &MF_STRING,
                                  2002,
                                  UserMenu2String

    On B$UserMenu3Path = EOS jmp S1>>

    On B$UserMenu3String = EOS jmp S1>>

        Call 'USER32.AppendMenuA' D$H.UserPopUp,
                                  &MF_STRING,
                                  2003,
                                  UserMenu3String

    On B$UserMenu4Path = EOS jmp S1>>

    On B$UserMenu4String = EOS jmp S1>>

        Call 'USER32.AppendMenuA' D$H.UserPopUp,
                                  &MF_STRING,
                                  2004,
                                  UserMenu4String

    On B$UserMenu5Path = EOS jmp S1>>

    On B$UserMenu5String = EOS jmp S1>>

        Call 'USER32.AppendMenuA' D$H.UserPopUp,
                                  &MF_STRING,
                                  2005,
                                  UserMenu5String

    On B$UserMenu6Path = EOS jmp S1>>

    On B$UserMenu6String = EOS jmp S1>>

        Call 'USER32.AppendMenuA' D$H.UserPopUp,
                                  &MF_STRING,
                                  2006,
                                  UserMenu6String

    On B$UserMenu7Path = EOS jmp S1>

    On B$UserMenu7String = EOS jmp S1>

        Call 'USER32.AppendMenuA' D$H.UserPopUp,
                                  &MF_STRING,
                                  2007,
                                  UserMenu7String

    On B$UserMenu8Path = EOS jmp S1>

    On B$UserMenu8String = EOS jmp S1>

        Call 'USER32.AppendMenuA' D$H.UserPopUp,
                                  &MF_STRING,
                                  2008,
                                  UserMenu8String

    On B$UserMenu9Path = EOS jmp S1>

    On B$UserMenu9String = EOS jmp S1>

        Call 'USER32.AppendMenuA' D$H.UserPopUp,
                                  &MF_STRING,
                                  2009,
                                  UserMenu9String

S1: Call 'USER32.InsertMenuA' D$H.MenuMain,
                              7,
                              &MF_BYPOSITION+&MF_STRING+&MF_POPUP,
                              D$H.UserPopUp,
                              UserPopMenu

ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; Using a Config File instead of the Registry. Implemented by Eric, October the 19th of 2006

[ConfigBinaryName: B$ 'config.bin' EOS]
[H.ConfigFile: D$ ?
 ConfigSizeReadWrite: D$ ?]
[ConfigDataSize: D$ ?]
[ConfigFilePath: B$ ? # &MAX_PATH]

SetMainConfigFilePath:

    Push esi

        Call GetDirectory ConfigFilePath

        Mov edi ConfigFilePath,
            eax 0,
            ecx 0-1

        repne scasb | dec edi

        On B$edi-1 <> '\' Mov B$edi '\'

        add edi ASCII

        Mov esi ConfigBinaryName,
            ecx 13

        rep movsb

    Pop esi

ret


WriteConfigFile:

    Push esi,
         edi

    Call 'KERNEL32.CreateFileA' ConfigFilePath,
                                &GENERIC_READ+&GENERIC_WRITE,
                                &FILE_SHARE_READ+&FILE_SHARE_WRITE,
                                &NULL,
                                &CREATE_ALWAYS,
                                &FILE_ATTRIBUTE_ARCHIVE,
                                &NULL

    Mov D$H.ConfigFile eax

    Mov esi RegistryData

L0: Call GetFieldDataSize esi

        Mov D$ConfigDataSize eax

        Call 'KERNEL32.WriteFile' D$H.ConfigFile,
                                  ConfigDataSize,
                                  (4*ASCII),
                                  ConfigSizeReadWrite,
                                  &NULL

        Call 'KERNEL32.WriteFile' D$H.ConfigFile,
                                  D$esi+(8*ASCII),
                                  D$ConfigDataSize,
                                  ConfigSizeReadWrite,
                                  &NULL

    add esi (12*ASCII) | Comp D$esi 0 > L0<

    Call 'KERNEL32.CloseHandle' D$H.ConfigFile

    Pop edi,
        esi

ret


ReadConfigFile:

    Push esi

        Call SetMainConfigFilePath

        Call 'KERNEL32.CreateFileA' ConfigFilePath,
                                    &GENERIC_READ+&GENERIC_WRITE,
                                    &FILE_SHARE_READ+&FILE_SHARE_WRITE,
                                    &NULL,
                                    &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_ARCHIVE,
                                    &NULL

        Mov D$H.ConfigFile eax

        Mov esi RegistryData

        ; The config is  structured like: [dword: sizeof data][data]
        ; So get the first dword and store it in >ConfigDataSize< :
L0:     Call 'KERNEL32.ReadFile' D$H.ConfigFile,
                                 ConfigDataSize,
                                 (4*ASCII),
                                 ConfigSizeReadWrite,
                                 &NULL

            ; Read the block of data the sizeof >ConfigDataSize< and copy into
            ; RegistryData:
            Call 'KERNEL32.ReadFile' D$H.ConfigFile,
                                     D$esi+(8*ASCII),
                                     D$ConfigDataSize,
                                     ConfigSizeReadWrite,
                                     &NULL

        add esi (12*ASCII) | Comp D$esi 0 > L0<

        Call 'KERNEL32.CloseHandle' D$H.ConfigFile

    Pop esi

ret
____________________________________________________________________________________________

;;
  When the Config.bin file is being used on an USB Memory Stick, if the user moves from
  Computer to computer, the Drive may be different, and force to re-initialize if the
  Drive Char is different.
  
  To avoid this problem, we give a try to substitute, with the actual one.
  
  'RegistryData'
;;

CheckPaths:
    Mov al B$ConfigFilePath, bl B$EquatesName

    .If al <> bl
        If D$EquatesName <> 'Equa'
            Mov B$EquatesName al
        End_If

        If D$B_U_AsmName <> 'B_U_'
            Mov B$B_U_AsmName al
        End_If

        ;If D$CalcName <> 'Calc'
        ;    On B$CalcName <> 0, Mov B$CalcName al
        ;End_If

        ;On B$F2Name <> 0, Mov B$F2Name al

        ;If D$Win32HlpName <> 'Win3'
        ;    Mov B$Win32HlpName al
        ;End_If

    .End_If
ret
____________________________________________________________________________________________

; Tag Dialog 3

Proc ConfigProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ..If D@msg = &WM_COMMAND
        and D@wParam 0FFFF

        .If D@wParam = &IDCANCEL
            Mov D$UserConfig 0FF
            Call 'USER32.EndDialog' D@hwnd, 0

        .Else_If D@wParam = &IDOK
            Call 'USER32.SendDlgItemMessageA' D@hwnd, 20, &BM_GETCHECK, 0, 0
            If eax = &TRUE
                Mov D$UserConfig REGISTRY
            Else
                Mov D$UserConfig CONFIGFILE
            End_If

            Call 'USER32.EndDialog' D@hwnd, 0
        .End_If

    ..Else_If D@msg = &WM_INITDIALOG
        Move D$H.ShowApiDialog D@hwnd
        Call 'USER32.SetClassLongA' D@hwnd, &GCL_HICON, D$STRUC.WINDOWCLASS@hIcon

        Call 'USER32.SetDlgItemTextA' D@hwnd, 10, ConfigMessage

        Call 'USER32.SendDlgItemMessageA' D@hwnd, 20, &BM_SETCHECK, 1, 0

    ..Else_If D@msg = &WM_CTLCOLOREDIT
        If B$FirstCTLCOLOREDIT = &TRUE
            Call 'USER32.SendMessageA' D@lParam, &EM_SETSEL, 0, 0
            Mov B$FirstCTLCOLOREDIT &FALSE
        End_If
        Call 'GDI32.SetBkColor' D@wParam D$ARVB.DialogsBackColor
        popad | Mov eax D$H.DialogsBackGroundBrush | jmp L9>

    ..Else
        popad | Mov eax &FALSE | jmp L9>

    ..End_If

    popad | Mov eax &TRUE

L9: EndP
EndP

____________________________________________________________________________________________

Create_Config_bin:
    Call 'USER32.MessageBoxA' D$H.MainWindow, Config.Bin_Message, Config.Bin_Title, &MB_YESNO

    If eax = &IDYES
        Call SetMainConfigFilePath
        Call WriteConfigFile
    End_If
ret

[Config.Bin_Message:
B$ "Do you want to create a 'config.bin' file in the current
Directory?

When a 'config.bin' is found aside RosAsm, this file is
used, instead of the Registry.

If you mean, later to recover the Registry functionalities,    
you will just have to delete this file" EOS]

[Config.Bin_Title: B$ 'Configuration File' EOS]
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
