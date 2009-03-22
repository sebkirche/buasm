TITLE MAIN            ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

[STRUC.RECT.MainWindow:
 WindowX: D$ 5
 WindowY: D$ 2
 WindowW: D$ 790
 WindowH: D$ 595]

[FL.SaveMainPos: D$ ?]

[IsMaximizedFlag: &SW_SHOWNORMAL]

[ListEditRect:
 ListERX: D$ ?
 ListERY: D$ ?
 ListERW: D$ ?
 ListERH: D$ ?]

[H.MenuMain: D$?]

[FL.ScrollBarWanted: D$ ?]

[FindString: B$ 'commdlg_FindReplace' EOS]
[FindStringMessage: D$ ?]

[FL.MultiInstance: D$ ?]

[STR.A.BUAWindowClass: B$ 'BUAWindowClass' EOS]
[STR.A.EditWindowClassName: B$ 'EDIT' EOS]

[STRUC.WINDOWCLASS:
 @style: D$ &CS_BYTEALIGNCLIENT+&CS_BYTEALIGNWINDOW+&CS_PARENTDC+&CS_HREDRAW+&CS_VREDRAW+&CS_DBLCLKS
 @lpfnWndProc: D$ MainWindowProc
 @cbClsExtra: D$ 0
 @cbWndExtra: D$ 0
 @hInstance: D$ &NULL
 @hIcon: D$ &NULL
 @hCursor: D$ &NULL
 @hbrBackground: D$ &NULL
 @lpszMenuName: D$ &NULL
 @lpszClassName: D$ STR.A.BUAWindowClass]

[H.CursorARROW: D$ ?
 H.CurrentCursor: D$ ?
 H.CursorWAIT: D$ ?]
 __________________________________________________________________________________

; First message structure is for the main loop (Get-Translate-DispatchMessage)
; Second is for Callback. They can't be only one because Wparam for ExitProcess is
; not the same as Wparam previously stored for CallBack.

[STRUC.MSG:
 @hwnd: D$ ?
 @message: D$ ?
 @wParam: D$ ?
 @lParam: D$ ?
 @time: D$ ?
 @STRUC.pt: D$ ? ?]

[H.Instance: D$ ?
 H.MainWindow: D$ ?]
____________________________________________________________________________________________

; For mem tests:

[UserHaveClickDown: B$ 0]

; (UserHaveClickDown flag is to prevent block drawing when back from 'OpenFileName' api
 __________________________________

; Accelerators:

; [DRAWLINE 628]

;;
ACCELNUMBER 2    FLAGLAST 080]

;  ;  01  070  03E8  00    ;03E8 > about ID  (F1)
;  ;  01  071  03F4  00    ;03F4 > Save ID   (F2)
;  ;  01  072  03EC  00    ;03EC > Open ID   (F3)
;  ; 090  078  03ED  00    ;03ED > Exit ID   (Alt-X)  78 > 'x'

; &FCONTROL  &FNOINVERT  &FSHIFT  &FVIRTKEY  &FALT

[ACCELERATORS:
 U$ &FVIRTKEY__&FNOINVERT                        &VK_F1    M00_RosAsm
    &FVIRTKEY__&FCONTROL__&FNOINVERT+FLAGLAST    &VK_8     DRAWLINE]
;;

[FL.Includes: D$ ?
 H.RichEdit: D$ ?]

MAIN:

    Call INIT


  ;  On D$StringsLanguage <> '.en' Call OpenStringsFile

    Call EnableMenutems | Call EnableHelpMenutems
    Call EnableVisualTutsMenu | Call EnableWizardsMenu
    Call EnableClipMenu

    Call CreateStatusBar
    On D$FL.ToolBarWanted = &TRUE, Call CreateToolBar
    On D$FL.ScrollBarWanted = &TRUE, Call CreateScrollBar
    Call CreateEditWindow
    Call CreateTitleTab

  ; (D$IsMaximizedFlag = &SW_SHOWNORMAL or &SW_MAXIMIZE):
    Call 'USER32.ShowWindow'  D$H.MainWindow,
                              D$IsMaximizedFlag

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

    On D$FL.BlinkingCaretWanted = &TRUE Call InitBlinkCursor

    Call InitExpressionBuffers

;    Call 'USER32.CreateAcceleratorTableA' ACCELERATORS ACCELNUMBER
;    Mov D$AccelHandle eax

    jmp L1>>
   ___________________________________________________________________________

   ; Our main loop: when 'DispatchMessageA' called, Win calls upper CallBack:
   ___________________________________________________________________________

L0: Call 'USER32.IsDialogMessageA' D$H.FindReplace STRUC.MSG  | On eax > 0, jmp L1>

    If D$FL.IsDebugging = &TRUE
        Call 'USER32.TranslateAcceleratorA' D$H.DebugDialog, D$H.DbgAccel, STRUC.MSG
        cmp eax &TRUE | je L1>
        Call 'USER32.IsDialogMessageA' D$H.CurrentDataPage, STRUC.MSG
        cmp eax &TRUE | je L1>
        Call 'USER32.IsDialogMessageA' D$H.DebugDialog, STRUC.MSG
        cmp eax &TRUE | je L1>
    End_If

    Call 'USER32.TranslateMessage' STRUC.MSG
    Call 'USER32.DispatchMessageA' STRUC.MSG

L1: Call 'USER32.GetMessageA' STRUC.MSG 0 0 0

    cmp eax 0 | ja L0<<

  ; Call ReleaseFonts
    Call UpdateRegistry

    Call 'KERNEL32.FreeLibrary' D$H.RichEdit

  ; Call 'USER32.DestroyAcceleratorTable' D$AccelHandle

    Call 'KERNEL32.ExitProcess' D$STRUC.MSG@wParam
____________________________________________________________________________________________

[ID_TIMER_CURSOR 1]

[DU.CaretTime: D$ ? ;600
 FL.ShowCaret: D$ ? ;&TRUE
 FL.BlinkingCaretWanted: D$ ?]

ResetBlinkCursor:

    Call KillBlinkCursor

InitBlinkCursor:

    Call 'USER32.SetTimer' D$H.MainWindow,
                           ID_TIMER_CURSOR,
                           D$DU.CaretTime,
                           TimerCusorProc

ret
____________________________________________________________________________________________

Proc TimerCusorProc:

    Arguments @hwnd,
              @msg,
              @idEvent,
              @dwTime

    xor D$FL.ShowCaret &TRUE | Call AskForRedraw

EndP
____________________________________________________________________________________________

KillBlinkCursor:

    Call 'USER32.KillTimer' D$H.MainWindow,
                            ID_TIMER_CURSOR

    Mov D$FL.ShowCaret &TRUE

ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; Init the Deleted Blocks Undo Folder and delete Undo Files if any:

[UndoDirectory: UndoFile: B$ ? # &MAX_PATH]
[PointerToUndoNumber: D$ '000.'] ; Old nUndoFile
[AllUndoFiles2: B$ ? # &MAX_PATH]

;[UndoFile: 'RosAsmUndo' nUndoFile: '000.$$$' 0  ; 17 Bytes.
; AllUndoFiles: B$ 'RosAsmUndo???.$$$' 0]
;
; The full name looks like this:
; 'E:\RosAsm3\RosAsmUndo\Undo000.$$$'
;
; 'PointerToUndoNumber' points to '000.$$$'
;
; 'AllUndoFiles', used to search for Files to be deleted, looks like this:
; 'E:\RosAsm3\RosAsmUndo\Undo???.$$$'

[UndoExist: B$ "
Block-Delete Undo-Files have been found in the
Temporary Directory (...\RosAsmUndo\).

The existing Undo-Files are going to be deleted and
the previous instance of RosAsm will no more be able to
UnDelete its saved Blocks.
"

MultiUndo: B$ "
You are runing several instances of RosAsm. Do not        
Delete/UnDelete Blocks of text [Ctrl][X] / [Ctrl][Z]
The results could be unwished.

" EOS]

SetUndoDirectory:

    Call ClearPATH UndoDirectory

    Call 'KERNEL32.GetTempPathA' &MAX_PATH,
                                 UndoDirectory

    Mov edi UndoDirectory,
         al EOS,
         ecx &MAX_PATH

    repne scasb | sub edi (1*ASCII)

    On B$edi-1 = '\' sub edi (1*ASCII)

    Mov eax '\Ros' | stosd

    Mov eax 'AsmU' | stosd

    Mov ax 'nd' | stosw

    Mov al 'o' | stosb

    Call 'KERNEL32.CreateDirectoryA' UndoDirectory,
                                     &NULL

    Mov ebx eax

    If D$FL.MultiInstance = &FALSE

        Mov D$edi '\Und',
            D$edi+(4*ASCII) 'o*.$',
            W$edi+(8*ASCII) '$$',
            B$edi+(9*ASCII) EOS

        Call 'KERNEL32.DeleteFileA' UndoDirectory

    End_If

    Mov eax '\Und' | stosd

    Mov eax 'o001' | stosd

    Mov eax '.$$$' | stosd

    Mov B$edi EOS

    sub edi (5*ASCII)

    Mov D$PointerToUndoNumber edi | sub D$PointerToUndoNumber (2*ASCII)

    Mov esi UndoDirectory,
        edi AllUndoFiles2,
        ecx (&MAX_PATH/DWORD)

    rep movsd

    Mov eax D$PointerToUndoNumber | sub eax UndoDirectory

    Mov edi AllUndoFiles2 | add edi eax | Mov eax '???.' | stosd

    ..If ebx = 0   ; 'CreateDirectoryA failed to create a new Dir >>> Already exist.

        Call 'KERNEL32.FindFirstFileA' AllUndoFiles2,
                                       STRUC.FindFile

        .If eax <> &INVALID_HANDLE_VALUE

            If D$FL.MultiInstance = &TRUE

                Call 'USER32.MessageBoxA' D$H.MainWindow,
                                          UndoExist,
                                          Argh,
                                          &MB_OKCANCEL+&MB_ICONHAND

                On eax = &IDCANCEL Call 'KERNEL32.ExitProcess' 0

            End_If

            Call DeleteOldUndoFiles

        .Else

            Call 'KERNEL32.FindClose' eax

            If D$FL.MultiInstance = &TRUE

                Call 'USER32.MessageBoxA' D$H.MainWindow,
                                          MultiUndo,
                                          Argh,
                                          &MB_OK

            End_If

        .End_If

    ..End_If

    Mov edi D$PointerToUndoNumber,
        eax '000.'

    stosd

ret


;;
 
    BackTable is used for moving back and forward in text after tree view or right clicks
    moves. It is an 8 bytes rotary table; this is to say that we use only lower byte of
    BackTablePtr to ajust moves inside table, like this:

    Mov ebx D$BackTablePtr | add (or sub) bl 4

    As memory given by win is page aligned, if bl = 0, "sub bl 4" points to end of table.
;;

[BackTable: D$ ?
 BackTablePtr: D$ ?]

SetBackTableMemory:

    Call VirtualAlloc BackTable,
                      0100

    Move D$BackTablePtr D$BackTable

ret

____________________________________________________________________________________________
____________________________________________________________________________________________

[H.ToolBar: D$ ?]
[TOOLBUTTONS_NUMBER 18] ; (zero based).

[ToolBarButtons:
 D$  0  M00_Tree  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$  0 0 B$ &TBSTATE_ENABLED  &TBSTYLE_SEP 0 0  D$ 0 0
 D$  1  M00_Open  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$  2  M00_New  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 0 0 B$ &TBSTATE_ENABLED  &TBSTYLE_SEP 0 0  D$ 0 0
 D$  3  M00_Compile  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$  4  M00_Run  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 0 0 B$ &TBSTATE_ENABLED  &TBSTYLE_SEP 0 0  D$ 0 0
 D$ 14  M00_About_ToolBar  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0

 D$ 0 0 B$ &TBSTATE_ENABLED  &TBSTYLE_SEP 0 0  D$ 0 0
 D$  5  M00_Calc  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$  6  M00_Clip_File  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$  7  M00_Structures  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$  8  M00_New_Dialog   B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$  9  M00_Find  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 10  M00_Replace  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 11  M00_Ascii_Table   B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 12  M00_Configuration   B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 13  M00_B_U_Asm  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
; D$ 14  M00_About   B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 15  M00_Print   B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 16  M00_Main_Icon   B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 17  M00_Save_Source_Only  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 18  M00_Exit  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 19 12  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
; D$ 20 0CE  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
; D$ 21 0CE  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
; D$ 22 0CE  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 ]

[toolbar00_str_00: B$ 'Tree' EOS]
[toolbar00_str_01: B$ '' EOS]
[toolbar00_str_02: B$ 'Open' EOS]
[toolbar00_str_03: B$ 'New' EOS]
[toolbar00_str_04: B$ '' EOS]
[toolbar00_str_05: B$ 'Compile' EOS]
[toolbar00_str_06: B$ 'Run' EOS]
[toolbar00_str_07: B$ '' EOS]
[toolbar00_str_08: B$ 'About ToolBar' EOS]
[toolbar00_str_09: B$ '' EOS]
[toolbar00_str_10: B$ 'Calc' EOS]
[toolbar00_str_11: B$ 'Clip' EOS]
[toolbar00_str_12: B$ 'Structures' EOS]
[toolbar00_str_13: B$ 'New Dialog' EOS]
[toolbar00_str_14: B$ 'Find' EOS]
[toolbar00_str_15: B$ 'Replace' EOS]
[toolbar00_str_16: B$ 'Ascii Table' EOS]
[toolbar00_str_17: B$ 'Configuration' EOS]
[toolbar00_str_18: B$ 'B_U_Asm' EOS]
[toolbar00_str_19: B$ 'Print' EOS]
[toolbar00_str_20: B$ 'Main Icon' EOS]
[toolbar00_str_21: B$ 'Save Source' EOS]
[toolbar00_str_22: B$ 'Exit' EOS]
[toolbar00_str_23: B$ 'Wizards' EOS 0]

[PointersToToolTipsStrings:
 toolbar00_str_00
 toolbar00_str_01
 toolbar00_str_02
 toolbar00_str_03
 toolbar00_str_04
 toolbar00_str_05
 toolbar00_str_06
 toolbar00_str_07
 toolbar00_str_08
 toolbar00_str_09
 toolbar00_str_10
 toolbar00_str_11
 toolbar00_str_12
 toolbar00_str_13
 toolbar00_str_14
 toolbar00_str_15
 toolbar00_str_16
 toolbar00_str_17
 toolbar00_str_18
 toolbar00_str_19
 toolbar00_str_20
 toolbar00_str_21
 toolbar00_str_22
 toolbar00_str_23]

[TOOLTIPTEXT_NMHDR_hwndFrom 0          ; With WM_NOTIFY Message, Win sends in
 TOOLTIPTEXT_NMHDR_idfrom   4          ; lParam a pointer to a TOOLTIPTEXT
 TOOLTIPTEXT_NMHDR_code     8          ; Structure (which win GIVES to us).

 TOOLTIPTEXT_lpszText       12         ; Item
 TOOLTIPTEXT_szText         16         ; These Equates are to ease accesses to
 TOOLTIPTEXT_hInst          96         ; this Structure (which doesn't belong
 TOOLTIPTEXT_uFlags         100]       ; to our Data area).


[TB_NOTIFY_NMHDR_hwndFrom     0          ; And same for "TBN_GETBUTTONINFO" answers:
 TB_NOTIFY_NMHDR__idfrom      4
 TB_NOTIFY_NMHDR__code        8

 TB_NOTIFY_Item      12

 TB_NOTIFY_TBBUTTON_iBitmap   16
 TB_NOTIFY_TBBUTTON_idCommand 20
 TB_NOTIFY_TBBUTTON_fsState   24
 TB_NOTIFY_TBBUTTON_fsStyle   23
 TB_NOTIFY_TBBUTTON_dwData    28
 TB_NOTIFY_TBBUTTON_iString   32

 TB_NOTIFY_CharCount 36
 TB_NOTIFY_TextPtr   40]


[NMMOUSE_hdr        00
 NMMOUSE_ItemSpec   04
 NMMOUSE_ItemData   08 ; Lp !!!
 NMMOUSE_pt         12
 NMMOUSE_HitInfo    20]

[TbRECT:
 @left: D$ ?
 @top: D$ ?
 @right: D$ ?
 @bottom: D$ ?]

[ToolBarPixelsHight: D$ ??]

[FL.ToolBarWanted: D$ ?]

[HelpToolBar: B$ 'Sure you need some help???!!!!' EOS]
[HelpToolBarTitle: B$ 'Poor you!' EOS]

[ToolBar_Registry:
 @TB_hkr: &HKEY_CURRENT_USER
 @TB_SubKey: ToolBarSubKey
 @TB_ValueName: ToolBarValueName]

[ToolBarChange: B$ 0]
[ToolBarSubKey: B$ 'Software\RosAsm\ToolBar' EOS]
[ToolBarValueName: B$ 'ToolBarState' EOS]

CreateToolBar:
 ;&TBSTYLE_TOOLTIPS__&TBSTYLE_ALTDRAG__&CCS_ADJUSTABLE,
    Call 'COMCTL32.CreateToolbarEx' D$H.MainWindow,
    &CCS_TOP__&TBSTYLE_TOOLTIPS__&WS_CHILD__&WS_VISIBLE__&TBSTYLE_ALTDRAG__&CCS_ADJUSTABLE__&WS_BORDER,
    0300, TOOLBUTTONS_NUMBER, D$H.Instance, 2, ToolBarButtons, 9, 0, 0, 20, 20, 20

; &WS_CHILD__&WS_VISIBLE__&WS_BORDER__&CCS_TOP__&TBSTYLE_ALTDRAG__&CCS_ADJUSTABLE,

    Mov D$H.ToolBar eax

    Call 'USER32.SendMessageA' D$H.ToolBar, &TB_AUTOSIZE, 0, 0

    Call 'USER32.GetWindowRect' D$H.ToolBar, TbRECT
    Mov eax D$TbRECT@bottom | sub eax D$TbRECT@top | dec eax
    Mov D$ToolBarPixelsHight eax

    Call 'USER32.SendMessageA' D$H.ToolBar, &TB_SAVERESTORE, &FALSE,  ; FALSE > restore
                               ToolBar_Registry
SaveToolBar:
    Call 'USER32.SendMessageA' D$H.ToolBar, &TB_SAVERESTORE, &TRUE,   ; TRUE > save
                               ToolBar_Registry
ret

[STRUC.RECT.StatusBar:
 D$ ?
 D$ ?
 D$ ?
 D$ ?]

[DU.StatusBarHight: D$ ?]

CreateStatusBar:

    Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  {B$ 'msctls_statusbar32' EOS},
                                  &NULL,
                                  &WS_CHILD+&WS_VISIBLE+&SBARS_SIZEGRIP,
                                  0,
                                  0,
                                  0,
                                  0,
                                  D$H.MainWindow,
                                  STATUSBAR_ID,
                                  D$H.Instance,
                                  0

    Mov D$H.StatusBar eax

    Call 'USER32.SendMessageA' D$H.StatusBar,
                               &SB_SETPARTS,
                               11,
                               TABLE.StatusPartsPos

    Call 'USER32.GetWindowRect' D$H.StatusBar,
                                STRUC.RECT.StatusBar

    Push D$STRUC.RECT.StatusBar+TOP

        Call 'USER32.GetClientRect' D$H.MainWindow,
                                    STRUC.RECT.StatusBar

        Call 'USER32.ClientToScreen' D$H.MainWindow,
                                     STRUC.RECT.StatusBar+RIGHT

    Pop edx

    Mov eax D$STRUC.RECT.StatusBar+BOTTOM | sub eax edx | Mov D$DU.StatusBarHight eax





ret

ResizeStatusBar:

    ; StatusBar:
    Call 'USER32.GetClientRect' D$H.MainWindow,
                                STRUC.RECT.StatusBar

    Mov eax D$STRUC.RECT.StatusBar+BOTTOM | sub eax D$DU.StatusBarHight | Mov D$STRUC.RECT.StatusBar+TOP eax

    Call 'USER32.MoveWindow' D$H.StatusBar,
                             D$STRUC.RECT.StatusBar+LEFT,
                             D$STRUC.RECT.StatusBar+TOP,
                             D$STRUC.RECT.StatusBar+RIGHT,
                             D$STRUC.RECT.StatusBar+BOTTOM,
                             &FALSE

ret

[STRUC.RECT.ScrollBarWindow:
 D$ ?
 D$ ?
 D$ ?
 D$ ?]

[H.ScrollBarWindow: D$ ?]

CreateScrollBar:

    Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  STR.A.EditWindowClassName,
                                  &NULL,
                                  &WS_VISIBLE+&WS_CHILD+&WS_VSCROLL,
                                  0,
                                  0,
                                  0,
                                  0,
                                  D$H.MainWindow,
                                  &NULL,
                                  D$H.Instance,
                                  &NULL
    Mov D$H.ScrollBarWindow eax

    Call 'USER32.SetWindowLongA' D$H.ScrollBarWindow,
                                 &GWL_WNDPROC,
                                 ScrollBarProc

ResizeScrollBar:

    Call 'USER32.GetClientRect' D$H.MainWindow,
                                STRUC.RECT.ScrollBarWindow

    If D$FL.ToolBarWanted = &TRUE

        Mov eax D$ToolBarPixelsHight | add D$STRUC.RECT.ScrollBarWindow+TOP eax | sub D$STRUC.RECT.ScrollBarWindow+BOTTOM eax

    End_If

    Call 'USER32.GetSystemMetrics' &SM_CXVSCROLL

    Mov edx D$STRUC.RECT.ScrollBarWindow+RIGHT, D$STRUC.RECT.ScrollBarWindow+RIGHT eax

    sub edx eax | Mov D$STRUC.RECT.ScrollBarWindow+LEFT edx

    Mov eax D$DU.StatusBarHight | sub D$STRUC.RECT.ScrollBarWindow+BOTTOM eax

    Call 'USER32.MoveWindow' D$H.ScrollBarWindow,
                             D$STRUC.RECT.ScrollBarWindow+LEFT,
                             D$STRUC.RECT.ScrollBarWindow+TOP,
                             D$STRUC.RECT.ScrollBarWindow+RIGHT,
                             D$STRUC.RECT.ScrollBarWindow+BOTTOM,
                             &TRUE

ret


[H.EditWindow: D$ ?
 H.BreakPointWindow: D$ ?]

[STRUC.RECT.EditWindow:
 D$ ?
 D$ ?
 D$ ?
 D$ ?]

[DU.BreakPointWindowMarginWidth: D$ ?]

CreateEditWindow:

    Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  STR.A.BUAWindowClass,
                                  &NULL,
                                  &WS_CHILD+&WS_VISIBLE,
                                  0, ;D$STRUC.RECT.EditWindow+LEFT,
                                  0, ;D$STRUC.RECT.EditWindow+TOP,
                                  0, ;D$STRUC.RECT.EditWindow+RIGHT,
                                  0, ;D$STRUC.RECT.EditWindow+BOTTOM,
                                  D$H.MainWindow,
                                  &NULL,
                                  D$H.Instance,
                                  &NULL
    Mov D$H.EditWindow eax

    ; Create the BreakPoint Margin Window:
    Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  STR.A.BUAWindowClass,
                                  &NULL,
                                  &WS_CHILD+&WS_VISIBLE,
                                  0,
                                  0, ;D$STRUC.RECT.EditWindow+TOP,
                                  0, ;D$STRUC.RECT.EditWindow+LEFT,
                                  0, ;D$STRUC.RECT.EditWindow+BOTTOM,
                                  D$H.MainWindow,
                                  &NULL,
                                  D$H.Instance,
                                  &NULL
    Mov D$H.BreakPointWindow eax

ResizeEditWindow:

    Mov eax D$FontWidth | shl eax 1 | Mov D$DU.BreakPointWindowMarginWidth eax

    Call 'USER32.GetClientRect' D$H.MainWindow,
                                STRUC.RECT.EditWindow

    Mov eax D$DU.BreakPointWindowMarginWidth | add D$STRUC.RECT.EditWindow+LEFT eax

    If D$FL.ToolBarWanted = &TRUE

        Mov eax D$ToolBarPixelsHight | add D$STRUC.RECT.EditWindow+TOP eax

        sub D$STRUC.RECT.EditWindow+BOTTOM eax

    End_If

    If D$FL.ScrollBarWanted = &TRUE

        Mov eax D$STRUC.RECT.ScrollBarWindow+RIGHT | sub D$STRUC.RECT.EditWindow+RIGHT eax

    End_If

    Mov eax D$DU.StatusBarHight | sub D$STRUC.RECT.EditWindow+BOTTOM eax

    Mov eax D$DU.BreakPointWindowMarginWidth | sub D$STRUC.RECT.EditWindow+RIGHT eax

    Call 'USER32.MoveWindow' D$H.EditWindow,
                             D$STRUC.RECT.EditWindow+LEFT,
                             D$STRUC.RECT.EditWindow+TOP,
                             D$STRUC.RECT.EditWindow+RIGHT,
                             D$STRUC.RECT.EditWindow+BOTTOM,
                             &TRUE

    Call 'USER32.MoveWindow' D$H.BreakPointWindow ,
                             0,
                             D$STRUC.RECT.EditWindow+TOP,
                             D$STRUC.RECT.EditWindow+LEFT,
                             D$STRUC.RECT.EditWindow+BOTTOM,
                             &TRUE

ret
____________________________________________________________________________________________

MainResize:

    ; StatusBar:
    Call ResizeStatusBar

    ; ToolBar:
    On D$FL.ToolBarWanted = &TRUE Call 'USER32.SendMessageA' D$H.ToolBar,
                                                             &TB_AUTOSIZE,
                                                             &NULL,
                                                             &NULL
    ; ScrollBar:
    On D$FL.ScrollBarWanted = &TRUE Call ResizeScrollBar

    Call ResizeEditWindow

    ; TabWindow (TITLEs)
    Call TabResize

ret


RedrawInterface:
    ____________________________________________________________________________

    ; TODO traduction
    ; MAJ de l'interface selon les changements d'options (ToolBar/ScrollBar...)
    ____________________________________________________________________________

    Call DestroyWindow H.ToolBar

    On D$FL.ToolBarWanted = &TRUE Call CreateToolBar

    Call DestroyWindow H.ScrollBarWindow

    On D$FL.ScrollBarWanted = &TRUE Call CreateScrollBar

    Call DestroyWindow H.BreakPointWindow

    Call DestroyWindow H.EditWindow

    Call CreateEditWindow

ret


Proc DestroyWindow:

;;
    
    TODO traduction

    Il faut utiliser une procédure indépendante de la MainWindowProc pour détruire ses
    fenêtres filles ; dans le cas contraire la MainWindow serait elle-même détruite

;;
    ____________________________________________________

    ; TODO traduction
    ; Destruction de la fenêtre hwnd et RAZ de son hwnd
    ____________________________________________________

    Argument @LP.Handle

    Uses ebx

    Mov ebx D@LP.Handle

    ; Attribution de la nouvelle procédure de traitement
    Call 'USER32.SetWindowLongA' D$ebx,
                                 &GWL_WNDPROC,
                                 DeleteProc

    ; Destrution de la fenêtre
    Call 'USER32.DestroyWindow' D$ebx

    ; RAZ du Lp -> Fenêtre
    Mov D$ebx &NULL

EndP


Proc DeleteProc:
    _________________________________________________________

    ; TODO traduction
    ; Procédure réservée à la destruction de fenêtres filles
    _________________________________________________________

    Arguments @hwnd,
              @msg,
              @wParam,
              @lParam

    Call 'USER32.DefWindowProcA' D@hwnd,
                                 D@msg,
                                 D@wParam,
                                 D@lParam

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
