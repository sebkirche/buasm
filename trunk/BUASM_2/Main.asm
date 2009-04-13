TITLE Main
 _______________________________________________________________________________________
 _______________________________________________________________________________________


[WindowX: 5  WindowY: 2  WindowW: 790  WindowH: 595
 SaveMainPosFlag: 0  IsMaximizedFlag: &SW_SHOWNORMAL]

[ListEditRect: ListERX: 0   ListERY: 0   ListERW: 0   ListERH: 0]
[WindowStyle: &WS_OVERLAPPEDWINDOW
 WindowExStyle: &WS_EX_CLIENTEDGE]


[ScrollBarWanted: &TRUE]

[FindString: 'commdlg_FindReplace' 0  FindStringMessage: 0]

[STR.A.BUAsmMutexName: B$ "BUAsmIsRuning" EOS]

[FL.MultiInstance: D$ ?]

[EditClassName: 'EDIT' 0]
 __________________________________________________________________________________

; First message structure is for the main loop (Get-Translate-DispatchMessage)
; Second is for Callback. They can't be only one because Wparam for ExitProcess is
; not the same as Wparam previously stored for CallBack.

[FirstMsg: FAdressee: ?  FuMsg: ?  FWparam: ?  FLparam: ?  FTime: ?  FPoint: ? ?]


; For mem tests:

[Meminfo: ? #20]

GetMemory:
    Call 'KERNEL32.GlobalMemoryStatus' Meminfo
    hexprint D$Meminfo+12
ret
 __________________________________


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

[IncludesOK: D$ ?
 H.RichEdit: D$ ?]
____________________________________________________________________________________________

MAIN:

    Call INIT

    Call MSG_PUMP

END:

    On D$H.MainWindow <> &NULL Call 'USER32.DestroyWindow' D$H.MainWindow

    Call 'KERNEL32.ExitProcess' D$STRUC.MSG@wParam
____________________________________________________________________________________________

[CaretTime: 600    ShowCaret: &TRUE    BlinkingCaretWanted: &FALSE]

ResetBlinkCursor:
    Call KillBlinkCursor
InitBlinkCursor:
    Call 'User32.SetTimer' D$H.MainWindow, 1, D$CaretTime, BlinkProc
ret


[CaretOnlyRedraw: ?]

BlinkProc:
    .If B$BlockInside = &FALSE
L1:     xor B$ShowCaret &TRUE
        If D$CaretRectangle+8 <> 0
            Mov B$CaretOnlyRedraw &TRUE | Call AskForRedraw
        End_If
    .Else
        On B$ShowCaret = &FALSE, jmp L1<
    .End_If
ret 16


KillBlinkCursor:
    Call 'User32.KillTimer' D$H.MainWindow, 1
    Mov B$ShowCaret &TRUE
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
; Init the Deleted Blocks Undo Folder and delete Undo Files if any:

[UndoDirectory: UndoFile: B$ ? #&MAX_PATH]
[PointerToUndoNumber: '000.'] ; Old nUndoFile
[AllUndoFiles2: B$ ? #&MAX_PATH]

;[UndoFile: 'RosAsmUndo' nUndoFile: '000.$$$' 0  ; 17 Bytes.
; AllUndoFiles: 'RosAsmUndo???.$$$' 0]
;
; The full name looks like this:
; 'E:\RosAsm3\RosAsmUndo\Undo000.$$$'
;
; 'PointerToUndoNumber' points to '000.$$$'
;
; 'AllUndoFiles', used to search for Files to be deleted, looks like this:
; 'E:\RosAsm3\RosAsmUndo\Undo???.$$$'

[UndoExist: B$ "
Block-delete Undo-files have been found in the
temporary directory (...\BUAsmUndo\).

The existing undo-files are going to be deleted and
the previous instance of BUAsm will no more be able to
undelete its saved blocks.
"

MultiUndo: "
You are runing several instances of BUAsm. Do not        
Delete/UnDelete blocks of text [Ctrl][X] / [Ctrl][Z]
the results could be unwished.

" EOS]

SetUndoDirectory:
    Mov edi UndoDirectory, ecx &MAX_PATH, al 0 | rep stosb
    Call 'KERNEL32.GetTempPathA' &MAX_PATH UndoDirectory

    Mov edi UndoDirectory, al 0, ecx &MAX_PATH | repne scasb | dec edi
    On B$edi-1 = '\', dec edi
    Mov eax '\Ros' | stosd | Mov eax 'AsmU' | stosd | Mov ax 'nd' | stosw | Mov al 'o' | stosb

    push edi
        Call 'KERNEL32.CreateDirectoryA' UndoDirectory &NULL | Mov ebx eax
    pop edi

    If D$FL.MultiInstance = &FALSE
        push edi
            Mov D$edi '\Und', D$edi+4 'o*.$', W$edi+8 '$$', B$edi+9 0
            Call 'KERNEL32.DeleteFileA' UndoDirectory
        pop edi
    End_If

    Mov eax '\Und' | stosd | Mov eax 'o001' | stosd | Mov eax '.$$$' | stosd | Mov B$edi 0
    sub edi 5
    Mov D$PointerToUndoNumber edi | sub D$PointerToUndoNumber 2

    Mov esi UndoDirectory, edi AllUndoFiles2, ecx &MAX_PATH | rep movsb
    Mov eax D$PointerToUndoNumber | sub eax UndoDirectory
    Mov edi AllUndoFiles2 | add edi eax | Mov eax '???.' | stosd

    ..If ebx = 0   ; 'CreateDirectoryA failed to create a new Dir >>> Already exist.

        Call 'KERNEL32.FindFirstFileA' AllUndoFiles2 FindFile
        .If eax <> &INVALID_HANDLE_VALUE
            If D$FL.MultiInstance = &TRUE

                Call MessageBox Argh,
                                UndoExist,
                                &MB_SYSTEMMODAL+&MB_OKCANCEL+&MB_USERICON

                On D$FL.MsgBoxReturn = &IDCANCEL jmp END

            End_If

            Call DeleteOldUndoFiles

        .Else
            Call 'KERNEL32.FindClose' eax
            If D$FL.MultiInstance = &TRUE

                Call MessageBox Argh,
                                MultiUndo,
                                &MB_SYSTEMMODAL+&MB_USERICON

            End_If

        .End_If
    ..End_If

    Mov edi D$PointerToUndoNumber, eax '000.' | stosd
ret


;;
 BackTable is used for moving back and forward in text after tree view or right clicks
 moves. It is an 8 bytes rotary table; this is to say that we use only lower byte of
 BackTablePtr to ajust moves inside table, like this:

 > Mov ebx D$BackTablePtr | add (or sub) BL 4

 As memory given by win is page aligned, if Bl = 0, "sub bl 4" points to end of table.
;;

[BackTable: ?  BackTablePtr: ?]

SetBackTableMemory:
    VirtualAlloc BackTable 0100
    move D$BackTablePtr D$BackTable
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

[ToolBarHandle: ?]
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

[toolbar00_str_00: B$ 'Tree', 0
 toolbar00_str_01: '', 0
 toolbar00_str_02: 'Open', 0
 toolbar00_str_03: 'New', 0
 toolbar00_str_04: '', 0
 toolbar00_str_05: 'Compile', 0
 toolbar00_str_06: 'Run', 0
 toolbar00_str_07: '', 0
 toolbar00_str_08: 'About ToolBar', 0
 toolbar00_str_09: '', 0
 toolbar00_str_10: 'Calc', 0
 toolbar00_str_11: 'Clip', 0
 toolbar00_str_12: 'Structures', 0
 toolbar00_str_13: 'New Dialog', 0
 toolbar00_str_14: 'Find', 0
 toolbar00_str_15: 'Replace', 0
 toolbar00_str_16: 'Ascii Table', 0
 toolbar00_str_17: 'Configuration', 0
 toolbar00_str_18: 'B_U_Asm', 0
 toolbar00_str_19: 'Print', 0
 toolbar00_str_20: 'Main Icon', 0
 toolbar00_str_21: 'Save Source',0
 toolbar00_str_22: 'Exit', 0
 toolbar00_str_23: 'Wizards', 0, 0]

 [PointersToToolTipsStrings: toolbar00_str_00   toolbar00_str_01
                              toolbar00_str_02
                             toolbar00_str_03   toolbar00_str_04
                             toolbar00_str_05
                             toolbar00_str_06   toolbar00_str_07
                              toolbar00_str_08
                             toolbar00_str_09
                            toolbar00_str_10   toolbar00_str_11
                             toolbar00_str_12   toolbar00_str_13   toolbar00_str_14
                             toolbar00_str_15   toolbar00_str_16   toolbar00_str_17
                             toolbar00_str_18   toolbar00_str_19   toolbar00_str_20
                             toolbar00_str_21   toolbar00_str_22   toolbar00_str_23]

[TOOLTIPTEXT_NMHDR_hwndFrom 0            ; With WM_NOTIFY Message, Win sends in
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

[TbRECT: TbRECTleft: ?  TbRECTtop: ?  TbRECTright: ?  TbRECTbottom: ?]
[ToolBarPixelsHight: ?    ToolBarLinesHight: ?]
[ToolBarWanted: ?]

[HelpToolBar: B$ "Sure you need some help ???!!!!" EOS]

[HelpToolBarTitle: B$ "Poor you !:" EOS]

[ToolBar_Registry:
 TB_hkr: &HKEY_CURRENT_USER
 TB_SubKey: ToolBarSubKey
 TB_ValueName: ToolBarValueName]

[ToolBarChange: B$ 0   ToolBarSubKey: 'Software\RosAsm\ToolBar' 0
                       ToolBarValueName: 'ToolBarState' 0]

CreateToolBar:
 ;&TBSTYLE_TOOLTIPS__&TBSTYLE_ALTDRAG__&CCS_ADJUSTABLE,
    Call 'COMCTL32.CreateToolbarEx' D$H.MainWindow,
    &CCS_TOP__&TBSTYLE_TOOLTIPS__&WS_CHILD__&WS_VISIBLE__&TBSTYLE_ALTDRAG__&CCS_ADJUSTABLE__&WS_BORDER,
    0300, TOOLBUTTONS_NUMBER, D$H.Instance, 2, ToolBarButtons, 9, 0, 0, 20, 20, 20

; &WS_CHILD__&WS_VISIBLE__&WS_BORDER__&CCS_TOP__&TBSTYLE_ALTDRAG__&CCS_ADJUSTABLE,

    Mov D$ToolBarHandle eax

    Call 'USER32.SendMessageA' D$ToolBarHandle, &TB_AUTOSIZE, 0, 0

    Call 'USER32.GetWindowRect' D$ToolBarHandle, TbRECT
    Mov eax D$TbRECTbottom | sub eax D$TbRECTtop | dec eax
    Mov D$ToolBarPixelsHight eax

    Call 'USER32.SendMessageA' D$ToolBarHandle, &TB_SAVERESTORE, &FALSE,  ; FALSE > restore
                               ToolBar_Registry
SaveToolBar:
    Call 'USER32.SendMessageA' D$ToolBarHandle, &TB_SAVERESTORE, &TRUE,   ; TRUE > save
                               ToolBar_Registry
ret



[StatusBarX: ?  StatusBarY: ? StatusBarW: ? StatusBarH: ?]
[StatusBarHight: ?]

CreateStatusBar:
    Call 'COMCTL32.CreateStatusWindowA',
        &WS_CHILD__&WS_VISIBLE__&WS_DLGFRAME__&WS_BORDER__&SBARS_SIZEGRIP, 0, D$H.MainWindow,
        STATUSBAR_ID
    Mov D$StatusbarHandle eax

    Call 'USER32.SendMessageA' D$StatusbarHandle, &SB_SETPARTS, 7, StatusPartsPos

    Call 'USER32.GetWindowRect' D$StatusbarHandle, StatusBarX
    push D$StatusBarY
        Call 'USER32.GetClientRect' D$H.MainWindow, StatusBarX
        Call 'USER32.ClientToScreen' D$H.MainWindow, StatusBarW
    pop ebx
    Mov eax D$StatusBarH | sub eax ebx | Mov D$StatusBarHight eax
    Call 'USER32.ShowWindow' D$StatusBarHandle &SW_SHOW
ret


[SbRECT: ScrollBarX: ?  ScrollBarY: ?  ScrollBarW: ?  ScrollBarH: ?]
[ScrollWindowHandle: ?]
[ScrollBarName: 'SCROLLBAR', 0]

CreateScrollBar:
    Call 'USER32.GetClientRect' D$H.MainWindow ScrollBarX
    push D$ScrollBarW
        If B$ToolBarWanted = &TRUE
            Mov eax D$ToolBarPixelsHight | add D$ScrollBarY eax | sub D$ScrollBarH eax
        End_If
        Call 'USER32.GetSystemMetrics' &SM_CXVSCROLL | Mov D$ScrollBarW eax
    pop ebx | sub ebx eax | Mov D$ScrollBarX ebx
    Mov eax D$StatusBarHight | sub D$ScrollBarH eax

    Call 'USER32.CreateWindowExA' 0, EditClassName, &NULL,
                                  &WS_CHILD__&WS_VISIBLE__&WS_VSCROLL,
                                  D$ScrollBarX, D$ScrollBarY, D$ScrollBarW, D$ScrollBarH,
                                  D$H.MainWindow, &NULL, D$H.Instance, 0
    Mov D$ScrollWindowHandle eax

    Call 'USER32.SetWindowLongA' D$ScrollWindowHandle, &GWL_WNDPROC, ScrollBarProc
ret


[EditWindowHandle: ?   BpWindowHandle: ?
 EditWindowX: ? EditWindowY: ? EditWindowW: EditWindowX2: ? EditWindowH: EditWindowY2: ?]

[BpMarginWidth: ?]

CreateEditWindow:
    Mov eax D$FontWidth | shl eax 1 | Mov D$BpMarginWidth eax

    Call 'USER32.GetClientRect' D$H.MainWindow EditWindowX
    Mov eax D$BpMarginWidth | add D$EditWindowX eax

    If B$ToolBarWanted = &TRUE
        Mov eax D$ToolBarPixelsHight
        add D$EditWindowY eax
        sub D$EditWindowH eax
    End_If

    If B$ScrollBarWanted = &TRUE
        Mov eax D$ScrollBarW | sub D$EditWindowW eax
    End_If

    Mov eax D$StatusBarHight | sub D$EditWindowH eax
    Mov eax D$BpMarginWidth | sub D$EditWindowW eax

    Call 'USER32.CreateWindowExA' 0, STR.A.WindowClassMain, &NULL,
                                  &WS_CHILD__&WS_VISIBLE,
                                  D$EditWindowX, D$EditWindowY, D$EditWindowW, D$EditWindowH,
                                  D$H.MainWindow, &NULL, D$H.Instance, 0
    Mov D$EditWindowHandle eax

  ; Prepare the Client area for printing:
    ;Call 'USER32.GetClientRect' D$EditWindowHandle, EditWindowX

  ; Create the BP Margin Window:
    Call 'USER32.CreateWindowExA' 0, STR.A.WindowClassMain, &NULL,
                                  &WS_CHILD__&WS_VISIBLE,
                                  0, D$EditWindowY, D$EditWindowX, D$EditWindowH,
                                  D$H.MainWindow, &NULL, D$H.Instance, 0
    Mov D$BpWindowHandle eax
ret
____________________________________________________________________________________________

[AraseBackEdit: ? #4]

Proc AraseBackGround:
    Argument @hdc
    Call 'USER32.GetClientRect' D$EditWindowHandle, AraseBackEdit
    Call 'USER32.FillRect' D@hdc, AraseBackEdit, D$H.BackGroundBrush

    Call AskForRedraw
EndP


MainResize:
  ; StatusBar:
    Call 'USER32.GetClientRect' D$H.MainWindow, StatusBarX

    Mov eax D$StatusBarH | sub eax D$StatusBarHight | Mov D$StatusBarY eax
    Call 'USER32.MoveWindow' D$StatusBarHandle,
                             D$StatusBarX, D$StatusBarY, D$StatusBarW, D$StatusBarH,
                             &TRUE

  ; ToolBar:
    If B$ToolBarWanted = &TRUE
        Call 'USER32.SendMessageA' D$ToolBarHandle, &TB_AUTOSIZE, 0, 0
    End_If

  ; ScrollBar:
    .If B$ScrollBarWanted = &TRUE
        Call 'USER32.GetClientRect' D$H.MainWindow ScrollBarX
        push D$ScrollBarW
            If B$ToolBarWanted = &TRUE
                Mov eax D$ToolBarPixelsHight | add D$ScrollBarY eax | sub D$ScrollBarH eax
            End_If
            Call 'USER32.GetSystemMetrics' &SM_CXVSCROLL | Mov D$ScrollBarW eax
        pop ebx | sub ebx eax | Mov D$ScrollBarX ebx
        Mov eax D$StatusBarHight | sub D$ScrollBarH eax

        Call 'USER32.MoveWindow' D$ScrollWindowHandle,
                                 D$ScrollBarX, D$ScrollBarY, D$ScrollBarW, D$ScrollBarH,
                                 &TRUE
    .End_If

  ; Edit:
    Mov eax D$FontWidth | shl eax 1 | Mov D$BpMarginWidth eax

    Call 'USER32.GetClientRect' D$H.MainWindow EditWindowX
    Mov eax D$BpMarginWidth | add D$EditWindowX eax

    If B$ToolBarWanted = &TRUE
        Mov eax D$ToolBarPixelsHight
        add D$EditWindowY eax
        sub D$EditWindowH eax
    End_If

    If B$ScrollBarWanted = &TRUE
        Mov eax D$ScrollBarW | sub D$EditWindowW eax
    End_If

    Mov eax D$StatusBarHight | sub D$EditWindowH eax
    Mov eax D$BpMarginWidth | sub D$EditWindowW eax


;If D$TitleFontHandle <> 0
;    Mov eax D$TitleTabHight | sub D$EditWindowH eax
;End_If

    Call 'USER32.MoveWindow' D$EditWindowHandle,
                             D$EditWindowX, D$EditWindowY, D$EditWindowW, D$EditWindowH,
                             &TRUE
  ; Initialize the Edit Window dims as used by the Editor:
    ;Call 'USER32.GetClientRect' D$EditWindowHandle, EditWindowX
    ;mov eax D$BpMarginWidth | add D$EditWindowX eax

  ; Bp Margin:
    Call 'USER32.MoveWindow' D$BpWindowHandle,
                             0, D$EditWindowY, D$BpMarginWidth, D$EditWindowH,
                             &TRUE
ret


RedrawInterface:
  ; Use of 'ScrollBarProc' for killing the various windows because this Proc.
  ; (As MainWindowProc would close RosAsm)

  ; Edit:
    Call 'USER32.SetWindowLongA' D$EditWindowHandle, &GWL_WNDPROC, ScrollBarProc
    Call 'USER32.DestroyWindow' D$EditWindowHandle

  ; ScrollBar
    If D$ScrollWindowHandle <> 0
        Call 'USER32.DestroyWindow' D$ScrollWindowHandle
        Mov D$ScrollWindowHandle 0
    End_If

  ; ToolBar:
    If D$ToolBarHandle <> 0
        Call 'USER32.SetWindowLongA' D$ToolBarHandle, &GWL_WNDPROC, ScrollBarProc
        Call 'USER32.DestroyWindow' D$ToolBarHandle
        Mov D$ToolBarHandle 0
    End_If

  ; StatusBar:
    Call 'USER32.SetWindowLongA' D$StatusbarHandle, &GWL_WNDPROC, ScrollBarProc
    Call 'USER32.DestroyWindow' D$StatusbarHandle

  ; BpWindow
    Call 'USER32.SetWindowLongA' D$BpWindowHandle, &GWL_WNDPROC, ScrollBarProc
    Call 'USER32.DestroyWindow' D$BpWindowHandle

    Call CreateStatusBar
    On B$ToolBarWanted = &TRUE,  Call CreateToolBar
    On B$ScrollBarWanted = &TRUE, Call CreateScrollBar
    Call CreateEditWindow
ret












