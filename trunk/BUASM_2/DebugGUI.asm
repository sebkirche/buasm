TITLE DebugGUI

;;
_____________________________________________________________________________________________

                                  Debugger GUI - Ludwig Haehne
 
 Split from the debugger title for better readability.
   
 TODO:

    * Save upper & lower tab selection
    * Memory inspector address edit should handle expressions (mouse hint code reuse)    
    * Hints for toolbar buttons    
    * Context menu in address space tree (?)
    * Next instruction indicator in source editor    
    * Separate the debug dialog from the main window (but stay on top)
    * Log Exceptions (also auto-forwarded)
    * Save to file for log (from context menu)
    * Watch tab (expression-entry; add,remove&clear button; list/tree view)
    * Disassembly tab
    * Restore defaults (set flag to delete debug.cfg at the end)
    * Recover toolbar when all bands are hidden
    * Nice FPU box
    * Nice CPU Info
    * Review code address form
    * Q$ / X$ / U$ (+string) support in mousehint
    * Sane menu/combo font (registry?) (test under wine)    
    * Address Space Tree vs. Nessie
____________________________________________________________________________________________

 Paragraphs
  
  * 'CreateDebugWindow' 'DebuggerMENU' 'DebuggerTOOLBAR' 'DebuggerMOUSEHINT' 
    'DebuggerSTRINGS'
  
 Dialogs
 
  * 'DebugDlgProc', 'DataViewProc', 'MemoryInspectorProc', 'PageTableProc', 'CallStackProc',
    'MouseOverDataHint', 'ExceptionInfoProc', 'AddressSpaceFormProc', 'LogFormProc'
 
 Window handling
 
  * 'AdjustControlSize', 'AdjustControlPos', 'SelectTab', 'ReportWinError'
  
 Misc routines
 
  * 'SourceDebugPos', 'NextInstructionDecode', 'IsMultiStepInstruction', 'TestCPUFeatures'    
   
 Format Conversion Procs
 
  * 'toHex', ...

;;
____________________________________________________________________________________________

; GENERAL WINDOW HANDLING PROCS
____________________________________________________________________________________________

[WINDOW_RESIZE  &SWP_NOMOVE+&SWP_NOZORDER+&SWP_NOACTIVATE]
[WINDOW_MOVE    &SWP_NOSIZE+&SWP_NOZORDER+&SWP_NOACTIVATE]

Proc AdjustControlSize:
    Arguments @hwnd, @ID, @dX, @dY
    Structure @RECT 16, @left 0, @top 4, @right 8, @bottom 12
    Uses ebx, esi, edi

        Mov edi D@RECT
        Call 'USER32.GetDlgItem' D@hwnd D@ID | Mov ebx eax
        ; Compute current width (eax) & height (esi) and add delta's
        Call 'USER32.GetWindowRect' ebx edi
        Mov eax D@right
        sub eax D@left
        add eax D@dX
        Mov esi D@bottom
        sub esi D@top
        add esi D@dY
        Call 'USER32.SetWindowPos' ebx, 0, 0, 0, eax, esi, WINDOW_RESIZE
        ; Return height error for listboxes.
        Call 'USER32.GetWindowRect' ebx edi
        Mov eax D@bottom
        sub eax D@top
        sub eax esi
EndP
____________________________________________________________________________________________

Proc AdjustControlPos:
    Arguments @hwnd, @ID, @dX, @dY
    Structure @RECT 16, @left 0, @top 4, @right 8, @bottom 12
    Uses ebx, edi

        Mov edi D@RECT
        Call 'USER32.GetDlgItem' D@hwnd D@ID | Mov ebx eax
        ; Compute current xpos (eax) & ypos (ecx) and add delta's
        Call 'USER32.GetWindowRect' ebx, edi
        Call 'USER32.ScreenToClient' D@hwnd, edi
        Mov eax D@left
        add eax D@dX
        Mov ecx D@top
        add ecx D@dY
        Call 'USER32.SetWindowPos' ebx, 0, eax, ecx, 0, 0, WINDOW_MOVE
EndP
____________________________________________________________________________________________

Proc ReportWinError:
    Arguments @Caption
    Local @String

        Call 'Kernel32.GetLastError'
        Mov edx (&SUBLANG_DEFAULT shl 16 or &LANG_NEUTRAL)
        lea ecx D@String
        Call 'Kernel32.FormatMessageA' &FORMAT_MESSAGE_ALLOCATE_BUFFER+&FORMAT_MESSAGE_FROM_SYSTEM,
            &NULL, eax, edx, ecx, 256, &NULL

        Call MessageBox D@Caption,
                        D@String,
                        &MB_SYSTEMMODAL+&MB_ICONERROR

        Call 'Kernel32.LocalFree' D@String

EndP
____________________________________________________________________________________________

Proc SelectTab:
    Arguments @DlgHandle, @TabCtrlID, @Index
    Structure @NotifyMsg 12, @hwnd 0, @ID 4, @Code 8
    Uses ebx

        Call 'User32.GetDlgItem' D@DlgHandle, D@TabCtrlID
        Mov ebx eax

        SendMessage ebx, &TCM_SETCURSEL, D@Index, 0
        Mov D@hwnd ebx
        move D@ID D@TabCtrlID
        Mov D@Code &TCN_SELCHANGE
        SendMessage D@DlgHandle, &WM_NOTIFY, D@TabCtrlID, D@NotifyMsg ; for Win9x!
        Call 'User32.PostMessageA' D@DlgHandle, &WM_NOTIFY, D@TabCtrlID, D@NotifyMsg
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; ADDRESS SPACE TREE
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc CreateNewForm_AddressSpaceForm:
    Arguments @Parent

    Call 'User32.RegisterClassExA' ASFormClass
    Call 'User32.CreateWindowExA',
        &WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        ASFormClassName,
        {"Main Window",0},
        &WS_CHILD,
        0,0,100,200,
        D@Parent,
        0,
        D$H.Instance,
        0
    Mov D$ASForm_handle eax
    [ASForm_handle: ?]

    Call 'User32.CreateWindowExA',
        &WS_EX_CLIENTEDGE+&WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        {"SysTreeView32",0},
        {"New Control",0},
        &WS_CHILD+&WS_CLIPSIBLINGS+&WS_VISIBLE+&TVS_FULLROWSELECT+&TVS_HASBUTTONS+&TVS_HASLINES+&TVS_LINESATROOT+&TVS_TRACKSELECT,
        0,0,100,200,
        D$ASForm_handle,
        2,
        D$H.Instance,
        0

    Mov D$ASForm.Tree_handle eax
    [ASForm.Tree_handle: ?]

EndP

[ASFormClass:
 @cbSize:        D$ len
 @style:         D$ 0
 @lpfnWndProc:   D$ AddressSpaceFormProc
 @cbClsExtra:    D$ 0
 @cbWndExtra:    D$ 0
 @hInstance:     D$ 0
 @hIcon:         D$ 0
 @hCursor:       D$ 0
 @hbrBackground: D$ 1
 @lpszMenuName:  D$ 0
 @lpszClassName: D$ ASFormClassName
 @hIconSm:       D$ 0]
[ASFormClassName: B$ "AddressSpaceForm" 0]

Proc AddressSpaceFormProc:
    Arguments @hwnd @msg @wParam @lParam
    Uses esi edi ebx

    .If D@msg = &WM_CREATE
        Mov D$AddressSpace.TVProc 0
        Mov eax 0

    .Else_If D@msg = &WM_CLOSE
        Call 'USER32.DestroyWindow' D@hwnd

    .Else_if D@msg = &WM_SIZE
        movzx ecx W@lParam
        movzx edx W@lParam+2

        Call 'USER32.SetWindowPos' D$ASForm.TreeHandle, 0, 0, 0, ecx, edx, WINDOW_RESIZE

    .Else_if D@msg = &WM_SETFONT
        SendMessage D$ASForm.TreeHandle, &WM_SETFONT, D@wParam, D@lParam

    .Else_if D@msg = WM_REFRESH_CONTENT
        Call AddressSpaceTree_Build

    .Else_if D@msg = &WM_SHOWWINDOW
        If D$AddressSpace.TVProc = 0
            Call AddressSpaceForm_OverrideTreeProc
        EndIf
        If D@wParam = &TRUE
            Call AddressSpaceTree_Build
        EndIf

    .Else_if D@msg = &WM_NOTIFY
        Mov eax D@lParam, edx D$ASForm.TreeHandle
        If D$eax = edx
            Call AddressSpaceForm_OnTreeNavigate eax
        EndIf
        Mov eax 0

    .Else
        Call 'User32.DefWindowProcA' D@hwnd D@msg D@wParam D@lParam
    .End_If
EndP
____________________________________________________________________________________________

[ASForm.CurrentItem: ? ASForm.CurrentAddress: ?]

Proc AddressSpaceForm_OnTreeNavigate:
    Arguments @NotifyInfo
    Uses esi ebx

    Mov esi D@NotifyInfo
    On D$esi+8 <> &TVN_SELCHANGED, ExitP
    add esi 12 ; skip notify header
    add esi 4 ; skip action flag
    add esi 40 ; skip old item
    Mov eax D$esi+4 ; hItem
    Mov D$ASForm.CurrentItem eax
    Mov ebx D$esi+36 ; lParam of new item
    Mov D$ASForm.CurrentAddress ebx

EndP

____________________________________________________________________________________________

; To allow rightclick inside the treeview we override the window proc.

AddressSpaceForm_OverrideTreeProc:
    Mov D$AddressSpace.TVProc 0
    Call 'User32.SetWindowLongA' D$ASForm.Tree_handle, &GWL_WNDPROC, AddressSpace_HandleMouseProc
    Mov D$AddressSpace.TVProc eax
ret

[AddressSpace.TVProc: ?]

Proc AddressSpace_HandleMouseProc:
    Arguments @hwnd, @msg, @wParam, @lParam
    Uses ebx esi edi

    .If D@msg = &WM_LBUTTONDBLCLK
      ; Simulate left click (select item) before showing the popup menu
        SendMessage D@hwnd, &WM_LBUTTONDOWN, D@wParam, D@lParam
        SendMessage D@hwnd, &WM_LBUTTONUP, D@wParam, D@lParam
      ; Is this a page item?
        cmp D$ASForm.CurrentAddress 0 | je L0>
      ; Check if the user clicked onto the item
        move D$DebugRect D$ASForm.CurrentItem
        SendMessage D@hwnd, &TVM_GETITEMRECT, 1, DebugRect
        movzx eax W@lParam
        cmp eax D$DebugRect@left  | jl L0>
        cmp eax D$DebugRect@right | ja L0>
        movzx eax W@lParam+2
        cmp eax D$DebugRect@top    | jl L0>
        cmp eax D$DebugRect@bottom | ja L0>
      ; Show the popup menu at the position of the mouse-click
        SendMessage D$MemoryInspectorHandle, WM_SET_PAGE, D$ASForm.CurrentAddress, 0
    .EndIf
L0: Call 'User32.CallWindowProcA' D$AddressSpace.TVProc, D@hwnd, D@msg, D@wParam, D@lParam
EndP
____________________________________________________________________________________________

[PageDesc: B$ ? #64]

Proc AddressSpaceTree_WritePageDescription:
    Arguments @Address, @Protect
    Uses esi, edi

      ; Write address
        Mov eax D@Address, edi PageDesc, ecx 4
        DwordToHex eax

      ; Write protection "XRWC GN"
        Mov al ' ' | stosb
        Mov ecx D@Protect

        Mov D$edi '----', D$edi+4 ' -- '
        If ecx = &PAGE_READONLY
            Mov B$edi+1 'R'
        Else_if ecx = &PAGE_READWRITE
            Mov W$edi+1 'RW'
        Else_if ecx = &PAGE_EXECUTE
            Mov B$edi 'X'
        Else_if ecx = &PAGE_EXECUTE_READ
            Mov W$edi 'XR'
        Else_if ecx = &PAGE_EXECUTE_READWRITE
            Mov D$edi 'XRW-'
        Else_if ecx = &PAGE_EXECUTE_WRITECOPY
            Mov D$edi 'XRWC'
        Else_if ecx = &PAGE_WRITECOPY
            Mov D$edi '-RWC'
        EndIf
        test ecx &PAGE_GUARD | jz L0>
        Mov B$edi+5 'G'
L0:     test ecx &PAGE_NOCACHE | jz L0>
        Mov B$edi+6 'N'

L0:     add edi 8
        Mov D$edi 0
EndP
____________________________________________________________________________________________

Proc AddressSpaceTree_AddPages:
    Arguments @Base, @Size, @Protect, @Type
    Uses esi, edi

      ; Add item for each commited page in region
        Mov esi D@Base, edi esi
        add edi D@Size

        While esi < edi
            Call AddressSpaceTree_WritePageDescription esi, D@Protect
            Mov D$TVI.Item.lParam esi
            Mov D$TVI.Item.pszText PageDesc
            SendMessage D$ASForm.Tree_handle, &TVM_INSERTITEM, 0, TVI
            add esi 01000
        EndWhile
EndP
____________________________________________________________________________________________

Proc AddressSpaceTree_AddModule:
    Arguments @ModEntry
    Local @LowerBound, @UpperBound, @hModItem
    Uses esi, ebx

        Mov esi D@ModEntry

        Mov D$TVI.hParent &TVI_ROOT
        Mov eax D$esi+ME_Name
        Mov D$TVI.Item.pszText eax
        move D$TVI.Item.lParam 0

        SendMessage D$ASForm.Tree_handle, &TVM_INSERTITEM, 0, TVI
        Mov D@hModItem eax

        Mov eax D$esi+ME_Base
        Mov D@LowerBound eax
        add eax D$esi+ME_Size
        Mov D@UpperBound eax

        Mov ebx D@LowerBound
        .While ebx < D@UpperBound
            Call VirtualQuery ebx
            .If eax = &TRUE
                If ebx = D$esi+ME_CodeBase
                    Mov D$TVI.Item.pszText {'Code Section' 0}
                    Mov D$TVI.Item.lParam 0
                    SendMessage D$ASForm.Tree_handle, &TVM_INSERTITEM, 0, TVI
                    Mov D$TVI.hParent eax
                Else
                    move D$TVI.hParent D@hModItem
                EndIf
                Call AddressSpaceTree_AddPages ebx, D$MemoryInformation@RegionSize, D$MemoryInformation@Protect, D$MemoryInformation@Type
            .ElseIf eax = 0-1
                ExitP
            .EndIf
            add ebx D$MemoryInformation@RegionSize
        .EndWhile

;;
        Mov D$TVI.Item.pszText {'Code Section' 0}
        move D$TVI.Item.lParam D$esi+ME_CodeBase
        

        Mov D$TVI.Item.pszText {'Export Section' 0}
        Mov eax D$esi+ME_ExportBase
        add eax D$esi+ME_Base
        Mov D$TVI.Item.lParam eax
        SendMessage D$ASForm.Tree_handle, &TVM_INSERTITEM, 0, TVI
;;

      ; The AS scanner should continue behind the module mapping area
        move D$MemoryInformation@RegionSize D$esi+ME_Size
EndP
____________________________________________________________________________________________

Proc AddressSpaceTree_AddGeneralRegion:
    Arguments @Base, @Size, @Protect, @Type

    [@LastAllocBase: ?]

      ; Check if the region belongs to the last added allocation block, otherwise
      ; add a new top-level item.
        Mov eax D$MemoryInformation@AllocationBase
        If D@LastAllocBase <> eax
            Mov D@LastAllocBase eax
            Call IntToHexString
            Mov D$TVI.hParent &TVI_ROOT
            Mov D$TVI.Item.pszText HexString
            Mov D$TVI.Item.lParam 0
            SendMessage D$ASForm.Tree_handle, &TVM_INSERTITEM, 0, TVI
            Mov D$TVI.hParent eax
        EndIf

      ; Add item for each commited page in region
        Call AddressSpaceTree_AddPages D@Base, D@Size, D@Protect, D@Type
EndP
____________________________________________________________________________________________

; Add a memory region returned by VirtualQuery

Proc AddressSpaceTree_AddRegion:
    Arguments @Base, @Size, @Protect, @Type

        Mov eax D$MemoryInformation@BaseAddress
        Mov D$TVI.Item.lParam eax

      ; Check if this region is a mapped module
        Call GetModuleName eax
        If eax <> 0
            Call AddressSpaceTree_AddModule edx
        Else
            Call AddressSpaceTree_AddGeneralRegion D@Base, D@Size, D@Protect, D@Type
        EndIf
EndP
____________________________________________________________________________________________

Proc AddressSpaceTree_Build:

    SendMessage D$ASForm.TreeHandle, &TVM_DELETEITEM, 0, &TVI_ROOT
    SendMessage D$ASForm.Tree_handle, &WM_SETREDRAW, 0, 0

    Mov D$TVI.hParent &TVI_ROOT
    Mov D$TVI.hInsertAfter &TVI_LAST
    Mov D$TVI.Item.imask &TVIF_TEXT+&TVIF_PARAM

    Mov esi D$AddressLowerBound
    While esi < D$AddressUpperBound
        Call VirtualQuery esi
        If eax = 1
            Call AddressSpaceTree_AddRegion esi, ecx,
                D$MemoryInformation@Protect, D$MemoryInformation@Type
        ElseIf eax = 0-1
            ExitP
        EndIf
        add esi D$MemoryInformation@RegionSize
    EndWhile

    SendMessage D$ASForm.Tree_handle, &WM_SETREDRAW, 1, 0
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; EVENT LOG
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc CreateNewForm_LogForm:
    Arguments @Parent

    Call 'User32.RegisterClassExA' LogFormClass
    Call 'User32.CreateWindowExA',
        &WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        LogFormClassName,
        {"Main Window",0},
        &WS_CHILD+&WS_VISIBLE,
        0,0,100,200,
        D@Parent,
        0,
        D$H.Instance,
        0
    Mov D$LogForm_handle eax
    [LogForm_handle: ?]

    Call 'User32.CreateWindowExA',
        &WS_EX_CLIENTEDGE+&WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        {"LISTBOX",0},
        {"New Control",0},
        &WS_CHILD+&WS_CLIPSIBLINGS+&WS_OVERLAPPED+&WS_VISIBLE+&WS_VSCROLL+&LBS_NOINTEGRALHEIGHT,
        0,0,100,200,
        D$LogForm_handle,
        2,
        D$H.Instance,
        0
    Mov D$LogForm.List_handle eax
    [LogForm.List_handle: ?]

EndP
____________________________________________________________________________________________
[LogFormClass:
 @cbSize:        D$ len
 @style:         D$ 0
 @lpfnWndProc:   D$ LogFormProc
 @cbClsExtra:    D$ 0
 @cbWndExtra:    D$ 0
 @hInstance:     D$ 0
 @hIcon:         D$ 0
 @hCursor:       D$ 0
 @hbrBackground: D$ 1
 @lpszMenuName:  D$ 0
 @lpszClassName: D$ LogFormClassName
 @hIconSm:       D$ 0]
[LogFormClassName: B$ "LogForm" 0]
____________________________________________________________________________________________

Proc LogFormProc:
    Arguments @hwnd @msg @wParam @lParam
    Uses esi edi ebx

    .If D@msg = &WM_CREATE
        Mov eax 0

    .Else_If D@msg = &WM_CLOSE
        Call 'USER32.DestroyWindow' D@hwnd

    .Else_if D@msg = &WM_SIZE
        movzx ecx W@lParam
        movzx edx W@lParam+2

        Call 'USER32.SetWindowPos' D$LogForm.ListHandle, 0, 0, 0, ecx, edx, WINDOW_RESIZE

    .Else_If D@msg = WM_LOG

        Mov edi LogString
        Call TimeToStr
        Mov al ' ' | stosb

        Mov esi D@wParam, ecx D@lParam
        On ecx > 255, Mov ecx 255
        If ecx = 0
            While B$esi+ecx <> 0 | inc ecx | EndWhile
        EndIf
        While ecx > 0
            lodsb
            On al <> 0A, stosb
            dec ecx
        EndWhile
        ;rep movsb
        Mov B$edi 0
        SendMessage D$LogForm.ListHandle, &LB_ADDSTRING, 0, LogString
        SendMessage D$LogForm.ListHandle, &LB_SETCURSEL, eax, 0

    .Else_If D@msg = &WM_DESTROY

    .Else_if D@msg = &WM_SETFONT
        SendMessage D$LogForm.ListHandle, &WM_SETFONT, D@wParam, D@lParam

    .Else
        Call 'User32.DefWindowProcA' D@hwnd D@msg D@wParam D@lParam
    .End_If
EndP

[LogString: B$ ? #256]
____________________________________________________________________________________________
____________________________________________________________________________________________

; ADDRESS INPUT
____________________________________________________________________________________________
____________________________________________________________________________________________

; Tag Wizard Form "J:\Projects\RosAsm\WizardFiles\WZRDForm0000.wwf"
CreateNewForm_CodeAddressForm:
    Call 'ComCtl32.InitCommonControlsEx' CodeAddressFormClassName@Init_All_Common_Controls
    Call 'User32.RegisterClassExA' CodeAddressFormClass
    Call 'User32.CreateWindowExA',
        &WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        CodeAddressFormClassName,
        {"Show code at address ...",0},
        &WS_BORDER+&WS_CAPTION+&WS_DLGFRAME+&WS_OVERLAPPED+&WS_POPUP+&WS_VISIBLE+&NULL,
        82,35,282,100,
        0,
        0,
        D$H.Instance,
        0
    Mov D$CodeAddressForm_handle eax
    [CodeAddressForm_handle: ?]
    Call 'GDI32.CreateFontIndirectA' CodeAddressForm_LOGFONTSTRUCT | Mov D$CodeAddressFormFont_handle eax
    Call 'User32.SendMessageA' D$CodeAddressForm_handle  &WM_SETFONT eax &TRUE
    [CodeAddressFormFont_handle: ?]
    [CodeAddressForm_LOGFONTSTRUCT:  0  0  0  0  0  0  536870912 'MS Sans Serif' 0 0 0 0 0 0 ]
    CodeAddressForm:

    Call 'User32.CreateWindowExA',
        &WS_EX_CLIENTEDGE+&WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        {"BUTTON",0},
        {"Show statement",0},
        &WS_CHILD+&WS_CLIPSIBLINGS+&WS_OVERLAPPED+&WS_VISIBLE+&BS_PUSHBUTTON+&BS_TEXT,
        159,22,101,28,
        D$CodeAddressForm_handle,
        2,
        D$H.Instance,
        0
    Mov D$CodeAddressForm.GotoButton_handle eax
    [CodeAddressForm.GotoButton_handle: ?]
    Call 'GDI32.CreateFontIndirectA' CodeAddressForm.GotoButton_LOGFONTSTRUCT | Mov D$CodeAddressForm.GotoButtonFont_handle eax
    Call 'User32.SendMessageA' D$CodeAddressForm.GotoButton_handle  &WM_SETFONT eax &TRUE
    [CodeAddressForm.GotoButtonFont_handle: ?]
    [CodeAddressForm.GotoButton_LOGFONTSTRUCT:  -11  0  0  0  400  0  570491393 'MS Sans Serif' 0 0 0 0 0 0 ]

    Call 'User32.CreateWindowExA',
        &WS_EX_CLIENTEDGE+&WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        {"EDIT",0},
        {"",0},
        &WS_CHILD+&WS_CLIPSIBLINGS+&WS_OVERLAPPED+&WS_VISIBLE+&ES_LEFT+&ES_RIGHT+&ES_UPPERCASE,
        22,25,125,21,
        D$CodeAddressForm_handle,
        3,
        D$H.Instance,
        0
    Mov D$CodeAddressForm.AddressEdit_handle eax
    [CodeAddressForm.AddressEdit_handle: ?]
    Call 'GDI32.CreateFontIndirectA' CodeAddressForm.AddressEdit_LOGFONTSTRUCT | Mov D$CodeAddressForm.AddressEditFont_handle eax
    Call 'User32.SendMessageA' D$CodeAddressForm.AddressEdit_handle  &WM_SETFONT eax &TRUE
    [CodeAddressForm.AddressEditFont_handle: ?]
    [CodeAddressForm.AddressEdit_LOGFONTSTRUCT:  -13  0  0  0  400  0  822149635 'Courier New' 0 0 0 0 0 0 ]

ret
____________________________________________________________________________________________
[CodeAddressFormClass:
 @cbSize:        D$ len
 @style:         D$ 0
 @lpfnWndProc:   D$ CodeAddressFormProc
 @cbClsExtra:    D$ 0
 @cbWndExtra:    D$ 0
 @hInstance:     D$ 0
 @hIcon:         D$ 0
 @hCursor:       D$ 0
 @hbrBackground: D$ 1
 @lpszMenuName:  D$ 0
 @lpszClassName: D$ CodeAddressFormClassName
 @hIconSm:       D$ 0]
[CodeAddressFormClassName: B$ "CodeAddressForm" 0]
[@Init_All_Common_Controls:
 @Init_All_Common_Controls_dwSize: D$ len
 @Init_All_Common_Controls_dwICC:  D$ &ICC_COOL_CLASSES+&ICC_DATE_CLASSES+&ICC_INTERNET_CLASSES+&ICC_NATIVEFNTCTL_CLASS,
             +&ICC_PAGESCROLLER_CLASS+&ICC_USEREX_CLASSES+&ICC_WIN95_CLASSES]
____________________________________________________________________________________________

Proc CodeAddressFormProc:
    Arguments @hwnd @msg @wParam @lParam

    pushad

    ...If D@msg = &WM_CREATE

    ...Else_If D@msg = &WM_CLOSE
        Call 'GDI32.DeleteObject' D$CodeAddressFormFont_handle
        Call 'GDI32.DeleteObject' D$CodeAddressForm.GotoButtonFont_handle
        Call 'GDI32.DeleteObject' D$CodeAddressForm.AddressEditFont_handle
        Call 'USER32.DestroyWindow' D@hwnd

    ...Else_if D@msg = &WM_COMMAND
        Mov eax D@lParam
        If eax = D$CodeAddressForm.GotoButton_handle
            Call CodeAddressForm_Goto
        EndIf

    ...Else
        popad
        Call 'User32.DefWindowProcA' D@hwnd D@msg D@wParam D@lParam
        ExitP

    ...End_If

    popad
    Mov eax &FALSE

EndP
; Tag End
;;
    ...Else_if D@msg = &WM_COMMAND
        Mov eax D@lParam
        If eax = D$CodeAddressForm.GotoButton_handle
            Call CodeAddressForm_Goto
        EndIf
;;


CodeAddressForm_Goto:
    Call 'User32.GetWindowTextA' D$CodeAddressForm.AddressEdit_handle, HexString, 10
    Mov esi HexString
    Call HexStringToInt
    Mov ebx eax

    Call IsProcessCode ebx
    If eax = &TRUE
        Call SourceDebugPos ebx
    Else

        Call MessageBox {B$ "INVALID ADRESS" EOS},
                        {B$ "This is not a valid code address !" EOS},
                        &MB_SYSTEMMODAL+&MB_ICONWARNING

    EndIf

    SendMessage D$CodeAddressForm_Handle, &WM_CLOSE, 0, 0

ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; Call STACK
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc CreateNewForm_CallStackForm:
    Arguments @Parent

    Call 'User32.RegisterClassExA' CSFormClass
    Call 'User32.CreateWindowExA',
        &WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        CSFormClassName,
        {"Callstack",0},
        &WS_CHILD,
        0,0,100,200,
        D@Parent,
        0,
        D$H.Instance,
        0

    Mov D$CallStackFormHandle eax

    Call 'User32.CreateWindowExA',
        &WS_EX_CLIENTEDGE+&WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        {"SysTreeView32",0},
        {"New Control",0},
        &WS_CHILD+&WS_CLIPSIBLINGS+&WS_VISIBLE+&TVS_FULLROWSELECT+&TVS_HASBUTTONS+&TVS_HASLINES+&TVS_LINESATROOT+&TVS_TRACKSELECT,
        0,0,100,200,
        D$CallStackFormHandle,
        CALLSTACK_TREE,
        D$H.Instance,
        0

    Mov D$CallStackTreeHandle eax

    Call CallStack_OverrideTreeProc
EndP

[CSFormClass:
 @cbSize:        D$ len
 @style:         D$ 0
 @lpfnWndProc:   D$ CallStackProc
 @cbClsExtra:    D$ 0
 @cbWndExtra:    D$ 0
 @hInstance:     D$ 0
 @hIcon:         D$ 0
 @hCursor:       D$ 0
 @hbrBackground: D$ 1
 @lpszMenuName:  D$ 0
 @lpszClassName: D$ CSFormClassName
 @hIconSm:       D$ 0]
[CSFormClassName: B$ "CallStackForm" 0]
____________________________________________________________________________________________

; Process WM_CREATE message.

Proc CallStackDialog_OnCreate:
    Arguments @hwnd

        move D$CallStackFormHandle D@hwnd

        Call 'USER32.GetClientRect' D@hwnd, DebugRect
        move W$CallStackForm.Width W$DebugRect@width
        move W$CallStackForm.Height W$DebugRect@height

        Call CallStack_CreatePopupMenu

        Mov eax &TRUE
EndP
____________________________________________________________________________________________

; Process WM_SIZE message.

Proc CallStackDialog_OnSize:
    Arguments @hwnd, @WidthHeight

        movzx eax W$CallStackForm.Width
        movzx esi W@WidthHeight
        sub esi eax
        movzx eax W$CallStackForm.Height
        movzx edi W@WidthHeight+2
        sub edi eax
        Call AdjustControlSize D@hwnd, CALLSTACK_TREE, esi, edi
        ;Call AdjustControlPos D@hwnd, CALLSTACK_SHOWDECLARATION, 0, edi
        ;Call AdjustControlPos D@hwnd, CALLSTACK_SHOWEVOKE, 0, edi
        Call 'USER32.InvalidateRect' D@hwnd, &NULL, &TRUE

        move D$CallStackFormSize D@WidthHeight
        Mov eax 0
EndP
____________________________________________________________________________________________

; Process WM_COMMAND message.

Proc CallStackDialog_OnCommand:
    Arguments @hwnd, @wParam, @lParam

        movzx ecx W@wParam
        movzx eax W@wParam+2
        Mov ebx D$CallStack.PopupMenu

        .If ecx = MCS_SHOW_INVOKE
            Call CallStackForm_ShowEvoke

        .Else_if ecx = MCS_SHOW_DECL
            Call CallStackForm_ShowDeclaration

        .ElseIf ecx = MCS_SHOW_ALL
            Mov D$CallStackFilter ecx
            Call 'USER32.CheckMenuRadioItem' ebx, MCS_SHOWALL, MCS_HIDE_INTRA,
               ecx, &MF_BYCOMMAND
            Call CallStackForm_Refresh

        .ElseIf ecx = MCS_HIDE_EXTERNAL
            Mov D$CallStackFilter ecx
            Call 'USER32.CheckMenuRadioItem' ebx, MCS_SHOWALL, MCS_HIDE_INTRA,
               ecx, &MF_BYCOMMAND
            Call CallStackForm_Refresh

        .ElseIf ecx = MCS_HIDE_INTRA
            Mov D$CallStackFilter ecx
            Call 'USER32.CheckMenuRadioItem' ebx, MCS_SHOWALL, MCS_HIDE_INTRA,
               ecx, &MF_BYCOMMAND
            Call CallStackForm_Refresh

        .ElseIf ecx = MCS_SHOWLOCALS
            xor B$CallStackShowLocal 1
            Mov eax D$CallStackShowLocal | shl eax 3
            Call 'USER32.CheckMenuItem' ebx, MCS_SHOWLOCALS, eax
            Call CallStackForm_Refresh

        .EndIf
        Mov eax 0
EndP
____________________________________________________________________________________________

; Process CDDS_ITEMPREPAINT notification. ebx > NMTVCUSTOMDRAW

CallStackDialog_OnPaintItem:
    Mov eax D$ebx+44 ; item param
    If eax <> 0

        Mov al B$eax+CSE_Flags
        test al CSEF_EXTERNAL | jz L0>

        Mov D$ebx+48 0_66_66_66 ; set text color to gray
    EndIf
L0: Mov eax &CDRF_NEWFONT
ret
____________________________________________________________________________________________

; Process WM_NOTIFY message.

Proc CallStackDialog_OnNotify:
    Arguments @NotifyMsg

        Mov ebx D@NotifyMsg

        .If D$ebx+8 = &NM_CUSTOMDRAW

            Mov eax D$ebx+12 ; draw stage
            If eax = &CDDS_PREPAINT
                Mov eax &CDRF_NOTIFYITEMDRAW
            Else eax = &CDDS_ITEMPREPAINT
                Call CallStackDialog_OnPaintItem
            Else
                Mov eax 0
            EndIf

        .ElseIf D$ebx+8 = &TVN_SELCHANGED
            Call CallStackForm_OnTreeNavigate ebx
            Mov eax 0

        .Else
            Mov eax 0
        .EndIf
EndP
____________________________________________________________________________________________

; Tag Dialog 1015

[CallStackFormSize:
 CallStackForm.Width: W$ ?
 CallStackForm.Height: W$ ?]

[CallStackFormHandle: ? CallStackTreeHandle: ?
 CallStackShowDeclarationHandle: ? CallStackShowEvokeHandle: ?]

[CALLSTACK_TREE 20]

Proc CallStackProc:
    Arguments @hwnd @msg @wParam @lParam
    Uses ebx, esi, edi

    .If D@msg = &WM_CREATE
        Call CallStackDialog_OnCreate D@hwnd
        Mov eax 0

    .Else_if D@msg = &WM_SHOWWINDOW
        If D@wParam = &TRUE
            Call CallStackForm_Refresh
        EndIf

    .Else_if D@msg = &WM_CLOSE
        Call 'USER32.DestroyWindow' D@hwnd

    .Else_If D@msg = &WM_DESTROY
        Call DestroyCallStack
        Mov D$CallStackFormHandle 0

    .Else_If D@msg = &WM_SIZE
        Call CallStackDialog_OnSize D@hwnd, D@lParam

    .Else_if D@msg = &WM_COMMAND
        Call CallStackDialog_OnCommand D@hwnd, D@wParam, D@lParam

    .Else_if D@msg = &WM_NOTIFY
        Call CallStackDialog_OnNotify D@lParam

    .Else_if D@msg = WM_REFRESH_CONTENT
        Call CallStackForm_Refresh

    .Else_if D@msg = &WM_SETFONT
        SendMessage D$CallStackTreeHandle, &WM_SETFONT, D@wParam, D@lParam

    .Else_if D@msg = &WM_DRAWITEM
        If D@wParam = 0
            Call DebugDialog_OnDrawMenuItem D@lParam
        EndIf
        Mov eax 1

    .Else_if D@msg = &WM_MEASUREITEM
        Mov eax D@lParam
        If D@wParam = 0
          ; menu
            Call DebugDialog_OnMeasureMenuItem D@lParam
        EndIf
        Mov eax 1

    .Else
        Call 'User32.DefWindowProcA' D@hwnd D@msg D@wParam D@lParam
    .EndIf
EndP
____________________________________________________________________________________________

; To allow rightclick inside the treeview we override the window proc.

CallStack_OverrideTreeProc:
    Mov D$CallStack.TVProc 0
    Call 'User32.SetWindowLongA' D$CallStackTreeHandle, &GWL_WNDPROC, CallStack_InterceptRightClick
    Mov D$CallStack.TVProc eax
ret

[CallStack.TVProc: ?]

Proc CallStack_InterceptRightClick:
    Arguments @hwnd, @msg, @wParam, @lParam
    Uses ebx esi edi

    .If D@msg = &WM_RBUTTONDOWN
      ; Simulate left click (select item) before showing the popup menu
        SendMessage D@hwnd, &WM_LBUTTONDOWN, D@wParam, D@lParam
        SendMessage D@hwnd, &WM_LBUTTONUP, D@wParam, D@lParam
      ; Is this a proc name item? (don't show menu for sub items)
        cmp D$CallStackForm.CurrentEntry 0 | jz L0>>
      ; Check if the user right-clicked onto the item
        move D$DebugRect D$CallStackForm.CurrentItem
        SendMessage D@hwnd, &TVM_GETITEMRECT, 1, DebugRect
        movzx eax W@lParam
        cmp eax D$DebugRect@left  | jl L0>
        cmp eax D$DebugRect@right | ja L0>
        movzx eax W@lParam+2
        cmp eax D$DebugRect@top    | jl L0>
        cmp eax D$DebugRect@bottom | ja L0>
      ; Show the popup menu at the position of the mouse-click
        movzx eax W@lParam
        movzx ecx W@lParam+2
        Mov D$PointX eax, D$PointY ecx
        Call 'USER32.ClientToScreen' D@hwnd, Point
        Call 'USER32.TrackPopupMenu' D$CallStack.PopupMenu, &TPM_LEFTALIGN, D$PointX, D$PointY, 0, D$CallStackFormHandle, 0 ; << TODO
    .ElseIf D@msg = &WM_LBUTTONDBLCLK
        Call CallStackForm_ShowDeclaration ; *TODO* strange effect in source editor
    .EndIf
L0: Call 'User32.CallWindowProcA' D$CallStack.TVProc, D@hwnd, D@msg, D@wParam, D@lParam
EndP
____________________________________________________________________________________________

[CallStack.PopupMenu: ?]

[MCS_SHOW_INVOKE    3200
 MCS_SHOW_DECL      3201
 MCS_SHOW_ALL       3202
 MCS_HIDE_EXTERNAL  3203
 MCS_HIDE_INTRA     3204
 MCS_SHOW_LOCALS    3205]

;[CALLSTACK_SHOWALL 30 CALLSTACK_HIDE_EXTERNAL 31 CALLSTACK_HIDE_INTRA 32
; CALLSTACK_SHOWLOCALS 35]

CallStack_CreatePopupMenu:
    Call 'USER32.CreatePopupMenu' | Mov ebx eax, D$CallStack.PopupMenu eax
    Call AppendMenu ebx, &MF_OWNERDRAW, MCS_SHOW_INVOKE, D$StrShowInvokePtr
    Call AppendMenu ebx, &MF_OWNERDRAW, MCS_SHOW_DECL, D$StrShowDeclPtr
    Call 'User32.AppendMenuA' ebx, &MF_SEPARATOR, 0, 0
    Call AppendMenu ebx, &MF_OWNERDRAW, MCS_SHOW_ALL, D$StrShowAllCallsPtr
    Call AppendMenu ebx, &MF_OWNERDRAW, MCS_HIDE_EXTERNAL, D$StrHideModCallsPtr
    Call AppendMenu ebx, &MF_OWNERDRAW, MCS_HIDE_INTRA, D$StrHideIMCallsPtr
    Call 'User32.AppendMenuA' ebx, &MF_SEPARATOR, 0, 0
    Call AppendMenu ebx, &MF_OWNERDRAW+&MF_CHECKED, MCS_SHOWLOCALS, D$StrShowLocalsPtr

    Call 'USER32.CheckMenuRadioItem' ebx, MCS_SHOWALL, MCS_HIDE_INTRA,
        MCS_SHOWALL, &MF_BYCOMMAND
ret

Proc CallStack_UpdatePopupMenu:
    ; TODO
EndP
____________________________________________________________________________________________

[TVI:
 TVI.hParent: ?
 TVI.hInsertAfter: ?
 TVI.Item:
 TVI.Item.imask: ?
 TVI.Item.hItem: ?
 TVI.Item.state: ?
 TVI.Item.stateMask: ?
 TVI.Item.pszText: ?
 TVI.Item.cchTextMax: ?
 TVI.Item.iImage: ?
 TVI.Item.iSelectedImage: ?
 TVI.Item.cChildren: ?
 TVI.Item.lParam: ?]
____________________________________________________________________________________________

; ebx = dword parameter / local number (starts with zero)
; edi > Param string (must have room for 100 bytes)

SearchLocalName:
  ; The first local is the parent frame pointer at offset 0.
    lea eax D$ebx*4
    If eax <> 0
        neg eax | jmp SearchStackFrameSymbol
    Else
        Mov eax ' [Ca' | stosd
        Mov eax 'ller' | stosd
        Mov eax ' EBP' | stosd
        Mov eax ']'    | stosd
    EndIf
ret

SearchParameterName:
  ; The first param is at EBP+8
    lea eax D$ebx*4+8

SearchStackFrameSymbol:
    Mov edx StackFrameSymbols, ecx D$NumStackFrameSymbols
    jecxz L9>

  ; Search correspondance table - cmp the offset
L0: cmp D$edx+4 eax | je L1>
        add edx 8
    loop L0<
    jmp L9>

  ; Copy the name to the buffer at EDI > " [name]"
L1: Mov edx D$edx
    Mov ecx 97
    Mov al ' ' | stosb
    Mov al '['
    Do
        stosb
        Mov al B$edx
        inc edx
        dec ecx | jz L8>
    Loop_Until al < LowSigns
L8: Mov al ']' | stosb
L9: ret
____________________________________________________________________________________________

; edi > CSE (CallStack Entry)

Proc CallStackForm_InsertParameters:

  ; Parameter string reserved on stack
    sub esp 128

    Mov D$TVI.Item.pszText {'Parameter' 0}, D$TVI.Item.lParam 0
    SendMessage D$CallStackTreeHandle, &TVM_INSERTITEM, 0, TVI
    Mov D$TVI.hParent eax

    Mov ebx 0
    Mov esi D$edi+CSE_Address

  ; esi > first parameter
    While ebx < D$edi+CSE_NumParams
        lodsd
        push edi
            lea edi D$esp+4
            DwordToHex eax
            Call SearchParameterName
            Mov al 0 | stosb
        pop edi
        Mov D$TVI.Item.pszText esp
        SendMessage D$CallStackTreeHandle, &TVM_INSERTITEM, 0, TVI
        inc ebx
    EndWhile

    SendMessage D$CallStackTreeHandle, &TVM_EXPAND, &TVE_EXPAND, D$TVI.hParent
EndP
____________________________________________________________________________________________

; edi > CSE (CallStack Entry)

Proc CallStackForm_InsertLocals:

  ; Local string reserved on stack
    sub esp 128

    Mov D$TVI.Item.pszText {'Local data' 0}, D$TVI.Item.lParam 0
    SendMessage D$CallStackTreeHandle, &TVM_INSERTITEM, 0, TVI
    Mov D$TVI.hParent eax

    Mov ebx 0
    Mov esi D$edi+CSE_Address | sub esi 8

  ; esi > caller ebp
    While ebx < D$edi+CSE_NumLocals
        Mov eax D$esi | sub esi 4
        push edi
            lea edi D$esp+4
            DwordToHex eax
            Call SearchLocalName
            Mov al 0 | stosb
        pop edi
        Mov D$TVI.Item.pszText esp
        SendMessage D$CallStackTreeHandle, &TVM_INSERTITEM, 0, TVI
        inc ebx
    EndWhile

    SendMessage D$CallStackTreeHandle, &TVM_EXPAND, &TVE_EXPAND, D$TVI.hParent
EndP
____________________________________________________________________________________________

; Check equate contents at ESI for stack frame symbols (relative to EBP). ECX is length.
; Return the signed offset in EAX if a string of the form "EBP+xx" / "EBP-xx" was found,
; zero otherwise. All registers are preserved (messy).

; Note: Only simple expressions with one offset and one op (+/-) are supported (not "EBP+8-4")

ParseFrameOffset:
    Mov eax 0
    cmp ecx 5 | jb L9>

    lodsd | sub ecx 4
    cmp eax 'EBP'+0A00_0000 | je L0>
    cmp eax 'EBP'+0900_0000 | je L0>

L9: ret

L0: shr eax 24 ; move operator (+/-) in AL

    push edi ebx edx
      ; Save the byte behind the string in AH and replace with 0
        Mov ah B$esi+ecx, B$esi+ecx 0
        push eax
            push esi ecx
                Call TranslateAny
            pop ecx esi
        pop edx
      ; DL = operator, DH = saved byte
        Mov B$esi+ecx dh
        On dl = subSign, neg eax
    pop edx ebx edi
ret
____________________________________________________________________________________________

; Scan the equates table for symbols local to the current callstack entry at EDI and
; fill the table 'StackFrameSymbols'.

[StackFrameSymbols: D$ ? #128 ; correspondance table [ Symbol name | signed offset ] (dwords)
 NumStackFrameSymbols: ?]     ; number of table entries
[MAX_STACK_FRAME_SYMBOLS 64]

Proc FindStackFrameSymbols:
    Local @PlainLabel, @LabelLen
    Uses edi

    Mov D$NumStackFrameSymbols 0

    Mov al B$edi+CSE_FLAGS

    test al CSEF_HAS_STACKFRAME | jz  P9>>
    ;test al CSEF_FUZZY_ADDRESS  | jnz P9>>
    test al CSEF_EXTERNAL       | jnz P9>>

  ; Get symbol name / len
    Mov edi D$edi+CSE_ProcName, D@PlainLabel edi
    Mov ecx 0-1, al 0
    repne scasb
    Mov eax 0-2 | sub eax ecx
    Mov D@LabelLen eax

  ; search the label list for symbols "THISLABEL@..."
    Mov edi D$EquateList, edx D$EquateListLimit
    add edi 5
    While edi < edx
        Mov esi D@PlainLabel, ecx D@LabelLen
        repe cmpsb | jne L0>

            cmp B$edi '@' | jne L0>
            inc edi

          ; Save address of local name without preceding '@'
            Mov ebx D$NumStackFrameSymbols
            Mov D$StackFrameSymbols+ebx*8 edi

          ; Scan to the end of the label name
            Mov al EOI
            Mov ecx edx | sub ecx edi
            repne scasb

          ; Parse equate contents
            Mov esi D$edi, ecx D$edi+4
            Call ParseFrameOffset
            cmp eax 0 | je L1>

          ; Save entry in correspondance table
            Mov D$StackFrameSymbols+ebx*8+4 eax
            inc D$NumStackFrameSymbols

            On D$NumStackFrameSymbols = MAX_STACK_FRAME_SYMBOLS, ExitP

            jmp L1>

L0:     Mov al EOI
        Mov ecx edx | sub ecx edi
        repne scasb
L1:     add edi 10
    EndWhile
EndP
____________________________________________________________________________________________

Proc CallStackForm_BuildTree:
    Local @hProcItem
    Uses edi, ebx, esi

    Call GenerateCallStack

    SendMessage D$CallStackTreeHandle, &WM_SETREDRAW, &FALSE, 0
    SendMessage D$CallStackTreeHandle, &TVM_DELETEITEM, 0, &TVI_ROOT

    Mov D$TVI.hParent &TVI_ROOT
    Mov D$TVI.hInsertAfter &TVI_LAST
    Mov D$TVI.Item.imask &TVIF_TEXT+&TVIF_PARAM

    Mov edi D$FirstCallStackEntry
    .While edi <> 0

        If D$CallStackFilter = MCS_HIDE_EXTERNAL
            test B$edi+CSE_Flags CSEF_EXTERNAL | jnz L0>>
        ElseIf D$CallStackFilter = MCS_HIDE_INTRA
          ; filter module internal calls, this is when the current CSE was called
          ; by a function in the same (external) module
            test B$edi+CSE_Flags CSEF_EXTERNAL | jz L1>
            Mov eax D$edi+CSE_Next
            test B$eax+CSE_Flags CSEF_EXTERNAL | jz L1>
            Call IsModuleCode D$eax+CSE_ProcAddress
            Mov ebx eax
            Call IsModuleCode D$edi+CSE_ProcAddress
            cmp eax ebx | je L0>>
        EndIf

L1:     Mov D$TVI.hParent &TVI_ROOT
        Mov D$TVI.Item.lParam edi
        Mov eax D$edi+CSE_ProcName
        Mov D$TVI.Item.pszText eax
        SendMessage D$CallStackTreeHandle, &TVM_INSERTITEM, 0, TVI
        Mov D$TVI.hParent eax, D@hProcItem eax

        Call FindStackFrameSymbols
        On D$edi+CSE_NumParams > 0,
            Call CallStackForm_InsertParameters

        move D$TVI.hParent D@hProcItem

        If D$CallStackShowLocal = 1
            On D$edi+CSE_NumLocals > 0,
                Call CallStackForm_InsertLocals
        EndIf

        test B$edi+CSE_Flags CSEF_EXTERNAL | jnz L0>
            SendMessage D$CallStackTreeHandle, &TVM_EXPAND, &TVE_EXPAND, D@hProcItem
L0:     Mov edi D$edi+CSE_Next
    .EndWhile

    SendMessage D$CallStackTreeHandle, &WM_SETREDRAW, &TRUE, 0
    Call 'USER32.InvalidateRect' D$CallStackTreeHandle, &NULL, &TRUE

EndP

Proc CallStackForm_OnTreeNavigate:
    Arguments @NotifyInfo
    Uses esi ebx

    Mov esi D@NotifyInfo
    On D$esi+8 <> &TVN_SELCHANGED, ExitP
    add esi 12 ; skip notify header
    add esi 4 ; skip action flag
    add esi 40 ; skip old item
    Mov eax D$esi+4 ; hItem
    Mov D$CallStackForm.CurrentItem eax
    Mov ebx D$esi+36 ; lParam of new item
    Mov D$CallStackForm.CurrentEntry ebx

    Mov eax 0
    On ebx <> 0, Mov eax D$ebx+CSE_ProcAddress
    Call IsProcessCode eax
    xor eax 1
    Call 'USER32.EnableMenuItem' D$CallStack.PopupMenu, MCS_SHOW_DECL, eax
    If ebx <> 0
        Mov ebx D$ebx+CSE_Address
        On ebx <> 0, Mov ebx D$ebx-4
    EndIf
    Call IsProcessCode ebx
    xor eax 1
    Call 'USER32.EnableMenuItem' D$CallStack.PopupMenu, MCS_SHOW_INVOKE, eax

EndP

[CallStackShowLocal: 1
 CallStackFilter: MCS_SHOWALL]

Proc CallStackForm_Refresh:

    Call CallStackForm_BuildTree
EndP

[CallStackForm.CurrentEntry: ? CallStackForm.CurrentItem: ?]

Proc CallStackForm_ShowEvoke:

    Mov eax D$CallStackForm.CurrentEntry
    On eax = 0, ExitP
    Mov edx D$eax
    Mov ecx D$edx-4 ; get return address from the stack-copy
    dec ecx
    Call SourceDebugPos ecx
EndP

Proc CallStackForm_ShowDeclaration:

    Mov eax D$CallStackForm.CurrentEntry
    On eax = 0, ExitP
    Call SourceDebugPos D$eax+4
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; DEBUGGER MAIN DIALOG (Registers & Execution Control)
____________________________________________________________________________________________
____________________________________________________________________________________________


[SendMessage | #=4 | Call 'User32.SendMessageA' #1 #2 #3 #4]
[CopyString | #=1 | Mov esi #1 | R9: | cmp B$esi 0 | je R9> | movsb | jmp R9< | R9: ]
____________________________________________________________________________________________

[GPRRegMap: C.regEax C.regEbx C.regEcx C.regEdx C.regEsi C.regEdi C.regEbp C.regEsp]
[SegRegMap: C.regCs C.regDs C.regEs C.regFs C.regGs C.regSs]
[DbgRegMap: C.regEip C.iDr0 C.iDr1 C.iDr2 C.iDr3 C.iDr6 C.iDr7]

; Format / Conversion tables
[GPRFormats: D$ 9 FmtHexPtr FmtUDecPtr FmtSDecPtr FmtBinaryPtr FmtPUBPtr FmtPSBPtr FmtPUWPtr FmtPSWPtr FmtAsciiPtr]
[GPRConvert: toHex toUDword toSDword toBinary toUByte toSByte toUWord toSWord toAscii]
[FPUFormats: D$ 3 FmtFloatPtr FmtHexPtr FmtBinaryPtr]
[FPUConvert: toExtended toHex toBinary]
[MMXFormats: D$ 9 FmtHexPtr FmtBinaryPtr FmtPUBPtr FmtPSBPtr FmtPUWPtr FmtPSWPtr FmtPUDPtr FmtPSDPtr FmtPFPtr]
[MMXConvert: toHex toBinary toUByte toSByte toUWord toSWord toUDword toSDword toFloat]
[SSEFormats: D$ 10 FmtHexPtr FmtBinaryPtr FmtPFPtr FmtPDPtr FmtPUBPtr FmtPSBPtr FmtPUWPtr FmtPSWPtr FmtPUDPtr FmtPSDPtr FmtPUQPtr FmtPSQPtr]
[SSEConvert: toHex toBinary toFloat toDouble toUByte toSByte toUWord toSWord toUDword toSDword toHex toHex]
[SegFormats: D$ 1 FmtHexPtr]
[SegConvert: toSegHex]
[DbgFormats: D$ 1 FmtHexPtr]
[DbgConvert: toHex]
[MemFormats: D$ 5 FmtHexAnsiPtr FmtHexDWPtr FmtHexWPtr FmtFloatsPtr FmtHexCookedPtr]
[MemConvert: toHexWithAnsi toHexDwords toHexWords toFloats toHexWithCookedAnsi]

; Buffers that hold the converted strings, used as sources while drawing the items
[ValueReg0: B$ ? #256
 ValueReg1: B$ ? #256
 ValueReg2: B$ ? #256
 ValueReg3: B$ ? #256
 ValueReg4: B$ ? #256
 ValueReg5: B$ ? #256
 ValueReg6: B$ ? #256
 ValueReg7: B$ ? #256]

[RegValues: ValueReg0 ValueReg1 ValueReg2 ValueReg3 ValueReg4 ValueReg5 ValueReg6 ValueReg7]

Proc DebugDialog_OnFormatChange:
    Arguments @hwnd @Index

        Mov B$HexSeparator 1
        Mov ebx 0
        .While ebx < 8
            Mov eax D$TabID, edx D@Index
            .If eax = 0 ; GPR
                Mov esi D$GPRRegMap+ebx*4
                Mov ecx 4
                Call D$GPRConvert+edx*4
            .ElseIf eax = 1 ; FPU
                ; test for empty register slots
                Mov eax D$C.FloatSave.TagWord
                ; get top of stack (ecx = TOS * 2)
                Mov ecx D$C.FloatSave.StatusWord
                shr ecx 10 | and ecx 00_1110
                lea ecx D$ebx*2+ecx
                and ecx 0F
                shr eax cl | and eax 3
                ;shl eax 2 | lea ecx D$ebx*2 | shl eax cl
                ;shr eax 16 | and eax 3 ; eax = TagBits for register (11: empty)
                If eax = 0011
                    Mov edi StringBuf
                    Mov D$edi 'EMPT', W$edi+4 'Y'
                Else
                    Mov esi C.FloatSave.RegisterArea
                    imul eax ebx 10
                    add esi eax
                    Mov ecx 10
                    Call D$FPUConvert+edx*4
                EndIf
            .ElseIf eax = 2 ; MMX [V2.1b]
                imul esi ebx 10
                add esi C.FloatSave.RegisterArea
                ;mov esi ebx | shl esi 4
                ;add esi C.regMM
                Mov ecx 8
                Call D$MMXConvert+edx*4
            .ElseIf eax = 3 ; SSE
                Mov esi ebx | shl esi 4
                add esi C.regXMM
                Mov ecx 16
                Call D$SSEConvert+edx*4
            .ElseIf eax = 4 ; Segment
                If ebx < 6
                    Mov esi D$SegRegMap+ebx*4
                    lea edi D$LinearSegmentAddresses+ebx*8
                    Call toSegHex
                    ;Call D$SegConvert+edx*4
                Else
                    Mov edi StringBuf
                    Mov B$edi 0
                EndIf
            .Else_if eax = 5 ; Debug & EIP
                If ebx < 7
                    Mov esi D$DbgRegMap+ebx*4
                    Mov ecx 4
                    Call D$DbgConvert+edx*4
                Else
                    Mov edi StringBuf
                    Mov B$edi 0
                EndIf
            .EndIf

            Mov esi edi, edi D$RegValues+ebx*4
            Do | movsb | Loop_Until B$esi-1 = 0

            inc ebx
        .End_While

        Mov ecx D$TabID, eax D@Index, D$TabFormats+ecx*4 eax
        Call 'USER32.InvalidateRect' D$RegListHandle, &NULL, &TRUE
        Call DebugDialog_RedrawRegisterButtons D@hwnd
EndP
____________________________________________________________________________________________

; Output register-name/value pair. The name is chosen by the tab-index, the value
; by the index provided in the DrawItemStructure passed by WM_DRAWITEM.

[GPRegs: B$ 'EAX' 0 'EBX' 0 'ECX' 0 'EDX' 0 'ESI' 0 'EDI' 0 'EBP' 0 'ESP' 0]
[STRegs: B$ 'ST0' 0 'ST1' 0 'ST2' 0 'ST3' 0 'ST4' 0 'ST5' 0 'ST6' 0 'ST7' 0]
[MMXRegs: B$ 'MM0' 0 'MM1' 0 'MM2' 0 'MM3' 0 'MM4' 0 'MM5' 0 'MM6' 0 'MM7' 0]
[SSERegs: B$ 'XMM0' 0 'XMM1' 0 'XMM2' 0 'XMM3' 0 'XMM4' 0 'XMM5' 0 'XMM6' 0 'XMM7' 0]
[SegRegs: B$ 'CS' 0 'DS' 0 'ES' 0 'FS' 0 'GS' 0 'SS' 0 ' ' 0 ' ' 0]
[DbgRegs: B$ 'EIP' 0 'DR0' 0 'DR1' 0 'DR2' 0 'DR3' 0 'DR6' 0 'DR7' 0 ' ' 0]

[RegSets: GPRegs STRegs MMXRegs SSERegs SegRegs DbgRegs]

[ItemRect: @x1: ? @y1: ? @x2: ? @y2: ?]
[ItemString: B$ ? #128]

[BackgroundCol:
 @Name:  0BB_BBBB 0CC_CCCC 0CC_FFFF
 @Value: 0DD_DDDD 0FF_FFFF 0CC_0000]

[DRAWITEM_CTLTYPE 0
 DRAWITEM_CTLID 4
 DRAWITEM_ITEMID 8
 DRAWITEM_ITEMACTION 12
 DRAWITEM_ITEMSTATE 16
 DRAWITEM_HWNDITEM 20
 DRAWITEM_HDC 24
 DRAWITEM_RCITEM_LEFT 28
 DRAWITEM_RCITEM_TOP 32
 DRAWITEM_RCITEM_RIGHT 36
 DRAWITEM_RCITEM_BOTTOM 40
 DRAWITEM_ITEMDATA 44]

Proc DebugDialog_OnDrawRegisterItem:
    Arguments @DrawItemStruc
    Local @Brush
    Uses ebx edi

    Mov ebx D@DrawItemStruc

    Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 0

  ; Print Register name
    Mov eax D$ebx+DRAWITEM_RCITEM_LEFT | Mov D$ItemRect@x1 eax
    add eax 35 | Mov D$ItemRect@x2 eax
    move D$ItemRect@y1 D$ebx+DRAWITEM_RCITEM_TOP
    move D$ItemRect@y2 D$ebx+DRAWITEM_RCITEM_BOTTOM

    Mov edi D$ebx+DRAWITEM_ITEMID | and edi 1
    Call 'GDI32.CreateSolidBrush' D$BackgroundCol@Name+edi*4 | Mov D@Brush eax
    Call 'USER32.FillRect' D$ebx+DRAWITEM_HDC, ItemRect, D@Brush
    Call 'GDI32.DeleteObject' D@Brush
    Call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC, D$BackgroundCol@Name+edi*4

    Mov ecx D$TabID, edi D$RegSets+ecx*4
    Mov edx 0, al 0
    While edx < D$ebx+DRAWITEM_ITEMID ; item index
        Mov ecx 0-1
        repne scasb
        inc edx
    EndWhile

    Call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, edi, 0-1, ItemRect,
         (&DT_SINGLELINE+&DT_CENTER+&DT_VCENTER)

  ; Hilite changed regs
    If D$TabId = 0 ; GPR
        Mov ecx D$ebx+DRAWITEM_ITEMID
        bt D$GPR_Modified_Mask ecx | jnc L0>
        Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 0C0
L0: EndIf

  ; Print value
    move D$ItemRect@x1 D$ItemRect@x2
    move D$ItemRect@x2 D$ebx+DRAWITEM_RCITEM_RIGHT

    Mov edi D$ebx+DRAWITEM_ITEMID | and edi 1
    Call 'GDI32.CreateSolidBrush' D$BackgroundCol@Value+edi*4 | Mov D@Brush eax
    Call 'USER32.FillRect' D$ebx+DRAWITEM_HDC, ItemRect, D@Brush
    Call 'GDI32.DeleteObject' D@Brush
    Call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC, D$BackgroundCol@Value+edi*4

    Mov ecx D$ebx+DRAWITEM_ITEMID | Mov edi D$RegValues+ecx*4
    Call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, edi, 0-1, ItemRect,
         (&DT_SINGLELINE+&DT_CENTER+&DT_VCENTER)

EndP
____________________________________________________________________________________________

; Create the tabs for the different register types. First check if they are available.

[RegGeneral: 'General' 0 RegFPU: 'FPU' 0 RegMMX: 'MMX' 0 RegSSE: 'SSE' 0 RegSSE2: 'SSE2' 0
 RegSegment: 'Segment' 0 RegDebug: 'Debug' 0]

[DebugRegTabHandle: ?]
[DebugFormatComboHandle: ?]

Proc DebugDialog_CreateRegisterTabs:
    Arguments @hwnd
    Local @Index
    ; Tag Dialog 1010

      ; Create register listbox
        movzx ecx W$DebugDialog.Width | sub ecx 6
        Mov edx 53 ;mov edx D$DebugDialog.RebarHeight | add edx 50

        Call 'User32.CreateWindowExA',
            &WS_EX_CLIENTEDGE,
            {'LISTBOX' 0},
            {'Register' 0},
            &WS_CHILD+&WS_CLIPSIBLINGS+&WS_VISIBLE+&LBS_NOSEL+&LBS_OWNERDRAWFIXED,
            3, edx, ecx, 138,
            D$H.DebugDialog,
            DEBUGDLG_REG_LIST,
            D$H.Instance,
            0

        Mov D$RegListHandle eax

      ; Create format combo
        movzx ecx W$DebugDialog.Width | sub ecx 122
        Mov edx 29 ;mov edx D$DebugDialog.RebarHeight | add edx 26

        Call 'User32.CreateWindowExA',
            0,
            {'COMBOBOX' 0},
            {'Representation' 0},
            &WS_CHILD+&WS_CLIPSIBLINGS+&WS_VISIBLE+&CBS_DROPDOWNLIST+&CBS_HASSTRINGS,
            120, edx, ecx, 200,
            D$H.DebugDialog,
            DEBUGDLG_FORMAT_COMBO,
            D$H.Instance,
            0

        Mov D$DebugFormatComboHandle eax
        SendMessage eax, &WM_SETFONT, D$DialogFontHandle, &TRUE

      ; Create static control
        Mov edx 32 ;mov edx D$DebugDialog.RebarHeight | add edx 29

        Call 'User32.CreateWindowExA',
            0,
            {'STATIC' 0},
            D$StrDataFmtPtr,
            &WS_CHILD+&WS_CLIPSIBLINGS+&WS_VISIBLE+&SS_LEFT,
            7, edx, 140, 16,
            D$H.DebugDialog,
            3,
            D$H.Instance,
            0

        SendMessage eax, &WM_SETFONT, D$DialogFontHandle, &TRUE

      ; Create tab control
        movzx ecx W$DebugDialog.Width
        Mov edx 6 ;mov edx D$DebugDialog.RebarHeight | add edx 4

        Call 'User32.CreateWindowExA',
            0,
            {"SysTabControl32",0},
            {"RegisterTab",0},
            &WS_CHILD+&WS_CLIPSIBLINGS+&WS_VISIBLE, ;+&TCS_FLATBUTTONS+&TCS_HOTTRACK+&TCS_BUTTONS,
            0, edx, ecx, 184,
            D$H.DebugDialog,
            DEBUGDLG_REGISTER_TAB,
            D$H.Instance,
            0

        Mov D$DebugRegTab_handle eax
        Mov ebx eax
        SendMessage eax, &WM_SETFONT, D$DialogFontHandle, &TRUE

      ; Add tabs
        Mov D@Index 0
        Mov D$TabItem@imask &TCIF_TEXT+&TCIF_PARAM

        Mov D$TabItem@pszText RegGeneral
        Mov D$TabItem@lParam 0
        SendMessage ebx, &TCM_INSERTITEM, D@Index, TabItem
        On eax = 0-1, Call ReportWinError {'TCM_INSERTITEM' 0}
        inc D@Index

        test D$CPUFlags FLAG_FPU | jz L0>

        Mov D$TabItem@pszText RegFPU
        Mov D$TabItem@lParam 1
        SendMessage ebx, &TCM_INSERTITEM, D@Index, TabItem
        inc D@Index

L0:     test D$CPUFlags FLAG_MMX | jz L0>

        Mov D$TabItem@pszText RegMMX
        Mov D$TabItem@lParam 2
        SendMessage ebx, &TCM_INSERTITEM, D@Index, TabItem
        inc D@Index

L0:     test D$CPUFlags FLAG_SSE | jz L0>

        Mov D$TabItem@pszText RegSSE
        Mov D$TabItem@lParam 3
        SendMessage ebx, &TCM_INSERTITEM, D@Index, TabItem
        inc D@Index

L0:     Call SelectTab ebx, DEBUGDLG_REGISTER_TAB, 0

        Call DebugDialog_InitRegisterListBox D@hwnd
        Call DebugDialog_CreateRegisterButtons D@hwnd
EndP
____________________________________________________________________________________________

; Show/Hide tabs for advanced registers (segments, debug).

Proc DebugDialog_ShowAdvancedTabs:
    Arguments @hwnd @Show
    Local @TabHandle @Index

        Call 'USER32.GetDlgItem' D@hwnd, DEBUGDLG_REGISTER_TAB
        Mov D@TabHandle eax

        SendMessage D@TabHandle, &TCM_GETITEMCOUNT, 0, 0
        Mov D@Index eax

        .If D@Show = &TRUE
            Mov D$TabItem@imask &TCIF_TEXT+&TCIF_PARAM

            Mov D$TabItem@pszText RegSegment
            Mov D$TabItem@lParam 4
            SendMessage D@TabHandle, &TCM_INSERTITEM, D@Index, TabItem
            inc D@Index

            Mov D$TabItem@pszText RegDebug
            Mov D$TabItem@lParam 5
            SendMessage D@TabHandle, &TCM_INSERTITEM, D@Index, TabItem
        .Else
            SendMessage D@TabHandle, &TCM_GETCURSEL, 0, 0 | Mov ebx eax
            dec D@Index
            SendMessage D@TabHandle, &TCM_DELETEITEM, D@Index, 0
            dec D@Index
            SendMessage D@TabHandle, &TCM_DELETEITEM, D@Index, 0
            On ebx >= D@Index, Mov ebx 0
            Call SelectTab D@hwnd, DEBUGDLG_REGISTER_TAB, ebx
        .EndIf
        Call 'USER32.InvalidateRect' D@hwnd &NULL &TRUE
EndP
____________________________________________________________________________________________

; Initially fill the listbox with eight items. The listbox does NOT contain strings, so
; we just set the address of the buffer as item data. Another issue: When the listbox
; is enabled it blocks the mouse from clicking on the register buttons, therefore it is
; disabled.

Proc DebugDialog_InitRegisterListBox:
    Arguments @hwnd

        Mov eax D$RegListHandle
        Call 'User32.EnableWindow' eax, &FALSE
        Mov esi RegValues, ebx 0
        While ebx < 8
            lodsd
            Mov B$eax 0
            SendMessage D$RegListHandle, &LB_ADDSTRING, 0, eax
            inc ebx
        EndWhile
EndP
____________________________________________________________________________________________

; Create the buttons for the general purpose registers.

Proc DebugDialog_CreateRegisterButtons:
    Arguments @hwnd

        Mov ebx 0, esi GPRegs
        .While ebx < 8
            ; Get position of item and transform in debug dialogs client coordinates
            SendMessage D$RegListHandle, &LB_GETITEMRECT, ebx, DebugRect
            Mov ecx D$DebugRect@bottom
            sub ecx D$DebugRect@top
            Mov D$DebugRect@height ecx
            Call 'USER32.ClientToScreen' D$RegListHandle, DebugRect
            Call 'USER32.ScreenToClient' D@hwnd, DebugRect
            ; Create a button
            Mov eax DEBUGDLG_FIRSTREG_BUTTON
            add eax ebx
            Call 'User32.CreateWindowExA' 0, ButtonClassName, esi,
                                        &WS_CHILD,
                                        D$DebugRect@left, D$DebugRect@top, 35, D$DebugRect@height,
                                        D@hwnd, eax,
                                        D$H.Instance, &NULL
            add esi 4
            inc ebx
        .EndWhile
EndP
____________________________________________________________________________________________

; The dialog has been resized, adjust the width and height of the controls.

Proc DebugDialog_OnSize:
    Arguments @hwnd, @WidthHeight

      ; calculate the delta values (pixel difference old -> new size)
        movzx eax W$DebugDialog.Width
        movzx esi W@WidthHeight
        sub esi eax
        movzx eax W$DebugDialog.Height
        movzx edi W@WidthHeight+2
        sub edi eax

        move D$DebugDialogSize D@WidthHeight

        Call AdjustControlSize D@hwnd, DEBUGDLG_REGISTER_TAB, esi, 0
        Call AdjustControlSize D@hwnd, DEBUGDLG_FORMAT_COMBO, esi, 0
        Call AdjustControlSize D@hwnd, DEBUGDLG_REG_LIST, esi, 0
        Call AdjustControlSize D@hwnd, DEBUGDLG_DATA_TAB, esi, edi
        Call DebugDialog_AdjustDataTabSize D@hwnd
        Call 'USER32.InvalidateRect' D@hwnd &NULL &TRUE

      ; notify rebar
        movzx eax W$DebugDialog.Width
        SendMessage D$Debug_RebarHandle, &WM_SIZE, eax, 0

        Mov eax 0
EndP
____________________________________________________________________________________________

; The tab-selection has changed, refill format combo.

[TabFormats: D$ 0 #6]
[RegFormats: GPRFormats FPUFormats MMXFormats SSEFormats SegFormats DbgFormats]
[TabID: ?]

Proc DebugDialog_OnChangeRegisterTab:
    Arguments @hwnd

    Call 'User32.GetDlgItem' D@hwnd, DEBUGDLG_REGISTER_TAB | Mov ebx eax
    SendMessage ebx, &TCM_GETCURSEL, 0, 0
    SendMessage ebx, &TCM_GETITEM, eax, TabItem
    move D$TabID D$TabItem@lParam

    Call DebugDialog_UpdateRegisterButtons D@hwnd

    ; Refill format/representation combo
    Mov ebx D$DebugFormatComboHandle
    SendMessage ebx, &CB_RESETCONTENT, 0, 0
    Mov ecx D$TabID, esi D$RegFormats+ecx*4
    lodsd | Mov ecx eax
D0:     push ecx
            lodsd
            If D$UnicodeStrings = 0
                SendMessage ebx, &CB_ADDSTRING, 0, D$eax
            Else
                Call 'User32.SendMessageW' ebx, &CB_ADDSTRING, 0, D$eax
            EndIf
        pop ecx
    loop D0<

    Mov ecx D$TabID
    SendMessage ebx, &CB_SETCURSEL, D$TabFormats+ecx*4, 0
    SendMessage D@hwnd, &WM_COMMAND, (&CBN_SELCHANGE shl 16 or DEBUGDLG_FORMAT_COMBO), ebx
EndP
____________________________________________________________________________________________

[DataViewHandle: ? MemoryInspectorHandle: ? H.CurrentDataPage: ?]

Proc DebugDialog_CreateDataTab:
    Arguments @hwnd

      ; Create tab control
        movzx ecx W$DebugDialog.Width
        movzx edx W$DebugDialog.Height | sub edx 195

        Call 'User32.CreateWindowExA',
            0,
            {"SysTabControl32",0},
            {"DataTab",0},
            &WS_CHILD+&WS_CLIPSIBLINGS+&WS_VISIBLE+&TCS_FLATBUTTONS, ;+&TCS_FLATBUTTONS+&TCS_HOTTRACK+&TCS_BUTTONS,
            0, 195, ecx, edx,
            D$H.DebugDialog,
            DEBUGDLG_DATA_TAB,
            D$H.Instance,
            0

        Mov ebx eax
        SendMessage eax &WM_SETFONT, D$DialogFontHandle, &TRUE

      ; Add tabs
        Mov D$TabItem@imask &TCIF_TEXT
        Mov D$TabItem@pszText DataView
        SendMessage ebx, &TCM_INSERTITEM, 0, TabItem
        Mov D$TabItem@pszText MemoryInspector
        SendMessage ebx, &TCM_INSERTITEM, 1, TabItem
        Mov D$TabItem@pszText CallStack
        SendMessage ebx, &TCM_INSERTITEM, 2, TabItem
        Mov D$TabItem@pszText Log
        SendMessage ebx, &TCM_INSERTITEM, 3, TabItem
        Mov D$TabItem@pszText AddressSpace
        SendMessage ebx, &TCM_INSERTITEM, 4, TabItem

      ; Create sub windows
        Call 'User32.CreateDialogParamA' D$H.Instance, 1011, ebx, DataViewProc, 0
        Mov D$DataViewHandle eax
        Mov D$H.CurrentDataPage eax
        Call 'User32.CreateDialogParamA' D$H.Instance, 1012, ebx, MemoryInspectorProc, 0
        Mov D$MemoryInspectorHandle eax
        Call 'USER32.ShowWindow' eax, &SW_HIDE

        ;Call 'user32.CreateDialogParamA' D$H.Instance, 1015, ebx, CallStackProc, 0
        Call CreateNewForm_CallStackForm ebx
        Call 'USER32.ShowWindow' D$CallStackFormHandle, &SW_HIDE

        Call CreateNewForm_LogForm ebx
        Call 'USER32.ShowWindow' D$LogForm_handle, &SW_HIDE

        Call CreateNewForm_AddressSpaceForm ebx
        Call 'USER32.ShowWindow' D$ASForm_handle, &SW_HIDE

        Call DebugDialog_AdjustDataTabSize D@hwnd
EndP
____________________________________________________________________________________________

[FPURndModes: FPURndNearest FPURndDown FPURndUp FPURndTrunc]

[FPUPrec24: '24 bits (single)' 0
 FPUPrec53: '53 bits (double)' 0
 FPUPrec64: '64 bits (extended)' 0]

[FPUPrecModes: FPUPrec24 0 FPUPrec53 FPUPrec64]

[NoException: 'No Exception' 0]

DebugDialog_GetFPUStatus:

    ; Output tag word
    CopyString FPUTagWord
    push edi
        Mov esi C.FloatSave.TagWord, ecx 2 | Call toBinary
        Mov ecx StringBufTail | sub ecx edi
        Mov esi edi
    pop edi
    rep movsb

    Mov W$edi 0A0D | add edi 2

    ; Output control word
    CopyString FPUControlWord
    push edi
        Mov esi C.FloatSave.ControlWord, ecx 2 | Call toBinary
        Mov ecx StringBufTail | sub ecx edi
        Mov esi edi
    pop edi
    rep movsb

    Mov W$edi 0A0D | add edi 2

    ; Rounding mode
    CopyString FPURoundingMode
    movzx eax W$C.FloatSave.ControlWord
    shr eax 10 | and eax 3
    CopyString D$FPURndModes+eax*4

    Mov W$edi 0A0D | add edi 2

    ; Precision
    CopyString FPUPrecision
    movzx eax W$C.FloatSave.ControlWord
    shr eax 8 | and eax 3
    CopyString D$FPUPrecModes+eax*4
    Mov W$edi 0A0D | add edi 2

    ; Output statusword
    CopyString FPUStatusWord
    push edi
        Mov esi C.FloatSave.Statusword, ecx 2 | Call toBinary
        Mov ecx StringBufTail | sub ecx edi
        Mov esi edi
    pop edi
    rep movsb

    Mov W$edi 0A0D | add edi 2

    movzx eax W$C.FloatSave.StatusWord
    Mov esi NoException
    test al 01 | jz L0>
    Mov esi FLT_INVALID_OPERATION
L0: test al 02 | jz L0>
    Mov esi FLT_DENORMAL_OPERAND
L0: test al 04 | jz L0>
    Mov esi FLT_DIVIDE_BY_ZERO
L0: test al 08 | jz L0>
    Mov esi FLT_OVERFLOW
L0: test al 010 | jz L0>
    Mov esi FLT_UNDERFLOW
L0: test al 020 | jz L0>
    Mov esi FLT_INEXACT_RESULT
L0: test al 040 | jz L0>
    Mov esi FLT_STACK_CHECK
L0: While B$esi <> 0
        movsb
    End_While
    Mov B$edi 0

ret

____________________________________________________________________________________________

[CPUFeatures: 'CPU Features:' 0]
[ConditionMove: 'Conditional Move Instructions (CMOV / FCMOV / FCOMI)' 0]

DebugDialog_GetCPUInfo:

    CopyString CPUVendor
    Mov W$edi 0A0D | add edi 2

    Mov esi CPUName
    While B$esi = ' '
        inc esi
    EndWhile
    While B$esi <> 0
        movsb
    EndWhile
    Mov D$edi 0A0D0A0D | add edi 4

    CopyString CPUFeatures
    Mov W$edi 0A0D | add edi 2

  ; Scan the general flags
    Mov edx D$CPUFlags

    test edx FLAG_MMX | jz L0>
    Mov D$edi 'MMX,', B$edi+4 ' '
    add edi 5

L0: test edx FLAG_SSE | jz L0>
    Mov D$edi 'SSE,', B$edi+4 ' '
    add edi 5

L0: test edx FLAG_SSE2 | jz L0>
    Mov D$edi 'SSE2', W$edi+4 ', '
    add edi 6
L0: sub edi 2
    Mov W$edi 0A0D | add edi 2

    test edx FLAG_CMOV | jz L0>
    CopyString ConditionMove
L0: Mov W$edi 0A0D | add edi 2

  ; Scan extended flags
    Mov edx D$CPUFlagsEx

    test edx FLAG_EX_MMX | jz L0>
    Mov D$edi 'AMD ', D$edi+4 'MMX ', D$edi+8 'Exte', D$edi+12 'nsio', D$edi+16 'ns, '
    add edi 20
L0: test edx FLAG_EX_3DNOW | jz L0>
    Mov D$edi '3dno', D$edi+4 'w,  '
    add edi 7
L0: test edx FLAG_EX_3DNOWEX | jz L0>
    Mov D$edi '3dno', D$edi+4 'w Ex', D$edi+8 'tens', D$edi+12 'ions', W$edi+16 ', '
    add edi 18
L0: sub edi 2
    Mov W$edi 0A0D | add edi 2

    Mov B$edi 0
ret
____________________________________________________________________________________________

; Set the flag checkboxes (button-style) according to the flag register

[FlagMasks: 0800 080 040 01 04 0400 0200 0100 010]
[FPUFlagMasks: 04000 0400 0200 0100]

Proc DebugDialog_UpdateFlags:
    Arguments @hwnd

        ; update the eflags
        Mov ebx D$C.regFlag, edi 90, esi FlagMasks
        Do
            Mov edx &FALSE
            lodsd
            test ebx eax | jz L0>
            inc edx
L0:         SendMessage D$Debug_FlagBarHandle, &TB_CHECKBUTTON, edi, edx
            ;Call 'User32.SendDlgItemMessageA' D@hwnd, edi, &BM_SETCHECK, edx, 0
            inc edi
        Loop_Until edi > 98

        ; update FPU flags
        Mov ebx D$C.FloatSave.StatusWord, edi 101, esi FPUFlagMasks
        Do
            Mov edx &FALSE
            lodsd
            test ebx eax | jz L0>
            inc edx
L0:         SendMessage D$DebugFPUbarHandle, &TB_CHECKBUTTON, edi, edx
            ;Call 'User32.SendDlgItemMessageA' D@hwnd, edi, &BM_SETCHECK, edx, 0
            inc edi
        Loop_Until edi > 104
EndP
____________________________________________________________________________________________

Proc DebugDialog_UpdateRegisterButtons:
    Arguments @hwnd

        On B$DebuggerReady = &FALSE, ExitP

        .If D$TabID = 0
            Mov ebx 0
            While ebx < 8
                Mov eax D$GPRRegMap+ebx*4
                Call IsProcessMemory D$eax
                Mov edi eax

                Mov eax ebx
                add eax DEBUGDLG_FIRSTREG_BUTTON
                Call 'USER32.GetDlgItem' D@hwnd, eax
                Mov esi eax
                Call 'USER32.ShowWindow' esi, &SW_SHOW
                Call 'USER32.EnableWindow' esi, edi
                inc ebx
            EndWhile
        .Else
            Mov ebx DEBUGDLG_FIRSTREG_BUTTON
            While ebx <= DEBUGDLG_LASTREG_BUTTON
                Call 'USER32.GetDlgItem' D@hwnd, ebx
                Call 'USER32.ShowWindow' eax, &SW_HIDE
                inc ebx
            EndWhile
        .EndIf
EndP

Proc DebugDialog_RedrawRegisterButtons:
    Arguments @hwnd

        Mov ebx DEBUGDLG_FIRSTREG_BUTTON
        While ebx <= DEBUGDLG_LASTREG_BUTTON
            Call 'USER32.GetDlgItem' D@hwnd, ebx
            Call 'USER32.InvalidateRect' eax, &NULL, &TRUE
            inc ebx
        EndWhile
EndP
____________________________________________________________________________________________

Proc DebugDialog_AdjustDataTabSize:
    Arguments @hwnd
    Local @TabHandle
    Uses ebx

        Call 'USER32.GetDlgItem' D@hwnd, DEBUGDLG_DATA_TAB
        Mov D@TabHandle eax

        Call 'USER32.GetClientRect' D@TabHandle, DebugRect
        SendMessage D@TabHandle, &TCM_ADJUSTRECT, &FALSE, DebugRect
        Mov eax D$DebugRect@left
        Mov ebx D$DebugRect@top
        Mov ecx D$DebugRect@right
        Mov edx D$DebugRect@bottom
        sub ecx eax
        sub edx ebx
        Call 'USER32.SetWindowPos' D$H.CurrentDataPage, 0, eax, ebx, ecx, edx, &SWP_NOZORDER
EndP
____________________________________________________________________________________________

[CONTINUE_RUN 0 CONTINUE_STEP 1 CONTINUE_STEPOVER 2 CONTINUE_RETURNTOCALLER 3]

[ContinueMode: ?] ; is used by debug thread

Proc DebugDialog_ContinueDebuggee:
    On B$IsDebugEvent = &FALSE, ExitP

    Call DebugDialog_EnableContinueMenu &FALSE

    move D$ContinueMode D$UserWants

    If B$ContinueMode = CONTINUE_STEPOVER
        Call NextInstructionDecode ; decode again - might be overwritten by callstackscanner
        Call IsMultiStepInstruction
        On eax = &FALSE, Mov B$ContinueMode CONTINUE_STEP
    ElseIf B$ContinueMode = CONTINUE_RUN
        Mov D$LastSourcePos 0
    EndIf

    Call 'User32.SetWindowTextA' D$H.DebugDialog, {'Running ...' 0}

    Call 'Kernel32.SetEvent' D$UserInputEvent
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; Debug dialog image list initialisation & finalization.

[DebugDialog_ImageList: ?]

Proc DebugDialog_CreateImageList:
    Local @Image, @Mask

  ; Create the images
    Call 'User32.LoadImageA' D$H.Instance, 10, &IMAGE_BITMAP, 0, 0, 0
    If eax = 0
        Call ReportWinError {'DebugDialog_CreateImageList: LoadImage (1)' 0}
    EndIf
    Mov D@Image eax

    Call 'User32.LoadImageA' D$H.Instance, 11, &IMAGE_BITMAP, 0, 0, 0
    If eax = 0
        Call ReportWinError {'DebugDialog_CreateImageList: LoadImage (2)' 0}
    EndIf
    Mov D@Mask eax

    Call 'ComCtl32.ImageList_Create' 16, 16, &ILC_COLOR8+&ILC_MASK, 8, 8
    Mov D$DebugDialog_ImageList eax

    Call 'ComCtl32.ImageList_Add' D$DebugDialog_ImageList, D@Image, D@Mask
    If eax = 0-1
        Call ReportWinError {'Debug TB: ImageList_Add' 0}
    EndIf

    Call 'GDI32.DeleteObject' D@Image
    Call 'GDI32.DeleteObject' D@Mask
EndP
____________________________________________________________________________________________

DebugDialog_DestroyImageList:
    Call 'COMCTL32.ImageList_Destroy' D$DebugDialog_ImageList
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

DebuggerMENU: ; Create the MENU of the main debugger window.
____________________________________________________________________________________________
____________________________________________________________________________________________

[DebugMenuTable: 29
 ; ID             | String           | Shortcut      | Imageindex
 M02_Run            StrRunPtr          StrF6           0
 M02_Step_into      StrStepIntoPtr     StrF7           3
 M02_Step_Over      StrStepOverPtr     StrF8           4
 M02_ReturnToCaller StrReturnPtr       StrCtrlF7       5
 M02_Terminate      StrTerminatePtr    StrCtrlF6       1
 M02_Pause          StrPausePtr        StrCtrlF12      2
 M02_HoldOnBP       StrHoldOnBPPtr     0               7
 M02_Inst_Level     StrInstStepPtr     0               7
 M02_Source_Level   StrSrcStepPtr      0               7
 M02_ShowAll        StrShowAllPtr      0               7
 M02_Font           StrFontPtr         0               0-1
 M02_CPU_Info       StrCPUInfoPtr      0               0-1
 M02_FPU_Status     StrFPUStatusPtr    0               0-1
 M02_ShowCodeAt     StrShowCodeAtPtr   0               0-1
 M02_About          StrAboutPtr        0               0-1
 M02_Help           StrDbgHelpPtr      StrF1           0-1

 M03_SHOW_MEM       StrShowInMemInspPtr  0             0-1
 M03_SHOW_PMEM      StrShowPInMemInspPtr 0             0-1
 M03_SHOW_DECL      StrShowDeclPtr       0             0-1
 M03_WATCH_W        StrBreakOnWPtr       0             7
 M03_WATCH_RW       StrBreakOnRWPtr      0             7
 M03_SORTBYNAME     StrSortByNamePtr     0             7
 M03_SORTBYADDRESS  StrSortByAddrPtr     0             7

 MCS_SHOW_INVOKE    StrShowInvokePtr    0              0-1
 MCS_SHOW_DECL      StrShowDeclPtr      0              0-1
 MCS_SHOW_ALL       StrShowAllCallsPtr  0              7
 MCS_HIDE_EXTERNAL  StrHideModCallsPtr  0              7
 MCS_HIDE_INTRA     StrHideIMCallsPtr   0              7
 MCS_SHOWLOCALS     StrShowLocalsPtr    0              7]
____________________________________________________________________________________________

[M02_Menu  3000                  M02_Run  3001                   M02_Step_Into  3002
 M02_Step_Over  3003             M02_Return_to_Caller  3004      M02_Pause  3005
 M02_Terminate  3006             M02_Hold_on_BP  3007            M02_Inst_Level 3008
 M02_Source_Level 3009           M02_Show_All 3010
 M02_Font  3011                  M02_CPU_Info  3012              M02_FPU_Status  3013
 M02_Show_code_at  3014          M02_About  3015                 M02_Help  3016]
____________________________________________________________________________________________

[DebugMenubarButtons:
 ; iBitmap D, idCommand D, fsState B, fsStyle B, wPad1 W, Data D, iString D
 D$ 0-2 0              B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE+&BTNS_DROPDOWN W$ 0 D$ 0 0
 D$ 0-2 1              B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE+&BTNS_DROPDOWN W$ 0 D$ 0 0
 D$ 0-2 2              B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE+&BTNS_DROPDOWN W$ 0 D$ 0 0
 D$ 0-2 3              B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE+&BTNS_DROPDOWN W$ 0 D$ 0 0
 D$ 0-2 4              B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE+&BTNS_DROPDOWN W$ 0 D$ 0 0]

[ContinueMenu: ? BreakMenu: ? SettingsMenu: ? ExtrasMenu: ? HelpMenu: ?]
[NUM_MENUBAR_ENTRIES 5]

Proc DebugWindow_CreateMenu:
    Structure @Item 44,
        @Size 0,  @Mask 4,  @Type 8,  @State 12,  @ID 16,  @SubMenu 20,
        @bmpChecked 24,  @bmpUnchecked 28,  @ItemData 32,  @Data 36,  @cch 40

  ; Store menu handle in ebx
;    Call 'User32.CreateMenu' | Mov ebx eax

  ; Create menu-bar (toolbar)
    Call 'User32.CreateWindowExA' 0, {'ToolbarWindow32' 0}, 0,
        DEBUG_TOOLBAR_STYLE,
        0, 0, 200, 0, D$H.DebugDialog, DEBUGDLG_MENUBAR, D$H.Instance, 0

    Mov ebx eax
    SendMessage ebx, &WM_SETFONT, D$DialogFont_handle, 0
    SendMessage ebx, &TB_BUTTONSTRUCTSIZE, 20, 0

    move D$DebugMenubarButtons+16   D$StrContinuePtr
    move D$DebugMenubarButtons+36   D$StrBreakPtr
    move D$DebugMenubarButtons+56   D$StrSettingsPtr
    move D$DebugMenubarButtons+76   D$StrInformationPtr
    move D$DebugMenubarButtons+96   D$StrHelpPtr

    If D$UnicodeStrings = 0
        SendMessage ebx, &TB_ADDBUTTONSA, 5, DebugMenubarButtons
    Else
        SendMessage ebx, &TB_ADDBUTTONSW, 5, DebugMenubarButtons
    EndIf

  ; Add the bands
    Mov D$DebugRebarBand@hwndChild ebx
    ;or D$DebugRebarBand@fStyle &RBBS_BREAK
    SendMessage ebx, &TB_GETMAXSIZE, 0, Point
    ;Call 'user32.GetClientRect'  D$Debug_ToolbarHandle, DebugRect
    ;mov eax D$DebugRect@right | sub eax D$DebugRect@left
    Mov eax D$PointX | add eax 10
    Mov D$DebugRebarBand@cx eax
    SendMessage D$DebugRebarHandle, &RB_INSERTBAND, BAND_MENUBAR, DebugRebarBand
    SendMessage D$DebugRebarHandle, &RB_SHOWBAND, BAND_MENUBAR, 1
    ;and D$DebugRebarBand@fStyle (not &RBBS_BREAK)
    ________________________________________________________________________________________

    Mov D@Size 44
    Mov D@Mask &MIIM_ID+&MIIM_TYPE
    Mov D@Type &MFT_OWNERDRAW

  ; Continue menu
    Call 'USER32.CreatePopupMenu' | Mov edi eax | Mov D$ContinueMenu eax
    ;Call AppendMenu ebx, &MF_STRING+&MF_POPUP, edi, D$StrContinuePtr

    Mov D@ID M02_Run
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

    Mov D@ID M02_Step_Into
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

    Mov D@ID M02_Step_Over
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

    Mov D@Mask &MIIM_TYPE
    Mov D@Type &MFT_SEPARATOR
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

    Mov D@Mask &MIIM_ID+&MIIM_TYPE
    Mov D@Type &MFT_OWNERDRAW
    Mov D@ID M02_Return_To_Caller
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

  ; Break menu
    Call 'USER32.CreatePopupMenu' | Mov edi eax, D$BreakMenu eax
    ;Call AppendMenu ebx, &MF_STRING+&MF_POPUP, edi, D$StrBreakPtr

    Mov D@ID M02_Pause
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

    Mov D@ID M02_Terminate
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

  ; Settings menu
    Call 'USER32.CreatePopupMenu' | Mov edi eax, D$SettingsMenu eax
    ;Call AppendMenu ebx, &MF_STRING+&MF_POPUP, edi, D$StrSettingsPtr

    Mov D@ID M02_Hold_on_BP
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item
    Mov D@ID M02_Inst_Level
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item
    Mov D@ID M02_Source_Level
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item
    Mov D@ID M02_Show_All
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item
    Mov D@ID M02_Font
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

    Mov D@Mask &MIIM_TYPE
    Mov D@Type &MFT_SEPARATOR
    Call 'User32.InsertMenuItemA' edi, 4, 1, D@Item
    Call 'User32.InsertMenuItemA' edi, 3, 1, D@Item
    Call 'User32.InsertMenuItemA' edi, 1, 1, D@Item
;;
  ; Toolbar sub menu
    Call 'USER32.CreatePopupMenu' | Mov esi eax, D$Debug_ToolbarMenu eax
    Call 'User32.AppendMenuA' edi, &MF_STRING+&MF_POPUP, esi, {'Toolbar' 0}
    ;Call 'User32.AppendMenuA' ebx, &MF_STRING, DEBUGDLG_TB_CONFIGURE, {'Configure ...' 0}
    Call 'User32.AppendMenuA' esi, &MF_STRING, DEBUGDLG_TB_SHOW_TEXT, {'Show text' 0}
    Call 'User32.AppendMenuA' esi, &MF_SEPARATOR, 0, 0
    Call 'User32.AppendMenuA' esi, &MF_STRING+&MF_CHECKED, DEBUGDLG_TB_SHOW_CMDS, {'Show commands' 0}
    Call 'User32.AppendMenuA' esi, &MF_STRING+&MF_CHECKED, DEBUGDLG_TB_SHOW_FLAGS, {'Show flags' 0}
    Call 'User32.AppendMenuA' esi, &MF_STRING, DEBUGDLG_TB_SHOW_FPU, {'Show FPU flags' 0}
;;
  ; Information menu
    Call 'USER32.CreatePopupMenu' | Mov edi eax, D$ExtrasMenu eax
    ;Call AppendMenu ebx, &MF_STRING+&MF_POPUP, edi, D$StrInformationPtr

    Mov D@Mask &MIIM_ID+&MIIM_TYPE
    Mov D@Type &MFT_OWNERDRAW

    Mov D@ID M02_CPU_Info
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item
    Mov D@ID M02_FPU_Status
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item
    Mov D@ID M02_Show_code_at
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

    Mov D@Mask &MIIM_TYPE
    Mov D@Type &MFT_SEPARATOR
    Call 'User32.InsertMenuItemA' edi, 2, 1, D@Item

  ; Help menu
    Call 'USER32.CreatePopupMenu' | Mov edi eax, D$HelpMenu eax
    ;Call AppendMenu ebx, &MF_STRING+&MF_POPUP, edi, D$StrHelpPtr

    Mov D@Mask &MIIM_ID+&MIIM_TYPE
    Mov D@Type &MFT_OWNERDRAW

    Mov D@ID M02_About
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item
    Mov D@ID M02_Help
    Call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

  ; Return menu handle
    Mov D$MenuBarHandle ebx
    Mov eax ebx
EndP
____________________________________________________________________________________________

[MenubarHook: ? CurrentMenubarIndex: ?]

; Observe mouse messages to close the popup menu when the user clicks outside the menu area
; and to switch to other submenus when the mouse hovers above another menubar item.

Proc MenubarHookProc:
    Arguments @Code, @wParam, @Msg
    Uses ebx, edi

    Call 'USER32.CallNextHookEx' D$MenubarHook, D@Code, D@wParam, D@Msg
    push eax

        Mov ebx D@Msg
        Mov eax D$ebx+4

        .If eax = &WM_LBUTTONDOWN
          ; Collapse menu if clicked outside menu area.
            If D@Code <> &MSGF_MENU
                Call 'User32.PostMessageA' D$H.DebugDialog, WM_COLLAPSEMENU, 0, 0
            EndIf

        .ElseIf eax = &WM_RBUTTONDOWN
          ; Collapse menu if clicked outside menu area.
            If D@Code <> &MSGF_MENU
                Call 'User32.PostMessageA' D$H.DebugDialog, WM_COLLAPSEMENU, 0, 0
            EndIf

        .ElseIf eax = &WM_MOUSEMOVE
          ; Check if mouse hovered above different menubar item. Perform hittest to
          ; identify which item is selected and if selection has changed collapse the
          ; old menu and track the new menu.
            movzx eax W$ebx+12
            movzx edx W$ebx+14
            Mov D$PointX eax
            Mov D$PointY edx
            Call 'USER32.ScreenToClient' D$MenuBarHandle, Point
            Call 'User32.SendMessageA' D$MenuBarHandle, &TB_HITTEST, 0, Point
            test eax eax | js L0>
            cmp eax NUM_MENUBAR_ENTRIES | jae L0>
            Mov edi eax
            If D$CurrentMenubarIndex <> eax
                Call 'User32.PostMessageA' D$H.DebugDialog, WM_COLLAPSEMENU, 0, 0
                Call 'User32.PostMessageA' D$H.DebugDialog, WM_POPUPMENU, edi, 0
            EndIf
L0:
        .ElseIf eax = &WM_KEYDOWN
          ; Select the next submenu when user presses left/right.
          ; Note: This does *not* work when the mouse is over the menubar as
          ; it continously receives WM_MOUSEMOVE messages (why?) which makes it
          ; switch back to the former menu immediately.
            If D$ebx+8 = &VK_LEFT
                Call 'User32.PostMessageA' D$H.DebugDialog, WM_COLLAPSEMENU, 0, 0
                Mov eax D$CurrentMenubarIndex
                dec eax | jns L1>
                Mov eax (NUM_MENUBAR_ENTRIES-1)
L1:             Call 'User32.PostMessageA' D$H.DebugDialog, WM_POPUPMENU, eax, 0
            ElseIf D$ebx+8 = &VK_RIGHT
                Call 'User32.PostMessageA' D$H.DebugDialog, WM_COLLAPSEMENU, 0, 0
                Mov eax D$CurrentMenubarIndex
                inc eax | cmp eax NUM_MENUBAR_ENTRIES | jb L1>
                Mov eax 0
L1:             Call 'User32.PostMessageA' D$H.DebugDialog, WM_POPUPMENU, eax, 0
            EndIf

        .EndIf

    pop eax
EndP
____________________________________________________________________________________________

[HotTracking: ? HotTrackMenu: ?]

; Track popup menu and prepare the GUI for menu navigation (install hook).
; TODO: Fix alignment of the rightmost popup menus.

Proc DebugDialog_OnPopupMenu:
    Arguments @Item
    Structure @Rect 16, @left 0,  @top 4,  @right 8,  @bottom 12

    SendMessage D$MenuBarHandle, &TB_GETRECT, D@Item, D@Rect
    move D$PointX D@left
    move D$PointY D@bottom
    Call 'USER32.ClientToScreen' D$MenuBarHandle, Point

    Mov edx D@Item
    If edx = 0
        Mov eax D$ContinueMenu
    ElseIf edx = 1
        Mov eax D$BreakMenu
    ElseIf edx = 2
        Mov eax D$SettingsMenu
    ElseIf edx = 3
        Mov eax D$ExtrasMenu
    ElseIf edx = 4
        Mov eax D$HelpMenu
    EndIf
    Mov D$HottrackMenu eax, D$CurrentMenuBarIndex edx

  ; How can I measure the width of a popup menu before it is tracked?
    Call 'USER32.GetWindowRect' D$HotTrackMenu, D@Rect
    Call 'USER32.GetSystemMetrics' &SM_CXSCREEN
    Mov ecx D@right | sub ecx D@left
    Mov edx D$PointX
    add edx ecx
    If edx > eax
        sub eax ecx
        Mov D$PointX eax
    EndIf

    SendMessage D$MenuBarHandle, &TB_PRESSBUTTON, D@Item, 1

    Mov D$HotTracking 1

  ; Install mouse hook
    If D$MenubarHook <> 0
        Call 'OutputDebugStringA' {'Unhook (!)' 0}
        Call 'USER32.UnhookWindowsHookEx' D$MenubarHook
        Mov D$MenubarHook 0
    EndIf

    Call 'Kernel32.GetCurrentThreadId'
    Call 'User32.SetWindowsHookExA' &WH_MSGFILTER, MenubarHookProc, D$H.Instance, eax
    Mov D$MenubarHook eax

    PRINTLN 'SetHook'

  ; Open the menu. Note: The Call doesn't return until the menu is closed!
    Call 'USER32.TrackPopupMenu' D$HotTrackMenu, &TPM_LEFTBUTTON, D$PointX, D$PointY, 0, D$H.DebugDialog, 0

    SendMessage D$MenuBarHandle, &TB_PRESSBUTTON, D@Item, 0

    If D$MenuBarHook <> 0
        PRINTLN 'Unhook'
        Call 'USER32.UnhookWindowsHookEx' D$MenubarHook
        Mov D$MenubarHook 0
    EndIf

    Mov D$HotTracking 0
EndP
____________________________________________________________________________________________
[PRINTLN | Call 'OutputDebugStringA' {#1 0}]

; Collapse menu.
; TODO: Check if WM_CANCELMODE works under Windows NT.

DebugDialog_OnCollapseMenu:
    SendMessage D$H.DebugDialog, &WM_CANCELMODE, 0, 0
    Mov D$HotTracking 0
    If D$MenubarHook <> 0
        PRINTLN 'Unhook (C)'
        Call 'USER32.UnhookWindowsHookEx' D$MenubarHook
        Mov D$MenubarHook 0
    EndIf
ret
____________________________________________________________________________________________

; Show menu when user clicks on menubar item.

Proc DebugDialog_MenubarNotify:
    Arguments @NotifyMsg, @Code, @Item

    Mov eax 0

    If D@Code = &TBN_DROPDOWN
        Call 'User32.PostMessageA' D$H.DebugDialog, WM_POPUPMENU, D@Item, 0
        Mov eax &TBDDRET_DEFAULT
    EndIf
EndP
____________________________________________________________________________________________

; Handle key commands [F10, ALT+x]

[PrevFocusWindow: ?]

Proc DebugDialog_OnSysCommand:
    Arguments @hwnd, @Type, @lParam

    ..If D@Type = &SC_KEYMENU

        .If D$HotTracking = 0

            Mov eax D@lParam
            If eax = 0 ; F10
                ;SendMessage D$MenuBarHandle, &TB_PRESSBUTTON, 0, 0
                SendMessage D$MenuBarHandle, &TB_SETHOTITEM, 0, 0
                Call 'USER32.SetFocus' D$MenuBarHandle
                ;mov D$PrevFocusWindow eax
            EndIf

        .EndIf

        Mov eax 0

    ..Else
        Call 'User32.DefWindowProcA' D@hwnd, &WM_SYSCOMMAND, D@Type, D@lParam
    ..EndIf
EndP
____________________________________________________________________________________________

; Handle keyboard menu navigation [Arrow keys, ESC]

Proc DebugDialog_OnKeyDown:
    Arguments @hwnd, @Key, @lParam

    SendMessage D$MenuBarHandle, &TB_GETHOTITEM, 0, 0

    .If eax <> 0-1
        If D@Key = &VK_RIGHT
            inc eax
            On eax = NUM_MENUBAR_ENTRIES,
                Mov eax 0

        ElseIf D@Key = &VK_LEFT
            dec eax
            On eax = 0-1
                Mov eax NUM_MENUBAR_ENTRIES-1

        ElseIf D@Key = &VK_ESCAPE
            ;Call 'USER32.SetFocus' D$PrevFocusWindow
            Mov eax 0-1
        EndIf

        SendMessage D$MenuBarHandle, &TB_SETHOTITEM, eax, 0
    .EndIf

    Mov eax 0
EndP
____________________________________________________________________________________________
; Call (indeed jump to) either to the ansi or unicode taste of API functions.

AppendMenu:
    cmp B$UnicodeStrings 1 | je L0>
    jmp 'User32.AppendMenuA'
L0: jmp 'User32.AppendMenuW'

DrawText:
    cmp B$UnicodeStrings 1 | je L0>
    jmp 'User32.DrawTextA'
L0: jmp 'User32.DrawTextW'
____________________________________________________________________________________________

____________________________________________________________________________________________

; Enable / Gray-out execution control commands.

Proc DebugDialog_EnableContinueMenu:
    Arguments @Enable

    If D@Enable = 1
        Mov ebx &MF_ENABLED
    Else
        Mov ebx &MF_GRAYED
    EndIf

    Call 'USER32.EnableMenuItem' D$ContinueMenu, M02_Run, ebx
    Call 'USER32.EnableMenuItem' D$ContinueMenu, M02_Step_Into, ebx
    Call 'USER32.EnableMenuItem' D$ContinueMenu, M02_Step_Over, ebx
    Call 'USER32.EnableMenuItem' D$ContinueMenu, M02_Return_to_Caller, ebx

    SendMessage D$Debug_ToolbarHandle, &TB_ENABLEBUTTON, M02_Run, D@Enable
    SendMessage D$Debug_ToolbarHandle, &TB_ENABLEBUTTON, M02_Step_Into, D@Enable
    SendMessage D$Debug_ToolbarHandle, &TB_ENABLEBUTTON, M02_Step_over, D@Enable
    SendMessage D$Debug_ToolbarHandle, &TB_ENABLEBUTTON, M02_Return_to_Caller, D@Enable

  ; Invert
    If D@Enable = 1
        Mov ebx &MF_GRAYED
        Mov D@Enable 0
    Else
        Mov ebx &MF_ENABLED
        Mov D@Enable 1
    EndIf
    Call 'USER32.EnableMenuItem' D$BreakMenu, M02_Pause, ebx
    SendMessage D$Debug_ToolbarHandle, &TB_ENABLEBUTTON, M02_Pause, D@Enable

    Call 'USER32.DrawMenuBar' D$H.DebugDialog
EndP
____________________________________________________________________________________________

DebugDialog_InitDbgMenu:
    Call 'USER32.CheckMenuItem' D$SettingsMenu, M02_Hold_on_BP, &MF_CHECKED
    Call 'USER32.CheckMenuRadioItem' D$SettingsMenu,
        M02_Inst_Level, M02_Source_Level, M02_Inst_Level, &MF_BYCOMMAND

    Call DebugDialog_EnableContinueMenu 0
ret
____________________________________________________________________________________________

; Process WM_DRAWITEM message for owner-drawn menu items.

Proc DebugDialog_OnDrawMenuItem:
    Arguments @DrawItemStruc
    Local @Brush

    Mov ebx D@DrawItemStruc

    Mov esi DebugMenuTable
    lodsd | Mov ecx eax ; number of entries
    Mov eax D$ebx+DRAWITEM_ITEMID
    While D$esi <> eax
        add esi 16
        dec ecx | jz L9>>
    EndWhile

    Call 'GDI32.SelectObject' D$ebx+DRAWITEM_HDC, D$DialogFont_handle
    push eax

    Test_If D$ebx+DRAWITEM_ITEMSTATE &ODS_GRAYED
        Call 'USER32.GetSysColor' &COLOR_GRAYTEXT
        Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax
        jmp L1>
    Test_Else_If D$ebx+DRAWITEM_ITEMSTATE &ODS_SELECTED
        Call 'USER32.GetSysColor' &COLOR_HIGHLIGHTTEXT
        Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax

        Call 'USER32.GetSysColor' &COLOR_HIGHLIGHT
        Call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC, eax

        Call 'USER32.GetSysColorBrush' &COLOR_HIGHLIGHT
    Test_Else
        Call 'USER32.GetSysColor' &COLOR_MENUTEXT
        Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax

L1:     Call 'USER32.GetSysColor' &COLOR_MENU
        Call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC, eax

        Call 'USER32.GetSysColorBrush' &COLOR_MENU
    Test_End

    lea edx D$ebx+DRAWITEM_RCITEM_LEFT
    Call 'USER32.FillRect' D$ebx+DRAWITEM_HDC, edx, eax

    Mov eax D$esi+12 ; image index
    If eax <> 0-1
        Mov ecx D$ebx+DRAWITEM_RCITEM_LEFT | add ecx 2
        Mov edx D$ebx+DRAWITEM_RCITEM_TOP  | add edx 2
        Test_If D$ebx+DRAWITEM_ITEMSTATE &ODS_GRAYED
            Mov edi &ILD_MASK
        Test_Else_If D$ebx+DRAWITEM_ITEMSTATE &ODS_SELECTED
            Mov edi &ILD_NORMAL
        Test_Else
            Mov edi &ILD_TRANSPARENT
        Test_End

        Test_If D$ebx+DRAWITEM_ITEMSTATE &ODS_CHECKED
            Mov eax 6
        Test_End

L0:     Call 'COMCTL32.ImageList_Draw' D$DebugDialog_ImageList, eax, D$ebx+DRAWITEM_HDC, ecx, edx, edi
    EndIf

    add D$ebx+DRAWITEM_RCITEM_LEFT 22

    Mov eax D$esi+4
    Call DrawMenuItemText D$eax, &DT_LEFT

    Mov eax D$esi+8
    If eax <> 0
      ; Draw shortcut rightaligned
        dec D$ebx+DRAWITEM_RCITEM_RIGHT
        Call DrawMenuItemTextA eax, &DT_RIGHT
    EndIf

    pop eax
    Call 'GDI32.SelectObject' D$ebx+DRAWITEM_HDC, eax

L9: Mov eax 1
EndP
____________________________________________________________________________________________

; ebx > DRAWITEMSTRUCT

Proc DrawMenuItemText: ; Localized (ANSI or Unicode)
    Arguments @Text, @Align

  ; Grayed text is drawn white overlayed by gray (shifted 1 pixel in both directions)
    Test_If D$ebx+DRAWITEM_ITEMSTATE &ODS_GRAYED

        inc D$ebx+DRAWITEM_RCITEM_TOP
        inc D$ebx+DRAWITEM_RCITEM_BOTTOM
        inc D$ebx+DRAWITEM_RCITEM_LEFT
        inc D$ebx+DRAWITEM_RCITEM_RIGHT

        Call 'USER32.GetSysColor' &COLOR_3DHIGHLIGHT
        Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax
        lea edx D$ebx+DRAWITEM_RCITEM_LEFT
        Mov eax &DT_SINGLELINE+&DT_VCENTER | or eax D@Align
        Call DrawText D$ebx+DRAWITEM_HDC, D@Text, 0-1, edx, eax

        dec D$ebx+DRAWITEM_RCITEM_TOP
        dec D$ebx+DRAWITEM_RCITEM_BOTTOM
        dec D$ebx+DRAWITEM_RCITEM_LEFT
        dec D$ebx+DRAWITEM_RCITEM_RIGHT

        Call 'USER32.GetSysColor' &COLOR_GRAYTEXT
        Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax
        Call 'GDI32.SetBkMode' D$ebx+DRAWITEM_HDC, &TRANSPARENT

    Test_End

    lea edx D$ebx+DRAWITEM_RCITEM_LEFT
    Mov eax &DT_SINGLELINE+&DT_VCENTER | or eax D@Align
    Call DrawText D$ebx+DRAWITEM_HDC, D@Text, 0-1, edx, eax

EndP
____________________________________________________________________________________________

; ebx > DRAWITEMSTRUCT

Proc DrawMenuItemTextA: ; ANSI only
    Arguments @Text, @Align

  ; Grayed text is drawn white overlayed by gray (shifted 1 pixel in both directions)
    Test_If D$ebx+DRAWITEM_ITEMSTATE &ODS_GRAYED

        inc D$ebx+DRAWITEM_RCITEM_TOP
        inc D$ebx+DRAWITEM_RCITEM_BOTTOM
        inc D$ebx+DRAWITEM_RCITEM_LEFT
        inc D$ebx+DRAWITEM_RCITEM_RIGHT

        Call 'USER32.GetSysColor' &COLOR_3DHIGHLIGHT
        Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax
        lea edx D$ebx+DRAWITEM_RCITEM_LEFT
        Mov eax &DT_SINGLELINE+&DT_VCENTER | or eax D@Align
        Call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, D@Text, 0-1, edx, eax

        dec D$ebx+DRAWITEM_RCITEM_TOP
        dec D$ebx+DRAWITEM_RCITEM_BOTTOM
        dec D$ebx+DRAWITEM_RCITEM_LEFT
        dec D$ebx+DRAWITEM_RCITEM_RIGHT

        Call 'USER32.GetSysColor' &COLOR_GRAYTEXT
        Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax
        Call 'GDI32.SetBkMode' D$ebx+DRAWITEM_HDC, &TRANSPARENT

    Test_End

    lea edx D$ebx+DRAWITEM_RCITEM_LEFT
    Mov eax &DT_SINGLELINE+&DT_VCENTER | or eax D@Align
    Call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, D@Text, 0-1, edx, eax

EndP
____________________________________________________________________________________________

Proc DebugDialog_OnMeasureMenuItem:
    Arguments @MeasureItem

    Mov ebx D@MeasureItem
    Mov edx D$ebx+8 ; item id
    Mov esi DebugMenuTable
    lodsd ; eax = num entries
    While D$esi <> edx
        add esi 16
        dec eax | jz L9>
    EndWhile

    Mov eax D$esi+4
    Call MeasureStringWidth D$eax, D$DialogFontHandle

    On D$esi+8 <> 0,
        add eax 50 ; shortcut

    add eax 22 ; icon+padding

    Mov D$ebx+12 eax ; width
    Mov D$ebx+16 20 ; height
L9:
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

DebuggerToolbar: ; TOOLBAR and coolbar (REBAR) of debugger main window.
____________________________________________________________________________________________
____________________________________________________________________________________________

[DEBUGDLG_REBAR 331 DEBUGDLG_TOOLBAR 332 DEBUGDLG_MENUBAR 333
 DEBUGDLG_FLAGS 334 DEBUGDLG_FPUFLAGS 335]

; Flow control buttons
[DebugToolbarButtons:
 ; iBitmap D, idCommand D, fsState B, fsStyle B, wPad1 W, Data D, iString D
 D$ 0 M02_Run              B$ 0                &BTNS_AUTOSIZE W$ 0 D$ 0 0
 D$ 1 M02_Terminate        B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE W$ 0 D$ 0 0
 D$ 2 M02_Pause            B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE W$ 0 D$ 0 0
 D$ 3 M02_Step_Into        B$ 0                &BTNS_AUTOSIZE W$ 0 D$ 0 0
 D$ 4 M02_Step_Over        B$ 0                &BTNS_AUTOSIZE W$ 0 D$ 0 0
 D$ 5 M02_Return_to_Caller B$ 0                &BTNS_AUTOSIZE W$ 0 D$ 0 0]
 ; Strings

[FlagStr:
    @O: 'O'   0 @S: 'S'    0 @Z: 'Z'   0 @C: 'C'   0 @P: 'P' 0 @D: 'Dir' 0
    @I: 'Int' 0 @T: 'Trap' 0 @A: 'Aux' 0
    @C3: 'C3' 0 @C2: 'C2'  0 @C1: 'C1' 0 @C0: 'C0' 0]

; Standard flag buttons
[DebugFlagButtons:
 D$ 0-2 90 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@O
 D$ 0-2 91 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@S
 D$ 0-2 92 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@Z
 D$ 0-2 93 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@C
 D$ 0-2 94 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@P
 D$ 0-2 95 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@D
 D$ 0-2 96 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@I
 D$ 0-2 97 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@T
 D$ 0-2 98 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@A]

; FPU flag buttons
[DebugFPUButtons:
 D$ 0-2 101 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@C3
 D$ 0-2 102 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@C2
 D$ 0-2 103 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@C1
 D$ 0-2 104 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@C0]

[DebugToolbarHandle: ? Debug_FlagBarHandle: ? Debug_FPUBarHandle: ? DebugRebarHandle: ? Debug_ToolbarMenu: ?]

[DebugRebarBand:
   @cbSize:     D$ len
   @fMask:      D$ &RBBIM_CHILD+&RBBIM_STYLE+&RBBIM_CHILDSIZE+&RBBIM_SIZE+&RBBIM_IDEALSIZE+&RBBIM_HEADERSIZE
   @fStyle:     D$ &RBBS_CHILDEDGE+&RBBS_GRIPPERALWAYS ;+&RBBS_BREAK
   0 0 0 0 0
   @hwndChild:  D$ 0
   @cxMinChild: D$ 10
   @cyMinChild: D$ 22
   @cx:         D$ 0 0
   @wID:        D$ 0
   @cyChild:    D$ 0
   @cyMaxChild: D$ 0
   @cyIntegral: D$ 0
   @cxIdeal:    D$ 0 0
   @cxHeader:   D$ 6 ]


[Init_Common_Controls:
 @dwSize: D$ len
 @dwICC:  D$ &ICC_COOL_CLASSES+&ICC_BAR_CLASSES]
____________________________________________________________________________________________

[DEBUG_TOOLBAR_STYLE &WS_CHILD+&CCS_ADJUSTABLE+&TBSTYLE_FLAT+&TBSTYLE_LIST+&TBSTYLE_AUTOSIZE+&TBSTYLE_TRANSPARENT+&CCS_NOPARENTALIGN+&CCS_NODIVIDER+&CCS_NORESIZE]

[DebugShowTBText: ?]

Proc DebugDialog_CreateCommandTB:
  ; Save states & clear if toolbar is REcreated
    .If D$Debug_ToolbarHandle <> 0
        SendMessage D$DebugToolbarHandle, &TB_GETSTATE, M02_Run, 0
        Mov B$DebugToolbarButtons+8 al
        SendMessage D$DebugToolbarHandle, &TB_GETSTATE, M02_Terminate, 0
        Mov B$DebugToolbarButtons+28 al
        SendMessage D$DebugToolbarHandle, &TB_GETSTATE, M02_Pause, 0
        Mov B$DebugToolbarButtons+48 al
        SendMessage D$DebugToolbarHandle, &TB_GETSTATE, M02_Step_Into, 0
        Mov B$DebugToolbarButtons+68 al
        SendMessage D$DebugToolbarHandle, &TB_GETSTATE, M02_Step_Over, 0
        Mov B$DebugToolbarButtons+88 al
        SendMessage D$DebugToolbarHandle, &TB_GETSTATE, M02_Return_to_Caller, 0
        Mov B$DebugToolbarButtons+108 al
        SendMessage D$Debug_RebarHandle, &RB_DELETEBAND, 1, 0
        Call 'USER32.DestroyWindow' D$Debug_ToolbarHandle
    .EndIf

  ; Create toolbar
    Call 'User32.CreateWindowExA' 0, {'ToolbarWindow32' 0}, 0,
        DEBUG_TOOLBAR_STYLE,
        0, 0, 120, 0, D$H.DebugDialog, DEBUGDLG_TOOLBAR, D$H.Instance, 0

    Mov D$DebugToolbarHandle eax
    SendMessage eax, &WM_SETFONT, D$DialogFont_handle, 0

    SendMessage D$Debug_ToolbarHandle, &TB_BUTTONSTRUCTSIZE, 20, 0
    SendMessage D$Debug_ToolbarHandle, &TB_SETIMAGELIST, 0, D$DebugDialog_ImageList

  ; Activate / Deactivate Text
    If D$DebugShowTBText = 1
        move D$DebugToolbarButtons+16   D$StrRunPtr
        move D$DebugToolbarButtons+36   D$StrTerminatePtr
        move D$DebugToolbarButtons+56   D$StrPausePtr
        move D$DebugToolbarButtons+76   D$StrStepPtr
        move D$DebugToolbarButtons+96   D$StrStepOverPtr
        move D$DebugToolbarButtons+116  D$StrRetPtr
    Else
        Mov D$DebugToolbarButtons+16   0
        Mov D$DebugToolbarButtons+36   0
        Mov D$DebugToolbarButtons+56   0
        Mov D$DebugToolbarButtons+76   0
        Mov D$DebugToolbarButtons+96   0
        Mov D$DebugToolbarButtons+116  0
    EndIf

    If D$UnicodeStrings = 0
        SendMessage D$Debug_ToolbarHandle, &TB_ADDBUTTONSA, 6, DebugToolbarButtons
    Else
        SendMessage D$Debug_ToolbarHandle, &TB_ADDBUTTONSW, 6, DebugToolbarButtons
    EndIf

  ; Add band
    move D$DebugRebarBand@hwndChild D$DebugToolbarHandle
    SendMessage D$Debug_ToolbarHandle, &TB_GETMAXSIZE, 0, Point
    ;Call 'user32.GetClientRect'  D$Debug_ToolbarHandle, DebugRect
    ;mov eax D$DebugRect@right | sub eax D$DebugRect@left
    Mov eax D$PointX | add eax 10
    Mov D$DebugRebarBand@cx eax
    ;mov D$DebugRebarBand@cxIdeal eax
    ;mov D$DebugRebarBand@cxMinChild eax

    SendMessage D$DebugRebarHandle, &RB_INSERTBAND, BAND_COMMANDBAR, DebugRebarBand
    SendMessage D$DebugRebarHandle, &RB_SHOWBAND, BAND_COMMANDBAR, 1
EndP
____________________________________________________________________________________________

[BAND_MENUBAR 0 BAND_COMMANDBAR 1 BAND_FLAGS 2 BAND_FPUFLAGS 3]

Proc DebugDialog_CreateToolbar:

    Call 'ComCtl32.InitCommonControlsEx' Init_Common_Controls

  ; Create rebar
    Call 'User32.CreateWindowExA',
        &WS_EX_TOOLWINDOW, {'ReBarWindow32' 0}, 0,
        &WS_VISIBLE+&WS_CHILD+&RBS_VARHEIGHT+&RBS_FIXEDORDER+&RBS_BANDBORDERS+&WS_BORDER+&RBS_VERTICALGRIPPER, ;+&CCS_NODIVIDER,
        0, 0, 0, 0,
        D$H.DebugDialog, DEBUGDLG_REBAR,
        D$H.Instance, 0

    Mov D$DebugRebarHandle eax

  ; Create menubar
    Call DebugWindow_CreateMenu

  ; Create flag toolbar
    Call 'User32.CreateWindowExA' 0, {'ToolbarWindow32' 0}, 0,
        DEBUG_TOOLBAR_STYLE,
        0, 0, 80, 0, D$H.DebugDialog, DEBUGDLG_FLAGS, D$H.Instance, 0

    Mov D$Debug_FlagBarHandle eax
    SendMessage eax, &WM_SETFONT, D$DialogFont_handle, 0

  ; Create FPU flag toolbar
    Call 'User32.CreateWindowExA' 0, {'ToolbarWindow32' 0}, 0,
        DEBUG_TOOLBAR_STYLE,
        0, 0, 80, 0, D$H.DebugDialog, DEBUGDLG_FPUFLAGS, D$H.Instance, 0

    Mov D$Debug_FPUBarHandle eax
    SendMessage eax, &WM_SETFONT, D$DialogFont_handle, 0

  ; Send the TB_BUTTONSTRUCTSIZE message, which is required for
  ; backward compatibility.
    SendMessage D$Debug_FlagBarHandle, &TB_BUTTONSTRUCTSIZE, 20, 0
    SendMessage D$Debug_FPUBarHandle, &TB_BUTTONSTRUCTSIZE, 20, 0

    Call DebugDialog_CreateCommandTB

  ; Add buttons
    SendMessage D$Debug_FlagBarHandle, &TB_ADDBUTTONS, 9, DebugFlagButtons
    SendMessage D$Debug_FPUBarHandle, &TB_ADDBUTTONS, 4, DebugFPUButtons

  ; Add the bands
    move D$DebugRebarBand@hwndChild D$Debug_FlagbarHandle
    SendMessage D$Debug_FlagbarHandle, &TB_GETMAXSIZE, 0, Point
    Mov eax D$PointX ;| add eax 8
    ;Call 'user32.GetClientRect'  D$Debug_FlagbarHandle, DebugRect
    ;mov eax D$DebugRect@right | sub eax D$DebugRect@left

    ;mov  D$DebugRebarBand@cxIdeal eax
    Mov  D$DebugRebarBand@cx eax
    ;mov  D$DebugRebarBand@cxMinChild eax
    SendMessage D$DebugRebarHandle, &RB_INSERTBAND, BAND_FLAGS, DebugRebarBand
    SendMessage D$DebugRebarHandle, &RB_SHOWBAND, BAND_FLAGS, 1

    move D$DebugRebarBand@hwndChild D$Debug_FPUbarHandle
    SendMessage D$Debug_FPUbarHandle, &TB_GETMAXSIZE, 0, Point
    Mov eax D$PointX ;| add eax 8
    ;Call 'user32.GetClientRect'  D$Debug_FPUbarHandle, DebugRect
    ;mov eax D$DebugRect@right | sub eax D$DebugRect@left

    ;mov  D$DebugRebarBand@cxIdeal eax
    Mov  D$DebugRebarBand@cx eax
    ;mov  D$DebugRebarBand@cxMinChild eax
    or   D$DebugRebarBand@fStyle &RBBS_HIDDEN
    SendMessage D$DebugRebarHandle, &RB_INSERTBAND, 0-1, DebugRebarBand

  ; Create the context menu
    Call 'USER32.CreatePopupMenu' | Mov ebx eax, D$Debug_ToolbarMenu eax
    ;Call 'User32.AppendMenuA' ebx, &MF_STRING, DEBUGDLG_TB_CONFIGURE, {'Configure ...' 0}
    Call 'User32.AppendMenuA' ebx, &MF_STRING, DEBUGDLG_TB_SHOW_TEXT, {'Show text' 0}
    Call 'User32.AppendMenuA' ebx, &MF_SEPARATOR, 0, 0
    Call 'User32.AppendMenuA' ebx, &MF_STRING+&MF_CHECKED, DEBUGDLG_TB_SHOW_CMDS, {'Show commands' 0}
    Call 'User32.AppendMenuA' ebx, &MF_STRING+&MF_CHECKED, DEBUGDLG_TB_SHOW_FLAGS, {'Show flags' 0}
    Call 'User32.AppendMenuA' ebx, &MF_STRING, DEBUGDLG_TB_SHOW_FPU, {'Show FPU flags' 0}

;;
    SendMessage D@TbHandle, &TB_GETITEMRECT, 0, DebugRect    
    Mov eax D$DebugRect@width | shl eax 16
    SendMessage D@TbHandle, &TB_SETBUTTONWIDTH, 0, eax
;;
;;
    SendMessage D$Debug_ToolbarHandle, &TB_AUTOSIZE, 0, 0 
    SendMessage D$Debug_FlagbarHandle, &TB_AUTOSIZE, 0, 0 
    SendMessage D$Debug_FPUbarHandle,  &TB_AUTOSIZE, 0, 0 
;;
    ;Call 'User32.ShowWindow' D@TbHandle, &SW_SHOW
EndP
____________________________________________________________________________________________

DebugDialog_DestroyToolbar:
    Call 'USER32.DestroyWindow' D$DebugToolbarHandle
    Mov D$DebugToolbarHandle 0
    Call 'USER32.DestroyWindow' D$DebugRebarHandle
    Mov D$DebugRebarHandle 0
    Call 'USER32.DestroyMenu' D$Debug_ToolbarMenu
    Mov D$Debug_ToolbarMenu 0
    Mov D$DebugDialog.RebarHeight 0
ret
____________________________________________________________________________________________

; User rightclicked on the dialog. Test if it is in the rebar and show context menu.
; Position is given in screen coordinates.

Proc DebugDialog_RebarHitTest:
    Arguments @X @Y
    Structure @RBHitTest 16, @pt.x 0,  @pt.y 4,  @flags 8,  @iBand 12

  ; Hittest expects client coordinates
    move D@pt.x D@X, D@pt.y D@Y
    Call 'USER32.ScreenToClient' D$Debug_RebarHandle, D@RBHitTest
    SendMessage D$Debug_RebarHandle, &RB_HITTEST, 0, D@RBHitTest

  ; Show context menu
    If D@iBand <> 0-1
        Call 'USER32.TrackPopupMenu' D$Debug_ToolbarMenu, 0, D@X, D@Y, 0, D$H.DebugDialog, 0
    EndIf
EndP
____________________________________________________________________________________________

; Show / Hide rebar-band containing a toolbar.
; Command references the menu item clicked.

Proc DebugDialog_ToggleToolbar:
    Arguments @Command

  ; store band index in ebx
    If D@Command = DEBUGDLG_TB_SHOW_CMDS
        Mov ebx 1
    ElseIf D@Command = DEBUGDLG_TB_SHOW_FLAGS
        Mov ebx 2
    ElseIf D@Command = DEBUGDLG_TB_SHOW_FPU
        Mov ebx 3
    EndIf

  ; get check state of menu item, invert, toggle band and set inverted check state
    Call 'User32.GetMenuState' D$Debug_ToolbarMenu, D@Command, &MF_BYCOMMAND
    push eax
        xor edx edx | test eax &MF_CHECKED | setz dl
        SendMessage D$Debug_RebarHandle, &RB_SHOWBAND, ebx, edx
    pop eax
    xor eax &MF_CHECKED | and eax &MF_CHECKED
    Call 'USER32.CheckMenuItem' D$Debug_ToolbarMenu, D@Command, eax
EndP
____________________________________________________________________________________________

Proc DebugDialog_ToggleToolbarText:
    Structure @TBButtonInfo 32, @Size 0, @Mask 4, @Text 24

    Mov D@Size 32, D@Mask &TBIF_TEXT

  ; get check state of menu item, invert, toggle band and set inverted check state
    Call 'User32.GetMenuState' D$Debug_ToolbarMenu, DEBUGDLG_TB_SHOWTEXT, &MF_BYCOMMAND
    push eax
        xor edx edx | test eax &MF_CHECKED | setz dl
        Mov B$DebugShowTBText dl
        Call DebugDialog_CreateCommandTB
;;
      ; TODO show text
        Mov ebx 0
        While ebx < 6
            Mov D@Text 0
            SendMessage D$Debug_ToolbarHandle, &TB_SETBUTTONINFOA, ebx, D@TBButtonInfo
            If eax = 0
                Call ReportWinError {'SetButtonInfo' 0}
            EndIf
            inc ebx
        EndWhile
;;
    pop eax
    xor eax &MF_CHECKED | and eax &MF_CHECKED
    Call 'USER32.CheckMenuItem' D$Debug_ToolbarMenu, DEBUGDLG_TB_SHOWTEXT, eax
    SendMessage D$Debug_ToolbarHandle, &TB_AUTOSIZE, 0, 0

EndP
____________________________________________________________________________________________

[DebugDialog.RebarHeight: ?]

Proc DebugDialog_RebarNotify:
    Arguments @msg

  ; Reposition all other child windows
    .If D@msg = &RBN_HEIGHTCHANGE
        SendMessage D$DebugRebarHandle, &RB_GETBARHEIGHT, 0, 0
        Mov edx D$DebugDialog.RebarHeight
        Mov D$DebugDialog.RebarHeight eax
        sub eax edx | Mov ebx eax

        Call AdjustControlPos D$H.DebugDialog, DEBUGDLG_REGISTER_TAB, 0, ebx
        Call AdjustControlPos D$H.DebugDialog, DEBUGDLG_FORMAT_COMBO, 0, ebx
        Call AdjustControlPos D$H.DebugDialog, DEBUGDLG_REG_LIST, 0, ebx
        Call AdjustControlPos D$H.DebugDialog, DEBUGDLG_DATA_TAB, 0, ebx
        Call AdjustControlPos D$H.DebugDialog, 3, 0, ebx
        Mov esi 71
        While esi < 79
            Call AdjustControlPos D$H.DebugDialog, esi, 0, ebx
            inc esi
        EndWhile
        neg ebx
        Call AdjustControlSize D$H.DebugDialog, DEBUGDLG_DATA_TAB, 0, ebx
        Call DebugDialog_AdjustDataTabSize D$H.DebugDialog

        Call 'USER32.InvalidateRect' D$H.DebugDialog, &NULL, &TRUE
    .EndIf
EndP
____________________________________________________________________________________________

Proc DebugDialog_SaveToolbarSettings:
    Arguments @FileHandle
    Structure @Tbar 36, @Id 0, @Size 4, @Flags 8, @X1 12, @X2 16, @X3 20, @Style1 24, @Style2 28, @Style3 32

    Mov D@Id 'TBar', D@Size 28

    Mov al B$DebugShowTBText | Mov B@Flags al

    SendMessage D$DebugRebarHandle, &RB_GETBANDINFO, 1, DebugRebarBand
    move D@Style1 D$DebugRebarBand@fStyle
    move D@X1 D$DebugRebarBand@cx

    SendMessage D$DebugRebarHandle, &RB_GETBANDINFO, 2, DebugRebarBand
    move D@Style2 D$DebugRebarBand@fStyle
    move D@X2 D$DebugRebarBand@cx

    SendMessage D$DebugRebarHandle, &RB_GETBANDINFO, 3, DebugRebarBand
    move D@Style3 D$DebugRebarBand@fStyle
    move D@X3 D$DebugRebarBand@cx

    Call 'KERNEL32.WriteFile' D@FileHandle, D@Tbar, 36, BytesTransfered, 0
EndP
____________________________________________________________________________________________

Proc DebugDialog_LoadToolbarSettings:
    Arguments @FileHandle @Size
    Structure @Tbar 28, @Flags 0, @X1 4, @X2 8, @X3 12, @Style1 16, @Style2 20, @Style3 24

    Mov eax 0
    On D@Size <> 28, ExitP

    Call 'KERNEL32.ReadFile' D@FileHandle, D@Tbar, D@Size, BytesTransfered, 0

    On B@Flags <> 0,
        Call DebugDialog_ToggleToolbarText

    Mov ebx D$DebugRebarBand@fMask
    Mov D$DebugRebarBand@fMask &RBBIM_STYLE+&RBBIM_SIZE

    move D$DebugRebarBand@fStyle D@Style1
    move D$DebugRebarBand@cx D@X1
    SendMessage D$DebugRebarHandle, &RB_SETBANDINFO, 1, DebugRebarBand

    move D$DebugRebarBand@fStyle D@Style2
    move D$DebugRebarBand@cx D@X2
    SendMessage D$DebugRebarHandle, &RB_SETBANDINFO, 2, DebugRebarBand

    move D$DebugRebarBand@fStyle D@Style3
    move D$DebugRebarBand@cx D@X3
    SendMessage D$DebugRebarHandle, &RB_SETBANDINFO, 3, DebugRebarBand

    xor eax eax | test D@Style1 &RBBS_HIDDEN | setz al | shl al 3
    Call 'USER32.CheckMenuItem' D$Debug_ToolbarMenu, DEBUGDLG_TB_SHOW_CMDS, eax
    xor eax eax | test D@Style2 &RBBS_HIDDEN | setz al | shl al 3
    Call 'USER32.CheckMenuItem' D$Debug_ToolbarMenu, DEBUGDLG_TB_SHOW_FLAGS, eax
    xor eax eax | test D@Style3 &RBBS_HIDDEN | setz al | shl al 3
    Call 'USER32.CheckMenuItem' D$Debug_ToolbarMenu, DEBUGDLG_TB_SHOW_FPU, eax

    Mov D$DebugRebarBand@fMask ebx

    Mov eax 1
EndP
____________________________________________________________________________________________

____________________________________________________________________________________________
____________________________________________________________________________________________

[DEBUGLOGFONT:
 DEBUGLOGFONT.lfHeight: D$ 0_FFFF_FFF5
 DEBUGLOGFONT.lfWidth: D$ 0
 DEBUGLOGFONT.lfEscapement: D$ 0
 DEBUGLOGFONT.lfOrientation: D$ 0
 DEBUGLOGFONT.lfWeight: D$ 0190
 DEBUGLOGFONT.lfItalic: B$ 0
 DEBUGLOGFONT.lfUnderline: B$ 0
 DEBUGLOGFONT.lfStrikeOut: B$ 0
 DEBUGLOGFONT.lfCharSet: B$ 0
 DEBUGLOGFONT.lfOutPrecision: B$ 03
 DEBUGLOGFONT.lfClipPrecision: B$ 02
 DEBUGLOGFONT.lfQuality: B$ 01
 DEBUGLOGFONT.lfPitchAndFamily: B$ 031]
[DEBUGLOGFONT.lfFaceName: B$ 'Courier New' 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]

[DEBUGCHOOSEFONT:
 DEBUGCHOOSEFONT.lStructSize: D$ len
 DEBUGCHOOSEFONT.hwndOwner: D$ 0
 DEBUGCHOOSEFONT.hDC: D$ 0
 DEBUGCHOOSEFONT.lpLogFont: D$ DEBUGLOGFONT
 DEBUGCHOOSEFONT.iPointSize: D$ 0
 DEBUGCHOOSEFONT.Flags: D$ &CF_SCREENFONTS__&CF_FIXEDPITCHONLY
 DEBUGCHOOSEFONT.rgbColors: D$ 0
 DEBUGCHOOSEFONT.lCustData: D$ 0
 DEBUGCHOOSEFONT.lpfnHook: D$ 0
 DEBUGCHOOSEFONT.lpTemplateName: D$ 0
 DEBUGCHOOSEFONT.hInstance: D$ 0
 DEBUGCHOOSEFONT.lpszStyle: D$ 0
 DEBUGCHOOSEFONT.nFontType: W$ 0
 DEBUGCHOOSEFONT.Alignment: W$ 0
 DEBUGCHOOSEFONT.nSizeMin: D$ 0
 DEBUGCHOOSEFONT.nSizeMax: D$ 0]

[DebugFontHandle: ?]

Proc DebugDialog_SetFont:
    Arguments @hwnd

        On D$DebugFontHandle <> 0,
            Call 'GDI32.DeleteObject' D$DebugFontHandle
        Call 'GDI32.CreateFontIndirectA' DEBUGLOGFONT
        Mov D$DebugFontHandle eax
        Call MeasureFont
        SendMessage D@hwnd, &WM_SETFONT, D$DebugFontHandle, &TRUE
EndP
____________________________________________________________________________________________

; Get character extents for the owner-drawn lists.
; Create a temporary device context, set the font & measure string sizes.
; (only deals with fixed sized fonts)

[DebugFontCharWidth: ?]

Proc MeasureFont:
    Local @DC

    Call 'GDI32.CreateDCA' {'DISPLAY' 0}, 0, 0, 0
    Mov D@DC eax

    Call 'GDI32.SelectObject' D@DC, D$DebugFontHandle
    Call 'GDI32.GetTextExtentPoint32A' D@DC, {'M' 0}, 1, Point

    move D$DebugFontCharWidth D$PointX

    Call 'GDI32.DeleteDC' D@DC
EndP
____________________________________________________________________________________________

MeasureStringWidth:
    cmp B$UnicodeStrings 1 | je L0>
    jmp MeasureStringWidthA
L0: jmp MeasureStringWidthW

; General purpose string measurement for owner-drawn controls

Proc MeasureStringWidthA:
    Arguments @String, @Font
    Local @DC
    Uses edi

    Call 'GDI32.CreateDCA' {'DISPLAY' 0}, 0, 0, 0
    Mov D@DC eax

    Call 'GDI32.SelectObject' D@DC, D@Font

    Mov edi D@String, ecx 0-1, al 0
    repne scasb
    Mov eax 0-2 | sub eax ecx

    Call 'GDI32.GetTextExtentPoint32A' D@DC, D@String, eax, Point
    Call 'GDI32.DeleteDC' D@DC

    Mov eax D$PointX
EndP
____________________________________________________________________________________________

Proc MeasureStringWidthW:
    Arguments @String, @Font
    Local @DC
    Uses edi

    Call 'GDI32.CreateDCA' {'DISPLAY' 0}, 0, 0, 0
    Mov D@DC eax

    Call 'GDI32.SelectObject' D@DC, D@Font

    Mov edi D@String, ecx 0-1, ax 0
    repne scasw
    Mov eax 0-2 | sub eax ecx

    Call 'GDI32.GetTextExtentPoint32W' D@DC, D@String, eax, Point
    Call 'GDI32.DeleteDC' D@DC

    Mov eax D$PointX
EndP
____________________________________________________________________________________________

; Present font dialog

Proc DebugDialog_ChangeFont:
    Arguments @hwnd

        move D$DEBUGCHOOSEFONT.hwndOwner D@hwnd
        Call 'COMDLG32.ChooseFontA' DEBUGCHOOSEFONT    ; user sets the font:
        If eax > 0
            Call DebugDialog_SetFont D@hwnd
        End_If
EndP
____________________________________________________________________________________________

; Compare the register contents (general purpose) for changed values since last tag.
; Mark changed registers in the bit mask (e.g. bit0=1 means: EAX has changed)

[OldGPR_Values: ? #8 GPR_Modified_Mask: ? GPR_FirstTime: ?]

Proc TagGPRModified:
    Uses esi edi

    Mov ecx 0, edx 0
    Mov esi GPRRegMap, edi OldGPR_Values
    Do
        lodsd | Mov eax D$eax
        cmp eax D$edi | je L0>
            bts edx ecx ; tag as changed
L0:     stosd
        inc ecx
    Loop_Until ecx = 8

    If D$GPR_FirstTime = 1
        Mov edx 0
        Mov D$GPR_FirstTime 0
    EndIf

    Mov D$GPR_Modified_Mask edx
EndP
____________________________________________________________________________________________

[DebugConfig: 'debug.cfg' 0]

; Load debugger configuration from file (in RosAsmFiles folder) if available

Proc LoadDebugConfig:
    Local @File, @Value, @Size, @Id
    Uses edi, esi

    Call GetRosAsmFilesPath

    Mov edi RosAsmFilesPath, eax 0, ecx 0-1
    repne scasb | dec edi
    Mov esi DebugConfig, ecx 13
    rep movsb

    Call 'Kernel32.CreateFileA' RosAsmFilesPath, &GENERIC_READ, &FILE_SHARE_READ, 0,
        &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0

    Mov B$edi 0

    On eax = &INVALID_HANDLE_VALUE, ExitP
    Mov D@File eax

L0: lea eax D@Id
    Call 'KERNEL32.ReadFile' D@File, eax, 8, BytesTransfered, 0
    ..If D$BytesTransfered = 8
        .If D@Id = 'RegX'
          ; Show seg&debug regs (0=No [default]; 1=Show)
            lea eax D@Value
            Call 'KERNEL32.ReadFile' D@File, eax, 4, BytesTransfered, 0
            On D@Value = 1,
                SendMessage D$H.DebugDialog, &WM_COMMAND, M02_Show_All, 0
        .ElseIf D@Id = 'Step'
          ; Stepping mode (0=instruction level [default]; 1=source level)
            lea eax D@Value
            Call 'KERNEL32.ReadFile' D@File, eax, 4, BytesTransfered, 0
            On D@Value = 1,
                SendMessage D$H.DebugDialog, &WM_COMMAND, M02_Source_Level, 0
        .ElseIf D@Id = 'Font'
            Call 'KERNEL32.ReadFile' D@File, DebugLogFont, D@Size, BytesTransfered, 0
            Call DebugDialog_SetFont D$H.DebugDialog
        .ElseIf D@Id = 'Rect'
            Call 'KERNEL32.ReadFile' D@File, DebugRect, D@Size, BytesTransfered, 0
            Mov eax D$DebugRect@right | sub eax D$DebugRect@left
            Mov edx D$DebugRect@bottom | sub edx D$DebugRect@top
            Call 'USER32.MoveWindow' D$H.DebugDialog, D$DebugRect@left, D$DebugRect@top, eax, edx, &TRUE
        .ElseIf D@Id = 'TBar'
            Call DebugDialog_LoadToolbarSettings D@File, D@Size
            cmp eax 0 | je L9>
        .ElseIf D@Id = 'LaLa'
            Call DataView_LoadSettings D@File, D@Size
        .Else
          ; Unknown chunk, skip it
L9:         Call 'KERNEL32.SetFilePointer' D@File, D@Size, 0, &FILE_CURRENT
        .EndIf
        jmp L0<<
    ..EndIf

    Call 'KERNEL32.CloseHandle' D@File
EndP
____________________________________________________________________________________________

; Write debugger configuration to a file

Proc SaveDebugConfig:
    Local @File, @Value, @Size, @Id
    Uses edi, esi

    Call GetRosAsmFilesPath

    Mov edi RosAsmFilesPath, eax 0, ecx 0-1
    repne scasb | dec edi
    Mov esi DebugConfig, ecx 13
    rep movsb

    Call 'Kernel32.CreateFileA' RosAsmFilesPath, &GENERIC_WRITE, &FILE_SHARE_READ, 0,
        &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0

    Mov B$edi 0

    On eax = &INVALID_HANDLE_VALUE, ExitP
    Mov D@File eax

  ; Show segment & debug regs
    Call 'User32.GetMenuState' D$SettingsMenu, M02_Show_All, &MF_BYCOMMAND
    and eax &MF_CHECKED
    If eax <> 0
        Mov D@Value 1
    Else
        Mov D@Value 0
    EndIf
    Mov D@Id 'RegX'
    Mov D@Size 4
    lea eax D@Id
    Call 'KERNEL32.WriteFile' D@File, eax, 12, BytesTransfered, 0

  ; Stepping Mode
    Mov D@Id 'Step'
    move D@Value D$Stepping
    lea eax D@Id
    Call 'KERNEL32.WriteFile' D@File, eax, 12, BytesTransfered, 0

  ; Font
    Mov D@Id 'Font'
    Mov D@Size 60
    lea eax D@Id
    Call 'KERNEL32.WriteFile' D@File, eax, 8, BytesTransfered, 0
    Call 'KERNEL32.WriteFile' D@File, DebugLogFont, D@Size, BytesTransfered, 0

  ; Position
    Mov D@Id 'Rect'
    Mov D@Size 16
    Call 'USER32.GetWindowRect' D$H.DebugDialog, DebugRect
    lea eax D@Id
    Call 'KERNEL32.WriteFile' D@File, eax, 8, BytesTransfered, 0
    Call 'KERNEL32.WriteFile' D@File, DebugRect, D@Size, BytesTransfered, 0

    Call DebugDialog_SaveToolbarSettings D@File
    Call DataView_SaveSettings D@File

    Call 'KERNEL32.CloseHandle' D@File
EndP
____________________________________________________________________________________________

[CurrentStackPointer: ?]

Proc DebugDialog_ShowCaller:
    If D$CurrentStackPointer = 0
        move D$CurrentStackPointer D$C.regEsp
    EndIf

    Call ScanStackForCodePointer D$CurrentStackPointer
    add edx 4
    add D$CurrentStackPointer edx

    dec eax
    Call SourceDebugPos eax
EndP
____________________________________________________________________________________________

; Process WM_DEBUGEVENT message.

Proc DebugDialog_OnDebugEvent:
    Arguments @hwnd
    Local @Inside

      ; Find changed registers
        Call TagGPRModified

      ; Find position in source
        Call SourceDebugPos D$SourcePosCodeAddress
        Call IsProcessCode D$C.regEip | Mov D@Inside eax

      ; Copy exception description
        Mov edi DebugCaption
        .If D$DebugEventType = DET_BP
            Mov D$edi 'BP  ' | add edi 3
        .ElseIf D$DebugEventType = DET_STEP
            Mov D$edi 'STEP', B$edi+4 ' ' | add edi 5
        .ElseIf D$DebugEventType = DET_WP
            Mov D$edi 'WP  ' | add edi 3
        .ElseIf D$DebugEventType = DET_EXCEPTION
            Mov D$edi 'EXCE', D$edi+4 'PTIO', W$edi+8 'N ' | add edi 10
        .EndIf

        ;mov esi D$BreakTitle, edi DebugCaption
        ;While B$esi <> 0 | movsb | EndWhile

        ; Under 95 we cannot access the process memory after an exception has occurred
        ; Step-Over and InstructionDecode wouldn't work for that reason. (really?)
        ...If D$ExceptionFlags = 0
            Call NextInstructionDecode
            ; For source level stepping, compare the last source pos with the current
            ; if it hasn't changed, continue to step into/over.
            ; UPDATE: Only when stepping inside the process code
            Mov eax D$SourcePos
            ..If eax = D$LastSourcePos
                .If D@Inside = &TRUE
                    If B$Stepping = 1
                        Call DebugDialog_ContinueDebuggee
                        ExitP
                    EndIf
                .EndIf
            ..Else
                Mov D$LastSourcePos eax
            ..EndIf
            ; Show next instruction in caption, e.g. "BREAKPOINT - [push eax]"
            ;mov eax ' - [' | stosd
            Mov al '[' | stosb
            Mov esi D$NextInstructionPtr
            While B$esi <> 0 | movsb | EndWhile
            Mov al ']' | stosb
        ...End_If

        On D$ExceptionFlags = 0,
            Call DebugDialog_EnableContinueMenu &TRUE
;;
        test D$ExceptionFlags E_MUSTEXIT | jnz L0>
            Call DebugDialog_EnableContinueMenu &TRUE
L0:
        test D$ExceptionFlags E_OUTSIDE | jnz L0>
L0:     
;;

      ; Show current module name
        If D$CurrentModule <> 0
            push esi
                Mov esi D$CurrentModule
                Mov B$edi ' ' | inc edi
                While B$esi <> 0 | movsb | EndWhile
            pop esi
        EndIf

      ; Show caption
        Mov B$edi 0
        SendMessage D@hwnd, &WM_SETTEXT, 0, DebugCaption

        ; Refresh register content output & flags
        Call DebugDialog_OnChangeRegisterTab D@hwnd
        Call DebugDialog_UpdateFlags D@hwnd
        Call DebugDialog_UpdateRegisterButtons D@hwnd
        Call 'User32.IsIconic' D$H.MainWindow
        If eax = &TRUE
            Call 'USER32.ShowWindow' D$H.MainWindow, &SW_RESTORE
        EndIf
        Call 'USER32.ShowWindow' D@hwnd, &SW_RESTORE
        Call 'USER32.SetForegroundWindow' D$H.MainWindow
        Call 'USER32.SetForegroundWindow' D@hwnd

      ; Refresh the data dialogs - they shall reload the displayed data from the debuggee
        SendMessage D$H.CurrentDataPage, WM_REFRESH_CONTENT, 0, 0

        On D$DebugEventType = DET_WP,
            SendMessage D$DataViewHandle, WM_SELECT_SYMBOL, D$WatchedAddress, 0

        On D$ExceptionFlags <> 0,
            Call ShowExceptionInfo

        Mov D$CurrentStackPointer 0

        ;test D$ExceptionFlags E_OUTSIDE | jz L0>
        ;    Call 'USER32.MessageBoxA' D$H.MainWindow, ErrorOutside, ErrorOutsideTitle, &MB_ICONEXCLAMATION
L0:     Mov eax 0
EndP
____________________________________________________________________________________________

; Kill Debugger

Proc DebugDialog_KillDebugger:
  ; Tried to kill debug-dialog while debugger is still running
    Mov D$TerminateDebuggee &TRUE, D$DialogKillsDebugger &TRUE

    .If D$IsDebugEvent = 1
      ; If the debug-thread waits for an user input event, simulate that the user pressed
      ; "Terminate Debuggee". If we don't do that, the debugger will wait forever--
        Call DebugDialog_ContinueDebuggee
    .Else
      ; Wait for the debug-thread to terminate. After a few seconds the debuggee and the
      ; debugger thread is terminated if the debugger thread does not exit voluntarily.
        Call 'KERNEL32.WaitForSingleObject' D$DebugThreadHandle, 5000
        If eax = &WAIT_TIMEOUT

            Call MessageBox CriticalError,
                            DebugThreadHangs,
                            &MB_SYSTEMMODAL+&MB_ICONEXCLAMATION

            Call CloseProcess

            Call 'KERNEL32.TerminateThread' D$DebugThreadHandle,
                                            0

        EndIf
        Mov D$DebugThreadHandle &NULL
    .EndIf
EndP
____________________________________________________________________________________________

; Process WM_CREATE message. Create child windows and load configuration.

Proc DebugDialog_OnCreate:
    Arguments @hwnd

        move D$H.DebugDialog D@hwnd
        ;Call 'USER32.GetMenu' D@hwnd | Mov D$DebugMenuHandle eax

      ; Hide tree
        On D$ShowTreeHandle <> 0, Call 'USER32.ShowWindow' D$ShowTreeHandle &SW_HIDE

      ; Init the flags
        Mov D$GPR_FirstTime 1, D$GPR_Modified_Mask 0
        Mov D$TerminateDebuggee &FALSE, D$DialogKillsDebugger &FALSE
        Mov D$HoldOnBreakpoints &TRUE, D$PauseThreads &FALSE

        Call 'User32.GetClientRect' D@hwnd, DebugRect
        move W$DebugDialog.Width W$DebugRect@width
        move W$DebugDialog.Height W$DebugRect@height

      ; Place dialog in upper right corner of the source editor.
      ; The position is overwritten when the configuration file is loaded so it
      ; only has any effect when no config file is available.
        Call 'User32.GetClientRect' D$H.MainWindow, DebugRect
        Mov esi D$DebugRect@width
        Mov edi D$DebugRect@height
        Call 'User32.GetWindowRect' D@hwnd, DebugRect
        Mov eax D$DebugRect@left
        sub D$DebugRect@right eax ; width
        sub esi D$DebugRect@width
        Mov D$DebugRect@left esi ; xpos
        Mov eax D$DebugRect@top
        sub D$DebugRect@bottom eax ; height
        If edi > D$DebugRect@bottom
            Mov D$DebugRect@bottom edi
        EndIf
        Mov D$DebugRect@top 0
        Call 'USER32.ClientToScreen' D$H.MainWindow, DebugRect
        Call 'USER32.MoveWindow' D@hwnd, D$DebugRect@left, D$DebugRect@top,
                                 D$DebugRect@right, D$DebugRect@bottom, &TRUE

      ; Key mapping
        Call 'User32.CreateAcceleratorTableA' DbgAccels, DbgAccelsNum
        Mov D$H.DebbugAccel eax

      ; Create default font
        Mov eax D$NationalFontHandle
        If eax <> 0
            Mov D$DialogFont_handle eax
        Else
            Call 'GDI32.CreateFontA' 8 4 0 0 400 0 0 0 1,   ;  DEFAULT_CHARSET 1  OEM_CHARSET 255
                                     0 0 0 0 Helv
            Mov D$DialogFont_handle eax
        EndIf

        ;SendMessage eax, &WM_SETFONT, D$DialogFont_handle, 1
        ;SendMessage eax, &WM_SETFONT, D@hwnd, 1

      ; Create the image list
        Call DebugDialog_CreateImageList

      ; Create the child windows / menu, toolbar, tabs
        Call DebugDialog_CreateRegisterTabs D@hwnd
        Call DebugDialog_CreateDataTab D@hwnd
        Call DebugDialog_CreateToolbar
        Call DebugDialog_InitDbgMenu

      ; Create the monospace font
        Call DebugDialog_SetFont D@hwnd

        Call 'User32.SetWindowTextA' D@hwnd, {'Running ...' 0}

      ; Create the mouse hint form and timer
        Call InitMouseOverDataHints

      ; Finally load configuration from file if available. All windows must be created
      ; at this point.
        Call LoadDebugConfig

        Mov eax 0 ;&TRUE
EndP
____________________________________________________________________________________________

; Process WM_CLOSE message. Save configuration & send destroy message.

Proc DebugDialog_OnClose:
    Arguments @hwnd

      ; Save configuration
        Call SaveDebugConfig

      ; Terminate debugger if it is still running.
        Mov D$DebuggerReady &FALSE
        If D$FL.IsDebugging = &TRUE
            Call DebugDialog_KillDebugger
        End_If

      ; Kill timer and destroy mouse hint window
        Call DeleteMouseOverDataHints

      ; Destroy dialog
        Call 'User32.DestroyWindow' D@hwnd
        Mov D$H.DebugDialog 0

      ; Restore RosAsm windows
        On D$ShowTreeHandle <> 0, Call 'USER32.ShowWindow' D$ShowTreeHandle, &SW_SHOW
        Call 'User32.IsIconic' D$H.MainWindow
        If eax = &TRUE
            Call 'USER32.ShowWindow' D$H.MainWindow, &SW_RESTORE
        EndIf
        Call 'USER32.SetForegroundWindow' D$H.MainWindow
        Mov eax 0
EndP
____________________________________________________________________________________________

; Process WM_DESTROY message. Free resources, destroy child windows.

Proc DebugDialog_OnDestroy:
    Arguments @hwnd

      ; Destroy child windows
        Call 'USER32.DestroyWindow' D$DataViewHandle
        Call 'USER32.DestroyWindow' D$MemoryInspectorHandle
        Call 'USER32.DestroyWindow' D$CallStackFormHandle
        Call 'USER32.DestroyWindow' D$LogForm_handle
        Call 'USER32.DestroyWindow' D$ASForm_handle
        Mov D$H.CurrentDataPage 0

        Call DebugDialog_DestroyToolbar

      ; Free images
        Call DebugDialog_DestroyImageList

      ; Destroy key map
        Call 'USER32.DestroyAcceleratorTable' D$H.DebbugAccel
        Mov D$H.DebbugAccel 0

      ; Delete fonts
        If D$DebugFontHandle <> 0
            Call 'GDI32.DeleteObject' D$DebugFontHandle
            Mov D$DebugFontHandle 0
        EndIf
        If D$DialogFontHandle <> 0
            On D$NationalFontHandle = 0,
                Call 'GDI32.DeleteObject' D$DialogFontHandle
            Mov D$DialogFontHandle 0
        EndIf

        Mov eax 0
EndP
____________________________________________________________________________________________

; Process WM_COMMAND message. Menu, accelerator (key-press) and button notifications.

Proc DebugDialog_OnCommand:
    Arguments @hwnd, @wParam, @lParam

        movzx eax W@wParam
        movzx edx W@wParam+2

        .If eax = M02_Hold_on_BP
            Call 'User32.GetMenuState' D$SettingsMenu, M02_Hold_on_BP, &MF_BYCOMMAND
            xor B$HoldOnBreakpoints 1 ; bool toggle
            xor eax &MF_CHECKED | and eax &MF_CHECKED
            Call 'USER32.CheckMenuItem' D$SettingsMenu, M02_Hold_on_BP, eax

        .Else_if eax = M02_Show_All
            Call 'User32.GetMenuState' D$SettingsMenu, M02_Show_All, &MF_BYCOMMAND
            push eax
                xor edx edx | test eax &MF_CHECKED | setz dl
                Call DebugDialog_ShowAdvancedTabs D@hwnd edx
            pop eax
            xor eax &MF_CHECKED | and eax &MF_CHECKED
            Call 'USER32.CheckMenuItem' D$SettingsMenu, M02_Show_All, eax

        .Else_if eax = M02_Run
            Mov B$UserWants CONTINUE_RUN
            Call DebugDialog_ContinueDebuggee

        .Else_if eax = M02_Step_Over
            Mov B$UserWants CONTINUE_STEPOVER
            Call DebugDialog_ContinueDebuggee

        .Else_if eax = M02_Step_Into
            Mov B$UserWants CONTINUE_STEP
            Call DebugDialog_ContinueDebuggee

        .Else_if eax = M02_Return_to_Caller
            Mov B$UserWants CONTINUE_RETURNTOCALLER
            Call DebugDialog_ContinueDebuggee

        .Else_if eax = M02_Terminate
            Mov B$TerminateDebuggee &TRUE
            Call DebugDialog_ContinueDebuggee

        .Else_if eax = M02_Pause
            Mov B$PauseThreads &TRUE

        .Else_If eax = M02_Show_code_at
            Call CreateNewForm_CodeAddressForm

        .Else_if eax = M02_Inst_Level
            Call 'USER32.CheckMenuRadioItem' D$SettingsMenu, M02_Inst_Level, M02_Source_Level,
                                             M02_Inst_Level, &MF_BYCOMMAND
            Mov B$Stepping 0

        .Else_if eax = M02_Source_Level
            Call 'USER32.CheckMenuRadioItem' D$SettingsMenu, M02_Inst_Level, M02_Source_Level,
                                             M02_Source_Level, &MF_BYCOMMAND
            Mov B$Stepping 1

        .Else_if eax = M02_Font
            Call DebugDialog_ChangeFont D@hwnd

        .Else_if eax = M02_CPU_Info
            VirtualAlloc DebugTextBuffer 4096
            Mov edi D$DebugTextBuffer
            Call DebugDialog_GetCPUInfo

            Call MessageBox {B$ "CPU INFORMATION:" EOS},
                            D$DebugTextBuffer,
                            &MB_SYSTEMMODAL+&MB_USERICON

            VirtualFree D$DebugTextBuffer

        .Else_if eax = M02_FPU_Status
            VirtualAlloc DebugTextBuffer 4096
            Mov edi D$DebugTextBuffer
            Call DebugDialog_GetFPUStatus

            Call MessageBox {B$ "FPU STATUS" EOS},
                            D$DebugTextBuffer,
                            &MB_SYSTEMMODAL+&MB_USERICON

            VirtualFree D$DebugTextBuffer

        .Else_if eax = M02_Help
            Call Help, B_U_AsmName, {'Debugger' 0}, ContextHlpMessage

        .Else_if eax = M02_About

            Call MessageBox DebuggerVersion,
                            D$DebugTextBuffer,
                            &MB_SYSTEMMODAL+&MB_USERICON

        .Else_If eax = DEBUGDLG_FORMAT_COMBO ; Representation Combo
            If edx = &CBN_SELCHANGE
                SendMessage D@lParam, &CB_GETCURSEL, 0, 0
                On eax <> &CB_ERR, Call DebugDialog_OnFormatChange D@hwnd eax
            End_If

        .Else_if eax = DEBUGDLG_TB_CONFIGURE
            ; TODO
        .Else_If eax = DEBUGDLG_TB_SHOW_CMDS
            Call DebugDialog_ToggleToolbar eax
        .Else_If eax = DEBUGDLG_TB_SHOW_FLAGS
            Call DebugDialog_ToggleToolbar eax
        .Else_If eax = DEBUGDLG_TB_SHOW_FPU
            Call DebugDialog_ToggleToolbar eax
        .Else_If eax = DEBUGDLG_TB_SHOW_TEXT
            Call DebugDialog_ToggleToolbarText

        .Else_if eax >= DEBUGDLG_FIRSTREG_BUTTON
            If eax <= DEBUGDLG_LASTREG_BUTTON
                sub eax DEBUGDLG_FIRSTREG_BUTTON
                Mov eax D$GPRRegMap+eax*4
                Mov ecx D$eax
                and ecx 0_FFFF_F000
                SendMessage D$MemoryInspectorHandle, WM_SET_PAGE, ecx, D$eax
            End_if
        .End_If
        Mov eax 0
EndP
____________________________________________________________________________________________

; Process WM_NOTIFY message. Handles tab selection changes and rebar height change.

Proc DebugDialog_OnNotify:
    Arguments @hwnd, @Notification

        Mov ebx D@Notification
        Mov edx D$ebx+4
        Mov eax D$ebx+8
        ..If edx = DEBUGDLG_REGISTER_TAB
            If eax = &TCN_SELCHANGE
                SendMessage D$ebx, &TCM_GETCURSEL, 0, 0
                Mov D$TabItem@imask &TCIF_PARAM
                SendMessage D$ebx, &TCM_GETITEM, eax, TabItem
                Call DebugDialog_OnChangeRegisterTab D@hwnd
            End_If
            Mov eax 0
        ..ElseIf edx = DEBUGDLG_DATA_TAB
            .If eax = &TCN_SELCHANGE
                SendMessage D$ebx, &TCM_GETCURSEL, 0, 0
                If eax = 0
                    Mov edi D$DataViewHandle
                ElseIf eax = 1
                    Mov edi D$MemoryInspectorHandle
                ElseIf eax = 2
                    Mov edi D$CallStackFormHandle
                ElseIf eax = 3
                    Mov edi D$LogForm_handle
                ElseIf eax = 4
                    Mov edi D$ASForm_handle
                EndIf
                Call 'USER32.ShowWindow' D$H.CurrentDataPage, &SW_HIDE
                Call 'USER32.ShowWindow' edi, &SW_SHOW
                Mov D$H.CurrentDataPage edi
                Call DebugDialog_AdjustDataTabSize D@hwnd
            .End_If
            Mov eax 0 ; mandatory for TCN_SELCHANGING !!!
        ..ElseIf edx = DEBUGDLG_REBAR
            Call DebugDialog_RebarNotify eax
            Mov eax 0
        ..ElseIf edx = DEBUGDLG_MENUBAR
            Call DebugDialog_MenubarNotify ebx eax D$ebx+12
        ..Else
            Mov eax 0
        ..EndIf

EndP
____________________________________________________________________________________________

; Process WM_SETFONT message. Propagate message to affected child windows.

Proc DebugDialog_OnSetFont:
    Arguments @hwnd @hFont, @Redraw

        SendMessage D$RegListHandle, &WM_SETFONT, D@hFont, D@Redraw
        SendMessage D$DataViewHandle, &WM_SETFONT, D@hFont, D@Redraw
        SendMessage D$MemoryInspectorHandle, &WM_SETFONT, D@hFont, D@Redraw
        SendMessage D$CallStackFormHandle, &WM_SETFONT, D@hFont, D@Redraw
        SendMessage D$LogFormHandle, &WM_SETFONT, D@hFont, D@Redraw
        SendMessage D$MouseHintFormHandle, &WM_SETFONT, D@hFont, D@Redraw
        SendMessage D$ASFormHandle, &WM_SETFONT, D@hFont, D@Redraw
        Mov ebx DEBUGDLG_FIRSTREG_BUTTON
        While ebx <= DEBUGDLG_LASTREG_BUTTON
            Call 'User32.GetDlgItem' D@hwnd, ebx
            SendMessage eax, &WM_SETFONT, D@hFont, D@Redraw
            inc ebx
        EndWhile
EndP
____________________________________________________________________________________________


____________________________________________________________________________________________

; Create the main debugger window.

CreateDebugWindow:

    move D$DebugWindowClass@hInstance D$H.Instance
    move D$DebugWindowClass@hIcon D$H.MainIcon
    move D$DebugWindowClass@hIconSm D$H.MainIcon
    move D$DebugWindowClass@hCursor D$H.MainIcon

    Call 'User32.RegisterClassExA' DebugWindowClass

    ;Call 'USER32.LoadMenuA' D$H.Instance, M02_MENU
    ;Call DebugWindow_CreateMenu

    Call 'User32.CreateWindowExA' 0,
        DebugWindowClassName,
        0,
        &WS_OVERLAPPEDWINDOW+&WS_VISIBLE+&WS_POPUP,
        0, 0, 160, 240,
        D$H.MainWindow, 0,
        D$H.Instance, 0

    If eax = 0
        Call ReportWinError {'Debugger: CreateWindowEx' 0}
        Mov eax 0 | ret
    EndIf

    Mov D$H.DebugDialog eax

    Mov eax 1
ret
____________________________________________________________________________________________
[DebugWindowClass:
 @cbSize:        D$ len
 @style:         D$ 11
 @lpfnWndProc:   D$ DebugDlgProc
 @cbClsExtra:    D$ 0
 @cbWndExtra:    D$ 0
 @hInstance:     D$ 0
 @hIcon:         D$ 0
 @hCursor:       D$ 0
 @hbrBackground: D$ 1
 @lpszMenuName:  D$ 0
 @lpszClassName: D$ DebugWindowClassName
 @hIconSm:       D$ 0]
[DebugWindowClassName: B$ "RosAsmDebugWindow" 0]
____________________________________________________________________________________________
; Custom events used to interchange informations between the debugger thread and the various
; windows/subwindows.

[WM_DEBUGEVENT      &WM_USER+1  ; debugger thread -> main debug dlg : when debug event occurs
 WM_BEGIN_DEBUG     &WM_USER+2  ; debugger thread -> main debug dlg : debuggee is set up
 WM_SET_PAGE        &WM_USER+3  ; various dlgs -> mem inspector : select virtual page
 WM_REFRESH_CONTENT &WM_USER+4  ; main debug dlg -> data dlgs : refresh data from debuggee
 WM_SELECT_SYMBOL   &WM_USER+5  ; main debug dlg -> data viewer : select symbol by address
 WM_LOG             &WM_USER+6  ; debugger thread -> main debug dlg : to log debug strings
 WM_POPUPMENU       &WM_USER+7  ; hook proc -> main debug dlg : popup menubar sub-menu
 WM_COLLAPSEMENU    &WM_USER+8] ; hook proc -> main debug dlg : collapse menubar sub-menu

; Accelerator table - assign key-combos to menu items

[DbgAccels:
 U$ &FVIRTKEY                   &VK_F6      M02_Run
    &FVIRTKEY                   &VK_F7      M02_Step_Into
    &FVIRTKEY                   &VK_F8      M02_Step_Over
    &FVIRTKEY+&FCONTROL         &VK_F7      M02_Return_to_Caller
    &FVIRTKEY+&FCONTROL         &VK_F12     M02_Pause
    &FVIRTKEY+&FCONTROL         &VK_F6      M02_Terminate]
    ;&FVIRTKEY+&FCONTROL         "F"         M02_FPU_Status


[DbgAccelsNum 6]
[H.DebbugAccel: ?]

;[BreakTitle: ?]
[DebugCaption: B$ ? #64]

; Debuggee is started.
[DebuggerReady: ?]

[TabItem:
 @imask: D$ ? #3
 @pszText: D$ ?
 @cchTextMax: D$ ?
 @iImage: D$ ?
 @lParam: D$ ?]

[DebugRect:
 @left: ?
 @top: ?
 @right: @width: ?
 @bottom: @height: ?]

[UserWants: ?] ; Execution control after debug event (step, run, ...)
[Stepping: ? LastSourcePos: ?]
[DbgLineHeight: 16]

[H.DebugDialog: D$?]
[RegListHandle: ?]; DebugMenuHandle: ?]
[MenubarHandle: ?]
[HoldOnBreakpoints: ? TerminateDebuggee: ? PauseThreads: ? DialogKillsDebugger: ?]

[ExceptionText: 'Exception occurred' 0]

[DebugTextBuffer: ?]

[DebugDialogSize:
 DebugDialog.Width: W$ ?
 DebugDialog.Height: W$ ?]

[DEBUGDLG_REGISTER_TAB 40
 DEBUGDLG_DATA_TAB 200
 DEBUGDLG_FORMAT_COMBO 60
 DEBUGDLG_REG_LIST 70
 DEBUGDLG_FIRSTREG_BUTTON 71
 DEBUGDLG_LASTREG_BUTTON 78
 DEBUGDLG_TB_CONFIGURE 300
 DEBUGDLG_TB_SHOW_CMDS 301
 DEBUGDLG_TB_SHOW_FLAGS 302
 DEBUGDLG_TB_SHOW_FPU 303
 DEBUGDLG_TB_SHOW_TEXT 304]

    [DialogFontHandle: ?]
    [DialogLOGFONTSTRUCT:  0  0  0  0  0  0  536870912 'MS Sans Serif' 0 0 0 0 0 0 ]

;;
    The main debugger window - related functions (right-click)
    
    'DebugDialog_InitDbgMenu', 'DebugDialog_CreateRegisterTabs', 'DebugDialog_CreateDataTab',
    'DebugDialog_InitRegisterListBox', 'DebugDialog_CreateRegisterButtons',
    'DebugDialog_SetFont', 'DebugDialog_ChangeFont', 'DebugDialog_ContinueDebuggee',
    'DebugDialog_OnChangeRegisterTab', 'DebugDialog_OnDrawRegisterItem', 'DebugDialog_OnFormatChange',
    'DebugDialog_UpdateFlags', 'DebugDialog_UpdateRegisterButtons', 
    'DebugDialog_ShowAdvancedTabs', 'DebugDialog_GetCPUInfo', 'DebugDialog_GetFPUStatus',
    'DebugDialog_AdjustDataTabSize', 'DebugDialog_ShowCaller'
;;

Proc DebugDlgProc:
    Arguments @hwnd @msg @wParam @lParam
    Uses ebx, esi, edi

    .If D@msg = &WM_CREATE
        Call DebugDialog_OnCreate D@hwnd

    .Else_If D@msg = &WM_CONTEXTMENU
        movzx eax W@lParam
        movzx ecx W@lParam+2
        Call DebugDialog_RebarHitTest eax ecx

    .Else_if D@msg = WM_BEGIN_DEBUG
        Mov D$DebuggerReady &TRUE
        Mov eax 0

    .Else_If D@msg = WM_LOG
        SendMessage D$LogForm_Handle, D@msg, D@wParam, D@lParam

    .Else_If D@msg = WM_DEBUGEVENT
        Call DebugDialog_OnDebugEvent D@hwnd

    .Else_If D@msg = WM_POPUPMENU
        Call DebugDialog_OnPopupMenu D@wParam

    .Else_If D@msg = WM_COLLAPSEMENU
        Call DebugDialog_OnCollapseMenu

    .Else_If D@msg = &WM_CLOSE
        Call DebugDialog_OnClose D@hwnd

    .Else_If D@msg = &WM_DESTROY
        Call DebugDialog_OnDestroy D@hwnd

    .Else_If D@msg = &WM_COMMAND
        Call DebugDialog_OnCommand D@hwnd, D@wParam, D@lParam

    .Else_If D@msg = &WM_SYSCOMMAND
        Call DebugDialog_OnSysCommand D@hwnd, D@wParam, D@lParam

    .Else_if D@msg = &WM_KEYDOWN
        Call DebugDialog_OnKeyDown D@hwnd, D@wParam, D@lParam

    .Else_If D@msg = &WM_NOTIFY
        Call DebugDialog_OnNotify D@hwnd, D@lParam

    .Else_if D@msg = &WM_GETMINMAXINFO
        Mov eax D@lParam
        Mov D$eax+24 200
        Mov D$eax+28 480
        Mov eax 0

    .Else_if D@msg = &WM_SIZE
        If D@wParam <> &SIZE_MINIMIZED
            Call DebugDialog_OnSize D@hwnd, D@lParam
        EndIf

    .Else_if D@msg = &WM_ACTIVATEAPP
        Mov eax 0

    .Else_if D@msg = &WM_SETFONT
        Call DebugDialog_OnSetFont D@hwnd, D@wParam, D@lParam

    .Else_if D@msg = &WM_DRAWITEM
        If D@wParam = 0
            Call DebugDialog_OnDrawMenuItem D@lParam
        Else
            Call DebugDialog_OnDrawRegisterItem D@lParam
        EndIf
        Mov eax &TRUE

    .Else_if D@msg = &WM_MEASUREITEM
        Mov eax D@lParam
        If D@wParam = 0
          ; menu
            Call DebugDialog_OnMeasureMenuItem D@lParam
        Else
          ; listbox
            move D$eax+16 D$DbgLineHeight
        EndIf
        Mov eax &TRUE

    .Else
        Call 'User32.DefWindowProcA' D@hwnd, D@msg, D@wParam, D@lParam
    .End_If

EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; MEMORY INSPECTOR
____________________________________________________________________________________________
____________________________________________________________________________________________

[MemInspectorSize:
 MemInspector.Width: W$ ?
 MemInspector.Height: W$ ?]

[MEMINSPECTOR_DATA_LIST 150
 MEMINSPECTOR_PAGE_EDIT 160
 MEMINSPECTOR_PREV_BUTTON 151
 MEMINSPECTOR_NEXT_BUTTON 152
 MEMINSPECTOR_TABLE_BUTTON 153
 MEMINSPECTOR_FORMAT_COMBO 170]

[MemFormatConversionProc: ?]

[CurrentPageAddress: ? CurrentPage: ?]

Proc MemoryInspector_OnDrawItem:
    Arguments @DrawItemStruc
    Local @Brush @Offset @Selected

    Mov ebx D@DrawItemStruc
    Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 0

    Mov D@Selected 0
    test D$ebx+DRAWITEM_ITEMSTATE &ODS_SELECTED
    setz al
    xor al 1
    Mov B@Selected al

    ; Print Offset
    Mov edx D$DebugFontCharWidth | shl edx 2 | add edx PADDING ; 4 chars
    Mov eax D$ebx+DRAWITEM_RCITEM_LEFT | Mov D$ItemRect@x1 eax
    add eax edx | Mov D$ItemRect@x2 eax
    move D$ItemRect@y1 D$ebx+DRAWITEM_RCITEM_TOP
    move D$ItemRect@y2 D$ebx+DRAWITEM_RCITEM_BOTTOM

    Mov edi D$ebx+DRAWITEM_ITEMID | and edi 1
    On B@Selected = 1, Mov edi 2
    Call 'GDI32.CreateSolidBrush' D$BackgroundCol@Name+edi*4 | Mov D@Brush eax
    Call 'USER32.FillRect' D$ebx+DRAWITEM_HDC ItemRect D@Brush
    Call 'GDI32.DeleteObject' D@Brush
    Call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC D$BackgroundCol@Name+edi*4

    Mov ecx D$ebx+DRAWITEM_ITEMDATA ; offset into page
    lea edi D@Offset
    Mov B$edi '+'
    Mov al ch | and al 0F
    add al '0'
    On al > '9', add al 7
    Mov B$edi+1 al
    Mov al cl | shr al 4
    add al '0'
    On al > '9', add al 7
    Mov B$edi+2 al
    Mov al cl | and al 0F
    add al '0'
    On al > '9', add al 7
    Mov B$edi+3 al

    Call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, edi, 4, ItemRect,
         (&DT_SINGLELINE+&DT_LEFT+&DT_VCENTER)
        ; (&DT_SINGLELINE+&DT_CENTER+&DT_VCENTER)
;;
  Modification proposed by AKar, because the &CENTER+&DT is causing some miss-alignments,
  in the Memory-View, when showing chinese Unicode.
;;

    ; Print value
    move D$ItemRect@x1 D$ItemRect@x2
    move D$ItemRect@x2 D$ebx+DRAWITEM_RCITEM_RIGHT

    Mov edi D$ebx+DRAWITEM_ITEMID | and edi 1
    On B@Selected = 1, Mov edi 2
    Call 'GDI32.CreateSolidBrush' D$BackgroundCol@Value+edi*4 | Mov D@Brush eax
    Call 'USER32.FillRect' D$ebx+DRAWITEM_HDC, ItemRect, D@Brush
    Call 'GDI32.DeleteObject' D@Brush
    Call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC, D$BackgroundCol@Value+edi*4

    If B@Selected = 1
        Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 0FF_FFFF
    EndIf

    Mov esi D$CurrentPage
    add esi D$ebx+DRAWITEM_ITEMDATA
    Mov edi ItemString
    Mov ecx 8
    Call D$MemFormatConversionProc

    Call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, ItemString, 0-1, ItemRect,
         (&DT_SINGLELINE+&DT_CENTER+&DT_VCENTER)

    ; Draw focus rect if selected
    If B@Selected = 1
        lea eax D$ebx+DRAWITEM_RCITEM_LEFT
        Call 'USER32.DrawFocusRect' D$ebx+DRAWITEM_HDC, eax
    EndIf
EndP

____________________________________________________________________________________________

; Converts string in Hexadecimal (uppercase) notation to a 32 bit integer. (no leading zeros)
;   esi : String
; Output
;   eax : Integer

HexStringToInt:
    Mov edx 0
    While B$esi <> 0

        lodsb

        ; Sort out invalid chars
        sub al '0' | js L6>  ; FAIL:    [esi] < '0'
        cmp al 9   | jle L0> ; SUCCESS: [esi] = '0'..'9'
        cmp al 17  | jl L6>  ; FAIL:    [esi] < 'A'
        sub al 7
        cmp al 16  | jae L6> ; FAIL:    [esi] > 'F'

L0:     shl edx 4
        or dl al

    EndWhile
    Mov eax edx
ret
; Invalid character error
L6: Call MessageBox {B$ "INVALID CHARACTER" EOS},
                    {B$ "Hexadecimal notation: Only 0-9 and A-F allowed !" EOS},
                    &MB_SYSTEMMODAL+&MB_ICONWARNING

    Mov eax 0-1
ret
____________________________________________________________________________________________

; Converts 32 bit integer to string in hexadecimal notation. (no leading zeros)
;   eax : Integer
; Output is stored in 'HexString'.

[HexString: B$ ? #10]

IntToHexString:
    push edi
        Mov ecx 8, edx eax, edi HexString

L0:     Mov eax 0
        shld eax edx 4
        shl edx 4

        add al '0'
        On al > '9', add al 7
        stosb
        loop L0<

        Mov B$edi 0
    pop edi
ret
____________________________________________________________________________________________

; When the user enters an address by hand and presses enter or the 'go' button, this
; proc is called. The string is converted to an integer. Then it is verified if it is
; a valid address. If the address is valid WM_SET_PAGE is sent to the dialog.
; Otherwise the old value is restored.

Proc MemInspector_OnGoToAddress:
    Arguments @hwnd
    Local @Address, @SegBase, @SegLimit
    Uses esi, edi

        Call 'USER32.GetDlgItemTextA' D@hwnd, MEMINSPECTOR_PAGEEDIT, HexString, 10
        Mov esi HexString
      ; Segment override?
        If W$esi = 'FS'
            move D@SegBase D$FS.Linear
            move D@SegLimit D$FS.Limit
            add esi 3
        Else
            Mov D@SegBase 0
            Mov D@SegLimit 0_FFFF_FFFF
        EndIf
        Call HexStringToInt
        If eax > D@SegLimit

            Call MessageBox {B$ "INVALID ADRESS" EOS},
                            {B$ "The offset is beyond the segment limit !" EOS},
                            &MB_SYSTEMMODAL+&MB_ICONWARNING

            jmp L0>
        EndIf

        add eax D@SegBase
        Mov D@Address eax

        Call IsProcessMemory D@Address
        If eax > 0
            Mov eax D@Address
            and eax PageBaseMask
            SendMessage D@hwnd, WM_SET_PAGE, eax, D@Address
        Else

            Call MessageBox {B$ "INVALID ADRESS" EOS},
                            {B$ "This is not a valid address !" EOS},
                            &MB_SYSTEMMODAL+&MB_ICONWARNING

L0:         Mov eax D$CurrentPageAddress
            Call IntToHexString
            Call 'User32.SetDlgItemTextA' D@hwnd, MEMINSPECTOR_PAGEEDIT, HexString
        EndIf
EndP
____________________________________________________________________________________________

; Tag Dialog 1012

Proc MemoryInspectorProc:
    Arguments @hwnd @msg @wParam @lParam
    Uses ebx, esi, edi

    ...If D@msg = &WM_INITDIALOG
        VirtualAlloc CurrentPage 4096
        Mov D$CurrentPageAddress 0

        Call 'USER32.GetClientRect' D@hwnd, DebugRect
        move W$MemInspector.Width W$DebugRect@width
        move W$MemInspector.Height W$DebugRect@height

        Call 'USER32.GetDlgItem' D@hwnd, MEMINSPECTOR_DATA_LIST | Mov ebx eax
        Mov esi 0
        While esi < 4096
            SendMessage ebx, &LB_ADDSTRING, 0, esi
            add esi 8
        EndWhile

        Call 'USER32.GetDlgItem' D@hwnd, MEMINSPECTOR_FORMAT_COMBO | Mov ebx eax
        SendMessage ebx, &CB_RESETCONTENT, 0, 0
        Mov esi MemFormats
        lodsd
        Mov edi eax
        While edi > 0
            lodsd
            If D$UnicodeStrings = 0
                SendMessage ebx, &CB_ADDSTRING, 0, D$eax
            Else
                Call 'User32.SendMessageW' ebx, &CB_ADDSTRING, 0, D$eax
            EndIf
            dec edi
        EndWhile
        SendMessage ebx, &CB_SETCURSEL, 0, 0
        move D$MemFormatConversionProc D$MemConvert

        Call 'USER32.GetDlgItem' D@hwnd, MEMINSPECTOR_PREV_BUTTON
        Call 'USER32.EnableWindow' eax, &FALSE
        Call 'USER32.GetDlgItem' D@hwnd, MEMINSPECTOR_NEXT_BUTTON
        Call 'USER32.EnableWindow' eax, &FALSE

        Call 'USER32.GetDlgItem' D@hwnd, MEMINSPECTOR_PAGE_EDIT
        SendMessage eax, &EM_SETLIMITTEXT, 8, 0

        Mov eax &TRUE

    ...ElseIf D@msg = WM_REFRESH_CONTENT
        If D$CurrentPageAddress <> 0
            Call 'KERNEL32.ReadProcessMemory' D$PI.hProcess,
                D$CurrentPageAddress, D$CurrentPage, PageSize, &NULL
            Call 'USER32.GetDlgItem' D@hwnd, MEMINSPECTOR_DATA_LIST
            Call 'USER32.InvalidateRect' eax, &NULL, &TRUE
        EndIf

        Mov eax 0

    ...ElseIf D@msg = WM_SET_PAGE
        Mov eax D@wParam
        If eax <> D$CurrentPageAddress
            Mov D$CurrentPageAddress eax
            Call 'KERNEL32.ReadProcessMemory' D$PI.hProcess, D@wParam, D$CurrentPage, 01000, &NULL
        EndIf
      ; Write Base address of page into an edit
        Mov eax D@wParam
        Call IntToHexString
        Call 'User32.SetDlgItemTextA' D@hwnd, MEMINSPECTOR_PAGE_EDIT, HexString
      ; Switch to memory inspector tab
        Call SelectTab D$H.DebugDialog, DEBUGDLG_DATA_TAB, 1
      ; Scroll list to address (lParam) and redraw list entries
        Call 'USER32.GetDlgItem' D@hwnd, MEMINSPECTOR_DATA_LIST
        Mov ebx eax
        Mov edi D@lParam
        and edi 0FFF
        shr edi 3
        SendMessage ebx, &LB_SETTOPINDEX, edi, 0
        SendMessage ebx, &LB_SETCURSEL, edi, 0
        Call 'USER32.SetFocus' ebx
      ; Activate / Deactivate Prev & Next Buttons
        Call 'USER32.GetDlgItem' D@hwnd, MEMINSPECTOR_PREV_BUTTON
        Mov ebx eax
        Call FindPrevPage D$CurrentPageAddress
        On eax > 0, Mov eax &TRUE
        Call 'USER32.EnableWindow' ebx, eax
        Call 'USER32.GetDlgItem' D@hwnd, MEMINSPECTOR_NEXT_BUTTON
        Mov ebx eax
        Call FindNextPage D$CurrentPageAddress
        On eax > 0, Mov eax &TRUE
        Call 'USER32.EnableWindow' ebx, eax

        Mov eax 0

    ...ElseIf D@msg = &WM_DESTROY
        VirtualFree D$CurrentPage
        Mov eax 0
;;
    ...Else_if D@msg = &WM_KEYDOWN
        Call 'USER32.GetFocus'
        Call 'USER32.GetDlgCtrlID' eax
        ;.If eax = MEMINSPECTOR_PAGE_EDIT
            If W@wParam = &VK_RETURN
                nop
            EndIf
        ;.EndIf
;;
    ...Else_if D@msg = &WM_COMMAND
        ..If W@wParam = MEMINSPECTOR_FORMAT_COMBO
            .If W@wParam+2 = &CBN_SELCHANGE
                SendMessage D@lParam, &CB_GETCURSEL, 0, 0
                If eax <> &CB_ERR
                    move D$MemFormatConversionProc D$MemConvert+eax*4
                    Call 'USER32.GetDlgItem' D@hwnd, MEMINSPECTOR_DATA_LIST
                    Call 'USER32.InvalidateRect' eax, &NULL, &TRUE
                EndIf
            .End_If
        ..Else_if W@wParam = MEMINSPECTOR_PREV_BUTTON
            Call FindPrevPage D$CurrentPageAddress
            If eax > 0
                SendMessage D@hwnd, WM_SET_PAGE, eax, eax
            EndIf
        ..Else_if W@wParam = MEMINSPECTOR_NEXT_BUTTON
            Call FindNextPage D$CurrentPageAddress
            If eax > 0
                SendMessage D@hwnd, WM_SET_PAGE, eax, eax
            EndIf
        ..Else_if W@wParam = MEMINSPECTOR_TABLE_BUTTON
            Call 'User32.DialogBoxParamA' D$H.Instance, 1013, D$H.DebugDialog,
                                          PageTableProc, D$CurrentPageAddress
            If eax >= 01000
                SendMessage D@hwnd, WM_SET_PAGE, eax, eax
            EndIf
        ..Else_if W@wParam = MEMINSPECTOR_PAGE_EDIT
            movzx eax W@wParam+2
        ..Else_if W@wParam = &IDOK
            Call MemInspector_OnGoToAddress D@hwnd
        ..End_If
        Mov eax 0

    ...ElseIf D@msg = &WM_SIZE
        movzx eax W$MemInspector.Width
        movzx esi W@lParam
        sub esi eax
        movzx eax W$MemInspector.Height
        movzx edi W@lParam+2
        sub edi eax
        Call AdjustControlSize D@hwnd, MEMINSPECTOR_DATA_LIST, esi, edi
        Call AdjustControlSize D@hwnd, MEMINSPECTOR_FORMAT_COMBO, esi, 0
        Call AdjustControlPos D@hwnd, MEMINSPECTOR_PREV_BUTTON, 0, edi
        Call AdjustControlPos D@hwnd, MEMINSPECTOR_NEXT_BUTTON, esi, edi
        Call AdjustControlPos D@hwnd, MEMINSPECTOR_TABLE_BUTTON, 0, edi
        Call AdjustControlSize D@hwnd, MEMINSPECTOR_TABLE_BUTTON, esi, 0
        Call 'USER32.InvalidateRect' D@hwnd, &NULL, &TRUE
        move D$MemInspectorSize D@lParam
        Mov eax 0

    ...Else_if D@msg = &WM_SETFONT
        Call 'USER32.GetDlgItem' D@hwnd, MEMINSPECTOR_DATA_LIST
        SendMessage eax, &WM_SETFONT, D@wParam, D@lParam
        Call 'USER32.GetDlgItem' D@hwnd, MEMINSPECTOR_PAGE_EDIT
        SendMessage eax, &WM_SETFONT, D@wParam, D@lParam

    ...Else_if D@msg = &WM_DRAWITEM
        Call MemoryInspector_OnDrawItem D@lParam
        Mov eax &TRUE

    ...Else_if D@msg = &WM_MEASUREITEM
        Mov eax D@lParam
        Mov D$eax+16 15
        Mov eax &TRUE

    ...Else
        Mov eax &FALSE
    ...EndIf

EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; PAGE TABLE
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc PageTable_WritePageDescription:
    Arguments @Address, @Protect
    Uses esi, edi

        ; Write address
        Mov eax D@Address
        Mov edi PageDesc, ecx 4, edx eax
        test eax 0FFFF | jz L0>
        Mov D$edi ' >> '
        add edi 4
L0:     add edi 8

L0:     Mov al dl
        Mov ah al | and ah 0F | shr al 4

        add ah '0'
        On ah > '9', add ah 7
        dec edi | Mov B$edi ah
        add al '0'
        On al > '9', add al 7
        dec edi | Mov B$edi al

        shr edx 8
        loop L0<

        ; Write protection "XRWC GN"
        Mov edi Pagedesc+9, ecx D@Protect
        On B$Pagedesc = ' ', add edi 4
        Mov B$edi-1 9
        Mov D$edi '----', D$edi+4 ' -- '
        If ecx = &PAGE_READONLY
            Mov B$edi+1 'R'
        Else_if ecx = &PAGE_READWRITE
            Mov W$edi+1 'RW'
        Else_if ecx = &PAGE_EXECUTE
            Mov B$edi 'X'
        Else_if ecx = &PAGE_EXECUTE_READ
            Mov W$edi 'XR'
        Else_if ecx = &PAGE_EXECUTE_READWRITE
            Mov D$edi 'XRW-'
        Else_if ecx = &PAGE_EXECUTE_WRITECOPY
            Mov D$edi 'XRWC'
        Else_if ecx = &PAGE_WRITECOPY
            Mov D$edi '-RWC'
        EndIf
        test ecx &PAGE_GUARD | jz L0>
        Mov B$edi+5 'G'
L0:     test ecx &PAGE_NOCACHE | jz L0>
        Mov B$edi+6 'N'
L0:
        ; Write module filename (dll/exe), if available
        Mov edi Pagedesc+17
        On B$Pagedesc = ' ', add edi 4
        Mov eax 0
        Call GetModuleName D@Address
        If eax <> 0
            Mov esi eax
L0:         movsb | cmp B$esi 0 | jne L0<
        EndIf
        Mov D$edi 0
EndP

Proc PageTable_AddItem:
    Arguments @Base, @Size, @Protect, @Type
    Uses esi, edi

        Mov esi D@Base, edi esi
        add edi D@Size

        While esi < edi
            Call PageTable_WritePageDescription esi, D@Protect
            SendMessage ebx, &LB_ADDSTRING, 0, PageDesc
            SendMessage ebx, &LB_SETITEMDATA, eax, esi
            add esi 01000
        EndWhile
EndP
____________________________________________________________________________________________

Proc PageTable_Build:
    Uses esi

        Mov esi D$AddressLowerBound
        While esi < D$AddressUpperBound
            Call VirtualQuery esi
            If eax = 1
                Call PageTable_AddItem esi, ecx,
                    D$MemoryInformation@Protect, D$MemoryInformation@Type
            ElseIf eax = 0-1
                ExitP
            EndIf
            add esi D$MemoryInformation@RegionSize
        EndWhile
EndP
____________________________________________________________________________________________

; Tag Dialog 1013

Proc PageTableProc:
    Arguments @hwnd, @msg, @wParam, @lParam
    Uses ebx, esi, edi

    ..If D@msg = &WM_INITDIALOG
        Call 'User32.GetDlgItem' D@hwnd, 10
        Mov ebx eax
        SendMessage ebx, &LB_RESETCONTENT, 0, 0
        SendMessage ebx, &WM_SETFONT, D$DebugFontHandle, &TRUE
        Call PageTable_Build
        Mov eax &TRUE

    ..Else_if D@msg = &WM_COMMAND
        .If D@wParam = 1
            Call 'User32.GetDlgItem' D@hwnd, 10
            Mov ebx eax
            SendMessage ebx, &LB_GETCURSEL, 0, 0
            SendMessage ebx, &LB_GETITEMDATA, eax, 0
            Call 'USER32.EndDialog' D@hwnd, eax
        .Else_if W@wParam = 10
            If W@wParam+2 = &LBN_DBLCLK
                SendMessage D@lParam, &LB_GETCURSEL, 0, 0
                SendMessage D@lParam, &LB_GETITEMDATA, eax, 0
                Call 'USER32.EndDialog' D@hwnd, eax
            EndIf
        .EndIf
        Mov eax 0

    ..Else_if D@msg = &WM_CLOSE
        Call WM_CLOSE
        Mov eax 0
    ..Else
        Mov eax &FALSE
    ..EndIf
EndP
____________________________________________________________________________________________

; Tag Dialog 1014

ShowExceptionInfo:
    Call 'User32.CreateDialogParamA' D$H.Instance, 1014, D$H.MainWindow, ExceptionInfoProc, D$E.ExceptionCode
ret

[EXCEPTINFO_DESC 10 EXCEPTINFO_ADDRESS 15 EXCEPTINFO_INSTRUCTION 20 EXCEPTINFO_INFO 25]

[ExceptionMap:
 &EXCEPTION_ACCESS_VIOLATION ACCESS_VIOLATION
 &EXCEPTION_ARRAY_BOUNDS_EXCEEDED ARRAY_BOUNDS_EXCEEDED
 &EXCEPTION_DATATYPE_MISALIGNMENT DATATYPE_MISALIGNMENT
 &EXCEPTION_FLT_DENORMAL_OPERAND FLT_DENORMAL_OPERAND
 &EXCEPTION_FLT_DIVIDE_BY_ZERO FLT_DIVIDE_BY_ZERO
 &EXCEPTION_FLT_INEXACT_RESULT FLT_INEXACT_RESULT
 &EXCEPTION_FLT_INVALID_OPERATION FLT_INVALID_OPERATION
 &EXCEPTION_FLT_OVERFLOW FLT_OVERFLOW
 &EXCEPTION_FLT_STACK_CHECK FLT_STACK_CHECK
 &EXCEPTION_FLT_UNDERFLOW FLT_UNDERFLOW
 &EXCEPTION_ILLEGAL_INSTRUCTION ILLEGAL_INSTRUCTION
 &EXCEPTION_IN_PAGE_ERROR IN_PAGE_ERROR
 &EXCEPTION_INT_DIVIDE_BY_ZERO INT_DIVIDE_BY_ZERO
 &EXCEPTION_INT_OVERFLOW INT_OVERFLOW
 &EXCEPTION_PRIV_INSTRUCTION PRIV_INSTRUCTION
 &EXCEPTION_STACK_OVERFLOW STACK_OVERFLOW
 &EXCEPTION_INVALID_DISPOSITION INVALID_DISPOSITION
 &EXCEPTION_NONCONTINUABLE_EXCEPTION NONCONTINUABLE_EXCEPTION
 0 UNKNOWN_EXCEPTION]

[ExceptionCaption: B$ ? #128]

Proc ExceptionInfoProc:
    Arguments @hwnd, @msg, @wParam, @lParam
    Uses ebx, esi, edi

    ..If D@msg = &WM_INITDIALOG

      ; Output module in caption
        If D$CurrentModule <> 0
            Mov eax D$CurrentModule
        Else
            Mov eax {'Exception in Non-Code Section' 0}
        EndIf
        Call 'User32.SetWindowTextA' D@hwnd, eax

      ; Output exception description
        Mov eax D$E.ExceptionCode
        Mov esi ExceptionMap
        While D$esi <> 0
            cmp eax D$esi | je L0>
            add esi 8
        EndWhile
        Mov edx eax, ecx 4, edi D$esi+4
        Call IntToHex
L0:     Call 'USER32.GetDlgItem' D@hwnd, EXCEPTINFO_DESC | Mov ebx eax
        SendMessage ebx, &WM_SETFONT, D$DebugFontHandle, &TRUE
        Call 'User32.SetWindowTextA' ebx, D$esi+4

      ; Output troubling instruction and its address
        Mov eax D$E.ExceptionAddress
        Call IntToHexString
        Call 'USER32.GetDlgItem' D@hwnd, EXCEPTINFO_ADDRESS | Mov ebx eax
        SendMessage ebx, &WM_SETFONT, D$Font1Handle, &TRUE
        Call 'User32.SetWindowTextA' ebx, HexString

        Call NextInstructionDecode
        Call 'USER32.GetDlgItem' D@hwnd, EXCEPTINFO_INSTRUCTION | Mov ebx eax
        SendMessage ebx, &WM_SETFONT, D$Font1Handle, &TRUE
        Call 'User32.SetWindowTextA' ebx, D$NextInstructionPtr

      ; Output further information for protection faults
        .If D$E.ExceptionCode = &EXCEPTION_ACCESS_VIOLATION
            Mov eax D$E.ExceptionInfo ; read/write
            If eax = 0
                Mov D$AV_ReadWrite 'read', D$AV_ReadWrite+4 ' fro', B$AV_ReadWrite+8 'm'
            Else
                Mov D$AV_ReadWrite 'writ', D$AV_ReadWrite+4 'e at', B$AV_ReadWrite+8 ' '
            EndIf
            Mov edx D$E.ExceptionInfo+4 ; inaccessible address
            Mov ecx 4, edi AV_Address
            Call IntToHex
            Call 'User32.SetDlgItemTextA' D@hwnd, EXCEPTINFO_INFO, Exception_AV
        .EndIf

      ; If the exception can be continued hand to the SEH
        Call 'USER32.GetDlgItem' D@hwnd, 2
        Mov edx D$ExceptionFlags | and edx E_MUSTEXIT
        If edx <> 0
            Mov edx 0
        Else
            Mov edx 1
        EndIf
        Call 'USER32.EnableWindow' eax, edx

        Call 'USER32.GetDlgItem' D@hwnd, 1
        Call 'USER32.SetFocus' eax
        Mov eax 0

    ..Else_if D@msg = &WM_CTLCOLORSTATIC
        Call 'USER32.GetDlgItem' D@hwnd, EXCEPTINFO_DESC
        If eax = D@lParam
            Call 'GDI32.SetBkColor' D@wParam, D$RVBA.DialogsBackgnd
            Mov eax D$H.DialogsBackGroundBrush
            ExitP
        EndIf
        Call 'USER32.GetDlgItem' D@hwnd, EXCEPTINFO_INSTRUCTION
        If eax = D@lParam
            Call 'GDI32.SetTextColor' D@wParam, 099
            Call 'GDI32.SetBkColor' D@wParam, D$RVBA.DialogsBackgnd
            Mov eax D$H.DialogsBackGroundBrush
            ExitP
        EndIf
        Call 'USER32.GetDlgItem' D@hwnd, EXCEPTINFO_ADDRESS
        If eax = D@lParam
            Call 'GDI32.SetBkColor' D@wParam, D$RVBA.DialogsBackgnd
            Mov eax D$H.DialogsBackGroundBrush
            ExitP
        EndIf
        Mov eax 0

    ..Else_if D@msg = &WM_COMMAND
        .If D@wParam = 1
            Call WM_CLOSE
            Mov B$TerminateDebuggee &TRUE
            Call DebugDialog_ContinueDebuggee
            ;SendMessage D$H.DebugDialog, &WM_CLOSE, 0, 0
        .ElseIf D@wParam = 2
            Call WM_CLOSE
            Call DebugDialog_ContinueDebuggee
        .EndIf
        Mov eax 0

    ..Else_if D@msg = &WM_CLOSE
        Call WM_CLOSE
        Mov eax 0
    ..Else
        Mov eax &FALSE
    ..EndIf
EndP


____________________________________________________________________________________________
____________________________________________________________________________________________

; DATA LABEL VIEWER
____________________________________________________________________________________________
____________________________________________________________________________________________

[PADDING 4]

Proc DataView_OnDrawItem:
    Arguments @DrawItemStruc
    Local @Brush @Address @Selected

    Mov ebx D@DrawItemStruc
    Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 0

    Mov D@Selected 0
    test D$ebx+DRAWITEM_ITEMSTATE &ODS_SELECTED
    setz al
    xor al 1
    Mov B@Selected al

  ; Print Offset
    Mov edx D$DebugFontCharWidth | shl edx 3 | add edx PADDING ; 8 chars
    Mov eax D$ebx+DRAWITEM_RCITEM_LEFT | Mov D$ItemRect@x1 eax
    add eax edx | Mov D$ItemRect@x2 eax
    move D$ItemRect@y1 D$ebx+DRAWITEM_RCITEM_TOP
    move D$ItemRect@y2 D$ebx+DRAWITEM_RCITEM_BOTTOM

    Mov edi D$ebx+DRAWITEM_ITEMID | and edi 1
    On B@Selected = 1, Mov edi 2
    Call 'GDI32.CreateSolidBrush' D$BackgroundCol@Name+edi*4 | Mov D@Brush eax
    Call 'USER32.FillRect' D$ebx+DRAWITEM_HDC ItemRect D@Brush
    Call 'GDI32.DeleteObject' D@Brush
    Call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC D$BackgroundCol@Name+edi*4

    Call IsWatchPoint D$ebx+DRAWITEM_ITEMDATA
    If eax = 1
        Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 0CC
    ElseIf eax = 3
        Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 066CC
    EndIf

    Mov eax D$ebx+DRAWITEM_ITEMDATA
    Call IntToHexString
    Call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, HexString, 8, ItemRect,
         (&DT_SINGLELINE+&DT_CENTER+&DT_VCENTER)

  ; Print value
    move D$ItemRect@x1 D$ItemRect@x2
    move D$ItemRect@x2 D$ebx+DRAWITEM_RCITEM_RIGHT

    Mov edi D$ebx+DRAWITEM_ITEMID | and edi 1
    On B@Selected = 1, Mov edi 2
    Call 'GDI32.CreateSolidBrush' D$BackgroundCol@Value+edi*4 | Mov D@Brush eax
    Call 'USER32.FillRect' D$ebx+DRAWITEM_HDC ItemRect D@Brush
    Call 'GDI32.DeleteObject' D@Brush
    Call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC, D$BackgroundCol@Value+edi*4

    If B@Selected = 1
        Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 0FF_FFFF
    EndIf

    SendMessage D$ebx+DRAWITEM_HWNDITEM, &CB_GETLBTEXT, D$ebx+DRAWITEM_ITEMID, ItemString

    add D$ItemRect@x1 5
    Call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, ItemString, 0-1, ItemRect,
         (&DT_SINGLELINE+&DT_LEFT+&DT_VCENTER)

    ; Draw focus rect if selected
    If B@Selected = 1
        lea eax D$ebx+DRAWITEM_RCITEM_LEFT
        Call 'USER32.DrawFocusRect' D$ebx+DRAWITEM_HDC, eax
    EndIf
EndP
____________________________________________________________________________________________

Proc DataView_OnDrawValueItem:
    Arguments @DrawItemStruc
    Local @Brush @Address @Selected

    Mov ebx D@DrawItemStruc
    On D$ebx+DRAWITEM_ITEMID = 0-1, ExitP

    Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 0

    Mov D@Selected 0
    test D$ebx+DRAWITEM_ITEMSTATE &ODS_SELECTED
    setz al
    xor al 1
    Mov B@Selected al

  ; Print size
    Mov edx D$DebugFontCharWidth | shl edx 3 | add edx PADDING ; 8 chars
    Mov eax D$ebx+DRAWITEM_RCITEM_LEFT | Mov D$ItemRect@x1 eax
    add eax edx | Mov D$ItemRect@x2 eax
    move D$ItemRect@y1 D$ebx+DRAWITEM_RCITEM_TOP
    move D$ItemRect@y2 D$ebx+DRAWITEM_RCITEM_BOTTOM

    Mov edi D$ebx+DRAWITEM_ITEMID | and edi 1
    ;On B@Selected = 1, Mov edi 2
    Call 'GDI32.CreateSolidBrush' D$BackgroundCol@Name+edi*4 | Mov D@Brush eax
    Call 'USER32.FillRect' D$ebx+DRAWITEM_HDC ItemRect D@Brush
    Call 'GDI32.DeleteObject' D@Brush
    Call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC D$BackgroundCol@Name+edi*4

    Mov eax D$ebx+DRAWITEM_ITEMDATA
    If al = 'D'
        Mov eax {'32bit' 0}
    ElseIf al = 'W'
        Mov eax {'16bit' 0}
    ElseIf al = 'B'
        Mov eax {'8bit' 0}
    ElseIf al = 'S'
        Mov eax {'ASCII' 0}
    ElseIf al = 'F'
        Mov eax {'32bit FP' 0}
    ElseIf al = 'R'
        Mov eax {'64bit FP' 0}
    ElseIf al = 'T'
        Mov eax {'80bit FP' 0}
    EndIf
    Call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, eax, -1, ItemRect, (&DT_SINGLELINE+&DT_CENTER+&DT_VCENTER)

  ; Print value
    move D$ItemRect@x1 D$ItemRect@x2
    move D$ItemRect@x2 D$ebx+DRAWITEM_RCITEM_RIGHT

    Mov edi D$ebx+DRAWITEM_ITEMID | and edi 1
    ;On B@Selected = 1, Mov edi 2
    Call 'GDI32.CreateSolidBrush' D$BackgroundCol@Value+edi*4 | Mov D@Brush eax
    Call 'USER32.FillRect' D$ebx+DRAWITEM_HDC ItemRect D@Brush
    Call 'GDI32.DeleteObject' D@Brush
    Call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC D$BackgroundCol@Value+edi*4

    Mov B$HexSeparator 3

    Mov eax D$ebx+DRAWITEM_ITEMDATA, esi DataBuffer
    If al = 'D'
        Mov ecx 4 | Call toHex
        sub edi 2 | Mov W$edi '0x'
    ElseIf al = 'W'
        Mov ecx 2 | Call toHex
        sub edi 2 | Mov W$edi '0x'
    ElseIf al = 'B'
        Mov ecx 1 | Call toHex
        sub edi 2 | Mov W$edi '0x'
    ElseIf al = 'S'
        Mov edi DataBuffer
    ElseIf al = 'F'
        Mov ecx 4 | Call toFloat
    ElseIf al = 'R'
        Mov ecx 8 | Call toDouble
    ElseIf al = 'T'
        Mov ecx 10 | Call toExtended
    EndIf

    add D$ItemRect@x1 5
    Call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, edi, 0-1, ItemRect,
         (&DT_SINGLELINE+&DT_LEFT+&DT_VCENTER)

    Mov eax D$ItemRect@x2 | sub eax D$ItemRect@x1
    Mov edx 0, ecx 3 | div ecx
    push eax
        add D$ItemRect@x1 eax

        Mov eax D$ebx+DRAWITEM_ITEMDATA, esi DataBuffer
        If al = 'D'
            Mov ecx 4 | Call toUDWord
        ElseIf al = 'W'
            Mov ecx 2 | Call toUWord
        ElseIf al = 'B'
            Mov ecx 1 | Call toUByte
        Else
            pop eax | jmp L0>
        EndIf
        Call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, edi, 0-1, ItemRect, (&DT_SINGLELINE+&DT_LEFT+&DT_VCENTER)
    pop eax
    add D$ItemRect@x1 eax

    Mov eax D$ebx+DRAWITEM_ITEMDATA, esi DataBuffer
    If al = 'D'
        Mov ecx 4 | Call toSDWord
    ElseIf al = 'W'
        Mov ecx 2 | Call toSWord
    ElseIf al = 'B'
        Mov ecx 1 | Call toSByte
    Else
        pop eax | jmp L0>
    EndIf
    Call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, edi, 0-1, ItemRect, (&DT_SINGLELINE+&DT_LEFT+&DT_VCENTER)

    ; Draw focus rect if selected
L0: If B@Selected = 1
        lea eax D$ebx+DRAWITEM_RCITEM_LEFT
        Call 'USER32.DrawFocusRect' D$ebx+DRAWITEM_HDC, eax
    EndIf
EndP
____________________________________________________________________________________________

; Insert data label sorted by address. Brute force search.

Proc DataView_InsertDataLabel:
    Arguments @ComboHandle, @Name, @Address
    Uses ebx

    Mov ebx 0

L0: SendMessage D@ComboHandle, &CB_GETITEMDATA, ebx, 0
    cmp eax &CB_ERR | je L1> ; EOL or highest address > append

    If eax > D@Address
L1:     SendMessage D@ComboHandle, &CB_INSERTSTRING, ebx, D@Name
        ExitP
    Endif

    inc ebx | jmp L0<
EndP
____________________________________________________________________________________________

; Filling the ComboBox with the Data Label Names (as string) / Addresses (as data)

[DataLabelComboHandle: ?]

Proc DataView_FillDataLabelCombo:
    Arguments @ComboHandle @SortByName

    SendMessage D@ComboHandle, &CB_RESETCONTENT, 0, 0
    move D$DataLabelComboHandle D@ComboHandle

    Mov esi D$PlainLabelList
    lodsd | Mov edx D$PlainLabelList | add edx eax

L0: inc esi ; '|'

L0: Mov ebx esi
    While B$esi <> 2
        inc esi
    End_While
    test B$esi+5 DataLabelFlag | jz L2>

        Mov edi ItemString, esi ebx
        While B$esi <> 2
            movsb
        End_While
        Mov B$edi 0
        push edx
            Mov ebx D$esi+1 | add ebx D$DataAjust
            If D@SortByName = 1
                SendMessage D@ComboHandle, &CB_ADDSTRING, 0, ItemString
            Else
                Call DataView_InsertDataLabel D@ComboHandle, ItemString, ebx
            EndIf
            SendMessage D@ComboHandle, &CB_SETITEMDATA, eax, ebx
        pop edx
        add esi 7
        cmp esi edx | jb L0<
    ExitP

L2: add esi 7 | cmp esi edx | jb L0<
EndP
____________________________________________________________________________________________

Proc DataView_LoadSettings:
    Arguments @FileHandle @Size

  ; Copy label to stack
    Mov eax D@Size | add eax 4 | and eax 0_FFFF_FFFC ; reserve for \0 and dw-align stack
    sub esp eax | Mov edi esp
    Call 'KERNEL32.ReadFile' D@FileHandle, edi, D@Size, BytesTransfered, 0
    Mov ecx D@Size | Mov B$edi+ecx 0 ; terminate string

  ; Search and select
    SendMessage D$DataLabelComboHandle, &CB_FINDSTRING, 0-1, edi
    SendMessage D$DataLabelComboHandle, &CB_SETCURSEL, eax, 0

    Mov eax 1
EndP
____________________________________________________________________________________________

Proc DataView_SaveSettings:
    Arguments @FileHandle
    Structure @Settings 8, @Id 0, @Size 4

    Mov D@Id 'LaLa' ; LAst LAbel

  ; Get currently selected label
    SendMessage D$DataLabelComboHandle, &CB_GETCURSEL, 0, 0
    On eax = 0-1, ExitP
    Mov ebx eax
    SendMessage D$DataLabelComboHandle, &CB_GETLBTEXTLEN, ebx, 0
    Mov D@Size eax

  ; Copy label to stack
    add eax 4 | and eax 0_FFFF_FFFC ; reserve for \0 and dw-align stack
    sub esp eax | Mov edi esp
    SendMessage D$DataLabelComboHandle, &CB_GETLBTEXT, ebx, edi

    Call 'KERNEL32.WriteFile' D@FileHandle, D@Settings, 8, BytesTransfered, 0
    Call 'KERNEL32.WriteFile' D@FileHandle, edi, D@Size, BytesTransfered, 0
EndP
____________________________________________________________________________________________

[DataBuffer: B$ ? #64]
[DataValue: B$ ? #32]
[DataPointer: ?]

Proc DataView_OnSelectDataLabel:
    Arguments @DlgHandle @ComboHandle
    Local @LabelLen @ListHandle

        SendMessage D@ComboHandle, &CB_GETCURSEL, 0, 0
        On eax = &CB_ERR, ExitP
        SendMessage D@ComboHandle, &CB_GETITEMDATA, eax, 0
        Mov D$DataPointer eax

        Call 'USER32.GetDlgItem' D@DlgHandle, DATAVIEW_VALUE_LIST
        Mov D@ListHandle eax

        SendMessage D@ListHandle, &LB_RESETCONTENT, 0, 0

        ; Read a block of memory with a max size of 64 byte
        Call IsProcessMemory D$DataPointer
        On eax = 0, ExitP
        Mov ebx 64
        If eax < ebx
            Mov ebx eax
        EndIf
        Call 'KERNEL32.ReadProcessMemory' D$PI.hProcess, D$DataPointer, DataBuffer, ebx, &NULL

        ; DWord Size
        On ebx >= 4, SendMessage D@ListHandle, &LB_ADDSTRING, 0, 'D'
        ; Word Size
        On ebx >= 2, SendMessage D@ListHandle, &LB_ADDSTRING, 0, 'W'
        ; Byte size
        SendMessage D@ListHandle, &LB_ADDSTRING, 0, 'B'
        ; String
        Call IsString DataBuffer 64
        If ah = 1
            Mov B$DataBuffer+63 0
            SendMessage D@ListHandle, &LB_ADDSTRING, 0, 'S'
        EndIf
        ; Float
        On ebx >= 4, SendMessage D@ListHandle, &LB_ADDSTRING, 0, 'F'
        ; Double
        On ebx >= 8, SendMessage D@ListHandle, &LB_ADDSTRING, 0, 'R'
        ; Extended
        On ebx >= 10, SendMessage D@ListHandle, &LB_ADDSTRING, 0, 'T'

        ; Activate 'More' button
        Call 'USER32.GetDlgItem' D@DlgHandle, M03_SHOW_MEM
        Call 'USER32.EnableWindow' eax, &TRUE
        ; Activate/Deactivate 'More From Pointer' button
        .If ebx >= 4
            Call IsProcessMemory D$DataBuffer
            If eax > 0
                Mov ebx &TRUE
            Else
                Mov ebx &FALSE
            EndIf
            Call 'User32.GetDlgItem' D@DlgHandle, M03_SHOW_PMEM
            Call 'USER32.EnableWindow' eax, ebx
            If ebx = &TRUE
                Mov eax &MF_ENABLED
            Else
                Mov eax &MF_GRAYED
            EndIf
            Call 'USER32.EnableMenuItem' D$DataView.PopupMenu, M03_SHOW_PMEM, eax
        .EndIf
EndP
____________________________________________________________________________________________

Proc DataView_SelectSymbol:
    Arguments @DlgHandle, @Address
    Local @ComboHandle

        Call SelectTab D$H.DebugDialog, DEBUGDLG_DATA_TAB, 0
        Call 'USER32.GetDlgItem' D@DlgHandle, DATAVIEW_LABEL_COMBO
        Mov D@ComboHandle eax
        SendMessage D@ComboHandle, &CB_GETCOUNT, 0, 0
        Mov edi eax, esi 0, ebx D@Address
        While esi < edi
            SendMessage D@ComboHandle, &CB_GETITEMDATA, esi, 0
            If eax = ebx
                SendMessage D@ComboHandle, &CB_SETCURSEL, esi, 0
                ExitP
            EndIf
            inc esi
        EndWhile
EndP

____________________________________________________________________________________________

Proc DataView_ShowDeclaration:
    Arguments @DlgHandle
    Local @ComboHandle, @LabelLen

        Call 'USER32.GetDlgItem' D@DlgHandle, DATAVIEW_LABEL_COMBO
        Mov D@ComboHandle eax

        SendMessage D@ComboHandle, &CB_GETCURSEL, 0, 0
        On eax = &CB_ERR, ExitP
        SendMessage D@ComboHandle, &CB_GETLBTEXT, eax, ItemString
        Mov D@LabelLen eax

        Call RestoreRealSource

        Mov edx ItemString, ebx D@LabelLen
        Call InternSearch
        If B$BlockInside = &TRUE
            Mov B$BlockInside &FALSE
            Mov esi D$CurrentWritingPos | dec esi | Call InternalRightClick
        End_If

        Call SetPartialEditionFromPos
EndP
____________________________________________________________________________________________

[DataView.PopupMenu: ?]

[M03_SHOW_MEM       3100
 M03_SHOW_PMEM      3101
 M03_SHOW_DECL      3102
 M03_WATCH_W        3103
 M03_WATCH_RW       3104
 M03_SORTBYNAME     3105
 M03_SORTBYADDRESS  3106]

Proc DataView_CreatePopupMenu:
    Arguments @DlgHandle

    Call 'USER32.CreatePopupMenu' | Mov ebx eax, D$DataView.PopupMenu eax
    Call AppendMenu ebx, &MF_OWNERDRAW, M03_SHOW_MEM, 0
    Call AppendMenu ebx, &MF_OWNERDRAW, M03_SHOW_PMEM, 0
    Call AppendMenu ebx, &MF_OWNERDRAW, M03_SHOW_DECL, 0
    Call AppendMenu ebx, &MF_SEPARATOR, 0, 0
    Call AppendMenu ebx, &MF_OWNERDRAW, M03_WATCH_W, 0
    Call AppendMenu ebx, &MF_OWNERDRAW, M03_WATCH_RW, 0
    Call AppendMenu ebx, &MF_SEPARATOR, 0, 0
    Call AppendMenu ebx, &MF_OWNERDRAW, M03_SORTBYNAME, 0
    Call AppendMenu ebx, &MF_OWNERDRAW, M03_SORTBYADDRESS, 0

    Call 'USER32.CheckMenuRadioItem' D$DataView.PopupMenu, M03_SORTBYNAME, M03_SORTBYADDRESS,
        M03_SORTBYNAME, &MF_BYCOMMAND
EndP
____________________________________________________________________________________________

Proc DataView_UpdatePopupMenu:
    Call 'USER32.CheckMenuItem' D$DataView.PopupMenu, M03_WATCH_RW, &MF_UNCHECKED
    Call 'USER32.CheckMenuItem' D$DataView.PopupMenu, M03_WATCH_W, &MF_UNCHECKED
    Call IsWatchPoint D$DataPointer
    If eax = 0011
        Call 'USER32.CheckMenuItem' D$DataView.PopupMenu, M03_WATCH_RW, &MF_CHECKED
    ElseIf eax = 1
        Call 'USER32.CheckMenuItem' D$DataView.PopupMenu, M03_WATCH_W, &MF_CHECKED
    EndIf
EndP
____________________________________________________________________________________________

; Process WM_DRAWITEM message for owner-drawn menu items.
;;
Proc DebugDialog_OnDrawMenuItem:
    Arguments @DrawItemStruc
    Local @Brush

    Mov ebx D@DrawItemStruc

    Mov esi DebugMenuTable
    lodsd | Mov ecx eax ; number of entries
    Mov eax D$ebx+DRAWITEM_ITEMID
    While D$esi <> eax
        add esi 16
        dec ecx | jz L9>>
    EndWhile

    Call 'GDI32.SelectObject' D$ebx+DRAWITEM_HDC, D$DialogFont_handle
    push eax

    Test_If D$ebx+DRAWITEM_ITEMSTATE &ODS_GRAYED
        Call 'USER32.GetSysColor' &COLOR_GRAYTEXT
        Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax
        jmp L1>
    Test_Else_If D$ebx+DRAWITEM_ITEMSTATE &ODS_SELECTED
        Call 'USER32.GetSysColor' &COLOR_MENUTEXT
        Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax

        Call 'USER32.GetSysColor' &COLOR_HIGHLIGHTTEXT
        Call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC, eax

        Call 'USER32.GetSysColorBrush' &COLOR_HIGHLIGHTTEXT
    Test_Else
        Call 'USER32.GetSysColor' &COLOR_MENUTEXT
        Call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax

L1:     Call 'USER32.GetSysColor' &COLOR_MENU
        Call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC, eax

        Call 'USER32.GetSysColorBrush' &COLOR_MENU
    Test_End

    lea edx D$ebx+DRAWITEM_RCITEM_LEFT
    Call 'USER32.FillRect' D$ebx+DRAWITEM_HDC, edx, eax

    Mov eax D$esi+12 ; image index
    If eax <> 0-1
        Mov ecx D$ebx+DRAWITEM_RCITEM_LEFT | add ecx 2
        Mov edx D$ebx+DRAWITEM_RCITEM_TOP  | add edx 2
        Test_If D$ebx+DRAWITEM_ITEMSTATE &ODS_GRAYED
            Mov edi &ILD_MASK
        Test_Else_If D$ebx+DRAWITEM_ITEMSTATE &ODS_SELECTED
            Mov edi &ILD_NORMAL
        Test_Else
            Mov edi &ILD_TRANSPARENT
        Test_End

        Test_If D$ebx+DRAWITEM_ITEMSTATE &ODS_CHECKED
            Mov eax 6
        Test_End

L0:     Call 'COMCTL32.ImageList_Draw' D$DebugDialog_ImageList, eax, D$ebx+DRAWITEM_HDC, ecx, edx, edi
    EndIf

    add D$ebx+DRAWITEM_RCITEM_LEFT 22

    Mov eax D$esi+4
    Call DrawMenuItemText D$eax, &DT_LEFT

    Mov eax D$esi+8
    If eax <> 0
      ; Draw shortcut rightaligned
        dec D$ebx+DRAWITEM_RCITEM_RIGHT
        Call DrawMenuItemTextA eax, &DT_RIGHT
    EndIf

    pop eax
    Call 'GDI32.SelectObject' D$ebx+DRAWITEM_HDC, eax

L9: Mov eax 1
EndP
;;
____________________________________________________________________________________________

; To allow rightclick inside the list of a combobox we override the window proc of the list.
; The listbox is a child window of the combobox from which the handle is retrieved through
; enumerating.

[ComboListClass: 'ComboLBox' 0] ; consistent among all win OS's ?
[ComboChildClass: B$ ? #32]

Proc EnumComboChilds:
    Arguments @hwnd, @Param
    Uses esi, edi

    Call 'User32.GetClassNameA' D@hwnd, ComboChildClass, 32
    Mov edi ComboListClass, esi ComboChildClass, ecx eax
    repe cmpsb | je L1>
    Mov eax 1 | ExitP ; continue search

  ; Listbox of the combo
L1: Call 'User32.SetWindowLongA' D@hwnd, &GWL_WNDPROC, DataView_InterceptRightClick
    Mov D$DataView.LBProc eax
    Mov eax 0 ; finish search
EndP
____________________________________________________________________________________________

Proc DataView_OverrideComboProc:
    Arguments @DlgHandle

    Mov D$DataView.LBProc 0
    Call 'USER32.GetDlgItem' D@DlgHandle, DATAVIEW_LABEL_COMBO | Mov ebx eax
    Call 'USER32.EnumChildWindows' ebx, EnumComboChilds, 0
    If D$DataView.LBProc = 0

        Call MessageBox {B$ "ENUM CHILD WINDOWS" EOS},
                        {B$ "Listbox not found in combo! Please report this problem and your OS version on the board." EOS},
                        &MB_SYSTEMMODAL+&MB_ICONWARNING

    EndIf

EndP

[DataView.LBProc: ?]

Proc DataView_InterceptRightClick:
    Arguments @hwnd, @msg, @wParam, @lParam
    Uses ebx esi edi

    If D@msg = &WM_RBUTTONDOWN
      ; Simulate left click (select item) before showing the popup menu
        SendMessage D@hwnd, &WM_LBUTTONDOWN, D@wParam, D@lParam
        SendMessage D@hwnd, &WM_LBUTTONUP, D@wParam, D@lParam
      ; Show the popup menu at the position of the mouse-click
        movzx eax W@lParam
        movzx ecx W@lParam+2
        Mov D$PointX eax, D$PointY ecx
        Call 'USER32.ClientToScreen' D@hwnd, Point
        Call 'USER32.TrackPopupMenu' D$DataView.PopupMenu, &TPM_LEFTALIGN, D$PointX, D$PointY, 0, D$DataViewHandle, 0
    ElseIf D@msg = &WM_LBUTTONDBLCLK
      ; Show more
        Mov eax D$DataPointer | and eax 0_FFFF_F000
        SendMessage D$MemoryInspectorHandle, WM_SET_PAGE, eax, D$DataPointer
    EndIf
    Call 'User32.CallWindowProcA' D$DataView.LBProc, D@hwnd, D@msg, D@wParam, D@lParam
EndP
____________________________________________________________________________________________

; Process WM_INITDIALOG message.

Proc DataViewDialog_OnCreate:
    Arguments @hwnd

        Call 'USER32.GetClientRect' D@hwnd, DebugRect
        move W$DataView.Width W$DebugRect@width
        move W$DataView.Height W$DebugRect@height

        Call 'USER32.GetDlgItem' D@hwnd, DATAVIEW_LABEL_COMBO
        Call DataView_FillDataLabelCombo eax 1

        Call DataView_CreatePopupMenu D@hwnd
        Call DataView_OverrideComboProc D@hwnd

        Mov eax &TRUE
EndP
____________________________________________________________________________________________

; Process WM_SIZE message.

Proc DataViewDialog_OnSize:
    Arguments @hwnd, @WidthHeight

        movzx eax W$DataView.Width
        movzx esi W@WidthHeight
        sub esi eax
        movzx eax W$DataView.Height
        movzx edi W@WidthHeight+2
        sub edi eax
        Call AdjustControlSize D@hwnd, DATAVIEW_LABEL_COMBO, esi, edi
        Call AdjustControlSize D@hwnd, DATAVIEW_VALUE_LIST, esi, 0
        Call AdjustControlPos D@hwnd, DATAVIEW_VALUE_LIST, 0, edi
        Call 'USER32.InvalidateRect' D@hwnd, &NULL, &TRUE

        move D$DataViewSize D@WidthHeight
        Mov eax 0
EndP
____________________________________________________________________________________________

; Process WM_COMMAND message.

Proc DataViewDialog_OnCommand:
    Arguments @hwnd, @wParam, @lParam

        movzx ecx W@wParam
        movzx eax W@wParam+2
        .If ecx = DATAVIEW_LABEL_COMBO
            If eax = &CBN_SELCHANGE
                Call DataView_OnSelectDataLabel D@hwnd, D@lParam
                Call DataView_UpdatePopupMenu
            EndIf
        .ElseIf ecx = M03_SHOW_MEM
            Mov eax D$DataPointer
            and eax 0_FFFF_F000
            SendMessage D$MemoryInspectorHandle, WM_SET_PAGE, eax, D$DataPointer
        .ElseIf ecx = M03_SHOW_PMEM
            Mov eax D$DataBuffer
            and eax 0_FFFF_F000
            SendMessage D$MemoryInspectorHandle, WM_SET_PAGE, eax, D$DataBuffer
        .ElseIf ecx = M03_SHOW_DECL
            Call DataView_ShowDeclaration D@hwnd
        .ElseIf ecx = M03_WATCH_W
            Call 'USER32.GetMenuState' D$DataView.PopupMenu, M03_WATCH_W, 0
            and eax &MF_CHECKED
            If eax = 0
                Call SetWatchPoint D$DataPointer, 4, 1
                Call 'USER32.CheckMenuItem' D$DataView.PopupMenu, M03_WATCH_W, &MF_CHECKED
            Else
                Call DeleteWatchPoint
                Call 'USER32.CheckMenuItem' D$DataView.PopupMenu, M03_WATCH_W, &MF_CHECKED
            EndIf
            Call 'USER32.GetDlgItem' D@hwnd, DATAVIEW_LABEL_COMBO
            Call 'USER32.InvalidateRect' eax, 0, &FALSE
        .ElseIf ecx = M03_WATCH_RW
            Call 'USER32.GetMenuState' D$DataView.PopupMenu, M03_WATCH_RW, 0
            and eax &MF_CHECKED
            If eax = 0
                Call SetWatchPoint D$DataPointer, 4, 3
                Call 'USER32.CheckMenuItem' D$DataView.PopupMenu, M03_WATCH_RW, &MF_CHECKED
            Else
                Call DeleteWatchPoint
                Call 'USER32.CheckMenuItem' D$DataView.PopupMenu, M03_WATCH_RW, &MF_CHECKED
            EndIf
            Call 'USER32.GetDlgItem' D@hwnd, DATAVIEW_LABEL_COMBO
            Call 'USER32.InvalidateRect' eax, 0, &FALSE
        .ElseIf ecx = M03_SORTBYNAME
            Call 'USER32.CheckMenuRadioItem' D$DataView.PopupMenu, M03_SORTBYNAME, M03_SORTBYADDRESS,
                                             M03_SORTBYNAME, &MF_BYCOMMAND
            Call DataView_FillDataLabelCombo D$DataLabelComboHandle 1

        .ElseIf ecx = M03_SORTBYADDRESS
            Call 'USER32.CheckMenuRadioItem' D$DataView.PopupMenu, M03_SORTBYNAME, M03_SORTBYADDRESS,
                                             M03_SORTBYADDRESS, &MF_BYCOMMAND
            Call DataView_FillDataLabelCombo D$DataLabelComboHandle 0
        .EndIf

        Mov eax 0
EndP
____________________________________________________________________________________________

[DataViewSize:
 DataView.Width: W$ ?
 DataView.Height: W$ ?]

[DATAVIEW_LABEL_COMBO 50
 DATAVIEW_VALUE_LIST 120]

; Tag Dialog 1011

Proc DataViewProc:
    Arguments @hwnd, @msg, @wParam, @lParam
    Uses ebx, esi, edi

    .If D@msg = &WM_INITDIALOG
        Call DataViewDialog_OnCreate D@hwnd

    .Else_If D@msg = &WM_SIZE
        Call DataViewDialog_OnSize D@hwnd, D@lParam

    .Else_If D@msg = &WM_COMMAND
        Call DataViewDialog_OnCommand D@hwnd, D@wParam, D@lParam

    .Else_if D@msg = WM_REFRESH_CONTENT
        Call 'USER32.GetDlgItem' D@hwnd, DATAVIEW_LABEL_COMBO
        Call DataView_OnSelectDataLabel D@hwnd, eax

    .Else_if D@msg = WM_SELECT_SYMBOL
        Call DataView_SelectSymbol D@hwnd, D@wParam

    .Else_if D@msg = &WM_SETFONT
        Call 'USER32.GetDlgItem' D@hwnd, DATAVIEW_LABEL_COMBO
        SendMessage eax, &WM_SETFONT, D@wParam, D@lParam
        Call 'USER32.GetDlgItem' D@hwnd, DATAVIEW_VALUE_LIST
        SendMessage eax, &WM_SETFONT, D@wParam, D@lParam

    .Else_if D@msg = &WM_DRAWITEM
        If D@wParam = 0
            Call DebugDialog_OnDrawMenuItem D@lParam
        ElseIf D@wParam = DATAVIEW_LABEL_COMBO
            Call DataView_OnDrawItem D@lParam
        Else
            Call DataView_OnDrawValueItem D@lParam
        EndIf
        Mov eax &TRUE

    .Else_if D@msg = &WM_MEASUREITEM
        Mov eax D@lParam
        If D@wParam = 0
          ; menu
            Call DebugDialog_OnMeasureMenuItem D@lParam
        Else
          ; listbox
            Mov D$eax+16 15
        EndIf
        Mov eax &TRUE

    .Else
        Mov eax &FALSE
    .EndIf
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; Conversion Routines
;   esi : Address of Register (in Context)
;   ecx : Size in Bytes
; Output
;   edi : Address of Ascii String

[HexSeparator: ?]

toHex:
    Mov edi StringBufTail
    Mov eax 0, B$edi 0

L0: lodsb
    Mov ah al | and ah 0F | shr al 4

    add ah '0'
    On ah > '9', add ah 7
    dec edi | Mov B$edi ah

    add al '0'
    On al > '9', add al 7
    dec edi | Mov B$edi al

    dec ecx
    jecxz L1>

    test B$HexSeparator cl | jnz L0<
    dec edi | Mov B$edi ' '

    jmp L0<

L1: ret

toAscii:
    Mov edi StringBuf
    rep movsb
    Mov B$edi 0
    Mov edi StringBuf
ret
____________________________________________________________________________________________

; Convert dword value to hex-string onto specified buffer
;   ecx : size of value (1 | 2 | 4)
;   edx : source value (dl | dx | edx)
;   edi : address of destination
; Output
;   edi : address behind hex-string
; Uses
;   eax, edx

[DwordToHex | Mov ecx 4 | Mov edx #1 | Call IntToHex]
[WordToHex  | Mov ecx 2 | Mov  dx #1 | Call IntToHex]
[ByteToHex  | Mov ecx 1 | Mov  dl #1 | Call IntToHex]

IntToHex:
    Mov eax ecx | add eax eax
    add edi eax
    Mov eax 0 ;, B$edi 0

    push edi

L0:     Mov al dl | shr edx 8
        Mov ah al | and ah 0F | shr al 4

        add ah '0'
        On ah > '9', add ah 7
        dec edi | Mov B$edi ah

        add al '0'
        On al > '9', add al 7
        dec edi | Mov B$edi al

        dec ecx | jnz L0<

    pop edi
ret
____________________________________________________________________________________________

; Special routine to output meaningful information for segment regs.
; The format is "ssss Base: bbbbbbbb Limit: llllllll"
;   esi: Address of Register
;   edi: Address of Base+Limit
; Output
;   edi: Ascii String

[SegInfo: 'xxxx Base: ' SegInfo.Base: 'xxxxxxxx Limit: ' SegInfo.Limit: 'xxxxxxxx' 0]

toSegHex:
    push edx
        Mov ecx 2, dx W$esi
        Mov esi edi
        Mov edi SegInfo
        Call IntToHex

        Mov ecx 4, edx D$esi
        Mov edi SegInfo.Base
        Call IntToHex

        Mov ecx 4, edx D$esi+4
        Mov edi SegInfo.Limit
        Call IntToHex
    pop edx
    Mov edi SegInfo
ret
____________________________________________________________________________________________

Proc toBinary:
    Uses edx

        Mov edi StringBufTail, B$edi 0

L0:     Mov dl 1
        lodsb

L1:     Mov ah '0'
        test al dl | jz L2>
        inc ah
L2:     dec edi | Mov B$edi ah
        shl dl 1
        jnc L1<

        dec edi | Mov B$edi ' '
        loop L0<
        inc edi
EndP
____________________________________________________________________________________________

Proc toSByte:
    Uses ebx, edx

        Mov edi StringBufTail, B$edi 0, ebx 10

L0:     lodsb
        Mov dl al
        test dl 0_80 | jz L1>
        neg al

L1:     Mov ah 0
        div bl
        add ah '0'
        dec edi | Mov B$edi ah
        cmp al 0 | jnz L1<

        test dl 0_80 | jz L2>
        dec edi | Mov B$edi '-'

L2:     dec edi | Mov B$edi ' '
        loop L0<

        inc edi
EndP

Proc toUByte:
    Uses ebx

        Mov edi StringBufTail, B$edi 0, ebx 10

L0:     lodsb

L1:     Mov ah 0
        div bl
        add ah '0'
        dec edi | Mov B$edi ah
        cmp al 0 | jnz L1<

        dec edi | Mov B$edi ' '
        loop L0<

        inc edi
EndP
____________________________________________________________________________________________

Proc toSWord:
    Uses ebx, edx

        Mov edi StringBufTail, B$edi 0, ebx 10
        shr ecx 1

L0:     lodsw

        test B$esi-1 0_80 | jz L4>
        neg ax

L4:     movzx eax ax
L1:     Mov edx 0
        div ebx
        add dl '0'
        dec edi | Mov B$edi dl
        cmp eax 0 | jnz L1<

        test B$esi-1 0_80 | jz L2>
        dec edi | Mov B$edi '-'

L2:     dec edi | Mov B$edi ' '
        loop L0<

        inc edi
EndP

Proc toUWord:
    Uses ebx, edx

        Mov edi StringBufTail, B$edi 0, ebx 10
        shr ecx 1

L0:     lodsw
        movzx eax ax

L1:     Mov edx 0
        div ebx
        add dl '0'
        dec edi | Mov B$edi dl
        cmp eax 0 | jnz L1<

        dec edi | Mov B$edi ' '
        loop L0<

        inc edi
EndP
____________________________________________________________________________________________

Proc toSDword:
    Uses ebx, edx

        Mov edi StringBufTail, B$edi 0, ebx 10
        shr ecx 2

L0:     lodsd

        test B$esi-1 0_80 | jz L1>
        neg eax

L1:     Mov edx 0
        div ebx
        add dl '0'
        dec edi | Mov B$edi dl
        cmp eax 0 | jnz L1<

        test B$esi-1 0_80 | jz L2>
        dec edi | Mov B$edi '-'

L2:     dec edi | Mov B$edi ' '
        loop L0<

        inc edi
EndP

Proc toUDword:
    Uses ebx, edx

        Mov edi StringBufTail, B$edi 0, ebx 10
        shr ecx 2

L0:     lodsd

L1:     Mov edx 0
        div ebx
        add dl '0'
        dec edi | Mov B$edi dl
        cmp eax 0 | jnz L1<

        dec edi | Mov B$edi ' '
        loop L0<

        inc edi
EndP
____________________________________________________________________________________________

Proc toFloat:
    Uses edx

        shr ecx 2 | dec ecx
        Mov B$StringBuf 0, edx ecx

L0:     fld D$esi+edx*4 | fstp T$FloatBuf
        Mov edi StringBuf, al 0, ecx 255
        repne scasb
        Mov B$edi-1 ' '
        Call FloatToUString FloatBuf edi

        dec edx | jns L0<

        Mov edi StringBuf | inc edi
EndP

Proc toDouble:
    Uses edx

        shr ecx 3 | dec ecx
        Mov B$StringBuf 0, edx ecx

L0:     fld Q$esi+edx*8 | fstp T$FloatBuf
        Mov edi StringBuf, al 0, ecx 255
        repne scasb
        Mov B$edi-1 ' '
        Call FloatToUString FloatBuf edi

        dec edx | jns L0<

        Mov edi StringBuf | inc edi
EndP

toExtended:
    Mov edi StringBuf
    Call FloatToUString esi edi
ret

[StringBuf: B$ ? #255 StringBufTail: B$ ?]
[FloatBuf: T$ ?]
____________________________________________________________________________________________

toHexWithAnsi:
    push esi, ecx
L0:     lodsb
        Mov ah al | and ah 0F | shr al 4
        add al '0'
        On al > '9', add al 7
        Mov B$edi al | inc edi
        add ah '0'
        On ah > '9', add ah 7
        Mov B$edi ah | inc edi
        Mov B$edi ' ' | inc edi
        loop L0<
        Mov al '"' | stosb
    pop ecx, esi
L0: lodsb
    If al < ' '
        Mov al '.'
    ElseIf al = '&'
        stosb
    EndIf
    stosb
    loop L0<
    Mov al '"' | stosb
    Mov B$edi 0
ret
____________________________________________________________________________________________

[InversedLowSigns: B$ '.|| ..^/*-+.)(:$.][][,}.{##."...']

toHexWithCookedAnsi:
    push esi, ecx
L0:     lodsb
        Mov ah al | and ah 0F | shr al 4
        add al '0'
        On al > '9', add al 7
        Mov B$edi al | inc edi
        add ah '0'
        On ah > '9', add ah 7
        Mov B$edi ah | inc edi
        Mov B$edi ' ' | inc edi
        loop L0<
        Mov al '"' | stosb
    pop ecx, esi

    xor eax eax

L0: lodsb
    On al < ' ', Mov al B$InversedLowSigns+eax
    and al 07F
    stosb
    On al = '&', stosb
    loop L0<

    Mov al '"' | stosb
    Mov B$edi 0
ret

____________________________________________________________________________________________

toHexDwords:
    Mov edx ecx
L1:     Mov ecx 4
L0:         lodsb
            Mov ah al | and ah 0F | shr al 4
            add ah '0'
            On ah > '9', add ah 7
            Mov B$edi+ecx*2-1 ah
            add al '0'
            On al > '9', add al 7
            Mov B$edi+ecx*2-2 al
        loop L0<
        Mov B$edi+8 ' '
        add edi 9
        sub edx 4
    jnz L1<
    Mov B$edi-1 0
ret
____________________________________________________________________________________________

toHexWords:
    Mov edx ecx
L1:     Mov ecx 2
L0:         lodsb
            Mov ah al | and ah 0F | shr al 4
            add ah '0'
            On ah > '9', add ah 7
            Mov B$edi+ecx*2-1 ah
            add al '0'
            On al > '9', add al 7
            Mov B$edi+ecx*2-2 al
        loop L0<
        Mov B$edi+4 ' '
        add edi 5
        sub edx 2
    jnz L1<
    Mov B$edi-1 0
ret
____________________________________________________________________________________________

Proc toFloats:
    Uses edx

        shr ecx 2
        Mov edx ecx
        Mov ecx 0-1

        While edx > 0
            fld D$esi | fstp T$FloatBuf
            Call FloatToUString FloatBuf edi
            add esi 4
            Mov al 0, ecx 255
            repne scasb
            Mov B$edi-1 ' '
            dec edx
        EndWhile
        Mov B$edi-1 0
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

DebuggerMOUSEHINT: ; Show a tooltip(-like) window.
____________________________________________________________________________________________
____________________________________________________________________________________________

; Show a hint when the user holds the mouse over a datalabel in the source-editor

;[DataHintWinHandle: ? DataHintVisible: ? DataHintTimer: ?]

[MouseHintTimer:    ?
 MouseHintVisible:  ?]

InitMouseOverDataHints:
;;
    Call 'User32.CreateWindowExA' 0, StaticClassName, 0, &WS_POPUP+&SS_LEFT,
        &CW_USEDEFAULT, &CW_USEDEFAULT, 250, D$FontHeight,
        D$H.MainWindow, 0, D$H.Instance, 0

    Mov D$DataHintWinHandle eax, D$DataHintVisible 0
    
    SendMessage D$DataHintWinHandle, &WM_SETFONT, D$DebugFontHandle, &TRUE
;;
    Call CreateNewForm_MouseHint
    Call 'USER32.SetTimer' 0, 2, 1000, MouseOverDataHint
    Mov D$MouseHintTimer eax
ret

DeleteMouseOverDataHints:
    Call 'USER32.KillTimer' 0, D$MouseHintTimer
    Mov D$MouseHintTimer 0
    Call 'USER32.DestroyWindow' D$MouseHintFormHandle
    Mov D$MouseHintFormHandle 0
ret

Proc MouseOverDataHint:
    Arguments @hwnd, @Msg, @id, @Time
    Local @Column, @Row, @Size, @Address
    Uses ebx, esi, edi

    On D$FL.IsDebugging = 0, jmp @Invalid

  ; Check if mouse is inside client space
    Call 'USER32.GetCursorPos' Point
    Call 'USER32.ScreenToClient' D$H.MainWindow, Point
    Mov eax D$PointX
    cmp eax D$RECTleft  | jl @Invalid
    cmp eax D$RECTright | jg @Invalid
    Mov edx D$PointY
    cmp edx D$RECTtop    | jl @Invalid
    cmp edx D$RECTbottom | jg @Invalid

    On D$ToolbarWanted = 1, sub edx D$ToolBarPixelsHight

    move W$MousePosX ax, W$MousePosY dx

    Call SimpleMouseTextPos

    Mov D@Column eax, D@Row ebx
    push D$CaretRow
        Call SearchTxtPtr
    pop D$CaretRow
    Mov esi eax, ecx 0-1

    Call MouseHint@ScanForSeparator
    On eax = 0-1, jmp @Invalid
    Do
        Call MouseHint@LookupEquates
    Loop_Until ebx = 0
    Call MouseHint@Evaluate
    On eax = 0, jmp @Invalid

    Call RowToX D@Column | Mov D$PointX eax
    Call LineToY D@Row   | add eax D$FontHeight
    On B$ToolbarWanted = 1, add eax D$ToolBarPixelsHight
    Mov D$PointY eax
    Call 'USER32.ClientToScreen' D$H.MainWindow, Point

    Call 'USER32.InvalidateRect' D$MouseHintFormHandle, 0, 1
    Call 'USER32.SetWindowPos' D$MouseHintFormHandle, 0, D$PointX, D$PointY, 0, 0, &SWP_NOSIZE+&SWP_SHOWWINDOW+&SWP_NOACTIVATE+&SWP_NOZORDER

    Mov D$MouseHintVisible 1
    ExitP

@Invalid:
    If D$MouseHintVisible = 1
        Call 'USER32.SetWindowPos' D$MouseHintFormHandle, 0, 0, 0, 0, 0, &SWP_HIDEWINDOW+&SWP_NOMOVE+&SWP_NOSIZE+&SWP_NOACTIVATE+&SWP_NOZORDER
        Mov D$MouseHintVisible 0
    EndIf
EndP

____________________________________________________________________________________________

[MouseHintFormHandle: ?]

; Tag Wizard Form "C:\projekte\RosAsm\Work\WizardFiles\WZRDForm0001.wwf"
CreateNewForm_MouseHint:
    Call 'User32.RegisterClassExA' MouseHintFormClass
    imul eax D$FontHeight 4
    Call 'User32.CreateWindowExA',
        &WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        MouseHintFormClassName,
        0,
        &WS_BORDER+&WS_OVERLAPPED+&WS_POPUP,
        0, 0, 160, eax,
        D$H.MainWindow,
        0,
        D$H.Instance,
        0
    Mov D$MouseHintFormHandle eax
    Call 'User32.SendMessageA' D$MouseHintFormHandle, &WM_SETFONT, D$DebugFontHandle, &TRUE
ret
____________________________________________________________________________________________
[MouseHintFormClass:
 @cbSize:        D$ len
 @style:         D$ 0
 @lpfnWndProc:   D$ MouseHintFormProc
 @cbClsExtra:    D$ 0
 @cbWndExtra:    D$ 0
 @hInstance:     D$ 0
 @hIcon:         D$ 0
 @hCursor:       D$ 0
 @hbrBackground: D$ 1
 @lpszMenuName:  D$ 0
 @lpszClassName: D$ MouseHintFormClassName
 @hIconSm:       D$ 0]
[MouseHintFormClassName: B$ "MouseHintForm" 0]
____________________________________________________________________________________________

[MH_Rect: D$ ? #4]

Proc MouseHintFormProc:
    Arguments @hwnd @msg @wParam @lParam
    Local @Brush
    Structure @PaintStruct 64 @hdc 0
    Uses esi, edi, ebx

    .If D@msg = &WM_CREATE
        Mov eax 0

    .Else_If D@msg = &WM_CLOSE
        Call 'USER32.DestroyWindow' D@hwnd

    .Else_If D@msg = &WM_DESTROY
        Mov eax 0

    .Else_If D@msg = &WM_PAINT
        If D$FL.IsDebugging = 1
            Call 'User32.BeginPaint' D@hwnd, D@PaintStruct
            Call 'GDI32.SelectObject' D@hdc, D$DebugFontHandle
            Call MouseHintDrawWindow D@hwnd, D@hdc
            Call 'User32.EndPaint' D@hwnd D@PaintStruct
        EndIf
        Mov eax 0

    .Else
        Call 'User32.DefWindowProcA' D@hwnd D@msg D@wParam D@lParam
    .End_If

EndP
; Tag End
____________________________________________________________________________________________


Proc MouseHintDrawWindow:
    Arguments @hwnd @hdc
    Local @Brush
    Structure @Rect 16 @left 0 @top 4 @right 8 @bottom 12

    [@Line1: B$ ? #16
     @Line2: B$ ? #16
     @Line3: B$ ? #16]

    Call 'USER32.GetClientRect' D@hwnd, D@Rect
    move D@bottom D$FontHeight

    Call 'GDI32.CreateSolidBrush' 0_FF_FF_FF | Mov D@Brush eax
    Call 'USER32.FillRect' D@hdc, D@Rect, D@Brush
    Call 'GDI32.DeleteObject' D@Brush

    Call 'GDI32.SetBkColor' D@hdc, 0_FF_FF_FF

    Call 'GDI32.SetTextColor' D@hdc, 0_33_33_33

  ; Output size
    Mov edi ItemString
    Mov al ' ' | stosb
    Mov al B$MouseHint@SizeMarker
    If al = 'D'
        stosb | Mov eax 'WORD' | stosd
    ElseIf al = 'W'
        Mov eax 'WORD' | stosd
    ElseIf al = 'B'
        Mov eax 'BYTE' | stosd
    ElseIf al = 'F'
        stosb | Mov eax 'LOAT' | stosd
    ElseIf al = 'R'
        Mov eax 'REAL' | stosd
    ElseIf al = 'T'
        Mov eax 'EXTE' | stosd
        Mov eax 'NDED' | stosd
    EndIf
    Mov B$edi 0

    Call 'USER32.DrawTextA' D@hdc, ItemString, 0-1, D@Rect,
                            &DT_SINGLELINE+&DT_LEFT+&DT_VCENTER

  ; Output address
    Mov edi ItemString
    DwordToHex D$MouseHint@Address
    Mov al ' ' | stosb
    Mov B$edi 0

    Call 'USER32.DrawTextA' D@hdc, ItemString, 0-1, D@Rect,
                            &DT_SINGLELINE+&DT_RIGHT+&DT_VCENTER

  ; Draw background for values
    Mov eax D$FontHeight
    lea edx D$eax*4
    move D@Top eax, D@Bottom edx

    Call 'GDI32.CreateSolidBrush' 0_DD_EE_EE | Mov D@Brush eax
    Call 'USER32.FillRect' D@hdc, D@Rect, D@Brush
    Call 'GDI32.DeleteObject' D@Brush
    Call 'GDI32.SetBkColor' D@hdc, 0_DD_EE_EE

  ; Read a block of memory with a max size of 64 byte
    Call IsProcessMemory D$MouseHint@Address
    On eax = 0, jmp @Invalid
    Mov ebx 64
    On eax < ebx, Mov ebx eax
    Call ReadProcessMem D$MouseHint@Address, DataBuffer, ebx

  ; Output value based on size selector (D$ / W$ / ...)
    Mov al B$MouseHint@SizeMarker
    Mov B@Line1 0, B@Line2 0, B@Line3 0
    .If al = 'D'
L0:     On ebx < 4, jmp @Invalid
        Mov edi @Line1
        DwordToHex D$DataBuffer
        Mov B$edi 0

        Mov esi DataBuffer, ecx 4
        Call toUDword
        Mov esi edi, edi @Line2
        Do | movsb | LoopUntil B$esi-1 = 0

        Mov esi DataBuffer, ecx 4
        Call toSDword
        Mov esi edi, edi @Line3
        Do | movsb | LoopUntil B$esi-1 = 0
    .ElseIf al = 'W'
        ; Word Size
        On ebx < 2, jmp @Invalid
        Mov edi @Line1
        WordToHex W$DataBuffer
        Mov B$edi 0

        Mov esi DataBuffer, ecx 2
        Call toUword
        Mov esi edi, edi @Line2
        Do | movsb | LoopUntil B$esi-1 = 0

        Mov esi DataBuffer, ecx 2
        Call toSword
        Mov esi edi, edi @Line3
        Do | movsb | LoopUntil B$esi-1 = 0

    .ElseIf al = 'B'
        ; Byte size
        Mov edi @Line1
        ByteToHex B$DataBuffer
        Mov B$edi 0

        Mov esi DataBuffer, ecx 1
        Call toUByte
        Mov esi edi, edi @Line2
        Do | movsb | LoopUntil B$esi-1 = 0

        Mov esi DataBuffer, ecx 1
        Call toSByte
        Mov esi edi, edi @Line3
        Do | movsb | LoopUntil B$esi-1 = 0

    .ElseIf al = 'F'
        On ebx < 4, jmp @Invalid
        Mov edi @Line2
        fld F$DataBuffer | fstp T$FloatBuf
        Call FloatToUString FloatBuf edi
        Mov al 0, ecx 0-1 | repne scasb
    .ElseIf al = 'R'
        On ebx < 8, jmp @Invalid
        Mov edi @Line2
        fld R$DataBuffer | fstp T$FloatBuf
        Call FloatToUString FloatBuf edi
        Mov al 0, ecx 0-1 | repne scasb
    .ElseIf al = 'T'
        On ebx < 10, jmp @Invalid
        Mov edi @Line2
        Call FloatToUString DataBuffer edi
        Mov al 0, ecx 0-1 | repne scasb
    .EndIf

    Mov eax D$FontHeight
    lea eax D$eax*2
    Mov D@bottom eax
    Call 'USER32.DrawTextA' D@hdc, @Line1, 0-1, D@Rect,
                            &DT_SINGLELINE+&DT_CENTER+&DT_VCENTER

    Mov eax D$FontHeight
    add D@top eax
    add D@bottom eax
    Call 'USER32.DrawTextA' D@hdc, @Line2, 0-1, D@Rect,
                            &DT_SINGLELINE+&DT_CENTER+&DT_VCENTER

    Mov eax D$FontHeight
    add D@top eax
    add D@bottom eax
    Call 'USER32.DrawTextA' D@hdc, @Line3, 0-1, D@Rect,
                            &DT_SINGLELINE+&DT_CENTER+&DT_VCENTER
    ExitP

@Invalid:
    Mov eax D$FontHeight
    lea edx D$eax*4
    move D@Top eax, D@Bottom edx
    Mov eax {'Invalid address' 0}

    Call 'USER32.DrawTextA' D@hdc, eax, 0-1, D@Rect,
                            &DT_SINGLELINE+&DT_CENTER+&DT_VCENTER
EndP
____________________________________________________________________________________________

MouseHint:
____________________________________________________________________________________________

; Get address of data symbol
; Input  :  esi -> name, ecx = len
; Output :  eax = value, edx = valid(1) invalid(0)

@EvaluateDataSymbol:

  ; Search address of data label (reuse information already stored in the label combo)
    Mov edx 0
    On D$DataLabelComboHandle = 0, ret
    Mov bl B$esi+ecx, B$esi+ecx 0 ; find routine requires zero terminated string!
    push ecx
        SendMessage D$DataLabelComboHandle, &CB_FINDSTRINGEXACT, ecx, esi
    pop ecx
    Mov B$esi+ecx bl
    Mov edx 0
    On eax = &CB_ERR, ret
    SendMessage D$DataLabelComboHandle, &CB_GETITEMDATA, eax, 0
    Mov edx 1
ret
____________________________________________________________________________________________

; Translate number representations
; Input  :  esi -> name, ecx = len
; Output :  eax = value, edx = valid(1) invalid(0)

@EvaluateNumber:
    Mov edx 0
    On B$esi < '0', ret
    On B$esi > '9', ret

    push esi ecx
        If W$esi = '00'
            Call TranslateBinary
        ElseIf B$esi = '0'
            Call TranslateHexa
        Else
            Call TranslateDecimal
        EndIf
    pop ecx esi

    Mov edx 1
ret
____________________________________________________________________________________________

; Get value of register
; Input  :  esi -> name, ecx = len
; Output :  eax = value, edx = valid(1) invalid(0)

@EvaluateRegister:
    Mov edx 0
    On B$esi <> 'E', ret
    On ecx <> 3, ret

    Mov eax D$esi
    and eax 0FF_FFFF
    Mov edi GPRegs
    Mov ecx 0

    While ecx < 8
        If D$GPRegs+ecx*4 = eax
            Mov eax D$GPRRegMap+ecx*4
            Mov eax D$eax
            Mov edx 1
            ret
        EndIf
        inc ecx
    EndWhile

    Mov edx 0
ret

@EvaluateSegmentSelector:
    Mov edx 0
    On ecx <> 2, ret
    On B$esi+1 <> 'S', ret

    Mov edx 1
    Mov al B$esi
    cmp al 'F' | jne L0>
        Mov eax D$FS.Linear | ret
L0: cmp al 'D' | jne L0>
        Mov eax D$DS.Linear | ret
L0: cmp al 'C' | jne L0>
        Mov eax D$CS.Linear | ret
L0: cmp al 'E' | jne L0>
        Mov eax D$ES.Linear | ret
L0: cmp al 'G' | jne L0>
        Mov eax D$GS.Linear | ret
L0: cmp al 'S' | jne L0>
        Mov eax D$SS.Linear | ret

L0: Mov edx 0 ; invalid seg reg
ret
____________________________________________________________________________________________

; Get value of string token
; Input  :  esi -> name, ecx = len
; Output :  eax = value, edx = success (1) failed (0)

Proc @EvaluateToken:
    Uses esi, edi, ebx, ecx

    Call @EvaluateRegister
    On edx = 1, ExitP
    Call @EvaluateNumber
    On edx = 1, ExitP
    Call @EvaluateDataSymbol
    On edx = 1, ExitP
EndP
____________________________________________________________________________________________

; Output :  eax = address

[@Address: ?]

Proc @Evaluate:

    Mov esi @Buffer
    Mov D@Address 0
    Mov ebx 0
    Mov dl addSign ; last operator
    Mov ecx 0-1

    .Do
        inc ecx
        .If B$esi+ecx < ' '
          ; operator precedence of '*' e.g. D$eax+ecx*4 -> @Address=eax, ebx=ecx
            If B$esi+ecx = mulSign
                add D@Address ebx
                Mov ebx 0
            EndIf

          ; get value of token -> eax. Break up if the token is unknown (e.g. local symbols)
            push edx
                Call @EvaluateToken
                If edx = 0
                    Mov eax 0
                    ExitP
                EndIf
            pop edx

          ; address arithmetic
            If dl = addSign
                add ebx eax
            ElseIf dl = subSign
                sub ebx eax
            ElseIf dl = mulSign
                imul ebx eax
            EndIf

          ; save operator
            Mov dl B$esi+ecx
            add esi ecx
            inc esi
            Mov ecx 0-1
        .ElseIf B$esi+ecx = ':'
          ; colon only after segment selector
            Call @EvaluateSegmentSelector
            If edx = 0
                Mov eax 0
                ExitP
            EndIf
            add ebx eax
            Mov dl addSign ; segment base implies addition of the offset
            add esi ecx
            inc esi
            Mov ecx 0-1
        .EndIf
    .Loop_Until B$esi+ecx = 0

    add D@Address ebx
    Mov eax 1
EndP
____________________________________________________________________________________________

; Get value of equate
; Input  :  esi -> name, ecx = namelen, edx = size of destination buffer
; Output :  eax = valid(1) invalid(0)

Proc @ReplaceEquate:
    Uses esi, ecx

  ; If naked local symbol, check if this belongs to the scope of the current code label.
    If B$esi = '@'
        push edi ecx
            Mov edi LabelName
            push ecx
                Mov ecx 0-1, al 0
                repne scasb
            pop ecx
            dec edi
            rep movsb
            Mov B$edi 0
        pop ecx edi
        Mov esi LabelName
    EndIf

    Call GetFromQwordCheckSum esi, D$EquateList, D$EquateListLimit

    If eax <> 0
      ; Already copied name (len=ecx) is wiped off the buffer
        add edx ecx

      ; Skip equate name
        While B$eax > LowSigns
            inc eax
        End_While
        inc eax

      ; Store equate contents. First check if there's sufficient room to store it.
        Mov esi D$eax, ecx D$eax+4
        sub edx ecx | js L8>
        rep movsb

        Mov eax 1
    Else
L8:     Mov eax 0
    End_If
EndP
____________________________________________________________________________________________

; Copy the cooked buffer and replace equates by its values. This must be done until no more
; equates are found in the expression (in case of nested equates).

Proc @LookupEquates:

  ; HACK - get current label into labelname
    Call IsProcessCode D$C.regEIP
    If eax = 1
        Call ScanLabelListForCodeLabel D$C.regEIP, 0
    Else
        Mov B$LabelName 0
    EndIf

  ; output buffer on stack
    sub esp 256
    Mov edi esp
    Mov edx 255

    Mov ebx 0
    Mov esi @Buffer
    Mov ecx 0-1

    Do
        inc ecx
        dec edx | js L8>

        Mov al B$esi+ecx
        Mov B$edi+ecx al

        .If al < ' '
          ; substitute equate with value
            Call @ReplaceEquate
            If eax = 1
                inc ebx
            Else
                add edi ecx
            EndIf

          ; restore operator (might have been overwritten, different len equate<->value)
            add esi ecx
            Mov al B$esi | inc esi
            Mov B$edi al | inc edi

            Mov ecx 0-1
        .EndIf
    Loop_Until B$esi+ecx = 0

L8: Mov ecx edi
    sub ecx esp

    Mov esi esp
    Mov edi @Buffer

    rep movsb

    Mov B$@Buffer+255 0

    Mov eax ebx ; number of replacements
EndP
____________________________________________________________________________________________

[@Buffer: B$ ? #256]

; Input
;   esi -> $ in expression
; Output
;   @Buffer contains cooked (uppercase with spaces stripped) expression

Proc @CopyAndCook:

  ; scan forward & copy
    Mov edi @Buffer, edx 0

L3:   ; next char
        inc esi
        Mov al B$esi
        On al = 167, Mov al 36

L0:     cmp dl 1 | je L0>

      ; step over legal symbol chars: A..Z a..z 0..9 _ & : @
        cmp al '&' | je L4>
        cmp al '.' | je L4>
        cmp al '0' | jb L0>
        cmp al ':' | jbe L4> ; '0'..'9', ':'
        cmp al '@' | jb L0>
        cmp al 'Z' | jbe L4> ; '@', 'A'..'Z'
        cmp al '_' | je L3<
        cmp al 'a' | jb L0>
        cmp al 'z' | jbe L2> ; 'a' .. 'z'

L0:   ; not a symbol char, check for 'connecting' chars ' ', '+', '-', '*'
        If al = '+'
            Mov dl 0-1
            Mov al addSign
            jmp L1>
        ElseIf al = '-'
            Mov dl 0-1
            Mov al subSign
            jmp L1>
        ElseIf al = '*'
            Mov dl 0-1
            Mov al mulSign
            jmp L1>
        ElseIf al = ' '
            On dl = 0, Mov dl 1
            jmp L3<
        EndIf

      ; over
        jmp L9>

L2:   ; a..z -> A..Z
        sub al 020

L4:     Mov dl 0

L1:   ; copy
        stosb

        cmp edi @Buffer+255 | je L9>

    jmp L3<


L9: Mov B$edi 0
EndP
____________________________________________________________________________________________

; Input
;   esi -> Text Ptr
; Output
;   edi -> Start of string
;   ecx = Size of string
;   eax = Expression (1) | Data declaration (0) | Invalid (-1)

[@SizeMarker: B$ ?]

Proc @ScanForSeparator:

    Mov al B$esi
    Mov dl 0 ; expect separator=1 symbol=-1

  ; scan backward
    While al <> 36 ;B$ParagraphChar ; '$' ; Dollar
        On al = 167, jmp L7>>

        cmp dl 1 | je L0>

      ; step over legal symbol chars: A..Z a..z 0..9 _ & : @ .
        cmp al '&' | je L3>
        cmp al '.' | je L3>
        cmp al '0' | jb L0>
        cmp al ':' | jbe L3> ; '0'..'9', ':'
        cmp al '@' | jb L0>
        cmp al 'Z' | jbe L3> ; '@', 'A'..'Z'
        cmp al '_' | je L1>
        cmp al 'a' | jb L0>
        cmp al 'z' | jbe L3> ; 'a' .. 'z'

L0:   ; not a symbol char, check for 'connecting' chars ' ', '+', '-', '*'
        If al = '+'
            Mov dl 0-1
            jmp L1>
        ElseIf al = '-'
            Mov dl 0-1
            jmp L1>
        ElseIf al = '*'
            Mov dl 0-1
            jmp L1>
        ElseIf al = ' '
            On dl = 0, Mov dl 1
            jmp L1>
        EndIf

      ; not preceded by '$', check for naked local "D@Local"
L5:     inc esi
        cmp B$esi ' ' | je L5<

        cmp B$esi+1 '@' | jne L2>
        Mov al B$esi
        Mov B@SizeMarker al

        jmp L4>

L3:   ; symbol
        Mov dl 0

L1:     dec esi
        Mov al B$esi
    EndWhile

L7: Mov al B$esi-1
    Mov B@SizeMarker al

L4: Call @CopyAndCook
    Mov eax 1
    ExitP

L2: ; not preceded by '$', check for data declaration
    Mov eax 0-1
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; Check if the specified buffer might be a string. It is rejected if it contains
; non-printable chars or is empty (starts with zero).
; Output in AH! [1 (yes) | 0 (no)]

Proc IsString:
    Arguments @Buffer, @Size
    Uses esi

    Mov ah 0, esi D@Buffer, ecx D@Size
L0: While B$esi <> 0
        lodsb
        Mov ah 1
        dec ecx | jz P9>
        cmp al ' ' | jb L9>
        cmp al 07F | jb L0<
        cmp al 080 | je L0<
        cmp al 091 | je L0<
        cmp al 092 | je L0<
        cmp al 0A0 | jb L9>
    EndWhile
    ExitP

L9: Mov ah 0
EndP

____________________________________________________________________________________________

; This is a printf clone for internal use in RosAsm.

[FormatString | &9=0 | C_Call FormatStr #L>1]
[C_Call | &9=&9+4 | push #2 | #+1 | Call #F | add esp &9]

; Formats a string
; The stack must be cleared by the caller - use FormatString macro!
; Invokation: Call FormatStr PatternString OutputString [Linked parameters in order of occurence]
;
; Link  | Converted to            | Expected parameter
; ______|_________________________|____________________
; %s    | string                  | address of string
; %d    | decimal                 | dword immediate
; %x    | hex with leading 0's    | dword immediate

FormatStr:
    push ebp
    Mov ebp esp

    push esi edi ebx edx ; rescue regs

    Mov esi D$ebp+12 ; pattern string >> esi
    Mov edi D$ebp+8 ; output buffer >> edi
    Mov ebx 2 ; paramter count >> ebx

L0: lodsb
    .If al = '%'
        lodsb
        If al = 's'
            ; Copy string to buffer
            push esi
                Mov esi D$ebp+8+ebx*4 | inc ebx
                cmp esi 0 | je L3>
                While B$esi <> 0 | movsb | EndWhile
        L3: pop esi
        ElseIf al = 'x'
            ; Convert to hex
            Mov eax D$ebp+8+ebx*4 | inc ebx
            Mov edx eax | add edi 7
            std
            Mov ecx 8
L1:         Mov al dl | and al 0F | cmp al 0A | jb L2>
            add al 7
L2:         add al '0' | stosb | shr edx 4 | loop L1<
            cld
            add edi 9
        ElseIf al = 'd'
            ; Convert integer to decimal representation
            Mov eax D$ebp+8+ebx*4 | inc ebx
            Call IntToStr
        Else
            stosb
        EndIf
    .Else
        stosb
    .EndIf
    cmp al 0 | jne L0<

    ; Return stringlength in ecx
    Mov ecx edi
    sub ecx D$ebp+8
    dec ecx

    pop edx ebx edi esi ; restore regs

    pop ebp
ret
____________________________________________________________________________________________

; Outputs the time in format hh:mm:ss
;   parameter : edi -> string (8 chars must fit in)
;   returns   : edi -> string (terminating null-char)
;   uses      : eax, ecx, edx

TimeToStr:
    sub esp 16

        Call 'KERNEL32.GetLocalTime' esp
        movzx eax W$esp+8
        Mov edx 0, ecx 10 | div ecx
        add al '0'
        stosb
        add dl '0' | Mov al dl
        stosb

        Mov al ':' | stosb

        movzx eax W$esp+10
        Mov edx 0, ecx 10 | div ecx
        add al '0'
        stosb
        add dl '0' | Mov al dl
        stosb

        Mov al ':' | stosb

        movzx eax W$esp+12
        Mov edx 0, ecx 10 | div ecx
        add al '0'
        stosb
        add dl '0' | Mov al dl
        stosb

        Mov B$edi 0

    add esp 16
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

DebuggerSTRINGS: ; Text visible to the user is collected here.
____________________________________________________________________________________________
____________________________________________________________________________________________

[StrRun:            'Run' 0
 StrStepInto:       'Step Into' 0
 StrStepOver:       'Step Over' 0
 StrStep:           'Step' 0
 StrReturn:         'Return to Caller' 0
 StrRet:            'Return' 0
 StrTerminate:      'Terminate' 0
 StrPause:          'Pause' 0
 StrHoldOnBp:       'Hold on breakpoints' 0
 StrInstStep:       'Instruction level stepping' 0
 StrSrcStep:        'Source level stepping' 0
 StrShowAll:        'Show segment/debug registers' 0
 StrFont:           'Choose font ...' 0
 StrCPUInfo:        'CPU info' 0
 StrFPUStatus:      'FPU status' 0
 StrShowCodeAt:     'Show code at address ...' 0
 StrAbout:          'About Debugger' 0
 StrDbgHelp:        'Debugger Help' 0
 StrContinue:       'Continue' 0
 StrBreak:          'Break' 0
 StrSettings:       'Settings' 0
 StrInformation:    'Information' 0
 StrHelp:           'Help' 0]

[StrDataFmt:        'Data Representation:' 0]

[StrF1:             'F1' 0
 StrF6:             'F6' 0
 StrF7:             'F7' 0
 StrF8:             'F8' 0
 StrCtrlF7:         'Ctrl+F7' 0
 StrCtrlF6:         'Ctrl+F6' 0
 StrCtrlF12:        'Ctrl+F12' 0]

[FPUControlWord:    'Control Word: ' 0
 FPUTagWord:        'Tag Word: ' 0
 FPURoundingMode:   'Rounding Mode: ' 0
 FPUPrecision:      'Precision: ' 0
 FPUStatusWord:     'Status Word: ' 0
 FPURndNearest:     'Nearest or even' 0
 FPURndDown:        'Round down' 0
 FPURndUp:          'Round up' 0
 FPURndTrunc:       'Truncate' 0]

[DataView:          'Data' 0
 MemoryInspector:   'Memory' 0
 CallStack:         'Call Stack' 0
 Log:               'Log' 0
 AddressSpace:      'Address Space' 0]

[FmtHex:            'Hexadecimal' 0
 FmtUDec:           'Unsigned Decimal' 0
 FmtSDec:           'Signed Decimal' 0
 FmtBinary:         'Binary' 0
 FmtFloat:          'Floating Point' 0
 FmtPUB:            'Packed Unsigned Byte' 0
 FmtPSB:            'Packed Signed Byte' 0
 FmtPUW:            'Packed Unsigned Word' 0
 FmtPSW:            'Packed Signed Word' 0
 FmtPUD:            'Packed Unsigned Dword' 0
 FmtPSD:            'Packed Signed Dword' 0
 FmtPUQ:            'Packed Unsigned Qword' 0
 FmtPSQ:            'Packed Signed Qword' 0
 FmtPF:             'Packed Single Precision Float' 0
 FmtPD:             'Packed Double Precision Float' 0
 FmtHexAnsi:        'Hexadecimal / ANSI' 0
 FmtHexDW:          'Hexadecimal Dwords' 0
 FmtHexW:           'Hexadecimal Words' 0
 FmtFloats:         'Single Precision Floats' 0
 FmtDoubles:        'Double Precision Floats' 0
 FmtAscii:          'Ascii Characters' 0
 FmtHexCooked:      'Hexadecimal / Cooked Ascii (RosAsm Development)' 0]

[StrShowInMemInsp:  'Show in memory inspector' 0
 StrShowPInMemInsp: 'Show memory pointed at' 0
 StrShowDecl:       'Show declaration' 0
 StrBreakOnW:       'Break on write access' 0
 StrBreakOnRW:      'Break on read/write access' 0
 StrSortByName:     'Sort by name' 0
 StrSortByAddr:     'Sort by address' 0]

[StrShowInvoke:     'Show invocation' 0
 StrShowAllCalls:   'Show all calls' 0
 StrHideModCalls:   'Hide module calls' 0
 StrHideIMCalls:    'Hide intra-module calls' 0
 StrShowLocals:     'Show Local data' 0]

[ACCESS_VIOLATION: B$      "ACCESS VIOLATION [C0000005]
The thread tried to read from or write to a virtual address for which it does not have the appropriate access." 0
 ARRAY_BOUNDS_EXCEEDED:    "ARRAY BOUNDS EXCEEDED [C000008C]
The thread tried to access an array element that is out of bounds and the underlying hardware supports bounds checking." 0
 DATATYPE_MISALIGNMENT:    "DATATYPE_MISALIGNMENT [80000002]
The thread tried to read or write data that is misaligned on hardware that does not provide alignment. 
For example, 16-bit values must be aligned on 2-byte boundaries; 32-bit values on 4-byte boundaries, and so on." 0
 FLT_DENORMAL_OPERAND:     "FLT DENORMAL OPERAND [C000008D]
One of the operands in a floating-point operation is denormal. 
A denormal value is one that is too small to represent as a standard floating-point value." 0
 FLT_DIVIDE_BY_ZERO:       "FLT DIVIDE BY ZERO [C000008E]
The thread tried to divide a floating-point value by a floating-point divisor of zero." 0
 FLT_INEXACT_RESULT:       "FLT INEXACT RESULT [C000008F]
The result of a floating-point operation cannot be represented exactly as a decimal fraction." 0
 FLT_INVALID_OPERATION:    "FLT INVALID OPERATION [C0000090]" 0
 FLT_OVERFLOW:             "FLT OVERFLOW [C0000091]
The exponent of a floating-point operation is greater than the magnitude allowed by the corresponding type." 0
 FLT_STACK_CHECK:          "FLT STACK CHECK [C0000092]
The stack overflowed or underflowed as the result of a floating-point operation." 0
 FLT_UNDERFLOW:            "FLT UNDERFLOW [C0000093]
The exponent of a floating-point operation is less than the magnitude allowed by the corresponding type." 0
 ILLEGAL_INSTRUCTION:      "ILLEGAL INSTRUCTION [C000001D] 
The thread tried to execute an invalid instruction." 0
 IN_PAGE_ERROR:            "IN PAGE ERROR [C0000006]
The thread tried to access a page that was not present, and the system was unable to load the page.
For example, this exception might occur if a network connection is lost while running a program over the network." 0
 INT_DIVIDE_BY_ZERO:       "INT DIVIDE BY ZERO [C0000094]
The thread tried to divide an integer value by an integer divisor of zero." 0
 INT_OVERFLOW:             "INT OVERFLOW [C0000095]
The result of an integer operation caused a carry out of the most significant bit of the result." 0
 PRIV_INSTRUCTION:         "PRIVILIGED INSTRUCTION [C0000096]
The thread tried to execute an instruction whose operation is not allowed in the current machine mode." 0
 STACK_OVERFLOW:           "STACK OVERFLOW [C00000FD]
The thread used up its stack." 0
 INVALID_DISPOSITION:      "INVALID DISPOSITION [C0000026]
An exception handler returned an invalid disposition to the exception dispatcher." 0
 NONCONTINUABLE_EXCEPTION: "NONCONTINUABLE EXCEPTION [C0000025]
The thread tried to continue execution after a noncontinuable exception occurred." 0
 UNKNOWN_EXCEPTION: "xxxxxxxx 
Unknown exception." 0]

