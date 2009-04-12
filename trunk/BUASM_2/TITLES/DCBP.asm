TITLE DCBP

;;
  "DCBP" stands for "Double-Click Break-Points"
  
   Left-Margin Double-Left-Click relative Routines:
  
  * User Double-Left-Click in the left Margin:
  
    'MarginAction', 'IsEaxInBpOnTable', 'BpMenu', 'InitBreakPointsTables'
  
  * User choice in the BP Float Menu:
  
    'SetBreakpoint', ('SortBpOnTable', 'DoStoreBP'),
    'DeleteBreakpoint', 'DeleteAllBreakpoints'
  
  * Routines Call by the Sources Editor (factual Edition):
  
    'AdjustBpTable' ('AdjustDownwardPointers' // 'DeleteBpInBlock') are called by:
    'DoStoreInsert', 'DoStoreCharDelete', 'DoStoreBlockPaste',
    'WriteUndoBlockFile', 'ControlZ'
  
  * Routines called by the Source Editor output:
  
    'DrawOneBreakPoint'
  
  * Temporary example for Pointers translation at the attention of Ludwig:
  
    'GetcodeBreakPointPosFromSourcePointer'
  
  
  The two Tables, 'BpOnTable' and 'BpOffTable' are 512 dWords each (No security
  needed with one 01000 Bytes page).
  Each dword is a Pointer to the user Source: Not to the Code 'CodeListPtr'.
  
  The Call to 'WriteBpOffTable' is actualy commented out for this Static version. 
  I suppose you will need it, for BP deleted by the user during the Debug session.
;;
____________________________________________________________________________________________

; Run by 'DoubleClick'.

[BreakPointLocation: ?]

MarginAction:
    Mov B$ReadyToRun &FALSE

    On D$BreakPointsTables = 0, Call InitBreakPointsTables
;;
  eax and ebx have been set to 'MousePosX' and 'MousePosY' by a previous Call to
  'MouseTextPos' in 'DoubleClick. 'SearchTxtPtr' is going to return the Source
  Pointer from these Screen Coordinates.
;;
    Call SearchTxtPtr | Mov D$BreakPointLocation eax | dec D$CaretRow
  ; ('Dec' because 'SearchTxtPtr' forces Row 0 to 1, for usual actions)
  ; eax now the Pointer to Row 1 (Source Pointer form). Is it already recorded or not?

    .If B$Keys+&VK_SHIFT = &TRUE
        Call DeleteAllBreakpoints

    .Else_If B$Keys+&VK_CONTROL = &TRUE
        Call IsEaxInBpOnTable

        Call BpMenu

;;
  The BreakPoints Float Menu may generate call's to:
  'SetBreakpoint', 'DeleteBreakpoint', 'DeleteAllBreakpoints'
;;
        inc D$CaretRow

    .Else
        Call IsEaxInBpOnTable

        If B$InsideBpTable = &TRUE
            Call DeleteBreakPoint | Call DoStoreRemoveBP
        Else
            Call SetBreakPoint | Call DoStoreBP
        End_If

     .End_If

     Mov B$Keys+&VK_SHIFT &FALSE, B$Keys+&VK_CONTROL &FALSE
ret


DoubleClickMarginAction:
    On D$BreakPointsTables = 0, Call InitBreakPointsTables
;;
  eax and ebx have been set to 'MousePosX' and 'MousePosY' by a previous Call to
  'MouseTextPos' in 'DoubleClick. 'SearchTxtPtr' is going to return the Source
  Pointer from these Screen Coordinates.
;;
    Call SearchTxtPtr | Mov D$BreakPointLocation eax | dec D$CaretRow
  ; ('Dec' because 'SearchTxtPtr' forces Row 0 to 1, for usual actions)
  ; eax now the Pointer to Row 1 (Source Pointer form). Is it already recorded or not?

;;
  The Left Button Double-Click generates: WM_LBUTTONDOWN, WM_LBUTTONUP, WM_LBUTTONDBLCLK,
  and WM_LBUTTONUP. So forth, the 'LeftButton' stuff has already (wrongly) been
  executed and we have to reverse its effect:
;;
    Call IsEaxInBpOnTable

    If B$InsideBpTable = &TRUE
        Call DeleteBreakPoint | Call DoStoreRemoveBP
    Else
        Call SetBreakPoint | Call DoStoreBP
    End_If

    xor B$InsideBpTable &TRUE

    Mov D$CaretRow 1

    Call BpMenu
;;
  The BreakPoints Float Menu may generate call's to:
  'SetBreakpoint', 'DeleteBreakpoint', 'DeleteAllBreakpoints'
;;
ret


IsBreakPointHere:
    Call RestoreRealSource
        Call SearchTxtPtr
        While B$eax-1 <> LF | dec eax | End_While
        Mov D$BreakPointLocation eax
        Call IsEaxInBpOnTable
    Call SetPartialEditionFromPos
  ; Returns 'InsideBpTable' &TRUE or &FALSE.
ret

____________________________________________________________________________________________

[FloatSetBp 5600    FloatDelBp 5601    FloatDelAllBp 5602]
[FloatSetBpString: B$ 'Set BreakPoint [F4]', 0 0 0 0 0
FloatDelBpString: 'Delete BreakPoint [F4]', 0 0 0 0 0
FloatDelAllBpString: 'Delete all BreakPoints [Shift]/[F4]', 0]

[FirstStrike: ?  BpMousePos: ? ? BpMouseX: ? BpMouseY: ?]

Proc KeyBoardProc:
    Arguments @nCode, @wParam, @lParam

        Inc D$FirstStrike | On D$FirstStrike < 2, jmp L9>>

        pushad
            ..If D@nCode = &HC_NOREMOVE
                .If D@wParam = &VK_F4
L1:                 Call KillBpMenu

                    If B$InsideBpTable = &FALSE
                        Call SetBreakPoint | Call DoStoreBP
                    Else
                        Call DeleteBreakPoint | Call DoStoreRemoveBP
                    End_If

                    popad | Mov eax &TRUE | ExitP ; Not forwarding

                .Else_If D@wParam = &VK_RETURN
                    Call 'USER32.GetMenuState' D$FloatHandle, 0, &MF_BYPOSITION
                    test eax &MF_HILITE | jnz L1<

                .End_If
            ..End_If
        popad

L9:     Mov eax &FALSE ; Forwarding
EndP


KillBpMenu:
  ; Forcing the Float Menu to close

  ; Simulate a Left-Click in the middle of the Window:
    push D$CaretRow, D$CaretLine
        Call 'USER32.GetClientRect' D$EditWindowHandle, BpMousePos
        shr D$BpMouseX 1 | shr D$BpMouseY 1
        Call 'USER32.ClientToScreen' D$EditWindowHandle, BpMouseX
        Call BpClick
    pop D$CaretLine, D$CaretRow

  ; Restore the Caret initial Position:
    Call FromCaretPosToScreen | Mov D$BpMouseX eax, D$BpMouseY ebx
    Call BpClick
ret


; Reurns: eax = X // ebx = Y

FromCaretPosToScreen:
    push D$EditWindowX, D$EditWindowY
        Call 'USER32.GetClientRect' D$EditWindowHandle, EditWindowX
        Call 'USER32.ClientToScreen' D$EditWindowHandle, EditWindowX
        Mov eax D$CaretLine, ecx D$FontHeight | mul ecx | Mov ebx eax
        Mov eax D$CaretRow | Mov ecx D$FontWidth | mul ecx
        add eax D$EditWindowX | add ebx D$EditWindowY
    pop D$EditWindowY, D$EditWindowX
ret


BpClick:
    Call 'USER32.SetCursorPos' D$BpMouseX, D$BpMouseY
    Call 'USER32.mouse_event' &MOUSEEVENTF_LEFTDOWN, D$BpMouseX, D$BpMouseY, 0, 0
    Call 'USER32.mouse_event' &MOUSEEVENTF_LEFTUP, D$BpMouseX, D$BpMouseY, 0, 0
ret


[BpMenuPOINT: ? ?]

BpMenu:
  ; Build the Foating Menu:
    Call 'USER32.CreatePopupMenu' | Mov D$FloatHandle eax

    .If B$InsideBpTable = &FALSE
        Call 'USER32.AppendMenuA' D$FloatHandle &MF_STRING, FloatSetBp, FloatSetBpString
        Mov eax D$BpOnTable
        If D$eax <> 0
            Call 'USER32.AppendMenuA' D$FloatHandle, &MF_STRING, FloatDelAllBp,
                                      FloatDelAllBpString
        End_If

    .Else
        Call 'USER32.AppendMenuA' D$FloatHandle &MF_STRING, FloatDelBp, FloatDelBpString
        Mov eax D$BpOnTable
        If D$eax+4 <> 0
            Call 'USER32.AppendMenuA' D$FloatHandle, &MF_STRING, FloatDelAllBp,
                                      FloatDelAllBpString
        End_If

    .End_If

    Call 'USER32.HiliteMenuItem' D$EditWindowHandle, D$FloatHandle, 0,
                                 &MF_BYPOSITION__&MF_HILITE

  ; Create the KeyBoard Hook:
    Mov D$FirstStrike 0
    Call 'KERNEL32.GetCurrentThreadId'
    Call 'USER32.SetWindowsHookExA' &WH_KEYBOARD, KeyBoardProc, &NULL, eax ; <<<<<<<<<<<
    Mov D$hHook eax

  ; Menu Position:
    push D$CaretRow
        Mov D$CaretRow 1 | Call FromCaretPosToScreen
    pop D$CaretRow
    Call 'USER32.TrackPopupMenu' D$FloatHandle, &TPM_LEFTALIGN , eax, ebx, 0,
                                D$EditWindowHandle, &NULL

    Call 'USER32.UnhookWindowsHookEx' D$hHook

    Call 'USER32.DestroyMenu' D$FloatHandle
ret
____________________________________________________________________________________________

[InsideBpTable: ?]

IsEaxInBpOnTable:
    Mov ebx D$BpOnTable

    While D$ebx <> 0
        If D$ebx = eax
            Mov B$InsideBpTable &TRUE | ret
        End_If
        add ebx 4
    End_While

    Mov B$InsideBpTable &FALSE
ret
____________________________________________________________________________________________

; 01000 Bytes (0400 dwords) >>> 2*512 dWords

[BreakPointsTables: BpOnTable: ?    BpOffTable: ?]

; No need to ever clear, as, each time a new file is loaded, the Table is released
; by 'ReInitUndo', at the same time other features are reset.

InitBreakPointsTables:
    pushad
        VirtualAlloc BreakPointsTables 01000
        add eax 0800 | Mov D$BpOffTable eax
    popad
ret

____________________________________________________________________________________________
;;
  Routines called on user choice in the BP Float Menu
  
  We reuse 'DebugDialogHandle' (set to 1 in case of Mouse BP without Debugger Runing
  and to zero if none), in order to forbid editing Sources with Mouse defined BP inside
;;

SetBreakpoint:
    Call RestoreRealSource

; >>> UPDATED
    If D$BPAnteroom <> 0
        Call AddBPToAnteroom D$BreakPointLocation &TRUE
    EndIf
; <<< UPDATED

    ;Call DoStoreBP

    Mov eax D$BreakPointLocation, edi D$BpOnTable

    While D$edi <> 0
        add edi 4
    End_While

  ; What we are storing is the real pos, in the real Source:

    stosd

    Mov B$ReadyToRun &FALSE

    Call SortBpOnTable

    Call SetPartialEditionFromPos
ret


DeleteBreakpoint:
    Call RestoreRealSource

; >>> UPDATED
    If D$BPAnteroom <> 0
        Call AddBPToAnteroom D$BreakPointLocation &FALSE
    EndIf
; <<< UPDATED

    Mov eax D$BreakPointLocation
    Mov esi D$BpOnTable

    While D$esi <> eax
        add esi 4 | On D$esi = 0, ret
    End_While

    Mov edi esi | add esi 4
    While D$esi <> 0 | movsd | End_While
    Mov D$edi 0
    Mov B$ReadyToRun &FALSE

    Call SetPartialEditionFromPos
ret


DeleteAllBreakpoints:
    Mov edi D$BpOnTable

    While D$edi <> 0
        Mov eax D$edi
        Call StoreUserAction ACTION_DELDCBP, eax, &NULL
        Mov D$edi 0 | add edi 4
    End_While

    Mov B$ReadyToRun &FALSE
ret


; For the Undo Feature:

DoStoreBP:
    Call StoreUserAction ACTION_DCBP, D$BreakPointLocation, &NULL
ret

DoStoreRemoveBP:
    Call StoreUserAction ACTION_DELDCBP, D$BreakPointLocation, &NULL
ret

;;
  The Undo is done without any need of some 'UnDoStoreBP' Routine, as this is done by:
  'AdjustBpTable' and 'AdjustDownwardPointers'.
;;


; This first one is called from 'SetBreakpoint' so that the Pointers are always sorted:

SortBpOnTable:
  ; Sort BpOnTable:
    Mov esi D$BpOnTable, ecx 0
    While D$esi <> 0 | add esi 4 | add ecx 4 | End_While
    Call BubbleSort D$BpOnTable, ecx

  ; Init the Pointer to 'BpOnTable' for insertions:
  ;  move D$BpOnTablePtr D$BpOnTable
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  If the user mofifies the Source with BreakPoints inside, we have to modify the
  Table Pointers accordingly.
  
  We make use of the Undo Table ('UndoMemory') Records to achieve this.
  
  This 'AdjustBpTable' Routine is called from:
  
  'DoStoreInsert', 'DoStoreCharDelete'
  'DoStoreBlockPaste', 'WriteUndoBlockFile'
  'ControlZ'
;;

; Called from 'StoreUserAction', 'ControlZ, 'ControlShiftZ'.

Proc AdjustBpTable:
    Argument @Type

    pushad
        Mov esi D$UndoPtr, eax D@Type

        If eax = ACTION_INSERT
            Mov eax D$esi+RECORD_CURRENTWRITINGPOS
;;
  The calls to 'DoStoreInsert' are dones after the Insertion. So, we have to
  substract the Number of inserted Chars:
;;
            sub eax D$esi+RECORD_PARAM1

        Else_If eax = ACTION_BLOCKCOPY
            Mov eax D$esi+RECORD_CURRENTWRITINGPOS

        Else_If eax = ACTION_BLOCKDELETE
            Mov eax D$esi+RECORD_CURRENTWRITINGPOS

        Else_If eax = ACTION_DEL
            Mov eax D$esi+RECORD_CURRENTWRITINGPOS

        Else_If eax = ControlZ
            Mov eax D$esi+RECORD_CURRENTWRITINGPOS

        Else_If eax = ControlShiftZ
            Mov eax D$esi+RECORD_CURRENTWRITINGPOS

        Else ; ACTION_OVERWRITE, ACTION_DCBP, ACTION_DELDCBP
            popad | ExitP

        End_If

        If D$ActualTitle <> 0
            add eax D$ActualTitle
        Else
            add eax D$CodeSource
        End_If

        Mov edi D$BpOnTable

        While D$edi <> 0
            If eax < D$edi
                Call AdjustDownwardPointers, D@Type | popad | ExitP
            End_If
            add edi 4
        End_While
    popad
EndP


Proc AdjustDownwardPointers:
    Argument @Type
      ; esi >>> last record in the UndoTable
      ; edi >>> first 'BpOnTable' Pointer coming downward from the 'CurrentWritingPos'
      ;
      ; Displacement Table are the ones from 'UndoPtr'

        Mov eax D$esi+RECORD_FLAG, ecx 0

        .If eax = ACTION_INSERT
          ;' n Chars':
            Mov ecx D$esi+RECORD_PARAM1

        .Else_If eax = ACTION_DEL
            Call DeleteBpOnDelCRLF
            Mov ecx 0-1

        .Else_If eax = ACTION_BLOCKDELETE
            Call DeleteBpInBlock
           ; Selected Block length:
            Mov ecx D$esi+RECORD_PARAM2 | sub ecx D$esi+RECORD_PARAM1 | inc ecx
            neg ecx

        .Else_If eax = ACTION_BLOCKCOPY
          ; 'ClipBoardLen' used in ControlV
            Mov ecx D$esi+RECORD_PARAM2 | sub ecx D$esi+RECORD_PARAM1 | inc ecx
;;
        .Else_If eax = ACTION_DCBP
          ;; CurrentWritingPos:
          ;  Mov eax D$esi+8
          ;  If D$TitleTable = 0
          ;      add eax D$CodeSource
          ;  Else
          ;      add eax D$ActualTitle
          ;  End_If
          ;  Mov D$BreakPointLocation eax
          ;  Call DeleteBreakpoint | ExitP
          
          ExitP
          
        .Else_If eax = ACTION_DELDCBP
            ExitP
;;
        .Else
            ExitP

        .End_If

      ; ControlZ cases:
        If D@Type = ControlZ
            neg ecx
            Mov eax D$CurrentWritingPos | On B$eax-1 = LF, dec ecx
        End_If

        While D$edi <> 0
            add D$edi ecx | add edi 4
        End_While
EndP


DeleteBpInBlock:
    pushad
        Mov ebx D$BlockStartTextPtr, edx D$BlockEndTextPtr | inc edx
;;
  The user is deleting a Selected Block with BreakPoints inside. Remove these BPs.
  
  The Selection Values are the real Edition ones. If TITLEs inside, we have to switch
  to the real Source. If no TITLE inside, the Values are the real Source ones and we
  have nothing to do, even though this Routine is called from:
  
  'StoreUserAction' >>> 'AdjustBpTable'  >>> 'AdjustDownwardPointers'
;;
        If D$TitleTable <> 0
            sub ebx D$CodeSource | add ebx D$ActualTitle
            sub edx D$CodeSource | add edx D$ActualTitle
        End_If

        Mov esi D$BpOnTable

        .While D$esi <> 0
            lodsd
            .If eax >= ebx
                If eax <= edx
                    push esi
                      ; Direct Delete of concerned BP:
                        Mov edi esi | sub edi 4
                        While D$esi <> 0 | movsd | End_While
                        Mov D$edi 0
                    pop esi
                End_If
            .End_If
        .End_While
    popad
ret
____________________________________________________________________________________________

; If the user [Del]s a CRLF followed by a DCBP, we remove it:

DeleteBpOnDelCRLF:
    pushad
        Mov ebx D$esi+RECORD_CURRENTWRITINGPOS

        If D$TitleTable <> 0
            add ebx D$ActualTitle
        Else
            add ebx D$CodeSource
        End_If

        inc ebx

        Mov esi D$BpOnTable

        .While D$esi <> 0
            lodsd
            If eax = ebx
              ; Direct Delete of concerned BP:
                Mov edi esi | sub edi 4
                While D$esi <> 0 | movsd | End_While
                Mov D$edi 0 | jmp L9>
            End_If
        .End_While
L9: popad
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; These are the Routines called by the Source Editor (at the end of 'TextOutput')

[RedPlotSize: ?   RedPlotX: ?]

DrawTheRedPlots:
    Mov eax D$FontHeight | shl eax 1 | add eax D$FontWidth | shr eax 2
    Mov D$RedPlotSize eax
    Mov eax D$BpMarginWidth | sub eax D$RedPlotSize | shr eax 1
    Mov D$RedPlotX eax

    Call 'User32.BeginPaint' D$BpWindowHandle, PAINTSTRUCT | Mov D$hdc eax

    Call 'GDI32.SelectObject' D$hdc D$RedBrushHandle
    Call 'GDI32.SelectObject'  D$hdc D$Font1Handle | Mov D$hfont eax
    Call 'GDI32.SetBkColor' D$hdc D$NormalBackColor

    Mov ecx D$LineNumber, D$Line 0 | inc ecx | inc ecx

L0: push ecx
        Call 'GDI32.TextOutA' D$hdc, 0, D$Line, BlankLine, 2
    pop ecx
    Mov eax D$FontHeight | add D$Line eax | dec ecx | cmp ecx 0 | jne L0<

    Mov esi D$UpperLine, D$Line 0

    While esi < D$LastCharPosOnScreen
        If B$esi-1 = LF
            push esi
                Call DrawIfEsiInBreakPointsTable
            pop esi
        End_If
        inc esi
        If B$esi = LF
            Mov eax D$FontHeight | add D$line eax
        End_If
    End_While

    Call DrawTheBpLine

    Call 'USER32.EndPaint' D$BpWindowHandle, PAINTSTRUCT
ret


BlankMargin:
    Call 'User32.BeginPaint' D$BpWindowHandle, PAINTSTRUCT | Mov D$hdc eax

    Call 'GDI32.SetBkColor' D$hdc D$NormalBackColor
    Call 'GDI32.SelectObject' D$hdc D$Font1Handle

    Mov ecx D$LineNumber, D$Line 0 | inc ecx | inc ecx

L0: push ecx
        Call 'GDI32.TextOutA' D$hdc, 0, D$Line, BlankLine, 2
    pop ecx
    Mov eax D$FontHeight | add D$Line eax | dec ecx | cmp ecx 0 | jne L0<

    Call DrawTheBpLine

    Call 'USER32.EndPaint' D$BpWindowHandle, PAINTSTRUCT
ret


DrawIfEsiInBreakPointsTable:
    Mov ebx D$BpOnTable

    .If B$RealSourceRestored = &FALSE
        If D$ActualTitle <> 0
            sub esi D$CodeSource | add esi D$ActualTitle
        End_If
    .End_If

    .While D$ebx <> 0
        If D$ebx = esi
            push esi, ebx
              ; Draw one Break-Point:
                Mov eax D$RedPlotX
                Mov ebx D$Line
                Mov ecx eax | add ecx D$RedPlotSize
                Mov edx ebx | add edx D$RedPlotSize
                Call 'GDI32.Ellipse' D$hdc, eax, ebx, ecx, edx
            pop ebx, esi
        End_If

        add ebx 4

    .End_While
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  This is a fully _un-tested_ ;) example about the way to switch from a dword found in
  the BP Tables -in form of Pointer to User Source- to a dword pointing to the Debuggee
  App Address space Code.
;;

; UPDATED >>>

Proc GetcodeBreakPointPosFromSourcePointer:
    Argument @SourcePtr

        Mov eax D@SourcePtr, esi D$StatementsTable, edx D$StatementsPtr
        Mov ebx D@SourcePtr

      ; 'edx' (D$StatementsPtr) should still point to the last record of 'StatementsTable'
      ; it doesn't :?

        While D$esi <> 0
            On D$esi >= eax, jmp L1>
;;
  Why '>=', up there? This is because the registred Sources Pointers, in the BP Tables,
  may point, for example, to leading spaces, and such (cases of Indents, Blank lines,
  Square Brackets Declarations,...).
;;
            add esi 4
        End_While

;;
  At 'L1:', edi should point to the Record of the searched Location in the 
  'StatementsTable' dwords Pointers Table. Now, get the Matching Code one:
;;
L1:     sub esi D$StatementsTable | add esi D$IpTable | lodsd
;;
  'eax' should now be a Pointer to Code as viewed by the Encoder. There is an
  adjustement variable called 'DebugBaseOfCode' that should enable with the true
  location inside the Debuggee App real Address space. (It is computed by 'SetCodeRVA')
  To be verified by experiment, but i think you should now say:
;;
        add eax D$DebugBaseOfCode

      ; 'eax' should ready as Return value...
EndP

; <<<
____________________________________________________________________________________________
____________________________________________________________________________________________




