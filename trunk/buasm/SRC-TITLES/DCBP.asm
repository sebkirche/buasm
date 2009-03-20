TITLE DCBP            ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

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

[BreakPointLocation: D$ ?]

MarginAction:

    Mov D$FL.ReadyToRun &FALSE

    On D$LP.MEM.TABLE.BreakPoints = &NULL Call InitBreakPointsTables
;;
  eax and ebx have been set to 'MousePosX' and 'MousePosY' by a previous Call to
  'MouseTextPos' in 'DoubleClick. 'SearchTxtPtr' is going to return the Source
  Pointer from these Screen Coordinates.
;;
    Call SearchTxtPtr | Mov D$BreakPointLocation eax | sub D$STRUCT.EditData@CaretRow 1
  ; ('Dec' because 'SearchTxtPtr' forces Row 0 to 1, for usual actions)
  ; eax now the Pointer to Row 1 (Source Pointer form). Is it already recorded or not?

    .If B$Keys+&VK_SHIFT = &TRUE
        Call DeleteAllBreakpoints

    .Else_If B$Keys+&VK_CONTROL = &TRUE
        Call IsEaxInBreakPointOnTable

        Call BreakPointMenu

;;
  The BreakPoints Float Menu may generate Call's to:
  'SetBreakpoint', 'DeleteBreakpoint', 'DeleteAllBreakpoints'
;;
        inc D$STRUCT.EditData@CaretRow

    .Else
        Call IsEaxInBreakPointOnTable

        If D$FL.InsideBreakPointTable = &TRUE
            Call DeleteBreakPoint | Call DoStoreRemoveBreakPoint
        Else
            Call SetBreakPoint | Call DoStoreBreakPoint
        End_If

     .End_If

     Mov B$Keys+&VK_SHIFT &FALSE, B$Keys+&VK_CONTROL &FALSE
ret


DoubleClickMarginAction:
    On D$LP.MEM.TABLE.BreakPoints = &NULL Call InitBreakPointsTables
;;
  eax and ebx have been set to 'MousePosX' and 'MousePosY' by a previous Call to
  'MouseTextPos' in 'DoubleClick. 'SearchTxtPtr' is going to return the Source
  Pointer from these Screen Coordinates.
;;
    Call SearchTxtPtr | Mov D$BreakPointLocation eax | dec D$STRUCT.EditData@CaretRow
  ; ('Dec' because 'SearchTxtPtr' forces Row 0 to 1, for usual actions)
  ; eax now the Pointer to Row 1 (Source Pointer form). Is it already recorded or not?

;;
  The Left Button Double-Click generates: WM_LBUTTONDOWN, WM_LBUTTONUP, WM_LBUTTONDBLCLK,
  and WM_LBUTTONUP. So forth, the 'LeftButton' stuff has already (wrongly) been
  executed and we have to reverse its effect:
;;
    Call IsEaxInBreakPointOnTable

    If D$FL.InsideBreakPointTable = &TRUE
        Call DeleteBreakPoint | Call DoStoreRemoveBreakPoint
    Else
        Call SetBreakPoint | Call DoStoreBreakPoint
    End_If

    xor D$FL.InsideBreakPointTable &TRUE

    Mov D$STRUCT.EditData@CaretRow 1

    Call BreakPointMenu
;;
  The BreakPoints Float Menu may generate Call's to:
  'SetBreakpoint', 'DeleteBreakpoint', 'DeleteAllBreakpoints'
;;
ret


IsBreakPointHere:
    Call RestoreRealSource
        Call SearchTxtPtr
        While B$eax-1 <> LF | dec eax | End_While
        Mov D$BreakPointLocation eax
        Call IsEaxInBreakPointOnTable
    Call SetPartialEditionFromPos
  ; Returns 'InsideBpTable' &TRUE or &FALSE.
ret

____________________________________________________________________________________________

[MenuFloatSetBreakPoint 5600
 MenuFloatDelBreakPoint 5601
 MenuFloatDelAllBreakPoint 5602]

[MenuFloatSetBreakPointString: B$ 'Set BreakPoint [F4]' EOS 0 0 0 0]
[MenuFloatDelBreakPointString: B$ 'Delete BreakPoint [F4]' EOS 0 0 0 0]
[MenuFloatDelAllBreakPointString: B$ 'Delete all BreakPoints [Shift]/[F4]' EOS]

[FirstStrike: D$ ?]

[BreakPointMousePos: D$ ? ?
 BreakPointMouseX: D$ ?
 BreakPointMouseY: D$ ?]

Proc KeyBoardProc:
    Arguments @nCode, @wParam, @lParam

        Inc D$FirstStrike | On D$FirstStrike < 2, jmp L9>>

        pushad
            ..If D@nCode = &HC_NOREMOVE
                .If D@wParam = &VK_F4
L1:                 Call KillBpMenu

                    If D$FL.InsideBreakPointTable = &FALSE
                        Call SetBreakPoint | Call DoStoreBreakPoint
                    Else
                        Call DeleteBreakPoint | Call DoStoreRemoveBreakPoint
                    End_If

                    popad | Mov eax &TRUE | ExitP ; Not forwarding

                .Else_If D@wParam = &VK_RETURN
                    Call 'USER32.GetMenuState' D$H.MenuFloatBreakPoint, 0, &MF_BYPOSITION
                    test eax &MF_HILITE NOT_ZERO L1<

                .End_If
            ..End_If
        popad

L9:     Mov eax &FALSE ; Forwarding
EndP


KillBpMenu:
  ; Forcing the Float Menu to close

  ; Simulate a Left-Click in the middle of the Window:
    Push D$STRUCT.EditData@CaretRow, D$STRUCT.EditData@CaretLine
        Call 'USER32.GetClientRect' D$H.EditWindow, BreakPointMousePos
        shr D$BreakPointMouseX 1 | shr D$BreakPointMouseY 1
        Call 'USER32.ClientToScreen' D$H.EditWindow, BreakPointMouseX
        Call BpClick
    Pop D$STRUCT.EditData@CaretLine, D$STRUCT.EditData@CaretRow

  ; Restore the Caret initial Position:
    Call FromCaretPosToScreen | Mov D$BreakPointMouseX eax, D$BreakPointMouseY ebx
    Call BpClick
ret


; Reurns: eax = X // ebx = Y

FromCaretPosToScreen:
    Push D$STRUC.RECT.EditWindow+LEFT, D$STRUC.RECT.EditWindow+TOP
        Call 'USER32.GetClientRect' D$H.EditWindow, STRUC.RECT.EditWindow
        Call 'USER32.ClientToScreen' D$H.EditWindow, STRUC.RECT.EditWindow
        Mov eax D$STRUCT.EditData@CaretLine, ecx D$FontHeight | mul ecx | Mov ebx eax
        Mov eax D$STRUCT.EditData@CaretRow | Mov ecx D$FontWidth | mul ecx
        add eax D$STRUC.RECT.EditWindow+LEFT | add ebx D$STRUC.RECT.EditWindow+TOP
    Pop D$STRUC.RECT.EditWindow+TOP, D$STRUC.RECT.EditWindow+LEFT
ret


BpClick:
    Call 'USER32.SetCursorPos' D$BreakPointMouseX, D$BreakPointMouseY
    Call 'USER32.mouse_event' &MOUSEEVENTF_LEFTDOWN, D$BreakPointMouseX, D$BreakPointMouseY, 0, 0
    Call 'USER32.mouse_event' &MOUSEEVENTF_LEFTUP, D$BreakPointMouseX, D$BreakPointMouseY, 0, 0
ret


[BreakPointMenuPOINT: D$ ? ?]

BreakPointMenu:
  ; Build the Foating Menu:
    Call 'USER32.CreatePopupMenu' | Mov D$H.MenuFloatBreakPoint eax

    .If D$FL.InsideBreakPointTable = &FALSE
        Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint &MF_STRING, MenuFloatSetBreakPoint, MenuFloatSetBreakPointString
        Mov eax D$TABLE.BreakPointsOn

        If D$eax <> 0

            Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint, &MF_STRING, MenuFloatDelAllBreakPoint,
                                      MenuFloatDelAllBreakPointString
        End_If

    .Else
        Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint &MF_STRING, MenuFloatDelBreakPoint, MenuFloatDelBreakPointString
        Mov eax D$TABLE.BreakPointsOn
        If D$eax+4 <> 0
            Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint, &MF_STRING, MenuFloatDelAllBreakPoint,
                                      MenuFloatDelAllBreakPointString
        End_If

    .End_If

    Call 'USER32.HiliteMenuItem' D$H.EditWindow, D$H.MenuFloatBreakPoint, 0,
                                 &MF_BYPOSITION__&MF_HILITE

  ; Create the KeyBoard Hook:
    Mov D$FirstStrike 0
    Call 'KERNEL32.GetCurrentThreadId'
    Call 'USER32.SetWindowsHookExA' &WH_KEYBOARD, KeyBoardProc, &NULL, eax ; <<<<<<<<<<<
    Mov D$H.Hook eax

  ; Menu Position:
    Push D$STRUCT.EditData@CaretRow
        Mov D$STRUCT.EditData@CaretRow 1 | Call FromCaretPosToScreen
    Pop D$STRUCT.EditData@CaretRow
    Call 'USER32.TrackPopupMenu' D$H.MenuFloatBreakPoint, &TPM_LEFTALIGN , eax, ebx, 0,
                                D$H.EditWindow, &NULL

    Call 'USER32.UnhookWindowsHookEx' D$H.Hook

    Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint
ret
____________________________________________________________________________________________

[FL.InsideBreakPointTable: D$ ?]

IsEaxInBreakPointOnTable:

    Mov edx D$TABLE.BreakPointsOn

    While D$edx <> 0

        If D$edx = eax

            Mov D$FL.InsideBreakPointTable &TRUE | ret

        End_If

        add edx DWORD

    End_While

    Mov D$FL.InsideBreakPointTable &FALSE

ret
____________________________________________________________________________________________

; 0_1_000 Bytes (0_400 dwords) >>> 2*512 Dwords

[LP.MEM.TABLE.BreakPoints:
 TABLE.BreakPointsOn: D$ ?]

; No need to ever clear, as, each time a new file is loaded, the Table is released
; by 'ReInitUndo', at the same time other features are reset.

InitBreakPointsTables:

    Call VirtualAlloc LP.MEM.TABLE.BreakPoints,
                      0_1_000

    mov eax D$LP.MEM.TABLE.BreakPoints

ret

____________________________________________________________________________________________
;;
  Routines called on user choice in the BP Float Menu
  
  We reuse 'H.DebugDialog' (set to 1 in case of Mouse BP without Debugger Runing
  and to zero if none), in order to forbid editing Sources with Mouse defined BP inside
;;

SetBreakpoint:
    Call RestoreRealSource

; >>> UPDATED
    If D$BPAnteroom <> 0
        Call AddBPToAnteroom D$BreakPointLocation &TRUE
    End_If
; <<< UPDATED

    ;Call DoStoreBP

    Mov eax D$BreakPointLocation, edi D$TABLE.BreakPointsOn

    While D$edi <> 0
        add edi 4
    End_While

  ; What we are storing is the real pos, in the real Source:

    stosd

    Mov D$FL.ReadyToRun &FALSE

    Call SortBreakPointOnTable

    Call SetPartialEditionFromPos
ret


DeleteBreakpoint:
    Call RestoreRealSource

; >>> UPDATED
    If D$BPAnteroom <> 0
        Call AddBPToAnteroom D$BreakPointLocation &FALSE
    End_If
; <<< UPDATED

    Mov eax D$BreakPointLocation
    Mov esi D$TABLE.BreakPointsOn

    While D$esi <> eax
        add esi 4 | On D$esi = 0, ret
    End_While

    Mov edi esi | add esi 4
    While D$esi <> 0 | movsd | End_While
    Mov D$edi 0
    Mov D$FL.ReadyToRun &FALSE

    Call SetPartialEditionFromPos
ret


DeleteAllBreakpoints:
    Mov edi D$TABLE.BreakPointsOn

    While D$edi <> 0
        Mov eax D$edi
        Call StoreUserAction ACTION_DELDCBP, eax, &NULL
        Mov D$edi 0 | add edi 4
    End_While

    Mov D$FL.ReadyToRun &FALSE
ret


; For the Undo Feature:

DoStoreBreakPoint:
    Call StoreUserAction ACTION_DCBP, D$BreakPointLocation, &NULL
ret

DoStoreRemoveBreakPoint:
    Call StoreUserAction ACTION_DELDCBP, D$BreakPointLocation, &NULL
ret

;;
  The Undo is done without any need of some 'UnDoStoreBP' Routine, as this is done by:
  'AdjustBpTable' and 'AdjustDownwardPointers'.
;;


; This first one is called from 'SetBreakpoint' so that the Pointers are always sorted:

SortBreakPointOnTable:

    ; Sort BpOnTable:
    Mov esi D$TABLE.BreakPointsOn,
        ecx 0

    While D$esi <> 0

        add esi DWORD | add ecx DWORD

    End_While

    Call BubbleSort D$TABLE.BreakPointsOn,
                    ecx

  ; Init the Pointer to 'BpOnTable' for insertions:
  ;  Move D$BpOnTablePtr D$BpOnTable
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

Proc AdjustBreakPointTable:
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

        If D$LP.ActualTitle <> &NULL
            add eax D$LP.ActualTitle
        Else
            add eax D$CodeSource
        End_If

        Mov edi D$TABLE.BreakPointsOn

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
            Call DeleteBreakPointOnDelCRLF
            Mov ecx 0-1

        .Else_If eax = ACTION_BLOCKDELETE
            Call DeleteBreakPointInBlock
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
          ;  If D$TABLE.Titles = &NULL
          ;      add eax D$CodeSource
          ;  Else
          ;      add eax D$LP.ActualTitle
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
            Mov eax D$STRUCT.EditData@CurrentWritingPos | On B$eax-1 = LF, dec ecx
        End_If

        While D$edi <> 0
            add D$edi ecx | add edi 4
        End_While
EndP


DeleteBreakPointInBlock:
    pushad

        Mov ebx D$LP.BlockStartText,
            edx D$LP.BlockEndText

        add edx (1*ASCII)
;;
  The user is deleting a Selected Block with BreakPoints inside. Remove these BPs.
  
  The Selection Values are the real Edition ones. If TITLEs inside, we have to switch
  to the real Source. If no TITLE inside, the Values are the real Source ones and we
  have nothing to do, even though this Routine is called from:
  
  'StoreUserAction' >>> 'AdjustBpTable'  >>> 'AdjustDownwardPointers'
;;
        If D$TABLE.Titles <> &NULL
            sub ebx D$CodeSource | add ebx D$LP.ActualTitle
            sub edx D$CodeSource | add edx D$LP.ActualTitle
        End_If

        Mov esi D$TABLE.BreakPointsOn

        .While D$esi <> 0
            lodsd
            .If eax >= ebx
                If eax <= edx
                    Push esi
                      ; Direct Delete of concerned BP:
                        Mov edi esi | sub edi 4
                        While D$esi <> 0 | movsd | End_While
                        Mov D$edi 0
                    Pop esi
                End_If
            .End_If
        .End_While
    popad
ret
____________________________________________________________________________________________

; If the user [Del]s a CRLF followed by a DCBP, we remove it:

DeleteBreakPointOnDelCRLF:
    pushad
        Mov ebx D$esi+RECORD_CURRENTWRITINGPOS

        If D$TABLE.Titles <> &NULL
            add ebx D$LP.ActualTitle
        Else
            add ebx D$CodeSource
        End_If

        inc ebx

        Mov esi D$TABLE.BreakPointsOn

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

[RedPlotSize: D$ ?
 RedPlotX: D$ ?]

DrawTheRedPlotsBreakPoint:

    Push esi

    Mov eax D$FontHeight | shl eax 1 | add eax D$FontWidth | shr eax 2

    Mov D$RedPlotSize eax,
        eax D$DU.BreakPointWindowMarginWidth

    sub eax D$RedPlotSize | shr eax 1

    Mov D$RedPlotX eax

    Call 'USER32.BeginPaint' D$H.BreakPointWindow,
                             PAINTSTRUCT

    Mov D$STRUCT.EditData@HDC eax

    Call 'GDI32.SelectObject' D$STRUCT.EditData@HDC,
                              D$H.RedBrush

    Call 'GDI32.SelectObject'  D$STRUCT.EditData@HDC,
                               D$H.Font1

    Mov D$STRUCT.EditData@hfont eax

    Call 'GDI32.SetBkColor' D$STRUCT.EditData@HDC,
                            D$ARVB.BackColor

    Mov ecx D$STRUCT.EditData@LineNumber,
        D$STRUCT.EditData@Line 0

    add ecx 2

L0: Push ecx

        Call 'GDI32.TextOutA' D$STRUCT.EditData@HDC,
                              0,
                              D$STRUCT.EditData@Line,
                              BlankLine,
                              2

    Pop ecx | Mov eax D$FontHeight | add D$STRUCT.EditData@Line eax | sub ecx 1 | jnz L0<

    Mov esi D$STRUCT.EditData@UpperLine,
            D$STRUCT.EditData@Line 0

    While esi < D$LastCharPosOnScreen

        If B$esi-1 = LF

            Call DrawIfEsiInBreakPointsTable

        End_If

        add esi ASCII

        If B$esi = LF

            Mov eax D$FontHeight | add D$STRUCT.EditData@line eax

        End_If

    End_While

    Call DrawTheBreakPointLine

    Call 'USER32.EndPaint' D$H.BreakPointWindow,
                           PAINTSTRUCT

    Pop esi

ret


BlankMargin:

    Call 'USER32.BeginPaint' D$H.BreakPointWindow, PAINTSTRUCT | Mov D$STRUCT.EditData@HDC eax

    Call 'GDI32.SetBkColor' D$STRUCT.EditData@HDC D$ARVB.BackColor

    Call 'GDI32.SelectObject' D$STRUCT.EditData@HDC D$H.Font1

    Mov ecx D$STRUCT.EditData@LineNumber, D$STRUCT.EditData@Line 0 | inc ecx | inc ecx

L0: Push ecx
        Call 'GDI32.TextOutA' D$STRUCT.EditData@HDC, 0, D$STRUCT.EditData@Line, BlankLine, 2
    Pop ecx
    Mov eax D$FontHeight | add D$STRUCT.EditData@Line eax | dec ecx | cmp ecx 0 | jne L0<

    Call DrawTheBreakPointLine

    Call 'USER32.EndPaint' D$H.BreakPointWindow, PAINTSTRUCT
ret


DrawIfEsiInBreakPointsTable:

    Push ebx,
         esi

        Mov ebx D$TABLE.BreakPointsOn

        .If d$FL.RealSourceRestored = &FALSE

            If D$LP.ActualTitle <> &NULL

                sub esi D$CodeSource | add esi D$LP.ActualTitle

            End_If

        .End_If

        .While D$ebx <> 0

            If D$ebx = esi

                    ; Draw one Break-Point
                    Mov eax D$RedPlotX | add eax D$RedPlotSize

                    Mov edx D$STRUCT.EditData@Line | add edx D$RedPlotSize

                    Call 'GDI32.Ellipse' D$STRUCT.EditData@HDC,
                                         D$RedPlotX,
                                         D$STRUCT.EditData@Line,
                                         eax,
                                         edx

            End_If

            add ebx 4

        .End_While

    Pop esi,
        ebx

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
; EOT
