TITLE Undo            ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________
;;
  Undo feature; 2046 events are stored in UndoTable:
  
  Note: 'DoStoreBP' is in the DCBP TITLE and, as the 'UnDoStoreBP' job is done by
  'AdjustBpTable' and 'AdjustDownwardPointers', there is no 'UnDoStoreBP' Routine.
  
  ____________________
  Undo Table Routines:
  
  'InitUndo', 'KillUndo', 'ReInitUndo', 'ReadUndoRecord'
  
  ______________
  "DoStore" Routines:
  
  'DoStoreInsert', 'DoStoreOverWrite', 'DoStoreCharDelete'
  'DoStoreBlockPaste' (called from 'ControlV', 'AddWizardCode' -???!!!...-)
  'WriteUndoBlockFile' (caled from 'ControlX')
  
  'DoStoreBP' (in DCBP TITLE)
  
  ____________________
  Main "UnDo" Routine: 'ControlZ', calling for:
  
  'ReInsertDeletedChar', 'UndoBlockCopy', 'ReadUndoBlockFile'
  
  __________________________
  Files Managements Routines (for Blocks):
  
  'ResetUndoFileName', 'IncUndoFileName', 'DecUndoFileName', 'DeleteUndoFiles'
  'DeleteOldUndoFiles'
  _____________
  ReDo Routines:
  
  'ControlShiftZ' >>> 'ReInsertSource'

;;
________________________________________________________________________________________
________________________________________________________________________________________

[UndoPtr: D$ ?
 UndoMemory: D$ ?] ; BuildTitleTable
;;
 The Undo Table is a rotary table 010000 sized for 2048 (0800h) records of 8 dWords:
;;

; Undo Flags:

[ACTION_INSERT 1, ACTION_OVERWRITE 2, ACTION_BLOCKCOPY 3, ACTION_BLOCKDELETE 4
 ACTION_DCBP 5, ACTION_DELDCBP 6, ACTION_DEL 7]

;[UNO_N_CHARS ecx  UNDOCHAR ebx  UNDOBLOCKBEGIN ebx  UNDOBLOCKEND edx  UNDOFLAG eax]

; Displacements Equates for pointing into one 'UndoMemory' Record:
; 0FFFF
[RECORD_CARET_ROW 0
 RECORD_CARET_LINE 4
 RECORD_CURRENTWRITINGPOS 8
 RECORD_ACTUALTITLE 12
 RECORD_UPPERLINE 16

 RECORD_PARAM1 20
 RECORD_PARAM2 24

 RECORD_FLAG 28

 UNDO_RECORD 32]
____________________________________________________________________________________________
____________________________________________________________________________________________

; The Undo Buffer Managements:

InitUndo:

    Call VirtualAlloc UndoMemory,
                      01_0000

    Move D$UndoPtr D$UndoMemory

ret


KillUndo:

   Call VirtualFree UndoMemory

ret


ReInitUndo:

    test D$CompletionWanted &TRUE ZERO S1>

    Call VirtualFree CompletionTable

    Call VirtualFree CompletionPointers

S1: Call ClearF11F12

    Mov D$TABLE.Titles &NULL, D$LP.ActualTitle &NULL

ReInitUndoOnly:

    Call DeleteUndoFiles

    Call KillUndo

    Call InitUndo

    Call CloseTree

    Call VirtualFree LP.MEM.TABLE.BreakPoints

ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; The centralized Storing Routines

Proc StoreUserAction:
    Argument @Flag, @Param1, @Param2
    Uses eax, ecx, edi

        Mov edi D$UndoPtr
    ; action_insert

        Move D$edi+RECORD_CARET_ROW D$STRUCT.EditData@CaretRow
        Move D$edi+RECORD_CARET_LINE D$STRUCT.EditData@CaretLine
        Mov eax D$STRUCT.EditData@CurrentWritingPos | sub eax D$CodeSource
        Mov D$edi+RECORD_CURRENTWRITINGPOS eax
        Move D$edi+RECORD_ACTUALTITLE D$LP.ActualTitle
        Mov eax D$STRUCT.EditData@UpperLine | sub eax D$CodeSource | Move D$edi+RECORD_UPPERLINE eax

        Move D$edi+RECORD_PARAM1 D@Param1
        Move D$edi+RECORD_PARAM2 D@Param2

        Move D$edi+RECORD_FLAG D@Flag

        On D$TABLE.BreakPointsOn <> &NULL Call AdjustBreakPointTable D@Flag

      ; Prepare the Undo Table Pointer for next registration:
        Mov edi D$UndoPtr | add di 32 | Mov D$UndoPtr edi
      ; Clear the next Record:
        Mov eax 0, ecx 8 | rep stosd
EndP


DoStoreInsert:
  ; The Callers set the Number of Chars into ebx. (Usefull for Tab Spaces):
    Call StoreUserAction ACTION_INSERT, ebx, D$InsertedChar
ret


DoStoreOverWrite:
  ; The Caller sets the overwritten Char into cl, and the new Char in al:
    and ecx 0FF | and eax 0FF
    Call StoreUserAction ACTION_OVERWRITE, ecx, eax
ret


DoStoreCharDelete:
  ; ebx set to BACK or FORTH by callers: 'BackSpace' 'StripOneChar'
    Mov esi D$STRUCT.EditData@CurrentWritingPos | movzx eax B$esi-1
  ; eax is the deleted Char:
    Call StoreUserAction ACTION_DEL, eax, ebx
ret


DoStoreBlockPaste:
    pushad
        Mov ebx D$STRUCT.EditData@CurrentWritingPos                       ; block start
        Mov edx ebx | add edx D$ClipBoardLen | dec edx    ; block end

        sub ebx D$CodeSource | sub edx D$CodeSource
        Call StoreUserAction ACTION_BLOCKCOPY, ebx, edx

        Call WriteUndoBlockFileFromClipBoard
    popad
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; The Undo Files Managements. (They are only used for the Blocks Selections Operations).

IncUndoFileName:
    Mov eax D$PointerToUndoNumber
    inc B$eax+2 | cmp B$eax+2 '9' | jbe L9>
    Mov B$eax+2 '0'
    inc B$eax+1 | cmp B$eax+1 '9' | jbe L9>
    Mov B$eax+1 '0'
    inc B$eax
L9: ;Showme D$PointerToUndoNumber

    ret

DecUndoFileName:
    ;Call 'KERNEL32.DeleteFileA' UndoFile ; Keep it, now, for Redo!!!!
    Mov eax D$PointerToUndoNumber
    dec B$eax+2 | cmp B$eax+2 '0' | jae L9>
    Mov B$eax+2 '9'
    dec B$eax+1 | cmp B$eax+1 '0' | jae L9>
    Mov B$eax+1 '9'
    dec B$eax
L9: ret


[H.UndoFile: D$ ?]

WriteUndoBlockFileFromBlock: ; ControlV ControlX
L0: Call IncUndoFileName | Mov eax UndoFile
    Call 'KERNEL32.CreateFileA' eax, &GENERIC_WRITE, 0, 0, &CREATE_ALWAYS,
                                &FILE_ATTRIBUTE_NORMAL, 0
    Mov D$H.UndoFile eax

    If eax = &INVALID_HANDLE_VALUE
        Call KillUndo | Call InitUndo
        Call SetUndoDirectory | jmp L0<
    End_If

    Mov ecx D$LP.BlockEndText | sub ecx D$LP.BlockStartText | add ecx (1*ASCII)

    Call 'KERNEL32.WriteFile' D$H.UndoFile, D$LP.BlockStartText, ecx, NumberOfReadBytes, 0
    Call 'KERNEL32.CloseHandle' D$H.UndoFile
ret


WriteUndoBlockFileFromClipBoard: ; ControlV ControlX
L0: Call IncUndoFileName | Mov eax UndoFile
    Call 'KERNEL32.CreateFileA' eax, &GENERIC_WRITE, 0, 0, &CREATE_ALWAYS,
                                &FILE_ATTRIBUTE_NORMAL, 0
    Mov D$H.UndoFile eax

    If eax = &INVALID_HANDLE_VALUE
        Call KillUndo | Call InitUndo
        Call SetUndoDirectory | jmp L0<
    End_If
    Call 'KERNEL32.WriteFile' D$H.UndoFile, D$ClipBoardPtr, D$ClipBoardLen,
                              NumberOfReadBytes, 0

    Call 'KERNEL32.CloseHandle' D$H.UndoFile
ret


[UndoBlockLen: D$ ?]
;;
  Called from 'ControlZ'. A Selection has been Delete, and save into an Undo File. 
  we re-paste this Selection:
;;
ReadUndoBlockFile: ; ControlX

    Mov D$LP.BlockStartText ebx,
        D$LP.BlockEndText ecx

    Call 'KERNEL32.CreateFileA' UndoFile, &GENERIC_READ, 0, 0, &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL, 0

    .If eax <> &INVALID_HANDLE_VALUE
        Mov D$H.UndoFile eax, D$NumberOfReadBytes 0

        Call 'KERNEL32.GetFileSize' eax, 0 | Mov D$UndoBlockLen eax
        add D$SourceLen eax | add D$STRUCT.EditData@SourceEnd eax

        Mov esi D$STRUCT.EditData@SourceEnd | add esi 400 | Mov edi esi | add edi eax
        Mov ecx edi | sub ecx D$STRUCT.EditData@CurrentWritingPos | std | rep movsb | cld
        Call 'KERNEL32.ReadFile' D$H.UndoFile, D$STRUCT.EditData@CurrentWritingPos,
                                 D$UndoBlockLen, NumberOfReadBytes, 0

        Call 'KERNEL32.CloseHandle' D$H.UndoFile
    .End_If
ret


StoreUserActionOfBlockDeletion:
    Push D$STRUCT.EditData@CurrentWritingPos
        Move D$STRUCT.EditData@CurrentWritingPos D$LP.BlockStartText
        Mov eax D$LP.BlockStartText | sub eax D$CodeSource

        Mov ebx D$LP.BlockEndText | sub ebx D$CodeSource

        Call StoreUserAction ACTION_BLOCKDELETE, eax, ebx
    Pop D$STRUCT.EditData@CurrentWritingPos
ret


DeleteUndoFiles:
    Mov eax D$PointerToUndoNumber | cmp D$eax '000.' | je L9>
L1:     Call 'KERNEL32.DeleteFileA' UndoFile
            Call DecUndoFileName
            Mov eax D$PointerToUndoNumber | cmp D$eax '000.' | ja L1<
L9: ret


; Undo Files WIN32_FIND_DATA Structure:

[DelUndo:
 @dwFileAttributes: D$ ?
 @ftCreationTime.dwLowDateTime: D$ ?
 @ftCreationTime.dwHighDateTime: D$ ?
 @ftLastAccessTime.dwLowDateTime: D$ ?
 @ftLastAccessTime.dwHighDateTime: D$ ?
 @ftLastWriteTime.dwLowDateTime: D$ ?
 @ftLastWriteTime.dwHighDateTime: D$ ?
 @nFileSizeHigh: D$ ?
 @nFileSizeLow: D$ ?
 @dwReserved0: D$ ?
 @dwReserved1: D$ ?
 @cFileName: B$ ? # &MAX_PATH
 @cAlternate: B$ ? # 14]

[H.DelUndo: D$ ?]

DeleteOldUndoFiles:
    Call 'KERNEL32.FindFirstFileA' AllUndoFiles2 DelUndo

    If eax <> &INVALID_HANDLE_VALUE
        Mov D$H.DelUndo eax
L0:     Call 'KERNEL32.DeleteFileA' DelUndo@cFileName
        Call 'KERNEL32.FindNextFileA' D$H.DelUndo DelUndo
        cmp eax &TRUE | je L0<

        Call 'KERNEL32.FindClose' D$H.DelUndo
    End_If

    Mov edi D$PointerToUndoNumber, eax '000.' | stosd
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; Main undo routine:

[ControlZreRunForCRLF: D$ ?]
[BackOrForth: D$ ?]

ControlZ:
    Mov D$FL.BlockInside &FALSE

    If D$H.DebugDialog <> 0
        Call CloseDebuggerOrIgnore | On eax = &IDNO jmp L9>>
    End_If

    Mov ebx D$UndoPtr | sub bx UNDO_RECORD
    cmp D$ebx+RECORD_FLAG 0 | je L9>>   ; depend on flag > 0 > End
        Mov D$UndoPtr ebx

    If D$TABLE.BreakPointsOn <> &NULL
        pushad
            Call AdjustBreakPointTable, ControlZ
        popad
    End_If

    Call ReadUndoRecord

  ; >>> eax = RECORD_FLAG, ebx = RECORD_PARAM1, ecx = RECORD_PARAM2

    .If eax = ACTION_INSERT
        Mov ecx ebx, ebx D$STRUCT.EditData@CurrentWritingPos
        If B$ebx-1 = LF
            Call StripBackSpace
            Mov ebx D$UndoPtr | sub bx 32 | Mov D$UndoPtr ebx
            Call StripBackSpace
            dec D$STRUCT.EditData@CurrentWritingPos, D$STRUCT.EditData@CaretRow
        Else
L1:         Push ecx
                Call StripBackSpace
            Pop ecx
            dec D$STRUCT.EditData@CaretRow
            loop L1<
        End_If

    .Else_If eax = ACTION_OVERWRITE
        dec D$STRUCT.EditData@CaretRow | Mov edi D$STRUCT.EditData@CurrentWritingPos, eax ebx | stosb

    .Else_If eax = ACTION_DEL
; Why this complicated stuff around 'ControlZreRunForCRLF': We may have to undo a CR/LF
; arasement done either by [BackSpace] or by [Del]. With [backSpace], we have first arased
; the LF and then the CR. With [Del], we have arased in the other way.
        Mov D$BackOrForth ecx
        If ebx = CR
            dec D$STRUCT.EditData@CurrentWritingPos | xor B$ControlZreRunForCRLF &TRUE
            Call ReInsertDeletedChar
            On B$ControlZreRunForCRLF = &TRUE, jmp ControlZ

        Else_If ebx = LF
            dec D$STRUCT.EditData@CurrentWritingPos | xor B$ControlZreRunForCRLF &TRUE
            Call ReInsertDeletedChar
            On B$ControlZreRunForCRLF = &TRUE, jmp ControlZ
        Else
            dec D$STRUCT.EditData@CurrentWritingPos, D$STRUCT.EditData@CaretRow | Call ReInsertDeletedChar
        End_If

        On D$BackOrForth = FORTH, dec D$STRUCT.EditData@CurrentWritingPos, D$STRUCT.EditData@CaretRow

    .Else_If eax = ACTION_BLOCKDELETE
        add ebx D$CodeSource | add ecx D$CodeSource
        Call ReadUndoBlockFile | Call DecUndoFileName

    .Else_If eax = ACTION_BLOCKCOPY
      ; 'DoStoreBlockPaste'
        add ebx D$CodeSource | add ecx D$CodeSource
        Call UndoBlockCopy | Call DecUndoFileName

    .Else_If eax = ACTION_DCBP
      ; 'DoStoreBp', 'DoStoreInsert', 'ReadUndoRecord'
        Mov D$BreakPointLocation ebx
        Call DeleteBreakPoint

    .Else_If eax = ACTION_DELDCBP
        Mov D$BreakPointLocation ebx
        Call SetBreakPoint

    .End_If

    Call AskForRedraw
L9: ret
____________________________________________________________________________________________
;;
All the Pointers in UndoPtr Table are turned into Displacements from the 'CodeSource'
origin. This origin is continuously moving when the Source increases or decreases.
;;
ReadUndoRecord:
    Mov esi D$UndoPtr

  ; When storing BPs, the Sources Pos are the ones of the Real Source. So:
   ; If D$esi+RECORD_FLAG = ACTION_DCBP
   ;     Mov ebx D$esi+RECORD_PARAM1, eax ACTION_DCBP | ret
   ; End_If

    Mov eax D$LP.ActualTitle

    If eax <> D$esi+RECORD_ACTUALTITLE
        Push esi
            Call RestoreRealSource
        Pop esi

        Mov eax D$esi+RECORD_CURRENTWRITINGPOS
        add eax D$esi+RECORD_ACTUALTITLE
        Mov D$STRUCT.EditData@CurrentWritingPos eax
        Move D$STRUCT.EditData@UpperLine D$esi+RECORD_UPPERLINE

        Push esi
            Call SetPartialEdition
        Pop esi
    End_If

    Move D$STRUCT.EditData@CaretRow D$esi+RECORD_CARET_ROW
    Move D$STRUCT.EditData@CaretLine D$esi+RECORD_CARET_LINE
    Mov eax D$esi+RECORD_CURRENTWRITINGPOS | add eax D$CodeSource | Mov D$STRUCT.EditData@CurrentWritingPos eax
    Mov eax D$esi+RECORD_UPPERLINE | add eax D$CodeSource | Mov D$STRUCT.EditData@UpperLine eax

    Mov eax D$esi+RECORD_FLAG, ebx D$esi+RECORD_PARAM1, ecx D$esi+RECORD_PARAM2
ret
____________________________________________________________________________________________

; Called by 'ControlZ':

ReInsertDeletedChar:
    MRealCaretRow

    Mov esi D$STRUCT.EditData@SourceEnd | add esi 400 | Mov edi esi      ; 400 is security 13/10/...
    Mov ecx esi | sub ecx D$STRUCT.EditData@CurrentWritingPos | inc ecx

    Mov eax ebx

    cmp al TAB | jne L1>
        cmp D$TabIs &TRUE | jne L2>
L1:         inc edi
            std
                rep movsb | stosb
            cld
            Mov ebx 1 | jmp L3>

L2:     Mov ebx D$TabIs | add edi ebx
        std
          rep movsb | Mov al SPC, ecx ebx | rep stosb
        cld

L3: add D$SourceLen ebx | add D$STRUCT.EditData@CurrentWritingPos ebx | add D$STRUCT.EditData@SourceEnd ebx

    Mov eax D$STRUCT.EditData@ColNumber

    cmp D$STRUCT.EditData@CaretRow  eax | jae L4>
    add D$STRUCT.EditData@CaretRow ebx | jmp L9>

L4: AlignOn RIGHT_FEED ebx | add D$STRUCT.EditData@RightScroll ebx | sub D$STRUCT.EditData@CaretRow ebx | dec D$STRUCT.EditData@CaretRow

L9: ret


UndoBlockCopy:

    Mov D$LP.BlockStartText ebx,
        D$LP.BlockEndText ecx,
        D$FL.BlockInside &TRUE

    Call UndoControlV
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[BACK 1, FORTH 0]

ControlShiftZ: ; ControlZ
    If D$H.DebugDialog <> 0
        Call CloseDebuggerOrIgnore | On eax = &IDNO jmp L9>>
    End_If

    Mov ebx D$UndoPtr | cmp D$ebx+RECORD_FLAG 0 | je L9>>   ; depend on flag > 0 > End
        Mov D$UndoPtr ebx

    If D$TABLE.BreakPointsOn <> &NULL
        pushad
            Call AdjustBreakPointTable, ControlShiftZ
        popad
    End_If

    Call ReadUndoRecord
  ; >>> eax = RECORD_FLAG, ebx = RECORD_PARAM1, ecx = RECORD_PARAM2

    ..If eax = ACTION_INSERT
      ; RECORD_PARAM1 = Number of Chars // RECORD_PARAM2 = Char
        dec D$STRUCT.EditData@CaretRow | sub D$STRUCT.EditData@CurrentWritingPos ebx
        Mov al cl, ecx ebx

L0:     Push eax, ecx
            If al = CR
                Call ReInsertSource | Mov al LF | add D$UndoPtr UNDO_RECORD
            End_If
            Call ReInsertSource
        Pop ecx, eax
        loop L0<

    ..Else_If eax = ACTION_OVERWRITE
      ; RECORD_PARAM1 = Overwritten Char // RECORD_PARAM2 = New Char
        Mov edi D$STRUCT.EditData@CurrentWritingPos, B$edi cl

    ..Else_If eax = ACTION_DEL ; 'DoStoreCharDelete' <<< 'BackSpace" // 'StripOneChar'
      ; RECORD_PARAM1 = Char // RECORD_PARAM2 = BACK, or FORTH
        .If ecx = FORTH
            If B$STRUCT.EditData@CurrentWritingPos = CR
                Call StripBackSpace
                Call StripBackSpace
                add D$UndoPtr UNDO_RECORD
            Else
                Call StripBackSpace
                dec D$STRUCT.EditData@CurrentWritingPos
            End_If
        .Else
            Call StripBackSpace
            dec D$STRUCT.EditData@CurrentWritingPos
            Mov eax D$STRUCT.EditData@CurrentWritingPos | dec eax
            If B$eax = CR
                Mov D$STRUCT.EditData@CurrentWritingPos eax
                Call StripBackSpace
                Call SetCaret D$STRUCT.EditData@CurrentWritingPos
                add D$UndoPtr UNDO_RECORD
            End_If
        .End_If

    ..Else_If eax = ACTION_BLOCKDELETE

        add ebx D$CodeSource | add ecx D$CodeSource

        Mov D$LP.BlockStartText ebx, D$LP.BlockEndText ecx

        Call WriteUndoBlockFileFromBlock | Call UndoControlV

        Mov ecx D$LP.BlockEndText | sub ecx D$LP.BlockStartText | add ecx (1*ASCII)

        sub D$STRUCT.EditData@CurrentWritingPos ecx | sub D$STRUCT.EditData@CaretRow ecx

    ..Else_If eax = ACTION_BLOCKCOPY
        add ebx D$CodeSource | add ecx D$CodeSource
        Call IncUndoFileName | Call ReadUndoBlockFile
        Mov eax D$UndoBlockLen | add D$STRUCT.EditData@CurrentWritingPos eax | add D$STRUCT.EditData@CaretRow eax

    ..Else_If eax = ACTION_DCBP
        Mov D$BreakPointLocation ebx
        Call SetBreakpoint

    ..Else_If eax = ACTION_DELDCBP
        Mov D$BreakPointLocation ebx
        Call DeleteBreakpoint

    ..End_If

    add D$UndoPtr UNDO_RECORD

    Call AskForRedraw
ret


; Simplified 'InsertSource':

ReInsertSource:
    Mov esi D$STRUCT.EditData@SourceEnd | add esi 400 | Mov edi esi      ; 400 is security 13/10/...
    Mov ecx esi | sub ecx D$STRUCT.EditData@CurrentWritingPos | inc ecx

L1: inc edi
    std
        rep movsb | stosb
    cld
    Mov ebx 1 | jmp L3>

L3: add D$SourceLen ebx | add D$STRUCT.EditData@CurrentWritingPos ebx | add D$STRUCT.EditData@SourceEnd ebx

    Mov eax D$STRUCT.EditData@ColNumber

    cmp D$STRUCT.EditData@CaretRow  eax | jae L4>
    add D$STRUCT.EditData@CaretRow ebx | jmp L9>
L4: Push ebx
        AlignOn RIGHT_FEED ebx | add D$STRUCT.EditData@RightScroll ebx | sub D$STRUCT.EditData@CaretRow ebx | inc D$STRUCT.EditData@CaretRow
    Pop ebx

L9: ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT

