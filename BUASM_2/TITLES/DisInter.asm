TITLE DisInter

;;
  With each successful Disassembly, the Disassembler outputs 4 Files:
  
  * 3 of them are simple copies of 'SectionsMap', 'RoutingMap' and 'SizesMap'.
  
  * The 4th one is the 'ForcedRecordsTable', storing, at least, a header for
    'DisImageBase', and followed by the user defined Records, as edited with
    the 'ForcedFlags' Dialog, if any.
    
  -----------------
  The 
  
  'ForcedFlags' >>> 'ForcedFlagsProc'
  
  is called from the Float menu
  ('RightClickOnBlock' >>> DisLabel >>> Float Menu)
  
  When this interface is called, 'DisLabelTypeWas' says if this was CODEFLAG or
  DATAFLAG, 'DisAddressWas' holds the dWord Value of the selected Label (ex: 040506B),
  and 'CopyOfLabelHexa' is the Hexa Text form of the Label Number. Same is it for
  'NextDisAddressWas' and 'CopyOfNextLabelHexa', that hold the next matching Label,
  to assume then end of a Chunk, in Strings Cases.
  
  'ForcedFlagsProc' is the main Procedure that:
  
  * Reads the Files: 'MyAppSection.map', 'MyAppRouting.map', 'MyAppSize.map',
                     and, also, the 'MyAppForced.map' File.
                     
    These 3 Files are images of the Disassembler Tables, saves at the end of each
    Disassembling. When 'ForcedFlagsProc' reads them, this is just to get the
    actual Flags of the Bytes targetted by the last Location the User selected
    in the Disassembled Source. These Flags are just read to initialize the Dialog.
;;

____________________________________________________________________________________________
____________________________________________________________________________________________

; Main of the Forced Flag Box: 'ForcedFlags' >>> 'ForcedFlagsProc'.

ForcedFlags:
    Call GetOriginalDisFileNameFromSource

    If B$SaveFilter <> 0
        Call 'USER32.DialogBoxParamA' D$H.Instance, 2500, D$H.MainWindow, ForcedFlagsProc, &NULL

        On eax = &TRUE, Call ReRunDisassembler
    Else

        Call MessageBox {B$ "FAILURE OF EDITION" EOS},
                        {B$ "The expected Original File Name was not found,
at the Top of this Source" EOS},
                        &MB_SYSTEMMODAL+&MB_USERICON

    End_If
ret


[ForcedFlagsProcHandle: ?]

; Tag Dialog 2500

Proc ForcedFlagsProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    ..If D@msg = &WM_INITDIALOG
        move D$ForcedFlagsProcHandle D@hwnd
        Mov B$ForcedFlagsModified &FALSE

        Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &EM_SETLIMITTEXT, 8, 0
        Call 'USER32.SendDlgItemMessageA' D@hwnd, 11, &EM_SETLIMITTEXT, 8, 0

        Call SetIconDialog
        Mov eax &TRUE

        Call InitForcedFlagsDialog

        If D$ForcedRecordsTable = 0

            Call MessageBox {B$ "FAILURE OF EDITION" EOS},
                            {B$ "The Forced Records File was not found" EOS},
                            &MB_SYSTEMMODAL+&MB_USERICON

            Call WM_CLOSE

        End_If

    ..Else_If D@msg = &WM_CLOSE
        Call WM_CLOSE

    ..Else_If D@msg = &WM_COMMAND
        .If D@wParam = &IDCANCEL
            Call WM_CLOSE

        .Else_If D@wParam = &IDOK
            Call RegisterUserFlags
            If B$BadUserFlag = &FALSE
                Call WriteForcedRecordsFile  ; ReadForcedRecordsFile
                Call WM_CLOSE
            End_If

        .Else_If D@wparam = ID_HELP
            Call Help, B_U_AsmName, DisMap, ContextHlpMessage

      ; [x] Code Flag
        .Else_If D@wparam = 10
            Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &BM_GETCHECK, 0, 0
            push eax
                xor eax &TRUE
                Call 'USER32.SendDlgItemMessageA' D@hwnd, 11, &BM_SETCHECK, eax, 0
            pop eax
            If eax = &TRUE
                Call DisableDisDataFlags
              ; Code cannot be 'POINTER':
                Call 'USER32.SendDlgItemMessageA' D@hwnd, 23, &BM_SETCHECK, &FALSE, 0
              ; But should be 'INSTRUCTION'
                Call 'USER32.SendDlgItemMessageA' D@hwnd, 20, &BM_SETCHECK, &TRUE, 0
            Else
                Call EnableDisDataFlags
              ; Data cannot be 'INSTRUCTION', 'EXPORTED':
                Call 'USER32.SendDlgItemMessageA' D@hwnd, 20, &BM_SETCHECK, &FALSE, 0
                Call 'USER32.SendDlgItemMessageA' D@hwnd, 22, &BM_SETCHECK, &FALSE, 0
            End_If
      ; [x] Data Flag
        .Else_If D@wparam = 11
            Call 'USER32.SendDlgItemMessageA' D@hwnd, 11, &BM_GETCHECK, 0, 0
            xor eax &TRUE
            push eax
                Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &BM_SETCHECK, eax, 0
            pop eax
            xor eax &TRUE
            If eax = &TRUE
                Call EnableDisDataFlags
              ; Data cannot be 'INSTRUCTION', 'EXPORTED':
                Call 'USER32.SendDlgItemMessageA' D@hwnd, 20, &BM_SETCHECK, &FALSE, 0
                Call 'USER32.SendDlgItemMessageA' D@hwnd, 22, &BM_SETCHECK, &FALSE, 0
            Else
                Call DisableDisDataFlags
              ; Code cannot be 'POINTER':
                Call 'USER32.SendDlgItemMessageA' D@hwnd, 23, &BM_SETCHECK, &FALSE, 0
            End_If
       ; [x] Instruction
         .Else_If D@wparam = 20
            Call CheckForcedRouting 20
       ; [x] Label
         .Else_If D@wparam = 21
            Call CheckForcedRouting 21
       ; [x] Exported
         .Else_If D@wparam = 22
            Call CheckForcedRouting 22
       ; [x] Pointer
         .Else_If D@wparam = 23
            Call CheckForcedRouting 23

      ; [x] Previous
        .Else_If D@wparam = 60
            Call DecForcedRecord

      ; [x] Next
        .Else_If D@wparam = 62
            Call IncForcedRecord

      ; [x] Delete This Record
        .Else_If D@wparam = 70
            Call DeleteForcedRecord
            If D$DisForcedRecordIndice = 0
                Call WriteForcedRecordsFile
                Call WM_CLOSE
            End_If

        .Else_If D@wparam < 30
          ; nop

      ; [x] Byte / Word / Dword / FPU4/8/10
        .Else_If D@wparam < 36
            Call Disable D$ForcedFlagsProcHandle, 51

      ;; [x] String or Unicode String
        .Else_If D@wparam < 38
            Call Enable D$ForcedFlagsProcHandle, 51
            Call SetForcedNextAddressEditControl

        .End_If

    ..Else_If D@msg = &WM_CTLCOLOREDIT
        Call WM_CTLCOLOREDIT | Return

    ..Else
        Return &FALSE

    ..End_If

    Mov eax &TRUE

L9: EndP
____________________________________________________________________________________________

; For coherency of the Routing Flags:

Proc CheckForcedRouting:
    Argument @ID

        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, D@ID, &BM_GETCHECK, 0, 0

        .If eax = &TRUE
            If D@ID < 23
              ;  >>> Cannot be Pointer:
                Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 23,
                                                  &BM_SETCHECK, &FALSE, 0
            Else
              ; 23 (Pointer) >>> Cannot be Instruction, Exported:
                Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 20,
                                                  &BM_SETCHECK, &FALSE, 0
                Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 22,
                                                  &BM_SETCHECK, &FALSE, 0
              ; But must be a dWord (ID 32):
                Call CheckForcedData 32

            End_If
        .End_If
EndP


; Tag Dialog 2500

Proc CheckForcedData:
    Argument @ID

        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 30, &BM_SETCHECK, &FALSE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 31, &BM_SETCHECK, &FALSE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 32, &BM_SETCHECK, &FALSE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 33, &BM_SETCHECK, &FALSE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 34, &BM_SETCHECK, &FALSE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 35, &BM_SETCHECK, &FALSE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 36, &BM_SETCHECK, &FALSE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 37, &BM_SETCHECK, &FALSE, 0

        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, D@ID, &BM_SETCHECK, &TRUE, 0
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[MapFileHandle: ?]

; Called from DisMain:

WriteMapFiles:
    pushad

    Call TakeCopyOfDisName

    push edi
        Mov D$edi 'Sect', D$edi+4 'ion.', D$edi+8 'map'

        Call 'KERNEL32.CreateFileA' DisassemblyMapName, &GENERIC_WRITE, 0, 0, &CREATE_ALWAYS,
                                    &FILE_ATTRIBUTE_NORMAL, 0

        If eax <> &INVALID_HANDLE_VALUE
            Mov D$MapFileHandle eax

            Mov ecx D$EndOfSectionsMap | sub ecx D$SectionsMap

            Call 'KERNEL32.WriteFile' D$MapFileHandle, D$SectionsMap, ecx,
                                      NumberOfReadBytes, 0

            Call 'KERNEL32.CloseHandle' D$MapFileHandle
        End_If

    pop edi
    push edi
        Mov D$edi 'Rout', D$edi+4 'ing.', D$edi+8 'map'

        Call 'KERNEL32.CreateFileA' DisassemblyMapName, &GENERIC_WRITE, 0, 0, &CREATE_ALWAYS,
                                    &FILE_ATTRIBUTE_NORMAL, 0

        If eax <> &INVALID_HANDLE_VALUE
            Mov D$MapFileHandle eax

            Mov ecx D$EndOfSectionsMap | sub ecx D$SectionsMap

            Call 'KERNEL32.WriteFile' D$MapFileHandle, D$SectionsMap, ecx,
                                      NumberOfReadBytes, 0

            Call 'KERNEL32.CloseHandle' D$MapFileHandle
        End_If

    pop edi
    push edi
        Mov D$edi 'Size', D$edi+4 '.map', B$edi+8 0

        Call 'KERNEL32.CreateFileA' DisassemblyMapName, &GENERIC_WRITE, 0, 0, &CREATE_ALWAYS,
                                    &FILE_ATTRIBUTE_NORMAL, 0

        If eax <> &INVALID_HANDLE_VALUE
            Mov D$MapFileHandle eax

            Mov ecx D$EndOfSectionsMap | sub ecx D$SectionsMap

            Call 'KERNEL32.WriteFile' D$MapFileHandle, D$SectionsMap, ecx,
                                      NumberOfReadBytes, 0

            Call 'KERNEL32.CloseHandle' D$MapFileHandle
        End_If
    pop edi

    popad
ret
____________________________________________________________________________________________
;;
  Called from DisMain.
  
  Must be executed in all case. For example, a user can change the AddressBase from
  exe to dll, without having yet used any Forced Record...
;;
WriteForcedRecordsFileBase:
  ; Is this usefull? (Case of empty File?)
    Call ReadForcedRecordsFile

    If D$ForcedRecordsTable = 0
        VirtualAlloc ForcedRecordsTable, 010
    End_If

  ; Really needed:
    Mov edi D$ForcedRecordsTable, eax D$DisImageBase | stosd

    Call WriteForcedRecordsFile

    VirtualFree D$ForcedRecordsTable
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

DecForcedRecord:
    If D$DisForcedRecordIndice > 1
        Call GetForcedDialogFlags
        Call RegisterForcedRecord
        dec D$DisForcedRecordIndice
        Call SetForcedFlagsFromForcedRecord
        Call SetSourceToForcedRecord
    End_If
ret

IncForcedRecord:
    Call GetForcedRecordPointer | Mov esi eax | add esi FORCED_RECORD_LENGHT

    If D$esi <> 0
        Call GetForcedDialogFlags
        Call RegisterForcedRecord
        inc D$DisForcedRecordIndice
        Call SetForcedFlagsFromForcedRecord
        Call SetSourceToForcedRecord
    End_If
ret

DeleteForcedRecord:
    Call GetForcedRecordPointer | Mov esi eax

    On D$DisForcedRecordIndice > 1, dec D$DisForcedRecordIndice

    Mov edi esi | add esi FORCED_RECORD_LENGHT

L0: movsd | movsd | movsd | cmp D$esi 0 | ja L0<

    Mov D$edi 0, D$edi+4 0, D$edi+8 0

    sub D$ForcedRecordsSize FORCED_RECORD_LENGHT

    Call SetForcedFlagsFromForcedRecord
    Call SetSourceToForcedRecord
ret

; Called by the Disassembler, when the 'WithForcedMapFile' is set to &FALSE:

DeleteForcedFile:
    Call TakeCopyOfDisName
    Mov D$edi 'Forc', D$edi+4 'ed.m', D$edi+8 'ap'

    Call 'KERNEL32.DeleteFileA' DisassemblyMapName
ret
____________________________________________________________________________________________

RegisterForcedRecord:
    Call GetForcedRecordPointer | Mov ebx eax

    move D$ebx+FORCED_RECORD_OFFSET1 D$DisAddressWas
    move D$ebx+FORCED_RECORD_OFFSET2 D$NextDisAddressWas
    Call ForcedFlagsIntoEax | Mov D$ebx+FORCED_RECORD_FLAGS eax
ret


; Returns a Pointer to the actual Forced Record:

GetForcedRecordPointer:
    push ecx
        Mov eax D$DisForcedRecordIndice | dec eax
        Mov ecx FORCED_RECORD_LENGHT | mul ecx
        add eax FORCED_FILE_HEADERLENGHT
        add eax D$ForcedRecordsTable
    pop ecx
ret
____________________________________________________________________________________________

[SearchCopyOfForcedLabel: ? #4]

; Moves the Disassembly Source to the actually edited Forced Label ([Previous]/[Next]):

SetSourceToForcedRecord:
    Mov esi CopyOfLabelHexa, edi SearchCopyOfForcedLabel

    If B$UserSectionFlag = CODEFLAG
        Mov eax 'Code'
    Else
        Mov eax 'Data'
    End_If
    stosd
    Mov ebx 4
    While B$esi <> 0 | movsb | inc ebx | End_While | Mov B$edi 0

  ; We now have a Copy of the Edited Record Label in 'SearchCopyOfForcedLabel'.
  ; Call for a Search:
    push ebx
        Call RestoreRealSource
    pop ebx
    Mov edx SearchCopyOfForcedLabel

    Call InternSearch | Call AskForRedrawNow

    If B$BlockInside = &TRUE
        Mov B$BlockInside &FALSE
        Mov esi D$CurrentWritingPos | dec esi | dec esi | dec esi | Call InternalRightClick
        Mov B$BlockInside &TRUE
    End_If

    Call SetPartialEditionFromPos
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[DisLabelTypeWas: ?    DisAddressWas: ?  NextDisAddressWas: ?
 CopyOfLabelHexa: ? ? ? ? ? ? ? ? ? ?
 CopyOfNextLabelHexa: ? ? ? ? ? ? ? ? ? ?]

[DisassemblyMapName: B$ ? #&MAXPATH]

TakeCopyOfDisName:
    Mov esi SaveFilter, edi DisassemblyMapName
    While B$esi <> 0 | movsb | End_While | Mov B$edi 0
    While B$edi <> '.' | dec edi | End_While
  ; Returns with edi >>> '.'
ret

[OriginalFileComment: B$ "
; Do not remove this Comment. It is used by the Disassembler for the interactive
; Editions of the Disassemblies Flags. Without, it would fail to guess which was
; the original File Name:
;
; "

OriginalDisFilePath: 0]

WriteOriginalFileNameInSource:
    Mov esi OriginalFileComment, ecx OriginalDisFilePath
    sub ecx OriginalFileComment | rep movsb

    Mov esi SaveFilter | While B$esi <> 0 | movsb | End_While

    Mov D$edi CRLF2 | add edi 4
ret


GetOriginalDisFileNameFromSource:
    Call RestoreRealSource

  ; Search, for example, for '; E:\':, as written by 'WriteOriginalFileNameInSource'
    Mov esi D$CodeSource
    Mov ecx OriginalDisFilePath | sub ecx OriginalFileComment
    add esi ecx

    Mov edi SaveFilter

    .If W$esi-2 = '; '
        If W$esi+1 = ':\'
            While B$esi > CR | movsb | End_While | Mov B$edi 0
        Else
            Mov B$edi 0
        End_If

    .Else
        Mov B$edi 0
    .End_If

    Call SetPartialEditionFromPos

    Call 'KERNEL32.FindFirstFileA' SaveFilter, FindFile

    If eax = &INVALID_HANDLE_VALUE
        Mov D$SaveFilter 0
    Else
        Call 'KERNEL32.FindClose' eax
    End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

InitForcedFlagsDialog:
    Call ReadForcedRecordsFile

    Call Disable D$ForcedFlagsProcHandle, 51

    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 50, &WM_SETTEXT, 0,
                                      CopyOfLabelHexa

    Mov esi D$ForcedRecordsTable, eax D$esi+FORCED_FILE_IMAGEBASE, D$DisImageBase eax

    Call GetUserSelectionIndice | On eax = 0, Call RegisterForcedRecord

    Call 'USER32.SetDlgItemInt' D$ForcedFlagsProcHandle, 61,
                                D$DisForcedRecordIndice, &FALSE

    Call TakeCopyOfDisName

    Mov ebx edi
    push ebx
        Mov D$ebx 'Sect', D$ebx+4 'ion.', D$ebx+8 'map'
        Call ReadMapFileByte D$DisAddressWas ; SectionsMap, EndOfSectionsMap
        .If D$DisassemblyMapHandle = &INVALID_HANDLE_VALUE
           ; Mov eax {'Sections Map File not found', 0} | Call MessageBox
L1:         pop ebx | Call InitForcedFlagsFromSelection | ret

        .Else
            Call InitForcedSectionsDialog

        .End_If
    pop ebx

    push ebx
        Mov D$ebx 'Rout', D$ebx+4 'ing.', D$ebx+8 'map'
        Call ReadMapFileByte D$DisAddressWas ; SectionsMap, EndOfSectionsMap
        .If D$DisassemblyMapHandle = &INVALID_HANDLE_VALUE

            Call MessageBox argh,
                            {B$ "Routing Map File not found" EOS},
                            &MB_SYSTEMMODAL+&MB_USERICON

            jmp L1<<

        .Else
            Call InitForcedRoutingDialog

        .End_If
    pop ebx

    push ebx
        Mov D$ebx 'Size', D$ebx+4 '.map', B$ebx+8 0
        Call ReadMapFileByte D$DisAddressWas

        ..If D$DisassemblyMapHandle = &INVALID_HANDLE_VALUE

            Call MessageBox argh,
                            {B$ "Sizes Map File not found" EOS},
                            &MB_SYSTEMMODAL+&MB_USERICON

            jmp L1<<

        ..Else
            Call InitForcedSizeDialog

        ..End_If
    pop ebx
ret


InitForcedSectionsDialog:
    If eax = CODEFLAG
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 10, &BM_SETCHECK, &TRUE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 11, &BM_SETCHECK, &FALSE, 0
        Call DisableDisDataFlags
    Else
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 10, &BM_SETCHECK, &FALSE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 11, &BM_SETCHECK, &TRUE, 0
        Call EnableDisDataFlags
    End_If
ret


InitForcedRoutingDialog:
    test eax INSTRUCTION | jz L1>
        push eax
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 20, &BM_SETCHECK, &TRUE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 10, &BM_SETCHECK, &TRUE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 11, &BM_SETCHECK, &FALSE, 0
        pop eax
L1: test eax NODE | jz L1>
        push eax
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 21, &BM_SETCHECK, &TRUE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 20, &BM_SETCHECK, &TRUE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 10, &BM_SETCHECK, &TRUE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 11, &BM_SETCHECK, &FALSE, 0
        pop eax
L1: ;test eax LABEL | jz L1> ; We suppose the Label always wanted
        push eax
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 21, &BM_SETCHECK, &TRUE, 0
        pop eax
L1: test eax EXPORTNODE | jz L1>
        push eax
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 22, &BM_SETCHECK, &TRUE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 21, &BM_SETCHECK, &TRUE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 20, &BM_SETCHECK, &TRUE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 10, &BM_SETCHECK, &TRUE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 11, &BM_SETCHECK, &FALSE, 0
        pop eax
L1: test eax INDIRECT | jz L1>
        push eax
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 23, &BM_SETCHECK, &TRUE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 10, &BM_SETCHECK, &FALSE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 11, &BM_SETCHECK, &TRUE, 0
        pop eax
L1: ret


InitForcedSizeDialog:
  ; Clear all Sizes Flags and disable the End Address Edit:
    push eax
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 30, &BM_SETCHECK, &FALSE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 31, &BM_SETCHECK, &FALSE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 32, &BM_SETCHECK, &FALSE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 33, &BM_SETCHECK, &FALSE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 34, &BM_SETCHECK, &FALSE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 35, &BM_SETCHECK, &FALSE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 36, &BM_SETCHECK, &FALSE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 37, &BM_SETCHECK, &FALSE, 0
        Call Disable D$ForcedFlagsProcHandle, 51
    pop eax

    .If eax = BYTE
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 30, &BM_SETCHECK, &TRUE, 0
    .Else_If eax = WORD
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 31, &BM_SETCHECK, &TRUE, 0
    .Else_If eax = DWORD
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 32, &BM_SETCHECK, &TRUE, 0
    .Else_If eax = FP4
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 33, &BM_SETCHECK, &TRUE, 0
    .Else_If eax = FP8
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 34, &BM_SETCHECK, &TRUE, 0
    .Else_If eax = FP10
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 35, &BM_SETCHECK, &TRUE, 0
    .Else_If eax = POINTER
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 23, &BM_SETCHECK, &TRUE, 0
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 32, &BM_SETCHECK, &TRUE, 0
    .Else_If eax = STRINGS+BYTE
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 36, &BM_SETCHECK, &TRUE, 0
        Call Enable D$ForcedFlagsProcHandle, 51
    .Else_If eax = STRINGS+WORD
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 37, &BM_SETCHECK, &TRUE, 0
        Call Enable D$ForcedFlagsProcHandle, 51
    .End_If
ret
____________________________________________________________________________________________

; First Record, for a given Disassembly. We take the Infos from the user's selection:

InitForcedFlagsFromSelection:
    Mov eax D$DisLabelTypeWas | Call InitForcedSectionsDialog

    ..If D$DisLabelTypeWas = CODEFLAG
        Mov eax D$BlockEndTextPtr
        If W$eax +1 = '::'
            Mov eax EXPORTNODE
        Else
            Mov eax 0
        End_If
        add eax INSTRUCTION+LABEL | Call InitForcedRoutingDialog
        Call DisableDisDataFlags

    ..Else
        Mov eax D$BlockEndTextPtr, edx D$eax+3, bl B$eax+6, al B$eax+3

        If edx = 'Data'
            Mov eax INDIRECT | Call InitForcedRoutingDialog
            Mov eax DWORD
        Else_If edx = 'Code'
            Mov eax INDIRECT | Call InitForcedRoutingDialog
            Mov eax DWORD
        Else_If al = 'B'
            Mov eax BYTE
            On bl = '"', add eax STRINGS
        Else_If al = 'W'
            Mov eax WORD
        Else_If al = 'D'
            Mov eax DWORD
        Else_If al = 'F'
            Mov eax FP4
        Else_If al = 'R'
            Mov eax FP8
        Else_If al = 'T'
            Mov eax FP10
        Else_If al = 'U'
            Mov eax STRINGS+WORD
        End_If

        Call InitForcedSizeDialog

    ..End_If

    Call Disable D$ForcedFlagsProcHandle, 60
    Call Disable D$ForcedFlagsProcHandle, 62
ret


DisableDisDataFlags:
    Call Disable D$ForcedFlagsProcHandle, 30
    Call Disable D$ForcedFlagsProcHandle, 31
    Call Disable D$ForcedFlagsProcHandle, 32
    Call Disable D$ForcedFlagsProcHandle, 33
    Call Disable D$ForcedFlagsProcHandle, 34
    Call Disable D$ForcedFlagsProcHandle, 35
    Call Disable D$ForcedFlagsProcHandle, 36
    Call Disable D$ForcedFlagsProcHandle, 37
    Call Disable D$ForcedFlagsProcHandle, 51
ret


EnableDisDataFlags:
    Call Enable D$ForcedFlagsProcHandle, 30
    Call Enable D$ForcedFlagsProcHandle, 31
    Call Enable D$ForcedFlagsProcHandle, 32
    Call Enable D$ForcedFlagsProcHandle, 33
    Call Enable D$ForcedFlagsProcHandle, 34
    Call Enable D$ForcedFlagsProcHandle, 35
    Call Enable D$ForcedFlagsProcHandle, 36
    Call Enable D$ForcedFlagsProcHandle, 37
    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 36, &BM_GETCHECK, 0, 0
    push eax
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 37, &BM_GETCHECK, 0, 0
    pop ebx
    or eax ebx
    If eax <> 0
        Call Enable D$ForcedFlagsProcHandle, 51
    End_If
ret


SetForcedNextAddressEditControl:
    Call GetForcedRecordPointer | Mov eax D$eax+FORCED_RECORD_OFFSET2

    Mov D$CopyOfNextLabelHexa 0

    If eax <> 0
        Mov edi CopyOfNextLabelHexa | Call WriteEax | Mov B$edi 0
    End_If

    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 51, &WM_SETTEXT,
                                      0, CopyOfNextLabelHexa
ret
____________________________________________________________________________________________

[DisassemblyMapHandle: ?  DisassemblyMapPtr: ?  DisassemblyMapLen: ?]
;;
  Reads one Byte, in any Map (SectionsMap, , 'RoutingMap' or 'SizesMap'):
;;
Proc ReadMapFileByte:
    Argument @Ptr
        Call 'KERNEL32.CreateFileA' DisassemblyMapName, &GENERIC_READ,
                                    &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_NORMAL, 0
        Mov D$DisassemblyMapHandle eax

        .If eax <> &INVALID_HANDLE_VALUE

            Call 'KERNEL32.GetFileSize' eax, 0 | Mov D$DisassemblyMapLen eax
                add eax 10

            VirtualAlloc DisassemblyMapPtr eax

            Call 'KERNEL32.ReadFile' D$DisassemblyMapHandle, D$DisassemblyMapPtr,
                                     D$DisassemblyMapLen, NumberOfReadBytes, 0

            Call 'KERNEL32.CloseHandle' D$DisassemblyMapHandle

            Mov eax D@Ptr | sub eax D$DisImageBase | add eax D$DisassemblyMapPtr
            movzx eax B$eax
          ; Flags Byte in eax

            push eax
                VirtualFree D$DisassemblyMapPtr
            pop eax
        .End_If
EndP
____________________________________________________________________________________________

[ForcedRecordsTable: ?  ForcedRecordsSize: ?]
;;
  'ForcedRecordsTable' holds Records of 2 dWords.
  
  * The leading Record is only for storing the 'DisImageBase', and is written at
    the end of the Disassembly Process, by the Disassembler itself.
  
  * The next Records store the user Editions: ... // DWORD Offset / DWORD Flags // ...
    where "Offset" is the displacement inside any Flags Table ('SectionsMap',
    'RoutingMap' and 'SizesMap'), plus the 'DisImageBase', and where "Flags" holds
    the 3 Flags Types, in order:
    
    Low Byte = SizesMap (Example, if Data: STRINGS+BYTE)
    Next Byte = RoutingMap Flag (Example INSTRUCTION+LABEL+EXPORTNODE)
    Next Byte = SectionsMap Flag (Example CODEFLAG)
    
    ... that are managed by 'ForcedFlagsIntoEax', 'EaxIntoForcedFlags'
    
    An "empty" Forced Records Table holds 8 Bytes, at least, for the DisImageBase.
;;

[FORCED_FILE_IMAGEBASE 0, FORCED_FILE_HEADERLENGHT 4
 FORCED_RECORD_OFFSET1 0, FORCED_RECORD_OFFSET2 4, FORCED_RECORD_FLAGS 8,
 FORCED_RECORD_LENGHT 12]

ReadForcedRecordsFile: ; WriteForcedRecordsFile
    VirtualFree D$ForcedRecordsTable

    Call TakeCopyOfDisName
    Mov D$edi 'Forc', D$edi+4 'ed.m', D$edi+8 'ap'

    Call 'KERNEL32.CreateFileA' DisassemblyMapName, &GENERIC_READ,
                                &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL, 0

    If eax <> &INVALID_HANDLE_VALUE
        Mov D$DisassemblyMapHandle eax

        Call 'KERNEL32.GetFileSize' D$DisassemblyMapHandle, 0
        Mov D$ForcedRecordsSize eax | add eax 010
        VirtualAlloc ForcedRecordsTable eax

        Call 'KERNEL32.ReadFile' D$DisassemblyMapHandle, D$ForcedRecordsTable,
                                 D$ForcedRecordsSize, NumberOfReadBytes, 0

        Call 'KERNEL32.CloseHandle' D$DisassemblyMapHandle

    Else
        Mov D$ForcedRecordsTable 0

    End_If
ret


IsForcedMapFile:
    Call TakeCopyOfDisName

    Mov D$edi 'Forc', D$edi+4 'ed.m', D$edi+8 'ap'

    Call 'KERNEL32.FindFirstFileA' DisassemblyMapName, FindFile

    If eax <> &INVALID_HANDLE_VALUE
        Call 'KERNEL32.FindClose' eax
        Mov eax &TRUE
    Else
        Mov eax &FALSE
    End_If
ret
____________________________________________________________________________________________

; Called from 'DisMain':

ForceRecordsToMaps:
    Mov esi D$ForcedRecordsTable | Mov edx D$esi+FORCED_FILE_IMAGEBASE
    add esi FORCED_FILE_HEADERLENGHT

    .While D$esi <> 0
        Mov eax D$esi+FORCED_RECORD_FLAGS | Call EaxIntoForcedFlags

      ; Take the Displacement to the Map Tables:
        Mov ebx D$esi+FORCED_RECORD_OFFSET1
      ; edx is the previously saved 'DisImageBase':

        sub ebx edx

      ; Force the SectionsMap:
      ; Useless: Everything is empty at this stage:
      ;  Mov edi D$SectionsMap | add edi ebx
      ;  Mov ecx D$esi+FORCED_RECORD_OFFSET2 | sub ecx D$esi+FORCED_RECORD_OFFSET1 | jecxz L1>
      ;  Mov al 0 | rep stosb

L1:     Mov edi D$SectionsMap, al B$UserSectionFlag, B$edi+ebx al

      ; Force the RoutingMap:
        Mov edi D$RoutingMap, al B$UserRoutingFlag
        On B$UserSectionFlag = CODEFLAG, or al LABEL+NODE+EVOCATED+INSTRUCTION+ACCESSED
        Mov B$edi+ebx al

      ; Force the SizesMap:
        Mov edi D$SizesMap, al B$UserSizeFlag
        .If B$UserSectionFlag = CODEFLAG
            Mov B$edi+ebx 0
        .Else
;;
  If Data, we may have to Flag Chunks of Bytes, in the SizesMap, instead of one
  single Byte, and to also flag the SectionsMap as Data:
;;
            If al = BYTE
                Call ForceDataFlag eax, 1
            Else_If al = WORD
                Call ForceDataFlag eax, 2
            Else_If al = DWORD
                Call ForceDataFlag eax, 4
            Else_If al = FP4
                Call ForceDataFlag eax, 4
            Else_If al = FP8
                Call ForceDataFlag eax, 8
            Else_If al = FP10
                Call ForceDataFlag eax, 10
            Else_If al = POINTER
                Call ForceDataFlag eax, 4
            Else_If al = STRINGS+BYTE
                Call ForceDataFlag eax, 0-1
            Else_If al = STRINGS+WORD
                Call ForceDataFlag eax, 0-1
            End_If
        .End_If

        add esi FORCED_RECORD_LENGHT
    .End_While
;map
ret

Proc ForceDataFlag:
    Argument @Flag, @n

        Mov al B@Flag

        .If D@n = 0-1
          ; Flag Strings down to the next Label:
            Mov ecx D$esi+FORCED_RECORD_OFFSET2 | sub ecx D$esi+FORCED_RECORD_OFFSET1

            push ebx, ecx
L0:             Mov B$edi+ebx al | inc ebx | loop L0<
            pop ecx, ebx

            Mov edi D$SectionsMap
L0:         Mov B$edi+ebx DATAFLAG | inc ebx | loop L0<

        .Else
            push ebx
                Mov ecx D@n
L0:             Mov B$edi+ebx al | inc ebx | loop L0<
            pop ebx

            push ebx
                Mov ecx D@n, edi D$SectionsMap
L0:             Mov B$edi+ebx DATAFLAG | inc ebx | loop L0<
            pop ebx
        .End_If
EndP
____________________________________________________________________________________________

WriteForcedRecordsFile: ; ReadForcedRecordsFile
    Call TakeCopyOfDisName
    Mov D$edi 'Forc', D$edi+4 'ed.m', D$edi+8 'ap'

    Call 'KERNEL32.CreateFileA' DisassemblyMapName, &GENERIC_WRITE,
                                &FILE_SHARE_READ, 0, &CREATE_ALWAYS,
                                &FILE_ATTRIBUTE_NORMAL, 0
    Mov D$DisassemblyMapHandle eax

    If eax <> &INVALID_HANDLE_VALUE
        Mov esi D$ForcedRecordsTable, ecx FORCED_FILE_HEADERLENGHT
        add esi ecx

        While D$esi <> 0
            add esi FORCED_RECORD_LENGHT | add ecx FORCED_RECORD_LENGHT
        End_While

        Call 'KERNEL32.WriteFile' D$DisassemblyMapHandle, D$ForcedRecordsTable,
                                  ecx, NumberOfReadBytes, 0

        Call 'KERNEL32.CloseHandle' D$DisassemblyMapHandle
    End_If
ret
____________________________________________________________________________________________

[DisForcedRecordIndice: ?]

GetUserSelectionIndice:
  ; Substract the 'DisImageBase' from the selected Label dWord:
    Mov eax D$DisAddressWas
;;
  Scan all Records, in 'ForcedRecordsTable'. In case a same Offest would already
  have been edited, we lock on that one. If none found, we start the Edition of
  a new Record, at the end.
;;
    Mov esi D$ForcedRecordsTable | add esi FORCED_FILE_HEADERLENGHT

    Mov ecx 1

    While D$esi <> 0
        cmp D$esi eax | je L1>
        add esi FORCED_RECORD_LENGHT | inc ecx
    End_While

  ; No matching Record found >>> Return 0
    Mov eax 0

L1: Mov D$DisForcedRecordIndice ecx
ret
____________________________________________________________________________________________
;;
  Job similar to the one already done by the Init of 'InitForcedFlagsDialog' (the caller),
  but done from the Values Recorded in 'ForcedRecordsTable', at the wanted Indice:
;;
SetForcedFlagsFromForcedRecord:
        Call Disable D$ForcedFlagsProcHandle, 51

        Call 'USER32.SetDlgItemInt' D$ForcedFlagsProcHandle, 61,
                                    D$DisForcedRecordIndice, &FALSE

      ; Take the DisImageBase:
        Mov esi D$ForcedRecordsTable
        Mov eax D$esi+FORCED_FILE_IMAGEBASE | Mov D$DisImageBase eax

      ; Point with esi to the proper Record
        Mov ecx D$DisForcedRecordIndice | dec ecx
        Mov eax FORCED_RECORD_LENGHT | mul ecx
        add eax FORCED_FILE_HEADERLENGHT
        add esi eax

      ; Write the Address to the EditBox:
        Mov eax D$esi+FORCED_RECORD_OFFSET1
        push esi
            Mov edi CopyOfLabelHexa | Call WriteEax | Mov B$edi 0
            Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 50, &WM_SETTEXT,
                                              0, CopyOfLabelHexa
        pop esi

      ; Split the Flags onto the wanted Variables:
        Mov eax D$esi+FORCED_RECORD_FLAGS | Call EaxIntoForcedFlags
      ; And set the Radio and CheckBoxes:
        push esi
            Mov eax D$UserSectionFlag | Call InitForcedSectionsDialog
            Mov eax D$UserRoutingFlag | Call InitForcedRoutingDialog
            Mov eax D$UserSizeFlag | Call InitForcedSizeDialog
        pop esi

        Test B$UserSizeFlag STRINGS | jz L2>
            Mov eax D$esi+FORCED_RECORD_OFFSET2

          ; And write it to the EditBox:
            Mov edi CopyOfNextLabelHexa | Call WriteEax | Mov B$edi 0
            Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 51, &WM_SETTEXT,
                                              0, CopyOfNextLabelHexa
L2:
        If D$UserSectionFlag = CODEFLAG
            Call DisableDisDataFlags
        End_If
ret
____________________________________________________________________________________________

[StartOfForcedSection: ? EndOfForcedSection: ?   ForcedFlagsModified: ?]
[ForcedStartAddressBuffer: ? #3][ForcedEndAddressBuffer: ? #3]

[UserSectionFlag: ?  UserRoutingFlag: ?   UserSizeFlag: ?   BadUserFlag: ?]

; The order, from High to low is: Empty/Section/Routing/Size.

ForcedFlagsIntoEax:
    Mov eax D$UserSectionFlag | shl eax 8
    or al B$UserRoutingFlag | shl eax 8
    or al B$UserSizeFlag
ret

EaxIntoForcedFlags:
    Mov B$UserSizeFlag al | shr eax 8
    Mov B$UserRoutingFlag al | shr eax 8
    Mov B$UserSectionFlag al
ret
____________________________________________________________________________________________

GetForcedDialogFlags:
    Mov B$BadUserFlag &FALSE

  ; Get the targetted Address:
    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 50, &WM_GETTEXT,
                                      10, ForcedStartAddressBuffer

    Call GetHexaFromText ForcedStartAddressBuffer

    If B$GetHexaFromTextError = &TRUE

        Call MessageBox argh,
                        {B$ "The Address must be given in Hexa Format" EOS},
                        &MB_SYSTEMMODAL+&MB_USERICON

        Mov B$BadUserFlag &TRUE | ret

    End_If

    Mov D$DisAddressWas eax

    Call GetForcedEndAddress | On eax <> 0, Mov D$NextDisAddressWas eax

  ; Get the SectionsMap Flag:
    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 10, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        Mov B$UserSectionFlag CODEFLAG
    Else
        Mov B$UserSectionFlag DATAFLAG
    End_If

  ; Get the RoutingMap Flags:
  ; [x] Instruction
    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 20, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        Mov B$UserRoutingFlag INSTRUCTION
    Else
        Mov B$UserRoutingFlag 0
    End_If
  ; [x] Label
    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 21, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        or B$UserRoutingFlag LABEL+EVOCATED
    End_If
  ; [x] Exported
    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 22, &BM_GETCHECK, 0, 0
    On eax = &TRUE, or B$UserRoutingFlag EXPORTNODE+NODE+EVOCATED+ACCESSED+LABEL
  ; [x] Pointer
    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 23, &BM_GETCHECK, 0, 0
    On eax = &TRUE, or B$UserRoutingFlag INDIRECT

  ; Get the SizesMap Flags:
    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 30, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        Mov B$UserSizeFlag BYTE | ret
    End_If

    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 31, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        Mov B$UserSizeFlag WORD | ret
    End_If

    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 32, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        Mov B$UserSizeFlag DWORD | ret
    End_If

    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 33, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        Mov B$UserSizeFlag FP4 | ret
    End_If

    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 34, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        Mov B$UserSizeFlag FP8 | ret
    End_If

    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 35, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        Mov B$UserSizeFlag FP10 | ret
    End_If

    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 36, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        Mov B$UserSizeFlag STRINGS+BYTE | ret
    End_If

    Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 37, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        Mov B$UserSizeFlag STRINGS+WORD | ret
    End_If
ret
____________________________________________________________________________________________

RegisterUserFlags:
    Call GetForcedDialogFlags

    If B$BadUserFlag = &FALSE
        Call GetForcedRecordPointer | Mov edi eax

        Mov eax D$DisAddressWas | Mov D$edi+FORCED_RECORD_OFFSET1 eax
        Mov eax D$NextDisAddressWas | Mov D$edi+FORCED_RECORD_OFFSET2 eax

        Call ForcedFlagsIntoEax | Mov D$edi+FORCED_RECORD_FLAGS eax

        push edi
            Call GetForcedEndAddress
        pop edi

        On eax <> 0, Mov D$edi+FORCED_RECORD_OFFSET2 eax
    End_If
ret
____________________________________________________________________________________________

GetForcedEndAddress:
    Call 'USER32.GetDlgItem' D$ForcedFlagsProcHandle, 51
    Call 'USER32.IsWindowEnabled' eax

    .If eax = &TRUE
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 51, &WM_GETTEXT,
                                          10, ForcedStartAddressBuffer
        If eax = 0

            Call MessageBox {B$ "MISSING ADRESS" EOS},
                            {B$ "You must give the End Address of the String (Excluded Label)" EOS},
                            &MB_SYSTEMMODAL+&MB_USERICON

            Mov B$BadUserFlag &TRUE | jmp L2>

        End_If

        Call GetHexaFromText ForcedStartAddressBuffer

        If B$GetHexaFromTextError = &TRUE

            Call MessageBox Argh,
                            {B$ "The address must be given in Hexa format" EOS},
                            &MB_SYSTEMMODAL+&MB_USERICON

            Mov B$BadUserFlag &TRUE | jmp L2>

        End_If
      ; eax = Value of the End Label

    .Else
L2:      Mov eax 0

    .End_If
ret
____________________________________________________________________________________________

[CopyOfEndLabelHexa: ? ? ? ? ? ? ? ? ? ?]

SetForcedEndAddress:
    push esi, edi
        Mov eax D$esi+FORCED_RECORD_OFFSET2, edi CopyOfEndLabelHexa | Call WriteEax
        Call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 51, &WM_SETTEXT,
                                          0, CopyOfEndLabelHexa
    pop edi, esi
ret
____________________________________________________________________________________________

[AddressToBeForced: ?   ForcedFlag: ?
 ForcedUpperLine: ?    ForcedCurrentWritingPos: ?]

ReRunDisassembler:
    Mov B$AddressToBeForced &TRUE

    Call RestoreRealSource

    Mov eax D$UpperLine | sub eax D$CodeSource
    Mov D$ForcedUpperLine eax
    Mov eax D$CurrentWritingPos | sub eax D$CodeSource
    Mov D$ForcedCurrentWritingPos eax

    Call SetPartialEditionFromPos | Call AskForRedrawNow

    Call ReInitUndo | Call ClearBackTable | Mov D$TiTleTable 0

    Call ReleaseMainFile | StoreNameOnly SaveFilter | Call ClearBackTable
    VirtualFree D$UserPeStart

    Mov B$WithForcedMapFile &TRUE | Call ReloadForDissassembler

    Mov eax D$ForcedUpperLine | add eax D$CodeSource
    Mov D$UpperLine eax
    Mov eax D$ForcedCurrentWritingPos | add eax D$CodeSource
    Mov D$CurrentWritingPos eax
    Mov D$PreviousPartialSourceLen 0

    Call SetPartialEditionFromPos
    Call SetCaret D$CurrentWritingPos

    Mov B$BlockInside &FALSE, B$AddressToBeForced &FALSE
ret
____________________________________________________________________________________________
____________________________________________________________________________________________











