TITLE MRU
____________________________________________________________________________________________
____________________________________________________________________________________________

;                            Most Recently Used Files job.
____________________________________________________________________________________________
____________________________________________________________________________________________

[MRU1: B$ ? #260][MRU2: B$ ? #260][MRU3: B$ ? #260][MRU4: B$ ? #260]


; Adds the menu Items in [File] Popup, between [OutPut] and [Exit]:

[FilePopUpHandle: ?    AddedMRUitems: ?]

SetMRUmenu:
    Call DeleteMRUmenu

    If D$MRU1 > 0
        Call 'USER32.InsertMenuA' D$FilePopUpHandle, M00_Exit, &MF_BYCOMMAND, 3001, MRU1
        inc D$AddedMRUitems
    End_If
    If D$MRU2 > 0
        Call 'USER32.InsertMenuA' D$FilePopUpHandle, M00_Exit, &MF_BYCOMMAND, 3002, MRU2
        inc D$AddedMRUitems
    End_If
    If D$MRU3 > 0
        Call 'USER32.InsertMenuA' D$FilePopUpHandle, M00_Exit, &MF_BYCOMMAND, 3003, MRU3
        inc D$AddedMRUitems
    End_If
    If D$MRU4 > 0
        Call 'USER32.InsertMenuA' D$FilePopUpHandle, M00_Exit, &MF_BYCOMMAND, 3004, MRU4
        inc D$AddedMRUitems
    End_If

  ; Separator:
    If D$MRU1 > 0
        Call 'USER32.InsertMenuA' D$FilePopUpHandle, M00_Exit, &MF_BYCOMMAND__&MF_SEPARATOR, 0 0
        inc D$AddedMRUitems
    End_If
ret


DeleteMRUmenu:
    Call 'USER32.GetSubMenu' D$MenuHandle 0 | Mov D$FilePopUpHandle eax
    While D$AddedMRUitems > 0
        Call 'USER32.DeleteMenu' D$FilePopUpHandle, 16, &MF_BYPOSITION
        dec D$AddedMRUitems
    End_While
ret


; Push down (out) one MRU record to make leading room for a new File:

PushOneMRUrecordDown:
    Mov esi MRU3, edi MRU4, ecx,(260*3)
    add esi 259 | add edi 259
    std | rep movsb | cld
ret

;;
 The recently saved file was already in the MRU list, but not in first Pos. We
 reorganise the List. Before calling, 'RecordMRU' Routine set esi to 'DestinationFile'
 and edi to MRU1/2/... We reuse 'DestinationFile', just in case the Name Cases would
 have been changed by user in between. So, at first, we arase the old record (this
 let room in first Pos). I do not use directely 'DestinationFile', but edi, instead,
 in case this function would be extended in the futur.
;;
RePosMRU:
    push esi, edi
      ; First arase the record at edi:
        std
            Mov esi edi | add edi 259 | dec esi
            Mov ecx esi | sub ecx MRU1
            rep movsb
        cld
    pop edi, esi
  ; then write new record at first Pos:
    Mov edi MRU1, ecx 260 | rep movsb
ret

;;
  Adding a new File in the MRU List if it is not already inside, or update the range
  if already there. The file is the last compiled one ('DestinationFile').
;;
RecordMRU:
    Mov esi DestinationFile | Call CheckMRUFile
    Mov ecx DestinationFile | While B$ecx <> 0 | inc ecx | End_While
    sub ecx DestinationFile

    Mov esi DestinationFile
    push esi, ecx
        Mov edi MRU1 | repe cmpsb | je L9>
L1: pop ecx, esi

    push esi, ecx
        Mov edi MRU2 | repe cmpsb | jne L1>
            Mov edi MRU2 | jmp L8>
L1: pop ecx, esi

    push esi, ecx
        Mov edi MRU3 | repe cmpsb | jne L1>
            Mov edi MRU3 | jmp L8>
L1: pop ecx, esi

    push esi, ecx
        Mov edi MRU4 | repe cmpsb | jne L1>
            Mov edi MRU4 | jmp L8>
  ; If here, it is a new file name:
L1: pop ecx, esi

    Call PushOneMRUrecordDown
    Mov esi DestinationFile, edi MRU1, ecx 260 | rep movsb | Call SetMRUmenu | ret

  ; If here, the File is already Listed, but not at first Pos:
L8: pop ecx, esi
    Call RePosMRU | Call SetMRUmenu | ret

  ; If here, the File is already Listed at first Pos >>> OK, exit:
L9: pop ecx, esi | ret


; Arase one record if the File do not exist (example, if user deleted the file):

[CheckMRUpointer: ?]

CheckMRUFile:
    Mov D$CheckMRUpointer esi
    Call 'KERNEL32.FindFirstFileA' esi, FindFile
    push eax

        .If eax = &INVALID_HANDLE_VALUE
            Mov eax 0, esi D$CheckMRUpointer, edi esi | add esi 260
            Mov ecx MRU4 | sub ecx edi
            jecxz L1>
                rep movsb
L1:         Mov ecx 260, al 0 | rep stosb

        .Else                                           ; All this is only to retrieve
            Mov edi D$CheckMRUpointer                   ; the Name case (upper/lower),
            Mov al 0, ecx 0-1 | repne scasb | dec edi   ; in case the user modified it
            Mov esi FindFile.cFileName, ecx 0           ; since last session.
            While B$esi <> 0
                inc ecx | inc esi
            End_While
            sub edi ecx
            Mov esi FindFile.cFileName | rep movsb

        .End_If

    pop eax | On eax <> &INVALID_HANDLE_VALUE, Call 'KERNEL32.FindClose' eax
ret


CheckAllMRUFile:
    Mov esi MRU1 | Call CheckMRUFile
    Mov esi MRU2 | Call CheckMRUFile
    Mov esi MRU3 | Call CheckMRUFile
    Mov esi MRU4 | Call CheckMRUFile
ret


[MRUfileHasBeenDeleted: 'This File has been deleted or renamed Since last MRU update', 0]

[SelectedMRUFileName: B$ ? #260]

; Called from Main Message Loop with Menu ID in eax.

LoadRMUfile:
    push eax
        If eax = 3001
            Mov esi MRU1
        Else_If eax = 3002
            Mov esi MRU2
        Else_If eax = 3003
            Mov esi MRU3
        Else_If eax = 3004
            Mov esi MRU4
        End_If

        Mov edi SelectedMRUFileName, ecx 260 | rep movsb

        Call CheckAllMRUFile | Call SetMRUmenu
        Call ReInitUndo
    pop eax

    If eax = 3001
        Mov esi MRU1
    Else_If eax = 3002
        Mov esi MRU2
    Else_If eax = 3003
        Mov esi MRU3
    Else_If eax = 3004
        Mov esi MRU4
    End_If

    push esi
        Mov edi SelectedMRUFileName, ecx 260 | rep cmpsb | je L1>
            Mov eax MRUfileHasBeenDeleted | Call MessageBox | pop esi | Mov eax &FALSE | ret
L1: pop esi

    Mov edi SaveFilter, ecx 260 | rep movsb

    Mov edi SaveFilter
    While B$edi <> 0 | inc edi | End_While
    While B$edi <> '\' | dec edi | End_While

    push D$edi, edi
        Mov B$edi 0
        Call 'KERNEL32.SetCurrentDirectoryA' SaveFilter  ; says TRUE.
    pop edi, D$edi

    If eax = &TRUE
        Call DirectMRUload
        Call UpdateTitlesFromIncludeFiles
        Call AskForRedraw
        Mov eax &TRUE
    End_If
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
; Loading the last MRU File on Startup, if 'LoadMRU' Flag is set On.

[LoadMRU: ?]

LoadLastMRUFile:
    .If D$MRU1 <> 0
        StoreNameOnly MRU1

        Mov esi MRU1, edi SaveFilter
        While B$esi <> 0 | movsb | End_While

        Call LastMRULoading

        If D$SourceLen > 0
            Call SetPartialEditionFromPos | Call EnableMenutems
            Call LoadBookMarks
        End_If

    .End_If
ret
;;
____________________________________________________________________________________________
____________________________________________________________________________________________


  ; Loading the last MRU File on Startup, if 'LoadMRU' Flag is set On.
; RWE1 start of drag and drop modification
[LoadMRU: ?]

LoadLastMRUFile:
    Call ProcessCL
  ; eax returned pointing to command line filename in CLbuffer:
  ;showme eax
    ..If eax = 0
        .If D$MRU1 <> 0 ; Do this if no command line parameters i.e. no drag and drop file
            StoreNameOnly MRU1 ; Do this if 'autoreload last file on start up' selected

            Mov esi MRU1, edi SaveFilter
            While B$esi <> 0 | movsb | End_While ; string MRU1 saved to Savefilter

            Call LastMRULoading

            If D$SourceLen > 0 ; If the file includes assembly language source
                Call SetPartialEditionFromPos | Call EnableMenutems
                Call LoadBookMarks
            End_If

        .End_If
    ..Else
        StoreNameOnly CLBuffer ; Do this if e.g. a file has been drag / dropped onto RosASM icon

        Mov esi CLBuffer, edi SaveFilter
        While B$esi <> 0 | movsb | End_While ; string MRU1 saved to Savefilter

        Call LastMRULoading

        If D$SourceLen > 0 ; If the file includes assembly language source
            Call SetPartialEditionFromPos | Call EnableMenutems
            Call LoadBookMarks
        End_If
    ..End_If
ret

______________________________________________________________________________________________________________________________________________

[CLBuffer: B$ 0 #&MAXPATH] ; Stores any filename drag and dropped to RosASM icon on desktop

ProcessCL:
    Call 'Kernel32.GetCommandLineA'
    Mov edx eax, edi eax, ecx 0FF, al 0 | repne scasb
    Mov esi edi, edi CLBuffer

    .While esi > edx
        dec esi
        .If B$esi = '"'
            dec esi
            While B$esi <> '"' | dec esi | End_While
            Mov eax edi
            If esi <> edx
                inc esi | While B$esi <> '"' | movsb | End_While
            Else
                Mov eax 0
            End_If
            Mov B$edi 0 | ret

        .Else_If B$esi = '.'
            While B$esi <> ':' | dec esi | End_While
            dec esi | Mov eax edi
            While B$esi <> 0 | movsb | End_While
            Mov B$edi 0 | ret

        .End_If
    .End_While

    Mov eax 0
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[ActualDir: B$ ? #&MAXPATH] [ActualDirPointer: ?]
[DllScannerDir: B$ ? #&MAXPATH]

Proc GetDirectory:
    Argument @Dir

        Call 'KERNEL32.GetCurrentDirectoryA' &MAXPATH, D@Dir
EndP


Proc SetDirectory:
    Argument @Dir

        Mov eax D@Dir | On D$eax <> 0, Call 'KERNEL32.SetCurrentDirectoryA' D@Dir
EndP




