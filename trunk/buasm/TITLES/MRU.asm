TITLE MRU             ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;                            Most Recently Used Files job.
____________________________________________________________________________________________
____________________________________________________________________________________________

[STR.A.MRU1: B$ ? # &MAX_PATH]
[STR.A.MRU2: B$ ? # &MAX_PATH]
[STR.A.MRU3: B$ ? # &MAX_PATH]
[STR.A.MRU4: B$ ? # &MAX_PATH]

[MRU1   3001
 MRU2   3002
 MRU3   3003
 MRU4   3004]

; Adds the menu Items in [File] Popup, between [OutPut] and [Exit]:

[H.FilePopUp: D$ ?
 AddedMRUitems: D$ ?]

SetMRUmenu:

    Call DeleteMRUmenu

    Test D$STR.A.MRU1 NA ZERO S1>

        Call 'USER32.InsertMenuA' D$H.FilePopUp,
                                  M00_Exit,
                                  &MF_BYCOMMAND,
                                  MRU1,
                                  STR.A.MRU1
        add D$AddedMRUitems 1

S1: Test D$STR.A.MRU2 NA ZERO S1>

        Call 'USER32.InsertMenuA' D$H.FilePopUp,
                                  M00_Exit,
                                  &MF_BYCOMMAND,
                                  MRU2,
                                  STR.A.MRU2

        add D$AddedMRUitems 1

S1: Test D$STR.A.MRU3 NA ZERO S1>

        Call 'USER32.InsertMenuA' D$H.FilePopUp,
                                  M00_Exit,
                                  &MF_BYCOMMAND,
                                  MRU3,
                                  STR.A.MRU3

        add D$AddedMRUitems 1

S1: Test D$STR.A.MRU4 NA ZERO S1>

        Call 'USER32.InsertMenuA' D$H.FilePopUp,
                                  M00_Exit,
                                  &MF_BYCOMMAND,
                                  MRU4,
                                  STR.A.MRU4

        add D$AddedMRUitems 1

  ; Separator:
S1: Test D$STR.A.MRU1 NA ZERO S0>

        Call 'USER32.InsertMenuA' D$H.FilePopUp,
                                  M00_Exit,
                                  &MF_BYCOMMAND+&MF_SEPARATOR,
                                  &NULL,
                                  &NULL

        add D$AddedMRUitems 1

S0:
ret


DeleteMRUmenu:

    Call 'USER32.GetSubMenu' D$H.MenuMain,
                             0

    Mov D$H.FilePopUp eax

    While D$AddedMRUitems > 0

        Call 'USER32.DeleteMenu' D$H.FilePopUp,
                                 16,
                                 &MF_BYPOSITION

        sub D$AddedMRUitems 1

    End_While

ret


; Push down (out) one MRU record to make leading room for a new File:

PushOneMRUrecordDown:

    Mov esi STR.A.MRU3,
        edi STR.A.MRU4,
        ecx (&MAX_PATH*3)

    add esi (&MAX_PATH-1) | add edi (&MAX_PATH-1)

    std | rep movsb | cld

ret

;;
 
    The recently saved file was already in the MRU list, but not in first Pos. We
    reorganise the List. Before calling, 'RecordMRU' Routine set esi to 'DestinationFile'
    and edi to STR.A.MRU1/2/... We reuse 'DestinationFile', just in case the Name Cases would
    have been changed by user in between. So, at first, we arase the old record (this
    let room in first Pos). I do not use directely 'DestinationFile', but edi, instead,
    in case this function would be extended in the futur

;;
RePosMRU:

    Push esi,
         edi

        ; First arase the record at edi:
        std

            Mov esi edi | add edi (&MAX_PATH-1) | dec esi

            Mov ecx esi | sub ecx STR.A.MRU1

            rep movsb

        cld

    Pop edi,
        esi

    ; then write new record at first Pos:
    Mov edi STR.A.MRU1,
            ecx (&MAX_PATH/DWORD)

    rep movsd

ret

;;

    Adding a new File in the MRU List if it is not already inside, or update the range
    if already there. The file is the last compiled one ('DestinationFile')

;;
RecordMRU:

    Mov esi DestinationFile | Call CheckMRUFile

    Mov ecx DestinationFile | While B$ecx <> EOS | add ecx ASCII | End_While

    sub ecx DestinationFile

    Mov esi DestinationFile

    Push esi,
         ecx

        Mov edi STR.A.MRU1 | repe cmpsb | je S0>

S1: Pop ecx,
        esi

    Push esi,
         ecx

        Mov edi STR.A.MRU2 | repe cmpsb | jne S1>

        Mov edi STR.A.MRU2 | jmp S2>

S1: Pop ecx,
        esi

    Push esi,
         ecx

        Mov edi STR.A.MRU3 | repe cmpsb | jne S1>

        Mov edi STR.A.MRU3 | jmp S2>

S1: Pop ecx,
        esi

    Push esi,
         ecx

        Mov edi STR.A.MRU4 | repe cmpsb | jne S1>

        Mov edi STR.A.MRU4 | jmp S2>

    ; If here, it is a new file name:
S1: Pop ecx,
        esi

    Call PushOneMRUrecordDown

    Mov esi DestinationFile,
        edi STR.A.MRU1, ecx 260

    rep movsb | Call SetMRUmenu

ret

    ; If here, the File is already Listed, but not at first Pos:
S2: Pop ecx,
        esi

    Call RePosMRU | Call SetMRUmenu

ret

    ; If here, the File is already Listed at first Pos >>> OK, exit:
S0: Pop ecx, esi

ret


; Arase one record if the File do not exist (example, if user deleted the file):

[CheckMRUpointer: D$ ?]

CheckMRUFile:

    Mov D$CheckMRUpointer esi

    Call 'KERNEL32.FindFirstFileA' esi,
                                   STRUC.FindFile

    Push eax

        If eax = &INVALID_HANDLE_VALUE

            Mov eax 0,
                esi D$CheckMRUpointer,
                edi esi

            add esi &MAX_PATH

            Mov ecx STR.A.MRU4 | sub ecx edi

            jecxz S1>

                rep movsb

S1:         Mov ecx (&MAX_PATH/DWORD)

            rep stosd

        Else                                           ; All this is only to retrieve

            Mov edi D$CheckMRUpointer                   ; the Name case (upper/lower),

            Mov al 0,
                ecx NA

            repne scasb | sub edi ASCII  ; in case the user modified it

            Mov esi STRUC.FindFile@cFileName,
                ecx 0           ; since last session.

            While B$esi <> EOS

                add ecx 1 | add esi ASCII

            End_While

            sub edi ecx

            Mov esi STRUC.FindFile@cFileName | rep movsb

        End_If

    Pop eax

    On eax <> &INVALID_HANDLE_VALUE Call 'KERNEL32.FindClose' eax

ret


CheckAllMRUFile:

    Mov esi STR.A.MRU1 | Call CheckMRUFile

    Mov esi STR.A.MRU2 | Call CheckMRUFile

    Mov esi STR.A.MRU3 | Call CheckMRUFile

    Mov esi STR.A.MRU4 | Call CheckMRUFile

ret


[MRUfileHasBeenDeleted: B$ 'This file has been deleted or renamed' EOS]

[SelectedMRUFileName: B$ ? # &MAX_PATH]

; Called from Main Message Loop with Menu ID in eax.

LoadRMUfile:

    Push eax

        If eax = MRU1

            Mov esi STR.A.MRU1

        Else_If eax = MRU2

            Mov esi STR.A.MRU2

        Else_If eax = MRU3

            Mov esi STR.A.MRU3

        Else_If eax = MRU4

            Mov esi STR.A.MRU4

        End_If

        Mov edi SelectedMRUFileName,
            ecx (&MAX_PATH/DWORD)

        rep movsd

        Call CheckAllMRUFile | Call SetMRUmenu

        Call ReInitUndo

    Pop eax

    If eax = MRU1

        Mov esi STR.A.MRU1

    Else_If eax = MRU2

        Mov esi STR.A.MRU2

    Else_If eax = MRU3

        Mov esi STR.A.MRU3

    Else_If eax = MRU4

        Mov esi STR.A.MRU4

    End_If

    Push esi

        Mov edi SelectedMRUFileName,
            ecx (&MAX_PATH/DWORD)

        rep cmpsd | jmpIf = S1>

    Pop esi

    Call MessageBox MRUfileHasBeenDeleted

    Mov eax &FALSE

ret

S1: Pop esi

    Mov edi SaveFilter,
        ecx (&MAX_PATH/DWORD)

    rep movsd

    Mov edi SaveFilter

    While B$edi <> 0 | add edi ASCII | End_While

    While B$edi <> '\' | SUB edi ASCII | End_While

    Push D$edi,
         edi

        Mov B$edi EOS

        Call SetDirectory SaveFilter  ; says TRUE.

    Pop edi,
        D$edi

    Test eax &TRUE FALSE S0>

        Call DirectLoad

        Call UpdateTitlesFromIncludeFiles

        Call AskForRedraw

        Mov eax &TRUE

S0:
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
; Loading the last MRU File on Startup, if 'LoadMRU' Flag is set On.

[LoadMRU: D$ ?]

LoadLastMRUFile:
    .If D$STR.A.MRU1 <> 0
        StoreNameOnly STR.A.MRU1

        Mov esi STR.A.MRU1, edi SaveFilter
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
[LoadMRU: D$ ?]

LoadLastMRUFile:

    Call ProcessCL

    ; eax returned pointing to command line filename in CLbuffer:
    ; showme eax

    Test eax NA NOT_ZERO S1>

        Test D$STR.A.MRU1 NA ZERO S0> ; Do this if no command line parameters i.e. no drag and drop file

            Call StoreChoosenName STR.A.MRU1 ; Do this if 'autoreload last file on start up' selected

            Mov esi STR.A.MRU1,
                edi SaveFilter

        jmp S2>

S1: Call StoreChoosenName CLBuffer ; Do this if e.g. a file has been drag / dropped onto RosASM icon

    Mov esi CLBuffer,
        edi SaveFilter

S2: While B$esi <> EOS | movsb | End_While ; string MRU1 saved to Savefilter

    Call LastLoading

    Test D$SourceLen NA ZERO S0> ; If the file includes assembly language source

        Call SetPartialEditionFromPos | Call EnableMenutems

        Call LoadBookMarks

S0:
ret
______________________________________________________________________________________________________________________________________________

[CLBuffer : B$ ? # &MAX_PATH] ; Stores any filename drag and dropped to RosASM icon on desktop

ProcessCL:

    Call 'KERNEL32.GetCommandLineA'

    Mov edx eax,
        edi eax,
        ecx 0FF,
        al EOS

    repne scasb

    Mov esi edi,
        edi CLBuffer

L1: Comp esi edx <= S0>

        sub esi ASCII

        Comp B$esi '"' <> S2>

            sub esi ASCII | While B$esi <> '"' | sub esi ASCII | End_While

            Mov eax edi

            If esi <> edx

                add esi ASCII | While B$esi <> '"' | movsb | End_While

            Else

                Mov eax &NULL

            End_If

            Mov B$edi EOS

ret

S2:     Comp B$esi '.' <> L1<

            While B$esi <> ':' | sub esi ASCII | End_While

            Mov eax edi

            sub esi ASCII | While B$esi <> EOS | movsb | End_While

            Mov B$edi EOS

     jmp L1<

S0:  Mov eax &NULL

ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[ActualDir: B$ ? # &MAX_PATH]

[ActualDirPointer: D$ ?]

Proc GetDirectory:

    Argument @STR.A.Dir

    pushad

        Call 'KERNEL32.GetCurrentDirectoryA' &MAX_PATH,
                                             D@STR.A.Dir

    popad

EndP


Proc SetDirectory:

    Argument @STR.A.Dir

    pushad

        On D@STR.A.Dir <> 0 Call 'KERNEL32.SetCurrentDirectoryA' D@STR.A.Dir

    popad

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
