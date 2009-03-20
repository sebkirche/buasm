TITLE Includes        ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________
;;

'BuildEquatesTables' 'GetEquates' 
 
 
Win Equates: How it works.

The Equates.equ Files expected by the Parsers is in the form of:

A0_REG 010
A1_REG 011
A2_REG 012
A3_REG 013
A4_REG 014
A5_REG 015
AADBE_ADD_ENTRY 01
AADBE_DEL_ENTRY 02
... and so on.

That is:

* Zero blank Line.
* One line is: NAME / Space / HEXA / CR-LF
* File CR-LF ended.

Zero flexibility. Simply have 2 spaces instead of one, or a space after the Hexa, will
make it fail.

At launch time, RosAsm attempts to open "Equates.equ". If not found, it sends a 
Message telling the user to provide the path for that file.

After having open "Equates.equ", RosAsm also opens all other '*.equ" Files encounted
aside "Equates.equ", loads all of these Data and compiles them.

Compiling the Win Equates:

The Equates Names and Values are stored into two different parallel Tables, one for
the Names, one for the Values. The names are computed into encoded dWords (a kind
of CheckSum), so that the two Tables are the same size.


The NamesTable begins by a Header of dWords Pointers: In fact, not only the Equates
are encoded, but they also are divided in 'First-Char's Chunks and each Chunk is sorted
in numerical order. When searching for an Equate Value, RosAsm first re-encodes the
Name into the CheckSum, condiders the First Equate Char, read the according Pointer
in the NameTable Header (and also the next Pointer to get the size by substraction),
and begins searching in the proper Chunk only. This final search, for recovering a
Value from a given Name, is done by a fast 2n Search Algo.

____________________________________________________________________________________________
____________________________________________________________________________________________


 Reading the "Include" Files: At least: Equates.equ / Functions.api / Structures.str,
 which are the required ones for Win32. RosAsm does not run without ( > Run Help File
 if not found / Load them if found).

 All other .equ Files found in the same Directory add pasted to the same Memory Tables
 for Equates.

 Functions.api is a single File (Right-Click / Disassembly).

 .str Files will be available trough [Struc] main menu option. If several .str Files
 are found > build a child menu with an Item for each, and branch the sub Menu to
 [Struc].
;;
____________________________________________________________________________________________

[IncludeFileName: B$ ? # &MAX_PATH]

; Called with '.ext' in eax:

Proc SetAllIncludeFilesExtension:
    Argument @Extension

    Mov esi EquatesName, edi IncludeFileName

    While B$esi <> 0 | movsb | End_While
    dec edi
    While B$edi <> '.' | dec edi | End_While

L0: dec edi | cmp B$edi '\' | je L1>
              cmp B$edi ':' | je L1>
              cmp edi IncludeFileName | ja L0<
                jmp L2>
L1: inc edi
L2: Mov B$edi '*' | inc edi | Mov eax D@Extension | stosd | Mov B$edi 0
EndP

____________________________________________________________________________________________

[StructuresIncFileSize: D$ ?
 H.StructPopUp: D$ ?
 StructuresFileOK: D$ ?]

[StrucPopMenu: B$ 'Struct' EOS]

[StructuresItem: B$ 'Structures' EOS]

PrepareStructuresFiles:
    Call SetAllIncludeFilesExtension '.str'
    Call 'KERNEL32.FindFirstFileA' IncludeFileName FIND_EQU | Call SetFullName

    ..If eax = &INVALID_HANDLE_VALUE
        Mov B$StructuresFileOK &FALSE
        ret
      ; Better let it run without Structures and Api Files if user wants to...
      ; May be temporary...

        Call Help, B_U_AsmName, IncludeFilesHelp, ContextHlpMessage

        Mov D$FL.Includes &FALSE

    ..Else  ; 'AddUserMenu'
        Mov D$H.FindInc eax
      ; Copy first the first File Path and Name in case there is only one:
        Mov B$StructuresFileOK &TRUE
        Mov esi FullName, edi MenuItemString, ecx (&MAX_PATH/DWORD) | rep movsd

      ; Is there more than one File.str?
        Call 'KERNEL32.FindNextFileA' D$H.FindInc FIND_EQU

;Mov eax &FALSE ; <<<<<<<<<<<<<<<< Temporary... (ToDo List...).

        .If eax = &TRUE
            Call 'KERNEL32.FindClose' D$H.FindInc

            Mov D$StructureMenuID 4000, B$SeveralStructuresFiles &TRUE

            Call 'USER32.CreatePopupMenu' | Mov D$H.StructPopUp eax

            Call 'KERNEL32.FindFirstFileA' IncludeFileName FIND_EQU
            Mov D$H.FindInc eax

            Do
                Call SetStructuresMenuItem
                Call 'KERNEL32.FindNextFileA' D$H.FindInc FIND_EQU
            Loop_Until eax = &FALSE



            Call 'USER32.InsertMenuA' D$H.MenuMain, M00_Structures,
                                  &MF_BYCOMMAND__&MF_POPUP__&MF_STRING,
                                  D$H.StructPopUp, StructuresItem

            Call 'USER32.DeleteMenu' D$H.MenuMain, M00_Structures, &MF_BYCOMMAND


          ;  Call 'USER32.DeleteMenu' D$H.MenuMain 8 &MF_BYPOSITION
          ;  Call 'USER32.InsertMenuA' D$H.MenuMain 8 &MF_BYPOSITION__&MF_STRING__&MF_POPUP,
          ;                    D$H.StructPopUp  StrucPopMenu
        .Else
          ; Else, there is only one .str File.
            Mov B$SeveralStructuresFiles &FALSE

        .End_If

        Call 'KERNEL32.FindClose' D$H.FindInc

    ..End_If
ret

[StructureMenuID: D$ ?]

; Builds the added PopUp Menu under [Struct] main Option:
; (We have a: > Mov D$StructureMenuID 4000 in caller ('OpenStructuresFiles').

SetStructuresMenuItem:
    Mov esi FIND_EQU_cFileName
    While B$esi <> 0 | inc esi | End_While
    dec esi
    While B$esi <> '.' | dec esi | End_While
    Mov B$esi 0
L0: dec esi | cmp B$esi '\' | je L1>
              cmp B$esi ':' | je L1>
              cmp esi FIND_EQU_cFileName | ja L0<

L1: Call 'USER32.AppendMenuA' D$H.StructPopUp &MF_STRING D$StructureMenuID esi
    inc D$StructureMenuID
ret

 ________________________________________________________________________________________
 _________________________________________________________________________________________
;;
 15948 Win Equates are stored in 2 tables of dWords: The first one for the Names
 (stored as one dWord per name after string coding) and the second one for the
 relative Values. The routine for coding the names i used when building these tables
 is the same as the one use here for the Win Equates Search (see down there).

 The storage (and search) algorithm is NOT univoque. It simply revealed efficient
 upon this list (without any double values -same value for 2 names- encounted).

 The Data for OS Equates are now outside RosAsm, and computed at launch time.
;;
____________________________________________________________________________________________





[EquatesCurrentDirectory: B$ ? # &MAX_PATH]

;;
  In case user did not re-define the 'Equates.equ' Path, but set the 'Equates.equ' aside
  RosAsm, if he changes the Current Directory when working, and then run, for example [Struct]
  Menu Item, RosAsm would hang when expecting to find out 'Structures.str' in the new Current
  Path. So, we complete the Path here, if needed.
;;

AppendToCurrentDirectory:
    Call GetDirectory EquatesCurrentDirectory
    If eax <> 0
        Mov edi EquatesCurrentDirectory | add edi eax
        Mov al '\'
        On B$edi-1 <> al, stosb
        Mov esi EquatesName | While B$esi <> 0 | movsb | End_While

        Mov esi EquatesCurrentDirectory, edi EquatesName
        While B$esi <> 0 | movsb | End_While
    End_If
ret
____________________________________________________________________________________________

[EquatesInFileTitle: B$ 'Number of Encoded Equates:' EOS]

[EquatesIncFileSize: D$ ?
 EquateIncMemory: D$ ?
 EquateIncMemoryPointer: D$ ?]

[H.FindInc: D$ ?]

[FIND_EQU:
 FIND_EQU_dwFileAttributes: D$ ?
 FIND_EQU_ftCreationTime.dwLowDateTime: D$ ?
 FIND_EQU_ftCreationTime.dwHighDateTime: D$ ?
 FIND_EQU_ftLastAccessTime.dwLowDateTime: D$ ?
 FIND_EQU_ftLastAccessTime.dwHighDateTime: D$ ?
 FIND_EQU_ftLastWriteTime.dwLowDateTime: D$ ?
 FIND_EQU_ftLastWriteTime.dwHighDateTime: D$ ?
 FIND_EQU_nFileSizeHigh: D$ ?
 FIND_EQU_nFileSizeLow: D$ ?
 FIND_EQU_dwReserved0: D$ ?
 FIND_EQU_dwReserved1: D$ ?]
[FIND_EQU_cFileName: B$ ? # &MAX_PATH]
[FIND_EQU_cAlternate: B$ ? # 14]

[FullName: B$ ? # &MAX_PATH]

SetFullName:
    pushad
        Mov esi IncludeFileName, edi FullName
        While W$esi <> '*.' | movsb | End_While
        Mov esi FIND_EQU_cFileName
        While B$esi <> EOS | movsb | End_While | movsb
    popad
ret
____________________________________________________________________________________________

OpenEquFiles:
    Call IsEquatesEquThere

    If D$FL.Includes = &TRUE
        Call GetEquFilesMemory
        Call ReadEquatesEqu
        Call ReadOtherEquFiles
        Call CleanEquateIncMemory
    End_If
ret


[EquatesEquFileName: B$ 'Equates.equ' EOS]

SetEquatesEquFileName:
    Mov esi EquatesName, edi IncludeFileName

    While B$esi <> 0 | movsb | End_While
    dec edi
    While B$edi <> '.' | dec edi | End_While

L0: dec edi | cmp B$edi '\' | je L1>
              cmp B$edi ':' | je L1>
              cmp edi IncludeFileName | ja L0<
                jmp L2>
L1: inc edi
L2: Mov esi EquatesEquFileName
    While B$esi <> 0 | movsb | End_While | movsb
ret


IsEquatesEquThere:
    Call SetEquatesEquFileName

    Call 'KERNEL32.FindFirstFileA' IncludeFileName FIND_EQU

    .If eax = &INVALID_HANDLE_VALUE
        Mov D$FL.Includes &FALSE
        Call Help B_U_AsmName, IncludeFilesHelp, BUAsmHlpMessage

    .Else
        Mov D$FL.Includes &TRUE
    .End_If
ret


GetEquFilesMemory:
    Call SetAllIncludeFilesExtension '.equ'

    Call 'KERNEL32.FindFirstFileA' IncludeFileName FIND_EQU | Call SetFullName

    Mov D$H.FindInc eax, D$EquatesIncFileSize 0

L0:     Mov eax D$FIND_EQU_nFileSizeLow | add D$EquatesIncFileSize eax

        Call 'KERNEL32.FindNextFileA' D$H.FindInc FIND_EQU
        Call SetFullName | On eax = &TRUE, jmp L0<

    Call VirtualAlloc EquateIncMemory,
                      D$EquatesIncFileSize

L9: Call 'KERNEL32.FindClose' D$H.FindInc
ret


ReadEquatesEqu:
    Call SetEquatesEquFileName
    Call 'KERNEL32.CreateFileA' IncludeFileName &GENERIC_READ, &FILE_SHARE_READ, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0


    Push eax
        Push eax
            Call 'KERNEL32.GetFileSize' eax, 0 | Mov ecx eax
            If ecx = 0
                Pop eax | jmp L9>
            End_If
            add eax D$EquateIncMemory | Mov D$EquateIncMemoryPointer eax
        Pop eax
        Call 'KERNEL32.ReadFile' eax, D$EquateIncMemory, ecx, NumberOfReadBytes, 0
    Pop eax

L9: Call 'KERNEL32.CloseHandle' eax
ret


IsItEquatesEqu:
    Mov esi FIND_EQU_cFileName, edi EquatesEquFileName
L0: Mov al B$esi, bl B$edi | inc edi | inc esi
    If al = 0
        cmp bl 0
    Else
        or al SPC | or bl SPC | cmp al bl | je L0<
    End_If
ret


ReadOtherEquFiles:
    Call SetAllIncludeFilesExtension '.equ'

    Call 'KERNEL32.FindFirstFileA' IncludeFileName FIND_EQU | Call SetFullName

        Push 0-1

        Mov D$H.FindInc eax

L0:     Call IsItEquatesEqu | je L1>
        Mov eax D$FIND_EQU_nFileSizeLow
        Push eax
            Call 'KERNEL32.CreateFileA' FullName &GENERIC_READ, &FILE_SHARE_READ, 0,
                                        &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
        Push eax

L1:     Call 'KERNEL32.FindNextFileA' D$H.FindInc FIND_EQU
        Call SetFullName | On eax = &TRUE, jmp L0<


        Mov edi D$EquateIncMemoryPointer
L0:     Pop eax                     ; Handle
        On eax = 0-1, jmp L9>
        Pop ecx                     ; Size
        Push edi, ecx, eax
            Call 'KERNEL32.ReadFile' eax, edi, ecx, NumberOfReadBytes, 0
        Pop eax, ecx, edi

        add edi ecx

        On W$edi-2 <> 0A0D, jmp BadEquatesFileEnd
        On W$edi-4 = 0A0D, jmp BadEquatesFileEnd

        Push edi | Call 'KERNEL32.CloseHandle' eax | Pop edi | jmp L0<

L9:     Call 'KERNEL32.FindClose' D$H.FindInc
ret


CleanEquateIncMemory:
    Mov esi D$EquateIncMemory, edi esi, edx esi | add edx D$EquatesIncFileSize

    While esi < edx
        On B$esi = TAB Mov B$esi SPC
        inc esi
    End_While

    Mov esi edi

    .While esi < edx
        lodsb | stosb
        If al = SPC
            While B$esi = SPC
                inc esi | dec D$EquatesIncFileSize
            End_While
        End_If
    .End_While
ret

____________________________________________________________________________________________

[NumberOfEquates: D$ ?]

; In factn ot only 'Count Equates', but also verify integrity of File syntax:
; One Symbol UpperCase / One space / One Hexa in RosAsm syntax / One CR/LF.

CountEquates:
    Mov edx D$EquateIncMemory | add edx D$EquatesIncFileSize

    On W$edx-2 <> 0A0D, jmp BadEquatesFileEnd
    On W$edx-4 = 0A0D, jmp BadEquatesFileEnd

    Mov esi D$EquateIncMemory, ecx 0

    .While esi < edx
        Mov ebx esi

        While B$esi > SPC | inc esi | End_While         ; Read Symbol.

        On B$esi <> SPC, jmp BadEquatesFile             ; One single space.

        inc esi | On B$esi <> '0', jmp BadEquatesFile

        While B$esi > SPC | inc esi | End_While         ; One RosAsm syntax Hexa number.

        On W$esi <> 0A0D, jmp BadEquatesFile  ; CR/LF

        add esi 2 | inc ecx
    .End_While

    Mov D$NumberOfEquates ecx
ret

____________________________________________________________________________________________

[BadEquatesFileTitle: B$ 'Bad Equates File' EOS]
[BadEquatesFileMessage: D$ ? # 10]
[BadEquatesFileEndMessage: B$ 'The Equates File must be ended by *one* CR/FL' EOS]

BadEquatesFile:
    Mov esi ebx, edi BadEquatesFileMessage
    While B$esi > SPC | movsb | End_While
    Mov D$edi '...', B$edi+4 0


    Call 'USER32.MessageBoxA' 0, BadEquatesFileMessage, BadEquatesFileTitle, 0
    Call 'KERNEL32.ExitProcess' 0


BadEquatesFileEnd:

    Call 'USER32.MessageBoxA' 0, BadEquatesFileEndMessage, BadEquatesFileTitle, 0
    Call 'KERNEL32.ExitProcess' 0


[EquatesNumber: D$ ?
 WinEquTableLenght: D$ ?
 NamesTable: D$ ?
 ValuesTable: D$ ?
 SortedNamesTable: D$ ?
 SortedValuesTable: D$ ?]

[SizeByFirstChar: D$ ? # 256] ; !!! -> ? -> 26 Length (Length for Equates beginning by 'A, by 'B',... 'Z'.
; plus '[/]^_', just to have '_' >>> 31 records.
[ApiChunksPointers: D$ ? # 256] ; Used to follow up when filling the Api List Chunks.
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
