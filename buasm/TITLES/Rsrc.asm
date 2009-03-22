TITLE Rsrc            ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;;
____________________________________________________________________________________________
____________________________________________________________________________________________

       Resources with customized Types and Names and Languages support
       Version 1.0
____________________________________________________________________________________________
____________________________________________________________________________________________
 
 Author name: Diego Fraga
 
 Email: < diegfrag@yahoo.com >
 
 Started: September 2004
 
____________________________________________________________________________________________

;;

; CustomList > Type/Name/Lang/Pointer/Size

[CustomList: D$ ? # MAXRESOURCE]

[RsrcType: D$ ?
 RsrcTypeStringLen: D$ ?
 RsrcTypeString: B$ ? # 33]
[RsrcNameStringLen: D$ ?
 RsrcNameString: B$ ? # 33]

[AviTypeStrLen: D$ 4
 AviTypeStr: B$ 'AVI' EOS]

[WaveTypeStrLen: D$ 5
 WaveTypeStr: B$ 'WAVE' EOS]

ClearCustomList:
    Mov edi CustomList, eax 0, ecx MAXRESOURCE | rep stosd
ret

;;

[CustomList:
 CustomList.Type: D$ 0
 CustomList.Name: D$ 0
 CustomList.Lang: D$ 0
 CustomList.Pointer: D$ 0
 CustomList.Size: D$ 0]
;;

[CustomList.TypeDis 0
 CustomList.NameDis 4
 CustomList.LangDis 8
 CustomList.PointerDis 12
 CustomList.SizeDis 16]

[Size_Of_CustomList 20]

Proc ReadRosAsmResources:

    pushad
        Call ClearCustomList
        Mov D$RsrcType 0
        Call FillCustomListFromResourceTree D$UserPEStartOfResources, CustomList
        Call CopyStandarTypeResources
    popad

EndP

Proc FillCustomListFromResourceTree: ; 'ResourcesStub' For infos.
    Argument @Pointer, @Output
    Local @Array
    Uses esi, ebx, eax

    Mov esi D@Pointer, eax 0, edi D@Output

    Push esi
      ; add ImgResDir.NumberOfNamedEntries to ImgResDir.NumberOfIdEntries
      ; and copy the result to N:
        add esi ImgResDir.NumberOfNamedEntriesDis
        lodsw | Mov D@Array eax
        lodsw | add D@Array eax
    Pop esi

    ; esi and ebx now points to the IMAGE_RESOURCE_DIRECTORY_ENTRY.
    add esi Size_Of_IMAGE_RESOURCE_DIRECTORY
    add ebx Size_Of_IMAGE_RESOURCE_DIRECTORY

          ; We need to see if we have a Unicode String Name or a ID
L0:
            lodsd ; load the name ID to eax
            Mov D$RsrcTypeStringLen 0
            Mov edx eax

            Mov D$edi+CustomList.TypeDis edx
            If D$RsrcType = 0
                Mov D$RsrcType edx
            End_If

            add edi 4

            Test_If eax 08000_0000 ; If it is a named ID, load th resource strings
                Push esi, edi
                    Mov esi eax, edi RsrcTypeString, ebx RsrcTypeStringLen
                    Call LoadRsrcIDString
                Pop edi, esi

            Test_End

            ; Now, load the OffsetToData and save it´s value at eax
            lodsd

            .Test_If eax 08000_0000 ; If the high bit (0x80000000) is set this is a node
                xor eax 08000_0000 | add eax D$UserPEStartOfResources
                Call FillCustomListFromResourceTree eax edi

                .If D@Array > 1
                  ; let´s search from the rest of the array IMAGE_RESOURCE_DIRECTORY_ENTRY.
                  ; point to the previous good datatype
                    add edi 4
                    dec D@Array
                    If D$RsrcType <> 0
                        Move D$edi+CustomList.TypeDis D$RsrcType
                        add edi 4
                    Else
                        Move D$RsrcType D$esi
                        Move D$edi+CustomList.TypeDis D$RsrcType
                    End_If
                    jmp L0<<
                .Else_If D@Array = 1
                    Mov D$RsrcType 0
                .End_If

            .Test_Else ; If the high bit (0x80000000) is not set this is a leaf
                ; Get the size and address of the data
                add eax D$UserPEStartOfResources
                Mov esi eax
                lodsd | sub eax D$ResourcesRVA| add eax D$UserPEStartOfResources
                add edi 4
                Mov ecx D$esi
                Mov D$edi ecx ; copy it to CustomList.SizeDis
                pushad
                    Call ReadResource
                    Mov D$edi-4 eax ; copy the read address to CustomList.PointerDis
                popad
            .Test_End


EndP

____________________________________________________________________________________________
; Copy Standard Type resources from CustomList to each list (AviList, BitmapList,MenuList, etc)

[AviTypeDir: D$ ?
 WaveTypeDir: D$ ?]

Proc CopyStandarTypeResources:

    Mov esi CustomList, edi esi, D$AviTypeDir 0, D$WaveTypeDir 0
L1: cmp D$esi 0 | je l5>>
    lodsd
    Mov D$RsrcTypeStringLen 0
    test eax 08000_0000 ZERO L4>
    Push esi, edi
        Mov esi eax, edi RsrcTypeString, ebx RsrcTypeStringLen
        Call LoadRsrcIDString
    Pop edi, esi

    If D$AviTypeDir = 0
        Push esi, edi, ecx
            Mov esi AviTypeStr, edi RsrcTypeString, ecx D$AviTypeStrLen
            repe cmpsb
        Pop ecx, edi, esi
        je L2>
    End_If

    If D$WaveTypeDir = 0
        Push esi, edi, ecx
            Mov esi WaveTypeStr, edi RsrcTypeString, ecx D$WaveTypeStrLen
            repe cmpsb
        Pop ecx, edi, esi
        je L3>
    End_If

L4: add esi 16 | jmp L1<
L2: Mov D$AviTypeDir eax | jmp L4<
L3: Mov D$WaveTypeDir eax | jmp L4<
L5:

    Call FillResourceTypeList CustomList, AviList, AviListPtr, D$AviTypeDir, MAXAVI
    Call FillResourceTypeList CustomList, WaveList, WaveListPtr, D$WaveTypeDir, MAXWAVE
    Call FillResourceTypeList CustomList, CursorList, CursorListPtr, &RT_CURSOR, MAXCURSOR
    Call FillResourceTypeList CustomList, BitmapList, BitmapListPtr, &RT_BITMAP, MAXBITMAP
    Call FillResourceTypeList CustomList, MenuList, MenuListPtr, &RT_MENU, MAXMENU
    Call FillResourceTypeList CustomList, DialogList, DialogListPtr, &RT_DIALOG, MAXDIALOG
    Call FillResourceTypeList CustomList, StringsList, StringsListPtr, &RT_STRING, MAXSTRINGS
    Call FillResourceTypeList CustomList, RCdataList, RCdataListPtr, &RT_RCDATA, MAXRCDATA
    Call FillResourceTypeList CustomList, GroupCursorList, GroupCursorListPtr, &RT_GROUP_CURSOR, MAXCURSOR
    Call FillResourceTypeList CustomList, IconList, IconListPtr, &RT_ICON, MAXICON
    ; Erase the First Icon, Which is the Main One (elsewhere...)
    If D$IconList = 1

        Call VirtualFree IconList+4

        Mov esi IconList, edi esi, ecx MAXICON-3 | add esi 12 | rep movsd
        On D$IconListPtr > IconList, sub D$IconListPtr 12
    End_If
    Call FillResourceTypeList CustomList, GroupIconList, GroupIconListPtr, &RT_GROUP_ICON, MAXICON
    ; Erase the First GroupIcon, Which is the Main One (elsewhere...)
    If D$GroupIconList = 1

        Call VirtualFree GroupIconList+4

        Mov esi GroupIconList, edi esi, ecx MAXICON-3 | add esi 12 | rep movsd
        On D$GroupIconListPtr > GroupIconList, sub D$GroupIconListPtr 12
    End_If

____________________________________________________________________________________________
; Remove "standard" resources from CustomList:
    Mov esi CustomList, edi esi
L1: cmp D$esi 0 | je L5>>
    lodsd
    Mov D$RsrcTypeStringLen 0
    cmp eax 16 | jg L2>
    test eax 08000_0000 ZERO L3>

    cmp eax D$AviTypeDir | je L4>
    cmp eax D$WaveTypeDir | je L4>

L2: stosd | movsd | movsd | movsd | movsd | jmp L1<

L3: cmp eax &RT_FONTDIR | je L2<
    cmp eax &RT_FONT | je L2<
    cmp eax &RT_ACCELERATOR | je L2<
    cmp eax &RT_MESSAGETABLE | je L2<
    cmp eax 13 | je L2<
    cmp eax 15 | je L2<
    cmp eax &RT_VERSION | je L2<

L4: add esi 16 | jmp L1<<

L5: Mov eax 0, ecx 5 | rep stosd

EndP
____________________________________________________________________________________________

Proc FillResourceTypeList:
    Arguments @InputList, @OutputList, @OutputListPtr, @ResType, @MaxResource

    pushad
    ; Clear the output list
    xor eax eax
    Mov edi D@OutputList, ecx D@MaxResource | rep stosd

    Mov esi D@InputList, edi D@OutputList
    While D$esi <> 0
        Mov edx D@ResType
        If D$esi+CustomList.TypeDis = edx
            Move D$edi D$esi+CustomList.NameDis ; copy Name only
            Move D$edi+4 D$esi+CustomList.PointerDis ; copy Pointer only
            Move D$edi+8 D$esi+CustomList.SizeDis ; copy size only
            add edi 12
        End_If
        add esi Size_Of_CustomList
    End_While

    Mov edx D@OutputListPtr
    Mov D$edx edi

    popad

EndP

____________________________________________________________________________________________
LoadRsrcIDString:
; In esi: source Unicode string entry ptr
; In edi: destination ptr
; In ebx: dest String size ptr (including null)

    Push eax, ecx
        and esi 07ff_ffff | add esi D$UserPEStartOfResources
        Mov eax 0
        lodsw
        Mov ecx eax, D$ebx eax | inc D$ebx
L1:     cmp ecx 0 | je L2>
            lodsw | stosb
            dec ecx
        jmp L1<
L2: Mov eax &NULL | stosb
    Pop ecx, eax

ret

____________________________________________________________________________________________

[MissingResource: D$ ?]

ReadResource:
; In eax: ptr to resource to read
; In ecx: size of data
; Out eax: Pointer to loaded data

    Mov B$MissingResource &FALSE


    If eax < D$UserPeStart
        ;Mov B$MissingResource &TRUE | ret
        jmp DisFail
    Else_If eax > D$UserPeEnd
        ;Mov B$MissingResource &TRUE | ret
        jmp DisFail
    End_If

    Push esi, edi
      ; Allocate space for data+id's strings
        Push eax
            Push ebx, ecx

                add ecx D$RsrcTypeStringLen
                add ecx D$RsrcNameStringLen

                Call VirtualAlloc TempoMemPointer,
                                  ecx

            Pop ecx, ebx
        Pop esi

      ; Copy data
        Mov edi D$TempoMemPointer
        Push edi, ecx
            rep movsb
        Pop ecx, eax

      ; Copy strings
        Mov esi RsrcTypeString, ecx D$RsrcTypeStringLen | rep movsb
        Mov esi RsrcNameString, ecx D$RsrcNameStringLen | rep movsb
    Pop edi, esi

ret

____________________________________________________________________________________________

NewTemporaryFillRsrcList:
  ; All the resources lists are in format Name/Pointer/Size except:
  ; CustomList, wich is Type/Name/Lang/Pointer/Size
  ; Now, we will fill uRsrcList with format Type/Name/Lang/Pointer/Size
  ; and then will sort it.

    Mov edi uRsrcList, D$TypeByName 0

    Mov esi CustomList
    If D$esi > 0
        While D$esi > 0
        Mov ecx 5 | rep movsd
        End_While
    End_If

    Mov esi AviList
    If D$esi > 0
        While D$esi > 0
            Mov eax RT_AVI | stosd | movsd | Mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    Mov esi WaveList
    If D$esi > 0
        While D$esi > 0
            Mov eax RT_WAVE | stosd | movsd | Mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    Mov esi CursorList
    If D$esi > 0
        While D$esi > 0
            Mov eax &RT_CURSOR | stosd | movsd | Mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    Mov esi BitMapList
    If D$esi > 0
        While D$esi > 0
            Mov eax &RT_BITMAP | stosd | movsd | Mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

  ; Store default icon if user didn't edit any (or user defined if any):
    If B$NoMainIcon = &FALSE
        Mov eax &RT_ICON | stosd | Mov eax ID_Icon | stosd | Mov eax Language | stosd
        Mov eax uIcon | stosd | Mov eax uIconEnd | sub eax uIcon | stosd
    End_If

    Mov esi IconList
    If D$esi > 0
        While D$esi > 0
            Mov eax &RT_ICON | stosd | movsd | Mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    Mov esi MenuList
    If D$esi > 0
        While D$esi > 0
            Mov eax &RT_MENU | stosd | movsd | Mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    Mov esi DialogList
    If D$esi > 0
        While D$esi > 0
            Mov eax &RT_DIALOG | stosd | movsd | Mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    Mov esi StringsList
    If D$esi > 0
        While D$esi > 0
            Mov eax &RT_STRING | stosd | movsd | Mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    Mov esi RcDataList
    If D$esi > 0
        While D$esi > 0
            Mov eax &RT_RCDATA | stosd | movsd | Mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    Mov esi GroupCursorList
    If D$esi > 0
        While D$esi > 0
            Mov eax &RT_GROUP_CURSOR | stosd | movsd | Mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    If B$NoMainIcon = &FALSE
        Mov eax &RT_GROUP_ICON | stosd | Mov eax ID_Group_Icon | stosd
        Mov eax Language | stosd
        Mov eax uGroupIcon | stosd
        Mov eax uGroupIconEnd | sub eax uGroupIcon | stosd
    End_If

    Mov esi GroupIconList
    If D$esi > 0
        While D$esi > 0
            Mov eax &RT_GROUP_ICON | stosd | movsd | Mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

  ; Close this List, because, in case user would have deleted some Resources, there
  ; could be old Records here, from a previous [Compile]:
    Mov eax 0, ecx 5 | rep stosd | sub edi (5*4)

  ; -4 > ready for backward read
    sub edi 4 | Mov D$uRsrcListPtr edi
____________________________________________________________________________________________
; Sort uRsrcList Table by Type, then by name and then by lang:
    Mov esi uRsrcList
    Push edi, esi

; First, sort by Type:
L0:     Mov edx &FALSE
        .While D$esi > 0
            Mov eax D$esi | cmp eax D$esi+20 | je L4>>
            test eax BIT31 NOT_ZERO L2>
L1:       ; Type's Id by number:
            cmp eax D$esi+20 | jle L4>>
                jmp L3>
L2:       ; Type's Id by string:
            test D$esi+20 BIT31 ZERO L4>>
            Push esi
                Mov eax esi
                If D$eax = RT_AVI
                    Mov esi AviTypeStr
                Else_If D$eax = RT_WAVE
                    Mov esi WaveTypeStr
                Else
                    Mov esi D$eax+12 | add esi D$eax+16
                End_If
                add eax 20
                If D$eax = RT_AVI
                    Mov edi AviTypeStr
                Else_If D$eax = RT_WAVE
                    Mov edi WaveTypeStr
                Else
                    Mov edi D$eax+12 | add edi D$eax+16
                End_If
                Mov ecx 32 | repe cmpsb
                Mov al B$esi-1
            Pop esi
            cmp al B$edi-1 | jbe L4>
L3:       ; Exchage Resource
            If D$esi+20 <> 0
                Exchange D$esi D$esi+20, D$esi+4 D$esi+24, D$esi+8 D$esi+28,
                    D$esi+12 D$esi+32, D$esi+16 D$esi+36
                Mov edx &TRUE
            End_If
L4:       ; Next Resource
            add esi 20
        .End_While
     Pop esi | Push esi
        cmp edx &TRUE | je L0<<

; Then sort by name:
L0:     Mov edx &FALSE
        .While D$esi > 0
            Mov eax D$esi | cmp eax D$esi+20 | jne L4>>
            Mov eax D$esi+4 | cmp eax D$esi+24 | je L4>>
            test eax BIT31 NOT_ZERO L2>
L1:       ; Name's Id by number:
            cmp eax D$esi+24 | jle L4>>
                jmp L3>
L2:       ; Name's Id by string:
            test D$esi+24 BIT31 ZERO L4>>
            Push esi
                Mov ebx esi, eax 0
                Mov esi D$ebx+12 | add esi D$ebx+16
                Mov edi D$ebx+32 | add edi D$ebx+36
                If D$ebx+20 <s 0
                    Mov ecx 32 | repne scasb | add esi 32 | sub esi ecx
                End_If
                Mov ecx 32 | repe cmpsb
                Mov al B$esi-1
            Pop esi
            cmp al B$edi-1 | jbe L4>
L3:       ; Exchage Resource
            If D$esi+20 <> 0
                Exchange D$esi D$esi+20, D$esi+4 D$esi+24, D$esi+8 D$esi+28,
                    D$esi+12 D$esi+32, D$esi+16 D$esi+36
                Mov edx &TRUE
            End_If
L4:       ; Next Resource
            add esi 20
        .End_While
   Pop esi | Push esi
        cmp edx &TRUE | je L0<<

; And finally, sort by lang:
L0:     Mov edx &FALSE
        .While D$esi > 0
            Mov eax D$esi | cmp eax D$esi+20 | jne L1>
            Mov eax D$esi+4 | cmp eax D$esi+24 | jne L1>
          ; Lang's Id are always by number, so:
            Mov eax d$esi+8 | cmp eax D$esi+28 | jbe L1>
          ; Exchage Resource
            If D$esi+20 <> 0
                Exchange D$esi D$esi+20, D$esi+4 D$esi+24, D$esi+8 D$esi+28,
                    D$esi+12 D$esi+32, D$esi+16 D$esi+36
                Mov edx &TRUE
            End_If
L1:       ; Next Resource
            add esi 20
        .End_While
    Pop esi | Push esi
        cmp edx &TRUE | je L0<

    Pop esi, edi


ret

____________________________________________________________________________________________
[BIT31 0_8000_0000]
; Resources Head Line Macro. In: #1: Entries by ID, #2: Entries by name
[RsrcHeadLine | add edi 2 | Mov eax #1 | stosw | Mov eax #2 | stosw
    sub edi 2 | Mov eax 0 | stosd | stosd | stosd]

NewBuildResourceTree:
  ; Initialisation of tree pointers:
    Mov eax D$CodeListPtr, D$RsrcHeadPtr eax, D$RsrcTypePtr eax,
      D$RsrcLangPtr eax, D$RsrcPtrPtr eax, D$RsrcSectionPtr eax,
      D$RsrcSectionOrigine eax

  ; Search for the tree size (evaluation begins at second record):
    Mov ecx 1                            ; how many resources in ecx
    Mov edx 1                            ; how many different resources types in edx
    Mov ebx 1                            ; how many langage in ebx
    Mov esi uRsrcList+20                 ; start comparisons at second record
L0: Mov eax D$esi | cmp eax 0 | je L3>
        inc ecx                          ; count resources
        cmp eax D$esi-20 | je L1>
            inc edx                      ; count different types
            inc ebx | jmp L2>            ; if new type >>> count language
L1: Mov eax D$esi+4
      On eax <> D$esi-16, inc ebx        ; same type: different name?>>> count language
L2: add esi 20 | jmp L0<
;;
    down > top:
    - as many ptr record as resources records in 'RsrcList' (ecx * 16)
    - as many lang headers as ebx + as many lang records as ptrs ((ebx * 16)+(ecx * 8))
    - as many type headers as edx + as many types records as lang ((edx * 16)+(ebx*8))
    - records in root as types (16 + (edx * 8))
    >>>  (ecx * 16)+(ebx * 16)+(ecx * 8)+(edx * 16)+(ebx*8)+16+(edx * 8)
    >>>  (ecx * (16+8)) + (ebx * (16+8)) + (edx * (16+8)) + 16
    >>>  ((ecx+ebx+edx) * (16+8)) + 16
;;
L3: add ecx ebx | add ecx edx | Mov eax ecx | shl eax 4 | shl ecx 3
    add eax ecx | add eax 16

____________________________________________________________________________________________

    Mov ebx uRsrcList | add ebx 12 | Mov edi D$CodeListPtr

  ; Clear the header (may bee corrupt by previous use of same memory)
    Mov ecx eax, al 0 | rep stosb

   Push edi

____________________________________________________________________________________________
  ; Strings for Types ID's by Name
    [RsrcAVIString: U$ 3 'AVI' RsrcWAVEString: U$ 4 'WAVE']
    Push ebx
      ; Type ID in uRsrcList
        sub ebx 12
        Mov edx 0
        .While D$ebx > 0
          ; If bit 31 = 0 skip resource
            test D$ebx BIT31 ZERO L4>>
                cmp edx D$ebx | jne L1>
                  ; If same Type by name that last one, Copy RVA
                    Move D$ebx D$ebx-20 | jmp L4>>

L1:       ; New type by name
            Mov edx D$ebx

          ; Avi Type
            cmp D$ebx RT_AVI | jne L2>
                Mov eax edi, esi RsrcAVIString, ecx 8 | rep movsb
              ; Change ID in uRsrcList to RVA and set the 'ID by name' flag:
                sub eax D$RsrcSectionOrigine | or eax BIT31 | Mov D$ebx eax
                jmp L4>

          ; Wave Type
L2:         cmp D$ebx RT_WAVE | jne L3>
                Mov eax edi, esi RsrcWAVEString, ecx 10 | rep movsb
              ; Change ID in uRsrcList to RVA and set the 'ID by name' flag:
                sub eax D$RsrcSectionOrigine | or eax BIT31 | Mov D$ebx eax
                jmp L4>

          ; Custom Type
          ; esi points String in uRsrcList
L3:         Mov esi D$ebx+12 | add esi D$ebx+16
          ; skip 'String Size' for now
            Push edi | add edi 2
                Mov eax 0, ecx 0
              ; Put String in Unicode format
                While B$esi <> 0
                    movsb | stosb
                    inc ecx
                End_While
            Pop eax
          ; now put 'String Size'
            Mov W$eax cx
          ; Change ID in uRsrcList to RVA and set the 'ID by name' flag:
            sub eax D$RsrcSectionOrigine | or eax BIT31 | Mov D$ebx eax

L4:         add ebx 20
        .End_While
    Pop ebx

  ; Strings for Name ID's by Name
    Push ebx
        sub ebx 8 ; Name ID in uRsrcList
        Mov edx 0
        .While D$ebx > 0
            test D$ebx BIT31 NOT_ZERO L1>
              ; bit 31 = 0, skip this resource
                Mov edx 0 | jmp L4>
          ; Last Type
L1:         Mov eax D$ebx-24
          ; If not same Type that last one jump
            cmp D$ebx-4 eax | jne L2>
                cmp edx D$ebx | jne L2>
                  ; If same ID by name that last one, Copy RVA
                    Move D$ebx D$ebx-20 | jmp L4>
L2:       ; New id by name
            Mov edx D$ebx
          ; esi points String in uRsrcList
            Mov esi D$ebx+8 | add esi D$ebx+12
            test D$ebx-4 BIT31 ZERO L3>
              ; Type by name, skip Type string:
                xchg edi esi | Mov ecx 32 | Mov al 0 | repne scasb | xchg edi esi
          ; skip 'String Size' for now
L3:         Push edi | add edi 2
                Mov eax 0, ecx 0
              ; Put String in Unicode format
                While B$esi <> 0
                    movsb | stosb
                    inc ecx
                End_While
            Pop eax
          ; Now put 'String Size'
            Mov W$eax cx
          ; Change ID in uRsrcList to RVA and set the 'ID by name' flag:
            sub eax D$RsrcSectionOrigine | or eax BIT31 | Mov D$ebx eax
L4:     add ebx 20
        .End_While
    Pop ebx

    AlignOn 16 edi
____________________________________________________________________________________________
  ; Write all data resources in .rsrc.
  ; Before action, the pointers in 'uRsrcList' point to each resource's raw data in memory
  ; After action, same pointers point to each resource in .rsrc section:
    While D$ebx > 0
      ; This is the ptr
        Mov esi D$ebx
        Mov eax edi | sub eax D$RsrcSectionOrigine
      ; change Ptrs in uRsrcList to RVA
        add eax D$uBaseOfRsrc | Mov D$ebx eax
      ; Size
        Mov ecx D$ebx+4
        rep movsb
        Mov eax edi | AlignOn 010 eax | Mov edi eax
        add ebx 20
    End_While

    Mov eax edi | sub eax D$RsrcSectionOrigine | Mov D$uRsrcSize eax

    Mov eax edi | AlignOn 0200 eax
    Mov D$CodeListPtr eax
    Pop edi | sub edi 4

____________________________________________________________________________________________

  ; Write the tree backward:
____________________________________________________________________________________________
  ; Pointers directory:

  ; end of uRsrcList (> size)
    Mov esi D$uRsrcListPtr
  ; BackWard
    std
    Do
      ; write ptrs records
        Mov eax 0 | stosd | stosd
      ; size / RVA ptr to true data
        movsd | movsd
      ; adress of record start
        Mov eax edi | add eax 4
      ; displacement from start of .rsrc
        sub eax D$RsrcSectionOrigine
      ; ptrs-dir pointer in next level up
        Mov D$esi+4 eax
        sub esi 12
    Loop_until D$esi = 0

____________________________________________________________________________________________
  ; Language directory

    Mov esi D$uRsrcListPtr
  ; Write Languages dirs
    sub esi 4 | Mov ecx 0

    Do
      ; pointers to data Ptrs / Lang
        movsd | movsd
      ; Name
        Mov ebx D$esi
      ; records counter for lang header
        inc ecx
      ; Check if there are more languages (same type and name that next resource):
      ; Compare Names
        Mov eax D$esi | cmp eax D$esi-20 | jne L1>
      ; Compare Types
        Mov eax D$esi-4 | cmp eax D$esi-24 | je L2>
      ; If not equal write headLine
L1:     RsrcHeadLine ecx, 0 | Mov ecx 0
        Mov eax edi | add eax 4
        sub eax D$RsrcSectionOrigine
      ; node flag
        or eax BIT31
      ; ptrs-dir pointer in next level up
        Mov D$esi+8 eax
      ; next ptr record in uRsrcList
L2:     sub esi 12
     Loop_until D$esi = 0

____________________________________________________________________________________________
  ; we do not need any more sizes and Lang values. We do not need any more double
  ; records for Languages. So, we rewrite uRsrcList:
    cld
    Push edi
        Mov esi uRsrcList, edi uRsrcList
        Do
          ; keep Type / keep ID / skip Lang / keep Ptr / skip size
            movsd | movsd | lodsd | movsd | lodsd
        Loop_Until D$esi = 0
        Mov D$edi 0, esi uRsrcList, edi esi
      ; type / ID??? / ptr
L0:     movsd | Lodsd | stosd | movsd
      ; Compare Names
L1:     Mov eax D$esi-8 | cmp eax D$esi+4 | jne L2>
      ; Compare Types
        Mov eax D$esi-12 | cmp eax D$esi | jne L2>
      ; Skip if equal
        add esi 12 | jmp L1<
L2:
        cmp D$esi 0 | jne L0<
      ; > last record ptr
        Mov esi edi | sub esi 4
        Mov eax 0 | stosd | stosd | stosd
    Pop edi

____________________________________________________________________________________________
  ; Types directory
    std
        Mov ecx 0, edx 0
      ; Pointer
L1:     movsd
        lodsd
      ; edx = ID by name count; ecx = ID by number count
        test eax BIT31 ZERO L2>
            inc edx
            jmp L3>
L2:         inc ecx
      ; Name / Type
L3:     stosd | lodsd
        cmp eax D$esi-8 | je L4>

        RsrcHeadLine ecx edx | Mov ecx 0, edx 0
        Mov eax edi | add eax 4
        sub eax D$RsrcSectionOrigine
      ; node flag
        or eax BIT31
      ; ptrs-dir pointer in next level up
        Mov D$esi+12 eax
L4:     cmp D$esi 0 | jne L1<

  ; We do not need any more ID. So, we rewrite uRsrcList:
    cld
    Push edi
        Mov esi uRsrcList, edi uRsrcList
      ; keep Type / skip ID / keep Ptr
L0:     movsd | lodsd | movsd
        cmp D$esi 0 | jne L0<
            Mov D$edi 0, esi uRsrcList, edi esi
      ; type??? / ptr
L0:     Lodsd | stosd | movsd
L1:     cmp D$esi eax | jne L2>
            add esi 8 | jmp L1<
L2:     cmp D$esi 0 | jne L0<
        Mov esi edi | sub esi 4
        Mov eax 0 | stosd | stosd
    Pop edi

____________________________________________________________________________________________
  ; Root Directory:
    std
        Mov ecx 0, edx 0
      ; Ptr
L1:     movsd
        lodsd
      ; edx = ID by name count; ecx = ID by number count
        test eax BIT31 ZERO L2>
            inc edx | jmp L3>
L2:         inc ecx
      ; Type
L3:     stosd
        cmp D$esi 0 | jne L1<
        RsrcHeadLine ecx, edx
    cld

ret


____________________________________________________________________________________________
;[NoResources: D$ ?    NoMainIcon: D$ ?]

BuildRsrc:
    Mov B$NoResources &FALSE, B$NoMainIcon &FALSE

    .If D$SavingExtension = '.DLL'
        Mov B$NoMainIcon &TRUE
    .Else_If D$SavingExtension = '.SYS'
        Mov B$NoMainIcon &TRUE
    .Else_If W$SubSystem = 3             ; Console > no Main Icon
        Mov B$NoMainIcon &TRUE
    .End_If


  ; THIS LINE SHOULD BE COMMENTED FOR TESTING WITH RsrcSTUB:
    Call NewTemporaryFillRsrcList

  ; THIS LINE SHOULD BE UNCOMMENTED FOR TESTING WITH RsrcSTUB:
  ;  Mov esi RsrcStub, edi uRsrcList, ecx D$RsrcStubLen | rep movsd | sub edi 4 | Mov D$uRsrcListPtr edi


    If D$uRsrcList = 0
        Mov B$NoResources &TRUE
    Else
        Call NewBuildResourceTree
    End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
