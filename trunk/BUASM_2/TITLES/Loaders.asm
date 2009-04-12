TITLE Loaders
________________________________________________________________________________________
________________________________________________________________________________________

                         ; Loaders for non-edited Resources
________________________________________________________________________________________
________________________________________________________________________________________

[WaveType: U$ 'WAVE'    AviType: 'AVI'] [WAVETYPELEN 4    AVITYPELEN 3] ; 'SOUND'

; read new open PE Resources:

ReadRosAsmWaves:
    Mov edi WaveList, eax 0, ecx 300 | rep stosd
    Mov edi WaveType, edx WAVETYPELEN | Call SearchResourceNamedType | On eax = 0, ret
    Mov D$WaveListPtr WaveList,  ebx WaveListPtr | Call ReadResourcesRecord
ret

ReadRosAsmAvis:
    Mov edi AviList, eax 0, ecx 300 | rep stosd
    Mov edi AviType, edx AVITYPELEN | Call SearchResourceNamedType | On eax = 0, ret
    Mov D$AviListPtr AviList,  ebx AviListPtr | Call ReadResourcesRecord
    Mov esi AviList
ret

ReadRosAsmRCs:
    Mov edi RcDataList, eax 0, ecx 300 | rep stosd
    Mov ebx &RT_RCDATA | Call SearchResourceType | On eax = 0, ret
    Mov D$RcDataListPtr RcDataList,  ebx RcDataListPtr | Call ReadResourcesRecord
ret


ReadRosAsmCursors:
    Mov edi CursorList, eax 0, ecx 300 | rep stosd
    Mov ebx &RT_CURSOR | Call SearchResourceType | On eax = 0, ret
    Mov D$CursorListPtr CursorList,  ebx CursorListPtr | Call ReadResourcesRecord
ret


ReadRosAsmGroupCursors:
    Mov edi GroupCursorList, eax 0, ecx 300 | rep stosd
    Mov ebx RT_GROUP_CURSOR | Call SearchResourceType | On eax = 0, ret
    Mov D$GroupCursorListPtr GroupCursorList,  ebx GroupCursorListPtr
    Call ReadResourcesRecord
ret

ReadRosAsmIcons:
    Mov edi IconList, eax 0, ecx 300 | rep stosd

    Mov ebx &RT_ICON | Call SearchResourceType | On eax = 0, ret

    Mov D$IconListPtr IconList,  ebx IconListPtr | Call ReadResourcesRecord

  ; Arase the First Icon, Which is the Main One (elsewhere...)
    VirtualFree D$IconList+4

    Mov esi IconList, edi esi, ecx MAXICON-3 | add esi 12 | rep movsd
    On D$IconListPtr > IconList, sub D$IconListPtr 12
ret


ReadRosAsmGroupIcons:
    Mov edi GroupIconList, eax 0, ecx 300 | rep stosd
    Mov ebx RT_GROUP_ICON | Call SearchResourceType | On eax = 0, ret
    Mov D$GroupIconListPtr GroupIconList,  ebx GroupIconListPtr
    Call ReadResourcesRecord

  ; Arase the First GroupIcon, Which is the Main One (elsewhere...)
    VirtualFree D$GroupIconList+4

    Mov esi GroupIconList, edi esi, ecx MAXICON-3 | add esi 12 | rep movsd
    On D$GroupIconListPtr > GroupIconList, sub D$GroupIconListPtr 12
ret

[ResourcePointersSecurity: ? #10]

; Each Record of these Lists is: ID / Ptr / Size.

[CursorList: ? #MAXCURSOR]      [CursorListPtr: CursorList]
[GroupCursorList: ? #MAXCURSOR] [GroupCursorListPtr: GroupCursorList]
[IconList: ? #MAXICON]          [IconListPtr: IconList]
[MenuList: ? #MAXMENU]          [MenuListPtr: MenuList]
[DialogList: ? #MAXDIALOG]      [DialogListPtr: DialogList]
[StringsList: ? #MAXSTRINGS]    [StringsListPtr: StringsList]
[GroupIconList: ? #MAXICON]     [GroupIconListPtr: GroupIconList]
[WaveList: ? #MAXWAVE]          [WaveListPtr: WaveList]
[AviList: ? #MAXAVI]            [AviListPtr: AviList]
[RCdataList: ? #MAXRCDATA]      [RCdataListPtr: RCdataList]
[BitMapList: ? #MAXBITMAP]      [BitMapListPtr: BitMapList]
[OtherList: 0    OtherListPtr: 0]

[FontFilesTitle:     B$ 'Choose a Font File', 0
 WaveFilesFilters:   B$ 'Wave Files' 0 '*.wav' 0 0
 WaveFilesTitle:     B$ 'Chose a Wave File', 0
 AviFilesFilters:    B$ 'Avi Files' 0   '*.avi', 0 0
 AviFilesTitle:      B$ 'Choose an Avi File', 0
 RCDataFilesFilters: B$ 'AnyThing' 0  '*.*' 0 0
 RCDataFilesTitle:   B$ 'Choose a File', 0
 CursorFilesFilters: B$ 'Cursor Files' 0   '*.cur' 0 0
 CursorFilesTitle:   B$ 'Choose a Cursor File' 0
 IconFilesFilters: B$ 'Icon Files' 0   '*.Ico' 0 0
 IconFilesTitle:   B$ 'Choose an Icon File' 0
 DialogFilesFilters: B$ 'Dlg Files', 0, '*.dlg', 0, 0
 BinDialogFilesFilters: B$ 'bdf Files', 0, '*.bdf', 0, 0
 BinMenuFilesFilters: B$ 'bmf Files', 0, '*.bmf', 0, 0
 DialogFilesTitle: B$ 'Choose a Dialog Template File' 0
 MenuFilesTitle: B$ 'Choose a Menu Template File' 0]

[OtherChoosenFile: B$ ? #260] [OtherFileFilter: B$ ? #260]
[OtherSourceFilterPtr: B$ ? #260][OtherSaveFilter: ? #260]

[OtherOpenStruc: len
 OtherhwndFileOwner: 0  OtherhInstance: 0
 OtherFilesFilters: 0  OtherFileFilter  260
 OtherSourceFilterPtr  OtherSaveFilter  260  OtherChoosenFile  260  0
 OpenOtherFileTitle: 0  OtherFlags: 0281804 ; for read
 0  0  0  0  0]                   ; 0280006 : for write

[OtherSourceHandle: ?    OtherFileLen: ?    OtherFilePtr: ?]

ReadRCData:
    Mov D$OtherFilesFilters RCDataFilesFilters
    Mov D$OpenOtherFileTitle RCDataFilesTitle
    Mov D$OtherList RcDataList | move D$OtherListPtr D$RCDataListPtr
    Call ReadOtherFile
        If D$OtherSaveFilter = 0
            Mov D$OtherList 0,  D$OtherListPtr 0 | ret
        End_If
        Call AskForResID | add D$RCDataListPtr 12
    Call CloseOtherFilesRead
ret


SaveRcData:
    If D$RCDataList = 0

        Call MessageBox  Argh,
                         NoRcData,
                         &MB_SYSTEMMODAL+&MB_USERICON

        ret

    End_If

    Mov D$WhatDialogListPtr RcDataList,  D$OkDialogFlag &FALSE
    add D$WhatDialogListPtr 4

    Call 'USER32.CreateDialogParamA' D$H.Instance, 1000, &NULL, EmptyProc, &NULL
    Mov D$EmptyDialogHandle eax


    .While B$OkDialogFlag = &FALSE
        Call SetRcDataTitle | Call ShowHexa
        Call SetNextChoiceID
        Call 'User32.DialogBoxIndirectParamA' D$H.Instance, ChoiceBar, D$H.MainWindow,
                                              ChoiceDialogBoxProc, RcDataList
        .If D$OkDialogFlag = &VK_ESCAPE
            jmp L9>>
        .Else_If D$WhatDialogListPtr < RCDataList
            add D$WhatDialogListPtr 12
            Call SetNextChoiceID
        .Else
            Mov esi D$WhatDialogListPtr
            If D$esi = 0
                sub D$WhatDialogListPtr 12
                Call SetNextChoiceID
            End_If
        .End_If
   .End_While

    Call MessageBox  Sure,
                     SaveRc,
                     &MB_SYSTEMMODAL+&MB_ICONQUESTION+&MB_YESNO

    push D$FL.MsgBoxReturn
        Call 'User32.EndDialog' D$EmptyDialogHandle &NULL
    pop eax

   .If eax = &IDYES
        Mov D$OpenDlg.lpstrFilter RcFilesFilters
        Call 'Comdlg32.GetSaveFileNameA' OpenDlg
        Mov D$OpenDlg.lpstrFilter DlgFilesFilters
        On eax = &FALSE, ret

        Call 'KERNEL32.CreateFileA' SaveDlgFilter &GENERIC_WRITE,
                                &FILE_SHARE_READ, 0,
                                &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0

        If eax = &INVALID_HANDLE_VALUE

            Call MessageBox argh,
                            D$BusyFilePtr,
                            &MB_SYSTEMMODAL+&MB_USERICON

            ret

        End_If

        Mov D$DestinationHandle eax, D$NumberOfReadBytes 0
        Mov esi D$WhatDialogListPtr, ecx D$esi+4, esi D$esi

        Call 'KERNEL32.WriteFile' D$DestinationHandle, esi, ecx, NumberOfReadBytes  0

        Call 'KERNEL32.CloseHandle' D$DestinationHandle | Mov D$DestinationHandle 0
   .End_If

L9: ret


[NoRcData: B$ "No RC data resources in this PE" EOS]
[KillRc: B$ "Delete this RC data resource ?" EOS]
[SaveRc: B$ "Save this RC resource to disk ?" EOS]
[IdTitle: B$ "RC data ID: " IdTitleID: B$ "       " EOS]

SetRcDataTitle:
    Mov eax D$WhatDialogListPtr, eax D$eax-4, edi IdTitleID
    Call TransDwordToAsciiDecimal
    Call 'USER32.SendMessageA' D$EmptyDialogHandle &WM_SETTEXT 0 IdTitle
ret


ShowHexa:
    Mov eax D$WhatDialogListPtr, esi D$eax, ecx D$eax+4
    shl ecx 3 | On ecx > 0FF00, Mov ecx 0FF00
    push ecx, esi
        VirtualAlloc TempoMemPointer ecx | Mov edi D$TempoMemPointer
    pop esi, ecx
    shr ecx 3 | sub ecx 001111 | or ecx 001111
    push edi
L0:     lodsb | shr al 4 | add al '0' | On al > '9', add al 7 | stosb
        dec esi | lodsb | and al 0F | add al '0' | On al > '9', add al 7 | stosb
        Mov al ' ' | stosb
        test ecx 00111 | jnz L1>
            Mov al ' ' | stosb
        test ecx 001111 | jnz L1>
           sub esi 16
           push ecx
               Mov ecx 16
T0:            lodsb | On al < ' ', Mov al '.' | stosb | loop T0<
           pop ecx
           Mov ax 0A0D | stosw
L1:     loop L0<
    pop edi

    Call 'USER32.SetDlgItemTextA' D$EmptyDialogHandle, 100, edi
ret


DeleteRcData:
    If D$RCDataList = 0

        Call MessageBox Argh,
                        NoRcData,
                        &MB_SYSTEMMODAL+&MB_USERICON

        ret

    End_If

    Mov D$WhatDialogListPtr RcDataList,  D$OkDialogFlag &FALSE
    add D$WhatDialogListPtr 4
; Tag Dialog 1000
    Call 'USER32.CreateDialogParamA' D$H.Instance, 1000, &NULL, EmptyProc, &NULL
    Mov D$EmptyDialogHandle eax

    .While B$OkDialogFlag = &FALSE
        Call SetRcDataTitle | Call ShowHexa
        Call SetNextChoiceID
        Call 'User32.DialogBoxIndirectParamA' D$H.Instance, ChoiceBar, D$H.MainWindow, ChoiceDialogBoxProc, RcDataList

        .If D$OkDialogFlag = &VK_ESCAPE
            jmp L9>>
        .Else_If D$WhatDialogListPtr < RCDataList
            add D$WhatDialogListPtr 12
            Call SetNextChoiceID
        .Else
            Mov esi D$WhatDialogListPtr
            If D$esi = 0
                sub D$WhatDialogListPtr 12 ;| Mov esi D$WhatDialogListPtr
                Call SetNextChoiceID
            End_If
        .End_If
   .End_While

   Call MessageBox Sure,
                   KillRc,
                   &MB_SYSTEMMODAL+&MB_ICONQUESTION+&MB_YESNO

   If D$FL.MsgBoxReturn = &IDYES
        sub D$WhatDialogListPtr 4
        Mov esi D$WhatDialogListPtr, edi esi | add esi 12
        Mov eax D$WhatDialogListPtr | sub eax RCDataList | shr eax 2 | Mov ecx 300 | sub ecx eax
        rep movsd
        sub D$RCDataListPtr 12
   End_If

L9: Call 'User32.EndDialog' D$EmptyDialogHandle &NULL
ret


ReadWaveFile:
    Mov D$OtherFilesFilters WaveFilesFilters
    Mov D$OpenOtherFileTitle WaveFilesTitle
    Mov D$OtherList WaveList | move D$OtherListPtr D$WaveListPtr
    Call ReadOtherFile
        If D$OtherSaveFilter = 0
            Mov D$OtherList 0,  D$OtherListPtr 0 | ret
        End_If
        Call AskForResID | add D$WaveListPtr 12
    Call CloseOtherFilesRead
ret


[KillWave: B$ "Delete this Wave Resource ?" EOS]
[NoWave: B$ "No Wave Resources in this PE" EOS]
[TempoWaveFileHandle: D$ ? ]
[TempoWaveFile: B$ "Wave$$$.Wav" EOS]

DeleteWave:
    If D$WaveList = 0

        Call MessageBox Argh,
                        NoWave,
                        &MB_SYSTEMMODAL+&MB_USERICON

        ret

    End_If

    Mov D$WhatDialogListPtr WaveList,  D$OkDialogFlag &FALSE
    add D$WhatDialogListPtr 4

    .While B$OkDialogFlag = &FALSE
        Call 'KERNEL32.CreateFileA' TempoWaveFile &GENERIC_WRITE, 0, 0,
                                    &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
        Mov D$TempoWaveFileHandle eax
        Mov esi D$WhatDialogListPtr
        Call 'KERNEL32.WriteFile' D$TempoWaveFileHandle D$esi D$esi+4 NumberOfReadBytes &NULL
        Call 'Kernel32.CloseHandle' D$TempoWaveFileHandle
        Call 'WINMM.PlaySound' TempoWaveFile &NULL  &SND_ASYNC__&SND_FILENAME
        Call SetNextChoiceID
        Call 'User32.DialogBoxIndirectParamA' D$H.Instance, ChoiceBar, D$H.MainWindow, ChoiceDialogBoxProc, WaveList
        Call 'WINMM.PlaySound' &NULL &NULL &NULL
        .If D$OkDialogFlag = &VK_ESCAPE
            jmp L9>>
        .Else_If D$WhatDialogListPtr < WaveList
            add D$WhatDialogListPtr 12
            Call SetNextChoiceID
        .Else
            Mov esi D$WhatDialogListPtr
            If D$esi = 0
              sub D$WhatDialogListPtr 12 | Mov esi D$WhatDialogListPtr
              Call SetNextChoiceID
            End_If
        .End_If
   .End_While

    Call MessageBox Sure,
                    KillWave,
                    &MB_SYSTEMMODAL+&MB_ICONQUESTION+&MB_YESNO

    If D$FL.MsgBoxReturn = &IDYES
        sub D$WhatDialogListPtr 4
        Mov esi D$WhatDialogListPtr, edi esi | add esi 12
        Mov eax D$WhatDialogListPtr | sub eax WaveList | shr eax 2 | Mov ecx 300 | sub ecx eax
        rep movsd
        sub D$WaveListPtr 12
    End_If

L9: Call 'KERNEL32.DeleteFileA' TempoWaveFile
ret


ReadAviFile:
    Mov D$OtherFilesFilters AviFilesFilters
    Mov D$OpenOtherFileTitle AviFilesTitle
    Mov D$OtherList AviList | move D$OtherListPtr D$AviListPtr
    Call ReadOtherFile
        If D$OtherSaveFilter = 0
            Mov D$OtherList 0,  D$OtherListPtr 0 | ret
        End_If

        Call AskForResID | add D$AviListPtr 12
    Call CloseOtherFilesRead
ret


[TempoCursorMem: ?    CursorHotSpot: ?]
[BadCurFile: B$ "Bad or multiple cursor(s) file" EOS]
[BadIcoFile: B$ "Bad or multiple icon(s) file" EOS]

ReadCursor:
    Mov D$OtherFilesFilters CursorFilesFilters
    Mov D$OpenOtherFileTitle CursorFilesTitle
    Mov D$OtherList CursorList | move D$OtherListPtr D$CursorListPtr

    move D$OtherhwndFileOwner D$H.MainWindow, D$OtherhInstance D$H.Instance

    Mov edi OtherSaveFilter, ecx 260, eax 0 | rep stosd
    Call 'Comdlg32.GetOpenFileNameA' OtherOpenStruc
        If D$OtherSaveFilter = 0
            Mov D$OtherList 0,  D$OtherListPtr 0 | ret
        End_If

    Call 'KERNEL32.CreateFileA' OtherSaveFilter &GENERIC_READ,
                                &FILE_SHARE_READ, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE

        Call MessageBox argh,
                        D$BusyFilePtr,
                        &MB_SYSTEMMODAL+&MB_USERICON

        ret  ; return to caller of caller

    Else
      Mov D$OtherSourceHandle eax
    End_If

    Call 'KERNEL32.GetFileSize'  eax 0
    Mov D$OtherFileLen eax
    VirtualAlloc OtherFilePtr eax

    Mov D$NumberOfReadBytes 0
    Call 'KERNEL32.ReadFile' D$OtherSourceHandle D$OtherFilePtr,
                            D$OtherFileLen NumberOfReadBytes 0

    Mov eax D$OtherFilePtr
    If D$eax+2 <> 010002

        Call MessageBox Argh,
                        BadCurFile,
                        &MB_SYSTEMMODAL+&MB_USERICON

        VirtualFree D$OtherFilePtr | ret

    End_If

  ; We write both the RT_CURSOR and the RT_GROUP_CURSOR:
    VirtualAlloc TempoCursorMem D$OtherFileLen
    VirtualAlloc TempoMemPointer 20
    Mov edi D$GroupCursorListPtr, eax D$TempoMemPointer, D$edi+4 eax, D$edi+8 20

    Mov edi eax                                       ; edi > RT_GROUP_CURSOR mem

    Mov esi D$OtherFilePtr

    Mov ecx 20 | rep movsb
    move D$CursorHotSpot D$edi-10                     ; x/y hot spot
    push edi

    Mov esi D$OtherFilePtr | add esi 016
    sub D$OtherFileLen 012 | Mov ecx D$OtherFileLen
    Mov eax D$CursorHotSpot                           ; PE cursors need the hot spot in data:
    Mov edi D$TempoCursorMem | stosd | rep movsb

    VirtualFree D$OtherFilePtr

    move D$OtherFilePtr D$TempoCursorMem

    Call AskForResID | Call CloseOtherFilesRead

    Mov esi D$OtherListPtr | lodsd                    ; rewrite ID to GroupCursor:
    Mov edi D$GroupCursorListPtr | stosd
    pop edi
    Mov W$edi-2 ax                           ; write the ID in RT_GROUP_CURSOR records
    add D$edi-6 4                            ; +4 size because upper added hot Spot in data

    add D$CursorListPtr 12 | add D$GroupCursorListPtr 12
ret


[TempoIconMem: ?]

ReadIcon:
    Mov D$OtherFilesFilters IconFilesFilters
    Mov D$OpenOtherFileTitle IconFilesTitle
    Mov D$OtherList IconList | move D$OtherListPtr D$IconListPtr

    move D$OtherhwndFileOwner D$H.MainWindow, D$OtherhInstance D$H.Instance

    Mov edi OtherSaveFilter, ecx 260, eax 0 | rep stosd
    Call 'Comdlg32.GetOpenFileNameA' OtherOpenStruc
        If D$OtherSaveFilter = 0
            Mov D$OtherList 0,  D$OtherListPtr 0 | ret
        End_If

    Call 'KERNEL32.CreateFileA' OtherSaveFilter &GENERIC_READ,
                                &FILE_SHARE_READ, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0

    If eax = &INVALID_HANDLE_VALUE

        Call MessageBox argh,
                        D$BusyFilePtr,
                        &MB_SYSTEMMODAL+&MB_USERICON

        ret  ; return to caller of caller

    Else
      Mov D$OtherSourceHandle eax
    End_If

    Call 'KERNEL32.GetFileSize'  eax 0
    Mov D$OtherFileLen eax
    VirtualAlloc OtherFilePtr eax

    Mov D$NumberOfReadBytes 0
    Call 'KERNEL32.ReadFile' D$OtherSourceHandle D$OtherFilePtr,
                            D$OtherFileLen NumberOfReadBytes 0

    Mov eax D$OtherFilePtr
    If D$eax+2 <> 010001

        Call MessageBox Argh,
                        BadIcoFile,
                        &MB_SYSTEMMODAL+&MB_USERICON

        VirtualFree D$OtherFilePtr | ret

    End_If

  ; We write both the RT_ICON and the RT_GROUP_ICON:
    VirtualAlloc TempoIconMem D$OtherFileLen
    VirtualAlloc TempoMemPointer 20
    Mov edi D$GroupIconListPtr, eax D$TempoMemPointer, D$edi+4 eax, D$edi+8 20

    Mov edi eax                                       ; edi > RT_GROUP_ICON mem

    Mov esi D$OtherFilePtr

    Mov ecx 20 | rep movsb
    push edi

    Mov esi D$OtherFilePtr | add esi 016
    sub D$OtherFileLen 012 | Mov ecx D$OtherFileLen
    Mov edi D$TempoIconMem | rep movsb

    VirtualFree D$OtherFilePtr

    move D$OtherFilePtr D$TempoIconMem

    Mov B$OnIconLoad &TRUE
        Call AskForResID | Call CloseOtherFilesRead
    Mov B$OnIconLoad &FALSE

    Mov esi D$OtherListPtr | lodsd                    ; rewrite ID to GroupCursor:
    Mov edi D$GroupIconListPtr | stosd
    pop edi
    Mov W$edi-2 ax                           ; write the ID in RT_GROUP_CURSOR records
    add D$edi-6 4                            ; +4 size because upper added hot Spot in data

    add D$IconListPtr 12 | add D$GroupIconListPtr 12
ret


[AnimateHandle: 0    TempoAviFileHandle: 0]

[AnimateClass: B$ "SysAnimate32" EOS]
[TempoAviFile: B$ "Avi$$$.avi" EOS]
[KillAvi: B$ "Delete this Avi Resource ?" EOS]
[NoAvi: B$ "No Avi Resources in this PE"  EOS]

[EmptyDialog: D$ 0900408C2 0    ; Style
 U$ 0 0 0 0DC 0C8              ; Dim
 0                             ;      no Menu
 '' 0                          ; Class
 '' 0                          ; Title
 08 'Helv' 0]                  ; Font

[EmptyDialogHandle: ?]

Proc EmptyProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    If D@msg = &WM_INITDIALOG
        Call SetIconDialog
        Mov eax &TRUE

    Else_If D@msg = &WM_CLOSE
        Mov B$OkDialogFlag &TRUE
       ; Call 'User32.EndDialog' D$ChoiceDialogBoxHandle 0
        Call 'User32.DestroyWindow' D@hwnd

    Else
        popad | Mov eax &FALSE | jmp L9>

    End_If

    popad | Mov eax &TRUE

L9: EndP


DeleteAviFile:
    If D$AviList = 0

        Call MessageBox Argh,
                        NoAvi,
                        &MB_SYSTEMMODAL+&MB_USERICON

        ret

    End_If

    Call 'USER32.CreateDialogIndirectParamA' D$H.Instance EmptyDialog &NULL Emptyproc 0
    Mov D$EmptyDialogHandle eax

    Call 'USER32.CreateWindowExA' &WS_SIZEBOX__&WS_DLGFRAME  AnimateClass &NULL,  ; &WS_Border  &WS_BORDER
                                 &ACS_AUTOPLAY__&WS_VISIBLE__&WS_CHILD__&ACS_CENTER,
                                 4 4 200 100 D$EmptyDialogHandle,
                                 0 D$H.Instance 0
    Mov D$AnimateHandle eax

    Mov D$WhatDialogListPtr AviList,  D$OkDialogFlag &FALSE
    add D$WhatDialogListPtr 4

    .While B$OkDialogFlag = &FALSE
        Call 'KERNEL32.CreateFileA' TempoAviFile &GENERIC_WRITE, 0, 0,
                                    &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0

        Mov D$TempoAviFileHandle eax
        Mov esi D$WhatDialogListPtr | Mov ecx D$esi+4
        Call 'KERNEL32.WriteFile' D$TempoAviFileHandle D$esi ecx NumberOfReadBytes &NULL
        Call 'Kernel32.CloseHandle' D$TempoAviFileHandle
        Call 'USER32.SendMessageA' D$AnimateHandle &ACM_OPEN  &NULL TempoAviFile
        Call SetNextChoiceID
        Call 'User32.DialogBoxIndirectParamA' D$H.Instance, ChoiceBar, D$H.MainWindow, ChoiceDialogBoxProc, AviList
        Call 'USER32.SendMessageA' D$AnimateHandle &ACM_STOP  &NULL &NULL
        .If D$OkDialogFlag = &VK_ESCAPE
            jmp L9>>
        .Else_If D$WhatDialogListPtr < AviList
            add D$WhatDialogListPtr 12
            Call SetNextChoiceID
        .Else
            Mov esi D$WhatDialogListPtr
            If D$esi = 0
              sub D$WhatDialogListPtr 12 | Mov esi D$WhatDialogListPtr
              Call SetNextChoiceID
            End_If
        .End_If
    .End_While

    Call MessageBox Sure,
                    KillAvi,
                    &MB_SYSTEMMODAL+&MB_ICONQUESTION+&MB_YESNO

    If D$FL.MsgBoxReturn = &IDYES
        sub D$WhatDialogListPtr 4
        Mov esi D$WhatDialogListPtr, edi esi | add esi 12
        Mov eax D$WhatDialogListPtr | sub eax AviList | shr eax 2 | Mov ecx 300 | sub ecx eax
        rep movsd
        sub D$AviListPtr 12
    End_If

L9: Call 'USER32.DestroyWindow' D$AnimateHandle
    Call 'User32.EndDialog' D$EmptyDialogHandle &NULL
    Call 'KERNEL32.DeleteFileA' TempoAviFile
ret


[NoCursor: B$ "No Cursor Resources in this PE" EOS]
[KillCursor: B$ "Delete this Cursor ?" EOS]
[KillIcon: B$ "Delete this Icon ?" EOS]
[TempoCursorFile: B$ "Cursor.$$$" EOS]
[CurDataPtr: W$ 0 D$ 016]

[UserCursorHandle: ?    TempoCursorFileHandle: ?]

DeleteCursor:
    If D$CursorList = 0

        Call MessageBox Argh,
                        NoCursor,
                        &MB_SYSTEMMODAL&MB_USERICON

        ret

    End_If

    Mov D$WhatDialogListPtr CursorList,  D$OkDialogFlag &FALSE, D$EmptyDialogHandle 0
    add D$WhatDialogListPtr 4

    .While B$OkDialogFlag = &FALSE
        On D$EmptyDialogHandle <> 0, Call 'User32.EndDialog' D$EmptyDialogHandle &NULL
        Call 'USER32.CreateDialogIndirectParamA' D$H.Instance EmptyDialog &NULL Emptyproc 0
            Mov D$EmptyDialogHandle eax
        Call 'KERNEL32.CreateFileA' TempoCursorFile &GENERIC_WRITE, 0, 0,
                                    &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
        Mov D$TempoCursorFileHandle eax
        Mov eax D$WhatDialogListPtr | sub eax CursorList | add eax GroupCursorList
        Mov ecx D$eax | sub D$ecx+14 4 | Mov ecx D$ecx+14  ; how much -4 for added Hot Spot
        push eax, ecx
            Call 'KERNEL32.WriteFile' D$TempoCursorFileHandle D$eax 010 NumberOfReadBytes &NULL
            Call 'KERNEL32.WriteFile' D$TempoCursorFileHandle CurDataPtr 6 NumberOfReadBytes &NULL
            Mov esi D$WhatDialogListPtr, esi D$esi | add esi 4
        pop ecx, eax
        Mov eax D$eax | add D$eax+14 4                     ; restore our 'how much'
        Call 'KERNEL32.WriteFile' D$TempoCursorFileHandle esi ecx NumberOfReadBytes &NULL
        Call 'Kernel32.CloseHandle' D$TempoCursorFileHandle
        Call 'User32.LoadCursorFromFileA' TempoCursorFile
        If D$UserCursorHandle > 0
            pushad
               Call 'USER32.DestroyCursor' D$UserCursorHandle
            popad
        End_If
        Mov D$UserCursorHandle eax
            Call 'USER32.GetDC' D$EmptyDialogHandle
            push eax
                Call 'User32.DrawIcon' eax 10 10 D$UserCursorHandle
            pop eax
            Call 'USER32.ReleaseDC' D$EmptyDialogHandle eax
            Call SetNextChoiceID
        Call 'User32.DialogBoxIndirectParamA' D$H.Instance, ChoiceBar,
                                             D$H.MainWindow, ChoiceDialogBoxProc, CursorList

        .If D$OkDialogFlag = &VK_ESCAPE
            jmp L9>>
        .Else_If D$WhatDialogListPtr < CursorList
            add D$WhatDialogListPtr 12
            Call SetNextChoiceID
        .Else
            Mov esi D$WhatDialogListPtr
            If D$esi = 0
              sub D$WhatDialogListPtr 12
              Call SetNextChoiceID
            End_If
        .End_If
   .End_While

   Call MessageBox Sure,
                   KillCursor,
                   &MB_SYSTEMMODAL+&MB_ICONQUESTION+&MB_YESNO


   If D$FL.MsgBoxReturn = &IDYES
       sub D$WhatDialogListPtr 4
       Mov esi D$WhatDialogListPtr, edi esi | add esi 12
       Mov ebx D$edi                                                ; ebx = ID
       Mov eax D$WhatDialogListPtr | sub eax CursorList | shr eax 2
       Mov ecx MAXCURSOR | sub ecx eax | rep movsd
       sub D$CursorListPtr 12

       Mov esi GroupCursorList, ecx MAXCURSOR
       While D$esi <> ebx
           add esi 12 | sub ecx 3
       End_While
       Mov edi esi | add esi 12  | rep movsd
       sub D$GroupCursorListPtr 12
   End_If

L9: Call 'User32.EndDialog' D$EmptyDialogHandle &NULL
    Call 'KERNEL32.DeleteFileA' TempoCursorFile
ret


[UserIconHandle: ?]

[ICONINFO:
 ICONINFO_fIcon: D$ &TRUE
 ICONINFO_xHotspot: D$ 0
 ICONINFO_yHotspot: D$ 0
 ICONINFO_hbmMask: D$ 0
 ICONINFO_hbmColor: D$ 0]


DeleteIcon:
    If D$IconList = 0

       Call MessageBox Argh,
                       NoIcon,
                       &MB_SYSTEMMODAL+&MB_USERICON

        ret

    End_If

    Mov D$WhatDialogListPtr IconList,  D$OkDialogFlag &FALSE, D$EmptyDialogHandle 0
    add D$WhatDialogListPtr 4

   ; ID / Pointer / Size.

    .While B$OkDialogFlag = &FALSE
        On D$EmptyDialogHandle <> 0, Call 'User32.EndDialog' D$EmptyDialogHandle &NULL
        Call 'USER32.CreateDialogIndirectParamA' D$H.Instance EmptyDialog &NULL Emptyproc 0
            Mov D$EmptyDialogHandle eax
        Call 'KERNEL32.CreateFileA' TempoCursorFile &GENERIC_WRITE, 0, 0,
                                    &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
        Mov D$TempoCursorFileHandle eax
        Mov eax D$WhatDialogListPtr | sub eax IconList | add eax GroupIconList

      ; May hang down there here with an ecx = 0. So, temporary:
        On D$eax = 0, jmp L9>>

        Mov ecx D$eax | Mov ecx D$ecx+14
        push ecx
            Call 'KERNEL32.WriteFile' D$TempoCursorFileHandle D$eax 010 NumberOfReadBytes &NULL
            Call 'KERNEL32.WriteFile' D$TempoCursorFileHandle CurDataPtr 6 NumberOfReadBytes &NULL
            Mov esi D$WhatDialogListPtr
        pop ecx
        Call 'KERNEL32.WriteFile' D$TempoCursorFileHandle D$esi D$esi+4 NumberOfReadBytes &NULL
        Call 'Kernel32.CloseHandle' D$TempoCursorFileHandle
        Call 'User32.LoadCursorFromFileA' TempoCursorFile
        If D$UserCursorHandle > 0
            pushad
               Call 'USER32.DestroyCursor' D$UserCursorHandle
            popad
        End_If
        Mov D$UserCursorHandle eax
            Call 'USER32.GetDC' D$EmptyDialogHandle
            push eax
                Call 'User32.DrawIcon' eax 10 10 D$UserCursorHandle
            pop eax
            Call 'USER32.ReleaseDC' D$EmptyDialogHandle eax
            Call SetNextChoiceID

        Call 'User32.DialogBoxIndirectParamA' D$H.Instance, ChoiceBar,
                                             D$H.MainWindow, ChoiceDialogBoxProc, IconList

        .If D$OkDialogFlag = &VK_ESCAPE
            jmp L9>>
        .Else_If D$WhatDialogListPtr < IconList
            add D$WhatDialogListPtr 12
            Call SetNextChoiceID
        .Else
            Mov esi D$WhatDialogListPtr
            If D$esi = 0
              sub D$WhatDialogListPtr 12
              Call SetNextChoiceID
            End_If
        .End_If
   .End_While

    Call MessageBox Sure,
                    KillIcon,
                    &MB_SYSTEMMODAL+&MB_ICONQUESTION+&MB_YESNO

    If D$FL.MsgBoxReturn = &IDYES
        sub D$WhatDialogListPtr 4
        Mov esi D$WhatDialogListPtr, edi esi | add esi 12
        Mov ebx D$edi                                                ; ebx = ID
        Mov eax D$WhatDialogListPtr | sub eax IconList | shr eax 2
        Mov ecx MAXICON | sub ecx eax | rep movsd
        sub D$IconListPtr 12

        Mov esi GroupIconList, ecx MAXICON
        While D$esi <> ebx
            add esi 12 | sub ecx 3
        End_While
        Mov edi esi | add esi 12  | rep movsd
        sub D$GroupIconListPtr 12
    End_If

L9: Call 'User32.EndDialog' D$EmptyDialogHandle &NULL
    Call 'KERNEL32.DeleteFileA' TempoCursorFile
ret


ReadOtherFile:
    move D$OtherhwndFileOwner D$H.MainWindow, D$OtherhInstance D$H.Instance

    Mov edi OtherSaveFilter, ecx 260, eax 0 | rep stosd
    Call 'Comdlg32.GetOpenFileNameA' OtherOpenStruc
      On D$OtherSaveFilter = 0,  ret

  ; Loading the entire file in memory:
    On D$OtherSourceHandle > 0, Call 'KERNEL32.CloseHandle' D$OtherSourceHandle
    Mov D$OtherSourceHandle 0

    Call 'KERNEL32.CreateFileA' OtherSaveFilter, &GENERIC_READ,
                                &FILE_SHARE_READ, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE

        Call MessageBox argh,
                        D$BusyFilePtr,
                        &MB_SYSTEMMODAL+&MB_USERICON

        ret  ; return to caller of caller

    Else
        Mov D$OtherSourceHandle eax
    End_If

    Call 'KERNEL32.GetFileSize'  eax 0 | Mov D$OtherFileLen eax

    VirtualAlloc OtherFilePtr eax

    Mov D$NumberOfReadBytes 0
    Call 'KERNEL32.ReadFile' D$OtherSourceHandle, D$OtherFilePtr,
                            D$OtherFileLen, NumberOfReadBytes 0
ret


CloseOtherFilesRead:
    On D$OtherSourceHandle > 0, Call 'KERNEL32.CloseHandle' D$OtherSourceHandle
    Mov D$OtherSourceHandle 0
ret



AskForResID:
    Call 'USER32.DialogBoxIndirectParamA' D$H.Instance, OtherIdTemplate, 0, OtherIDProc, 0
ret


[OtherEditHandle: 0    OtherID: '                   ', 0]

[OnIconLoad: ?]

Proc OtherIDProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    .If D@msg = &WM_COMMAND
       ..If D@wParam = &IDCANCEL
            Call WM_CLOSE
       ..Else_If D@wParam = &IDOK
           Call 'User32.GetDlgItem' D@hwnd 3 | Mov D$OtherEditHandle eax
           Call 'User32.SendMessageA' D$OtherEditHandle &WM_GETTEXTLENGTH 0 0 | inc eax
           Call 'User32.SendMessageA' D$OtherEditHandle &WM_GETTEXT eax OtherID
           TranslateAsciiToDword OtherID
           If eax > 0FFFF

                Call MessageBox argh,
                                D$IdTooBigPtr,
                                &MB_SYSTEMMODAL+&MB_USERICON

            Else_If eax < 1

                Call MessageBox argh,
                                D$IdTooSmallPtr,
                                &MB_SYSTEMMODAL+&MB_USERICON

           Else
             Mov edi D$OtherListPtr | stosd
             Mov eax D$OtherFilePtr | stosd
             Mov eax D$OtherFileLen | stosd
             Call WM_CLOSE
           End_If
       ..End_If

    .Else_If D@msg = &WM_INITDIALOG
        Call SetIconDialog
        Call 'User32.GetDlgItem' D@hwnd 3
        Call 'User32.SendMessageA' eax &EM_SETLIMITTEXT 5  0
           Mov esi D$OtherListPtr | On esi > D$OtherList, sub esi 12
           If D$esi = 0
             Mov eax 1
             On B$OnIconLoad = &TRUE, inc eax
           Else
             lodsd | inc eax
           End_If
           Call 'USER32.SetDlgItemInt' D@hwnd 3 eax 0
    .Else
       popad | Mov eax &FALSE | jmp L9>

    .End_If

    popad | Mov eax &TRUE

L9: EndP


[OtherIdTemplate: D$ 090C408C2 0    ; Style
 U$ 03 0 0 0B9 018             ; Dim
 0                             ; no Menu
 '' 0                          ; Class
 'What ID number for new Resource?' 0 ; Title
 08 'Helv' 0]                  ; Font

[OID0: D$ 050000000 0         ; Style
 U$ 07E 03 038 013             ; Dim
 01                            ; ID
 0FFFF 080                     ; Class
 'OK' 0                        ; Title
 0]                            ; No creation data

[OID1: D$ 050000000 0         ; Style
 U$ 03 03 038 013              ; Dim
 02                            ; ID
 0FFFF 080                     ; Class
 'Abort' 0                     ; Title
 0]                            ; No creation data

[OID2: D$ 050802000 0         ; Style
 U$ 040 05 038 0F              ; Dim
 03                            ; ID
 0FFFF 081                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data


