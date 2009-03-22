TITLE Loaders         ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________
________________________________________________________________________________________
________________________________________________________________________________________

                         ; Loaders for non-edited Resources
________________________________________________________________________________________
________________________________________________________________________________________

; read new open PE Resources:

[ResourcePointersSecurity: D$ ? # 10]

; Each Record of these Lists is: ID / Ptr / Size.

[CursorList: D$ ? # MAXCURSOR]
[CursorListPtr: D$ CursorList]
[GroupCursorList: D$ ? # MAXCURSOR]
[GroupCursorListPtr: D$ GroupCursorList]
[IconList: D$ ? # MAXICON]
[IconListPtr: D$ IconList]
[MenuList: D$ ? # MAXMENU]
[MenuListPtr: D$ MenuList]
[DialogList: D$ ? # MAXDIALOG]
[DialogListPtr: D$ DialogList]
[StringsList: D$ ? # MAXSTRINGS]
[StringsListPtr: D$ StringsList]
[GroupIconList: D$ ? # MAXICON]
[GroupIconListPtr: D$ GroupIconList]
[WaveList: D$ ? # MAXWAVE]
[WaveListPtr: D$ WaveList]
[AviList: D$ ? # MAXAVI]
[AviListPtr: D$ AviList]
[RCdataList: D$ ? # MAXRCDATA]
[RCdataListPtr: D$ RCdataList]
[BitMapList: D$ ? # MAXBITMAP]
[BitMapListPtr: D$ BitMapList]

[OtherList: D$ ?
 OtherListPtr: D$ ?]

[WaveFilesFilters: B$ 'Wave Files' EOS '*.wav' EOS 0
 WaveFilesTitle: B$ 'Chose a Wave File' EOS
 AviFilesFilters: B$ 'Avi Files' EOS   '*.avi' EOS 0
 AviFilesTitle: B$ 'Choose an Avi File' EOS
 RCDataFilesFilters: B$ 'AnyThing' EOS  '*.*' EOS 0
 RCDataFilesTitle: B$ 'Choose a File' EOS
 CursorFilesFilters: B$ 'Cursor Files' EOS   '*.cur' EOS 0
 CursorFilesTitle: B$ 'Choose a Cursor File' EOS
 IconFilesFilters: B$ 'Icon Files' EOS   '*.Ico' EOS 0
 IconFilesTitle: B$ 'Choose an Icon File' EOS
 DialogFilesFilters: B$ 'Dlg Files' EOS, '*.dlg' EOS, 0
 BinDialogFilesFilters: B$ 'bdf Files' EOS, '*.bdf' EOS, 0
 BinMenuFilesFilters: B$ 'bmf Files' EOS, '*.bmf' EOS, 0
 DialogFilesTitle: B$ 'Choose a Dialog Template File' EOS]

[OtherChoosenFile: B$ ? # &MAX_PATH]
[OtherFileFilter: B$ ? # &MAX_PATH]
[OtherSourceFilterPtr: B$ ? # &MAX_PATH]
[OtherSaveFilter: B$ ? # &MAX_PATH]

[OtherOpenStruc: len
 OtherhwndFileOwner: D$ 0
 OtherhInstance: D$ 0
 OtherFilesFilters: D$ 0
 D$ OtherFileFilter
 D$ &MAX_PATH
 D$ OtherSourceFilterPtr
 D$ OtherSaveFilter
 D$ &MAX_PATH
 D$ OtherChoosenFile
 D$ &MAX_PATH
 D$ 0
 OpenOtherFileTitle: D$ 0
 @OtherFlags: D$  0281804 ; for read 0280006 : for write !!! WinEquates !!!
 D$ 0  0  0  0  0]

[H.OtherSource: D$ ?
 OtherFileLen: D$ ?
 OtherFilePtr: D$ ?]

ReadRCData:
    Mov D$OtherFilesFilters RCDataFilesFilters
    Mov D$OpenOtherFileTitle RCDataFilesTitle
    Mov D$OtherList RcDataList | Move D$OtherListPtr D$RCDataListPtr
    Call ReadOtherFile
        If D$OtherSaveFilter = 0
            Mov D$OtherList 0,  D$OtherListPtr 0 | ret
        End_If
        Call AskForResID | add D$RCDataListPtr 12
    Call CloseOtherFilesRead
ret


SaveRcData:
    If D$RCDataList = 0
        Call 'USER32.MessageBoxA' D$H.MainWindow, NoRcData, Argh, &MB_OK+&MB_SYSTEMMODAL | ret
    End_If

    Mov D$WhatDialogListPtr RcDataList,  D$OkDialogFlag &FALSE
    add D$WhatDialogListPtr 4

    Call 'USER32.CreateDialogParamA' D$H.Instance, 1000, &NULL, EmptyProc, &NULL
    Mov D$H.EmptyDialog eax


    .While B$OkDialogFlag = &FALSE
        Call SetRcDataTitle | Call ShowHexa
        Call SetNextChoiceID
        Call 'USER32.DialogBoxIndirectParamA' D$H.Instance, ChoiceBar, D$H.MainWindow,
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

    Call 'USER32.MessageBoxA' D$H.MainWindow, SaveRc, Sure, &MB_ICONQUESTION+&MB_YESNO+&MB_SYSTEMMODAL

    Push eax
        Call 'USER32.EndDialog' D$H.EmptyDialog &NULL
    Pop eax

   .If eax = &IDYES
        Mov D$OpenDlg@lpstrFilter RcFilesFilters
        Call 'Comdlg32.GetSaveFileNameA' OpenDlg
        Mov D$OpenDlg@lpstrFilter DlgFilesFilters
        On eax = &FALSE, ret

        Call 'KERNEL32.CreateFileA' SaveDlgFilter &GENERIC_WRITE,
                                &FILE_SHARE_READ__&FILE_SHARE_WRITE, 0,
                                &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0

        If eax = &INVALID_HANDLE_VALUE

            Call MessageBox D$BusyFilePtr

ret

        End_If

        Mov D$H.Destination eax, D$NumberOfReadBytes 0
        Mov esi D$WhatDialogListPtr, ecx D$esi+4, esi D$esi

        Call 'KERNEL32.WriteFile' D$H.Destination, esi, ecx, NumberOfReadBytes  0

        Call 'KERNEL32.CloseHandle' D$H.Destination | Mov D$H.Destination 0
   .End_If

L9: ret


[NoRcData: B$ 'No RC Data Resources in this PE' EOS]
[KillRc: B$ 'Delete this Rc Data Resource?' EOS]
[SaveRc: B$ 'Save this RC Resource to Disk?' EOS]
[IdTitle: B$ 'RC Data ID: ' IdTitleID: B$ '       ' EOS]

SetRcDataTitle:
    Mov eax D$WhatDialogListPtr, eax D$eax-4, edi IdTitleID
    Call TransDwordToAsciiDecimal
    Call 'USER32.SendMessageA' D$H.EmptyDialog &WM_SETTEXT 0 IdTitle
ret


ShowHexa:
    Mov eax D$WhatDialogListPtr, esi D$eax, ecx D$eax+4
    shl ecx 3 | On ecx > 0FF00, Mov ecx 0FF00

    Call VirtualAlloc TempoMemPointer,
                      ecx

    Mov edi D$TempoMemPointer

    shr ecx 3 | sub ecx 001111 | or ecx 001111
    Push edi
L0:     lodsb | shr al 4 | add al '0' | On al > '9', add al 7 | stosb
        dec esi | lodsb | and al 0F | add al '0' | On al > '9', add al 7 | stosb
        Mov al SPC | stosb
        test ecx 00111 NOT_ZERO L1>
            Mov al SPC | stosb
        test ecx 001111 NOT_ZERO L1>
           sub esi 16
           Push ecx
               Mov ecx 16
T0:            lodsb | On al < SPC, Mov al '.' | stosb | loop T0<
           Pop ecx
           Mov ax 0A0D | stosw
L1:     loop L0<
    Pop edi

    Call 'USER32.SetDlgItemTextA' D$H.EmptyDialog, 100, edi
ret


DeleteRcData:
    If D$RCDataList = 0
        Call 'USER32.MessageBoxA' D$H.MainWindow, NoRcData, Argh, &MB_OK+&MB_SYSTEMMODAL | ret
    End_If

    Mov D$WhatDialogListPtr RcDataList,  D$OkDialogFlag &FALSE
    add D$WhatDialogListPtr 4
; Tag Dialog 1000
    Call 'USER32.CreateDialogParamA' D$H.Instance, 1000, &NULL, EmptyProc, &NULL
    Mov D$H.EmptyDialog eax

    .While B$OkDialogFlag = &FALSE
        Call SetRcDataTitle | Call ShowHexa
        Call SetNextChoiceID
        Call 'USER32.DialogBoxIndirectParamA' D$H.Instance, ChoiceBar, D$H.MainWindow, ChoiceDialogBoxProc, RcDataList

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

   Call 'USER32.MessageBoxA' D$H.MainWindow, KillRc, Sure, &MB_ICONQUESTION+&MB_YESNO+&MB_SYSTEMMODAL
   If eax = &IDYES
        sub D$WhatDialogListPtr 4
        Mov esi D$WhatDialogListPtr, edi esi | add esi 12
        Mov eax D$WhatDialogListPtr | sub eax RCDataList | shr eax 2 | Mov ecx 300 | sub ecx eax
        rep movsd
        sub D$RCDataListPtr 12
   End_If

L9: Call 'USER32.EndDialog' D$H.EmptyDialog &NULL
ret


ReadWaveFile:
    Mov D$OtherFilesFilters WaveFilesFilters
    Mov D$OpenOtherFileTitle WaveFilesTitle
    Mov D$OtherList WaveList | Move D$OtherListPtr D$WaveListPtr
    Call ReadOtherFile
        If D$OtherSaveFilter = 0
            Mov D$OtherList 0,  D$OtherListPtr 0 | ret
        End_If
        Call AskForResID | add D$WaveListPtr 12
    Call CloseOtherFilesRead
ret


[KillWave: B$ 'Delete this Wave Resource?' EOS]
[NoWave: B$ 'No Wave Resources in this PE' EOS]
[H.TempoWaveFile: D$ ?]
[TempoWaveFile: B$ 'Wave$$$.Wav' EOS]

DeleteWave:
    If D$WaveList = 0
        Call 'USER32.MessageBoxA' D$H.MainWindow, NoWave, Argh, &MB_OK+&MB_SYSTEMMODAL | ret
    End_If

    Mov D$WhatDialogListPtr WaveList,  D$OkDialogFlag &FALSE
    add D$WhatDialogListPtr 4

    .While B$OkDialogFlag = &FALSE
        Call 'KERNEL32.CreateFileA' TempoWaveFile &GENERIC_WRITE, 0, 0,
                                    &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
        Mov D$H.TempoWaveFile eax
        Mov esi D$WhatDialogListPtr
        Call 'KERNEL32.WriteFile' D$H.TempoWaveFile D$esi D$esi+4 NumberOfReadBytes &NULL
        Call 'KERNEL32.CloseHandle' D$H.TempoWaveFile
        Call 'WINMM.PlaySound' TempoWaveFile &NULL  &SND_ASYNC__&SND_FILENAME
        Call SetNextChoiceID
        Call 'USER32.DialogBoxIndirectParamA' D$H.Instance, ChoiceBar, D$H.MainWindow, ChoiceDialogBoxProc, WaveList
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

    Call 'USER32.MessageBoxA' D$H.MainWindow, KillWave, Sure, &MB_ICONQUESTION+&MB_YESNO+&MB_SYSTEMMODAL
    If eax = &IDYES
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
    Mov D$OtherList AviList | Move D$OtherListPtr D$AviListPtr
    Call ReadOtherFile
        If D$OtherSaveFilter = 0
            Mov D$OtherList 0,  D$OtherListPtr 0 | ret
        End_If

        Call AskForResID | add D$AviListPtr 12
    Call CloseOtherFilesRead
ret


[TempoCursorMem: D$ ?
 CursorHotSpot: D$ ?]
[BadCurFile: B$ 'Bad or multiple Cursor(s) file' EOS]
[BadIcoFile: B$ 'Bad or multiple Icon(s) file' EOS]

ReadCursor:
    Mov D$OtherFilesFilters CursorFilesFilters
    Mov D$OpenOtherFileTitle CursorFilesTitle
    Mov D$OtherList CursorList | Move D$OtherListPtr D$CursorListPtr

    Move D$OtherhwndFileOwner D$H.MainWindow, D$OtherhInstance D$H.Instance

    Mov edi OtherSaveFilter, ecx 260, eax 0 | rep stosd
    Call 'Comdlg32.GetOpenFileNameA' OtherOpenStruc
        If D$OtherSaveFilter = 0
            Mov D$OtherList 0,  D$OtherListPtr 0 | ret
        End_If

    Call 'KERNEL32.CreateFileA' OtherSaveFilter &GENERIC_READ,
                                &FILE_SHARE_READ+&FILE_SHARE_WRITE, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE

      Call MessageBox D$BusyFilePtr

ret  ; return to caller of caller

    Else
      Mov D$H.OtherSource eax
    End_If

    Call 'KERNEL32.GetFileSize'  eax 0
    Mov D$OtherFileLen eax

    Call VirtualAlloc OtherFilePtr,
                      eax

    Call 'KERNEL32.ReadFile' D$H.OtherSource D$OtherFilePtr,
                            eax NumberOfReadBytes 0

    Mov eax D$OtherFilePtr
    If D$eax+2 <> 010002
        Call 'USER32.MessageBoxA' D$H.MainWindow, BadCurFile, Argh, &MB_OK+&MB_SYSTEMMODAL

        Call VirtualFree OtherFilePtr

ret

    End_If

  ; We write both the RT_CURSOR and the RT_GROUP_CURSOR:
    Call VirtualAlloc TempoCursorMem,
                      D$OtherFileLen

    Call VirtualAlloc TempoMemPointer,
                      20

    Mov edi D$GroupCursorListPtr, eax D$TempoMemPointer, D$edi+4 eax, D$edi+8 20

    Mov edi eax                                       ; edi > RT_GROUP_CURSOR mem

    Mov esi D$OtherFilePtr

    Mov ecx 20 | rep movsb
    Move D$CursorHotSpot D$edi-10                     ; x/y hot spot
    Push edi

    Mov esi D$OtherFilePtr | add esi 016
    sub D$OtherFileLen 012 | Mov ecx D$OtherFileLen
    Mov eax D$CursorHotSpot                           ; PE cursors need the hot spot in data:
    Mov edi D$TempoCursorMem | stosd | rep movsb

    Call VirtualFree OtherFilePtr

    Move D$OtherFilePtr D$TempoCursorMem

    Call AskForResID | Call CloseOtherFilesRead

    Mov esi D$OtherListPtr | lodsd                    ; rewrite ID to GroupCursor:
    Mov edi D$GroupCursorListPtr | stosd
    Pop edi
    Mov W$edi-2 ax                           ; write the ID in RT_GROUP_CURSOR records
    add D$edi-6 4                            ; +4 size because upper added hot Spot in data

    add D$CursorListPtr 12 | add D$GroupCursorListPtr 12
ret


[TempoIconMem: D$ ?]

ReadIcon:
    Mov D$OtherFilesFilters IconFilesFilters
    Mov D$OpenOtherFileTitle IconFilesTitle
    Mov D$OtherList IconList | Move D$OtherListPtr D$IconListPtr

    Move D$OtherhwndFileOwner D$H.MainWindow, D$OtherhInstance D$H.Instance

    Mov edi OtherSaveFilter, ecx 260, eax 0 | rep stosd
    Call 'Comdlg32.GetOpenFileNameA' OtherOpenStruc
        If D$OtherSaveFilter = 0
            Mov D$OtherList 0,  D$OtherListPtr 0 | ret
        End_If

    Call 'KERNEL32.CreateFileA' OtherSaveFilter &GENERIC_READ,
                                &FILE_SHARE_READ+&FILE_SHARE_WRITE, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE

      Call MessageBox D$BusyFilePtr

ret  ; return to caller of caller

    Else
      Mov D$H.OtherSource eax
    End_If

    Call 'KERNEL32.GetFileSize'  eax 0
    Mov D$OtherFileLen eax

    Call VirtualAlloc OtherFilePtr,
                      eax


    Call 'KERNEL32.ReadFile' D$H.OtherSource D$OtherFilePtr,
                            eax NumberOfReadBytes 0

    Mov eax D$OtherFilePtr
    If D$eax+2 <> 010001
        Call 'USER32.MessageBoxA' D$H.MainWindow, BadIcoFile, Argh, &MB_OK+&MB_SYSTEMMODAL

        Call VirtualFree OtherFilePtr

ret

    End_If

  ; We write both the RT_ICON and the RT_GROUP_ICON:
    Call VirtualAlloc TempoIconMem,
                      D$OtherFileLen

    Call VirtualAlloc TempoMemPointer,
                      20

    Mov edi D$GroupIconListPtr, eax D$TempoMemPointer, D$edi+4 eax, D$edi+8 20

    Mov edi eax                                       ; edi > RT_GROUP_ICON mem

    Mov esi D$OtherFilePtr

    Mov ecx 20 | rep movsb
    Push edi

    Mov esi D$OtherFilePtr | add esi 016
    sub D$OtherFileLen 012 | Mov ecx D$OtherFileLen
    Mov edi D$TempoIconMem | rep movsb

    Call VirtualFree OtherFilePtr

    Move D$OtherFilePtr D$TempoIconMem

    Mov B$OnIconLoad &TRUE
        Call AskForResID | Call CloseOtherFilesRead
    Mov B$OnIconLoad &FALSE

    Mov esi D$OtherListPtr | lodsd                    ; rewrite ID to GroupCursor:
    Mov edi D$GroupIconListPtr | stosd
    Pop edi
    Mov W$edi-2 ax                           ; write the ID in RT_GROUP_CURSOR records
    add D$edi-6 4                            ; +4 size because upper added hot Spot in data

    add D$IconListPtr 12 | add D$GroupIconListPtr 12
ret


[H.Animate: D$ ?
 H.TempoAviFile: D$ ?]
[AnimateClass: B$ 'SysAnimate32' EOS]
[TempoAviFile: B$ 'Avi$$$.avi' EOS]
[KillAvi: B$ 'Delete this Avi Resource?' EOS]
[NoAvi: B$ 'No Avi Resources in this PE' EOS]

[EmptyDialog: D$ 0900408C2 0    ; Style
 U$ 0 0 0 0DC 0C8              ; Dim
 0                             ;      no Menu
 '' 0                          ; Class
 '' 0                          ; Title
 08 'Helv' 0]                  ; Font

[H.EmptyDialog: D$ ?]

Proc EmptyProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    If D@msg = &WM_INITDIALOG
        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$STRUC.WINDOWCLASS@hIcon
        Mov eax &TRUE

    Else_If D@msg = &WM_CLOSE
        Mov B$OkDialogFlag &TRUE
       ; Call 'USER32.EndDialog' D$H.ChoiceDialogBox 0
        Call 'USER32.DestroyWindow' D@hwnd

    Else
        popad | Mov eax &FALSE | jmp L9>

    End_If

    popad | Mov eax &TRUE

L9: EndP


DeleteAviFile:
    If D$AviList = 0
        Call 'USER32.MessageBoxA' D$H.MainWindow, NoAvi, Argh, &MB_OK+&MB_SYSTEMMODAL | ret
    End_If

    Call 'USER32.CreateDialogIndirectParamA' D$H.Instance EmptyDialog &NULL Emptyproc 0
    Mov D$H.EmptyDialog eax

    Call 'USER32.CreateWindowExA' &WS_EX_LEFT+&WS_SIZEBOX+&WS_DLGFRAME,
                                  AnimateClass,
                                  &NULL,  ; &WS_Border  &WS_BORDER
                                  &ACS_AUTOPLAY+&WS_VISIBLE+&WS_CHILD+&ACS_CENTER,
                                  4,
                                  4,
                                  200,
                                  100,
                                  D$H.EmptyDialog,
                                  &NULL,
                                  D$H.Instance,
                                  0

    Mov D$H.Animate eax

    Mov D$WhatDialogListPtr AviList,  D$OkDialogFlag &FALSE
    add D$WhatDialogListPtr 4

    .While B$OkDialogFlag = &FALSE
        Call 'KERNEL32.CreateFileA' TempoAviFile &GENERIC_WRITE, 0, 0,
                                    &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0

        Mov D$H.TempoAviFile eax
        Mov esi D$WhatDialogListPtr | Mov ecx D$esi+4
        Call 'KERNEL32.WriteFile' D$H.TempoAviFile D$esi ecx NumberOfReadBytes &NULL
        Call 'KERNEL32.CloseHandle' D$H.TempoAviFile
        Call 'USER32.SendMessageA' D$H.Animate &ACM_OPEN  &NULL TempoAviFile
        Call SetNextChoiceID
        Call 'USER32.DialogBoxIndirectParamA' D$H.Instance, ChoiceBar, D$H.MainWindow, ChoiceDialogBoxProc, AviList
        Call 'USER32.SendMessageA' D$H.Animate &ACM_STOP  &NULL &NULL
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

    Call 'USER32.MessageBoxA' D$H.MainWindow, KillAvi, Sure, &MB_ICONQUESTION+&MB_YESNO+&MB_SYSTEMMODAL
    If eax = &IDYES
        sub D$WhatDialogListPtr 4
        Mov esi D$WhatDialogListPtr, edi esi | add esi 12
        Mov eax D$WhatDialogListPtr | sub eax AviList | shr eax 2 | Mov ecx 300 | sub ecx eax
        rep movsd
        sub D$AviListPtr 12
    End_If

L9: Call 'USER32.DestroyWindow' D$H.Animate
    Call 'USER32.EndDialog' D$H.EmptyDialog &NULL
    Call 'KERNEL32.DeleteFileA' TempoAviFile
ret


[NoCursor: B$ 'No Cursor Resources in this PE' EOS]
[KillCursor: B$ 'Delete this Cursor?' EOS]
[KillIcon: B$ 'Delete this Icon?' EOS]
[TempoCursorFile: B$ 'Cursor.$$$' EOS]
[CurDataPtr: W$ 0  D$ 22]

[H.UserCursor: D$ ?
 H.TempoCursorFile: D$ ?]

DeleteCursor:
    If D$CursorList = 0
        Call 'USER32.MessageBoxA' D$H.MainWindow, NoCursor, Argh, &MB_OK+&MB_SYSTEMMODAL | ret
    End_If

    Mov D$WhatDialogListPtr CursorList,  D$OkDialogFlag &FALSE, D$H.EmptyDialog 0
    add D$WhatDialogListPtr 4

    .While B$OkDialogFlag = &FALSE
        On D$H.EmptyDialog <> 0, Call 'USER32.EndDialog' D$H.EmptyDialog &NULL
        Call 'USER32.CreateDialogIndirectParamA' D$H.Instance EmptyDialog &NULL Emptyproc 0
            Mov D$H.EmptyDialog eax
        Call 'KERNEL32.CreateFileA' TempoCursorFile &GENERIC_WRITE, 0, 0,
                                    &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
        Mov D$H.TempoCursorFile eax
        Mov eax D$WhatDialogListPtr | sub eax CursorList | add eax GroupCursorList
        Mov ecx D$eax | sub D$ecx+14 4 | Mov ecx D$ecx+14  ; how much -4 for added Hot Spot
        Push eax, ecx
            Call 'KERNEL32.WriteFile' D$H.TempoCursorFile D$eax 010 NumberOfReadBytes &NULL
            Call 'KERNEL32.WriteFile' D$H.TempoCursorFile CurDataPtr 6 NumberOfReadBytes &NULL
            Mov esi D$WhatDialogListPtr, esi D$esi | add esi 4
        Pop ecx, eax
        Mov eax D$eax | add D$eax+14 4                     ; restore our 'how much'
        Call 'KERNEL32.WriteFile' D$H.TempoCursorFile esi ecx NumberOfReadBytes &NULL
        Call 'KERNEL32.CloseHandle' D$H.TempoCursorFile
        Call 'USER32.LoadCursorFromFileA' TempoCursorFile
        If D$H.UserCursor > 0
            pushad
               Call 'USER32.DestroyCursor' D$H.UserCursor
            popad
        End_If
        Mov D$H.UserCursor eax
            Call 'USER32.GetDC' D$H.EmptyDialog
            Push eax
                Call 'USER32.DrawIcon' eax 10 10 D$H.UserCursor
            Pop eax
            Call 'USER32.ReleaseDC' D$H.EmptyDialog eax
            Call SetNextChoiceID
        Call 'USER32.DialogBoxIndirectParamA' D$H.Instance, ChoiceBar,
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

   Call 'USER32.MessageBoxA' D$H.MainWindow, KillCursor, Sure, &MB_ICONQUESTION+&MB_YESNO+&MB_SYSTEMMODAL
   If eax = &IDYES
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

L9: Call 'USER32.EndDialog' D$H.EmptyDialog &NULL
    Call 'KERNEL32.DeleteFileA' TempoCursorFile
ret

DeleteIcon:
    If D$IconList = 0
        Call 'USER32.MessageBoxA' D$H.MainWindow, NoIcon, Argh, &MB_OK+&MB_SYSTEMMODAL | ret
    End_If

    Mov D$WhatDialogListPtr IconList,  D$OkDialogFlag &FALSE, D$H.EmptyDialog 0
    add D$WhatDialogListPtr 4

   ; ID / Pointer / Size.

    .While B$OkDialogFlag = &FALSE
        On D$H.EmptyDialog <> 0, Call 'USER32.EndDialog' D$H.EmptyDialog &NULL
        Call 'USER32.CreateDialogIndirectParamA' D$H.Instance EmptyDialog &NULL Emptyproc 0
            Mov D$H.EmptyDialog eax
        Call 'KERNEL32.CreateFileA' TempoCursorFile &GENERIC_WRITE, 0, 0,
                                    &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
        Mov D$H.TempoCursorFile eax
        Mov eax D$WhatDialogListPtr | sub eax IconList | add eax GroupIconList

      ; May hang down there here with an ecx = 0. So, temporary:
        On D$eax = 0, jmp L9>>

        Mov ecx D$eax | Mov ecx D$ecx+14
        Push ecx
            Call 'KERNEL32.WriteFile' D$H.TempoCursorFile D$eax 010 NumberOfReadBytes &NULL
            Call 'KERNEL32.WriteFile' D$H.TempoCursorFile CurDataPtr 6 NumberOfReadBytes &NULL
            Mov esi D$WhatDialogListPtr
        Pop ecx
        Call 'KERNEL32.WriteFile' D$H.TempoCursorFile D$esi D$esi+4 NumberOfReadBytes &NULL
        Call 'KERNEL32.CloseHandle' D$H.TempoCursorFile
        Call 'USER32.LoadCursorFromFileA' TempoCursorFile
        If D$H.UserCursor > 0
            pushad
               Call 'USER32.DestroyCursor' D$H.UserCursor
            popad
        End_If
        Mov D$H.UserCursor eax
            Call 'USER32.GetDC' D$H.EmptyDialog
            Push eax
                Call 'USER32.DrawIcon' eax 10 10 D$H.UserCursor
            Pop eax
            Call 'USER32.ReleaseDC' D$H.EmptyDialog eax
            Call SetNextChoiceID

        Call 'USER32.DialogBoxIndirectParamA' D$H.Instance, ChoiceBar,
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

    Call 'USER32.MessageBoxA' D$H.MainWindow, KillIcon, Sure, &MB_ICONQUESTION+&MB_YESNO+&MB_SYSTEMMODAL
    If eax = &IDYES
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

L9: Call 'USER32.EndDialog' D$H.EmptyDialog &NULL
    Call 'KERNEL32.DeleteFileA' TempoCursorFile
ret


ReadOtherFile:
    Move D$OtherhwndFileOwner D$H.MainWindow, D$OtherhInstance D$H.Instance

    Mov edi OtherSaveFilter, ecx 260, eax 0 | rep stosd
    Call 'Comdlg32.GetOpenFileNameA' OtherOpenStruc
      On D$OtherSaveFilter = 0,  ret

  ; Loading the entire file in memory:
    On D$H.OtherSource > 0, Call 'KERNEL32.CloseHandle' D$H.OtherSource
    Mov D$H.OtherSource 0

    Call 'KERNEL32.CreateFileA' OtherSaveFilter, &GENERIC_READ,
                                &FILE_SHARE_READ+&FILE_SHARE_WRITE, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE

      Call MessageBox D$BusyFilePtr

ret  ; return to caller of caller

    Else
        Mov D$H.OtherSource eax
    End_If

    Call 'KERNEL32.GetFileSize'  eax 0 | Mov D$OtherFileLen eax

    Call VirtualAlloc OtherFilePtr,
                      eax


    Call 'KERNEL32.ReadFile' D$H.OtherSource, D$OtherFilePtr,
                            eax, NumberOfReadBytes 0
ret


CloseOtherFilesRead:
    On D$H.OtherSource > 0, Call 'KERNEL32.CloseHandle' D$H.OtherSource
    Mov D$H.OtherSource 0
ret



AskForResID:
    Call 'USER32.DialogBoxIndirectParamA' D$H.Instance, OtherIdTemplate, 0, OtherIDProc, 0
ret


[H.OtherEdit: D$ ?]
[OtherID: B$ '                   ' EOS]

[OnIconLoad: D$ ?]

Proc OtherIDProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    .If D@msg = &WM_COMMAND
       ..If D@wParam = &IDCANCEL
            Call 'USER32.EndDialog' D@hwnd 0
       ..Else_If D@wParam = &IDOK
           Call 'USER32.GetDlgItem' D@hwnd 3 | Mov D$H.OtherEdit eax
           Call 'USER32.SendMessageA' D$H.OtherEdit &WM_GETTEXTLENGTH 0 0 | inc eax
           Call 'USER32.SendMessageA' D$H.OtherEdit &WM_GETTEXT eax OtherID
           TranslateAsciiToDword OtherID
           If eax > 0FFFF

             Call MessageBox D$IdTooBigPtr

           Else_If eax < 1

             Call MessageBox D$IdTooSmallPtr

           Else
             Mov edi D$OtherListPtr | stosd
             Mov eax D$OtherFilePtr | stosd
             Mov eax D$OtherFileLen | stosd
             Call 'USER32.EndDialog' D@hwnd 0
           End_If
       ..End_If

    .Else_If D@msg = &WM_INITDIALOG
        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$STRUC.WINDOWCLASS@hIcon
        Call 'USER32.GetDlgItem' D@hwnd 3
        Call 'USER32.SendMessageA' eax &EM_SETLIMITTEXT 5  0
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

[@OID0: D$ 050000000 0         ; Style
 U$ 07E 03 038 013             ; Dim
 01                            ; ID
 0FFFF 080                     ; Class
 'OK' 0                        ; Title
 0]                            ; No creation data

[@OID1: D$ 050000000 0         ; Style
 U$ 03 03 038 013              ; Dim
 02                            ; ID
 0FFFF 080                     ; Class
 'Abort' 0                     ; Title
 0]                            ; No creation data

[@OID2: D$ 050802000 0         ; Style
 U$ 040 05 038 0F              ; Dim
 03                            ; ID
 0FFFF 081                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data

____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
