TITLE BitMap          ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________
;;
                                 BitMaps jobs.

 All-in-One organisation have some limits: I do not see what interrest it could be to
 implement a full BitMap Editor inside RosAsm... Maybe one day. Now, as we NEED BitMaps
 and as there are so many BitMaps Editors available for free, i only implement a BitMaps
 Import feature.
;;
___________________________________________________________________________________________
___________________________________________________________________________________________

;Reads Bitmaps, if any, in new load RosAsm PE (just like "ReadRosAsmMenus" / "ReadRosAsmDialogs":

ReadRosAsmBitMaps:
    Mov edi BitMapList, eax 0, ecx 300 | rep stosd
    Mov ebx &RT_BITMAP | Call SearchResourceType | On eax = 0, ret
    Mov D$BitMapListPtr BitMapList, ebx BitMapListPtr | Call ReadResourcesRecord
ret


[BmStartOfResources: D$ ?
 BmExePtr: D$ ?
 BmFileLen: D$ ?
 H.BmpFile: D$ ?]

[BmFilterPtr: D$ 1]

[BmFilesFilters:  B$ 'BitMap files' EOS  '*.bmp' EOS 0]
[OpenBitMapFileTitle: B$ 'Choose a BitMap File to open' EOS]


[BmuFileFilter: B$ ? # &MAX_PATH]
[BmSaveFilter: B$ ? # &MAX_PATH]
[BmChoosenFile: B$ ? # &MAX_PATH]

[BmOpenStruc:
 D$ len
 BmhwndFileOwner: D$ 0
 BmOpenInstance: D$ 0
 D$ BmFilesFilters
 D$ BmuFileFilter
 D$ &MAX_PATH
 D$ BmFilterPtr
 D$ BmSaveFilter
 D$ &MAX_PATH
 D$ BmChoosenFile
 D$ &MAX_PATH
 D$ 0
 D$ OpenBitMapFileTitle
 D$ 0281804 ; Win Equate ?!
 D$ 0  0  0  0  0]

[PointerToData: D$ ?]

LoadBitMap:
  ; Opening a .bmp file:
    Call SearchEmptyBitMapListRecord

    Call ClearPATH BmSaveFilter

    Call 'Comdlg32.GetOpenFileNameA' BmOpenStruc | On D$BmSaveFilter = 0,  ret

    On D$H.BmpFile > 0, Call 'KERNEL32.CloseHandle' D$H.BmpFile

    Call 'KERNEL32.CreateFileA' BmSaveFilter, &GENERIC_READ,
                                &FILE_SHARE_READ+&FILE_SHARE_WRITE, 0, &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE

        Call MessageBox D$BusyFilePtr

ret

    Else
        Mov D$H.BmpFile eax
    End_If

    Call 'KERNEL32.GetFileSize' eax 0 | sub eax 14
    Mov edi D$BitMapListPtr | add edi 8 | stosd         ; write BitMap lenght in List
    add eax 14
    Mov D$BmFileLen eax                                 ; > eax = adress for asked memory

    Call VirtualAlloc TempoMemPointer,
                      eax

    Mov edi D$BitMapListPtr | add edi 4 | Move D$edi D$TempoMemPointer ; write BitMap mem adress in List
    Push edi

        Call 'KERNEL32.ReadFile' D$H.BmpFile D$TempoMemPointer,
                              14 NumberOfReadBytes 0    ; jump over BitMapFile header.
    Pop edi

    Mov esi D$edi
    lodsw | cmp ax 'BM' | jne BadBitMapFileHeader
    lodsd | cmp eax D$BmFileLen | jne BadBitMapFileHeader
    lodsd | cmp eax 0 | jne BadBitMapFileHeader
    lodsd | Mov D$PointerToData eax

  ; Load BitMap Data in same table (overwrite no more use header):
    Mov ecx D$BmFileLen | sub ecx 14  ; 14 Bytes = len of File Header
  ; (File header is: W$ Style // D$ Size // D$ 0 // D$ Ptr to Data).

    Push edi
        Call 'KERNEL32.ReadFile' D$H.BmpFile D$edi ecx NumberOfReadBytes 0
    Pop edi

  ; Ajust image size if this record is missing:
    Mov edi D$edi
    If D$edi+20 = 0
        Mov eax D$BmFileLen | sub eax D$PointerToData
        Mov D$edi+20 eax
    End_If

  ; Ask user for what BitMap ID number:
L1: Call 'USER32.DialogBoxIndirectParamA' D$H.Instance  BMIDDialog  0  BMIDDialogProc  0

    If B$UserValidateBitMap = &TRUE
        Call ReOrderBitMapList
    Else
        Mov edi D$BitMapListPtr, eax 0 | stosd | stosd | stosd
    End_If
ret


[ConflictIDs: B$ 'This ID number is already in Use' EOS]

[ReorderFlag: D$ ?]

ReOrderBitMapList:
    Mov B$ReorderFlag &FALSE
    Mov esi BitMapList, edi esi | add edi 12
    While D$edi > 0
        Mov eax D$esi
        If eax > D$edi
            Exchange D$esi D$edi, D$esi+4 D$edi+4, D$esi+8 D$edi+8
            Mov B$ReorderFlag &TRUE
        End_If
        add esi 12 | add edi 12
    End_While
    cmp B$ReorderFlag &TRUE | je ReOrderBitMapList
ret

SearchEmptyBitMapListRecord:
    Push esi
        Mov esi BitMapList
        While D$esi > 0
            add esi 12
        End_While
        Mov D$BitMapListPtr esi
    Pop esi
ret


[BadBitMapFile: B$ 'Bad BitMap file header' EOS]

BadBitMapFileHeader:
    Call 'USER32.MessageBoxA' D$H.MainWindow, BadBitMapFile, Argh, &MB_SYSTEMMODAL
L8: Mov edi D$BitMapListPtr, eax 0, ecx 3 | rep stosd
ret


[BMIDDialog: D$ 090C408C2 0    ; Style
 U$ 03 0 0 0B9 018             ; Dim
 0                             ; no Menu
 '' 0                          ; Class
 'What ID number for new BitMap?' 0 ; Title
 08 'Helv' 0]                  ; Font

[BMID0: D$ 050000000 0         ; Style
 U$ 07E 03 038 013             ; Dim
 01                            ; ID
 0FFFF 080                     ; Class
 'OK' 0                        ; Title
 0]                            ; No creation data

[BMID1: D$ 050000000 0         ; Style
 U$ 03 03 038 013              ; Dim
 02                            ; ID
 0FFFF 080                     ; Class
 'Abort' 0                     ; Title
 0]                            ; No creation data

[BMID2: D$ 050802000 0         ; Style
 U$ 040 05 038 0F              ; Dim
 03                            ; ID
 0FFFF 081                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data


[H.BmpIDEdit: D$ ?]
[uBitMapID: B$ '                   ' EOS]

; Previously, i had set a test to prevent from IDs numbers smaller than 1000 and bigger than
; 32000. I do not remember why. May be an old wrong idea about unique IDs across Types.
; Suppress...

Proc BMIDDialogProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_COMMAND
       ..If D@wParam = &IDCANCEL
            Call 'USER32.EndDialog' D@hwnd 0

       ..Else_If D@wParam = &IDOK
           Call 'USER32.GetDlgItem' D@hwnd 3 | Mov D$H.BmpIDEdit eax
           Call 'USER32.SendMessageA' D$H.BmpIDEdit &WM_GETTEXTLENGTH 0 0 | inc eax
           Call 'USER32.SendMessageA' D$H.BmpIDEdit &WM_GETTEXT eax uBitMapID
           TranslateAsciiToDword uBitMapID
           Mov D$uBitMapID 0                         ; just for abort tests in callers:
           .If eax > 0FFFF    ; 32000                 ; 'StoreMenuEdition' / 'MenuEditProc'

             Call MessageBox D$IdTooBigPtr

           .Else_If eax < 1   ; 000

             Call MessageBox D$IdTooSmallPtr

           .Else
                Mov esi BitMapList
                While D$esi > 0
                    On D$esi = eax, Mov eax 0
                    add esi 12
                End_While
                If eax = 0
                    Call 'USER32.MessageBoxA' D$H.MainWindow, ConflictIDs, Argh, &MB_SYSTEMMODAL
                Else
                    Mov edi D$BitMapListPtr, D$edi eax
                    Mov B$UserValidateBitMap &TRUE
                    Call 'USER32.EndDialog' D@hwnd 0
                End_If
           .End_If

       ..End_If

    ...Else_If D@msg = &WM_INITDIALOG
        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$STRUC.WINDOWCLASS@hIcon
        Mov B$UserValidateBitMap &FALSE
        Call 'USER32.GetDlgItem' D@hwnd 3
        Call 'USER32.SendMessageA' eax &EM_SETLIMITTEXT 5  0
           Mov esi D$BitMapListPtr | On esi > BitMapList, sub esi 12
           If D$esi = 0
             Mov eax 1   ; 30000
           Else
             lodsd | inc eax
           End_If
           Call 'USER32.SetDlgItemInt' D@hwnd 3 eax 0

    ...Else
       popad | Mov eax &FALSE | jmp L9>

    ...End_If

    popad | Mov eax &TRUE

L9: EndP



[HCDC.Bitmap: D$ ?]

[H.Bitmap: D$ ?]

[BMP: 0    BMPw: 0    BMPh: 0    BMPline: 011C    BMPplane: 0    BMPpixBits: 0   BMPptr: 0
 BitMapInfoHeader: 0    FileHeaderOffset: 0    BipMapCopyPtr: 0]

BitMapViewer:
    .If D$BitMapListPtr > BitMapList
        Mov W$BitMapDialogControlsNumber 2
        Call 'USER32.DialogBoxIndirectParamA' D$H.Instance BitMapDialog  0  BitMapProc  0
        If B$UserValidateBitMap = &FALSE
            Mov edi D$BitMapListPtr, eax 0 | stosd | stosd | stosd
        End_If
    .End_If
ret


[NoBitMap: B$ 'No BitMap in This file' EOS]

[DeleteBitMapTitle: U$ 'Delete'  ShowBitMapTitle: U$ 'Exit  '] ; UNICODE ? EOS...

DeleteBitMap:
    Mov edi BMPEXIT, esi DeleteBitMapTitle, ecx 12 | rep movsb
    Mov W$BitMapDialogControlsNumber 5 | Call BitMapView

    If B$UserValidateBitMap = &TRUE
        Mov edi D$BitMapListPtr, esi edi | add esi 12
        While D$edi > 0
            movsd | movsd |movsd
        End_While
    End_If
ret


ShowBitMapsIds:
    Mov edi BMPEXIT, esi ShowBitMapTitle, ecx 12 | rep movsb
    Mov W$BitMapDialogControlsNumber 4 | Call BitMapView
ret


BitMapView:
    Mov D$BitMapListPtr BitMapList, eax D$BitMapListPtr

    If D$eax = 0
        Mov B$UserValidateBitMap &FALSE
        Call 'USER32.MessageBoxA' D$H.MainWindow, NoBitMap, Argh, &MB_SYSTEMMODAL
    Else
        Call 'USER32.DialogBoxIndirectParamA' D$H.Instance, BitMapDialog, 0, BitMapProc, 0
    End_If
ret


[UserValidateBitMap: D$ ?]
[BitMapIdText: D$ ? ? ? ?]

Proc BitMapProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_COMMAND
         ..If D@wParam = &IDOK
             Mov B$UserValidateBitMap &TRUE
             Call 'USER32.EndDialog' D@hwnd 0

         ..Else_If D@wParam = &IDCANCEL
             Mov B$UserValidateBitMap &FALSE
             Call 'USER32.EndDialog' D@hwnd 0

         ..Else_If D@wParam = 3                        ; >>>>
             Mov eax D$BitMapListPtr | add eax 12
             Mov ebx MAXBITMAP | shl ebx 2 | add ebx BitMapList
             .If eax < ebx                             ; ebx = end of BitMapList
                If D$eax > 0
                    Mov D$BitMapListPtr eax
                    Call 'USER32.RedrawWindow' D@hwnd 0  0,
                                           &RDW_ERASE+&RDW_INVALIDATE+&RDW_INTERNALPAINT
                End_If
             .End_If

         ..Else_If D@wParam = 4                        ; <<<<
             If D$BitMapListPtr > BitMapList
                 sub D$BitMapListPtr 12
                 Call 'USER32.RedrawWindow' D@hwnd 0  0,
                                           &RDW_ERASE+&RDW_INVALIDATE+&RDW_INTERNALPAINT
             End_If
       ..End_If

    ...Else_If D@msg = &WM_PAINT

         Call 'USER32.BeginPaint'  D@hwnd  PAINTSTRUCT
             Mov D$STRUCT.EditData@HDC eax
             Call 'GDI32.CreateCompatibleDC' D$STRUCT.EditData@HDC | Mov D$HCDC.Bitmap eax

             Mov esi D$BitMapListPtr | lodsd          ; ID
             Call SetBitMapIdText D@hwnd
             lodsd | Mov edi eax                      ; > edi > adress
             lodsd                                    ; eax = lenght
             Mov esi edi, ebx D$esi+20                ; ebx = image size
             sub eax ebx | add eax edi                ; eax > ptr to bmp data

             Call 'GDI32.CreateDIBitmap' D$STRUCT.EditData@HDC  edi  &CBM_INIT  eax  edi  &DIB_RGB_COLORS
                 Mov D$H.Bitmap eax
                 Call 'GDI32.SelectObject' D$HCDC.Bitmap D$H.Bitmap
                 Call 'USER32.GetClientRect' D@hwnd RECT
                 Call 'GDI32.BitBlt' D$STRUCT.EditData@HDC 0 0 D$Rect_Right D$Rect_bottom D$HCDC.Bitmap 0 0 &SRCCOPY
             Call 'GDI32.DeleteDC' D$HCDC.Bitmap
         Call 'USER32.EndPaint' D@hwnd PAINTSTRUCT
         Call 'GDI32.DeleteObject' D$H.Bitmap

    ...Else_If D@msg = &WM_INITDIALOG
        Mov D$BitMapListPtr BitMapList
        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$STRUC.WINDOWCLASS@hIcon

    ...Else
       popad | Mov eax &FALSE | jmp L9>

    ...End_If

    popad | Mov eax &TRUE

L9: EndP


Proc SetBitMapIdText:
    Argument @hwnd

    pushad
        Push 0_FFFF_FFFF
        Mov edi BitMapIdText, ecx 10
L0:     Mov edx 0 | div ecx | cmp eax 0 | je L2>
            Push edx | jmp L0<
L2:         Push edx
L2:     Pop eax | cmp eax 0_FFFF_FFFF | je L3>
            add al '0' | stosb | jmp L2<
L3:     Mov al 0 | stosb
        Call 'USER32.GetDlgItem' D@hwnd 5
        Call 'USER32.SetWindowTextA' eax BitMapIdText
    popad
EndP


[BitMapDialog: D$ 0900408C2 0  ; Style
 BitMapDialogControlsNumber:
 U$ 02 0 0 0DC 0C8             ; Dim
 0                             ; Menu
 '' 0                           ; Class
 'New Dialog' 0                ; Title
 08 'Helv' 0]                  ; Font

[BMD0: D$ 050000001 0      ; Style
 U$ 0AB 0B9 030 0F             ; Dim
 01                            ; ID
 0FFFF 080                     ; Class
 BMPEXIT:
 'Delete' 0                        ; Title
 0]                            ; No creation data

[BMD2: D$ 050000000 0      ; Style
 U$ 04E 0B9 029 0F             ; Dim
 03                            ; ID
 0FFFF 080                     ; Class
 '>>>>' 0                      ; Title
 0]                            ; No creation data

[BMD3: D$ 050000000 0      ; Style
 U$ 0 0B8 029 010              ; Dim
 04                            ; ID
 0FFFF 080                     ; Class
 '<<<<' 0                      ; Title
 0]                            ; No creation data

[BMD4: D$ 050000307 0      ; Style
 U$ 02B 0B7 01F 010            ; Dim
 05                            ; ID
 0FFFF 080                     ; Class
 '65000' 0                     ; Title
 0]                            ; No creation data

[BMD1: D$ 050000000 0      ; Style
 U$ 079 0B9 030 0F             ; Dim
 02                            ; ID
 0FFFF 080                     ; Class
 'Abort' 0                     ; Title
 0]                            ; No creation data
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
