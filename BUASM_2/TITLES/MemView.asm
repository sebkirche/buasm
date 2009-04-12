TITLE MemView

; Maintainer: Guga june the 29th of 2006

____________________________________________________________________________________________

; Routine for: Menu > Tools > 'View RosAs mMems':

[MemTableView: D$ ? #(MEMTABLESIZE*5)]
[MemTableViewEnd: ?]
[MemTableViewTitle: 'VirtualAlloc Mems actually commited to RosAsm:', 0]


[IDD_VIEWMEMWARNINGMSG 10000]
[IDC_DISPLAYMEMWARNING 10]
[IDC_DISPLAYMEMDATA 20]
[IDC_DUMPMEM 30]
[IDC_SAVEMEMREPORT 35]
[IDC_DISPLAYMEMINFO 40]

ViewRosAsmMems:
    Call 'USER32.DialogBoxParamA' D$H.Instance, IDD_VIEWMEMWARNINGMSG, &NULL,
                                  MemViewWarning, &NULL
ret


[DumpMemWarnMsg: B$ 0 #256]

_______________________________________________________

; Tag Dialog 10000

Proc MemViewWarning:
    Arguments @hwnd, @msg, @wParam, @lParam

     pushad


    ..If D@msg = &WM_INITDIALOG
       ; Get the handle of the Static Image:
        Call 'USER32.GetDlgItem' D@hwnd IDC_LIBWARNING_SHOWICON
        Mov D$LibWarningStaticImage eax
        Call 'USER32.LoadBitmapA' D$H.Instance, IDB_LIBWARNING_BITMAP
        Mov D$LibScanhIcon eax

        Call 'USER32.SendMessageA' D$LibWarningStaticImage, &STM_SETIMAGE,
                                   &IMAGE_BITMAP, D$LibScanhIcon
        Call 'USER32.SendDlgItemMessageA' D@hwnd, IDC_DISPLAYMEMWARNING,
                                          &WM_SETTEXT, 0, DumpMemWarnMsg
        Call 'USER32.SendDlgItemMessageA' D@hwnd IDC_DISPLAYMEMWARNING,
                                          &WM_GETTEXT, 0, DumpMemWarnMsg

        Call DisplayMemData D@hwnd
        Call DisplayMemInfo D@hwnd

    ..Else_If D@msg = &WM_CLOSE
        Call ClearBuffer MemTableView (MEMTABLESIZE*5*4)
       ; Call ClearBuffer MemTable (MEMTABLESIZE*4)
        Call ClearBuffer DumpMemWarnMsg 256
        Call WM_CLOSE

    ..Else_If D@msg = &WM_COMMAND
        .If D@wParam = &IDOK
            Call ClearBuffer MemTableView (MEMTABLESIZE*5*4)
          ;  Call ClearBuffer MemTable (MEMTABLESIZE*4)
            Call ClearBuffer DumpMemWarnMsg 256
            Call WM_CLOSE
        .Else_If D@wParam = IDC_DUMPMEM
            Mov eax MemTableEnd
            sub eax MemTable
            Call DumpMemory D@hwnd, MemTable, eax
        .Else_If D@wParam = IDC_SAVEMEMREPORT
            Call SaveMemoryReport D@hwnd, MemTableView, D$MemReportInfoSize
        .End_If

    ..Else

       popad | Mov eax &FALSE | ExitP

    ..End_If

    popad
    Mov eax &TRUE
EndP

_______________________________________________________

[DumpMemFileTitle: B$ 'Save Dumped memory as...', 0]
[MemSaveFileFilter: B$ 'RosAsm Memory File (*.mem)', 0  '*.mem', 0 0]
[MemSaveFilter: "Report" 0 #(&MAX_PATH-6)]

[DumpMem:
 DumpMem.lStructSize: D$ Len
 DumpMem.hwndOwner: D$ 0
 DumpMem.hInstance: D$ 0
 DumpMem.lpstrFilter: D$ MemSaveFileFilter
 DumpMem.lpstrCustomFilter: D$ CustomLibFileFilter
 DumpMem.nMaxCustFilter: D$ 260
 DumpMem.nFilterIndex: D$ 1
 DumpMem.lpstrFile: D$ MemSaveFilter
 DumpMem.nMaxFile: D$ 260
 DumpMem.lpstrFileTitle: D$ ChoosenLibFile
 DumpMem.nMaxFileTitle: D$ 260
 DumpMem.lpstrInitialDir: D$ 0
 DumpMem.lpstrTitle: D$ DumpMemFileTitle
 DumpMem.Flags: D$ &OFN_EXPLORER__&OFN_FILEMUSTEXIST__&OFN_LONGNAMES__&OFN_PATHMUSTEXIST
 DumpMem.nFileOffset: W$ 0
 DumpMem.nFileExtension: W$ 0
 DumpMem.lpstrDefExt: D$ 0
 DumpMem.lCustData: D$ 0
 DumpMem.lpfnHook: D$ 0
 DumpMem.lpTemplateName: D$ 0]

Proc DumpMemory:
    Arguments @hwnd, @File, @FileSize
    pushad

    Mov D$ChoosenLibFile 0
    move D$DumpMem.lpstrFile MemSaveFilter
    move D$DumpMem.hwndOwner D@hwnd
    move D$DumpMem.hInstance D$H.Instance
    move D$DumpMem.lpstrFilter MemSaveFileFilter
    move D$DumpMem.lpstrTitle DumpMemFileTitle
    Call 'Comdlg32.GetSaveFileNameA' DumpMem

    ..If eax <> 0
        .If D$ChoosenLibFile <> 0
            Call ForceExtension MemSaveFilter, '.mem'
            Call SaveLibFile, D@File, D@FileSize, MemSaveFilter
        .End_If
    ..End_If
    popad

EndP
_______________________________________________________

[MemSaveReportFilter: B$ 'RosAsm Memory Report (*.txt)', 0  '*.txt', 0 0]
[SaveReportMemFileTitle: B$ 'Save Report memory as...', 0]

Proc SaveMemoryReport:
    Arguments @hwnd, @File, @FileSize
    pushad

    Mov D$ChoosenLibFile 0
    move D$DumpMem.lpstrFile MemSaveFilter
    move D$DumpMem.hwndOwner D@hwnd
    move D$DumpMem.hInstance D$H.Instance
    move D$DumpMem.lpstrFilter MemSaveReportFilter
    move D$DumpMem.lpstrTitle SaveReportMemFileTitle
    Call 'Comdlg32.GetSaveFileNameA' DumpMem

    ..If eax <> 0
        .If D$ChoosenLibFile <> 0
            Call ForceExtension MemSaveFilter, '.txt'
            Call SaveLibFile, D@File, D@FileSize, MemSaveFilter
        .End_If
    ..End_If
    popad

EndP
_______________________________________________________

Proc DisplayMemData:
    Arguments @hwnd

    pushad

    Mov esi MemTable, edi MemTableView, edx 0

    While esi < MemTableEnd
        Mov eax D$esi
        .If eax > 0
            If eax = D$esi+8
                Mov B$edi 'X' ; Main committed with Reserved Blocks.
            Else
                Mov B$edi 'x' ; Child (small and nested) committed Blocks.
            End_If
        .Else
            Mov B$edi '_'
        .End_If
        add esi MEMRECORD | inc edi | inc edx

        ; Spaces and CR/LF each 4 / 8 / 32 Chunk.
        Mov eax edx | and eax 00_11
        If eax = 0
            Mov al ' ' | stosb
        End_If
            Mov eax edx | and eax 00_111
        If eax = 0
            Mov eax '    ' | stosd
        End_If
            Mov eax edx | and eax 00_11111
        If eax = 0
            Mov al CR | stosb | Mov al LF | stosb
        End_If
    End_While
    Mov B$edi 0
    Call 'USER32.SetDlgItemTextA' D@hwnd, IDC_DISPLAYMEMDATA, MemTableView

    popad
EndP
_______________________________________________________

; Calculate the maximum size of the report....
;0000000: 023 069 066 06E  - Ascii: # i f n
;0403ED4: 073 079 073 02F  - Ascii: s y s /  Size = 43
;0403ED4: 073 079 073 02F  - Not used - Ascii: s y s /  Size = 54
;0403ED4: 073 079 073 02F  - Commited - Ascii: s y s /  Size = 54
;0403ED4: 073 079 073 02F  - Reserved - Ascii: s y s /  Size = 54
; MEMTABLESIZE = 10650 dwords = 42600 bytes. The displacement is from 12 to 12 dwords
; So we have 887,5 * 4 (10650/12*4). 3550 Lines.
; On Our case this means:
; Size*3550 = 54*3550 = 191700 bytes
; Multiplication Factor = 191700/42600 = 4,5. So, for safety we do
; [MemTableView: D$ ? #(MEMTABLESIZE*5)] = 213000 Bytes

[MemTitleDisplay1: B$ "_______________________________________________________________________________
MemTable Start Address: ", 0]

[MemTitleDisplay2: B$ "
The data is displayed as:
Address; 4 Bytes sequence, Memory Type, Ascii representation.

Address: The address of the MemTable where the data is found. Each address
         is related to the 1st 12 Bytes, but only the 1st four ones are displayed
         here to preserve space.

Bytes:   1st four bytes of the memory that are displayed.

Memory Type: Specifies what is happening with the memory. It can be:
             Commited: 'x' chars. Child (small and nested) committed Blocks.
             Reserved: 'X' chars. Main committed with Reserved Blocks.
             Not Used:  Zeroed Bytes.

Ascii: Ascii representation of the 1st four bytes.
_______________________________________________________________________________
", 0]

[MemReportInfoSize: D$ 0]

Proc DisplayMemInfo:
    Arguments @hwnd

    pushad

    Mov esi MemTable, edi MemTableView, edx 0

    push esi | zCopy MemTitleDisplay1 | pop esi
    Mov eax esi
    Call Writeeax
    push esi | zCopy MemTitleDisplay2 | pop esi

    .While esi < MemTableEnd
        Mov eax D$esi
        Call WriteMemTableOffset esi
        Call WriteMemDwordChunck eax
        .If eax > 0
            If eax = D$esi+8
                push esi | zCopy {" - Commited - Ascii: ", 0} | pop esi
            Else
                push esi | zCopy {" - Reserved - Ascii: ", 0} | pop esi
            End_If
        .Else
            push esi | zCopy {" - Not used - Ascii: ", 0} | pop esi
        .End_If
        Call WriteMemAsciiChunck eax
        add esi MEMRECORD
        Mov W$edi 0A0D | add edi 2

    .End_While
    Mov B$edi 0
    Mov eax edi
    sub eax MemTableView
    Mov D$MemReportInfoSize eax
    Call 'USER32.SetDlgItemTextA' D@hwnd, IDC_DISPLAYMEMINFO, MemTableView

    popad
EndP
________________________________________________________________________________________________________

Proc WriteMemTableOffset:
    Arguments @Value
    Uses eax, ebx

    Mov eax D@Value
    Call Writeeax
    Mov W$edi ': ' | add edi 2

EndP

________________________________________________________________________________________________________

Proc WriteMemDwordChunck:
    Arguments @Value
    Uses eax, esi, edx, ecx, ebx

    lea esi D@Value
    Mov ecx 4
    xor eax eax
    Mov edx 0
    Do
        ; Arrange the Strings to the chars be displayed on the same row one below each other.
        ; Ex.:  09 055   0 014
        ;      055  01 0FF 075

        If B$esi = 0
            Mov W$edi '  ' | add edi 2
        Else_If B$esi <= 0F
            Mov B$edi ' ' | inc edi
        End_If

        lodsb
        Call writeeax
        xor eax eax
        Mov B$edi ' ' | inc edi
        dec ecx
    Loop_Until ecx = 0

EndP
________________________________________________________________________________________________________

Proc WriteMemAsciiChunck:
    Arguments @Value
    Uses eax, esi, edx, ecx, ebx

    lea esi D@Value
    Mov ecx 4
    xor eax eax
    Mov edx 0

    .Do
        lodsb
        If eax < ' '
            Mov B$edi '.' | inc edi
        Else_If eax >= 127
            Mov B$edi '.' | inc edi
        Else
            stosb
        End_If

        xor eax eax
        Mov B$edi ' ' | inc edi
        dec ecx
    .Loop_Until ecx = 0

EndP

____________________________________________________________________________________________
____________________________________________________________________________________________
































