TITLE Tools_Exec
 _______________________________________________________________________________________
 _______________________________________________________________________________________
;;
                               ; run "Calc.exe" and friends...
 _______________________________________________________________________________________
 _______________________________________________________________________________________

 Icons Links are not yet used by RosAsm. I had a look at Win32.hlp for how to use them
 from an app, in order to retrieve the true link. >>> 3 pages of C chiet (!!!!!!).

 So, i will try later to read the link file. It looks like this:


 D:\RosAsm\Calc.exe.lnk  334 bytes

00000000: 4C 00 00 00 01 14 02 00 - 00 00 00 00 C0 00 00 00
00000010: 00 00 00 46 13 00 00 00 - 20 00 00 00 C0 9C 12 13
00000020: 7C D1 BF 01 00 B0 92 B9 - CB D0 BF 01 00 8C FC C4
00000030: F8 CD BC 01 00 82 03 00 - 00 00 00 00 01 00 00 00
00000040: 00 00 00 00 00 00 00 00 - 00 00 00 00 90 00 14 00
00000050: 1F 00 E0 4F D0 20 EA 3A - 69 10 A2 D8 08 00 2B 30
00000060: 30 9D 19 00 23 43 3A 5C - 00 00 00 00 00 00 00 00
00000070: 00 00 00 00 00 00 00 00 - 00 11 EE 25 00 31 00 00
00000080: 00 00 00 6D 28 9A 65 11 - 00 50 72 6F 67 72 61 6D
00000090: 20 46 69 6C 65 73 00 50 - 52 4F 47 52 41 7E 31 00
000000A0: 20 00 31 00 00 00 00 00 - C8 28 4B 98 10 00 42 61
000000B0: 73 65 43 61 6C 63 00 42 - 41 53 45 43 41 4C 43 00
000000C0: 1C 00 32 00 00 82 03 00 - 3E 23 C0 BB 20 80 42 61
000000D0: 73 65 63 61 6C 63 2E 65 - 78 65 00 00 00 00 55 00
000000E0: 00 00 1C 00 00 00 01 00 - 00 00 1C 00 00 00 2D 00
000000F0: 00 00 00 00 00 00 54 00 - 00 00 11 00 00 00 03 00
00000100: 00 00 EE 15 26 16 10 00 - 00 00 00 43 3A 5C 50 72
                                              C  :  \  P  r
00000110: 6F 67 72 61 6D 20 46 69 - 6C 65 73 5C 42 61 73 65
           o  g  r  a  m --  F  i    l  =  s  \  <  a  s  e
00000120: 43 61 6C 63 5C 42 41 53 - 45 43 41 4C 43 2E 45 58
           C  a  l  c  \  <  A  S    =  C  A  L  C  .  =  X
00000130: 45 00 00 19 00 43 3A 5C - 50 72 6F 67 72 61 6D 20
           =              C  :  \    P  r  o  g  r  a  m --
00000140: 46 69 6C 65 73 5C 42 61 - 73 65 43 61 6C 63
           F  i  l  =  s  \  <  a    s  =  C  a  l  c

 So do i think it will be much easier to read this thing down to the end, make

 > std
 >   Mov al ':', ecx 0FFFF | repne scasb | repne scasb   ; >>> edi > C:\Pr...
 > cld

 store a copy in place of default, and go on (3 simple lines instead of 3 uncertain pages).

 i'll see if it works like this later because i do not know if the links files are
 the same in all win32 platforms.... (i am affraid they are not...)

 Right now, if you use a calc that need installation at C:\somewhere\...
 just copy the full path and name down here instead of "CalcName" or "CalcLinkName".
 the "CalcLinkName" down here is the one i use on my computer... do it your own and
 recompile "CopyOfRosAsm.exe".
;;


[CalcMessage: "

    Copy your prefered Calc in this directory

    and rename it 'Calc.exe'

    or have a search for 'Calc.exe' in RosAsm source    
    and read the header comment...

     "  0

 FileNotFound: 'Requested file not found', 0]


[ProcessInfos: ? #4]

[CalcHandle: ?  CalcExitCode: ?  CalcWindow: ?]

;;
 Multi-instances Calc are a problem: I would have prefered to put the calc foreground
 in case user clic on RosAsm window without closing the calc. I have never been able to
 get the handle of it to transmit to 'SetForeground'. So i leave it like this: Each
 time [Calc] menu item is clicked, we first kill the calc (even if already dead). It
 works supringly well, but i do not understand why. It seams that Win doesn't reuse
 the same "handles"... help wanted!!!
;;

Calc:
    If B$OnCalc = &TRUE
      Call 'KERNEL32.TerminateProcess' D$ProcessInfos D$CalcExitCode
    End_If

    Call 'KERNEL32.CreateProcessA'  CalcName  0 0 0  0  0 0 0  STARTUPINFO  ProcessInfos

    On eax = 0,
      Call 'KERNEL32.CreateProcessA'  CalcLinkName  0 0 0  0  0 0 0  STARTUPINFO  ProcessInfos

    If eax = 0
      Call 'User32.MessageBoxA' D$H.MainWindow, CalcMessage, FileNotFound,
                                &MB_ICONINFORMATION+&MB_SYSTEMMODAL
    Else
      Call 'KERNEL32.GetExitCodeProcess' D$ProcessInfos CalcExitCode
      Mov B$OnCalc &TRUE
    End_If
ret
________________________________________________________________________________________
________________________________________________________________________________________

[open: 'open', 0]

; Same but for OpCodes.hlp / Win32.hlp
;;
[OpCodesMessage: " Copy 'x86eas.hlp' in this directory
or run [Config] menu option" 0]

Opcodes_Hlp:
    Call 'Shell32.ShellExecuteA' D$H.MainWindow open  OpcodeHlpName NoParameter &NULL &SW_SHOWDEFAULT
    If eax <= 32
      Call 'User32.MessageBoxA' 0 OpCodesMessage, FileNotFound, &MB_ICONINFORMATION+&MB_SYSTEMMODAL
    End_If
ret
;;

[WinHlpMessage: "Copy 'Win32.hlp' in this directory
or run [Config] menu option", 0]

Win32_Hlp:
    Call Help Win32HlpName, NoParameter, WinHlpMessage
ret

;;
[Asm32HlpMessage: "Copy 'Asm32Tut.exe' in this directory
or run [Config] menu option", 0]

Asm32_Hlp:
    Call 'Shell32.ShellExecuteA' D$H.MainWindow open Asm32TutName NoParameter &NULL &SW_SHOWDEFAULT
    If eax <= 32
      Call 'User32.MessageBoxA' 0 Asm32HlpMessage, FileNotFound, &MB_ICONINFORMATION+&MB_SYSTEMMODAL
    End_If
ret
;;

[MmediaHlpMessage: "Copy 'Mmedia.hlp' in this directory
or run [Config] menu option", 0]

Mmedia_Hlp:
    Call Help MmediaHlpName, NoParameter, MmediaHlpMessage
ret


[OpenGlHlpMessage: "Copy 'OpenGl.hlp' in this directory
or run [Config] menu option", 0]

OpenGl_Hlp:
    Call Help OpenGlHlpName, NoParameter, OpenGlHlpMessage
ret


[WinSockHlpMessage: "Copy 'WinSock.hlp' in this directory
or run [Config] menu option", 0]

WinSock_Hlp:
    Call Help WinSockHlpName, NoParameter, WinSockHlpMessage
ret


[SDLRefHlpMessage: "Copy 'SDL.chm' in this directory
or run [Config] menu option", 0]

SDL_Hlp:
    Call Help SDLRefName, NoParameter, SDLRefHlpMessage
ret


[sqliteHlpMessage: "Copy 'sqlite.hlp' in this directory
or run [Config] menu option", 0]

sqlite_Hlp:
    Call Help sqliteName, NoParameter, sqliteHlpMessage
ret


[DevILHlpMessage: "Copy 'DevIL.html' in this directory
or run [Config] menu option", 0]

DevIL_Hlp:
    Call Help DevILName, NoParameter, DxHlpMessage
ret


[DxHlpMessage: "Copy 'DirectX.hlp' in this directory
or run [Config] menu option", 0]

Dx_Hlp:
    Call Help DxHlpName, NoParameter, DxHlpMessage
ret

;;
[WinDataFileMessage: "Run [Config] menu option and define a File    ", 0]

WinDataFile:
    Call 'Shell32.ShellExecuteA' D$H.MainWindow open WinDataFileName NoParameter &NULL &SW_SHOWDEFAULT
    If eax <= 32
      Call 'User32.MessageBoxA' 0 WinDataFileMessage, FileNotFound, &MB_ICONINFORMATION+&MB_SYSTEMMODAL
    End_If
ret
;;

[RosAsmHlpMessage: "
Copy 'B_U_Asm.exe' in this directory or run:

[Configuration] menu option.


If this is for a first run, select:

[Configuration][Companion Files] and provide the Path to:

* [B_U_Asm.exe]

Then provide the Path to:        

* [Equates Path]

After this, RosAsm will shut down, and will be ready
for a first Run.

", 0]

RosAsmHelp:
    Call Help B_U_AsmName, B_U_AsmTop, RosAsmHlpMessage
ret


[F2HlpMessage: "The File you defined in the [Configuration][Other Files]
to be activated by [F2] is not available", 0]

F2Help:
    If D$F2Name <> 0
        Call Help F2Name, NoParameter, F2HlpMessage
    End_If
ret


[RosAsmLicenceHelp: 'RosAsm_License', 0]

LicenseView:
    Call Help, B_U_AsmName, RosAsmLicenceHelp, ContextHlpMessage

    Call 'USER32.MessageBoxA' D$H.MainWindow,
        {"
        RosAsm License viewed       
        
                   Accept?", 0},
        {'License', 0}, &MB_SYSTEMMODAL__&MB_YESNO

    On eax = &IDNO, Call 'KERNEL32.ExitProcess' 0
ret


[GplHelp: 'GPL', 0]

GPLView:
    Call Help, B_U_AsmName, GplHelp, ContextHlpMessage
ret






[NoParameter: B$ 0 ;
 B_U_AsmTop: 'Top', 0
 StringsHelp: 'Strings_Editor', 0
 IconHelp: 'Icon_Editor', 0
 MenuHelp: 'Menus_Editor', 0
 DialogHelp: 'Dialog_Editor', 0
 TempateHelp: 'Reusing_Code' 0
 ConfigHelp:  'Configuration_Tab', 0
 DisassemblerHelp: 'Disassembling', 0
 IncludeFilesHelp: 'Include_Files', 0
 SourceEditor: 'Source_Editor', 0
 StructuresHelp: 'Structures', 0
 HelPFiles: 'HelpFiles', 0
 DisMap: 'Disassembler_Flags', 0
 UnusedSymbolsHelp: 'Unused_symbols_Scanner', 0]

[ContextHlpMessage: 'Run [Configuration]/|Files Locations]/[RosAsm]     ' 0]

;[ContextHelp | Call 'Shell32.ShellExecuteA' D$H.MainWindow open, B_U_AsmName, #1, &NULL, &SW_SHOWNORMAL
; cmp eax 32 | ja M0>
;    Call 'User32.MessageBoxA' D$H.MainWindow ContextHlpMessage, FileNotFound, &MB_ICONINFORMATION+&MB_SYSTEMMODAL
; M0: ]


Proc ContextHelp:
    Argument @Doc, @ErrorMessage

        Call 'Shell32.ShellExecuteA' D$H.MainWindow, open, B_U_AsmName, D@Doc, &NULL,
                                     &SW_SHOWNORMAL

        If eax <= 32
            Call 'User32.MessageBoxA' D$H.MainWindow, D@ErrorMessage, FileNotFound,
                                      &MB_ICONINFORMATION+&MB_SYSTEMMODAL
        End_If
EndP


Proc Help:
    Arguments @HelpFile, @Page, @ErrorMessage

        Call 'Shell32.ShellExecuteA' D$H.MainWindow, open, D@HelpFile, D@Page, &NULL,
                                     &SW_SHOWNORMAL
        If eax <= 32
            push eax
                Call 'User32.MessageBoxA' D$H.MainWindow, D@ErrorMessage, FileNotFound,
                                          &MB_ICONINFORMATION+&MB_SYSTEMMODAL
            pop eax
        End_If
EndP


; Menu is build at 'AddUserMenu'

UserDefinedAction:
    sub eax 2000  ; eax = 0 to 8
    Mov ecx (64*4) | mul ecx
    add eax UserMenu0Path

    Call 'Shell32.ShellExecuteA' D$H.MainWindow, open, eax, NoParameter, &NULL, &SW_SHOWDEFAULT
ret

______________________________________________________________________________________

; Show a Box with Ascii codes in decimal and Hexa:

[AsciiData: B$ ? #2860]

[AsciiDialog: D$ 090CC08C2 0   ; Style
 U$ 01 0 0 01E6 0109           ; Dim
 0                             ; Menu
 0                             ; Class(not yet)
 "Ascii Table" 0                ; Title
 08 'Helv' 0]                  ; Font

[AC0: D$ 0508008C4 0           ; Style
 U$ 1 1 01E5 0108              ; Dim
 0100                          ; ID
 0FFFF 081                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data


; Build a text table to send to Edit Control:

AsciiTable:
    If D$AsciiDialogHandle > 0
        Beep | ret
    End_If

    push ebp
        Mov edi AsciiData, ebx 0, ecx 10, ebp 0
        Mov al tab | stosb
        .While ebp < 256
            Mov al bl | On al < 32, Mov al 2 | stosb
            Mov al tab | stosb
            Mov eax ebx | add edi 2
            Mov edx 0 | div ecx
            add dl '0' | Mov B$edi dl
            dec edi
            Mov edx 0 | div ecx
            add dl '0' | Mov B$edi dl
            dec edi
            Mov edx 0 | div ecx
            add dl '0' | Mov B$edi dl
            add edi 3 | Mov al tab | stosb

            Mov al '0' | stosb
            Mov eax ebx, edx eax | shr eax 4
            and eax 0F | add al '0' | On al > '9', add al 7
            stosb
            Mov eax edx
            and eax 0F | add al '0' | On al > '9', add al 7
            stosb

            add ebx 32
            cmp ebx 255 | jna L2>
            sub ebx 255
            Mov al CR | stosb | Mov al LF | stosb
            Mov al tab | stosb | jmp L3>
L2:         Mov al tab | stosb | stosb
L3:         inc ebp
        .End_While

        Mov eax 0 | stosd
       ; Show resulting box:
        Call 'User32.DialogBoxIndirectParamA' D$hinstance, AsciiDialog, 0, AsciiProc, 0
    pop ebp
ret


[TabDimPtr: 10] [AsciiDialogHandle: ?]

Proc AsciiProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    .If D@msg = &WM_COMMAND
        If D@wParam = &IDCANCEL
            Mov D$AsciiDialogHandle 0
            Call 'User32.EndDialog' D@hwnd 0
        Else
            Call 'User32.GetDlgItem' D@hwnd 0100
            Call 'User32.SendMessageA' eax &EM_SETSEL  0-1 0
        End_If

    .Else_If D@msg = &WM_INITDIALOG
        move D$AsciiDialogHandle D@hwnd
        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$wc_hIcon
        Call 'User32.GetDlgItem' D@hwnd 0100
        Call 'User32.SendMessageA' eax &EM_SETTABSTOPS 1 TabDimPtr
        Call 'User32.SetDlgItemTextA' D@hwnd 0100 AsciiData

    .Else
       popad | Mov eax &FALSE | jmp L9>

    .End_If

    popad | Mov eax &TRUE

L9: EndP

___________________________________________________________________________________________
___________________________________________________________________________________________

;             Printer job (here, with fixed pitch fonts for sources alignment)
___________________________________________________________________________________________
___________________________________________________________________________________________

; This Structure is for Call 'Comdlg32.PrintDlgA'. We really use only 'PD_Flags' to tell
; if we want/propose a user selected block to print or not:

[PRINTDLG:
 PD_lStructSize: len             PD_hWndOwner: 0       PD_hDevMode: 0
 PD_hDevNames: 0                 PD_hDC: 0
 PD_Flags: &PD_RETURNDC+&PD_SELECTION
 PD_nFromPage: W$ 0              PD_nToPage: 0         PD_nMinPage: 0
 PD_nMaxPage: 0                  PD_nCopies: 1
 PD_hInstance: D$ 0              PD_lCustData: 0
 PD_lpfnPrintHook: 0             PD_lpfnSetupHook: 0   PD_lpPrintTemplateName: 0
 PD_lpPrintSetupTemplateName: 0  PD_hPrintTemplate: 0  PD_hSetupTemplate: 0]

; Needed for 'GDI32.StartDocA' (no interrest for us):

[DocName: ' - Poor text Printer -'  0]

[DOCINFO: DI_cbSize: len  DI_lpszDocName: DocName  DI_lpszOutput: 0  DI_lpszDatatype: 0
           DI_fwType: 0]

; This is for Call 'Comdlg32.ChooseFontA'. Important:

;[CF_PRINTERFONTS 2  CF_FIXEDPITCHONLY 04000  CF_INITTOLOGFONTSTRUCT 040]


; Default font for choose-font-box (Courier new 14).
; LOGFONTA Structure (Pointed by record 4 in CHOOSEFONT):

[CbBuffer:
 lfHeight: D$ 0FFFF_FFED    ; (-19) (for details, see "LOGFONT" in win32.hlp)
 lfWidth: D$ 0
 lfEscapement: D$ 0
 lfOrientation: D$ 0
 lfWeight: D$ 0190
 lfItalic: B$ 0
 lfUnderline: B$ 0
 lfStrikeOut: B$ 0
 lfCharSet: B$ 0
 lfOutPrecision: B$ 03
 lfClipPrecision: B$ 02
 lfQuality: B$ 01
 lfPitchAndFamily: B$ 031
 lfFaceName: B$ 'Courier New' 0] [CBBsecurityTail: 0 #5] ; 32 Bytes > Total lenght = 60 Bytes

[PrintTextMetric: 0 #20]

[CHOOSEFONT: CF_lStructSize: Len     CF_hWndOwner: 0      CF_hDC: 0
             CF_lpLogFont: cbbuffer  CF_iPointSize: 0
             CF_Flags: &CF_PRINTERFONTS+&CF_FIXEDPITCHONLY+&CF_INITTOLOGFONTSTRUCT
             CF_rgbColors: 0         CF_lCustData: 0      CF_lpfnHook: 0
             CF_lpTemplateName: 0    CF_hInstance: 0      CF_lpszStyle: 0
             CF_nFontType: W$ 0      CF_Alignment:  0     CF_nSizeMin: D$ 3
             CF_nSizeMax: 0ff]

[UserChoosenPrinterFont: ?]

; Our pointers:
[PrintStartPtr: ?  PrintEndPtr: ?   CharHight: ?  PageHight: ?  PrinterDCavailable: ?]

Print:
    On D$SourceLen = 0, ret

    If B$PrinterDCavailable = &TRUE
        Call 'GDI32.DeleteDC' D$PD_hDC
        Call 'GDI32.DeleteObject' D$UserChoosenPrinterFont
    End_If

    If B$BlockInside = &TRUE
        Mov D$PD_Flags &PD_RETURNDC+&PD_SELECTION
    Else
        Mov D$PD_Flags &PD_RETURNDC+&PD_NOSELECTION
    End_If

    Call 'Comdlg32.PrintDlgA' PRINTDLG | On eax = 0, ret    ; The Printer dialog sets
    move D$CF_hDC D$PD_hDC,  D$CF_hWndOwner D$H.MainWindow          ; the DC handle in PRINTDLG struc
    If D$PD_Flags = &PD_RETURNDC+&PD_SELECTION
        move D$PrintStartPtr D$BlockStartTextPtr, D$PrintEndPtr D$BlockEndTextPtr
    Else
        Mov eax D$UpperLine | Mov D$PrintStartPtr eax
        add eax D$SourceLen | Mov D$PrintEndPtr eax
    End_If

    Call 'Comdlg32.ChooseFontA' CHOOSEFONT | On eax = 0, ret    ; user sets the font:
    Call 'GDI32.CreateFontIndirectA' cbbuffer
        Mov D$UserChoosenPrinterFont eax

    Call UpdateRegistry                                 ; To save last used Printer Font.

StartControlP:
    Call 'GDI32.GetDeviceCaps' D$PD_hDC &VERTRES | Mov D$PageHight eax   ; we set the sizes:
    Call 'GDI32.GetTextMetricsA' D$PD_hDC PrintTextMetric
        Mov eax D$PrintTextMetric, D$CharHight eax
        shl eax 4 | sub D$PageHight eax                 ; sub 16 lines for top/bottom blanks

    Call 'GDI32.StartDocA' D$PD_hDC  DOCINFO | On eax le 0, jmp L7>>; we print:
        Call 'GDI32.StartPage' D$PD_hDC | On eax le 0, jmp L7>>
            Call 'GDI32.SelectObject' D$PD_hDC D$UserChoosenPrinterFont

          Mov esi D$PrintStartPtr, edx 0
L0:       Mov ebx esi, ecx 0
L1:       cmp B$esi ' ' | jb L2>
            lodsb | inc ecx | cmp esi D$PrintEndPtr | jae L3>
              jmp L1<
L2:       add esi 2                                                      ; strip CR/LR
L3:       push esi, edx
            On ecx > 0
              Call 'USER32.TabbedTextOutA'  D$PD_hDC 0 edx ebx ecx 0,0,0
            End_If
          pop edx esi
          add edx D$CharHight
          If edx >= D$PageHight                                          ; Next page?:
            pushad
              Call 'GDI32.EndPage' D$PD_hDC | On eax le 0, jmp L7>>
                Call 'GDI32.StartPage' D$PD_hDC | On eax le 0, jmp L7>>
                  Call 'GDI32.SelectObject' D$PD_hDC D$UserChoosenPrinterFont
            popad
            Mov edx 0
          End_If
          On esi < D$PrintEndPtr, jmp L0<<

        Call 'GDI32.EndPage' D$PD_hDC | On eax le 0, jmp L7>>
      Call 'GDI32.EndDoc' D$PD_hDC
    Mov B$PrinterDCavailable &TRUE | ret

L7: Call 'GDI32.AbortDoc' D$PD_hDC
    Call 'GDI32.DeleteObject' D$UserChoosenPrinterFont
    Call 'GDI32.DeleteDC' D$PD_hDC

    Mov B$PrinterDCavailable &FALSE
ret


ControlP:
    If B$BlockInside = &FALSE
        ret
    Else_If B$PrinterDCavailable = &FALSE
        jmp Print
    Else
        move D$PrintStartPtr D$BlockStartTextPtr,  D$PrintEndPtr D$BlockEndTextPtr
        jmp StartControlP
    End_If

____________________________________________________________________________________________
____________________________________________________________________________________________

; Tool for viewing the System Resources and retrieving the IDs.

ViewSysResources:
    If D$ViewSysResourcesHandle = 0
        Call 'USER32.DialogBoxParamA' D$hinstance 29000 &NULL ViewSysResourcesProc &NULL
    Else
        Beep
    End_If
ret


[ViewSysResourcesHandle: ?    SysBitMapsHandle: ?    SysIconsHandle: ?    SysCursorsHandle: ?]

Proc ViewSysResourcesProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_INITDIALOG
        move D$ViewSysResourcesHandle D@hwnd
        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$wc_hIcon
        Call 'USER32.GetDlgItem' D@hwnd 10 | Mov D$SysBitMapsHandle eax
        Call 'USER32.GetDlgItem' D@hwnd 11 | Mov D$SysIconsHandle eax
        Call 'USER32.GetDlgItem' D@hwnd 12 | Mov D$SysCursorsHandle eax
        Call InitViewSysResourcesListBoxes
        Mov D$SysButtonText 0 | jmp L8>>

    ...Else_If D@msg = &WM_COMMAND
        Mov eax D@wParam | and D@wParam 0FFFF | shr eax 16

        ..If eax = &LBN_SELCHANGE
            Call 'USER32.SendDlgItemMessageA' D@hwnd D@wParam &LB_GETCURSEL 0 0

            add D@wParam 10     ; Listy Box ID + 10 >>> Static Control ID

            If D@wParam = 20
                SetOKButtonSysText SysBitMapsStrings SysBitMapsIDs
                lea ebx D$SysBitMapsIDs+eax*4
                Call 'USER32.LoadBitmapA' &NULL D$ebx
                Mov ecx &IMAGE_BITMAP
            Else_If D@wParam = 21
                SetOKButtonSysText SysIconsStrings SysIconsIDs
                lea ebx D$SysIconsIDs+eax*4
                Call 'USER32.LoadIconA' &NULL D$ebx
                Mov ecx &IMAGE_ICON
            Else_If D@wParam = 22
                SetOKButtonSysText SysCursorsStrings SysCursorsIDs
                lea ebx D$SysCursorsIDs+eax*4
                Call 'USER32.LoadCursorA' &NULL D$ebx
                Mov ecx &IMAGE_CURSOR
            End_If

            Call 'USER32.SendDlgItemMessageA' D@hwnd D@wParam &STM_SETIMAGE ecx eax
            Call 'USER32.SendDlgItemMessageA' D@hwnd 1 &WM_SETTEXT 0 SysButtonText

        ..Else_If D@wParam = &IDCANCEL
            jmp L1>

        ..Else_If D@wParam = &IDOK
            If B$SysButtonText <> 0
                Mov esi SysButtonText, edi SysButtonText
                While B$esi > 0
                    movsb
                    On W$esi = '&&', inc esi
                End_While
                Mov B$edi 0

                Mov eax SysButtonText, D$BlockStartTextPtr eax
                While B$eax > 0 | inc eax | End_While
                dec eax | Mov D$BlockEndTextPtr eax, B$BlockInside &TRUE
                Call ControlC | Mov B$BlockInside &FALSE
            End_If

L1:         Mov D$ViewSysResourcesHandle 0
            Call 'USER32.EndDialog' D@hwnd 0

        ..End_If

    ...Else_If D@msg = &WM_CTLCOLOREDIT
        jmp L1>

    ...Else_If D@msg = &WM_CTLCOLORLISTBOX
L1:     Call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | Mov eax D$DialogsBackGroundBrushHandle | jmp L9>

    ...Else
L8:     popad | Mov eax &FALSE | jmp L9>

    ...End_If

    popad | Mov eax &TRUE

L9: EndP


[SetOKButtonSysText | Mov edi #1 | Mov edx #2 | Call OKButtonSysText]

; Example: edi > SysBitMapsStrings // eax = zero Based Indice

[SysButtonText: ? #12]
[SysBitMapApiCall: "Call 'USER32.LoadBitmapA' &&NULL &&" 0
 SysIconApiCall: "Call 'USER32.LoadIconA' &&NULL &&" 0
 SysCursorApiCall: "Call 'USER32.LoadCursorA' &&NULL &&" 0]

OKButtonSysText:
    push eax, ebx
        .While eax > 0
            While B$edi <> 0 | inc edi | End_While
            inc edi | dec eax
        .End_While

        push edi
            Mov edi SysButtonText
            If edx = SysBitMapsIDs
                Mov esi SysBitMapApiCall
            Else_If edx = SysIconsIDs
                Mov esi SysIconApiCall
            Else
                Mov esi SysCursorApiCall
            End_If
            While B$esi <> 0 | movsb | End_While
        pop esi

      ; Copy the Equate in the Button Buffer:
        inc esi
        While B$esi <> 0 | movsb | End_While
        Mov D$edi ' ; =', B$edi+4 ' ' | add edi 5

    pop ebx, eax
    push eax, ebx
        Mov eax D$edx+eax*4 | Call WriteEax | Mov B$edi 0
    pop ebx, eax
ret


[SysStringPointer: ?]

[SysBitMapsStrings: B$
'&OBM_CLOSE' 0       '&OBM_UPARROW' 0     '&OBM_DNARROW' 0     '&OBM_RGARROW' 0
'&OBM_LFARROW' 0     '&OBM_REDUCE' 0      '&OBM_ZOOM' 0        '&OBM_RESTORE' 0
'&OBM_REDUCED' 0     '&OBM_ZOOMD' 0       '&OBM_RESTORED' 0    '&OBM_UPARROWD' 0
'&OBM_DNARROWD' 0    '&OBM_RGARROWD' 0    '&OBM_LFARROWD' 0    '&OBM_MNARROW' 0
'&OBM_COMBO' 0       '&OBM_UPARROWI' 0    '&OBM_DNARROWI' 0    '&OBM_RGARROWI' 0
'&OBM_LFARROWI' 0    '&OBM_SIZE' 0        '&OBM_BTSIZE' 0      '&OBM_CHECK' 0
'&OBM_CHECKBOXES' 0  '&OBM_BTNCORNERS' 0 0]

[SysBitMapsIDs:
 &OBM_CLOSE       &OBM_UPARROW     &OBM_DNARROW     &OBM_RGARROW
 &OBM_LFARROW     &OBM_REDUCE      &OBM_ZOOM        &OBM_RESTORE
 &OBM_REDUCED     &OBM_ZOOMD       &OBM_RESTORED    &OBM_UPARROWD
 &OBM_DNARROWD    &OBM_RGARROWD    &OBM_LFARROWD    &OBM_MNARROW
 &OBM_COMBO       &OBM_UPARROWI    &OBM_DNARROWI    &OBM_RGARROWI
 &OBM_LFARROWI    &OBM_SIZE        &OBM_BTSIZE      &OBM_CHECK
 &OBM_CHECKBOXES  &OBM_BTNCORNERS]

[SysIconsStrings: B$
'&IDI_APPLICATION' 0     '&IDI_HAND' 0    '&IDI_QUESTION' 0    '&IDI_EXCLAMATION' 0
'&IDI_ASTERISK' 0        '&IDI_WINLOGO' 0 0]

[SysIconsIDs: &IDI_APPLICATION  &IDI_HAND  &IDI_QUESTION  &IDI_EXCLAMATION
              &IDI_ASTERISK     &IDI_WINLOGO]

[SysCursorsStrings: B$
'&IDC_ARROW' 0      '&IDC_IBEAM' 0      '&IDC_WAIT' 0       '&IDC_CROSS' 0
'&IDC_UPARROW' 0    '&IDC_SIZENWS' 0    '&IDC_SIZENESW' 0   '&IDC_SIZEWE' 0
'&IDC_SIZENS' 0     '&IDC_SIZEALL' 0    '&IDC_NO' 0         '&IDC_APPSTARTING' 0
'&IDC_HELP' 0 0]

[SysCursorsIDs:
 &IDC_ARROW     &IDC_IBEAM         &IDC_WAIT       &IDC_CROSS     &IDC_UPARROW
 &IDC_SIZENWSE  &IDC_SIZENESW      &IDC_SIZEWE     &IDC_SIZENS    &IDC_SIZEALL
 &IDC_NO        &IDC_APPSTARTING   &IDC_HELP]

InitViewSysResourcesListBoxes:
    Mov edi SysBitMapsStrings
    While B$edi <> 0
        push edi
        Call 'USER32.SendMessageA' D$SysBitMapsHandle &LB_ADDSTRING 0 edi
        pop edi
        Mov al 0, ecx 0FF | repne scasb
    End_While

    Mov edi SysIconsStrings
    While B$edi <> 0
        push edi
        Call 'USER32.SendMessageA' D$SysIconsHandle &LB_ADDSTRING 0 edi
        pop edi
        Mov al 0, ecx 0FF | repne scasb
    End_While

    Mov edi SysCursorsStrings
    While B$edi <> 0
        push edi
        Call 'USER32.SendMessageA' D$SysCursorsHandle &LB_ADDSTRING 0 edi
        pop edi
        Mov al 0, ecx 0FF | repne scasb
    End_While
ret

;;
____________________________________________________________________________________________
____________________________________________________________________________________________

 Used DLL in the actual Source.
;;


ShowSourceImports:
    If D$ImportDialogHandle = 0
        Call 'USER32.DialogBoxParamA' D$hinstance, 1100, &NULL,
                                      ViewSourceImportsProc, &NULL
    End_If
ret


ShowSourceExports:
    If D$ExportDialogHandle = 0
        Call 'USER32.DialogBoxParamA' D$hinstance, 1101, &NULL,
                                      ViewSourceExportsProc, &NULL
    End_If
ret


[DLLsProcListHandle: ?    DLLsProcFunctionsListHandle: ?]

Proc Enable:
    Arguments @ParentHandle, @ID

        Call 'User32.GetDlgItem' D@ParentHandle, D@ID
        Call 'User32.EnableWindow' eax &TRUE
EndP

Proc Disable:
    Arguments @ParentHandle, @ID

        Call 'User32.GetDlgItem' D@ParentHandle, D@ID
        Call 'User32.EnableWindow' eax &FALSE
EndP


[ImportDialogHandle: ?]

; Tag Dialog 1100

Proc ViewSourceImportsProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_COMMAND
        Mov eax D@wParam | and eax 0FFFF
        ..If eax = &IDCANCEL
            Mov D$ImportDialogHandle 0
            Call 'User32.EndDialog' D@hwnd, 0

        ..Else
            .If W@wParam = 10
                If W@wParam+2 = &LBN_SELCHANGE
                    Call RestoreRealSource
                    Call ViewDllFunctionList
                    Call SetPartialEditionFromPos
                End_If
                Call 'USER32.SetDlgItemInt' D@hwnd, 12, 0, &FALSE
                Call Disable D@hwnd, 3
                Call Disable D@hwnd, 4

            .Else_If W@wParam = 11
                If W@wParam+2 = &LBN_SELCHANGE
                    Call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle,
                                               &LB_GETCURSEL, 0, 0

                    Call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle,
                                               &LB_GETITEMDATA, eax, 0

                    Call 'USER32.SetDlgItemInt' D@hwnd, 12, eax, &FALSE
                    Call 'USER32.SendDlgItemMessageA' D@hwnd, 3, &WM_ENABLE,
                                                      &TRUE, 0
                    Call Enable D@hwnd, 3
                    Call Disable D@hwnd, 4

                End_If

            .Else_If W@wParam = 3
                Call ImportFunctionFindFirst

                Call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle, &LB_GETCURSEL, 0, 0
                Call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle, &LB_GETITEMDATA,
                                           eax, 0
                On eax > 1, Call Enable D@hwnd, 4

            .Else_If W@wParam = 4
                Call ImportFunctionFindNext

            .End_If
        ..End_If

    ...Else_If D@msg = &WM_INITDIALOG
        move D$ImportDialogHandle D@hwnd

        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$wc_hIcon

        Call 'USER32.GetDlgItem' D@hwnd, 10 | Mov D$DLLsProcListHandle eax
        Call 'USER32.GetDlgItem' D@hwnd, 11 | Mov D$DLLsProcFunctionsListHandle eax

        Call Disable D@hwnd, 3
        Call Disable D@hwnd, 4

        Call RestoreRealSource
        Call InitImportsProcList
        Call SetPartialEditionFromPos

        If B$DLLsFoundInSource = &FALSE
            Call 'USER32.MessageBoxA' D$H.MainWindow, {'No Import Function found in this Source', 0},
                                     {'Failure:', 0}, 0
            Call 'USER32.EndDialog' D@hwnd, 0
        End_If

        popad | Mov eax &FALSE | ExitP

    ...Else
        popad | Mov eax &FALSE | ExitP

    ...End_If

L8: popad | Mov eax &TRUE
EndP


ExportFunctionFindFirst:
    Call 'USER32.SendMessageA' D$DLLsProcListHandle, &LB_GETCURSEL, 0, 0
    Call 'USER32.SendMessageA' D$DLLsProcListHandle, &LB_GETTEXT, eax, SearchString

    ON eax = &LB_ERR, ret
    Mov cl B$SearchString+eax-1 | cmp cl '"' | je L1> | cmp cl "'" | je L1>
    Mov W$SearchString+eax '::' | add eax 2 | jmp L1>

ImportFunctionFindFirst:
    Call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle, &LB_GETCURSEL, 0, 0
    Call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle, &LB_GETTEXT, eax, SearchString

    .If eax <> &LB_ERR
        Mov D$SearchString+eax "'" | inc eax
L1:     Mov D$LenOfSearchedString eax

        Call RestoreRealSource

        push D$DownSearch, D$CaseSearch, D$WholeWordSearch
            Mov B$DownSearch &TRUE, B$CaseSearch &TRUE, B$WholeWordSearch &FALSE
            move D$CurrentWritingPos D$CodeSource
            Call SetCaret D$CodeSource | move D$UpperLine D$CodeSource
            Call AskForRedrawNow

                Mov D$NextSearchPos 0

                Call StringSearch

        pop D$WholeWordSearch, D$CaseSearch, D$DownSearch

        Call SetPartialEditionFromPos

    .End_If
ret


ImportFunctionFindNext:
ExportFunctionFindNext:
    Call RestoreRealSource
        push D$DownSearch, D$CaseSearch, D$WholeWordSearch
            Call StringSearch
        pop D$WholeWordSearch, D$CaseSearch, D$DownSearch
    Call SetPartialEditionFromPos
ret


[DLLsFoundInSource: ?]

InitImportsProcList:
    Mov esi D$CodeSource, edx D$SourceEnd, B$DLLsFoundInSource &FALSE

    .While esi < edx
        Mov eax D$esi
        ..If al = '"'
            inc esi
            While B$esi <> '"'
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ..Else_If al = "'"
            inc esi
            While B$esi <> "'"
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ..Else_If eax = MLC
            add esi 4
            While D$esi <> MLC
                inc esi | On esi = edx, jmp L9>>
            End_While
            add esi 4

        ..Else_If al = ';'
                While B$esi >= ' ' | inc esi | End_While

        ..Else
            or eax 020202020
            If eax = 'call'
                add esi 4
            Else_If eax = 'jmp '
                add esi 3
            Else
                jmp L8>>
            End_If

            While B$esi = ' ' | inc esi | End_While
            Mov al B$esi
            .If al = '"'
                jmp L1>
            .Else_If al = "'"
L1:             inc esi | Mov ebx esi
                While B$esi <> al
                    On B$esi <= ' ', jmp L2>
                    If B$esi = '.'
                        Mov B$DLLsFoundInSource &TRUE
                        push eax, edx, D$esi, esi
                            Mov B$esi 0
                            push ebx
                             Call Lb_FindString D$DLLsProcListHandle, ebx
         ; Call 'USER32.SendMessageA' D$DLLsProcListHandle, &LB_FINDSTRING, 0, ebx
                            pop ebx
                            On eax = &LB_ERR,
                            Call 'USER32.SendMessageA' D$DLLsProcListHandle,
                                                       &LB_ADDSTRING, 0, ebx
                        pop esi, D$esi, edx, eax
                        jmp L2>
                    End_If

                    inc esi
                End_While

L2:             While B$esi <> al
                    inc esi | On esi >= edx, jmp L9>
                End_While

            .End_If

        ..End_If

L8:     inc esi

    .End_While

L9: ret


InitExportsProcList:
    Mov esi D$CodeSource, edx D$SourceEnd, B$DLLsFoundInSource &FALSE

    .While esi < edx
        Mov eax D$esi
        ..If al = '"'
            inc esi
            While B$esi <> '"'
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ..Else_If al = "'"
            inc esi
            While B$esi <> "'"
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ..Else_If eax = MLC
            add esi 4
            While D$esi <> MLC
                inc esi | On esi = edx, jmp L9>>
            End_While
            add esi 4

        ..Else_If al = ';'
                While B$esi >= ' ' | inc esi | End_While

        ..Else_If ax = '::'
                        Mov B$DLLsFoundInSource &TRUE
                        push edx, esi
                            Mov edi esi
                            Mov al B$esi+2 | cmp al '"' | je L0> | cmp al "'" | jne l2>
L0:                         lea edi D$esi+3 | Mov ecx 0FF | repne scasb
L2:                         push D$edi | Mov B$edi 0
                            While B$esi-1 = ' ' | dec esi | End_While
L0:                         cmp B$esi-1 ' ' | jbe L1>
                            cmp B$esi-1 '[' | je L1>
                            cmp B$esi-1 ']' | je L1>
                            cmp W$esi-2 '[<' | je L1>
                            cmp B$esi-1 '|' | je L1>
                            dec esi | jmp L0<
L1:
                            Call 'USER32.SendMessageA' D$DLLsProcListHandle,
                                                       &LB_ADDSTRING, 0, esi
                            pop D$edi
                        pop esi, edx
        ..End_If

        inc esi

    .End_While
L9: ret


[CurrentSelectionInDLLsList: ?]

ViewDllFunctionList:
    Call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle, &LB_RESETCONTENT, 0, 0

    Call 'USER32.SendMessageA' D$DLLsProcListHandle, &LB_GETCURSEL, 0, 0
    Mov D$CurrentSelectionInDLLsList eax

    Mov esi D$CodeSource, edx D$SourceEnd

    ..While esi < edx
        Mov eax D$esi
        On al = 'C', or eax 020202020 | On al = 'c', or eax 020202020
        On al = 'J', or eax 020202020 | On al = 'j', or eax 020202020
        ...If al = '"'
            inc esi
            While B$esi <> '"'
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ...Else_If al = "'"
            inc esi
            While B$esi <> "'"
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ...Else_If eax = MLC
            add esi 4
            While D$esi <> MLC
                inc esi | On esi = edx, jmp L9>>
            End_While
            add esi 4

        ...Else_If al = ';'
                While B$esi >= ' ' | inc esi | End_While

        ...Else_If eax = 'jmp '
            add esi 3 | jmp L1>

        ...Else_If eax = 'call'
            add esi 4
L1:         While B$esi = ' ' | inc esi | End_While
            Mov al B$esi
            ..If al = "'"
L1:             inc esi | Mov ebx esi
                .While B$esi <> al
                    On B$esi <= ' ', jmp L2>>
                    .If B$esi = '.'
                        push eax, ebx, edx, D$esi, esi
                            Mov B$esi 0
                                Call Lb_FindString D$DLLsProcListHandle, ebx
                            Mov ecx eax
                        pop esi, D$esi, edx, ebx, eax

                        On ecx <> D$CurrentSelectionInDLLsList, jmp L2>>

                            inc esi | Mov ebx esi

                            While B$esi <> al
                                inc esi | On esi >= edx, jmp L9>>
                            End_While
                            push D$esi, esi, edx
                                Mov B$esi 0
                                push ebx
                                    Call Lb_FindString D$DLLsProcFunctionsListHandle, ebx
                                pop ebx

                                If eax = &LB_ERR
                                    Call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle,
                                                               &LB_ADDSTRING, 0, ebx
                                    Call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle,
                                                               &LB_SETITEMDATA, eax, 1
                                Else
                                    push eax
                                    Call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle,
                                                               &LB_GETITEMDATA, eax, 0
                                    inc eax
                                    pop edx
                                    Call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle,
                                                               &LB_SETITEMDATA, edx, eax
                                End_If

                            pop edx, esi, D$esi
                            jmp L3>
                    .End_If

                    inc esi
                .End_While

L2:             While B$esi <> al
                    inc esi | On esi >= edx, jmp L9>
                End_While

            ..Else_If al = '"'
                jmp L1<<

            ..End_If

        ...End_If

L3:     inc esi

    ..End_While

L9: ret


Proc Lb_FindString:
    Argument @Lb_Handle, @Pointer
    Local @FirstFound
    Uses esi, edi, edx

        Mov edx 0-1, D@FirstFound edx

L0:     push edx
            Call 'USER32.SendMessageA' D@Lb_Handle, &LB_FINDSTRING, edx, D@Pointer
        pop edx

        .If eax <> &LB_ERR
            push eax
                Call 'USER32.SendMessageA' D@Lb_Handle, &LB_GETTEXT, eax, SearchString
            pop edx

            Mov esi D@Pointer, edi SearchString | lodsb
            and al UPPERCASEMASK | and B$edi UPPERCASEMASK

            While al = B$edi
                If al = 0
                    Mov eax edx | ExitP
                End_If

                lodsb | inc edi
                and al UPPERCASEMASK | and B$edi UPPERCASEMASK
            End_While

            If D@FirstFound = 0-1
                Mov D@FirstFound edx | inc edx | jmp L0<
            Else_If edx = D@FirstFound
                Mov eax &LB_ERR
            Else
                inc edx | jmp L0<
            EndIf

        .End_If
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[ExportsFoundInSource: ?  ExportDialogHandle: ?]

; Tag Dialog 1101

Proc ViewSourceExportsProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_COMMAND
        Mov eax D@wParam | and eax 0FFFF

        ..If eax = &IDCANCEL
            Mov D$ExportDialogHandle 0
            Call 'User32.EndDialog' D@hwnd, 0

        ..Else
            .If W@wParam = 10
                If W@wParam+2 = &LBN_SELCHANGE
                    Call ExportFunctionFindFirst
                End_If

            .End_If

        ..End_If

    ...Else_If D@msg = &WM_INITDIALOG
        move D$ExportDialogHandle D@hwnd

        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$wc_hIcon

        Call 'USER32.GetDlgItem' D@hwnd, 10 | Mov D$DLLsProcListHandle eax

        Call RestoreRealSource
        Call InitExportsProcList
        Call SetPartialEditionFromPos

        If B$DLLsFoundInSource = &FALSE
            Call 'USER32.MessageBoxA' D$H.MainWindow, {'No Export Function found in this Source', 0},
                                     {'Failure:', 0}, 0
            Call 'USER32.EndDialog' D@hwnd, 0
        End_If

        popad | Mov eax &FALSE | ExitP

    ...Else
        popad | Mov eax &FALSE | ExitP

    ...End_If

L8: popad | Mov eax &TRUE
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; DLLs Scan.

ExportScanner:
    Call GetExportScannerFile

    ..If D$SfFileMemory <> 0
        push D$UserPeStart
            move D$UserPeStart D$SfFileMemory

            Call StartScan

            .If eax <> 0
                Call ReAlignPE | Call GetExportDirPointer

                If D$NumberOfDisExportedFunctions <> 0
                    Call 'USER32.DialogBoxParamA' D$hInstance, 30500, &NULL,
                                                  ViewExport, &NULL

                Else
                    jmp L7>

                End_If
            .Else
L7:             Call 'USER32.MessageBoxA' D$H.MainWindow, {'No Export found in this File', 0}, SfFile, 0

            .End_If

            VirtualFree D$UserPeStart
          ; D$UserPeStart and not D$SfFileMemory, because of the switch in 'ReAlignPE'

        pop D$UserPeStart

    ..End_If
ret


; Simplified version of 'StartNewDisFile', reuse the same Variables:

StartScan:
    Mov edi FirstDisVirtualData, eax 0
    Mov ecx LastDisVirtualData | sub ecx edi | shr ecx 2 | repe stosd
    Mov D$DisPeOrigine 0 | GetPeHeader PeHeaderPointer
    Mov eax D$eax | sub eax 080 | Mov D$DisPeOrigine eax
  ; (080 is the RosAsm Data 'PeHeaderPointer')

    GetPeHeader PeHeader

    ..If eax < D$SfFileMemory
        Mov eax 0

    ..Else_If eax > D$SfFileMemoryEnd
        Mov eax 0

    ..Else
        Mov eax D$eax

        .If eax = D$PeHeader
            GetPeHeader NumberOfSections | movzx eax W$eax

            If eax <> 0
                Mov D$DisNumberOfSections eax
                GetPeHeader ImageBase | Mov ebx D$eax, D$DisImageBase ebx
                move D$DisRvaSectionAlignment D$eax+4, D$DisFileSectionAlignment D$eax+8
            End_If

        .Else
            Mov eax 0

        .End_If

    ..End_If
ret


; SImplified version of 'CheckExport':

GetExportDirPointer:
    GetPeHeader SectionTable | Mov edx D$eax

    and D$NumberOfDisExportedFunctions 0

    If edx <> 0
        Mov ecx D$DisNumberOfSections

        GetPeHeader SectionsHeaders

L0:     Mov ebx D$eax+SECTION_RVA |  cmp edx ebx | jb L1>
            add ebx D$eax+SECTION_FILESIZE | cmp edx ebx | jb L2>

L1:     add eax SECTIONHEADERSIZE | loop L0<
            ret

L2:     add edx D$UserPeStart | Mov eax D$edx+(5*4), ebx D$edx+(6*4), ecx D$edx+(4*4)
        Mov D$NumberOfDisExportedFunctions eax, D$NumberOfDisExportNames ebx, D$DisExportOrdBase ecx
    End_If
ret


[SF:
 SF.lStructSize: D$ len
 SF.hwndOwner: D$ 0
 SF.hInstance: D$ 0
 SF.lpstrFilter: D$ SfFilter
 SF.lpstrCustomFilter: D$ 0
 SF.nMaxCustFilter: D$ 0
 SF.nFilterIndex: D$ 1
 SF.lpstrFile: D$ SfFile
 SF.nMaxFile: D$ &MAXPATH
 SF.lpstrFileTitle: D$ 0
 SF.nMaxFileTitle: D$ 0
 SF.lpstrInitialDir: D$ 0
 SF.lpstrTitle: D$ 0
 SF.Flags: D$ &OFN_CREATEPROMPT__&OFN_EXPLORER__&OFN_HIDEREADONLY__&OFN_LONGNAMES__&OFN_NONETWORKBUTTON__&OFN_OVERWRITEPROMPT__&OFN_PATHMUSTEXIST
 SF.nFileOffset: W$ 0
 SF.nFileExtension: W$ 0
 SF.lpstrDefExt: D$ 0
 SF.lCustData: D$ 0
 SF.lpfnHook: D$ 0
 SF.lpTemplateName: D$ 0]

[SfFilter: B$ 'Module Name', 0, '*.*', 0, 0]
[SfFile: B$ ? #&MAXPATH]
[SfFileHandle: ?    SfFileLen: ?    SfFileMemory: ?   SfFileMemoryEnd: ?]

GetExportScannerFile:
    Mov D$SfFile 0, D$SfFileMemory 0
    move D$SF.hwndOwner D$H.MainWindow ;, D$SF.hInstance D$hInstance
    Call 'Comdlg32.GetOpenFileNameA' SF

    ..If D$SfFile <> 0
        Call 'KERNEL32.CreateFileA' SfFile,
                                    &GENERIC_READ, &FILE_SHARE_READ,
                                    0, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, &NULL

        .If eax = &INVALID_HANDLE_VALUE
            Mov eax D$BusyFilePtr | Call MessageBox | ret

        .Else
            Mov D$SfFileHandle eax
            Call 'KERNEL32.GetFileSize' eax, 0 | Mov D$SfFileLen eax

            VirtualAlloc SfFileMemory D$SfFileLen
            add eax D$SfFileLen | Mov D$SfFileMemoryEnd eax

            Call 'KERNEL32.ReadFile' D$SfFileHandle, D$SfFileMemory D$SfFileLen,
                                     NumberOfReadBytes, 0

            Call 'KERNEL32.CloseHandle' D$SfFileHandle

          ; Is it a PE?
            Mov eax D$SfFileMemory | On W$eax <> 'MZ', jmp L7>
            add eax D$eax+03C | lea ecx D$eax+078
            On ecx > D$SfFileMemoryEnd, jmp L7>
            On ecx < D$SfFileMemory, jmp L7>
            On D$eax <> 'PE', jmp L7>

        .End_If
    ..End_If
ret

L7: VirtualFree D$SfFileMemory
    Mov eax D$NotPeExePtr | Call MessageBox
ret


[LV_COLUMN:
 @imask: D$ &LVCF_TEXT__&LVCF_WIDTH__&LVCF_FMT  ;__&LVCF_SUBITEM
 @fmt: D$ &LVCFMT_LEFT
 @cx: D$ 0
 @pszText: D$ 0
 @cchTextMax: D$ 0
 @iSubItem: D$ 0
 @iImage: D$ 0
 @iOrder: D$ 0]

[LV_ITEM:
 @imask: D$ &LVIF_TEXT
 @iItem: D$ 0
 @iSubItem: D$ 0
 @state: D$ 0
 @stateMask: D$ 0
 @pszText: D$ ExportViewBuffer
 @cchTextMax: D$ 0
 @iImage: D$ 0
 @lParam: D$ 0
 @iIndent: D$ 0]

[ScanExportPointer: ?    ScanPeOrigine: ?    SfListHandle: ?]

[ExportViewBuffer: ? #60] [AfterExportViewBuffer: ?]

; Tag Dialog 30500

Proc ViewExport:
    Arguments @hwnd, @msg, @wParam, @lParam
    Structure @RECT 16, @leftDis 0,  @topDis 4,  @rightDis 8,  @bottomDis 12

    pushad

    ...If D@msg = &WM_COMMAND
        Mov eax D@wParam | and eax 0FFFF
        ..If eax = &IDCANCEL
            Call 'User32.EndDialog' D@hwnd, 0

        ..Else
            Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &EM_SETSEL, 0-1, 0

        ..End_If

    ...Else_If D@msg = &WM_INITDIALOG

        Call 'USER32.GetDlgItem' D@hwnd, 10 | Mov D$SfListHandle eax

        Call 'USER32.GetClientRect' D$SfListHandle D@RECT
        Mov eax D@rightDis | sub eax D@leftDis | Mov D@bottomDis eax
        shr eax 3 | Mov D$LV_COLUMN@cx eax | sub D@bottomDis eax

        push eax
            Mov D$LV_COLUMN@pszText {'Ordinal', 0}
            Call 'USER32.SendMessageA' D$SfListHandle, &LVM_INSERTCOLUMN, 0, LV_COLUMN

            shl D$LV_COLUMN@cx 1 | Mov eax D$LV_COLUMN@cx | sub D@bottomDis eax
            Mov D$LV_COLUMN@pszText {'Relative Address', 0}
            Call 'USER32.SendMessageA' D$SfListHandle, &LVM_INSERTCOLUMN, 1, LV_COLUMN

            move D$LV_COLUMN@cx D@bottomDis
        pop eax

        Mov D$LV_COLUMN@pszText {'Name', 0}
        Call 'USER32.SendMessageA' D$SfListHandle, &LVM_INSERTCOLUMN, 2, LV_COLUMN

      ; 'ExportSectionComments'

        GetPeHeader SectionTable | Mov edi D$eax | add edi D$UserPeStart

            Mov eax D$edi+(3*4) | add eax D$UserPeStart
            If eax > D$UserPeStart
                On eax < D$UserPeEnd,
                        Call 'USER32.SendMessageA', D@hwnd, &WM_SETTEXT, 0, eax
            End_If

            Call 'USER32.SendMessageA' D$SfListHandle, &LVM_SETITEMCOUNT, D$edi+(5*4), 0

      ; Number of Functions:
        Mov ecx D$edi+(5*4)
      ; Pointer to ExportAdressesTable:
        Mov ebx D$edi+(7*4) | On ebx <> 0, add ebx D$UserPeStart
      ; Pointer to ExportNamesTable:
        Mov esi D$edi+(8*4) | On esi <> 0, add esi D$UserPeStart
      ; Pointer to ExportOrdinals:
        Mov edx D$edi+(9*4) | On edx <> 0, add edx D$UserPeStart

        If esi = edx
         Mov esi 0, edx 0
        End_If

        Mov D$LV_ITEM@iItem 0
L0:
        If D$ebx = 0
           pushad
           dec D$LV_ITEM@iItem
           jmp L5>>
        End_If
; Write the Ordinal:
        Mov eax ebx | sub eax D$UserPeStart | sub eax D$edi+(7*4) | shr eax 2
        pushad
        add eax D$edi+(4*4) | Mov edi ExportViewBuffer | Call WriteEax | Mov B$edi 0
        Mov D$LV_ITEM@iSubItem 0
        Call 'USER32.SendMessageA' D$SfListHandle, &LVM_INSERTITEM, 0, LV_ITEM
        popad
; Write the Relative Address:
        pushad
        Mov eax D$ebx
        Mov edi ExportViewBuffer | Call WriteEax | Mov B$edi 0
        Mov D$LV_ITEM@iSubItem 1
        Call 'USER32.SendMessageA' D$SfListHandle, &LVM_SETITEM, 0, LV_ITEM
        popad

        pushad
        cmp edx 0 | je L5>>
        Mov ecx D$edi+(6*4) | Mov edi edx | repne scasw | jne L5>>
        sub edi 2 |sub edi edx | shl edi 1 | Mov esi D$esi+edi | test esi esi | je L5>>
        add esi D$UserPeStart | Mov edi ExportViewBuffer
        .If esi =< D$UserPeStart
            Mov D$edi '???'
        .Else_If esi >= D$UserPeEnd
            Mov D$edi '???'
        .Else
            While B$esi <> 0
               If B$esi < ' '
                  Mov D$ExportViewBuffer '???' | jmp L2>
               End_If
               movsb
               If edi = AfterExportViewBuffer
                  Mov D$edi-3 '...' | jmp L2>
               End_If
               If esi = D$UserPeEnd
                  Mov D$edi '???' | jmp L2>
               End_If
            End_While | Mov B$edi 0
        .End_If
L2:
        Mov D$LV_ITEM@iSubItem 2
        Call 'USER32.SendMessageA' D$SfListHandle, &LVM_SETITEM, 0, LV_ITEM
L5:
        popad | add ebx 4 | inc D$LV_ITEM@iItem | dec ecx | jnz L0<<
        Call 'USER32.SendMessageA' D$SfListHandle, &LVM_SETITEMCOUNT, D$LV_ITEM@iItem, 0
L7:     popad | Mov eax &FALSE | ExitP

    ...Else
        popad | Mov eax &FALSE | ExitP

    ...End_If

L8: popad | Mov eax &TRUE
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; GUIDs Stuff

[ShowGUIDsHandle: ?  GUDfileIsThere: ?]

; All of this GUIDs stuff and the associated GUD file is to be used with:
;
; [COMCall | Mov eax D$#1 eax D$eax | push #L>3 | Call D$eax+#1.#2 D$#1]

ViewGUIDs:
    If D$ShowGUIDsHandle = 0
        Call 'USER32.CreateDialogParamA' D$hInstance, 35, D$H.MainWindow, ShowGUIDsProc, &NULL
    End_If
ret


[GUIDsHelp: 'GUIDs', 0]

; Tag Dialog 35

Proc ShowGUIDsProc:
     Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ..If D@msg = &WM_COMMAND
        .If D@wParam = &IDCANCEL
L7:        Mov D$ShowGUIDsHandle 0
           VirtualFree D$GUIDsFileMemory
           Call 'User32.EndDialog' D@hwnd, 0

        .Else_If D@wParam = &IDOK
L8:         Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &LB_GETCURSEL, 0, 0
            Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &LB_GETTEXT, eax,
                                              TrashString

            Call GetGUID eax | On D$GUIDsPastingType <> 101, jmp L7<<

        .Else_If D@wParam = &IDHELP
            Call Help, B_U_AsmName, GUIDsHelp, ContextHlpMessage

        .Else_If D@wParam = 100
            Mov D$GUIDsPastingType 100

        .Else_If D@wParam = 101
            Mov D$GUIDsPastingType 101

        .Else
            Mov eax D@wParam | shr eax 16
            On eax = &LBN_DBLCLK, jmp L8<<

        .End_If

    ..Else_If D@msg = &WM_INITDIALOG
        move D$ShowGUIDsHandle D@hwnd
        Call 'USER32.GetDlgItem' D@hwnd, 10
        Call InitGUIDsView eax | On B$GUIDsInit = &FALSE, jmp L7<<

        If B$SourceReady = &FALSE
            Call Disable D@hwnd 100
            Call Disable D@hwnd 101
        End_If

        On D$GUIDsPastingType = 0, Mov D$GUIDsPastingType 101

        If D$GUIDsPastingType = 100
            Call 'User32.SendDlgItemMessageA' D@hwnd, 100, &BM_SETCHECK, &TRUE, 0
        Else
            Call 'User32.SendDlgItemMessageA' D@hwnd, 101, &BM_SETCHECK, &TRUE, 0
        End_If

    ..Else_If D@msg = &WM_VKEYTOITEM
            If W@wParam = &VK_RETURN
                jmp L8<<

            Else_If W@wParam = &VK_ESCAPE
                jmp L7<<

            End_If

    ..Else_If D@msg = &WM_CTLCOLORLISTBOX
        Call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | Mov eax D$DialogsBackGroundBrushHandle | jmp L9>

    ..Else
        popad | Mov eax &FALSE | jmp L9>

    ..End_If

    popad | Mov eax &TRUE
L9: EndP
____________________________________________________________________________________________

[GUIDsFilePath: B$ ? #&MAXPATH]

[GUIDsFileNamePointer: ?  GUIDsFileHandle: ?  GUIDsFileLength: ?
 GUIDsFileMemory: ?  GUIDsInit: ?]

[MissingGUIDsFile: B$ "
Before using this Tool,
you have to save the GUIDs File(s)
in the RosAsmFiles Directory
(aside Equates.equ)
", 0]

Proc InitGUIDsView:
    Argument @hwnd

    Mov esi IncludeFileName, edi GUIDsFilePath
    While B$esi <> 0 | movsb | End_While
    While B$edi-1 <> '\' | dec edi | End_While
    Mov D$GUIDsFileNamePointer edi
    Mov W$edi '*.', D$edi+2 'GUD'

    Call 'KERNEL32.FindFirstFileA' GUIDsFilePath, FindFile

    ...If eax <> &INVALID_HANDLE_VALUE
        Call 'KERNEL32.FindClose' eax

        Mov esi FindFile.cFileName, edi D$GUIDsFileNamePointer
        While B$esi <> 0 | movsb | End_While | Mov B$edi 0

        Call 'KERNEL32.CreateFileA' GUIDsFilePath, &GENERIC_READ,
                                    &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_NORMAL, 0
        Mov D$GUIDsFileHandle eax

        Call 'KERNEL32.GetFileSize' eax, 0 | Mov D$GUIDsFileLength eax

        VirtualAlloc GUIDsFileMemory eax

        Call 'KERNEL32.ReadFile' D$GUIDsFileHandle, D$GUIDsFileMemory,
                                 D$GUIDsFileLength, NumberOfReadBytes, 0

        Call 'KERNEL32.CloseHandle' D$GUIDsFileHandle

        Mov esi D$GUIDsFileMemory, edx esi | add edx D$GUIDsFileLength

        .While esi < edx
            .If B$esi = '['
                inc esi
                If B$esi = '&'
                    ;While B$esi <> '_' | inc esi | End_While |
                    inc esi
                    Mov edi TrashString
                    While B$esi <> ':' | movsb | End_While | Mov B$edi 0
                    push edx
                        Call 'USER32.SendMessageA' D@hwnd, &LB_ADDSTRING, 0, TrashString
                    pop edx
                End_If
            .End_If

            inc esi

        .End_While

        Mov B$GUIDsInit &TRUE

    ...Else
        Call 'USER32.MessageBoxA', 0, MissingGUIDsFile, {'File not found', 0}, 0

    ...End_If
EndP
____________________________________________________________________________________________

[GUID_MotherNamePointer: ?   SourceComment: ?  GUIDsPastingType: ?]

Proc GetGUID:
    Argument @Length

    Mov D$SourceComment 0
    Mov esi D$GUIDsFileMemory, edx esi | add edx D$GUIDsFileLength

    .While esi < edx
        .If B$esi = '['
            Mov ebx esi
            inc esi
            If B$esi = '&' ;'IID_'
                ;While B$esi <> '_' | inc esi | End_While |
                inc esi
                Mov D$GUID_MotherNamePointer TrashString
                Mov edi TrashString, ecx D@Length | repe cmpsb | je L5>

            End_If

        .Else_If B$esi = ';'
          ; Example: "; SOURCE FILE -> strmf.h"
            If D$esi+2 = 'SOUR'
                On D$esi+6 = 'CE F', Mov D$SourceComment esi
            End_If

        .End_If

        inc esi

    .End_While

    ExitP

L5:

  ; ebx pointing to the wanted GUID.
    Mov esi ebx, edi Trash

    If D$SourceComment <> 0
        push esi
            Mov esi D$SourceComment
            While B$esi <> CR | movsb | End_While
            Mov D$edi CRLF2 | add edi 4
        pop esi
    End_If

  ; Copy the GUID Data, with the Sizes Markers:
    Mov B$edi '[' | inc edi | add esi 2
    While B$esi <> ':' | movsb | End_While | Movsb

    Mov D$edi ' D$ ' | add edi 4
    While B$esi = ' ' | inc esi | End_While
    Call MovsbHexa

    Mov D$edi ' W$ ' | add edi 4
    While B$esi = ' ' | inc esi | End_While
    Call MovsbHexa

    Mov D$edi ' W$ ' | add edi 4
    While B$esi = ' ' | inc esi | End_While
    Call MovsbHexa

    Mov D$edi ' B$ ' | add edi 4
    .While B$esi <> ']'
        While B$esi = ' ' | inc esi | End_While
        Call MovsbHexa
        Mov B$edi ' ' | inc edi
    .End_While
    Mov B$edi-1 ']'
    While B$esi <> CR | inc esi | End_While

    Mov D$edi CRLF2 | add edi 4
    While B$esi <= ' '  | inc esi | End_While

  ; Copy the other stuff:
    .While esi < edx
        If W$esi = '[.'
            push edx
              ; Skip the "IID_" thingies:
                Mov eax D$GUID_MotherNamePointer
                While B$eax <> '_' | inc eax | End_While | inc eax
                Mov D$GUID_MotherNamePointer eax
                Call BuildGUIDvTable | Call InsertGUIDsObjectHandle
            pop edx
        Else_If W$esi = '[&'
            jmp L5>
        Else_If B$esi = ';'
            While B$esi <> CR | inc esi | End_While | add esi 2
        Else
            movsb
        End_If
    .End_While

L5: Mov B$edi 0

    push D$BlockInside, D$BlockEndTextPtr, D$BlockStartTextPtr
        Mov B$BlockInside &TRUE, D$BlockStartTextPtr Trash
        dec edi | Mov D$BlockEndTextPtr edi

        Call ControlC

        If D$GUIDsPastingType = 101
            Mov B$BlockInside &FALSE
            Call ControlV | Call AskForRedrawNow
        End_If

    pop D$BlockStartTextPtr, D$BlockEndTextPtr, D$BlockInside
EndP


InsertGUIDsObjectHandle:
  ; Insert the Object Handle Declaration:
    Mov D$edi CRLF2 | add edi 4

    Mov B$edi '[' | inc edi
    push esi
        Mov esi D$GUID_MotherNamePointer
        ;While B$esi <> '_' | inc esi | End_While | inc esi
        While B$esi <> 0 | movsb | End_While
        Mov B$edi ':' | inc edi
    pop esi
    Mov D$edi ' ?]' | add edi 3
ret
____________________________________________________________________________________________

; Just to turn all GUIDs' hexa numbers upper cases:

MovsbHexa:
L0: lodsb
    .If al > ' '
        If al = ']'
            dec esi | ret
        Else_If al < 'Z'

        Else
            sub al ' '
        End_If

        stosb | jmp L0<
    .End_If
ret
____________________________________________________________________________________________

[vTableQueryInterface: '.QueryInterface', 0
 vTableAddRef: '.AddRef', 0
 vTableRelease: '.Release', 0]

BuildGUIDvTable:
    Mov D$GUIDDisplacementEquate 0
  ; Copy the '[':
    movsb
    push esi
        zCopy D$GUID_MotherNamePointer, vTableQueryInterface
        Call WriteGUIDDisplacementEquate
        Mov B$edi ' ' | inc edi
        zCopy D$GUID_MotherNamePointer, vTableAddRef
        Call WriteGUIDDisplacementEquate
        Mov B$edi ' ' | inc edi
        zCopy D$GUID_MotherNamePointer, vTableRelease
        Call WriteGUIDDisplacementEquate
    pop esi

    .While B$esi <> ']'
        Mov B$edi ' ' | inc edi
        push esi | zCopy D$GUID_MotherNamePointer | pop esi
        While B$esi <> CR
            Mov al B$esi, B$edi al | inc edi | inc esi
            If al = ']'
                dec edi | Call WriteGUIDDisplacementEquate
                sub edi 2 | Mov B$edi ']' | inc edi | ret
            End_If
        End_While

        Call WriteGUIDDisplacementEquate

        While B$esi <= ' ' | inc esi | End_While
    .End_While
ret
____________________________________________________________________________________________

[GUIDDisplacementEquate: ?]

WriteGUIDDisplacementEquate:
    push ebx, edx, esi
        Mov B$edi ' ' | inc edi
        Mov eax D$GUIDDisplacementEquate
        Call WriteEax
        add D$GUIDDisplacementEquate 4
    pop esi, edx, ebx
    Mov W$edi CRLF | add edi 2
ret
____________________________________________________________________________________________


































