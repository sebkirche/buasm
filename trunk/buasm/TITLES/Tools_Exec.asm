TITLE Tools_Exec      ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________
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


[CalcMessage: B$ "

    Copy your prefered Calc in this directory

    and rename it 'Calc.exe'

    or have a search for 'Calc.exe' in RosAsm source    
    and read the header comment...

     " EOS]

[FileNotFound: B$ 'Requested file not found' EOS]


[ProcessInfos: D$ ? # 4]

[CalcExitCode: D$ ?]

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
      Call 'USER32.MessageBoxA' D$H.MainWindow, CalcMessage, FileNotFound,
                                &MB_ICONINFORMATION+&MB_SYSTEMMODAL
    Else
      Call 'KERNEL32.GetExitCodeProcess' D$ProcessInfos CalcExitCode
      Mov B$OnCalc &TRUE
    End_If
ret
________________________________________________________________________________________
________________________________________________________________________________________

[open: B$ 'open' EOS]

; Same but for OpCodes.hlp / Win32.hlp
;;
[OpCodesMessage: B$ " Copy 'x86eas.hlp' in this directory
or run [Config] menu option" EOS]

Opcodes_Hlp:
    Call 'SHELL32.ShellExecuteA' D$H.MainWindow open  OpcodeHlpName NoParameter &NULL &SW_SHOWDEFAULT
    If eax <= 32
      Call 'USER32.MessageBoxA' D$H.MainWindow OpCodesMessage, FileNotFound, &MB_ICONINFORMATION+&MB_SYSTEMMODAL
    End_If
ret
;;

[WinHlpMessage: B$ "Copy 'Win32.hlp' in this directory
or run [Config] menu option" EOS]

Win32_Hlp:
    Call Help Win32HlpName, NoParameter, WinHlpMessage
ret

;;
[Asm32HlpMessage: B$ "Copy 'Asm32Tut.exe' in this directory
or run [Config] menu option" EOS]

Asm32_Hlp:
    Call 'SHELL32.ShellExecuteA' D$H.MainWindow open Asm32TutName NoParameter &NULL &SW_SHOWDEFAULT
    If eax <= 32
      Call 'USER32.MessageBoxA' D$H.MainWindow Asm32HlpMessage, FileNotFound, &MB_ICONINFORMATION+&MB_SYSTEMMODAL
    End_If
ret
;;

[MmediaHlpMessage: B$ "Copy 'Mmedia.hlp' in this directory
or run [Config] menu option" EOS]

Mmedia_Hlp:
    Call Help MmediaHlpName, NoParameter, MmediaHlpMessage
ret


[OpenGlHlpMessage: B$ "Copy 'OpenGl.hlp' in this directory
or run [Config] menu option" EOS]

OpenGl_Hlp:
    Call Help OpenGlHlpName, NoParameter, OpenGlHlpMessage
ret


[WinSockHlpMessage: B$ "Copy 'WinSock.hlp' in this directory
or run [Config] menu option" EOS]

WinSock_Hlp:
    Call Help WinSockHlpName, NoParameter, WinSockHlpMessage
ret


[SDLRefHlpMessage: B$ "Copy 'SDL.chm' in this directory
or run [Config] menu option" EOS]

SDL_Hlp:
    Call Help SDLRefName, NoParameter, SDLRefHlpMessage
ret


[sqliteHlpMessage: B$ "Copy 'sqlite.hlp' in this directory
or run [Config] menu option" EOS]

sqlite_Hlp:
    Call Help sqliteName, NoParameter, sqliteHlpMessage
ret


DevIL_Hlp:
    Call Help DevILName, NoParameter, DxHlpMessage
ret


[DxHlpMessage: B$ "Copy 'DirectX.hlp' in this directory
or run [Config] menu option" EOS]

Dx_Hlp:
    Call Help DxHlpName, NoParameter, DxHlpMessage
ret

;;
[WinDataFileMessage: B$ "Run [Config] menu option and define a File    " EOS]

WinDataFile:
    Call 'SHELL32.ShellExecuteA' D$H.MainWindow open WinDataFileName NoParameter &NULL &SW_SHOWDEFAULT
    If eax <= 32
      Call 'USER32.MessageBoxA' D$H.MainWindow WinDataFileMessage, FileNotFound, &MB_ICONINFORMATION+&MB_SYSTEMMODAL
    End_If
ret
;;

[BUAsmHlpMessage: B$ "
Copy 'B_U_Asm.exe' in this directory or run:

[Configuration] menu option.


If this is for a first run, select:

[Configuration][Companion Files] and provide the Path to:

* [B_U_Asm.exe]

Then provide the Path to:        

* [Equates Path]

After this BUAsm will shut down and will be ready
for a first Run.

" EOS]

BUAsmHelp:

    Call Help B_U_AsmName,
    B_U_AsmTop,
    BUAsmHlpMessage

ret


[F2HlpMessage: B$ "The File you defined in the [Configuration][Other Files]
to be activated by [F2] is not available" EOS]

F2Help:
    If D$F2Name <> 0
        Call Help F2Name, NoParameter, F2HlpMessage
    End_If
ret


[RosAsmLicenceHelp: B$ 'RosAsm_License' EOS]

LicenseView:
    Call Help, B_U_AsmName, RosAsmLicenceHelp, ContextHlpMessage

    Call 'USER32.MessageBoxA' D$H.MainWindow,
        {B$ "
        RosAsm License viewed       
        
                   Accept?" EOS},
        {B$ 'License' EOS}, &MB_SYSTEMMODAL__&MB_YESNO

    On eax = &IDNO, Call 'KERNEL32.ExitProcess' 0
ret


[GplHelp: B$ 'GPL' EOS]

GPLView:
    Call Help, B_U_AsmName, GplHelp, ContextHlpMessage
ret






[NoParameter: B$ EOS] ;
[B_U_AsmTop: B$ 'Top' EOS]
[StringsHelp: B$ 'Strings_Editor' EOS]
[IconHelp: B$ 'Icon_Editor' EOS]
[MenuHelp: B$ 'Menus_Editor' EOS]
[DialogHelp: B$ 'Dialog_Editor' EOS]
[TempateHelp: B$ 'Reusing_Code' EOS]
[ConfigHelp:  'Configuration_Tab' EOS]
[DisassemblerHelp: B$ 'Disassembling' EOS]
[IncludeFilesHelp: B$ 'Include_Files' EOS]
[SourceEditor: B$ 'Source_Editor' EOS]
[StructuresHelp: B$ 'Structures' EOS]
[HelPFiles: B$ 'HelpFiles' EOS]
[DisMap: B$ 'Disassembler_Flags' EOS]
[UnusedSymbolsHelp: B$ 'Unused_symbols_Scanner' EOS]

[ContextHlpMessage: B$ 'Run [Configuration]/|Files Locations]/[RosAsm]     ' EOS]

;[ContextHelp | Call 'SHELL32.ShellExecuteA' D$H.MainWindow open, B_U_AsmName, #1, &NULL, &SW_SHOWNORMAL
; cmp eax 32 | ja M0>
;    Call 'USER32.MessageBoxA' D$H.MainWindow ContextHlpMessage, FileNotFound, &MB_ICONINFORMATION+&MB_SYSTEMMODAL
; M0:]

Proc Help:
    Arguments @HelpFile, @Page, @ErrorMessage

        Call 'SHELL32.ShellExecuteA' D$H.MainWindow, open, D@HelpFile, D@Page, &NULL,
                                     &SW_SHOWNORMAL
        If eax <= 32
            Push eax
                Call 'USER32.MessageBoxA' D$H.MainWindow, D@ErrorMessage, FileNotFound,
                                          &MB_ICONINFORMATION+&MB_SYSTEMMODAL
            Pop eax
        End_If
EndP


; Menu is build at 'AddUserMenu'

UserDefinedAction:
    sub eax 2000  ; eax = 0 to 8
    Mov ecx (64*4) | mul ecx
    add eax UserMenu0Path

    Call 'SHELL32.ShellExecuteA' D$H.MainWindow, open, eax, NoParameter, &NULL, &SW_SHOWDEFAULT
ret

______________________________________________________________________________________

; Show a Box with Ascii codes in decimal and Hexa:

[AsciiData: B$ ? # 2860]

[AsciiDialog: D$ 090CC08C2 0   ; Style
 U$ 01 0 0 01E6 0109           ; Dim
 0                             ; Menu
 0                             ; Class(not yet)
 "Ascii Table" 0                ; Title
 08 'Helv' 0]                  ; Font

[@AC0: D$ 0508008C4 0           ; Style
 U$ 1 1 01E5 0108              ; Dim
 0100                          ; ID
 0FFFF 081                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data


; Build a text table to send to Edit Control:

AsciiTable:
    If D$H.AsciiDialog > 0
        Call Beep | ret
    End_If

    Push ebp
        Mov edi AsciiData, ebx 0, ecx 10, ebp 0
        Mov al TAB | stosb
        .While ebp < 256
            Mov al bl | On al < 32, Mov al 2 | stosb
            Mov al TAB | stosb
            Mov eax ebx | add edi 2
            Mov edx 0 | div ecx
            add dl '0' | Mov B$edi dl
            dec edi
            Mov edx 0 | div ecx
            add dl '0' | Mov B$edi dl
            dec edi
            Mov edx 0 | div ecx
            add dl '0' | Mov B$edi dl
            add edi 3 | Mov al TAB | stosb

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
            Mov al TAB | stosb | jmp L3>
L2:         Mov al TAB | stosb | stosb
L3:         inc ebp
        .End_While

        Mov eax 0 | stosd
       ; Show resulting box:
        Call 'USER32.DialogBoxIndirectParamA' D$H.Instance, AsciiDialog, 0, AsciiProc, 0
    Pop ebp
ret


[TabDimPtr: D$ 10]
[H.AsciiDialog: D$ ?]

Proc AsciiProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    .If D@msg = &WM_COMMAND
        If D@wParam = &IDCANCEL
            Mov D$H.AsciiDialog 0
            Call 'USER32.EndDialog' D@hwnd 0
        Else
            Call 'USER32.GetDlgItem' D@hwnd 0100
            Call 'USER32.SendMessageA' eax &EM_SETSEL  0-1 0
        End_If

    .Else_If D@msg = &WM_INITDIALOG
        Move D$H.AsciiDialog D@hwnd
        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$STRUC.WINDOWCLASS@hIcon
        Call 'USER32.GetDlgItem' D@hwnd 0100
        Call 'USER32.SendMessageA' eax &EM_SETTABSTOPS 1 TabDimPtr
        Call 'USER32.SetDlgItemTextA' D@hwnd 0100 AsciiData

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
 @PD_lStructSize: len
 @PD_hWndOwner: 0
 @PD_hDevMode: 0
 @PD_hDevNames: 0
 @PD_hDC: 0
 @PD_Flags: &PD_RETURNDC+&PD_SELECTION
 @PD_nFromPage: W$ 0
 @PD_nToPage: 0
 @PD_nMinPage: 0
 @PD_nMaxPage: 0
 @PD_nCopies: 1
 @PD_hInstance: D$ 0
 @PD_lCustData: 0
 @PD_lpfnPrintHook: 0
 @PD_lpfnSetupHook: 0
 @PD_lpPrintTemplateName: 0
 @PD_lpPrintSetupTemplateName: 0
 @PD_hPrintTemplate: 0
 @PD_hSetupTemplate: 0]

; Needed for 'GDI32.StartDocA' (no interrest for us):

[DocName: B$ ' - Poor text Printer -' EOS]

[DOCINFO:
 @DI_cbSize: len
 @DI_lpszDocName: DocName
 @DI_lpszOutput: 0
 @DI_lpszDatatype: 0
 @DI_fwType: 0]

; This is for Call 'Comdlg32.ChooseFontA'. Important:

;[CF_PRINTERFONTS 2  CF_FIXEDPITCHONLY 04000  CF_INITTOLOGFONTSTRUCT 040]


; Default font for choose-font-box (Courier new 14).
; LOGFONTA Structure (Pointed by record 4 in CHOOSEFONT):

[CbBuffer:
 @lfHeight: D$ 0FFFF_FFED    ; (-19) (for details, see "LOGFONT" in win32.hlp)
 @lfWidth: D$ 0
 @lfEscapement: D$ 0
 @lfOrientation: D$ 0
 @lfWeight: D$ 0190
 @lfItalic: B$ 0
 @lfUnderline: B$ 0
 @lfStrikeOut: B$ 0
 @lfCharSet: B$ 0
 @lfOutPrecision: B$ 03
 @lfClipPrecision: B$ 02
 @lfQuality: B$ 01
 @lfPitchAndFamily: B$ 031
 @lfFaceName: B$ 'Courier New' EOS
 @CBBsecurityTail: 0 # 5] ; 32 Bytes > Total lenght = 60 Bytes

[PrintTextMetric: D$ ? # 20]

[CHOOSEFONT:
 @CF_lStructSize: Len
 @CF_hWndOwner: 0
 @CF_hDC: 0
 @CF_lpLogFont: cbbuffer
 @CF_iPointSize: 0
 @CF_Flags: &CF_PRINTERFONTS+&CF_FIXEDPITCHONLY+&CF_INITTOLOGFONTSTRUCT
 @CF_rgbColors: 0
 @CF_lCustData: 0
 @CF_lpfnHook: 0
 @CF_lpTemplateName: 0
 @CF_hInstance: 0
 @CF_lpszStyle: 0
 @CF_nFontType: W$ 0
 @CF_Alignment:  0
 @CF_nSizeMin: D$ 3
 @CF_nSizeMax: 0ff]

[UserChoosenPrinterFont: D$ ?]

; Our pointers:
[PrintStartPtr: D$ ?
 PrintEndPtr: D$ ?
 CharHight: D$ ?
 PageHight: D$ ?
 PrinterDCavailable: D$ ?]

Print:
    On D$SourceLen = 0, ret

    If B$PrinterDCavailable = &TRUE
        Call 'GDI32.DeleteDC' D$PRINTDLG@PD_hDC
        Call 'GDI32.DeleteObject' D$UserChoosenPrinterFont
    End_If

    If D$FL.BlockInside = &TRUE
        Mov D$PRINTDLG@PD_Flags &PD_RETURNDC+&PD_SELECTION
    Else
        Mov D$PRINTDLG@PD_Flags &PD_RETURNDC+&PD_NOSELECTION
    End_If

    Call 'Comdlg32.PrintDlgA' PRINTDLG | On eax = 0, ret    ; The Printer dialog sets
    Move D$CHOOSEFONT@CF_hDC D$PRINTDLG@PD_hDC,  D$CHOOSEFONT@CF_hWndOwner D$H.MainWindow          ; the DC handle in PRINTDLG struc
    If D$PRINTDLG@PD_Flags = &PD_RETURNDC+&PD_SELECTION

        Move D$PrintStartPtr D$LP.BlockStartText,
             D$PrintEndPtr D$LP.BlockEndText

    Else

        Mov eax D$STRUCT.EditData@UpperLine,
            D$PrintStartPtr eax

        add eax D$SourceLen | Mov D$PrintEndPtr eax

    End_If

    Call 'Comdlg32.ChooseFontA' CHOOSEFONT | On eax = 0, ret    ; user sets the font:
    Call 'GDI32.CreateFontIndirectA' cbbuffer
        Mov D$UserChoosenPrinterFont eax

    Call UpdateRegistry                                 ; To save last used Printer Font.

StartControlP:
    Call 'GDI32.GetDeviceCaps' D$PRINTDLG@PD_hDC &VERTRES | Mov D$PageHight eax   ; we set the sizes:
    Call 'GDI32.GetTextMetricsA' D$PRINTDLG@PD_hDC PrintTextMetric
        Mov eax D$PrintTextMetric, D$CharHight eax
        shl eax 4 | sub D$PageHight eax                 ; sub 16 lines for top/bottom blanks

    Call 'GDI32.StartDocA' D$PRINTDLG@PD_hDC  DOCINFO | On eax le 0, jmp L7>>; we print:
        Call 'GDI32.StartPage' D$PRINTDLG@PD_hDC | On eax le 0, jmp L7>>
            Call 'GDI32.SelectObject' D$PRINTDLG@PD_hDC D$UserChoosenPrinterFont

          Mov esi D$PrintStartPtr, edx 0
L0:       Mov ebx esi, ecx 0
L1:       cmp B$esi SPC | jb L2>
            lodsb | inc ecx | cmp esi D$PrintEndPtr | jae L3>
              jmp L1<
L2:       add esi 2                                                      ; strip CR/LR
L3:       Push esi, edx
            On ecx > 0
              Call 'USER32.TabbedTextOutA'  D$PRINTDLG@PD_hDC 0 edx ebx ecx 0,0,0
            End_If
          Pop edx esi
          add edx D$CharHight
          If edx >= D$PageHight                                          ; Next page?:
            pushad
              Call 'GDI32.EndPage' D$PRINTDLG@PD_hDC | On eax le 0, jmp L7>>
                Call 'GDI32.StartPage' D$PRINTDLG@PD_hDC | On eax le 0, jmp L7>>
                  Call 'GDI32.SelectObject' D$PRINTDLG@PD_hDC D$UserChoosenPrinterFont
            popad
            Mov edx 0
          End_If
          On esi < D$PrintEndPtr, jmp L0<<

        Call 'GDI32.EndPage' D$PRINTDLG@PD_hDC | On eax le 0, jmp L7>>
      Call 'GDI32.EndDoc' D$PRINTDLG@PD_hDC
    Mov B$PrinterDCavailable &TRUE | ret

L7: Call 'GDI32.AbortDoc' D$PRINTDLG@PD_hDC
    Call 'GDI32.DeleteObject' D$UserChoosenPrinterFont
    Call 'GDI32.DeleteDC' D$PRINTDLG@PD_hDC

    Mov B$PrinterDCavailable &FALSE
ret


ControlP:

    If D$FL.BlockInside = &FALSE

        ret

    Else_If B$PrinterDCavailable = &FALSE

        jmp Print

    Else

        Move D$PrintStartPtr D$LP.BlockStartText,
             D$PrintEndPtr D$LP.BlockEndText

        jmp StartControlP

    End_If
____________________________________________________________________________________________
____________________________________________________________________________________________

; Tool for viewing the System Resources and retrieving the IDs.

ViewSysResources:
    If D$H.ViewSysResources = 0
        Call 'USER32.DialogBoxParamA' D$H.Instance 29000 &NULL ViewSysResourcesProc &NULL
    Else
        Call Beep
    End_If
ret


[H.ViewSysResources: D$ ?
 H.SysBitMaps: D$ ?
 H.SysIcons: D$ ?
 H.SysCursors: D$ ?]

Proc ViewSysResourcesProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_INITDIALOG
        Move D$H.ViewSysResources D@hwnd
        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$STRUC.WINDOWCLASS@hIcon
        Call 'USER32.GetDlgItem' D@hwnd 10 | Mov D$H.SysBitMaps eax
        Call 'USER32.GetDlgItem' D@hwnd 11 | Mov D$H.SysIcons eax
        Call 'USER32.GetDlgItem' D@hwnd 12 | Mov D$H.SysCursors eax
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

                Mov eax SysButtonText, D$LP.BlockStartText eax

                While B$eax > 0 | inc eax | End_While

                dec eax

                Mov D$LP.BlockEndText eax,
                    D$FL.BlockInside &TRUE

                Call ControlC | Mov D$FL.BlockInside &FALSE

            End_If

L1:         Mov D$H.ViewSysResources 0
            Call 'USER32.EndDialog' D@hwnd 0

        ..End_If

    ...Else_If D@msg = &WM_CTLCOLOREDIT
        jmp L1>

    ...Else_If D@msg = &WM_CTLCOLORLISTBOX
L1:     Call 'GDI32.SetBkColor' D@wParam D$ARVB.DialogsBackColor
        popad | Mov eax D$H.DialogsBackGroundBrush | jmp L9>

    ...Else
L8:     popad | Mov eax &FALSE | jmp L9>

    ...End_If

    popad | Mov eax &TRUE

L9: EndP


[SetOKButtonSysText | Mov edi #1 | Mov edx #2 | Call OKButtonSysText]

; Example: edi > SysBitMapsStrings // eax = zero Based Indice

[SysButtonText: D$ ? # 12]
[SysBitMapApiCall: B$ "Call 'USER32.LoadBitmapA' &&NULL &&" EOS
 SysIconApiCall: B$ "Call 'USER32.LoadIconA' &&NULL &&" EOS
 SysCursorApiCall: B$ "Call 'USER32.LoadCursorA' &&NULL &&" EOS]

OKButtonSysText:
    Push eax, ebx
        .While eax > 0
            While B$edi <> 0 | inc edi | End_While
            inc edi | dec eax
        .End_While

        Push edi
            Mov edi SysButtonText
            If edx = SysBitMapsIDs
                Mov esi SysBitMapApiCall
            Else_If edx = SysIconsIDs
                Mov esi SysIconApiCall
            Else
                Mov esi SysCursorApiCall
            End_If
            While B$esi <> 0 | movsb | End_While
        Pop esi

      ; Copy the Equate in the Button Buffer:
        inc esi
        While B$esi <> 0 | movsb | End_While
        Mov D$edi ' ; =', B$edi+4 SPC | add edi 5

    Pop ebx, eax
    Push eax, ebx
        Mov eax D$edx+eax*4 | Call WriteEax | Mov B$edi 0
    Pop ebx, eax
ret

[SysBitMapsStrings:
 B$ '&OBM_CLOSE' EOS
 B$ '&OBM_UPARROW' EOS
 B$ '&OBM_DNARROW' EOS
 B$ '&OBM_RGARROW' EOS
 B$ '&OBM_LFARROW' EOS
 B$ '&OBM_REDUCE' EOS
 B$ '&OBM_ZOOM' EOS
 B$ '&OBM_RESTORE' EOS
 B$ '&OBM_REDUCED' EOS
 B$ '&OBM_ZOOMD' EOS
 B$ '&OBM_RESTORED' EOS
 B$ '&OBM_UPARROWD' EOS
 B$ '&OBM_DNARROWD' EOS
 B$ '&OBM_RGARROWD' EOS
 B$ '&OBM_LFARROWD' EOS
 B$ '&OBM_MNARROW' EOS
 B$ '&OBM_COMBO' EOS
 B$ '&OBM_UPARROWI' EOS
 B$ '&OBM_DNARROWI' EOS
 B$ '&OBM_RGARROWI' EOS
 B$ '&OBM_LFARROWI' EOS
 B$ '&OBM_SIZE' EOS
 B$ '&OBM_BTSIZE' EOS
 B$ '&OBM_CHECK' EOS
 B$ '&OBM_CHECKBOXES' EOS
 B$ '&OBM_BTNCORNERS' EOS
 B$ 0]

[SysBitMapsIDs:
 &OBM_CLOSE
 &OBM_UPARROW
 &OBM_DNARROW
 &OBM_RGARROW
 &OBM_LFARROW
 &OBM_REDUCE
 &OBM_ZOOM
 &OBM_RESTORE
 &OBM_REDUCED
 &OBM_ZOOMD
 &OBM_RESTORED
 &OBM_UPARROWD
 &OBM_DNARROWD
 &OBM_RGARROWD
 &OBM_LFARROWD
 &OBM_MNARROW
 &OBM_COMBO
 &OBM_UPARROWI
 &OBM_DNARROWI
 &OBM_RGARROWI
 &OBM_LFARROWI
 &OBM_SIZE
 &OBM_BTSIZE
 &OBM_CHECK
 &OBM_CHECKBOXES
 &OBM_BTNCORNERS]

[SysIconsStrings:
 B$ '&IDI_APPLICATION' EOS
 B$ '&IDI_HAND' EOS
 B$ '&IDI_QUESTION' EOS
 B$ '&IDI_EXCLAMATION' EOS
 B$ '&IDI_ASTERISK' EOS
 B$ '&IDI_WINLOGO' EOS
 B$ 0]

[SysIconsIDs:
 &IDI_APPLICATION
 &IDI_HAND
 &IDI_QUESTION
 &IDI_EXCLAMATION
 &IDI_ASTERISK
 &IDI_WINLOGO]

[SysCursorsStrings:
 B$ '&IDC_ARROW' EOS
 B$ '&IDC_IBEAM' EOS
 B$ '&IDC_WAIT' EOS
 B$ '&IDC_CROSS' EOS
 B$ '&IDC_UPARROW' EOS
 B$ '&IDC_SIZENWS' EOS
 B$ '&IDC_SIZENESW' EOS
 B$ '&IDC_SIZEWE' EOS
 B$ '&IDC_SIZENS' EOS
 B$ '&IDC_SIZEALL' EOS
 B$ '&IDC_NO' EOS
 B$ '&IDC_APPSTARTING' EOS
 B$ '&IDC_HELP' EOS
 0]

[SysCursorsIDs:
 &IDC_ARROW
 &IDC_IBEAM
 &IDC_WAIT
 &IDC_CROSS
 &IDC_UPARROW
 &IDC_SIZENWSE
 &IDC_SIZENESW
 &IDC_SIZEWE
 &IDC_SIZENS
 &IDC_SIZEALL
 &IDC_NO
 &IDC_APPSTARTING
 &IDC_HELP]

InitViewSysResourcesListBoxes:
    Mov edi SysBitMapsStrings
    While B$edi <> 0
        Push edi
        Call 'USER32.SendMessageA' D$H.SysBitMaps &LB_ADDSTRING 0 edi
        Pop edi
        Mov al 0, ecx 0FF | repne scasb
    End_While

    Mov edi SysIconsStrings
    While B$edi <> 0
        Push edi
        Call 'USER32.SendMessageA' D$H.SysIcons &LB_ADDSTRING 0 edi
        Pop edi
        Mov al 0, ecx 0FF | repne scasb
    End_While

    Mov edi SysCursorsStrings
    While B$edi <> 0
        Push edi
        Call 'USER32.SendMessageA' D$H.SysCursors &LB_ADDSTRING 0 edi
        Pop edi
        Mov al 0, ecx 0FF | repne scasb
    End_While
ret

;;
____________________________________________________________________________________________
____________________________________________________________________________________________

 Used DLL in the actual Source.
;;


ShowSourceImports:
    If D$H.ImportDialog = 0
        Call 'USER32.DialogBoxParamA' D$H.Instance, 1100, &NULL,
                                      ViewSourceImportsProc, &NULL
    End_If
ret


ShowSourceExports:
    If D$H.ExportDialog = 0
        Call 'USER32.DialogBoxParamA' D$H.Instance, 1101, &NULL,
                                      ViewSourceExportsProc, &NULL
    End_If
ret


[H.DLLsProcList: D$ ?
 H.DLLsProcFunctionsList: D$ ?]

Proc Enable:

    Arguments @H.Parent,
              @ID

    Call 'USER32.GetDlgItem' D@H.Parent,
                             D@ID

    Call 'USER32.EnableWindow' eax,
                               &TRUE

EndP

Proc Disable:

    Arguments @H.Parent,
              @ID

    Call 'USER32.GetDlgItem' D@H.Parent,
                             D@ID

    Call 'USER32.EnableWindow' eax,
                               &FALSE

EndP


[H.ImportDialog: D$ ?]

; Tag Dialog 1100

Proc ViewSourceImportsProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_COMMAND
        Mov eax D@wParam | and eax 0FFFF
        ..If eax = &IDCANCEL
            Mov D$H.ImportDialog 0
            Call 'USER32.EndDialog' D@hwnd, 0

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
                    Call 'USER32.SendMessageA' D$H.DLLsProcFunctionsList,
                                               &LB_GETCURSEL, 0, 0

                    Call 'USER32.SendMessageA' D$H.DLLsProcFunctionsList,
                                               &LB_GETITEMDATA, eax, 0

                    Call 'USER32.SetDlgItemInt' D@hwnd, 12, eax, &FALSE
                    Call 'USER32.SendDlgItemMessageA' D@hwnd, 3, &WM_ENABLE,
                                                      &TRUE, 0
                    Call Enable D@hwnd, 3
                    Call Disable D@hwnd, 4

                End_If

            .Else_If W@wParam = 3
                Call ImportFunctionFindFirst

                Call 'USER32.SendMessageA' D$H.DLLsProcFunctionsList, &LB_GETCURSEL, 0, 0
                Call 'USER32.SendMessageA' D$H.DLLsProcFunctionsList, &LB_GETITEMDATA,
                                           eax, 0
                On eax > 1, Call Enable D@hwnd, 4

            .Else_If W@wParam = 4
                Call ImportFunctionFindNext

            .End_If
        ..End_If

    ...Else_If D@msg = &WM_INITDIALOG
        Move D$H.ImportDialog D@hwnd

        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$STRUC.WINDOWCLASS@hIcon

        Call 'USER32.GetDlgItem' D@hwnd, 10 | Mov D$H.DLLsProcList eax
        Call 'USER32.GetDlgItem' D@hwnd, 11 | Mov D$H.DLLsProcFunctionsList eax

        Call Disable D@hwnd, 3
        Call Disable D@hwnd, 4

        Call RestoreRealSource
        Call InitImportsProcList
        Call SetPartialEditionFromPos

        If B$DLLsFoundInSource = &FALSE
            Call 'USER32.MessageBoxA' D$H.MainWindow, {B$ 'No Import Function found in this Source' EOS},
                                     {B$ 'Failure:' EOS}, 0
            Call 'USER32.EndDialog' D@hwnd, 0
        End_If

        popad | Mov eax &FALSE | ExitP

    ...Else
        popad | Mov eax &FALSE | ExitP

    ...End_If

L8: popad | Mov eax &TRUE
EndP


ExportFunctionFindFirst:
    Call 'USER32.SendMessageA' D$H.DLLsProcList, &LB_GETCURSEL, 0, 0
    Call 'USER32.SendMessageA' D$H.DLLsProcList, &LB_GETTEXT, eax, SearchString

    .If eax <> &LB_ERR
        Mov W$SearchString+eax '::' | add eax 2 | jmp L1>

ImportFunctionFindFirst:
    Call 'USER32.SendMessageA' D$H.DLLsProcFunctionsList, &LB_GETCURSEL, 0, 0
    Call 'USER32.SendMessageA' D$H.DLLsProcFunctionsList, &LB_GETTEXT, eax, SearchString

    .If eax <> &LB_ERR
        Mov D$SearchString+eax "'" | inc eax
L1:     Mov D$LenOfSearchedString eax

        Call RestoreRealSource

        Push D$DownSearch, D$CaseSearch, D$WholeWordSearch
            Mov B$DownSearch &TRUE, B$CaseSearch &TRUE, B$WholeWordSearch &FALSE
            Move D$STRUCT.EditData@CurrentWritingPos D$CodeSource
            Call SetCaret D$CodeSource | Move D$STRUCT.EditData@UpperLine D$CodeSource
            Call AskForRedrawNow

                Mov D$NextSearchPos 0

                Call StringSearch

        Pop D$WholeWordSearch, D$CaseSearch, D$DownSearch

        Call SetPartialEditionFromPos

    .End_If
ret


ImportFunctionFindNext:

    Call RestoreRealSource
        Push D$DownSearch, D$CaseSearch, D$WholeWordSearch
            Call StringSearch
        Pop D$WholeWordSearch, D$CaseSearch, D$DownSearch
    Call SetPartialEditionFromPos
ret


[DLLsFoundInSource: D$ ?]

InitImportsProcList:
    Mov esi D$CodeSource, edx D$STRUCT.EditData@SourceEnd, B$DLLsFoundInSource &FALSE

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
                While B$esi >= SPC | inc esi | End_While

        ..Else
            or eax 020202020
            If eax = 'Call'
                add esi 4
            Else_If eax = 'jmp '
                add esi 3
            Else
                jmp L8>>
            End_If

            While B$esi = SPC | inc esi | End_While
            Mov al B$esi
            .If al = '"'
                jmp L1>
            .Else_If al = "'"
L1:             inc esi | Mov ebx esi
                While B$esi <> al
                    On B$esi <= SPC, jmp L2>
                    If B$esi = '.'
                        Mov B$DLLsFoundInSource &TRUE
                        Push eax, edx, D$esi, esi
                            Mov B$esi 0
                            Push ebx
                             Call Lb_FindString D$H.DLLsProcList, ebx
         ; Call 'USER32.SendMessageA' D$H.DLLsProcList, &LB_FINDSTRING, 0, ebx
                            Pop ebx
                            On eax = &LB_ERR,
                            Call 'USER32.SendMessageA' D$H.DLLsProcList,
                                                       &LB_ADDSTRING, 0, ebx
                        Pop esi, D$esi, edx, eax
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
    Mov esi D$CodeSource, edx D$STRUCT.EditData@SourceEnd, B$DLLsFoundInSource &FALSE

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
                While B$esi >= SPC | inc esi | End_While

        ..Else_If ax = '::'
                        Mov B$DLLsFoundInSource &TRUE
                        Push eax, edx, D$esi, esi
                            Mov B$esi 0
                            While B$esi-1 > SPC | dec esi | End_While

                            Call 'USER32.SendMessageA' D$H.DLLsProcList,
                                                       &LB_ADDSTRING, 0, esi
                        Pop esi, D$esi, edx, eax
        ..End_If

        inc esi

    .End_While
L9: ret


[CurrentSelectionInDLLsList: D$ ?]

ViewDllFunctionList:
    Call 'USER32.SendMessageA' D$H.DLLsProcFunctionsList, &LB_RESETCONTENT, 0, 0

    Call 'USER32.SendMessageA' D$H.DLLsProcList, &LB_GETCURSEL, 0, 0
    Mov D$CurrentSelectionInDLLsList eax

    Mov esi D$CodeSource, edx D$STRUCT.EditData@SourceEnd

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
                While B$esi >= SPC | inc esi | End_While

        ...Else_If eax = 'jmp '
            add esi 3 | jmp L1>

        ...Else_If eax = 'Call'
            add esi 4
L1:         While B$esi = SPC | inc esi | End_While
            Mov al B$esi
            ..If al = "'"
L1:             inc esi | Mov ebx esi
                .While B$esi <> al
                    On B$esi <= SPC, jmp L2>>
                    .If B$esi = '.'
                        Push eax, ebx, edx, D$esi, esi
                            Mov B$esi 0
                                Call Lb_FindString D$H.DLLsProcList, ebx
                            Mov ecx eax
                        Pop esi, D$esi, edx, ebx, eax

                        On ecx <> D$CurrentSelectionInDLLsList, jmp L2>>

                            inc esi | Mov ebx esi

                            While B$esi <> al
                                inc esi | On esi >= edx, jmp L9>>
                            End_While
                            Push D$esi, esi, edx
                                Mov B$esi 0
                                Push ebx
                                    Call Lb_FindString D$H.DLLsProcFunctionsList, ebx
                                Pop ebx

                                If eax = &LB_ERR
                                    Call 'USER32.SendMessageA' D$H.DLLsProcFunctionsList,
                                                               &LB_ADDSTRING, 0, ebx
                                    Call 'USER32.SendMessageA' D$H.DLLsProcFunctionsList,
                                                               &LB_SETITEMDATA, eax, 1
                                Else
                                    Push eax
                                    Call 'USER32.SendMessageA' D$H.DLLsProcFunctionsList,
                                                               &LB_GETITEMDATA, eax, 0
                                    inc eax
                                    Pop edx
                                    Call 'USER32.SendMessageA' D$H.DLLsProcFunctionsList,
                                                               &LB_SETITEMDATA, edx, eax
                                End_If

                            Pop edx, esi, D$esi
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

    Argument @H.Lb, @Pointer
    Local @FirstFound
    Uses esi, edi, edx

        Mov edx 0-1, D@FirstFound edx

L0:     Push edx
            Call 'USER32.SendMessageA' D@H.Lb, &LB_FINDSTRING, edx, D@Pointer
        Pop edx

        .If eax <> &LB_ERR
            Push eax
                Call 'USER32.SendMessageA' D@H.Lb, &LB_GETTEXT, eax, SearchString
            Pop edx

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
            End_If

        .End_If
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[H.ExportDialog: D$ ?]

; Tag Dialog 1101

Proc ViewSourceExportsProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_COMMAND
        Mov eax D@wParam | and eax 0FFFF

        ..If eax = &IDCANCEL
            Mov D$H.ExportDialog 0
            Call 'USER32.EndDialog' D@hwnd, 0

        ..Else
            .If W@wParam = 10
                If W@wParam+2 = &LBN_SELCHANGE
                    Call ExportFunctionFindFirst
                End_If

            .End_If

        ..End_If

    ...Else_If D@msg = &WM_INITDIALOG
        Move D$H.ExportDialog D@hwnd

        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$STRUC.WINDOWCLASS@hIcon

        Call 'USER32.GetDlgItem' D@hwnd, 10 | Mov D$H.DLLsProcList eax

        Call RestoreRealSource
        Call InitExportsProcList
        Call SetPartialEditionFromPos

        If B$DLLsFoundInSource = &FALSE
            Call 'USER32.MessageBoxA' D$H.MainWindow, {B$ 'No Export Function found in this Source' EOS},
                                     {B$ 'Failure:' EOS}, 0
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
        Push D$UserPeStart
            Move D$UserPeStart D$SfFileMemory

            Call StartScan

            .If eax <> 0
                Call ReAlignPE | Call GetExportDirPointer

                If D$NumberOfDisExportedFunctions <> 0
                    Call 'USER32.DialogBoxParamA' D$H.Instance, 30500, &NULL,
                                                  ViewExport, &NULL

                Else
                    jmp L7>

                End_If
            .Else
L7:             Call 'USER32.MessageBoxA' D$H.MainWindow, {B$ 'No Export found in this File' EOS}, SfFile, 0

            .End_If

            Call VirtualFree UserPeStart
          ; D$UserPeStart and not D$SfFileMemory, because of the switch in 'ReAlignPE'

        Pop D$UserPeStart

    ..End_If
ret


; Simplified version of 'StartNewDisFile', reuse the same Variables:

StartScan:
    Mov edi FirstDisVirtualData, eax 0 | while edi < LastDisVirtualData | stosd | End_While

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
                Move D$DisRvaSectionAlignment D$eax+4, D$DisFileSectionAlignment D$eax+8
            End_If

        .Else
            Mov eax 0

        .End_If

    ..End_If
ret


; SImplified version of 'CheckExport':

GetExportDirPointer:
    GetPeHeader SectionTable | Mov edx D$eax

    Mov D$NumberOfDisExportedFunctions 0

    If edx <> 0
        Mov ecx D$DisNumberOfSections

        GetPeHeader SectionsHeaders

L0:     Mov ebx D$eax+SECTION_RVA |  cmp edx ebx | jb L1>
            add ebx D$eax+SECTION_FILESIZE | cmp edx ebx | jb L2>

L1:     add eax SECTIONHEADERSIZE | loop L0<
            ret

L2:     add edx D$UserPeStart | Mov eax D$edx+(5*4), ebx D$edx+(6*4)
        On ebx > eax, Mov eax ebx
        Move D$NumberOfDisExportedFunctions eax
    End_If
ret


[OPENFILENAME.SF:
 @lStructSize: D$ len
 @hwndOwner: D$ 0
 @hInstance: D$ 0
 @lpstrFilter: D$ SfFilter
 @lpstrCustomFilter: D$ 0
 @nMaxCustFilter: D$ 0
 @nFilterIndex: D$ 1
 @lpstrFile: D$ SfFile
 @nMaxFile: D$ &MAX_PATH
 @lpstrFileTitle: D$ 0
 @nMaxFileTitle: D$ 0
 @lpstrInitialDir: D$ 0
 @lpstrTitle: D$ 0
 @Flags: D$ &OFN_CREATEPROMPT__&OFN_EXPLORER__&OFN_HIDEREADONLY__&OFN_LONGNAMES__&OFN_NONETWORKBUTTON__&OFN_OVERWRITEPROMPT__&OFN_PATHMUSTEXIST
 @nFileOffset: W$ 0
 @nFileExtension: W$ 0
 @lpstrDefExt: D$ 0
 @lCustData: D$ 0
 @lpfnHook: D$ 0
 @lpTemplateName: D$ 0]

[SfFilter: B$ 'Module Name' EOS, '*.*' EOS, 0]
[SfFile: B$ ? # &MAX_PATH]
[H.SfFile: D$ ?
 SfFileLen: D$ ?
 SfFileMemory: D$ ?
 SfFileMemoryEnd: D$ ?]

GetExportScannerFile:
    Mov D$SfFile 0, D$SfFileMemory 0
    Move D$OPENFILENAME.SF@hwndOwner D$H.MainWindow, D$OPENFILENAME.SF@hInstance D$H.Instance
    Call 'Comdlg32.GetOpenFileNameA' OPENFILENAME.SF

    ..If D$SfFile <> 0
        Call 'KERNEL32.CreateFileA' SfFile,
                                    &GENERIC_READ, &FILE_SHARE_READ__&FILE_SHARE_WRITE,
                                    0, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, &NULL

        .If eax = &INVALID_HANDLE_VALUE

            Call MessageBox D$BusyFilePtr

ret

        .Else
            Mov D$H.SfFile eax
            Call 'KERNEL32.GetFileSize' eax, 0 | Mov D$SfFileLen eax

            Call VirtualAlloc SfFileMemory,
                              D$SfFileLen

            Mov eax D$SfFileMemory | add eax D$SfFileLen | Mov D$SfFileMemoryEnd eax

            Call 'KERNEL32.ReadFile' D$H.SfFile, D$SfFileMemory D$SfFileLen,
                                     NumberOfReadBytes, 0

            Call 'KERNEL32.CloseHandle' D$H.SfFile

          ; Is it a PE?
            Mov eax D$SfFileMemory | On W$eax <> 'MZ', jmp L7>
            sub eax DosHeader | add eax PeHeaderPointer
            Mov ebx D$eax, eax D$SfFileMemory | add eax ebx
            On eax > D$SfFileMemoryEnd, jmp L7>
            On eax < D$SfFileMemory, jmp L7>
            On W$eax <> 'PE', jmp L7>

        .End_If
    ..End_If
ret

L7: Call MessageBox D$NotPeExePtr

    Call VirtualFree SfFileMemory

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

[H.SfList: D$ ?]

[ExportViewBuffer: D$ ? # 20]

[AfterExportViewBuffer: D$ ?]

; Tag Dialog 30500

Proc ViewExport:
    Arguments @hwnd, @msg, @wParam, @lParam
    Structure @RECT 16, @leftDis 0,  @topDis 4,  @rightDis 8,  @bottomDis 12

    pushad

    ...If D@msg = &WM_COMMAND
        Mov eax D@wParam | and eax 0FFFF
        ..If eax = &IDCANCEL
            Call 'USER32.EndDialog' D@hwnd, 0

        ..Else
            Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &EM_SETSEL, 0-1, 0

        ..End_If

    ...Else_If D@msg = &WM_INITDIALOG

        Call 'USER32.GetDlgItem' D@hwnd, 10 | Mov D$H.SfList eax

        Call 'USER32.GetClientRect' D$H.SfList D@RECT
        Mov eax D@rightDis | sub eax D@leftDis | Mov D@bottomDis eax
        shr eax 3 | Mov D$LV_COLUMN@cx eax | sub D@bottomDis eax

        Push eax
            Mov D$LV_COLUMN@pszText {B$ 'Ordinal' EOS}
            Call 'USER32.SendMessageA' D$H.SfList, &LVM_INSERTCOLUMN, 0, LV_COLUMN

            shl D$LV_COLUMN@cx 1 | Mov eax D$LV_COLUMN@cx | sub D@bottomDis eax
            Mov D$LV_COLUMN@pszText {B$ 'Relative Address' EOS}
            Call 'USER32.SendMessageA' D$H.SfList, &LVM_INSERTCOLUMN, 1, LV_COLUMN

            Move D$LV_COLUMN@cx D@bottomDis
        Pop eax

        Mov D$LV_COLUMN@pszText {B$ 'Name' EOS}
        Call 'USER32.SendMessageA' D$H.SfList, &LVM_INSERTCOLUMN, 2, LV_COLUMN

      ; 'ExportSectionComments'

        GetPeHeader SectionTable | Mov eax D$eax | add eax D$UserPeStart

        Push eax
            Mov eax D$eax+(3*4) | add eax D$UserPeStart
            If eax > D$UserPeStart
                On eax < D$UserPeEnd,
                        Call 'USER32.SendMessageA', D@hwnd, &WM_SETTEXT, 0, eax
            End_If
        Pop eax

        Push eax
            Mov ebx D$eax+(5*4), eax D$eax+(6*4)
            On ebx > eax, Mov eax ebx
            Call 'USER32.SendMessageA' D$H.SfList, &LVM_SETITEMCOUNT, eax, 0
        Pop eax

      ; Number of Functions:
        Mov ecx D$eax+(5*4) | On D$eax+(6*4) > ecx, Mov ecx D$eax+(6*4)
      ; Pointer to ExportAdressesTable:
        Mov ebx D$eax+(7*4) | On ebx <> 0, add ebx D$UserPeStart
      ; Pointer to ExportNamesTable:
        Mov esi D$eax+(8*4) | On esi <> 0, add esi D$UserPeStart
      ; Pointer to ExportOrdinals:
        Mov edx D$eax+(9*4) | On edx <> 0, add edx D$UserPeStart

        On esi = edx, Mov esi 0

        Mov D$LV_ITEM@iItem 0

L0:   ; Write the Ordinal:
        ..If edx <> 0
            Mov edi ExportViewBuffer, D$LV_ITEM@iSubItem 0
            Push eax, ebx
                movzx eax W$edx | add edx 2 | Call WriteEax | Mov B$edi 0
            Pop ebx, eax
            pushad
                Call 'USER32.SendMessageA' D$H.SfList, &LVM_INSERTITEM, 0, LV_ITEM
            popad
        ..End_If

      ; Write the Relative Address:
        ..If ebx <> 0
            Mov edi ExportViewBuffer
            Mov eax D$ebx
            If eax = 0
                Call 'USER32.SendMessageA' D$H.SfList, &LVM_DELETEITEM,
                                           D$LV_ITEM@iItem, 0
                jmp L7>>
            End_If
            add ebx 4  | Push ebx | Call WriteEax | Pop ebx
            Mov B$edi 0

            pushad
                Mov D$LV_ITEM@iSubItem 1
                Call 'USER32.SendMessageA' D$H.SfList, &LVM_SETITEM, 0, LV_ITEM
            popad
        ..End_If

      ; Write the Function Name:
        ..If esi <> 0
            Push esi
                Mov esi D$esi, edi ExportViewBuffer | add esi D$UserPeStart
                .If esi =< D$UserPeStart
                    Mov D$edi '???'
                .Else_If esi >= D$UserPeEnd
                    Mov D$edi '???'
                .Else
                    While B$esi <> 0
                        If B$esi < SPC
                           ; Call 'USER32.SendMessageA' D$H.SfList, &LVM_DELETEITEM,
                           ;                            D$LV_ITEM@iItem, 0
                           ; Pop esi | jmp L7>>
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
L2:         Pop esi
            add esi 4
            pushad
                Mov D$LV_ITEM@iSubItem 2
                Call 'USER32.SendMessageA' D$H.SfList, &LVM_SETITEM, 0, LV_ITEM
            popad
        ..End_If

        inc D$LV_ITEM@iItem | dec ecx | jnz L0<<

L7:     popad | Mov eax &FALSE | ExitP

    ...Else
        popad | Mov eax &FALSE | ExitP

    ...End_If

L8: popad | Mov eax &TRUE
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; GUIDs Stuff

[H.ShowGUIDs: D$ ?]

; All of this GUIDs stuff and the associated GUD file is to be used with:
;
; [COMCall | Mov eax D$#1 eax D$eax | Push #L>3 | Call D$eax+#1.#2 D$#1]

ViewGUIDs:
    If D$H.ShowGUIDs = 0
        Call 'USER32.CreateDialogParamA' D$H.Instance, 35, D$H.MainWindow, ShowGUIDsProc, &NULL
    End_If
ret


[GUIDsHelp: B$ 'GUIDs' EOS]

; Tag Dialog 35

Proc ShowGUIDsProc:
     Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ..If D@msg = &WM_COMMAND
        .If D@wParam = &IDCANCEL
L7:        Mov D$H.ShowGUIDs 0

           Call VirtualFree GUIDsFileMemory

           Call 'USER32.EndDialog' D@hwnd, 0

        .Else_If D@wParam = &IDOK
L8:         Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &LB_GETCURSEL, 0, 0
            Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &LB_GETTEXT, eax,
                                              STR.A.Trash

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
        Move D$H.ShowGUIDs D@hwnd
        Call 'USER32.GetDlgItem' D@hwnd, 10
        Call InitGUIDsView eax | On B$GUIDsInit = &FALSE, jmp L7<<

        If D$FL.SourceReady = &FALSE
            Call Disable D@hwnd 100
            Call Disable D@hwnd 101
        End_If

        On D$GUIDsPastingType = 0, Mov D$GUIDsPastingType 101

        If D$GUIDsPastingType = 100
            Call 'USER32.SendDlgItemMessageA' D@hwnd, 100, &BM_SETCHECK, &TRUE, 0
        Else
            Call 'USER32.SendDlgItemMessageA' D@hwnd, 101, &BM_SETCHECK, &TRUE, 0
        End_If

    ..Else_If D@msg = &WM_VKEYTOITEM
            If W@wParam = &VK_RETURN
                jmp L8<<

            Else_If W@wParam = &VK_ESCAPE
                jmp L7<<

            End_If

    ..Else_If D@msg = &WM_CTLCOLORLISTBOX
        Call 'GDI32.SetBkColor' D@wParam D$ARVB.DialogsBackColor
        popad | Mov eax D$H.DialogsBackGroundBrush | jmp L9>

    ..Else
        popad | Mov eax &FALSE | jmp L9>

    ..End_If

    popad | Mov eax &TRUE
L9: EndP
____________________________________________________________________________________________

[GUIDsFilePath: B$ ? # &MAX_PATH]

[GUIDsFileNamePointer: D$ ?
 H.GUIDsFile: D$ ?
 GUIDsFileLength: D$ ?
 GUIDsFileMemory: D$ ?
 GUIDsInit: D$ ?]

[MissingGUIDsFile: B$ "
Before using this Tool,
you have to save the GUIDs File(s)
in the RosAsmFiles Directory
(aside Equates.equ)
" EOS]

Proc InitGUIDsView:
    Argument @hwnd

    Mov esi IncludeFileName, edi GUIDsFilePath
    While B$esi <> 0 | movsb | End_While
    While B$edi-1 <> '\' | dec edi | End_While
    Mov D$GUIDsFileNamePointer edi
    Mov W$edi '*.', D$edi+2 'GUD'

    Call 'KERNEL32.FindFirstFileA' GUIDsFilePath, STRUC.FindFile

    ...If eax <> &INVALID_HANDLE_VALUE
        Call 'KERNEL32.FindClose' eax

        Mov esi STRUC.FindFile@cFileName, edi D$GUIDsFileNamePointer
        While B$esi <> 0 | movsb | End_While | Mov B$edi 0

        Call 'KERNEL32.CreateFileA' GUIDsFilePath, &GENERIC_READ,
                                    &FILE_SHARE_READ+&FILE_SHARE_WRITE, 0, &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_NORMAL, 0
        Mov D$H.GUIDsFile eax

        Call 'KERNEL32.GetFileSize' eax, 0 | Mov D$GUIDsFileLength eax

        Call VirtualAlloc GUIDsFileMemory,
                          eax

        Call 'KERNEL32.ReadFile' D$H.GUIDsFile, D$GUIDsFileMemory,
                                 eax, NumberOfReadBytes, 0

        Call 'KERNEL32.CloseHandle' D$H.GUIDsFile

        Mov esi D$GUIDsFileMemory, edx esi | add edx D$GUIDsFileLength

        .While esi < edx
            .If B$esi = '['
                inc esi
                If B$esi = '&'
                    ;While B$esi <> '_' | inc esi | End_While |
                    inc esi
                    Mov edi STR.A.Trash
                    While B$esi <> ':' | movsb | End_While | Mov B$edi 0
                    Push edx
                        Call 'USER32.SendMessageA' D@hwnd, &LB_ADDSTRING, 0, STR.A.Trash
                    Pop edx
                End_If
            .End_If

            inc esi

        .End_While

        Mov B$GUIDsInit &TRUE

    ...Else
        Call 'USER32.MessageBoxA' D$H.MainWindow, MissingGUIDsFile, {B$ 'File not found' EOS}, 0

    ...End_If
EndP
____________________________________________________________________________________________

[GUID_MotherNamePointer: D$ ?
 SourceComment: D$ ?
 GUIDsPastingType: D$ ?]

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
                Mov D$GUID_MotherNamePointer STR.A.Trash
                Mov edi STR.A.Trash, ecx D@Length | repe cmpsb | je L5>

            End_If

        .Else_If B$esi = ';'
          ; Example: "; SOURCE FILE -> strmf.h"
            If D$esi+2 = 'SOUR'
                On D$esi+6 = 'CE F', Mov D$SourceComment esi
            End_If

        .End_If

        inc esi

    .End_While

EndP

L5:

  ; ebx pointing to the wanted GUID.
    Mov esi ebx, edi STR.A.Trash

    If D$SourceComment <> 0
        Push esi
            Mov esi D$SourceComment
            While B$esi <> CR | movsb | End_While
            Mov D$edi CRLF2 | add edi 4
        Pop esi
    End_If

  ; Copy the GUID Data, with the Sizes Markers:
    Mov B$edi '[' | inc edi | add esi 2
    While B$esi <> ':' | movsb | End_While | Movsb

    Mov D$edi ' D$ ' | add edi 4
    While B$esi = SPC | inc esi | End_While
    Call MovsbHexa

    Mov D$edi ' W$ ' | add edi 4
    While B$esi = SPC | inc esi | End_While
    Call MovsbHexa

    Mov D$edi ' W$ ' | add edi 4
    While B$esi = SPC | inc esi | End_While
    Call MovsbHexa

    Mov D$edi ' B$ ' | add edi 4
    .While B$esi <> ']'
        While B$esi = SPC | inc esi | End_While
        Call MovsbHexa
        Mov B$edi SPC | inc edi
    .End_While
    Mov B$edi-1 ']'
    While B$esi <> CR | inc esi | End_While

    Mov D$edi CRLF2 | add edi 4
    While B$esi <= SPC  | inc esi | End_While

  ; Copy the other stuff:
    .While esi < edx
        If W$esi = '[.'
            Push edx
              ; Skip the "IID_" thingies:
                Mov eax D$GUID_MotherNamePointer
                While B$eax <> '_' | inc eax | End_While | inc eax
                Mov D$GUID_MotherNamePointer eax
                Call BuildGUIDvTable | Call InsertGUIDsObjectHandle
            Pop edx
        Else_If W$esi = '[&'
            jmp L5>
        Else_If B$esi = ';'
            While B$esi <> CR | inc esi | End_While | add esi 2
        Else
            movsb
        End_If
    .End_While

L5: Mov B$edi 0

    Push D$FL.BlockInside,
         D$LP.BlockEndText,
         D$LP.BlockStartText

        Mov D$FL.BlockInside &TRUE,
            D$LP.BlockStartText STR.A.Trash

        sub edi (1*ASCII) | Mov D$LP.BlockEndText edi

        Call ControlC

        If D$GUIDsPastingType = 101

            Mov D$FL.BlockInside &FALSE

            Call ControlV | Call AskForRedrawNow

        End_If

    Pop D$LP.BlockStartText,
        D$LP.BlockEndText,
        D$FL.BlockInside

EndP


InsertGUIDsObjectHandle:
  ; Insert the Object Handle Declaration:
    Mov D$edi CRLF2 | add edi 4

    Mov B$edi '[' | inc edi
    Push esi
        Mov esi D$GUID_MotherNamePointer
        ;While B$esi <> '_' | inc esi | End_While | inc esi
        While B$esi <> 0 | movsb | End_While
        Mov B$edi ':' | inc edi
    Pop esi
    Mov D$edi ' ?]' | add edi 3
ret
____________________________________________________________________________________________

; Just to turn all GUIDs' hexa numbers upper cases:

MovsbHexa:
L0: lodsb
    .If al > SPC
        If al = ']'
            dec esi | ret
        Else_If al < 'Z'

        Else
            sub al SPC
        End_If

        stosb | jmp L0<
    .End_If
ret
____________________________________________________________________________________________

[vTableQueryInterface: B$ '.QueryInterface' EOS]
[vTableAddRef: B$ '.AddRef' EOS]
[vTableRelease: B$ '.Release' EOS]

BuildGUIDvTable:
    Mov D$GUIDDisplacementEquate 0
  ; Copy the '[':
    movsb
    Push esi

        Push esi | Mov esi D$GUID_MotherNamePointer | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0: Pop esi

        Push esi | Mov esi vTableQueryInterface | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0: Pop esi

        Call WriteGUIDDisplacementEquate
        Mov B$edi SPC | inc edi

        Push esi | Mov esi D$GUID_MotherNamePointer | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0: Pop esi

        Push esi | Mov esi vTableAddRef | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0: Pop esi

        Call WriteGUIDDisplacementEquate
        Mov B$edi SPC | inc edi

        Push esi | Mov esi D$GUID_MotherNamePointer | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0: Pop esi

        Push esi | Mov esi vTableRelease | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0: Pop esi

        Call WriteGUIDDisplacementEquate
    Pop esi

    .While B$esi <> ']'
        Mov B$edi SPC | inc edi

        Push esi | Mov esi D$GUID_MotherNamePointer | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0: Pop esi

        While B$esi <> CR
            Mov al B$esi, B$edi al | inc edi | inc esi
            If al = ']'
                dec edi | Call WriteGUIDDisplacementEquate
                sub edi 2 | Mov B$edi ']' | inc edi | ret
            End_If
        End_While

        Call WriteGUIDDisplacementEquate

        While B$esi <= SPC | inc esi | End_While
    .End_While
ret
____________________________________________________________________________________________

[GUIDDisplacementEquate: D$ ?]

WriteGUIDDisplacementEquate:
    Push ebx, edx, esi
        Mov B$edi SPC | inc edi
        Mov eax D$GUIDDisplacementEquate
        Call WriteEax
        add D$GUIDDisplacementEquate 4
    Pop esi, edx, ebx
    Mov W$edi CRLF | add edi 2
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
