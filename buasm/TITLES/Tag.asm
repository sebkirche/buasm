TITLE Tag             ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;;
  Tag Feature:
  
  When a commented out 'Tag' KeyWord is found out (Right-Click feature), these Parsers
  are run and branced to the according associated action.
  
  First implementation, for test, if:
_________________

; Tag Dialog 1000
_________________

  'WizardTag' 'NewWizardForm'

  is written in some Source, when Right-Clicking on 'Tag', and if the Edited PE Resources
  contain a Dialog with an ID 1000 (Decimal only), the Dialog Editor should be run.
;;

____________________________________________________________________________________________
____________________________________________________________________________________________

; General purpose Routines: B$ 'IsItTag', 'TagParser', 'NoSuchTag', 'TagDecimalToBin', 'NewWizardForm', 'WizardFormTag'

; When called from 'RightClick' > 'InternalRightClick', esi+1 point to 'Tag '

IsItTag:
    Push esi
  ; Is it a Comment?
L0:     If B$esi = ';'
            ; OK
        Else_If B$esi = SPC
            dec esi | jmp L0<
        Else
            Pop esi | Mov eax &FALSE | ret
        End_If

    Pop esi
    Mov eax &TRUE
ret
____________________________________________________________________________________________

TagParser:
  ; Check for the Tag KeyWords:
  ; 'Dialog' or 'Wizard'

  ; esi > 'tag '
    add esi 4 | While B$esi = SPC | inc esi | End_While
    Mov eax D$esi | or eax 020202020
  ; "Tag Dialog DialogID":
    ...If eax = 'dial'
        Mov ax W$esi+4 | or ax 02020
        .If ax = 'og'
            If B$esi+6 = SPC
                Mov D$FL.ReadyToRun &FALSE
                add esi 7 | jmp DialogTag
            End_If
        .End_If
  ; "Tag Wizard WizardName 'Path...........' ":
    ...Else_If eax = 'wiza'
        Mov ax W$esi+4 | or ax 02020
        .If ax = 'rd'
            If B$esi+6 = SPC
                Mov D$FL.ReadyToRun &FALSE
                add esi 6 | jmp WizardTag
            End_If
        .End_If
  ; "Tag Unicode LabelName":
    ...Else_If eax = 'unic'
        Mov eax D$esi+3 | or eax 020202020
        .If eax = 'code'
            If B$esi+7 = SPC
                Mov D$FL.ReadyToRun &FALSE
                add esi 8 | jmp UnicodeTag
            End_If
        .End_If
  ; "Tag Menu MenuID":
    ...Else_If eax = 'menu'
        If B$esi+4 = SPC
            Mov D$FL.ReadyToRun &FALSE
            add esi 5 | jmp MenuTag
        End_If

    ...End_If

    Call 'USER32.MessageBoxA' D$H.MainWindow, {B$ 'Unknown Tag KeyWord' EOS}, {B$ 'No such Tag' EOS}, 0
ret


TagDecimalToBin:
    Mov ecx 0, eax 0

L0: lodsb
    cmp al SPC | jbe L9>
    cmp al '9' | ja L8>
    cmp al '0' | jb L8>
        sub al '0'                  ; convert Decimal to binary:
        lea ecx D$ecx+ecx*4         ;     ecx = ecx * 5
        lea ecx D$eax+ecx*2         ;     ecx = eax + old ecx * 10
    jmp L0<

L8: Call 'USER32.MessageBoxA' D$H.MainWindow, {B$ "Tags' Numbers are to be provided in Decimal notation" EOS},
                              {B$ 'Bad Number' EOS}, 0
    Mov eax &FALSE | ret

L9: Mov eax &TRUE | ret

____________________________________________________________________________________________

; "Tag Dialog" Routines:

; "; Tag Dialog " found // esi on next possible ID

DialogTag:
    While B$esi = SPC | inc esi | End_While

    Call TagDecimaltoBin

    .If eax = &TRUE
        Mov eax ecx

        If eax = 0
            ; nope
        Else_If eax > 0_FFFF
            ; nope
        Else
            Call GetTagedDialog
        End_If
    .End_If
ret

____________________________________________________________________________________________

; Simplified version of the 'LoadFromResources' executed by Menu Selection:

GetTagedDialog:
    If D$FL.DialogEdition = &TRUE
        Call Beep | ret  ; prevents from multi-runs
    End_If

    Push eax
        Call ReleaseDialogMemories | Call InitDialogMemory
    Pop eax

    Mov esi DialogList, D$MenuListPtr esi      ; 1 record:  ID / Ptr / Size

L0: .If D$esi = eax
        Mov D$WhatDialogListPtr esi | add D$WhatDialogListPtr 4  ;DialogList+4

        Mov ebx D$WhatDialogListPtr, ebx D$ebx
        If W$ebx+18 = 0
            Mov D$ActualMenutestID 0, D$DialogMenuTrueID 0
        Else
            movzx eax W$ebx+20 | Mov D$DialogMenuTrueID eax
            Mov esi MenuList
            While D$esi <> eax
                add esi 12
            End_While
            Mov D$MenuListPtr esi
            add esi 4
            Call 'USER32.LoadMenuIndirectA' D$esi | Mov D$ActualMenutestID eax
        End_If

        Mov B$DialogLoadedFromResources &TRUE
        Call FromBinToTextTemplate
        Call ReInitDialogEdition

    .Else_If D$esi = 0
        Call 'USER32.MessageBoxA' D$H.MainWindow, {B$ 'No Dialog found with matching ID Number' EOS},
                                  {B$ 'Bad ID' EOS}, 0
    .Else
        add esi 12 | jmp L0<<
    .End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
[WizardCommandLine: D$ ? # 512]

[NewFormPath: B$ ? # &MAX_PATH]
[ProducedCode_BeginWrite: D$ ?
 ProducedCode_Written: D$ ?
 H.ProducedCode: D$ ?
 ProducedCode: D$ ?
 H.WizardSearch: D$ ?]

[WizardName: B$ 'WZRD010Form.exe' EOS]

[WizardSaving:
 @dwFileAttributes: D$ ?
 @ftCreationTime.dwLowDateTime: D$ ?
 @ftCreationTime.dwHighDateTime: D$ ?
 @ftLastAccessTime.dwLowDateTime: D$ ?
 @ftLastAccessTime.dwHighDateTime: D$ ?
 @ftLastWriteTime.dwLowDateTime: D$ ?
 @ftLastWriteTime.dwHighDateTime: D$ ?
 @nFileSizeHigh: D$ ?
 @nFileSizeLow: D$ ?
 @dwReserved0: D$ ?
 @dwReserved1: D$ ?]
[@cFileName: B$ ? # &MAX_PATH]
[@cAlternate: B$ ? # 14]

; tag example :
; >  ; Tag Wizard Form "C\MyPath\MyFolder\MyFile.wwf" -R

; esi points on the space after "; Tag wizard"
WizardTag:

    If D$H.DebugDialog <> 0
          Push esi | Call CloseDebuggerOrIgnore | Pop esi | On eax = &IDNO ret
    End_If

    ;Push D$CurrentWritingPos
    Mov D$STRUCT.EditData@CurrentWritingPos esi
    Call SetPartialEditionFromPos
    Mov esi D$STRUCT.EditData@CurrentWritingPos

    ; selects the wizard
    Mov eax D$esi+1
    or eax 020202020
    If eax = 'form'

        Call WizardFormTag
    ;  ; Temporary add, because of the new 'ControlZ', that does not work on the Full Source:
    ;    Call ReInitUndo
    Else_If eax = 'test'
        ;jmp WizardFormTest
    End_If

    Call RestoreRealSource

    Call ReInitUndoOnly
ret
____________________________________________________________________________________________
WizardFormTag:

    Call CheckTagEndPos | On eax = &FALSE, ret

    ; retrieves Wizard Path :
    Push esi

        Call GetBUAsmFilesPath

        Mov esi BUAsmFilesPath | Mov edi WizardPath
        While B$esi <> 0 | movsb | End_While
        Mov esi WizardName
        While B$esi <> 0 | movsb | End_While | Mov B$edi 0
    Pop esi

    ; set esi on the character after the first double quote following 'Tag Wizard Form'
    While B$esi <> '"' | inc esi | End_While | inc esi

    ; => command line writing:
    Mov edi WizardCommandLine

    ;    * Wizard FileName
    Mov B$edi '"' | inc edi
    Push esi
        Mov esi WizardPath
        While B$esi <> 0 | movsb | End_While
    Pop esi
    Mov B$edi '"' | inc edi

    ;    * File location
    Mov D$edi '-f "'  | add edi 4
    While B$esi <> '"' | movsb | End_While
    Mov W$edi '" ' | add edi 2

    ;    * RosAsm ID
    Mov W$edi '-R'  | add edi 2 | Mov B$edi 0 | inc edi

    Call LoadWizardAndPasteCode esi

ret
____________________________________________________________________________________________
Proc DeleteLastWizardCode:
    Argument @TagBegin
    Uses edx

    Mov esi D@TagBegin
    While B$esi <> LF | dec esi | End_While | inc esi | Mov edx esi
    While esi <= D$STRUCT.EditData@SourceEnd
        .If W$esi = ((59 shl 8) or LF)
            Mov eax D$esi+3 | or eax 0202020
            If eax = 'tag '
                Mov eax D$esi+6 | or eax 020202000
                On eax = ' end', jmp L1>
                ExitP
            End_If
        .End_If
        inc esi
    End_While
    ; should not be accessed ( checked before)
    ExitP

L1: While B$esi <> CR | add esi (1*ASCII) | End_While | sub esi (1*ASCII) | Mov D$LP.BlockEndText esi

    Mov D$LP.BlockStartText edx,
        D$FL.BlockInside &TRUE,
        D$STRUCT.EditData@CaretRow 1,              ; the caret is set at the beginning of the block
        D$STRUCT.EditData@PhysicalCaretRow 1,
        eax D$STRUCT.EditData@CaretRow,
        ebx D$STRUCT.EditData@CaretLine

    Call SearchTxtPtr

    Mov D$STRUCT.EditData@CurrentWritingPos eax

    Call ControlD

    ;ControlZ

EndP
____________________________________________________________________________________________
Proc CheckTagEndPos:
    Uses esi

    While B$esi <> LF | dec esi | End_While | inc esi | Mov edx esi
    While esi <= D$STRUCT.EditData@SourceEnd
        .If W$esi = ((59 shl 8) or LF)
            Mov eax D$esi+3 | or eax 0202020
            If eax = 'tag '
                Mov eax D$esi+6 | or eax 020202000
                On eax = ' end', jmp L1>
                jmp L2>
            End_If
        .End_If
        inc esi
    End_While

L2: Call 'USER32.MessageBoxA' D$H.MainWindow,
{B$ "Can't find '; Tag End' mark.
Write it back and try again ;o)" EOS},  {B$ "Error" EOS}, &MB_ICONERROR
    Mov eax &FALSE
    ExitP

L1: Mov eax &TRUE

EndP
____________________________________________________________________________________________
Proc AddWizardCode:
    Argument @WizardCodePtr @WizardCodeLen
    Uses D$ClipBoardLen

    Move D$ClipBoardLen D@WizardCodeLen

    Call ReMapSourceMemoryIfNeeded D$ClipBoardLen | On eax = &IDNO, ret

    Call DoStoreBlockPaste

    Mov esi D$STRUCT.EditData@SourceEnd | add esi 400                       ; make room inside our text:
    Mov edi esi | add edi D$ClipBoardLen
    Mov ecx esi | sub ecx D$STRUCT.EditData@CurrentWritingPos | inc ecx
    std | rep movsb | cld | inc esi
                                                            ; write from clipboard:
    Mov edi esi, esi D@WizardCodePtr, ecx D$ClipBoardLen
    pushad | rep movsb | popad
                                                            ; position:
    Mov esi edi, ebx D$STRUCT.EditData@CaretLine
L0: lodsb | inc D$STRUCT.EditData@CaretRow | cmp al CR | jne L1>
        inc ebx | Mov D$STRUCT.EditData@CaretRow 1 | lodsb | dec ecx | jz L0>
L1: loop L0<

L0: cmp ebx D$STRUCT.EditData@LineNumber | jna L6>
        Mov esi D$STRUCT.EditData@UpperLine | Mov ecx ebx | sub ecx D$STRUCT.EditData@CaretLine
L1:     lodsb | cmp al LF | jne L1<
            Mov D$STRUCT.EditData@UpperLine esi | dec ebx | jmp L0<

L6: Mov D$STRUCT.EditData@CaretLine ebx

    Mov eax D$ClipBoardLen
    add D$SourceLen eax | add D$STRUCT.EditData@SourceEnd eax | add D$STRUCT.EditData@CurrentWritingPos eax


EndP
____________________________________________________________________________________________
NewWizardForm:

    .If D$H.DebugDialog <> 0
          Call CloseDebuggerOrIgnore | If eax = &IDNO | Pop esi | ret | End_If
    .End_If

    Call 'USER32.MessageBoxA' D$H.MainWindow {B$ 'Set the caret where you want the code to be pasted, and click OK.' EOS},
                              {B$ 'Wait a minute...' EOS}    &MB_ICONEXCLAMATION+&MB_SYSTEMMODAL

    Call GetBUAsmFilesPath

    Mov esi BUAsmFilesPath | Mov edi WizardPath

    While B$esi <> 0 | movsb | End_While
    Mov esi WizardName
    While B$esi <> 0 | movsb | End_While | Mov B$edi 0


    Call GetDirectory NewFormPath
    Mov esi NewFormPath | add esi eax
    Mov D$esi '\Wiz' | add esi 4 | Mov D$esi 'ardF' | add esi 4 | Mov D$esi 'iles' | add esi 4
    Mov B$esi 0

    Push esi | Call 'KERNEL32.CreateDirectoryA' NewFormPath &NULL | Pop esi

    Mov D$esi '\WZR' | add esi 4 | Mov D$esi 'DFor' | add esi 4 | Mov D$esi 'm*.w' | add esi 4
    Mov W$esi 'wf'   | add esi 2 | Mov B$esi 0

  Push esi
    ; looks for existing wizard files in the '\WizardFiles\' sub-directory of the program's directory
    Call 'KERNEL32.FindFirstFileA' NewFormPath WizardSaving
    Mov D$H.WizardSearch eax
    xor ecx ecx
    If eax <> &INVALID_HANDLE_VALUE
L1:     Push ecx | Call 'KERNEL32.FindNextFileA' D$H.WizardSearch WizardSaving | Pop ecx
        inc ecx
        cmp eax 0 | jnz L1<
        Push ecx | Call 'KERNEL32.FindClose' D$H.WizardSearch | Pop ecx
    End_If
  Pop esi

    ; creates the new file name according to the number of already existing files
    Mov ebx ecx | Mov edi esi | sub edi 2
    std
        Mov ecx, 4
L1:     Mov al bl | and al 0F | On al >= 0A, add al 7
        add al, '0' | stosb | shr ebx, 4 | loop L1
    cld
    Mov D$esi-1 '.wwf' | Mov B$esi+3 0


    ; => command line writing:
    Mov edi WizardCommandLine

    ;    * Wizard FileName
    Mov B$edi '"' | inc edi
    Mov esi WizardPath
    While B$esi <> 0 | movsb | End_While
    Mov B$edi '"' | inc edi

    ;    * File location
    Mov D$edi '-f "'  | add edi 4
    Mov esi NewFormPath
    While B$esi <> 0 | movsb | End_While
    Mov W$edi '" ' | add edi 2

    ;    * create the Wizard File
    Mov W$edi '-c'  | add edi 2 | Mov B$edi SPC | inc edi

    ;    * RosAsm ID
    Mov W$edi '-R'  | add edi 2 | Mov B$edi 0 | inc edi

    Call LoadWizardAndPasteCode &FALSE

    Call ReInitUndoOnly
ret

____________________________________________________________________________________________
Proc LoadWizardAndPasteCode:
    Argument @TagBegin

    Call 'KERNEL32.CreateEventA' &NULL &FALSE &FALSE {B$ 'ProducedCode_BeginWrite' EOS}
    Mov D$ProducedCode_BeginWrite eax

    Call 'KERNEL32.CreateProcessA' WizardPath WizardCommandLine 0 0  0  0 0 0  STARTUPINFO  ProcessInfos
    If eax = 0
        Call 'USER32.MessageBoxA' D$H.MainWindow {B$ "The Wizard cannot be loaded." EOS},
                                        ErrorMessageTitle   &MB_ICONEXCLAMATION+&MB_SYSTEMMODAL
        jmp L9>>
    End_If
    Call 'KERNEL32.WaitForSingleObject' D$ProducedCode_BeginWrite &INFINITE  ; wait while the user uses the wizard
    ; open the shared memory where the code has been written.
    ; the first DWord of this data is the code length.
    Call 'KERNEL32.OpenFileMappingA' &FILE_MAP_ALL_ACCESS 0 {B$ 'RosAsmWizardProducedCode' EOS}
    Mov D$H.ProducedCode eax
    Call 'KERNEL32.MapViewOfFile' D$H.ProducedCode &FILE_MAP_ALL_ACCESS 0 0 0 ; jE!
    Mov D$ProducedCode eax | add D$ProducedCode 4

    If D$eax <> 0
        On D@TagBegin <> &FALSE, Call DeleteLastWizardCode D@TagBegin
        Mov eax D$ProducedCode | Mov edx D$eax-4
        Call AddWizardCode eax edx
    Else
        On D@TagBegin = &FALSE, Call 'KERNEL32.DeleteFileA' NewFormPath
    End_If

    sub D$ProducedCode 4
    Call 'KERNEL32.UnmapViewOfFile' D$ProducedCode
    Call 'KERNEL32.CloseHandle' D$H.ProducedCode
    Call 'KERNEL32.OpenEventA' &EVENT_MODIFY_STATE &FALSE {B$ 'ProducedCode_Written' EOS}
    Mov D$ProducedCode_Written eax
    Call 'KERNEL32.SetEvent' D$ProducedCode_Written
    Call 'KERNEL32.CloseHandle' D$ProducedCode_Written
    Call 'KERNEL32.CloseHandle' D$ProcessInfos+4
    Call 'KERNEL32.CloseHandle' D$ProcessInfos
L9: Call 'KERNEL32.CloseHandle' D$ProducedCode_BeginWrite

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

 ; TAG Unicode @MyUnicodeString
; Clic droit sur TAG pour ouvrir l'éditeur !
[@MyUnicodeString: U$ 06D, 06C, 06B, 06D, 06C, 06B, 06D, 06C, 06B, 0D, 0A,
                      06D, 06D, 0
 @MyUnicodeStringNchars: D$ 0D]

UnicodeTag:

    ; Esi points to 'MyUnicodeString', inside "; Tag Unicode MyUnicodeString":
    Call 'USER32.DialogBoxParamW' D$H.Instance,
                                  600,
                                  &NULL,
                                  UnicodeEditorProc,
                                  esi

ret


[H.UnicodeEditor: D$ ?
 UnicodeDataPointer: D$ ?]

; Tag Dialog 600

Proc UnicodeEditorProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_INITDIALOG
        Move D$H.UnicodeEditor D@hwnd, D$UnicodeDataPointer D@lParam

        Call 'USER32.SetClassLongW' D@hwnd, &GCL_HICON, D$STRUC.WINDOWCLASS@hIcon

        Call LoadUnicodeEditorFont

        On D$H.UnicodeEditorFont <> 0,
            Call 'USER32.SendDlgItemMessageW' D@hwnd, 10, &WM_SETFONT,
                                              D$H.UnicodeEditorFont, &TRUE

        Call SetUnicodeDialogContent | jmp L8>>

    ...Else_If D@msg = &WM_COMMAND
        Mov eax D@wParam | and D@wParam 0FFFF | shr eax 16

        .If D@wParam = &IDCANCEL
            If D$H.UnicodeEditorFont <> 0
                Call 'GDI32.DeleteObject' D$H.UnicodeEditorFont
                Mov D$H.UnicodeEditorFont 0
            End_If

            Call 'USER32.DestroyWindow' D@hwnd

        .Else_If D@wParam = &IDOK
          ; The Call is from inside the Right-Click, with full Source restored. So:
            Call SetPartialEditionFromPos
                Call PasteUnicodeDialogContent
            Call RestoreRealSource

            If D$H.UnicodeEditorFont <> 0
                Call 'GDI32.DeleteObject' D$H.UnicodeEditorFont
                Mov D$H.UnicodeEditorFont 0
            End_If

            Call 'USER32.DestroyWindow' D@hwnd

        .Else_If D@wParam = 3
            Call GetUnicodeEditorFont

            If D$H.UnicodeEditorFont <> 0
                Call 'USER32.SendDlgItemMessageW' D@hwnd, 10, &WM_SETFONT,
                                              D$H.UnicodeEditorFont, &TRUE
                Call UpdateRegistry
            End_If
;;
; Does not work, at all:

        .Else_If D@wParam = 4
            Call 'USER32.GetDlgItem' D@hwnd, 10
            Push eax
                Call 'USER32.GetWindowLongW' eax, &GWL_STYLE
                xor eax &ES_AUTOHSCROLL__&WS_HSCROLL
            Pop ebx
            Call 'USER32.SetWindowLongW' ebx, &GWL_STYLE, eax
;;
        .End_If

    ...Else_If D@msg = &WM_CTLCOLOREDIT
        Call 'USER32.SendMessageA' D@lParam, &EM_SETSEL, 0-1, 0
        Call 'GDI32.SetBkColor' D@wParam D$ARVB.DialogsBackColor
        popad | Mov eax D$H.DialogsBackGroundBrush | ExitP


    ...Else
L8:     popad | Mov eax &FALSE | ExitP

    ...End_If

    popad | Mov eax &TRUE
EndP


[UNICODE_EDITION_CHOOSEFONT: ; EditorCHOOSEFONT
 @lStructSize: D$ len
 @hwndOwner: D$ 0
 @hDC: D$ 0
 @lpLogFont: D$ UNICODE_EDITION_LOGFONT
 @iPointSize: D$ 0
 @Flags: D$ &CF_SCREENFONTS__&CF_INITTOLOGFONTSTRUCT
 @rgbColors: D$ 0
 @lCustData: D$ 0
 @lpfnHook: D$ 0
 @lpTemplateName: D$ 0
 @hInstance: D$ 0
 @lpszStyle: D$ &SCREEN_FONTTYPE
 @nFontType: W$ 0
 @Alignment: W$ 0
 @nSizeMin: D$ 0
 @nSizeMax: D$ 0]

[UNICODE_EDITION_LOGFONT:
 @lfHeight: D$ 0
 @lfWidth: D$ 0
 @lfEscapement: D$ 0
 @lfOrientation: D$ 0
 @lfWeight: D$ 0
 @lfItalic: B$ 0
 @lfUnderline: B$ 0
 @lfStrikeOut: B$ 0
 @lfCharSet: B$ 0
 @lfOutPrecision: B$ 0
 @lfClipPrecision: B$ 0
 @lfQuality: B$ 0
 @lfPitchAndFamily: B$ 0
 @lfFaceName: U$ 0 # &LF_FACESIZE
 UNICODE_EDITION_LOGFONTlen: D$ len]

[H.UnicodeEditorFont: D$ ?]

LoadUnicodeEditorFont:
    If D$H.UnicodeEditorFont = 0
        Call 'GDI32.CreateFontIndirectA' UNICODE_EDITION_LOGFONT
        Mov D$H.UnicodeEditorFont eax
    End_If
ret


GetUnicodeEditorFont:
    Move D$UNICODE_EDITION_CHOOSEFONT@hwndOwner D$H.UnicodeEditor
    Call 'Comdlg32.ChooseFontW' UNICODE_EDITION_CHOOSEFONT

    If eax = &TRUE
        On D$H.UnicodeEditorFont <> 0, Call 'GDI32.DeleteObject' D$H.UnicodeEditorFont
        Call 'GDI32.CreateFontIndirectW' UNICODE_EDITION_LOGFONT
        Mov D$H.UnicodeEditorFont eax
    End_If
ret


[AddedCRLF: D$ ?
 UnicodeDataInsertionPoint: D$ ?
 UnicodeValuesAlignment: D$ ?
 NumberOfUnicodeChars: D$ ?]

PasteUnicodeDialogContent:

    Mov esi D$UnicodeDataPointer

    Call InternalRightClick

    If D$FL.BlockInside = &TRUE

        Mov esi D$LP.BlockStartText

        While B$esi <> '[' | sub esi (1*ASCII) | End_While

        Mov D$LP.BlockStartText esi,
            D$UnicodeDataInsertionPoint esi

        Mov esi D$LP.BlockEndText

        While B$esi <> ']' | add esi (1*ASCII) | End_While

        Mov D$LP.BlockEndText esi

        Call ControlD

        Mov B$AddedCRLF &FALSE

    Else

        Mov B$AddedCRLF &TRUE


        Mov esi D$UnicodeDataPointer

        While B$esi >= SPC | add esi (1*ASCII) | End_While

        Mov D$UnicodeDataInsertionPoint esi

    End_If

    ;    Mov ebx D$UnicodeDataPointer
    ;    While B$ebx > CR | inc ebx | End_While | add ebx 2

    Call 'USER32.SendDlgItemMessageW' D$H.UnicodeEditor,
                                      10,
                                      &WM_GETTEXTLENGTH,
                                      D$H.UnicodeEditorFont,
                                      &FALSE

    add eax 100

    shl eax 1

    Call VirtualAlloc Trash1,
                      eax

    Push eax

        Call 'USER32.SendDlgItemMessageW' D$H.UnicodeEditor,
                                          10,
                                          &WM_GETTEXT,
                                          eax,
                                          D$Trash1

    Pop eax

  ; Max = 7 Char ("0FFFF, ") >>> 8 + Alignements (....) >>>> 32
    shl eax 5

    Call VirtualAlloc Trash2,
                      eax

    Mov edi D$Trash2

    If B$AddedCRLF = &TRUE

        Mov D$edi CRLF2 | add edi (4*ASCII)

    End_If

    Mov B$edi '[', ecx edi

    add edi (1*ASCII)

  ; Write the Data Label to the Tempo Buffer:
    Mov esi D$UnicodeDataPointer

    While B$esi > SPC | movsb | End_While

    Mov D$edi ': U$', B$edi+(4*ASCII) SPC | add edi (5*ASCII)

  ; Alignement count:
    sub ecx edi | neg ecx | Mov D$UnicodeValuesAlignment ecx

    Mov esi D$Trash1,
        ecx 0,
        D$NumberOfUnicodeChars 0

    .While W$esi <> 0

        movzx eax W$esi| add esi (2*ASCII) | Call WriteEax | Mov W$edi ', ' | add edi (2*ASCII) | add ecx (1*ASCII)

        add D$NumberOfUnicodeChars (1*ASCII)

      ; New Line when wished:
        If ecx = (11*ASCII)

            Mov ecx 0

            Mov W$edi CRLF | add edi (2*ASCII)

            Mov ecx D$UnicodeValuesAlignment

            While ecx > 0 | Mov B$edi SPC | sub ecx (1*ASCII) | add edi (1*ASCII) | End_While

        End_If

    .End_While

  ; Remove the possible CRLF and ',', at the end of the buffer:
    While B$edi <= SPC | sub edi (1*ASCII) | End_While | On B$edi = ',' sub edi (1*ASCII)

    add edi (1*ASCII)

  ; Cases of empty Edition:
    If W$edi-(2*ASCII) = 'U$'

        Mov W$edi ' 0' | add edi (2*ASCII)

    Else

        Mov D$edi ', 0]' | add edi (3*ASCII)

    End_If

    Mov W$edi CRLF | add edi (2*ASCII)

    Mov esi D$UnicodeDataPointer

    Mov B$edi SPC | add edi (1*ASCII)

    While B$esi > SPC | movsb | End_While

    Mov D$edi 'Ncha', D$edi+(4*ASCII) 'rs: ' | add edi (8*ASCII)

    Mov D$edi 'D$  ' | add edi (3*ASCII)

    Mov eax D$NumberOfUnicodeChars | Call WriteEax

    Mov B$edi ']' | add edi (1*ASCII)

    Move D$LP.BlockStartText D$Trash2,
         D$LP.BlockEndText edi

    Mov D$FL.BlockInside &TRUE

    Call ControlC

    Mov D$FL.BlockInside &FALSE

    Call SetCaret D$UnicodeDataInsertionPoint

    Call ControlV

    Call VirtualFree Trash2

    Call VirtualFree Trash1

ret


SetUnicodeDialogContent:

    Mov esi D$UnicodeDataPointer | Call InternalRightClick

    .If D$FL.BlockInside = &TRUE

        Mov esi D$LP.BlockEndText

        While B$esi <> '0'

            add esi (1*ASCII) | On B$esi = ']' ret

        End_While

        Push esi
        ; Count the number of Words (at least...):

          Mov ecx 0

          While B$esi <> ']'

            On B$esi = '0' add ecx 1

            add esi (1*ASCII)

          End_While

          add ecx 1 | shl ecx 1
        ; Get a mem:
          Call VirtualAlloc Trash1,
                            ecx

        Pop esi

      ; esi >>> first '0' of the first hexa data.
      ; Translate each Hexa Word to Binary:
        Mov edi D$Trash1
      ; strip the leading '0'
L0:     lodsb

        Mov ebx 0
L1:
        lodsb | cmp al ',' | je L8>

                cmp al ']' | je L8>

                    sub al '0' | cmp al 9 | jbe L2>

                        sub al 7

                        cmp al 0F | ja L9>
L2:
        shl ebx 4 | or bl al | jmp L1<

L8:     Mov W$edi bx | add edi (2*ASCII)

        If al <> ']'

            While B$esi < '0'

                add esi (1*ASCII)| On esi >= D$STRUCT.EditData@SourceEnd jmp L9>

            End_While

            On B$esi = '0', jmp L0<

        End_If

L9:     Mov W$edi 0

        Call 'USER32.SendDlgItemMessageW' D$H.UnicodeEditor,
                                          10,
                                          &WM_SETTEXT,
                                          0,
                                          D$Trash1

        Call VirtualFree Trash1

    .End_If

ret

____________________________________________________________________________________________
____________________________________________________________________________________________

MenuTag: ; 'ExistingMenu'
    While B$esi = SPC | inc esi | End_While

    Call TagDecimalToBin

    If eax = &TRUE
        Mov eax ecx | Call GetTagedMenu
    End_If
ret


[TagedEdition: D$ ?]

GetTagedMenu:
    Mov esi MenuList
  ; (ID / Ptr / Size)
    While D$esi <> 0

        If D$esi = eax

            Mov B$TagedEdition &TRUE

            Mov D$MenuListPtr esi

            Call ReEditExistingMenu | jmp L9>

        Else
            add esi (4*3)

        End_If

    End_While

L9: .If B$TagedEdition = 0-1
      ; Delete the Previous Menu Equates Block, and paste the New Menu Equates Block:
        Lea esi D$DataForClipEquates+3 | Call InternalRightClick

        If D$FL.BlockInside = &TRUE

            Mov esi D$LP.BlockStartText

            While B$esi <> '[' | sub esi (1*ASCII) | End_While

            Mov D$LP.BlockStartText esi

            add esi (1*ASCII)

            While B$esi <> ']' | add esi (1*ASCII) | End_While

            Mov D$LP.BlockEndText esi

            Call ControlD | Call ControlV

        End_If

    .End_If

    Mov B$TagedEdition &FALSE

ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
