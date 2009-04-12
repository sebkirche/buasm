TITLE Tag


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

; General purpose Routines: 'IsItTag', 'TagParser', 'NoSuchTag', 'TagDecimalToBin', 'NewWizardForm', 'WizardFormTag'

; When called from 'RightClick' > 'InternalRightClick', esi+1 point to 'Tag '

IsItTag:
    push esi
  ; Is it a Comment?
L0:     If B$esi = ';'
            ; OK
        Else_If B$esi = ' '
            dec esi | jmp L0<
        Else
            pop esi | Mov eax &FALSE | ret
        End_If

    pop esi
    Mov eax &TRUE
ret
____________________________________________________________________________________________

TagParser:
  ; Check for the Tag KeyWords:
  ; 'Dialog' or 'Wizard'

  ; esi > 'tag '
    add esi 4 | While B$esi = ' ' | inc esi | End_While
    Mov eax D$esi | or eax 020202020
  ; "Tag Dialog DialogID":
    ...If eax = 'dial'
        Mov ax W$esi+4 | or ax 02020
        .If ax = 'og'
            If B$esi+6 = ' '
                Mov B$ReadyToRun &FALSE
                add esi 7 | jmp DialogTag
            End_If
        .End_If
  ; "Tag Wizard WizardName 'Path...........' ":
    ...Else_If eax = 'wiza'
        Mov ax W$esi+4 | or ax 02020
        .If ax = 'rd'
            If B$esi+6 = ' '
                Mov B$ReadyToRun &FALSE
                add esi 6 | jmp WizardTag
            End_If
        .End_If
  ; "Tag Unicode LabelName":
    ...Else_If eax = 'unic'
        Mov eax D$esi+3 | or eax 020202020
        .If eax = 'code'
            If B$esi+7 = ' '
                Mov B$ReadyToRun &FALSE
                add esi 8 | jmp UnicodeTag
            End_If
        .End_If
  ; "Tag Menu MenuID":
    ...Else_If eax = 'menu'
        If B$esi+4 = ' '
            Mov B$ReadyToRun &FALSE
            add esi 5 | jmp MenuTag
        End_If

    ...End_If

    Call MessageBox {B$ "NO SUCH TAG" EOS},
                    {B$ 'Unknown Tag KeyWord' EOS},
                    &MB_SYSTEMMODAL+&MB_USERICON
ret


TagDecimalToBin:
    Mov ecx 0, eax 0

L0: lodsb
    cmp al ' ' | jbe L9>
    cmp al '9' | ja L8>
    cmp al '0' | jb L8>
        sub al '0'                  ; convert Decimal to binary:
        lea ecx D$ecx+ecx*4         ;     ecx = ecx * 5
        lea ecx D$eax+ecx*2         ;     ecx = eax + old ecx * 10
    jmp L0<

L8: Call MessageBox {B$ "BAD NUMBER" EOS},
                    {B$ "Tags' Numbers are to be provided in Decimal notation" EOS},
                    &MB_SYSTEMMODAL+&MB_USERICON

    Mov eax &FALSE | ret

L9: Mov eax &TRUE | ret

____________________________________________________________________________________________

; "Tag Dialog" Routines:

; "; Tag Dialog " found // esi on next possible ID

DialogTag:
    While B$esi = ' ' | inc esi | End_While

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
    If B$OnDialogEdition = &TRUE
        Beep | ret  ; prevents from multi-runs
    End_If

    push eax
        Call ReleaseDialogMemories | Call InitDialogMemory
    pop eax

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
            Call 'User32.LoadMenuIndirectA' D$esi | Mov D$ActualMenutestID eax
        End_If

        Mov B$DialogLoadedFromResources &TRUE
        Call FromBinToTextTemplate
        Call ReInitDialogEdition

    .Else_If D$esi = 0

        Call MessageBox {B$ "BAD ID" EOS},
                        {B$ "No Dialog found with matching ID Number" EOS},
                        &MB_SYSTEMMODAL+&MB_USERICON

    .Else
        add esi 12 | jmp L0<<
    .End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
[WizardCommandLine: ?  #512]
[ApplicationPath:   ? #&MAX_PATH]
[NewFormPath:       ? #&MAX_PATH]
[ProducedCode_BeginWrite: ?    ProducedCode_Written: ?
 ProducedCodeHandle:      ?    ProducedCode:         ?
 WizardSearchHandle:      ?]

[WizardName: 'WZRD010Form.exe', 0]

[WizardSaving:
 @dwFileAttributes: D$ 0
 @ftCreationTime.dwLowDateTime: D$ 0
 @ftCreationTime.dwHighDateTime: D$ 0
 @ftLastAccessTime.dwLowDateTime: D$ 0
 @ftLastAccessTime.dwHighDateTime: D$ 0
 @ftLastWriteTime.dwLowDateTime: D$ 0
 @ftLastWriteTime.dwHighDateTime: D$ 0
 @nFileSizeHigh: D$ 0
 @nFileSizeLow: D$ 0
 @dwReserved0: D$ 0
 @dwReserved1: D$ 0]
[@cFileName: B$ 0 #&MAX_PATH]
[@cAlternate: B$ 0 #14]

; tag example :
; >  ; Tag Wizard Form "C\MyPath\MyFolder\MyFile.wwf" -R

; esi points on the space after "; Tag wizard"
WizardTag:

    If D$H.DebugDialog <> 0
          push esi | Call KillDebugger | pop esi | On eax = &IDNO, ret
    End_If

    ;push D$CurrentWritingPos
    Mov D$CurrentWritingPos esi
    Call SetPartialEditionFromPos
    Mov esi D$CurrentWritingPos

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
WizardFormTest:
    Call CheckTagEndPos | On eax = &FALSE, ret
    Call DeleteLastWizardCode esi
ret
____________________________________________________________________________________________
WizardFormTag:

    Call CheckTagEndPos | On eax = &FALSE, ret

    ; retrieves Wizard Path :
    push esi
        Call GetRosAsmFilesPath
        Mov esi RosAsmFilesPath | Mov edi WizardPath
        While B$esi <> 0 | movsb | End_While
        Mov esi WizardName
        While B$esi <> 0 | movsb | End_While | Mov B$edi 0
    pop esi

    ; set esi on the character after the first double quote following 'Tag Wizard Form'
    While B$esi <> '"' | inc esi | End_While | inc esi

    ; => command line writing:
    Mov edi WizardCommandLine

    ;    * Wizard FileName
    Mov B$edi '"' | inc edi
    push esi
        Mov esi WizardPath
        While B$esi <> 0 | movsb | End_While
    pop esi
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
    While esi <= D$SourceEnd
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

L1: While B$esi <> CR | inc esi | End_While | dec esi | Mov D$BlockEndTextPtr esi
    Mov D$BlockStartTextPtr edx
    Mov B$BlockInside &TRUE
    Mov D$CaretRow 1              ; the caret is set at the beginning of the block
    Mov D$PhysicalCaretRow 1      ;
    Mov eax D$CaretRow, ebx D$CaretLine | Call SearchTxtPtr
    Mov D$CurrentWritingPos eax
    Call ControlD

    ;ControlZ

EndP
____________________________________________________________________________________________
Proc CheckTagEndPos:
    Uses esi

    While B$esi <> LF | dec esi | End_While | inc esi | Mov edx esi
    While esi <= D$SourceEnd
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

L2: Call MessageBox STR.A.ErrorMessageTitle,
                    {B$ "Can't find '; Tag end' mark.
Write it back and try again ;o)" EOS},
                    &MB_SYSTEMMODAL+&MB_ICONERROR

   Mov eax &FALSE
    ExitP

L1: Mov eax &TRUE

EndP
____________________________________________________________________________________________
Proc AddWizardCode:
    Argument @WizardCodePtr @WizardCodeLen
    Uses D$ClipBoardLen

    move D$ClipBoardLen D@WizardCodeLen

    Call ReMapSourceMemoryIfNeeded D$ClipBoardLen | On eax = &IDNO, ret

    Call DoStoreBlockPaste

    Mov esi D$SourceEnd | add esi 400                       ; make room inside our text:
    Mov edi esi | add edi D$ClipBoardLen
    Mov ecx esi | sub ecx D$CurrentWritingPos | inc ecx
    std | rep movsb | cld | inc esi
                                                            ; write from clipboard:
    Mov edi esi, esi D@WizardCodePtr, ecx D$ClipBoardLen
    pushad | rep movsb | popad
                                                            ; position:
    Mov esi edi, ebx D$CaretLine
L0: lodsb | inc D$CaretRow | cmp al CR | jne L1>
        inc ebx | Mov D$CaretRow 1 | lodsb | dec ecx | jz L0>
L1: loop L0<

L0: cmp ebx D$LineNumber | jna L6>
        Mov esi D$UpperLine | Mov ecx ebx | sub ecx D$CaretLine
L1:     lodsb | cmp al LF | jne L1<
            Mov D$UpperLine esi | dec ebx | jmp L0<

L6: Mov D$CaretLine ebx

    Mov eax D$ClipBoardLen
    add D$SourceLen eax | add D$SourceEnd eax | add D$CurrentWritingPos eax


EndP
____________________________________________________________________________________________
NewWizardForm:

    .If D$H.DebugDialog <> 0
          Call KillDebugger | If eax = &IDNO | pop esi | ret | End_If
    .End_If

    Call MessageBox {B$ "WAIT A MINUTE" EOS},
                    {B$ "Set the caret where you want the code to be pasted, and click OK." EOS},
                    &MB_SYSTEMMODAL+&MB_ICONEXCLAMATION

    Call GetRosAsmFilesPath
    Mov esi RosAsmFilesPath | Mov edi WizardPath
    While B$esi <> 0 | movsb | End_While
    Mov esi WizardName
    While B$esi <> 0 | movsb | End_While | Mov B$edi 0


    Call 'Kernel32.GetCurrentDirectoryA' &MAX_PATH NewFormPath
    Mov esi NewFormPath | add esi eax
    Mov D$esi '\Wiz' | add esi 4 | Mov D$esi 'ardF' | add esi 4 | Mov D$esi 'iles' | add esi 4
    Mov B$esi 0

    push esi | Call 'Kernel32.CreateDirectoryA' NewFormPath &NULL | pop esi

    Mov D$esi '\WZR' | add esi 4 | Mov D$esi 'DFor' | add esi 4 | Mov D$esi 'm*.w' | add esi 4
    Mov W$esi 'wf'   | add esi 2 | Mov B$esi 0

  push esi
    ; looks for existing wizard files in the '\WizardFiles\' sub-directory of the program's directory
    Call 'Kernel32.FindFirstFileA' NewFormPath WizardSaving
    Mov D$WizardSearchHandle eax
    xor ecx ecx
    If eax <> &INVALID_HANDLE_VALUE
L1:     push ecx | Call 'Kernel32.FindNextFileA' D$WizardSearchHandle WizardSaving | pop ecx
        inc ecx
        cmp eax 0 | jnz L1<
        push ecx | Call 'Kernel32.FindClose' D$WizardSearchHandle | pop ecx
    End_If
  pop esi

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
    Mov W$edi '-c'  | add edi 2 | Mov B$edi 020 | inc edi

    ;    * RosAsm ID
    Mov W$edi '-R'  | add edi 2 | Mov B$edi 0 | inc edi

    Call LoadWizardAndPasteCode &FALSE

    Call ReInitUndoOnly
ret

____________________________________________________________________________________________
Proc LoadWizardAndPasteCode:
    Argument @TagBegin

    Call 'Kernel32.CreateEventA' &NULL &FALSE &FALSE {'ProducedCode_BeginWrite',0}
    Mov D$ProducedCode_BeginWrite eax

    Call 'Kernel32.CreateProcessA' WizardPath WizardCommandLine 0 0  0  0 0 0  STARTUPINFO  ProcessInfos
    If eax = 0

        Call MessageBox STR.A.ErrorMessageTitle,
                        {B$ "The Wizard cannot be loaded." EOS},
                        &MB_SYSTEMMODAL+&MB_ICONEXCLAMATION

        jmp L9>>
    End_If
    Call 'Kernel32.WaitForSingleObject' D$ProducedCode_BeginWrite &INFINITE  ; wait while the user uses the wizard
    ; open the shared memory where the code has been written.
    ; the first DWord of this data is the code length.
    Call 'Kernel32.OpenFileMappingA' &FILE_MAP_ALL_ACCESS 0 {'RosAsmWizardProducedCode',0}
    Mov D$ProducedCodeHandle eax
    Call 'Kernel32.MapViewOfFile' D$ProducedCodeHandle &FILE_MAP_ALL_ACCESS 0 0 0 ; jE!
    Mov D$ProducedCode eax | add D$ProducedCode 4

    If D$eax <> 0
        On D@TagBegin <> &FALSE, Call DeleteLastWizardCode D@TagBegin
        Mov eax D$ProducedCode | Mov edx D$eax-4
        Call AddWizardCode eax edx
    Else
        On D@TagBegin = &FALSE, Call 'Kernel32.DeleteFileA' NewFormPath
    End_If

    sub D$ProducedCode 4
    Call 'Kernel32.UnmapViewOfFile' D$ProducedCode
    Call 'Kernel32.CloseHandle' D$ProducedCodeHandle
    Call 'Kernel32.OpenEventA' &EVENT_MODIFY_STATE &FALSE {'ProducedCode_Written',0}
    Mov D$ProducedCode_Written eax
    Call 'Kernel32.SetEvent' D$ProducedCode_Written
    Call 'Kernel32.CloseHandle' D$ProducedCode_Written
    Call 'Kernel32.CloseHandle' D$ProcessInfos+4
    Call 'Kernel32.CloseHandle' D$ProcessInfos
L9: Call 'Kernel32.CloseHandle' D$ProducedCode_BeginWrite

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

 ; Tag Unicode MyUnicodeString

[MyUnicodeString: W$ 06D, 06C, 06B, 06D, 06C, 06B, 06D, 06C, 06B, 0D, 0A,
                     06D, 06D, 06C, 06B, 06D, 06C, 06B, 0D, 0A, 06C, 06B,
                     06A, 06C, 06A]

UnicodeTag:
  ; esi points to 'MyUnicodeString', inside "; Tag Unicode MyUnicodeString":
    Call 'USER32.DialogBoxParamW' D$H.Instance, 600, &NULL, UnicodeEditorProc, esi
ret


[UnicodeEditorHandle: ?, UnicodeDataPointer: ?]

; Tag Dialog 600

Proc UnicodeEditorProc:
    Arguments @hwnd, @msg, @wParam, @lParam

      ...If D@msg = &WM_INITDIALOG
        move D$UnicodeEditorHandle D@hwnd, D$UnicodeDataPointer D@lParam

        Call 'USER32.SetClassLongW' D@hwnd, &GCL_HICON, D$H.MainIcon

        Call LoadUnicodeEditorFont

        On D$UnicodeEditorFontHandle <> 0,
            Call 'USER32.SendDlgItemMessageW' D@hwnd, 10, &WM_SETFONT,
                                              D$UnicodeEditorFontHandle, &TRUE

        Call SetUnicodeDialogContent | jmp L8>>

    ...Else_If D@msg = &WM_COMMAND
        Mov eax D@wParam | and D@wParam 0FFFF | shr eax 16

        .If D@wParam = &IDCANCEL
            If D$UnicodeEditorFontHandle <> 0
                Call 'GDI32.DeleteObject' D$UnicodeEditorFontHandle
                Mov D$UnicodeEditorFontHandle 0
            End_If

            Call 'User32.DestroyWindow' D@hwnd

        .Else_If D@wParam = &IDOK
          ; The Call is from inside the Right-Click, with full Source restored. So:
            Call SetPartialEditionFromPos
                Call PasteUnicodeDialogContent
            Call RestoreRealSource

            If D$UnicodeEditorFontHandle <> 0
                Call 'GDI32.DeleteObject' D$UnicodeEditorFontHandle
                Mov D$UnicodeEditorFontHandle 0
            End_If

            Call 'User32.DestroyWindow' D@hwnd

        .Else_If D@wParam = 3
            Call GetUnicodeEditorFont

            If D$UnicodeEditorFontHandle <> 0
                Call 'USER32.SendDlgItemMessageW' D@hwnd, 10, &WM_SETFONT,
                                              D$UnicodeEditorFontHandle, &TRUE
                Call WriteConfigFile
            End_If
;;
; Does not work, at all:

        .Else_If D@wParam = 4
            Call 'USER32.GetDlgItem' D@hwnd, 10
            push eax
                Call 'USER32.GetWindowLongW' eax, &GWL_STYLE
                xor eax &ES_AUTOHSCROLL__&WS_HSCROLL
            pop ebx
            Call 'USER32.SetWindowLongW' ebx, &GWL_STYLE, eax
;;
        .End_If

    ...Else_If D@msg = &WM_CTLCOLOREDIT

        Call WM_CTLCOLOREDIT | Return

    ...Else
L8:     Return &FALSE

    ...End_If

   Mov eax &TRUE
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
 @lfFaceName: U$ 0 #&LF_FACESIZE
 UNICODE_EDITION_LOGFONTlen: len]

[UnicodeEditorFontHandle: ?]

LoadUnicodeEditorFont:
    If D$UnicodeEditorFontHandle = 0
        Call 'GDI32.CreateFontIndirectA' UNICODE_EDITION_LOGFONT
        Mov D$UnicodeEditorFontHandle eax
    End_If
ret


GetUnicodeEditorFont:
    move D$UNICODE_EDITION_CHOOSEFONT@hwndOwner D$UnicodeEditorHandle
    Call 'Comdlg32.ChooseFontW' UNICODE_EDITION_CHOOSEFONT

    If eax = &TRUE
        On D$UnicodeEditorFontHandle <> 0, Call 'GDI32.DeleteObject' D$UnicodeEditorFontHandle
        Call 'GDI32.CreateFontIndirectW' UNICODE_EDITION_LOGFONT
        Mov D$UnicodeEditorFontHandle eax
    End_If
ret


[AddedCRLF: ?, UnicodeDataInsertionPoint: ?, UnicodeValuesAlignment: ?
 NumberOfUnicodeChars: ?]

PasteUnicodeDialogContent:
    Mov esi D$UnicodeDataPointer
    Call InternalRightClick
    If B$BlockInside = &TRUE
        Mov esi D$BlockStartTextPtr
        While B$esi <> '[' | dec esi | End_While
        Mov D$BlockStartTextPtr esi, D$UnicodeDataInsertionPoint esi

        Mov esi D$BlockEndTextPtr
        While B$esi <> ']' | inc esi | End_While
        Mov D$BlockEndTextPtr esi
        Call ControlD

        Mov B$AddedCRLF &FALSE

    Else
        Mov B$AddedCRLF &TRUE

        Mov esi D$UnicodeDataPointer
        While B$esi >= ' ' | inc esi | End_While
        Mov D$UnicodeDataInsertionPoint esi

    End_If

    ;    Mov ebx D$UnicodeDataPointer
    ;    While B$ebx > CR | inc ebx | End_While | add ebx 2

    Call 'USER32.SendDlgItemMessageW' D$UnicodeEditorHandle, 10, &WM_GETTEXTLENGTH,
                                      D$UnicodeEditorFontHandle, &FALSE
    add eax 100
    push eax
        shl eax 1 | VirtualAlloc Trash1 eax
    pop eax

    push eax
        Call 'USER32.SendDlgItemMessageW' D$UnicodeEditorHandle, 10, &WM_GETTEXT,
                                          eax, D$Trash1
    pop eax

  ; Max = 7 Char ("0FFFF, ") >>> 8 + Alignements (....) >>>> 32
    shl eax 5 | VirtualAlloc Trash2 eax

    Mov edi D$Trash2

    If B$AddedCRLF = &TRUE
        Mov D$edi CRLF2 | add edi 4
    End_If

    Mov B$edi '[', ecx edi | inc edi

  ; Write the Data Label to the Tempo Buffer:
    Mov esi D$UnicodeDataPointer
    While B$esi > ' ' | movsb | End_While
    Mov D$edi ': U$', B$edi+4 ' '| add edi 5

  ; Alignement count:
    sub ecx edi | neg ecx | Mov D$UnicodeValuesAlignment ecx

    Mov esi D$Trash1, ecx 0, D$NumberOfUnicodeChars 0
    .While W$esi <> 0
        movzx eax W$esi| add esi 2 | Call WriteEax | Mov W$edi ', ' | add edi 2 | inc ecx
        inc D$NumberOfUnicodeChars
      ; New Line when wished:
        If ecx = 11
            Mov ecx 0
            Mov W$edi CRLF | add edi 2
            Mov ecx D$UnicodeValuesAlignment
            While ecx > 0 | Mov B$edi ' ' | dec ecx | inc edi | End_While
        End_If
    .End_While

  ; Remove the possible CRLF and ',', at the end of the buffer:
    While B$edi <= ' ' | dec edi | End_While | On B$edi = ',', dec edi
    inc edi

  ; Cases of empty Edition:
    If W$edi-2 = 'U$'
        Mov W$edi ' 0' | add edi 2
    Else
        Mov D$edi ', 0]' | add edi 3
    End_If

    Mov W$edi CRLF | add edi 2
    Mov esi D$UnicodeDataPointer
    Mov B$edi ' ' | inc edi
    While B$esi > ' ' | movsb | End_While
    Mov D$edi 'Ncha', D$edi+4 'rs: ' | add edi 8
    Mov D$edi 'D$  ' | add edi 3
    Mov eax D$NumberOfUnicodeChars | Call WriteEax

    Mov B$edi ']' | inc edi

    move D$BlockStartTextPtr D$Trash2, D$BlockEndTextPtr edi
    Mov B$BlockInside &TRUE
    Call ControlC

    Mov B$BlockInside &FALSE
    Call SetCaret D$UnicodeDataInsertionPoint
    Call ControlV

    VirtualFree D$Trash2, D$Trash1
ret


SetUnicodeDialogContent:
    Mov esi D$UnicodeDataPointer | Call InternalRightClick

    .If B$BlockInside = &TRUE
        Mov esi D$BlockEndTextPtr
        While B$esi <> '0'
            inc esi | On B$esi = ']', ret
        End_While
        push esi
        ; Count the number of Words (at least...):
          Mov ecx 0
          While B$esi <> ']'
            On B$esi = '0', inc ecx
            inc esi
          End_While
          inc ecx | shl ecx 1
        ; Get a mem:
          VirtualAlloc Trash1, ecx
        pop esi

      ; esi >>> first '0' of the first hexa data.
      ; Translate each Hexa Word to Binary:
        Mov edi D$Trash1
      ; strip the leading '0'
L0:     lodsb
        Mov ebx 0
L1:     lodsb | cmp al ',' | je L8>
                cmp al ']' | je L8>
                    sub al '0' | cmp al 9 | jbe L2>
                        sub al 7
                        cmp al 0F | ja L9>
L2:     shl ebx 4 | or bl al | jmp L1<

L8:     Mov W$edi bx | add edi 2

        If al <> ']'
            While B$esi < '0'
                inc esi | On esi >= D$SourceEnd, jmp L9>
            End_While
            On B$esi = '0', jmp L0<
        End_If

L9:     Mov W$edi 0
        Call 'USER32.SendDlgItemMessageW' D$UnicodeEditorHandle, 10, &WM_SETTEXT, 0, D$Trash1

        VirtualFree D$Trash1
    .End_If
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

MenuTag: ; 'ExistingMenu'
    While B$esi = ' ' | inc esi | End_While

    Call TagDecimalToBin

    If eax = &TRUE
        Mov eax ecx | Call GetTagedMenu
    End_If
ret


[TagedEdition: ?]

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

        If D$BlockInside = &TRUE
            Mov esi D$BlockStartTextPtr
            While B$esi <> '[' | dec esi | End_While
            Mov D$BlockStartTextPtr esi
            inc esi
            While B$esi <> ']' | inc esi | End_While
            Mov D$BlockEndTextPtr esi
            Call ControlD | Call ControlV
        End_If
    .End_If

    Mov B$TagedEdition &FALSE
ret









