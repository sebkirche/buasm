TITLE Click
 _______________________________________________________________________________________
 _______________________________________________________________________________________
;;
 Search feature by right-clicking on text: We search for main Labels, macros or
 equates declarations, data declarations. This is to say that, if we are inside
 square brackets, any fitting first word, or odd word, or ':' ended word is good;
 if outside, only ':' ended words.
;;

[InsideText: ?  RCstart: ?  RCend: ?  DataDeclaration: ?    MacroDeclaration: ?]

[OldEditData: ? #21] [PreviousSearch: ? #32]
[OneWordChars: B$ '0123456789_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.' 0
 OneWordCharsLen: D$ len   Edge: B$ 0   OddWord: 0   BackAnyWhere: &TRUE]

; WordEdge looks if a char is or not inside OneWordChars and tells if word edge or not.
; we preserve the flags because direction flag may be set on or off by caller:

WordEdge:
    pushfd | cld
        push ecx, esi, edi
            Mov ecx D$OneWordCharsLen, edi OneWordChars, B$Edge &TRUE
            repne scasb | jne L9>
                Mov B$Edge &FALSE
L9:     pop edi, esi, ecx
    popfd
ret


[ShowEquateTitle: B$ "WIN EQUATE VALUE:" EOS]

[ShowEquateHexa: B$ ? # 160]

ShowEquate:
     Mov ebx, eax

     Mov edi ShowEquateHexa, esi TrashString
     While B$esi <> 0
        movsb
     End_While
     Mov eax '   =' | stosd | Mov eax '    ', ecx 3 | rep stosd

     push edi, ebx
         std
            Mov ecx, 9
L1:         If ecx = 5
                Mov al '_' | stosb
            End_If
            If ecx = 1
                Mov al '_' | stosb
            End_If
            Mov al bl | and al 0F | add al, '0' |  On al > '9', add al 7
            stosb | shr ebx, 4 | loop L1<
         cld
     pop ebx, edi
     inc edi

     Mov D$edi '_h  ', D$edi+4 '  [' | add edi 7

     Mov eax ebx | Call WriteEaxDecimal

     Mov D$edi ']   ', B$edi+4 0

     Call MessageBox ShowEquateTitle,
                     ShowEquateHexa,
                     &MB_SYSTEMMODAL+&MB_USERICON

ret

;;
 I suppress OdWord testing for Equates pointing because it is much difficult work for
 NOP: Usually Equates are defined BEFORE they are used. A wrong pointing could only
 append if user define some Equate AFTER reusing it in Equates definitions (???!!!).
 Good for him!
;;

[PossibleWinApi: B$ ?   PossibleOpCode: ?   LocalSymbol: ?] [NumberDashLines: ?]

RightClick: ; 'InternSearch'
    Mov B$ShiftBlockInside &FALSE

    push D$BlockInside
        Call LeftButtonSimulation | Call LeftButtonUp
        Mov eax D$CurrentWritingPos
    pop D$BlockInside

    ..If B$BlockInside = &TRUE
       .If eax >= D$BlockStartTextPtr
            If eax <= D$BlockEndTextPtr
               jmp RightClickOnBlock
            End_If
       .End_If
    ..End_If

    Call LeftButtonSimulation | Call LeftButtonUp | Call AskForRedraw

  ; Save all 'EditData's
    Mov esi EditData, edi OldEditData, ecx 21 | rep movsd

    Mov esi D$CurrentWritingPos

  ; Special Selections cases when Clicking exactely on '"' or '[':
    If B$esi = '"'
        Call SelectDoubleQuotedText | ret
    Else_If B$esi = '['
        Call SelectDataBlock | ret
    End_If

  ; Is it a Right-Click on 'blank' area for simply routing back?
  ; Or on a DashLine >>> do nothing.
    Mov al B$esi
    If B$esi = '_'
        On B$esi+1 = '_', ret
        On B$esi-1 = '_', ret
    End_If

    Call WordEdge

    .If B$Edge <> &FALSE
        If B$esi <= ' '
            jmp BlankRightClick
        Else_If B$esi = '&'
          ; '&' not considered in the WordEdge Routine, but valid here for Win32 Equates
        Else
          ; Abort on alien Char that could be found, for example, in Strings:
            ret
        End_If
    .End_If

    Mov B$PossibleWinApi &FALSE

; Reused by 'SearchFromTreeListBox' and the Debugger 'DataView_ShowDeclaration':
InternalRightClick: ; 'InternSearch'
    Mov B$InsideMLC &FALSE, B$InsideComment &FALSE

  ; Go to start of Clicked word:
    std
L0:     lodsb | Call WordEdge | cmp B$Edge &TRUE | jne L0<
    cld
    inc esi

  ; Special case for Numbers:
    ..If B$esi+1 >= '0'
        .If B$esi+1 <= '9'
            Call RightClickedNumber | ret
        .End_If
    ..End_If

  ; Special case of Tag Comment:
    Mov eax D$esi+1 | or eax 020202020
    If eax = 'tag '
        Call IsItTag | On eax = &TRUE, jmp TagParser
    End_If

  ; To differenciate for example a Click on 'mov' (if 'mov' is a Macro) from '[mov | ...':
    push esi
        While B$esi = ' ' | dec esi | End_While
        Mov B$PossibleOpCode &FALSE
        On B$esi = '[', Mov B$PossibleOpCode &TRUE
    pop esi

    inc esi

  ; Special case for OS Equates, plus, take care of Strings Delimiters for Api calls:
    If B$esi-1 = '&'
        push esi
            Call NewGetEquates | Mov edx esi
        pop esi
        On B$EquateFound = &TRUE, Call ShowEquate
        ret
    Else_If B$esi-1 = "'"
        Mov B$PossibleWinApi &TRUE
    Else_If B$esi-1 = "'"
        Mov B$PossibleWinApi &TRUE
    End_If

  ; First Char, Low Case into ah:
    Mov ah B$esi | or ah 32

  ; edx > second char (> edi)
    inc esi | Mov ebx 0, edx esi

  ; Search for end of Clicked word:
L0: lodsb | inc ebx | Call WordEdge | cmp B$Edge &TRUE | jne L0<
    sub ebx 1 | jc L9>>             ; ebx = length-1
        cmp ebx 0 | je L9>>
    cmp ebx 1 | ja L0>              ; Abort if local label
    cmp B$esi-2 '9' | ja L0>
        ret

  ; Now, edi (edx) > start+1 of right clicked word; ebx = lenght-1. Search maching Symbol:
L0: Mov esi D$CodeSource, ecx D$SourceLen, B$InsideBracket &FALSE, B$InsideComment &FALSE
    and D$MacroNamePointer 0 ;jE!
;;
  Clean up, in actual Style to be continued from here, when i will have time.
  Old Routines yet here at 'OldRightClick', 'OldInternalRightClick'.
;;
    jmp L0>

T0: lodsb | jmp L1>                         ; simplified loop for strip texts and comments
T1: loop T0<
    On B$InsideMLC = &TRUE, jmp C0>         ; Because unpaired MLC are allowed
      ret

L0: lodsb | cmp al ah | je L3>>             ; test XORed AH each pass in order to handle
            xor ah 32 | cmp al ah | je L3>> ; case without modifying AL (following tests)
        jmp L1>
L2: loop L0<

C0:     .If B$PossibleWinApi = &TRUE
            Call WinApiFirstPass ; SearchWinApi
        .Else
            Mov B$MnemonicHelpFound &FALSE
            Call SearchMneMonic             ; Nothing found > it it a Mnemonic?
            If B$MnemonicHelpFound = &FALSE
                On ebx < 3, Call SearchForReg
            End_If
        .End_If
        ret

L1: cmp B$InsideMLC &TRUE | jne L1>
      cmp D$esi-2 MLC | jne T1<
        Mov B$InsideMLC &FALSE | jmp L2<
L1: cmp B$InsideComment &TRUE | jne L1>
      cmp al LF  | jne T1<<
        Mov B$InsideComment &FALSE | jmp L2<
L1: cmp B$InsideText &FALSE | je L1>
      cmp al B$InsideText | jne T1<<
        Mov B$InsideText &FALSE | jmp L2<<
L1: cmp al "'" | jne L1>
      Mov B$InsideText al | jmp T1<<
L1: cmp al '"' | jne L1>
      Mov B$InsideText al | jmp T1<<
L1: cmp al '[' | jne L1>
        Call ScaningBracket

S0:   cmp B$esi ' ' | jne L2<<
        inc esi | sub ecx 1 | jnc S0<        ; strip double spaces
          ret

L1: cmp al ']' | jne L1>
      Mov B$InsideBracket &FALSE, B$DataDeclaration &FALSE, B$MacroDeclaration &FALSE
      jmp L2<<

L1: cmp al ';' | jne L1>                     ; jmp over comments
        If D$esi-2 = MLC   ; (LF ; ; CR)
            Mov B$InsideMLC &TRUE | jmp T1<<
        Else
            Mov B$InsideComment &TRUE | jmp T1<<
        End_If

L1: cmp al '|' | jne L1>
      Mov B$InsideBracket &FALSE | jmp L2<<
L1: cmp al ':' | jne L2<<
      Mov B$DataDeclaration &TRUE

            jmp L2<<                         ; (avoids pointing equates datas).

L3: Mov al B$esi-2 | Call WordEdge | cmp B$Edge &FALSE | je L2<<     ; left edge?

        Mov D$NumberDashLines 0

        pushad | Mov ecx ebx, edi edx

C0:       lodsb | Mov ah B$edi | inc edi

          ; case insensitive comparison:
            If al >= 'A'
                On al <= 'Z', or al 020
            End_If

            If ah >= 'A'
                On ah <= 'Z', or ah 020
            End_If

          ; Edi is pointing to the clicked Word. Esi ---> Parsed Source:

            .If B$edi-1 <> '_'
                While B$esi-1 = '_'
                    lodsb
                    If al >= 'A'
                        On al <= 'Z', or al 020
                    End_If
                    inc D$NumberDashLines | dec ecx | jz X1>
                End_While
            .End_If

            .If B$esi-1 <> '_'
                While B$edi-1 = '_'
                    Mov ah B$edi | inc edi
                    If ah >= 'A'
                        On ah <= 'Z', or ah 020
                    End_If
                    dec D$NumberDashLines | dec ecx | jz X1>
                End_While
            .End_If

            cmp ah al | jne C1>
            loop C0<

X1:         Mov al B$esi | Call WordEdge
            If B$Edge = &FALSE
                popad | jmp L2<<
            End_If

            popad | jmp C2>

C1:     popad | jne L2<<

   ; Mov al B$esi+ebx | Call WordEdge | cmp B$Edge &FALSE | je L2<<    ; right edge?

C2: If B$MacroDeclaration = &TRUE
      ; Was it first word?
        cmp esi D$MacroNamePointer | jne L2<<
    End_If

    push ebx
        add ebx D$NumberDashLines
        cmp B$esi+ebx ':'
    pop ebx
    je L4>                                 ; Label?

; as we have tested for '|' (> InsideBracket = FALSE), "test B$OddWord 1" applies either
; uppon first word of macro def. or odd word of equate def. But data body could still
; be pointed as odd equate dec. So we finally test 'B$DataDeclaration'.

      cmp B$InsideBracket &TRUE | jne L2<<                     ; equ. / macro
          cmp B$DataDeclaration &TRUE | je L2<<  ; avoid pointing data body instead of Equate
              If B$MacroDeclaration = &FALSE
                Call PairedEquate | On B$ValidEquateOrMacro = &FALSE, jmp L2<<
              End_If

L4: dec esi                                                    ; found
    Mov D$BlockStartTextPtr esi, D$RCstart esi                 ; RCstart/End used by
    add esi ebx | Mov D$BlockEndTextPtr esi, D$RCend esi       ; 'BackClick'
    Mov B$BlockInside &TRUE
    inc esi | Mov D$CurrentWritingPos esi

    std | Mov ecx 0
L5:     lodsb | inc ecx
        cmp al LF | jne L5<                    ; search for start of line
        cld | ;dec ecx

    Call StorePosInBackTable

    add esi 2 | Mov D$UpperLine esi                            ; and set all needed
    Call UpOneLine | Call UpOneLine | Call UpOneLine           ; variables for Pos, in

  ; Would be a good thing... but doesn't work. The Block seems cleared by 'TryToMove'
  ; when it can't move any more upward... complicated... later...

  ;  Call UpOneLine | Call UpOneLine | Call UpOneLine
  ;  Call UpOneLine | Call UpOneLine | Call UpOneLine

    Mov D$CaretLine 3, D$CaretRow ecx, D$PhysicalCaretRow ecx  ; case user wish editing
    Call TryToMove
    Mov D$RightScroll 0 | Call AskForRedraw

    If B$PossibleOpCode = &TRUE
        Mov esi D$BlockStartTextPtr, ah B$esi, edx esi, ebx D$BlockEndTextPtr
        or ah 32 | inc edx | sub ebx esi
        Call SearchMneMonic
    End_If

L9: ret


SearchForReg:
    pushad
        dec edx | Mov esi edx, edi MnemonicCopy, ecx ebx | inc ecx
L0:     lodsb | On al > 'Z', and eax (not 020) | stosb | loop L0<
        Mov B$edi 0
        Mov esi MnemonicCopy
        Call IsItaReg
        On eax <> 0, Call Help B_U_AsmName, {'Registers', 0}, ContextHlpMessage
    popad
ret
____________________________________________________________________________________________

[ValidEquateOrMacro: ?]
;;
[LowSigns            31
    TextSign            30
;;

;;
  'PairedEquate' job is to make sure, for the Right-Click feature, that, the pointed
  word is really an Equate Declaration, and not an Evocation.
;;

PairedEquate:
  ; esi points to the Second Char of the pointed word.
    pushad
    push D$CodeSourceA, D$CodeSourceB
        While B$esi >= '0' | inc esi | End_While
        Mov ecx 2

L0:     While B$esi <> '[' | dec esi | inc ecx | End_While

      ; Case of '[' in Text or Comments:
      cmp D$MacroNamePointer 0 | je L1> ; jE!
        If esi > D$MacroNamePointer
            dec esi | inc ecx | jmp L0<
        End_If
L1:   ;and D$MacroNamePointer 0 ; ?? jE!
        Mov edi Trash1, D$StripLen ecx | rep movsb | Mov D$edi CRLF2

        move D$CodeSourceA Trash1, D$CodeSourceB Trash2

      ; CoolParsers
        Call KillMultiLineComments
        Call NewKillVirtualCRLF
        Call KillMeaninglessCommas

      ; HotParsers
        Call StripUnderscore
        Call TranslateAsciiToMyAscii
        Call StripUnneededSpaces
        Call ConvertCommasToSpace

        Mov esi D$CodeSourceA, edx D$StripLen | add edx esi

        While esi < edx
            If B$esi < Separators
                Mov B$esi Space
            End_If
            inc esi
        End_While

        Call StripUnneededSpaces

        Mov esi D$CodeSourceA, edx D$StripLen | add edx esi | dec edx
        Mov D$ValidEquateOrMacro &FALSE

      ; Don't know why, but it seems that when stripping un-needed Spaces,
      ; the last one might be lost. Probably when followed by EOI:
        On B$edx <> Space, Mov B$edx Space

        While esi < edx
            inc esi | On B$esi = Space, xor D$ValidEquateOrMacro &TRUE
            On B$esi = Space, inc eax
        End_While
    pop D$CodeSourceB, D$CodeSourceA
    popad
ret



BlankRightClick:    On B$BackAnyWhere = &TRUE, Call BackClick | ret


[MacroNamePointer: ?]

ScaningBracket:
    Mov B$InsideBracket &TRUE, B$DataDeclaration &FALSE, B$MacroDeclaration &FALSE

  ; Verify that this is not an Alternate Syntax Instruction:
    push eax
        Mov al B$esi-2 | or al 020
        If al = 'd'
L0:         pop eax
            While B$esi > LF | inc esi | End_While
            ret
        Else_If al = 'r'
            jmp L0<
        End_If
    pop eax

    push esi

      ; Go to first word and keep pointer as required for final test (+1):
        While B$esi = ' ' | inc esi | End_While
        Mov D$MacroNamePointer esi | inc D$MacroNamePointer

      ; Skip first word:
        While B$esi > ' '
            inc esi
            If B$esi = '|'
                Mov B$MacroDeclaration &TRUE | jmp L9>
            End_If
        End_While

      ; What last Char in first word:
        If B$esi-1 = ':'
            Mov B$DataDeclaration &TRUE | jmp L9>
        Else_If B$esi-1 = '|'
            Mov B$MacroDeclaration &TRUE | jmp L9>
        End_If

      ; What is next non space Char:
        While B$esi = ' ' | inc esi | End_While | lodsb

        If al = '|'
            Mov B$MacroDeclaration &TRUE
        Else_If al = CR
            Mov B$MacroDeclaration &TRUE
        Else_If al = ';'
            Mov B$MacroDeclaration &TRUE
        End_If
L9: pop esi
ret

____________________________________________________________________________________________

SelectDoubleQuotedText:
    Mov D$BlockStartTextPtr esi | inc D$BlockStartTextPtr

    Mov B$TextGoingOn &FALSE | lodsb | Call IsItFirstText

L1: lodsb | On esi >= D$SourceEnd, ret
            Call IsItFirstText | je L1<

    sub esi 2
    Mov D$BlockEndTextPtr esi, B$BlockInside &TRUE
    Call AskForRedraw
ret


SelectDataBlock:
    inc esi | Mov D$BlockStartTextPtr esi, B$TextGoingOn &FALSE
    .While B$esi <> ']'

        .If B$esi = ';'
            If D$esi-2 = MLC   ; (LF ; ; CR)
                Do
                    inc esi | On esi >= D$SourceEnd, ret
                Loop_Until D$esi-2 = MLC
            Else
                While B$esi <> LF
                    inc esi
                End_While
            End_If
        .End_If

L1:     lodsb | On esi >= D$SourceEnd, ret
                Call IsItFirsttext | je L1<
    .End_While

L9: dec esi
    Mov D$BlockEndTextPtr esi, B$BlockInside &TRUE
    Call AskForRedraw
ret

____________________________________________________________________________

; User selected a Block of text and then RightClick uppon it:

[FloatHandle: 0
 Float_Copy_String: B$ 'Copy', 0
 Float_Delete_String: 'Delete', 0
 Float_Replace_String: 'Replace', 0

 Float_SearchFromTop_String: 'Search from Top', 0
 Float_SearchUp_String: 'Search Up', 0
 Float_SearchDown_String: 'Search Down', 0

 Float_Unfold_String: 'Unfold', 0         Float_BookMark_String: 'BookMark', 0
 Float_UnBookMark_String: 'UnBookMark', 0 Float_Number_String: 'Number forms', 0
 Float_SelReplace_String: 'Replace in Selection', 0
 Float_BadDisLabel_String: 'Bad Disassembly', 0
 Float_Turn_Code_String: 'This should have been Code', 0]


[Float_Copy 5500    Float_SearchFromTop 5501    Float_SearchUp 5502    Float_SearchDown 5503
 Float_Unfold 5504  Float_BookMark 5505         Float_UnBookMark 5506  Float_Number 5507
 Float_ReArange 5508 Float_SelReplace 5509         Float_Delete 5510      Float_Replace 5511
 Float_BadDisLabel 5512]

RightClickOnBlock:
    Call 'USER32.CreatePopupMenu' | Mov D$FloatHandle eax
    Call 'USER32.AppendMenuA' D$FloatHandle &MF_STRING, Float_Copy, Float_Copy_String
    Call 'USER32.AppendMenuA' D$FloatHandle &MF_STRING, Float_Delete, Float_Delete_String
    Call 'USER32.AppendMenuA' D$FloatHandle &MF_STRING, Float_Replace, Float_Replace_String

    Call 'USER32.AppendMenuA' D$FloatHandle &MF_SEPARATOR &NULL &NUll
    Call 'USER32.AppendMenuA' D$FloatHandle &MF_STRING Float_SearchFromTop,
                             Float_SearchFromTop_String
    Call 'USER32.AppendMenuA' D$FloatHandle &MF_STRING Float_SearchUp Float_SearchUp_String
    Call 'USER32.AppendMenuA' D$FloatHandle &MF_STRING Float_SearchDown Float_SearchDown_String

    Call IsItaNumber
    If eax > 0
        Call 'USER32.AppendMenuA' D$FloatHandle, &MF_SEPARATOR, &NULL, &NUll
        Call 'USER32.AppendMenuA' D$FloatHandle, &MF_STRING, Float_Number, Float_Number_String
    End_If

    Call IsItanEqual | On eax = &TRUE, jmp L0>

    Call IsItaMacro
    If eax > 0
L0:     Call 'USER32.AppendMenuA' D$FloatHandle, &MF_SEPARATOR, &NULL, &NUll
        Call 'USER32.AppendMenuA' D$FloatHandle, &MF_STRING, Float_Unfold, Float_Unfold_String
    End_If

    Call IsItaLabel
    If eax = 1
        Call 'USER32.AppendMenuA' D$FloatHandle, &MF_SEPARATOR, &NULL, &NUll
        Call 'USER32.AppendMenuA' D$FloatHandle, &MF_STRING, Float_BookMark,
                                  Float_BookMark_String
    Else_If eax = 2
        Call 'USER32.AppendMenuA' D$FloatHandle, &MF_SEPARATOR, &NULL, &NUll
        Call 'USER32.AppendMenuA' D$FloatHandle, &MF_STRING, Float_UnBookMark,
                                  Float_UnBookMark_String
    End_If

    .If D$FL.IsDebugging = &FALSE
        Mov ecx D$BlockEndTextPtr | sub ecx D$BlockStartTextPtr
        If ecx > 50
            Call 'USER32.AppendMenuA' D$FloatHandle, &MF_SEPARATOR, &NULL, &NUll
            Call 'USER32.AppendMenuA' D$FloatHandle, &MF_STRING, Float_SelReplace,
                                    Float_SelReplace_String
        End_If
    .End_If


    .If B$ThisSourceIsDisassembled = &TRUE
        Call IsItDisassembledLabel
        If eax = &TRUE
            Call 'USER32.AppendMenuA' D$FloatHandle, &MF_SEPARATOR, &NULL, &NUll
            Call 'USER32.AppendMenuA' D$FloatHandle, &MF_STRING, Float_BadDisLabel,
                                      Float_BadDisLabel_String
        end_If
    .End_If


    Call 'USER32.GetWindowRect' D$H.MainWindow RECT
    Mov eax D$RECTleft | add eax 20 | add D$MousePosX eax
    Mov eax D$RECTtop | add D$MousePosY eax

    Call 'KERNEL32.GetCurrentThreadId'
    Call 'USER32.SetWindowsHookExA' &WH_KEYBOARD, FloatMenuProc, &NULL, eax
    Mov D$hHook eax

    Mov eax D$MousePosX | add eax D$BpMarginWidth

    Call 'USER32.TrackPopupMenu' D$FloatHandle,
                                 0, eax, D$MousePosY, 0,
                                 D$EditWindowHandle, &NULL

    Call 'USER32.UnhookWindowsHookEx' D$hHook
ret


IsItDisassembledLabel:
    Mov esi D$BlockEndTextPtr | On B$esi+1 <> ':', jmp L7>

    Mov esi D$BlockStartTextPtr
    If D$esi = 'Code'
        Mov D$DisLabelTypeWas CODEFLAG
        Call GetDisLabelHexaValue
        xor B$GetHexaFromTextError &TRUE | Mov eax D$GetHexaFromTextError

    Else_If D$esi = 'Data'
        Mov D$DisLabelTypeWas DATAFLAG
        Call GetDisLabelHexaValue
        xor B$GetHexaFromTextError &TRUE | Mov eax D$GetHexaFromTextError

    Else
L7:     Mov eax 0

    End_If
ret


GetDisLabelHexaValue:
;;
  The "On B$esi = '_', jmp L1>" are for cases of Label that are appended with a "_Symbol"
  taken from the 'StringsMap'.
;;
    Mov esi D$BlockStartTextPtr, edi CopyOfLabelHexa | add esi 4
    While B$esi <> ':'
        movsb | On B$esi = '_', jmp L1>
    End_While
L1: Mov B$edi 0

    Call GetHexaFromText CopyOfLabelHexa

    If B$GetHexaFromTextError = &TRUE
        Mov D$DisAddressWas 0
    Else
        Mov D$DisAddressWas eax
    End_If

    Mov D$CopyOfNextLabelHexa 0
    Mov eax D$CopyOfLabelHexa
    .While esi < D$SourceEnd
        inc esi
        .If D$esi = eax
            Mov ebx esi
            While B$esi <> ':'
                inc esi | On B$esi <= ' ', jmp L2>
            End_While

            Mov esi ebx, edi CopyOfNextLabelHexa
            While B$esi <> ':'
                movsb | On B$esi = '_', jmp L1>
            End_While
L1:         Mov B$edi 0
            push D$GetHexaFromTextError
                Call GetHexaFromText CopyOfNextLabelHexa
                If B$GetHexaFromTextError = &TRUE
                    Mov D$NextDisAddressWas 0, D$CopyOfNextLabelHexa 0
                Else
                    Mov D$NextDisAddressWas eax
                End_If
            pop D$GetHexaFromTextError
        .End_If
L2: .End_While
ret


Proc FloatMenuProc:
    Arguments @nCode, @wParam, @lParam

        ..If D@nCode = &HC_ACTION ; HC_NOREMOVE
            On D@wParam = &VK_ESCAPE
L1:             Mov B$BlockInside &FALSE
            End_If
        ..End_If

L9:     Mov eax &FALSE ; Forwarding
EndP


CopyFromFloatMenu:
    Call 'USER32.DestroyMenu' D$FloatHandle
    Call ControlC
ret


SetFloatSearch:
    Mov esi D$BlockStartTextPtr, edi SearchString, ecx D$BlockEndTextPtr
    sub ecx esi | inc ecx
    Mov D$LenOfSearchedString ecx
    rep movsb
ret

SearchUpFromFloatMenu:
    Mov B$DownSearch &FALSE
    Call SetFloatSearch | Call SetCaret D$BlockStartTextPtr | Call StringSearch
ret

SearchFromTopFromFloatMenu:
    Mov B$DownSearch &TRUE
    Call SetFloatSearch
    Call FullUp | Mov D$CaretRow 1, D$CaretLine 0 | move D$CurrentWritingPos D$CodeSource
    Call StringSearch
ret

SearchDownFromFloatMenu:
    Mov B$DownSearch &TRUE
    Call SetFloatSearch | Call StringSearch
ret


RightClickedNumber:
    inc esi
    push D$BlockStartTextPtr, D$BlockEndTextPtr
        Mov D$BlockStartTextPtr esi
        push esi
L0:         lodsb | Call WordEdge | cmp B$Edge &TRUE | jne L0<
            sub esi 2 | Mov D$BlockEndTextPtr esi
        pop esi
        Call IsItaNumber
    pop D$BlockEndTextPtr, D$BlockStartTextPtr
    If eax <> 0
        Call ViewClickedNumber
    End_If
ret


[ClickedNumberValue: ?    HexaInBlock: ?    BinaryInBlock: ?]

[NumberCopy: ? #25]

IsItaNumber:
    Mov eax 0, esi D$BlockStartTextPtr, bl B$esi
    Mov B$HexaInBlock &FALSE, B$BinaryInBlock &TRUE

    cmp bl '0' | jb L9>>
        cmp bl '9' | ja L9>>

L0: inc esi
    cmp B$esi '_' | je L1>
    cmp B$esi '0' | jb L2>  ; 010_0000_0000
    cmp B$esi 'F' | ja L2>
    cmp B$esi 'A' | jae L1>
    cmp B$esi '9' | ja L2>
L1: jmp L0<

L2: Mov ecx esi | sub ecx D$BlockStartTextPtr
    If ecx > 50
        Mov eax 0 | jmp L9>>
    End_If
    Mov esi D$BlockStartTextPtr, edi NumberCopy

    While esi <= D$BlockEndTextPtr
L3:     lodsb
        If al >= 'a'
            On al <= 'f', sub al 32
        End_If

        If al = '_'
            jmp L3<
        Else_If al > 'F'
            Mov eax 0 | jmp L9>>
        Else_If al < '0'
            Mov eax 0 | jmp L9>>
        End_If

        On al > '9',  Mov B$HexaInBlock &TRUE
        On al > '1', Mov B$BinaryInBlock &FALSE

        On al <> '_', stosb
    End_While

    Mov D$OldStackPointer esp
    Mov B$edi 0
    Mov esi NumberCopy
    .If W$esi = '00'
        If B$BinaryInBlock = &TRUE
            Call ClickBinary
        Else
            Mov eax 0
        End_If
    .Else_If B$esi = '0'
        Call ClickHexa
    .Else
        If B$HexaInBlock = &FALSE
            Call ClickDecimal
        Else
            Mov eax 0
        End_If
    .End_If

  ; eax = Number if any (or 0):
L9: Mov D$ClickedNumberValue eax
ret
____________________________________________________________________________________________

; Numbers translations Routines without error report (no Menu Option on failure, instead)

ClickBinary:
    lodsw                                               ; clear first '00'
NackedClickBinary:
    Mov ebx 0, edx 0, ecx 0
L0: lodsb | cmp al Closebracket | jbe L9>
    sub al '0' | shld edx ebx 1 | shl ebx 1 | or bl al
    cmp edx ecx | jb L8>
        Mov ecx edx
            cmp al 2 | jb L0<
L8:             Mov ecx D$BinTypePtr | jmp BadNumberFormat
L9: Mov eax ebx
ret


ClickHexa:
    lodsb                                               ; clear first '0'
NackedClickHexa:
    Mov ebx 0,  edx 0, ecx 0
L0: lodsb | cmp al LowSigns | jbe L9>
        sub al '0' | cmp al 9 | jbe L2>
            sub al 7
L2: shld edx ebx 4 | shl ebx 4 | or bl al
    cmp edx ecx | jb L8>
        Mov ecx edx
            cmp al 0F | jbe L0<
L8: Mov ecx HexType | jmp BadClickFormat
L9: Mov eax ebx
ret


ClickDecimal:
    Mov eax 0, ecx 0

L2: Mov cl B$esi | inc esi                        ; (eax used for result > no lodsb)
    cmp cl LowSigns | jbe  L9>

      Mov edx 10 | mul edx | jo L3>               ; loaded part * 10
                                                  ; Overflow >>> Qword
        sub  ecx '0' | jc L7>
        cmp  ecx 9   | ja L7>

          add  eax ecx | jnc  L2<
            jmp  L4>                              ; carry >>> Qword

                                                  ; if greater than 0FFFF_FFFF:
L3: sub ecx '0' | jc L7>
    cmp ecx 9   | ja L7>

      add eax ecx

L4:   adc edx 0
      Mov cl B$esi | inc  esi
      cmp cl LowSigns | jbe L9>

        Mov ebx eax, eax edx, edx 10 | mul edx    ; high part * 10
          jo L6>                                  ; Qword overflow
            xchg eax ebx | Mov edx 10 | mul edx   ; low part * 10
            add  edx ebx
            jnc   L3<                             ; carry >>> overflow
L6:           Mov eax 0 | ret

L7: Mov ecx D$DezimalTypePtr | jmp BadNumberFormat
L9: ret                                           ; >>> number in EDX:EAX


BadClickFormat:
    dec esi
L0: lodsb | On al = 'X', lodsb

    ..If al = 'H'
        cmp B$esi LowSigns | ja L7>
    ..Else_If al = 'D'
        cmp B$esi LowSigns | ja L7>
    ..Else_If al = 'B'
        cmp B$esi LowSigns | ja L7>
    ..Else
      ; Try to read a Type Marker at the end, and re-run if possible:
L7:     While B$esi > LowSigns | inc esi | End_While | dec esi | lodsb
        .If al = 'H'
            If ecx = HexType
                Mov eax 0 | ret
            End_If
        .Else_If al = 'D'
            If ecx = DezimalType
                Mov eax 0 | ret
            End_If
        .Else_If al = 'B'
            If ecx = BinType
                Mov eax 0 | ret
            End_If
        .Else
            Mov eax 0 | ret
        .End_If
    ..End_If

    dec esi
;;
 esi now points to the last Char of the Number. We overwrite it: We kill the Types Markers
 and we fill at the other end (start), with zeros:
;;
    push edi
        Mov edi esi | dec esi | On B$esi = 'X', dec esi
        std
            While B$esi > LowSigns | movsb | End_While
        cld
        While B$edi > LowSigns | Mov B$edi '0' | dec edi | End_While
    pop edi

    inc esi | While B$esi = '0' | inc esi | End_While

    If al = 'H'
        jmp NackedClickHexa
    Else_If al = 'D'
        jmp ClickDecimal
    Else  ; al = 'B'
        jmp NackedClickBinary
    End_If
____________________________________________________________________________________________



[ClickedNumberText: ClickedHexa: B$ "

                                    

                                                               

                                                        



" EOS]

[ClickedNumberTitle: B$ "BASE FORMS" EOS]


ViewClickedNumber:
    Mov eax D$ClickedNumberValue | lea edi D$ClickedHexa+8

  ; Write Hexa form:
    Call WriteEax
    Mov al ' '
    While B$edi <> CR
        stosb
    End_While
    add edi 8

  ; Write Decimal form:
    Mov eax D$ClickedNumberValue
    Mov dl 0FF | push edx                       ; Push stack end mark
    Mov ecx 10
L0: Mov edx 0
    div ecx | push edx | cmp eax 0 | ja L0<     ; Push remainders
L2: pop eax                                     ; Retrieve Backward
    cmp al 0FF | je L9>                         ; Over?
       add al '0' | stosb | jmp L2<             ; Write
L9:
    Mov al ' '
    While B$edi <> CR
        stosb
    End_While
    add edi 8

  ; Write Binary form:
    Mov D$edi '00_ ' | add edi 3
    Mov ebx D$ClickedNumberValue, ecx 4
L0: shl ebx 1 | Mov al '0' | adc al 0 | stosb | loop L0<
    Mov al '_' | stosb | Mov ecx 4
L0: shl ebx 1 | Mov al '0' | adc al 0 | stosb | loop L0<
    Mov al '_' | stosb | Mov ecx 4
L0: shl ebx 1 | Mov al '0' | adc al 0 | stosb | loop L0<
    Mov al '_' | stosb | Mov ecx 4
L0: shl ebx 1 | Mov al '0' | adc al 0 | stosb | loop L0<
    Mov al '_' | stosb | Mov ecx 4

    Mov al '_' | stosb | stosb

L0: shl ebx 1 | Mov al '0' | adc al 0 | stosb | loop L0<
    Mov al '_' | stosb | Mov ecx 4
L0: shl ebx 1 | Mov al '0' | adc al 0 | stosb | loop L0<
    Mov al '_' | stosb | Mov ecx 4
L0: shl ebx 1 | Mov al '0' | adc al 0 | stosb | loop L0<
    Mov al '_' | stosb | Mov ecx 4
L0: shl ebx 1 | Mov al '0' | adc al 0 | stosb | loop L0<

    Call MessageBox ClickedNumberTitle,
                    ClickedNumberText,
                    &MB_SYSTEMMODAL+&MB_USERICON

ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; Little feature for "Replace all" in Selected Block: Whole words / Case insensitive

; Tag Dialog 1050

BlockReplaceAll:
    Call 'USER32.DialogBoxParamA' D$H.Instance, 1050, D$H.MainWindow, BlockReplaceAllProc, &NULL
ret


[BlockFrCase: &TRUE    BlockWwSearch: &TRUE]

; Tag Dialog 1050

Proc BlockReplaceAllProc:
    Arguments @hwnd, @msg, @wParam, @lParam
    Local @StartOfBlock, @EndOfBlock

    pushad

    ...If D@msg = &WM_INITDIALOG
       ; Call 'USER32.SendDlgItemMessageA' D@hwnd, 50, &BM_SETCHECK, D$BlockFrCase, 0
       ; Call 'USER32.SendDlgItemMessageA' D@hwnd, 51, &BM_SETCHECK, D$BlockWwSearch, 0
        Call 'USER32.GetDlgItem' D@hwnd, 10
        Call 'USER32.SetFocus' eax
        popad | Mov eax &TRUE | ExitP

    ...Else_If D@msg = &WM_COMMAND

        Mov eax D@wParam | and D@wParam 0FFFF | shr eax 16

        ..If D@wParam = &IDCANCEL
            Call WM_CLOSE

        ..Else_If D@wParam = &IDOK
            Call 'USER32.SendDlgItemMessageA' D@hwnd, 50, &BM_GETCHECK, 0, 0
            Mov D$BlockFrCase eax

            Call 'USER32.SendDlgItemMessageA' D@hwnd, 51, &BM_GETCHECK, 0, 0
            Mov D$BlockWwSearch eax

            Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &WM_GETTEXT, 80,
                                              SearchString
            Call 'USER32.SendDlgItemMessageA' D@hwnd, 11, &WM_GETTEXT, 80,
                                              ReplaceWithString
            Mov esi SearchString, ecx 0
            While B$esi > 0 | inc esi | inc ecx | End_While
            Mov esi ReplaceWithString, D$LenOfReplaceString 0
            While B$esi > 0 | inc esi | inc D$LenOfReplaceString | End_While

            .If ecx > 0
                push D$UpperLine, D$CaretRow, D$CaretLine
                push D$DownSearch, D$CaseSearch, D$WholeWordSearch
                    Mov B$BlockInside &FALSE

                    Mov D$LenOfSearchedString ecx
                  ; No 'String not found" Message at the end:
                    Mov B$OnReplaceAll &TRUE
                    move D$NextSearchPos D$BlockStartTextPtr
                    move D$CaseSearch D$BlockFrCase, D$WholeWordSearch D$BlockWwSearch
                    Mov B$DownSearch &TRUE
                    move D@StartOfBlock D$BlockStartTextPtr, D@EndOfBlock D$BlockEndTextPtr

L0:                 Call StringSearch

                    If B$BlockInside = &TRUE
                        Mov eax D@EndOfBlock | cmp D$BlockStartTextPtr eax | ja L1>
                        Call StringReplace | jmp L0<
                    End_If

L1:                 Mov B$OnReplaceAll &FALSE, B$Disassembling &FALSE, B$BlockInside &TRUE
                    move D$BlockStartTextPtr D@StartOfBlock, D$BlockEndTextPtr D@EndOfBlock
                pop D$WholeWordSearch, D$CaseSearch, D$DownSearch
                pop D$CaretLine, D$CaretRow, D$UpperLine
                Call AskForRedraw
            .End_If

            Call WM_CLOSE
        ..End_If

    ...Else
L8:     popad | Mov eax &FALSE | ExitP

    ...End_If

    popad | Mov eax &TRUE
EndP


____________________________________________________________________________________________
____________________________________________________________________________________________


; If user DoubleClick or RightClick on Block, we check if the pointed thing is a Macro
; evocation (to append an 'Unfold' Item in the Floating Menu). Called from
; 'RightClickOnBlock'.

IsItaMacro:
    Mov esi D$BlockStartTextPtr

  ; Verify we are not pointing a Declaration:
    push esi
        While B$esi-1 = ' '
            dec esi
        End_While
        Mov al B$esi-1
    pop esi
    If al = '['
        Mov eax 0 | ret
    End_If

    Mov edi D$CodeSource, al '[', ecx D$SourceLen
    Mov edx D$BlockEndTextPtr | sub edx D$BlockStartTextPtr | inc edx

  ; Search for '[' in user Source:
L0: repne scasb | jne L8>
        push eax, ecx, esi, edi
            While B$edi = ' '
                inc edi                     ; Strip possible leading Space(s).
            End_While
            Mov ecx edx
L1:         Mov al B$esi, bl B$edi | inc esi | inc edi | dec ecx
            or al 020 | or bl 020 | cmp al bl | jne L2>          ; No case compare with Block.
            cmp ecx 0 | ja L1<
               jmp L9>
L2:     pop edi, esi, ecx, eax

L3: cmp ecx 0 | ja L0<

L8: Mov eax 0 | ret                         ; No mach found.

  ; Mach found, but is it a Macro?
L9: While B$edi = ' '
        inc edi                             ; strip trailing Space(s)
    End_While
    cmp B$edi CR | je L9>                   ; Macro.
    cmp B$edi '|' | je L9>                  ; Macro too.
    cmp B$edi ';' | je L9>                  ; Macro too with comment after symbol.
        pop edi, esi, ecx, eax | jmp L3<         ; Equate, Data or Macro with the same begining

L9: pop edi, esi, ecx, eax

    move D$InstructionToUnfold D$BlockStartTextPtr
    dec edi | Mov eax edi, D$UnfoldedMacro eax
ret


; Checks if we are pointing on an Equal PreParser line.


[ASCII_DOLLAR 024, ASCII_PARAGRAPH 0A7]

IsItAnEqual:
    Mov esi D$BlockStartTextPtr, eax 0, B$UnfoldEqual &FALSE
    If B$esi-1 = ASCII_DOLLAR
        sub esi 2
    Else_If B$esi-1 = ASCII_PARAGRAPH
        sub esi 2
    End_If

    Mov D$InstructionToUnfold esi

  ; Go to the start of the Statement:
L0: dec esi
    If B$esi = '|'
        jmp L1>
    Else_If B$esi = LF
        jmp L1>
    Else_If B$esi = ' '
        je L0<
    Else
        ret
    End_If

  ; OK, the Selection is the first Member of a Statement. Go to next Component:
L1: Mov esi D$BlockEndTextPtr
L0: inc esi
    If B$esi = ' '
        jmp L1>
    Else_If B$esi = LF
        ret
    Else_If B$esi = ','
        ret
    End_If
    jmp L0<

L1: While B$esi = ' ' | inc esi | End_While

    .If B$esi = '='
        If B$esi+1 = ' '
            Mov B$UnfoldEqual &TRUE
            move D$UnfoldedMacro D$CodeSource
            dec D$UnfoldedMacro
            Mov eax &TRUE | ret
        End_If
    .End_If
ret
____________________________________________________________________________________________

; Called by User DoubleLefClick. If the Block is a Label, the user can store it as
; BookMarked.

[BookMarks: ?    BookMarkLen: ?    BookMarkPtr: ?]
[ToBeBookMarked: ? #20]

IsItaLabel:
  ; If not a Label, we abort:
    Mov esi D$BlockEndTextPtr
    If B$esi+1 <> ':'
        Mov eax 0 | ret
    End_If

  ; Local Label, we abort:
    Mov esi D$BlockStartTextPtr
    If B$esi+2 = ':'
        Mov eax 0 | ret
    End_If

  ; If It is a Local Symbol, we extend. If it is a Local Label we abort:
    Mov esi D$BlockStartTextPtr, edi ToBeBookMarked

    If B$esi-1 = '@'
        push edi | Call SearchUpperMainLabel | pop edi | On eax = 0, ret
        Mov esi eax
        While B$esi <> ':'
            movsb
        End_While
        Mov B$edi '@' | inc edi
        Mov esi D$BlockStartTextPtr
    End_If

    While esi < D$BlockEndTextPtr | movsb | End_While | movsb | Mov B$edi 0

    sub edi ToBeBookMarked | Mov D$BookMarkLen edi
    Mov esi ToBeBookMarked

  ; If the Block is aready BookMarked, we enable the [UnBookMark] option:
    If D$BookMarks > 0
        Mov edi D$BookMarks, ecx D$BookMarkLen
        inc edi
L0:     push esi, edi, ecx
            Mov D$BookMarkPtr edi
L1:         Mov al B$esi, bl B$edi | or al 020 | or bl 020 | inc esi | inc edi
            cmp al bl | jne L2>
            loop L1<
        pop ecx, eax, esi                       ; Found.
        cmp B$edi ' ' | ja L3>
        Mov eax 2 | ret                         ; Return for [UnBookMark] option.
L2:     pop ecx, eax, esi                       ; Not yet found.
L3:     Mov edi eax
L3:     cmp B$edi 0 | je L4>
            inc edi | jmp L3<
L4:     inc edi | cmp B$edi 0 | jne L0<         ; Not yet end of Stored BookMarks.
    End_If

  ; If here, the Label is a Main label and is not yet BookMarked:
    Mov eax 1
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

SearchUpperMainLabel:
    Mov edi D$BlockStartTextPtr
L0: dec edi
    While B$edi <> ':'
        dec edi
        If B$edi = '"'
            Do
                dec edi | on edi = D$CodeSource, jmp L7>
            Loop_Until B$edi = '"'
            dec edi
        Else_If B$edi = "'"
            Do
                dec edi | on edi = D$CodeSource, jmp L7>
            Loop_Until B$edi = "'"
            dec edi
        End_If
        on edi = D$CodeSource, jmp L7>
    End_While
    While B$edi > ' '
        dec edi | on edi = D$CodeSource, jmp L7>
    End_While
    inc edi
    If B$edi = '@'
        jmp L0<<
    Else_If B$edi+2 = ':'
        jmp L0<<
    Else
        Mov eax edi | ret
    End_If

L7: Mov eax 0
ret


[FullBookMarks: B$ "No more room to store BookMarks" EOS]
[BookMarksTitle: "               ----------- BookMarks -----------" EOS]
[EndBookMarks:   "                 ------------- Tree --------------" EOS]
[NumberOfBookMarks: 0]

StoreBookMark:
    Call CreateTreeViewList
    If D$BookMarks = 0
        VirtualAlloc BookMarks 01000 | Mov D$NumberOfBookMarks 2
        Call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle, 100, &LB_INSERTSTRING, 0,
                                          EndBookMarks
        Call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle, 100, &LB_INSERTSTRING, 0,
                                          BookMarksTitle
    End_If
    Mov edi D$BookMarks, al 0, ecx 01000
L0: repne scasb | cmp B$edi 0 | jne L0<
    push edi
        Mov eax ToBeBookMarked
        While B$eax <> 0 | inc eax | End_While
        sub eax ToBeBookMarked
        If ecx <= eax

            Call MessageBox Argh,
                            FullBookMarks,
                            &MB_SYSTEMMODAL+&MB_USERICON

            pop edi | jmp L9>
        End_If
        Mov ecx eax, esi ToBeBookMarked
        rep movsb
        Mov al ' ' | stosb | Mov al 0 | stosb
    pop edi

    Call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle 100  &LB_INSERTSTRING 1 edi
    inc D$NumberOfBookMarks
L9: ret


ReInsertBookMarks:
    Mov D$NumberOfBookMarks 2
    Call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle 100  &LB_INSERTSTRING 0,
                                     EndBookMarks
    Call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle 100  &LB_INSERTSTRING 0,
                                     BookMarksTitle

    Mov esi D$BookMarks | inc esi
    .While B$esi > 0
        Call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle 100  &LB_INSERTSTRING 1 esi
        inc D$NumberOfBookMarks
        While B$esi <> 0
            inc esi
        End_While
        inc esi
    .End_While
ret


DeleteBookMark:
    Call CreateTreeViewList
    Call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle 100  &LB_FINDSTRING 0-1 D$BookMarkPtr
    Call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle 100  &LB_DELETESTRING eax 0
    Mov edi D$BookMarkPtr, esi edi
    add esi D$BookMarkLen | While B$esi > 0 | inc esi | End_While | inc esi
    Mov ecx D$BookMarks | add ecx 01000 | sub ecx esi | rep movsb

  ; Delete the 2 added titles if no more BookMarks, delete the .BKM File and release Mem:
    dec D$NumberOfBookMarks
    If D$NumberOfBookMarks = 2
        Call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle 100  &LB_DELETESTRING 0 0
        Call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle 100  &LB_DELETESTRING 0 0
        Call DeleteBookMarkFile

        VirtualFree D$BookMarks
    End_If
ret


DeleteBookMarkFile:
    Mov D$NumberOfBookMarks 0
    Mov edi SaveFilter, al 0, ecx 0-1 | repne scasb
    While B$edi <> '.'
        dec edi
    End_While

    push D$edi, edi
        Mov D$edi '.BKM'

        Call 'KERNEL32.FindFirstFileA' SaveFilter FindFile

        .If eax <> &INVALID_HANDLE_VALUE
            Call 'KERNEL32.FindClose' eax
            Call 'KERNEL32.DeleteFileA' SaveFilter
        .End_If
    pop edi, D$edi
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
           Unfolder's jobs.
           
  'ShowUnfoldMacro' ---> 'ShowUnfoldDialog' ---> 'UnfoldMacro' ---> 'AsmMain'
  
  Both 'AsmMain' and (after final RET), 'UnfoldMacro' Call for 'UnfoldOutput'.
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[UnfoldedMacro: ?    InstructionToUnfold: ?    StackBeforeUnfolding: ?    WeAreUnfolding: ?]

[ShowUnfoldDialogHandle: ?]

ShowUnfoldMacro:
    If D$H.DebugDialog <> 0
        Call KillDebugger | On eax = &IDNO, ret
    End_If

    .If D$ShowUnfoldDialogHandle = 0
        Mov B$CompileErrorHappend &FALSE
        Call 'USER32.DialogBoxParamA' D$H.Instance, 23000, &NULL, ShowUnfoldDialog, &NULL

    .Else
        Beep

    .End_If
ret
____________________________________________________________________________________________

[UnfoldTitle: B$ 'Macro Unfolding', 0]

; Tag Dialog 23000

Proc ShowUnfoldDialog:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    .If D@msg = &WM_COMMAND
         If W@wParam = &IDCANCEL
L0:         Mov D$ShowUnfoldDialogHandle 0
            Call WM_CLOSE

         Else_If W@wParam = &IDOK
            jmp L0<

         End_If

    .Else_If D@msg = &WM_SIZE
        Call ResizeEditControl

    .Else_If D@msg = &WM_INITDIALOG
        move D$ShowUnfoldDialogHandle D@hwnd
        Call SetIconDialog

        Call UnfoldMacro

        If B$UnfoldCompleted = &FALSE
            jmp L0<
        Else
            Call 'USER32.SendMessageA' D@hwnd, &WM_SETTEXT, &NULL, UnfoldTitle
            Mov B$FirstCTLCOLOREDIT &TRUE
        End_If

    .Else_If D@msg = &WM_CTLCOLOREDIT
        If B$FirstCTLCOLOREDIT = &TRUE
            Call 'USER32.SendMessageA' D@lParam, &EM_SETSEL, 0, 0
            Mov B$FirstCTLCOLOREDIT &FALSE
        End_If
        Call 'GDI32.SetBkColor' D@wParam, D$RVBA.DialogsBackgnd
        popad | Mov eax D$H.DialogsBackGroundBrush | jmp L9>

    .Else_If B$CompileErrorHappend = &TRUE
        Mov D$ShowUnfoldDialogHandle 0
        Call WM_CLOSE

    .Else
        popad | Mov eax &FALSE | jmp L9>

    .End_If

    popad | Mov eax &TRUE

L9: EndP


Proc ResizeEditControl:
    Structure @RECT 16, @RECT_leftDis 0, @RECT_topDis 4, @RECT_rightDis 8, @RECT_bottomDis 12

        Call 'USER32.GetClientRect' D$ShowUnfoldDialogHandle, D@RECT

        Call 'USER32.GetDlgItem' D$ShowUnfoldDialogHandle, 101

        Mov ebx D@RECT_rightDis | sub ebx D@RECT_leftDis
        Mov ecx D@RECT_bottomDis | sub ecx D@RECT_topDis

        Call 'USER32.MoveWindow' eax, 0, 0, ebx, ecx, &TRUE
EndP
____________________________________________________________________________________________

[UnfoldEqual: ?  UnfoldCompleted: ?]

UnfoldMacro:
    Call 'USER32.SetCursor' D$H.CursorWAIT | Call AskForRedrawNow

    Mov B$WeAreUnfolding &TRUE, B$UnfoldStepIndice '0'
    Mov D$TrashPointer Trash3

    push D$SourceLen, D$SourceEnd

        Mov D$StackBeforeUnfolding esp

        Call AsmMain
      ; 'AsmMain' stops after the Macros jobs, in cases when "B$WeAreUnfolding = &TRUE".

        Call 'USER32.SetCursor' D$ActualCursor

        Call UnfoldOutput

        Mov D$edi CRLF2, B$edi+4 0

      ; Show the result:
        Mov eax D$CodeSourceB
        If D$eax <> 0
            Call 'USER32.SetDlgItemTextA' D$ShowUnfoldDialogHandle, 101, Trash3
            Mov B$UnfoldCompleted &TRUE

        Else
            Mov B$UnfoldCompleted &FALSE

        End_If

L8: pop D$SourceEnd, D$SourceLen

    ;VirtualFree D$LabelList, D$MacroList, D$PlainLabelList,
    ;        D$StatementsTable, D$StatementsTable2, D$CodeSourceB, D$CodeSourceA
    Call ReleaseAsmTables

    Mov B$WeAreUnfolding &FALSE, B$ReadyToRun &FALSE

    Call ReleaseAsmTables
ret

UnfoldingError:
    Mov D$UnfoldErrorMessage eax, B$CompileErrorHappend &TRUE, B$UnfoldCompleted &FALSE

    Call 'USER32.SetCursor' D$ActualCursor

    While esp <> D$StackBeforeUnfolding
        pop ebx
    End_While

    jmp L8<<
____________________________________________________________________________________________

[UnfoldErrorMessage: ?]

GetUnfoldStatement:
    push esi, ebx, ecx
        Mov esi D$CodeSourceA, eax 0
        Mov edx esi | add edx D$SourceLen

        While eax <> ecx
            .If B$esi = EOI
                If B$esi+1 = OpenBracket
                    ;
                Else_If B$esi+1 = OpenVirtual
                    ;
                Else
                    inc eax
                End_If

            .Else_If B$esi = OpenBracket
                inc eax

            .Else_If B$esi = OpenVirtual
                inc eax

            .End_If

            inc esi | On esi > edx, jmp L8>>
        End_While

      ; Translate into normal Ascii form:
        Mov edi D$TrashPointer

        If B$esi-1 = OpenVirtual ; 016
            dec esi | Call TranslateDeclarationToNormalAscii

        Else_If B$esi-1 = OpenBracket ; 014
            dec esi | Call TranslateDeclarationToNormalAscii

        Else
            Call TranslateCodeToNormalAscii

        End_If

L8: pop ecx, ebx, esi
ret
____________________________________________________________________________________________

TranslateDeclarationToNormalAscii:
L0: lodsb

    .If al = Space
        Mov al ' '
    .Else_If al = EOI
        jmp L2>>
    .Else_If al = meEOI
        Mov al CR | stosb | Mov al LF
    .Else_If al = TextSign
        Mov B$edi '"' | inc edi
        While B$esi <> TextSign
            lodsb
            If al = CR
                Mov W$edi CRLF | add edi 2 | add esi 2
            Else
                stosb
            End_If
        End_While
        inc esi | Mov al '"'

    .Else_If al = MemMarker
        Mov al '$'
    .Else_If al = OpenBracket
        Mov al '['
    .Else_If al = CloseVirtual
        Mov al ']' | stosb
        Mov al CR | stosb | Mov al LF | stosb | jmp L2>>
    .Else_If al = CloseBracket
        Mov al ']' | stosb
        Mov al CR | stosb | Mov al LF | stosb | jmp L2>>
    .Else_If al = OpenVirtual
        Mov al '['
    .Else_If al = AddSign
        Mov al '+'
    .Else_If al = SubSign
        Mov al '-'
    .Else_If al = MulSign
        Mov al '*'
    .Else_If al = DivSign
        Mov al '/'
    .Else_If al = numSign
        Mov al '#'
    .Else_If al = colonSign
        Mov al ':' | stosb | Mov al ' ' | stosb
    .End_If

    stosb

    If al = LF
        Mov D$edi '    ' | add edi 4
    End_If

    jmp L0<<

L2: Mov ax CRLF | stosw ;| Mov al 0 | stosb
ret
____________________________________________________________________________________________

TranslateCodeToNormalAscii:
L0: lodsb

    .If al = Space
        Mov al ' '
    .Else_If al = EOI
        jmp L2>>
    .Else_If al = meEOI
        Mov al CR | stosb | Mov al LF
    .Else_If al = TextSign
        Mov B$edi '"' | inc edi
        While B$esi <> TextSign
            lodsb
            If al = CR
                Mov W$edi CRLF | add edi 2 | add esi 2
            Else
                stosb
            End_If
        End_While
        inc esi | Mov al '"'
    .Else_If al = MemMarker
        Mov al '$'
    .Else_If al = OpenBracket
        jmp L2>>
    .Else_If al = CloseVirtual
        Mov al ']'
    .Else_If al = OpenVirtual
        Mov al '['
    .Else_If al = AddSign
        Mov al '+'
    .Else_If al = SubSign
        Mov al '-'
    .Else_If al = MulSign
        Mov al '*'
    .Else_If al = DivSign
        Mov al '/'
    .Else_If al = numSign
        Mov al '#'
    .Else_If al = colonSign
        Mov al ':' | stosb | Mov al CR | stosb | Mov al LF
    .End_If

    stosb

    If al = LF
        Mov D$edi '    ' | add edi 4
    End_If

    jmp L0<<

L2: Mov ax CRLF | stosw ;| Mov al 0 | stosb
ret
____________________________________________________________________________________________

; pos '0' = 31
[UnfoldSteps: "
******************************
*    Macros-Engine Pass "

UnfoldStepIndice: "0    *
******************************

    ", 0]

UnfoldOutput:
    inc B$UnfoldStepIndice

    Mov edi D$TrashPointer
    zCopy UnfoldSteps
    Mov esi D$CodeSourceA
    Mov D$TrashPointer edi

  ; Count how many Statements, in the 'StatementsTable', down to our Line:
    Mov ebx D$BlockStartTextPtr, esi D$StatementsTable, ecx 1

  ; For "some reason", leading Labels must be included in the Statement:
    Mov eax ebx | dec eax | While B$eax = ' ' | dec eax | End_While
    If B$eax = ':'
        While B$eax-1 > ' ' | dec eax | End_While
        Mov ebx eax
    End_If

  ; Unfold upon an Equal Pre-Parser Statement:
    If B$ebx-1 = 024 ;'$' ; CharMessage
        sub ebx 2
    Else_If B$ebx-1 = 0A7 ;'$'
        sub ebx 2
    End_If

  ; Several Statements are possible. Example, in Data and in Code with a Para-Macro:
    While D$esi <> 0
        If D$esi = ebx
            Call GetUnfoldStatement
            Mov D$edi '    ' | add edi 4
            Mov D$TrashPointer edi
        End_If

        add esi 4 | inc ecx
    End_While
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

EncodeBoxError:
    Call ErrorMessageBox 0, D$ErrorMessagePtr
    Mov esp D$OldStackPointer | sub esp 4
ret
____________________________________________________________________________________________

MarginRightClick:
    On D$BreakPointsTables = 0, Call InitBreakPointsTables

    Call MouseTextPos
    push ebx
        Call SearchTxtPtr | Mov D$BreakPointLocation eax
    pop ebx

    Mov D$PhysicalCaretRow eax, D$CaretRow eax, D$StartBlockCol eax, D$EndBlockCol eax,
        D$CaretLine ebx, D$StartBlockLine ebx, D$EndBlockLine ebx

    Mov D$CaretRow 1

    Mov eax D$BreakPointLocation | Call IsEaxInBpOnTable

    Call BpMenu
ret
____________________________________________________________________________________________

DoubleClick:
    Call MouseTextPos

    Mov D$PhysicalCaretRow eax, D$CaretRow eax, D$StartBlockCol eax, D$EndBlockCol eax,
        D$CaretLine ebx, D$StartBlockLine ebx, D$EndBlockLine ebx

    If D$DBPMenuOn = DOUBLE_CLICK_ACTION
        On B$ClickOnMargin = &TRUE, jmp DoubleClickMarginAction
    End_If

    Call SearchTxtPtr

    Mov al B$esi | Call WordEdge | On B$Edge = &TRUE, ret

    push esi
        std
L0:       lodsb | Call WordEdge | cmp B$Edge &TRUE | jne L0<         ; search start
        cld
        add esi 2 | Mov D$BlockStartTextPtr esi
    pop esi

L0: lodsb | Call WordEdge | cmp B$Edge &TRUE | jne L0<               ; search end

    sub esi 2 | Mov D$BlockEndTextPtr esi

    Mov B$BlockInside &TRUE | Call SetCaret esi
    Call AskForRedraw | Call RightClickOnBlock
ret
____________________________________________________________________________________________


_________________________________________________________

; See comment for rotary BackTable at "SetBackTableMemory"

ClearBackTable:
    Mov edi D$BackTable, D$BackTablePtr edi, eax 0, ecx 040 | rep stosd
ret


StorePosInBackTable:
    Mov ebx D$BackTablePtr, eax D$UpperLine
    Mov D$ebx eax | add bl 4 | Mov D$ebx 0
    Mov D$BackTablePtr ebx, B$MovingBack  &FALSE
ret


[MovingBack: ?]

BackClick:
    Mov eax D$CodeSource | On D$SourceEnd = eax, ret

    If B$MovingBack = &FALSE              ; BackTable store old pos, not last new one.
      Call StorePosInBackTable            ; here we add last new one to allow Forward
      sub bl 4                            ; moves completion.
      Mov D$BackTablePtr ebx, B$MovingBack &TRUE
    End_If

    Mov ebx D$BackTablePtr | sub bl 4
    If D$ebx = 0                          ; If Start pointer, lock on it
        Call StartEdition | Call AskForRedraw | ret
    End_If

L1: Mov eax D$ebx
    Mov D$BackTablePtr ebx, D$UpperLine eax
    Mov D$CaretRow 1, D$CaretLine 0, D$CurrentWritingPos eax
    Call TryToMove
    Call ResetUpperline
    Call AskForRedraw
L9: ret


ForwardClick:
    Mov eax D$CodeSource | On D$SourceEnd = eax, ret

    Mov ebx D$BackTablePtr | add bl 4
    Mov eax D$ebx | cmp eax 0 | je L9>
      Mov D$BackTablePtr ebx, D$UpperLine eax
      Mov D$CaretRow 1, D$CaretLine 0, D$CurrentWritingPos eax
      Call TryToMove | Call ResetUpperline | Call AskForRedraw
L9: ret


; If text lenght have change between two right-click moves, D$Upperline may point to
; any char in a line. We ensure start of line in D$Upperline:

ResetUpperline:
    Mov esi D$Upperline
L0: cmp B$esi-1 LF | je L9>
        dec esi | jmp L0<
L9: Mov D$Upperline esi
ret






