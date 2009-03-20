TITLE Click           ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________
;;
 Search feature by right-clicking on text: We search for main Labels, macros or
 equates declarations, data declarations. This is to say that, if we are inside
 square brackets, any fitting first word, or odd word, or ':' ended word is good;
 if outside, only ':' ended words.
;;

[InsideText: D$ ?
 RCstart: D$ ?
 RCend: D$ ?
 DataDeclaration: D$ ?
 MacroDeclaration: D$ ?]

[OldEditData: D$ ? # 21]
[PreviousSearch: D$ ? # 32]
[OneWordChars: B$ '0123456789_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.' EOS
 OneWordCharsLen: D$ len] ; !!! Attention <aligné sur fin chaîne prédédente !!!

[Edge: D$ 0
 OddWord: D$ 0
 BackAnyWhere: D$ &TRUE]

; WordEdge looks if a char is or not inside OneWordChars and tells if word edge or not.
; we preserve the flags because direction flag may be set on or off by caller:

WordEdge:
    pushfd | cld
        Push ecx, esi, edi
            Mov ecx D$OneWordCharsLen, edi OneWordChars, B$Edge &TRUE
            repne scasb | jne L9>
                Mov B$Edge &FALSE
L9:     Pop edi, esi, ecx
    popfd
ret


[ShowEquateTitle: B$ 'Win Equate Value' EOS]

[ShowEquateHexa: D$ ? # 40]

ShowEquate:
     Mov ebx, eax

     Mov edi ShowEquateHexa, esi STR.A.Trash
     While B$esi <> 0
        movsb
     End_While
     Mov eax '   =' | stosd | Mov eax '    ', ecx 3 | rep stosd

     Push edi, ebx
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
     Pop ebx, edi
     inc edi

     Mov D$edi '_h  ', D$edi+4 '  [' | add edi 7

     Mov eax ebx | Call WriteEaxDecimal

     Mov D$edi ']   ', B$edi+4 0

     Call 'USER32.MessageBoxA' D$H.MainWindow, ShowEquateHexa, ShowEquateTitle, &MB_SYSTEMMODAL
ret

;;
 I suppress OdWord testing for Equates pointing because it is much difficult work for
 NOP: Usually Equates are defined BEFORE they are used. A wrong pointing could only
 append if user define some Equate AFTER reusing it in Equates definitions (???!!!).
 Good for him!
;;

[PossibleWinApi: D$ ?
 PossibleOpCode: D$ ?
 LocalSymbol: D$ ?
 NumberDashLines: D$ ?]

RightClick: ; 'InternSearch'
    Mov B$ShiftBlockInside &FALSE

    Push D$FL.BlockInside
        Call LeftButtonSimulation | Call LeftButtonUp
        Mov eax D$STRUCT.EditData@CurrentWritingPos
    Pop D$FL.BlockInside

    ..If D$FL.BlockInside = &TRUE

       .If eax >= D$LP.BlockStartText

            If eax <= D$LP.BlockEndText

               jmp RightClickOnBlock

            End_If

       .End_If

    ..End_If

    Call LeftButtonSimulation | Call LeftButtonUp | Call AskForRedraw

  ; Save all 'EditData's
    Mov esi STRUCT.EditData, edi OldEditData, ecx 21 | rep movsd

    Mov esi D$STRUCT.EditData@CurrentWritingPos

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
        If B$esi <= SPC
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

  ; To differenciate for example a Click on 'Mov' (if 'Mov' is a Macro) from '[Mov | ...':
    Push esi
        While B$esi = SPC | dec esi | End_While
        Mov B$PossibleOpCode &FALSE
        On B$esi = '[', Mov B$PossibleOpCode &TRUE
    Pop esi

    inc esi

  ; Special case for OS Equates, plus, take care of Strings Delimiters for Api calls:
    If B$esi-1 = '&'
        Push esi
            Call NewGetEquates | Mov edx esi
        Pop esi
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

S0:   cmp B$esi SPC | jne L2<<
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
                On al <= 'Z', or al SPC
            End_If

            If ah >= 'A'
                On ah <= 'Z', or ah SPC
            End_If

          ; Edi is pointing to the clicked Word. Esi ---> Parsed Source:

            .If B$edi-1 <> '_'
                While B$esi-1 = '_'
                    lodsb
                    If al >= 'A'
                        On al <= 'Z', or al SPC
                    End_If
                    inc D$NumberDashLines | dec ecx | jz X1>
                End_While
            .End_If

            .If B$esi-1 <> '_'
                While B$edi-1 = '_'
                    Mov ah B$edi | inc edi
                    If ah >= 'A'
                        On ah <= 'Z', or ah SPC
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

    Push ebx
        add ebx D$NumberDashLines
        cmp B$esi+ebx ':'
    Pop ebx
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

    Mov D$LP.BlockStartText esi,
        D$RCstart esi                 ; RCstart/End used by

    add esi ebx

    Mov D$LP.BlockEndText esi,
        D$RCend esi       ; 'BackClick'

    Mov D$FL.BlockInside &TRUE
    inc esi | Mov D$STRUCT.EditData@CurrentWritingPos esi

    std | Mov ecx 0
L5:     lodsb | inc ecx
        cmp al LF | jne L5<                    ; search for start of line
        cld | ;dec ecx

    Call StorePosInBackTable

    add esi 2 | Mov D$STRUCT.EditData@UpperLine esi                            ; and set all needed
    Call UpOneLine | Call UpOneLine | Call UpOneLine           ; variables for Pos, in

  ; Would be a good thing... but doesn't work. The Block seems cleared by 'TryToMove'
  ; when it can't Move any more upward... complicated... later...

  ;  Call UpOneLine | Call UpOneLine | Call UpOneLine
  ;  Call UpOneLine | Call UpOneLine | Call UpOneLine

    Mov D$STRUCT.EditData@CaretLine 3, D$STRUCT.EditData@CaretRow ecx, D$STRUCT.EditData@PhysicalCaretRow ecx  ; case user wish editing
    Call TryToMove
    Mov D$STRUCT.EditData@RightScroll 0 | Call AskForRedraw

    If B$PossibleOpCode = &TRUE

        Mov esi D$LP.BlockStartText,
            ah B$esi,
            edx esi,
            ebx D$LP.BlockEndText

        or ah SPC | add edx (1*ASCII) | sub ebx esi

        Call SearchMneMonic

    End_If

L9: ret


SearchForReg:
    pushad
        dec edx | Mov esi edx, edi MnemonicCopy, ecx ebx | inc ecx
L0:     lodsb | On al > 'Z', and eax (not SPC) | stosb | loop L0<
        Mov B$edi 0
        Mov esi MnemonicCopy
        Call IsItaReg
        On eax <> 0, Call Help B_U_AsmName, {B$ 'Registers' EOS}, ContextHlpMessage
    popad
ret
____________________________________________________________________________________________

[ValidEquateOrMacro: D$ ?]
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
    Push D$CodeSourceA, D$CodeSourceB
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

        Move D$CodeSourceA Trash1, D$CodeSourceB Trash2

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
    Pop D$CodeSourceB, D$CodeSourceA
    popad
ret



BlankRightClick:    On B$BackAnyWhere = &TRUE, Call BackClick | ret


[MacroNamePointer: D$ ?]

ScaningBracket:
    Mov B$InsideBracket &TRUE, B$DataDeclaration &FALSE, B$MacroDeclaration &FALSE

  ; Verify that this is not an Alternate Syntax Instruction:
    Push eax
        Mov al B$esi-2 | or al SPC
        If al = 'd'
L0:         Pop eax
            While B$esi > LF | inc esi | End_While
            ret
        Else_If al = 'r'
            jmp L0<
        End_If
    Pop eax

    Push esi

      ; Go to first word and keep pointer as required for final test (+1):
        While B$esi = SPC | inc esi | End_While
        Mov D$MacroNamePointer esi | inc D$MacroNamePointer

      ; Skip first word:
        While B$esi > SPC
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
        While B$esi = SPC | inc esi | End_While | lodsb

        If al = '|'
            Mov B$MacroDeclaration &TRUE
        Else_If al = CR
            Mov B$MacroDeclaration &TRUE
        Else_If al = ';'
            Mov B$MacroDeclaration &TRUE
        End_If
L9: Pop esi
ret

____________________________________________________________________________________________

SelectDoubleQuotedText:
    Mov D$LP.BlockStartText esi | inc D$LP.BlockStartText

    Mov B$TextGoingOn &FALSE | lodsb | Call IsItFirstText

L1: lodsb | On esi >= D$STRUCT.EditData@SourceEnd, ret
            Call IsItFirstText | je L1<

    sub esi (2*ASCII)

    Mov D$LP.BlockEndText esi,
        D$FL.BlockInside &TRUE

    Call AskForRedraw
ret


SelectDataBlock:
    inc esi | Mov D$LP.BlockStartText esi, B$TextGoingOn &FALSE
    .While B$esi <> ']'

        .If B$esi = ';'
            If D$esi-2 = MLC   ; (LF ; ; CR)
                Do
                    inc esi | On esi >= D$STRUCT.EditData@SourceEnd, ret
                Loop_Until D$esi-2 = MLC
            Else
                While B$esi <> LF
                    inc esi
                End_While
            End_If
        .End_If

L1:     lodsb | On esi >= D$STRUCT.EditData@SourceEnd, ret
                Call IsItFirsttext | je L1<
    .End_While

L9: sub esi (1*ASCII)

    Mov D$LP.BlockEndText esi,
        D$FL.BlockInside &TRUE

    Call AskForRedraw
ret

____________________________________________________________________________

; User selected a Block of text and then RightClick uppon it:

[H.MenuFloatBreakPoint: D$ ?]
[Float_Copy_String: B$ 'Copy' EOS]
[Float_Delete_String: B$ 'Delete' EOS]
[Float_Replace_String: B$ 'Replace' EOS]

[Float_SearchFromTop_String: B$ 'Search from Top' EOS]
[Float_SearchUp_String: B$ 'Search Up' EOS]
[Float_SearchDown_String: B$ 'Search Down' EOS]

[Float_Unfold_String: B$ 'Unfold' EOS]
[Float_BookMark_String: B$ 'BookMark' EOS]
[Float_UnBookMark_String: B$ 'UnBookMark' EOS]
[Float_Number_String: B$ 'Number forms' EOS]
[Float_SelReplace_String: B$ 'Replace in Selection' EOS]
[Float_BadDisLabel_String: B$ 'Bad Disassembly' EOS]
[Float_Turn_Code_String: B$ 'This should have been Code' EOS]


[Float_Copy 5500    Float_SearchFromTop 5501    Float_SearchUp 5502    Float_SearchDown 5503
 Float_Unfold 5504  Float_BookMark 5505         Float_UnBookMark 5506  Float_Number 5507
 Float_ReArange 5508 Float_SelReplace 5509         Float_Delete 5510      Float_Replace 5511
 Float_BadDisLabel 5512]

RightClickOnBlock:
    Call 'USER32.CreatePopupMenu' | Mov D$H.MenuFloatBreakPoint eax
    Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint &MF_STRING, Float_Copy, Float_Copy_String
    Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint &MF_STRING, Float_Delete, Float_Delete_String
    Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint &MF_STRING, Float_Replace, Float_Replace_String

    Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint &MF_SEPARATOR &NULL &NUll
    Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint &MF_STRING Float_SearchFromTop,
                             Float_SearchFromTop_String
    Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint &MF_STRING Float_SearchUp Float_SearchUp_String
    Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint &MF_STRING Float_SearchDown Float_SearchDown_String

    Call IsItaNumber
    If eax > 0
        Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint, &MF_SEPARATOR, &NULL, &NUll
        Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint, &MF_STRING, Float_Number, Float_Number_String
    End_If

    Call IsItanEqual | On eax = &TRUE, jmp L0>

    Call IsItaMacro
    If eax > 0
L0:     Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint, &MF_SEPARATOR, &NULL, &NUll
        Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint, &MF_STRING, Float_Unfold, Float_Unfold_String
    End_If

    Call IsItaLabel
    If eax = 1
        Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint, &MF_SEPARATOR, &NULL, &NUll
        Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint, &MF_STRING, Float_BookMark,
                                  Float_BookMark_String
    Else_If eax = 2
        Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint, &MF_SEPARATOR, &NULL, &NUll
        Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint, &MF_STRING, Float_UnBookMark,
                                  Float_UnBookMark_String
    End_If

    .If D$FL.IsDebugging = &FALSE

        Mov ecx D$LP.BlockEndText

        sub ecx D$LP.BlockStartText

        If ecx > 50
            Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint, &MF_SEPARATOR, &NULL, &NUll
            Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint, &MF_STRING, Float_SelReplace,
                                    Float_SelReplace_String
        End_If
    .End_If


    .If B$ThisSourceIsDisassembled = &TRUE
        Call IsItDisassembledLabel
        If eax = &TRUE
            Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint, &MF_SEPARATOR, &NULL, &NUll
            Call 'USER32.AppendMenuA' D$H.MenuFloatBreakPoint, &MF_STRING, Float_BadDisLabel,
                                      Float_BadDisLabel_String
        End_If
    .End_If


    Call 'USER32.GetWindowRect' D$H.MainWindow RECT
    Mov eax D$RECTleft | add eax 20 | add D$MousePosX eax
    Mov eax D$RECTtop | add D$MousePosY eax

    Call 'KERNEL32.GetCurrentThreadId'
    Call 'USER32.SetWindowsHookExA' &WH_KEYBOARD, FloatMenuProc, &NULL, eax
    Mov D$H.Hook eax

    Mov eax D$MousePosX | add eax D$DU.BreakPointWindowMarginWidth

    Call 'USER32.TrackPopupMenu' D$H.MenuFloatBreakPoint,
                                 0, eax, D$MousePosY, 0,
                                 D$H.EditWindow, &NULL

    Call 'USER32.UnhookWindowsHookEx' D$H.Hook
ret


IsItDisassembledLabel:

    Mov esi D$LP.BlockEndText | On B$esi+1 <> ':' jmp L7>

    Mov esi D$LP.BlockStartText

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
    Mov esi D$LP.BlockStartText, edi CopyOfLabelHexa | add esi 4
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
    .While esi < D$STRUCT.EditData@SourceEnd
        inc esi
        .If D$esi = eax
            Mov ebx esi
            While B$esi <> ':'
                inc esi | On B$esi <= SPC, jmp L2>
            End_While

            Mov esi ebx, edi CopyOfNextLabelHexa
            While B$esi <> ':'
                movsb | On B$esi = '_', jmp L1>
            End_While
L1:         Mov B$edi 0
            Push D$GetHexaFromTextError
                Call GetHexaFromText CopyOfNextLabelHexa
                If B$GetHexaFromTextError = &TRUE
                    Mov D$NextDisAddressWas 0, D$CopyOfNextLabelHexa 0
                Else
                    Mov D$NextDisAddressWas eax
                End_If
            Pop D$GetHexaFromTextError
        .End_If
L2: .End_While
ret


Proc FloatMenuProc:
    Arguments @nCode, @wParam, @lParam

        ..If D@nCode = &HC_ACTION ; HC_NOREMOVE
            On D@wParam = &VK_ESCAPE
L1:             Mov D$FL.BlockInside &FALSE
            End_If
        ..End_If

L9:     Mov eax &FALSE ; Forwarding
EndP


CopyFromFloatMenu:
    Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint
    Call ControlC
ret


SetFloatSearch:

    Mov esi D$LP.BlockStartText,
        edi SearchString,
        ecx D$LP.BlockEndText

    sub ecx esi | inc ecx

    Mov D$LenOfSearchedString ecx

    rep movsb

ret

SearchUpFromFloatMenu:
    Mov B$DownSearch &FALSE
    Call SetFloatSearch | Call SetCaret D$LP.BlockStartText | Call StringSearch
ret

SearchFromTopFromFloatMenu:
    Mov B$DownSearch &TRUE
    Call SetFloatSearch
    Call FullUp | Mov D$STRUCT.EditData@CaretRow 1, D$STRUCT.EditData@CaretLine 0 | Move D$STRUCT.EditData@CurrentWritingPos D$CodeSource
    Call StringSearch
ret

SearchDownFromFloatMenu:
    Mov B$DownSearch &TRUE
    Call SetFloatSearch | Call StringSearch
ret


RightClickedNumber:

    add esi (1*ASCII)

    Push D$LP.BlockStartText,
         D$LP.BlockEndText

        Mov D$LP.BlockStartText esi

        Push esi

L0:         lodsb | Call WordEdge | cmp B$Edge &TRUE | jne L0<

            sub esi (2*ASCII) | Mov D$LP.BlockEndText esi

        Pop esi

        Call IsItaNumber

    Pop D$LP.BlockEndText,
        D$LP.BlockStartText

    On eax <> 0 Call ViewClickedNumber

ret


[ClickedNumberValue: D$ ?
 HexaInBlock: D$ ?
 BinaryInBlock: D$ ?]

[NumberCopy: D$ ? # 25]

IsItaNumber:
    Mov eax 0, esi D$LP.BlockStartText, bl B$esi
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

L2: Mov ecx esi | sub ecx D$LP.BlockStartText
    If ecx > 50
        Mov eax 0 | jmp L9>>
    End_If

    Mov esi D$LP.BlockStartText,
        edi NumberCopy

    While esi <= D$LP.BlockEndText

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
    Push edi
        Mov edi esi | dec esi | On B$esi = 'X', dec esi
        std
            While B$esi > LowSigns | movsb | End_While
        cld
        While B$edi > LowSigns | Mov B$edi '0' | dec edi | End_While
    Pop edi

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

[ClickedNumberTitle: B$ 'Bases forms' EOS]


ViewClickedNumber:
    Mov eax D$ClickedNumberValue | lea edi D$ClickedHexa+8

  ; Write Hexa form:
    Call WriteEax
    Mov al SPC
    While B$edi <> CR
        stosb
    End_While
    add edi 8

  ; Write Decimal form:
    Mov eax D$ClickedNumberValue
    Mov dl 0FF | Push edx                       ; Push stack end mark
    Mov ecx 10
L0: Mov edx 0
    div ecx | Push edx | cmp eax 0 | ja L0<     ; Push remainders
L2: Pop eax                                     ; Retrieve Backward
    cmp al 0FF | je L9>                         ; Over?
       add al '0' | stosb | jmp L2<             ; Write
L9:
    Mov al SPC
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

    Call 'USER32.MessageBoxA' D$H.MainWindow, ClickedNumberText, ClickedNumberTitle, &MB_SYSTEMMODAL
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
        popad | Mov eax &TRUE

EndP

    ...Else_If D@msg = &WM_COMMAND

        Mov eax D@wParam | and D@wParam 0FFFF | shr eax 16

        ..If D@wParam = &IDCANCEL
            Call 'USER32.EndDialog' D@hwnd, 0

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

                Push D$STRUCT.EditData@UpperLine,
                     D$STRUCT.EditData@CaretRow,
                     D$STRUCT.EditData@CaretLine,
                     D$DownSearch,
                     D$CaseSearch,
                     D$WholeWordSearch

                    Mov D$FL.BlockInside &FALSE

                    Mov D$LenOfSearchedString ecx
                    ; No 'String not found" Message at the end:
                    Mov B$OnReplaceAll &TRUE
                    Move D$NextSearchPos D$LP.BlockStartText
                    Move D$CaseSearch D$BlockFrCase, D$WholeWordSearch D$BlockWwSearch
                    Mov B$DownSearch &TRUE

                    Move D@StartOfBlock D$LP.BlockStartText,
                         D@EndOfBlock D$LP.BlockEndText

L0:                 Call StringSearch

                    If D$FL.BlockInside = &TRUE
                        Mov eax D@EndOfBlock | cmp D$LP.BlockStartText eax | ja L1>
                        Call StringReplace | jmp L0<
                    End_If

L1:                 Mov B$OnReplaceAll &FALSE,
                        B$Disassembling &FALSE,
                        D$FL.BlockInside &TRUE

                    Move D$LP.BlockStartText D@StartOfBlock,
                         D$LP.BlockEndText D@EndOfBlock

                Pop D$WholeWordSearch,
                    D$CaseSearch,
                    D$DownSearch,
                    D$STRUCT.EditData@CaretLine,
                    D$STRUCT.EditData@CaretRow,
                    D$STRUCT.EditData@UpperLine

                Call AskForRedraw

            .End_If

            Call 'USER32.EndDialog' D@hwnd, 0
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

    Mov esi D$LP.BlockStartText

  ; Verify we are not pointing a Declaration:
    Push esi
        While B$esi-1 = SPC
            dec esi
        End_While
        Mov al B$esi-1
    Pop esi
    If al = '['
        Mov eax 0 | ret
    End_If

    Mov edi D$CodeSource, al '[', ecx D$SourceLen

    Mov edx D$LP.BlockEndText | sub edx D$LP.BlockStartText | add edx (1*ASCII)

  ; Search for '[' in user Source:
L0: repne scasb | jne L8>
        Push eax, ecx, esi, edi
            While B$edi = SPC
                inc edi                     ; Strip possible leading Space(s).
            End_While
            Mov ecx edx
L1:         Mov al B$esi, bl B$edi | inc esi | inc edi | dec ecx
            or al SPC | or bl SPC | cmp al bl | jne L2>          ; No case compare with Block.
            cmp ecx 0 | ja L1<
               jmp L9>
L2:     Pop edi, esi, ecx, eax

L3: cmp ecx 0 | ja L0<

L8: Mov eax 0 | ret                         ; No mach found.

  ; Mach found, but is it a Macro?
L9: While B$edi = SPC
        inc edi                             ; strip trailing Space(s)
    End_While
    cmp B$edi CR | je L9>                   ; Macro.
    cmp B$edi '|' | je L9>                  ; Macro too.
    cmp B$edi ';' | je L9>                  ; Macro too with comment after symbol.
        Pop edi, esi, ecx, eax | jmp L3<         ; Equate, Data or Macro with the same begining

L9: Pop edi, esi, ecx, eax

    dec edi | Mov eax edi
ret


; Checks if we are pointing on an Equal PreParser line.


[ASCII_DOLLAR 024, ASCII_PARAGRAPH 0A7]

IsItAnEqual:
    Mov esi D$LP.BlockStartText eax 0
    If B$esi-1 = ASCII_DOLLAR
        sub esi 2
    Else_If B$esi-1 = ASCII_PARAGRAPH
        sub esi 2
    End_If

    ; Go to the start of the Statement:
L0: dec esi
    If B$esi = '|'
        jmp L1>
    Else_If B$esi = LF
        jmp L1>
    Else_If B$esi = SPC
        je L0<
    Else
        ret
    End_If

    ; OK, the Selection is the first Member of a Statement. Go to next Component:
L1: Mov esi D$LP.BlockEndText

L0: inc esi
    If B$esi = SPC
        jmp L1>
    Else_If B$esi = LF
        ret
    Else_If B$esi = ','
        ret
    End_If
    jmp L0<

L1: While B$esi = SPC | inc esi | End_While

    .If B$esi = '='
        If B$esi+1 = SPC

            Mov eax &TRUE | ret
        End_If
    .End_If
ret
____________________________________________________________________________________________

; Called by User DoubleLefClick. If the Block is a Label, the user can store it as
; BookMarked.

[BookMarks: D$ ?
 BookMarkLen: D$ ?
 BookMarkPtr: D$ ?]

[ToBeBookMarked: D$ ? # 20]

IsItaLabel:
  ; If not a Label, we abort:

    Mov esi D$LP.BlockEndText

    If B$esi+(1*ASCII) <> ':'

        Mov eax 0 | ret

    End_If

  ; Local Label, we abort:
    Mov esi D$LP.BlockStartText
    If B$esi+2 = ':'
        Mov eax 0 | ret
    End_If

  ; If It is a Local Symbol, we extend. If it is a Local Label we abort:
    Mov esi D$LP.BlockStartText edi ToBeBookMarked

    If B$esi-1 = '@'
        Push edi | Call SearchUpperMainLabel | Pop edi | On eax = 0, ret
        Mov esi eax
        While B$esi <> ':'
            movsb
        End_While
        Mov B$edi '@' | inc edi
        Mov esi D$LP.BlockStartText
    End_If

    While esi < D$LP.BlockEndText | movsb | End_While | movsb | Mov B$edi 0

    sub edi ToBeBookMarked | Mov D$BookMarkLen edi
    Mov esi ToBeBookMarked

  ; If the Block is aready BookMarked, we enable the [UnBookMark] option:
    If D$BookMarks > 0
        Mov edi D$BookMarks, ecx D$BookMarkLen
        inc edi
L0:     Push esi, edi, ecx
            Mov D$BookMarkPtr edi
L1:         Mov al B$esi, bl B$edi | or al SPC | or bl SPC | inc esi | inc edi
            cmp al bl | jne L2>
            loop L1<
        Pop ecx, eax, esi                       ; Found.
        cmp B$edi SPC | ja L3>
        Mov eax 2 | ret                         ; Return for [UnBookMark] option.
L2:     Pop ecx, eax, esi                       ; Not yet found.
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

    Mov edi D$LP.BlockStartText

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
    While B$edi > SPC
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


[FullBookMarks: B$ 'No more room to store BookMarks' EOS]
[BookMarksTitle: B$ '               ----------- BookMarks -----------' EOS]
[EndBookMarks: B$ '                 ------------- Tree --------------' EOS]
[NumberOfBookMarks: D$ ?]

StoreBookMark:
    Call CreateTreeViewList
    If D$BookMarks = 0

        Call VirtualAlloc BookMarks,
                          01000

        Mov D$NumberOfBookMarks 2

        Call 'USER32.SendDlgItemMessageA' D$H.ShowTree, 100, &LB_INSERTSTRING, 0,
                                          EndBookMarks

        Call 'USER32.SendDlgItemMessageA' D$H.ShowTree, 100, &LB_INSERTSTRING, 0,
                                          BookMarksTitle
    End_If
    Mov edi D$BookMarks, al 0, ecx 01000
L0: repne scasb | cmp B$edi 0 | jne L0<
    Push edi
        Mov eax ToBeBookMarked
        While B$eax <> 0 | inc eax | End_While
        sub eax ToBeBookMarked
        If ecx <= eax
            Call 'USER32.MessageBoxA' D$H.MainWindow, FullBookMarks, Argh, &MB_SYSTEMMODAL
            Pop edi | jmp L9>
        End_If
        Mov ecx eax, esi ToBeBookMarked
        rep movsb
        Mov al SPC | stosb | Mov al 0 | stosb
    Pop edi

    Call 'USER32.SendDlgItemMessageA' D$H.ShowTree 100  &LB_INSERTSTRING 1 edi
    inc D$NumberOfBookMarks
L9: ret


ReInsertBookMarks:
    Mov D$NumberOfBookMarks 2
    Call 'USER32.SendDlgItemMessageA' D$H.ShowTree 100  &LB_INSERTSTRING 0,
                                     EndBookMarks
    Call 'USER32.SendDlgItemMessageA' D$H.ShowTree 100  &LB_INSERTSTRING 0,
                                     BookMarksTitle

    Mov esi D$BookMarks | inc esi
    .While B$esi > 0
        Call 'USER32.SendDlgItemMessageA' D$H.ShowTree 100  &LB_INSERTSTRING 1 esi
        inc D$NumberOfBookMarks
        While B$esi <> 0
            inc esi
        End_While
        inc esi
    .End_While
ret


DeleteBookMark:
    Call CreateTreeViewList
    Call 'USER32.SendDlgItemMessageA' D$H.ShowTree 100  &LB_FINDSTRING 0-1 D$BookMarkPtr
    Call 'USER32.SendDlgItemMessageA' D$H.ShowTree 100  &LB_DELETESTRING eax 0
    Mov edi D$BookMarkPtr, esi edi
    add esi D$BookMarkLen | While B$esi > 0 | inc esi | End_While | inc esi
    Mov ecx D$BookMarks | add ecx 01000 | sub ecx esi | rep movsb

  ; Delete the 2 added titles if no more BookMarks, delete the .BKM File and release Mem:
    dec D$NumberOfBookMarks
    If D$NumberOfBookMarks = 2
        Call 'USER32.SendDlgItemMessageA' D$H.ShowTree 100  &LB_DELETESTRING 0 0
        Call 'USER32.SendDlgItemMessageA' D$H.ShowTree 100  &LB_DELETESTRING 0 0
        Call DeleteBookMarkFile

        Call VirtualFree BookMarks

    End_If
ret


DeleteBookMarkFile:
    Mov D$NumberOfBookMarks 0
    Mov edi SaveFilter, al 0, ecx 0-1 | repne scasb
    While B$edi <> '.'
        dec edi
    End_While

    Push D$edi, edi
        Mov D$edi '.BKM'

        Call 'KERNEL32.FindFirstFileA' SaveFilter FindFile

        .If eax <> &INVALID_HANDLE_VALUE
            Call 'KERNEL32.FindClose' eax
            Call 'KERNEL32.DeleteFileA' SaveFilter
        .End_If
    Pop edi, D$edi
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

[LP.EspStackBeforeUnfolding: D$ ?
 FL.WeAreUnfolding: D$ ?]

[H.ShowUnfoldDialog: D$ ?]

[DIALOG_SHOW_UNFLODING_MACRO   23000
 EDIT_UNFOLDIND_DIALOG         101]

; Tag Dialog 23000

ShowUnfoldMacro:

   Test D$H.DebugDialog NA ZERO S1>

        Call CloseDebuggerOrIgnore

        Comp eax &IDNO <> S1>

ret

S1: Test D$H.ShowUnfoldDialog NA NOT_ZERO S2>

        Mov D$FL.CompileErrorHappend &FALSE

        Call 'USER32.DialogBoxParamA' D$H.Instance,
                                      DIALOG_SHOW_UNFLODING_MACRO,
                                      &NULL,
                                      ShowUnfoldDialog,
                                      &NULL

ret

S2: Call Beep

ret
____________________________________________________________________________________________

Proc ShowUnfoldDialog:

    Arguments @hwnd,
              @msg,
              @wParam,
              @lParam

    If D@msg = &WM_INITDIALOG

        Move D$H.ShowUnfoldDialog D@hwnd

        Call SetIconDialog

        Call UnfoldMacro

        On D$FL.UnfoldCompleted = &FALSE Call WM_CLOSE_ShowUnfoldDialog

    Else_If D@msg = &WM_CTLCOLOREDIT

        Call WM_CTLCOLOREDIT | ExitP

    Else_If D@msg = &WM_COMMAND

         On W@wParam = HIDE_PUSHBUTTON_OK Call WM_CLOSE_ShowUnfoldDialog

    Else_If D@msg = &WM_CLOSE

        Call WM_CLOSE_ShowUnfoldDialog

    Else

        Mov eax &FALSE | jmp P9> ; TODO ExitP

    End_If

    Mov eax &TRUE

EndP
____________________________________________________________________________________________

WM_CLOSE_ShowUnfoldDialog:

    Call EndDialog

    Mov D$H.ShowUnfoldDialog &NULL

ret
____________________________________________________________________________________________

[FL.UnfoldCompleted: D$ ?]

UnfoldMacro:

    Call 'USER32.SetCursor' D$H.CursorWAIT

    Mov D$FL.WeAreUnfolding &TRUE,
        B$ID.UnfoldStep '0',
        D$LP.Trash Trash3

    Push D$SourceLen,
         D$STRUCT.EditData@SourceEnd

        Mov D$LP.EspStackBeforeUnfolding esp

        ; [AsmMain] S'arrête si [FL.WeAreUnfolding] TRUE
        Call AsmMain

        Call 'USER32.SetCursor' D$H.CurrentCursor

        Call UnfoldOutput

        Mov D$edi CRLFEOS0,
            eax D$CodeSourceB

        Test D$eax NA NULL S1>

            Call 'USER32.SetDlgItemTextA' D$H.ShowUnfoldDialog,
                                          EDIT_UNFOLDIND_DIALOG,
                                          Trash3

            Mov D$FL.UnfoldCompleted &TRUE

        jmp L1>

S1:     Mov D$FL.UnfoldCompleted &FALSE

L1: Pop D$STRUCT.EditData@SourceEnd,
        D$SourceLen

    Call ReleaseAsmTables

    Mov D$FL.WeAreUnfolding &FALSE,
        D$FL.ReadyToRun &FALSE

    Call ReleaseAsmTables
ret

UnfoldingError:

    Mov D$UnfoldErrorMessage eax,
        D$FL.CompileErrorHappend &TRUE,
        D$FL.UnfoldCompleted &FALSE

    Call 'USER32.SetCursor' D$H.CurrentCursor

    While esp <> D$LP.EspStackBeforeUnfolding

        Pop ebx ; !!! TODO

    End_While

    jmp L1<
____________________________________________________________________________________________

[UnfoldErrorMessage: D$ ?]

GetUnfoldStatement:

    Push esi,
         ebx,
         ecx

        Mov esi D$CodeSourceA,
            eax 0,
            edx esi

        add edx D$SourceLen

        While eax <> ecx

            .If B$esi = EOI

                If B$esi+(1*ASCII) = OpenBracket

                Else_If B$esi+(1*ASCII) = OpenVirtual

                Else

                    add eax 1

                End_If

            .Else_If B$esi = OpenBracket

                add eax 1

            .Else_If B$esi = OpenVirtual

                add eax 1

            .End_If

            add esi (1*ASCII) | On esi > edx jmp S0>

        End_While

        ; Translate into normal Ascii form:
        Mov edi D$LP.Trash

        If B$esi-(1*ASCII) = OpenVirtual ; 016

            sub esi (1*ASCII) | Call TranslateDeclarationToNormalAscii

        Else_If B$esi-(1*ASCII) = OpenBracket ; 014

            sub esi (1*ASCII)| Call TranslateDeclarationToNormalAscii

        Else

            Call TranslateCodeToNormalAscii

        End_If

S0: Pop ecx,
        ebx,
        esi
ret
____________________________________________________________________________________________

TranslateDeclarationToNormalAscii:

L1: lodsb

        .If al = Space

            Mov al SPC

        .Else_If al = EOI

            jmp S0>>

        .Else_If al = meEOI

            Mov al CR | stosb | Mov al LF

        .Else_If al = TextSign

            Mov B$edi '"' | add edi (1*ASCII)

            While B$esi <> TextSign

                lodsb

                If al = CR

                    Mov W$edi CRLF | add edi (2*ASCII) | add esi (2*ASCII)

                Else

                    stosb

                End_If

            End_While

            add esi (1*ASCII) | Mov al '"'

        .Else_If al = MemMarker

            Mov al '$'

        .Else_If al = OpenBracket

            Mov al '['

        .Else_If al = CloseVirtual

            Mov al ']' | stosb

            Mov al CR | stosb | Mov al LF | stosb | jmp S0>>

        .Else_If al = CloseBracket

            Mov al ']' | stosb

            Mov al CR | stosb | Mov al LF | stosb | jmp S0>>

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

            Mov al ':' | stosb | Mov al SPC | stosb

        .End_If

        stosb

        If al = LF

            Mov D$edi '    ' | add edi (4*ASCII)

        End_If

    jmp L1<<

S0: Mov ax CRLF | stosw ;| Mov al 0 | stosb

ret
____________________________________________________________________________________________

TranslateCodeToNormalAscii:
L0: lodsb

    .If al = Space
        Mov al SPC
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

; Pos '0' = 31
[STR.A.UnfoldStepsTitle: B$ "
______________________________________________________________________

                      Unfold Macro Pass "

ID.UnfoldStep: B$ "0 
______________________________________________________________________

    " EOS]

UnfoldOutput:

    add B$ID.UnfoldStep 1

    Mov edi D$LP.Trash,
        esi STR.A.UnfoldStepsTitle

L0: movsb | Test B$esi NA NOT_EOS L0<

    Mov D$LP.Trash edi

    ; Count how many Statements, in the 'StatementsTable', down to our Line:
    Mov ebx D$LP.BlockStartText,
        esi D$StatementsTable

    ; For "some reason", leading Labels must be included in the Statement:
    Mov eax ebx | sub eax (1*ASCII) | While B$eax = SPC | sub eax (1*ASCII) | End_While

    If B$eax = ':'

        While B$eax-(1*ASCII) > SPC | sub eax (1*ASCII) | End_While

        Mov ebx eax

    End_If

    ; Unfold upon an Equal Pre-Parser Statement:
    If B$ebx-(1*ASCII) = 024 ;'$' ; CharMessage

        sub ebx (2*ASCII)

    Else_If B$ebx-(1*ASCII) = 0A7 ;'$'

        sub ebx (2*ASCII)

    End_If

    ; Several Statements are possible. Example, in Data and in Code with a Para-Macro:
    Mov ecx 1

    While D$esi <> EOS

        If D$esi = ebx

            Call GetUnfoldStatement

            Mov D$edi '    ' | add edi (4*ASCII)

            Mov D$LP.Trash edi

        End_If

        add esi (4*ASCII) | inc ecx

    End_While

ret
____________________________________________________________________________________________
____________________________________________________________________________________________

EncodeBoxError:

    Call ErrorMessageBox 0,
                         D$ErrorMessagePtr
    Mov esp D$OldStackPointer | sub esp 4
ret
____________________________________________________________________________________________

MarginRightClick:

    On D$LP.MEM.TABLE.BreakPoints = &NULL Call InitBreakPointsTables

    Call MouseTextPos
    Push ebx
        Call SearchTxtPtr | Mov D$BreakPointLocation eax
    Pop ebx

    Mov D$STRUCT.EditData@PhysicalCaretRow eax, D$STRUCT.EditData@CaretRow eax, D$StartBlockCol eax, D$EndBlockCol eax,
        D$STRUCT.EditData@CaretLine ebx, D$StartBlockLine ebx, D$EndBlockLine ebx

    Mov D$STRUCT.EditData@CaretRow 1

    Mov eax D$BreakPointLocation | Call IsEaxInBreakPointOnTable

    Call BreakPointMenu
ret
____________________________________________________________________________________________

DoubleClick:

    Call MouseTextPos

    Mov D$STRUCT.EditData@PhysicalCaretRow eax,
        D$STRUCT.EditData@CaretRow eax,
        D$StartBlockCol eax,
        D$EndBlockCol eax,
        D$STRUCT.EditData@CaretLine ebx,
        D$StartBlockLine ebx,
        D$EndBlockLine ebx

    If D$DBPMenuOn = DOUBLE_CLICK_ACTION

        On B$ClickOnMargin = &TRUE jmp DoubleClickMarginAction

    End_If

    Call SearchTxtPtr

    Mov al B$esi | Call WordEdge | On B$Edge = &TRUE, ret

    Push esi

        std

L0:       lodsb | Call WordEdge | cmp B$Edge &TRUE | jne L0<         ; search start

        cld

        add esi (2*ASCII) | Mov D$LP.BlockStartText esi

    Pop esi

L0: lodsb | Call WordEdge | cmp B$Edge &TRUE | jne L0<               ; search end

    sub esi (2*ASCII) | Mov D$LP.BlockEndText esi

    Mov D$FL.BlockInside &TRUE | Call SetCaret esi

    Call AskForRedraw | Call RightClickOnBlock

ret
____________________________________________________________________________________________


_________________________________________________________

; See comment for rotary BackTable at "SetBackTableMemory"

ClearBackTable:

    Mov eax D$BackTable, D$BackTablePtr eax

    Call ClearPATH D$BackTablePtr

ret


StorePosInBackTable:
    Mov ebx D$BackTablePtr, eax D$STRUCT.EditData@UpperLine
    Mov D$ebx eax | add bl 4 | Mov D$ebx 0
    Mov D$BackTablePtr ebx, B$MovingBack  &FALSE
ret


[MovingBack: D$ ?]

BackClick:
    Mov eax D$CodeSource | On D$STRUCT.EditData@SourceEnd = eax, ret

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
    Mov D$BackTablePtr ebx, D$STRUCT.EditData@UpperLine eax
    Mov D$STRUCT.EditData@CaretRow 1, D$STRUCT.EditData@CaretLine 0, D$STRUCT.EditData@CurrentWritingPos eax
    Call TryToMove
    Call ResetUpperline
    Call AskForRedraw
L9: ret


ForwardClick:
    Mov eax D$CodeSource | On D$STRUCT.EditData@SourceEnd = eax, ret

    Mov ebx D$BackTablePtr | add bl 4
    Mov eax D$ebx | cmp eax 0 | je L9>
      Mov D$BackTablePtr ebx, D$STRUCT.EditData@UpperLine eax
      Mov D$STRUCT.EditData@CaretRow 1, D$STRUCT.EditData@CaretLine 0, D$STRUCT.EditData@CurrentWritingPos eax
      Call TryToMove | Call ResetUpperline | Call AskForRedraw
L9: ret


; If text lenght have change between two right-click moves, D$Upperline may point to
; any char in a line. We ensure start of line in D$Upperline:

ResetUpperline:
    Mov esi D$STRUCT.EditData@Upperline
L0: cmp B$esi-1 LF | je L9>
        dec esi | jmp L0<
L9: Mov D$STRUCT.EditData@Upperline esi
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
