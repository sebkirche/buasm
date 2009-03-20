TITLE Error           ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________
;;
 'error' deal. 'OutOnError'

 how it works:

 Because of the structure of RosAsm (multiple passes with source transformations), pointing
 a bad statement, in true source file is a real head break. I thaught of two solutions:
 line numbers, that would allowed jumping from one state to a previous one, and error
 level flag that would allowed jumping to specific error check routines level-dependant.
 i choose the second solution. Heavy, but it works,... as far as i can know. Sometimes
 dirty; always difficult to understand. Sorry; after 3 writings from scratch, i think it
 can't be really simple anyway.

 In this version, 2 main routines (+ others...) are used, depending on the fact that the 
 error occurred inside EOIs (|...|) or inside brackets ([...]). Some ajustements are made:
 
- when a statement is the result of macro expending (doesn't exist in first source text),
 it is inside 'meEOIs' (Macro expension End Of Instruction) so that they are not counted
 in 'StatementCounter'.
 
- when error occur in data storing work any 'non data' bracket must be added to
 'bracketCounter'
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[OldStackPointer: D$ ?]

[bracketCounter: D$ ?
 StatementsCounter: D$ ?
 ErrorLevel: D$ ?
 InsideBracket: D$ ?
 InsideComment: D$ ?
 InsideMLC: D$ ?]

[Error | pushad | CookedError #2>L | popad | Mov eax, #1 | jmp OutOnError]

[CookedError | Mov esi #1 | Call ViewCookedError]
 ________________________________________________________________________________________

[LinesCounter: D$ ?
 DontCountNext: D$ ?
 EndingComma: D$ ?
 StatementsTable: D$ ?
 StatementsTable2: D$ ?
 StatementsPtr: D$ ?
 StatementsPtr2: D$ ?]

TestStatementsTable:

    Mov ecx 25,
        esi D$StatementsTable

L0: lodsd

    If eax = 0

        Call HexPrint eax | jmp L9>

    Else_If eax < D$CodeSource

        Call HexPrint eax | jmp L0<

    Else_If eax > D$STRUCT.EditData@SourceEnd

        Call HexPrint eax | jmp L0<

    Else

        Mov ebx eax | add ebx 3

        Mov dl B$ebx,
            B$OldChar dl,
            B$ebx 0,
            D$OldCharPos ebx

        pushad

            Mov esi eax

            Call SetDebuggeeText

            Call AskForRedrawNow

        popad

        Call HexPrint eax ; for viewing the source.

    End_If

  ; Uses '25',for partial testing of big Files:
  ; loop L0<
    jmp L0<

L9: ret
_____________________________________________________________________________________

; final error prompting:

[ErrorMessagePtr: D$ ?]

ShowError:
; First, we make the MessageBox a little wider if the title is longer than the Message.
; Win set the Box width on the Message (not on the Title).
    pushad
        Mov ecx 10, edx 0
        While B$eax > 0
            inc eax | inc ecx        ; > lenght of Title
        End_While
        Mov edi Trash2
        While B$esi <> 0FF
            movsb | inc edx          ; lenght of Message Text
        End_While

        If edx < ecx
            sub ecx edx
            shl ecx 1
            Mov al SPC | rep stosb   ; ajusts text widht around the title width
        End_If
;;
  If CookedErrorMessage Buffer is not empty, this is because Errror Macro has been
  evocated with two Parameters 1) Error Title 2) Pointer to the internal Member of
  the Instruction that produced the error. If two Parameters, we add this Member
  under the Statement viewed in the Dialog Box. 'CookedErrorMessage' has been "Un-Cooked"
  by 'ViewCookedError'.
;;
        If D$CookedErrorMessage = 0
            Mov al 0 | stosb
        Else
            Mov W$edi CRLF, W$edi+2 CRLF | add edi 2 ;4
            Mov D$edi '>>> ' | add edi 4
            Mov esi CookedErrorMessage

            While B$esi <> 0
                movsb | On esi = EndOfCookedErrorMessage, jmp L1>
            End_While
L1:         Mov B$edi 0

        End_If

        Mov esi D$OldCharPos, al B$OldChar, B$esi al
        dec esi | Mov D$BlockEndTextPtr esi
    popad
    pushad
      ;Call ErrorMessageBox Trash2, eax
      Call ReleaseAsmTables
    popad

    Call SetDebuggeeText

    Call ErrorMessageBox Trash2, D$ErrorMessagePtr
ret


SetDebuggeeText:
    Mov D$BlockStartTextPtr esi, D$STRUCT.EditData@UpperLine esi, D$STRUCT.EditData@CurrentWritingPos esi

    Call TryToMove

    Mov ebx 0, esi D$STRUCT.EditData@UpperLine               ; Where is the block at screen?
L0: lodsb | On al = LF, inc ebx
    cmp esi D$BlockStartTextPtr | jb L0<

    Mov ecx D$STRUCT.EditData@LineNumber | shr ecx 1         ; if higher than half screen,
    shr ecx 1
    .If ebx < ecx                             ; scroll to half screen.
        sub ecx ebx                           ; if down, we are at end of source...

      ; Also, don't Move, for error pointing, upper than possible:
        If D$LP.ActualTitle <> &NULL
            Mov edx D$LP.ActualTitle
        Else
            Mov edx D$CodeSource
        End_If

L2:     cmp D$STRUCT.EditData@UpperLine edx | jbe L3>
        Call UpOneLine | loop L2<
   .End_If

L3: Mov D$FL.BlockInside &TRUE

    Mov esi D$OldCharPos, al B$OldChar, B$esi al

    Call SearchForEndOfErrorBlock
    Call SetCaret D$BlockEndTextPtr

  ; Clear possible previous ShiftPos, in case user first hit [Shift] right after
  ; the error isq pointed out:
    Move D$ShiftBlockCol D$STRUCT.EditData@CaretRow, D$ShiftBlockLine D$STRUCT.EditData@CaretLine

    Call AskForRedrawNow
ret


[LastErrorBlockPipe: D$ ?]

SearchForEndOfErrorBlock:
    Mov esi D$BlockStartTextPtr, ecx 0, D$LastErrorBlockPipe 0
    On B$esi = '[', jmp SearchForEndOfErrorBracketBlock

L0: lodsb | On esi = D$STRUCT.EditData@SourceEnd, jmp L2>>

    If al = "'"
        While B$esi <> "'" | inc esi | On esi = D$STRUCT.EditData@SourceEnd, jmp L2>>
        End_While | inc esi
        Mov ecx 0
    Else_If al = '"'
        While B$esi <> '"' | inc esi | On esi = D$STRUCT.EditData@SourceEnd, jmp L2>
        End_While | inc esi
        Mov ecx 0
    Else_If al = ';'
        On ecx = 0, jmp L2>
        While B$esi >= SPC | inc esi | On esi = D$STRUCT.EditData@SourceEnd, jmp L2>
        End_While | add esi 2
    Else_If al = ','
        Mov ecx 1
    Else_If al = '|'
        On D$LastErrorBlockPipe = 0, Mov D$LastErrorBlockPipe, esi
    Else_If al < SPC
        On ecx = 0, jmp L2>
    Else_If al = SPC

    Else
        Mov ecx 0
    End_If
    jmp L0<<

L2: sub esi 2 | Mov D$BlockEndTextPtr esi
    If D$LastErrorBlockPipe <> 0
        Mov eax D$LastErrorBlockPipe | sub eax 2 | Mov D$BlockEndTextPtr eax
    End_If

    Move D$STRUCT.EditData@CurrentWritingPos D$BlockEndTextPtr
ret


SearchForEndOfErrorBracketBlock:
L0: lodsb | On esi = D$STRUCT.EditData@SourceEnd, jmp L2>

    If al = "'"
        While B$esi <> "'" | inc esi | End_While | inc esi
    Else_If al = '"'
        While B$esi <> '"' | inc esi | End_While | inc esi
    Else_If al = ';'
        While B$esi >= SPC | inc esi | End_While | add esi 2
    Else_If al = ']'
        jmp L2>
    End_If
    jmp L0<

L2: Mov D$BlockEndTextPtr esi
ret

 _________________________________________________________________________________________

[CompileErrorHappend: D$ ?
 FirstPass: D$ ?]

OutOnError:
    Mov D$ErrorMessagePtr eax

    Mov B$CompileErrorHappend &TRUE, D$NextSearchPos 0

    On B$WeAreChecking = &TRUE, ret
    On B$WeAreUnfolding = &TRUE, jmp UnfoldingError
   ; Error in the second part of the Encode-DecodeBox 'B$Errorlevel 7'
    On B$Errorlevel = 7,  jmp EncodeError
    On B$WeAreInTheCodeBox = &TRUE, jmp EncodeBoxError

    cld
L0: Mov esp D$OldStackPointer               ; restor stack (...comming from anywhere)

L1: pushad

       Call CloseProgressBar
    popad

    On B$Errorlevel = 0,  jmp Error0        ; open text error in 'SourceCleaner'
    On B$Errorlevel = 1,  jmp Error1        ; error inside square bracket
    On B$Errorlevel = 2,  jmp Error2        ; error in statements
    On B$Errorlevel = 3,  jmp Error3        ; error inside     ;;;;;;;;;;;;StoreDatas
    On B$Errorlevel = 4,  jmp Error4        ; error in DLL name ; Mov B$Errorlevel 1
    On B$Errorlevel = 5,  jmp Error5        ; error in api name  ; Mov B$Errorlevel 5
    On B$Errorlevel = 6,  jmp Error6        ; error in the form of api Call
  ; Error7 is reserved for the Encode-Decode Box.
    On B$Errorlevel = 8,  jmp Error8        ; error in Win Equate Name
    On B$Errorlevel = 11,  jmp Error11      ; error in ClASSes
   ; On B$Errorlevel = 12,  jmp Error12      ; Short Displacement error

  ; Error9 (or none) falls there:
    Call AskForRedrawNow

    If D$CookedErrorMessage <> 0
        Call ErrorMessageBox CookedErrorMessage, D$ErrorMessagePtr
    Else
        Call ErrorMessageBox 0, D$ErrorMessagePtr
    End_If

    Call ReleaseAsmTables

    ret ; for 'no search' errors (esi known) or 'Main:' missing

____________________________________________________________________________________________

[DashLine: B$ '___________________' EOS]

;;
  To YeoH: '.zh' should be the extension of your 'RosAsmStrings' File.
  
  I hope you will succeed to make this "...If D$StringsLanguage = '.zh'" work... ;)
  As, off course, as you may guess, i cannot see anything on my Computer...
;;

Proc ErrorMessageBox:
    Arguments @Text1, @Text2

        ...If D$StringsLanguage = '.zh'
          ; Unicode:
            Mov esi D@Text1, edi Trash1, ecx 0
            If esi <> 0
                While W$esi > 0
                    movsw | inc ecx | cmp ecx 400 | ja L1>
                End_While
            End_If
L1:         Mov W$edi 0

            .If D@Text2 <> 0
                Mov esi D@Text2, edi Trash2, ecx 0
                If esi <> 0
                    While W$esi > 0
                        movsw | inc ecx | cmp ecx 400 | ja L1>
                    End_While
                End_If
L1:             Mov W$edi 0
            .Else
                Mov W$Trash2 0
            .End_If

          ; Tag Dialog 10

            Call 'USER32.DialogBoxIndirectParamW' D$H.Instance, ErrorUnicodeDialog,
                                                  &NULL, ErrorMessageProcW, &NULL

        ...Else
          ; Ascii:
            If D@Text1 <> 0
                Mov esi D@Text1, edi Trash1, ecx 0 | Mov D$edi CRLF | add edi 2
                While B$esi > 0
                    movsb | inc ecx | cmp ecx 400 | ja L1>
                End_While
            End_If
L1:         Mov B$edi 0

            .If D@Text2 <> 0
                Mov esi D@Text2, edi Trash2, ecx 0 | Mov D$edi CRLF | add edi 2
                If esi <> 0
                    While B$esi > 0
                        movsb | inc ecx | cmp ecx 400 | ja L1>
                    End_While
                End_If
L1:             Mov B$edi 0
            .Else
                Mov B$Trash2 0
            .End_If

            Mov al B$Trash1+2 | or al B$Trash2
          ; Tag Dialog 10
            On al <> 0,
            Call 'USER32.DialogBoxParamA' D$H.Instance, 10, D$H.MainWindow, ErrorMessageProcA, &NULL

        ...End_If
EndP

[ErrorUnicodeDialog: D$ 090CC08C2 0        ; Style
 U$ 03 0 0 0DC 065             ; Dim
 0                             ;      no Menu
 '' 0                          ; Class
 '' 0                          ; Title
 12 'SimSun' 0]                ; Font  MS Song // SimSun  //   'MS Song', 0] ;

[Control0000: D$ 050800004 0      ; Style
 U$ 0 01 0DC 028               ; Dim
 0A                            ; ID
 0FFFF 081                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data

[Control0001: D$ 050800004 0      ; Style
 U$ 0 02A 0DC 028              ; Dim
 014                           ; ID
 0FFFF 081                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data

[Control0002: D$ 050000000 0      ; Style
 U$ 050 054 037 010            ; Dim
 01                            ; ID
 0FFFF 080                     ; Class
 'OK' 0                        ; Title
 0]                            ; No creation data


[uError: U$ 'Error' EOS]

; Tag Dialog 10

Proc ErrorMessageProcW:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_INITDIALOG
        Call 'USER32.SetClassLongW' D@hwnd, &GCL_HICON, D$STRUC.WINDOWCLASS@hIcon

        Call 'USER32.SendDlgItemMessageW' D@hwnd, 10, &WM_SETFONT,
                                              D$H.Font1, &FALSE

        On D$H.NationalFont <> 0,
            Call 'USER32.SendDlgItemMessageW' D@hwnd, 20, &WM_SETFONT,
                                              D$H.NationalFont, &FALSE

        Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &EM_SETMARGINS,
                                          &EC_LEFTMARGIN__&EC_RIGHTMARGIN, 10
        Call 'USER32.SendDlgItemMessageW' D@hwnd, 20, &EM_SETMARGINS,
                                          &EC_LEFTMARGIN__&EC_RIGHTMARGIN, 10

        Call 'USER32.SendMessageW' D@hwnd, &WM_SETTEXT, &NULL, uError
                                   ;D$ErrorMessageTitlePtr

        Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &WM_SETTEXT, 0, Trash1

        On B$trash2 <> 0,
        Call 'USER32.SendDlgItemMessageW' D@hwnd, 20, &WM_SETTEXT, 0, Trash2

        Call 'USER32.GetDlgItem' D@hwnd, 1
        Call 'USER32.SetFocus' eax

        jmp L8>>
;;
    ...Else_If D@msg = &WM_SETFONT
        If D$NationalFont <> 0
            popad | Mov eax D$H.NationalFont | ExitP
        End_If
;;
    ...Else_If D@msg = &WM_COMMAND
        Mov eax D@wParam | and D@wParam 0FFFF | shr eax 16

        If D@wParam = &IDCANCEL
            Call 'USER32.EndDialog' D@hwnd, &NULL

        Else_If D@wParam = &IDOK
            Call 'USER32.EndDialog' D@hwnd, &NULL

        End_If

    ...Else_If D@msg = &WM_CTLCOLOREDIT
        Call 'USER32.SendMessageW' D@lParam, &EM_SETSEL, 0-1, 0
        Call 'GDI32.SetBkColor' D@wParam, D$DialogsBackColor
        popad | Mov eax D$H.DialogsBackGroundBrush | ExitP

    ...Else
L8:     popad | Mov eax &FALSE | ExitP

    ...End_If

    popad | Mov eax &TRUE
EndP


Proc ErrorMessageProcA:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_INITDIALOG
        Call 'USER32.SetClassLongA' D@hwnd, &GCL_HICON, D$STRUC.WINDOWCLASS@hIcon

        Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &EM_SETMARGINS,
                                        &EC_LEFTMARGIN__&EC_RIGHTMARGIN, 10

        Call 'USER32.SendMessageA' D@hwnd, &WM_SETTEXT, &NULL,
                                   D$ErrorMessageTitlePtr

        Call 'USER32.SendDlgItemMessageA' D@hwnd, 20, &EM_SETMARGINS,
                                        &EC_LEFTMARGIN__&EC_RIGHTMARGIN, 10


        Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &WM_SETTEXT, 0, Trash1

        On D$H.NationalFont <> 0,
            Call 'USER32.SendDlgItemMessageA' D@hwnd, 20, &WM_SETFONT,
                                              D$H.NationalFont, &FALSE

        Call 'USER32.SendDlgItemMessageW' D@hwnd, 10, &WM_SETFONT,
                                              D$H.Font1, &FALSE

        On B$trash2 <> 0,
            Call 'USER32.SendDlgItemMessageA' D@hwnd, 20, &WM_SETTEXT, 0, Trash2

        Call 'USER32.GetDlgItem' D@hwnd, 1
        Call 'USER32.SetFocus' eax

        jmp L8>>

    ...Else_If D@msg = &WM_COMMAND

        If W@wParam = &IDCANCEL
            Call 'USER32.EndDialog' D@hwnd, &NULL
        Else_If W@wParam = &IDOK
            Call 'USER32.EndDialog' D@hwnd, &NULL
        End_If

    ...Else_If D@msg = &WM_CTLCOLOREDIT
        Call 'USER32.SendMessageA' D@lParam, &EM_SETSEL, 0-1, 0
        Call 'GDI32.SetBkColor' D@wParam, D$DialogsBackColor
        popad | Mov eax D$H.DialogsBackGroundBrush | ExitP

    ...Else
L8:     popad | Mov eax &FALSE | ExitP

    ...End_If

    popad | Mov eax &TRUE
EndP


____________________________________________________________________________________________

[CookedErrorMessage: D$ ? # 20]
[EndOfCookedErrorMessage: D$ ? ?]

;;
  This shows the internal Source of error. Can't work actually as i don't know how to
  to make sure that esi points to something inside the Assembler Tables Memory, and i
  don't find any function that would say if the pointer is valid or not. Nothing like
  'ReadProcessmemory' in the Debugger. There is 'KERNEL32.SetErrorMode', but i don't
  know if it works or not for x86, and what Message is suppose to be sent back.
  
  Would probably require SEH...
;;

ViewCookedError:
    pushad
        While B$esi > EOI | dec esi | End_While | inc esi

        Mov edi CookedErrorMessage

L0:     lodsb
        .If al = TextSign
            Mov al '"'
        .Else_If al = numSign
            Mov al '#'
        .Else_If al = CommaSign
            Mov al ','
        .Else_If al = OpenVirtual
            Mov al '{'
        .Else_If al = CloseVirtual
            Mov al '}'
        .Else_If al = Openbracket
            Mov al '['
        .Else_If al = Closebracket
            Mov al ']'
        .Else_If al = memMarker
            Mov al '$'
        .Else_If al = colonSign
            Mov al ':'
        .Else_If al = openSign
            Mov al '('
        .Else_If al = closeSign
            Mov al ')'
        .Else_If al = addSign
            Mov al '+'
        .Else_If al = subSign
            Mov al '-'
        .Else_If al = mulSign
            Mov al '*'
        .Else_If al = divSign
            Mov al '/'
        .Else_If al = expSign
            Mov al '^'
        .Else_If al = Space
            Mov al SPC
        .Else_If al = EOI
            Mov al '|' | jmp L9>
        .Else_If al = meEOI
            Mov al '|'
        .End_If

        stosb

        On edi < EndOfCookedErrorMessage, jmp L0<<

L9:     Mov B$edi 0

    popad
ret

 _________________________________________________________________________________________

; open text error in 'SourceCleaner'

[OldChar: D$ ?
 OldCharPos: D$ ?]

SetEndOfErrorText:

    Mov esi D$StatementsPtr, esi D$esi

    .If esi < D$CodeSource
        Mov esi D$CodeSource

    .Else_If esi > D$STRUCT.EditData@SourceEnd
        Mov esi D$STRUCT.EditData@SourceEnd | std
L0:     lodsb
        If al = LF
            inc esi ; stop
        Else_If al = '|'
            inc esi ; stop
        Else_If al = '['
                    ; stop
        Else_If esi > D$CodeSource
                    ; stop
        Else
            jmp L0<
        End_If
        cld
    .End_If

DirectSourcePointing:
    Mov ecx 200
    Push esi, eax
L0:     lodsb
        If al <= CR
            dec esi ; stop
        Else_If al = '|'
            dec esi ; stop
        Else_If al = ']'
                    ; stop
        Else_If esi > D$STRUCT.EditData@SourceEnd
                    ; stop
        Else
            loop L0<
        End_If
        Mov al B$esi, B$OldChar al, B$esi 0FF, D$OldCharPos esi
        dec esi | Mov D$BlockEndTextPtr esi  ;, D$CurrentWritingPos esi ; ?Case of Bad Pos?
    Pop eax esi

    Call VerifyNotOneChar
ret


; Because Blocks of one char was not outputed, in older version of the Editor.
; Should be of no more use now:

VerifyNotOneChar:
    Push esi, eax
        Mov eax D$BlockEndTextPtr
        cmp eax D$BlockStartTextPtr | ja L9>
            inc D$BlockEndTextPtr
            Mov eax D$BlockEndTextPtr, al B$eax
                On al = CR, inc D$BlockEndTextPtr
                Mov esi D$OldCharPos, al B$OldChar, B$esi al
                Mov esi D$BlockEndTextPtr | inc esi
                Mov al B$esi, B$OldChar al, B$esi 0FF, D$OldCharPos esi
L9: Pop eax, esi
ret


Error0:

    Call SetEndOfErrorText

    jmp ShowError
 _________________________________________________________________________________________

; error inside square bracket: we search [bracketCounter] square bracket number

Error1:
    Push eax
      Mov esi, D$CodeSource | Mov ecx, 0
L1:   lodsb | cmp al '"' | jne L3>
L2:   lodsb | cmp al '"' | jne L2<         ; strip "text"
        jmp L1<
L3:   cmp al "'" | jne L5>
L4:   lodsb | cmp al "'" | jne L4<         ; strip 'text'
        jmp L1<


L5: cmp al ';' | jne L7>                     ; jmp over comments
        If D$esi-2 = MLC   ; (LF ; ; CR)
            Do
                inc esi | cmp esi D$STRUCT.EditData@SourceEnd | jae L9>
            Loop_Until D$esi = MLC
            add esi 3 | cmp esi D$STRUCT.EditData@SourceEnd | jae L9>
        Else
L6:         lodsb | cmp al LF | jne L6<
        End_If
        jmp L1<

;L5:   cmp al ';' | jne L7>
;L6:   lodsb | cmp al LF | jne L6<          ; strip comments
;        jmp L1<

L7:   cmp al '[' | jne L1<
        inc ecx | cmp ecx D$bracketCounter | jb L1<

      Push esi
L8:     lodsb | cmp al ']' | jne L8<
          Mov al B$esi, B$esi 0FF, B$OldChar al, D$OldCharPos esi
      Pop esi
L9: Pop eax

    dec esi | jmp ShowError
 _________________________________________________________________________________________

; error in statements. We search D$StatementsCounter '|' number
; Nothing but a modified version of text cleaner first part.

Error2:
    Mov esi D$StatementsCounter
 ; showme eax
    Push esi, eax
L0:     lodsb
        If al = CR
            dec esi ; stop
        Else_If al = '|'
            dec esi ; stop
        Else_If al = ']'
                    ; stop
        Else_If esi > D$STRUCT.EditData@SourceEnd
                    ; stop
        Else
            jmp L0<
        End_If

    Mov al B$esi, B$OldChar al, D$OldCharPos esi, B$esi 0FF

    Pop eax esi

    jmp ShowError

 ________________________________________________________________________________________
;;
 error inside ReplaceEquate in brackets statements or...
 error inside StoreData: value in [bracketCounter] did not count equates and macros
 so that we have to do a new count in ecx before jumping to error1 bracket research.
;;

Error3:
;;
 now: ecx = lenght, esi > start of bad Name in Data.
 Used now only by 'SearchRegularLabel' when filling Data symbols evocations. At this
 time, we do not have any more the source Data pointers available in "StatementsTable".
;;
    pushad

    While B$esi-1 > LowSigns | dec esi | End_While

    Push esi
        Mov ecx 1
        While B$esi > LowSigns | inc esi | inc ecx | End_While
    Pop esi

    dec ecx | Mov D$LenOfSearchedString ecx

    Mov edi SearchString | rep movsb | Mov al 0 | stosb

    Push D$DownSearch, D$CaseSearch, D$WholeWordSearch, D$STRUCT.EditData@CurrentWritingPos
        Mov B$DownSearch &TRUE, B$CaseSearch &FALSE, B$WholeWordSearch &TRUE
        Move D$STRUCT.EditData@CurrentWritingPos D$CodeSource

        Mov D$Trash2 0

        Push D$NextSearchPos
            Mov D$NextSearchPos 0
L0:         Call StringSearch | On D$FL.BlockInside = &FALSE, jmp L7>>

          ; just in case the searched word is too inside a comment:
            Mov esi D$BlockStartTextPtr | dec esi
            .While B$esi > LF
                If B$esi = '"'
                    dec esi
                    While B$esi <> '"'
                        dec esi
                    End_While
                Else_If B$esi = "'"
                    dec esi
                    While B$esi <> "'"
                        dec esi
                    End_While
                Else_If B$esi = ';'
                    jmp L0<
                End_If
                dec esi
            .End_While

            Mov esi D$BlockStartTextPtr, edi Trash2, ecx D$BlockEndTextPtr
            sub ecx D$BlockStartTextPtr | inc ecx | rep movsb | Mov al 0 | stosb

L7: Pop D$NextSearchPos
    Pop D$STRUCT.EditData@CurrentWritingPos, D$WholeWordSearch, D$CaseSearch, D$DownSearch
    popad
    pushad
        Call AskForRedrawNow

        If D$Trash2 <> 0
            Call ErrorMessageBox Trash2, D$ErrorMessagePtr
        Else
            Call ErrorMessageBox esi, D$ErrorMessagePtr
        End_If
        Call ReleaseAsmTables
    popad
ret

_________________________________________________________________________________________

; Error in DLL name:

Error4:
    Mov B$CompileErrorHappend &TRUE

    Push esi

;L9: pushad
;        ;Call 'USER32.MessageBoxA' D$H.MainWindow, esi, eax, &MB_SYSTEMMODAL
;        Call AskForRedrawNow
;
;        Call ErrorMessageBox esi, D$ErrorMessagePtr
;    popad

    Mov edi SearchString, B$DownSearch &TRUE, B$WholeWordSearch &FALSE
    Move D$STRUCT.EditData@CurrentWritingPos D$CodeSource
    Mov ecx 0

L0: inc ecx | lodsb | stosb | cmp al 0 | jne L0<
    dec edi | Mov al '.' | stosb | Mov D$LenOfSearchedString ecx

    Call  StringSearch

    Call AskForRedrawNow

    Pop esi

    Call ErrorMessageBox esi, D$ErrorMessagePtr

    Call ReleaseAsmTables
ret


; error in function name:

Error5:
    Mov B$CompileErrorHappend &TRUE

    ;Push esi

    cld

;L9: ;Call 'USER32.MessageBoxA' D$H.MainWindow, esi, eax, &MB_SYSTEMMODAL
;    Call ErrorMessageBox esi, D$ErrorMessagePtr

    Mov edi SearchString, B$DownSearch &TRUE, B$WholeWordSearch &FALSE, B$CaseSearch &FALSE
    Move D$STRUCT.EditData@CurrentWritingPos D$CodeSource

    Mov esi D$StartOfFunctionName, ecx 0

L0: inc ecx | lodsb | stosb | cmp al 0 | jne L0<
    dec ecx
    Mov D$LenOfSearchedString ecx

    Call  StringSearch

    Call AskForRedrawNow

    ;Pop esi

    Call ErrorMessageBox D$StartOfFunctionName, D$ErrorMessagePtr

    Call ReleaseAsmTables
ret


; for bad api Call formulation:

Error6: ; Mov B$ErrorLevel 6
    Push esi
    Mov B$CompileErrorHappend &TRUE

L9: ;pushad
      ;Call 'USER32.MessageBoxA' D$H.MainWindow, esi, eax, &MB_SYSTEMMODAL
    ;  Call ErrorMessageBox esi, D$ErrorMessagePtr
    ;popad

    Mov edi SearchString, B$DownSearch &TRUE, B$WholeWordSearch &FALSE
    Move D$STRUCT.EditData@CurrentWritingPos D$CodeSource
    Mov ecx 0

L0: inc ecx | lodsb | stosb | cmp al 0 | jne L0<
    dec ecx | Mov D$LenOfSearchedString ecx

    Call StringSearch

    Call AskForRedrawNow

    Pop esi

    Call ErrorMessageBox esi, D$ErrorMessagePtr

    Call ReleaseAsmTables
ret


; For Bad Win32 Equate Name:

Error8:
    Mov B$CompileErrorHappend &TRUE

    Push esi

;;;    Mov B$esi 0
;;;    While B$esi <> '&' | dec esi | End_While

   ; pushad
   ;   ;Call 'USER32.MessageBoxA' D$H.MainWindow, esi, eax, &MB_SYSTEMMODAL
   ;   Call ErrorMessageBox esi, D$ErrorMessagePtr
   ; popad

    Push D$DownSearch, D$WholeWordSearch
        Mov edi SearchString, B$DownSearch &TRUE, B$WholeWordSearch &FALSE
        Move D$STRUCT.EditData@CurrentWritingPos D$CodeSource
        Mov ecx 0

L0:     inc ecx | lodsb | stosb | cmp al 0 | jne L0<
        dec ecx | Mov D$LenOfSearchedString ecx


L1:     Call StringSearch | cmp B$StringFound &FALSE | je L2>
        Mov esi D$BlockEndTextPtr | Mov al B$esi+1

      ; Next Char Must be some separator, or some '_':
        If al = '_'
            ; OK
        Else
            Call WordEdge | On B$Edge = &FALSE, jmp L1<
        End_If

L2:     Call ReleaseAsmTables
    Pop D$WholeWordSearch, D$DownSearch

    Pop esi

    Call AskForRedrawNow

    Call ErrorMessageBox esi, D$ErrorMessagePtr
ret


Error9:
    Call DirectSourcePointing | jmp ShowError
ret


Error11:
    Mov B$CompileErrorHappend &TRUE

    Push esi

L9: ;pushad
    ;  ;Call 'USER32.MessageBoxA' D$H.MainWindow, esi, eax, &MB_SYSTEMMODAL
    ;  Call ErrorMessageBox esi, D$ErrorMessagePtr
    ;popad

    Mov edi SearchString, B$DownSearch &TRUE, B$WholeWordSearch &FALSE
    Move D$STRUCT.EditData@CurrentWritingPos D$CodeSource
    Mov esi OneCLASSname, ecx 0

L0: inc ecx | lodsb | stosb | cmp al 0 | jne L0<
    dec ecx | Mov D$LenOfSearchedString ecx

L1: Call StringSearch
    Mov esi D$BlockStartTextPtr | dec esi
    While B$esi = SPC | dec esi | End_While
    Mov eax D$esi-4 | and eax (not 020202020)

  ;  On eax <> 'CLAS', jmp L1<
  ;  On B$esi-5 <> '5', jmp L1<

    sub esi 5 | Mov D$BlockStartTextPtr esi

    While B$esi <> ']' | inc esi | End_While
    Mov D$BlockEndTextPtr esi

    Call ReleaseAsmTables
    Call AskforRedraw

    Pop esi

    Call ErrorMessageBox esi, D$ErrorMessagePtr
ret

;;
Error12:
    Push eax
      ; Write the Number out out of range Bytes at 'TooLongOf'
        Mov edi D$TooLongOfPtr, D$edi '    ' | Mov eax ebx | Call WriteEax

      ; Kill the Line Break between 'ShortDis' and 'TooLongOf'
        Mov esi D$TooLongOfPtr | dec esi
        While B$esi >= SPC | Mov B$esi SPC | dec esi | End_While
        While B$esi < SPC | Mov B$esi SPC | dec esi | End_While
    Pop eax
    
    jmp Error2
ret
;;


[CookedError2 | Mov esi #1 | Call ViewCookedError2]

[SyntaxErrorInMacro1 | pushad | inc #2 | CookedError2 #2>L | popad
pushad
Mov eax, #1 | Move D$ErrorMessagePtr #1 | Call ErrorMessageBox CookedErrorMessage, D$ErrorMessagePtr
popad]


ViewCookedError2:
    pushad
    Mov eax esi


    sub esi 20 ; 20 is the size of CookedErrorMessage message
    Mov edi CookedErrorMessage

    Do
        movsb
    Loop_Until esi = eax

     Mov B$edi 0

    popad
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
