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

[Error | Call ViewCookedError #2>L | Mov eax #1 | jmp OutOnError]
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

        sub esi 1 | Mov D$LP.BlockEndText esi

    popad

    pushad

      ;Call ErrorMessageBox Trash2, eax
      Call ReleaseAsmTables

    popad

    Call SetDebuggeeText

    Call ErrorMessageBox Trash2, D$ErrorMessagePtr
ret


SetDebuggeeText:
    Mov D$LP.BlockStartText esi, D$STRUCT.EditData@UpperLine esi, D$STRUCT.EditData@CurrentWritingPos esi

    Call TryToMove

    Mov ebx 0, esi D$STRUCT.EditData@UpperLine               ; Where is the block at screen?
L0: lodsb | On al = LF, inc ebx
    cmp esi D$LP.BlockStartText | jb L0<

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

    Call SetCaret D$LP.BlockEndText

  ; Clear possible previous ShiftPos, in case user first hit [Shift] right after
  ; the error isq pointed out:
    Move D$ShiftBlockCol D$STRUCT.EditData@CaretRow, D$ShiftBlockLine D$STRUCT.EditData@CaretLine

    Call AskForRedrawNow
ret


[LastErrorBlockPipe: D$ ?]

SearchForEndOfErrorBlock:
    Mov esi D$LP.BlockStartText, ecx 0, D$LastErrorBlockPipe 0
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

L2: sub esi 2 | Mov D$LP.BlockEndText esi

    If D$LastErrorBlockPipe <> 0
        Mov eax D$LastErrorBlockPipe | sub eax 2 | Mov D$LP.BlockEndText eax
    End_If

    Move D$STRUCT.EditData@CurrentWritingPos D$LP.BlockEndText
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

L2: Mov D$LP.BlockEndText esi

ret
_________________________________________________________________________________________

[FL.CompileErrorHappend: D$ ?
 FirstPass: D$ ?]

OutOnError:

    Mov D$ErrorMessagePtr eax

    Mov D$FL.CompileErrorHappend &TRUE,
        D$NextSearchPos 0

    On B$WeAreChecking = &TRUE ret

    On D$FL.WeAreUnfolding = &TRUE jmp UnfoldingError

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

;;
  To YeoH: '.zh' should be the extension of your 'RosAsmStrings' File.
  
  I hope you will succeed to make this "...If D$StringsLanguage = '.zh'" work... ;)
  As, off course, as you may guess, i cannot see anything on my Computer...
;;

Proc ErrorMessageBox:

    Arguments @Text1,
              @Text2

        ...If D$StringsLanguage = '.zh'

            ; Unicode:
            Mov esi D@Text1,
                edi Trash1,
                ecx 0

            If esi <> &NULL

                While W$esi > EOS

                    movsw | inc ecx | cmp ecx 400 | ja L1>

                End_While

            End_If

L1:         Mov W$edi EOS

            .If D@Text2 <> &NULL

                Mov esi D@Text2,
                    edi Trash2,
                    ecx 0

                If esi <> &NULL

                    While W$esi > EOS

                        movsw | inc ecx | cmp ecx 400 | ja L1>

                    End_While

                End_If
L1:
                Mov W$edi EOS

            .Else

                Mov W$Trash2 EOS

            .End_If

            ; Tag Dialog 10
            Call 'USER32.DialogBoxIndirectParamW' D$H.Instance,
                                                  ErrorUnicodeDialog,
                                                  &NULL,
                                                  ErrorMessageDialogW,
                                                  &NULL

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
            Call 'USER32.DialogBoxParamA' D$H.Instance, 10, D$H.MainWindow, ErrorMessageDialogA, &NULL

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


; Tag Dialog 10

Proc ErrorMessageDialogW:

    Arguments @hwnd,
              @msg,
              @wParam,
              @lParam

    If D@msg = &WM_INITDIALOG

        Call WM_INITDIALOG_ErrorMessageDialogW

    Else_If D@msg = &WM_CTLCOLOREDIT

        Call WM_CTLCOLOREDIT | ExitP

    Else_If D@msg = &WM_COMMAND

        On W@wParam = HIDE_PUSHBUTTON_OK Call WM_CLOSE_ErrorMessageDialog

    Else_If D@msg = &WM_CLOSE

        Call WM_CLOSE_ErrorMessageDialog

    Else

         Return &FALSE

    End_If

    Mov eax &TRUE

EndP


WM_INITDIALOG_ErrorMessageDialogW:

    Call SetIconDialog

    Call 'USER32.SendDlgItemMessageA' D$HWND,
                                      10,
                                      &WM_SETFONT,
                                      D$H.Font1,
                                      &FALSE

    On D$H.NationalFont <> &NULL Call 'USER32.SendDlgItemMessageW' D$HWND,
                                                                   20,
                                                                   &WM_SETFONT,
                                                                   D$H.NationalFont,
                                                                   &FALSE

    Call 'USER32.SendDlgItemMessageA' D$HWND,
                                      10,
                                      &EM_SETMARGINS,
                                      &EC_LEFTMARGIN+&EC_RIGHTMARGIN,
                                      10

    Call 'USER32.SendDlgItemMessageA' D$HWND,
                                      20,
                                      &EM_SETMARGINS,
                                      &EC_LEFTMARGIN+&EC_RIGHTMARGIN,
                                      10

    Call 'USER32.SendMessageA' D$HWND,
                               &WM_SETTEXT,
                               &NULL,
                               D$ErrorMessageTitlePtr

    Call 'USER32.SendDlgItemMessageA' D$HWND,
                                      10,
                                      &WM_SETTEXT,
                                      0,
                                      Trash1

    On B$trash2 <> EOS Call 'USER32.SendDlgItemMessageW' D$HWND,
                                                         20,
                                                         &WM_SETTEXT,
                                                         0,
                                                         Trash2

ret


Proc ErrorMessageDialogA:

    Arguments @hwnd,
              @msg,
              @wParam,
              @lParam

    If D@msg = &WM_INITDIALOG

        Call WM_INITDIALOG_ErrorMessageDialogA

    Else_If D@msg = &WM_CTLCOLOREDIT

        Call WM_CTLCOLOREDIT | ExitP

    Else_If D@msg = &WM_COMMAND

        On W@wParam = HIDE_PUSHBUTTON_OK Call WM_CLOSE_ErrorMessageDialog

    Else_If D@msg = &WM_CLOSE

        Call WM_CLOSE_ErrorMessageDialog

    Else

        Return &FALSE

    End_If

    Mov eax &TRUE

EndP

WM_INITDIALOG_ErrorMessageDialogA:

        Call SetIconDialog

        Call 'USER32.SendDlgItemMessageA' D$HWND,
                                          10,
                                          &EM_SETMARGINS,
                                          &EC_LEFTMARGIN+&EC_RIGHTMARGIN,
                                          10

        Call 'USER32.SendMessageA' D$HWND,
                                   &WM_SETTEXT,
                                   &NULL,
                                   D$ErrorMessageTitlePtr

        Call 'USER32.SendDlgItemMessageA' D$HWND,
                                          20,
                                          &EM_SETMARGINS,
                                          &EC_LEFTMARGIN+&EC_RIGHTMARGIN,
                                          10


        Call 'USER32.SendDlgItemMessageA' D$HWND,
                                          10,
                                          &WM_SETTEXT,
                                          0,
                                          Trash1

        On D$H.NationalFont <> &NULL Call 'USER32.SendDlgItemMessageA' D$HWND,
                                                                       20,
                                                                       &WM_SETFONT,
                                                                       D$H.NationalFont,
                                                                       &FALSE

        Call 'USER32.SendDlgItemMessageW' D$HWND,
                                          10,
                                          &WM_SETFONT,
                                          D$H.Font1,
                                          &FALSE

        On B$trash2 <> EOS Call 'USER32.SendDlgItemMessageA' D$HWND,
                                                             20,
                                                             &WM_SETTEXT,
                                                             0,
                                                             Trash2

ret

WM_CLOSE_ErrorMessageDialog:

    Call 'USER32.EndDialog' D$HWND,
                            &NULL

ret
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

Proc ViewCookedError:

    Argument @STR.A

    Uses esi,
         edi

        While B$esi > EOI | sub esi (1*ASCII) | End_While | add esi (1*ASCII)

        Mov edi CookedErrorMessage

L1:     lodsb
        ; TODO TABLE !!!
        .If al = TextSign ; 30

            Mov al '"'

        .Else_If al = numSign ; 28
            Mov al '#'
        .Else_If al = CommaSign ; 23
            Mov al ','
        .Else_If al = OpenVirtual ; 22
            Mov al '{'
        .Else_If al = CloseVirtual ; 21
            Mov al '}'
        .Else_If al = Openbracket ; 20
            Mov al '['
        .Else_If al = Closebracket ; 19
            Mov al ']'
        .Else_If al = memMarker ; 15
            Mov al '$'
        .Else_If al = colonSign ; 14
            Mov al ':'
        .Else_If al = openSign ; 13
            Mov al '('
        .Else_If al = closeSign ; 12
            Mov al ')'
        .Else_If al = addSign ; 10
            Mov al '+'
        .Else_If al = subSign ; 9
            Mov al '-'
        .Else_If al = mulSign ; 8
            Mov al '*'
        .Else_If al = divSign ; 7
            Mov al '/'
        .Else_If al = expSign ; 6
            Mov al '^'
        .Else_If al = Space ;  3
            Mov al SPC
        .Else_If al = EOI ;  2
            Mov al '|' | jmp S0>
        .Else_If al = meEOI ; 1
            Mov al '|'
        .End_If

        stosb

        On edi < EndOfCookedErrorMessage jmp L1<<

S0:     Mov B$edi EOS

EndP

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

        dec esi | Mov D$LP.BlockEndText esi  ;, D$CurrentWritingPos esi ; ?Case of Bad Pos?

    Pop eax esi

    Call VerifyNotOneChar
ret


; Because Blocks of one char was not outputed, in older version of the Editor.
; Should be of no more use now:

VerifyNotOneChar:
    Push esi, eax

        Mov eax D$LP.BlockEndText

        cmp eax D$LP.BlockStartText | ja L9>

            add D$LP.BlockEndText 1

            Mov eax D$LP.BlockEndText,
                al B$eax

            On al = CR add D$LP.BlockEndText 1

            Mov esi D$OldCharPos, al B$OldChar, B$esi al

            Mov esi D$LP.BlockEndText | add esi 1

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
            Mov esi D$LP.BlockStartText | dec esi
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

            Mov esi D$LP.BlockStartText,
                edi Trash2,
                ecx D$LP.BlockEndText

            sub ecx D$LP.BlockStartText | inc ecx | rep movsb | Mov al 0 | stosb

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

    Mov D$FL.CompileErrorHappend &TRUE

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

    Mov D$FL.CompileErrorHappend &TRUE

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

    Mov D$FL.CompileErrorHappend &TRUE

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

    Mov D$FL.CompileErrorHappend &TRUE

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

        Mov esi D$LP.BlockEndText | Mov al B$esi+1

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

    Mov D$FL.CompileErrorHappend &TRUE

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
    Mov esi D$LP.BlockStartText | dec esi
    While B$esi = SPC | dec esi | End_While
    Mov eax D$esi-4 | and eax (not 020202020)

  ;  On eax <> 'CLAS', jmp L1<
  ;  On B$esi-5 <> '5', jmp L1<

    sub esi 5 | Mov D$LP.BlockStartText esi

    While B$esi <> ']' | inc esi | End_While

    Mov D$LP.BlockEndText esi

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
