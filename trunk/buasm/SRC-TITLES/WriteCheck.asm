TITLE WriteCheck      ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;;
  Write-Time syntax error checker. First Try&See implementation:
  
  Checks for Mnemonics and Macros evocations in Code.
;;


[WriteCheckPointer: D$ ?
 WriteChar: D$ ?
 WriteCheckerThreadID: D$ ?
 WeAreChecking: D$ ?]

[WriteCheckerWanted: D$ ?
 WriteCheckerRuning: D$ ?]

Proc WriteChecker:
    Argument @Pointer, @Char

        On B$WriteCheckerRuning =  &TRUE, ExitP

      ; BackSpace:
        On eax = 8, ExitP

        Mov B$WriteCheckerRuning &TRUE

        pushad

            Mov eax D@Pointer, D$WriteChar 0

            If B$eax-1 <= SPC
                Mov B$WriteCheckerRuning &FALSE

            Else_If B$eax-1 <> ':'
                Mov D$WriteCheckPointer eax | Move D$WriteChar D@Char

                Call 'KERNEL32.CreateThread' &NULL, 0, WriteCheckerThread, 0,
                                             &THREAD_PRIORITY_IDLE, ;THREAD_PRIORITY_NORMAL,
                                             WriteCheckerThreadID
            Else
                Mov B$WriteCheckerRuning &FALSE

            End_If

        popad
EndP
____________________________________________________________________________________________


Proc WriteCheckerThread: ; 'CharMessage'
    Local @Exit @ContinueStatus @hThread

    pushad

      ; The Pointer is given when user hits CRLF, Space or Comma:
        Mov esi D$WriteCheckPointer | dec esi
        Mov D$BlockEndTextPtr esi

      ; We go to start of the word:
L0:     dec esi | cmp B$esi ',' | je L1>
                  cmp B$esi '|' | je L1>
                  cmp B$esi ':' | je L1>
                  cmp B$esi SPC | ja L0<

      ; Keep track of component case, in eax (Either Instruction or Parameter):
L1:     Mov ebx esi | While B$ebx = SPC | dec ebx | End_While
        If B$ebx = '|'
            Mov eax 1
        Else_If B$ebx < SPC
          ; "1" >>> First Component (Mnemonic or Macro)
            Mov eax 1
        Else
          ; "2" >>> Parameter
            Mov eax 2
        End_If

        inc esi | Mov D$BlockStartTextPtr esi

      ; Must be a Statement > Color = Statememts Color (1) ?
        Mov ebx esi | sub ebx D$CodeSource | add ebx D$ColorsMap

      ; If Code Color Statement:
        ...If B$ebx = 1
            ..If D$WriteChar = ':'
                Call CheckUniqueSymbol

          ; If Instruction:
            ..Else_If eax = 1
                Mov ecx D$BlockEndTextPtr | sub ecx D$BlockStartTextPtr
                Mov eax &FALSE
                If ecx < 15
                    Call CheckMnemonic
                End_If

                If eax = &FALSE
                    Call CheckForMacro
                End_If

                .If eax = &FALSE
                    Mov D$FL.BlockInside &TRUE | Call AskForRedrawNow | Call Beep
                .End_If

            ..End_If

      ; If Data Color Statement:
        ...Else_If B$ebx = 2
            ..If D$WriteChar = ':'
                Call CheckUniqueSymbol

            ..Else

            ..End_If

        ...End_If

    popad

    Mov B$WriteCheckerRuning &FALSE

    Call 'KERNEL32.ExitThread' 0
EndP
____________________________________________________________________________________________

[TrashCode: D$ ? # 8]

CheckMnemonic:
    Mov B$WeAreChecking &TRUE

    Mov edi MnemonicCopy, esi D$BlockStartTextPtr, D$LineStart esi
    Mov ecx D$BlockEndTextPtr | sub ecx esi | inc ecx
L0: lodsb | and eax (not 32) | stosb | loop L0<
    Mov B$edi 0

    Mov edi TrashCode | Call Encode

    Mov B$WeAreChecking &FALSE


    .If B$CompileErrorHappend = &TRUE
        If eax = NotAnOpcode
            Mov eax &FALSE
        Else
            Mov eax &TRUE
        End_If

    .Else
        Mov eax &TRUE

    .End_If
ret
____________________________________________________________________________________________

; User entered a Colon Char:

CheckUniqueSymbol:
  ; Compute the lenght of the actualy edited Label into ebx:
    Mov esi D$STRUCT.EditData@CurrentWritingPos, ebx 0 | dec esi

  ; Do not check again Exported Labels:
    cmp B$esi-1 ':' | je L9>>

L0: dec esi | inc ebx
    cmp B$esi SPC | jbe L1>
    cmp B$esi '[' | jne L0<

  ; Do not consider Local Labels:
    cmp ebx 3 | jb L9>>

L1: std
        Mov esi D$CodeSource, edx D$STRUCT.EditData@SourceEnd

        While esi < edx
            inc esi
            If B$esi = ':'
                Push esi
                    Mov edi D$STRUCT.EditData@CurrentWritingPos, ecx ebx
                    dec edi | cmp esi edi | je L1>

                    repe cmpsb | jne L1>
                    cmp B$esi SPC | jbe L0>
                    cmp B$esi '[' | jne L1>

L0:                     cld
                        Pop esi
                        inc edi | Mov D$BlockStartTextPtr edi
                        Mov eax D$STRUCT.EditData@CurrentWritingPos | dec eax | Mov D$BlockEndTextPtr eax

                        Call Beep | ret

L1:             Pop esi
            End_If

        End_While
L8: cld

L9: ret
____________________________________________________________________________________________

CheckForMacro:
    Call IsItaMacro

    .If eax = 0
        If D$TABLE.Titles+DWORD > 0
            Push D$CodeSource, D$STRUCT.EditData@SourceEnd, D$SourceLen
                Move D$CodeSource D$RealCodeSource
                Move D$STRUCT.EditData@SourceEnd D$RealSourceEnd
                Move D$SourceLen D$RealSourceLen
                Call IsItaMacro
            Pop D$SourceLen, D$STRUCT.EditData@SourceEnd, D$CodeSource
        End_If
    .End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
