TITLE EquateResolver  ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;;
    Nested Equate Resolver - Ludwig Haehne <wkx@gmx.li>
    
    Substitute nested equates by its values in the equate table to reduce compilation time
    and memory requirements.
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[EquateInflation: B$
"Buffer overrun because of equate inflation detected!

Put expressions inside your equates in brackets to
allow the parser to merge them.

e.g. write [B (A+4)] instead of [B A+4]" EOS]

[BackwardNesting: B$
"Maximum equate nesting level exceeded! 
Declare equates *before* you reuse them in other equates!" EOS]

[CyclicDependancy: B$ "Cyclic dependency detected!" EOS]

[CompilationStopped: B$ 'Compilation stopped' EOS]
____________________________________________________________________________________________

; This proc resolves nested equates. It scans all equates texts for other nested equates
; and unfolds these into a new buffer. It uses a mixture of iteration and "fake recursion".
; It makes a linear scan of the equate list and replaces nested equates with its value. If
; a not yet scanned equate is found (e.g. [B A+1, A 1]), it saves the current scan position
; on the stack and proceeds with the nested equate.
; To reduce size and compilation time expressions (which contain '(..)') are merged finally.

; Possible Optimizations:
; * merge expressions without brackets e.g. EBP+4+4+...+4 -> EBP+020
; * faster execution path for plain number equates (no nesting, no operators)

; Buffer where the substituted equate contents are stored
[EquateSubstitutes: D$ ?
 EquateSubstitutesPtr: D$ ?
 EquateSubstitutesLimit: D$ ?]
; Remember in case of relocation
[LastEquateList: D$ ?
 LastEquateListPtr: D$ ?]

; The proc works in this way:
;
; For each equate e in E
;   For each token t in e
;     If IsEquate(t)
;       If IsResolved(t)
;         Substitute t
;       Else
;         Resolve(t)
;       End
;     End
;   Next
;   Merge(e)
; Next

ResolveEquates:

    [@ProcessedLimit:   D$ ? ; address of the last resolved equate (in the iteration !)
     @IsNested:         B$ ? ; equate contains nested equates and/or must be restored
     @NumBrackets:      B$ ? ; number of bracket levels in expression
     @UnpairedBrackets: B$ ? ; non-zero if brackets are unpaired
     @UnknownSymbols:   B$ ? ; number of unresolved symbols (regs, macro emitted equates)
     @ErrorText:        D$ ?]; points at the appropriate error text in case of resolving problems

    ;Call 'KERNEL32.OutputDebugStringA' {B$ 'ResolveEquates' EOS}

  ; Set exception handler
    Push @AbortCompilation
    Push @AVHandler
    Push D$fs:0
    Mov D$fs:0 esp

      ; If table was relocated through memory extension the pointers must be altered
        Mov eax D$LastEquateList, edx D$EquateList
        If eax <> edx
            sub D$LastEquateListPtr eax
            add D$LastEquateListPtr edx
            Mov D$LastEquateList edx
        End_If

        Mov esi D$LastEquateListPtr, edi D$EquateSubstitutesPtr

      ; Empty list marker
        Push 0
____________________________________________________________________________________________

@GetNextEquate: ; Search for next equate to process

      ; Continue with skipped equate (if any)
        On D$esp <> 0, Pop esi

      ; Already dealt with? > Skip
        While B$esi-1 = 04
            Mov B$esi-1 EOI
L0:         inc esi
            cmp B$esi LowSigns | ja L0<
            add esi 11
        EndWhile

        Mov D@ProcessedLimit esi

      ; Check for end of list (when we finish the todo-stack has to be empty)
        If D$esi = 02020202
            add esp 4 ; clear list marker
            Pop D$fs:0 | add esp 8; Clear exception handler

            Mov D$EquateSubstitutesPtr edi
            Move D$LastEquateListPtr D$EquateListPtr
            ret
        End_If
____________________________________________________________________________________________

@EvaluateEquate: ; We start here when a not-yet-processed equate was found

      ; Skip name.
        Mov ebx esi
        SkipName ebx

      ; ebx, esi > Address (D), Size (D), Flag (B)
      ; Scan the equate tokens, copy and unfold but only redirect the pointer
      ; when there were really nested equates.
        Push esi

            Mov D@IsNested 0

          ; esi > equate content,  ecx = size,  edx > substitute
            Mov esi D$ebx
            Mov ecx D$ebx+4
            Mov edx edi

          ; We MUST copy the expression or GetFromQwordCheckSum might fail
          ; when there's no separator between the equate strings.
          ; (e.g. "...SummerWinter...") The expression buffer is abused for this.
            If B$esi+ecx >= SPC
                Push ecx
                    Mov edi D$ExpressionA
                    rep movsb
                    Mov B$edi 0
                    Mov esi D$ExpressionA
                Pop ecx
                Mov edi edx
            End_If

          ; Copy contents and replace nested equates
            .While ecx > 0

L7:             While B$esi <= LowSigns
                    movsb
                    dec ecx | jz L5>>
                    cmp B$esi-1 TextSign | je L4>
                EndWhile

                Mov al B$esi

                cmp al '0' | jb L3> ; ?
                cmp al '9' | jbe L4>

L3:           ; The equate string contains a text token. Check if it is an equate
                Call GetFromQwordCheckSum esi, D$EquateList, D$EquateListLimit
                .If eax <> 0

                  ; Was equate already processed? Either it is among the equates resolved
                  ; by linear table processing (indicated by ProcessedLimit) or was tagged
                  ; as being (forward-)resolved with 04.
                    ..If eax >= D@ProcessedLimit
                        If B$eax-1 <> 04
                            Mov esi eax
                            Mov edi edx
                            Call @CheckCyclicDependency
                            jmp @EvaluateEquate ; leave esi on stack so we can later continue with this
                        End_If
                    ..End_If

                    Mov B@IsNested 1

                  ; Resolved equates contents can be copied.
                    SkipName eax
                    Push esi ecx
                        Mov esi D$eax
                        Mov ecx D$eax+4
                        rep movsb
                    Pop ecx esi

                  ; Skip name in the source buffer
                    While B$esi > LowSigns
                        inc esi
                        dec ecx | jz L5>
                    EndWhile
                .End_If

              ; Copy rest of the token
L4:             While B$esi > LowSigns
                    movsb
                    dec ecx | jz L5>
                EndWhile

            .EndWhile


          ; Finished unfold

L5:       ; Rescan equate contents, count brackets and unresolved symbols.
          ; This is done here to keep the unfold loop simple.
            Mov eax edx
            Push edx
                Mov eax 0
                While edx < edi
                    Mov al B$edx
                    If al = openSign
                        inc B@NumBrackets
                        inc B@UnpairedBrackets
                    Else_If al = closeSign
                        dec B@UnpairedBrackets
                    Else_If al > '9'
                        On ah < LowSigns,
                            inc B@UnknownSymbols
                    End_If
                    Mov ah al
                    inc edx
                EndWhile
            Pop edx

          ; Merge expressions if brackets were found and it is a valid statement
            cmp B@NumBrackets      0 | je L6>
            cmp B@UnpairedBrackets 0 | jne L6>
            cmp B@UnknownSymbols   0 | jne L6>

            Push ebx
                movzx ebx B@NumBrackets ; nr of bracket pairs
                ;Call OutputExp
                    Call @MergeEquate
                ;Call OutputExp
            Pop ebx
            Mov B@IsNested 1

L6:     Pop esi

      ; Mark list entry as forward-resolved so it can be skipped later in the linear
      ; processing of the equatelist (the temporary 04 tag is removed then).
        If D$esp <> 0
            Mov B$esi-1 04
        End_If

      ; If the equate contents have been altered (nested substituted, expression merged)
      ; redirect the content pointer, otherwise ignore the copied contents.
        If B@IsNested = 1
            Mov D$ebx edx ; redirect
            Mov ecx edi
            sub ecx edx
            Mov D$ebx+4 ecx
        Else
            Mov edi edx ; throw away copied substitute
        End_If

        Mov esi ebx
        add esi 10

    jmp @GetNextEquate
____________________________________________________________________________________________

@AbortCompilation: ; Declaration error, reset old SEH & abort
  ; Reset exception handler
    Mov eax D$fs:0
    Mov eax D$eax
    Mov D$fs:0 eax
  ; Abort
    Mov eax D@ErrorText
    Mov B$ErrorLevel 13 ; TODO assign meaningful error level
    jmp OutOnError
____________________________________________________________________________________________

; The exception handler deals with buffer overruns due to equate inflations which are most
; likely programming errors. Therefore the compilation is stopped.
; Another possible exception is a stack overflow when an outragous amount of nested equates
; must be resolved.

Proc @AVHandler:
    Arguments @ExceptionRecord, @Error, @ThreadContext

        Mov ecx D@ExceptionRecord     ; exception record
        test D$ecx+4 1 NOT_ZERO L8>      ; non-continueable exception

      ; Check which type of exception occurred:
      ; We catch access violations that tried to read/write beyond the limit of the
      ; equate substitutes buffer. All other AVs are not dealt with.
        If D$ecx = &EXCEPTION_ACCESS_VIOLATION
            Mov eax D$ecx+24
            Mov edx D$EquateSubstitutesLimit
            sub eax edx
            cmp eax 01000 | ja L8>
            Mov D@ErrorText EquateInflation
            ;Call 'USER32.MessageBoxA' D$H.MainWindow, EquateInflation, CompilationStopped, &MB_ICONEXCLAMATION

      ; Deal with stack overflows (I've never seen this happening here)
        Else_If D$ecx = &EXCEPTION_STACK_OVERFLOW
            Mov D@ErrorText BackwardNesting
            ;Call 'USER32.MessageBoxA' D$H.MainWindow, BackwardNesting, CompilationStopped, &MB_ICONEXCLAMATION

      ; Unhandled exceptions are forwarded to the OS
        Else
L8:         Mov eax 1 | jmp L9>
        End_If

      ; Set the instruction pointer to continue with the cleanup routine
        Mov ecx D@ThreadContext ; get context record in ecx
        Mov edx D@Error         ; get pointer to ERR structure
        Mov eax D$edx+8         ; get safe place given in ERR structure
        Mov D$ecx+0B8 eax       ; replace instruction pointer

        Mov eax 0

L9: Mov esp ebp
    Pop ebp
ret
____________________________________________________________________________________________

; Check if the referenced equate corresponds to an already listed equate in the
; nesting hierarchy.
; Return address is at [esp], therefore we scan from [esp+4] -> [esp+x]=0

@CheckCyclicDependency:
    Mov eax 1
    While D$esp+eax*4 <> 0
        cmp esi D$esp+eax*4 | je L0>
        inc eax
    EndWhile
ret
  ; highlight first equate in the cycle
L0: Mov edx D$esp+eax*4, edi edx
    Mov ecx 0-1, al EOI
    repne scasb
    Mov ebx 0-2 | sub ebx ecx
    Call InternSearch
    Mov D@ErrorText CyclicDependancy

jmp @AbortCompilation
____________________________________________________________________________________________

; Solve expressions inside an equate and replace with the compressed string.
; All registers must be preserved for the calling routine.
;   edx -> Start of expression
;   edi -> End of expression
;   ebx -> Number of bracket pairs '(...)'
;   (1+((2 shl 3)*0010011))
;   ^                      ^
;  edx     ebx=3          edi

@MergeEquate:
    Push eax ecx esi
        Push edx
            inc edx
            Mov D$StartOfSourceExpression edx
            Mov D$StartOfDestinationExpression edi
            Mov esi edi
            Call ComputeExpression
            Mov esi D$ExpressionA
            Mov ecx edi
            sub ecx esi
            dec ecx
        Pop edx

        Mov edi edx
        rep movsb

    Pop esi ecx eax
ret
____________________________________________________________________________________________

[SkipName
    S0: inc #1
        cmp B$#1 LowSigns | ja S0<
        inc #1]
____________________________________________________________________________________________

[DebugStr: B$ ? # &MAX_PATH]

OutputExp:

    pushad

        Mov ecx edi

        sub ecx edx

        Mov esi edx

        Mov edi DebugStr

        While ecx > 0

            lodsb

            If al = openSign

                Mov al '('

            Else_If al = closeSign

                Mov al ')'

            Else_If al = addSign

                Mov al '+'

            Else_If al = subSign

                Mov al '-'

            Else_If al = divSign

                Mov al '/'

            Else_If al = mulSign

                Mov al '*'

            Else_If al = Space

                Mov al SPC

            End_If

            stosb

            dec ecx

        EndWhile

        Mov B$edi EOS

        Call 'KERNEL32.OutputDebugStringA' DebugStr

    popad

ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
