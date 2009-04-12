TITLE Alternates
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
                             Pre-Parser (Alternate syntaxes).
____________________________________________________________________________________________

 None of these routines change the lenght of Data.
 None checks for any Error; Error are holded by downward computations.
;;

AlternatesPreParsers:
    Call BToBS | Call ByteToBS | Call BytePtrToBS | Call ImplicitSize
    Call BracketsToPPBrackets
        Call DbToBS
        Call EqualToEquates | Call EQUToEquates
    Call ReWriteBrackets
ret
____________________________________________________________________________________________


; For Pre-Parser analyzes, we turn '[...]' for Data and Equates into:

[PP_OPEN_DATA 20    PP_CLOSE_DATA 21    PP_OPEN_EQU 22    PP_CLOSE_EQU 23]

BracketsToPPBrackets:
    Mov esi D$CodeSourceA, edx D$StripLen, cl 0 | add edx esi

    .While esi < edx
        lodsb
        .If al = ';'
            While al <> LF
                lodsb | cmp esi edx | ja L9>>
            End_While
        .Else_If al = "'"
            Do
                lodsb | cmp esi edx | ja L9>>
            Loop_Until al = "'"
        .Else_If al = '"'
            Do
                lodsb | cmp esi edx | ja L9>>
            Loop_Until al = '"'
        .Else_If al = '['
            Mov ebx esi
            While B$ebx = ' '
                inc ebx                         ; jump over possible leading spaces
            End_While
            While B$ebx > ' '
                inc ebx                         ; go to end of first word
            End_While
            If B$ebx-1 = ':'
L1:            Mov B$esi-1 PP_OPEN_DATA         ; <<<<<<<<<<<<<<<<<<<<<<
                Mov cl PP_OPEN_DATA

            Else   ; ( B$ebx = ' ' )
                While B$ebx = ' '
                    inc ebx                     ; Verify no [Symbol : (>Data) // CR/| (>Macro)
                End_While
                On B$ebx = ':', jmp L1<
                On B$ebx = CR, jmp L3>
                On B$ebx = '|', jmp L3>
                    Mov B$esi-1 PP_OPEN_EQU     ; <<<<<<<<<<<<<<<<<<<<<<
                    Mov cl PP_OPEN_EQU

          ;  Else_If B$ebx = CR // '|'
L3:            ; case of Macro (not used actually here...).
            End_If

        .Else_If al = ']'
            If cl = PP_OPEN_DATA
                Mov B$esi-1 PP_CLOSE_DATA
            Else_If cl = PP_OPEN_EQU
                Mov B$esi-1 PP_CLOSE_EQU
            End_If
            Mov cl 0
        .End_If

    .End_While
L9: ret


; Turns "DD / DW / DB"   into:   "D$ / W$ / B$":

DbToBS:
    Mov edi D$CodeSourceA, ecx D$StripLen

        Mov al PPOPENDATA, bl PPCLOSEDATA

L3:     repne scasb
        .If ecx > 0
            push eax
                .While B$edi <> bl
                    Mov eax D$edi | or eax 020202020
                    If B$edi = '"'
L1:                     inc edi | dec ecx | jz L7>>
                        cmp B$edi '"' | jne L1<
                    Else_If B$edi = "'"
L1:                     inc edi | dec ecx | jz L7>>
                        cmp B$edi "'" | jne L1<
                    Else_If eax = ' dd '
                        Mov D$edi ' D$ '
                    Else_If eax = ' db '
                        Mov D$edi ' B$ '
                    Else_If eax = ' dw '
                        Mov D$edi ' W$ '
                    Else_If eax = ' dq '
                        Mov D$edi ' Q$ '
                    Else_If eax = ' dr '
                        Mov D$edi ' R$ '
                    Else_If eax = ' df '
                        Mov D$edi ' F$ '
                    Else_If eax = ' dt '
                        Mov D$edi ' T$ '
                    End_If
                    inc edi | dec ecx | jz L7>
                .End_While
L7:         pop eax
        .End_If

        cmp ecx 0 | ja L3<<
ret


; Replaces:  "One EQU 1"   by:   "One     1":

EQUToEquates:
    Mov edi D$CodeSourceA, ecx D$StripLen

        Mov al PPOPENEQU, bl PPCLOSEEQU

L3:     repne scasb
        .If ecx > 0
            push eax
                While B$edi <> bl
                    Mov eax D$edi | or eax 020202020
                    If B$edi = '"'
L1:                     inc edi | dec ecx | jz L7>
                        cmp B$edi '"' | jne L1<
                    Else_If B$edi = "'"
L1:                     inc edi | dec ecx | jz L7>
                        cmp B$edi "'" | jne L1<
                    Else_If eax = ' equ'
                        Mov al B$edi+4 | or al 020
                        On al = ' ', Mov D$edi '    '
                    End_If
                    inc edi | dec ecx | jz L7>
                End_While
L7:         pop eax
        .End_If

        cmp ecx 0 | ja L3<
ret


; Replaces:  "One = 1"   by:   "One   1":

EqualToEquates:
    Mov edi D$CodeSourceA, ecx D$StripLen

        Mov al PPOPENEQU, bl PPCLOSEEQU

L3:     repne scasb
        ...If ecx > 0
            push eax
                While B$edi <> bl
                    ..If B$edi = '"'
L1:                     inc edi | dec ecx | jz L7>
                        cmp B$edi '"' | jne L1<
                    ..Else_If B$edi = "'"
L1:                     inc edi | dec ecx | jz L7>
                        cmp B$edi "'" | jne L1<
                    ..Else_If B$edi = '='
                        .If B$edi-1 = ' '
                            If B$edi+1 = ' '
                                Mov B$edi ' '
                            End_If
                        .End_If
                    ..End_If
                    inc edi | dec ecx | jz L7>
                End_While
L7:         pop eax
        ...End_If

        cmp ecx 0 | ja L3<
ret


; Simple replace of 'Byte Ptr[Value]' by:
;                   'B$       Value '

BytePtrToBS:
    Mov esi D$CodeSourceA | Mov ecx esi | add ecx D$StripLen

    .While esi < ecx
        push esi

        Mov eax D$esi, bl B$esi+4 | or eax 020202020 | or bl 020

        ...If B$esi = '"'
            pop eax
L1:         inc esi | cmp B$esi '"' | jne L1<
            push esi
        ...Else_If B$esi = "'"
            pop eax
L1:         inc esi | cmp B$esi "'" | jne L1<
            push esi
        ...Else_If eax = 'byte'
            Mov dx 'B$', ebx 0 | jmp L1>>
        ...Else_If eax = 'dwor'
            If bl = 'd'
                Mov dx 'D$', ebx 1 | jmp L1>>
            End_If
        ...Else_If eax = 'word'
            Mov dx 'W$', ebx 0 | jmp L1>>
        ...Else_If eax = 'qwor'
            If bl = 'd'
                Mov dx 'Q$', ebx 1 | jmp L1>>
            End_If
        ...Else_If eax = 'real'
            Mov dx 'R$', ebx 0 | jmp L1>>
        ...Else_If eax = 'floa'
            If bl = 't'
                Mov dx 'F$', ebx 1 | jmp L1>>
            End_If
        ...Else_If eax = 'tbyt'
            If bl = 'e'
                Mov dx 'T$', ebx 1 | jmp L1>
            End_If
        ...Else_If eax = 'twor'
            If bl = 'd'
                Mov dx 'T$', ebx 1 | jmp L1>
            End_If
        ...Else_If eax = 'owor'
            If bl = 'd'
                Mov dx 'O$', ebx 1 | jmp L1>
            End_If
        ...Else_If eax = 'xwor'
            If bl = 'd'
                Mov dx 'X$', ebx 1 | jmp L1>
            End_If

L1:         ..If B$esi-1 = ' '
                jmp L1>
            ..Else_If B$esi-1 = ','
                jmp L1>
            ..Else
                jmp L7>>
            ..End_If

L1:         Mov eax esi | add esi 4 | add esi ebx

L2:         ..If B$esi = ' '
                While B$esi = ' '
                    inc esi | On esi > ecx, jmp L7>>
                End_While
                dec esi
                Mov ebx D$esi | or ebx 020202020
                .If ebx = ' ptr'
                    add esi 4
                    While B$esi = ' '
                        inc esi | On esi > ecx, jmp L7>>
                    End_While
                    If B$esi = '['
                        Mov W$eax dx | add eax 2
                        While B$eax <> '['
                            Mov B$eax ' ' | inc eax | On B$eax = 13, jmp L7>
                        End_While
                        Mov B$eax ' '
                        While B$eax <> ']'
                            inc eax | On B$eax = 13, jmp L7>
                        End_While
                        Mov B$eax ' '
                    End_If
                .End_If
            ..End_If
        ...End_If
L7:     pop esi
        inc esi
    .End_While
ret


; Simple replace of 'Byte [Value]' by:
;                   'B$    Value '

ByteToBS:
    Mov esi D$CodeSourceA | Mov ecx esi | add ecx D$StripLen

    .While esi < ecx
        push esi
        Mov eax D$esi, bl B$esi+4 | or eax 020202020 | or bl 020
        ...If B$esi = '"'
            pop eax
L1:         inc esi | cmp B$esi '"' | jne L1<
            push esi
        ...Else_If B$esi = "'"
            pop eax
L1:         inc esi | cmp B$esi "'" | jne L1<
            push esi
        ...Else_If eax = 'byte'
            Mov dx 'B$', ebx 0 | jmp L1>>
        ...Else_If eax = 'dwor'
            If bl = 'd'
                Mov dx 'D$', ebx 1 | jmp L1>>
            End_If
        ...Else_If eax = 'word'
            Mov dx 'W$', ebx 0 | jmp L1>>
        ...Else_If eax = 'qwor'
            If bl = 'd'
                Mov dx 'D$', ebx 1 | jmp L1>>
            End_If
        ...Else_If eax = 'real'
            Mov dx 'R$', ebx 0 | jmp L1>>
        ...Else_If eax = 'floa'
            If bl = 't'
                Mov dx 'F$', ebx 1 | jmp L1>>
            End_If
        ...Else_If eax = 'tbyt'
            If bl = 'e'
                Mov dx 'T$', ebx 1 | jmp L1>
            End_If
        ...Else_If eax = 'twor'
            If bl = 'd'
                Mov dx 'T$', ebx 1 | jmp L1>
            End_If
        ...Else_If eax = 'owor'
            If bl = 'd'
                Mov dx 'O$', ebx 1 | jmp L1>
            End_If
        ...Else_If eax = 'xwor'
            If bl = 'd'
                Mov dx 'X$', ebx 1 | jmp L1>
            End_If

L1:         ..If B$esi-1 = ' '
                jmp L1>
            ..Else_If B$esi-1 = ','

L1:             Mov eax esi | add esi 4 | add esi ebx

                While B$esi = ' '
                    inc esi | On esi > ecx, jmp L7>>
                End_While
                .If B$esi = '['
                    Mov W$eax dx | add eax 2
                    While B$eax <> '['
                        Mov B$eax ' ' | inc eax | On B$eax = 13, jmp L7>
                    End_While
                    Mov B$eax ' '
                    While B$eax <> ']'
                        inc eax | On B$eax = 13, jmp L7>
                    End_While
                    Mov B$eax ' '
                .End_If
            ..End_If
        ...End_If
L7:     pop esi
        inc esi
    .End_While
ret


[ImplicitReg: ? ?  EndOfImplicitReg: ?   ImplicitRegSize: ?]

; Replaces  Mov eax [ebx]  by  Mov eax D$ebx. It never changes the Source size because
; "mov eax [ebx]"  and  "mov eax D$ebx" are same text length.

ImplicitSize:
    Mov esi D$CodeSourceA, edi D$CodeSourceB | Mov ecx esi | add ecx D$StripLen

L0: .While esi < ecx

        ...If B$esi = '"'
L1:         movsb | cmp B$esi '"' | jne L1<
            movsb
        ...Else_If B$esi = "'"
L1:         movsb | cmp B$esi "'" | jne L1<
            movsb
        ...Else_If B$esi = '['
            Mov ebx esi | dec ebx
            ..If B$ebx = ' '
                jmp L1>
            ..Else_If B$ebx = ','

L1:             dec ebx | cmp B$ebx ' ' | je L1<
                          cmp B$ebx ',' | je L1<
                Mov B$OneOperandwBit 0FF
                .If B$ebx > ' '                             ; Mov eax [ebx]
                    dec ebx
                    While B$ebx > ' '
                        dec ebx                             ; Search start of 'eax'
                    End_While
                    On B$ebx <> ' ', jmp L3>
                        inc ebx
                        pushad
                            Mov esi ebx, edi ImplicitReg
                            While B$esi > ' '
                                lodsb | and al 00_1101_1111 | stosb               ; upper case
                                On edi = EndOfImplicitReg, jmp L2>
                            End_While                                             ; wanted for
                            Mov B$edi Space | Mov esi ImplicitReg | Call IsItaReg ; IsItaReg
L2:                     popad

                        cmp B$OneOperandwBit 0FF | jne L4>
                      ; Case of "mov [ebx] eax":
L3:                         While B$ebx <> ']'
                                inc ebx | On B$ebx = CR, jmp L7>>
                            End_While
                            inc ebx | On B$ebx <> ' ', jmp L7>>
                            While B$ebx = ' '
                                inc ebx
                            End_While
                            pushad
                                Mov esi ebx, edi ImplicitReg
                                While B$esi > ' '
                                    lodsb | and al 00_1101_1111 | stosb               ; upper case
                                    On edi = EndOfImplicitReg, jmp L2>
                                End_While                                             ; wanted for
                                Mov B$edi Space | Mov esi ImplicitReg | Call IsItaReg ; IsItaReg
L2:                         popad

                        cmp B$OneOperandwBit 0FF | je L7>
L4:                     cmp B$OneOperandwBit ByteSize | jne L4>
                            Mov ax 'B$' | stosw | jmp L5>
L4:                     cmp B$OneOperandwBit WordSize | jne L4>
                            Mov ax 'W$' | stosw | jmp L5>
L4:                     cmp B$OneOperandwBit DoubleSize | jne L7>
                            Mov ax 'D$' | stosw

L5:                     inc esi
L5:                     cmp B$esi ']' | je L6>
                        cmp B$esi 14 | jb L7>
                            movsb | jmp L5<
L6:                         inc esi | jmp L0<<
L7:                     movsb

                .Else
                    movsb

                .End_If

             ..Else
                  movsb

             ..End_If

         ...Else
            movsb

         ...End_If
     .End_While

    Exchange D$CodeSourceA D$CodeSourceB
ret


; Simple replace of 'B[Value]' by:
;                   'B$Value '
;                   'B [Value]' >>>

BToBS:
    Mov esi D$CodeSourceA | Mov ecx esi | add ecx D$StripLen

    .While esi < ecx
        ...If B$esi = '"'
L1:         inc esi | cmp B$esi '"' | jne L1<
        ...Else_If B$esi = "'"
L1:         inc esi | cmp B$esi "'" | jne L1<
        ...Else_If B$esi = '['
            Mov ebx esi | dec ebx
            While B$ebx = ' '
                dec ebx
            End_While
            ..If B$ebx-1 = ' '
                jmp L1>
            ..Else_If B$ebx-1 = ','
L1:             Mov al B$ebx | or al 020
                .If al = 'd'
                    jmp L2>>
                .Else_If al = 'b'
                    jmp L2>>
                .Else_If al = 'w'
                    jmp L2>>
                .Else_If al = 'b'
                    jmp L2>
                .Else_If al = 'q'
                    jmp L2>
                .Else_If al = 'r'
                    jmp L2>
                .Else_If al = 'f'
                    jmp L2>
                .Else_If al = 't'
                    jmp L2>
                .Else_If al = 'o'
                    jmp L2>
                .Else_If al = 'x'
L2:                 inc ebx
                    If B$ebx = '['
                        Mov B$ebx '$'
                    Else
                        Mov B$ebx '$'
                        While B$ebx <> '['
                            inc ebx
                        End_While
                        Mov B$ebx ' '
                    End_If
                    While B$ebx <> ']'
                        inc ebx | On B$ebx = 13, jmp L7>
                    End_While
                    Mov B$ebx ' '
                .End_If
            ..End_If
        ...End_If
L7:     inc esi
    .End_While
ret


; Last routine:

ReWriteBrackets:
    Mov esi D$CodeSourceA | Mov ecx esi | add ecx D$StripLen
    Mov ebx 0
    .While esi < ecx
        Mov al B$esi
        If al = PP_OPEN_DATA
            Mov B$esi '['
        Else_If al = PP_CLOSE_DATA
            Mov B$esi ']'
        Else_If al = PP_OPEN_EQU
            Mov B$esi '['
        Else_If al = PP_CLOSE_EQU
            Mov B$esi ']'
        End_If
        inc esi
    .End_While
ret
____________________________________________________________________________________________
























