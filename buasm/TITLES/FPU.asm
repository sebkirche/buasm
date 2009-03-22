TITLE FPU             ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;;
  'UsedByTheAssembler' (very old traditional Routines for Ascii to Float on Stack)
  
  'UsedBytheEqualParser' 
  
  'UsedByTheDebugger' (and by the Disassembler for 'FloatToUString')
  
  'UsedForStudy' (Under developements)
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

@UsedByTheAssembler:

[Int10: 10   CurrentConvertedDigit: 0   NumberOfDecimalPlaces: 0   FaultCharAndPos: 0]

atof:
    finit | Mov D$FaultCharAndPos 0
    Push edi, esi
      Call convert                  ; convert mantissa (returns with 'E' if exponent present)

      neg edx                                ; save -1 * decimal places
      Mov D$NumberOfDecimalPlaces edx

      IF al <> 'E'
        Mov B$FaultCharAndPos al             ; faulty char present in mantissa
        Pop ecx | Mov edi esi | sub edi ecx  ; fault char pos
        fldz                                 ; assume zero exponent
      Else_If al = 'E'
        Call convert                         ; convert exponent
        Mov B$FaultCharAndPos al      ; Save faulty character present in exponent
        Pop ecx | Mov edi esi | sub edi ecx  ; fault char pos
      End_If

      fiadd D$NumberOfDecimalPlaces          ; adjust exponent for dec. places in mantissa
      Call falog                             ; raise 10 to power
      fmul                                   ; exponent * mantissa

      ; Provide information about faulty character and its position

      Mov eax edi                            ; Get possible fault position
      shl eax 8                              ; shift to high word
      or eax D$FaultCharAndPos               ; OR faults togeter
    Pop edi
ret                                      ; return ST(0) = result

_________________________________________________________________
;;
 CONVERT:      Called by ATOF to convert ASCII number with possible sign and/or decimal point
 Call with:    ESI = address of string
 Returns:      ST(0) = result
               AL    = first unconvertable character
               EDX    = number of digits after decimal point
               ESI = address+1 of character in AL
 Uses:         AH, CX
;;
_________________________________________________________________        ;

convert:                                ; convert numeric field
    fldz | Mov ecx 0, edx 0-1           ; initialize result, sign, decimal count

   lodsb
        cmp al AddSign | je L2>         ; if + sign proceed
        cmp al SubSign | jne L3>        ; is it - sign? no > test if numeric
          dec ecx                       ; yes, set sign flag

L2: lodsb                               ; get next character

L3: cmp al '0' | jb L4>                 ; is character valid?
      cmp al,'9' | ja L4>
        and eax 0f                      ; isolate lower four bits
        Mov D$CurrentConvertedDigit eax ; and save digit value
        fimul D$int10                   ; previous value * 10
        fiadd D$CurrentConvertedDigit   ; accumulate new digit

        or edx edx | js L2<             ; past decimal point? no > convert next digit
          inc edx | jmp L2<             ; yes > count digits / convert next digit

L4: cmp al '.' | jne L5>                ; no point > proceed
      inc edx | jmp L2<                 ; indicate decimal found > convert more digits
L5:    jcxz L6>                         ; jump if result pos.
        fchs                            ; make result negative

L6: or edx edx | jns L7>                ; decimal point found? yes > jump
      Mov edx 0                         ; no, return zero places

L7: ret                                 ; return ST(0) = result


_____________________________________________________________________________________
;;
-------------------------------------------------------;
 Calculate Common Antilog on 80x87   ;
       ;
 Call  :    st(0)     = logarithm (base 10)  ;
       ;
 Return:    st(0)     = antilog to base 10  ;
       ;
 Coprocessor should be initialised before Call  ;
-------------------------------------------------------;
;;
; FWAIT doesn't seam to be of any use (works the same without on my computer).

falog:
        fldl2t                          ; st= log2(10), st(1)=argument
        fmulp   st1 st0                 ; st= log2(10) * argument
        fld     st0                     ; take copy
     ;   fwait
     ;   fstcw   W$Falogoldcw            ; store old control word
     ;   fwait                           ; wait 'till it arrives
     ;   Mov     ax W$Falogoldcw         ; Load control word
     ;   and     ax 0f3ff                ; Field to "round down"
     ;   or      ax 0400                 ; Set precision to 53 bit mantissa
     ;   Mov     W$Falognewcw ax         ; Got new control word
     ;   fldcw   W$Falognewcw            ; Force rounding mode
        frndint                         ; Round from real to integer
     ;   fldcw   W$Falogoldcw            ; Restore old rounding mode
        fld     st0                     ; take copy
        fxch    st2                     ; Get original product
        fsubrp  st1 st0                 ; Get fractional part
        fld1
        fchs
        fxch    st1                     ; Scale fractional part
        fscale    ;
        fstp    st1                     ; Discard coprocessor junk
        f2xm1                           ; Raise 2 to power-1
        fld1                            ; Push 1 onto stack
        faddp   st1 st0                 ; Correct for the -1
        fmul    st0 st0                 ; Square result
        fscale                          ; Scale by integer part
        fstp    st1                     ; Return with result in st(0)
      ret

____________________________________________________________________________________________
____________________________________________________________________________________________

@UsedBytheEqualParser:


____________________________________________________________________________________________
Proc AsciitoFloat:
;;
  This procedure was written by Raymond Filiatreault, December 2002
  Modified Betov, December 2002
  (Slightly modified by Scarmatil to fit upon RosAsm's signs, December 2003)
  
  This AsciitoFloat function converts a decimal number from a zero terminated
  alphanumeric string format (Src) to an 80-bit REAL number and returns
  the result as an 80-bit REAL number at the specified destination (either
  the FPU top or a memory location), unless an invalid operation is
  reported by the FPU.
 
  The source can be a string in regular numeric format or in scientific
  notation. The number of digits (including leading 0's) must not exceed
  18. If in scientific format, the exponent must be within +/-4931
 
  The source is checked for validity. The procedure returns an error if
  a character other than those acceptable is detected prior to the
  terminating zero or the above limits are exceeded.
 
  This procedure is based on converting the digits into a specific packed
  decimal format which can be used by the FPU and then adjusted for an
  exponent of 10.
 
  Only EAX is used to return error or success. All other registers are
  preserved.
____________________________________________________________________________________________

  Calling:     > Call AsciitoFloat Source, Destination
  
  Source: Pointer to a Floating Point String (either regular or scientific 
          notation).
  
  Destination: Either a Pointer to a [T$FPValue: ...] or &NULL.

  In case of &NULL Destination, the result is left on the FPU Stack, and
  you have to you have to Pop the result by yourself. Usefull in cases when
  you want Real8 or Real4, or when you want to go on computing with the result.
____________________________________________________________________________________________
;;
    Arguments @lpSrc, @lpDest
    Local @stword, @ten
    Structure @BCD 12, @bcdstr 0
    Uses ebx, ecx, edx, esi, edi

        Mov eax 0, ebx 0, edx 0, ecx 19, D@ten 10
        lea edi D@bcdstr | Mov D$edi 0, D$edi+4 0, D$edi+8 0 | add edi 8

        Mov esi D@lpSrc
        Mov al B$esi
        If al = Space       ; string empty?
            jmp E7>>
        Else_If al = minusSign
            Mov B$edi+1 080
            inc esi
        End_If

        ; Strip pointless 0
        While B$esi = '0'
            If B$esi+1 = pointSign | inc esi | jmp L2> | End_If
            On B$esi+1 < '0', jmp L2>
            On B$esi+1 > '9', jmp L2>
            inc esi
        End_While

      ; Convert the digits to packed decimal:
L2:     lodsb | On al = 'e', Mov al 'E'

      ; bh used to set the decimal point flag (one point only):
        ...If al = pointSign
            If bh = 0
                or bh 1 | jmp L2<
            End_If
        ...Else_If al = 'E'
            On cl < 19, jmp L6>>        ;error if no digit before E
        ...Else_If al = Space
            If cl < 19                  ;error if no digit before terminating Space
                xor al al | rol al 4 | ror ax 4 | Mov B$edi al | jmp L5>>
            End_If
        ...Else
            ..If al >= '0'
                .If al <= '9'           ;error if bad Char.
                    dec ecx
                    If ecx > 0          ;error if more than 18 digits in number
                        sub al '0' | On bh = 0, inc bl
                        test ah 040 ZERO L1>
                            rol al 4 | ror ax 4 | Mov B$edi al | dec edi | xor eax eax  | jmp L2<<
L1:                     Mov ah al | or ah 040 | jmp L2<<
                    End_If
                .End_If
            ..End_If
        ...End_If

        jmp E7>>                        ; Error case if falling here.

      ; Output:
L5:     fbld T@bcdstr
        Mov eax 18 | sub al bl | sub edx eax | Call XexpY edx
        fmulp ST1 ST0
        fstsw W@stword                      ;retrieve exception flags from FPU
        wait | test W@stword 1 NOT_ZERO E7>>   ;test for invalid operation
        Mov eax D@lpDest
        If D@lpDest <> &NULL
            Mov eax D@lpDest |  fstp T$eax      ;store result at specified address
        End_If
        jmp E8>>

      ; Scientific notation (exponent in edx):
L6:     movzx eax B$esi | inc esi
        cmp al plusSign | je L0>
            cmp al minusSign | jne L6>
            stc | rcr eax 1         ;keep sign of exponent in most significant bit of EAX
L0:     lodsb                               ;get next digit after sign

L6:     Push eax |
            and eax 0FF | jnz L0>           ;continue if 1st byte of exponent is not terminating 0
L6:             Pop eax | jmp E7>>          ;no exponent
L0:         sub al '0' | jc L6<             ;unacceptable character
            cmp al 9 | ja L6<               ;unacceptable character
            Push eax
                Mov eax edx | mul D@ten | Mov edx eax
            Pop eax
            add edx eax | cmp edx 4931 | ja L6<     ;exponent too large
            lodsb
            cmp al Space | jne L0<
        Pop eax                             ;retrieve exponent sign flag
        rcl eax 1 | jnc L0>                 ;is most significant bit set?
            neg edx
L0:     jmp L5<<

E7:     Mov eax &FALSE | finit | jmp E9>
E8:     Mov eax &TRUE
E9: EndP


;put 10 to the proper exponent (value in EDX) on the FPU

Proc XexpY:
    Argument @Tempdw

        fild D@tempdw           ;load the exponent
        fldl2t                  ;load log2(10)
        fmulp ST1 ST0           ;->log2(10)*exponent

;at this point, only the log base 2 of the 10^exponent is on the FPU
;the FPU can compute the antilog only with the mantissa
;the characteristic of the logarithm must thus be removed

        fld ST0                 ;copy the logarithm
        frndint                 ;keep only the characteristic
        fsub ST1 ST0            ;keeps only the mantissa
        fxch                    ;get the mantissa on top

        f2xm1                   ;->2^(mantissa)-1
        fld1
        faddp ST1 ST0           ;add 1 back

;the number must now be readjusted for the characteristic of the logarithm

        fscale                  ;scale it with the characteristic

;the characteristic is still on the FPU and must be removed

        fxch                    ;bring it back on top
        fstp ST0                ;clean-up the register
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

@UsedByTheDebugger:

; From FPU to Ascii
;
; Procedures Originaly written by Tim Roberts.

[TempoAsciiFpu: D$ ? # 5] [BCDtempo: T$ ?]

[ten: R$ 10.0    ten7: 1.0e6
 ten_1: T$ 1.0e1  ,    1.0e2,    1.0e3,    1.0e4,    1.0e5,    1.0e6,    1.0e7,   1.0e8
           1.0e9,    1.0e10,   1.0e11,   1.0e12,   1.0e13,   1.0e14,   1.0e15
 ten_16:   1.0e16,   1.0e32,   1.0e48,   1.0e64,   1.0e80,   1.0e96,   1.0e112, 1.0e128
           1.0e144,  1.0e160,  1.0e176,  1.0e192,  1.0e208,  1.0e224,  1.0e240
 ten_256:  1.0e256,  1.0e512,  1.0e768,  1.0e1024, 1.0e1280, 1.0e1536, 1.0e1792
           1.0e2048, 1.0e2304, 1.0e2560, 1.0e2816, 1.0e3072, 1.0e3328, 1.0e3584, 1.0e3840
           1.0e4096, 1.0e4352, 1.0e4608, 1.0e4864]

Proc PowerOf10:
    Mov ecx, eax
    test eax 0_8000_0000 ZERO L1>
        neg eax

L1: fld1

    Mov dl al | and edx 0f
    If edx > 0
        lea edx D$edx+edx*4 | fld T$ten_1+edx*2-10 | fmulp st1 st0
    End_If

    Mov dl al | shr dl 4 | and edx 0F
    If edx > 0
        lea edx D$edx+edx*4 | fld T$ten_16+edx*2-10 | fmulp st1 st0
    End_If

    Mov dl ah | and edx 01F
    If edx > 0
        lea edx D$edx+edx*4 | fld T$ten_256+edx*2-10 | fmulp st1 st0
    End_If

    test ecx 0_8000_0000 ZERO L1>
        fdivp st1 st0 | ExitP
L1:     fmulp st1 st0
EndP

Proc FloatToBCD:
    Uses esi, edi, ecx

        fbstp T$BCDtempo

        lea esi D$BCDtempo+8 | Mov edi TempoAsciiFpu
        Mov ecx, 9

L0:     Mov al B$esi | dec esi | rol ax 12 | rol ah 4
        and ax 0f0f | add ax 03030 | stosw | loop L0<
EndP

[NegatedReg: D$ ?]

Proc FloatToUString:
    Arguments @Float80Pointer, @DestinationPointer
    Local @iExp, @ControlWord, @MyControlWord
    Uses esi, edi, edx, ecx

        Mov edi D@DestinationPointer, eax D@Float80Pointer

        ..If D$eax = 0
            .If D$eax+4 = 0
                If W$eax+8 = 0
                    Mov B$edi '0', B$edi+1 0

EndP

                End_If
            .End_If
        ..End_If

        Mov B$NegatedReg &FALSE
        Test B$eax+9 0_80 ZERO L1>
          xor D$eax+9 0_80 | Mov B$NegatedReg &TRUE | Mov B$edi '-' | inc edi

L1:     ;  _______________________________________________________
        ; |        |                 |             |              |
        ; | Bit 79 | Bit 78 ... 64   | Bit 63      | Bit 62 ... 0 |
        ; | Sign   | Biased Exponent | Integer Bit | Fraction     |
        ; |________|_________________|_____________|______________|
        ;
        ; SNaN  : S=0  E=7FFF  I=1  F=1..3FFF_FFFF_FFFF_FFFF
        ; QNaN  : S=0  E=7FFF  I=1  F=4000_0000_0000_0000..7FFF_FFFF_FFFF_FFFF
        ; INF   : S=0  E=7FFF  I=1  F=0
        ; -SNaN : S=1  E=7FFF  I=1  F=1..3FFF_FFFF_FFFF_FFFF
        ; -QNaN : S=1  E=7FFF  I=1  F=4000_0000_0000_0000..7FFF_FFFF_FFFF_FFFF
        ; -INF  : S=1  E=7FFF  I=1  F=0

        ; Add: Tiny : S=x  E=0     I=0  F<>0


        movzx edx W$eax+8 | and edx 07FFF ; edx = E

        If edx = 07FFF
            On B$NegatedReg = &TRUE, xor B$eax+9 0_80
            ; test lower 32 bits of fraction
            test D$eax 0 NOT_ZERO L1>
            ; test upper 31 bits of fraction
            Mov edx D$eax+4 | and edx 0_7FFF_FFFF | jnz L1>
            Mov D$edi 'INF'

EndP

L1:         ; test most significant fraction bit
            test B$eax+7 040 ZERO L1>
            Mov D$edi 'QNaN', B$edi+4 0

EndP

L1:         Mov D$edi 'SNaN', B$edi+4 0

EndP

        End_If

        fclex | fstcw W@ControlWord | Mov W@MyControlWord 027F | fldcw W@MyControlWord

        fld T$eax | fld st0

        fxtract | fstp st0 | fldlg2 | fmulp st1 st0 | fistp D@iExp

        .If D@iExp L 16
            fld st0 | frndint | fcomp st1 | fstsw ax
            Test ax 040 ZERO L1>
                Call FloatToBCD

                Mov eax 17 | Mov ecx D@iExp | sub eax ecx | inc ecx
                lea esi D$TempoAsciiFpu+eax

                If B$esi = '0'
                    inc esi | dec ecx
                End_If

                Mov eax 0
                rep movsb | jmp L9>>

        .End_If

L1:     Mov eax, 6 | sub eax D@iExp

        Call PowerOf10

        fcom Q$ten7 | fstsw ax | Test ah 1 ZERO L1>
            fmul Q$ten | dec D@iExp

L1:     Call FloatToBCD

        lea esi D$TempoAsciiFpu+11 | Mov ecx D@iExp

        If ecx = 0-1
            Mov B$edi '0' | inc edi
        End_If

        inc ecx

        If ecx <= 7
            Mov eax 0
            rep movsb | Mov B$edi '.' | inc edi
            Mov ecx 6 | sub ecx D@iExp | rep movsb

            While B$edi-1 = '0' | dec edi | End_While
            On B$edi-1 = '.', dec edi

            jmp L9>>
        Else
            movsb | Mov B$edi '.' | inc edi | movsd | movsw

            Mov B$edi 'e' | Mov eax D@iExp
            Mov B$edi+1 '+'
            Test eax 0_8000_0000 ZERO L1>
                neg eax | Mov B$edi+1 '-'

L1:         Mov ecx 10, edx 0 | div ecx | add dl '0' | Mov B$edi+4 dl
            Mov edx 0 | div ecx | add dl '0' | Mov B$edi+3 dl
            Mov edx 0 | div ecx | add dl, '0' | Mov B$edi+2 dl
            add edi 5
        End_If

L9:     Mov B$edi 0 | fldcw W@ControlWord | fwait

        If B$NegatedReg = &TRUE
            Mov eax D@Float80Pointer | xor D$eax+9 0_80
        End_If
EndP


[SpecialFPU: D$ ?]

Proc DisassemblerFloatToUString:
    Arguments @Float80Pointer, @DestinationPointer
    Local @iExp, @ControlWord, @MyControlWord
    Uses esi, edi, edx, ecx

        Mov B$SpecialFPU &FALSE
        Mov edi D@DestinationPointer, eax D@Float80Pointer

        ..If D$eax = 0
            .If D$eax+4 = 0
                If W$eax+8 = 0
                    Mov B$edi '0', B$edi+1 0

EndP

                End_If
            .End_If
        ..End_If

        Mov B$NegatedReg &FALSE
        Test B$eax+9 0_80 ZERO L1>
          xor D$eax+9 0_80 | Mov B$NegatedReg &TRUE | Mov B$edi '-' | inc edi

L1:     ;  _______________________________________________________
        ; |        |                 |             |              |
        ; | Bit 79 | Bit 78 ... 64   | Bit 63      | Bit 62 ... 0 |
        ; | Sign   | Biased Exponent | Integer Bit | Fraction     |
        ; |________|_________________|_____________|______________|
        ;
        ; SNaN  : S=0  E=7FFF  I=1  F=1..3FFF_FFFF_FFFF_FFFF
        ; QNaN  : S=0  E=7FFF  I=1  F=4000_0000_0000_0000..7FFF_FFFF_FFFF_FFFF
        ; INF   : S=0  E=7FFF  I=1  F=0
        ; -SNaN : S=1  E=7FFF  I=1  F=1..3FFF_FFFF_FFFF_FFFF
        ; -QNaN : S=1  E=7FFF  I=1  F=4000_0000_0000_0000..7FFF_FFFF_FFFF_FFFF
        ; -INF  : S=1  E=7FFF  I=1  F=0

        ; Add: Tiny : S=x  E=0     I=0  F<>0

        movzx edx W$eax+8 | and edx 07FFF ; edx = E

        If edx = 07FFF
            Mov B$SpecialFPU &TRUE

EndP

        End_If

        fclex | fstcw W@ControlWord | Mov W@MyControlWord 027F | fldcw W@MyControlWord

        fld T$eax | fld st0

        fxtract | fstp st0 | fldlg2 | fmulp st1 st0 | fistp D@iExp

        .If D@iExp L 16
            fld st0 | frndint | fcomp st1 | fstsw ax
            Test ax 040 ZERO L1>
                Call FloatToBCD

                Mov eax 17 | Mov ecx D@iExp | sub eax ecx | inc ecx
                lea esi D$TempoAsciiFpu+eax

                If B$esi = '0'
                    inc esi | dec ecx
                End_If

                Mov eax 0
                rep movsb | jmp L9>>

        .End_If

L1:     Mov eax, 6 | sub eax D@iExp

        Call PowerOf10

        fcom Q$ten7 | fstsw ax | Test ah 1 ZERO L1>
            fmul Q$ten | dec D@iExp

L1:     Call FloatToBCD

        lea esi D$TempoAsciiFpu+11 | Mov ecx D@iExp

        If ecx = 0-1
            Mov B$edi '0' | inc edi
        End_If

        inc ecx

        If ecx <= 7
            Mov eax 0
            rep movsb | Mov B$edi '.' | inc edi
            Mov ecx 6 | sub ecx D@iExp | rep movsb

            While B$edi-1 = '0' | dec edi | End_While
            On B$edi-1 = '.', dec edi

            jmp L9>>
        Else
            movsb | Mov B$edi '.' | inc edi | movsd | movsw

            Mov B$edi 'e' | Mov eax D@iExp
            Mov B$edi+1 '+'
            Test eax 0_8000_0000 ZERO L1>
                neg eax | Mov B$edi+1 '-'

L1:         Mov ecx 10, edx 0 | div ecx | add dl '0' | Mov B$edi+4 dl
            Mov edx 0 | div ecx | add dl '0' | Mov B$edi+3 dl
            Mov edx 0 | div ecx | add dl, '0' | Mov B$edi+2 dl
            add edi 5
        End_If

L9:     Mov B$edi 0 | fldcw W@ControlWord | fwait

        If B$NegatedReg = &TRUE
            Mov eax D@Float80Pointer | xor D$eax+9 0_80
        End_If
EndP
____________________________________________________________________________________________
; EOT
