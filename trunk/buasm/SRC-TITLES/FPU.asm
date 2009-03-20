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

UsedByTheAssembler:

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

[FalogOldcw: W$ 0   FalogNewcw: 0]

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

UsedBytheEqualParser:

Proc FloatToAscii:
;;
  This procedure was written by Raymond Filiatreault, December 2002
  (Modified Betov December 2002). 
  (Slightly modified by Scarmatil to fit upon RosAsm's signs, December 2003) 
  
  This FloatToAscii function converts an 80-bit REAL number (Src) to its
  decimal representation as a zero terminated alphanumeric string which
  is returned at the specified memory destination unless an invalid
  operation is reported by the FPU. The format of the string can be 
  specified as regular, or scientific notation. The number of decimal
  places returned must also be specified but the total number of digits
  must not exceed 18.
  
  The source can be an 80-bit REAL number from the FPU itself or from
  memory. If the source is taken from the FPU, its value will be preserved
  there if no error is reported.
  
  The source is not checked for validity. This is the programmer's
  responsibility.
  
  This procedure is based on using an FPU instruction to convert the
  REAL number into a specific packed decimal format. After unpacking,
  the decimal point is positioned as required.
  
  Only EAX is used to return error or success. All other registers are
  preserved.
____________________________________________________________________________________________

  Calling:     > Call FloatToAscii Source, Destination, Decimal, FLAG
  
  Source: Either a Pointer to a Data [T$Source: ...], or &NULL if you 
          "fld F$ / R$ / T$ Source"  before calling.
          
  Destination: Pointer to a Data Buffer for the Ascii (Space-terminated) String 
               (Max Size = 25 Bytes).
  
  Decimal: The Number of wanted decimals (Max = 15).
  
  FLAG (for notation choice): Either SCIENTIFIC or REGULAR.
____________________________________________________________________________________________
;;

; Flags:

[REGULAR 0    SCIENTIFIC 1]

    Arguments @Source, @Destination, @Decimal, @Flag
    Local @temporary, @eSize, @oldcw, @truncw, @stword
    Structure @BCD 12, @bcdstr 0

        fclex                   ;clear exception flags on FPU

      ; Get the specified number of decimals for result (MAX = 15):
        On D@Decimal > 0F, Mov D@Decimal 0F

      ; The FPU will be initialized only if the source parameter is not taken
      ; from the FPU itself (D@ Source <> &NULL):
        .If D@Source = &NULL
            fld st0             ;copy it to preserve the original value
        .Else
            Mov eax D@Source
            If eax > 0400_000
                finit | fld T$eax
              ; Check first if value on FPU is valid or equal to zero:
                ftst                    ;test value on FPU
                fstsw W@stword          ;get result
                test W@stword 04000,    ;check it for zero or NAN
                ZERO L0>                ;continue if valid non-zero
                test W@stword 0100,     ;now check it for NAN
                NOT_ZERO L1>            ;Src is NAN or infinity - cannot convert
                  ; Here: Value to be converted = 0
                    Mov eax D@Destination | Mov W$eax '0' ; Write '0', 0 szstring
                    Mov eax &TRUE | finit |
EndP

            Else
L1:             finit | Mov eax &FALSE

EndP

            End_If
        .End_If

      ; Get the size of the number:
L0:     fld st0                 ;copy it
        fabs                    ;insures a positive value
        fld1 | fldl2t
        fdivp ST1 ST0           ;->1/[log2(10)]
        fxch | fyl2x            ;->[log2(Src)]/[log2(10)] = log10(Src)

        fstcw W@oldcw           ;get current control word
        Mov ax W@oldcw
        or ax 0C00              ;code it for truncating
        Mov W@truncw ax
        fldcw W@truncw          ;change rounding code of FPU to truncate

        fist D@eSize            ;store characteristic of logarithm
        fldcw W@oldcw           ;load back the former control word

        ftst                    ;test logarithm for its sign
        fstsw W@stword          ;get result
        test W@stword 0100,     ;check if negative
        ZERO L0>
            dec D@eSize

L0:     On D@eSize > 15, Mov D@Flag SCIENTIFIC

      ; Multiply the number by a power of 10 to generate a 16-digit integer:
L0:     fstp st0                ;get rid of the logarithm
        Mov eax 15
        sub eax D@eSize         ;exponent required to get a 16-digit integer
        jz L0>                  ;no need if already a 16-digit integer
            Mov D@temporary eax
            fild D@temporary
            fldl2t | fmulp ST1 ST0       ;->log2(10)*exponent
            fld st0 | frndint | fxch
            fsub st0 st1        ;keeps only the fractional part on the FPU
            f2xm1               ;->2^(fractional part)-1
            fld1
            faddp ST1 ST0       ;add 1 back
            fscale              ;re-adjust the exponent part of the REAL number
            fxch
            fstp st0
            fmulp ST1 ST0       ;->16-digit integer

L0:     fbstp T@bcdstr          ;transfer it as a 16-digit packed decimal
        fstsw W@stword          ;retrieve exception flags from FPU
        test W@stword 1,        ;test for invalid operation
        NOT_ZERO L1<<           ;clean-up and return error

      ; Unpack bcd, the 10 bytes returned by the FPU being in the little-endian style:
        Push ecx, esi, edi
            lea esi D@bcdstr+9
            Mov edi D@Destination
            Mov al B$esi        ;sign byte
            dec esi | dec esi
            If al = 080
                Mov al minusSign      ;insert sign if negative number
            Else
                Mov al Space      ;insert space if positive number
            End_If
            stosb

            ...If D@Flag = REGULAR
              ; Verify number of decimals required vs maximum allowed:
                Mov eax 15 | sub eax D@eSize
                cmp eax D@Decimal | jae L0>
                    Mov D@Decimal eax

              ; ;check for integer digits:
L0:             Mov ecx D@eSize
                or ecx ecx           ;is it negative
                jns L3>
                  ; Insert required leading 0 before decimal digits:
                    Mov ax '0o' | stosw
                    neg ecx
                    cmp ecx D@Decimal | jbe L0>
                        jmp L8>>

L0:                 dec ecx | jz L0>
                        stosb | jmp L0<
L0:
                    Mov ecx D@Decimal | inc ecx
                    add ecx D@eSize | jg L4>
                        jmp L8>>

              ; Do integer digits:
L3:             inc ecx
L0:             movzx eax B$esi | dec esi | ror ax 4 | ror ah 4
                add ax '00' | stosw | sub ecx 2 | jg L0<
                jz L0>
                    dec   edi

L0:             cmp D@Decimal 0 | jz L8>>
                    Mov al pointSign | stosb
                    If ecx <> 0
                        Mov al ah | stosb
                        Mov ecx D@Decimal | dec ecx | jz L8>>
                    Else
                        Mov ecx D@Decimal
                    End_If

              ; Do decimal digits:
L4:             movzx eax B$esi
                dec esi
                ror ax 4 | ror ah 4 | add ax 03030 | stosw
                sub ecx 2 | jg L4<
                jz L1>
                    dec   edi
L1:             jmp L8>>

          ; scientific notation
            ...Else
                 Mov ecx D@Decimal | inc ecx
                movzx eax B$esi | dec esi
                ror ax 4 | ror ah 4 | add ax '00' | stosb
                Mov al pointSign | stosb
                Mov al ah | stosb
                sub ecx 2 | jz L7>
                jns L0>
                    dec edi | jmp L7>
L0:             movzx eax B$esi
                dec esi
                ror ax 4 | ror ah 4
                add ax '00' | stosw | sub ecx 2 | jg L0<
                jz L7>
                    dec edi

L7:             Mov al 'E' | stosb
                Mov al plusSign, ecx D@eSize | or ecx ecx | jns L0>
                    Mov al minusSign | neg ecx
L0:             stosb
              ; Note: the absolute value of the size could not exceed 4931
                Mov eax ecx
                Mov cl 100
                div cl          ;->thousands & hundreds in AL, tens & units in AH
                Push eax
                    and eax 0FF ;keep only the thousands & hundreds
                    Mov cl 10
                    div cl      ;->thousands in AL, hundreds in AH
                    add ax '00' ;convert to characters
                    stosw       ;insert them
                Pop eax
                shr eax 8       ;get the tens & units in AL
                div cl          ;tens in AL, units in AH
                add ax '00'     ;convert to characters
                stosw           ;insert them
            ...End_If

L8:         Mov B$edi Space         ;string terminating character
        Pop edi, esi, ecx

        finit | Mov eax D@eSize
EndP
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

;;
  Routines for FPU study.
;;

UsedForStudy:
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  Strings Tables used to translate from ST0 to String:

  The effective range of integer Values is for 'Fp.Bit1' (Value 1) considered
  as an implyed 'FPBit0'. The Max Value of the Mantissa (all Bits set on) is:
  Max qWord:      18446744073709551615  (0FFFF_FFFF__FFFF_FFFF) (???....).
;;
 [FPBit64: B$ 0  '18446744073709551616' EOS  ; Max + 1 (not used).
  FPBit63: B$ 1   '9223372036854775808' EOS  ; <<<<<<<<<<<<<<<<<<< Last high Bit.
  FPBit62: B$ 1   '4611686018427387904' EOS
  FPBit61: B$ 1   '2305843009213693952' EOS
  FPBit60: B$ 1   '1152921504606846976' EOS
  FPBit59: B$ 2    '576460752303423488' EOS
  FPBit58: B$ 2    '288230376151711744' EOS
  FPBit57: B$ 2    '144115188075855872' EOS
  FPBit56: B$ 3     '72057594037927936' EOS
  FPBit55: B$ 3     '36028797018963968' EOS
  FPBit54: B$ 3     '18014398509481984' EOS
  FPBit53: B$ 4      '9007199254740992' EOS
  FPBit52: B$ 4      '4503599627370496' EOS
  FPBit51: B$ 4      '2251799813685248' EOS
  FPBit50: B$ 4      '1125899906842624' EOS
  FPBit49: B$ 5       '562949953421312' EOS

  FPBit48: B$ 5       '281474976710656' EOS
  FPBit47: B$ 5       '140737488355328' EOS
  FPBit46: B$ 6        '70368744177664' EOS
  FPBit45: B$ 6        '35184372088832' EOS
  FPBit44: B$ 6        '17592186044416' EOS
  FPBit43: B$ 7         '8796093022208' EOS
  FPBit42: B$ 7         '4398046511104' EOS
  FPBit41: B$ 7         '2199023255552' EOS
  FPBit40: B$ 7         '1099511627776' EOS
  FPBit39: B$ 8          '549755813888' EOS
  FPBit38: B$ 8          '274877906944' EOS
  FPBit37: B$ 8          '137438953472' EOS
  FPBit36: B$ 9           '68719476736' EOS
  FPBit35: B$ 9           '34359738368' EOS
  FPBit34: B$ 9           '17179869184' EOS
  FPBit33: B$ 10           '8589934592' EOS

  FPBit32: B$ 10           '4294967296' EOS
  FPBit31: B$ 10           '2147483648' EOS
  FPBit30: B$ 10           '1073741824' EOS
  FPBit29: B$ 11            '536870912' EOS
  FPBit28: B$ 11            '268435456' EOS
  FPBit27: B$ 11            '134217728' EOS
  FPBit26: B$ 12             '67108864' EOS
  FPBit25: B$ 12             '33554432' EOS
  FPBit24: B$ 12             '16777216' EOS
  FPBit23: B$ 13              '8388608' EOS
  FPBit22: B$ 13              '4194304' EOS
  FPBit21: B$ 13              '2097152' EOS
  FPBit20: B$ 13              '1048576' EOS
  FPBit19: B$ 14               '524288' EOS
  FPBit18: B$ 14               '262144' EOS
  FPBit17: B$ 14               '131072' EOS

  FPBit16: B$ 15                '65536' EOS
  FPBit15: B$ 15                '32768' EOS
  FPBit14: B$ 15                '16384' EOS
  FPBit13: B$ 16                 '8192' EOS
  FPBit12: B$ 16                 '4096' EOS
  FPBit11: B$ 16                 '2048' EOS
  FPBit10: B$ 16                 '1024' EOS
  FPBit9:  B$ 17                  '512' EOS
  FPBit8:  B$ 17                  '256' EOS
  FPBit7:  B$ 17                  '128' EOS
  FPBit6:  B$ 18                   '64' EOS
  FPBit5:  B$ 18                   '32' EOS
  FPBit4:  B$ 18                   '16' EOS  ; ...
  FPBit3:  B$ 19                    '8' EOS  ; ...
  FPBit2:  B$ 19                    '4' EOS  ; Exponent = 2
  FPBit1:  B$ 19                    '2' EOS] ; Exponent = 1
;;
  The Strings computations begins at the Label depending on the Exponent, and goes along
  downward, for each Bit set on in the Mantissa.
;;
 [Fp.Bit1:  B$ 19 '1' EOS                    ; Exponent = 0
; Position 20 is the Point, in the String.
  Fp.Bit2:  B$ 21 '5' EOS                    ; Exponent = -1
  Fp.Bit3:  B$ 21 '25' EOS                   ; Exponent = -2
  Fp.Bit4:  B$ 21 '125' EOS                  ; ...
  Fp.Bit5:  B$ 22  '625' EOS                 ; ...
  Fp.Bit6:  B$ 22  '3125' EOS
  Fp.Bit7:  B$ 22  '15625' EOS
  Fp.Bit8:  B$ 23   '78125' EOS
  Fp.Bit9:  B$ 23   '390625' EOS
  Fp.Bit10: B$ 23   '1953125' EOS
  Fp.Bit11: B$ 24    '9765625' EOS
  Fp.Bit12: B$ 24    '48828125' EOS
  Fp.Bit13: B$ 24    '244140625' EOS
  Fp.Bit14: B$ 24    '1220703125' EOS
  Fp.Bit15: B$ 25     '6103515625' EOS
  Fp.Bit16: B$ 25     '30517578125' EOS

  Fp.Bit17: B$ 25     '152587890625' EOS
  Fp.Bit18: B$ 26      '762939453125' EOS
  Fp.Bit19: B$ 26      '3814697265625' EOS
  Fp.Bit20: B$ 26      '19073486328125' EOS
  Fp.Bit21: B$ 27       '95367431640625' EOS
  Fp.Bit22: B$ 27       '476837158203125' EOS
  Fp.Bit23: B$ 27       '2384185791015625' EOS
  Fp.Bit24: B$ 27       '11920928955078125' EOS
  Fp.Bit25: B$ 28        '59604644775390625' EOS
  Fp.Bit26: B$ 28        '298023223876953125' EOS
  Fp.Bit27: B$ 28        '1490116119384765625' EOS
  Fp.Bit28: B$ 29         '7450580596923828125' EOS
  Fp.Bit29: B$ 29         '37252902984619140625' EOS
  Fp.Bit30: B$ 29         '186264514923095703125' EOS
  Fp.Bit31: B$ 30          '931322574615478515625' EOS
  Fp.Bit32: B$ 30          '4656612873077392578125' EOS

  Fp.Bit33: B$ 30          '23283064365386962890625' EOS
  Fp.Bit34: B$ 30          '116415321826934814453125' EOS
  Fp.Bit35: B$ 31           '582076609134674072265625' EOS
  Fp.Bit36: B$ 31           '2910383045673370361328125' EOS
  Fp.Bit37: B$ 31           '14551915228366851806640625' EOS
  Fp.Bit38: B$ 32            '72759576141834259033203125' EOS
  Fp.Bit39: B$ 32            '363797880709171295166015625' EOS
  Fp.Bit40: B$ 32            '1818989403545856475830078125' EOS
  Fp.Bit41: B$ 33             '9094947017729282379150390625' EOS
  Fp.Bit42: B$ 33             '45474735088646411895751953125' EOS
  Fp.Bit43: B$ 33             '227373675443232059478759765625' EOS
  Fp.Bit44: B$ 33             '1136868377216160297393798828125' EOS
  Fp.Bit45: B$ 34              '5684341886080801486968994140625' EOS
  Fp.Bit46: B$ 34              '28421709430404007434844970703125' EOS
  Fp.Bit47: B$ 34              '142108547152020037174224853515625' EOS
  Fp.Bit48: B$ 35               '710542735760100185871124267578125' EOS

  Fp.Bit49: B$ 35               '3552713678800500929355621337890625' EOS
  Fp.Bit50: B$ 35               '17763568394002504646778106689453125' EOS
  Fp.Bit51: B$ 36                '88817841970012523233890533447265625' EOS
  Fp.Bit52: B$ 36                '444089209850062616169452667236328125' EOS
  Fp.Bit53: B$ 36                '2220446049250313080847263336181640625' EOS
  Fp.Bit54: B$ 36                '11102230246251565404236316680908203125' EOS
  Fp.Bit55: B$ 37                 '55511151231257827021181583404541015625' EOS
  Fp.Bit56: B$ 37                 '277555756156289135105907917022705078125' EOS
  Fp.Bit57: B$ 37                 '1387778780781445675529539585113525390625' EOS
  Fp.Bit58: B$ 38                  '6938893903907228377647697925567626953125' EOS
  Fp.Bit59: B$ 38                  '34694469519536141888238489627838134765625' EOS
  Fp.Bit60: B$ 38                  '173472347597680709441192448139190673828125' EOS
  Fp.Bit61: B$ 39                   '867361737988403547205962240695953369140625' EOS
  Fp.Bit62: B$ 39                   '4336808689942017736029811203479766845703125' EOS
  Fp.Bit63: B$ 39                   '21684043449710088680149056017398834228515625' EOS
  Fp.Bit64: B$ 39                   '108420217248550443400745280086994171142578125' EOS]
  ;  (???!!!...)  .000000000000000000108420217248550443400745280086994171142578125
  ; Does this mean something, internally ??? I suppose only the very first Char(s)...
  ; If this length is no use, it would be interresting to cut off (speed...).

[FPBits:
 FpBit64, FpBit63, FpBit62, FpBit61
 FpBit60, FpBit59, FpBit58, FpBit57, FpBit56, FpBit55, FpBit54, FpBit53, FpBit52, FpBit51
 FpBit50, FpBit49, FpBit48, FpBit47, FpBit46, FpBit45, FpBit44, FpBit43, FpBit42, FpBit41
 FpBit40, FpBit39, FpBit38, FpBit37, FpBit36, FpBit35, FpBit34, FpBit33, FpBit32, FpBit31
 FpBit30, FpBit29, FpBit28, FpBit27, FpBit26, FpBit25, FpBit24, FpBit23, FpBit22, FpBit21
 FpBit20, FpBit19, FpBit18, FpBit17, FpBit16, FpBit15, FpBit14, FpBit13, FpBit12, FpBit11
 FpBit10,  FpBit9,  FpBit8,  FpBit7,  FpBit6,  FpBit5,  FpBit4,  FpBit3,  FpBit2,  FpBit1

 Fp.Bits:
 Fp.Bit1,  Fp.Bit2,  Fp.Bit3,  Fp.Bit4,  Fp.Bit5,  Fp.Bit6,  Fp.Bit7,  Fp.Bit8,  Fp.Bit9,  Fp.Bit10
 Fp.Bit11, Fp.Bit12, Fp.Bit13, Fp.Bit14, Fp.Bit15, Fp.Bit16, Fp.Bit17, Fp.Bit18, Fp.Bit19, Fp.Bit20
 Fp.Bit21, Fp.Bit22, Fp.Bit23, Fp.Bit24, Fp.Bit25, Fp.Bit26, Fp.Bit27, Fp.Bit28, Fp.Bit29, Fp.Bit30
 Fp.Bit31, Fp.Bit32, Fp.Bit33, Fp.Bit34, Fp.Bit35, Fp.Bit36, Fp.Bit37, Fp.Bit38, Fp.Bit39, Fp.Bit40
 Fp.Bit41, Fp.Bit42, Fp.Bit43, Fp.Bit44, Fp.Bit45, Fp.Bit46, Fp.Bit47, Fp.Bit48, Fp.Bit49, Fp.Bit50
 Fp.Bit51, Fp.Bit52, Fp.Bit53, Fp.Bit54, Fp.Bit55, Fp.Bit56, Fp.Bit57, Fp.Bit58, Fp.Bit59, Fp.Bit60
 Fp.Bit61, Fp.Bit62, Fp.Bit63, LastFPBits: Fp.Bit64]

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  'ST0ToAscii' translates an FP value, provided in ST0, to an Ascii String, in Decimal
  or Scientific notation (depending on the Exponent size).
  
  A Pointer to the output Buffer is expected, when calling.
  
  Also expected, a Value giving the wanted number of Decimals. This number can be from
  1 to 64. 
  
  The Function does not process any rounding (in case of "0001" or "999" endings) and
  chooses by itself if the Scientific Notation is whishable or not.
  
  'ST0ToAscii' does not make any additional use of the FPU Registers for computations
  and relies entirely, for the String building, on Data Check Tables for the translation,
  which is made by direct addition upon the String (a temporary version of the String is 
  build before formating the final output to the caller's Buffer).
  
  So forth, it is not at all designed for speed competition (10 times slower than similar
  Functions, even worse on Numbers with very high and very low Exponents). As opposed to
  the reverse Function ('AsciiToST0', which, being to be used in Encoders, requires the
  higher possible speed), 'ST0ToAscii', being designed for Outputs, does not require a
  great speed, but requires to preserve the FPU Contents (at any time cost).
  
  Note that, the OS may corrupt the Contents of FPU, when runing the GDI Functions. So, 
  for viewing intermediate results, along FP computations (for viewing sub-results), you
  must not output the partial Results on the screen during your Computation, but, instead,
  you must save all of your temporary results in as many Buffers, and output them all after
  your Computations are over.
  
  Usage:
  
> [MyFp: R$ 123.0885]
> [MyFPString: B$ ? # 80]
>
> fld R$MyFp
> Call ST0ToAscii MyFpString, 4
;;

Proc ST0ToAscii:
    Argument @Destination, @Decimals
    Local @Exponent, @AbortPosition
    Uses esi, edi, ecx, ebx, edx
    [@String: B$ ? # 90] [@Real10: T$ ?]

        Mov eax @String | add eax 32 | add eax D@Decimals | Mov D@AbortPosition eax

      ; Zeroed output String (20 zeros // 1 Point // XXX++ zeros):
        Mov edi @String, eax '0000', ecx 23 | rep stosd | Mov B@String+20 '.'

      ; Get a copy of ST0:
        fstp T@Real10 | fld T@Real10

      ; If Zero, Write and exit:
        Mov eax D@Real10 | or eax D@Real10+4
        If eax = 0
@Zero:      Mov edi D@Destination, D$edi '0', D$edi+1 0

EndP

        End_If

      ; If negative, write the Sign and adjust the output Pointer to next Pos:
        Mov esi @Real10, edi D@Destination
        movzx ebx W$esi+8 | Test ebx 08000 ZERO L0>
            Mov B$edi '-' | and ebx (not 08000) | inc D@Destination

      ; Biased signed Exponent in ebx:
L0:     On ebx = 0, jmp @Zero

        Mov D@Exponent 0

      ; It seems that the Binary exponent = 07FFF means something like "undefined" (...)
      ; and seems to appear when dividing. OK. But:
      ; It also seems that Binary exponent = 0 may be given for a zero value (...).

        If ebx < 03FFF ; Ex: 0.000123456...
            While ebx < 03FFF
                fmul R$TenTable+8
                fstp T@Real10 | fld T@Real10 | dec D@Exponent
                movzx ebx W$@Real10+8 | and ebx (not 08000)
                On ebx = 0, jmp @Zero
            End_While

        Else                                            ; Ex: 123.456...
            While ebx > (03FFF+29)
                fdiv R$TenTable+8
                fstp T@Real10 | fld T@Real10
                movzx ebx W$@Real10+8 | and ebx (not 08000) | inc D@Exponent
                On ebx = 07FFF, jmp @Zero
            End_While

        End_If

L1:     sub ebx 03FFF | Mov eax ebx | shl eax 2

      ; 'ebx' will hold the pointer to the Table of Pointers to Decimal Strings.
      ; For example, if eax = 0 (Exponent 0), ebx points to 'Fp.Bit1' Pointer,
      ; in 'FPBits' Table (4 'sub' because dWords Table):
        Mov ebx Fp.Bits | sub ebx eax | sub ebx 4

      ; Parse now the Mantissa (64 Bits). Each Bit is a power of 2 figured in the
      ; [FPBit64 // Fp.Bit64] Strings Table:

L1:     Mov ecx 32, edx @Real10, edx D$edx+4            ; (High dWord).
L0:     shr edx 1 | jnc L5>
            lea esi D$ebx+ecx*4
            cmp esi FPBits | jb L5>
            cmp esi LastFPBits | ja L6>>
                Mov esi D$esi, edi @String, eax 0
                lodsb | add edi eax | cmp edi D@AbortPosition | ja L6>>
                .While B$esi <> 0
                    On B$edi = '.', inc edi
                    cmp edi D@AbortPosition | ja L5>
                    lodsb | sub al '0' | add B$edi al | Mov eax 0
                  ; Left reporting, if needed:
                    While B$edi+eax > '9'
                        sub B$edi+eax 10
                        dec eax | On B$edi+eax = '.', dec eax
                        inc B$edi+eax
                    End_While
                    inc edi
                .End_While
L5:     loop L0<

        Mov ecx 32, edx @Real10, edx D$edx              ; (Low dWord).
        add ebx 4+(31*4)
L0:     shr edx 1 | jnc L5>
            lea esi D$ebx+ecx*4
            cmp esi FPBits | jb L5>
            cmp esi LastFPBits | ja L6>
                Mov esi D$esi, edi @String, eax 0
                lodsb | add edi eax | cmp edi D@AbortPosition | ja L6>
                .While B$esi <> 0
                    On B$edi = '.', inc edi
                    cmp edi D@AbortPosition | ja L5>
                    lodsb | sub al '0' | add B$edi al | Mov eax 0
                  ; Left reporting, if needed:
                    While B$edi+eax > '9'
                        sub B$edi+eax 10
                        dec eax | On B$edi+eax = '.', dec eax
                        inc B$edi+eax
                    End_While
                    inc edi
                .End_While
L5:     loop L0<

L6:   ; Now, format the String. First, cut off zeroed tail:
        Mov esi @String | add esi 89
        While B$esi = '0' | dec esi | End_While | Mov B$esi+1 0

      ; Search for first significant Char:
        Mov esi @String | While B$esi = '0' | inc esi | End_While
        On B$esi = '.' dec esi

      ; Copy to caller Destination:
        Mov edi D@Destination | While B$esi <> '.' | movsb | End_While
        Mov B$edi 0
      ; Copy the Point and the wanted Decimals, if not End_Of_String:
        ...If B$esi+1 <> 0
            movsb                                       ; Point.
            Mov ecx D@Decimals
L1:         lodsb | cmp al 0 | je L2>
                .If D$esi = '9999'
                    If D$esi+4 = '9999'
                        inc al | Mov B$esi 0
                    End_If
                .End_If
                stosb | loop L1<                        ; Decimals.

L2:         While B$edi-1 = '0' | dec edi | End_While | Mov B$edi 0

        ...Else
            While B$edi-1 = '0' | dec edi | inc D@Exponent | End_While

        ...End_If

        On B$edi-1 = '.', dec edi

        While B$edi-1 = '0' | dec edi | inc D@Exponent | End_While

      ; Write Exponent if Needed:
        Mov eax D@Exponent

        If eax <> 0
            test eax 08000_0000 NOT_ZERO L1>
                Mov D$edi ' e+ ' | jmp L2>
L1:             Mov D$edi ' e- ' | neg eax
L2:         add edi 3 | Mov ecx 10

            Mov dl 0FF | Push edx                       ; Push stack end mark
            Mov ecx 10
L0:         Mov edx 0
            div ecx | Push edx | cmp eax 0 | ja L0<     ; Push remainders
L2:         Pop eax                                     ; Retrieve Backward
            cmp al 0FF | je L9>                         ; Over?
            add al '0' | stosb | jmp L2<                ; Write
        End_If

L9:     Mov B$edi 0
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
* 'AsciiToST0' reads the Ascii String expression of a Real Number, and stores it on the
  FPU Stack Top.

* Scientific or Decimal notation.

* 20 or 30% faster than fast Ascii-to-Float Routines. Twice faster than previous RosAsm one.
  
* Uses only one (ST0) FPU Register.

* No 'finit'.
  
* Usage:

>   [SourceString: B$ '12345678.12345' EOS]
>
>   Call AsciiToST0 SourceString 
>
>   If eax = &NO_ERROR
>       fstp F$MyReal4 // R$MyReal // ...
>   Else_If eax = TOOBIG_INTEGER
>     ; Error Message...
>   Else_If eax = TOOBIG_EXPONENT
>     ; Error Message...
>   End_If

* Eax, on return may hold &NO_ERROR, TOOBIG_INTEGER, or TOOBIG_EXPONENT
;;

[TOOBIG_INTEGER 1    TOOBIG_EXPONENT 2]

[CharAfterFpNumber: D$ ?]

Proc AsciiToST0:
    Argument @String
    Local @Sign, @Digits
    Uses esi, edi, eax, ebx, ecx, edx
    [@Tempo: T$ ?]

        Mov esi D@String, D@Sign &FALSE
        While B$esi <= SPC | inc esi | End_While

        If B$esi = '+'
            inc esi
        Else_If B$esi = '-'
            inc esi | Mov D@Sign &TRUE
        End_If

        While B$esi <= SPC | inc esi | End_While
        While B$esi = '0' | inc esi | End_While

        Mov ebx 0, ecx 0, edx 0, eax 0
L0:     lodsb
      ; cmp al '_' | je L0<
        cmp al '9' | ja L1>
        cmp al '0' | jb L1>
            lea ebx D$ebx+ebx*4 | lea ebx D$eax+ebx*2-030   ; ebx = ebx*10 + (al)-'0'
            inc ecx | cmp ecx 10 | jb L0<

L0:     lodsb
      ; cmp al '_' | je L0<
        cmp al '9' | ja L1>
        cmp al '0' | jb L1>
            lea edx D$edx+edx*4 | lea edx D$eax+edx*2-030   ; edx = edx*10 + (al)-'0'
            inc ecx | cmp ecx 18 | jb L0<
                Mov eax TOOBIG_INTEGER

EndP

L1:   ; The Integer part is now in ebx:edx. Write it in ST0:
        Mov D@Digits ecx
        Mov eax ebx | or eax edx
        .If eax <> 0
            Mov D@Tempo ebx | fild D@Tempo
            If ecx > 10
                sub ecx 10 | lea ecx D$TenTable+ecx*8 | fmul R$ecx
                Mov D@Tempo edx | fiadd D@Tempo
            End_If
        .Else
            fldz
        .End_If

      _____________________________________________________________________________
      ; Now, parse the Decimals, if any, the same way ( also limited to 18 Digits):

        ...If B$esi-1 = '.'
          ; Pop and save the Integer Part, in order to use only 1 Register:
            fstp R@Tempo

            Mov ebx 0, ecx 0, edx 0, eax 0
L0:         lodsb
          ; cmp al '_' | je L0<
            cmp al '9' | ja L1>
            cmp al '0' | jb L1>
                lea ebx D$ebx+ebx*4 | lea ebx D$eax+ebx*2-030    ; ebx = ebx*10 + (al)-'0'
                inc ecx | cmp ecx 10 | jb L0<

L0:         lodsb
          ; cmp al '_' | je L0<
            cmp al '9' | ja L1>
            cmp al '0' | jb L1>
                lea edx D$edx+edx*4 | lea edx D$eax+edx*2-030    ; edx = edx*10 + (al)-'0'
                inc ecx | cmp ecx 18 | jb L0<
                    ; Too much: No Error case for Decimals > cut off.

L1:         Mov eax ebx | or eax edx
          ; Decimal in ebx:edx, Write it in ST0:
            .If eax <> 0
                Mov D@Tempo ebx | fild D@Tempo
                If ecx > 10
                    sub ecx 10
                        lea ecx D$TenTable+ecx*8 | fmul R$ecx
                        Mov D@Tempo edx | fiadd D@Tempo
                    add ecx 10
                End_If
                lea ecx D$TenTable+ecx*8 | fdiv R$ecx
              ; Add the Integer Part previously saved in Memory:
                fadd R@Tempo

            .Else
              ; If Decimal Part = 0, load the previously saved Integer Part:
                fld R@Tempo

            .End_If
        ...End_If
      ; The Complete User Number is now in ST0.
      _____________________________________________________________
      ; Scientific Notation. (The exponent must be within +4932/-4932).
        While B$esi-1 = SPC | inc esi | End_While

        Mov al B$esi-1 | or al 32

        ...If al = 'e'
            Mov edx &FALSE
            If B$esi = '-'
                Mov edx &TRUE | inc esi
            Else_If B$esi = '+'
                inc esi
            End_If

          ; Compute the Exponent Value:
            Mov ebx 0, eax 0, ecx 0
L0:         lodsb
         ;  cmp al '_' | je L0<
            cmp al '9' | ja L1>
            cmp al '0' | jb L1>
                lea ebx D$ebx+ebx*4 | lea ebx D$eax+ebx*2-030    ; ebx = ebx*10 + (al)-'0'
                inc ecx | cmp ecx 10 | jb L0<

L1:         If ebx > 4932
                Mov eax TOOBIG_EXPONENT | ExitP
            End_If

          ; Negative Exponent, if edx = &TRUE // Exponent in ebx // User Number in ST0.
          ; No additional use of FPU Registers:
            While ebx > 0
                If ebx > 18
                    Mov eax 18
                Else
                    Mov eax ebx
                End_If

                If edx = &TRUE
                    fdiv R$TenTable+eax*8
                Else
                    fmul R$TenTable+eax*8
                End_If

                sub ebx eax
            End_While

        ...End_If

        On B@Sign = &TRUE, fchs

        dec esi | Mov D$CharAfterFpNumber esi

        Mov eax &NO_ERROR
EndP

____________________________________________________________________________________________

; 18 values, not counting '1'.

[ST0TenTable: R$ 1, 10, 100, 1000, 10_000, 100_000, 1000_000, 10_000_000
              100_000_000, 1000_000_000, 10_000_000_000

              100_000_000_000, 1000_000_000_000, 10_000_000_000_000
              100_000_000_000_000, 1000_000_000_000_000
              10_000_000_000_000_000, 100_000_000_000_000_000
              1000_000_000_000_000_000]
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
 Last Kenny 'Ascii2Float':
 
 [testfloat: B$'.09e+1' EOS]
[maxexp: D$100]
Function Ascii2Float:
local @numneg, @decplace, @expneg, @num, @exp
; IN:  esi = pointer to ascii string (terminated by <= space)
; OUT: st0 = floating point value

    Mov D@decplace 0
    Mov D@exp 0
    Mov D@num 0
L1: cmp B$esi '-' | jne L1>
        Mov B@numneg 1
        inc esi
L1: xor ecx ecx
    xor edx edx
L0: lodsb | inc edx
    cmp al '.' | je L1>
    cmp al 'e' | je L2>
    cmp al '9' | ja L8>
    cmp al '0' | jb L8>
    cmp al SPC | jbe L9>
        sub al '0'                  ; convert Decimal to binary:
        lea ecx D$ecx+ecx*4         ;     ecx = ecx * 5
        lea ecx D$eax+ecx*2         ;     ecx = eax + old ecx * 10       
    jmp L0<
L1: xor edx edx | jmp L0<
L2: Mov D@num ecx
    lodsb | cmp al '-' | sete B@expneg
    Mov D@decplace edx
    xor ecx ecx
    jmp L0<
L8: ; Error case
L9: cmp B@decplace 0
    cmove ebx edx
    cmovne ebx D@decplace
    dec ebx
   
    xor edx edx
    cmp D@num 0
    cmove eax ecx ; eax = number
    cmovne eax D@num
    cmovne edx ecx
    Mov D@num eax
    Mov D@exp edx
   
    fld F$f0.1
    fld F$f10.0
   
    fild D@num
    cmp D@decplace 0 | je L2>
   
L1: fmul st2 | dec ebx | jnz L1<
   
L2: cmp edx 0 | je L9>
    cmp edx D$maxexp | cmove edx D$maxexp
    cmp B@expneg 1 | je L2>
L3:     fmul st1 | dec edx | jnz L3<
        jmp L9>
L2:     fmul st2 | dec edx | jnz L2<

L9: On B@numneg = 1, fchs
    fxch st2 | fcompp
   
    int 3
EndF
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

UsedByTheDebugger:

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
