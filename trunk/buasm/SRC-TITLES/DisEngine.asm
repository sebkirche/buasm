TITLE DisEngine       ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

; This is the only one Table used in the Disassembler.
; Table of Pointers to each primary Opcode computation Routine:

[DisOp1:
 Op00 Op01 Op02 Op03 Op04 Op05 Op06 Op07 Op08 Op09 Op0A Op0B Op0C Op0D Op0E Op0F
 Op10 Op11 Op12 Op13 Op14 Op15 Op16 Op17 Op18 Op19 Op1A Op1B Op1C Op1D Op1E Op1F
 Op20 Op21 Op22 Op23 Op24 Op25 Op26 Op27 Op28 Op29 Op2A Op2B Op2C Op2D Op2E Op2F
 Op30 Op31 Op32 Op33 Op34 Op35 Op36 Op37 Op38 Op39 Op3A Op3B Op3C Op3D Op3E Op3F
 Op40 Op41 Op42 Op43 Op44 Op45 Op46 Op47 Op48 Op49 Op4A Op4B Op4C Op4D Op4E Op4F
 Op50 Op51 Op52 Op53 Op54 Op55 Op56 Op57 Op58 Op59 Op5A Op5B Op5C Op5D Op5E Op5F
 Op60 Op61 Op62 Op63 Op64 Op65 Op66 Op67 Op68 Op69 Op6A Op6B Op6C Op6D Op6E Op6F
 Op70 Op71 Op72 Op73 Op74 Op75 Op76 Op77 Op78 Op79 Op7A Op7B Op7C Op7D Op7E Op7F
 Op80 Op81 Op82 Op83 Op84 Op85 Op86 Op87 Op88 Op89 Op8A Op8B Op8C Op8D Op8E Op8F
 Op90 Op91 Op92 Op93 Op94 Op95 Op96 Op97 Op98 Op99 Op9A Op9B Op9C Op9D Op9E Op9F
 OpA0 OpA1 OpA2 OpA3 OpA4 OpA5 OpA6 OpA7 OpA8 OpA9 OpAA OpAB OpAC OpAD OpAE OpAF
 OpB0 OpB1 OpB2 OpB3 OpB4 OpB5 OpB6 OpB7 OpB8 OpB9 OpBA OpBB OpBC OpBD OpBE OpBF
 OpC0 OpC1 OpC2 OpC3 OpC4 OpC5 OpC6 OpC7 OpC8 OpC9 OpCA OpCB OpCC OpCD OpCE OpCF
 OpD0 OpD1 OpD2 OpD3 OpD4 OpD5 OpD6 OpD7 OpD8 OpD9 OpDA OpDB OpDC OpDD OpDE OpDF
 OpE0 OpE1 OpE2 OpE3 OpE4 OpE5 OpE6 OpE7 OpE8 OpE9 OpEA OpEB OpEC OpED OpEE OpEF
 OpF0 OpF1 OpF2 OpF3 OpF4 OpF5 OpF6 OpF7 OpF8 OpF9 OpFA OpFB OpFC OpFD OpFE OpFF]

[AMDassumed: &FALSE]
;;
  Prefixes: Op0F (EscapePrefix) , Op66 (OperandSizeOverride), Op67 (AddressSizeOverride)
;;


; Jcc Prefixes (02E / 03E) implemented as UTJ LTJ (Unlikely / Likely Taken Jump).
; Op2E > UTJ // Op3E > LTJ Followed by Jcc / (+JECXz ???).

;;
  Bad TD Files:

  115, impossible: Trick with create Window STATIC + Resource Name !!!!
  170, Menu? + Does not show the BitMaps (???...). Hang
  710, Strings IDs? BitMaps?

  810, Tricky (Auto-Write ?) Code (not Writeable in [Output].

* The Short Jumps Sizes adjustements do not seem to work (TD 170 example).
* Menu IDs replacement to be implemented.
* Some detail os wrong in the Menus until they be reloaded and resaved by the Menu Editor...
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

; 256 Routines for the primary Opcodes:
____________________________________________________________________________________________
____________________________________________________________________________________________

Op00:
    On D$esi-5 = 0, add D$UnLikelyCode 4
    On D$esi-1 = 0, add D$UnLikelyCode 1 ; ???...

; This hangs here, because a valid "add reg8 reg8" is found somewhere and
; produces something wrong in the Sections recognitions...
;
; To be analyzed on Calc.exe:
;
; RoutingMap differences with or without this added 'UnLikelyCode', in order
; to understand the failure point of the Recogitions...

   ; Push eax
   ;     Mov eax esi | add eax 7
   ;     If eax < D$UserPeEnd
   ;        ; On D$esi = 0,
;           add D$UnLikelyCode 1
   ;     End_If
   ; Pop eax

    .If B$EscapePrefix = &FALSE           ; add r/m8 r8
        Mov B$LockPrefix &FALSE
        Mov D$edi 'add ' | add edi 4 | jmp Dis_rm8_r8

    .Else
        Mov bl B$esi | inc esi | DigitMask bl To al

        If al = 0       ; 0F 00 /0 SLDT r/m16 ; 0F 00 /0 SLDT r/m32
            Mov D$edi 'sldt', B$edi+4 SPC | add edi 5 | jmp Dis_rm32_rm16
        Else_If al = 1  ; 0F 00 /1 STR r/m16
            Mov D$edi 'str ' | add edi 4
        Else_If al = 2  ; 0F 00 /2 LLDT r/m16
            Mov D$edi 'lldt', B$edi+4 SPC | add edi 5
        Else_If al = 3  ; 0F 00 /3 > LTR r/m16
            Mov D$edi 'ltr ' | add edi 4 | jmp EndWith.W.mem
        Else_If al = 4  ; 0F 00 /4 VERR r/m16
            Mov D$edi 'verr', B$edi+4 SPC | add edi 5 ; VERW
        Else_If al = 5  ; 0F 00 /5 VERW r/m16
            Mov D$edi 'verw', B$edi+4 SPC | add edi 5
        Else
            dec esi | ret
        End_If
        inc D$UnLikelyCode
        jmp EndWith.W.mem
    .End_If


Op01:
    ..If B$EscapePrefix = &FALSE
        Mov B$LockPrefix &FALSE
        Mov D$edi 'add ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16
        inc D$LikelyCode

    ..Else_If W$esi = 0C801 ; 0F,01,C8 MONITOR
        inc D$LikelyCode
        add esi 2
        Mov D$edi 'moni', D$edi+4 'tor ' | add edi 8
        Mov B$DisFlag DISDONE+DISLINEOVER | ret

    ..Else_If W$esi = 0C901 ; 0F,01,C9 MWAIT
        inc D$LikelyCode
        add esi 2
        Mov D$edi 'mwai', D$edi+4 't ' | add edi 6
        Mov B$DisFlag DISDONE+DISLINEOVER | ret

    ..Else
        Mov bl B$esi | inc esi | DigitMask bl To al
        inc D$UnLikelyCode

        .If al = 0          ; 0F 01 /0 SGDT m
            Mov D$edi 'sgdt', B$edi+4 SPC | add edi 5 | jmp EndWith.X.mem
        .Else_If al = 1     ; 0F 01 /1 SIDT m
            Mov D$edi 'sidt', B$edi+4 SPC | add edi 5 | jmp EndWith.X.mem
        .Else_If al = 2     ; LGDT m16&32
            Mov D$edi 'lgdt', B$edi+4 SPC | add edi 5 | jmp EndWith.X.mem
        .Else_If al = 3     ; LIDT m16&32
            Mov D$edi 'lidt', B$edi+4 SPC | add edi 5 | jmp EndWith.X.mem
        .Else_If al = 4     ; 0F 01 /4 SMSW r/m16 ; 0F 01 /4 SMSW r32/m16
            Mov D$edi 'smsw', B$edi+4 SPC | add edi 5 | jmp Dis_rm32_rm16
        .Else_If al = 6     ; LMSW r/m16
            Mov D$edi 'lmsw', B$edi+4 SPC | add edi 5 | jmp EndWith.W.mem
        .Else_If al = 7     ; INVLPG m
            Mov D$edi 'invl', D$edi+4 'pg  ' | add edi 7 | jmp EndWith.X.mem
        .Else
            dec esi | ret
        .End_If
    ..End_If


Op02:
    If B$EscapePrefix = &FALSE
        Mov B$LockPrefix &FALSE
        Mov D$edi 'add ' | add edi 4 | jmp Dis_r8_rm8
    Else       ; LAR r16,r/m16
        Mov D$edi 'lar ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16
    End_If


Op03:
    If B$EscapePrefix = &FALSE
        Mov D$edi 'add '
    Else     ; LSL r32,r/m32 ; LSL r16,r/m16
        Mov D$edi 'lsl '
        inc D$UnLikelyCode
    End_If
    add edi 4 | jmp Dis_r32_r16__rm32_rm16


Op04: ; add al imm8
    If D$esi-1 = 4
        inc D$UnlikelyCode
    Else
        inc D$LikelyCode
    End_If
    Mov D$edi 'add ' | add edi 4 | jmp Dis_al_imm8


Op05: ; add eax/ax imm32/imm16
    Mov D$edi 'add ' | add edi 4 | jmp Dis_eax_ax__imm32_imm16


Op06: ; clts
    inc D$UnLikelyCode
    If B$EscapePrefix = &TRUE
        Mov D$edi 'clts' | add edi 4 | Mov B$DisFlag DISDONE+DISLINEOVER
    Else        ; 06 Push ES
        Mov D$edi 'Push', D$edi+4 ' es ' | add edi 7 | Mov B$DisFlag DISDONE+DISLINEOVER
    End_If
ret


Op07: ; 07 Pop ES
    inc D$UnLikelyCode
    Mov D$edi 'Pop ', W$edi+4 'es' | add edi 6 | Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op08:
    If B$EscapePrefix = &FALSE        ; OR r/m8,r8
        inc D$LikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'or  ' | add edi 3 | jmp Dis_rm8_r8
    Else     ; INVD
        inc D$UnLikelyCode
        Mov D$edi 'invd' | add edi 4 | Mov B$DisFlag DISDONE+DISLINEOVER
    End_If
ret


Op09:
    If B$EscapePrefix = &FALSE    ; OR r/m16,r16 // OR r/m32,r32
        inc D$LikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'or  ' | add edi 3 | jmp Dis_rm32_rm16__r32_r16
    Else    ; 0F 09 WBINVD
        inc D$UnLikelyCode
        Mov D$edi 'wbin', D$edi+4 'vd  ' | add edi 6
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op0A: ; OR r8,r/m8
    inc D$LikelyCode
    Mov B$LockPrefix &FALSE
    Mov D$edi 'or  ' | add edi 3 | jmp Dis_r8_rm8


Op0B:
    If B$EscapePrefix = &FALSE    ; OR r32,r/m32 // OR r16,r/m16
        inc D$LikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'or  ' | add edi 3 | jmp Dis_r32_r16__rm32_rm16
    Else    ; 0F 0B UD2
        inc D$UnLikelyCode
        Mov D$edi 'ud2 ' | add edi 3
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op0C: ; OR AL,imm8
    inc D$LikelyCode
    Mov D$edi 'or  ' | add edi 3 | jmp Dis_al_imm8


Op0D: ; OR AX,imm16 ; OR EAX,imm32
    .If B$AMDassumed = &TRUE
         If B$EscapePrefix = &TRUE ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Looki
            Mov D$edi 'PREF', D$edi+4 'ETCH', W$edi+8 'W ' | add edi 10
            jmp Dis_rm8
        End_If
    .End_If
    inc D$LikelyCode
    Mov D$edi 'or  ' | add edi 3 | jmp Dis_eax_ax__imm32_imm16


Op0E: ; 0E Push CS
    .If B$AMDassumed = &TRUE
        If B$EscapePrefix = &TRUE ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Looki
            Mov D$edi 'FEMM', B$edi+4 'S' | add edi 5
            Mov B$DisFlag DISDONE+DISLINEOVER
            ret
        End_If
    .End_If

    inc D$UnLikelyCode
    Mov D$edi 'Push', D$edi+4 ' cs ' | add edi 7
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op0F:
    ...If B$AMDassumed = &TRUE
        ..If B$EscapePrefix = &TRUE ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< new
            .If B$esi+1 = 0AE ; 0F 0F xx 0AE - PFACC
                Mov D$edi 'PFAC', W$edi+4 'C ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 09E ; 0F 0F xx 09E - PFADD
                Mov D$edi 'PFAD', W$edi+4 'D ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 09A ; 0F 0F xx 09A - PFSUB
                Mov D$edi 'PFSU', W$edi+4 'B ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 0B4 ; 0F 0F xx 0B4 - PFMUL
                Mov D$edi 'PFMU', W$edi+4 'L ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 096 ; 0F 0F xx 096 - PFRCP
                Mov D$edi 'PFRC', W$edi+4 'P ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 097 ; 0F 0F xx 097 - PFRSQRT <<<<<< until here all are common
                Mov D$edi 'PFRS', D$edi+4 'QRT ' | add edi 8
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 0BF ; 0F 0F xx 0BF - PAVGUSB
                Mov D$edi 'PAVG', D$edi+4 'USB ' | add edi 8
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 01D ; 0F 0F xx 01D - PF2ID
                Mov D$edi 'PF2I', W$edi+4 'D ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 090 ; 0F 0F xx 090 - PFCMPGE
                Mov D$edi 'PFCM', D$edi+4 'PGE ' | add edi 8
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 0A0 ; 0F 0F xx 0A0 - PFCMPGT
                Mov D$edi 'PFCM', D$edi+4 'PGT ' | add edi 8
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 0B0 ; 0F 0F xx 0B0 - PFCMPEQ
                Mov D$edi 'PFCM', D$edi+4 'PEQ ' | add edi 8
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 094 ; 0F 0F xx 094 - PFMIN
                Mov D$edi 'PFMI', W$edi+4 'N ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 0A4 ; 0F 0F xx 0A4 - PFMAX
                Mov D$edi 'PFMA', W$edi+4 'X ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 0A6 ; 0F 0F xx 0A6 - PFRCPIT1
                Mov D$edi 'PFRC', D$edi+4 'PIT1', B$edi+8 SPC | add edi 9
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 0A7 ; 0F 0F xx 0A7 - PFRSQIT1
                Mov D$edi 'PFRS', D$edi+4 'QIT1', B$edi+8 SPC | add edi 9
                jmp Dis_mmx1__mmx2_m64_v2
    ;
            .Else_If B$esi+1 = 0B6 ; 0F 0F xx 0B6 - PFRCPIT2
                Mov D$edi 'PFRC', D$edi+4 'PIT2', B$edi+8 SPC | add edi 9
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 0AA ; 0F 0F xx 0AA - PFSUBR
                Mov D$edi 'PFSU', W$edi+4 'BR', B$edi+6 SPC | add edi 7 ; ??????
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 0D ; 0F 0F xx 0D - PI2FD
                Mov D$edi 'PI2F', W$edi+4 'D ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 0B7 ; 0F 0F xx 0B7 - PMULHRW
                Mov D$edi 'PMUL', D$edi+4 'HRW ' | add edi 8
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 0C ; 0F 0F xx 0C - PI2FW*
                Mov D$edi 'PI2F', W$edi+4 'W ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 01C ; 0F 0F xx 01C - PF2IW*
                Mov D$edi 'PF2I', W$edi+4 'W ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 08E ; 0F 0F xx 08E - PFPNACC*
                Mov D$edi 'PFPN', D$edi+4 'ACC ' | add edi 8
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 08A ; 0F 0F xx 08A - PFNACC*
                Mov D$edi 'PFNA', W$edi+4 'CC', B$edi+6 SPC | add edi 7 ; ??????
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 0BB ; 0F 0F xx 0BB - PSWAPD*
                Mov D$edi 'PSWA', W$edi+4 'PD', B$edi+6 SPC | add edi 7 ; ??????
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 086 ; 0F 0F xx 086 - PFRCPV**
                Mov D$edi 'PFRC', W$edi+4 'PV', B$edi+6 SPC | add edi 7 ; ??????
                jmp Dis_mmx1__mmx2_m64_v2
            .Else_If B$esi+1 = 087 ; 0F 0F xx 087 - PFRSQRTV**
                Mov D$edi 'PFRS', D$edi+4 'QRTV', B$edi+8 SPC | add edi 9
    ;* Enhanced 3DNow! or Extended 3DNow! or 3DNow!+ (Athlon/XP+-Doc[22466.pdf])
    ;** 3DNow! Professional or 3DNow! Pro (Geode LX/GX)
            .Else
                inc D$UnLikelyCode
            .End_If
        ..End_If
    ...End_If

    On B$EscapePrefix = &TRUE, inc D$UnLikelyCode
    Mov B$EscapePrefix &TRUE, B$DisFlag DISDONE
    inc D$Prefixes
ret


Op10:
    .If B$EscapePrefix = &FALSE            ; adc r/m8 m8
        inc D$LikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'adc ' | add edi 4 | jmp Dis_rm8_m8

    .Else
        ;inc D$UnLikelyCode
        Mov D$edi 'movu'
        If B$OperandSizeOverride = &TRUE   ; 66 0F 10 /r MOVUPD xmm1, xmm2/m128
            Call MarkSSEdata SSE_2_R
            Mov D$edi+4 'pd  '
        Else       ; 0F 10 /r MOVUPS xmm1, xmm2/m128
            Call MarkSSEdata SSE_4_F
            Mov D$edi+4 'ps  '
        End_If
        add edi 7 | jmp Dis_xmm1__xmm2_m128

    .End_If


Op11:
    .If B$EscapePrefix = &FALSE         ; adc r/m32//r/m16 r32/r16
        inc D$LikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'adc ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16

    .Else
        ;inc D$UnLikelyCode
        Mov D$edi 'movu'
        If B$OperandSizeOverride = &TRUE   ; 66 0F 11 /r MOVUPD xmm2/m128, xmm
            Call MarkSSEdata SSE_2_R
            Mov D$edi+4 'pd  '
        Else       ; 0F 11 /r MOVUPS xmm2/m128, xmm1
            Call MarkSSEdata SSE_4_F
            Mov D$edi+4 'ps  '
        End_If
        add edi 7 | jmp Dis_xmm2_m128__xmm1

    .End_If


Op12:
    ..If B$EscapePrefix = &TRUE
       ; inc D$UnLikelyCode
        .If B$OperandSizeOverride = &TRUE        ; 66 0F 12 /r MOVLPD xmm, m64
            Call MarkSSEdata SSE_1_R
            Mov bl B$esi | inc esi
            Mov D$edi 'movl', D$edi+4 'ps  ' | add edi 7 | jmp Dis_xmm_m64
        .Else        ; OF 12 /r MOVHLPS xmm1, xmm2 // 0F 12 /r MOVLPS xmm, m64
            Mov bl B$esi | inc esi | ModMask bl to al

            If al = 3
                Mov D$edi 'movh', D$edi+4 'lps ' | add edi 8
                jmp Dis_xmm1_xmm2
            Else
                dec esi | Call MarkSSEdata SSE_2_F | inc esi
                Mov D$edi 'movl', D$edi+4 'ps  ' | add edi 7
                jmp Dis_xmm_m64
            Else
                inc D$UnLikelyCode | dec esi | ret
            End_If
        .End_If

    ..Else    ; adc r8 r/m8
        inc D$LikelyCode
        Mov D$edi 'adc ' | add edi 4 | jmp Dis_r8_rm8

    ..End_If


Op13:
    .If B$EscapePrefix = &TRUE
       ; inc D$UnLikelyCode
        Mov D$edi 'movl'
        If B$OperandSizeOverride = &TRUE        ; 66 0F 13 /r MOVLPD m64, xmm
            Call MarkSSEdata SSE_1_R
            Mov D$edi+4 'pd  '
        Else    ; 0F 13 /r MOVLPS m64, xmm
            Call MarkSSEdata SSE_2_F
            Mov D$edi+4 'ps  '
        End_If
        add edi 7 | Mov bl B$esi | inc esi | jmp Dis_m64_xmm

    .Else       ; adc r32/r16 r/m32//r/m16
        inc D$LikelyCode
        Mov D$edi 'adc ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16

    .End_If


Op14:
    .If B$EscapePrefix = &TRUE
       ; inc D$UnLikelyCode
        Mov D$edi 'unpc'
        If B$OperandSizeOverride = &TRUE    ; 66 0F 14 /r UNPCKLPD xmm1, xmm2/m128
            Call MarkSSEdata SSE_2_R
            Mov D$edi+4 'klpd'
        Else        ; 0F 14 /r UNPCKLPS xmm1, xmm2/m128
            Call MarkSSEdata SSE_4_F
            Mov D$edi+4 'klps'
        End_If
        Mov B$edi+8 SPC | add edi 9 | jmp Dis_xmm1__xmm2_m128

    .Else ; adc al imm8
        inc D$LikelyCode
        Mov D$edi 'adc ' | add edi 4 | jmp Dis_al_imm8

    .End_If
ret


Op15:
    .If B$EscapePrefix = &TRUE
       ; inc D$UnLikelyCode
        Mov D$edi 'unpc'
        If B$OperandSizeOverride = &TRUE    ; 66 0F 15 /r UNPCKHPD xmm1, xmm2/m128
            Call MarkSSEdata SSE_2_R
            Mov D$edi+4 'khpd'
        Else        ; 0F 15 /r UNPCKHPS xmm1, xmm2/m128
            Call MarkSSEdata SSE_4_F
            Mov D$edi+4 'khps'
        End_If
        Mov B$edi+8 SPC | add edi 9 | jmp Dis_xmm1__xmm2_m128

    .Else   ; adc eax/ax imm32/imm16
        inc D$LikelyCode
        Mov D$edi 'adc ' | add edi 4 | jmp Dis_eax_ax__imm32_imm16

    .End_If


Op16:

    ..If B$EscapePrefix = &TRUE
        .If B$OperandSizeOverride = &TRUE    ; 66 0F 16 /r MOVHPD xmm, m64
            Call MarkSSEdata SSE_1_R
            Mov bl B$esi | inc esi
            Mov D$edi 'movh', D$edi+4 'pd  ' | add edi 7 | jmp Dis_xmm_m64

        .Else            ; 0F 16 /r MOVHPS xmm, m64 // OF 16 /r MOVLHPS xmm1, xmm2
            Mov bl B$esi | inc esi | ModMask bl to al

            If al = 3
                Mov D$edi 'movl', D$edi+4 'hps ' | add edi 8
                jmp Dis_xmm1_xmm2
            Else
                dec esi | Call MarkSSEdata SSE_2_F | inc esi
                Mov D$edi 'movh', D$edi+4 'ps  ' | add edi 7
                jmp Dis_xmm_m64
            Else
                inc D$UnLikelyCode | dec esi | ret
            End_If
        .End_If

    ..Else  ; 16 Push SS
        Mov D$edi 'Push', D$edi+4 ' ss ' | add edi 7

    ..End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op17:
    .If B$EscapePrefix = &TRUE
        Mov D$edi 'movh'
        If B$OperandSizeOverride = &TRUE    ; 66 0F 17 /r MOVHPD m64, xmm
            Call MarkSSEdata SSE_1_R
            Mov D$edi+4 'pd  '
        Else            ; 0F 17 /r MOVHPS m64, xmm
            Call MarkSSEdata SSE_2_F
            Mov D$edi+4 'ps  '
        End_If
        add edi 7 | Mov bl B$esi | inc esi | jmp Dis_m64_xmm

    .Else       ; 17 Pop SS
        inc D$UnLikelyCode
        Mov D$edi 'Pop ', W$edi+4 'ss' | add edi 6

    .End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op18:
    .If B$EscapePrefix = &TRUE

        Mov D$edi 'pref', D$edi+4 'etch' | add edi 8
        Mov bl B$esi | inc esi | DigitMask bl to al

        If al = 0           ; 0F 18 /0 PREFETCHNTA m8
            Mov D$edi 'nta ' | add edi 4
        Else_If al = 1      ; 0F 18 /1 PREFETCHT1 m8
            Mov D$edi 't0  ' | add edi 3
        Else_If al = 2      ; 0F 18 /1 PREFETCHT2 m8
            Mov D$edi 't1  ' | add edi 3
        Else_If al = 3      ; 0F 18 /1 PREFETCHT3 m8
            Mov D$edi 't2  ' | add edi 3
        Else
            inc D$UnLikelyCode | dec esi | ret
        End_If
        jmp EndWith.B.mem

    .Else       ; 18 /r SBB r/m8,r8
        inc D$LikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'sbb ' | add edi 4 | jmp Dis_rm8_r8

    .End_If


Op19: ; 19 /r SBB r/m16,r16 ; 19 /r SBB r/m32,r32
    inc D$LikelyCode
    Mov B$LockPrefix &FALSE
    Mov D$edi 'sbb ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16


Op1A: ; 1A /r SBB r8,r/m8
    inc D$LikelyCode
    Mov D$edi 'sbb ' | add edi 4 | jmp Dis_r8_rm8


Op1B: ; 1B /r SBB r16,r/m16 ; 1B /r SBB r32,r/m32
    inc D$LikelyCode
    Mov D$edi 'sbb ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16


Op1C: ; 1C ib SBB AL,imm8
    inc D$LikelyCode
    Mov D$edi 'sbb ' | add edi 4 | jmp Dis_al_imm8


Op1D: ; 1D iw SBB AX,imm16  ; 1D id SBB EAX,imm32
    inc D$LikelyCode
    Mov D$edi 'sbb ' | add edi 4 | jmp Dis_eax_ax__imm32_imm16


Op1E:   ; 1E Push DS
    inc D$UnLikelyCode
    Mov D$edi 'Push', D$edi+4 ' ds ' | add edi 7
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op1F: ; 1F Pop DS
    inc D$UnLikelyCode
    Mov D$edi 'Pop ', W$edi+4 'ds' | add edi 6
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op20:
    If B$EscapePrefix = &FALSE
        inc D$LikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'and ' | add edi 4 | jmp Dis_rm8_r8

    Else        ; Mov r32,CR0 // CR1...
        inc D$UnLikelyCode
        Mov bl B$esi | inc esi | BaseMask bl to al | and eax 0FF
        Mov D$edi 'Mov ' | add edi 4
        Move D$edi D$dWordRegsTable+eax*4 | add edi 4
        Mov D$edi 'CR0 ' | DigitMask bl To al | add B$edi+2 al | add edi 3
        Mov B$DisFlag DISDONE+DISLINEOVER

    End_If
ret


Op21:
    If B$EscapePrefix = &FALSE
        inc D$LikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'and ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16

    Else        ; Mov r32, DR0-DR7
        inc D$UnLikelyCode
        Mov bl B$esi | inc esi | BaseMask bl to al | and eax 0FF
        Mov D$edi 'Mov ' | add edi 4
        Move D$edi D$dWordRegsTable+eax*4 | add edi 4
        Mov D$edi 'DR0 ' | DigitMask bl To al | add B$edi+2 al | add edi 3
        Mov B$DisFlag DISDONE+DISLINEOVER

    End_If
ret


Op22:
    If B$EscapePrefix = &FALSE
        inc D$LikelyCode
        Mov D$edi 'and ' | add edi 4 | jmp Dis_r8_rm8

    Else        ; Mov CR0,r32 // CR1...
        inc D$UnLikelyCode
        Mov bl B$esi | inc esi | DigitMask bl To al
        Mov D$edi 'Mov ', D$edi+4 'CR0 '
        add B$edi+6 al | add edi 8
        BaseMask bl to al | and eax 0FF
        Move D$edi D$dWordRegsTable+eax*4 | add edi 3
        Mov B$DisFlag DISDONE+DISLINEOVER

    End_If
ret


Op23:
    If B$EscapePrefix = &FALSE
        inc D$LikelyCode
        Mov D$edi 'and ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16

    Else        ; Mov DR0-DR7,r32
        inc D$UnLikelyCode
        Mov bl B$esi | inc esi | DigitMask bl To al
        Mov D$edi 'Mov ', D$edi+4 'DR0 '
        add B$edi+6 al | add edi 8
        BaseMask bl to al | and eax 0FF
        Move D$edi D$dWordRegsTable+eax*4 | add edi 3
        Mov B$DisFlag DISDONE+DISLINEOVER

    End_If
ret


Op24: ; and al imm8
    inc D$LikelyCode
    Mov D$edi 'and ' | add edi 4 | jmp Dis_al_imm8


Op25: ; 25 iw AND AX,imm16 ; 25 id AND EAX,imm32
    inc D$LikelyCode
    Mov D$edi 'and ' | add edi 4 | jmp Dis_eax_ax__imm32_imm16


Op26:
    inc D$UnLikelyCode
    inc D$Prefixes
    Mov D$SegmentOverride 'es: ', B$DisFlag DISDONE
ret


Op27: ; DAA
    inc D$LikelyCode
    Mov D$edi 'daa ' | add edi 3 | Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op28: ; 66 0F 28 /r > MOVAPD xmm1, xmm2/m128
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        Mov D$edi 'mova'
        If B$OperandSizeOverride = &TRUE
            Call MarkSSEdata SSE_2_R
            Mov D$edi+4 'pd  '
        Else        ; 0F 28 /r MOVAPS xmm1, xmm2/m128
            Call MarkSSEdata SSE_4_F
            Mov D$edi+4 'ps  '
        End_If
        add edi 7 | jmp Dis_xmm1__mmx2_m128

    .Else   ; 28 /r SUB r/m8,r8
        inc D$LikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'sub ' | add edi 4 | jmp Dis_rm8_r8

    .End_If


Op29:   ; 66 0F 29 /r MOVAPD xmm2/m128, xmm1
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        Mov D$edi 'mova'
        If B$OperandSizeOverride = &TRUE
            Call MarkSSEdata SSE_2_R
            Mov D$edi+4 'pd  '
        Else        ; 0F 29 /r MOVAPS xmm2/m128, xmm1
            Call MarkSSEdata SSE_4_F
            Mov D$edi+4 'ps  '
        End_If
        add edi 7 | jmp Dis_mmx2_m128__xmm1

    .Else   ; 29 /r SUB r/m16,r16 ; 29 /r SUB r/m32,r32
        inc D$LikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'sub ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16

    .End_If


Op2A:
    .If B$EscapePrefix = &TRUE
       ; inc D$UnLikelyCode
        Mov D$edi 'cvtp'
        If B$OperandSizeOverride = &TRUE  ; ; CVTPI2PD xmm, mm/m64
            Call MarkSSEdata SSE_2_D
            Mov D$edi+4 'i2pd'
        Else                                ; CVTPI2PS xmm, mm/m64
            Call MarkSSEdata SSE_2_D
            Mov D$edi 'cvtp', D$edi+4 'i2ps'
        End_If
        Mov B$edi+8 SPC | add edi 9 | jmp Dis_xmm1__mmx2_m64

    .Else       ; 2A /r SUB r8,r/m8
        inc D$LikelyCode
        Mov D$edi 'sub ' | add edi 4 | jmp Dis_r8_rm8

    .End_If


Op2B:
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        Mov D$edi 'movn'
        If B$OperandSizeOverride = &TRUE        ; 66 0F 2B /r MOVNTPD m128, xmm
            Call MarkSSEdata SSE_2_R
            Mov D$edi+4 'tpd '
        Else        ; 0F 2B /r MOVNTPS m128, xmm
            Call MarkSSEdata SSE_4_F
            Mov D$edi+4 'tps '
        End_If
        add edi 8 | Mov bl B$esi | inc esi | jmp Dis_m128_xmm

    .Else   ; 2B /r SUB r16,r/m16 ; 2B /r SUB r32,r/m32
        inc D$LikelyCode
        Mov D$edi 'sub ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16

    .End_If


Op2C: ; CVTTPD2PI mm, xmm/m128
    .If B$EscapePrefix = &TRUE
       ; inc D$UnLikelyCode
        Mov D$edi 'cvtt'
        If B$OperandSizeOverride = &TRUE    ; CVTTPD2PI mm, xmm/m128
            Call MarkSSEdata SSE_2_R
            Mov D$edi+4 'pd2p', W$edi+8 'i ' | add edi 10
            jmp Dis_mmx1__xmm2_m128
        Else  ; CVTTPS2PI mm, xmm/m64
            Call MarkSSEdata SSE_2_F
            Mov D$edi 'cvtt', D$edi+4 'ps2p', W$edi+8 'i ' | add edi 10
            jmp Dis_mmx1__xmm2_m64
        End_If

    .Else       ; 2C ib SUB AL,imm8
        If D$esi-1 = 02C
            inc D$UnlikelyCode
        Else
            inc D$LikelyCode
        End_If
        Mov D$edi 'sub ' | add edi 4 | jmp Dis_al_imm8

    .End_If


Op2D:
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        Mov D$edi 'cvtp'
        If B$OperandSizeOverride = &TRUE    ; CVTPD2PI mm, xmm/m128
            Call MarkSSEdata SSE_2_R
            Mov D$edi+4 'd2pi', B$edi+8 SPC | add edi 9
            jmp Dis_mmx1__xmm2_m128
        Else                                ; CVTPS2PI mm, xmm/m64
            Call MarkSSEdata SSE_2_F
            Mov D$edi+4 's2pi', B$edi+8 SPC | add edi 9
            jmp Dis_mmx1__xmm2_m64
        End_If

    .Else       ; 2D iw SUB AX,imm16 ; 2D id SUB EAX,imm32
        inc D$LikelyCode
        Mov D$edi 'sub ' | add edi 4 | jmp Dis_eax_ax__imm32_imm16

    .End_If


; Unikely Taken Jump: This is a Mnemonic non defined by Intel, which purpose is to
; reverse the Jump Prediction. UTJ / LTJ have been defined in collaboration with
; the NASM developers group. UTJ / LTJ are prefixes to Jcc Instructions. I have no
; info about if it is active or not before JECXZ. As i think it can't hurt, i allow
; it in RosAsm assembling.

Op2E:
    ;inc D$UnLikelyCode
    .If B$EscapePrefix = &TRUE
        Mov D$edi 'ucom'
        If B$OperandSizeOverride = &TRUE    ; 66 0F 2E /r UCOMISD xmm1, xmm2/m64
            Call MarkSSEdata SSE_2_R
            Mov D$edi+4 'isd ' | add edi 8 | jmp Dis_xmm1__xmm2_m64
        Else    ; 0F 2E /r UCOMISS xmm1, xmm2/m32
            Call MarkSSEdata SSE_4_F
            Mov D$edi+4 'iss ' | add edi 8 | jmp Dis_xmm1__xmm2_m32
        End_If

    .Else
        ; UTJ if Jcc:
        ;      op70 71 72 73 74 75 76 77 Op78 79 7A 7B 7C 7D 7E 7F  E3 (E3 is JCXZ JECXZ...)
        ; Of / 80 81 82 83 84 85 86 86 88 89 8A 8B 8C 8D 8E 8F
        Mov al B$esi
        .If al = 0F
            Mov al B$esi+1
            cmp al 080 | jb L5>
                cmp al 08F | ja L5>
                    inc esi | Mov B$EscapePrefix &TRUE | jmp L3>

        .Else
            cmp al 0E3 | je L3>
                cmp al 070 | jb L5>
                    cmp al 07F | ja L5>
L3:                     Mov D$edi 'utj ' | add edi 4
                            movzx eax B$esi
                            inc esi | Call D$DisOp1+eax*4
                        Mov B$DisFlag DISDONE+DISLINEOVER | ret

        .End_If

L5:     Mov D$SegmentOverride 'cs: '
        inc D$Prefixes
      ; Watcom-C incodes the Api calls in the form of Call D$cs:apiname:
        If W$esi <> 015FF
            inc D$UnLikelyCode
        Else
            Mov eax D$esi+2
            sub eax D$DisImageBase | add eax D$UserPeStart | Mov eax D$eax
            On eax < D$ApiBuffer, inc D$UnLikelyCode
            On eax >= D$EndOfApiBuffer, inc D$UnLikelyCode
        End_If

    .End_If

L9: Mov B$DisFlag DISDONE
ret


Op2F:
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        Mov D$edi 'comi'
        If B$OperandSizeOverride = &TRUE
            Call MarkSSEdata SSE_1_R
            Mov D$edi+4 'sd  ' | add edi 7
            jmp Dis_xmm1__xmm2_m64     ; COMISD xmm1, xmm2/m64
        Else
            Call MarkSSEdata SSE_1_F
            Mov D$edi+4 'ss  ' | add edi 7
            jmp Dis_xmm1__xmm2_m32     ; COMISS xmm1, xmm2/m32
        End_If

    .Else ; DAS
        inc D$LikelyCode
        Mov D$edi 'das ' | add edi 3 | Mov B$DisFlag DISDONE+DISLINEOVER

    .End_If
ret


Op30:
    If B$EscapePrefix = &FALSE  ; 30 /r XOR r/m8,r8
        inc D$LikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'xor ' | add edi 4 | jmp Dis_rm8_r8
    Else         ; 0F 30 WRMSR
        inc D$UnLikelyCode
        Mov D$edi 'wrms', B$edi+4 'r' | add edi 5
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op31: ; 0F 31 RDTSC
    If B$EscapePrefix = &FALSE  ; 31 /r XOR r/m16,r16 ; 31 /r XOR r/m32,r32
        inc D$LikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'xor ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16
    Else
        ;inc D$UnLikelyCode
        Mov D$edi 'rdts', B$edi+4 'c' | add edi 5  ; rdtsc
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op32:
    If B$EscapePrefix = &FALSE  ; 32 /r XOR r8,r/m8
        inc D$LikelyCode
        Mov D$edi 'xor ' | add edi 4 | jmp Dis_r8_rm8
    Else     ; 0F 32 RDMSR
        ;inc D$UnLikelyCode
        Mov D$edi 'rdms', B$edi+4 'r' | add edi 5
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op33:
    If B$EscapePrefix = &FALSE; 33 /r XOR r16,r/m16 ; 33 /r XOR r32,r/m32
        inc D$LikelyCode
        Mov D$edi 'xor ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16
    Else        ; 0F 33 RDPMC
        ;inc D$UnLikelyCode
        Mov D$edi 'rdpm', B$edi+4 'c' | add edi 5
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op34:
    If B$EscapePrefix = &FALSE        ; 34 ib XOR AL,imm8
        inc D$LikelyCode
        Mov D$edi 'xor ' | add edi 4 | jmp Dis_al_imm8
    Else         ; 0F 34 SYSENTER
        inc D$UnLikelyCode
        Mov D$edi 'syse', D$edi+4 'nter' | add edi 8
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op35:
    If B$EscapePrefix = &FALSE        ; 35 iw XOR AX,imm16 ; 35 id XOR EAX,imm32
        inc D$LikelyCode
        Mov D$edi 'xor ' | add edi 4 | jmp Dis_eax_ax__imm32_imm16
    Else             ; 0F 35 SYSEXIT
        inc D$UnLikelyCode
        Mov D$edi 'syse', D$edi+4 'xit ' | add edi 7
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op36:
    inc D$UnLikelyCode
    inc D$Prefixes
    Mov D$SegmentOverride 'ss: ', B$DisFlag DISDONE
ret


Op37: ; aaa
    inc D$LikelyCode
    Mov D$edi 'aaa ', B$DisFlag DISDONE+DISLINEOVER | add edi 3
ret


Op38: ; CMP rm8 r8
    inc D$LikelyCode
    Mov D$edi 'cmp ' | add edi 4 | jmp Dis_rm8_r8
ret


Op39: ; cmp rm32/rm16 r32/r16
    inc D$LikelyCode
    Mov D$edi 'cmp ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16
ret


Op3A: ; CMP r8 rm8

    inc D$LikelyCode
    Mov D$edi 'cmp ' | add edi 4 | jmp Dis_r8_rm8
ret


Op3B: ; r32/r16 cmp rm32/rm16
    inc D$LikelyCode
    Mov D$edi 'cmp ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16
ret


Op3C: ; cmp al imm8
    inc D$LikelyCode
    Mov D$edi 'cmp ' | add edi 4 | jmp Dis_al_imm8


Op3D: ; cmp eax // ax,  imm32 // imm16
    inc D$LikelyCode
    Mov D$edi 'cmp ' | add edi 4 | jmp Dis_eax_ax__imm32_imm16


; Likely Taken Jump: This is a Mnemonic non defined by Intel, which purpose is to
; reverse the Jump Prediction. UTJ / LTJ have been defined in collaboration with
; the NASM developers group. UTJ / LTJ are prefixes to Jcc Instructions. I have no
; info about if it is active or not before JECXZ. As i think it can't hurt, i allow
; it in RosAsm assembling.

Op3E: ; Op2E for UTJ

    ; LTJ if Jcc:
  ;      70 71 72 73 74 75 76 77 78 79 7A 7B 7C 7D 7E 7F E3
  ; Of / 80 81 82 83 84 85 86 86 88 89 8A 8B 8C 8D 8E 8F

    Mov al B$esi
        .If al = 0F
            Mov al B$esi+1
            cmp al 080 | jb L5>
                cmp al 08F | ja L5>
                    inc esi | Mov B$EscapePrefix &TRUE | jmp L3>

        .Else
            cmp al 0E3 | je L3>
                cmp al 070 | jb L5>
                    cmp al 07F | ja L5>
L3:                     Mov D$edi 'ltj ' | add edi 4
                        Push edi
                            movzx eax B$esi
                            inc esi | add edi 5 | Call D$DisOp1+eax*4
                        Pop eax
                        Mov B$eax SPC | Mov B$DisFlag DISDONE+DISLINEOVER | ret

        .End_If

L5: Mov D$SegmentOverride 'ds: '
    inc D$Prefixes
    inc D$UnLikelyCode

L9: Mov B$DisFlag DISDONE
ret


Op3F: ; aas
    inc D$LikelyCode
    Mov D$edi 'aas ', B$DisFlag DISDONE+DISLINEOVER | add edi 4
ret


Op40: ; cmovo
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        Mov D$edi 'cmov', W$edi+4 'o ' | add edi 6 | jmp Dis_r32_r16__rm32_rm16

    .Else
        inc D$LikelyCode
        Mov D$edi 'inc ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            Mov B$edi 'e' | inc edi
        End_If
        Mov W$edi 'ax' | add edi 2 | Mov B$DisFlag DISDONE+DISLINEOVER

    .End_If
ret


Op41: ; cmovno
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        Mov D$edi 'cmov', D$edi+4 'no  ' | add edi 7 | jmp Dis_r32_r16__rm32_rm16

    .Else
        Mov D$edi 'inc ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov B$edi 'e' | inc edi
        End_If
        Mov W$edi 'cx' | add edi 2 | Mov B$DisFlag DISDONE+DISLINEOVER
    .End_If
ret


Op42: ; cmovb / cmovc / cmovnae
    .If B$EscapePrefix = &TRUE
        Mov D$edi 'cmov', W$edi+4 'c ' | add edi 6 | jmp Dis_r32_r16__rm32_rm16

    .Else
        Mov D$edi 'inc ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov B$edi 'e' | inc edi
        End_If
        Mov W$edi 'dx' | add edi 2 | Mov B$DisFlag DISDONE+DISLINEOVER

    .End_If
ret


Op43: ; cmovae / cmovnb / cmovnc
    .If B$EscapePrefix = &TRUE
        Mov D$edi 'cmov', D$edi+4 'ae  ' | add edi 7 | jmp Dis_r32_r16__rm32_rm16

    .Else
        Mov D$edi 'inc ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov B$edi 'e' | inc edi
        End_If
        Mov W$edi 'bx' | add edi 2 | Mov B$DisFlag DISDONE+DISLINEOVER

    .End_If
ret


Op44: ; cmove / cmovz
    .If B$EscapePrefix = &TRUE
        Mov D$edi 'cmov', W$edi+4 'e ' | add edi 6 | jmp Dis_r32_r16__rm32_rm16

    .Else
        Mov D$edi 'inc ' | add edi 4
        inc D$UnLikelyCode
        If B$OperandSizeOverride = &FALSE
            Mov B$edi 'e' | inc edi
        End_If
        Mov W$edi 'sp' | add edi 2 | Mov B$DisFlag DISDONE+DISLINEOVER

    .End_If
ret


Op45: ; cmovne / cmonnz
    .If B$EscapePrefix = &TRUE
        Mov D$edi 'cmov', D$edi+4 'ne  ' | add edi 7 | jmp Dis_r32_r16__rm32_rm16

    .Else
        Mov D$edi 'inc ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov B$edi 'e' | inc edi
        End_If
        Mov W$edi 'bp' | add edi 2 | Mov B$DisFlag DISDONE+DISLINEOVER

    .End_If
ret


Op46: ; cmovbe : cmovna
    .If B$EscapePrefix = &TRUE
        Mov D$edi 'cmov', D$edi+4 'be  ' | add edi 7 | jmp Dis_r32_r16__rm32_rm16

    .Else
        Mov D$edi 'inc ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov B$edi 'e' | inc edi
        End_If
        Mov W$edi 'si' | add edi 2 | Mov B$DisFlag DISDONE+DISLINEOVER

    .End_If
ret


Op47: ; cmova / cmovnbe
    .If B$EscapePrefix = &TRUE
        Mov D$edi 'cmov', W$edi+4 'a ' | add edi 6 | jmp Dis_r32_r16__rm32_rm16

    .Else
        Mov D$edi 'inc ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov B$edi 'e' | inc edi
        End_If
        Mov W$edi 'di' | add edi 2 | Mov B$DisFlag DISDONE+DISLINEOVER

    .End_If
ret


Op48: ; cmovs
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'dec ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov B$edi 'e' | inc edi
        End_If
        Mov  W$edi 'ax' | add edi 2 | Mov B$DisFlag DISDONE+DISLINEOVER
    .Else
        Mov D$edi 'cmov', W$edi+4 's ' | add edi 6 | jmp Dis_r32_r16__rm32_rm16
    .End_If
ret


Op49: ; cmovns
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'dec ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov B$edi 'e' | inc edi
        End_If
        Mov  W$edi 'cx' | add edi 2 | Mov B$DisFlag DISDONE+DISLINEOVER
    .Else
        Mov D$edi 'cmov', D$edi+4 'ns  ' | add edi 7 | jmp Dis_r32_r16__rm32_rm16
    .End_If
ret


Op4A: ; cmovp / cmovpe
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'dec ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov B$edi 'e' | inc edi
        End_If
        Mov  W$edi 'dx' | add edi 2 | Mov B$DisFlag DISDONE+DISLINEOVER
    .Else
        Mov D$edi 'cmov', W$edi+4 'p ' | add edi 6 | jmp Dis_r32_r16__rm32_rm16
    .End_If
ret


Op4B: ; cmovnp / cmovpo
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'dec ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov B$edi 'e' | inc edi
        End_If
        Mov  W$edi 'bx' | add edi 2 | Mov B$DisFlag DISDONE+DISLINEOVER
    .Else
        Mov D$edi 'cmov', D$edi+4 'np  ' | add edi 7 | jmp Dis_r32_r16__rm32_rm16
    .End_If
ret


Op4C: ; cmovl / cmovnge
    If B$EscapePrefix = &FALSE
        inc D$UnLikelyCode
        Mov D$edi 'dec ', D$edi+4 'esp ' | add edi 7 | Mov B$DisFlag DISDONE+DISLINEOVER
    Else
        Mov D$edi 'cmov', W$edi+4 'l ' | add edi 6 | jmp Dis_r32_r16__rm32_rm16
    End_If
ret


Op4D: ; cmovge / cmovnl
    If B$EscapePrefix = &FALSE
        Mov D$edi 'dec ', D$edi+4 'ebp ' | add edi 7 | Mov B$DisFlag DISDONE+DISLINEOVER
    Else
        Mov D$edi 'cmov', D$edi+4 'ge  ' | add edi 7 | jmp Dis_r32_r16__rm32_rm16
    End_If
ret


Op4E: ; cmovle / cmovng
    If B$EscapePrefix = &FALSE
        Mov D$edi 'dec ', D$edi+4 'esi ' | add edi 7 | Mov B$DisFlag DISDONE+DISLINEOVER
    Else
        Mov D$edi 'cmov', D$edi+4 'le  ' | add edi 7 | jmp Dis_r32_r16__rm32_rm16
    End_If
ret


Op4F: ; cmovg / cmovnle
    If B$EscapePrefix = &FALSE
        Mov D$edi 'dec ', D$edi+4 'edi ' | add edi 7 | Mov B$DisFlag DISDONE+DISLINEOVER
    Else
        Mov D$edi 'cmov', W$edi+4 'g ' | add edi 6 | jmp Dis_r32_r16__rm32_rm16
    End_If
ret


Op50:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Push' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov D$edi ' eax' | add edi 4
        Else
           ; inc D$UnLikelyCode
            Mov D$edi ' ax ' | add edi 3
        End_If

     .Else
        ;inc D$UnLikelyCode
        Mov D$edi 'movm'
        If B$OperandSizeOverride = &TRUE ; 66 0F 50 /r MOVMSKPD r32, xmm
            Mov D$edi+4 'skpd'
        Else            ; 0F 50 /r MOVMSKPS r32, xmm
            Mov D$edi+4 'skps'
        End_If
        Mov B$edi+8 SPC | add edi 9 | jmp Dis_r32_xmm

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op51:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Push' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov D$edi ' ecx' | add edi 4
        Else
            ;inc D$UnLikelyCode
            Mov D$edi ' cx ' | add edi 3
        End_If

    .Else
       ; inc D$UnLikelyCode
        Mov D$edi 'sqrt'
        If B$OperandSizeOverride = &TRUE   ; 66 0F 51 /r SQRTPD xmm1, xmm2/m128
            Call MarkSSEdata SSE_2_R
            Mov D$edi+4 'pd  '
        Else        ; 0F 51 /r SQRTPS xmm1, xmm2/m128
            Call MarkSSEdata SSE_4_F
            Mov D$edi+4 'ps  '
        End_If
        add edi 7 | jmp Dis_xmm1__xmm2_m128

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op52:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Push' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov D$edi ' edx' | add edi 4
        Else
            ;inc D$UnLikelyCode
            Mov D$edi ' dx ' | add edi 3
        End_If

    .Else   ; 0F 52 /r RSQRTPS xmm1, xmm2/m128
        Call MarkSSEdata SSE_4_F
        Mov D$edi 'rsqr', D$edi+4 'tps ' | add edi 8 | jmp Dis_xmm1__xmm2_m128

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op53:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Push' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov D$edi ' ebx' | add edi 4
        Else
           ; inc D$UnLikelyCode
            Mov D$edi ' bx ' | add edi 3
        End_If

    .Else  ; 0F 53 /r RCPPS xmm1, xmm2/m128
        Call MarkSSEdata SSE_4_F
        Mov D$edi 'rcpp', W$edi+4 's ' | add edi 6 | jmp Dis_xmm1__xmm2_m128

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op54:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Push' | add edi 4
        If B$OperandSizeOverride = &FALSE
            Mov D$edi ' esp' | add edi 4
        Else
            inc D$UnlikelyCode
            Mov D$edi ' sp ' | add edi 3
        End_If

    .Else
        Mov D$edi 'andp' | add edi 4
        If B$OperandSizeOverride = &TRUE                ; 066 0F 054 ...
            Call MarkSSEdata SSE_2_R    ; ANDPD 66 0F 54 /r
            Mov W$edi 'd '
        Else
            Call MarkSSEdata SSE_4_F
            Mov W$edi 's '              ; ANDPS 0F 54 /r
        End_If
        add edi 2 | jmp Dis_xmm1__xmm2_m128

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op55:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Push' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov D$edi ' ebp' | add edi 4
        Else
            inc D$UnLikelyCode
            Mov D$edi ' bp ' | add edi 3
        End_If

    .Else
        ;inc D$UnLikelyCode
        Mov D$edi 'andn' | add edi 4
        If B$OperandSizeOverride = &TRUE
            Call MarkSSEdata SSE_2_R
            Mov D$edi 'pd  '    ; ANDNPD 66 0F 55 /r
        Else
            Call MarkSSEdata SSE_4_F
            Mov D$edi 'ps  '    ; ANDNPS 0F 55 /r
        End_If
        add edi 3 | jmp Dis_xmm1__xmm2_m128

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op56:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Push' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov D$edi ' esi' | add edi 4
        Else
            inc D$UnLikelyCode
            Mov D$edi ' si ' | add edi 3
        End_If

    .Else
        ;inc D$UnLikelyCode
        If B$OperandSizeOverride = &TRUE    ; ORPD xmm1, xmm2/m128
            Call MarkSSEdata SSE_2_R
            Mov D$edi 'orpd'
        Else        ; ORPS xmm1, xmm2/m128
            Call MarkSSEdata SSE_4_F
            Mov D$edi 'orps'
        End_If
        Mov B$edi+4 SPC | add edi 5 | jmp Dis_xmm1__xmm2_m128

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op57:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Push' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov D$edi ' edi' | add edi 4
        Else
            inc D$UnLikelyCode
            Mov D$edi ' di ' | add edi 3
        End_If

    .Else
        ;inc D$UnLikelyCode
        Mov D$edi 'xorp'
        If B$OperandSizeOverride = &TRUE  ; 66 0F 57 /r XORPD xmm1, xmm2/m128
            Call MarkSSEdata SSE_2_R
            Mov W$edi+4 'd '
        Else        ; 0F 57 /r XORPS xmm1, xmm2/m128
            Call MarkSSEdata SSE_4_F
            Mov W$edi+4 's '
        End_If
        add edi 6 | jmp Dis_xmm1__xmm2_m128

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op58:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov D$edi 'eax ' | add edi 3
        Else
           ; inc D$UnLikelyCode
            Mov W$edi 'ax' | add edi 2
        End_If

    .Else
        ;inc D$UnLikelyCode
        Mov D$edi 'addp'
        If B$OperandSizeOverride = &TRUE       ; 066 0F 058...
            Call MarkSSEdata SSE_2_R
            Mov W$edi+4 'd '                   ; ADDPD
        Else
            Call MarkSSEdata SSE_4_F
            Mov W$edi+4 's '                   ; ADDPS
        End_If
        add edi 6 | jmp Dis_xmm1__xmm2_m128

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op59:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov D$edi 'ecx ' | add edi 3
        Else
           ; inc D$UnLikelyCode
            Mov D$edi 'cx' | add edi 2
        End_If

    .Else
        ;inc D$UnLikelyCode
        Mov D$edi 'mulp'
        If B$OperandSizeOverride = &TRUE        ; 66 0F 59 /r MULPD xmm1, xmm2/m128
            Call MarkSSEdata SSE_2_R
            Mov W$edi+4 'd '
        Else        ; 0F 59 /r MULPS xmm1, xmm2/m128
            Call MarkSSEdata SSE_4_F
            Mov W$edi+4 's '
        End_If
        add edi 6 | jmp Dis_xmm1__xmm2_m128

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op5A:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov D$edi 'edx ' | add edi 3
        Else
           ; inc D$UnLikelyCode
            Mov W$edi 'dx' | add edi 2
        End_If

    .Else
        ;inc D$UnLikelyCode
        Mov D$edi 'cvtp'
        If B$OperandSizeOverride = &TRUE    ; CVTPD2PS xmm1, xmm2/m128
            Call MarkSSEdata SSE_2_R
            Mov D$edi+4 'd2ps', B$edi+8 SPC | add edi 9 | jmp Dis_xmm1__xmm2_m128
        Else                                ; CVTPS2PD xmm1, xmm2/m64
            Call MarkSSEdata SSE_2_F
            Mov D$edi+4 's2pd', B$edi+8 SPC | add edi 9 | jmp Dis_xmm1__xmm2_m64
        End_If

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op5B:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov D$edi 'ebx ' | add edi 3
        Else
           ; inc D$UnLikelyCode
            Mov W$edi 'bx' | add edi 2
        End_If

    .Else
        ;inc D$UnLikelyCode

        If B$OperandSizeOverride = &TRUE ; CVTPS2DQ xmm1, xmm2/m128
            Call MarkSSEdata SSE_4_F
            Mov D$edi 'cvtp', D$edi+4 's2dq'
        Else                             ; CVTDQ2PS xmm1, xmm2/m128
            Call MarkSSEdata SSE_4_D
            Mov D$edi 'cvtd', D$edi+4 'q2ps'
        End_If
        Mov B$edi+8 SPC | add edi 9 | jmp Dis_xmm1__xmm2_m128

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op5C:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            Mov D$edi 'esp ' | add edi 3
        Else
            inc D$UnLikelyCode
            Mov W$edi 'sp' | add edi 2
        End_If

    .Else
        Mov D$edi 'subp'
        If B$OperandSizeOverride = &TRUE    ; 66 0F 5C /r SUBPD xmm1, xmm2/m128
            Call MarkSSEdata SSE_2_R
            Mov W$edi+4 'd '
        Else    ; 0F 5C /r SUBPS xmm1 xmm2/m128
            Call MarkSSEdata SSE_4_F
            Mov W$edi+4 's '
        End_If
        add edi 6 | jmp Dis_xmm1__xmm2_m128

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op5D:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov D$edi 'ebp ' | add edi 3
        Else
            inc D$UnLikelyCode
            Mov W$edi 'bp' | add edi 2
        End_If

    .Else
        ;inc D$UnLikelyCode
        Mov D$edi 'minp'
        If B$OperandSizeOverride = &TRUE ; MINPD xmm1, xmm2/m128
            Call MarkSSEdata SSE_2_R
            Mov W$edi+4 'd '
        Else        ; MINPS xmm1, xmm2/m128
            Call MarkSSEdata SSE_4_F
            Mov W$edi+4 's '
        End_If
        add edi 6 | jmp Dis_xmm1__xmm2_m128

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op5E:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov D$edi 'esi ' | add edi 3
        Else
            inc D$UnLikelyCode
            Mov W$edi 'si' | add edi 2
        End_If

    .Else
        ;inc D$UnLikelyCode
        Mov D$edi 'divp'
        If B$OperandSizeOverride = &TRUE    ; DIVPD xmm1, xmm2/m128
            Call MarkSSEdata SSE_2_R
            Mov W$edi+4 'd '
        Else                                ; DIVPS xmm1, xmm2/m128
            Call MarkSSEdata SSE_4_F
            Mov W$edi+4 's '
        End_If
        add edi 6 | jmp Dis_xmm1__xmm2_m128

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op5F:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov D$edi 'edi ' | add edi 3
        Else
            inc D$UnLikelyCode
            Mov W$edi 'di' | add edi 2
        End_If

    .Else
        ;inc D$UnLikelyCode
        Mov D$edi 'maxp'
        If B$OperandSizeOverride = &TRUE    ; 66 0F 5F /r MAXPD xmm1, xmm2/m128
            Call MarkSSEdata SSE_2_R
            Mov W$edi+4 'd '
        Else                                ; MAXPS xmm1, xmm2/m128
            Mov W$edi+4 's '
        End_If
        add edi 6 | jmp Dis_xmm1__xmm2_m128

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op60:
    .If B$EscapePrefix = &FALSE   ; 60 PUSHA 60 PUSHAD
        inc D$LikelyCode
        Mov D$edi 'Push', B$edi+4 'a' | add edi 5
        If B$OperandSizeOverride = &FALSE
            Mov B$edi 'd' | inc edi
        End_If

    .Else; 0F 60 /r PUNPCKLBW mm, mm/m32   ; 66 0F 60 /r PUNPCKLBW xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'punp', D$edi+4 'cklb', W$edi+8 'w ' | add edi 10
        If B$OperandSizeOverride = &TRUE
            jmp Dis_xmm1__xmm2_m128
        Else
            jmp Dis_mmx1__mmx2_m64
        End_If

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op61:
    .If B$EscapePrefix = &FALSE   ; 61 POPA     ; 61 POPAD
        inc D$LikelyCode
        Mov D$edi 'popa' | add edi 4
        If B$OperandSizeOverride = &FALSE
            Mov B$edi 'd' | inc edi
        End_If

    .Else ; 0F 61 /r PUNPCKLWD mm, mm/m32   ; 66 0F 61 /r PUNPCKLWD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'punp', D$edi+4 'cklw', W$edi+8 'd ' | add edi 10
        If B$OperandSizeOverride = &TRUE
            jmp Dis_xmm1__xmm2_m128
        Else
            jmp Dis_mmx1__mmx2_m64
        End_If
    .End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op62:
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
      ; 0F 62 /r PUNPCKLDQ mm, mm/m32   ; 66 0F 62 /r PUNPCKLDQ xmm1, xmm2/m128
        Mov D$edi 'punp', D$edi+4 'ckld', W$edi+8 'q ' | add edi 10
        If B$OperandSizeOverride = &TRUE
            jmp Dis_xmm1__xmm2_m128
        Else
            jmp Dis_mmx1__mmx2_m64
        End_If
    .Else
        inc D$UnLikelyCode
        Mov D$edi 'boun', W$edi+4 'd ' | add edi 6  ; BOUND
        jmp Dis_r32_r16__rm32_rm16
    .End_If



Op63:
    ;inc D$UnLikelyCode
    .If B$EscapePrefix = &TRUE
        Mov D$edi 'pack', D$edi+4 'sswb', B$edi+8 SPC | add edi 9
        If B$OperandSizeOverride = &TRUE    ; 66 0F 63 /r PACKSSWB xmm1, xmm2/m128
            jmp Dis_xmm1__xmm_m128
        Else        ; 0F 63 /r PACKSSWB mm1, mm2/m64
            jmp Dis_mmx1__mmx2_m64
        End_If

    .Else
        inc D$UnlikelyCode
        Mov D$edi 'arpl', B$edi+4 SPC | add edi 5 ; ARPL
        Mov B$OperandSizeOverride &TRUE             ; to avoid one more case for 16/16

        jmp Dis_rm32_rm16__r32_r16

    .End_If


Op64:
    .If B$EscapePrefix = &TRUE
      ;0F 64 /r PCMPGTB mm, mm/m64 ;66 0F 64 /r PCMPGTB xmm1, xmm2/m128
        Mov D$edi 'pcmp', D$edi+4 'gtb ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        Mov D$SegmentOverride 'fs: ', B$DisFlag DISDONE
        inc D$Prefixes

    .End_If
ret


Op65:
    .If B$EscapePrefix = &TRUE
      ; 0F 65 /r PCMPGTW mm, mm/m64   ; 66 0F 65 /r PCMPGTW xmm1, xmm2/m128
        Mov D$edi 'pcmp', D$edi+4 'gtw ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        Mov D$SegmentOverride 'gs: ', B$DisFlag DISDONE
        inc D$Prefixes
        inc D$UnLikelyCode
    .End_If
ret


Op66:
    .If B$EscapePrefix = &TRUE  ; 0F 66 /r PCMPGTD mm, mm/m64
        ;inc D$UnLikelyCode
        Mov D$edi 'pcmp', D$edi+4 'gtd ' | add edi 8
        jmp Dis_mmx1__mmx2_m64

    .Else_If W$esi = 0660F      ; 66 0F 66 /r PCMPGTD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        add esi 2
        Mov D$edi 'pcmp', D$edi+4 'gtd ' | add edi 8
        jmp Dis_xmm1__xmm2_m128

    .Else_If W$esi = 07C0F      ; 66,0F,7C,/r HADDPD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        add esi 2 | Call MarkSSEdata SSE_2_R
        Mov D$edi 'hadd', D$edi+4 'pd ' | add edi 7
        jmp Dis_xmm1__xmm2_m128

    .Else_If W$esi = 07D0F      ; 66,0F,7D,/r HSUBPD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        add esi 2 | Call MarkSSEdata SSE_2_R
        Mov D$edi 'hsub', D$edi+4 'pd ' | add edi 7
        jmp Dis_xmm1__xmm2_m128

    .Else_If W$esi = 0D00F      ; 66,0F,D0,/r ADDSUBPD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        add esi 2
        Call MarkSSEdata SSE_2_R
        Mov D$edi 'adds', D$edi+4 'ubpd', B$edi+8 SPC | add edi 9
        jmp Dis_xmm1__xmm2_m128

    .Else
        On B$OperandSizeOverride = &TRUE, inc D$UnLikelyCode
        Mov B$OperandSizeOverride &TRUE, B$DisFlag DISDONE, W$DisSizeMarker 'W$'
        inc D$Prefixes
    .End_If
ret


Op67:
    .If B$EscapePrefix = &FALSE
        On B$AddressSizeOverride = &TRUE, inc D$UnLikelyCode
        Mov B$AddressSizeOverride &TRUE, B$DisFlag DISDONE
        inc D$Prefixes

    .Else   ; 66 0F 67 /r PACKUSWB xmm1, xmm2/m128 ; 0F 67 /r PACKUSWB mm, mm/m64
        ;inc D$UnLikelyCode
        Mov D$edi 'pack', D$edi+4 'uswb', B$edi+8 SPC | add edi 9
        jmp Dis_xmmx1__xmmx2_m64_128

    .End_If
ret


Op68:
    .If B$EscapePrefix = &FALSE   ; 68 Push imm16 / 32
        inc D$LikelyCode
        If B$OperandSizeOverride = &FALSE
            Mov D$edi 'Push', B$edi+4 SPC | add edi 5
        Else
            Mov D$edi 'Push', W$edi+4 'W ' | add edi 6
        End_If
        jmp Dis_imm32_16

    .Else ; 0F 68 /r PUNPCKHBW mm, mm/m64   ; 66 0F 68 /r PUNPCKHBW xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'punp', D$edi+4 'ckhb', W$edi+8 'w ' | add edi 10
        jmp Dis_xmmx1__xmmx2_m64_128
    .End_If
ret


Op69:
    .If B$EscapePrefix = &TRUE
      ; 0F 69 /r PUNPCKHWD mm, mm/m64   ; 66 0F 69 /r PUNPCKHWD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'punp', D$edi+4 'ckhw', W$edi+8 'd ' | add edi 10
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
      ; IMUL r16,r/m16,imm16 (word register r/m16 * immediate word) (+ 32)
        inc D$LikelyCode
        Mov D$edi 'imul', B$edi+4 SPC | add edi 5
        jmp Dis_r32_r16__rm32_rm16_OrNone__SignedImm16_32
    .End_If
ret


Op6A:
    .If B$EscapePrefix = &TRUE
      ; 0F 6A /r PUNPCKHDQ mm, mm/m64   ; 66 0F 6A /r PUNPCKHDQ xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'punp', D$edi+4 'ckhd', W$edi+8 'q ' | add edi 10
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else       ; 6A Push imm8
        inc D$LikelyCode
        If B$OperandSizeOverride = &FALSE
            Mov D$edi 'Push', B$edi+4 SPC | add edi 5 | jmp Dis_Imm8
        Else
            Mov D$edi 'Push', W$edi+4 'W ' | add edi 6 | jmp Dis_Imm8
        End_If
    .End_If
ret


Op6B:
    .If B$EscapePrefix = &TRUE
      ; 66 0F 6B /r PACKSSDW xmm1, xmm2/m128 ; 0F 6B /r PACKSSDW mm1, mm2/m64
        ;inc D$UnLikelyCode
        Mov D$edi 'pack', D$edi+4 'ssdw', B$edi+8 SPC | add edi 9
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else  ; 6B /r ib > IMUL r16,r/m16,imm8
        inc D$LikelyCode
        Mov D$edi 'imul', B$edi+4 SPC | add edi 5
        jmp Dis_r32_r16__rm32_rm16_OrNone__SignedImm8

    .End_If
ret


Op6C:
    .If B$EscapePrefix = &TRUE
      ; 66 0F 6C /r PUNPCKLQDQ xmm1, xmm2/m128
        Mov D$edi 'punp', D$edi+4 'cklq', D$edi+8 'dq  ' | add edi 11
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
      ; INS m8, DX (ins 'B$es:edi dx')
        inc D$UnLikelyCode
        Mov D$edi 'insb' | add edi 4

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op6D:

    .If B$EscapePrefix = &TRUE
      ; 66 0F 6D /r PUNPCKHQDQ xmm1, xmm2/m128
        Mov D$edi 'punp', D$edi+4 'ckhq', D$edi+8 'dq  ' | add edi 11
        jmp Dis_xmm1__xmm2_m128

    .Else ; INS m32, DX +16
        inc D$UnLikelyCode
        If B$OperandSizeOverride = &TRUE
            Mov D$edi 'insw'
        Else
            Mov D$edi 'insd'
        End_If
        add edi 4

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op6E:
    .If B$EscapePrefix = &TRUE
        inc D$LikelyCode
        Mov D$edi 'movd', B$edi+4 SPC | add edi 5
        If B$OperandSizeOverride = &TRUE    ; 66 0F 6E /r MOVD xmm, r/m32
            jmp Dis_xmm_rm32
        Else         ; 0F 6E /r MOVD mm, r/m32
            jmp Dis_mmx_rm32
        End_If

    .Else       ; OUTS DX, m8
        inc D$UnLikelyCode
        Mov D$edi 'outs', B$edi+4 'B' | add edi 5

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op6F:
    .If B$EscapePrefix = &TRUE
        inc D$LikelyCode
        If B$OperandSizeOverride = &TRUE        ; MOVDQA xmm1, xmm2/m128
            Mov D$edi 'movd', D$edi+4 'qa  ' | add edi 7 | jmp Dis_xmm1__xmm2_m128
        Else        ; 0F 6F /r MOVQ mm, mm/m64
            Mov D$edi 'movq', B$edi+4 SPC | add edi 5 | jmp Dis_mmx1__mmx2_m64
        End_If

    .Else       ; OUTSW OUTSD
        inc D$UnLikelyCode
        Mov D$edi 'outs' | add edi 4
        If B$OperandSizeOverride = &TRUE
            Mov D$edi 'W'
        Else
            Mov D$edi 'D'
        End_If
        inc edi

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op70:
    .If B$EscapePrefix = &FALSE        ; JO rel8
        inc D$LikelyCode
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', W$edi+4 'o ' | add edi 6
        jmp EndWithDisByteRelative

    .Else
        ;inc D$UnLikelyCode
        Mov D$edi 'pshu'
        If B$OperandSizeOverride = &TRUE    ; 66 0F 70 /r ib PSHUFD xmm1, xmm2/m128, imm8
            Mov D$edi+4 'fd  '
            add edi 7 | Call Dis_xmm1__xmm2_m128
        Else        ; 0F 70 /r ib PSHUFW mm1, mm2/m64, imm8
            Mov D$edi+4 'fw  '
            add edi 7 | Call Dis_mm1__mm2_m128
        End_If
        Mov D$edi SPC | inc edi | Call WriteImm8

    .End_If
     Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op71:
    .If B$EscapePrefix = &FALSE        ; JNO rel8
        inc D$LikelyCode
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', D$edi+4 'no  ' | add edi 7
        jmp EndWithDisByteRelative

    .Else
        Mov bl B$esi | inc esi | DigitMask bl To al

        If al = 2   ; 0F 71 /2 ib PSRLW mm, imm8    ; 66 0F 71 /2 ib PSRLW xmm1, imm8
            Mov D$edi 'psrl'
        Else_If al = 4   ; 0F 71 /4 ib PSRAW mm, imm8   ; 66 0F 71 /4 ib PSRAW xmm1, imm8
            Mov D$edi 'psra'
        Else_If al = 6   ; 0F 71 /6 ib PSLLW mm, imm8 ; 66 0F 71 /6 ib PSLLW xmm1, imm8
            Mov D$edi 'psll'
        Else
            inc D$UnLikelyCode | dec esi | ret
        End_If
        Mov W$edi+4 'w ' | add edi 6 | jmp Dis_xmmx_imm8

    .End_If


Op72:
    .If B$EscapePrefix = &FALSE ; JB rel8 ; JNAE rel8
        inc D$LikelyCode
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', W$edi+4 'b ' | add edi 6
        jmp EndWithDisByteRelative

    .Else
        Mov bl B$esi | inc esi | DigitMask bl To al

        If al = 2      ; 0F 72 /2 ib PSRLD mm, imm8  ; 66 0F 72 /2 ib PSRLD xmm1, imm8
            Mov D$edi 'psrl'
        Else_If al = 4 ; 0F 72 /4 ib PSRAD mm, imm8  ; 66 0F 72 /4 ib PSRAD xmm1, imm8
            Mov D$edi 'psra'
        Else_If al = 6 ; 0F 72 /6 ib PSLLD mm, imm8  ; 66 0F 72 /6 ib PSLLD xmm1, imm8
            Mov D$edi 'psll'
        Else
            inc D$UnLikelyCode | dec esi | ret
        End_If
        Mov W$edi+4 'd ' | add edi 6 | jmp Dis_xmmx_imm8

    .End_If



Op73:
    ...If B$EscapePrefix = &TRUE
        Mov bl B$esi | inc esi | DigitMask bl To al

        .If al = 2   ; 0F 73 /2 ib PSRLQ mm, imm8  ; 66 0F 73 /2 ib PSRLQ xmm1, imm8
            Mov D$edi 'psrl', W$edi+4 'q ' | add edi 6
            jmp Dis_xmmx_imm8
        .Else_If al = 3    ; 66 0F 73 /3 ib PSRLDQ xmm1, imm8
            If B$OperandSizeOverride = &TRUE
                Mov D$edi 'psrl', D$edi+4 'dq  ' | add edi 7
                jmp Dis_xmm_imm8
            End_If
            inc D$UnLikelyCode
        .Else_If al = 6
              ; 0F 73 /6 ib PSLLQ mm, imm8 ; 66 0F 73 /6 ib PSLLQ xmm1, imm8
            Mov D$edi 'psll', W$edi+4 'q ' | add edi 6 | jmp Dis_xmmx_imm8
        .Else_If al = 7    ; 66 0F 73 /7 ib PSLLDQ xmm1, imm8
            If B$OperandSizeOverride = &TRUE
                Mov D$edi 'psll', D$edi+4 'dq  ' | add edi 7
                jmp Dis_xmm_imm8
            End_If
            inc D$UnLikelyCode
        .Else
            inc D$UnLikelyCode | dec esi | ret
        .End_If

    ...Else        ; JAE rel8 ; JNB rel8 ; JNC rel8
        inc D$LikelyCode
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', D$edi+4 'ae  ' | add edi 7
        jmp EndWithDisByteRelative

    ...End_If
ret


Op74:
    If B$EscapePrefix = &FALSE  ; JE rel8 ; JZ rel8
        inc D$LikelyCode
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', W$edi+4 'e ' | add edi 6
        jmp EndWithDisByteRelative

    Else
      ; 0F 74 /r PCMPEQB mm, mm/m64    ; 66 0F 74 /r PCMPEQB xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'pcmp', D$edi+4 'eqb ', B$edi+8 SPC | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    End_If


Op75:
    If B$EscapePrefix = &FALSE  ; JNE rel8 ; JNZ rel8
        inc D$LikelyCode
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', D$edi+4 'ne  ' | add edi 7
        jmp EndWithDisByteRelative

    Else
      ; 0F 75 /r PCMPEQW mm, mm/m64     ; 66 0F 75 /r PCMPEQW xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'pcmp', D$edi+4 'eqw ', B$edi+8 SPC | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    End_If


Op76:
    If B$EscapePrefix = &FALSE        ; JBE rel8 ; JNA rel8
        inc D$LikelyCode
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', D$edi+4 'be  ' | add edi 7
        jmp EndWithDisByteRelative

    Else
      ; 0F 76 /r PCMPEQD mm, mm/m64     ; 66 0F 76 /r PCMPEQD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'pcmp', D$edi+4 'eqd ', B$edi+8 SPC | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    End_If


Op77:
    If B$EscapePrefix = &FALSE        ; JA rel8 ; JNBE rel8
        inc D$LikelyCode
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', W$edi+4 'a ' | add edi 6 | jmp EndWithDisByteRelative

    Else     ; EMMS
        Mov D$edi 'emms' | add edi 4

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op78: ; JS rel8
    inc D$LikelyCode
    Mov B$CALLInstruction &TRUE
    SubEdi6 | Mov D$edi ' | j', W$edi+4 's ' | add edi 6 | jmp EndWithDisByteRelative
ret


Op79: ; JNS rel8
    inc D$LikelyCode
    Mov B$CALLInstruction &TRUE
    SubEdi6 | Mov D$edi ' | j', D$edi+4 'ns  ' | add edi 7 | jmp EndWithDisByteRelative
ret


Op7A: ; JP rel8 ; JPE rel8
    inc D$LikelyCode
    Mov B$CALLInstruction &TRUE
    SubEdi6 | Mov D$edi ' | j', W$edi+4 'p ' | add edi 6 | jmp EndWithDisByteRelative
ret


Op7B: ; JNP rel8 ; JPO rel8
    inc D$LikelyCode
    Mov B$CALLInstruction &TRUE
    SubEdi6 | Mov D$edi ' | j', D$edi+4 'np  ' | add edi 7 | jmp EndWithDisByteRelative
ret


Op7C: ; JL rel8 ; JNGE rel8
    inc D$LikelyCode
    Mov B$CALLInstruction &TRUE
    SubEdi6 | Mov D$edi ' | j', W$edi+4 'l ' | add edi 6 | jmp EndWithDisByteRelative
ret


Op7D: ; JGE rel8 ; JNL rel8
    inc D$LikelyCode
    Mov B$CALLInstruction &TRUE
    SubEdi6 | Mov D$edi ' | j', D$edi+4 'ge  ' | add edi 7 | jmp EndWithDisByteRelative
ret


Op7E:
    inc D$LikelyCode
    If B$EscapePrefix = &FALSE     ; JLE rel8 ; JNG rel8
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', D$edi+4 'le  ' | add edi 7
        jmp EndWithDisByteRelative

    Else
       ; 66 0F 7E /r MOVD r/m32, xmm ; 0F 7E /r MOVD r/m32, mm
         Mov D$edi 'movd', B$edi+4 SPC | add edi 5 | jmp Dis_rm32_xmmx

    End_If


Op7F:
    .If B$EscapePrefix = &FALSE  ; JG rel8 ; JNLE rel8
        inc D$LikelyCode
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', W$edi+4 'g ' | add edi 6
        jmp EndWithDisByteRelative

    .Else
        If B$OperandSizeOverride = &TRUE    ; 66 0F 7F /r MOVDQA xmm2/m128, xmm1
            Mov D$edi 'movd', D$edi+4 'qa  ' | add edi 7
            jmp Dis_xmm2_m128__xmm1
        Else        ; 0F 7F /r MOVQ mm/m64, mm
            Mov D$edi 'movq', B$edi+4 SPC | add edi 5
            jmp Dis_mmx1_m64__mmx2
        End_If

    .End_If


Op80:
    inc D$LikelyCode
    ..If B$EscapePrefix = &TRUE       ; 0F 80 cw/cd JO rel16/32
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', W$edi+4 'o ' | add edi 6
        jmp EndWithDisWordDwordRelative

    ..Else
        Mov bl B$esi | inc esi | DigitMask bl To al              ; ModRm with /2 ?

        .If al = 0
            Mov B$LockPrefix &FALSE
            Mov D$edi 'add ' | add edi 4
        .Else_If al = 1  ; OR r/m8,imm8
            Mov B$LockPrefix &FALSE
            Mov D$edi 'or  ' | add edi 3
        .Else_If al = 2  ; adc r/m8 imm8
            Mov B$LockPrefix &FALSE
            Mov D$edi 'adc ' | add edi 4
        .Else_If al = 3  ; 80 /3 ib SBB r/m8,imm8
            Mov B$LockPrefix &FALSE
            Mov D$edi 'sbb ' | add edi 4
        .Else_If al = 4
            Mov B$LockPrefix &FALSE
            Mov D$edi 'and ' | add edi 4
        .Else_If al = 5  ; 80 /5 ib SUB r/m8,imm8
            Mov B$LockPrefix &FALSE
            Mov D$edi 'sub ' | add edi 4
        .Else_If al = 6  ; 80 /6 ib XOR r/m8,imm8
            Mov B$LockPrefix &FALSE
            Mov D$edi 'xor ' | add edi 4
        .Else_If al = 7
            Mov D$edi 'cmp ' | add edi 4
        .End_If

        jmp Dis_rm8_imm8

    ..End_If


Op81:
    inc D$LikelyCode
    ..If B$EscapePrefix = &TRUE      ; JNO rel16/32
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', D$edi+4 'no  ' | add edi 7
        jmp EndWithDisWordDwordRelative

    ..Else
      ; adc r/m32//r/m16 imm32/imm16
        Mov bl B$esi | inc esi | DigitMask bl To al              ; ModRm with /2 ?

        .If al = 0
            Mov B$LockPrefix &FALSE
            Mov D$edi 'add ' | add edi 4
        .Else_If al = 1      ; OR r/m16,imm16 // OR r/m32,imm32
            Mov B$LockPrefix &FALSE
            Mov D$edi 'or  ' | add edi 3
        .Else_If al = 2
            Mov B$LockPrefix &FALSE
            Mov D$edi 'adc ' | add edi 4
        .Else_If al = 3  ; 81 /3 iw SBB r/m16,imm16  ; 81 /3 id SBB r/m32,imm32
            Mov B$LockPrefix &FALSE
            Mov D$edi 'sbb ' | add edi 4
        .Else_If al = 4
            Mov B$LockPrefix &FALSE
            Mov D$edi 'and ' | add edi 4
        .Else_If al = 5  ; 81 /5 iw SUB r/m16,imm16 ; 81 /5 id SUB r/m32,imm32
            Mov B$LockPrefix &FALSE
            Mov D$edi 'sub ' | add edi 4
        .Else_If al = 6  ; 81 /6 iw XOR r/m16,imm16 ; 81 /6 id XOR r/m32,imm32
            Mov B$LockPrefix &FALSE
            Mov D$edi 'xor ' | add edi 4
        .Else_If al = 7
            Mov D$edi 'cmp ' | add edi 4
        .End_If

        jmp Dis_rm32_rm16__imm32_imm16

    ..End_If


Op82:
    If B$EscapePrefix = &TRUE       ; JB rel16/32 ; JC rel16/32 ; JNAE rel16/32
        inc D$LikelyCode
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', W$edi+4 'b ' | add edi 6
        jmp EndWithDisWordDwordRelative
    End_If
ret


Op83:
    inc D$LikelyCode

    ..If B$EscapePrefix = &TRUE      ; JAE rel16/32 ; JNB rel16/32 ; JNC rel16/32
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', D$edi+4 'ae  ' | add edi 7
        jmp EndWithDisWordDwordRelative

    ..Else

        Mov bl B$esi | inc esi | DigitMask bl To al              ; ModRm with /2 ?

        .If al = 0
            Mov B$LockPrefix &FALSE
            Mov D$edi 'add ' | add edi 4
        .Else_If al = 1      ; OR r/m16,imm8 // OR r/m32,imm8
            Mov B$LockPrefix &FALSE
            Mov D$edi 'or  ' | add edi 3
        .Else_If al = 2   ; adc r/m32//r/m16 imm8
            Mov B$LockPrefix &FALSE
            Mov D$edi 'adc ' | add edi 4
        .Else_If al = 3  ; 83 /3 ib SBB r/m16,imm8   ; 83 /3 ib SBB r/m32,imm8
            Mov B$LockPrefix &FALSE
            Mov D$edi 'sbb ' | add edi 4
        .Else_If al = 4
            Mov B$LockPrefix &FALSE
            Mov D$edi 'and ' | add edi 4
        .Else_If al = 5  ; 83 /5 ib SUB r/m16,imm8 ; 83 /5 ib SUB r/m32,imm8
            Mov B$LockPrefix &FALSE
            Mov D$edi 'sub ' | add edi 4
        .Else_If al = 6  ; 83 /6 ib XOR r/m16,imm8 ; 83 /6 ib XOR r/m32,imm8
            Mov B$LockPrefix &FALSE
            Mov D$edi 'xor ' | add edi 4
        .Else_If al = 7
            Mov D$edi 'cmp ' | add edi 4
        .End_If

        jmp Dis_rm32_rm16__imm8

    ..End_If


Op84:
    inc D$LikelyCode

    If B$EscapePrefix = &TRUE       ; JE rel16/32 ; JZ rel16/32 ; JZ rel16/32
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', W$edi+4 'e ' | add edi 6
        jmp EndWithDisWordDwordRelative

    Else    ; 84 /r TEST r/m8,r8
        Mov D$edi 'test', B$edi+4 SPC | add edi 5 | jmp Dis_rm8_r8

    End_If


Op85:
    inc D$LikelyCode

    If B$EscapePrefix = &TRUE    ; JNE rel16/32 ; JNZ rel16/32
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', D$edi+4 'ne  ' | add edi 7
        jmp EndWithDisWordDwordRelative

    Else    ; 85 /r TEST r/m16,r16 ; 85 /r TEST r/m32,r32
        Mov D$edi 'test', B$edi+4 SPC | add edi 5 | jmp Dis_rm32_rm16__r32_r16

    End_If


Op86:
    inc D$LikelyCode

    If B$EscapePrefix = &TRUE    ; JBE rel16/32 ; JNA rel16/32
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', D$edi+4 'be  ' | add edi 7
        jmp EndWithDisWordDwordRelative

    Else        ; 86 /r XCHG r/m8, r8
        Mov B$LockPrefix &FALSE
        Mov D$edi 'xchg', B$edi+4 SPC | add edi 5 | jmp Dis_rm8_r8

    End_If


Op87:
    inc D$LikelyCode

    If B$EscapePrefix = &TRUE    ; 0F 87 cw/cd > JA rel16/32 ; JNBE rel16/32
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', W$edi+4 'a ' | add edi 6

        jmp EndWithDisWordDwordRelative

    Else        ; 87 /r XCHG r/m16, r16 ; 87 /r XCHG r/m32, r32 ( and reverse)
        Mov B$LockPrefix &FALSE
        Mov D$edi 'xchg', B$edi+4 SPC | add edi 5 | jmp Dis_r32_r16__rm32_rm16

    End_If


Op88:
    inc D$LikelyCode

    If B$EscapePrefix = &TRUE           ; JS rel16/32
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', D$edi+4 's ' | add edi 6
        jmp EndWithDisWordDwordRelative

    Else    ; Mov r/m8,r8
        Mov D$edi 'Mov ' | add edi 4 | jmp Dis_rm8_r8

    End_If


Op89:
    inc D$LikelyCode

    If B$EscapePrefix = &TRUE           ; JNS rel16/32
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', D$edi+4 'ns  ' | add edi 7
        jmp EndWithDisWordDwordRelative

    Else        ; Mov r/m16,r16 ; Mov r/m32,r32
        Mov D$edi 'Mov ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16

    End_If


Op8A:
    inc D$LikelyCode

    If B$EscapePrefix = &TRUE           ; JP rel16/32 ; JPE rel16/32
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', D$edi+4 'p ' | add edi 6
        jmp EndWithDisWordDwordRelative

    Else        ; Mov r8,r/m8
        Mov D$edi 'Mov ' | add edi 4 | jmp Dis_r8_rm8

    End_If


Op8B:
    inc D$LikelyCode

    If B$EscapePrefix = &TRUE           ; JNP rel16/32 ; JPO rel16/32
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', D$edi+4 'np  ' | add edi 7
        jmp EndWithDisWordDwordRelative

    Else        ; Mov r16,r/m16 ; Mov r32,r/m32
        Mov D$edi 'Mov ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16

    End_If


Op8C:
    .If B$EscapePrefix = &TRUE           ; JL rel16/32 ; JNGE rel16/32
        inc D$LikelyCode
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', D$edi+4 'l ' | add edi 6
        jmp EndWithDisWordDwordRelative

    .Else        ; Mov r/m16,Sreg** (there was a 066...)
        inc D$UnLikelyCode
        Mov D$edi 'Mov ' | add edi 4
        If B$OperandSizeOverride = &TRUE
            jmp Dis_rm16_Sreg
        Else
            jmp Dis_rm32_Sreg
        End_If
    .End_If
ret


Op8D:
    inc D$LikelyCode

    .If B$EscapePrefix = &TRUE           ; JGE rel16/32 ; JNL rel16/32
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', D$edi+4 'ge  ' | add edi 7
        jmp EndWithDisWordDwordRelative

    .Else            ; LEA r16,m
        If B$esi > 00_10_111_111      ; Never: lea eax ebx
            inc D$UnLikelyCode
            ret
        Else

        ;If B$TestNow = 1
        ;    On D$esi+1 = 04398C0, int3
        ;End_If

            Mov B$LeaInstruction &TRUE
            Mov D$edi 'lea ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16
        End_If
    .End_If


Op8E:
    .If B$EscapePrefix = &TRUE           ; JLE rel16/32 ; JNG rel16/32
        inc D$LikelyCode
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', D$edi+4 'le  ' | add edi 7
        jmp EndWithDisWordDwordRelative

    .Else        ; Mov Sreg,r/m16** (there was a 066...)
        inc D$UnLikelyCode
        Mov D$edi 'Mov ' | add edi 4
        If B$OperandSizeOverride = &TRUE
            jmp Dis_Sreg_rm16
        Else
            jmp Dis_Sreg_rm32 ;jE! ALLOWED!!
        End_If

    .End_If
ret


Op8F:
    .If B$EscapePrefix = &TRUE           ; JG rel16/32 ; JNLE rel16/32
        inc D$LikelyCode
        Mov B$CALLInstruction &TRUE
        SubEdi6 | Mov D$edi ' | j', W$edi+4 'g ' | add edi 6
        jmp EndWithDisWordDwordRelative

    .Else
        Mov bl B$esi | inc esi | DigitMask bl To al

        If al = 0       ; 8F /0 Pop m16
            Mov D$edi 'Pop ' | add edi 4 | jmp EndWithModRm
        Else
            inc D$UnLikelyCode | dec esi | ret
        End_If
    .End_If


Op90:
    If B$EscapePrefix = &FALSE  ; NOP ;  ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
        inc D$LikelyCode
        Mov D$edi 'nop ' | add edi 4

    Else     ; 0F 90 SETO r/m8
       ; inc D$UnLikelyCode
        Mov D$edi 'seto', B$edi+4 SPC | add edi 5 | jmp Dis_rm8

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op91: ; 0F 91 SETNO r/m8
    If B$EscapePrefix = &TRUE
       ; inc D$UnLikelyCode
        Mov D$edi 'setn', W$edi+4 'o ' | add edi 6 | jmp Dis_rm8

    Else    ; ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
        inc D$LikelyCode
        Mov D$edi 'xchg', B$edi+4 SPC | add edi 5 | jmp Dis_eax_ax__rd_rw

    End_If


Op92: ; 0F 92 SETB r/m8 ; 0F 92 SETC r/m8  ; 0F 92 SETNAE r/m8
    If B$EscapePrefix = &FALSE    ; ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
        inc D$LikelyCode
        Mov D$edi 'xchg', B$edi+4 SPC | add edi 5 | jmp Dis_eax_ax__rd_rw

    Else
       ; inc D$UnLikelyCode
        Mov D$edi 'setc', B$edi+4 SPC | add edi 5 | jmp Dis_rm8

    End_If


Op93:
    If B$EscapePrefix = &FALSE   ; ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
        inc D$LikelyCode
        Mov D$edi 'xchg', B$edi+4 SPC | add edi 5 | jmp Dis_eax_ax__rd_rw

    Else
       ; inc D$UnLikelyCode
      ; 0F 93 SETAE r/m8    ; 0F 93 SETNB r/m8  ; 0F 93 SETNC r/m8
        Mov D$edi 'seta', W$edi+4 'e ' | add edi 6 | jmp Dis_rm8

    End_If


Op94: ; 0F 94 SETE r/m8 ; 0F 94 SETZ r/m8
    If B$EscapePrefix = &FALSE    ; ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
        inc D$LikelyCode
        Mov D$edi 'xchg', B$edi+4 SPC | add edi 5 | jmp Dis_eax_ax__rd_rw

    Else
       ; inc D$UnLikelyCode
        Mov D$edi 'sete', B$edi+4 SPC | add edi 5 | jmp Dis_rm8

    End_If


Op95: ; 0F 95 SETNE r/m8 ; 0F 95 SETNZ r/m8
    If B$EscapePrefix = &FALSE    ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
        inc D$LikelyCode
        Mov D$edi 'xchg', B$edi+4 SPC | add edi 5 | jmp Dis_eax_ax__rd_rw

    Else
       ; inc D$UnLikelyCode
        Mov D$edi 'setn', W$edi+4 'e ' | add edi 6 | jmp Dis_rm8

    End_If


Op96:
    If B$EscapePrefix = &FALSE    ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
        inc D$LikelyCode
        Mov D$edi 'xchg', B$edi+4 SPC | add edi 5 | jmp Dis_eax_ax__rd_rw

    Else         ; 0F 96 SETBE r/m8    ; 0F 96 SETNA r/m8
       ; inc D$UnLikelyCode
        Mov D$edi 'setb', W$edi+4 'e ' | add edi 6 | jmp Dis_rm8

    End_If


Op97:
    If B$EscapePrefix = &FALSE    ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
        inc D$LikelyCode
        Mov D$edi 'xchg', B$edi+4 SPC | add edi 5 | jmp Dis_eax_ax__rd_rw

    Else         ; 0F 97 SETA r/m8 ; 0F 97 SETNBE r/m8
       ; inc D$UnLikelyCode
        Mov D$edi 'seta', B$edi+4 SPC | add edi 5 | jmp Dis_rm8

    End_If


Op98:
    .If B$EscapePrefix = &TRUE   ; 0F 98 SETS r/m8
        Mov D$edi 'sets', B$edi+4 SPC | add edi 5 | jmp Dis_rm8

    .Else ; cbw / cwde
        If B$OperandSizeOverride = &TRUE
            Mov D$edi 'cbw ' | add edi 3
        Else
            Mov D$edi 'cwde' | add edi 4
        End_If

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op99:
    .If B$EscapePrefix = &TRUE   ; 0F 99 SETNS r/m8
        Mov D$edi 'setn', W$edi+4 's ' | add edi 6 | jmp Dis_rm8

    .Else    ; CWD / CDQ
        If B$OperandSizeOverride = &FALSE
            Mov D$edi 'cdq ' | add edi 3
        Else
            Mov D$edi 'cwd ' | add edi 3
        End_If

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op9A:

    ..If B$EscapePrefix = &TRUE      ; 0F 9A SETP r/m8 ; 0F 9A SETPE r/m8
        Mov D$edi 'setp', B$edi+4 SPC | add edi 5 | jmp Dis_rm8

    ..Else
        add D$UnLikelyCode 0FF
        If B$OperandSizeOverride = &TRUE        ; Call far ptr16:selector16 ; callF16
            Mov D$edi 'Call', D$edi+4 'F W$' | add edi 8 ; jE! fix
            Push D$esi, esi
                Exchange W$esi, W$esi+2
                Call WriteImm16 | Mov B$edi SPC | inc edi | Call WriteImm16
            Pop eax, D$eax
        Else                                ;   Call far ptr32:selector16
            Mov D$edi 'Call', D$edi+4 'F D$' | add edi 8 ; jE! fix
            Push D$esi, D$esi+4, esi
                Mov eax D$esi, bx W$esi+4, W$esi bx, D$esi+2 eax
                Call WriteImm16 | Mov B$edi SPC | inc edi | Call WriteImm32
            Pop eax D$eax+4, D$eax
        End_If

        Mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8
    ..End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op9B:
    ..If B$EscapePrefix = &TRUE     ; 0F 9B SETNP r/m8 ; 0F 9B SETPO r/m8
        ;inc D$UnLikelyCode
        Mov D$edi 'setn', W$edi+4 'p ' | add edi 6 | jmp Dis_rm8

    ..Else
        .If W$esi = 0E2DB           ; 9B DB E2 > FCLEX
            Mov D$edi 'fcle', W$edi+4 'x ' | add edi 5 | add esi 2

        .Else_If W$esi = 0E3DB      ; 9B DB E3 >FINIT
            Mov D$edi 'fini', W$edi+4 't ' | add edi 5 | add esi 2

        .Else_If W$esi = 0E0DF      ; 9B DF E0 FSTSW AX
            Mov D$edi 'fsts', D$edi+4 'w ax' | add edi 8 | add esi 2

        .Else_If B$esi = 0DD
            Mov bl B$esi+1 | DigitMask bl to al

            If al = 6               ; 9B DD /6 > FSAVE m94/108byte
                Mov D$edi 'fsav', W$edi+4 'e ' | add edi 6 | inc esi | lodsb
                jmp EndWith.X.mem
            Else_If al = 7          ; 9B DD /7 FSTSW m2byte
                Mov D$edi 'fsts', W$edi+4 'w ' | add edi 6 | inc esi | lodsb
                jmp EndWith.W.mem
            Else
                jmp L5>
            End_If

        .Else_If B$esi = 0D9 ; opD9
            Mov bl B$esi+1 | DigitMask bl to al

            If al = 6               ; 9B D9 /6 FSTENV m14/28byte
                Mov D$edi 'fste', D$edi+4 'nv  ' | add edi 7 | inc esi | lodsb
                jmp EndWith.X.mem
            Else_If al = 7          ; 9B DD /6 > FSTCW m2byte
                Mov D$edi 'fstc', W$edi+4 'w ' | add edi 6 | inc esi | lodsb
                jmp EndWith.W.mem
            Else
                jmp L5>
            End_If

        .Else                       ; 9B WAIT
L5:         Mov D$edi 'wait' | add edi 4
        .End_If

    ..End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op9C: ; setl

    .If B$EscapePrefix = &TRUE   ; 0F 9C SETL r/m8 ; 0F 9C SETNGE r/m8
        Mov D$edi 'setl', B$edi+4 SPC | add edi 5 | jmp Dis_rm8

    .Else       ; 9C PUSHF 9C PUSHFD
        ;inc D$UnLikelyCode
        Mov D$edi 'Push', B$edi+4 'f' | add edi 5
        If B$OperandSizeOverride = &TRUE
            Mov B$edi 'd' | inc edi
        End_If

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op9D:

    .If B$EscapePrefix = &TRUE   ; 0F 9D SETGE r/m8 ; 0F 9D SETNL r/m8
        Mov D$edi 'setg', W$edi+4 'e ' | add edi 6 | jmp Dis_rm8

    .Else    ; 9D POPF  ; 9D POPFD
        ;inc D$UnLikelyCode
        Mov D$edi 'popf' | add edi 4
        If B$OperandSizeOverride = &TRUE
            Mov B$edi 'd' | inc edi
        End_If

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op9E:
    .If B$EscapePrefix = &TRUE   ; 0F 9E SETLE r/m8 ; 0F 9E SETNG r/m8
        Mov D$edi 'setl', W$edi+4 'e ' | add edi 6 | jmp Dis_rm8

    .Else       ; 9E SAHF
        ;inc D$UnLikelyCode
        Mov D$edi 'sahf' | add edi 4

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Op9F:

    If B$EscapePrefix = &TRUE   ; 0F 9F SETG r/m8 ; 0F 9F SETNLE r/m8
        Mov D$edi 'setg', B$edi+4 SPC | add edi 5 | jmp Dis_rm8

    Else    ; LAHF
        ;inc D$UnLikelyCode
        Mov D$edi 'lahf' | add edi 4

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA0:
    If B$EscapePrefix = &TRUE   ; 0F A0 Push FS
        inc D$UnLikelyCode
        Mov D$edi 'Push', D$edi+4 ' fs ' | add edi 7

    Else ; Mov AL,moffs8*
        Mov D$edi 'Mov ', D$edi+4 'al B', B$edi+8 '$' | add edi 9 | Call WriteImm32

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret



OpA1:
    .If B$EscapePrefix = &TRUE ; 0F A1 Pop FS
        inc D$UnLikelyCode
        Mov D$edi 'Pop ', W$edi+4 'fs' | add edi 6

    .Else        ; Mov EAX,moffs32* ; Mov AX,moffs16*
        Mov D$edi 'Mov ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            Mov D$edi 'eax ', W$edi+4 'D$' | add edi 6
        Else
            Mov D$edi 'ax W', B$edi+4 '$' | add edi 5
        End_If
        If D$SegmentOverride <> 0
            Move D$edi D$SegmentOverride | add edi 3
        End_If
        Call WriteDis32 ; WriteImm32

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA2:
    If B$EscapePrefix = &TRUE ; CPUID
        Mov D$edi 'cpui', B$edi+4 'd' | add edi 5

    Else        ; Mov moffs8*,AL
        Mov D$edi 'Mov ', D$edi+4 'B$  ' | add edi 6
        Call WriteImm32 | Mov D$edi ' al ' | add edi 3

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA3:
    .If B$EscapePrefix = &TRUE
        Mov D$edi 'bt  ' | add edi 3 | jmp Dis_rm32_rm16__r32_r16

    .Else        ; Mov moffs32*,EAX ; Mov moffs16*,AX
        Mov D$edi 'Mov '
        If B$OperandSizeOverride = &FALSE
            Mov D$edi+4 'D$  '
        Else
            Mov D$edi+4 'W$  '
        End_If
        add edi 6
        If D$SegmentOverride <> 0
            Move D$edi D$SegmentOverride | add edi 3
        End_If

        Call WriteDis32

        If B$OperandSizeOverride = &FALSE
            Mov D$edi ' eax' | add edi 4
        Else
            Mov D$edi ' ax ' | add edi 3
        End_If

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA4:
    inc  D$LikelyCode
    If B$EscapePrefix = &TRUE   ; 0F A4 SHLD r/m16, r16, imm8 ; 0F A4 SHLD r/m32, r32, imm8
        Mov D$edi 'shld', B$edi+4 SPC | add edi 5 | jmp Dis_rm32_rm16__r32_r16__imm8

    Else ; MOVSB
        Mov D$edi 'movs', B$edi+4 'b' | add edi 5

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA5:
    inc D$LikelyCode
    .If B$EscapePrefix = &TRUE   ; 0F A5 SHLD r/m16, r16, CL ; 0F A5 SHLD r/m32, r32, CL
        Mov D$edi 'shld', B$edi+4 SPC | add edi 5 | jmp Dis_rm32_rm16__r32_r16__cl

    .Else
        If B$OperandSizeOverride = &FALSE  ; MOVSD
            Mov D$edi 'movs', B$edi+4 'd'
        Else  ; MOVSW
            Mov D$edi 'movs', B$edi+4 'w'
        End_If
        add edi 5

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA6: ; cmpsb
    inc D$LikelyCode
    Mov D$edi 'cmps', B$edi+4 'b' | add edi 5
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA7: ; cmpsw / d
    inc D$LikelyCode
    Mov D$edi 'cmps' | add edi 4
    If B$OperandSizeOverride = &TRUE
        Mov B$edi 'w'
    Else
        Mov B$edi 'd'
    End_If
    inc edi
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA8:
    If B$EscapePrefix = &TRUE   ; 0F A8 Push GS
        inc D$UnLikelyCode
        Mov D$edi 'Push', D$edi+4 ' gs ' | add edi 7

    Else    ; A8 ib TEST AL,imm8
        inc D$LikelyCode
        Mov D$edi 'test', B$edi+4 SPC | add edi 5 | jmp Dis_al_imm8

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA9: ; 0F A9 Pop GS
    If B$EscapePrefix = &TRUE
        inc D$UnLikelyCode
        Mov D$edi 'Pop ', W$edi+4 'gs' | add edi 6

    Else   ; A9 iw TEST AX,imm16 ; A9 id TEST EAX,imm32
        inc D$LikelyCode
        Mov D$edi 'test', B$edi+4 SPC | add edi 5 | jmp Dis_eax_ax__imm32_imm16

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpAA:
    If B$EscapePrefix = &TRUE    ; 0F AA RSM
        inc D$UnLikelyCode
        Mov D$edi 'rsm ' | add edi 3

    Else       ; AA STOSB
        inc D$LikelyCode
        Mov D$edi 'stos', B$edi+4 'b' | add edi 5

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpAB:
    If B$EscapePrefix = &TRUE
        Mov B$LockPrefix &FALSE
        Mov D$edi 'bts ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16

    Else        ; AB STOSW ; AB STOSD
        inc D$LikelyCode
        Mov D$edi 'stos', B$edi+4 'd' | add edi 5
        On B$OperandSizeOverride = &TRUE, Mov B$edi-1 'w'

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpAC:
    If B$EscapePrefix = &TRUE   ; 0F AC SHRD r/m16, r16, imm8 ; 0F AC SHRD r/m32, r32, mm8
        Mov D$edi 'shrd', B$edi+4 SPC | add edi 5 | jmp Dis_rm32_rm16__r32_r16__imm8

    Else ; LODSB
        inc D$LikelyCode
        Mov D$edi 'lods', B$edi+4 'b' | add edi 5

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpAD:
    .If B$EscapePrefix = &TRUE   ; 0F AD SHRD r/m16, r16, CL ; 0F AD SHRD r/m32, r32, CL
        Mov D$edi 'shrd', B$edi+4 SPC | add edi 5 | jmp Dis_rm32_rm16__r32_r16__cl

    .Else    ; LODSD ; LODSW
        inc D$LikelyCode
        Mov D$edi 'lods', B$edi+4 'd'
        On B$OperandSizeOverride = &TRUE, Mov B$edi+4 'w'
        add edi 5

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpAE:
    ..If B$EscapePrefix = &TRUE
        movzx ebx B$esi | inc esi | DigitMask bl to al

        .If al = 0          ; FXSAVE m512byte
            Mov D$edi 'fxsa', D$edi+4 've  ' | add edi 7 | jmp EndWith.X.mem
        .Else_If al = 1     ; FXRSTOR m512byte
            Mov D$edi 'fxrs', D$edi+4 'tor ' | add edi 8 | jmp EndWith.X.mem
        .Else_If al = 2     ; LDMXCSR m32
            Mov D$edi 'ldmx', D$edi+4 'csr ' | add edi 8 | jmp EndWith.D.mem
        .Else_If al = 3     ; 0F AE /3 STMXCSR m32
            Mov D$edi 'stmx', D$edi+4 'csr ' | add edi 8 | jmp EndWith.D.mem
        .Else_If al = 5     ; LFENCE
            Mov D$edi 'lfen', W$edi+4 'ce' | add edi 6
        .Else_If al = 6     ; MFENCE
            Mov D$edi 'mfen', W$edi+4 'ce' | add edi 6
        .Else_If al = 7     ; 0F AE /7 CLFLUSH   ; 0F AE /7 SFENCE
            ModMask bl to al
            If al = 3
                Mov D$edi 'sfen', D$edi+4 'ce  '  | add edi 6
            Else
                Mov D$edi 'clfl', D$edi+4 'ush ' | add edi 8 | jmp Dis_m8
            End_If
        .Else
            dec esi | ret
        .End_If

    ..Else  ; AE SCASB
        Mov D$edi 'scas', B$edi+4 'b' | add edi 5
    ..End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpAF:  ; 0F AF /r > IMUL r16,r/m16 ou 32
    inc D$LikelyCode
    If B$EscapePrefix = &TRUE
        Mov D$edi 'imul', B$edi+4 SPC | add edi 5 | jmp Dis_r32_r16__rm32_rm16

    Else        ; AF SCASD AF SCASW
         Mov D$edi 'scas', B$edi+4 'd' | add edi 5
         On B$OperandSizeOverride = &TRUE, Mov B$edi-1 'w'

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB0:
    If B$EscapePrefix = &FALSE  ; Mov r8,imm8
        inc D$LikelyCode
        Mov D$edi 'Mov ', D$edi+4 'al  ' | add edi 7 | Call WriteImm8

    Else     ; CMPXCHG r/m8,r8
        ;inc D$UnLikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'cmpx', D$edi+4 'chg ' | add edi 8 | jmp Dis_rm8_r8

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB1: ; CMPXCHG r/m32,r32 (+16)
    If B$EscapePrefix = &FALSE
        Mov D$edi 'Mov ', D$edi+4 'cl  ' | add edi 7 | Call WriteImm8

    Else
        Mov B$LockPrefix &FALSE
        Mov D$edi 'cmpx', D$edi+4 'chg ' | add edi 8 | jmp Dis_rm32_rm16__r32_r16

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB2: ; 0F B2 > LSS r16,m16:16 ; LSS r32,m16:32
    If B$EscapePrefix = &FALSE
        Mov D$edi 'Mov ', D$edi+4 'dl  ' | add edi 7 | Call WriteImm8

    Else
        inc D$UnLikelyCode
        Mov D$edi 'lss ' | add edi 4 | Call Dis_r32_r16__rm32_rm16 ; LSL
        Mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB3:
    If B$EscapePrefix = &FALSE
        Mov D$edi 'Mov ', D$edi+4 'bl  ' | add edi 7 | Call WriteImm8

    Else
        ;inc D$UnLikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'btr ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16 ; BTR

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB4:
    If B$EscapePrefix = &FALSE
        inc D$LikelyCode
        Mov D$edi 'Mov ', D$edi+4 'ah  ' | add edi 7 | Call WriteImm8

    Else     ; 0F B4 > LFS r16,m16:16 ; LFS r32,m16:32
        inc D$UnLikelyCode
        Mov D$edi 'lfs ' | add edi 4 | Call Dis_r32_r16__rm32_rm16
        Mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB5:
    If B$EscapePrefix = &FALSE
        Mov D$edi 'Mov ', D$edi+4 'ch  ' | add edi 7 | Call WriteImm8

    Else     ; LGS r16,m16:16 ; LGS r32,m16:32
        inc D$UnLikelyCode
        Mov D$edi 'lgs ' | add edi 4 | Call Dis_r32_r16__rm32_rm16
        Mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB6:
    If B$EscapePrefix = &FALSE
        Mov D$edi 'Mov ', D$edi+4 'dh  ' | add edi 7 | Call WriteImm8

    Else        ; 0F B6 /r MOVZX r16,r/m8 ; 0F B6 /r MOVZX r32,r/m8
        inc D$LikelyCode
        Mov D$edi 'movz', W$edi+4 'x ' | add edi 6 | jmp Dis_r32_r16__rm8

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB7:
    If B$EscapePrefix = &FALSE
        Mov D$edi 'Mov ', D$edi+4 'bh  ' | add edi 7 | Call WriteImm8

    Else        ; 0F B7 /r MOVZX r32,r/m16
        inc D$LikelyCode
        Mov D$edi 'movz', W$edi+4 'x ' | add edi 6 | jmp Dis_r32_rm16

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB8:
    inc D$LikelyCode
    If B$OperandSizeOverride = &FALSE
        Mov D$edi 'Mov ', D$edi+4 'eax ' | add edi 8 | Call WriteImm32
    Else
        Mov D$edi 'Mov ', D$edi+4 'ax  ' | add edi 7 | Call WriteImm16
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB9:
    inc D$LikelyCode
    If B$OperandSizeOverride = &FALSE
        Mov D$edi 'Mov ', D$edi+4 'ecx ' | add edi 8 | Call WriteImm32

    Else
        Mov D$edi 'Mov ', D$edi+4 'cx  ' | add edi 7 | Call WriteImm16

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpBA:
    .If B$EscapePrefix = &TRUE
        movzx ebx B$esi | inc esi | DigitMask bl to al

        If al = 4
            Mov D$edi 'bt  ' | add edi 3 ; bts, ...
        Else_If al = 5
            Mov B$LockPrefix &FALSE
            Mov D$edi 'bts ' | add edi 4
        Else_If al = 6
            Mov B$LockPrefix &FALSE
            Mov D$edi 'btr ' | add edi 4
        Else_If al = 7
            Mov B$LockPrefix &FALSE
            Mov D$edi 'btc ' | add edi 4
        Else
            dec esi | ret
        End_If
        jmp Dis_rm32_rm16__imm8

    .Else
        inc D$LikelyCode
        If B$OperandSizeOverride = &FALSE
            Mov D$edi 'Mov ', D$edi+4 'edx ' | add edi 8 | Call WriteImm32
        Else
            Mov D$edi 'Mov ', D$edi+4 'dx  ' | add edi 7 | Call WriteImm16
        End_If

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpBB:
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'btc ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16 ; BTC

    .Else
        inc D$LikelyCode
        If B$OperandSizeOverride = &FALSE
            Mov D$edi 'Mov ', D$edi+4 'ebx ' | add edi 8 | Call WriteImm32
        Else
            Mov D$edi 'Mov ', D$edi+4 'bx  ' | add edi 7 | Call WriteImm16
        End_If

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpBC:
    .If B$EscapePrefix = &TRUE
        Mov D$edi 'bsf ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16 ; bsf

    .Else
        If B$OperandSizeOverride = &FALSE
            Mov D$edi 'Mov ', D$edi+4 'esp ' | add edi 8 | Call WriteImm32
        Else
            inc D$UnLikelyCode
            Mov D$edi 'Mov ', D$edi+4 'sp  ' | add edi 7 | Call WriteImm16
        End_If

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpBD:
    .If B$EscapePrefix = &TRUE
        Mov D$edi 'bsr ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16 ; bsr

    .Else
        If B$OperandSizeOverride = &FALSE
            Mov D$edi 'Mov ', D$edi+4 'ebp ' | add edi 8 | Call WriteImm32
        Else
            inc D$UnLikelyCode
            Mov D$edi 'Mov ', D$edi+4 'bp  ' | add edi 7 | Call WriteImm16
        End_If

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpBE:
    inc D$LikelyCode
    .If B$EscapePrefix = &FALSE
        If B$OperandSizeOverride = &FALSE
            Mov D$edi 'Mov ', D$edi+4 'esi ' | add edi 8 | Call WriteImm32
        Else
            Mov D$edi 'Mov ', D$edi+4 'si  ' | add edi 7 | Call WriteImm16
        End_If

    .Else
      ; 0F BE ¯r MOVSX r32,r/m8  ; 0F BE /r MOVSX r16,r/m8
        Mov D$edi 'movs', W$edi+4 'x ' | add edi 6 | jmp Dis_r32_r16__rm8

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpBF:
    inc D$LikelyCode
    .If B$EscapePrefix = &FALSE
        If B$OperandSizeOverride = &FALSE
            Mov D$edi 'Mov ', D$edi+4 'edi ' | add edi 8
            Call WriteImm32
        Else
            Mov D$edi 'Mov ', D$edi+4 'di  ' | add edi 7
            Call WriteImm16
        End_If

    .Else       ; 0F BF /r MOVSX r32,r/m16
        Mov D$edi 'movs', W$edi+4 'x ' | add edi 6 | jmp Dis_r32_rm16

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpC0:
    .If B$EscapePrefix = &FALSE
        inc D$LikelyCode
        Mov bl B$esi | inc esi | DigitMask bl to al

        If al = 0   ; C0 /0 ib ROL r/m8, imm8
            Mov D$edi 'rol '
        Else_If al = 1  ; C0 /1 ib ROR r/m8, imm8
            Mov D$edi 'ror '
        Else_If al = 2  ; C0 /2 ib RCL r/m8, imm8
            Mov D$edi 'rcl '
        Else_If al = 3  ; C0 /3 ib RCR r/m8, imm8
            Mov D$edi 'rcr '
        Else_If al = 4   ; C0 /4 ib SAL r/m8,imm8   ; C0 /4 ib SHL r/m8,imm8
            Mov D$edi 'shl '
        Else_If al = 5  ; C0 /5 ib SHR r/m8,imm8
           Mov D$edi 'shr '
        Else_If al = 7  ; C0 /7 ib SAR r/m8,imm8
            Mov D$edi 'sar '
        Else
            dec esi | ret
        End_If

        add edi 4 | jmp Dis_rm8_imm8

    .Else   ; 0F C0 /r XADD r/m8, r8
        Mov B$LockPrefix &FALSE
        Mov D$edi 'xadd', B$edi+4 SPC | add edi 5 | jmp Dis_rm8_r8

    .End_If


OpC1:
    .If B$EscapePrefix = &FALSE
        inc D$LikelyCode
        movzx ebx B$esi | inc esi | DigitMask bl to al

        If al = 0   ; C1 /0 ib ROL r/m16, imm8  C1 /0 ib ROL r/m32, imm8
            Mov D$edi 'rol '
        Else_If al = 1    ; C1 /1 ib ROR r/m16, imm8    ; C1 /1 ib ROR r/m32, imm8
            Mov D$edi 'ror '
        Else_If al = 2    ; C1 /2 ib RCL r/m16, imm8 ; C1 /2 ib RCL r/m32,i mm8
            Mov D$edi 'rcl '
        Else_If al = 3  ; C1 /3 ib RCR r/m16, imm8  C1 /3 ib RCR r/m32, imm8
            Mov D$edi 'rcr '
        Else_If al = 4  ; C1 /4 ib SAL r/m16,imm8   ; C1 /4 ib SAL r/m32,imm8 ; C1 /4 ib SHL r/m32,imm8  ; C1 /4 ib SHL r/m16,imm8
            Mov D$edi 'shl '
        Else_If al = 5  ; C1 /5 ib SHR r/m16,imm8   ; C1 /5 ib SHR r/m32,imm8
            Mov D$edi 'shr '
        Else_If al = 7  ; C1 /7 ib SAR r/m16,imm8   ; C1 /7 ib SAR r/m32,imm8
            Mov D$edi 'sar '
        Else
            dec esi | ret
        End_If

        add edi 4 | jmp Dis_rm32_rm16__imm8

    .Else   ; 0F C1 /r XADD r/m16, r16 ; 0F C1 /r XADD r/m32, r32
        Mov B$LockPrefix &FALSE
        Mov D$edi 'xadd', B$edi+4 SPC | add edi 5 | jmp Dis_rm32_rm16__r32_r16

    .End_If


OpC2:
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        Mov D$edi 'cmp_'
        If B$OperandSizeOverride = &TRUE
          ; 66 0F C2 /r ib CMPPD xmm1, xmm2/m128, imm8
          ; 66 0F C2 _1D_ 010 34 42 00 01
            Call MarkSSEdata SSE_2_R    ; CMPPD 66 0F C2 /r ib
            Mov D$edi+4 'pd  '
        Else
          ; cmpps xmm1, xmm2/m128, imm8
            Call MarkSSEdata SSE_4_F    ; CMPPS 0F C2 /r ib
            Mov D$edi+4 'ps  '
        End_If

        add edi 7
        Call Dis_xmm1__xmm2_m128 | jmp WritePacketCondition

    .Else       ; C2 iw RET imm16
        inc D$LikelyCode
        Mov D$edi 'ret ' | add edi 4 | Call WriteImm16
        Mov al CR | stosb | Mov al LF | stosb
        Mov B$DisEndOfChunk &TRUE

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpC3:
    If B$EscapePrefix = &FALSE  ; C3 RET
        inc D$LikelyCode
        Mov D$edi 'ret ' | add edi 3
        Mov al CR | stosb | Mov al LF | stosb
        Mov B$DisEndOfChunk &TRUE

    Else        ; 0F C3 /r MOVNTI m32, r32
        ;inc D$UnLikelyCode
        Mov D$edi 'movn', D$edi+4 'ti  ' | add edi 7 | jmp Dis_m32_r32

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpC4:
    .If B$EscapePrefix = &TRUE
      ; 66 0F C4 /r ib PINSRW xmm, r32/m16, imm8 ; 0F C4 /r ib PINSRW mm, r32/m16, imm8
        Mov D$edi 'pins', D$edi+4 'rw  ' | add edi 7
        jmp Dis_PINSRW

    .Else        ; LES r16,m16:16 ; LES r32,m16:32
        Mov D$edi 'les ' | add edi 4 | Call Dis_r32_r16__rm32_rm16
        Mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8
        inc D$UnLikelyCode

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpC5:
    .If B$EscapePrefix = &TRUE
      ; 66 0F C5 /r ib PEXTRW r32, xmm, imm8 ; 0F C5 /r ib PEXTRW r32, mm, imm8
        Mov D$edi 'pext', D$edi+4 'rw  ' | add edi 7
        jmp Dis_r32_xmmx_imm8
       ; Call Dis_r32_xmmx | Mov B$edi SPC | inc edi | Call Writeimm8
;Dis_xmmx_r32
    .Else        ; LDS r16,m16:16
        inc D$UnLikelyCode
        Mov D$edi 'lds ' | add edi 4 | Call Dis_r32_r16__rm32_rm16
        Mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpC6:
    ..If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
      ; 66 0F C6 /r ib SHUFPD xmm1, xmm2/m128, imm8 ; 0F C6 /r ib SHUFPS xmm1, xmm2/m128, imm8
        Mov D$edi 'shuf'
        If B$OperandSizeOverride = &TRUE
            Call MarkSSEdata SSE_2_R
            Mov D$edi+4 'pd  '
        Else
            Call MarkSSEdata SSE_4_F
            Mov D$edi+4 'ps  '
        End_If
        add edi 7 | jmp Dis_xmm1__xmm2_m128__imm8

    ..Else
        Mov bl B$esi | inc esi | DigitMask bl To al

        If al = 0     ; Mov r/m8,imm8
            inc D$LikelyCode
            Mov D$edi 'Mov ' | add edi 4 | jmp Dis_rm8_imm8
        Else
            dec esi | ret
        End_If

    ..End_If


OpC7:
    movzx ebx B$esi | inc esi | DigitMask bl to al

    .If B$EscapePrefix = &FALSE
        inc D$LikelyCode
        If al = 0       ; Mov r/m32,imm32 ; Mov r/m16,imm16
            Mov B$MovOrJmpImmInstruction &TRUE
            Mov D$edi 'Mov ' | add edi 4 | jmp Dis_rm32_rm16__imm32_imm16
        End_If

    .Else    ; 0F C7 /1 m64 CMPXCHG8B m64
        If al = 1
            ;inc D$UnLikelyCode
            Mov B$LockPrefix &FALSE
            Mov D$edi 'cmpx', D$edi+4 'chg8', W$edi+8 'b ' | add edi 10 | jmp Dis_m64
        End_If

    .End_If

    dec esi
ret


OpC8:
    .If B$EscapePrefix = &FALSE ; ENTER imm16,imm8
        Mov D$edi 'ente', W$edi+4 'r ' | add edi 6
      ; Enter Max is 0FFFC and must be 4 Bytes Aligned:
        If W$esi > 0FFFC
            add D$UnlikelyCode 5
        Else
            test W$esi 00_11 ZERO L1>
                add D$UnlikelyCode 5
        End_If

L1:     On B$esi+2 > 31, add D$UnlikelyCode 5
        Call WriteImm16 | Mov B$edi SPC | inc edi
        Call WriteImm8

    .Else
        Mov D$edi 'bswa', W$edi+4 'p ', D$edi+6 'eax ' | add edi 9

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpC9:
    If B$EscapePrefix = &FALSE
        Mov D$edi 'leav', B$edi+4 'e' | add edi 5

    Else
        Mov D$edi 'bswa', W$edi+4 'p ', D$edi+6 'ecx ' | add edi 9

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpCA:
    If B$EscapePrefix = &FALSE        ; CA iw RET imm16
        inc D$UnLikelyCode
        Mov D$edi 'retf', B$edi+4 SPC | add edi 5 | Call WriteImm16
        Mov B$DisEndOfChunk &TRUE

    Else
        Mov D$edi 'bswa', W$edi+4 'p ', D$edi+6 'edx ' | add edi 9

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpCB:
    If B$EscapePrefix = &FALSE        ; CB RET
        inc D$UnLikelyCode
        Mov D$edi 'retF' | add edi 4
        Mov B$DisEndOfChunk &TRUE

    Else
        Mov D$edi 'bswa', W$edi+4 'p ', D$edi+6 'ebx ' | add edi 9

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpCC:
    .If B$EscapePrefix = &TRUE
        inc D$UnLikelyCode
        Mov D$edi 'bswa', W$edi+4 'p ', D$edi+6 'esp ' | add edi 9

    .Else        ; INT 3
        Mov D$edi 'int ', B$edi+4 '3' | add edi 5
        If B$No0CC = &FALSE
            Mov B$DisEndOfChunk &TRUE
        Else
            inc D$UnLikelyCode
        End_If

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpCD:
    If B$EscapePrefix = &TRUE
        Mov D$edi 'bswa', W$edi+4 'p ', D$edi+6 'ebp ' | add edi 9

    Else        ; INT imm8
        inc D$UnLikelyCode
        Mov D$edi 'int ' | add edi 4 | Call WriteImm8

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpCE:
    If B$EscapePrefix = &TRUE
        Mov D$edi 'bswa', W$edi+4 'p ', D$edi+6 'esi ' | add edi 9

    Else        ; INTO
        inc D$UnLikelyCode
        Mov D$edi 'into' | add edi 4

    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpCF:
    .If B$EscapePrefix = &TRUE
        Mov D$edi 'bswa', W$edi+4 'p ', D$edi+6 'edi ' | add edi 9

    .Else       ; IRET IRETD
        inc D$UnLikelyCode
        Mov D$edi 'iret' | add edi 4
        If B$OperandSizeOverride = &FALSE
            Mov B$edi 'd' | inc edi
        End_If

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpD0:
    inc D$LikelyCode
    Mov bl B$esi | inc esi | DigitMask bl To al

    If al = 0       ; D0 /0 ROL r/m8, 1
        Mov D$edi 'rol '
    Else_If al = 1  ; D0 /1 ROR r/m8, 1
        Mov D$edi 'ror '
    Else_If al = 2  ; D0 /2 RCL r/m8, 1
        Mov D$edi 'rcl '
    Else_If al = 3  ; D0 /3 RCR r/m8, 1
        Mov D$edi 'rcr '
    Else_If al = 4  ; D0 /4 SAL r/m8,1  ; D0 /4 SHL r/m8,1
        Mov D$edi 'shl '
    Else_If al = 5  ; D0 /5 SHR r/m8,1
        Mov D$edi 'shr '
    Else_If al = 7  ; D0 /7 SAR r/m8,1
        Mov D$edi 'sar '
    Else
        dec esi | ret
    End_If

    add edi 4 | jmp Dis_rm8_1



OpD1:
    .If B$EscapePrefix = &TRUE ; Op0F
        ;inc D$UnLikelyCode
      ; 0F D1 /r PSRLW mm, mm/m64       ; 66 0F D1 /r PSRLW xmm1, xmm2/m128
        If B$esi-3 <> 0F
            Mov D$edi 'psrl', W$edi+4 'w ' | add edi 6
            jmp Dis_xmmx1__xmmx2_m64_128
        End_If

    .Else
        inc D$LikelyCode
        Mov bl B$esi | inc esi | DigitMask bl To al

        If al = 0   ; D1 /0 ROL r/m16, 1    D1 /0 ROL r/m32, 1
            Mov D$edi 'rol '
        Else_If al = 1   ; D1 /1 ROR r/m16, 1   ; D1 /1 ROR r/m32, 1
            Mov D$edi 'ror '
        Else_If al = 2   ; D1 /2 RCL r/m16, 1 D1 /2 RCL r/m32, 1
            Mov D$edi 'rcl '
        Else_If al = 3  ; D1 /3 RCR r/m16, 1    ; D1 /3 RCR r/m32, 1
            Mov D$edi 'rcr '
        Else_If al = 4  ; D1 /4 SAL r/m16,1 ; D1 /4 SAL r/m32,1 ; D1 /4 SHL r/m16,1; D1 /4 SHL r/m32,1
            Mov D$edi 'shl '
        Else_If al = 5  ; D1 /5 SHR r/m16,1 ; D1 /5 SHR r/m32,1
            Mov D$edi 'shr '
        Else_If al = 7  ; D1 /7 SAR r/m16,1 ; D1 /7 SAR r/m32,1
            Mov D$edi 'sar '
        Else
            dec esi | ret
        End_If

        add edi 4 | jmp Dis_rm32_rm16__1

    .End_If


OpD2:
    .If B$EscapePrefix = &TRUE
      ; 0F D2 /r PSRLD mm, mm/m64   ; 66 0F D2 /r PSRLD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'psrl', W$edi+4 'd ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        inc D$LikelyCode
        Mov bl B$esi | inc esi | DigitMask bl to al

        If al = 0       ; D2 /0 ROL r/m8, CL
            Mov D$edi 'rol '
        Else_If al = 1      ;     D2 /1 ROR r/m8, CL
             Mov D$edi 'ror '
        Else_If al = 2      ; D2 /2 RCL r/m8, CL
            Mov D$edi 'rcl '
        Else_If al = 3 ; D2 /3 RCR r/m8, CL
            Mov D$edi 'rcr '
        Else_If al = 4   ; D2 /4 SAL r/m8,CL    ; D2 /4 SHL r/m8,CL
            Mov D$edi 'shl '
        Else_If al = 5  ; D2 /5 SHR r/m8,CL
            Mov D$edi 'shr '
        Else_If al = 7  ; D2 /7 SAR r/m8,CL
            Mov D$edi 'sar '
        Else
            dec esi | ret
        End_If

        add edi 4 | jmp Dis_rm8_cl

    .End_If


OpD3:
    .If B$EscapePrefix = &TRUE
      ; 0F D3 /r PSRLQ mm, mm/m64   ; 66 0F D3 /r PSRLQ xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'psrl', W$edi+4 'q ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        inc D$LikelyCode
        Mov bl B$esi | inc esi | DigitMask bl to al

        If al = 0   ; D3 /0 ROL r/m16, CL   D3 /0 ROL r/m32, CL
            Mov D$edi 'rol '
        Else_If al = 1      ; D3 /1 ROR r/m16, CL    ; D3 /1 ROR r/m32, CL
            Mov D$edi 'ror '
        Else_If al = 2      ; D3 /2 RCL r/m16, CL    D3 /2 RCL r/m32, CL
            Mov D$edi 'rcl '
        Else_If al = 3  ; D3 /3 RCR r/m16, CL   D3 /3 RCR r/m32, CL
            Mov D$edi 'rcr '
        Else_If al = 4  ; D3 /4 SAL r/m16,CL    ; D3 /4 SAL r/m32,CL  ;D3 /4 SHL r/m32,CL  ; D3 /4 SHL r/m16,CL
            Mov D$edi 'shl '
        Else_If al = 5  ; D3 /5 SHR r/m16,CL    ; D3 /5 SHR r/m32,CL
            Mov D$edi 'shr '
        Else_If al = 7  ; D3 /7 SAR r/m16,CL    ; D3 /7 SAR r/m32,CL
            Mov D$edi 'sar '
        Else
            dec esi | ret
        End_If

        add edi 4 | jmp Dis_rm32_rm16__cl

    .End_If


OpD4:
    .If B$EscapePrefix = &TRUE
        ; 66 0F D4 /r PADDQ xmm1,xmm2/m128       ; 0F D4 /r PADDQ mm1,mm2/m64
        ;inc D$UnLikelyCode
        Mov D$edi 'padd', W$edi+4 'q ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        Mov D$edi 'aam ', B$DisFlag DISDONE+DISLINEOVER | add edi 4
        lodsb
        If al <> 0A
            Call LoadedOpToHexa | stosw
        End_If

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpD5:
    .If B$EscapePrefix = &TRUE
      ; 0F D5 /r PMULLW mm, mm/m64      ; 66 0F D5 /r PMULLW xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'pmul', D$edi+4 'lw  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        Mov D$edi 'aad ', B$DisFlag DISDONE+DISLINEOVER | add edi 4
        lodsb
        If al <> 0A
            Call loadedOpToHexa | stosw
        End_If

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER

ret


OpD6:
    .If B$EscapePrefix = &TRUE
        If B$OperandSizeOverride = &TRUE    ; 66 0F D6 MOVQ xmm2/m64, xmm1
            Mov D$edi 'movq', B$edi+4 SPC | add edi 5 | jmp Dis_xmm2_m64__xmm1
        End_If
    .Else
        Mov D$edi 'salc'  | add edi 4
        Mov B$DisFlag DISDONE+DISLINEOVER
    .End_If
ret


OpD7:
    ;inc D$UnLikelyCode
    .If B$EscapePrefix = &TRUE
      ; 66 0F D7 /r PMOVMSKB r32, xmm ; 0F D7 /r PMOVMSKB r32, mm
        Mov D$edi 'pmov', D$edi+4 'mskb', B$edi+8 SPC | add edi 9 | jmp Dis_r32_xmmx

    .Else       ; D7 XLATB
        Mov D$edi 'xlat', B$edi+4 'b' | add edi 5

    .End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpD8:
    ..If B$EscapePrefix = &TRUE
      ; 0F D8 /r PSUBUSB mm, mm/m64 ; 66 0F D8 /r PSUBUSB xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'psub', D$edi+4 'usb ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    ..Else
        Mov bl B$esi | inc esi | DigitMask bl To al

        .If al = 0          ; D8 /0 FADD m32fp ; D8 C0+i FADD ST(0), ST(i)
            Mov D$edi 'fadd', B$edi+4 SPC | add edi 5
        .Else_If al = 1     ; D8 /1 FMUL m32fp ; D8 C8+i FMUL ST(0), ST(i)
            Mov D$edi 'fmul', B$edi+4 SPC | add edi 5
        .Else_If al = 2  ; FCOM m32fp ; FCOM ST(i)
            Mov D$edi 'fcom', B$edi+4 SPC | add edi 5
        .Else_If al = 3     ; FCOMP m32fp; FCOMP ST(i)
           Mov D$edi 'fcom', W$edi+4 'p ' | add edi 6
        .Else_If al = 4     ; FSUB m32fp ; FSUB ST(0), ST(i)
            Mov D$edi 'fsub', B$edi+4 SPC | add edi 5
        .Else_If al = 5     ; FSUBR m32fp ; FSUBR ST(0), ST(i)
            Mov D$edi 'fsub', W$edi+4 'r ' | add edi 6
        .Else_If al = 6     ; FDIV m32fp ; FDIV ST(0), ST(i)
            Mov D$edi 'fdiv', B$edi+4 SPC | add edi 5
        .Else_If al = 7     ; FDIVR m32fp ; FDIVR ST(0), ST(i)
            Mov D$edi 'fdiv', W$edi+4 'r ' | add edi 6
        .End_If

        jmp Dis_St0Sti_or_Fmem

    ..End_If
ret


OpD9:
    If B$EscapePrefix = &TRUE
      ; 0F D9 /r PSUBUSW mm, mm/m64     ; 66 0F D9 /r PSUBUSW xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'psub', D$edi+4 'usw ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128
    End_If

    Mov bl B$esi | inc esi | ModMask bl to al  ; 0EE 0011_101_110

    ..If al = 3
        Mov al bl
        .If al = 0E4       ; D9 E4 FTST
            Mov D$edi 'ftst' | add edi 4
        .Else_If al = 0E5  ; FXAM
            Mov D$edi 'fxam' | add edi 4
        .Else_If al = 0F0  ; F2XM1
            Mov D$edi 'f2xm', B$edi+4 '1' | add edi 5
        .Else_If al = 0F1  ; FYL2X
            Mov D$edi 'fyl2', B$edi+4 'x' | add edi 5
        .Else_If al = 0F2  ; FPTAN
            Mov D$edi 'fpta', B$edi+4 'n' | add edi 5
        .Else_If al = 0F3  ; FPATAN
            Mov D$edi 'fpat', W$edi+4 'an' | add edi 6
        .Else_If al = 0F4  ; FXTRACT
            Mov D$edi 'fxtr', D$edi+4 'act ' | add edi 7
        .Else_If al = 0F5  ; FPREM1
            Mov D$edi 'fpre', W$edi+4 'm1' | add edi 6
        .Else_If al = 0F8  ; FPREM
            Mov D$edi 'fpre', B$edi+4 'm' | add edi 5
        .Else_If al = 0F9  ; FYL2XP1
            Mov D$edi 'fyl2', D$edi+4 'xp1 ' | add edi 7
        .Else_If al = 0FA  ; FSQRT
            Mov D$edi 'fsqr', B$edi+4 't' | add edi 5
        .Else_If al = 0FB  ; FSINCOS
            Mov D$edi 'fsin', D$edi+4 'cos ' | add edi 7
        .Else_If al = 0FC  ; FRNDINT
            Mov D$edi 'frnd', D$edi+4 'int ' | add edi 7
        .Else_If al = 0FD  ; FSCALE
            Mov D$edi 'fsca', W$edi+4 'le' | add edi 6
        .Else_If al = 0FE  ; FSIN
            Mov D$edi 'fsin' | add edi 4
        .Else_If al = 0D0  ; FNOP
            Mov D$edi 'fnop' | add edi 4
        .Else_If al = 0E1  ; FABS
            Mov D$edi 'fabs' | add edi 4
        .Else_If al = 0E0  ; FCHS
            Mov D$edi 'fchs' | add edi 4
        .Else_If al = 0E8  ; FLD1
            Mov D$edi 'fld1' | add edi 4
        .Else_If al = 0E9  ; FLDL2T
            Mov D$edi 'fldl', W$edi+4 '2t' | add edi 6
        .Else_If al = 0EA  ; FLDL2E
            Mov D$edi 'fldl', W$edi+4 '2e' | add edi 6
        .Else_If al = 0EB  ; FLDPI
            Mov D$edi 'fldp', B$edi+4 'i' | add edi 5
        .Else_If al = 0EC  ; FLDLG2
            Mov D$edi 'fldl', W$edi+4 'g2' | add edi 6
        .Else_If al = 0ED  ; FLDLN2
            Mov D$edi 'fldl', W$edi+4 'n2' | add edi 6
        .Else_If al = 0EE  ; FLDZ
            Mov D$edi 'fldz' | add edi 4
        .Else_If al = 0F6  ; FDECSTP
            Mov D$edi 'fdec', D$edi+4 'stp ' | add edi 7
        .Else_If al = 0F7  ; FINCSTP
            Mov D$edi 'finc', D$edi+4 'stp ' | add edi 7
        .Else_If al = 0FF  ; FCOS
            Mov D$edi 'fcos' | add edi 4
        .Else
            and eax (not 7)
            If al = 0C0         ; D9 C0+i > FLD ST(i)
                Mov D$edi 'fld ' | add edi 4
            Else_If al = 0C8    ; D9 C8+i FXCH ST(i)
                Mov D$edi 'fxch', B$edi+4 SPC | add edi 5
            Else
                inc D$UnLikelyCode | dec esi | ret
            End_If

            Call WriteSti

        .End_If

    ..Else
        DigitMask bl To al ; 0EE

        .If al = 0            ; D9 /0 > FLD m32fp
            Mov D$edi 'fld ' | add edi 4 | jmp EndWith.F.mem
        .Else_If al = 2       ; FST m32fp
            Mov D$edi 'fst ' | add edi 4 | jmp EndWith.F.mem
        .Else_If al = 3       ; FSTP m32fp
            Mov D$edi 'fstp', B$edi+4 SPC | add edi 5 | jmp EndWith.F.mem
        .Else_If al = 4       ; FLDENV m14/28byte
            Mov D$edi 'flde', D$edi+4 'nv  ' | add edi 7 | jmp EndWith.X.mem
        .Else_If al = 5       ; D9 /5 > FLDCW m2byte
            Mov D$edi 'fldc', W$edi+4 'w ' | add edi 6
        .Else_If al = 6       ; D9 /6FNSTENV* m14/28byte
            Mov D$edi 'fnst', D$edi+4 'env ' | add edi 8 | jmp EndWith.X.mem
        .Else_If al = 7       ; D9 /7 > FNSTCW m2byte
            Mov D$edi 'fnst', D$edi+4 'cw  ' | add edi 7
        .Else
            inc D$UnLikelyCode | dec esi | ret
        .End_If

        jmp EndWith.W.mem

    ..End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpDA:
    If B$EscapePrefix = &TRUE
      ; 0F DA /r PMINUB mm1, mm2/m64      ; 66 0F DA /r PMINUB xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'pmin', D$edi+4 'ub  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128
    End_If

    Mov bl B$esi | inc esi | ModMask bl to al

    ..If al = 3
        Mov al bl

        .If al = 0E9     ; FUCOMPP
            Mov D$edi 'fuco', D$edi+4 'mpp ' | add edi 7
            Mov B$DisFlag DISDONE+DISLINEOVER | ret

        .Else
            and eax (not 7)
            If al = 0C0         ; DA C0+i : FCMOVB ST(0), ST(i)
                Mov D$edi 'fcmo', D$edi+4 'vb  ' | add edi 7
            Else_If al = 0C8    ; FCMOVE ST(0), ST(i)
                Mov D$edi 'fcmo', D$edi+4 've  ' | add edi 7
            Else_If al = 0D0    ; FCMOVBE ST(0), ST(i)
                Mov D$edi 'fcmo', D$edi+4 'vbe ' | add edi 8
            Else_If al = 0D8    ; FCMOVU ST(0), ST(i)
                Mov D$edi 'fcmo', D$edi+4 'vu  ' | add edi 7
            Else
                inc D$UnLikelyCode | dec esi | ret
            End_If

        .End_If

        Call WriteSt0Sti

    ..Else
        DigitMask bl To al

        .If al = 0          ; FIADD m32int
            Mov D$edi 'fiad', W$edi+4 'd ' | add edi 6
        .Else_If al = 1     ; FIMUL m32int
            Mov D$edi 'fimu', W$edi+4 'l ' | add edi 6
        .Else_If al = 2     ; FICOM m32int
            Mov D$edi 'fico', W$edi+4 'm ' | add edi 6
        .Else_If al = 3     ; FICOMP m32int
            Mov D$edi 'fico', D$edi+4 'mp  ' | add edi 7
        .Else_If al = 4     ; FISUB m32int
            Mov D$edi 'fisu', W$edi+4 'b ' | add edi 6
        .Else_If al = 5     ; FISUBR m32int
            Mov D$edi 'fisu', D$edi+4 'br  ' | add edi 7
        .Else_If al = 6     ; FIDIV m32int
            Mov D$edi 'fidi', W$edi+4 'v ' | add edi 6
        .Else_If al = 7     ; FIDIVR m32int
            Mov D$edi 'fidi', D$edi+4 'vr  ' | add edi 7
        .End_If

        jmp EndWith.D.mem

    ..End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret




OpDB:
    If B$EscapePrefix = &TRUE
        ; 66 0F DB /r PAND xmm1, xmm2/m128      ; 0F DB /r PAND mm, mm/m64
        Mov D$edi 'pand', B$edi+4 SPC | add edi 5
        jmp Dis_xmmx1__xmmx2_m64_128
    End_If

    Mov bl B$esi | inc esi | ModMask bl to al

    ..If al = 3
        Mov al bl

        If al = 0E3      ; DB E3 FNINIT >>> E3 DigitBit = 4
            Mov D$edi 'fnin', D$edi+4 'it  '
L0:         add edi 6 | Mov B$DisFlag DISDONE+DISLINEOVER | ret
        Else_If al = 0E2    ; DB E2 FNCLEX*
            Mov D$edi 'fncl', D$edi+4 'ex  ' | jmp L0<
        End_If

        and eax (not 7)

        .If al = 0C0        ; DA C0+i : FCMOVNB ST(0), ST(i)
            Mov D$edi 'fcmo', D$edi+4 'vnb ' | add edi 8
        .Else_If al = 0C8   ; FCMOVNE ST(0), ST(i)
            Mov D$edi 'fcmo', D$edi+4 'vne ' | add edi 8
        .Else_If al = 0D0   ; FCMOVNBE ST(0), ST(i)
            Mov D$edi 'fcmo', D$edi+4 'vnbe', B$edi+8 SPC | add edi 9
        .Else_If al = 0D8   ; FCMOVNU ST(0), ST(i)
            Mov D$edi 'fcmo', D$edi+4 'vnu ' | add edi 8
        .Else_If al = 0E8   ; FUCOMI ST, ST(i)
            Mov D$edi 'fuco', D$edi+4 'mi  ' | add edi 7
        .Else_If al = 0F0   ; FCOMI ST, ST(i)
            Mov D$edi 'fcom', W$edi+4 'i ' | add edi 6
        .Else
            inc D$UnLikelyCode | dec esi | ret
        .End_If

        Call WriteSt0Sti

    ..Else
        DigitMask bl To al

        .If al = 0     ; FILD m32int
            Mov D$edi 'fild', B$edi+4 SPC | add edi 5 | jmp EndWith.D.mem
        .Else_If al = 1        ; FISTTP  DB /1 FISTTP m32int
            Mov D$edi 'fist', D$edi+4 'tp ' | add edi 7 | jmp EndWith.D.mem
        .Else_If al = 2        ; FIST m32int
            Mov D$edi 'fist', B$edi+4 SPC | add edi 5 | jmp EndWith.D.mem
        .Else_If al = 3        ; FISTP m32int
            Mov D$edi 'fist', W$edi+4 'p ' | add edi 6 | jmp EndWith.D.mem
        .Else_If al = 5        ; FLD m80fp
            Mov D$edi 'fld ' | add edi 4 | jmp EndWith.T.mem
        .Else_If al = 7        ; FSTP m80fp
            Mov D$edi 'fstp', B$edi+4 SPC | add edi 5 | jmp EndWith.T.mem
        .Else
            inc D$UnLikelyCode | dec esi | ret
        .End_If

     ..End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpDC:
    If B$EscapePrefix = &TRUE
        ; 66 0F DC /r PADDUSB xmm1, xmm2/m128     ; 0F DC /r PADDUSB mm, mm/m64
        ;inc D$UnLikelyCode
        Mov D$edi 'padd', D$edi+4 'usb ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128
    End_If

    Mov bl B$esi | inc esi | ModMask bl to al

    ..If al = 3
        Mov al bl | and eax (not 7)

        If al = 0C0             ; FADD ST(i), ST(0)
            Mov D$edi 'fadd', B$edi+4 SPC | add edi 5
        Else_If al = 0C8        ; FMUL ST(i), ST(0)
            Mov D$edi 'fmul', B$edi+4 SPC | add edi 5
        Else_If al = 0E0        ; FSUBR ST(i), ST(0)
            Mov D$edi 'fsub', W$edi+4 'r ' | add edi 6  ; fsubr
        Else_If al = 0E8        ; FSUB ST(i), ST(0)
            Mov D$edi 'fsub', B$edi+4 SPC | add edi 5
        Else_If al = 0F0        ; FDIVR ST(i), ST(0)
            Mov D$edi 'fdiv', W$edi+4 'r ' | add edi 6
        Else_If al = 0F8        ; FDIV ST(i), ST(0)
            Mov D$edi 'fdiv', B$edi+4 SPC | add edi 5
        Else
            inc D$UnLikelyCode | dec esi | ret
        End_If

        Call WriteStiSt0

    ..Else
        DigitMask bl To al

        .If al = 0          ; FADD m64fp
            Mov D$edi 'fadd', B$edi+4 SPC | add edi 5
        .Else_If al = 1     ; FMUL m64fp
            Mov D$edi 'fmul', B$edi+4 SPC | add edi 5
        .Else_If al = 2     ; FCOM m64fp
            Mov D$edi 'fcom', B$edi+4 SPC | add edi 5
        .Else_If al = 3     ; FCOMP m64fp
            Mov D$edi 'fcom', W$edi+4 'p ' | add edi 6
        .Else_If al = 4     ; FSUB m64fp
            Mov D$edi 'fsub', B$edi+4 SPC | add edi 5
        .Else_If al = 5      ; FSUBR m64fp
            Mov D$edi 'fsub', W$edi+4 'r ' | add edi 6
        .Else_If al = 6     ; FDIV m64fp
            Mov D$edi 'fdiv', B$edi+4 SPC | add edi 5
        .Else_If al = 7     ; FDIVR m64fp
            Mov D$edi 'fdiv', W$edi+4 'r ' | add edi 6
        .End_If

        jmp EndWith.R.mem

    ..End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpDD:
    If B$EscapePrefix = &TRUE
        ; 66 0F DD /r PADDUSW xmm1, xmm2/m128     ; 0F DD /r PADDUSW mm, mm/m64
        Mov D$edi 'padd', D$edi+4 'usw ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128
    End_If

    Mov bl B$esi | inc esi | ModMask bl to al

    ..If al = 3
        Mov al bl | and eax (not 7)

        If al = 0C0     ; C0+i :  FFREE ST(i)
            Mov D$edi 'ffre', W$edi+4 'e ' | add edi 6
        Else_If al = 0D0    ; FST ST(i)
            Mov D$edi 'fst ' | add edi 4
        Else_If al = 0D8        ; FSTP ST(i)
            Mov D$edi 'fstp', B$edi+4 SPC | add edi 5
        Else_If al = 0E0        ; FUCOM ST(i)
            Mov D$edi 'fuco', W$edi+4 'm ' | add edi 6
        Else_If al = 0E8        ; FUCOMP ST(i)
            Mov D$edi 'fuco', D$edi+4 'mp  ' | add edi 7
        Else
            inc D$UnLikelyCode | dec esi | ret
        End_If

        Call WriteSti

    ..Else
        DigitMask bl To al

        .If al = 0          ; FLD m64fp
            Mov D$edi 'fld ' | add edi 4 | jmp EndWith.R.mem
        .Else_If al = 1     ; FISTTP  DD /1 FISTTP m64int
            Mov D$edi 'fst ', D$edi+4 'tp '| add edi 7 | jmp EndWith.R.mem
        .Else_If al = 2     ; FST m64fp
            Mov D$edi 'fst ' | add edi 4 | jmp EndWith.R.mem
        .Else_If al = 3     ; FSTP m64fp
            Mov D$edi 'fstp', B$edi+4 SPC | add edi 5 | jmp EndWith.R.mem
        .Else_If al = 4     ; FRSTOR m94/108byte
            Mov D$edi 'frst', D$edi+4 'or  ' | add edi 7 | jmp EndWith.X.mem
        .Else_If al = 6     ; FNSAVE* m94/108byte
            Mov D$edi 'fnsa', D$edi+4 've  ' | add edi 7 | jmp EndWith.X.mem
        .Else_If al = 7     ; DD /7 FNSTSW* m2byte
            Mov D$edi 'fnst', D$edi+4 'sw  ' | add edi 7 | jmp EndWith.W.mem
        .Else
            inc D$UnLikelyCode | dec esi | ret
        .End_If

    ..End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpDE:
    If B$EscapePrefix = &TRUE
        ; 0F DE /r PMAXUB mm1, mm2/m64      ; 66 0F DE /r PMAXUB xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'pmax', D$edi+4 'ub  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128
    End_If

    Mov bl B$esi | inc esi | ModMask bl to al

    ...If al = 3
        Mov al bl

        .If al = 0D9        ; FCOMPP
            Mov D$edi 'fcom', W$edi+4 'pp' | add edi 6
        .Else_If al = 0E9   ; FSUBP
            Mov D$edi 'fsub', W$edi+4 'p ' | add edi 6
            Call WriteStiSt0
        .Else
            and eax (not 7)

            If al = 0C0             ; FADDP ST(0), ST(i)
                Mov D$edi 'fadd', W$edi+4 'p ' | add edi 6 | jmp WriteStiSt0
            Else_If al = 0C8        ; FMULP ST(i), ST(0)
                Mov D$edi 'fmul', W$edi+4 'p ' | add edi 6
            Else_If al = 0E0        ; FSUBRP ST(i), ST(0)
                Mov D$edi 'fsub', D$edi+4 'rp ' | add edi 7
            Else_If al = 0E8        ; FSUBP ST(i), ST(0)
                Mov D$edi 'fsub', W$edi+4 'p ' | add edi 6
            Else_If al = 0F0        ; FDIVRP ST(i), ST(0)
                Mov D$edi 'fdiv', D$edi+4 'rp ' | add edi 7
            Else_If al = 0F8        ; FDIVP ST(i), ST(0)
                Mov D$edi 'fdiv', W$edi+4 'p ' | add edi 6
            Else
                inc D$UnLikelyCode | dec esi | ret
            End_If

            Call WriteStiSt0

        .End_If

    ...Else
        DigitMask bl To al

        .If al = 0             ; FIADD m16int
            Mov D$edi 'fiad', W$edi+4 'd ' | add edi 6
        .Else_If al = 1        ; FIMUL m16int
            Mov D$edi 'fimu', W$edi+4 'l ' | add edi 6
        .Else_If al = 2        ; FICOM m16int
            Mov D$edi 'fico', W$edi+4 'm ' | add edi 6
        .Else_If al = 3        ; FICOMP m16int
            Mov D$edi 'fico', D$edi+4 'mp  ' | add edi 7
        .Else_If al = 4        ; FISUB m16int
            Mov D$edi 'fisu', W$edi+4 'b ' | add edi 6
        .Else_If al = 5        ; FISUBR m16int
             Mov D$edi 'fisu', D$edi+4 'br  ' | add edi 7
        .Else_If al = 6        ; FIDIV m16int
            Mov D$edi 'fidi', W$edi+4 'v ' | add edi 6
        .Else_If al = 7        ; FIDIVR m16int
            Mov D$edi 'fidi', D$edi+4 'vr  ' | add edi 7
        .End_If

        jmp EndWith.W.mem

    ...End_If

L9: Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpDF:
    If B$EscapePrefix = &TRUE
        ; 66 0F DF /r PANDN xmm1, xmm2/m128     ; 0F DF /r PANDN mm, mm/m64
        ;inc D$UnLikelyCode
        Mov D$edi 'pand', W$edi+4 'n ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128
    End_If

    Mov bl B$esi | inc esi | ModMask bl to al

    ...If al = 3
        Mov al bl

        .If al = 0E0       ; DF E0 FNSTSW* AX
            Mov D$edi 'fnst', D$edi+4 'sw a', B$edi+8 'x' | add edi 9
        .Else
            and eax (not 7)

            If al = 0F0        ; FCOMIP ST, ST(i)
                Mov D$edi 'fcom', D$edi+4 'ip  ' | add edi 7
            Else_If al = 0C0
                Mov D$edi 'ffre', D$edi+4 'ep  ' | add edi 7 | jmp WriteSti
            Else_If al = 0E8       ; FUCOMIP ST, ST(i)
                Mov D$edi 'fuco', D$edi+4 'mip ' | add edi 8
            Else
                inc D$UnLikelyCode | dec esi | ret
            End_If

            jmp WriteSt0Sti

        .End_If

    ...Else
        DigitMask bl To al

        ..If al = 0     ; FILD m16int
            Mov D$edi 'fild', B$edi+4 SPC | add edi 5 | jmp EndWith.W.mem
        ..Else_If al = 1        ; FISTTP  DF /1 FISTTP m16int
            Mov D$edi 'fist', D$edi+4 'tp ' | add edi 7 | jmp EndWith.W.mem
        ..Else_If al = 2        ; FIST m16int
            Mov D$edi 'fist', B$edi+4 SPC | add edi 5 | jmp EndWith.W.mem
        ..Else_If al = 3        ; FISTP m16int
            Mov D$edi 'fist', W$edi+4 'p ' | add edi 6 | jmp EndWith.W.mem
        ..Else_If al = 4           ; /4 > FBLD m80 dec
            Mov D$edi 'fbld', B$edi+4 SPC | add edi 5 | jmp EndWith.T.mem
            ; This is an m80 Binary coded decimal chunk.
        ..Else_If al = 5        ; FILD m64int
            Mov D$edi 'fild', B$edi+4 SPC | add edi 5 | jmp EndWith.Q.mem
        ..Else_If al = 6      ; /6 > FBSTP m80bcd
            Mov D$edi 'fbst', W$edi+4 'p ' | add edi 6 | jmp EndWith.T.mem
            ; This is an m80 Binary coded decimal chunk.
        ..Else_If al = 7        ; FISTP m64int
            Mov D$edi 'fist', W$edi+4 'p ' | add edi 6 | jmp EndWith.Q.mem
        ..Else
            inc D$UnLikelyCode | dec esi | ret
        ..End_If
    ...End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpE0:
    If B$EscapePrefix = &TRUE
        ; 66 0F E0, /r PAVGB xmm1, xmm2/m128     ; 0F E0 /r PAVGB mm1, mm2/m64
        ;inc D$UnLikelyCode
        Mov D$edi 'pavg', W$edi+4 'b ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128

    Else        ; LOOPNE rel8 ; LOOPNZ rel8
        inc D$LikelyCode
        Mov D$edi 'loop', D$edi+4 'ne  ' | add edi 7
        Mov B$CALLInstruction &TRUE
        test B$esi 080 NOT_ZERO L1>
            add D$UnLikelyCode 0FF
L1:     jmp EndWithDisByteRelativeBack

    End_If


OpE1:
    If B$EscapePrefix = &TRUE
      ; 0F E1 /r PSRAW mm, mm/m64       ; 66 0F E1 /r PSRAW xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'psra', W$edi+4 'w ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128

    Else       ;  ; LOOPE rel8 ; LOOPZ rel8
        inc D$LikelyCode
        Mov D$edi 'loop', W$edi+4 'e ' | add edi 6
        Mov B$CALLInstruction &TRUE
        test B$esi 080 NOT_ZERO L1>
            add D$UnLikelyCode 0FF
L1:     jmp EndWithDisByteRelativeBack

    End_If


OpE2:
    If B$EscapePrefix = &TRUE
      ; 0F E2 /r PSRAD mm, mm/m64   ; 66 0F E2 /r PSRAD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'psra', W$edi+4 'd ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128

    Else        ; LOOP rel8
        inc D$LikelyCode
        Mov D$edi 'loop', B$edi+4 SPC | add edi 5
        Mov B$CALLInstruction &TRUE
        test B$esi 080 NOT_ZERO L1>
            add D$UnLikelyCode 0FF
L1:     jmp EndWithDisByteRelativeBack

    End_If


OpE3:
    .If B$EscapePrefix = &TRUE
        ; 66 0F E3 /r PAVGW xmm1, xmm2/m128 ; 0F E3 /r PAVGW mm1, mm2/m64
        ;inc D$UnLikelyCode
        Mov D$edi 'pavg', W$edi+4 'w ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        Mov B$CALLInstruction &TRUE
        If B$OperandSizeOverride = &FALSE       ; JECXZ rel8
            inc D$LikelyCode
            SubEdi6 | Mov D$edi ' | j', D$edi+4 'ecxz', B$edi+8 SPC | add edi 9
        Else                                    ; JCXZ rel8
            inc D$UnLikelyCode
            SubEdi6 | Mov D$edi ' | j', D$edi+4 'cxz ' | add edi 8
        End_If
        jmp EndWithDisByteRelative

    .End_If


OpE4:
    .If B$EscapePrefix = &TRUE
      ; 0F E4 /r PMULHUW mm1, mm2/m64       ; 66 0F E4 /r PMULHUW xmm1, xmm2/m128
        Mov D$edi 'pmul', D$edi+4 'huw ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else        ; IN AL,imm8
        inc D$UnLikelyCode
        Mov D$edi 'in a', W$edi+4 'l ' | add edi 6 | Call WriteImm8

    .End_If

     Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpE5:

    .If B$EscapePrefix = &TRUE
      ; 0F E5 /r PMULHW mm, mm/m64      ; 66 0F E5 /r PMULHW xmm1, xmm2/m128
        Mov D$edi 'pmul', D$edi+4 'hw  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        inc D$UnLikelyCode
        If B$OperandSizeOverride = &FALSE   ; IN AX,imm8 / IN EAX,imm8
            Mov D$edi 'in e', D$edi+4 'ax  ' | add edi 7
        Else
            Mov D$edi 'in a', W$edi+4 'x ' | add edi 6
        End_If
        Call WriteImm8

    .End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpE6: ; CVTTPD2DQ xmm1, xmm2/m128

    .If B$EscapePrefix = &TRUE
        If B$OperandSizeOverride = &TRUE    ; CVTTPD2DQ xmm1, xmm2/m128
            Call MarkSSEdata SSE_2_R
            Mov D$edi 'cvtt', D$edi+4 'pd2d', W$edi+8 'q ' | add edi 10
            jmp Dis_mmx1__xmm2_m128
        Else
            ret
        End_If
    .Else       ; OUT imm8, AL
        inc D$UnLikelyCode
        Mov D$edi 'out ' | add edi 4
        Call WriteImm8 | Mov D$edi ' al ' | add edi 3

    .End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpE7:
    .If B$EscapePrefix = &TRUE
        Mov bl B$esi | inc esi
        If B$OperandSizeOverride = &TRUE        ; 66 0F E7 /r MOVNTDQ m128, xmm
            Mov D$edi 'movn', D$edi+4 'tdq ' | add edi 8
            jmp Dis_m128_xmm
        Else        ; 0F E7 /r MOVNTQ m64, mm
            Mov D$edi 'movn', D$edi+4 'tq  ' | add edi 7
            jmp Dis_m64_mmx
        End_If

    .Else       ; OUT imm8, AX // OUT imm8, EAX
        inc D$UnLikelyCode
        Mov D$edi 'out ' | add edi 4
        Call WriteImm8
        If B$OperandSizeOverride = &TRUE
            Mov D$edi ' ax ' | add edi 3
        Else
            Mov D$edi ' eax' | add edi 4
        End_If

    .End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpE8:
    .If B$EscapePrefix = &TRUE
      ; 0F E8 /r PSUBSB mm, mm/m64  ; 66 0F E8 /r PSUBSB xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'psub', D$edi+4 'sb  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else        ; Call rel 16/32
        Mov D$edi 'Call', B$edi+4 SPC | add edi 5
        Mov B$CALLInstruction &TRUE

        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            lodsd
        Else
            inc D$UnLikelyCode
            movsx eax W$esi | add esi 2
        End_If

        Mov D$LastCodeRef eax
        Call RelativeToAbsolute | Call WriteDisRelative

    .End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret

OpE9: ; jmp rel 16/32
    .If B$EscapePrefix = &TRUE
      ; 0F E9 /r PSUBSW mm, mm/m64  ; 66 0F E9 /r PSUBSW xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        Mov D$edi 'psub', D$edi+4 'sw  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else        ; jmp rel 16/32
        On B$edi-2 = ';', sub edi 4
        Mov D$edi 'jmp ' | add edi 4
        Mov B$CALLInstruction &TRUE, B$DisEndOfChunk &TRUE

        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            lodsd
        Else
            inc D$UnLikelyCode
            movsx eax W$esi | add esi 2
        End_If

        Mov D$LastCodeRef eax
        Call RelativeToAbsolute | Call WriteDisRelative

    .End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpEA:
    ;inc D$UnLikelyCode
    .If B$EscapePrefix = &TRUE
      ; 0F EA /r PMINSW mm1, mm2/m64        ; 66 0F EA /r PMINSW xmm1, xmm2/m128
        Mov D$edi 'pmin', D$edi+4 'sw  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        inc D$UnLikelyCode
        If B$OperandSizeOverride = &TRUE    ; JMP ptr16:32, JMP ptr16:16 (jmp inter-segment).
            Mov D$edi 'jmpF', D$edi+4 ' W$ ' | add edi 7 ; jE! fix
            Push D$esi, esi
                Exchange W$esi, W$esi+2
                Call WriteImm16 | Mov B$edi SPC | inc edi | Call WriteImm16 ; jE! fix
            Pop eax, D$eax
        Else                                ;   JMP far ptr32:selector16
            Mov D$edi 'jmpF', D$edi+4 ' D$ ' | add edi 7 ; jE! fix
            Push D$esi, D$esi+4, esi
                Mov eax D$esi, bx W$esi+4, W$esi bx, D$esi+2 eax
                Call WriteImm16 | Mov B$edi SPC | inc edi | Call WriteImm32 ; jE! fix
            Pop eax, D$eax+4, D$eax
        End_If
        Mov B$DisEndOfChunk &TRUE
        Mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8

    .End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpEB:
    If B$EscapePrefix = &TRUE
      ; 0F EB /r POR mm, mm/m64     ; 66 0F EB /r POR xmm1, xmm2/m128
        Mov D$edi 'por ' | add edi 4
        jmp Dis_xmmx1__xmmx2_m64_128

    Else        ; jmp rel 8
        inc D$LikelyCode
        On B$edi-2 <> ';', sub edi 4
        Mov D$edi 'jmp ' | add edi 4
        Mov B$CALLInstruction &TRUE, B$DisEndOfChunk &TRUE | jmp EndWithDisByteRelative

    End_If


OpEC:
    If B$EscapePrefix = &TRUE
        ; 66 0F EC /r PADDSB xmm1,xmm2/m128       ; 0F EC /r PADDSB mm, mm/m64
        Mov D$edi 'padd', D$edi+4 'sb  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    Else           ; IN AL,DX
        inc D$UnLikelyCode
        Mov D$edi 'in a', D$edi+4 'l dx' | add edi 8

    End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpED:
    .If B$EscapePrefix = &TRUE
        ; 66 0F ED /r PADDSW xmm1, xmm2/m128    ; 0F ED /r PADDSW mm, mm/m64
        Mov D$edi 'padd', D$edi+4 'sw  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else        ; IN AX,DX / IN EAX,DX
        inc D$UnLikelyCode
        If B$OperandSizeOverride = &FALSE
            Mov D$edi 'in e', D$edi+4 'ax d', B$edi+8 'x' | add edi 9
        Else
            Mov D$edi 'in a', D$edi+4 'x dx' | add edi 8
        End_If

    .End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret

OpEE:

    If B$EscapePrefix = &TRUE
        ; 0F EE /r PMAXSW mm1, mm2/m64      ; 66 0F EE /r PMAXSW xmm1, xmm2/m128
        Mov D$edi 'pmax', D$edi+4 'sw  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    Else          ; OUT DX, AL
        inc D$UnLikelyCode
        Mov D$edi 'out ', D$edi+4 'dx a', B$edi+8 'l' | add edi 9

    End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpEF:
    .If B$EscapePrefix = &TRUE
      ; 0F EF /r PXOR mm, mm/m64    ; 66 0F EF /r PXOR xmm1, xmm2/m128
        Mov D$edi 'pxor', B$edi+4 SPC | add edi 5
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        inc D$UnLikelyCode
      ; OUT DX, AX // OUT DX, EAX
        Mov D$edi 'out ', W$edi+4 'dx' | add edi 6
        If B$OperandSizeOverride = &FALSE
            Mov D$edi ' eax' | add edi 4
        Else
            Mov D$edi ' ax ' | add edi 3
        End_If

    .End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret

[LockPrefix: D$ ?]

; ADD, ADC, AND, BTC, BTR, BTS, CMPXCHG, CMPXCH8B, DEC, INC, NEG, NOT, OR,
; SBB, SUB, XOR, XADD, XCHG

OpF0:
    Mov B$LockPrefix &TRUE
    inc D$Prefixes
    Mov D$edi 'lock', B$edi+4 SPC, B$DisFlag DISDONE | add edi 5
    Mov B$DisFlag DISDONE ;+DISLINEOVER
ret


OpF1:
    ;inc D$UnLikelyCode
    If B$EscapePrefix = &TRUE
      ; 0F F1 /r PSLLW mm, mm/m64  ; 66 0F F1 /r PSLLW xmm1, xmm2/m128
        Mov D$edi 'psll', W$edi+4 'w ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128
    End_If
ret


OpF2:
    ..If B$esi = 0F
        .If B$esi+1 = 010       ; F2 0F 10 /r MOVSD xmm1, xmm2/m64 ; op10
            add esi 2 | Mov D$edi 'movs', W$edi+4 'd ' | add edi 6
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 011       ; F2 0F 11 /r MOVSD xmm2/m64, xmm
            add esi 2 | Mov D$edi 'movs', W$edi+4 'd ' | add edi 6
            jmp Dis_xmm2_m64__xmm1
        .Else_If B$esi+1 = 012       ; F2,0F,12,/r MOVDDUP xmm1, xmm2/m64
            add esi 2 | Call MarkSSEdata SSE_1_Q
            Mov D$edi 'movd', D$edi+4 'dup ' | add edi 8
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 02A       ; CVTSI2SD xmm, r/m32
            add esi 2 | Call MarkSSEdata SSE_1_D
            Mov D$edi 'cvts', D$edi+4 'i2sd', B$edi+8 SPC | add edi 9
            jmp Dis_xmm1__rm32
        .Else_If B$esi+1 = 02C       ; CVTTSD2SI r32, xmm/m64
            add esi 2 | Call MarkSSEdata SSE_1_R
            Mov D$edi 'cvtt', D$edi+4 'sd2s', W$edi+8 'i ' | add edi 10
            jmp Dis_r32__xmm_m64
        .Else_If B$esi+1 = 02D       ; CVTSD2SI r32, xmm/m64
            add esi 2 | Call MarkSSEdata SSE_1_R
            Mov D$edi 'cvts', D$edi+4 'd2si', B$edi+8 SPC | add edi 9
            jmp Dis_r32__xmm_m64
        .Else_If B$esi+1 = 051      ; F2 0F 51 /r SQRTSD xmm1, xmm2/m64
            add esi 2 | Call MarkSSEdata SSE_2_R
            Mov D$edi 'sqrt', D$edi+4 'sd  ' | add edi 7
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 058      ; ADDSD
            add esi 2 | Call MarkSSEdata SSE_1_R
            Mov D$edi 'adds', W$edi+4 'd ' | add edi 6 | Call Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 059      ; F2 0F 59 /r MULSD xmm1, xmm2/m64
            add esi 2 | Call MarkSSEdata SSE_1_R
            Mov D$edi 'muls', W$edi+4 'd ' | add edi 6
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 05A       ; CVTSD2SS xmm1, xmm2/m64
            add esi 2 | Call MarkSSEdata SSE_1_R
            Mov D$edi 'cvts', D$edi+4 'd2ss', B$edi+8 SPC | add edi 9
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 05C   ; F2 0F 5C /r SUBSD xmm1, xmm2/m64
            add esi 2 | Call MarkSSEdata SSE_2_R
            Mov D$edi 'subs', W$edi+4 'd ' | add edi 6
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 05D       ; MINSD xmm1, xmm2/m64
            add esi 2 | Call MarkSSEdata SSE_1_R
            Mov D$edi 'mins', W$edi+4 'd ' | add edi 6
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 05E         ; DIVSD xmm1, xmm2/m64
            add esi 2 | Call MarkSSEdata SSE_1_R
            Mov D$edi 'divs', W$edi+4 'd ' | add edi 6
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 05F      ; F2 0F 5F /r MAXSD xmm1, xmm2/m64
            add esi 2 | Call MarkSSEdata SSE_1_R
            Mov D$edi 'maxs', W$edi+4 'd ' | add edi 6
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 070      ; F2 0F 70 /r ib PSHUFLW xmm1, xmm2/m128, imm8
            add esi 2 | Mov D$edi 'pshu', D$edi+4 'flw ' | add edi 8
            Call Dis_xmm1__xmm2_m128 | Mov B$edi SPC | inc edi
            Call WriteImm8
        .Else_If B$esi+1 = 07C      ; F2,0F,7C,/r HADDPS xmm1, xmm2/m128
            add esi 2 | Call MarkSSEdata SSE_4_F
            Mov D$edi 'hadd', D$edi+4 'ps ' | add edi 7
            jmp Dis_xmm1__xmm2_m128
        .Else_If B$esi+1 = 07D      ; F2,0F,7D,/r HSUBPS xmm1, xmm2/m128
            add esi 2 | Call MarkSSEdata SSE_4_F
            Mov D$edi 'hsub', D$edi+4 'ps ' | add edi 7
            jmp Dis_xmm1__xmm2_m128
        .Else_If B$esi+1 = 0C2       ; CMPSD xmm1, xmm2/m64, imm8
            add esi 2 | Mov D$edi 'cmps', W$edi+4 'd ' | add edi 6
            Call Dis_xmm1__xmm2_m64 | jmp WritePacketCondition
        .Else_If B$esi+1 = 0D0          ; F2,0F,D0,/r ADDSUBPS xmm1, xmm2/m128
            add esi 2 | Call MarkSSEdata SSE_4_F
            Mov D$edi 'adds', D$edi+4 'ubps', B$edi+8 SPC | add edi 9
            jmp Dis_xmm1__xmm2_m128
        .Else_If B$esi+1 = 0D6          ; F2 0F D6 MOVDQ2Q mm, xmm
            add esi 2 | Mov D$edi 'movd', D$edi+4 'q2q ' | add edi 8
            jmp Dis_mmx_xmm
        .Else_If B$esi+1 = 0E6       ; CVTPD2DQ xmm1, xmm2/m128
            add esi 2 | Call MarkSSEdata SSE_2_R
            Mov D$edi 'cvtp', D$edi+4 'd2dq', B$edi+8 SPC | add edi 9
            jmp Dis_xmm1__xmm2_m128
        .Else_If B$esi+1 = 0F0       ; F2,0F,F0,/r LDDQU xmm, mem
            add esi 2 | Mov D$edi 'lddq', D$edi+4 'u ' | add edi 6
            jmp Dis_xmm1__xmm2_m128  ; Xmm2 is dummy, here.
        .Else
            inc D$UnLikelyCode | ret
        .End_If

    ..Else_If B$esi = 0A6   ; F2 A6 REPNE CMPS m8, m8
        inc esi | Mov D$edi 'repn', D$edi+4 'e cm', D$edi+8 'psb ' | add edi 11
    ..Else_If W$esi = 0A766   ; F2 A7 REPNE CMPS m16, m16 ; F2 A7 REPNE CMPS m32, m32
        add esi 2 | Mov D$edi 'repn', D$edi+4 'e cm', D$edi+8 'psw ' | add edi 11
    ..Else_If B$esi = 0A7   ; F2 A7 REPNE CMPS m16, m16 ; F2 A7 REPNE CMPS m32, m32
        inc esi | Mov D$edi 'repn', D$edi+4 'e cm', D$edi+8 'psd ' | add edi 11
    ..Else_If B$esi = 0AE   ; F2 AE REPNE SCAS m8
        inc esi | Mov D$edi 'repn', D$edi+4 'e sc', D$edi+8 'asb ' | add edi 11
    ..Else_If W$esi = 0AF66   ; F2 AF REPNE SCAS m16 ; F2 AF REPNE SCAS m32
        add esi 2 | Mov D$edi 'repn', D$edi+4 'e sc', D$edi+8 'asw ' | add edi 11
    ..Else_If B$esi = 0AF   ; F2 AF REPNE SCAS m16 ; F2 AF REPNE SCAS m32
        inc esi | Mov D$edi 'repn', D$edi+4 'e sc', D$edi+8 'asd ' | add edi 11
    ..Else
        .If B$EscapePrefix = &TRUE
          ; 0F F2 /r PSLLD mm, mm/m64   ;  66 0F F2 /r PSLLD xmm1, xmm2/m128
            Mov D$edi 'psll', W$edi+4 'd ' | add edi 6 | jmp Dis_xmmx1__xmmx2_m64_128
        .Else
            inc D$UnLikelyCode | ret
        .End_If
    ..End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpF3:
    ..If B$esi = 0F
        .If B$esi+1 = 010       ; F3 0F 10 /r MOVSS xmm1, xmm2/m32
            add esi 2 | Call MarkSSEdata SSE_1_R
            Mov D$edi 'movs', W$edi+4 's ' | add edi 6
            jmp Dis_xmm1__xmm2_m32F
        .Else_If B$esi+1 = 011       ; F3 0F 11 /r MOVSS xmm2/m32, xmm
            add esi 2 | Call MarkSSEdata SSE_1_R
            Mov D$edi 'movs', W$edi+4 's ' | add edi 6
            jmp Dis_xmm2_m32F__xmm1
        .Else_If B$esi+1 = 012       ; F3,0F,12,/r MOVSLDUP xmm1, xmm2/m128
            add esi 2 | Call MarkSSEdata SSE_4_F
            Mov D$edi 'movs', D$edi+4 'ldup', B$edi+8 SPC | add edi 9
            jmp Dis_xmm1__xmm2_m128
        .Else_If B$esi+1 = 016       ; F3,0F,16,/r MOVSHDUP xmm1, xmm2/m128
            add esi 2 | Call MarkSSEdata SSE_4_F
            Mov D$edi 'movs', D$edi+4 'hdup', B$edi+8 SPC | add edi 9
            jmp Dis_xmm1__xmm2_m128
        .Else_If B$esi+1 = 051      ; F3 0F 51 /r SQRTSS xmm1, xmm2/m32
            add esi 2 | Call MarkSSEdata SSE_4_F
            Mov D$edi 'sqrt', D$edi+4 'ss  ' | add edi 7 | jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 052      ; F3 0F 52 /r RSQRTSS xmm1,xmm2/m32
            add esi 2 | Call MarkSSEdata SSE_4_F
            Mov D$edi 'rsqr', D$edi+4 'tss ' | add edi 8 | jmp Dis_xmm1__xmm2_m32
        .Else_If al = 053; F3 0F 53 /r RCPSS xmm1, xmm2/m32; F3 0F 53 /r RCPSS xmm1, xmm2/m32
            add esi 2 | Call MarkSSEdata SSE_4_F
            Mov D$edi 'rcpp', W$edi+4 's ' | add edi 6 | jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 058 ; F3 0F 58 /r ADDSS
            add esi 2 | Call MarkSSEdata SSE_1_F
            Mov D$edi 'adds', W$edi+4 's ' | add edi 6 | jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 059       ; F3 0F 59 /r MULSS xmm1, xmm2/m32
            add esi 2 | Call MarkSSEdata SSE_1_F
            Mov D$edi 'muls', W$edi+4 's ' | add edi 6
            jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 05C   ; F3 0F 5C /r SUBSS xmm1, xmm2/m32
            add esi 2 | Call MarkSSEdata SSE_4_F
            Mov D$edi 'subs', W$edi+4 's ' | add edi 6
            jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 05D       ; MINSS xmm1, xmm2/m32
            add esi 2 | Call MarkSSEdata SSE_1_F
            Mov D$edi 'mins', W$edi+4 's ' | add edi 6
            jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 05F      ; F3 0F 5F /r MAXSS xmm1, xmm2/m32
            add esi 2 | Call MarkSSEdata SSE_1_F
            Mov D$edi 'maxs', W$edi+4 's ' | add edi 6
            jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 070       ; F3 0F 70 /r ib PSHUFHW xmm1, xmm2/m128, imm8
            add esi 2 | Mov D$edi 'pshu', D$edi+4 'fhw ' | add edi 8
            Call Dis_xmm1__xmm2_m128 | Mov B$edi SPC | inc edi
            Call WriteImm8
        .Else_If B$esi+1 = 07E      ;  F3 0F 7E MOVQ xmm1, xmm2/m64
            add esi 2 | Mov D$edi 'movq', B$edi+4 SPC | add edi 5
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 0C2      ; CMPSD xmm1, xmm2/m64, imm8
            add esi 2 | Mov D$edi 'cmps', W$edi+4 's ' | add edi 6
            Call  Dis_xmm1__xmm2_m64 | jmp WritePacketCondition
        .Else_If B$esi+1 = 0D6        ; F3 0F D6 MOVQ2DQ xmm, mm
            add esi 2 | Mov D$edi 'movq', D$edi+4 '2dq ' | add edi 8
            jmp Dis_xmm_mmx
        .Else_If B$esi+1 = 0E6      ; CVTDQ2PD xmm1, xmm2/m64
            add esi 2 | Call MarkSSEdata SSE_2_D
            Mov D$edi 'cvtd', D$edi+4 'q2pd', B$edi+8 SPC | add edi 9
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 02A      ; CVTSI2SS xmm, r/m32
            add esi 2 | Call MarkSSEdata SSE_1_D
            Mov D$edi 'cvts', D$edi+4 'i2ss', B$edi+8 SPC | add edi 9
            jmp Dis_xmm1__rm32
        .Else_If B$esi+1 = 02C      ; CVTTSS2SI r32, xmm/m32
            add esi 2 | Call MarkSSEdata SSE_1_F
            Mov D$edi 'cvtt', D$edi+4 'ss2s', W$edi+8 'i ' | add edi 10
            jmp Dis_r32__xmm_m32
        .Else_If B$esi+1 = 05A      ; CVTSS2SD xmm1, xmm2/m32
            add esi 2 | Call MarkSSEdata SSE_1_F
            Mov D$edi 'cvts', D$edi+4 's2sd', B$edi+8 SPC | add edi 9
            jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 02D      ; CVTSS2SI r32, xmm/m32
            add esi 2 | Call MarkSSEdata SSE_1_F
            Mov D$edi 'cvts', D$edi+4 's2si', B$edi+8 SPC | add edi 9
            jmp Dis_r32__xmm_m32
        .Else_If B$esi+1 = 05B      ; CVTTPS2DQ xmm1, xmm2/m128
            add esi 2 | Call MarkSSEdata SSE_4_F
            Mov D$edi 'cvtt', D$edi+4 'ps2d', W$edi+8 'q ' | add edi 10
            jmp Dis_xmm1__xmm2_m128
        .Else_If B$esi+1 = 05E      ; DIVSS xmm1, xmm2/m32
            add esi 2 | Call MarkSSEdata SSE_1_F
            Mov D$edi 'divs', W$edi+4 's ' | add edi 6
            jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 06F      ; F3 0F 6F /r MOVDQU xmm1, xmm2/m128
            add esi 2 | Mov D$edi 'movd', D$edi+4 'qu  ' | add edi 7
            jmp Dis_xmm1__xmm2_m128
        .Else_If B$esi+1 = 07F      ; F3 0F 7F /r MOVDQU xmm2/m128, xmm1
            add esi 2 | Mov D$edi 'movd', D$edi+4 'qu  ' | add edi 7
            jmp Dis_xmm2_m128__xmm1
        .Else
            ret
        .End_If

    ..Else_If B$esi = 090   ; F3 90 PAUSE
        inc esi | Mov D$edi 'paus', B$edi+4 'e' | add edi 5
    ..Else_If W$esi = 0A766 ; F3 66 17
        add esi 2 | Mov D$edi 'rep ', D$edi+4 'cmps', B$edi+8 'w' | add edi 9
      ;  add esi 2 | Mov D$edi 'rep ', D$edi+4 'movs', B$edi+8 'w' | add edi 9
    ..Else_If W$esi = 0AF66 ; F3 66 17
        add esi 2 | Mov D$edi 'rep ', D$edi+4 'scas', B$edi+8 'w' | add edi 9
    ..Else_If W$esi = 0A566 ; F3 66 17
        add esi 2 | Mov D$edi 'rep ', D$edi+4 'movs', B$edi+8 'w' | add edi 9
      ;!!!!  add esi 2 | Mov D$edi 'rep ', D$edi+4 'cmps', B$edi+8 'w' | add edi 9
    ..Else_If B$esi = 06C       ; F3 6C REP INS r/m8, DX
        inc D$UnLikelyCode
        inc esi | Mov D$edi 'rep ', D$edi+4 'insb' | add edi 8 ; | jmp Dis_rm8_dx
    ..Else_If B$esi = 06D       ; F3 6D REP INS r/m16, DX ; F3 6D REP INS r/m32, DX
        inc D$UnLikelyCode
        inc esi | Mov D$edi 'rep ', D$edi+4 'insd' | add edi 8 ; | jmp Dis_rm32_rm16__dx
    ..Else_If W$esi = 06D66       ; F3 6D REP INS r/m16, DX ; F3 6D REP INS r/m32, DX
        inc D$UnLikelyCode
        add esi 2 | Mov D$edi 'rep ', D$edi+4 'insw' | add edi 8 ; | jmp Dis_rm32_rm16__dx
    ..Else_If B$esi = 06E       ; F3 6E REP OUTS DX, r/m8
        inc D$UnLikelyCode
        inc esi | Mov D$edi 'rep ', D$edi+4 'outs', B$edi+8 'b' | add edi 9 ;| jmp Dis_dx_rm8
    ..Else_If W$esi = 06F66       ; F3 6F REP OUTS DX, r/m16 ; F3 6F REP OUTS DX, r/m32
        inc D$UnLikelyCode
        add esi 2 | Mov D$edi 'rep ', D$edi+4 'outs', B$edi+8 'w' | add edi 9
    ..Else_If B$esi = 06F       ; F3 6F REP OUTS DX, r/m16 ; F3 6F REP OUTS DX, r/m32
        inc D$UnLikelyCode
        inc esi | Mov D$edi 'rep ', D$edi+4 'outs', B$edi+8 'd' | add edi 9 ;| jmp Dis_dx__rm32_rm16
    ..Else_If B$esi = 0A4       ; F3 A4 REP MOVS m8, m8
        inc esi | Mov D$edi 'rep ', D$edi+4 'movs', B$edi+8 'b' | add edi 9
    ..Else_If W$esi = 0A566       ; F3 A5 REP MOVS m16, m16 ; F3 A5 REP MOVS m32, m32
        add esi 2 | Mov D$edi 'rep ', D$edi+4 'movs', B$edi+8 'w' | add edi 9
    ..Else_If B$esi = 0A5       ; F3 A5 REP MOVS m16, m16 ; F3 A5 REP MOVS m32, m32
        inc esi | Mov D$edi 'rep ', D$edi+4 'movs', B$edi+8 'd' | add edi 9
    ..Else_If B$esi = 0A6       ; F3 A6 REPE CMPS m8, m8
        inc esi | Mov D$edi 'rep ', D$edi+4 'cmps', B$edi+8 'b' | add edi 9
    ..Else_If W$esi = 0A766       ; F3 A7 REPE CMPS m16, m16 ; F3 A7 REPE CMPS m32, m32
        add esi 2 | Mov D$edi 'rep ', D$edi+4 'cmps', B$edi+8 'w' | add edi 9
    ..Else_If B$esi = 0A7       ; F3 A7 REPE CMPS m16, m16 ; F3 A7 REPE CMPS m32, m32
        inc esi | Mov D$edi 'rep ', D$edi+4 'cmps', B$edi+8 'd' | add edi 9
    ..Else_If B$esi = 0AA       ; F3 AA REP STOS m8
        inc esi | Mov D$edi 'rep ', D$edi+4 'stos', B$edi+8 'b' | add edi 9
    ..Else_If W$esi = 0AB66       ; F3 AB REP STOS m16 ; F3 AB REP STOS m32
        add esi 2 | Mov D$edi 'rep ', D$edi+4 'stos', B$edi+8 'w' | add edi 9
    ..Else_If B$esi = 0AB       ; F3 AB REP STOS m16 ; F3 AB REP STOS m32
        inc esi | Mov D$edi 'rep ', D$edi+4 'stos', B$edi+8 'd' | add edi 9
    ..Else_If B$esi = 0AC       ; F3 AC REP LODS AL
        inc esi | Mov D$edi 'rep ', D$edi+4 'lods', B$edi+8 'b' | add edi 9
    ..Else_If W$esi = 0AD66       ; F3 AD REP LODS AX ; F3 AD REP LODS EAX
        add esi 2 | Mov D$edi 'rep ', D$edi+4 'lods', B$edi+8 'w' | add edi 9
    ..Else_If B$esi = 0AD       ; F3 AD REP LODS AX ; F3 AD REP LODS EAX
        inc esi | Mov D$edi 'rep ', D$edi+4 'lods', B$edi+8 'd' | add edi 9
    ..Else_If B$esi = 0AE       ; F3 AE REPE SCAS m8
        inc esi | Mov D$edi 'rep ', D$edi+4 'scas', B$edi+8 'b' | add edi 9
    ..Else_If W$esi = 0AF66       ; F3 AF REPE SCAS m16 ; F3 AF REPE SCAS m32
        add esi 2 | Mov D$edi 'rep ', D$edi+4 'scas', B$edi+8 'w' | add edi 9
    ..Else_If B$esi = 0AF       ; F3 AF REPE SCAS m16 ; F3 AF REPE SCAS m32
        inc esi | Mov D$edi 'rep ', D$edi+4 'scas', B$edi+8 'd' | add edi 9
    ..Else
        If B$EscapePrefix = &TRUE
            ; 0F F3 /r PSLLQ mm, mm/m64     ; 66 0F F3 /r PSLLQ xmm1, xmm2/m128
            Mov D$edi 'psll', W$edi+4 'q ' | add edi 6
            jmp Dis_xmmx1__xmmx2_m64_128
        Else
            ret
        End_If
    ..End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpF4: ; HLT
    If B$EscapePrefix = &TRUE
      ; 0F F4 /r PMULUDQ mm1, mm2/m64       ; 66 OF F4 /r PMULUDQ xmm1, xmm2/m128
        Mov D$edi 'pmul', D$edi+4 'udq ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    Else
        inc D$UnLikelyCode
        Mov D$edi 'hlt ' | add edi 3

    End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpF5: ; cmc
    If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
      ; 0F F5 /r PMADDWD mm, mm/m64       ; 66 0F F5 /r PMADDWD xmm1, xmm2/m128
        Mov D$edi 'pmad', D$edi+4 'dwd ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    Else
        Mov D$edi 'cmc ' | add edi 3  ; cmc

    End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpF6:
    .If B$EscapePrefix = &TRUE
      ; 0F F6 /r PSADBW mm1, mm2/m64        ; 66 0F F6 /r PSADBW xmm1, xmm2/m128
        ;inc D$UnLikelyCode
         Mov D$edi 'psad', D$edi+4 'bw  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        inc D$LikelyCode
        Mov bl B$esi | inc esi | DigitMask bl To al              ; ModRm with /6 ?

        If al = 0   ; F6 /0 ib TEST r/m8,imm8
            Mov D$edi 'test', B$edi+4 SPC | add edi 5 | jmp Dis_rm8_imm8
        Else_If al = 2       ; F6 /2 NOT r/m8
            Mov B$LockPrefix &FALSE
            Mov D$edi 'not ' | add edi 4
        Else_If al = 3       ; F6 /3 NEG r/m8
            Mov B$LockPrefix &FALSE
            Mov D$edi 'neg ' | add edi 4
        Else_If al = 4           ; F6 /4 MUL r/m8
            Mov D$edi 'mul ' | add edi 4
        Else_If al = 5           ; IMUL r/m8
            Mov D$edi 'imul', B$edi+4 SPC | add edi 5
        Else_If al = 6      ; DIV r/m8
            Mov D$edi 'div ' | add edi 4
        Else_If al = 7      ; IDIV r/m8
            Mov D$edi 'idiv', B$edi+4 SPC | add edi 5
        Else
            inc D$UnLikelyCode | ret
        End_If

        jmp EndWith.B.mem

    .End_If
ret


OpF7:
    ..If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        Mov D$edi 'mask' | Mov bl B$esi | inc esi
        If B$OperandSizeOverride = &TRUE    ; 66 0F F7 /r MASKMOVDQU xmm1, xmm2
            Mov D$edi+4 'movd', D$edi+8 'qu  ' | add edi 11
            jmp Dis_xmm1_xmm2
        Else                                ; 0F F7 /r MASKMOVQ mm1, mm2
            Mov D$edi+4 'movq', B$edi+8 SPC | add edi 9
            jmp Dis_mmx1_mmx2
        End_If


    ..Else
        inc D$LikelyCode
        Mov bl B$esi | inc esi | DigitMask bl To al              ; ModRm with /6 ?

        .If al = 0  ; F7 /0 iw TEST r/m16,imm16 ; F7 /0 id TEST r/m32,imm32
            Mov D$edi 'test', B$edi+4 SPC | add edi 5 | jmp Dis_rm32_rm16__imm32_imm16
        .Else_If al = 2      ; F7 /2 NOT r/m16
            Mov B$LockPrefix &FALSE
            Mov D$edi 'not ' | add edi 4
        .Else_If al = 3       ; F7 /3 NEG r/m16
            Mov B$LockPrefix &FALSE
            Mov D$edi 'neg ' | add edi 4
        .Else_If al = 4           ; F7 /4 MUL r/m16
            Mov D$edi 'mul ' | add edi 4
        .Else_If al = 5           ; IMUL r/m16
            Mov D$edi 'imul', B$edi+4 SPC | add edi 5
        .Else_If al = 6      ; DIV r/m32 r/m16
            Mov D$edi 'div ' | add edi 4
        .Else_If al = 7      ; IDIV r/m32 // r/m16
            Mov D$edi 'idiv', B$edi+4 SPC | add edi 5
        .Else
            inc D$UnLikelyCode | ret
        .End_If

        On B$OperandSizeOverride = &TRUE, jmp EndWith.W.mem
        jmp EndWith.D.mem

    ..End_If
ret



OpF8:
    If B$EscapePrefix = &TRUE
      ; 0F F8 /r PSUBB mm, mm/m64   ; 66 0F F8 /r PSUBB xmm1, xmm2/m128
        Mov D$edi 'psub', W$edi+4 'b ' | add edi 6 | jmp Dis_xmmx1__xmmx2_m64_128

    Else; clc
        Mov D$edi 'clc ' | add edi 3

    End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpF9:
    If B$EscapePrefix = &TRUE
      ; 0F F9 /r PSUBW mm, mm/m64   ; 66 0F F9 /r PSUBW xmm1, xmm2/m128
        Mov D$edi 'psub', W$edi+4 'w ' | add edi 6 | jmp Dis_xmmx1__xmmx2_m64_128

    Else       ; F9 STC
        Mov D$edi 'stc ' | add edi 3

    End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpFA:
    If B$EscapePrefix = &TRUE
      ; 0F FA /r PSUBD mm, mm/m64   ; 66 0F FA /r PSUBD xmm1, xmm2/m128
        Mov D$edi 'psub', W$edi+4 'd ' | add edi 6 | jmp Dis_xmmx1__xmmx2_m64_128

    Else
        inc D$UnLikelyCode
        Mov D$edi 'cli ' | add edi 3
    End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpFB:
    If B$EscapePrefix = &TRUE
      ; 0F FB /r PSUBQ mm1, mm2/m64 ; 66 0F FB /r PSUBQ xmm1, xmm2/m128
        Mov D$edi 'psub', W$edi+4 'q ' | add edi 6 | jmp Dis_xmmx1__xmmx2_m64_128

    Else       ; FB STI
        inc D$UnLikelyCode
        Mov D$edi 'sti ' | add edi 3

    End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpFC:
    If B$EscapePrefix = &TRUE
        ; 66 0F FC /r PADDB xmm1,xmm2/m128    ; 0F FC /r PADDB mm, mm/m64
        Mov D$edi 'padd', W$edi+4 'b ' | add edi 6 | jmp Dis_xmmx1__xmmx2_m64_128

    Else               ; cld
        Mov D$edi 'cld ' | add edi 3

    End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpFD:
    If B$EscapePrefix = &TRUE
        ; 66 0F FD /r PADDW xmm1, xmm2/m128     ;  0F FD /r PADDW mm, mm/m64
        Mov D$edi 'padd', W$edi+4 'w ' | add edi 6 | jmp Dis_xmmx1__xmmx2_m64_128

    Else       ; FD STD
        Mov D$edi 'std ' | add edi 3

    End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


OpFE:
    .If B$EscapePrefix = &TRUE
        ; 66 0F FE /r PADDD xmm1, xmm2/m128    ; 0F FE /r PADDD mm, mm/m64
        ;inc D$UnLikelyCode
        Mov D$edi 'padd', W$edi+4 'd ' | add edi 6 | jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        movzx ebx B$esi | inc esi | DigitMask bl to al
        If al = 0           ; INC r/m8
            Mov B$LockPrefix &FALSE
            Mov D$edi 'inc '
        Else_If al = 1      ; DEC r/m8
            Mov B$LockPrefix &FALSE
            Mov D$edi 'dec '
        Else
            inc D$UnLikelyCode | ret
        End_If

        add edi 4 | jmp EndWith.B.mem
    .End_If
ret

;075 00_01_110_101

OpFF:
    movzx ebx B$esi | inc esi | DigitMask bl to al

    .If al = 0           ; INC r/m16 / r/m32
        inc D$LikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'inc ' | add edi 4 | jmp EndWith.WD.mem

    .Else_If al = 1      ; DEC r/m32 // DEC r/m16
        inc D$LikelyCode
        Mov B$LockPrefix &FALSE
        Mov D$edi 'dec ' | add edi 4 | jmp EndWith.WD.mem

    .Else_If al = 2     ;  ; FF /2 Call r/m16 ; FF /2 Call r/m32
        inc D$LikelyCode
        Mov D$edi 'Call', B$edi+4 SPC | add edi 5
      ; add here the DLL Functions calls.
      ; If BL = 15 and D$esi inside .import Address Table
      ;             or D$esi points to a JMP Pointer To .import Address Table
        jmp L5> ;;;Call Dis_rm32_rm16

    .Else_If al = 3
        inc D$UnLikelyCode
        If B$OperandSizeOverride = &TRUE
            Mov D$edi 'Call', D$edi+4 'F W$' | add edi 6 ; jE! fix
        Else
            Mov D$edi 'Call', D$edi+4 'F D$' | add edi 6 ; jE! fix
        End_If
        Call WriteEffectiveAddressFromModRm         ; Mem pointing to 16:16/32
        Mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8

    .Else_If al = 4      ; JMP r/m16 ; ; JMP r/m32
        inc D$LikelyCode
        Mov B$DisEndOfChunk &TRUE
        Mov D$edi 'jmp ' | add edi 4

L5:     ;Push D$LastCodeRef
            Call Dis_rm32_rm16
;       ;Pop eax
;;
        cmp eax D$LastCodeRef | je L9>>
      ; This 'D$LastCodeRef' may be, for example, 'Mov eax D$Codexxxx' or 'D$Dataxxxx'.
            Mov ebx D$LastCodeRef
            cmp ebx D$DisCodeMin | jb L9>>
            cmp ebx D$DisCodeMax | ja L9>>
                Call ExtendToSideCodePointers

L9:         cmp ebx D$DisDataMin | jb L9>>
            cmp ebx D$DisDataMax | ja L9>>
                Call ExtendToSideDataPointers
;;

  ; OpEA: ; JMP ptr16:32, JMP ptr16:16 (jmp inter-segment).
    .Else_If al = 5      ; JMP m16:32
        inc D$UnLikelyCode
        If B$OperandSizeOverride = &FALSE
            Mov D$edi 'jmpF', D$edi+4 ' W$ ' | add edi 5 ; jE! fix
        Else
            Mov D$edi 'jmpF', D$edi+4 ' D$ ' | add edi 5 ; jE! fix
        End_If
        Mov B$DisEndOfChunk &TRUE
        Call WriteEffectiveAddressFromModRm
        Mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8

    .Else_If al = 6     ; FF /6 Push r/m16  ; FF /6 Push r/m32
        inc D$LikelyCode
        Mov D$edi 'Push', B$edi+4 SPC | add edi 5 | jmp Dis_m32_16

    .Else
        inc D$UnLikelyCode
        ret

    .End_If

L9: Mov B$DisFlag DISDONE+DISLINEOVER
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; Routines usable by several encodings:
____________________________________________________________________________________________
____________________________________________________________________________________________

Dis_rm8_r8:
    Mov W$DisSizeMarker 'B$'
    movzx ebx B$esi | inc esi
    Push ebx
        Call WriteEffectiveAddressFromModRm | Mov B$edi SPC | inc edi
    Pop ebx
    Call WriteByteRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_al_imm8:
    Mov D$edi 'al  ' | add edi 3 | Call WriteImm8
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_eax_ax__imm32_imm16:
    If B$OperandSizeOverride = &TRUE
        Mov D$edi 'ax  ' | add edi 3 | Call WriteImm16
    Else
        Mov D$edi 'eax ' | add edi 4 | Call WriteImm32
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_eax_ax__rd_rw:
  ; See: ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
  ;
  ; the Reg is given in the last Opcode Byte, in the BaseMask

    movzx eax B$esi-1 | and eax 00_000_111

    If B$OperandSizeOverride = &TRUE
        Mov D$edi 'ax  ' | add edi 3
        Mov eax D$WordRegsTable+eax*4 | stosd | dec edi
    Else
        Mov D$edi 'eax ' | add edi 4
        Mov eax D$dWordRegsTable+eax*4 | stosd
    End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm8_m8:
    Mov W$DisSizeMarker 'B$'
    movzx ebx B$esi | inc esi
    Push ebx
        Call WriteEffectiveAddressFromModRm | Mov B$edi SPC | inc edi
    Pop ebx
    Call WriteByteRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm32_rm16__r32_r16:
    On B$OperandSizeOverride = &TRUE, Mov W$DisSizeMarker 'W$'
    movzx ebx B$esi | inc esi
    Push ebx
        Call WriteEffectiveAddressFromModRm | Mov B$edi SPC | inc edi
    Pop ebx
    Call WritedWordRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Dis_rm32_rm16__r32_r16__imm8:
    Call Dis_rm32_rm16__r32_r16 | Mov B$edi SPC | inc edi
    Call WriteImm8      ; not signed this is for SHLD / SHRD
ret

Dis_rm32_rm16__r32_r16__cl:
    Call Dis_rm32_rm16__r32_r16 | Mov D$edi ' cl ' | add edi 3
ret

Dis_m32_r32:
    Mov W$DisSizeMarker 'D$'
    movzx ebx B$esi | inc esi
    Push ebx
        Call WriteEffectiveAddressFromModRm | Mov B$edi SPC | inc edi
    Pop ebx
    Call WritedWordRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_r8_rm8:
    Mov W$DisSizeMarker 'B$'
    movzx ebx B$esi | inc esi
    Call WriteByteRegsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWithModRm ;EndWith.D.mem
ret

Dis_r32_r16__rm32_rm16:
    On  B$OperandSizeOverride = &TRUE, Mov W$DisSizeMarker 'W$'
    movzx ebx B$esi | inc esi
    Call WritedWordRegsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWith.D.mem
ret

Dis_r32_r16__rm32_rm16_orNone__SignedImm8:
    On  B$OperandSizeOverride = &TRUE, Mov W$DisSizeMarker 'W$'
    movzx ebx B$esi | inc esi
    Call WritedWordRegsFromRegBits | Mov B$edi SPC | inc edi
    Test bl 00_11_000_000 ZERO L2>
        RegMask bl to al | RmMask bl to cl | ;on al = cl jmp L3>
            Call WriteEffectiveAddressFromModRm
            Mov B$edi SPC | inc edi | jmp L3>
L2: Call WriteEffectiveAddressFromModRm | Mov B$edi SPC | inc edi
L3: Call WriteSignedImm8
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_r32_r16__rm32_rm16_OrNone__SignedImm16_32:
    On B$OperandSizeOverride = &TRUE, Mov W$DisSizeMarker 'W$'
    movzx ebx B$esi | inc esi
    Call WritedWordRegsFromRegBits | Mov B$edi SPC | inc edi
    Test bl 00_11_000_000 ZERO L2>
        RegMask bl to al | RmMask bl to cl | ;on al = cl jmp L3>
            Call WriteEffectiveAddressFromModRm
            Mov B$edi SPC | inc edi | jmp L3>
L2: Call WriteEffectiveAddressFromModRm | Mov B$edi SPC | inc edi
L3: If B$OperandSizeOverride = &TRUE
        Call WriteSignedImm16
    Else
        Call WriteSignedImm32
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Dis_rm8_1:
    Mov W$DisSizeMarker 'B$' | jmp Dis_rm32_1
Dis_rm16_1:
    Mov W$DisSizeMarker 'W$'
Dis_rm32_1:
  ;  movzx ebx B$esi | inc esi
    Call WriteEffectiveAddressFromModRm | Mov W$edi ' 1' | add edi 2
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm8:
    Mov W$DisSizeMarker 'B$'
    movzx ebx B$esi | inc esi
    Call WriteEffectiveAddressFromModRm
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm8_dx:
    Mov W$DisSizeMarker 'B$' | jmp L1>
Dis_rm32_rm16__dx:
    On B$OperandSizeOverride = &TRUE, Mov W$DisSizeMarker 'W$'
L1: movzx ebx B$esi | inc esi
    Call WriteEffectiveAddressFromModRm | Mov D$edi ' dx ' | add edi 3
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_dx_rm8:
    Mov W$DisSizeMarker 'B$' | jmp L1>
Dis_dx__rm32_rm16:
    On B$OperandSizeOverride = &TRUE, Mov W$DisSizeMarker 'W$'
L1: Mov D$edi ' dx ' | add edi 3
    movzx ebx B$esi | inc esi
    Call WriteEffectiveAddressFromModRm
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm32_rm16__1:
    If B$OperandSizeOverride = &FALSE
        jmp Dis_rm32_1
    Else
        jmp Dis_rm16_1
    End_If

Dis_rm8_cl:
    Mov W$DisSizeMarker 'B$' | jmp Dis_rm32_cl
Dis_rm16_cl:
    Mov W$DisSizeMarker 'W$'
Dis_rm32_cl:
   ; movzx ebx B$esi | inc esi
    Call WriteEffectiveAddressFromModRm | Mov D$edi ' cl ' | add edi 3
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm32_rm16:
    On B$OperandSizeOverride = &TRUE, Mov W$DisSizeMarker 'W$'
    Call WriteEffectiveAddressFromModRm
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm32_rm16__cl:
    If B$OperandSizeOverride = &FALSE
        jmp Dis_rm32_cl
    Else
        jmp Dis_rm16_cl
    End_If

;Dis_rm16_imm8:
;    Mov W$DisSizeMarker 'W$'
;    movzx ebx B$esi | inc esi
;    Call WriteEffectiveAddressFromModRm | Mov B$edi SPC | inc edi
;    Call WriteImm8
;    Mov B$DisFlag DISDONE+DISLINEOVER
;ret

Dis_rm16_Sreg:
    Mov W$DisSizeMarker 'W$'
    movzx ebx B$esi | inc esi
    Push ebx
        Call WriteEffectiveAddressFromModRm | Mov B$edi SPC | inc edi
    Pop ebx
    Call WriteSregsFromModRm
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm32_Sreg:
    Mov W$DisSizeMarker 'D$'
    movzx ebx B$esi | inc esi
    Push ebx
        Call WriteEffectiveAddressFromModRm | Mov B$edi SPC | inc edi
    Pop ebx
    Call WriteSregsFromModRm
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Dis_Sreg_rm16:
    Mov W$DisSizeMarker 'W$'
    movzx ebx B$esi | inc esi
    Call WriteSregsFromModRm | Mov B$edi SPC | inc edi
    Call WriteEffectiveAddressFromModRm
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_Sreg_rm32:
    Mov W$DisSizeMarker 'D$'
    movzx ebx B$esi | inc esi
    Call WriteSregsFromModRm | Mov B$edi SPC | inc edi
    Call WriteEffectiveAddressFromModRm
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm8_imm8:
    Mov W$DisSizeMarker 'B$'
    Call WriteEffectiveAddressFromModRm | Mov B$edi SPC | inc edi
    Call WriteImm8
    Mov B$DisFlag DISDONE+DISLINEOVER

ret

Dis_Imm8:
    Call WriteSignedImm8
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_imm32_16:
    If B$OperandSizeOverride = &FALSE
        Call WriteImm32
    Else
        Call WriteImm16
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm32_rm16__imm32_imm16:
    On B$OperandSizeOverride = &TRUE, Mov W$DisSizeMarker 'W$'
    Call WriteEffectiveAddressFromModRm | Mov B$edi SPC | inc edi
    If B$OperandSizeOverride = &FALSE
        Call WriteImm32
    Else
        Call WriteImm16
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm32_rm16__imm8:
    On  B$OperandSizeOverride = &TRUE, Mov W$DisSizeMarker 'W$'
    Call WriteEffectiveAddressFromModRm | Mov B$edi SPC | inc edi
    Call WriteSignedImm8
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_r32_r16__rm8:
    On B$OperandSizeOverride = &TRUE, jmp Dis_r16_rm8
Dis_r32_rm8:
    Mov bl B$esi | inc esi
    Call WriteEregsFromRegBits | Mov B$edi SPC | inc edi
    Mov W$DisSizeMarker 'B$' | Call WriteEffectiveAddressFromModRm
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_r16_rm8:
    Mov bl B$esi | inc esi
    Call WriteWordregsFromRegBits | Mov B$edi SPC | inc edi
    Mov W$DisSizeMarker 'B$' | Call WriteEffectiveAddressFromModRm
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_r32_rm16:
    Mov bl B$esi | inc esi
    Call WriteEregsFromRegBits | Mov B$edi SPC | inc edi
    Mov W$DisSizeMarker 'W$' | Call WriteEffectiveAddressFromModRm
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_r32_xmmx:
    On B$OperandSizeOverride = &FALSE, jmp Dis_r32_mmx
Dis_r32_xmm:
    Mov bl B$esi | inc esi
    Call WriteEregsFromRegBits | Mov B$edi SPC | inc edi
    Call WriteXmmRegsFromRmBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_r32_mmx:
    Mov bl B$esi | inc esi
    Call WriteEregsFromRegBits | Mov B$edi SPC | inc edi
    Call WriteMMXRegsFromRmBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

; Some encodings like MOVMSKPD are in the form: 11 r32 mm
; Some like PMOVMSKB are: 11 mm r32.
; This 'Rev'ersed  form is for this second case:

; Was wronbg Doce of Intel-2001. Correct in Intel-2004
;;
Dis_r32_xmmx_Rev:
    On B$OperandSizeOverride = &FALSE, jmp Dis_r32_mmx_Rev
Dis_r32_xmmRev:
    Mov bl B$esi | inc esi
    Call WriteEregsFromRmBits | Mov B$edi SPC | inc edi
    Call WriteXmmRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_r32_mmx_Rev:
    Mov bl B$esi | inc esi
    Call WriteEregsFromRmBits | Mov B$edi SPC | inc edi
    Call WriteMMXRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret
;;

Dis_r32_xmmx_imm8:
    On B$OperandSizeOverride = &FALSE, jmp Dis_r32_mmx_imm8
Dis_r32_xmm_imm8:
    Mov bl B$esi | inc esi
    Call WriteEregsFromRmBits | Mov B$edi SPC | inc edi
    Call WriteXmmRegsFromRegBits | jmp L9>

Dis_r32_mmx_imm8:
    Mov bl B$esi | inc esi
    Call WriteEregsFromRmBits | Mov B$edi SPC | inc edi
    Call WriteMMXRegsFromRegBits
L9: Mov B$edi SPC | inc edi | Call Writeimm8
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmmx_r32_imm8:
    On B$OperandSizeOverride = &FALSE, jmp Dis_r32_mmx_imm8
Dis_xmm_r32_imm8:
    Mov bl B$esi | inc esi
    Call WriteXmmRegsFromRmBits | Mov B$edi SPC | inc edi
    Call WriteEregsFromRegBits | jmp L9>

Dis_mmx_r32_imm8:
    Mov bl B$esi | inc esi
    Call WriteMMXRegsFromRmBits | Mov B$edi SPC | inc edi
    Call WriteEregsFromRegBits
L9: Mov B$edi SPC | inc edi | Call Writeimm8
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_PINSRW:
    Mov W$DisSizeMarker 'D$'
    Mov bl B$esi | inc esi

    On B$OperandSizeOverride = &FALSE, jmp L2>

    Call WriteXmmRegsFromRegBits | Mov B$edi SPC | inc edi
    Push edi ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        Call WriteEffectiveAddressFromModRm | jmp L9>

L2:     Call WriteMMXRegsFromRegBits | Mov B$edi SPC | inc edi
    Push edi ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        Call WriteEffectiveAddressFromModRm

L9:     Mov B$edi SPC | inc edi | Call Writeimm8
    Pop eax ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    On B$eax = 'D', Mov B$eax 'W' ; <<<<<<<<<< Old EDI.
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


Dis_r32__xmm_m64:
    Mov bl B$esi | inc esi
    Call WriteEregsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWith.X.XMMmem
ret

Dis_r32__xmm_m32:
    Mov bl B$esi | inc esi
    Call WriteEregsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWith.X.XMMmem
ret

Dis_xmm1__xmm2_m128:
    Mov bl B$esi | inc esi
    Call WriteXMMRegsFromRegBits | Mov B$edi SPC | inc edi
    ModMask bl to al
    If al = 3
        Call WriteXMMRegsFromRmBits
    Else
        Call EndWith.X.XMMmem
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_mm1__mm2_m128:
    Mov bl B$esi | inc esi
    Call WriteMMXRegsFromRegBits | Mov B$edi SPC | inc edi
    ModMask bl to al
    If al = 3
        Call WriteMMXRegsFromRmBits
    Else
        Call EndWith.Q.mem
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmm1__xmm2_m128__imm8:
    Call Dis_xmm1__xmm2_m128 | Mov B$edi SPC | inc edi
    Call WriteImm8
ret

Dis_xmm2_m128__xmm1:
    Mov W$DisSizeMarker 'X$'
    Mov bl B$esi | inc esi
    Push ebx
        Call WriteEffectiveXMMAddressFromModRm | Mov B$edi SPC | inc edi
    Pop ebx
    Call WriteXMMRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmm1__xmm2_m64:
    Mov bl B$esi | inc esi
    Call WriteXMMRegsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWith.X.XMMmem
ret

Dis_xmm1__xmm2_m32:
    Mov bl B$esi | inc esi
    Call WriteXMMRegsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWith.X.XMMmem
ret

Dis_xmm1__xmm2_m32F:
    Mov bl B$esi | inc esi
    Call WriteXMMRegsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWith.F.mem
ret

Dis_mmx_rm32:
    Mov bl B$esi | inc esi
    Call WriteMMXRegsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWith.D.mem
ret

Dis_xmm_rm32:
    Mov bl B$esi | inc esi
    Call WriteXMMRegsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWith.D.mem
ret

Dis_rm32_xmmx:
    On B$OperandSizeOverride = &TRUE, jmp Dis_rm32_xmm
Dis_rm32_mmx:
    Mov bl B$esi | inc esi
    Mov W$DisSizeMarker 'D$'
    Push ebx
        Call WriteEffectiveAddressFromModRm | Mov B$edi SPC | inc edi
    Pop ebx
    Call WriteMMXRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm32_xmm:
    Mov bl B$esi | inc esi
    Mov W$DisSizeMarker 'D$'
    Push ebx
        Call WriteEffectiveAddressFromModRm | Mov B$edi SPC | inc edi
    Pop ebx
    Call WriteXMMRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_mmx1__xmm2_m128:
    Mov bl B$esi | inc esi
    Call WriteMMXRegsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWith.X.XMMmem
ret

Dis_mmx1__xmm2_m64:
    Mov bl B$esi | inc esi
    Call WriteMMXRegsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWith.X.XMMmem
ret

Dis_xmm2_m64__xmm1:
    Mov bl B$esi | inc esi
    Mov W$DisSizeMarker 'X$'
    Push ebx
        Call WriteEffectiveXMMAddressFromModRm | Mov B$edi SPC | inc edi
    Pop ebx
    Call WriteXMMRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmm2_m32__xmm1:
    Mov bl B$esi | inc esi
    Mov W$DisSizeMarker 'D$'
    Push ebx
        Call WriteEffectiveAddressFromModRm | Mov B$edi SPC | inc edi
    Pop ebx
    Call WriteXMMRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmm2_m32F__xmm1:
    Mov bl B$esi | inc esi
    Mov W$DisSizeMarker 'F$'
    Push ebx
        Call WriteEffectiveAddressFromModRm | Mov B$edi SPC | inc edi
    Pop ebx
    Call WriteXMMRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_mmx1__mmx2_m64:
    Mov bl B$esi | inc esi
    Call WriteMMXRegsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWith.Q.MMXmem

Dis_mmx1__mmx2_m64_v2: ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Looki
    Mov bl B$esi | inc esi | inc esi
    Call WriteMMXRegsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWith.Q.MMXmem

Dis_xmmx1__xmmx2_m64_128:
    If B$OperandSizeOverride = &TRUE
        jmp Dis_xmm1__xmm2_m128
    Else
        jmp Dis_mmx1__mmx2_m64
    End_If

Dis_xmm1__xmm_m128:
    Mov bl B$esi | inc esi
    Call WriteXMMRegsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWith.X.XMMmem

Dis_mmx1__mmx2_m128:
    Mov bl B$esi | inc esi
    Call WriteMMXRegsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWith.X.MMXmem

Dis_mmx_xmm:
    Mov bl B$esi | inc esi
    RegMask bl to al
    Mov D$edi 'MMX0', B$edi+4 SPC | add B$edi+3 al | add edi 5
    BaseMask bl to al
    Mov D$edi 'XMM0', B$edi+4 SPC | add B$edi+3 al | add edi 4
ret

Dis_xmm_mmx:
    Mov bl B$esi | inc esi
    RegMask bl to al
    Mov D$edi 'XMM0', B$edi+4 SPC | add B$edi+3 al | add edi 5
    BaseMask bl to al
    Mov D$edi 'MM0 ' | add B$edi+2 al | add edi 3
ret

Dis_xmm1__mmx2_m64:
Dis_xmm1__mmx2_m128:
    Mov bl B$esi | inc esi
    Call WriteXMMRegsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWith.X.XMMmem
ret

Dis_mmx2_m128__xmm1:
    Mov bl B$esi | inc esi
    Mov W$DisSizeMarker 'X$'
    Push ebx
        Call WriteEffectiveXMMAddressFromModRm | Mov B$edi SPC | inc edi
    Pop ebx
    Call WriteXMMRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_mmx1_m64__mmx2:
    Mov bl B$esi | inc esi
    Mov W$DisSizeMarker 'Q$'
    Push ebx
        Call WriteEffectiveMMXAddressFromModRm | Mov B$edi SPC | inc edi
    Pop ebx
    Call WriteMMXRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmm1__rm32:
    Mov bl B$esi | inc esi
    Call WriteXMMRegsFromRegBits | Mov B$edi SPC | inc edi | jmp EndWith.D.mem
ret

Dis_xmmx_imm8:
    On B$OperandSizeOverride = &FALSE, jmp Dis_mmx_imm8

Dis_xmm_imm8:
   ; Mov bl B$esi | inc esi
    Call WriteXMMRegsFromRmBits | Mov B$edi SPC | inc edi
    Call WriteImm8
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_mmx_imm8:
   ; Mov bl B$esi | inc esi
    Call WriteMMXRegsFromRmBits | Mov B$edi SPC | inc edi
    Call WriteImm8
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_m64:
    Mov W$DisSizeMarker 'Q$'
    Call WriteEffectiveAddressFromModRm
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_m32_16:
    On B$OperandSizeOverride = &TRUE, Mov W$DisSizeMarker 'W$'
    Call WriteEffectiveAddressFromModRm
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_m8:
    Mov W$DisSizeMarker 'B$'
    Call WriteEffectiveAddressFromModRm
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


WritePacketCondition:
    Mov B$edi SPC | inc edi | lodsb | and eax 0FF

    Push eax
        Call WriteEax
    Pop eax

    Mov D$edi ' ; (' | add edi 4

    If al = 0
        Mov D$edi 'EQ) ' | add edi 3
    Else_If al = 1
        Mov D$edi 'LT) ' | add edi 3
    Else_If al = 2
        Mov D$edi 'LE) ' | add edi 3
    Else_If al = 3
        Mov D$edi 'UNOR', W$edi 'D)' | add edi 6
    Else_If al = 4
        Mov D$edi 'NEQ)' | add edi 4
    Else_If al = 5
        Mov D$edi 'NLT)' | add edi 4
    Else_If al = 6
        Mov D$edi 'NLE)' | add edi 4
    Else_If al = 7
        Mov D$edi 'ORD)' | add edi 4
    Else
        add D$UnlikelyCode 5
    End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret


EndWith.B.mem:
    Mov W$DisSizeMarker 'B$' | jmp EndWithModRm
ret

EndWith.W.mem:
    Mov W$DisSizeMarker 'W$'
EndWith.D.mem:
EndWithModRm:
    Call WriteEffectiveAddressFromModRm
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

EndWith.WD.mem:
    On B$OperandSizeOverride = &TRUE, jmp EndWith.W.mem
    Call WriteEffectiveAddressFromModRm
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

EndWith.Q.mem:
    Mov W$DisSizeMarker 'Q$' | jmp EndWithModRm
ret

EndWith.F.mem:
    Mov W$DisSizeMarker 'F$' | jmp EndWithModRm
ret

EndWith.R.mem:
    Mov W$DisSizeMarker 'R$' | jmp EndWithModRm
ret

EndWith.T.mem:
    Mov W$DisSizeMarker 'T$' | jmp EndWithModRm
ret

EndWith.X.mem:
    Mov W$DisSizeMarker 'X$' | jmp EndWithModRm
ret

EndWith.X.XMMmem:
EndWith.X.MMXmem:
    Mov W$DisSizeMarker 'X$'
    ModMask bl to al
    If al = 3
        Call WriteXMMregsFromRmBits
    Else
        Call EndWithModRm
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

EndWith.Q.MMXmem:
    Mov W$DisSizeMarker 'Q$'
    ModMask bl to al
    If al = 3
        Call WriteMMXregsFromRmBits
    Else
        Call EndWithModRm
    End_If
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmmx1_xmmx2:
    On B$OperandSizeOverride = &TRUE, jmp Dis_xmm1_xmm2

Dis_mmx1_mmx2:
    Call WriteMMXRegsFromRegBits | Mov B$edi SPC | inc edi
    Call WriteMMXRegsFromRmBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmm1_xmm2:
    Call WriteXMMRegsFromRegBits | Mov B$edi SPC | inc edi
    Call WriteXMMRegsFromRmBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmm_m64:
    Call WriteXMMRegsFromRegBits | Mov B$edi SPC | inc edi
    Mov W$DisSizeMarker 'X$' | jmp EndWithModRm
ret

Dis_m64_xmm:
    Mov W$DisSizeMarker 'X$' | Call WriteEffectiveAddressFromModRm
    Mov B$edi SPC | inc edi
    Call WriteXMMRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_m64_mmx:
    Mov W$DisSizeMarker 'Q$' | Call WriteEffectiveAddressFromModRm
    Mov B$edi SPC | inc edi
    Call WriteMMXRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_m128_xmm:
    Mov W$DisSizeMarker 'X$' | Call WriteEffectiveAddressFromModRm
    Mov B$edi SPC | inc edi
    Call WriteXMMRegsFromRegBits
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; Now come the terminal writing Routines.
____________________________________________________________________________________________
____________________________________________________________________________________________


[NextDisLine | Push eax | Mov eax 020200A0D | stosd | Mov eax '    ' | stosw | Pop eax]

[DisFlag: D$ ?
 SegmentOverride: D$ ?
 OperandSizeOverride: D$ ?
 AddressSizeOverride: D$ ?
 EscapePrefix: D$ ?]

[DISDONE 1    DISLINEOVER 2    DISFAILED 0]

; Reads a Byte, returns the Hexa in ax. If The Hexa is greater than '0F', writes the
; wanted leading '0'.

[STR.A.Hexa: B$ '0123456789ABCDEF']

OpToHexa:
    movzx eax B$esi | inc esi
LoadedOpToHexa:             ; If the Byte has already been loaded for some caller test.
    Mov ebx eax | shr ebx 4
    and eax 0F | and ebx 0F
    Mov al B$STR.A.Hexa+eax, bl B$STR.A.Hexa+ebx
    If ebx > '0'
        Mov B$edi '0' | inc edi
    End_If
    shl eax 8 | or eax ebx
ret


[ModMask | Mov #3 #1 | and #3 0011_000_000 | shr #3 6]
[DigitMask | Mov #3 #1 | and #3 00_111_000 | shr #3 3]
[RegMask   | Mov #3 #1 | and #3 00_111_000 | shr #3 3]
[RmMask    | Mov #3 #1 | and #3 00_000_111]

[ScaleMask | Mov #3 #1 | and #3 00_11_000_000 | shr #3 6]
[IndexMask | Mov #3 #1 | and #3 00_111_000 | shr #3 3]
[BaseMask  | Mov #3 #1 | and #3 00_000_111]
;;
 'DisSizeMarker' is set to 'D$' by default. If a 066 OpCode is encouneted, 'DisSizeMarker'
 is set to 'W$' at that time. So, All the OpCode Routines have the overwrite this Marker
 only in case of Bytes (which is always specific to one given OpCode).
;;
[DisSizeMarker: D$ ?]


[dWordRegsTable: B$ 'eax ecx edx ebx esp ebp esi edi '
 WordRegsTable: B$ 'ax  cx  dx  bx  sp  bp  si  di  '
 ByteRegsTable: B$ 'al  cl  dl  bl  ah  ch  dh  bh  '
 SregsTable: B$ 'es  cs  ss  ds  fs  gs          ']

WriteEregsFromRmBits:
    RmMask bl To al | and eax 0FF
    Move D$edi D$dWordRegsTable+eax*4 | add edi 3
ret

WriteEregsFromRegBits:
    RegMask bl To al | and eax 0FF
    Move D$edi D$dWordRegsTable+eax*4 | add edi 3
ret

WritedWordRegsFromRegBits:
    On B$OperandSizeOverride = &TRUE, jmp WriteWordRegsFromRegBits
    RegMask bl To al | and eax 0FF
    Move D$edi D$dWordRegsTable+eax*4 | add edi 3
Ret

WriteSregsFromModRm:
    RegMask bl To al | and eax 0FF
    Move D$edi D$SregsTable+eax*4 | add edi 2  ; 0010001110 08C 00111
ret

WriteWordRegsFromRegBits:
    RegMask bl To al | and eax 0FF
    Move D$edi D$WordRegsTable+eax*4 | add edi 2

Ret

WriteMMXRegsFromRegBits:
    RegMask bl To al
    Mov D$edi 'MM0 ' | add B$edi+2 al | add edi 3
Ret

WriteMMXRegsFromRmBits:
    RmMask bl To al
    Mov D$edi 'MM0 ' | add B$edi+2 al | add edi 3
Ret

WriteXMMRegsFromRegBits:
    RegMask bl To al
    Mov D$edi 'XMM0' | add B$edi+3 al | add edi 4
Ret

WriteXMMRegsFromRmBits:
    RmMask bl To al
    Mov D$edi 'XMM0' | add B$edi+3 al | add edi 4
Ret

WriteWordRegsFromRmBits:
    RmMask bl To al | and eax 0FF
    Move D$edi D$WordRegsTable+eax*4 | add edi 2
ret

WriteByteRegsFromRmBits:
    RmMask bl To al | and eax 0FF
    Move D$edi D$ByteRegsTable+eax*4 | add edi 2
ret

WriteByteRegsFromRegBits:
    RegMask bl To al | and eax 0FF
    Move D$edi D$ByteRegsTable+eax*4 | add edi 2
ret


; Writes, for example: 'D$', 'CS:B$cs:', ...
StartEffectiveAddress:
    Move W$edi W$DisSizeMarker | add edi 2

    If D$SegmentOverride <> 0
        Move D$edi D$SegmentOverride | add edi 3
    End_If
ret


WriteFromSib: ; 085  00_10_000_101
    ModMask bl To cl                                ; Saves the MOD Bits for Base5 case.
    lodsb | Mov bl al | BaseMask bl To al ; 025 00_00_100_101

    If al = 0      | Mov D$edi 'eax+' | add edi 4
    Else_If al = 1 | Mov D$edi 'ecx+' | add edi 4
    Else_If al = 2 | Mov D$edi 'edx+' | add edi 4
    Else_If al = 3 | Mov D$edi 'ebx+' | add edi 4
    Else_If al = 4 | Mov D$edi 'esp+' | add edi 4
    Else_If al = 5 | Call Base5                     ; No Base or ebp with dis 8 or 32
    Else_If al = 6 | Mov D$edi 'esi+' | add edi 4
    Else           | Mov D$edi 'edi+' | add edi 4
    End_If

    IndexMask bl To al

    .If al = 0      | Mov D$edi 'eax ' | add edi 3
    .Else_If al = 1 | Mov D$edi 'ecx ' | add edi 3
    .Else_If al = 2 | Mov D$edi 'edx ' | add edi 3
    .Else_If al = 3 | Mov D$edi 'ebx ' | add edi 3
    .Else_If al = 4
        If cl = 0FF
          ; Example: cmp ah B$078
            On W$DisSizeMarker <> 'D$', inc D$UnlikelyCode
        End_If
        On B$edi-1 = '+', dec edi             ; None / Strip the '+'
        jmp L9>
    .Else_If al = 5 | Mov D$edi 'ebp ' | add edi 3
    .Else_If al = 6 | Mov D$edi 'esi ' | add edi 3
    .Else           | Mov D$edi 'edi ' | add edi 3
    .End_If

    ScaleMask bl To al

    If al = 0                                       ; no need *1
    Else_If al = 1 | Mov W$edi '*2' | add edi 2
    Else_If al = 2 | Mov W$edi '*4' | add edi 2
    Else_If al = 3 | Mov W$edi '*8' | add edi 2
    End_If
L9: ret


Base5:          ; No Base or ebp with dis 8 or 32 (cl is the MOD bits from previous ModRm).
    If cl <> 0
        Mov D$edi 'ebp+' | add edi 4
    Else
        Mov cl 0FF
    End_If
ret


[ToJumpsTable: D$ ?] ; Might now be removed entirely.

WriteBase5dis32:
    If B$edi-1 <> '+'
        Mov B$edi '+' | inc edi
    End_If

WriteDis32:
    If B$AddressSizeOverride = &FALSE
        lodsd
    Else
WriteDis16:
        lodsw | and eax 0FFFF | Call WriteEax | ret
    End_If

WriteDisRelative:
    Mov D$LastCodeRef eax | On eax = 0, jmp L8>>

L0: If B$SimpleScan = &TRUE
        Mov B$DisFlag DISDONE+DISLINEOVER | ret
    End_If

L0: On B$WeAreInTheCodeBox = &TRUE, jmp L8>>
;On eax = 01013E38, int3
    sub eax D$DisImageBase | add eax D$SectionsMap

    On eax >= D$EndOfSectionsMap, jmp L8>>
    On eax <= D$SectionsMap, jmp L8>>

    Mov B$ToJumpsTable &FALSE
    Mov al B$eax | and al DATAFLAG+VIRTUALFLAG+IMPORTFLAG+CODEFLAG
;map
    ..If al = 0
        Mov eax D$LastCodeRef

        Mov ebx eax | sub ebx D$DisImageBase | add ebx D$SizesMap

            .If B$LeaInstruction = &TRUE
                Mov B$LeaInstruction &FALSE
                sub ebx D$SizesMap | add ebx D$SectionsMap | Mov B$ebx DATAFLAG
            .Else_If W$edi-2 = 'B$'
                or B$ebx BYTE
                sub ebx D$SizesMap | add ebx D$SectionsMap | Mov B$ebx DATAFLAG
            .Else_If W$edi-2 = 'W$'
                or B$ebx WORD
                sub ebx D$SizesMap | add ebx D$SectionsMap
                Mov B$ebx DATAFLAG, B$ebx+1 DATAFLAG
            .Else_If W$edi-2 = 'D$'
                or B$ebx DWORD
                sub ebx D$SizesMap | add ebx D$SectionsMap
                Mov D$ebx FOURDATAFLAGS
            .Else_If W$edi-2 = 'F$'
                or B$ebx FP4
                sub ebx D$SizesMap | add ebx D$SectionsMap
                Mov D$ebx FOURDATAFLAGS
            .Else_If W$edi-2 = 'R$'
                or B$ebx FP8
                sub ebx D$SizesMap | add ebx D$SectionsMap
                Mov D$ebx FOURDATAFLAGS, D$ebx+4 FOURDATAFLAGS
            .Else_If W$edi-2 = 'T$'
                or B$ebx FP10
                sub ebx D$SizesMap | add ebx D$SectionsMap
                Mov D$ebx FOURDATAFLAGS, D$ebx+4 FOURDATAFLAGS, D$ebx+6 FOURDATAFLAGS
            .Else
                jmp L8>>
               ; or B$ebx POINTER
            .End_If

    ..Else_If al = DATAFLAG
        Mov eax D$LastCodeRef | Call StoreDisSize
        sub eax D$DisImageBase | add eax D$RoutingMap | or B$eax LABEL+EVOCATED
        Mov D$edi 'Data' | add edi 4 | jmp L8>>

    ..Else_If al = CODEFLAG
      ; Is it a Call to a Jumps Table?
        Mov eax D$LastCodeRef
        sub eax D$DisImageBase | add eax D$UserPeStart

        If W$eax = 025FF ; Code of jmp relative long
            Mov ebx D$eax+2 | sub ebx D$DisImageBase | add ebx D$SectionsMap
            On ebx > D$EndOfSectionsMap, jmp L1>
            On ebx < D$SectionsMap, jmp L1>
                On B$ebx <> IMPORTFLAG, jmp L1>

                    Mov B$ApiCommentWanted &TRUE, ebx D$eax+2, D$PtrToApiName ebx
        End_If

L1:     Mov eax D$LastCodeRef
        sub eax D$DisImageBase | add eax D$RoutingMap
        test B$eax INSTRUCTION ZERO L8>>
        or B$eax NODE+LABEL | Mov D$edi 'Code' | add edi 4 | jmp L8>>

    ..Else_If al = VIRTUALFLAG
        Mov eax D$LastCodeRef | Call StoreDisSize
        sub eax D$DisImageBase | add eax D$RoutingMap | or B$eax LABEL+EVOCATED
        Mov D$edi 'Virt', D$edi+4 'ual ' | add edi 7 | jmp L8>>

    ..Else_If al = IMPORTFLAG
        Mov ebx D$LastCodeRef
L5:     sub ebx D$DisImageBase | add ebx D$UserPeStart | Mov ebx D$ebx
      ; May be a wrong pointing inside the .Import!
      ; Add a Pointer test!
        On ebx < D$ApiBuffer, ret
        On ebx >= D$EndOfApiBuffer, ret

        Push esi

            On W$edi-2 = 'D$', sub edi 2

            Mov esi ebx

            .If D$edi-4 = 'jmp '    ; Jumps Table?
                Call WriteApiJmpTableLabel

                While B$esi <> 0 | movsb | End_While

            .Else_If D$edi-4 = 'all '  ; Call api?
                Call FlagNoReturnApi
                Call FlagApiProcedures
                While B$esi <> 0 | movsb | End_While

            .Else_If D$edi-4 = '$cs:'
                sub edi 5 | jmp L6>

          ; Other case: Either "Mov eax D$ApiCall" or "Mov D$eax ApiCall"
            .Else
                Push edi
                    Mov al "'"
                    While B$edi > LF
                        dec edi
                        On B$edi = '$', Mov al 0
                    End_While
                Pop edi
L6:             Mov esi ebx
                If al = 0
                    While B$esi <> '.' | inc esi | End_While | inc esi
                End_If
                While B$esi <> 0 | movsb | End_While
                On al = 0, dec edi

            .End_If
        Pop esi

     ..End_If
ret

L8: On B$WeAreInTheCodeBox = &FALSE, jmp L8>

    INT3 | NOP ; ANCIEN TITLE LIB
;;
    
    On D$LibFileMemory = 0, jmp L8>
        Push esi
            Mov esi D$LastCodeRef
            .If esi > D$LibFileMemory
                Mov eax D$LibFileMemory | add eax D$LibFileLength
                If esi < eax
                    While B$esi <> 0 | movsb | End_While
                   ; Mov D$edi ' ; <', D$edi+4 '<<<<' | add edi 8
                Else
                    Pop esi | jmp L8>
                End_If
            .Else
                Pop esi | jmp L8>
            .End_If
        Pop esi
        ret
;;

L8: ;On D$LastCodeRef = 0438E28, int3

    .If B$edi-1 = '+'
        If W$edi-3 = '*2'
            Call TryWithIndice 2
        Else_If W$edi-3 = '*4'
            Call TryWithIndice 4
        Else_If W$edi-3 = '*8'
            Call TryWithIndice 8
        End_If
    .End_If

    If W$DisplacementFromLabel = 0
        Mov eax D$LastCodeRef | sub eax D$DisImageBase | add eax D$SizesMap
        Mov ebx D$SizesMap | add ebx 4
        On eax < ebx, jmp L8>
        On eax > D$EndOfSizesMap, jmp L8>
        test B$eax-4 FP8 ZERO L8>
            sub D$LastCodeRef 4 | Mov W$DisplacementFromLabel '+4'
    End_If

L8: Push 0-1

    Mov ebx D$LastCodeRef

L0: Mov eax ebx | shr ebx 4 | and eax 0F
    add eax '0' | On eax > '9', add eax 7
    Push eax
    cmp ebx 0 | ja L0<

    Mov B$edi '0' | inc edi
L0: Pop eax | cmp eax 0-1 | je L9>
    Mov B$edi al | inc edi | jmp L0<

L9: Mov ebx D$LastCodeRef | sub ebx D$DisImageBase | add ebx D$UserPeStart

    ..If ebx > D$UserPeStart
        .If ebx < D$UserPeEnd
            Mov ebx D$LastCodeRef | ToStringsMapFrom DisImageBase, ebx
            If D$ebx <> 0
                Push esi
                ;On D$LastCodeRef = 04037EC, showme D$ebx

                Push esi | Mov esi D$ebx | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0: Pop esi

                Pop esi
            End_If
        .End_If
    ..End_If

    If W$DisplacementFromLabel <> 0
        Mov ax W$DisplacementFromLabel | stosw
        Mov W$DisplacementFromLabel 0
    End_If

    Mov eax D$LastCodeRef | sub eax D$DisImageBase | add eax D$SectionsMap

    .If eax < D$SectionsMap
        ;
    .Else_If eax < D$EndOfSectionsMap
        If B$eax = DATAFLAG
            sub eax D$SectionsMap | add eax D$RoutingMap | or B$eax LABEL+EVOCATED
        End_If
    .End_If

    On B$ApiCommentWanted = &TRUE, Call WriteApiLabelComment
ret


[DisplacementFromLabel: D$ ?]

Proc TryWithIndice:
    Argument @Indice

        Mov eax D$LastCodeRef | add eax D@Indice
        sub eax D$DisImageBase | add eax D$SectionsMap

        ..If eax < D$SectionsMap
            ;
        ..Else_If eax < D$EndOfSectionsMap
            .If B$eax = DATAFLAG
                sub eax D$SectionsMap | add eax D$RoutingMap
                test B$eax LABEL+EVOCATED ZERO L9>
                    Mov eax D@Indice | add D$LastCodeRef eax
                    If eax = 2
                        Mov W$DisplacementFromLabel '-2'
                    Else_If eax = 4
                        Mov W$DisplacementFromLabel '-4'
                    Else_If eax = 8
                        Mov W$DisplacementFromLabel '-8'
                    End_If

                    Mov D$edi 'Data' | add edi 4
            .End_If
        ..End_If
L9:
EndP
____________________________________________________________________________________________

WriteApiLabel:
    While B$esi <> 0
        lodsb
;;
  ; No need as long as they are all Comments, now:
        If al = '$'
            Mov al 'S'
        Else_If al = '$'
            Mov al 'S'
        Else_If al = '@'
            Mov al 'a'
        End_If
;;
        stosb
    End_While
ret

[ApiCommentWanted: D$ ?
 PtrToApiName: D$ ?]

WriteApiLabelComment:
    Push esi
        Mov B$ApiCommentWanted &FALSE

        Mov D$edi ' ; ' | add edi 3
        Mov esi D$PtrToApiName
        sub esi D$DisImageBase | add esi D$UserPeStart
        Mov esi D$esi
        Push esi
            Call WriteApiLabel
        Pop esi
        Call FlagApiProcedures
    Pop esi
ret


WriteApiJmpTableLabel:
    While B$edi-1 > LF | dec edi | End_While

    Push esi
        Mov W$edi '; ' | add edi 2

        Mov esi ebx | inc esi

        Call WriteApiLabel
    Pop esi

    Mov B$edi-1 ':' | NextDisLine
    Mov D$edi 'jmp ' | add edi 4
ret
____________________________________________________________________________________________

FlagNoReturnApi:
    Push esi
        inc esi
;;
      ; MSVBVM50.064 ???
        ...If D$esi = 'MSVB'
            ..If D$esi+4 = 'VM50'
                .If D$esi+8 = '.064'
                    If B$esi+12 = "'"
                        Mov B$DisEndOfChunk &TRUE
                    End_If
                .End_If
            ..End_If
        ...End_If
;;
        While B$esi <> '.' | inc esi | End_While | inc esi

      ; ExitProcess
        ...If D$esi = 'Exit'
            ..If D$esi+4 = 'Proc'
                If D$esi+8 = "ess'"
                    Mov B$DisEndOfChunk &TRUE
                End_If
            ..End_If

      ; __vbaErrorOverflow
        ...Else_If D$esi = '__vb'
            ..If D$esi+4 = 'aErr'
                .If D$esi+8 = 'orOv'
                    If D$esi+12 = 'erfl'
                        On W$esi+16 = 'ow', Mov B$DisEndOfChunk &TRUE
                    End_If
                .End_If
            ..End_If

        ...Else
          ; For freezing the 'IsItNoReturnCall' test:
            Mov B$CallInstruction &FALSE

        ...End_If
    Pop esi
ret
____________________________________________________________________________________________

;;
  The 4 Dialogs Creations Functions:
  
        'USER32.CreateDialogParamA'         >>> second above Push
        'USER32.CreateDialogindirectParamA' >>> second above Push
        
        'USER32.DialogBoxParamA'            >>> second above Push
        'USER32.DialogBoxindirectParamA'    >>> second above Push

  The 'USER32.SetWindowLong' Function, with the "GWL_WNDPROC" Parameter
  GWL_WNDPROC >>> second above Push
  Procedure >>> first above Push

  The 'KERNEL32.CreateThread' Function.     >>> fourth above Push

  The 'USER32.SetWindowsHookExA' Function.          >>> third above Push
  
  List of Api with CallBacks:
  
    USER32.CreateDialogParamA
    USER32.CreateDialogindirectParamA
    USER32.DialogBoxParamA
    USER32.DialogBoxindirectParamA
    USER32.SetWindowLongA
    KERNEL32.CreateThread
    USER32.SetWindowsHookExA
    USER32.EnumChildWindows
;;

FlagApiProcedures:
    Push esi

    While B$esi <> '.' | inc esi | End_While | inc esi

      ; ExitProcess
        ...If D$esi = 'Crea'
            ..If D$esi+4 = 'teDi'
                .If D$esi+8 = "alog"
                    If D$esi+12 = 'Para'
                      ; 'USER32.CreateDialogParamA'
                        Call GetDialogProcedure

                    Else_If D$esi+12 = 'indi'
                      ; 'USER32.CreateDialogindirectParamA'
                        Call GetDialogProcedure

                    End_If
                .End_If
            ..End_If

        ...Else_If D$esi = 'Dial'
            ..If D$esi+4 = 'ogBo'
                If D$esi+8 = 'xPar'
                  ; 'USER32.DialogBoxParamA'
                    Call GetDialogProcedure

                Else_If D$esi+8 = 'indi'
                  ; 'USER32.DialogBoxindirectParamA'
                    Call GetDialogProcedure

                End_If
            ..End_If

        ...Else_If D$esi = 'SetW'
            ..If D$esi+4 =  'indo'
                .If D$esi+8 = 'wLon'
                  ; 'USER32.SetWindowLongA'
                    Call GetApiPush 2
                    If eax = &GWL_WNDPROC
                        Call GetApiPush 3
                        On eax <> 0, Call SetApiProcedure eax, WindowProcName
                    End_If
                .End_If
            ..End_If

        ...Else_If D$esi = 'Crea'
            ..If D$esi+4 = 'teTh'
                If D$esi+8 = 'read'
                  ; 'KERNEL32.CreateThread'
                    Call GetApiPush 3
                    On eax <> 0, Call SetApiProcedure eax, ThreadProcName
                End_If
            ..End_If

        ...Else_If D$esi = 'SetW'
            ..If D$esi+4 = 'indo'
                .If D$esi+8 = 'wsHo'
                    If D$esi+12 = 'okEx'
                      ; 'USER32.SetWindowsHookExA'
                        Call GetApiPush 2
                        On eax <> 0, Call SetApiProcedure eax, HookProcName
                    End_If
                .End_If
            ..End_If

        ...Else_If D$esi = 'Enum'
            ..If D$esi+4 = 'Chil'
                .If D$esi+8 = 'dWin'
                    If D$esi+12 = 'dows'
                      ; 'USER32.EnumChildWindows'
                        Call GetApiPush 2
                        On eax <> 0, Call SetApiProcedure eax, EnumChildWindowsProcName
                    End_If
                .End_If
            ..End_If

        ...Else_If D$esi = 'lstr'
            ..If D$esi+4 = 'cpyA'
              ; 'KERNEL32.lstrcpyA'
                Call GetApiPush 1
                On eax <> 0, Call SetApiData eax
                Call GetApiPush 2
                On eax <> 0, Call SetApiData eax
            ..End_If

        ...Else_If D$esi = 'Regi'
            ..If D$esi+4 = 'ster'
                .If D$esi+8 = 'Clas'
                    If D$esi+12 = 'sExA'
                      ; 'USER32.RegisterClassExA/W'
L1:                     Call GetApiPush 1 | On eax = 0, jmp L9>
                        Push eax
                            Call SelectMainWindowProc eax, 12
                        Pop eax
                        Call SetWINDCLASSEXData eax

                    Else_If D$esi+12 = 'sExW'
                        jmp L1<

                    Else_If B$esi+12 = 's'
                      ; 'USER32.RegisterClassA/W'
                        Call GetApiPush 1 | On eax = 0, jmp L9>
                        Push eax
                            Call SelectMainWindowProc eax, 10
                        Pop eax
                        Call SetWINDCLASSData eax
                    End_If

                .End_If
            ..End_If
        ...End_If
L9: Pop esi
ret


[DisMainWindowProc: D$ ?]

Proc SelectMainWindowProc:
    Argument @WNDCLASS, @N
    Uses edi, ebx, ecx

        Mov eax D@WNDCLASS | sub eax D$DisImageBase | add eax D$UserPeStart

        Mov eax D$eax+8

        ..If eax <> 0
            .If D$DisMainWindowProc = 0
                Mov D$DisMainWindowProc eax
                Call SetApiProcedure eax, MainWindowProcName

            .Else
              ; Several candidates: Take the one closer to 'Main':
                Mov ebx D$DisMainWindowProc | sub ebx D$DisEntryPoint | Absolute ebx
                Mov ecx eax | sub ecx D$DisEntryPoint | Absolute ecx
                If ecx < ebx
                    Call SetApiProcedure D$DisMainWindowProc, WindowProcName
                    Call SetApiProcedure eax, MainWindowProcName
                    Mov D$DisMainWindowProc eax
                Else_If ecx > ebx
                    Call SetApiProcedure eax, WindowProcName
                End_If
            .End_If
        ..End_If

        Mov edi D@WNDCLASS | sub edi D$DisImageBase | add edi D$SectionsMap

        Mov ecx D@N, eax FOURDATAFLAGS | rep stosd
EndP


[WNDCLASS.cbSize.Name: B$'_cbSize' EOS
 WNDCLASS.style.Name: B$ '_style' EOS
 WNDCLASS.lpfnWndProc.Name: B$ '_lpfnWndProc' EOS
 WNDCLASS.cbClsExtra.Name: B$ '_cbClsExtra' EOS
 WNDCLASS.cbWndExtra.Name: B$ '_cbWndExtra' EOS
 WNDCLASS.hInstance.Name: B$ '_hInstance' EOS
 WNDCLASS.hIcon.Name: B$ '_hIcon' EOS
 WNDCLASS.hCursor.Name: B$ '_hCursor' EOS
 WNDCLASS.hbrBackground.Name: B$ '_hbrBackground' EOS
 WNDCLASS.lpszMenuName.Name: B$ '_lpszMenuName' EOS
 WNDCLASS.lpszClassName.Name: B$ '_lpszClassName' EOS
 WNDCLASS.hIconSm.Name: B$'_hIconSm' EOS]

Proc SetWINDCLASSData:
    Argument @Pointer
    Uses edi

        Mov eax D@Pointer

        ToStringsMapFrom DisImageBase eax
        Mov D$eax WNDCLASS.style.Name | add eax 16
        Mov D$eax WNDCLASS.lpfnWndProc.Name | add eax 16
        Mov D$eax WNDCLASS.cbClsExtra.Name | add eax 16
        Mov D$eax WNDCLASS.cbWndExtra.Name | add eax 16
        Mov D$eax WNDCLASS.hInstance.Name | add eax 16
        Mov D$eax WNDCLASS.hIcon.Name | add eax 16
        Mov D$eax WNDCLASS.hCursor.Name | add eax 16
        Mov D$eax WNDCLASS.hbrBackground.Name | add eax 16
        Mov D$eax WNDCLASS.lpszMenuName.Name | add eax 16
        Mov D$eax WNDCLASS.lpszClassName.Name

        Mov edi D@Pointer | sub edi D$DisImageBase | add edi D$RoutingMap
        Mov eax LABEL, ecx 10 | rep stosd

        Mov edi D@Pointer | sub edi D$DisImageBase | add edi D$SizesMap
        Mov eax DWORD, ecx 10 | rep stosd
EndP


Proc SetWINDCLASSEXData:
    Argument @Pointer
    Uses edi

        Mov eax D@Pointer | ToStringsMapFrom DisImageBase, eax

        Mov D$eax WNDCLASS.cbSize.Name | add eax 16
        Mov D$eax WNDCLASS.style.Name | add eax 16
        Mov D$eax WNDCLASS.lpfnWndProc.Name | add eax 16
        Mov D$eax WNDCLASS.cbClsExtra.Name | add eax 16
        Mov D$eax WNDCLASS.cbWndExtra.Name | add eax 16
        Mov D$eax WNDCLASS.hInstance.Name | add eax 16
        Mov D$eax WNDCLASS.hIcon.Name | add eax 16
        Mov D$eax WNDCLASS.hCursor.Name | add eax 16
        Mov D$eax WNDCLASS.hbrBackground.Name | add eax 16
        Mov D$eax WNDCLASS.lpszMenuName.Name | add eax 16
        Mov D$eax WNDCLASS.lpszClassName.Name | add eax 16
        Mov D$eax WNDCLASS.hIconSm.Name

        Mov edi D@Pointer | sub edi D$DisImageBase | add edi D$RoutingMap
        Mov eax LABEL+EVOCATED, ecx 12 | rep stosd

        Mov edi D@Pointer | sub edi D$DisImageBase | add edi D$SizesMap
        Mov eax DWORD, ecx 12 | rep stosd
EndP


Proc GetApiPush:
    Argument @Indice

        Mov D$PossibleApiPointer 0

        Mov esi D$TestLastLineLocation | On esi = 0, jmp L9>>

        dec esi | Mov ebx esi | sub ebx D$UserPeStart | add ebx D$RoutingMap

L0:     movzx eax B$ebx
        test eax INSTRUCTION ZERO L2>>
        test eax NODE+EXPORTNODE+PUSH_EBP NOT_ZERO L9>>

            Mov al B$esi
          ; IsItPush    >>>>>>>>>   Op68 !!!!!!!!!!!
            .If al = 060 ; pushad
                jmp L9>>
            .Else_If al = 0FF ; OpFF // /6   ; 75
                DigitMask B$esi+1 To al
                If al = 6
                    Mov eax &TRUE
                Else
                    Mov eax &FALSE
                End_If

            .Else_If al = 06A
                Mov eax &TRUE
            .Else_If al = 068
                Mov eax &TRUE
            .Else_If al < 050
                Mov eax &FALSE
            .Else_If al < 058
                Mov eax &TRUE       ; Push reg (050 to 057)
            .Else
                Mov eax &FALSE
            .End_If

            ...If eax = &TRUE
                dec D@Indice

                ..If D@Indice = 0
                    If B$esi = 0FF ; OpFF, 035 = 00_110_101
                        add esi 2 | lodsd
                        cmp eax D$UserPeStart | jb L9>>
                        cmp eax D$UserPeEnd | ja L9>>
                            Mov eax D$eax | ExitP

                    Else_If B$esi = 068 ; Op68
                        cmp B$esi-1 0F | je L9>  ; Op0F: Escape Prefix
                        cmp B$esi-1 066 | je L9>;   Op66 >>> Operand Size Override
                            inc esi | lodsd | ExitP
                    End_If

                ..End_If
            ...End_If

L2:     dec ebx | dec esi | jmp L0<<

L9:     Mov eax 0
EndP


[PossibleApiPointer: D$ ?
 PossibleApiPointerIndice: D$ ?]

Proc GetPossibleApiPointer:
    Argument @Pointer, @Indice

        Mov eax D@Pointer

        sub eax D$DisImageBase | add eax D$UserPeStart | Mov eax D$eax
      ; May be a wrong pointing inside the .Import!
      ; Add a Pointer test!
        If eax < D$ApiBuffer
            Mov D$PossibleApiPointer 0
        Else_If eax >= D$EndOfApiBuffer
            Mov D$PossibleApiPointer 0
        Else
            Mov D$PossibleApiPointer eax
            Mov eax D@Indice, D$PossibleApiPointerIndice eax
        End_If
EndP


GetDialogProcedure:
    Call GetApiPush 4

    .If eax <> 0
        Call SetApiProcedure eax, DialogProcName

    .Else_If D$PossibleApiPointer <> 0
;;
  There are case of:
  
        Call 'KERNEL32.GetModuleHandleA'  ; FF 15 80 C1 43 00 
        Push eax
        
        used inside the PUSHes flow, to set-up the 'H.Instance' Member.
;;
        If D$PossibleApiPointerIndice = 3
            Call GetApiPush 5
            On eax <> 0, Call SetApiProcedure eax, DialogProcName
        End_If

    .End_If
ret


[MainWindowProcName: B$ 'MainWindowProc' EOS]
[WindowProcName: B$ '_WindowProc' EOS]
[ThreadProcName: B$ '_ThreadProc' EOS]
[HookProcName: B$ '_HookProc' EOS]
[EnumChildWindowsProcName: B$ '_EnumChildWindowsProc' EOS]
[DialogProcName: B$ '_DialogProc' EOS]

Proc SetApiProcedure:
    Argument @Pointer, @Name
    Uses eax

        Mov eax D@Pointer

        sub eax D$DisImageBase | add eax D$SectionsMap

        .If eax > D$SectionsMap
            If eax < D$EndOfSectionsMap
                Mov B$eax CODEFLAG

                sub eax D$SectionsMap | add eax D$RoutingMap
                Mov B$eax LABEL+NODE+EVOCATED+INSTRUCTION+ACCESSED

                ToStringsMapFrom RoutingMap, eax
                Move D$eax D@Name
            End_If
        .End_If
EndP


Proc SetApiData:
    Argument @Pointer

        Mov eax D@Pointer

        sub eax D$DisImageBase | add eax D$SectionsMap

        .If eax > D$SectionsMap
            If eax < D$EndOfSectionsMap
                Mov B$eax DATAFLAG

                sub eax D$SectionsMap | add eax D$RoutingMap
                or B$eax LABEL+EVOCATED
            End_If
        .End_If

EndP


____________________________________________________________________________________________

; Storing the SizeOf the accessed element into 'SizesMap'. Before eax holds D$LastCodeRef.

StoreDisSize:
    Mov ebx eax | sub ebx D$DisImageBase | add ebx D$SizesMap

    .If B$LeaInstruction = &TRUE
        or B$ebx POINTER | Mov B$LeaInstruction &FALSE
    .Else_If W$edi-2 = 'B$'
        or B$ebx BYTE | Call FlagData 1
    .Else_If W$edi-2 = 'W$'
        or B$ebx WORD | Call FlagData 2
    .Else_If W$edi-2 = 'D$'
        or B$ebx DWORD | Call FlagPointerToPointer| Call FlagData 4
    .Else_If W$edi-2 = 'F$'
        or B$ebx FP4 | Call FlagData 4
    .Else_If W$edi-2 = 'R$'
        or B$ebx FP8 | Call FlagData 8
    .Else_If W$edi-2 = 'T$'
        or B$ebx FP10 | Call FlagData 10
    .Else
        ;or B$ebx POINTER
        Call FlagPointerToPointer
    .End_If
ret


Proc FlagData:
    Argument @N
    Uses edi, eax, ecx

        Mov edi ebx | sub edi D$SizesMap | add edi D$SectionsMap
        On B$edi = VIRTUALFLAG, Exitp
        Mov ecx D@N, al DATAFLAG | rep stosb
EndP


FlagPointerToPointer:
   Push ebx
      ; If it is a Pointer to Pointer, LABEL the final Location:
        Mov ebx eax | sub ebx D$DisImageBase | add ebx D$UserPeStart
      ; Here, eax still is 'D$LastCodeRef'
        ..If ebx < D$UserPeStart
            ;
        ..Else_If ebx < D$UserPeEnd
            sub ebx D$UserPeStart | add ebx D$RoutingMap | or B$ebx EVOCATED+LABEL

            sub ebx D$RoutingMap | add ebx D$UserPeStart
            Mov ebx D$ebx | sub ebx D$DisImageBase | add ebx D$UserPeStart
            .If ebx < D$UserPeStart
                ;
            .Else_If ebx < D$UserPeEnd
                Push eax
                    sub ebx D$UserPeStart | add ebx D$RoutingMap
                    Mov eax ebx | sub eax D$RoutingMap | add eax D$SectionsMap
                    If B$eax = CODEFLAG
                        or B$ebx EVOCATED;+LABEL
                    Else
                        or B$ebx EVOCATED+LABEL
                    End_If
                Pop eax
            .End_If

        ..End_If
    Pop ebx
ret

____________________________________________________________________________________________

[DisShortRef: B$ ?]

[EncreasedLocalJmp: D$ ?]

EndWithDisByteRelative:
    ;On B$esi = 0, add B$UnlikelyCode 5 ; Absurd, but really found in some Code.

    If B$SimpleScan = &TRUE
        inc esi | Mov B$DisFlag DISDONE+DISLINEOVER | ret
    End_If

    Mov B$EncreasedLocalJmp &FALSE
    movsx eax B$esi | inc esi | Mov D$LastCodeRef eax
    Call RelativeToAbsolute
    Push eax
        Call WriteLocalLabelFromEax
    Pop eax
    If eax > D$LastCodeRef
        Mov B$edi-1 '>'
    Else
        Mov B$edi-1 '<'
    End_If

    On B$WeAreInTheCodeBox = &TRUE, ret
;jmp L7>>
  ; Is the short jump out of range because of the replacement of a DLL Call Direct
  ; instead of Indirect?
    Push esi, eax
        Mov ecx 0
        Mov al B$esi-1, bl al, B$DisShortRef al | and bl 00_1000_0000

      ; Negatif short?
        ..If bl = 00_1000_0000
            sub esi 3 | neg al
            .While al <> 0
                dec esi
              ; Code of Call Indirect (To Jumps Table?).
                .If B$esi = 0E8
                  ; Yes, but is it an Instruction ?
                    Mov ebx esi | sub ebx D$UserPeStart | add ebx D$RoutingMap
                    test B$ebx INSTRUCTION ZERO L1>
                  ; Relative to absolute:
                    Mov ebx esi | add ebx D$esi+1 | add ebx 5
                    cmp ebx D$UserPeStart | jb L1>
                    cmp ebx D$UserPeEnd | ja L1>
                        If W$ebx = 025FF
                            Mov ebx D$ebx+2
                            sub ebx D$DisImageBase | add ebx D$SectionsMap

                            On ebx > D$EndOfSectionsMap, jmp L1>
                            On ebx < D$SectionsMap, jmp L1>
                                On B$ebx = IMPORTFLAG, inc ecx
L1:                     End_If
                .End_If
                dec al
            .End_While

        ..Else
            .While al <> 0
              ; Code of Call Direct (To Jumps Table?).
                .If B$esi = 0E8
                  ; Yes, but is it an Instruction ?
                    Mov ebx esi | sub ebx D$UserPeStart | add ebx D$RoutingMap
                    test B$ebx INSTRUCTION ZERO L1>
                  ; Relative to absolute:
                    Mov ebx esi | add ebx D$esi+1 | add ebx 5
                    cmp ebx D$UserPeStart | jb L1>
                    cmp ebx D$UserPeEnd | ja L1>
                        If W$ebx = 025FF  ; FF 25 XX XX XX XX > jmp D$xxxx
                            Mov ebx D$ebx+2
                            sub ebx D$DisImageBase | add ebx D$SectionsMap
                            cmp ebx D$SectionsMap | jb L1>
                            cmp ebx D$EndOfSectionsMap | ja L1>
                                On B$ebx = IMPORTFLAG, inc ecx
L1:                     End_If
                .End_If
                dec al | inc esi
            .End_While

        ..End_If

L2:     .If ecx <> 0
            Mov al B$DisShortRef
            If B$edi-1 = '>'
                add al cl | test al 00_1000_0000 ZERO L5>
                Mov B$edi '>' | inc edi
            Else
                sub al cl | test al 00_1000_0000 NOT_ZERO L5>
                Mov B$edi '<' | inc edi
            End_If
            Mov B$EncreasedLocalJmp &TRUE
        .End_If

L5: Pop eax, esi
L7:
    Mov D$edi '  ; ' | add edi 4

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;Mov D$edi 'Code' | add edi 4
    Call RelativeToAbsolute | Mov D$LastCodeRef eax
    ;Push eax
    ;    Call WriteEax
    ;Pop eax
    Mov ebx D$LastCodeRef
    sub ebx D$DisImageBase | add ebx D$SectionsMap | Mov B$ebx CODEFLAG
    Call WriteDisRelative
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    If B$EncreasedLocalJmp = &TRUE
        NextDisLine
        Mov D$edi "; >>", D$edi+4 "> 'C", D$edi+8 "omme", D$edi+12 "nt1'"
        add edi 16
    End_If

    Mov B$DisFlag DISDONE+DISLINEOVER
ret

; Same as upper, but, in cases of Data in code, a LOOP L2>, might be generated. Not
; that important, but what is important is that this could mess up the targeted Code
; when re-aligning because of Code reference.

EndWithDisByteRelativeBack:
    movsx eax B$esi | On eax < 3, add B$UnlikelyCode 5

    inc esi | Mov D$LastCodeRef eax
    Call RelativeToAbsolute
    Push eax
        Call WriteLocalLabelFromEax
    Pop eax
    If eax > D$LastCodeRef
        Mov B$edi-1 '>'; ; ', D$edi+3 '!!!!' | add edi 7 ; jE! >>Agree DOWN_Loop
        ;Mov D$LastCodeRef 0, B$DisFlag DISDONE+DISLINEOVER | ret
    Else
        Mov B$edi-1 '<'
    End_If
    Mov D$edi '  ; ' | add edi 4
    Call RelativeToAbsolute | Mov D$LastCodeRef eax
    Call WriteDisRelative
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

EndWithDisWordDwordRelative:
    If B$OperandSizeOverride = &TRUE
        movsx eax W$esi | add esi 2
    Else
        lodsd
    End_If
    Mov D$LastCodeRef eax
    Call RelativeToAbsolute | Call WriteDisRelative
    Mov B$DisFlag DISDONE+DISLINEOVER
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; This is the Chunks of Macros to be saved at Top of the Disassembly Source:

[DisMacros2: B$ "____________________________________________________________________________________________

;;
  Used Macros Strings Variables:
  
    &1, &2, &3              Proc
    &9, &10 to &19          For
 
  Used Macros Counters Variables:
 
    &&0, &&1                If
    &&2, &&3                While
    &&4, &&5                Do
    &&6, &&7                For
    
  Local Labels Attributions:
  
    O1                      On
    P9                      Proc
    I0,... I9, J0,... J9    If
    W0,... W9               While
    D0,... D9               Do
    F0,... F9               For
;;
____________________________________________________________________________________________

[= e   < b    > a    <s l    >s g    =< be    <= be    => ae    >= ae    <> ne]
____________________________________________________________________________________________

; Multi Push, Pop, Mov, Move, inc, and dec  Macros:

[Push | Push #1 | #+1]
[Pop | Pop #1 | #+1]
[Mov | Mov #1 #2 | #+2]
[Move | Push #2 | Pop #1 | #+2]
[inc | inc #1 | #+1]
[dec | dec #1 | #+1]
____________________________________________________________________________________________

[Exchange | Push #1 | Push #2 | Pop #1 | Pop #2 | #+2]
____________________________________________________________________________________________

[On | cmp #1 #3 | jn#2 O1> | #4>L | O1:]
____________________________________________________________________________________________

[Call | #If #1=str
            ApiPush #L>2
        #Else
            Push #L>2
        #End_If
        Call #1]

[ApiPush | #If #1=str
                Push {#1, 0}
           #Else
                Push #1
           #End_If
           #+1]
____________________________________________________________________________________________

; C calling convention:

[ccall
    Push #L>2 | Call #1
    #If #N>1
        add esp ((#N-1)*4)
    #EndIf]
____________________________________________________________________________________________

[.If
    #If &&0<>0
        #If &&0<> '0'
            #Error 'Unpaired If'
        #End_If
    #End_If
    &&0= '0' | &&1=Pos
    AndCmp I&&0>>, #1>L]

[.End_If | I&&0: | J&&0:
    #If &&0<> '0'
        #ErrorPos &&1 'Unpaired If'
    #End_If
    &&0=0]

[If
    #If &&0=0
        &&0= '0'
    #Else
        &&0=&&0+1
    #End_If

    AndCmp I&&0>>, #1>L]

[Else_If | jmp J&&0>> | I&&0:
    AndCmp I&&0>>, #1>L]

[Else | jmp J&&0>> | I&&0: ]

[End_If | I&&0: | J&&0:
    #If &&0= '0'
        &&0=0
    #Else
        &&0=&&0-1
    #End_If]

[AndCmp | cmp #2 #4 | jn#3 #F | #+3]
____________________________________________________________________________________________

[.While
    #If &&2<>0
        #If &&2<> '0'
                #Error 'Unpaired While'
        #End_If
    #End_If

    &&2= '0' | &&3=Pos

    W&&2: cmp #1 #3 | jn#2 W&&2>>]

[.End_While
    #If &&2<> '0'
        #ErrorPos  &&3 'Unpaired While'
    #End_If
    jmp W&&2<< | w&&2:
    &&2=0]

[While
    #If &&2=0
        &&2= '0'
    #Else
        &&2=&&2+1
    #End_If
    W&&2: cmp #1 #3 | jn#2 W&&2>>]

[End_While | jmp W&&2<< | w&&2:
             #If &&2= '0'
                &&2=0
             #Else
                &&2=&&2-1
             #End_If]
____________________________________________________________________________________________

[.Do
    #If &&4<>0
        #If &&4<> '0'
                #Error 'Unpaired Do'
        #End_If
    #End_If

    &&4= '0' | &&5=Pos

    D&&4:]

[.Loop_Until
    #If &&4<> '0'
        #ErrorPos  &&5 'Unpaired Do Until'
    #End_If

    cmp #1 #3 | jn#2 D&&4<<
    &&4=0]

[.Loop_While
    #If &&4<> '0'
        #ErrorPos  &&5 'Unpaired Do While'
    #End_If

    cmp #1 #3 | j#2 D&&4<<
    &&4=0]

[Do
    #If &&4= 0
        &&4= '0'
    #Else
        &&4=&&4+1
    #End_If

    D&&4: ]

[Loop_Until | cmp #1 #3 | jn#2 D&&4<<  | D&&4:
 #If &&4= '0'
    &&4=0
 #Else
    &&4=&&4-1
 #End_If]

[Loop_While | cmp #1 #3 | j#2 D&&4<<  | D&&4:
 #If &&4= '0'
    &&4=0
 #Else
    &&4=&&4-1
 #End_If]
 
[Do_Loop | jmp D&&4<<]
____________________________________________________________________________________________

[.For
    #If &&6<>0
        #If &&6<> '0'
            #Error 'Unpaired For'
        #End_If
    #End_If
    &&6= '0' | &&7=Pos

    #If #3=imm
        Mov #1 (#3-1)
    #Else
        Mov #1 #3
        dec #1
    #EndIf
 F&&6:
    inc #1 | cmp #1 #5 | ja F&&6>> ]

[.Next | jmp F&&6<<
 F&&6:
    #If &&6<> '0'
        #ErrorPos &&7 'Unpaired For'
    #End_If
    &&6=0]

[For
    #If &&6=0
        &&6= '0'
    #Else
        &&6=&&6+1
    #EndIf

    #If #3=imm
        Mov #1 (#3-1)
    #Else
        Mov #1 #3
        dec #1
    #EndIf
 F&&6:
    inc #1 | cmp #1 #5 | ja F&&6>> ]


[Next | jmp F&&6<< | F&&6: &&6=&&6-1]

[Break | jmp F&&6>>]
[Continue | jmp F&&6<<]
[ExitF | jmp F0>>]
____________________________________________________________________________________________

;;
  &1 = Size of arguments 
  &2 = Size of local data (local+structures) 
  &3 = preserved regs 
;;

[Proc | &1=0 | &2=0 | &3= | #1 | Push ebp | Mov ebp esp]

[Arguments | {#1 ebp+((#x*4)+4)} | #+1 | &1=(#N*4)]

[Local | {#1 ebp-(#x*4)} | #+1 | &2=(#N*4) | sub esp &2]

[GetMember | {#3 ebp-(#F-#2)} | #+2]

[Structure | {#1 ebp-(&2+#2+4)} | sub esp #2 | Push esp | GetMember &2+#2 #L>3 | &2=&2+#2+4]

[Uses | Push #1>L | &3=Pop #L>1]

[Return | #If #N=1 | Mov eax #1 | #EndIf | jmp P9>>]

[ExitP | jmp P9>>]

[EndP | P9: | &3 | Mov esp ebp | Pop ebp | ret &1]

[.EndP | P9: | &3 | Mov esp ebp | Pop ebp | ret &1]
_____________________________________________________________________________________________

; little message routines for values tests (dWords only / text pointer) to be called with:
; > Hexprint D$esi / showme esi+5, for exemple:

[InfoTitle: 'Application Base', 0]

[HexprintString: B$ '        h' 0
 MessageTitle:      'HihoHiho' 0]


Proc HexPrnt:
    Arguments @N

    pushad
        Mov ebx D@N, ecx 8, edi HexPrintString | add edi 7
        std
            Do
                Mov al bl | and al 0F | add al '0'
                On al > '9', add al 7
                stosb | shr ebx 4
            Do_Loop
        cld
        Call 'USER32.MessageBoxA'  0  HexPrintString  MessageTitle  &MB_OK__&MB_SYSTEMMODAL
    popad
EndP

[Hexprint | Call Hexprnt #1 | #+1]


Proc ShowYou:
    Arguments @Pointer

    pushad
        Call 'MessageBoxA'  &NULL  D@Pointer  MessageTitle  &MB_SYSTEMMODAL__&MB_OK
    popad
EndP

[Showme | Push eax | lea eax D$#1 | Call ShowYou eax | Pop eax]

_______________________________________________________________________________________

____________________________________________________________________________________________

Comment1:
;;
  Many Compilers encode the DLLs calls under the form of a Call to JUMPs Table. This is
  to say that each Call to a DLL Function is encoded with two instructions: a Call plus
  a JMP.
  
  RosAsm performs these calls in one single Instruction (a direct Call to the .Import
  Section record).
  
  Unfortunately, the direct RosAsm form is one Byte longer than the indirect Call to a
  Jumps Table (the Instruction Opcode for 'Call D$Location' is two Bytes, whereas the
  one for 'Call Label' is one Byte).
  
  It may happend that, when a short JMP jumps over one or more DLLs calls, the shorter
  form found in the Disassembly becomes out of range because of these added Bytes.
  
  In such cases, the Disassembler replaces, for example, 'je K5>', by 'je K5>>'.
  
  All occurences of '>>' and '<<' found in a disassembly Source fall under that case.
;;
",

DisTitle: B$ CR LF ; ('1' is at DisTitle+12).
"TITLE Part01 
_____________________________________________________________________________________________

" EOS]


WriteMacros:
    Mov esi DisMacros2 | While B$esi <> 0 | movsb | End_While
ret
____________________________________________________________________________________________

[EvocatedOnly: D$ ?]

RemoveNonAccessedEvocatedData:
    Mov esi D$RoutingMap, edx D$EndOfRoutingMap | add esi D$FirstSection
    Mov edi D$SectionsMap | add edi D$FirstSection
Mov D$EvocatedOnly 0
    While esi < edx
        If B$esi = LABEL
          ; When here, this '0' means that it is going to be considered Data:
          ;  On B$edi = 0, Mov B$esi 0
        Else_If B$esi = EVOCATED
          ;  On B$edi = 0, Mov B$esi 0
          inc D$EvocatedOnly
        End_If

        inc esi | inc edi
    End_While

    Call HexPrint D$EvocatedOnly

ret
____________________________________________________________________________________________

[DisReadyForTable: D$ ?
 DisCloseBracket: D$ ?
 DisVirtual: D$ ?]

;[PeWithoutData: B$ "
; Weird PE without .Data Section inside !!!" PeWithoutDataLen: Len
; AndVirtualIsRunable: B$ "
; And a Virtual Section is runable!!!
;
;" AndVirtualIsRunableLen: Len]

[TargetSize: D$ ?]     ; May be BYTE, WORD, DWORD, FP4, FP8, FP10, POINTER.

; check if a Data set may be represented by [Data: 0 #XXX]
; (B$ / W$ / D$ // 0 or other):

[FirstDisDataByte: D$ ?]

WriteBytesData:
    Push ebx, ecx
        Mov edx 0, D$edi 'B$  ' | add edi 3

L5:     movzx eax B$ebx | Push ebx | Call WriteEax | Pop ebx
        Mov B$edi SPC | inc edi | inc edx
        .If edx = 16
            If ecx > 1
                NextDisLine
                Mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    ', B$edi+12 SPC
                add edi 13 | Mov edx 0
            End_If
        .End_If
        inc ebx | loop L5<
        Mov B$edi-1 ']'
    Pop ecx, ebx
ret


WriteWordsData:
    Push ebx, ecx
        shr ecx 1

        NextDisLine
        Mov D$edi '    ', D$edi+4 '    '
        add edi 7 | Mov D$edi '    ' | add edi 3
        Mov edx 0, D$edi 'W$  ' | add edi 3

L5:     movzx eax W$ebx | Push ebx | Call WriteEax | Pop ebx
        Mov B$edi SPC | inc edi | inc edx
        .If edx = 11
            If ecx > 1
                NextDisLine
                Mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    '
                add edi 10 | Mov D$edi '    ' | add edi 3 | Mov edx 0
            End_If
        .End_If
        add ebx 2 | On ebx < D$UserPeEnd, loop L5<
        Mov B$edi-1 ']'
    Pop ecx, ebx
ret


WriteUnicodeData:
    Push ebx, ecx
        shr ecx 1

        NextDisLine
        Mov D$edi '    ', D$edi+4 '    '
        add edi 7 | Mov D$edi '    ' | add edi 3
        Mov edx 0, D$edi 'U$  ' | add edi 3

L5:     movzx eax W$ebx | Push ebx | Call WriteEax | Pop ebx
        Mov B$edi SPC | inc edi | inc edx
        .If edx = 11
            If ecx > 1
                NextDisLine
                Mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    '
                add edi 10 | Mov D$edi '    ' | add edi 3 | Mov edx 0
            End_If
        .End_If
        add ebx 2 | loop L5<
        Mov B$edi-1 ']'
    Pop ecx, ebx
ret


L9: Pop ecx, ebx
ret

WriteCommentedWordsData:
    Push ebx, ecx
        shr ecx 1 | jecxz L9<

        NextDisLine
        Mov D$edi '    ', D$edi+4 '    '
        add edi 7 | Mov D$edi ';   ' | add edi 3
        Mov edx 0, D$edi 'W$  ' | add edi 3

L5:     movzx eax W$ebx | Push ebx | Call WriteEax | Pop ebx
        Mov B$edi SPC | inc edi | inc edx
        .If edx = 11
            If ecx > 1
                NextDisLine
                Mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    '
                add edi 10 | Mov D$edi ';   ' | add edi 3 | Mov edx 0
            End_If
        .End_If
        add ebx 2 | loop L5<
        Mov B$edi-1 ']'
L9: Pop ecx, ebx
ret


WritedWordPointers:
    Push ebx, ecx
        shr ecx 2 | Mov edx 0

L0:     Mov eax D$ebx

        Push ebx
            Mov ebx eax | sub ebx D$DisImageBase | add ebx D$RoutingMap
            cmp ebx D$RoutingMap | jb L2>>
                cmp ebx D$EndOfRoutingMap | jae L2>>

                    Push ebx
                        sub ebx D$RoutingMap | add ebx D$SectionsMap
                        Mov bl B$ebx | and bl CODEFLAG+DATAFLAG+VIRTUALFLAG
                        If bl = CODEFLAG
                            Mov D$edi 'Code' | add edi 4
                        Else_If bl = DATAFLAG
                            Mov D$edi 'Data' | add edi 4
                        Else_If bl = VIRTUALFLAG
                            Mov D$edi 'Virt', D$edi+4 'ual' | add edi 7
                        End_If
                    Pop ebx

                    test B$ebx LABEL NOT_ZERO L2>>
                    Mov cl B$ebx | and cl EVOCATED+INSTRUCTION
                    cmp cl EVOCATED+INSTRUCTION | je L2>>
                    If cl = INSTRUCTION
                        or B$ebx EVOCATED | jmp L2>>
                    End_If
                        Mov ecx 0
L1:                     dec ebx
                        .If ebx <= D$RoutingMap
                            Call EraseSectionName
                            add eax ecx | jmp L2>>
                        .Else_If ecx > 8
                            Call EraseSectionName
                            add eax ecx | jmp L2>>
                        .End_If
                        dec eax | inc ecx
                        test B$ebx LABEL ZERO L1<
                            sub ebx D$RoutingMap | add ebx D$SectionsMap
                            Push edx
                                Mov dl B$ebx, dh B$ebx+ecx
                                .If dl <> dh
                                    Call EraseSectionName
                                    If dl = CODEFLAG
                                        sub ebx D$SectionsMap | add ebx D$RoutingMap
                                        test B$ebx INSTRUCTION ZERO L1<<
                                        Mov D$edi 'Code' | add edi 4
                                    Else_If dl = DATAFLAG
                                        Mov D$edi 'Data' | add edi 4
                                    Else_If dl = VIRTUALFLAG
                                        Mov D$edi 'Virt', D$edi+4 'ual' | add edi 7
                                    End_If
                                .End_If
                            Pop edx
                            Call WriteEax | Mov B$edi '+' | inc edi
                            Mov eax ecx

L2:                 Call WriteEax
L3:     Pop ebx
        Mov B$edi SPC | inc edi | inc edx
        .If edx = 6
            If ecx > 1
                NextDisLine
                Mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    '
                add edi 10 ;| Mov D$edi ';   ' | add edi 3 |
                Mov edx 0
            End_If
        .End_If
        add ebx 4 | dec ecx | cmp ecx 0 | ja L0<<
        Mov B$edi-1 ']'
    Pop ecx, ebx
ret


EraseSectionName:
    If D$edi-4 = 'tual'
        sub edi 7
    Else_If D$edi-4 = 'Code'
        sub edi 4
    Else_If D$edi-4 = 'Data'
        sub edi 4
    End_If
ret


WritedWordsData:
    Push ebx, ecx
        shr ecx 2

L0:     Mov edx 0, D$edi 'D$  ' | add edi 3

L0:     Mov eax D$ebx
        Push ebx
            Mov ebx eax | sub ebx D$DisImageBase | add ebx D$RoutingMap
            cmp ebx D$RoutingMap | jb L2>>
                cmp ebx D$EndOfRoutingMap | jae L2>>
                    test B$ebx NODE ZERO L1>
                        sub ebx D$RoutingMap | add ebx D$SectionsMap

                        Mov bl B$ebx | and bl CODEFLAG+DATAFLAG+VIRTUALFLAG
                        If bl = CODEFLAG
                            Mov D$edi 'Code' | add edi 4
                        Else_If bl = DATAFLAG
                            Mov D$edi 'Data' | add edi 4
                        Else_If bl = VIRTUALFLAG
                            Mov D$edi 'Virt', D$edi+4 'ual' | add edi 7
                        End_If
                        jmp L2>

L1:                 test B$ebx Evocated ZERO L2>
                        Push ebx, eax
                            Call WriteEax | Mov D$edi '  ; ' | add edi 4
                        Pop eax, ebx
                        sub ebx D$RoutingMap | add ebx D$SectionsMap
                        Mov bl B$ebx | and bl CODEFLAG+DATAFLAG+VIRTUALFLAG
                        If bl = CODEFLAG
                            Mov D$edi 'Code' | add edi 4
                        Else_If bl = DATAFLAG
                            Mov D$edi 'Data' | add edi 4
                        Else_If bl = VIRTUALFLAG
                            Mov D$edi 'Virt', D$edi+4 'ual' | add edi 7
                        End_If
                        Call WriteEax
                        Mov D$edi '??? ' | add edi 3 | Mov W$edi CRLF | add edi 2
                        Mov edx 6 | jmp L3>


L2:                 Call WriteEax
L3:     Pop ebx
        Mov B$edi SPC | inc edi | inc edx
        .If edx = 7
            If ecx > 1
                NextDisLine
                Mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    '
                add edi 10 | Mov D$edi '    ' | add edi 3 | Mov edx 0
            End_If
        .End_If
        add ebx 4 | dec ecx | cmp ecx 0 | ja L0<<
        Mov B$edi-1 ']'
    Pop ecx, ebx
ret


L9: Pop ecx, ebx
ret

WritedCommentedWordsData:
    Push ebx, ecx
        shr ecx 2 | jecxz L9<

        NextDisLine
        Mov D$edi '    ', D$edi+4 '    '
        add edi 7 | Mov D$edi ';   ' | add edi 3
L0:     Mov edx 0, D$edi 'D$  ' | add edi 3

L0:     Mov eax D$ebx
        Push ebx
            Mov ebx eax | sub ebx D$DisImageBase | add ebx D$RoutingMap
            cmp ebx D$RoutingMap | jb L2>>
                cmp ebx D$EndOfRoutingMap | jae L2>>
                    test B$ebx NODE ZERO L1>
                        sub ebx D$RoutingMap | add ebx D$SectionsMap

                        Mov bl B$ebx | and bl CODEFLAG+DATAFLAG+VIRTUALFLAG
                        If bl = CODEFLAG
                            Mov D$edi 'Code' | add edi 4
                        Else_If bl = DATAFLAG
                            Mov D$edi 'Data' | add edi 4
                        Else_If bl = VIRTUALFLAG
                            Mov D$edi 'Virt', D$edi+4 'ual' | add edi 7
                        End_If
                        jmp L2>

L1:                 test B$ebx Evocated ZERO L2>
                        Push ebx, eax
                            Call WriteEax | Mov D$edi '  ; ' | add edi 4
                        Pop eax, ebx
                        sub ebx D$RoutingMap | add ebx D$SectionsMap
                        Mov bl B$ebx | and bl CODEFLAG+DATAFLAG+VIRTUALFLAG
                        If bl = CODEFLAG
                            Mov D$edi 'Code' | add edi 4
                        Else_If bl = DATAFLAG
                            Mov D$edi 'Data' | add edi 4
                        Else_If bl = VIRTUALFLAG
                            Mov D$edi 'Virt', D$edi+4 'ual' | add edi 7
                        End_If
                        Call WriteEax
                        Mov D$edi '??? ' | add edi 3 | Mov edx 6 | jmp L3>


L2:                 Call WriteEax
L3:     Pop ebx
        Mov B$edi SPC | inc edi | inc edx
        .If edx = 7
            If ecx > 1
                NextDisLine
                Mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    '
                add edi 10 | Mov D$edi ';   ' | add edi 3 | Mov edx 0
            End_If
        .End_If
        add ebx 4 | dec ecx | cmp ecx 0 | ja L0<<
        Mov B$edi-1 ']'
L9: Pop ecx, ebx
ret


[ControlFloat: D$ ? ? ? ?]



; ecx = how many Bytes
[ST0trash: T$ ?]
[SourceOfSpecialFPU: D$ ?
 SourceOfSpecialFPUSize: D$ ?]

WriteFP4:
    Mov D$SourceOfSpecialFPU ebx, D$SourceOfSpecialFPUSize 4
    finit
    fld F$ebx | add ebx 4 | Mov D$edi 'F$  ' | jmp L0>
WriteFP8:
    Mov D$SourceOfSpecialFPU ebx, D$SourceOfSpecialFPUSize 8
    finit
    fld R$ebx | add ebx 8 | Mov D$edi 'R$  ' | jmp L0>
WriteFP10:
    Mov D$SourceOfSpecialFPU ebx, D$SourceOfSpecialFPUSize 10
    finit
    fld T$ebx | add ebx 10 | Mov D$edi 'T$  '

L0: add edi 3
    pushad
        Push edi
            Mov eax 0, ecx 10 | rep stosd
        Pop edi

        fstp T$ST0trash | Call DisassemblerFloatToUString ST0trash edi
    popad

    While B$edi > 0 | inc edi | End_While

    .If B$SpecialFPU = &TRUE
        Push ecx
            While B$edi <> '$' | dec edi | End_While | dec edi
            Mov D$edi 'B$  ' | add edi 3
            Mov ecx D$SourceOfSpecialFPUSize, ebx D$SourceOfSpecialFPU

L0:         Mov al B$ebx
            Mov B$edi '0' | inc edi
            If al = 0
              ; Done above
            Else_If al > 0F
                shr al 4 | and eax 0F | Mov al B$STR.A.Hexa+eax | stosb
                Mov al B$ebx
                and eax 0F | Mov al B$STR.A.Hexa+eax | stosb
            Else
                and eax 0F | Mov al B$STR.A.Hexa+eax | stosb
            End_If

            Mov W$edi ', ' | add edi 2

            inc ebx | loop L0<

            sub edi 2
        Pop ecx

    .End_If
ret


[FpModeTroncated 00_00001100_00000000
 FpModeNearest   00_00000000_00000000
 FpModeDown      00_00000100_00000000
 FpModeUp        00_00001000_00000000]

Proc FpuRounding:
    Argument @Mode
    local @ControlWord
        fstcw W@ControlWord
        and W@ControlWord 00_11110011_11111111
        Mov eax D@Mode | or W@ControlWord ax
        fldcw W@ControlWord
EndP


Proc qWordToAscii:
    Arguments @qWordPointer, @StringPointer
    local @Divisor, @Remainder

        Call FpuRounding FpModeTroncated

        Push 0-1 ; End mark on the Stack.

        Mov D@Divisor 10 | fild D@Divisor
        Mov ebx D@qWordPointer
        fild Q$ebx
L0:     fprem | fistp D@Remainder | Push D@Remainder
        fild Q$ebx | fdiv
        fld ST0 | fistp Q$ebx
        Mov eax D$ebx | or eax D$ebx+4 | cmp eax 0 | jne L0<

        Mov edi D@StringPointer
L0:     Pop eax | cmp eax 0-1 | je L7>
            add al '0' | stosb | jmp L0<
L7:     Mov al 0 | stosb
EndP
____________________________________________________________________________________________

NextDataDisLine:
    NextDisLine
    Mov D$edi '    ', D$edi+4 '    ', D$edi+8 '  ; ' | add edi 12
ret
____________________________________________________________________________________________

[TruthAsciiTable: D$ ?]

[BADASCII 2
 GOODASCII 1]

BuildTruthAsciiTable:

    Call VirtualAlloc TruthAsciiTable,
                      256

    Mov edi D$TruthAsciiTable, al BADASCII, ecx 256 | rep stosb

    Mov edi D$TruthAsciiTable,
        B$edi 0

    Mov B$edi+LF GOODASCII,
        B$edi+CR GOODASCII,
        B$edi+0A7 GOODASCII,    ; $
        B$edi+025 GOODASCII,    ; %
        B$edi+0A9 GOODASCII,    ; ®
        B$edi+02F GOODASCII     ; /

    Mov ebx 32,
        ecx 127

    While ebx < ecx

        Mov B$edi+ebx GOODASCII | inc ebx

    End_While

ret


[GoodAsciiSum: D$ ?
 BadAsciiSum: D$ ?
 ZeroAsciiSum: D$ ?]

;;
 Ascii recognition: The 3 upper Variables are used to store the number of 'good', 'bad'
 and 'zeroed' Bytes. When several zero-Bytes are consecutive, they count for 1 zero
 Bytes (possible cases of Strings Alignements). The truth Table is build by
 'BuildTruthAsciiTable' (called from 'DisInitialise'). As the intermediate Zeros are reduced
 to 1 when consecutive, the final number of zeros in 'D$ZeroAsciiSum' tells the possible
 Number of Strings in the parsed Chunk. Dividing the overall Number of 'D$GoodAsciiSum'
 by the number gives the average length of string(s). If too small, interpretation is
 statistically impossible. I set the limit, here to '< 4', what seems to me very risky.
 5 good Ascii Chars on 5 Bytes is only around 1 chance on 2*2*2*2*2 (32). Much too low
 to consider it 100% ensured. I must add a MAYBE Flag, for cases between 32 and ... ???
 The MAYBE case, would allow arasing the simple Bytes interpretation (same > no hurt),
 but should not forbid Words and dWords interpretations.
;;

CheckAsciiData:
    Push ebx, ecx, edx, edi

        Mov edx D$TruthAsciiTable, eax 0
        Mov D$GoodAsciiSum 0, D$BadAsciiSum 0, D$ZeroAsciiSum 0
        Mov B$ItWasReallyAscii &FALSE

L0:     Mov al B$ebx | inc ebx
        If al = 0
            While B$ebx = 0
                inc ebx | cmp ebx D$UserPeEnd | jae L1>
                dec ecx | jz L1>
            End_While
            inc D$ZeroAsciiSum      ; Counts how many separated Strings, in fact;
        End_If

      ; 'GOODASCII' is 1, 'BADASCII' is 2. So:
        Mov al B$edx+eax | shr al 1 | adc D$GoodAsciiSum 0 | add D$BadAsciiSum eax | loop L0<

      ; Extend the Ascii Recognition if Bytes coming after the parsed ones are also Ascii:
        Mov al B$ebx, al B$edx+eax
        ..If al = GOODASCII
            inc D$GoodAsciiSum | inc ebx | Mov al B$ebx, al B$edx+eax
            .If al = GOODASCII
                inc D$GoodAsciiSum | inc ebx | Mov al B$ebx, al B$edx+eax
                If al = GOODASCII
                    inc D$GoodAsciiSum
                End_If
            .End_If
        ..End_If

L1:     .If D$ZeroAsciiSum > 1
            If D$GoodAsciiSum < 10
                Mov edx 0, eax D$GoodAsciiSum, ecx D$ZeroAsciiSum | div ecx
                Mov D$GoodAsciiSum eax
            End_If
        .End_If

        If D$BadAsciiSum = 0
            On D$GoodAsciiSum > 3, Mov B$ItWasReallyAscii &TRUE
        End_If

    Pop edi, edx, ecx, ebx
ret


[ItWasReallyUnicode: D$ ?
 NumberOfStringChars: D$ ?]

CheckUnicodeData:
    Push ebx, ecx, edx, edi

        Mov edx D$TruthAsciiTable, eax 0
        Mov D$GoodAsciiSum 0, D$BadAsciiSum 0, D$ZeroAsciiSum 0
        Mov B$ItWasReallyAscii &FALSE

L0:     Mov al B$ebx, al B$edx+eax | cmp al BADASCII | je L2>
        If B$ebx+1 = 0
            add ebx 2 | dec ecx | jz L2>
                        dec ecx | jz L2>
                            jmp L0<
        End_If

L2:     jecxz L3>

        While W$ebx = 0
            add ebx 2
            dec ecx | jz L3>
            dec ecx | jz L3>
        End_While

L3:     Mov D$NumberOfStringChars ecx

        If ecx = 0
            Mov B$ItWasReallyUnicode &TRUE
        Else
            Mov B$ItWasReallyUnicode &FALSE
        End_If

    Pop edi, edx, ecx, ebx
ret
____________________________________________________________________________________________

[SectionsMapForStrings: D$ ?
 EndOfSectionsMapForStrings: D$ ?]

GetSectionsMapForStrings:

    Mov eax D$EndOfSectionsMap | sub eax D$SectionsMap

    Call VirtualAlloc SectionsMapForStrings,
                      eax

    add eax D$SectionsMapForStrings | Mov D$EndOfSectionsMapForStrings eax

ret

ReleaseSectionsMapForStrings:

    Call VirtualFree SectionsMapForStrings

ret
____________________________________________________________________________________________
;;
  Winner in matter of bad Strings Recognition:
  
  [Data0453031: B$ "=.midtd=.aift]=.au" EOS,
                   "tV=.sndtO=.wmatH=.jpgtA=.bmpt:=.gift3=.pict,=.imgt%=.tift"]
  [<Data045307D: B$ 01E]
  [<Data045307E: B$ "=.tgat"]
  [Data0453084: B$ 017]
  [<Data0453085: B$ "=.pcxt"]
  [<Data045308B: B$ 010]
  [Data045308C: B$ "=.jpgt"]
  [<Data0453092: B$ 09]
  [<Data0453093: B$ "=.jpet"]
  
  ... which is real Code... :)))))))
;;

[Forced: D$ ?]

Proc AsciiRecognition:
    Argument @Length
    Uses ecx

      ; First, for all Ascii Char, write a TEMPOFLAG into the relative SectionsMap Bytes:
        Mov esi D$UserPeStart | add esi D$FirstSection
        Mov ebx D$SectionsMap | add ebx D$FirstSection
        Mov edx D$TruthAsciiTable, eax 0

        sub D$UserPeEnd 3

        While esi < D$UserPeEnd
            test B$ebx IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG+CODEFLAG NOT_ZERO L2>
            Mov al B$esi

            .If B$edx+eax = GOODASCII
                If B$Forced = &TRUE
                    Mov B$ebx TEMPOFLAG
                Else
                    On B$ebx = 0, Mov B$ebx TEMPOFLAG
                End_If
            .End_If

L2:         inc esi | inc ebx
        End_While

      ; Append zero-ends:
        Mov esi D$SectionsMap | add esi D$FirstSection

        .While esi < D$EndOfSectionsMap
            .If B$esi = TEMPOFLAG
              ; Consider case of Strings Length >= D@length/2 :
                Mov ecx 0
                While B$esi = TEMPOFLAG | inc esi | inc ecx | End_While
                shl ecx 1 | cmp ecx D@Length | jb L2>

                If B$esi = 0
                  ; Write a TEMPOFLAG on the trailing zero, if any:
                    Mov ebx esi | sub ebx D$SectionsMap | add ebx D$UserPeStart
                    On B$ebx = 0, Mov B$esi TEMPOFLAG
                End_If
            .End_If

L2:         inc esi

        .End_While

      ; Delete too small Strings Chunks:
        Mov esi D$SectionsMap | add esi D$FirstSection
        Mov edx D$EndOfSectionsMap | sub edx 4

        .While esi < edx
            ..If B$esi = TEMPOFLAG
                Mov ecx 0, ebx esi
L0:             While B$esi = TEMPOFLAG
                    inc esi | inc ecx | On esi >= edx, jmp L0>
                End_While

              ; Append any following small Chunk (down to 5 uChars):
                .If B$esi+1 = TEMPOFLAG                 ; 1 Byte in between!
                    If D$esi+2 = FOURTEMPOFLAGS
                        add esi 6 | add ecx 5 | jmp L0<
                    End_If
                .Else_If B$esi+2 = TEMPOFLAG            ; 1 Word in between!
                    If D$esi+3 = FOURTEMPOFLAGS
                        add esi 7 | add ecx 5 | jmp L0<
                    End_If
               ; .Else_If B$esi+3 = TEMPOFLAG            ; 3 Bytes >>> unlikely!
                   ; If D$esi+4 = FOURTEMPOFLAGS
                   ;     add esi 8 | add ecx 5 | jmp L0<
                   ; End_If
              ;  .Else_If B$esi+4 = TEMPOFLAG            ; 1 dWord in between!
                   ; If D$esi+5 = FOURTEMPOFLAGS
                   ;     add esi 8 | add ecx 5 | jmp L0<
                   ; End_If
                .End_If

L0:             If ecx < D@Length
                  ; Too small > arase the TEMPOFLAGs
                    Push esi
L0:                     dec esi | Mov B$esi 0 | cmp esi ebx | ja L0<
                    Pop esi
                End_If
            ..End_If

            inc esi

        .End_While

      ; Now flag the long enough Chunks:
        Mov esi D$SectionsMap | add esi D$FirstSection

        .While esi < D$EndOfSectionsMap
            ..If B$esi = TEMPOFLAG
                Mov ebx esi
                While B$esi = TEMPOFLAG
                    inc esi | On esi >= D$EndOfSectionsMap, jmp L0>
                End_While

L0:             sub ebx D$SectionsMap | add ebx D$RoutingMap | or B$ebx EVOCATED
                sub ebx D$RoutingMap | add ebx D$SizesMap | Mov B$ebx STRINGS+BYTE
              ; and include trailing zeros in Data:
                Mov ebx esi | sub ebx D$SectionsMap | add ebx D$UserPeStart
                On ebx >= D$UserPeEnd, jmp L2>
                If W$ebx = 0
                    Mov eax esi | sub eax D$SectionsMap | add eax D$RoutingMap
                    or B$eax+2 EVOCATED
                    While B$ebx+1 = 0
                        Mov B$esi TEMPOFLAG | inc esi | inc ebx
                        On ebx >= D$UserPeEnd, jmp L2>
                    End_While
                Else
                    Mov eax esi | sub eax D$SectionsMap | add eax D$RoutingMap
                    or B$eax EVOCATED
                End_If

            ..End_If

            inc esi
        .End_While

L2:     Call ReplaceTempoFlagBy DATAFLAG, STRINGS+BYTE | add D$UserPeEnd 3
EndP
____________________________________________________________________________________________

Proc UnicodeRecognition:
    Argument @Length
    Uses ecx

      ; First, for all Ascii Char, write a TEMPOFLAG into the relative SectionsMap Bytes:
        Mov esi D$UserPeStart | add esi D$FirstSection
        Mov ebx D$SectionsMap | add ebx D$FirstSection
        Mov edx D$TruthAsciiTable, eax 0

        sub D$UserPeEnd 3

        While esi < D$UserPeEnd
            test B$ebx IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG+CODEFLAG NOT_ZERO L2>
            Mov al B$esi

            ..If B$edx+eax = GOODASCII
              ; If No Section Flag yet:
                On B$Forced = &TRUE, jmp L1>
                .If W$ebx = 0
L1:                 If B$esi+1 = 0
                        Mov B$ebx TEMPOFLAG, B$ebx+1 TEMPOFLAG
                        inc esi | inc ebx
                    End_If
                .End_If
            ..End_If

L2:         inc esi | inc ebx
        End_While

      ; Append zero-ends:
        Mov esi D$SectionsMap | add esi D$FirstSection

        .While esi < D$EndOfSectionsMap
            ..If B$esi = TEMPOFLAG
              ; Consider case of Strings Length >= D@length/2 :
                Mov ecx 0
                While B$esi = TEMPOFLAG | inc esi | inc ecx | End_While
                cmp ecx D@Length | jb L2>

                If W$esi = 0
                  ; Write a TEMPOFLAG on the trailing zero, if any:
                    Mov ebx esi | sub ebx D$SectionsMap | add ebx D$UserPeStart
                    On W$ebx = 0, Mov W$esi TWOTEMPOFLAGS
                End_If
            ..End_If

L2:         inc esi

        .End_While

      ; Delete too small Strings Chunks:
        Mov esi D$SectionsMap | add esi D$FirstSection

        .While esi < D$EndOfSectionsMap
            ..If B$esi = TEMPOFLAG
                Mov ecx 0, ebx esi
L0:             While B$esi = TEMPOFLAG
                    inc esi | inc ecx | On esi = D$EndOfSectionsMap, jmp L0>
                End_While

              ; Append any following small Chunk (down to 5 uChars):
                .If B$esi+1 = TEMPOFLAG                 ; 1 Byte in between!
                    If D$esi+2 = FOURTEMPOFLAGS
                        add esi 6 | add ecx 5 | jmp L0<
                    End_If
                .Else_If B$esi+2 = TEMPOFLAG            ; 1 Word in between!
                    If D$esi+3 = FOURTEMPOFLAGS
                        add esi 7 | add ecx 5 | jmp L0<
                    End_If
                .Else_If B$esi+3 = TEMPOFLAG            ; 3 Bytes >>> unlikely!
                   ; If D$esi+4 = FOURTEMPOFLAGS
                   ;     add esi 8 | add ecx 5 | jmp L0<
                   ; End_If
                .Else_If B$esi+4 = TEMPOFLAG            ; 1 dWord in between!
                    If D$esi+5 = FOURTEMPOFLAGS
                        add esi 8 | add ecx 5 | jmp L0<
                    End_If
                .End_If

L0:             shr ecx 1

                If ecx < D@Length
                  ; Too small > arase the TEMPOFLAGs
                    Push esi
L0:                     dec esi | Mov B$esi 0 | cmp esi ebx | ja L0<
                    Pop esi
                End_If
            ..End_If

            inc esi

        .End_While

      ; Now flag the long enough Chunks:
        Mov esi D$SectionsMap | add esi D$FirstSection

        .While esi < D$EndOfSectionsMap
            ..If B$esi = TEMPOFLAG
                Mov ebx esi
                While B$esi = TEMPOFLAG
                    inc esi | On esi = D$EndOfSectionsMap, jmp L0>
                End_While

L0:             sub ebx D$SectionsMap | add ebx D$RoutingMap | or B$ebx EVOCATED
                sub ebx D$RoutingMap | add ebx D$SizesMap | Mov B$ebx STRINGS+WORD
              ; and include trailing zeros in Data:
                Mov ebx esi | sub ebx D$SectionsMap | add ebx D$UserPeStart
                On ebx >= D$UserPeEnd, jmp L2>
                If W$ebx = 0
                    Mov eax esi | sub eax D$SectionsMap | add eax D$RoutingMap
                    or B$eax+2 EVOCATED
                    While W$ebx+2 = 0
                        Mov B$esi TEMPOFLAG | inc esi | inc ebx
                        Mov B$esi TEMPOFLAG | inc esi | inc ebx
                        On ebx >= D$UserPeEnd, jmp L2>
                    End_While
                Else
                    Mov eax esi | sub eax D$SectionsMap | add eax D$RoutingMap
                    or B$eax EVOCATED
                End_If

            ..End_If

            inc esi
        .End_While

L2:     Call ReplaceTempoFlagBy DATAFLAG, STRINGS+WORD | add D$UserPeEnd 3
EndP
____________________________________________________________________________________________

Proc ReplaceTempoFlagBy:
    Argument @SectionFlag, @FLAG

      ; replace, for example, all TEMPOFLAGs by DATAFLAGs:
L2:     Mov esi D$SectionsMap | add esi D$FirstSection
        Mov eax D@SectionFlag

        Mov ebx esi | sub ebx D$SectionsMap | add ebx D$SizesMap

        Mov ecx D@FLAG

        While esi < D$EndOfSectionsMap
            If B$esi = TEMPOFLAG
                Mov B$esi al
                or B$ebx cl
            End_If

            inc esi, ebx
        End_While
EndP

;;
  Small Strings before and after identified Strings are much likely also Strings.
  Not if this is inside Code that could as well be read as String.
;;
ExtendStrings: ret ; 'AsciiRecognition'
    Push ebp

    Mov esi D$SizesMap | add esi D$FirstSection
    Mov edx D$EndOfSizesMap, eax 0, ebp D$TruthAsciiTable

    Mov ebx esi | sub ebx D$SizesMap | add ebx D$SectionsMap
    Mov edx esi | sub edx D$SizesMap | add edx D$RoutingMap
    Mov edi esi | sub edi D$SizesMap | add edi D$UserPeStart

; esi > SizesMap // ebx > SectionsMap // edx > RoutingMap // edi > UserPeStart

    .While esi < edx
        test B$esi STRINGS ZERO L5>
          ; What is above? Data or Code?
            .If B$ebx-1 = CODEFLAG
              ; Are we at the end of a Code Chunk?
                If B$edx = CHUNKEND
                  ; Yes > do nothing.
                Else
                    Mov ecx 0
L0:                 inc ecx | dec edx | dec ebx
                    On B$ebx-1 <> CODEFLAG, jmp L2>
                        Mov al B$edi, al B$ebp+eax | On al = BADASCII, jmp L5>
                        test B$edx CHUNKEND ZERO L0<

L2:
                End_If

            .Else_If B$ebx-1 = DATAFLAG

            .End_If


L5:     inc esi | inc ebx | inc edi
    .End_While

    Pop ebp
ret


FillStringsSizes:
    Mov esi D$SizesMap | add esi D$FirstSection
    Mov ecx D$EndOfSizesMap

    Mov ebx esi | sub ebx D$SizesMap | add ebx D$UserPeStart

    Mov edx D$TruthAsciiTable, eax 0

    .While esi < ecx
        test B$esi STRINGS ZERO L2>
            If B$esi = STRINGS+BYTE
                Mov al B$ebx
                While D$edx+eax = GOODASCII
                    or B$esi STRINGS
                    inc esi, ebx
                End_While

                On B$esi = 0, or B$esi STRINGS

            Else_If B$esi = STRINGS+WORD
                Mov al B$ebx
                While D$edx+eax = GOODASCII
                    cmp B$esi+1 0 | jne L2>
                    or W$esi ((STRINGS shl 8)+STRINGS)
                    inc esi, ebx
                    inc esi, ebx
                End_While

                On W$esi = 0, or W$esi ((STRINGS shl 8)+STRINGS)
            End_If

L2:     inc esi, ebx
    .End_While
ret
____________________________________________________________________________________________

;;
  Repetitive Bytes may be real Instructions. Example:
  
  05, 05, 05, 05, 05   ; add eax 05050505
  C7,80, BC,00,00,00, 00,00,00,00  ; Mov D$eax+0BC 0
;;

MarkRepetitiveDataBytes:
    Mov esi D$UserPeStart | add esi D$FirstSection
    Mov edx D$UserPeEnd | sub edx 8

    Mov ebx esi | sub ebx D$UserPeStart | add ebx D$SectionsMap

    .While esi < edx
L0:     cmp D$ebx 0 | jne L8>
        cmp D$ebx+4 0 | jne L8>

            Mov al B$esi | cmp al 090 | je L8> ; Op90 nop
                           cmp al 0CC | je L8> ; OpCC int3
                           cmp al 0A4 | je L8> ; OpA4 movsb
                           cmp al 0A5 | je L8> ; OpA5 movsd
                           cmp al 0C3 | je L8> ; OpC3 ret

            cmp B$esi+1 al | jne L8>
            cmp B$esi+2 al | jne L8>
            cmp B$esi+3 al | jne L8>
            cmp B$esi+4 al | jne L8>
            cmp B$esi+5 al | jne L8>
            cmp B$esi+6 al | jne L8>
            cmp B$esi+7 al | jne L8>

              ; Case of dummy "Push/Pop eax" for all Registers >>> 9 cases:
                cmp al 050 | je L1>     ; Op50 Push eax
                cmp al 058 | jne L2>    ; Op58 Pop eax
L1:                 cmp B$esi+8 al | jne L8>

L2:             While B$esi = al
                    Mov B$ebx DATAFLAG
                    inc esi | inc ebx | On esi = edx, ret
                    cmp B$ebx 0 | jne L8>
                End_While

                jmp L0<

L8:     inc esi | inc ebx
    .End_While
ret


MarkVeryRepetitiveDataBytes:
    Mov esi D$UserPeStart | add esi D$FirstSection
    Mov edx D$UserPeEnd | sub edx 15

    Mov ebx esi | sub ebx D$UserPeStart | add ebx D$SectionsMap

    .While esi < edx
L0:     cmp D$ebx 0 | jne L2>
        cmp D$ebx+4 0 | jne L2>
        cmp D$ebx+8 0 | jne L2>

            movzx eax B$esi | cmp al 090 | je L2> ; Op90 nop
                              cmp al 0CC | je L2> ; OpCC int3
                              cmp al 0A4 | je L2> ; OpA4 movsb
                              cmp al 0A5 | je L2> ; OpA5 movsd
                              cmp al 0C3 | je L2> ; OpC3 ret

                Mov ecx eax | shl ecx 8 | or ecx eax | shl ecx 16 | or ecx eax
                cmp D$esi ecx | jne L2>
                cmp D$esi+4 ecx | jne L2>
                cmp D$esi+8 ecx | jne L2>

                    While B$esi = al
                        Mov B$ebx DATAFLAG
                        inc esi | inc ebx | On esi = edx, ret
                        cmp B$ebx 0 | jne L2>
                    End_While

                    jmp L0<

L2:     inc esi | inc ebx
    .End_While
ret

____________________________________________________________________________________________

MarkEvocatedSizes:
    Mov esi D$SizesMap, edx D$EndOfSizesMap | add esi D$FirstSection

    .While esi < edx
        Mov al B$esi

        ..If al <> 0
            .If al = POINTER
L1:             Mov ebx esi | sub ebx D$SizesMap | add ebx D$UserPeStart
                Mov ecx ebx | add ecx 100
                Push esi, edx, ebx
                    Call IsItCode ebx, ecx, 25
                Pop ebx, edx, esi

                sub ebx D$UserPeStart | add ebx D$SectionsMap
                If eax = &TRUE
                    On B$ebx = 0, Mov B$ebx CODEFLAG
                Else
                    On D$ebx = 0, Mov D$ebx FOURDATAFLAGS
                End_If

            .Else_If al = DWORD
                jmp L1<

            .Else
              ; Bytes, Words, qWords, FP >>> Data:
                If al < FP4     ; BYTE 1, WORD 00_10, ; // ;DWORD  00_100
                    movzx ecx al
                Else_If al = FP4
                    Mov ecx 4
                Else_If al = FP8
                    Mov ecx 8
                Else_If al = FP10
                    Mov ecx 10
                Else
                    jmp L5>
                End_If

                Mov ebx esi | sub ebx D$SizesMap | add ebx D$SectionsMap
               ; Force it:
L0:             cmp B$ebx VIRTUALFLAG | je L5>
                    Mov B$ebx DATAFLAG | inc ebx | loop L0<

            .End_If
        ..End_If

L5:     inc esi
    .End_While
ret

____________________________________________________________________________________________

[FlowNumberOfPointers: D$ ?]

MarkPointersFlows:
    Mov esi D$UserPeStart | add esi D$FirstSection
    Mov edx D$UserPeEnd | sub edx 4
    Mov D$FlowNumberOfPointers 0

L0:     .While esi < edx
            Mov eax esi | sub eax D$UserPeStart | add eax D$SectionsMap
            test B$eax IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG NOT_ZERO L5>>

            Mov eax D$esi | sub eax D$DisImageBase | add eax D$UserPeStart
            .If eax > D$UserPeStart
                If eax < D$UserPeEnd
                    sub eax D$UserPeStart | add eax D$SectionsMap
                    test B$eax IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG NOT_ZERO L5>>

                    inc D$FlowNumberOfPointers | add esi 4 | jmp L0<
                End_If
            .End_If

          ; Minimum number of Pointers in the flow: 4
            .If D$FlowNumberOfPointers > 3
                Mov ecx D$FlowNumberOfPointers | shl ecx 2 | sub esi ecx
                Mov edi esi | sub edi D$UserPeStart | add edi D$SizesMap
                Mov ebx edi | sub ebx D$SizesMap | add ebx D$RoutingMap
                Push edi
                    Mov ecx D$FlowNumberOfPointers

                    Mov D$edi POINTER | add edi 4
                    Mov D$ebx INDIRECT+LABEL+EVOCATED | add ebx 4
                    dec ecx

L1:                 Mov D$edi POINTER | add edi 4
                    Mov D$ebx INDIRECT | add ebx 4 | loop L1<
                Pop edi

                Mov ecx D$FlowNumberOfPointers | shl ecx 2
                sub edi D$SizesMap | add edi D$SectionsMap
                Mov al DATAFLAG | rep stosb
              ; Give a chance to downward Code Analyzes:
                sub edi D$SectionsMap | add edi D$RoutingMap | or B$edi EVOCATED

              ; Flag the Locations pointed to by the Pointers Flow:
                Mov ecx D$FlowNumberOfPointers
L1:             lodsd | sub eax D$DisImageBase | add eax D$RoutingMap
                or B$eax EVOCATED+LABEL | loop L1<

                Mov D$FlowNumberOfPointers 0 | jmp L0<<
            .End_If

L5:         Mov ecx 0

            inc esi | Mov D$FlowNumberOfPointers 0
        .End_While
ret


[NumberOfAlternatedPointers: D$ ?]

MarkAlternatedPointersFlows:
    Mov esi D$UserPeStart | add esi D$FirstSection
    Mov edx D$UserPeEnd | sub edx 4
    Mov D$NumberOfAlternatedPointers 0

L0: .While esi < edx
        Mov eax esi | sub eax D$UserPeStart | add eax D$SectionsMap
        test B$eax IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG NOT_ZERO L5>>
;Mov eax esi | sub eax D$UserPeStart | add eax D$ImageBase
;On eax = 077ED83D4, int3
        Mov eax D$esi | sub eax D$DisImageBase | add eax D$UserPeStart
        .If eax > D$UserPeStart
            If eax < D$UserPeEnd
                sub eax D$UserPeStart | add eax D$SectionsMap
                test B$eax IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG NOT_ZERO L5>>
                Call IsNBytesInstruction 4 | On eax = &TRUE, jmp L5>>
                ;hexprint D$NumberOfAlternatedPointers
                inc D$NumberOfAlternatedPointers | add esi 8 | jmp L0<<
            End_If
        .End_If

      ; Minimum number of Pointers in the flow: 4
        .If D$NumberOfAlternatedPointers > 3
        ;hexprint D$NumberOfAlternatedPointers
            Mov ecx D$NumberOfAlternatedPointers | shl ecx 3 | sub esi ecx
            Mov edi esi | sub edi D$UserPeStart | add edi D$SizesMap
            Mov ebx edi | sub ebx D$SizesMap | add ebx D$RoutingMap
          ; ebx > RoutingMap // edi >> SizesMap
            Push edi
                Mov ecx D$NumberOfAlternatedPointers

                Mov D$edi POINTER | add edi 8
                Mov D$ebx INDIRECT+LABEL+EVOCATED | add ebx 8
                dec ecx

L1:             Mov D$edi POINTER | add edi 8
                Mov D$ebx INDIRECT | add ebx 8 | loop L1<
            Pop edi

            Mov ecx D$NumberOfAlternatedPointers | shl ecx 3 | sub ecx 4
            sub edi D$SizesMap | add edi D$SectionsMap
            Mov al DATAFLAG | rep stosb
          ; Give a chance to downward Code Analyzes:
            sub edi D$SectionsMap | add edi D$RoutingMap | or B$edi EVOCATED

          ; Flag the Locations pointed to by the Pointers Flow:

            Mov ecx D$NumberOfAlternatedPointers
L1:         lodsd | add esi 4 | sub eax D$DisImageBase | add eax D$RoutingMap
            or B$eax EVOCATED | loop L1<

            Mov D$NumberOfAlternatedPointers 0 | jmp L0<<
        .End_If

L5:     Mov D$NumberOfAlternatedPointers 0

        inc esi
    .End_While
ret
____________________________________________________________________________________________

Proc IsNBytesInstruction:
    Argument @N
    Local @Pointer, @End
    Uses esi, edx

      ; @N: Number of Byte before the four bytes Address.
      ; esi: Points to the Four Bytes Address.

      ; Like for 'DecodeOnly':
        Mov B$WeAreInTheCodeBox &TRUE, B$SimpleScan &TRUE

        Mov D@Pointer esi
      ; D@Pointer points to the first Byte of the Adress Candidate.
        lea eax D$esi+4 | Mov D@End eax
      ; D@End points to the next Byte after the Address Candidate.
        sub esi D@N
      ; Esi points to the start Address of an Instruction Candidate, to be now tested.

      ; Disassemble: (DisMain)
L0:     Mov B$DisFlag 0, D$SegmentOverride 0, B$AddressSizeOverride 0
        Mov B$OperandSizeOverride 0, W$DisSizeMarker 'D$'
        Mov B$DisCodeDisplacement &FALSE, B$EscapePrefix &FALSE

        Mov edi DecodeText
L1:     movzx eax B$esi | inc esi | Call D$DisOp1+eax*4
        On B$DisFlag = DISDONE, jmp L1<

        .If esi = D@End
            Mov eax &TRUE
        .Else_If esi < D@Pointer
            jmp L0<
        ;.Else_If esi = D@Pointer
          ; The Pointer candidate could, itself, also be an Instruction:
        ;    Mov eax &TRUE
        .Else
            Mov eax &FALSE
        .End_If

        Mov B$WeAreInTheCodeBox &FALSE, B$SimpleScan &FALSE
EndP

____________________________________________________________________________________________

;;
  On a big File of 1 Mega, parsing the yet zeroed SectionsMap Bytes may be evaluated
  to parsing 500 Ko. (We execute this Recognition in between the positive and the
  megative Recognitions).
  ___________________________
  Probablity rude evaluations:
  
  * 500,000 on 4,294,967,295 (0_FFFF_FFFF) >>> say, 1 random chance on 2000.
  
    Two consecutive Pointer >>> say, 1 random chance on 4,000,000.
    Three consecutive Pointer >>> say, 1 random chance on 16,000,000,000,000.
    (Four consecutive Pointer and more are already identified erlier, by
    MarkPointersFlows'
  
  * For isolated Pointers (1 random chance on 2000) we also execute:
  
    'IsIsolatedPointer' >>> 'IsNBytesInstruction' that encreases a little bit
    the 1 on 2000 probablity when saying that the Pointer Candidate cannot be
    member of an Instruction (not guaranted at all, but better than nothing).
    
  * For Pointer to identified Code we also ensure that the Pointer does not
    point into the middle of a valid Instruction
;;

[PointerInData: D$ ?]

MarkIsolatedPointers: ; MarkPointersFlows
    Mov esi D$UserPeStart | add esi D$FirstSection
    Mov edx D$UserPeEnd | sub edx 4
    Mov D$FlowNumberOfPointers 0

L0: .While esi < edx
        Mov B$PointerInData &FALSE

        Mov eax esi | sub eax D$UserPeStart | add eax D$SectionsMap
        If B$eax = 0
            ;
        Else_If B$eax = DATAFLAG
            Mov B$PointerInData &TRUE
        Else
            jmp L5>>
        End_If

        Mov eax D$esi | sub eax D$DisImageBase | add eax D$UserPeStart

        .If eax > D$UserPeStart
            If eax < D$UserPeEnd
                sub eax D$UserPeStart | add eax D$SectionsMap
                test B$eax IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG NOT_ZERO L5>>
                test B$eax+1 IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG NOT_ZERO L5>>
                test B$eax+2 IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG NOT_ZERO L5>>
                test B$eax+3 IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG NOT_ZERO L5>>

                inc D$FlowNumberOfPointers | add esi 4 | jmp L0<<
            End_If
        .End_If

        .If D$FlowNumberOfPointers = 0
            ;
        .Else_If D$FlowNumberOfPointers = 1
            If B$PointerInData = &TRUE
                lea eax D$esi-4 | sub eax D$UserPeStart | add eax D$SizesMap
               ; test B$eax POINTER | jnz L1>
               ; test B$eax DWORD | jnz L1>
                On B$eax = POINTER, jmp L1>
                On B$eax = DWORD, jmp L1>
                Mov eax D$esi-4 | sub eax D$DisImageBase | add eax D$RoutingMap
                Test B$eax EVOCATED+LABEL+INDIRECT NOT_ZERO L1>

                On B$eax <> 0, jmp L5>>

L1:             Mov eax D$esi-4 | sub eax D$DisImageBase | add eax D$SectionsMap
                On B$eax <> CODEFLAG, jmp L1>>
                    sub eax D$SectionsMap | add eax D$RoutingMap
                    test B$eax INSTRUCTION NOT_ZERO L1>
            End_If
;;
  There is no logic that would tell if an isolated Pointer Candidate is or not
  a Pointer, by scaning weither or not, it could be some Code.
;;
            jmp L5>>
            ;sub esi 4 | Call IsIsolatedPointer | add esi 4

            ;If eax = &TRUE
            ;    Mov eax D$esi-4 | sub eax D$DisImageBase | add eax D$SectionsMap
            ;    On D$eax = 0, jmp L1>
            ;End_If

        .Else_If D$FlowNumberOfPointers = 2
            If B$PointerInData = &TRUE
                lea eax D$esi-4 | sub eax D$UserPeStart | add eax D$SizesMap
                test B$eax STRINGS NOT_ZERO L5>>

                jmp L1>
            End_If

            ;sub esi 8 | Call IsIsolatedPointer | add esi 8
            ;On eax = &TRUE, jmp L1>

        .Else
          ; Restore esi to the first Pointer:
L1:         Mov ecx D$FlowNumberOfPointers | shl ecx 2 | sub esi ecx
            Mov edi esi | sub edi D$UserPeStart | add edi D$SizesMap
            Mov ebx edi | sub ebx D$SizesMap | add ebx D$RoutingMap
            Push edi
                Mov ecx D$FlowNumberOfPointers
              ; First Pointer of a list:
                Mov D$edi POINTER | add edi 4
                Mov D$ebx INDIRECT+LABEL+EVOCATED | add ebx 4
                dec ecx | jz L2>
              ; Other Pointers if any:
L1:             Mov D$edi POINTER | add edi 4
                and D$ebx 0_FF | or D$ebx INDIRECT | add ebx 4 | loop L1<
L2:         Pop edi

            Mov ecx D$FlowNumberOfPointers | shl ecx 2
            sub edi D$SizesMap | add edi D$SectionsMap
            Mov al DATAFLAG | rep stosb
          ; Give a chance to downward Code Analyzes:
            sub edi D$SectionsMap | add edi D$RoutingMap | or B$edi EVOCATED

          ; Flag the Locations pointed to by the Pointers Flow:
            Mov ecx D$FlowNumberOfPointers

L1:         lodsd | sub eax D$DisImageBase | add eax D$RoutingMap
            or B$eax EVOCATED+LABEL | loop L1<

            Mov D$FlowNumberOfPointers 0 | jmp L0<<
        .End_If

L5:     inc esi | Mov D$FlowNumberOfPointers 0
    .End_While
ret


IsIsolatedPointer:
    Push esi, edx
      ; esi: UserPeStart (Four Bytes Pointer Adress)

;If D$esi = 04293E8 ; <<< Value of the Pointer Candidate
;    Push esi | sub esi D$UserPeStart | add esi D$DisImageBase
;        On esi = 041F1C0, int3 ; <<< Location
;    Pop esi
;End_If

        Mov ecx 0, ebx esi
        sub ebx D$UserPeStart | add ebx D$SectionsMap

        While B$ebx-1 = 0
        ; "Mov D$eax*4+ecx+01234 Data": 11 Bytes Instruction - 4 = 7 Bytes before Address
            inc ecx | dec ebx
            On ecx = 7, jmp L0>
        End_While

        ..If ecx <> 0
L0:         Push ecx
                Mov D$LastCodeRef 0 | Call IsNBytesInstruction ecx
            Pop ecx
            .If eax = &TRUE
                jmp L9>
            .Else
                Mov eax D$LastCodeRef
                If eax = D$esi
                    Mov eax &TRUE | jmp L9>
                End_If
            .End_If
            dec ecx | jnz L0<
            ;Mov D$LastCodeRef 0 | Call IsNBytesInstruction 0 | jmp L9>

        ..End_If

        Mov eax &FALSE

L9: Pop edx, esi

    xor eax &TRUE
ret
____________________________________________________________________________________________

;;
  When all possible Recognitions are done we generalize the found Flags in all
  the coming Sections. Based on the 'DisRvaSectionAlignment'.
;;

ExtendSections:
    Mov esi D$SectionsMap, edx D$EndOfSectionsMap | add esi D$FirstSection

  ; Ensure, first that the first Section starts with a valid Flag. If not, extend
  ; by the next coming one:
    If B$esi = 0
        Mov edi esi, ecx 0
        While B$esi = 0 | inc esi | inc ecx | End_While
        Mov al B$esi | rep stosb
    End_If

  ; Now unicize the Flags depending on the previous one. But, in case the next
  ; Flag is Data and the previous oneis  Code with CHUNKEND, we extend to Data:
    Mov esi D$SectionsMap, edx D$EndOfSectionsMap | add esi D$FirstSection
    .While esi < edx
        Mov al B$esi
        While B$esi = al
            inc esi | cmp esi edx | je L9>
        End_While
        Mov ebx esi
        While B$ebx = 0
            inc ebx | cmp ebx edx | je L1>
        End_While
        .If al = CODEFLAG
            If B$ebx = DATAFLAG
                Mov ebx esi | sub ebx D$SectionsMap | add ebx D$RoutingMap
                Test B$ebx CHUNKEND ZERO L1>
                    Mov al DATAFLAG
            End_If
        .End_If
L1:     While B$esi = 0
            Mov B$esi al | inc esi | cmp esi edx | je L9>
        End_While
    .End_While
L9: ret


StripSectionsZeroEnd:
    Mov esi D$UserPeStart, edx D$UserPeEnd | add esi D$FirstSection
    add esi D$DisRvaSectionAlignment | dec esi

    .While esi < edx
        Push esi

            Mov ecx 0
            While B$esi = 0
                Mov eax esi | sub eax D$UserPeStart | add eax D$RoutingMap
                On B$eax <> 0, jmp L1>
                inc ecx | dec esi | On ecx = D$DisRvaSectionAlignment, jmp L1>
            End_While
            inc esi

            .If ecx > 0
                Mov ebx esi | sub ebx D$UserPeStart | add ebx D$SectionsMap

                If B$ebx = DATAFLAG
                    sub ebx D$SectionsMap | add ebx D$RoutingMap
                    inc ebx | On ecx > 4, AlignOn 4 ebx
                    Mov B$ebx EVOCATED

                Else_If B$ebx = VIRTUALFLAG

                Else_If B$ebx <> KILLFLAG
                    Mov edi ebx, al 0 | rep stosb

                End_If

            .End_If

L1:     Pop esi

        add esi D$DisRvaSectionAlignment
    .End_While
ret
____________________________________________________________________________________________

[ItWasReallyAscii: D$ ?]

WriteCommentedAsciiData:
    ;Call CheckAsciiData |
    On D$BadAsciiSum = ecx, ret

    Push ebx, ecx, edi
        NextDisLine

        Mov D$NextStringBreak edi | add D$NextStringBreak 70

        .If B$ItWasReallyAscii = &TRUE
            If D$FirstDisDataByte <> 0
                Mov edi D$FirstDisDataByte | Mov B$edi '"' | inc edi | jmp L0>
            End_If
        .End_If

        Mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    '
        add edi 7 | Mov D$edi ';  B', D$edi+4 '$ " ' | add edi 7

K0:     Mov al B$ebx | inc ebx
            jmp L1>
L0:             loop K0<
                    jmp L9>>

L1:     ...If al = CR
            cmp B$ebx  LF | jne L2>
                 inc ebx | dec ecx | cmp ecx 0 | je L9>>
L1:         If B$ItWasReallyAscii = &TRUE
                On D$FirstDisDataByte = 0, jmp L1>
                NextDisLine
                Mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    ', D$edi+12 '    '
                add edi 13
            Else
L1:             Call NextDataDisLine
            End_If
            jmp L0<

        ...Else_If al = LF
L2:         Mov al '.' | inc edx

        ...Else_If al = 0
            inc edx
            ..If D$edi-4 = 'B$ "'
                Mov B$edi-1 '0'   ; > 'B$ 0'

            ..Else_If B$edi-1 = '0'
                If D$edi-4 = 'B$ 0'
                    Mov D$edi ', 0 ' | add edi 3
                Else_If W$edi-3 = '" '
                    Mov D$edi ', 0 ' | add edi 3
                Else_If W$edi-3 = ', '
                    Mov D$edi ', 0 ' | add edi 3
                Else
                    Mov D$edi '", 0' | add edi 4
                End_If

            ..Else
                Mov D$edi '" 0 ' | add edi 3

            ..End_If

            cmp ecx 1 | je L8>>
            .If B$ebx <> 0
                If B$ItWasReallyAscii = &TRUE
                    On D$FirstDisDataByte = 0, jmp L1>
                    NextDisLine
                    Mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    ', D$edi+12 '    '
                    add edi 13
                Else
L1:                 Call NextDataDisLine
                End_If
                Mov B$edi '"' | inc edi
            .End_If
            cmp ecx 1 | je L8>>
            jmp L0<<

        ...Else_If al = 9
            Mov eax '    ' | stosd

        ...Else_If al = '"'
            Mov D$edi '",02', D$edi+4 '2, "' | add edi 8 | jmp L0<<

        ...Else_If al < SPC
L2:         Mov al '.' | inc edx


        ...Else_If al > 127
            ; ... ???

        ...End_If

        .If B$edi-1 = '0'
            If W$edi-3 = '" '
                Mov D$edi ', " ' | add edi 3
            Else_If W$edi-3 = ', '
                Mov D$edi ', " ' | add edi 3
            Else
                ; ????
            End_If
        .End_If

        stosb

        If edi = D$NextStringBreak
            Mov W$edi '",', W$edi+2 CRLF | add edi 4
            Mov eax '    ' | stosd | stosd | stosd | stosd
            Mov W$edi-2 ';"'

            add D$NextStringBreak 88
        End_If

        jmp L0<<

L9:     Mov W$edi '" ' | add edi 2
L8:     Mov B$edi ']' | inc edi
    Pop eax, ecx, ebx
ret


[NextStringBreak: D$ ?
 InsideDataText: D$ ?]

Proc WriteAsciiData:
    Argument @Format
    Uses ebx, ecx

        Mov D$NextStringBreak edi | add D$NextStringBreak 70

        If D@Format = 1
            Mov D$edi 'B$ "'
        Else
            Mov D$edi 'U$ "'
        End_If
        add edi 4
        Mov B$InsideDataText &TRUE

L0:     Mov al B$ebx | add ebx D@Format

L1:     ...If al = CR
            Mov D$NextStringBreak edi | add D$NextStringBreak 70
            Mov eax D@Format | dec eax
            cmp B$ebx+eax LF | jne L2>
                 add ebx D@Format | sub ecx D@Format | cmp ecx 0 | jle L9>>
L1:         If B$ItWasReallyAscii = &TRUE
                On D$FirstDisDataByte = 0, jmp L1>
             ;   NextDisLine
             ;   Mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    ', D$edi+12 '    '
             ;   add edi 13
                Mov W$edi CRLF | add edi 2
            Else
L1:             Call NextDataDisLine
            End_If

            jmp L5>>

        ...Else_If al = LF
L2:         Mov al '.' | inc edx

        ...Else_If al = 0
           ; If B$InsideDataText = &TRUE
           ;     Mov D$edi '",  ' | add edi 3
           ;     Mov B$InsideDataText &FALSE
           ; End_If


L1:         ..If D$edi-4 = 'B$ "'
                Mov B$edi-1 '0'   ; > 'B$ 0'

            ..Else_If D$edi-4 = 'U$ "'
                Mov B$edi-1 '0'   ; > 'B$ 0'

            ..Else_If B$edi-1 = '0'
                If D$edi-4 = 'B$ 0'
                    Mov D$edi ', 0 ' | add edi 3
                Else_If D$edi-4 = 'U$ 0'
                    Mov D$edi ', 0 ' | add edi 3
                Else_If W$edi-3 = '" '
                    Mov D$edi ', 0 ' | add edi 3
                Else_If W$edi-3 = ', '
                    Mov D$edi ', 0 ' | add edi 3
                Else
L1:                 Mov D$edi '", 0' | add edi 4
                End_If

          ; Case of 'NextDisLine' in the previous pass:
            ..Else_If D$edi-4 = '    '
                On B$InsideDataText = &TRUE, jmp L1<
                Mov D$edi '    ', D$edi+4 '    ', D$edi+8 ', 0' | add edi 11

            ..Else
                Mov D$edi '" 0 ' | add edi 3

            ..End_If

            Mov B$InsideDataText &FALSE

            cmp ecx D@Format | jle L8>>

            ..If B$ebx <> 0
                .If D@Format = 2
                    If B$ItWasReallyUnicode = &TRUE
                      jmp L1>
                       ; NextDisLine
                       ; Mov D$NextStringBreak edi | add D$NextStringBreak 70
                    End_If
                .Else_If B$ItWasReallyAscii = &TRUE
L1:                 On D$FirstDisDataByte = 0, jmp L1>
                    NextDisLine
                    Mov D$NextStringBreak edi | add D$NextStringBreak 70
                    Mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    ', D$edi+12 '    '
                    add edi 13
                .Else
L1:                 Call NextDataDisLine
                    Mov D$NextStringBreak edi | add D$NextStringBreak 70
                .End_If
                Mov B$edi '"' | inc edi
                Mov B$InsideDataText &TRUE
            ..End_If

            jmp L5>>

        ...Else_If al = 9
            Mov eax '    ' | stosd | jmp L5>>

        ...Else_If al = '"'  ; 022, 34
            Mov D$edi '",02', D$edi+4 '2, "' | add edi 8 | jmp L5>>

        ...Else_If al < SPC
L2:         Mov al '.'

        ...Else_If al > 127
            ; ... ???

        ...End_If

        .If B$InsideDataText = &FALSE
            If D$edi-4 = '", 0'
                Mov D$edi ', " ' | add edi 3
            Else_If D$edi-4 = 'B$ 0'
                Mov D$edi ', " ' | add edi 3
            Else_If D$edi-4 = 'U$ 0'
                Mov D$edi ', " ' | add edi 3
            Else_If D$edi-3 = ', 0 '
                Mov D$edi ', " ' | add edi 3
            End_If
        .End_If

        stosb

L5:     .If edi > D$NextStringBreak
            If B$InsideDataText = &TRUE
                Mov W$edi '",', W$edi+2 CRLF, B$edi+4 '"' | add edi 5
            Else
                NextDisLine
            End_If
           ; Mov B$InsideDataText &TRUE
            Mov D$NextStringBreak edi |
            add D$NextStringBreak 70 ;88
        .End_If

L5:     sub ecx D@Format | jg L0<<

L9:     Mov W$edi '" ' | add edi 2
L8:     Mov B$edi ']' | inc edi
EndP

Proc NewWriteAsciiData:
    Argument @Format
    Uses ebx, ecx

        Mov D$NextStringBreak edi | add D$NextStringBreak 70

        If D@Format = 1
            Mov D$edi 'B$ "'
        Else
            Mov D$edi 'U$ "'
        End_If
        add edi 4
        Mov B$InsideDataText &TRUE

L0:     Mov al B$ebx | add ebx D@Format

L1:     ...If al = CR
            Mov D$NextStringBreak edi | add D$NextStringBreak 70
            Mov eax D@Format | dec eax
            cmp B$ebx+eax LF | jne L2>
                 add ebx D@Format | sub ecx D@Format | cmp ecx 0 | jle L9>>
L1:         If B$ItWasReallyAscii = &TRUE
                On D$FirstDisDataByte = 0, jmp L1>
             ;   NextDisLine
             ;   Mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    ', D$edi+12 '    '
             ;   add edi 13
                Mov W$edi CRLF | add edi 2
            Else
L1:             Call NextDataDisLine
            End_If

            jmp L5>>

        ...Else_If al = LF
L2:         Mov al '.' | inc edx

        ...Else_If al = 0
           ; If B$InsideDataText = &TRUE
           ;     Mov D$edi '",  ' | add edi 3
           ;     Mov B$InsideDataText &FALSE
           ; End_If


L1:         ..If D$edi-4 = 'B$ "'
                Mov B$edi-1 '0'   ; > 'B$ 0'

            ..Else_If D$edi-4 = 'U$ "'
                Mov B$edi-1 '0'   ; > 'B$ 0'

            ..Else_If B$edi-1 = '0'
                If D$edi-4 = 'B$ 0'
                    Mov D$edi ', 0 ' | add edi 3
                Else_If D$edi-4 = 'U$ 0'
                    Mov D$edi ', 0 ' | add edi 3
                Else_If W$edi-3 = '" '
                    Mov D$edi ', 0 ' | add edi 3
                Else_If W$edi-3 = ', '
                    Mov D$edi ', 0 ' | add edi 3
                Else
L1:                 Mov D$edi '", 0' | add edi 4
                End_If

          ; Case of 'NextDisLine' in the previous pass:
            ..Else_If D$edi-4 = '    '
                On B$InsideDataText = &TRUE, jmp L1<
                Mov D$edi '    ', D$edi+4 '    ', D$edi+8 ', 0' | add edi 11

            ..Else
                ;Mov D$edi '" 0 ' | add edi 3 ; <<<<<<<<<<<<<<<<<<<<<<<<<<<
                Mov D$edi '", 0' | add edi 4
                NextDisLine
                ;Mov B$edi SPC | inc edi

            ..End_If

            Mov B$InsideDataText &FALSE

            cmp ecx D@Format | jle L8>>

            ..If B$ebx <> 0
                .If D@Format = 2
                    If B$ItWasReallyUnicode = &TRUE
                      jmp L1>
                       ; NextDisLine
                       ; Mov D$NextStringBreak edi | add D$NextStringBreak 70
                    End_If
                .Else_If B$ItWasReallyAscii = &TRUE
L1:                 On D$FirstDisDataByte = 0, jmp L1>
                    NextDisLine
                    Mov D$NextStringBreak edi | add D$NextStringBreak 70
                    Mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    ', D$edi+12 '    '
                    add edi 13
                .Else
L1:                 ;Call NextDataDisLine
                    NextDisLine
                    Mov D$NextStringBreak edi | add D$NextStringBreak 70
                .End_If
                Mov B$edi '"' | inc edi
                Mov B$InsideDataText &TRUE
            ..End_If

            jmp L5>>

        ...Else_If al = 9
            Mov eax '    ' | stosd | jmp L5>>

        ...Else_If al = '"'  ; 022, 34
            Mov D$edi '",02', D$edi+4 '2, "' | add edi 8 | jmp L5>>

        ...Else_If al < SPC
L2:         Mov al '.'

        ...Else_If al > 127
            ; ... ???

        ...End_If

        .If B$InsideDataText = &FALSE
            If D$edi-4 = '", 0'
                Mov D$edi ', " ' | add edi 3
            Else_If D$edi-4 = 'B$ 0'
                Mov D$edi ', " ' | add edi 3
            Else_If D$edi-4 = 'U$ 0'
                Mov D$edi ', " ' | add edi 3
            Else_If D$edi-3 = ', 0 '
                Mov D$edi ', " ' | add edi 3
            End_If
        .End_If

        stosb

L5:     .If edi > D$NextStringBreak
            If B$InsideDataText = &TRUE
                Mov W$edi '",', W$edi+2 CRLF, B$edi+4 '"' | add edi 5
            Else
                NextDisLine
            End_If
           ; Mov B$InsideDataText &TRUE
            Mov D$NextStringBreak edi |
            add D$NextStringBreak 70 ;88
        .End_If

L5:     sub ecx D@Format | jg L0<<

L9:     Mov W$edi '" ' | add edi 2
L8:     Mov B$edi ']' | inc edi
EndP


WriteEcxByte:
L0: movzx eax B$esi | inc esi
    Mov ebx eax | shr ebx 4
    and eax 0F | and ebx 0F
    Mov al B$STR.A.Hexa+eax, bl B$STR.A.Hexa+ebx
    shl eax 8 | or eax ebx | or eax 020200000 | Mov D$edi eax | add edi 3 | loop L0<

ret

WriteImm8:
    movzx ebx B$esi | inc esi | jmp S1>
WriteImm16:
    movzx ebx W$esi | add esi 2 | jmp S1>
WriteImm32:  ; WriteDis32
    On B$OperandSizeOverride = &TRUE, jmp WriteImm16

    lodsd | On B$SimpleScan = &TRUE, jmp WriteEax

    Mov ebx eax | sub ebx D$DisImageBase | add ebx D$SectionsMap
    If ebx >= D$EndOfSectionsMap
        ; >>> WriteEax
    Else_If ebx <= D$SectionsMap
        ; >>> WriteEax
    Else
        Mov bl B$ebx
        and bl DATAFLAG+VIRTUALFLAG+CODEFLAG | jnz WriteDisRelative
    End_If

WriteEax:

S1: Mov edx eax

    Push NA

L1: Mov eax edx

        shr edx 4 | and eax 0_F | Mov al B$STR.A.Hexa+eax

        Push eax

    Test edx NA NOT_ZERO L1<

    Mov al '0' | stosb

L2: Pop eax

        Comp eax NA = S0>

    stosb | jmp L2<

S0: ret


WriteSignedImm32:

    Mov ebx D$esi | add esi 4 | jmp L0>

WriteSignedImm16:

    movsx ebx W$esi | add esi 2 | jmp L0>

WriteSignedImm8:

    movsx ebx B$esi | inc esi

L0: Push 0-1

    test ebx dWordHighbit ZERO L0>

        If B$edi-1 = '+'

            Mov B$edi-1 '-'

        Else

            Mov W$edi '0-' | add edi 2

        End_If

        neg ebx

L0: Mov eax ebx | shr ebx 4 | and eax 0F

    Mov al B$STR.A.Hexa+eax

    Push eax

    cmp ebx 0 | ja L0<

    Mov B$edi '0' | inc edi

L0: Pop eax | cmp eax 0-1 | je L9>

    Mov B$edi al | inc edi | jmp L0<

L9: ret

UnlikelyOut:
    Push eax
        Mov eax D$esi
        sub eax D$DisImageBase | add eax D$SectionsMap
        On eax >= D$EndOfSectionsMap, jmp L8>
        On eax <= D$SectionsMap, jmp L8>
    Pop eax
ret

L8: If D$SegmentOverride = 0
      ; In case, for example of 'fs:0', this would be normal
        add B$UnlikelyCode 0F
    End_If
    Pop eax | ret

;;
UnlikelyOut:
    Push eax
        Mov eax D$esi
        sub eax D$DisImageBase | add eax D$SectionsMap
        On eax >= D$EndOfSectionsMap, jmp L8>
        On eax <= D$SectionsMap, jmp L8>
    Pop eax
ret

L8: add B$UnlikelyCode 0FF
    Pop eax | ret
;;

; Input: bl = Mod (0 / 1 / 2.  - 3 is already done before calling here by a Call to
; WriteEregsFromRmBits -)

WriteEffectiveAddressFromModRm:  ; 044 00_100_100    015 00_010_101
    On B$AddressSizeOverride = &TRUE, jmp WriteEffectiveAddressFromModRm16
    ModMask bl To al

    .If al = 0
        Call StartEffectiveAddress | RmMask bl To al

        If al = 0      | Mov D$edi 'eax ' | add edi 4
        Else_If al = 1 | Mov D$edi 'ecx ' | add edi 4
        Else_If al = 2 | Mov D$edi 'edx ' | add edi 4
        Else_If al = 3 | Mov D$edi 'ebx ' | add edi 4
        Else_If al = 4 | Call WriteFromSib
            ..If cl = 0FF
                Call WriteBase5dis32
            ..End_If
        Else_If al = 5
            Call UnlikelyOut
            Call Writedis32
        Else_If al = 6 | Mov D$edi 'esi ' | add edi 4
        Else           | Mov D$edi 'edi ' | add edi 4
        End_If

    .Else_If al = 1
        Call StartEffectiveAddress | RmMask bl To al

        If al = 0      | Mov D$edi 'eax+' | add edi 4
        Else_If al = 1 | Mov D$edi 'ecx+' | add edi 4
        Else_If al = 2 | Mov D$edi 'edx+' | add edi 4
        Else_If al = 3 | Mov D$edi 'ebx+' | add edi 4
        Else_If al = 4 | Call WriteFromSib
            ..If B$edi-1 <> '+'
                Mov B$edi '+' | inc edi
            ..End_If
        Else_If al = 5 | Mov D$edi 'ebp+' | add edi 4
        Else_If al = 6 | Mov D$edi 'esi+' | add edi 4
        Else           | Mov D$edi 'edi+' | add edi 4
        End_If

        Call WriteSignedImm8 ;| Mov W$edi ax | add edi 2  ; OpToHexa

    .Else_If al = 2
        Call StartEffectiveAddress | RmMask bl To al

        If al = 0      | Mov D$edi 'eax+' | add edi 4
        Else_If al = 1 | Mov D$edi 'ecx+' | add edi 4
        Else_If al = 2 | Mov D$edi 'edx+' | add edi 4
        Else_If al = 3 | Mov D$edi 'ebx+' | add edi 4
        Else_If al = 4 | Call WriteFromSib
            ..If B$edi-1 <> '+'
                Mov B$edi '+' | inc edi
            ..End_If
        Else_If al = 5 | Mov D$edi 'ebp+' | add edi 4
        Else_If al = 6 | Mov D$edi 'esi+' | add edi 4
        Else           | Mov D$edi 'edi+' | add edi 4
        End_If

        Call Writedis32

    .Else ; bl = 3
        If W$DisSizeMarker = 'D$'
            Call WriteEregsFromRmBits
        Else_If W$DisSizeMarker = 'B$'
            Call WriteByteRegsFromRmBits
        Else_If W$DisSizeMarker = 'W$'
            Call WriteWordRegsFromRmBits
        End_If

    .End_If

    If W$edi-2 = '00'
        On B$edi-3 = '+', sub edi 3
    End_If
ret


WriteEffectiveAddressFromModRm16:
    inc B$UnlikelyCode

    ModMask bl To al

    .If al = 0
        Call StartEffectiveAddress | RmMask bl To al

        If al = 0      | Mov D$edi 'bx+s', D$edi+4 'i ' | add edi 6
        Else_If al = 1 | Mov D$edi 'bx+d', D$edi+4 'i ' | add edi 6
        Else_If al = 2 | Mov D$edi 'bp+s', D$edi+4 'i ' | add edi 6
        Else_If al = 3 | Mov D$edi 'bp+d', D$edi+4 'i ' | add edi 6
        Else_If al = 4 | Mov D$edi 'si ' | add edi 3
        Else_If al = 5 | Mov D$edi 'di ' | add edi 3
        Else_If al = 6 | Call Writedis16
        Else           | Mov D$edi 'bx ' | add edi 3
        End_If

    .Else_If al = 1
        Call StartEffectiveAddress | RmMask bl To al

        If al = 0      | Mov D$edi 'bx+s', D$edi+4 'i ' | add edi 6
        Else_If al = 1 | Mov D$edi 'bx+d', D$edi+4 'i ' | add edi 6
        Else_If al = 2 | Mov D$edi 'bp+s', D$edi+4 'i ' | add edi 6
        Else_If al = 3 | Mov D$edi 'bp+d', D$edi+4 'i ' | add edi 6
        Else_If al = 4 | Mov D$edi 'si ' | add edi 3
        Else_If al = 5 | Mov D$edi 'di ' | add edi 3
        Else_If al = 6 | Mov D$edi 'bp ' | add edi 3
        Else           | Mov D$edi 'bx ' | add edi 3
        End_If

        Call WriteSignedImm8 ;| Mov W$edi ax | add edi 2  ; OpToHexa

    .Else_If al = 2
        Call StartEffectiveAddress | RmMask bl To al

        If al = 0      | Mov D$edi 'bx+s', D$edi+4 'i ' | add edi 6
        Else_If al = 1 | Mov D$edi 'bx+d', D$edi+4 'i ' | add edi 6
        Else_If al = 2 | Mov D$edi 'bp+s', D$edi+4 'i ' | add edi 6
        Else_If al = 3 | Mov D$edi 'bp+d', D$edi+4 'i ' | add edi 6
        Else_If al = 4 | Mov D$edi 'si ' | add edi 3
        Else_If al = 5 | Mov D$edi 'di ' | add edi 3
        Else_If al = 6 | Mov D$edi 'bp ' | add edi 3
        Else           | Mov D$edi 'bx ' | add edi 3
        End_If

        Call WriteSignedImm16

    .Else ; bl = 3
        If W$DisSizeMarker = 'D$'
            Call WriteEregsFromRmBits
        Else_If W$DisSizeMarker = 'B$'
            Call WriteByteRegsFromRmBits
        Else_If W$DisSizeMarker = 'W$'
            Call WriteWordRegsFromRmBits
        End_If

    .End_If

    If W$edi-2 = '00'
        On B$edi-3 = '+', sub edi 3
    End_If
ret


WriteEffectiveXMMAddressFromModRm:
    ModMask bl To al

    .If al = 0
        Call StartEffectiveAddress | RmMask bl To al

        If al = 0      | Mov D$edi 'eax ' | add edi 4
        Else_If al = 1 | Mov D$edi 'ecx ' | add edi 4
        Else_If al = 2 | Mov D$edi 'edx ' | add edi 4
        Else_If al = 3 | Mov D$edi 'ebx ' | add edi 4
        Else_If al = 4 | Call WriteFromSib | On cl = 0FF, Call WriteBase5dis32
        Else_If al = 5 | Call UnlikelyOut | Call Writedis32
        Else_If al = 6 | Mov D$edi 'esi ' | add edi 4
        Else           | Mov D$edi 'edi ' | add edi 4
        End_If

    .Else_If al = 1
        Call StartEffectiveAddress | RmMask bl To al

        If al = 0      | Mov D$edi 'eax+' | add edi 4
        Else_If al = 1 | Mov D$edi 'ecx+' | add edi 4
        Else_If al = 2 | Mov D$edi 'edx+' | add edi 4
        Else_If al = 3 | Mov D$edi 'ebx+' | add edi 4
        Else_If al = 4 | Call WriteFromSib | Mov B$edi '+' | inc edi
        Else_If al = 5 | Mov D$edi 'ebp+' | add edi 4
        Else_If al = 6 | Mov D$edi 'esi+' | add edi 4
        Else           | Mov D$edi 'edi+' | add edi 4
        End_If

        Call WriteSignedImm8 ;;;OpToHexa | Mov W$edi ax | add edi 2

    .Else_If al = 2
        Call StartEffectiveAddress | RmMask bl To al

        If al = 0      | Mov D$edi 'eax+' | add edi 4
        Else_If al = 1 | Mov D$edi 'ecx+' | add edi 4
        Else_If al = 2 | Mov D$edi 'edx+' | add edi 4
        Else_If al = 3 | Mov D$edi 'ebx+' | add edi 4
        Else_If al = 4 | Call WriteFromSib | Mov B$edi '+' | inc edi
        Else_If al = 5 | Mov D$edi 'ebp+' | add edi 4
        Else_If al = 6 | Mov D$edi 'esi+' | add edi 4
        Else           | Mov D$edi 'edi+' | add edi 4
        End_If

        Call Writedis32

    .Else ; bl = 3
        Call WriteXMMregsFromRmBits

    .End_If

    If W$edi-2 = '00'
        On B$edi-3 = '+', sub edi 3
    End_If
ret


WriteEffectiveMMXAddressFromModRm: ; 04  00_000_100
    ModMask bl To al

    .If al = 0
        Call StartEffectiveAddress | RmMask bl To al

        If al = 0      | Mov D$edi 'eax ' | add edi 4
        Else_If al = 1 | Mov D$edi 'ecx ' | add edi 4
        Else_If al = 2 | Mov D$edi 'edx ' | add edi 4
        Else_If al = 3 | Mov D$edi 'ebx ' | add edi 4
        Else_If al = 4 | Call WriteFromSib | On cl = 0FF, Call WriteBase5dis32
        Else_If al = 5 | Call UnlikelyOut | Call Writedis32
        Else_If al = 6 | Mov D$edi 'esi ' | add edi 4
        Else           | Mov D$edi 'edi ' | add edi 4
        End_If

    .Else_If al = 1
        Call StartEffectiveAddress | RmMask bl To al

        If al = 0      | Mov D$edi 'eax+' | add edi 4
        Else_If al = 1 | Mov D$edi 'ecx+' | add edi 4
        Else_If al = 2 | Mov D$edi 'edx+' | add edi 4
        Else_If al = 3 | Mov D$edi 'ebx+' | add edi 4
        Else_If al = 4 | Call WriteFromSib | Mov B$edi '+' | inc edi
        Else_If al = 5 | Mov D$edi 'ebp+' | add edi 4
        Else_If al = 6 | Mov D$edi 'esi+' | add edi 4
        Else           | Mov D$edi 'edi+' | add edi 4
        End_If

        Call WriteSignedImm8 ;;;OpToHexa | Mov W$edi ax | add edi 2

    .Else_If al = 2
        Call StartEffectiveAddress | RmMask bl To al

        If al = 0      | Mov D$edi 'eax+' | add edi 4
        Else_If al = 1 | Mov D$edi 'ecx+' | add edi 4
        Else_If al = 2 | Mov D$edi 'edx+' | add edi 4
        Else_If al = 3 | Mov D$edi 'ebx+' | add edi 4
        Else_If al = 4 | Call WriteFromSib | Mov B$edi '+' | inc edi
        Else_If al = 5 | Mov D$edi 'ebp+' | add edi 4
        Else_If al = 6 | Mov D$edi 'esi+' | add edi 4
        Else           | Mov D$edi 'edi+' | add edi 4
        End_If

        Call Writedis32

    .Else ; bl = 3
        Call WriteMMXregsFromRmBits

    .End_If

    If W$edi-2 = '00'
        On B$edi-3 = '+', sub edi 3
    End_If
ret
____________________________________________________________________________________________

[NoDirectAccess: B$ ' ; Not accessed by direct Call or JMP.' EOS]

[ActualDisCodePointer: D$ ?
 BackStep: D$ ?
 LabelWritten: D$ ?
 LastWrittenLabel: D$ ?]

WriteDisCodeLabel: ; 'WriteOneDataLabel'
    On esi = D$LastWrittenLabel, ret
    Mov D$LastWrittenLabel esi

    Mov eax esi | ToStringsMapFrom UserPeStart, eax
    .If D$eax <> 0
        Push esi
            If D$eax = EntryPointName

               Push esi | Mov esi D$eax | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0: Pop esi

            Else_If D$eax = MainWindowProcName
                Mov W$edi CRLF | add edi 2

                Push esi | Mov esi D$eax | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0: Pop esi

                Mov B$edi ':' | inc edi
            End_If
        Pop esi
    .End_If

    Mov eax esi | sub eax D$UserPeStart | add eax D$DisImageBase
  ; >>> eax = RVA. ebx = 'RoutingMap' pointer.

   ; Call BuildCommentedCodeReference ebx ; made by Guga

    test B$ebx EXPORTNODE ZERO L0>
        Call WriteExportedFunctionLabel

L0: Mov ecx ebx, ebx eax
    Push ebx
        If W$edi-3 <> '::'
            Mov W$edi CRLF | add edi 2
        End_If

        Mov D$edi 'Code' | add edi 4
        Push 0-1

L0:     Mov eax ebx | shr ebx 4 | and eax 0F
        add eax '0' | On eax > '9', add eax 7
        Push eax
        cmp ebx 0 | ja L0<
        Mov B$edi '0' | inc edi
L0:     Pop eax | cmp eax 0-1 | je L1>
        Mov B$edi al | inc edi | jmp L0<

L1: Pop eax | Push eax

    Mov eax esi | ToStringsMapFrom UserPeStart, eax
    .If D$eax <> 0
        If D$eax = EntryPointName
            ;
        Else_If D$eax = MainWindowProcName
            ;
        Else

            Push esi | Mov esi D$eax | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0: Pop esi

        End_If
    .End_If

    Mov W$edi ': ' | add edi 2

L2: Pop eax
    Mov ecx 10, edx 0 | div ecx | add dl '0'
    and al 001111 | add al 'A'
    Mov B$edi al, B$edi+1 dl, B$edi+2 ':' | add edi 3

L9: ret


WriteExportedFunctionLabel:
  ; >>> eax = RVA. ebx = 'RoutingMap' pointer.       'ExportSectionComments' 'CheckExport'

    Push eax, ebx, esi
        sub eax D$DisImageBase

      ; May be a wrong Flag resulting from a DLL Name pointer saved inside the
      ; 'RoutingMap'. If so, the relative 'SectionsMap' Byte is 'IMPORTFLAG':
        Mov ecx D$NumberOfDisExportedFunctions | cmp ecx 0 | je L9>>

        Mov esi D$DisExportFunctionsPointers, ebx D$DisExportNamesPointers

        .If esi < D$UserPeStart
            ; nop
        .Else_If esi > D$UserPeEnd
            ; nop
        .Else
          ; Scan the Functions Pointers, to get the Ordinal Indice:
            Mov edx D$UserPeEnd | sub edx 4
L0:         cmp esi edx | jae L9>>
            cmp D$esi eax | je L1>
K0:             add esi 4 | add ebx 4 | loop L0<
                    jmp L9>>

L1:         Push eax, ebx, ecx, edx, esi

            Mov eax D$NumberOfDisExportedFunctions | sub eax ecx

          ; Search the Ordinal Number, in the Ordinal Table
            Mov esi D$DisExportOrdinal, ecx D$NumberOfDisExportedFunctions

L2:         cmp esi edx | jae L8>>
            cmp W$esi ax | je L3>
              ; Ordinal are Words:
                add esi 2 | loop L2<
                    jmp L8>>

L3:         Mov eax D$NumberOfDisExportedFunctions | sub eax ecx

          ; Get the parallel Pointer, in the Functions Pointers Table:
            Mov esi D$DisExportNamesPointers | shl eax 2 | add esi eax
            Mov esi D$esi | add esi D$UserPeStart

            On esi < D$UserPeStart, jmp L8>>
                On esi > D$UserPeEnd, jmp L8>>
                    ;sub edi 4

                    While B$edi-1 = SPC | dec edi | End_While
                    If B$edi-3 > SPC
                        Mov W$edi CRLF | add edi 2
                    End_If
                    Push esi
                        Mov bl 0, dl 0
                        While B$esi <> 0
                            lodsb
                                If al = '@'
                                    On dl = 0, Mov al 'a', bl 1
                                Else_If al = '$'
                                    Mov al 'S', bl 1
                                Else_If al = '$'
                                    Mov al 'S', bl 1
                                Else_If al <> '_'
                                    inc dl
                                End_If
                            stosb
                        End_While
                        Mov D$edi '::  ' | add edi 3
                    Pop esi
                    If bl = 1

                        Push esi | Mov esi {B$ '; The_real_Name_was: ' EOS} | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0: Pop esi

                        While B$esi <> 0 | movsb | End_While
                        Mov D$edi '::  ' | add edi 3
                        Mov W$edi CRLF | add edi 2
                    End_If

                    Pop esi, edx, ecx, ebx, eax | jmp K0<<

L8:             Pop esi, edx, ecx, ebx, eax
        .End_If

L9: Pop esi ebx, eax
ret
____________________________________________________________________________________________

; Provides Local Labels, depending on the Code Pointer Value, in the range of
; A0-A9 to P0-P9:

WriteLocalLabelFromEax:
    Mov ecx 10, edx 0 | div ecx | add dl '0'
    and al 001111 | add al 'A'
    Mov B$edi al, B$edi+1 dl, B$edi+2 ':' | add edi 3
ret


[BadDecodeText: B$ "
 ; Downward Chunk would be bad if previous one was good. On Reference, aligning back: "
 BackSteps: "          
"
 BadDecodeTextLen: Len
 CommentItHexa: "; Get the Bytes Values with the Dialog option: [With Commented Hexa Code].
 ", CommentItHexaLen: Len]

BadDecode:
    Push eax, ebx, ecx, esi
        Push edi
            Mov D$BackSteps '    ', D$BackSteps+4 '    '
            Mov edi BackSteps
            Mov eax esi | sub eax ebx | Call WriteEax
        Pop edi

        Mov esi BadDecodeText, ecx D$BadDecodeTextLen | rep movsb

        If B$WithCommentedHexa = &FALSE
            Mov esi CommentItHexa, ecx D$CommentItHexaLen | rep movsb
        End_If
    Pop esi, ecx, ebx, eax
ret


[CALLInstruction: D$ ?
 MovOrJmpImmInstruction: D$ ?]

RelativeToAbsolute:
 ; When called, eax = relative Dis
    Mov eax D$LastCodeRef
    add eax esi | sub eax D$UserPeStart | add eax D$DisImageBase

    Push eax
        sub eax D$DisImageBase | add eax D$SectionsMap

        .If eax < D$EndOfSectionsMap
            If eax > D$SectionsMap
                jmp L1>
            End_If
        .End_If

        add D$UnlikelyCode 0F
L1: Pop eax
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

Dis_St0Sti_or_Fmem:
    ModMask bl to al | On al <> 3, jmp EndWith.F.mem

WriteSt0Sti: ; (Byte in bl).
    Mov D$edi 'ST0 ', D$edi+4 'ST0 '
    RmMask bl to al
    add B$edi+6 al
    add edi 7
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


WriteStiSt0:
    Mov D$edi 'ST0 ', D$edi+4 'ST0 '
    RmMask bl to al
    add B$edi+2 al
    add edi 7
    Mov B$DisFlag DISDONE+DISLINEOVER
ret


WriteSti:
    Mov D$edi 'ST0 '
    RmMask bl to al
    add B$edi+2 al
    add edi 3
    Mov B$DisFlag DISDONE+DISLINEOVER
ret
____________________________________________________________________________________________



;;; 'Dis_mmx2_m128__xmm1'
; When called, ESI position is on the Mod/Reg/Rm Byte.

Proc MarkSSEdata:
    Arguments @Type
    Uses esi

        On B$WeAreInTheCodeBox = &TRUE EndP

      ; Remove the 'reg' bits from the mod/r/m byte:
        lodsb | and al 00_11_000_111 | On al <> 00_000_101 EndP

        lodsd | sub eax D$DisImageBase | add eax D$RoutingMap

        .If eax < D$RoutingMap
            Mov B$UnlikelyCode 5

        .Else_If eax > D$EndOfRoutingMap
            Mov B$UnlikelyCode 5

        .Else
            or B$eax LABEL+EVOCATED

            sub eax D$RoutingMap | add eax D$SizesMap

            If D@Type = SSE_4_F
                Mov D$eax FP4, D$eax+4 FP4, D$eax+8 FP4, D$eax+12 FP4

L4:             sub eax D$SizesMap | add eax D$SectionsMap
                On D$eax <> VIRTUALFLAG,
                    Mov D$eax FOURDATAFLAGS, D$eax+4 FOURDATAFLAGS,
                        D$eax+8 FOURDATAFLAGS, D$eax+12 FOURDATAFLAGS

            Else_If D@Type = SSE_2_R
                Mov D$eax FP8, D$eax+8 FP8 | jmp L4<

            End_If

        .End_If
EndP

[SSE_4_F 1, SSE_2_R 2, SSE_1_R 3, SSE_1_F 4, SSE_2_F 5,
 SSE_4_D 6, SSE_2_D 7, SSE_1_D 8, SSE_1_Q 9]

;;
  List of SSE Mnemonics >>> Types:  (Call MarkSSEdata )
  
  addpd     Call MarkSSEdata SSE_2_R
  addps     Call MarkSSEdata SSE_4_F
  addsd     Call MarkSSEdata SSE_1_R
  addss     Call MarkSSEdata SSE_1_F
  addsubpd  Call MarkSSEdata SSE_2_R
  addsubps  Call MarkSSEdata SSE_4_F
  andnpd    Call MarkSSEdata SSE_2_R
  andnps    Call MarkSSEdata SSE_4_F
  andpd     Call MarkSSEdata SSE_2_R
  andps     Call MarkSSEdata SSE_4_F
  cmp..ps   Call MarkSSEdata SSE_4_F opc2
  cmp..ss   Call MarkSSEdata SSE_2_F
  comisd    Call MarkSSEdata SSE_1_R
  comiss    Call MarkSSEdata SSE_1_F
  cvtdq2pd  Call MarkSSEdata SSE_2_D
  cvtdq2ps  Call MarkSSEdata SSE_4_D
  cvtpd2dq  Call MarkSSEdata SSE_2_R   ; F2 0F E6 /r opf2
  cvtpd2pi  Call MarkSSEdata SSE_2_R
  cvtpd2ps  Call MarkSSEdata SSE_2_R
  cvtpi2pd  Call MarkSSEdata SSE_2_D
  cvtpi2ps  Call MarkSSEdata SSE_2_D
  cvtps2dq  Call MarkSSEdata SSE_4_F
  cvtps2pd  Call MarkSSEdata SSE_2_F
  cvtps2pi  Call MarkSSEdata SSE_2_F
  cvtsd2si  Call MarkSSEdata SSE_1_R
  cvtsd2ss  Call MarkSSEdata SSE_1_R
  cvtsi2sd  Call MarkSSEdata SSE_1_D
  cvtsi2ss  Call MarkSSEdata SSE_1_D
  cvtss2sd  Call MarkSSEdata SSE_1_F
  cvtss2si  Call MarkSSEdata SSE_1_F
  cvttpd2dq Call MarkSSEdata SSE_2_R
  cvttpd2pi Call MarkSSEdata SSE_2_R
  cvttps2dq Call MarkSSEdata SSE_4_F
  cvttps2pi Call MarkSSEdata SSE_2_F
  cvttsd2si Call MarkSSEdata SSE_1_R
  cvttss2si Call MarkSSEdata SSE_1_F
  divpd     Call MarkSSEdata SSE_2_R
  divps     Call MarkSSEdata SSE_4_F
  divsd     Call MarkSSEdata SSE_1_R
  divss     Call MarkSSEdata SSE_1_F
  haddpd    Call MarkSSEdata SSE_2_R
  haddps    Call MarkSSEdata SSE_4_F
  hsubpd    Call MarkSSEdata SSE_2_R
  hsubps    Call MarkSSEdata SSE_4_F
  maxpd     Call MarkSSEdata SSE_2_R
  maxsd     Call MarkSSEdata SSE_1_R
  maxss     Call MarkSSEdata SSE_1_F
  minpd     Call MarkSSEdata SSE_2_R
  minps     Call MarkSSEdata SSE_4_F
  minsd     Call MarkSSEdata SSE_1_R
  minss     Call MarkSSEdata SSE_1_F
  movapd    Call MarkSSEdata SSE_2_R
  movaps    Call MarkSSEdata SSE_4_F
  movddup   Call MarkSSEdata SSE_1_Q
  movhpd    Call MarkSSEdata SSE_1_R
  movhps    Call MarkSSEdata SSE_2_F
  movlpd    Call MarkSSEdata SSE_1_R
  movlps    Call MarkSSEdata SSE_2_F
  movshdup  Call MarkSSEdata SSE_4_F
  movsldup  Call MarkSSEdata SSE_4_F
  movntpd   Call MarkSSEdata SSE_2_R
  movntps   Call MarkSSEdata SSE_4_F
  movss     Call MarkSSEdata SSE_1_R
  movupd    Call MarkSSEdata SSE_2_R
  movups    Call MarkSSEdata SSE_4_F
  mulpd     Call MarkSSEdata SSE_2_R
  mulps     Call MarkSSEdata SSE_4_F
  mulsd     Call MarkSSEdata SSE_1_R
  mulss     Call MarkSSEdata SSE_1_F
  orpd      Call MarkSSEdata SSE_2_R
  orps      Call MarkSSEdata SSE_4_F
  rcpps     Call MarkSSEdata SSE_4_F
  rcpss     Call MarkSSEdata SSE_4_F
  rsqrtps   Call MarkSSEdata SSE_4_F
  rsqrtss   Call MarkSSEdata SSE_4_F
  shufpd    Call MarkSSEdata SSE_2_R
  shufps    Call MarkSSEdata SSE_4_F
  sqrtpd    Call MarkSSEdata SSE_2_R
  sqrtps    Call MarkSSEdata SSE_4_F
  sqrtsd    Call MarkSSEdata SSE_2_R
  sqrtss    Call MarkSSEdata SSE_4_F
  subpd     Call MarkSSEdata SSE_2_R
  subps     Call MarkSSEdata SSE_4_F
  subsd     Call MarkSSEdata SSE_2_R
  subss     Call MarkSSEdata SSE_4_F
  ucomisd   Call MarkSSEdata SSE_2_R
  ucomiss   Call MarkSSEdata SSE_4_F
  unpckhpd  Call MarkSSEdata SSE_2_R
  unpckhps  Call MarkSSEdata SSE_4_F
  unpcklpd  Call MarkSSEdata SSE_2_R
  unpcklps  Call MarkSSEdata SSE_4_F
  xorpd     Call MarkSSEdata SSE_2_R
  xorps     Call MarkSSEdata SSE_4_F
;;


____________________________________________________________________________________________
____________________________________________________________________________________________

; The Encode / Decode Dialog (Tool Menu).
____________________________________________________________________________________________
____________________________________________________________________________________________

ViewEncoding:
    Call 'USER32.DialogBoxParamA' D$H.Instance, 28000, &NULL, EncodingProc, &NULL
ret

[ZeroString: D$ ?]

[EncodeHelp: B$ 'Code_Viewer' EOS]

; This is the Source For Encoding (Upper Edite Control);
[EncodeSourceMargin: B$ CR LF CR LF CR LF CR LF CR LF CR LF]
[EncodeSource: B$ "[DATA: 0 0 0 0]
LABEL0:
L0: ", EncodeText: "                                                                  
L1:
Label1:
"]
; This is to ensure that the Assembler do not overwrite the downward Data when ajusting,
; for example, the Asm Source ending CR/LF(s):
[EncodeSecurity: D$ ? # 10]

; This is the second Edit Control showing Text Hexa Code
[HexaCodeText: D$ ? # 10]

; This is the third read only EditBox for Disassembly:
[DummyDecodeText: D$ ? ?]
[DecodeText: D$ ? # 40] ; 40+2 >>> 168

; This is for storing Binary Hexa of Code when only Disassembling:
[DecodeOnlyHexa: D$ ? # 10]

Proc EncodingProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_INITDIALOG

        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$STRUC.WINDOWCLASS@hIcon
        Call 'USER32.GetDlgItem' D@hwnd 13
        Call 'USER32.SetFocus' eax
        jmp L8>>

    ...Else_If D@msg = &WM_COMMAND
        Mov eax D@wParam | and D@wParam 0FFFF | shr eax 16
        ..If eax = &CBN_SELCHANGE

        ..Else_If D@wParam = &IDCANCEL

            Call 'USER32.EndDialog' D@hwnd 0

        ..Else_If D@wParam = &IDOK
            Call 'USER32.GetFocus' | Call 'USER32.GetDlgCtrlID' eax
            On eax = 16, jmp L3>

            Mov eax '    ', edi EncodeText, ecx 15 | rep stosd
            Call 'USER32.GetDlgItemTextA' D@hwnd, 13, EncodeText, 60
            On eax = 0, jmp L3>

            Mov B$EncodeText+eax SPC | Call EncodeDecode

            Call 'USER32.SetDlgItemTextA' D@hwnd, 16, HexaCodeText
            Call 'USER32.SetDlgItemTextA' D@hwnd, 17, DecodeText

        ..Else_If D@wParam = 3
L3:         Call 'USER32.GetDlgItemTextA' D@hwnd, 16, HexaCodeText, 80

            Call DecodeOnly

            Call 'USER32.SetDlgItemTextA' D@hwnd, 13, ZeroString
            Call 'USER32.SetDlgItemTextA' D@hwnd, 17, DecodeText
            Call 'USER32.SetDlgItemTextA' D@hwnd, 16, HexaCodeText


        ..Else_If D@wParam = &IDHELP
            Call Help, B_U_AsmName, EncodeHelp, ContextHlpMessage

        ..End_If

    ...Else_If D@msg = &WM_CTLCOLOREDIT
        jmp L1>

    ...Else_If D@msg = &WM_CTLCOLORLISTBOX
L1:     Call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | Mov eax D$H.DialogsBackGroundBrush | jmp L9>

    ...Else
L8:     popad | Mov eax &FALSE | jmp L9>

    ...End_If

    popad | Mov eax &TRUE

L9: EndP


[WeAreInTheCodeBox: D$ ?]

DecodeOnly:
    Mov B$WeAreInTheCodeBox &TRUE, B$SimpleScan &TRUE
    Mov esi HexaCodeText

    While B$esi > 0
        On B$esi <= SPC, Mov B$esi Space
        inc esi
    End_While
    Mov B$esi Space, D$esi+1 0

    Mov esi HexacodeText, edi DecodeOnlyHexa

    .While B$esi > 0
        Mov ebx 0,  edx 0, ecx 0, eax 0

        While B$esi < '0'
            inc esi | On B$esi = 0, jmp L9>
        End_While
        While B$esi = '0'
            inc esi
            On B$esi <= Space, jmp L8>
            On B$esi = 0, jmp L9>
        End_While

L0:     lodsb | On al > 'Z', and eax (not 32) | cmp al Space | jbe L8>
            sub al '0' | cmp al 9 | jbe L2>
                sub al 7
L2:     shld edx ebx 4 | shl ebx 4 | or bl al
        cmp edx ecx | jb L7>
        Mov ecx edx
        cmp al 0F | jbe L0<

L7:     Call Beep | ret

L8:     Mov eax ebx
        cmp eax 0FF | ja L7<

        stosb
    .End_While
L9:  Mov D$edi 0

  ; Disassemble: (DisMain)
    Mov B$DisFlag 0, D$SegmentOverride 0, B$AddressSizeOverride 0
    Mov B$OperandSizeOverride 0, W$DisSizeMarker 'D$'
    Mov B$DisCodeDisplacement &FALSE, B$EscapePrefix &FALSE
    Mov esi DecodeOnlyHexa, edi DecodeText
L0: movzx eax B$esi | inc esi | Call D$DisOp1+eax*4
; WriteImm32 WeAreInTheCodeBox WriteDisRelative
    On B$DisFlag = DISDONE, jmp L0<
    Mov D$edi 0

  ; Re-write the Hexa Code Text (simply for clean up and striping extra-Bytes):
    Mov edx esi, esi DecodeOnlyHexa, edi HexacodeText
    While esi < edx
        movzx eax B$esi | inc esi
        Mov ebx eax | shr ebx 4
        and eax 0F | and ebx 0F
        Mov al B$STR.A.Hexa+eax, bl B$STR.A.Hexa+ebx
        shl eax 8 | or eax ebx | or eax 020200000 | stosd
    End_While
    Mov D$edi 0

  ; In case of text organisation (sub edi 6, for example), we reset:
    If D$DummyDecodeText+4 <> 0
        Mov eax DecodeText
        While B$eax-1 > 0
            dec eax
        End_While
        Mov ecx DecodeText | sub ecx eax
        Mov esi DecodeText | add esi 160-1 | Mov edi esi | sub esi ecx
        std
L0:         movsb | cmp edi DecodeText | jae L0<
        cld
        Mov D$DummyDecodeText 0, D$DummyDecodeText+4 0
    End_If

    Mov B$WeAreInTheCodeBox &FALSE, B$SimpleScan &FALSE
ret


[LastDisVirtualData: D$ ?]
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
