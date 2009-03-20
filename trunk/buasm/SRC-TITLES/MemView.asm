TITLE MemView         ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

; Routine for: Menu > Tools > 'Show BUAsm mem'

[TABLE.MemView: D$ ? # (MEM_TABLE_SIZE*5)]

; Tag Dialog 10000
[IDD_SHOW_BUASM_MEM 10000
 IDC_DISPLAYMEMINFO    10]

ViewBUAsmMems:

    pushad

        Call 'USER32.DialogBoxParamA' D$H.Instance,
                                      IDD_SHOW_BUASM_MEM,
                                      &NULL,
                                      ShowBUAsmMemDialog,
                                      &NULL

    popad

ret
_______________________________________________________

Proc ShowBUAsmMemDialog:

    Arguments @hwnd,
              @msg,
              @wParam,
              @lParam

    Comp D@msg &WM_CLOSE <> S1>

    ;    Call ClearDwordBuffer TABLE.MemView,
    ;                          (MEM_TABLE_SIZE*5)

        Call 'USER32.EndDialog' D@hwnd,
                                &NULL

EndP

S1: Comp D@msg &WM_INITDIALOG = S1>

S2: Mov eax &FALSE

EndP

S1: Call DisplayBUAsmMemInfos

    Mov eax &TRUE

EndP
_______________________________________________________

; Calculate the maximum size of the report....
;0000000: 023 069 066 06E  - Ascii: # i f n
;0403ED4: 073 079 073 02F  - Ascii: s y s /  Size = 43
;0403ED4: 073 079 073 02F  - Not used - Ascii: s y s /  Size = 54
;0403ED4: 073 079 073 02F  - Commited - Ascii: s y s /  Size = 54
;0403ED4: 073 079 073 02F  - Reserved - Ascii: s y s /  Size = 54
; MEM_TABLE_SIZE = 10650 dwords = 42600 bytes. The displacement is from 12 to 12 dwords
; So we have 887,5 * 4 (10650/12*4). 3550 Lines.
; On Our case this means:
; Size*3550 = 54*3550 = 191700 bytes
; Multiplication Factor = 191700/42600 = 4,5. So, for safety we do
; [MemTableView: D$ ? # (MEM_TABLE_SIZE*5)] = 213000 Bytes

DisplayBUAsmMemInfos:

    Push ebx,
         esi,
         edi

    Mov esi TABLE.MemBUAsm,
        edi TABLE.MemView

    .While esi <= D$LastMemRecord

       push esi

            mov eax esi, ecx MEM_RECORD | sub eax TABLE.MemBUAsm | xor edx edx | div ecx

            Push NA

            Mov ecx 10

L1:         xor edx edx

                div ecx | Push edx

            Test eax eax NOT_ZERO L1<

            Pop eax

            Mov edx '0000'

            ; '0 ~ 9' ASCII -> Flag NA (0-1)
L2:         or dl al

                ror edx 8 | Pop eax

            Test eax NA POSITIVE L2<

            Mov eax edx | stosd | Mov al SPC | stosb

        Pop esi

        Push esi

            ; Adresse bloc en hexa
            mov eax D$esi+MEM_LP | Call WriteEax

            Mov al SPC | stosb

            ; Taille bloc en hexa
            mov eax D$esi+MEM_CHUNK_SIZE | Call WriteEax

            ; 4 premiers bytes dans le bloc
            Mov ecx 4

L3:         Push NA

                lodsb

                Mov dl al | and eax 0_F | Mov al B$STR.A.Hexa+eax | Push eax

                Test dl 0F0 | jmpIf ZERO S1>

                    Mov al dl | shr eax 4 | Mov al B$STR.A.Hexa+eax | Push eax

                jmp S2>

S1:             Mov al SPC | stosb

S2:             Push '0'

L4:             Pop eax | Comp eax NA = S3>

                    stosb

                jmp L4<

S3:             Mov eax SPC | stosb

            sub ecx 1 | JmpIf NOT_ZERO L3<

        pop esi

        Push esi

            mov edx D$esi+MEM_LP

            Test edx NA ZERO S5>

            Comp edx D$esi+MEM_MOTHER_LP = S4>

                Mov eax " CHI" | stosd | Mov eax "LD: " | stosd | Call WriteDwordAscii D$edx

            jmp S6>

S4:             Mov eax " MAI" | stosd | Mov eax "N:  " | stosd | Call WriteDwordAscii D$edx

            jmp S6>

S5:             Mov eax " NA " | stosd

S6:         Mov ax CRLF | stosw

        Pop esi

        add esi MEM_RECORD

    .End_While

    Mov B$edi EOS

    Call 'USER32.SetDlgItemTextA' D$HWND,
                                  IDC_DISPLAYMEMINFO,
                                  TABLE.MemView

    Pop edi,
        esi,
        ebx

ret
________________________________________________________________________________________________________

Proc WriteDwordAscii:

    Arguments @Value

    Uses esi

    lea esi D@Value

    Mov edx 4

L0: lodsb

        Comp al SPC => S1>

            Mov al '.'

S1:     Mov ah SPC

        stosw

    sub edx 1 | JmpIf NOT_ZERO L0<

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT

