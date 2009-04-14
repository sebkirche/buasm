TITLE MemView         ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

; Routine for: Menu > Tools > 'Show BUAsm mem'
; 64 est la réserve pour le nombre MAXIMUM d'ASCII affichables (par ligne) dans l'edit
[STR.A.MemView: B$ ? # ((MEM_TABLE_SIZE/MEM_RECORD)*64)]

; ID de l'edit
[IDC_DISPLAYMEMINFO 10]

; Tag Dialog 10000

ViewBUAsmMems:

    ; C'est une thread: Tous les registres sont protégés
    Call 'USER32.DialogBoxParamA' D$H.Instance,
                                  10000,
                                  D$H.MainWindow,
                                  ShowBUAsmMemDialog,
                                  &NULL

ret
_______________________________________________________

Proc ShowBUAsmMemDialog:

    Arguments @hwnd,
              @msg,
              @wParam,
              @lParam

    If D@msg = &WM_INITDIALOG

        Call DisplayBUAsmMemInfos

    Else_If D@msg = &WM_CTLCOLOREDIT

        Call WM_CTLCOLOREDIT | ExitP

    Else_If D@msg = &WM_COMMAND

        On W@wParam = HIDE_PUSHBUTTON_OK Call WM_CLOSE

    Else_If D@msg = &WM_CLOSE

        Call WM_CLOSE

    Else

        Return &FALSE

    End_If

    Mov eax &TRUE

EndP
_______________________________________________________

; !?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!
; MEM_TABLE_SIZE = 10650 dwords = 42600 bytes
; The displacement is from 12 to 12 dwords -> !!! [MEM_RECORD] !!! 3 DWORDS / 16 BYTES
; So we have 887,5 * 4 (10650/12*4). 3550 Lines
; On Our case this means:
; Size*3550 = 54*3550 = 191700 bytes
; Multiplication Factor = 191700/42600 = 4,5. So, for safety we do
; [MemTableView: D$ ? # (MEM_TABLE_SIZE*5)] = 213000 Bytes
; !?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!

DisplayBUAsmMemInfos:

    Call SetIconDialog

    Call 'USER32.SendDlgItemMessageA' D$HWND,
                                      IDC_DISPLAYMEMINFO,
                                      &EM_SETMARGINS,
                                      &EC_LEFTMARGIN+&EC_RIGHTMARGIN,
                                      10

    Mov edi STR.A.MemView,
        ecx ((MEM_TABLE_SIZE/MEM_RECORD)*(64/DWORD)) ; /DWORD -> stosD/B

    xor eax eax | rep stosd

    Mov ebx TABLE.MemBUAsm,
        edi STR.A.MemView

L0:    Mov esi ebx
        ____________________

        ; Affichage N° bloc
        ____________________

        Mov eax esi,
            ecx MEM_RECORD

        sub eax TABLE.MemBUAsm | xor edx edx | div ecx

        Push NA

            Mov ecx 10

L1:         xor edx edx | div ecx | Push edx | Test eax eax NOT_ZERO L1<

            Mov eax "0000"

            Pop edx

        ; '0 ~ 9' ASCII -> Flag NA (0-1)
L2:     rol eax 8| or al dl | Pop edx | Test edx NA POSITIVE L2<

        bswap eax | stosd | Mov al SPC | stosb
        _______________

        ; Adresse bloc
        _______________

        Mov eax D$ebx+MEM_LP | Call WriteEaxHexadecimalMEM
        ______________

        ; Taille bloc
        ______________

        Mov eax D$ebx+MEM_CHUNK_SIZE | Call WriteEaxHexadecimalMEM
        _______________

        ; 4 datas bloc
        _______________

        Mov esi D$ebx+MEM_LP

        Test esi NA NULL S6>

S1:     Mov ecx DWORD

L3:     Push NA

            lodsb

            Mov dl al | and eax 0_F | Mov al B$STR.A.Hexa+eax | Push eax

            Test dl 0F0 | jmpIf ZERO S2>

                Mov al dl | shr eax 4 | Mov al B$STR.A.Hexa+eax | Push eax

                jmp S3>

S2:         Mov al SPC | stosb

S3:         Push '0'

L4:         Pop eax | Comp eax NA = S4>

                stosb

            jmp L4<

S4:         Mov eax SPC | stosb

        sub ecx 1 | JmpIf NOT_ZERO L3<
        __________________

        ; Type allocation
        __________________

        Mov edx D$ebx+MEM_LP

        Comp edx D$ebx+MEM_MOTHER_LP = S5>

            Mov eax "[CHU" | stosd | Mov eax "NK] " | jmp S7>

S5:     Mov eax "[*MA" | stosd | Mov eax "IN*]" | jmp S7>

S6:     Mov eax "[EMP" | stosd | Mov eax "TY] " | stosd | jmp S8>
        _______________

        ; 4 ASCII bloc
        _______________

S7:     stosd | Mov eax "  " | stosw | Call WriteDwordAscii D$edx

S8:     Mov ax CRLF | stosw

        add ebx MEM_RECORD

    Comp ebx D$LP.LastMemRecord < L0<<

    Mov B$edi EOS

    Call 'USER32.SetDlgItemTextA' D$HWND,
                                  IDC_DISPLAYMEMINFO,
                                  STR.A.MemView
ret
________________________________________________________________________________________________________

WriteEaxHexadecimalMEM:

    Mov edx eax,
        ecx 9

    Push NA

L1: Mov eax edx

        shr edx 4 | and eax 0_F | Mov al B$STR.A.Hexa+eax | Push eax | sub ecx 1

    Test edx NA NOT_ZERO L1<

    Mov al SPC | rep stosb | Mov al '0'

L2: stosb | Pop eax | Test eax NA NOT_NEGATIVE L2<

    Mov al SPC | stosb

ret
____________________________________________________________________________________________

Proc WriteDwordAscii:

    Arguments @Value

    Uses esi

    lea esi D@Value

    Mov edx 4,
        ah SPC

L0: lodsb

        Comp al SPC => S1>

            Mov al '–'

S1:     stosw

    sub edx 1 | JmpIf NOT_ZERO L0<

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT

