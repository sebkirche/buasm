TITLE START_BUASM     ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

; Colors:
[ARVB.DialogsBackColor: B$ 0_F0 0_F0 0_FF 0_00] ; For Edit Controls, List Boxes (Default: Light Blue)

                                          ; Source Editor colors:
[ARVB.BackColor: B$ 0_FF 0_FF 0_FF 0_00   ; white BackGround color
 StatementColor: D$ 0_00_00_00            ; black for instructions
 BracketColor: D$ 0_A0                    ; [red] for Data / Equates / Macros
 TextColor: D$ 0_64_00                    ; 'green' for text
 CommentColor: D$ 0_82_00_00 ]            ; blue for comments


[DRAWLINELEN 92]    ; This is the number of '_' in a Line drawn by [Ctrl][_]

[UPPERCASEMASK 0_DF] ; (not 020) applied on a Byte.

;;
 To modify the default font used by the [Print] feature, Right Click on >>>  cbbuffer  <<<
 and do what you can there. Be aware that specifying a font for a Printer under Win32
 is absolutely crazy; i won't help you... Welcome to the one who could rewrite this
 in RosAsm.
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

; Displacements inside a 'SectionsHeaders': ; 'PeHeader'

[SECTION_NAME 0
 SECTION_RVASIZE 8
 SECTION_RVA 12
 SECTION_FILESIZE 16
 SECTION_FILEPOINTER 20

 SECTION_RELOC_PTR 24
 SECTION_LINES_NUMBER_PTR 28
 SECTION_RELOC_NUMBER 32
 SECTION_NUMBER_OF_LINES_NUMBER 34

 SECTION_FLAG 36

 SECTIONHEADERSIZE 40]
_____________________________________________________________________________________________

;;
 
    Used abreviations:

    EOI      End Of Instruction
    meEOI    Macro Expension EOI
    CR       Carriage Returrn
    LF       Line Feed
    ALEOD    Api Lists End Of Data
    RVA      Relative Virtual Adress
    Len      Lenght

    Usual Tintel abreviations:

    SIB      Scale_Base_Index Byte
    Mod/RM   Mode and Register/Memory Byte
    tttnBits condition encodage

    MLC      Multi-Lines Comments

;;
_____________________________________________________________________________________________

; General purpose macros:

[inc | inc #1 | #+1]    [dec | dec #1 | #+1]

[If | #=3 | cmp #1 #3 | jn#2 I1>]
[Else_If | #=3 | jmp I9> | I1: | cmp #1 #3 | jn#2 I1>]
[Else | Jmp I9> | I1:]
[End_If | I1: | I9:]

[.If | #=3 | cmp #1 #3 | jn#2 J1>>]
[.Else_If | #=3 | jmp J9>> | J1: | cmp #1 #3 | jn#2 j1>>]
[.Else | Jmp j9>> | j1:]
[.End_If | j1: | j9:]

[..If | #=3 | cmp #1 #3 | jn#2 K1>>]
[..Else_If | #=3 | jmp K9>> | K1: | cmp #1 #3 | jn#2 K1>>]
[..Else | Jmp K9>> | K1:]
[..End_If | K1: | K9:]

[...If | #=3 | cmp #1 #3 | jn#2 Z1>>]
[...Else_If | #=3 | jmp Z9>> | Z1: | cmp #1 #3 | jn#2 Z1>>]
[...Else | Jmp Z9>> | Z1:]
[...End_If | Z1: | Z9:]

[.While | #=3 | X0: | cmp #1 #3 | jn#2 X9>>]
[.End_While | jmp X0<< | X9:]

[..While | #=3 | Y0: | cmp #1 #3 | jn#2 Y9>>]
[..End_While | jmp Y0<< | Y9:]

[Do | D0:]
[Loop_Until | #=3 | cmp #1 #3 | jn#2 D0<]
[Do_Loop | jmp D0<<]

[.Do | E0:]
[.Loop_Until | #=3 | cmp #1 #3 | jn#2 E0<<]

[Exchange | push #1 | push #2 | pop #1 | pop #2 | #+2]

[Agree | cmp #1 #3 | j#2 A9> | #+3]
[Reject | cmp #1 #3 | j#2 A8> | #+3 | jmp A9> | A8: | ret | A9:]

_________________________________________________________________________________________
_________________________________________________________________________________________
; Proc Macros and Equates. Internal storages are:
;
; &1 <<< Size of Argument(s) (for ending Ret n, in EndP). Set by Argument(s)
; &2 <<< Size of Local (for Stack Management). Set by Local
; &3 <<< What to pop before ret. Set by Uses.

[Proc | &1=0 | &2=0 | &3= | #1 | push ebp | mov ebp esp]

[ExitP | jmp P9>]

[Arguments | {#1 Arg#x} | #+1 | &1=SizeOf#x]
[Argument  | {#1 Arg#x} | #+1 | &1=SizeOf#x]

[Local | {#1 Local#x} | #+1 | sub esp SizeOf#x | &2=SizeOf#x]

[StrucPtrs | {#3 ebp+#2+#F} | #+2]

[Structure | {#1 ebp-&2-4} | sub esp #2+4 | mov D$#1 esp | StrucPtrs 0-&2-#2-4 #L>3]

[Uses | push #1>L | &3=pop #L>1]

[EndP | P9: | &3 | mov esp ebp | pop ebp | ret &1]

; For pointing to transmitted parameters (upper "Arg#x" fall here):

[Arg1 ebp+8    Arg2 ebp+12    Arg3 ebp+16    Arg4 ebp+20   Arg5 ebp+24
 Arg6 ebp+28   Arg7 ebp+32    Arg8 ebp+36    Arg9 ebp+40   Arg10 ebp+44]

; For pointing Local Stack declared data (upper "Local#x" fall here):

[Local1 ebp-4     Local2 ebp-8     Local3 ebp-12    Local4 ebp-16    Local5 ebp-20
 Local6 ebp-24    Local7 ebp-28    Local8 ebp-32    Local9 ebp-36    Local10 ebp-40]

; To help preventing from stack sizes' mistakes (upper "SizeOf#x" fall here):

[SizeOf1 4     SizeOf2 8     SizeOf3 12    SizeOf4 16    SizeOf5 20
 SizeOf6 24    SizeOf7 28    SizeOf8 32    SizeOf9 36    SizeOf10 40]

____________________________________________________________________________________________
____________________________________________________________________________________________

; Some basic System calls:

[HIDE_PUSHBUTTON_OK 1]

SetIconDialog:
    ______________________________________________

    ; Attribue l'icône de BUAsm au dialog courant
    ______________________________________________

    Call 'USER32.SetClassLongA' D$HWND,
                                &GCL_HICON,
                                D$STRUC.WINDOWCLASS@hIcon

ret

WM_CTLCOLOREDIT:
    __________________________________________________

    ; Attribue RVB fond utilisateur au dialog courant
    __________________________________________________

    Call 'USER32.SendMessageA' D$LPARAM,
                               &EM_SETSEL,
                               0-1,
                               0

    Call 'GDI32.SetBkColor' D$WPARAM,
                            D$ARVB.DialogsBackColor

    ; Eax doit être rtourné à l'OS dans le dialog
    Mov eax D$H.DialogsBackGroundBrush

ret


EndDialog:
    ______________________________

    ; Fermeture du dialog courant
    ______________________________


    Call 'USER32.EndDialog' D$HWND,
                            0

ret

Proc MessageBox:

    Argument @STR.A

    pushad

        Call 'USER32.MessageBoxA' &NULL,
                                  D@STR.A,
                                  &NULL,
                                  &MB_SYSTEMMODAL

    popad

EndP


Proc HexPrint:

    Argument @DWORD

    [@STR.A.HexprintString: B$ '         Hex' EOS]

    pushad

        Mov eax D@DWORD,
            edx eax,
            ecx 7

L0:     Mov al dl

            and eax 0F

            Mov al B$STR.A.Hexa+eax,
                B@STR.A.HexprintString+ecx al

            shr edx 4

        sub ecx 1 | jns L0<

        Call 'USER32.MessageBoxA' &NULL,
                                  @STR.A.HexprintString,
                                  &NULL,
                                  &MB_SYSTEMMODAL+&MB_ICONINFORMATION

    popad

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; Replaces a Chunk in an existing Data set, by another Chunk.

Proc ChunkReplace:

    Argument @TargetA,          ; Original Buffer insertion start Point (first Byte)
             @TargetB,          ; Original Buffer insertion End Point (after last Byte)
             @TargetEnd,        ; Original Buffer end (Byte after last valid content)
             @TargetMemEnd,     ; Original Buffer Memory limit (after last Byte)
             @SourceA,          ; Substitute Start Point (first Byte)
             @SourceB           ; Substitute End Point (after last Byte)
    Local @NewEnd

    Uses esi, edi, ecx, edx

        Mov ecx D@SourceB | sub ecx D@SourceA
        Mov edx D@TargetB | sub edx D@TargetA
        Move D@NewEnd D@TargetEnd | add D@NewEnd ecx | sub D@NewEnd edx
        .If ecx > edx
          ; If the new Chunk is bigger than the old one:
            sub ecx edx
            Mov edi D@TargetEnd | dec edi | Mov esi edi | add edi ecx

            If edi >= D@TargetMemEnd
                Mov eax &FALSE | ExitP
            End_If

            Mov ecx D@TargetEnd | sub ecx D@TargetB | inc ecx | std | rep movsb | cld

        .Else_If ecx < edx
          ; If the new Chunk is smaller than the old one:
            xchg ecx edx | sub ecx edx
            Mov edi D@TargetB, esi edi | sub edi ecx

            Mov ecx D@TargetEnd | sub ecx D@TargetB | rep movsb
        .End_If

      ; Now, Copy the Chunk:
        Mov esi D@SourceA, edi D@TargetA
        Mov ecx D@SourceB | sub ecx D@SourceA | jecxz L9>
            Mov edx ecx | shr ecx 2 | rep movsd
            Mov ecx edx | and ecx 00_11 | jecxz L9>
                rep movsb

L9:     Mov eax D@NewEnd, B$eax 0 | Mov eax &TRUE
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[LP.NewMem: D$ ?]

[TABLES_SECURITY 100]

Proc ExtendTableMemory:

    Argument @Mem,
             @Pointer,
             @End

    pushad

        Test D$FL.Dynamic &TRUE TRUE S1>

            Call 'USER32.MessageBoxA' &NULL,
                                      &NULL,
                                      {B$ 'Petit problème...' EOS},
                                      &MB_OK

            Call CloseProgressBar

            Mov esp D$OldStackPointer
    popad

EndP


S1:     Mov eax D@Mem,
            eax D$eax

        Push eax

            Mov ecx D@Pointer, ecx D$ecx

            ; Get the actual Size, multiply by 2, and add the security:
            sub ecx eax | shl ecx 1

            Push ecx

                add ecx TABLES_SECURITY | Call VirtualAlloc LP.NewMem,
                                                            ecx
                ; Copy:
                Mov edi D$LP.NewMem
                Mov esi D@Mem, esi D$esi
                Mov ecx D@End, ecx D$ecx | sub ecx esi | AlignOn PAGE_SIZE ecx

                shr ecx 2 | rep movsd

            Pop ecx

            ; Save the new Mem Limit:
            add ecx D$LP.NewMem | Mov eax D@End, D$eax ecx

            ; Adjust and Save the new follow-up Pointer:
            Mov ebx D@Mem, ebx D$ebx
            Mov eax D@Pointer, eax D$eax
            sub eax ebx | add eax D$LP.NewMem
            Mov ebx D@Pointer, D$ebx eax

            ; Save the new Mem:
            Mov eax D$LP.NewMem | Mov ebx D@Mem, D$ebx eax

        Pop eax

        Call VirtualFree eax

    popad

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; Enough for small Tables:

Proc BubbleSort:

    Arguments @Array,
              @Size ; In bytes

    Uses eax,
         ecx,
         edx,
         edi

    Mov edi D@Array, ecx D@Size | shr ecx 2 | jecxz P9> ; TODO ExitP

L1: lea edx D$edi+ecx*DWORD

        Mov eax D$edi

L2:     sub edx DWORD | Comp eax D$edx s<= S1>

            xchg eax D$edx

S1:     Comp edx edi <> L2<

    stosd | loop L1<

EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc zStringsSort:
    Argument @Source, @Destination, @Number

        Mov ecx D@Number, edi D@Destination

L0:     Push ecx
            Mov esi D@Source, ecx D@Number, edx 0, bl 0FF

L1:         lodsb

            .If al = 0FF
                ; nop
            .Else_If al < bl
                Mov bl al | lea edx D$esi-1
            .Else_If al = bl
                Push ebx
                    Push edx, esi
                        While al = bl
                            lodsb | inc edx | Mov bl B$edx
                            cmp al 0 | je L2>
                        End_While
L2:                 Pop esi, edx
                    On al < bl, lea edx D$esi-1
                Pop ebx
            .End_If

            While B$esi <> 0 | inc esi | End_While | inc esi | loop L1<

            If edx > 0
                Mov esi edx
                While B$esi <> 0
                    movsb | Mov B$esi-1 0FF
                End_While

                Mov B$edi 0 | inc edi
            End_If

        Pop ecx | dec ecx | cmp ecx 0 | ja L0<<
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

[BUAsmFilesPath: B$ ? # &MAX_PATH]

GetBUAsmFilesPath:

    Push esi,
         edi

        Mov esi EquatesName, edi BUAsmFilesPath

        While B$esi <> EOS | movsb | End_While

        While B$edi <> '\' | sub edi ASCII | End_While

        Mov B$edi+1 EOS

    Pop edi,
        esi

ret
____________________________________________________________________________________________

; Trash Buffers:


[LP.MemTrash: D$ ?]

[LP.Trash: D$ ?
 STR.A.Trash: B$ ? # &MAX_PATH
 Trash1: B$ ? # 40_000
 Trash2: B$ ? # 40_000
 Trash3: B$ ? # 40_000]

ClearTrashTables:

    Push edi,
         ecx,
         eax

        Mov edi STR.A.Trash,
            ecx ((&MAX_PATH+(3*40_000))/DWORD)

        xor eax eax | rep stosd

    Pop eax,
        ecx,
        edi

ret
____________________________________________________________________________________________

[GetHexaFromTextError: D$ ?]

Proc GetHexaFromText:

    Argument @Buffer

    Uses esi

        Mov esi D@Buffer, edx 0, B$GetHexaFromTextError &FALSE

        While B$esi > 0

            lodsb | On al > 'Z', sub al SPC

            sub al '0' | On al > 9, sub al 7

            If al > 0F

                Call MessageBox {B$ 'The Number should be HexaDecimal' EOS}

                Mov B$GetHexaFromTextError &TRUE

EndP

            End_If

            shl edx 4 | or dl al

        End_While

        Mov eax edx

EndP
____________________________________________________________________________________________

Proc ClearDwordBuffer:

    Arguments @Buffer,
              @Size

    Uses eax,
         ecx,
         edi

    Mov edi D@Buffer,
        ecx D@Size,
        eax 0

    rep stosd

EndP
____________________________________________________________________________________________

; Computes the Length of a zero-ended Ascii String. Regs under caller responsability:

StrLen: ; edi -> String // StrLenProc

    Mov ecx 0-1,
        al 0

    repne scasb

    Mov eax 0-2 | sub eax ecx      ; Lenght in eax.

ret
____________________________________________________________________________________________________________

; Same with regs under Proc responsability:

Proc StrLenProc:

    Arguments @Pointer

    Uses edi,
         ecx

    Mov edi D@Pointer,
        ecx NA,
        al 0

    repne scasb

    Mov eax 0-2 | sub eax ecx      ; Lenght in eax

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
