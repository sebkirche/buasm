TITLE START_CODE     ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

@TopOffCode:
____________________________________________________________________________________________

MSG_PUMP:
    __________________________________________________

    ; Msg pump général en attente de son optimisation
    __________________________________________________

    [STRUC.MSG:  ; En attendant les structures C-Like sur la (pile) stack ;))
     @hwnd: D$ ?
     @message: D$ ?
     @wParam: D$ ?
     @lParam: D$ ?
     @time: D$ ?
     @STRUC.pt: D$ ? ?]

    jmp S2>

L1: Call 'USER32.IsDialogMessageA' D$H.FindReplace,
                                   STRUC.MSG

    Test eax eax NOT_ZERO S2>

    Test D$FL.IsDebugging &TRUE FALSE S1>

        Call 'USER32.TranslateAcceleratorA' D$H.DebugDialog,
                                            D$H.DebbugAccel,
                                            STRUC.MSG

        Test eax eax NOT_ZERO S2>

        Call 'USER32.IsDialogMessageA' D$H.CurrentDataPage,
                                       STRUC.MSG

        Test eax eax NOT_ZERO S2>

        Call 'USER32.IsDialogMessageA' D$H.DebugDialog,
                                       STRUC.MSG

        Test eax eax NOT_ZERO S2>

S1: Call 'USER32.TranslateMessage' STRUC.MSG

    Call 'USER32.DispatchMessageA' STRUC.MSG

S2: Call 'USER32.GetMessageA' STRUC.MSG,
                              &NULL,
                              &WM_NULL,
                              &WM_NULL

    Test eax eax NOT_ZERO L1<<

    Call WriteConfigFile

    Call 'KERNEL32.FreeLibrary' D$H.RichEdit

ret
____________________________________________________________________________________________

;;

                               R O S A S M    S O U R C E
                               B U A S M     S O U R C E

_____________________________________________________________________________________________
_____________________________________________________________________________________________


 RosAsm: The Top Down Assembler for ReactOS PE files.

 This program is free open source.

 First Author: René Tournois.
 
 (i begun to work on SpAsm, the ancestor of RosAsm, in September 1998)

 Actual maintainer: René Tournois.

____________________________________________________________________________________________
____________________________________________________________________________________________

         Copyright (C) 1998, René Tournois

         This program is free software; you can redistribute it and/or modify
         it under the terms of the GNU General Public License as published by
         the Free Software Foundation.

         This program is distributed in the hope that it will be useful,
         but WITHOUT ANY WARRANTY; without even the implied warranty of
         MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
         GNU General Public License included in B_U_Asm.exe for more details.

         You should have received a copy of the GNU General Public License
         along with this program; if not, write to the Free Software
         Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

Rules:

;;
  Developments rules for the volunteers contributing to RosAsm itself


  * In order to give the Source a bit of shape, it is wishable to apply a consistent
    writing Style, where each part would try to follow up, with a couple of rules.
    See an example of the actualy choosen Style with, 'CompactLabelListAndCodeRef':
    
    - Indent must be 4 Spaces.
    
    - The Text of the Multi-Lines Comments are two spaces away from the left margin.
    
    - Mono-Lines Comments start two Spaces before the next (or previous Statement).
      Never: Comment at the end of lines.
      A comment for the next Statement(s) end(s) with a column (":")
      A comment for the previous Statement ends with a "."
      Comments must be "english only".
      Comments start with a Space after the ";".
      
    - Old fashion vertical alignements of the Statements members: NEVER.
      
    - Symbolic Names: Never: "Routine_1", "Buffer", "Variable", and so on. Never,
      ever. The Symbols must be in the form of 'InitLabelListScanDwordPointer'
      (no '_', upper case for each leading char of a component), and should be as
      self-descriptive as possible. English required.
    
    - Equates must be UPPER_CASE. ['_' tolerated inside Equates when wishable for
      readability].
      
    - Separators: Make use of [F8] only. Avoid "---------" and friends.
    
    - There still exist users with a small screen and low resolution. So, the
      Source must not be too wide. 80 Chars is a good base.
      
    - For the same reasons as above, do not use anything like "MMX and above"
    
    - Verticaly, try to make your source optimal with the most proper usage
      of blank lines.
      
    - A multi-Instructions line should "make sense": Try to not group instructions
      which have completely different purposes, and to group the ones which concure
      to a given action, such as you could give it a kind of 'name', if you were
      making it a Routine.


  * Macros:
  
    - Do not implement your own Macros.
    
  
  * At top of your TITLE, we should found, at least:
  
    - The Name of the maintainer (yours).
      
    - The Date of the first release.
      
    - The Data of the actual release.
      
    - An organized list of the TITLE Routines. See an example at the top
      of the TITLE [Optimize], above 'InitShortenJmpsTable'.


  * For exchanging (updates) with the main maintainer:
  
    - For the TITLE you make a [Ctrl][S], and save your "MyTitle.asm". RosAsm is
      full featured for this. Sending a zip of RosAsm.exe is never necessary.
      
    - For Dialogs, make use of Binary Saving Format (Menu).
      
    - The zips of these Files are all that should be necessary for the updates.
      
    - When a new implementation requires to modify something in the main Source
      (branching, typically), this is the job of the main maintainer to do that.
  
  
  * Developments "property":

    At any given time, there can only be _one_ volunteer "touching" a TITLE.
    So, the very first step, before anything else, is about defining a new TITLE
    with the main maintainer. Once done, this TITLE is your "private area", and
    nobody else is allowed to touch it as long as you are the active maintainer.
    
    If two volunteers wish to work upon the same TITLE, the TITLE must be "splitted"
    to create "each one, each private room, each provate task", even if the two TITLEs
    will be merged, later. This simple method is very helpfull for defining "who does
    what".
;;
_____________________________________________________________________________________________
_____________________________________________________________________________________________
; Colors:

[RVBA.DialogsBackgnd: B$ 0_F0 0_F0 0_FF 0_00] ; Fond des dialogues et des diverse RSC

                                       ; Source Editor colors:
[NormalBackColor:  D$ 0_FF_FF_FF       ; white BackGround color (Default: light yellow)
 RVBA.Normal: B$ 0_00 0_00 0_00 0_00   ; Couleur par défaut
 BracketColor: D$ 0A0                  ; for Data / Equates / Macros
 TextColor: D$ 0_64_00                 ; 'green' for text
 CommentColor: D$ 0_82_00_00 ]         ; blue for comments


[DRAWLINELEN 92]    ; This is the number of '_' in a Line drawn by [Ctrl][_]

[UPPERCASEMASK 0DF] ; (not 020) applied on a Byte.

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
_____________________________________________________________________________________________

;;
 Used abreviations:

 EOI,      End Of Instruction
 meEOI,    Macro Expension EOI
 CR,       Carriage Returrn
 LF,       Line Feed
 ALEOD,    Api Lists End Of Data
 RVA,      Relative Virtual Adress
 Len,      Lenght

 Usual Tintel abreviations:

 SIB,      Scale_Base_Index Byte
 Mod/RM,   Mode and Register/Memory Byte
 tttnBits  condition encodage


 MLC       Multi-Lines Comments
;;

_____________________________________________________________________________________________
_____________________________________________________________________________________________

; General purpose macros:

[inc | inc #1 | #+1]    [dec | dec #1 | #+1]

[If | #=3 | cmp #1 #3 | jn#2 I1>]
[Else_if | #=3 | jmp I9> | I1: | cmp #1 #3 | jn#2 I1>]
[Else | Jmp I9> | I1: ]
[End_if | I1: | I9: ]

[.If | #=3 | cmp #1 #3 | jn#2 J1>>]
[.Else_if | #=3 | jmp J9>> | J1: | cmp #1 #3 | jn#2 j1>>]
[.Else | Jmp j9>> | j1: ]
[.End_if | j1: | j9: ]

[..If | #=3 | cmp #1 #3 | jn#2 K1>>]
[..Else_if | #=3 | jmp K9>> | K1: | cmp #1 #3 | jn#2 K1>>]
[..Else | Jmp K9>> | K1: ]
[..End_if | K1: | K9: ]

[...If | #=3 | cmp #1 #3 | jn#2 Z1>>]
[...Else_if | #=3 | jmp Z9>> | Z1: | cmp #1 #3 | jn#2 Z1>>]
[...Else | Jmp Z9>> | Z1: ]
[...End_if | Z1: | Z9: ]

[While | #=3 | W0: | cmp #1 #3 | jn#2 W9>]
[End_While | jmp W0< | W9: ]

[.While | #=3 | X0: | cmp #1 #3 | jn#2 X9>>]
[.End_While | jmp X0<< | X9: ]

[..While | #=3 | Y0: | cmp #1 #3 | jn#2 Y9>>]
[..End_While | jmp Y0<< | Y9: ]

[Do | D0: ]
[Loop_Until | #=3 | cmp #1 #3 | jn#2 D0<]
[Do_Loop | jmp D0<<]

[.Do | E0: ]
[.Loop_Until | #=3 | cmp #1 #3 | jn#2 E0<<]

[Exchange | push #1 | push #2 | pop #1 | pop #2 | #+2]


[Agree | cmp #1 #3 | j#2 A9> | #+3]
[Reject | cmp #1 #3 | j#2 A8> | #+3 | jmp A9> | A8: | ret | A9: ]

_________________________________________________________________________________________
_________________________________________________________________________________________

; Some basic System calls:

[PrintErrorCode | Call 'KERNEL32.GetLastError' | hexprint eax]

 ________________________________________________________________________________________

[HexprintString: '        h', 0]

HexPrn:
    Mov ebx, eax | Mov edi HexPrintString | add edi 7
    std
        Mov ecx, 8
L1:     Mov al bl | and al 0F | On al >= 0A, add al 7
        add al, '0' | stosb | shr ebx, 4 | loop L1
    cld

    Call MessageBox STR.A.ErrorMessageTitle,
                    HexPrintString,
                    &MB_USERICON+&MB_SYSTEMMODAL

ret
[ShowMe | pushad  | Call 'USER32.MessageBoxA' D$H.MainWindow, #1, argh, &MB_SYSTEMMODAL | popad]

[HexPrint | pushad | push #1 | pop eax | Call hexprn | popad | #+1]

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
        move D@NewEnd D@TargetEnd | add D@NewEnd ecx | sub D@NewEnd edx
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

[NewMen: ?]

[Surprise: U$ 048, 06F, 077, 020, 061, 072, 065, 020, 079, 06F, 075,
              020, 04D, 061, 073, 074, 065, 072, 020, 050, 064, 066,
              03F, 0
 SurpriseNchars: D$ 017]

[TABLES_SECURITY 100]

; Tag Unicode Surprise

Proc ExtendTableMemory:
    Argument @Mem, @Pointer, @End
    Uses ecx, ebx, esi, edi

        If B$Dynamic = &FALSE
            Call 'USER32.MessageBoxW' D$H.MainWindow, Surprise, Surprise, &MB_OK
            Call CloseProgressBar
            Mov esp D$OldStackPointer | ret
        End_If

        Mov eax D@Mem, eax D$eax
        push eax
            Mov ecx D@Pointer, ecx D$ecx
          ; Get the actual Size, multiply by 2, and add the security:
            sub ecx eax | shl ecx 1
            push ecx
                add ecx TABLES_SECURITY | VirtualAlloc NewMen ecx
              ; Copy:
                Mov edi eax
                Mov esi D@Mem, esi D$esi
                Mov ecx D@End, ecx D$ecx | sub ecx esi | Align_On 01000, ecx
                shr ecx 2 | rep movsd
            pop ecx
          ; Save the new Mem Limit:
            add ecx D$NewMen | Mov eax D@End, D$eax ecx
          ; Adjust and Save the new follow-up Pointer:
            Mov ebx D@Mem, ebx D$ebx
            Mov eax D@Pointer, eax D$eax
            sub eax ebx | add eax D$NewMen
            Mov ebx D@Pointer, D$ebx eax
          ; Save the new Mem:
            Mov eax D$NewMen | Mov ebx D@Mem, D$ebx eax
        pop eax
        VirtualFree eax
Endp
____________________________________________________________________________________________
____________________________________________________________________________________________

; Enough for small Tables:

Proc BubbleSort:
    Arguments @Array, @Size ; In Bytes!
    Uses eax, ebx, ecx, edi

        Mov edi D@Array, ecx D@Size | shr ecx 2 | jecxz L9>

L0:     lea ebx D$edi+ecx*4 | Mov eax D$edi

L1:     sub ebx 4 | cmp eax D$ebx | jle L2>
            xchg eax D$ebx

L2:         cmp ebx edi | jne L1<

        stosd | loop L0<
L9: EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc zStringsSort:
    Argument @Source, @Destination, @Number

        Mov ecx D@Number, edi D@Destination

L0:     push ecx
            Mov esi D@Source, ecx D@Number, edx 0, bl 0FF

L1:         lodsb

            .If al = 0FF
                ; nop
            .Else_If al < bl
                Mov bl al | lea edx D$esi-1
            .Else_If al = bl
                push ebx
                    push edx, esi
                        While al = bl
                            lodsb | inc edx | Mov bl B$edx
                            cmp al 0 | je L2>
                        End_While
L2:                 pop esi, edx
                    On al < bl, lea edx D$esi-1
                pop ebx
            .End_If

            While B$esi <> 0 | inc esi | End_While | inc esi | loop L1<

            If edx > 0
                Mov esi edx
                While B$esi <> 0
                    movsb | Mov B$esi-1 0FF
                End_While

                Mov B$edi 0 | inc edi
            End_If

        pop ecx | dec ecx | cmp ecx 0 | ja L0<<
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

WaitForUserAction:
L0: Call 'USER32.PeekMessageA' FirstMSG, D$H.MainWindow, 0, 0FFFF, &PM_REMOVE
    cmp D$FuMsg &WM_LBUTTONUP | je L9>
    cmp D$FuMsg &WM_RBUTTONUP | je L9>
    cmp D$FuMsg &WM_KEYDOWN | jne L0<
L9: ret

____________________________________________________________________________________________
____________________________________________________________________________________________

[RosAsmFilesPath: ? #&MaxPath]

GetRosAsmFilesPath:
    push esi, edi
        Mov esi EquatesName, edi RosAsmFilesPath
        While B$esi <> 0 | movsb | End_While
        While B$edi <> '\' | dec edi | End_While
        Mov B$edi+1 0
    pop edi, esi
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; Trash Buffers:

[TrashPointer: ?]
[TrashString: ? #80]
[Trash: Trash1: ? #10000] [Trash2: ? #10000] [Trash3: ? #10000]

ClearTrashTables:
    push edi, ecx, eax
        Mov edi TrashString, ecx 20080, eax 0 | rep stosd
    pop eax, ecx, edi
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

[GetHexaFromTextError: ?]

Proc GetHexaFromText:
    Argument @Buffer
    Uses esi

        Mov esi D@Buffer, edx 0, B$GetHexaFromTextError &FALSE

        While B$esi > 0
            lodsb | On al > 'Z', sub al 020
            sub al '0' | On al > 9, sub al 7

            If al > 0F

                Call MessageBox D$STR.A.MessageWindowTitleError,
                                {B$ "The Number should be HexaDecimal" EOS},
                                &MB_USERICON+&MB_SYSTEMMODAL

                Mov B$GetHexaFromTextError &TRUE | ExitP

            End_If
            shl edx 4 | or dl al
        End_While

        Mov eax edx
EndP


[zCopy | Mov esi #1 | While B$esi <> 0 | movsb | End_While | #+1]

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc ClearBuffer: ; (Guga: Instead of ClearCharMapData)
    Arguments @Buffer, @Size
    Uses edi, ecx

        Mov edi D@Buffer, ecx D@Size, eax 0

        rep stosb
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

[RGB | (#1 or (#2 shl 8) or (#3 shl 16))]

____________________________________________________________________________________________
____________________________________________________________________________________________

; Computes the Length of a zero-ended Ascii String. Regs under caller responsability:

StrLen: ; edi -> String // StrLenProc
    Mov ecx 0-1, al 0
    repne scasb
    Mov eax 0-2 | sub eax ecx      ; Lenght in eax.
ret
____________________________________________________________________________________________________________

; Same with regs under Proc responsability:

Proc StrLenProc:
    Arguments @Pointer
    Uses edi, ecx

        Mov edi D@Pointer, ecx 0-1, al 0
        repne scasb
        Mov eax 0-2 | sub eax ecx      ; Lenght in eax
EndP








