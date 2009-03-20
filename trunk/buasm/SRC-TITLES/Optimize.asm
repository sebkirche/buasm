TITLE Optimize        ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;;
  Release 2: Betov November 2006.
  _______________________________
    SetShortenJmpFlag
    
    SetJMPShortenJmpFlag
  
    JmpsOptimize
        ScanShortenJmpsTable
            InitScanShortenJmpsTable
            TryToShortenLong
                ShortenLongDown
                ShortenLongUp
        CompactLabelListAndCodeRef
            InitLabelListScanDwordPointer
            AdjustAboveListPointers
            AdjustLastListPointers
        CompactCodeList
        CompactShortenJmpsTable
       
        ScanCouples
            InitScanShortenJmpsTable
            IsJumpDownOrUp
            CouldBothBeShorten
                GetDisplacementDown
                GetDisplacementUp
            SetBothShort
  _______________________________
  
  Process:
  
  * A 'ShortenJmpsTable' Map is created by a cal from 'AsmMain': Empty Table,
    representing the Code area.
  
  * Jcc Encodage (above 'JMPmnemo') calls for 'SetShortenJmpFlag': 'LONG_Jcc' at the
    matching Byte of the 'ShortenJmpsTable'.
     
    JMP Encodage ('JMPmnemo') calls for 'SetJMPShortenJmpFlag': 'LONG_JMP' at the
    matching Byte of the 'ShortenJmpsTable'.
     
    Case of Plain Labels are not considered: They are always Long in RosAsm Syntax
    and the Jumps Sizes apply to Local Labels only.
  
  * After the Encodage, a "Long to Short" Optimization is computed, here.
;;
____________________________________________________________________________________________

[ShortenJumpsWanted: D$ &FALSE]

[ShortenJmpsTable: D$ ?
 CodeListOrigine: D$ ?]

InitShortenJmpsTable:
    On D$ShortenJmpsTable <> 0, Call ReleaseShortenJmpsTable

    Call VirtualAlloc ShortenJmpsTable,
                      D$SourceLen

    Move D$CodeListOrigine D$CodeListPtr
ret


ReleaseShortenJmpsTable:

    Call VirtualFree ShortenJmpsTable

ret
____________________________________________________________________________________________

;;
  Called from 'Encode' // 'Letter_J' // Cases of Jcc and JMP (DownLong or UpLong only):
  
  The Plain Labels are not considered (always long in RosAsm Syntax).
;;

[EndOfShortenJmpsTable: D$ ?]

[LONG_JMP 1, LONG_Jcc 2
 SHORTEN 3
 SHORTEN_LONG_JMP 4, SHORTEN_LONG_Jcc 5
 ALIGN_FLAG 010]

SetShortenJmpFlag:
    Push edi
        sub edi D$CodeListOrigine | add edi D$ShortenJmpsTable | add edi 2
        Mov B$edi LONG_Jcc, D$EndOfShortenJmpsTable edi
    Pop edi
    inc D$NumberOfLongToShort
ret


SetJMPShortenJmpFlag:
    Push edi
        sub edi D$CodeListOrigine | add edi D$ShortenJmpsTable | add edi 1
        Mov B$edi LONG_JMP, D$EndOfShortenJmpsTable edi
    Pop edi
    inc D$NumberOfLongToShort
ret


[AlignFound: D$ ?]

SetAlignFlag:
    Mov B$AlignFound &TRUE
ret
    Push edi
        sub edi D$CodeListOrigine | add edi D$ShortenJmpsTable
        Mov B$edi ALIGN_FLAG, D$EndOfShortenJmpsTable edi

        Mov ecx D$imm32
    Pop edi
ret
____________________________________________________________________________________________

[CodeRefScan: D$ ?
 NumberOfLongToShort: D$ ?
 JumpSizeStat: D$ ?]

;;
  On RosAsm itself, the Optimization adds 7% to the Compile time, and resolves
  7,619 Long to Short. The main "While" is run 4 times.
;;

JmpsOptimize:
    Mov eax D$CodeListPtr | dec eax

    Call ScanShortenJmpsTable
    Mov eax D$NumberOfLongToShort, D$JumpSizeStat eax

    While D$NumberOfLongToShort <> 0
L1:     Call CompactLabelListAndCodeRef

        Call CompactCodeList

        Call CompactShortenJmpsTable

        Call ScanShortenJmpsTable
        Mov eax D$NumberOfLongToShort | add D$JumpSizeStat eax
    End_While

    Call ScanCouples
    Mov eax D$NumberOfLongToShort | add D$JumpSizeStat eax
    On D$NumberOfLongToShort <> 0, jmp L1<
ret
____________________________________________________________________________________________

[LongInstruction: D$ ?] ; will be either 'LONG_JMP' or 'LONG_Jcc'

InitScanShortenJmpsTable:
    Mov ebx D$CodeRef | add ebx 5

  ; Case of Api Calls: ...|0FF dWordCodeAddress|:
  ;                   ....|.....|
    While B$ebx = 0FF | add ebx 6 | End_While

    Mov D$CodeRefName ebx

    While B$ebx > EOI | inc ebx | End_While | inc ebx

    Mov D$CodeRefScan ebx
  ; 'CodeRefScan' points to the first Char of a LabelName, in 'CodeRef'.

    Mov D$NumberOfLongToShort 0

    Call InitLabelListScanDwordPointer

    Mov esi D$ShortenJmpsTable
ret
____________________________________________________________________________________________

[CodeRefName: D$ ?
 CodeRefName1: D$ ?
 CodeRefName2: D$ ?]

ScanShortenJmpsTable:
;;
  Job: The 'ShortenJmpsTable' has been filled with 'LONG_JMP' or 'LONG_Jcc' at each
  location of a relative jump address (parallel to 'CodeListOrigine'). If JUMPs can
  be shorten, it turns them into 'SHORTEN_LONG_JMP' or 'SHORTEN_LONG_Jcc'.
  
  Calls for: 'TryToShortenLong' >>> 'ShortenLongDown' and 'ShortenLongUp'
;;

    Call InitScanShortenJmpsTable

  ; Scanning 'ShortenJmpsTable' 0 and 1 (esi ---> 'ShortenJmpsTable'):
L0: Mov edx D$EndOfShortenJmpsTable

    While B$esi = 0
        inc esi | cmp esi edx | ja L9>>
    End_While

  ; Here we have a Long Displacement that will be computed in 'FillCodeSymbols'.
    Mov al B$esi, B$LongInstruction al ; Either 'LONG_JMP' or 'LONG_Jcc'

  ; Translate into a 'CodeList' Pointer:
    Mov eax esi | sub eax D$ShortenJmpsTable | add eax D$CodeListOrigine

  ; Scan CodeRef:

  ; >>> Two Pointers (One in CodeList -eax-, and one in CodeRef -ecx-):
L1: Mov ecx D$ebx | and ecx (not relativeFlag)

    ..If ecx = eax
      ; Found matching Record, in CodeRef. Take a copy of the Name in 'CodeRefName':
        Mov eax ebx | sub eax 5 | Move D$CodeRefName D$eax

            Call TryToShortenLong

            .If B$CanBeShorten = &TRUE
                If B$LongInstruction = LONG_JMP
                    Mov B$esi SHORTEN_LONG_JMP
                Else_If B$LongInstruction = LONG_Jcc
                    Mov B$esi SHORTEN_LONG_Jcc
                End_If

              ; For the lasting 'FillCodeSymbols' to know it is a special "Short":
                Mov B$ebx-2 '.'
            .End_If

          ; Next CodeRef Record:
            add ebx 9 | inc esi
          ; Skip the Api calls:
            While B$ebx = 0FF | add ebx 6 | End_While
          ; Skip the "labelName|":
            While B$ebx > EOI | inc ebx | End_While | inc ebx

            jmp L0<<

        ..Else_If ecx <> 0
          ; Next CodeRef Record::
            add ebx 9
          ; Skip the Api calls:
            While B$ebx = 0FF | add ebx 6 | End_While
          ; Skip the "labelName|":
            While B$ebx > EOI | inc ebx | End_While | inc ebx

            jmp L1<<

        ..End_If
L9: ret
____________________________________________________________________________________________

[FirstDownCodeRefScan: D$ ?
 SecondUpCodeRefScan: D$ ?]

[FirstDownShortenJmp: D$ ?
 SecondUpShortenJmp: D$ ?]

ScanCouples:
    Call InitScanShortenJmpsTable
  ; >>> ebx ---> 'CodeRefScan' ---> 'CodeRef'.
  ; >>> esi ---> 'ShortenJmpsTable'.
    Mov edx D$EndOfShortenJmpsTable

    .While esi <= edx
        ...If B$esi <> 0
            Push esi, edx
            Call IsJumpDownOrUp

            ..If B$JumpIs = DOWN
                Mov D$CodeRefScan eax
                Mov D$FirstDownCodeRefScan eax, D$FirstDownShortenJmp esi
                Move W$CodeRefName1 W$eax-5
                Mov ebx esi | add esi 4
                Push esi, edx
                    Mov eax esi | add eax 133-4 | On eax < edx, Mov edx eax

                    While esi <= edx
                        .If B$esi <> 0
                            Call IsJumpDownOrUp ; >>> eax ---> 'CodeRefScan'

                            If B$JumpIs = UP
                                Mov D$SecondUpCodeRefScan eax
                                Move W$CodeRefName2 W$eax-5
                                Mov D$SecondUpShortenJmp esi
                                Call CouldBothBeShorten
                                On eax = &FALSE, jmp L2>
                                Call SetBothShort | jmp L4>
                            End_If

                        .End_If

L2:                     inc esi
                    End_While
L4:             Pop edx, esi

            ..End_If
            Pop edx, esi
        ...End_If

L5:     inc esi
    .End_While
ret
____________________________________________________________________________________________

[JumpIs: D$ ?]

[Down 1
 Up 2]

IsJumpDownOrUp:
    Push ebx, ecx
        Mov eax esi | sub eax D$ShortenJmpsTable | add eax D$CodeListOrigine
        Mov ebx D$CodeRefScan

    ; >>> Two Pointers (One to CodeList -eax-, and one to CodeRef -ecx-):
L1:     Mov ecx D$ebx | and ecx (not relativeFlag)

        .If ecx = eax
            If W$ebx-3 = '>>'
                Mov B$JumpIs DOWN
                Mov eax ebx

            Else_If W$ebx-3 = '<<'
                Mov B$JumpIs UP
                Mov eax ebx

            Else
                Mov B$JumpIs 0

            End_If

        .Else_If ecx = 0
            Mov B$JumpIs 0

        .Else
          ; Next Record in 'CodeRef':
            add ebx 9
          ; No API Call:
            While B$ebx = 0FF | add ebx 6 | End_While
          ; Skip the "labelName|":
            While B$ebx > EOI | inc ebx | End_While | inc ebx | jmp L1<<

        .End_If

L9: Pop ecx, ebx
ret


CouldBothBeShorten:
;;
  When reducing from (5 or 6) to 2, we reduce of (3 or 4)
  From Flag to Label:
  
  Down: (07F): 1 + 127 +(3 or 4) =
                129  +    3 or 4      = 132 or 133
                
  Up:   (080): 128 -(1 or 0) +(3 or 4) =
                128 or 129     3 or 4    = 130, 131, 132
;;

  ; In between Distance, in the 'ShortenJmpsTable' Table:
    Push ebx, ecx, edx
        Mov ebx D$FirstDownShortenJmp, edx D$SecondUpShortenJmp
        Mov ecx edx | sub ecx ebx

        Mov eax &FALSE

        If B$ebx = LONG_Jcc
            Mov ecx 135 | On B$edx = LONG_JMP, dec ecx
        Else
            Mov ecx 134 | On B$edx = LONG_Jcc, inc ecx
        End_If
        inc ecx

        Call GetDisplacementDown

        .If B$CanBeShorten = &TRUE
            If B$edx = LONG_Jcc
                Mov ecx 132 | On B$ebx = LONG_JMP, dec ecx
            Else
                Mov ecx 130 | On B$ebx = LONG_Jcc, inc ecx
            End_If
            inc ecx

            Call GetDisplacementUp

            If B$CanBeShorten = &TRUE
                Mov eax &TRUE
            Else
                Mov eax &FALSE
            End_If

        .Else
            Mov eax &FALSE

        .End_If

L9: Pop edx, ecx, ebx
ret
____________________________________________________________________________________________

SetBothShort:
    Mov eax D$FirstDownShortenJmp

    If B$eax = LONG_JMP
        Mov B$eax SHORTEN_LONG_JMP
    Else
        Mov B$eax SHORTEN_LONG_Jcc
    End_If

    Mov eax D$SecondUpShortenJmp

    If B$eax = LONG_JMP
        Mov B$eax SHORTEN_LONG_JMP
    Else
        Mov B$eax SHORTEN_LONG_Jcc
    End_If

    Mov eax D$FirstDownCodeRefScan, B$eax-2, '.'
    Mov eax D$SecondUpCodeRefScan,  B$eax-2, '.'

    add D$NumberOfLongToShort 2
ret
____________________________________________________________________________________________

[CanBeShorten: D$ ?
 ScanSize: D$ ?]

TryToShortenLong:
    pushad
;;
  ecx is a Pointer to CodeRef, to a Long jmp Displacement. Could it be Short?
  
  1) Is it Long Up or Long Down? >>> Read the '<<' or '>>', at D$CodeRefScan
;;
        Mov B$CanBeShorten &FALSE

        If W$CodeRefName+2 = '>>'
            Mov D$ScanSize (07F+4+1)
;;
  '+4' is the size of the long jump Displacement (We are pointing to the first Byte ot it).
  Either B$LongInstruction = LONG_Jcc, or LONG_JMP do not change a thing:
  Just moving it all upward 1 Byte, later, if LONG_Jcc.
  Why '+1'? Because the Processor will start from the Byte after the jump.
;;
            Call ShortenLongDown

        Else_If W$CodeRefName+2 = '<<'
            Mov D$ScanSize 080
          ; If an Instruction is a Jcc the Opcode length will switch from 2 to 1:
            On B$LongInstruction = LONG_Jcc, inc D$ScanSize

            Call ShortenLongUp

        End_If
    popad
ret
____________________________________________________________________________________________

[ScanLabelPointer: D$ ?]

ShortenLongDown:
  ; ebx = D$CodeRefScan >>> Label Name // ecx = 'CodeRef' Pointer to 'CodeList'

  ; Search a Label Declaration Pointer bigger that the CodeRef Evocation Pointer:
  ; eax = 'LabelList' Pointer to 'CodeList' // ecx = 'CodeRef' Pointer to 'CodeList'
  ; We search for a Pointer, in Label List, such as "ecx < D$esi < edx":
    Mov esi D$LabelListScanPointer

    While D$esi > ecx
        PreviousLabelListPointer esi
    End_While

    While D$esi < ecx
        NextLabelListPointer esi
    End_While

    Mov D$LabelListScanPointer esi

  ; Search the next same Label, in LabelList, as the one pointed by ebx.
  ; Store, for example 'K9', of "K9>>", in ebx:
    Mov bx W$CodeRefName
  ; Scan-Down Limit (07F (127) is the limit for positive signed bytes):
    Mov edx ecx | add edx D$ScanSize

  ; Search for the matching Label in the matching range, if any:
    While D$esi < edx
      ; Does 'esi-3' point to "K9", in "|K9|....f|" ?
      ; A LabelList Record is: // EOI, LabelName, EOI, Pointer, Flag, // EOI, ...
        .If W$esi-3 = bx
            cmp B$esi-4 EOI | jne L5>
            cmp B$esi-1 EOI | jne L5>
              ; Found a matching Label, that is Long, and that can be made Short:
                Mov B$CanBeShorten &TRUE
                inc D$NumberOfLongToShort | jmp L9>

        .Else
L5:         NextLabelListPointer esi

        .End_If

    End_While

    Mov B$CanBeShorten &FALSE
L9: ret
____________________________________________________________________________________________

GetDisplacementDown:
;;
  'FirstDownCodeRefScan' has kept track of the Reference. That is, the pointer to Codelist
  found in the CodeRef Table.
  
  We search for a Pointer, in LabelList, that would be bigger than 'FirstDownCodeRefScan',
  with a matching Name.
  
  Abort if bigger than 'FirstDownCodeRefScan' + 133.
;;
    pushad
      ; Go to the first Label closer to 'FirstDownShortenJmp'
        Mov esi D$LabelListScanPointer
        Mov eax D$FirstDownCodeRefScan, eax D$eax | and eax (not relativeFlag)

        While D$esi > eax
            PreviousLabelListPointer esi ; 'LabelList'
        End_While

        While D$esi < eax
            NextLabelListPointer esi ; 'LabelList'
        End_While
        PreviousLabelListPointer esi
        Mov D$LabelListScanPointer esi

      ; Search the next same Label, in LabelList, as the one pointed by ebx.
      ; Store, for example 'K9', of "K9>>", in ebx:
        Mov bx W$CodeRefName1
      ; Scan-Down Limit:
        Mov edx eax | add edx ecx

      ; Search for the matching Label in the matching range, if any:
        While D$esi < edx
            .If W$esi-3 = bx
                cmp B$esi-4 EOI | jne L5>
                cmp B$esi-1 EOI | jne L5>
                  ; Found a matching Label, that is Long, and that could be made Short:
                    Mov B$CanBeShorten &TRUE | jmp L9>

            .Else
L5:             NextLabelListPointer esi

            .End_If

        End_While

        Mov B$CanBeShorten &FALSE
L9: popad
ret
____________________________________________________________________________________________

; A Jump can be short Up if it is > 0FFFF_FF80

ShortenLongUp:
  ; ebx = D$CodeRefScan >>> Label Name // ecx = 'CodeRef' Pointer to 'CodeList'

  ; Search the next same Label, in LabelList, as the one pointed by ebx.
  ; Store, for example 'K9', of "K9>>", in ebx:
    Mov bx W$CodeRefName
    Mov edx ecx | sub edx D$ScanSize

  ; Search a Label Declaration Pointer smaller that the CodeRef Evocation Pointer:
  ; eax = 'LabelList' Pointer to 'CodeList' // ecx = 'CodeRef' Pointer to 'CodeList'
  ; We search for a Pointer, in Label List, such as "ecx < D$esi < edx":
    Mov esi D$LabelListScanPointer

    While D$esi > ecx
        PreviousLabelListPointer esi
    End_While

    While D$esi < ecx
        NextLabelListPointer esi
        On D$esi = 0, jmp L7>
    End_While

L7: PreviousLabelListPointer esi
    Mov D$LabelListScanPointer esi

  ; Search for the matching Label in the matching range, if any:
    While D$esi > edx
      ; Does 'esi-7' point to "K9", in "|K9|....f|
      ; A LabelList Record is: // EOI, LabelName, EOI, Pointer, Flag, // EOI, ...
        .If W$esi-3 = bx
            cmp B$esi-4 EOI | jne L5>
            cmp B$esi-1 EOI | jne L5>
          ; Found a matching Label, that is Long, and that can be made Short:
                Mov B$CanBeShorten &TRUE
                inc D$NumberOfLongToShort | jmp L9>

        .Else
L5:         PreviousLabelListPointer esi

        .End_If

    End_While

    Mov B$CanBeShorten &FALSE
L9: ret
____________________________________________________________________________________________

GetDisplacementUp:
;;
  'FirstDownCodeRefScan' has kept track of the Reference. That is, the pointer to Codelist
  found in the CodeRef Table.
  
  We search for a Pointer, in LabelList, that would be bigger than 'FirstDownCodeRefScan',
  with a matching Name.
;;
    pushad
      ; Go to the first Label closer to 'FirstDownShortenJmp'
        Mov esi D$LabelListScanPointer
        Mov eax D$SecondUpCodeRefScan, eax D$eax | and eax (not relativeFlag)

        While D$esi < eax
            nextLabelListPointer esi
        End_While

        While D$esi > eax
            PreviousLabelListPointer esi
        End_While
        NextLabelListPointer esi

        Mov D$LabelListScanPointer esi

      ; Search the next same Label, in LabelList, as the one pointed by ebx.
      ; Store, for example 'K9', of "K9>>", in ebx:
        Mov bx W$CodeRefName2
      ; Scan-Down Limit:
        Mov edx eax | sub edx ecx

      ; Search for the matching Label in the matching range, if any:
        Mov eax D$FirstDownCodeRefScan
        While D$esi > edx
            If W$esi-3 = bx
                ;On D$esi > eax, jmp L8>
                cmp B$esi-4 EOI | jne L5>
                cmp B$esi-1 EOI | jne L5>
                  ; Found a matching Label, that is Long, and that could be made Short:
                    Mov B$CanBeShorten &TRUE | jmp L9>

            Else
L5:             PreviousLabelListPointer esi

            End_If
        End_While

L8:     Mov B$CanBeShorten &FALSE
L9: popad
ret
____________________________________________________________________________________________

; LabelListScanPointer points to one of the dWord Pointers, in LabelList:

[LabelListScanPointer: D$ ?]

InitLabelListScanDwordPointer:
  ; Dword | ... // ...| Name | Dword1 Byte | // ....  ;;; 'StoreDataLabel'
  ;    ...|LabelName|....f|LabelName|....f|
    Mov esi D$LabelList | add esi 5
L0: While B$esi > EOI | inc esi | End_While | inc esi

    While B$esi+4 < CodeLabelFlag
        NextLabelListPointer esi
    End_While

    Mov D$LabelListScanPointer esi
  ; >>> D$LabelListScanPointer points to the dWord (Pointer to CodeList)
ret

[NextLabelListPointer
 add #1 6
 N4: cmp B$#1 EOI | je N5> | cmp D$#1 0 | je N5> | inc #1 | jmp N4<
 N5: inc #1]

[PreviousLabelListPointer
 sub #1 2
 N4: cmp B$#1 EOI | je N5> | cmp #1 LabelList | je N5> | dec #1 | jmp N4<
 N5: sub #1 5]

[CodeRefScanPointer: D$ ?]
____________________________________________________________________________________________

;;
  'SHORTEN_LONG_JMP' or 'SHORTEN_LONG_Jcc' found in 'ShortenJmpsTable':
  
  * add 3 to eax.
  
  * All LabelList References to CodeList, downward this Pos, must be substract
    with eax.
    
  * All CodeRef Pointers to a downward Pos must also be substract with eax.
;;

CompactLabelListAndCodeRef:
    Mov ebx D$ShortenJmpsTable, edx D$EndOfShortenJmpsTable

    Call InitLabelListScanDwordPointer
  ; >>> esi ---> D$LabelListScanPointer

  ; Edi = Will point to the dWord1 of 'CodeRef':
    Mov edi D$CodeRef | add edi 5

  ; Case of Api Calls: ...|0FF dWordCodeAddress|:
  ;                   ....|.....|
    While B$edi = 0FF | add edi 6 | End_While

  ; Pointing to the 'CodeRef' Pointer to 'CodeList';
    While B$edi <> EOI | inc edi | End_While | inc edi

    Mov eax 0

    While ebx <= edx
        If B$ebx > SHORTEN
            Mov cl B$ebx, B$LongInstruction cl
            Call AdjustAboveListPointers
            add eax 3
        End_If

        inc ebx
    End_While

    Call AdjustLastListPointers
ret
____________________________________________________________________________________________

AdjustAboveListPointers:
  ; ("Above" means in between the previous access and the actual access).

    Push ebx
      ; esi ---> LabelList / edi ---> CodeRef.

      ; ecx = Offset matching the actual Shorten Jump Displacement, in 'CodeList':
        Mov ecx ebx | sub ecx D$ShortenJmpsTable | add ecx D$CodeListOrigine

      ; LabelList adjustments of the Pointers to Code:
      ; ("<" because the Label is necessary _before_ the Instruction).
        While D$esi < ecx
            sub D$esi eax
            NextLabelListPointer esi
            On D$esi = 0, jmp L1>
        End_While
;;
  CodeRef adjustments of the Pointers to Code:
  
  2 Problems: 
  
  1) The Records for Api calls are irregular. We just skip over them.
  
  2) The Pointer to 'CodeList', in 'CodeRef' (dWord1) the 'RelativeFlag' is [On].
;;
L1:     Mov ebx D$edi | and ebx (not RelativeFlag)
      ; "<=", because the Evocation Reference and the Shorten Jump match exactly:
        .While ebx <= ecx
            .If ebx = ecx
                If B$LongInstruction = SHORTEN_LONG_Jcc
                    inc eax
                End_If
            .End_If
            sub ebx eax
            test D$edi RelativeFlag ZERO L2>
                or ebx RelativeFlag
L2:         Mov D$edi ebx

          ; Next CodeRef Record:
            add edi 9
          ; Cases of Api calls. Comments in 'FillCodeSymbols'. We just skip over:
            While B$edi = 0FF | add edi 6 | End_While
            While B$edi <> EOI | inc edi
                On D$edi = 0, jmp L9>
            End_While
            inc edi
            Mov ebx D$edi | and ebx (not RelativeFlag)
        .End_While

L9: Pop ebx
ret
____________________________________________________________________________________________

AdjustLastListPointers:
  ; Same as above 'AdjustAboveListPointers', but just assume the lasting Records:

  ; LabelList adjustments:
    While D$esi <> 0
        sub D$esi eax
        NextLabelListPointer esi
    End_While

  ; CodeRef adjustments:
    .While D$edi <> 0
        Mov ebx D$edi | and ebx (not RelativeFlag)
        sub ebx eax | Test D$edi RelativeFlag ZERO L1>
            or ebx RelativeFlag

L1:     Mov D$edi ebx
      ; Next CodeRef Record:
        add edi 9
      ; Cases of Api calls. Comments in 'FillCodeSymbols'. We just skip over:
        While B$edi = 0FF | add edi 6 | End_While
        While B$edi <> EOI
            inc edi | On D$edi = 0, jmp L9>
        End_While | inc edi
    .End_While
L9: ret
____________________________________________________________________________________________

CompactCodeList:
    Mov ebx D$ShortenJmpsTable, edx D$EndOfShortenJmpsTable
    Mov esi D$CodeListOrigine, edi esi

    While ebx <= edx
        If B$ebx = SHORTEN_LONG_JMP
          ; jmp Long = 0E9 >>> jmp short = 0EB
            Mov B$edi-1 0EB, B$edi 0
            add esi 4 | add ebx 4 | inc edi

        Else_If B$ebx = SHORTEN_LONG_Jcc
          ; Example: B$esi-2 >>> 0F, 084 (JE long) >>> 074 (JE short), 0:
            Mov al B$esi-1 | sub al 010 | Mov B$edi-2 al, B$edi-1 0
            add esi 4 | add ebx 4

        Else
            movsb | inc ebx

        End_If

    End_While

  ; Copy the Bytes coming after the last modified CodeRef Pointer:
    While esi < D$CodeListPtr | movsb | End_While

  ; Cosmetic clean-up of the trailing Bytes in CodeList:
    Push edi
        Mov al 0 | While edi < D$CodeListPtr | stosb | End_While
    Pop edi
    Mov D$CodeListPtr edi
ret
____________________________________________________________________________________________

CompactShortenJmpsTable:
    Mov esi D$ShortenJmpsTable, edx D$EndOfShortenJmpsTable
    Mov edi esi, eax 0

    While esi <= edx
        If B$esi = SHORTEN_LONG_JMP
            Mov B$esi 0 | add esi 3

        Else_If B$esi = SHORTEN_LONG_Jcc
            Mov B$esi 0 | add esi 4

        Else
            movsb

        End_If

    End_While

    Mov D$EndOfShortenJmpsTable edi
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
