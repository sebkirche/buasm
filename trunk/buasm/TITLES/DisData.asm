TITLE DisData         ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;;
  Write all Data and VirtualData, at once, at the Top of Source.

  3Dfun:
  
  [Data0404040: D$ 0 #03
                 D$ Data0404093]
  [<Data040404D: B$ "@@"]
  [<Data040404F: B$ 0]
  [Data0404050: B$ "Software\Chris Dragan\"]
  
  0408290 non flaged Resources 'IMAGE_RESOURCES_DATA_ENTRY'
;;

;;
; Obsolete
WriteAllDisData:
    Mov esi D$SectionsMap | add esi D$FirstSection
    Mov edx D$EndOfSectionsMap

L0: .While esi < edx
        test B$esi DATAFLAG+VIRTUALFLAG | jz L9>
            Mov cl B$esi
            Mov ebx esi, eax esi
            sub ebx D$SectionsMap | add ebx D$RoutingMap

L1:         inc ebx | inc esi | cmp esi edx | jae L2>
            If B$esi = cl
                test B$ebx LABEL+EVOCATED | jz L1<
            End_If

L2:         sub eax D$SectionsMap | add eax D$SizesMap
            sub ebx D$RoutingMap | add ebx D$SizesMap

            If cl = VIRTUALFLAG
                Call WriteOneVirtualDataChunk, eax, ebx
            Else
                Call WriteOneDataChunk eax, ebx
            End_If

            jmp L0<

L9:     inc esi
    .End_While
ret
;;

Proc WriteOneDataChunksAsFound:
    Argument @Section
    Local @ChunkSize, @Type

      ; Reset the output to the start of Line (first row):
        While B$edi-1 <= SPC | dec edi | End_While
        Mov D$edi CRLF2 | add edi 4

      ; esi points into 'UserPeStart'.

      ; Consider DATA or VIRTUALDATA Flags, in SectionsMap:
        Mov eax D@Section, edx eax, cl B$eax, D@ChunkSize 0, B@Type cl

        While B$edx = cl
            inc edx | inc D@ChunkSize | On edx = D$EndOfSectionsMap, jmp L0>
        End_While
;;
  This 'ChunSize' is the whole size of a 'Chunk'; that may hold several
  Labels, and so forth, several "[Chunk: XXXX]".
  
  'eax:edx' is now the 'Start:End' of the whole 'Chunk', inside 'SectionsMap'.
;;
L0:     .While eax < edx
            Mov ebx eax | sub ebx D$SectionsMap | add ebx D$RoutingMap
            Mov ecx edx | sub ecx D$SectionsMap | add ecx D$RoutingMap

L1:         inc ebx | cmp ebx ecx | jae L2>
            test B$ebx LABEL+EVOCATED ZERO L1<

L2:         sub eax D$SectionsMap | add eax D$SizesMap
            sub ebx D$RoutingMap | add ebx D$SizesMap

          ; Call BuildCommentedDataReference eax ; made by Guga

            Push ebx, edx
                If B@Type = VIRTUALFLAG
                    Call WriteOneVirtualDataChunk, eax, ebx
                Else
                    Call WriteOneDataChunk eax, ebx
                End_If
            Pop edx, eax

            sub eax D$SizesMap | add eax D$SectionsMap
        .End_While

        add esi D@ChunkSize
EndP


[NextDataOutputBreak: D$ ?]
;;
  Here, 'Chunk' stands for 'Chunk from Label to Label'.
;;

[NextDataChunkStart: D$ ?]

; Writes one Data Chunk, in between two 'LABEL+EVOCATED':

Proc WriteOneDataChunk:
    Arguments @SizesmapStart, @SizesmapEnd
    Uses esi, edx

        InitDataLineBreak | Mov B$edi '[' | inc edi

        test D@SizesmapStart 00_11 ZERO L0>
            Mov eax D@SizesmapStart
            If eax = D$NextDataChunkStart
                Mov B$edi '<' | inc edi
            Else
                Mov D$edi '<2 ' | add edi 3
            End_If

L0:     test D@SizesmapStart 00_1111 NOT_ZERO L0>
                Mov D$edi '<16 ' | add edi 4

L0:     Mov eax D@SizesmapStart | sub eax D$SizesMap | add eax D$SectionsMap
        Call WriteOneDataLabel eax

        Mov esi D@SizesmapStart, edx D@SizesmapEnd, cl B$esi
        Mov D$NextDataChunkStart edx

        Mov B$RepetitiveBytesDone &FALSE | Call DisDataTypeRouter

        While B$edi-1 <= SPC | dec edi | End_While
        On B$edi-1 = ',' dec edi

        Mov B$edi ']', W$edi+1 CRLF | add edi 3
EndP


[ActualSizeFlag: D$ ?
 LastSizeFlag: D$ ?
 RealDataChunkEdx: D$ ?]

DisDataTypeRouter:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag

 ; Mov eax esi | sub eax D$SizesMap | add eax D$DisImageBase
 ; On eax = 0403000, int3 ;map

    Mov B$ActualSizeFlag cl, D$RealDataChunkEdx edx

    Call AlignSizeOn ecx

    If edx = esi
        Mov edx D$RealDataChunkEdx
        Mov cl BYTE, B$ActualSizeFlag cl | Call WriteDisBytes | jmp L9>>
    End_If

    On cl = 0, Mov cl BYTE

    .If cl = BYTE
        Mov eax edx | sub eax esi | and eax 00_11
        If eax = 00_10
            Mov cl WORD
        Else_If eax = 00_00
            Mov cl DWORD
        End_If
    .End_If

    .If cl = BYTE
        Call WriteDisBytes
    .Else_If cl = WORD
        Call WriteDisWords
    .Else_If cl = DWORD
        Call WriteDisPointers ; WriteDisdWords
    .Else_If cl = POINTER
        Call WriteDisPointers
    .Else_If cl = POINTER+DWORD
        Call WriteDisPointers
    .Else_If cl = STRINGS+BYTE
        Call WriteDisAscii
    .Else_If cl = STRINGS+WORD
        Call WriteDisUnicode
    .Else_If cl = FP4
        Call WriteDisFP4
    .Else_If cl = FP8
        Call WriteDisFP8
    .Else_If cl = FP10
        Call WriteDisFP10
    .Else_If cl = FP4+POINTER
        Call WriteDisFP4
    .Else_If cl = FP8+POINTER
        Call WriteDisFP8
    .Else_If cl = FP10+POINTER
        Call WriteDisFP10
    .Else_If cl = STRINGS+BYTE+POINTER
        Mov cl STRINGS+BYTE, B$ActualSizeFlag cl
        Call WriteDisAscii
    .Else_If cl = STRINGS+WORD+POINTER
        Mov cl STRINGS+WORD, B$ActualSizeFlag cl
        Call WriteDisUnicode
    .Else_If cl = STRINGS+POINTER
        Mov cl STRINGS+BYTE, B$ActualSizeFlag cl
        Call WriteDisAscii
    .Else
        test cl STRINGS ZERO L1>
            If B$LastSizeFlag = STRINGS+BYTE
                Mov cl STRINGS+BYTE, B$ActualSizeFlag cl | Call WriteDisAscii
            Else_If B$LastSizeFlag = STRINGS+WORD
                Mov cl STRINGS+WORD, B$ActualSizeFlag cl | Call WriteDisUnicode
            Else
                Mov cl STRINGS+BYTE, B$ActualSizeFlag cl | Call WriteDisAscii
            End_If
            jmp L5>>

L1:     Mov eax edx | sub eax esi
        cmp eax 4 | jb L1>
            Call IsPointerCandidate

            If eax = &TRUE
                Mov cl POINTER, B$ActualSizeFlag cl
                Call AlignSizeOn ecx
                Call WriteDisPointers | jmp L5>>
            End_If

L1:   ; edx (end) has been aligned: Unalign:
        Mov edx D$RealDataChunkEdx | Call IsStringCandidate

        If eax = &TRUE
           ; Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
           ; Push esi, edx
                Mov cl STRINGS+BYTE, B$ActualSizeFlag cl | Call WriteDisAscii
           ; Pop edx, esi
           ; Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
        Else
            Mov cl BYTE, B$ActualSizeFlag cl | Call WriteDisBytes
        End_If

    .End_If

  ; Trailing Bytes may remain there because of the above Call to 'AlignSizeOn'. So:
L5: .If esi < D$RealDataChunkEdx
        Mov edx D$RealDataChunkEdx
        Mov W$edi '  ' | add edi 2
        Mov eax edx | sub eax esi
        If eax < 4
            Mov cl BYTE, B$ActualSizeFlag cl | Call WriteDisBytes
        Else
            jmp DisDataTypeRouter
        End_If
    .End_If

L9: Mov cl B$ActualSizeFlag, B$LastSizeFlag cl
ret

[ExportedDataWarning: B$ "
; This Data was an Export, in the original Application.
; RosAsm does not (yet) support such Exports.
" EOS]

Proc WriteOneDataLabel: ; 'WriteDisCodeLabel', 'WriteExportedFunctionLabel'
; called by 'WriteOneDataChunk' and 'WriteOneVirtualDataChunk'
    Argument @SectionsMapPtr

        Mov eax D@SectionsMapPtr | sub eax D$SectionsMap | add eax D$RoutingMap
        Test B$eax EXPORTNODE ZERO L1>  ; 'CheckExport'
            Mov ebx eax, eax D@SectionsMapPtr
            sub eax D$SectionsMap | add eax D$DisImageBase
            Call WriteExportedFunctionLabel

            Push esi | Mov esi ExportedDataWarning | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0: Pop esi

L1:     Mov eax D@SectionsMapPtr

        If B$eax = DATAFLAG
            Mov D$edi 'Data' | add edi 4
        Else_If B$eax = VIRTUALFLAG
            Mov D$edi 'Virt', D$edi+4 'ual' | add edi 7
        End_If

        sub eax D$SectionsMap | add eax D$DisImageBase
        Push eax
            Call WriteEax
        Pop eax

        ToStringsMapFrom DisImageBase,
                         eax


            ; !!! TODO D$eax = 0
            Push esi | Mov esi D$eax | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0: Pop esi



        End_If

        Mov W$edi ': ' | add edi 2
EndP


Proc AlignSizeOn:
    Argument @Unit
    Uses ecx

      ; esi = SizesmapStart, edx = SizesmapEnd
        Mov ecx D@Unit | and ecx (not STRINGS)

        .If cl = POINTER
            Mov ecx 4
        .Else
            and ecx (not POINTER)
            If cl = 0
                Mov ecx BYTE | ExitP
            Else_If cl = BYTE
                ExitP
            Else_If cl = WORD
                Mov ecx 2
            Else_If cl = DWORD
                Mov ecx 4
            Else_If cl = FP4
                Mov ecx 4
            Else_If cl = FP8
                Mov ecx 8
            Else_If cl = FP10
                Mov ecx 10
            End_If
        .End_If

        Mov eax edx | sub eax esi
        Mov edx 0 | div ecx | mul ecx
        Mov edx esi | add edx eax
EndP


[RepetitiveBytesDone: D$ ?
 NumberOfRepetitiveData: D$ ?]

IsRepetitiveBytes:
    If B$RepetitiveBytesDone = &TRUE
        Mov D$NumberOfRepetitiveData 0 | ret
    End_If

    Push esi, ecx, edx
        sub edx D$SizesMap | add edx D$UserPeStart

        Mov esi ebx, al B$esi, ecx 0

        While B$esi = al
            lodsb | inc ecx | cmp esi edx | jae L2>>
        End_While

L2:     If ecx > 1
            Mov D$NumberOfRepetitiveData ecx
        Else
            Mov D$NumberOfRepetitiveData 0
        End_If

    Pop edx, ecx, esi

    Mov B$RepetitiveBytesDone &TRUE
ret


IsRepetitiveWords:
    If B$RepetitiveBytesDone = &TRUE
        Mov D$NumberOfRepetitiveData 0 | ret
    End_If

    Push esi, ecx, edx
        sub edx D$SizesMap | add edx D$UserPeStart

        Mov esi ebx, ax W$esi, ecx 0

        While W$esi = ax
            lodsw | inc ecx | cmp esi edx | jae L2>>
        End_While

L2:     If ecx > 1
            Mov D$NumberOfRepetitiveData ecx
        Else
            Mov D$NumberOfRepetitiveData 0
        End_If

    Pop edx, ecx, esi

    Mov B$RepetitiveBytesDone &TRUE
ret


IsRepetitivedWords:
    If B$RepetitiveBytesDone = &TRUE
        Mov D$NumberOfRepetitiveData 0 | ret
    End_If

    Push esi, ecx, edx
        sub edx D$SizesMap | add edx D$UserPeStart

        Mov esi ebx, eax D$esi, ecx 0

        While D$esi = eax
            lodsd | inc ecx | cmp esi edx | jae L2>>
        End_While

L2:     If ecx > 1
            Mov D$NumberOfRepetitiveData ecx
        Else
            Mov D$NumberOfRepetitiveData 0
        End_If

    Pop edx, ecx, esi

    Mov B$RepetitiveBytesDone &TRUE
ret


WriteDisBytes: ; 'WriteBytesData', 'WriteAsciiData'
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    On B$edi-2 = '$', sub edi 3
    Mov D$edi 'B$ ' | add edi 3

    Mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart

    Call IsRepetitiveBytes

    If D$NumberOfRepetitiveData > 0
        movzx eax B$ebx | Push ebx | Call WriteEax | Pop ebx
        Mov W$edi ' #' | add edi 2
        Mov eax D$NumberOfRepetitiveData | Push ebx | Call WriteEax | Pop ebx
        add esi D$NumberOfRepetitiveData
        add ebx D$NumberOfRepetitiveData | cmp esi edx | jae L9>
        Call NextDisDataLine
    End_If

L0: movzx eax B$ebx | Push ebx | Call WriteEax | Pop ebx

    inc esi | inc ebx | cmp esi edx | jae L9>

        .If B$esi <> 0
            If B$esi <> cl
                Call NextDisDataLine
                Mov cl B$esi | ret
            End_If
        .End_If

        If edi > D$NextDataOutputBreak
            Call NextDisDataLine
        Else_If esi < edx
            Mov D$edi ', ' | add edi 2
        End_If

    jmp L0<
L9: ret


WriteDisWords:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
   On B$edi-2 = '$', sub edi 3
    Mov D$edi 'W$ ' | add edi 3

    Mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart

    Call IsRepetitiveWords

    If D$NumberOfRepetitiveData > 0
        movzx eax W$ebx | Push ebx | Call WriteEax | Pop ebx
        Mov W$edi ' #' | add edi 2
        Mov eax D$NumberOfRepetitiveData | Push ebx | Call WriteEax | Pop ebx
        Mov eax D$NumberOfRepetitiveData | shl eax 1
        add esi eax | add ebx eax | cmp esi edx | jae L9>
        Call NextDisDataLine
    End_If

L0: Mov eax edx | sub eax esi
    If eax < 2
        Mov cl BYTE | ret
    End_If

    movzx eax W$ebx | Push ebx | Call WriteEax | Pop ebx

    add esi 2 | add ebx 2 | cmp esi edx | jae L9>

        .If B$esi <> 0
            If B$esi <> cl
                Call NextDisDataLine
                Mov cl B$esi | ret
            End_If
        .End_If

        If edi > D$NextDataOutputBreak
            Call NextDisDataLine
        Else_If esi < edx
            Mov D$edi ', ' | add edi 2
        End_If

    jmp L0<
L9: ret


[WasValidPointer: D$ ?]

WriteDisPointers:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    On B$edi-2 = '$', sub edi 3
    Mov D$edi 'D$ ' | add edi 3

    Mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart

L0: Mov eax edx | sub eax esi
    If eax < 4
        Mov cl BYTE | ret
    End_If

    Mov eax D$ebx

    ;sub eax D$DisImageBase | add eax D$RoutingMap
    ;test B$eax LABEL | jz WriteDisdWords
    ;Mov eax D$ebx
;;
  This case of zeroed POINTERs should be turned DWORDs, in 'CheckFlagsCoherency'
;;
    .If eax = 0
        Call IsRepetitivedWords

        If D$NumberOfRepetitiveData > 0
            Mov eax D$ebx | Push ebx | Call WriteEax | Pop ebx
            Mov W$edi ' #' | add edi 2
            Mov eax D$NumberOfRepetitiveData | Push ebx | Call WriteEax | Pop ebx
            Mov eax D$NumberOfRepetitiveData | shl eax 2
            add esi eax | add ebx eax | cmp esi edx | jae L9>>
            Call NextDisDataLine | jmp L0<
        End_If

    .End_If

    Mov B$RepetitiveBytesDone &TRUE

    Push eax
        sub eax D$DisImageBase | add eax D$SectionsMap
        ..If eax > D$SectionsMap
            .If eax < D$EndOfSectionsMap
                sub eax D$SectionsMap | add eax D$RoutingMap
                test B$eax LABEL+INSTRUCTION+EVOCATED ZERO L2>
L1:             sub eax D$RoutingMap | add eax D$SectionsMap
                If B$eax = CODEFLAG
                    sub eax D$SectionsMap | add eax D$RoutingMap
                    test B$eax LABEL+INSTRUCTION ZERO L2>
                    Mov D$edi 'Code' | add edi 4
                Else_If B$eax = DATAFLAG
                    Mov D$edi 'Data' | add edi 4
                Else_If B$eax = VIRTUALFLAG
                    Mov D$edi 'Virt', D$edi+4 'ual' | add edi 7
                End_If
            .End_If
        ..End_If
L2: Pop eax

    .If D$edi-4 = 'tual'
        Mov B$WasValidPointer &TRUE
    .Else_If D$edi-4 = 'Data'
        Mov B$WasValidPointer &TRUE
    .Else_If D$edi-4 = 'Code'
        Mov B$WasValidPointer &TRUE
    .Else
        Mov B$WasValidPointer &FALSE
        Push eax
            Mov edx D$RealDataChunkEdx | Call IsStringCandidate
            If eax = &TRUE
                Pop eax
                Mov cl STRINGS+BYTE, B$ActualSizeFlag cl | Call WriteDisAscii | ret
            End_If
        Pop eax
    .End_If

    Push ebx, eax | Call WriteEax | Pop eax, ebx

    ..If B$WasValidPointer = &TRUE
        ToStringsMapFrom DisImageBase, eax
        .If D$eax <> 0
            Push esi
                If D$eax = MainWindowProcName
                    While D$edi <> 'Code' | dec edi | End_While
                End_If

                Mov esi D$eax | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0:

            Pop esi
        .End_If
    ..End_If

    add esi 4 | add ebx 4 | cmp esi edx | jae L9>

        .If B$esi <> 0
            If B$esi <> cl
                Call NextDisDataLine
                Mov cl B$esi | ret
            End_If
        .End_If

        If edi > D$NextDataOutputBreak
            Call NextDisDataLine
        Else_If esi < edx
            Mov D$edi ', ' | add edi 2
        End_If

    jmp L0<<
L9: ret


[InsideQuotes: D$ ?]

WriteDisAscii: ; 'WriteAsciiData'
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    Mov B$RepetitiveBytesDone &TRUE
    On B$edi-2 = '$', sub edi 3
    Mov D$edi 'B$ ' | add edi 3
    Mov B$InsideQuotes &FALSE

    Mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart

L0: movzx eax B$ebx
;On D$ebx = '3D F', int3
    Push ebx
        Mov ebx D$TruthAsciiTable

        ...If B$ebx+eax = GOODASCII
;;
  Isolated cases of 13 or 10 might be miss-interpretated as part of a CRLF:
;;
            If al = CR
                Pop ebx | Push ebx
                On B$ebx <> LF, jmp L2>
            Else_If al = LF
                Pop ebx | Push ebx
                On B$esi-2 <> CR, jmp L2>
            End_If

            On al = '"', jmp L2>

            If B$InsideQuotes = &FALSE
                Mov B$edi '"' | inc edi
                Mov B$InsideQuotes &TRUE
            End_If
            stosb

        ...Else
L2:         If B$InsideQuotes = &TRUE
                Mov B$edi '"' | inc edi
                Mov B$InsideQuotes &FALSE
            End_If
            .If W$edi-2 <> ', '
                If W$edi-3 <> 'B$'
                    Mov W$edi ', ' | add edi 2
                End_If
            .End_If
            Call WriteEax
            inc esi
            ..If esi < edx
                .If W$edi-2 <> ', '
                    If W$edi-3 <> 'B$'
                        Mov D$edi ', ' | add edi 2
                    End_If
                .End_If
            ..End_If
            dec esi
        ...End_If
    Pop ebx

    inc esi | inc ebx | cmp esi edx | jae L9>

        ..If B$esi <> 0
            .If B$esi <> cl
                If B$InsideQuotes = &TRUE
                    Mov B$edi '"' | inc edi
                    Mov B$InsideQuotes &FALSE
                End_If
                Call NextDisDataLine
                Mov cl B$esi | ret
            .End_If
        ..End_If

        jmp L0<<

L9: If B$InsideQuotes = &TRUE
        Mov B$edi '"' | inc edi
        Mov B$InsideQuotes &FALSE
    End_If
ret


WriteDisUnicode:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    Mov B$RepetitiveBytesDone &TRUE
    On B$edi-2 = '$', sub edi 3
    Mov D$edi 'U$ ' | add edi 3
    Mov B$InsideQuotes &FALSE

    Mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart

L0: movzx eax B$ebx

    Push ebx, ecx
        Mov ecx ebx, ebx D$TruthAsciiTable
        ..If B$ebx+eax = GOODASCII
            On al = '"', jmp L2>
            If al = CR
                On B$ecx+2 <> LF, jmp L2>
            Else_If al = LF
                On B$ecx-2 <> CR, jmp L2>
            End_If
            If B$InsideQuotes = &FALSE
                Mov B$edi '"' | inc edi
                Mov B$InsideQuotes &TRUE
            End_If
            stosb
        ..Else
L2:         If B$InsideQuotes = &TRUE
                Mov B$edi '"' | inc edi
                Mov B$InsideQuotes &FALSE
            End_If
            .If W$edi-2 <> ', '
                If W$edi-3 <> 'U$'
                    Mov W$edi ', ' | add edi 2
                End_If
            .End_If
            Call WriteEax
            inc esi
            .If esi < edx
                .If W$edi-2 <> ', '
                    If W$edi-3 <> 'U$'
                        Mov W$edi ', ' | add edi 2
                    End_If
                .End_If
            .End_If
            dec esi
        ..End_If
    Pop ecx, ebx

    add esi 2 | add ebx 2 | cmp esi edx | jae L9>

        ..If B$esi <> 0
            .If B$esi <> cl
                If B$InsideQuotes = &TRUE
                    Mov B$edi '"' | inc edi
                    Mov B$InsideQuotes &FALSE
                End_If
                Call NextDisDataLine
                Mov cl B$esi | ret
            .End_If
        ..End_If

        jmp L0<<

L9: If B$InsideQuotes = &TRUE
        Mov B$edi '"' | inc edi
        Mov B$InsideQuotes &FALSE
    End_If
ret


WriteDisFP4: ; WriteDisdWords
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    On B$edi-2 = '$', sub edi 3

L0: Mov eax edx | sub eax esi
    If eax < 4
        Mov cl BYTE | ret
    End_If

    Push esi, edx
        Mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart
        Call WriteFP4
    Pop edx, esi

    add esi 4

    .If B$esi <> 0
        If B$esi <> cl
            Call NextDisDataLine
            Mov cl B$esi | ret
        End_If
    .End_If

    .If esi < edx
        If edi > D$NextDataOutputBreak
            Call NextDisDataLine
        Else_If esi < edx
            Mov D$edi ', ' | add edi 2
        End_If

        jmp L0<
    .End_If

    Mov B$RepetitiveBytesDone &TRUE
ret


WriteDisFP8:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    On B$edi-2 = '$', sub edi 3

L0: Mov eax edx | sub eax esi
    If eax < 8
        Mov cl BYTE | ret
    End_If

    Push esi, edx
        Mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart
        Call WriteFP8
    Pop edx, esi

    add esi 8

    .If B$esi <> 0
        If B$esi <> cl
            Call NextDisDataLine
            Mov cl B$esi | ret
        End_If
    .End_If

    .If esi < edx
        If edi > D$NextDataOutputBreak
            Call NextDisDataLine
        Else_If esi < edx
            Mov D$edi ', ' | add edi 2
        End_If

        jmp L0<
    .End_If

    Mov B$RepetitiveBytesDone &TRUE
ret


WriteDisFP10:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    On B$edi-2 = '$', sub edi 3

L0: Mov eax edx | sub eax esi
    If eax < 10
        Mov cl BYTE | ret
    End_If

    Push esi, edx
        Mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart
        Call WriteFP10
    Pop edx, esi

    add esi 10

    .If B$esi <> 0
        If B$esi <> cl
            Call NextDisDataLine
            Mov cl B$esi | ret
        End_If
    .End_If

    .If esi < edx
        If edi > D$NextDataOutputBreak
            Call NextDisDataLine
        Else_If esi < edx
            Mov D$edi ', ' | add edi 2
        End_If

        jmp L0<
    .End_If

    Mov B$RepetitiveBytesDone &TRUE
ret


NextDisDataLine:
    InitDataLineBreak
    Mov D$edi 020200A0D, D$edi+4 '    ', D$edi+8 '    ', D$edi+12 '    ', D$edi+16 '    '
    add edi 19
ret

[InitDataLineBreak | Mov D$NextDataOutputBreak edi | add D$NextDataOutputBreak 70]


IsPointerCandidate:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    Push ecx, esi
        Mov ecx edx | sub ecx esi | shr ecx 2

        sub esi D$Sizesmap | add esi D$Routingmap

L0:     test B$esi INDIRECT ZERO L1>
            Mov eax esi
            sub eax D$Routingmap | add eax D$UserPeStart
            Mov eax D$eax
            sub eax D$DisImageBase | add eax D$RoutingMap

            If eax < D$Routingmap
                ;
            Else_If eax < D$EndOfRoutingMap
                test B$eax LABEL ZERO L1>
                    Mov eax &TRUE | jmp L9>
            End_If

L1:         add esi 4 | loop L0<

            Mov eax &FALSE
L9: Pop esi, ecx
ret


IsStringCandidate:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    Push esi, ecx, edx
L0:     Mov ecx edx | sub ecx esi
        sub esi D$Sizesmap | add esi D$UserPeStart | On B$esi = 0, jmp L8>

        Mov eax 0, edx D$TruthAsciiTable
L0:     lodsb | On B$edx+eax = GOODASCII, loop L0<
        jecxz L9>

L1:         If al = 0
                lodsb | loop L1<
                jecxz L9>
            Else_If B$edx+eax = GOODASCII
                jecxz L9>
                loop L0<
            End_If

L8:     Mov eax &FALSE
    Pop edx, ecx, esi
ret

L9:     Mov eax &TRUE
    Pop edx, ecx, esi
ret
____________________________________________________________________________________________

Proc WriteOneVirtualDataChunk:
    Arguments @SizesmapStart, @SizesmapEnd
    Uses esi, edx

        InitDataLineBreak | Mov B$edi '[' | inc edi

        test D@SizesmapStart 00_11 ZERO L0>
            Mov B$edi '<' | inc edi

L0:     Mov eax D@SizesmapStart | sub eax D$SizesMap | add eax D$SectionsMap
        Call WriteOneDataLabel eax

        Mov esi D@SizesmapStart, ecx D@SizesmapEnd | sub ecx esi

        ..If ecx = 1
            Mov D$edi 'B$ ?' | add edi 4
        ..Else_If ecx = 2
            Mov D$edi 'W$ ?' | add edi 4
        ..Else_If ecx = 4
            If B$esi = FP4
                Mov D$edi 'F$ ?'
            Else
                Mov D$edi 'D$ ?'
            End_If
            add edi 4
        ..Else
        ; LOOPVDATAMAX
L1:         Mov eax ecx | and eax 0011
            .If eax = 0
                If B$esi = FP4
                    Mov D$edi 'F$ ?'
                Else
                    Mov D$edi 'D$ ?'
                End_If
                Mov W$edi+4 ' #'  | add edi 6
                Mov eax ecx | shr eax 2
                On eax > LOOPVDATAMAX, Mov eax LOOPVDATAMAX
                Call WriteEax
            .Else
                Mov D$edi 'B$ ?', W$edi+4 ' #'  | add edi 6
                Mov eax ecx | On eax > LOOPVDATAMAX, Mov eax LOOPVDATAMAX
                Call WriteEax
            .End_If
        ..End_If
        Mov B$edi ']' | inc edi

      ; Case of set sizes > RosAsm Max, split into as many sub-sets as wanted:
        If ecx > LOOPVDATAMAX
            sub ecx LOOPVDATAMAX | NextDisLine
            Push esi
                Mov esi edi
                While B$esi <> '[' | dec esi | End_While
                While B$esi <> ':' | movsb | End_While
                Mov B$edi 'X' | inc edi
                While B$esi <> '$' | movsb | End_While | dec edi
            Pop esi
            jmp L1<<
        End_If

        Mov D$edi CRLF2 | add edi 4
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
