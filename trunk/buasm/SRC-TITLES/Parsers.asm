TITLE Parsers         ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________
; Maintainer: Rikkert Wiggerink (EvilBro)
; Started: Somewhere in april 2003
; Email: r.wiggerink@student.utwente.nl
____________________________________________________________________________________________
;;
29-4-2003:
 This is the first version of the rewritten parsers in an official RosAsm release.
 This is by no means the end of the revision. :)
  
 The philosophy behind this revision:
    The routines that previously cleaned up the source have been replaced and split up into 
    many different routines (duplicate actions were removed). This is done for maintainance 
    purposes. Anyone who has taken a look at the old routines will see the importance of this 
    split up (from a maintainance point of view anyway :) )
    
    There will be some people who will think the clean up needs to be done in a single routine
    (Yes, I mean Kenny :) ). This will not work. It is what Betov started with when the old
    routines were written (and we all know the hell that followed from that... at least I do).
    There is also no need for the extra performance a single clean up routine would bring.
    On my Pentium 90 compilation of RosAsm takes 39.8 seconds with the new routines and 
    38.5 seconds with the old routines (average timing values used). That means that the new 
    routines are roughly a second slower (on a source like RosAsm). However, this second is 
    completely insignificant compared to the total compile time.

 All parsers I've written expect the source to be in CodeSourceA. They will either modify the 
 source in CodeSourceA directly or copy/modify the source to CodeSourceB. On exiting a routine
 the source will be in CodeSourceA. This is done for simplicity.

 Anyway, I expect that now this version is implemented in an official RosAsm release, errors 
 are bound to turn up. Just post them on the RosAsm forum or mail them directly to me and 
 I'll fix them.

 BTW 'Kill' means 'replace by spaces'. 'Strip' means 'replace by nothing'.
;;
____________________________________________________________________________________________
____________________________________________________________________________________________
; NewCopyToCodeSourceA replaces CopyToCodeSourceA which always used CodeSource as the <Source>.
; usage: Call NewCopyToCodeSourceA <Source> <SourceLength>

Proc NewCopyToCodeSourceA:
    Arguments @Pointer, @Length

        Mov esi D@Pointer, ecx D@Length, edi D$CodeSourceA, D$StripLen ecx
        rep movsb
EndP

Proc InjectedCopyToCodeSourceA:
    Arguments @Pointer, @Length

        Mov esi InjectedTIME_COUNT, edi D$CodeSourceA, ecx D$InjectedTIME_COUNT_Len
        Mov D$StripLen ecx | rep movsb

        Mov esi D@Pointer, ecx D@Length | add D$StripLen ecx | rep movsb
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________
; CoolParsers are all parsers that do not change the position of statements in the source.

CoolParsers:
    Call CheckTextDelimitersPairing
    Call KillMultiLineComments ; and Comments
   ; Call KillSingleLineComments
    Call NewKillVirtualCRLF
    Call KillMeaninglessCommas
    Call CheckandKillPreParsers
        On B$ParseIncInclude = &TRUE, Call ClearIncludeStateMentsFromSource ;Call IncParser
        On B$ParseAlternates = &TRUE, Call AlternatesPreparsers
      ; +0.2 seconds (2.850 >>> 3.650) on RosAsm 4 Megas, with a Celeron 1.3.
    Call KillTitles ; + Old 'ConvertTextSigns'
   ; Call CheckBracketsPairing
    ;Call CheckNestedBracketsPairing
    Call CheckPairings
    Call ReplaceParaMacrosBrackets
   ; Call CheckOpenCloseSignPairing
ret
;;
CoolParsersOnInc:
        Call CheckTextDelimitersPairing
        Call KillMultiLineComments ; and Comments
       ; Call KillSingleLineComments
        Call NewKillVirtualCRLF
        Call KillMeaninglessCommas
;;

CoolParsersOnInc: ; CoolParsers
    Push D$CodeSourceA, D$StripLen

        Move D$CodeSourceA D$bininc.mem, D$StripLen D$bininc.filesize

        Call CheckTextDelimitersPairing
        Call KillMultiLineComments ; and Comments
       ; Call KillSingleLineComments
        Call NewKillVirtualCRLF
        Call KillMeaninglessCommas

    Pop D$StripLen, D$CodeSourceA
ret

____________________________________________________________________________________________
; HotParsers are parsers that can change the position of statements in the source.

HotParsers:
    Call TranslateAsciiToMyAscii

    Call StripUnderscore
    On B$ProfilerFlag = &TRUE, Call InjectDashLines

    Call StripUnneededSpaces
    Call ConvertCommasToSpace
    Call StripUnneededEOI
    Call ConvertEOIinBracketsTOmeEOI
   ; Call ConvertTextSigns    ; This one 'needs' to be done sooner. Would simplify earlier routines
    Call ExtendLocalSymbols
    Call IdentifyVirtualData
    Call ReorderSource
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
CheckTextDelimitersPairing:
    Mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

@TODO.CheckTextDelimitersPairing:

    Mov B$esi-1 LF      ; for MultiLineComment starting on the first line

    .While esi < ecx
        .If B$esi = '"'
            Mov edx esi
            Do
                inc esi | cmp esi ecx | je L9>  ; Error: no closing delimiter found inside source.
            Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            Mov edx esi
            Do
                inc esi
                cmp esi ecx | je L9>        ; Error: no closing delimiter found inside source.
                cmp B$esi CR | je L9>       ; Error: ' isn't allowed to be multiline.
            Loop_Until B$esi = "'"
        .Else_If B$esi = ';'
            If D$esi-1 = MLC
                Do
                    inc esi | cmp esi ecx | je L8>
                Loop_Until D$esi = MLC
                inc esi
            Else
                Do
                    inc esi
                Loop_Until B$esi < SPC
            End_If
        .End_If
        inc esi
    .End_While
L8: ret

L9: ;ERROR! Unpaired textdelimiter.
    Mov esi edx
    While B$esi > LF | dec esi | End_While
    Mov edi CookedErrorMessage
    While B$esi <> CR
        movsb | On edi = EndOfCookedErrorMessage, jmp L2>
    End_While
L2: Mov B$edi 0

    Mov B$Errorlevel 9 | Error D$OpenTextPtr

ret
____________________________________________________________________________________________
; Multiline comments are converted to spaces.

KillMultiLineComments:
    Mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    Mov B$esi-1 LF      ; for MultiLineComment starting on the first line

    .While esi < ecx
        ..If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        ..Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        ..Else_If B$esi = ';'
            .If D$esi-1 = MLC
                Do
                    Mov B$esi SPC
                    inc esi | On esi >= ecx, ret
                Loop_Until D$esi = MLC
                Mov D$esi 0D202020  ; Replace 'LF ; ; CR' with 'Space Space Space CR'.
                add esi 3
            .Else
                ;On D$esi+1 = ' Tag', Call AssemblyTag
                Do | Mov B$esi SPC | inc esi | Loop_Until B$esi < SPC
            .End_If
        ..End_If
        inc esi
    .End_While
L8: ; KillMultiLineComments might have killed the closing CRLF, thus it is restored.
    Mov W$esi-2 0A0D
ret
________________________________________________________________________________________________
; Singleline comments are converted to spaces.
; Warning: This routine will also kill ";;", so if it is run before MultiLineComment are killed
; then you can be pretty sure, you won't be able to strip MultiLineComments correctly.

KillSingleLineComments:
    Mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        .Else_If B$esi = ';'
            Do | Mov B$esi SPC | inc esi | Loop_Until B$esi < SPC
        .End_If
        inc esi
    .End_While
L8: ret
________________________________________________________________________________________________
; Titles are converted to spaces.

KillTitles: ; ConvertTextSigns
    Mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = '"'
            Mov B$esi TextSign
            Do | inc esi | Loop_Until B$esi = '"'
            Mov B$esi TextSign
        .Else_If B$esi = "'"
            Mov B$esi TextSign
            Do | inc esi | Loop_Until B$esi = "'"
            Mov B$esi TextSign
        .Else_If D$esi = 'TITL'
            If B$esi-1 = LF
                On W$esi+4 <> 'E ', jmp L0>
                    Do | Mov B$esi SPC | inc esi | Loop_Until B$esi < SPC
L0:
            End_If
        .End_If

        inc esi

    .End_While

L8: ret

________________________________________________________________________________________________
; VirtualCRLFs are converted to spaces.

[DisableWarning: B$ 0]

NewKillVirtualCRLF:
    Mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    ..While esi < ecx
        ...If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        ...Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        ...Else_If B$esi = ','
            Push esi
            .While W$esi-1 <> CRLF
                dec esi
                    ..If_Or B$esi = '+', B$esi = '-', B$esi = '/', B$esi = '*', B$esi = '^'
                        .If B$DisableWarning = &FALSE
                            SyntaxErrorInMacro1 D$BadSyntaxBeforeCommaPtr esi
                            pushad
                            Call 'USER32.MessageBoxA' 0, {B$ "Disable previous warning ?" EOS}, {B$ 'Syntax Error Found' EOS}, &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION__&MB_YESNO
                            If eax = &IDYES
                                Mov B$DisableWarning &TRUE
                            End_If
                            popad

                        .End_If
                        Mov B$esi SPC
                    ..Else_If B$esi = SPC
                    ..Else ; any other char, exit
                        jmp L5>
                    ..End_If
            .End_While
         L5: | Pop esi
        ...Else_If B$esi = CR
            Mov edi esi, al SPC
            While B$edi <= SPC
                dec edi | On edi < D$CodeSourceA, jmp L7>
            End_While
            If B$edi = ','
                Do
                    stosb | cmp edi ecx | ja L8>
                Loop_Until B$edi > SPC
            End_If
        ...End_If

L7:     inc esi

    ..End_While
L8: ret
________________________________________________________________________________________________
; Meaningless commas are converted to spaces.
; Note: This routine will probably Move to HotParsers in the future.

KillMeaninglessCommas:
    Mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        .Else_If B$esi = ','
            Mov edi esi
            Do
                inc edi
            Loop_Until B$edi <> SPC

            If B$edi = '+'
                Mov esi edi
            Else_If B$edi = '-'
                Mov esi edi
            Else
                Mov B$esi SPC
            End_If
        .End_If

        inc esi

    .End_While
ret
____________________________________________________________________________________________
; Checks for PREPARSE statements, sets the right flags and then kills the PREPARSE statements.
; Note that any amount of PREPARSE statements can be used in the source with this new routine.

[ParseAlternates: D$ ?
 ParseEqual: D$ ?
 ParseOOA: D$ ?
 ParseNew: D$ ?
 ParseBinInclude: D$ ?
 ParseIncInclude: D$ ?]

CheckandKillPreParsers:

    Mov B$ParseAlternates &FALSE, B$ParseEqual &FALSE, B$ParseOOA &FALSE, D$FL.Dynamic &FALSE

    Mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        .Else_If D$esi = 'PREP'             ; PREPARSE
            ..If D$esi+4 = 'ARSE'
                If B$esi-1 = LF
                    On B$esi+8 <> SPC, jmp L6>
                    Call NewCheckPreparser
                    Do
                        Mov B$esi SPC
                        inc esi
                    Loop_Until B$esi < SPC
L6:
                End_If
            ..End_If
        .End_If

L7:     inc esi

    .End_While
L8: ret
_______________________________________________________________________________________________
; This routine is not really altered at this point compared to the old routine. This will be
; done in a future version as this version is really unreadable (and thus hard to maintain).

[FL.Dynamic: D$ ?
 MemReservation: D$ ?]

NewCheckPreparser:
    Mov D$MemReservation 0

    Mov edi esi
    add edi 9

L1: While B$edi = SPC | inc edi | End_While

    Mov eax D$edi | and eax (not 020202020)     ; Convert to uppercase.
    ...If eax = 'ALTE'                          ; ALTERNATES
        Mov eax D$edi+4 | and eax (not 020202020)
        ..If eax = 'RNAT'
            Mov ax W$edi+8 | and eax (not 02020)
            .If ax = 'ES'
                If B$edi+10 <= SPC
                    Mov B$ParseAlternates &TRUE | add edi 11 | jmp L8>>
                End_If
            .End_If
    ...Else_If eax = 'EQUA'                     ; EQUAL
        Mov al B$edi+4 | and eax (not SPC)
        .If al = 'L'
            If B$edi+5 <= SPC
                Mov B$ParseEqual &TRUE | add edi 6 | jmp L8>>
            End_If
        .End_If
    ...Else_If eax = 'BINI'                     ; BinIncluder
        Mov eax D$edi+4 | and eax (not 020202020)
        .If eax = 'NCLU'
            Mov eax D$edi+7 | and eax (not 020202020)
            If eax = 'UDER'
                Mov B$ParseBinInclude &TRUE | add edi 12 | jmp L8>>
            End_If
        .End_If
    ...Else_If eax = 'INCI'                     ; IncIncluder
        Mov eax D$edi+4 | and eax (not 020202020)
        .If eax = 'NCLU'
            Mov eax D$edi+7 | and eax (not 020202020)
            If eax = 'UDER'
                Mov B$ParseIncInclude &TRUE | add edi 12 | jmp L8>>
            End_If
        .End_If
    ...Else_If ax = 'OO'                        ; OOA
        Mov al B$edi+2 | and eax (not SPC)
        .If al = 'A'
            If B$edi+3 <= SPC
                Mov B$ParseOOA &TRUE | add edi 4 | jmp L8>>
            End_If
        .End_If
    ...Else_If ax = 'NE'                        ; Prepare New
        Mov al B$edi+2 | and eax (not SPC)
        .If al = 'W'
            If B$edi+3 <= SPC
                Mov B$ParseNew &TRUE | add edi 4 | jmp L8>>
            End_If
        .End_If
    ...Else_If ax = 'EN'                        ; Preparse EntryPoint
        Mov eax D$edi+2 | and eax (not 020202020)
        .If eax = 'TRYP'
            Mov eax D$edi+6 | and eax (not 020202020)
            If eax = 'OINT'
                Call TakeNewEntryPoint | ret   ; Must be separated on one line
            End_If
        .End_If
    ...Else_If eax = 'DYNA'                        ; Preparse Dynamic
        Mov eax D$edi+3 | and eax (not 020202020)
        If eax = 'AMIC'
            Mov D$FL.Dynamic &TRUE | add edi 8 | jmp L8>
        End_If
;;
  ; Must be run before this time...
  
    ...Else_If eax = 'RESE'                        ; Preparse Reserve
        Mov eax D$edi+3 | and eax (not 020202020)
        If eax = 'ERVE'
            Call ReadMemoryReservation
            add edi 8 | jmp L8>
        End_If
;;

    ...End_If

    Mov B$edi-1 0, esi edi
    While B$edi > SPC | inc edi | End_While | Mov B$edi 0
    Mov B$ErrorLevel 9 | Error D$BadPreParsePtr, esi

L8:  On B$edi >= SPC, jmp L1<<

; Possible add of multiple Pre-Parsers conflicts checking...
ret


ReadMemoryReservation:
    Push esi
        Mov esi edi
        While B$esi > SPC | inc esi | End_While
        While B$esi = SPC | inc esi | End_While
        If B$esi = '0'
            Call TranslateHexa
        Else
            Call TranslateDecimal
        End_If

        Mov D$MemReservation eax
    Pop esi
ret


[BadEntryDef: B$ 'Bad definition of EntryPoint in "Preparse EntryPoint"' EOS]

TakeNewEntryPoint: ; Preparse EntryPoint Name
    Push esi, edi, ecx
        Mov esi edi | add esi 11 | Mov edi EntryPointLabel, ecx 0

        While B$esi > SPC
            lodsb | and eax (not SPC) | stosb | inc ecx
        End_While
        Mov B$edi 0

        Mov B$ErrorLevel 9
        Mov D$EntryPointLabelLen ecx | On ecx = 0, error BadEntryDef
    Pop ecx, edi, esi
ret
________________________________________________________________________________________________

[InsideBrackets: D$ ?]

CheckBracketsPairing:
    Mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    Mov B$InsideBrackets &FALSE

    .While esi < ecx
        .If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        .Else_If B$esi = '['
            If B$InsideBrackets = &TRUE
                jmp L9>
            Else
                Mov edx esi
                Mov B$InsideBrackets &TRUE
            End_If
        .Else_If B$esi = ']'
            If B$InsideBrackets = &TRUE
                Mov B$InsideBrackets &FALSE
            Else
                Mov edx esi
                jmp L9>
            End_If
        .End_If
        inc esi
    .End_While

    On B$InsideBrackets = &TRUE, jmp L9>

L8: ret

L9: ;ERROR! Unpaired bracket
    Mov esi edx
    While B$esi > LF | dec esi | End_While
    Mov edi CookedErrorMessage
    While B$esi <> CR
        movsb | On edi = EndOfCookedErrorMessage, jmp L2>
    End_While
L2: Mov B$edi 0

    Mov B$Errorlevel 9 | error D$OrphanBracketPtr
ret


L8: ;ERROR! Unpaired open/close-sign
    Mov esi edx
    While B$esi > LF | dec esi | End_While
    Mov edi CookedErrorMessage
    While B$esi <> CR
        movsb | On edi = EndOfCookedErrorMessage, jmp L2>
    End_While
L2: Mov B$edi 0

    Mov B$Errorlevel 9 | error D$ParenthesisPtr
ret
________________________________________________________________________________________________

[VirtualBracketsCount: D$ ?
 FirstBracket: D$ ?
 LastBracket: D$ ?
 FirstVirtualBracket: D$ ?
 LastVirtualBracket: D$ ?
 FirstParenthesis: D$ ?
 LastParenthesis: D$ ?]

CheckPairings:
    Mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    Mov B$InsideBrackets &FALSE, edx esi
    Mov D$OpenSignsCount 0, D$VirtualBracketsCount 0

    .While esi < ecx
        Mov al B$esi

        .If al = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign

        .Else_If al = '['
            If B$InsideBrackets = &TRUE
                jmp L9>>
            Else_If D$VirtualBracketsCount <> 0
                jmp L9>>
            Else_If D$OpenSignsCount <> 0
                jmp L9>>
            Else
                Mov B$InsideBrackets &TRUE, D$FirstBracket esi
            End_If

        .Else_If al = ']'
            Mov D$LastBracket esi
            If D$VirtualBracketsCount <> 0
                jmp L9>>
            Else_If B$OpenSignsCount <> 0
                jmp L9>>
            Else_If B$InsideBrackets = &FALSE
                jmp L9>>
            End_If
            Mov B$InsideBrackets &FALSE

        .Else_If al = '{'
            Mov D$FirstVirtualBracket esi
            If B$InsideBrackets = &FALSE
                On D$VirtualBracketsCount > 0, jmp L9>> ; <<<<<<<<<<<<<<<<<<<<
            End_If
            inc D$VirtualBracketsCount

        .Else_If al = '}'
            Mov D$LastVirtualBracket esi
            dec D$VirtualBracketsCount | On D$VirtualBracketsCount = 0-1, jmp L9>>

        .Else_If al = '('
            On B$OpenSignsCount = 0, Mov D$FirstParenthesis esi
            inc B$OpenSignsCount

        .Else_If al = ')'
            Mov D$LastParenthesis esi
            dec D$OpenSignsCount | On D$OpenSignsCount = 0-1, jmp L9>>
        .End_If

        inc esi
    .End_While

    If B$InsideBrackets = &TRUE
        Mov B$esi '[' | jmp L9>
    Else_If D$VirtualBracketsCount <> 0
        Mov B$esi '{' | jmp L9>
    Else_If B$OpenSignsCount <> 0
        Mov B$esi ')' | jmp L9>
    End_If
ret

L9: ; Pointing the unpairing error:
    Push esi
        .If B$esi = '['
            If D$VirtualBracketsCount <> 0
                Mov esi D$FirstVirtualBracket
            Else_If D$OpenSignsCount <> 0
                Mov esi D$FirstParenthesis
            Else
                Mov esi D$FirstBracket
            End_If

        .Else_If B$esi = ']'
            If D$VirtualBracketsCount <> 0
                Mov esi D$FirstVirtualBracket
            Else_If D$OpenSignsCount <> 0
                Mov esi D$FirstParenthesis
            Else
                ;
            End_If

        .Else_If B$esi = '{'
            Mov esi D$FirstVirtualBracket
        .Else_If B$esi = '}'
            ;
        .Else_If B$esi = '('
            Mov esi D$FirstParenthesis
        .Else_If B$esi = ')'
            ;
        .End_If

        sub esi D$CodeSourceA | add esi D$CodeSource
        Mov eax esi
        While B$esi > LF | dec esi | End_While | inc esi
        While B$eax > LF | inc eax | End_While | dec eax

        Mov D$LP.BlockStartText esi,
            D$LP.BlockEndText eax,
            D$FL.BlockInside &TRUE

        Mov D$STRUCT.EditData@UpperLine esi

        Call UpOneLine | Call UpOneLine | Call UpOneLine

    Pop esi

  ; Set the Error Message Text:
    .If B$esi = '['
        If D$VirtualBracketsCount <> 0
            Mov eax UnPairedNestedBrackets
        Else_If D$OpenSignsCount <> 0
            Mov eax D$ParenthesisPtr
        Else
            Mov eax D$OrphanBracketPtr
        End_If

    .Else_If B$esi = ']'
        If D$VirtualBracketsCount <> 0
            Mov eax D$UnPairedNestedBracketsPtr
        Else_If D$OpenSignsCount <> 0
            Mov eax D$ParenthesisPtr
        Else
            Mov eax D$OrphanBracketPtr
        End_If

    .Else_If B$esi = '{'
        Mov eax D$UnPairedNestedBracketsPtr

    .Else_If B$esi = '}'
        Mov eax D$UnPairedNestedBracketsPtr

    .Else_If B$esi = '('
        Mov eax D$ParenthesisPtr

    .Else_If B$esi = ')'
        Mov eax D$ParenthesisPtr

    .Else
        Mov eax D$unknownPtr

    .End_If

    Mov edi CookedErrorMessage, esi D$LP.BlockStartText

    While esi < D$LP.BlockEndText

        movsb | On edi = EndOfCookedErrorMessage, jmp L2>

    End_While

L2: Mov B$edi 0

    Mov B$Errorlevel 9 | error eax
ret
____________________________________________________________________________________________

[InsideParaMacro: D$ ?]

ReplaceParaMacrosBrackets:
    Mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    Mov B$InsideBrackets &FALSE, B$InsideParaMacro &FALSE, ebx 0, edx 0

    .While esi < ecx
        lodsb

        ..If al = TextSign
            While B$esi <> TextSign | inc esi | End_While | inc esi
        ..Else_If al = '['
            Mov B$InsideBrackets &TRUE
        ..Else_If al = ']'
            Mov B$InsideBrackets &FALSE
        ..Else_If al = '{'
            .If B$InsideBrackets = &FALSE
                Mov B$esi-1 OpenParaMacro
                Mov B$InsideParaMacro &TRUE
            .Else
                Mov ebx esi | dec ebx
                While B$ebx <> '['
                    dec ebx
                    On B$ebx = '|', jmp L2>
                    On B$ebx = LF, jmp L2>
                    If B$ebx > SPC
                        Mov B$esi-1 OpenParaMacro
                        Mov B$InsideParaMacro &TRUE | jmp L2>
                    End_If
                End_While
            .End_If

        ..Else_If al = '}'
            If B$InsideParaMacro = &TRUE
                Mov B$esi-1 CloseParaMacro
                Mov B$InsideParaMacro &FALSE
            End_If

        ..End_If
L2: .End_While
ret
________________________________________________________________________________________________
; This routine will be moved to Hotparsers at a later time.
[OpenSignsCount: D$ ?]

CheckOpenCloseSignPairing:
    Mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    Mov B$OpenSignsCount 0

    .While esi < ecx
        .If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        .Else_If B$esi = '('
            If B$OpenSignsCount = 0
                Mov edx esi
            End_If
            inc B$OpenSignsCount
        .Else_If B$esi = ')'
            If B$OpenSignsCount = 0
                jmp L9>
            End_If
            dec B$OpenSignsCount
        .End_If
        inc esi
    .End_While
L8: ret

L9: ;ERROR! Unpaired open/close-sign
    Mov esi edx
    While B$esi > LF | dec esi | End_While
    Mov edi CookedErrorMessage
    While B$esi <> CR
        movsb | On edi = EndOfCookedErrorMessage, jmp L2>
    End_While
L2: Mov B$edi 0

    Mov B$Errorlevel 9 | error D$ParenthesisPtr
ret
____________________________________________________________________________________________
________________________________________________________________________________________________
NewCountStatements:
  ; How many statements:
    Mov B$ErrorLevel 0
    Mov esi D$CodeSourceA, D$StatementsCounter 0, D$LinesCounter 0
    Mov B$DontCountNext &FALSE
    Mov ebx esi | add ebx D$StripLen

    .While esi < ebx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign
        .Else_If B$esi = LF
            Mov B$DontCountNext &FALSE
        .Else_If B$esi = '|'
            Mov B$DontCountNext &FALSE
        .Else_If B$esi = '['
            inc D$LinesCounter
            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If
                inc esi
            .Loop_Until B$esi = ']'
            Mov B$DontCountNext &FALSE
        .Else_If B$esi > SPC
            ..If B$esi <> '_'
                If B$DontCountNext = &FALSE
                    inc D$LinesCounter
                    Mov B$DontCountNext &TRUE
                End_If
            ..End_If
        .End_If

        inc esi
    .End_While

; set mem tables:

L9: If D$LinesCounter = 0

        Call CloseProgressBar

        Call 'USER32.MessageBoxA' &NULL,
                                  {B$ "BUAsm can't compile empty files" EOS},
                                  {B$ ' Sorry' EOS},
                                  &MB_OK

        Mov D$FL.CompileErrorHappend &TRUE

        Mov esp D$OldStackPointer | ret ; direct error
       ; Pop eax | ret                  ; Abort, Pop caller and return to Message Loop
    End_If

    Mov eax D$LinesCounter | add eax 20 | shl eax 3  ; 2 > dword +1 > security

    Call VirtualAlloc StatementsTable,
                      eax

    Call VirtualAlloc StatementsTable2,
                      eax

;StoreStatements:

    Mov ecx D$CodeSource | sub ecx D$CodeSourceA    ; Ajust from CodeSource to CodeSourceA.
    Mov esi D$CodeSourceA, edi D$StatementsTable
    Mov B$DontCountNext &FALSE
    Move D$StatementsPtr D$StatementsTable | Move D$edi esi | add D$edi ecx
    Mov ebx esi | add ebx D$StripLen

    .While esi < ebx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign
        .Else_If B$esi = LF
            Mov B$DontCountNext &FALSE
        .Else_If B$esi = '|'
            Mov B$DontCountNext &FALSE
        .Else_If B$esi = '['

            Mov eax esi | add eax ecx | stosd | add D$StatementsPtr 4

            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If

                inc esi
            .Loop_Until B$esi = ']'

            Mov B$DontCountNext &FALSE

        .Else_If B$esi > SPC
            ..If B$esi <> '_'
                If B$DontCountNext = &FALSE
                    Mov eax esi | add eax ecx | stosd | add D$StatementsPtr 4
                    Mov B$DontCountNext &TRUE
                End_If
            ..End_If
        .End_If

        inc esi
    .End_While

    Mov eax 0 | stosd
ret
________________________________________________________________________________________________
________________________________________________________________________________________________
; As of now a '_' outside a Win_equate is the same '' like it is supposed to be according to
; RosAsm help.

StripUnderscore:
    Mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign
        .Else_If B$esi = '&'
            If B$esi+1 > '9'
                Do | movsb | Loop_Until B$esi <= SPC
            End_If
        .Else_If B$esi = '_'
            Do | inc esi | Loop_Until B$esi <> '_'
        .End_If
        movsb
    .End_While

    Mov ecx edi
    sub ecx D$CodeSourceB
    Mov D$StripLen ecx

    Exchange D$CodeSourceA D$CodeSourceB
ret
________________________________________________________________________________________________
; To simplify operations, the source is converted to a special format called MyAscii.

[LowSigns            31
    TextSign            30

  NoSpaceAfterThis    29
    numSign             28   ; #  01C
    IfNumSign           27   ; Substitute of # for the Conditional macros #If, ... 01B

    OpenParaMacro       26   ; { for ParaMacros  01A
  NoSpaceBeforeThis   25
    CloseParaMacro      24   ; } for ParaMacros

    CommaSign           23   ; ,

    OpenVirtual         22   ; [   016 (Macros expanded '[' -{-)
    CloseVirtual        21   ; ]   015 (Macros expanded ']' -}-) 019
    OpenBracket         20   ; [   014
    CloseBracket        19   ; ]   013
; 18, 17 >>> NewOpenBracket / NewCloseBracket
  PartEnds            16
    memMarker           15   ; $ or $  exemple: Mov B$MYVALUE 1
    colonSign           14   ; :
    openSign            13   ; (
    closeSign           12   ; )

  OperatorSigns       11
    addSign             10   ; +
    subSign              9   ; -
    mulSign              8   ; *
    divSign              7   ; /
    expSign              6   ; ^
; 5
  Separators          4
   ; Statement           0FF
    Space               3    ; space
    EOI                 2    ; |  End Of Instruction (separator)
    meEOI               1]   ; |  End Of Instruction in macro expansion
                             ; 0 is used as erase sign inside treatements


[MyAsciiTable: B$ 0 1 2 3 4 5 6 7 8 Space EOI 11 12 EOI 14 15 16 17 18 19 20 21,
 22 23 24 25 26 27 28 29 30 31 Space '!' '"' NumSign memMarker '%' '&' 39 OpenSign,
 CloseSign MulSign AddSign CommaSign SubSign '.' DivSign 48 49 50 51 52 53 54,
 55 56 57 ColonSign ';' '<' 61 '>' '?' '@', ; !!! pas de virgule au départ à chaque !!!
 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90, ;!!! ; (A > Z)
 Openbracket '\' Closebracket expSign 95 96, ;!!!
 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90, ;!!! ; (A > Z)
 '{' EOI '}' 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143, ; !!!
 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164, ; !!!
 165 166 memMarker 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183,
 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204,
 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225,
 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246,
 247 248 249 250 251 252 253 254 255]

TranslateAsciiToMyAscii:
    Mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen, ebx 0
    add ecx D$CodeSourceA

    .While esi < ecx
        If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign

        Else
            Mov bl B$esi
            Mov al B$ebx+MyAsciiTable
            Mov B$esi al

        End_If

        inc esi
    .End_While
ret
________________________________________________________________________________________________

StripUnneededSpaces:
    Mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign | movsb
        .Else_If B$esi = Space
            ..If B$esi+1 = OpenSign
                If B$edi-1 = CloseSign
                    movsb
                Else_If B$edi-1 = CloseParaMacro
                    movsb
                Else_If B$edi-1 < NoSpaceAfterThis
                    inc esi
                Else
                    movsb
                End_If
            ..Else_If B$esi+1 < NoSpaceBeforeThis
                inc esi
            ..Else_If B$edi-1 < NoSpaceAfterThis
                If B$edi-1 = CloseSign
                    movsb
                Else_If B$edi-1 = CloseParaMacro
                    movsb
                Else
                    inc esi
                End_If
            ..Else_If B$esi+1 = '}'
                inc esi
            ..Else_If B$esi-1 = '{'
                inc esi
            ..Else
                movsb
            ..End_If
        .Else
            movsb
        .End_If
    .End_While

    Mov ecx edi | sub ecx D$CodeSourceB | Mov D$StripLen ecx

    Exchange D$CodeSourceA D$CodeSourceB
ret
____________________________________________________________________________________________

; Remaining some valid Comma. Example "Mov eax, -1".

ConvertCommasToSpace:
    Mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    While esi < ecx
        On B$esi = CommaSign, Mov B$esi Space
        inc esi
    End_While
ret
________________________________________________________________________________________________
StripUnneededEOI:
    Mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

; Ensure that the source starts with an EOI
    If B$esi <> EOI
        Mov B$edi EOI
        inc edi
    End_If

    .While esi < ecx
        .If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign | movsb
        .Else_If B$esi = EOI
            ..If B$esi+1 = EOI
                inc esi
            ..Else_If B$esi+1 = CloseBracket
                inc esi
            ..Else_If B$esi+1 = OpenBracket
                If B$edi-1 = CloseBracket
                    inc esi
                Else
                    movsb
                End_If
            ..Else
                movsb
            ..End_If
        .Else
            movsb
        .End_If
    .End_While

    Mov ecx edi
    sub ecx D$CodeSourceB
    Mov D$StripLen ecx

    Mov B$edi EOI,  B$edi+1 EOI | add D$Striplen 2          ; write end mark '||'

    Exchange D$CodeSourceA D$CodeSourceB
ret
____________________________________________________________________________________________

ConvertEOIinBracketsTOmeEOI:
    Mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen, ebx 0 | add ecx D$CodeSourceA

    Mov B$InsideBrackets &FALSE

    .While esi < ecx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign
        .Else_if B$esi = OpenBracket
            Mov B$InsideBrackets &TRUE
        .Else_if B$esi = CloseBracket
            Mov B$InsideBrackets &FALSE
        .Else_if B$esi = EOI
            If B$InsideBrackets = &TRUE
                Mov B$esi meEOI
            End_If
        .End_If
        inc esi
    .End_While
ret
________________________________________________________________________________________________
ConvertTextSigns:
    Mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = '"'
            Mov B$esi TextSign
            Do | inc esi | Loop_Until B$esi = '"'
            Mov B$esi TextSign
        .Else_If B$esi = "'"
            Mov B$esi TextSign
            Do | inc esi | Loop_Until B$esi = "'"
            Mov B$esi TextSign
        .End_If
        inc esi
    .End_While
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  Automatic Labels (created by the Assembler, for example, with the "&0" Macro Key,
  are 8 Bytes Long. Example: ZZZZZZZZ: (See 'NoMeanLabel').
  
  The user cannot make use of this form of Labels.
;;

NoAutomaticLabel:
    Mov esi D$CodeSourceA, edx esi | add edx D$Striplen

    .While esi < edx
        If B$esi = TextSign
            inc esi
            While B$esi <> TextSign | inc esi | End_While
        End_If

        ..If B$esi = 'Z'
            .If B$esi-1 < LowSigns
                If D$esi = 'ZZZZ'
                  ; We have something begining with 'ZZZZ'. Must 8 chars long: 'ZZZZZZZ'
                    cmp B$esi+8 LowSigns | ja L2>

                    cmp B$esi+7 LowSigns | jb L2>
                    cmp B$esi+6 LowSigns | jb L2>
                    cmp B$esi+5 LowSigns | jb L2>
                    cmp B$esi+4 LowSigns | jb L2>

                        Mov B$esi+9 0, B$Errorlevel 9 | error ZZZZreserved, esi

                End_If
            .End_If
        ..End_If

L2:     inc esi
    .End_While
ret
________________________________________________________________________________________________
; The way local symbols are defined in the RosAsm syntax needs to be reviewed. Until that is
; properly done (somewhere in the future) this routine will have to do. :)

[LastFoundLabel: B$ ? # 80]
;;
  Beware: This Routine is called twice:
  
  First time from inside the HotParsers, to expand the 'normal' @Locals
  
  A second time for 'AsmMain', after the Equates and Macros Jobs, to expand the
  @Locals inserted by Macros Evocations.
;;
ExtendLocalSymbols:
    Mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    Mov B$LastFoundLabel 0

    .While esi < ecx
        ...If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign

        ...Else_If B$esi = ColonSign
          ; On '::' don't process (as the label is already stored):
            ..If B$esi-1 < LowSigns
                ; Error holded downward.

            ..Else_If B$esi-1 <> ColonSign
              ; nonlocal label, as local labels are always 2 characters:
                .If B$esi-3 > LowSigns
                    Push esi
                    Mov al 0
                    Do | dec esi | On B$esi = '@', Mov al 1 | Loop_Until B$esi < LowSigns
                    If al = 1
                        Pop esi

                    Else
                        Pop eax
                        inc esi
                        Mov ebx LastFoundLabel
                        Do | lodsb | Mov B$ebx al | inc ebx | Loop_Until B$esi = ColonSign
                        Mov B$ebx 0
                    End_If

                .End_If
            ..End_If

        ...Else_If B$esi = OpenBracket
            Mov ebx esi | inc ebx
            While B$ebx > LowSigns | inc ebx | End_While
            If B$ebx = meEOI
                While B$esi <> CloseBracket | movsb | End_While
            End_If

        ...Else_If B$esi = '@'
            If B$esi-1 < LowSigns
                Mov ebx LastFoundLabel
                While B$ebx <> 0
                    Mov al B$ebx | inc ebx | stosb
                End_While
            Else_if B$esi-2 < LowSigns
                Mov al memMarker | stosb
                Mov ebx LastFoundLabel
                While B$ebx <> 0
                    Mov al B$ebx | inc ebx | stosb
                End_While
            End_If

            Do | movsb | Loop_Until B$esi < LowSigns
        ...End_If

        movsb
    .End_While

    Mov ecx edi | sub ecx D$CodeSourceB | Mov D$StripLen ecx

    Exchange D$CodeSourceA D$CodeSourceB
ret
________________________________________________________________________________________________

[VirtualDataFlag: D$ ?]

IdentifyVirtualData:
    Mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign
        .Else_If B$esi = OpenBracket
            Mov edx esi, B$VirtualDataFlag &FALSE
            .Do
                inc esi
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                Else_If B$esi = '{'
                    Do | inc esi | Loop_Until B$esi = '}'
                Else_If B$esi = '?'
                    Mov B$VirtualDataFlag &TRUE
                End_If
            .Loop_Until B$esi = CloseBracket

            If B$VirtualDataFlag = &TRUE
                Mov B$edx OpenVirtual, B$esi CloseVirtual
            End_If
        .End_If
        inc esi
    .End_While
ret
________________________________________________________________________________________________
; The source in CodeSourceA is reorders to: Data/Equates/Macro, Virtual Data, Code..

ReorderSource:
    Mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    Move D$StatementsPtr D$StatementsTable, D$StatementsPtr2  D$StatementsTable2

; Copy Brackets to CodeSourceB:

  ; this might be needed to skip first EOI if present.
    On B$esi = EOI, inc esi

    .While esi < ecx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign | inc esi

        .Else_If B$esi = OpenBracket
            lea edx D$esi+1

            .Do
                If B$esi = TextSign
                    Do | movsb | Loop_Until B$esi = TextSign
                End_If
                movsb

            .Loop_Until B$esi = CloseBracket
            movsb

            Mov eax D$StatementsPtr, eax D$eax
            Mov ebx D$StatementsPtr2, D$ebx eax
            add D$StatementsPtr2 4 | On D$edx <> 'ZZZZ', add D$StatementsPtr 4

        .Else_If B$esi = OpenVirtual
            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If
                inc esi
            .Loop_Until B$esi = CloseVirtual
            inc esi

            add D$StatementsPtr 4

        .Else_If B$esi = EOI
            If B$esi-1 = CloseBracket
                ; nop
            Else_If B$esi-1 = CloseVirtual
                ; nop
            Else_If B$esi-1 = EOI
                ; nop
            Else
                add D$StatementsPtr 4
            End_If
            inc esi
        .Else
            inc esi
        .End_If
    .End_While

    Mov esi D$CodeSourceA

  ; Copy Virtual to CodeSourceB.
  ; this might be needed to skip first EOI if present.
    If B$esi = EOI
        inc esi
    End_If

    Move D$StatementsPtr D$StatementsTable

    .While esi < ecx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign | inc esi

        .Else_If B$esi = OpenBracket
            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If
                inc esi
            .Loop_Until B$esi = CloseBracket
            inc esi

            add D$StatementsPtr 4

        .Else_If B$esi = OpenVirtual
            lea edx D$esi+1

            .Do
                If B$esi = TextSign
                    Do | movsb | Loop_Until B$esi = TextSign
                End_If
                movsb
            .Loop_Until B$esi = CloseVirtual
            movsb

            Mov eax D$StatementsPtr, eax D$eax
            Mov ebx D$StatementsPtr2, D$ebx eax
            add D$StatementsPtr2 4 | On D$edx <> 'ZZZZ', add D$StatementsPtr 4

        .Else_If B$esi = EOI
            If B$esi-1 = CloseBracket
                ; nop
            Else_If B$esi-1 = CloseVirtual
                ; nop
            Else_If B$esi-1 = EOI
                ; nop
            Else
                add D$StatementsPtr 4
            End_If
            inc esi

        .Else
            inc esi

        .End_If
    .End_While


    Mov B$edi EOI
    inc edi

    Mov esi D$CodeSourceA

  ; Copy the other statements to CodeSourceB.
  ; this might be needed to skip first EOI if present.
    If B$esi = EOI
        inc esi
    End_If

    Move D$StatementsPtr D$StatementsTable

    .While esi < ecx
        .If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign | movsb

        .Else_If B$esi = OpenBracket
            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If
                inc esi
            .Loop_Until B$esi = CloseBracket
            inc esi
            add D$StatementsPtr 4

        .Else_If B$esi = OpenVirtual
            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If
                inc esi
            .Loop_Until B$esi = CloseVirtual
            inc esi
            add D$StatementsPtr 4

        .Else_If B$esi = EOI
            If B$esi-1 = CloseBracket
                ; nop
            Else_If B$esi-1 = CloseVirtual
                ; nop
            Else_If B$esi-1 = EOI
                ; nop
            Else
                Mov eax D$StatementsPtr, eax D$eax
                Mov ebx D$StatementsPtr2
                Mov D$ebx eax

                add D$StatementsPtr 4
                add D$StatementsPtr2 4
            End_If
            movsb

        .Else
            movsb

        .End_If
    .End_While

    Mov ecx edi
    sub ecx D$CodeSourceB
    Mov D$StripLen ecx

    Mov eax D$StatementsPtr2, D$eax 0

    Exchange D$CodeSourceA D$CodeSourceB
    Exchange D$StatementsTable D$StatementsTable2

    Call StripNewlyAddedUnneededEOI
ret
____________________________________________________________________________________________

; called by 'ReorderSource' only. 'StripUnneededEOI' ('HotParsers') is a bit similar.

StripNewlyAddedUnneededEOI:

    Mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign | movsb

        .Else_If B$esi = EOI
            ..If B$esi+1 = EOI
                inc esi
            ..Else
                movsb
            ..End_If

        .Else
            movsb

        .End_If
    .End_While

    Mov ecx edi | sub ecx D$CodeSourceB | Mov D$StripLen ecx

  ; Write the end mark '||':
    Mov B$edi EOI | inc D$Striplen
    If B$edi-1 <> EOI
        Mov B$edi+1 EOI | inc D$Striplen
    End_If

    Exchange D$CodeSourceA D$CodeSourceB
ret
________________________________________________________________________________________________
____________________________________________________________________________________________

NewPrepareExport:

    Call VirtualFree ExportListAPtr

    Call VirtualFree ExportListBPtr

    Call NewHowManyExport

    If B$ExportsectionWanted = &TRUE

        Call NewStoreToExportListA

        Call NewSortExportListA

    End_If
ret


NewHowManyExport:
    Mov B$ExportsectionWanted &FALSE

    Mov esi D$CodeSourceA, ecx esi, ebx 0, edx 0 | add ecx D$StripLen

    .While esi < ecx
        .If W$esi = '::'
            Mov B$ErrorLevel 9
            If B$esi+2 = ':'
                Mov B$esi-1, EOI, B$esi+3 EOI
                error D$WhatIsThisPtr esi
            End_If
            While B$esi-1 = SPC | Mov B$esi-1 ':' | Mov B$esi+1 SPC |dec esi | End_While ; jE! crack spaces
            While B$esi-1 > SPC | dec esi | End_While
            While B$esi <> ':' | inc esi | inc ebx | End_While
            inc ebx
            inc edx
        .Else_If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign
       ; .Else_If B$esi = '['
       ;     .Do
       ;         If B$esi = TextSign
       ;             Do | inc esi | Loop_Until B$esi = TextSign
       ;         End_If
       ;         inc esi
       ;     .Loop_Until B$esi = ']'
        .End_If
        inc esi
    .End_While

    If edx > 0
        Mov B$ExportsectionWanted &TRUE
        Mov D$NumberOfExportedFunctions edx
        add ebx 40                           ; Header
        add ebx 255                          ; name room
        shl edx 4                            ; pointers > n*16 (*10 would be enough..)
        add ebx edx
        Mov D$ExportSectionLen ebx

        Call VirtualAlloc ExportListAPtr,
                          ebx

        Call VirtualAlloc ExportListBPtr,
                          ebx

    End_If

ret


NewStoreToExportListA:
    Mov esi D$CodeSourceA, edi D$ExportListAPtr
    Mov ecx esi | add ecx D$StripLen

    ..While esi < ecx
        ..If W$esi = '::'
            While B$esi-1 > SPC | dec esi | End_While
            If B$esi = '@'
                Mov B$esi-1 EOI
                While B$esi <> ':' | inc esi | End_While | Mov B$esi+2 EOI
                Mov B$ErrorLevel 9
                Error BadLabel, esi
            End_If
            While B$esi <> ':' | movsb | End_While
            Mov al EOI | stosb

        ..Else_If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign

        ..Else_If B$esi = '['
            .Do
                .If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                .Else_If W$esi = '::'
                    While B$esi-1 > SPC | dec esi | End_While
                    If B$esi = '@'
                        Mov B$esi-1 EOI
                        While B$esi <> ':' | inc esi | End_While | Mov B$esi+2 EOI
                        Mov B$ErrorLevel 9
                        Error BadLabel, esi
                    End_If
                    On B$esi = '[', inc esi
                    While B$esi <> ':' | movsb | End_While | movsb
                    Mov al EOI | stosb
                    Mov B$esi Space
                .End_If
                inc esi
            .Loop_Until B$esi = ']'

        ..End_If
        inc esi
    ..End_While
ret


; EvilBro: I haven't actually rewritten this routine yet.
NewSortExportListA:
    Mov edi D$ExportListBPtr, ecx D$NumberOfExportedFunctions

L0: Push ecx
        Mov esi D$ExportListAPtr, ecx D$NumberOfExportedFunctions, edx 0, bl 0FF

L1:     lodsb
        .If al = 0FF
            ; nop
        .Else_If al < bl
            Mov bl al | lea edx D$esi-1
        .Else_If al = bl
            Push ebx
                Push edx, esi
                    While al = bl
                        lodsb | inc edx | Mov bl B$edx
                        cmp al EOI | je L2>
                    End_While
L2:             Pop esi, edx
                On al < bl, lea edx D$esi-1
            Pop ebx
        .End_If

        While B$esi > EOI
            inc esi
        End_While
        inc esi | loop L1<

        If edx > 0
            Mov esi edx
            While B$esi > EOI
                movsb | Mov B$esi-1 0FF
            End_While
            Mov al EOI | stosb
        End_If

    Pop ecx | dec ecx | cmp ecx 0 | ja L0<<

    Exchange D$ExportListAPtr D$ExportListBPtr
    Mov edi D$ExportListBPtr, eax 0 | stosd | stosd | stosd | stosd | stosd
    Mov eax D$NumberOfExportedFunctions | stosd | stosd
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; This shouldn't be here, but is now for development purposes.

FromDataToStructure:
    Mov D$DisScale 4, D$EquateValue 0

    Call 'USER32.GetDlgItemTextA' D$H.DataToStructureDialog, 10, D$DataTextTable, 01000
    On eax < 10, ret

    Mov B$WeAreInTheCodeBox &TRUE
;    Push D$CodeSource, D$SourceLen, D$SourceEnd
        Mov eax esp,
            D$OldStackPointer eax,
            D$FL.CompileErrorHappend &FALSE

        Mov eax D$DataTextTable
        While B$eax > 0
            inc eax
        End_While
        Mov B$eax CR, B$eax+1 LF | add eax 2
        inc eax

        sub eax D$DataTextTable
        Push eax
            Call GetAsmTables
        Pop eax
        Call NewCopyToCodeSourceA D$DataTextTable, eax ; D$DataTextTableLen

        Call Coolparsers

        Call NewCountStatements

        On D$FL.CompileErrorHappend = &TRUE jmp L9>>

        Call Hotparsers | On D$FL.CompileErrorHappend = &TRUE jmp L9>>


        Mov esi D$CodeSourceA, edi D$StructureTextTable, D$FirstDataLabel 0

        On B$esi = OpenBracket, inc esi
        Mov B$edi '[' | inc edi

L0:     .While B$esi > EOI
            Mov ebx esi
            While B$ebx > LowSigns | inc ebx | End_While
            .If B$ebx = ColonSign
                On D$FirstDataLabel = 0, Mov D$FirstDataLabel esi
                While B$esi <> ColonSign | movsb | End_While
                Mov B$edi SPC | inc edi
                Mov eax D$EquateValue | Call WriteEax
                Mov B$edi CR, B$edi+1 LF, B$edi+2 SPC | add edi 3
            .Else_If B$ebx = MemMarker
                If B$ebx-1 = 'D'
                    Mov D$DisScale 4
                Else_If B$ebx-1 = 'W'
                    Mov D$DisScale 2
                Else_If B$ebx-1 = 'U'
                    Mov D$DisScale 2
                Else_If B$ebx-1 = 'B'
                    Mov D$DisScale 1
                Else_If B$ebx-1 = 'Q'
                    Mov D$DisScale 8
                Else_If B$ebx-1 = 'R'
                    Mov D$DisScale 8
                Else_If B$ebx-1 = 'F'
                    Mov D$DisScale 4
                Else_If B$ebx-1 = 'T'
                    Mov D$DisScale 10
                End_If
            .Else_If B$ebx = NumSign
                inc esi
                If B$esi = '0'
                    Call TranslateHexa
                Else
                    Call TranslateDecimal
                End_If
                mul D$DisScale | sub eax D$DisScale | add D$EquateValue eax | jmp L1>>
            .Else_If B$esi = '?'
                Mov eax D$DisScale | add D$EquateValue eax
            .Else_If B$esi < '0'

            .Else_If B$esi > '9'

            .Else
                Mov eax D$DisScale | add D$EquateValue eax
                While B$esi > LowSigns | inc esi | End_While
            .End_If
            inc esi
L1:     .End_While
        On D$FirstDataLabel = 0, jmp L9>

        inc esi | cmp B$esi EOI | ja L0<<

L2:     Mov B$edi CR, B$edi+1 LF | add edi 2
        Mov esi D$FirstDataLabel
        While B$esi <> ColonSign
            On B$esi = 0, jmp L9>
            movsb
        End_While
        Mov D$edi 'SIZE', B$edi+4 SPC | add edi 5
        Mov eax D$EquateValue | Call WriteEax
        Mov B$edi ']', B$edi+1 0
        Call 'USER32.SetDlgItemTextA' D$H.DataToStructureDialog, 11, D$StructureTextTable

L9:     Call ReleaseAsmTables

;    Pop D$SourceEnd, D$SourceLen, D$CodeSource
    Mov B$WeAreInTheCodeBox &FALSE
ret

____________________________________________________________________________________________

EncodeDecode:
    Mov B$WeAreInTheCodeBox &TRUE
;    Push D$CodeSource, D$SourceLen, D$SourceEnd
      ; ('AsmMain' 'OutOnError')
        Mov D$OldStackPointer esp,
            D$FL.CompileErrorHappend &FALSE

; What on earth is EncodeSource???
        Mov eax EncodeSource ;, D$CodeSource eax
        While B$eax > 0
            inc eax
        End_While
        Mov B$eax CR, B$eax+1 LF | add eax 2
        inc eax

;        Mov D$SourceEnd eax |
[EncodeSourceLen: D$ ?]
        sub eax EncodeSource | Mov D$EncodeSourceLen eax

        Call GetAsmTables
        Call NewCopyToCodeSourceA EncodeSource D$EncodeSourceLen

        Call Coolparsers

        Call NewCountStatements

        Call ClearQwordCheckSum

        On D$FL.CompileErrorHappend = &TRUE jmp L9>>

        Call Hotparsers

        On D$FL.CompileErrorHappend = &TRUE jmp L9>>

        Call InitIndex1 | Call InitIndex2

        Exchange D$CodeSourceA D$CodesourceB
        Push D$SourceLen
            Move D$SourceLen D$EncodeSourceLen
            Move D$AsmTablesLength D$SourceLen
            Call ReuseSourceAForCodeList
        Pop D$SourceLen
        Call InitIndex3

        Call BuildData                          ; result 'CodeSourceB' > 'CodeSourceB'

        On D$FL.CompileErrorHappend = &TRUE jmp L9>>

        Call InitDebugIpTable
        Mov B$ErrorLevel 7                      ; For outOnError, Error

        Call ReCodeLine | On D$FL.CompileErrorHappend = &TRUE jmp L9>>

      ; Prepare Text to show in the Code Hexa view:
        Mov esi D$CodeOrigine, edi HexaCodeText
        While esi < D$CodeListPtr
            movzx eax B$esi | inc esi
            Mov ebx eax | shr ebx 4
            and eax 0F | and ebx 0F
            Mov al B$STR.A.Hexa+eax, bl B$STR.A.Hexa+ebx
            shl eax 8 | or eax ebx | or eax 020200000 | stosd
        End_While
        Mov D$edi 0

      ; Disassemble: (DisMain)
        Mov B$DisFlag 0, D$SegmentOverride 0, B$AddressSizeOverride 0
        Mov B$OperandSizeOverride 0, W$DisSizeMarker 'D$'
        Mov B$DisCodeDisplacement &FALSE, B$EscapePrefix &FALSE
        Mov esi D$CodeOrigine, edi DecodeText
L0:     movzx eax B$esi | inc esi | Call D$DisOp1+eax*4
        On B$DisFlag = DISDONE, jmp L0<
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
L0:             movsb | cmp edi DecodeText | jae L0<
            cld
            Mov D$DummyDecodeText 0, D$DummyDecodeText+4 0
        End_If

L9:     Call ReleaseAsmTables

;    Pop D$SourceEnd, D$SourceLen, D$CodeSource
    Mov B$WeAreInTheCodeBox &FALSE

    Mov D$EncodeSecurity 0 ; Ensure the Buffer never overflows
ret


EncodeError:
L0: Mov ebx, esp | cmp ebx, D$OldStackPointer | jnb L1>
    Pop ebx | jmp L0<
L1: sub esp 8 | Call ErrorMessageBox 0, D$ErrorMessagePtr
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; Some routines I haven't got round to deleting. :)

[StripLen: D$ ?
 TextDelimiter: D$ ?
 TextGoingOn: D$ ?]

IsItFirstText:
    cmp B$TextGoingOn &FALSE | je L1>            ; if OFF > test if ON  needed
L0:   cmp al, B$TextDelimiter | jne L9>         ; if ON  > test if OFF needed
        Jmp L3>
L1: cmp al '"' | je L2>
      cmp al "'" | jne L9>
L2: Mov B$TextDelimiter al
L3: Mov al, TextSign | xor B$TextGoingOn &TRUE
L9: cmp B$TextGoingOn &TRUE
ret

IsItText:                                       ; called by many routines after cleaner
    cmp al, TextSign | jne L9>
L2: xor B$TextGoingOn &TRUE
L9: cmp B$TextGoingOn &TRUE
    ret

[IfItIsText | cmp al TextSign | jne M9>         ; macro a bit faster than 'Call IsIttext'
 M0: stosb | lodsb | cmp al TextSign | jne M0<     ; when it fits
    jmp #1
 M9:]


; same as IsItFirstText, but simplified: no error check, no modification of AL. This
; is for other text ruling routines:
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
