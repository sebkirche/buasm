TITLE AsmMain

; 'AsmMain' 'EncodeLines'
 _________________________________________________________________________________________
 _________________________________________________________________________________________

; After Macros and Equates replacement, CodeSourceA is of no more use.
; we reused it for CodeList storage table:

; EvilBro: This next routine is evil, because it uses D$SourceLen. I will have to do something
; about that in the future.

ReuseSourceAForCodeList:
    Mov eax D$CodeSourceA | and eax 0_FFFF_FF00  ; kill +010 margin in CodeSources tables
    Mov D$CodeList eax, edi eax                ; (needed for alignement)
    Mov ecx D$AsmTablesLength
    shr ecx 2
    push eax, edi                              ; recent add > clean it all for ease of
        Mov eax 0 | rep stosd                  ; read of Hexa PEs.
    pop edi, eax
    add eax 0400 | Mov D$CodeListPtr eax       ; clear header and
ret

;;
 This table is for the Debug routine. Each intruction got a pointer. When searching
 for some instruction in the Debugger, we read EIP reg and compare with this List
 in order to point to the source instruction (after error search with the computed
 number of instruction). This List is not closed after compilation.
;;

[IpTable: ?    IpTablePtr: ?    CodeOrigine: ?]

InitDebugIpTable:
    VirtualFree D$IpTable

    Mov eax D$SourceLen | add eax 01000 | VirtualAlloc IpTable eax
    move D$IpTablePtr D$IpTable, D$CodeOrigine D$CodeListPtr
ret


[Time1: ?  Time2: ?  Time3: ?  Time4: ?  Time5: ?  Time6: ?  Time7: ?]

[Alert: 'Something is overwriting the Resources Pointers!!!', 0
 AlertTitle: 'Internal Error!!!...', 0]

AlertResources:

    Call MessageBox AlertTitle,
                    Alert,
                    &MB_SYSTEMMODAL+&MB_USERICON

ret


AsmMain:
    Mov D$EntryPointLabel 'MAIN', B$EntryPointLabel+4 0, D$EntryPointLabelLen 4

    On D$ResourcePointersSecurity <> 0, jmp AlertResources

    Mov B$CompileErrorHappend &TRUE | On B$SourceReady = &FALSE, ret
    Mov eax D$CodeSource | On D$SourceEnd = eax, ret

    Call 'KERNEL32.GetTickCount' | Mov D$Time1 eax

    Mov eax esp, D$OldStackPointer eax         ; To restore stack on error jump

    Mov D$NoMeanLabel 'ZZZZ', D$NoMeanLabel+4 'ZZZZ'
    Mov B$CompileErrorHappend &FALSE, B$FirstPass &True

    If B$WeAreUnfolding = &FALSE
        Call InitProgressBar | Call InitProgressSteps 16, 1
    End_If

  ; Ensure Source is ended with at least one CR/LF for ease of line analyzes:
    Mov eax D$SourceEnd | Mov bx W$eax-2
    cmp bx CRLF | je L1>
      cmp bh CR | jne L0>
          Mov B$eax LF | inc D$SourceEnd | inc D$SourceLen | jmp L1>
L0: Mov W$eax CRLF | add D$SourceEnd 2 | add D$SourceLen 2

L1: Call GetAsmTables                       ; files ready for read in 'CodeSource'
    Call ClearUserStubMainData
    Call BarProgress

; -------------------------------  First Parsers Jobs ---------------------------------

    If B$ProfilerFlag = &TRUE
        Call InjectedCopyToCodeSourceA, D$CodeSource, D$SourceLen
    Else
        Call NewCopyToCodeSourceA D$CodeSource, D$SourceLen
    End_If

    Call CoolParsers
;jmp L7>>
    Call NewCountStatements | Call BarProgress

    Call NewPrepareExport

    Call HotParsers

    Call NoAutomaticLabel

;jmp L7>>
;;
 The Source is no longer a crude Ascii but a cooked translation.
 For example, spaces are no longer ' ' (020), but 'Space' (03), and so on.
 See 'LowSigns' and 'MyAsciiTable'.
;;
    Call ReplaceWin32Equates
;jmp L7>>

    Call InitIndex1

;jmp L7>>
   ;On B$ParseOOA = &TRUE, Call ClassParser     ; <<<<<<<<< Delayed (doesn't work at all).
    On B$ParseEqual = &TRUE, Call EqualParser
;jmp L7>>
    On B$ParseNew = &TRUE, Call NewParser       ; Volunteers private room
    On B$ParseBinInclude = &TRUE, Call BinIncluderParser

; jmp L7>>
    Call BarProgress
    Call 'User32.SendMessageA'  D$hwndForBar, &WM_SETTEXT, 0, Storing
;jmp L7>>
    Call ClearQwordCheckSum
;jmp L7>>

; ------------------------  Start of the Macros and Equates Jobs ------------------------

L1: Call StoreEquatesAndMacros

    Call BarProgress
    Call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, Replacing

    Call NewReplaceMacAndEqu

    Call NewBrackets

    If B$MoreBracket = &TRUE
        Mov B$FirstPass &FALSE
        Call ResetForNewBrackets
        On B$WeAreUnfolding = &TRUE, Call UnfoldOutput
        jmp L1<
    End_If

    On B$WeAreUnfolding = &TRUE, ret

; ------------------------  End of the Macros and Equates Jobs ------------------------

L2: Call ExtendLocalSymbols
;;
  Do not remove this 'Exchange': There must be one, at the end of 'ExtendLocalSymbols',
  because it is also called from inside the 'HotParsers', for the @Locals first Pass.
;;
    Exchange D$CodeSourceA D$CodeSourceB
;;
  Down to here, we alway operated by reading at 'CodeSourceA' and writting at
  'CodeSourceB'. These manipulation are over: From here, we take source in
  'CodeSourceB', and will reuse CodeSourceA for building the Binary
;;

    Call InitIndex2

    Call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, BuildingImport
    Call BarProgress

    Call ReuseSourceAForCodeList ;| jmp L7>>

    Call BuildImport

;jmp L7>>
    Call InitIndex3
    Call BarProgress
    Call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, BuildingRsrc

    Call BuildRsrc

    Call 'KERNEL32.GetTickCount' | Mov D$Time2 eax
    Call BarProgress
    Call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, BuildingData


    Call BuildData                         ; result 'CodeSourceB' > 'CodeSourceB'

    Call BarProgress
    Call InitDebugIpTable
    Call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, Encoding

    If B$ShortenJumpsWanted = &TRUE
        Call InitShortenJmpsTable
        Call EncodeLines
        Call JmpsOptimize
        Call ReleaseShortenJmpsTable

    Else_If B$ProfilerFlag = &TRUE
        Call EncodeLines
        Call CreateProfilerTables

    Else
        Call EncodeLines                       ; Line > Para > Code > op
    End_If

;jmp L7>>
    Call 'KERNEL32.GetTickCount' | Mov D$Time3 eax
    Call BarProgress
    Call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, BuildingHeader

    Call PreparePeHeader | Call FixTableSizes

    Call BarProgress
    Call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, Fixing

    Call BuildRelocationAndFillSymbols

    If B$ExportsectionWanted = &TRUE
        Call PrepareDllVariables | Call FillExportSection
    End_If

    Call WritePeHeaders

    Call BarProgress
    Call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, Writing

    Call 'KERNEL32.GetTickCount' | Mov D$Time4 eax

    Call CloseProgressBar

    If B$ShowStats = &TRUE
        Mov D$UnusedSymbolsDialogWanted &FALSE
        Call 'USER32.DialogBoxParamA', D$H.Instance, 2, D$H.MainWindow, Statistics, 0
    Else
        Call WritePE | Call RecordMRU | On D$BookMarks > 0, Call SaveBookMarks
    End_If

L8:
    On B$CompletionWanted = &TRUE, Call BuildCompletionTable

    If D$UnusedSymbolsDialogWanted = &TRUE
        Call DisplayUnusedSymbolsDialog D$H.Instance
    End_If

    Call ReleaseAsmTables

    If D$ShowTreeHandle <> 0
        On B$AutoRebuildTreeView = &TRUE, Call TreeUpDate
    End_If
   ;Call TestStatementsTable
ret

  ; For developments tests only (comments at "WriteDestination:"):
L7: Call SetTestSavingName |  Call WriteDestination | Call CloseProgressBar | jmp L8<<


QuickOut:    Call CloseProgressBar | Call ReleaseAsmTables | ret

____________________________________________________________________________________________
____________________________________________________________________________________________

[MultipleCompileFindHandle: ?    OneOfMultiplePathNamePointer: ?]

[MultipleCompilePathTitle: 'For Multiple Compilations Test', 0]

[MultipleCompilePath: B$ ? #&MAXPATH]


MultipleCompileTests:
    Call BrowseForFolder D$H.MainWindow, MultipleCompilePathTitle
    On B$BrowseForFolderAborted = &TRUE, ret

    Mov esi FolderPath, edi MultipleCompilePath
    While B$esi <> 0 | movsb | End_While | Mov B$edi '\' | inc edi
    Mov D$OneOfMultiplePathNamePointer edi

    Mov edi FolderPath | While B$edi <> 0 | inc edi | End_While
    Mov D$edi '\*.e', D$edi+4 'xe'

    Call 'KERNEL32.FindFirstFileA' FolderPath, FindFile

    .If eax <> &INVALID_HANDLE_VALUE
        Mov D$MultipleCompileFindHandle eax

L1:     Mov esi FindFile.cFileName, edi D$OneOfMultiplePathNamePointer
        While B$esi <> 0 | movsb | End_While | movsb

        Mov esi MultipleCompilePath, edi SaveFilter
        While D$esi <> 0 | movsb | End_While | movsb

        Call DirectLoad

       Call MessageBox {B$ "Ready to Compile:" EOS},
                       SaveFilter,
                       &MB_SYSTEMMODAL+&MB_USERICON

        Call AsmMain | Mov D$OldStackPointer 0
        On B$CompileErrorHappend = &TRUE, jmp L9>

        Call 'KERNEL32.FindNextFileA' D$MultipleCompileFindHandle, FindFile
        cmp eax &TRUE | je L1<
    .End_If

L9: Call 'KERNEL32.FindClose' D$MultipleCompileFindHandle
ret
____________________________________________________________________________________________









