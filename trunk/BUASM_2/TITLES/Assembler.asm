TITLE Assembler

;;

  'EncodeLines' 'NewSearchApiName'
____________________________________________________________________________________________
____________________________________________________________________________________________

 This part holds the various Routines envolved in the general Assembly process. No
 overall description can be given for them. The simpler way to understand the Assembler
 organisation is to start at 'AsmMain', and to follow up with Right-Click.

 At a TITLE point of vue, the Assembler is dispatched into 4 TITLEs:
 
 [Parsers] / [Assembler] / [AsmMain] / [Encoder]


 Just a quick description:
 
 Encodage of an instruction is done in 4 steps.

 In the first one, parameters are analyzed by '...ParameterAnalyze's routines. They
 tell us how many parameters, what kind of parameter each and if they are 'complex'
 (for exemple if SIB is needed); all needed variables and flags are set.

 In 'Encode', (second step) we do nothing else than 'routing' the more stupid way
 according with mnemonics and parameters. I did this this way to give the easiest
 access for new mnemonics adding. Killing to write but blazing fast and simple to
 maintain.

 When jumping out 'Encode', a third party performs ending fitting controls and writes
 true code in CodeList. This is this collection of small routines which names discribe
 the critical Intel significants like 'op_op_modReg2Rm_imm8'. I cut encodage itself
 in second and third steps just to make source shorter but these two steps are, in fact,
 one and same operation.

 At the very end of assembly job, a 'FillSymbol' routine fullfills the zeroed part of
 code with whished labels' adresses, either absolute or relative (at coding time these
 values may be unknown, and we work one pass. RosAsm is, as far as i know, the only
 one assembler that do it this way, with respect of user defined sizes).
____________________________________________________________________________________________
____________________________________________________________________________________________
;;

; some basic equates:
                    ; > (As concerned tables have 3 dwords per pointed resource),
                    ; > Allow 100 differents...

[MAXDIALOG 2100]     ; ... Dialogs.
[MAXMENU 2100]       ; ... Menus.
[MAXBITMAP 2100]     ; ... BitMaps
[MAXSTRINGS 2100]
[MAXFONT 2100]
[MAXWAVE 2100]
[MAXAVI 2100]
[MAXRCDATA 2100]
[MAXCURSOR 2100]
[MAXICON 2100]

[MAXRESOURCE 3500]   ; Allow, as a whole, 1000 different resources in one PE


[LINKERDEFAULT 0400000, DRIVERDEFAULT 010000][LinkerDllDefault: 0_1000_0000][RelocsWanted: D$ 0]

[PageSize 01000
 ;PageMask 00_1111_0000_0000_0000   ; to check if a pointer reach next page
 PageMask 0FFFF_F000
 ByteHighbit  080
 WordHighbit  08000
 dWordHighBit 08000_0000]

; For Encoding of reg field:
[regEax  00_0000  regAx   00_0000  regAl   00_0000  regEcx  00_0001  regCx   00_0001
 regCl   00_0001  regEdx  00_0010  regDx   00_0010  regDl   00_0010  regEbx  00_0011
 regBx   00_0011  regBl   00_0011  regEsp  00_0100  regSp   00_0100  regAh   00_0100
 regEbp  00_0101  regBp   00_0101  regCh   00_0101  regEsi  00_0110  regSi   00_0110
 regDh   00_0110  regEdi  00_0111  regDi   00_0111  regBh   00_0111]

; For Encoding of the Segment Register (sreg) Field:
[regEs  0000_0000  regCs  0000_1000  regSs  0001_0000  regDs  0001_1000
 regFs  0010_0000  regGs  0010_1000]

; For Encoding of Special-Purpose Register (eee) Field
; (Control Registers and Debug Registers cannot be used in applications. There need
;  privilege 0 and are for system - not implemented in 'Encode:' Mov to/from Debug/Control
;  registers))
[regCr0  00_0000  regCr2  00_0010  regCr3  00_0011  regCr4  00_0100
 regDr0  00_0000  regDr1  00_0001  regDr2  00_0010
 regDr3  00_0011  regDr6  00_0110  regDr7  00_0111]

; For encoding of FPU regs:
[regST0 0  regST1 1  regST2 2  regST3 3  regST4 4  regST5 5  regST6 6  regST7 7]

; For encoding of MMX registers:
[regMM0 0  regMM1 1  regMM2 2  regMM3 3  regMM4 4  regMM5 5  regMM6 6  regMM7 7]


; For Encoding of Operand Size (w) Bit

[ByteSize    0000        ; true wBit, when encoding, can only be 0 (byte size) or
 wordSize    0011        ; 1 (full size). Word / Double size discrimination
 doubleSize  0001        ; will be done by Operand-size override (066 if bit 2 set)
 QuadSize  000100        ; for FPU
 TenSize   001000        ; for FPU
 HalfSize  001111        ; for Packed BCD
 FPUsize   001010        ; for 108 bytes of FPU image > now X$ !!!!!
 OctoSize 0010000        ; for XMM memories
 Xsize        0FF]       ; for 'Fitting' with specific unregular size Opcodes
;;
 following are to fill 'Operands', which is a global image of what operands in a line.
 in some unregular cases, AL, AX and EAX may have specific encodage we can't' guess
 while analysing parameters. having symbols in order allow tests like:
    cmp Operands, RegToMem | ja >L1    instead of:
    cmp operands, RegToMem | je >L1    which solves this problem in case of no use
 of possible Areg by an instruction encodage.
;;
[RegToReg  1
 MemToReg  2
 RegToMem  3
 ImmToReg  4
 ImmToMem  5
 RegToImm  6  ; yes...: for 'out imm8, accum'
 ImmToImm  7  ; for ENTER and OUT
 ;;;  MemToMem  8  ; for FPU ???

 reg   1    ; for general registers
 sReg  2    ; for segment register
 cReg  3    ; for control registers
 dReg  4    ; for debug registers
 imm   5    ; immediate values
 mem   6    ; for memory symbolic adresses (.$ Labels)
 dis   7
 STreg 8    ; for FPU regs
 MMreg 9    ; for MMX regs
 XMMreg 10] ; for XMM regs

[bMem  'B'  ; byte sized memory
 uMem  'U'  ; Unicode string
 wMem  'W'  ; word size memory
 dMem  'D'  ; double word size memory
 qMem  'Q'  ; height bytes size memory
 rMem  'R'  ; height bytes size for FPU real numbers
 tMem  'T'  ; Ten bytes size  for FPU real numbers
 hMem  'H'  ; Height bytes size for FPU real numbers (to keep a while....)
 fMem  'F'  ; Four bytes size for FPU real numbers
 oMem  'O'  ; OctoWords XMM sizes
 xMem  'X'  ; Weird and XMM sizes

 NotFound 0FF]

; Where to jump:
[DownLong   00_1000    ; ja L9>>
 DownShort  00_0100    ; jmp L7>
 UpLong        0010    ; jz L0<<
 UpShort       0001]   ; loop L5<

; Labels flags:

[DataLabelFlag 00_0010
 CodeLabelFlag 00_0100
 DoneFlag      00_0001]

____________________________________________________________________________________________
____________________________________________________________________________________________

; Main asm memories:

[AsmTablesLength: ?]

GetAsmTables:
    ;VirtualFree D$NewWinEquatesMem
    VirtualFree D$IpTable, D$StatementsTable, D$StatementsTable2
    VirtualFree D$PlainLabelList, D$EquateList, D$MacroData

    Call GetResourcesSize

    add eax D$SourceLen | add eax 1_000_000 | add eax D$MemReservation
    Mov D$AsmTablesLength eax
    VirtualAlloc CodeSourceA eax | add D$CodeSourceA 010

    ;mov eax D$SourceLen | add eax 1_000_000 | add eax D$MemReservation
    VirtualAlloc CodeSourceB D$AsmTablesLength | add D$CodeSourceB 010
ret


GetResourcesSize:
    Mov esi CursorList, eax 0 | Call AddResourcesSyze
    Mov esi GroupCursorList | Call AddResourcesSyze
    Mov esi IconList | Call AddResourcesSyze
    Mov esi MenuList | Call AddResourcesSyze
    Mov esi DialogList | Call AddResourcesSyze
    Mov esi StringsList | Call AddResourcesSyze
    Mov esi GroupIconList | Call AddResourcesSyze
    Mov esi WaveList | Call AddResourcesSyze
    Mov esi AviList | Call AddResourcesSyze
    Mov esi RCdataList | Call AddResourcesSyze
    Mov esi BitMapList | Call AddResourcesSyze
ret

AddResourcesSyze:
    While D$esi <> 0
        add eax D$esi+8 | add esi (4*3)
    End_While
ret


; clear memory (CodeList now is a reuse of CodeSourceB):

ReleaseAsmTables:
    VirtualFree D$CodeSourceA, D$CodeSourceB, D$LabelList, D$MacroList,
                D$CodeRef, D$DataRef, D$Relocation, D$ApiListA, D$ApiListB,
                D$DllList
    Mov D$CookedErrorMessage 0

    If B$ProfilerFlag = &TRUE
        Call ReleaseProfilerTables
        Mov B$ProfilerFlag &FALSE
    End_If

ret


ReleaseMainFile:
    VirtualFree D$UserPeStart | Mov D$CodeSource 0
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; Files access:
____________________________________________________________________________________________
____________________________________________________________________________________________

; 'Open File Name' Box:

[uFileFilter: ? #&MAX_PATH] [SaveFilter: ? #&MAX_PATH] [ChoosenFile: ? #&MAX_PATH]

[FilterPtr: 1  OpenFileTitle: 'Choose a file...' 0]

[UserPeStart: ?   UserPeLen: ?   UserPeEnd: ?]

[SourceHandle: 0 SourceLen: 0
 DestinationHandle: 0 DestinationLen: 0
 NumberOfReadBytes: 0
 SourceFile: B$ 'Src.b', 0]  ;;  DestinationFile: 'Dest2.exe', 0, '                       ']

; All the second 0 in lists declarations are for mem size. We need it for 'VirtualFree'

[CodeSource: ? ? CodeSourceA: ? ? CodeSourceB: ? ?]

;;
  Beware: 'CodeSource' is *not* the Memory Chunk Pointer. 'CodeSource' is a Pointer to
  the Source inside 'UserPeStart'. See, for example 'ReleaseMainFile'.
  
  For a direct Allocation/Re-Allocation, it must be, for example:
  > VirtualAlloc UserPeStart 1_000_000 | move D$CodeSource D$UserPeStart
  
  The only case 'CodeSource' is a real Memory Chunk is while disassembling. The switch
  between the two pointer, with the wished release, is done when the Disassembly is 
  finished.
;;

[InstructionAptr: ?  InstructionBptr: ?]

;;
  'MacroList': |NAME|dWord1 dWord2 Byte|
  
  dWord1: Ptr to MacroData // dWord2: Lenght of these Data // Byte: Done Flag
;;

[EquateList: ? ?  MacroData: ? ?  MacroList: ? ?  Relocation: ? ?  DataList: ? ?
 LabelList: ? ?   CodeList: ? ?   Coderef: ? ?    DataRef: ? ?     DllList: ? ?
 ApiListA: ? ?    ApiListB: ? ?]

; 'CodeRef' is: Dword | ... //... | Name | Dword1 dWord2 | // ....       ; 'CodeRefPtr'
;               ....|LabelName|........|LabelName|........|
; Where dWord is the Table Size.
; In the Records, Dword1 is a Pointer to CodeList and dWord2 a Pointer to Source.

; 'LabelList' is: Dword | ... // ...| Name | Dword1 Byte | // ....
; Where dWord is the Table Size.
; In the Records, Dword1 is the Pointer to CodeList, and Byte can be either/or:
; 'CodeLabelFlag', 'DataLabelFlag' , 'DoneFlag'

[DataRefLimit: ?  EquateListLimit: ? MacroListLimit: ? MacroDataLimit: ?
 LabelListLimit: ?]

[EquateListPtr: ?  MacroDataPtr: ?  MacroListPtr: ?  RelocationPtr: ?  DataListPtr: ?
 DataListPtrAtLastColon: ?
 LabelListPtr: ?   CodeListPtr: ?   CoderefPtr: ?    DataRefPtr: ?     DllListPtr: ?
 ApiListAPtr: ?    ApiListBPtr: ?]

[LenOfCode: ?  SourceReady: ?]

[SourceFilterPtr: 1  OpenSourceFileTitle: B$ 'Choose main asm file...' 0
                     OpenPEFileTitle: B$ 'Choose main RosAsm PE file...' 0
                     ChangeNameTitle:    'Change the PE File Name...', 0
                     SaveDlgNameTitle:   'Give the Dialog Template File Name', 0

 SourceFilesFilters: B$ 'Sources'     0  '*.asm'   0
                        'All'         0  '*.*'   0  0]

 ;[PEFilesFilters: B$ 'RosAsm PE'     0  '*.exe;*.scr;*.dll'   0  0]

[PEFilesFilters: B$ 'Files (*.exe, *.dll, *.wll, *.ocx, *.drv, *.bpl, *.cpl, *.fon, *.mpd, *.vbx, *.vxd, *.sys)', 0

'*.exe;*.dll;*.wll;*.ocx;*.drv;*.bpl;*.cpl;*.fon;*.mpd;*.vbx;*.vxd;*.sys', 0
                 B$ 'Executable Files (*.exe)', 0 '*.exe', 0
                 B$ 'Dll Files (*.dll, *.wll)', 0 '*.dll;*.wll', 0
                 B$ 'Delphi Dll Files (*.bpl)', 0 '*.bpl', 0
                 B$ 'Ocx Files (*.ocx)', 0 '*.ocx', 0
                 B$ 'Driver Files (*.drv)', 0 '*.drv', 0
                 B$ 'Cpl Files (*.cpl)', 0 '*.cpl', 0
                 B$ 'fon Files(*.fon)', 0 '*.fon', 0
                 B$ 'mpd Files(*.mpd)', 0 '*.mpd', 0
                 B$ 'vbx Files(*.ocx)', 0 '*.ocx', 0
                 B$ 'vbx Files(*.vbx)', 0 '*.vbx', 0
                 B$ 'vxd Files(*.vxd)', 0 '*.vxd', 0
                 B$ 'sys Files(*.sys)', 0 '*.sys', 0
                 B$ 'All Files', 0  '*.*', 0 0]

[OpenSourceStruc:  len
 hwndFileOwner: 0  OSSInstance: 0  SourceFilesFilters  uFileFilter  260
 1  SaveFilter  260  ChoosenFile  260  0    ; SaveFilter:  full Path/Name.ext
                                            ; ChoosenFile: only Name.ext
 OpenSourceFileTitle  0281804
 0  0  0  0  0]

[OpenPEStruc: len
 hwndPEFileOwner: 0  OPESInstance: 0  PEFilesFilters  uFileFilter  260
 1 SaveFilter  260  ChoosenFile  260  0
 OpenPEFileTitle  OpenPEStrucFlags: 0281804
 0  0  0  0  0]

; &OFN_NOCHANGEDIR
 _________________________________________________________________________________________


[WindowTitle Trash]

[SetWindowText | Call SetRosAsmWindowText | push WindowTitle | push D$H.MainWindow |
 Call 'USER32.SetWindowTextA']

SetRosAsmWindowText:
    Mov esi STR.A.AppName, edi WindowTitle, ecx 8 | rep movsb
    While D$esi <> ' -V.' | inc esi | End_While
    Mov B$edi ' '| inc edi
    While B$esi <> 0 | movsb | End_While
    Mov D$edi '    '| add edi 4
    Mov esi SaveFilter
    While B$esi <> 0 | movsb | End_While | Mov B$edi 0
ret


;;
  CreateFile is called by 'LoadSrc' 'ReplaceSourceOnly' 'OpenRosAsmPE',
                          'DirectMRUload', 'DirectLoad', 'LastMRULoading',
                          'ReloadForDissassembler'
;;

CreateFile:

    If D$SourceHandle > 0
        push esi
            Call 'KERNEL32.CloseHandle' D$SourceHandle | and D$SourceHandle 0
        pop esi
    End_If

    Call 'KERNEL32.CreateFileA' esi, &GENERIC_READ, &FILE_SHARE_READ,
                                0, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, &NULL


   .If eax = &INVALID_HANDLE_VALUE
        Mov eax D$BusyFileptr | Call MessageBox
        Mov eax &INVALID_HANDLE_VALUE | ret

   .Else
        Mov D$SourceHandle eax

   .End_If

   Call 'KERNEL32.GetFileSize'  eax, 0 | Mov D$SourceLen eax
ret

 _______________________________

; Opening file:

ClearSaveFilter:
   Mov ecx (&MAX_PATH / 4), edi SaveFilter, eax 0 | rep stosd
ret


ClearChoosenFile:
   Mov ecx (&MAX_PATH / 4), edi ChoosenFile, eax 0 | rep stosd
ret


LoadSrc:
    Call CreateFile
  ; > eax = D$SourceLen
    On eax = &INVALID_HANDLE_VALUE, ret

    add eax 1_000_000

    push eax
        VirtualAlloc UserPeStart eax | move D$CodeSource D$UserPeStart
    pop eax

    add eax D$UserPeStart | Align_On PAGESIZE eax | sub eax 32
    Mov D$EndOfSourceMemory eax

    Mov edi D$CodeSource
      Mov ax 0A0D, ecx 5
        rep stosw
          Mov eax edi       ; security for back test like ESI or EDI - 2 (sys page fault)

    Mov D$CodeSource eax, D$NumberOfReadBytes 0

    Call 'KERNEL32.ReadFile' D$SourceHandle eax,                ; eax = mem buffer start
                            D$SourceLen NumberOfReadBytes 0

    Call 'KERNEL32.CloseHandle' D$SourceHandle | Mov D$SourceHandle 0

    Mov edi D$CodeSource | add edi D$SourceLen | Mov eax 0A0D0A0D, ecx 100 | rep stosd
    Mov edi D$CodeSource, ecx 5 | sub edi 10 | rep stosw
    Mov eax D$CodeSource | add eax D$SourceLen | Mov D$SourceEnd eax
  ;  add eax 1_000_000 | Mov D$EndOfSourceMemory eax
ret


; In case user wants to Load a Source Only and Resources are actually available, sends
; a warning Message:

[LosingResources: 'Delete actual Resources?', 0
 WarningTitle: ' Warning:', 0]

LooseResources:
    Mov B$KeepResources &FALSE, eax 0
    or eax D$CursorList | or eax D$IconList | or eax D$MenuList
    or eax D$DialogList | or eax D$StringsList | or eax D$WaveList
    or eax D$AviList | or eax D$RCdataList | or eax D$BitMapList

    On eax = 0, ret

    Mov B$KeepResources &TRUE

    .If D$NoResourcesPE = &FALSE
        Call 'USER32.MessageBoxA' D$H.MainWindow, LosingResources, WarningTitle,
                                 &MB_SYSTEMMODAL__&MB_ICONSTOP__&MB_YESNO
        If eax = &IDYES
            Mov B$KeepResources &FALSE
        End_If
    .Else
        Mov B$KeepResources &FALSE
    .End_If
ret

[LoadSource | Mov esi #1 | Call LoadSrc]


[MainName: B$ ? #&MAXPATH] [KeepResources: ?]

OpenSourceOnly:
    Call ClearSaveFilter | Call 'Comdlg32.GetOpenFileNameA' OpenSourceStruc

    On D$SaveFilter = 0, ret

L0: On B$SourceReady = &TRUE, Call ReleaseMainFile

    VirtualFree D$UserPeStart

    Call ClearBackTable | On B$KeepResources = &FALSE, Call ReleaseResourceMemory

    LoadSource SaveFilter | StoreNameOnly SaveFilter

    Mov B$SourceReady &TRUE | move D$UpperLine D$CodeSource

    Call KillTabs | Call KillTrailingSpaces

    SetWindowText
    move D$SavingExtension D$ExeExtension

    Call StartEdition
ret


[IncludeLen: ?]

IncludeSource:
    Call ClearSaveFilter | Call 'Comdlg32.GetOpenFileNameA' OpenSourceStruc

    On D$SaveFilter = 0, ret

    Call ClearBackTable

    Call 'KERNEL32.CreateFileA' SaveFilter,
                                &GENERIC_READ, &FILE_SHARE_READ,
                                0, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, &NULL
    If eax = &INVALID_HANDLE_VALUE
        Mov eax D$BusyFilePtr | Call MessageBox | ret
    Else
        Mov D$SourceHandle eax
    End_If

    Call 'KERNEL32.GetFileSize' eax, 0 | Mov D$IncludeLen eax

    Call ReMapSourceMemoryIfNeeded D$IncludeLen

    If eax = &IDNO
        Call 'KERNEL32.CloseHandle' D$SourceHandle | Mov D$SourceHandle 0 | ret
    End_If

  ; Ensure the inclusion will not break an existing line in two parts:
    Mov edi D$CurrentWritingPos
    While B$edi-1 <> LF | inc edi | inc D$CurrentWritingPos |  End_While

  ; Make room inside actual Source by copying backward:
  ; esi at Source End // edi at Source End + whished room:
    Mov esi D$SourceEnd | add esi 400
    Mov edi esi | add edi D$IncludeLen | add edi 2      ; this '2' is for added CRLF at end.

  ; How many bytes to move = how many bytes between Source End and actual Pos:
    Mov ecx D$SourceEnd | add ecx 400 | sub ecx D$CurrentWritingPos | inc ecx

    std | rep movsb | cld

    move D$BlockStartTextPtr D$CurrentWritingPos

    Mov D$NumberOfReadBytes 0
    Call 'KERNEL32.ReadFile' D$SourceHandle, D$BlockStartTextPtr, D$IncludeLen, NumberOfReadBytes, 0
    Call 'KERNEL32.CloseHandle' D$SourceHandle | Mov D$SourceHandle 0

    Mov edi D$CurrentWritingPos | add edi D$IncludeLen | Mov W$edi CRLF
    Mov D$BlockEndTextPtr edi | dec D$BlockEndTextPtr

    Mov eax D$IncludeLen | add eax 2 | add D$SourceLen eax | add D$SourceEnd eax
    Mov B$BlockInside &TRUE

    Mov esi D$BlockStartTextPtr, ecx D$BlockEndTextPtr | sub ecx esi
    Call BlockCleaner
ret


[OpeningSourceOnly: ?]

ReplaceSourceOnly:  ; 'ControlS'
    Mov B$KeepResources &TRUE

    ..If D$TitleTable = 0
L1:     Call OpenSourceOnly | Call LoadBookMarks

    ..Else
        Mov B$OpeningSourceOnly &TRUE
      ; Tag Dialog 26001
        Call 'USER32.DialogBoxParamA' D$hinstance, 26001, &NULL, AllOrPartProc, &NULL
        Mov B$OpeningSourceOnly &FALSE

        .If B$AllOrPart = 0-1
            Call RestoreRealSource | Mov D$TiTleTable 0 | jmp L1<

        .Else_If B$AllOrPart = 1   ; 'OpenSourceOnly'
            Call ClearSaveFilter | Call 'Comdlg32.GetOpenFileNameA' OpenSourceStruc
            If D$SaveFilter <> 0
                Mov esi SaveFilter | Call CreateFile | On eax = &INVALID_HANDLE_VALUE, ret
                Mov eax D$CodeSource | add eax D$SourceLen | Mov D$SourceEnd eax
                Mov D$NumberOfReadBytes 0
                Call 'KERNEL32.ReadFile' D$SourceHandle, D$CodeSource, D$SourceLen,
                                         NumberOfReadBytes 0

                Call 'KERNEL32.CloseHandle' D$SourceHandle | Mov D$SourceHandle 0

                Call KillTabs | Call KillTrailingSpaces
            End_If
        .End_If

    ..End_If
ret


; In case of 'New' Internaly called because user attempted to load a 'no source inside'
; PE, we change the Saving File Name to 'NoName.exe':

[NoNameNewFile: 'NoName.exe' 0]
[NewFileBoxTitle: 'New File Name', 0]

NewFileNameDialog:
    Call GetDirectory ActualDir

  ; Tag Dialog 5
    Call 'USER32.DialogBoxParamA' D$hInstance, 5, D$H.MainWindow, NewFileType, 0

    Mov eax D$FileTypeChoice

    .If eax = 0-1
L8:     Call SetDirectory ActualDir | jmp L9>>

    .Else
        Call GetBaseName
X1:     Mov edi D$BaseNamePointer
        Mov ebx ActualDir
        While B$ebx <> 0 | inc ebx | End_While
        Mov B$ebx '\' | inc ebx
        While B$esi <> 0
            Mov al B$esi, B$edi al, B$ebx al
            inc esi, ebx, edi
        End_While
        Mov B$edi 0, B$ebx 0
    .End_If

    Call SetDirectory ActualDir

    Call 'KERNEL32.CopyFileA', BaseFilesPath, ActualDir, &TRUE

    ..If eax = &FALSE
        Call 'KERNEL32.GetLastError'

        .If eax = &ERROR_FILE_EXISTS

            Call 'USER32.MessageBoxA' &NULL, {'Overwrite?', 0},
                                    {'File already exist', 0}, &MB_YESNO
            If eax = &IDYES
                Call 'KERNEL32.CopyFileA', BaseFilesPath, ActualDir, &FALSE

            Else
                jmp L9>>
            End_If

        .Else
            jmp L8<<

        .End_If
    ..End_If

    Mov esi ActualDir, edi SaveFilter
    While B$esi <> 0 | movsb | End_While | movsb

    Call DirectLoad
    and D$RelocsWanted 0 ; jE!
    Call ChangeName
    cmp D$SavingExtension '.DLL' | setz B$RelocsWanted ; jE! Relocs for Dll
    Mov B$SourceReady &TRUE
    Call AskForRedraw
    Call ReInitUndo
    Call SetPartialEditionFromPos | Call EnableMenutems
    Call LoadBookMarks
L9: ret


ChangeName:
    or D$OpenPEStrucFlags &OFN_OVERWRITEPROMPT
    Mov D$OpenPEStruc+(12*4) NewFileBoxTitle
    Call 'Comdlg32.GetSaveFileNameA' OpenPEStruc
    Mov D$OpenPEStruc+(12*4) &NULL
    On eax = &FALSE, ret

    xor D$OpenPEStrucFlags &OFN_OVERWRITEPROMPT | jmp L1>
AutoNew:
    Mov edi SaveFilter, eax 0, ecx 65 | rep stosd


    Mov edi SaveFilter

NewEmptyFromMenu:
    Mov esi NoNameNewFile
    While B$esi > 0
        movsb
    End_While
    movsb

L1: Call SaveNewFileName
    On eax = &FALSE, jmp NewFileNameDialog
    push eax
        SetWindowText
    pop eax
  ; (If here, eax = D$SourceLen)
    xor D$OpenPEStrucFlags &OFN_OVERWRITEPROMPT
    Mov eax &TRUE
ret
____________________________________________________________________________________________

;;
  ID in RcData (Files in E:\RosAsm\Start\):
  
  ID 1: Menu
    10: Low/LowStart/Menu      'LowStartMenu.exe'
    11: Low/LowStart           'LowStart.exe'
    20: StandardMacros
    21:     Standard/StandardStart/Menu
    22:     Standard/StandartStart/
    40: HllMacros <<<<<<<<<<<<<<<<<<<< Removed: Now Standard + PREPARSE Equal.
    41:     HLL/HllStart/Menu
    42:     HLL/HllStart
    50: Dll/Standard or HLL
    51: DLL/Low
;;

[PreParseEqual: "PREPARSE Equal


", 0]

FillNewSource:
    Mov edi D$CodeSource, bl B$FileTypeChoice+1
    If bl = 20
      ; Standart macros:
        Call GetRcData 20 | rep movsb
    Else_If bl = 21
      ; HLL Macros:
        Call GetRcData 20 | rep movsb
        Mov esi PreParseEqual
        While B$esi <> 0 | movsb | End_While
    End_If

L1: Mov al B$FileTypeChoice

      ; exe/Start/Menu:
        .If al = 10
          ; Menu:
            Call GetRcData 1
            push esi, edi, ecx, ebx
                VirtualAlloc Trash, ecx
            pop ebx, ecx, edi, esi
            Mov D$MenuList 2000, D$MenuList+4 eax, D$MenuList+8 ecx
            lea eax D$MenuList+12 | Mov D$MenuListPtr eax

            push edi
                Mov edi D$Trash | rep movsb
            pop edi

            If bl = 20
              ; Standard/StandardStart/Menu
                Call GetRcData 21 | rep movsb
            Else_If bl = 21
              ; HLL/HllStart/Menu
                Call GetRcData 41 | rep movsb
            Else
              ; Low/LowStart/Menu      'LowStartMenu.exe'
                Call GetRcData 10 | rep movsb
            End_If

      ; exe/Start:
        .Else_If al = 11
            If bl = 20
              ; Standard/StandardStart
                Call GetRcData 22 | rep movsb
            Else_If bl = 21
              ; HLL/HllStart
                Call GetRcData 42 | rep movsb
            Else
              ; exe/LowStart
                Call GetRcData 11 | rep movsb
            End_If

      ; exe (empty Source):
        .Else_If al = 12
            On bl = 22, jmp L9>>

      ; dll/StandartStart:
        .Else_If al = 13
            If bl = 22
                Call GetRcData 51
            Else
                Call GetRcData 50
            End_If
            rep movsb

      ; dll (empty Source)
        .Else_If al = 14
            ;
        .End_If

L1: Mov D$edi CRLF2 | add edi 4 | Mov D$SourceEnd edi
    Mov ecx edi | sub ecx D$CodeSource | Mov D$SourceLen ecx

    Mov eax CRLF2, ecx 100 | rep stosd

L9: Call GetEditWindowClientRectangle
    Mov D$TitleTable 0, D$PreviousPartialSourceLen 0

    Mov B$OnReplaceAll &FALSE, B$BlocKInside &FALSE, B$DownSearch &TRUE, B$ReadyToRun &FALSE
    Call StorePosInBackTable

L9: ret


Proc GetRcData:
    Argument @ID
    Local @hwnd
    Uses edi

        Call 'KERNEL32.FindResourceA' D$hInstance, D@ID, &RT_RCDATA
        Mov D@hwnd eax
        Call 'KERNEL32.SizeofResource' D$hInstance, D@hwnd
        push eax
            Call 'KERNEL32.LoadResource' D$hInstance, D@hwnd
            Call 'KERNEL32.LockResource' eax
            Mov esi eax
        pop ecx
EndP
____________________________________________________________________________________________

____________________________________________________________________________________________

[FileTypeChoice: ?  NewChoiceListHandle: ?]

; Tag Dialog 5

Proc NewFileType:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_COMMAND
        ..If D@wParam = &IDCANCEL
L0:         Mov D$FileTypeChoice 0-1
L1:         Call 'User32.EndDialog' D@hwnd 0

        ..Else
            .If W@wParam = 10
                If W@wParam+2 = &LBN_DBLCLK
                    Call 'USER32.SendMessageA' D$NewChoiceListHandle, &LB_GETCURSEL, 0, 0
                    inc eax | Mov D$FileTypeChoice eax | jmp L1<

                End_If

            .End_If

        ..End_If

    ...Else_If D@msg = &WM_INITDIALOG
        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$wc_hIcon

        Call 'USER32.GetDlgItem' D@hwnd, 10 | Mov D$NewChoiceListHandle eax

        Call SetBaseList

    ...Else
        popad | Mov eax &FALSE | ExitP

    ...End_If

    popad | Mov eax &TRUE
EndP


Proc SetNewChoice:
    Arguments @ListHandle, @Pointer

        pushad
            Call 'USER32.SendMessageA' D@ListHandle, &LB_ADDSTRING, 0, D@Pointer
        popad
EndP


[BaseFilesPath: ? #&MAXPATH]

[BaseNamePointer: 0

 BasesNames:
 Base_01.exe: B$ 'Base_01.exe', 0
 Base_02.exe: 'Base_02.exe', 0
 Base_10.exe: 'Base_10.exe', 0
 Base_11.exe: 'Base_11.exe', 0

 Base_03.dll: 'Base_03.dll', 0
 Base_04.dll: 'Base_04.dll', 0
 Base_05.dll: 'Base_05.dll', 0, 0]

[MissBaseTitle: 'Missing Bases Files', 0

 MissBase: "
 Download the Bases Files from RosAsm.org
 and save them in a folder called 'Bases',
 inside your 'RosAsmFiles' Folder.
 
 If you already have these Files at the
 proper Path, update them with the new
 Version.
 ", 0]

[BasesListHandle: ?  BaseListEnd: ?]

SetBaseList:
    ;Call SetNewChoice eax, NewEmpty

    Call GetRosAsmFilesPath

    Mov esi RosAsmFilesPath, edi BaseFilesPath
    While B$esi <> 0 | movsb | End_While
    Mov D$edi 'Base', D$edi+4 's\'
    add edi 6 | Mov D$BaseNamePointer edi

    zCopy {'BasesList.txt', 0} | Mov B$edi 0

    Call 'KERNEL32.CreateFileA' BaseFilesPath, &GENERIC_READ,
                                &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL, 0

    .If eax = &INVALID_HANDLE_VALUE
        Mov eax 0-1 | ret

    .Else
        Mov D$BasesListHandle eax
        Call 'KERNEL32.GetFileSize' eax, 0
        Mov D$BaseListEnd Trash | add D$BaseListEnd eax
        Call 'KERNEL32.ReadFile' D$BasesListHandle, Trash, eax, NumberOfReadBytes, 0
        Call 'KERNEL32.CloseHandle' D$BasesListHandle

        Mov esi Trash
        .While esi < D$BaseListEnd
            While B$esi <> ' ' | inc esi | End_While
            Mov edi TrashString
            While B$esi <> CR | movsb | End_While | Mov B$edi 0 | add esi 2

            Call SetNewChoice D$NewChoiceListHandle, TrashString
        .End_While

    .End_If
ret


GetBaseName:
  ; eax = Indice (Indice zero assumed as 'NewEmpty' by 'SetBaseList').
    Mov esi Trash, ecx 1

  ; Skip lines until indice matches:
    .While ecx < eax
        While B$esi <> CR | inc esi | End_While | add esi 2
        inc ecx
    .End_While

  ; Set a zero at the end of the File Name:
    push esi
        While B$esi <> ' ' | inc esi | End_While
        Mov D$esi 0
    pop esi
ret


VerifyBasesFolder:
    Call GetRosAsmFilesPath

    Mov esi RosAsmFilesPath, edi BaseFilesPath
    While B$esi <> 0 | movsb | End_While
    Mov D$edi 'Base', D$edi+4 's\'
    add edi 6 | Mov D$BaseNamePointer edi
    Mov esi BasesNames

    .While B$esi <> 0
        push edi
            While B$esi <> 0 | movsb | End_While | movsb
            push esi
                Call 'KERNEL32.FindFirstFileA' BaseFilesPath, FindFile
                push eax
                    Call 'KERNEL32.FindClose' eax
                pop eax
                If eax = &INVALID_HANDLE_VALUE
                    pop eax, eax
                    Call 'USER32.MessageBoxA' &NULL, MissBase, MissBaseTitle, &MB_SYSTEMMODAL
                    Mov eax &FALSE | ret
                End_If
            pop esi
        pop edi
    .End_While

    Mov eax &TRUE
ret

____________________________________________________________________________________________

[BadNewExtension: 'Bad Extension', 0]

SaveNewFileName:
    Mov esi SaveFilter, edi MainName

L0: lodsb | stosb | cmp B$esi '.' | je L8>
    cmp B$esi 0 | jne L0<

L8: On B$esi+4 <> 0, jmp L0<

    Mov B$edi 0

  ; Check extension:
    lodsd | or eax 020202000
    If eax = '.exe'
        jmp L9>
    Else_If eax = '.dll'
        jmp L9>
    Else_If eax = '.sys'
        jmp L9>
    Else_If eax = '.scr'
        jmp L9>
    End_If

    If al = '.'
        ; OK
    Else_If al = 0
        ; OK
    Else
        Mov eax BadNewExtension | Call MessageBox | Mov eax &FALSE | ret
    End_If

    sub esi 4 | Mov edi esi
    Mov eax '.exe' | stosd | Mov B$edi 0   ; Complete user given name (any use?...)

L9: or eax 020202000 | xor eax 020202000 | Mov D$SavingExtension eax | Mov eax &TRUE
ret


StartNewFile:
    Call ReInitUndo
    Call ReInitHeaderFormat
    On B$SourceReady = &TRUE, Call ReleaseMainFile

    Call ClearBackTable | Call ReleaseResourceMemory

    Mov D$SourceLen 0

  ;  VirtualAlloc CodeSource 1_000_000
    VirtualAlloc UserPeStart 1_000_000 | move D$CodeSource D$UserPeStart

    add D$CodeSource 10     ; security for back test like ESI or EDI - 2 (no page fault)

    Mov edi D$CodeSource
    Mov eax 0A0D0A0D, ecx 100 | rep stosd                            ; end security tail
    Mov edi D$CodeSource, ecx 5 | sub edi 10 | rep stosw
    Mov eax D$CodeSource
    Mov D$SourceEnd eax, D$CurrentWritingPos eax, D$CaretLine 0, D$CaretRow 1
    add eax 1_000_000 | Mov D$EndOfSourceMemory eax

    Mov B$SourceReady &TRUE | move D$UpperLine D$CodeSource

    Mov D$TitleTable 0, D$PreviousPartialSourceLen 0

  ; Clear possible previous resources:
    Call ClearCustomList

    Call AskForRedraw
ret


[UserPEStartOfResources: ?    ResourcesSize: ?]

SearchPEstartOfResource:
    Mov B$NoResourcesPE &FALSE
    Mov esi D$UserPeStart, D$UserPEStartOfResources 0
    movzx eax W$esi+8
   ; Mov eax 0 | add esi 8 | lodsw    ; parag. size of dos header end >PE header adress
    shl eax 4 | sub eax 4
    Mov esi D$UserPeStart | add esi eax | lodsd      ; eax = PE header

    Mov esi D$UserPeStart | add esi eax
    cmp D$esi 'PE' | jne L9>
 ______________________________________

; read data in PeHeader:

L0: Mov ecx 0 | Mov cx w$esi+6            ; word record of section number
    add esi 136 | lodsd | Mov ebx eax     ; RVA of resources from "Image Data Dir..."
    Mov D$ResourcesRVA eax
    move D$ResourcesSize D$esi

                     ; jmp over general purpose headers and reach PE sections headers:
    add esi 120                           ; esi points to RVA of first section header
    If eax = 0
        Mov B$NoResourcesPE &TRUE | ret
    End_If

L0: lodsd | cmp eax ebx | je L1>
        add esi 36 | loop L0<
          jmp L9>
 ______________________________________

  ; if here, '.rsrc' section found:

L1:
    add esi 4 | lodsd                            ; > app ptr to resources
    add eax D$UserPeStart | Mov D$UserPEStartOfResources eax
L9: ret


[EndOfSectionSearch: ?]

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
 This is a little 'unregular' CheckSum stored in an 'unregular' place. It does *not*
 prevent final user from viruses. It is designed to protect RosAsm programmers who
 exchange files from dispatching corrupted files.

 The base idea is that we can hope that nobody will ever write any virus specifically
 designed to target RosAsm produced PEs. Not at all sophisticated, and very easy to
 overcome, but who would?

 As is, if infected, when you [Load] the file in RosAsm, you get a MessageBox to alert
 you. Nothing more, but i think that it is a very good solution to allow Asm communauty
 to exchange files without fearing.
;;
[CheckMessageTitle: 'Bad CheckSum', 0
 CheckInfo: "
 This File has been modified, either
 by an external Tool, or by a Virus.     ", 0]

ReadCheckSum:
    pushad
      ; If a real PE CheckSum has been written, it must be zeroed, before calculation
      ; because it was stored after 'MyCheckSum':
        Mov esi D$UserPeStart | add esi CheckSum | sub esi DosHeader
        Mov D$esi 0

      ; 'MyCheckSum' must not be considered:
        Mov esi D$UserPeStart | add esi MyCheckSum | sub esi DosHeader
        Mov edx D$esi, D$esi 0, ebx 0, eax 0

        .If edx > 0
            Mov esi D$UserPeStart, ecx D$UserPeLen

L0:         lodsb | add ebx eax | loop L0<

            If edx <> ebx
                Call 'USER32.MessageBoxA' D$H.MainWindow, CheckInfo, CheckMessageTitle,
                                           &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION
            End_If
        .End_If
    popad
ret


WriteCheckSum:
    Mov eax 0, ebx 0, esi D$CodeList, ecx D$LenOfCode
L0: lodsb | add ebx eax | loop L0<

    If B$ExportsectionWanted = &TRUE
        Mov esi D$ExportListBPtr, ecx D$ExportSectionLen
L0:     lodsb | add ebx eax | loop L0<
    End_If

    .If D$SavingExtension = '.SYS'
        jmp L0>
    .Else_If D$RelocsWanted = &TRUE ;SavingExtension = '.DLL' ; jE!
L0:     If D$RelocSectionSize => 8 ;jE!
            Mov esi D$Relocation, ecx D$RelocSectionSize
L0:         lodsb | add ebx eax | loop L0<
        End_If
    .End_If

    Mov esi D$CodeSource, ecx D$SourceLen
L0: lodsb | add ebx eax | loop L0<

    Mov edi D$CodeList | add edi MyCheckSum | sub edi DosHeader
    Mov D$edi ebx
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
  Regular PE CheckSum (original Source for csum.exe, from 'RudeBoy'):
  
  Will be needed for Drivers.


    Mov edx 0, esi D$FilePointer, ecx D$FileLen | shr ecx 1

L0: lodsw
    add edx eax
    Mov eax edx
    and edx 0ffff 
    shr eax 010
    add edx eax
    loop L0<

    Mov eax edx
    shr eax 010
    add ax dx
    add eax dwFileLen
  
____________________________________________________________________________________________

Another one:

    Mov ecx, D$Length, esi D$BufferPointer, eax 0
    shr ecx, 1              

    clc                                 
L0: adc ax W$esi+ecx*2-2 | dec ecx | jnz L0<
    
    adc eax D$Length
    
    
    
    
    MicrosoftCheckSum2              PROC C uses esi, buf:dword, len:dword
        Mov             ecx, [len]           
        Mov             edx, [buf]           
        shr             ecx, 1               
        xor             eax, eax             

        clc                                  
@@theLoop:
        adc ax, [edx + (ecx * 2) - 2]
        dec ecx
        jnz @@theLoop
        adc  eax, [len]
        ret
;;

WriteSysFile:
    Call SetExeSavingName

    Call OpenDestinationFile

    Mov D$NumberOfReadBytes 0

    Call AraseSourceHeader | Call AraseCheckSum

    Call WritePeCheckSum

    Call 'KERNEL32.WriteFile' D$DestinationHandle, D$CodeList, D$LenOfCode,
                              NumberOfReadBytes, 0

    If B$ExportsectionWanted = &TRUE
        Call 'KERNEL32.WriteFile' D$DestinationHandle, D$ExportListBPtr,
                                  D$FileAlignedExportSectionLen, NumberOfReadBytes, 0
    End_If

    If D$SavingExtension = '.SYS'
        jmp L1>
    Else_If D$RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
L1:
        If D$RelocSectionSize => 8 ;jE!
          Call 'KERNEL32.WriteFile' D$DestinationHandle,
                                   D$Relocation, D$FileAlignedRelocationSize,
                                   NumberOfReadBytes, 0
        End_If
    End_If

  ;  Call 'KERNEL32.WriteFile' D$DestinationHandle, D$CodeSource, D$SourceLen,
  ;                            NumberOfReadBytes, 0

    Call 'KERNEL32.CloseHandle' D$DestinationHandle | Mov D$DestinationHandle 0
ret


AraseSourceHeader:
    Mov edi D$LocOfSourceHeader
    Mov ecx 20, eax 0 | rep stosd
ret


AraseCheckSum:
    Mov edi D$CodeList | add edi MyCheckSum | sub edi DosHeader
    Mov D$edi 0
ret


WritePeCheckSum:
    Mov esi D$CodeList, ecx D$LenOfCode, eax 0, ebx ecx
    shr ecx, 1

    clc
L0: adc ax W$esi+ecx*2-2 | dec ecx | jnz L0<
    adc ax 0

    If B$ExportsectionWanted = &TRUE
        Mov esi D$ExportListBPtr, ecx D$FileAlignedExportSectionLen
        add ebx ecx
        shr ecx 1
        clc
L0:     adc ax W$esi+ecx*2-2 | dec ecx | jnz L0<
        adc ax 0
    End_If

    If D$RelocSectionSize => 8 ;jE!
        Mov esi D$Relocation, ecx D$FileAlignedRelocationSize
        add ebx ecx
        shr ecx 1
        clc
L0:     adc ax W$esi+ecx*2-2 | dec ecx | jnz L0<
       ; adc ax 0
    End_If
;;
    Mov esi D$CodeSource, ecx D$SourceLen
        add ebx ecx
        shr ecx 1
        clc
L0:     adc ax W$esi+ecx*2-2 | dec ecx | jnz L0<
;;
    adc eax ebx | Call WriteEaxToPeCheckSum
ret


WriteEaxToPeCheckSum:
    Mov edi D$CodeList | sub edi DosHeader | add edi CheckSum
    Mov D$edi eax
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[BookMarksFileHandle: ?    BookMarksFileLen: ?]

[FindFile:
 FindFile.dwFileAttributes: D$ ?
 FindFile.ftCreationTime.dwLowDateTime: D$ ?
 FindFile.ftCreationTime.dwHighDateTime: D$ ?
 FindFile.ftLastAccessTime.dwLowDateTime: D$ ?
 FindFile.ftLastAccessTime.dwHighDateTime: D$ ?
 FindFile.ftLastWriteTime.dwLowDateTime: D$ ?
 FindFile.ftLastWriteTime.dwHighDateTime: D$ ?
 FindFile.nFileSizeHigh: D$ ?
 FindFile.nFileSizeLow: D$ ?
 FindFile.dwReserved0: D$ ?
 FindFile.dwReserved1: D$ ?]
[FindFile.cFileName: B$ ? #&MAXPATH]
[FindFile.cAlternate: B$ ? #14]

LoadBookMarks:
    Mov D$NumberOfBookMarks 0
    Mov edi SaveFilter, al 0, ecx 0-1 | repne scasb
    While B$edi <> '.'
        dec edi
    End_While
    push D$edi, edi

        Mov D$edi '.BKM'

        Call 'KERNEL32.FindFirstFileA' SaveFilter FindFile
        .If eax = &INVALID_HANDLE_VALUE
            VirtualFree D$BookMarks | pop edi, D$edi | ret
        .Else
            Call 'KERNEL32.FindClose' eax
        .End_If

        Call 'KERNEL32.CreateFileA' SaveFilter &GENERIC_READ,
                                    &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_NORMAL, 0
   pop edi, D$edi

   .If eax = &INVALID_HANDLE_VALUE
        Mov eax D$BusyFilePtr | Call MessageBox
        pop eax | ret                       ; pop return adress of caller and ret to Callback
   .Else
        Mov D$BookMarksFileHandle eax
   .End_If

    Call 'KERNEL32.GetFileSize'  eax 0 | Mov D$BookMarksFileLen eax

    VirtualFree D$BookMarks | VirtualAlloc BookMarks 01000

    Call 'KERNEL32.ReadFile' D$BookMarksFileHandle D$BookMarks D$BookMarksFileLen,
                            NumberOfReadBytes 0
    Call CreateTreeViewList | Call SetTreeDialogPos

    Call 'KERNEL32.CloseHandle' D$BookMarksFileHandle
ret



[EndOfSourceMemory: ?    OldSourceReady: ?]

OpenRosAsmPE:
    move D$OldSourceReady D$SourceReady | Mov B$SourceReady &FALSE

    Call ClearSaveFilter | Call ClearChoosenFile

    push edi
        Mov edi OpenPEStruc
        Mov eax 04C | stosd
        Mov eax D$H.MainWindow | stosd
        Mov eax 0 | stosd
        Mov eax PEFilesFilters | stosd
        Mov eax uFileFilter | stosd
        Mov eax 260 | stosd
        Mov eax 1 | stosd
        Mov eax SaveFilter | stosd
        Mov eax 260 | stosd
        Mov eax ChoosenFile | stosd
        Mov eax 260 | stosd
        Mov eax 0 | stosd
        Mov eax OpenPEFileTitle | stosd
        Mov eax 0281804 | stosd
        Mov eax 0 | stosd
        Mov eax 0 | stosd
        Mov eax 0 | stosd
        Mov eax 0 | stosd
        Mov eax 0 | stosd
    pop edi

    Call 'Comdlg32.GetOpenFileNameA' OpenPEStruc

    If D$SaveFilter = 0
        move D$SourceReady D$OldSourceReady | ret
    End_If

DirectMRUload:
DirectLoad:
    Call ReleaseMainFile | StoreNameOnly SaveFilter
    Call ClearBackTable | Call ReleaseResourceMemory

    VirtualFree D$UserPeStart

LastMRULoading:
ReloadForDissassembler:
    Mov esi MainName
    While B$esi <> 0 | inc esi | EndWhile
    While B$esi <> '\' | dec esi | EndWhile
    push D$esi
        Mov B$esi+1 0 | Call 'Kernel32.SetCurrentDirectoryA' MainName
    pop D$esi

    Mov esi SaveFilter | Call CreateFile | On eax = &INVALID_HANDLE_VALUE, ret
    Call GetFileNameFromPath
  ; > eax = source len
;;
  The reason for allocating a twice (>>> shl eax 1) bigger Mem:
  
  When having TITLEs inside, the editor make a Copy of the edited TITLE, at the bottom
  of this Mem. Therefore, if the Source is made of two TITLEs, a very short one, say
  for Macros, and a very big one, for everything else, we need twice more Mem. Plus the
  additional Edition security, of course:
;;
    Mov D$UserPeEnd eax, D$UserPeLen eax | shl eax 1 | add eax 100_000

    push eax
        VirtualAlloc UserPeStart eax

        add D$UserPeEnd eax
    pop eax

    Align_On PAGESIZE eax | sub eax 32 | add eax D$UserPeStart
    Mov D$EndOfSourceMemory eax

    Mov D$NumberOfReadBytes 0

    Call 'KERNEL32.ReadFile' D$SourceHandle, D$UserPeStart, D$UserPeLen, NumberOfReadBytes, 0

    Call 'KERNEL32.CloseHandle' D$SourceHandle | Mov D$SourceHandle 0

    Mov eax D$UserPeStart | On W$eax <> 'MZ', jmp ExitNotPeExe

    Mov eax D$UserPeStart | sub eax DosHeader | add eax PeHeaderPointer
    Mov ebx D$eax, eax D$UserPeStart | add eax ebx
    On eax >= D$UserPeEnd, jmp ExitNotPeExe
    On D$eax <> 'PE', jmp ExitNotPeExe
    cmp D$eax+0A0 0 | setne B$RelocsWanted; jE! RelocsWanted
    Mov B$ThisSourceIsDisassembled &FALSE

    Mov eax D$UserPeStart | add eax 0178 | Mov esi eax     ; +0178 > first section header
    add eax 400 | Mov D$EndOfSectionSearch eax             ; +(10*40) bytes per section

L0: lodsd | cmp eax '.src' | je L1>
        add esi 36 | cmp esi D$EndOfSectionSearch | jb L0<
            jmp TryDisassembly

ExitNotPeExe:
    Mov eax D$NotPeExePtr | Call MessageBox | Call AutoNew | jmp StartNewFile


L1: lodsd | lodsd | Mov D$SourceLen eax
    lodsd | lodsd | lodsd
  ; eax = 0 based pointer to source

  ; 'MessageBox' messages could send a WM_PAINT before complete initializations:
    Mov D$TiTleTable 0, D$ActualTitle 0

    Call ReadCheckSum | Call ReadHeaderFormat

    add eax D$UserPeStart | Mov D$CodeSource eax

    Mov edi D$CodeSource | add edi D$SourceLen | Mov eax 0A0D0A0D, ecx 100

    rep stosd

    Mov eax D$CodeSource | add eax D$SourceLen | Mov D$SourceEnd eax
   ; add eax 1_000_000 | Mov D$EndOfSourceMemory eax
    move D$UpperLine D$CodeSource | Mov B$SourceReady &TRUE

    Call KillTabs | Call KillTrailingSpaces

    Call SearchPEstartOfResource | On B$NoResourcesPE = &TRUE, jmp L9>>

    ; Upload icon:

; Icon storing more stand-alone because of feature for reading icon in other PEs. So does
; it again job done by 'SearchPEstartOfResource'. So, i make it set 'ResourcesRVA' value
; (very durty, but...).

    move D$iExePtr D$UserPeStart | Call ReadRosAsmPeIcon ; load icon with icon editor routine

    If B$PeIconFound = &TRUE
        Mov esi eax | Mov edi iIcon | rep movsb         ; Copying to icon editor buffer
         Call StoreIcon                                  ; and copy it to 'compilable' image
    End_If

   ; Call ReadRosAsmPeMenus

   ; Call ReadRosAsmPeDialogs

  ;  Call ReadRosAsmBitMaps

  ;  Call ReadRosAsmStrings

   ; Call ReadRosAsmWaves

   ; Call ReadRosAsmAvis

   ; Call ReadRosAsmRCs

   ; Call ReadRosAsmCursors | Call ReadRosAsmGroupCursors

   ; Call ReadRosAsmIcons | Call ReadRosAsmGroupIcons

    Call ReadRosAsmResources

L9: Mov edi D$CodeSource, ecx 5, eax 0A0D | sub edi 10 | rep stosw ; security for back search
    Mov D$DestinationFile 0

    SetWindowText | Call StartEdition
ret

; For 'FillExportSection':

GetFileNameFromPath:
    push eax, esi, edi
        Mov esi SaveFilter, eax esi
        Mov edi ChoosenFile

        While B$esi <> 0 | inc esi | End_while

        While esi > eax
            dec esi
            On B$esi = '\', jmp L1>
        End_While

L1:     inc esi

        While B$esi <> 0
            movsb
        End_While
        Mov B$edi 0
    pop edi, esi, eax
ret


[DisChoice: ?]

TryDisassembly:
    On D$AddressToBeForced = &TRUE, jmp DisMain

  ; Tag Dialog 27000
    Call 'USER32.DialogBoxParamA' D$hInstance, 27000, D$H.MainWindow, DisassembleProc, 0
    Test D$DisChoice 0_8000_0000 | jz L9>
        and D$DisChoice 0FF | jmp Dismain

L9:     Call AutoNew | jmp StartNewFile

____________________________________________________________________________________________

[WithCommentedHexa: &FALSE   WithForcedMapFile: &TRUE  WithMacros: &TRUE]

; Tag Dialog 27000

Proc DisassembleProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_COMMAND                  ; User action
        ..If D@wParam = &IDCANCEL                   ; User clicks on upper right [X]
            and D$DisChoice 0FF
            Call 'User32.EndDialog' D@hwnd 0
          ; Call 'User32.DestroyWindow' D@hwnd ; Comments...

        ..Else_If D@wParam = &IDOK
            or D$DisChoice 0_8000_0000
            Call 'User32.EndDialog' D@hwnd 0

        ..Else_If D@wParam = &IDHELP
            Call Help B_U_AsmName, DisassemblerHelp, ContextHlpMessage

        ..Else
            movzx eax W@wParam
            If eax = 10
                Mov B$WithCommentedHexa &FALSE
            Else_If eax = 11
                Mov B$WithCommentedHexa &TRUE
            Else_If eax = 12
                Mov B$WithCommentedHexa &FALSE
            Else_If eax = 14
                xor B$WithForcedMapFile &TRUE
            Else_If eax = 15
                xor B$WithMacros &TRUE
            Else_If eax = 16
                xor B$AMDassumed &TRUE
            End_If

        ..End_If

    ...Else_If D@msg = &WM_INITDIALOG          ; Win ready to build the Dialog
        If B$WithCommentedHexa = &TRUE
            Call 'USER32.CheckDlgButton' D@hwnd, 11, &TRUE
        Else_If B$WithMacros = &TRUE
            Call 'USER32.CheckDlgButton' D@hwnd, 15, &TRUE
        Else
            Call 'USER32.CheckDlgButton' D@hwnd, 10, &TRUE
        End_If

        Call IsForcedMapFile
        .If eax = &TRUE
            Call Enable D@hwnd, 14
            If B$WithForcedMapFile = &TRUE
                Call 'USER32.CheckDlgButton' D@hwnd, 14, &TRUE
            End_If
        .Else
            Call Disable D@hwnd, 14
        .End_If

        If B$AMDassumed = &TRUE
            Call 'USER32.CheckDlgButton' D@hwnd, 16, &TRUE
        End_If


  ;  ...Else_If D$Message = &WM_CTLCOLOREDIT        ; Win ready to paint the Dialog
  ;      ; Control of output

    ...Else
        popad | Mov eax &FALSE | ExitP               ; Non processed

    ...End_If

    popad | Mov eax &TRUE                           ; Processed
EndP
____________________________________________________________________________________________

; >>> peheader <<<

ReadHeaderFormat:
    pushad
        Mov eax D$UserPeStart | add eax 0178 | Mov esi eax     ; +0178 > first section header
        add eax 400 | Mov D$EndOfSectionSearch eax             ; +(10*40) bytes per section

      ; Data Characteristics:
        push esi
L0:         lodsd | cmp eax '.dat' | je L1>
                add esi 36 | cmp esi D$EndOfSectionSearch | jb L0<
                jmp L0>
L1:         add esi 32
            lodsd | Mov D$DataCharacteristics eax
L0:     pop esi

      ; Code Characteristics:
L0:     lodsd | cmp eax '.tex' | je L1>
            add esi 36 | cmp esi D$EndOfSectionSearch | jb L0<
            Mov eax D$NotPEPtr | Call MessageBox | ret
L1:     add esi 32
        lodsd | Mov D$CodeCharacteristics eax

      ; SubSystem, DLL_characteristcs:
        Mov esi PeHeader | sub esi DosHeader
        add esi D$UserPeStart                   ; >>> 'PE' 0 0
        add esi 52                              ; >>>> ImageBase

        Mov eax D$esi, D$ImageBase eax, D$LinkerDllDefault eax

        Mov edi SubSystem | add esi 40          ; >>> SubSystem // DLL characteristcs
        Mov ax W$esi, W$DllAttachDetach ax

        movsd
        movsd   ; AppStackMax
        movsd   ; AppStackMin
        movsd   ; AppHeapMax
        movsd   ; AppHeapMin
    popad
ret


ReInitHeaderFormat:
    push edi
        Mov edi SubSystem
        Mov eax 2 | stosd                       ; Subsystem GUI  //  DLL characteristics 0
        Mov eax 0100000 | stosd                 ; AppStackMax
        Mov eax   01000 | stosd                 ; AppStackMin
        Mov eax 0100000 | stosd                 ; AppHeapMax
        Mov eax       0 | stosd                 ; AppHeapMin
        Mov D$CodeCharacteristics 0_60000020    ; readable, runable, code
        Mov D$DataCharacteristics 0_C0000040    ; readable, writeble, Data
        Mov W$DllCharacteristics 0
    pop edi
ret
 _______________________________

; Writing files:

[DestinationFile: ? #262]

OpenDestinationFile:
    On D$DestinationHandle > 0, Call 'KERNEL32.CloseHandle' D$DestinationHandle

    Call 'KERNEL32.CreateFileA' DestinationFile, &GENERIC_WRITE,
                                &FILE_SHARE_READ, 0,
                                &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE
        Mov eax D$BusyFilePtr | Call MessageBox
        Mov B$CompileErrorHappend &TRUE
      ; pop return adress of caller and ret to Callback:
        pop eax | ret

    Else
        Mov D$DestinationHandle eax

    End_If
ret

____________________________________________________________________________________________
;;
 This is for developpements test:

 DANGER >>> Writes a 'Test.a' on the disk without warning.
 
 'WriteDestination' is used only by 'AsmMain' when you branch to it. All these commented
 lines are for tests along developpement giving ability, by calling it, to output
 internal tables for exemple, if we want to see what is in 'DllList', we route here with
 a 'jmp L7>' after 'BuildImport'; with "mov eax, 010000 | push eax" and "push D$DllList"
 uncommented.
;;
WriteDestination:
    Call OpenDestinationFile

   ; hexprint D$StripLen
   ; showme DestinationFile

    push 0
    Mov D$NumberOfReadBytes 0 | push NumberOfReadBytes

      Mov eax, 0100

 ;;;; Just uncomment the one you want in 'Test.a':

    ; push D$LenOfCode, D$CodeSource
    ; push D$StripLen, D$CodeSourceB
     push D$StripLen, D$CodeSourceA
    ; push eax, D$EquateList
    ; push eax, D$MacroList
    ; push eax, D$PlainLabelList
    ; push eax, D$MacroData
    ; push eax, D$DataList
    ; push eax, D$LabelList
    ; push D$StripLen, D$Coderef
    ; push eax, D$DataRef
    ; push eax, D$DllList
    ; push eax, D$ApiListA
    ; push eax, D$ApiListB
    ; push eax, D$Relocation
    ; push eax, D$TreeList
    ; push eax, D$CodeList
    ; push eax, D$ExportListAPtr
;;
    Mov esi D$StatementsTable | While D$esi > 0 | add esi 4 | End_While
    sub esi D$StatementsTable
    push esi, D$StatementsTable
;;

   ; Mov eax D$EndOfSectionsMap | sub eax D$SectionsMap | push eax
;
 ;   push D$SectionsMap

    push D$DestinationHandle
    Call 'KERNEL32.WriteFile'
ret


ControlS:
    ..If D$TitleTable = 0
        Call SaveSource
        Call 'USER32.MessageBoxA' D$H.MainWindow, DestinationFile,
                                {'Saved in actual Directory:', 0}, 0

    ..Else
        Call 'USER32.DialogBoxParamA' D$hinstance, 26000, &NULL, AllOrPartProc, &NULL

        .If B$AllOrPart = 0-1
            Call RestoreRealSource | Call SaveSource
            Call SetPartialEditionFromPos

        .Else_If B$AllOrPart = 1
            Mov B$WeAreSavingPart &TRUE
            Call SaveSource
            Mov B$WeAreSavingPart &FALSE

        .Else_If B$AllOrPart = 9
            Call 'USER32.MessageBoxA' D$H.MainWindow, {"     Do you wish to save all the TITLEs
of this Application into as many asm Files?      ", 0}, {'Sure?', 0}, &MB_YESNO
            If eax = &IDYES
                Mov B$WeAreSavingPart &TRUE
                Call SaveActualPos
                Call GetTitlesNumber

L0:             push ecx
                    Call SelectTitle ecx | Call SaveSource
                pop ecx
                loop L0<

                Call 'USER32.MessageBoxA' D$H.MainWindow, {'Done', 0}, 0, 0
                Call RestoreActualPos
                Mov B$WeAreSavingPart &FALSE
            End_If

        .End_If

    ..End_If
ret


[ActualPos: ?]

SaveActualPos:
    move D$ActualPos D$CurrentWritingPos
ret


Proc SelectTitle:
    Argument @Indice

        Call RestoreRealSource
        Mov eax D@Indice
        dec eax
        Mov D$ActualPartIndex eax
        move D$CurrentWritingPos D$TitleTable+eax*4
        Call SetPartialEdition

        Mov esi D$CodeSource, edi PartName

        If D$esi = 'TITL'
            add esi 5 | While B$esi = ' ' | inc esi | End_While
            While B$esi > ' ' | movsb | End_While | Mov B$edi 0
        Else
            Mov D$PartName 'Top'
        End_If
EndP


GetTitlesNumber:
    Mov esi TitleTable, ecx 0
    While D$esi <> 0
        inc ecx | add esi 4
    End_While
ret


RestoreActualPos:
    move D$CurrentWritingPos D$ActualPos
ret


[BackUpPathName: B$ ? #&MAXPATH][BackUpNewPathName: B$ ? #&MAXPATH]

[WIN32_FIND_DATA:
 dwFileAttributes: D$ 0
 ftCreationTime.dwLowDateTime: D$ 0
 ftCreationTime.dwHighDateTime: D$ 0
 ftLastAccessTime.dwLowDateTime: D$ 0
 ftLastAccessTime.dwHighDateTime: D$ 0
 ftLastWriteTime.dwLowDateTime: D$ 0
 ftLastWriteTime.dwHighDateTime: D$ 0
 nFileSizeHigh: D$ 0
 nFileSizeLow: D$ 0
 dwReserved0: D$ 0
 dwReserved1: D$ 0]
[cFileName: B$ 0 #&MAX_PATH]
[cAlternate: B$ 0 #80]

[BackUpFileHandle: ?    LastBackUpIndice: ?]    [MaxBackUp: B$ '0010' 0]

ControlK:
    On D$MaxBackUp = '0000', ret

    If B$ReadyToRun = &FALSE
        Call 'USER32.MessageBoxA' D$H.MainWindow,
                                 {'You have to [Compile] or to [Run] before a PE BackUp', 0},
                                 {'[Ctrl][K] BackUp aborted:', 0}, 0
        ret
    End_If

    Mov esi MainName, edi BackUpPathName

    While B$esi <> 0 | movsb | End_While
    While B$esi <> '\' | dec esi | dec edi | End_While

    push esi
        While B$esi <> 0 | movsb | End_While
        Mov B$edi '\' | inc esi
        Mov D$edi 'Back', D$edi+4 'Up\ ' | add edi 7
    pop esi

    inc esi | While B$esi <> 0 | movsb | End_While
    Mov B$edi '*' | inc edi
    Mov eax D$SavingExtension | stosd | Mov B$edi 0

    push edi
        Call 'KERNEL32.FindFirstFileA' BackUpPathName, WIN32_FIND_DATA
        Mov D$BackUpFileHandle eax
    pop edi

    ...If eax = &INVALID_HANDLE_VALUE
        While B$edi <> '\' | dec edi | End_While | Mov B$edi 0
        push edi
            Call 'KERNEL32.CreateDirectoryA' BackUpPathName, 0
        pop edi
        Mov B$edi '\'
        While B$edi <> '*' | inc edi | End_While
        Mov D$edi '0000' | add edi 4
L0:     Mov eax D$SavingExtension | stosd | Mov B$edi 0
        Mov esi MainName | While B$esi <> 0 | inc esi | End_While
        move D$esi D$SavingExtension | Mov B$esi+4 0
        push esi
            Call 'KERNEL32.CopyFileA' MainName, BackUpPathName, &FALSE
        pop esi
        Mov B$esi 0

    ...Else
L2:     Call 'KERNEL32.FindNextFileA' D$BackUpFileHandle, WIN32_FIND_DATA
        cmp eax &TRUE | je L2<

        Call 'KERNEL32.FindClose' D$BackUpFileHandle

        Mov esi cFileName
        While B$esi <> 0 | inc esi | End_While
        While B$esi <> '.' | dec esi | End_While
        Call IncTextDwordBeforeEsi

        Mov edi BackUpPathName
        While B$edi <> 0 | inc edi | End_While
        While B$edi <> '*' | dec edi | End_While
        Mov eax D$esi-4, D$LastBackUpIndice eax | stosd | jmp L0<<

    ...End_If

    Mov eax D$MaxBackUp, ebx D$LastBackUpIndice | bswap eax | bswap ebx

    .If ebx >= eax
        Mov esi BackUpPathName
        While B$esi <> 0 | inc esi | End_While
        While B$esi <> '.' | dec esi | End_While
        sub esi 4 | Mov D$esi '0000'
        Mov ebx esi | sub ebx BackUpPathName
        push ebx
            Call 'KERNEL32.DeleteFileA' BackUpPathName
            Mov esi BackUpPathName, edi BackUpNewPathName
            While B$esi <> 0 | movsb | End_While | Mov B$edi 0
        pop ebx

L0:     lea esi D$BackUpPathName+ebx+4
        Call IncTextDwordBeforeEsi
        Mov eax D$MaxBackUp, edx D$esi-4 | bswap eax | bswap edx | cmp edx eax | ja L9>
        push ebx
            Call 'KERNEL32.MoveFileA' BackUpPathName, BackUpNewPathName
        pop ebx
        If eax = &TRUE
            lea esi D$BackUpNewPathName+ebx+4
            Call IncTextDwordBeforeEsi
        End_If
        jmp L0<

L9:     Mov esi BackUpNewPathName, edi BackUpPathName
        While B$esi <> 0 | movsb | End_While | Mov B$edi 0
        lea esi D$BackUpPathName+ebx+4
        Call DecTextDwordBeforeEsi

    .End_If

    Call 'USER32.MessageBoxA' D$H.MainWindow, BackUpPathName, {'Backup done to:', 0}, 0
ret

IncTextDwordBeforeEsi:
    inc B$esi-1
    ..If B$esi-1 > '9'
        Mov B$esi-1 '0' | inc B$esi-2
        .If B$esi-2 > '9'
            Mov B$esi-2 '0' | inc B$esi-3
            If B$esi-3 > '9'
                Mov B$esi-3 '0' | inc B$esi-4
            End_If
        .End_If
    ..End_If
ret

decTextDwordBeforeEsi:
    dec B$esi-1
    ..If B$esi-1 < '0'
        Mov B$esi-1 '9' | dec B$esi-2
        .If B$esi-2 < '0'
            Mov B$esi-2 '9' | dec B$esi-3
            If B$esi-3 < '0'
                Mov B$esi-3 '9' | dec B$esi-4
            End_If
        .End_If
    ..End_If
ret

____________________________________________________________________________________________



[PartName: ? #16] [WeAreSavingPart: ?    AllOrPart: ?]
[ReplaceSourceText: 'Load Source', 0]

; Tag Dialog 26000

Proc AllOrPartProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_INITDIALOG
            On B$OpeningSourceOnly = &TRUE,
                Call 'USER32.SendMessageA' D@hwnd, &WM_SETTEXT, 0, ReplaceSourceText

            Mov esi D$ActualTitle
            .If esi = D$RealCodeSource
                If D$esi <> 'TITL'
                    Mov D$PartName 'Top ', B$PartName+3 0
                    Call 'USER32.SetDlgItemTextA' D@hwnd, 1, DefaultTopTitle
                    jmp L8>>
                End_If
            .End_If

            While B$esi > ' ' | inc esi | End_While ; jump over 'TITLE'

            While B$esi = ' ' | inc esi | End_While ; >>> 'TitleName
            Mov edi PartName
            While B$esi > ' ' | movsb | End_While
            Mov al 0 | stosb
            Call 'USER32.SetDlgItemTextA' D@hwnd, 1, PartName
            jmp L8>>

    ...Else_If D@msg = &WM_COMMAND
        Mov eax D@wParam | and D@wParam 0FFFF | shr eax 16

        If D@wParam = &IDCANCEL
            Mov B$AllOrPart 0

        Else_If D@wParam = &IDOK
            Mov B$AllOrPart 1

        Else_If D@wParam = 3
            Mov B$AllOrPart 0-1

        Else_If D@wParam = 4
            Mov B$AllOrPart 9

        End_If

        Call 'User32.DestroyWindow' D@hwnd

    ...Else
L8:     popad | Mov eax &FALSE | jmp L9>

    ...End_If

    popad | Mov eax &TRUE
L9: EndP


[IncSaveTitle: B$ 'Saving .inc or .asm ? ...', 0
 IncSaveBody: "
 
   Yes > Save the inc File in its original Directory      
   
   No  > Save the asm File in current Directory", 0]


SaveSource:
    .If B$WeAreSavingPart = &TRUE
        Call RestoreRealSource

        Call SearchForIncInclude

        If eax = &TRUE
            Mov esi D$PointerToIncFileName, edi DestinationFile
            While B$esi > ' ' | movsb | End_While | Mov B$edi 0
        End_If

        push eax
            Call SetPartialEditionFromPos
        pop eax

        If eax = &TRUE
            Call 'USER32.MessageBoxA' D$H.MainWindow, IncSaveBody, IncSaveTitle,
                                      &MB_SYSTEMMODAL_&MB_YESNO
            On eax = &IDYES, jmp L2>
        End_If
    .End_If

    Call SetAsmSavingName

L2: Call OpenDestinationFile | Mov D$NumberOfReadBytes 0
    Call 'KERNEL32.WriteFile'  D$DestinationHandle, D$CodeSource, D$SourceLen,
                               NumberOfReadBytes, 0

    Call 'KERNEL32.CloseHandle' D$DestinationHandle | Mov D$DestinationHandle 0
ret


SearchForIncInclude:
    Mov esi D$CodeSource, eax &FALSE

    .While esi < D$SourceEnd
        Call IsItPREPARSE
        .If eax = &TRUE
            add esi 9
L1:         While B$esi = ' ' | inc esi | End_While
            If B$esi = ','
                inc esi | jmp L1<
            End_If

            Mov eax &FALSE | Call IsItIncIncluder

            If eax = &TRUE
                Mov eax &FALSE | Call IsTheActualTitleIncluded | On eax = &TRUE, ret
            End_If
        .End_If

        inc esi

    .End_While

    Mov eax &FALSE
ret


IsItPREPARSE:
    ..If D$esi = 'PREP'
        .If D$esi+4 = 'ARSE'
            If B$esi-1 < ' '
                On B$esi+8 = ' ', Mov eax &TRUE
            End_If
        .End_If
    ..End_If
ret


IsItIncIncluder:
L0:
    Mov ebx D$esi | or ebx 020202020
    ..If ebx = 'inci' ; IncIncluder
        Mov ebx D$esi+4 | or ebx 020202020
        .If ebx = 'nclu'
            Mov bx W$esi+8 | or bx 02020
            If bx = 'de'
                Mov bl B$esi+10 | or bl 020 | On bl = 'r', Mov eax &TRUE
            End_If
        .End_If
    ..End_If

    .If eax = &FALSE
L1:     While B$esi > ' ' | inc esi | End_While

        While B$esi = ' '  | inc esi | End_While

        If B$esi = ','
            inc esi | jmp L1<
        End_If

        On B$esi > ' ', jmp L0<
    .End_If
ret

IsTheActualTitleIncluded:
    push esi
        Mov esi D$CodeSource, eax &FALSE

        While esi < D$SourceEnd
            ...If D$esi = 'INCI'  ; INCI NCLU DE
                ..If D$esi+4 = 'NCLU'
                    .If W$esi+8 = 'DE'
                        If B$esi-1 < ' '
                            On B$esi+10 = ' ', Call IsThisTheActualTitle
                            On eax = &TRUE, jmp L9>
                        End_If
                    .End_If
                ..End_If
            ...End_If

            inc esi
        End_While
L9: pop esi
ret


[PointerToIncFileName: ?]

IsThisTheActualTitle:
    add esi 10
    While B$esi = ' ' | inc esi | End_While
    Mov D$PointerToIncFileName esi

    Mov edx &FALSE
    While B$esi <> CR | inc esi | End_While
    While B$esi <> '\' | dec esi | End_While | inc esi
    Mov edi D$ActualTitle  ; TITLE xxxx
    add edi 6 | While B$edi = ' ' | inc edi | End_While

    While B$esi <> '.'
        Mov al B$esi, bl B$edi | inc esi | inc edi | cmp al bl | jne L7>
    End_While

    .If B$esi = '.'
        If B$edi <= ' '
            Mov edx &TRUE
        End_If
    .End_If

L7: Mov eax edx
ret

____________________________________________________________________________________________


[ExportAjust: ?]    ; The value to turn an Export section Adress to RVA

PrepareDllVariables:
    Mov eax D$AppStartOfCode
    add eax D$AppTrueCodeSize | Align_On 0200 eax | Mov D$AppStartOfExp eax
    add eax D$ExportSectionLen | Align_On 0200 eax

    If D$RelocSectionSize => 8 ;jE!
        Mov D$AppStartOfReloc eax | add eax D$RelocSectionSize | Align_On 0200 eax
    Else
        Mov D$AppStartOfSrc eax | dec W$NumberOfSections
    End_If
    sub eax D$AppStartOfExp | add D$AppAllDataSize eax

    move eax D$ExportSectionLen , D$AppExpTrueSize eax
    Align_On 0200 eax | Mov D$AppExpAlignedSize eax

    Mov eax D$AppCodeRVAoffset
    add eax D$AppFileSizeOfCode | Align_On 01000 eax | Mov D$AppBaseOfExp eax

    add eax D$AppExpAlignedSize | Align_On 01000 eax

    move D$SectionTable D$AppBaseOfExp
    move D$SectionTable+4 D$AppExpAlignedSize

  ; For ease of 'FillExportSection' job:
    Mov eax D$AppBaseOfExp | sub eax D$ExportListBPtr | Mov D$ExportAjust eax

    Mov eax D$ExportSectionLen | Align_On 0200 eax | Mov D$FileAlignedExportSectionLen eax
ret

;;
 Same for Resources (DLL cases, too).

StripResourcesHeader:
    Mov edi ResourceSectionHeader | sub edi DosHeader | add edi D$CodeList
    Mov esi DataSectionHeader | sub esi DosHeader | add esi D$CodeList
    Mov ecx SourceSectionHeader | sub ecx DataSectionHeader

  ; Just because if we already stripped the Import Section in a DLL, all the records
  ; we are attempting to point to are one Section upward now:
    If D$ImportTrueSize = 0
        sub esi 40 | sub edi 40 | sub ecx 40
    End_If

    rep movsb
    Mov ebx NumberOfSections | sub ebx DosHeader | add ebx D$CodeList
    dec W$ebx

  ; Clear the Image Base record for Resource RVA and Size:
    Mov edi AppBaseOfRsrc | sub edi DosHeader | add edi D$CodeList
    Mov eax 0 | stosd | stosd
ret
;;

WritePE:
    If D$SavingExtension = '.SYS'
        Call SetSysSourceSavingName
    Else
        Call SetExeSavingName
        Mov eax 0 | Call WriteEaxToPeCheckSum
    End_If

    Call OpenDestinationFile

    Mov D$NumberOfReadBytes 0

    Call WriteCheckSum

    Call 'KERNEL32.WriteFile' D$DestinationHandle, D$CodeList, D$LenOfCode,
                              NumberOfReadBytes, 0

    If B$ExportsectionWanted = &TRUE
        Call 'KERNEL32.WriteFile' D$DestinationHandle, D$ExportListBPtr,
                                  D$FileAlignedExportSectionLen, NumberOfReadBytes, 0
    End_If

    If D$SavingExtension = '.SYS'
        jmp L1>
    Else_If D$RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
L1:
        If D$RelocSectionSize => 8 ;jE!
          Call 'KERNEL32.WriteFile' D$DestinationHandle,
                                   D$Relocation, D$FileAlignedRelocationSize,
                                   NumberOfReadBytes, 0
        End_If
    End_If

    Call 'KERNEL32.WriteFile' D$DestinationHandle, D$CodeSource, D$SourceLen,
                              NumberOfReadBytes, 0

    Call 'KERNEL32.CloseHandle' D$DestinationHandle | Mov D$DestinationHandle 0

    On D$SavingExtension = '.SYS', Call WriteSysFile
ret

;;
  It has happend that under some condition(s), it is possible that a wrong
  paring of CR/LF come out in a Source. For example "0D, 0D, 0A". As these
  conditions are unknown, a fix is done, here, at Write-Time:

CheckCRLFs:
    Mov esi D$CodeSource, edx esi, eax 0 | add edx D$SourceLen
    
    While esi < edx
        If B$esi = CR
            On B$esi+1 <> LF, inc eax
        Else_If B$esi = LF
            On B$esi-1 <> CR, inc eax
        End_If
        
        inc esi
    End_While
 
    ..If eax <> 0
        Mov ecx D$SourceLen | add ecx eax | VirtualAlloc Trash ecx
        Mov esi D$CodeSource, edi D$Trash, edx esi | add edx D$SourceLen
        
        While esi < edx
            .If B$esi = CR
                If B$esi+1 <> LF
                    movsb | Mov B$edi LF | inc edi | inc D$SourceLen
                    hexprint 1
                Else
                    movsb
                End_If
            .Else_If B$esi = LF
                If B$esi-1 <> CR
                    Mov B$edi CR | inc edi | movsb | inc D$SourceLen
                    hexprint 2
                Else
                    movsb
                End_If
            .Else
                movsb
            .End_If
            
        End_While
        
        Call 'KERNEL32.WriteFile' D$DestinationHandle, D$Trash, D$SourceLen,
                              NumberOfReadBytes, 0
hexprint D$SourceLen
        Call 'KERNEL32.CloseHandle' D$DestinationHandle | Mov D$DestinationHandle 0
        
        Call 'USER32.MessageBoxA' 0, {"
A bad pairing of CR/LF has been found in the Source,
at Write-Time. The problem is fixed, but RosAsm is
going to shut down, and you will have to Re-Load.

Do not care, then, of the Alert Message for corruption.

Sorry for the inconvenient.

In case you could point out what action(s), in the Editor,
could help reproducing these wrong CR/LFs, please report.
", 0}, 
                                     {'Source failure', 0}, 0
        
        Call 'Kernel32.ExitProcess' 0
        
    ..End_If
ret
;;

SaveBookMarks:
    Mov edi DestinationFile, ecx 0-1, al 0 | repne scasb
    While B$edi <> '.'
        dec edi
    End_While

    push D$edi, edi
        Mov eax '.BKM' | stosd

        Call OpenDestinationFile

        Mov edi D$BookMarks, al 0, ecx 0-1
L0:     repne scasb | cmp B$edi 0 | ja L0<
        sub edi D$BookMarks | inc edi                    ; edi = lenght.

        Call 'KERNEL32.WriteFile'  D$DestinationHandle, D$BookMarks, edi,
                                   NumberOfReadBytes, 0

        Call 'KERNEL32.CloseHandle' D$DestinationHandle | Mov D$DestinationHandle 0
    pop edi, D$edi
ret


StoreChoosenName:
    Mov edi MainName, ecx 262, al 0 | rep stosb
    Mov edi MainName
L0: lodsb | cmp al 0 | je L9>
           ; cmp al '.' | je L9>
      stosb | jmp L0<

L9: dec edi | cmp B$edi '.' | jne L9< ; Recent add ('.' can be inside the name too!)
    Mov B$edi 0
    While B$esi <> '.'
        dec esi
    End_While

L9: lodsd | or eax 020202000
    If eax = '.exe'
        move D$SavingExtension D$ExeExtension
    Else_If eax = '.scr'
        move D$SavingExtension D$ScrExtension
    Else_If eax = '.asm'
        move D$SavingExtension D$ExeExtension
    Else_If eax = '.dll'
        move D$SavingExtension D$DLLExtension
    Else_If eax = '.sys'
        move D$SavingExtension D$SysExtension
    Else
       ; hexxxprint 088888
    End_If
ret

[StoreNameOnly | push #1 | pop esi | Call StoreChoosenName]

[SavingExtension: '.EXE'    ExeExtension: '.EXE'    ScrExtension: '.SCR'
 DLLExtension: '.DLL'       SysExtension: '.SYS']

SetExeSavingName:
    Mov edi DestinationFile, ecx 262, al 0 | rep stosb
    Mov edi DestinationFile, esi MainName
    If B$esi = 0
        Mov ax W$DefaultFileName | stosw | jmp L9>
    End_If
L0: lodsb | cmp al 0 | je L9>
        stosb | jmp L0<
L9: Mov eax D$SavingExtension | stosd
    Mov B$edi 0
ret


SetSysSourceSavingName:
    Mov edi DestinationFile, ecx 262, al 0 | rep stosb
    Mov edi DestinationFile, esi MainName
    If B$esi = 0
        Mov ax W$DefaultFileName | stosw | jmp L9>
    End_If
L0: lodsb | cmp al 0 | je L8>
        stosb | jmp L0<

L8: On W$edi-6 <> 'So', jmp L8>

    If D$edi-4 = 'urce'
        Mov B$esi  -7, 0 | jmp L9>
    End_If

L8: Mov D$edi 'Sour', W$edi+4 'ce' | add edi 6

L9: Mov eax D$SavingExtension | stosd
    Mov B$edi 0
ret


[TestName: 'Test.a', 0]

SetTestSavingName:
    Mov edi DestinationFile, ecx 262, al 0 | rep stosb
    Mov edi DestinationFile, esi TestName
L0: lodsb | stosb | cmp al 0 | ja L0<
    Mov B$edi 0
ret


[DefaultFileName: 'PE.', 0]


; Revue: Does not work if the Path contains some '.' Period in a Folder Name (!!!!)....

[FullPartName: B$ ? #&MAXPATH]

SetAsmSavingName:
    Mov edi DestinationFile, ecx 262, al 0 | rep stosb

    If B$WeAreSavingPart = &TRUE
        Mov esi MainName, edi FullPartName, ecx &MAXPATH | rep movsb | dec edi

        While B$edi = 0 | dec edi | End_While
L0:     cmp B$edi '\' | je L1>
        cmp B$edi ':' | je L1>
            dec edi | jmp L0<

L1:     inc edi
        Mov esi PartName
        While B$esi > 0 | movsb | End_While | movsb

        Mov esi FullPartName

    Else
        Mov esi MainName

    End_If

    Mov edi DestinationFile

    If B$esi = 0
        Mov ax W$DefaultFileName | stosw | jmp L9>
    End_If

L0: lodsb | cmp al 0 | je L9>
           ; cmp al '.' | je L9>
      stosb | jmp L0<

L9: On B$edi-4 = '.', sub esi 4  ; (RosAsm outputs are always with 3 Chars Extensions).

    Mov eax '.asm' | stosd
ret

 ________________________________________________________________________________________
 ________________________________________________________________________________________


____________________________________________________________________________________________
____________________________________________________________________________________________

[ShowWinEquateError: &TRUE]

ReplaceWin32Equates:
    Mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen
    Mov B$ShowWinEquateError &TRUE, B$ErrorLevel 8  ; 'error8'

L0: lodsb

    ..If al = '&'
        On B$esi < 'A', jmp L2>>
        push ecx, esi, edi
            ;dec esi |
            Mov D$imm32 0 | Call NewSearchForWinEquate
        pop edi, eax, ecx
      ; Mov ebx esi | sub ebx eax | sub ecx ebx ; Same:
        add ecx eax | sub ecx esi | jz L9>
            Mov eax D$imm32 | Call WriteEax


    ..Else_If al = textSign
        On B$esi-2 > LowSigns, jmp L7>
        dec ecx | Mov bl al
L1:     stosb | lodsb
        If al = TextSign
            On B$esi > LowSigns, jmp L8>
            jmp L2>
        End_If
        loop L1<

    ..Else

L2:     stosb

    ..End_If

    loop L0<

L9: sub edi D$CodeSourceB | Mov D$StripLen edi
    Exchange D$CodeSourceA D$CodeSourceB

    Mov D$imm32 0
ret

L7: Mov edx D$MissingSeparator1Ptr | jmp L9>

L8: Mov edx D$MissingSeparator2Ptr

L9: Mov B$Errorlevel 9, ebx esi
    While B$esi > EOI | dec esi | End_While | inc esi
    While B$ebx > EOI | inc ebx | End_While | Mov B$ebx 0
    error edx, esi

 ________________________________________________________________________________________
 ________________________________________________________________________________________


  _____________________
  _____________________

; Same as Above, but with some specific computing of newly added [] inside macros
; unfolded. To be restructured later: make the OpenBracket > OpenVirtual job a called
; routine for both here and uper treatements. Too hot now...

ResetForNewBrackets:
    Mov esi D$CodeSourceA, edx esi | add edx D$StripLen
L0: .While esi < edx
        lodsb

        If al = TextSign
            While B$esi <> TextSign | inc esi | End_While | inc esi

        Else_If al = '?'
            On B$esi > LowSigns, jmp L0<
            On B$esi-2 > LowSigns, jmp L0<
            Mov ebx esi
            While B$ebx <> OpenBracket
                On B$ebx = OpenVirtual, jmp L0<
                dec ebx
            End_While
            Mov B$ebx OpenVirtual, ecx ebx

            While B$esi <> CloseBracket
                inc esi
                On esi >= edx, error D$UnPairedNestedBracketsPtr, ecx
            End_While
            Mov B$esi CloseVirtual

        End_If
    .End_While
;;
 'ReorderSource' halas impossible, here.
 
 keeping instructions and brackets' declarations mixed together drives to many
 problems; so we now rewrite it all: at first all bracket declarations, then all
 Virtual Data and at last all EOI instructions text:
;;
    move D$StatementsPtr D$StatementsTable, D$StatementsPtr2  D$StatementsTable2

    Mov esi D$CodeSourceA,  edi D$CodesourceB

    Mov ecx esi | add ecx D$Striplen | sub ecx 3         ; end of source adress

    On B$esi = EOI, inc esi

  ; Store, first, Brackets / VirtualBrackets:
L0: lodsb | cmp esi ecx | ja L9>>
    If al = TextSign
T0:     lodsb | cmp al TextSign | jne T0<
        jmp L0<
    End_If

    cmp al OpenVirtual | je L1>
    cmp al Openbracket | je L1>
    .If al = EOI
        If B$esi = OpenVirtual
            Call IsMixed | On eax = &TRUE, add D$StatementsPtr 4
        Else_If B$esi = OpenBracket
            Call IsMixed | On eax = &TRUE, add D$StatementsPtr 4
        Else
            add D$StatementsPtr 4
        End_If
    .End_If

    jmp L0<

  ; Brackets / VirtualBrackets found >>> adjust the Statements Table and copy:
L1: Call FillStatementsTable2 | add D$StatementsPtr 4

    stosb | Mov B$esi-1 0FF | lodsb | jmp L2> ; 0FF mark for start of [ in next part
L1: stosb | Mov B$esi-1 0 | lodsb             ; write [...brackets...]
L2: Call IsItText | je L1<                    ; for "text with CR/LF inside"
    cmp al CloseVirtual | je L3>
    cmp al Closebracket | jne L1<
L3: Mov B$esi-1 0 | stosb

    jmp L0<<

L9:  _________________________________

  ; Restart the copy for Code Statements:
L3: Mov esi D$CodeSourceA

    move D$StatementsPtr D$StatementsTable

L4: lodsb | cmp esi ecx | jae L9>> ;!!!!!!!!!    ; je and not ja because of ending EOIs...
    .If al = 0FF
        add D$StatementsPtr 4
        While B$esi = 0 | lodsb | End_While | jmp L4<

    .Else_If al = EOI
        If B$edi-1 = EOI
            sub D$StatementsPtr 4 | sub D$StatementsPtr2 4 | dec ebx
            Call FillStatementsTable2 | add D$StatementsPtr 4 | jmp L4<<
        End_If
        Call FillStatementsTable2 | add D$StatementsPtr 4

    .Else_If al = meEOI
        On B$esi <> 0FF, jmp L8>
        On B$esi-2 = 0, jmp L4<<

    .End_If
  ; Write the instructions:
L8: stosb | jmp L4<<
  ; Last EOI:
L9: stosb | Mov eax edi | sub eax D$CodeSourceB | Mov D$Striplen eax

    Mov eax D$StatementsPtr2, D$eax 0
    Exchange D$StatementsTable D$StatementsTable2
    ;Call TestStatementsTable
    Exchange D$CodeSourceA D$CodeSourceB
ret


; Does the Statement's unfold result in mixed chunks of normal Code and of Declarations?

IsMixed:
    push esi
        Mov al B$esi

        If al = OpenVirtual
            Mov bl CloseVirtual
        Else
            Mov bl CloseBracket
        End_If

        While B$esi <> EOI
            inc esi

            .If B$esi = bl
                On B$esi+1 = meEOI, inc esi

                If B$esi+1 = EOI
                    Mov eax &FALSE | jmp L9>
                Else_If B$esi+1 = OpenVirtual
                    Mov bl CloseVirtual
                Else_If B$esi+1 = OpenBracket
                    Mov bl CloseBracket
                Else
                    Mov eax &TRUE | jmp L9>
                End_If
            .End_If

        End_While

L9: pop esi
ret


FillStatementsTable2:
    push eax, ebx
        Mov eax D$StatementsPtr, eax D$eax
        Mov ebx D$StatementsPtr2, D$ebx eax
        add D$StatementsPtr2 4
    pop ebx, eax
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  Parser of the Parameters being declared as Macros (or direct Data with Automatic Labels).
  Example:
  
  > [Return | Mov eax #1]

  > return {RGB 011,022,033}        ; Unfold as: Mov eax 0332211
;;

[ParaMacrosMaxLevel: ?    ParaMacroLevel: ?    MacroModifiedExpression: ?]

ParaMacrosParser:
    Mov esi D$InstructionB, edi D$InstructionA

    Mov edx D$StripLen | add edx esi
    Mov D$ParaMacrosMaxLevel 0, D$ParaMacroLevel 0

    While esi < edx
        On B$esi = CloseParaMacro, jmp L1>
        On B$esi = OpenParaMacro, inc D$ParaMacrosMaxLevel
        movsb
    End_While

    On D$ParaMacrosMaxLevel = 0, ret

L1: Mov B$MacroModifiedExpression &TRUE

    Mov esi D$InstructionB, edi D$InstructionA, ecx D$ParaMacrosMaxLevel

    .While esi < edx
        ...If B$esi = OpenParaMacro
            inc D$ParaMacroLevel

            ..If D$ParaMacroLevel = ecx
                push edx
                    inc esi     ; Strip 'OpenParaMacro' Char.

                    If B$esi = TextSign
                        Call UnfoldDataParameter

                    Else_If B$esi+1 = memMarker
                        Call UnfoldDataParameter

                    Else
                        Call UnfoldMacroParameter

                    End_If

                    Mov B$MacroJobIsOver &FALSE
                pop edx

            ..Else
                movsb

            ..End_If

        ...Else_If B$esi = CloseParaMacro
            dec D$ParaMacroLevel
            movsb

        ...Else
            movsb

        ...End_If
    .End_While

    Mov ecx edi | sub ecx D$InstructionA
    Mov D$StripLen ecx
    Exchange D$InstructionA D$InstructionB
    Exchange D$InstructionAEnd D$InstructionBEnd
ret


UnfoldDataParameter:
    Call CreateNoMeanLabel | Call WriteNoMeanLabel

    push esi
        inc esi | While B$esi <> CloseParaMacro | inc esi | End_While | inc esi

        While esi < edx | movsb | End_While
        While B$edi-1 = EOI | dec edi | End_While

        Mov B$edi meEOI, B$edi+1 '{' | add edi 2
        Call WriteNoMeanLabel | Mov B$edi ColonSign | inc edi
    pop esi

    While B$esi <> CloseParaMacro | movsb | End_While
    Mov B$edi '}', B$edi+1 EOI, B$edi+2 EOI | add edi 3

    Mov esi edx
ret

UnfoldMacroParameter:
    push D$Striplen
        Mov D$InstructionAptr esi, ebx esi, ecx 0

      ; clear all possible 'Done Flag's:
        While B$ebx <> CloseParamacro
            On B$ebx = 0, error D$ParaMacroPtr
            and B$ebx 00_0111_1111
            inc ecx | inc ebx
        End_While

      ; End Mark and 'Striplen', for the Macros Parser:
        Mov B$ebx EOI | Mov D$StripLen ecx

      ; Count the Macro Name Length for 'IsItInMacroList':
        Mov ebx esi, D$OneWordLen 0
        While B$ebx > LowSigns
            inc D$OneWordLen | inc ebx
        End_While

        push esi
            push edi
                Call GetFromQwordCheckSum esi, D$MacroList, D$MacroListLimit
                On eax = 0, error D$ParaMacroPtr

                While B$eax > LowSigns | inc eax | End_While | inc eax
                Mov esi D$eax, ecx D$eax+4

            pop edi

            Mov D$InstructionBptr edi
            Call ReplaceFromMacroData

        pop esi

        While B$esi <> EOI | inc esi | End_While | inc esi

    pop D$Striplen
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;
; Mov eax (((12*3) xor 001010101)+2)

[InsideExpression: ?    StartOfSourceExpression: ?    StartOfDestinationExpression: ?
 RealExpression: ?      RealHexaSize: ?]
;;
 Simply searches for '(', saves Pos and Call for the translation after count of nested
 levels in ebx.

 In case of Real, i change the Mem Marker from 'R$/F$' to 'Q$/D$' because i store the
 computed Real in Hexa format (it would be far too complicated to rewrite them in
 Math Decimal notation).
;;

ExpressionParser:
    Mov edx esi | add edx D$Striplen
    Mov B$InsideExpression &FALSE, B$RealHexaSize 0
    Mov ebx 0, ecx 0

L0: .While esi < edx
        lodsb

        ..If al = TextSign
            stosb | While B$esi <> TextSign | movsb | End_While | movsb | jmp L0<
        ..Else_If al = OpenSign
            .If B$InsideExpression = &FALSE
                cmp B$esi-2 memMarker | jne L3>
                Mov al B$esi-3 | and al 00_0111_1111 ; Mask Equates and Macros Done Flag.
                cmp al 'F' | jne L1>
                    Mov B$edi-2 'D', B$RealHexaSize 8 | jmp L2>
L1:             cmp al 'R' | jne L3>
                    Mov B$edi-2 'Q', B$RealHexaSize 16
L2:                 Mov B$RealExpression &TRUE | jmp L4>
L3:             Mov B$RealExpression &FALSE
L4:             Mov ebx 0, ecx 0
                Mov D$StartOfSourceExpression esi, D$StartOfDestinationExpression edi
            .End_If
            inc ebx | inc ecx | Mov B$InsideExpression &TRUE

        ..Else_If al = CloseSign
            On ecx = 0, error D$ParenthesisPtr
            dec ecx

        ..Else_If al = OpenVirtual
            On B$InsideExpression = &TRUE, error D$ParenthesisPtr
            Call CheckBracketExpression

        ..Else_If al = Openbracket
            On B$InsideExpression = &TRUE, error D$ParenthesisPTR
            Call CheckBracketExpression

        ..Else_If al = EOI
            On B$InsideExpression = &TRUE, error D$ParenthesisPtr

        ..End_If

        ..If ebx > 0
            .If ecx = 0
                push edx
                    If B$RealExpression = &TRUE
                        Call ComputeRealExpression
                    Else
                        Call ComputeExpression
                    End_If
                    Call WriteExpresionResult
                    Mov ebx 0, ecx 0, B$InsideExpression &FALSE
                pop edx
                jmp L0<<
            .End_If
        ..End_If

        stosb
    .End_While
ret

;;
Due to the way the Expression Parser replaces Real by 'D$' / 'Q$' Hexa, all the
Components of a Data set with Real Expressions have to be given with a size marker.
The checking operations are repeatedly splitted for speed purpose.
;;


CheckBracketExpression:
; To be reviewed entirely. There is something out of logic at "L1":
    push ebx
    push eax, esi
      ; Is there any Parenthesis???
;;
        While esi < edx
            On B$esi = OpenSign, jmp L1>
            inc esi
        End_While
        jmp L9>>

      ; OK, OpenSign Inside. Is there any Real Marker???
L1:     pop esi | push esi
        While esi < edx
            If B$esi = MemMarker
                Mov al B$esi-1 | and al 00_0111_1111
                On al = 'F', jmp L1>
                On al = 'R', jmp L1>
            End_If
            inc esi
        End_While
        jmp L9>>
;;
        Mov ebx 0
        While esi < edx
            lodsb
            .If al = MemMarker
                Mov bl B$esi-2 | and bl 00_0111_1111

            .Else_If al = OpenSign
                If bl = 'F'
                    jmp L1>
                Else_If bl = 'R'
                    jmp L1>
                End_If

            .End_If
        End_While
        jmp L9>>


      ; OK, OpenSign plus Real Marker inside >>> Want SizeMarkers everywhere. Not
      ; 'everywhere', in fact, but before and after the Expression... :
L1:     pop esi | push esi
        .While esi < edx
            ...If B$esi = OpenSign
              ; MemMarker wanted before Real Expression:
                If B$esi-1 <> numSign
                    On B$esi-1 <> MemMarker, error D$MarkerBeforePtr
                End_If

                Mov eax 1
                While eax > 0
                    inc esi
                    If B$esi = OpenSign
                        inc eax
                    Else_If B$esi = CloseSign
                        dec eax
                    End_If
                    On B$esi = CloseBracket, error D$ParenthesisPtr
                    On B$esi = CloseVirtual, error D$ParenthesisPtr
                End_While

              ; Now, ebx at the end of Expression. CloseBracket >>> OK:
                cmp B$esi+1 CloseBracket | je L9>
                cmp B$esi+1 CloseVirtual | je L9>

                inc esi | On B$esi > Separators, error D$MarkerAfterPtr

                push esi
                    inc esi
                    While B$esi > PartEnds
                        inc esi
                    End_While
                    .If B$esi = ColonSign
                        On B$esi+2 <> MemMarker, error D$MarkerAfterPtr
                    .Else_If B$esi <> MemMarker
                        Error D$MarkerAfterPtr
                    .End_If
                pop esi

            ...End_If
L8:         inc esi
        .End_While

L9: pop esi, eax
    pop ebx
ret


[ExpressionResult: ? ?    Operator: ?]

[ExpressionA: ? ExpressionB: ?]
[ExpressionAPtr: ? ExpressionBPtr: ?]
[ExpressionALimit: ?  ExpressionBLimit: ?]

InitExpressionBuffers:
    VirtualAlloc ExpressionA 01000 | add eax 0800
    Mov D$ExpressionALimit eax, D$ExpressionAPtr eax
    VirtualAlloc ExpressionB 01000 | add eax 0800
    Mov D$ExpressionBLimit eax, D$ExpressionBPtr eax
ret

; Main parsing of one Expression. Calls for storage (in Binary) and for re-write after
; computation, from the more inside level to first one:

ComputeExpression:
    push esi
        Mov ecx esi, esi D$StartOfSourceExpression, edi D$ExpressionA
        dec esi | sub ecx esi | rep movsb            ; copy the Expression to 'ExpressionA'.
        Mov al 0 | stosb

L0:     Mov D$ExpressionResult 0, D$ExpressionResult+4 0, B$Operator OpenSign
        Mov esi D$ExpressionA, edi D$ExpressionB, edx 0

        push ebx
L1:         lodsb | stosb | cmp al OpenSign | jne L1<
                dec ebx | jnz L1<
        pop ebx

        If edi > D$ExpressionBLimit
            Call ExtendTableMemory ExpressionA, ExpressionAPtr, ExpressionALimit
            Call ExtendTableMemory ExpressionB, ExpressionBPtr, ExpressionBLimit
        End_If

L2:     On B$esi = Space, inc esi
        Mov eax D$esi | and al 00_0111_1111     ; clear possible 'Done Flag'.

        ...If al >= 'A'
            ..If ax = 'OR'
                Mov B$Operator 'O' | add esi 2 | On B$esi = Space, inc esi
            ..Else_If ax = 'XO'
                If B$esi+2 = 'R'
                    Mov B$Operator 'X' | add esi 3 | On B$esi = Space, inc esi
                Else
                    jmp L9>> ;error D$ExpressionMemberPtr
                End_If
            ..Else_If al = 'S'
                If W$esi+1 = 'HL'
                    Mov B$Operator 'L' | add esi 3 | On B$esi = Space, inc esi
                Else_If W$esi+1 = 'HR'
                    Mov B$Operator 'R' | add esi 3 | On B$esi = Space, inc esi
                Else
                    jmp L9>> ;error D$ExpressionMemberPtr
                End_If
            ..Else_If ax = 'AN'
                If B$esi+2 = 'D'
                    Mov B$Operator 'A' | add esi 3 | On B$esi = Space, inc esi
                Else
                    jmp L9>> ;error D$ExpressionMemberPtr
                End_If
            ..Else_If ax = 'NO'
                If B$esi+2 = 'T'
                    Mov B$Operator 'N' | add esi 3 | On B$esi = Space, inc esi
                Else
                    jmp L9>> ;error D$ExpressionMemberPtr
                End_If
            ..Else
                jmp L9>> ;error D$ExpressionMemberPtr
            ..End_If

        ...Else_If al < '0'
            move D$Operator D$esi | Mov B$Operator al | inc esi

        ...Else_If al <= '9'
            push ebx
                If ax = '00'
                    Call TranslateBinary | dec esi | Call StoreOnExpression
                Else_If al = '0'
                    Call TranslateHexa | dec esi | Call StoreOnExpression
                Else
                    and B$esi 00_0111_1111
                    Call TranslateDecimal | dec esi | Call StoreOnExpression

                End_If
            pop ebx

        ...Else
            jmp L9>> ;error D$ExpressionMemberPtr

        ...End_If

        cmp B$esi-1 CloseSign | jne L2<<
            Call ReWriteExpression
            dec ebx | jnz L0<<
L9: pop esi
    ret


[ExpressionReal: R$ ?    RealExpressionResult: R$ ?]

ComputeRealExpression:
    On ebx > 1, error D$RealNotationPtr

    push esi
        Mov ecx esi, esi D$StartOfSourceExpression, edi D$ExpressionA
        dec esi | sub ecx esi ;| rep movsb          ; copy the Expression to 'ExpressionA'.
      ; Since the Expression parser has moved in between the Equates and Macros jobs,
      ; we have to mask out the Byte HighBit (Done Flag):
L0:     lodsb | and al 00_0111_1111 | stosb | loop L0<
        Mov al 0 | stosb


L0:     Mov D$RealExpressionResult 0, D$RealExpressionResult+4 0, B$Operator OpenSign
        Mov esi D$ExpressionA, edi D$ExpressionB, edx 0

        push ebx
L1:         lodsb | stosb | cmp al OpenSign | jne L1<
                dec ebx | jnz L1<
        pop ebx

L2:     On B$esi = Space, inc esi

        ...If B$esi < '0'
            move D$Operator D$esi | inc esi

        ...Else_If B$esi <= '9'
            push ebx
                Mov edi ExpressionReal | Call atof      ; Result in ST0
                dec esi
                Call StoreOnRealExpression
            pop ebx

        ...Else
            error D$RealNotationPtr

        ...End_If

        cmp B$esi CloseSign | jne L2<<
            Call ReWriteRealExpression
            dec ebx | jnz L0<<

L9: pop esi
ret


[NegativeMember: ?]

StoreOnExpression:
    ..If B$Operator >= 'A'
        .If B$Operator = 'O'
            or D$ExpressionResult+4 edx | or D$ExpressionResult eax

        .Else_If B$Operator = 'X'
            xor D$ExpressionResult+4 edx | xor D$ExpressionResult eax

        .Else_If B$Operator = 'N'
            On D$ExpressionResult > 0, error D$ExpressionNOTPtr
            On edx > 0, error D$ExpressionNOTPtr
            not eax
            Mov D$ExpressionResult eax

        .Else_If B$Operator = 'A'
            and D$ExpressionResult+4 edx | and D$ExpressionResult eax

        .Else_If B$Operator = 'L'
            On edx > 0, error D$ExpressionSHRPtr
            On eax > 0FF, error D$ExpressionSHRPtr
                Mov cl al
                shl D$ExpressionResult+4 cl
                shl D$ExpressionResult cl | adc D$ExpressionResult+4 0

        .Else_If B$Operator = 'R'
            On edx > 0, error D$ExpressionSHRPtr
            On eax > 0FF, error D$ExpressionSHRPtr
                Mov cl al
                shr D$ExpressionResult cl
                shr D$ExpressionResult+4 cl | adc D$ExpressionResult 0

        .Else
            Mov D$ExpressionResult+4 edx, D$ExpressionResult eax

        .End_If

    ..Else_If B$Operator = AddSign
        add D$ExpressionResult+4 edx | add D$ExpressionResult eax

    ..Else_If B$Operator = SubSign
        sub D$ExpressionResult+4 edx | sub D$ExpressionResult eax

    ..Else_If B$Operator = MulSign
        On edx > 0, error D$TooMuchExpressionPtr
        On D$ExpressionResult+4 > 0, error D$TooMuchExpressionPtr
        Mov ecx eax, eax D$ExpressionResult
        mul ecx
        Mov D$ExpressionResult+4 edx | Mov D$ExpressionResult eax

    ..Else_If B$Operator = DivSign
        Mov ecx eax, eax D$ExpressionResult, edx D$ExpressionResult+4
        div ecx | Mov edx 0
        Mov D$ExpressionResult+4 0 | Mov D$ExpressionResult eax

    ..Else_If B$Operator = OpenSign
        Mov D$ExpressionResult+4 edx, D$ExpressionResult eax

    ..Else
        error D$ExpressionSignPtr

    ..End_If
ret


; The result of new computed Value is in ST0.

[TempoReal: Q$ ?]

StoreOnRealExpression:
    ..If B$RealHexaSize = 8
        fstp F$TempoReal | fld F$RealExpressionResult
        .If B$Operator = AddSign
            fadd F$TempoReal
        .Else_If B$Operator = SubSign
            fsub F$TempoReal
        .Else_If B$Operator = MulSign
            fmul F$TempoReal
        .Else_If B$Operator = DivSign
            fdiv F$TempoReal
        .Else_If B$Operator = OpenSign
            fld F$TempoReal
        .Else
            error D$RealNotationPtr
        .End_If
        fstp F$RealExpressionResult
    ..Else
        fstp R$TempoReal | fld R$RealExpressionResult
        .If B$Operator = AddSign
            fadd R$TempoReal
        .Else_If B$Operator = SubSign
            fsub R$TempoReal
        .Else_If B$Operator = MulSign
            fmul R$TempoReal
        .Else_If B$Operator = DivSign
            fdiv R$TempoReal
        .Else_If B$Operator = OpenSign
            fld R$TempoReal
        .Else
            error D$RealNotationPtr
        .End_If
        fstp R$RealExpressionResult
    ..End_If
ret


[StartOfHexaExpression: ?]

ReWriteExpression:
    push ebx
        On edx > 0, error D$TooMuchExpressionPtr
        Mov esi D$ExpressionA, edi D$ExpressionB

L1:     lodsb | stosb | cmp al OpenSign | jne L1<
                dec ebx | jnz L1<
        dec edi                                         ; skip first '('.
        Mov eax D$ExpressionResult

        Mov ebx eax, ecx 8, B$StartOfHexaExpression &TRUE

L0:     Mov eax ebx | and eax 0_F000_0000 | shr eax 28
        add al '0' | On al > '9', add al 7
        shl ebx 4
        .If B$StartOfHexaExpression = &TRUE
            If al > '0'
                Mov B$StartOfHexaExpression &FALSE
                Mov B$edi '0' | inc edi
            End_If
        .End_If
        On B$StartOfHexaExpression = &FALSE, stosb | loop L0<

        If B$StartOfHexaExpression = &TRUE              ; Case of result = 0
            Mov B$edi '0' | inc edi
        End_If

        While B$esi-1 <> CloseSign
            inc esi                                     ; jmp over resolved part.
        End_While

        If B$esi > PartEnds
            Mov al Space | stosb
        End_If

        While B$esi-1 > 0
            movsb                       ; recopy remainder of expression.
        End_While
        Exchange D$ExpressionA D$ExpressionB
        Exchange D$ExpressionALimit D$ExpressionBLimit
    pop ebx
ret


ReWriteRealExpression:
    push ebx
       ; On edx > 0, error TooMuchExpression ; ??????!!!!!!......
        Mov esi D$ExpressionA, edi D$ExpressionB

L1:     lodsb | stosb | cmp al OpenSign | jne L1<
                dec ebx | jnz L1<
        dec edi                                         ; skip first '('.

        Mov ecx D$RealHexaSize, B$StartOfHexaExpression &TRUE

        If ecx = 16
            Mov ebx D$RealExpressionResult+4
        Else
            Mov ebx D$RealExpressionResult
        End_If

L0:     Mov eax ebx | and eax 0_F000_0000 | shr eax 28
        add al '0' | On al > '9', add al 7
        shl ebx 4
        .If B$StartOfHexaExpression = &TRUE
            If al > '0'
                Mov B$StartOfHexaExpression &FALSE
                Mov B$edi '0' | inc edi
            End_If
        .End_If
        On ecx = 9, Mov ebx D$RealExpressionResult
        On B$StartOfHexaExpression = &FALSE, stosb | loop L0<

        If B$StartOfHexaExpression = &TRUE              ; Case of result = 0
            Mov B$edi '0' | inc edi
        End_If

        While B$esi-1 <> CloseSign
            inc esi                                     ; jmp over resolved part.
        End_While

        If B$esi > PartEnds
            Mov al Space | stosb
        End_If

        While B$esi-1 > 0
            movsb                                       ; recopy remainder of expression.
        End_While
        Exchange D$ExpressionA D$ExpressionB
        Exchange D$ExpressionALimit D$ExpressionBLimit
    pop ebx
ret


; True final result rewrite in source:

WriteExpresionResult:
    push esi
        Mov edi D$StartOfDestinationExpression, esi D$ExpressionA
      ; (Expression 'A' and not 'B' as source, because they are switched after each pass).

      ; SourceCleaner may leave one more SpAce when ",(" encounted >>> to review.
        If B$edi-1 <> Space
            .If B$edi-1 > NoSpaceAfterThis
               ; Mov al Space | stosb
            .End_If
        End_If

        While B$esi > 0
            movsb
        End_While
;;
   ; For Analyzes:
  
        push edi
            Mov B$edi 0
            Mov edi D$StartOfDestinationExpression
            sub edi 3
            howme edi
            exprint D$edi
        pop edi
;;
    pop esi
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
 Routines for adressing the lists used for data, data labels and code labels
 three main tables are used:
                              DataList, LabelList, CodeRef.
 DataList is a simple list of double words values. LabelList is a index for DataList
 (set of couples of name / adress pointing to DataList). CodeRef is an index
 (same couples structure), pointing to code symbols evocation (offset in code).

 The two index tables are sets of fields like this:
                             ...|MySymbol|000024A0...|...

 Tables will begin with a dWord for the size in bytes. (of little use)


 Filling certain heads words with zeros is usefull in case of error search
;;

[ListsLength: ?]

InitIndex1:
;;
  None of these tables should be greater than stripLen.
  For a big file like RosAsm source, it makes about 3 Mo. for the 11 tables together
  could be much shorter, if any problem, mainly for last 4 tables.
  
  Though, because of Macros possibly creating Data Labels, for example, it is
  possible to overflow easily, particulary on very small files. So one Page is
  added to these Allocations.
  
  Also 'ListsLength', that is used in 'GetFromQwordCheckSum' for controling the
  matching of a scanned Name with its proper List, must be aligned on its final
  - and not so predictable - size (Align_On 01000).
;;
    add D$StripLen 01000

    move D$ListsLength D$StripLen
    Align_On 01000 D$ListsLength

    VirtualAlloc LabelList D$StripLen
    Mov edi D$LabelList | Mov eax 0 | stosd
    Mov al EOI | stosb | Mov D$LabelListPtr edi
    add edi D$StripLen | sub edi TABLES_SECURITY | Mov D$LabelListLimit edi

    VirtualAlloc EquateList D$StripLen
    Mov edi D$EquateList | Mov eax 0 | stosd
    Mov al EOI | stosb | Mov D$EquateListPtr edi
    add edi D$StripLen | sub edi TABLES_SECURITY | Mov D$EquateListLimit edi

    VirtualFree D$EquateSubstitutes
    VirtualAlloc EquateSubstitutes D$StripLen
    Mov edi D$EquateSubstitutes | Mov D$EquateSubstitutesPtr edi
    add edi D$StripLen | sub edi TABLES_SECURITY
    Mov D$EquateSubstitutesLimit edi
    move D$LastEquateList D$EquateList
    move D$LastEquateListPtr D$EquateListPtr

    VirtualAlloc MacroList D$StripLen
    Mov edi D$MacroList | Mov eax 0 | stosd
    Mov al EOI | stosb | Mov D$MacroListPtr edi
    add edi D$StripLen | sub edi TABLES_SECURITY | Mov D$MacroListLimit edi

    VirtualAlloc MacroData D$StripLen | move D$MacroDataPtr D$MacroData
    Mov eax D$MacroData | add eax D$StripLen | sub eax TABLES_SECURITY
    Mov D$MacroDataLimit eax

    sub D$StripLen 01000
ret


InitIndex2:
  ; Don't supress reloc table: Pointers needed for setting of other values:
    VirtualAlloc Relocation D$StripLen | move D$RelocationPtr D$Relocation

    VirtualAlloc ApiListA D$StripLen | move D$ApiListAPtr D$ApiListA

    VirtualAlloc ApiListB D$StripLen | move D$ApiListBPtr D$ApiListB

    VirtualAlloc DllList D$StripLen | move D$DllListPtr D$DllList
ret


InitIndex3:
    VirtualFree D$DllList, D$ApiListB, D$ApiListA

    VirtualAlloc CodeRef D$StripLen

    Mov edi D$CodeRef | Mov eax 0 | stosd
    Mov al EOI | stosb | Mov D$CodeRefPtr edi

    VirtualAlloc DataRef D$StripLen
    Mov edi D$DataRef | Mov eax 0 | stosd
    Mov al EOI | stosb | Mov D$DataRefPtr edi
    Mov eax D$StripLen | add eax D$DataRef | sub eax TABLES_SECURITY
    Mov D$DataRefLimit eax
ret

 ________________________________________________________________________________________
 ________________________________________________________________________________________
;;
 analyze of data declarations and macros (+ equates)

 Macros and equates bodies are all stored in MacroData. MacroList and EquateList
 could also be only one table, but i choose to separate them because i wanted to
 alternate the two replacement routines (see ReplaceEquatesAndMacros) for flow
 control (source goes forth and back between CodeSourceA and CodeSourceB) and
 because i wanted equate to be replaced in macro statement 'before' expanding job
 Third reason: A macro is always first word of a line / equates may be anywhere
 in a line; so, the job is different.
 ________________________________________________________________________________________

 MacroList looks  ....|name|dWord1 dWord2 Byte|....
                            dWord1 = adress in MacroData
                                  dWord2 = lenght of statement(s)
                                         byte is set to 0 when storing
 This byte is not for expanding control. It is only for end user can check if unsused
 macro are in his source file (The "Unused Symbols" thingie in the Statistics)

 EquateList looks the same.

 MacroData is a simple text list of what is to replace equates and macros without
 any separators.

 EquateList:
 MacroList:         ; storage of macros and equates ...|name|adr/len/flag|...
 MacroData:         ; raugh storage of macros and equates body(no lenght as first word)
;;
 _______________________________________________________________________________________

[LoadAndClear | lodsb | Mov B$esi-1 0]
 _______________________________________________________________________________________

; MacroList begin with a Dword for lenght of table followed by macro items.
 _______________________________________________________________________________________

[LastOctetInSource: ?]

StoreEquates:
    Mov edi D$EquateListPtr,  B$esi-1 0FF

    If edi > D$EquateListLimit
        Call ExtendTableMemory EquateList, EquateListPtr, EquateListLimit
        Mov edi D$EquateListPtr
    End_If

L0: push esi | Mov ebx 0
L1:     lodsb | inc ebx | cmp al LowSigns | ja L1<
    pop esi | dec ebx | ;Call IsItNewEquate  ; with lenght in ebx

L1: LoadAndClear
    If al = Closebracket
        Error D$MissEquateValPtr
    Else_If al = ColonSign
        While B$edi-1 <> EOI | dec edi | End_While
        Error D$EquateLabelPtr edi
    End_If
    cmp al, Space | je L2>                  ; write name in EquateList up to first space
    On al < LowSigns, error D$MissEquateValPtr
        stosb | jmp L1<
L2: Mov al EOI | stosb
    Mov eax D$MacroDataPtr | stosd          ; write futur adress of data in Macrolist

    Call SetQwordCheckSum D$EquateListPtr

    Mov D$EquateListPtr edi                 ; save pointer
    Mov edi eax                             ; switch from index to data

L3: loadAndClear | cmp al Separators | jb L9>   ; write equate body
    cmp al Closebracket | je L9>            ; in macroData

    .If al = TextSign
        stosb
L4:     loadAndClear | stosb | cmp al TextSign | jne L4<

    .Else_If al = '<'
        On B$esi = SpAce, LoadAndClear
L4:     loadAndClear | cmp al '>' | je L3<
        On al = Closebracket, error D$TextEquatePtr
        stosb
      ; If Space before '>', strip Space:
        If B$esi = Space
            On B$esi+1 = '>', LoadAndClear
        End_If
        jmp L4<

    .Else
        stosb

    .End_If
    jmp L3<

L9: Mov bl al                               ; save lasting sign for ending test
    Mov eax edi | sub eax, D$macroDataPtr   ; written data lenght in ax
    Mov D$MacroDataPtr edi                  ; save pointer
    If edi > D$MacroDataLimit
        Call ExtendTableMemory MacroData, MacroDataPtr, MacroDataLimit
    End_If
    Mov edi D$EquateListPtr | stosd         ; store data lenght in MacroList (second word)
    Mov al 0 | stosb                        ; room for 'done' flag used when replacing
    Mov al EOI | stosb
    Mov D$EquateListPtr edi                 ; save pointer
    cmp bl Separators | jb L0<<             ; one more equate > store again
ret

[Equateee "merde agaa"  ]
 _______________________________________________________________________________________

StoreMacros:
    Mov edi D$MacroListPtr,  B$esi-1 0

    If edi > D$MacroListLimit
        Call ExtendTableMemory MacroList, MacroListPtr, MacroListLimit
        Mov edi D$MacroListPtr
    End_If

    push esi
        Mov ebx 0
L0:     lodsb | inc ebx | cmp al LowSigns | ja L0<
    pop esi
    dec ebx | ;Call IsItNewMacro           ; with symbol lenght in ebx

L1: LoadAndClear | cmp al meEOI | je L2>  ; write name in MacroList up to first separator
        stosb | jmp L1<

L2: Mov al EOI | stosb                    ; no meEOI in MacroList (yes in MacroData)
    Mov eax D$MacroDataPtr | stosd        ; write futur adress of data in Macrolist

    Call SetQwordCheckSum D$MacroListPtr

    Mov D$MacroListPtr edi                ; save pointer
    Mov edi eax                           ; switch EDI from index to data

L3: LoadAndClear | cmp al CloseBracket | je L9>
        stosb | jmp L3<                   ; write macro body and loop
L9: Mov eax edi | sub eax D$macroDataPtr  ; written data lenght in eax
    Mov D$MacroDataPtr edi

    If edi > D$MacroDataLimit
        Call ExtendTableMemory MacroData, MacroDataPtr, MacroDataLimit
    End_If

    Mov edi D$MacroListPtr

    stosd                                 ; store data lenght in MacroList (second word)
    Mov al 0 | stosb                      ; room for 'done' flag used when replacing
    Mov al EOI | stosb
    Mov D$MacroListPtr edi
ret
 ________________________________________________________________________________________

; here we use 'LastOctetInSource' instead of checking '||'

StripZeros:
    Mov esi D$CodeSourceA,  edi D$CodeSourceB,  ebx 0
L0: lodsb | cmp esi D$LastOctetInSource | jae L9>>
        cmp al 0FF | je L0<
        cmp al 0 | je L0<

L1:     .If al = EOI                            ; difficulty: double separators '||' may
            If B$edi-1 = meEOI                  ; result after storage. case: '|[....]|'
                Mov B$edi-1 EOI | jmp L0<       ; same on second pass after '{}'.
            Else_If B$edi-1 = EOI
                jmp L0<
            End_If

        .Else_If al = meEOI
            If B$edi-1 = meEOI
                jmp L0<
            Else_If B$edi-1 = EOI
                jmp L0<
            End_If

        .End_If

L5: stosb | inc ebx | jmp L0<<
L9: Mov al EOI | stosb | inc ebx | Mov D$StripLen ebx
    Mov eax 02020202 | stosd                    ; security

    Exchange D$CodeSourceA D$CodeSourceB
ret


StripDoneStatementsPointers:
    Mov esi D$StatementsTable, edi esi

L0: lodsd
    If eax = DoneFlag
        jmp L0<
    Else_If eax > 0
        stosd | jmp L0<
    End_If

    stosd

; Test of Last recorded Code Statement:

;    Mov eax edi | sub eax 8 | Mov eax D$eax
;
;    Mov ebx eax, dl B$ebx+3, B$OldChar dl, B$ebx+3 0
;    pushad
;        Mov esi eax
;        Call SetDebuggeeText
;        Call AskForRedraw
;    popad
;    exprint eax

ret
 _______________________________________________________________________________________

; after storing equates and macros, stripped results of remaining source will be
; written from CodeSourceA to CodeSourceB.
; At last, StripZero will work back again < > A
 _______________________________________________________________________________________
[FirstPassTest: ?]

StoreAllEquates:
    Mov B$FirstPassTest 0

    Mov esi D$CodeSourceA,  ecx D$StripLen | add ecx esi | inc ecx  ; ecx = max esi value
    Mov D$LastOctetInSource ecx,  B$ErrorLevel 0,  D$bracketCounter 0
    move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4

L0: lodsb | Call IsItText | je L0<
        cmp al EOI | je L9>>                        ; EOI >>> no more brackets.
    cmp al OpenVirtual | jne L1>
        inc D$bracketCounter
        add D$StatementsPtr 4 | jmp L0<
L1: cmp al Openbracket | jne L0<                    ; loop until '['
        inc D$bracketCounter
        add D$StatementsPtr 4
        push esi
        cmp B$esi '<' | jne L2>                     ; Data Alignement?
       ; cmp B$esi+1 '0' | jb L2>
       ; cmp B$esi+1 '9' | ja L2>
        or B$esi 00_1000_0000  ; to prevent ReplaceEquates from replacing '[<' by '[B' in
                               ; case user define an Equate for '<' Char (not reserved Char)
        pop eax | jmp L0<
L2:     lodsb | cmp al, OpenParaMacro | ja L2<
            Mov ecx esi                             ; for end reached test
        pop esi
    cmp al ColonSign | jne L3>                      ; data?
        Mov esi, ecx | jmp L0<
L3: cmp al Space | jne L3>                          ; equate?
        Call StoreEquates
            Mov eax D$StatementsPtr, D$eax DoneFlag
        jmp L0<
L3: cmp al meEOI | je L0<                           ; macro?

L3: Error D$UnknownDataPtr

L9: Mov edi D$EquateListPtr, ecx 20 | rep stosb     ; '|||||||' as security tail
    Mov eax, D$EquateListPtr | sub eax, D$EquateList; write size of equate list
    Mov edi, D$Equatelist | inc eax | stosd         ; at first word
ret


StoreAllMacros:
    Mov esi D$CodeSourceA,  ecx D$StripLen | add ecx esi | inc ecx  ; ecx = max esi value
    Mov D$LastOctetInSource ecx,  B$ErrorLevel 0,  D$bracketCounter 0
    move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4

L0: lodsb | Call IsItText | je L0<
    cmp al EOI | je L9>                             ; EOI >>> no more brackets.
    cmp al OpenVirtual | jne L1>
        inc D$bracketCounter
        add D$StatementsPtr 4 | jmp L0<
L1: cmp al 0FF | je L8>
    cmp al Openbracket | jne L0<                    ; loop until '['
L7:     inc D$bracketCounter
        add D$StatementsPtr 4
    cmp B$esi 0BC | je L0< ; jE! fixed '[<' case
        push esi
L2:         lodsb | cmp al, LowSigns | ja L2<
            Mov ecx esi                             ; for end reached test
        pop esi

L3: cmp al meEOI | jne L0<                          ; macro?
        On B$esi = meEOI, error D$UnexpectedCRLFPtr
        Call storeMacros
            Mov eax D$StatementsPtr, D$eax DoneFlag
        jmp L0<

L8: lodsb | cmp al 0 | je L8<
    inc D$bracketCounter
    add D$StatementsPtr 4
    dec esi | jmp L0<<

L9: Mov edi D$MacroListPtr, ecx 20 | rep stosb      ; '|||||||' as security tail
    Mov eax, D$MacroListPtr | sub eax D$MacroList   ; write size of macro list
    Mov edi, D$Macrolist | inc eax | stosd          ; at first word
ret


StoreEquatesAndMacros:
    Call StoreAllEquates
    Call ResolveEquates
    Call StoreAllMacros
    Call StripZeros

    Call StripDoneStatementsPointers
  ; Source: A > B
  ; Call TestStatementsTable
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[SortTempoMemory: ?]

SortEquatesAndMacrosLists:
    Mov eax D$EquateListPtr | sub eax D$EquateList | inc eax
    On eax = 6, jmp L9>>

    push eax
        VirtualAlloc SortTempoMemory eax
    pop ecx
    push ecx
        Mov edi D$SortTempoMemory, esi D$EquateList
        shr ecx 2 | inc ecx | rep movsd

        move D$SortSource D$SortTempoMemory | add D$SortSource 5
        move D$SortDestination D$EquateList | add D$SortDestination 5

        Mov D$SortStringNumber 0 | Mov esi D$SortDestination

L0:     lodsb | cmp al EOI | jne L0<
        add esi 11
        inc D$SortStringNumber
        cmp esi D$EquateListPtr | jb L0<

        Call SortStrings

    pop ecx
    Mov esi D$EquateList, edi D$SortTempoMemory
    shr ecx 2 | inc ecx | rep movsd

     move D$SortSource D$SortTempoMemory | add D$SortSource 5
     move D$SortDestination D$EquateList | add D$SortDestination 5

    Call SortEquatesStringsBySize

    VirtualFree D$SortTempoMemory
L9: ret


;;
 Source and Destination are Pointers to 2 Tables (same lenght). Source holds a set
 of zero ended strings to be sorted and stored in Destination. Source is overwritten
 with 0FF Bytes when finished.
;;
[SortSource: ?    SortDestination: ?    SortStringNumber: ?]

SortStrings:
    Mov ecx D$SortStringNumber, edi D$SortDestination

L0: push ecx

        Mov esi D$SortSource, ecx D$SortStringNumber, edx 0, bl 0FF

L1:     lodsb
        .If al = 0FF
            ; nop
        .Else_If al < bl
            Mov bl al | lea edx D$esi-1
        .Else_If al = bl
            push ebx
                push edx, esi
                    While al = bl
                        lodsb | inc edx | Mov bl B$edx
                        cmp al EOI | je L2>
                    End_While
L2:             pop esi, edx
                On al < bl, lea edx D$esi-1
            pop ebx
        .End_If

        While B$esi > EOI
            inc esi
        End_While
        add esi 11 | loop L1<
; add esi 11, because:
; |EquateName| D$ .... .... B$ .|
;            >    >>>> >>>>    >>^

        If edx > 0
            Mov esi edx
            While B$esi > EOI
                movsb | Mov B$esi-1 0FF
            End_While
            movsb  ; |
            movsd  ; Ptr to MacroData
            movsd  ; MacroData record length
            movsb  ; Flag
            movsb  ; |
        End_If

    pop ecx | dec ecx | cmp ecx 0 | ja L0<<
ret


; Pointers by Sizes: Each dWord will be (possibily -0, if none-) a Pointer to the
; position of the first Equate Equate in the List having the 'given' number of Chars.
; ('given' by the Routine that will finally *search* for the Equate body:
[LabelsPointersBySizes: EquatesPointersBySizes: ? #100]
[MacrosPointersBySizes: ? #100]

[SortBySizeOver: ?]

SortEquatesStringsBySize:
    Mov edi EquatesPointersBySizes, eax 0, ecx 100 | rep stosd

    Mov edi D$SortDestination, edx 1

L0: Mov esi D$SortSource, ecx D$SortStringNumber, B$SortBySizeOver &TRUE

; There is something wrong in this: If i state 'While B$esi <> EOI' instead of
; 'While B$esi > EOI', it doesn't work. It should, and there is no reason for what
; D$esi could be = 0 (this is to say after the end of the Table -i suppose...)...

L1:     lodsb
        .If al = 0FF        ; Done
            While B$esi > EOI | inc esi | End_While
            add esi 11

        .Else
            push ecx
                Mov eax esi, ecx 1
                While B$eax > EOI | inc ecx | inc eax | End_While
                If ecx = edx
                    Mov al B$esi-1, B$esi-1 0FF
                    On D$EquatesPointersBySizes+edx*4 = 0,
                        Mov D$EquatesPointersBySizes+edx*4 edi
                    stosb
                    dec ecx | jecxz L2>
                        rep movsb
L2:                 movsb  ; |
                    movsd  ; Ptr to MacroData
                    movsd  ; MacroData record length
                    movsb  ; Flag
                    movsb  ; |

                Else
                    lea esi D$eax+11
                End_If

            pop ecx
            Mov B$SortBySizeOver &FALSE
        .End_If
    loop L1<

    inc edx | cmp B$SortBySizeOver &FALSE | je L0<<
ret




; input: EBX = lenght of researched label set by caller; >ESI > first letter of new label

[ListNumerix: ?]  ; this value is to be add to tables pointers to jmp over numeric data
                  ; when searching for a name

[SearchMain: ?]
SearchForEntryPoint:
    Mov B$SearchMain &TRUE
        Call GetFromQwordCheckSum EntryPointLabel, D$LabelList, D$LabelListLimit
        If eax = 0
            Mov B$ErrorLevel 9
            error D$NoEntryPtr
        End_If

        While B$eax <> EOI | inc eax | End_While | inc eax
        or B$eax+4 DoneFlag
        Mov eax D$eax

    Mov B$SearchMain &FALSE
ret

 _________________________________________________________________________________________
;;
 replacements of equates and macro evocation of source file
                              source: < > A
 ReplaceMacAndEqu reads one word from source and Call IsItInMacroList. If yes, this
 called routine returns, after read in Macrolist, in ECX the lenght of stored text
 data in MacroData and in ESI the offset of these data.
 Then, ReplaceMacAndEqu calls ReplaceFromMacroData to read these data and write them
 in destination file (a new 'source file'). If a '#' symbol is encounted, in turn,
 ParametersUnfolding is called to read the source parameters
 ________________________________________________________________________________________

 input: InstructionBptr point one text word and OneWordLen gets its lenght; al = last
 character of a one word.
 to avoid a same symbol be booth a label and an equate/macro, no unfolding before ':'
;;

[OneWordLen: ?]

 _______________________________________________________________________________________
;;
 Effective replacement of macros:
 It allows loop if '#+n' statement found at the end of declaration, while getting
 control on parameters number when needed....
 Constant First parameter allowed with '#F'. Last one, with '#L'
 Reverse job allowed with '#n' and global replacement with #x>y'

 Very simple for user > very difficult inside...
;;
 _______________________________________________________________________________________

[AfterLastParameter: ?  NumberOfMacroParameters: ?   LastMacPara: ?
 FirstUnfolded: ?  LastUnfolded: ?  UnfoldingRotations: ?]

SearchLastParameter:
    push esi
        Mov B$LastMacPara '0', D$NumberOfMacroParameters 0 | Mov esi D$InstructionAptr

      ; Number of spaces >>> Number of Parameters:
        lodsb
        While al > EOI
            If al = space
L1:             inc B$LastMacPara
                inc D$NumberOfMacroParameters
           ; Else_If al = ColonSign
           ;     On B$esi <> EOI, jmp L1<
            End_If
            lodsb
        End_While

        dec esi | Mov D$AfterLastParameter esi
    pop esi
ret


[NoLabel: ?  NoSizeMarker: ?]

; Unfolds one parameter

ParameterUnfolding:
    Mov B$NoLabel &FALSE, B$NoSizeMarker &FALSE
    .If B$esi = '!'
        inc esi
        If B$esi = '!'
            inc esi | Mov B$NoSizeMarker &TRUE
        Else
            Mov B$NoLabel &TRUE
        End_If
    .End_If
    push ecx, esi
        Mov bl al | cmp bl 'F' | jne L0>    ; 'x' of #x in bl
            Mov bl '1' | jmp L6>>
L0:     cmp bl, 'L' | jne L1>
            Mov bl B$LastMacPara | jmp L6>>
L1:     cmp bl 'N' | jne L2>
            Mov bl B$LastMacPara ; | add bl B$MacrosX ; ah have been 'neg'ed
        ;    On bl = '1',  Mov B$FirstUnfolded &TRUE | jmp L6>>

L2:     On bl < '1',  error D$MacNumberPtr
        On bl > '9',  error D$MacNumberPtr
        add bl B$MacrosX | On bl = '1',  Mov B$FirstUnfolded &TRUE
        On bl = B$LastMacPara,  Mov B$LastUnfolded &TRUE
        ...If B$esi >= '0'
            ..If B$esi <= '9'
              ; Extreemely durty hack to inc _ESI_ and dec ECX: (!!!!!!!!!!!!!!!!!)
                pop eax, ecx
                    inc eax | dec ecx
                push ecx, eax
                sub bl '0' | lea ebx D$ebx*4+ebx | shl ebx 1
                lodsb | sub al '0' | add bl al
                add bl '0'
                .If B$esi >= '0'
                    If B$esi <= '9'
                        error D$TooBigXpTR
                    End_If
                .End_If
            ..End_If

        ...End_If

L6:     Mov esi D$InstructionAptr                ; switch from MacroData to clean Code Source (A)

L7:     lodsb                                  ; and search parameter
        On al =< EOI, error D$MissingParameterPtr
        cmp al space | jne L7<                 ; spaces count gives
        inc ecx
            dec bl | cmp bl, '0' | jne L7<       ; parameter position
        and B$esi 00_0111_1111                 ; Strip Equates-done-Flag, if any, because
                                                 ; it may corrupt macro-built-symbols.
        On B$NoSizeMarker = &TRUE, add esi 2

L8:     lodsb | cmp al Separators | jb L9>     ; read in sourceA
        stosb | jmp L8<                        ; write parameter in destination (SourceB)

L9: pop esi, ecx
    If B$NoLabel = &TRUE
       dec edi, ecx
       On B$edi = TextSign, error D$TextKillingPtr
    Else_If B$NoSizeMarker = &TRUE
        sub ecx 2
    End_If
ret


MultiParaCheck:
    cmp al 'F' | jne L0>
        Mov al '1' | jmp L9>
L0: cmp al 'L' | jne L1>
        Mov al B$LastMacPara | jmp L9>
L1: On al = 'N',  error D$NForbidenPtr
L2: On al < '1',  error D$MacNumberPtr
    On al > '9',  error D$MacNumberPtr
L9: ret
 ____________________________________________
;;
 #
 2   >  al  > bl  |               |  >  al
 >                |  >  xchg  >   |
 L          > al  |               |  >  bl
;;
 ____________________________________________


MultiParametersUnfolding:               ; #x>y  has been found  al = x
   ; On ah > 0, error MacNumber
    On B$MacrosX > 0, error D$MacNumberPtr
    Call MultiParacheck

    dec ecx | Mov bl al | lodsb         ; ecx = lenght from 'IsItInMacroList'
    dec ecx | lodsb                     ; bl = x   al = y
    Call MultiParacheck

    push esi, ecx
        Mov esi D$InstructionAptr              ; switch from MacroData to clean Code Source
        xchg al bl | cmp al bl | ja L2>

L1:     Call UnfoldOneOfMultiPara | inc al | cmp al bl | jna L1<
            jmp L9>

L2:     Call UnfoldOneOfMultiPara | dec al | cmp bl al | jna L2<

L9: pop ecx, esi
ret


UnfoldOneOfMultiPara:
    push eax ebx
        Mov bl al | Mov esi D$InstructionAptr

      ; Search parameter:
L1:     lodsb
        On al <= EOI, error D$MissingParameterPtr
      ; Spaces count gives parameter position:
        cmp al space | jne L1<
            dec bl | cmp bl, '0' | jne L1<

L2:     lodsb | cmp al, Separators | jb L8>
          ; Cases of Text Parameters (???...):
            If al = TextSign
                stosb
                While B$esi <> TextSign | movsb | End_While
                movsb | jmp L2<
          ; Case of {... #2>L} nested Declarations found in Macro Declarations:
            Else_If al = '}'
                error {"| wanted between Multiple Parameters and '}', in Macro Declaration", 0},
                      D$InstructionAptr
            End_If
          ; write parameter in destination
            stosb | jmp L2<
L3:         dec D$AfterLastParameter
      ; Space forced (avoid '|' when reading, at first, the last parameter)
L8:     Mov al space | stosb
L9: pop ebx eax
ret
;;
; I do not reCall the reason why i did this strange '}' thingie, previously:
L2:     lodsb
        cmp al, Separators | jb L9>
        cmp al '}' | je L3>
          ; write parameter in destination
            stosb | jmp L2<
L3:         dec D$AfterLastParameter
      ; Space forced (avoid '|' when reading at first the last parameter)
L9:     Mov al space | stosb                   
    pop ebx eax
ret
;;

;;
  These are the 128 Bytes Tables for storing whatever the user wants to store:
  The internal macros Variables, &0, &1,... , &99:
;;

[MacrosVariablesTable: ? #(32*102)]
[MacroCounters: ? #100]
[MACRO_VARIABLES 100]

ClearMacroVariable:
    Mov edi MacrosVariablesTable, ecx ((32*102)+100), eax 0 | rep stosd
ret


[MixedInternalVariables: 'You cannot mix Internal Variables by text and by Number', 0
 CounterSyntax: "Macros Counters syntax example:
&&21=&&21+1 // &&3=&&60-1 // &&5=0", 0]

StoreMacroVariableByNumber: ; StoreMacroVariable
  ; eax = Displacement to the MacroVariable Record, inside 'MacrosVariablesTable'

  ; esi is pointing after '&&23='

  ; Macros Variables Declarations are not true Statements.
  ; So, we have to strip the leading '|' (meEOI) previously written:
    ;dec edi

    push edi
        lea edi D$MacroCounters+eax*4

L0:     cmp B$esi EOI | jbe L9>>

        lodsb

        .If al = '&'
            lodsb | dec ecx
            On al <> '&', error MixedInternalVariables
                dec ecx
                Call ParseCounterToCounterAttribution
              ; Unwanted meEOI:
                ;dec ecx

        .Else_If al = TextSign
          ; skip first TextSign
            dec ecx
            Call GetAttributionChar | Mov D$edi ebx
          ; skip last TextSign
            inc esi | dec ecx

        .Else_If al = NumSign
            dec ecx | lodsb | dec ecx
            If al = 'N'
                move D$edi D$NumberOfMacroParameters
            Else
                error CounterSyntax
            End_If

        .Else_If al < '0'
            error CounterSyntax

        .Else_If al <= '9'
            dec esi
            Call GetAttributionNumber | Mov D$edi ebx

        .Else_If al = 'P'
            Call SaveStatementCounter | sub ecx 3

        .Else
            error CounterSyntax

        .End_If

        ;loop L0<
L9: pop edi
ret


;;
  Example: &&51=Pos (For the #If #ErrorPos).
  
  Saves the actual Source Parsing Pointer into a Macro Counter that the user will
  possibly reuse for having his private error message coming out, with the source
  being pointed to, eventually, at an upward position from the error detection.
;;
SaveStatementCounter:
    If W$esi = 'OS'
        Mov eax D$StatementsPtr
        Mov D$edi eax
        add esi 2
    Else
        error CounterSyntax
    End_If
ret


ParseCounterToCounterAttribution:
    Call GetMacroVariableDis | Mov eax D$MacroCounters+eax*4

    ..If ecx = 0
        ; eax ready

    ..Else_If B$esi <= EOI
        ; eax ready

    ..Else_If B$esi = AddSign
        inc esi | dec ecx
        Call GetAttributionNumber
        add eax ebx
        ;.If B$esi = '1'
        ;    inc eax | stosd | inc esi | dec ecx
        ;    If ecx <> 0
        ;        On B$esi > EOI, error CounterSyntax
        ;    End_If
        ;.Else
        ;    error CounterSyntax
        ;.End_If

    ..Else_If B$esi = SubSign
        inc esi | dec ecx
        Call GetAttributionNumber
        sub eax ebx
       ; .If B$esi = '1'
       ;     dec eax | stosd | inc esi | dec ecx
       ;     If ecx <> 0
       ;         On B$esi > EOI, error CounterSyntax
       ;     End_If
       ; .Else
       ;     error CounterSyntax
       ; .End_If

    ..Else
        error CounterSyntax

    ..End_If

    Mov D$edi eax
ret


[CounterAttribution: 'Bad Number Attribution for internal Counter', 0]

GetAttributionNumber:
    If W$esi = '00'
        Call GetAttributionBinary
    Else_If B$esi = '0'
        Call GetAttributionHexa
    Else_If B$esi < '0'
        error CounterAttribution
    Else_If B$esi <= '9'
        Call GetAttributionDecimal
    Else
        error CounterAttribution
    End_If
ret


GetAttributionBinary:
    Mov ebx 0 | inc esi | dec ecx

L0: If B$esi = '1'
        shl ebx 1 | or ebx 1
    Else_If B$esi = '0'
        shl ebx 1
    Else
        ret
    End_If
    inc esi | dec ecx | jnz L0<
ret


GetAttributionHexa:
    Mov ebx 0
    push eax
L0:     If B$esi < '0'
            pop eax | ret
        Else_If B$esi <= '9'
            shl ebx 4 | Mov al B$esi | sub al '0' | or bl al
        Else_If B$esi < 'A'
            pop eax | ret
        Else_If B$esi <= 'F'
            shl ebx 4 | Mov al B$esi | sub al '0' | sub al 7 | or bl al
        Else
            pop eax | ret
        End_If
        inc esi | dec ecx | jnz L0<
    pop eax
ret


GetAttributionDecimal:
    push eax
    Mov ebx 0
L0: lodsb
    If al < '0'
        ; Out
    Else_If al > '9'
        ; Out
    Else
        sub al '0'
        lea ebx D$ebx+ebx*4
        lea ebx D$eax+ebx*2
        dec ecx | jnz L0<
    End_If

    dec esi | pop eax
ret

[CounterTextAttribution: 'Max Text Attribution is 4 Chars, for a Counter', 0]

GetAttributionChar:
    Mov ebx 0

L0: or bl B$esi  | dec ecx | inc esi
    On B$esi = TextSign, ret
    shl ebx 8 | or bl B$esi  | dec ecx | inc esi
    On B$esi = TextSign, ret
    shl ebx 8 | or bl B$esi  | dec ecx | inc esi
    On B$esi = TextSign, ret
    shl ebx 8 | or bl B$esi  | dec ecx | inc esi
    On B$esi <> TextSign, error CounterTextAttribution
ret
____________________________________________________________________________________________

[WritingLowCounter:
'Only "0" to "9" or "A" to "Z" accepted.', "
Bad writing attempt with Counter &&"
 WritingCounter: 0, 0, 0]

WriteMacroVariableByNumber:
    Mov ebx eax
    Mov eax D$MacroCounters+eax*4 | stosb

    If al < '0'
        ; bad
    Else_If al > 'Z'
        ; bad
    Else_If al <= '9'
        ; good '1' to '9'
        ret
    Else_If al >= 'A'
        ; goo ''A' to 'Z'
        ret
    End_If

    Mov edi WritingCounter, eax ebx
    Call WriteEaxDecimal | Mov B$edi 0
    error WritingLowCounter
ret


StoreMacroVariable:
  ; eax = Displacement to the MacroVariable Record, inside 'MacrosVariablesTable'

  ; Macros Variables Declarations are not true Statements.
  ; So, we have to strip the leading '|' (meEOI) previously written:
    On B$edi-1 = meEOI, dec edi

    push edi
        lea edi D$MacrosVariablesTable+eax

L0:     cmp B$esi EOI | jbe L9>>
        lodsb                           ; read data from MacroData
        .If al = NumSign                ; if parameter, > unfolding
            lodsb | dec ecx
            If al = 'X'
                Mov al B$MacrosX | inc al | Call AlToDecimal | loop L0<

            Else_If al = 'N'
                Mov al B$NumberOfMacroParameters | Call AlToDecimal | loop L0<

            Else_If B$esi = '>'
                Mov bl B$LastMacPara
                cmp al '9' | ja L2>
                    cmp al bl | ja L3>
L2:             cmp bl '9' | ja L4>
                    cmp B$esi+1 'L' | je L4>
                        cmp B$esi+1 bl | jbe L4>
L3:             dec edi
                cmp B$edi ColonSign | je C1>
                    cmp B$edi EOI | ja L3<
                        jmp C2>
C1:             inc edi
C2:             add esi 2 | sub ecx 2

                jmp L6>
L4:             Call MultiParametersUnfolding
                dec edi
L6:             dec ecx

            Else
                Call ParameterUnfolding | dec ecx | jnz L0<<

            End_If

        .Else_If al = '&'
            Call GetMacroVariableDis | shl eax 7
            On B$esi = '=', error D$NestedMacroVariablePtr
            Call WriteMacroVariable
            jmp L8>
L7:         jmp L0<<
L8:         loop L7<

        .Else_If al = '!'
            dec edi
            jmp L8>
L7:         jmp L0<<
L8:         loop L7<

        .Else
            jmp L8>
L7:         jmp L0<<
L8:         stosb | loop L7<                    ; write body in MacroVx

        .End_If

L9:     Mov al 0 | stosb
    pop edi
ret


WriteMacroVariable:
    push esi, ebx
        lea esi D$MacrosVariablesTable+eax
        Mov ebx edi | add ebx 80

        If B$esi = 0
         ; This is commented out because it outputs NOPEs every now and then when unwished.
         ; I don't find example, when it could be a problem for Conditional Macros...

         ; Mov D$edi 'NOPE' | add edi 4
           pop ebx, esi | ret
        End_If
        If B$esi = Space
           inc esi
           ; jE! skip Space, if is 1st byte!
        End_If
        .While B$esi > 0
            If B$esi = '&'
                inc esi
                push ecx
                    Call GetMacroVariableDis | shl eax 7
                    On B$esi = '=', error D$NestedMacroVariablePtr

                    push esi
                        Call WriteMacroVariable
                    pop esi
                pop ecx
            Else
                movsb | On edi = ebx, error D$MacVarOverFlowPtr
            End_If
        .End_While

    pop ebx, esi
ret

____________________________________________________________________________________________

[NoMeanLabel: 'ZZZZZZZZ' 0    NoMeanEaten: 0]

; B$esi > '0' of '&0' when called:

DataNoMeanLabel:
    lodsb                                                ; strip '0'
    If B$esi = ColonSign                                 ; '&0:' Declaration
        On B$NoMeanEaten = &TRUE, error D$Double0Ptr
        Call CreateNoMeanLabel
        inc esi | sub ecx 3                              ; strip ':' and '&0:' count
        Call WriteNoMeanLabel
        Mov al ColonSign | stosb
        Mov B$NoMeanEaten &TRUE
    Else                                                 ; '&0' Evocation
        Call WriteNoMeanLabel
        sub ecx 2                                        ; strip '&0' count
  ;  Else
        ; i think no error check usefull here because compilation will fail later...
    End_If
ret


; Prepares a new Automatic Label Name, to be written by 'WriteNoMeanLabel':

CreateNoMeanLabel:
    push edi
        Mov edi NoMeanLabel | add edi 7
        dec B$edi
        While B$edi < 'A'
            Mov B$edi 'Z' | dec edi | dec B$edi
        End_While
    pop edi
ret


; Write the Automatic Label at edi (without trailing Colon):

WriteNoMeanLabel:
    push esi, ecx
        Mov esi NoMeanLabel, ecx 8 | rep movsb
    pop ecx, esi
ret

____________________________________________________________________________________________

; Writes al to edi in Decimal form:

AlToDecimal:
    push ecx, eax
        Mov ecx 10
        push 0-1
L1:     Mov ah 0 | div cl | add ah '0' | push eax | cmp al 0 | ja L1<

L2:     pop eax
        If eax <> 0-1
            Mov B$edi ah | inc edi | jmp L2<
        End_If
    pop eax, ecx
ret

;;
  Gets the Macro Variable Displacement, in the 'MacrosVariablesTable'.
  
  called from: 'ReplaceFromMacroData', 'StoreMacroVariable', plus 'WriteFromMacroVariable'
  in cases of nested MacroVariable.
;;

GetMacroVariableDis:
  ; esi points to the Number first Char after the '&':
    Mov al B$esi
    cmp al '9' | ja L0>
    cmp al '0' | jae L1>
L0:     error D$MacroVariableIndicePtr

L1: dec ecx
    push ebx
        Mov eax 0, ebx 0

L0:     lodsb | sub al '0' | add ebx eax
        cmp B$esi '0' | jb L9>
        cmp B$esi '9' | ja L9>
          ; ebx = ebx*10:
            lea ebx D$ebx*4+ebx | shl ebx 1 | loop L0<

L9:     Mov eax ebx
    pop ebx

  ; eax = &Variable Indice from 1 to 100:
    On eax > MACRO_VARIABLES, error D$MacroVariableIndicePtr

  ; eax = Displacement to the proper 128 Byte Record:
  ;  shl eax 7 ; Done by caller
ret


[MacrosX: ?]

; esi point to MacroData, ecx = length of data (set by IsItInMacroList)

testVariable:
    pushad
        Mov D$edi '////'
        Mov edx D$InstructionBptr | add edx 128
        Call ShowMaping D$InstructionBptr, edx, 0
    popad
ret


ReplaceFromMacroData:
    Call SearchLastParameter

    Mov edi D$InstructionBptr

    Mov B$MacrosX 0, B$FirstUnfolded &FALSE, B$LastUnfolded &FALSE, B$NoMeanEaten &FALSE

X0: push ecx, esi
      ; read data from MacroData:
L0:     lodsb

        ...If al = NumSign
          ; "#" Parameter, > Unfolding:
            Call ParseMacroParameter

            If ecx = 0-1
                pop esi, ecx | jmp X0<
            Else_If ecx > 0
               jmp L0<
            Else
                jmp L9>>
            End_If

        ...Else_If al = '&'

            ..If B$esi = '0'
            ; '&0' found:
                Call DataNoMeanLabel | cmp ecx 0 | ja L0<
                    jmp L9>>

            ..Else
                .If B$esi = '&'
                ; &&1... &&99 numbered Variable:
                    dec ecx | inc esi
                    cmp B$esi '0' | jb N0>
                    cmp B$esi '9' | ja N0>
                        Call ParserMacroVariableByNumber | cmp ecx 0 | jg L0<<
                        jmp L9>

                .Else_If B$esi < '1'
                  ; nop
                .Else_If B$esi > '9'
                  ; nop
                .Else
                  ; &1... &99 Variable:
                    Call ParseMacroVariable | cmp ecx 0 | ja L0<<

                    jmp L9>

                .End_If

            ..End_If
        ...End_If

N0:     stosb                          ; write
        On edi >= D$InstructionAEnd, error D$MacrosOverFlowPtr
        dec ecx | jnz L0<<
L9: pop esi, ecx                                 ; true stack restore
ret

____________________
; '#' NumSign found:

ParseMacroParameter:
    lodsb | dec ecx

    ..If al = SubSign
        On B$esi-3 > EOI, error D$BadMacroLoopPtr
      ; +/- in bl // 1/.../9 in al:
        Mov bl, al | lodsb | dec ecx

      ; Sign was "-":
        If B$FirstUnfolded = &FALSE
            sub al '0' | sub B$MacrosX al
            On B$MacrosX >= 080, error D$InfiniteLoopPtr
            On B$LastMacPara = '0', error D$MacNumberPtr

          ; Restart unfolding signal:
            Mov ecx 0-1

        Else
          ; job is done. strip lasting separator:
            dec edi
          ; if some more text after loop symbol (another one is ready from source)
            On ecx > 0, dec ecx

        End_If
        ret

    ..Else_If al = addSign
        On B$esi-3 > EOI, error D$BadMacroLoopPtr
        Mov bl, al | lodsb | dec ecx
      ; sign was "+":
        If B$LastUnfolded = &FALSE
            sub al '0' | add B$MacrosX al
            On B$MacrosX >= 080, error D$InfiniteLoopPtr
            On B$LastMacPara = '0', error D$MacNumberPtr

          ; Restart unfolding signal:
            Mov ecx 0-1

        Else
          ; job is done. strip lasting separator:
            dec edi
          ; if some more text after loop symbol (another one is ready from source)
            On ecx > 0, dec ecx

        End_If
        ret

    ..Else_If al = 'X'
        Mov al B$MacrosX | On al >= 080, neg al
        inc al | Call AlToDecimal

    ..Else_If al = 'N'
        Mov al B$NumberOfMacroParameters
        Call AlToDecimal

    ..Else_If al = '='
      ; Case of "#=4", for forced Parameters control:
        dec ecx | On ecx = 0, error D$MacParaPtr
        lodsb | sub al '0' | On al > 9, error D$MacNumberPtr
        add al '0' | On B$LastMacPara <> al, error D$MissingParameterPtr
        lodsb | dec ecx | On ecx = 0, error D$MacNumberPtr

    ..Else_If B$esi = '<'
        error D$BadMacroDirectionPtr

    ..Else_If B$esi = '>'
        Mov bl B$LastMacPara

        If al <= '9'
          ; 'x' expressed number out of parameter range?
            cmp al bl | ja L3>>
        End_If

        .If bl > '9'
          ; #x>y found (x in AL)
            Call MultiParametersUnfolding
            dec edi
        .Else_If B$esi+1 = 'L'
            Call MultiParametersUnfolding
            dec edi
        .Else_If B$esi+1 <= bl
            Call MultiParametersUnfolding
            dec edi
        .Else
L3:         Do
              ; if no parameter fitting with parameter in...
                dec edi

                If B$edi = ColonSign
                    inc edi | jmp C2>
                End_If
            Loop_Until B$edi <= EOI
          ; previous possible mnemonic written at L0:
C2:         add esi 2 | sub ecx 2
        .End_If

    ..Else
      ; #1/.../#9/#F/#L   found:
        Call ParameterUnfolding
    ..End_If

L6: dec ecx | ret

____________________________________________________________________________________________



ParserMacroVariableByNumber:
    dec ecx
    Call GetMacroVariableDis
    .If B$esi = '='
      ; strip '=', keep '1' in al
        inc esi | dec ecx
        If B$esi = Space
            inc esi | dec ecx
        End_If

        Call StoreMacroVariableByNumber

        If ecx = 0
            Mov D$edi 'NOPE' | add edi 4
        Else
          ; Strip next '|' (End >>> None >>> 'jg':
            inc esi | dec ecx
        End_If

    .Else
        Call WriteMacroVariableByNumber

    .End_If
ret


ParseMacroVariable:
  ; For the previous '&':
    dec ecx
    Call GetMacroVariableDis | shl eax 7
    .If B$esi = '='
        inc esi | dec ecx   ; strip '='
        Call StoreMacroVariable

      ;  If ecx = 0
      ;      Mov D$edi 'NOPE' | add edi 4
      ;  Else
      ;    ; Strip next '|' (End >>> None >>> 'jg':
      ;      inc esi | dec ecx
      ;  End_If

    .Else
        Call WriteMacroVariable

    .End_If
ret

 ________________________________________________________________________________________
;;
; NestingCheck compares source before and after unfolding (sourceA / SourceB)
; if something is different , a macro job has been done. We loop all this job
; the stupid way until it be found of no more use.
; When NestingCheck is called, ESI points either to SourceCodeB or to MacroData,
; depending of the replacement work: as text from ESI could be either shorter or
; longer than new text in EDI, a lenght for shorter > ECX > REPE CMPSB can't be known...
; We can't check if parameters were all used because it is allowed to transmit dummy
; parameters (parameters that would be written in macro evocation only for ease of read
; purpose, for exemple).
;
;
; Infinite loop is not completly impossible. exemple:
;        [ Mov | Mov #2 #1 ]
;          Mov eax, ebx      ; >>>>  Mov ebx, eax ... and so on.
;
; The only one solution i choose is to stop unfolding after a certain iterations number:
;;

[MacroJobIsOver: B$ ?]

NestingCheck:
    push esi, ecx, eax, edi
        Mov esi D$InstructionAptr, edi D$InstructionBptr, ecx 0

        Mov ah B$esi-1        ; exemple: with  [Api | push #L>2 | Call #1], an
        Mov al B$edi-1        ; api Call without parameter will be authorized :
        cmp ax ((EOI shl 8)+meEOI) | jne L0>
        ;cmp ax 0201 | jne L0> ; no error message. Compute > (02h)call(03h)Function(02)
            Mov B$edi-1 EOI    ; and not:                    (01h)call(03h)Function(02)

L0:     Mov ah B$esi,  al B$edi
        ;and eax 00_01111111__01111111
        inc esi | inc edi
        cmp ax ((EOI shl 8)+meEOI) ;0201 ; this appends when a nested macro evocation does'nt have fitting
            jne L1>             ; transmited parameters and is authorized (L>2 for example)
        Mov B$edi 2 | Mov al 2
L1:     cmp ah al | jne L2>                   ; AH <> AL          >  Job is not over
            cmp al meEOI | je L9>
            cmp al EOI | je L9>               ; AH = AL = '|' (12)    >  Job is over
            jmp L0<
L2:     On B$UnfoldingRotations = 0FF, error D$InfiniteLoopPtr
        Mov D$MacroJobIsOver &FALSE
L9: pop edi, eax, ecx, esi
ret


ReplaceOneMacro: ; esi = D$InstructionB, edi = D$InstructionA
    movsb | jmp L1>                           ; first separator

L0: stosb

L1: lodsb | IfItIsText L0<
        cmp al EOI | je N0>
            cmp al LowSigns | jb L0<          ; meEOI...
                dec esi | jmp L2>
N0:     cmp B$esi EOI | jne L2>               ; end marker = '||' > write it
            stosb | stosb | ret               ; and exit

L2: Mov D$InstructionAptr esi,  D$InstructionBptr edi,  D$OneWordLen 0

L3: lodsb | cmp al LowSigns | jb L4>          ; search now lenght of text word:
        inc D$OneWordLen | jmp L3<            ; lenght of one text word in OneWordLen

; now: one source word is pointed by InstructionBptr and OneWordLen contain the length.

L4: cmp al ColonSign | je L9>>                 ; is it a line label?
    Mov esi D$InstructionAptr

    and B$esi 00_0111_1111

    Call GetFromQwordCheckSum esi, D$MacroList, D$MacroListLimit
    cmp eax 0 | je L5>

        While B$eax > LowSigns | inc eax | End_While | inc eax
        Mov esi D$eax, ecx D$eax+4

        Call MacroWithIf

        Call ReplaceFromMacroData

        Call NestingCheck

        Mov esi D$AfterLastParameter | jmp L1<<

  ; direct writing:
L5: Mov esi D$InstructionAptr,  edi D$InstructionBptr

L6: lodsb | cmp al EOI | jbe L7>
        stosb | jmp L6<
L7:     dec esi | jmp L1<<

  ; write line labels:
L9: Mov esi D$InstructionAptr,  edi D$InstructionBptr
L8: lodsb | cmp al ColonSign | je L9>
        stosb | jmp L8<

L9: dec esi | jmp L1<<
____________________________________________________________________________________________

ReplaceEquates:
    movsb | jmp L1>                             ; strip first separator
L0: stosb
L1: lodsb | cmp al TextSign | jne C5>
        stosb
C0: lodsb | stosb | cmp al TextSign | jne C0<
        jmp L1<

C5: cmp al EOI | ja L2>                         ; all possible separators.
C6:     cmp B$esi EOI | jne L0<                 ; end marker = '||' > write it
            stosb | stosb
            ret

L2: cmp al LowSigns | jb L0<
    dec esi | Mov D$InstructionBptr esi,  D$InstructionAptr edi,  D$OneWordLen 0

L3: lodsb | cmp al LowSigns | jb L4>            ; search now lenght of text word
        inc D$OneWordLen | jmp L3<              ; > lenght in 'OneWordLen'

  ; now: one source word is pointed by InstructionBptr and OneWordLen contain the length
L4: Mov esi D$InstructionBptr
    test B$esi 00_1000_0000 | jnz L6>>           ; to avoid testing words not in list
    cmp B$esi '0' | jb L4>
    cmp B$esi '9' | ja L4>                     ; No need Parsing Numbers.
        Mov ecx 0 | jmp L5>

L4: ;Call IsItInEquateList                       ; > MacroData adress in esi, len in ecx
   ; On al = ColonSign, inc D$OneWordLen

    cmp al ColonSign | jne L4>
        Mov ecx 0 | jmp L5>

L4: Call GetFromQwordCheckSum esi, D$EquateList, D$EquateListLimit

    Mov ecx eax | cmp ecx 0 | je L5>
        While B$eax > LowSigns | inc eax | End_While | inc eax
        Mov esi D$eax, ecx D$eax+4

L5: cmp ecx 0 | ja L5>                          ; ecx = lenght from 'IsItInMacroList'
        Mov esi D$InstructionBptr
        or B$esi 00_1000_0000                   ; word was not in list > flag cancel next time
    jmp L6>

  ; write equate body from MacroData (equ and mac body are all in Macrodata)
L5: Mov edi D$InstructionAptr | rep movsb | Mov D$InstructionAptr edi
    Mov esi D$InstructionBptr | add esi D$OneWordLen | Mov D$InstructionBptr esi
    Mov D$MacroJobIsOver &FALSE | jmp L1<<

L6: ; direct writing from source:
    Mov esi D$InstructionBptr,  edi D$InstructionAptr,  ecx D$OneWordLen | rep movsb
    Mov D$InstructionBptr esi,  D$InstructionAptr edi | jmp L1<<


NewReplaceEquates:
    Mov D$MacroJobIsOver &TRUE, esi D$InstructionA, edi D$InstructionB

    .While B$esi <> 0
        Mov al B$esi

        ..If al = TextSign
            movsb | While B$esi <> TextSign | movsb | End_While

        ..Else_If al < LowSigns
            movsb

        ..Else_If al >= '0'
            .If al <= '9'
              ; Skip numbers:
L1:             While B$esi > LowSigns | movsb | End_While

            .Else
              ; Skip non-Equates:
                test B$esi 00_1000_0000 | jnz L1<

              ; Is it in EquateList:
                Call GetFromQwordCheckSum esi, D$EquateList, D$EquateListLimit

                If eax = 0
                  ; Not an Equate:
                    or B$esi 00_1000_0000 | jmp L1<

                Else
                  ; Equate body in 'MacroData' (equ and mac body are all in 'Macrodata'):
                    While B$eax > LowSigns | inc eax | End_While | inc eax
                    push esi
                        Mov esi D$eax, ecx D$eax+4
                        rep movsb
                    pop esi

                    Mov D$MacroJobIsOver &FALSE
                  ; Skip the parsed Equate:
                    jmp L1<

                End_If
            .End_If

        ..End_If
    .End_While

    Mov B$esi 0
ret





    movsb | jmp L1>                             ; strip first separator
L0: stosb
L1: lodsb | cmp al TextSign | jne C5>
        stosb
C0: lodsb | stosb | cmp al TextSign | jne C0<
        jmp L1<

C5: cmp al EOI | ja L2>                         ; all possible separators.
C6:     cmp B$esi EOI | jne L0<                 ; end marker = '||' > write it
            stosb | stosb
            ret

L2: cmp al LowSigns | jb L0<
    dec esi | Mov D$InstructionBptr esi,  D$InstructionAptr edi,  D$OneWordLen 0

L3: lodsb | cmp al LowSigns | jb L4>            ; search now lenght of text word
        inc D$OneWordLen | jmp L3<              ; > lenght in 'OneWordLen'

  ; now: one source word is pointed by InstructionBptr and OneWordLen contain the length
L4: Mov esi D$InstructionBptr
    test B$esi 00_1000_0000 | jnz L6>>           ; to avoid testing words not in list
    cmp B$esi '0' | jb L4>
    cmp B$esi '9' | ja L4>                     ; No need Parsing Numbers.
        Mov ecx 0 | jmp L5>

L4: ;Call IsItInEquateList                       ; > MacroData adress in esi, len in ecx
   ; On al = ColonSign, inc D$OneWordLen

    cmp al ColonSign | jne L4>
        Mov ecx 0 | jmp L5>

L4: Call GetFromQwordCheckSum esi, D$EquateList, D$EquateListLimit

    Mov ecx eax | cmp ecx 0 | je L5>
        While B$eax > LowSigns | inc eax | End_While | inc eax
        Mov esi D$eax, ecx D$eax+4

L5: cmp ecx 0 | ja L5>                          ; ecx = lenght from 'IsItInMacroList'
        Mov esi D$InstructionBptr
        or B$esi 00_1000_0000                   ; word was not in list > flag cancel next time
    jmp L6>

  ; write equate body from MacroData (equ and mac body are all in Macrodata)
L5: Mov edi D$InstructionAptr | rep movsb | Mov D$InstructionAptr edi
    Mov esi D$InstructionBptr | add esi D$OneWordLen | Mov D$InstructionBptr esi
    Mov D$MacroJobIsOver &FALSE | jmp L1<<

L6: ; direct writing from source:
    Mov esi D$InstructionBptr,  edi D$InstructionAptr,  ecx D$OneWordLen | rep movsb
    Mov D$InstructionBptr esi,  D$InstructionAptr edi | jmp L1<<

____________________________________________________________________________________________


_________________________________________________________________________________________
;;
 As many full passes as needed: one full for equate, one full for macros, ... and loop
 until job is found to be of no more use; this is to say when source and destination
 are the same. All this process is so made one more time than really needed. RosAsm
 Spoil most of compile time here. I have not found something faster with same flexibility.
 Setting a high bit on 'non equates words' saves about 1/7 of compile time.
;;
_________________________________________________________________________________________

ClearDoneHighBit:
    Mov esi D$CodeSourceA, ecx D$StripLen, B$InsideText &FALSE
L0: On B$esi = TextSign, xor B$InsideText &TRUE
    On B$InsideText = &FALSE, and B$esi 00_0111_1111
L9: inc esi | loop L0<
  ret


ReplaceEquOnly:
    Mov B$UnfoldingRotations 0
L0: inc B$UnfoldingRotations | On B$UnfoldingRotations = 0FF, error D$InfiniteLoopPtr

    Mov D$MacroJobIsOver &TRUE
    Mov esi D$InstructionA, edi D$InstructionB

    Call ReplaceEquates                         ; SourceB  >  SourceA

    Mov ecx edi | sub ecx D$InstructionB
    Mov D$StripLen ecx

    If B$MacroJobIsOver = &FALSE
        Exchange D$InstructionA D$InstructionB
        Exchange D$InstructionAEnd D$InstructionBEnd | jmp L0<<
    End_If

    Mov esi D$InstructionB, edi D$InstructionA
    Call ExpressionParser                       ; SourceA  >  SourceB
    Mov ecx edi | sub ecx D$InstructionA | Mov D$StripLen ecx

 ;  cmp D$MacroJobIsOver &TRUE | jne L0<        ; set by 'NestingCheck'
ret                                             ; when over > SourceB


NewReplaceEquOnly:
    Mov B$UnfoldingRotations 0
L0: inc B$UnfoldingRotations | On B$UnfoldingRotations = 0FF, error D$InfiniteLoopPtr

    Call NewReplaceEquates                         ; SourceB  >  SourceA

    Mov ecx edi | sub ecx D$InstructionB
    Mov D$StripLen ecx

    If B$MacroJobIsOver = &FALSE
        Exchange D$InstructionA D$InstructionB
        Exchange D$InstructionAEnd D$InstructionBEnd | jmp L0<<
    End_If

    Mov esi D$InstructionB, edi D$InstructionA
    Call ExpressionParser                       ; SourceA  >  SourceB
    Mov ecx edi | sub ecx D$InstructionA | Mov D$StripLen ecx

 ;  cmp D$MacroJobIsOver &TRUE | jne L0<        ; set by 'NestingCheck'
ret                                             ; when over > SourceB




ReplaceMacAndEqu:
    Mov B$UnfoldingRotations 0
L0: inc B$UnfoldingRotations | On B$UnfoldingRotations = 0FF, error D$InfiniteLoopPtr

    Mov D$MacroJobIsOver &TRUE
    Mov esi D$InstructionA, edi D$InstructionB

        Call ReplaceEquates ; zReplaceEquates

        Mov ecx edi | sub ecx D$InstructionB | Mov D$StripLen ecx
            If B$MacroJobIsOver = &FALSE
                Exchange D$InstructionA D$InstructionB
                Exchange D$InstructionAEnd D$InstructionBEnd | jmp L0<<
            End_If

            .Do
                Mov B$MacroModifiedExpression &FALSE

                    Call ParaMacrosParser

                        Mov esi D$InstructionB, edi D$InstructionA

                        Call ExpressionParser

                          ; We have to do this here (and not before the RET of
                          ; 'ExpressionParser') because 'ExpressionParser' is
                          ; also called from 'ReplaceEquOnly':
                            Mov ecx edi | sub ecx D$InstructionA | Mov D$StripLen ecx
                            Exchange D$InstructionA D$InstructionB
                            Exchange D$InstructionAEnd D$InstructionBEnd
                 inc B$UnfoldingRotations
                 On B$UnfoldingRotations = 0FF, error D$InfiniteLoopPtr

            .Loop_Until B$MacroModifiedExpression = &FALSE

        Mov esi D$InstructionB, edi D$InstructionA

        Call ReplaceOneMacro

        ;;;;;;;;;;;;;;;;;;; Remove EOI, meEOIs and Spaces right after an EOI:
        Mov D$edi 0

        Mov esi D$InstructionA, edi esi
        .While B$esi <> 0
            lodsb | stosb
            If al < Separators
                While B$esi = meEOI | inc esi | End_While
            End_If
        .End_While
        ;;;;;;;;;;;;;;;;;;

        Mov ecx edi | sub ecx D$InstructionA

      ; If D$InstructionA = EOI, EOI (Cases of empty outputs - impossible actually -):
L2:     If ecx <= 2
            Mov edi D$InstructionA, B$edi EOI, D$edi+1 'NOPE', B$edi+5 EOI
            Mov D$StripLen 6 | ret
        End_If

        If ecx <> D$StripLen
            Mov D$MacroJobIsOver &FALSE
           ; On B$UnfoldingRotations = 0FF, hexprint 6
            On B$UnfoldingRotations = 0FF, error D$InfiniteLoopPtr
        End_If
        Mov D$StripLen ecx

L9: cmp D$MacroJobIsOver &TRUE | jne L0<<                          ; set by 'NestingCheck'
ret                                                                ; when over > SourceB

____________________________________________________________________________________________

zReplaceEquOnly:
    Mov B$UnfoldingRotations 0
L0: inc B$UnfoldingRotations | On B$UnfoldingRotations = 0FF, error D$InfiniteLoopPtr

    Call zReplaceEquates

    Mov ecx edi | sub ecx D$InstructionB
    Mov D$StripLen ecx

    Exchange D$InstructionA D$InstructionB,
             D$InstructionAEnd D$InstructionBEnd

    On B$MacroJobIsOver = &FALSE, jmp L0<<

    Call zExpressionParser
ret


zReplaceEquates:
    Mov D$MacroJobIsOver &TRUE, esi D$InstructionA, edi D$InstructionB

    .While B$esi <> 0
        Mov al B$esi

        ..If al = TextSign
            movsb | While B$esi <> TextSign | movsb | End_While | movsb

        ..Else_If al < LowSigns
            movsb
            On B$esi = '0', jmp L1> ; In case, if the 1st found byte is '0', it obviusly anumbers, and also means
                                    ; it is TextSign, so simply copy the whole data.
        ..Else_If al < '0'
          ; Example: '.If':
            jmp L2>

        ..Else_If al >= '0'
            .If al <= '9'
              ; Simple copy (numbers, non-Equates):
L1:             While B$esi > LowSigns | movsb | End_While

            .Else
              ; Skip non-Equates:
L2:             test B$esi 00_1000_0000 | jnz L1<

              ; Is it in EquateList:
                Call GetFromQwordCheckSum esi, D$EquateList, D$EquateListLimit
                If eax = 0
                    ; Not an Equate:
                    ; the OR will only happens if the Byte is > '9' and <= LowSigns
                    On B$esi <= LowSigns, or B$esi 00_1000_0000
                    jmp L1<

                Else
                  ; Equate body in 'MacroData' (equ and mac body are all in 'Macrodata'):
                    While B$eax > LowSigns | inc eax | End_While | inc eax
                    push esi
                        Mov esi D$eax, ecx D$eax+4
                        rep movsb
                    pop esi

                    Mov D$MacroJobIsOver &FALSE
                  ; Skip the done Equate:
                    While B$esi > LowSigns | inc esi | End_While

                End_If
            .End_If

        ..End_If
    .End_While

    Mov B$edi 0
ret



zExpressionParser:
    Mov esi D$InstructionA, edi D$InstructionB
    Mov edx esi | add edx D$Striplen | inc edx
    Mov B$InsideExpression &FALSE, B$RealHexaSize 0
    Mov ebx 0, ecx 0

L0: .While esi < edx
        lodsb

        ..If al = TextSign
            stosb | While B$esi <> TextSign | movsb | End_While | movsb | jmp L0<

        ..Else_If al = OpenSign
            .If B$InsideExpression = &FALSE
                cmp B$esi-2 memMarker | jne L3>
                Mov al B$esi-3 | and al 00_0111_1111 ; Mask Equates and Macros Done Flag.
                cmp al 'F' | jne L1>
                    Mov B$edi-2 'D', B$RealHexaSize 8 | jmp L2>
L1:             cmp al 'R' | jne L3>
                    Mov B$edi-2 'Q', B$RealHexaSize 16
L2:                 Mov B$RealExpression &TRUE | jmp L4>
L3:             Mov B$RealExpression &FALSE
L4:             Mov ebx 0, ecx 0
                Mov D$StartOfSourceExpression esi, D$StartOfDestinationExpression edi
            .End_If
            inc ebx | inc ecx | Mov B$InsideExpression &TRUE

        ..Else_If al = CloseSign
            On ecx = 0, error D$ParenthesisPtr
            dec ecx

        ..Else_If al = 0
            On B$InsideExpression = &TRUE, error D$ParenthesisPtr
            Call CheckBracketExpression

        ..End_If

        ..If ebx > 0
            .If ecx = 0
                push edx
                    If B$RealExpression = &TRUE
                        Call ComputeRealExpression
                    Else
                        Call ComputeExpression
                    End_If
                    Call zWriteExpressionResult
                    Mov ebx 0, ecx 0, B$InsideExpression &FALSE
                    Mov B$MacroModifiedExpression &TRUE
                pop edx
                jmp L0<<
            .End_If
        ..End_If

        stosb
    .End_While

    Mov ecx edi | sub ecx D$InstructionB | Mov D$StripLen ecx

    Exchange D$InstructionA D$InstructionB,
             D$InstructionAEnd D$InstructionBEnd
ret


zWriteExpressionResult:
    push esi
        Mov esi D$ExpressionA, edi D$StartOfDestinationExpression

        While B$esi > 0
            movsb
        End_While
    pop esi
ret
____________________________________________________________________________________________


;;
        Mov ecx edi | sub ecx D$InstructionA

      ; If D$InstructionA = EOI, EOI (Cases of empty outputs - impossible actually -):
        If ecx <= 2
            Mov edi D$InstructionA, B$edi EOI, D$edi+1 'NOPE', B$edi+5 EOI
            Mov D$StripLen 6 | ret
        End_If

        If ecx <> D$StripLen
            Mov D$MacroJobIsOver &FALSE
           ; On B$UnfoldingRotations = 0FF, hexprint 6
            On B$UnfoldingRotations = 0FF, error D$InfiniteLoopPtr
        End_If
        Mov D$StripLen ecx

L9: cmp D$MacroJobIsOver &TRUE | jne L0<<                          ; set by 'NestingCheck'
ret                                                                ; when over > SourceB
;;

;;
 In previous versions, 'ReplaceMacAndEqu' was called from AsmMain and all replacement job
 was done at once. This new version cuts the job in as tiny parts as possible in order
 to avoid no use loop analyzes.

 First, as Data brackets are stored at the begining of the cleaned source (and can't
 contain any Macro), we compute them one set by one set, only Equates replacement.

 Second, each instruction is done in turn. All this saves about 15% of replacement job.
 2 wide tables are used to store the parts, and exchanges are done between the pointers
 just because i do not wish to rewrite it all for more accurate namings.
;;

[InstructionA: ?  InstructionB: ?   InstructionAEnd: ?  InstructionBEnd: ?
 CodeSourceBpointer: ?  CodeSourceApointer: ?
 OpenType: ?  CloseType: ?]




;;
  NewReplaceMacAndEqu
  
        zReplaceEquOnly
                zReplaceEquates
                zExpressionParser
        
        ReplaceMacAndEquStatement
                zReplaceEquates
                ParaMacrosParser
                        UnfoldDataParameter
                        UnfoldMacroParameter
                zExpressionParser
                        CheckBracketExpression
                        ComputeRealExpression
                        ComputeExpression
                        WriteExpressionResult
                ReplaceOneMacro
                        MacroWithIf
                        ReplaceFromMacroData
                        NestingCheck
;;

NewReplaceMacAndEqu:
  ; Initializations:

    Call ClearMacroVariable

    VirtualAlloc InstructionA D$StripLen
    add eax D$StripLen | Align_On 01000 eax | Mov D$InstructionAEnd eax

    VirtualAlloc InstructionB D$StripLen
    add eax D$StripLen | Align_On 01000 eax | Mov D$InstructionBEnd eax

    Mov B$ErrorLevel 0, D$BracketCounter 0

    move D$StatementsPtr D$StatementsTable,
         D$CodeSourceBpointer D$CodeSourceB,
         D$CodeSourceApointer D$CodeSourceA

    Mov edi D$CodeSourceBpointer

;;
  The Source has been re-organized: Brackets first // Code Statements last. The
  Brackets sets do not need anything else but the Equates jobs. So, we parse them,
  first. Once an EOI is encounted, this first job is over:
;;

L0: Mov esi D$CodeSourceApointer, edi D$InstructionA
    cmp B$esi EOI | je L0>>

  ; Take a copy one Bracket Statement:
    Mov al B$esi ; copy B$esi to al. Forget the lodsb because it will increment esi, bypassing the good pointer
    Mov B$OpenType al; <--- I added only this to it points to the proper place on the next instruction, so what is being copied to edi is the starting byte (014)

    Do
        lodsb | stosb
        If al = TextSign
L1:         lodsb | stosb | cmp al TextSign | jne L1<
        End_If
        On B$esi-1 = CloseVirtual, jmp C1>
    Loop_Until B$esi-1 = CloseBracket

C1: Mov al B$esi-1, B$CloseType al
  ; Keep track of the real Source:
    Mov D$CodeSourceApointer esi
  ; Set an end-mark:
    Mov B$edi 0 ; <-------- Here i added too.. We need to set the last byte to 0, and not edi-1.
                ; Othewise we are erasing the good final ending mark (edi was already incremented during the loop,
                ; so the proper place is edi and not edi-1

    inc D$BracketCounter

    Call zReplaceEquOnly ; <<<<<<<<<<<<<<<<

    add D$StatementsPtr 4

  ; Copy the Bracket Statement back to CodeSourceB:
    Mov esi D$InstructionA, edi D$CodeSourceBpointer

    Mov al B$OpenType | stosb
    inc esi ; <---- Ok we want to copy to edi, but on stosb we already copied the 1st byte 014 to edi. So, to avoid duplications, we simply increment esi to the next byte
    While B$esi <> 0 | movsb | End_While ; copy the whole thing to edi
    dec edi ; decrease edi to we put the proper close type here
    Mov al B$CloseType | stosb ; put the CoseType at the real end of our data. (After the string)

    Mov D$CodeSourceBpointer edi | jmp L0<<
____________________________________________________

L0: Mov esi D$CodeSourceApointer
    sub D$StatementsPtr 4
    Mov B$ErrorLevel 0, D$StatementsCounter 0

[LenghtOfBracketStatements: ?]

    Mov eax D$CodeSourceApointer | sub eax D$CodeSourceA | Mov D$LenghtOfBracketStatements eax

  ; Take a copy for the Macros jobs:
L0: Mov edi D$InstructionA

    Mov al EOI | stosb
    Do
        movsb
    Loop_Until B$esi-1 = EOI

    Mov D$CodeSourceApointer esi | Mov al EOI | stosb | stosb

    inc D$StatementsCounter | Call ReplaceMacAndEqu  ;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    add D$StatementsPtr 4

    Mov esi D$InstructionA, edi D$CodeSourceBpointer | lodsb

L5: lodsb | stosb | cmp al EOI | jne L5<

    Mov D$CodeSourceBpointer edi | Mov esi D$CodeSourceApointer | cmp B$esi EOI | jne L0<<
    movsb

    Exchange D$CodeSourceA D$CodeSourceB

    push edi
        inc edi | sub edi D$CodeSourceA | Mov D$StripLen edi
    pop edi
    Mov eax ((EOI shl 24)+(EOI shl 16)+(EOI shl 8)+EOI)  | stosd   ; security

    VirtualFree D$InstructionA, D$InstructionB
    Call ClearDoneHighBit
ret

____________________________________________________________________________________________
;;
  'NewBrackets' makes nested Declarations of '[...]' available for next Equates and
  Macros job pass. We have to replace all substitutes by the true signs ('{' > '[',
  and so on). But we have a difficult problem to solve first: These new Declarations
  will be move to top of 'file' by next treatements, and in case of user error search
  among these new created statements, we have to be able to point out the wrong
  original Statement (the user macro Evocation). So have we to add some source pointers
  in 'StatementsTable', with the same pointer value as the one of user Macro. One
  added difficulty is that, is some case, exemple:

  > [CreateEquatesMacro | {#1 #2} | +2]

  ... the original Macro was stored as a Code Statement into 'StatementsTable'. After
  unfolding, there is no more code at this place. So have we to strip one record for
  this, in such cases:
;;
[MoreBracket: ?    InsideNewBracket: ?    TrueCodeInside: ?]

StatementStep:
    Mov ebx D$StatementsPtr, ebx D$ebx
    Mov edx D$StatementsPtr2, D$edx ebx | add D$StatementsPtr2 4

   ; Mov edx D$StatementsPtr+4
   ; While D$edx = ebx
   ;     Mov edx D$StatementsPtr2, D$edx ebx
   ;     add D$StatementsPtr 4 | add D$StatementsPtr2 4
   ;     add D$StatementsPtr 4
   ;     Mov edx D$StatementsPtr
   ; End_While
ret

NewBrackets:
    Mov esi D$CodeSourceA
    Mov ecx esi | add ecx D$Striplen                    ; end of source adress

    move D$StatementsPtr D$StatementsTable, D$StatementsPtr2 D$StatementsTable2

    ..While esi < ecx
L0:    lodsb
;On D$esi = 'ENTS', int3
       ...If al = TextSign
L1:         lodsb | cmp al TextSign | jne L1<

       ...Else_If al = '{'
         ; Possible "Bracket only" statement:
      ; On D$esi+15 = 'BIDO', int3
       ; MainWindowProc@BIDON
           ..If B$esi-2 = EOI
               sub D$StatementsPtr 4 | sub D$StatementsPtr2 4
               Mov B$TrueCodeInside &FALSE
               .While al <> EOI
                   If al = '{'
                       Mov ebx D$StatementsPtr, ebx D$ebx
                       Mov edx D$StatementsPtr2, D$edx ebx | add D$StatementsPtr2 4
                       While al <> '}'
                           On al = textSign, Call SkipText
                           lodsb
                       End_While
                      ; dec esi ; <<<<<<<< New add while tracking the "Unfolder bug".

                   Else_If al = meEOI
                       On B$esi = EOI, jmp L2>
                       On B$esi <> '{', Mov B$TrueCodeInside &TRUE

                   Else_If al = TextSign
                       Call Skiptext

                   End_If

L2:                lodsb
               .End_While

               If B$TrueCodeInside = &TRUE
                   Mov ebx D$StatementsPtr, ebx D$ebx
                   Mov edx D$StatementsPtr2, D$edx ebx | add D$StatementsPtr2 4
               End_If
               add D$StatementsPtr 4
               dec esi

           ..Else
             ; (B$esi-2 = meEOI // al = '{') ---> code sure inside...
               sub D$StatementsPtr 4
               .While al <> EOI
                   If al = '{'
                       Mov ebx D$StatementsPtr, ebx D$ebx
                       Mov edx D$StatementsPtr2, D$edx ebx | add D$StatementsPtr2 4
                       While al <> '}'
                            On al = textSign, Call SkipText
                            lodsb
                       End_While

                   End_If
L2:                lodsb
               .End_While

               dec esi
               add D$StatementsPtr 4

           ..End_If

       ...Else_If al = OpenBracket
           Mov ebx D$StatementsPtr, ebx D$ebx | add D$StatementsPtr 4
           Mov edx D$StatementsPtr2, D$edx ebx | add D$StatementsPtr2 4

       ...Else_If al = OpenVirtual
           Mov ebx D$StatementsPtr, ebx D$ebx | add D$StatementsPtr 4
           Mov edx D$StatementsPtr2, D$edx ebx | add D$StatementsPtr2 4

       ...Else_If al = EOI
           On B$esi = OpenVirtual, jmp L3>
           On B$esi = OpenBracket, jmp L3>
               Mov ebx D$StatementsPtr, ebx D$ebx | add D$StatementsPtr 4
               Mov edx D$StatementsPtr2, D$edx ebx | add D$StatementsPtr2 4
L3:
       ...End_If

    ..End_While

    Mov eax D$StatementsPtr2, D$eax 0
    Exchange D$StatementsTable D$StatementsTable2
________________________

; Now, we simply replace substitutes by true Chars. Use of BL reg as a 'Level Counter'
; in order to allow infinite nestings.

    Mov B$MoreBracket &FALSE, B$InsideNewBracket &FALSE
    Mov esi D$CodeSourceA,  edi D$CodesourceB, ebx 0

    .While esi < ecx
L0:    lodsb
       .If al = TextSign
         ; Skip Text:
L1:        stosb | lodsb | cmp al TextSign | jne L1<

       .Else_If al = '{'
           inc bl | On bl > 1, jmp L8>
           xor B$InsideNewBracket &TRUE
           On B$esi = Space, lodsb
           Mov al OpenBracket, B$MoreBracket &TRUE

       .Else_If al = '}'
           dec bl | jnz L8>
           xor B$InsideNewBracket &TRUE
           On B$esi = Space, lodsb
           ;On B$edi-1 = Space, dec edi
           ;On B$edi-1 = meEOI, dec edi
           On B$edi-1 < Separators, dec edi
           Mov al CloseBracket

       .Else_If al = '%'
           If B$InsideNewBracket = &TRUE
               On bl = 1, Mov al NumSign
               On bl = 2, Mov al NumSign

           End_If

       .End_If

L8:    stosb

    .End_While

    Mov eax ((EOI shl 24)+(EOI shl 16)+(EOI shl 8)+EOI) | stosd
    Mov eax edi | sub eax D$CodeSourceB | Mov D$Striplen eax

   ; Call TestStatementsTable
    Exchange D$CodeSourceA D$CodeSourceB
ret


SkipText:
    lodsb | While al <> TextSign | lodsb | End_While
ret
__________________________________________________________________________________________
__________________________________________________________________________________________

; PE Import section construction

; uses 3 tables:   ApiListA to store zero ended api calls
;                  ApiListB to store function name with a number at first for Dll
;                  DllList  to store Dll names
__________________________________________________________________________________________


[ALEOD 0FF]       ; Api Lists End Of Data


[Align_on | add #2 #1-1 | and #2 0-#1]
; Same for Aligning on a Variable-defined-Alignment (eax broken):
[Align_on_Variable | push eax
                         Mov eax #1 | dec eax | add #2 eax | xor eax 0-1 | and #2 eax
                     pop eax]


[DllNumber: 0  FunctionNumber: 0  ImportHeaderPtr: 0   ImportTablePtr: 0]


InitApiTables:
   Mov ax 0FF00                               ; in memory: 00 FF end mark
   Mov edi D$ApiListA | Mov W$edi ax
   Mov edi D$ApiListB | Mov W$edi ax
   Mov edi D$DllList  | Mov W$edi ax
ret
;;
 Test that api Call is in the good form:  Call 'LIB.Function' (one point, no space, not
 open text and turn dll name upper case to prevent from double storage).
 Return: EBX = lenght / ECX = lenght-1  (- last "'")
;;
TestGoodApiCall:
    push esi
      Mov ecx 0,  ebx 0                       ; ebx = '.' counter (must be 0 / 1 / or 2)
      push esi
        While B$esi <> TextSign | On B$esi = '.', inc ebx | inc esi | End_While
      pop esi
      On ebx = 0, jmp L9>

      Mov ebx 0
L0:   lodsb | inc ecx | cmp ebx, 0 | ja L1>   ; if ebx = 0  >>>  before '.'  >>>  dll name
        cmp al 'a' | jb L1>
        cmp al 'z' | ja L1>
          sub al 32                     ; turn upper case dll name
          Mov B$esi-1 al                ; rewrite CodeSourceB
L1:   On al = '.',  inc ebx
      cmp al Space | je L8>
      cmp ecx 1600 ; 150 ; 125 ; 120 |
      je L8>              ; too long
      cmp al TextSign | jne L0<
      cmp ebx 3 |  jb L9>               ; only one '.' wanted
                                        ; ... or 2 in case of Module given Extension...
L8:       Mov B$ErrorLevel 6
          Mov edi D$ApiListA            ; just to store zero ended name for error search
          pop esi
              While B$esi > TextSign
                  movsb
              End_While
              Mov al 0 | stosb
              Mov esi D$ApiListA
              Error D$BadApiPtr

L9: pop esi
ret
 ________________________________________________________________________________________

 ; storage of all encounted api calls in ApilistA:

StoreApi:                                ; 5 =  Call 'LIBRARY.Function'
  ; add esi 5                            ;       .....^
    Call TestGoodApiCall
L0: lodsb | cmp al TextSign | je L1>
      stosb | jmp L0<                    ; write in ApilistA
L1: Mov al 0 | stosb
ret


; Simple copy of api calls into 'ApiListA' by 'StoreApi'.
; 'ApiListA' will be: LIBRARY.Function, 0, LIBRARY.Function, 0, ... 0FF

SearchForApis:
    Mov esi D$CodeSourceB,  edi D$ApiListA

    ;add esi D$LenghtOfBracketStatements

    While B$esi <> EOI | inc esi | End_While | inc esi

  ; Regular Rosasm Api calls:
L0: ...If D$esi = 'CALL'        ; Call '
L1:     If W$esi+4 = ((TextSign shl 8)+Space)
            add esi 6 | Call StoreApi | jmp L0<
        End_If
;;
  For fancyful things for Disassemblies re-compilation: Store in eax +1 for each TextSign,
  +1 for each '.', +1 if lenght > 6.
  
  Must have: 1 or two '.' // 2 TextSigns // be longer than '..xx' (6 Chars).
;;
    ...Else_If B$esi = Textsign
        inc esi | Mov eax 1, ebx esi

        While B$esi > LowSigns
            If B$esi = '.'
                inc ah
           ; Else_If B$esi < '0'  ; Dement Chars in some Api Names !!!!.... :(
           ;      jmp L5>
            Else_If B$esi > 'z'
                jmp L5>
            End_If

            inc esi
        End_While

        On B$esi = TextSign, inc al

        On ah = 2, dec ah       ; 2 '.' allowed in:  Mov eax 'MODULE.ext.Funtion'

        .If eax = 01_02
            Mov ecx esi | sub ecx ebx
            If ecx > 6
                Mov esi ebx | Call StoreApi | jmp L0<<
            End_If
        .End_If

    ...End_If

L5: lodsb | cmp al EOI | jne L0<<
    cmp B$esi EOI | jne L0<<                     ; End of file reached?

    .If edi = D$ApiListA
        On D$SavingExtension = '.DLL', jmp L9>

            Call 'USER32.MessageBoxA' D$H.MainWindow,
            {"Are you sure you want to continue assemblying this?" 0},
            D$NoApiPtr, &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION__&MB_YESNO
            If eax = &IDNO
                Mov B$CompileErrorHappend &TRUE, D$NextSearchPos 0
                Call CloseProgressBar
                cld | Mov esp D$OldStackPointer
                Call ReleaseAsmTables
                ret ; for 'no search' errors (esi known) or 'Main:' missing
           End_If
           jmp L9>
    .End_If

    Mov al ALEOD |  stosb                       ; end mark for ApiList (strings are zero ended)
L9: ret

;;
'ApiListA' may contain 3 different forms o api calls: 
> MODULE.Function, 0, MODULE.ext.Function, 0, Function, 0...
We turn them all into:
> MODULE.ext.Function, 0, MODULE.ext.Function, 0, MODULE.ext.Function, 0...
;;
FullfillApiList:
    Mov D$ModuleHandlesListPointer ModuleHandlesList

  ; First generalise the found MODULE(s) to all naked 'Function'(s)
    Mov esi D$ApiListA, edi D$ApiListB

    Call FullfillWithExtensions

    Mov esi D$ApiListA | While B$esi = 0 | inc esi | End_While

    Call FullfillWithoutExtensions

    Mov B$edi ALEOD | Exchange D$ApiListA D$ApiListB

    Mov ebx D$ModuleHandlesListPointer, D$ebx 0

; Test for viewing the new Api List:
;;
    Mov esi D$ApiListA
    .While B$esi <> ALEOD
        howMe esi
        While B$esi <> 0 | inc esi | End_While | inc esi
    .End_While
    Error BadApi
;;

  ; Now, verify that 'ApiListB' is entirely zeroed:

    Mov esi D$ApiListB
    .While B$esi <> ALEOD
        If B$esi <> 0
            Mov B$ErrorLevel 6 | Error D$BadApiPtr
        End_If
        inc esi
    .End_While
ret


FullfillWithExtensions: ; 'FullfillWithoutExtensions'
    ..While B$esi <> ALEOD
        Mov ebx esi, edx 0, ecx 0

        While B$esi <> 0
            inc esi
            .If B$esi = '.'
                If edx = 0
                    lea edx D$esi+1
                Else_If ecx = 0
                    Mov ecx edx | lea edx D$esi+1
                Else
                    Error D$BadApiPtr
                End_If
            .End_If
        End_While
      ; Here, edx > Start of Function Name,
      ;       ecx > Start of Module Extension,
      ;       ebx > Start of Module Name
      ;       esi > ending 0
        ..If ecx <> 0
          ; Cases of 'MODULE.ext.Function':
            Mov eax edx | sub eax ecx
            If eax > 4
                Mov B$ErrorLevel 6, esi ebx
                error D$BadApiPtr
            End_If
            Mov esi ebx, D$StartOfDllName edi, D$StartOfFunctionName edx

            While B$esi <> '.' | movsb | End_While | movsb

          ; HAL DLL cannot be loaded in User Mode (consider it like a .sys):
            Mov B$WasSysModule &FALSE

            Call IsItHAL

          ; Also, .sys Modules cannot be loaded like a DLL:
            Mov eax D$esi | or eax 0202020
            On eax = 'sys.', Mov B$WasSysModule &TRUE

            While B$esi <> '.' | movsb | End_While | Mov D$edi 0

            .If D$ApiCheckFlag <> 2
                If B$WasSysModule = &FALSE
                    Call VerifyModuleWithExtensionExist
                    Call VerifyFunctionExist
                End_If
            .End_If

            movsb | While B$esi <> 0 | movsb | End_While | movsb

            Call AraseSameFunctions

        ..End_If

        While B$esi = 0 | inc esi | End_While
    ..End_While
ret


[WasSysModule: WasHALdll: ?]

IsItHAL:
    Mov eax D$StartOfDllName, eax D$eax | or eax 0202020
    If eax = 'hal.'
        Mov B$WasSysModule &TRUE
    Else_If eax = 'hal'
        Mov B$WasSysModule &TRUE
    End_If
ret


FullfillWithoutExtensions: ; 'FullfillWithExtensions'
L0: ..While B$esi <> ALEOD
        Mov ebx esi, edx 0, ecx 0
        While B$esi <> 0
            inc esi
            If B$esi = '.'
                lea edx D$esi+1 | jmp L1>
            End_If
        End_While

      ; Here, edx > Start of Function Name,
      ;       ebx > Start of Module Name
L1:     ..If edx <> 0
          ; Cases of 'MODULE.Function'
            Mov D$StartOfDllName edi, D$StartOfFunctionName edx
            Mov esi ebx | While B$esi <> '.' | movsb | End_While | Mov B$edi 0

            Mov B$WasSysModule &FALSE | Call IsItHAL

            .If B$WasHALdll = &FALSE
                If D$ApiCheckFlag <> 2
                    Call VerifyModuleExist | stosd
                    Call VerifyFunctionExist
                Else
                    Mov D$edi '.dll' | add edi 4
                End_If
            .Else
                Mov D$edi '.dll' | add edi 4
            .End_If

            While B$esi <> 0 | movsb | End_While | Mov D$edi 0 | inc edi

            Call AraseSameFunctions

        ..End_If

        While B$esi = 0 | inc esi | End_While
    ..End_While
ret


; A 'MODULE.ext.Function' or a 'MODULE.Function' has been found, and recorded.
; We arase all other occurences of 'xxxxFunction':
[CopyOfFunctionName TrashString] ;: ? #32]

AraseSameFunctions:
  ; Now, arase all other evocations of 'MODULE.ext.Function', 'MODULE.Function',
  ; 'Function', in 'ApiListA':
    push esi, edi, edx

        Mov esi D$StartOfFunctionName, edi CopyOfFunctionName
        While B$esi <> 0 | movsb | End_While | Mov D$edi 0

        Mov esi D$ApiListA, edi CopyOfFunctionName, bl B$edi
      ; edx used to test if the first name in ApiListA fits with the searched one:
        Mov edx esi | inc edx

L1:     .While B$esi <> ALEOD
            lodsb

            ...If al = bl
                If esi = edx
                    ; Compare (Cases of very first api Call in 'ApiListA' given without DLL)
                Else_If B$esi-2 = 0
                    ; Compare
                Else_If B$esi-2 = '.'
                    ; Compare
                Else
                    jmp L1<
                End_If
                push ebx, esi
                    dec esi | Mov edi CopyOfFunctionName
L2:                 Mov al B$esi, bl B$edi
                    On al <> bl, jmp L7>>
                    On al = 0, jmp L5>
                    On bl = 0, jmp L5>
                    inc esi | inc edi | jmp L2<
L5:                 or al bl
                    ..If al = 0               ; Same > arase:
                        Mov edx esi
                        pop esi | push esi
                        Mov ebx 0
                        While B$esi <> 0
                            dec esi | On B$esi = '.', inc ebx
                            cmp esi D$ApiListA | je L2>
                        End_While
                        inc esi
L2:                     .If ebx > 0
                            Mov ebx D$StartOfDllName
                            Mov ebx D$ebx
                            On ebx = D$esi, jmp L2>
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ; New: for Disassemblies:
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                If B$CopyOfFunctionName = '0'
                                    While B$esi <> 0 | inc esi | End_While | jmp L7>
                                End_If
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                Mov B$ErrorLevel 6 | error D$DoubleFunctionPtr
                        .End_If
L2:                     While B$esi <> 0 | Mov B$esi 0 | inc esi | End_While
                    ..End_If
L7:             pop esi, ebx
            ...End_If

        .End_While
    pop edx, edi, esi
ret


IsItNewDll:
    push edi
      Mov edi D$DllList | Mov ebx 0 | jmp L1>
L0:   Mov al 0,  ecx 0FF
        repne scasb
      cmp B$edi ALEOD | jne L1>
        Mov ebx 0 | jmp L9>
L1:     push esi edi
          Mov ecx 0FF,  eax 0 | inc ebx
            repe cmpsb
          Mov al B$edi-1,  ah B$esi-1
        pop edi esi
        cmp eax 02E00 | jne L0<         ; ('.' in ApiListA, 0 in DllList)
L9: pop edi
ret                                     ; >>> ebx = Dll #n or 0


IsItNewFunction:       ; edi > ApiListB end; esi > ApiListA function name; al > dll #n
    push edi
      Mov bl al | Mov edi D$ApiListB | jmp L1>
L0:   Mov al 0,  ecx 0FF
        repne scasb
      cmp B$edi ALEOD | jne L1>             ; reach end ?
        Mov al bl | jmp L9>
L1:   cmp B$edi bl | jne L0<
        inc edi
        push esi edi
          Mov eax 0
L2:       Mov al B$edi,  ah B$esi
          inc edi | inc esi
          cmp eax 0 | je L3>
          cmp ah al | je L2<
L3:     pop edi esi
        cmp eax 0 | jne L0<
L9: pop edi
    cmp al 0
ret                                         ; >>> ebx = Dll #n or 0


StoreDllName:
    cmp D$DllNumber 0 | je L1>
    Call IsItNewDll | cmp ebx 0 | je L1>    ; if 0  >>>  new name to store

L0: lodsb | cmp al '.' | jne L0<
    inc esi
L3: lodsb | cmp al '.' | jne L3<
L4: Mov eax ebx                             ; ebx from 'IsItNewDll'
ret

L1: push edi
        Mov edi D$DllListPtr
      ; write a new "Dll" name (with Extension):
L2:     lodsb | stosb | cmp al '.' | jne L2<
        While B$esi <> '.' | movsb | End_While
        inc esi
        Mov al 0 | stosb | Mov B$edi ALEOD                     ; end mark
        Mov D$DllListPtr edi | inc D$DllNumber | Mov eax D$DllNumber
    pop edi
ret


SortApis:
    Mov D$DllNumber 0,  D$FunctionNumber 0
    Mov esi D$ApiListA,  edi D$ApiListB
    Mov eax 0,  ebx 0
L0: cmp B$esi ALEOD | je L9>>
      Call StoreDllName             ; return: ebx = 0 if new, else Dll #
      Call IsItNewFunction          ; return: ebx = 0 if new, else Dll #
        ja L2>
L1:  lodsb
       cmp al 0 | jne L1<
         jmp L0<
L2:  stosb                          ; write dll #n
L3:  lodsb
     stosb                          ; write function name until 0
       cmp al 0 | jne L3<
         Mov B$edi ALEOD | inc D$FunctionNumber | jmp L0<
L9: ret

;;
 "FreeLibrary" function does NOT really strip off the DLL from memory, if it is of no
 more use. This appends only when we exit our Process. So, After each DDL testing, we
 let it in memory. At each test, we first check if it is already loaded, instead.
;;
[DllHandle: ?    StartOfDllName: ?    StartOfFunctionName: ?    RosAsmDLL: ?]

[ModuleHandlesList: ? #100] [ModuleHandlesListPointer: ?]

VerifyModuleExist:
    pushad
        Mov esi D$StartOfDllName

        Call TryToLoadModule '.dll'
        If eax <> &NULL
            Mov D$DllHandle eax | popad | Mov eax '.dll' | ret
        End_If

        Call TryToLoadModule '.sys'
        If eax <> &NULL
            Mov D$DllHandle eax | popad | Mov eax '.sys' | ret
        End_If

        Call TryToLoadModule '.drv'
        If eax <> &NULL
            Mov D$DllHandle eax | popad | Mov eax '.drv' | ret
        End_If

        Call TryToLoadModule '.exe'
        If eax <> &NULL
            Mov D$DllHandle eax | popad | Mov eax '.exe' | ret
        End_If

    Mov esi D$StartOfDllName, B$ErrorLevel 4 | error D$BadLibNamePtr


VerifyModuleWithExtensionExist:
    pushad
        Mov B$RosAsmDLL &TRUE | Call 'KERNEL32.GetModuleHandleA' D$StartOfDllName

        .If eax = &NULL
            Mov B$RosAsmDLL &FALSE | Call 'KERNEL32.LoadLibraryExA' D$StartOfDllName, 0, D$APICheckFlag
; &DONT_RESOLVE_DLL_REFERENCES &LOAD_LIBRARY_AS_DATAFILE &LOAD_WITH_ALTERED_SEARCH_PATH
            If eax = &NULL
                Call TryModuleFromAppDirectory D$StartOfDllName
            End_If

            Mov ebx D$ModuleHandlesListPointer, D$ebx eax
            add D$ModuleHandlesListPointer 4
        .End_If

        If eax = &NULL
            Mov esi D$StartOfDllName, B$ErrorLevel 4 | error D$BadLibNamePtr
        End_If

        Mov D$DllHandle eax
    popad
ret


[ExeSysDrvModuleName: ? #20]

Proc TryToLoadModule:
    Argument @Ext

        Mov esi D$StartOfDllName, edi ExeSysDrvModuleName
        While B$esi <> 0 | movsb | End_While
        Mov B$edi 0
        move D$edi D@Ext | Mov B$edi+4 0

        Mov B$RosAsmDLL &TRUE | Call 'KERNEL32.GetModuleHandleA' ExeSysDrvModuleName

        .If eax = &NULL
            Mov B$RosAsmDLL &FALSE | Call 'KERNEL32.LoadLibraryExA' ExeSysDrvModuleName, 0, D$APICheckFlag

            If eax = &NULL
                Call TryModuleFromAppDirectory ExeSysDrvModuleName
            End_If

            If eax <> &NULL
                Mov ebx D$ModuleHandlesListPointer, D$ebx eax
                add D$ModuleHandlesListPointer 4
            End_If
        .End_If
EndP


;;
  Some users' reports seem to imply that, under some OS Versions, the Directory
  that is considered the default one, for the LoadLibrary Function, could be the
  one where RosAsm lies, instead of the one where the Compiled App lies.
  
  This routines tries to force the Full Path as a last rescue.
;;
[ModuleFullPath: B$ ? #&MAXPATH]

Proc TryModuleFromAppDirectory:
    Argument @Module

        Mov edi ModuleFullPath, esi MainName
        While B$esi <> 0 | movsb | End_While
        While B$edi-1 <> '\' | dec edi | End_While

        Mov esi D@Module
        While B$esi <> 0 | movsb | End_While | movsb

        Call 'KERNEL32.LoadLibraryA' ModuleFullPath
;;
        If eax = 0
            ; Call 'KERNEL32.GetLastError'
            ; ---> &ERROR_NOACCESS 
            
            Call 'KERNEL32.CreateFileA' ModuleFullPath, &GENERIC_READ,
                                    &FILE_SHARE_READ+&FILE_SHARE_WRITE, 0, &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_NORMAL, 0
            
        End_If
;;
EndP


[Aerror: ?]

VerifyFunctionExist:
    pushad
        Mov eax D$StartOfFunctionName

        If B$eax = '0'
            Mov esi eax | Call TranslateHexa
            Call 'KERNEL32.GetProcAddress' D$DllHandle, eax
            On eax = &NULL, error D$BadOrdinalPtr
        Else
            Call 'KERNEL32.GetProcAddress' D$DllHandle, D$StartOfFunctionName
        End_If

        .If eax = &NULL
            Mov edi D$StartOfFunctionName, al 0, ecx 0FFFF | repne scasb
            sub edi 2
            push D$edi, edi
                If B$edi = 'A'
                    Mov B$Aerror &FALSE
                Else
                    inc edi | Mov al 'A' | stosb | Mov B$Aerror &true
                End_If
                Mov al 0 | stosb

              ; test with ending 'A'
                Call 'KERNEL32.GetProcAddress' D$DllHandle, D$StartOfFunctionName

                Mov esi D$StartOfFunctionName, B$ErrorLevel 5  ; error5
            pop edi, D$edi
            inc edi
            push eax
                Mov al "'" | stosb | Mov al 0 | stosb          ; retore for string search
            pop eax
            If eax = &NULL
                error D$BadFunctionNamePtr
            Else_If B$Aerror = &TRUE
                error D$MissingApiAPtr
            Else
                error D$NoAapiPtr
            End_If
        .End_If
    popad
ret

; Room for headers has been set by 'InitIndex' (CodeListPtr=CodeList+0400)

; We read a Dll Name in DllList (> first = #1)
; We read all #1Functions in ApiListB, and so on.

CreateImportSection:
 ; room for header = (dWord * 5) * (DllNumber + 1)
 ; room for Tables 1, 2 = FunctionNumber + group zero ending = FunctionNumber + DllNumber

    Mov D$DllHandle 0
    Mov edi D$CodeListPtr                   ; set at 0400 by 'List.a' initialisations
    Mov D$ImportHeaderPtr edi

    Mov eax D$Dllnumber | inc eax
    shl eax 2 | Mov ebx eax | shl ebx 2 | add eax ebx    ; (eax*4)+(eax*16) = eax * 20
    Mov ecx eax | Mov D$AppImportSize eax   ; write import header size in PE sections header
    push eax
      Mov al 0 | rep stosb        ; room for header (eax = header size)
    pop eax
    Mov D$ImportTablePtr edi      ; Pointer for Functions names adresses tables writing
    Mov eax D$FunctionNumber | add eax D$Dllnumber
    shl eax 2                     ; eax = room for Dwords (shl 2) ptrs
    Mov edx eax                   ; EDX = one table size
    add edi eax                   ; room for table 1
    Mov ebx edi
      sub ebx D$CodeList
      add ebx 01000                       ; 'uBaseOfImport' not yet filled
        sub ebx 0400                      ; 'uStartOfImport' not yet filled
          Mov D$AppSecondImport ebx       ; write adress in PE sections header
          Mov D$AppSecondImportSize edx   ; write size in PE sections header
    add edi eax                           ; room for table 2

    Mov esi D$DllList | Mov eax D$ApiListB,  D$ApiListBPtr eax
    Mov cl 1                              ; cl = DLL indice for each function

nextdll:
    On B$esi = ALEOD,  jmp L9>>     ; end mark in DllList
    Mov eax edi | sub eax D$CodeList | add eax 01000-0400
    Mov ebx D$ImportHeaderPtr
    Mov D$ebx+12 eax                ; List adress of dll name written at header fourth dWord
    Mov eax D$ImportTablePtr | sub eax D$CodeList | add eax 01000-0400
    Mov D$ebx eax | add eax edx | Mov D$ebx+16 eax
    Mov D$StartOfDllName edi
L0: lodsb                           ; from DllList
      cmp al 0 | je L1>
        stosb                       ; write dll name in Import name list
      jmp L0<
L1: Mov B$edi 0                     ; end mark in Import table (will be overwritten)

    Mov D$DllListPtr esi
    Mov al 0 | stosb
T0: Mov esi D$ApiListBPtr

L2: lodsb
     cmp al cl | je L4>             ; actual DLL function?
     cmp al, ALEOD | jne L3>        ; reach end of ApiListB ?
       Mov W$edi 0 | add edi 2
       test edi 1 | jz T0>
         Mov B$edi 0 | inc edi
T0:    Mov ebx D$ImportTablePtr
       Mov D$ebx 0                 ; write function name end of chunk in table 1
       Mov D$ebx+edx 0             ; write function name end of chunk in table 2
       add D$ImportTablePtr 4      ; ready for next one
       inc cl
       Mov  esi D$DllListPtr
       add D$ImportHeaderPtr 20

       jmp nextdll
L3: lodsb
      cmp al 0 | jne L3<
        jmp L2<

L4: Mov ebx D$ImportTablePtr
    test edi 1 | jz T0>
      Mov B$edi 0 | inc edi
T0: If B$esi = '0'
        pushad
            push ebx, edx
                Call TranslateHexa | or eax 08000_0000
            pop edx, ebx
            Mov D$ebx eax                  ; write function ordinal in table 1
            Mov D$ebx+edx eax              ; write function ordinal in table 2
            add D$ImportTablePtr 4         ; ready for next one
        popad
        Mov D$StartOfFunctionName esi
L5:     lodsb | cmp al 0 | jne L5<
    Else
        Mov eax edi | sub eax D$CodeList | add eax 01000-0400
        Mov D$ebx eax                  ; write function name adress in table 1
        Mov D$ebx+edx eax              ; write function name adress in table 2
        add D$ImportTablePtr 4         ; ready for next one
        Mov ax 0
        stosw
        Mov D$StartOfFunctionName edi

L5:     lodsb | stosb | cmp al 0 | jne L5<   ; writing Functions names list
    End_If
    jmp L2<<                    ; two '0' ??? needed ???... alignement needed ???
                                ; usual import tables names lists seem to be aligned.
L9: ret


BuildImport:
    Call InitApiTables

    Call SearchForApis      ; copy all api calls in ApiListA (> edi > end of ApiListA).

    If edi = D$ApiListA     ; case of DLL with no api call
        Mov D$uBaseOfRsrc 01000, D$uImportSize 0 | ret
    End_If

    Call FullfillApiList   ; FullFill each Function in the 'MODULE.ext.Function' Form.

    Call SortApis          ; >>> Dll in DllList, Functions in ApiListB + Dll #n at first

    Call CreateImportSection

    Mov eax edi | Align_on 0200 eax | Mov ecx eax | sub ecx edi

    push edi, eax
      Mov al 0 | rep stosb  ; fill with 0 because destination have been "reused".
    pop eax edi

    Mov D$CodeListPtr eax

    sub eax D$CodeList | sub eax 0400 | add eax 01000 | Align_on 01000 eax
    Mov D$uBaseOfRsrc eax
    Mov eax edi | sub eax 0400 | sub eax D$CodeList | Mov D$uImportSize eax

  ; Release all the loaded Modules not belonging to RosAsm Process:
    Mov esi ModuleHandlesList
    While D$esi <> 0
        lodsd
        push esi
            Call 'KERNEL32.FreeLibrary' eax
        pop esi
    End_While
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

ExportSectionComments:
;;
; Export Section looks like this:
;
; [.Export: D$ 0, 0, 0, DLLNamePtr, Ord_BASE, NumberOfFunctions, NumberOfNames,
;              ExportAdressesTable, ExportNamesTable, ExportOrdinals
;
;  DLLName: 'MYDLL.DLL', 0  0 0 0 0 0 0 0 0 .....
;
;  ExportAdressesTable: Function1, Function2, Function3 ....
;
;  ExportNamesTable: Function1Name, Function2Name, Function3Name  ....
;
;  ExportOrdinals: W$ (Ord_BASE - Name_Ordinal), ....
;
;
;      Function1Name: 'Function1', 0
;      Function2Name: 'Function2', 0
;      Function3Name: 'Function3', 0 ....]
;;

[EXPnName 12
 EXPnBase 16
 EXPNumberOfFunctions 20
 EXPNumberOfNames 24
 EXPAddressOfFunctions 28
 EXPAddressOfNames 32
 EXPAddressOfNameOrdinals 36]

[ExportAdressesTablePtr: ?    ExportNamesTablePtr: ?    ExportOrdinals: ?
 FunctionNamesPtr: ?          ExportSectionLen: ?       FileAlignedExportSectionLen: ?]

FillExportSection:
    Mov edi D$ExportListBPtr
    add edi 40                           ; (nf / nf already written by 'NewBuildExport').

  ; edi points now to 'ExportAdressesTable'. We store the edi in the Header record
  ; 'FunctionsAdressesTable':
    Mov eax edi | add eax D$ExportAjust
    Mov esi D$ExportListBPtr | Mov D$esi+EXPAddressOfFunctions eax, D$ExportAdressesTablePtr eax
    move D$ExpOrdBase D$esi+EXPnBase
  ; We will have as many dWords for Adresses as Exported Functions. So, next header record,
  ; 'ExportNamesTable' will be:
    Mov eax D$esi+EXPNumberOfFunctions | shl eax 2 | add eax edi | add eax D$ExportAjust
    Mov D$esi+EXPAddressOfNames eax, D$ExportNamesTablePtr eax
    sub eax D$ExportAjust | Mov edi eax

  ; We will have as many dWords for Names Pointers as Exported Names.
    Mov eax D$esi+EXPNumberOfNames | shl eax 2 | add eax edi | add eax D$ExportAjust
    Mov D$esi+EXPAddressOfNameOrdinals eax, D$ExportOrdinals eax
    sub eax D$ExportAjust | Mov edi eax

  ; We will have as many Words for Ordinals as Exported Names.
    Mov eax D$esi+EXPNumberOfNames | shl eax 1 | add eax edi | add eax D$ExportAjust
    Mov D$FunctionNamesPtr eax
;int3
    Mov esi D$ExportListAPtr
    and D$Ordinal 0 | Mov D$ErrorLevel 9

L1: Call StoreExportAdresse | Call StoreExportNamePtr
    Call StoreExportOrdinal | Call StoreFunctionName
    dec D$NumberOfExportedFunctions | jnz L1<

; sort Names & adjaust Ordinals to Base
    Mov ebx D$ExportListBPtr | Mov ecx D$ebx+EXPNumberOfNames | Mov eax D$ebx+EXPnBase
    Mov edi D$ebx+EXPAddressOfNameOrdinals | sub edi D$ExportAjust
    Call DualBubbleSortExportListPointersAndOrdinals
L0: sub W$edi ax | add edi 2 | loop L0<

    Mov ebx D$ExportListBPtr | add ebx EXPnName
    Mov edi D$FunctionNamesPtr
    Mov D$ebx edi                       ; write DLName pointer.
    sub edi D$ExportAjust
    Mov esi ChoosenFile

L0: lodsb | stosb | cmp B$esi '.' | ja L0<

    Mov eax D$SavingExtension | or eax 020202000 | xor eax 020202000 | stosd
    Mov al 0 | stosb
    sub edi D$ExportListBPtr
    Mov D$ExportSectionLen edi
ret


; This is same as 'SearchRegularLabel' with some specifics (no error control -impossible-)
; + end control

[DataExportedLabel: ? OnlyOrdinalExport: ? OrdinalExport: ? ExpOrdBase: ?]

StoreExportAdresse:
    Mov B$DataExportedLabel &FALSE | Mov B$OnlyOrdinalExport &FALSE
    Mov eax D$esi | or eax 0202020 | cmp eax 'ord0' | jne L0>
    lea edi D$esi+4 | Mov al EOI | Mov ecx 6 | repne scasb
    cmp B$edi TextSign | setne B$OnlyOrdinalExport
    lea edi D$esi+4 | sub edx edx
L1:
    Mov al B$edi | inc edi | cmp al '0' | jb L4>
    or al 020 | cmp al '9' | jbe L3> | sub al 027
L3: sub al 030 | shl edx 4 | or dl al | jmp L1<
L4: Mov D$Ordinal edx
L0:
    Mov edi D$LabelList | Mov edx D$edi | add edx edi | add edi 5

L0: push esi
    cmp edx edi | jbe FindLostExportString
L1:     lodsb | Mov bl B$edi | inc edi
L2:     If al = '_'
            lodsb | jmp L2<
        Else_If al = EOI
L4:         cmp bl EOI | je L8>
        Else_If al = colonSign ; instead of ':' jE!
            Mov B$DataExportedLabel &TRUE | jmp L4< ; jE!
        Else_If al >= 'a'
            and al 00_11011111 | cmp al bl | je L1<
        Else
            cmp al bl | je L1<
        End_If

        cmp bl EOI | jbe L3>            ; case of LabelList name shorter than searched Label
        Mov ecx 0FF,  al EOI | repne scasb      ; longer: LabelList edi ptr > after next '|'
L3:     add edi 6                                   ; |LABELNAME|dWord FlagByte|NEXTNAME

L7: pop esi | jmp L0<

L8: pop esi
    Mov eax D$edi

    sub eax D$CodeList

    If B$DataExportedLabel = &FALSE
        add eax D$AppBaseOfCode
        sub eax D$AppStartOfCode
    Else
        add eax D$AppBaseOfData
        sub eax D$AppStartOfData
    End_If

    If D$Ordinal <> 0
        Mov edi D$ExportListBPtr | add edi 40 | Mov edx D$Ordinal | Mov ecx edx
        sub edx D$ExpOrdBase ;| cmp D$edi+edx*4 ecx |
        Mov D$edi+edx*4 eax
    Else
        Mov edi D$ExportAdressesTablePtr | sub edi D$ExportAjust
        While D$edi <> 0 | add edi 4 | End_While
        stosd
        add edi D$ExportAjust | Mov D$ExportAdressesTablePtr edi
    End_If
ret


StoreExportNamePtr:
    ON B$OnlyOrdinalExport = &TRUE, ret
    Mov edi D$ExportNamesTablePtr, eax D$FunctionNamesPtr
    sub edi D$ExportAjust
    stosd | add D$ExportNamesTablePtr 4
ret


[Ordinal: ?]

StoreExportOrdinal:
    If B$OnlyOrdinalExport = &TRUE
       and D$Ordinal 0 | ret
    End_If

    If D$Ordinal <> 0
        Mov edi D$ExportOrdinals, eax D$Ordinal | sub edi D$ExportAjust | stosw
        add D$ExportOrdinals 2 | and D$Ordinal 0
    Else
        Mov edi D$ExportOrdinals | sub edi D$ExportAjust ; calc ODR from given pos
        Mov eax D$ExportAdressesTablePtr
        sub eax D$ExportAjust | sub eax D$ExportListBPtr | sub eax 40 | shr eax 2
        dec eax | add eax D$ExpOrdBase | stosw | add D$ExportOrdinals 2
    End_If
ret


StoreFunctionName:
    If B$OnlyOrdinalExport = &TRUE
       Mov edi esi | Mov al EOI | Mov ecx 0FF | repne scasb | Mov esi edi
       Mov edi D$ExportListBPtr | ret
    End_If

    Mov edi esi | Mov al EOI | Mov ecx 0FF | repne scasb | cmp B$edi TextSign | je L3>
    Mov edi D$FunctionNamesPtr | sub edi D$ExportAjust
    While B$esi <> EOI
        If B$esi = colonSign ; instead of ':' jE!
            inc esi | jmp L2>
        End_If
        movsb
    End_While
L2: inc esi | jmp L2>
; Copy if FullName
L3: Mov esi edi | Mov edi D$FunctionNamesPtr | sub edi D$ExportAjust | inc esi
    While B$esi <> TextSign | movsb | End_While | inc esi
L2: Mov al 0 | stosb | add edi D$ExportAjust | Mov D$FunctionNamesPtr edi
ret

[ExportsectionWanted: ?  NumberOfExportedFunctions: ?  ExportALL: ?
 ExportListAPtr: ?  ExportListBPtr: ?  ExportListPointers: ?  NumberOfJunkBytes: ? ExpOrdArray: ?]

DualBubbleSortExportListPointersAndOrdinals:
    pushad
    Mov ebx D$ebx+EXPAddressOfNames | sub ebx D$ExportAjust
    push ecx | sub eax eax

L0: Mov esi D$esp | sub ebp ebp | sub ebx 4 | sub edi 2
L1: dec esi | je L5>
    add ebx 4 | add edi 2
    Mov ecx D$ebx | Mov edx D$ebx+4 | sub ecx D$ExportAjust | sub edx D$ExportAjust
L3: Mov al B$ecx | Mov ah B$edx | inc ecx | inc edx | test eax eax | je L4>
    cmp ah al | je L3< | ja L1<
    Exchange D$ebx D$ebx+4 | Exchange W$edi W$edi+2 | or ebp 1 | jmp L1<
L4: Mov eax D$ebx | sub eax D$ExportAjust | pop ecx | Mov D$esp+01C eax | popad
    jmp FindExportFullNameString
L5: test ebp ebp | je L7>

    Mov esi D$esp | sub ebp ebp | add ebx 4 | add edi 2
L1: dec esi | je L5>
    sub ebx 4 | sub edi 2
    Mov ecx D$ebx | Mov edx D$ebx+4 | sub ecx D$ExportAjust | sub edx D$ExportAjust
L3: Mov al B$ecx | Mov ah B$edx | inc ecx | inc edx | test eax eax | je L4<
    cmp ah al | je L3< | ja L1<
    Exchange D$ebx D$ebx+4 | Exchange W$edi W$edi+2 | or ebp 1 | jmp L1<
L5: test ebp ebp | jne L0<<
L7: pop ecx | popad
ret

FindExportFullNameString:
    Mov esi eax | Mov edi eax | sub eax eax | or ecx 0-1 | repne scasb | not ecx | dec ecx
    Mov ebx ecx | Mov edi D$CodeSource | Mov edx D$SourceEnd
    While edi < edx
        cmp W$edi '::' | jne L8>
        Mov al B$edi+2 | cmp al "'" | je L0> | cmp al '"' | jne L8>
L0:     add edi 2 | pushad | inc edi | Mov ecx ebx | repe cmpsb | popad | jne L8>
        cmp B$edi+ebx+1 al | je E0>
L8:
        inc edi
    End_While
    Mov eax D$SymbolDupPtr | jmp OutOnError

E0: Mov D$ErrorLevel 0 |mov B$esi-1 EOI | Mov B$esi+ebx EOI
    Mov ecx D$StatementsPtr | Mov D$ecx edi
    Error D$SymbolDupPtr, esi

FindLostExportString:
    pop esi
    Mov edi esi | Mov al EOI | or ecx 0-1 | repne scasb
    If B$edi-2 = 0E
        dec edi | Mov B$edi-1 EOI
    End_If
    Mov ecx edi | sub ecx esi | Mov ebx ecx
L0: Mov al B$edi-1 | Mov B$edi al | dec edi | loop L0<
    Mov B$edi EOI | dec ebx | inc esi | Mov edi D$CodeSource | Mov edx D$SourceEnd
    While edi < edx
        Mov al B$edi | cmp al '"' | je L3> | cmp al "'" | je L3> | cmp al ';' | jne L0>
        If D$edi-1 = MLC
            add edi 2
            Do
            inc edi
            Loop_Until D$edi = MLC
            add edi 3
        Else
            Do
            inc edi
            Loop_Until B$edi < ' '
        End_If
        jmp L8>
L3:     inc edi | or ecx 0-1 | repne scasb | dec edi | jmp L8>
L0:     cmp W$edi '::' | jne L8> | Mov ecx edi
L0:     cmp B$edi-1 ' ' | jbe L1>
        cmp B$edi-1 '[' | je L1>
        cmp B$edi-1 ']' | je L1>
        cmp W$edi-2 '[<' | je L1>
        cmp B$edi-1 '|' | je L1>
        dec edi | jmp L0<
L1:     Mov eax ecx | sub eax edi | cmp eax ebx | jne L2>
L0:     pushad | Mov ecx ebx | repe cmpsb | popad | je E0>
L2:     Mov edi ecx
L8:     inc edi
    End_While
E1: Mov eax D$UnknownSymbolPtr | jmp OutOnError

E0: Mov D$ErrorLevel 0 | Mov ecx D$StatementsPtr | Mov D$ecx edi
    Error D$UnknownSymbolPtr, esi

 __________________________________________________________________________________________
 __________________________________________________________________________________________
;;
 these 2 following stubs are used to create new PE. only 'Labelled' values are
 modified according with source values. Main filling work is done by the 'Build...'
 routines.

 These two stubs must remain all in one single data set (prevent from RosAms data
 alignement).

00000000: Dos exe file header stub:
;;

[DosHeader:
B$ 'MZ'  ; dos exe signature
D$ 030090; Size of file (I don't understand what it means...)
W$ 00    ; Number of reloc. adresses
W$ 04    ; this dos header size (16*4)
W$ 00    ; min size
W$ 0FFFF ; max size
W$ 00    ; SP reg. value at run time

W$ 0B8  ; checksum for header
W$ 00   ; IP reg. value
W$ 00   ; start of Cseg in file
W$ 00   ; start of reloc. table in file
W$ 040  ; overlay default
W$ 0,0,0

W$ 0,0,0,0, 0,0,0,0     ; reserved words

W$ 0,0,0,0
MyCheckSum: D$ 0  ; 30
PeHeaderPointer:
D$ 080   ; File adress of win header

B$   0E, 01F, 0BA, 0E, 00, 0B4, 09, 0CD, 021, 0B8, 01, 04C, 0CD, 021
; push cs // pop ds // Mov dx 0E // Mov ah 09 // int 021 // Mov ax 4C01 // int 021
; 18
B$ ;'Spindoz 32 spit PEfile made wiz RosAsm Assembler.$'
   'This program cannot be run in DOS mode', CR, LF, '$', 0, 0, 0, 0, 0, 0, 0, 0, 0

; 50+18+30 = 98
; if you modify upper string you must absolutely keep the same lenght.

PeHeader:

B$ 'PE',0,0             ; signature
W$ 014C                 ; 386 and more
NumberOfSections:
W$ 04                   ; 4 sections (code, data, import, resource) not 5...
B$ 0,0,0,0              ; time and date stamp
D$ 0                    ; pointer to symbol table (for debug)
D$ 0                    ; number of symbol
W$ 0E0                  ; size of 'optional header'
PeHeaderCharacteristics:
W$ 00100001111          ; characteristics
   ; bit 0 > 1 > reloc. infos not there
  ; bit 1 > 1 > Runable
 ; bit 2 > 1 > no line number for debug
; bit 3 > 1 > no bebug symbol
; others : unknown
B$ 0B,01                ; referred as 'magic'...
W$ 03                   ; linker version
AppCodeSize: D$ 0       ; size of code (.text section)
AppAllDataSize: D$ 0    ; size of initialized data (.data + .rsrc+... + .reloc)
D$ 0                    ; size of uninitialised data
AppRVAentryPoint: D$ 0  ; RVA entry point adress (414h in RDNrect file)
AppBaseOfCode: D$ 0     ; RVA Base of code (0400 in file)
SHAppBaseOfData: D$ 0   ; RVA Base of data ('SH' because one more 'AppBaseOfData' down there)
ImageBase:
D$ 0400000              ; image base (linker base default)
D$ PageSize             ; sections alignement
D$ 0200                 ; file alignement
W$ 04,00                ; OS version
W$ 01,00                ; image version
W$ 04,00                ; sub system version
B$ 00,00,00,00          ; reserved
AppRVAimageSize: D$ 0   ; RVA image size
D$ 0400                 ; headers size
CheckSum:
D$ 0                    ; checksum (works when zero)
SubSystem:
W$ 02                   ; sub system
DllCharacteristics:     ; 0001h - Per-Process Library Initialization
                        ; 0002h - Per-Process Library Termination
                        ; 0004h - Per-Thread Library Initialization
                        ; 0008h - Per-Thread Library Termination
W$ 0                   ; DLL characteristics
AppStackMax: D$ 0100000     ; stack max
AppStackMin: D$ 01000       ; stack min
AppHeapMax: D$ 0100000      ; heap max
AppHeapMin: D$ 0            ; heap min
D$ 0                   ; loader flags
D$ 0_10                 ; number of possible entries in following section table (16 records)

; Section table (called image data directory):
; first Dwords are the RVA adresses; second ones are the sizes
SectionTable:
D$                 00,                        00   ; export
; In fact, not 'Base_of' but rather a pointer to the Import Directory.
; Some Linkers do not write the Import Directory at first place of .Import,
; but at second place (why do it simple when you can do i complicated???...).
AppBaseOfImport: D$ 0  AppImportSize:       D$ 0   ; import header (Directory only)
AppBaseOfRsrc:   D$ 0    AppRsrcSize:       D$ 0   ; resource
D$                 00,                        00   ; exeption
D$                 00,                        00   ; security
RelocSectionTable:
D$                 00,                        00   ; Relocation
DebugDir:
D$                 00,                        00   ; debug
D$                 00,                        00   ; copyright
D$                 00,                        00   ; machine values (mips gp and global ptr)
D$                 00,                        00   ; thread local storage
D$                 00,                        00   ; load configuration directory
D$                 00,                        00   ;
AppSecondImport: D$ 0  AppSecondImportSize: D$ 0   ; second import (Address Table)
D$                 00,                        00
D$                 00,                        00
D$                 00,                        00

SectionsHeaders:

idataSectionHeader:         ; (import section)

B$   '.idata',0,0
AppImportTrueSize: D$ 0     ; EndOfImport - StartOfImport  true size (Virtual Size)
AppBaseOfImports: D$ 0      ; RVA
AppImportAlignedSize: D$ 0  ; 200h+ImportExt (Physical File Size)
AppStartOfImport: D$ 0      ; idata ptr
D$ 0,0,0
D$ 0_C0000040               ; readable, writable, initialised data


ResourceSectionHeader:

B$ '.rsrc',0,0,0
AppRsrcTrueSize: D$ 0       ; EndOfResource-StartOfResource  true size
AppBaseOfRsrcs: D$ 0        ; RVA
AppRsrcAlignedSize: D$ 0    ; 200h+ResourceExt
AppStartOfRsrc: D$  0
D$ 0,0,0
D$ 0_40000040               ; readable initialised data


DataSectionHeader:

B$ '.data',0,0,0
AppDataTrueSize: D$ 0       ; EndOfData-StartOfData  true size
AppBaseOfData: D$ 0         ; RVA
AppDataAlignedSize: D$ 0    ; 200h+DataExt    aligned size
AppStartOfData: D$ 0        ; data ptr
D$ 0,0,0
DataCharacteristics:
D$ 0_C0000040               ; readable, writable, initialised data


; Code section header: (the four 'dummy' D$ and W$ are of no mean in EXE and DLL files)

B$ '.text',0,0,0
AppTrueCodeSize: D$   0     ; true size of code in file
AppCodeRVAoffset: D$   0    ; RVA offset (aligned on 01000 boundary)
AppFileSizeOfCode: D$   0   ; file aligned size of code (0200 aligned)
AppStartOfCode: D$   00     ; pointer to code (true first code in file - not entry point-)
D$   00                     ; dummy reloc ptr
D$   00                     ; dummy line number ptr
W$   00                     ; dummy reloc number
W$   00                     ; dummy number of line number
CodeCharacteristics:
D$   0_60000020             ; characteristics (readable, runable, code)

ExportSectionHeader:        ;, if any:
D$ 0 0
AppExpTrueSize: 0
AppBaseOfExp: 0
AppExpAlignedSize: 0
AppStartOfExp: 0   0 0 0
D$ 0_40000040               ; readable initialised data

RelocSectionHeader:         ;, if Export:
D$ 0 0
AppRelocTrueSize: 0
AppBaseOfReloc: 0
AppRelocAlignedSize: 0
AppStartOfReloc: 0   0 0 0
D$ 0_40000040               ; readable initialised data

D$ 0 0   0 0 0 0   0 0 0 0  ; just ensure to stop win search of sections.

D$ 0 0   0 0 0 0   0 0 0 0

SourceSectionHeader:
B$ '.src',0,0,0,0           ; Used by RosAsm only (not by loader: 4 sections, not 5)
AppSrcTrueSize: D$ 0        ; D$SourceLen  true size
AppBaseOfSrc: D$ 0          ; RVA
AppSrcAlignedSize: D$ 0     ; 200h+ResourceExt
AppStartOfSrc: D$  0
D$ 0,0,0
D$ 06000840                 ; Not readable initialised data; don't keep; don't cache...


EOPE:
PeHeaderSize: D$  EOPE-PeHeader]  ; 'Len' unusable here
 ________________________________________________________________________________________

; User stub main data:

[uImportSize: D$ 0  uRsrcSize: D$ 0  uDataSize: D$ 0  uCodeSize: D$ 0
 uStartOfImport: D$ 0  uStartOfRsrc: D$ 0  uStartOfData: D$ 0  uStartOfCode: D$ 0
 uEndOfFile: D$ 0  uBaseOfImport: D$ 0  uBaseOfRsrc: D$ 0  uBaseOfData: D$ 0
; uBaseOfExport: D$ 0 uExportSize: D$ 0
 uBaseOfCode: D$ 0  uImageSize: D$ 0  uCodeRVA: D$ 0  uAllDataSize: D$ 0
 uStackMax: D$ 0  uStackMin: D$ 0  uHeapMax: D$ 0  uHeapMin: D$ 0]

; exemple with the fixed values i used in the very first versions:

; [CodeExt 05C00  DataExt 01000  ImportExt 0200  RsrcExt 0200]
; [StartOfImport 0400]
; [StartOfRsrc   0800]   ; 0600+ImportExt (0200)
; [StartOfData   0C00]   ; 0800+ImportExt+RsrcExt (0400)
; [StartOfCode   1E00]   ; 0A00+ImportExt+RsrcExt+DataExt (01400)
; [EndOfFile     7D00]   ; 0C00+ImportExt+RsrcExt+DataExt+CodeExt (07100)

 _________________________________________________________________________________________

; For Saving here the true sizes of each section because they are aligned for computations.
; I need them at the end to fill the so called 'Virtual' sizes records (first record of
; each type header:

[ImportTrueSize: 0    ResourcesTrueSize: 0    DataTrueSize: 0    CodeTrueSize: 0]

[LockInstruction: ?]

[TrueUserDataSize: 0]


; Was no use without the new DLLs stuff, but, when building a DLL after having an EXE PE
; compiled, some of these Data may remain with a 'wrong' value from the previous compilation.

ClearUserStubMainData:
    Mov edi uImportSize, eax 0, ecx TrueUserDataSize | add ecx 4
    sub ecx edi | shr ecx 2 | rep stosd
ret

UserMainData:
    move D$ImportTrueSize D$uImportSize,
         D$ResourcesTrueSize D$uRsrcSize,
         D$DataTrueSize D$uDataSize

    Mov eax D$uImportSize | Align_on 0200 eax

    Mov D$uImportSize eax | Mov ecx eax
    Mov eax D$uRsrcSize | Align_on 0200 eax
    Mov D$uRsrcSize eax | add ecx, eax
    move D$TrueUserDataSize D$uDataSize    ; preserve for the whole (with virtual size) Align
    Mov eax D$uDataSize | Align_on 0200 eax
    Mov D$uDataSize eax | add ecx, eax
    Mov eax D$CodelistPtr | sub eax D$CodeList | sub eax 0400 | sub eax, ecx
    move D$CodeTrueSize eax
    Mov D$AppTrueCodeSize eax
    Align_on 0200 eax | Mov D$uCodeSize eax

    Mov eax 0400 | Mov D$uStartOfImport eax
        add eax D$uImportSize | Mov D$uStartOfRsrc eax
        add eax, D$URsrcSize | Mov D$uStartOfData eax
        add eax D$uDataSize | Mov D$uStartOfCode eax
        add eax D$uCodeSize | Mov D$uEndOfFile eax

    Mov eax 01000 | Mov D$uBaseOfImport eax
        add eax D$UImportSize | Align_on 01000 eax
        Mov D$uBaseOfRsrc eax
        add eax D$uRsrcSize | Align_on 01000 eax
        Mov D$uBaseOfData eax
        add eax D$TrueUserDataSize | add eax D$uVirtualDataSize | Align_on 01000 eax
        Mov D$uBaseOfCode eax
        add eax D$uCodeSize | Align_on 01000 eax
        Mov D$uImageSize eax

    Mov eax LINKERDEFAULT | add eax D$uBaseOfCode
        sub eax D$uStartOfCode | Mov D$uCodeRVA eax

    Mov eax D$uStartOfCode | sub eax D$uStartOfImport | Mov D$uAllDataSize eax
ret
 _______________________________________________________________________________________


PreparePeHeader:
  Call UserMainData

  move D$AppCodeSize D$uCodeSize
  move D$AppAllDataSize D$uAllDataSize

  Call SearchForEntryPoint
  sub eax D$CodeList | sub eax D$uStartOfCode | add eax D$uBaseOfCode
  Mov D$AppRVAentryPoint eax

  move D$AppBaseOfCode D$uBaseOfCode
  move D$SHAppBaseOfData D$uBaseOfData
  move D$AppRVAimageSize D$uImageSize

;  hexprint D$uBaseOfCode,
;           D$uBaseOfImport,
;           D$uBaseOfCode

  move D$AppBaseOfImport D$uBaseOfImport

  move D$AppBaseOfRsrc D$uBaseOfRsrc
  move D$AppRsrcSize D$uRsrcSize       ; should be unaligned size of the section

__________________

  move D$AppCodeRVAoffset D$uBaseOfCode
  move D$AppFileSizeOfCode D$uCodeSize
  move D$AppStartOfCode D$uStartOfCode

  Mov eax D$DataTrueSize | add eax D$uVirtualDataSize

  move D$AppDataTrueSize eax
  move D$AppBaseOfData D$uBaseOfData
  move D$AppDataAlignedSize D$uDataSize
  move D$AppStartOfData D$uStartOfData

  move D$AppImportTrueSize D$ImportTrueSize
  move D$AppBaseOfImports D$uBaseOfImport
  move D$AppImportAlignedSize D$uImportSize
  move D$AppStartOfImport D$uStartOfImport

  move D$AppRsrcTrueSize D$ResourcesTrueSize
  move D$AppBaseOfRsrcs D$uBaseOfRsrc
  move D$AppRsrcAlignedSize D$uRsrcSize
  move D$AppStartOfRsrc D$uStartOfRsrc

; Store source values in .Src section:
  move D$AppSrcTrueSize D$SourceLen
  Mov eax D$uBaseOfCode | add eax D$LenOfCode | Align_on 01000 eax
  Mov D$AppBaseOfSrc eax
  Mov eax D$SourceLen | Align_on 0200 eax
  Mov D$AppSrcAlignedSize eax
  Mov eax D$uEndOfFile | Mov D$AppStartOfSrc eax

; Copy:
  Mov esi DosHeader | Mov edi D$CodeList
  Mov ecx 080 | rep movsb                                   ; store Dos header
  Mov ecx D$PeHeaderSize | Mov esi PeHeader | rep movsb     ; room for PE header
ret


[LocOfSourceHeader: ?]

WritePeHeaders:
    or D$SavingExtension 020202000 | xor D$SavingExtension 020202000

    Mov esi DosHeader, edi D$CodeList

    Mov ecx PeHeader | sub ecx esi | shr ecx 2 | rep movsd

    Mov D$edi 'PE'; W$edi+2 0                   ; signature
    add edi 4 | Mov W$edi 014C                  ; 386 and more

  ; Compute Number of Sections (NumberOfSections):
    Mov eax 1                                   ; Code Section anyway, i suppose.
    On D$uImportSize > 0, inc eax               ; If Import Section wanted.
    On D$uRsrcList > 0, inc eax                 ; If Resources Section wanted.
    On D$uDataSize > 0, inc eax                 ; If Data Section wanted.
                                                ; Code section assumed.
    On B$ExportsectionWanted = &TRUE, inc eax   ; If Export Section wanted.

    If D$SavingExtension = '.SYS'
        jmp L1>
    Else_If D$RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
L1:     On D$RelocSectionSize => 8, inc eax      ;jE! If Reloc Section wanted.
    End_If
    add edi 2 | stosw                           ; NumberOfSections

    Mov eax 0 | stosd | stosd | stosd           ; time and date stamp
                                                ; pointer to symbol table (for debug)
                                                ; number of symbol
    Mov W$edi 0E0 | add edi 2                   ; size of 'optional header'

    Mov eax &IMAGE_FILE_32BIT_MACHINE__&IMAGE_FILE_EXECUTABLE_IMAGE
    or eax &IMAGE_FILE_LINE_NUMS_STRIPPED__&IMAGE_FILE_LOCAL_SYMS_STRIPPED
    If D$SavingExtension = '.DLL'
        or eax &IMAGE_FILE_DLL
        On D$RelocSectionSize < 8, or eax &IMAGE_FILE_RELOCS_STRIPPED ;jE!
    Else_If D$SavingExtension = '.SYS'
        On D$RelocSectionSize < 8, or eax &IMAGE_FILE_RELOCS_STRIPPED ;jE!
    Else
        or eax &IMAGE_FILE_RELOCS_STRIPPED
    End_If


    Mov W$edi ax | add edi 2                    ; 'PeHeaderCharacteristics'

    Mov D$edi 03010B | add edi 4                ; B$ 0B,01  ; referred as 'magic'...
                                                ; W$ 03     ; Dummy linker version (???)
    Mov eax D$uCodeSize | stosd                 ; size of code (.text section)

    Mov eax D$AppAllDataSize | stosd            ; size of initialized data (.data + .rsrc+... + .reloc)

    Mov D$edi 0 | add edi 4                     ; Never any real Uninitialised section.

    Call SearchForEntryPoint
    sub eax D$CodeList | sub eax D$uStartOfCode
    add eax D$uBaseOfCode | stosd               ; RVA entry point (Adress of 'Main:')

    Mov eax D$uBaseOfCode | stosd               ; RVA Base of code (0400 in file)
    Mov eax D$uBaseOfData | stosd               ; RVA Base of data

    If D$SavingExtension = '.DLL'
        Mov eax D$LinkerDllDefault
    Else_If D$SavingExtension = '.SYS'
        Mov eax DRIVERDEFAULT
    Else
        Mov eax LINKERDEFAULT                   ; image base
    End_If

   ; Mov eax LINKERDEFAULT ; ??? Should it be allowed or not ???
   ; Does not seem to work... Whereas 2 indentical DLLs, with same Base work well.
    stosd

    Mov D$edi PageSize | add edi 4              ; sections alignement
    Mov D$edi 0200 | add edi 4                  ; file alignement

    Mov D$edi 04 | add edi 4                    ; W$ 04,00       ; OS version
    Mov D$edi 01 | add edi 4                    ; W$ 01,00       ; image version
    Mov D$edi 04 | add edi 4                    ; W$ 04,00       ; sub system version
    Mov eax 0 | stosd                           ; B$ 00,00,00,00 ; reserved

; 'AppRVAimageSize:'
    Mov D$FinalImageSize edi
    Mov eax 0 | stosd                           ; RVA image size
    Mov D$edi 0400 | add edi 4                  ; headers size
    Mov D$edi 0 | add edi 4                     ; checksum (works when zero)
    Mov ax W$SubSystem | stosw                  ; sub system

    If D$SavingExtension = '.DLL'
        Mov ax W$DllCharacteristics
    Else_If D$SavingExtension = '.SYS'
        Mov W$edi-2 1 ; SubSystem NATIVE jE! - corrected.
        Mov ax 0; &IMAGE_DLLCHARACTERISTICS_WDM_DRIVER. removed WDM, bcoz WDM is not unloadable
    Else
        Mov eax 0
    End_If
    stosw                                       ; DllCharacteristics
    Mov eax D$AppStackMax | stosd
    Mov eax D$AppStackMin | stosd
    Mov eax D$AppHeapMax | stosd
    Mov eax D$AppHeapMin | stosd

    Mov D$edi 0 | add edi 4                     ; loader flags
    Mov D$edi 010 | add edi 4                   ; number of possible entries in following
                                                ; section table (16 records)
    _________________________

    push edi
        Mov eax 0, ecx 020 | rep stosd  ; Clear all 16 * (2 dWords) Entires ('SectionTable').
    pop edi

 [FinalBaseOfExport: ?    FinalAppBaseOfReloc: ?    FinalImageSize: ?]

    If B$ExportsectionWanted = &TRUE
        Mov D$FinalBaseOfExport edi, eax 0 | stosd
        Mov eax D$ExportSectionLen | stosd
    Else
        add edi 8
    End_If

    If D$uImportSize > 0
        Mov eax D$uBaseOfImport | stosd
        Mov eax D$Dllnumber | inc eax
        shl eax 2 | Mov ebx eax | shl ebx 2 | add eax ebx    ; (eax*4)+(eax*16) = eax * 20
        stosd
    Else
        add edi 8
    End_If

    If D$uRsrcList > 0
        Mov eax D$uBaseOfRsrc | stosd
        Mov eax D$uRsrcSize | stosd
    Else
        add edi 8
    End_If

    add edi 16                                  ; Now pointing to 'RelocSectionTable'

    .If D$SavingExtension = '.SYS'
        jmp L1>
    .Else_If D$RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
L1:     If D$RelocSectionSize => 8 ;jE!
            Mov eax 0, D$FinalAppBaseOfReloc edi | stosd
            Mov eax D$RelocSectionSize | stosd
        Else
            add edi 8
        End_If
    .Else
        add edi 8
    .End_If

    add edi (12*4)                              ; Now pointing to 'AppSecondImport'

    Mov eax D$AppSecondImport | stosd
    Mov eax D$AppSecondImportSize | stosd

    add edi (6*4)                               ; Now pointing to 'SectionsHeaders'
    _____________

    Mov D$NextSectionHeaderRVA 01000, D$NextSectionHeaderFilePointer 0400

    If D$uImportSize > 0
        Call WriteOneSectionHeader '.ida', 'ta', D$ImportTrueSize,
            &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
    End_If

    If D$uRsrcList > 0
        Call WriteOneSectionHeader '.rsr', 'c', D$ResourcesTrueSize,
            &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ
    End_If

    If D$uDataSize > 0
        push D$NextSectionHeaderRVA
        Mov eax D$DataTrueSize
        Call WriteOneSectionHeader '.dat', 'a', eax, D$DataCharacteristics
           ; &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE

      ;  Mov eax D$uDataSize | add eax D$uVirtualDataSize
        Mov eax D$TrueUserDataSize | add eax D$uVirtualDataSize

        pop D$NextSectionHeaderRVA
        Mov D$edi-(8*4) eax | Align_On 01000 eax | add D$NextSectionHeaderRVA eax
    End_If

  ; 'AppTrueCodeSize'
    Call WriteOneSectionHeader '.tex', 't', D$CodeTrueSize, D$CodeCharacteristics
       ; &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_MEM_EXECUTE___&IMAGE_SCN_MEM_READ

  ; 'ExportSectionHeader'
    If B$ExportsectionWanted = &TRUE
        Mov ebx D$FinalBaseOfExport, eax D$NextSectionHeaderRVA, D$ebx eax

        Call WriteOneSectionHeader '.eda', 'ta', D$ExportSectionLen,
            &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ
    End_If

  ; 'RelocSectionHeader'
    .If D$SavingExtension = '.SYS'
        jmp L1>
    .Else_If D$RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
L1:     If D$RelocSectionSize => 8 ;jE!
            Mov ebx D$FinalAppBaseOfReloc, eax D$NextSectionHeaderRVA, D$ebx eax

            Call WriteOneSectionHeader '.rel', 'oc', D$RelocSectionSize,
                &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ
        End_If
    .End_If

    Mov ebx D$FinalImageSize, eax D$NextSectionHeaderRVA, D$ebx eax

  ; Security Dummy empty Headeers before Source
    Mov eax 0, ecx 10 | rep stosd
  ; SourceSectionHeader
  ; Not readable initialised data; don't keep; don't cache...
    Mov D$LocOfSourceHeader edi
    Call WriteOneSectionHeader '.src', 0, D$SourceLen, 06000840

    Mov ecx D$CodeList | add ecx 0400 | sub ecx edi | shr ecx 2
    Mov eax 0 | rep stosd
;Else
;    Mov ecx 20, eax 0 | rep stosd
;End_If
ret


; Writes 10 dWords for each Section Header:

[NextSectionHeaderRVA: ?    NextSectionHeaderFilePointer: ?]

Proc WriteOneSectionHeader:
    Arguments @NameLow, @NameHigh, @Size, @Flags

        Mov eax D@NameLow | stosd
        Mov eax D@NameHigh | stosd
        Mov eax D@Size | stosd
        Mov eax D$NextSectionHeaderRVA | stosd
        Mov eax D@Size | Align_On 0200 eax | stosd
        Mov ecx eax
        Align_On 01000 eax | add D$NextSectionHeaderRVA eax
        Mov eax D$NextSectionHeaderFilePointer | stosd
        Mov eax 0 | stosd | stosd | stosd
        Mov eax D@Flags | stosd
        add D$NextSectionHeaderFilePointer ecx
EndP



;&IMAGE_FILE_32BIT_MACHINE 0100h
;IMAGE_FILE_AGGRESIVE_WS_TRIM 010h
;&IMAGE_FILE_BYTES_REVERSED_HI 8000h
;&IMAGE_FILE_BYTES_REVERSED_LO 080h
;IMAGE_FILE_DEBUG_STRIPPED 0200h
;&IMAGE_FILE_DLL 2000h
;&IMAGE_FILE_EXECUTABLE_IMAGE 02h
;IMAGE_FILE_LARGE_ADDRESS_AWARE 020h
;&IMAGE_FILE_LINE_NUMS_STRIPPED 04h
;&IMAGE_FILE_LOCAL_SYMS_STRIPPED 08h
;IMAGE_FILE_NET_RUN_FROM_SWAP 0800h
;IMAGE_FILE_NET_RUN_FROM_SWAP 800
;IMAGE_FILE_RELOCS_STRIPPED 01h
;IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP 0400h
;IMAGE_FILE_SYSTEM 1000h
;IMAGE_FILE_UP_SYSTEM_ONLY 4000h
 _________________________________________________________________________________________

ResourcesStub:
____________________________________________________________________________________________

[NodeFlag  080000000]

    ;RT_NEWRESOURCE      02000
    ;RT_ERROR            07fff

[RT_AVI              0-1
 RT_WAVE             0-2
 RT_CURSOR           00_0001
 RT_BITMAP          00_0010
 RT_ICON             00_0011
 RT_MENU             00_0100
 RT_DIALOG           00_0101
 RT_STRING           00_0110
 RT_FONTDIR          00_0111
 RT_FONT             00_1000
 RT_ACCELERATORS     00_1001
 RT_RCDATA           00_1010
 RT_MESSAGETABLE     00_1011
 RT_GROUP_CURSOR     00_1100
 RT_GROUP_ICON       00_1110
 RT_VERSION     00_0001_0000]

    ;[RT_NEWBITMAP        (RT_BITMAP|RT_NEWRESOURCE)
    ; RT_NEWMENU          (RT_MENU|RT_NEWRESOURCE)
    ; RT_NEWDIALOG        (RT_DIALOG|RT_NEWRESOURCE) ]

;RT_GROUP_CURSOR  equ RT_CURSOR + DIFFERENCE
;RT_GROUP_ICON  equ RT_ICON + DIFFERENCE
;RT_VERSION   equ 16
;RT_DLGINCLUDE  equ 17
;RT_PLUGPLAY  equ 19
;RT_VXD   equ 20
;RT_ANICURSOR  equ 21
;RT_ANIICON   equ 22
;RT_HTML   equ 23
 ________________________________________________________________________________________

; Level 1:

[Resources: W$ 0,0,0,0,0,0,0,2                               ; 2 next resources records
 D$ RT_ICON,  0_80000020         ;Level2Rt_Icon-StartOfRsrc+NodeFlag ;80000028h (0C28h)
    RT_GROUP_ICON, 0_80000038    ;Level2Rt_Group_Icon-StartOfRsrc+NodeFlag;80000040h (0C40h)

; Level 2: (resource TYPEs directory)

Level2Rt_Icon:
W$ 0,0,0,0,0,0,0,1   ; 1 record
D$ 01, 080000050     ;Level3Rt_Icon-StartOfRsrc+NodeFlag;80000070h; ID 1: icon; > 0C70h

Level2Rt_Group_Icon:
W$ 0,0,0,0,0,0,0,1   ; 1 record
D$ 064, 080000068    ;Level3Rt_Group_Icon-StartOfRsrc+NodeFlag; ID 64h: Group icon > 0C88h

; Level 3: (last one to one language resources pointers - lang. dir -)

Level3Rt_Icon:
W$ 0,0,0,0,0,0,0,1
D$ 0409, 080 ;Level4Rt_Icon-StartOfRsrc;B8h, 409 = langage ID: can_english; B8h > CB8h

Level3Rt_Group_Icon:
W$ 0,0,0,0,0,0,0,1
D$ 0409, 090  ;Level4Rt_Group_Icon-StartOfRsrc   ;C8h, CC8h

;Level 4: (records of each resource: PTR, size, CodePage, reserved)

;;RsrcRVA = BaseOfRsrc-StartOfRsrc

Level4Rt_Icon:
;;D$ IconHeader+RsrcRVA, 02E8, 0, 0  ; icon at CF0h;         size=2E8h
D$ 020A8, 02E8, 0, 0

Level4Rt_Group_Icon:
;;D$ Group_Icon+RsrcRVA, 014, 0, 0   ; group icon at FD8h;   size = 14h
D$ 02390, 014, 0, 0

; icon data. This icon image is for compilation only. At start of RosAsm Run, the default
; icon is copyed from Icon Editor to here. The Editor version is in fact used as temporary
; storage.

uIcon:

IconHeader:
B$ 028,0,0,0   ; size
 020,0,0,0     ; width
 040,0,0,0     ; height (maybe 40h because of the two masks)
 01,0          ; planes
 04,0          ; bit count
 0,0,0,0       ; compression 0
 080,02,0,0    ; 0280 > size of icon data
 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0   ; (dummy)


IconPalette:
;Blue,green,red,0    (16 color palette -values seams to be fixed ones-)

   0,  0,  0,  0 ; color 0
   0,  0,080,  0 ;       1
   0,080,  0,  0 ;       2
   0,080,080,  0 ;       3
 080,  0,  0,  0 ;       4
 080,  0,080,  0 ;       5
 080,080,  0,  0 ;       6
 0C0,0C0,0C0,  0 ;       7
 080,080,080,  0 ;       8
   0,  0,0FF,  0 ;       9
   0,0FF,  0,  0 ;       A
   0,0FF,0FF,  0 ;       B
 0FF,  0,  0,  0 ;       C
 0FF,  0,0FF,  0 ;       D
 0FF,0FF,  0,  0 ;       E
 0FF,0FF,0FF,  0 ;       F

IconXorMask:
; XOR color mask: (32*16 octets > 2 pixels / byte > 32*32 pixels)

     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0C,0F0,  0
    09,099,099,099,099,099,099,099,099,099,099,099,099, 0C,0C0,  0
    09,099,099,099,099,099,099,099,099,099,099,099,099, 0C,0CC,  0
     0,  0, 09,099,099,099,099,099,099,099,099,099,099, 0C,0CC,0C0
    03,03B, 09,099,099,099,099,099,099,099,099,099,099, 0C,0CC,0CF
     0,033, 09,099,090,  0,  0,  0,  0,099,099,099,099, 0C,0CC,0CF
     0, 03, 09,099,090,0DB,0BB,0BB,0B0,099,099,099,099, 0C,0CC,0CF
     0,  0, 09,099,090,0DD,0BB,0BB,0B0,099,099,099,099, 0C,0CC,0CF
     0,  0, 09,099,090,0DD,0DB,0BB,0B0,099,099,099,099, 0C,0CC,0CF
     0,  0, 09,099,090,0DD,0DD,033,030,099,099,099,099, 0C,0CC,0CF
     0,  0,  0,  0,  0,0DD,0DD,0FF,0F0,099,099,099,099, 0C,0CC,0CF
     0,  0, 03,03B,0BB,0BD,0DD,  0,  0,099,099,099,099, 0C,0CC,0CF
     0, 0F,  0,033,0BB,0BB,0DD,  0,  0,099,099,099,099, 0C,0CC,0CF
     0,  0,0C0, 03,03B,0BB,0BD,  0,  0,  0,  0,  0,  0, 0C,0CC,0CF
    0E,0E0,0CC,  0,033,033,033,  0, 0F,033,0BB,0BB,0BB,0BB,0CC,0CF
    0E,0E0,0CC,0C0,  0,  0,  0,  0,0FF,0F3,03B,0BB,0BB,0BB,0BC,0CF
    0E,0E0,0CC,0CC,  0,  0,  0,  0, 0C,0FF,033,0BB,0BB,0BB,0BB,0CF
    0E,0E0,0CC,0CC,  0, 0E,0EE,0EE, 0C,0CF,0F3,033,033,033,033,03F
    0E,0E0,0CC,0CC,  0, 0E,0EE,0EE, 0C,0CC,0FF,0FF,0FF,0FF,0FF,0FF
    0E,0E0,0CC,0CC,  0, 0E,0EE,0EE, 0C,0CC,0CF,0FF,0FF,0FF,0FF,0FF
    0E,0E0,  0,  0,  0, 0E,0EE,0EE, 0C,0C0,  0,  0,  0,  0,  0,  0
    0E,0EE,0EE,0EE,0EE,0EE,0EE,0EE, 0C,0C0,0AA,0AA,0AA,0AA, 0C,  0
    0E,0EE,0EE,0EE,0EE,0EE,0EE,0EE, 0C,0C0,0AA,0AA,0AA,0AA, 0C,0C0
    0E,0EE,0EE,0EE,0EE,0EE,0EE,0EE, 0C,0C0,0AA,0AA,0AA,0AA, 0C,0CC
    0E,0EE,0EE,0EE,0EE,0EE,0EE,0EE, 0C,0C0,0AA,0AA,0AA,0AA, 0C,0CC
     0,  0,  0,  0,  0,  0,  0,  0, 0C,0C0,0AA,0AA,0AA,0AA, 0C,0CC
    03,03B,0BB,0BB,0BB,0BB,0BB,0BB,0BB,0C0,0AA,0AA,0AA,0AA, 0C,0CC
     0,033,0BB,0BB,0BB,0BB,0BB,0BB,0BB,0B0,0AA,0AA,0AA,0AA, 0C,0CC
     0, 03,03B,0BB,0BB,0BB,0BB,0BB,0BB,0B0,  0,  0,  0,  0, 0C,0CC
     0,  0,033,033,033,033,033,033,033,033, 03,0BB,0BB,0BB,0BB,0CC
     0,  0,  0,  0,  0,  0,  0,  0,  0,0FF,0F0,03B,0BB,0BB,0BB,0BC
     0,  0,  0,  0,  0,  0,  0,  0,  0,0FF,0FF, 03,033,033,033,033

IconAndMask:
; AND monochrome mask: (8*16 octects > 8 pixels / byte > 32*32 pixels)

     0,  0,  0, 07,  0,  0,  0, 07,  0,  0,  0, 03,  0,  0,  0, 01
   080,  0,  0,  0,0C0,  0,  0,  0,0E0,  0,  0,  0,0F0,  0,  0,  0
   0F0,  0,  0,  0,0F0,  0,  0,  0,0F0,  0,  0,  0,0F8, 03,080,  0
   0EC, 03,080,  0, 06, 03,080,  0, 03, 03,080,  0, 01,0FF,  0,  0
     0,0C0,  0,  0,  0,0C0,  0,  0,  0,0C0,  0,  0,  0,0C0,  0,  0
     0,  0,  0, 07,  0,  0,  0, 03,  0,  0,  0, 01,  0,  0,  0,  0
     0,  0,  0,  0,  0,  0,  0,  0,080,  0,  0,  0,0C0,  0,  0,  0
   0E0,  0,  0,  0,0F0,  0,  0,  0,0FF,0FF,0C0,  0,0FF,0FF,0C0,  0

uIconEnd:
;;
 Group icon and entry may be considered as fixed, like upper headers. Theorycaly, they are
 not, but in practice, they are. More than that, all these values don't seam to be used
 by win95: changing them all to anything else doesn't make any difference... I suppose
 that Win simply reads the sections headers and jumps directly to the file icon header
 (before palette), which one is NOT unused.
;;
; group icon: (icons directory)

uGroupIcon:
W$  00      ; reserved
    01      ; type 1 (always)
    01      ; 1 entry following
            ; As opposed to what doc says: no padding here.
; icon entry:
B$ 020, 020 ; width, height
B$ 010, 0   ; 16 colors, reserved
W$ 01, 04   ; 1 color plane, 4 bits
D$ 02E8     ; size in bytes (true)
W$ 1        ; Seams to be the order number for both Cursors and icons as they come in the tree.
uGroupIconEnd:
EndOfRsrc: 0]
____________________________________________________________________________________________

[CopyOfuIcon:

;CopyOfIconHeader:
B$ 028,0,0,0   ; size
 020,0,0,0     ; width
 040,0,0,0     ; height (maybe 40h because of the two masks)
 01,0          ; planes
 04,0          ; bit count
 0,0,0,0       ; compression 0
 080,02,0,0    ; 0280 > size of icon data
 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0   ; (dummy)


;CopyOfIconPalette:
;Blue,green,red,0    (16 color palette -values seams to be fixed ones-)

   0,  0,  0,  0 ; color 0
   0,  0,080,  0 ;       1
   0,080,  0,  0 ;       2
   0,080,080,  0 ;       3
 080,  0,  0,  0 ;       4
 080,  0,080,  0 ;       5
 080,080,  0,  0 ;       6
 0C0,0C0,0C0,  0 ;       7
 080,080,080,  0 ;       8
   0,  0,0FF,  0 ;       9
   0,0FF,  0,  0 ;       A
   0,0FF,0FF,  0 ;       B
 0FF,  0,  0,  0 ;       C
 0FF,  0,0FF,  0 ;       D
 0FF,0FF,  0,  0 ;       E
 0FF,0FF,0FF,  0 ;       F

;CopyOfIconXorMask:
; XOR color mask: (32*16 octets > 2 pixels / byte > 32*32 pixels)

     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0C,0F0,  0
    09,099,099,099,099,099,099,099,099,099,099,099,099, 0C,0C0,  0
    09,099,099,099,099,099,099,099,099,099,099,099,099, 0C,0CC,  0
     0,  0, 09,099,099,099,099,099,099,099,099,099,099, 0C,0CC,0C0
    03,03B, 09,099,099,099,099,099,099,099,099,099,099, 0C,0CC,0CF
     0,033, 09,099,090,  0,  0,  0,  0,099,099,099,099, 0C,0CC,0CF
     0, 03, 09,099,090,0DB,0BB,0BB,0B0,099,099,099,099, 0C,0CC,0CF
     0,  0, 09,099,090,0DD,0BB,0BB,0B0,099,099,099,099, 0C,0CC,0CF
     0,  0, 09,099,090,0DD,0DB,0BB,0B0,099,099,099,099, 0C,0CC,0CF
     0,  0, 09,099,090,0DD,0DD,033,030,099,099,099,099, 0C,0CC,0CF
     0,  0,  0,  0,  0,0DD,0DD,0FF,0F0,099,099,099,099, 0C,0CC,0CF
     0,  0, 03,03B,0BB,0BD,0DD,  0,  0,099,099,099,099, 0C,0CC,0CF
     0, 0F,  0,033,0BB,0BB,0DD,  0,  0,099,099,099,099, 0C,0CC,0CF
     0,  0,0C0, 03,03B,0BB,0BD,  0,  0,  0,  0,  0,  0, 0C,0CC,0CF
    0E,0E0,0CC,  0,033,033,033,  0, 0F,033,0BB,0BB,0BB,0BB,0CC,0CF
    0E,0E0,0CC,0C0,  0,  0,  0,  0,0FF,0F3,03B,0BB,0BB,0BB,0BC,0CF
    0E,0E0,0CC,0CC,  0,  0,  0,  0, 0C,0FF,033,0BB,0BB,0BB,0BB,0CF
    0E,0E0,0CC,0CC,  0, 0E,0EE,0EE, 0C,0CF,0F3,033,033,033,033,03F
    0E,0E0,0CC,0CC,  0, 0E,0EE,0EE, 0C,0CC,0FF,0FF,0FF,0FF,0FF,0FF
    0E,0E0,0CC,0CC,  0, 0E,0EE,0EE, 0C,0CC,0CF,0FF,0FF,0FF,0FF,0FF
    0E,0E0,  0,  0,  0, 0E,0EE,0EE, 0C,0C0,  0,  0,  0,  0,  0,  0
    0E,0EE,0EE,0EE,0EE,0EE,0EE,0EE, 0C,0C0,0AA,0AA,0AA,0AA, 0C,  0
    0E,0EE,0EE,0EE,0EE,0EE,0EE,0EE, 0C,0C0,0AA,0AA,0AA,0AA, 0C,0C0
    0E,0EE,0EE,0EE,0EE,0EE,0EE,0EE, 0C,0C0,0AA,0AA,0AA,0AA, 0C,0CC
    0E,0EE,0EE,0EE,0EE,0EE,0EE,0EE, 0C,0C0,0AA,0AA,0AA,0AA, 0C,0CC
     0,  0,  0,  0,  0,  0,  0,  0, 0C,0C0,0AA,0AA,0AA,0AA, 0C,0CC
    03,03B,0BB,0BB,0BB,0BB,0BB,0BB,0BB,0C0,0AA,0AA,0AA,0AA, 0C,0CC
     0,033,0BB,0BB,0BB,0BB,0BB,0BB,0BB,0B0,0AA,0AA,0AA,0AA, 0C,0CC
     0, 03,03B,0BB,0BB,0BB,0BB,0BB,0BB,0B0,  0,  0,  0,  0, 0C,0CC
     0,  0,033,033,033,033,033,033,033,033, 03,0BB,0BB,0BB,0BB,0CC
     0,  0,  0,  0,  0,  0,  0,  0,  0,0FF,0F0,03B,0BB,0BB,0BB,0BC
     0,  0,  0,  0,  0,  0,  0,  0,  0,0FF,0FF, 03,033,033,033,033

;CopyOfIconAndMask:
; AND monochrome mask: (8*16 octects > 8 pixels / byte > 32*32 pixels)

     0,  0,  0, 07,  0,  0,  0, 07,  0,  0,  0, 03,  0,  0,  0, 01
   080,  0,  0,  0,0C0,  0,  0,  0,0E0,  0,  0,  0,0F0,  0,  0,  0
   0F0,  0,  0,  0,0F0,  0,  0,  0,0F0,  0,  0,  0,0F8, 03,080,  0
   0EC, 03,080,  0, 06, 03,080,  0, 03, 03,080,  0, 01,0FF,  0,  0
     0,0C0,  0,  0,  0,0C0,  0,  0,  0,0C0,  0,  0,  0,0C0,  0,  0
     0,  0,  0, 07,  0,  0,  0, 03,  0,  0,  0, 01,  0,  0,  0,  0
     0,  0,  0,  0,  0,  0,  0,  0,080,  0,  0,  0,0C0,  0,  0,  0
   0E0,  0,  0,  0,0F0,  0,  0,  0,0FF,0FF,0C0,  0,0FF,0FF,0C0,  0

CopyOfuIconEnd: D$ CopyOfuIconEnd-CopyOfuIcon]


RestoreDefaultIcon:
    Mov esi CopyOfuIcon, edi uIcon, ecx D$CopyOfuIconEnd | rep movsb
    Mov esi CopyOfuIcon, edi iIcon, ecx D$CopyOfuIconEnd | rep movsb
ret


____________________________________________________________________________________________

;;
 This is what would looks like a hand made menu:

[uMenu:     U$ 01 04 00 00       ; 01 unknown by me; 04 must be &RT_MENU

0 0 0 0          M00_Tree       0   0  '&Tree' 0  0


0 0 0 0          0                0   1  '&File' 0  0 0 0
0 0 0 0          M00_New         0   0  '&New' 0
0 0 0 0          M00_Open        0   0  '&Open' Tab 'F3' 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 0 0          M00_Open_source_only 0   0  'Open source only' 0
0 0 1 0          M00_Open_Icon_only   0   0  'Open Icon only' 0
0 0 1 0          M00_Open_Menu_only   0   0  'Open Menu only' 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 1 0          M00_Side_open   0   0  '&Side open' 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 1 0          M00_Save_As      0   0  'Save &As' 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 0 0          M00_Save_Source_only 0   0  'Save source only' Tab 'F2' 0 0
0 0 1 0          M00_Save_Icon_only   0   0  'Save Icon only' Tab 'F2' 0 0
0 0 1 0          M00_Save_Menu_only   0   0  'Save Menu only' Tab 'F2' 0 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 0 0          M00_Exit        0 080  'E&xit' Tab 'Alt+X' 0 0


0 0 0 0          0               0   1  '&Edit' 0  0 0 0
0 0 1 0          M00_Undo       0   0  '&Undo' Tab 'Ctrl+Z' 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 0 0          M00_Cut        0   0  'Cu&t' Tab 'Ctrl+X'   0 0
0 0 0 0          M00_Copy       0   0  '&Copy' Tab 'Ctrl+C'    0
0 0 0 0          M00_Paste      0   0  '&Paste' Tab 'Ctrl+V' 0 0
0 0 0 0          M00_Delete     0   0  '&Delete' Tab 'Del'   0 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 1 0          M00_Select_All  0 080  'Select &All'  0 0

0 0 0 0          0               0   1  'Search' 0 0 0
0 0 0 0          M00_Find       0   0  'Find' 0
0 0 0 0          M00_Replace    0 080  'Replace' 0 0

0 0 0 0          M00_Compile      0   0  '&Compile' 0           ; alone

0 0 1 0          M00_Run          0   0  '&Run' 0

0 0 0 0          M00_Calc         0   0  'Calc' 0

0 0 1 0          M00_HexAsc       0   0  'HexAsc' 0

0 0 0 0          0                 0   1  'Resources' 0 0 0 0
0 0 0 0          M00_Icon         0   0  '&Icon' 0 0
0 0 1 0          M00_Dialog_box       0   0  '&Dialog Box' 0 0
0 0 1 0          M00_BitMap       0   0  '&BitMap' 0 0
0 0 1 0          M00_Cursor       0   0  '&Cursor' 0 0
0 0 1 0          M00_String       0   0  '&String' 0 0
0 0 1 0          M00_Font         0   0  '&Font' 0 0
0 0 1 0          M00_Accelerators 0   0  '&Accelerators' 0 0
0 0 1 0          M00_RcData       0   0  'RcData' 0
0 0 1 0          M00_MessageTable 0   0  'MessageTable' 0
0 0 0 0          M00_Menus         0 080  '&Menus' 0

0 0 0 0          M00_Configuration       0   0  'Configuration' 0 0

0 0 0 0          0                 0    1  '&Help' 0 0 0 0
0 0 0 0          M00_Help_Editor    0    0  'Help &Editor' 0
0 0 0 0          M00_Help_Tree_view    0    0  'Help Tree &View' 0 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 1 0          M00_Asm_Mnemonics        0    0  'Asm &Mnemonics' 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 1 0          M00_Win32_Api_List         0    0  'Win32 &Api List' 0 0
0 0 1 0          M00_Win32_Equates_List      0    0  'Win32 E&quates List' 0 0
0 0 1 0          M00_Win32_Structures_List        0    0  'Win32 &Structures List' 0
0 0 1 0          M00_Win32_Data_Types        0    0  'Win32 Data &Types' 0 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 0 0          M00_About        0  080  '&About' Tab 'F1' 0 0

0 0 0 0          M00_<<<<_         0   0  '   <<<<  ' 0 0

0 0 0 0          M00_>>>>_      0   080  '   >>>>  ' 0 0


    ; hot keys (see later):

  ;  01  070  03E8  00    ;03E8 > about ID  (F1)
  ;  01  071  03F4  00    ;03F4 > Save ID   (F2)
  ;  01  072  03EC  00    ;03EC > Open ID   (F3)
  ; 090  078  03ED  00    ;03ED > Exit ID   (Alt-X)  78 > 'x'

  ; D$ 0 0 0 0 0 0 0

uMenuEnd: ]
;;

 __________________________________________________________________________________________
;;
 Resource section begin with a tree structure pointing to data. This tree is like a
 hard disk directory structure. At root, we find the main entries, one for each resource
 type. At last, we find something like a file icon: pointers to true data. This last one
 is a leave (the 'file icon', not the true data). Up others are nodes.
 The levels are:
 - root dir (points to resources types)
 - types dir (points to languages types)
 - pointers dir (points to true data)

 Each dir has an height words header. they are all zero but last one: records number in
 the dir; and as many records of two dWords. Exemple for root dir with two resources:

[Resources: W$ 0,0,0,0,0,0,0,2  ; 2 next resources records
 D$ &RT_ICON,  0_80000020        ; 080 > it points to a node.
 &RT_GROUP_ICON, 0_80000038      ; 020 / 038 are displacememnts Ptr (from start of .rsrc)

 Leaves have no 8 words header and are 4 dWords long (RVA ptr / size / CodePage / reserved)

 The only info i found about CodePage dWord is this:
 "... CodePage ...should be used when decoding code point values within the resource data.
  Typically for new applications the code page would be the unicode code page."
 (??????????????????!!!!!!!!!!!!!!!) >>> set at zero until i understand more...

 For .rsrc section construction, we first search for the total lenght of this tree, save
 pointers to each dir and work downside up: first write the data, then fill dirs backward.
 uRsrcsList is a temporary list containing needed information on each resource in
 memory.
;;
 ___________________________________________________________________________________________

[ID_Icon 1,  Language 0409,  ID_Group_Icon 1]

; resource temporary list: up to 200 resource >>> to change for dynamic memory later.

[DumRL: 0 0 0 0 0]  ; to stop upward search on zeros

[uRsrcList:         ; RosAsm infos storage for .rsrc section building (5 dWords per resource)
   D$ 0             ; Type
      0             ; ID
   0409             ; Language
      0             ; Data pointer
      0             ; Size
      #MAXRESOURCE]
[Dum2RL: 0 0 0 0 0]

[RsrcHeadPtr: 0  RsrcSectionOrigine: 0   RsrcSectionPtr: 0
              RsrcTypePtr: 0  RsrcLangPtr: 0  RsrcPtrPtr: 0
              uRsrcListPtr: 0]

[HeadLine | add edi 2  ; we have just written some dWords with strings instructions
            Mov eax #1 | stosw | Mov eax 0 | stosw | stosd | stosd | stosd
            sub edi 2] ; we are going to write some more dWords...


; We set here the list of existing resources for the app resources section. We store
; the type, the ID, Dummy language, Pointer to resource data and size. For menu, i let
; some 'RosAsm developpement internal' test that i use to study menus encoding:

[TypeByName: 0]

; Re-ordering the resources Tables Records in ID order:

SortRsrcTable:
    push eax, ebx, esi
L0:     Mov bl 0
        While D$esi > 0
            Mov eax D$esi
            .If eax > D$esi+12
                If D$esi+12 <> 0
                    Exchange D$esi D$esi+12, D$esi+4 D$esi+16, D$esi+8 D$esi+20
                    Mov bl 1
                End_If
            .End_If
            add esi 12
        End_While
        pop esi | push esi | cmp bl 1 | je L0<
    pop esi, ebx, eax
ret
____________________________________________________________________________________________

[NoResources: ?    NoMainIcon: ?]
 ________________________________________________________________________________________
 ________________________________________________________________________________________

;                                   data job
 ________________________________________________________________________________________
 ________________________________________________________________________________________

[LenOfDataSet: 0  AfterLastDataLabel: 0  DataLoopValue: 0  DefSize: 0]

BuildData:
    Mov eax D$CodeListPtr | Mov D$DataList eax | Mov D$DataListPtr eax

        Call StoreDatas

    Mov eax D$DataListPtr | sub eax D$DataList | Mov D$uDataSize eax

    Mov eax D$DataListPtr

  ; Clear all possible trailing left Source Text:
    push eax
        While B$eax <> 0
            Mov B$eax 0 | inc eax
        End_While
    pop eax

    Align_on 0200 eax | Mov D$CodeListPtr eax

        Call StoreVirtualData

;;
  Case of ?Data without any Data >>> We force a dummy .Data Section, because usually,
  in RosAsm outputed PEs, the ?Data are a simple RVA extension of Data, for saving the
  addition of one more, and no use .bss Section, with a different header:
;;
    .If D$uDataSize = 0
        If D$uVirtualDataSize <> 0
            add D$CodeListPtr 0200
            Mov D$uDataSize 1
        End_If
    .End_If

    Call StripDoneStatementsPointers
ret


TranslateText:
    lodsb                        ; strip first text sign

    On B$DefSize = uMem, jmp TranslateUnicode

L0: lodsb | cmp al TextSign | je L9>
      stosb | jmp L0<
L9: lodsb                        ; strip lasting one
    If al = CloseBracket
        dec esi
    Else_If al > Separators
        Error D$MissingSeparatorPtr
    End_If
    Mov edx 0
ret


TranslateUnicode:
L0: lodsb | cmp al TextSign | je L9>
        stosb | Mov al 0 | stosb | jmp L0<
L9: lodsb                        ; strip lasting one
    Mov edx 0
ret


; translations from text expressions to true numbers. Text is previously pointed by esi
; results stored in (ebx >) EDX:EAX

TranslateBinary:
    lodsw                                               ; clear first '00'
NackedBinary:
    Mov ebx 0, edx 0, ecx 0
L0: lodsb | cmp al Closebracket | jbe L9>
    sub al '0' | shld edx ebx 1 | shl ebx 1 | or bl al
    cmp edx ecx | jb L8>
        Mov ecx edx
            cmp al 2 | jb L0<
L8:             Mov ecx D$BinTypePtr | jmp BadNumberFormat
L9: Mov eax ebx
ret


TranslateHexa:
    lodsb                                               ; clear first '0'
NackedHexa:
    Mov ebx 0,  edx 0, ecx 0
L0: lodsb | cmp al LowSigns | jbe L9>
        sub al '0' | cmp al 9 | jbe L2>
            sub al 7
L2: shld edx ebx 4 | shl ebx 4 | or bl al
    cmp edx ecx | jb L8>
        Mov ecx edx
            cmp al 0F | jbe L0<
L8: Mov ecx D$HexTypePtr | jmp BadNumberFormat
L9: Mov eax ebx
ret


TranslateDecimal:
    Mov eax 0, ecx 0

L2: Mov cl B$esi | inc esi                        ; (eax used for result > no lodsb)
    cmp cl LowSigns | jbe  L9>

      Mov edx 10 | mul edx | jo L3>               ; loaded part * 10
                                                  ; Overflow >>> Qword
        sub  ecx '0' | jc L7>
        cmp  ecx 9   | ja L7>

          add  eax ecx | jnc  L2<
            jmp  L4>                              ; carry >>> Qword

                                                  ; if greater than 0FFFF_FFFF:
L3: sub ecx '0' | jc L7>
    cmp ecx 9   | ja L7>

      add eax ecx

L4:   adc edx 0
      Mov cl B$esi | inc  esi
      cmp cl LowSigns | jbe L9>

        Mov ebx eax, eax edx, edx 10 | mul edx    ; high part * 10
          jo L6>                                  ; Qword overflow
            xchg eax ebx | Mov edx 10 | mul edx   ; low part * 10
            add  edx ebx
            jnc   L3<                             ; carry >>> overflow

L6:           On B$esi < '0', error D$OverFlowPtr
              If B$esi <= '9'
                  inc esi | jmp L6<
              End_If

L7: Mov ecx D$DezimalTypePtr | jmp BadNumberFormat
L9: ret                                           ; >>> number in EDX:EAX


TranslateAny:
    If W$esi = '00'
        Call TranslateBinary
    Else_If B$esi = '0'
        Call TranslateHexa
    Else
        Call TranslateDecimal
    End_If
ret

;;
 In error cases, we come here with the error Message in ecx. This is done this way, in
 order to give zero time penality to Clean written Sources. Here, we check for
 alternate expressions of Numbers ( ...B, ... xB, ...D, ...xD, ...H, ...xH, and crazy
 leading zeros in excess).
 
 Note that the whole thingy may be run twice (No care of time cost for alien syntaxes,
 but full care of time for clean written Source -zero added cost for them-).
 
 esi is after the bad Char.
;;

BadNumberFormat:
    dec esi
L0: lodsb | On al = 'X', lodsb

    .If al = 'H'
        cmp B$esi LowSigns | ja L7>
    .Else_If al = 'D'
        cmp B$esi LowSigns | ja L7>
    .Else_If al = 'B'
        cmp B$esi LowSigns | ja L7>
    .Else
      ; Try to read a Type Marker at the end, and re-run if possible:
L7:     While B$esi > LowSigns | inc esi | End_While | dec esi | lodsb
        If al = 'H'
            On ecx = HexType, Error ecx
        Else_If al = 'D'
            On ecx = DezimalType, Error ecx
        Else_If al = 'B'
            On ecx = BinType, Error ecx
        Else
            Error ecx
        End_If
    .End_If

    dec esi
;;
 esi now points to the last Char of the Number. We overwrite it: We kill the Types Markers
 and we fill at the other end (start), with zeros:
;;
    push edi
        Mov edi esi | dec esi | On B$esi = 'X', dec esi
        std
            While B$esi > LowSigns | movsb | End_While
        cld
        While B$edi > LowSigns | Mov B$edi '0' | dec edi | End_While
    pop edi

    inc esi | While B$esi = '0' | inc esi | End_While

  ; Cases of, for example, "[<0200h" DataAlignment:
    On B$esi = '<', inc esi

    If al = 'H'
        jmp NackedHexa
    Else_If al = 'D'
        jmp TranslateDecimal
    Else  ; al = 'B'
        jmp NackedBinary
    End_If

____________________________________________________________________________________________

AsciiToQword:
L0: Call atof
    On al > CloseBracket, error D$BadRealPtr
    fadd D$edi
    fstp Q$edi

L9: cmp B$esi-1 addSign | jne L9>
        Mov al addSign | dec esi | jmp L0<<
L9: cmp B$esi-1 subSign | jne L9>
        Mov al subSign | dec esi | jmp L0<<

L9: add edi 8
    Mov eax edi | sub eax D$DataListPtr | add D$LenOfDataSet eax
    Mov D$DataListPtr edi
ret

FAsciiToQword:
L0: Call atof
    On al > CloseBracket, error D$BadRealPtr
    fadd D$edi
    fstp D$edi

L9: cmp B$esi-1 addSign | jne L9>
        Mov al addSign | dec esi | jmp L0<<
L9: cmp B$esi-1 subSign | jne L9>
        Mov al subSign | dec esi | jmp L0<<

L9: add edi 4
    Mov eax edi | sub eax D$DataListPtr | add D$LenOfDataSet eax
    Mov D$DataListPtr edi
ret


TAsciiToQword:
L0: Call atof
    On al > CloseBracket, error D$BadRealPtr
    fadd D$edi
    fstp T$edi

L9:   cmp B$esi-1 addSign | jne L9>
        Mov al addSign | dec esi | jmp L0<<
L9:   cmp B$esi-1 subSign | jne L9>
        Mov al subSign | dec esi | jmp L0<<

L9: add edi 10
    Mov eax edi | sub eax D$DataListPtr | add D$LenOfDataSet eax
    Mov D$DataListPtr edi
ret


ParseFpWords:
    push esi

        Mov ax 07FFF

        If B$esi = addSign
            inc esi
        Else_If B$esi = subSign
            inc esi | Mov ax 0FFFF
        End_If

L1:     ...If B$esi = 'N'
            ..If B$esi+1 = 'A'
                .If B$esi+2 = 'N'
                    If B$esi+3 < LowSigns
                        Mov D$edi 0, D$edi+4 0, W$edi+8 07FFF
                        or W$edi+8 ax | add esi 4 | pop eax | jmp L9>
                    End_If
                .End_If
            ..End_If

        ...Else_If B$esi = 'I'
            ..If B$esi+1 = 'N'
                .If B$esi+2 = 'F'
                    If B$esi+3 < LowSigns
                        Mov D$edi 0FFFFFFFF, D$edi+4 0FFFFFFFF, W$edi+8 07FFF
                        or W$edi+8 ax | add esi 4 | pop eax | jmp L9>
                    End_If
                .End_If
            ..End_If

        ...Else_If D$esi = 'QNAN'
          ; QNaN  : S=0  E=7FFF  I=1  F=4000_0000_0000_0000..7FFF_FFFF_FFFF_FFFF
            Mov D$edi 0FFFFFFFF, D$edi+4 0FFFFFFFF, W$edi+8 07FFF
            or W$edi+8 ax | add esi 4 | pop eax | jmp L9>

        ...End_If

    pop esi | ret

L9: add edi 10
    Mov eax edi | sub eax D$DataListPtr | add D$LenOfDataSet eax
    Mov D$DataListPtr edi
ret


[DataSign: 0]

StoreDataRef:
    On B$DefSize <> 'D' error D$DsizeForLabPtr
    push edi
        Mov edi D$DataRefPtr
        ..If B$Dynamic = &TRUE
            If edi > D$DataRefLimit
                Call ExtendTableMemory DataRef, DataRefPtr, DataRefLimit
                Mov edi D$DataRefPtr
            End_If
        ..End_If

L0:     lodsb | cmp al LowSigns | jb L1>
        stosb | jmp L0<
L1:     Mov al EOI | stosb
        Mov eax D$DataListPtr | stosd
        Mov eax D$bracketCounter | stosd
        Mov al B$DataSign | stosb
        Mov al EOI | stosb
      Mov D$DataRefPtr edi
    pop edi
ret


; j'ai oubli  de commenter   '<<<<' pourquoi faut-il ajouter un dWord apr s la valeur de
; LEN. Sans ce 0 suppl mentaire, la donn e suivante est  crasee (0). Comme  a, tout
; fonctionne mais je ne sais plus pourquoi!!!!!!!!!!...........

[HeadLenPtr: 0    HeadLenFlag: 0    HeadLenSize: dMem]

StoreOneData:
    Mov B$DataSign &FALSE
    push edi

        cmp B$esi+1 MemMarker | jne L0>
            lodsb | Mov B$DefSize al | lodsb

L0:     Mov edi D$DataListPtr | Mov D$edi 0 | Mov al B$esi
            cmp al Separators | ja L0>
            inc esi | Mov al B$esi

L0:     cmp al 'L' | jne L0>>
            cmp B$esi+1 'E' | jne L0>>
            cmp B$esi+2 'N' | jne L0>>
            cmp B$esi+3 LowSigns | ja L0>>

              ; Direct storage for LEN keyword:
                Mov eax D$LenOfDataSet
                .If eax = 0
                    Mov B$HeadLenFlag &TRUE, D$HeadLenPtr edi

                  ; Store dummy zero. Will be overwritten, after, at the top of 'StoreDatas'
                  ; HeadLen may be W$ for some Win32 Structures. Byte should be no use:
                    If B$DefSize = dMem
                        stosd | Mov D$HeadLenSize dMem, D$LenOfDataSet 4
                    Else_If B$DefSize = wMem
                        stosw | Mov D$HeadLenSize wMem, D$LenOfDataSet 2
                    Else_If B$DefSize = bMem
                        stosb | Mov D$HeadLenSize bMem, D$LenOfDataSet 1
                    Else
                        error D$LenSizePtr
                    End_If

                .Else_If B$HeadLenFlag = &TRUE
                    error D$MixedLenPtr

                .Else
                  ; Normal Len (after Data) is always a dWord, whatever 'DefSize':
                    stosd | Mov D$LenOfDataSet 0

                .End_If

                Mov D$DataListPtr edi | add esi 4
    pop edi
    ret

L0:     cmp B$DefSize 'R' | jne L0>
        Call AsciiToQword
    pop edi
    ret

L0:     cmp B$DefSize 'H' | jne L0>
        error D$NewHmemPtr
      ; Call AsciiToQword
    pop edi
    ret

L0:     cmp B$DefSize 'F' | jne L0>
        Call FAsciiToQword
    pop edi
    ret

L0:     cmp B$DefSize 'T' | jne L0>
      ;  push esi
      ;      Call ParseFpWords
      ;  pop eax
      ;  On esi = eax,
        Call TAsciiToQword
    pop edi
    ret

L0:     Mov B$DataSign &FALSE | cmp al SubSign | jne L1>
        Mov B$DataSign &TRUE | lodsb | Mov al B$esi | jmp L1>
L1:     cmp al addsign | jne L2>
        lodsb | Mov al B$esi
L2:     cmp al TextSign | jne L2>
        Call TranslateText | jmp E9>>   ; (direct storage of text in Translation routine)
L2:     cmp al '0' | ja L4>
        cmp B$esi+1 '0' | jne L2>
            Call TranslateBinary | jmp L5>
L2:     Call TranslateHexa | jmp L5>
L4:     cmp al '9' | jbe L4>
            Call StoreDataRef | jmp L9>>
L4:     Call TranslateDecimal

L5:     cmp edx 0 | je L5>
        On B$DefSize <> 'Q', error D$OverFlowPtr
            jmp Q7>>

L5:     cmp B$DefSize 'D' | je L7>
        cmp B$DefSize 'B' | je L6>
        cmp B$DefSize 'W' | je U0>
        cmp B$DefSize 'U' | je U0>
        cmp B$DefSize 'Q' | je Q7>                              ; keep (Q$ with edx = 0)
            error D$UnknownSizePtr

U0:     cmp eax 0FFFF | ja L8>>                                 ; if here, DefSize = 'W'
        On B$DataSign = &TRUE, neg ax | add W$edi ax | jnc L9>  ; include 0 unicode endMark
            jmp L8>>
L6:     cmp eax 0FF | ja L8>>                                   ; 'B'
        On B$DataSign = &TRUE, neg al | add B$edi al | jnc L9>
            jmp L8>>
L7:     On B$DataSign = &TRUE, neg eax | add D$edi eax | jnc L9>; 'D'
        jmp L8>>
Q7:     If B$DataSign = &TRUE
            push 0 | push 0 | sub D$esp eax | sbb D$esp+4 edx ;jE!
            pop eax | pop edx
        End_If

        add D$edi eax | jnc Q8>
        add edx 1 | On edx = 0, error D$OverFlowPtr
Q8:     add edi 4 | add D$edi edx | jnc Q9>
            jmp L8>>
Q9:     sub edi 4

L9:     cmp B$esi-1 addSign | jne L9>
            Mov al addSign | dec esi | jmp L0<<
L9:     cmp B$esi-1 subSign | jne L9>
            Mov al subSign | dec esi | jmp L0<<

L9:     cmp B$DefSize 'D' | jne L9>
            add edi 4 | jmp E9>
L9:     cmp B$DefSize 'B' | jne L9>
            inc edi   | jmp E9>
L9:     cmp B$DefSize 'Q' | jne L9>
            add edi 8 | jmp E9>
L9:     add edi 2                         ; good for 'W' and 'U'

E9:     Mov eax edi | sub eax D$DataListPtr | add D$LenOfDataSet eax
        Mov D$DataListPtr edi
    pop edi
ret

L8: error D$OverFlowPtr

 _________________________________________________________________________________________
;;
 storage of labels and datas founded in data declarations square brackets:
 datas are stored in 'DataList'. Labels are stored in the same 'LabelList' as
 code labels, but with adress high bit set.
 indentifications of either label or data is made through Space or Colon ending signs.
 the only one difficulty is that a declaration might end with a label (in case
 user programmer wish to know some data lenght without using 'len', for exemple)
;;
 _________________________________________________________________________________________

AlignDataSet:
    ..If B$esi = '<'
        lodsb                       ; strip '<'
        On B$esi = Space, lodsb
        If W$esi = '00'
            Call TranslateBinary | cmp eax 0 | je L9>>
            dec eax | add D$DataListPtr eax
            inc eax | neg eax | and D$DataListPtr eax

        Else_If B$esi = '0'
            Call TranslateHexa | cmp eax 0 | je L9>>
            dec eax | add D$DataListPtr eax
            inc eax | neg eax | and D$DataListPtr eax

        Else_If B$esi < '1'
            ; Don't Align

        Else_If B$esi > '9'
            ; Don't Align

        Else
            Call TranslateDecimal
            dec eax | add D$DataListPtr eax
            inc eax | neg eax | and D$DataListPtr eax

        End_If
    ..Else
        push esi
            While B$esi > LowSigns | inc esi | End_While | inc esi
            Mov eax 4
            .If B$esi+1 = memMarker
                If B$esi = 'B'
                    Mov eax 1
                Else_If B$esi = 'D'
                    Mov eax 4
                Else_If B$esi = 'W'
                    Mov eax 2
                Else_If B$esi = 'U'
                    Mov eax 2
                Else_If B$esi = 'F'
                    Mov eax 4
                Else_If B$esi = 'R'
                    Mov eax 8
                Else_If B$esi = 'Q'
                    Mov eax 8
                Else_If B$esi = 'T'
                    Mov eax 16
                End_If
            .End_If
        pop esi
        Align_On_Variable eax D$DataListPtr
    ..End_If
L9: ret


[DataLabelsCounter: ?    CodeLabelsCounter: ?] [DataLoopWinEquate: ? #20]
[LOOPDATAMAX 01_000]

[ColonWanted: ?]

StoreDatas:
    Mov esi D$CodeSourceB
    Mov B$ErrorLevel 0,  D$bracketCounter 0, D$DataLabelsCounter 0
    Mov D$HeadLenPtr 0, D$HeadLenFlag 0, D$HeadLenSize dMem

    move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4

L0: .If B$HeadLenFlag = &TRUE
        Mov ebx D$HeadLenPtr, eax D$LenOfDataSet, B$HeadLenFlag &FALSE

        If B$HeadLenSize = dMem
            Mov D$ebx eax
        Else_If B$HeadLenSize = wMem
            On eax > 0FFFF, error D$OverWordPtr
            Mov W$ebx ax
        Else
            On eax > 0FF, error D$OverBytePtr
            Mov B$ebx al
        End_If
    .End_If

L1: lodsb
    If al = TextSign
        While B$esi <> TextSign
            lodsb
        End_While
        lodsb | jmp L1<

    Else_If al = OpenVirtual
        inc D$bracketCounter | add D$StatementsPtr 4
        While al <> CloseVirtual
             lodsb
        End_While
        jmp L1<

    Else_If al = Openbracket
        inc D$bracketCounter | add D$StatementsPtr 4 | jmp L1>

    Else_If al = EOI
        ret

    Else
        jmp L1<

    End_If

L1: Mov D$LenOfDataSet 0
    Mov B$DefSize 'D'   ; default size: dWord

    Call AlignDataSet

L2: push esi
L3:     lodsb                                   ; we search ending char (first for 'text')
        cmp al LowSigns | ja L3<
          cmp al TextSign     | je L5>          ; Text (first sign, no need of last one)
          cmp al Space        | je L5>          ; data
          cmp al Closebracket | je L5>          ; lasting data
          cmp al meEOI        | je L5>          ; no error if no comma...
          cmp al ColonSign    | je L6>          ; label (last sign)
          cmp al NumSign      | je L7>>
          cmp al addsign      | je L5>
          cmp al subSign      | je L5>

          cmp al memMarker    | je L3<
            error D$BadSeparatorPtr

L5: pop esi

    Call StoreOneData | jmp L9>>                ; if data

; Case "__:__"
L6:     Mov B$ColonWanted &FALSE
        On B$esi = meEOI, inc esi               ; no '|' betweem ':' and data
        Mov D$AfterLastDataLabel esi
    pop esi

    Call StoreDataLabel                         ; if label
    On B$LocalLabel = &TRUE, error D$NoDataLocalLabelPtr
    inc D$DataLabelsCounter
    Mov esi D$AfterLastDataLabel | jmp L9>>     ; Called Subs don't change ESI

; Case "__#__"
L7: On B$esi-2 > PartEnds, error D$WhatIsThisPtr
    On B$ColonWanted = &TRUE, error D$NestedLoopPtr
    pop eax

    cmp B$esi-2 memMarker | je E1>
    cmp B$esi-2 ColonSign | jne E2>

E1:     error D$BadLoopPtr
E2:
    On B$esi < '0', error D$BadLoopNumberPtr
    On B$esi > '9',  error D$BadLoopNumberPtr
    cmp D$DataLoopValue 0 | ja L7>
      If B$esi = '0'
        Call TranslateHexa
      Else
        Call TranslateDecimal
      End_If
      On edx > 0, error D$DataLoopNumberPtr
      On eax > LOOPDATAMAX, error D$DataLoopNumberPtr
      On eax < 2, error D$SmallDataLoopPtr
      Mov D$DataLoopValue eax

L7: dec D$DataLoopValue | cmp D$DataLoopValue 0 | ja L8>      ; full set when edx ...
L7: lodsb
      cmp al LowSigns | ja L7<
        Mov B$ColonWanted &TRUE | jmp L9>

L8: Mov esi D$AfterLastDataLabel | jmp L2<<

L9: If B$esi-1 = Closebracket
       Mov eax D$StatementsPtr, D$eax DoneFlag
       jmp L0<<                    ; lasting data
    End_If
    On B$esi  <> Closebracket,  jmp L2<<                      ; lasting label
    Mov eax D$StatementsPtr, D$eax DoneFlag
    inc esi | jmp L0<<

_____________________________________________________________________________________

StoreOneVirtualData:
    cmp B$esi+1 MemMarker | jne L0>
        lodsb | Mov B$DefSize al | lodsb

L0: lodsb
        cmp al Separators | ja L0>
            lodsb

L0: cmp al '?' | je L0>
        error D$VirtualDataPtr

L0: .If B$DefSize = 'D'
        Mov eax 4
    .Else_If B$DefSize = 'B'
        Mov eax 1
    .Else_If B$DefSize = 'W'
        Mov eax 2
    .Else_If B$DefSize = 'U'
        Mov eax 2
    .Else_If B$DefSize = 'Q'
        Mov eax 8
    .Else_If B$DefSize = 'R'
        Mov eax 4
    .Else_If B$DefSize = 'H'
        Mov eax 8
    .Else_If B$DefSize = 'F'
        Mov eax 4
    .Else_If B$DefSize = 'T'
        Mov eax 10
    .Else
        error D$UnknownSizePtr
    .End_If

    add D$uVirtualDataSize eax | add D$DataListPtr eax

    On B$esi < Separators, inc esi
ret

;;
 Beware, here, we both read from and copy to 'CodeSourceB'. We strip off at once Brackets
 and Virtual Brackets Declarations. We didn't strip Data Brackets previouly to allow
 error checking of user source Brackets.
;;
[uVirtualDataSize: ?]
[NoVirtualLimit: &FALSE]
[LOOPVDATAMAX 010_0000]

StoreVirtualData:
    Mov esi D$CodeSourceB,  edi D$CodeSourceB
    Mov B$ErrorLevel 0,  D$bracketCounter 0, D$uVirtualDataSize 0

    move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4

; Next 'if' could be reused (???) if we want to speed up "Looping" '?' Data.
; (All of this could be much faster...):
;
L0: ;If B$HeadLenFlag = &TRUE
;      Mov ebx D$HeadLenPtr, eax D$LenOfDataSet, D$ebx eax, B$HeadLenFlag &FALSE
;    End_If

L1: lodsb
      If al = OpenBracket
          While al <> CloseBracket
              lodsb
              cmp al TextSign | jne T9>
          T0: lodsb | cmp al TextSign | jne T0<
          T9:
          End_While
          inc D$bracketCounter | add D$StatementsPtr 4
          jmp L1<
      End_If
      cmp al OpenVirtual | je L1>               ; search for data declaration
      stosb | cmp al EOI | jne L1<
           On B$esi = OpenVirtual, jmp S0>
           On B$esi = Openbracket, jmp S0>
           add D$StatementsPtr 4
S0:  On B$edi-2 = EOI,  dec edi         ; prevents unwished '||', wich occurs for exemple
                                            ; in: ...|[val: 25]|...  after data strip
        cmp B$esi EOI | jne L0<             ; end of source
          stosb
            Mov eax edi | sub eax D$CodeSourceB | Mov D$StripLen eax
            Mov al EOI, ecx 10 | rep stosb      ; ajout recent pour tester un plantage
          ret

L1: inc D$bracketCounter | add D$StatementsPtr 4

    Mov B$DefSize 'D'   ; default size: dWord

  ; Re-Write that shit: Need an 'AlignVirtualDataSet' version, instead.
    push D$DataListPtr
        Call AlignDataSet
    pop eax
    If D$DataListPtr > eax
        Mov ebx D$DataListPtr | sub ebx eax | add D$uVirtualDataSize ebx
    End_If

L2: push esi
L3:   lodsb                                     ; we search ending char (first for 'text')
        cmp al LowSigns | ja L3<
          cmp al TextSign     | je L4>          ; Text (first sign, no need of last one)
          cmp al Space        | je L5>          ; data
          cmp al CloseVirtual | je L5>          ; lasting data
          cmp al meEOI        | je L5>          ; no error if no comma...
          cmp al ColonSign    | je L6>          ; label (last sign)
          cmp al NumSign      | je L7>>
          cmp al addsign      | je L4>
          cmp al subSign      | je L4>
        jmp L3<

L4: error D$VirtualDataPtr

L5: pop esi | Call StoreOneVirtualData | jmp L9>>   ; if data

L6:   Mov B$ColonWanted &FALSE
      move D$DataListPtrAtLastColon D$DataListPtr
      On B$esi = meEOI, inc esi                 ; no '|' betweem ':' and data
      Mov D$AfterLastDataLabel esi
    pop esi
    Call StoreDataLabel                         ; if label
    On B$LocalLabel = &TRUE, error D$NoDataLocalLabelPtr
    inc D$DataLabelsCounter
    Mov esi D$AfterLastDataLabel | jmp L9>>     ; Called Subs don't change ESI

L7: On B$esi-2 > PartEnds, error D$WhatIsThisPtr
    On B$ColonWanted = &TRUE, error D$NestedLoopPtr
    pop eax
    cmp B$esi-2 memMarker | je E1>
    cmp B$esi-2 ColonSign | jne E2>
E1:     error D$BadLoopPtr

E2: On B$esi < '0', error D$BadLoopNumberPtr
    On B$esi > '9',  error D$BadLoopNumberPtr

    cmp D$DataLoopValue 0 | ja L7>
        If B$esi = '0'
            Call TranslateHexa
        Else
            Call TranslateDecimal
        End_If
        On edx > 0, error D$VDataLoopNumberPtr
        If B$NoVirtualLimit = &FALSE
            On eax > LOOPVDATAMAX, error D$VDataLoopNumberPtr
        End_If
        On eax < 2, error D$SmallDataLoopPtr
        Mov D$DataLoopValue eax

L7: dec D$DataLoopValue | cmp D$DataLoopValue 0 | ja L8>        ; full set when edx ...
L7: lodsb
      cmp al LowSigns | ja L7<
        Mov B$ColonWanted &TRUE | jmp L9>

L8:     If D$DataLoopValue > 1
            Mov eax D$DataListPtr | sub eax D$DataListPtrAtLastColon
            Mov ecx D$DataLoopValue | dec ecx | mul ecx
            add D$DataListPtr eax | add D$uVirtualDataSize eax
            Mov D$DataLoopValue 1
        End_If

        Mov esi D$AfterLastDataLabel | jmp L2<<

L9: If B$esi-1 = CloseVirtual
        Mov eax D$StatementsPtr, D$eax DoneFlag | jmp L0<<      ; lasting data
    End_If
    On B$esi <> CloseVirtual,  jmp L2<<                         ; lasting label
        inc esi | Mov eax D$StatementsPtr, D$eax DoneFlag | jmp L0<<

_________________________________________________________________________________________
 _________________________________________________________________________________________

; Labels deal

[StartOfLabelName: ?  LocalLabel: B$ ?]

E0: error D$BadLabelPtr

StoreDataLabel:
    Mov edx D$DataListPtr | Mov cl DataLabelFlag | jmp L0>

StoreCodeLabel:
    Mov edx D$CodeListPtr | Mov cl CodeLabelFlag

L0: push edi
        Mov edi D$LabelListPtr | Mov ebx 0 | Mov D$StartOfLabelName esi
        and B$esi 00_0111_1111

        On B$esi = ColonSign, error D$OrphanColonPtr

L1:     lodsb | cmp al ColonSign | je L2>               ; esi set by caller
        inc ebx                                         ; ebx < lenght of label name
        cmp al '.' | jb E0<
        cmp al 'Z' | ja E0<
        stosb | jmp L1<                                 ; write to LabelList

L2:     Mov al EOI | stosb | Mov B$LocalLabel &FALSE
        cmp ebx 2 | jne L3>                             ; is it a possible local label?
            Mov ah B$esi-3,  al B$esi-2
            cmp ah 'A' | jb L3>
                cmp ah 'Z' | ja L3>
                    cmp al '0' | jb L3>
                        cmp al '9' | ja L3>
                            Mov B$LocalLabel &TRUE      ; if yes, multiple decs. allowed

L3:   Mov eax edx                                       ; either data or code List Pointer
      stosd                                             ; label offset (code or data)
      Mov al cl | stosb                                 ; code or data flag byte
      Mov al EOI | stosb

    ;  Mov esi D$StartOfLabelName                        ; LabelListPtr > first label letter
      On B$LocalLabel = &FALSE, Call SetQwordCheckSum D$LabelListPtr
    ;  On B$LocalLabel = &FALSE, Call IsItNewDataOrCodeLabel       ; send error message if not new

      Mov D$LabelListPtr edi

      If edi > D$LabelListLimit
        Call ExtendTableMemory LabelList, LabelListPtr, LabelListLimit
      End_If

    pop edi
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

OpError: Error D$NotAnOpcodePtr
OperandsError: Error D$OperandsTypesPtr
OperandSizeError: Error D$OperandSizePtr
NoFpAssumeError: Error D$FPregNotAssumedPtr

[BadMnemonic | jmp OpError]
[BadOperand | jmp OperandsError]
[BadOperandSize | jmp OperandSizeError]
[NoFpAssume | jmp NoFpAssumeError]

[op1 ah,  op2 al,  op3 bh,  op4 bl,  op5 ch,  op6 cl,  op7 dh,  op8 dl]

[LineStart: D$ 0,  Operands: B$ 0]

[ParametersData: 0  ParametersNumber: 0
 LongRelative: 0  dis32: 0  imm64: imm32: 0 0   ABSimm32: 0  SignedImm8: 0
 Relative: 0                   ; set with >flag when coding calls or jumps

 LocalSize: B$ 0
 Ereg: 0  SIB: 0
 EregInside: 0  immInside: 0  DisInside: 0   SibInside: 0
 DummyDis: 0                   ; true or false, for SIB without dis and without base

 LabelInside: B$ 0  ExpressedLabels: 0  TrueSize: 0
 FirstParaMem: 0  SecondParaMem: 0
 FirstReg: 0  SecondReg: 0  ThirdReg: 0
 FirstGender: 0  SecondGender: 0  ThirdGender: 0
 FirstRegGender: 0  SecondRegGender: 0  ThirdRegGender: 0

 RMbits: B$ 0  wBit: 0            ; for size encoding 1 or 0
 wBitDisagree: 0  sBit: 0            ; 010B or 0
 sBitPossible: 0  sBitWritten: 0
 OneOperandwBit: 0            ; for temporary storage inside One parameter analyze
 FirstOperandwBit: 0            ; These two
 SecondOperandwBit: 0            ; for test of fitting sizes
 ThirdOperandwBit: 0
 ModBits: 0  SIBreg1: 0  SIBreg2: 0
 ScaledIndex: 0  Base: 0  ScaleFound: 0  Reg1isIndex: 0  Reg2isIndex: 0  TwoRegsFound: 0
 PossibleRmEreg: 0  ExpressedSign: 0

 PossibleImmLabel: B$ 0  PossibleFirstImmLabel: 0

 ParametersDataLen: len]

[relativeFlag  00__10000000_00000000_00000000_00000000]

 _________________________________________________________________________________________
 _________________________________________________________________________________________

; expression deal: ModR/M, SIB and immediate in expressions

[IfEregFound | cmp B$Ereg NotFound | jne #1]

[IfEregNotFound | cmp B$Ereg NotFound | je #1]

[IfNotPartEnd | cmp #1 PartEnds | ja #2]

[IfSeparator | cmp #1 Separators | jb #2]

IsitaReg:
     Call Store8cars | On op3 nb Separators,  jmp L5>>      ; a 2 letters reg name?

L0: ifnot op2 'X', L2>
        ifnot op1 'A', L1>
            Mov al regAx | jmp L3>
L1:     ifnot op1 'B', L1>
            Mov al regBx | jmp L3>
L1:     ifnot op1 'C', L1>
            Mov al regCx | jmp L3>
L1:     On op1 <> 'D',  jmp L9>>
            Mov al regDx

L3:     Mov W$esi 0 | add esi 3 | Mov ah reg | Mov B$OneOperandwBit WordSize | ret

L2: ifnot op2 'H', L2>
        ifnot op1 'A', L1>
                Mov al regAh | jmp L3>
L1:     ifnot op1 'B', L1>
            Mov al regBh | jmp L3>
L1:     ifnot op1 'C', L1>
            Mov al regCh | jmp L3>
L1:     On ah <> 'D',  jmp L9>>
            Mov al regDh | jmp L3>

L2: ifnot op2 'L', L2>
        ifnot op1 'A', L1>
            Mov al regAl | jmp L3>
L1:     ifnot op1 'B', L1>
            Mov al regBl | jmp L3>
L1:     ifnot op1 'C', L1>
            Mov al, regCl | jmp L3>
L1:     On op1 <> 'D',  jmp L9>>
            Mov al, regDl

L3:     Mov W$esi 0 | add esi 3 | Mov ah reg | Mov B$OneOperandwBit ByteSize | ret

L2: ifnot op2 'P', L2>
        ifnot op1 'B', L1>
            Mov al regBp | jmp L3>
L1:     On op1 <> 'S',  jmp L9>>
            Mov al regSp | jmp L3>

L2: ifnot op2 'I', L2>
        ifnot op1 'S', L1>
            Mov al regSi | jmp L3>
L1:     On op1 <> 'D',  jmp L9>>
            Mov al regDi

L3:     Mov W$esi 0 | add esi 3 | Mov ah reg | Mov B$OneOperandwBit WordSize | ret

L2: ifnot op2 'S', L5>
        ifnot op1 'E', L1>
           Mov al regEs | jmp L3>
L1:     ifnot op1 'C', L1>
           Mov al regCs | jmp L3>
L1:     ifnot op1 'S', L1>
           Mov al regSs | jmp L3>
L1:     ifnot op1 'D', L1>
           Mov al regDs | jmp L3>
L1:     ifnot op1 'F', L1>
           Mov al regFs | jmp L3>
L1:     On op1 <> 'G',  jmp L9>>
           Mov al regGs

L3:     Mov W$esi 0 | add esi 3 | Mov ah sReg | Mov B$OneOperandwBit WordSize | ret

; Is it a 3 letters MMX register name?

L5: On op4 nb Separators,  jmp L9>>
       IfNot op1, 'M', L5>
         IfNot op2, 'M', L5>
           cmp op3, '0' | jb L5>
           cmp op3, '7' | jg L5>
             sub bh, '0' | Mov al bh | Mov ah MMreg
             and D$esi 0FF000000 | add esi 4 | Mov B$OneOperandwBit DoubleSize | ret

; Is it a 3 letters STx register name?

L5: IfNot op1, 'S', L5>
        IfNot op2, 'T', L5>
           cmp op3, '0' | jb L5>
           cmp op3, '7' | jg L5>
             sub bh, '0' | Mov al bh | Mov ah STreg
             and D$esi 0FF000000 | add esi 4 | Mov B$OneOperandwBit DoubleSize | ret

; Is it a 3 letters DRx register name?

L5: IfNot op1, 'D', L5>
        IfNot op2, 'R', L5>
           cmp op3, '0' | jb L5>
           cmp op3, '7' | jg L5>
             sub bh, '0' | Mov al bh | Mov ah dReg
             and D$esi 0FF000000 | add esi 4 | Mov B$OneOperandwBit DoubleSize | ret

; Is it a 3 letters CRx register name?

L5: IfNot op1, 'C', L5>
        IfNot op2, 'R', L5>
           cmp op3, '0' | jb L5>
           cmp op3, '4' | jg L5>
             sub bh, '0' | Mov al bh | Mov ah cReg
             and D$esi 0FF000000 | add esi 4 | Mov B$OneOperandwBit DoubleSize | ret


;IsItAreg32:

L5: On op1 <> 'E',  jmp L9>
        ifnot op3 'X', L3>
            ifnot op2 'A', L2>
                Mov al regEAX | jmp L4>
L2:         ifnot op2 'C', L2>
                Mov al regECX | jmp L4>
L2:         ifnot op2 'D', L2>
                Mov al regEDX | jmp L4>
L2:         On op2 <> 'B',  jmp L9>
            Mov al regEBX | jmp L4>
L3:     ifnot op3 'P', L3>
            ifnot op2 'S', L2>
                Mov al regESP | jmp L4>
L2:         On op2 <> 'B',  jmp L9>
                Mov al regEBP | jmp L4>
L3:     On op3 <> 'I',  jmp L9>
            ifnot op2 'S', L2>
                Mov al regESI | jmp L4>
L2:         On op2 <> 'D',  jmp L9>
                Mov al regEDI

L4:     and D$esi 0FF000000 | add esi 4 | Mov ah, Reg | Mov B$OneOperandwBit DoubleSize | ret

L9: ; 4 Chars Register? > XMM.?

    ifnot op1 'X', L9>
        ifnot op2 'M', L9>
            ifnot op3 'M', L9>
                cmp op4, '0' | jb L9>
                    cmp op4, '7' | jg L9>
                        sub op4, '0' | Mov al op4 | Mov ah XMMreg
                        Mov D$esi 0 | add esi 5
                        Mov B$OneOperandwBit OctoSize | ret


L9: Mov eax 0 | ret


; when called 'E' found, esi points to the next letter
; 'IsItAnEreg' doesn't change esi
; 'SearchForEreg' does.

IsItAnEreg:
    Mov B$Ereg NotFound | Call Store4cars | IfNotPartEnd op3, L9>>

    IfNotPartEnd B$esi-2, L9>>
        ifnot op2 'X', L2>
            ifnot op1 'A', L1>
                Mov B$Ereg regEAX | ret
L1:         ifnot op1 'C', L1>
                Mov B$Ereg regECX | ret
L1:         ifnot op1 'D', L1>
                Mov B$Ereg regEDX | ret
L1:         ifnot op1 'B', L9>
                Mov B$Ereg regEBX | ret
L2:     ifnot op2 'P', L2>
            ifnot op1 'B', L1>
                Mov B$Ereg regEBP | ret
L1:         ifnot op1 'S', L9>
                Mov B$Ereg regESP | ret
L2:     ifnot op2 'I', L9>
            ifnot op1 'S', L1>
                Mov B$Ereg regESI | ret
L1:         ifnot op1 'D', L9>
                Mov B$Ereg regEDI | ret
L9: ret


SearchForEreg:
    Mov B$Ereg NotFound                     ; this double declaration is NOT useless
L0: lodsb | cmp al, 0 | je L0<
        cmp al Separators | jb L9>
            cmp al PartEnds | jb L0<
                On al = 'E',  Call IsItAnEreg
                IfEregNotFound L0<

    Mov B$EregInside &TRUE | Mov al B$Ereg,  B$PossibleRmEReg al
    add esi 2                               ; esi >>> next char. after Ereg.
    Mov B$esi-3 0,  B$esi-2 0,  B$esi-1 0   ; clear eReg
    ret

L9: dec esi                                 ; reach end of text word >>> Stay on EOI
ret                                         ; or Space for further possible calls


; SIB byte is [xx xxx xxx] >>> [sf index base]
; in  ECX*4+EDX  , ECX is the index, 4 is the scale factor, EDX is the base.
; when SearchForScale is called , an Ereg has been found. ESI points to possible '*'

SearchForScale:
    Mov B$ScaleFound &FALSE | cmp B$esi MulSign | jne L9>
    IfNotPartEnd B$esi+2,  L8>
    Mov al B$esi+1
    cmp al '2' | jne L1>
        Mov al 00_0100_0000 | jmp L3>
L1: cmp al '4' | jne L2>
        Mov al 00_1000_0000 | jmp L3>
L2: cmp al '8' | jne L8>
        Mov al 00_1100_0000
L3: On B$Ereg = regESP,  error D$ESPsibPtr              ; ESP can not be an index
    add esi 2 | Mov B$ScaleFound &TRUE
    Mov B$esi-2 0,  B$esi-1 0                      ; clear scale
    Mov bl B$Ereg | shl bl 3 | or bl al            ; store [xx xxx ...]
    Mov B$ScaledIndex bl                           ; in ScaledIndex
L9: ret
L8: error D$ScaleValuePtr


; forbidden use of non extended registers in memory adressing statements:

VerifyNoOtherReg:
    push esi
L0:   lodsb
      cmp al 0 | je L0<
        cmp al Separators | jb L9>
            cmp al PartEnds | jb L0<
                dec esi | Call IsItAreg | cmp eax 0 | je L1>
                On B$OneOperandwBit <> DoubleSize, error D$WishEregPtr
                On ah <> Reg, error D$WishEregPtr
L1:             inc esi
L2:             cmp B$esi PartEnds | jb L0>         ; strip remaining text
                    inc esi | jmp L2<
L9: pop esi
ret


SearchForSIB:
    Mov B$SibInside &FALSE, B$Reg1isIndex &FALSE, B$Reg2isIndex &FALSE, B$TwoRegsFound &FALSE
    push esi
      Call SearchForEreg | IfEregFound L0>
        pop esi
          Call VerifyNoOtherReg
        ret

L0: Mov al B$Ereg,  B$SIBreg1 al

    Call SearchForScale | Mov al B$ScaleFound, B$Reg1isIndex al   ; true or FALSE
    Call SearchForEreg | IfEregNotFound L1>
        On B$esi-4 <> addSign, error D$ExpressionPtr
        Mov al B$Ereg,  B$SIBreg2 al | Mov B$TwoRegsFound &TRUE

        Call SearchForScale | Mov al B$ScaleFound,  B$Reg2isIndex al
        Call SearchForEreg | On B$Ereg <> NotFound,  error D$expressionPtr    ; 3 regs forbidden

        .If B$ScaleFound = &FALSE
            If B$SIBreg2 = RegEBP
              ; Exchange the two regs, for saving from having to add the zero Dis:
                Mov al B$SIBreg1, bl B$SIBreg2
                Mov B$SIBreg2 al, B$SIBreg1 bl
            End_If
        .End_If

L1: Mov al B$Reg1isIndex,  ah B$Reg2isIndex                    ; 2 index forbidden
    On ax = 0101,  error D$DoubleIndexPtr
    cmp ax 0 | je L4>                                          ; if no index found >L8

    Mov B$SIBinside &TRUE | cmp ax 1 | je L2>            ; if here: 1 reg / 1 index
        Mov al B$ScaledIndex | or al B$SIBreg1                 ; reg2 is Index and
        Mov B$SIB al | jmp L9>>                                 ; reg1 is base

L2: cmp B$TwoRegsFound &TRUE | je L3>
        Mov B$SibReg2 00101                              ; if no base > base = 00101
        Mov B$DummyDis &TRUE                                    ;

L3: Mov al B$ScaledIndex | or al B$SIBreg2                      ; reg1 is Index
      Mov B$SIB al | jmp L9>

L4: cmp B$TwoRegsFound &TRUE | jne L5>                           ; no index, but
        Mov B$SibInside &TRUE
        If B$SIBreg1 = 00_100
          ; Other way round, because there is no esp Index (only Base)
            Mov al B$SIBreg2 | On al = 00_100, error D$ExpressionPtr
            shl al, 3 | or al B$SIBreg1
        Else
            Mov al B$SIBreg1 | shl al, 3 | or al B$SIBreg2
        End_If

        Mov B$SIB al | jmp L9>

L5: cmp B$SIBreg1 regESP | jne L9>             ; no index found, only one reg found, but
        Mov B$SibInside &TRUE | Mov B$SIB 024     ; if reg = ESP >SIB
L9: pop esi
    ret


[immSign: ?]

SearchForImm:      ; if yes, return value set in eax by translating routines
                   ; esi incrementations done by translating routine
    Mov B$immSign &FALSE
    push esi | jmp L1>

L9: pop esi | ret

L0:     inc esi
L1:     Mov dh B$esi-1,  B$ExpressedSign dh | Mov ah B$esi,  al B$esi+1
        cmp ah textSign | je L7>>
        ifSeparator ah, L9<
        cmp ah '0' | jb L0<
        cmp ah '9' | ja L0<
        cmp dh PartEnds | ja L0<
        Mov B$immInside &TRUE
        push esi
            IfNot ah, '0', L2>
            IfNot al, '0', L3>
            Call translateBinary | jmp L4>
L2:         Call TranslateDecimal | jmp L4>
L3:         Call TranslateHexa
L4:         On edx > 0, error D$OutOfRangePtr
            cmp B$ExpressedSign Space | je S1>

            cmp B$ExpressedSign addSign | jne S2>
S1:             Mov ebx D$imm32 | add D$imm32 eax
                On D$imm32 < ebx, Mov B$immSign &FALSE
                jmp L5>
S2:         On B$ExpressedSign <> SubSign,  error D$NotYetSignPtr
            On D$imm32 < eax, Mov B$immSign &TRUE
            sub  D$imm32 eax

L5:     pop esi
L6:     Mov B$esi 0                                         ; clear imm
        inc esi | cmp B$esi PartEnds | ja L6<
        jmp L1<<
;;
  Why the Api calls (that are "Call D$Location" Type...) are routed to 'SearchForImm',
  is because the Text Parameter ('Module.Function') looks like a 'Text' Member, for
  the Parsers of the Instructions Parameters.
;;
L7:     inc esi | Call NewSearchApiName | dec esi
        On B$ApiFound = &TRUE, jmp L9<<

        push edi
            Mov B$immInside &TRUE | Mov edi imm32 | lodsb   ; read text sign
L7:         Mov B$esi-1 0 | lodsb                           ; clear imm text
            cmp al TextSign | je L8>
            add B$edi al | inc edi
            On edi = imm32+5,  error D$TxtTooMuchPtr             ; max of 4 letters
            jmp L7<
L8:         Mov B$esi-1 0                                   ; clear last text marker sign
        pop edi
        jmp L1<<

L9: pop esi
    cmp D$imm32 07F        ; Test for sBit: turn sBit to 0010 if byte sign imm
        ja L9>                   ; max. is 07F for positive values (and  0FF for
        Mov B$sBit 0010               ; negative ones).
        Mov B$immSign &FALSE  ; Because no need for storing SignExtended Bytes in 'StoreImm'
L9:     Mov eax D$imm32
        and eax 0FFFFFF00 | cmp eax 0FFFFFF00 | jne L9>
        Mov B$sBit 0010
        Mov B$immSign &FALSE  ; Because no need for storing SignExtended Bytes in 'StoreImm'
L9: ret

;;
 I don't use upper chunk (from 'L9: pop esi'), because of the Sign Extensions able
 Opcodes. In case of Byte immediate with a possible immediate Label, we can't know
 here if there will be or not some trailing Label. Example:

 > and D$Label1+020 Label2-4

 Label2 will only be computed and the end of job, *after* other encodages. There is
 a test Routine, 'sim', that does this complicated job at the end of OpCode encodage.
 In cases of negative Sign Extended imm Bytes, the 'B$immSign' is turned &FALSE there
 to solve the problem of overflow tests done in 'StoreImm', for negative Bytes stored
 as dWords for 'non-Sign-Extended' Opcodes.
;;
____________________________________________________________________________________________

; Search for a displacement. If found, return value set by translating routine in eax:

SearchForDis:
    push esi | jmp L1>
L0:     inc esi
L1:     Mov dh B$esi-1,  B$ExpressedSign dh
        Mov ah B$esi,  al B$esi+1
        cmp ah 0 | je L0<
        ifSeparator ah, L8>>
        cmp ah '0' | jb L0<
        cmp ah '9' | ja L0<
        cmp dh PartEnds | ja L0<
        Mov B$DisInside &TRUE
        push esi
            IfNot ah, '0', L2>
            IfNot al, '0', L3>
            Call translateBinary | jmp L4>
L2:         Call TranslateDecimal | jmp L4>
L3:         Call TranslateHexa
L4:         On edx > 0, error D$OutOfRangePtr
            Mov bl B$esi-1
            If bl < Separators
            Else_If bl = addSign
            Else_If bl = subSign
            Else_If bl < LowSigns
                Error D$ExpressionPtr
            End_If
            cmp B$ExpressedSign Space | je S1>  ; ... ???? What case ???? ...
            cmp B$ExpressedSign 0 | je S1>
            cmp B$ExpressedSign addSign | jne S2>
                test D$Dis32 0_8000_0000 | jz S1>
                    neg D$Dis32 | sub D$Dis32 eax | neg D$Dis32 | jmp L6>
S1:                 add D$Dis32 eax | jmp L6>
S2:         On B$ExpressedSign <> SubSign,  error D$NotYetSignPtr
            test D$Dis32 0_8000_0000 | jz L5>
                neg D$Dis32 | add D$Dis32 eax | neg D$Dis32 | jmp L6>
L5:                 sub D$Dis32 eax
L6:     pop esi
L7:     Mov B$esi 0
        inc esi | cmp B$esi PartEnds | ja L7<
        jmp L1<<

L8: pop esi

    Mov eax D$Dis32        ; 080 > -128          //  07F > +127
    If eax > 0FFFF_FF7F    ; 0FFFF_FF80 > -128  //   0FFFF_FF7F > -129
        Mov B$LongRelative &FALSE
    Else_If eax < 080
        Mov B$LongRelative &FALSE
    Else
        Mov B$LongRelative &TRUE
    End_If
ret


SearchForLabel:
    push esi
        Mov edi D$CodeRefPtr
L0:     lodsb | cmp al 0 | je L0<
                cmp al Separators | jb L9>>
                cmp al 'A' | jb L0<
                cmp al, 'Z' | ja L0<
        On B$esi-2 = AddSign,  Mov B$esi-2 0
        cmp al 'E' | jne L1>
        push eax
            Call IsItAnEreg                 ; usefull only in case of mem adressing
        pop eax
        IfEregNotFound L1>
            add esi 2 | jmp L0<             ; (Mod/RM byte done after)

L1:     Mov B$esi-1 0                       ; clear label evocation

      ; Local size is used for Local Labels coding
        .If al = '>'
            add B$LocalSize 4     ; > 00100    >> 001000
            If B$LocalSize = 4
              ; OK.
            Else_If B$LocalSize <> 8
E7:             error D$WhatIsThisPtr
            End_If
            If B$esi = '>'
              ; OK
            Else_If B$esi > Separators
                jmp E7<
            End_If
        .Else_If al = '<'
            add B$LocalSize 1     ; < 001      << 0010
            If B$LocalSize = 1
              ; OK.
            Else_If B$LocalSize <> 2
                jmp E7<
            End_If
            If B$esi = '<'
              ; OK
            Else_If B$esi > Separators
                jmp E7<
            End_If

        .End_If

        .If D$esi-9 = 'LOOP'
            If B$LocalSize = 0
                ; OK.
            Else_If B$LocalSize <> 1
                error D$LongLoopPtr
            End_If
        .End_If

        stosb                               ; write label name in CodeRef
        lodsb | IfNotPartEnd al, L1<<
        Mov eax D$CodeRefPtr | add eax 2 | cmp eax edi | jne L2>  ; local label without '<'?
          Mov ah B$edi-2,  al B$edi-1
          cmp ah 'A' | jb L2>
            cmp ah 'Z' | ja L2>
              cmp al '0' | jb L2>
                cmp al '9' | ja L2>         ; Local label without direction specifier?
                  Mov al '<' | stosb        ; default, for exemple for LOOP L0
                    Mov B$LocalSize 1       ; UpShort
L2:       Mov al EOI | stosb
          inc B$ExpressedLabels
          On B$ExpressedLabels > 2, error D$TooMuchLabelsPtr
          Mov B$LabelInside &TRUE,  B$DisInside &TRUE

L9:     Mov D$CodeRefPtr edi
    pop esi
ret


SearchForModRMreg:
    Mov cl 32 | cmp B$LabelInside &TRUE | je L0>
    Mov eax D$Dis32

    On B$LongRelative = &FALSE, Mov cl 8
                                      ; for ByteSize tests on Displacement
                                                ; (080 >>> -128)
L0: push esi
        cmp B$SIBinside &TRUE | je L0>                      ; no SIB found? >>> no more regs
            cmp B$EregInside &FALSE | je L1>                ; may be, an Ereg?
                Mov al B$PossibleRmEreg | Mov B$RMbits al | jmp L3>>

; no RMreg > choice: simple dis32, dis followed by SIB byte, SIB byte only:

L0:     cmp B$DisInside &TRUE | je L2>
            Mov B$ModBits 0 | Mov B$RMbits 00100 | jmp L9>> ; SIB with no dis

L1:     cmp B$DisInside &TRUE | jne L9>>                    ; nothing at all > exit
            Mov B$ModBits 0 | Mov B$RMbits 00101 | jmp L9>> ; dis32 with no SIB and no reg

L2:     Mov B$RMbits 00100 | cmp cl 32 | je L2>
            cmp B$DummyDis &TRUE | je L2>
            Mov B$ModBits 00_01_000_000 | jmp L9>           ; dis8 + SIB
L2:             If B$TwoRegsFound = &TRUE
                    Mov B$ModBits 00_10_000_000             ; dis32 + SIB
                Else_If B$Sib = 024
                    Mov B$ModBits 00_10_000_000             ; dis32 + esp only SIB
                Else
                    Mov B$ModBits 0                         ; dis32 + SIB with no Base
                End_If
                jmp L9>

; RMreg found > choice: no dis, dis8, dis32 (if reg = EBP and no dis > add zero 8bits dis)

L3:     cmp B$DisInside &FALSE | je L6>
            cmp cl 32 | je L5>
L4:             Mov B$ModBits 00_01_000_000 | jmp L9>       ; dis8 + reg
L5:             Mov B$ModBits 00_10_000_000 | jmp L9>       ; dis32 + reg

L6:     cmp B$RmBits regEBP | jne L7>                       ; reg EBP >>> add zero dis
            Mov B$DisInside &TRUE | jmp L4<

L7:     Mov B$ModBits 0                                     ; reg without displacement

L9: pop esi

  ; This is to force things like "push D$fs:+0' to be encoded Long, and not short dis form:
    cmp B$DisInside &TRUE | jne L9>
        cmp B$LabelInside &FALSE | jne L9>
            cmp B$EregInside &FALSE | jne L9>
                Mov B$LongRelative &TRUE

L9: ret


SearchForFirstFPUimm:
    Mov B$FirstGender imm | jmp L0>
SearchForSecondFPUimm:
    Mov B$SecondGender imm

L0: push esi
        Call atof
        On al > CloseBracket, error D$BadRealPtr
        fstp D$imm32
        Mov B$ImmInside &TRUE, B$immSign &FALSE
    pop esi
L0: Mov B$esi 0                                     ; clear imm
    inc esi | cmp B$esi PartEnds | ja L0<
  ret


FirstParameterAnalyze:
    Mov esi D$LineStart

L0: lodsb                               ; simply increase esi over first space; after this,
    cmp al Space | jne L0<                    ; save esi pointing to parameter (> push/pop)

    cmp B$esi+1 memMarker | jne L4>>               ; if mem marker found, store ascii value
      Mov al B$esi | Mov B$FirstParaMem al
      Mov W$esi 0 | add esi 2                     ; (see equ. for bMem, wMem, dMem)
      If B$esi = MemMarker
        inc esi
      End_If
      Mov B$FirstGender mem

        On B$esi = EOI, error D$ParameterPtr

        .If al = dMem
             Mov B$FirstOperandwbit DoubleSize   ; D$            4 bytes
        .Else_If al = bMem
             Mov B$FirstOperandwbit ByteSize     ; B$            1 byte
        .Else_If al = wMem
             Mov B$FirstOperandwbit WordSize     ; W$            2 bytes
        .Else_If al = qMem
             Mov B$FirstOperandwbit QuadSize     ; Q$            8 bytes
        .Else_If al = rMem
             Mov B$FirstOperandwbit QuadSize     ; R$ = FPU Q$   8 bytes
      ;  .Else_If al = hMem
      ;       error NewHmem
       ;      Mov B$FirstOperandwbit QuadSize     ; H$ = FPU Q$   8 bytes >>> 16 bytes (!!!)
        .Else_If al = fMem
             If B$esi < 'A'
                 Call SearchForFirstFPUimm | ret     ; exemple: push F$45.2
             Else
                 Mov B$FirstOperandwbit DoubleSize   ; F$ = FPU D$
             End_If
        .Else_If al = tMem
             Mov B$FirstOperandwbit TenSize      ; T$ = FPU 10 Bytes / 80 bits (m80)
        .Else_If al = xMem
            Mov B$FirstOperandwbit XSize         ; Weird and XMM sizes
        .Else_If al = oMem
            Mov B$FirstOperandwbit OctoSize         ; Weird and XMM sizes
        .Else
            On al = hMem, error D$NewHmemPtr
            error D$UnknownSizePtr
        .End_If

L3:   Call SearchForSIB
      Call SearchForDis
      Call SearchForLabel
      ...If B$SibInside = &TRUE
        ..If B$DisInside = &FALSE
            .If B$ParametersNumber = 1
                If B$TwoRegsFound = &TRUE
                    Mov al B$SIB | and al 00_111
                    On al = 00_101, Mov B$DisInside &TRUE, B$LongRelative &true
                End_If
            .End_If
        ..End_If
    ...End_If
      Call SearchForModRMreg
        or B$ExpressedLabels 1                           ; we want B$ExpressedLabels < 3
        On B$ExpressedLabels > 2, error D$TooMuchLabelsPtr ; but only one Lab per member
    ret

L4: Call IsItaReg | cmp ah 0 | je L5>
      Mov B$FirstGender reg,  B$FirstReg al,  B$FirstRegGender ah
      Mov al B$OneOperandwbit,  B$FirstOperandwbit al
    ret

L5: Call SearchForImm | cmp B$ImmInside &FALSE | je L6>
      Mov B$FirstGender imm | Mov B$PossibleFirstImmLabel &TRUE
    ret

L6: If B$ApiFound = &TRUE   ; 'NewSearchApiName'
        Mov B$DisInside &TRUE, B$ApiFound &FALSE, B$FirstGender mem
        Mov B$ModBits 0, B$RmBits 00101
        Mov B$FirstOperandwBit doubleSize
        ret
    End_If

L6: Call SearchForLabel | On B$LabelInside = &FALSE,  error D$UnknownParameterPtr
      Mov B$FirstGender dis
      or B$ExpressedLabels 1                            ; we want B$ExpressedLabels < 3
      On B$ExpressedLabels > 2, error D$TooMuchLabelsPtr     ; but only one Lab per member
    ret
____________________________________________________________________________________________

SecondParameterAnalyze:
    Mov esi D$LineStart

L0: lodsb                                        ; simply increase esi over second space
    cmp al Space | jne L0<
L1: lodsb | cmp al Space | jne L1<

    cmp B$esi+1 memMarker | jne L5>>             ; if mem marker found, store ascii value
        Mov al B$esi | Mov B$secondParaMem al
        Mov W$esi 0 | add esi 2                  ; (see equ. for bMem, wMem, dMem, ...)
        Mov B$secondGender mem

        On B$esi = EOI, error D$ParameterPtr

        .If al = dMem
             Mov B$SecondOperandwBit DoubleSize   ; D$
        .Else_If al = bMem
             Mov B$SecondOperandwBit ByteSize     ; B$
        .Else_If al = wMem
             Mov B$SecondOperandwBit WordSize     ; W$
        .Else_If al = qMem
             Mov B$SecondOperandwBit QuadSize     ; Q$
        .Else_If al = rMem
             Mov B$SecondOperandwBit QuadSize     ; R$ = FPU Q$
        .Else_If al = hMem
             error D$NewHmemPtr
       ;      Mov B$SecondOperandwBit QuadSize     ; H$ = FPU Q$ (!!! > 16 bytes / 128 bits !!!)
        .Else_If al = fMem
             If B$esi < 'A'
                 Call SearchForSecondFPUimm | ret      ; exemple: Mov eax F$45.2
             Else
                 Mov B$SecondOperandwBit DoubleSize   ; F$ = FPU D$
             End_If

        .Else_If al = tMem
             Mov B$SecondOperandwBit TenSize      ; T$ = FPU 10 Bytes
        .Else_If al = xMem
            Mov B$SecondOperandwbit XSize         ; Weird and XMM sizes
        .Else_If al = oMem
            Mov B$SecondOperandwbit OctoSize      ; XMM sizes
        .Else
            On al = hMem, error D$NewHmemPtr
            error D$UnknownSizePtr
        .End_If

L4:     Call SearchForSIB
        Call SearchForDis
        Call SearchForLabel
        ...If B$SibInside = &TRUE
            ..If B$DisInside = &FALSE
                .If B$FirstGender = reg
                    If B$TwoRegsFound = &TRUE
                        Mov al B$SIB | and al 00_111
                        On al = 00_101, Mov B$DisInside &TRUE, B$LongRelative &FALSE
                    End_If
                .End_If
            ..End_If
        ...End_If
        Call SearchForModRMreg
    ret

L5:   Call IsItaReg | cmp ah 0 | je L6>
      Mov B$secondGender reg,  B$secondReg al,  B$SecondRegGender ah
      Mov al B$OneOperandwbit,  B$secondOperandwbit al

      ...If B$SibInside = &TRUE
        ..If B$DisInside = &FALSE
            .If B$FirstGender = Mem
                If B$TwoRegsFound = &TRUE
                    Mov al B$SIB | and al 00_111
                    On al = 00_101, Mov B$DisInside &TRUE, B$LongRelative &FALSE
                    Call SearchForModRMreg
                End_If
            .End_If
        ..End_If
    ...End_If

    ret

L6:   Call SearchForImm
      cmp B$ImmInside &FALSE | je L7>
      Mov B$secondGender imm | Mov B$PossibleImmLabel &TRUE
    ret

L7: If B$ApiFound = &TRUE   ; 'NewSearchApiName'
        Mov B$DisInside &TRUE, B$ApiFound &FALSE, B$SecondGender mem
        Mov B$ModBits 0, B$RmBits 00101
        Mov B$SecondOperandwBit doubleSize
        ret
    End_If

L7:   cmp D$DisInside &TRUE | jne L8>
      Mov B$secondGender imm | Mov B$PossibleImmLabel &TRUE
    ret

L8:  If B$ExpressedLabels = 0
        Call SearchForLabel
        On B$LabelInside = &FALSE,  error D$UnknownParameterPtr
        Mov B$SecondGender dis
      Else                                     ; Case of 'mov D$Lab1 Lab2'
        Mov B$ImmInside &TRUE, B$secondGender imm, B$PossibleImmLabel &TRUE
      End_If                                   ; Lab2 checked by PossibleImmLabel
      Mov B$sBit 0
    ret

FirstParameterLabel:
    Mov esi D$LineStart | jmp L1>

SecondParameterLabel:
    Mov esi D$LineStart
L0: lodsb | cmp al Space | jne L0<
L1: lodsb | cmp al Space | jne L1<
    push esi
      Mov edi D$CodeRefPtr
L0:   lodsb | cmp al 0 | je L0<
              cmp al Separators | jb L9>>
              cmp al 'A' | jb L0<
              cmp al, 'Z' | ja L0<
      On B$esi-2 = AddSign,  Mov B$esi-2 0
      cmp al 'E' | jne L1>
      push eax
        Call IsItAnEreg                      ; usefull only in case of mem adressing
      pop eax
          IfEregNotFound L1>
            add esi 2 | jmp L0<              ; (Mod/RM byte done after)
L1:     cmp al  '>' | je A1>
        cmp al  '<' | jne A2>
A1:       error D$NoLocalPtr
A2:     stosb                                ; write label name in CodeRef
        Mov B$esi-1 0
        lodsb | IfNotPartEnd al, L1<
        Mov eax D$CodeRefPtr | add eax 2 | cmp eax edi | jne L2>  ; no local label here
          Mov ah B$edi-2,  al B$edi-1
          cmp ah 'A' | jb L2>
            cmp ah 'Z' | ja L2>
              cmp al '0' | jb L2>
                cmp al '9' | ja L2>
                  jmp A1<                                          ; no local label here
L2:       Mov al EOI | stosb
          Mov eax D$CodeListPtr
          On B$immInside = &TRUE, sub eax 4
          stosd

          Mov eax D$StatementsPtr, eax D$eax | stosd     ; write pointer to source.
          Mov al EOI | stosb |  Mov D$CodeRefPtr edi

          If B$immInside = &FALSE
             Mov edi D$CodeListPtr, eax 0 | stosd | Mov D$CodeListPtr edi
          End_If

        ; Very stupid. 'FirstParameterLabel' /'SecondParameterLabel' to be re-structured:
          If B$ParametersNumber > 1
            On B$FirstOperandWbit <> DoubleSize, error D$MissTypePtr
          End_If
L9: pop esi
ret
____________________________________________________________________________________________

; Used only in case of SHLD / SHRD. third parameter must be either imm8 or CL

ThirdParameterAnalyze:
    Mov esi D$LineStart
L0: lodsb                            ; Simply increases esi over third space; after this,
    cmp al Space | jne L0<
L1: lodsb | cmp al Space | jne L1<
L2: lodsb | cmp al Space | jne L2<

        Call IsItaReg | cmp ah 0 | je L3>
        Mov B$ThirdGender reg,  B$ThirdReg al,  B$ThirdRegGender ah
        Mov al B$OneOperandwbit,  B$ThirdOperandwbit al
    ret

L3:     Call SearchForImm
        cmp B$ImmInside &FALSE | je L4>
        Mov B$ThirdGender imm
    ret

L4: error D$ParameterPtr
 _________________________________________________________________________________________
 _________________________________________________________________________________________
;;
 Main routine for one assembler line encoding (Data lines have been analyzed before)
 esi points to the first letter of an instruction in CodeSourceB
 we may find a label, one or more prefix(es), a mnemonic and parameters.
 Translates StrippedLine in OpCodes.
 after Data Storage, clean source is in  CodeSourceB as input
 results in:
            - LabelList      (data labels done with storage and now code labels)
            - CodeRef        (labels adresses evocations in codeList -will be used by
                              'fillCodeSymbols', in Main.a-)
            - CodeList       (true code for .code section)
            - Relocation     (for .reloc)

 i Call a 'Line' one set of statements between between '||'.
;;
 _________________________________________________________________________________________
 _________________________________________________________________________________________

StoreSIB:
   Mov edi D$CodeListPtr | Mov al B$SIB | stosb | inc D$CodeListPtr
ret
;;
 Label evocations are fulfill by 'Fill'. In case, for exemple, of
 'MOV EDI, My_Data_Label + 8', we store '8'
 as 'LongRelative' default is true, it is used here for any non short relative. this
 is to say even for not relative at all statements (Intel doc is killing on that point)
;;
StoreDis:
    If B$ApiDis = &TRUE
        Mov B$ApiDis &FALSE
        Mov edi D$CodeRefPtr, eax D$CodeListPtr     ; Store actual code offset in
        stosd                                       ; Coderef for ending 'Fill' routine
        Mov al EOI | stosb
        Mov D$CodeRefPtr edi
;;
  The CodeRef registration of Api calls does not have the Source Pointer Record.
  (No use as all Errors Checking have already been done when collecting the Api
  calls). So, only // 0FF / PointerToCode / EOI //, here, where "0FF" is written
  by 'NewSearchApiName' (called by 'SearchForImm').
  
  Why the Api calls (that are "Call D$Location" Type...) are routed to 'SearchForImm',
  is because the Text Parameter ('Module.Function') looks like a 'Text' Parameter,
  for the Parsers.
;;
        Mov edi D$CodeListPtr
        Mov eax D$Dis32 | stosd
        Mov D$CodeListPtr edi
        ret
    End_If

    cmp B$LabelInside &TRUE | jne L1>
        Mov edi D$CodeRefPtr, eax D$CodeListPtr     ; Store actual code offset in
        or eax D$Relative | stosd                   ; Coderef for ending 'Fill' routine
        Mov eax D$StatementsPtr, eax D$eax | stosd  ; write pointer to source:
        Mov al EOI | stosb
        Mov D$CodeRefPtr edi

L1: Mov edi D$CodeListPtr
    cmp B$LabelInside &TRUE | jne L2>
      cmp B$LocalSize 4 | je L4>
        cmp B$LocalSize 1 | je L4>
          jmp L3>

L2: cmp B$DummyDis &TRUE | je L3>
    cmp B$LongRelative &TRUE | jne L4>              ; default is true

L3:     Mov eax D$Dis32 | stosd                     ; Store displacement in code
        Mov D$CodeListPtr edi
    ret

L4:     Mov al B$Dis32 | stosb
        Mov D$CodeListPtr edi
ret


StoreImm:
    cmp B$immSign &TRUE | jne L0>
        cmp B$TrueSize ByteSize | jne L1>
            On D$imm32 < 0_FFFF_FF00,  error D$overflowPtr
            and D$imm32 0FF | jmp L0>
L1:     cmp B$TrueSize WordSize | jne L0>
            On D$imm32 < 0_FFFF_0000,  error D$overflowPtr
            and D$imm32 0FFFF

L0: Mov edi D$CodeListPtr
    cmp B$TrueSize DoubleSize | je L2>
    cmp B$TrueSize ByteSize | je L3>

L1: On D$imm32 > 0FFFF,  error D$overflowPtr     ; word size
    Mov ax W$imm32 | stosw | jmp L9>

L2: Mov eax D$imm32 | stosd | jmp L9>       ; double size

L3: On D$imm32 > 0FF,  error D$overflowPtr       ; Byte size
    Mov al B$imm32 | stosb

L9: Mov D$CodeListPtr edi
ret

;;
 wBit is used when encoding instruction as 0 or 1, telling if it is byte or full
 size. full size is either 2 or 4 bytes: 32 bits > double word. With operand size
 override prefix, it is turn to word.
 'TrueSize' is used internally to hold lenght of code to write when fixing adresses
 of symbols after encoding (see Main)
;;
Store2Wbit:
    Mov al B$FirstOperandWbit | cmp B$secondOperandWbit al | je L0>
        Mov B$wBitDisagree &TRUE
ret


StoreFirstWbit:
    Mov al B$FirstOperandWbit

StoreWbit:
L0: cmp al WordSize | jne L1>
        Mov edi D$CodeListPtr
        Mov B$edi 066 | inc D$CodeListPtr        ; write operand size override prefix
L1:     Mov B$TrueSize al | and al 1 | Mov B$wBit al
ret


FixInstructionType:
    Mov edi D$CodeListPtr
    cmp B$FirstGender reg | jne L3>
      cmp B$SecondGender reg | jne L0>
        Mov B$Operands RegToReg | Call Store2Wbit | ret
L0:   cmp B$SecondGender mem | jne L1>
        Mov B$Operands MemToReg | Call Store2Wbit | ret
L1:   cmp B$SecondGender dis | je L2>
      cmp B$SecondGender imm | jne L9>>
L2:     Mov B$Operands ImmToReg | Call StoreFirstWbit | ret

L3: cmp B$FirstGender Mem | jne L6>
      cmp B$SecondGender reg | jne L4>
        Mov B$Operands RegToMem | Call Store2Wbit | ret

L4:   cmp B$SecondGender dis | je L5>
      cmp B$SecondGender Imm | jne L9>
L5:
        Mov B$Operands ImmToMem | Call StoreFirstWbit | ret

L6: cmp B$FirstGender imm | jne L9>
      cmp B$SecondGender reg | jne L7>
        Mov B$Operands RegToImm | ret
L7:   cmp B$SecondGender imm | jne L9>
        Mov B$Operands ImmToImm
        Mov esi D$LineStart | lodsb | cmp al, 'E' | jne L9>
             ret           ; exemple: ENTER 8, 2
                           ; exceptions: Job will be done when coding ENTER
L9: error D$MixTypePtr


SearchLineLabel:
L0: lodsb
    cmp al EOI | jbe L2>                        ; case of simple mnemonics, like |AAA|
        cmp al Space | jne L3>
L2:  Mov esi D$LineStart | jmp L9>
L3:     cmp al ColonSign | jne L0<
        push esi
            Mov esi D$LineStart | Call StoreCodeLabel  ; in 'LabelList'
            inc D$CodeLabelsCounter
        pop esi
        Mov D$LineStart esi                     ; new instruction's first letter after label
        If B$esi = ColonSign
            inc esi | inc D$LineStart           ; '::' for .export in DLL.
        End_If
    jmp L0<                                     ; Another Label following?:
L9: ret


ClearParameters:                                ;(clear parameters data area)
    push edi
        Mov al 0,  edi ParametersData,  ecx D$ParametersDataLen | rep stosb
    pop edi
ret


; Count of spaces = number of parameters (previous componants are cleared if any)

ParametersCount:
    Mov cl 0

    On D$esi = 'ENTE', jmp L9>             ; WordImm / ByteImm Param for Enter...
L0: lodsb | cmp al space | jne L1>
        inc cl                           ; one space >  +1 parameter in cl
        If B$esi-2 = MemMarker
            dec cl | Mov B$esi-1 MemMarker
        End_If
L1: cmp al EOI | ja L0<
L9: Mov B$ParametersNumber cl
ret

 _______________________________________________________________________________________
;;
 encodage of Apis calls is not done in 'Encode' but here with a memory indirect call:
  1111 1111 : mod 010 r/m
 the 3 routines are linked by jumps when needed (not by calls)
;;

[ApiFound: ?]

; Called with esi pointing to the next Char after 'TextSign'. Called from 'SearchForImm'
; because 'xxxx' may as well be a immediate.


; Previous holding of numbered Api (!!!...):
;
;    push edi
;        Mov edx D$ebx               ; Table 1 pointers Ptr
;        Mov ebx D$ebx+16            ; table 2 pointers Ptr
;        add edx ecx | add ebx ecx
;
;L0:     Mov edi D$edx               ; pointer to function name
;        test edi 0_8000_0000 | jz L1>
;L5:         lodsb | cmp al TextSign | jne L5<
;                pop edi | jmp EncodeApiCall

[ApiByNumber: ? #4] [ApiDis: ?]

NewSearchApiName:
; A Description of .Import is in the Disassembler 'CheckImport'.
    On D$uImportSize = 0, ret

    pushad

    Mov B$ApiFound &FALSE

  ; Adjust esi on the FunctionName first Char. We do not take care of DLL Name,
  ; because this verification has already been done by 'BuildImport'. Instead,
  ; we search along all Functions Chunks.
    While B$esi <> TextSign | inc esi | End_While
    While B$esi <> '.'
        dec esi | On B$esi = TextSign, jmp L0>
    End_While
L0: inc esi                             ; Skip '.' or TextSign >>> FunctionName
    Mov edx D$CodeList | add edx 0400   ; ebx > Import header dll pointers
    Mov ecx edx                         ; to ajust adress, down there.
    sub ecx 01000                       ; Base Of Import.

  ; edx will keep track of the header
  ; ebx will keep track of the Functions Pointers List
L0: Mov ebx D$edx+16

  ; Cases of DLL without .Import:
    If ebx = 0
        popad | ret
    End_If

    add ebx ecx      ; Address Table Pointer

  ; ebx > FunctionName Pointers List
L1: Mov edi D$ebx
  ; ??????????????
  ; Un-resolved problem: What if several Functions in different DLLs having the same
  ; Function number???!!!...
    test edi 0_8000_0000 | jz L2>
        If B$esi <> '0'
            push esi | jmp L4>
        End_If

        Mov eax edi | xor eax 0_8000_0000

        push esi, ebx, eax
            While B$esi = '0' | inc esi | End_While
            Mov ebx 0
H0:         lodsb | sub al '0' | On al > 9, sub al 7
            shl ebx 4 | or bl al
            cmp B$esi TextSign | jne H0<
        pop eax

        If eax = ebx
            pop ebx | jmp L5>
        End_If

        pop ebx | jmp L4>

L2: add edi ecx | add edi 2
  ; edi > Function name (+2 is for the leading word).

L3: push esi
L3:     Mov al B$esi | Mov ah B$edi | inc esi | inc edi | cmp al ah | je L3<
        cmp al TextSign | jne L4>
        cmp ah 0 | je L5>

  ; Not found:
L4: pop esi

    add ebx 4
    .If D$ebx = 0
        add edx 20
        If D$edx = 0
            popad | ret
        Else
            jmp L0<<
        End_If
    .Else
        jmp L1<<
    .End_If

  ; Found:
L5: pop esi
  ; Clear the 'DLL.Function':
    While B$esi <> TextSign | dec esi | End_While
    Do
        Mov B$esi 0 | inc esi
    Loop_Until B$esi = TextSign
    Mov B$esi 0

    Mov eax ebx
    If D$SavingExtension = '.SYS'
        add eax DRIVERDEFAULT | jmp L1>
    Else_If D$RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
        add eax D$LinkerDllDefault
L1:     push ebx
          ; Comments in 'FillCodeSymbols':
            Mov ebx D$CodeRefPtr, B$ebx 0FF ;, D$ebx+1 edi, B$ebx+5 EOI
            Mov B$ApiDis &TRUE
            add D$CodeRefPtr 1 ;6
          ; The CodeRef thingies are now completed by the 'StoreDis', in order
          ; to assume the Relocations of Api Calls in DLLs.
        pop ebx
    Else
        add eax LINKERDEFAULT
    End_If
    sub eax ecx | Mov D$Dis32 eax

    Mov B$ApiFound &TRUE

    popad
ret
____________________________________________________________________________________________

;;
 Encodage of an instruction is done. At this time, we control that all parameters
 have been translated (they are supposed to be zeroed)? Reg BL is used to jump
 tests over mnemonic (not zeroed):
;;

PointNextInstruction:
    Mov esi D$LineStart, bl 0
L0: lodsb
    cmp al Space | jne L1>
      Mov bl 1 | jmp L0<
L1: cmp bl 1 | jne L1>
      cmp al '0' | jbe L1>
     ;  Call InternalNextControl  ; (for developpement only)
        error D$UnknownParameterPtr
L1: cmp B$esi EOI | ja L0<
    cmp B$esi 0 | je L0<
  ret


; Private developper control of what exactly is wrong (not zeroed):

InternalNextControl:
    Mov esi D$LineStart, ecx 80
L5: lodsb | On al < 32, Mov B$esi-1 '.' | loop L5<
    Mov B$esi 0
    pushad
        Call 'USER32.MessageBoxA' D$H.MainWindow, D$LineStart, ErrorMessageTitle, &MB_SYSTEMMODAL
    popad
ret


Proc IsTrueLocalLabel:
    Argument @Pointer
    Uses ebx

        Mov ebx D@Pointer

        ..If B$ebx+2 = ColonSign
            Mov al B$ebx
            .If al < 'A'

            .Else_If al > 'Z'

            .Else
                Mov al B$ebx+1
                If al < '0'

                Else_If al > '9'

                Else
                    Mov eax &TRUE | ExitP
                End_If
            .End_If
        ..End_If

        Mov eax &FALSE
EndP


; 'DB 0....' encounted:

StoreFlatData:
    push eax
        If al = 'B'
            ; OK

        Else_If al = 'W'
            ; OK
        Else_If al = 'U'
            ; OK
        Else_If al = 'D'
            ; OK
        Else_If al = 'Q'
            ; OK
        Else_If al = 'F'
            ; OK
        Else_If al = 'R'
            ; OK
        Else_If al = 'T'
            ; OK
        Else_If al = 'X'
            ; OK

        Else
            error D$NotAnOpcodePtr
        End_If

    ;  ; We must have a Label at the beginning of Line:
    ;    Mov ebx esi | While B$ebx > EOI | dec ebx | End_While | inc ebx
    ;  ; Cannot be a True Local Label:
    ;    Call IsTrueLocalLabel ebx | On eax = &TRUE, error D$LocalDefLabelPtr
    ;  ; But must be a Label:
    ;    While B$ebx > LowSigns | inc ebx | End_While
    ;    On B$ebx <> ColonSign, error D$WhatIsThisPtr
    pop eax

  ; in: al = Size_Type // esi >>> 'DB ' >>> Go to the first Data after 'DB '
    Mov edi D$CodeListPtr
; StoreDatas StoreOneData
L0: .If al = 'B'

        Call StoreDBcode

    .Else_If al = 'W'
        Call StoreDWcode
    .Else_If al = 'U'
        Call StoreDUcode
    .Else_If al = 'D'
        Call StoreDDcode
    .Else_If al = 'Q'
        Call StoreDQcode
    .Else_If al = 'F'
        error D$NotYetMnemoPtr
        ;Call StoreDFcode
    .Else_If al = 'R'
        error D$NotYetMnemoPtr
        ;Call StoreDRcode
    .Else_If al = 'T'
        error D$NotYetMnemoPtr
        ;Call StoreDTcode
    .Else_If al = 'X'
        error D$NotYetMnemoPtr
        ;Call StoreDXcode
    .Else
        error D$UnknownSizePtr
    .End_If

    dec esi
    If B$esi > EOI
        inc esi | jmp L0<<
    End_If

    Mov D$CodeListPtr edi

  ; Arase the Source Line:
    Mov ebx D$LineStart
    While ebx < esi
        Mov B$ebx 0 | inc ebx
    End_While
    dec esi
ret


GetFlatInteger:
    If W$esi = '00'
        Call TranslateBinary
    Else_If B$esi = '0'
        Call TranslateHexa
    Else_If B$esi < '0'
        error D$UnknownDataPtr
    Else
        Call TranslateDecimal
    End_If
ret


StoreDBcode:
L0: If B$esi = TextSign
      ; Cases of Text:
        inc esi | While B$esi <> TextSign | movsb | End_While | inc esi
        On B$esi > Space, error D$MissingSeparatorPtr
    Else
      ; Cases of Numbers:
        Call GetFlatInteger | On eax > 0FF, error D$OverBytePtr
        stosb
    End_If

    On B$esi+2 = memMarker, ret

    If B$esi+3 = Space
        On B$esi+1 = 'D', ret
    End_If

    On B$esi-1 = Space, jmp L0<
ret


StoreDWcode:
L0: Call GetFlatInteger | On eax > 0FFFF, error D$OverWordPtr
    stosw | cmp B$esi-1 Space | je L0<
ret


StoreDUcode:
    .If B$esi = TextSign
        inc esi
        While B$esi <> TextSign
            movsb | Mov B$edi 0 | inc edi
        End_While
        inc esi
        On B$esi > Space, error D$MissingSeparator
        On B$esi+2 = memMarker, ret
        If B$esi+3 = Space
            On B$esi+1 = 'D', ret
        End_If
    .End_If

    Call GetFlatInteger | On eax > 0FFFF, error D$OverWordPtr

    stosw | cmp B$esi-1 Space | je L0<
ret


StoreDDcode:
L0: Call GetFlatInteger | On edx > 0, error D$OverDWordPtr
    stosd | cmp B$esi-1 Space | je L0<
ret


StoreDQcode:
L0: Call GetFlatInteger
    stosd | Mov D$edi edx | add edi 4 | cmp B$esi-1 Space | je L0<
ret


StoreDFcode:

ret


StoreDRcode:

ret


StoreDTcode:

ret


StoreDXcode:

ret

____________________________________________________________________________________________

[InstructionsCounter: ?]

EncodeLines:
    Mov B$ErrorLevel 0, D$CodeLabelsCounter 0
ReCodeLine:
    Mov esi D$CodeSourceB
    Mov D$StatementsCounter 1, D$InstructionsCounter 0

    move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4
L0: lodsb | cmp al 0 | je L0<
    On B$esi <= EOI,  ret                           ; search for end mark '||'

    Mov D$LineStart esi

    If al = EOI
        inc D$StatementsCounter | add D$StatementsPtr 4
        Mov edi D$IpTablePtr, eax D$CodeListPtr
        sub eax D$CodeOrigine                       ; instruction pointers
        stosd | Mov D$IpTablePtr edi                ; List for Debug
    End_If

    Call ClearParameters | Call SearchLineLabel
      cmp B$esi EOI | jbe L0<                       ; case of Label alone
        inc D$InstructionsCounter

   ; Call SearchApiCall | cmp B$esi EOI | jbe L0<
   ; Now done by 'NewSearchApiName' (when computing 'Integers')

    Call StoreSOP | Call Prefix     ; Is There SOP (Segment Prefix Ovewrite) or prefixes?
                                    ; if yes, coding and striping done by called routines
    cmp B$esi EOI | jbe L0<         ; case of prefix alone


    .If B$esi+1 = memMarker
         Mov al B$esi | add esi 2 | Call StoreFlatData | jmp L0<<
    .Else_If B$esi+2 = Space
        If B$esi = 'D'
            Mov al B$esi+1 | add esi 3 | Call StoreFlatData | jmp L0<<
        End_If
    .End_If


L3: Call ParametersCount            ; > cl = ParametersNumber

    Mov B$LongRelative &TRUE        ; will be turn FALSE when needed for short dis.

    cmp cl 0 | je L3>> ; !!!!!!!!!!!!!!! >>
    If cl = 1
      Call FirstParameterAnalyze
      Mov al B$FirstOperandwbit | Call StoreWbit
    Else_If cl = 2
      Call FirstParameterAnalyze | Call SecondParameterAnalyze
      Call FixInstructionType
    Else_If cl = 3                                            ; (SHLD / SHRD > 3 parameters)
      Call FirstParameterAnalyze | Call SecondParameterAnalyze
      Call ThirdParameterAnalyze | Call FixInstructionType
    Else
      Mov B$errorLevel 0 | error D$TooMuchPtr
    End_If

    On B$LockInstruction = &TRUE, Call CheckLockMem

L3: Call Encode | Mov D$CodeListPtr edi

    On B$SIBinside = &TRUE,  Call storeSIB
    cmp B$DisInside &TRUE | jne L4>
        Call StoreDis | jmp L5>                    ; cases of label / displacement

L4: cmp B$DummyDis &TRUE | jne L5>

        Mov eax 0 | Mov edi D$CodeListPtr | stosd
        add D$CodeListPtr 4

L5: On B$immInside = &TRUE,  Call storeImm

    On B$PossibleFirstImmLabel = &TRUE, Call FirstParameterLabel
        On B$PossibleImmLabel = &TRUE, Call SecondParameterLabel

    If B$mm3Dsuffix <> 0
        Mov al B$mm3Dsuffix, B$mm3Dsuffix 0
        Mov edi D$CodeListPtr | stosb
        inc D$CodeListPtr
    End_If

L6: Call PointNextInstruction       ; Includes a developper Message in case of error
                                    ; cases / searches become a problem.
    jmp L0<<

____________________________________________________________________________________________
____________________________________________________________________________________________

; reading and writing chars.
____________________________________________________________________________________________

Store8cars:
    Mov op5 B$esi+4, op6 B$esi+5, op7 B$esi+6, op8 B$esi+7
Store4cars:
    Mov op1 B$esi, op2 B$esi+1, op3 B$esi+2, op4 B$esi+3
ret


[IfNot | cmp #1, #2 | jne #3]
 ________________________________________________________________________________________

; Called when cc conditions needed (Jcc, MOVcc, ...). esi set by caller:

[tttnBits: B$ ?]

SearchFortttnbits:
     On W$esi = 'NN', add esi 2      ; accept double negation ('jnna' = 'ja')
     Call Store4Cars
     ifnot op2 Space, L2>>
       ifnot op1 'E', L1>
         Mov B$tttnBits 00100 | ret        ; e
L1:    ifnot op1 'Z',  L1>
         Mov B$tttnBits 00100 | ret        ; z
L1:    ifnot op1 'A', L1>
         Mov B$tttnBits 00111 | ret        ; a
L1:    ifnot op1 'G', L1>
         Mov B$tttnBits 001111 | ret       ; g
L1:    ifnot op1 'B', L1>
         Mov B$tttnBits 0010 | ret         ; b
L1:    ifnot op1 'C', L1>
         Mov B$tttnBits 0010 | ret         ; c
L1:    ifnot op1 'L', L1>
         Mov B$tttnBits 001100 | ret       ; l
L1:    ifnot op1 'O', L1>
         Mov B$tttnBits 0000 | ret         ; o
L1:    ifnot op1 'S', L1>
         Mov B$tttnBits 001000 | ret       ; s
L1:    ifnot op1 'P', L1>
         Mov B$tttnBits 001010 | ret       ; p
L1:    BadMnemonic

L2:  ifnot op3 Space, L3>>
       ifnot op1 'N', L1>
         ifnot op2 'E', L2>
           Mov B$tttnBits 00101 | ret    ; ne
L2:      ifnot op2 'Z', L2>
           Mov B$tttnBits 00101 | ret    ; nz
L2:      ifnot op2 'O', L2>
           Mov B$tttnBits 0001 | ret     ; no
L2:      ifnot op2 'B', L2>
           Mov B$tttnBits 0011 | ret     ; nb
L2:      ifnot op2 'C', L2>
           Mov B$tttnBits 0011 | ret     ; nc
L2:      ifnot op2 'L', L2>
           Mov B$tttnBits 001101 | ret   ; nl
L2:      ifnot op2 'A', L2>
           Mov B$tttnBits 00110 | ret    ; na
L2:      ifnot op2 'G', L2>
           Mov B$tttnBits 001110 | ret   ; ng
L2:      ifnot op2 'P', L2>
           Mov B$tttnBits 001011 | ret   ; np
L2:      ifnot op2 'S', L2>
           Mov B$tttnBits 001001 | ret   ; ns
L2:      BadMnemonic
L1:    ifnot op2 'E', L2>
         ifnot op1 'A', L1>
           Mov B$tttnBits 0011 | ret     ; ae
L1:      ifnot op1 'B', L1>
           Mov B$tttnBits 00110 | ret    ; be
L1:      ifnot op1 'P', L1>
           Mov B$tttnBits 001010 | ret   ; pe
L1:      ifnot op1 'G', L1>
           Mov B$tttnBits 001101 | ret   ; ge
L1:      ifnot op1 'L', L1>
           Mov B$tttnBits 001110 | ret   ; le
L1:       BadMnemonic
L2:      ifnot op1 'P', L1>
           ifnot op2 'O', L2>
             Mov B$tttnBits 001011 | ret  ; po
L2:       ; BadMnemonic
L1:      BadMnemonic

L3:   ifnot op1 'N', L9>
        ifnot op3 'E', L9>
          ifnot op4 Space, L9>
            ifnot op2 'G', L2>
              Mov B$tttnBits 001100 | ret  ; nge
L2:         ifnot op2 'B', L2>
              Mov B$tttnBits 00111 | ret   ; nbe
L2:         ifnot op2 'A', L2>
              Mov B$tttnBits 0010 | ret    ; nae
L2:         ifnot op2 'L', L2>
              Mov B$tttnBits 001111 | ret  ; nle
L2:
L9: BadMnemonic
 _______________________________________________________________________________________
;;
 search for prefix(es)
 Only one prefix of each group should be allowed > test addition needed
 (to do later if wanted)

 Segment Override Prefix  (SOP)
 When a xS: encounted, the code value is stored and xS: is stripped by ClearSOP
 esi > CodeSourceB  edi > CodeList by CodeListPtr
;;
 _______________________________________________________________________________________

WriteSOP:
    sub esi 3 | lodsb | Mov ah al | lodsb | On al <> 'S', Error D$NotSegmentPtr

    ifnot op1 'C',  L1>
        Mov al 02E | jmp L9>
L1: ifnot op1 'S', L1>
        Mov al 036 | jmp L9>
L1: ifnot op1 'D', L1>
        Mov al 03E | jmp L9>
L1: ifnot op1 'E', L1>
        Mov al 026 | jmp L9>
L1: ifnot op1 'F', L1>
        Mov al 064 | jmp L9>
L1: On op1 <> 'G', error D$NotSegmentPtr
        Mov al 065
L9: Mov edi D$CodeListPtr | stosb | inc D$CodeListPtr
ret
 __________________________________________________________________________________________
;;
 exemple of ClearSOP job: when called, esi point to  ':'

                  |MOV CS:D$EBX 1|
          new esi>>>>>^  ^<<<<<<< old esi > new edi


 after std, lodsb and stosb esi (new line start) point to the first letter of mnemonic:

                  |MOVMOV D$EBX 1|
                      ^<<<<< edi > last new esi  >>> last new LineStart
;;
 __________________________________________________________________________________________

ClearSOP:                   ; esi > ':' sign  >>>  '.xS:'  (this point is a space)
    push edi
        Mov edi esi | sub esi 3
        std
L0:         lodsb | cmp al EOI | jbe L9>
                    cmp al colonSign | je L9> ; To hold Lines with 'head Labels'.
            stosb | jmp L0<
L9:     cld
        Mov esi edi | inc esi       ; esi > new start of instruction
        Mov D$LineStart esi
    pop edi
ret

StoreSOP:
    push esi
L0:     lodsb | cmp al EOI | jbe L9>
        cmp al ColonSign | jne L0<  ; no possible confusion:
            Call WriteSOP           ; labels have been treated before
            Call ClearSOP
        pop eax                     ; scratch old esi
        push esi                    ; (re)save new esi
L9: pop esi
ret

 _______________________________________________________________________________________
 _______________________________________________________________________________________

Prefix:
    Mov edi D$CodeListPtr,  esi D$LineStart
L0: Call Store8cars
    ifnot op1 'R', L6>
      ifnot op2 'E', L9>>
        ifnot op3 'P', L9>>
          cmp op4 Separators | ja L2>                                   ; REP
            Mov B$edi 0F3 | add esi 4 | inc edi | jmp L0<
L2:       cmp op5 Separators | ja L4>
            cmp op4 'E' | je L3>
            ifnot op4 'Z', L9>
L3:           Mov B$edi 0F3 | add esi 5 | inc edi | jmp L0<             ; REPE/REPZ
L4:       ifnot op4 'N', L9>
            cmp op6 Separators | ja L9>
              cmp op5 'E' | je L5>
              ifnot op5 'Z', L9>
L5:             Mov B$edi 0F2 | add esi 6 | inc edi | jmp L0<           ; REPNE/REPNZ
L6: ifnot op1 'U', L6>
      ifnot op2 'T', L9>
        ifnot op3 'J', L9>
            Mov B$esi+3 '_' | jmp L9>
L6: ifnot op1 'L', L9>
      ifnot op2 'T', L6>
        ifnot op3 'J', L9>
            Mov B$esi+3 '_' | jmp L9>
L6:   ifnot op2 'O', L9>
        ifnot op3 'C', L9>
          ifnot op4 'K' L9>
            cmp op5 Separators | ja L9>
              Call CheckLockInstruction
              Mov B$LockInstruction &TRUE
              Mov B$edi 0F0 | add esi, 5 | inc edi | jmp L0<<            ; LOCK
L9: On esi = D$LineStart,  ret                                 ; nothing done > exit
      On B$esi-1 <= EOI,  dec esi
      Mov D$LineStart esi,  D$CodeListPtr edi
ret


LockInstructionError: Mov B$LockInstruction &FALSE | error D$LockErrorPtr


CheckLockMem:
    Mov B$LockInstruction &FALSE

    If B$FirstGender <> mem
        On B$SecondGender <> mem, error D$LockMemErrorPtr
    End_If
ret

CheckLockInstruction:
;;
  Edi Points to the next Member of the Instruction.
  
  The Instruction can only be ADD, ADC, AND, BTC, BTR, BTS, CMPXCHG, CMPXCH8B, DEC,
  INC, NEG, NOT, OR, SBB, SUB, XADD, XCHG, or XOR
;;

  push esi
  add esi 5
  ; ADD, ADC, AND:
    ...If B$esi = 'A'
        .If B$esi+1 = 'D'
            If B$esi+2 = 'D'           ; ADD
                ;
            Else_If B$esi+2 = 'C'      ; ADC
                ;
            Else
                jmp LockInstructionError
            End_If

            On B$esi+3 <> Space, jmp LockInstructionError

        .Else_If B$esi+1 = 'N'
            If B$esi+2 = 'D'           ; AND
                On B$esi+3 <> Space, jmp LockInstructionError
            Else
                jmp LockInstructionError
            End_If

        .Else
            jmp LockInstructionError
        .End_If

  ; BTC, BTR, BTS:
    ...Else_If B$esi = 'B'
        .If B$esi+1 = 'T'
            If B$esi+2 = 'C'
                ;
            Else_If B$esi+2 = 'R'
                ;
            Else_If B$esi+2 = 'S'
                ;
            Else
                jmp LockInstructionError
            End_If

            On B$esi+3 <> Space, jmp LockInstructionError

        .Else
            jmp LockInstructionError

        .End_If

  ; DEC
    ...Else_If W$esi = 'DE'
        If B$esi+2 = 'C'
            On B$esi+3 <> Space, jmp LockInstructionError
        Else
            jmp LockInstructionError
        End_If

  ; INC, NEG, NOT, OR, SBB
    ...Else_If W$esi = 'IN'
        If B$esi+2 = 'C'
            On B$esi+3 <> Space, jmp LockInstructionError
        Else
            jmp LockInstructionError
        End_If

    ...Else_If W$esi = 'NE'
        If B$esi+2 = 'G'
            On B$esi+3 <> Space, jmp LockInstructionError
        Else
            jmp LockInstructionError
        End_If

    ...Else_If W$esi = 'NO'
        If B$esi+2 = 'T'
            On B$esi+3 <> Space, jmp LockInstructionError
        Else
            jmp LockInstructionError
        End_If

    ...Else_If W$esi = 'OR'
        On B$esi+2 <> Space, jmp LockInstructionError ; jE!

    ...Else_If W$esi = 'SB'
        If B$esi+2 = 'B'
            On B$esi+3 <> Space, jmp LockInstructionError
        Else
            jmp LockInstructionError
        End_If
  ; SUB, XADD, XCHG, or XOR
    ...Else_If W$esi = 'SU'
        If B$esi+2 = 'B'
            On B$esi+3 <> Space, jmp LockInstructionError
        Else
            jmp LockInstructionError
        End_If

    ...Else_If D$esi = 'XADD'
        On B$esi+4 <> Space, jmp LockInstructionError

    ...Else_If D$esi = 'XCHG'
        On B$esi+4 <> Space, jmp LockInstructionError

    ...Else_If W$esi = 'XO'
        If B$esi+2 = 'R'
            On B$esi+3 <> Space, jmp LockInstructionError
        Else
            jmp LockInstructionError
        End_If

  ; CMPXCHG, CMPXCHG8B:
    ...Else_If D$esi = 'CMPX'
        ..If W$esi+4 = 'CH'
            .If B$esi+6 = 'G'
                If B$esi+7 = Space
                    ;
                Else_If W$esi+7 = '8B'
                    On B$esi+9 <> Space, jmp LockInstructionError
                Else
                    jmp LockInstructionError
                End_If
            .Else
                jmp LockInstructionError
            .End_If
        ..Else
            jmp LockInstructionError
        ..End_If

    ...Else
        jmp LockInstructionError

    ...End_If

    pop esi
ret

 _________________________________________________________________________________________
 _________________________________________________________________________________________
;;
 When all encoding job is done, we now fill label evocation in code with label adresses
 stored in LabelList. We creat a .reloc section if wanted (DLLs).
;;

[RelocPage: ?    RelocSectionSize: ?    FileAlignedRelocationSize: ?]
[StartOfCodeSection: 01000  StartOfDataSection: 02000] ; redefined any case after encoding

[LastUpSearchLabel: ?]

SearchLocalLabelUp:                     ; EDI >>> LabelList+3 / EDX >>> end of LabelList
L0: push esi, ecx, ebx                  ; this ESI points to CodeRef <<<<<< ESI >>>>>>
L1:   lodsb                             ; jmp over name in Coderef
      cmp al EOI | ja L1<
        lodsb                           ; strip one '|' ('L1<|..' turn previously 'L1||..')
        On B$LongRelative = &TRUE, lodsb ; idem for long > 'L1>>|'  > 'L1|||'
        lodsd                           ; local label evocation offset in EAX
        and eax 07FFFFFFF   ; 0111111111111111B strip relative flag from offset (if any)
        Mov ecx eax
        lodsd | Mov D$StatementsCounter eax
        Mov esi edi         ; switch ESI to LabelList    <<<<<< ESI >>>>>>
        On D$LastUpSearchLabel > 0, Mov esi D$LastUpSearchLabel
; ESI > start of first name in LabelList. We now search in LabelList a fitting adress
; by comparison with evocation offset stored in ECX: search for a neighbour label
; located after evocation and set EDI to previous one's end

L2:   Mov ebx esi                       ; save start of LabelList name
L3:   lodsb                             ; jmp over name in LabelList
        cmp esi edx | jae L5>           ; reach end of LabelList? (EDX = Max)
        cmp al EOI | ja L3<
        lodsd                           ; read an offset in LabelList
        test B$esi CodeLabelFlag | je L4>
        cmp eax ecx | ja L5>            ; is it about the same offset than the Coderef one?
L4:     lodsb                           ; jump over flag byte
        lodsb                           ; jmp over '|'
      jmp L2<
                                        ;          |PREVIOUSNAME|Dword byte|NAME|
                                        ;                      ^ <<<<<<<<  ^
L5: sub ebx 8 | Mov edi ebx             ; set EDI > end of previous LabelList name
    Mov D$LastUpSearchLabel ebx         ; This Value saves a lot of Search tim!!!
    pop ebx, ecx, esi                   ; ESI back to CodeRef     <<<<<<< SI >>>>>>
    inc esi                             ; For 'L1' label, ESI was at 'L'. Now, ready for
                                                                  ; backward search
    std                                 ; ready to check backward

    align 32

L6: push esi ecx
      repe cmpsb                 ;  cmp ESI (CodeRef) to EDI (LabelList) until different
      je L9>                            ;  > found
      cmp B$edi+1 EOI | jbe L7>         ; case of LabelList name shorter than CodeRef's one
      Mov ecx 0FF,  al EOI | repne scasb  ; longer: LabelList Di ptr > after next '|'
L7:   sub edi 6                           ;          |PREVIOUSNAME|Dword byte|NAME|
                                          ;                      ^ <<<<<<< ^
    pop ecx, esi
    On edi <= D$LabelList, error D$UnknownSymbolPtr esi      ; (we are searching upward)
    jmp L6<

L9: cld                                  ; ESI, EDI > start of identical found label name
    pop ecx esi                          ; ESI > end of name
    On B$LongRelative = &TRUE,  inc esi   ; strip additionnal '|' (old '<')
    add esi 3                            ; ESI > Dword offset of code evocation
    add edi 5                            ; EDI > Dword offset of code or data label
ret


[LastDownSearchLabel: ?]

SearchLocalLabelDown:
L0: push esi, ecx, ebx
L1:   lodsb                             ; jmp over name in Coderef
        cmp al EOI | jne L1<
        lodsb                           ; stip one '|' ('L1>|..' turn previously 'L1||..')
        On B$LongRelative = &TRUE,  lodsb
        lodsd                           ; local label evocation offset in eax
        test eax RelativeFlag | jz L2>
          and eax 00_01111111_11111111_11111111_11111111   ;07FFFFFFF   ; strip flag
L2:   Mov ecx eax
      lodsd | Mov D$StatementsCounter eax | Mov esi edi
      On D$LastDownSearchLabel > 0, Mov esi D$LastDownSearchLabel
L3:   Mov ebx esi                       ; save start of LabelList name
L4:   lodsb                             ; jmp over name in LabelList
        cmp esi edx | jae L7>           ; reach end of LabelList? (EDX = Max)
        cmp al EOI | ja L4<
        lodsd
        test B$esi CodeLabelFlag | je L5>
          cmp eax ecx | jae L6>         ; is it about the same offset?
L5:     lodsb                           ; jump over flag byte
        lodsb                           ; jmp over '|'
      jmp L3<
L6: Mov edi ebx                         ; restore start of LabelList name in DI
    Mov D$LastDownSearchLabel ebx       ; This Value saves lot of search time!!!
    pop ebx ecx esi           ; edi > First label name's letter with possible adress
    Call SearchRegularLabel
    inc esi                             ; strip '>' for short jump
    On B$LongRelative = &TRUE,  inc esi  ; strip next '>' if long jump
ret

L7: pop ebx ecx esi
    error D$UnknownSymbolPtr esi


; when called, ESI > start of a label name in CodeRef, ECX lenght of this name
; EDI set to >  LabelList + 5  by FillCodeSymbols (start of first label name in LabelList)

SearchRegularLabel:
    Mov al EOI
L0: push esi ecx
        repe cmpsb              ;  cmp esi (CodeRef/DataRef) to edi (LabelList) until different
        je L8>                  ;  > found
            cmp B$edi-1 al | jbe L3>      ; case of LabelList name shorter than CodeRef's one
            Mov ecx 0FF | repne scasb     ; longer: LabelList edi ptr > after next '|'
L3:         add edi 6                                 ; |LABELNAME|dword FlagByte|NEXTNAME
                                                    ;        EDI ^ >>>>>>>>     ^
            If edi >= edx                       ; edx = LabelList Max.
                pop ecx, esi
                error D$UnknownSymbolPtr esi
            end_if
    pop ecx, esi | jmp L0<

L8: cmp B$Relative &TRUE | je L9>           ; if comming from 'SearchLocalLabeldown', OK
      test D$esi RelativeFlag | jz L9>           ; if not, possible relative:
        Mov D$Relative &TRUE,  B$LongRelative &TRUE     ; this is for 'JMP label'
L9: pop eax eax             ; if here, found: dummy stack restore. edi > code adress dWord
    ret


SearchSortedRegularLabel:
;;
    dec ecx
    If D$LabelsPointersBySizes+ecx*4 <> 0
        Mov edi D$LabelsPointersBySizes+ecx*4
    Else
        error UnknownSymbol esi
    End_If
    inc ecx | Mov al EOI
    

L0: push esi ecx
        repe cmpsb              ;  cmp esi (CodeRef/DataRef) to edi (LabelList) until different
        je L8>                  ;  > found
            cmp B$edi-1 al | jbe L3>      ; case of LabelList name shorter than CodeRef's one
            Mov ecx 0FF | repne scasb     ; longer: LabelList edi ptr > after next '|'
L3:         add edi 6                             ; |LABELNAME|dword FlagByte|NEXTNAME
                                                  ;        EDI ^ >>>>>>>>     ^
            If edi >= edx                       ; edx = LabelList Max.
                pop ecx, esi
                error UnknownSymbol esi
            End_if
    pop ecx, esi | jmp L0<
;;

L8: Call GetFromQwordCheckSum esi, D$LabelList, D$LabelListLimit

    If eax = 0
        push D$esi
            pushad
                Call IsItAreg | On eax <> 0, error D$UnexpectedRegPtr, esi
            popad
        pop D$esi
        error D$UnknownSymbolPtr, esi
    Else
        While B$eax > LowSigns | inc eax | inc esi | End_While | inc eax | inc esi
        Mov edi eax, ecx 0
    End_If

L8: cmp B$Relative &TRUE | je L9>           ; if comming from 'SearchLocalLabeldown', OK
        test D$esi RelativeFlag | jz L9>           ; if not, possible relative:
            Mov D$Relative &TRUE,  B$LongRelative &TRUE     ; this is for 'JMP label'

L9: ret


; construction of relocation table. called by FillDataSymbols routine when an uncomputed
; adress is found. EDI > code offset of an evocation.

[CodeOrData: ?]

StoreApiReloc: ; NewSearchApiName PreparePeHeader
  ; eat 0FF found at esi, by 'FillCodeSymbols'
    inc esi | Mov edi D$esi | add esi 5
;jE!
StoreReloc: ; 'RelocComments'
    push eax, ebx, ecx, edi
L0:
      Mov eax edi
      sub eax D$CodeOrData | and eax PageMask ;| add eax PageSize
     ; cmp eax D$RelocPage | je L3>
      cmp eax 01000 | jb L3>>
     ; if in the same page of code, just write (>L3). if not:
     ; writing of reloc page header. Each section begins by a header of two Dwords.
     ; first one is the virtual adress; exemple, 01000 if begins at 00401000.
     ; second one is the total number of octets of the section.
      add D$CodeOrData 01000

If D$RelocSectionSize = 8 ; if unused RelocSection, reuse it for Next
 add D$RelocPage PageSize | Mov eax D$RelocPage |
 Mov ecx D$RelocationPtr | Mov D$ecx-8 eax | jmp L0<
End_IF
      push eax, edi
        Mov eax 0 | Mov edi D$RelocationPtr
L1:     test D$RelocationPtr 0011 | jz L2>                   ; align on Dword boudary
          stosw                                              ; fill with zeros
          add D$RelocationPtr 2 | add D$RelocSectionSize 2

L2:     Mov ebx D$RelocationPtr | sub ebx D$RelocSectionSize | add ebx 4
        Mov eax D$RelocSectionSize | Mov D$ebx eax     ; set second dWord to section size

        add D$RelocPage PageSize | Mov eax D$RelocPage | stosd
        add edi 4 | Mov D$RelocationPtr edi | Mov D$RelocSectionSize 8
      pop edi, eax
jmp L0<< ; don't continue, becouse can be over NEXT-PAGE

L3:   Mov eax edi | sub eax D$CodeOrData | and eax 0FFF| add eax 03000  ; or! eax 03000, RelocTypeFlag
      Mov edi D$RelocationPtr | stosw

      add D$RelocSectionSize 2 | Mov D$RelocationPtr edi
    pop edi, ecx, ebx, eax
ret
 ________________________________________________________________________________________
;;
 Filling code label evocations (set to zeros at coding time, fill when coding is over)

 > esi points to label name in CodeRef (fix)
 > edi points to label name in LabelList (moveable)
 ecx = lenght of researched name (count from CodeRef)

 True labels adresses writing in generated code. See full comments down there
 about the tables used in this deal
 esi > CodeRef  ebx = CodeRef End     edi > LabelList  edx = LabelList End
 The job is difficult enough: it may be about:
 uncomputed code or data references,
 computed (+/-) jumps or calls either short (one byte) or long (four bytes)
 Values are turned RVA with 'CodeAjust' an 'DataAjust' set by 'InitRelocation'
;;
 ________________________________________________________________________________________

[BadShortDisplacement: 0 #4]

ErrorShortDisUp:
    neg eax | sub eax 127
    If eax > 1
        Mov ebx {'Bytes Up', 0}
    Else
        Mov ebx {'Byte Up', 0}
    End_If

    jmp L1>

ErrorShortDisDown:
    sub eax 080
    If eax > 1
        Mov ebx {'Bytes Down', 0}
    Else
        Mov ebx {'Byte Down', 0}
    End_If

L1: ;error D$ShortDisPtr

    CustomError D$ShortDisPtr, '#1', 'Int', eax,
                               '#2', 'Str', ebx

    Mov B$ErrorLevel 2 | error TrashString

    ;mov ebx eax | Mov B$ErrorLevel 12 | error D$ShortDisPtr

[ApplyShortenJump: ?]

FillCodeSymbols:
    Mov eax D$CodeRef | add eax 5
    On D$CodeRefPtr = eax,  ret                 ; if no symbol at all (???!!!)
    Mov edx D$LabelList | Mov esi edx | lodsd   ; len of LabelList (-1) > EAX > EDX
    add edx eax | dec edx                       ; last octet of LabelList table (edx=edi max)
    Mov ebx D$CodeRef | Mov esi ebx | lodsd     ; len of CodeRef (-1) > EAX > EBX
    add ebx eax | dec ebx                       ; last octet of CodeRef table (ebx=esi max)
    Mov esi D$CodeRef | add esi 5               ; (+5) > jmp over len and '|'

L0: Mov edi D$LabelList | add edi 5

    Mov B$ApplyShortenJump &FALSE
;;
 For Api calls relocations, the 'EncodeLines' does it directely (> 'NewSearchApiName'): 
 The 'CodeRef' Record for such cases is simply:
 |0FF dWordCodeAddress|. This is done by 'NewSearchApiName' only in case of:
 > If D$SavingExtension = '.DLL'
 Again, The record is irregular: 0FF, 1 dWord, EOI, as there is nothing else to be done
 with this, out of the DLL relocations building:
;;
    If B$esi = 0FF
        Call StoreApiReloc | cmp esi ebx | jb L0<
        ret
    End_If

    push esi
      Mov ecx 0
L1:   lodsb
        inc ecx                                 ; simple lenght counter of Code symbolics'
        cmp al EOI | ja L1<                     ; lenghts, in CodeRef, including separator
        lodsd | lodsd | Mov D$StatementsCounter eax
    pop esi                       ; when encoding 'Relative' is either 0 or high bit flag
    Mov D$Relative &FALSE                        ; now on, either true or false
      Mov ah B$esi,  al B$esi+1
      cmp ah 'A' | jb S9>>
        cmp ah 'Z' | ja S9>>
          cmp al '0' | jb S9>>
            cmp al '9' | ja S9>>                 ; is it a local label evocation?

            .If B$esi+2 = '>'
                Mov D$Relative &TRUE
                If B$esi+3 = '>'
                    Mov B$LongRelative &TRUE
                Else_If B$esi+3 = '.'
                    Mov B$LongRelative &TRUE | Mov B$esi+3 '>'
                    Mov B$ApplyShortenJump &TRUE
                Else_If B$esi+3 < Separators
                    Mov B$LongRelative &FALSE
                Else
                    Error D$WhatIsThisPtr
                End_If

                Mov B$esi+2 EOI | Mov ecx 3 | Call SearchLocalLabelDown

            .Else_If B$esi+2 = '<'
                Mov D$Relative &TRUE
                If B$esi+3 = '<'
                    Mov B$LongRelative &TRUE
                Else_If B$esi+3 = '.'
                    Mov B$LongRelative &TRUE | Mov B$esi+3 '<'
                    Mov B$ApplyShortenJump &TRUE
                Else_If B$esi+3 < Separators
                    Mov B$LongRelative &FALSE
                Else
                    Error D$WhatIsThisPtr
                End_If

                Mov B$esi+2 EOI | Mov ecx 3 | Call SearchLocalLabelUp

            .Else
S9:             push edx
                    Call SearchSortedRegularLabel
                pop edx

            .End_If
;;
  got here after label search up, down or regular (and found)
  edi points to a LabelList'symbol adress (code or data true adress of label)
  esi points to a Coderef evocation adress (code ... adress needing 'fill job')
  'add esi 9', 4 lines down here set this ptr on Coderef next label name (2 dWords + EOI)
;;
    Mov eax D$edi                              ; true label adress in eax (EDI > LabelList)

    On B$ApplyShortenJump = &TRUE, Mov B$LongRelative &FALSE

  ; write 'done' on LabelList flag byte:
    Mov cl B$edi+4 | or B$edi+4 DoneFlag
    Mov edi D$esi | add esi 9
  ; ESI > CodeRef offset of evocation > EDI

    ...If D$Relative = &TRUE
      ; no flag > uncomputed label adress: Strip flag (computed signed displacement);
        and edi 07FFFFFFF | sub eax edi
      ; no signed displacement?
        .If B$LongRelative = &FALSE
            test eax dWordHighBit | jz L6>
          ; -127 (0FFFF_FF80 = -128) ; short negative value:
          ; -128 (-1) (limit for signed negative byte)
            On eax < 0FFFF_FF81,  jmp ErrorShortDisUp
                jmp L7>

L6:         On eax > 080, jmp ErrorShortDisDown    ; short positive value > 07F + 1
                                                    ; +127 (limit for signed positive byte)
L7:         sub eax 1 | add B$edi al | cmp esi ebx | jb L0<<     ; store on one byte
            ret

        .Else
            On B$ProfilerFlag = &TRUE, Call TimingCalls

L8:         sub eax 4 | add D$edi eax | cmp esi ebx | jb L0<<       ; store on 4 bytes
            ret      ; why ADD and not MOV: exemple: 'ADC ebx MyLabel+2' > '2' previously stored
                ; 'sub ax, 4'  is to jump over storage bytes (L7: idem for one byte storage)
        .End_If
    ...End_If

L9: If D$RelocsWanted = &TRUE ;SavingExtension = '.DLL' ; jE!
        Call StoreReloc
    Else_If D$SavingExtension = '.SYS'
        Call StoreReloc
    End_If

    If cl < CodeLabelFlag
        add eax D$DataAjust         ; 2 >>> data / 3 >>> data+done
    Else
        add eax D$CodeAjust         ; 4 >>> code / 5 >>> code+done
    End_If

    add D$edi eax                                   ; store on 4 bytes (uncomputed)
    cmp esi ebx | jb L0<<
ret

 ________________________________________________________________________________________
;
; Filling empty rooms left in Data section (same comments as upper ones)
 ________________________________________________________________________________________

FillDataSymbols:
    Mov eax D$DataRef | add eax 5
    On D$DataRefPtr = eax,  ret                 ; if no symbol at all
   ; Mov edx D$PlainLabelList | Mov esi edx | lodsd   ; len of LabelList (-1) > EAX > EDX

   ; add edx eax | dec edx                       ; last octet of LabelList table (edx=edi max)
    Mov ebx D$DataRef | Mov esi ebx | lodsd     ; len of DataRef (-1) > EAX > EBX
    add ebx eax | dec ebx                       ; last octet of DataRef table (ebx=esi max)
    Mov esi D$DataRef | add esi 5               ; (+5) > jmp over len and '|'

L0: ;mov edi D$PlainLabelList | add edi 5
    push esi
        Mov ecx 0
L1:     lodsb
            inc ecx                                 ; simple lenght counter of Code symbolics'
            cmp al EOI | ja L1<                     ; lenghts, in DataRef, including separator
            lodsd | Mov D$DataFillPtr eax           ; doublon de DataListPtr
            lodsd | Mov D$bracketCounter eax
            lodsb | Mov B$DataSign al
            lodsb | Mov D$DataRefPtr esi            ; strip lasting EOI
    pop esi

    Call SearchSortedRegularLabel | Mov eax D$edi | Mov cl B$edi+4

    cmp cl CodeLabelFlag | jb D1>               ; 'jb' because:
        add eax D$CodeAjust | jmp D2>                 ; 2 > data / 3 > data+done
D1:     add eax D$DataAjust                           ; 4 > code / 5 > code+done
D2:     or B$edi+4 DoneFlag

D2: On B$DataSign = &TRUE, neg eax | Mov edi D$DataFillPtr | add D$edi eax

    If D$RelocsWanted = &TRUE ;SavingExtension = '.DLL' ;jE!
        Call StoreReloc
    Else_If D$SavingExtension = '.SYS'
        Call StoreReloc
    End_If

    Mov esi D$DataRefPtr | cmp esi ebx | jb L0<<
  ret


[DataFillPtr: ?  DataAjust: ?  CodeAjust: ?]

Proc SetCodeAdjust:
    Argument @Base

        Mov eax D@Base | add eax D$uBaseOfData | sub eax D$Datalist
        Mov D$DataAjust eax
        Mov eax D@Base | add eax D$uBaseOfCode | sub eax D$Codelist
        sub eax D$uStartOfCode | Mov D$CodeAjust eax
EndP


InitRelocationForData:
    push edi eax

        If D$SavingExtension = '.SYS'
            Call SetCodeAdjust DRIVERDEFAULT
        Else_If D$SavingExtension = '.DLL'
            Call SetCodeAdjust D$LinkerDllDefault
        Else
            Call SetCodeAdjust LINKERDEFAULT
        End_If

        Mov edi D$RelocationPtr

        Mov eax D$uBaseOfImport | add eax D$ImportTrueSize | Align_On 01000 eax

        If B$NoResources = &FALSE
            add eax D$uRsrcSize | Align_On 01000 eax
        End_If

        Mov D$RelocPage eax | stosd

        Mov eax 0 | stosd
        Mov D$RelocationPtr edi | Mov D$RelocSectionSize 8

    pop eax edi
ret


InitRelocationForCode:
    push edi eax
        Mov edi D$RelocationPtr

        Mov eax D$uBaseOfImport | add eax D$ImportTrueSize | Align_On 01000 eax

        If B$NoResources = &FALSE
            add eax D$uRsrcSize | Align_On 01000 eax
        End_If

        ;add eax D$uDataSize | Align_On 01000 eax
        Mov eax D$uBaseOfCode
        Mov D$RelocPage eax | stosd

        Mov eax 0 | stosd
        Mov D$RelocationPtr edi | Mov D$RelocSectionSize 8
    pop eax edi
ret


CloseRelocation:
    Mov eax 0 | Mov edi D$RelocationPtr

L1: test D$RelocationPtr 0011 | jz L2>              ; align on Dword boudary
        stosb                                       ; fill with zeros
        inc D$RelocationPtr | inc D$RelocSectionSize
        jmp L1<

L2: Mov ebx D$RelocationPtr | sub ebx D$RelocSectionSize | add ebx, 4
    Mov eax D$RelocSectionSize | Mov D$ebx eax      ; set second dword to section size
ret

RelocComments:
;;
 Relocation Section looks like this, for example:

 D$ 02000  ; RVA (Liker Default is to be added to this by the Loader).
 D$ 034    ; Aligned number of Bytes in the coming Chunk (including upper 8 Bytes+Alignement)
 W$ 03004 03010 03058 ....
 ; the chunk is aligned on dWord with zero padding if needed.
 D$ 03000  ; Next chunk header
 ....

 Even the last Chunk must be aligned and the number of bytes in this last Chunk Header
 holds it too.
;;

[BigFirstTick: ?  BickTickCount: ?]

BuildRelocationAndFillSymbols:                      ; >>> StoreReloc <<<
    Call InitRelocationForData
        Mov B$ErrorLevel 3
        move D$CodeOrData D$DataList
        Call BuildPlainLabelList

      ; Left for viewing the List of Labels, the Debugger, ...
       Call SortPlainLabelList

       Call FillDataSymbols

        Call CloseRelocation
        If D$RelocSectionSize = 8                   ; Case of no relocation
            sub D$RelocationPtr 8                   ; wanted in Data
        End_If

    Call InitRelocationForCode
        Mov B$ErrorLevel 2
        Mov D$LastDownSearchLabel 0, D$LastUpSearchLabel 0
        move D$CodeOrData D$DataList
        Mov eax D$uDataSize | Align_On 0200 eax | add D$CodeOrData eax

        Call FillCodeSymbols

        Call CloseRelocation
        If D$RelocSectionSize = 8                   ; Case of no relocation
            sub D$RelocationPtr 8                   ; wanted in CODE jE!
        Mov eax D$RelocationPtr | and D$eax 0 | and D$eax+4 0 ;kill unused
        End_If

        Mov eax D$RelocationPtr | sub eax D$Relocation
        ; Case of no relocation at ALL, make MINIMUM-RELOC
        If eax = 0
           Mov eax D$Relocation | Mov D$eax 0 | Mov D$eax+4 08
           add D$RelocationPtr 8
        End_If
  ; Reuse 'RelocSectionSize' to hold what it says, as a whole for headers values:
    Mov eax D$RelocationPtr | sub eax D$Relocation | Mov D$RelocSectionSize eax
    Align_On 0200 eax | Mov D$FileAlignedRelocationSize eax
ret


SortPlainLabelList:
    Mov eax D$PlainLabelList, eax D$eax

    push eax
        add eax 4
        VirtualAlloc SortTempoMemory eax
    pop ecx
    push ecx
        Mov edi D$SortTempoMemory, esi D$PlainLabelList
        shr ecx 2 | inc ecx | rep movsd

        move D$SortSource D$SortTempoMemory | add D$SortSource 5
        move D$SortDestination D$PlainLabelList | add D$SortDestination 5

        Mov D$SortStringNumber 0 | Mov esi D$SortDestination

L0:     lodsb | cmp al EOI | jne L0<
        add esi 6
        inc D$SortStringNumber
        cmp esi D$EndOfPlainLabelList | jb L0<

    pop ecx

    Call SortLabelStringsBySize

    VirtualFree D$SortTempoMemory
L9: ret


SortLabelStringsBySize:
    Mov edi LabelsPointersBySizes, eax 0, ecx 100 | rep stosd

    Mov edi D$SortDestination, edx 1

L0: Mov esi D$SortSource, ecx D$SortStringNumber, B$SortBySizeOver &TRUE

; There is something wrong in this: If i state 'While B$esi <> EOI' instead of
; 'While B$esi > EOI', it doesn't work. It should, and there is no reason for what
; D$esi could be = 0 (this is to say after the end of the Table -i suppose...)...

L1:     lodsb
        .If al = 0FF        ; Done
            While B$esi > EOI | inc esi | End_While
            add esi 7

        .Else
            push ecx
                Mov eax esi, ecx 1
                While B$eax > EOI | inc ecx | inc eax | End_While
                If ecx = edx
                    Mov al B$esi-1, B$esi-1 0FF
                    On D$LabelsPointersBySizes+edx*4 = 0,
                        Mov D$LabelsPointersBySizes+edx*4 edi
                    stosb
                    dec ecx | jecxz L2>
                        rep movsb
L2:                 movsb  ; |
                    movsd  ; Ptr
                    movsb  ; Flag
                    movsb  ; |

                Else
                    lea esi D$eax+7
                End_If

            pop ecx
            Mov B$SortBySizeOver &FALSE
        .End_If
    loop L1<

    inc edx | cmp B$SortBySizeOver &FALSE | je L0<<
ret

;;
 When searching Labels for filling Data Sybols, we do not need Meaningless Local
 Labels. So, we can build a List without them to make FillDataSymbols faster (No
 use sorting the list, as all Label are supposed to be found. If not error later):
 The 'PlainLabelList' Table is: ...|Name|Ptr Flag|Name....
                                        ...... .  ..
                                        1 4   1  1
;;

[PlainLabelList: ?    EndOfPlainLabelList: ?]

BuildPlainLabelList:
    Mov eax D$LabelList, eax D$eax | inc eax

    VirtualAlloc PlainLabelList eax

    Mov edx D$LabelList | add edx D$edx
    Mov edi D$PlainLabelList | add edi 5
    Mov esi D$LabelList | add esi 5

    .While esi < edx
        cmp B$esi+2 EOI | jne L1>
            cmp B$esi 'A' | jb L1>
            cmp B$esi 'Z' | ja L1>
                cmp B$esi+1 '0' | jb L1>
                cmp B$esi+1 '9' | ja L1>
                    ; |L0|dWord Byte| >>> 9
                    add esi 9 | jmp L2>

L1:     While B$esi <> EOI
            movsb
        End_While
        movsb   ; |
        movsd   ; Ptr
        movsb   ; Flag
        movsb   ; |
L2: .End_While

    Mov eax edi | Mov D$EndOfPlainLabelList eax | sub eax D$PlainLabelList
    Mov edi D$PlainLabelList | stosd | Mov al EOI | stosb
ret

 _______________________________________________________________________________________


FixTableSizes:
    Mov eax D$CodeListPtr | Align_on 0200 eax
    Mov edi D$CodeList | sub eax edi | Mov D$LenOfCode eax

    Mov eax D$CodeRefPtr   | Mov edi D$CodeRef  | sub eax edi  | stosd

    Mov eax D$DataRefPtr   | Mov edi D$DataRef  | sub eax edi  | stosd

   ; Mov eax D$DataListPtr  | Mov edi D$DataList  | sub eax edi | stosd ; no need ???...
    Mov eax D$LabelListPtr | Mov edi D$LabelList | sub eax edi | stosd

    Mov eax D$StartOfCodeSection | add eax D$LenOfCode
    and eax 0FFFFFF00
    add eax 01000 | add eax 0400000
    Mov D$StartOfDataSection eax
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  The Symbolic Names are encoded into a qWord CheckSum. Then, this qWord is encoded
  into one another Word CheckSum. Let's Call them 'CheckSum64' and 'CheckSum16'.
  __________________
  A two Stages Table is declared to hold 010000h (for storing the distributions
  of the Records) plus 010000h Records (for storing the Linked Records in case
  of identical CheckSum16).
  
  The CheckSum16 is used as a Index to point to the Records of the first stage
  of the Table.
  
  _______________
  Each Record is: [CheckSum64, Pointer, LinkedPointer]

  * 'Pointer' points to a Name, in one of the Assembler internal Lists. It is
    nothing but the Pointer transmitted to the 'SetQwordCheckSum' Procedure,
    when called. We do not need to do do any Name String Copy, as it is already
    there, in the concerned List ('LabelList', 'MacroList', 'EquateList').
    
  * If the second half of the Records Table is empty, 'LinkedPointer' points to
    the top of this Second half Table. Otherwise, in case several 'CheckSum64'
    achieve into the same 'CheckSum16', 'LinkedPointer' points to the next same
    'CheckSum16' Record, in the second half of the Table... and so on...
    
  * The first half of the Table is filled 'randomaly-like' (depending on the
    CheckSum16 value. The second half of the Table is filled in order (Top-Down),
    each time a Record of the First Half Table is found to not be empty.
    
  _________
  Routines:
  
  * 'ClearQwordCheckSum' zeroes the Records Table and sets
    'PointerToCheckSumsLinkedRecords' to the Top of the second half Table.
  
  * 'SetQwordCheckSum' is called with a Parameter pointing to the first Char
    of a Name - in its family List - to be recorded. It also Call for:
    
  * 'NoDuplication' to make sure of unique Symbolics Declarations.
  
  * 'GetFromQwordCheckSum' is called with a Parameter pointing to a Name to
     be checked. If found, the Procedure returns the Pointers that was used
     when calling 'SetQwordCheckSum', that are nothing but Pointers to the
     Lists ('MacroList', 'EquateList', 'LabelList'), that hold all infos the
     Assembler needs for doing its job. If not found, it returns zero to the
     caller.
     
  * 'TestRepartition', is just a Dev-test for viewing how all of this goes.
  
  ______________________________
  How the Records Table is used:
  
  When calling 'SetQwordCheckSum', the CheckSum64 is computed from the given
  Name. Then, the CheckSum16 is computed from the given CheckSum64.
  
  CheckSum16 is used as an Indice to the first half of the Records Table.
  For example, if CheckSum16 is 25, we point to the 25th Record, that is
  (CheckSumsRecords + (25*(8+4+4))).
  
  If this record is found empty, it is written: CheckSum64 / Pointer / Link
  
  As long as there is nothing in the second half of the Table, 'Link' points
  to the empty Record at (CheckSumsRecords + (010000h*16) ).
  
  If the first half Table is not found empty, the 'Link' Pointer is read,
  and we jump there,... and so on...
  
  The chances for having two different Symbols achieving into the same CheckSum64
  are, of course, of 1 on 01_0000_0000_0000_0000h, for the second Name, of 1 on
  01_0000_0000_0000_0000h, for the third name, and so on... So, in the very unlikely
  coming out cases when two different Names are computed into the same CheckSum64,
  the new record is linked downward, the same way the Duplications of the CheckSum16
  are Linked.
  
  The reverse case is also theorically possible: For example, you do _not_ implement,
  say, some 'GetPointer' Macro, and you use it in a Statement. It is theorically
  not impossible that some other _declared_ Macro achieves into the same CheckSum64
  as would your non existing 'GetPointer'. In such -very unlikely coming out cases-,
  the 'GetFromQwordCheckSum' would return a pointer, instead of zero. So, when a
  valid CheckSum64 is found, 'GetFromQwordCheckSum' also calls for 'CompareSymbols',
  to make it 100% secure.
;;
____________________________________________________________________________________________

; CheckSum64 / Pointer / Link

[CheckSumsRecords: ? ? ? ? #010000    CheckSumsLinkedRecords: ? ? ? ? #020000
 PointerToCheckSumsLinkedRecords: ?   CheckSumsEnd: ]

ClearQwordCheckSum:
    Mov edi CheckSumsRecords, eax 0
    Mov ecx CheckSumsEnd | sub ecx CheckSumsRecords | shr ecx 2
    rep stosd

    Mov D$PointerToCheckSumsLinkedRecords CheckSumsLinkedRecords
ret

[CheckSumImage: ?]

SaveCheckSumTable:
    pushad
        Mov ecx PointerToCheckSumsLinkedRecords | sub ecx CheckSumsRecords

        push ecx
            VirtualAlloc CheckSumImage, ecx
        pop ecx

        shr ecx 2

        Mov esi CheckSumsRecords, edi D$CheckSumImage | rep movsd
    popad
ret

RestoreCheckSumTable:
    pushad
        Mov ecx PointerToCheckSumsLinkedRecords | sub ecx CheckSumsRecords | shr ecx 2

        Mov esi D$CheckSumImage, edi CheckSumsRecords | rep movsd

        VirtualFree D$CheckSumImage
    popad
ret

____________________________________________________________________________________________

CheckSum64:
  ; esi -> Name
    Mov eax 0, ebx 0, ecx 0

    While B$esi > ' ' ;LowSigns
        rol eax 1 | lodsb | mul eax | xor ebx edx | inc ecx
    End_While
    add ebx ecx
    If eax = 0
        On ebx = 0, Mov eax 1
    End_If
  ; ebx:eax = CheckSum64 // ecx = Length
ret


CheckSum16:
  ; ebx:eax = CheckSum64 (not modified here))
    Mov ecx eax | xor ecx ebx | Mov edx ecx
    rol edx 16 | xor cx dx
    and ecx 0FFFF | shl ecx 4
  ; ecx = CheckSum16, to be used as a Displacement to the matching Record
  ; (To 'CheckSumsRecords' first half part, 16 Bytes per Record)
ret
____________________________________________________________________________________________

Proc SetQwordCheckSum:
    Argument @Pointer

    pushad

        Mov esi D@Pointer

        If B$esi < '0'
            ;
        Else_If B$esi <= '9'
            error D$NumerAsSymbolPtr, D@Pointer
        End_If

        Call CheckSum64 | Call NoDuplication D@Pointer | Call CheckSum16

      ; The List Pointer is used to test empty Records (Lists Pointers can never be zero):
        .If D$CheckSumsRecords+ecx = 0
            On D$CheckSumsRecords+ecx+4 <> 0, jmp L1>
            Mov D$CheckSumsRecords+ecx eax
            Mov D$CheckSumsRecords+ecx+4 ebx
            move D$CheckSumsRecords+ecx+8 D@Pointer
          ; D$CheckSumsRecords+ecx+12 = 0
        .Else
L1:         If D$CheckSumsRecords+ecx+12 = 0
                move D$CheckSumsRecords+ecx+12 D$PointerToCheckSumsLinkedRecords
            Else
                Mov edi D$CheckSumsRecords+ecx+12
                While D$edi+12 <> 0 | Mov edi D$edi+12 | End_While
                move D$edi+12 D$PointerToCheckSumsLinkedRecords
            End_If

            Mov edi D$PointerToCheckSumsLinkedRecords
            Mov D$edi eax
            Mov D$edi+4 ebx
            move D$edi+8 D@Pointer
            ;mov eax D$PointerToCheckSumsLinkedRecords | add eax 16
          ; D$edi+12 = 0
            ;mov D$PointerToCheckSumsLinkedRecords eax
            add D$PointerToCheckSumsLinkedRecords 16
        .End_If

    popad
EndP
____________________________________________________________________________________________

Proc GetFromQwordCheckSum:
    Argument @Pointer, @List, @Limit
    Uses esi, edi, ebx, ecx, edx

        Mov esi D@Pointer

        Call CheckSum64 | Call CheckSum16

        lea esi D$CheckSumsRecords+ecx | Mov ecx &TRUE

L0:     ..If D$esi = eax
            .If D$esi+4 = ebx
                Mov eax D$esi+8
                Mov ebx D@List | On eax < ebx, Mov ecx &FALSE
                Mov ebx D@Limit | On eax > ebx, Mov ecx &FALSE

                On ecx = &TRUE, Call CompareSymbols D@Pointer, eax

                If ecx = &FALSE
                    push esi
                        Mov esi D@Pointer | Call CheckSum64
                    pop esi

                    Mov ecx &TRUE | jmp L2>
                End_If

            .Else
                jmp L2>

            .End_If

        ..Else
L2:
            ;inc D$EquatesPasses
            Mov esi D$esi+12 | cmp esi 0 | je L3>

          ; If no List Pointer, this is the first empty Record in the Linked Record Table:
            Mov ecx &TRUE | cmp D$esi+8 0 | ja L0<<

L3:         Mov eax 0

        ..End_If
   ; hexprint D$EquatesPasses
EndP
____________________________________________________________________________________________



Proc NoDuplication:
    Argument @Pointer
    Uses eax, ebx, ecx

      ; ebx:eax = CheckSum64
        Mov ecx eax | xor ecx ebx | Mov edx ecx
        rol edx 16 | xor cx dx
        and ecx 0FFFF | shl ecx 4

        lea esi D$CheckSumsRecords+ecx

L0:     While D$esi+4 <> 0
            If D$esi = eax
                On D$esi+4 = ebx, Call CompareSymbols D@Pointer, D$esi+8
                On ecx = &TRUE, error D$SymbolDupPtr
            End_If

            Mov esi D$esi+12 | On esi = 0, ExitP

        End_While
EndP
____________________________________________________________________________________________

Proc CompareSymbols:
    Argument @Source, @Destination
    Uses eax, esi, edi

        Mov esi D@Source, edi D@Destination

L0:     lodsb | cmp al LowSigns | jb L5>
        inc edi | cmp al B$edi-1 | je L0<

        Mov ecx &FALSE | ExitP

L5:     If B$edi < LowSigns
            Mov ecx &TRUE
        Else
            Mov ecx &FALSE
        End_If
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  This test is for viewing the Records distribution, in the CheckSums Table.
  
  The first Pixels square shows the occupied Records in the first half Part of
  the Table (black Pixels). The occupied Records, in the second half of the
  Table are represented by red Pixels. They, of course, come in the form of
  a red _line_.
  
  The actual distribution seems to be pretty close to a good random one.
;;

[CheckSumsPixelsCount: ?     CheckSumsLinkedRecordsPixels: ?]
[SecondTable: ?]

TestRepartition:
    Mov D$CheckSumsPixelsCount 0, D$CheckSumsLinkedRecordsPixels 0

    Call 'User32.BeginPaint' D$EditWindowHandle, PAINTSTRUCT | Mov D$hdc eax
    Call 'User32.GetClientRect' D$EditWindowHandle, RECT

    Mov esi CheckSumsRecords

L0: Mov eax D$esi | or eax D$esi+4
    If eax <> 0
        Mov eax esi | sub eax CheckSumsRecords | shr eax 4
        Mov ebx eax | shr eax 8 | and ebx 0FF
        inc D$CheckSumsPixelsCount
        Call 'GDI32.SetPixel' D$hdc, eax, ebx, 0
    End_If

    add esi 16 | cmp esi CheckSumsLinkedRecords | jb L0<

L0: Mov eax D$esi | or eax D$esi+4
    If eax <> 0
        Mov eax esi | sub eax CheckSumsLinkedRecords | shr eax 4
        Mov ebx eax | shr eax 8 | and ebx 0FF | add eax 0100
        inc D$CheckSumsPixelsCount
        inc D$CheckSumsLinkedRecordsPixels
        Call 'GDI32.SetPixel' D$hdc, ebx, eax, 0FF
    End_If

    add esi 16 | cmp esi PointerToCheckSumsLinkedRecords | jb L0<

    Call 'USER32.ReleaseDC' D$EditWindowHandle, D$hdc
    Call 'USER32.EndPaint' D$EditWindowHandle, PAINTSTRUCT

    If D$CheckSumsPixelsCount > 0
       Call WaitForUserAction

      ; Comment out to have the total number of Pixel and how many Linked Records:
      ; (On RosAsm, for V.1.24e: 64,536 // 313)

      ; Hexprint D$CheckSumsPixelsCount
      ; hexprint D$CheckSumsLinkedRecordsPixels

;;
  __________________________
  Results on RosAsm V.1.25d:
  
  Total = 019D9 = 6617
  Links =  0139 =  313

  Total of available Records in the Table first half = 010000 = 65536

  6617 / 65536 = 10.09 % of the Table is occupied by Records(+Linked Records)

  313 / 6617 = 4.73 % of the Records require a linkage
  
  In short: 5% of the Records are Linked when 10% of the table first half is occupied
;;

    Else
        Call 'USER32.MessageBoxA' 0,
      {"The Symoblics'CheckSums'Table can be viewed only after a Compilation.", 0},
      {'Nothing to show', 0}, &MB_OK

    End_If
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
  Reuse of the CkeckSum64 Method for computing the Win32 Equates:
  
  The CheckSum64 Method is so fast, that it is now useless to save the computed
  Tables, 'Equates.nam' and 'Equates.num', like we did, before V.2.015c.
  
  So, each time RosAsm is started, it now loads the 'Equates.equ' File, and reuse
  the CheckSum64 Tables, to rebuild the search Table.
  
  The Records are used in different menner. Instead of "CheckSum64 / Pointer / Link",
  the 'CheckSumsRecords' for the Win32 Equates, are, "CheckSum64 / Value / Link",
  so that, - as the integrity is verified before usage of the Equates.equ File -,
  we can retrieve the Value immidiately from the CheckSum64 Table.
  
  Another difference is that the normal 'CheckSumsRecords' is Static, for the Symbols
  Jobs for the Assembler. We use it, as is, for building the Win32 Equates Table, but,
  once done, we copy this Table to a Dynamic Memory Chunk.
  
  'NewBuildWin32Equates':
  
      Build the Win32 Equates Table and saves it in 'NewWinEquatesMem'. This Routine
      includes two commented out calls to verify the conformity of the List:
    
      'VerifyEquatesFileConformity' and 'NoDuplicationOfEquates'
    
  'NewSearchForWinEquate' and 'NewGetEquates' are for retrieving the Value from
  an &EQUATE_NAME, from the Assembler ('ReplaceWin32Equates') and/or from 'RightClick'
  
  At Start-Up, you can see the Win32 Equates Table with
  [Tools] / [RosAsm Devs Tests] / [Show Symbols Repartition]
  
  Depending on the Processor, the speed improvement is between 1.5 and 5 %, compared
  to the previous Method, on an Auto-Compilation of RosAsm. For the Table built, it
  is many times faster (now, one second, on my old 95 Box with a generic Pentium, and
  the Click time on my Celeron 1.3).
;;
____________________________________________________________________________________________
____________________________________________________________________________________________


[NewWinEquatesMem: ?   dWordsLenOfEquatesList: ?]

; Called from 'Main', each time RosAsm is started:

NewBuildWin32Equates: ; 'CheckSumsRecords' Call OpenEquFiles
    Call OpenEquFiles | On B$IncludesOK = &FALSE, ret

    ;Call VerifyEquatesFileConformity

    Call CountEquates ; >>> 'NumberOfEquates'
    ;hexprint D$NumberOfEquates
    push D$NumberOfEquates

        Mov esi D$EquateIncMemory

        Mov D$PointerToCheckSumsLinkedRecords CheckSumsLinkedRecords

L0:     Mov ebx esi | While B$ebx > ' ' | inc ebx | End_While | Mov B$ebx 0

        Call CheckSum64 | Call CheckSum16
    ; ebx:eax = CheckSum64 // ecx = CheckSum16

    ; Get the Hexa Value into edx:
        push eax
            inc esi  ; skip over the space
            Mov edx 0
L1:         shl edx 4 | Mov al B$esi | sub al '0' | On al > 9, sub al 7
            or dl al
            inc esi | cmp B$esi CR | ja L1<
            add esi 2
        pop eax

        .If D$CheckSumsRecords+ecx = 0
            On D$CheckSumsRecords+ecx+4 <> 0, jmp L1>
            Mov D$CheckSumsRecords+ecx eax
            Mov D$CheckSumsRecords+ecx+4 ebx
            move D$CheckSumsRecords+ecx+8 edx
        .Else
L1:         If D$CheckSumsRecords+ecx+12 = 0
                move D$CheckSumsRecords+ecx+12 D$PointerToCheckSumsLinkedRecords
            Else
                Mov edi D$CheckSumsRecords+ecx+12
                While D$edi+12 <> 0
                    ;Call NoDuplicationOfEquates
                    Mov edi D$edi+12
                End_While
                ;Call NoDuplicationOfEquates
                move D$edi+12 D$PointerToCheckSumsLinkedRecords
            End_If

            Mov edi D$PointerToCheckSumsLinkedRecords
            Mov D$edi eax
            Mov D$edi+4 ebx
            move D$edi+8 edx
            add D$PointerToCheckSumsLinkedRecords 16
        .End_If

        dec D$NumberOfEquates | cmp D$NumberOfEquates 0 | jne L0<<

    pop D$NumberOfEquates

  ; Now, store the Win32 Equates CheckSums Table into Memory:

    Mov ecx D$PointerToCheckSumsLinkedRecords | add ecx 010 | sub ecx CheckSumsRecords

    push ecx
        VirtualAlloc NewWinEquatesMem, ecx
    pop ecx

    Mov esi CheckSumsRecords, edi D$NewWinEquatesMem | shr ecx 2
    Mov D$dWordsLenOfEquatesList ecx | rep movsd
;;
  Adjust all of the Linked Records Pointer (above, they are pointing for a Base
  of 'CheckSumsRecords'. Now, the Base must be inside 'NewWinEquatesMem'
;;
  ; Adjustement Value:
    Mov eax CheckSumsRecords | sub eax D$NewWinEquatesMem

    Mov ebx CheckSumsRecords | sub ebx eax
  ; edi still points to the End of the fresh copied 'NewWinEquatesMem':
    While ebx < edi
        On D$ebx+12 <> 0, sub D$ebx+12 eax
        add ebx 16
    End_While

    ;Call ClearQwordCheckSum
ret
____________________________________________________________________________________________

VerifyEquatesFileConformity:
     Mov esi D$EquateIncMemory, edx esi | add edx D$EquatesIncFileSize

     .While esi < edx
L0:   ; Read one Symbol;
        If W$esi = '__'
            jmp BadEquateLine
        Else_If B$esi = '_'
            ; Good
        Else_If B$esi < '0'
            jmp BadEquateLine
        Else_If B$esi > 'Z'
            jmp BadEquateLine
        End_If
        inc esi | cmp B$esi ' ' | ja L0<

      ; No trailing '_':
        On B$esi-1 = '_', jmp BadEquateLine

      ; One Space, followed by one single '0':
        inc esi
        On B$esi <> '0', jmp BadEquateLine
        On B$esi+1 = '0', jmp BadEquateLine

      ; One Hexa Value:
        While B$esi > ' '
            If B$esi < '0'
                jmp BadEquateLine
            Else_If B$esi > 'F'
                jmp BadEquateLine
            End_If

            inc esi
        End_While

      ; CRLF
        On W$esi <> CRLF, jmp BadEquateLine

        add esi 2
    .End_While
ret


[BadEquateLineMessage: "Bad Equate line in Equates.equ: 

"
 BadEquateLineLine: '                                                        ' ]

BadEquateLine:
    While B$esi > CR
        inc esi | On esi = edx, jmp L0>
    End_While

    Mov D$esi 0

    dec esi

    While B$esi > LF
        dec esi | On esi = D$EquateIncMemory, jmp L0>
    End_While

L0: Mov edi BadEquateLineLine
    While B$esi <> 0 | movsb | End_While | Mov B$edi 0

    showme BadEquateLineMessage
ret

____________________________________________________________________________________________

[DuplicatedEquate: 'Duplicated Equates', 0]

NoDuplicationOfEquates:
    If D$edi = eax
        On D$edi+4 = ebx, Call ShowDumplicatedEquates
    End_If
ret

[CheckSumEax: ?   CheckSumEbx: ?]

ShowDumplicatedEquates:
    pushad
        Mov D$CheckSumEax eax, D$CheckSumEbx ebx
        Mov esi D$EquateIncMemory

L0:     Mov ebx esi | While B$ebx > ' ' | inc ebx | End_While | Mov B$ebx 0

        push esi
            Call CheckSum64
        pop edx

        .If eax = D$CheckSumEax
            If ebx = D$CheckSumEbx
                Mov edi TrashString
                While B$edx <> 0 | Mov al B$edx, B$edi al | inc edx | inc edi | End_While
                Mov D$edi ' == ' | add edi 4
                jmp L0>
            End_If
        .End_If
        While B$esi > CR | inc esi | End_While
        add esi 2 | jmp L0<

L0:     Mov ebx esi | While B$ebx > ' ' | inc ebx | End_While | Mov B$ebx 0

        push esi
            Call CheckSum64
        pop edx

        .If eax = D$CheckSumEax
            If ebx = D$CheckSumEbx
                While B$edx <> 0 | Mov al B$edx, B$edi al | inc edx | inc edi | End_While
                Mov B$edi 0
                jmp L0>
            End_If
        .End_If
        While B$esi > CR | inc esi | End_While
        add esi 2 | jmp L0<

L0: showme TrashString
    popad
ret
____________________________________________________________________________________________

[EquateFound: ?]

NewSearchForWinEquate:
    push esi
        Call NewGetEquates

        If B$EquateFound = &FALSE
            Mov esi TrashString | error D$BadWinEquPtr ; error8
        End_If

        or D$imm32 eax
    pop esi

    add esi D$NewWinEquateLenght

    .If B$esi = addSign
        On B$esi+1 <> '&', ret
        add esi 2 | jmp NewSearchForWinEquate
    .Else_If B$esi = '&'
        inc esi | jmp NewSearchForWinEquate
    .Else
        While B$esi = '_'
            inc esi
        End_While
        If B$esi = '&'
            inc esi | jmp NewSearchForWinEquate
        End_If
    .End_If
ret


[NewWinEquateLenght: ?]

; Called from the Assembler and from 'RightClick'

NewGetEquates: ;New GetEquates: RightClick
    Mov edi TrashString
    While B$esi > ' '
        Mov ax W$esi
        If al = '+'
            jmp L1>
        Else_If al = '-'
            jmp L1>
        Else_If ax = '__'
            jmp L1>
        Else_If al = ','
            jmp L1>
        Else_If al = ';'
            jmp L1>
        Else_If ax = '_&'
            jmp L1>
        Else_If al = '&'
            jmp L1>
        Else_If al = ']'
            jmp L1>
        Else_If al = ')'
            jmp L1>
        Else
            On al >= 'a', sub al 32
            Mov B$edi al | inc esi | inc edi
        End_If
    End_While

L1: Mov B$edi 0 | sub edi TrashString | Mov D$NewWinEquateLenght edi

  ; Simplified 'GetFromQwordCheckSum':
    Mov esi TrashString

ReadWin32Equate:
    Call CheckSum64 | Call CheckSum16

    Mov esi D$NewWinEquatesMem | add esi ecx

L0: .If D$esi = eax
        If D$esi+4 = ebx
            Mov eax D$esi+8, B$EquateFound &TRUE | ret
        End_If
    .End_If

    Mov esi D$esi+12 | cmp esi 0 | jne L0<

    Mov B$EquateFound &FALSE
ret




