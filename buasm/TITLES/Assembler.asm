TITLE Assembler       ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

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

[LinkerDllDefault: D$ 0_1000_0000]

[FL.RelocsWanted: D$ ?]
; some basic equates:
                        ; As concerned tables have 3 dwords per pointed resource
                        ; Allow 100
[MAXDIALOG      1200    ; Dialogs
 MAXMENU        1200    ; Menus
 MAXBITMAP      1200    ; BitMaps
 MAXSTRINGS     1200    ; ...
 MAXFONT        1200
 MAXWAVE        1200
 MAXAVI         1200
 MAXRCDATA      1200
 MAXCURSOR      1200
 MAXICON        1200

 MAXRESOURCE    3500    ; Allow, as a whole, 1000 different resources in one PE

 LINKERDEFAULT  0_40_0000
 DRIVERDEFAULT  0_1_0000

 PAGE_SIZE      0_1000
 PAGE_MASK      0_FFFF_F000
 DWORD_HIGH_BIT 0_8000_0000

 ; For Encoding of reg field
 REG_EAX    00_0000
 REG_AX     00_0000
 REG_AL     00_0000
 REG_ECX    00_0001
 REG_CX     00_0001
 REG_CL     00_0001
 REG_EDX    00_0010
 REG_DX     00_0010
 REG_DL     00_0010
 REG_EBX    00_0011
 REG_BX     00_0011
 REG_BL     00_0011
 REG_ESP    00_0100
 REG_SP     00_0100
 REG_AH     00_0100
 REG_EBP    00_0101
 REG_BP     00_0101
 REG_CH     00_0101
 REG_ESI    00_0110
 REG_SI     00_0110
 REG_DH     00_0110
 REG_EDI    00_0111
 REG_DI     00_0111
 REG_BH     00_0111

 ; For Encoding of the Segment Register (sreg) Field
 REG_ES     00_00_0000
 REG_CS     00_00_1000
 REG_SS     00_01_0000
 REG_DS     00_01_1000
 REG_FS     00_10_0000
 REG_GS     00_10_1000

 ; For Encoding of Special-Purpose Register (eee) Field
 ; (Control Registers and Debug Registers cannot be used in applications. There need
 ; privilege 0 and are for system - not implemented in 'Encode:' Mov to/from Debug/Control
 ; registers))
 REG_CR0    00_0000
 REG_CR2    00_0010
 REG_CR3    00_0011
 REG_CR4    00_0100
 REG_DR0    00_0000
 REG_DR1    00_0001
 REG_DR2    00_0010
 REG_DR3    00_0011
 REG_DR6    00_0110
 REG_DR7    00_0111

 ; For encoding of FPU regs
 REG_ST0 0
 REG_ST1 1
 REG_ST2 2
 REG_ST3 3
 REG_ST4 4
 REG_ST5 5
 REG_ST6 6
 REG_ST7 7

 ; For encoding of MMX registers
 REG_MM0 0
 REG_MM1 1
 REG_MM2 2
 REG_MM3 3
 REG_MM4 4
 REG_MM5 5
 REG_MM6 6
 REG_MM7 7

 ; For Encoding of Operand Size (w) Bit
 BYTE_SIZE  00_00000000  ; True wBit, when encoding, can only be 0 (byte size) or
 WORD_SIZE  00_00000011  ; 1 (full size). Word / Double size discrimination
 DWORD_SIZE 00_00000001  ; Will be done by Operand-size override (066 if bit 2 set)
 QWORD_SIZE 00_00000100  ; FPU
 TBYTE_SIZE 00_00001000  ; FPU
; BCD_SIZE   00_00001111  ; Packed BCD
; FPU_SIZE   00_00001010  ; 108 bytes of FPU image > now X$ !!!!!
 OWORD_SIZE 00_00010000  ; XMM memories
 USO_SIZE   00_11111111  ; Fitting with specific Unregular Size Opcodes

;;
 
    Following are to fill 'Operands', which is a global image of what operands in a line.
    In some unregular cases, AL, AX and EAX may have specific encodage we can't' guess
    while analysing parameters. having symbols in order allow tests like:
    cmp Operands, REG_TO_MEM | ja >L1    instead of:
    cmp operands, REG_TO_MEM | je >L1    which solves this problem in case of no use of
    possible Areg by an instruction encodage.

;;

 REG_TO_REG  1
 MEM_TO_REG  2
 REG_TO_MEM  3
 IMM_TO_REG  4
 IMM_TO_MEM  5
 REG_TO_IMM  6    ; OUT imm8 accum
 IMM_TO_IMM  7    ; ENTER and OUT
 ;  MEM_TO_MEM  8  ; for FPU ???

 GENERAL_REG  1   ; General registers
 SEGMENT_REG  2   ; Segment register
 CONTROL_REG  3   ; Control registers
 DEBUG_REG    4   ; Debug registers
 IMM_VALUE    5   ; Immediate values
 MEM_LABEL    6   ; Memory symbolic adresses (.$ Labels)
 DISPLACEMENT 7
 REG_FPU      8   ; FPU regs
 REG_MMX      9   ; MMX regs
 REG_XMM     10   ; XMM regs


 BYTE_MEM    'B' ; Byte sized memory
 UNICODE_MEM 'U' ; Unicode string
 WORD_MEM    'W' ; Word size memory
 DWORD_MEM   'D' ; Dword size memory
 QWORD_MEM   'Q' ; Qword bytes size memory
 REEL_MEM    'R' ; Reel 8 bytes size for FPU real numbers
 TBYTE_MEM   'T' ; Ten bytes size  for FPU real numbers
 REEL2_MEM   'H' ; Height bytes size for FPU real numbers (to keep a while....)
 FLOAT_MEM   'F' ; Four bytes size for FPU real numbers
 OWORD_MEM   'O' ; OctoWords XMM sizes
 XWORD_MEM   'X' ; Weird and XMM sizes

 ; Where to jump:
 JMP_DOWN_LONG  00_1000 ; ja L9>>
 JMP_DOWN_SHORT 00_0100 ; jmp L7>
 JMP_UP_LONG    00_0010 ; jz L0<<
 JMP_UP_SHORT   00_0001 ; loop L5<

]
;;
 ; Labels flags:
 FLAG_CODE_LABEL 00_0100
 FLAG_DATA_LABEL 00_0010
 FLAG_DONE       00_0001]
;;


; Labels flags:

[FLAG_CODE_LABEL 00_0100
 FLAG_DATA_LABEL 00_0010
 FLAG_DONE      00_0001]

____________________________________________________________________________________________
____________________________________________________________________________________________

; Main asm memories:

[AsmTablesLength: D$ ?]

GetAsmTables:
    ;VirtualFree D$NewWinEquatesMem

    Call VirtualFree IpTable

    Call VirtualFree StatementsTable

    Call VirtualFree StatementsTable2

    Call VirtualFree PlainLabelList

    Call VirtualFree EquateList

    Call VirtualFree MacroData

    Call GetResourcesSize

    add eax D$SourceLen | add eax 1_000_000 | add eax D$MemReservation

    Mov D$AsmTablesLength eax

    Call VirtualAlloc CodeSourceA,
                      eax

    add D$CodeSourceA 010

    ;Mov eax D$SourceLen | add eax 1_000_000 | add eax D$MemReservation
    Call VirtualAlloc CodeSourceB,
                      D$AsmTablesLength

    add D$CodeSourceB 010

ret


GetResourcesSize:

    xor eax eax

    Mov edx CursorList | Call AddResourcesSyze

    Mov edx GroupCursorList | Call AddResourcesSyze

    Mov edx IconList | Call AddResourcesSyze

    Mov edx MenuList | Call AddResourcesSyze

    Mov edx DialogList | Call AddResourcesSyze

    Mov edx StringsList | Call AddResourcesSyze

    Mov edx GroupIconList | Call AddResourcesSyze

    Mov edx WaveList | Call AddResourcesSyze

    Mov edx AviList | Call AddResourcesSyze

    Mov edx RCdataList | Call AddResourcesSyze

    Mov edx BitMapList | Call AddResourcesSyze

ret

AddResourcesSyze:

    While D$edx <> &NULL

        add eax D$edx+MEM_MOTHER_LP

        add edx MEM_RECORD

    End_While

ret


; clear memory (CodeList now is a reuse of CodeSourceB):

ReleaseAsmTables:

    Call VirtualFree CodeSourceA

    Call VirtualFree CodeSourceB

    Call VirtualFree LabelList

    Call VirtualFree MacroList

    Call VirtualFree CodeRef

    Call VirtualFree DataRef

    Call VirtualFree Relocation

    Call VirtualFree ApiListA

    Call VirtualFree ApiListB

    Call VirtualFree DllList

    Mov D$CookedErrorMessage 0

    If B$ProfilerFlag = &TRUE

        Call ReleaseProfilerTables

        Mov B$ProfilerFlag &FALSE

    End_If

ret


ReleaseMainFile:

    Call VirtualFree UserPeStart

    Mov D$CodeSource &NULL

ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; Files access:
____________________________________________________________________________________________
____________________________________________________________________________________________

[STRUC.OPENFILENAME:
 @lStructSize: D$ len
 @hwndOwner: D$ 0
 @hInstance: D$ 0
 @lpstrFilter: D$ PEFilesFilters
 @lpstrCustomFilter: D$ uFileFilter
 @nMaxCustFilter: D$ &MAX_PATH
 @nFilterIndex: D$ 1
 @lpstrFile: D$ SaveFilter
 @nMaxFile: D$ &MAX_PATH
 @lpstrFileTitle: D$ ChoosenFile
 @nMaxFileTitle: D$ &MAX_PATH
 @lpstrInitialDir: D$ &NULL
 @lpstrTitle: D$ 0 ;OpenPEFileTitle
 @Flags: D$ &OFN_LONGNAMES+&OFN_EXPLORER+&OFN_FILEMUSTEXIST+&OFN_PATHMUSTEXIST+&OFN_HIDEREADONLY
 @nFileOffset: W$ 0
 @nFileExtension: W$ 0
 @lpstrDefExt: D$ 0
 @lCustData: D$ 0
 @lpfnHook: D$ 0
 @lpTemplateName: D$ 0]
; @pvReserved: D$ 0
; @dwReserved: D$ 0
; @FlagsEx: D$ 0]


[PEFilesFilters:
 B$ 'Files (*.exe, *.dll, *.wll, *.ocx, *.drv, *.bpl, *.cpl, *.fon, *.mpd, *.vbx, *.vxd, *.sys)' EOS
 B$ '*.exe;*.dll;*.wll;*.ocx;*.drv;*.bpl;*.cpl;*.fon;*.mpd;*.vbx;*.vxd;*.sys' EOS
 B$ 'Executable Files (*.exe)' EOS
 B$ '*.exe' EOS
 B$ 'Dll Files (*.dll, *.wll)' EOS
 B$ '*.dll;*.wll' EOS
 B$ 'Delphi Dll Files (*.bpl)' EOS
 B$ '*.bpl' EOS
 B$ 'Ocx Files (*.ocx)' EOS
 B$ '*.ocx' EOS
 B$ 'Driver Files (*.drv)' EOS
 B$ '*.drv' EOS
 B$ 'Cpl Files (*.cpl)' EOS
 B$ '*.cpl' EOS
 B$ 'fon Files(*.fon)' EOS
 B$ '*.fon' EOS
 B$ 'mpd Files(*.mpd)' EOS
 B$ '*.mpd' EOS
 B$ 'vbx Files(*.ocx)' EOS
 B$ '*.ocx' EOS
 B$ 'vbx Files(*.vbx)' EOS
 B$ '*.vbx' EOS
 B$ 'vxd Files(*.vxd)' EOS
 B$ '*.vxd' EOS
 B$ 'sys Files(*.sys)' EOS
 B$ '*.sys' EOS
 B$ 'All Files' EOS
 B$  '*.*' EOS 0]


; 'Open File Name' Box:

[uFileFilter: B$ ? # &MAX_PATH]
[SaveFilter: B$ ? # &MAX_PATH]
[ChoosenFile: B$ ? # &MAX_PATH]

;[OpenFileTitle: B$ 'Choose a file...' EOS]

[UserPeStart: D$ ?
 UserPeLen: D$ ?
 UserPeEnd: D$ ?]

[H.Source: D$ ?
 SourceLen: D$ ?
 H.Destination: D$ ?

 NumberOfReadBytes: D$ ?]

;[SourceFile: B$ 'Src.b' EOS]  ;;  DestinationFile: 'Dest2.exe' EOS, '                       ']

; All the second ? in lists declarations are for mem size. We need it for 'VirtualFree'

[CodeSource: D$ ? ?
 CodeSourceA: D$ ? ?
 CodeSourceB: D$ ? ?]

;;
  Beware: 'CodeSource' is *not* the Memory Chunk Pointer. 'CodeSource' is a Pointer to
  the Source inside 'UserPeStart'. See, for example 'ReleaseMainFile'.
  
  For a direct Allocation/Re-Allocation, it must be, for example:
  > VirtualAlloc UserPeStart 1_000_000 | Move D$CodeSource D$UserPeStart
  
  The only case 'CodeSource' is a real Memory Chunk is while disassembling. The switch
  between the two pointer, with the wished release, is done when the Disassembly is 
  finished.
;;

[InstructionAptr: D$ ?
 InstructionBptr: D$ ?]

;;
  'MacroList': |NAME|dWord1 dWord2 Byte|
  
  dWord1: Ptr to MacroData // dWord2: Lenght of these Data // Byte: Done Flag
;;

[EquateList: D$ ? ?
 MacroData: D$ ? ?
 MacroList: D$ ? ?
 Relocation: D$ ? ?
 DataList: D$ ? ?
 LabelList: D$ ? ?
 CodeList: D$ ? ?
 Coderef: D$ ? ?
 DataRef: D$ ? ?
 DllList: D$ ? ?
 ApiListA: D$ ? ?
 ApiListB: D$ ? ?]

; 'CodeRef' is: Dword | ... //... | Name | Dword1 dWord2 | // ....       ; 'CodeRefPtr'
;               ....|LabelName|........|LabelName|........|
; Where dWord is the Table Size.
; In the Records, Dword1 is a Pointer to CodeList and dWord2 a Pointer to Source.

; 'LabelList' is: Dword | ... // ...| Name | Dword1 Byte | // ....
; Where dWord is the Table Size.
; In the Records, Dword1 is the Pointer to CodeList, and Byte can be either/or:
; [FLAG_CODE_LABEL] [FLAG_DATA_LABEL] [FLAG_DONE]

[DataRefLimit: D$ ?
 EquateListLimit: D$ ?
 MacroListLimit: D$ ?
 MacroDataLimit: D$ ?
 LabelListLimit: D$ ?]

[EquateListPtr: D$ ?
 MacroDataPtr: D$ ?
 MacroListPtr: D$ ?
 RelocationPtr: D$ ?
 DataListPtr: D$ ?
 DataListPtrAtLastColon: D$ ?
 LabelListPtr: D$ ?
 CodeListPtr: D$ ?
 CoderefPtr: D$ ?
 DataRefPtr: D$ ?
 DllListPtr: D$ ?
 ApiListAPtr: D$ ?
 ApiListBPtr: D$ ?]

[LenOfCode: D$ ?
 FL.SourceReady: D$ ?
 FL.OldSourceReady: D$ ?]

;[SourceFilterPtr: D$ 1]
[OpenSourceFileTitle: B$ 'Choose main asm file...' EOS]
;[OpenPEFileTitle: B$ 'Choose main BUA PE file...' EOS]
[ChangeNameTitle: B$ 'Change the PE File Name...' EOS]
[SaveDlgNameTitle: B$ 'Give the Dialog Template File Name' EOS]

[ SourceFilesFilters:
  B$ 'Sources' EOS
  B$ '*.asm' EOS
  B$ 'All' EOS
  B$ '*.*' EOS 0]

 ;[PEFilesFilters: B$ 'RosAsm PE'     0  '*.exe;*.scr;*.dll' EOS 0]


[OpenSourceStruc:
 D$ len
 hwndFileOwner: D$ 0
 OSSInstance: D$ 0
 SourceFilesFilters
 uFileFilter
 260
 1
 SaveFilter
 260
 ChoosenFile
 260
 0                                ; SaveFilter:  full Path/Name.ext
                                  ; ChoosenFile: only Name.ext
 OpenSourceFileTitle
 D$ &OFN_LONGNAMES+&OFN_EXPLORER+&OFN_FILEMUSTEXIST+&OFN_PATHMUSTEXIST+&OFN_HIDEREADONLY
 0
 0
 0
 0
 0]

____________________________________________________________________________________________

SetBUAWindowText:

    Push esi,
         edi

    Mov esi STR.A.ApplicationName,
        edi STR.A.Trash

L1: movsb

    Test B$esi 0_FF NOT_ZERO L1<

S1: Mov eax '    ' | stosd

    Mov esi SaveFilter

L2: movsb

    Test B$esi 0_FF NOT_ZERO L2<

    Call 'USER32.SetWindowTextA' D$H.MainWindow,
                                 STR.A.Trash

    Pop edi,
        esi

ret

;;

  CreateFile is called by 'LoadSrc' 'ReplaceSourceOnly' 'OpenBUAsmPE',
                          'DirectMRUload', 'DirectLoad', 'LastMRULoading',
                          'ReloadForDissassembler'

;;

CreateSourceFile:

    On D$H.Source > 0 Call 'KERNEL32.CloseHandle' D$H.Source

    Call 'KERNEL32.CreateFileA' SaveFilter,
                                &GENERIC_READ,
                                &FILE_SHARE_READ+&FILE_SHARE_WRITE,
                                0,
                                &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL,
                                &NULL

    Comp eax &INVALID_HANDLE_VALUE = S1>

        Mov D$H.Source eax

        Call 'KERNEL32.GetFileSize' eax,
                                    0

        Mov D$SourceLen eax

ret

S1: Call MessageBox D$BusyFileptr

    mov eax &INVALID_HANDLE_VALUE

ret
 _______________________________

; Opening file:

Proc ClearPATH:

    Argument @STR.A.PATH

    Uses edi

    Mov ecx (&MAX_PATH/DWORD),
        edi D@STR.A.PATH

    xor eax eax | rep stosd

EndP


LoadSourceFile:

    ; Eax = D$SourceLen sinon &INVALID_HANDLE_VALUE
    Call CreateSourceFile

    Comp eax &INVALID_HANDLE_VALUE <> S1>

ret

S1: add eax 1_000_000  ; !!! TODO pas très clair ni très propre !!! -> TITLE_MAX ?!? pas sûr...

    Call VirtualAlloc UserPeStart,
                      eax

    Move D$CodeSource D$UserPeStart

    add eax D$UserPeStart | AlignOn PAGE_SIZE eax | sub eax 32

    Mov D$EndOfSourceMemory eax

    Push edi

        Mov edi D$CodeSource,
            ax CRLF,
            ecx 5

        rep stosw

        Mov eax edi       ; security for back test like ESI or EDI - 2 (sys page fault)

    Mov D$CodeSource eax

    Call 'KERNEL32.ReadFile' D$H.Source,
                             eax,                ; eax = mem buffer start
                             D$SourceLen,
                             NumberOfReadBytes,
                             &NULL

    Call 'KERNEL32.CloseHandle' D$H.Source

    Mov D$H.Source &NULL

    Mov edi D$CodeSource | add edi D$SourceLen | Mov eax CRLF2, ecx 100 | rep stosd
    Mov edi D$CodeSource, ecx 5 | sub edi 10 | rep stosw
    Mov eax D$CodeSource | add eax D$SourceLen | Mov D$STRUCT.EditData@SourceEnd eax
  ;  add eax 1_000_000 | Mov D$EndOfSourceMemory eax

    pop edi

ret


; In case user wants to Load a Source Only and Resources are actually available, sends
; a warning Message:

[LosingResources: B$ 'Delete actual Resources?' EOS]
[WarningTitle: B$ ' Warning:' EOS]

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

[MainName: B$ ? # &MAX_PATH]
[KeepResources: D$ ?]

OpenSourceOnly:

    Call ClearPATH SaveFilter

    Call 'Comdlg32.GetOpenFileNameA' OpenSourceStruc

    Test D$SaveFilter NA NOT_ZERO S1>

ret

S1: On D$FL.SourceReady = &TRUE Call ReleaseMainFile

    Call VirtualFree UserPeStart

    Call ClearBackTable | On B$KeepResources = &FALSE Call ReleaseResourceMemory

    Call LoadSourceFile | Call StoreChoosenName SaveFilter

    Mov D$FL.SourceReady &TRUE | Move D$STRUCT.EditData@UpperLine D$CodeSource

    Call KillTabs | Call KillTrailingSpaces

    Call SetBUAWindowText

    Mov D$SavingExtension '.exe'

    Call StartEdition

ret


[IncludeLen: D$ ?]

IncludeSource:

    Call ClearPATH SaveFilter | Call 'Comdlg32.GetOpenFileNameA' OpenSourceStruc

    On D$SaveFilter = 0 ret

    Call ClearBackTable

    Call 'KERNEL32.CreateFileA' SaveFilter,
                                &GENERIC_READ, &FILE_SHARE_READ__&FILE_SHARE_WRITE,
                                0, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, &NULL
    If eax = &INVALID_HANDLE_VALUE
        Call MessageBox D$BusyFilePtr | ret
    Else
        Mov D$H.Source eax
    End_If

    Call 'KERNEL32.GetFileSize' eax, 0 | Mov D$IncludeLen eax

    Call ReMapSourceMemoryIfNeeded D$IncludeLen

    If eax = &IDNO
        Call 'KERNEL32.CloseHandle' D$H.Source | Mov D$H.Source 0 | ret
    End_If

  ; Ensure the inclusion will not break an existing line in two parts:
    Mov edi D$STRUCT.EditData@CurrentWritingPos
    While B$edi-1 <> LF | inc edi | inc D$STRUCT.EditData@CurrentWritingPos |  End_While

  ; Make room inside actual Source by copying backward:
  ; esi at Source End // edi at Source End + whished room:
    Mov esi D$STRUCT.EditData@SourceEnd | add esi 400
    Mov edi esi | add edi D$IncludeLen | add edi 2      ; this '2' is for added CRLF at end.

  ; How many bytes to Move = how many bytes between Source End and actual Pos:
    Mov ecx D$STRUCT.EditData@SourceEnd | add ecx 400 | sub ecx D$STRUCT.EditData@CurrentWritingPos | inc ecx

    std | rep movsb | cld

    Move D$LP.BlockStartText D$STRUCT.EditData@CurrentWritingPos


    Call 'KERNEL32.ReadFile' D$H.Source, D$LP.BlockStartText, D$IncludeLen, NumberOfReadBytes, 0
    Call 'KERNEL32.CloseHandle' D$H.Source | Mov D$H.Source 0

    Mov edi D$STRUCT.EditData@CurrentWritingPos | add edi D$IncludeLen | Mov W$edi CRLF

    Mov D$LP.BlockEndText edi | sub D$LP.BlockEndText 1

    Mov eax D$IncludeLen | add eax 2 | add D$SourceLen eax | add D$STRUCT.EditData@SourceEnd eax
    Mov D$FL.BlockInside &TRUE

    Mov esi D$LP.BlockStartText,
        ecx D$LP.BlockEndText

    sub ecx esi

    Call BlockCleaner
ret


[OpeningSourceOnly: D$ ?]

ReplaceSourceOnly:  ; 'ControlS'
    Mov B$KeepResources &TRUE

    ..If D$TABLE.Titles = 0
L1:     Call OpenSourceOnly | Call LoadBookMarks

    ..Else
        Mov B$OpeningSourceOnly &TRUE
      ; Tag Dialog 26001
        Call 'USER32.DialogBoxParamA' D$H.Instance, 26001, &NULL, AllOrPartProc, &NULL
        Mov B$OpeningSourceOnly &FALSE

        .If B$AllOrPart = 0-1
            Call RestoreRealSource | Mov D$TABLE.Titles &NULL | jmp L1<

        .Else_If B$AllOrPart = 1   ; 'OpenSourceOnly'

            Call ClearPATH SaveFilter | Call 'Comdlg32.GetOpenFileNameA' OpenSourceStruc

            On D$SaveFilter = EOS ret

                Call CreateSourceFile | On eax = &INVALID_HANDLE_VALUE ret
                Mov eax D$CodeSource | add eax D$SourceLen | Mov D$STRUCT.EditData@SourceEnd eax

                Call 'KERNEL32.ReadFile' D$H.Source, D$CodeSource, D$SourceLen,
                                         NumberOfReadBytes 0

                Call 'KERNEL32.CloseHandle' D$H.Source | Mov D$H.Source 0

                Call KillTabs | Call KillTrailingSpaces

        .End_If

    ..End_If
ret


; In case of 'New' Internaly called because user attempted to load a 'no source inside'
; PE, we change the Saving File Name to 'NoName.exe':

[NoNameNewFile: B$ 'NoName.exe' EOS]
[NewFileBoxTitle: B$ 'New File Name' EOS]

NewFileNameDialog:
    Call GetDirectory ActualDir

  ; Tag Dialog 5
    Call 'USER32.DialogBoxParamA' D$H.Instance, 5, D$H.MainWindow, NewFileType, 0

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

            Call 'USER32.MessageBoxA' D$H.MainWindow, {B$ 'Overwrite?' EOS},
                                    {B$ 'File already exist' EOS}, &MB_YESNO
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
    Mov D$FL.RelocsWanted &FALSE
    Call ChangeName
    cmp D$SavingExtension '.DLL' | setz B$FL.RelocsWanted ; jE! Relocs for Dll
    Mov D$FL.SourceReady &TRUE
    Call AskForRedraw
    Call ReInitUndo
    Call SetPartialEditionFromPos | Call EnableMenutems
    Call LoadBookMarks
L9: ret


ChangeName:
    or D$STRUC.OPENFILENAME@Flags &OFN_OVERWRITEPROMPT
    Mov D$STRUC.OPENFILENAME@lpstrTitle NewFileBoxTitle
    Call 'Comdlg32.GetSaveFileNameA' STRUC.OPENFILENAME
    Mov D$STRUC.OPENFILENAME@lpstrTitle &NULL
    On eax = &FALSE, ret

    xor D$STRUC.OPENFILENAME@Flags &OFN_OVERWRITEPROMPT | jmp L1>

AutoNew:
    Mov edi SaveFilter, eax 0, ecx 65 | rep stosd


    Mov edi SaveFilter

@NewEmptyFromMenu:
    Mov esi NoNameNewFile
    While B$esi > 0
        movsb
    End_While
    movsb

L1: Call SaveNewFileName

    On eax = &FALSE jmp NewFileNameDialog

    Call SetBUAWindowText

  ; (If here, eax = D$SourceLen)
    xor D$STRUC.OPENFILENAME@Flags &OFN_OVERWRITEPROMPT

    Mov eax &TRUE
ret
____________________________________________________________________________________________

[FileTypeChoice: D$ ?
 H.NewChoiceList: D$ ?]

; Tag Dialog 5

Proc NewFileType:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_COMMAND
        ..If D@wParam = &IDCANCEL
L0:         Mov D$FileTypeChoice 0-1
L1:         Call 'USER32.EndDialog' D@hwnd 0

        ..Else
            .If W@wParam = 10
                If W@wParam+2 = &LBN_DBLCLK
                    Call 'USER32.SendMessageA' D$H.NewChoiceList, &LB_GETCURSEL, 0, 0
                    inc eax | Mov D$FileTypeChoice eax | jmp L1<

                End_If

            .End_If

        ..End_If

    ...Else_If D@msg = &WM_INITDIALOG

        Call SetIconDialog

        Call 'USER32.GetDlgItem' D@hwnd, 10 | Mov D$H.NewChoiceList eax

        Call SetBaseList

    ...Else
        popad | Mov eax &FALSE | ExitP

    ...End_If

    popad | Mov eax &TRUE
EndP


[BaseFilesPath: D$ ? # &MAX_PATH]

[BaseNamePointer: D$ 0] ; !!! je pense qu'il y à un truc...

[@H.BasesList: D$ ?
 @BaseListEnd: D$ ?]

SetBaseList:
INT3 | NOP ; !!! TODO !!!
;;
    Call GetBUAsmFilesPath

    Mov esi BUAsmFilesPath, edi BaseFilesPath
    While B$esi <> 0 | movsb | End_While
    Mov D$edi 'Base', D$edi+4 's\'
    add edi 6 | Mov D$BaseNamePointer edi

    Push esi | Mov esi {B$ 'BasesList.txt' EOS} | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0: Pop esi

    Mov B$edi EOS

    Call 'KERNEL32.CreateFileA' BaseFilesPath, &GENERIC_READ,
                                &FILE_SHARE_READ+&FILE_SHARE_WRITE, 0, &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL, 0

    .If eax = &INVALID_HANDLE_VALUE
        Mov eax 0-1 | ret

    .Else
        Mov D$H.BasesList eax
        Call 'KERNEL32.GetFileSize' eax, 0     ; !!!!!!!!!!!!!! TODO !!!!!!!!!!!!!! ICI !!!
        Mov D$BaseListEnd Trash | add D$BaseListEnd eax
        Call 'KERNEL32.ReadFile' D$H.BasesList, Trash, eax, NumberOfReadBytes, 0
        Call 'KERNEL32.CloseHandle' D$H.BasesList

        Mov esi Trash
        .While esi < D$BaseListEnd
            While B$esi <> SPC | inc esi | End_While
            Mov edi TrashString
            While B$esi <> CR | movsb | End_While | Mov B$edi 0 | add esi 2

            Call 'USER32.SendMessageA' D$H.NewChoiceList,
                                       &LB_ADDSTRING,
                                       0,
                                       TrashString

        .End_While

    .End_If
;;
ret


GetBaseName:
  ; eax = Indice (Indice zero assumed as 'NewEmpty' by 'SetBaseList').
    Mov esi LP.MemTrash, ecx 1 ; !!! TODO !!!

  ; Skip lines until indice matches:
    .While ecx < eax
        While B$esi <> CR | inc esi | End_While | add esi 2
        inc ecx
    .End_While

  ; Set a zero at the end of the File Name:
    Push esi
        While B$esi <> SPC | inc esi | End_While
        Mov D$esi 0
    Pop esi
ret
____________________________________________________________________________________________

[BadNewExtension: B$ 'Bad Extension' EOS]

SaveNewFileName:

    Mov esi SaveFilter,
        edi MainName

L0: lodsb | stosb | Comp B$esi '.' = S1>

    Comp B$esi EOS <> L0<

S1: On B$esi+(4*ASCII) <> EOS jmp L0<

    Mov B$edi EOS

    ; Eax =Suffixe tout en minuscules
    lodsd | or eax 020202000

    Comp eax '.exe' = S3>

    Comp eax '.dll' = S3>

    Comp eax '.sys' = S3>

    Comp eax '.scr' = S3>

    Comp al '.' = S2>

    Comp al EOS = S2>

    Call MessageBox BadNewExtension

    Mov eax &FALSE

ret

S2: sub esi (4*ASCII)

    Mov edi esi,
        eax '.exe'

    stosd | Mov B$edi EOS   ; Complete user given name (any use?...)

    ; Eax = Suffixe tout en majuscules
S3: or eax 020202000 ; | xor eax 020202000

    Mov D$SavingExtension eax,
        eax &TRUE

ret

StartNewFile:

    Call ReInitUndo
    Call ReInitHeaderFormat
    On D$FL.SourceReady = &TRUE Call ReleaseMainFile

    Call ClearBackTable | Call ReleaseResourceMemory

    Mov D$SourceLen 0

    ;  VirtualAlloc CodeSource 1_000_000
    Call VirtualAlloc UserPeStart,
                      1_000_000

    Move D$CodeSource D$UserPeStart

    add D$CodeSource 10     ; security for back test like ESI or EDI - 2 (no page fault)

    Mov edi D$CodeSource
    Mov eax 0A0D0A0D, ecx 100 | rep stosd                            ; end security tail
    Mov edi D$CodeSource, ecx 5 | sub edi 10 | rep stosw
    Mov eax D$CodeSource
    Mov D$STRUCT.EditData@SourceEnd eax, D$STRUCT.EditData@CurrentWritingPos eax, D$STRUCT.EditData@CaretLine 0, D$STRUCT.EditData@CaretRow 1
    add eax 1_000_000 | Mov D$EndOfSourceMemory eax

    Mov D$FL.SourceReady &TRUE | Move D$STRUCT.EditData@UpperLine D$CodeSource

    Mov D$TABLE.Titles &NULL, D$PreviousPartialSourceLen 0

  ; Clear possible previous resources:
    Call ClearCustomList

    Call AskForRedraw

ret


[UserPEStartOfResources: D$ ?
 ResourcesSize: D$ ?]

SearchPEstartOfResource:
    Mov B$NoResourcesPE &FALSE
    Mov esi D$UserPeStart, D$UserPEStartOfResources 0
    movzx eax W$esi+8
   ; Mov eax 0 | add esi 8 | lodsw    ; parag. size of dos header end >PE header adress
    shl eax 4 | sub eax 4
    Mov esi D$UserPeStart | add esi eax | lodsd      ; eax = PE header

    Mov esi D$UserPeStart | add esi eax
    cmp W$esi 'PE' | jne L9>
 ______________________________________

; read data in PeHeader:

L0: Mov ecx 0 | Mov cx w$esi+6            ; word record of section number
    add esi 136 | lodsd | Mov ebx eax     ; RVA of resources from "Image Data Dir..."
    Mov D$ResourcesRVA eax
    Move D$ResourcesSize D$esi

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


[EndOfSectionSearch: D$ ?]

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
[CheckMessageTitle: B$ 'Bad CheckSum' EOS]

[CheckInfo: B$ "
 This File has been modified, either
 by an external Tool, or by a Virus.     " EOS]

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
    .Else_If D$FL.RelocsWanted = &TRUE ;SavingExtension = '.DLL' ; jE!
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

    Call 'KERNEL32.WriteFile' D$H.Destination, D$CodeList, D$LenOfCode,
                              NumberOfReadBytes, 0

    If B$ExportsectionWanted = &TRUE
        Call 'KERNEL32.WriteFile' D$H.Destination, D$ExportListBPtr,
                                  D$FileAlignedExportSectionLen, NumberOfReadBytes, 0
    End_If

    If D$SavingExtension = '.SYS'
        jmp L1>
    Else_If D$FL.RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
L1:
        If D$RelocSectionSize => 8 ;jE!
          Call 'KERNEL32.WriteFile' D$H.Destination,
                                   D$Relocation, D$FileAlignedRelocationSize,
                                   NumberOfReadBytes, 0
        End_If
    End_If

  ;  Call 'KERNEL32.WriteFile' D$H.Destination, D$CodeSource, D$SourceLen,
  ;                            NumberOfReadBytes, 0

    Call 'KERNEL32.CloseHandle' D$H.Destination | Mov D$H.Destination 0
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

[H.BookMarksFile: D$ ?
 BookMarksFileLen: D$ ?]

[STRUC.FindFile:
 @dwFileAttributes: D$ ?
 @ftCreationTime.dwLowDateTime: D$ ?
 @ftCreationTime.dwHighDateTime: D$ ?
 @ftLastAccessTime.dwLowDateTime: D$ ?
 @ftLastAccessTime.dwHighDateTime: D$ ?
 @ftLastWriteTime.dwLowDateTime: D$ ?
 @ftLastWriteTime.dwHighDateTime: D$ ?
 @nFileSizeHigh: D$ ?
 @nFileSizeLow: D$ ?
 @dwReserved0: D$ ?
 @dwReserved1: D$ ?
 @cFileName: B$ ? # &MAX_PATH
 @cAlternate: B$ ? # 14]

LoadBookMarks:
    Mov D$NumberOfBookMarks 0
    Mov edi SaveFilter, al 0, ecx 0-1 | repne scasb
    While B$edi <> '.'
        dec edi
    End_While

    Push D$edi, edi

        Mov D$edi '.BKM'

        Call 'KERNEL32.FindFirstFileA' SaveFilter STRUC.FindFile
        .If eax = &INVALID_HANDLE_VALUE

            Call VirtualFree BookMarks

            Pop edi, D$edi | ret

        .Else

            Call 'KERNEL32.FindClose' eax

        .End_If

        Call 'KERNEL32.CreateFileA' SaveFilter &GENERIC_READ,
                                    &FILE_SHARE_READ+&FILE_SHARE_WRITE, 0, &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_NORMAL, 0
   Pop edi, D$edi

   If eax = &INVALID_HANDLE_VALUE
        Call MessageBox D$BusyFilePtr

        Pop eax | ret                       ; Pop return adress of caller and ret to Callback

   Else
        Mov D$H.BookMarksFile eax
   End_If

    Call 'KERNEL32.GetFileSize'  eax 0 | Mov D$BookMarksFileLen eax

    Call VirtualFree BookMarks

    Call VirtualAlloc BookMarks,
                      01000

    Call 'KERNEL32.ReadFile' D$H.BookMarksFile D$BookMarks D$BookMarksFileLen,
                            NumberOfReadBytes 0
    Call CreateTreeViewList | Call SetTreeDialogPos

    Call 'KERNEL32.CloseHandle' D$H.BookMarksFile

ret



[EndOfSourceMemory: D$ ?]

OpenBUAsmPE:

    Mov eax D$FL.SourceReady,
        D$FL.SourceReady &FALSE,
        D$FL.OldSourceReady eax

    Call ClearPATH SaveFilter | Call ClearPATH ChoosenFile

    Call 'Comdlg32.GetOpenFileNameA' STRUC.OPENFILENAME

    test D$SaveFilter NA NOT_ZERO S1>

        Mov eax D$FL.OldSourceReady,
            D$FL.SourceReady eax

EndP

S1:DirectLoad:

    Call ReleaseMainFile | Call StoreChoosenName SaveFilter

    Call ClearBackTable | Call ReleaseResourceMemory

    Call VirtualFree UserPeStart

LastLoading:

    Mov esi MainName

    While B$esi <> EOS | add esi ASCII | EndWhile

    While B$esi <> '\' | sub esi ASCII | EndWhile

    Push D$esi

        Mov B$esi+1 EOS | Call SetDirectory MainName

    Pop D$esi

    Call CreateSourceFile | On eax = &INVALID_HANDLE_VALUE ret

; ESi = SaveFilter !!! Voir plus bas !!!

    Call GetFileNameFromPath

  ; > eax = source len

;;

    The reason for allocating a twice (>>> shl eax 1) bigger Mem:
  
    When having TITLEs inside, the editor make a Copy of the edited TITLE, at the bottom
    of this Mem. Therefore, if the Source is made of two TITLEs, a very short one, say
    for Macros, and a very big one, for everything else, we need twice more Mem. Plus the
    additional Edition security, of course:

;;

    Mov D$UserPeEnd eax,
        D$UserPeLen eax

    shl eax 1 | add eax 100_000

    Call VirtualAlloc UserPeStart,
                      eax

    Mov edx D$UserPeStart,
        eax edx

    add D$UserPeEnd eax

    AlignOn PAGE_SIZE eax | sub eax 32 | add eax edx | Mov D$EndOfSourceMemory eax

    Call 'KERNEL32.ReadFile' D$H.Source,
                             D$UserPeStart,
                             D$UserPeLen,
                             NumberOfReadBytes,
                             &NULL

    Call 'KERNEL32.GetLastError'



    Call 'KERNEL32.CloseHandle' D$H.Source

    Mov D$H.Source &NULL

    Mov eax D$UserPeStart | On W$eax <> 'MZ' jmp ExitNotPeExe

    Mov eax D$UserPeStart | sub eax DosHeader | add eax PeHeaderPointer

    Mov ebx D$eax, eax D$UserPeStart | add eax ebx

    On eax >= D$UserPeEnd jmp ExitNotPeExe

    On W$eax <> 'PE' jmp ExitNotPeExe

    cmp D$eax+0A0 0 | setne B$FL.RelocsWanted; jE! RelocsWanted

    Mov B$ThisSourceIsDisassembled &FALSE

    Mov eax D$UserPeStart | add eax 0178 | Mov esi eax     ; +0178 > first section header

    add eax 400 | Mov D$EndOfSectionSearch eax             ; +(10*40) bytes per section

L0: lodsd | cmp eax '.src' | je L1>
        add esi 36 | cmp esi D$EndOfSectionSearch | jb L0<
            jmp TryDisassembly

ExitNotPeExe:

    Call MessageBox D$NotPeExePtr | Call AutoNew | jmp StartNewFile


L1: lodsd | lodsd | Mov D$SourceLen eax
    lodsd | lodsd | lodsd
  ; eax = 0 based pointer to source

  ; 'MessageBox' messages could send a WM_PAINT before complete initializations:
    Mov D$TABLE.Titles &NULL, D$LP.ActualTitle &NULL

    Call ReadCheckSum | Call ReadHeaderFormat

    add eax D$UserPeStart | Mov D$CodeSource eax

    Mov edi D$CodeSource | add edi D$SourceLen | Mov eax 0A0D0A0D, ecx 100

    rep stosd

    Mov eax D$CodeSource | add eax D$SourceLen | Mov D$STRUCT.EditData@SourceEnd eax
   ; add eax 1_000_000 | Mov D$EndOfSourceMemory eax
    Move D$STRUCT.EditData@UpperLine D$CodeSource | Mov D$FL.SourceReady &TRUE

    Call KillTabs | Call KillTrailingSpaces

    Call SearchPEstartOfResource | On B$NoResourcesPE = &TRUE, jmp L9>>

    ; Upload icon:

; Icon storing more stand-alone because of feature for reading icon in other PEs. So does
; it again job done by 'SearchPEstartOfResource'. So, i make it set 'ResourcesRVA' value
; (very durty, but...).

    Move D$iExePtr D$UserPeStart | Call ReadRosAsmPeIcon ; load icon with icon editor routine

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

L9: Mov edi D$CodeSource, ecx 5, eax CRLF | sub edi 10 | rep stosw ; security for back search

    Mov D$DestinationFile EOS

    Call SetBUAWindowText

    Call StartEdition

ret

; For 'FillExportSection':

GetFileNameFromPath:
    Push eax, esi, edi
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
    Pop edi, esi, eax
ret


[DisChoice: D$ ?]

TryDisassembly:
    On D$AddressToBeForced = &TRUE, jmp DisMain

  ; Tag Dialog 27000
    Call 'USER32.DialogBoxParamA' D$H.Instance, 27000, D$H.MainWindow, DisassembleProc, 0
    Test D$DisChoice 0_8000_0000 ZERO L9>
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
            Call 'USER32.EndDialog' D@hwnd 0
          ; Call 'USER32.DestroyWindow' D@hwnd ; Comments...

        ..Else_If D@wParam = &IDOK
            or D$DisChoice 0_8000_0000
            Call 'USER32.EndDialog' D@hwnd 0

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
        Push esi
L0:         lodsd | cmp eax '.dat' | je L1>
                add esi 36 | cmp esi D$EndOfSectionSearch | jb L0<
                jmp L0>
L1:         add esi 32
            lodsd | Mov D$DataCharacteristics eax
L0:     Pop esi

      ; Code Characteristics:
L0:     lodsd | cmp eax '.tex' | je L1>

            add esi 36 | cmp esi D$EndOfSectionSearch | jb L0<

            Call MessageBox D$NotPEPtr

ret

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
    Push edi
        Mov edi SubSystem
        Mov eax 2 | stosd                       ; Subsystem GUI  //  DLL characteristics 0
        Mov eax 0100000 | stosd                 ; AppStackMax
        Mov eax   01000 | stosd                 ; AppStackMin
        Mov eax 0100000 | stosd                 ; AppHeapMax
        Mov eax       0 | stosd                 ; AppHeapMin
        Mov D$CodeCharacteristics 0_60000020    ; readable, runable, code
        Mov D$DataCharacteristics 0_C0000040    ; readable, writeble, Data
        Mov W$DllCharacteristics 0
    Pop edi
ret
 _______________________________

; Writing files:

[DestinationFile: B$ ? # &MAX_PATH]

OpenDestinationFile:
    On D$H.Destination > 0, Call 'KERNEL32.CloseHandle' D$H.Destination

    Call 'KERNEL32.CreateFileA' DestinationFile, &GENERIC_WRITE,
                                &FILE_SHARE_READ__&FILE_SHARE_WRITE, 0,
                                &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE
        Call MessageBox D$BusyFilePtr

        Mov D$FL.CompileErrorHappend &TRUE

      ; Pop return adress of caller and ret to Callback:
        Pop eax | ret

    Else
        Mov D$H.Destination eax

    End_If
ret

____________________________________________________________________________________________
;;
 This is for developpements test:

 DANGER >>> Writes a 'Test.a' on the disk without warning.
 
 'WriteDestination' is used only by 'AsmMain' when you branch to it. All these commented
 lines are for tests along developpement giving ability, by calling it, to output
 internal tables for exemple, if we want to see what is in 'DllList', we route here with
 a 'jmp L7>' after 'BuildImport'; with "Mov eax, 010000 | Push eax" and "Push D$DllList"
 uncommented.
;;
WriteDestination:
    Call OpenDestinationFile

   ; hexprint D$StripLen
   ; showme DestinationFile

    Push 0
    Mov D$NumberOfReadBytes 0 | Push NumberOfReadBytes

      Mov eax, 0100

 ;;;; Just uncomment the one you want in 'Test.a':

    ; Push D$LenOfCode, D$CodeSource
    ; Push D$StripLen, D$CodeSourceB
     Push D$StripLen, D$CodeSourceA
    ; Push eax, D$EquateList
    ; Push eax, D$MacroList
    ; Push eax, D$PlainLabelList
    ; Push eax, D$MacroData
    ; Push eax, D$DataList
    ; Push eax, D$LabelList
    ; Push D$StripLen, D$Coderef
    ; Push eax, D$DataRef
    ; Push eax, D$DllList
    ; Push eax, D$ApiListA
    ; Push eax, D$ApiListB
    ; Push eax, D$Relocation
    ; Push eax, D$TreeList
    ; Push eax, D$CodeList
    ; Push eax, D$ExportListAPtr
;;
    Mov esi D$StatementsTable | While D$esi > 0 | add esi 4 | End_While
    sub esi D$StatementsTable
    Push esi, D$StatementsTable
;;

   ; Mov eax D$EndOfSectionsMap | sub eax D$SectionsMap | Push eax
;
 ;   Push D$SectionsMap

    Push D$H.Destination
    Call 'KERNEL32.WriteFile'
ret


ControlS:
    ..If D$TABLE.Titles = &NULL
        Call SaveSource
        Call 'USER32.MessageBoxA' D$H.MainWindow, DestinationFile,
                                {B$ 'Saved in actual Directory:' EOS}, 0

    ..Else
        Call 'USER32.DialogBoxParamA' D$H.Instance, 26000, &NULL, AllOrPartProc, &NULL

        .If B$AllOrPart = 0-1
            Call RestoreRealSource | Call SaveSource
            Call SetPartialEditionFromPos

        .Else_If B$AllOrPart = 1
            Mov B$WeAreSavingPart &TRUE
            Call SaveSource
            Mov B$WeAreSavingPart &FALSE

        .Else_If B$AllOrPart = 9
            Call 'USER32.MessageBoxA' D$H.MainWindow, {B$ "     Do you wish to save all the TITLEs
of this Application into as many asm Files?      " EOS}, {B$ 'Sure?' EOS}, &MB_YESNO
            If eax = &IDYES
                Mov B$WeAreSavingPart &TRUE
                Call SaveActualPos
                Call GetTitlesNumber

L0:             Push ecx
                    Call SelectTitle ecx | Call SaveSource
                Pop ecx
                loop L0<

                Call 'USER32.MessageBoxA' D$H.MainWindow, {B$ 'Done' EOS}, 0, 0
                Call RestoreActualPos
                Mov B$WeAreSavingPart &FALSE
            End_If

        .End_If

    ..End_If
ret


[ActualPos: D$ ?]

SaveActualPos:
    Move D$ActualPos D$STRUCT.EditData@CurrentWritingPos
ret


Proc SelectTitle:

    Argument @Indice

    Call RestoreRealSource

    Mov eax D@Indice | sub eax 1 | Mov D$ID.ActualTitle eax

    Move D$STRUCT.EditData@CurrentWritingPos D$TABLE.Titles+eax*DWORD

    Call SetPartialEdition

    Mov esi D$CodeSource, edi PartName

    If D$esi = 'TITL'

        add esi 5 | While B$esi = SPC | inc esi | End_While

        While B$esi > SPC | movsb | End_While | Mov B$edi 0

    Else

        Mov D$PartName 'Top'

    End_If

EndP


GetTitlesNumber:
    Mov esi TABLE.Titles, ecx 0
    While D$esi <> 0
        inc ecx | add esi 4
    End_While
ret


RestoreActualPos:
    Move D$STRUCT.EditData@CurrentWritingPos D$ActualPos
ret


[BackUpPathName: B$ ? # &MAX_PATH]
[BackUpNewPathName: B$ ? # &MAX_PATH]

[WIN32_FIND_DATA:
 @dwFileAttributes: D$ ?
 @ftCreationTime.dwLowDateTime: D$ ?
 @ftCreationTime.dwHighDateTime: D$ ?
 @ftLastAccessTime.dwLowDateTime: D$ ?
 @ftLastAccessTime.dwHighDateTime: D$ ?
 @ftLastWriteTime.dwLowDateTime: D$ ?
 @ftLastWriteTime.dwHighDateTime: D$ ?
 @nFileSizeHigh: D$ ?
 @nFileSizeLow: D$ ?
 @dwReserved0: D$ ?
 @dwReserved1: D$ ?
 @cFileName: B$ ? # &MAX_PATH
 @cAlternate: B$ ? # 80]

[H.BackUpFile: D$ ?
 LastBackUpIndice: D$ ?]

[MaxBackUp: B$ '0010' EOS]

ControlK:
    On D$MaxBackUp = '0000' ret

    If D$FL.ReadyToRun = &FALSE
        Call 'USER32.MessageBoxA' D$H.MainWindow,
                                 {B$ 'You have to [Compile] or to [Run] before a PE BackUp' EOS},
                                 {B$ '[Ctrl][K] BackUp aborted:' EOS}, 0
        ret
    End_If

    Mov esi MainName, edi BackUpPathName

    While B$esi <> 0 | movsb | End_While
    While B$esi <> '\' | dec esi | dec edi | End_While

    Push esi
        While B$esi <> 0 | movsb | End_While
        Mov B$edi '\' | inc esi
        Mov D$edi 'Back', D$edi+4 'Up\ ' | add edi 7
    Pop esi

    inc esi | While B$esi <> 0 | movsb | End_While
    Mov B$edi '*' | inc edi
    Mov eax D$SavingExtension | stosd | Mov B$edi 0

    Push edi
        Call 'KERNEL32.FindFirstFileA' BackUpPathName, WIN32_FIND_DATA
        Mov D$H.BackUpFile eax
    Pop edi

    ...If eax = &INVALID_HANDLE_VALUE
        While B$edi <> '\' | dec edi | End_While | Mov B$edi 0
        Push edi
            Call 'KERNEL32.CreateDirectoryA' BackUpPathName, 0
        Pop edi
        Mov B$edi '\'
        While B$edi <> '*' | inc edi | End_While
        Mov D$edi '0000' | add edi 4
L0:     Mov eax D$SavingExtension | stosd | Mov B$edi 0
        Mov esi MainName | While B$esi <> 0 | inc esi | End_While
        Move D$esi D$SavingExtension | Mov B$esi+4 0
        Push esi
            Call 'KERNEL32.CopyFileA' MainName, BackUpPathName, &FALSE
        Pop esi
        Mov B$esi 0

    ...Else
L2:     Call 'KERNEL32.FindNextFileA' D$H.BackUpFile, WIN32_FIND_DATA
        cmp eax &TRUE | je L2<

        Call 'KERNEL32.FindClose' D$H.BackUpFile

        Mov esi WIN32_FIND_DATA@cFileName
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
        Push ebx
            Call 'KERNEL32.DeleteFileA' BackUpPathName
            Mov esi BackUpPathName, edi BackUpNewPathName
            While B$esi <> 0 | movsb | End_While | Mov B$edi 0
        Pop ebx

L0:     lea esi D$BackUpPathName+ebx+4
        Call IncTextDwordBeforeEsi
        Mov eax D$MaxBackUp, edx D$esi-4 | bswap eax | bswap edx | cmp edx eax | ja L9>
        Push ebx
            Call 'KERNEL32.MoveFileA' BackUpPathName, BackUpNewPathName
        Pop ebx
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

    Call 'USER32.MessageBoxA' D$H.MainWindow, BackUpPathName, {B$ 'Backup done to:' EOS}, 0
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



[PartName: D$ ? # 16]

[WeAreSavingPart: D$ ?
 AllOrPart: D$ ?]

[ReplaceSourceText: B$ 'Load Source' EOS]

; Tag Dialog 26000

Proc AllOrPartProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_INITDIALOG
            On B$OpeningSourceOnly = &TRUE,
                Call 'USER32.SendMessageA' D@hwnd, &WM_SETTEXT, 0, ReplaceSourceText

            Mov esi D$LP.ActualTitle
            .If esi = D$RealCodeSource
                If D$esi <> 'TITL'
                    Mov D$PartName 'Top ', B$PartName+3 EOS

                    Call 'USER32.SetDlgItemTextA' D@hwnd,
                                                  1,
                                                  STR.A.DefaultTopTITLE

                    jmp L8>>
                End_If
            .End_If

            While B$esi > SPC | inc esi | End_While ; jump over 'TITLE'

            While B$esi = SPC | inc esi | End_While ; >>> 'TitleName
            Mov edi PartName
            While B$esi > SPC | movsb | End_While
            Mov al EOS | stosb
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

        Call 'USER32.DestroyWindow' D@hwnd

    ...Else
L8:     popad | Mov eax &FALSE | jmp L9>

    ...End_If

    popad | Mov eax &TRUE
L9: EndP


[IncSaveTitle: B$ 'Saving .inc or .asm ? ...' EOS]
[IncSaveBody: B$ "
 
   Yes > Save the inc File in its original Directory      
   
   No  > Save the asm File in current Directory" EOS]


SaveSource:
    .If B$WeAreSavingPart = &TRUE
        Call RestoreRealSource

        Call SearchForIncInclude

        If eax = &TRUE
            Mov esi D$PointerToIncFileName, edi DestinationFile
            While B$esi > SPC | movsb | End_While | Mov B$edi 0
        End_If

        Push eax
            Call SetPartialEditionFromPos
        Pop eax

        If eax = &TRUE
            Call 'USER32.MessageBoxA' D$H.MainWindow, IncSaveBody, IncSaveTitle,
                                      &MB_SYSTEMMODAL_&MB_YESNO
            On eax = &IDYES, jmp L2>
        End_If
    .End_If

    Call SetAsmSavingName

L2: Call OpenDestinationFile | Mov D$NumberOfReadBytes 0
    Call 'KERNEL32.WriteFile'  D$H.Destination, D$CodeSource, D$SourceLen,
                               NumberOfReadBytes, 0

    Call 'KERNEL32.CloseHandle' D$H.Destination | Mov D$H.Destination 0
ret


SearchForIncInclude:
    Mov esi D$CodeSource, eax &FALSE

    .While esi < D$STRUCT.EditData@SourceEnd
        Call IsItPREPARSE
        .If eax = &TRUE
            add esi 9
L1:         While B$esi = SPC | inc esi | End_While
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
            If B$esi-1 < SPC
                On B$esi+8 = SPC, Mov eax &TRUE
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
                Mov bl B$esi+10 | or bl SPC | On bl = 'r', Mov eax &TRUE
            End_If
        .End_If
    ..End_If

    .If eax = &FALSE
L1:     While B$esi > SPC | inc esi | End_While

        While B$esi = SPC  | inc esi | End_While

        If B$esi = ','
            inc esi | jmp L1<
        End_If

        On B$esi > SPC, jmp L0<
    .End_If
ret

IsTheActualTitleIncluded:
    Push esi
        Mov esi D$CodeSource, eax &FALSE

        While esi < D$STRUCT.EditData@SourceEnd
            ...If D$esi = 'INCI'  ; INCI NCLU DE
                ..If D$esi+4 = 'NCLU'
                    .If W$esi+8 = 'DE'
                        If B$esi-1 < SPC
                            On B$esi+10 = SPC, Call IsThisTheActualTitle
                            On eax = &TRUE, jmp L9>
                        End_If
                    .End_If
                ..End_If
            ...End_If

            inc esi
        End_While
L9: Pop esi
ret


[PointerToIncFileName: D$ ?]

IsThisTheActualTitle:
    add esi 10
    While B$esi = SPC | inc esi | End_While
    Mov D$PointerToIncFileName esi

    Mov edx &FALSE
    While B$esi <> CR | inc esi | End_While
    While B$esi <> '\' | dec esi | End_While | inc esi
    Mov edi D$LP.ActualTitle  ; TITLE xxxx
    add edi 6 | While B$edi = SPC | inc edi | End_While

    While B$esi <> '.'
        Mov al B$esi, bl B$edi | inc esi | inc edi | cmp al bl | jne L7>
    End_While

    .If B$esi = '.'
        If B$edi <= SPC
            Mov edx &TRUE
        End_If
    .End_If

L7: Mov eax edx
ret

____________________________________________________________________________________________


[ExportAjust: D$ ?]    ; The value to turn an Export section Adress to RVA

PrepareDllVariables:
    Mov eax D$AppStartOfCode
    add eax D$AppTrueCodeSize | AlignOn 0200 eax | Mov D$AppStartOfExp eax
    add eax D$ExportSectionLen | AlignOn 0200 eax

    If D$RelocSectionSize => 8 ;jE!
        Mov D$AppStartOfReloc eax | add eax D$RelocSectionSize | AlignOn 0200 eax
    Else
        Mov D$AppStartOfSrc eax | dec W$NumberOfSections
    End_If
    sub eax D$AppStartOfExp | add D$AppAllDataSize eax

    Move eax D$ExportSectionLen , D$AppExpTrueSize eax
    AlignOn 0200 eax | Mov D$AppExpAlignedSize eax

    Mov eax D$AppCodeRVAoffset
    add eax D$AppFileSizeOfCode | AlignOn 01000 eax | Mov D$AppBaseOfExp eax

    add eax D$AppExpAlignedSize | AlignOn 01000 eax

    Move D$SectionTable D$AppBaseOfExp
    Move D$SectionTable+4 D$AppExpAlignedSize

  ; For ease of 'FillExportSection' job:
    Mov eax D$AppBaseOfExp | sub eax D$ExportListBPtr | Mov D$ExportAjust eax

    Mov eax D$ExportSectionLen | AlignOn 0200 eax | Mov D$FileAlignedExportSectionLen eax
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

    Call 'KERNEL32.WriteFile' D$H.Destination, D$CodeList, D$LenOfCode,
                              NumberOfReadBytes, 0

    If B$ExportsectionWanted = &TRUE
        Call 'KERNEL32.WriteFile' D$H.Destination, D$ExportListBPtr,
                                  D$FileAlignedExportSectionLen, NumberOfReadBytes, 0
    End_If

    If D$SavingExtension = '.SYS'
        jmp L1>
    Else_If D$FL.RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
L1:
        If D$RelocSectionSize => 8 ;jE!
          Call 'KERNEL32.WriteFile' D$H.Destination,
                                   D$Relocation, D$FileAlignedRelocationSize,
                                   NumberOfReadBytes, 0
        End_If
    End_If

    Call 'KERNEL32.WriteFile' D$H.Destination, D$CodeSource, D$SourceLen,
                              NumberOfReadBytes, 0

    Call 'KERNEL32.CloseHandle' D$H.Destination | Mov D$H.Destination 0

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
        
        Call 'KERNEL32.WriteFile' D$H.Destination, D$Trash, D$SourceLen,
                              NumberOfReadBytes, 0
hexprint D$SourceLen
        Call 'KERNEL32.CloseHandle' D$H.Destination | Mov D$H.Destination 0
        
        Call 'USER32.MessageBoxA' D$H.MainWindow, {B$ "
A bad pairing of CR/LF has been found in the Source,
at Write-Time. The problem is fixed, but RosAsm is
going to shut down, and you will have to Re-Load.

Do not care, then, of the Alert Message for corruption.

Sorry for the inconvenient.

In case you could point out what action(s), in the Editor,
could help reproducing these wrong CR/LFs, please report.
" EOS}, 
                                     {B$ 'Source failure' EOS}, 0
        
        Call 'KERNEL32.ExitProcess' 0
        
    ..End_If
ret
;;

SaveBookMarks:
    Mov edi DestinationFile, ecx 0-1, al 0 | repne scasb
    While B$edi <> '.'
        dec edi
    End_While

    Push D$edi, edi
        Mov eax '.BKM' | stosd

        Call OpenDestinationFile

        Mov edi D$BookMarks, al 0, ecx 0-1
L0:     repne scasb | cmp B$edi 0 | ja L0<
        sub edi D$BookMarks | inc edi                    ; edi = lenght.

        Call 'KERNEL32.WriteFile'  D$H.Destination, D$BookMarks, edi,
                                   NumberOfReadBytes, 0

        Call 'KERNEL32.CloseHandle' D$H.Destination | Mov D$H.Destination 0
    Pop edi, D$edi
ret


Proc StoreChoosenName:

    Argument @STR.A.Name

    Uses esi,
         edi

    Call ClearPATH MainName

    Mov edi MainName,
        esi D@STR.A.Name

L1: lodsb | stosb | Test al 0_FF NOT_ZERO L1<

L2: sub edi ASCII | Comp B$edi '.' <> L2< ; '.' Can be inside the name / '.' peut aussi se trouver dans le nom

    Mov B$edi EOS

L3: sub esi ASCII | Comp B$esi '.' <> L3<

    ; TODO traduction
    ; Eax = Suffixe en minuscules
    lodsd | or eax 0_20_20_20_00

    mov D$SavingExtension eax

;;
  
    If eax = '.exe'
    
    
    Else_If eax = '.scr'
    
    Else_If eax = '.asm'
    
    Else_If eax = '.dll'
    
    Else_If eax = '.sys'
    
        Move D$SavingExtension D$SysExtension
    
    Else
    
        INT3 | NOP ; TODO :) Prévoir un message ...
    
    End_If
;;

EndP

[SavingExtension: D$ '.exe']

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


[TestName: B$ 'Test.a' EOS]

SetTestSavingName:
    Mov edi DestinationFile, ecx 262, al 0 | rep stosb
    Mov edi DestinationFile, esi TestName
L0: lodsb | stosb | cmp al 0 | ja L0<
    Mov B$edi 0
ret


[DefaultFileName: B$ 'PE.' EOS]


; Revue: Does not work if the Path contains some '.' Period in a Folder Name (!!!!)....

[FullPartName: B$ ? # &MAX_PATH]

SetAsmSavingName:

    Call ClearPATH DestinationFile

    If B$WeAreSavingPart = &TRUE

        Mov esi MainName, edi FullPartName, ecx (&MAX_PATH/DWORD) | rep movsd | dec edi

        While B$edi = EOS | sub edi (1*ASCII) | End_While

L0:     cmp B$edi '\' | je L1>

        cmp B$edi ':' | je L1>

        sub edi (1*ASCII) | jmp L0<

L1:     add edi (1*ASCII)

        Mov esi PartName

        While B$esi > EOS | movsb | End_While | movsb

        Mov esi FullPartName

    Else
        Mov esi MainName

    End_If

    Mov edi DestinationFile

    If B$esi = EOS

        Mov ax W$DefaultFileName | stosw | jmp L9>

    End_If

L0: lodsb | cmp al EOS | je L9>
           ; cmp al '.' | je L9>
    stosb | jmp L0<

L9: On B$edi-(4*ASCII) = '.' sub esi (4*ASCII)  ; BUAsm outputs are always with 3 Chars Extensions

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
        Push ecx, esi, edi
            ;dec esi |
            Mov D$imm32 0 | Call NewSearchForWinEquate
        Pop edi, eax, ecx
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
    Move D$StatementsPtr D$StatementsTable, D$StatementsPtr2  D$StatementsTable2

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

    Move D$StatementsPtr D$StatementsTable

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
    Push esi
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

L9: Pop esi
ret


FillStatementsTable2:
    Push eax, ebx
        Mov eax D$StatementsPtr, eax D$eax
        Mov ebx D$StatementsPtr2, D$ebx eax
        add D$StatementsPtr2 4
    Pop ebx, eax
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

;;

  Parser of the Parameters being declared as Macros (or direct Data with Automatic Labels).
  Example:
  
  > [Return | mov eax #1]

  > Return {RGB 0_C1 0_C2 0_C3 0_A0} -> Unfold -> MOV EAX 0A0C3C2C1

;;

[ParaMacrosMaxLevel: D$ ?
 ParaMacroLevel: D$ ?
 MacroModifiedExpression: D$ ?]

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
                Push edx
                    inc esi     ; Strip 'OpenParaMacro' Char.

                    If B$esi = TextSign
                        Call UnfoldDataParameter

                    Else_If B$esi+1 = memMarker
                        Call UnfoldDataParameter

                    Else
                        Call UnfoldMacroParameter

                    End_If

                    Mov B$MacroJobIsOver &FALSE
                Pop edx

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

    Push esi
        inc esi | While B$esi <> CloseParaMacro | inc esi | End_While | inc esi

        While esi < edx | movsb | End_While
        While B$edi-1 = EOI | dec edi | End_While

        Mov B$edi meEOI, B$edi+1 '{' | add edi 2
        Call WriteNoMeanLabel | Mov B$edi ColonSign | inc edi
    Pop esi

    While B$esi <> CloseParaMacro | movsb | End_While
    Mov B$edi '}', B$edi+1 EOI, B$edi+2 EOI | add edi 3

    Mov esi edx
ret

UnfoldMacroParameter:
    Push D$Striplen
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

        Push esi
            Push edi
                Call GetFromQwordCheckSum esi, D$MacroList, D$MacroListLimit
                On eax = 0, error D$ParaMacroPtr

                While B$eax > LowSigns | inc eax | End_While | inc eax
                Mov esi D$eax, ecx D$eax+4

            Pop edi

            Mov D$InstructionBptr edi
            Call ReplaceFromMacroData

        Pop esi

        While B$esi <> EOI | inc esi | End_While | inc esi

    Pop D$Striplen
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;
; Mov eax (((12*3) xor 001010101)+2)

[InsideExpression: D$ ?
 StartOfSourceExpression: D$ ?
 StartOfDestinationExpression: D$ ?
 RealExpression: D$ ?
 RealHexaSize: D$ ?]

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
                Push edx
                    If B$RealExpression = &TRUE
                        Call ComputeRealExpression
                    Else
                        Call ComputeExpression
                    End_If
                    Call WriteExpresionResult
                    Mov ebx 0, ecx 0, B$InsideExpression &FALSE
                Pop edx
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
    Push ebx
    Push eax, esi
      ; Is there any Parenthesis???
;;
        While esi < edx
            On B$esi = OpenSign, jmp L1>
            inc esi
        End_While
        jmp L9>>

      ; OK, OpenSign Inside. Is there any Real Marker???
L1:     Pop esi | Push esi
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
L1:     Pop esi | Push esi
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

                Push esi
                    inc esi
                    While B$esi > PartEnds
                        inc esi
                    End_While
                    .If B$esi = ColonSign
                        On B$esi+2 <> MemMarker, error D$MarkerAfterPtr
                    .Else_If B$esi <> MemMarker
                        Error D$MarkerAfterPtr
                    .End_If
                Pop esi

            ...End_If
L8:         inc esi
        .End_While

L9: Pop esi, eax
    Pop ebx
ret


[ExpressionResult: D$ ? ?
 Operator: D$ ?]

[ExpressionA: D$ ?
 ExpressionB: D$ ?]

[ExpressionAPtr: D$ ?
 ExpressionBPtr: D$ ?]

[ExpressionALimit: D$ ?
 ExpressionBLimit: D$ ?]

InitExpressionBuffers:

    Call VirtualAlloc ExpressionA,
                      PAGE_SIZE

    add eax 0800

    Mov D$ExpressionALimit eax,
        D$ExpressionAPtr eax

    Call VirtualAlloc ExpressionB,
                      PAGE_SIZE

    add eax 0800

    Mov D$ExpressionBLimit eax,
        D$ExpressionBPtr eax

ret

; Main parsing of one Expression. Calls for storage (in Binary) and for re-write after
; computation, from the more inside level to first one:

ComputeExpression:

    Push esi

        Mov ecx esi, esi D$StartOfSourceExpression, edi D$ExpressionA
        dec esi | sub ecx esi | rep movsb            ; copy the Expression to 'ExpressionA'.
        Mov al 0 | stosb

L0:     Mov D$ExpressionResult 0, D$ExpressionResult+4 0, B$Operator OpenSign
        Mov esi D$ExpressionA, edi D$ExpressionB, edx 0

        Push ebx
L1:         lodsb | stosb | cmp al OpenSign | jne L1<
                dec ebx | jnz L1<
        Pop ebx

        If edi > D$ExpressionBLimit

            Call ExtendTableMemory ExpressionA,
                                   ExpressionAPtr,
                                   ExpressionALimit

            Call ExtendTableMemory ExpressionB,
                                   ExpressionBPtr,
                                   ExpressionBLimit

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
            Move D$Operator D$esi | Mov B$Operator al | inc esi

        ...Else_If al <= '9'
            Push ebx
                If ax = '00'
                    Call TranslateBinary | dec esi | Call StoreOnExpression
                Else_If al = '0'
                    Call TranslateHexa | dec esi | Call StoreOnExpression
                Else
                    and B$esi 00_0111_1111
                    Call TranslateDecimal | dec esi | Call StoreOnExpression

                End_If
            Pop ebx

        ...Else
            jmp L9>> ;error D$ExpressionMemberPtr

        ...End_If

        cmp B$esi-1 CloseSign | jne L2<<
            Call ReWriteExpression
            dec ebx | jnz L0<<
L9: Pop esi
    ret


[ExpressionReal: R$ ?
 RealExpressionResult: R$ ?]

ComputeRealExpression:
    On ebx > 1, error D$RealNotationPtr

    Push esi
        Mov ecx esi, esi D$StartOfSourceExpression, edi D$ExpressionA
        dec esi | sub ecx esi ;| rep movsb          ; copy the Expression to 'ExpressionA'.
      ; Since the Expression parser has moved in between the Equates and Macros jobs,
      ; we have to mask out the Byte HighBit (Done Flag):
L0:     lodsb | and al 00_0111_1111 | stosb | loop L0<
        Mov al 0 | stosb


L0:     Mov D$RealExpressionResult 0, D$RealExpressionResult+4 0, B$Operator OpenSign
        Mov esi D$ExpressionA, edi D$ExpressionB, edx 0

        Push ebx
L1:         lodsb | stosb | cmp al OpenSign | jne L1<
                dec ebx | jnz L1<
        Pop ebx

L2:     On B$esi = Space, inc esi

        ...If B$esi < '0'
            Move D$Operator D$esi | inc esi

        ...Else_If B$esi <= '9'
            Push ebx
                Mov edi ExpressionReal | Call atof      ; Result in ST0
                dec esi
                Call StoreOnRealExpression
            Pop ebx

        ...Else
            error D$RealNotationPtr

        ...End_If

        cmp B$esi CloseSign | jne L2<<
            Call ReWriteRealExpression
            dec ebx | jnz L0<<

L9: Pop esi
ret

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


[StartOfHexaExpression: D$ ?]

ReWriteExpression:
    Push ebx
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
    Pop ebx
ret


ReWriteRealExpression:
    Push ebx
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
    Pop ebx
ret


; True final result rewrite in source:

WriteExpresionResult:
    Push esi
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
  
        Push edi
            Mov B$edi 0
            Mov edi D$StartOfDestinationExpression
            sub edi 3
            howme edi
            exprint D$edi
        Pop edi
;;
    Pop esi
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

[ListsLength: D$ ?]

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
  - and not so predictable - size (AlignOn 01000).
;;
    add D$StripLen 01000

    Move D$ListsLength D$StripLen
    AlignOn 01000 D$ListsLength

    Call VirtualAlloc LabelList,
                      D$StripLen

    Mov edi D$LabelList | Mov eax 0 | stosd
    Mov al EOI | stosb | Mov D$LabelListPtr edi
    add edi D$StripLen | sub edi TABLES_SECURITY | Mov D$LabelListLimit edi

    Call VirtualAlloc EquateList,
                      D$StripLen

    Mov edi D$EquateList | Mov eax 0 | stosd
    Mov al EOI | stosb | Mov D$EquateListPtr edi
    add edi D$StripLen | sub edi TABLES_SECURITY | Mov D$EquateListLimit edi

    Call VirtualFree EquateSubstitutes

    Call VirtualAlloc EquateSubstitutes,
                      D$StripLen

    Mov edi D$EquateSubstitutes | Mov D$EquateSubstitutesPtr edi
    add edi D$StripLen | sub edi TABLES_SECURITY
    Mov D$EquateSubstitutesLimit edi
    Move D$LastEquateList D$EquateList
    Move D$LastEquateListPtr D$EquateListPtr

    Call VirtualAlloc MacroList,
                      D$StripLen

    Mov edi D$MacroList | Mov eax 0 | stosd
    Mov al EOI | stosb | Mov D$MacroListPtr edi
    add edi D$StripLen | sub edi TABLES_SECURITY | Mov D$MacroListLimit edi

    Call VirtualAlloc MacroData,
                      D$StripLen

    Move D$MacroDataPtr D$MacroData
    Mov eax D$MacroData | add eax D$StripLen | sub eax TABLES_SECURITY
    Mov D$MacroDataLimit eax

    sub D$StripLen 01000
ret


InitIndex2:
  ; Don't supress reloc table: Pointers needed for setting of other values:
    Call VirtualAlloc Relocation,
                      D$StripLen

    Move D$RelocationPtr D$Relocation

    Call VirtualAlloc ApiListA,
                      D$StripLen

    Move D$ApiListAPtr D$ApiListA

    Call VirtualAlloc ApiListB,
                      D$StripLen

    Move D$ApiListBPtr D$ApiListB

    Call VirtualAlloc DllList,
                      D$StripLen

    Move D$DllListPtr D$DllList

ret


InitIndex3:

    Call VirtualFree DllList

    Call VirtualFree ApiListB

    Call VirtualFree ApiListA

    Call VirtualAlloc CodeRef,
                      D$StripLen

    Mov edi D$CodeRef | Mov eax 0 | stosd
    Mov al EOI | stosb | Mov D$CodeRefPtr edi

    Call VirtualAlloc DataRef,
                      D$StripLen

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

[LoadAndClear | lodsb | mov B$esi-1 0]
 _______________________________________________________________________________________

; MacroList begin with a Dword for lenght of table followed by macro items.
 _______________________________________________________________________________________

[LastOctetInSource: D$ ?]

StoreEquates:
    Mov edi D$EquateListPtr,  B$esi-1 0FF

    If edi > D$EquateListLimit
        Call ExtendTableMemory EquateList, EquateListPtr, EquateListLimit
        Mov edi D$EquateListPtr
    End_If

L0: Push esi | Mov ebx 0
L1:     lodsb | inc ebx | cmp al LowSigns | ja L1<
    Pop esi | dec ebx | ;Call IsItNewEquate  ; with lenght in ebx

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

_______________________________________________________________________________________

StoreMacros:
    Mov edi D$MacroListPtr,  B$esi-1 0

    If edi > D$MacroListLimit
        Call ExtendTableMemory MacroList, MacroListPtr, MacroListLimit
        Mov edi D$MacroListPtr
    End_If

    Push esi
        Mov ebx 0
L0:     lodsb | inc ebx | cmp al LowSigns | ja L0<
    Pop esi
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
    If eax = FLAG_DONE
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
[FirstPassTest: D$ ?]

StoreAllEquates:
    Mov B$FirstPassTest 0

    Mov esi D$CodeSourceA,  ecx D$StripLen | add ecx esi | inc ecx  ; ecx = max esi value
    Mov D$LastOctetInSource ecx,  B$ErrorLevel 0,  D$bracketCounter 0
    Move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4

L0: lodsb | Call IsItText | je L0<
        cmp al EOI | je L9>>                        ; EOI >>> no more brackets.
    cmp al OpenVirtual | jne L1>
        inc D$bracketCounter
        add D$StatementsPtr 4 | jmp L0<
L1: cmp al Openbracket | jne L0<                    ; loop until '['
        inc D$bracketCounter
        add D$StatementsPtr 4
        Push esi
        cmp B$esi '<' | jne L2>                     ; Data Alignement?
       ; cmp B$esi+1 '0' | jb L2>
       ; cmp B$esi+1 '9' | ja L2>
        or B$esi 00_1000_0000  ; to prevent ReplaceEquates from replacing '[<' by '[B' in
                               ; case user define an Equate for '<' Char (not reserved Char)
        Pop eax | jmp L0<
L2:     lodsb | cmp al, OpenParaMacro | ja L2<
            Mov ecx esi                             ; for end reached test
        Pop esi
    cmp al ColonSign | jne L3>                      ; data?
        Mov esi, ecx | jmp L0<
L3: cmp al Space | jne L3>                          ; equate?
        Call StoreEquates
            Mov eax D$StatementsPtr, D$eax FLAG_DONE
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
    Move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4

L0: lodsb | Call IsItText | je L0<
    cmp al EOI | je L9>                             ; EOI >>> no more brackets.
    cmp al OpenVirtual | jne L1>
        inc D$bracketCounter
        add D$StatementsPtr 4 | jmp L0<
L1: cmp al 0FF | je L8>
    cmp al Openbracket | jne L0<                    ; loop until '['
    cmp B$esi '<' | je L0<
L7:     inc D$bracketCounter
        add D$StatementsPtr 4
        Push esi
L2:         lodsb | cmp al, LowSigns | ja L2<
            Mov ecx esi                             ; for end reached test
        Pop esi

L3: cmp al meEOI | jne L0<                          ; macro?
        On B$esi = meEOI, error D$UnexpectedCRLFPtr
        Call storeMacros
            Mov eax D$StatementsPtr, D$eax FLAG_DONE
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

[SortTempoMemory: D$ ?]

;;
 Source and Destination are Pointers to 2 Tables (same lenght). Source holds a set
 of zero ended strings to be sorted and stored in Destination. Source is overwritten
 with 0FF Bytes when finished.
;;
[SortSource: D$ ?
 SortDestination: D$ ?
 SortStringNumber: D$ ?]


; Pointers by Sizes: Each dWord will be (possibily -0, if none-) a Pointer to the
; position of the first Equate Equate in the List having the 'given' number of Chars.
; ('given' by the Routine that will finally *search* for the Equate body:
[LabelsPointersBySizes: @EquatesPointersBySizes: D$ ? # 100]

[SortBySizeOver: D$ ?]

; input: EBX = lenght of researched label set by caller; >ESI > first letter of new label

[SearchMain: D$ ?]
SearchForEntryPoint:
    Mov B$SearchMain &TRUE
        Call GetFromQwordCheckSum EntryPointLabel, D$LabelList, D$LabelListLimit
        If eax = 0
            Mov B$ErrorLevel 9
            error D$NoEntryPtr
        End_If

        While B$eax <> EOI | inc eax | End_While | inc eax
        or B$eax+4 FLAG_DONE
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

[OneWordLen: D$ ?]

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

[AfterLastParameter: D$ ?
 NumberOfMacroParameters: D$ ?
 LastMacPara: D$ ?
 FirstUnfolded: D$ ?
 LastUnfolded: D$ ?
 UnfoldingRotations: D$ ?]

SearchLastParameter:
    Push esi
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
    Pop esi
ret


[NoLabel: D$ ?
 NoSizeMarker: D$ ?]

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
    Push ecx, esi
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
                Pop eax, ecx
                    inc eax | dec ecx
                Push ecx, eax
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

L9: Pop esi, ecx
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

    Push esi, ecx
        Mov esi D$InstructionAptr              ; switch from MacroData to clean Code Source
        xchg al bl | cmp al bl | ja L2>

L1:     Call UnfoldOneOfMultiPara | inc al | cmp al bl | jna L1<
            jmp L9>

L2:     Call UnfoldOneOfMultiPara | dec al | cmp bl al | jna L2<

L9: Pop ecx, esi
ret


UnfoldOneOfMultiPara:
    Push eax ebx
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
                error {"| wanted between Multiple Parameters and '}', in Macro Declaration" EOS},
                      D$InstructionAptr
            End_If
          ; write parameter in destination
            stosb | jmp L2<
L3:         dec D$AfterLastParameter
      ; Space forced (avoid '|' when reading, at first, the last parameter)
L8:     Mov al space | stosb
L9: Pop ebx eax
ret
;;
; I do not recall the reason why i did this strange '}' thingie, previously:
L2:     lodsb
        cmp al, Separators | jb L9>
        cmp al '}' | je L3>
          ; write parameter in destination
            stosb | jmp L2<
L3:         dec D$AfterLastParameter
      ; Space forced (avoid '|' when reading at first the last parameter)
L9:     Mov al space | stosb                   
    Pop ebx eax
ret
;;

;;
  These are the 128 Bytes Tables for storing whatever the user wants to store:
  The internal macros Variables, &0, &1,... , &99:
;;

[MacrosVariablesTable: D$ ? # (32*102)]
[MacroCounters: D$ ? # 100]
[MACRO_VARIABLES 100]

ClearMacroVariable:
    Mov edi MacrosVariablesTable, ecx ((32*102)+100), eax 0 | rep stosd
ret


[MixedInternalVariables: B$ 'You cannot mix Internal Variables by text and by Number' EOS]
[CounterSyntax: B$ "Macros Counters syntax example:
&&21=&&21+1 // &&3=&&60-1 // &&5=0" EOS]

StoreMacroVariableByNumber: ; StoreMacroVariable
  ; eax = Displacement to the MacroVariable Record, inside 'MacrosVariablesTable'

  ; esi is pointing after '&&23='

  ; Macros Variables Declarations are not true Statements.
  ; So, we have to strip the leading '|' (meEOI) previously written:
    ;dec edi

    Push edi
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
                Move D$edi D$NumberOfMacroParameters
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
L9: Pop edi
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


[CounterAttribution: B$ 'Bad Number Attribution for internal Counter' EOS]

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
    Push eax
L0:     If B$esi < '0'
            Pop eax | ret
        Else_If B$esi <= '9'
            shl ebx 4 | Mov al B$esi | sub al '0' | or bl al
        Else_If B$esi < 'A'
            Pop eax | ret
        Else_If B$esi <= 'F'
            shl ebx 4 | Mov al B$esi | sub al '0' | sub al 7 | or bl al
        Else
            Pop eax | ret
        End_If
        inc esi | dec ecx | jnz L0<
    Pop eax
ret


GetAttributionDecimal:
    Push eax
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

    dec esi | Pop eax
ret

[CounterTextAttribution: B$ 'Max Text Attribution is 4 Chars, for a Counter' EOS]

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
B$ 'Only "0" to "9" or "A" to "Z" accepted.', "
Bad writing attempt with Counter &&"
 WritingCounter: 0 0 0] ; !!! alignement

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
    Call WriteEaxDecimal | Mov B$edi EOS
    error WritingLowCounter
ret


StoreMacroVariable:
  ; eax = Displacement to the MacroVariable Record, inside 'MacrosVariablesTable'

  ; Macros Variables Declarations are not true Statements.
  ; So, we have to strip the leading '|' (meEOI) previously written:
    On B$edi-1 = meEOI, dec edi

    Push edi
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
    Pop edi
ret


WriteMacroVariable:
    Push esi, ebx
        lea esi D$MacrosVariablesTable+eax
        Mov ebx edi | add ebx 80

        If B$esi = 0
         ; This is commented out because it outputs NOPEs every now and then when unwished.
         ; I don't find example, when it could be a problem for Conditional Macros...

         ; Mov D$edi 'NOPE' | add edi 4
           Pop ebx, esi | ret
        End_If
        If B$esi = Space
           inc esi
           ; jE! skip Space, if is 1st byte!
        End_If
        .While B$esi > 0
            If B$esi = '&'
                inc esi
                Push ecx
                    Call GetMacroVariableDis | shl eax 7
                    On B$esi = '=', error D$NestedMacroVariablePtr

                    Push esi
                        Call WriteMacroVariable
                    Pop esi
                Pop ecx
            Else
                movsb | On edi = ebx, error D$MacVarOverFlowPtr
            End_If
        .End_While

    Pop ebx, esi
ret

____________________________________________________________________________________________

[NoMeanLabel: B$ 'ZZZZZZZZ' EOS]
[NoMeanEaten: D$ 0]

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
    Push edi
        Mov edi NoMeanLabel | add edi 7
        dec B$edi
        While B$edi < 'A'
            Mov B$edi 'Z' | dec edi | dec B$edi
        End_While
    Pop edi
ret


; Write the Automatic Label at edi (without trailing Colon):

WriteNoMeanLabel:
    Push esi, ecx
        Mov esi NoMeanLabel, ecx 8 | rep movsb
    Pop ecx, esi
ret

____________________________________________________________________________________________

; Writes al to edi in Decimal form:

AlToDecimal:
    Push ecx, eax
        Mov ecx 10
        Push 0-1
L1:     Mov ah 0 | div cl | add ah '0' | Push eax | cmp al 0 | ja L1<

L2:     Pop eax
        If eax <> 0-1
            Mov B$edi ah | inc edi | jmp L2<
        End_If
    Pop eax, ecx
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
    Push ebx
        Mov eax 0, ebx 0

L0:     lodsb | sub al '0' | add ebx eax
        cmp B$esi '0' | jb L9>
        cmp B$esi '9' | ja L9>
          ; ebx = ebx*10:
            lea ebx D$ebx*4+ebx | shl ebx 1 | loop L0<

L9:     Mov eax ebx
    Pop ebx

  ; eax = &Variable Indice from 1 to 100:
    On eax > MACRO_VARIABLES, error D$MacroVariableIndicePtr

  ; eax = Displacement to the proper 128 Byte Record:
  ;  shl eax 7 ; Done by caller
ret


[MacrosX: D$ ?]

ReplaceFromMacroData:
    Call SearchLastParameter

    Mov edi D$InstructionBptr

    Mov B$MacrosX 0, B$FirstUnfolded &FALSE, B$LastUnfolded &FALSE, B$NoMeanEaten &FALSE

X0: Push ecx, esi
      ; read data from MacroData:
L0:     lodsb

        ...If al = NumSign
          ; "#" Parameter, > Unfolding:
            Call ParseMacroParameter

            If ecx = 0-1
                Pop esi, ecx | jmp X0<
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
L9: Pop esi, ecx                                 ; true stack restore
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
;        [ Mov | mov #2 #1 ]
;          Mov eax, ebx      ; >>>>  Mov ebx, eax ... and so on.
;
; The only one solution i choose is to stop unfolding after a certain iterations number:
;;

[MacroJobIsOver: D$ ?]

NestingCheck:
    Push esi, ecx, eax, edi
        Mov esi D$InstructionAptr, edi D$InstructionBptr, ecx 0

        Mov ah B$esi-1        ; exemple: with  [Api | Push #L>2 | Call #1], an
        Mov al B$edi-1        ; api Call without parameter will be authorized :
        cmp ax ((EOI shl 8)+meEOI) | jne L0>
        ;cmp ax 0201 | jne L0> ; no error message. Compute > (02h)Call(03h)Function(02)
            Mov B$edi-1 EOI    ; and not:                    (01h)Call(03h)Function(02)

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
L9: Pop edi, eax, ecx, esi
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
    test B$esi 00_1000_0000 NOT_ZERO L6>>           ; to avoid testing words not in list
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
L2:             test B$esi 00_1000_0000 NOT_ZERO L1<

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
                    Push esi
                        Mov esi D$eax, ecx D$eax+4
                        rep movsb
                    Pop esi

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
                Push edx
                    If B$RealExpression = &TRUE
                        Call ComputeRealExpression
                    Else
                        Call ComputeExpression
                    End_If
                    Call zWriteExpressionResult
                    Mov ebx 0, ecx 0, B$InsideExpression &FALSE
                    Mov B$MacroModifiedExpression &TRUE
                Pop edx
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
    Push esi
        Mov esi D$ExpressionA, edi D$StartOfDestinationExpression

        While B$esi > 0
            movsb
        End_While
    Pop esi
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

[InstructionA: D$ ?
 InstructionB: D$ ?
 InstructionAEnd: D$ ?
 InstructionBEnd: D$ ?
 CodeSourceBpointer: D$ ?
 CodeSourceApointer: D$ ?
 OpenType: D$ ?
 CloseType: D$ ?]

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

    Call VirtualAlloc InstructionA,
                      D$StripLen

    Mov eax D$InstructionA | add eax D$StripLen | AlignOn 01000 eax | Mov D$InstructionAEnd eax

    Call VirtualAlloc InstructionB,
                      D$StripLen

    Mov eax D$InstructionB | add eax D$StripLen | AlignOn 01000 eax | Mov D$InstructionBEnd eax

    Mov B$ErrorLevel 0, D$BracketCounter 0

    Move D$StatementsPtr D$StatementsTable,
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

[LenghtOfBracketStatements: D$ ?]

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

    Push edi
        inc edi | sub edi D$CodeSourceA | Mov D$StripLen edi
    Pop edi
    Mov eax ((EOI shl 24)+(EOI shl 16)+(EOI shl 8)+EOI)  | stosd   ; security

    Call VirtualFree InstructionA

    Call VirtualFree InstructionB

    Call ClearDoneHighBit

ret

____________________________________________________________________________________________
;;
  'NewBrackets' makes nested Declarations of '[...]' available for next Equates and
  Macros job pass. We have to replace all substitutes by the true signs ('{' > '[',
  and so on). But we have a difficult problem to solve first: These new Declarations
  will be Move to top of 'file' by next treatements, and in case of user error search
  among these new created statements, we have to be able to point out the wrong
  original Statement (the user macro Evocation). So have we to add some source pointers
  in 'StatementsTable', with the same pointer value as the one of user Macro. One
  added difficulty is that, is some case, exemple:

  > [CreateEquatesMacro | {#1 #2} | +2]

  ... the original Macro was stored as a Code Statement into 'StatementsTable'. After
  unfolding, there is no more code at this place. So have we to strip one record for
  this, in such cases:
;;
[MoreBracket: D$ ?
 InsideNewBracket: D$ ?
 TrueCodeInside: D$ ?]

NewBrackets:
    Mov esi D$CodeSourceA
    Mov ecx esi | add ecx D$Striplen                    ; end of source adress

    Move D$StatementsPtr D$StatementsTable, D$StatementsPtr2 D$StatementsTable2

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


[AlignOn | add #2 #1-1 | and #2 0-#1]
; Same for Aligning on a Variable-defined-Alignment (eax broken):
[Align_on_Variable | Push eax
                         Mov eax #1 | dec eax | add #2 eax | xor eax 0-1 | and #2 eax
                     Pop eax]


[DllNumber: D$ ?
 FunctionNumber: D$ ?
 ImportHeaderPtr: D$ ?
 ImportTablePtr: D$ ?]


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
    Push esi
      Mov ecx 0,  ebx 0                       ; ebx = '.' counter (must be 0 / 1 / or 2)
      Push esi
        While B$esi <> TextSign | On B$esi = '.', inc ebx | inc esi | End_While
      Pop esi
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
          Pop esi
              While B$esi > TextSign
                  movsb
              End_While
              Mov al 0 | stosb
              Mov esi D$ApiListA
              Error D$BadApiPtr

L9: Pop esi
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
L0: ...If D$esi = 'Call'        ; Call '
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
            {B$ "Are you sure you want to continue assemblying this?" EOS},
            D$NoApiPtr, &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION__&MB_YESNO
            If eax = &IDNO

                Mov D$FL.CompileErrorHappend &TRUE,
                    D$NextSearchPos 0

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


[WasSysModule: WasHALdll: D$ ?]

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
[CopyOfFunctionName STR.A.Trash] ;: ? #32]

AraseSameFunctions:
  ; Now, arase all other evocations of 'MODULE.ext.Function', 'MODULE.Function',
  ; 'Function', in 'ApiListA':
    Push esi, edi, edx

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
                Push ebx, esi
                    dec esi | Mov edi CopyOfFunctionName
L2:                 Mov al B$esi, bl B$edi
                    On al <> bl, jmp L7>>
                    On al = 0, jmp L5>
                    On bl = 0, jmp L5>
                    inc esi | inc edi | jmp L2<
L5:                 or al bl
                    ..If al = 0               ; Same > arase:
                        Mov edx esi
                        Pop esi | Push esi
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
L7:             Pop esi, ebx
            ...End_If

        .End_While
    Pop edx, edi, esi
ret


IsItNewDll:
    Push edi
      Mov edi D$DllList | Mov ebx 0 | jmp L1>
L0:   Mov al 0,  ecx 0FF
        repne scasb
      cmp B$edi ALEOD | jne L1>
        Mov ebx 0 | jmp L9>
L1:     Push esi edi
          Mov ecx 0FF,  eax 0 | inc ebx
            repe cmpsb
          Mov al B$edi-1,  ah B$esi-1
        Pop edi esi
        cmp eax 02E00 | jne L0<         ; ('.' in ApiListA, 0 in DllList)
L9: Pop edi
ret                                     ; >>> ebx = Dll #n or 0


IsItNewFunction:       ; edi > ApiListB end; esi > ApiListA function name; al > dll #n
    Push edi
      Mov bl al | Mov edi D$ApiListB | jmp L1>
L0:   Mov al 0,  ecx 0FF
        repne scasb
      cmp B$edi ALEOD | jne L1>             ; reach end ?
        Mov al bl | jmp L9>
L1:   cmp B$edi bl | jne L0<
        inc edi
        Push esi edi
          Mov eax 0
L2:       Mov al B$edi,  ah B$esi
          inc edi | inc esi
          cmp eax 0 | je L3>
          cmp ah al | je L2<
L3:     Pop edi esi
        cmp eax 0 | jne L0<
L9: Pop edi
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

L1: Push edi
        Mov edi D$DllListPtr
      ; write a new "Dll" name (with Extension):
L2:     lodsb | stosb | cmp al '.' | jne L2<
        While B$esi <> '.' | movsb | End_While
        inc esi
        Mov al 0 | stosb | Mov B$edi ALEOD                     ; end mark
        Mov D$DllListPtr edi | inc D$DllNumber | Mov eax D$DllNumber
    Pop edi
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
[H.Dll: D$ ?
 StartOfDllName: D$ ?
 StartOfFunctionName: D$ ?
 RosAsmDLL: D$ ?]

[ModuleHandlesList: D$ ? # 100]
[ModuleHandlesListPointer: D$ ?]

VerifyModuleExist:
    pushad
        Mov esi D$StartOfDllName

        Call TryToLoadModule '.dll'
        If eax <> &NULL
            Mov D$H.Dll eax | popad | Mov eax '.dll' | ret
        End_If

        Call TryToLoadModule '.sys'
        If eax <> &NULL
            Mov D$H.Dll eax | popad | Mov eax '.sys' | ret
        End_If

        Call TryToLoadModule '.drv'
        If eax <> &NULL
            Mov D$H.Dll eax | popad | Mov eax '.drv' | ret
        End_If

        Call TryToLoadModule '.exe'
        If eax <> &NULL
            Mov D$H.Dll eax | popad | Mov eax '.exe' | ret
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

        Mov D$H.Dll eax
    popad
ret


[ExeSysDrvModuleName: D$ ? # 20]

Proc TryToLoadModule:
    Argument @Ext

        Mov esi D$StartOfDllName, edi ExeSysDrvModuleName
        While B$esi <> 0 | movsb | End_While
        Mov B$edi 0
        Move D$edi D@Ext | Mov B$edi+4 0

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
[ModuleFullPath: B$ ? # &MAX_PATH]

Proc TryModuleFromAppDirectory:

    Argument @Module

    Uses edi,
         esi

    Mov edi ModuleFullPath,
        esi MainName

    While B$esi <> EOS | movsb | End_While

    While B$edi-1 <> '\' | sub edi (1*ASCII) | End_While

    Mov esi D@Module

    While B$esi <> EOS | movsb | End_While | movsb

    Call 'KERNEL32.LoadLibraryA' ModuleFullPath

;;

    If eax = 0
        ; Call 'KERNEL32.GetLastError'
        ; ---> &ERROR_NOACCESS 
            
        Call 'KERNEL32.CreateFileA' ModuleFullPath,
                                    &GENERIC_READ,
                                    &FILE_SHARE_READ+&FILE_SHARE_WRITE,
                                    0,
                                    &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_NORMAL,
                                    0
            
    End_If

;;

EndP


[Aerror: D$ ?]

VerifyFunctionExist:
    pushad
        Mov eax D$StartOfFunctionName

        If B$eax = '0'
            Mov esi eax | Call TranslateHexa
            Call 'KERNEL32.GetProcAddress' D$H.Dll, eax
            On eax = &NULL, error D$BadOrdinalPtr
        Else
            Call 'KERNEL32.GetProcAddress' D$H.Dll, D$StartOfFunctionName
        End_If

        .If eax = &NULL
            Mov edi D$StartOfFunctionName, al 0, ecx 0FFFF | repne scasb
            sub edi 2
            Push D$edi, edi
                If B$edi = 'A'
                    Mov B$Aerror &FALSE
                Else
                    inc edi | Mov al 'A' | stosb | Mov B$Aerror &TRUE
                End_If
                Mov al 0 | stosb

              ; test with ending 'A'
                Call 'KERNEL32.GetProcAddress' D$H.Dll, D$StartOfFunctionName

                Mov esi D$StartOfFunctionName, B$ErrorLevel 5  ; error5
            Pop edi, D$edi
            inc edi
            Push eax
                Mov al "'" | stosb | Mov al 0 | stosb          ; retore for string search
            Pop eax
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

    Mov D$H.Dll 0
    Mov edi D$CodeListPtr                   ; set at 0400 by 'List.a' initialisations
    Mov D$ImportHeaderPtr edi

    Mov eax D$Dllnumber | inc eax
    shl eax 2 | Mov ebx eax | shl ebx 2 | add eax ebx    ; (eax*4)+(eax*16) = eax * 20
    Mov ecx eax | Mov D$AppImportSize eax   ; write import header size in PE sections header
    Push eax
      Mov al 0 | rep stosb        ; room for header (eax = header size)
    Pop eax
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
       test edi 1 ZERO T0>
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
    test edi 1 ZERO T0>
      Mov B$edi 0 | inc edi
T0: If B$esi = '0'
        pushad
            Push ebx, edx
                Call TranslateHexa | or eax 08000_0000
            Pop edx, ebx
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

    If edi = D$ApiListA     ; case of DLL with no api Call
        Mov D$uBaseOfRsrc 01000, D$uImportSize 0 | ret
    End_If

    Call FullfillApiList   ; FullFill each Function in the 'MODULE.ext.Function' Form.

    Call SortApis          ; >>> Dll in DllList, Functions in ApiListB + Dll #n at first

    Call CreateImportSection

    Mov eax edi | AlignOn 0200 eax | Mov ecx eax | sub ecx edi

    Push edi, eax
      Mov al 0 | rep stosb  ; fill with 0 because destination have been "reused".
    Pop eax edi

    Mov D$CodeListPtr eax

    sub eax D$CodeList | sub eax 0400 | add eax 01000 | AlignOn 01000 eax
    Mov D$uBaseOfRsrc eax
    Mov eax edi | sub eax 0400 | sub eax D$CodeList | Mov D$uImportSize eax

  ; Release all the loaded Modules not belonging to RosAsm Process:
    Mov esi ModuleHandlesList
    While D$esi <> 0
        lodsd
        Push esi
            Call 'KERNEL32.FreeLibrary' eax
        Pop esi
    End_While
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

@ExportSectionComments:
;;
; Export Section looks like this:
;
; [.Export: D$ 0, 0, 0, DLLName, 1, NumberOfFunctions, NumberOfFunctions,
;              ExportAdressesTable, ExportNamesTable, ExportOrdinals
;
;  DLLName: 'MYDLL.DLL', 0  0 0 0 0 0 0 0 0 .....
;
;  ExportAdressesTable: Function1, Function2, Function3 ....
;
;  ExportNamesTable: Function1Name, Function2Name, Function3Name  ....
;
;  ExportOrdinals: W$ 0, 1, 2 ....
;
;
;      Function1Name: B$ 'Function1' EOS
;      Function2Name: B$ 'Function2' EOS
;      Function3Name: B$ 'Function3' EOS ....]
;;

[ExportAdressesTablePtr: D$ ?
 ExportNamesTablePtr: D$ ?
 ExportOrdinals: D$ ?
 FunctionNamesPtr: D$ ?
 ExportSectionLen: D$ ?
 FileAlignedExportSectionLen: D$ ?]

FillExportSection:
    Mov D$Ordinal 0
    Mov edi D$ExportListBPtr, eax 0 | stosd | stosd | stosd
    Mov ebx edi                         ; ebx ready to write the DLLName Pointer.
    stosd | Mov eax 1 | stosd
    add edi 8                           ; (nf / nf already written by 'NewBuildExport').
    add edi 12                          ; Start of DLLName.
    Mov eax edi | add eax D$ExportAjust
    Mov D$ebx eax                       ; write DLName pointer.

    Mov esi ChoosenFile
    Push edi
L0: lodsb
    If al >= 'a'
        and al 00_11011111
    End_If
    stosb | cmp B$esi '.' | ja L0<
    Pop eax

    Mov eax D$SavingExtension | or eax 020202000 | xor eax 020202000 | stosd
    Mov al 0 | stosb | AlignOn 4 edi

  ; edi points now to 'ExportAdressesTable'. We store the edi in the Header record
  ; 'FunctionsAdressesTable':
    Mov eax edi | add eax D$ExportAjust
    Mov esi D$ExportListBPtr, D$esi+28 eax, D$ExportAdressesTablePtr eax

  ; We will have as many dWords for Adresses as Exported Functions. So, next header record,
  ; 'ExportNamesTable' will be:
    Mov eax D$NumberOfExportedFunctions | shl eax 2 | add eax edi | add eax D$ExportAjust
    Mov esi D$ExportListBPtr, D$esi+32 eax, D$ExportNamesTablePtr eax
    sub eax D$ExportAjust | Mov edi eax

  ; We will have as many dWords for Names Pointers as Exported Functions.
    Mov eax D$NumberOfExportedFunctions | shl eax 2 | add eax edi | add eax D$ExportAjust
    Mov esi D$ExportListBPtr, D$esi+36 eax, D$ExportOrdinals eax
    sub eax D$ExportAjust | Mov edi eax

  ; We will have as many Words for Ordinals as Exported Functions.
    Mov eax D$NumberOfExportedFunctions | shl eax 1 | add eax edi | add eax D$ExportAjust
    Mov D$FunctionNamesPtr eax

    Mov esi D$ExportListAPtr

L1: Call StoreExportAdresse | Call StoreExportNamePtr
    Call StoreExportOrdinal | Call StoreFunctionName

    dec D$NumberOfExportedFunctions | jnz L1<
ret


; This is same as 'SearchRegularLabel' with some specifics (no error control -impossible-)
; + end control

[DataExportedLabel: D$ ?]

StoreExportAdresse:
    Mov B$DataExportedLabel &FALSE
    Mov edi D$LabelList | add edi 5

L0: Push esi

L1:     lodsb | Mov bl B$edi | inc edi
L2:     If al = '_'
            lodsb | jmp L2<
        Else_If al = EOI
            cmp bl EOI | je L8>
        Else_If al = ':'
            Mov B$DataExportedLabel &TRUE | jmp L8>
        Else_If al >= 'a'
            and al 00_11011111 | cmp al bl | je L1<
        Else
            cmp al bl | je L1<
        End_If

        cmp bl EOI | jbe L3>            ; case of LabelList name shorter than searched Label
        Mov ecx 0FF,  al EOI | repne scasb      ; longer: LabelList edi ptr > after next '|'
L3:     add edi 6                                   ; |LABELNAME|dWord FlagByte|NEXTNAME

L7: Pop esi | jmp L0<

L8: Pop esi
    Mov eax D$edi

    sub eax D$CodeList

    If B$DataExportedLabel = &FALSE
        add eax D$AppBaseOfCode
        sub eax D$AppStartOfCode
    Else
        add eax D$AppBaseOfData
        sub eax D$AppStartOfData
    End_If

    Mov edi D$ExportAdressesTablePtr | sub edi D$ExportAjust | stosd   ; Make it RVA.
    add D$ExportAdressesTablePtr 4
ret


StoreExportNamePtr:
    Mov edi D$ExportNamesTablePtr, eax D$FunctionNamesPtr
    sub edi D$ExportAjust
    stosd | add D$ExportNamesTablePtr 4
ret


[Ordinal: D$ ?]

StoreExportOrdinal:
    Mov edi D$ExportOrdinals, eax D$Ordinal | sub edi D$ExportAjust | stosw
    add D$ExportOrdinals 2 | inc D$Ordinal
ret


StoreFunctionName:
    Mov edi D$FunctionNamesPtr | sub edi D$ExportAjust
    While B$esi <> EOI
        If B$esi = ':'
            inc esi | jmp L2>
        End_If
        movsb
    End_While
L2: Mov al 0 | stosb | add edi D$ExportAjust | Mov D$FunctionNamesPtr edi
    inc esi
ret

[ExportsectionWanted: D$ ?
 NumberOfExportedFunctions: D$ ?]

[ExportListAPtr: D$ ?
 ExportListBPtr: D$ ?]
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
; Push cs // Pop ds // Mov dx 0E // Mov ah 09 // int 021 // Mov ax 4C01 // int 021
; 18
B$ ;'Spindoz 32 spit PEfile made wiz RosAsm Assembler.$'
   'This program cannot be run in DOS mode', CR, LF, '$' EOS, 0, 0, 0, 0, 0, 0, 0, 0

; 50+18+30 = 98
; if you modify upper string you must absolutely keep the same lenght.

PeHeader:

B$ 'PE' EOS, 0          ; signature
W$ 014C                 ; 386 and more
NumberOfSections:
W$ 04                   ; 4 sections (code, data, import, resource) not 5...
B$ 0,0,0,0              ; time and date stamp
D$ 0                    ; pointer to symbol table (for debug)
D$ 0                    ; number of symbol
W$ 0E0                  ; size of 'optional header'
@PeHeaderCharacteristics:
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
D$ PAGE_SIZE            ; sections alignement
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

@idataSectionHeader:         ; (import section)

B$   '.idata' EOS, 0
AppImportTrueSize: D$ 0     ; EndOfImport - StartOfImport  true size (Virtual Size)
AppBaseOfImports: D$ 0      ; RVA
AppImportAlignedSize: D$ 0  ; 200h+ImportExt (Physical File Size)
AppStartOfImport: D$ 0      ; idata ptr
D$ 0,0,0
D$ 0_C0000040               ; readable, writable, initialised data


@ResourceSectionHeader:

B$ '.rsrc' EOS, 0, 0
AppRsrcTrueSize: D$ 0       ; EndOfResource-StartOfResource  true size
AppBaseOfRsrcs: D$ 0        ; RVA
AppRsrcAlignedSize: D$ 0    ; 200h+ResourceExt
AppStartOfRsrc: D$  0
D$ 0,0,0
D$ 0_40000040               ; readable initialised data


@DataSectionHeader:

B$ '.data' EOS, 0, 0
AppDataTrueSize: D$ 0       ; EndOfData-StartOfData  true size
AppBaseOfData: D$ 0         ; RVA
AppDataAlignedSize: D$ 0    ; 200h+DataExt    aligned size
AppStartOfData: D$ 0        ; data ptr
D$ 0,0,0
DataCharacteristics:
D$ 0_C0000040               ; readable, writable, initialised data


; Code section header: (the four 'dummy' D$ and W$ are of no mean in EXE and DLL files)

B$ '.text' EOS, 0, 0
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

@ExportSectionHeader:        ;, if any:
D$ 0 0
AppExpTrueSize: D$ 0
AppBaseOfExp: D$ 0
AppExpAlignedSize: D$ 0
AppStartOfExp: D$ 0   0 0 0
D$ 0_40000040               ; readable initialised data

@RelocSectionHeader:         ;, if Export:
D$ 0 0
@AppRelocTrueSize: D$ 0
@AppBaseOfReloc: D$ 0
@AppRelocAlignedSize: D$ 0
AppStartOfReloc: D$ 0   0 0 0
D$ 0_40000040               ; readable initialised data

D$ 0 0   0 0 0 0   0 0 0 0  ; just ensure to stop win search of sections.

D$ 0 0   0 0 0 0   0 0 0 0

@SourceSectionHeader:
B$ '.src' EOS, 0, 0, 0      ; Used by RosAsm only (not by loader: 4 sections, not 5)
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
 @uStackMax: D$ 0  @uStackMin: D$ 0  @uHeapMax: D$ 0  @uHeapMin: D$ 0]

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


[ImportTrueSize: D$ 0       ; !!! * Ici embrouille si passe tout-ça en datas virtuelles
 ResourcesTrueSize: D$ 0    ; !!! problème à la compilation.
 DataTrueSize: D$ 0         ; !!! débordement à tous les coups...
 CodeTrueSize: D$ 0]        ; !!!

[LockInstruction: D$ ?]

[TrueUserDataSize: D$ 0]    ; !!! voir * ci-dessus si D$ ? au lieux de D$ 0 bug -> [@TODO.CheckTextDelimitersPairing]
                            ; [ClearUserStubMainData] ecx TrueUserDataSize | add ecx 4

; Was no use without the new DLLs stuff, but, when building a DLL after having an EXE PE
; compiled, some of these Data may remain with a 'wrong' value from the previous compilation.

ClearUserStubMainData:
    Mov edi uImportSize, eax 0, ecx TrueUserDataSize | add ecx 4
    sub ecx edi | shr ecx 2 | rep stosd
ret

UserMainData:
    Move D$ImportTrueSize D$uImportSize,
         D$ResourcesTrueSize D$uRsrcSize,
         D$DataTrueSize D$uDataSize

    Mov eax D$uImportSize | AlignOn 0200 eax

    Mov D$uImportSize eax | Mov ecx eax
    Mov eax D$uRsrcSize | AlignOn 0200 eax
    Mov D$uRsrcSize eax | add ecx, eax
    Move D$TrueUserDataSize D$uDataSize    ; preserve for the whole (with virtual size) Align
    Mov eax D$uDataSize | AlignOn 0200 eax
    Mov D$uDataSize eax | add ecx, eax
    Mov eax D$CodelistPtr | sub eax D$CodeList | sub eax 0400 | sub eax, ecx
    Move D$CodeTrueSize eax
    Mov D$AppTrueCodeSize eax
    AlignOn 0200 eax | Mov D$uCodeSize eax

    Mov eax 0400 | Mov D$uStartOfImport eax
        add eax D$uImportSize | Mov D$uStartOfRsrc eax
        add eax, D$URsrcSize | Mov D$uStartOfData eax
        add eax D$uDataSize | Mov D$uStartOfCode eax
        add eax D$uCodeSize | Mov D$uEndOfFile eax

    Mov eax 01000 | Mov D$uBaseOfImport eax
        add eax D$UImportSize | AlignOn 01000 eax
        Mov D$uBaseOfRsrc eax
        add eax D$uRsrcSize | AlignOn 01000 eax
        Mov D$uBaseOfData eax
        add eax D$TrueUserDataSize | add eax D$uVirtualDataSize | AlignOn 01000 eax
        Mov D$uBaseOfCode eax
        add eax D$uCodeSize | AlignOn 01000 eax
        Mov D$uImageSize eax

    Mov eax LINKERDEFAULT | add eax D$uBaseOfCode
        sub eax D$uStartOfCode | Mov D$uCodeRVA eax

    Mov eax D$uStartOfCode | sub eax D$uStartOfImport | Mov D$uAllDataSize eax
ret
 _______________________________________________________________________________________


PreparePeHeader:
  Call UserMainData

  Move D$AppCodeSize D$uCodeSize
  Move D$AppAllDataSize D$uAllDataSize

  Call SearchForEntryPoint
  sub eax D$CodeList | sub eax D$uStartOfCode | add eax D$uBaseOfCode
  Mov D$AppRVAentryPoint eax

  Move D$AppBaseOfCode D$uBaseOfCode
  Move D$SHAppBaseOfData D$uBaseOfData
  Move D$AppRVAimageSize D$uImageSize

;  hexprint D$uBaseOfCode,
;           D$uBaseOfImport,
;           D$uBaseOfCode

  Move D$AppBaseOfImport D$uBaseOfImport

  Move D$AppBaseOfRsrc D$uBaseOfRsrc
  Move D$AppRsrcSize D$uRsrcSize       ; should be unaligned size of the section

__________________

  Move D$AppCodeRVAoffset D$uBaseOfCode
  Move D$AppFileSizeOfCode D$uCodeSize
  Move D$AppStartOfCode D$uStartOfCode

  Mov eax D$DataTrueSize | add eax D$uVirtualDataSize

  Move D$AppDataTrueSize eax
  Move D$AppBaseOfData D$uBaseOfData
  Move D$AppDataAlignedSize D$uDataSize
  Move D$AppStartOfData D$uStartOfData

  Move D$AppImportTrueSize D$ImportTrueSize
  Move D$AppBaseOfImports D$uBaseOfImport
  Move D$AppImportAlignedSize D$uImportSize
  Move D$AppStartOfImport D$uStartOfImport

  Move D$AppRsrcTrueSize D$ResourcesTrueSize
  Move D$AppBaseOfRsrcs D$uBaseOfRsrc
  Move D$AppRsrcAlignedSize D$uRsrcSize
  Move D$AppStartOfRsrc D$uStartOfRsrc

; Store source values in .Src section:
  Move D$AppSrcTrueSize D$SourceLen
  Mov eax D$uBaseOfCode | add eax D$LenOfCode | AlignOn 01000 eax
  Mov D$AppBaseOfSrc eax
  Mov eax D$SourceLen | AlignOn 0200 eax
  Mov D$AppSrcAlignedSize eax
  Mov eax D$uEndOfFile | Mov D$AppStartOfSrc eax

; Copy:
  Mov esi DosHeader | Mov edi D$CodeList
  Mov ecx 080 | rep movsb                                   ; store Dos header
  Mov ecx D$PeHeaderSize | Mov esi PeHeader | rep movsb     ; room for PE header
ret


[LocOfSourceHeader: D$ ?]

WritePeHeaders:
    or D$SavingExtension 020202000 | xor D$SavingExtension 020202000

    Mov esi DosHeader, edi D$CodeList

    Mov ecx PeHeader | sub ecx esi | shr ecx 2 | rep movsd

    Mov W$edi 'PE', W$edi+2 0                   ; signature
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
    Else_If D$FL.RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
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

    Mov D$edi PAGE_SIZE | add edi 4             ; sections alignement
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

    Push edi
        Mov eax 0, ecx 020 | rep stosd  ; Clear all 16 * (2 dWords) Entires ('SectionTable').
    Pop edi

 [FinalBaseOfExport: D$ ?
  FinalAppBaseOfReloc: D$ ?
  FinalImageSize: D$ ?]

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
    .Else_If D$FL.RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
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
        Push D$NextSectionHeaderRVA
        Mov eax D$DataTrueSize
        Call WriteOneSectionHeader '.dat', 'a', eax, D$DataCharacteristics
           ; &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE

      ;  Mov eax D$uDataSize | add eax D$uVirtualDataSize
        Mov eax D$TrueUserDataSize | add eax D$uVirtualDataSize

        Pop D$NextSectionHeaderRVA
        Mov D$edi-(8*4) eax | AlignOn 01000 eax | add D$NextSectionHeaderRVA eax
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
    .Else_If D$FL.RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
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

[NextSectionHeaderRVA: D$ ?
 NextSectionHeaderFilePointer: D$ ?]

Proc WriteOneSectionHeader:
    Arguments @NameLow, @NameHigh, @Size, @Flags

        Mov eax D@NameLow | stosd
        Mov eax D@NameHigh | stosd
        Mov eax D@Size | stosd
        Mov eax D$NextSectionHeaderRVA | stosd
        Mov eax D@Size | AlignOn 0200 eax | stosd
        Mov ecx eax
        AlignOn 01000 eax | add D$NextSectionHeaderRVA eax
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

@ResourcesStub:
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

; Level 2: (resource TYPEs directory)

[@Level2Rt_Icon:
W$ 0,0,0,0,0,0,0,1   ; 1 record
D$ 01, 080000050     ;Level3Rt_Icon-StartOfRsrc+NodeFlag;80000070h; ID 1: icon; > 0C70h

@Level2Rt_Group_Icon:
W$ 0,0,0,0,0,0,0,1   ; 1 record
D$ 064, 080000068    ;Level3Rt_Group_Icon-StartOfRsrc+NodeFlag; ID 64h: Group icon > 0C88h

; Level 3: (last one to one language resources pointers - lang. dir -)

@Level3Rt_Icon:
W$ 0,0,0,0,0,0,0,1
D$ 0409, 080 ;Level4Rt_Icon-StartOfRsrc;B8h, 409 = langage ID: can_english; B8h > CB8h

@Level3Rt_Group_Icon:
W$ 0,0,0,0,0,0,0,1
D$ 0409, 090  ;Level4Rt_Group_Icon-StartOfRsrc   ;C8h, CC8h

;Level 4: (records of each resource: PTR, size, CodePage, reserved)

;;RsrcRVA = BaseOfRsrc-StartOfRsrc

@Level4Rt_Icon:
;;D$ IconHeader+RsrcRVA, 02E8, 0, 0  ; icon at CF0h;         size=2E8h
D$ 020A8, 02E8, 0, 0

@Level4Rt_Group_Icon:
;;D$ Group_Icon+RsrcRVA, 014, 0, 0   ; group icon at FD8h;   size = 14h
D$ 02390, 014, 0, 0

; icon data. This icon image is for compilation only. At start of RosAsm Run, the default
; icon is copyed from Icon Editor to here. The Editor version is in fact used as temporary
; storage.

uIcon:

@IconHeader:
B$ 028,0,0,0   ; size
 020,0,0,0     ; width
 040,0,0,0     ; height (maybe 40h because of the two masks)
 01,0          ; planes
 04,0          ; bit count
 0,0,0,0       ; compression 0
 080,02,0,0    ; 0280 > size of icon data
 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0   ; (dummy)


@IconPalette:
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

@IconXorMask:
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

@IconAndMask:
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
@EndOfRsrc: 0] ; !!! WORD
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

uMenuEnd:]
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

[@DumRL: 0 0 0 0 0]  ; to stop upward search on zeros

[uRsrcList:         ; BUAsm infos storage for .rsrc section building (5 dWords per resource)
   D$ 0             ; Type
      0             ; ID
  0_409             ; Language
      0             ; Data pointer
      0             ; Size
      # MAXRESOURCE]
[@Dum2RL: D$ 0 0 0 0 0]

[RsrcHeadPtr: D$ ?
 RsrcSectionOrigine: D$ ?
 RsrcSectionPtr: D$ ?
 RsrcTypePtr: D$ ?
 RsrcLangPtr: D$ ?
 RsrcPtrPtr: D$ ?
 uRsrcListPtr: D$ ?]

; We set here the list of existing resources for the app resources section. We store
; the type, the ID, Dummy language, Pointer to resource data and size. For menu, i let
; some 'RosAsm developpement internal' test that i use to study menus encoding:

[TypeByName: D$ ?]


[NoResources: D$ ?
 NoMainIcon: D$ ?]
 ________________________________________________________________________________________
 ________________________________________________________________________________________

;                                   data job
 ________________________________________________________________________________________
 ________________________________________________________________________________________

[LenOfDataSet: D$ ?
 AfterLastDataLabel: D$ ?
 DataLoopValue: D$ ?
 DefSize: D$ ?]

BuildData:
    Mov eax D$CodeListPtr | Mov D$DataList eax | Mov D$DataListPtr eax

        Call StoreDatas

    Mov eax D$DataListPtr | sub eax D$DataList | Mov D$uDataSize eax

    Mov eax D$DataListPtr

  ; Clear all possible trailing left Source Text:
    Push eax
        While B$eax <> 0
            Mov B$eax 0 | inc eax
        End_While
    Pop eax

    AlignOn 0200 eax | Mov D$CodeListPtr eax

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

    On B$DefSize = UNICODE_MEM jmp TranslateUnicode

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
    Push edi
        Mov edi esi | dec esi | On B$esi = 'X', dec esi
        std
            While B$esi > LowSigns | movsb | End_While
        cld
        While B$edi > LowSigns | Mov B$edi '0' | dec edi | End_While
    Pop edi

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



[DataSign: D$ ?]

StoreDataRef:
    On B$DefSize <> 'D' error D$DsizeForLabPtr
    Push edi
        Mov edi D$DataRefPtr
        ..If D$FL.Dynamic = &TRUE
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
    Pop edi
ret


; j'ai oubli  de commenter   '<<<<' pourquoi faut-il ajouter un dWord apr s la valeur de
; LEN. Sans ce 0 suppl mentaire, la donn e suivante est  crasee (0). Comme  a, tout
; fonctionne mais je ne sais plus pourquoi!!!!!!!!!!...........

;;

    !!! Perso je soupçonne un problème d'alignement ;))
;;

[HeadLenPtr: 0    HeadLenFlag: 0    HeadLenSize: DWORD_MEM]

StoreOneData:
    Mov B$DataSign &FALSE
    Push edi

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
                    If B$DefSize = DWORD_MEM
                        stosd | Mov D$HeadLenSize DWORD_MEM D$LenOfDataSet 4
                    Else_If B$DefSize = WORD_MEM
                        stosw | Mov D$HeadLenSize WORD_MEM D$LenOfDataSet 2
                    Else_If B$DefSize = BYTE_MEM
                        stosb | Mov D$HeadLenSize BYTE_MEM D$LenOfDataSet 1
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
    Pop edi
    ret

L0:     cmp B$DefSize 'R' | jne L0>
        Call AsciiToQword
    Pop edi
    ret

L0:     cmp B$DefSize 'H' | jne L0>
        error D$NewHmemPtr
      ; Call AsciiToQword
    Pop edi
    ret

L0:     cmp B$DefSize 'F' | jne L0>
        Call FAsciiToQword
    Pop edi
    ret

L0:     cmp B$DefSize 'T' | jne L0>
      ;  Push esi
      ;      Call ParseFpWords
      ;  Pop eax
      ;  On esi = eax,
        Call TAsciiToQword
    Pop edi
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
            Push 0 | Push 0 | sub D$esp eax | sbb D$esp+4 edx ;jE!
            Pop eax | Pop edx
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
    Pop edi
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
        Push esi
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
        Pop esi
        Align_On_Variable eax D$DataListPtr
    ..End_If
L9: ret


[DataLabelsCounter: D$ ?
 CodeLabelsCounter: D$ ?]

[DataLoopWinEquate: D$ ? # 20]

[LOOPDATAMAX 01_000]

[ColonWanted: D$ ?]

StoreDatas:
    Mov esi D$CodeSourceB
    Mov B$ErrorLevel 0,  D$bracketCounter 0, D$DataLabelsCounter 0
    Mov D$HeadLenPtr 0, D$HeadLenFlag 0, D$HeadLenSize DWORD_MEM

    Move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4

L0: .If B$HeadLenFlag = &TRUE
        Mov ebx D$HeadLenPtr, eax D$LenOfDataSet, B$HeadLenFlag &FALSE

        If B$HeadLenSize = DWORD_MEM
            Mov D$ebx eax
        Else_If B$HeadLenSize = WORD_MEM
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

L2: Push esi
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

L5: Pop esi

    Call StoreOneData | jmp L9>>                ; if data

; Case "__:__"
L6:     Mov B$ColonWanted &FALSE
        On B$esi = meEOI, inc esi               ; no '|' betweem ':' and data
        Mov D$AfterLastDataLabel esi
    Pop esi

    Call StoreDataLabel                         ; if label
    On B$LocalLabel = &TRUE, error D$NoDataLocalLabelPtr
    inc D$DataLabelsCounter
    Mov esi D$AfterLastDataLabel | jmp L9>>     ; Called Subs don't change ESI

; Case "__#__"
L7: On B$esi-2 > PartEnds, error D$WhatIsThisPtr
    On B$ColonWanted = &TRUE, error D$NestedLoopPtr
    Pop eax

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
       Mov eax D$StatementsPtr, D$eax FLAG_DONE
       jmp L0<<                    ; lasting data
    End_If
    On B$esi  <> Closebracket,  jmp L2<<                      ; lasting label
    Mov eax D$StatementsPtr, D$eax FLAG_DONE
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
[uVirtualDataSize: D$ ?]
[NoVirtualLimit: D$ ?]
[LOOPVDATAMAX 010_0000]

StoreVirtualData:
    Mov esi D$CodeSourceB,  edi D$CodeSourceB
    Mov B$ErrorLevel 0,  D$bracketCounter 0, D$uVirtualDataSize 0

    Move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4

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
    Push D$DataListPtr
        Call AlignDataSet
    Pop eax
    If D$DataListPtr > eax
        Mov ebx D$DataListPtr | sub ebx eax | add D$uVirtualDataSize ebx
    End_If

L2: Push esi
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

L5: Pop esi | Call StoreOneVirtualData | jmp L9>>   ; if data

L6:   Mov B$ColonWanted &FALSE
      Move D$DataListPtrAtLastColon D$DataListPtr
      On B$esi = meEOI, inc esi                 ; no '|' betweem ':' and data
      Mov D$AfterLastDataLabel esi
    Pop esi
    Call StoreDataLabel                         ; if label
    On B$LocalLabel = &TRUE, error D$NoDataLocalLabelPtr
    inc D$DataLabelsCounter
    Mov esi D$AfterLastDataLabel | jmp L9>>     ; Called Subs don't change ESI

L7: On B$esi-2 > PartEnds, error D$WhatIsThisPtr
    On B$ColonWanted = &TRUE, error D$NestedLoopPtr
    Pop eax
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
        Mov eax D$StatementsPtr, D$eax FLAG_DONE | jmp L0<<      ; lasting data
    End_If
    On B$esi <> CloseVirtual,  jmp L2<<                         ; lasting label
        inc esi | Mov eax D$StatementsPtr, D$eax FLAG_DONE | jmp L0<<

_________________________________________________________________________________________
 _________________________________________________________________________________________

; Labels deal

[StartOfLabelName: D$ ?
 LocalLabel: D$ ?]

E0: error D$BadLabelPtr

StoreDataLabel:
    Mov edx D$DataListPtr | Mov cl FLAG_DATA_LABEL | jmp L0>

StoreCodeLabel:
    Mov edx D$CodeListPtr | Mov cl FLAG_CODE_LABEL

L0: Push edi
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

    Pop edi
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
 LongRelative: 0  dis32: 0  @imm64: imm32: 0 0   ABSimm32: 0  SignedImm8: 0
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
 @sBitPossible: 0  @sBitWritten: 0
 OneOperandwBit: 0            ; for temporary storage inside One parameter analyze
 FirstOperandwBit: 0            ; These two
 SecondOperandwBit: 0            ; for test of fitting sizes
 ThirdOperandwBit: 0
 ModBits: 0  SIBreg1: 0  SIBreg2: 0
 ScaledIndex: 0  @Base: 0  ScaleFound: 0  Reg1isIndex: 0  Reg2isIndex: 0  TwoRegsFound: 0
 PossibleRmEreg: 0  ExpressedSign: 0

 PossibleImmLabel: B$ 0  PossibleFirstImmLabel: 0

 ParametersDataLen: len]

[relativeFlag  00__10000000_00000000_00000000_00000000]

 _________________________________________________________________________________________
 _________________________________________________________________________________________

; expression deal: ModR/M, SIB and immediate in expressions

[IfEregFound | cmp B$Ereg NA | jne #1]

[IfEregNotFound | cmp B$Ereg NA | je #1]

[IfNotPartEnd | cmp #1 PartEnds | ja #2]

[IfSeparator | cmp #1 Separators | jb #2]

IsitaReg:
     Call Store8cars | On op3 nb Separators,  jmp L5>>      ; a 2 letters reg name?

L0: ifnot op2 'X', L2>
        ifnot op1 'A', L1>
            Mov al REG_AX | jmp L3>
L1:     ifnot op1 'B', L1>
            Mov al REG_BX | jmp L3>
L1:     ifnot op1 'C', L1>
            Mov al REG_CX | jmp L3>
L1:     On op1 <> 'D',  jmp L9>>
            Mov al REG_DX

L3:     Mov W$esi 0 | add esi 3 | Mov ah GENERAL_REG | Mov B$OneOperandwBit WORD_SIZE | ret

L2: ifnot op2 'H', L2>
        ifnot op1 'A', L1>
                Mov al REG_AH | jmp L3>
L1:     ifnot op1 'B', L1>
            Mov al REG_BH | jmp L3>
L1:     ifnot op1 'C', L1>
            Mov al REG_CH | jmp L3>
L1:     On ah <> 'D',  jmp L9>>
            Mov al REG_DH | jmp L3>

L2: ifnot op2 'L', L2>
        ifnot op1 'A', L1>
            Mov al REG_AL | jmp L3>
L1:     ifnot op1 'B', L1>
            Mov al REG_BL | jmp L3>
L1:     ifnot op1 'C', L1>
            Mov al, REG_CL | jmp L3>
L1:     On op1 <> 'D',  jmp L9>>
            Mov al, REG_DL

L3:     Mov W$esi 0 | add esi 3 | Mov ah GENERAL_REG | Mov B$OneOperandwBit BYTE_SIZE | ret

L2: ifnot op2 'P', L2>
        ifnot op1 'B', L1>
            Mov al REG_BP | jmp L3>
L1:     On op1 <> 'S',  jmp L9>>
            Mov al REG_SP | jmp L3>

L2: ifnot op2 'I', L2>
        ifnot op1 'S', L1>
            Mov al REG_SI | jmp L3>
L1:     On op1 <> 'D',  jmp L9>>
            Mov al REG_DI

L3:     Mov W$esi 0 | add esi 3 | Mov ah GENERAL_REG | Mov B$OneOperandwBit WORD_SIZE | ret

L2: ifnot op2 'S', L5>
        ifnot op1 'E', L1>
           Mov al REG_ES | jmp L3>
L1:     ifnot op1 'C', L1>
           Mov al REG_CS | jmp L3>
L1:     ifnot op1 'S', L1>
           Mov al REG_SS | jmp L3>
L1:     ifnot op1 'D', L1>
           Mov al REG_DS | jmp L3>
L1:     ifnot op1 'F', L1>
           Mov al REG_FS | jmp L3>
L1:     On op1 <> 'G',  jmp L9>>
           Mov al REG_GS

L3:     Mov W$esi 0 | add esi 3 | Mov ah SEGMENT_REG | Mov B$OneOperandwBit WORD_SIZE | ret

; Is it a 3 letters MMX register name?

L5: On op4 nb Separators,  jmp L9>>
       IfNot op1, 'M', L5>
         IfNot op2, 'M', L5>
           cmp op3, '0' | jb L5>
           cmp op3, '7' | jg L5>
             sub bh, '0' | Mov al bh | Mov ah REG_MMX
             and D$esi 0FF000000 | add esi 4 | Mov B$OneOperandwBit DWORD_SIZE | ret

; Is it a 3 letters STx register name?

L5: IfNot op1, 'S', L5>
        IfNot op2, 'T', L5>
           cmp op3, '0' | jb L5>
           cmp op3, '7' | jg L5>
             sub bh, '0' | Mov al bh | Mov ah REG_FPU
             and D$esi 0FF000000 | add esi 4 | Mov B$OneOperandwBit DWORD_SIZE | ret

; Is it a 3 letters DRx register name?

L5: IfNot op1, 'D', L5>
        IfNot op2, 'R', L5>
           cmp op3, '0' | jb L5>
           cmp op3, '7' | jg L5>
             sub bh, '0' | Mov al bh | Mov ah DEBUG_REG
             and D$esi 0FF000000 | add esi 4 | Mov B$OneOperandwBit DWORD_SIZE | ret

; Is it a 3 letters CRx register name?

L5: IfNot op1, 'C', L5>
        IfNot op2, 'R', L5>
           cmp op3, '0' | jb L5>
           cmp op3, '4' | jg L5>
             sub bh, '0' | Mov al bh | Mov ah CONTROL_REG
             and D$esi 0FF000000 | add esi 4 | Mov B$OneOperandwBit DWORD_SIZE | ret


;IsItAreg32:

L5: On op1 <> 'E',  jmp L9>
        ifnot op3 'X', L3>
            ifnot op2 'A', L2>
                Mov al REG_EAX | jmp L4>
L2:         ifnot op2 'C', L2>
                Mov al REG_ECX | jmp L4>
L2:         ifnot op2 'D', L2>
                Mov al REG_EDX | jmp L4>
L2:         On op2 <> 'B',  jmp L9>
            Mov al REG_EBX | jmp L4>
L3:     ifnot op3 'P', L3>
            ifnot op2 'S', L2>
                Mov al REG_ESP | jmp L4>
L2:         On op2 <> 'B',  jmp L9>
                Mov al REG_EBP | jmp L4>
L3:     On op3 <> 'I',  jmp L9>
            ifnot op2 'S', L2>
                Mov al REG_ESI | jmp L4>
L2:         On op2 <> 'D',  jmp L9>
                Mov al REG_EDI

L4:     and D$esi 0FF000000 | add esi 4 | Mov ah GENERAL_REG | Mov B$OneOperandwBit DWORD_SIZE | ret

L9: ; 4 Chars Register? > XMM.?

    ifnot op1 'X', L9>
        ifnot op2 'M', L9>
            ifnot op3 'M', L9>
                cmp op4, '0' | jb L9>
                    cmp op4, '7' | jg L9>
                        sub op4, '0' | Mov al op4 | Mov ah REG_XMM
                        Mov D$esi 0 | add esi 5
                        Mov B$OneOperandwBit OWORD_SIZE | ret


L9: Mov eax 0 | ret


; when called 'E' found, esi points to the next letter
; 'IsItAnEreg' doesn't change esi
; 'SearchForEreg' does.

IsItAnEreg:
    Mov B$Ereg NA | Call Store4cars | IfNotPartEnd op3, L9>>

    IfNotPartEnd B$esi-2, L9>>
        ifnot op2 'X', L2>
            ifnot op1 'A', L1>
                Mov B$Ereg REG_EAX | ret
L1:         ifnot op1 'C', L1>
                Mov B$Ereg REG_ECX | ret
L1:         ifnot op1 'D', L1>
                Mov B$Ereg REG_EDX | ret
L1:         ifnot op1 'B', L9>
                Mov B$Ereg REG_EBX | ret
L2:     ifnot op2 'P', L2>
            ifnot op1 'B', L1>
                Mov B$Ereg REG_EBP | ret
L1:         ifnot op1 'S', L9>
                Mov B$Ereg REG_ESP | ret
L2:     ifnot op2 'I', L9>
            ifnot op1 'S', L1>
                Mov B$Ereg REG_ESI | ret
L1:         ifnot op1 'D', L9>
                Mov B$Ereg REG_EDI | ret
L9: ret


SearchForEreg:
    Mov B$Ereg NA                     ; this double declaration is NOT useless
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
L3: On B$Ereg = REG_ESP error D$ESPsibPtr              ; ESP can not be an index
    add esi 2 | Mov B$ScaleFound &TRUE
    Mov B$esi-2 0,  B$esi-1 0                      ; clear scale
    Mov bl B$Ereg | shl bl 3 | or bl al            ; store [xx xxx ...]
    Mov B$ScaledIndex bl                           ; in ScaledIndex
L9: ret
L8: error D$ScaleValuePtr


; forbidden use of non extended registers in memory adressing statements:

VerifyNoOtherReg:
    Push esi
L0:   lodsb
      cmp al 0 | je L0<
        cmp al Separators | jb L9>
            cmp al PartEnds | jb L0<
                dec esi | Call IsItAreg | cmp eax 0 | je L1>
                On B$OneOperandwBit <> DWORD_SIZE error D$WishEregPtr
                On ah <> GENERAL_REG error D$WishEregPtr
L1:             inc esi
L2:             cmp B$esi PartEnds | jb L0>         ; strip remaining text
                    inc esi | jmp L2<
L9: Pop esi
ret


SearchForSIB:
    Mov B$SibInside &FALSE, B$Reg1isIndex &FALSE, B$Reg2isIndex &FALSE, B$TwoRegsFound &FALSE
    Push esi
      Call SearchForEreg | IfEregFound L0>
        Pop esi
          Call VerifyNoOtherReg
        ret

L0: Mov al B$Ereg,  B$SIBreg1 al

    Call SearchForScale | Mov al B$ScaleFound, B$Reg1isIndex al   ; true or FALSE
    Call SearchForEreg | IfEregNotFound L1>
        On B$esi-4 <> addSign, error D$ExpressionPtr
        Mov al B$Ereg,  B$SIBreg2 al | Mov B$TwoRegsFound &TRUE

        Call SearchForScale | Mov al B$ScaleFound,  B$Reg2isIndex al
        Call SearchForEreg | On B$Ereg <> NA  error D$expressionPtr    ; 3 regs forbidden

        .If B$ScaleFound = &FALSE
            If B$SIBreg2 = REG_EBP
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

L5: cmp B$SIBreg1 REG_ESP | jne L9>             ; no index found, only one reg found, but
        Mov B$SibInside &TRUE | Mov B$SIB 024     ; if reg = ESP >SIB
L9: Pop esi
    ret


[immSign: D$ ?]

SearchForImm:      ; if yes, return value set in eax by translating routines
                   ; esi incrementations done by translating routine
    Mov B$immSign &FALSE
    Push esi | jmp L1>

L9: Pop esi | ret

L0:     inc esi
L1:     Mov dh B$esi-1,  B$ExpressedSign dh | Mov ah B$esi,  al B$esi+1
        cmp ah textSign | je L7>>
        ifSeparator ah, L9<
        cmp ah '0' | jb L0<
        cmp ah '9' | ja L0<
        cmp dh PartEnds | ja L0<
        Mov B$immInside &TRUE
        Push esi
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

L5:     Pop esi
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

        Push edi
            Mov B$immInside &TRUE | Mov edi imm32 | lodsb   ; read text sign
L7:         Mov B$esi-1 0 | lodsb                           ; clear imm text
            cmp al TextSign | je L8>
            add B$edi al | inc edi
            On edi = imm32+5,  error D$TxtTooMuchPtr             ; max of 4 letters
            jmp L7<
L8:         Mov B$esi-1 0                                   ; clear last text marker sign
        Pop edi
        jmp L1<<

L9: Pop esi
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
 I don't use upper chunk (from 'L9: Pop esi'), because of the Sign Extensions able
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
    Push esi | jmp L1>
L0:     inc esi
L1:     Mov dh B$esi-1,  B$ExpressedSign dh
        Mov ah B$esi,  al B$esi+1
        cmp ah 0 | je L0<
        ifSeparator ah, L8>>
        cmp ah '0' | jb L0<
        cmp ah '9' | ja L0<
        cmp dh PartEnds | ja L0<
        Mov B$DisInside &TRUE
        Push esi
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
                test D$Dis32 0_8000_0000 ZERO S1>
                    neg D$Dis32 | sub D$Dis32 eax | neg D$Dis32 | jmp L6>
S1:                 add D$Dis32 eax | jmp L6>
S2:         On B$ExpressedSign <> SubSign,  error D$NotYetSignPtr
            test D$Dis32 0_8000_0000 ZERO L5>
                neg D$Dis32 | add D$Dis32 eax | neg D$Dis32 | jmp L6>
L5:                 sub D$Dis32 eax
L6:     Pop esi
L7:     Mov B$esi 0
        inc esi | cmp B$esi PartEnds | ja L7<
        jmp L1<<

L8: Pop esi

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
    Push esi
        Mov edi D$CodeRefPtr
L0:     lodsb | cmp al 0 | je L0<
                cmp al Separators | jb L9>>
                cmp al 'A' | jb L0<
                cmp al, 'Z' | ja L0<
        On B$esi-2 = AddSign,  Mov B$esi-2 0
        cmp al 'E' | jne L1>
        Push eax
            Call IsItAnEreg                 ; usefull only in case of mem adressing
        Pop eax
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
    Pop esi
ret


SearchForModRMreg:
    Mov cl 32 | cmp B$LabelInside &TRUE | je L0>
    Mov eax D$Dis32

    On B$LongRelative = &FALSE, Mov cl 8
                                      ; for BYTE_SIZE tests on Displacement
                                                ; (080 >>> -128)
L0: Push esi
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

L6:     cmp B$RmBits REG_EBP | jne L7>                       ; reg EBP >>> add zero dis
            Mov B$DisInside &TRUE | jmp L4<

L7:     Mov B$ModBits 0                                     ; reg without displacement

L9: Pop esi

  ; This is to force things like "Push D$fs:+0' to be encoded Long, and not short dis form:
    cmp B$DisInside &TRUE | jne L9>
        cmp B$LabelInside &FALSE | jne L9>
            cmp B$EregInside &FALSE | jne L9>
                Mov B$LongRelative &TRUE

L9: ret


SearchForFirstFPUimm:
    Mov B$FirstGender IMM_VALUE | jmp L0>
SearchForSecondFPUimm:
    Mov B$SecondGender IMM_VALUE

L0: Push esi
        Call atof
        On al > CloseBracket, error D$BadRealPtr
        fstp D$imm32
        Mov B$ImmInside &TRUE, B$immSign &FALSE
    Pop esi
L0: Mov B$esi 0                                     ; clear imm
    inc esi | cmp B$esi PartEnds | ja L0<
  ret


FirstParameterAnalyze:
    Mov esi D$LineStart

L0: lodsb                               ; simply increase esi over first space; after this,
    cmp al Space | jne L0<                    ; save esi pointing to parameter (> Push/Pop)

    cmp B$esi+1 memMarker | jne L4>>               ; if mem marker found, store ascii value
      Mov al B$esi | Mov B$FirstParaMem al
      Mov W$esi 0 | add esi 2                     ; (see equ. for BYTE_MEM, WORD_MEM, DWORD_MEM)
      If B$esi = MemMarker
        inc esi
      End_If
      Mov B$FirstGender MEM_LABEL

        On B$esi = EOI, error D$ParameterPtr

        .If al = DWORD_MEM
             Mov B$FirstOperandwbit DWORD_SIZE   ; D$            4 bytes
        .Else_If al = BYTE_MEM
             Mov B$FirstOperandwbit BYTE_SIZE     ; B$            1 byte
        .Else_If al = WORD_MEM
             Mov B$FirstOperandwbit WORD_SIZE     ; W$            2 bytes
        .Else_If al = QWORD_MEM
             Mov B$FirstOperandwbit QWORD_SIZE     ; Q$            8 bytes
        .Else_If al = REEL_MEM
             Mov B$FirstOperandwbit QWORD_SIZE     ; R$ = FPU Q$   8 bytes
      ;  .Else_If al = REEL2_MEM
      ;       error NewHmem
       ;      Mov B$FirstOperandwbit QWORD_SIZE     ; H$ = FPU Q$   8 bytes >>> 16 bytes (!!!)
        .Else_If al = FLOAT_MEM
             If B$esi < 'A'
                 Call SearchForFirstFPUimm | ret     ; exemple: Push F$45.2
             Else
                 Mov B$FirstOperandwbit DWORD_SIZE   ; F$ = FPU D$
             End_If
        .Else_If al = TBYTE_MEM
             Mov B$FirstOperandwbit TBYTE_SIZE      ; T$ = FPU 10 Bytes / 80 bits (m80)
        .Else_If al = XWORD_MEM
            Mov B$FirstOperandwbit USO_SIZE         ; Weird and XMM sizes
        .Else_If al = OWORD_MEM
            Mov B$FirstOperandwbit OWORD_SIZE         ; Weird and XMM sizes
        .Else
            On al = REEL2_MEM error D$NewHmemPtr
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
      Mov B$FirstGender GENERAL_REG,  B$FirstReg al,  B$FirstRegGender ah
      Mov al B$OneOperandwbit,  B$FirstOperandwbit al
    ret

L5: Call SearchForImm | cmp B$ImmInside &FALSE | je L6>
      Mov B$FirstGender IMM_VALUE | Mov B$PossibleFirstImmLabel &TRUE
    ret

L6: If B$ApiFound = &TRUE   ; 'NewSearchApiName'
        Mov B$DisInside &TRUE, B$ApiFound &FALSE, B$FirstGender MEM_LABEL
        Mov B$ModBits 0, B$RmBits 00101
        Mov B$FirstOperandwBit DWORD_SIZE
        ret
    End_If

L6: Call SearchForLabel | On B$LabelInside = &FALSE,  error D$UnknownParameterPtr
      Mov B$FirstGender DISPLACEMENT
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
        Mov W$esi 0 | add esi 2                  ; (see equ. for BYTE_MEM, WORD_MEM, DWORD_MEM, ...)
        Mov B$secondGender MEM_LABEL

        On B$esi = EOI, error D$ParameterPtr

        .If al = DWORD_MEM
             Mov B$SecondOperandwBit DWORD_SIZE   ; D$
        .Else_If al = BYTE_MEM
             Mov B$SecondOperandwBit BYTE_SIZE     ; B$
        .Else_If al = WORD_MEM
             Mov B$SecondOperandwBit WORD_SIZE     ; W$
        .Else_If al = QWORD_MEM
             Mov B$SecondOperandwBit QWORD_SIZE     ; Q$
        .Else_If al = REEL_MEM
             Mov B$SecondOperandwBit QWORD_SIZE     ; R$ = FPU Q$
        .Else_If al = REEL2_MEM
             error D$NewHmemPtr
       ;      Mov B$SecondOperandwBit QWORD_SIZE     ; H$ = FPU Q$ (!!! > 16 bytes / 128 bits !!!)
        .Else_If al = FLOAT_MEM
             If B$esi < 'A'
                 Call SearchForSecondFPUimm | ret      ; exemple: Mov eax F$45.2
             Else
                 Mov B$SecondOperandwBit DWORD_SIZE   ; F$ = FPU D$
             End_If

        .Else_If al = TBYTE_MEM
             Mov B$SecondOperandwBit TBYTE_SIZE      ; T$ = FPU 10 Bytes
        .Else_If al = XWORD_MEM
            Mov B$SecondOperandwbit USO_SIZE         ; Weird and XMM sizes
        .Else_If al = OWORD_MEM
            Mov B$SecondOperandwbit OWORD_SIZE      ; XMM sizes
        .Else
            On al = REEL2_MEM error D$NewHmemPtr
            error D$UnknownSizePtr
        .End_If

L4:     Call SearchForSIB
        Call SearchForDis
        Call SearchForLabel
        ...If B$SibInside = &TRUE
            ..If B$DisInside = &FALSE
                .If B$FirstGender = GENERAL_REG
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
      Mov B$secondGender GENERAL_REG,  B$secondReg al,  B$SecondRegGender ah
      Mov al B$OneOperandwbit,  B$secondOperandwbit al

      ...If B$SibInside = &TRUE
        ..If B$DisInside = &FALSE
            .If B$FirstGender = MEM_LABEL
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
      Mov B$secondGender IMM_VALUE | Mov B$PossibleImmLabel &TRUE
    ret

L7: If B$ApiFound = &TRUE   ; 'NewSearchApiName'
        Mov B$DisInside &TRUE, B$ApiFound &FALSE, B$SecondGender MEM_LABEL
        Mov B$ModBits 0, B$RmBits 00101
        Mov B$SecondOperandwBit DWORD_SIZE
        ret
    End_If

L7:   cmp D$DisInside &TRUE | jne L8>
      Mov B$secondGender IMM_VALUE | Mov B$PossibleImmLabel &TRUE
    ret

L8:  If B$ExpressedLabels = 0
        Call SearchForLabel
        On B$LabelInside = &FALSE,  error D$UnknownParameterPtr
        Mov B$SecondGender DISPLACEMENT
      Else                                     ; Case of 'Mov D$Lab1 Lab2'
        Mov B$ImmInside &TRUE, B$secondGender IMM_VALUE, B$PossibleImmLabel &TRUE
      End_If                                   ; Lab2 checked by PossibleImmLabel
      Mov B$sBit 0
    ret

FirstParameterLabel:
    Mov esi D$LineStart | jmp L1>

SecondParameterLabel:
    Mov esi D$LineStart
L0: lodsb | cmp al Space | jne L0<
L1: lodsb | cmp al Space | jne L1<
    Push esi
      Mov edi D$CodeRefPtr
L0:   lodsb | cmp al 0 | je L0<
              cmp al Separators | jb L9>>
              cmp al 'A' | jb L0<
              cmp al, 'Z' | ja L0<
      On B$esi-2 = AddSign,  Mov B$esi-2 0
      cmp al 'E' | jne L1>
      Push eax
        Call IsItAnEreg                      ; usefull only in case of mem adressing
      Pop eax
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
            On B$FirstOperandWbit <> DWORD_SIZE error D$MissTypePtr
          End_If
L9: Pop esi
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
        Mov B$ThirdGender GENERAL_REG,  B$ThirdReg al,  B$ThirdRegGender ah
        Mov al B$OneOperandwbit,  B$ThirdOperandwbit al
    ret

L3:     Call SearchForImm
        cmp B$ImmInside &FALSE | je L4>
        Mov B$ThirdGender IMM_VALUE
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
 'Mov EDI, My_Data_Label + 8', we store '8'
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
        cmp B$TrueSize BYTE_SIZE | jne L1>
            On D$imm32 < 0_FFFF_FF00,  error D$overflowPtr
            and D$imm32 0FF | jmp L0>
L1:     cmp B$TrueSize WORD_SIZE | jne L0>
            On D$imm32 < 0_FFFF_0000,  error D$overflowPtr
            and D$imm32 0FFFF

L0: Mov edi D$CodeListPtr
    cmp B$TrueSize DWORD_SIZE | je L2>
    cmp B$TrueSize BYTE_SIZE | je L3>

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
L0: cmp al WORD_SIZE | jne L1>
        Mov edi D$CodeListPtr
        Mov B$edi 066 | inc D$CodeListPtr        ; write operand size override prefix
L1:     Mov B$TrueSize al | and al 1 | Mov B$wBit al
ret


FixInstructionType:
    Mov edi D$CodeListPtr
    cmp B$FirstGender GENERAL_REG | jne L3>
      cmp B$SecondGender GENERAL_REG | jne L0>
        Mov B$Operands REG_TO_REG | Call Store2Wbit | ret
L0:   cmp B$SecondGender MEM_LABEL | jne L1>
        Mov B$Operands MEM_TO_REG | Call Store2Wbit | ret
L1:   cmp B$SecondGender DISPLACEMENT | je L2>
      cmp B$SecondGender IMM_VALUE | jne L9>>
L2:     Mov B$Operands IMM_TO_REG | Call StoreFirstWbit | ret

L3: cmp B$FirstGender MEM_LABEL | jne L6>
      cmp B$SecondGender GENERAL_REG | jne L4>
        Mov B$Operands REG_TO_MEM | Call Store2Wbit | ret

L4:   cmp B$SecondGender DISPLACEMENT | je L5>
      cmp B$SecondGender IMM_VALUE | jne L9>
L5:
        Mov B$Operands IMM_TO_MEM | Call StoreFirstWbit | ret

L6: cmp B$FirstGender IMM_VALUE | jne L9>
      cmp B$SecondGender GENERAL_REG | jne L7>
        Mov B$Operands REG_TO_IMM | ret
L7:   cmp B$SecondGender IMM_VALUE | jne L9>
        Mov B$Operands IMM_TO_IMM
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
        Push esi
            Mov esi D$LineStart | Call StoreCodeLabel  ; in 'LabelList'
            inc D$CodeLabelsCounter
        Pop esi
        Mov D$LineStart esi                     ; new instruction's first letter after label
        If B$esi = ColonSign
            inc esi | inc D$LineStart           ; '::' for .export in DLL.
        End_If
    jmp L0<                                     ; Another Label following?:
L9: ret


ClearParameters:                                ;(clear parameters data area)
    Push edi
        Mov al 0,  edi ParametersData,  ecx D$ParametersDataLen | rep stosb
    Pop edi
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
 encodage of Apis calls is not done in 'Encode' but here with a memory indirect Call:
  1111 1111 : mod 010 r/m
 the 3 routines are linked by jumps when needed (not by calls)
;;

[ApiFound: D$ ?]

; Called with esi pointing to the next Char after 'TextSign'. Called from 'SearchForImm'
; because 'xxxx' may as well be a immediate.


; Previous holding of numbered Api (!!!...):
;
;    Push edi
;        Mov edx D$ebx               ; Table 1 pointers Ptr
;        Mov ebx D$ebx+16            ; table 2 pointers Ptr
;        add edx ecx | add ebx ecx
;
;L0:     Mov edi D$edx               ; pointer to function name
;        test edi 0_8000_0000 | jz L1>
;L5:         lodsb | cmp al TextSign | jne L5<
;                Pop edi | jmp EncodeApiCall

[ApiDis: D$ ?]

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
    test edi 0_8000_0000 ZERO L2>
        If B$esi <> '0'
            Push esi | jmp L4>
        End_If

        Mov eax edi | xor eax 0_8000_0000

        Push esi, ebx, eax
            While B$esi = '0' | inc esi | End_While
            Mov ebx 0
H0:         lodsb | sub al '0' | On al > 9, sub al 7
            shl ebx 4 | or bl al
            cmp B$esi TextSign | jne H0<
        Pop eax

        If eax = ebx
            Pop ebx | jmp L5>
        End_If

        Pop ebx | jmp L4>

L2: add edi ecx | add edi 2
  ; edi > Function name (+2 is for the leading word).

L3: Push esi
L3:     Mov al B$esi | Mov ah B$edi | inc esi | inc edi | cmp al ah | je L3<
        cmp al TextSign | jne L4>
        cmp ah 0 | je L5>

  ; Not found:
L4: Pop esi

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
L5: Pop esi
  ; Clear the 'DLL.Function':
    While B$esi <> TextSign | dec esi | End_While
    Do
        Mov B$esi 0 | inc esi
    Loop_Until B$esi = TextSign
    Mov B$esi 0

    Mov eax ebx
    If D$SavingExtension = '.SYS'
        add eax DRIVERDEFAULT | jmp L1>
    Else_If D$FL.RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
        add eax D$LinkerDllDefault
L1:     Push ebx
          ; Comments in 'FillCodeSymbols':
            Mov ebx D$CodeRefPtr, B$ebx 0FF ;, D$ebx+1 edi, B$ebx+5 EOI
            Mov B$ApiDis &TRUE
            add D$CodeRefPtr 1 ;6
          ; The CodeRef thingies are now completed by the 'StoreDis', in order
          ; to assume the Relocations of Api Calls in DLLs.
        Pop ebx
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


; 'DB 0....' encounted:

StoreFlatData:
    Push eax
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
    Pop eax

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

____________________________________________________________________________________________

[InstructionsCounter: D$ ?]

EncodeLines:
    Mov B$ErrorLevel 0, D$CodeLabelsCounter 0
ReCodeLine:
    Mov esi D$CodeSourceB
    Mov D$StatementsCounter 1, D$InstructionsCounter 0

    Move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4
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

[tttnBits: D$ ?]

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

                  |Mov CS:D$EBX 1|
          new esi>>>>>^  ^<<<<<<< old esi > new edi


 after std, lodsb and stosb esi (new line start) point to the first letter of mnemonic:

                  |MOVMOV D$EBX 1|
                      ^<<<<< edi > last new esi  >>> last new LineStart
;;
 __________________________________________________________________________________________

ClearSOP:                   ; esi > ':' sign  >>>  '.xS:'  (this point is a space)
    Push edi
        Mov edi esi | sub esi 3
        std
L0:         lodsb | cmp al EOI | jbe L9>
                    cmp al colonSign | je L9> ; To hold Lines with 'head Labels'.
            stosb | jmp L0<
L9:     cld
        Mov esi edi | inc esi       ; esi > new start of instruction
        Mov D$LineStart esi
    Pop edi
ret

StoreSOP:
    Push esi
L0:     lodsb | cmp al EOI | jbe L9>
        cmp al ColonSign | jne L0<  ; no possible confusion:
            Call WriteSOP           ; labels have been treated before
            Call ClearSOP
        Pop eax                     ; scratch old esi
        Push esi                    ; (re)save new esi
L9: Pop esi
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

    If B$FirstGender <> MEM_LABEL
        On B$SecondGender <> MEM_LABEL error D$LockMemErrorPtr
    End_If
ret

CheckLockInstruction:
;;
  Edi Points to the next Member of the Instruction.
  
  The Instruction can only be ADD, ADC, AND, BTC, BTR, BTS, CMPXCHG, CMPXCH8B, DEC,
  INC, NEG, NOT, OR, SBB, SUB, XADD, XCHG, or XOR
;;

  Push esi
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

    Pop esi
ret

 _________________________________________________________________________________________
 _________________________________________________________________________________________
;;
 When all encoding job is done, we now fill label evocation in code with label adresses
 stored in LabelList. We creat a .reloc section if wanted (DLLs).
;;

[RelocPage: D$ ?
 RelocSectionSize: D$ ?
 FileAlignedRelocationSize: D$ ?]

[StartOfCodeSection: D$ 01000
 StartOfDataSection: D$ 02000] ; redefined any case after encoding

[LastUpSearchLabel: D$ ?]

SearchLocalLabelUp:                     ; EDI >>> LabelList+3 / EDX >>> end of LabelList
L0: Push esi, ecx, ebx                  ; this ESI points to CodeRef <<<<<< ESI >>>>>>
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
        test B$esi FLAG_CODE_LABEL ZERO L4>
        cmp eax ecx | ja L5>            ; is it about the same offset than the Coderef one?
L4:     lodsb                           ; jump over flag byte
        lodsb                           ; jmp over '|'
      jmp L2<
                                        ;          |PREVIOUSNAME|Dword byte|NAME|
                                        ;                      ^ <<<<<<<<  ^
L5: sub ebx 8 | Mov edi ebx             ; set EDI > end of previous LabelList name
    Mov D$LastUpSearchLabel ebx         ; This Value saves a lot of Search tim!!!
    Pop ebx, ecx, esi                   ; ESI back to CodeRef     <<<<<<< SI >>>>>>
    inc esi                             ; For 'L1' label, ESI was at 'L'. Now, ready for
                                                                  ; backward search
    std                                 ; ready to check backward

    align 32

L6: Push esi ecx
      repe cmpsb                 ;  cmp ESI (CodeRef) to EDI (LabelList) until different
      je L9>                            ;  > found
      cmp B$edi+1 EOI | jbe L7>         ; case of LabelList name shorter than CodeRef's one
      Mov ecx 0FF,  al EOI | repne scasb  ; longer: LabelList Di ptr > after next '|'
L7:   sub edi 6                           ;          |PREVIOUSNAME|Dword byte|NAME|
                                          ;                      ^ <<<<<<< ^
    Pop ecx, esi
    On edi <= D$LabelList, error D$UnknownSymbolPtr esi      ; (we are searching upward)
    jmp L6<

L9: cld                                  ; ESI, EDI > start of identical found label name
    Pop ecx esi                          ; ESI > end of name
    On B$LongRelative = &TRUE,  inc esi   ; strip additionnal '|' (old '<')
    add esi 3                            ; ESI > Dword offset of code evocation
    add edi 5                            ; EDI > Dword offset of code or data label
ret


[LastDownSearchLabel: D$ ?]

SearchLocalLabelDown:
L0: Push esi, ecx, ebx
L1:   lodsb                             ; jmp over name in Coderef
        cmp al EOI | jne L1<
        lodsb                           ; stip one '|' ('L1>|..' turn previously 'L1||..')
        On B$LongRelative = &TRUE,  lodsb
        lodsd                           ; local label evocation offset in eax
        test eax RelativeFlag ZERO L2>
          and eax 00_01111111_11111111_11111111_11111111   ;07FFFFFFF   ; strip flag
L2:   Mov ecx eax
      lodsd | Mov D$StatementsCounter eax | Mov esi edi
      On D$LastDownSearchLabel > 0, Mov esi D$LastDownSearchLabel
L3:   Mov ebx esi                       ; save start of LabelList name
L4:   lodsb                             ; jmp over name in LabelList
        cmp esi edx | jae L7>           ; reach end of LabelList? (EDX = Max)
        cmp al EOI | ja L4<
        lodsd
        test B$esi FLAG_CODE_LABEL ZERO L5>
          cmp eax ecx | jae L6>         ; is it about the same offset?
L5:     lodsb                           ; jump over flag byte
        lodsb                           ; jmp over '|'
      jmp L3<
L6: Mov edi ebx                         ; restore start of LabelList name in DI
    Mov D$LastDownSearchLabel ebx       ; This Value saves lot of search time!!!
    Pop ebx ecx esi           ; edi > First label name's letter with possible adress
    Call SearchRegularLabel
    inc esi                             ; strip '>' for short jump
    On B$LongRelative = &TRUE,  inc esi  ; strip next '>' if long jump
ret

L7: Pop ebx ecx esi
    error D$UnknownSymbolPtr esi


; when called, ESI > start of a label name in CodeRef, ECX lenght of this name
; EDI set to >  LabelList + 5  by FillCodeSymbols (start of first label name in LabelList)

SearchRegularLabel:
    Mov al EOI
L0: Push esi ecx
        repe cmpsb              ;  cmp esi (CodeRef/DataRef) to edi (LabelList) until different
        je L8>                  ;  > found
            cmp B$edi-1 al | jbe L3>      ; case of LabelList name shorter than CodeRef's one
            Mov ecx 0FF | repne scasb     ; longer: LabelList edi ptr > after next '|'
L3:         add edi 6                                 ; |LABELNAME|dword FlagByte|NEXTNAME
                                                    ;        EDI ^ >>>>>>>>     ^
            If edi >= edx                       ; edx = LabelList Max.
                Pop ecx, esi
                error D$UnknownSymbolPtr esi
            End_If
    Pop ecx, esi | jmp L0<

L8: cmp B$Relative &TRUE | je L9>           ; if comming from 'SearchLocalLabeldown', OK
      test D$esi RelativeFlag ZERO L9>           ; if not, possible relative:
        Mov D$Relative &TRUE,  B$LongRelative &TRUE     ; this is for 'JMP label'
L9: Pop eax eax             ; if here, found: dummy stack restore. edi > code adress dWord
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
    

L0: Push esi ecx
        repe cmpsb              ;  cmp esi (CodeRef/DataRef) to edi (LabelList) until different
        je L8>                  ;  > found
            cmp B$edi-1 al | jbe L3>      ; case of LabelList name shorter than CodeRef's one
            Mov ecx 0FF | repne scasb     ; longer: LabelList edi ptr > after next '|'
L3:         add edi 6                             ; |LABELNAME|dword FlagByte|NEXTNAME
                                                  ;        EDI ^ >>>>>>>>     ^
            If edi >= edx                       ; edx = LabelList Max.
                Pop ecx, esi
                error UnknownSymbol esi
            End_If
    Pop ecx, esi | jmp L0<
;;

L8: Call GetFromQwordCheckSum esi, D$LabelList, D$LabelListLimit

    If eax = 0
        Push D$esi
            pushad
                Call IsItAreg | On eax <> 0, error D$UnexpectedRegPtr, esi
            popad
        Pop D$esi
        error D$UnknownSymbolPtr, esi
    Else
        While B$eax > LowSigns | inc eax | inc esi | End_While | inc eax | inc esi
        Mov edi eax, ecx 0
    End_If

L8: cmp B$Relative &TRUE | je L9>           ; if comming from 'SearchLocalLabeldown', OK
        test D$esi RelativeFlag ZERO L9>           ; if not, possible relative:
            Mov D$Relative &TRUE,  B$LongRelative &TRUE     ; this is for 'JMP label'

L9: ret


; construction of relocation table. called by FillDataSymbols routine when an uncomputed
; adress is found. EDI > code offset of an evocation.

[CodeOrData: D$ ?]

StoreApiReloc: ; NewSearchApiName PreparePeHeader
  ; eat 0FF found at esi, by 'FillCodeSymbols'
    inc esi | Mov edi D$esi | add esi 5
;jE!
StoreReloc: ; 'RelocComments'
    Push eax, ebx, ecx, edi
L0:
      Mov eax edi
      sub eax D$CodeOrData | and eax PAGE_MASK ;| add eax PAGE_SIZE
     ; cmp eax D$RelocPage | je L3>
      cmp eax 01000 | jb L3>>
     ; if in the same page of code, just write (>L3). if not:
     ; writing of reloc page header. Each section begins by a header of two Dwords.
     ; first one is the virtual adress; exemple, 01000 if begins at 00401000.
     ; second one is the total number of octets of the section.
      add D$CodeOrData 01000

If D$RelocSectionSize = 8 ; if unused RelocSection, reuse it for Next
 add D$RelocPage PAGE_SIZE | Mov eax D$RelocPage |
 Mov ecx D$RelocationPtr | Mov D$ecx-8 eax | jmp L0<
End_If
      Push eax, edi
        Mov eax 0 | Mov edi D$RelocationPtr
L1:     test D$RelocationPtr 0011 ZERO L2>                   ; align on Dword boudary
          stosw                                              ; fill with zeros
          add D$RelocationPtr 2 | add D$RelocSectionSize 2

L2:     Mov ebx D$RelocationPtr | sub ebx D$RelocSectionSize | add ebx 4
        Mov eax D$RelocSectionSize | Mov D$ebx eax     ; set second dWord to section size

        add D$RelocPage PAGE_SIZE | Mov eax D$RelocPage | stosd
        add edi 4 | Mov D$RelocationPtr edi | Mov D$RelocSectionSize 8
      Pop edi, eax
jmp L0<< ; don't continue, becouse can be over NEXT-PAGE

L3:   Mov eax edi | sub eax D$CodeOrData | and eax 0FFF| add eax 03000  ; or! eax 03000, RelocTypeFlag
      Mov edi D$RelocationPtr | stosw

      add D$RelocSectionSize 2 | Mov D$RelocationPtr edi
    Pop edi, ecx, ebx, eax
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

ErrorShortDisUp:
    neg eax | sub eax 127
    If eax > 1
        Mov ebx {B$ 'Bytes Up' EOS}
    Else
        Mov ebx {B$ 'Byte Up' EOS}
    End_If

    jmp L1>

ErrorShortDisDown:
    sub eax 080
    If eax > 1
        Mov ebx {B$ 'Bytes Down' EOS}
    Else
        Mov ebx {B$ 'Byte Down' EOS}
    End_If

L1: ;error D$ShortDisPtr

    CustomError D$ShortDisPtr,
                '#1',
                'Int',
                 eax,
                 '#2',
                 'Str',
                 ebx

    Mov B$ErrorLevel 2 | error STR.A.Trash

    ;Mov ebx eax | Mov B$ErrorLevel 12 | error D$ShortDisPtr

[ApplyShortenJump: D$ ?]

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

    Push esi
      Mov ecx 0
L1:   lodsb
        inc ecx                                 ; simple lenght counter of Code symbolics'
        cmp al EOI | ja L1<                     ; lenghts, in CodeRef, including separator
        lodsd | lodsd | Mov D$StatementsCounter eax
    Pop esi                       ; when encoding 'Relative' is either 0 or high bit flag
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
S9:             Push edx
                    Call SearchSortedRegularLabel
                Pop edx

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
    Mov cl B$edi+4 | or B$edi+4 FLAG_DONE
    Mov edi D$esi | add esi 9
  ; ESI > CodeRef offset of evocation > EDI

    ...If D$Relative = &TRUE
      ; no flag > uncomputed label adress: Strip flag (computed signed displacement);
        and edi 07FFFFFFF | sub eax edi
      ; no signed displacement?
        .If B$LongRelative = &FALSE
            test eax DWORD_HIGH_BIT ZERO L6>
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
            ret      ; why ADD and not Mov: exemple: 'ADC ebx MyLabel+2' > '2' previously stored
                ; 'sub ax, 4'  is to jump over storage bytes (L7: idem for one byte storage)
        .End_If
    ...End_If

L9: If D$FL.RelocsWanted = &TRUE ;SavingExtension = '.DLL' ; jE!
        Call StoreReloc
    Else_If D$SavingExtension = '.SYS'
        Call StoreReloc
    End_If

    If cl < FLAG_CODE_LABEL
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

L0: ;Mov edi D$PlainLabelList | add edi 5
    Push esi
        Mov ecx 0
L1:     lodsb
            inc ecx                                 ; simple lenght counter of Code symbolics'
            cmp al EOI | ja L1<                     ; lenghts, in DataRef, including separator
            lodsd | Mov D$DataFillPtr eax           ; doublon de DataListPtr
            lodsd | Mov D$bracketCounter eax
            lodsb | Mov B$DataSign al
            lodsb | Mov D$DataRefPtr esi            ; strip lasting EOI
    Pop esi

    Call SearchSortedRegularLabel | Mov eax D$edi | Mov cl B$edi+4

    cmp cl FLAG_CODE_LABEL | jb D1>               ; 'jb' because:
        add eax D$CodeAjust | jmp D2>                 ; 2 > data / 3 > data+done
D1:     add eax D$DataAjust                           ; 4 > code / 5 > code+done
D2:     or B$edi+4 FLAG_DONE

D2: On B$DataSign = &TRUE, neg eax | Mov edi D$DataFillPtr | add D$edi eax

    If D$FL.RelocsWanted = &TRUE ;SavingExtension = '.DLL' ;jE!
        Call StoreReloc
    Else_If D$SavingExtension = '.SYS'
        Call StoreReloc
    End_If

    Mov esi D$DataRefPtr | cmp esi ebx | jb L0<<
  ret


[DataFillPtr: D$ ?
 DataAjust: D$ ?
 CodeAjust: D$ ?]

Proc SetCodeAdjust:
    Argument @Base

        Mov eax D@Base | add eax D$uBaseOfData | sub eax D$Datalist
        Mov D$DataAjust eax
        Mov eax D@Base | add eax D$uBaseOfCode | sub eax D$Codelist
        sub eax D$uStartOfCode | Mov D$CodeAjust eax
EndP


InitRelocationForData:
    Push edi eax

        If D$SavingExtension = '.SYS'
            Call SetCodeAdjust DRIVERDEFAULT
        Else_If D$SavingExtension = '.DLL'
            Call SetCodeAdjust D$LinkerDllDefault
        Else
            Call SetCodeAdjust LINKERDEFAULT
        End_If

        Mov edi D$RelocationPtr

        Mov eax D$uBaseOfImport | add eax D$ImportTrueSize | AlignOn 01000 eax

        If B$NoResources = &FALSE
            add eax D$uRsrcSize | AlignOn 01000 eax
        End_If

        Mov D$RelocPage eax | stosd

        Mov eax 0 | stosd
        Mov D$RelocationPtr edi | Mov D$RelocSectionSize 8

    Pop eax edi
ret


InitRelocationForCode:
    Push edi eax
        Mov edi D$RelocationPtr

        Mov eax D$uBaseOfImport | add eax D$ImportTrueSize | AlignOn 01000 eax

        If B$NoResources = &FALSE
            add eax D$uRsrcSize | AlignOn 01000 eax
        End_If

        ;add eax D$uDataSize | AlignOn 01000 eax
        Mov eax D$uBaseOfCode
        Mov D$RelocPage eax | stosd

        Mov eax 0 | stosd
        Mov D$RelocationPtr edi | Mov D$RelocSectionSize 8
    Pop eax edi
ret


CloseRelocation:
    Mov eax 0 | Mov edi D$RelocationPtr

L1: test D$RelocationPtr 0011 ZERO L2>              ; align on Dword boudary
        stosb                                       ; fill with zeros
        inc D$RelocationPtr | inc D$RelocSectionSize
        jmp L1<

L2: Mov ebx D$RelocationPtr | sub ebx D$RelocSectionSize | add ebx, 4
    Mov eax D$RelocSectionSize | Mov D$ebx eax      ; set second dword to section size
ret

@RelocComments:
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

BuildRelocationAndFillSymbols:                      ; >>> StoreReloc <<<
    Call InitRelocationForData
        Mov B$ErrorLevel 3
        Move D$CodeOrData D$DataList
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
        Move D$CodeOrData D$DataList
        Mov eax D$uDataSize | AlignOn 0200 eax | add D$CodeOrData eax

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
    AlignOn 0200 eax | Mov D$FileAlignedRelocationSize eax
ret


SortPlainLabelList:
    Mov eax D$PlainLabelList, eax D$eax

    Push eax

        add eax 4

        Call VirtualAlloc SortTempoMemory,
                          eax

    Pop ecx
    Push ecx
        Mov edi D$SortTempoMemory, esi D$PlainLabelList
        shr ecx 2 | inc ecx | rep movsd

        Move D$SortSource D$SortTempoMemory | add D$SortSource 5
        Move D$SortDestination D$PlainLabelList | add D$SortDestination 5

        Mov D$SortStringNumber 0 | Mov esi D$SortDestination

L0:     lodsb | cmp al EOI | jne L0<
        add esi 6
        inc D$SortStringNumber
        cmp esi D$EndOfPlainLabelList | jb L0<

    Pop ecx

    Call SortLabelStringsBySize

    Call VirtualFree SortTempoMemory

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
            Push ecx
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

            Pop ecx
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

[PlainLabelList: D$ ?
 EndOfPlainLabelList: D$ ?]

BuildPlainLabelList:
    Mov eax D$LabelList, eax D$eax | inc eax

    Call VirtualAlloc PlainLabelList,
                      eax

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
    Mov eax D$CodeListPtr | AlignOn 0200 eax
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

[CheckSumsRecords: D$ ? ? ? ? # 010000
 CheckSumsLinkedRecords: D$ ? ? ? ? # 020000
 PointerToCheckSumsLinkedRecords: D$ ?
 CheckSumsEnd:]

ClearQwordCheckSum:
    Mov edi CheckSumsRecords, eax 0
    Mov ecx CheckSumsEnd | sub ecx CheckSumsRecords | shr ecx 2
    rep stosd

    Mov D$PointerToCheckSumsLinkedRecords CheckSumsLinkedRecords
ret

[CheckSumImage: D$ ?]

SaveCheckSumTable:
    pushad
        Mov ecx PointerToCheckSumsLinkedRecords | sub ecx CheckSumsRecords

        Call VirtualAlloc CheckSumImage,
                          ecx

        shr ecx 2

        Mov esi CheckSumsRecords, edi D$CheckSumImage | rep movsd
    popad
ret

RestoreCheckSumTable:

    pushad

        Mov ecx PointerToCheckSumsLinkedRecords | sub ecx CheckSumsRecords | shr ecx 2

        Mov esi D$CheckSumImage, edi CheckSumsRecords | rep movsd


        Call VirtualFree CheckSumImage

    popad

ret

____________________________________________________________________________________________

CheckSum64:
  ; esi -> Name
    Mov eax 0, ebx 0, ecx 0

    While B$esi > SPC ;LowSigns
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
            Move D$CheckSumsRecords+ecx+8 D@Pointer
          ; D$CheckSumsRecords+ecx+12 = 0
        .Else
L1:         If D$CheckSumsRecords+ecx+12 = 0
                Move D$CheckSumsRecords+ecx+12 D$PointerToCheckSumsLinkedRecords
            Else
                Mov edi D$CheckSumsRecords+ecx+12
                While D$edi+12 <> 0 | Mov edi D$edi+12 | End_While
                Move D$edi+12 D$PointerToCheckSumsLinkedRecords
            End_If

            Mov edi D$PointerToCheckSumsLinkedRecords
            Mov D$edi eax
            Mov D$edi+4 ebx
            Move D$edi+8 D@Pointer
            ;Mov eax D$PointerToCheckSumsLinkedRecords | add eax 16
          ; D$edi+12 = 0
            ;Mov D$PointerToCheckSumsLinkedRecords eax
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
                    Push esi
                        Mov esi D@Pointer | Call CheckSum64
                    Pop esi

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

[CheckSumsPixelsCount: D$ ?
 CheckSumsLinkedRecordsPixels: D$ ?]

TestRepartition:
    Mov D$CheckSumsPixelsCount 0, D$CheckSumsLinkedRecordsPixels 0

    Call 'USER32.BeginPaint' D$H.EditWindow, PAINTSTRUCT | Mov D$STRUCT.EditData@HDC eax
    Call 'USER32.GetClientRect' D$H.EditWindow, RECT

    Mov esi CheckSumsRecords

L0: Mov eax D$esi | or eax D$esi+4
    If eax <> 0
        Mov eax esi | sub eax CheckSumsRecords | shr eax 4
        Mov ebx eax | shr eax 8 | and ebx 0FF
        inc D$CheckSumsPixelsCount
        Call 'GDI32.SetPixel' D$STRUCT.EditData@HDC, eax, ebx, 0
    End_If

    add esi 16 | cmp esi CheckSumsLinkedRecords | jb L0<

L0: Mov eax D$esi | or eax D$esi+4
    If eax <> 0
        Mov eax esi | sub eax CheckSumsLinkedRecords | shr eax 4
        Mov ebx eax | shr eax 8 | and ebx 0FF | add eax 0100
        inc D$CheckSumsPixelsCount
        inc D$CheckSumsLinkedRecordsPixels
        Call 'GDI32.SetPixel' D$STRUCT.EditData@HDC, ebx, eax, 0FF
    End_If

    add esi 16 | cmp esi PointerToCheckSumsLinkedRecords | jb L0<

    Call 'USER32.ReleaseDC' D$H.EditWindow, D$STRUCT.EditData@HDC
    Call 'USER32.EndPaint' D$H.EditWindow, PAINTSTRUCT

    If D$CheckSumsPixelsCount > 0

        ; Wait for user action
L0:     Call 'USER32.PeekMessageA' STRUC.MSG,
                                   D$H.MainWindow,
                                   &WM_KEYDOWN,
                                   &WM_RBUTTONUP,
                                   &PM_REMOVE

        cmp D$STRUC.MSG@message &WM_LBUTTONUP | je S0>

        cmp D$STRUC.MSG@message &WM_RBUTTONUP | je S0>

        cmp D$STRUC.MSG@message &WM_KEYDOWN | jne L0<

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
        Call 'USER32.MessageBoxA' D$H.MainWindow,
      {B$ "The Symoblics'CheckSums'Table can be viewed only after a Compilation." EOS},
      {B$ 'Nothing to show' EOS}, &MB_OK

    End_If
S0:
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


[NewWinEquatesMem: D$ ?
 dWordsLenOfEquatesList: D$ ?]

; Called from 'Main', each time RosAsm is started:

NewBuildWin32Equates: ; 'CheckSumsRecords' Call OpenEquFiles
    Call OpenEquFiles | On D$FL.Includes = &FALSE ret

    ;Call VerifyEquatesFileConformity

    Call CountEquates ; >>> 'NumberOfEquates'
    ;hexprint D$NumberOfEquates
    Push D$NumberOfEquates

        Mov esi D$EquateIncMemory

        Mov D$PointerToCheckSumsLinkedRecords CheckSumsLinkedRecords

L0:     Mov ebx esi | While B$ebx > SPC | inc ebx | End_While | Mov B$ebx 0

        Call CheckSum64 | Call CheckSum16
    ; ebx:eax = CheckSum64 // ecx = CheckSum16

    ; Get the Hexa Value into edx:
        Push eax
            inc esi  ; skip over the space
            Mov edx 0
L1:         shl edx 4 | Mov al B$esi | sub al '0' | On al > 9, sub al 7
            or dl al
            inc esi | cmp B$esi CR | ja L1<
            add esi 2
        Pop eax

        .If D$CheckSumsRecords+ecx = 0
            On D$CheckSumsRecords+ecx+4 <> 0, jmp L1>
            Mov D$CheckSumsRecords+ecx eax
            Mov D$CheckSumsRecords+ecx+4 ebx
            Move D$CheckSumsRecords+ecx+8 edx
        .Else
L1:         If D$CheckSumsRecords+ecx+12 = 0
                Move D$CheckSumsRecords+ecx+12 D$PointerToCheckSumsLinkedRecords
            Else
                Mov edi D$CheckSumsRecords+ecx+12
                While D$edi+12 <> 0
                    ;Call NoDuplicationOfEquates
                    Mov edi D$edi+12
                End_While
                ;Call NoDuplicationOfEquates
                Move D$edi+12 D$PointerToCheckSumsLinkedRecords
            End_If

            Mov edi D$PointerToCheckSumsLinkedRecords
            Mov D$edi eax
            Mov D$edi+4 ebx
            Move D$edi+8 edx
            add D$PointerToCheckSumsLinkedRecords 16
        .End_If

        dec D$NumberOfEquates | cmp D$NumberOfEquates 0 | jne L0<<

    Pop D$NumberOfEquates

  ; Now, store the Win32 Equates CheckSums Table into Memory:

    Mov ecx D$PointerToCheckSumsLinkedRecords | add ecx 010 | sub ecx CheckSumsRecords

    Call VirtualAlloc NewWinEquatesMem,
                      ecx

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

[EquateFound: D$ ?]

NewSearchForWinEquate:
    Push esi
        Call NewGetEquates

        If B$EquateFound = &FALSE
            Mov esi STR.A.Trash | error D$BadWinEquPtr ; error8
        End_If

        or D$imm32 eax
    Pop esi

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


[NewWinEquateLenght: D$ ?]

; Called from the Assembler and from 'RightClick'

NewGetEquates: ;New GetEquates: RightClick
    Mov edi STR.A.Trash
    While B$esi > SPC
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

L1: Mov B$edi 0 | sub edi STR.A.Trash | Mov D$NewWinEquateLenght edi

  ; Simplified 'GetFromQwordCheckSum':
    Mov esi STR.A.Trash

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
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
