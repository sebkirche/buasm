TITLE Strings         ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;;
  TITLE where to save the BUAsm Messages Strings having a possible counter-part
  in the national languages Files.
  
  In the BUAsmFiles Folder, if a 'BUAsmStrings.xx' is found out, the substitution
  is done by 'OpenStringsFile'.
  
  Organization:
  
  * The default English Strings are declared the usual way.
  
  * The [TABLE.STR.A.Error] contains 2 Dwords Records. Typically:
  
      STRING1 ErrorMessageTitlePtr: ErrorMessageTitle
      
      Where: 
      - 'STRING1' is the StringID (Number 1)
      - 'ErrorMessageTitlePtr' is the Substitution Label
      - 'ErrorMessageTitle' is the Pointer to the default English Strings, in the Source
      
   The StringIDs purpose is to introduce some flexibility in the management:
   
   1) The ID as found in the 'BUAsmStrings.xx' File and in the [TABLE.STR.A.Error]
   are simple Ordinal, and must be given, in both cases, in the Ordinal order.
   
   2) Some Strings and IDs may be missing.
   
   * How it works:
   
   When a National Language String is found out, it is loaded for once, in Memory.
   
   CRLF are zeroed and the Strings are parsed:
   
   When some '$$$$25' is found, the 'OpenStringsFile' translates '25' into Binary
   and searches the matching Record ID, inside the [TABLE.STR.A.Error]. Once this
   Record is pointed out, the String Pointer is overwritten, in order to point
   to the char after '$$$$25 '.
   
   In failures cases, an error Message is send.
   
   __________________________
   For the BUAsm volunteers:
   
   When you develop your TITLE, do not use this. Simply write your own errors
   Messages inside your TITLE, the usual way.
   
   Then, only when you think that your developements are more or less in a fixed
   state, considering the errors Messages, Implement the Internationalization.
   
   How to:
   
   1) Move your Strings here, after the existing ones.
   2) If there is not enough 'STRINGXXX' available, add the ones you need.
   3) Extend the [TABLE.STR.A.Error] to hold your own Strings.
   4) In your Source, replace all the concerned 'StringSymbol' by 'D$StringSymbolPtr'.
   2) Take a copy of 'BUAsmStrings.en', and append your Strings.
   
   For the Equates Ordinals, you can go to the next 1000 Boundary, so that each
   Volunteer (or purpose) will have a set of 1000 Strings IDs reserved, in order
   to ease a little bit the future Maintainance. For example, the Equal Pre-Parser
   Strings could begin at 2000, the Debugger Strings at 3000, and so on... I plan
   to use 1000 for the BUAsm Menu, and some other things.
   
   Once done, send me, all at once: Your TITLE, The Strings TITLE and the
   'BUAsmStrings.en' File.
;;



[ErrorMessageTitle: B$ ' error' EOS]  ; Error,  OutOnError

; List of error messages:

[EmptyString: B$   '                                                        ' EOS]
[MissingFile: B$ 'File not found' EOS]
[BusyFile: B$ 'Unable to open: System mangling destination file' EOS]
[CreationFile: B$ 'Error when trying writting file' EOS]
[OpenFile: B$ 'File To open Not found' EOS]
[MissFile: B$ 'File not found' EOS]
[NoReadFile: B$ 'Unable to read file' EOS]
[MissEquateVal: B$ 'Missing equate value' EOS]
[EquateLabel: B$ 'An Equate name can not be followed by a Colon Char' EOS]
[TextEquate: B$ 'Missing ">" Char at the end of Text Equate' EOS]
[MacPara: B$ 'Bad parameters number in macro evocation' EOS]
[MissingParameter: B$ 'Bad parameters number in Macro evocation' EOS]
[MacNumber: B$ 'Bad # construction' EOS]
[BadMacroDirection: B$ 'Always ">", for Macros Multiple Parameters. Never "<"' EOS]
[TooBigX: B$ '#x limit is 9' EOS]
[TooMuchParam: B$ 'Too much Parameters found when unfolding' EOS]
[InfiniteLoop: B$ 'An infinite loop has been generated when unfolding this macro' EOS]
[MacrosOverFlow: B$ 'More than 4000 Bytes outputed at once, when unfolding this Macro!!!' EOS]
[UnknownData: B$ 'Unknown data type' EOS]
[UnknownSize: B$ 'Unknown Size marker' EOS]
[Mem3D: B$ '64 Bits (Q$...) mem size expected for all 3D Now instructions' EOS]
[NewHmem: B$ '"H$" now reserved for futher developement. Use "R$" or "F$" instead' EOS]
[PrefetchMem: B$ 'Prefetch/w/0/1/2/NTA syntax: > PrefetchXXX B$Address' EOS]
[DsizeForLab: B$ 'D$ needed for Labels data' EOS]
[Unknown: B$ 'Unknown text structure. May be internal error' EOS]
[BadSeparator: B$ 'Unexpected separator' EOS]
[BinType: B$ 'Invalid binary number' EOS]
[HexType: B$ 'Invalid Hexa number' EOS]
[DezimalType: B$ 'Invalid decimal number' EOS]
[OutOfRange: B$ 'Out of range value' EOS]
[DefEquate: B$ 'This equate or macro symbol is already used' EOS]
[DefReEquate: B$ 'This equate symbole is not in use' EOS]
[TooMuchLabels: B$ 'More than 2 labels is this expression' EOS]
[BadLabel: B$ 'Bad character in Label name' EOS]
[LabelDup: B$ 'Duplicate label definition' EOS]
[SymbolDup: B$ 'Duplicate symbol definition' EOS]
[OrphanColon: B$ 'Orphan Colon sign encounted' EOS]
[LocalDefLabel: B$ 'Local label not allowed in data definition' EOS]
[NoLocalCall: B$ 'You cannot Call for a Local Label' EOS]
[NotAnOpcode: B$ 'Unknown mnemonic' EOS]
[OverByte: B$ 'Value out of byte range' EOS]
[OverWord: B$ 'Value out of word range' EOS]
[OverDword: B$ 'Value out of Dword range' EOS]
[LenSize: B$ 'Leading Len can only be dWord or word' EOS]
[BadReal: B$ 'Bad real number declaration' EOS]
[OrphanPrefix: B$ 'An instruction prefix can not stand alone' EOS]
[DoubleSOP: B$ 'Double Segment override not allowed' EOS]
[NotSegment: B$ 'Unresolved segment override' EOS]
[Parameter: B$ 'Unvalid parameter' EOS]
[Double0: B$ 'Only one "&0" automatic Label per Macro' EOS]
[MissingSign: B$ 'Need of equal or colon sign' EOS]
[MissingSeparator: B$ 'Missing space at end of a Data String declaration' EOS]
[BadMacroLoop: B$ 'Bad placement of Loop instruction in Macro' EOS]
[TooMuch: B$ 'Too much operands found' EOS]
[DBsize: B$ 'Hexa Octets values expected for Code Bytes' EOS]
[LockError: B$ 'No Lock Prefix with this instruction' EOS]
[LockMemError: B$ 'Lock: One Member must be Memory' EOS]
[MissingOperand: B$ 'Missing operand' EOS]
[NotEnough: B$ 'Missing operand' EOS]
[MemMem: B$ 'Expressions are allowed for only one memory adressing parameter' EOS]
[Parenthesis: B$ 'Unpaired parenthesis' EOS]
[ParaMacro: B$ 'ad ParaMacro' EOS]
[MacroVariableIndice: B$ 'Macros Variables are from "&1" to "&100" (Decimal)' EOS]
[NestedMacroVariable: B$ 'Imbricated declaration of Macro Variable' EOS]
[MarkerBefore: B$ '$ize Marker wanted before Real Expression' EOS]
[MarkerAfter: B$ '$ize Marker wanted after Real Expression' EOS]
[ExpressionMember: B$ 'Immediate only in Expressions' EOS]
[NoDataLocalLabel: B$ 'No Local Labels in Data: They are reserved for Code' EOS]
[RealNotation: B$ 'Real Expression: only +-*/ Operators, no nested Parenthesis' EOS]
[ExpressionSign: B$ 'Bad operator in Expression' EOS]
[DataLoopNumber: B$ 'For Huge Data Declaration, use Virtual (?) Data or Call for VirtualAlloc' EOS]
[SmallDataLoop: B$ 'What do you mean with "repeat less than 2 times???..."' EOS]
[BadLoop: B$ 'Loop with no previous Data Declaration' EOS]
[VDataLoopNumber: B$ 'For huge Virtual Tables: VirtualAlloc or [Configuration]/[Bad Habits]' EOS]
[TooMuchExpression: B$ 'Expression number too big in result or before multiplication' EOS]
[ExpressionNOT: B$ 'NOT must be at leading Pos of an Expression member' EOS]
[ExpressionSHR: B$ 'SHR / SHL Parameter over Byte size in Expression' EOS]
[OpenText: B$ 'Unpaired text delimiter' EOS]
[Orphanbracket: B$ 'Unpaired square bracket' EOS]
[UnexpectedCRLF: B$ 'Unexpected CR LF after Bracket' EOS]
[MissingSeparator1: B$ 'Missing Separator before Text Delimiter' EOS]
[MissingSeparator2: B$ 'Missing Separator after Text Delimiter' EOS]
[UnPairedNestedBrackets: B$ 'Unpaired {Nested} Brackets' EOS]
[NestedBrackets: B$ "'{}' chars are reserved for nested Declarations" EOS]
[PseudoLocal: B$ "Pseudo local '@Label' before any Plain Label declaration" EOS]
[DoubleSIB: B$ 'Only one Index and one base allowed in SIB' EOS]
[EspIndex: B$ 'ESP can t be use as an index in SIB' EOS]
[Expression: B$ 'Impossible effective address parameter' EOS]
[ScaleValue: B$ 'Scale possible values are 2, 4, 8 and extensions to 3, 5, 9' EOS]
[ESPsib: B$ 'ESP can t be used as index register' EOS]
[DoubleIndex: B$ 'Only one index allowed in expression' EOS]
[DoubleLabel: B$ 'Double label or unknown error' EOS]
[UnknownParameter: B$ 'unable to resolve this parameter' EOS]
[EnterStack: B$ 'Enter Stack size goes up to 0FFFC and must be 4 Bytes aligned' EOS]
[EnterLevel: B$ 'Enter level goes from 0 to 31' EOS]
[LeaInstead: B$ 'To retrieve Label+imm Adress, use LEA. Exemple: " > lea ebx D$Label+8" ' EOS]
[MixType: B$ 'these two parameters types not allowed together' EOS]
[MissType: B$ 'UnFitting sizes of operands' EOS]
[OperandsTypes: B$ 'Failure of Analyze in operands types for this mnemonic' EOS]
[BadAlign: B$ 'The Alignment must be a power of two between 4 and 0100' EOS]
[OperandSize: B$ 'Bad operand size' EOS]
[FPregNotAssumed: B$ "Can't guess Registers for this FP Instruction" EOS]
[LeaTypes: B$ '"lea reg, mem" only. For "lea eax imm", use "Mov eax imm"' EOS]
[LeaSize: B$ 'There is no Byte form for lea' EOS]
[EndingImm: B$ 'Third imm parameter wanted' EOS]
[Xmarker: B$ 'X$Memory parameter wanted' EOS]
[OverFlow: B$ 'Over flow of immediate number' EOS]
[UnknownSymbol: B$ 'This symbolic name does not fit with any label' EOS]
[WhatIsThis: B$ 'What is this?' EOS]
[UnAble: B$ 'BUAsm has been unable to assemble this instruction' EOS]
[NotYetSign: B$ 'Only + - signs allowed here, or see [Expressions] in Help' EOS]
[NotYetMnemo: B$ 'Sorry, this mnemonic is not yet implemented. Information needed' EOS]
  ;ShortDis: B$ 'Need of long dis., >> or << (never on LOOP)' EOS
[TooLongOf: B$ '     Byte(s) out of range' EOS]
[ShortDis: B$ "Need of long dis., >> or << (never on LOOP)
#1 #2 out of range" EOS]

[NoPlainLabelForLoop: B$ 'Local Up Short Label is required for LOOP' EOS]
[LongLoop: B$ 'Local Up Short Label is required for LOOP' EOS]
[NoPlainLabelForJECX: B$ 'Short Local Label (up or down) is required for JCXZ / JECXZ' EOS]
[LongDis: B$ 'Only Short displacement allowed with this mnemonic' EOS]
[NForbiden: B$ 'N symbol not allowed here. Use L' EOS]
[MixedLen: B$ "You can't mix 'head LEN' and 'Free LEN'" EOS]
[BadLoopNumber: B$ 'decimal value expected for data declaration loop number' EOS]
[VirtualData: B$ 'Bad Virtual Data' EOS]
[NestedLoop: B$ 'Sorry, data loop can not be nested' EOS]
[NeedByteSize: B$ 'Byte size required' EOS]
[OnlyAcc: B$ 'Only accumulator with this instruction' EOS]
[TxtTooMuch: B$ 'Text parameter exceeding 4 bytes' EOS]
[GPregister: B$ 'A register must be general purpose' EOS]
[VERRword: B$ 'VERR/VERW / LTR parameter must be 16 bits register or memory' EOS]
[WishEreg: B$ 'Memory adressing expects full 32 bit general purpose registers' EOS]
[BadApi: B$ "Api calls form is: Call 'Module.Function'" EOS]
[NoApi: B$ 'No Api Call found in source' EOS]
[DllNotFound: B$ 'Internal error: user called DLL not found at compile time' EOS]
[ApiNotFound: B$ 'Internal error: user called API not found at compile time' EOS]
[NoLocal: B$ 'Local Label not allowed here' EOS]
[NoEntry: B$ 'Entry Point not found (should be "Main:" if not modified)' EOS]
[BadFPUcond: B$ 'FPU available conditions: B, E, BE, NA, U, NB, AE, NE, NZ, NBE, A, NU' EOS]
[ST0wished: B$ 'One of these two reg must be ST0' EOS]
[STwished: B$ 'Only ST0, ST1, ...., ST7  regs with this mnemonic' EOS]
[FADDPreg0: B$ 'FADDP second Reg must be ST0' EOS]
[FSUBPreg0: B$ 'FSUBP second Reg must be ST0' EOS]
[BadFpChar: B$ 'Unexpected Char inside Float Expression' EOS]
[NoAdressee: B$ 'Internal: Unknown adressee' EOS]
[BadMemRelease: B$ 'Internal: Fail of Memory release' EOS]
[NotPE: B$ 'No source in this PE. Disassemble it?' EOS]
[NotPeExe: B$ 'This is not a PE File' EOS]
[BadWinEqu: B$ 'Unknown Win Equates name' EOS]
[IdTooSmall: B$ 'ID base number must be between 1 and 65535' EOS]
[IdTooBig: B$ 'ID base number must be between 1 and 65535' EOS] ;;;1000 and 32000' EOS
[BadLibName: B$ 'Wrong Module name' EOS]
[DoubleFunction: B$ 'Impossible: Same Function in 2 DLLs       ' EOS]
[BadFunctionName: B$ 'Wrong api name' EOS]
[BadOrdinal: B$ 'Wrong api Ordinal' EOS]
[NoAapi: B$ 'No ending "A" here' EOS]
[MacVarOverFlow: B$ 'Too long Data (or infinite storage loop) for Macro "&x="' EOS]
[MacVarNumber: B$ 'Too high Number for a Macro Variable' EOS]
[MissingApiA: B$ 'Ending "A" (or "W") wanted' EOS]
;[BadPreParse: B$ 'PREPARSE is for: Alternates, Equal' EOS]
[MissingCLASSname: B$ 'Missing Class Name' EOS]
[NoParentClass: B$ 'Parent CLASS not found when unfolding Inheritance' EOS]
[SizeOfResource: B$ 'Too Big Resources. Big Images or Sounds should be left in Files' EOS]
[NumerAsSymbol: B$ 'A Number cannot be a Symbolic Name' EOS]
[UnexpectedReg: B$ 'Unexpected Register or too short Symbol' EOS]
[TextKilling: B$ 'This Macro removes a Text Delimiter' EOS]
[NestedMacroLoop: B$ 'Split your Macro: No Nested Loop here' EOS]
[ConditionalLoop: B$ 'A Macro Loop cannot be inside a Conditional "If"' EOS]
[UnpairedMacroIf: B$ 'Unpaired If in the Macro Declaration' EOS]
[BadCMIndice: B$ 'Bad Conditional Indice (should be from 1 to 99)' EOS]
[BadConditionalmacro: B$ 'Bad Conditional macro' EOS]
[ZZZZreserved: B$ 'Symbols in "ZZZZZZZZ" form are reserved for Automatic Labels' EOS]
[BadSyntaxBeforeComma: B$ "Bad syntax. Inside your macro and before the comma, you inserted Naked Operator Signs (+ - / ^ *).
BUAsm will fix this automatically in order to you continue assembling your app.
  Please, consider fixing the sytax manually the next time you assemble your program, to avoid this warning." EOS]


;;
  This Table is for holding the national languages, if some 'BUAsmStrings.xx' is
  found out, at launch time. 
;;

[TABLE.STR.A.Error:
 1 ErrorMessageTitlePtr: ErrorMessageTitle
 2 @EmptyStringPtr: EmptyString
 3 @MissingFilePtr: MissingFile
 4 BusyFilePtr: BusyFile
 5 @CreationFilePtr: CreationFile
 6 @OpenFilePtr: OpenFile
 7 @MissFilePtr: MissFile
 8 @NoReadFilePtr: NoReadFile
 9 MissEquateValPtr: MissEquateVal
 10 EquateLabelPtr: EquateLabel
 11 TextEquatePtr: TextEquate
 12 MacParaPtr: MacPara
 13 MacNumberPtr: MacNumber
 14 BadMacroDirectionPtr: BadMacroDirection
 15 TooBigXPtr: TooBigX
 16 @TooMuchParamPtr: TooMuchParam
 17 InfiniteLoopPtr: InfiniteLoop
 18 MacrosOverFlowPtr: MacrosOverFlow
 19 UnknownDataPtr: UnknownData
 20 UnknownSizePtr: UnknownSize
 21 Mem3DPtr: Mem3D
 22 NewHmemPtr: NewHmem
 23 PrefetchMemPtr: PrefetchMem
 24 DsizeForLabPtr: DsizeForLab
 25 UnknownPtr: Unknown
 26 BadSeparatorPtr: BadSeparator
 27 BinTypePtr: BinType
 28 HexTypePtr: HexType
 29 DezimalTypePtr: DezimalType
 30 OutOfRangePtr: OutOfRange
 31 @DefEquatePtr: DefEquate
 32 @DefReEquatePtr: DefReEquate
 33 TooMuchLabelsPtr: TooMuchLabels
 34 BadLabelPtr: BadLabel
 35 @LabelDupPtr: LabelDup
 36 SymbolDupPtr: SymbolDup
 37 OrphanColonPtr: OrphanColon
 38 @LocalDefLabelPtr: LocalDefLabel
 39 NoLocalCallPtr: NoLocalCall
 40 NotAnOpcodePtr: NotAnOpcode
 41 OverBytePtr: OverByte
 42 OverWordPtr: OverWord
 43 OverDwordPtr: OverDword
 44 LenSizePtr: LenSize
 45 BadRealPtr: BadReal
 46 @OrphanPrefixPtr: OrphanPrefix
 47 @DoubleSOPPtr: DoubleSOP
 48 NotSegmentPtr: NotSegment
 49 ParameterPtr: Parameter
 50 MissingParameterPtr: MissingParameter
 51 Double0Ptr: Double0
 52 @MissingSignPtr: MissingSign
 53 MissingSeparatorPtr: MissingSeparator
 54 BadMacroLoopPtr: BadMacroLoop
 55 TooMuchPtr: TooMuch
 56 @DBsizePtr: DBsize
 57 LockErrorPtr: LockError
 58 LockMemErrorPtr: LockMemError
 59 MissingOperandPtr: MissingOperand
 60 NotEnoughPtr: NotEnough
 61 @MemMemPtr: MemMem
 62 ParenthesisPtr: Parenthesis
 63 ParaMacroPtr: ParaMacro
 64 MacroVariableIndicePtr: MacroVariableIndice
 65 NestedMacroVariablePtr: NestedMacroVariable
 66 MarkerBeforePtr: MarkerBefore
 67 MarkerAfterPtr: MarkerAfter
 68 @ExpressionMemberPtr: ExpressionMember
 69 NoDataLocalLabelPtr:  NoDataLocalLabel
 70 RealNotationPtr: RealNotation
 71 ExpressionSignPtr: ExpressionSign
 72 DataLoopNumberPtr: DataLoopNumber
 73 SmallDataLoopPtr: SmallDataLoop
 74 BadLoopPtr: BadLoop
 75 VDataLoopNumberPtr: VDataLoopNumber
 76 TooMuchExpressionPtr: TooMuchExpression
 77 ExpressionNOTPtr: ExpressionNOT
 78 ExpressionSHRPtr: ExpressionSHR
 79 OpenTextPtr: OpenText
 80 OrphanBracketPtr: OrphanBracket
 81 UnexpectedCRLFPtr: UnexpectedCRLF
 82 MissingSeparator1Ptr: MissingSeparator1
 83 MissingSeparator2Ptr: MissingSeparator2
 84 UnPairedNestedBracketsPtr: UnPairedNestedBrackets
 85 @NestedBracketsPtr: NestedBrackets
 86 @PseudoLocalPtr: PseudoLocal
 87 @DoubleSIBPtr: DoubleSIB
 88 @EspIndexPtr: EspIndex
 89 ExpressionPtr: Expression
 90 ScaleValuePtr: ScaleValue
 91 ESPsibPtr: ESPsib
 92 DoubleIndexPtr: DoubleIndex
 93 @DoubleLabelPtr: DoubleLabel
 94 UnknownParameterPtr: UnknownParameter
 95 EnterStackPtr: EnterStackPtr
 96 @EnterLevelPtr: EnterLevel
 97 @LeaInsteadPtr: LeaInstead
 98 MixTypePtr: MixType
 99 MissTypePtr: MissType
 100 OperandsTypesPtr: OperandsTypes
 101 BadAlignPtr: BadAlign
 102 OperandSizePtr: OperandSize
 103 FPregNotAssumedPtr: FPregNotAssumed
 104 LeaTypesPtr: LeaTypes
 105 LeaSizePtr: LeaSize
 106 EndingImmPtr: EndingImm
 107 XmarkerPtr: Xmarker
 108 OverFlowPtr: OverFlow
 109 UnknownSymbolPtr: UnknownSymbol
 110 WhatIsThisPtr: WhatIsThis
 111 @UnAblePtr: UnAble
 112 NotYetSignPtr: NotYetSign
 113 NotYetMnemoPtr: NotYetMnemo
 114 ShortDisPtr: ShortDis
 115 @TooLongOfPtr: TooLongOf
 116 NoPlainLabelForLoopPtr: NoPlainLabelForLoop
 117 LongLoopPtr: LongLoop
 118 NoPlainLabelForJECXPtr: NoPlainLabelForJECX
 119 LongDisPtr: LongDis
 120 NForbidenPtr: NForbiden
 121 MixedLenPtr: MixedLen
 122 BadLoopNumberPtr: BadLoopNumber
 123 VirtualDataPtr: VirtualData
 124 NestedLoopPtr: NestedLoop
 125 NeedByteSizePtr: NeedByteSize
 126 OnlyAccPtr: OnlyAcc
 127 TxtTooMuchPtr: TxtTooMuch
 128 GPregisterPtr: GPregister
 129 VERRwordPtr: VERRword
 130 WishEregPtr: WishEreg
 131 BadApiPtr: BadApi
 132 NoApiPtr: NoApi
 133 @DllNotFoundPtr: DllNotFound
 134 @ApiNotFoundPtr: ApiNotFound
 135 NoLocalPtr: NoLocal
 136 NoEntryPtr: NoEntry
 137 BadFPUcondPtr: BadFPUcond
 138 ST0wishedPtr: ST0wished
 139 STwishedPtr: STwished
 140 FADDPreg0Ptr: FADDPreg0
 141 FSUBPreg0Ptr: FSUBPreg0
 142 @BadFpCharPtr: BadFpChar
 143 @NoAdresseePtr: NoAdressee
 144 @BadMemReleasePtr: BadMemRelease
 145 NotPEPtr: NotPE
 146 NotPeExePtr: NotPeExe
 147 BadWinEquPtr: BadWinEqu
 148 IdTooSmallPtr: IdTooSmall
 149 IdTooBigPtr: IdTooBig
 150 BadLibNamePtr: BadLibName
 151 DoubleFunctionPtr: DoubleFunction
 152 BadFunctionNamePtr: BadFunctionName
 153 BadOrdinalPtr: BadOrdinal
 154 NoAapiPtr: NoAapi
 155 MacVarOverFlowPtr: MacVarOverFlow
 156 @MacVarNumberPtr: MacVarNumber
 157 MissingApiAPtr: MissingApiA
 158 BadPreParsePtr: BadPreParsePtr
 159 @MissingCLASSnamePtr: MissingCLASSname
 160 @NoParentClassPtr: NoParentClass
 161 @SizeOfResourcePtr: SizeOfResource
 162 NumerAsSymbolPtr: NumerAsSymbol
 163 UnexpectedRegPtr: UnexpectedReg

 164 TextKillingPtr: TextKilling
 165 NestedMacroLoopPtr: NestedMacroLoop
 166 UnpairedMacroIfPtr: UnpairedMacroIf
 167 ConditionalLoopPtr: ConditionalLoop
 168 @BadCMIndicePtr: BadCMIndice
 169 BadConditionalmacroPtr: BadConditionalmacro
 170 BadSyntaxBeforeCommaPtr: BadSyntaxBeforeComma
____________________________________________________________________________________________
; Debugger strings

 3000 StrRunPtr: StrRun
 3001 StrStepIntoPtr: StrStepInto
 3002 StrStepOverPtr: StrStepOver
 3003 StrStepPtr: StrStep
 3004 StrReturnPtr: StrReturn
 3005 StrRetPtr: StrRet
 3006 StrTerminatePtr: StrTerminate
 3007 StrPausePtr: StrPause
 3008 StrHoldOnBpPtr: StrHoldOnBp
 3009 StrInstStepPtr: StrInstStep
 3010 StrSrcStepPtr: StrSrcStep
 3011 StrShowAllPtr: StrShowAll
 3012 StrFontPtr: StrFont
 3013 StrCPUInfoPtr: StrCPUInfo
 3014 StrFPUStatusPtr: StrFPUStatus
 3015 StrShowCodeAtPtr: StrShowCodeAt
 3016 StrAboutPtr: StrAbout
 3017 StrDbgHelpPtr: StrDbgHelp

 3040 StrDataFmtPtr: StrDataFmt

 3050 StrContinuePtr: StrContinue
 3051 StrBreakPtr: StrBreak
 3052 StrSettingsPtr: StrSettings
 3053 StrInformationPtr: StrInformation
 3054 StrHelpPtr: StrHelp

 3060 StrShowInMemInspPtr: StrShowInMemInsp
 3061 StrShowPInMemInspPtr: StrShowPInMemInsp
 3062 StrShowDeclPtr: StrShowDecl
 3063 StrBreakOnWPtr: StrBreakOnW
 3064 StrBreakOnRWPtr: StrBreakOnRW
 3065 StrSortByNamePtr: StrSortByName
 3066 StrSortByAddrPtr: StrSortByAddr

 3080 StrShowInvokePtr: StrShowInvoke
 3081 StrShowAllCallsPtr: StrShowAllCalls
 3082 StrHideModCallsPtr: StrHideModCalls
 3083 StrHideIMCallsPtr: StrHideIMCalls
 3084 StrShowLocalsPtr: StrShowLocals

 3100 FmtHexPtr:      FmtHex
 3101 FmtUDecPtr:     FmtUDec
 3102 FmtSDecPtr:     FmtSDec
 3103 FmtBinaryPtr:   FmtBinary
 3104 FmtFloatPtr:    FmtFloat
 3105 FmtPUBPtr:      FmtPUB
 3106 FmtPSBPtr:      FmtPSB
 3107 FmtPUWPtr:      FmtPUW
 3108 FmtPSWPtr:      FmtPSW
 3109 FmtPUDPtr:      FmtPUD
 3110 FmtPSDPtr:      FmtPSD
 3111 FmtPUQPtr:      FmtPUQ
 3112 FmtPSQPtr:      FmtPSQ
 3113 FmtPFPtr:       FmtPF
 3114 FmtPDPtr:       FmtPD
 3115 FmtHexAnsiPtr:  FmtHexAnsi
 3116 FmtHexDWPtr:    FmtHexDW
 3117 FmtHexWPtr:     FmtHexW
 3118 FmtFloatsPtr:   FmtFloats
 3119 @FmtDoublesPtr:  FmtDoubles
 3120 FmtAsciiPtr:    FmtAscii
 3121 FmtHexCookedPtr: FmtHexCooked

 TABLE.STR.A.ErrorEnd: LP.TABLE.STR.A.Error: TABLE.STR.A.Error]

[StringNamePath: B$ ? # &MAX_PATH]
[H.BUAStringsFile: D$ ?
 BUAsmStringsFilelenght: D$ ?
 BUAsmStringsMemory: D$ ?]
[StringsLanguage: B$ '.en' EOS]
[BUAsmStringsFiles: B$ 'BUAsmStrings' EOS]
[UnicodeStrings: &FALSE]

; Tag Dialog 4800

SetNationalLanguage:

    .If eax = 10
        Mov D$StringsLanguage '.en'
    .Else_If eax = 11
        Mov D$StringsLanguage '.fr'
    .Else_If eax = 12
        Mov D$StringsLanguage '.br'
    .Else_If eax = 13
        Mov D$StringsLanguage '.sp'
    .Else_If eax = 14
        Mov D$StringsLanguage '.zh'
    .Else_If eax = 15
        Mov D$StringsLanguage '.it'
    .Else_If eax = 16
        Mov D$StringsLanguage '.de'
    .Else_If eax = 17
        Mov D$StringsLanguage '.no'
    .Else_If eax = 18
        Mov D$StringsLanguage '.ca'
    .Else_If eax = 201
        Call GetNationalFont
    .End_If

    Call OpenStringsFile
ret


OpenStringsFile:

    Call GetBUAsmFilesPath

    Mov esi BUAsmFilesPath, edi StringNamePath

    While B$esi <> 0 | movsb | End_While

    Mov D$edi 'Lang', B$edi+4 '\' | add edi 5
    Mov esi BUAsmStringsFiles
    While B$esi <> 0 | movsb | End_While | Move D$edi D$StringsLanguage

    Call 'KERNEL32.FindFirstFileA' StringNamePath, STRUC.FindFile

    ...If eax <> &INVALID_HANDLE_VALUE

        Call 'KERNEL32.FindClose' eax

        Call 'KERNEL32.CreateFileA' StringNamePath, &GENERIC_READ,
                                    &FILE_SHARE_READ+&FILE_SHARE_WRITE, 0, &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_NORMAL, 0
        Mov D$H.BUAStringsFile eax

        Call 'KERNEL32.GetFileSize' eax,
                                    0

        Mov D$BUAsmStringsFilelenght eax

        add eax 10

        Call VirtualAlloc BUAsmStringsMemory,
                          eax

        add D$BUAsmStringsMemory 10

        Call 'KERNEL32.ReadFile' D$H.BUAStringsFile, D$BUAsmStringsMemory,
                                 D$BUAsmStringsFilelenght, NumberOfReadBytes, 0

        Call 'KERNEL32.CloseHandle' D$H.BUAStringsFile

        Mov esi D$BUAsmStringsMemory,
            edx esi ;!!! EDX pour ParseNationalUstrings/ParseNationalStrings !!!

        add edx D$BUAsmStringsFilelenght

        If D$StringsLanguage = '.zh'
            Mov B$UnicodeStrings &TRUE | Call ParseNationalUstrings
        Else
            Mov B$UnicodeStrings &FALSE | Call ParseNationalStrings
        End_If

    ...Else
        Mov D$StringsLanguage '.en'

    ...End_If
ret


ParseNationalUstrings:
    .While esi < edx
        ..If W$esi < '1'
            ;
        ..Else_If W$esi <= '9'
            cmp W$esi-8 '$' | jne L9>>
                cmp W$esi-6 '$' | jne L9>>
                    cmp W$esi-4 '$' | jne L9>>
                        cmp W$esi-2 '$' | jne L9>>

          ; Write the Strings Zero End Marks upon the '$':
            Mov W$esi-12 0

          ; Compute the ID Number
L0:         Mov ecx 0

L0:         lodsw
            cmp ax SPC | je L7>
            cmp ax '9' | ja L5>
            cmp ax '0' | jb L5>
                sub ax '0'
                lea ecx D$ecx+ecx*4
                lea ecx D$eax+ecx*2
            jmp L0<

L5:         Call 'USER32.MessageBoxA' D$H.MainWindow, {B$ 'Bad National Strings File. English assumed' EOS},
                                          {B$ 'National Strings' EOS}, 0
            Mov D$StringsLanguage '.en'
            ret

          ; esi point to the beginning of a String, and ecx is the ID Number.
          ; Write it into the 'StringsTable'. Allow missing Ordinal IDs:
L7:         Mov ebx D$LP.TABLE.STR.A.Error

            While D$ebx > ecx
                sub ebx 8 | On ebx < TABLE.STR.A.Error jmp L5<
            End_While
            While D$ebx < ecx
                add ebx 8 | On ebx >= TABLE.STR.A.ErrorEnd jmp L5<
            End_While
            Mov D$ebx+4 esi | Mov D$LP.TABLE.STR.A.Error ebx
        ..End_If

L9:     add esi 2
    .End_While

  ; Write the Strings Zero End Marks for the last Strings:
    Mov W$esi 0
ret


ParseNationalStrings:
    .While esi < edx
        ..If B$esi < '1'
            ;
        ..Else_If B$esi <= '9'
            cmp D$esi-4 '$$$$' | jne L9>>

          ; Write the Strings Zero End Marks:
            Mov B$esi-6 0

          ; Compute the ID Number
L0:         Mov ecx 0

L0:         lodsb
            cmp al SPC | je L7>
            cmp al '9' | ja L5>
            cmp al '0' | jb L5>
                sub al '0'
                lea ecx D$ecx+ecx*4
                lea ecx D$eax+ecx*2
            jmp L0<

L5:         Call 'USER32.MessageBoxA' D$H.MainWindow, {B$ 'Bad National Strings File. English assumed' EOS},
                                          {B$ 'National Strings' EOS}, 0
            Mov D$StringsLanguage '.en'
            ret

          ; esi point to the beginning of a String, and ecx is the ID Number.
          ; Write it into the 'StringsTable'. Allow missing Ordinal IDs:
L7:         Mov ebx D$LP.TABLE.STR.A.Error

            While D$ebx > ecx
                sub ebx 8 | On ebx < TABLE.STR.A.Error jmp L5<
            End_While
            While D$ebx < ecx
                add ebx 8 | On ebx >= TABLE.STR.A.ErrorEnd jmp L5<
            End_While
            Mov D$ebx+4 esi | Mov D$LP.TABLE.STR.A.Error ebx
        ..End_If

L9:     inc esi
    .End_While

  ; Write the Strings Zero End Marks for the last Strings:

    Mov B$esi 0
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
  Custom Strings Routines:
  
  See usage downward, at 'TestCustomString'.
;;
[CustomError
 If D$StringsLanguage = '.zh'
    Call CopyUnicodeToTrash1 #1
    BuildTheUnicodeMessage #2>L
 Else
    Call CopyToTrash1 #1
    BuildTheAsciiMessage #2>L
 End_If]

[BuildTheUnicodeMessage | Call CustomUnicodeStringProc #1, #2, #3 | #+3
    mov esi D$Trash1Ptr, edi STR.A.Trash
    While W$esi > 0 | movsw | End_While | movsb]

[BuildTheAsciiMessage | Call CustomStringProc #1, #2, #3 | #+3
    mov esi D$Trash1Ptr, edi STR.A.Trash
    While B$esi > 0 | movsb | End_While | movsw]

[CustomString | Call CopyToTrash1 #1 | CustomString2 #2>L]

[CustomString2 | Call CustomStringProc #1, #2, #3 | #+3
 mov esi D$Trash1Ptr, edi STR.A.Trash
 While B$esi > 0 | movsb | End_While | movsb]

Proc CopyToTrash1:
    Argument @Source
    Uses esi

        Mov esi D@Source, edi Trash1
        While B$esi <> 0 | movsb | End_While | movsb

        Mov D$Trash1Ptr Trash1, D$Trash2Ptr Trash2
EndP


Proc CopyUnicodeToTrash1:
    Argument @Source
    Uses esi

        Mov esi D@Source, edi Trash1
        While W$esi <> 0 | movsw | End_While | movsw

        Mov D$Trash1Ptr Trash1, D$Trash2Ptr Trash2
EndP


Proc CustomStringProc:
    Arguments @Xn, @Type, @Arg
    Uses esi, edi, ebx

        Mov esi D$Trash1Ptr, edi D$Trash2Ptr

        Mov eax D@Xn
        While W$esi <> ax
            movsb | On B$esi = 0, ExitP
        End_While

        If D@Type = 'Int'
            Mov eax D@Arg | Call WriteEax

        Else_If D@Type = 'Str'
            Push esi
                Mov esi D@Arg
                While B$esi <> 0 | movsb | End_While
            Pop esi

        End_If

        add esi 2 | While B$esi <> 0 | movsb | End_While | Mov B$edi 0

       Exchange D$Trash1Ptr D$Trash2Ptr
EndP


Proc CustomUnicodeStringProc:
    Arguments @Xn, @Type, @Arg
    Uses esi, edi, ebx

        Mov esi D$Trash1Ptr, edi D$Trash2Ptr
;;
   '@Xn' stands is the '#1', '#2', '#3' thingies. We make it Unicode, that is,
   for example:
   
   '#1' (031, 023) >>> 0, 031, 0, 023 >>> '#', 0, '1', 0
;;
        Mov eax D@Xn | movzx ebx ah
        shl ebx 16 | or eax ebx  | and eax 0FF00FF

        While D$esi <> eax
            movsw | On W$esi = 0, ExitP
        End_While

        If D@Type = 'Int'
            Mov eax D@Arg | Call WriteEaxUnicode

        Else_If D@Type = 'Str'
            Push esi
                Mov esi D@Arg
                While B$esi <> 0 | movsb | Mov B$edi 0 | inc edi | End_While
            Pop esi

        End_If

        add esi 4 | While W$esi <> 0 | movsw | End_While | Mov W$edi 0

        Exchange D$Trash1Ptr D$Trash2Ptr
EndP


WriteEaxUnicode:
    Mov ebx eax

L3: If ebx = 0
        Mov W$edi '0' | add edi 2 | ret
    End_If

    Push 0-1

L0: Mov eax ebx | shr ebx 4 | and eax 0F

    Mov al B$STR.A.Hexa+eax
    Push eax
    cmp ebx 0 | ja L0<
    Mov W$edi '0' | add edi 2
L0: Pop eax | cmp eax 0-1 | je L9>
    Mov B$edi al | inc edi | Mov B$edi 0 | inc edi | jmp L0<
L9: ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
