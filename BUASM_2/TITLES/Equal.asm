TITLE Equal
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
 Author name: Scarmatil (Julien OTELE MANDA)

 eMail: < scarmatil@gmail.com>
 
 Last Update: 27-07-2005      
____________________________________________________________________________________________

 ProcessEqual  EqualParser  WriteOperationCode  IntegratedFunctionManagement
 EvalOneEquation   GetStringType  ProcessarrayDestination  ComputeOperationsIn_EPPSourceEquation
 
 
 EPP_All_Data    
 
____________________________________________________________________________________________
HOW TO USE THE EQUAL PREPARSER: 

This pre-parser actually groups three differents parsers :


=> Evaluation parser:  (Expression1 OP Expression2) 

- Expression1 and Expression2 are the two expression you want to compare, 
  SourceExpression can be a single register (Common or FPU), memory data or an equation combining the 
  operators below with any memory data, register or constant.

- OP is the comparison operator, it can be one of the following:  =  <  >

- The whole expression (including the parentheses are replaced by a boolean DWord value containing 
  the result of the evaluation of this expression.


=> String maker:  str(Destination) = Source

- Destination can be a 32 bits register, a Data Label (i.e a constant) or a Memory DWord. 

- Source can be a string, a 32 bits register, a Data Label or a Memory DWord. 
 
- /!\ The destination is a pointer, you have to make sure there is enough space to write the string /!\

- /!\ Each source value is a null-terminated string, make sure that the trailing null character is not missing. /!\ 

example :
[CRLF: W$ 0D0A 0] [strbuf:B$ ? #128] [strbuf2:B$ ? #128]
 > ecx = strbuf2
 > str(ecx) = 'Scarmatil'
 > str(strbuf) = 'hop !' CRLF ecx ' et ' ecx ' sont sur un bateau. ' ecx " tombe a l'eau? " CRLF "Qui reste-t-il ?"
 


=> Equation Parser:   Destination = SourceExpression 

- Destination can be a register (Common or FPU) or memory Data (including array, see below for the syntax).

- SourceExpression can be a single register (Common or FPU), memory data or an equation combining the 
  operators below with any memory data, register or constant.
  It can also be a call, providing that the return value is in EAX.
    
- You can use parentheses with no limit of nesting (except your time while compiling)

- To display the code output by the preparser inside RosAsm, simply double-click on the Destination 
  operand and choose 'unfold' in the menu.

* Operator list : 
    => ^       ; power operator, the first operand has to be strictly positive 
    => * / + - : common operator
    => // **   ; signed operators 

* Available functions : 
  - cos(), sin(), abs(), tan(), sqrt(), ln(), log10(), log2(), exp() and atan().  

* Conversions :
 The parser can perform any conversion as long as they are not impossible (store a DWord in a byte !?):
  > ST0 = (D$a+D$b)/6+(D$c*7)       ; integer result stored in a FPU register
  > Q$MyQValue = B$MyByteVal  
  > ...
    
* Signed operations:
  To force signed integer division and multiplication you can use '//' and '**' operators .
  Notice that if one of the operand is a floating point value (6.2,T$MyVal,...) the operation
  will be signed even with '*' and '/' operators.

* Addressing support
    - X$(expression)  : this will be interpreded as : X$expression (like in basic RosAsm syntax)
    - Without parentheses if the writing is the same that in basic RosAsm syntax when it is possible
      and the operation otherwise.
ex:
(1) - edx = D$(eax+3)     ; Mov edx D$eax+3
(2) - edx = (D$eax)+3     ; Mov edx D$eax | add edx 3 
(3) - edx = D$eax+3       ; Mov edx D$eax+3
(3) - edx = D$eax+D$ebx   ; Mov edx D$eax | add edx D$ebx  
(4) - edx = D$(eax+D$ebx) ; add eax D$ebx | Mov edx D$eax 

* Array support:    
    - X$Array(index)  : this will be interpreded as : X$Array+index*(X_size)

ex:- [MyTable: D$ 'ZERO' 'ONE ' 'TWO ' 'THRE' 'FOUR' 'FIVE']
     eax = D$MyTable(4)  ; eax = 'FOUR'
                 
                        
* Supported data size (both in source and destination): B$, W$, D$, Q$, F$, R$ and T$    


/!\  No FINIT is written by the parser, write it yourself at the beginning of       /!\
     the code if you use expressions containing reals .
    
examples :

 > eax = 1
 > edx = &TRUE+(2*2)
 > D$Value1 = D$Value2
 > W$Value1+2 = W$Value2+2
 > D$Handle = Call 'DLLNAME.DllFunction', Para1, ...    
 > ST2 = (tan(45)/sqrt(3.5+9)-1/T$MyReal)               
 > eax = (D$a+D$b)/6+(D$c*7)
 > D$Val3 = (3 * D$a + 12) / (24 * D$b) - D$c * D$c * 18 / (13 + D$z) 
 > ebx = sin(45) 
 > T$FloatNumber = sqrt(abs(7+-3*ebx)+R$FpVal)
 > eax = F$MyArrayName(eax)
 > D$DestArray(ecx) = W$SourceArray(eax)   

        ____________________________________________________________________________________________
 
  Basically this parser works as follow :
  It first copies the equation into EPP_SourceEquation and the destination operand into 
  EPP_DestOperand 
  Then the equation is formated to ease the parsing.

  The parsing begins and the first closing parenthesis is searched. Each time the parser meets an 
  opening parenthesis, it keeps its position.
  Once the first closing parenthesis is found, the operations in these parentheses are processed
  in this order: ^ then * and / and finally + and - (to keep prority).  
  
  Then each operation type is processed and the corresponding code is written thanks to 
  the macro EPP_Code                                                          
;;
____________________________________________________________________________________________
EqualParser:
    Mov esi D$CodeSourceA, edi D$CodeSourceB | Mov ecx esi | add ecx D$StripLen
    Mov B$ErrorLevel 0, D$bracketCounter 0      ; error0
   ; Mov D$StatementsCounter 1, D$InstructionsCounter 0
    move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4

    .While esi < ecx
        ..If B$esi = TextSign
            movsb | While B$esi <> TextSign | movsb | End_While
        ..Else_If_And B$esi+1 = '=', B$esi = Space, B$esi+2 = Space
            Call ParseOneEqualLine
        ..Else_If B$esi = openSign
            Call EvaluateEquation
        ..End_If

        If B$esi = EOI
            add D$StatementsPtr 4
        Else_If B$esi = OpenBracket
            add D$StatementsPtr 4
        Else_If B$esi = OpenVirtual
            add D$StatementsPtr 4
        End_If

        movsb
    .End_While

; post-parsing optimization :

    jmp L1>
    While ebx <> CODE_NOT_FOUND
        Call DeleteFinalCodeLine ebx edi | sub edi eax

L1:     EPP_FindCodePos 'end_equal||' D$CodeSourceB edi
        Mov ebx eax
    End_While
    Mov B$edi-1 EOI, B$edi 0

L2:
    sub edi D$CodeSourceB | Mov D$StripLen edi
    Exchange D$CodeSourceA D$CodeSourceB

ret
____________________________________________________________________________________________
EvaluateEquation:
    ; Look for Equation between parentheses which can be evualuated
    ; There three possibles comparison operators (=,<,>)
    ; ex: (eax*7=F$a/2)  ;  (D$Array(ecx)<exp(ecx))
    Mov eax INITIAL_STATE
    lea edx D$esi+1

    While B$edx > EOI
        If_And B$edx = closeSign, eax = FOUND_EQUATION_TEST
            jmp L1>
        End_If
        On B$edx = openSign, inc eax
        On B$edx = closeSign, dec eax

        .If_Or B$edx = '=', B$edx = '<', B$edx = '>'
            If eax = INITIAL_STATE
                Mov ebx edx
                Mov eax FOUND_EQUATION_TEST
            Else
                Mov eax 0 ; unpaired parenthesis
            End_If
        .End_If
        inc edx
    End_While
L1:
    If eax = FOUND_EQUATION_TEST
        Call EvalOneEquation esi edx ebx edi
    End_If
ret
____________________________________________________________________________________________
Proc EvalOneEquation:
    ; This routine evaluates each member of the Equation between D@openSign and D@closeSign,
    ; then compare them using de D@CompPos operator (=,<,>) and replace the whole expression
    ; by a boolean value stored in a DWord (&TRUE if the comparison is true &FALSE otherwise)
    Arguments @openSign @closeSign @CompPos @ProducedCode
    Uses ebx, ecx

    ; debug Data
    move D$LastEqualLine   D@ProducedCode

    ; init EPP_Code
    move D$EPP_WrittenCode    D@ProducedCode
    move D$EPP_CodeBegining   D@ProducedCode

    EPP_Code 'PUSH ESI|MOV ESI ESP|'

    push D$EPP_WrittenCode
        Mov D$EPP_WrittenCode EPP_DestOperand
        EPP_Code 'T$&8 '
    pop D$EPP_WrittenCode
    EPP_Code 'SUB ESP 0A|{!8 ESI-0A}|'

    Mov esi D@openSign | inc esi
    Mov edi EPP_SourceEquation
    Mov ebx D@CompPos
    While esi < ebx | movsb | End_While | Mov B$edi EOI

    push D$EPP_CodeBegining
        move D$EPP_CodeBegining D$EPP_WrittenCode
        Call ComputeOperationsIn_EPPSourceEquation &FALSE
        Call StoreResultInFirstOperand
        Call OptimizeEqualPreparserCode EPP_CODE_BEGIN EPP_CODE_END
        Call UpdateLocalDataShift
    pop D$EPP_CodeBegining

    push D$EPP_WrittenCode
        Mov D$EPP_WrittenCode EPP_DestOperand
        EPP_Code 'T$&9 '
    pop D$EPP_WrittenCode
    EPP_Code 'SUB ESP 0A|{!9 ESI-014}|'

    Mov esi D@CompPos | inc esi
    Mov edi EPP_SourceEquation
    Mov ebx D@closeSign
    While esi < ebx | movsb | End_While | Mov B$edi EOI

    push D$EPP_CodeBegining
        move D$EPP_CodeBegining D$EPP_WrittenCode
        Call ComputeOperationsIn_EPPSourceEquation &FALSE
        Call StoreResultInFirstOperand
        Call OptimizeEqualPreparserCode EPP_CODE_BEGIN EPP_CODE_END
        Call UpdateLocalDataShift
    pop D$EPP_CodeBegining

    EPP_Code 'FLD T$!9|FLD T$!8|'

    Mov ebx D@CompPos
    If B$ebx = '='
        EPP_Code 'FCOMP ST1|FSTSW AX|AND AX 04400|CMP AX 04000|JNE &1|MOV EAX 1|JMP &3|!1:|MOV EAX 0|!3:|'
    Else_If B$ebx = '<'
        EPP_Code 'FCOMP ST1|FSTSW AX|AND AX 0500|CMP AX 0100|JNE &1|MOV EAX 1|JMP &3|!1:|MOV EAX 0|!3:|'
    Else_If B$ebx = '>'
        EPP_Code 'FCOMP ST1|FSTSW AX|AND AX 04500|TEST AX AX|JNE &1|MOV EAX 1|JMP &3|!1:|MOV EAX 0|!3:|'
    End_If
    EPP_Code 'SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'

    ; Replaces the two expressions and the comparison operator by the DWord result
    Mov esi D@openSign | While B$esi > EOI | dec esi | End_While | inc esi
    Mov ebx D@openSign
    Mov edi D$EPP_WrittenCode
        While esi < ebx | movsb | End_While
    Mov D$EPP_WrittenCode edi
    EPP_Code 'D$?'
    Mov esi D@closeSign | inc esi |While B$esi > EOI | inc esi | End_While | Mov ebx esi
    Mov esi D@closeSign | inc esi
    Mov edi D$EPP_WrittenCode
        While esi <= ebx | movsb | End_While
    Mov D$EPP_WrittenCode edi
    Mov B$edi meEOI
    EPP_Code 'MOV ESP ESI|POP ESI||'

    ; remove what was written before D$EPP_CodeBegining (which was initially before the expression)
    Mov edi D$EPP_CodeBegining
    Mov eax 0
    If B$edi-1 <> EOI
        Mov B$edi-1 EOI
        sub edi 2 | While B$edi > EOI | dec edi | End_While | inc edi
        Call DeleteFinalCodeLine edi D$EPP_WrittenCode
    End_If

    ; Updates edi
    Mov edi D$EPP_WrittenCode | sub edi eax

    Call TranslateExpressions
EndP
____________________________________________________________________________________________
ParseOneEqualLine:
    Mov ebx esi, edx edi   ; D$esi = ' = '  ; eax = 1  (keep ebx at ' = ')

  ; Go to start of Instruction, and abort if, for example "If eax = 1" is encounted
  ; because of such user Defined Equates. In such Cases, we will have either one more
  ; Space backward, or, maybe, '[', if encounted inside a Declaration:
    While B$esi-1 > EOI
        On B$esi-1 = ColonSign, jmp L2>
        dec esi | dec edi
        .If B$esi <= OpenVirtual
            If B$esi = Space
                jmp L1>
            Else_If B$esi = OpenBracket
                jmp L1>
            Else_If B$esi = OpenVirtual
L1:             Mov esi ebx, edi edx | ret
            End_If
        .End_If
    End_While

L2: Call ProcessEqual ebx edi

    ;Sets esi (in CodeSourceA) and edi (in CodeSourceB) on the end of Line (EOI)
    Mov esi ebx
    While B$esi <> EOI | inc esi | End_While

    Mov edi edx
    While B$edi <> EOI | inc edi | End_While
    On B$edi-1 = meEOI, Mov B$edi-1 EOI     ;The code mustn't end with 0102 (meEOI,EOI) (or RosAsm hangs when unfolding)
ret
____________________________________________________________________________________________
Proc ProcessEqual:
    ; D@EqualPos is the position of ' = '
    Arguments @EqualPos @ProducedCode
    pushad

    ; debug Data
    move D$LastEqualLine   D@ProducedCode
    On D$FirstEqualLine = &TRUE, move D$FirstEqualLine D@ProducedCode

    ; init EPP_Code
    move D$EPP_WrittenCode D@ProducedCode
    move D$EPP_CodeBegining   D@ProducedCode

    ; copies the Destination operand in EPP_DestOperand
    Mov esi D@EqualPos
    While B$esi <> EOI | dec esi | End_While | inc esi
    Mov edi EPP_DestOperand
    While B$esi > Space | movsb | End_While | Mov B$edi Space

    ; copies the line into EPP_SourceEquation
    Mov esi D@EqualPos | add esi 3
    Mov edi EPP_SourceEquation
    While B$esi > EOI | movsb | End_While | Mov B$edi EOI

    Call IsStringMaker | If eax = &TRUE | popad | ExitP | End_If

    Call IsItaCall | If eax = &TRUE | popad | ExitP | End_If

    Call ReserveGlobalFunctionAdress

    Call ComputeOperationsIn_EPPSourceEquation &FALSE

    Call ProcessArrayDestination

    Call StoreResultInFirstOperand

    EPP_Code 'end_equal||'

    Call OptimizeEqualPreparserCode EPP_CODE_BEGIN EPP_CODE_END

    Call UpdateLocalDataShift

    Call TranslateExpressions

    ;Call ShowSource 0 7

    popad
EndP
____________________________________________________________________________________________
IsStringMaker:

    If_Or W$EPP_DestOperand <> 'ST', B$EPP_DestOperand+2 <> 'R', B$EPP_DestOperand+3 <> openSign
        Mov eax &FALSE | ret
    End_If

    EPP_Code 'PUSH ESI|PUSH EDI|'

    lea esi D$EPP_DestOperand+4 | Mov D$EPP_Operand1 esi
    If_And B$esi+1 = memMarker, B$esi <> 'D'
        error EqualPreparser_InvalidStringDest_Type
    Else_If_And B$esi >= '0', B$esi <= '9'
        error EqualPreparser_InvalidStringDest_Numeric
    End_If

    While B$esi > openSign | inc esi | End_While | Mov B$esi Space

    EPP_Code 'MOV EDI #1|'
    Mov ecx EPP_SourceEquation
    .While B$ecx > EOI
        If B$ecx = TextSign
            Mov D$EPP_TextPointer ecx
            EPP_Code '{&1:B$ #text 0}|MOV ESI !1|&2:|CMP B$ESI 0|JZ &3|MOVSB|JMP !2|!3:|'
            inc ecx | While B$ecx > TextSign | inc ecx | End_While
            On B$ecx = TextSign, inc ecx

        Else
            Mov D$EPP_Operand2 ecx
            EPP_Code 'MOV ESI #2|&2:|CMP B$ESI 0|JZ &3|MOVSB|JMP !2|!3:|'
            While B$ecx <> Space | inc ecx | End_While
        End_If
        While B$ecx = Space | inc ecx | End_While
    .End_While

    EPP_Code 'MOV B$EDI 0|POP EDI|POP ESI||'
    Mov eax &TRUE

ret
____________________________________________________________________________________________
ReserveGlobalFunctionAdress:
    ; Initialize labels to be used later for global functions declaration and call. (not used)

    Call CreateNoMeanlabel
    Mov edi EPP_atofAddress | Mov B$edi+8 0
    Call CreateNoMeanlabel
    Mov edi EPP_ftoaAddress | Mov B$edi+8 0
ret
____________________________________________________________________________________________
Proc ComputeOperationsIn_EPPSourceEquation:
    ; Writes code from the most nested parenthesis to the least one.
    ; The parser search for the first couple of parentheses with no nested parenthesis inside and then
    ; computes the operations in the operator's priority order.
    ; When an operation immediately computable is found while searching for theses couples of parentheses, this
    ; operation is computed first. (See TestIfThisOperationIsComputable)
    ;
    ; This allows keep the very last results on the top of the FPU Stack, so that no move is needed.
    ; For instance with this equation:
    ; > T$res = (T$a-T$b)*T$e+(T$b+T$c)
    ;
    ; - if the operations between parentheses were computed first this would lead to
    ;       > T$res = ST1*T$e+ST0    ;with ST0=T$b+T$c ; ST1=T$a-T$b
    ;       > T$res = ST2*ST0+ST1    ;with ST0=T$e     ; ST1=T$b+T$c ; ST2=T$a-T$b
    ;   The first operation to compute is ST2*ST0, then ST2 must be copied on TOS with 'FLD ST2'
    ;
    ; - On the other hand, if we compute an operation as soon as is it possible, we have:
    ;       > T$res = ST0*T$e+(T$b+T$c)     ; with ST0=T$a-T$b
    ;       > T$res = ST0+(T$b+T$c)         ; with ST0=(T$a-T$b)*T$e
    ;   Here the '+' operation is not immediately computable, so the parsing continues to
    ;       > T$res = ST1+ST0               ; with ST1=(T$a-T$b)*T$e  ; ST0=T$b+T$c
    ;       > T$res = ST0                   ; with ST0=(T$a-T$b)*T$e+(T$b+T$c)
    ;   All the operations have been made on TOS :)
    Arguments @SecondPass
    Uses esi, edi

    ; add rounding parentheses
    Mov edi EPP_SourceEquation
    Call ShiftLineRight 1 | Mov B$edi openSign
    While B$edi > EOI | inc edi | End_While | Mov B$edi closeSign | Mov B$edi+1 EOI

    Mov eax &FALSE

    ; Checks the parentheses number.
    Mov eax 0
    Mov esi EPP_SourceEquation
    While B$esi > EOI
        On B$esi = openSign, inc eax
        On B$esi = closeSign, dec eax
        inc esi
    End_While
    On eax <> 0, error Parenthesis

    Call FormatEqualLine
    On B$UseCommonAddressingSyntax = &TRUE, Call SetCommonAddressingSyntax
    Call EqualFirstScan D@SecondPass

    Mov D$FPUStackNumber 0
    Mov B$ParsingInsideArray &FALSE
    Mov esi EPP_SourceEquation
    .While B$esi > EOI
        .If B$esi = openSign
            Call CheckIfArray
            Mov ebx esi | inc esi

        .Else_If B$esi = closeSign
            Call WriteCodeForTheseParentheses ebx
            On eax = &FALSE, error EqualPreparser_UnexpectedError
            Call IntegratedFunctionManagement ebx eax
            Mov esi EPP_SourceEquation
            Mov B$ParsingInsideArray &FALSE

        .Else_If B$esi = Space
            inc esi

        .Else_If_Or B$esi < OperatorSigns, B$esi = SignedMulSign, B$esi = SignedDivSign
            Call TestIfThisOperationIsComputable esi
            If eax <> &FALSE
                Call GetOperandTypeAndPos esi
                Call WriteOperationCode eax ecx
                Mov esi EPP_SourceEquation
                Mov B$ParsingInsideArray &FALSE

            Else
                inc esi
            End_If
        .Else
            inc esi
        .End_If
    .End_While

EndP
____________________________________________________________________________________________
SetCommonAddressingSyntax:
; Look for memory data followed by an expression which can be an address
; [i.e. : Memory+register(+,*)constant+(register,constant)+constant)  ]
; and surround the expression with parentheses.

    Mov esi EPP_SourceEquation
    .While B$esi > EOI
L1:     If_Or B$esi <= LowSigns, B$esi = SignedMulSign, B$esi = SignedDivSign
            inc esi
            jmp L1<
        End_If

        Call GetStringType esi
        While B$esi > Space | inc esi | End_While | inc esi
        ...Test_If ax MemoryOperand
            ...If B$esi = addSign
                ..If_And B$esi+2 > LowSigns, B$esi+2 <> SignedMulSign, B$esi+2 <> SignedDivSign
                    add esi 2 | Call GetStringType esi
                    ..Test_If ax constantOperand
                        ReplaceByAddressingOperator plusSign   ; Memory+constant

                    ..Test_Else_If ax registerOperand
                        ReplaceByAddressingOperator plusSign   ; Memory+register

                        While B$esi > Space | inc esi | End_While | inc esi
                        .If_Or B$esi = mulSign, B$esi = SignedMulSign
                            add esi 2 | Call GetStringType esi
                            .Test_If ax ConstantOperand
                                ReplaceByAddressingOperator addressSign   ; Memory+register*constant

                                While B$esi > Space | inc esi | End_While | inc esi
                                If B$esi = addSign
                                    add esi 2 | Call GetStringType esi
                                    Test_If ax ConstantOperand
                                        ReplaceByAddressingOperator plusSign   ; Memory+register*constant+constant
                                    Test_End
                                Else_If B$esi = subSign
                                    add esi 2 | Call GetStringType esi
                                    Test_If ax ConstantOperand
                                        ReplaceByAddressingOperator minusSign   ; Memory+register*constant-constant
                                    Test_End
                                End_If
                            .Test_End

                        .Else_If B$esi = addSign
                            add esi 2 | Call GetStringType esi
                            Test_If ax ConstantOperand
                                ReplaceByAddressingOperator plusSign    ; Memory+register+constant
                            Test_End
                        .Else_If B$esi = subSign
                            add esi 2 | Call GetStringType esi
                            Test_If ax ConstantOperand
                                ReplaceByAddressingOperator minusSign    ; Memory+register-constant
                            Test_End
                        .End_If
                    ..Test_End
                ..End_If
            ...End_If
        ...Test_End
    .End_While

ret
____________________________________________________________________________________________
Proc GetStringType:
    ; D@StringPos points on the string whose type is to be found. (it can be terminated by EOI,meEOI,Space or &NULL)
    Arguments @StringPos
    Uses esi, ecx

    Mov esi D@StringPos
    Call IsARegister D@StringPos
    ..If eax = &FALSE
        .If B$esi+1 = memMarker
            or ax MemoryOperand
            If B$esi = 'T'
                or ax (FloatingPointOperand or TByteValue)
            Else_If B$esi = 'R'
                or ax (FloatingPointOperand or RealValue)
            Else_If B$esi = 'F'
                or ax (FloatingPointOperand or FloatValue)
            Else_If B$esi = 'Q'
                or ax QWordValue
            Else_If B$esi = 'D'
                or ax DWordValue
            Else_If B$esi = 'W'
                or ax WordValue
            Else_If B$esi = 'B'
                or ax ByteValue
            End_If
        .Else_If_And B$esi >= '0', B$esi <= '9'
            Mov ecx esi
            While B$ecx > Space
                If B$ecx = pointSign
                    or ax FloatingPointOperand
                    jmp L1>
                Else
                    inc ecx
                End_If
            End_While
L1:         or ax (ConstantOperand or NumericOperand)
        .Else_If_And B$esi = minusSign, B$esi+1 >= '0', B$esi+1 <= '9'
            or ax (ConstantOperand or NumericOperand)
        .Else
            While B$esi > Space | inc esi | End_While | inc esi
            If B$esi = openSign
                or ax FunctionOperand
            Else
                or ax ConstantOperand
            End_If
        .End_If
    ..End_If

EndP
____________________________________________________________________________________________
Proc ProcessArrayDestination:
    ; If the destination operand is an array (ex: F$Array(ecx+3) ) The current state is saved and
    ; the address of the array is computed and stored in EDI (resp. ECX). Then the state of the current operation is
    ; restored with X$EDI (resp. X$ECX) as new destination operand (with X the size of array's elements)
    Local @AddressRegister

    Mov D$DestinationArrayRegister &FALSE
    Mov esi EPP_DestOperand
    .While B$esi > Space
        .If B$esi = openSign
            or B$UsedRegisters (EPP_EDI or EPP_ECX)
            Call BackupFirstResults

            ; Copy address in EPP_SourceEquation
            Mov edi EPP_SourceEquation | Mov B$edi openSign | inc edi
            While B$esi > Space | movsb | End_While | Mov B$edi closeSign | inc edi
            If B$EPP_DestOperand+2 <> openSign
                Mov B$edi mulSign | inc edi | Call GetAddressingCoef EPP_DestOperand | Mov B$edi al | inc edi
                Mov B$edi addSign | inc edi
                lea esi D$EPP_DestOperand+2 | While B$esi <> openSign | movsb | End_While
            End_If
            Mov B$edi EOI

            ; defines addressing register
            If_Or W$EPP_SourceEquation_Backup+1 = 'CX', W$EPP_SourceEquation_Backup+1 = 'CH',
                                                        W$EPP_SourceEquation_Backup+1 = 'CL'
                jmp L1>
            Else_If_And W$EPP_SourceEquation_Backup+2 = 'CX', B$EPP_SourceEquation_Backup+1 = 'E'
                jmp L1>
            Else_If_And W$EPP_SourceEquation_Backup+4 = 'CX', B$EPP_SourceEquation_Backup+3 = 'E',
                                                              B$EPP_SourceEquation_Backup+2 = memMarker
L1:             Mov D@AddressRegister 'EDI'
                Mov D$DestinationArrayRegister EPP_EDI
            Else
                Mov D@AddressRegister 'ECX'
                Mov D$DestinationArrayRegister EPP_ECX
            End_If
            move D$EPP_DestOperand D@AddressRegister | Mov B$EPP_DestOperand+3 Space

            Mov al B$UsedRegisters_Backup | Mov B$UsedRegisters al
            Call WriteRestoringCodeForUsedRegisters

            Call ComputeOperationsIn_EPPSourceEquation &TRUE

            ; Stores result in EDI/ECX
            Mov D$EPP_LastStorageStatement_InMemory StrNull
            Mov D$EPP_LastStorageStatement          StrNull
            Mov D$EPP_Operand1 EPP_SourceEquation | inc D$EPP_Operand1
            Mov D$EPP_Operand2 EPP_DestOperand
            Mov edi EPP_SourceEquation | Call StoreIn32BitsRegister
            Call ImmediateCodeParser D$EPP_LastStorageStatement_InMemory
            Call ImmediateCodeParser D$EPP_LastStorageStatement

            Call RestoreFirstResults
            move D$EPP_DestOperand+2 D@AddressRegister | Mov B$EPP_DestOperand+5 Space

            ExitP
        .End_If

        inc esi
    .End_While

EndP
____________________________________________________________________________________________
WriteRestoringCodeForUsedRegisters:
    ; This writes the code which restores from the stack the registers which have been used
    ; during the computation of the equation.

    Mov cl minusSign
    Mov bx '04'

    Test_If B$UsedRegisters EPP_EDI
        EPP_Code 'MOV EDI D$ESI' cl bx '|'
        add bh 4
    Test_End

    Test_If B$UsedRegisters EPP_ECX
        EPP_Code 'MOV ECX D$ESI' cl bx '|'
        add bh 4
    Test_End

    Test_If B$UsedRegisters EPP_EAX
        On bh > '9', add bh 7
        EPP_Code 'MOV EAX D$ESI' cl bx '|'
        add bh 4
    Test_End

    Test_If B$UsedRegisters EPP_EBX
        On bh = 010, Mov bl '1', bh '0'
        EPP_Code 'MOV EBX D$ESI' cl bx '|'
        add bh 4
    Test_End

    Test_If B$UsedRegisters EPP_EDX
        On bh = 010, Mov bl '1', bh '0'
        On bh > '9', add bh 7
        EPP_Code 'MOV EDX D$ESI' cl bx '|'
    Test_End


ret
____________________________________________________________________________________________
Proc BackupFirstResults:
    Uses esi, edi

    Mov esi EPP_DestOperand
    Mov edi EPP_DestOperand_Backup
    While B$esi > Space | movsb | End_While

    Mov esi EPP_SourceEquation
    Mov edi EPP_SourceEquation_Backup
    While B$esi > EOI | movsb | End_While | Mov B$edi EOI

    Mov al B$UsedRegisters | Mov B$UsedRegisters_Backup al
EndP
____________________________________________________________________________________________
Proc RestoreFirstResults:
    Uses esi, edi

    Mov esi EPP_DestOperand_Backup
    Mov edi EPP_DestOperand
    While B$esi > Space | movsb | End_While

    Mov esi EPP_SourceEquation_Backup
    Mov edi EPP_SourceEquation
    While B$esi > EOI | movsb | End_While | Mov B$edi EOI

    Mov al B$UsedRegisters_Backup | Mov B$UsedRegisters al
EndP
____________________________________________________________________________________________
Proc CheckIfArray:
    ; Sets B$ParsingInsideArray to &TRUE, if the last parenthesis is after an array.
    Uses esi

    dec esi
    If_And esi <> EPP_SourceEquation, B$esi-1 > openSign
        dec esi | While B$esi > Space | dec esi | End_While
        On B$esi+2 = memMarker, Mov B$ParsingInsideArray &TRUE
    End_If

EndP
____________________________________________________________________________________________
Proc TestIfThisOperationIsComputable:
    ; return &FALSE if the operation at D@OperatorPos has NOT the priority to the next one.
    ; otherwise the operator is returned in eax and its sign in ecx.
    ; ex: eax = 8+3*eax+(7-D$a)/2
    ; - if D@OperatorPos points on '*', the function returns '*' (i.e. mulSign) and &FALSE in ecx (unsigned operation)
    ; - if D@OperatorPos points on the first '+', the function returns &FALSE
    ;
    Arguments @OperatorPos
    Uses ebx, esi

    Mov esi D@OperatorPos
    Call GetSignPriority D$esi | Mov bl al

    add esi 2 | While B$esi <> Space | inc esi | End_While | inc esi
    Call GetSignPriority D$esi

    Mov esi D@OperatorPos
    .If bl >= al
        If B$esi = SignedMulSign
            Mov ecx &TRUE
            Mov eax mulSign
        Else_If B$esi = SignedDivSign
            Mov ecx &TRUE
            Mov eax divSign
        Else
            Mov ecx &FALSE
            movzx eax B$esi
        End_If
    .Else
        Mov eax &FALSE
    .End_If

EndP
____________________________________________________________________________________________
Proc GetSignPriority:
    Argument @Sign

    If B@Sign = closeSign
        Mov al 0
    Else_If_Or B@Sign = addSign, B@Sign = subSign
        Mov al 1
    Else_If_Or B@Sign = mulSign, B@Sign = divSign, B@Sign = SignedMulSign, B@Sign = SignedDivSign
        Mov al 2
    Else_If_Or B@Sign = expSign
        Mov al 3
    Else
        Mov al 255
    End_If
EndP
____________________________________________________________________________________________
IsItaCall:
; if the equation is a CALL, the result (EAX) is stored in memory and then written in the Destination operand.
    Mov esi EPP_SourceEquation
    .If D$esi = 'CALL'
        EPP_Code 'PUSH ESI|MOV ESI ESP|'

        Mov edi D$EPP_WrittenCode
        While B$esi > EOI | movsb | End_While | Mov B$edi meEOI | inc edi
        Mov D$EPP_WrittenCode edi

        EPP_Code 'SUB ESP 04|{?? ESI-04}|MOV D$? EAX|'
        Mov B$EPP_SourceEquation Space | Mov B$EPP_SourceEquation+1 'D' | Mov B$EPP_SourceEquation+2 memMarker
        lea edi D$EPP_SourceEquation+3 | Call WriteNoMeanLabel | Mov B$edi Space | Mov B$edi+1 EOI

        Call ProcessArrayDestination
        Call StoreResultInFirstOperand

        Mov B$UsedRegisters EPP_ESI
        EPP_FindCodePos 'CALL' EPP_CODE_BEGIN EPP_CODE_END
        Call OptimizeEqualPreparserCode eax EPP_CODE_END

        Call TranslateExpressions
        Mov eax &TRUE
    .Else
        Mov eax &FALSE
    .End_If
ret
____________________________________________________________________________________________
Proc ImmediateCodeParser:
    ; Writes the given code at edi.
    ; special chars:
    ;  '||'    => EOI
    ;  '??'    => Writes a new meaningless label
    ;  '?'     => Writes the current meaningless label
    ;  '#n'    => Writes the operand n (1 or 2)
    ;  '#atof' => Writes the address of the AsciiToFloat conversion function (not used)
    ;  '#ftoa' => Writes the address of the FloatToAscii conversion function (not used)
    ;  '#text' => Writes the text pointed by D$EPP_TextPointer (D$EPP_TextPointer points on the opening TextSign)
    ;  '&n'    => Writes a new meaningless label and store it the n-th internal variable
    ;             (0 to 7, See EPP_InternalMeaninglessLabels)
    ;             &7 is used for FPU state adresss (See EqualFirstScan)
    ;  '!n'    => Writes the n-th meaningless label
    ;
    ;
    ;  - the character erased by the security-EOI at the end of the string is
    ;    returned in AL (in case it must be rewritten)
    Arguments @Content
    Uses ecx, esi, edi, ebx

    Mov esi D@Content
    Mov edi D$EPP_WrittenCode
    .While B$esi <> 0
        ...If B$esi = ' '
            Mov B$edi Space | inc edi, esi
        ...Else_If W$esi = '||'
            Mov B$edi EOI | inc edi | add esi 2
        ...Else_If B$esi = '+'
            Mov B$edi plusSign | inc esi,edi
        ...Else_If B$esi = '-'
            Mov B$edi minusSign | inc esi,edi
        ...Else_If B$esi = '|'
            Mov B$edi meEOI | inc edi, esi
        ...Else_If B$esi = ':'
            Mov B$edi colonSign | inc edi, esi
        ...Else_If B$esi = '$'
            Mov B$edi memMarker | inc edi, esi
        ...Else_If B$esi = '&'
            Call CreateNoMeanLabel
            push edi
                movzx eax B$esi+1 | sub eax '0' | Mov ebx 9 | mul ebx
                lea edi D$EPP_InternalMeaninglessLabels+eax
                Mov B$edi+8 0
                Call WriteNoMeanLabel
            pop edi
            Call WriteNoMeanLabel
            add esi 2
        ...Else_If B$esi = '!'
            push esi
                movzx eax B$esi+1 | sub eax '0' | Mov ebx 9 | mul ebx
                lea esi D$EPP_InternalMeaninglessLabels+eax
                While B$esi <> 0 | movsb | End_While
            pop esi
            add esi 2
        ...Else_If B$esi = '?'
            If B$esi+1 = '?' | Call CreateNoMeanLabel  | inc esi | End_If
            Call WriteNoMeanLabel
            inc esi
        ...Else_If B$esi = '#'
            ..If B$esi+1 <> '#'
                .If D$esi+1 <> 'text'
                    If B$esi+1 = '1'
                        Mov ecx D$EPP_Operand1 | add esi 2
                    Else_If B$esi+1 = '2'
                        Mov ecx D$EPP_Operand2 | add esi 2
                    Else_If D$esi+1 = 'atof'
                        Mov ecx D$EPP_atofAddress  | add esi 5
                    Else_If D$esi+1 = 'ftoa'
                        Mov ecx D$EPP_ftoaAddress  | add esi 5
                    End_If
                    While B$ecx > Space | Mov al B$ecx | Mov B$edi al | inc ecx, edi | End_While
                .Else
                    Mov ecx D$EPP_TextPointer | Mov B$edi TextSign | inc ecx, edi
                    While B$ecx > TextSign | Mov al B$ecx | Mov B$edi al | inc ecx, edi | End_While
                    Mov B$edi TextSign | inc ecx, edi
                    add esi 5
                .End_If
            ..Else
                Mov B$edi numSign | inc edi
                add esi 2
            ..End_If
        ...Else
            movsb
        ...End_If
    .End_While

    Mov al B$edi
    Mov B$edi EOI
    Mov D$EPP_WrittenCode edi

EndP
____________________________________________________________________________________________
Proc ReplaceOperand:
    ; Replaces the given operand by
    ;   - the current meaningless label if D@Replacement = 0
    ;   - the content of D@Replacement if D@Replacement <> 0  (a string of 1,2,3 or 4 characters)
    ; When D@Datatype = 'X', the new operand is prefixed by 'X$' (0 for no prefix)
    ; When D@operand = 0, both operands and their operator will be deleted.
    ; If the position of the second operand has changed and if both operands points into EPP_SourceEquation
    ; then D$EPP_Operand2 is updated.
    ;
    ; ex:  - ReplaceOperand 0 'B' 'ECX' ; => replaces both operands by 'B$ECX'
    ;      - ReplaceOperand 1 'W' 0     ; => replaces the first operand by 'W$AAAAAAAA'
    ;      - ReplaceOperand 2 0 'EDI'   ; => replaces the second operand by 'EDI'
    ;
    Arguments  @operand @DataType @Replacement
    Local @UpdateOperand2
    Uses edi, ecx, esi

    .If D@operand = 2
        Mov edi D$EPP_Operand2
        Mov D@UpdateOperand2 &FALSE
    .Else
        Mov edi D$EPP_Operand1
        Mov ecx D$EPP_Operand2
        If_Or edi < EPP_SourceEquation, edi > EPP_SourceEquation_End,
              ecx < EPP_SourceEquation, ecx > EPP_SourceEquation_End
            Mov D@UpdateOperand2 &FALSE
        Else
            Mov D@UpdateOperand2 &TRUE
        End_If
    .End_If

    Mov ecx 0
    While B$edi > Space | inc edi, ecx | End_While
    If D@operand = 0
        add edi 3 | add ecx 3
        While B$edi > Space | inc edi, ecx | End_While
    End_If

    Call ShiftLineLeft ecx
    sub edi ecx
    On D@UpdateOperand2 = &TRUE, sub D$EPP_Operand2 ecx

    ..If D@Replacement = 0
        If D@DataType <> 0
            On D@UpdateOperand2 = &TRUE, add D$EPP_Operand2 10
            Call ShiftLineRight 10
            move D$edi D@DataType | Mov B$edi+1 memMarker | add edi 2
            Call WriteNoMeanLabel
        Else
            On D@UpdateOperand2 = &TRUE, add D$EPP_Operand2 8
            Call ShiftLineRight 8
            Call WriteNoMeanLabel
        End_If
    ..Else
        Mov ecx 0 | lea esi D@Replacement
        While B$esi <> 0
            On ecx = 4, jmp L1>
            inc ecx, esi
        End_While
L1:     .If D@DataType <> 0
            add ecx 2
            Call ShiftLineRight ecx
            On D@UpdateOperand2 = &TRUE, add D$EPP_Operand2 ecx
            Mov al B@DataType | Mov B$edi al | Mov B$edi+1 memMarker | add edi 2
        .Else
            Call ShiftLineRight ecx
            On D@UpdateOperand2 = &TRUE, add D$EPP_Operand2 ecx
        .End_If

        lea esi D@Replacement
        Mov ecx 0
        While B$esi <> 0
            On ecx = 4, jmp L1>
            movsb | inc ecx
        End_While
L1:
   ..End_If
EndP
____________________________________________________________________________________________
Proc WriteCodeForTheseParentheses:
    ; ^ then is first searched between the parentheses then * and / and finally + and - (to keep priority)
    ; if nothing is found, we check if there is no space between the parentheses (i.e there is either
    ; an equate or an immediate number (integer or not), and when no space is found the
    ; parentheses are removed, otherwise : error)
    Argument @OpeningParenthesis
    Uses ebx

    Mov eax 0
    Mov ecx D@OpeningParenthesis
    .While B$ecx <> closeSign
        If B$ecx = expSign
            Call GetOperandTypeAndPos ecx
            Call WriteOperationCode expsign &FALSE
            On eax = &TRUE, Call RemoveParentheses
            Mov ecx D@OpeningParenthesis
        End_If
        On eax = &TRUE, ExitP
        inc ecx
    .End_While

    Mov eax 0
    Mov ecx D@OpeningParenthesis
    .While B$ecx <> closeSign
        If B$ecx = mulSign
            Mov eax &FALSE
            jmp L1>
        Else_If B$ecx = SignedMulSign
            Mov eax &TRUE
L1:         Call GetOperandTypeAndPos ecx
            Call WriteOperationCode mulSign eax
            On eax = &TRUE, Call RemoveParentheses
            Mov ecx D@OpeningParenthesis
        Else_If B$ecx = divSign
            Mov eax &FALSE
            jmp L1>
        Else_If B$ecx = SignedDivSign
            Mov eax &TRUE
L1:         Call GetOperandTypeAndPos ecx
            Call WriteOperationCode divSign eax
            On eax = &TRUE, Call RemoveParentheses
            Mov ecx D@OpeningParenthesis
        End_If
        On eax = &TRUE, ExitP
        inc ecx
    .End_While


    Mov eax 0
    Mov ecx D@OpeningParenthesis
    .While B$ecx <> closeSign
        If B$ecx = addSign
            Call GetOperandTypeAndPos ecx
            Call WriteOperationCode addsign &FALSE
            On eax = &TRUE, Call RemoveParentheses
            Mov ecx D@OpeningParenthesis
        Else_If B$ecx = subSign
            Call GetOperandTypeAndPos ecx
            Call WriteOperationCode subSign &FALSE
            On eax = &TRUE, Call RemoveParentheses
            Mov ecx D@OpeningParenthesis
        End_If
        On eax = &TRUE, ExitP
        inc ecx
    .End_While


    Mov ecx D@OpeningParenthesis | add ecx 2
    Mov D$EPP_Operand1 ecx
    Mov eax 0

    While B$ecx <> closeSign
        If B$ecx = Space
            On B$ecx+1 <> closeSign, jmp L9>        ; space found => error
        End_If

        On B$ecx = pointSign, Mov eax &TRUE       ; Is this a floating point number

        inc ecx
    End_While

    .If eax = &TRUE
        dec D@OpeningParenthesis
        If D@OpeningParenthesis = EPP_SourceEquation  ; saves the FP number in memory only if there is no more operation.
            EPP_Code '{??:R$ #1}|'
            Call ReplaceOperand 1 'R' 0
        End_If
    .End_If

    Call RemoveParentheses
    If eax = &TRUE
        Mov eax EPP_NO_OPERATION
        ExitP
    End_If

L9:
    Mov eax &FALSE

EndP
____________________________________________________________________________________________
ShiftLineRight:
    ; shift right the line (until EOI) pointed by edi of X bytes.
    push esi edi ecx
        Mov ecx 1
        While B$edi <> EOI | inc edi ecx | End_While
        Mov esi edi
        push ecx edi
            add edi D$esp+24
            std
                rep movsb
            cld
        pop edi ecx
        If ecx <= D$esp+16   ; erase the last EOI(s) if it was not previously overwritten by the shift.
            Mov esi D$esp+16 | sub esi ecx
            Mov ecx 0
            While ecx <= esi | Mov B$edi+ecx Space | inc ecx | End_While
        End_If
    pop ecx edi esi
ret 4
____________________________________________________________________________________________
Proc ShiftLineLeft:
    ; shift left the line (until EOI) pointed by edi of D@shift bytes.
    Argument @shift
    Local @EndOfLine
    Uses esi edi ecx

    Mov ecx 1
    Mov esi edi
    While B$edi <> EOI | inc edi ecx | End_While | Mov D@EndOfLine edi
    Mov edi esi | sub edi D@shift
    rep movsb

    Mov esi D@EndOfLine | sub esi D@shift | inc esi
    While esi <= D@EndOfLine | Mov B$esi 0 | inc esi | End_While
EndP
____________________________________________________________________________________________
Proc GetOperandTypeAndPos:
    ;The type of each operand is identified and stored in as flags. (See EPP_Types)
    ;BX contains the type of the first operand and DX contains the type of the second one.
    ;The position of the first character of each operand is stored in D$EPP_Operand1 and D$EPP_Operand1
    Argument @OperatorPos
    Uses eax

    Mov ebx 0
    Mov edx 0

    Mov eax D@OperatorPos | sub eax 2
    While B$eax > Space | dec eax | End_While | inc eax | Mov D$EPP_Operand1 eax
    Call GetStringType eax
    Mov bx ax

    Mov eax D@OperatorPos | add eax 2 | Mov D$EPP_Operand2 eax
    Call GetStringType eax
    Mov dx ax

EndP
____________________________________________________________________________________________
Proc WriteOperationCode:
  ; D@OperationType can be divSign,mulSign,addSign or subSign
  ; BX contains the type of the first operand and DX contains the type of the second one. (See GetOperandTypeAndPos)
  ; return &TRUE if code has been written.
  Arguments @OperationType @Signed
  Uses D$DestinationType

    If B$ParsingInsideArray = &TRUE
        Mov D$DestinationType (MemoryOperand or DWordValue)
    End_If

    Test_If_And bx NumericOperand, dx NumericOperand
        Call ComputeImmediateExpression D@OperationType
        Mov eax &FALSE

    Test_Else_If_Not_And bx FloatingPointOperand, dx FloatingPointOperand, D$DestinationType FloatingPointOperand
        Call ReplaceIndirectAddressing
        Call WriteIntegerOperationCode D@OperationType D@Signed
        Mov eax &TRUE

    Test_Else ; at least one operand non-integer
        Call ReplaceIndirectAddressing
        Call WriteFloatingPointOperationCode D@OperationType
        Call StoreFPUResult 0
        Mov eax &TRUE

    Test_End

EndP
____________________________________________________________________________________________
Proc StoreFPUResult:
; The result of the FP operation is either left on the FPU Stack or stored in memory.
; In the first case, the operand is replaced by 'st0' (in lowercase so as to be locatable
; afterwards (see incFPUStackNumber)) otherwise it is replaced by a meaningless label.
    Argument @operand

    If D$FPUStackNumber < 6
        Call ReplaceOperand D@operand 0 'st0'
    Else
        EPP_Code 'SUB ESP 0A|{?? ESI-000}|FSTP T$?|'

        Call ReplaceOperand D@operand 'T' 0
        Call decFPUStackNumber
    End_If

EndP
____________________________________________________________________________________________
; As the temporary FP result are stored in the FPU Stack (ST0-ST6), each time a new result is stored the
; last ones are pushed on the stack. Theses two functions update the temporary FP result on the stack
; by incrementing or decrementing the register number.
; The registers to be updated are identified by the 'st' prefix (instead of 'ST').
Proc incFPUStackNumber:
    Uses ecx

    Mov ecx EPP_SOURCE_BEGIN
    jmp L1>
    While eax <> CODE_NOT_FOUND
        inc B$eax+2 | lea ecx D$eax+1
L1:     EPP_FindCodePos 'st' ecx EPP_SOURCE_END
    End_While
    inc D$FPUStackNumber

EndP
                            __________________________________________________
Proc decFPUStackNumber:
    Uses ecx

    Mov ecx EPP_SOURCE_BEGIN
    jmp L1>
    While eax <> CODE_NOT_FOUND
        dec B$eax+2 | lea ecx D$eax+1
L1:     EPP_FindCodePos 'st' ecx EPP_SOURCE_END
    End_While
    dec D$FPUStackNumber

EndP
____________________________________________________________________________________________
Proc ReplaceIndirectAddressing:
    ; Replaces in EPP_SourceEquation, the operand of this type: 'F$D$AAAAAAA' (which are written when an array is met)
    ; by 'F$ECX' , then the code to move 'MOV ECX D$AAAAAAAA' is written

    Call ExtractIndirectAddress 1 EPP_ECX
    If eax = &TRUE
        Call ExtractIndirectAddress 2 EPP_EDI
    Else
        Call ExtractIndirectAddress 2 EPP_ECX
    End_If

EndP

____________________________________________________________________________________________
Proc ExtractIndirectAddress:
    ; Extracts the address (which is stored in a memory DWord) of the given operand (1 or 2),
    ; move it into the given register and then replace the operand.
    ; D@register can be (EPP_EAX,EPP_EBX,EPP_ECX,EPP_EDX,EPP_ESI,EPP_EDI)
    ;
    ; return &FALSE if no extraction was done on the given operand, &TRUE otherwise.
    Argument @Operand @register
    Uses esi, edi, edx

    If D@Operand = 1
        Mov esi D$EPP_Operand1
    Else
        Mov esi D$EPP_Operand2
    End_If

    .If_And B$esi+1 = memMarker, B$esi+2 = 'D', B$esi+3 = memMarker
        EPP_Code 'MOV ' | Call WriteRegister D@Register | Mov edx eax
        push D$EPP_Operand1
            Mov D$EPP_Operand1 esi | add D$EPP_Operand1 2
            EPP_Code ' #1|'
        pop D$EPP_Operand1

        Call ReplaceOperand D@Operand D$esi edx
        Mov eax &TRUE
    .Else
        Mov eax &FALSE
    .End_If

EndP
____________________________________________________________________________________________
Proc WriteRegister:
    Argument @register

    If D@register = EPP_EAX
        EPP_CODE 'EAX'
        Mov eax 'EAX'
    Else_If D@register = EPP_EBX
        EPP_CODE 'EBX'
        Mov eax 'EBX'
    Else_If D@register = EPP_ECX
        EPP_CODE 'ECX'
        Mov eax 'ECX'
    Else_If D@register = EPP_EDX
        EPP_CODE 'EDX'
        Mov eax 'EDX'
    Else_If D@register = EPP_ESI
        EPP_CODE 'ESI'
        Mov eax 'ESI'
    Else_If D@register = EPP_EDI
        EPP_CODE 'EDI'
        Mov eax 'EDI'
    End_If
EndP
____________________________________________________________________________________________
Proc WriteAddressingCode:
    ; Writes the code which is before an array. Basically this translate F$Array(6) to F$Array+6*4
    Argument @DataType


    .If B@DataType = 'T'
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EAX #1|MOV EBX 10|MUL EBX|SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'
    .Else_If B@DataType = 'R'
        EPP_Code 'MOV EAX #1|SHL EAX 3|SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'
    .Else_If B@DataType = 'F'
        EPP_Code 'MOV EAX #1|SHL EAX 2|SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'
    .Else_If B@DataType = 'B'
        EPP_Code 'MOV EAX #1|SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'
    .Else_If B@DataType = 'D'
        EPP_Code 'MOV EAX #1|SHL EAX 2|SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'
    .Else_If B@DataType = 'W'
        EPP_Code 'MOV EAX #1|SHL EAX 1|SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'
    .Else_If B@DataType = 'Q'
        EPP_Code 'MOV EAX #1|SHL EAX 3|SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'
    .End_If

    add D$EPP_Operand2 2 | EPP_Code 'ADD D$? #2|' | sub D$EPP_Operand2 2
    Call ReplaceOperand 1 'D' 0
EndP
____________________________________________________________________________________________
Proc RemoveParentheses:
    ; Return &TRUE if the two parentheses surrounding EPP_Operand1 have been removed from EPP_SourceEquation,
    ; &FALSE otherwise.
    Uses edi, ecx

    Mov eax &FALSE
    Mov edi D$EPP_Operand1

    .If B$edi-2 = openSign
        Mov ecx 0
        While B$edi+ecx > Space | inc ecx | End_While | dec ecx
        If B$edi+ecx+2 = closeSign
            Call ShiftLineLeft 2
            lea edi D$edi+ecx+(3-2)
            Call ShiftLineLeft 2
            Mov eax &TRUE    ; the parentheses have been removed
        End_If
    .End_If

EndP
____________________________________________________________________________________________
TranslateExpressions:
    ; This part converts characters used by the preparser to RosAsm characters.
    ; It is called once all modifications of the code have been performed.
    Mov ecx D$EPP_CodeBegining
    .While B$ecx <> EOI
        If B$ecx = plusSign
            Mov B$ecx addSign
        Else_If B$ecx = minusSign
            Mov B$ecx subSign
        Else_If B$ecx = pointSign
            Mov B$ecx '.'
        Else_If B$ecx =  addressSign
            Mov B$ecx mulSign
        Else_If W$ecx = 'st'
            Mov W$ecx 'ST'
        Else_If B$ecx = TextSign
            inc ecx | While B$ecx <> TextSign | inc ecx | End_While
        End_If
        inc ecx
    .End_While

ret
____________________________________________________________________________________________
Proc IntegerDivisionCode_DWORD_Destination:
    Arguments @Symb_S @Symb_I

    ; >> DWORD Destination
    or B$UsedRegisters EPP_EDX
    .Test_If_And bx ConstantOperand, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And bx ConstantOperand, dx WordValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And bx ConstantOperand, dx DWordValue
        EPP_Code 'MOV EDX 0|MOV EAX #1|' D@Symb_I 'DIV #2|'
    .Test_Else_If_And bx Word, dx DWordValue
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X EAX #1|' D@Symb_I 'DIV #2|'
    .Test_Else_If_And bx Word, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And bx DWord, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'DIV EBX|'

    .Test_Else_If_And bx ByteValue, dx ConstantOperand
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X EAX #1|MOV EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And bx WordValue, dx ConstantOperand
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X EAX #1|MOV EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And  bx DWordValue, dx ConstantOperand
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV EAX #1|MOV EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And  bx DWordValue, dx Word
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And  bx ByteValue, dx Word
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And  bx ByteValue, dx DWord
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X EAX #1|' D@Symb_I 'DIV #2|'

    .Test_Else_If_And bx ByteValue, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And bx WordValue, dx WordValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And bx DWordValue, dx DWordValue
        EPP_Code 'MOV EDX 0|MOV EAX #1|' D@Symb_I 'DIV #2|'
    .Test_End
    Mov eax DWordValue
EndP
____________________________________________________________________________________________
Proc IntegerMultiplicationCode_DWORD_Destination:
    Arguments @Symb_S @Symb_I

    ; >> DWORD Destination
    or B$UsedRegisters EPP_EDX
    .Test_If_And bx ConstantOperand, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And bx ConstantOperand, dx WordValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And bx ConstantOperand, dx DWordValue
        EPP_Code 'MOV EAX #1|' D@Symb_I 'MUL #2|'
    .Test_Else_If_And bx Word, dx DWordValue
        EPP_Code 'MOV' D@Symb_S 'X EAX #1|' D@Symb_I 'MUL #2|'
    .Test_Else_If_And bx Word, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And bx DWord, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'MUL EBX|'

    .Test_Else_If_And bx ByteValue, dx ConstantOperand
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X EAX #1|MOV EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And bx WordValue, dx ConstantOperand
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X EAX #1|MOV EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And  bx DWordValue, dx ConstantOperand
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EAX #1|MOV EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And  bx DWordValue, dx Word
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And  bx ByteValue, dx Word
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And  bx ByteValue, dx DWord
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X EAX #1|' D@Symb_I 'MUL #2|'

    .Test_Else_If_And bx ByteValue, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And bx WordValue, dx WordValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And bx DWordValue, dx DWordValue
        EPP_Code 'MOV EAX #1|' D@Symb_I 'MUL #2|'
    .Test_End
    Mov eax DWordValue
EndP
____________________________________________________________________________________________
Proc IntegerDivisionCode_WORD_Destination:
    Arguments @Symb_S @Symb_I

    ; >> WORD Destination
    .Test_If_And bx ConstantOperand, dx ByteValue
        EPP_Code 'MOV AX #1|' D@Symb_I 'DIV #2|'
        Mov eax ByteValue
    .Test_Else_If_And bx ConstantOperand, dx WordValue
        or B$UsedRegisters EPP_EDX
        EPP_Code 'MOV EDX 0|MOV AX #1|' D@Symb_I 'DIV #2|'
        Mov eax WordValue
    .Test_Else_If_And bx ConstantOperand, dx DWordValue
        ; error
    .Test_Else_If_And bx Word, dx DWordValue
        ; error
    .Test_Else_If_And bx Word, dx ByteValue
        EPP_Code 'MOV AX #1|' D@Symb_I 'DIV #2|'
        Mov eax ByteValue
    .Test_Else_If_And bx DWord, dx ByteValue
        ; error

    .Test_Else_If_And bx ByteValue, dx ConstantOperand
        or B$UsedRegisters (EPP_EBX or EPP_EDX)
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X AX #1|MOV BX #2|' D@Symb_I 'DIV BX|'
        Mov eax WordValue
    .Test_Else_If_And bx WordValue, dx ConstantOperand
        or B$UsedRegisters (EPP_EBX or EPP_EDX)
        EPP_Code 'MOV EDX 0|MOV AX #1|MOV BX #2|' D@Symb_I 'DIV BX|'
        Mov eax WordValue
    .Test_Else_If_And  bx DWordValue, dx ConstantOperand
        ; error
    .Test_Else_If_And  bx DWordValue, dx Word
        or B$UsedRegisters (EPP_EBX or EPP_EDX)
        EPP_Code 'LEA EAX #1|MOV DX W$EAX+2|MOV AX W$EAX|' D@Symb_I 'DIV #2|'
        Mov eax WordValue
    .Test_Else_If_And  bx ByteValue, dx Word
        or B$UsedRegisters EPP_EDX
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X AX #1|' D@Symb_I 'DIV #2|'
        Mov eax WordValue
    .Test_Else_If_And  bx ByteValue, dx DWord
        ; error

    .Test_Else_If_And bx ByteValue, dx ByteValue
        EPP_Code 'MOV' D@Symb_S 'X AX #1|' D@Symb_I 'DIV #2|'
        Mov eax ByteValue
    .Test_Else_If_And bx WordValue, dx WordValue
        or B$UsedRegisters EPP_EDX
        EPP_Code 'MOV EDX 0|MOV AX #1|' D@Symb_I 'DIV #2|'
        Mov eax WordValue
    .Test_Else_If_And bx DWordValue, dx DWordValue
        ; error
    .Test_End

EndP
____________________________________________________________________________________________
Proc IntegerMultiplicationCode_WORD_Destination:
    Arguments @Symb_S @Symb_I

    ; >> WORD Destination
    .Test_If_And bx ConstantOperand, dx ByteValue
        EPP_Code 'MOV AL #1|' D@Symb_I 'MUL #2|'
        Mov eax WordValue
    .Test_Else_If_And bx ConstantOperand, dx WordValue
        or B$UsedRegisters EPP_EDX
        EPP_Code 'MOV AX #1|' D@Symb_I 'MUL #2|'
        Mov eax WordValue
    .Test_Else_If_And bx ConstantOperand, dx DWordValue
        ; error
    .Test_Else_If_And bx Word, dx DWordValue
        ; error
    .Test_Else_If_And bx Word, dx ByteValue
        or B$UsedRegisters EPP_EDX
        EPP_Code 'MOV' D@Symb_S 'X AX #2|' D@Symb_I 'MUL #1|'
        Mov eax WordValue
    .Test_Else_If_And bx DWord, dx ByteValue
        ; error

    .Test_Else_If_And bx ByteValue, dx ConstantOperand
        EPP_Code 'MOV AL #2|' D@Symb_I 'MUL #1|'
        Mov eax WordValue
    .Test_Else_If_And bx WordValue, dx ConstantOperand
        or B$UsedRegisters EPP_EDX
        EPP_Code 'MOV AX #2|' D@Symb_I 'MUL #1|'
        Mov eax WordValue
    .Test_Else_If_And  bx DWordValue, dx ConstantOperand
        ; error
    .Test_Else_If_And  bx DWordValue, dx Word
        ; error
    .Test_Else_If_And  bx ByteValue, dx Word
        or B$UsedRegisters EPP_EDX
        EPP_Code 'MOV' D@Symb_S 'X AX #1|' D@Symb_I 'MUL #2|'
        Mov eax WordValue
    .Test_Else_If_And  bx ByteValue, dx DWord
        ; error

    .Test_Else_If_And bx ByteValue, dx ByteValue
        EPP_Code 'MOV AL #1|' D@Symb_I 'MUL #2|'
        Mov eax WordValue
    .Test_Else_If_And bx WordValue, dx WordValue
        or B$UsedRegisters EPP_EDX
        EPP_Code 'MOV AX #1|' D@Symb_I 'MUL #2|'
        Mov eax WordValue
    .Test_Else_If_And bx DWordValue, dx DWordValue
        ; error
    .Test_End

EndP
____________________________________________________________________________________________
Proc IntegerDivisionCode_BYTE_Destination:
    Arguments @Symb_S @Symb_I

    ; >> BYTE Destination
    .Test_If_And bx ConstantOperand, dx ByteValue
        EPP_Code 'MOV AX #1|' D@Symb_I 'DIV #2|'
        Mov eax ByteValue
    .Test_Else_If_And bx ConstantOperand, dx WordValue
        ; error
    .Test_Else_If_And bx ConstantOperand, dx DWordValue
                                                                                                                                                                                                                                                                                                                                                                                                                                        ; error
    .Test_Else_If_And bx Word, dx DWordValue
        ; error
    .Test_Else_If_And bx Word, dx ByteValue
        EPP_Code 'MOV AX #1|' D@Symb_I 'DIV #2|'
        Mov eax ByteValue
    .Test_Else_If_And bx DWord, dx ByteValue
        ; error

    .Test_Else_If_And bx ByteValue, dx ConstantOperand
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X AX #1|MOV BL #2|' D@Symb_I 'DIV BL|'
        Mov eax ByteValue
    .Test_Else_If_And bx WordValue, dx ConstantOperand
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV AX #1|MOV BL #2|' D@Symb_I 'DIV BL|'
        Mov eax ByteValue
    .Test_Else_If_And  bx DWordValue, dx ConstantOperand
        ; error
    .Test_Else_If_And  bx DWordValue, dx Word
        ; error
    .Test_Else_If_And  bx ByteValue, dx Word
        ; error
    .Test_Else_If_And  bx ByteValue, dx DWord
        ; error

    .Test_Else_If_And bx ByteValue, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X AX #1||' D@Symb_I 'DIV #2|'
        Mov eax ByteValue
    .Test_Else_If_And bx WordValue, dx WordValue
        ; error '
    .Test_Else_If_And bx DWordValue, dx DWordValue
        ; error
    .Test_End

EndP
____________________________________________________________________________________________
Proc WriteIntegerOperationCode:
    ; BX contains the type of the first operand and DX contains the type of the second one.
    ; D@Symb_I and D@Symb_S contain the letters which changes in mnemonics from a signed to
    ; an unsigned operation. This way, no further test of the sign is needed.
    Arguments @OperationType @Signed
    Local @Symb_I @Symb_S @OperationString
    Uses ecx, esi

    If D@Signed = &TRUE
        Mov D@Symb_I 'I'
        Mov D@Symb_S 'S'
    Else
        Mov D@Symb_I ''
        Mov D@Symb_S 'Z'
    End_If

    Mov eax 0_FF00_0000 ; error check

    .If D@OperationType = divSign
        or B$UsedRegisters EPP_EAX
        Test_If D$DestinationType DWordValue
            Call IntegerDivisionCode_DWORD_Destination D@Symb_S D@Symb_I

        Test_Else_If D$DestinationType WordValue
            Call IntegerDivisionCode_WORD_Destination D@Symb_S D@Symb_I

        Test_Else D$DestinationType ByteValue
            Call IntegerDivisionCode_BYTE_Destination D@Symb_S D@Symb_I

        Test_End

    ; >> MULTIPLICATION
    .Else_If D@OperationType = mulSign
        or B$UsedRegisters EPP_EAX
        Test_If D$DestinationType DWordValue
            Call IntegerMultiplicationCode_DWORD_Destination D@Symb_S D@Symb_I

        Test_Else_If D$DestinationType WordValue
            Call IntegerMultiplicationCode_WORD_Destination D@Symb_S D@Symb_I

        Test_Else D$DestinationType ByteValue
            ; error

        Test_End

    ; >> POWER
    .Else_If D@OperationType = expSign
        or B$UsedRegisters (EPP_EAX or EPP_EBX or EPP_ECX)
        Mov cx 0 | Test_If_Not_And bx DWordValue, bx ConstantOperand | Mov cl B@Symb_S | Mov ch 'X' | Test_End
        Test_If dx DWordValue
            Mov esi 'ECX'
        Test_Else_If dx WordValue
            Mov esi 'CX'
        Test_Else_If dx WordValue
            Mov esi 'CL'
        Test_Else ; constant
            Mov esi 'ECX'
        Test_End

        EPP_Code 'MOV ECX 0|MOV EAX 1|MOV' cx ' EBX #1|&1:|CMP ' esi ' #2|JE &2|'
        EPP_Code  D@Symb_I 'MUL EBX|INC ECX|JMP !1|!2:|'

        Mov eax DWordValue

    ; >> ADDITION AND SUBSTRACTION
    .Else_If_Or D@OperationType = addSign, D@OperationType = subSign
        or B$UsedRegisters EPP_EAX
        If D@OperationType = addSign
            Mov D@OperationString 'ADD'
        Else
            Mov D@OperationString 'SUB'
        End_If
        Mov cx 0
        .Test_If D$DestinationType DWordValue
            Test_If_Not_And bx DWordValue, bx ConstantOperand | Mov cl B@Symb_S | Mov ch 'X' | Test_End
            EPP_Code 'MOV' cx ' EAX #1|' D@OperationString ' EAX #2|'
            Mov eax DWordValue

        .Test_Else_If D$DestinationType WordValue
            Test_If_Not_And  bx WordValue, bx ConstantOperand | Mov cl B@Symb_S | Mov ch 'X' | Test_End
            EPP_Code 'MOV' cx ' AX #1|' D@OperationString ' AX #2|'
            Mov eax WordValue

        .Test_Else_If D$DestinationType ByteValue
            EPP_Code 'MOV AL #1|' D@OperationString ' AL #2|'
            Mov eax ByteValue

        .Test_End

    .EndIf

    ; stores result in memory
    .If eax = DWordValue
        EPP_Code 'SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'
        Call ReplaceOperand 0 'D' 0
    .Else_If eax = WordValue
        EPP_Code 'SUB ESP 02|{?? ESI-000}|MOV W$? AX|'
        Call ReplaceOperand 0 'W' 0
    .Else_If eax = ByteValue
        EPP_Code 'SUB ESP 01|{?? ESI-000}|MOV B$? AL|'
        Call ReplaceOperand 0 'B' 0
    .Else
        error EqualPreparser_SourceInvalid
    .End_If

EndP
____________________________________________________________________________________________
Proc WriteFloatingPointOperationCode:
    ; Writing of data loading according to its type.
    ; BX contains the type of the first operand and DX contains the type of the second one.
    Arguments @OperationType

    ; copy integers registers into memory and change the operand type to MemoryOperand
    ..Test_If bx RegisterOperand
        .Test_If_Not bx FloatingPointOperand
            and bx ((not RegisterOperand) and 0_FFFF)
            or bx MemoryOperand
            Test_If_Not bx DWordValue
                Call ExtendOperandToDWord 1 MemoryOperand
            Test_Else
                EPP_Code 'SUB ESP 04|{?? ESI-000}|MOV D$? #1|'
                Call ReplaceOperand 1 'D' 0
            Test_End
        .Test_End
    ..Test_End

    ..Test_If dx RegisterOperand
        .Test_If_Not dx FloatingPointOperand
            and dx ((not RegisterOperand) and 0_FFFF)
            or dx MemoryOperand
            Test_If_Not dx DWordValue
                Call ExtendOperandToDWord 1 MemoryOperand
            Test_Else
                EPP_Code 'SUB ESP 04|{?? ESI-000}|MOV D$? #2|'
                Call ReplaceOperand 2 'D' 0
            Test_End
        .Test_End
    ..Test_End


    ...Test_If_Not_And bx FloatingPointOperand, dx FloatingPointOperand
        ; only when the result is FP
        ..Test_If_Not_And bx ConstantOperand, dx ConstantOperand
            EPP_Code 'FILD #1|' | Call incFPUStackNumber
            Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_IntegerOperand
        ..Test_Else_If_And bx ConstantOperand, dx ConstantOperand
            EPP_Code '{??:R$ #1}|FLD R$?|{??:R$ #2}|FLD R$?|' | Call incFPUStackNumber
            Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST1_ST0
        ..Test_Else_If bx ConstantOperand
            EPP_Code '{??:R$ #1}|FLD R$?|' | Call incFPUStackNumber
            Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_IntegerOperand
        ..Test_Else
            EPP_Code '{??:R$ #2}|FLD R$?|' | Call incFPUStackNumber
            Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_IntegerOperand_ST0
        ..Test_End

    ...Test_Else_If bx ConstantOperand
        ..Test_If dx FloatingPointOperand
            ; #1 constant / #2 Floating Point
            ; ex: ( 51 - R$float1 )
            EPP_Code '{??:R$ #1}|'

            .Test_If dx TByteValue
                Test_If_Not dx RegisterOperand | EPP_Code 'FLD #2|' | Test_End
                Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ConstantOperand_ST0
            .Test_Else
                EPP_Code 'FLD R$?|' | Call incFPUStackNumber
                Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_FloatingPointOperand
            .Test_End
        ..Test_Else
            ; #1 constant / #2 integer
            ; ex: ( 7.4 + D$val1 )
            EPP_Code '{??:R$ #1}|FLD R$?|' | Call incFPUStackNumber
            Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_IntegerOperand
        ..Test_End

    ...Test_Else_If dx ConstantOperand
        ..Test_If bx FloatingPointOperand
            ; #1 Floating Point / #2 constant
            ; ex: ( T$float2 / 8 )
            EPP_Code '{??:R$ #2}|'

            .Test_If bx TByteValue
                Test_If_Not bx RegisterOperand | EPP_Code 'FLD #1|' | Test_End
                Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_ConstantOperand
            .Test_Else
                EPP_Code 'FLD R$?|' | Call incFPUStackNumber
                Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_FloatingPointOperand_ST0
            .Test_End
        ..Test_Else
            ; #1 integer / #2 constant
            ; ex: ( D$val1 + 7.4 )
            EPP_Code '{??:R$ #2}|FLD R$?|' | Call incFPUStackNumber
            Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_IntegerOperand_ST0
        ..Test_End

    ...Test_Else ; Both operand non-constant :
        ..Test_If_Not bx FloatingPointOperand
            ; #1 integer / #2 Floating Point
            ; ex: ( D$val2 * F$float2 )
            Test_If_Not dx RegisterOperand | EPP_Code 'FLD #2|' | Call incFPUStackNumber | Test_End
            Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_IntegerOperand_ST0

        ..Test_Else_If_Not dx FloatingPointOperand
            ; #1 Floating Point / #2 integer
            ; ex: ( R$float1 - W$val3 )
            Test_If_Not bx RegisterOperand | EPP_Code 'FLD #1|' | Call incFPUStackNumber | Test_End
            Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_IntegerOperand

        ..Test_Else
            ; #1 Floating Point / #2 Floating Point
            ; ex: ( R$float2 + F$float5 )

            .Test_If_And bx TByteValue, dx TByteValue
                Test_If_And bx RegisterOperand, dx RegisterOperand
                    Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST1_ST0
                Test_Else_If dx RegisterOperand
                    EPP_Code 'FLD #1|' | Call incFPUStackNumber
                    Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_ST1
                Test_Else_If bx RegisterOperand
                    EPP_Code 'FLD #2|' | Call incFPUStackNumber
                    Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST1_ST0
                Test_Else
                    EPP_Code 'FLD #1|FLD #2|' | Call incFPUStackNumber
                    Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST1_ST0
                Test_End

            .Test_Else_If bx TByteValue
                Test_If_Not bx RegisterOperand | EPP_Code 'FLD #1|' | Call incFPUStackNumber | Test_End
                Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_FloatingPointOperand

            .Test_Else_If dx TByteValue
                Test_If_Not dx RegisterOperand | EPP_Code 'FLD #2|' | Call incFPUStackNumber | Test_End
                Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_FloatingPointOperand_ST0

            .Test_Else
                EPP_Code 'FLD #1|' | Call incFPUStackNumber
                Call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_FloatingPointOperand

            .Test_End

        ..Test_End
    ...Test_End

EndP
____________________________________________________________________________________________
Proc StoreResultInFirstOperand:
  ; This function store the result of the operations in the destination operand.
    Uses edx, esi, edi

    Mov edi EPP_SourceEquation
    Mov ebx EPP_DestOperand
    Mov D$EPP_Operand1 EPP_SourceEquation | inc D$EPP_Operand1
    Mov D$EPP_Operand2 EPP_DestOperand

    If D$DestinationArrayRegister = EPP_ECX
        Mov edx EPP_EDI
    Else_If D$DestinationArrayRegister = EPP_EDI
        Mov edx EPP_ECX
    Else
        Mov edx EPP_EDI
    End_If
    Call ExtractIndirectAddress 1 edx
    If eax = &TRUE
        or B$UsedRegisters dl
    End_If

    If_And W$ebx = 'SI', B$ebx+2 < Separators
        Mov D$ebx 'W$ES', B$ebx+1 memMarker, B$ebx+4 'I', B$ebx+5 plusSign, B$ebx+6 '4', B$ebx+7 Space
    Else_If_And W$ebx = 'ES', B$ebx+2 = 'I', B$ebx+3 < Separators
        Mov D$ebx 'D$ES', B$ebx+1 memMarker, B$ebx+4 'I', B$ebx+5 plusSign, B$ebx+6 '4', B$ebx+7 Space
    End_If

    Mov D$EPP_LastStorageStatement_InMemory StrNull
    Mov D$EPP_LastStorageStatement          StrNull

    .If B$ebx+1 = memMarker
        Call StoreInMemory
    .Else_If B$ebx+2 = Space
        ; 2 characters destination
        If B$ebx+1 = 'H'
            jmp L1>
        Else_If B$ebx+1 = 'L'
L1:         Call StoreIn8BitsRegister
        Else
            Call StoreIn16BitsRegister
        End_If
        Call GetRegister ebx | not al | and B$UsedRegisters al
    .Else_If B$ebx+3 = Space
        ; 3 characters destination
        If B$ebx = 'E'
            Call StoreIn32BitsRegister
            Call GetRegister ebx | not al | and B$UsedRegisters al
        Else_If W$ebx = 'ST'
            Call StoreInFPURegister
        End_If
    .Else
        error EqualPreparser_DestInvalid
    .End_If

    EPP_Code 'POP EDX|POP EBX|POP EAX|POP ECX|POP EDI|'
    Call ImmediateCodeParser D$EPP_LastStorageStatement_InMemory
    EPP_Code  'MOV ESP ESI|POP ESI|'
    Call ImmediateCodeParser D$EPP_LastStorageStatement
EndP
____________________________________________________________________________________________
Proc GetRegister:
    Argument @RegisterPos
    Uses esi

    Mov esi D@RegisterPos
    On B$esi = 'E', inc esi

    If B$esi = 'A'
        Mov eax EPP_EAX
    Else_If B$esi = 'B'
        Mov eax EPP_EBX
    Else_If B$esi = 'C'
        Mov eax EPP_ECX
    Else_If W$esi = 'DI'
        Mov eax EPP_EDI
    Else_If W$esi = 'SI'
        Mov eax EPP_ESI
    Else_If B$esi = 'D'
        Mov eax EPP_EDX
    End_If

EndP
____________________________________________________________________________________________
Proc ExtendOperandToDWord:
    Argument @Operand @type
    or B$UsedRegisters EPP_EAX

    .If D@Operand = 1
        If D@type = MemoryOperand
            EPP_Code 'SUB ESP 04|{?? ESI-000}|MOVZX EAX #1|MOV D$? EAX|'
            Call ReplaceOperand 1 'D' 0
        Else
            EPP_Code 'MOVZX EAX #1|' | Call ReplaceOperand 1 0 'EAX'
        End_If
    .Else
        If D@type = MemoryOperand
            EPP_Code 'SUB ESP 04|{?? ESI-000}|MOVZX EAX #2|MOV D$? EAX|'
            Call ReplaceOperand 2 'D' 0
        Else
            EPP_Code 'MOVZX EAX #2|' | Call ReplaceOperand 2 0 'EAX'
        End_If
    .End_If

EndP
____________________________________________________________________________________________
Proc WriteFloatingOperationInstruction:
    Arguments @OperationType @OperandsType

    .Test_If D@OperandsType EqualPreparser_ST1_ST0
        .If D@OperationType = addSign
            EPP_Code 'FADDP ST1 ST0|'
        .Else_If D@OperationType = subSign
            EPP_Code 'FSUBP ST1 ST0|'
        .Else_If D@OperationType = mulSign
            EPP_Code 'FMULP ST1 ST0|'
        .Else_If D@OperationType = divSign
            EPP_Code 'FDIVP ST1 ST0|'
        .Else_If D@OperationType = expSign
            EPP_Code 'FXCH|'
            EPP_Code 'FYL2X|FLD ST0|FRNDINT|FSUB ST1 ST0|'
            EPP_Code 'FLD1|FSCALE|FSTP ST1|FXCH|F2XM1|'
            EPP_Code 'FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        .End_If

    .Test_Else_If D@OperandsType EqualPreparser_ST0_ST1
        .If D@OperationType = addSign
            EPP_Code 'FADDP ST1 ST0|'
        .Else_If D@OperationType = subSign
            EPP_Code 'FSUBRP ST1 ST0|'
        .Else_If D@OperationType = mulSign
            EPP_Code 'FMULP ST1 ST0|'
        .Else_If D@OperationType = divSign
            EPP_Code 'FDIVRP ST1 ST0|'
        .Else_If D@OperationType = expSign
            EPP_Code 'FYL2X|FLD ST0|FRNDINT|FSUB ST1 ST0|'
            EPP_Code 'FLD1|FSCALE|FSTP ST1|FXCH|F2XM1|'
            EPP_Code 'FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        .End_If

    .Test_Else_If D@OperandsType EqualPreparser_IntegerOperand_ST0
        .If D@OperationType = addSign
            EPP_Code 'FIADD #1|'
        .Else_If D@OperationType = subSign
            EPP_Code 'FISUBR #1|'
        .Else_If D@OperationType = mulSign
            EPP_Code 'FIMUL #1|'
        .Else_If D@OperationType = divSign
            EPP_Code 'FIDIVR #1|'
        .Else_If D@OperationType = expSign
            EPP_Code 'FILD #1|'
            EPP_Code 'FYL2X|FLD ST0|FRNDINT|FSUB ST1 ST0|'
            EPP_Code 'FLD1|FSCALE|FSTP ST1|FXCH|F2XM1|'
            EPP_Code 'FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        .End_If

    .Test_Else_If D@OperandsType EqualPreparser_ST0_IntegerOperand
        .If D@OperationType = addSign
            EPP_Code 'FIADD #2|'
        .Else_If D@OperationType = subSign
            EPP_Code 'FISUB #2|'
        .Else_If D@OperationType = mulSign
            EPP_Code 'FIMUL #2|'
        .Else_If D@OperationType = divSign
            EPP_Code 'FIDIV #2|'
        .Else_If D@OperationType = expSign
            EPP_Code 'FILD #2|FXCH|'
            EPP_Code 'FYL2X|FLD ST0|FRNDINT|FSUB ST1 ST0|'
            EPP_Code 'FLD1|FSCALE|FSTP ST1|FXCH|F2XM1|'
            EPP_Code 'FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        .End_If

    .Test_Else_If D@OperandsType EqualPreparser_FloatingPointOperand_ST0
        .If D@OperationType = addSign
            EPP_Code 'FADD #1|'
        .Else_If D@OperationType = subSign
            EPP_Code 'FSUBR #1|'
        .Else_If D@OperationType = mulSign
            EPP_Code 'FMUL #1|'
        .Else_If D@OperationType = divSign
            EPP_Code 'FDIVR #1|'
        .Else_If D@OperationType = expSign
            EPP_Code 'FLD #1|'
            EPP_Code 'FYL2X|FLD ST0|FRNDINT|FSUB ST1 ST0|'
            EPP_Code 'FLD1|FSCALE|FSTP ST1|FXCH|F2XM1|'
            EPP_Code 'FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        .End_If

    .Test_Else_If D@OperandsType EqualPreparser_ST0_FloatingPointOperand
        .If D@OperationType = addSign
            EPP_Code 'FADD #2|'
        .Else_If D@OperationType = subSign
            EPP_Code 'FSUB #2|'
        .Else_If D@OperationType = mulSign
            EPP_Code 'FMUL #2|'
        .Else_If D@OperationType = divSign
            EPP_Code 'FDIV #2|'
        .Else_If D@OperationType = expSign
            EPP_Code 'FLD #2|FXCH|'
            EPP_Code 'FYL2X|FLD ST0|FRNDINT|FSUB ST1 ST0|'
            EPP_Code 'FLD1|FSCALE|FSTP ST1|FXCH|F2XM1|'
            EPP_Code 'FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        .End_If

    .Test_Else_If D@OperandsType EqualPreparser_ConstantOperand_ST0
        .If D@OperationType = addSign
            EPP_Code 'FADD R$?|'
        .Else_If D@OperationType = subSign
            EPP_Code 'FSUBR R$?|'
        .Else_If D@OperationType = mulSign
            EPP_Code 'FMUL R$?|'
        .Else_If D@OperationType = divSign
            EPP_Code 'FDIVR R$?|'
        .Else_If D@OperationType = expSign
            EPP_Code 'FLD R$?|'
            EPP_Code 'FYL2X|FLD ST0|FRNDINT|FSUB ST1 ST0|'
            EPP_Code 'FLD1|FSCALE|FSTP ST1|FXCH|F2XM1|'
            EPP_Code 'FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        .End_If

    .Test_Else_If D@OperandsType EqualPreparser_ST0_ConstantOperand
        .If D@OperationType = addSign
            EPP_Code 'FADD R$?|'
        .Else_If D@OperationType = subSign
            EPP_Code 'FSUB R$?|'
        .Else_If D@OperationType = mulSign
            EPP_Code 'FMUL R$?|'
        .Else_If D@OperationType = divSign
            EPP_Code 'FDIV R$?|'
        .Else_If D@OperationType = expSign
            EPP_Code 'FLD R$?|FXCH|'
            EPP_Code 'FYL2X|FLD ST0|FRNDINT|FSUB ST1 ST0|'
            EPP_Code 'FLD1|FSCALE|FSTP ST1|FXCH|F2XM1|'
            EPP_Code 'FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        .End_If

    .Test_Else
        error EqualPreParser_UnexpectedError
    .Test_End

EndP
____________________________________________________________________________________________
Proc IntegratedFunctionManagement:
    ; This handle Arrays and functions, respectively manged by WriteArrayCode and WriteFunctionCode
    ; At this point, if we have: _FUNCTION_PARAMETER_
    ; D@OpeningParenthesis points on PARAMETER.
    ; #1 is set on PARAMETER and #2 is set on FUNCTION
    Argument @OpeningParenthesis
    Local @FunctionNamePos @ParameterNamePos @FunctionNameLength
    Uses ebx, ecx, edi

    Mov ebx D@OpeningParenthesis
    Mov D$EPP_Operand1 ebx
    Mov D@ParameterNamePos ebx

    dec ebx | On ebx = EPP_SourceEquation, ExitP   ; Exit if no function name in front of the parenthesis
    On B$ebx-1 <= openSign, ExitP                  ;

    Mov D@FunctionNameLength 0
    lea ecx D$ebx-1 | While B$ecx > Space | dec ecx | inc D@FunctionNameLength | End_While
    inc ecx | Mov D@FunctionNamePos ecx | inc D@FunctionNameLength

    Mov D$EPP_Operand2 ecx
    If B$ecx+1 = memMarker ; Arrays
        Call WriteArrayCode D@FunctionNamePos D@ParameterNamePos D@FunctionNameLength

    Else ; ex: _COS_F$VAL1_
        Call WriteFunctionCode D@FunctionNamePos D@FunctionNameLength

    End_If

EndP
____________________________________________________________________________________________
Proc WriteArrayCode:
    Arguments @ArrayNamePos @ParameterNamePos @FunctionNameLength
    ; we have: _X$ARRAY_PARAMETER_
    ; #1 points on PARAMETER and #2 points on X$ARRAY

    Mov ecx D@ArrayNamePos
    Mov ebx D@ParameterNamePos

    Call GetStringType D$EPP_Operand1
    ..Test_If ax MemoryOperand ; _F$TBL_D$AAAAAAAA_ ;  _F$_D$MyVal
        If_Or B$ebx = 'B', B$ebx = 'W'
            Call ExtendOperandToDWord 1 RegisterOperand | jmp L1>
        Else_If B$ebx = 'D'
L1:         On B$ecx+2 <> Space, Call WriteAddressingCode D$ecx
            Call EraseArrayName D@FunctionNameLength

        Else ; index is not an integer
             error EqualPreparser_InvalidIndex
        End_If

    ..Test_Else_If ax RegisterOperand  ; _F$ARRAY_ECX_
        If B$ecx+2 <> Space ; if not an expression
            On B$ecx = 'T', jmp L1<<
            Mov B$ebx-1 plusSign
            Test_If ax DWordValue
                lea edi D$ebx+3 | Call shiftLineRight 2         ; _F$ARRAY_ECX_  => _F$ARRAY+ECX*4_
                Mov B$ebx+3 addressSign
                Call GetAddressingCoef ecx | Mov B$ebx+4 al
            Test_Else
                lea edi D$ebx+2 | Call shiftLineRight 2         ; _W$ARRAY_CL_   => _W$ARRAY+CL*2_
                Mov B$ebx+2 addressSign
                Call GetAddressingCoef ecx | Mov B$ebx+3 al
            Test_End
        Else ; F$_ECX  ; D$_EDI
            Mov edi ebx | Call shiftLineLeft 1
        End_If

    ..Test_Else_If ax NumericOperand  ; _F$ARRAY_2_
        .If B$ecx+2 <> Space ; if not an expression
            On B$ecx = 'T', jmp L1<<
            If B$ebx-2 <> plusSign                     ; first step  : _F$ARRAY_13_ => _F$ARRAY+_(_13_*_4_)_
                Mov B$ebx-1 plusSign
                Mov edi ebx | Call shiftLineRight 3
                Mov B$ebx Space | Mov B$ebx+1 openSign | Mov B$ebx+2 Space

                add ebx 3 | While B$ebx > Space | inc ebx | End_While | inc ebx

                Mov edi ebx | Call shiftLineRight 6

                Mov B$ebx mulSign | Mov B$ebx+1 Space | Call GetAddressingCoef ecx | Mov B$ebx+2 al
                Mov B$ebx+3 Space | Mov B$ebx+4 closeSign | Mov B$ebx+5 Space

            Else                                       ; second step : _F$ARRAY+_52_ => _F$ARRAY+52_
                Mov edi ebx | Call shiftLineLeft 1
            End_If
        .Else ; F$_1  ; D$_2254
            error EqualPreparser_InvalidAddress
        .End_If

    ..Test_Else
        error EqualPreparser_InvalidIndex

    ..Test_End

EndP
____________________________________________________________________________________________
Proc EraseArrayName:
    Argument @FunctionNameLength

    Mov edi D$EPP_Operand1
    Mov ecx D@FunctionNameLength | sub ecx 2
    Call ShiftLineLeft ecx

EndP
____________________________________________________________________________________________
Proc LoadOperandOnTOS:
    ;This writes the code which load the D@Operand (1 or 2) on the Top of the FPU Stack.
    Arguments @Operand
    Uses ebx, ecx

    If D@Operand = 1
        Mov ebx D$EPP_Operand1
        Mov cx '#1'
    Else
        Mov ebx D$EPP_Operand2
        Mov cx '#2'
    End_If

    .If B$ebx+1 = memMarker
        If_Or B$ebx = 'T', B$ebx = 'F', B$ebx = 'R'
            EPP_Code 'FLD ' cx '|' | Call incFPUStackNumber
        Else_If B$ebx = 'B'
            Call ExtendOperandToDWord 1 MemoryOperand
            EPP_Code 'FILD ' cx '|' | Call incFPUStackNumber
        Else  ;  others integer
            EPP_Code 'FILD ' cx '|' | Call incFPUStackNumber
        End_If
    .Else_If W$ebx = 'st'
        ; result is already on TOS

    .Else ; constant
        Call IsARegister ebx
        Test_If ax (ByteValue or WordValue)
            Call ExtendOperandToDWord 1 MemoryOperand
            EPP_Code 'FILD ' cx '|'
        Test_Else_If ax DWordValue
            EPP_Code 'SUB ESP 04|{?? ESI-000}|MOV D$? ' cx '|FILD D$?|'
        Test_Else
            EPP_Code '{??:R$ ' cx '}|FLD R$?|' | Call incFPUStackNumber
        Test_End

    .End_If

EndP
____________________________________________________________________________________________
Proc IsARegister:
    ; Returns &FALSE if D@pos does NOT points on a register (it can be terminated by EOI,meEOI,Space or &NULL)
    ; Otherwise returns '(RegisterOperand OR SizeOp)' with SizeOp, the size of the register.
    ; (which can be ByteValue,WordValue,DWordValue,FloatingPointOperand+TByteValue)
    Argument @pos
    Uses esi

    Mov eax &FALSE
    Mov esi D@pos
    ..If B$esi+2 <= Space
        .If_Or B$esi+1 = 'H', B$esi+1 = 'L'
            If_And B$esi >= 'A', B$esi <= 'D'
                Mov eax (RegisterOperand or ByteValue)
            End_If

        .Else_If_Or W$esi = 'AX', W$esi = 'BX', W$esi = 'CX', W$esi = 'DX',
                    W$esi = 'BP', W$esi = 'DI', W$esi = 'SI'
            Mov eax (RegisterOperand or WordValue)
        .End_If

    ..Else_If B$esi+3 <= Space
        .If B$esi = 'E'
            If_Or W$esi+1 = 'AX', W$esi+1 = 'BX', W$esi+1 = 'CX', W$esi+1 = 'DX',
                  W$esi+1 = 'BP', W$esi+1 = 'DI', W$esi+1 = 'SI'
                Mov eax (RegisterOperand or DWordValue)
            End_If
        .Else_If_Or W$esi = 'ST', W$esi = 'st'
            ; 'st' is for temporary results stored on the FPU Stack (See incFPUStackNumber)
            If_And B$esi+2 >= '0', B$esi+2 <= '7'
                Mov eax (RegisterOperand or FloatingPointOperand or TByteValue)
            End_If
        .End_If
    ..End_If

EndP
____________________________________________________________________________________________
Proc GetAddressingCoef:
    ; returns in al the corresponding size of D@SizeMarkerPos
    Argument @SizeMarkerPos
    Uses ecx

    Mov ecx D@SizeMarkerPos
    .If B$ecx = 'B'
        Mov al '1'
    .Else_If B$ecx = 'R'
        Mov al '8'
    .Else_If B$ecx = 'F'
        Mov al '4'
    .Else_If B$ecx = 'D'
        Mov al '4'
    .Else_If B$ecx = 'W'
        Mov al '2'
    .Else_If B$ecx = 'Q'
        Mov al '8'
    .End_If
EndP
____________________________________________________________________________________________
Proc WriteFunctionCode:
    ;Set the result of the operations on the FPU Stack and compute the function
    ; we have: _FUNCTION_PARAMETER_
    ; #1 points on PARAMETER and #2 points on FUNCTION
    Arguments @FunctionNamePos @FunctionNameLength

    .If_str D@FunctionNamePos = 'COS'
        Call LoadOperandOnTOS 1
        EPP_Code 'FCOS|'
        Call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'SIN'
        Call LoadOperandOnTOS 1
        EPP_Code 'FSIN|'
        Call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'TAN'
        Call LoadOperandOnTOS 1
        EPP_Code 'FPTAN|FINCSTP|FFREE ST7|'
        Call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'ABS'
        Call LoadOperandOnTOS 1
        EPP_Code 'FABS|'
        Call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'SQRT'
        Call LoadOperandOnTOS 1
        EPP_Code 'FSQRT|'
        Call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'LOG10'
        EPP_Code 'FLDLG2|' | Call incFPUStackNumber
        Call LoadOperandOnTOS 1
        EPP_Code 'FYL2X|' | Call decFPUStackNumber
        Call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'LOG2'
        EPP_Code 'FLD1|' | Call incFPUStackNumber
        Call LoadOperandOnTOS 1
        EPP_Code 'FYL2X|' | Call decFPUStackNumber
        Call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'LN'
        EPP_Code 'FLDLN2|' | Call incFPUStackNumber
        Call LoadOperandOnTOS 1
        EPP_Code 'FYL2X|' | Call decFPUStackNumber
        Call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'ATAN'
        Call LoadOperandOnTOS 1
        EPP_Code 'FLD1|'
        EPP_Code 'FPATAN|'
        Call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'EXP'
        Call LoadOperandOnTOS 1
        EPP_Code 'FLDL2E|FMULP ST1 ST0|FLD ST0|FRNDINT|FSUB ST1 ST0|FLD1|'
        EPP_Code 'FSCALE|FSTP ST1|FXCH|F2XM1|FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        Call StoreFPUResult 1

    .Else
        error EqualPreparser_InvalidFunction
    .End_If


    Mov edi D$EPP_Operand1 | Call ShiftLineLeft D@FunctionNameLength ; erase function name

EndP
____________________________________________________________________________________________
Proc UpdateLocalDataShift:
    Local @LocalDataShift @RegisterLetter

    Mov D@LocalDataShift 0
    Mov ecx D$EPP_CodeBegining
    Mov ebx &FALSE


    EPP_FindCodePos 'MOV ESI ESP'  EPP_CODE_BEGIN EPP_CODE_END
    jmp L1>
    .While eax <> CODE_NOT_FOUND
        add D@LocalDataShift 4 | inc eax

L1:     EPP_FindCodePos 'PUSH' eax EPP_CODE_BEGIN EPP_CODE_END
    .End_While

    jmp L1>>
    .While eax <> CODE_NOT_FOUND
        Mov ebx &TRUE
        Mov esi D$EPP_SearchedStringAddress
        If B$esi+2 < Separators
            Mov al B$esi+1 | sub al '0' | On al > 9 , sub al 7
        Else
            Mov al B$esi+1 | sub al '0' | On al > 9 , sub al 7 | shl al 4
            Mov ah B$esi+2 | sub ah '0' | On ah > 9 , sub ah 7
            add al ah
        End_If
        movzx edx al | add D@LocalDataShift edx

        EPP_FindCodePos 'ESI-' ecx EPP_CODE_END | add eax 5 | Mov ecx eax
        ; We always have 'SUB ESP &0|{&1 ESI-0xx}|'
        If D@LocalDataShift < 010
            Mov al B@LocalDataShift | add al '0' | On al > '9', add al 7
            Mov B$ecx al
            lea edi D$ecx+2 | Call ShiftLineLeft 1
        Else_If D@LocalDataShift > 0FF
            error EqualPreparser_LocalStackOverflow
        Else
            Mov al B@LocalDataShift
            Mov ah al | and ah 0F | shr al 4
            add ah '0' | On ah > '9', add ah 7 | Mov B$ecx+1 ah
            add al '0' | On al > '9', add al 7 | Mov B$ecx   al
        End_If


L1:     EPP_FindCodePos 'SUB ESP &0' ecx EPP_CODE_END | Mov ecx eax
    .End_While

    .If ebx = &FALSE
        EPP_ReplaceCode 'PUSH ESI|MOV ESI ESP'  0           EPP_CODE_BEGIN EPP_CODE_END
        EPP_ReplaceCode 'MOV ESP ESI|POP ESI'   0           EPP_CODE_BEGIN EPP_CODE_END
        EPP_ReplaceCode 'D$ESI+4'               'ESI'       EPP_CODE_BEGIN EPP_CODE_END
        EPP_ReplaceCode 'MOV &1 D$ESI-??'       0           EPP_CODE_BEGIN EPP_CODE_END

    .End_If

EndP
____________________________________________________________________________________________
Proc ReplaceCodeMacroProc:
    Arguments @CodeToReplace @CodeReplacement @Begin @End
    Uses ebx

    jmp L1>
    While eax <> CODE_NOT_FOUND
        Call ReplaceCodeProc eax D@CodeReplacement ebx

L1:     Call FindCodePosProc D@CodeToReplace D@Begin D@End
    End_While

EndP
____________________________________________________________________________________________
Proc ReplaceCodeProc:
    ; This procedure replaces the code at D@CodeToReplacePos by D@CodeReplacement.
    ; D@CodeLength is the length of the code to be replaced.
    ; In D@CodeReplacement two specials characters can be used :
    ;   - '!n' which is replaced by the a single characters and '&n' by an instruction/operand
    ;       (with n a digit between 1 and 9)
    ;       See FindCodePosProc for more information about theses internal variables.
    Arguments @CodeToReplacePos @CodeReplacement @CodeLength
    USes esi edi ecx

    Mov edi EPP_ReplacementBuffer
    Mov esi D@CodeReplacement

    Mov ecx 0
    lodsb
    ..While al <> 0
        inc ecx
        .If al = '!'
            movzx edx B$esi | sub edx '0' | inc esi
            Mov al B$EPP_SearchedSingleCharacter+edx

        .Else_If al = '&'
            movzx edx B$esi | sub edx '0' | inc esi | shl edx 2
            push esi
                Mov esi D$EPP_SearchedStringAddress+edx
                .While B$esi > LowSigns
                    movsb  | inc ecx
                    While B$esi = memMarker | movsb | End_While
                    On B$esi = '}', jmp L2>
                .End_While
                dec ecx
L2:         pop esi
            dec edi | Mov al B$edi
        .Else_If al = ' '
            Mov al Space
        .Else_If al = '$'
            Mov al memMarker
        .Else_If al = '|'
            If B$esi = '|'
                Mov al EOI | inc esi
            Else
                Mov al meEOI
            End_If
        .End_If
        stosb

        lodsb
    ..End_While
    Mov B$edi 0

    On ecx = 0, inc D@CodeLength
    Mov edi D@CodeToReplacePos | add edi D@CodeLength
    Call ShiftLineLeft D@CodeLength
    Mov eax D@CodeLength | sub D$EPP_WrittenCode eax
    Mov edi D@CodeToReplacePos
    Call ShiftLineRight ecx
    add D$EPP_WrittenCode ecx

    Mov esi EPP_ReplacementBuffer
    While B$esi <> 0 | movsb | End_While

EndP
____________________________________________________________________________________________
Proc FindCodePosProc:
    ; Returns: - in eax the position of D@CodeToFind between D@Begin and D@End
    ;          - in ebx the length of the code found.
    ; and CODE_NOT_FOUND otherwise.
    ; In D@CodeToFind the following specials characters are used :
    ;   - '?'  means 'any character'
    ;   - '!n' represents a single character which must be the same each timle it appears.
    ;          (n is a number between 0 and 9)
    ;   - '&n' represents a mnemonic or an operand '}'/Space/meEOI/EOI-terminated which must be the same
    ;          each time it appears. (n is a number between 0 and 9)
    ;   '&n' and '!n' internal variables are re-initialized each time this procedure is called, so that
    ;   they can be used by a Call to ReplaceCodeProc right after this procedure.
    ;
    ; - D@Begin can be:
    ;   * EPP_CODE_BEGIN   : to search up from the beginning of the produced code(in one Equal line).
    ;   * EPP_SOURCE_BEGIN : to search up from the beginning of the produced code(in one Equal line).
    ;
    ; - D@End can be:
    ;   * EPP_CODE_END   : to search up to the end of the produced code(in one Equal line).
    ;   * EPP_SOURCE_END : to search up to the end of the produced code(in one Equal line).
    ;
    ; This function must be called BEFORE TranslateExpressions
    Arguments  @CodeToFind @Begin @End
    Local @FirstCharPos @result
    Uses ecx, esi, edi, edx

    On D@Begin = EPP_CODE_BEGIN, move D@Begin D$EPP_CodeBegining
    On D@End   = EPP_CODE_END  , move D@End   D$EPP_WrittenCode
    On D@Begin = EPP_SOURCE_BEGIN, move D@Begin EPP_SourceEquation
    If D@End   = EPP_SOURCE_END
        Mov ecx EPP_SourceEquation
        While B$ecx <> EOI | inc ecx | End_While
        Mov D@End ecx
    End_If

    Mov ecx D@End | sub ecx D@Begin
    Test_If ecx 0_8000_0000  ; (D$EPP_WrittenCode - D@From) < 0   ==>   ExitP
        Mov eax CODE_NOT_FOUND
        ExitP
    Test_End

    Mov D@FirstCharPos &FALSE
    Mov D@result CODE_NOT_FOUND
    Mov esi D@CodeToFind
    Mov edi D@Begin
    Mov ebx 0
    Call InitSearchVariables

    ..While ecx >s 0
        lodsb | inc ebx
        ..If al = ' '
            Mov al Space
        ..Else_If al = '?'
            Mov al B$edi
        ..Else_If al = '$'
            Mov al memMarker
        ..Else_If al = ':'
            Mov al colonSign
        ..Else_If al = '-'
            Mov al minusSign
        ..Else_If al = '+'
            Mov al plusSign
        ..Else_If al = '*'
            Mov al addressSign
        ..Else_If al = '|'
            If B$esi = '|'
                Mov al EOI | inc esi
            Else
                Mov al meEOI
            End_If
        ..Else_If al = '!'
            movzx edx B$esi | sub edx '0' | inc esi
            If B$EPP_SearchedSingleCharacter+edx = 0
                Mov al B$edi
                Mov B$EPP_SearchedSingleCharacter+edx al
            Else
                Mov al B$EPP_SearchedSingleCharacter+edx
            End_If
        ..Else_If al = '&'
            movzx edx B$esi | sub edx '0' | inc esi | shl edx 2
            .If D$EPP_SearchedStringAddress+edx = 0
                Mov D$EPP_SearchedStringAddress+edx edi
                .While B$edi > LowSigns
                    inc edi | inc ebx
                    While B$edi = memMarker | inc edi | inc ebx | End_While
                    On B$edi = '}', jmp L2>
                .End_While
L2:             dec edi ebx | Mov al B$edi
            .Else
                Call CompareCode edi D$EPP_SearchedStringAddress+edx
                If eax = &TRUE
                    .While B$edi > LowSigns
                        inc edi | inc ebx
                        While B$edi = memMarker | inc edi | inc ebx | End_While
                        On B$edi = '}', jmp L2>
                    .End_While
L2:                 dec edi ebx | Mov al B$edi
                Else
                    Mov D@FirstCharPos &TRUE
                    Mov al 0
                End_If
            .End_If

        ..Else_If al = 0
            move D@result D@FirstCharPos
            jmp L5>
        ..End_If

        .If D@FirstCharPos <> &FALSE
            If B$edi <> al
                Mov D@FirstCharPos &FALSE
                Mov esi D@CodeToFind
                Call InitSearchVariables
            Else
                inc edi
            End_If
        .Else
            repne scasb
            Mov D@FirstCharPos edi | dec D@FirstCharPos
            Mov ebx 0
        .End_If

    ..End_While
L5: Mov eax D@result

EndP
____________________________________________________________________________________________
Proc CompareCode:
    ; returns &TRUE if D@Code1 and D@Code2 points on identicals mnemonics or operands ('}'/Space/meEOI/EOI-terminated)
    Arguments @Code1 @Code2
    Uses esi edi

    Mov esi D@Code1
    Mov edi D@Code2

    lodsb
    .While al = B$edi
        inc edi
        .If_And B$esi < LowSigns, B$edi < LowSigns, B$esi <> memMarker, B$edi <> memMarker
            Mov eax &TRUE  | ExitP
        .Else_If_And  B$esi = 125 , B$edi = 125 ; 125 = '}'
            Mov eax &TRUE | ExitP
        .Else_If_And  B$esi < LowSigns, B$edi = 125, B$esi <> memMarker
            Mov eax &TRUE | ExitP
        .Else_If_And  B$edi < LowSigns, B$esi = 125, B$edi <> memMarker
            Mov eax &TRUE | ExitP
        .Else_If_Or  B$esi = 125 , B$edi = 125
            Mov eax &FALSE | ExitP
        .Else_If_And B$esi < LowSigns, B$esi <> memMarker
            Mov eax &FALSE | ExitP
        .Else_If_And  B$edi < LowSigns, B$edi <> memMarker
            Mov eax &FALSE | ExitP
        .End_If
        lodsb
    .End_While
    Mov eax &FALSE

EndP
____________________________________________________________________________________________
InitSearchVariables:
    push ecx
        Mov ecx 0
        While ecx < SEARCH_VARIABLE_NUMBER
            Mov B$EPP_SearchedSingleCharacter+ecx 0
            Mov D$EPP_SearchedStringAddress+ecx*4   0
            inc ecx
        End_While
    pop ecx
ret
____________________________________________________________________________________________
Proc DeleteCodeLine:
    ; Delete a code line while parsing. (the end of the source is D$EPP_WrittenCode)
    Argument @LinePos
    Uses ecx, edi

    Mov edi D$EPP_WrittenCode | inc edi
    Call DeleteFinalCodeLine D@LinePos edi | sub edi eax
    Mov D$EPP_WrittenCode edi

EndP
____________________________________________________________________________________________
Proc DeleteFinalCodeLine:
    ; LinePos point on the beginning of a code line (after meEOI or EOI)
    ; returns the number of characters deleted.
    Arguments @LinePos @EndOfSource
    Uses ecx, esi, edi

    Mov edi D@LinePos
    Mov esi D@LinePos | While B$esi > EOI | inc esi | End_While
    Mov eax esi | sub eax D@LinePos | inc eax
    Mov ecx D@EndOfSource
    If_And B$esi = EOI, B$edi-1 = meEOI
        dec edi | dec ecx
    Else
        inc esi
    End_If
    sub ecx esi
    rep movsb

    ; reset to 0 the end of the source (needed for the macros unfolding)
    Mov ecx D@EndOfSource | sub ecx eax
    While ecx < D@EndOfSource | Mov B$ecx 0 | inc ecx | End_While

EndP
____________________________________________________________________________________________
Proc OptimizeEqualPreparserCode:
    Arguments @From @To

    Test_If_Not B$UsedRegisters EPP_EAX
        EPP_ReplaceCode 'PUSH EAX' 0 D@From  D@To
        EPP_ReplaceCode 'POP EAX'  0 D@From  D@To
    Test_End

    Test_If_Not B$UsedRegisters EPP_EBX
        EPP_ReplaceCode 'PUSH EBX' 0 D@From  D@To
        EPP_ReplaceCode 'POP EBX'  0 D@From  D@To
    Test_End

    Test_If_Not B$UsedRegisters EPP_ECX
        EPP_ReplaceCode 'PUSH ECX' 0 D@From  D@To
        EPP_ReplaceCode 'POP ECX'  0 D@From  D@To
    Test_End

    Test_If_Not B$UsedRegisters EPP_EDX
        EPP_ReplaceCode 'PUSH EDX' 0 D@From  D@To
        EPP_ReplaceCode 'POP EDX'  0 D@From  D@To
    Test_End

    Test_If_Not B$UsedRegisters EPP_ESI
        EPP_ReplaceCode 'PUSH ESI' 0 D@From  D@To
        EPP_ReplaceCode 'POP ESI'  0 D@From  D@To
    Test_End

    Test_If_Not B$UsedRegisters EPP_EDI
        EPP_ReplaceCode 'PUSH EDI' 0 D@From  D@To
        EPP_ReplaceCode 'POP EDI'  0 D@From  D@To
    Test_End


    EPP_ReplaceCode 'SUB ESP &0|{&1 ESI?&2}|MOV ?$&1 &3|MOV &4 ?$&1',
                    'MOV &4 &3',
                    D@From D@To


    EPP_ReplaceCode 'SUB ESP &0|{&1 ESI?&2}|MOV ?$&1 &3|PUSH ?$&1',
                    'PUSH &3',
                    D@From D@To


    EPP_ReplaceCode 'SUB ESP &0|{&1 ESI-&2}|MOV D$&1 &3|POP &4|MOV &5 D$&1',
                    'MOV &5 &3|POP &4',
                    D@From D@To

    EPP_ReplaceCode 'MOV &1 &1',
                    0,
                    D@From D@To

    EPP_ReplaceCode 'SUB ESP &0|{&1 ESI?&2}|FSTP ?$&1|FLD ?$&1',
                    0,
                    D@From D@To


EndP
____________________________________________________________________________________________
Proc EqualFirstScan:
    ; Replaces each register by a meaningless label and writes the code that copies its value in the memory.
    ; D@SecondPass is true when we are computing the address of an array.
    Argument @SecondPass

    Mov B$FPUInSourceExpression &FALSE
    Mov B$UsedRegisters  EPP_ESI

    ; identify destination type
    Call GetStringType EPP_DestOperand
    Mov D$DestinationType eax

    If D@SecondPass = &FALSE
        EPP_Code 'PUSH ESI|MOV ESI ESP|PUSH EDI|PUSH ECX|PUSH EAX|PUSH EBX|PUSH EDX|'
    Else
        ;...
    End_If

    Test_If_And D$DestinationType RegisterOperand, D$DestinationType FloatingPointOperand
        EPP_Code 'SUB ESP 06C|{&7 ESI-000}|FSAVE X$!7|FINIT|'
    Test_End

    Mov esi EPP_SourceEquation
    .While B$esi > EOI
        Mov D$EPP_Operand1 esi
        Call GetStringType esi
        ...Test_If eax RegisterOperand
            ..Test_If_Not D$NoOperation &TRUE
                .Test_If eax ByteValue
                    EPP_Code 'SUB ESP 01|{?? ESI-000}|MOV B$? #1|'
                    Call ReplaceOperand 1 'B' 0
                .Test_Else_If eax WordValue
                    If W$esi = 'SI'
                        EPP_Code '{?? ESI}|'
                        Call ReplaceOperand 1 'W' 0
                    Else
                        EPP_Code 'SUB ESP 02|{?? ESI-000}|MOV W$? #1|'
                        Call ReplaceOperand 1 'W' 0
                    End_If
                .Test_Else_If eax DWordValue
                    If_And W$esi = 'ES', B$esi+2 = 'I'
                        EPP_Code '{?? ESI}|'
                        Call ReplaceOperand 1 'D' 0
                    Else
                        EPP_Code 'SUB ESP 04|{?? ESI-000}|MOV D$? #1|'
                        Call ReplaceOperand 1 'D' 0
                    End_If
                .Test_End
            ..Test_Else
                Test_If eax FloatingPointOperand
                    EPP_Code 'SUB ESP 0A|{?? ESI-000}|FLD #1|FSTP T$?|'
                    Call ReplaceOperand 1 'T' 0
                    Mov B$FPUInSourceExpression &TRUE
                Test_End
            ..Test_End
        ...Test_End
        While B$esi > Space | inc esi | End_While | inc esi ; jump to next space
    .End_While


EndP
____________________________________________________________________________________________
FormatEqualLine:
; - Surrounds each parenthesis and operator by two spaces and performs a slight error processing.
; - Minus signs in front of numeric operands are replaced by minusSign
; - When '.' is a floating point (i.e not in a user-label) then it is replaced by pointSign
; - if an operator is doubled (**,//), it is replaced by its signed equivalent (SignedMulSign,SignedDivSign)

    Mov B$NoOperation &TRUE
    Mov esi EPP_SourceEquation
    .While B$esi > EOI
        ..If B$esi = openSign
            .If B$esi+1 = subSign   ; '(-9', '(-1'   OK
                If_And B$esi+2 >= '0', B$esi+2 <= '9'
                    Mov B$esi+1 minusSign
                    Call SourroundCurrentSign
                    inc esi
                Else
                    error EqualPreparser_OperatorsInvalid
                End_If
            .Else_If_And B$esi+1 < openSign, B$esi+1 <> Space
                error EqualPreparser_OperatorsInvalid
            .Else
                Call SourroundCurrentSign
            .End_If

        ..Else_If B$esi =  closeSign   ; ')+',')/', etc..   OK
            If B$esi+1 > closeSign
                error EqualPreparser_OperatorsInvalid
            Else
                Call SourroundCurrentSign
            End_If

        ..Else_If_And B$esi < OperatorSigns, B$esi > Separators
            Mov B$NoOperation &FALSE
            Mov al B$esi
            .If B$esi+1 = subSign          ; ...  +-7 ...  OK
                If_And B$esi+2 >= '0', B$esi+2 <= '9'
                    Mov B$esi+1 minusSign
                    Call SourroundCurrentSign
                    inc esi
                Else
                    error EqualPreparser_OperatorsInvalid
                End_If
            .Else_If B$esi+1 = al
                lea edi D$esi+1  | Call ShiftLineLeft 1       ; delete the doubled operator
                On al = mulSign, Mov B$esi SignedMulSign
                On al = divSign, Mov B$esi SignedDivSign
                Call SourroundCurrentSign

            .Else_If B$esi+1 < openSign
                error EqualPreparser_OperatorsInvalid
            .Else
                Call SourroundCurrentSign
            .End_If

        ..Else_If B$esi = '.'
            If_And B$esi+1 >= '0', B$esi+1 <= '9', B$esi-1 >= '0', B$esi-1 <= '9'
                Mov B$esi pointSign
            End_If
            inc esi

        ..Else
            inc esi
        ..End_If
    .End_While

ret
                                                ____________________________
SourroundCurrentSign:
    Mov edi esi
    Mov al B$esi
    .If B$esi-1 = Space
        If B$esi+1 <> Space     ; adds one space after the operator
            Call ShiftLineRight 1
            Mov B$esi+1 Space
        End_If
        add esi 2
    .Else_If B$esi+1 = Space    ; adds one space before the operator
        Call ShiftLineRight 1
        Mov B$esi   Space
        add esi 3
    .Else                       ; adds two spaces
        Call ShiftLineRight 2
        Mov B$esi     Space
        Mov B$esi+1   al
        Mov B$esi+2   Space
        add esi 3
    .End_If

ret
____________________________________________________________________________________________
____________________________________________________________________________________________
Proc ComputeImmediateExpression:
; This function computes immediates operations between constants.
; D@OperationType may be one of these (addSign,subSign,mulSign,divSign,expSign)
    Argument @OperationType
    Local @ValueExponent @temp
    Uses ecx, esi, edi
    finit
    Call AsciiToFloat D$EPP_Operand1 0    | On eax = &FALSE, error EqualPreparser_InvalidFPValue
    Call AsciiToFloat D$EPP_Operand2 0    | On eax = &FALSE, error EqualPreparser_InvalidFPValue

    On D@OperationType = addSign, fadd  ST0 ST1
    On D@OperationType = subSign, fsubr ST0 ST1
    On D@OperationType = mulSign, fmul  ST0 ST1
    On D@OperationType = divSign, fdivr ST0 ST1
    If D@OperationType = expSign
        fxch | fyl2x | fld ST0 | frndint | fsub ST1 ST0
        fld1 | fscale | fstp ST1 | fxch | f2xm1
        fld1 | faddp ST1 ST0 | fmulp ST1 ST0
    End_If

    fstp T$EPP_ImmediateResult
    Call FloatToUString EPP_ImmediateResult EPP_ImmediateResultString

    .If_str EPP_ImmediateResultString = 'INF'
        jmp L1>>
    .Else_If_str EPP_ImmediateResultString = 'SNaN'
        jmp L1>>
    .Else_If_str EPP_ImmediateResultString = 'QNaN'
        jmp L1>
    .Else_If_str EPP_ImmediateResultString = '-SNaN'
        jmp L1>
    .Else_If_str EPP_ImmediateResultString = '-QNaN'
        jmp L1>
    .Else_If_str EPP_ImmediateResultString = '-INF'
L1:     error EqualPreparser_InvalidImmVal
    .Else
        Mov esi EPP_ImmediateResultString
        Mov ecx 0
        While B$esi <> 0
            If B$esi = '.'
                Mov B$esi pointSign
            Else_If B$esi = 'e'
                Mov B$esi 'E'
            Else_If B$esi = '+'
                Mov B$esi plusSign
            Else_If B$esi = '-'
                Mov B$esi minusSign
            End_If
            inc esi, ecx
        End_While
        Call ReplaceOperand 0 0 ' '
        Mov edi D$EPP_Operand1 | inc edi | dec ecx
        Call ShiftLineRight ecx
        Mov esi EPP_ImmediateResultString
        Mov edi D$EPP_Operand1
        While B$esi <> 0 | movsb | End_While
    .End_If

EndP
____________________________________________________________________________________________
StoreInMemory:

    ...If_Or B$ebx = 'T', B$ebx = 'R', B$ebx = 'F'
    ;
    ; -> FIRST OPERAND IS FLOATING POINT (T$,R$ or F$):
    ;
        ..If B$edi+2 = memMarker
        ; second operand is memory data
            .If B$edi+1 = 'B'
                Call ExtendOperandToDWord 1 MemoryOperand
                jmp L1>
            .Else_If_Or B$edi+1 = 'D', B$edi+1 = 'Q', B$edi+1 = 'W'
L1:             EPP_Code 'FILD #1|FSTP #2|'

            .Else_If_Or B$edi+1 = 'T', B$edi+1 = 'R', B$edi+1 = 'F'
                EPP_Code 'FLD #1|FSTP #2|'
            .End_If

        ..Else_If W$edi+1 = 'st'
        ; result on the FPU Stack
            EPP_Code 'FSTP #2|'

        ..Else
        ; second operand is a single register, an integer number or an Equate
            lea eax D$edi+1 | Call IsARegister eax
            Test_If ax (ByteValue or WordValue)
                EPP_Code 'SUB ESP 04|{?? ESI-000}|MOVZX EAX #1|MOV D$? EAX|FILD D$?|FSTP #2|'
            Test_Else_If ax DWordValue
                EPP_Code 'SUB ESP 04|{?? ESI-000}|MOV D$? #1|FILD D$?|FSTP #2|'
            Test_Else
                EPP_Code '{??:R$ #1}|FLD R$?|FSTP #2|'
            Test_End
        ..End_If
    ;
    ; -> FIRST OPERAND IS INTEGER :
    ;
    ...Else_If B$ebx = 'B'
    ; BYTE
        ..If B$edi+2 = memMarker
            .If B$edi+1 = 'B'
                EPP_Code 'MOV AL #1|MOV #2 AL|' | or B$UsedRegisters EPP_EAX
            .Else
                error EqualPreparser_SourceInvalid
            .End_If

        ..Else_If W$edi+1 = 'st'
        ; result on the FPU Stack
            error EqualPreparser_SourceInvalid

        ..Else
        ; second operand is a single register, an integer number or an Equate
            lea eax D$edi+1 | Call IsARegister eax
            If ax <> &FALSE
                Test_If_Not ax ByteValue | error EqualPreparser_SourceInvalid | Test_End
            End_If
            EPP_Code 'MOV #2 #1|'
        ..End_If

    ...Else_If B$ebx = 'W'
    ; WORD
        ..If B$edi+2 = memMarker
            .If B$edi+1 = 'B'
                EPP_Code 'MOVZX AX #1|MOV #2 AX|' | or B$UsedRegisters EPP_EAX
            .Else_If B$edi+1 = 'W'
                EPP_Code 'MOV AX #1|MOV #2 AX|' | or B$UsedRegisters EPP_EAX
            .Else_If_Or B$edi+1 = 'T', B$edi+1 = 'R', B$edi+1 = 'F'
                EPP_Code 'FLD #1|FISTP #2|'
            .Else
                error EqualPreparser_SourceInvalid
            .End_If

        ..Else_If W$edi+1 = 'st'
        ; result on the FPU Stack
            EPP_Code 'FISTP #2|'

        ..Else
        ; second operand is a single register, an integer number or an Equate
            lea eax D$edi+1 | Call IsARegister eax
            If ax <> &FALSE
                Test_If_Not ax (ByteValue or WordValue) | error EqualPreparser_SourceInvalid | Test_End
            End_If
            Test_If ax ByteValue
                EPP_Code 'MOVZX #2 #1|'
            Test_Else
                EPP_Code 'MOV #2 #1|'
            Test_End
        ..End_If

    ...Else_If B$ebx = 'Q'
    ; QWORD
        ..If B$edi+2 = memMarker
            .If B$edi+1 = 'Q'
                Mov B$edi+1 'D'  ; the QWORD is moved in two steps, DWord by DWord
                Mov B$ebx   'D'
                EPP_Code 'PUSH #1|POP #2|PUSH #1+4|POP #2+4|'

            .Else_If_Or B$edi+1 = 'B', B$edi+1 = 'W'
                Call ExtendOperandToDWord 1 RegisterOperand
                jmp L1>
            .Else_If B$edi+1 = 'D'
L1:             Mov B$ebx 'D'
                EPP_Code 'PUSH #1|POP #2|MOV #2+4 0|'

            .Else_If_Or B$edi+1 = 'T', B$edi+1 = 'R', B$edi+1 = 'F'
                EPP_Code 'FLD #1|FISTP #2|'

            .Else
                error EqualPreparser_SourceInvalid
            .End_If

        ..Else_If W$edi+1 = 'st'
        ; result on the FPU Stack
            EPP_Code 'FISTP #2|'

        ..Else
        ; second operand is a single register, an integer number or an Equate
            lea eax D$edi+1 | Call IsARegister eax
            Test_If ax ByteValue
                Mov B$ebx 'B'
            Test_Else_If ax WordValue
                Mov B$ebx 'W'
            Test_Else
                Mov B$ebx 'D'
            Test_End
            EPP_Code 'MOV #2 #1|MOV #2+4 0|'
        ..End_If

    ...Else_If B$ebx = 'D'
    ; DWORD
        ..If B$edi+2 = memMarker
        ; second operand is memory data
            .If_Or B$edi+1 = 'B', B$edi+1 = 'W'
                Call ExtendOperandToDWord 1 RegisterOperand
                jmp L1>
            .Else_If B$edi+1 = 'D'
L1:             EPP_Code 'PUSH #1|POP #2|'
            .Else_If_Or B$edi+1 = 'T', B$edi+1 = 'R', B$edi+1 = 'F'
                EPP_Code 'FLD #1|FISTP #2|'
            .Else
                error EqualPreparser_SourceInvalid
            .End_If

        ..Else_If W$edi+1 = 'st'
        ; result on the FPU Stack
            EPP_Code 'FISTP #2|'

        ..Else
        ; second operand is a single register, an integer number or an Equate
            lea eax D$edi+1 | Call IsARegister eax
            Test_If ax (ByteValue or WordValue)
                EPP_Code 'MOVZX #2 #1|'
            Test_Else
                EPP_Code 'MOV #2 #1|'
            Test_End
        ..End_If

    ...Else
    ; Invalid data identifier
        error EqualPreparser_DestInvalid
    ...End_If

ret
____________________________________________________________________________________________
Proc StoreIn32BitsRegister:

    ..If B$edi+2 = memMarker
        ; second operand is memory data
        .If_Or B$edi+1 = 'B', B$edi+1 = 'W'
            Mov D$EPP_LastStorageStatement_InMemory {'MOVZX #2 #1|',0}
        .Else_If B$edi+1 = 'D'
            Mov D$EPP_LastStorageStatement_InMemory {'MOV #2 #1|',0}
        .Else_If_Or B$edi+1 = 'F', B$edi+1 = 'R', B$edi+1 = 'T'
            EPP_Code 'SUB ESP 04|{?? ESI-000}|FLD #1|FISTP D$?|'
            Mov D$EPP_LastStorageStatement_InMemory {'MOV #2 D$?|',0}
        .Else
            error EqualPreparser_SourceInvalid
        .End_If

    ..Else_If W$edi+1 = 'st'
        ; result on the FPU Stack
        EPP_Code 'SUB ESP 04|{?? ESI-000}|FISTP D$?|'
        Mov D$EPP_LastStorageStatement_InMemory {'MOV #2 D$?|',0}

    ..Else
    ; second operand is an integer number or a register or an equate
        lea eax D$edi+1 | Call IsARegister eax
        Test_If ax (ByteValue or WordValue)
            Mov D$EPP_LastStorageStatement {'MOVZX #2 #1|',0}
        Test_Else
            Mov D$EPP_LastStorageStatement {'MOV #2 #1|',0}
        Test_End
    ..End_If

EndP
____________________________________________________________________________________________
StoreInFPURegister:

    ..If B$edi+2 = memMarker
    ; second operand is memory data
        EPP_Code 'FRSTOR X$!7|'
        .If_Or B$edi+1 = 'B'
            Call ExtendOperandToDWord 1 MemoryOperand
            jmp L1>
        .Else_If_Or B$edi+1 = 'W', B$edi+1 = 'D', B$edi+1 = 'Q'
L1:         If W$EPP_DestOperand+1 = 'T7'
                EPP_Code 'FFREE ST7|FILD #1|FINCSTP|'
            Else
                inc B$EPP_DestOperand+2
                EPP_Code 'FILD #1|FSTP #2|'
            End_If
        .Else_If_Or B$edi+1 = 'R', B$edi+1 = 'F', B$edi+1 = 'T'
            If W$EPP_DestOperand+1 = 'T7'
                EPP_Code 'FFREE ST7|FLD #1|FINCSTP|'
            Else
                inc B$EPP_DestOperand+2
                EPP_Code 'FLD #1|FSTP #2|'
            End_If

        .Else
            error EqualPreparser_SourceInvalid
        .End_If

    ..Else_If W$edi+1 = 'st'
        ; result on the FPU Stack
        EPP_Code 'SUB ESP 0A|{?? ESI-000}|FSTP T$?|'
        Call ReplaceOperand 1 'T' 0
        Mov D$EPP_Operand2 EPP_DestOperand
        EPP_Code 'FRSTOR X$!7|'
        If W$EPP_DestOperand+1 = 'T7'
            EPP_Code 'FFREE ST7|FLD #1|FINCSTP|'
        Else
            inc B$EPP_DestOperand+2
            EPP_Code 'FLD #1|FSTP #2|'
        End_If

    ..Else
        ; second operand is a register, an integer number or an equate
        EPP_Code 'FRSTOR X$!7|'
        On W$EPP_DestOperand+1 = 'T7', EPP_Code 'FFREE ST7|'
        lea eax D$edi+1 | Call IsARegister eax
        Test_If ax (ByteValue or WordValue)
            EPP_Code 'SUB ESP 04|{?? ESI-000}|MOVZX EAX #1|MOV D$? EAX|FILD D$?|'
        Test_Else_If ax DWordValue
            EPP_Code 'SUB ESP 04|{?? ESI-000}|MOV D$? #1|FILD D$?|'
        Test_Else
            EPP_Code '{??:R$ #1}|FLD R$?|'
        Test_End

        If W$EPP_DestOperand+1 = 'T7'
            EPP_Code 'FINCSTP|'
        Else
            inc B$EPP_DestOperand+2
            EPP_Code 'FSTP #2|'
        End_If
    ..End_If

ret
____________________________________________________________________________________________
StoreIn16BitsRegister:

    ..If B$edi+2 = memMarker
    ; second operand is memory data
        .If B$edi+1 = 'B'
            Mov D$EPP_LastStorageStatement_InMemory {'MOVZX #2 #1|',0}
        .Else_If B$edi+1 = 'W'
            Mov D$EPP_LastStorageStatement_InMemory {'MOV #2 #1|',0}
        .Else_If_Or B$edi+1 = 'F', B$edi+1 = 'R', B$edi+1 = 'T'
            EPP_Code 'SUB ESP 02|{?? ESI-000}|FLD #1|FISTP W$?|'
            Mov D$EPP_LastStorageStatement_InMemory {'MOV #2 W$?|',0}
        .Else
            error EqualPreparser_SourceInvalid
        .End_If

    ..Else_If W$edi+1 = 'st'
        ; result on the FPU Stack
        EPP_Code 'SUB ESP 02|{?? ESI-000}|FISTP W$?|'
        Mov D$EPP_LastStorageStatement_InMemory {'MOV #2 W$?|',0}

    ..Else
    ; second operand is an integer number, a register or an equate
        lea eax D$edi+1 | Call IsARegister eax
        Test_If ax ByteValue
            Mov D$EPP_LastStorageStatement {'MOVZX #2 #1|',0}
        Test_Else_If_Not ax DWord
            Mov D$EPP_LastStorageStatement {'MOV #2 #1|',0}
        Test_Else
            error EqualPreparser_SourceInvalid
        Test_End
    ..End_If

ret
____________________________________________________________________________________________
StoreIn8BitsRegister:

    .If B$edi+2 = memMarker
    ; second operand is memory data
        If B$edi+1 = 'B'
            Mov D$EPP_LastStorageStatement_InMemory {'MOV #2 #1|',0}
        Else
            error EqualPreparser_SourceInvalid
        End_If
    .Else
    ; second operand is an integer number, a register or an equate
        On W$edi+1 = 'st', error EqualPreparser_SourceInvalid
        lea eax D$edi+1 | Call IsARegister eax
        If ax <> &FALSE
            Test_If_Not ax ByteValue | error EqualPreparser_SourceInvalid | Test_End
        End_If
        Mov D$EPP_LastStorageStatement {'MOV #2 #1|',0}
    .End_If

ret
____________________________________________________________________________________________








____________________________________________________________________________________________
; EPP Macros:
[EPP_Code
#If #1=str
    EPP_CodeDirect #1
#Else_If #1=mem
    EPP_CodePtr #1
#Else_If #1=reg
    #If #1=D
        EPP_CodeReg #1 D
    #Else_If #1=W
        EPP_CodeReg #1 W
    #Else_If #1=B
        EPP_CodeReg #1 B
    #End_If
#End_If
#+1]

[EPP_CodeReg | {&0: 0} | Mov #2$&0 #1 | Call ImmediateCodeParser &0]
[EPP_CodePtr | lea eax #1 | Call ImmediateCodeParser eax]
[EPP_CodeDirect | {&0:B$ #1,0} | Call ImmediateCodeParser &0]

; returns in fourth parameter (if it exists) the length of string
[EPP_FindCodePos | {&0:B$ #1,0} | push ebx | Call FindCodePosProc &0 #2 #3 | pop ebx ]

[EPP_ReplaceCode | {&0:B$ #2,0} | Call ReplaceCodeMacroProc {#1,0} &0 #3 #4 ]

[ReplaceByAddressingOperator
mov B$esi-2 #1
lea edi D$esi   | Call ShiftLineLeft 1
lea edi D$esi-2 | Call ShiftLineLeft 1
sub esi 2]


[Test_If    |#=2| test #1 #2 | jz T0> ]
[.Test_If   |#=2| test #1 #2 | jz T1>>]
[..Test_If  |#=2| test #1 #2 | jz T2>>]
[...Test_If |#=2| test #1 #2 | jz T3>>]

[Test_If_Not     |#=2| test #1 #2 | jnz T0> ]
[.Test_If_Not    |#=2| test #1 #2 | jnz T1>>]
[..Test_If_Not   |#=2| test #1 #2 | jnz T2>>]
[...Test_If_Not  |#=2| test #1 #2 | jnz T3>>]


[Test_Else    | jmp T5>  | T0: ]
[.Test_Else   | jmp T6>> | T1: ]
[..Test_Else  | jmp T7>> | T2: ]
[...Test_Else | jmp T8>> | T3: ]

[Test_End    | T0: | T5: ]
[.Test_End   | T1: | T6: ]
[..Test_End  | T2: | T7: ]
[...Test_End | T3: | T8: ]



[Test_If_And    | Test_If    #1 #2 | #+2]
[.Test_If_And   | .Test_If   #1 #2 | #+2]
[..Test_If_And  | ..Test_If  #1 #2 | #+2]
[...Test_If_And | ...Test_If #1 #2 | #+2]

[Test_If_Not_And    | Test_If_Not    #1 #2 | #+2]
[.Test_If_Not_And   | .Test_If_Not   #1 #2 | #+2]
[..Test_If_Not_And  | ..Test_If_Not  #1 #2 | #+2]
[...Test_If_Not_And | ...Test_If_Not #1 #2 | #+2]


[Test_Else_If    | Test_Else    | Test_If    #1 #2]
[.Test_Else_If   | .Test_Else   | .Test_If   #1 #2]
[..Test_Else_If  | ..Test_Else  | ..Test_If  #1 #2]
[...Test_Else_If | ...Test_Else | ...Test_If #1 #2]

[Test_Else_If_Not    | Test_Else    | Test_If_Not    #1 #2]
[.Test_Else_If_Not   | .Test_Else   | .Test_If_Not   #1 #2]
[..Test_Else_If_Not  | ..Test_Else  | ..Test_If_Not  #1 #2]
[...Test_Else_If_Not | ...Test_Else | ...Test_If_Not #1 #2]


[Test_Else_If_And    | Test_Else    | Test_If_And    #F>L ]
[.Test_Else_If_And   | .Test_Else   | .Test_If_And   #F>L ]
[..Test_Else_If_And  | ..Test_Else  | ..Test_If_And  #F>L ]
[...Test_Else_If_And | ...Test_Else | ...Test_If_And #F>L ]

[Test_Else_If_Not_And    | Test_Else    | Test_If_Not_And    #F>L ]
[.Test_Else_If_Not_And   | .Test_Else   | .Test_If_Not_And   #F>L ]
[..Test_Else_If_Not_And  | ..Test_Else  | ..Test_If_Not_And  #F>L ]
[...Test_Else_If_Not_And | ...Test_Else | ...Test_If_Not_And #F>L ]



[If_Or    | cmp #1 #3 | j#2 O0>  | #+3 | jmp I1>  | O0: ]
[.If_Or   | cmp #1 #3 | j#2 O1>> | #+3 | jmp J1>> | O1: ]
[..If_Or  | cmp #1 #3 | j#2 O2>> | #+3 | jmp K1>> | O2: ]
[...If_Or | cmp #1 #3 | j#2 O3>> | #+3 | jmp Z1>> | O3: ]

[Else_If_Or    | Else    | If_Or    #F>L]
[.Else_If_Or   | .Else   | .If_Or   #F>L]
[..Else_If_Or  | ..Else  | ..If_Or  #F>L]
[...Else_If_Or | ...Else | ...If_Or #F>L]


[If_And    | If #1 #2 #3    | #+3]
[.If_And   | .If #1 #2 #3   | #+3]
[..If_And  | ..If #1 #2 #3  | #+3]
[...If_And | ...If #1 #2 #3 | #+3]

[Else_If_And    | Else    | If_And    #F>L]
[.Else_If_And   | .Else   | .If_And   #F>L]
[..Else_If_And  | ..Else  | ..If_And  #F>L]
[...Else_If_And | ...Else | ...If_And #F>L]




; * max string length = 10
; * OK if #3 is included in #1, jump to next 'If' otherwise
;
; ex:
;   If_str eax = 'glouglou'
;       ;...
;   Else
;       ;...
;   End_If

[.If_str | {&0: #3,0} | Mov esi &0 | Mov edi #1 | Mov ecx 10 | repe cmpsb
         On ecx = 0, jmp J1>  | On B$esi-1 <> 0, jmp J1> ]

[.Else_If_str| .Else | .If_str #1 #2 #3]


____________________________________________________________________________________________
EPP_All_Data:
[EPP_SourceEquation: B$ ? #256   EPP_SourceEquation_End: B$ ?]  ; last char is EOI
[EPP_DestOperand: B$? #64]     ; last char is Space

[EPP_SourceEquation_Backup: B$ ? #256 ]
[EPP_DestOperand_Backup: B$? #64]
[EPP_atofAddress: B$ ? #9] ; null-terminated
[EPP_ftoaAddress: B$ ? #9] ; null-terminated
[EPP_ImmediateResultString: B$ ? #32]

[EPP_InternalMeaninglessLabels: B$ ? #(9*10)]   ; 8 and 9 are reserved for Evaluation parser


[EPP_SearchedSingleCharacter: B$ ? #SEARCH_VARIABLE_NUMBER]
[EPP_SearchedStringAddress:   D$ ? #SEARCH_VARIABLE_NUMBER]
[EPP_ReplacementBuffer: B$ ? #128 ]

[EqualPreparser_DestInvalid: B$             'Destination operand is invalid.' 0
 EqualPreparser_SourceInvalid:              'Source expression is invalid.' 0
 EqualPreparser_UnexpectedError:            'Unexpected preparsing error.' 0
 EqualPreparser_InvalidIndex:               'Invalid index type.' 0
 EqualPreparser_InvalidAddress:             'Invalid address.' 0
 EqualPreparser_OperatorsInvalid:           'Two operators are following each other.' 0
 EqualPreparser_ParameterInvalid:           'Invalid integrated function parameter.' 0
 EqualPreparser_InvalidFPValue:             'Invalid floating point value.' 0
 EqualPreparser_OptimisationError:          'Error occurred while optimizing. Please report this error :)' 0
 EqualPreparser_InvalidFunction:            'No such function implemented.' 0
 EqualPreparser_InvalidStringDest_Type:     'A string pointer MUST be a DWord .' 0
 EqualPreparser_InvalidStringDest_Numeric:  'You cannot write at a numeric address.' 0
 EqualPreparser_InvalidStringinSource:      'Invalid string in source operand.'  0
 EqualPreparser_InvalidImmVal:              'Invalid immediate expression.'  0
 EqualPreparser_LocalStackOverflow:         'Local stack overflow !'  0

FPUInSourceExpression: B$ 0   UsedRegisters_Backup: 0   UsedRegisters: 0   NoOperation: 0  ParsingInsideArray: 0
DestinationArrayRegister: 0    UseCommonAddressingSyntax: &TRUE   StrNull: 0]

[EPP_ImmediateResult: T$ ?]

[EPP_CodeBegining: ?   EPP_Operand1: ?     EPP_Operand2: ?  EPP_WrittenCode:  ?  FPUStackNumber: ?
 DestinationType: ?  EPP_TextPointer: ?   EPP_LastStorageStatement: ?   EPP_LastStorageStatement_InMemory: ?]


 [INITIAL_STATE 07FFFFFFF
  FOUND_EQUATION_TEST 0FFFFFF
  SEARCH_VARIABLE_NUMBER  9

  EPP_EAX 00______1
  EPP_EBX 00_____10
  EPP_ECX 00____100
  EPP_EDX 00___1000
  EPP_ESI 00__10000
  EPP_EDI 00_100000

  EPP_NO_OPERATION        2
  CODE_NOT_FOUND          0_FFFF_FFFF
  EPP_CODE_BEGIN              0
  EPP_CODE_END                0
  EPP_SOURCE_BEGIN            1
  EPP_SOURCE_END              1

  SignedMulSign  '*'
  SignedDivSign  '/'

  pointSign      'o'
  concatSign     '&'

  ; +,- and * used for addressing in RosAsm syntax
  minusSign      'm'
  plusSign       'p'
  addressSign    'x'

  ; All data types used in this TITLE are below
  EPP_Types 0

        FunctionOperand      00__0000_1000__0000_0000
        NumericOperand       00__0100_0000__0000_0000
        RegisterOperand      00__0010_0000__0000_0000
        ConstantOperand      00__0001_0000__0000_0000

        MemoryOperand        00__0000_0001__0000_0000
                QWordValue   00_____________0000_1000
                DWordValue   00_____________0000_0100
                WordValue    00_____________0000_0010
                ByteValue    00_____________0000_0001

        FloatingPointOperand 00_____________1000_0000
                  TByteValue 00_____________0001_0000
                  RealValue  00_____________0000_1000
                  FloatValue 00_____________0000_0100





  EqualPreparser_IntegerOperand_ST0            00________1
  EqualPreparser_ST0_IntegerOperand            00_______10
  EqualPreparser_FloatingPointOperand_ST0      00______100
  EqualPreparser_ST0_FloatingPointOperand      00_____1000
  EqualPreparser_ST1_ST0                       00____10000
  EqualPreparser_ST0_ST1                       00___100000
  EqualPreparser_ST0_ConstantOperand           00__1000000
  EqualPreparser_ConstantOperand_ST0           00_10000000]
____________________________________________________________________________________________




____________________________________________________________________________________________
____________________________________________________________________________________________
;DEBUG

[TempSourceBuffer: ? #1024]

[ShowSourceTitle: 'SOURCE ' ]
[ShowSourceTitle_ID: 0 0 0 ]                    ; [<ShowSourceTitle_ID: 0 0 0 ]  disables the breakpoints !!!!!
[FirstEqualLine: &TRUE   LastEqualLine: 0]

; Call ShowSource edi 1    ; show from edi and diplay 1 in the title
; Call ShowSource 0 7      ; show the whole written code and diplay 7 in the title
; Call ShowSource ebx 0    ; show the line where ebx points on
Proc ShowSource:
    Arguments @ShowFrom @Title

    pushad

        Mov ebx, D@Title | Mov edi ShowSourceTitle_ID | inc edi
        std
            Mov ecx, 2
L1:         Mov al bl | and al 0F | On al >= 0A, add al 7
            add al, '0' | stosb | shr ebx, 4 | loop L1
        cld

        If D@ShowFrom = 0
            Mov esi D$LastEqualLine
            ;While B$esi <> EOI | inc esi | End_While | inc esi  ; shows instruction after the current one
            ;While B$esi <> EOI | dec esi | End_While | dec esi  ; shows instruction before the current one
            ;While B$esi <> EOI | dec esi | End_While | inc esi
            Mov edx &TRUE ; only one line is displayed
        Else_If D@Title = 0
            ;While B$esi <> EOI | inc esi | End_While | inc esi  ; shows instruction after the current one
            ;While B$esi <> EOI | dec esi | End_While | dec esi  ; shows instruction before the current one
            Mov esi D@ShowFrom
            While B$esi <> EOI | dec esi | End_While | inc esi
            Mov edx &FALSE
        Else
            ; shows from pointer
            Mov esi D@ShowFrom
            Mov edx &TRUE ; only one line is displayed
        End_If

        ;mov ebx D$LastEqualLine | While B$ebx <> EOI | inc ebx | End_While
        ; ebx points on the end of the source currently written

        Mov ecx 0
        Mov edi TempSourceBuffer

        ; Each charater is replaced by its source equivalent so as to being diplayed.
        ; If a character is neither alphanumeric nor a RosAsm operator, it is replaced by '~' so as to be easily locatable.

L0:     lodsb
        .If al = TextSign
            Mov al '"'
        .Else_If al = pointSign
             Mov al '.'
        .Else_If al = minusSign
            Mov al '-'
        .Else_If al = plusSign
            Mov al '+'
        .Else_If al = SignedDivSign
            Mov al '/'
        .Else_If al = SignedMulSign
            Mov al '*'
        .Else_If al = numSign
            Mov al '#'
        .Else_If al = CommaSign
            Mov al ','
        .Else_If al = OpenVirtual
            Mov al '{'
        .Else_If al = CloseVirtual
            Mov al '}'
        .Else_If al = Openbracket
            Mov al '['
        .Else_If al = Closebracket
            Mov al ']'
        .Else_If al = memMarker
            Mov al '$'
        .Else_If al = colonSign
            Mov al ':'
        .Else_If al = openSign
            Mov al '('
        .Else_If al = closeSign
            Mov al ')'
        .Else_If al = addSign
            Mov al '+'
        .Else_If al = subSign
            Mov al '-'
        .Else_If al = mulSign
            Mov al '*'
        .Else_If al = divSign
            Mov al '/'
        .Else_If al = expSign
            Mov al '^'
        .Else_If al = '{'
            Mov al '{'
        .Else_If al = '}'
            Mov al '}'
        .Else_If al = '.'
            Mov al '.'
        .Else_If al = Space
            Mov al '_'
        .Else_If al = 0
            Mov al '@'
        .Else_If al = EOI
            Mov al '|'
            On edx = &TRUE, jmp L9>
            On esi >= ebx, jmp L9>
            Mov ax 0A0D | stosw
            Mov al 13 | stosb
            Mov al 10
        .Else_If al = meEOI
            Mov al 13 | stosb
            Mov al 10
        .Else_If al < 48
            Mov al '~'
        .Else_If al > 90
            Mov al '~'
        .End_If

        stosb
        inc ecx

        jmp L0<<

L9:     Mov B$edi 0

        ;Call 'USER32.DialogBoxParamA' D$hInstance 23000  &NULL EqualDebugProcProc  &NULL
        Call 'USER32.MessageBoxA' D$H.MainWindow, TempSourceBuffer, ShowSourceTitle, &MB_ICONINFORMATION
    popad


EndP
____________________________________________________________________________________________
Proc EqualDebugProcProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    If D@msg = &WM_INITDIALOG
        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$wc_hIcon
        Mov eax &TRUE
        Call 'User32.SetWindowTextA' D@hwnd ShowSourceTitle
        Call 'User32.SetDlgItemTextA' D@hwnd 101 TempSourceBuffer

    Else_If D@msg = &WM_CLOSE
        Call 'User32.EndDialog' D@hwnd 0


    Else
        popad | Mov eax &FALSE | jmp L9>

    End_If

    popad | Mov eax &TRUE

L9: EndP

