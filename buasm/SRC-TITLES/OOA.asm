TITLE OOA             ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;;
 OOA (Object Oriented Assembly) Parser.
 
 Author name: Betov

 eMail: < betov@free.fr >

 Home Page: ( http://betov.free.fr/RosAsm.html >
 
____________________________________________________________________________________________
____________________________________________________________________________________________

Plan for RosAsm Object Oriented Programing. (obsolete and delayed...)
____________________________________________________________________________________________
____________________________________________________________________________________________

Introduction.

I use the OOA formulation because we are talking about one Assembly OOP implementation
(not of OOP generally speacking).

The center of RosAsm OOA is the OOA Memory Manager. This is a robust and fast
Memory Manager, that provides your OOA programing with a 'vTable' holding your
Objects representations in Memory. Before any OOA programing, you have to paste
this Manager (from the [Clip] feature), in your Source. This technical choice tends 
to hide as few things as possible, and allows you to add, modify the default OOA 
Memory Manager Routines, if you want to.

____________________________________________________________________________________

A RosAsm OOA Object is a contiguous range of Memory Chunks, organised under a structured
form, holding a Call-Table (Pointers to Procedures) and Objects "private" Data.

"private", here does of course not mean that you can not access these Data by
hand when you want to. We are in Assembly... This simply means that these Data
are supposed to be accessed only by the Call-Table Procedures. It's on you to
apply this rule (or not...). Same for the polymorphism implementation. It's on 
you to not 'overload' your Call-Table' from your 'normal' Code, and to do it from
inside your OOA Procedures, instead.

Objects are Dynamically created, at run time. That is, they live only in Memory
(in vTable), at Run-Time. Their specifications are defined by the CLASS(es)
Declaration(s).
____________________________________________________________________________________________
____________________________________________________________________________________________

CLASSes are abstract (without existance in the dead File) OOA Structures giving 
informations to the RosAsm CLASS Parser. You may consider them a kind of C abstract
Structures.

CLASS Declaration:

>[CLASS Integer         ; 'Integer' will be the Instances Creator.
>  {PreviousClass}      ; {...} is (are) optional Inheritance from another CLASS.
>   SetValue, Int       ; Call-Table, (Procedures pointers with implicit Parameter(s)).
>   AddValue, Int       ; 
>   ShowValue, Int      ; 
>  {Int: D$ 0}]         ; Whatever Data in usual form, with default intitialisations.

* {Inheritance(s)} are optional and can be multiple and/or nested.

* The CLASS Call-Table is Code Procedures Pointers List defining also the default
Parameters to be silently transmitted to the Procedures. These default Parameters
can only be pointers to the same Object elements (Data Pointers, usually).

* {Data: ...} are in the same syntax as ordinary RosAsm Data Declarations, but enclosed
inside {...}, instead of [...], just the same as when Declaring Data by Macros in the
usual RosAsm Convention.


When encounted by the CLASS Parser, the 'Integer' CLASS is stored like a particular
kind of Macro, generating other Macros, and allowing all downward formulations.

You can define the default Initialisations Values of the Data when declaring the CLASS,
and, of course, with one of the Methods of the Object. Example, for overwriting some
Data Value, after the Object Instance creation:

> Integer i        ; D$i+12 = 0  (the '0' declared in the CLASS).
> i.setValue 1     ; D$i+12 = 1
____________________________________________________________________________________________

The Inheritance Declaration is as simple as can be:

[ClASS X {Integer}]

[ClASS Y {Integer}]

[ClASS LeftTop
 {X}
 {Y}]

[ClASS RightBottom
 {X}
 {Y}]

[ClASS RectPos
 {LeftTop}
 {RightBottom}]
 
________________________________________

Notice carefully the difference between:
            
            "[ClASS X {Integer}]"       ; Case 1.

            and:

            "[ClASS X               
              {Integer}]"               ; Case 2
              
            Case 1 is what OOP theory calls an "ISA" relationship, and Case 1 is an "HASA"
            relationship. (Read "Is a" // "Has a").
            
            Macros Names built by case 1 do not require pasting the original name, whereas
            Case 2 does:
            
            * In case 1, for 'SetValue', you say:  > X.SetValue 32
            
            * In case 2, for 'SetValue', you say:  > X.Integer.SetValue 32
            
            To make it short, in Case 1, 'X' is an 'Alias' of Interger. So, in the upper
            example, you have the shorter form of:
            
            > RectPos.RightBottom.X.SetValue 32
            
            ... and not:
            
            > RectPos.RightBottom.X.Integer.SetValue 32
            
            Notice also that Case 1 can only hold one single inheritance, whereas 
            Case 2 may hold as many as wished. You may consider Case 1 as a simple
            Compile time duplication (a substitute) of an existing CLASS, rather 
            than a real new CLASS creation.
              
            
When encounting {PreviousClass}, the CLASS Parser simply replaces this by the real 
"[CLASS PreviousClass...] Declaration. So... :

> RectPos MyRectanglePos
> ...
> MyRectanglePos.LeftTop.X.SetValue 32
____________________________________________________________________________________________
____________________________________________________________________________________________

The default Parameters purpose is not only to save you from having to write the 
default Parameters expected by a Procedure of the Call-Table. It is also the way
for making your Object little closed boxes with an automatic access of the Object
Procedures to the Data belonging to the object:

>Proc SetValue:
>    arguments @NamePtr, @Value
>    
>        Mov esi D@NamePtr, eax D@Value, D$esi eax
>EndP

> i.SetValue 1    ; <<<<<<<<<<

; (Instead of "Call D$i.SetValue D$i.Int, 1", or something like this).

____________________________________________________________________________________________
____________________________________________________________________________________________

So, first, you declare a Data Table (or Variable(s)) where to store your Objects
Handles:

[i: D$ ?] ; User-side Objects Variable (= Handle = Pointer to vTable).

An Object Handle is nothing but a simple Pointer to the vTable created at run time, 
in Memory, when the Objects creations statements are executed:

> Integer i ; Produces the Creation and the storage in vTable of an image of the
            ; 'Integer' declared CLASS. The Return Parameter (the Pointer to
            ; Object "i", is filled with the real Pointer to the Object image set
            ; in the vTable. Eax, of course, also holds this same Value.

If "Integer i" was not unfolded by the CLASS Parser, but by yourself, 'by Hand',
it would look like this:

> Call GetvTable i, 16                      ; Parameters = ObjectHandleRoom, Size.
> Push eax, ebx
>   Mov D$eax SetValue                      ; store METHOD
>   lea ebx D$eax+24 | Mov D$eax+4 ebx      ; 'Int' Pointer default Parameter
>   Mov D$eax+8 AddValue                    ; store METHOD
>   lea ebx D$eax+24 | Mov D$eax+12 ebx     ; 'Int' Pointer default Parameter
>   Mov D$eax+16 ShowValue                  ; store METHOD
>   lea ebx D$eax+24 | Mov D$eax+20 ebx     ; 'Int' Pointer default Parameter
>   Mov D$eax+24 0                          ; Store Data initial Value(s).
> Pop ebx, eax

"GetvTable" (like "GetMultiplevTable") is a Main Procedure of the OOA Memory Manager. 

Not only the CLASS Parser replaces your "Integer i" statement by the previous Code 
Chunk, but, at the same time, it creates a set of pseudo-Macros, in order to make
your OOP programing friendly. Those Macros could look like this:

> [i.SetValue | Mov eax D$i | Call D$eax+0 D$eax+4, #1]
> [i.AddValue | Mov eax D$i | Call D$eax+8 D$eax+12, #1]
> [i.ShowValue | Mov eax D$i | Call D$eax+16 D$eax+20, #1]

You may consider them usual RosAsm syntax Macros, written for you for free by the
CLASS Parser, and immidiately unfolded by the ClASS Parser *before* all other
downward computations. If an implicit Parameter is to be given in the Call to a
"Call-Table-Procedure", you just have to declare it in the CLASS, after the Procedure
Name. The upper Macros examples cover this with "D$eax+4" and friends, what will 
effectively be unfolded under the usual form of a "Push D$i+12".

____________________________________________________________________________________

Usage:

> [ClASS Integer
>   SetValue, Int
>   AddValue, Int
>   ShowValue, Int
>  {Int: D$ 0}]
>
> [i: D$ ?]
>
> TestOop:
>    Integer i     ; Object Creation (return: eax = D$i = Pointer to Integer vTable image)
>  ; ...
>    i.SetValue 01 ; D$i+12 = 1
>    i.AddValue 01 ; D$i+12 = 2
>    i.ShowValue   ; "Says 2"
>  ; ...
>    Destroy i     ; Aborts if D$i = 0 // Free the i Object vTable Memory / Set D$i to zero.
> ret


Of course, the example supposes you have 3 Procedures (SetValue, AddValue and ShowValue)
previously written the usual way, the 2 first ones expecting 2 parameters, the last one,
one Parameter.

There is absolutely nothing particular with OOA Procedures. There are nothing but usual
Procedure called in an unusual way, that is, through a Call-Table. Nothing less, 
nothing more:

>Proc SetValue:
>    arguments @NamePtr, @Value
>    
>        Mov esi D@NamePtr, eax D@Value, D$esi eax
>EndP

>Proc AddValue:
>    arguments @NamePtr, @Value
>    
>        Mov esi D@NamePtr, eax D@Value | add D$esi eax
>EndP

>Proc ShowValue:
>    arguments @NamePtr
>    
>        HexPrint D@NamePtr
>EndP

____________________________________________________________________________________

You can copy an existing Object into a new created one:

> Integer m From i

This formulation allows you to create, for example, a temporary Copy of an Object, 
designed for a temporary Object Data modifications.

Note that this copy does nothing but duplicating the Object: 

If, for example, in "InstanceOne", a Function has been run, in order to allocate some
memory, and to save this memory Pointer into its object Data, the "InstanceTwo" Member
holding this allocated memory Pointer *will* point to the same Memory as the one in
"InstanceOne". If you plan to have different Memories allocated for each Instance,
Call the new created one Function for reinitializing the Memory Allocation and its
object Data Pointer. If you plan to have the same dynamic Data (Pointers) in several
Instances, this is the helpfull way. Of course, if your plan is to have two copies
of the Pointed Data, you have to implement a Procedure performing the allocated Data 
Copy from D$PointerOne to D$Pointertwo.

This way for Copying an Object may also be usefull in order to reduce the Code size.
As you may have noticed, each Object Creation adds some real Code in your Application.
This Code Bloat may be reduced by the usage of the "From" Keyword. For example, after
your "Interger i" Statement, instead of:

> Integer j                                 ; what will, again, add to your Code:

> Call GetvTable j, 16                      ; Parameters = ObjectHandleRoom, Size.
> Push eax, ebx
>   Mov D$eax SetValue                      ; store METHOD
>   lea ebx D$eax+24 | Mov D$eax+4 ebx      ; 'Int' Pointer default Parameter
>   Mov D$eax+8 AddValue                    ; store METHOD
>   lea ebx D$eax+24 | Mov D$eax+12 ebx     ; 'Int' Pointer default Parameter
>   Mov D$eax+16 ShowValue                  ; store METHOD
>   lea ebx D$eax+24 | Mov D$eax+20 ebx     ; 'Int' Pointer default Parameter
>   Mov D$eax+24 0                          ; Store Data initial Value(s).
> Pop ebx, eax

You can do:

> Integer j From i                   

; That will be unfolded as one single "Call" instead of Bloatware:

> Call GetvTableFrom D$i, j, 16

As this formulation may save much of Code Bloat, it is silently applied by the CLASS
Parser, when encounting Multiple Declarations:

> Integer i, j, k  ; Will be unfolded by the CLASS parser as:
>
> ; First shot:
> Push eax, ebx
>   Mov D$eax SetValue                      ; store METHOD
>   lea ebx D$eax+24 | Mov D$eax+4 ebx      ; 'Int' Pointer default Parameter
>   Mov D$eax+8 AddValue                    ; store METHOD
>   lea ebx D$eax+24 | Mov D$eax+12 ebx     ; 'Int' Pointer default Parameter
>   Mov D$eax+16 ShowValue                  ; store METHOD
>   lea ebx D$eax+24 | Mov D$eax+20 ebx     ; 'Int' Pointer default Parameter
>   Mov D$eax+24 0                          ; Store Data initial Value(s).
> Pop ebx, eax

>
> ; Next shots:
> Call GetvTableFrom D$i, j, 16
> Call GetvTableFrom D$i, k, 16

____________________________________________________________________________________

        (> [i.SetValue | Mov eax D$i | Call D$eax+0 D$eax+12, #1])

Now, let us suppose you want a Table of 200 Integers Objects.

> [IntegersHandleTable: D$ ? # 200]
>
> Integer IntegersHandleTable, # 200    ; <<< "# 200" = "Does this 200 times"

When encounting this '#200' Parameter, the CLASS Parser, instead of creating the
usual macros (IntegersHandleTable.SetValue, IntegersHandleTable.AddValue,
IntegersHandleTable.ShowValue) create the Macros in the form of:
'esi.SetValue', 'esi.AddValue', 'esi.ShowValue'. So, limitation:
In order to not multiply no end the created Macros, only esi can be use as a pointer
to OOA handle Tables.

> [IntegersHandleTable: D$ ? # 200]
> ...
> Integer IntegersHandleTable, # 200    ; <<< "# 200" = "Does this 200 times"
> ...
> lea esi D$IntegershandleTable+(3*4)
> esi.SetValue 012345

Will be unfolded as:

> [IntegersHandleTable: D$ ? # 200]
> ...
> Call GetvTable IntegersHandleTable, 16
> Push eax, ebx
>   Mov D$eax SetValue                      ; store METHOD
>   lea ebx D$eax+24 | Mov D$eax+4 ebx      ; 'Int' Pointer default Parameter
>   Mov D$eax+8 AddValue                    ; store METHOD
>   lea ebx D$eax+24 | Mov D$eax+12 ebx     ; 'Int' Pointer default Parameter
>   Mov D$eax+16 ShowValue                  ; store METHOD
>   lea ebx D$eax+24 | Mov D$eax+20 ebx     ; 'Int' Pointer default Parameter
>   Mov D$eax+24 0                          ; Store Data initial Value(s).
> Pop ebx, eax
> 
> Call GetMultiplevTableFrom D$IntegersHandleTable, IntegersHandleTable, 16, 199
> ...
> lea esi D$IntegershandleTable+(3*4)
> Mov eax D$esi
> Call D$eax+0 D$eax+4, 012345

____________________________________________________________________________________________
____________________________________________________________________________________________

Polymorphism formulations look like this:

> i.AddValue = SubValue

... of course to be stated *inside* one of the Call-Table Routines. Example:

>Proc SetValue:
>    arguments @NamePtr, @Value
>    
>        Mov esi D@NamePtr, eax D@Value, D$esi eax
>
>        If eax > 0FFF
>            i.addValue = SubValue
>        Else_If eax < 0FF
>            i.addValue = AddValue
>        End_If
>EndP


Stating "i.addValue = SubValue" in your "normal" Code, outside the OOA Procedures,
would breack the OOP style programation, just like directely modifying *by hand*
some "private" Data of an Object. To keep it OOP-like *you* have to follow the OOP
way for doing these things. RosAsm OOA organisation does not at all control if you
are following the OOP programing rules or not. It's on your own choice.

Notice also, that, in the upper example,

> i.SubValue 3

is a wrong statement, as long as "SubValue" appears nowhere inside any CLASS
Declaration. The right statement is yet:

> i.AddValue 3

This *is* one aspect of "Polymorphism": You don't have to know and to care about
how the Object "little independant Robot" does the things once you have defined
this inside the Call-Table Routines. At the time your write your 'normal' Source,
you do not have any more to know if the robot will "add" or "sub" your Value, or
anything Else. The robot itself chooses if he wants to add or sub. So, the applyed
Names are to be carefully choosen. In the ridiculous upper example, of course,
you should not say "SubValue" / "AddValue", but, "Modify". A simple way for doing
this is to give *two* Names to your default Procedure:

> Proc AddValue: Modify:             ; <<<<<< 2 Names !!!
>     arguments @NamePtr, @Value
>    
>         Mov esi D@NamePtr, eax D@Value | add D$esi eax
> EndP

And to provide the general meaning default Name inside the CLASS Declaration:

> [ClASS Integer
>   SetValue, Int
>   Modify, Int      ; <<<< Default AddValue represented by the General Name.
>   ShowValue, Int
>  {Int: D$ 0}]
> ...
> Integer i
> ...
> i.Modify 1

... and go on, without caring if it is an addition or a substraction.

May be you do not see the point of this, at all. So imagine you have a ball
moving on the screen. You want this ball to grow and decrease repeatitly without
taking care of how the Robot "Ball" does this, in your 'normal' Code. Then, it
would be, simply:

> If D@msg = &WM_PAINT
>     ....
>     Ball.Size.Modify 1
>     ...

Instead of (without Polymorphism):

> If D@msg = &WM_PAINT
>     ....
>     Ball.GetSize
>
>     If B$TheBallIsGrowingUpFlag = &TRUE
>         If eax >= MAX
>             Ball.Size.SubValue 1
>             Mov B$TheBallIsGrowingUpFlag &FALSE
>         Else
>             Ball.Size.AddValue 1
>         End_If
>
>     Else
>         If eax =< MIN
>             Ball.Size.AddValue 1
>             Mov B$TheBallIsGrowingUpFlag &TRUE
>         Else
>             Ball.Size.SubValue 1
>         End_If
>
>     End_If
____________________________________________________________________________________________
____________________________________________________________________________________________
;;

[OOAinside: D$ ?
 PreviousSeparator: D$ ?]

[UnfoldedCLASS: D$ ?
 TempoUnfoldedCLASS: D$ ?
 ClassUnfoldingOver: D$ ?
 ClassListPtr: D$ ?]

[OneCLASSname: D$ ? # 80]

[OneClassNamePtr: D$ ?
 OriginalClassDataPointer: D$ ?]

;;
We Search for [CLASS ...., and store all occurence encounted in the Source into
MacroList and MacroData (the same way as for Macros). At the same time we arase
the Declarations in the Source.
;;

ClassParser:
    Mov esi D$CodeSourceA, edi D$CodeSourceB | Mov ecx esi | add ecx D$StripLen
    Mov B$ErrorLevel 11  ; 'error11'

    Move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4
;;
[CLASS Integer   30 37 31 0D  >>> 0 7 1 ]
 {NestedClass}
 addvalue Data
 subValue Data
 {Data: 0}]      0a 0d 0a04c >>> ':' ] 
;;
    .While esi < ecx
        If B$esi = TextSign
            movsb | While B$esi <> TextSign | movsb | End_While

        Else_If D$esi = 'CLAS'
            cmp B$esi-1 OpenBracket | jne L1>
            cmp B$esi+4 'S' | jne L1>
            cmp B$esi+5 Space | jne L1>
                Mov al B$esi-2, B$PreviousSeparator al
              ; Suppress the last written '[':
                dec edi

                Call StoreOneCLASS | Mov B$OOAinside &TRUE

        End_If

L1:     movsb
    .End_While
;;
If Some CLASS Declaration has been found, we do all the job, CLASS by CLASS:
;;
    .If B$OOAinside = &TRUE
        sub edi D$CodeSourceB | Mov D$StripLen edi
        Exchange D$CodeSourceA D$CodeSourceB

        Call VirtualAlloc UnfoldedCLASS,
                          10_000

        Call VirtualAlloc TempoUnfoldedCLASS,
                          10_000

        Move D$ClassListPtr D$MacroList | add D$ClassListPtr 5

L0:     Mov esi D$ClassListPtr, edi OneCLASSname

        If B$esi > 0
            While B$esi > EOI | movsb | End_While | Mov B$edi 0     ; Name
            Mov D$OneClassNamePtr edi
            inc esi                                                 ; EOI
            Mov D$OriginalClassDataPointer esi
            lodsd                                                   ; Pointer to 'MacroData'
            Mov ecx D$esi                                           ; Size
            add esi 5                                               ;( + EOI)
            Mov D$ClassListPtr esi                                  ; Ready for Next Record.

            Mov esi eax, edi D$UnfoldedCLASS | rep movsb | Mov B$edi 0

            Call UnfoldIncludedCLASSES | Call UnfoldMethods
            Call UnfoldPrivateData | Call UnfoldOneOOA | jmp L0<    ; Loop each CLASS.
        End_If

        Call VirtualFree UnfoldedCLASS

        Call VirtualFree TempoUnfoldedCLASS

      ; Restore the Macros Pointers for Normal Macros:
        Move D$MacroListPtr D$MacroList | add D$MacroListPtr 5
        Move D$MacroDataPtr D$MacroData
    .End_If

ret


StoreOneCLASS:
    Push edi, ecx
        Mov edi D$MacroListPtr
      ; esi > 'CLASS Name'
        add esi 6

        On B$esi < '.', error D$MissingCLASSnamePtr
      ; Store the Name into MacroList:
        While B$esi > LowSigns | movsb | End_While
        Mov B$edi EOI | inc edi | inc esi

      ; Write Pointer to Body Data:
        Mov eax D$MacroDataPtr | stosd | Mov ebx edi
        Mov edi eax
; [CLASS X = Integer]
      ; Now write the Data Body for the instruction > Name i
;;
> [ClASS Integer
>   SetValue, Int
>   AddValue, Int
>   ShowValue, Int
>  {Int: D$ 0}]
>
> [i: D$ ?]
>
> TestOop:
>    Integer i     ; Object Creation (return: eax = D$i = Pointer to Integer vTable image)
>  ; ...
>    i.SetValue 01 ; D$i+12 = 1
>    i.AddValue 01 ; D$i+12 = 2
>    i.ShowValue   ; "Says 2"
>  ; ...
>    Destroy i     ; Aborts id D$i = 0 // Free the i Object vTable Memory / Set D$i to zero.
> ret

If "Integer i" was not unfolded by the CLASS Parser, but by yourself, 'by Hand',
it would look like this:

> Call GetvTable i, 16              ; Parameters = ObjectHandleRoom, Size.
> Mov D$eax SetValue | add eax 4    ; store METHOD(s).
> Mov D$eax AddValue | add eax 4    ; ...
> Mov D$eax ShowValue | add eax 4   ; ...
> Mov D$eax 0                       ; Store Data initial Value(s).
;;
        Mov edx esi, ecx 0

        While B$esi <> CloseBracket
            lodsb
            If al = EOI
                Mov al meEOI
            Else_If al = '{' | inc ecx
                Mov al OpenVirtual
            Else_If al = '}' | dec ecx
                Mov al CloseVirtual
            End_If
            stosb
        End_While
        On ecx <> 0, error D$UnPairedNestedBracketsPtr

        Mov D$MacroDataPtr edi

      ; Write size into MacroList:
        Mov ecx esi | sub ecx edx
        Mov D$ebx ecx | add ebx 4 | Mov B$ebx EOI | inc ebx | Mov D$MacroListPtr ebx

      ; Adjust Source Pointer:
        inc esi | Mov al B$esi
        On al = B$PreviousSeparator, inc esi

    Pop ecx, edi
ret


;;
If a CLASS Declaration body (in MacroData) includes a {Inheritance}, we unfold it now.
This is to say, if this is stored:

[CLASS Integer         ; 'Integer' will the Instances Creator.
  ;{PreviousClass}      ; {...} is (are) Inheritance from another CLASS.
   SetValue, Int       ; Call-Table, (Procedures pointers with implicit Parameter(s).
   AddValue, Int       ; 
   ShowValue, Int      ; 
  {Int: D$ 0}]

[ClASS X
 {Integer}]
 
... We turn the this last {Integer] into:

 X.SetValue, X.Int       ; Call-Table, (Procedures pointers with implicit Parameter(s).
 X.AddValue, X.Int       ; 
 X.ShowValue, X.Int      ; 
{X.Int: D$ 0}]
;;


UnfoldIncludedCLASSES:

L0: Mov B$ClassUnfoldingOver &TRUE

    Mov esi D$UnfoldedCLASS, edi D$TempoUnfoldedCLASS

  ; Case ot [CLASS X ISA Integer]:
  ;  If D$esi = 03415349
  ;      Call ClassSubstitute    ; 'ISA ' >>> 03415349
  ;      jmp L0<
  ;  End_If

L1: .While B$esi > 0
        lodsb

        ..If al = OpenVirtual
            Mov ebx esi | While B$ebx > LowSigns | inc ebx | End_While

            .If B$ebx = CloseVirtual
              ; Cases of {InHeritance}
                Push ebx
                    Call AppendThisClassName esi
                    Call CopyThisCLASS | Mov B$ClassUnfoldingOver &FALSE
                    If B$NestedClassInside = &TRUE
                        While B$esi-1 > 0 | movsb | End_While
                        Exchange D$UnfoldedCLASS D$TempoUnfoldedCLASS
                        Pop ebx | jmp L0<<
                    End_If
                    Call StripThisClassName
                Pop esi
                inc esi

            .Else
              ; If it is not CloseVirtual, it may be a ColonSign >>> Data (the UnPaired '{}'
              ; checked with Data parsing):
L2:             stosb
            .End_If

        ..Else
          ; If not OpenVirtual >>> Call-Table.
            stosb
        ..End_If
    .End_While

    Mov B$edi 0 | Exchange D$UnfoldedCLASS D$TempoUnfoldedCLASS

    If B$ClassUnfoldingOver = &FALSE
      ;  Call UpdateMacroData |
      jmp L0<<
    End_If
ret

; In Case ot [CLASS NewClassName = ClassName], we do nothing but substitute the MacroList
; Pointer and Size with the ones of the "Mother" CLASS. This is to say that Two entries
; will exist (2 different Names), for the same CLASS Data. The "= ClassName", in MacroData
; is simply lost:

ClassSubstitute:
  ; D$OriginalClassDataPointer yet points to this Class Pointer to macroList.
L0: add esi 4
    Call SearchMacroListClassName
  ; >>> eax = Pointer to MacroData // ecx = Size.
;    If D$eax = 03415349   ; 'ISA ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;        Mov esi eax | jmp L0<
;    Else
;        Mov ebx D$OriginalClassDataPointer, D$ebx eax, D$ebx+4 ecx
;        Mov esi eax, edi D$UnfoldedCLASS | rep movsb | Mov B$edi 0
;    End_If
ret


UpdateMacroData:
    Mov esi D$UnfoldedCLASS, edi D$MacroDataPtr, ecx 0
  ; New Pointer to MacroData, in MacroList:
    Mov ebx D$OriginalClassDataPointer,  D$ebx edi | add ebx 4
  ; Copy the new Body at end of MacroData:
    While B$esi <> 0 | movsb | inc ecx | End_While
  ; New Body Length in MacroList and adjust the Writing Pointer:
    Mov D$ebx ecx, D$MacroDataPtr edi
ret


Proc AppendThisClassName:
    Argument @Source
    Uses esi, edi

        Mov esi D@Source, edi D$OneClassNamePtr, B$edi '.' | inc edi
        While B$esi > LowSigns | movsb | End_While | Mov B$edi 0
        Mov D$OneClassNamePtr edi
EndP


StripThisClassName:
  ;  Mov eax D$OneClassNamePtr
  ;  While B$eax <> '.' | dec eax | End_While
  ;  Mov B$eax 0, D$OneClassNamePtr eax
    Mov eax OneCLASSname
    While B$eax <> '.' | inc eax | End_While
    Mov B$eax 0, D$OneClassNamePtr eax
ret


SearchMacroListClassName:
    Push edi, esi
        Mov edi D$MacroList | add edi 5

L0:     Mov edx edi | lodsb
        While al = B$edi
            On al < LowSigns, jmp L1>
            lodsb | inc edi
        End_While

        ..If al < LowSigns
L1:         .If B$edi < Separators
              ; Found:
                inc edi
                Mov eax D$edi           ; Pointer to MacroData.
                Mov ecx D$edi+4   ; Size.
                jmp L9>

            .Else
L2:             Pop esi | Push esi

                While B$edi > Separators | inc edi | End_While | add edi 10
                If B$edi = 0
                    Mov ebx esi | While B$ebx <> CloseVirtual | inc ebx | End_While
                    Mov B$ebx 0 | Error D$NoParentClassPtr
                Else
                    jmp L0<
                End_If
            .End_If
        ..Else
            jmp L2<
        ..End_If

L9: Pop esi, edi
ret                 ; eax = Pointer to MacroData // ecx = Size.


[NestedClassInside: D$ ?]

CopyThisCLASS:
    Call SearchMacroListClassName
    Mov B$NestedClassInside &FALSE
  ; Copy Parent CLASS Body, and add the actual CLASS Name before all Components, but
  ; nested Inheritances:
    Push esi
        Mov esi eax

L0:     ..If B$esi = OpenVirtual

            Do
                .If B$esi > LowSigns
                    Mov ebx esi | While B$ebx > LowSigns | inc ebx | End_While
                  ; Cases of Data Declaration: Add the Actual Class Name:
                    If B$ebx = ColonSign
                        Call CopyActualClassName

                        While B$esi > ColonSign
                            movsb | dec ecx | jz L9>
                        End_While

                    Else
                        Mov B$NestedClassInside &TRUE

                    End_If
                .End_If

                movsb | dec ecx | jz L9>
            Loop_Until B$esi = CloseVirtual

            movsb | dec ecx | jz L9>
            jmp L0<

        ..Else
          ; In case the first written element start with a Method Char:
            If B$esi < LowSigns
                movsb | dec ecx | jz L9>
                jmp L0<

            Else
                Call CopyActualClassName

                While B$esi > Space
                    movsb | dec ecx | jz L9>
                End_While
                movsb | dec ecx | jz L9>
                jmp L0<<

            End_If

        ..End_If

L9: Pop esi
ret

CopyActualClassName:
    Push esi
        Mov esi OneCLASSname
        While B$esi > 0 | movsb | End_While | Mov al '.' | stosb
    Pop esi
ret


UnfoldMethods:

ret


UnfoldPrivateData:

ret


UnfoldOneOOA:

ret
____________________________________________________________________________________________
; EOT
