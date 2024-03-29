TITLE MACROS          ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;;

    English:

    To understand what these Macros do for you you can click on them and choose UNFOLD.
    You will know what kind of code they generate. Macros are useful in assembly programming
    because they bring long-term READABILITY.

    All the Macros you plan to use should be here in this TITLE. Here are some important
    guidelines you should know when implementing Macros:

    1. Macros shouldn't use registers internally for their functionment.
    2. Macros shoudn't use global variables or global labels
    3. Don't encapsulate easy APIs.

    Fran�ais:

    Pour mieux (bien) comprendre ce que ces Macros 'g�n�rent' nous vous invitons � un double
    clic gauche sur leurs �vocations dans le code. Choisissez l'option "Unfold" dans le menu
    pop: Vous verrez ainsi que RosAsm ne substitue pas de code � votre insue mais que c'est
    VOUS qui construisez vos substitutions -> Pour gagner en lisibilis�, en souplesse et en
    facilit� de maintenance de votre code. Plus il sera lisible et facilement compr�hensible,
    plus vous pourrez y retourner facilement apr�s plusieurs mois ou permettre � d'autres de
    participer sa la maintenance...

    Toutes les Macros utilis�es dans RosASm doivent �tre d�clar�es ici et, dans la mesure du
    possible, regroup�es par cat�gories. N'impl�mentez pas de nouvelles Macros utilisant en
    interne des registres, des variables globales ou des Label globaux (c'est une source de
    conflits).
    !!! Par principe, ne multipliez pas la cr�ation de nouvelles Macros -> Evitez, par-dessus
    toutes choses, les diverses formes d'encapsulations d'API: Cela n'apporte rien au niveau
    de la lisibilit� (au contraire) et provoque de nombreuses sources d'erreurs:
    Soignez les commentaires et utilisez des Proc/routines communes
    ________________________________________________________________

    ; List of Local labels to use and that Macros use:

    ; Labels locaux utilis�e au sein des Macros:
      (Liste � compl�ter en fonction des nouvelles impl�mentations)
    ________________________________________________________________

    P9 -> [ExitP]
    P7 -> [On]

    M0 ~ M9 et N0 ~N9 [If][Else_If][Else][End_If]

    O0 ~ O9 [Repeat][Until] [While][End_While]

    Jumps: S0 ~ S9

    Loops: L1 ~ L9 and L0 unique Loop or without link to other loops 
           L1 ~ L9 et L0 Boucle unique ou sans imbrication avec d'autres boucles
    _________________________________________________________________

    ; Internal variables of the Macro parser used inside the Macros:
      (List to complete when new impl�mentations are made)

    ; Variables interne du Macro parser utilis�e au sein des Macros:
      (Liste � compl�ter en fonction des nouvelles impl�mentations)
    _________________________________________________________________

    &1 -> Size of arguments / Taille des arguments

    &2 -> Size of local variables and structures / Taille des variables locales et des structure

    &3 -> String of register or variable saved / Cha�ne de registre ou variables pr�serv�es

    &4 -> Structure name / Nom de la structure

    &5 -> Structure offset counter / Compteur d'offsets de structure

    &6 -> Constructor of strings for debugg.dll macros / Constructeur de Ch�ines pour les Macro de debugg.dll

    &&1 -> Compteur de Macros [If][Else] / Macros counter

    M N &&1 -> Counter of the times this macro was used. This is used to generate proper label names M0 to M9
    M N &&1 -> Compteur d'embo�tements, utilis�s pour la cr�ation des labels M0..M9, N0..N9

    &&2 -> Compteur de Macros [Repeat][Until] [While][Wend] / Macros counter 

    O&&2 -> Pour la construction des labels [Repeat][Until] [While][End_While] 
            For the construction of the labels [Repeat][Until] [While][End_While] 

;;
_______________

; Basic macros
_______________

;;

    Description : Macros that Move data around
                  Macros de d�placement des donn�es


    Examples:

        Mov eax 45 | Mov D$edi '0000'

        [Mov] eax D$eax, B$MyVar 0_7F, eax MyLp

        Mov eax {RGBA 0_C1 0_C2 0_C3 0_A0}

        Push ebx
        
        Pop ebx

        [Push] W$eax,
               D$eax,
               ax

        [Pop] ax,
              D$eax,
              W$eax

        [Move] D$MyVar D$eax, W$eax W$Mylp

;;

[Mov

    mov #1 #2

    #+2]

[RGBA

    (#1 or (#2 shl 8) or (#3 shl 16) or (#4 shl 24))]

[Push

    push #1

    #+1]

[Pop

    pop #1

    #+1]

[Move

    push #2 | pop #1

    #+2]

;;

    Description : Macros used to Call functions
                  Macros utilis�es pour appeller vos fonctions et les API

    EOS = End of String Equate
          Equate de fin de cha�ne

    Examples :

    [Call] Label

    STDCALL convention : The function will cleanup the stack
                         La fonction est r�sponable de la restauration de la pile

    Call Label D$MyParameter,
               MyLp ...

    Call Label D$MyParameter,
               {B$ 'StringASCII' EOS}, ; ParaMacro: Local label generation [@xxx: B$ 'StringASCII' EOS]
                                       ;            G�n�ration d'un Label local [@xxx: B$ 'StringASCII' EOS]
               {U$ 'StringUNICODE' EOS},
               MyLp ...

    STDCALL convention : the DLL function will cleanup the stack
                         La dll est r�sponable de la restauration de la pile

    Call 'DLLNAME.FunctionName'

    Call 'DLLNAME.FunctionName' D$Parameter,
                                Lp ...

    cdecl convention (MSVCRT mainly)
    The caller must cleanup the stack (add esp, [DWORD]*numberOfArguments)
    !!! Le CCall est r�sponable de la restauration de la pile

    [CCall] 'NOMDLL.AdresseName' D$Parameter,
                                 Lp ...

;;

[Call

    push #L>2

    Call #1]
;;
[CCall

    push #L>2

    Call #1

    #If #N>1

        add esp ((#N-1)*DWORD)

    #EndIf]
;;
______________________________

; Macros HLL Proc / Functions
______________________________

;;

    For the GUI messages routines, if you don't modify the initial stack, you can use the MSG
    equates (they are coded to work with a typical stack value) See [CALLBACK]

    Pour les Routines de traitement des messages GUI, afin d'�viter la transmission redondante
    d'arguments, CE TANT QUE LA PILE N'EST PAS MODIFIEE PAR UN PASSAGE D'ARGUMENT(S) OU PAR UN
    PUSHs DE TYPE Uses OU PUSH EBX, il est possible d'utiliser les Equates de MSG voir [CALLBACK]

    ~~~~~~~~~~~~~~~~~
    Label:

        You can access the arguments like this when the EQUATES work
        D$[HWND] D$[MSG] D$[WPARAM] D$[LPARAM]

        Utilisables -> D$[HWND] D$[MSG] D$[WPARAM] D$[LPARAM]

    ret or [RetR]

    ~~~~~~~~~~~~~~~~~

        You can use -> D$HWND D$MSG D$WPARAM D$LPARAM
        Utilisables -> D$HWND D$MSG D$WPARAM D$LPARAM

        [StackOn]

            You can use these Macros when you are in StackOn : [Local]/[Locals] [Structure] [Uses]
            And you cannot use that now -> D$HWND D$MSG D$WPARAM D$LPARAM

            !!! Attention les Macros [Local]/[Locals] [Structure] [Uses] ne sont pas utilisables
            dans les routines en dehors de StackOn / StackOff
            Inutilisables -> D$HWND D$MSG D$WPARAM D$LPARAM

        [StackOff]

        You can use these again -> D$HWND D$MSG D$WPARAM D$LPARAM
        Utilisables ici � nouveau -> D$HWND D$MSG D$WPARAM D$LPARAM

    ret or Retr

    ~~~~~~~~~~~~~~~~~
      !!! Align your structures (From the biggest member (Q$) to the smallest (B$)
      !!! Alignez vos strutures (de la plus grande taille (Q$) � la plus petite (B$)

  *(1~5)  Structure @STC.XXXXXX, -> (D)@STRUC.XXXXXX contenant le Lp -> @STC.XXXXXX
              QWORD .Q.Var1,     -> Q@STRUC.Q.XXXXXX.Var1  ; QWORD or 2*DWORD aligned
              DWORD .DU.Var2,    -> D@STRUC.DU.XXXXXX.Var2 ; DWORD aligned LSD
              DWORD .DU.Var3,    -> D@STRUC.DU.XXXXXX.Var2 ; DWORD aligned MSD
               WORD .WU.Var4,    -> W@STRUC.WU.XXXXXX.Var3 ; WORD  aligned LSW
               WORD .WU.Var5,    -> W@STRUC.WU.XXXXXX.Var4 ; WORD  aligned MSW
               BYTE .BU.Var6,    -> B@STRUC.BU.XXXXXX.Var5 ; BYTE  aligned LSB
               BYTE .BU.Var7,    -> B@STRUC.BS.XXXXXX.Var6 ; BYTE  aligned MSB
               BYTE .BS.Var8,    -> B@STRUC.BU.XXXXXX.Var7 ; etc.
               BYTE .BU.Var9     -> B@STRUC.BU.XXXXXX.Var8

        (@STC.XXXXXXSIZE is en equate generated by a Macro that contains the structure
         size -> mov eax (!!! not D) @STC.XXXXXXSIZE)

        (@STC.XXXXXXSIZE est une equate g�n�r�e par la Macro et contenant la taille en bytes
         de la structure -> mov eax (!!! pas de pr�fixe de taille D) @STC.XXXXXXSIZE)

;;

[Proc

    #1                          ; Creation of global label / Cr�ation du label global

    StackOn]

[StackOn

    &1=0                        ; &1 = Size of arguments. We zero the counter
                                ; &1 = Taille des arguments. RAZ compteur

    &2=0                        ; &2 = Size of local variables and structures. We zero the counter
                                ; &2 = Taille des variables locales et des structure. RAZ compteur

    &3=                         ; &3 = String of saved variables. Set string to zero
                                ;      Cha�ne de variables pr�serv�es. RAZ cha�ne

    &4=                         ; &4 = Structure name / Nom de la structure

    &5=0                        ; Zero structure offset counter / RAZ compteur d'offsets de structure

    push ebp                    ; Stack frame init / Initialisation du cadre de pile

    mov ebp esp]

[Argument

    {#1 ebp+((#x*DWORD)+DWORD)} ; Equate creation ex:[MyProc@ParamA ebp+0C]
                                ; Fabrique equate ex:[MyProc@ParamA ebp+0C]

    #+1                         ; Next argument / Argument suivant

    &1=(#N*DWORD)]              ; Save size of args / Sauvegarde la taille des arguments

[Arguments

    {#1 ebp+((#x*DWORD)+DWORD)} ; Equate creation ex:[MyProc@ParamA ebp+0C]
                                ; Fabrique equate ex:[MyProc@ParamA ebp+0C]

    #+1                         ; Next argument / Argument suivant

    &1=(#N*DWORD)]              ; Save size of args / Sauvegarde la taille des arguments

[Local

    {#1 ebp-(#x*DWORD)}         ; Equate creation ex:[MyProc@LocalA ebp-4]

    #+1                         ; Next local variable / Variable locale suivante

    &2=(#N*DWORD)               ; Offset counter update / MAJ du compteur d'offet

    sub esp (#N*DWORD)]         ; Save space for local variables on stack
                                ; Reserve la place pour la variable locale sur la pile

[Locals

    {#1 ebp-(#x*DWORD)}         ; Equate creation ex:[MyProc@LocalA ebp-4]

    #+1                         ; Next local variable / Variable locale suivante

    &2=(#N*DWORD)               ; Offset counter update / MAJ du compteur d'offet

    sub esp (#N*DWORD)]         ; Save space for local variables on stack

; TODO anciennes macros de RosAsm � remplacer par nouvelles Macros C Like
[StrucPtrs | {#3 ebp+#2+#F} | #+2]

[Structure | {#1 ebp-&2-4} | sub esp #2+4 | mov D$#1 esp | StrucPtrs 0-&2-#2-4 #L>3]
                                ; Reserve la place pour les variables locales sur la pile
;;
[Structure ; TODO � refaire avec enchainement de strutures et un seul sub esp !

    &4=#1                       ; &4 structure name / &4 Nom structure

    &5=0                        ; Zero structure offset counter / RAZ compteur

    BuildMember #2 #L>2]        ; Create structure on stack / Fabrique la structure sur la pile

[BuildMember

     {&4#2 ebp-(&2+&5+#3)}      ; Member Equates creation ex:[MyProc@MyStructure.MyMembre2 D$ebp-08]
                                ; Fabrique equates membre ex:[MyProc@MyStructure.MyMembre2 D$ebp-08]

     &5=&5+#3                   ; Size counter incrementation / Inc le compteur de la taille

     #+2                        ; Next member/ Membre suivant

     {&4 D$ebp-(&2+&5+DWORD)}   ; Equate creation (Structure LP) ex:[MyProc@MyStructure D$ebp-0C]
                                ; Fabrique equate Lp structure ex:[MyProc@MyStructure D$ebp-0C]

     {&4Size (&5)}              ; Equate creation (Sizeof structure) ex: [MyProc@MyStructureSize 08]
                                ; Fabrique equate taille structure ex: [MyProc@MyStructureSize 08]

     sub esp (&5)               ; Save space on stack for the structure
                                ; Reserve la place pour la structure sur la pile

     push esp                   ; Save structure LP / Sauvegarde le Lp de la structure

     &2=&2+&5+DWORD]            ; Offset counter update / MAJ du compteur d'offet
;;

[Use

    push #1>L                   ; Save register/variables on the stack
                                ; Sauvegarde registres/variables sur la pile

    &3=pop #L>1]                ; Automatic reg restore pop code

[Uses

    push #1>L                   ; Save register/variables on the stack
                                ; Sauvegarde registres/variables sur la pile

    &3=pop #L>1]                ; Automatic reg restore pop code
                                ; Fabrique la cha�ne de commande de restauration registres/variables
[Return

    #If #N=1                    ; Test if there is a return argument / Test si un argument en retour

        mov eax #1              ; Return &TRUE -> mov eax &TRUE | jump P9> (EndP)

    #EndIf                      ; Single return  = Exit Proc / Return seul = Exit Proc

    ExitP] ;  jmp ExitP]                  ; See next equate / Saute � EndP, voir equate suivante


;[ExitP P9>]                     ; Equate to exit proc / Equate pour sortie Proc -> cmp eax &NULL | je ExitP

[ExitP | jmp P9>>] ; TODO quand les If Eslse etc et les sauts courts struturants seront impl�ment�s...

[StackOff

    P9:                         ; Jump label for ExitP / Label de saut pour ExitP

    &3                          ; If &3 >'' restore the registers/variables saved
                                ; Si &3 >'' restaure les registres/variables sauvegard�s

    mov esp ebp                 ; Restore stack pointer / Restaure le pointeur de pile

    pop ebp]                    ; Restore ebp / Restaure le cadre de pile appelant

[EndP

    StackOff

    RetR]

[RetR

  ;  ExitF

    ret &1]                     ; Return to caller and clean the stack by numberofArgs*DWORD
                                ; Retour � l'appelant et retrait des arguments de la pile
;;

   TODO en attente de l'impl�mentation des sauts courts structur�s
   
[ExitF

    #If &&1=> '0'               ; Check if If/Else Macros are paired
                                ; V�rifie si les Macros [If]/[Else_If]/[End_If] sont apair�es

        #Error 'Unpaired use of If / End_If !'

    #EndIf

    #If &&2=> '0'               ; Check if while, repeat Macros are paired
                                ; V�rifie si les Macros [Repeat]/[Until] [While]/[Wend] sont apair�es

        #Error 'Unpaired use of loop constructs !'

    #EndIf]
;;
_______________________________________

; Conditional Branch HLL macros

; Macros HLL branchements contitionels
_______________________________________

; [EQUATES_HLL] Equates for cond jumps / Equates pour la forme HLL integers sign�s et non-sign�s seulement

;;

    Multiple conditional branch (Maximum of 4 + a jmp):

    Multiple branchement conditionel (4 + jmp maxi.):

    Comp D$eax Label = S1>

    Comp eax edx > S1> < S2> = S3> NEGATIVE S9> -> [EQUATES_HLL]

    Comp eax D$LabelA = LabelB,
                      < S2>,
                      NEGATIVE L1<, -> [EQUATES_HLL]
                      ZERO S9>      -> [EQUATES_HLL]
                     
                      jmp S0>

    cmp is kept for spaguetti code / cmp est conserv� pour le code spaghetti

;;

[Comp

    #If #N<4

        #Error '[A] [B] [Condition] [Label] !'

    #Else

        cmp #1 #2 | JmpIf #3>L

    #EndIf]

;;
    
    Conditional execution / Ex�cution conditionelle
    On eax > ebx Mov ebx eax
    On eax = ebx Call Procedure
    On eax > ebx Mov ebx eax, jmp S1<

;;

[On

    cmp #1 #3 | jn#2 P7>

        #4>L

    P7:]

;;
    
    Conditional AND jmp / Saut AND conditionel

    sub eax 45 | JmpIf ZERO L1<

    sub D$ebx 127 |JmpIf ZERO S1>,
                         NEGATIVE S2>
   
    Test eax ebx ZERO S1>
    
    Test eax NA ZERO S1>,
                NEGATIVE S2>
    
    Test D$FL.Var &TRUE TRUE S1>
  
    Test D$FL.Var &TRUE FALSE S1>
    
    On eax = ebx Call Procedure
    On eax > ebx Mov ebx eax, jmp S1<

;;

[JmpIf

    j#1 #2

    #+2]

[Test

    test #1 #2

    JmpIf #3>L]

;;
    
    If/Else/Else_If/End_If -> !!! Short jumps only. Structure properly!
                              !!! Sauts courts uniquement (structurez convenablement).

    If eax = 0
        ...
    Else_If eax > 17, eax < 42
            ...

        If ebx = &FALSE
            ...

        Else
            ...

        End_If

        ...

    End_If
    
;;

;;
[IncCnt1

    #If &&1=0       ; Zero the counter / RAZ compteur l'ors de la premi�re �vocation de la macro

        &&1= '0'

    #Else

        &&1=&&1+1   ; Can be incremented up to 9 / Incr�ment jusqu'� '9'

    #EndIf]

[If.

    cmp #1 #3

    jn#2 M&&1>

    #+3]

[If

    IncCnt1

    If. #1>L]

[Else_If

    jmp N&&1>

 M&&1:

    If. #1>L]

[Else

    jmp N&&1>

 M&&1:]

[End_If

    M&&1:
    N&&1:

    &&1=&&1-1]
;;

;;

    Repeat macro

        ... 

    Until B$esi = EOS 

;;
;;
[Repeat

    #If &&2=0

        &&2= '0'

    #Else

        &&2=&&2+1

    #EndIf

    O&&2:]

[Until

    cmp #1 #3 | jn#2 O&&2<

    O&&2: &&2=&&2-1]
;;
;;

    While B$esi <> EOS 

        ... 

    EndWhile 

;;
;;
[While

    #If &&2=0

        &&2= '0'

    #Else

        &&2=&&2+1

    #EndIf

    O&&2:

    cmp #1 #3 | jn#2 O&&2>]

[End_While

    jmp O&&2<

    O&&2: &&2=&&2-1]
;;

____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT


