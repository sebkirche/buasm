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

    Français:

    Pour mieux (bien) comprendre ce que ces Macros 'génèrent' nous vous invitons à un double
    clic gauche sur leurs évocations dans le code. Choisissez l'option "Unfold" dans le menu
    pop: Vous verrez ainsi que RosAsm ne substitue pas de code à votre insue mais que c'est
    VOUS qui construisez vos substitutions -> Pour gagner en lisibilisé, en souplesse et en
    facilité de maintenance de votre code. Plus il sera lisible et facilement compréhensible,
    plus vous pourrez y retourner facilement après plusieurs mois ou permettre à d'autres de
    participer sa la maintenance...

    Toutes les Macros utilisées dans RosASm doivent être déclarées ici et, dans la mesure du
    possible, regroupées par catégories. N'implémentez pas de nouvelles Macros utilisant en
    interne des registres, des variables globales ou des Label globaux (c'est une source de
    conflits).
    !!! Par principe, ne multipliez pas la création de nouvelles Macros -> Evitez, par-dessus
    toutes choses, les diverses formes d'encapsulations d'API: Cela n'apporte rien au niveau
    de la lisibilité (au contraire) et provoque de nombreuses sources d'erreurs:
    Soignez les commentaires et utilisez des Proc/routines communes
    ________________________________________________________________

    ; List of Local labels to use and that Macros use:

    ; Labels locaux utilisée au sein des Macros:
      (Liste à compléter en fonction des nouvelles implémentations)
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
      (List to complete when new implémentations are made)

    ; Variables interne du Macro parser utilisée au sein des Macros:
      (Liste à compléter en fonction des nouvelles implémentations)
    _________________________________________________________________

    &1 -> Size of arguments / Taille des arguments

    &2 -> Size of local variables and structures / Taille des variables locales et des structure

    &3 -> String of register or variable saved / Chaîne de registre ou variables préservées

    &4 -> Structure name / Nom de la structure

    &5 -> Structure offset counter / Compteur d'offsets de structure

    &6 -> Constructor of strings for debugg.dll macros / Constructeur de Châines pour les Macro de debugg.dll

    &&1 -> Compteur de Macros [If][Else] / Macros counter

    M N &&1 -> Counter of the times this macro was used. This is used to generate proper label names M0 to M9
    M N &&1 -> Compteur d'emboîtements, utilisés pour la création des labels M0..M9, N0..N9

    &&2 -> Compteur de Macros [Repeat][Until] [While][Wend] / Macros counter 

    O&&2 -> Pour la construction des labels [Repeat][Until] [While][End_While] 
            For the construction of the labels [Repeat][Until] [While][End_While] 

;;
_______________

; Basic macros
_______________

;;

    Description : Macros that move data around
                  Macros de déplacement des données


    Examples:

        mov eax 45 | mov D$edi '0000'

        [Mov] eax D$eax, B$MyVar 0_7F, eax MyLp

        [RGBAmov] D$MyVar 0_00 0_00 0_FF 0_00, eax 45 120 255 &NULL

        push ebx

        [Push] W$eax,
               D$eax,
               ax

        pop ebx

        [Pop] ax,
              D$eax,
              W$eax

        [Move] D$MyVar D$eax, W$eax W$Mylp

;;

[Mov

    mov #1 #2

    #+2]

[RGBAmov

    mov #1 (#2 or (#3 shl 8) or (#4 shl 16) or (#5 shl 24))

    #+5]

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

    Description : Macros used to call functions
                  Macros utilisées pour appeller vos fonctions et les API

    EOS = End of String Equate
          Equate de fin de chaîne

    Examples :

    [Call] Label

    STDCALL convention : The function will cleanup the stack
                         La fonction est résponable de la restauration de la pile

    Call Label D$MyParameter,
               MyLp ...

    Call Label D$MyParameter,
               {B$ 'StringASCII' EOS}, ; ParaMacro: Local label generation [@xxx: B$ 'StringASCII' EOS]
                                       ;            Génération d'un Label local [@xxx: B$ 'StringASCII' EOS]
               {U$ 'StringUNICODE' EOS},
               MyLp ...

    STDCALL convention : the DLL function will cleanup the stack
                         La dll est résponable de la restauration de la pile

    Call 'DLLNAME.FunctionName'

    Call 'DLLNAME.FunctionName' D$Parameter,
                                Lp ...

    cdecl convention (MSVCRT mainly)
    The caller must cleanup the stack (add esp, [DWORD]*numberOfArguments)
    !!! Le CCall est résponable de la restauration de la pile

    [CCall] 'NOMDLL.AdresseName' D$Parameter,
                                 Lp ...

;;

[Call

    push #L>2

    call #1]

[CCall

    push #L>2

    call #1

    #If #N>1

        add esp ((#N-1)*DWORD)

    #EndIf]
______________________________

; Macros HLL Proc / Functions
______________________________

;;

    For the GUI messages routines, if you don't modify the initial stack, you can use the MSG
    equates (they are coded to work with a typical stack value) See [CALLBACK]

    Pour les Routines de traitement des messages GUI, afin d'éviter la transmission redondante
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
        Utilisables ici à nouveau -> D$HWND D$MSG D$WPARAM D$LPARAM

    ret or Retr

    ~~~~~~~~~~~~~~~~~
      !!! Align your structures (From the biggest member (Q$) to the smallest (B$)
      !!! Alignez vos strutures (de la plus grande taille (Q$) à la plus petite (B$)

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

        (@STC.XXXXXXSIZE est une equate générée par la Macro et contenant la taille en bytes
         de la structure -> mov eax (!!! pas de préfixe de taille D) @STC.XXXXXXSIZE)

;;

[Proc

    #1                          ; Creation of global label / Création du label global

    StackOn]

[StackOn

    &1=0                        ; &1 = Size of arguments. We zero the counter
                                ; &1 = Taille des arguments. RAZ compteur

    &2=0                        ; &2 = Size of local variables and structures. We zero the counter
                                ; &2 = Taille des variables locales et des structure. RAZ compteur

    &3=                         ; &3 = String of saved variables. Set string to zero
                                ;      Chaîne de variables préservées. RAZ chaîne

    &4=                         ; &4 = Structure name / Nom de la structure

    &5=0                        ; Zero structure offset counter / RAZ compteur d'offsets de structure

    push ebp                    ; Stack frame init / Initialisation du cadre de pile

    mov ebp esp]

[Argument
                                ; Callback = 1*DWORD  Argument = 1*DWORD
    {#1 ebp+((1*DWORD)+DWORD)}  ; Equate creation [MyProc@ParamA ebp+8]
                                ; Fabrique equate [MyProc@ParamA ebp+8]

    &1=DWORD]                   ; Save size of arg / Sauvegarde la taille de l'arguments

[Arguments

    {#1 ebp+((#x*DWORD)+DWORD)} ; Equate creation ex:[MyProc@ParamA ebp+0C]
                                ; Fabrique equate ex:[MyProc@ParamA ebp+0C]

    #+1                         ; Next argument / Argument suivant

    &1=(#N*DWORD)]              ; Save size of args / Sauvegarde la taille des arguments

[Local

    {#1 ebp-DWORD}              ; Equate creation [MyProc@LocalA ebp-4]
                                ; Fabrique equate [MyProc@LocalA ebp-4]

    &2=DWORD                    ; Offset counter update / MAJ du compteur d'offet

    sub esp DWORD]              ; Save space for local variable on stack
                                ; Reserve la place pour la variable locale sur la pile

[Locals

    {#1 ebp-(&2+(#x*DWORD))}    ; Equate creation ex:[MyProc@LocalA ebp-4]

    #+1                         ; Next local variable / Variable locale suivante

    &2=&2+(#N*DWORD)            ; Offset counter update / MAJ du compteur d'offet

    sub esp (#N*DWORD)]         ; Save space for local variables on stack
                                ; Reserve la place pour les variables locales sur la pile

[Structure

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

[Uses

    push #1>L                   ; Save register/variables on the stack
                                ; Sauvegarde registres/variables sur la pile

    &3=pop #L>1]                ; Automatic reg restore pop code
                                ; Fabrique la chaîne de commande de restauration registres/variables

[Return

    #If #N=1                    ; Test if there is a return argument / Test si un argument en retour

        mov eax #1              ; Return &TRUE -> mov eax &TRUE | jump P9> (EndP)

    #EndIf                      ; Single return  = Exit Proc / Return seul = Exit Proc

    jmp ExitP]                  ; See next equate / Saute à EndP, voir equate suivante

[ExitP P9>]                     ; Equate to exit proc / Equate pour sortie Proc -> cmp eax &NULL | je ExitP

[StackOff

    P9:                         ; Jump label for ExitP / Label de saut pour ExitP

    &2=0                        ; &2 = Size of local variables and structures. We zero the counter

    &3 | &3=                    ; If &3 >'' restore the registers/variables saved and reset to zero string &3
                                ; Si &3 >'' restaure les registres/variables sauvegardés et RAZ chaîne &3

    &4=                         ; &4 = Structure name / Nom de la structure

    &5=0                        ; Zero structure offset counter / RAZ compteur d'offsets de structure

    mov esp ebp                 ; Restore stack pointer / Restaure le pointeur de pile

    pop ebp]                    ; Restore ebp / Restaure le cadre de pile appelant

[EndP

    StackOff

    RetR]

[RetR

    ExitF

    ret &1                      ; Return to caller and clean the stack by numberofArgs*DWORD
                                ; Retour à l'appelant et retrait des arguments de la pile

    &1=0]                       ; We zero the counter / RAZ compteur

[ExitF

    #If &&1=> '0'               ; Check if If/Else Macros are paired
                                ; Vérifie si les Macros [If]/[Else_If]/[End_If] sont apairées

        #Error 'Unpaired use of If / End_If !'

    #EndIf

    #If &&2=> '0'               ; Check if while, repeat Macros are paired
                                ; Vérifie si les Macros [Repeat]/[Until] [While]/[Wend] sont apairées

        #Error 'Unpaired use of loop constructs !'

    #EndIf]
_______________________________________

; Conditional Branch HLL macros

; Macros HLL branchements contitionels
_______________________________________

; [EQUATES_HLL] Equates for cond jumps / Equates pour la forme HLL integers signés et non-signés seulement

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

    cmp is kept for spaguetti code / cmp est conservé pour le code spaghetti

;;

[Comp

    #If #N<4

        #Error '[A] [B] [Condition] [Label] !'

    #ElseIf #1=mem

        #If #2=mem

            #Error 'Comp Mem -> Reg/Label/Imm !'

        #EndIf

    #ElseIf #N>10

        #Error '4 x Conditions Max !'

    #ElseIf #N=4

        cmp #1 #2 | j#3 #4

    #ElseIf #N=5

        #Error "Wrong parameter's number"

    #ElseIf #N=6

            cmp #1 #2 | j#3 #4 | j#5 #6

    #ElseIf #N=7

        #Error "Wrong parameter's number"

    #ElseIf #N=8

        cmp #1 #2 | j#3 #4 | j#5 #6 | j#7 #8

    #ElseIf #N=9

        #Error "Wrong parameter's number"

    #ElseIf #N=10

        cmp #1 #2 | j#3 #4 | j#5 #6 | j#7 #8 | j#9 #10

    #EndIf]

;;
    
    Conditional execution / Exécution conditionelle
    On eax > ebx mov ebx eax
    On eax = ebx Call Procedure
    On eax > ebx mov ebx eax, jmp S1<

;;

[On

    cmp #1 #3 | jn#2 P7>

        #4>L

    P7:]

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

    EndIf
    
;;

[IncCnt1

    #If &&1=0       ; Zero the counter / RAZ compteur l'ors de la première évocation de la macro

        &&1= '0'

    #Else

        &&1=&&1+1   ; Can be incremented up to 9 / Incrément jusqu'à '9'

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

    Repeat macro

        ... 

    Until B$esi = EOS 

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

    While B$esi <> EOS 

        ... 

    EndWhile 

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

    !!! EXCEPTIONEL NE JAMAIS FAIRE DE MACROS D'ENCAPSULATION DE CE GENRE

    Macros pour utiliser le module de debuggage de la DEBUG.DLL:

        ; Saute une ligne
        OutPutStringLiteral CR LF

        ; Affiche le nom de la variable [FLOAT] et son contenu
        OutPutFloat F$F.Variable | OutPutStringLiteral CR LF

        ; Affiche le nom de la variable [REEL] et son contenu
        OutPutReel R$R.Variable | OutPutStringLiteral CR LF

        ; Affiche le nom de la variable [DWORD] non signé et son contenu au format decimal
        OutPutDec D$DU.Variable | OutPutStringLiteral CR LF

        ; Affiche le nom du Lp -> Préfixe.Variable et son adresse au format hexadecimal
        OutPutDec DU.Variable | OutPutStringLiteral CR LF

        ; Idem avec un registre
        mov eax 1999 | OutPutDec eax | OutPutStringLiteral CR LF

        ; Affiche le nom de du registre et son contenu au format hexadecimal
        mov eax 0_ABCDEF | OutPutHex eax | OutPutStringLiteral CR LF

        ; Affiche le nom de la variable [DWORD] non signé et son contenu au format hexadecimal
        OutPutHex D$DU.Variable | OutPutStringLiteral CR LF

        ; Affiche le nom du registre et son contenu signed au format decimal
        mov eax 0-32000 | OutPutSignedDec eax | OutPutStringLiteral CR LF

        ; Idem
        mov eax 0FFFF_FFFF | OutPutSignedDec eax | OutPutStringLiteral CR LF

        ; Affiche le nom du Lpointeur sur la chaine de bytes et son contenu au format ASCII
        OutPutString STR.A.Variable | OutPutStringLiteral CR LF

        ; Affiche le contenu ASCII transmis
        OutPutStringLiteral "1234567890123456789012345678901234567890123456789012345678901234567890123456" CR LF,
                            "1234567890123456789012345678901234567890123456789012345678901234567890123456" CR LF,
                            "1234567890123456789012345678901234567890123456789012345678901234567890123456" CR LF,
                            "1234567890123456789012345678901234567890123456789012345678901234567890123456" CR LF,
                            "1234567890123456789012345678901234567890123456789012345678901234567890123456" CR LF,
                            "1234567890123456789012345678901234567890123456789012345678901234567890123456" CR LF,
                            "1234567890123456789012345678901234567890123456789012345678901234567890123456" CR LF,
                            "1234567890123456789012345678901234567890123456789012345678901234567890123456" CR LF,

                            ;...

                            "1234567890123456789012345678901234567890123456789012345678901234567890123456" CR LF,
                            "1234567890123456789012345678901234567890123456789012345678901234567890123456" CR LF,
                            "1234567890123456789012345678901234567890123456789012345678901234567890123456" CR LF,
                            "1234567890123456789012345678901234567890123456789012345678901234567890123456" CR LF,
                            "1234567890123456789012345678901234567890123456789012345678901234567890123456" CR LF

        ; Ajoute dans le fichier DebugLog.txt le contenu ASCII transmis (EOS n'est pas
        ; nécessaire en fin de liste).
        ; !!! Le texte n'apparait pas dans la fenêtre de debugg, c'est une REM pour le fichier
        OutPutLog "Texte enregistré dans le fichier DebugLog.txt" CR LF

        ; Affiche le dernier message d'erreur
        OutPutError

;;

[OutPutDec

    &6=#1

    {&0: B$ "&6" EOS}

    &6=

    Call 'DEBUGG.OutPutDec' &0,
                            #1]

[OutPutSignedDec

    &6=#1

    {&0: B$ "&6" EOS}

    &6=

    Call 'DEBUGG.OutPutSignedDec' &0,
                                  #1]

[OutPutHex

    &6=#1

    {&0: B$ "&6" EOS}

    &6=

    Call 'DEBUGG.OutPutHex' &0,
                            #1]

[OutPutFloat

    fld #1

    &6=#1

    {&0: B$ "&6" EOS}

    &6=

    Call 'DEBUGG.OutPutFloat' &0]

[OutPutReel

    fld #1

    &6=#1

    {&0: B$ "&6" EOS}

    &6=

    Call 'DEBUGG.OutPutReel' &0]

[OutPutString

    &6=#1

    {&0: B$ "&6" EOS}

    &6=

    Call 'DEBUGG.OutPutString' &0,
                               #1]

[OutPutError

    Call 'DEBUGG.OutPutError' ]

[OutPutStringLiteral

    {&0: B$ #F>L EOS}

    Call 'DEBUGG.OutPutStringSimple' &0]

[OutPutLog

    {&0: B$ #F>L EOS}

    Call 'DEBUGG.OutPutLogFile' &0]

[OutVideDebbug

    Call 'DEBUGG.VideDebbug']
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
TITLE EQUATES         ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;;

    English:

    Equates guidelines:

    - All the global equates should be here in this TITLE.
      It applies to Individual equates and structure equates

    - They must be regrouped in categories

    - They must be in alphabetical order of their prefix

    - They must be in alphabetical order

    Français:

    - TOUTES les définitions d'EQUATES à portées globales doivent se trouver dans ce TITLE:
      que ce soit des EQUATES individuelles ou des structures d'EQUATES

    - Etre rangées par catégories

    - Etre rangées en ordre alphabétique de préfixe

    - Etre rangées en ordre alphabétique de nom (à moins que ce ne soit une Struture
      d'Equates évidement)

;;
________________________________________________________________________

; Message [MSG_PUMP] equates (Used in WindowProc or DialogProc)

; Equates pour manipuler les arguments provenant du [MSG_PUMP] dans les
; routines de traitements des WindowProc ou DialogProc
________________________________________________________________________

[CALLBACK ebp+(01*DWORD)  ; Adresse de retour
 HWND     ebp+(02*DWORD)  ; Handle de la fenêtre courante
 MSG      ebp+(03*DWORD)  ; Id message fenêtre courant
 WPARAM   ebp+(04*DWORD)  ; 32-bit message-specifique
 LPARAM   ebp+(05*DWORD)] ; 32-bit message-specifique
______________________________________________________________________

; Size equates : mov eax D$esi+(2*DWORD), cl B$ah+(3*BYTE)

; Equates d'offsets taille: mov eax D$esi+(2*DWORD), cl B$ah+(3*BYTE)
______________________________________________________________________

[ASCII    1  ; Need the A suffix in some APIS (See also Tools menu -> Ascii Table)
 BYTE     1  ; 0-128/+127 or 0/255 00/00_FF
 DWORD    4  ; 0-2147483648/+2147483647 or 0/4294967295 00/00_FFFF_FFFF
 FLOAT    4  ; http://en.wikipedia.org/wiki/IEEE_754-1985
 QWORD    8  ; 00/00_FFFF_FFFF_FFFF_FFFF
 REEL     8  ; http://en.wikipedia.org/wiki/IEEE_754-1985
 UNICODE  2  ; Need the W suffix in some APIs
 WORD     2  ; 0-32768/32767 ou 0/65535 00/00_FFFF
 XWORD   16] ; For SSE

;;

[ASCII   (01*BYTE)  ; Nécessite le suffixe A dans certaines API (Voir menu Tools -> Ascii Table)
 BYTE    1          ; 0-128/+127 ou 0/255 00/00_FF
 DWORD   (04*BYTE)  ; 0-2147483648/+2147483647 ou 0/4294967295 00/00_FFFF_FFFF
 FLOAT   (04*BYTE)  ; http://en.wikipedia.org/wiki/IEEE_754-1985
 QWORD   (08*BYTE)  ; 00/00_FFFF_FFFF_FFFF_FFFF
 REEL    (08*BYTE)  ; http://en.wikipedia.org/wiki/IEEE_754-1985
 UNICODE (02*BYTE)  ; Nécessite le suffixe W dans certaines API
 WORD    (02*BYTE)  ; 0-32768/32767 ou 0/65535 00/00_FFFF
 XWORD   (16*BYTE)] ; Pour SSE

;;
______________________

; Equates for strings

; Equates chaînes
______________________

[CR  0D  ; Carriage return
 EOS 00  ; End of String
 LF  0A] ; Line feed
___________________

; Equates for HLL

; Equates pour HLL
___________________

@EQUATES_HLL:

[= e    < b     > a     =< be   <= be   => ae   >= ae   <> ne

        <s l    >s g    =<s le  <=s le  =>s ge  >=s ge
        s< l    s> g    s=< le  s<= le  s=> ge  s>= ge

        ZERO z          NOT_ZERO nz
        FALSE z         TRUE nz
        NOT_FALSE nz    NOT_TRUE z
        NEGATIVE s       POSITIVE ns]
______________________________________

; Coordinates equate structures

; Structures d'equates de coordonnées
______________________________________

[PX (00*WORD)
 PY (01*WORD)]

[POINTX (00*DWORD)
 POINTY (01*DWORD)]

[LEFT   (00*DWORD)
 TOP    (01*DWORD)
 RIGHT  (02*DWORD)
 BOTTOM (03*DWORD)]
_______________

; Flag equates
_______________

[NA  0_FFFF_FFFF ; 0-1 00_11111111_11111111_11111111_1111111
 NO  0-1
 YES 0+1]
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
TITLE CODE            ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

@TREE:

;;
    
    Ce PE est utilisé par la DEBUGG.DLL pour afficher ses divers messages. Il est chargé,
    lancé et terminé par celle-ci.
   
     - [MAIN] est le point d'entrée du PE. C'est cette routine qui lance la thread [MSG]
       après avoir procédé à l'initialisation de la fenêtre principale [INIT_Window_Main].
       C'est aussi elle qui, après l'arrêt de la thread [MSG_PUMP] rends la main au mécanisme
       qui l'a appelée

     - [MSG_PUMP] se charge du routing de toutes les fenêtres: Elle est en mode Broadcast
     
     - [WM_CREATE_Main] initialise l'Edit box fils ainsi que sa fonte. c'est dans cet Edit
       Box que tous les messages provenant de DEBUGG.DLL sont affichés
      
     - [MainWindowProc] trie les messages qui lui sont envoyés et effectue les Call vers les
       Procédures et routines de traitements appropriés  
 
      A) Lancements des initialisations nécessaires au PE
 
          A1) Lancement de [INIT_Window_Main]
                
      B) Attente, réception et routing des messages de l'interface GUI de type fenêtre

      C) Réception et routing des MSG de [MainWindowProc] vers leurs traitements 
      
      D) [WM_SIZE_Main] fixe la taille l'edit sur la fenêtre mère
   
      E) [WM_CREATE_Main] création de l'Edit box et de sa fonte 
 
      F) [WM_DESTROY_Main] destruction de la font de l'Edit box et de la fenêtre Main
    
      G) Reception du message &WM_NULL et fin de la boucle du [MSG_PUMP]

      H) Rends la main au mécanisme qui a appelé le PE

   [MAIN] A)
    |
    |--->[INIT_Window_Main] A1)
    |
    |--->[MSG_PUMP] B)/G)
    |    |
    |    |--->[MainWindowProc] C)
    |    °    |
    |         |--->[WM_SIZE_Main] D)
    |         |
    |         |--->[WM_CREATE_Main] E)
    |         |
    |         |--->[WM_DESTROY_Main] F)    
    |         °
    [-'KERNEL32.ExitProcess'-] H)
    °

;;

Proc MSG_PUMP:
    __________________________________________________

    ; Thread principale du process principal: Attente
    ; réception et routing des MSG des interfaces GUI
    __________________________________________________

    Structure @STC.MSG,
        DWORD .hwnd,
        DWORD .message,
        DWORD .wParam,
        DWORD .lParam,
        DWORD .time,
    (2*DWORD) .point ; [PX PY]

    ; Saute au début de la boucle
    jmp S1>
    ___________

    ; MSG loop
    ___________

    ; Envoyer MSG à la fenêtre concernée
L0: Call 'USER32.DispatchMessageA' @STC.MSG

        ; Attend l'arrivée d'un MSG (Wait on Event bloquant)
S1:     Call 'USER32.GetMessageA' @STC.MSG,
                                  &NULL,    ; NULL -> MSG Broadcast
                                  &WM_NULL, ; NULL -> Pas de filtrage mininum des MSG
                                  &WM_NULL  ; NULL -> Pas de filtrage maximum des MSG

    ; Eax > &WM_NULL
    test eax eax | jg L0<

    ; Eax = 0-1 -> MSG ERREUR
    js S0>
    _________________________________

    ; Sortie de boucle sans d'erreur
    _________________________________

    ; Eax = &WM_NULL

EndP
    _________________________________

    ; Sortie de boucle avec d'erreur
    _________________________________

    ; MSG ERREUR
S0: int3 | nop ; >>> ERREUR ?!? <<<
____________________________________________________________________________________________

Proc MainWindowProc:

;;

    [PREFIXES]
    
    [TREE]

    [MAIN]

;;

    Arguments @hwnd,
              @msg,
              @wParam,
              @lParam

    If D@msg = &WM_SETFOCUS

        Call WM_SETFOCUS_Main

    Else_If D@msg = &WM_KILLFOCUS

        Call WM_KILLFOCUS_Main

    Else_If D@msg = &WM_SIZE

        Call WM_SIZE_Main

    Else_If D@msg = &WM_CREATE

        Call WM_CREATE_Main

    Else_If D@msg = &WM_DESTROY

        Call WM_DESTROY_Main

    Else

        Call 'USER32.DefWindowProcA' D@hwnd,
                                     D@msg,
                                     D@wParam,
                                     D@lParam

    End_If

EndP
____________________________________________________________________________________________

WM_SETFOCUS_Main:

    Call 'USER32.SetLayeredWindowAttributes' D$HWND,
                                             0,
                                             0_FF,      ; Niveau Alpha 0_FF: opaque 100% 0_00: transparent 100 %
                                             &LWA_ALPHA

ret
____________________________________________________________________________________________

WM_KILLFOCUS_Main:

    Call 'USER32.SetLayeredWindowAttributes' D$HWND, ;eax,
                                             0_FF_FF_FF, ; Couleur transparente
                                             0_7F,       ; Niveau Alpha 0_FF: opaque 100% 0_00: transparent 100 %
                                             &LWA_ALPHA+&LWA_COLORKEY

ret
____________________________________________________________________________________________

WM_SIZE_Main:

    ; Width = LOWORD(lParam)
    ; Height = HIWORD(lParam)

    ; Fixe la taille l'edit sur la fenêtre mère
    mov eax D$LPARAM, edx eax | shr edx 16 | and eax 0FFFF

    Call 'USER32.MoveWindow' D$H.Edit,
                             0,
                             0,
                             eax,
                             edx,
                             &TRUE

     xor eax eax

ret
____________________________________________________________________________________________

WM_CREATE_Main:

    StackOn

        Structure @STRUC.LOGFONTA,
           DWORD .lfHeight,
           DWORD .lfWidth,
           DWORD .lfEscapement,
           DWORD .lfOrientation,
           DWORD .lfWeight,
            BYTE .lfItalic,
            BYTE .lfUnderline,
            BYTE .lfStrikeOut,
            BYTE .lfCharSet,
            BYTE .lfOutPrecision,
            BYTE .lfClipPrecision,
            BYTE .lfQuality,
            BYTE .lfPitchAndFamily,
    &LF_FACESIZE .lfFaceName

        mov D@STRUC.LOGFONTA.lfHeight 0-13

        mov D@STRUC.LOGFONTA.lfWidth  &NULL

        mov D@STRUC.LOGFONTA.lfEscapement &NULL

        mov D@STRUC.LOGFONTA.lfOrientation &NULL

        mov D@STRUC.LOGFONTA.lfWeight &FW_DONTCARE

        mov B@STRUC.LOGFONTA.lfItalic &FALSE

        mov B@STRUC.LOGFONTA.lfUnderline &FALSE

        mov B@STRUC.LOGFONTA.lfStrikeOut &FALSE

        mov B@STRUC.LOGFONTA.lfCharSet &DEFAULT_CHARSET

        mov B@STRUC.LOGFONTA.lfOutPrecision &OUT_DEFAULT_PRECIS

        mov B@STRUC.LOGFONTA.lfClipPrecision &CLIP_DEFAULT_PRECIS

        mov B@STRUC.LOGFONTA.lfQuality &DEFAULT_QUALITY

        mov B@STRUC.LOGFONTA.lfPitchAndFamily &DEFAULT_PITCH

        mov D@STRUC.LOGFONTA.lfFaceName+0 'Cour'
        mov D@STRUC.LOGFONTA.lfFaceName+4 'ier '
        mov D@STRUC.LOGFONTA.lfFaceName+8 'New'+EOS

        ; Création de la fonte utilisée dans l'Edit
        Call 'GDI32.CreateFontIndirectA' @STRUC.LOGFONTA

        mov D$H.Font eax

    StackOff

    ; Recupère le Handle de la fenêtre
    mov eax D$HWND, D$H.Window eax

    ; Creation de l'Edit Box
    Call 'User32.CreateWindowExA' &WS_EX_LEFT,
                                  {B$ 'EDIT' EOS},
                                  &NULL,
                                  &WS_CHILD+&WS_VISIBLE+&ES_MULTILINE+&ES_LEFT+&ES_AUTOVSCROLL+&WS_VSCROLL+&WS_HSCROLL,
                                  &NULL,
                                  &NULL,
                                  &NULL,
                                  &NULL,
                                  eax,
                                  01,
                                  D$H.Instance,
                                  &NULL

    mov D$H.Edit eax

    ; Attribuer la fonte à l'Edit
    Call 'USER32.SendMessageA' eax,
                               &WM_SETFONT,
                               D$H.Font,
                               &TRUE

    xor eax eax

ret
____________________________________________________________________________________________

WM_DESTROY_Main:

    Call 'GDI32.DeleteObject' D$H.Font

    Call 'USER32.PostQuitMessage' &WM_NULL

    xor eax eax

ret
____________________________________________________________________________________________

Main:

    Call INIT_Window_Main

    Call MSG_PUMP

    Call 'KERNEL32.ExitProcess' eax
____________________________________________________________________________________________

INIT_Window_Main:

    StackOn

        Structure @STRUC.WNDCLASS,
            DWORD .style,
            DWORD .lpfnWndProc,
            DWORD .cbClsExtra,
            DWORD .cbWndExtra,
            DWORD .hInstance,
            DWORD .hIcon,
            DWORD .hCursor,
            DWORD .hbrBackground,
            DWORD .lpszMenuName,
            DWORD .lpszClassName

        mov D@STRUC.WNDCLASS.style &CS_NOCLOSE

        mov D@STRUC.WNDCLASS.lpfnWndProc MainWindowProc

        mov D@STRUC.WNDCLASS.cbClsExtra 0

        mov D@STRUC.WNDCLASS.cbWndExtra 0

        Call 'KERNEL32.GetModuleHandleA' &NULL

        mov D$H.Instance eax, D@STRUC.WNDCLASS.hInstance eax

        Call 'USER32.LoadIconA' eax,
                                1

        mov D@STRUC.WNDCLASS.hIcon eax

        Call 'USER32.LoadCursorA' 0,
                                  &IDC_ARROW

        mov D@STRUC.WNDCLASS.hCursor eax

        mov D@STRUC.WNDCLASS.hbrBackground &FALSE

        mov D@STRUC.WNDCLASS.lpszMenuName &NULL

        mov D@STRUC.WNDCLASS.lpszClassName STR.A.WindowClassName

        Call 'USER32.RegisterClassA' @STRUC.WNDCLASS

        Call 'USER32.CreateWindowExA' &WS_EX_LEFT+&WS_EX_TOPMOST+&WS_EX_LAYERED,
                                      STR.A.WindowClassName,
                                      STR.A.WindowClassName,
                                      &WS_VISIBLE+&WS_THICKFRAME+&WS_CAPTION,
                                      &CW_USEDEFAULT,
                                      &CW_USEDEFAULT,
                                      &CW_USEDEFAULT,
                                      &CW_USEDEFAULT,
                                      &NULL,
                                      &FALSE,
                                      D$H.Instance,
                                      &NULL

   StackOff

ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
TITLE DATAS           ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

@PREFIXES:

;;
    
    !!! Pay attention, this is very important:
    
    - All global data declarations must be placed in this TITLE:
      Wether data or structures of data, initialized or virtuel
     
    - All variable's names are implicitly Lp(pointers)
     
    - Never create a variable name without a Prefix.
      ~~~~~                        ~~~~~~~~~~~~~~~~
    - Data must stay aligned with their types : 16 ,8 , 4 , 2 , 1 bytes
    
    - They also MUST be in their category:
    
    
    !!! Attention très important:

    - TOUTES les déclarations de datas à portée globale doivent se trouver dans ce TITLE:
      Que ce soit des datas ou des structures de datas, initialisées ou vituelles

    - Tous les noms de variables sont des Lp implicites
   
    - Ne créez jamais de nom de variable sans préfixe
               ~~~~~~                    ~~~~~~~~~~~~ 
    - Les datas doivent rester alignées selon leur type: 16, 8, 4, 2, 1 octets/bytes

    - Etre rangées dans leur catégorie:

       <16 [INITIALISED_DATAS_128]           [VIRTUAL_DATAS_128]
       
       <8  [INITIALISED_TABLES_64]           [VIRTUAL_TABLES_64]

       <8  [INITIALISED_DATAS_64]            [VIRTUAL_DATAS_64]

       <4  [INITIALISED_DATAS_32]            [VIRTUAL_DATAS_32]

       <4  [INITIALISED_STRUCTURES_32]       [VIRTUAL_STRUCTURES_32]
     
       <4  [INITIALISED_TABLES_32]           [VIRTUAL_TABLES_32]
      
       <4  [INITIALISED_BUFFER_32]           [VIRTUAL_BUFFER_32]
 
       <4  [INITIALISED_STRINGS_ASCII_32]    [VIRTUAL_STRINGS_ASCII_32]

       <4  [INITIALISED_STRINGS_UNICODE_32]  [VIRTUAL_STRINGS_UNICODE_32]

       <2  [INITIALISED_DATAS_16]            [VIRTUAL_DATAS_16]

       <1  [INITIALISED_DATAS_8]             [VIRTUAL_DATAS_8]

    - Be sorted out by prefixs / Etre rangées par préfixe

      Préfix:  Type:   Align: Signé/Non-signé:     Initialisée:  Virtuelle:
      BS.      [BYTE]  <01... +/-................. [BS.i]....... [BS.v]
      BU.      [BYTE]  <01... + .................. [BU.i]....... [BU.v]
      BUFFER.x [DWORD] <04... Lp -> BUFFER.Type... [BUFFER.BU.i] [BUFFER.BU.v] 
      DS.      [DWORD] <04... +/-................. [DS.i]....... [DS.v]
      DU.      [DWORD] <04... +  ................. [DU.i]....... [DU.v]
      F.       [FLOAT] <04... +/-................. [F.i]........ [F.v]
      FL.      [DWORD] <04... Flag TRUE/FALSE..... [FL.i]....... [FL.v]
      FLM.     [DWORD] <04... Flags masque........ [FLM.i]...... [FLM.v]
      H.       [DWORD] <04... Handle.............. [H.i]........ [H.v]
      HDC      [DWORD] <04... H display........... [HDC.i]...... [HDC.v]
      HCDC     [DWORD] <04... H display compatible [HCDC.i]..... [HCDC.v]
      ID.      [DWORD] <04... Id +/- +............ [ID.i]....... [ID.v]
      LP.      [DWORD] <04... Lp -> Lp............ [LP.i]....... [LP.v]
      Q.       [QWORD] <08....+................... [Q.i]........ [Q.v]
      R.       [REEL]  <08... +/-................. [R.i]........ [R.v]
      STRUC.   [DWORD] <04... Lp -> Structure..... [STRUC.i].... [STRUC.v]
      STR.x    [DWORD] <04... String ASCII/UNICODE [STR.A.i]...  [STR.A.v]
      TABLE.x  [DWORD] <04/08 Lp -> TABLE.Type.... [TABLE.BU.i]. [TABLE.BU.v]
      WS.      [WORD]  <02... +/-................. [WU.i]....... [WU.v]
      WU.      [WORD]  <02... + .................. [WS.i]....... [WS.v]
      XR.      [XWORD] <16... 02*[REEL] +/-....... [XR.i]....... [XR.v]
      XF.      [XWORD] <16... 04*[FLOAT] +/-...... [XF.i]....... [XF.v]
      XDS.     [XWORD] <16... 04*[DWORD] +/-...... [XDS.i]...... [XDS.v]
      XDU.     [XWORD] <16... 04*[DWORD] +........ [XDU.i]...... [XDU.v]
      XW.      [XWORD] <16... 08*[WORD] +......... [XW.i]....... [XW.v]
      XB.      [XWORD] <16... 16*[BYTE] +......... [XB.i]....... [XB.v]

    - In their Prefix, they must be in alphabetic order 
    
    - Etre, au sein des préfixes, rangées en ordre alphabétique 
    
     For numbers (FLOAT}{REEL} refer to: / Pour les nombres [FLOAT][REEL]
     -> http://en.wikipedia.org/wiki/IEEE_754-1985
    
    OS structures have NO Prefix but are written intierly in CAP_ITAL letters 
    Les structure de l'OS n'ont pas de préfixe mais s'écrivent entièrement en MAJ_USCULES

;;
______________________

; Datas initialisées:
______________________

; Alignement sur 128 bits
[<16 @INITIALISED_DATAS_128:

 ; [BYTE] 0/255
 @XB.i: B$ 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255

 ; [DWORD] Signed / Signé 0-2147483648/+2147483647
 @XDS.i: D$ 0-2147483648 0-2147483648 0-2147483648 0-2147483648

 ; [DWORD] Unsigned / Non-signé 0/4294967295
 @XDU.i: D$ 4294967295 4294967295 4294967295 4294967295

 ; [FLOAT]
 @XF.i: F$ 3.14 3.14 3.14 3.14

 ; [REEL]
 @XR.i: R$ 3.14 3.14

 ; [WORD] 0/65535
 @XW.i: W$ 65535 65535 65535 65535 65535 65535 65535 65535

]

; Alignement sur 64 bits
[<8 @INITIALISED_TABLES_64:
 @TABLE.Q.i: Q$ 0 # 8]

; [<8...@TABLE

; Alignement sur 64 bits
[<8 @INITIALISED_DATAS_64:

 ; [QWORD] 00/0_FFFF_FFFF_FFFF_FFFF
 @Q.i: Q$ 0_FFFF_FFFF_FFFF_FFFF

 ; [REEL]
 @R.i: R$ 3.14

]

; Alignement sur 32 bits
[<4 @INITIALISED_DATAS_32:

 ; [DWORD] Signed / Signé 0-2147483648/+2147483647
 @DS.i: D$ 0-2147483648

 ; [DWORD] Unsigned / Non-signé 0/4294967295
 @DU.i: D$ 4294967295

 ; [FLOAT]
 @F.i: F$ 3.14

 ; [DWORD] Flag
 @FL.i: D$ &TRUE ; [YES]/[NO]

 ; [DWORD] Flags mask / Flags masque 00/00_11111111111111111111111111111111
 @FLM.i: D$ 00_11111111111111111111111111111111

 ; [DWORD]
 @H.i: D$ &NULL

 ; [DWORD]
 @HDC.i: D$ &NULL

 ; [DWORD]
 @HCDC.i: D$ &NULL

 ; [DWORD] Signed-Unsigned / Signé-Non-signé 0/4294967295 Ou 0-2147483648/+2147483647
 @ID.i: D$ 4294967295

 ; [DWORD]
 @LP.i: D$ &NULL

]

; Alignement sur 32 bits
[<4 @INITIALISED_STRUCTURES_32:
 @STRUC.i:
 @DU.i1: D$ 0
 @DU.i2: D$ 2
 ;...
 @BU.ix: B$ 0]

; [<4...@STC.Xxx

; Alignement sur 32 bits
[<4 @INITIALISED_TABLES_32: ; BU. BS. WU. WS. DU. DS. F. FL. FM. Q. R. LP.
 @TABLE.BU.i:
 B$ 00
 B$ 01
 B$ 02
 B$ 03
 B$ 04
 B$ 05
 B$ 06
 B$ 07
 B$ 08
 B$ 09]

; [<4...TABLE.PX.Xxx

; Alignement sur 32 bits
[<4 @INITIALISED_BUFFER_32: ; BU. BS. WU. WS. DU. DS. F. FL. FM. Q. R. LP.
 @BUFFER.BU.i: B$ 255 # 16]

; [<4...BUFFER.PX.Xxx

; Alignement sur 32 bits Lp -> ASCII
[<4 @INITIALISED_STRINGS_ASCII_32:
 @STR.A.i: B$ "*******" EOS]

; Si le nom est changé ici, il faut aussi le changer dans DEBUGG.dll
[<4 STR.A.WindowClassName: B$ 'DEBUGG' EOS]

; [<4...STR.A.Xxx

; Alignement sur 32 bits Lp -> UNICODE
[<4 @INITIALISED_STRINGS_UNICODE_32:
 @STR.U.i: U$ "*******" EOS]

; [<4...STR.U.Xxx

; Alignement sur 16 bits
[<2 @INITIALISED_DATAS_16:

 ; [WORD] Signed / Signé 0-32768/32767
 @WS.i: W$ 0-32767

 ; [WORD] Unsigned / Non-signé 0/65535
 @WU.i: W$ 65535

]

; Alignement sur 8 bits
[@INITIALISED_DATAS_8:

 ; [BYTE] Signed / Signé 0-128/+127
 @BS.i: B$ 0-128

 ; [BYTE] Unsigned / Non-signé 0/255
 @BU.i: B$ 255

]
____________________________________________________________________________________________
____________________

; Datas virtuelles:
____________________

; Alignement sur 128 bits
[<16 @VIRTUAL_DATAS_128:

 ; [BYTE] 0/255
 @XB.v: B$ ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ?

 ; [DWORD] Signed / Signé 0-2147483648/+2147483647
 @XDS.v: D$ ? ? ? ?

 ; [DWORD] Unsigned / Non-signé 0/4294967295
 @XDU.v: D$ ? ? ? ?

 ; [FLOAT]
 @XF.v: F$ ? ? ? ?

 ; [REEL]
 @XR.v: R$ ? ?

 ; [WORD] 0/65535
 @XW.v: W$ ? ? ? ? ? ? ? ?

]

; Alignement sur 64 bits
[<8 @VIRTUAL_TABLES_64:
 @TABLE.Q.v: Q$ ? # 8]

; [<4...@TABLE


; Alignement sur 64 bits
[<8 @VIRTUAL_DATAS_64:

 ; [QWORD] 00/0_FFFF_FFFF_FFFF_FFFF
 @Q.v: Q$ ?

 ; [REEL]
 @R.v: R$ ?

]

; Alignement sur 32 bits
[<4 @VIRTUAL_DATAS_32:

 ; [DWORD] Signed / Signé 0-2147483648/+2147483647
 @DS.v: D$ ?

 ; [DWORD] Unsigned / Non-signé 0/4294967295
 @DU.v: D$ ?

 ; [FLOAT]
 @F.v: F$ ?

 ; [DWORD] Flag &TRUE/&FALSE [YES]/[NO]
 @FL.v: F$ ?

 ; [DWORD] Flags masque 00/00_11111111111111111111111111111111
 @FLM.v: D$ ?

 ; [DWORD]
 @H.v: D$ ?

 ; E:
 H.Edit: ?

 ; F:
 H.Font: ?

 ; I:
 H.Instance: D$ ?

 ; W
 H.Window: D$ ?

 ; [DWORD]
 @HDC.v: D$ ?

 ; [DWORD]
 @HCDC.v: D$ ?

 ; [DWORD] Signed-Unsigned / Signé-Non-signé 0-2147483648/+2147483647 ou 0/4294967295
 @ID.v: D$ ?

 ; C:
 ID.ClipBoardFormat: D$ ?

 ; [DWORD]
 @LP.v: D$ ?

]

; Alignement sur 32 bits
[<4 @VIRTUAL_STRUCTURES_32:
 @STRUC.v:
 @DU.v1: D$ ?
 @DU.v2: D$ ?
 ;...
 @BU.vx: B$ ?]

; [<4...@STC.Xxx

; Alignement sur 32 bits
[<4 @VIRTUAL_TABLES_32: ; BU. BS. WU. WS. DU. DS. F. FL. FM. Q. R. LP.
 @TABLE.BU.v:
 B$ ? # 10]

; [<4...TABLE.PX.Xxxx

; Alignement sur 32 bits
[<4 @VIRTUAL_BUFFER_32: ; BU. BS. WU. WS. DU. DS. F. FL. FM. Q. R. LP.
 @BUFFER.BU.v: B$ ? # 16]

; [<4...BUFFER.PX.Xxx

; Alignement sur 32 bits chaînes ASCII
[<4 @VIRTUAL_STRINGS_ASCII_32:
 @STR.A.v: B$ ? # &MAX_PATH]

; [<4...STR.A.Xxxx

; Alignement sur 32 bits chaînes UNICODE
[<4 @VIRTUAL_STRINGS_UNICODE_32:
 @STR.U.v: U$ ? # &MAX_PATH]

; [<4...STR.U.Xxxx

; Alignement sur 16 bits
[<2 @VIRTUAL_DATAS_16:

 ; [WORD] Signed / Signé 0-32768 /+32767
 @WS.v: W$ ?

 ; [WORD] Unsigned / Non-signé 0/65535
 @WU.v: W$ ?

]

[@VIRTUAL_DATAS_8:

 ; [BYTE] Signed / Signé 0-128/+127
 @BS.v: B$ ?

 ; [BYTE] Unsigned / Non-signé 0/255
 @BU.v: B$ ?

]
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
TITLE CONVENTIONS     ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

@CONVENTIONS:

;;

    Displacements marks/ Balises de déplacements:
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        TODO <- Double-Click Left and "Search from Top"in order to jump to the points being developped
                Double clic gauche et "Search from Top" pour sauter aux points en cours de dev.

        [-Xxxxx-] <- Right Click to reach the explanation
                     Right clic pour atteindre l'explication

        [Xxxx] <- Right Click to reach the Label
                  Right clic pour atteindre le Label

        Arguments <- Macro (Mmmm): Right Click to reach the Macro Declarationou
                                 : Clic droit pour atteindre la déclaration de la Macro

        If, Mov, Call, Move <- Macro (Mmmm): Double Click to Unfold the Code Created by the Macro
                                           : Double-clic pour Unfold le code substitué par la Macro 

        &WM_SETFOCUS <- Right Click to Show the Hex/Dec Value of the Equate
                        Clic droit pour afficher la valeur Hex/Dec de l'Equate

        eax test movq <- Right Click to Show integrated Help
                         Clic droit pour afficher l'aide intégrée

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Labels et @Labels:
    ~~~~~~~~~~~~~~~~~

        In order to increase (if necessary)the legibility of your loops and keep the jumps short,
        use reserved local labels S0 ~ S9 and L0 to L9(pourquoi difference)and add to them ,
        on the same line , a Localized Global Label:  @MyLocalLabel.

        Pour augmenter (si besoin) la lisibilité de vos boucles et maintenir des sauts courts,
        utilisez les labels locaux réservés S0 ~ S9 et L0 à L9 et adjoignez-leur, sur la même
        ligne, un Label global localisé: @MyLocalLabel

        L0:@DO: ; ou  @DO:L0:

                ...

                ...

                sub ecx DWORD ; [-DWORD-]

            test ecx ecx | jns L0< ; jns @DO generate a long jump equivalent to jns L0<<
                                   ; jns @DO génère un saut long équivalent à jns L0<<

        !!! ALWAYS watch the Orphan list and KEEP IT EMPTY
        !!! Dans tous les cas, veillez à conserver l'Orphans List constament vide

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Diverse Typing rules/ Typos diverses:
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        "!!!" PAY ATTENTION: Important Remark / Attention, remarque importante

        Macros evocation:
        - Mov eax 45, Call, If, Comp:
          All Macros invocations have a CAPITAL First Letter        

        Macro évocation:
        - Mov eax 45, Call, If, Comp:
          Toutes les invocations de Macros ont une majuscule

        eax cmp test -> Right-clic open BUAsm
        Comp Call Push -> Right-clic Unfolding
        
        eax cmp test -> Clic-droit ouvre BUAsm
        Comp Call Push -> Clic-droit Unfold la Macro
         
    
        EQUATE ALWAYS in CAPITAL LETTERS / EQUATE toujours en majuscules

        '_' is allowed in Equates: MY_EQUATE @MY_EQUATE
        '_' autorisé dans les Equates: MY_EQUATE @MY_EQUATE 

        NO  '_' in the datas's names: MyData  OR My.Data but NOT My_Data NOR my_data
        Pas '_' dans les noms de datas: MaData ou Ma.Data mais pas Ma_Data ou ma_data

        Always : PREFIX.NameOfMyVariable @PREFIX.NameOfMyVariable Label@PREFIX.NameOfMyVariable
        NEVER: NameOfMyVariable @NameOfMyVariable Label@NameOfMyVariable
        * see [-PREFIXES-]

        Toujours: PREFIX.NomDeMaVariable @PREFIX.NomDeMaVariable Label@PREFIX.NomDeMaVariable
        Jamais: NomDeMaVariable @NomDeMaVariable Label@NomDeMaVariable (sans préfixe)
        * voir [-PREFIXES-]

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Separation and comments / Séparations et commentaires:
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        TITLE Commun: XXXXXXXX

             ; Version A.Bvvv DD.MM.YY maintainer email   
 TITLE DATAS ; Version A.Bxxx DD.MM.YY Nom(s) mainteneur(s) adresse(s) de courriel
____________________________________________________________________________________________

; ...
____________________________________________________________________________________________

        TITLE Spécifique: XxxxxxXxxx

                  ; Version A.Bvvv DD.MM.YY maintainer email
 TITLE MainWindow ; Version A.Bxxx DD.MM.YY Nom(s) mainteneur(s) adresse(s) de courriel
____________________________________________________________________________________________

;; 

    Global multilangual explanation of the architecture and workings of diverse mechanisms
    implemented in the specific TITLE

    Explication globale et multilingue de l'architecture et du fonctionement des divers
    mécanismes implémentés dans le TITLE Commun:

    - Use of module present in the TITLE, what it does, who for, why, how
    - Utilité du module présent dans le TITLE, ce qu'il fait, pour qui, pourquoi, comment

    - Simplified tree / Arborescence simplifiée:

    [XxxxA0]
    |   
    |--->[XxxxB0]
    |     |
    |     |--->[XxxxB1] 
    |     |
    |     |--->[XxxxB2]
    |     |  
    |     |--->[XxxxB3]
    |     °     |
    |           |--->[XxxxB3.1]
    |           |    | The branch carries on outside the TITLE / La branche continue en dehors du TITLE
    |           |
    |           |--->[XxxxB3.2]
    | suite...  °    ° End of branch / fin de branche

;;  
____________________________________________________________________________________________

LabelX:

    Code

L1: ...

        Code

    jmp L1<

L2: ...

        Code

L3:     ...

            Code

        jmp L3<

   jmp L2<
____________________________________________________________________________________________

LableY:

    Code

ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT

 (End Of Title)
    _________________________________

    ; Comments for code section
    ; Commentaire de section de code
    _________________________________

    ; Local comment on the line above the code / Commentaire local sur la ligne au dessus du code
    mov eax 45 | xor edx edx | div..

    ; Global comment Call / Commentaire global Call
    Call 'MODULE.Api' P1,
                      P2, ; Specific comment for P2 / Commentaire spécifique sur P2
                      ...
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Structuration:
    ~~~~~~~~~~~~~

LabelRoutine:

    [If] A
 
        If A1

            Call TraitementA1 ; !!! NEVER any Code / Jamais de code

        [Else_If] A2

            Call TraitementA2

        [Else]
        
            [ExitR]

        [End_If]

    Else_If B

        Call TraitementB

    Else_If X

        !!! NEVER any Code / Jamais de code

    End_If

[RetR]   
     ______________________________________ F8

TraitementB:

    If B1

        Call TraitementB1

    Else_If X

        !!! NEVER any Code / Jamais de code

    Else_If B2

        Call TraitementB2

    Else_If B3

        Call TraitementB3

    End_If
    _________________________________________________________________

    ; Identation illustration, here with local labels, A "spaghetti"
    ; construct a little complicated without comments
 
    ; Illustration d'indentations, ici avec Labels localisés, d'une
    ; imbrications spaghetti un peu compliquée et non commentée
    _________________________________________________________________

L1: @Debut:

        Code

        [Comp] eax edx je L1<,
                          js S1>,
                          jz S2>>

    sub ecx [DWORD] | jns L1<
 
        Code

        L2: @OK

                mov ecx 16

S1: @OK sub edx 1 | jnz L1<

            sub edx 1 | jnz L2<

S2: @Fin

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOF

; (End Of File)

