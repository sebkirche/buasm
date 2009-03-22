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

    &1   -> Size of arguments / Taille des arguments

    &2   -> Size of local variables and structures / Taille des variables locales et des structure

    &3   -> String of register or variable saved / Chaîne de registre ou variables préservées

    &4   -> Structure name / Nom de la structure

    &5   -> Structure offset counter / Compteur d'offsets de structure

    &6   -> Constructor of strings for debugg.dll macros / Constructeur de Châines pour les Macro de debugg.dll

    &&1  -> Macros counter / Compteur de Macros [If][Else]
    &&2  -> TODO traduction / Compteur d'imbrications

    M&&1 -> Counter of the times this macro was used. This is used to generate proper label names M0 to M9
    M&&1 -> Compteur d'emboîtements, utilisés pour la création des labels M0..M9, N0..N9

    O&&2 -> For the construction of the labels [Repeat][Until] [While][End_While] 
            Pour la construction des labels [Repeat][Until] [While][End_While]

;;
_______________

; Basic macros
_______________

;;

    Description : Macros that Move data around
                  Macros de déplacement des données


    Examples:

        Mov eax 45 | Mov D$edi '0000'

        [Mov] eax D$eax, B$MyVar 0_7F, eax MyLp

        Mov eax {ARGB 0_C1 0_C2 0_C3 0_A0}

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

[ARGB

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

    Call #1]

[CCall

    push #L>2

    Call #1

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

    #If &&3=> '0'               ; Check if while, repeat Macros are paired
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

    #Else

        cmp #1 #2 | JmpIf #3>L

    #EndIf]

;;
    
    Conditional execution / Exécution conditionelle
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

[IncCnt1

    #If &&1=0       ; Zero the counter / RAZ compteur l'ors de la première évocation de la macro

        &&1= '0'

    #Else

        &&1=&&1+1   ; Can be incremented up to 9 / Incrément jusqu'à '9'

    #EndIf]

[If.

    cmp #1 #3 | jn#2 M&&1>

    #+3]

[If
    IncCnt1

    If. #1>L]

[Else_If

    jn#2 N&&1>

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
        Mov eax 1999 | OutPutDec eax | OutPutStringLiteral CR LF

        ; Affiche le nom de du registre et son contenu au format hexadecimal
        Mov eax 0_ABCDEF | OutPutHex eax | OutPutStringLiteral CR LF

        ; Affiche le nom de la variable [DWORD] non signé et son contenu au format hexadecimal
        OutPutHex D$DU.Variable | OutPutStringLiteral CR LF

        ; Affiche le nom du registre et son contenu signed au format decimal
        Mov eax 0-32000 | OutPutSignedDec eax | OutPutStringLiteral CR LF

        ; Idem
        Mov eax 0FFFF_FFFF | OutPutSignedDec eax | OutPutStringLiteral CR LF

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

; Size equates : Mov eax D$esi+(2*DWORD),
;                     cl B$ah+(3*BYTE)

; Equates d'offsets taille: Mov eax D$esi+(2*DWORD),
;                           cl B$ah+(3*BYTE)
______________________________________________________________________

[BYTE     1  ; 0-128/+127 or 0/255 00/00_FF
 ASCII    1  ; Need the A suffix in some APIS (See also Tools menu -> Ascii Table)
 DWORD    4  ; 0-2147483648/+2147483647 or 0/4294967295 00/00_FFFF_FFFF
 FLOAT    4  ; http://en.wikipedia.org/wiki/IEEE_754-1985
 QWORD    8  ; 00/00_FFFF_FFFF_FFFF_FFFF
 REEL     8  ; http://en.wikipedia.org/wiki/IEEE_754-1985
 UNICODE  2  ; Need the W suffix in some APIs
 WORD     2  ; 0-32768/32767 ou 0/65535 00/00_FFFF
 XWORD   16] ; For SSE
______________________

; Equates for strings

; Equates chaînes
______________________

[EOS      0_0           ; End of string (BYTE)
 SPC      0_20          ; Space (BYTE)
 TAB      0_9           ; Tabulation (BYTE)
 CR       0_D           ; Carriage return (BYTE)
 CRCR     0_D_0D        ; Double carriage return (WORD)
 LF       0_A           ; Line feed (BYTE)
 LFLF     0_A_0A        ; Double line feed (WORD)
 CRLF     0_A_0D        ; Carriage return + line feed (WORD)
 LFCR     0_D_0A        ; Line feed + carriage return (WORD)
 CRLF2    0_A_0D_0A_0D  ; Double carriage return + line feed (DWORD)
 CRLFEOS0 0_A_0D_00     ; Carriage return + line feed + EOS + 0 (DWORD)
 MLC      0_D_3B_3B_0A] ; MLC: Multi-Lines Comment: LF;;CR (DWORD)
___________________

; Equates for HLL

; Equates pour HLL
___________________

@EQUATES_HLL:

[= e    < b     > a     =< be   <= be   => ae   >= ae   <> ne

        <s l    >s g    =<s le  <=s le  =>s ge  >=s ge
        s< l    s> g    s=< le  s<= le  s=> ge  s>= ge

 FIND_EOS z   NOT_EOS  nz ; End Of String Search
 ZERO     z   NOT_ZERO nz ; Numérique
 NULL     z   NOT_NULL nz ; Lp
 FALSE    z   TRUE     nz ; Flags
 ODD      po  EVEN     pe ; Parité Impair/Pair
 NEGATIVE s   POSITIVE ns]; Sign
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

; MINMAXINFO Structure Equates Windows
[MINMAXINFO.ptReserved     (0*DWORD)
 MINMAXINFO.ptMaxSize      (2*DWORD)
 MINMAXINFO.ptMaxPosition  (4*DWORD)
 MINMAXINFO.ptTrackSize    (6*DWORD)
 MINMAXINFO.ptMaxTrackSize (8*DWORD)]
_______________

; Flag equates
_______________

[NA         0-1         ; 0-1 00_11111111_11111111_11111111_11111111 BYTE/WORD/DWORD
 NO         0_FFFF_FFFF ; 0-1 00_11111111_11111111_11111111_11111111 DWORD
 YES        0+1]        ; 1   00_00000000_00000000_00000000_00000001 0_1 &TRUE BYTE/WORD/DWORD
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
TITLE KILL            ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

[STR.A.NotSourceInPE: B$ "No Source in this PE" EOS]
[STR.A.OpenTitleBuasmPE: B$ " Open BUAsm PE File" EOS]
[STR.A.PEFilesFilters: B$ "Executable Files" EOS "*.EXE" EOS
                          "Dynamic Link Library" EOS "*.DLL" EOS
                          "Drivers" EOS "*.SYS" EOS
                          "Object File" EOS "*.OCX" EOS
                          0]
[STR.A.ReadyPE: B$ "File ready for source destruction" EOS]

[STR.A.TextWindowInfo: B$ "This tool purpose is to retrieve the source section
from a BUAsm produced PE

- Your source will be saved aside the stripped version:
  
                      PE name + 'SAVE.asm'" EOS]
[STR.A.TitleWindowInfo: B$ " INFOS:" EOS]
[STR.A.WindowCaption: B$ "  BUAsm PE source cleaner" EOS]
[STR.A.WindowClassName: B$ "BUAsm" EOS]

[H.File: D$ ?
 H.Instance: D$ ?
 H.Menu: D$ ?
 LP.EndOfSectionSearch: D$ ?
 LP.FileMemory: D$ ?
 DU.FileSize: D$ ?
 DU.NakedPeLen: D$ ?
 DU.NumberOfBytes: D$ ?]

[STR.A.CustomFileFilter: B$ ? # &MAX_PATH]
[STR.A.File: B$ ? # &MAX_PATH]
[STR.A.FileCopyName: B$ ? # &MAX_PATH]
[STR.A.FileTitle: B$ ? # &MAX_PATH]

[STRUC.OPENFILENAME:
 @lStructSize: D$ len
 @hwndOwner: D$ &NULL
 @hInstance: D$ &NULL
 @lpstrFilter: D$ STR.A.PEFilesFilters
 @lpstrCustomFilter: D$ STR.A.CustomFileFilter
 @nMaxCustFilter: D$ &MAX_PATH
 @nFilterIndex: D$ 1
 @lpstrFile: D$ STR.A.File
 @nMaxFile: D$ &MAX_PATH
 @lpstrFileTitle: D$ STR.A.FileTitle
 @nMaxFileTitle: D$ &MAX_PATH
 @lpstrInitialDir: D$ &NULL
 @lpstrTitle: D$ STR.A.OpenTitleBuasmPE
 @Flags: D$ &OFN_LONGNAMES+&OFN_EXPLORER+&OFN_FILEMUSTEXIST+&OFN_PATHMUSTEXIST+&OFN_HIDEREADONLY
 @nFileOffset: W$ 0
 @nFileExtension: W$ 0
 @lpstrDefExt: D$ &NULL
 @lCustData: D$ 0
 @lpfnHook: D$ &NULL
 @lpTemplateName: D$ &NULL]

[STRUC.WINDOWCLASS:
 @style: D$ ?
 @lpfnWndProc: D$ ?
 @cbClsExtra: D$ ?
 @cbWndExtra: D$ ?
 @hInstance: D$ ?
 @hIcon: D$ ?
 @hCursor: D$ ?
 @hbrBackground: D$ ?
 @lpszMenuName: D$ ?
 @lpszClassName: D$ ?]

[ID_EDIT_KILL_RSC_PE 10]

[M00_Menu  2000
 M00_Open  2001
 M00_Exit  2002
 M00_Info  2003]

Proc MSG_PUMP:
    __________________________________________________

    ; Thread principale du process principal: Attente
    ; réception et routing des MSG des interfaces GUI
    __________________________________________________

    Structure @STRUC.MSG,
        DWORD .hwnd,
        DWORD .message,
        DWORD .wParam,
        DWORD .lParam,
        DWORD .time,
    (2*DWORD) .point ; [POINTX POINTY]

    ; Saute au début de la boucle
    jmp S1>
    ___________

    ; MSG loop
    ___________

    ; Envoyer MSG à la fenêtre concernée
L0: Call 'USER32.DispatchMessageA' @STRUC.MSG

        ; Attend l'arrivée d'un MSG (Wait on Event bloquant)
S1:     Call 'USER32.GetMessageA' @STRUC.MSG,
                                  &NULL,    ; NULL -> MSG Broadcast
                                  &WM_NULL, ; NULL -> Pas de filtrage mininum des MSG
                                  &WM_NULL  ; NULL -> Pas de filtrage maximum des MSG


    Test eax eax s> L0<, ; Eax > &WM_NULL
           NEGATIVE S0>  ; Eax = 0-1 -> MSG ERREUR
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

    ; [MAIN]

    Arguments @hwnd,
              @msg,
              @wParam,
              @lParam

    If D@msg = &WM_DESTROY

        Call 'USER32.PostQuitMessage' &NULL

    Else_If D@msg = &WM_COMMAND

        Call WM_COMMAND_MainWindow

    Else

        Call 'USER32.DefWindowProcA' D@hwnd,
                                     D@msg,
                                     D@wParam,
                                     D@lParam

        Return eax

    End_If

    xor eax eax

EndP

WM_COMMAND_MainWindow:

    If D$WPARAM = M00_Exit

        Call 'USER32.SendMessageA' D$HWND,
                                   &WM_CLOSE,
                                   &NULL,
                                   &NULL

    Else_If D$WPARAM = M00_Open

        Call 'COMDLG32.GetOpenFileNameA' STRUC.OPENFILENAME

        On eax = &TRUE call LoadFile

    Else_If D$WPARAM = M00_Info

        Call 'USER32.MessageBoxA' D$HWND,
                                  STR.A.TextWindowInfo,
                                  STR.A.TitleWindowInfo,
                                  &MB_OK+&MB_ICONINFORMATION

    End_If

ret
_________________________________________________________________________________________

LoadFile:

    Call 'KERNEL32.CreateFileA' STR.A.FileTitle,
                                &GENERIC_READ,
                                0,
                                0,
                                &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL,
                                &NULL

    Mov D$H.File eax

    Call 'KERNEL32.GetFileSize' D$H.File &NULL

    Mov D$DU.FileSize eax

    Call 'KERNEL32.GlobalAlloc' &GMEM_FIXED+&GMEM_ZEROINIT,
                                eax

    Mov D$LP.FileMemory eax

    Call 'KERNEL32.ReadFile' D$H.File,
                             eax,
                             D$DU.FileSize,
                             DU.NumberOfBytes,
                             &NULL

    Call 'KERNEL32.CloseHandle' D$H.File

    ; 376 first section header
    mov eax D$LP.FileMemory | add eax (376*BYTE) | mov esi eax

    ; (10*40) bytes per section
    add eax (10*40*BYTE) | mov D$LP.EndOfSectionSearch eax

L1: lodsd

        Comp eax '.src' = S1>

        add esi (36*ASCII)

    Comp esi D$LP.EndOfSectionSearch < L1<

    Call 'USER32.MessageBoxA' D$HWND,
                              STR.A.NotSourceInPE,
                              STR.A.TitleWindowInfo,
                              &MB_OK+&MB_ICONERROR

ret

S1: Move D$DU.NakedPeLen D$esi+(16*ASCII)

    lea edi D$esi-(20*ASCII) | Mov ecx ((40*ASCII)/DWORD) | xor eax eax | rep stosd

    Call 'USER32.MessageBoxA' D$HWND,
                              STR.A.ReadyPE,
                              STR.A.TitleWindowInfo,
                              &MB_OK+&MB_OKCANCEL+&MB_ICONQUESTION

    On eax = &IDOK Call DestroySource

    Call 'KERNEL32.GlobalFree' D$LP.FileMemory

ret
____________________________________________________________________________________________

DestroySource:

    Mov esi STR.A.FileTitle,
        edi STR.A.FileCopyName

    While B$esi <> EOS

        movsb

    End_While

    Mov D$edi EOS

    While B$edi <> '.'

        sub edi (1*ASCII)

    End_While

    Mov D$edi '.exe',
        D$edi+(4*ASCII) '.SAV'

    Call 'KERNEL32.CopyFileA' STR.A.FileTitle,
                              STR.A.FileCopyName,
                              &FALSE

    Call 'KERNEL32.CreateFileA' STR.A.FileTitle,
                                &GENERIC_WRITE,
                                0,
                                0,
                                &CREATE_ALWAYS,
                                &FILE_ATTRIBUTE_NORMAL,
                                &NULL

    Mov D$H.File eax

    Call 'KERNEL32.WriteFile' eax,
                              D$LP.FileMemory,
                              D$DU.NakedPeLen,
                              DU.NumberOfBytes,
                              &NULL

    Call 'KERNEL32.CloseHandle' D$H.File

    Mov D$H.File &NULL

    Call 'USER32.SendDlgItemMessageA' D$HWND,
                                      ID_EDIT_KILL_RSC_PE,
                                      &EM_SETMARGINS,
                                      &EC_LEFTMARGIN+&EC_RIGHTMARGIN,
                                      10

    Mov eax D$LP.FileMemory | add eax D$DU.NakedPeLen

    Call 'USER32.SetDlgItemTextA' D$HWND,
                                  ID_EDIT_KILL_RSC_PE,
                                  eax

ret
____________________________________________________________________________________________

INIT_MainWindow:

    Call 'KERNEL32.GetModuleHandleA' &NULL

    Mov D$H.Instance eax,
        D$STRUC.OPENFILENAME@hInstance eax,
        D$STRUC.WINDOWCLASS@hInstance eax

    Call 'USER32.LoadIconA' eax,
                            1

    Mov D$STRUC.WINDOWCLASS@hIcon eax

    Call 'USER32.LoadCursorA' &NULL,
                              &IDC_ARROW

    Mov D$STRUC.WINDOWCLASS@hCursor eax

    Mov D$STRUC.WINDOWCLASS@lpfnWndProc MainWindowProc,
        D$STRUC.WINDOWCLASS@lpszClassName STR.A.WindowClassName

    Call 'USER32.RegisterClassA' STRUC.WINDOWCLASS

    Call 'USER32.LoadMenuA' D$STRUC.WINDOWCLASS@hInstance,
                            M00_Menu

    Mov D$H.Menu eax

    Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  STR.A.WindowClassName,
                                  STR.A.WindowCaption,
                                  &WS_VISIBLE+&WS_OVERLAPPED+&WS_SYSMENU,
                                  &CW_USEDEFAULT,
                                  &CW_USEDEFAULT,
                                  &CW_USEDEFAULT,
                                  &CW_USEDEFAULT,
                                  &NULL,
                                  D$H.Menu,
                                  D$H.Instance,
                                  &NULL

    Mov D$STRUC.OPENFILENAME@hwndOwner eax

    Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  {B$ 'EDIT' EOS},
                                  &NULL,
                                  &WS_VISIBLE+&WS_CHILD+&ES_LEFT+&WS_VSCROLL+&ES_AUTOHSCROLL+&WS_MAXIMIZE+&ES_MULTILINE,
                                  &NULL,
                                  &NULL,
                                  &NULL,
                                  &NULL,
                                  eax,
                                  ID_EDIT_KILL_RSC_PE,
                                  D$H.Instance,
                                  &NULL

 ret
____________________________________________________________________________________________

MAIN:

    Call INIT_MainWindow

    Call MSG_PUMP

    Call 'KERNEL32.ExitProcess' eax
__________________________________________________________________________________________
____________________________________________________________________________________________
; EOT


