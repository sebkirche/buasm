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
    Mov eax 45 | xor edx edx | div..

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

                Mov ecx 16

S1: @OK sub edx 1 | jnz L1<

            sub edx 1 | jnz L2<

S2: @Fin

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOF

; (End Of File)









