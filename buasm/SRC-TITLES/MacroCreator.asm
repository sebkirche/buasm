TITLE MacroCreator    ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________
;;
MacroCreator

Created By Looki in October/2006

The goal of this routine is to substitute some code to Macros, recover the HLL style from
disassembled code, and clean the code from the various types of aligns

Current Version: 20/November/2006
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[If_MemComp | Mov #4 #3 | cmp #1 #4 | jn#2 F1>> | #+4]
[End_If_MemComp | F1: | F9:]
[Byter dl Wordr dx Dwordr edx]

[While_MultComp | N0: | cmp #1 #3 | jn#2 N9> | #+3]
[End_While_MultComp | jmp N0< | N9:]

[Buffer: B$ "    
Code02704D20: M8:
    cmp eax ebx | jne Code0403045
    Mov eax 0
    cmp eax ecx | jne Code0403040
    Mov eax 01
    cmp edx ecx | jne Code0403029
    Mov eax 02

Code0403029: H3:
    cmp ecx edx | jne Code040303B
    Mov eax 064
jmp Code0403029
    
Code040303B: J1:
    Mov eax 0C
    
Code0403040: J6:
    Mov eax 0D
    
Code0403045: K1:" EOS]
[FirstAdress: D$ ?]
[LastAdress: D$ ?]
[FirstAdressWithMacro: D$ ?]
[LastAdressWithMacro: D$ ?]
[EndOfCodeLine: D$ ?]
[Cache: D$ ? # 20]
[CodeSectName: D$ ? # 10]
____________________________________________________________________________________________

FindEndAdress:
    Mov eax Buffer
    While B$eax <> 0
        inc eax
    End_While
    Mov D$LastAdress eax
    ret
____________________________________________________________________________________________

ScanAndRealoc:
    Mov ebx 0
    While edx < D$LastAdress
        Mov bl B$edx
        Mov B$ecx bl
        inc edx
        inc ecx
    End_While
    Mov ebx ecx
    While ecx < D$LastAdress
        Mov D$ecx 0
        add ecx 4
    End_While
    Mov D$LastAdress ebx ; atualiza o ultimo endereço no handle principal
    ret
____________________________________________________________________________________________

InternalSubstitution:
            lea edx D$eax+25 ; endereço da ultima linha onde havia o código da macro
            Mov eax D$FirstAdressWithMacro ; pega o primeiro endereço onde reside a macro
                    ; Move os dados para o cache
                    Mov ecx D$eax
                    Mov D$Cache ecx
                    Mov ecx D$eax+4
                    Mov D$Cache+4 ecx
                    Mov ecx D$eax+8
                    Mov D$Cache+8 ecx
                    Mov ecx D$eax+12
                    Mov D$Cache+12 ecx
                    Mov cl B$eax+16
                    Mov B$Cache+16 cl
                    ; escreve o nome da macro antes do nome da seçao code
                    Mov D$eax 'Proc'
                    Mov cl SPC
                    Mov B$eax+4 cl
                    ; Move os do cache para a posiçao apos o nome da macro
                    Mov ecx D$Cache
                    Mov D$eax+5 ecx
                    Mov ecx D$Cache+4
                    Mov D$eax+9 ecx
                    Mov ecx D$Cache+8
                    Mov D$eax+13 ecx
                    Mov ecx D$Cache+12
                    Mov D$eax+17 ecx
                    Mov cl B$Cache+16
                    Mov B$eax+21 cl
                    lea ecx D$eax+22 ; endereço da ultima posiçao da linha do code, antes de pular a linha
                    Mov eax ecx ; endereço do final da linha code no handle principal
            ret
____________________________________________________________________________________________

MacroIfRecog:
;------------------------------------------------------------------------------------
;macro validada |"cmp eax ebx | jne Code0403045" |Code0403045: K1:|
;------------------------------------------------------------------------------------
                    Mov D$eax '..En'
                    Mov D$eax+4 'd_if'
                    lea edx D$eax+16 ; endereço da ultima linha onde havia o código da macro [fik apontado no Byte anterior ao 0D]
                    lea ecx D$eax+8 ; endereço da ultima posiçao da linha do code, antes de pular a linha
                    Mov eax ecx ; endereço do final da linha code no handle principal
                    Call ScanAndRealoc

                    Mov eax esi
                    Mov ecx Cache
;------------------------------------------------------------------------------------
;pega o primeiro parametro da macro
;------------------------------------------------------------------------------------
                    While B$eax <> SPC
                        Mov dl B$eax
                        Mov B$ecx dl
                        inc ecx
                        inc eax
                    End_While

                    Mov D$esi-4 '..If'
                    Mov B$esi SPC
                    inc esi
;------------------------------------------------------------------------------------
;prepara para receber o segundo parametro que indica o tipo de operação a se fazer
;------------------------------------------------------------------------------------
                    inc eax
                    Mov B$ecx SPC
                    inc ecx

                    Mov edi eax ; armazena endereço atual de eax [final do primeiro parâmetro] para procurar o segundo parametro

                    While W$eax <> 'jn'
                        add eax 2
                    End_While
;------------------------------------------------------------------------------------
;reconhece o segundo parametro
;------------------------------------------------------------------------------------
                    ; o lea não modifica a flag do jmp, isso garante que no caso de o código
                    ; já ter passado em uma comparação com resultado positivo ele vai para o L8
            cmp B$eax+2 'e' | jne L1> | Mov B$ecx '='| lea ecx D$ecx+1 | je L8>
        L1: cmp B$eax+2 'b' | jne L2> | Mov B$ecx '<'| lea ecx D$ecx+1 | je L8>
        L2: cmp B$eax+2 'a' | jne L3> | Mov B$ecx '>'| lea ecx D$ecx+1 | je L8>
        L3: cmp B$eax+2 'l' | jne L4> | Mov W$ecx '<s'| lea ecx D$ecx+2 | je L8>
        L4: cmp B$eax+2 'g' | jne L5> | Mov W$ecx '>s'| lea ecx D$ecx+2 | je L8>
        L5: cmp W$eax+2 'be' | jne L6> | Mov W$ecx '<='| lea ecx D$ecx+2 | je L8>
        L6: cmp W$eax+2 'ae' | jne L7> | Mov W$ecx '>='| lea ecx D$ecx+2 | je L8>
        L7: cmp W$eax+2 'ne' | jne L8> | Mov W$ecx '<>'| lea ecx D$ecx+2 | je L8>
        L8:
;------------------------------------------------------------------------------------
;volta ao fim do primeiro parametro e prepara para receber o terceiro parametro
;------------------------------------------------------------------------------------
                    Mov B$ecx SPC
                    Mov eax edi ; volta para o endereço do fim do primeiro parâmetro
                    inc ecx
;------------------------------------------------------------------------------------
;pega o terceiro parametro da macro
;------------------------------------------------------------------------------------
                    While B$eax <> SPC
                        Mov dl B$eax
                        Mov B$ecx dl
                        inc ecx
                        inc eax
                    End_While
;------------------------------------------------------------------------------------
;Monta os 3 parametros na macro
;------------------------------------------------------------------------------------
                    Mov edi Cache

                    While edi < ecx
                        Mov dl B$edi
                        Mov B$esi dl
                        inc edi
                        inc esi
                    End_While

                    lea edx D$eax+18 ; endereço da ultima linha onde havia o código da macro [fik apontado no Byte anterior ao 0D]
                    lea ecx D$eax+3 ; endereço da ultima posiçao da linha do code, antes de pular a linha
                    Mov eax ecx ; endereço do final da linha code no handle principal
                    Call ScanAndRealoc

                    add eax 4
                ret
____________________________________________________________________________________________

CreatorMain:

Call FindEndAdress
Mov D$FirstAdress Buffer
Mov eax D$FirstAdress

..While eax < D$LastAdress
;--------------------------------------------------------------------------------------------------
;Proc/EndP Recon Start
;--------------------------------------------------------------------------------------------------
    ...If D$eax = 'Code'
        Mov D$FirstAdressWithMacro eax ; pega o primeiro endereço onde reside a macro
        add eax 23
        ..If_And D$eax = 'Push', D$eax+4 = ' ebp', D$eax+14 = 'Mov ', D$eax+18 = 'ebp ', D$eax+21 = ' esp'
                Call InternalSubstitution
                Call ScanAndRealoc
        ..End_If
    ...End_If
;--------------------------------------------------------------------------------------------------
;Proc/EndP Recon End
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
;Loop Recon Start
;--------------------------------------------------------------------------------------------------
    ...If D$eax = 'cmp '

            add eax 4
            Mov esi eax ; salva o endereço atual do eax, local do primeiro parametro

;------------------------------------------------------------------------------------
;procura e pega o nome do label[seção]
;------------------------------------------------------------------------------------
            .While D$eax <> 'Code'
                inc eax
            .End_While

            Mov ebx CodeSectName ; guarda o nome do label para comparar

            .While B$eax <> 0a
                Mov dx W$eax
                Mov W$ebx dx
                add ebx 2
                add eax 2
            .End_While

            Mov D$ebx-1 ':'
            sub ebx 12
;------------------------------------------------------------------------------------
;parte da validação
;------------------------------------------------------------------------------------
            .While eax < D$LastAdress

                If_MemComp D$eax = D$ebx Dwordr, D$eax+4 = D$ebx+4 Dwordr, D$eax+8 = D$ebx+8 Dwordr
                    Call MacroIfRecog
                    jmp M1>
                End_If_MemComp

                ;If_And D$eax = 'jmp ', D$eax+4 = 'Code'
                inc eax

            .End_While
M1:
    ...End_If
;--------------------------------------------------------------------------------------------------
;Loop Recon End
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
;Align Recon Start
;--------------------------------------------------------------------------------------------------
    ...If D$eax = 'Mov '
        If_MemComp D$eax+3 = D$eax+7 Dwordr
            Mov D$eax 'Alig'
            Mov D$eax+4 'n 04'
            lea edx D$eax+11 ; endereço da ultima linha onde havia o código da macro [fik apontado no Byte anterior ao 0D]
            lea ecx D$eax+8 ; endereço da ultima posiçao da linha do code, antes de pular a linha
            Mov eax ecx ; endereço do final da linha code no handle principal
            Call ScanAndRealoc
        End_If_MemComp
    ...End_If
;--------------------------------------------------------------------------------------------------
    ...If D$eax = 'xchg'
        If_MemComp D$eax+4 = D$eax+8 Dwordr
            Mov D$eax 'Alig'
            Mov D$eax+4 'n 04'
            lea edx D$eax+12 ; endereço da ultima linha onde havia o código da macro [fik apontado no Byte anterior ao 0D]
            lea ecx D$eax+8 ; endereço da ultima posiçao da linha do code, antes de pular a linha
            Mov eax ecx ; endereço do final da linha code no handle principal
            Call ScanAndRealoc
        End_If_MemComp
    ...End_If
;--------------------------------------------------------------------------------------------------
    ...If D$eax = 'lea '
        If_MemComp W$eax+4 = W$eax+10 Wordr;, B$eax+6 = B$eax+12 Byter
            Mov D$eax 'Alig'
            Mov D$eax+4 'n 04'
            lea edx D$eax+13 ; endereço da ultima linha onde havia o código da macro [fik apontado no Byte anterior ao 0D]
            lea ecx D$eax+8 ; endereço da ultima posiçao da linha do code, antes de pular a linha
            Mov eax ecx ; endereço do final da linha code no handle principal
            Call ScanAndRealoc
        End_If_MemComp
    ...End_If
;--------------------------------------------------------------------------------------------------
    ...If D$eax = 'add '
        ..If B$eax+8 = '0'
            If_Or D$eax+4 = 'eax ', D$eax+4 = 'ebx ', D$eax+4 = 'ecx ', D$eax+4 = 'edx ',
                  D$eax+4 = 'edi ', D$eax+4 = 'esi ', D$eax+4 = 'ebp ', D$eax+4 = 'esp '
                Mov D$eax 'Alig'
                Mov D$eax+4 'n 04'
                lea edx D$eax+9 ; endereço da ultima linha onde havia o código da macro [fik apontado no Byte anterior ao 0D]
                lea ecx D$eax+8 ; endereço da ultima posiçao da linha do code, antes de pular a linha
                Mov eax ecx ; endereço do final da linha code no handle principal
                Call ScanAndRealoc
            End_If
        ..End_If
    ...End_If
;--------------------------------------------------------------------------------------------------
    ...If D$eax = 'nop ';, B$eax+2 = 'p'
            Mov D$eax 'Alig'
            Mov D$eax+4 'n 04'
;;
            lea edx D$eax+12 ; endereço da ultima linha onde havia o código da macro [fik apontado no Byte anterior ao 0D]
            lea ecx D$eax+8 ; endereço da ultima posiçao da linha do code, antes de pular a linha
            Mov eax ecx ; endereço do final da linha code no handle principal
            Call ScanAndRealoc
;;
    ...End_If
;--------------------------------------------------------------------------------------------------
    ...If_And D$eax = 'int ', B$eax+4 = '3'
            Mov D$eax 'Alig'
            Mov D$eax+4 'n 04'
;;
            lea edx D$eax+12 ; endereço da ultima linha onde havia o código da macro [fik apontado no Byte anterior ao 0D]
            lea ecx D$eax+8 ; endereço da ultima posiçao da linha do code, antes de pular a linha
            Mov eax ecx ; endereço do final da linha code no handle principal
            Call ScanAndRealoc
;;
    ...End_If
;--------------------------------------------------------------------------------------------------
;Align Recon End
;--------------------------------------------------------------------------------------------------

    inc eax

..End_While

Call 'KERNEL32.ExitProcess' &Null
____________________________________________________________________________________________

;;
Alinhamentos

;;falta fazer
nop
int 3

;;feito
Mov eax eax | Mov ebx ebx | Mov ecx ecx | Mov edx edx
Mov edi edi | Mov esi esi | Mov ebp ebp | Mov esp esp

xchg eax eax

add eax 0 | add ebx 0 | add ecx 0 | add edx 0
add edi 0 | add esi 0 | add ebp 0 | add esp 0

lea eax D$eax | lea ebx D$ebx | lea ecx D$ecx
lea edx D$edx | lea edi D$edi | lea esi D$esi
lea ebp D$ebp | lea esp D$esp
;;
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
