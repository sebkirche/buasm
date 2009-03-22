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
