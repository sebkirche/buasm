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

 ; [DWORD]
 @HDC.v: D$ ?

 ; [DWORD]
 @HCDC.v: D$ ?

 ; [DWORD] Signed-Unsigned / Signé-Non-signé 0-2147483648/+2147483647 ou 0/4294967295
 @ID.v: D$ ?

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
