TITLE VB              ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________
;;
  Special case of VB compiled PEs. The first Instructions of a VB PE are:
  
  > Push Vb6Header
  > Call 'MSVBVM50.064'
  
  The VB Header is a Structure that is saved in Code, and that discribes the
  organisation of the File:
  
  [VB6HEADER:|Vbsignature B 4|CompilerVersion W|SzLangDLL B 14|SzSecLangDLL B 14|
  RuntimeDLLVersion W|LanguageID D|BackupLanguageID D|
  >>> SubMain <<< D| >>> ProjectInfo <<< D|
  Flag2 W|Flag3 W|Flag4 D|ThreadSpace B|Const1 B|Flag5 W|Flag6 W|Flag7 W|FormCount W|
  ExternalComponentCount W|Flag8 B|Flag9 B|Flag10 W|DialogStructureGUI D|
  ExternalComponentTable D|Project D|ProjectExename D|ProjectTitle D|HelpFile D|
  ProjectName D|Flag11 D|Flag12 D|Flag13 D|Flag14 W
  
  
  Main Routine: 'MarkVbPe'
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

; VB6HEADER Structure (Displacements Equates):

[VBdis.VbsignatureDis 0
 VBdis.CompilerVersionDis 4
 VBdis.SzLangDLLDis 6
 VBdis.SzSecLangDLLDis 20
 VBdis.RuntimeDLLVersionDis 34
 VBdis.LanguageIDDis 36
 VBdis.BackupLanguageIDDis 40
 VBdis.SubMainDis 44
 VBdis.ProjectInfoDis 48
 VBdis.Flag2Dis 52
 VBdis.Flag3Dis 54
 VBdis.Flag4Dis 56
 VBdis.ThreadSpaceDis 60
 VBdis.Const1Dis 61
 VBdis.Flag5Dis 62
 VBdis.Flag6Dis 64
 VBdis.Flag7Dis 66
 VBdis.FormCountDis 68
 VBdis.ExternalComponentCountDis 70
 VBdis.Flag8Dis 72
 VBdis.Flag9Dis 73
 VBdis.Flag10Dis 74
 VBdis.DialogStructureGUIDis 76
 VBdis.ExternalComponentTableDis 80
 VBdis.ProjectDis 84
 VBdis.ProjectExenameDis 88
 VBdis.ProjectTitleDis 92
 VBdis.HelpFileDis 96
 VBdis.ProjectNameDis 100
 VBdis.Flag11Dis 104
 VBdis.Flag12Dis 108
 VBdis.Flag13Dis 112
 VBdis.Flag14Dis 116
 VB_HEADER_LEN 118]


; For flaging the 'SizesMap':

 [VBsizes:
  @Vbsignature: B$ BYTE+STRINGS #4
  @CompilerVersion: W$ WORD
  @SzLangDLL: B$ BYTE #14
  @SzSecLangDLL: B$ BYTE #14
  @RuntimeDLLVersion: W$ WORD
  @LanguageID: D$ DWORD
  @BackupLanguageID: D$ DWORD
  @SubMain: D$ DWORD
  @ProjectInfo: D$ DWORD
  @Flag2: W$ WORD
  @Flag3: W$ WORD
  @Flag4: D$ DWORD
  @ThreadSpace: B$ BYTE
  @Const1: B$ BYTE
  @Flag5: W$ WORD
  @Flag6: W$ WORD
  @Flag7: W$ WORD
  @FormCount: W$ WORD
  @ExternalComponentCount: W$ WORD
  @Flag8: B$ BYTE
  @Flag9: B$ BYTE
  @Flag10: W$ WORD
  @DialogStructureGUI: D$ DWORD
  @ExternalComponentTable: D$ DWORD
  @Project: D$ DWORD
  @ProjectExename: D$ DWORD
  @ProjectTitle: D$ DWORD
  @HelpFile: D$ DWORD
  @ProjectName: D$ DWORD
  @Flag11: D$ DWORD
  @Flag12: D$ DWORD
  @Flag13: D$ DWORD
  @Flag14: W$ WORD]


; For Flaging the 'RoutingMap':

[VBrouting:
 @Vbsignature: D$ EVOCATED+LABEL
 @CompilerVersion: W$ EVOCATED+LABEL
 @SzLangDLL: B$ 0 #14
 @SzSecLangDLL: B$ 0 #14
 @RuntimeDLLVersion: W$ EVOCATED+LABEL
 @LanguageID: D$ EVOCATED+LABEL
 @BackupLanguageID: D$ EVOCATED+LABEL
 @SubMain: D$ EVOCATED+LABEL+POINTER ; >>> Flag the pointed to Code later
 @ProjectInfo: D$ EVOCATED+LABEL
 @Flag2: W$ EVOCATED+LABEL
 @Flag3: W$ EVOCATED+LABEL
 @Flag4: D$ EVOCATED+LABEL
 @ThreadSpace: B$ EVOCATED+LABEL
 @Const1: B$ EVOCATED+LABEL
 @Flag5: W$ EVOCATED+LABEL
 @Flag6: W$ EVOCATED+LABEL
 @Flag7: W$ EVOCATED+LABEL
 @FormCount: W$ EVOCATED+LABEL
 @ExternalComponentCount: W$ EVOCATED+LABEL
 @Flag8: B$ EVOCATED+LABEL
 @Flag9: B$ EVOCATED+LABEL
 @Flag10: W$ EVOCATED+LABEL
 @DialogStructureGUI: D$ EVOCATED+LABEL+POINTER
 @ExternalComponentTable: D$ EVOCATED+LABEL
 @Project: D$ EVOCATED+LABEL
 @ProjectExename: D$ EVOCATED+LABEL
 @ProjectTitle: D$ EVOCATED+LABEL
 @HelpFile: D$ EVOCATED+LABEL
 @ProjectName: D$ EVOCATED+LABEL
 @Flag11: D$ EVOCATED+LABEL
 @Flag12: D$ EVOCATED+LABEL
 @Flag13: D$ EVOCATED+LABEL
 @Flag14: W$ EVOCATED+LABEL]


; ToDo: Implied SectionsMap Flags. Example, 'SubMain': At the DWORD >>> CODE

____________________________________________________________________________________________
____________________________________________________________________________________________

MarkVbPe:
    Mov eax D$DisEntryPoint | sub eax D$DisImageBase | add eax D$UserPeStart
    Mov esi D$eax+1 | sub esi D$DisImageBase | add esi D$UserPeStart
    On D$esi <> 'VB5!', ret

  ; Mark the VB Header Flags in the Tables. First, the 'SectionsMap' to Data:
    Mov edi esi | sub edi D$UserPeStart | add edi D$SectionsMap
    Mov ecx VB_HEADER_LEN, al DATAFLAG | rep stosb

  ; Routing Map according 'VB6HeaderRouting':
    Mov eax esi | sub eax D$UserPeStart | add eax D$RoutingMap
    Mov B$eax ACCESSED+LABEL
    Push esi
        Mov edi eax,  esi VBrouting, ecx VB_HEADER_LEN | rep movsb
    Pop esi

  ; Sizes Map according 'VB6HeaderSizes':
    sub eax D$RoutingMap | add eax D$SizesMap
    Push esi
        Mov edi eax,  esi VBSizes, ecx VB_HEADER_LEN | rep movsb
    Pop esi

    add esi VBdis.SubMainDis

    Mov eax D$esi | sub eax D$DisImageBase | add eax D$SectionsMap
    If eax < D$SectionsMap
        ; nop (There are VB PEs without any direct Code inside).
    Else_If eax < D$EndOfSectionsMap
        Mov B$eax CODEFLAG
        sub eax D$SectionsMap | add eax D$RoutingMap
        Mov B$eax INSTRUCTION+EVOCATED+LABEL+ACCESSED+PUSH_EBP
    End_If

    add esi VBdis.ProjectInfoDis ; >>> ProjectInfo Data

    Mov eax D$esi | sub eax D$DisImageBase | add eax D$SectionsMap
    If eax < D$SectionsMap
        ; nop (There are VB PEs without any direct Code inside).
    Else_If eax < D$EndOfSectionsMap
        Mov B$eax DATAFLAG
        sub eax D$SectionsMap | add eax D$RoutingMap
        Mov B$eax LABEL+ACCESSED
    End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
