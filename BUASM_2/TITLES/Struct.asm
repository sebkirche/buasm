TITLE Struct
____________________________________________________________________________________________
____________________________________________________________________________________________

; The Structures DialogBox ([Struct] Menu option):

[WinStructures: ?]
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
 Data in the .str Files are like this:

     [POINT:|x D|y D
     [POINTS:|x W|y W
     [RECT:|left D|top D|right D|bottom D

 For multi-units records:

     [FONTSIGNATURE:|fsUsb4 D 4|fsCsb2 D 2
                              ^          ^
 Nested Structures are expanded:

     [RBHITTESTINFO:|pt.x D|pt.y D|flags D|iBand D
                     ^^^^^^^^^^^^^
 Equates Values are given as this:

     [MIXERCONTROLDETAILS_LISTTEXTA:|dwParam1 D|dwParam2 D|szName < MIXER_LONG_NAME_CHARS
                                                                    ^^^^^^^^^^^^^^^^^^^^^
 (Without the '&').
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[StructHelp: 'Structures', 0]

StructDialog:
    .If B$StructuresFileOK = &TRUE
        If D$StructHandle = 0
            Call 'USER32.DialogBoxParamA' D$H.Instance, 18000, &NULL, StrucProc, &NULL
        Else
            Beep
        End_If

    .Else
        Call Help B_U_AsmName, IncludeFilesHelp, RosAsmHlpMessage

    .End_If
ret


[StructHandle: ?   StructComboHandle: ?    StructEditHandle: ?    StructTitleEditHandle: ?]

; Tag Dialog 18000

Proc StrucProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    ...If D@msg = &WM_INITDIALOG
        move D$StructHandle D@hwnd

        Call SetIconDialog

       ; Call 'USER32.SetWindowLongA' D@hwnd &GWL_EXSTYLE &WS_EX_TOOLWINDOW

        Call 'USER32.GetDlgItem' D@hwnd 10 | Mov D$StructComboHandle eax
        Call 'USER32.GetDlgItem' D@hwnd 11 | Mov D$StructEditHandle eax
        Call 'USER32.GetDlgItem' D@hwnd 30 | Mov D$StructTitleEditHandle eax
        Call InitStructListBox
        Call InitSerFormFlag
        Call 'USER32.GetDlgItem' D@hwnd 10
        Call 'USER32.SetFocus' eax
        jmp L8>>

    ...Else_If D@msg = &WM_COMMAND
        .If W@wParam = 21
            Mov D$StructHeadFlag NACKEDSTRUCT | Call ReBuildStructForm
        .Else_If W@wParam = 22
            Mov D$StructHeadFlag LOCALSTRUCT | Call ReBuildStructForm
        .Else_If W@wParam = 23
            Mov D$StructHeadFlag SEMISTRUCT | Call ReBuildStructForm
        .Else_If W@wParam = 24
            Mov D$StructHeadFlag DASHSTRUCT | Call ReBuildStructForm
        .Else_If W@wParam = 40
            movzx eax W@wParam | Call SetFormFlags | Call ReBuildStructForm
        .Else_If W@wParam = 41
            movzx eax W@wParam | Call SetFormFlags | Call ReBuildStructForm
        .Else_If W@wParam = 42
            movzx eax W@wParam | Call SetFormFlags | Call ReBuildStructForm
        .Else_If W@wParam = 50
            If B$ZeroOrQuestionMark = '?'
                Mov B$ZeroOrQuestionMark '0'
            Else
                Mov B$ZeroOrQuestionMark '?'
            End_If
           Call ReBuildStructForm
        .End_If

        Mov eax D@wParam | and D@wParam 0FFFF | shr eax 16
        .If eax = &CBN_SELCHANGE
            Call InitStructureName | Call BuildStructForm

        .Else_If eax = &EN_CHANGE
            If W@wParam = 30
                Call GetStructureUserName | Call ReBuildStructForm
            End_If

        .Else_If D@wParam = &IDCANCEL
            jmp L1>

        .Else_If D@wParam = &IDOK
            Call ClipStructure
L1:         Mov D$StructHandle 0
            VirtualFree D$WinStructures
            Call WM_CLOSE

        .Else_If D@wParam = &IDHELP
            Call Help, B_U_AsmName, StructHelp, ContextHlpMessage

        .Else_If eax = &BN_CLICKED
            Mov eax D@wParam

       .End_If

    ...Else_If D@msg = &WM_CTLCOLOREDIT
        jmp L1>

    ...Else_If D@msg = &WM_CTLCOLORLISTBOX
L1:     Call WM_CTLCOLOREDIT | Return

    ...Else
L8:     Return &FALSE

    ...End_If

    Mov eax &TRUE

L9: EndP

____________________________________________________________________________________________

[StructTitle: ? # 50] [StructTitleLen: ?]
[UserStructTitle: ? #50]
[StructEditText: ? #1000]

; Fill the ComboBox with all 'WinStructures' Data:

InitStructListBox:
    Call OpenStructureFile

    Mov esi D$WinStructures
    While B$esi <> 0
        lodsb
        If al = '['
            Mov edi StructTitle
L1:             lodsb | cmp al ':' | je L2>
                stosb | jmp L1<
L2:         Mov B$edi 0
            Call 'USER32.SendMessageA' D$StructComboHandle &CB_ADDSTRING 0 StructTitle
        End_If
    End_While
    Mov D$StructTitle 0
ret


[MenuID: ?    SeveralStructuresFiles: ?    StructuresFileHandle: ?    StructuresFileSize: ?]
[ApiFileString: MenuItemString: B$ ? #&MAX_PATH]

OpenStructureFile:
    .If B$StructuresFileOK = &FALSE
        Call Help, B_U_AsmName, IncludeFilesHelp, ContextHlpMessage | ret

    .Else_If B$SeveralStructuresFiles = &TRUE
        Mov esi EquatesName, edi MenuItemString
        While B$esi <> 0 | movsb | End_While
        dec edi
        While B$edi <> '.' | dec edi | End_While

L0:     dec edi | cmp B$edi '\' | je L1>
                  cmp B$edi ':' | je L1>
                  cmp edi MenuItemString | ja L0<
                    jmp L2>
L1:     inc edi
L2:     Mov ecx &MAX_PATH | add ecx MenuItemString | sub ecx edi
      ; Case when Call from the ToolBar
        On D$MenuID = 0, Mov D$MenuID 4001
        Call 'USER32.GetMenuStringA' D$H.Menu, D$MenuID, edi, ecx, &MF_BYCOMMAND

        Mov esi MenuItemString
        While B$esi <> 0 | inc esi | End_While | Mov D$esi '.str', B$esi+4 0

    .End_If

    Call 'KERNEL32.CreateFileA' MenuItemString &GENERIC_READ,
                                &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL, 0
    Mov D$StructuresFileHandle eax

    Call 'KERNEL32.GetFileSize' eax 0 | Mov D$StructuresFileSize eax

    VirtualAlloc WinStructures eax

    Mov D$NumberOfReadBytes 0
    Call 'KERNEL32.ReadFile' D$StructuresFileHandle, D$WinStructures,
                             D$StructuresFileSize, NumberOfReadBytes, 0

    Call 'KERNEL32.CloseHandle' D$StructuresFileHandle
ret

____________________________________________________________________________________________

InitStructureName:
    Call 'User32.SendMessageA' D$StructComboHandle &CB_GETCURSEL 0 0
    push eax
        Call 'User32.SendMessageA' D$StructComboHandle &CB_GETLBTEXT eax StructTitle
        Mov D$StructTitleLen eax
    pop eax
    Call 'User32.SendMessageA' D$StructComboHandle &CB_GETLBTEXT eax UserStructTitle
    Call 'User32.SendMessageA' D$StructTitleEditHandle &WM_SETTEXT 0 UserStructTitle
ret

____________________________________________________________________________________________

[OnModeHandle: ?   StructMode: ?] ; strucMode: 0 > Data    1 > Equates    2 > Stack Macro


InitSerFormFlag:
    Mov eax D$StructHeadFlag | add eax 21
    Call 'User32.SendDlgItemMessageA' D$StructHandle eax &BM_SETCHECK 1 0

    Mov eax D$StructMode | add eax 40
    Call 'User32.SendDlgItemMessageA' D$StructHandle eax &BM_SETCHECK 1 0
ret



SetFormFlags:
    Mov D$StructMode eax | sub D$StructMode 40

    Call 'User32.GetDlgItem' D$StructHandle eax | Mov D$OnModeHandle eax

    Call 'User32.GetDlgItem' D$StructHandle 40
    Call 'User32.SendMessageA' eax &BM_SETCHECK 0 0
    Call 'User32.GetDlgItem' D$StructHandle 41
    Call 'User32.SendMessageA' eax &BM_SETCHECK 0 0
    Call 'User32.GetDlgItem' D$StructHandle 42
    Call 'User32.SendMessageA' eax &BM_SETCHECK 0 0

    Call 'User32.SendMessageA' D$OnModeHandle &BM_SETCHECK 1 0
ret

____________________________________________________________________________________________


ReBuildStructForm:
    cmp B$StructTitle 0 | je L9>
        Call SetStructHeadText | Call BuildStructForm
L9: ret


BuildStructForm:
    Mov edi StructEditText, eax 0, ecx 1000 | rep stosd

  ; First, search the structure inside 'WinStructures' list same for all forms):
    Mov esi D$WinStructures, bl B$StructTitle, eax D$StructTitleLen

L1: inc esi
    While B$esi <> bl
        inc esi
    End_While
    cmp B$esi-1 '[' | jne L1<
    push esi
        Mov ecx eax
        Mov edi StructTitle | repe cmpsb
        Mov dl B$esi
    pop esi | jne L1<
    cmp dl ':' | jne L1<
    add esi eax | inc esi

    If D$StructMode = 0
        Call BuildStructDataForm
        sub edi 3 | Mov al ']' | stosb |  Mov B$edi 0
    Else_If D$StructMode = 1
       Call BuildStructEquForm
       sub edi 3 | Mov al ']' | stosb |  Mov B$edi 0
    Else_If D$StructMode = 2
       Call BuildStructStackForm
       sub edi 3 |  Mov B$edi 0
    End_If

    Call StripDoubleColon

    Call 'User32.SendMessageA' D$StructEditHandle &WM_SETTEXT 0 StructEditText
ret


StripDoubleColon:
    Mov esi StructEditText
    While B$esi > 0
        lodsb
        If al = ':'
            On B$esi = ':', Mov B$esi ' '
        End_If
    End_While
ret

[ZeroOrQuestionMark: '?']

BuildStructDataForm:
  ; Write the Structure main name in the EditBox:
    Mov edi StructEditText

    push esi
        Mov esi UserStructTitle
        Mov al '[' | stosb
        While B$esi <> 0
            movsb
        End_While
        Mov al ':' | stosb
    pop esi
    Mov al 13 | stosb | Mov al 10 | stosb | Mov al ' ' | stosb

; Write the items:
;
; [REBARBANDINFO:|cbSize D|fMask D|fStyle D|clrFore D|cl...
;                ^
    Call SetStructHeadText

L0: Call WriteStructHead
    inc esi                                           ; jmp over first '|'
    If B$esi+1 = '|'
        ; case of missing names: "[DDEUP:|D|D'
    Else_If B$esi+1 < ' '
        ; ... Same for: "[DDELN:|D"
    Else
        While B$esi <> ' '
        movsb
        End_While
        Mov al ':' | stosb
        inc esi
    End_If

; [REBARBANDINFO:|cbSize D|fMa...
;                        ^
    ..If B$esi+1 = '|'
L3:     If B$esi = 'B'
            Mov eax ' B$ '
        Else_If B$esi = 'W'
            Mov eax ' W$ '
        Else_If B$esi = 'D'
            Mov eax ' D$ '
        Else_If B$esi = 'Q'
            Mov eax ' Q$ '
        Else_If B$esi = 'F'
            Mov eax ' F$ '
        Else_If B$esi = 'U'
            Mov eax ' U$ '
        Else_If B$esi = 'T'
            Mov eax ' T$ '
        End_If
        stosd | Mov eax 0200A0D30 | Mov al B$ZeroOrQuestionMark | stosd
      ; 0200A0D30 =  '0' 13 10 ' '
        inc esi

    ..Else_If B$esi+1 = 13
        jmp L3<

    ..Else_If B$esi+1 = ' '
        add edi 2

L4:         dec edi | cmp B$edi 13 | jne L4<
L4:         dec esi | cmp B$esi-1 '|' | jne L4<
            On B$edi-1 = ']', dec edi
          ; No more need of Square Brackets for Multiple Data:
           ; Mov eax 05B0A0D5D | stosd              ;  05B0A0D5D = ']' 13 10 '['
           Mov W$edi CRLF, B$edi+2 ' ' | add edi 3
                Call WriteStructHead
                While B$esi <> ' '
                    movsb
                End_While
            Mov al ':' | stosb | inc esi

        If B$esi = 'B'
            Mov eax ' B$ '
        Else_If B$esi = 'W'
            Mov eax ' W$ '
        Else_If B$esi = 'D'
            Mov eax ' D$ '
        Else_If B$esi = 'U'
            Mov eax ' U$ '
        Else_If B$esi = 'Q'
            Mov eax ' Q$ '
        Else_If B$esi = 'F'
            Mov eax ' F$ '
        Else_If B$esi = 'T'
            Mov eax ' T$ '
        End_If

        stosd | dec edi | Mov eax ' 0 #' | stosd | Mov al B$ZeroOrQuestionMark, B$edi-3 al
        add esi 2
        If B$esi >= 'A'
            Mov al '&' | stosb  ; If it is a Win Equate instead of a value.
        End_If
        While B$esi <> '|'
            On B$esi = 13, jmp L5>
            movsb
        End_While
     ; No more need of Square Brackets for Multiple Data:
L5:   ;  Mov eax 05B0A0D5D | stosd   ; 05B0A0D5D =  ']' 13 10 '['
      Mov W$edi CRLF, B$edi+2 ' ' | add edi 3
        If B$esi <> '|'
          ; dec edi
        End_If
    ..Else
L6:     movsb | cmp B$esi '|' | je L7>
                cmp B$esi 13 | jne L6<
L7:     Mov al 13 | stosb | Mov al 10 | stosb | Mov al ' ' | stosb
    ..End_If

L9: cmp B$esi '|' | je L0<<
ret


[StructDisplacement: ?]

BuildStructEquForm:
  ; Write the Structure main name in the EditBox:
  ; esi is on 'WinStructures'
    Mov edi StructEditText, D$StructDisplacement 0

        Mov al '[' | stosb

BuildFromEquRoutine:           ; reused (called by 'BuildStructStackForm')

; Write the items:
;
; [REBARBANDINFO:|cbSize D|fMask D|fStyle D|clrFore D|cl...
;                ^
    Call SetStructHeadText

L0: Call WriteStructHead

    inc esi                                           ; jmp over first '|'

    .If B$esi+1 = '|'
        ; case of missing names: "[DDEUP:|D|D'
    .Else_If B$esi+1 < ' '
        ; ... Same for: "[DDELN:|D"
    .Else
        While B$esi <> ' '
            movsb
            If W$esi = '::'
                inc esi | jmp L1>
            End_If
        End_While
L1:     Mov eax 'Dis ' | stosd
    .End_If

    Mov eax D$StructDisplacement
  ; Destination String pointed by edi. eax holds the value to translate in Ascii Decimal.
    Mov dl 0FF | push edx                       ; Push stack end mark
    Mov ecx 10
L1: Mov edx 0
    div ecx | push edx | cmp eax 0 | ja L1<     ; Push remainders
L2: pop eax                                     ; Retrieve Backward
    cmp al 0FF | je L3>                         ; Over?
       add al '0' | stosb | jmp L2<             ; Write
L3: Mov al 13 | stosb | Mov al 10 | stosb | Mov al ' ' | stosb

  ; Cases of '::', as found above:
    If B$esi = ':'
        jmp L0<
    End_If

    inc esi

; [REBARBANDINFO:|cbSize D|fMa|cl... |bReserved < 2|dwDa...|rcChild RECT<>|rcBand RECT<>...
;                        ^
    ..If B$esi+1 = '|'
        If B$esi = 'B'
            add D$StructDisplacement 1
        Else_If B$esi = 'W'
            add D$StructDisplacement 2
        Else_If B$esi = 'D'
            add D$StructDisplacement 4                                   ; WinStructures
        Else_If B$esi = 'Q'
            add D$StructDisplacement 8
        Else_If B$esi = 'F'
            add D$StructDisplacement 4
        End_If
        inc esi

    ..Else_If B$esi+1 = ' '
      ; Cases of multiple Values (#n)
        Mov bl B$esi | add esi 2 | Mov ecx 0, eax 0
L4:     lodsb
        cmp al '|' | je L6>>
        cmp al '9' | ja L4>
        cmp al '0' | jb L5>>

                sub al '0'                  ; convert Decimal to binary:
                lea ecx D$ecx+ecx*4         ;     ecx = ecx * 5
                lea ecx D$eax+ecx*2         ;     ecx = eax + old ecx * 10
        jmp L4<
L4:
            Mov D$imm32 0
            pushad
                Mov edi DataLoopWinEquate
                ;mov al '&' | stosb |
                dec esi
                While B$esi > ' '
                    movsb | On B$esi = '|', jmp L4>
                End_While
L4:             Mov al 0 | stosb
                Mov esi DataLoopWinEquate
                Mov B$ShowWinEquateError &FALSE

                Call ReadWin32Equate | on B$EquateFound = &TRUE, Mov D$imm32 eax
            popad

            If B$EquateFound = &TRUE
                Mov ecx D$imm32
            Else
                Mov ecx 1
            End_If

            Mov B$ShowWinEquateError &TRUE, D$imm32 0
            While B$esi >= 'A'
                inc esi | On B$esi = '|', jmp L5>
            End_While
            If D$esi = ' + 1'
                inc ecx | add esi 4
            End_If

L5:   ; Error case

L6:     If bl = 'B'
            Mov eax 1
        Else_If bl = 'W'
            Mov eax 2
        Else
            Mov eax 4
        End_If
        mul ecx | add D$StructDisplacement eax | dec esi

    ..End_If

        While B$esi <> '|'
            On B$esi = 13, jmp L9>
            inc esi
        End_While

L9: cmp B$esi '|' | je L0<<
ret


BuildStructStackForm:

    Mov edi StructEditText | add edi 100 | Mov D$StructDisplacement 0

    Call BuildFromEquRoutine

    While B$esi <> '|'        ; Back one item to count the last one size
        dec esi
    End_While

    While B$esi <> ' '
        inc esi
    End_While
    inc esi

    .If B$esi+1 <= CR
        If B$esi = 'B'
            add D$StructDisplacement 1
        Else_If B$esi = 'W'
            add D$StructDisplacement 2
        Else_If B$esi = 'D'
            add D$StructDisplacement 4
        Else_If B$esi = 'Q'
            add D$StructDisplacement 8
        Else_If B$esi = 'F'
            add D$StructDisplacement 4
        End_If
        inc esi
;;
    .Else    ;_If B$esi+1 = ' '
  ; In fact(, it apears that, when the Structure is ended by a Table, this Table Length
  ; is already counted inside the D$StructDisplacement provided by 'BuildFromEquRoutine'.
        Mov bl B$esi | add esi 2 | Mov ecx 0
L4:     lodsb
        cmp al '9' | ja L5>
        cmp al '0' | jb L5>
        cmp al '|' | je L6>
                sub al '0'                  ; convert Decimal to binary:
                lea ecx D$ecx+ecx*4         ;     ecx = ecx * 5
                lea ecx D$eax+ecx*2         ;     ecx = eax + old ecx * 10
        jmp L4<
L5:     ; Error case
L6:     If bl = 'B'
            Mov eax 1
        Else_If bl = 'W'
            Mov eax 2
        Else
            Mov eax 4
        End_If
        mul ecx | add D$StructDisplacement eax | dec esi
;;

    .End_If

    Mov edi StructEditText, esi UserStructTitle  ; Write "Structure @NAME"
    Mov eax 'Stru' | stosd | Mov eax 'ctur' | stosd | Mov ax 'e ' | stosw | Mov al '@' | stosb
        While B$esi <> 0
            movsb
        End_While
        Mov al ' ' | stosb

    Mov eax D$StructDisplacement
    Align_On 4 eax                              ; Stack must remain aligned, whatever.

  ; Destination String pointed by edi. eax holds the value to translate in Ascii Decimal.
    Mov dl 0FF | push edx                       ; Push stack end mark
    Mov ecx 10
L0: Mov edx 0
    div ecx | push edx | cmp eax 0 | ja L0<     ; Push remainders
L2: pop eax                                     ; Retrieve Backward
    cmp al 0FF | je L9>                         ; Over?
       add al '0' | stosb | jmp L2<             ; Write
L9:
    Mov ax ', ' | stosw

    Mov esi StructEditText | add esi 100

    While B$esi <> 0
        lodsb
        If al = 13
            Mov al ','
        Else_If al = 10
            Mov al ' '
        End_If
        stosb
    End_While
ret

____________________________________________________________________________________________


[StructHeadText: ? #50]

WriteStructHead:
    push esi
        Mov esi StructHeadText
        While B$esi > 0
            movsb
        End_While
    pop esi
ret

[NACKEDSTRUCT 0    LOCALSTRUCT 1    SEMISTRUCT 2    DASHSTRUCT 3]

[StructHeadFlag: ?]

; Defining What text will (or none) be added at the begining of each symbol:

SetStructHeadText:
    pushad
        Mov edi StructHeadText

        If B$StructMode = 2
            Mov al '@' | stosb | jmp L1>
        End_If

        .If D$StructHeadFlag = NACKEDSTRUCT
            ; 0

        .Else_If D$StructHeadFlag = LOCALSTRUCT
            Mov al '@' | stosb

        .Else
L1:         Call GetStructureUserName
            Mov esi UserStructTitle
            While B$esi <> 0
                movsb
            End_While
            If D$StructHeadFlag = SEMISTRUCT
                Mov al '.' | stosb
            Else
                Mov al '_' | stosb
            End_If

        .End_If

        Mov al 0 | stosb
    popad
ret
____________________________________________________________________________________________

[UserStructName: ? #50]

GetStructureUserName:
    Call 'USER32.SendMessageA' D$StructTitleEditHandle &WM_GETTEXT 150 UserStructTitle
ret

____________________________________________________________________________________________

[ClipStructureMemory: ?]

ClipStructure:
    push D$BlockStartTextPtr, D$BlockEndTextPtr, D$BlockInside

        VirtualAlloc ClipStructureMemory 4000
        move D$BlockStartTextPtr D$ClipStructureMemory

        Call 'USER32.SendMessageA' D$StructEditHandle, &WM_GETTEXT, 4000, D$ClipStructureMemory

        If eax > 0
            add eax D$BlockStartTextPtr
            Mov B$BlockInside &TRUE, D$BlockEndTextPtr eax
            Call ControlC

            VirtualFree D$ClipStructureMemory
        End_If

L9: pop D$BlockInside, D$BlockEndTextPtr, D$BlockStartTextPtr
ret


