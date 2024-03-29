TITLE Clip
____________________________________________________________________________________________
____________________________________________________________________________________________
;                                     Templates.
;
; A simple bank of Templates to be copied inside the user ClipBoard trough a friendly
; Dialog interface ([Clip] Menu Option).

[GenericName: 0 #10]     ; 40 bytes for a name to add to symbols.

; 'TemplateChoice' 'AddGroup' 'DeleteGroup' 'AddTemplate' 'DeleteTemplate'
;
;
; Proc modified is: 'ShowClip'
; Additional procs added are: 'Clip_Disable_Customization_Controls' and
; 'Clip_Enable_Customization_Controls'
____________________________________________________________________________________________
____________________________________________________________________________________________

[ClipMessage: B$ " Copy 'Clip.txt' in this directory
or run [Config] menu option" EOS]

Templates:
    If D$ClipperHandle > 0
        Beep | ret
    End_If

    On eax = M00_Clip_File, jmp L1>

    If D$NumberOfClipFiles > 1
        Call LoadSelectedClipFile
    Else
L1:     Call LoadClipFile
    End_If

    If B$ClipFileReady = &TRUE
      ; Tag Dialog 2000
        Call 'USER32.DialogBoxParamA' D$H.Instance, 2000, &NULL, TemplateChoice, &NULL
        Call ReleaseClipFile
    Else_If B$ClipFileReady = &FALSE

        Call MessageBox FileNotFound,
                        ClipMessage,
                        &MB_SYSTEMMODAL+&MB_USERICON

    Else_If B$ClipFileReady = 0-1

        Call MessageBox UnvalidClipTitle,
                        KillIcon,
                        &MB_SYSTEMMODAL+&MB_ICONQUESTION+&MB_YESNO

        Call ReleaseClipFile
    End_If
ret


[ClipFileHandle: ?   ClipFileSize: ?    ClipFileMemoryPointer: ?    ClipMemoryEnd: ?
 ClipFileReady: ?]

[CLIPRESERVATION 010000]

LoadClipFile:
    Call 'KERNEL32.CreateFileA' ClipName, &GENERIC_READ, 0, 0, &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL, &NULL
    If eax = &INVALID_HANDLE_VALUE
        Mov B$ClipFileReady &FALSE | ret
    Else
        Mov B$ClipFileReady &TRUE, D$ClipFileHandle eax
    End_If

    Call 'KERNEL32.GetFileSize' D$ClipFileHandle, &NULL | Mov D$ClipFileSize eax

    add eax CLIPRESERVATION | VirtualAlloc ClipFileMemoryPointer, eax

    add eax D$ClipFileSize | add eax CLIPRESERVATION | Mov D$ClipMemoryEnd eax

    Call 'KERNEL32.ReadFile' D$ClipFileHandle, D$ClipFileMemoryPointer, D$ClipFileSize,
                            NumberOfReadBytes, &NULL
    Call ClipCRLFs
ret


LoadSelectedClipFile:
  ; eax stil hold the ID: 7000, 7001, ...
    sub eax 7000

    Mov esi ClipMenuStrings
    .While eax > 0
        While B$esi <> 0 | inc esi | End_While | inc esi
        dec eax
    .End_While

    Mov edi ClipName
    While B$edi <> 0 | inc edi | End_While
    While B$edi <> '\' | dec edi | End_While | inc edi
    While B$esi <> 0 | movsb | End_While | movsb

    Call LoadClipFile
ret


WriteClipFile:
    Call 'KERNEL32.CloseHandle' D$ClipFileHandle

    Call 'KERNEL32.CreateFileA' ClipName, &GENERIC_READ__&GENERIC_WRITE, 0, 0,
                                &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, &NULL
    Mov D$ClipFileHandle eax, D$NumberOfReadBytes 0

    Call 'KERNEL32.WriteFile' D$ClipFileHandle, D$ClipFileMemoryPointer, D$ClipFileSize,
                              NumberOfReadBytes, 0
ret


ReleaseClipFile:
    VirtualFree D$ClipFileMemoryPointer
    Call 'KERNEL32.CloseHandle' D$ClipFileHandle
ret


[OneClipName: ? #40]

ReadClipsections:
    Call 'USER32.SendMessageA' D$TemplateList1, &LB_RESETCONTENT, 0, 0

    Mov esi D$ClipFileMemoryPointer

    .While esi < D$ClipMemoryEnd
        lodsb
        If al = '/'
            .If B$esi = '/'
                ..If B$esi-2 < ' '
                    inc esi | Mov edi OneClipName
                    While B$esi >= ' '
                        movsb
                    End_While
                    Mov al 0 | stosb

                    Call 'USER32.SendMessageA' D$TemplateList1, &LB_ADDSTRING,
                                               0, OneClipName
                ..End_If
            .End_If
        End_If
    .End_While
ret


; in eax: 0 based index of the first ListBox (Sections)

[SectionPointer: ?]

SearchSection:
    inc eax | Mov ebx eax
    Mov esi D$ClipFileMemoryPointer

    .While esi < D$ClipMemoryEnd
        lodsb
        If al = '/'
            .If B$esi = '/'
                ..If B$esi-2 < ' '
                    dec ebx | jnz L1>
                        While B$esi <> LF
                            inc esi
                        End_While
                        inc esi | Mov D$SectionPointer esi | ret
L1:             ..End_If
            .End_If
        End_If
    .End_While

    Mov D$SectionPointer 0
ret


ReadClipTitles:
    Mov esi D$SectionPointer
    Mov edx D$ClipFileMemoryPointer | add edx D$ClipFileSize

    .While esi < edx ;D$ClipMemoryEnd
        lodsb
        If al = '/'
            .If B$esi-2 < ' '
                ..If B$esi = '/'
                    ret
                ..Else
                    Mov edi OneClipName
                    While B$esi >= ' '
                        movsb
                    End_While
                    Mov al 0 | stosb
                    push edx
                        Call 'USER32.SendMessageA' D$TemplateList2  &LB_ADDSTRING,
                                                   0 OneClipName
                    pop edx
                ..End_If
            .End_If
        End_If
    .End_While
ret


; in eax: 0 based index of the second ListBox (Titles)

[ClipPointer: ?   ClipGroupIndex: ?   ClipIndex: ?]

SearchTheClip:
    Mov D$TheClipLenght 0, D$ClipPointer 0
    Call 'USER32.SendMessageA' D$TemplateList2, &LB_GETCURSEL, 0, 0 | cmp eax &LB_ERR | je L9>>
    Mov D$ClipIndex eax
    Mov esi D$SectionPointer | inc eax | Mov ebx eax

    .While esi < D$ClipMemoryEnd
        lodsb
        If al = '/'
            .If B$esi-2 < ' '
                dec ebx | jnz L1>
                    While B$esi <> LF
                            inc esi
                    End_While

                    inc esi | Mov D$ClipPointer esi | Call SearchTheClipLenght | jmp L9>
L1:         .End_If
        End_If
    .End_While
L9: ret


[TheClipLenght: ?]

SearchTheClipLenght:
    .While esi < D$ClipMemoryEnd
        lodsb | inc D$TheClipLenght
        .If al = '/'
            If B$esi-2 < ' '
                dec D$TheClipLenght | ret
            End_If
        .Else_If al = 0
            dec D$TheClipLenght | ret
        .End_If
    .End_While
ret
____________________________________________________________________________________________


[TempoClipmemoryPtr: ?]
[ControlID108: ?] ; Handle to Generic Name textbox
[ControlID104: ?] ; These are the 4 handles to the radio buttons.
[ControlID105: ?]
[ControlID106: ?]
[ControlID107: ?]


ShowClip:
    Call RetrieveGenericName
    Call SearchTheClip | On D$TheClipLenght = 0, ret

    Mov ecx D$TheClipLenght | shl ecx 2               ; *4 > room for Generic Names
    push ecx
        VirtualAlloc TempoClipmemoryPtr ecx
        Mov edi D$TempoClipmemoryPtr, esi D$ClipPointer
    pop ecx
    shr ecx 2                                                   ; restore true data size

  ; If the very first Char after the first CR/LF is '@', we load the Clip 'As is':
    Mov ebx esi | add ebx 2
    While B$ebx <= ' ' | inc ebx | End_While

    .If B$ebx = '@'
        Call Clip_Disable_Customization_Controls
        Call SearchTheClip | On D$TheClipLenght = 0, ret
        Mov esi D$ClipPointer, ecx D$TheClipLenght
        While B$esi <> '@'
            inc esi | dec ecx | jz L8>
        End_While | inc esi | dec ecx | jz L8>
        rep movsb

    .Else_If B$WithData = &TRUE
        If B$GlobalScope = &TRUE
            Call WithDataGlobalScope
        Else
            Call WithDataLocalScope
        End_If
        Call Clip_Enable_Customization_Controls
    .Else
        If B$GlobalScope = &TRUE
            Call WithoutDataGlobalScope
        Else
            Call WithoutDataLocalScope
        End_If
        Call Clip_Enable_Customization_Controls
    .End_If

    Mov eax 0 | stosd

    Call 'USER32.SendMessageA' D$TemplateList3  &WM_SETTEXT 0 D$TempoClipmemoryPtr
L8:
ret


Clip_Disable_Customization_Controls:
   Call 'USER32.EnableWindow' D$ControlID108 &FALSE
   Call 'USER32.EnableWindow' D$ControlID104 &FALSE
   Call 'USER32.EnableWindow' D$ControlID105 &FALSE
   Call 'USER32.EnableWindow' D$ControlID106 &FALSE
   Call 'USER32.EnableWindow' D$ControlID107 &FALSE
ret

Clip_Enable_Customization_Controls:
   Call 'USER32.EnableWindow' D$ControlID108 &TRUE
   Call 'USER32.EnableWindow' D$ControlID104 &TRUE
   Call 'USER32.EnableWindow' D$ControlID105 &TRUE
   Call 'USER32.EnableWindow' D$ControlID106 &TRUE
   Call 'USER32.EnableWindow' D$ControlID107 &TRUE
ret

____________________________________________________________________________________________


[TemplateList1: ?    TemplateList2: ?    TemplateList3: ?]

[TPointer: ?    RadioButtonID: ?    ClipperHandle: ?]

[DEL_TEMPLATE 5    AD_TEMPLATE 6    DEL_GROUP 7   AD_GROUP 8   DEL_GROUP_TEMPLATES 4
 CLIP_MANAGER 25]

[ClipDialogIsExtended: ?]

ExtendClipDialog:
    Call 'USER32.GetWindowPlacement' D$ClipperHandle Control

    If B$ClipDialogIsExtended = &FALSE
        add D$Control.rcNormalPosition.bottom 66
    Else
        sub D$Control.rcNormalPosition.bottom 66
    End_If

    Call 'USER32.SetWindowPlacement' D$ClipperHandle Control

    xor B$ClipDialogIsExtended &TRUE
ret


Proc TemplateChoice:
    Arguments @hwnd, @msg, @wParam, @lParam

    ...If D@msg = &WM_COMMAND
         ..If D@wParam = &IDCANCEL
            Mov D$ClipperHandle 0
            Call WM_CLOSE

         ..Else_If D@wParam = &IDOK
             Call RetrieveGenericName
             Call SearchTheClip
             Call TemplateToClipBoard
             Mov D$ClipperHandle 0
             Call WM_CLOSE

         ..Else_If D@wParam = &IDHELP
             Call Help, B_U_AsmName TempateHelp, ContextHlpMessage

         ..Else_If D@wParam = CLIP_MANAGER
            Call ExtendClipDialog

         ..Else_If D@wParam = AD_GROUP
            Call AddGroup

         ..Else_If D@wParam = DEL_GROUP
            Call DeleteGroup

         ..Else_If D@wParam = DEL_GROUP_TEMPLATES
            Call DeleteGroupAndTemplates

         ..Else_If D@wParam = AD_TEMPLATE
            Call AddTemplate

         ..Else_If D@wParam = DEL_TEMPLATE
            Call DeleteTemplate

         ..Else_If W@wParam = 101                     ; 101 > First ListBox
             shr D@wParam 16
             .If D@wParam = &LBN_SELCHANGE
                Call ViewTemplatesItems
             .End_If

         ..Else_If W@wParam = 102                     ; 102 > Second ListBox
             shr D@wParam 16
             .If D@wParam = &LBN_SELCHANGE
                 Call ShowClip
             .End_If

         ..Else_If W@wParam = 108                   ; Generic Name Edit Control
            Call ShowClip
         ..Else_If W@wParam >= 104                   ; 104/105/106/107 > Radio Buttons
             move D$RadioButtonID D@wParam
             and D$RadioButtonID 0FFFF
             .If W@wParam <= 107
                 shr D@wParam 16
                 If D@wParam = &BN_CLICKED
                     Call CheckTemplateRadioButtons
                     Call ShowClip
                 End_If
             .End_If
         ..End_If

    ...Else_If D@msg = &WM_INITDIALOG
        move D$ClipperHandle D@hwnd
        Mov B$ClipDialogIsExtended &FALSE
        Call SetIconDialog
        Call 'USER32.GetDlgItem' D@hwnd 101 | Mov D$TemplateList1 eax
        Call 'USER32.GetDlgItem' D@hwnd 102 | Mov D$TemplateList2 eax
        Call 'USER32.GetDlgItem' D@hwnd 110 | Mov D$TemplateList3 eax
        Call 'USER32.GetDlgItem' D@hwnd 108 | Mov D$ControlID108 eax
        Call 'USER32.GetDlgItem' D@hwnd 104 | Mov D$ControlID104 eax
        Call 'USER32.GetDlgItem' D@hwnd 105 | Mov D$ControlID105 eax
        Call 'USER32.GetDlgItem' D@hwnd 106 | Mov D$ControlID106 eax
        Call 'USER32.GetDlgItem' D@hwnd 107 | Mov D$ControlID107 eax
        Call ReadClipsections
        Call InitTemplateRadioButtons D@hwnd
        Call 'USER32.SendMessageA' D$TemplateList1  &LB_SETCURSEL 0 0
        Call ViewTemplatesItems
       ; Call 'USER32.SendDlgItemMessageA' D@hwnd, 110, &WM_SETFONT, D$Font1Handle, &FALSE

    ...Else_If D@msg = &WM_CTLCOLOREDIT
        jmp L1>

   ; ...Else_If D@msg = &WM_CTLCOLORBTN ; Never received for Radio/Check Buttons !!!???
   ;     jmp L1>

    ...Else_If D@msg = &WM_CTLCOLORLISTBOX
L1:     Call WM_CTLCOLOREDIT | Return

    ...Else
       ; Mov eax D$BlockEndTextPtr | sub eax D$BlockStartTextPtr
       ; Mov ebx D$ClipMemoryEnd | sub ebx D$ClipFileMemoryPointer
       ; Mov ecx D$BlockInside | On eax >= ebx, Mov ecx &FALSE
       ; EnableControl D@hwnd, AD_TEMPLATE, ecx

        Return &FALSE

    ...End_If

    Mov eax &TRUE

L9: Endp


ViewTemplatesItems:
    Call 'USER32.SendMessageA' D$TemplateList2, &LB_RESETCONTENT, 0, 0
    Call 'USER32.SendMessageA' D$TemplateList3, &WM_SETTEXT, 0, 0

    Call 'USER32.SendMessageA' D$TemplateList1  &LB_GETCURSEL 0 0
    Call SearchSection | On D$SectionPointer <> 0, Call ReadClipTitles
ret

____________________________________________________________________________________________

[GetNewGroupNameHandle: ?]

AddGroup: ; 'LoadClipFile'
    Call SaveClipSelections

    On D$GetNewGroupNameHandle <> 0,
        Call 'User32.EndDialog' D$GetNewGroupNameHandle, 0
    Call 'USER32.DialogBoxParamA' D$H.Instance 20001, &NULL, GetNewGroupName, &NULL
    On D$NewGroupName = 0, ret

    ...If B$GroupInsert = &TRUE
      ; Go to the selected Group:
        Mov esi D$ClipFileMemoryPointer, edx D$ClipGroupIndex | inc edx

        While esi < D$ClipMemoryEnd
            lodsb
            ..If al = '/'
                .If B$esi = '/'
                    If B$esi-2 < ' '
                        dec edx | jz L1>
                    End_If
                .End_If
            ..End_If
        End_While

L1:     dec esi

        push esi
          ; Make room: ('D$NewGroupNameLen'+6) Bytes:
            Mov ecx D$ClipFileMemoryPointer | add ecx D$ClipFileSize
            Mov edi ecx     ; edi > End.
            sub ecx esi     ; ecx = How many Bytes to be moved downward.
            Mov esi edi | add edi D$NewGroupNameLen | add edi 8 ; 6 = 3 CRLF + '//'
          ; esi > End // edi > (End+room). Copy BackWard:
            inc ecx | std | rep movsb | cld
        pop edi

        add edi 2 ; (old '//' yet there).
        Mov esi NewGroupName, ecx D$NewGroupNameLen | rep movsb
        Mov D$edi CRLF2, W$edi+4 CRLF

        Mov eax D$NewGroupNameLen | add eax 8 | add D$ClipFileSize eax

    ...Else ; B$GroupInsert = &FALSE >>> Append:
        Mov edi D$ClipFileMemoryPointer | add edi D$ClipFileSize
        While B$edi < ' ' | dec edi | dec D$ClipFileSize | End_While
        inc edi | inc D$ClipFileSize
        Mov D$edi CRLF2, W$edi+4 CRLF, W$edi+6 '//' | add edi 8
        Mov esi NewGroupName
        While B$esi <> 0 | movsb | inc D$ClipFileSize | End_While
        Mov D$edi CRLF2 | add edi 4
        add D$ClipFileSize 12 ; (5 CRLF + '//')

    ...End_If

    Call ClipUpdate
ret


[GroupInsert: ?]

Proc GetNewGroupName:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

        .If D@msg = &WM_COMMAND
            If D@wParam = &IDOK         ; [Append]
                Mov D$GroupInsert &FALSE
L1:             Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &WM_GETTEXTLENGTH, 0, 0
                Mov D$NewGroupNameLen eax
                Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &WM_GETTEXT, 38,
                                                  NewGroupName
                Call WM_CLOSE | popad | Mov eax &TRUE | ExitP

            Else_If D@wParam = 20       ; [Insert]
                Mov D$GroupInsert &TRUE | jmp L1<

            Else_If D@wParam = &IDCANCEL
                Mov D$GetNewGroupNameHandle 0
                Call WM_CLOSE | popad | Mov eax &TRUE | ExitP

            End_If

        .Else_If D@msg = &WM_INITDIALOG
            Mov B$WithCustomisation &FALSE
            move D$GetNewGroupNameHandle D@hwnd
            Mov edi NewGroupName, eax 0, ecx 10 | rep stosd

            Call 'USER32.SendDlgItemMessageA' D@hwnd, 21,  &BM_SETCHECK, &TRUE, 0
            Call 'USER32.GetDlgItem', D@hwnd, 10
            Call 'USER32.SetFocus' eax

        .End_If

    popad | Mov eax &FALSE
EndP


; Deleting a Group name only.

[SureDelGroupName: B$ "Delete                                  " EOS]

DeleteGroup:
    Call SaveClipSelections

    Mov esi D$ClipFileMemoryPointer, ecx D$ClipGroupIndex | inc ecx

L0: inc esi | cmp D$esi 02F2F0A0D | jne L0<     ; "CR LF //" = 02F2F0A0D
    dec ecx | jnz L0<

    push esi
        add esi 4 | lea edi D$SureDelGroupName+8
        While B$esi > CR | movsb | End_While
        Mov D$edi '" ? ', B$edi+3 0

        Call MessageBox Sure,
                        SureDelGroupName,
                        &MB_SYSTEMMODAL+&MB_YESNO+&MB_ICONEXCLAMATION

    pop esi

    .If D$FL.MsgBoxReturn = &IDYES
        Mov edx D$ClipFileMemoryPointer | add edx D$ClipFileSize
        add esi 2 | Mov edi esi | add esi 2 | sub D$ClipFileSize 2
      ; edi > '//'. esi > Name. Skip over Line:
        While B$esi > CR | inc esi | dec D$ClipFileSize | End_While
      ; Skip over coming CRLF:
        While W$esi = CRLF | add esi 2 | sub D$ClipFileSize 2 | End_While
        While esi < edx | movsb | End_While

        Call ClipUpdate
    .End_If
ret


DeleteGroupAndTemplates:
    Call SaveClipSelections

    Mov esi D$ClipFileMemoryPointer, ecx D$ClipGroupIndex | inc ecx

L0: inc esi | cmp D$esi 02F2F0A0D | jne L0<     ; "CR LF //" = 02F2F0A0D
    dec ecx | jnz L0<

    push esi
        add esi 4 | lea edi D$SureDelGroupName+8
        While B$esi > CR | movsb | End_While
        Mov D$edi '" ? ', B$edi+3 0

         Call MessageBox Sure,
                        SureDelGroupName,
                        &MB_SYSTEMMODAL+&MB_YESNO+&MB_ICONHAND

    pop esi

    .If D$FL.MsgBoxReturn = &IDYES
        Mov edx D$ClipFileMemoryPointer | add edx D$ClipFileSize
        Mov edi esi | add esi 4 | sub D$ClipFileSize 4
        While D$esi <> 02F2F0A0D
            inc esi | dec D$ClipFileSize
            On esi >= edx, jmp L2>
        End_While

        While esi < edx | movsb | End_While

L2:     Call ClipUpdate
    .End_If
ret


[GetNewTemplateNameHandle: ?]

[NoClipSelection: B$ "No selection found in the Source Editor" EOS]

[SelectionTooBig: B$ "The Selection is too big" EOS]

AddTemplate: ; 'LoadClipFile'
    Mov eax D$BlockEndTextPtr | sub eax D$BlockStartTextPtr
    Mov ebx D$ClipMemoryEnd | sub ebx D$ClipFileMemoryPointer

    If D$BlockInside = &FALSE

        Call MessageBox Argh,
                        NoClipSelection,
                        &MB_SYSTEMMODAL+&MB_USERICON

        ret

    Else_If eax >= ebx

        Call MessageBox Argh,
                        SelectionTooBig,
                        &MB_SYSTEMMODAL+&MB_USERICON

        ret

    End_If

    Call SaveClipSelections

    On D$GetNewTemplateNameHandle <> 0,
        Call 'User32.EndDialog' D$GetNewTemplateNameHandle, 0
    Call 'USER32.DialogBoxParamA' D$H.Instance 20000, &NULL, GetNewTemplateName, &NULL

    On D$NewTemplateName = 0, ret

    Call SearchTheClip

    Mov eax D$ClipFileSize | add eax CLIPRESERVATION
    VirtualAlloc TempoClipFileMemoryPointer, eax

    If D$TheClipLenght = 0
      ; No Item yet, or no Item selected >>> paste at end:
        Mov ebx D$SectionPointer, edx D$ClipFileMemoryPointer | add edx D$ClipFileSize
L0:     inc ebx | cmp ebx edx | jae L1>
        cmp W$ebx '//' | jne L0<
L1:         Mov D$ClipPointer ebx

        Mov esi D$ClipFileMemoryPointer, edi D$TempoClipFileMemoryPointer
        While esi < D$ClipPointer | movsb | End_While

    Else
      ; Item selected >>> Insert:
        Mov esi D$ClipFileMemoryPointer, edi D$TempoClipFileMemoryPointer
        While esi < D$ClipPointer | movsb | End_While

L0:     dec esi | dec edi | cmp B$esi '/' | jne L0<
        If B$esi-1 = '/'
            dec esi | dec edi
        End_If
        cmp W$esi-2 CRLF | jne L0<

    End_If

    Mov B$edi '/' | inc edi | inc D$ClipFileSize
    push esi
        Mov esi NewTemplateName
        While B$esi <> 0 | movsb | inc D$ClipFileSize | End_While
    pop esi
    Mov D$edi CRLF2 | add edi 4 | add D$ClipFileSize 4

    If B$WithCustomisation = &FALSE
        Mov B$edi '@', W$edi+1 CRLF | add edi 3 | add D$ClipFileSize 3
    End_If

    Mov W$edi CRLF | add edi 2 | add D$ClipFileSize 2

    push esi
        Mov esi D$BlockStartTextPtr
        While esi < D$BlockEndTextPtr
            movsb | inc D$ClipFileSize
        End_While | movsb | inc D$ClipFileSize
        Mov D$edi CRLF2, W$edi+4 CRLF | add edi 6 | add D$ClipFileSize 6
    pop esi

    Mov edx D$ClipFileMemoryPointer | add edx D$ClipFileSize
    While esi < edx | movsb | End_While
    Exchange D$ClipFileMemoryPointer D$TempoClipFileMemoryPointer
    VirtualFree D$TempoClipFileMemoryPointer
    Call ClipUpdate
ret


SaveClipSelections:
    Call 'USER32.SendMessageA' D$TemplateList1  &LB_GETCURSEL 0 0 | Mov D$ClipGroupIndex eax
    Call 'USER32.SendMessageA' D$TemplateList2  &LB_GETCURSEL 0 0 | Mov D$ClipIndex eax
ret

ClipUpdate:
    Call ClipCRLFs | Call WriteClipFile | Call ReleaseClipFile
    Call LoadClipFile | Call ReadClipsections



    Call 'USER32.SendMessageA' D$TemplateList2, &LB_SETCURSEL, D$ClipIndex, 0
    If eax = &LB_ERR
        Call 'USER32.SendMessageA' D$TemplateList2, &LB_SETCURSEL, 0-1, 0
    End_If

    Call 'USER32.SendMessageA' D$TemplateList1, &LB_SETCURSEL, D$ClipGroupIndex, 0
    If eax = &LB_ERR
        Call 'USER32.SendMessageA' D$TemplateList1, &LB_SETCURSEL, 0, 0
    End_If

    Call ViewTemplatesItems
ret
____________________________________________________________________________________________

[UnvalidClipTitle: B$ "Unvalid clip file" EOS]

[ClipTitleWanted: B$ "A clip file must contain some text before the first //Group" EOS]

;;
  Ensure that all '/Name', '//Name' are preceeded and ended by 3 CRLF and than the
  whole thing is ended by 2 CRLF.
;;

ClipCRLFs:
    Mov esi D$ClipFileMemoryPointer
    While W$esi = CRLF | add esi 2 | End_While
    If B$esi = '/'
        Mov B$ClipFileReady 0-1 | ret
    End_If

    Mov eax D$ClipFileSize | add eax CLIPRESERVATION
    VirtualAlloc TempoClipFileMemoryPointer, eax

    Mov eax D$TempoClipFileMemoryPointer
    add eax D$ClipFileSize | add eax CLIPRESERVATION | Mov D$ClipMemoryEnd eax

    Mov esi D$ClipFileMemoryPointer, edi D$TempoClipFileMemoryPointer
    Mov edx esi | add edx D$ClipFileSize

L0: .While esi < edx
        lodsb

        .If al = '/'
            If W$esi-3 = CRLF
              ; Skip the CRLF Back:
                While W$edi-2 = CRLF | sub edi 2 | End_While
              ; Re-Write the wanted 3 CRLFs:
                Mov D$edi CRLF2, W$edi+4 CRLF | add edi 6
              ; Write the '/Name', '//Name':
                dec esi | While B$esi <> CR | movsb | End_While
              ; Write the wanted 3 CRLFs:
                Mov D$edi CRLF2, W$edi+4 CRLF | add edi 6
              ; Arase the existing source CRLF:
                While W$esi = CRLF | add esi 2 | End_While | jmp L0<
            End_If
        .End_If

        stosb
    .End_While

  ; Arase existing CRLF at End:
    While W$edi-2 = CRLF | sub edi 2 | End_While
  ; And rewrite the 2 wanted ones:
    Mov D$edi CRLF2 | add edi 4

    sub edi D$TempoClipFileMemoryPointer | Mov D$ClipFileSize edi
    Exchange D$ClipFileMemoryPointer D$TempoClipFileMemoryPointer

    Mov eax D$ClipMemoryEnd | sub eax D$ClipFileMemoryPointer

    VirtualFree D$TempoClipFileMemoryPointer
ret


[NewTemplateName: NewGroupName: ? #10] [WithCustomisation: ?    NewGroupNameLen: ?]

Proc GetNewTemplateName:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

        .If D@msg = &WM_COMMAND
            If D@wParam = &IDOK
                Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &WM_GETTEXTLENGTH, 0, 0
                inc eax | On eax > 38, Mov eax 38
                Call 'USER32.SendDlgItemMessageA' D@hwnd, 10, &WM_GETTEXT, eax,
                                                   NewTemplateName

                Call WM_CLOSE | popad | Mov eax &TRUE | ExitP

            Else_If D@wParam = &IDCANCEL
                Mov D$GetNewTemplateNameHandle 0
                Call WM_CLOSE | popad | Mov eax &TRUE | ExitP

            Else_If D@wParam = 20
                xor B$WithCustomisation &TRUE

            End_If

        .Else_If D@msg = &WM_INITDIALOG
            Mov B$WithCustomisation &FALSE
            move D$GetNewTemplateNameHandle D@hwnd
            Mov edi NewTemplateName, eax 0, ecx 10 | rep stosd

        .End_If

    popad | Mov eax &FALSE
EndP


[NoClipSelected: B$ "Select a Template in the second List Box, if you want to delete it" EOS]
[SureDelTemplate: B$ "Delete                                  " EOS]

DeleteTemplate:
    Call SaveClipSelections

    Call SearchTheClip

    .If D$TheClipLenght = 0
      ; No Item yet, or no Item selected:

        Call MessageBox Argh,
                        NoClipSelected,
                        &MB_SYSTEMMODAL+&MB_USERICON

    .Else
      ; Item selected >>> Delete:
        Mov esi D$ClipPointer, edi esi

      ; Go to start of Template to be deleted:
        While B$edi <> '/' | dec edi | End_While | Mov D$ClipPointer edi

        push esi, edi
            lea edi D$SureDelTemplate+8 | Mov esi D$ClipPointer | inc esi
            While B$esi <> CR | movsb | End_While | Mov D$edi '"  ?', B$edi+4 0

            Call MessageBox Sure,
                            SureDelTemplate,
                            &MB_SYSTEMMODAL__&MB_YESNO__&MB_ICONEXCLAMATION

        pop edi, esi

        .If D$FL.MsgBoxReturn = &IDYES
            Mov edx D$ClipFileMemoryPointer | add edx D$ClipFileSize
            Mov esi D$ClipPointer | inc esi | dec D$ClipFileSize
            While B$esi <> '/'
L0:             inc esi | dec D$ClipFileSize | On esi >= edx, jmp L1>
            End_While
            cmp B$esi-1 LF | jne L0<
            Mov ecx D$ClipMemoryEnd | sub ecx esi | rep movsb

L1:         Call ClipUpdate
        .End_If

    .End_If
ret

[TempoClipFileMemoryPointer: ?]

____________________________________________________________________________________________

Proc InitTemplateRadioButtons:
    Argument @hwnd

    Mov eax 105 | xor al B$WithData
    Call 'USER32.GetDlgItem' D@hwnd eax
    Call 'USER32.SendMessageA' eax &BM_SETCHECK eax &TRUE
    Mov eax 107 | xor al B$GlobalScope
    Call 'USER32.GetDlgItem' D@hwnd eax
    Call 'USER32.SendMessageA' eax &BM_SETCHECK eax &TRUE
EndP


[WithData: B$ &TRUE    GlobalScope: &TRUE]

CheckTemplateRadioButtons:
    Call 'USER32.GetDlgItem' D$ClipperHandle D$RadioButtonID
    push eax
        Call 'USER32.SendMessageA' eax &BM_GETCHECK 0 0 | xor eax &TRUE
    pop ebx
    Call 'USER32.SendMessageA' ebx &BM_SETCHECK eax 0
    xor D$RadioButtonID 1                                    ; 104 <> 105 // 106 <> 107
    Call 'USER32.GetDlgItem' D$ClipperHandle D$RadioButtonID
    push eax
        Call 'USER32.SendMessageA' eax &BM_GETCHECK 0 0 | xor eax &TRUE
    pop ebx
    Call 'USER32.SendMessageA' ebx &BM_SETCHECK eax 0
    If D$RadioButtonID < 106
        xor B$WithData &TRUE
    Else
        xor B$GlobalScope &TRUE
    End_If
ret


TemplateToClipBoard:
    On D$TheClipLenght = 0, ret
    Call 'USER32.OpenClipboard' D$H.MainWindow | cmp eax 0 | je L9>>
    Call 'USER32.EmptyClipboard' | cmp eax 0 | je L8>>

    Mov ecx D$TheClipLenght | shl ecx 2               ; *4 > room for Generic Names
    push ecx
        Call 'KERNEL32.GlobalAlloc' &GMEM_DDESHARE ecx | cmp eax 0 | jne L1>  ; > eax = handle
        pop eax | jmp L8>>
L1:     Mov D$hBlock eax
        Call 'KERNEL32.GlobalLock' eax                                       ; > eax = adress
    pop ecx
    shr ecx 2                                                   ; restore true data size
    Mov edi eax, esi D$ClipPointer

  ; If the very first Char after the first CR/LF is '@', we load the Clip 'As is':
    Mov ebx esi | add ebx 2
    While B$ebx <= ' ' | inc ebx | End_While

    .If B$ebx = '@'
        Call SearchTheClip | On D$TheClipLenght = 0, ret
        Mov esi D$ClipPointer, ecx D$TheClipLenght
        While B$esi <> '@'
            inc esi | dec ecx | jz L8>
        End_While | inc esi | dec ecx | jz L8>
        rep movsb

    .Else_If B$WithData = &TRUE
        If B$GlobalScope = &TRUE
            Call WithDataGlobalScope
        Else
            Call WithDataLocalScope
        End_If
    .Else
        If B$GlobalScope = &TRUE
            Call WithoutDataGlobalScope
        Else
            Call WithoutDataLocalScope
        End_If
    .End_If

    Mov eax 0 | stosd

    Call 'KERNEL32.GlobalUnlock' D$hBlock
    Call 'USER32.SetClipboardData' &CF_TEXT  D$hBlock
    Call 'USER32.SendMessageA' D$TemplateList3  &WM_SETTEXT 0 D$hBlock
L8: Call 'USER32.CloseClipboard'
L9: ret


[GenericNameHandle: ?]

RetrieveGenericName:
    Mov edi GenericName, ecx 10, eax 0 | rep stosd
    Call 'USER32.GetDlgItem' D$ClipperHandle 108  ; 108 = ID of Generic Name Edit Control.
    Mov D$GenericNameHandle eax
    Call 'USER32.SendMessageA' D$GenericNameHandle &EM_LINELENGTH 0 0
    If eax > 0
        Mov W$GenericName ax
        Call 'USER32.SendMessageA' D$GenericNameHandle, &EM_GETLINE, 0, GenericName
        Mov B$GenericName+eax 0  ; >>> 'EM_GETLINE_Comment'
    End_If
ret


WithDataGlobalScope:
L0: lodsb
    .If al = '@'
        If B$GenericName > 0
            push esi
                Mov esi GenericName | lodsb
L1:             stosb | lodsb | cmp al 0 | ja L1<
            pop esi
        End_If
    .Else
        stosb
    .End_If
    loop L0<
ret


WithDataLocalScope:
L0: lodsb
    .If al = '@'
        stosb
        If B$GenericName > 0
            push esi
                Mov esi GenericName | lodsb
L1:             stosb | lodsb | cmp al 0 | ja L1<
            pop esi
        End_If
    .Else_If al = '['
        If B$esi = '@'
            Mov eax 'Loca' | stosd | Mov ax 'l ' | stosw
            Call CopyTemplateLabelsOnly
        Else
            stosb
        End_If
    .Else_If al = '$'
      ; Strip '$' in 'D$@Value', but not in D$Value.
        On B$esi <> '@', stosb
    .Else_If al = '$'
      ; Strip '$' in 'D$@'Value', but not in D$Value.
        On B$esi <> '@', stosb
    .Else
        stosb
    .End_If
    loop L0<
ret


CopyTemplateLabelsOnly:
L0:
    lodsb
        .If al = '@'
            stosb
            If B$GenericName > 0
                push esi
                    Mov esi GenericName | lodsb
L1:                 stosb | lodsb | cmp al 0 | ja L1<
                pop esi
            End_If
L1:         lodsb | dec ecx | stosb
            If B$esi = ' '
                dec ecx | jmp L2>
            Else_If B$esi <> ':'
                jmp L1<
            End_If

L2:         Mov ax ', ' | stosw
        .Else_If al = ']'
            sub edi 2 | Mov D$edi 0 | ret       ; strip ending ' ,'
        .End_If
    Loop L0<


WithoutDataGlobalScope:
L0: lodsb
    .If al = '['
        On B$esi <> '@', jmp L2>
        While al <> ']'
            lodsb | dec ecx         ; Strip Data if unwished
        End_While
    .Else_If al = '@'
        If B$GenericName > 0
            push esi
                Mov esi GenericName | lodsb
L1:             stosb | lodsb | cmp al 0 | ja L1<
            pop esi
        End_If
    .Else
L2:     stosb
    .End_If
    loop L0<
ret


WithoutDataLocalScope:
L0: lodsb
    .If al = '['
        On B$esi <> '@', jmp L2>
        While al <> ']'
            lodsb | dec ecx         ; Strip Data if unwished
        End_While
    .Else_If al = '@'
        stosb
        If B$GenericName > 0
            push esi
                Mov esi GenericName | lodsb
L1:             stosb | lodsb | cmp al 0 | ja L1<
            pop esi
        End_If
    .Else_If al = '$'
      ; Strip '$' in 'D$@'
        On B$esi <> '@', stosb
    .Else_If al = '$'
      ; Strip '$' in 'D$@'
        On B$esi <> '@', stosb
    .Else
L2:     stosb
    .End_If
    loop L0<
ret







