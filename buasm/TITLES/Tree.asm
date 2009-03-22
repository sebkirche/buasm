TITLE Tree            ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________
;;
 Tree view
 _______________________________________________________________________________________
 _______________________________________________________________________________________

 Building the tree view:

 here we build a simple list of labels encounted in source: label declarations 'Label:'
 and label evocations ('Call label'). A byte flag is used for declarations and for
 evocations. A Declaration record is:         Flag (1) / FLAG_DONE (0/1) / adr / name
                                              ..(Byte)....(Byte)........(dWord)(Bytes)
             An evocation record is:          Flag (2) / name
                                              ..(Byte)..(Bytes)
 Names Strings are NOT zero ended (No need as following byte is either 1 or 2).

 We store everything we find in read order. Local label are not considered; so that
 each main label is followed by its own labels evocations, without any check of
 'ret' statement (that would be too uncertain...).

 No error check here for open text / brackets. We Pop the return adress and return
 to caller of 'CreateTreeViewList'.
;;

[TreeList: D$ ?
 TreeListPtr: D$ ?
 TreeListEnd: D$ ?]

[DeclarationFlag 1  EvocationFlag 2  ListFlagMax 3]

BuildLabelTreeList:
    Mov eax D$STRUCT.EditData@SourceEnd | add eax 400 | Mov D$eax-4 0A0D0A0D
    Mov D$TreeList eax, D$TreeListPtr eax
    Mov esi D$CodeSource, edx D$STRUCT.EditData@SourceEnd

L0: lodsb | cmp esi edx | jae L9>>

    cmp al ';' | jne L1>                     ; jmp over comments
        If D$esi-2 = MLC   ; (LF ; ; CR)
            Do
                inc esi | cmp esi edx | jae L9>>
            Loop_Until D$esi = MLC
            add esi 3 | jmp L0<
        Else
L2:         lodsb | cmp al LF | jne L2<
            jmp L0<
        End_If

L1:   cmp al "'" | jne L1>                   ; jump over 'text'
L2:     lodsb | cmp al CR | je L3>
        cmp al "'" | jne L2<
          jmp L0<
L3:   jmp AbortTree

L1:   cmp al '"' | jne L1>                   ; jump over "text"
L2:     lodsb | cmp al '"' | je L0<
          cmp esi edx | jb L2<
      jmp AbortTree

; __________________________    Inside [brackets]: ________________________________

L1:   cmp al '[' | jne L1>                   ; jump over [Brackets]

L2:     lodsb | cmp al ';' | jne L4>

        If D$esi-2 = MLC   ; (LF ; ; CR)
            Do
                inc esi | cmp esi edx | jae L9>>
            Loop_Until D$esi = MLC
            add esi 3 | cmp esi edx | jae L9>>
        Else
L3:         lodsb | cmp al LF | jne L3<
        End_If
        jmp L2<

L4:     cmp al "'" | jne L4>
L5:       lodsb | cmp al "'" | je L2<        ; 'text' in brackets
          cmp al CR | jne L5<
            jmp L8>

L4:     cmp al '"' | jne L4>
L5:       lodsb | cmp al '"' | je L2<        ; "text" in brackets
          cmp esi edx | jb L5<
            jmp L8>

L4:     cmp al ']' | je L0<<
        cmp esi edx | jb L2<
L8:   jmp AbortTree

L1: cmp al ':' | jne L1>                              ; Label declaration found ?
        cmp B$esi-4 32  | jbe L0<<                         ; local ?
        cmp B$esi-4 '|' | je L0<<

      ; End of Label:
        Mov ecx esi | dec ecx

      ; Case of Exported Functions.
        On B$esi = ':', inc esi

        Push esi
L2:       dec esi | cmp B$esi 32 | ja L2<
            inc esi | Mov edi D$TreeListPtr
            Mov al DeclarationFlag | stosb            ; declaration flag
            Mov al 0 | stosb                          ; done flag
            Mov eax esi | stosd                       ; adress in source
            sub ecx esi | rep movsb                   ; label name
            Mov D$edi 0
            Mov D$TreeListPtr edi
        Pop esi
      jmp L0<<

L1:   or al 32 | cmp al 'a' | je IsItDialogApi        ; >>> down here
      cmp al 'c' | jne L0<<                           ; Label evocation found ?
      cmp B$esi-2 32 | jbe L2>
        cmp B$esi-2 '|' | jne L0<<
L2:   lodsd | or eax 020202020 | cmp eax 'all ' | je L3>
        sub esi 4 | jmp L0<<
L3:   cmp B$esi 32 | jne L4>                          ; strip double spaces
        inc esi | jmp L3<
L4:   If B$esi = "'"                                  ; direct api Call (without 'api' macro)
        Mov ebp esi | dec ebp | inc esi | jmp L3>>
      End_If

L8:           Mov edi D$TreeListPtr                   ; jmp target of successfull 'IsItDialogApi'
                Mov al EvocationFlag | stosb          ; evocation flag
L5:             lodsb | cmp al '0' | jb L6>
                        cmp al 'z' | ja L6>
                stosb | jmp L5<                       ; label name
L6:           Mov D$edi 0
              Mov D$TreeListPtr edi | dec esi
        jmp L0<<

L9: Mov edi D$TreeListPtr
    Mov al DeclarationFlag | stosb         ; security if 'Callback:' is last main label
    Mov al 0 | stosb
    Move D$TreeListEnd D$TreeListPtr
ret


IsItDialogApi:
    cmp B$esi-2 32 | jbe L2>
        cmp B$esi-2 '|' | jne L0<<
L2: Mov ebp esi
    lodsd | or eax 02020 | cmp eax "pi '" | je L3>
L7:     Mov esi ebp | jmp L0<<
L3: add esi 7 | lodsd | cmp eax 'Crea' | jne L4>      ; esi+7 > jumps over 'USER32.'
      lodsd | cmp eax 'teDi' | jne L7<
        lodsd | cmp eax 'alog' | jne L7<
          jmp L5>
L4: cmp eax 'Dial' | jne L7<
      lodsd | cmp eax 'ogBo' | jne L7<

L5: ; We found either Call 'CreateDialog/Indirect/paramA' or 'DialogBox/Indirect/ParamA'.
    ; the pointed Proc is always the fourth following parameter:

    Mov ecx 4

P0: lodsb | cmp al SPC | je P1>                      ; search a separator
            cmp al ',' | jne P0<

P1: lodsb | cmp al SPC | jbe P1<                     ; search start of parameter
            cmp al ';' | jne P4>                     ; jmp over comments
P2:            lodsb | cmp al LF | jne P2<
                   jmp P1<
P4: loop P0<

L6: dec esi | jmp L8<<


; If Building tree fails, we abort: strip previous record in BackTable, Pop the return
; adress of caller and return to Callback.

[TreeAborted: D$ ?]
[AbortTreeMessage: B$ "Unpaired Text delimiter or unpaired Bracket" EOS]

AbortTree:
    Mov ebx D$BackTablePtr | sub bl 4 | Mov D$ebx 0 | Mov D$BackTablePtr ebx
    Call 'USER32.MessageBoxA' D$H.MainWindow, AbortTreeMessage, Argh, &MB_SYSTEMMODAL
    Mov B$TreeAborted &TRUE
ret


;;
 Now the list of labels declarations and of labels evocation (by Call) is done. Let us
 take a short exemple: 'CallBack:' contains three calls (to SubA1/A2/A3); SubA1 contains
 two calls (to SubB1/B2); SubB1 again two calls (SubC1/C2); and at last, SubC2 has one
 Call (to SubD1). The tree we want to print could be:

 > CallBack
 >      SubA1
 >          SubB1
 >              SubC1
 >              SubC2
 >                  SubD1
 >          SubB2
 >      SubA2
 >      SubA3

 The List could be (1 is DeclarationFlag, 2 EvocationFlag, adr is Ptr in source):

 / 1 / adr / CallBack / 2 / SubA1 / 2 / SubA2 / 2 / SubA3 /
 / 1 / adr / SubA3 /
 / 1 / adr / SubA1 / 2 / SubB1 / 2 / SubB2 /
 / 1 / adr / SubA2
 / 1 / adr / SubB1 / 2 / SubC1 / 2 / SubC2 /
 / 1 / adr / SubC1 /
 / 1 / adr / SubC2 / 2 / SubD1 /
 / 1 / adr / SubD1 /
 / 1 / adr / SubB2 /
 / 1 ......

 We are going to get the tree from this list simply with playing with Push/Pop: We first
 Push 0, search for 'MainWindowProc' in the list, read/write; we Push a pointer to next
 evocation, search actual label declaration, write it, and so on until we find a label
 with a declaration flag. In this case, we Pop previous adress and go on. So, we go
 forth and back in the list until the entirely job is done; this is to say, until we
 Pop 0. The 'base' of search (edi) is always an evocation and 'what we search' (esi)
 is always a declaration.

 this entire job is done each time user click on 'Tree'.
;;

[TreeLabelPtr: D$ ?
 DoNotWriteInTree: B$ ?]

SearchListDeclaration:  ; edi(ah) > what to find (evoc.) // esi(al) search ptr (> declar.)

    Mov edx D$TreeListEnd, esi D$TreeList, B$DoNotWriteInTree &FALSE
    Push edi
L0:     lodsb | cmp esi edx | jae L8>
L1:     cmp al DeclarationFlag | ja L0<           ; jump over evocations
        Mov ebx esi | inc esi                     ; ebx = done flag ptr. Jump over
        Mov D$TreeLabelPtr esi | add esi 4 | Pop edi | Push edi
L4:     lodsb | Mov ah B$edi | inc edi
        and eax 00_11011111_11011111              ; ah, al > Upper Case
        cmp eax 0202 | je L7>
        cmp eax 0101 | je L7>
        cmp al 0 | je L8>>
        cmp al ah | je L4<

        cmp al ListFlagMax | ja L0<
L6:     cmp ah ListFlagMax | ja L1<

L7:     If B$ShowLabelsOnce = &TRUE
            On B$ebx = 1, Mov B$DoNotWriteInTree &TRUE
        End_If
        Mov B$ebx &TRUE                           ; write done flag
        dec esi | Mov ecx esi                     ; ecx = size
        Mov esi D$TreeLabelPtr
        lodsd | Mov ebx eax                       ; eax, ebx = declaration ptr in source
        sub ecx esi | jmp L9>

L8:     Mov ecx 0
L9: Pop edi
ret                     ; >>> ecx = 0 if not found,  esi = pointer to name if found


; For RightClick (direct search in tree View), the edge of the text must be defined.

[TreeViewItemEdge: D$ ? ]
[TreeViewItem: D$ ? # 100]

; when In, ecx = size of label name.

WriteTreeView:           ; Page set up of the tree like in Win32 TreeView Lists.
                         ; Here one Item once.
    Push esi, edi, ecx
        Mov edi TreeViewItem
L1:     cmp D$TreeIndent 0 | je L1>
L0:         Push ecx
                Mov ecx D$TreeIndent, ebx ecx
L0:             Mov eax '    ' | stosd
                Mov eax '|   ' | stosd
                loop L0<
            Pop ecx
L1:     rep movsb                   ; ecx still good from 'SearchLabelInTreeList'

L2:     Mov al SPC | stosb | Mov al 0 | stosb
        Call 'USER32.SendDlgItemMessageA' D$H.ShowTree, 100, &LB_ADDSTRING,
                                          0, TreeViewItem
    Pop ecx, edi, esi
ret

;;
 In next routine, reintrant procedure would produce infinite loop; This may append, for
 exemple when a DialogBox routine Closes the Dialog and re-run the same fresh Dialog to
 re-init its values (This is the case in RosAsm Dialog Editor source).

 For such cases, we store all Source Adresses of declarations at each increment of tree
 writting position, and check if new written declaration adress is up there in the same
 tree branching. It yes, we Pop back and go on.

 The checked value (Adress of Declaration in source) is given in eax by "SearchListDeclaration".
 but on "WriteTreeView" exit it is POPed in ebx, so that here, ebx is used instead.

 These adresses are stored in "TreeLevels" table which grows and decreases with indentations.

 What we Push and Pop here until poping 0 is nothing but a pointer to TreeList records.
 What we search in "SearchListDeclaration" is always the source adress of a declaration.
 When we see several following Labels at the same indent in Tree View window, we have
 both Push (for "If Next") and Pop (for "Next was a Declaration")... Difficult to explain
 difficult to understand, ... difficult to write... PUSHing is like going down in Tree
 branching and POPing is like going up. At each leave, we both Push and pop.
;;

[TreeIndent: D$ ?]
[TreeRoot: D$ ? # 120]

[TreeLevels: D$ ? # 30]

ClearTreeLevels: Mov edi TreeLevels, eax 0, ecx 30 | rep stosd | ret

BuildOneTree:
    Push 0                                           ; end search mark on stack first.
    Mov D$TreeIndent 0, edi TreeRoot                 ; Root String set by caller.
    Call SearchListDeclaration
    If ecx = 0                                       ; zero lenght (not found) > abort
        Pop eax | ret
    End_If
    Mov D$TreeLevels ebx

L0: Call WriteTreeView | add esi ecx
                                                     ; Testing NEXT RECORD flag:
L2: .If B$esi = EvocationFlag                        ; ________Evocation Case______________
        inc esi | Mov edi esi
L3:     lodsb | cmp al ListFlagMax | ja L3<          ; Search end of evocation Name
        dec esi | Push esi                           ; Push next possible evocation record Ptr
        inc D$TreeIndent                             ; > esi points to next record Type Flag
    .Else                                            ; ________Declaration Case____________
L4:     dec D$TreeIndent                             ; > No more evocation in that branch
        Pop esi | cmp esi 0 | ja L2<                 ; Pop previous TreeList pointer
            jmp L9>>
    .End_If

    Call SearchListDeclaration | cmp ecx 0 | je L4<  ; Evocation of a non existing Label
    cmp B$DoNotWriteInTree &TRUE | je L4<            ; If "First Call Only" selected

    Push edi, ecx                                    ; Compare new Source Label Adress with
        Mov edi TreeLevels, ecx D$TreeIndent         ; other adresses of the same branch
        inc ecx | repne scasd                        ; to prevent from infine loop in case
    Pop ecx, edi                                     ; of imbricated re-intrant calls.
    je L4<

    Mov eax D$TreeIndent, D$TreeLevels+eax*4 ebx | jmp L0<< ; store source Label Adresses.
                                                            ; for next time upper control
L9: ret


[ContinueOrphans: B$ 'List Orphans Labels?' EOS]
[ManyOrphans: B$ "
   After analyzes, the resulting Tree is found poorly organised,   
   and the amount of Labels is uge.
   
   Listing all of the orphan Labels may take a very long time,
   because the tree Builder will try to recreate a sub-Tree from
   each orphan...
 
                                    Go on listing?" EOS]

ListOrphanLabels:
    Mov D$TreeIndent 0, edx D$TreeListEnd, esi D$TreeList, ecx 0, ebx 0

  ; Count how many Orphan Labels to be listed:
L1: lodsb | cmp esi edx | jae L2>
    cmp al DeclarationFlag | ja L1<                       ; jump over evocations
      lodsb | add esi 4                                   ; jump over adress
        inc ebx
        cmp al &TRUE | je L1<                             ; done flag ?
            inc ecx | jmp L1<

  ; The number of orphans Labels may be uge with Disassemblies:
L2: shr ecx 2
    If ecx > ebx
            Call 'USER32.MessageBoxA' D$H.MainWindow, ManyOrphans, ContinueOrphans,
                                      &MB_SYSTEMMODAL__&MB_YESNO
        On eax = &IDNO, ret
    End_If

    Mov D$TreeIndent 0, edx D$TreeListEnd, esi D$TreeList

L1: lodsb | cmp esi edx | jae L9>
    cmp al DeclarationFlag | ja L1<                       ; jump over evocations
      lodsb | add esi 4                                   ; jump over adress
        cmp al &TRUE | je L1<                             ; done flag ?
        Push esi                                          ; start of name
L4:       lodsb | cmp al ListFlagMax | ja L4<             ; search end of name
          Mov ecx esi
        Pop eax
          sub ecx eax                                     ; ecx = size
          dec esi
        Push esi, edx                                     ; next record
          Mov edi TreeRoot, D$TreeIndent 0, esi eax       ; esi > start of name
          rep movsb | Mov al 0 | stosb
          Call BuildOneTree
        Pop edx, esi                                      ; next record
      jmp L1<
L9: ret


[ShowOrphan: &FALSE  ShowLabelsOnce: &TRUE]    ; keep dWords (evocated both as Bytes and dWords)

BuildTree:             ; build the tree from the List

    Mov D$TreeIndent 0
    Mov esi CallBackName, ecx D$CallBackNameLen, edi TreeRoot
    rep movsb | Mov al 0 | stosb

    Call BuildOneTree

    Mov esi EntryPointLabel, edi TreeRoot, D$TreeIndent 0
    While B$esi <> 0 | movsb | End_While | movsb
    Call BuildOneTree

    On B$ShowOrphan = &TRUE, Call ListOrphanLabels
L9: ret

 _______________________________________________________________________________________
 _______________________________________________________________________________________

; tree view main routines:

; Tag Dialog 22000

CreateTreeViewList:
    If D$H.ShowTree = 0
        Call 'USER32.CreateDialogParamA' D$H.Instance, 22000, D$H.MainWindow, ShowTree, &NULL
        On D$BookMarks > 0, Call ReInsertBookMarks
    Else
        Call 'USER32.SetForegroundWindow' D$H.ShowTree
        Call 'USER32.ShowWindow' D$H.ShowTree, &SW_RESTORE
    End_If
ret


; Called when loading a new file:

CloseTree:
    If D$H.ShowTree > 0
        Call 'USER32.DestroyWindow' D$H.ShowTree
        Mov D$H.ShowTree 0
    End_If
ret


[H.ShowTree: D$ ?]

[TreeWP:
 @TreeWP.iLength: D$ Len
 TreeWP.flags: D$ 0
 TreeWP.showCmd: D$ 0
 TreeWP.ptMinPosition.x: D$ 0
 TreeWP.ptMinPosition.y: D$ 0
 @TreeWP.ptMaxPosition.x: D$ 0
 @TreeWP.ptMaxPosition.y: D$ 0
 TreeWP.rcNormalPosition.left: D$ 0
 TreeWP.rcNormalPosition.top: D$ 0
 TreeWP.rcNormalPosition.right: D$ 0
 TreeWP.rcNormalPosition.bottom: D$ 0]

[ListKeyBoardInput: D$ ?
 AutoHideTreeView: D$ ?
 AutoRebuildTreeView: D$ ?]

Proc ShowTree:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ..If D@msg = &WM_COMMAND
        .If D@wParam = &IDCANCEL
L7:        Mov D$H.ShowTree 0
           Call 'USER32.EndDialog' D@hwnd 0   ; CloseTree

        .Else
            shr D@wParam 16

            If D@wParam = &LBN_KILLFOCUS
                On B$AutoHideTreeView = &TRUE,
                    Call 'USER32.ShowWindow' D@hwnd &SW_MINIMIZE
            Else_If D@wParam = &LBN_SELCHANGE
                On B$ListKeyBoardInput = &FALSE, Call SearchFromTreeListBox D@lParam
                Mov B$ListKeyBoardInput &FALSE
            End_If
        .End_If

    ..Else_If D@msg = &WM_VKEYTOITEM    ; Prevents from searching hundreads of
        If W@wParam <> CR                   ; Label when user is moving through the
            Mov B$ListKeyBoardInput &TRUE   ; List with the KeyBorad.
        Else                                ; But allow Search if [Return] Key depressed.
            Call SearchFromTreeListBox D@lParam
        End_If
        popad | Mov eax 0-1 | jmp L9>>

    ..Else_If D@msg = &WM_INITDIALOG
      ; Does not survive in the Minimized state...:
      ; Call 'USER32.SetWindowLongA' D@hwnd, &GWL_EXSTYLE, &WS_EX_TOOLWINDOW
        Call RestoreRealSource
        Mov D$TreeViewItemEdge '    '
        Move D$H.ShowTree D@hwnd
        Call StorePosInBackTable
        Call 'USER32.SetClassLongA' D$H.ShowTree &GCL_HICON D$STRUC.WINDOWCLASS@hIcon
        Call ClearTreeLevels
        Mov B$TreeAborted &FALSE | Push ebp | Call BuildLabelTreeList | Pop ebp
        cmp B$TreeAborted &TRUE | je L7<<
        Call BuildTree
        Call SetPartialEditionFromPos

    ..Else_If D@msg = &WM_SIZE
      ; This is for adjusting the List Control to the Dialog new Size:
        Call 'USER32.GetClientRect' D@hwnd, ListEditRect
        Mov eax D$ListERX | sub D$ListERW eax
        Mov eax D$ListERY | sub D$ListERH eax
        Call 'USER32.GetDlgItem' D@hwnd, 100
        Call 'USER32.MoveWindow' eax D$ListERX D$ListERY D$listERW D$ListERH &FALSE

      ; Save the user's Tree Dialog Width:
        Call 'USER32.GetWindowRect' D@hwnd, ListEditRect
        Mov eax D$ListERW | sub eax D$ListERX | Mov D$TreeWidth eax

L1:     Push D$TreeWP.ptMinPosition.x, D$TreeWP.ptMinPosition.y, D$TreeWP.flags
            Call 'USER32.GetWindowPlacement' D$H.ShowTree TreeWP
        Pop D$TreeWP.flags, D$TreeWP.ptMinPosition.y, D$TreeWP.ptMinPosition.x

    ..Else_If D@msg = &WM_MOVE
        jmp L1<

    ..Else_If D@msg = &WM_CTLCOLORLISTBOX
        Call 'GDI32.SetBkColor' D@wParam D$ARVB.DialogsBackColor
        popad | Mov eax D$H.DialogsBackGroundBrush | jmp L9>

    ..Else_If D@msg = &WM_ACTIVATE
       ; Wanted by... Anvar if i remember, but conflicts with any wish of moving
       ; the reduced Tree Window, with AutoHide Flag set on. So commented out:

       ; On B$AutoHideTreeView = &TRUE, Call 'USER32.ShowWindow' D@hwnd &SW_NORMAL

    ..Else
        popad | Mov eax &FALSE | jmp L9>

    ..End_If

    popad | Mov eax &TRUE

L9: EndP


[TreeCurrentSel: D$ ?
 TreeUpperMainLabElsel: D$ ?]

Proc SearchFromTreeListBox:
    Argument @lParam

    Call RestoreRealSource

    Call 'USER32.SendMessageA' D@lParam, &LB_GETCURSEL, 0, 0
    Mov D$TreeCurrentSel eax, D$TreeUpperMainLabElsel eax

    Call 'USER32.SendMessageA' D@lParam, &LB_GETTEXT, D$TreeUpperMainLabElsel, TreeViewItem
    Mov esi TreeViewItem | On B$esi > SPC, jmp L2>
L1: lodsb | cmp B$esi SPC | je L1<
        cmp B$esi '|' | je L1<

L2: If B$esi = '@'
L0:     dec D$TreeUpperMainLabElsel | jc L9>>
        Call 'USER32.SendMessageA' D@lParam, &LB_GETTEXT, D$TreeUpperMainLabElsel,
                                   TreeViewItem
        Mov esi TreeViewItem | On B$esi > SPC, jmp L2>
L1:     lodsb | cmp B$esi SPC | je L1<
            cmp B$esi '|' | je L1<
L2:             On B$esi = '@', jmp L0<
                    Call InternalRightClick

    Else
        Call InternalRightClick | jmp L9>>

    End_If
;;
  If here, after having a selected Item saying "@Label", we have found out the upper
  MainLabel, which is now a selected Block.  We search downward for the wanted relative
  "@Label":
;;
    Call 'USER32.SendMessageA' D@lParam, &LB_GETTEXT, D$TreeCurrentSel, TreeViewItem
        Mov esi TreeViewItem | On B$esi > SPC, jmp L2>
L1:     lodsb | cmp B$esi SPC | je L1<
            cmp B$esi '|' | je L1<

L2: Push D$LenOfSearchedString
        Mov edi SearchString,
            D$LenOfSearchedString 1

        While B$esi > SPC

            movsb | inc D$LenOfSearchedString

        End_While

        Mov al ':' | stosb | Mov al 0 | stosb

        Push D$DownSearch, D$CaseSearch, D$WholeWordSearch, D$STRUCT.EditData@CurrentWritingPos

            Mov B$DownSearch &TRUE, B$CaseSearch &FALSE, B$WholeWordSearch &TRUE

            Move D$STRUCT.EditData@CurrentWritingPos D$LP.BlockEndText

            Push D$NextSearchPos

                Move D$NextSearchPos D$STRUCT.EditData@CurrentWritingPos

                Call StringSearch

            Pop D$NextSearchPos

        Pop D$STRUCT.EditData@CurrentWritingPos, D$STRUCT.EditData@CurrentWritingPos, D$CaseSearch, D$DownSearch

    Pop D$LenOfSearchedString

L9: Call SetPartialEditionFromPos
EndP



; Set the Tree Window at left of Main Window:

SetTreeDialogPos:
    Call GetTreePlacement

    Call 'USER32.SetWindowPlacement' D$H.ShowTree, TreeWP

    Call 'USER32.GetWindowLongA' D$H.ShowTree, &GWL_STYLE | or eax &WS_VISIBLE
    Call 'USER32.SetWindowLongA' D$H.ShowTree, &GWL_STYLE, eax
ret


[TreePlacementDone: D$ ?
 TreeWidth: D$ ?]

[EditWindowXforTree: D$ ?
 EditWindowYforTree: D$ ?
 EditWindowX2forTree: D$ ?
 EditWindowY2forTree: D$ ?]

GetTreePlacement:
    On B$TreePlacementDone = &TRUE, ret

    Mov B$TreePlacementDone &TRUE

    Call 'USER32.GetWindowRect' D$H.EditWindow, EditWindowXforTree

  ; Default Width if the user never redefined it:
    On D$TreeWidth = 0, Mov D$TreeWidth 250

    Mov eax D$EditWindowX2forTree | Mov D$TreeWP.rcNormalPosition.right eax
    sub eax D$TreeWidth | Mov D$TreeWP.rcNormalPosition.left eax
    Mov eax D$EditWindowYforTree | Mov D$TreeWP.rcNormalPosition.top eax
    Mov eax D$EditWindowY2forTree | Mov D$TreeWP.rcNormalPosition.bottom eax

    Mov D$TreeWP.flags &WPF_SETMINPOSITION
    Mov eax D$TreeWP.rcNormalPosition.right | sub eax 160    ; founded Minimized Width.
    Mov D$TreeWP.ptMinPosition.x eax
    Mov eax D$TreeWP.rcNormalPosition.top
    Move D$TreeWP.ptMinPosition.Y eax
    Mov D$TreeWP.flags &WPF_SETMINPOSITION

    Mov D$TreeWP.showCmd &SW_SHOWNORMAL
ret


____________________________________________________________________________________________

;;
  Simplified version of the &WM_INITDIALOG case in 'ShowTree' Proc. Called from
  'AsmMain' when 'AutoRebuildTreeView' is TRUE.
;;

TreeUpDate:
    Call 'USER32.SendDlgItemMessageA' D$H.ShowTree, 100, &LB_RESETCONTENT, 0, 0

    Mov D$TreeViewItemEdge '    ' | Call ClearTreeLevels

    Mov B$TreeAborted &FALSE | Push ebp | Call BuildLabelTreeList | Pop ebp

    On B$TreeAborted = &FALSE, Call BuildTree
L9: ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
