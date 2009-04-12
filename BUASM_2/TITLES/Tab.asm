TITLE Tab
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
                  Main Editor's Upper Tab Control for TITLEd Chapters.

 When working with enTITLEd Sources, The Editor Copy/Paste the Actually edited Part
 at the very end of the Source Memory. Each time a new Part is activated, the previous
 one is copied back to its own room, and the new selected part is copied down in turn.

 At each event, a 'TitleTable' is build. This Table holds nothing but the Addresses
 of each 'TITLE' inside the Source.

 During Partial Edition, the Partial Source is substituted to the real Source. This
 means that all the KeyBoard features work like if the whole Source was nothing but
 the Partial Source. The modified behaviours are:

 * All KeyBoard actions (example: [Ctrl][PgDwn/PgUp] are limited to the Actual Part).
 * The ScrollBar, if any.
 * The Status Bar, if any.
 * The [Ctrl][S] feature is added a Dialog to choose between Whole/Part Saving.

 All of the Search Features (Right-Click / Tree View / SearchBox / ...) work the
 same as without TITLEs (upon the real whole Source).

 The goals of the feature are:

 * To make Part savings easier (for example, for reuse of wide Chuncks).
 * To accelerate the Source Editor, which, with uge Sources (1 Mega and more) may
   become slow on older Computers.
 * To make easy uge Source reorganisations.
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[TITLE_TC_ITEM:
 TITLE_TC_ITEM_imask: D$ &TCIF_TEXT__&TCIF_IMAGE
 TITLE_TC_ITEM_lpReserved1: D$ 0
 TITLE_TC_ITEM_lpReserved2: D$ 0
 TITLE_TC_ITEM_pszText: D$ 0
 TITLE_TC_ITEM_cchTextMax: D$ 0
 TITLE_TC_ITEM_iImage: D$ 0-1
 TITLE_TC_ITEM_lParam: D$ 0]

SetPartialEditionFromPos:
    On D$PreviousPartialSourceLen <> 0, ret

SetPartialEdition:
; Called when opening a new File, or by 'TitleWindowProc' (on user Tab Selection).
    Call BuildTitleTable

    If D$TiTleTable <> 0
        Call GetActualPartFromPos | Call SetActualPartFromPos
    End_If

    Mov D$PreviousUpperLine 0-1 ; (To force remaping the Source Editor Colors)

    Call AskForRedraw

    Mov B$RealSourceRestored &FALSE
ret


SetPartialEditionFromPosNoRedraw:
    On D$PreviousPartialSourceLen <> 0, ret

; Called when opening a new File, or by 'TitleWindowProc' (on user Tab Selection).
    Call BuildTitleTable

    If D$TiTleTable <> 0
        Call GetActualPartFromPos | Call SetActualPartFromPos
    End_If

    Mov D$PreviousUpperLine 0-1

    Mov B$RealSourceRestored &FALSE
ret


; 'Called by 'ControlZ'.

UndoTitleMove:
    push D$CaretRow D$CaretLine D$CurrentWritingPos D$UpperLine
    push ebx
        Call RestoreRealSource | Call BuildTitleTable
    pop ebx
    Mov eax D$TitleTable+ebx*4, D$CurrentWritingPos eax
    Call GetActualPartFromPos | Call SetActualPartFromPos

    Mov ebx D$UndoPtr | sub bx 32 | Mov esi ebx
    If D$esi+(7*4) <> 0
        lodsd | Mov D$CaretRow eax
        lodsd | Mov D$CaretLine eax
        lodsd | add eax D$CodeSource | Mov D$CurrentWritingPos eax
        lodsd | add eax D$CodeSource | Mov D$UpperLine eax
        pop eax, eax, eax, eax
    Else
        pop D$UpperLine D$CurrentWritingPos D$CaretLine D$CaretRow
    End_If
ret


[AddedSize: ?]
[RealSourceRestored: ?]

RestoreRealSource:                      ; Called only by 'UndoTitleMove'
    On D$PreviousPartialSourceLen = 0, ret

   ; Call DeleteUndoFiles
   ; Call KillUndo
   ; Call InitUndo

    Mov eax D$SourceEnd
    If W$eax-2 <> CRLF
        add D$SourceEnd 2 | add D$SourceLen 2
    End_If

    Mov eax D$SourceLen | sub eax D$PreviousPartialSourceLen
    Mov D$AddedSize eax

  ; Move down (as many 'AddedSize') from Top of Actual Source Part to
  ; End of Actual Edition, to make room:
    .If eax = 0
        Mov edi D$ActualTitle, esi D$CodeSource, ecx D$SourceLen
        rep movsb

    .Else_If eax g 0                ; Positive Number.

        Mov esi D$SourceEnd         ; End of Partial Edition
        Mov edi esi | add edi D$AddedSize
        Mov ecx esi | sub ecx D$NextTitle | inc ecx
      ; ecx = length from Source-Next-Title to End-of-Partial-Edition
        std
            rep movsb
        cld
        Mov esi D$CodeSource, edi D$ActualTitle
        add esi D$AddedSize         ; equal to the made up room
        Mov ecx D$SourceLen | rep movsb

    .Else                           ; Negative Number.
        Mov esi D$CodeSource, edi D$ActualTitle
        Mov ecx D$SourceLen | jecxz L0>
            rep movsb
L0:     Mov esi D$NextTitle
        Mov ecx D$CodeSource | sub ecx D$NextTitle | rep movsb

    .End_If

    Mov eax D$CurrentWritingPos
    If eax = D$SourceEnd
L0:     dec eax | cmp B$eax ' ' | jb L0<
    End_If

    sub eax D$CodeSource
    add eax D$ActualTitle | Mov D$CurrentWritingPos eax

    Mov eax D$ActualTitle | sub eax D$CodeSource    ; eax = adjustement to new Pos.

    add D$UpperLine eax

    If B$BlockInside = &TRUE
        add D$BlockStartTextPtr eax
        add D$BlockEndTextPtr eax
    End_If

    move D$CodeSource D$RealCodeSource
    move D$SourceEnd D$RealSourceEnd
    move D$SourceLen D$RealSourceLen
    Mov eax D$AddedSize | add D$SourceEnd eax | add D$SourceLen eax

    Mov D$PreviousPartialSourceLen 0

    Mov B$RealSourceRestored &TRUE
ret


; Called when Mouse outside User area.

ShowTitles:
    Call RestoreRealSource | Call BuildTitleTable

    If D$TitleTable > 0
        Call CreateTitleTab | Call ShowTitleTab | Call SetActualTitle

        On B$BlinkingCaretWanted = &TRUE, Call KillBlinkCursor
    End_If

    Call SetPartialEditionFromPos
ret


[NumberOfTitles: ?   PreviousNumberOfTitles: ?]

BuildTitleTable:
    Mov edi D$CodeSource, ecx D$SourceLen, ebx TitleTable
    Mov B$edi-1 LF

    move D$PreviousNumberOfTitles D$NumberOfTitles
    Mov D$NumberOfTitles 0

L0: Mov al 'T'

L1: repne scasb | jne L9>>

        cmp D$edi 'ITLE' | jne L1<
            cmp B$edi+4 ' ' | ja L1<
                cmp B$edi-2 LF | jne L1<

              ; If the first 'TITLE' is not Top-of-File, set one now:
                .If ebx = TitleTable
                    Mov eax edi | dec eax
                    If eax > D$CodeSource
                        move D$TitleTable D$CodeSource | add ebx 4
                        inc D$NumberOfTitles
                    End_If
                .End_If

                If ebx > TitleTable
                    Call TitleSize | jna L2>
                End_If

                Mov eax edi | dec eax | Mov D$ebx eax
                add ebx 4
                inc D$NumberOfTitles
                If ebx = TitleTableEnd
                    Call 'USER32.MessageBoxA', 0, {'Too many TITLEs', 0},
                                                  {'BuildTitleTable', 0}, 0
                    sub ebx 4
                End_If

L2:             On ecx > 0, jmp L0<<

L9: Mov D$ebx 0     ; 'TitleTable' End Mark.

  ; In case when Only one valid TITLE is found, we run no TITLE:
    If D$TitleTable > 0
        On D$TitleTable+4 = 0, Mov D$TitleTable 0
    End_If

    Mov eax D$NumberOfTitles
    If eax <> D$PreviousNumberOfTitles
        Call KillUndo | Call InitUndo
    End_If
ret

; If the TITLE too smaller for the screen, skip over:

TitleSize:
    push ecx
        Mov esi D$ebx-4 ; previous Title Pos.
        Mov ecx 0
        while esi < edi
            On B$esi = CR, inc ecx
            inc esi
        End_While
        cmp ecx D$LineNumber
    pop ecx
ret

____________________________________________________________________________________________
;;
 Partial Edition is done inside the 1_000_000 Octets reserved space that
 RosAsm always sets at the end of User Source. In case User would be
 dividing for the first time a very uge source, and, for example, write
 only one TITLE statement at the Top of Source, and then try to see what
 happends by moving the mouse outside the Client Area, the Partial Edition
 would be unable to copy Megas inside the reserved space. In such cases,
 we abort, and wait until user cut the Source in smaller parts, with
 more TITLES.
;;

[TooLongTitle: 'Part too long:', 0]

[TITLE_MAX 1_000_000]


; Sets the Actual Part Variables, depending on the Writing Position.

[ActualTiTle: ?    NextTitle: ?    PreviousPartialSourceLen: ?
 ActualPartIndex: ?    PreviousPartIndex: ?]

GetActualPartFromPos:
    Mov esi TiTleTable, ebx D$CurrentWritingPos, eax D$TiTleTable, edx 0
    Mov D$ActualTitle eax | move D$NextTitle D$esi+4

    Mov D$ActualPartIndex 0-2
  ; 0-2 because Tab Index are zero based (> -1) and because we are
  ; INCreasing until *next* Title found (> -1)

    While eax <= ebx
        inc D$ActualPartIndex
        Mov edx eax | lodsd | On eax = 0, jmp L1>
    End_While

L1: On edx = 0, Mov edx D$TiTleTable, eax D$TiTleTable+4
    On eax = 0, Mov eax D$SourceEnd

    Mov D$ActualTitle edx, D$NextTitle eax
ret
____________________________________________________________________________________________

;;
 The Code Source is always followed by 200 CR/LF (security) + 1_000_000
 octets reserved for Editing. We set the Partial Edition after the 200
 CR/LF.
;;

[RealCodeSource: ?    RealSourceEnd: ?    RealSourceLen: ?]

SetActualPartFromPos:
    On D$TiTleTable = 0, ret

    move D$RealCodeSource D$CodeSource
    move D$RealSourceEnd D$SourceEnd
    move D$RealSourceLen D$SourceLen

    Mov edi D$SourceEnd | add edi 400
    Mov D$CodeSource edi
    Mov esi D$ActualTitle, ecx D$NextTitle | sub ecx esi

    Mov D$PreviousPartialSourceLen ecx, D$SourceLen ecx
    Mov D$SourceEnd edi | add D$SourceEnd ecx | rep movsb
    Mov eax 0A0D0A0D, ecx 100 | rep stosd

    Mov eax D$CodeSource | sub eax D$ActualTiTle  ; eax = Displacement.

    add D$CurrentWritingPos eax | add D$UpperLine eax

    If B$BlockInside = &TRUE
        add D$BlockStarttextPtr eax | add D$BlockEndTextPtr eax
        move D$CurrentWritingPos D$BlockEndTextPtr
        Call SetCaret D$CurrentWritingPos
    End_If

  ; If user add or suppress a TITLE statement in the Partial Edition;
  ; all these Pointers are wrong. We reset at Top of Part:
    Mov eax D$CurrentWritingPos
    If eax >= D$SourceEnd
L1:     move D$CurrentWritingPos D$CodeSource
        move D$UpperLine D$CodeSource
        Mov B$BlockInside &FALSE | jmp L9>>
    Else_If eax < D$CodeSource
        jmp L1<
    End_If

    Mov eax D$UpperLine
    If eax >= D$SourceEnd
        jmp L1<
    Else_If eax < D$CodeSource
        jmp L1<
    End_If
ret
____________________________________________________________________________________________

KillTitleTab:
    Call 'USER32.DestroyWindow' D$TitleWindowHandle | Mov D$TitleWindowHandle 0

    On B$BlinkingCaretWanted = &TRUE, Call InitBlinkCursor
ret


SetActualTitle:
    Call 'User32.SendMessageA' D$TitleWindowHandle, &TCM_SETCURSEL, D$ActualPartIndex, 0
    Call 'USER32.ShowWindow' D$TitleWindowHandle, &SW_SHOW
ret


[MAXTITLE 100]
[TitleIndex: ?] [ TiTleTable: ? #MAXTITLE] [TitleTableEnd: ?]
[DefaultTopTitle: 'Top', 0]

[TabRECT: TabRECT.left: D$ 0,  TabRECT.top: D$ 0
          TabRECT.right: D$ 0, TabRECT.bottom: D$ 0]

[ClientToScreenPoint: ? ?]

; One TiTleTable Record is: dWord = Top of Part Pointer
; (>>> "TITLE TitleName")

ShowTitleTab: ; CreateTitleTab
    Mov esi TitleTable, D$TitleIndex 0

  ; Add a [Top] Tab Item if user did not.
    Mov eax D$esi
    If D$eax <> 'TITL'
        Mov D$TITLE_TC_ITEM_pszText DefaultTopTitle
        push esi
            Call 'User32.SendMessageA' D$TitleWindowHandle, &TCM_INSERTITEM,
                                       D$TitleIndex, TITLE_TC_ITEM
        pop esi
        inc D$TitleIndex
        move D$TiTleTable D$CodeSource | add esi 4
        On esi = TitleTableEnd, sub esi 4
    End_If

L0: lodsd | cmp eax 0 | je L9>>

    While B$eax = ' ' | inc eax | End_While  ; jump over 'TITLE'.
    While B$eax > ' ' | inc eax | End_While
    Mov ebx eax | add ebx 20
    Mov D$TITLE_TC_ITEM_pszText eax
    While B$eax = ' '
        inc eax | On eax > ebx, jmp L1>
    End_While

  ; Search for end of 'TitleName', to write a zero, in user source.
    While B$eax > ' '
        inc eax | On eax > ebx, jmp L1>
    End_While
L1: push D$eax, eax, esi
        Mov B$eax 0

        Call 'USER32.SendMessageA' D$TitleWindowHandle, &TCM_INSERTITEM,
                                   D$TitleIndex, TITLE_TC_ITEM
        inc D$TitleIndex
    pop esi, eax, D$eax
    jmp L0<<

  ; This first 'MoveWindow' is to ensure that the Tab Control has the Width of the
  ; Window, before asking for how many Tab Rows:
L9: move D$TabWindowX D$EditWindowX, D$TabWindowY D$EditWindowY,
         D$TabWindowW D$EditWindowW, D$TabWindowH D$EditWindowH

         move D$TabWindowY D$EditWindowH

    Call 'USER32.MoveWindow' D$TitleWindowHandle,
                             D$TabWindowX, D$TabWindowY, D$TabWindowW, D$TabWindowH,
                             &FALSE

  ; Get the hight of _one_ Tab, inside the Tabbed Control:
    Call 'USER32.SendMessageA' D$TitleWindowHandle, &TCM_GETITEMRECT, 0, TabRECT
    Mov eax D$TabRECT.bottom | sub eax D$TabRECT.top | inc eax

    push eax
        Call 'USER32.SendMessageA' D$TitleWindowHandle, &TCM_GETROWCOUNT, 0, 0
    pop ecx

  ; Now: ecx = Hight of a Tab // eax Number of Rows
    mul ecx | Mov D$TitleTabHight eax

    Call 'USER32.GetSystemMetrics' &SM_CYDLGFRAME | shl eax 1 | add eax 4
    add D$TitleTabHight eax

    Mov eax D$TabWindowH | sub eax D$TitleTabHight | Mov D$TabWindowY eax

    move D$TabWindowH D$TitleTabHight

    move D$ClientToScreenPoint 0, D$ClientToScreenPoint+4 0

    Call 'USER32.ClientToScreen' D$BpWindowHandle, ClientToScreenPoint

    Mov eax D$ClientToScreenPoint | add D$TabWindowX eax
    Mov eax D$ClientToScreenPoint+4 | add D$TabWindowY eax
    Mov eax D$BpMarginWidth | sub D$TabWindowX eax | add D$TabWindowW eax

    Call 'USER32.MoveWindow' D$TitleWindowHandle,
                             D$TabWindowX, D$TabWindowY, D$TabWindowW, D$TabWindowH,
                             &FALSE

  ; Set 'TabWindowY' to 'H.MainWindow' coordinates, for firing 'KillTitleTab' on &WM_MOUSEMOVE:
    Call 'USER32.ScreenToClient' D$H.MainWindow, TabWindowX

  ; Adjust the "Dead-Point" if the ToolBar is [On]:
    If B$ToolBarWanted = &TRUE
        Mov eax D$ToolBarPixelsHight | sub D$TabWindowY eax
    End_If
ret

[TabWindowX: ?  TabWindowY: ?  TabWindowW: ?  TabWindowH: ?]

[ErrorInTab: 'Error in Tab Building:', 0
 MoreThanFive: 'More than 5 Rows encounted', 0
 ZeroTabRow:   'Zero Tab Row...', 0
 BadTabWidth: 'Bad Tab Width', 0]


[TitleWindowHandle: 0    TitleTabHight: 60    TitleFontHandle: 0
 TitleTabWindowTitle: 'SysTabControl32' 0]

;[TITLEWINDOWSTYLE &TCS_MULTILINE__&TCS_BUTTONS__&TCS_HOTTRACK__&TCS_TOOLTIPS__&WS_DLGFRAME__&WS_VISIBLE]
[TITLEWINDOWSTYLE &TCS_MULTILINE__&WS_VISIBLE__&TCS_BUTTONS__&TCS_HOTTRACK]

CreateTitleTab:
    Call 'USER32.CreateWindowExA' 0, TitleTabWindowTitle, &NULL,
                                  TITLEWINDOWSTYLE, 0, 0, 0, 0,
                                  D$H.MainWindow, &NULL, D$hInstance, &NULL

    Mov D$TitleWindowHandle eax

    Call 'USER32.GetWindowLongA' D$TitleWindowHandle, &GWL_STYLE
    and eax (not &WS_CAPTION)
    Call 'USER32.SetWindowLongA' D$TitleWindowHandle, &GWL_STYLE, eax

    On D$TitleFontHandle = 0, Call CreateFontForTitles
    Call 'User32.SendMessageA' D$TitleWindowHandle, &WM_SETFONT, D$TitleFontHandle, &FALSE

    Call 'USER32.SetWindowLongA' D$TitleWindowHandle, &GWL_WNDPROC, TitleWindowProc
    Mov D$OriginalTitleBarProcedure eax
ret


CreateFontForTitles:
  Call 'GDI32.CreateFontA' 8 4 0 0 400 0 0 0  1,   ;  DEFAULT_CHARSET 1  OEM_CHARSET 255
                       0 0 0 0 Helv
  Mov D$TitleFontHandle eax
ret


[OriginalTitleBarProcedure: ?]

Proc TitleWindowProc:
    Arguments @hwnd, @msg, @wParam, @lParam

        On B$SourceReady = &FALSE, jmp L8>>

L8:     Call 'USER32.CallWindowProcA' D$OriginalTitleBarProcedure,
                                      D$TitleWindowHandle,
                                      D@msg, D@wParam, D@lParam

; Make sure that something would likely work under whatever OS version.
; But makes it as short as possible, because this is re-intrant with...
; here! Risks of Stack overflow, with the Tab Messages.
        ...If D@msg >= &WM_MOUSEFIRST
            ..If D@msg =< &WM_MOUSELAST ; (0209) // &WM_CAPTURECHANGED (0215)
                pushad
                    Call 'User32.SendMessageA' D$TitleWindowHandle, &TCM_GETCURSEL, 0, 0
                    If eax <> D$ActualPartIndex
                        Mov B$BlockInside &FALSE
                        push eax
                            Call RestoreRealSource
                        pop eax
                        Mov D$ActualPartIndex eax
                        move D$CurrentWritingPos D$TitleTable+eax*4
                        Call SetPartialEdition
                        Call AskForRedrawNow
                    End_If
                popad
            ..End_If
        ...End_If
EndP













