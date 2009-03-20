TITLE SourceEdit      ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________
;;
  RosAsm Sources Editor
  
  - The Caret is Screen dependant. Not Text dependant.
  
  - The Editor is _not_ a Line-Oriented one.
  
  - The Main Routine called on WM_PAINT is 'PrintColorText'
  
  - Most minor Routines are called from 'CharMessage' or from 'KeyMessage'
  
  - The coloring is achieve through a parallele Table ('ColorsMap')
  
  __________________
  Overall mechanism: 
  
  This Editor is "File Oriented". This is to say, that, each time we insert
  or delete one single Char, all of the downward Text is moved. Over some size
  (depending on the Processor speed) the Editor looses its responsivity, once
  the Copy Operations are slwoer than the KeyBoard inputs speed.
  
  This was the first reason why a Mechanism of TITLEs was introduced, in order
  to divide the real momolitic user Source into "Sub-Sources": For Sources divided
  into TITLEs, the Editor makes a Copy of the Edited TITLE, at the End of the real
  Source, in Memory, and we always work on this isolated Part. This reduces the
  Copy operation to one smaller TITLE part, and the real Copy, into the real user
  Source is done only for each operation requiring to work with the real Source.
  
  The concerned Routines is 'RestoreRealSource' and, for going back to a TITLE
  Edition, 'SetPartialEditionFromPos'.
  
  ________________
  The user inputs:
  
  As said above ('CharMessage', 'KeyMessage') two imputs are considered:
  
  * &WM_CHAR, of course (Nothing special).
  
  * &WM_KEYDOWN / &WM_KEYUP. For these ones, the Editor manages a 'Keys' Table,
    where each Key State (1/0 -On/Off-) is stored.
    
  So, some Functionalities are driven from the &WM_CHAR, and some are driven
  from the &WM_KEYDOWN Messages.
  
    ____________________
    The output Routines.
    
    'XtoRow' 'RowToX' 'TroncatedXtoRow' 'YtoLine' 'LineToY' 'TroncatedYtoLine'
    
    'InitPrintText' 'BlankRemainders'
    
        'PrintColorText'
        
            'PrintCaretAndUnderline' 'GetLastCharPosOnScreen' 'AjustForRightScroll'
        
            'SetColorsMap' 'TextColorsMap'  'TextOutput'
    
            'InitPrintText'  'ClosePrint'
    
    ______________
    Minor Routines
   
    'MouseTextPos' 'SimpleMouseTextPos'
    
    'DownOneLine' 'UpOneLine' 'FullDown' 'OnlyOnePageDown' 'FullUp' 'OnlyOnePageUp'
    
    'AutoDeleteBlock'
    
    'OverwriteSource' 'InsertSource' 'InsertDoubleByte'
    
    'SetIndent' 'CarriageReturn'
    
    'TryToMove'
    
    'ResetCaretOnStripTab' 'StripBackSpace' 'BackSpace'
    
    'KeyDown' 'KeyUp' 'StartOfLine' 'KeyLeft'
    
    'SetPhysicalCaretRow'
    
    'StartOfWord' 'EndOfLine' 'KeyRight' 'EndOfWord'
    
    'KeyInsert' 'StripOneChar' 'KeyDelete'
    
    'SearchTxtPtr'
    
    'SetCaret' 'LeftButton'
    
    'SetBlock'
    
    'LeftButtonUp'
    
    'ControlC' 'ControlY' 'ControlD' 'ControlX' 'UndoControlV'
    
    'OpenClipBoard' 'ClipBordCleaner' 'KillTabs' 'CloseClipBoard'
    
    'ControlV'
    
    'ReMapSourceMemoryIfNeeded'
    
    'DrawOneLine'
    
    ____________
    Undo feature
    
    'InitUndo' 'KillUndo' 'ReInitUndo' 'DoStoreInsert' 'DoStoreOverWrite'
    'DoStoreCharDelete' 'ReInsertDeletedChar' 'DoStoreBlockPaste'
    
    'ResetUndoFileName' 'IncUndoFileName' 'DecUndoFileName'
    
    'WriteUndoBlockFile'
    
    'DeleteUndoFiles' 'DeleteOldUndoFiles'
    
    'UndoBlockCopy' 'ReadUndoBlockFile'
    
    'ControlZ' 'ReadUndoRecord'
    
    ____________________
    Main Redraw Routines
    
    'AskForRedraw' 'AskForRedrawNow'
    
    _____________________
    Text Pos and searches
    
    'TextPos' 'RedrawScrollBar' 'RePosFromScroll'
    
    'KillTrailingSpaces'
    
    'GetWheelInfo' 'WheelMsg'
    
    'StartEdition' 'ReplaceParagraphByDollar'
    
    'CloseHelp'
    
    'InternSearch'

    'SavePosOnF11' 'SetPosOnF12' 'ClearF11F12' 'ClearF12'
    
    _____
    Fonts (called from the Configuration Tab or from main
    
    'ChooseFontHook' 'SelectFont' 'LoadFont'
;;

; looks: Red , Green , Blue , 0  when bytes,     0/Blue/Green/Red  when dWord

[NormalTextColor: B$    0   0   0 0    ; color for Statements or brackets
 CaretColor:          150 150 150 0    ; (xor 00_01101001 for Insert Mode... avoid modifying)
 PreviousNormalColor:   0   0   0 0]

[PAINTSTRUCT:
 PShdc: D$ ?
 PSfErase: D$ ?
 PSrcPaint: D$ ? ? ? ?
 PSfRestore: D$ ?
 PSfIncUpdate: D$ ?
 PSrgbReserved: D$ ? ? ? ? ? ? ? ?]

[RECT:
 RECTleft: D$ ?
 RECTtop: D$ ?
 RECTright: D$ ?
 RECTbottom: D$ ?]
____________________________________________________________________________________________

[STRUCT.EditData:  ; 21 dwords
 @hFont: D$ 0
 @Col: D$ 0
 @Line: D$ 0  ; Col2: 0  Line2: 0
 @HDC: D$ 0
 @CurrentWritingPos: D$ 0
 @ColNumber: D$ 8
 @LineNumber: D$ 0
 @SourceEnd: D$ 0
 @CaretLine: D$ 0
 @CaretRow: D$ 1
 @CaretRowValue: D$ 8
 @PhysicalCaretRow: D$ 1
 @Caret: D$ 0
 @RightScroll: D$ 0
 @CaretEndOfLine: D$ 0
 @Overwrite: D$ 0
 @StartOfNormalText: D$ 0
 @StartOfComment: D$ 0
 @StartOfBlockText: D$ 0
 @UpperLine: D$ 0]

____________________________________________________________________________________________

; SourceEnd: points AFTER the last byte (so that D$SourceEnd-D$CodeSource = D$SourceLen).

[BlankLine: B$ 32 # 200 EOS]

[H.BackGroundBrush: D$ ?
 H.DialogsBackGroundBrush: D$ ?]

[H.Font1: D$ ?]
____________________________________________________________________________________________

Proc XtoRow:
    Argument @X
    Uses edx, ecx

        Mov edx 0, eax D@X, ecx D$FontWidth | div ecx
      ; Round up:
        shr ecx 1 | On edx > ecx, inc eax
EndP


[XRemainder: D$ ?
 YRemainder: D$ ?]

Proc TroncatedXtoRow:
    Argument @X
    Uses edx

        Mov edx 0, eax D@X | div D$FontWidth
        Mov D$XRemainder edx
EndP


Proc YtoLine:
    Argument @Y
    Uses edx, ecx

        Mov edx 0, eax D@Y, ecx D$FontHeight | div ecx
      ; No 'HalfWay' adjustement to do: The Hot Point of the default Edit-type Cursor
      ; seems to be at the bottom of its shape...
EndP

Proc TroncatedYtoLine:
    Argument @Y
    Uses edx, ecx

        Mov edx 0, eax D@Y | div D$FontHeight
        Mov D$YRemainder edx
EndP


Proc RowToX:
    Argument @Row
    Uses edx

        Mov eax D@Row | mul D$FontWidth
EndP


Proc LineToY:
    Argument @Line
    Uses edx

        Mov eax D@Line | mul D$FontHeight
EndP
____________________________________________________________________________________________

[SourceEndReached: D$ ?
 EditRightPixel: D$ ?
 EditBottomPixel: D$ ?]

[AraseBackEdit:  ; TODO locla
 D$ ?
 D$ ?
 D$ ?
 D$ ? ]

InitPrintText:
    Call 'USER32.BeginPaint' D$H.EditWindow, PAINTSTRUCT | Mov D$STRUCT.EditData@HDC eax

    Call 'GDI32.SelectObject'  D$STRUCT.EditData@HDC D$H.Font1 | Mov D$STRUCT.EditData@hfont eax
    Call 'GDI32.SetBkColor' D$STRUCT.EditData@HDC D$ARVB.BackColor

    Call TroncatedXtoRow D$STRUC.RECT.EditWindow+RIGHT | Mov D$STRUCT.EditData@ColNumber eax
    Call RowToX eax | Mov D$EditRightPixel eax

    ;If D$H.TitleWindow <> 0
    ;    Mov eax D$TabWindowH | sub D$STRUC.RECT.EditWindow+BOTTOM eax
    ;End_If

    Call TroncatedYtoLine D$STRUC.RECT.EditWindow+BOTTOM | Mov D$STRUCT.EditData@LineNumber eax
    Call LineToY eax | dec eax | Mov D$EditBottomPixel eax

;    Call BlankRemainders
; If the division of the writing area by the Font size is not exact (no remainder)
; redraw this background part.

;BlankRemainders:

    If D$XRemainder <> 0

        Call 'USER32.GetClientRect' D$H.EditWindow,
                                    AraseBackEdit

        Move D$AraseBackEdit D$AraseBackEdit+8

        Mov eax D$XRemainder | sub D$AraseBackEdit eax

        Call 'USER32.FillRect' D$STRUCT.EditData@HDC,
                               AraseBackEdit,
                               D$H.BackGroundBrush

    End_If


    If D$YRemainder <> 0

        Call 'USER32.GetClientRect' D$H.EditWindow,
                                    AraseBackEdit

        Move D$AraseBackEdit+4 D$AraseBackEdit+12

        Mov eax D$YRemainder | sub D$AraseBackEdit+4 eax

        Call 'USER32.FillRect' D$STRUCT.EditData@HDC,
                               AraseBackEdit,
                               D$H.BackGroundBrush

    End_If

;ret

    dec D$STRUCT.EditData@LineNumber | dec D$STRUCT.EditData@ColNumber

    Mov B$TextGoingOn &FALSE | Move D$NormalTextColor D$StatementColor
    Mov B$SourceEndReached &FALSE
ret


[MarginLine:
 @X1: D$ ?
 @Y1: D$ ?
 @X2: D$ ?
 @Y2: D$ ?]

DrawMarginLine:
    Mov eax D$FontWidth | dec eax | Mov D$MarginLine@X2 eax
    dec eax | Mov D$MarginLine@X1 eax
    Mov D$MarginLine@Y1 0
    Move D$MarginLine@Y2 D$STRUC.RECT.EditWindow+BOTTOM

    Call 'USER32.FillRect' D$STRUCT.EditData@HDC, MarginLine, D$H.CaretBrush
ret


RemoveCaretAndUnderline:
    If D$STRUC.RECT.Caret+RIGHT <> 0
        Call 'USER32.FillRect'  D$STRUCT.EditData@HDC STRUC.RECT.Caret D$H.BackGroundBrush
        Mov D$STRUC.RECT.Caret+RIGHT 0
    End_If

    If D$STRUC.RECT.UnderLine+RIGHT <> 0
        Call 'USER32.FillRect' D$STRUCT.EditData@HDC, STRUC.RECT.UnderLine, D$H.BackGroundBrush
    End_If
ret

ClosePrint:
    Call 'GDI32.SelectObject' D$STRUCT.EditData@HDC, D$STRUCT.EditData@hfont
    Call 'USER32.EndPaint' D$H.EditWindow, PAINTSTRUCT

    Mov B$TextGoingOn &FALSE
ret
____________________________________________________________________________________________

[TabOldCaretRow: D$ 1]

[Underline: D$ ?]

[STRUC.RECT.Caret:
 D$ ?
 D$ ?
 D$ ?
 D$ ?]

[STRUC.RECT.UnderLine:
 D$ ?
 D$ ?
 D$ ?
 D$ ?]

[H.CaretBrush: D$ ?
 H.RedBrush: D$ ?]


PrintCaretAndUnderline:
    Mov eax D$STRUCT.EditData@LineNumber | On D$STRUCT.EditData@CaretLine > eax, Mov D$STRUCT.EditData@CaretLine eax ; TODO Cmov...

    Mov esi D$STRUCT.EditData@UpperLine, ecx 0

L0: cmp ecx D$STRUCT.EditData@CaretLine | je L2>                 ; search for Caret line (esi)

    On ecx > D$STRUCT.EditData@LineNumber, ret
L1: lodsb | cmp al LF | jne L1<
    inc ecx | jmp L0<

L2: Mov D$STRUCT.EditData@CaretEndOfLine &FALSE, ecx 0
    On D$STRUCT.EditData@RightScroll > 0, sub ecx D$STRUCT.EditData@RightScroll

L0: cmp ecx D$STRUCT.EditData@CaretRow | je L5>>
L1: lodsb | cmp al TAB | je L3>
            cmp al CR | ja L2>                      ; search for Caret col (esi too)
      Mov D$STRUCT.EditData@CaretEndOfLine &TRUE | inc ecx
      Mov D$STRUCT.EditData@Caret 32, ebx ecx, D$STRUCT.EditData@PhysicalCaretRow ebx | jmp L6>>   ; Caret at end of line
L2: inc ecx | jmp L0<

L3: Mov ebx D$STRUCT.EditData@CaretRow, edx ecx | or ecx 00_111 | inc ecx | On ebx > ecx, Jmp L0<

    inc edx | Mov D$STRUCT.EditData@CaretRow edx | cmp ebx D$TabOldCaretRow | jbe L5>

    If edx = D$TabOldCaretRow
        inc ecx | Mov D$STRUCT.EditData@CaretRow ecx | lodsb  ; just for insert Pos on Tab
    End_If

L5: Mov B$STRUCT.EditData@Caret al ; !!! eax D$
    Mov ebx D$STRUCT.EditData@CaretRow, D$STRUCT.EditData@PhysicalCaretRow ebx
L6: dec esi | Mov D$STRUCT.EditData@CurrentWritingPos esi | Mov D$TabOldCaretRow ebx

    Call LineToY D$STRUCT.EditData@CaretLine | Mov ecx eax
    Call RowToX ebx | Mov ebx eax

    Mov D$STRUCT.EditData@CaretRowValue ebx
    On esi >= D$STRUCT.EditData@SourceEnd Mov D$STRUCT.EditData@Caret SPC

    If ecx > 0
        Mov eax D$FontHeight | shr eax 2 | sub ecx eax
    Else
        Mov eax 0
    End_If
    Mov D$STRUC.RECT.Caret ebx, D$STRUC.RECT.Caret+TOP ecx
    add ebx 3 | add ecx eax | add ecx D$FontHeight
    Mov D$STRUC.RECT.Caret+RIGHT ebx, D$STRUC.RECT.Caret+BOTTOM ecx

  ; Let Overwrite Dims a special case to let max speed to the normal Caret:
    If D$STRUCT.EditData@Overwrite = &TRUE
        Mov eax D$FontWidth | add eax D$STRUC.RECT.Caret+LEFT | Mov D$STRUC.RECT.Caret+RIGHT eax
        Mov eax D$FontHeight | shr eax 2 | add D$STRUC.RECT.Caret+TOP eax
    End_If

    On D$FL.ShowCaret = &TRUE, Call 'USER32.InvertRect' D$STRUCT.EditData@HDC, STRUC.RECT.Caret

    .If B$Underline = &TRUE
        Move D$STRUC.RECT.UnderLine+LEFT D$STRUC.RECT.Caret+LEFT,
             D$STRUC.RECT.UnderLine+TOP D$STRUC.RECT.Caret+TOP,
             D$STRUC.RECT.UnderLine+RIGHT D$STRUC.RECT.Caret+RIGHT,
             D$STRUC.RECT.UnderLine+BOTTOM D$STRUC.RECT.Caret+BOTTOM

        Mov ebx D$STRUCT.EditData@CurrentWritingPos | dec ebx
        Mov eax D$FontWidth
        While B$ebx > SPC | dec ebx | sub D$STRUC.RECT.UnderLine+LEFT eax | End_While
        Mov eax D$FontHeight | inc eax
        add D$STRUC.RECT.UnderLine+TOP eax | sub D$STRUC.RECT.UnderLine+RIGHT 6
        Mov eax D$STRUC.RECT.UnderLine+TOP | add eax 2 | Mov D$STRUC.RECT.UnderLine+BOTTOM eax

        Call 'USER32.FillRect' D$STRUCT.EditData@HDC, STRUC.RECT.UnderLine D$H.CaretBrush
    .End_If
ret

____________________________________________________________________________________________

[RosAsmBracket: D$ ?
 MultiLinesComment: D$ ?]

____________________________________________________________________________________________
____________________________________________________________________________________________

[LastCharPosOnScreen: D$ ?]

GetLastCharPosOnScreen:
  ; LineNumber is zero based:
    Mov eax D$STRUCT.EditData@LineNumber | inc eax

    Mov esi D$STRUCT.EditData@UpperLine

L0: .If B$esi < SPC
        If B$esi = CR
            On B$esi+1 <> LF, Mov B$esi SPC
        Else_If B$esi = LF
            On B$esi-1 <> CR, Mov B$esi SPC
        Else
            Mov B$esi SPC
        End_If
    .End_If

    inc esi | cmp B$esi CR | jne L0<
    dec eax | jnz L0<

    inc esi | Mov D$LastCharPosOnScreen esi
ret
____________________________________________________________________________________________

[WantSizeMarkerColor: &TRUE]

Proc SetColorsMap:
    Argument @First, @Last, @Color

        Mov esi D@First, edx D@Last | add edx 100
        Mov edi esi | sub edi D$CodeSource | add edi D$ColorsMap
        Mov bl B@Color | On bl = 0, Mov bl 1

L0:     .While esi < edx
            Mov al B$esi

            ; Normal = 1 // Brackets = 2 // Strings = 3 // Comments = 4 // Selection 5
            ...If al = "'"
                Do
                    If B$esi >= SPC
                        Mov B$edi 3
                    Else
                        Mov B$edi 0
                    End_If
                    inc esi | inc edi | cmp esi edx | je L9>>
                Loop_Until B$esi = "'"
                Mov B$edi 3 | inc esi | inc edi | jmp L0<

            ...Else_If al = '"'
                Mov eax esi
                Do
                    If B$esi >= SPC
                        Mov B$edi 3
                    Else
                        Mov B$edi 0
                    End_If
                    inc esi | inc edi | cmp esi edx | je L9>>
                Loop_Until B$esi = '"'
                Mov B$edi 3 | inc esi | inc edi | jmp L0<<

            ...Else_If al = '['
                Mov bl 2, B$edi 2
                inc esi | inc edi | jmp L0<<

            ...Else_If al = ']'
                Mov B$edi 2, bl 1
                inc esi | inc edi | jmp L0<<

            ...Else_If B$esi = ';'
                .If D$esi-1 = MLC
                    Mov eax esi
                    Do
                        If B$esi < SPC
                            Mov B$edi 0
                        Else
                            Mov B$edi 4
                        End_If
                        inc esi | inc edi | cmp esi edx | je L9>>
                    Loop_Until D$esi = MLC
                    Mov B$edi 0, W$edi+1 0404, B$edi+3 0
                    Mov B@Color 0
                    add esi 4 | add edi 4 | jmp L0<<
                .Else
                    Do
                        Mov B$edi 4
                        inc esi | inc edi | cmp esi edx | je L9>>
                    Loop_Until B$esi < SPC
                    Mov B@Color 0
                    jmp L0<<
                .End_If

            ...Else_If al < SPC
                Do
                    Mov B$edi 0
                    inc esi | inc edi | cmp esi edx | je L9>>
                Loop_Until B$esi >= SPC
                jmp L0<<

            ...Else_If al = 36
                Mov al B$ParagraphChar, B$esi al
                Mov B$edi bl | On B$WantSizeMarkerColor = &TRUE, xor B$edi 0011
                inc esi | inc edi | jmp L0<<

            ...Else_If al = 167
                Mov al B$ParagraphChar, B$esi al
                Mov B$edi bl | On B$WantSizeMarkerColor = &TRUE, xor B$edi 0011
                inc esi | inc edi | jmp L0<<

            ...Else_If al = '@'
                Mov B$edi bl
                On B$WantSizeMarkerColor = &TRUE, xor B$edi 0011
                inc esi | inc edi | jmp L0<<

            ...Else
L1:             Mov B$edi bl | inc esi | inc edi | jmp L0<<

            ...End_If

        .End_While

      ; Arase any remaining color flag at the end of the last line
        Mov ecx 100, al 0 | rep stosb

L9:     If D$FL.BlockInside = &TRUE

            Mov esi D@First,
                edx D@Last,
                eax D$LP.BlockStartText,
                ebx D$LP.BlockEndText

            On eax > ebx, xchg eax ebx

            On eax < esi, Mov eax esi
            On ebx > edx, Mov ebx edx

            sub eax D$CodeSource | add eax D$ColorsMap
            sub ebx D$CodeSource | add ebx D$ColorsMap | inc ebx

          ; Normal = 1 // Brackets = 2 // Strings = 3 // Comments = 4 // Selection 5
            While eax < ebx | On B$eax <> 0, Mov B$eax 5 | inc eax | End_While
        End_If
;;
        ..If B$WantSizeMarkerColor = &TRUE
            Mov esi D@First, edx D@Last, eax esi, ebx edx

            sub eax D$CodeSource | add eax D$ColorsMap
            sub ebx D$CodeSource | add ebx D$ColorsMap

            .While esi < edx
                .If B$esi = 167     ; Paragraph Char
                    If B$eax = 1
                        Mov B$eax 2
                    Else_If B$eax = 2
                        Mov B$eax 1
                    End_If
                .Else_If B$esi = 36 ; Dollar Char
                    If B$eax = 1
                        Mov B$eax 2
                    Else_If B$eax = 2
                        Mov B$eax 1
                    End_If
                .Else_If B$esi = '@'
                    If B$eax = 1
                        Mov B$eax 2
                    Else_If B$eax = 2
                        Mov B$eax 1
                    End_If
                .End_If
                
                inc eax | inc esi
            .End_While
        ..End_If
;;
EndP
____________________________________________________________________________________________

[ColorsMap: D$ ?
 ColorMapSize: D$ ?]

[LP.BlockStartText: D$ ?
 LP.BlockEndText: D$ ?
 ReverseBlock: D$ ?]

TextColorsMap:

    Mov eax D$SourceLen | add eax 300 | AlignOn 01000 eax

    If D$ColorMapSize < eax

        Mov D$ColorMapSize eax

        Call VirtualFree ColorsMap

        Call VirtualAlloc ColorsMap,
                          D$ColorMapSize

    End_If

ret
____________________________________________________________________________________________

[ParagraphChar: 36] ; 36 or 167 (Dollar or Paragraph)

TextOutput:
    Mov esi D$STRUCT.EditData@UpperLine, ebx esi | sub ebx D$CodeSource | add ebx D$ColorsMap
    Move D$STRUCT.EditData@Col D$FontWidth | Mov D$STRUCT.EditData@Line 0

    Push D$STRUCT.EditData@LineNumber
        inc D$STRUCT.EditData@LineNumber

        On D$STRUCT.EditData@RightScroll <> 0, Call AjustForRightScroll

        Push ebx
            Call 'GDI32.TextOutA' D$STRUCT.EditData@HDC, 0, D$STRUCT.EditData@Line, BlankLine, 1
        Pop ebx

      ; Set the Color for a Chunk of Text:
L0:     movzx eax B$ebx
        If al <> 5
            Mov eax D$StatementColor+eax*4-4
            Push esi, ebx
                Push eax
                    Call 'GDI32.SetBkColor' D$STRUCT.EditData@HDC D$ARVB.BackColor
                Pop eax
                Call 'GDI32.SetTextColor' D$STRUCT.EditData@HDC, eax
            Pop ebx, esi
        Else
            Push esi, ebx
                Call 'GDI32.SetBkColor' D$STRUCT.EditData@HDC D$CaretColor
                Call 'GDI32.SetTextColor' D$STRUCT.EditData@HDC D$ARVB.BackColor
            Pop ebx, esi
        End_If

      ; How many Chars in that Color Chunk, into ecx:
        Mov al B$ebx, ecx 0
        If al <> 0
            While B$ebx = al
                inc ebx | inc ecx
            End_While
        Else
          ; CR/LF:
            add ebx 2 | Mov ecx 2 | sub D$STRUCT.EditData@LineNumber 1
        End_If

      ; Output:
        Push esi, ebx, ecx
            ..If al <> 0
              ; Normal output:
                Call RowToX ecx
              ; > eax = Number of Pixels to output.
                Push eax
                  ; Limit to the available number of Chars:
                    add eax D$STRUCT.EditData@Col
                    If eax > D$EditRightPixel
                        Pop ecx
                            Mov ecx D$EditRightPixel | sub ecx D$STRUCT.EditData@Col
                        Push ecx
                        Call XToRow ecx | Mov ecx eax
                    End_If
                    .If ecx <> 1
                        Call 'GDI32.TextOutA' D$STRUCT.EditData@HDC, D$STRUCT.EditData@Col, D$STRUCT.EditData@Line, esi, ecx
                    .Else
                        If B$esi = 36
                            Call 'GDI32.TextOutA' D$STRUCT.EditData@HDC, D$STRUCT.EditData@Col, D$STRUCT.EditData@Line, ParagraphChar, ecx
                        Else_If B$esi = 167
                            Call 'GDI32.TextOutA' D$STRUCT.EditData@HDC, D$STRUCT.EditData@Col, D$STRUCT.EditData@Line, ParagraphChar, ecx
                        Else
                            Call 'GDI32.TextOutA' D$STRUCT.EditData@HDC, D$STRUCT.EditData@Col, D$STRUCT.EditData@Line, esi, ecx
                        End_If
                    .End_If
                Pop eax
                add D$STRUCT.EditData@Col eax

            ..Else
              ; CR/LF:
                Mov eax D$EditRightPixel
                If eax > D$STRUCT.EditData@Col
                  ; Here we "blank" the End of Line:
                    sub eax D$STRUCT.EditData@Col | Call XToRow eax
                    Push ecx
                        Call 'GDI32.TextOutA' D$STRUCT.EditData@HDC, D$STRUCT.EditData@Col, D$STRUCT.EditData@Line, BlankLine, eax
                    Pop ecx
                End_If

              ; And here, we Blank the next Row Zero (DCBP,...):
                Move D$STRUCT.EditData@col D$FontWidth | Mov eax D$FontHeight | add D$STRUCT.EditData@Line eax
                On D$STRUCT.EditData@LineNumber > 0, Call 'GDI32.TextOutA' D$STRUCT.EditData@HDC, 0, D$STRUCT.EditData@Line, BlankLine, 1

                If D$STRUCT.EditData@RightScroll <> 0
                    Pop ecx, ebx, esi
                        Call AjustForRightScroll
                    Push esi, ebx, ecx
                End_If

            ..End_If
        Pop ecx, ebx, esi

        add esi ecx | cmp D$STRUCT.EditData@LineNumber 0 | ja L0<<
    Pop D$STRUCT.EditData@LineNumber
ret
____________________________________________________________________________________________

;;
  Outputs the Char under the Cursor. Usefull in case when the Blinking Cursor is
  set On, in Config, and that the partial redraw is called by 'BlinkProc',
  particulary in OverWrite Mode, that, otherwise, would leave this char blanked.
;;

CharOutput:
    Mov esi D$STRUCT.EditData@UpperLine, eax 0, ebx 1, edx D$ColorsMap

    While esi < D$STRUCT.EditData@CurrentWritingPos
        If B$esi = CR
            add esi 2 | add edx 2
            Mov ebx 1 | inc eax
        Else
            inc ebx | inc esi | inc edx
        End_If
    End_While

    Call LineToY eax

    Push eax | Call RowToX ebx | Pop ebx

    pushad
        If B$edx <> 5
            Mov eax D$StatementColor+eax*4-4
            Push eax
                Call 'GDI32.SetBkColor' D$STRUCT.EditData@HDC D$ARVB.BackColor
            Pop eax
            Call 'GDI32.SetTextColor' D$STRUCT.EditData@HDC, eax
        Else
            Call 'GDI32.SetBkColor' D$STRUCT.EditData@HDC D$CaretColor
            Call 'GDI32.SetTextColor' D$STRUCT.EditData@HDC D$ARVB.BackColor
        End_If
    popad

    On B$esi <> CR, Call 'GDI32.TextOutA' D$STRUCT.EditData@HDC, eax, ebx, esi, 1
ret
____________________________________________________________________________________________

AjustForRightScroll:
    Mov eax D$STRUCT.EditData@RightScroll

L0: cmp B$ebx 0 | je L9>

    inc esi | inc ebx | dec eax | cmp eax 0 | ja L0<

L9: ret

;;
  We do not search directely for _the_ previous Color (far too complicated). We search
  for an upward Position that will give a proper start point for building the ColorMap.
  
  So, we:
  
  * exclude the Text Color (too many possible cases).
  
  * exclude the Comments cases (because of Multi-Lines Comments complications).
  
  We simply search for the next upper Line starting with either Statement or Data
  color. 'GetStartUpColorPos' return this Color (eax) and the according Pos in the
  user's SourceCode (esi). If Top of File is reached, we return the Statement Color.
;;

[PreviousStartUpColour: D$ ?
 PreviousStartUpColourPos: D$ ?]

GetStartUpColorPos:
    Mov esi D$STRUCT.EditData@UpperLine | sub esi D$CodeSource | add esi D$ColorsMap
    Mov edx D$ColorsMap

  ; CRLF = 0 // Normal = 1 // Brackets = 2 // Strings = 3 // Comments = 4 // Selection 5
    While esi > edx
        .If B$esi = 0
            If B$esi+1 = 1
                Mov eax 1 | jmp L5>
            Else_If B$esi+1 = 2
                Mov eax 2 | jmp L5>
            End_If

L0:         dec esi
        .End_If

        dec esi
    End_While

  ; Top of File reached:
    Move D$PreviousStartUpColourPos D$CodeSource
    Mov D$PreviousStartUpColour 1  | ret

  ; Start point found. Check cases of Local Symbols label at first Row:
L5: inc esi | Mov ebx esi
    sub ebx D$ColorsMap | add ebx D$CodeSource | On B$ebx = '@', jmp L0<

    sub esi D$ColorsMap | add esi D$CodeSource
    Mov D$PreviousStartUpColourPos esi
    Mov D$PreviousStartUpColour eax | ret
____________________________________________________________________________________________

____________________________________________________________________________________________

[PreviousUpperLine: D$ ?]

PrintColorText:
    On D$FL.SourceReady = &FALSE ret
   ; On D$H.TitleFont <> 0, ret

    Call TextColorsMap | Call InitPrintText | On D$STRUCT.EditData@LineNumber <s 1, ret

   ; If D$FL.CaretOnlyRedraw = &TRUE
   ;     Call RemoveCaretAndUnderline | Call CharOutput | Call PrintCaretAndUnderline
   ;     Mov D$FL.CaretOnlyRedraw &FALSE | jmp L9>>
   ; End_If

    Call GetLastCharPosOnScreen

    Mov eax D$PreviousUpperLine
    If eax <> D$STRUCT.EditData@Upperline
        Call SetColorsMap D$CodeSource, D$LastCharPosOnScreen, 1
    Else
        Call GetStartUpColorPos
        Call SetColorsMap D$PreviousStartUpColourPos,
                          D$LastCharPosOnScreen, D$PreviousStartUpColour
    End_If

    Call RemoveCaretAndUnderline | Call TextOutput | Call PrintCaretAndUnderline

    Move D$PreviousUpperLine D$STRUCT.EditData@UpperLine

    ;If D$BreakPointsTables <> 0
    ;    On D$RightScroll = 0, Call DrawTheRedPlots
    ;End_If

L9: ;Call DrawMarginLine

    Call ClosePrint
ret


PrintBreakPoint:
    On D$FL.BreakPointLineDrawn = &FALSE, Call DrawBreakPointLine

    If D$LP.MEM.TABLE.BreakPoints <> &NULL
        Call DrawTheRedPlotsBreakPoint
    Else
        Call BlankMargin
    End_If
ret


[FL.BreakPointLineDrawn: D$ ?]

[STRUCT.RECT.BreakPointLine:
 @X1: D$ ?
 @Y1: D$ ?
 @X2: D$ ?
 @Y2: D$ ?]

DrawBreakPointLine:

    Call 'USER32.BeginPaint' D$H.BreakPointWindow,
                             PAINTSTRUCT

    Mov D$STRUCT.EditData@HDC eax

    Call DrawTheBreakPointLine

    Call 'USER32.EndPaint' D$H.BreakPointWindow,
                           PAINTSTRUCT

    Mov D$FL.BreakPointLineDrawn &TRUE

ret

DrawTheBreakPointLine:

    Call 'USER32.GetClientRect' D$H.BreakPointWindow,
                                STRUCT.RECT.BreakPointLine

    Mov eax D$STRUCT.RECT.BreakPointLine@X2 | dec eax | Mov D$STRUCT.RECT.BreakPointLine@X1 eax

    Call 'USER32.FillRect' D$STRUCT.EditData@HDC,
                           STRUCT.RECT.BreakPointLine,
                           D$H.CaretBrush

ret
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
 Once user has set the caret, let us say, in the middle of a line, and then moves
 up or down, we do not want the caret horizontal position to be reset definitively
 at left if vertical movement crosses some empty lines (or to left if shorter). So:
 Caret has 2 possible horizontal positions: the one we see at screen (Physical)
 and a virtual one (or physical, if the same), that allow keeping a raw displacement
 while jumping on shorter lines. 'PhysicalCaretRow' is set by writing routine;
 'CaretRow' is set by actions routines. When user hits a key driving an horizontal
 mouvement, we clear this virtual difference if any:
;;

[MRealCaretRow
    Move D$STRUCT.EditData@CaretRow D$STRUCT.EditData@PhysicalCaretRow]

[MousePosX: D$ ?
 MousePosY: D$ ?]

[GetMousePos | Push D@Lparam | Pop W$MousePosX, W$MousePosY]

[ClickOnMargin: D$ ?
 DoubleClickOnMargin: D$ ?]

MouseTextPos:
    Call YtoLine D$MousePosY | Mov ebx eax
    Call XtoRow D$MousePosX

L9: ;If eax < 1
    ;    Mov B$ClickOnMargin &TRUE | Mov eax 1
    ;Else
    ;    Mov B$ClickOnMargin &FALSE
    ;End_If

    On eax = 0, Mov eax 1

  ; > eax = Row from start of line // ebx = Line
ret


; Same as above, but with no adjustement (this is for the Mouse Blocks Selections):

SimpleMouseTextPos:
   ; Mov eax D$MousePosX, ebx D$MousePosY
   ; shr eax 3 | shr ebx 4
    Call YtoLine D$MousePosY | Mov ebx eax
    Call XtoRow D$MousePosX

    On eax = 0, inc eax
ret                            ; > eax = col from start of line   ebx = line


DownOneLine:
    Push eax, ebx, ecx
        Mov esi D$STRUCT.EditData@UpperLine
L0:     lodsb | cmp al LF | jne L0<                     ; New pos OK:

        Mov ebx esi, ecx D$STRUCT.EditData@LineNumber
L0:     lodsb | On esi > D$STRUCT.EditData@SourceEnd, jmp L9>           ; abort if end of text found
            cmp al LF | jne L0<
        loop L0<
        Mov D$STRUCT.EditData@UpperLine ebx
L9: Pop ecx, ebx, eax
ret


UpOneLine:

    Push eax
        Mov esi D$STRUCT.EditData@UpperLine
        std
            lodsw
L0:         lodsb | cmp al LF | jne L0<
            add esi 2
            If esi >= D$CodeSource
                Mov D$STRUCT.EditData@UpperLine esi
            Else
                Move D$STRUCT.EditData@UpperLine D$CodeSource
            End_If
        cld
    Pop eax

ret


FullDown:
    Move D$STRUCT.EditData@UpperLine D$STRUCT.EditData@SourceEnd

  ; Call OnlyOnePageUp
    Mov ecx D$STRUCT.EditData@LineNumber | On ecx > 1, dec ecx
L0: Call UpOneLine | loop L0<

    Call OnlyOnePageDown
ret


OnlyOnePageDown:
L1: Mov ebx D$STRUCT.EditData@LineNumber

    If D$STRUCT.EditData@CaretLine < ebx
        Mov esi D$STRUCT.EditData@UpperLine, ecx D$STRUCT.EditData@LineNumber, ebx 0
L2:     lodsb | On esi > D$STRUCT.EditData@SourceEnd, jmp L3>
        cmp al LF | jne L2<
        inc ebx
        loop L2<
L3:     Mov D$STRUCT.EditData@CaretLine ebx
    Else
        Mov ecx ebx | On ecx > 1, dec ecx
L4:     Push ecx
            Call DownOneLine
        Pop ecx | loop L4<
    End_If
ret


FullUp:
    Move D$STRUCT.EditData@UpperLine D$CodeSource | Mov D$STRUCT.EditData@CaretLine 0
ret

OnlyOnePageUp:
    Mov eax D$STRUCT.EditData@LineNumber
    If D$STRUCT.EditData@CaretLine > 0
        Mov D$STRUCT.EditData@CaretLine 0
    Else
        Mov ecx eax | On ecx > 1, dec ecx
L0:     Call UpOneLine | loop L0<
    End_If
ret


[BlockAutoDelete: D$ ?]

AutoDeleteBlock:
    If B$OldBlockInside = &TRUE
        .If B$SimulatedBlock <> &TRUE
            Push eax
                Mov D$FL.BlockInside &TRUE | Call ControlD | Call AskForRedrawNow
            Pop eax
        .End_If
    End_If
ret


[RIGHT_FEED 8]

OverwriteSource:
    On B$BlockAutoDelete = &TRUE, Call AutoDeleteBlock

    Mov edi D$STRUCT.EditData@CurrentWritingPos
    On B$edi = CR, jmp InsertSource
  ;  On eax = TAB, jmp TabCarret
    Mov cl B$edi

    On al < 32, ret

    stosb

    Push eax
        Mov eax D$STRUCT.EditData@ColNumber
        If D$STRUCT.EditData@CaretRow < eax
            inc D$STRUCT.EditData@CaretRow
        Else
            add D$STRUCT.EditData@RightScroll RIGHT_FEED
            sub D$STRUCT.EditData@CaretRow 7
        End_If
    Pop eax

    Call DoStoreOverWrite
ret


[InsertedChar: D$ ?]

InsertSource:   ; eax = Char.

    On B$BlockAutoDelete = &TRUE, Call AutoDeleteBlock

InsertSourceOnBlockIndent:

    MRealCaretRow

    Agree  al = 13, al = 10, al = TAB
    Reject  al < 32  ;, al > 126

    Mov esi D$STRUCT.EditData@SourceEnd | add esi 400 | Mov edi esi      ; 400 is security 13/10/...
    Mov ecx esi | sub ecx D$STRUCT.EditData@CurrentWritingPos | inc ecx

    If al = TAB
        Mov B$InsertedChar SPC | jmp L2>
    Else
        Mov B$InsertedChar al
    End_If

L1: inc edi
    std
        rep movsb | stosb
    cld
    Mov ebx 1 | jmp L3>

L2: Mov eax D$STRUCT.EditData@CaretRow | add eax D$STRUCT.EditData@RightScroll
    add eax D$TabIs | dec eax
    Mov ebx D$TabIs | neg ebx | and eax ebx
    Mov ebx eax
    sub ebx D$STRUCT.EditData@CaretRow | sub ebx D$STRUCT.EditData@RightScroll | inc ebx
    add edi ebx
        std
            rep movsb | Mov al SPC, ecx ebx | rep stosb
        cld

L3: add D$SourceLen ebx | add D$STRUCT.EditData@CurrentWritingPos ebx | add D$STRUCT.EditData@SourceEnd ebx

    Mov eax D$STRUCT.EditData@ColNumber

    cmp D$STRUCT.EditData@CaretRow  eax | jae L4>
      add D$STRUCT.EditData@CaretRow ebx | jmp L9>
L4:   Push ebx
      AlignOn RIGHT_FEED ebx | add D$STRUCT.EditData@RightScroll ebx | sub D$STRUCT.EditData@CaretRow ebx | add D$STRUCT.EditData@CaretRow 1
      Pop ebx

L9: Call DoStoreInsert

    .If B$CompletionWanted = &TRUE
      ; To prevent from runing when doing the Substitution:
        If B$CompletionRuning = &FALSE
            Call CodeComplete | Mov B$CompletionRuning &FALSE
        End_If
    .End_If

ret
____________________________________________________________________________________________

InsertDoubleByte:
    cmp ah 0 | jne L1>
    Call InsertSource
    ret

L1: Mov esi D$STRUCT.EditData@SourceEnd | add esi 400 | Mov edi esi
    Mov ecx esi | sub ecx D$STRUCT.EditData@CurrentWritingPos | inc ecx

L2: add edi 2
        std
            rep movsb | stosb | Mov al ah | stosb
        cld
        Mov ebx 2

L3: add D$SourceLen ebx | add D$STRUCT.EditData@CurrentWritingPos ebx | add D$STRUCT.EditData@SourceEnd ebx

    Mov eax D$STRUCT.EditData@ColNumber

    cmp D$STRUCT.EditData@CaretRow  eax | jae L4>
    add D$STRUCT.EditData@CaretRow ebx | jmp L9>
L4: Push ebx
        AlignOn RIGHT_FEED ebx | add D$STRUCT.EditData@RightScroll ebx | sub D$STRUCT.EditData@CaretRow ebx | sub D$STRUCT.EditData@CaretRow 1
    Pop ebx
L9: Call DoStoreInsert
ret
____________________________________________________________________________________________

;;
 [Carriage Return] includes an indent feature. As many spaces may have to be added
 at once, we do not simply Call to "InsertSource" for each but, instead insert all
 spaces at once for speed reasons (we make a Move on the entire tail of file...):
;;

[AutoIndent: 0   AutoIndentFlag: &TRUE]

SetIndent:
    Push eax
        Mov D$AutoIndent 0, esi D$STRUCT.EditData@CurrentWritingPos

L0:     dec esi | cmp B$esi LF | ja L0<

        inc esi                             ; Start of Current Line.

        .If B$esi+2 = ':'
            add esi 3 | Mov D$AutoIndent 3
            If esi > D$STRUCT.EditData@CurrentWritingPos
              ; Cases of CRLF from  a Pos before a Local Label:
                sub esi 3 | sub D$AutoIndent 3
            End_If
        .End_If

        While esi < D$STRUCT.EditData@CurrentWritingPos
            lodsb | cmp al SPC | jne L9>
                inc D$AutoIndent
        End_While
L9: Pop eax
ret


CarriageReturn:
    .If B$BlockAutoDelete = &TRUE
        If B$OldBlockInside = &TRUE
            Call AutoDeleteBlock | ret
        End_If
    .End_If

    On B$AutoIndentFlag = &TRUE, Call SetIndent
    Mov eax CR | Call InsertSource | Mov eax LF | Call InsertSource

    If D$AutoIndent > 0                                ; instead of "InsertSource":
        Mov esi D$STRUCT.EditData@SourceEnd | add esi 400
        Mov edi esi | add edi D$AutoIndent | Mov ecx esi
        sub ecx D$STRUCT.EditData@CurrentWritingPos | inc ecx
        std
            rep movsb | Mov ecx D$AutoIndent, al SPC | rep stosb
        cld
        Mov eax D$AutoIndent
        add D$SourceLen eax | add D$STRUCT.EditData@CurrentWritingPos eax | add D$STRUCT.EditData@SourceEnd eax
    End_If

    Mov D$STRUCT.EditData@RightScroll 0 | Move D$STRUCT.EditData@CaretRow D$AutoIndent
    inc D$STRUCT.EditData@CaretRow | Mov eax D$STRUCT.EditData@LineNumber ;;;| Mov D$AutoIndent 0   ; in case flag changed

    If D$STRUCT.EditData@CaretLine = eax
        Call DownOneLine
    Else
        add D$STRUCT.EditData@CaretLine 1
    End_If

    Mov ebx D$AutoIndent     ; If AutoIndent on, we have to memorize this event now
    If ebx > 0               ; (after CR/LF recording. Undo will hold 2 jobs for this:
        Mov D$InsertedChar SPC
        Call DoStoreInsert   ; one for undo indent and another for undo CR/LF).
    End_If
ret
____________________________________________________________________________________________

;;
 Instead of searching if end of text is on screen, we try to Move one line up: if it
 is possible, we try to Move one line down. If it is unpossible, end of text is on
 screen and the one-line-up mouvement is validate:
;;

[CanGoUp: D$ ?
 CanGoDown: D$ ?]

TryToMove:
    On D$STRUCT.EditData@LineNumber <s 1 ret

L0: Mov B$CanGoUp &FALSE, B$CanGoDown &FALSE,  esi D$STRUCT.EditData@UpperLine     ; try Up One Line:
    std
        lodsw
L1:     lodsb | cmp al LF | ja L1<
        add esi 2
    cld
    Mov edx esi | On esi > D$CodeSource, Mov B$CanGoUp &TRUE

    Mov esi D$STRUCT.EditData@UpperLine, ecx D$STRUCT.EditData@LineNumber       ; try down one line:

L1: lodsb | cmp al LF | ja L1<
    loop L1<
    add esi 2 | On esi < D$STRUCT.EditData@SourceEnd, Mov B$CanGoDown &TRUE

    On B$CanGoDown = &TRUE, ret
    On B$CanGoUp = &FALSE, ret

    Mov D$STRUCT.EditData@UpperLine edx | add D$STRUCT.EditData@CaretLine 1       ; up one line (down false / up true)
    jmp L0<
____________________________________________________________________________________________

ResetCaretOnStripTab:
    pushad
        std
L0:         lodsb | cmp al LF | jne L0<
        cld
        add esi 2 | Mov ecx 0
L0:     lodsb | inc ecx | cmp al 255 | je L9>
            cmp al TAB | jne L0<
                AlignOn 8 ecx | jmp L0<
L9:     inc ecx | Mov D$STRUCT.EditData@CaretRow ecx, D$STRUCT.EditData@PhysicalCaretRow ecx
    popad
ret


StripBackSpace:
    Mov esi D$STRUCT.EditData@CurrentWritingPos
    If B$esi-1 = TAB
        Mov B$esi-1 255 | Call ResetCaretOnStripTab
    End_If
    Mov edi esi | dec edi, D$STRUCT.EditData@CurrentWritingPos, D$SourceLen, D$STRUCT.EditData@SourceEnd
    Mov ecx D$STRUCT.EditData@SourceEnd | sub ecx D$STRUCT.EditData@CurrentWritingPos | add ecx 0100 | rep movsb
ret


BackSpace:
    ..If B$BlockAutoDelete = &TRUE
        .If B$OldBlockInside = &TRUE
            If al <> TAB
                Call AutoDeleteBlock | ret
            End_If
        .End_If
    ..End_If
EraseLastChar:
    MRealCaretRow | On D$SourceLen = 0, ret
    Mov eax D$STRUCT.EditData@CurrentWritingPos | On eax = D$CodeSource, ret
    Mov ebx BACK | Call DoStoreCharDelete
    Call StripBackSpace

    .If D$STRUCT.EditData@RightScroll = 0
        sub D$STRUCT.EditData@CaretRow 1
    .Else
        If D$STRUCT.EditData@CaretRow = 1
            sub D$STRUCT.EditData@RightScroll RIGHT_FEED
            add D$STRUCT.EditData@CaretRow (RIGHT_FEED-1)
        Else
            sub D$STRUCT.EditData@CaretRow 1
        End_If
    .End_If

    ...If D$STRUCT.EditData@CaretRow = 0
        Mov esi D$STRUCT.EditData@CurrentWritingPos
        If D$STRUCT.EditData@CaretLine = 0
            Call UpOneLine
        Else
            sub D$STRUCT.EditData@CaretLine 1
        End_If
        std | lodsb                                        ; strip CR
L0:         lodsb | cmp al LF | jne L0<
        cld
        lodsw                                              ; strip CR/LF
L0:     lodsb | add D$STRUCT.EditData@CaretRow 1 | cmp al TAB | jne L1>
        or D$STRUCT.EditData@CaretRow 00_111 | inc D$STRUCT.EditData@CaretRow | jmp L0<
L1:     cmp al CR | ja L0<
        Mov ebx BACK | Call DoStoreCharDelete | Call StripBackSpace | Call TryToMove
    ...Else
;;
  When called from 'SimulateBlockForBackIndent' / 'IsItBlockIndent' / 'RetrieveBlockIndent'
  don't play: [Shift][Tab] is going on, not [Shift][BackSpace]. The 'OldBlockInside' Flag
  let us know of it:
;;
        ..If B$keys+&VK_SHIFT = &TRUE
            .If B$OldBlockInside = &FALSE
                Mov eax D$STRUCT.EditData@CaretRow | dec eax | Mov ebx D$TabIs | dec ebx | and eax ebx
                If eax <> 0
                    Call AskForRedrawNow
                    Mov eax D$STRUCT.EditData@CurrentWritingPos
                    On B$eax-1 = SPC, jmp BackSpace
                End_If
            .End_If
        ..End_If
    ...End_If
ret


KeyDown:
    Mov eax D$STRUCT.EditData@LineNumber

    If D$STRUCT.EditData@CaretLine >= eax
        Call DownOneLine
    Else
        Mov esi D$STRUCT.EditData@CurrentWritingPos
L0:     lodsb | On esi > D$STRUCT.EditData@SourceEnd, ret
        cmp al LF | ja L0<
            add D$STRUCT.EditData@CaretLine 1
    End_If
ret


KeyUp:
    If D$STRUCT.EditData@CaretLine = 0
      Call UpOneLine
    Else
      sub D$STRUCT.EditData@CaretLine 1
    End_If

ret


StartOfLine:
    Mov D$STRUCT.EditData@RightScroll 0, eax 1

    Mov esi D$STRUCT.EditData@CurrentWritingPos | While B$esi-1 <> LF | dec esi | End_While

    .If B$esi+2 = ':'
        add esi 3 | add eax 3
        If B$esi = ':'
            inc esi | inc eax
        End_If
    .End_If
    While B$esi = SPC | inc esi | inc eax | End_While

    If D$STRUCT.EditData@CaretRow = 1
        Mov D$STRUCT.EditData@CaretRow eax
    Else_If D$STRUCT.EditData@CaretRow = eax
        Mov D$STRUCT.EditData@CaretRow 1
    Else
        Mov D$STRUCT.EditData@CaretRow eax
    End_If
ret


KeyLeft:
    MRealCaretRow
SimpleKeyLeft:
    .If D$STRUCT.EditData@RightScroll > 0
        If D$STRUCT.EditData@CaretRow = 1
            sub D$STRUCT.EditData@RightScroll RIGHT_FEED
            add D$STRUCT.EditData@CaretRow (RIGHT_FEED-1)
        Else
            sub D$STRUCT.EditData@CaretRow 1
        End_If
    .Else_If D$STRUCT.EditData@CaretRow > 1
        sub D$STRUCT.EditData@CaretRow 1
    .End_If
ret
____________________________________________________________________________________________

; When user selects a Block with the KeyBoard, when he goes Down, and the start point of
; the selection is, say, at row 52, when he stops on a Line with 5 Chars, and hits the
; Left Arrow, he does not mean to wait that the Real Carret Col achieves moving back to
; the end of the 5 Chars Line, but, directely to unselect the 5th Char. For normal use,
; this complicated computation is done both by 'KeyLeft' and by 'PrintCaretAndUnderline',
; but, with Block Selection, there is no Call to 'PrintCaretAndUnderline'? So:

SetPhysicalCaretRow:
    Mov esi D$STRUCT.EditData@UpperLine, ecx 0

L0: cmp ecx D$STRUCT.EditData@CaretLine | je L2>                    ; search for Caret line (esi)
L1: lodsb | cmp al LF | jne L1<
    inc ecx | jmp L0<

L2: Mov ecx 0
    On D$STRUCT.EditData@RightScroll > 0 sub ecx D$STRUCT.EditData@RightScroll

L0: cmp ecx D$STRUCT.EditData@CaretRow | je L5>>
L1: lodsb | cmp al CR | ja L2>                      ; search for Caret col (esi too)
        inc ecx
         Mov D$STRUCT.EditData@PhysicalCaretRow ecx | jmp L6>>      ; Caret at end of line
L2: inc ecx | jmp L0<
L6: ret


StartOfWord:  ; KeyLeft
    Mov esi D$STRUCT.EditData@CurrentWritingPos
    While B$esi-1 = SPC | Call SimpleKeyLeft | dec esi | End_While
    While B$esi-1 > SPC | Call SimpleKeyLeft | dec esi | End_While
    Mov D$STRUCT.EditData@CurrentWritingPos esi
ret



EndOfLine:
    Mov esi D$STRUCT.EditData@CurrentWritingPos | Mov ecx D$STRUCT.EditData@ColNumber

L0: While B$esi <> CR
        If D$STRUCT.EditData@CaretRow < ecx
            inc D$STRUCT.EditData@CaretRow
        Else
            inc D$STRUCT.EditData@RightScroll
        End_If
        inc esi
    End_While

    AlignOn RIGHT_FEED D$STRUCT.EditData@RightScroll
ret


KeyRight:
    MRealCaretRow

SimpleKeyRight:
    Mov eax D$STRUCT.EditData@ColNumber

    If D$STRUCT.EditData@CaretRow < eax
        inc D$STRUCT.EditData@CaretRow
    Else
        add D$STRUCT.EditData@RightScroll RIGHT_FEED
        sub D$STRUCT.EditData@CaretRow 7
    End_If

    If D$STRUCT.EditData@CaretEndOfLine = &TRUE
        .If D$STRUCT.EditData@RightScroll > 0
            If D$STRUCT.EditData@CaretRow = 1
                sub D$STRUCT.EditData@RightScroll RIGHT_FEED
            End_If
        .Else
            dec D$STRUCT.EditData@CaretRow
        .End_If
    End_If
ret


EndOfWord:
    Mov esi D$STRUCT.EditData@CurrentWritingPos
    While B$esi = SPC | Call SimpleKeyRight | inc esi | End_While
    While B$esi > SPC | Call SimpleKeyRight | inc esi | End_While
    Mov D$STRUCT.EditData@CurrentWritingPos esi
ret


KeyInsert:
    xor B$CaretColor 00_01101001 | xor D$STRUCT.EditData@Overwrite &TRUE
ret


StripOneChar:
    inc D$STRUCT.EditData@CurrentWritingPos | Mov ebx FORTH | Call DoStoreCharDelete
    sub D$STRUCT.EditData@CurrentWritingPos 1
    Mov esi D$STRUCT.EditData@CurrentWritingPos, edi esi | inc esi | dec D$SourceLen, D$STRUCT.EditData@SourceEnd
    Mov ecx D$STRUCT.EditData@SourceEnd | sub ecx D$STRUCT.EditData@CurrentWritingPos
    add ecx 0100 | rep movsb
ret


KeyDelete:
    .If B$BlockAutoDelete = &TRUE
        If D$FL.BlockInside = &TRUE
            Call ControlD | ret
        End_If
    .End_If

    MRealCaretRow | On D$SourceLen = 0, ret
    Mov eax D$STRUCT.EditData@CurrentWritingPos
    If eax < D$STRUCT.EditData@SourceEnd
        Call StripOneChar | Mov esi D$STRUCT.EditData@CurrentWritingPos
        On B$esi = LF, Call StripOneChar
    End_If
ret

; (CaretPos to Source Pos).
; This is to translate screen line / col coordinates in one text pointer:
; input eax = Row; ebx = Line
; output eax = ptr

SearchTxtPtr:
    Mov esi D$STRUCT.EditData@UpperLine, ecx 0, edx D$STRUCT.EditData@SourceEnd
    Push eax
L0:     cmp ecx ebx | je L2>
L1:
            lodsb | cmp esi edx | ja L2>
            cmp al LF | jne L1<
                inc ecx | jmp L0<

L2: Pop eax | Mov ebx eax, ecx 0               ; switch eax (col Pos) > ebx
    add ebx D$STRUCT.EditData@RightScroll

L3: cmp esi edx | ja L9>
L3: lodsb | cmp esi edx | ja L9>
    cmp B$esi LF | je L8>
            inc ecx | cmp ecx ebx | jb L3<
                jmp L9>

L8: sub ebx ecx | sub D$STRUCT.EditData@CaretRow ebx | inc D$STRUCT.EditData@CaretRow

L9: dec esi | Mov eax esi

    If D$STRUCT.EditData@CaretRow <s 1
        Mov D$STRUCT.EditData@CaretRow 1 | Mov D$STRUCT.EditData@RightScroll 0
    End_If
ret

____________________________________________________________________________________________

; In fact, sets the Caret and the Current Writing Position at the next Char after
; the Parameter.

Proc SetCaret:
    Argument @Location

        Mov esi D$STRUCT.EditData@UpperLine, D$STRUCT.EditData@CaretRow 1, D$STRUCT.EditData@CaretLine 0

      ; Very rude and durty hak: There is a problem when the Search Box is runing and the
      ; user Click on a TITLE Tab... (temporary security >> To do analyze step by step what
      ; is going on when crossing these two...).
        Mov eax D@Location
        If eax < D$CodeSource
            Move D@Location D$CodeSource
        Else_If eax > D$STRUCT.EditData@SourceEnd
            Move D@Location D$STRUCT.EditData@SourceEnd
        End_If

        While esi < D@Location
            If B$esi = LF
                Mov D$STRUCT.EditData@CaretRow 1 | inc D$STRUCT.EditData@CaretLine
            Else
                inc D$STRUCT.EditData@CaretRow
            End_If
            inc esi
        End_While
        inc D$STRUCT.EditData@CaretRow | Mov D$STRUCT.EditData@CurrentWritingPos esi
EndP

[STRUC.RECT.Confined:
 D$ ?
 D$ ?
 D$ ?
 D$ ?]

[UserClickAfterEnd: D$ ?
 TrueCaretRow: D$ ?]

LeftButtonSimulation:
    Mov D$FL.BlockInside &FALSE, B$UserClickAfterEnd &FALSE
    Call MouseTextPos |  | jmp L2>>

LeftButton:
    Mov D$FL.BlockInside &FALSE, B$UserClickAfterEnd &FALSE, B$ClickOnMargin &FALSE

    If eax = D$H.EditWindow
        Call MouseTextPos | Mov D$STRUCT.EditData@CaretRow eax, D$STRUCT.EditData@CaretLine ebx
    Else_If eax = D$H.BreakPointWindow
        Mov B$ClickOnMargin &TRUE
        Call MouseTextPos | Mov D$STRUCT.EditData@CaretRow 1, eax 1, D$STRUCT.EditData@CaretLine ebx

    End_If

    .If B$ClickOnMargin = &TRUE
L1:    ;jmp DoubleClickMarginAction

        On D$LP.MEM.TABLE.BreakPoints = &NULL Call InitBreakPointsTables

        Call IsBreakPointHere

        If D$FL.InsideBreakPointTable = &TRUE
            Call DeleteBreakPoint | Call DoStoreRemoveBreakPoint
        Else
            Call SetBreakPoint | Call DoStoreBreakPoint
        End_If

        Mov B$UserClickAfterEnd &TRUE | jmp L9>>
    .End_If

L2: Mov D$STRUCT.EditData@PhysicalCaretRow eax, D$STRUCT.EditData@CaretRow eax

    Mov D$STRUCT.EditData@CaretLine ebx

L1: Call SearchTxtPtr | Mov D$ShiftDown eax  ; Ready for Block Selection

    If eax < D$STRUCT.EditData@SourceEnd
        Mov D$STRUCT.EditData@CurrentWritingPos eax
    Else
        Mov B$UserClickAfterEnd &TRUE
        Call SetCaret D$STRUCT.EditData@SourceEnd
       ; Mov eax D$CaretRow, ebx D$CaretLine | dec ebx | Mov D$CaretLine ebx | jmp L1<
    End_If

    Call 'USER32.GetClientRect' D$H.EditWindow,
                                STRUC.RECT.Confined

    Call 'USER32.ClientToScreen' D$H.EditWindow,
                                 STRUC.RECT.Confined

  ;  lea eax D$STRUC.RECT.Confined+RIGHT

    Call 'USER32.ClientToScreen' D$H.EditWindow, STRUC.RECT.Confined+RIGHT

    Mov eax D$FontWidth | sub D$STRUC.RECT.Confined+LEFT eax

    Call 'USER32.ClipCursor' STRUC.RECT.Confined

L9: ret


[StartBlockCol: D$ ?
 StartBlockLine: D$ ?
 EndBlockCol: D$ ?
 EndBlockLine: D$ ?
 FL.BlockInside: D$ ?
 BlockRedraw: D$ ?
 ShiftBlockInside: D$ ?]

[MovingBlock: D$ ?
 BlockMoveDelay: D$ ?
 BlockMoveTicks: D$ ?]

[PreviousMouseLine: D$ ?
 PreviousMouseCol: D$ ?
 FirstBlockDraw: D$ ?]

SetBlock:
   ; Slow down the Block moving up or down (by mouse action). Speed increases while moving:
    .If B$MovingBlock = &TRUE
        If D$BlockMoveTicks = 0
            Call 'KERNEL32.GetTickCount'
            add eax D$BlockMoveDelay | Mov D$BlockMoveTicks eax
        Else
            Do
                Call 'KERNEL32.GetTickCount'
            Loop_Until eax > D$BlockMoveTicks
            add eax D$BlockMoveDelay | Mov D$BlockMoveTicks eax
            dec D$BlockMoveDelay | jnz L0>
                Mov D$BlockMoveDelay 1
        End_If

    .Else
        Mov D$BlockMoveTicks 0, D$BlockMoveDelay 60

    .End_If

  ; Set the new Block:
L0: If D$FL.BlockInside = &TRUE
        Move D$STRUCT.EditData@PhysicalCaretRow D$STRUCT.EditData@CaretRow
        Mov D$STRUCT.EditData@CaretEndOfLine &FALSE
    Else
      ; D$ShiftDown is now set in 'LeftButton' (from WM_LBUTTONDOWN Case).
        Mov eax D$STRUCT.EditData@CurrentWritingPos

        Mov D$LP.BlockStartText eax,
            D$LP.BlockEndText eax

        Call SimpleMouseTextPos
        Mov D$STRUCT.EditData@CaretLine ebx, D$STRUCT.EditData@CaretRow eax
        Mov D$PreviousMouseLine ebx, D$PreviousMouseCol eax
        Mov D$ShiftBlockLine ebx, D$ShiftBlockCol eax
        Mov B$FirstBlockDraw &TRUE, B$BlockRedraw &TRUE, D$FL.BlockInside &TRUE | ret
    End_If

  ; Do not run for nop with an empty Block:
    Call SimpleMouseTextPos | Call SearchTxtPtr
    If eax = D$ShiftDown
        Mov D$FL.BlockInside &FALSE | ret
    End_If

    Call SimpleMouseTextPos | Mov D$STRUCT.EditData@PhysicalCaretRow eax, D$STRUCT.EditData@CaretRow eax

  ; Vertical Scrolling, if needed:
    Push eax, ebx
        If ebx = 0
            Push D$STRUCT.EditData@UpperLine
                Call UpOneLine
            Pop eax
            On D$STRUCT.EditData@UpperLine < eax, Mov D$MovingBlock &TRUE
        Else_If ebx >= D$STRUCT.EditData@LineNumber
            Push D$STRUCT.EditData@UpperLine
                Call DownOneLine
            Pop eax
            On D$STRUCT.EditData@UpperLine > eax, Mov D$MovingBlock &TRUE
        Else
            Mov B$MovingBlock &FALSE
        End_If
    Pop ebx, eax

  ; Don't redraw for moves after End-of-Line (lazy reuse of SearchTxtPtr to ajust Row):
    Push ebx | Call SearchTxtPtr | Pop ebx | Mov eax D$STRUCT.EditData@CaretRow

  ; Compute and Redraw only if different:
    If B$FirstBlockDraw = &TRUE
        Mov B$FirstBlockDraw &FALSE | jmp L1>
    Else_If B$MovingBlock = &TRUE
        jmp L1>
    Else_If eax <> D$PreviousMouseCol
        jmp L1>
    Else_If ebx <> D$PreviousMouseLine
L1:     Mov D$PreviousMouseLine ebx, D$PreviousMouseCol eax
        Mov D$STRUCT.EditData@CaretRow eax, D$STRUCT.EditData@CaretLine ebx

            Call SetShiftBlock | Mov B$ShiftBlockInside &FALSE

        Mov B$BlockRedraw &TRUE
    Else
        Mov B$BlockRedraw &FALSE
    End_If
ret
____________________________________________________________________________________________

; "On B$ebx = CR, dec D$BlockEndTextPtr" is because when a block-end-pointer points
; to an empty line, it is right upon the CR of next line. I spent much time to understand
; that, but it is normal. If it was a simple Caret, this would really be the good place.

LeftButtonUp:

    Call 'USER32.IsMenu' D$H.MenuFloatBreakPoint

    On eax = &TRUE Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint

    Call 'USER32.ClipCursor' &NULL

    On D$FL.BlockInside = &FALSE ret

    If B$UserClickAfterEnd = &TRUE

        Mov D$FL.BlockInside &FALSE | ret

    End_If

    Mov B$MovingBlock &FALSE

ret

    ; Should be no more use, with new Block Routines:
    Mov eax D$LP.BlockStartText

    cmp eax D$LP.BlockEndText | jna L5>

        Push eax,
             D$LP.BlockEndText

        Pop D$LP.BlockStartText,
            D$LP.BlockEndText

L5: Mov edx D$LP.BlockEndText

    On B$edx = CR sub D$LP.BlockEndText (1*ASCII)

    Mov eax D$STRUCT.EditData@RightScroll | add D$LP.BlockStartText eax | add D$LP.BlockEndText eax

ret
____________________________________________________________________________________________

; Block action routines:

[hBlock: D$ ?
 BlockSize: D$ ?
 ClipBoardPTR: D$ ?
 ClipBoardLen: D$ ?]

ControlC:
    cmp D$FL.BlockInside &FALSE | je L9>>

    Call 'USER32.OpenClipboard' D$H.MainWindow | cmp eax 0 | je L9>>
    Call 'USER32.EmptyClipboard' | cmp eax 0 | je L8>>

    Mov ecx D$LP.BlockEndText | sub ecx D$LP.BlockStartText | add ecx (1*ASCII)

    Mov D$BlockSize ecx | Mov ebx ecx | inc ebx
    Call 'KERNEL32.GlobalAlloc' &GMEM_DDESHARE, ebx | cmp eax 0 | je L8>  ; > eax = handle
    Mov D$hBlock eax
    Call 'KERNEL32.GlobalLock' eax                                       ; > eax = adress
    Mov edi eax, esi D$LP.BlockStartText, ecx D$BlockSize
    rep movsb | Mov al 0 | stosb
    Call 'KERNEL32.GlobalUnlock' D$hBlock
    Call 'USER32.SetClipboardData' &CF_TEXT, D$hBlock

L8: Call 'USER32.CloseClipboard'
L9: ret


ControlY:
    If D$H.DebugDialog <> 0
        Call CloseDebuggerOrIgnore | On eax = &IDNO jmp L9>>
    End_If

    Mov esi D$STRUCT.EditData@CurrentWritingPos
    While esi > D$CodeSource
        dec esi | cmp B$esi CR | je L1>
    End_While

L1: Mov D$LP.BlockStartText esi
    While esi < D$STRUCT.EditData@SourceEnd
        inc esi | cmp B$esi CR | je L1>
    End_While

L1: sub esi (1*ASCII) | Mov D$LP.BlockEndText esi

    If esi > D$LP.BlockStartText
        Push D$STRUCT.EditData@CaretRow
            Mov d$FL.BlockInside &TRUE | Call ControlD | Call KeyDown
        Pop D$STRUCT.EditData@CaretRow
    End_If
ret


ControlD:
    cmp D$FL.BlockInside &FALSE | je L9>>
    jmp L0>

ControlX:
    cmp D$FL.BlockInside &FALSE | je L9>>
    Call ControlC

L0: If D$H.DebugDialog <> 0
        Call CloseDebuggerOrIgnore | On eax = &IDNO jmp L9>>
    End_If

    Call WriteUndoBlockFileFromBlock | Call StoreUserActionOfBlockDeletion

    Mov eax D$STRUCT.EditData@CaretRow, ebx D$STRUCT.EditData@CaretLine | Call SearchTxtPtr

    ...If eax <> D$LP.BlockStartText

        std                                         ; reset cursor at "BlockStartText":
            Mov esi D$LP.BlockEndText
L0:
            cmp esi D$LP.BlockStartText | je L2>

                lodsb
                cmp al LF | jne L0<
                If esi < D$LastCharPosOnScreen         ; do not DEC Caret Line if block bigger
                    sub D$STRUCT.EditData@CaretLine 1 | jc L1>      ; than screen "upward drawn" by user
                End_If
                jmp L0<
L1:         Mov D$STRUCT.EditData@CaretLine 0 | jmp L0<             ; same, but "downward drawn" by user

L2:         Mov D$STRUCT.EditData@CaretRow 1 | dec esi              ; search for Col:
L3:         lodsb | cmp al LF | je L4>              ; count how many chars between start of block
                ;cmp esi D$CodeSource | jbe L4>      ; and left edge
                If esi <= D$CodeSource
                    add D$STRUCT.EditData@CaretRow 2 | jmp L4>
                End_If
                inc D$STRUCT.EditData@CaretRow | jmp L3<

L4:         .If esi < D$STRUCT.EditData@Upperline                   ; rePos screen if block greater than screen:
                Mov eax D$STRUCT.EditData@UpperLine
                If eax > D$CodeSource
                    add esi 2
                    Mov D$STRUCT.EditData@Upperline esi
                    Mov D$STRUCT.EditData@CaretLine 0
                Else
                    inc D$STRUCT.EditData@CaretRow
                End_If
            .End_If

            Move D$STRUCT.EditData@PhysicalCaretRow D$STRUCT.EditData@CaretRow
        cld

        Move D$STRUCT.EditData@CurrentWritingPos D$LP.BlockStartText

    ...End_If

UndoControlV:                                   ; strip text:

    Mov edi D$LP.BlockStartText, esi D$LP.BlockEndText | add esi (1*ASCII)

    Mov eax esi | sub eax edi                   ; eax = Block-to-strip-out length
    Push eax
        Mov ecx D$STRUCT.EditData@SourceEnd | add ecx 100 | sub ecx esi | rep movsb
    Pop eax
    sub D$STRUCT.EditData@SourceEnd eax | sub D$SourceLen eax | jnc L0>
        Mov D$SourceLen 0

  ; Should be no use. just to be absolutely sure no wrong CR/LF after End Of Source:
L0: Mov edi D$STRUCT.EditData@SourceEnd | Mov eax 0A0D0A0D, ecx 100 | rep stosd
    Call TryToMove                              ; ensure screen Pos in all cases.

    Mov D$FL.BlockInside &FALSE

;;
  This rebuild should be no use (???), and switches to the Next TITLE, in case of
  Block Deletions, probably, because the 'BlockEndTextPtr', of the deleted Chunk
  is at a virtual Pos in the next Tab (???) before the Redraw of the Caret (???).
  
  In case this Comment-Out, would produce some un-wished side-effect, restore it,
  and try to redefine the Caret Pos (line and Row), at the 'BlockStartTextPtr',
  immidiately after the Block Deletion, above, in 'ControlX'. It is already "done",
  but seems to be ineffective... :( :(( :(((
;;
L9: ;Call RestoreRealSource | Call SetPartialEditionFromPos
ret


OpenClipBoard:
    Mov D$ClipBoardLen 0
    Call 'USER32.IsClipboardFormatAvailable' &CF_TEXT | cmp eax 0 | je L9>>
    Call 'USER32.OpenClipboard' D$H.MainWindow | cmp eax 0 | je L9>>
    Call 'USER32.GetClipboardData' &CF_TEXT  | cmp eax 0 | je L8>>    ; > eax = handle
    Mov D$hBlock eax
    Call 'KERNEL32.GlobalLock' eax                          ; > eax = pointer

    Mov D$ClipBoardPtr eax
    Mov edi eax, al 0, ecx 0-1 | repne scasb
    Mov ecx edi | sub ecx D$ClipBoardPtr | dec ecx          ; len
    Mov D$ClipBoardLen ecx
    Call ClipBordCleaner
L9: ret
L8: Call 'USER32.CloseClipboard' | ret


ClipBordCleaner:
    Mov esi D$ClipBoardPtr
BlockCleaner:
L0: lodsb | If al = CR
                On B$esi <> LF, Mov B$esi-1 SPC
                inc esi | dec ecx | jecxz L9>
            Else_If al = LF
                Mov B$esi-1 SPC
            Else_If al < SPC
                Mov B$esi-1 SPC
            Else_If al = 255      ;> 127
                Mov B$esi-1 SPC
            End_If
    loop L0<
L9: ret


KillTabs:
    Mov esi D$CodeSource, ecx D$SourceLen
KillTab:
L0: lodsb | cmp al TAB | jne L1>
        Mov B$esi-1 SPC
L1: loop L0<
ret


CloseClipBoard:
    Call 'KERNEL32.GlobalUnlock' D$hBlock
    Call 'USER32.CloseClipboard'
ret


[TooLongClipBoard: B$ "
  ClipBord size too big. The limit is 1,000,000 octets.
  Do this in several operations. After each paste, Compile,
  close RosAsm and re-run." EOS

NotEnoughMemLeft: B$ "
  Not enough Memory left for pasting this CliBoard.
  Compile, if you want to save your work, Close
  RosAsm and re-run" EOS

CloseToMemoryEnd: B$ "
  After pasting this ClibBoard, Compile, to save your work,
  Close RosAsm and re-run, to enable more Memory" EOS]

ControlV:
    Call OpenClipBoard | On D$ClipBoardLen = 0, jmp L7>>

    If D$H.DebugDialog <> 0
        Call CloseDebuggerOrIgnore | On eax = &IDNO jmp L9>>
    End_If

    Call ReMapSourceMemoryIfNeeded D$ClipBoardLen | On eax = &IDNO, jmp L7>>

    If B$BlockAutoDelete = &TRUE
        On D$FL.BlockInside = &TRUE, Call ControlD
    End_If

    Call DoStoreBlockPaste

  ; Make room inside our text:
    Mov esi D$STRUCT.EditData@SourceEnd | add esi 400
    Mov edi esi | add edi D$ClipBoardLen
    Mov ecx esi | sub ecx D$STRUCT.EditData@CurrentWritingPos | inc ecx
    std | rep movsb | cld | inc esi

  ; Write from clipboard:
    Mov edi esi, esi D$ClipBoardPtr, ecx D$ClipBoardLen
    pushad | rep movsb | popad

  ; Search for the new Caret Position:
    Mov esi edi, ebx D$STRUCT.EditData@CaretLine
L0: lodsb | inc D$STRUCT.EditData@CaretRow | cmp al CR | jne L1>
        inc ebx | Mov D$STRUCT.EditData@CaretRow 1 | lodsb | dec ecx | jz L0>
L1: loop L0<

L0: cmp ebx D$STRUCT.EditData@LineNumber | jna L6>
        Mov esi D$STRUCT.EditData@UpperLine | Mov ecx ebx | sub ecx D$STRUCT.EditData@CaretLine
L1:     lodsb | cmp al LF | ja L1<
            Mov D$STRUCT.EditData@UpperLine esi | dec ebx | jmp L0<

L6: Mov D$STRUCT.EditData@CaretLine ebx

    Mov eax D$ClipBoardLen
    add D$SourceLen eax | add D$STRUCT.EditData@SourceEnd eax | add D$STRUCT.EditData@CurrentWritingPos eax
L7: Call 'KERNEL32.GlobalUnlock' D$hBlock
L8: Call 'USER32.CloseClipboard'
L9: ret
____________________________________________________________________________________________

[ExtendMemoryString: B$ "This operation requires more Memory than actually reserved:

Extend ?..." EOS]

[UserPeStartEqualCodeSource: D$ ?]

Proc ReMapSourceMemoryIfNeeded:
    Argument @Added
    [@TempoPointer: D$ ?]

        shl D@Added 1
      ; (to care about Tabs-partial-editions !!!)

        Mov eax D$CodeSource, ebx D$UserPeStart
        and eax 0_FFFF_FFF0 | and ebx 0_FFFF_FFF0

        If eax = ebx
            Mov B$UserPeStartEqualCodeSource &TRUE
        Else
            Mov B$UserPeStartEqualCodeSource &FALSE
        End_If

        add eax D$SourceLen | add eax D@Added | add eax 400

        ...If eax >= D$EndOfSourceMemory
            Call 'USER32.MessageBoxA' D$H.MainWindow, ExtendMemoryString, Argh, &MB_YESNO
            .If eax = &IDYES
                Call RestoreRealSource

              ; New User PE Memory size:
                Mov ecx D$EndOfSourceMemory | sub ecx D$UserPeStart
                add ecx D@Added | shl ecx 1 | add ecx 0100_000

                Push ecx
                  ; Allocate to 'TempoPointer':
                    Call VirtualAlloc @TempoPointer,
                                      ecx

                  ; Copy it all:
                    Mov esi D$UserPeStart, edi D@TempoPointer

                    If B$UserPeStartEqualCodeSource = &TRUE
                        Mov D$edi CRLF2, D$edi+4 CRLF2 | add edi 4 | add D@TempoPointer 4
                    End_If

                    Mov ecx D$EndOfSourceMemory | sub ecx D$UserPeStart
                    shr ecx 2 | rep movsd
                Pop ecx
                add ecx D@TempoPointer | Mov D$EndOfSourceMemory ecx

              ; New 'CodeSource', 'SourceEnd', 'CurrentWritingPos', 'UpperLine':
                Mov eax D@TempoPointer | sub eax D$UserPeStart
                add D$CodeSource eax
                add D$STRUCT.EditData@CurrentWritingPos eax
                add D$STRUCT.EditData@UpperLine eax
                add D$STRUCT.EditData@SourceEnd eax

                Push D$STRUCT.EditData@CurrentWritingPos, D$STRUCT.EditData@UpperLine, D$STRUCT.EditData@CaretLine, D$STRUCT.EditData@CaretRow

                  ; Release old Memory:
                    Exchange D$UserPeStart D@TempoPointer
                    Call VirtualFree D@TempoPointer

                  ; SourceLen may be = 0, when pasting after a [File]/[New]:
                    On D$Sourcelen > 0, Call StartEdition

                    Mov eax &IDOK

                Pop D$STRUCT.EditData@CaretRow, D$STRUCT.EditData@CaretLine, D$STRUCT.EditData@UpperLine, D$STRUCT.EditData@CurrentWritingPos

                Call SetPartialEditionFromPos

                Mov eax &IDYES
            .End_If

          ; eax = &IDNO if not &IDYES

        ...Else
            Mov eax &IDOK

        ...End_If
EndP
____________________________________________________________________________________________

; [F8] Separator Line

DrawOneLine:
    MRealCaretRow

    Mov esi D$STRUCT.EditData@SourceEnd | add esi 400 | Mov edi esi      ; 400 is security 13/10/...
    Mov ecx esi, ebx DRAWLINELEN | sub ecx D$STRUCT.EditData@CurrentWritingPos | inc ecx

    add edi ebx
        std
          rep movsb
          Mov al '_', ecx ebx | rep stosb
        cld

    add D$SourceLen ebx | add D$STRUCT.EditData@CurrentWritingPos ebx | add D$STRUCT.EditData@SourceEnd ebx

    Mov D$InsertedChar '_'

    Call DoStoreInsert | Call CarriageReturn
ret

____________________________________________________________________________________________

[WithControlA: D$ ?]

ControlA:
    If B$WithControlA = &TRUE

        Move D$LP.BlockStartText D$CodeSource,
             D$LP.BlockEndText D$STRUCT.EditData@SourceEnd

      ; SourceEnd is the Byte _after_ // BlockEndTextPtr is the last Byte:
        sub D$LP.BlockEndText (1*ASCII)

        Mov D$FL.BlockInside &TRUE

    End_If

ret
____________________________________________________________________________________________

AskForRedraw:

    Call 'USER32.RedrawWindow' D$H.BreakPointWindow,
                               0,
                               0,
                               &RDW_INVALIDATE+&RDW_INTERNALPAINT

    Call 'USER32.RedrawWindow' D$H.EditWindow,
                               0,
                               0,
                               &RDW_INVALIDATE+&RDW_INTERNALPAINT

ret

AskForRedrawNow:

    Call 'USER32.RedrawWindow' D$H.BreakPointWindow,
                               0,
                               0,
                               &RDW_INVALIDATE+&RDW_INTERNALPAINT+&RDW_UPDATENOW

    Call 'USER32.RedrawWindow' D$H.EditWindow,
                               0,
                               0,
                               &RDW_INVALIDATE+&RDW_INTERNALPAINT+&RDW_UPDATENOW

ret
____________________________________________________________________________________________
____________________________________________________________________________________________

;                               Source Edition Routines.
____________________________________________________________________________________________

; The Source Editor does not use any concept of Line / Row for defining the actual
; editing Position. For Status Bar, we need them, and for ScrollBar, the Line too.

[StatusLine: D$ ?
 StatusCol: D$ ?
 TotalNumberOfLines: D$ ?]

[VScroll:
 VScroll.cbSize: D$ len
 VScroll.fMask: D$ &SIF_ALL__&SIF_DISABLENOSCROLL ; = &SIF_PAGE+&SIF_POS+&SIF_RANGE // &SIF_DISABLENOSCROLL
 VScroll.nMin: D$ 1
 VScroll.nMax: D$ 0
 VScroll.nPage: D$ 0
 VScroll.nPos: D$ 0
 VScroll.nTrackPos: D$ 0]

[TestCodeSource: D$ ?]

TextPos:
    Mov edi D$CodeSource, ecx 0-1, al CR, D$StatusLine 0, D$StatusCol 0
    Align 32

    Mov ebx D$STRUCT.EditData@UpperLine
    If ebx < D$CodeSource
        Move D$STRUCT.EditData@UpperLine D$CodeSource, D$STRUCT.EditData@CurrentWritingPos D$CodeSource
        Mov D$STRUCT.EditData@CaretRow 1, D$STRUCT.EditData@CaretLine 0, D$STRUCT.EditData@PhysicalCaretRow 1

    Else_If ebx > D$STRUCT.EditData@SourceEnd
        Move D$STRUCT.EditData@UpperLine D$CodeSource, D$STRUCT.EditData@CurrentWritingPos D$CodeSource
        Mov D$STRUCT.EditData@CaretRow 1, D$STRUCT.EditData@CaretLine 0, D$STRUCT.EditData@PhysicalCaretRow 1

    End_If

L0: repne scasb | inc D$StatusLine | cmp edi D$STRUCT.EditData@UpperLine | jb L0<

    Move D$TotalNumberOfLines D$StatusLine
    Mov eax D$STRUCT.EditData@CaretLine | add D$StatusLine eax

    Mov al CR
L0: repne scasb | inc D$TotalNumberOfLines | cmp edi D$STRUCT.EditData@SourceEnd | jb L0<
    Mov eax D$TotalNumberOfLines | dec eax

    Move D$VScroll.nMax eax
    Move D$VScroll.nPage D$STRUCT.EditData@LineNumber
    Mov eax D$statusLine | sub eax D$STRUCT.EditData@CaretLine | inc eax
    Mov D$VScroll.nPos eax

RedrawScrollBar:

    On D$FL.ScrollBarWanted = &TRUE Call 'USER32.SetScrollInfo' D$H.ScrollBarWindow,
                                                                &SB_VERT,
                                                                VScroll,
                                                                &TRUE
ret


RePosFromScroll:    ; called with eax = Line Number Wanted by user Bar Scrolling.

    Push edi

        Mov edi D$CodeSource, al CR, D$StatusLine 0, D$StatusCol 0

        Comp edx 1 = S1>

        ; Align 32
        Mov ecx D$SourceLen

L0:     repne scasb

            sub edx 1

        Comp edx 1 > L0<

    add edi 1

S1: Mov D$STRUCT.EditData@UpperLine edi

    Pop edi

ret

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc KillTrailingSpaces:

;;

    The Editor may let no use Spaces (between the end of text and CR/LF. This happend, for
    example when you enter blanks Lines on Indentations

    I do not implement it in the Menu, and as this is fast enough i implement it when opening
    a file [OpenSourceOnly] / [OpenBUAsmPE]

;;

    Uses ebx,
         esi,
         edi

    Mov esi D$CodeSource,
        edi esi,
        edx D$STRUCT.EditData@SourceEnd
    ________________________________________

    ; TODO traduction

    ; Correction [CRCR] et [LFLF] en [CRLF]
    ________________________________________

L1: Comp W$esi CRCR = S1>

        Comp W$esi LFLF = S1>

        add esi (2*ASCII)

    Comp esi edx < L1<,
                 => S2>

S1:     Mov W$esi CRLF

        add esi (2*ASCII)

    Comp esi edx < L1<
    __________________________________

    ; TODO traduction

    ; Suppression des [SPC] superflus
    __________________________________

    ; Esi = D$CodeSource
S2: Mov esi edi

    jmp S3>

L1:@Start:

    stosb

S3:     Comp esi edx > S0> ; @End

            lodsb

            Comp al 0_22 = S4> ; " @Clean022

            Comp al 0_27 = S5> ; ' @Clean027

            Comp al 0_3B = S6> ; ; @Clean03B

            Comp al CR = S7>   ;   @Clean020

    jmp L1< ; @Start

S4:@Clean022:

L0:             stosb | lodsb | Comp esi edx = S0> ; @End

                Comp al '"'  <> L0< ; Allow blank Lines in Data Text

    jmp L1< ; @Start

S5:@Clean027:

L0:             stosb | lodsb | Comp esi edx = S0> ; @End

                Comp al "'" <> L0<

    jmp L1< ; @Start

S6:@Clean03B:

            Comp D$esi-2 MLC <> L2> ; LF;;CR

L0:             stosb | lodsb | Comp esi edx = S0> ; @End

                Comp D$esi-(2*ASCII) MLC <> L0<

    jmp L1< ; @Start

L2:             stosb | lodsb | Comp al CR <> L2<

S7:@Clean020:

L0:         Comp B$edi-1 SPC <> L1< ; @Start

                sub edi (1*ASCII) | sub D$SourceLen 1 | sub D$STRUCT.EditData@SourceEnd 1

            jmp L0< ; @Start

S0:@End:   Mov eax D$STRUCT.EditData@SourceEnd

    cmp W$eax-(1*WORD) CRLF | je P9> ; TODO ExitP

    cmp W$eax-(1*WORD) LFCR | je P9> ; TODO ExitP

        Mov W$eax CRLF | add D$STRUCT.EditData@SourceEnd (2*ASCII) | add D$SourceLen (2*ASCII)


EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

WheelMsg:

    Call KillCompletionList

    test W$Wparam+(1*WORD) 0_FFFF NEGATIVE S1>

        Call UpOneLine

ret

S1: Call DownOneLine

ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; Positioning Edition at 'MainWindowProc' or at 'Main' or at Top:

StartEdition:
    Call GetEditWindowClientRectangle

    ;Call ScanSourceForBadChars

    Mov edx CallBackName, ebx D$CallBackNameLen

    Call InternSearch

    On D$FL.BlockInside = &TRUE, jmp L9>
        Mov edx EntryPointLabel, ebx 0, eax edx
        While B$eax <> 0 | inc ebx | inc eax | End_While

        Call InternSearch

L9: Mov B$OnReplaceAll &FALSE, D$FL.BlockInside &FALSE, B$DownSearch &TRUE, D$FL.ReadyToRun &FALSE
    Call StorePosInBackTable

    Mov D$TABLE.Titles &NULL, D$PreviousPartialSourceLen 0

    Call ReplaceParagraphByDollar
ret


ScanSourceForBadChars:
    Mov esi D$CodeSource, edx D$STRUCT.EditData@SourceEnd

    sub edx 100

    Call MessageBox edx

ret

    While esi < edx
        lodsb

        If al < LF
           ; hexprint 1
        Else_If al > 127
            Mov edi STR.A.Trash, ecx 100
            dec esi
            rep movsb

            Call MessageBox STR.A.Trash

        End_If
    End_While
ret


GetEditWindowClientRectangle:
    Call 'USER32.GetClientRect' D$H.EditWindow RECT
      Mov eax D$RECTright | sub eax D$RECTleft
      shr eax 3 | dec eax | Mov D$STRUCT.EditData@ColNumber eax
        Mov eax D$RECTbottom | sub eax D$RECTtop
          or eax 00111 | xor eax 00111
            shr eax 4 | dec eax | Mov D$STRUCT.EditData@LineNumber eax
ret


ReplaceParagraphByDollar:
    Mov eax D$CodeSource, ecx D$STRUCT.EditData@SourceEnd

    While eax < ecx
      ; 167 = Code of Paragraph Char (yet available as keyboard input, but no more
      ; available as screen output).
        If D$eax = MLC
            add eax 4
            While D$eax <> MLC
                On eax = ecx, jmp L9>>
                inc eax
            End_While
            add eax 2

        Else_If B$eax = ';'
            While B$eax > CR
                inc eax | On eax = ecx, jmp L9>>
            End_While

        Else_If B$eax = '"'
            inc eax
            While B$eax <> '"'
                inc eax | On eax = ecx, jmp L9>>
            End_While

        Else_If B$eax = "'"
            inc eax
            While B$eax <> "'"
                inc eax | On eax = ecx, jmp L9>>
            End_While

        Else_If B$eax = 167
            Mov B$eax '$'

        End_If

        inc eax
    End_While

L9: ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[H.Help: D$ ?]
[RosAsmHelpClassName: B$ 'RosAsmHelpEditClass' EOS]

; We close B_U_Asm if run from BUAsm:

CloseHelp:

    Call 'KERNEL32.CreateMutexA' &NULL,
                                 &TRUE,
                                 {B$'BaseMutexName' EOS}

    Call 'KERNEL32.GetLastError'

    .If eax = &ERROR_ALREADY_EXISTS

        Call 'USER32.FindWindowA' RosAsmHelpClassName,
                                  &NULL

         Mov D$H.Help eax

        Call 'USER32.GetWindowLongA' eax,
                                     &GWL_EXSTYLE

        If eax <> &WS_EX_WINDOWEDGE  ; WS_EX value if auto-run

            ; If run from RosAsm
            Call 'USER32.SendMessageA' D$H.Help,
                                       &WM_CLOSE,
                                       &NULL,
                                       &NULL

        End_If

    .End_If

ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; Simplified version of 'RightClick' Search (used by 'StartEdition' to point out 'Main' or
; MainWindowProc:

InternSearch:
    Mov B$InsideMLC &FALSE, B$InsideComment &FALSE
    Mov ah B$edx | or ah 32                           ; ah = first char
    inc edx                                           ; edx > second char (> edi)
    sub ebx 1

    ; Now, edi (edx) > start+1 of right clicked word; ebx = len-1. Search fitting:
L0: Mov esi D$CodeSource, ecx D$SourceLen, B$InsideBracket &FALSE, B$InsideComment &FALSE

    jmp L0>

T0: lodsb | jmp L1>                          ; simplified loop for strip texts and comments
T1: loop T0<
      ret

L0: lodsb | cmp al ah | je L3>>              ; test XORed AH each pass in order to handle
            xor ah 32 | cmp al ah | je L3>>  ; case without modifying AL (following tests)
      jmp L1>
L2: loop L0<
        ret

L1: cmp B$InsideMLC &TRUE | jne L1>
      cmp D$esi-2 MLC | jne T1<          ; (LF ; ; CR)
        Mov B$InsideMLC &FALSE | jmp L2<
L1: cmp B$InsideComment &TRUE | jne L1>
      cmp al LF  | jne T1<
        Mov B$InsideComment &FALSE | jmp L2<
L1: cmp B$InsideText &FALSE | je L1>
      cmp al B$InsideText | jne T1<
        Mov B$InsideText &FALSE | jmp L2<
L1: cmp al "'" | jne L1>
      Mov B$InsideText al | jmp T1<
L1: cmp al '"' | jne L1>
      Mov B$InsideText al | jmp T1<
L1: cmp al '[' | jne L1>
      Mov B$InsideBracket &TRUE, B$DataDeclaration &FALSE    ;;;, B$OddWord 1
S0:   cmp B$esi SPC | jne L2<
        inc esi | sub ecx 1 | jnc S0<        ; strip double spaces
          ret
L1: cmp al ']' | jne L1>
      Mov B$InsideBracket &FALSE, B$DataDeclaration &FALSE | jmp L2<<
L1: cmp al ';' | jne L1>                     ; jmp over comments
        If D$esi-2 = MLC   ; (LF ; ; CR)
            Mov B$InsideMLC &TRUE | jmp T1<<
        Else
            Mov B$InsideComment &TRUE | jmp T1<<
        End_If
L1: cmp al '|' | jne L1>
      Mov B$InsideBracket &FALSE | jmp L2<<
L1: cmp al ':' | jne L2<<
      Mov B$DataDeclaration &TRUE

            jmp L2<<                     ; (avoids pointing equates datas).

L3: Mov al B$esi-2 | Call WordEdge | cmp B$Edge &FALSE | je L2<<     ; left edge?

        Mov D$NumberDashLines 0

        pushad | Mov ecx ebx, edi edx
C0:       lodsb | Mov ah B$edi | inc edi | or ax 02020     ; case insensitive comparison

            While B$esi-1 = '_'
                lodsb | or al SPC | inc D$NumberDashLines
            End_While

            While B$edi-1 = '_'
                Mov ah B$edi | or ah SPC | inc edi | dec ecx | jz C1>
            End_While

          cmp ah al | jne C1>
            loop C0<

            Mov al B$esi | Call WordEdge
            If B$Edge = &FALSE
                popad | jmp L2<<
            End_If
            popad | jmp C2>

C1:     popad | jne L2<<

   ; Mov al B$esi+ebx | Call WordEdge | cmp B$Edge &FALSE | je L2<<    ; right edge?

; as we have tested for '|' (> InsideBracket = FALSE), "test B$OddWord 1" applies either
; uppon first word of macro def. or odd word of equate def. But data body could still
; be pointed as odd equate dec. So we finally test 'B$DataDeclaration'.

C2: Push ebx
        add ebx D$NumberDashLines
        cmp B$esi+ebx ':'
    Pop ebx
    je L4>                                 ; Label?
      cmp B$InsideBracket &TRUE | jne L2<<                     ; equ. / macro
          cmp B$DataDeclaration &TRUE | je L2<<  ; avoid pointing data body instead of Equate

L4: dec esi                                                    ; found
    Mov D$LP.BlockStartText esi, D$RCstart esi                 ; RCstart/End used by

    add esi ebx

    Mov D$LP.BlockEndText esi,
        D$RCend esi       ; 'BackClick'

    Mov D$FL.BlockInside &TRUE
    inc esi | Mov D$STRUCT.EditData@CurrentWritingPos esi

    std | Mov ecx 0
L5:     lodsb | inc ecx | cmp al LF | jne L5<                  ; search for start of line
    cld | dec ecx

    add esi 2 | Mov D$STRUCT.EditData@UpperLine esi                            ; and set all needed
    Call UpOneLine | Call UpOneLine | Call UpOneLine           ; variables for Pos, in
    Mov D$STRUCT.EditData@CaretLine 3, D$STRUCT.EditData@CaretRow ecx, D$STRUCT.EditData@PhysicalCaretRow ecx  ; case user wish editing
    Call TryToMove

L9: ret
____________________________________________________________________________________________

; [F11] / [F12] instant BookMark feature

[F11Upperline: D$ ?
 F11CurrentWritingPos: D$ ?
 F11CaretLine: D$ ?
 F11CaretRow: D$ ?]

SavePosOnF11:
    Call ClearF12
    Call RestoreRealSource
    Move D$F11Upperline D$STRUCT.EditData@Upperline, D$F11CurrentWritingPos D$STRUCT.EditData@CurrentWritingPos,
         D$F11CaretLine D$STRUCT.EditData@CaretLine, D$F11CaretRow D$STRUCT.EditData@CaretRow
    Call SetPartialEditionFromPos
ret


[F12Upperline: D$ ?
 F12CurrentWritingPos: D$ ?
 F12CaretLine: D$ ?
 F12CaretRow: D$ ?]

SetPosOnF12:
    ..If D$F11Upperline <> 0
        Call RestoreRealSource

        Mov eax D$STRUCT.EditData@Upperline

      ; If we are not already at the saved F11 Pos, we go to it:
        .If eax <> D$F11Upperline
          ; Save first the actual F12 Pos for On-Off effect:
            Move D$F12Upperline D$STRUCT.EditData@Upperline, D$F12CurrentWritingPos D$STRUCT.EditData@CurrentWritingPos,
                 D$F12CaretLine D$STRUCT.EditData@CaretLine, D$F12CaretRow D$STRUCT.EditData@CaretRow
          ; Go:
            Move D$STRUCT.EditData@Upperline D$F11Upperline, D$STRUCT.EditData@CurrentWritingPos D$F11CurrentWritingPos,
                 D$STRUCT.EditData@CaretLine D$F11CaretLine, D$STRUCT.EditData@CaretRow D$F11CaretRow

        .Else
          ; Pos is the one save by F11: we switch back to the F12 previous Pos:
            If D$F12Upperline <> 0
                Move D$STRUCT.EditData@Upperline D$F12Upperline, D$STRUCT.EditData@CurrentWritingPos D$F12CurrentWritingPos,
                     D$STRUCT.EditData@CaretLine D$F12CaretLine, D$STRUCT.EditData@CaretRow D$F12CaretRow
            End_If

        .End_If

        Call SetPartialEditionFromPos

        Mov esi D$STRUCT.EditData@Upperline
        While B$esi-1 <> LF | dec esi | End_While
        Mov D$STRUCT.EditData@Upperline esi

    ..End_If
ret


ClearF11F12:
    Mov edi F11Upperline, eax 0, ecx 4 | rep stosd
ClearF12:
    Mov edi F12Upperline, eax 0, ecx 4 | rep stosd
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[EditorLOGFONT:
 @lfHeight: D$ 0_FFFF_FFF3
 @lfWidth: D$ 0
 @lfEscapement: D$ 0
 @lfOrientation: D$ 0
 @lfWeight: D$ 02BC
 @lfItalic: B$ 0
 @lfUnderline: B$ 0
 @lfStrikeOut: B$ 0
 @lfCharSet: B$ 0
 @lfOutPrecision: B$ 03
 @lfClipPrecision: B$ 02
 @lfQuality: B$ 01
 @lfPitchAndFamily: B$ 031
 @lfFaceName: B$ 'Courier New' EOS
 @Trailing: B$ 0 # &LF_FACESIZE
 EditorLOGFONT.len: Len]

[EditorCHOOSEFONT:
 @lStructSize: D$ len
 @hwndOwner: D$ 0
 @hDC: D$ &NULL
 @lpLogFont: D$ EditorLOGFONT
 @iPointSize: D$ 0
 @Flags: D$
 &CF_FIXEDPITCHONLY__&CF_SCREENFONTS__&CF_INITTOLOGFONTSTRUCT__&CF_APPLY__&CF_ENABLEHOOK__&CF_NOSIMULATIONS
 @rgbColors: D$ 0
 @lCustData: D$ 0
 @lpfnHook: D$ ChooseFontHook
 @lpTemplateName: D$ 0
 @hInstance: D$ 0
 @lpszStyle: D$ 0
 @nFontType: W$ &SCREEN_FONTTYPE
 @Alignment: W$ 0
 @nSizeMin: D$ 0
 @nSizeMax: D$ 0]

[HDC.EditFont: D$ ?]

[TEXTMETRICA:
 tmHeight: D$ ?
 tmAscent: D$ ?
 tmDescent: D$ ?
 tmInternalLeading: D$ ?
 tmExternalLeading: D$ ?
 tmAveCharWidth: D$ ?
 tmMaxCharWidth: D$ ?
 tmWeight: D$ ?
 tmOverhang: D$ ?
 tmDigitizedAspectX: D$ ?
 tmDigitizedAspectY: D$ ?
 tmFirstChar: B$ ?
 tmLastChar: B$ ?
 tmDefaultChar: B$ ?
 tmBreakChar: B$ ?
 tmItalic: B$ ?
 tmUnderlined: B$ ?
 tmStruckOut: B$ ?
 tmPitchAndFamily: B$ ?
 tmCharSet: B$ ?]

[FontHeight: D$ 16
 FontWidth: D$ 8
 LineSpacingAdjust: D$ 0]

; React to the [Apply] Button (&CF_APPLY__&CF_ENABLEHOOK, in EditorCHOOSEFONT@Flags):

Proc ChooseFontHook:
    Arguments @hwnd, @msg, @wParam, @lParam

            .If D@msg = &WM_COMMAND
                If D@wParam = 0402
                    Call LoadFont | Call AskForRedrawNow
                    Mov eax &TRUE | ExitP
                End_If
            .End_If

    Mov eax &FALSE
EndP


SelectFont:
    Move D$EditorCHOOSEFONT@hwndOwner D$H.ConfigDialog
    Call 'Comdlg32.ChooseFontA' EditorCHOOSEFONT

    If eax = &TRUE
        Call LoadFont | Call AskForRedrawNow | Call MainResize
    End_If
ret


LoadFont:
    On D$H.Font1 <> 0, Call 'GDI32.DeleteObject' D$H.Font1
    Call 'GDI32.CreateFontIndirectA' EditorLOGFONT | Mov D$H.Font1 eax
    Call 'USER32.GetDC' D$H.EditWindow | Mov D$HDC.EditFont eax
    Call 'GDI32.SetMapMode' eax &MM_TEXT
    Call 'GDI32.SelectObject' D$HDC.EditFont, D$H.Font1
    Call 'GDI32.GetTextMetricsA' D$HDC.EditFont, TEXTMETRICA

    Mov eax D$tmAveCharWidth | Mov D$FontWidth eax
    Mov eax D$tmHeight | Mov D$FontHeight eax
ret


LoadNationalFont:
    Call 'GDI32.CreateFontIndirectA' NATION_LOGFONT | Mov D$H.NationalFont eax
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
