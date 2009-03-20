TITLE Tab             ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;;
                  Main Editor's Upper Tab Control for TITLEd Chapters.

    When working with enTITLEd Sources, The Editor Copy/Paste the Actually edited Part
    at the very end of the Source Memory. Each time a new Part is activated, the previous
    one is copied back to its own room, and the new selected part is copied down in turn.

    At each event, a 'TABLE.Titles' is build. This Table holds nothing but the Addresses
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

SetPartialEditionFromPos:

    test D$PreviousPartialSourceLen NA ZERO S1>

ret

SetPartialEdition:

S1: test D$TABLE.Titles NA ZERO S1>

        Call GetActualPartFromPos | Call SetActualPartFromPos

    ; To force remaping the Source Editor Colors
S1: Mov D$PreviousUpperLine NA

 ;   Call AskForRedraw

    Mov D$FL.RealSourceRestored &FALSE

ret


SetPartialEditionFromPosNoRedraw:

    test D$PreviousPartialSourceLen NA ZERO S1>

ret

S1:

; Called when opening a new File, or by 'TitleWindowProc' (on user Tab Selection).
;    Call BuildTitleTable

;    If D$TABLE.Titles <> &NULL
;        Call GetActualPartFromPos | Call SetActualPartFromPos
;    End_If

    Mov D$PreviousUpperLine NA

    Mov D$FL.RealSourceRestored &FALSE

ret


; 'Called by 'ControlZ'.

UndoTitleMove:

    Push D$STRUCT.EditData@CaretRow,
         D$STRUCT.EditData@CaretLine,
         D$STRUCT.EditData@CurrentWritingPos,
         D$STRUCT.EditData@UpperLine

    Push ebx

        Call RestoreRealSource | Call BuildTitlesTable

    Pop ebx

    Mov eax D$TABLE.Titles+ebx*DWORD,
        D$STRUCT.EditData@CurrentWritingPos eax

    Call GetActualPartFromPos | Call SetActualPartFromPos

    Mov ebx D$UndoPtr | sub bx 32 | Mov esi ebx

    test D$esi+(7*DWORD) NA ZERO S1>

        lodsd | Mov D$STRUCT.EditData@CaretRow eax

        lodsd | Mov D$STRUCT.EditData@CaretLine eax

        lodsd | add eax D$CodeSource | Mov D$STRUCT.EditData@CurrentWritingPos eax

        lodsd | add eax D$CodeSource | Mov D$STRUCT.EditData@UpperLine eax

        Pop eax,
            eax,
            eax,
            eax
ret

S1: Pop D$STRUCT.EditData@UpperLine,
        D$STRUCT.EditData@CurrentWritingPos,
        D$STRUCT.EditData@CaretLine,
        D$STRUCT.EditData@CaretRow

ret


[AddedSize: D$ ?]
[FL.RealSourceRestored: D$ ?]

RestoreRealSource:                      ; Called only by 'UndoTitleMove'

    On D$PreviousPartialSourceLen = 0 ret

   ; Call DeleteUndoFiles
   ; Call KillUndo
   ; Call InitUndo

    Mov eax D$STRUCT.EditData@SourceEnd

    If W$eax-2 <> CRLF

        add D$STRUCT.EditData@SourceEnd 2 | add D$SourceLen 2

    End_If

    Mov eax D$SourceLen | sub eax D$PreviousPartialSourceLen | Mov D$AddedSize eax

    ; Move down (as many 'AddedSize') from Top of Actual Source Part to
    ; End of Actual Edition, to make room:
    If eax = 0

        Mov edi D$LP.ActualTitle,
            esi D$CodeSource,
            ecx D$SourceLen

        rep movsb

    Else_If eax s> 0                ; Positive Number.

        Mov esi D$STRUCT.EditData@SourceEnd,         ; End of Partial Edition
            edi esi

        add edi D$AddedSize

        Mov ecx esi | sub ecx D$NextTitle | inc ecx

        ; ecx = length from Source-Next-Title to End-of-Partial-Edition
        std

            rep movsb

        cld

        Mov esi D$CodeSource,
            edi D$LP.ActualTitle

        add esi D$AddedSize         ; equal to the made up room

        Mov ecx D$SourceLen | rep movsb

    Else                           ; Negative Number.

        Mov esi D$CodeSource,
            edi D$LP.ActualTitle

        Mov ecx D$SourceLen | jecxz L0>

            rep movsb
L0:
        Mov esi D$NextTitle,
            ecx D$CodeSource

        sub ecx D$NextTitle | rep movsb

    End_If

    Mov eax D$STRUCT.EditData@CurrentWritingPos

    If eax = D$STRUCT.EditData@SourceEnd

L0:     dec eax | cmp B$eax SPC | jb L0<

    End_If

    sub eax D$CodeSource

    add eax D$LP.ActualTitle | Mov D$STRUCT.EditData@CurrentWritingPos eax

    Mov eax D$LP.ActualTitle | sub eax D$CodeSource    ; eax = adjustement to new Pos.

    add D$STRUCT.EditData@UpperLine eax

    If D$FL.BlockInside = &TRUE

        add D$BlockStartTextPtr eax

        add D$BlockEndTextPtr eax

    End_If

    Move D$CodeSource D$RealCodeSource

    Move D$STRUCT.EditData@SourceEnd D$RealSourceEnd

    Move D$SourceLen D$RealSourceLen

    Mov eax D$AddedSize | add D$STRUCT.EditData@SourceEnd eax | add D$SourceLen eax

    Mov D$PreviousPartialSourceLen 0

    Mov D$FL.RealSourceRestored &TRUE

ret


TITLESOnOff:

    test D$CodeSource NA ZERO S1>

ret

S1: xor D$FL.TabWindow &TRUE | jz S2>

        Call ShowTitles

S2: Call 'USER32.ShowWindow' D$H.TabWindow,
                             D$FL.TabWindow

ret


ShowTitles:

    Call RestoreRealSource | Call BuildTitlesTable | Call ShowWindowTitleTab
    ____________________________

    ; TODO traduction
    ; Afficher l'onglet courant
    ____________________________

    Call 'USER32.SendMessageA' D$H.TabWindow,
                               &TCM_SETCURSEL,
                               D$ID.ActualTitle,
                               &NULL

    Call SetPartialEditionFromPos

    ; TODO traduction
    ; Afin de pouvoir continuer à utiliser la molette de la souris pour le scroll dans l'éditeur de source
    Call 'USER32.SetFocus' D$H.MainWindow

ret



Proc BuildTitlesTable:

    Uses ebx,
         edi,
         esi

    [@ID.NumberOfTitles: D$ ?
     @ID.PreviousNumberOfTitles: D$ ?]

    Mov edi D$CodeSource,
        ecx D$SourceLen,
        ebx TABLE.Titles,
        eax 'TITL',
        D$TABLE.Titles EOS,
        B$edi-(1*ASCII) LF

    Move D@ID.PreviousNumberOfTitles D@ID.NumberOfTitles | Mov D@ID.NumberOfTitles 0

    ; TODO traduction Recherche dans edi par DWORD
    shr ecx 2

L1: repne scasd | jne S7>

    Comp W$edi 'E ' <> L1<

    Comp B$edi-5 LF <> L1<

        sub edi (4*ASCII)

        ; If the first TITLE is not Top of file, set one now
        ; Si le premier TITLE n'est pas le début du fichier, le créer
        Comp ebx TABLE.Titles = S2>
            _____________________________________________________

            ; If the TITLE too smaller for the screen, skip over
            ; Si le TITLE est trop court pour l'écran, l'ignorer
            _____________________________________________________

            ; Previous TITLE Pos/ Position du précédent TITLE
            Mov esi D$ebx-DWORD

            xor edx edx

            L2: Comp B$esi CR <> S1>

                    add edx 1

                S1: add esi ASCII

            Comp esi edi < L2<

            Comp edx D$STRUCT.EditData@LineNumber < S6>

       ; Else


S2:     add D@ID.NumberOfTitles 1

        Comp D@ID.NumberOfTitles MAXTITLE = S9>

        Mov D$ebx edi | add ebx DWORD

        add edi (4*ASCII)

S6: test ecx ecx NOT_ZERO L1<

    ; In case when only one valid TITLE is found, we run no TITLE
S7: test D$TABLE.Titles+DWORD NA NOT_ZERO S8>

        Mov D$TABLE.Titles &NULL,
            D@ID.NumberOfTitles 0

    ; TABLE.Titles end mark
S8: Mov D$ebx &NULL

    Mov eax D@ID.NumberOfTitles

    Comp eax D@ID.PreviousNumberOfTitles = S0>

        Call KillUndo | Call InitUndo

S0:
EndP

S9:     Call 'USER32.MessageBoxA' &NULL,
                                  {B$ 'Too many TITLEs' EOS},
                                  {B$ 'BuildTitleTable' EOS},
                                  &MB_OK

    jmp S8<
____________________________________________________________________________________________
;;

 
    Partial Edition is done inside the 1_000_000 Octets reserved space that
    BUAsm always sets at the end of User Source. In case User would be
    dividing for the first time a very uge source, and, for example, write
    only one TITLE statement at the Top of Source, and then try to see what
    happends by moving the mouse outside the Client Area, the Partial Edition
    would be unable to copy Megas inside the reserved space. In such cases,
    we abort, and wait until user cut the Source in smaller parts, with
    more TITLES

;;

[TooLongTitle: B$ 'Part too long:' EOS]

[TITLE_MAX 1_000_000]

; Sets the Actual Part Variables, depending on the Writing Position.

[LP.ActualTitle: D$ ?
 NextTitle: D$ ?
 PreviousPartialSourceLen: D$ ?
 ID.ActualTitle: D$ ?]

GetActualPartFromPos:

    Mov esi TABLE.Titles,
        ebx D$STRUCT.EditData@CurrentWritingPos,
        eax D$TABLE.Titles,
        edx 0

    Mov D$LP.ActualTitle eax | Move D$NextTitle D$esi+DWORD

    ; 0-2 because Tab Index are zero based (> -1) and because we are
    ; INCreasing until *next* Title found (> -1)
    Mov D$ID.ActualTitle 0-2

    While eax <= ebx

        inc D$ID.ActualTitle

        Mov edx eax | lodsd | On eax = 0, jmp L1>

    End_While

L1: On edx = 0 Mov edx D$TABLE.Titles eax D$TABLE.Titles+DWORD

    On eax = 0 Mov eax D$STRUCT.EditData@SourceEnd

    Mov D$LP.ActualTitle edx,
        D$NextTitle eax

ret
____________________________________________________________________________________________

;;

    The Code Source is always followed by 200 CR/LF (security) + 1_000_000 octets reserved
    for Editing. We set the Partial Edition after the 200 CR/LF
;;

[RealCodeSource: D$ ?
 RealSourceEnd: D$ ?
 RealSourceLen: D$ ?]

SetActualPartFromPos:

    On D$TABLE.Titles = &NULL ret

    Move D$RealCodeSource D$CodeSource
    Move D$RealSourceEnd D$STRUCT.EditData@SourceEnd
    Move D$RealSourceLen D$SourceLen

    Mov edi D$STRUCT.EditData@SourceEnd | add edi 400
    Mov D$CodeSource edi
    Mov esi D$LP.ActualTitle, ecx D$NextTitle | sub ecx esi

    Mov D$PreviousPartialSourceLen ecx, D$SourceLen ecx
    Mov D$STRUCT.EditData@SourceEnd edi | add D$STRUCT.EditData@SourceEnd ecx | rep movsb
    Mov eax 0A0D0A0D, ecx 100 | rep stosd

    Mov eax D$CodeSource | sub eax D$LP.ActualTitle  ; eax = Displacement.

    add D$STRUCT.EditData@CurrentWritingPos eax | add D$STRUCT.EditData@UpperLine eax

    If D$FL.BlockInside = &TRUE
        add D$BlockStarttextPtr eax | add D$BlockEndTextPtr eax
        Move D$STRUCT.EditData@CurrentWritingPos D$BlockEndTextPtr
        Call SetCaret D$STRUCT.EditData@CurrentWritingPos
    End_If

  ; If user add or suppress a TITLE statement in the Partial Edition;
  ; all these Pointers are wrong. We reset at Top of Part:
    Mov eax D$STRUCT.EditData@CurrentWritingPos
    If eax >= D$STRUCT.EditData@SourceEnd
L1:     Move D$STRUCT.EditData@CurrentWritingPos D$CodeSource
        Move D$STRUCT.EditData@UpperLine D$CodeSource
        Mov D$FL.BlockInside &FALSE

        Call 'USER32.ShowWindow' D$H.TabWindow,
                                 &TRUE

        ret
    Else_If eax < D$CodeSource
        jmp L1<
    End_If

    Mov eax D$STRUCT.EditData@UpperLine
    If eax >= D$STRUCT.EditData@SourceEnd
        jmp L1<
    Else_If eax < D$CodeSource
        jmp L1<
    End_If
ret
____________________________________________________________________________________________

[MAXTITLE 100]

[ID.Title: D$ ?
 TABLE.Titles: D$ ? # MAXTITLE]

[STR.A.DefaultTopTITLE: B$ 'Top' EOS]

[STRUC.RECT.Tab:
 D$ ?
 D$ ?
 D$ ?
 D$ ?]

; One TABLE.Titles Record is: DWORD = Top of Part Pointer
; (>>> "TITLE TitleName")

[STRUC.TITLE.tcItem:
 @imask: D$ ?
 @lpReserved1: D$ ?
 @lpReserved2: D$ ?
 @pszText: D$ ?
 @cchTextMax: D$ ?
 @iImage: D$ ?
 @lParam: D$ ?]

Proc ShowWindowTitleTab:

    Local @DU.TitleTabHight

    Uses ebx,
         esi,
         edi

    test D$TABLE.Titles NA NOT_ZERO S1>

EndP

S1: Mov D$STRUC.TITLE.tcItem@imask &TCIF_TEXT

    Mov esi TABLE.Titles, D$ID.Title 0

    ;  Add a [Top] Tab Item if user did not
    Mov eax D$esi

    If D$eax <> 'TITL'

        Mov D$STRUC.TITLE.tcItem@pszText STR.A.DefaultTopTITLE

        Call 'USER32.SendMessageA' D$H.TabWindow,
                                   &TCM_INSERTITEM,
                                   D$ID.Title,
                                   STRUC.TITLE.tcItem

        add D$ID.Title 1

        Move D$TABLE.Titles D$CodeSource | add esi DWORD

        On D$ID.Title = MAXTITLE sub esi DWORD

    End_If

L1: lodsd | test eax eax ZERO S0> ; ExitP

        jmp S1>

        ; Jump over 'TITLE'/ Saute 'TITLE'
L2:     add eax ASCII

S1: Comp B$eax SPC = L2<

        jmp S2>

L3:     add eax ASCII

S2:     Comp B$eax SPC > L3<

        Mov D$STRUC.TITLE.tcItem@pszText eax

        ; Edx = Eax+20 (20 len max Tab)
        lea edx D$eax+20

        jmp S3>

L4:     add eax ASCII

            Comp eax edx > S5>

S3:     Comp B$eax SPC = L4<

        ; Search for end of 'TitleName', to write a zero, in user source.

        jmp S4>

L5:     add eax ASCII

            Comp eax edx > S5>

S4:     Comp B$eax SPC > L5<

S5:     Mov B$eax 0

        Call 'USER32.SendMessageA' D$H.TabWindow,
                                   &TCM_INSERTITEM,
                                   D$ID.Title,
                                   STRUC.TITLE.tcItem

        add D$ID.Title 1

    jmp L1<

S0:

EndP

[STRUC.RECT.TabWindow:
 D$ ?
 D$ ?
 D$ ?
 D$ ?]

[H.TabWindow: D$ ?
 FL.TabWindow: D$ ?
 H.TabFont: D$ ?]

[STR.A.WindowType.SysTabControl32: B$ 'SysTabControl32' EOS]


[STRUC.LOGFONTA:
 @lfHeight: D$ 0-13
 @lfWidth: D$ &NULL
 @lfEscapement: D$ &NULL
 @lfOrientation: D$ &NULL
 @lfWeight: D$ &FW_DONTCARE
 @lfItalic: B$ &FALSE
 @lfUnderline: B$ &FALSE
 @lfStrikeOut: B$ &FALSE
 @lfCharSet: B$ &DEFAULT_CHARSET
 @lfOutPrecision: B$ &OUT_DEFAULT_PRECIS
 @lfClipPrecision: B$ &CLIP_DEFAULT_PRECIS
 @lfQuality: B$ &DEFAULT_QUALITY
 @lfPitchAndFamily: B$ &DEFAULT_QUALITY
 @lfFaceName: B$ 'Courier New' EOS]

CreateTitleTab:

; [BuildTitlesTable]

    Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  STR.A.WindowType.SysTabControl32,
                                  &NULL,
                                  &WS_CHILD+&WS_POPUPWINDOW+&TCS_HOTTRACK+&TCS_BUTTONS+&TCS_FLATBUTTONS,
                                  0,
                                  0,
                                  0,
                                  0,
                                  D$H.MainWindow,
                                  &NULL,
                                  D$H.Instance,
                                  &NULL

    Mov D$H.TabWindow eax

    Call 'GDI32.CreateFontIndirectA' STRUC.LOGFONTA

    Mov D$H.TabFont eax

    Call 'USER32.SendMessageA' D$H.TabWindow,
                               &WM_SETFONT,
                               eax,
                               &TRUE

TabResize:

    Call 'USER32.GetWindowRect' D$H.EditWindow,
                                STRUC.RECT.TabWindow

    Mov eax D$STRUC.RECT.TabWindow+LEFT | sub D$STRUC.RECT.TabWindow+RIGHT eax

    sub D$STRUC.RECT.TabWindow+LEFT 1

    Call 'USER32.GetSystemMetrics' &SM_CYFIXEDFRAME

    add eax D$DU.StatusBarHight | sub eax 1

    Mov edx D$STRUC.RECT.TabWindow+BOTTOM | sub edx eax | Mov D$STRUC.RECT.TabWindow+TOP edx

    add eax 1 | Mov D$STRUC.RECT.TabWindow+BOTTOM eax

    Call 'USER32.MoveWindow' D$H.TabWindow,
                             D$STRUC.RECT.TabWindow+LEFT,
                             D$STRUC.RECT.TabWindow+TOP,
                             D$STRUC.RECT.TabWindow+RIGHT,
                             D$STRUC.RECT.TabWindow+BOTTOM,
                             &FALSE

ret

WM_BUTTON_Tab:

    Call 'USER32.SendMessageA' D$H.TabWindow,
                               &TCM_GETCURSEL,
                               &NULL,
                               &NULL

    Mov D$ID.ActualTitle eax

    Mov D$FL.BlockInside &FALSE

    Push eax

       Call RestoreRealSource

    Pop eax

    Mov edx D$TABLE.Titles+eax*DWORD, D$STRUCT.EditData@CurrentWritingPos edx

    Call SetPartialEdition

    Call ShowTitles

ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
