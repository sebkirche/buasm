TITLE Dialog          ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

DialogMenuComment:
;;
 Main at: 'InitDialogEdition' / 'EditDialogBoxProc' / 'EditedDialogBoxProc'
          'CloseDialogEdition'
          'HelpDialog' / 'HelpDialogProc'

'InstallHook' 'MouseProc' 'ShowDialogResult'


                                  Dialog Editor

 Menus in Dialogs are stored either as "0" (no menu) or as, for exemple, "FFFF 0D8F"
 (menu with ID 0D8F). When Win runs a Dialog, it read the ID according menu in resources.
 As the "runing resources" in RosAsm Dialog Editor is nothing but RosAsm resources itselves
 (and not the futur resources of the futur PE we are writing), we can't set the wished
 value in the edited dialog template. Instead, we have to "LoadMenuIndirectA" / "SetMenu"
 inside the "EditedDialogBoxProc". (So do we in 'WhatMenu' > 'WhatMenuProc' > 'SetTestMenu'
 routine). So, we have a difficult job to do with 2 menus IDs instead of one (and hide any
 in "EditedDialogBoxData"):

 - When an existing Dialog is loaded, we save the true menu ID in D$DialogMenuTrueID.
   Then we ask Win for a handle for the menu template (for "SetMenu")
   The Dialog box template, then, has menu set at "0"...
 - In the Dialog-Main-Edit-Control (Editor), we show to the user the 'true' ID
 - As Menu are usually choosen by "WhatMenu" routine, we store the 'runing ID' in
   'D$ActualMenutestID'.

 So:  >  ActualMenutestID = Menu ID used by RosAsm to activate a menu.
      >  DialogMenuTrueID = Menu ID really saved in Editied PE Resources.

 A similar problem arises for Dialog Class. As soon as a class is written by user inside
 the template, the Dialog would desappear from screen (as not registered by RosAsm...).
 So, have we to hide this record from the editor internal Template and restore it before
 saving either in ClipBoard or in Resources. As the record is a string -and not a number-
 i choose to simply save it in a string table ('ClassRecord'), to restored it at saving
 time.
;;
 _______________________________________________________________________________________
 _______________________________________________________________________________________
 _______________________________________________________________________________________

[DialogMenuTrueID: D$ ?]

[TypeFace: B$ 'Arial' EOS]
[Helv: B$ 'Helv' EOS]

[H.MyFont: D$ ?]

CreateFontForDialogEdition:
    Call 'GDI32.CreateFontA' 10, 5, 0, 0, 400, 0, 0, 0, 1,   ;  DEFAULT_CHARSET 1  OEM_CHARSET 255
                             0, 0, 0, 0, TypeFace
    Mov D$H.MyFont eax
ret

_______________________________________________________________________________________
_______________________________________________________________________________________

[MaxTemplateText 0FFFF]

[D_button 080  D_Edit 081  D_Static 082  D_ListBox 083  D_ScrollBar 084  D_ComboBox 085]

[ID_Ilist 101  ID_IpopUpMenu 102]

; 080=button / 081=Edit / 082=Static / 083=ListBox / 084=ScrollBar / 085=ComboBox

; This is the text default template that user can see in in the main editor list:

[NewDialogTemplateText: D$ ?]
[ClassRecord: D$ ? # 20]

[DefaultDialogTemplateText:
 B$
 'D$ 90C408C2 ; Style' 0                 ; style
 'D$ 00000000 ; ExStyle' 0               ; extended style
 'U$ 0000 0000 0000 00DC 00C8 ; Dim' 0   ; control-number, x, y, width, hight
 '0 ;      no Menu' 0                    ; no menu >>> 0. If Menu > 0FFFF ID
 '"" 0 ; Class' 0                        ; class 0 > default
 '"New Dialog" 0 ; Title' 0              ; title
 '08 "Helv" 0 ; Font'  0 0 255           ; font

 DefaultDialogTemplateTextLen: len]

[CommentDialogStyle:  ' ; Style' EOS
 CommentDialogDim:    ' ; Dim' EOS
 CommentDialogMenu:   ' ; Menu' EOS
 CommentDialogClass:  ' ; Class' EOS
 CommentDialogTitle:  ' ; Title' EOS
 CommentDialogFont:   ' ; Font' EOS
 CommentControlStyle: ' ; Style' EOS
 CommentControlDim:   ' ; Dim' EOS
 CommentControlID:    ' ; ID' EOS
 CommentControlClass: ' ; Class' EOS
 CommentControlTitle: ' ; Title' EOS
 CommentControlCData: ' ; No creation data' EOS]


; Default Visible text in Editor when adding/inserting a new Control:

[NewDialogControlText: B$
 'D$ 50000000 ; Style' 0          ; style
 'D$ 00000000 ; ExStyle' 0        ; extended style
 'U$ 0000 0000 0038 0018 ; Dim' 0   ; x y w h
 '0000 ; ID' 0                      ; ID
 'FFFF 0080 ; Class' 0              ; Predefined class / 080=button
 '"New Control" 0 ; Title' 0        ; title
 '0 ; No creation data' 0 0 255     ; no creation data
 DefaultControlLenght: len]
; len = 07E 126 Octets // 'D$NewDialogTemplateText' = 010000 // 010000/07E = 520

; This is the template for edition (THE Editor):

[DialogBoxData:
 DialogStyle:
 D$ &WS_VISIBLE+&WS_THICKFRAME+&DS_SYSMODAL+&DS_SETFONT+&DS_3DLOOK+&DS_MODALFRAME+&WS_POPUP+&WS_CAPTION
 DialogExtStyle: 0

 ControlsNumber: U$ 1
 DialogX: 3
 DialogY: 3
 DialogW: 100
 DialogH: 340                       ; control-number, x, y, width, hight
    0                               ; no menu
    0                               ; class 0 > default
 DialogTitle: 'Dialogs Editor' EOS    ; title
 DialogFontSize: 8  DialogFont: 'Helv' EOS

; controls:
; main editor list:

D$ &WS_CHILD&WS_VISIBLE+&LBS_HASSTRINGS+&LBS_NOTIFY+&WS_VSCROLL+&WS_HSCROLL+&ES_AUTOVSCROLL+&ES_AUTOHSCROLL+&WS_BORDER;+&WS_THICKFRAME

    0                        ; style / ext.style
   U$ 2 2  96 75             ; x y w h
     ID_Ilist                ; ID
     0FFFF                   ; Predefined class
     D_ListBox               ; List control for main editor
    '  ' EOS 0                 ; button title
     0]


[DialogBoundingRectangle:
 DBRX1: D$ ?
 DBRY1: D$ ?
 DBRX2: D$ ?
 DBRY2: D$ ?]

[BaseUnits:
 BaseUnitX1: D$ ?
 BaseUnitY1: D$ ?
 BaseUnitX2: D$ ?
 BaseUnitY2: D$ ?]

[IDFstring: B$ "

    You must set the ID number of last created control.

    Unlike the Menu Editor, the Dialog Editor will not do this for you    
    and let you free of your equates choices.

    1) You are allowed to give the same ID number to several controls

    2) The Dialog Editor have no way to save your Equates Names
        nor to set them for you as the names MUST be unique.

    So >>> paper / pencil...

    " EOS]

;;
 Of no use now, but of some interrest:

 I wrote this at a time when the two dialogs (editior and edited) were redrawn after
 each modification. Now, only edited dialog is redrawn and all this is of no more use.
 But, as it have been some work..., i let it here (in case of need). This is a trick
 that gives the base unit (absolute need if we want to know where to set a dialog, but
 the direct function do NOT exist in Win api (!!!!!!!!)).
;;

;SaveDialogUserPosition:
;    ; MapDialog do this to BaseUnits values:
;    ;
;    ; left   = (left   * baseunitX) / 4
;    ; right  = (right  * baseunitX) / 4
;    ; top    = (top    * baseunitY) / 8
;    ; bottom = (bottom * baseunitY) / 8
;    ;
;    ; i only want to know baseUnits. So:
;
;    Mov D$BaseUnitX1 4, D$BaseUnitY1 8, D$BaseUnitX2 0, D$BaseUnitY2, 0     ; <<< the
;    Call 'USER32.MapDialogRect' D$Adressee BaseUnits                        ; <<< trick
;
;    Call 'USER32.GetWindowRect' D$Adressee DialogBoundingRectangle
;   ; Call 'USER32.GetDialogBaseUnits'   ; of no need
;
;   ; dialogUnitX = (pixelX * 4) / baseunitX
;   ; dialogUnitY = (pixelY * 8) / baseunitY
;
;    Mov eax D$DBRX1, edx 0
;    shl eax 2 | div D$BaseUnitX1
;    Mov W$DialogX ax
;
;    Mov eax D$DBRY1, edx 0
;    shl eax 3 | div D$BaseUnitY1
;    Mov W$DialogY ax
;ret


[PreviousControlID: 0FFFF]

; If user selected a blank separator line, we do not add > we insert:

AddOneControl:
    If W$PreviousControlID = 0
        Call 'USER32.MessageBoxA' D$H.MainWindow, IDFstring, CCFtitle, &MB_SYSTEMMODAL | ret
    Else
        Mov W$PreviousControlID 0            ; for next time test (filled -or not- by user)
    End_If

    Call 'USER32.SendMessageA' D$H.DialogList &LB_GETCURSEL eax 0
    Mov D$DialogListIndex eax

    On D$DialogListIndex > 0FFFF, Mov D$DialogListIndex 0      ; no sel. > OK  > add

    Mov eax D$DialogListIndex
    While eax >= Line_empty+1 | sub eax Line_empty+1 | End_While

    Push eax

    If eax = Line_empty
      ; Insert 'DefaultControlLenght' of 'NewDialogControlText':
        dec D$DefaultControlLenght                                  ; no 255 end mark
        Call SearchDialogLine | inc edi | Mov edx edi               ; > start of next control
        Mov al 255, ecx MaxTemplateText | repne scasb | dec edi     ; actual end
        Mov esi edi | add edi D$DefaultControlLenght                ; new end
        Mov ecx esi | sub ecx edx | inc ecx                         ; count moveable chars
        std | rep movsb | cld                                       ; make room
        Mov edi edx, esi NewDialogControlText                       ; ready for copy
        Mov ecx D$DefaultControlLenght                              ; how much
        rep movsb                                                   ; copy default control
        inc D$DefaultControlLenght                                  ; restore full lenght
    Else
      ; add:
        Mov edi D$NewDialogTemplateText, al 255, ecx MaxTemplateText
        repne scasb | dec edi
        Mov esi NewDialogControlText, ecx D$DefaultControlLenght
        rep movsb
    End_If

    Call FromTextToBinTemplate | Call ShowDialogResult | Call FillDialogListBox
    Pop eax

    If eax = Line_empty                                             ; if "insert":
        Call ScrollToInsertedControl
    Else                                                            ; if "add":
        Call ScrollDownToLastControl
    End_If

    Mov edi D$NewDialogTemplateText, al 0, ecx 200 | repne scasb
    Mov al '0' | repne scasb | add edi 2 | inc B$edi
    On B$edi > '9', add B$edi 7
    .If B$edi > 'F'
        Mov B$edi '0' | inc B$edi-1
        On B$edi-1 > '9', add B$edi-1 7
        If B$edi-1 > 'F'
            Mov B$edi-1 '0' | inc B$edi-2
            On B$edi-2 > '9', add B$edi-2 7       ; >>> up to FFF (4095 controls -enough?-)
        End_If
    .End_If
ret


ShowDialogResult:
  ; Under 98, impossible to destroy the old Window *after* having created the new one.
  ; (Works fine under 2000... too bad... ).

    Mov eax D$EditedDialogBoxData
    Push D$eax
        and D$eax 0FFF_FFFF | or D$eax &WS_VISIBLE__&WS_POPUP

        Call 'USER32.DestroyWindow' D$H.EditedDialog
        Call 'USER32.CreateDialogIndirectParamA' D$H.Instance, D$EditedDialogBoxData,
                                                 D$H.MainWindow, EditedDialogBoxProc, 0
        Mov D$H.EditedDialog eax

        Mov eax D$EditedDialogBoxData
    Pop D$eax
ret

________________________________________________________________________________________

; This is the default 'StartUp' Dialog Data you see when cliclng on [New Dialog]:
; WS_THICKFRAME turns it 'sizeable' (for editing only, striped from savings).

[EditedDialogBoxData: D$ ?]

[DefaultEditedDialogBoxData:
 D$ &WS_VISIBLE+&WS_THICKFRAME+&DS_SETFONT+&DS_SYSMODAL+&DS_MODALFRAME+&WS_POPUP+&WS_CAPTION+&DS_CENTER
    0                       ; style / extended style
 U$ 0 0 0 220 200           ; control-number, x, y, width, hight
    0                       ; no menu
    0                       ; class 0 > default
    "New Dialog" 0          ; title
    8 "Helv" 0              ; font

 DefaultEditedDialogBoxDataLenght: len]


;;
 Dialog proc for the result of edition. Clicking on a control select the dim record
 of the according Main list template part. Works only with controls that send
 some WM_COMMAND message > doesn't work on static controls. Another problem is that
 selection must not run at initialisation time > Nothing works at all until user didn't
 click on a Button. Exemple: if user first click on an Edit Control, nothing happends.
 see if we can do better later.
;;

[H.DialogEditor: D$ ?
 H.EditedDialog: D$ ?
 EditionInitFlag: D$ ?]

[ArrowCursor: D$ ?]

Proc EditedDialogBoxProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

  ; while opening and closing this dialog to show work progress to user, after
  ; some time, the menu seems destroyed (must be by Win -as bound to dialog-)
  ; > so, reinitialise. But then, the menu (visible) do not work any more
  ; in the editor. Saved data work fine....
    ...If D@msg = &WM_INITDIALOG
        Move D$H.EditedDialog D@hwnd
        Call MakeControlIDList
        Mov B$EditionInitFlag &TRUE

        Call GetDialoBaseUnits

        If D$ActualMenutestID <> 0
            Call 'USER32.DestroyMenu' D$ActualMenutestID
            Mov eax D$MenuListPtr | add eax 4
            Call 'USER32.LoadMenuIndirectA' D$eax
            Mov D$ActualMenutestID eax
            Call 'USER32.SetMenu' D@hwnd eax
        End_If

  ;  ...Else_If D@msg = &WM_CLOSE
  ;      Call 'USER32.DestroyWindow' D@hwnd

    ...Else
L8:     popad | Mov eax &FALSE | jmp L9>

    ...End_If

    popad | Mov eax &TRUE

L9: EndP


[Text12345: B$ '1234567890' EOS]

[DialogUnitX: D$ ?
 DialogUnitY: D$ ?]

UpdateControlDims:
    Mov eax D$Control.rcNormalPosition.left, edx 0
    shl eax 2 | div D$BaseUnitX1
    On edx >= 5, inc eax
    Mov edi Text12345 | Call DecimalToAscii
    Call 'USER32.SendMessageA' D$TABLE.H.DialogControls+4, &WM_SETTEXT, 0, Text12345
    Mov ecx D$TABLE.H.DialogControls+4 | Call WriteDimOnly

    Mov eax D$Control.rcNormalPosition.Top, edx 0
    shl eax 3 | div D$BaseUnitY1
    On edx >= 5, inc eax
    Mov edi Text12345 | Call DecimalToAscii
    Call 'USER32.SendMessageA' D$TABLE.H.DialogControls+12+4, &WM_SETTEXT, 0, Text12345
    Mov ecx D$TABLE.H.DialogControls+12+4 | Call WriteDimOnly

    Mov eax D$Control.rcNormalPosition.Right, edx 0
    sub eax D$Control.rcNormalPosition.left
    shl eax 2 | div D$BaseUnitX1
    On edx >= 5, inc eax
    Mov edi Text12345 | Call DecimalToAscii
    Call 'USER32.SendMessageA' D$TABLE.H.DialogControls+24+4, &WM_SETTEXT, 0, Text12345
    Mov ecx D$TABLE.H.DialogControls+24+4 | Call WriteDimOnly

    Mov eax D$Control.rcNormalPosition.Bottom, edx 0
    sub eax D$Control.rcNormalPosition.Top
    shl eax 3 | div D$BaseUnitY1
    On edx >= 5, inc eax
    Mov edi Text12345 | Call DecimalToAscii
    Call 'USER32.SendMessageA' D$TABLE.H.DialogControls+36+4, &WM_SETTEXT, 0, Text12345
    Mov ecx D$TABLE.H.DialogControls+36+4 | Call WriteDimOnly

    Call FromTextToBinTemplate | Call ShowDialogResult | Call FillDialogListBox
ret


;;
  For the Dialog itself, only Sizing modifications are allowed. We only set the Hight
  and width.
;;

UpdateDialogDims:
    Mov B$ModifiedControl &FALSE

    Call 'USER32.GetClientRect' D$H.EditedDialog, Control.rcNormalPosition.left

  ; Result of '.GetClientRect' >>> right > Weidth // Bottom > Hight:
    Mov eax D$Control.rcNormalPosition.Right, edx 0
    shl eax 2 | div D$BaseUnitX1
    On edx >= 5, inc eax
    Mov edi Text12345 | Call DecimalToAscii
    Call 'USER32.SendMessageA' D$TABLE.H.DialogControls+24+4, &WM_SETTEXT, 0, Text12345
    Mov ecx D$TABLE.H.DialogControls+24+4 | Call WriteDimOnly

    Mov eax D$Control.rcNormalPosition.Bottom, edx 0
    shl eax 3 | div D$BaseUnitY1
    On edx >= 5, inc eax
    Mov edi Text12345 | Call DecimalToAscii
    Call 'USER32.SendMessageA' D$TABLE.H.DialogControls+36+4, &WM_SETTEXT, 0, Text12345
    Mov ecx D$TABLE.H.DialogControls+36+4 | Call WriteDimOnly

    Call FromTextToBinTemplate | Call ShowDialogResult | Call FillDialogListBox
ret


;;
  This is the trick for retrieving the Values needed for translating Screen Coordinates
  into DialogBoxes coordinates. It take account of the Font.
  
  For X Dim, the formula is:   shl eax 2 | div D$BaseUnitX1
  For Y Dim, the formula is:   shl eax 3 | div D$BaseUnitY1
;;

GetDialoBaseUnits:
    Mov D$BaseUnitX1 4, D$BaseUnitY1 8, D$BaseUnitX2 0, D$BaseUnitY2, 0
    Call 'USER32.MapDialogRect' D$H.EditedDialog BaseUnits
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

HookComments:
;;
  'InstallHook' is called from the Init of 'ReInitDialogEdition'. Its purpose is to
  install a Mouse Hook Procedure, 'MouseProc', that enable the Direct Mouse Edition
  of Dialog Controls Coordinates.
  
  The Coordinates of the target Cotrol are directely modified by 'MouseProc' and, for
  avoiding blinckering they are made effective only when user release the Button.
  
  Left Button is for Pos. Right Button is for Dim.
  
  When moving a Control, the Cursor is confined to the Dialog Client Rectangle.
  (not when re-sizing).
  
  As this would not mean a thing, if user left-Click on the Dialog (instead of a Control)
  Right-Click is silently subtituted, in that case.
  
  'KickDims' is run when the user releases the Mouse Button. It first select the proper
  ListBox Item for Dims, according with the target Control or with the Dialog. Then it
  runs 'SetDialogTools', that shows the Size and Pos Edition Controls in the Editor,
  and finaly calls either for 'UpdateControlDims' or for 'UpdateDialogDims'.
  
  When drawing a Control, it is replaced by a Static Control with SS_BLACKFRAME Style.
  This is for viewing, for example, the real sizes of Radio Buttons.
;;

[Control:
 Control.iLength: D$ len
 Control.flags: D$ 0
 Control.showCmd: D$ 0
 Control.ptMinPosition.x: D$ 0
 Control.ptMinPosition.y: D$ 0
 Control.ptMaxPosition.x: D$ 0
 Control.ptMaxPosition.y: D$ 0
 Control.rcNormalPosition.left: D$ 0
 Control.rcNormalPosition.top: D$ 0
 Control.rcNormalPosition.right: D$ 0
 Control.rcNormalPosition.bottom: D$ 0]

[oControl:
 oControl.iLength: D$ len
 oControl.flags: D$ 0
 oControl.showCmd: D$ 0
 oControl.ptMinPosition.x: D$ 0
 oControl.ptMinPosition.y: D$ 0
 oControl.ptMaxPosition.x: D$ 0
 oControl.ptMaxPosition.y: D$ 0
 oControl.rcNormalPosition.left: D$ 0
 oControl.rcNormalPosition.top: D$ 0
 oControl.rcNormalPosition.right: D$ 0
 oControl.rcNormalPosition.bottom: D$ 0]

[H.Hook: D$ ?
 H.Hooked: D$ ?
 UserClickedOnControl: D$ ?
 UserRightClickedOnControl: D$ ?
 FromPointX: D$ ?
 FromPointY: D$ ? ? ?
 ClickFromPointX: D$ ?
 ClickFromPointY: D$ ?
 ModifiedControl: D$ ?]

[EditedDialogRectangle:
 D$ ?
 D$ ?
 D$ ?
 D$ ?]

; 'HookComments'.


Proc MouseProc:
    Arguments @nCode, @wParam, @lParam
;;
  Win Doc: 
  
  * If nCode is less than zero, the hook procedure must pass the message 
  to the CallNextHookEx function without further processing and should return the 
  value returned by CallNextHookEx. 
  
  * To enable the system to process the message, the return value must be zero. 
  To discard the message, the return value must be a nonzero value. 
  
  * If the 'CallNextHookEx' function succeeds, the return value is the value returned
  by the next hook procedure in the chain. The current hook procedure must also
  return this value.
  __________________
  
  In fact, under 98, if the MouseProc processes the Message, it *must* return a non
  zero value. Otherwise, the App hangs randomaly.
;;
    pushad

    ..If D@nCode > 07FFF_FFFF
        jmp L9>>

    ..Else_If D$H.EditedDialog = 0
      ; All of this should be no use. Just added security...
L0:     Mov al B$UserClickedOnControl | or al B$UserRightClickedOnControl
        If al <> &FALSE
            Mov B$UserClickedOnControl &FALSE, B$UserRightClickedOnControl &FALSE
            Call 'USER32.ClipCursor' &NULL
        End_If
        jmp L9>>

    ..Else
        Mov al B$UserClickedOnControl | or al B$UserRightClickedOnControl
        .If al = &FALSE
            Mov ebx D@lParam, eax D$ebx, ebx D$ebx+4
            Mov D$FromPointX eax, D$FromPointY ebx
            Mov D$ClickFromPointX eax, D$ClickFromPointY ebx
            Call 'USER32.WindowFromPoint' eax, ebx | On eax = &FALSE, jmp L0<<
            If eax <> D$H.EditedDialog
                Call 'USER32.IsChild' D$H.EditedDialog, eax | On eax = &FALSE, jmp L0<<
            End_If
        .End_If

    ..End_If
    _______________________________

    If D@wParam = &WM_LBUTTONDOWN
        Call SearchForWhatControl

    Else_If D@wParam = &WM_RBUTTONDOWN
        Call SearchForWhatControl

    End_If

    ...If D$H.Hooked <> 0
        ..If D@wParam = &WM_LBUTTONDOWN
            Mov ebx D@lParam, eax D$ebx, ebx D$ebx+4
            Mov D$FromPointX eax, D$FromPointY ebx
            Mov D$ClickFromPointX eax, D$ClickFromPointY ebx

            Mov eax D$H.EditedDialog
            If D$H.Hooked = eax
                Mov B$UserRightClickedOnControl &TRUE
            Else
                Mov B$UserClickedOnControl &TRUE
            End_If

            Call 'USER32.GetWindowPlacement' D$H.Hooked, Control

            Call ClipCursorInDialog

            popad | Mov eax &TRUE

EndP


        ..Else_If D@wParam = &WM_RBUTTONDOWN
            Mov ebx D@lParam, eax D$ebx, ebx D$ebx+4
            Mov D$FromPointX eax, D$FromPointY ebx
            Mov D$ClickFromPointX eax, D$ClickFromPointY ebx

            Mov B$UserRightClickedOnControl &TRUE

            Call 'USER32.GetWindowPlacement' D$H.Hooked, Control

            Call ClipCursorInSelected

            popad | Mov eax &TRUE

EndP

        ..Else_If D@wParam = &WM_LBUTTONUP
            Mov B$UserClickedOnControl &FALSE, B$UserRightClickedOnControl &FALSE
            Call KickDims
            Mov D$H.Hooked 0
            Call 'USER32.ClipCursor' &NULL

            popad | Mov eax &TRUE

EndP

        ..Else_If D@wParam = &WM_RBUTTONUP
L1:         Mov B$UserRightClickedOnControl &FALSE, B$UserClickedOnControl &FALSE
            Call KickDims
            Mov D$H.Hooked 0
            Call 'USER32.ClipCursor' &NULL

            popad | Mov eax &TRUE

EndP

        ..Else_If D@wParam = &WM_MOUSEMOVE
            Mov ebx D@lParam, eax D$ebx, ebx D$ebx+4
            Mov D$FromPointX eax, D$FromPointY ebx

            .If B$UserClickedOnControl = &TRUE
                Mov eax D$FromPointX | sub eax D$ClickFromPointX
                add D$Control.rcNormalPosition.left eax
                add D$Control.rcNormalPosition.right eax
                add D$ClickFromPointX eax

                Mov ebx D$FromPointY | sub ebx D$ClickFromPointY
                add D$Control.rcNormalPosition.Top ebx
                add D$Control.rcNormalPosition.Bottom ebx
                add D$ClickFromPointY ebx

                Call 'USER32.SetWindowPlacement' D$H.Hooked, Control
                Mov B$ModifiedControl &TRUE

            .Else_If B$UserRightClickedOnControl = &TRUE
                Mov eax D$FromPointX | sub eax D$ClickFromPointX
                add D$Control.rcNormalPosition.right eax
                add D$ClickFromPointX eax

                Mov eax D$FromPointY | sub eax D$ClickFromPointY
                add D$Control.rcNormalPosition.Bottom eax
                add D$ClickFromPointY eax

                Call 'USER32.SetWindowPlacement' D$H.Hooked, Control
                Mov B$ModifiedControl &TRUE

            .End_If

            popad | Mov eax &TRUE | ExitP

        ..End_If

    ...End_If

L9: popad
    Call 'USER32.CallNextHookEx' D$H.Hook D@nCode D@wParam D@lParam
EndP


; User Left-Clicked on the Edited Dialog. The 'Control' Structure contains the
; 'H.Hooked' Window dimentions.

ClipCursorInDialog:
    Call 'USER32.GetClientRect' D$H.EditedDialog RECT
    Call 'USER32.ClientToScreen' D$H.EditedDialog RECTleft
    Call 'USER32.ClientToScreen' D$H.EditedDialog RECTright

    Mov eax D$H.Hooked
    If eax = D$H.EditedDialog
      ; Target = Dialog >>> Bottom-Right limits = Screen limits, (for resizing Dialog):
        Call 'USER32.GetSystemMetrics' &SM_CXSCREEN | Mov D$RECTright eax
        Call 'USER32.GetSystemMetrics' &SM_CYSCREEN | Mov D$RECTbottom eax

    Else
      ; Target = Control
      ; >>> Bottom-Right limits = Dialog limits
      ; >>> Top-Left limits = Dialog limits - (Mouse Pos - Control Top-Left)
        Move D$STRUC.POINT+POINTX D$Control.rcNormalPosition.left
        Move D$STRUC.POINT+POINTY D$Control.rcNormalPosition.top
        Call 'USER32.ClientToScreen' D$H.EditedDialog STRUC.POINT
        Mov eax D$FromPointX | sub eax D$STRUC.POINT+POINTX
        Mov ebx D$FromPointY | sub ebx D$STRUC.POINT+POINTY
        add D$RECTleft eax | add D$RECTtop ebx

    End_If

    Call 'USER32.ClipCursor' RECT
ret


[STRUC.POINT:
 D$ ?
 D$ ?]

[EditedDialogX: D$ ?
 EditedDialogY: D$ ?]

[SlideBarX: D$ ?
 SlideBarY: D$ ?]

ClipCursorInSelected:
    Mov eax D$H.Hooked | On eax = D$H.EditedDialog, jmp ClipCursorInDialog

  ; Dialog Dims:
    Call 'USER32.GetClientRect' D$H.EditedDialog RECT
    Move D$EditedDialogX D$RECTleft, D$EditedDialogY D$RECTtop
    Call 'USER32.ClientToScreen' D$H.EditedDialog EditedDialogX

  ; Control Left-Top Dims:
    Move D$RECTleft D$Control.rcNormalPosition.left
    Move D$RECTtop D$Control.rcNormalPosition.top
    Call 'USER32.ClientToScreen' D$H.EditedDialog RECTleft
    Call 'USER32.ClientToScreen' D$H.EditedDialog RECTright

  ; Target = Control >>> Limit resizing to 25/25:
    Move D$STRUC.POINT+POINTX D$Control.rcNormalPosition.right
    Move D$STRUC.POINT+POINTY D$Control.rcNormalPosition.bottom
    Call 'USER32.ClientToScreen' D$H.EditedDialog STRUC.POINT
;;
  For sizing, if the user Right-Clicks rigth upon the lower right corner, OK, but, if
  he Clicks, say, in the middle of the Control, we have to substract that 'Half-Size'
  of the control from the upper-left limit of the ClipCursor Call, to let the modification
  go down to the minimum allowed size:
;;
    Mov ebx D$EditedDialogX
    Mov eax D$STRUC.POINT+POINTX | sub eax D$FromPointX
    sub eax D$MinimumX
    sub D$RECTleft eax ;| add D$RECTleft 25
  ; Don't let the Mouse go outside the Dialog, in cases of compensation.
    On D$RECTleft < ebx, Mov D$RECTleft ebx

    Mov ebx D$EditedDialogY
    Mov eax D$STRUC.POINT+POINTY | sub eax D$FromPointY
    sub eax D$MinimumY
    sub D$RECTtop eax ;| add D$RECTtop 25
  ; Don't let the Mouse go outside the Dialog, in cases of compensation.
    On D$RECTtop < ebx, Mov D$RECTtop ebx

    Call 'USER32.ClipCursor' RECT
ret

____________________________________________________________________________________________

; 'HookComments'

Proc InstallHook:
    Call 'KERNEL32.GetCurrentThreadId'
    Call 'USER32.SetWindowsHookExA' &WH_MOUSE, MouseProc, &NULL, eax
    Mov D$H.Hook eax
EndP


Proc UninstallHook:
    On D$H.Hook <> 0, Call 'USER32.UnhookWindowsHookEx' D$H.Hook
    Mov D$H.Hook 0
EndP


;;
  Force the New Dims and Pos (by Mouse action) to be displayed and updated (by calling
  either 'UpdateControlDims' or 'UpdateDialogDims':
;;

KickDims:
    Mov eax D$H.Hooked

    .If eax <> D$H.EditedDialog
        Call 'USER32.GetDlgCtrlID' D$H.Hooked

        Mov edi ControlsIDlist, ecx ControlsIDlistdWords
        repne scasd

L1:     If ecx > 0
            Mov eax ControlsIDlistdWords
            sub eax ecx                         ; eax = ID list position (in ControlsIDlist)
            Mov ecx Line_empty+1 | imul eax ecx ; what control dim
            Push eax
            Call 'USER32.SendMessageA' D$H.DialogList &LB_SETTOPINDEX eax 0 ; Pos
            Pop eax | add eax Line_Dim
            Call 'USER32.SendMessageA' D$H.DialogList &LB_SETCURSEL eax 0   ; select
            Call SetDialogTools
            On B$ModifiedControl = &TRUE, Call UpdateControlDims
        End_If

    .Else
        Call 'USER32.SendMessageA' D$H.DialogList &LB_SETTOPINDEX 0 0
        Call 'USER32.SendMessageA' D$H.DialogList &LB_SETCURSEL Line_Dim 0

        Call SetDialogTools

        On B$ModifiedControl = &TRUE, Call UpdateDialogDims
    .End_If
ret

____________________________________________________________________________________________

;;
  User has Clicked down on a Control in the Edited Dialog. We search in 
  'D$EditedDialogBoxData' for what Control.
  
  D$FromPointX, D$FromPointY hold the Mouse coordinates.
;;

[HookedID: D$ ?
 HookedControlStyle: D$ ?
  HookedControlClass: D$ ?]

[ClassTail: D$ ? # 10]

[HookedControlStylePtr: D$ ?
 HookedControlClassPtr: D$ ?
 ClassNameInside: D$ ?]

SearchForWhatControl:
    Push D$FromPointX, D$FromPointY

        Call 'USER32.GetWindowRect' D$H.EditedDialog EditedDialogRectangle

        Mov eax D$FromPointX
        cmp eax D$EditedDialogRectangle | jb L9>>
        cmp eax D$EditedDialogRectangle+8 | ja L9>>
        Mov eax D$FromPointY
        cmp eax D$EditedDialogRectangle+4 | jb L9>>
        cmp eax D$EditedDialogRectangle+12 | ja L9>>

        Mov eax D$EditedDialogRectangle | sub D$FromPointX eax
        Mov eax D$EditedDialogRectangle+4 | sub D$FromPointY eax
        Mov ebx D$EditedDialogRectangle+12 | sub ebx eax
        Push ebx
        Call 'USER32.GetClientRect' D$H.EditedDialog EditedDialogRectangle
        Pop ebx
        sub ebx D$EditedDialogRectangle+12 | sub D$FromPointY ebx


        Mov D$BaseUnitX1 4, D$BaseUnitY1 8, D$BaseUnitX2 0, D$BaseUnitY2, 0     ; <<< the

        Call 'USER32.MapDialogRect' D$H.EditedDialog BaseUnits               ; <<< trick

        Mov eax D$FromPointX, edx 0 | shl eax 2 | div D$BaseUnitX1 | Mov D$FromPointX eax
        Mov eax D$FromPointY, edx 0 | shl eax 3 | div D$BaseUnitY1 | Mov D$FromPointY eax

        Mov D$HookedID 0, D$H.Hooked 0

        Mov esi D$EditedDialogBoxData

        add esi 8  ; Style // Extended Style.

        Mov eax 0 | lodsw | Mov ecx eax | cmp ecx 0 | je L8>>   ; How many Controls
        add esi (4*2)                                           ; Dims
        While W$esi <> 0 | add esi 2 | End_While | add esi 2    ; Menu.
        While W$esi <> 0 | add esi 2 | End_While | add esi 2    ; Class.
        While W$esi <> 0 | add esi 2 | End_While | add esi 2    ; Title.
        While W$esi <> 0 | add esi 2 | End_While | add esi 2    ; Font.

        AlignOn 4, esi

      ; Parse Controls (last good one = top one at that coordinate):

L0:     add esi 8  ; Style // Extended Style.

      ; Here are the searched Dims: X, Y, W, H (Words).
        Mov ax W$esi   | cmp D$FromPointX eax | jb L2>>
        Mov ax W$esi+2 | cmp D$FromPointY eax | jb L2>>
        Mov ax W$esi+4 | add ax W$esi | cmp D$FromPointX eax | ja L2>
        Mov ax W$esi+6 | add ax W$esi+2 | cmp D$FromPointY eax | ja L2>
            Move W$HookedID W$esi+8

            Mov eax esi | sub eax 8 | Mov D$HookedControlStylePtr eax
            Move D$HookedControlStyle D$eax

            add eax 18 | Mov D$HookedControlClassPtr eax
            Move D$HookedControlClass D$eax

            Mov B$ClassNameInside &FALSE
            If W$eax <> 0FFFF
                Push esi
                    Mov esi eax, edi HookedControlClass
                    While W$esi <> 0
                        movsw
                    End_While
                    movsw
                Pop esi
                Mov B$ClassNameInside &TRUE
            End_If

            Mov eax 0

L2:     Call NextControl | dec ecx | jnz L0<<

        If D$HookedID <> 0
            Push ebx | Call GetSmallerReSize | Pop ebx

            Mov ebx D$HookedControlStylePtr
            Mov D$ebx &BS_PUSHBUTTON__&BS_BITMAP__&BS_FLAT__&WS_CHILD__&WS_VISIBLE
            Mov ebx D$HookedControlClassPtr, D$ebx 080_FFFF ; Button.
            On B$ClassNameInside = &TRUE, Call AdjustTitle
            Call ShowDialogResult
            Call 'USER32.GetDlgItem' D$H.EditedDialog, D$HookedID
            Mov D$H.Hooked eax

        Else
L8:         Move D$H.Hooked D$H.EditedDialog
            Mov D$MinimumX 200, D$MinimumY 50

        End_If

L9:     Pop D$FromPointY, D$FromPointX
ret


[MinimumX: D$ ?
 MinimumY: D$ ?]

GetSmallerReSize:
    Mov D$MinimumX 10, D$MinimumY 10

    Mov esi D$HookedControlStylePtr

    .If D$esi+18 = 0_81_FFFF  ; +18 >>> Class = 0FFFF 081 >>> Edit Control
        test D$esi &WS_VSCROLL ZERO L1>
            add D$MinimumX 30
L1:     test D$esi &WS_HSCROLL ZERO L2>
            add D$MinimumY 30
    .Else_If D$esi+18 = 0_83_FFFF  ; +18 >>> Class = 0FFFF 083 >>> ListBox
        add D$MinimumX 30 | add D$MinimumY 30
    .Else_If D$esi+18 = 0_85_FFFF  ; +18 >>> Class = 0FFFF 083 >>> ComboBox
        add D$MinimumX 30 | add D$MinimumY 30
    .End_If

L2: ret



; jump over Dims and search for end of Control Data. Pointer in esi:

NextControl:
  ; esi > Dims: X, Y, W, H (Words)
  ;             ID
  ;             0FFFF 0080 ou "msctls_progress32", 0
  ;             "Title", 0
  ;             0
    add esi 10

    If W$esi = 0FFFF
        add esi 4
    Else
        While W$esi <> 0 | add esi 2 | End_While | add esi 2
    End_If

    While W$esi <> 0 | add esi 2 | End_While | add esi 4

    AlignOn 4, esi
ret


; If Class Name, recover the whole length with a simulated Title (the old part of the
; Classe Name, is simply made one sigle Title with the real Title, by inserting an 'X'
; instead of the zero Class string termination:

AdjustTitle:
    Mov ebx D$HookedControlClassPtr
    While W$ebx <> 0 | add ebx 2 | End_While | Mov B$ebx 'X'
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; This is for the edited dialog proc. Used to know what control user clicked on, and
; after, make the according "dim" controls appear in the editor:

[ControlsIDlist: D$ ? # 1000]

[ControlsIDlistdWords 1000]

MakeControlIDList:
    Mov edi ControlsIDlist, eax 0, ecx ControlsIDlistdWords | rep stosd ; clear ID table
    Mov ebx ControlsIDlist
    Mov edi D$EditedDialogBoxData
    Mov edx 0, dx W$edi+8                       ; number of controls in edx
    On edx = 0, ret
    add edi 22
    Mov ax 0, ecx 100 | repne scasw
    Mov ecx 100 | repne scasw                   ; edi > end of dialog data
L0: test edi 00_11 ZERO L1>
        add edi 2                               ; > start of Control data
L1: On edx = 0, jmp L9>
    add edi 16                                  ; > ID number
    Move W$ebx W$edi | add ebx 4 | add edi 2
    If W$edi = 0FFFF                            ; type by number
        add edi 4
    Else
        Mov ax 0, ecx 100 | repne scasw         ; type by name
    End_If
    Mov ax 0, ecx 100 | repne scasw             ; control title
    add edi 2                                   ; > end of Control data
    dec edx | jmp L0<
L9: ret

_______________________________________________________________________________

; Menu for Dialog-Edition Dialog:

[H.DialogMenu: D$ ?
 DialogPopUpMenuTemplate: D$ ?
 DialogPopUpMenuExit: D$ ?]

[DETemplates: B$ 'Templates' EOS
    DEadd: B$ 'Add Control' EOS
    DEdel: B$ 'Delete Control' EOS

    DEdelMenu: B$ 'Delete Menu' EOS
    DEreplaceMenu: B$ 'Replace Menu' EOS

    DEreset: B$ 'New Dialog' EOS

 DEExit: B$ 'Exit' EOS
    DEsaveToResources: B$ 'Save to Resources and Exit' EOS
    DEsaveToClipBoard: B$ 'Save to Clipboard and Exit' EOS
    DEsaveToDisk: B$ 'Save to Disk and Exit' EOS

    DEabort: B$ 'Abort' EOS

 DEhelp: B$ 'Help' EOS]

[ID_DETemplates 500
    ID_DEadd 501
    ID_DEdel 502

    ID_DEdelMenu 503
    ID_DEreplaceMenu 504

    ID_DEreset 505

 ID_DEExit 510
    ID_DEsaveToResources 511
    ID_DEsaveToClipBoard 512
    ID_DEsaveToDisk 513

    ID_DEabort 514

 ID_DEhelp 520]

; Creates and Dispatch the menu for Dialog Edition:

CreateDialogMenu:
  ; Main Menu:
    Call 'USER32.CreateMenu' | Mov D$H.DialogMenu eax

  ; Templates PopUp:
    Call 'USER32.CreatePopupMenu' | Mov D$DialogPopUpMenuTemplate eax

    Call 'USER32.AppendMenuA' D$DialogPopUpMenuTemplate, 0, ID_DEadd, DEadd
    Call 'USER32.AppendMenuA' D$DialogPopUpMenuTemplate, 0, ID_DEdel, DEdel
    Call 'USER32.AppendMenuA' D$DialogPopUpMenuTemplate, &MF_SEPARATOR, &NULL, &NUll
    Call 'USER32.AppendMenuA' D$DialogPopUpMenuTemplate, 0, ID_DEdelMenu, DEdelMenu
    Call 'USER32.AppendMenuA' D$DialogPopUpMenuTemplate, 0, ID_DEreplaceMenu, DEreplaceMenu
    Call 'USER32.AppendMenuA' D$DialogPopUpMenuTemplate, &MF_SEPARATOR, &NULL, &NUll
    Call 'USER32.AppendMenuA' D$DialogPopUpMenuTemplate, 0, ID_DEreset, DEreset

    Call 'USER32.InsertMenuA' D$H.DialogMenu, 0,
                              &MF_BYCOMMAND__&MF_POPUP__&MF_STRING,
                              D$DialogPopUpMenuTemplate, DETemplates

  ; Exit PopUp:
    Call 'USER32.CreatePopupMenu' | Mov D$DialogPopUpMenuExit eax

    Call 'USER32.AppendMenuA' D$DialogPopUpMenuExit, 0, ID_DEsaveToResources, DEsaveToResources
    Call 'USER32.AppendMenuA' D$DialogPopUpMenuExit, 0, ID_DEsaveToClipBoard, DEsaveToClipBoard
    Call 'USER32.AppendMenuA' D$DialogPopUpMenuExit, 0, ID_DEsaveToDisk, DEsaveToDisk
    Call 'USER32.AppendMenuA' D$DialogPopUpMenuExit, &MF_SEPARATOR, &NULL, &NUll
    Call 'USER32.AppendMenuA' D$DialogPopUpMenuExit, 0, ID_DEabort, DEabort

    Call 'USER32.InsertMenuA' D$H.DialogMenu, 1,
                              &MF_BYCOMMAND__&MF_POPUP__&MF_STRING,
                              D$DialogPopUpMenuExit, DEExit

  ; Menu Help:
    Call 'USER32.AppendMenuA' D$H.DialogMenu, 0, ID_DEhelp, DEhelp

    Call 'USER32.SetMenu' D$H.DialogEditor, D$H.DialogMenu
ret

_______________________________________________________________________________
________________________________________________________________________________


FillDialogListBox:
    ; preserv Pos and Selection
    Call 'USER32.SendMessageA' D$H.DialogList &LB_GETTOPINDEX D$DialogListIndex 0
    Push eax

    Call 'USER32.SendMessageA' D$H.DialogList &LB_GETCURSEL D$DialogListIndex 0
    Push eax

    Call 'USER32.SendMessageA' D$H.DialogList &LB_RESETCONTENT 0 0     ; makes list empty

    Mov edi D$NewDialogTemplateText

L1: Push edi
      Call 'USER32.SendMessageA' D$H.DialogList &LB_ADDSTRING  0  edi   ; fill
    Pop edi
    Mov al 0, ecx 200 | repne scasb
    cmp B$edi 255 | jne L1<

    ; restore Selection and Pos:
    Pop ebx, eax
    Push ebx
    Call 'USER32.SendMessageA' D$H.DialogList &LB_SETTOPINDEX eax 0
    Pop eax
    Call 'USER32.SendMessageA' D$H.DialogList &LB_SETCURSEL eax 0
L9: ret


; Restore end mark, in case of second run:

ResetDialogListBox:
    Mov edi D$NewDialogTemplateText, ecx Line_empty+1, al 0

L0: Push ecx
        Mov ecx 200 | repne scasb
    Pop ecx
    loop L0<

    Mov B$edi 255
ret

____________________________________________________________________________________
___________________________________________________________________________________

; Main routines for dialog edition.
_____________________________________________________________________________________
_____________________________________________________________________________________

ReleaseDialogMemories:

    Call VirtualFree NewDialogTemplateText

    Call VirtualFree EditedDialogBoxData

ret


InitDialogMemory:

    Call VirtualFree NewDialogTemplateText

    Call VirtualAlloc NewDialogTemplateText,
                      010000

    Call VirtualFree EditedDialogBoxData

    Call VirtualAlloc EditedDialogBoxData,
                      010000

ret


; missing the &ICC_LINK_CLASS ?
InitDialogEdition:

    Call 'ComCtl32.InitCommonControlsEx' CodeAddressFormClassName@Init_All_Common_Controls ; added by Guga

    If D$FL.DialogEdition = &TRUE
        Call Beep | ret
    End_If
    Call InitDialogMemory

    Mov edi D$NewDialogTemplateText,
        esi DefaultDialogTemplateText,
        ecx D$DefaultDialogTemplateTextLen
    rep movsb
    Mov D$edi 0FFFFFFFF                          ; End mark

    Mov edi D$EditedDialogBoxData,
        esi DefaultEditedDialogBoxData,
        ecx D$DefaultEditedDialogBoxDataLenght
    rep movsb

    Mov edi DialogList, eax 0, ecx 300
    repne scasd | sub edi 4
    Mov D$DialogListPtr edi

    Mov D$DialogMenutrueID 0, D$ActualMenutestID 0

ReInitDialogEdition:
    Call InstallHook

    Call FromTextToBinTemplate
;;
 Little difficulty: User can check either "POPUP" or "CHILD". Results visible dialog
 must remain "POPUP/VISIBLE" because created with "CreateDialogIndirectParamA"
 (either "CHILD" or not "VISIBLE" would make it unvisible for edition. So do we
 save true Dialog Style value and set the fitting one for edition:
;;
    Mov eax D$EditedDialogBoxData
    Push D$eax
        and D$eax 0FFF_FFFF | or D$eax 0_9000_0000

        Call 'USER32.CreateDialogIndirectParamA' D$H.Instance, D$EditedDialogBoxData,
                                                 D$H.EditWindow, EditedDialogBoxProc, 0
        Mov D$H.EditedDialog eax

        Call 'USER32.DialogBoxIndirectParamA' D$H.Instance,      ; "create..." > modeless
                    DialogBoxData, 0, EditDialogBoxProc, 0      ; "Dialog..." > modal
      ; Editor Handle in 'H.DialogEditor'.

        Mov eax D$EditedDialogBoxData
    Pop D$eax

    Mov W$PreviousControlID 0FFFF

    Call UninstallHook

    Call SortDialogs
ret


[DoubleID: B$ ? # 20]

SortDialogs:
  ; ID / Ptr / Size // ...
L0: Mov esi DialogList, edi esi | add edi 12

    While D$edi <> 0
        Mov eax D$esi
        If eax > D$edi
            Exchange D$esi D$edi
            Exchange D$esi+4 D$edi+4
            Exchange D$esi+8 D$edi+8 | jmp L0<
        End_If
        add esi 12 | add edi 12
    End_While
ret

; Verify that all Dialogs have unique IDs:

[SameIdAllowed: D$ ?]

Proc CheckNotUsedId:
    Argument @ID, @Parent
    Uses esi

        On B$SameIdAllowed = &TRUE, ExitP

L0:     Mov esi DialogList

        While D$esi <> 0
            Mov eax D$esi
            If eax = D@ID
                Call WriteDecimalID, eax, DoubleID
                Call 'USER32.MessageBoxA' D@Parent, {B$ 'This Dialog ID already exist' EOS},
                                          DoubleID, &MB_OK
                Mov eax &IDCANCEL | ExitP
            Else
                add esi 12 | add edi 12
            End_If
        End_While

        Mov eax &IDOK
EndP


Proc WriteDecimalID:
    Argument @Value, @Destination

    pushad
        Mov eax D@Value, edi D@Destination, ecx 10

        Mov D$edi ' ID ' | add edi 4

        Push 0-1
L0:     Mov edx 0 | div ecx | Push edx | cmp eax 0 | ja L0<

L0:     Pop eax | cmp eax 0-1 | je L9>
        add al '0' | stosb | jmp L0<

L9:     Mov B$edi 0
    popad
EndP

____________________________________________________________________________________________

[H.DialogList: D$ ?
 LastDialogListItem: D$ ?]

ScrollDownToLastControl:
  ; Scroll full down to last new added control edition:
    Call 'USER32.SendMessageA' D$H.DialogList &LB_GETCOUNT 0 0
    dec eax | Mov D$LastDialogListItem eax
    Push eax
        Call 'USER32.SendMessageA' D$H.DialogList &LB_SETTOPINDEX eax 0
    Pop eax
    sub eax 2
    Call 'USER32.SendMessageA' D$H.DialogList &LB_SETCURSEL  eax 0
ret


ScrollToInsertedControl:
    Call 'USER32.SendMessageA' D$H.DialogList &LB_GETCOUNT 0 0
    dec eax | Mov D$LastDialogListItem eax
    Mov eax D$DialogListIndex | add eax Line_Font
    Call 'USER32.SendMessageA' D$H.DialogList &LB_SETCURSEL  eax 0  ; select title
ret

[UpDownEndScroll 8]

[UserModifiedDim: D$ ?]

[FL.DialogEdition: D$ ?
 DialogLoadedFromResources: D$ ?]

; D$ControlIndex values (set by "SetDialogTools"):
;   0 > Style  1 > exStyle 2 > Dim   3 > ID   4 > Class   5 > Title   6 > Font
;   0_FFFF_FFFF if not yet or blank line.
[Line_Style 0 | Line_exStyle 1 | Line_Dim 2 | Line_ID 3 | Line_Class 4 | Line_Title 5 | Line_Font 6 | Line_empty 7]
[ProposedUpDowmChange: D$ ?] ; The UDN_DELTAPOS WM_NOTIFY Message is sent before the
; Edit Control update. I use this Message to ease differenciating between all the
; Various Edit Controls. Immidiately after 'WriteDim' has used it, it reset it to
; zero. The Value is a signed dWord.

Proc EditDialogBoxProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    .If B$OnMenuEdition = &TRUE
        If D@msg = &WM_NOTIFY
            jmp L1>
        Else_If D@msg = &WM_VSCROLL
            jmp L1>
        Else_If D@msg = &WM_COMMAND
L1:         Call 'USER32.SendMessageA' D$H.MenuEditor, &WM_COMMAND, &IDCANCEL, 0
        End_If
    .End_If

    If D$UserModifiedDim <> &FALSE
        Mov ecx D$UserModifiedDim | Call WriteDim
        Mov D$UserModifiedDim &FALSE
    End_If

    ...If D@msg = &WM_NOTIFY
        Mov ebx D@lParam, eax D$ebx+8     ; user click and hold on Updown in Dim edition
        If eax = &UDN_DELTAPOS
            Mov eax D$ebx, edi TABLE.H.DialogControls, ecx 12 | repne scasd
            sub edi 8 | Mov ecx D$edi               ; each "dim": string / edit / UpDown
            Move D$ProposedUpDowmChange D$ebx+16    ; This is the proposed change (signed dWord)
            Call WriteDim
            Call 'USER32.SetForegroundWindow' D$H.DialogEditor
            popad | Mov eax &FALSE | jmp L9>>
        End_If

    ...Else_If D@msg = &WM_VSCROLL        ; user clicked and release Updown in Dim edition
        Mov eax D@wParam | and eax UpDownEndScroll | jz C9>
          Mov eax D@lParam, edi TABLE.H.DialogControls, ecx 12 | repne scasd
          sub edi 8 | Mov ecx D$edi           ; each "dim": string / edit / UpDown
          Mov D$UserModifiedDim ecx
          Call WriteDim
          Call 'USER32.SetForegroundWindow' D$H.DialogEditor

; ID_DEadd 3  ID_DEdel 4  ID_DEreset 5  ID_DEexit 6  ID_DEhelp 7

; D$StyleHelpButtonsHandles > List of Styles Help
C9:
    ...Else_If D@msg = &WM_COMMAND
        .If D@wParam = ID_DEadd
            Call AddOneControl
            Call 'USER32.SendMessageA' D$H.DialogList, &LB_GETCURSEL, 0, 0
            sub eax 2
            Call 'USER32.SendMessageA' D$H.DialogList, &LB_SETCURSEL, eax, 0
            Call SetDialogTools
            Call 'USER32.SetForegroundWindow' D$H.DialogEditor
            jmp L7>>

        .Else_If D@wParam = ID_DEdel
            Call DelOneControl
            Call 'USER32.SetForegroundWindow' D$H.DialogEditor
            jmp L7>>

        .Else_If D@wParam = ID_DEdel_Menu
            Call SearchDialogLine
            If D$edi = 'FFFF'
                Mov D$ActualMenutestID 0, D$DialogMenuTrueID 0

                Push esi | Mov esi {B$ "0 ;      no Menu" EOS} | L0: test B$esi 0_FF ZERO P0> | movsb | jmp L0< | P0: Pop esi

                Call FromTextToBinTemplate | Call ShowDialogResult
                Call FillDialogListBox
            Else
                Call NoDialogMenu
            End_If
            jmp L7>>

        .Else_If D@wParam = ID_DEreplaceMenu
            Call SearchDialogLine
            If D$edi = 'FFFF'
                Call AddMenuToDialog
            Else
                Call NoDialogMenu
            End_If
            jmp L7>>

        .Else_If D@wParam = ID_DEreset
            Call ResetDialogListBox | Call FillDialogListBox
            Call 'USER32.SetForegroundWindow' D$H.DialogEditor
            Mov W$PreviousControlID 0FFFF
            Call FromTextToBinTemplate | Call ShowDialogResult | Call FillDialogListBox
            jmp L7>>

        .Else_If D@wParam = ID_DEhelp
L1:         Call Help, B_U_AsmName, DialogHelp, ContextHlpMessage | jmp L7>>

        .Else_If D@wParam = ID_DEsaveToResources
            Mov ebx 4 | Call ExitDialog | jmp L7>>

        .Else_If D@wParam = ID_DEsaveToClipBoard
            Mov ebx 0 | Call ExitDialog | jmp L7>>

        .Else_If D@wParam = ID_DEsaveToDisk
            Mov ebx 8 | Call ExitDialog | jmp L7>>

        .Else_If D@wParam = ID_DEabort
            Mov ebx 12 | Call ExitDialog | jmp L7>>

        .Else_If D@wParam = &IDCANCEL
            Call 'USER32.EndDialog' D@hwnd 0
            Call 'USER32.DestroyWindow' D$H.EditedDialog
            Mov D$H.DialogEditor 0, D$H.EditedDialog 0
            Mov D$FL.DialogEdition &FALSE, B$DialogLoadedFromResources &FALSE | jmp L7>>
        .End_If

        Mov eax D@wParam, ebx eax, ecx D@lParam
      ; buttons handles in ecx
        shr eax 16 | and ebx 0FFFF

        ..If ecx = D$H.DialogList
            On eax = &LBN_SELCHANGE, Call SetDialogTools

        ..Else
            .If eax = &BN_CLICKED
              ; Check button?
                On ebx = 1, jmp L7>>
              ; case of user hit [Return]
                Mov esi TABLE.H.DialogControls, ebx 0
L0:             lodsd
                If eax = ecx
                   Call WriteStyle
                Else_If eax <> 0
                  ; stop on 0 (user click, but not on a 'moveable' control)
                    add ebx 4 | jmp L0<
                End_If

                On ecx = 0, jmp L2>
                    Mov esi TABLE.H.StyleHelpButtons, ebx 0
L0:                 lodsd

              ; Ready for Styles Help ('RunDialogHelp' / 'HelpDialogProc'):
                    If eax = ecx
                        Call ShowStyleInfo
                    Else_If eax <> 0      ; stop on 0 (user click, but not on a 'moveable' control)
                        add ebx 4 | jmp L0<
                    End_If
                ;  Call 'USER32.SetForegroundWindow' D$H.DialogEditor

            .Else_If eax = &EN_CHANGE           ; Edit Box?
                If D$ControlIndex = Line_Title
                    Call WriteTitle
                Else_If D$ControlIndex = Line_ID
                    Call WriteID
                Else_If D$ControlIndex = Line_Class
                    Call WriteTitle              ; reuse for Dialog Class
                End_If
L2:             Call 'USER32.SetForegroundWindow' D$H.DialogEditor

            .Else_If eax = &LBN_SELCHANGE       ; List Box >>> font syze or type, or Class
                If D$TABLE.H.DialogControls+4 = 0
                    Call WriteClass
                Else_If ecx = D$TABLE.H.DialogControls
                    Call WriteFontType
                Else
                    Call WriteFontSize
                End_If
                Call 'USER32.SetForegroundWindow' D$H.DialogEditor

            .Else_If eax = &EN_UPDATE                  ; direct input in Dim edit control
                On D$ControlIndex = Line_Dim, Call WriteDim
                Call 'USER32.SetForegroundWindow' D$H.DialogEditor

            .End_If

        ..End_If

    ...Else_If D@msg = &WM_INITDIALOG
        Move D$H.DialogEditor D@hwnd

        Call 'USER32.GetDlgItem' D@hwnd ID_Ilist | Mov D$H.DialogList eax

        Call CreateDialogMenu

        If B$OnClipDialog = &FALSE
            Call ResetDialogListBox | Call FillDialogListBox
            Call ScrollDownToLastControl
        Else
            Call FillDialogListBox | Call ScrollDownToLastControl
            Mov B$OnClipDialog &FALSE
        End_If
        Mov D$FL.DialogEdition &TRUE

        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$STRUC.WINDOWCLASS@hIcon

    ...Else_If D@msg = &WM_CTLCOLORLISTBOX
        jmp L1>

    ...Else_If D@msg = &WM_CTLCOLOREDIT
L1:     Call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | Mov eax D$H.DialogsBackGroundBrush | jmp L9>

    ...Else
L8:     popad | Mov eax &FALSE | jmp L9>
    ...End_If

L7: popad | Mov eax &TRUE

L9: EndP

_____________________________________________________________________________________
_____________________________________________________________________________________

; Routine to show particular controls on the editor dialog, depending on what main list
; line user click.
_____________________________________________________________________________________
_____________________________________________________________________________________

[TABLE.H.DialogControls: D$ ? # 40]
[TABLE.H.StyleHelpButtons: D$ ? # 40]
[EditClass: B$ 'EDIT' EOS]

; these tables are used to set contents and pos of controls for style edition:

; Dialog window styles:

[DialogCheckingMask:
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[DialogExcludeBitTable:
 D$ 0C0000000
 D$ 0C0000000
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[DialogMustHaveBItTable:
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0C00000
 D$ 0C80000
 D$ 0C80000
 D$ 0C80000
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[DialogTextTable:
 B$ 'WS_CHILD' EOS
 B$ 'WS_POPUP' EOS
 B$ 'WS_VISIBLE' EOS
 B$ 'WS_DISABLED' EOS
 B$ 'WS_BORDER' EOS
 B$ 'WS_CAPTION' EOS
 B$ 'WS_HSCROLL' EOS
 B$ 'WS_VSCROLL' EOS
 B$ 'WS_SYSMENU' EOS
 B$ 'WS_MAXIMIZEBOX' EOS
 B$ 'WS_MINIMIZEBOX' EOS
 B$ 'DS_CONTEXTHELP' EOS
 B$ 'DS_ABSALING' EOS
 B$ 'DS_CENTER' EOS
 B$ 'WS_THICKFRAME' EOS
 B$ 'DS_CONTROL' EOS
 B$ 'DS_MODALFRAME' EOS
 B$ 'DS_NOFAILCREATE' EOS
 B$ 'DS_NOIDLEMSG' EOS
 B$ 'DS_SETFOREGROUND' EOS
 B$ 'DS_SYSMODAL' EOS 0]

[DialogBitTable:
 D$ &WS_CHILD
 D$ &WS_POPUP
 D$ &WS_VISIBLE
 D$ &WS_DISABLED
 D$ &WS_BORDER
 D$ &WS_CAPTION
 D$ &WS_HSCROLL
 D$ &WS_VSCROLL
 D$ &WS_SYSMENU
 D$ &WS_MAXIMIZEBOX
 D$ &WS_MINIMIZEBOX
 D$ &DS_CONTEXTHELP
 D$ &DS_ABSALIGN
 D$ &DS_CENTER
 D$ &WS_THICKFRAME
 D$ &DS_CONTROL
 D$ &DS_MODALFRAME
 D$ &DS_NOFAILCREATE
 D$ &DS_NOIDLEMSG
 D$ &DS_SETFOREGROUND
 D$ &DS_SYSMODAL] ; &DS_CENTERMOUSE

[WS_CHILDhelp: B$ "WS_CHILD:

> Child window.

Cannot be used with the WS_POPUP style." EOS]

[WS_POPUPhelp: B$ "WS_POPUP:

> Pop-up window.

Cannot be used with the WS_CHILD style." EOS]

[WS_VISIBLEhelp: B$ "WS_VISIBLE:

window is initially visible." EOS]

[WS_DISABLEDhelp: B$ "WS_DISABLED:

The window is initially disabled and can't receive input from the user." EOS]

[WS_BORDERhelp: B$ "WS_BORDER:

The window has a thin-line border." EOS]

[WS_CAPTIONhelp: B$ "WS_CAPTION:

The window has a title bar (includes WS_BORDER)." EOS]

[WS_HSCROLLhelp: B$ "WS_HSCROLL:

The window has an horizontal scroll bar." EOS]

[WS_VSCROLLhelp: B$ "WS_VSCROLL:

The window has a vertical scroll bar." EOS]

[WS_SYSMENUhelp: B$ "WS_SYSMENU:

The window has a System-menu box in the title bar.

Must be WS_CAPTION too." EOS]

[WS_MAXIMIZEBOXhelp: B$ "WS_MAXIMIZEBOX:

Adds the Maximize button to the title bar." EOS]

[WS_MINIMIZEBOXhelp: B$ "WS_MINIMIZEBOX:

Adds the Minimize button to the title bar." EOS]

[DS_CONTEXTHELPhelp: B$ "DS_CONTEXTHELP:

Adds a question mark in the title bar" EOS]

[DS_ABSALINGhelp: B$ "DS_ABSALIGN:

The coordinates of the dialog box will be screen coordinates instead of client area ones." EOS]

[DS_CENTERhelp: B$ "DS_CENTER:

Centers the dialog box in the working area." EOS]

[WS_THICKFRAMEhelp: B$ "WS_THICKFRAME:

The window has a sizing border.

Same as WS_SIZEBOX." EOS]

[DS_CONTROLhelp: B$ "DS_CONTROL:

The dialog will works as a child window of another dialog (example: A page in a Tabbed Dialog)." EOS]

[DS_MODALFRAMEhelp: B$ "DS_MODALFRAME:

The dialog will have a modal dialog-box frame." EOS]

[DS_NOFAILCREATEhelp: B$ "DS_NOFAILCREATE:

Creates the dialog even if an error occur, for example, if a child window cannot be created." EOS]

[DS_NOIDLEMSGhelp: B$ "DS_NOIDLEMSG:

Blocks the WM_ENTERIDLE messages that Windows would otherwise send to the owner of the dialog box when the dialog box is displayed." EOS]

[DS_SETFOREGROUNDhelp: B$ "DS_SETFOREGROUND:

Brings the dialog box to the foreground, like it could be done with a Call to SetForegroundWindow" EOS]

[DS_SYSMODALhelp: B$ "DS_SYSMODAL:

> System-modal dialog box.

The dialog will have the WS_EX_TOPMOST style." EOS
 0]


[DialogStylesHelp:
 D$ WS_CHILDhelp
 D$ WS_POPUPhelp
 D$ WS_VISIBLEhelp
 D$ WS_DISABLEDhelp
 D$ WS_BORDERhelp
 D$ WS_CAPTIONhelp
 D$ WS_HSCROLLhelp
 D$ WS_VSCROLLhelp
 D$ WS_SYSMENUhelp
 D$ WS_MAXIMIZEBOXhelp
 D$ WS_MINIMIZEBOXhelp
 D$ DS_CONTEXTHELPhelp
 D$ DS_ABSALINGhelp
 D$ DS_CENTERhelp
 D$ WS_THICKFRAMEhelp
 D$ DS_CONTROLhelp
 D$ DS_MODALFRAMEhelp
 D$ DS_NOFAILCREATEhelp
 D$ DS_NOIDLEMSGhelp
 D$ DS_SETFOREGROUNDhelp
 D$ DS_SYSMODALhelp
 D$ 0]
___________________________________________________________________________________

ShowDialogStyleControl:
    ShowSetOfCheckBoxes DialogTextTable
ret

__________________________________________________________________________________

; very simplified version of TranslateHexa
; (esi >>> first text hexa number char after leading '0'):

TranslateDialogHexa:
    Mov ebx 0
L0: lodsb | cmp al SPC | je L9>
            cmp al ';' | je L9>
            cmp al CR | jbe L9>
      sub al '0' | On al > 9, sub al 7
      shl ebx 4 | or bl al | jmp L0<
L9: ret                                     ; >>> ebx = binary value

TranslateDialogText8:
    Mov ecx 8
L0: Mov eax ebx | and eax 0_F000_0000 | shr eax 28
    add al '0' | On al > '9', add al 7
    shl ebx 4
    stosb | loop L0<
ret

TranslateDialogText4:
    shl ebx 16
    Mov ecx 4 | jmp L0<

TranslateDialogText2:
    shl ebx 24
    Mov ecx 2 | jmp L0<

____________________________________________________________________________________

ShowDialogStyles:
    Mov esi D$NewDialogTemplateText | add esi 3
    Call TranslateDialogHexa | Mov D$CheckActual ebx

    Move D$CheckingMask DialogCheckingMask
    Mov esi DialogBitTable

    Push ebp
        Call CheckControlStyles
    Pop ebp
ret


[CheckBit: D$ ?
 CheckActual: D$ ?
 CheckExclude: D$ ?
 CheckMust: D$ ?
 CheckBitPtr: D$ ?
 CheckResult: D$ ?
 CheckingMask: D$ ?]
____________________________________________________________________________________

CheckControlStyles:
;;
 check / uncheck the controls:

 Set by caller (WriteStyle):

      D$CheckActual = Actual value for style (full value)
      D$CheckingMask = Bits table adress of checking masks (the main difficulty)

      esi = ptr to Style Bits table
;;
    Mov edi TABLE.H.DialogControls, ecx 0
    While D$edi > 0
       add edi 4 | inc ecx                ; number of check boxes at screen in ecx
    End_While
;;
 Very killing problem: This routine MUST be able to Check/UnCheck the edition CheckBoxes
 according with the value of "Style" (D$CheckActual). The fact is that we can have, for
 exemple, for buttons styles, values sets like:

 BS_3STATE 5         BS_AUTO3STATE 6     BS_AUTOCHECKBOX 3  BS_AUTORADIOBUTTON 9
 BS_CHECKBOX 2       BS_DEFPUSHBUTTON 1  BS_GROUPBOX 7      BS_PUSHBUTTON 0
 BS_RADIOBUTTON 4

 ... what makes it impossible to trust any bit value for checking. For exemple, if user
 click on "BS_GROUPBOX" (= 00_111), we must not check 00_001 (BS_DEFPUSHBUTTON), 00_010
 (BS_CHECKBOX), and so on... Sorry, next lines are very very difficult to understand. The
 only readable solution would be to write a no end cases selections routine for each
 control class, but i have choosen this shorter way. A second difficulty is that zero
 may be significant - and Check / UnCheck- (exemple: BS_PUSHBUTTON) and that we have to
 discriminate 00_10 (BS_CHECKBOX, for exemple) from 00_10... Killing:
;;
    Mov edx D$CheckingMask, ebp 0              ; ebp = handles index for 'SendMessage'

L0: lodsd                                      ; loads one style value from Styles list
    pushad
      Mov edx D$edx                            ; CheckingMask  !!!!!!!!!!!!!!!!!!!!!!!!
      ; eax Style bit value from list
        ; (Style and eax) <> eax >>> false?:
      Mov ebx D$CheckActual | and ebx eax | cmp eax ebx | jne L6>
        ; (eax and CheckingMask) = 0  > next line
      Mov ebx eax | and ebx edx | cmp ebx 0 | jne L4>
        ; check only if really 0 wanted:
        cmp eax 0 | jne L5>
          ; true zero?:
          Mov eax D$CheckActual | and eax 00_111 | cmp eax 0 | je L5>
            jmp L6>
        ; last test for 'true', only if: bit value = (Bit value and CheckingMask):
L4:     Mov eax D$CheckActual | and eax edx | cmp ebx eax | jne L6>

L5:   Mov eax &TRUE | jmp L7>    ; check

L6:   Mov eax &FALSE             ; UnCheck

L7:   Call 'USER32.SendMessageA' D$TABLE.H.DialogControls+ebp &BM_SETCHECK eax 0
    popad | add ebp 4 | add edx 4 | loop L0<
ret


; Same as above, but for showing the checkboxes set at first time (user did'nt yet click
; at any check box, we just need actual style value and according checking mask table):

ShowControlStyles:
    Call SearchDialogLine | Mov esi edi | add esi 3
    Call TranslateDialogHexa | Mov D$CheckActual ebx

    add D$DialogListIndex Line_Class
        Call SearchWhatControlClass             ; ebx = indice of class (0 / 1 / 2 / 3...)
    sub D$DialogListIndex Line_Class

    ..If edi = ControlClassByNumber             ; >>> class by Number
        .If ebx = 0
            Move D$CheckingMask ButtonCheckingMask
            Mov esi ButtonBitTable
        .Else_If ebx = 1
            Move D$CheckingMask EditCheckingMask
            Mov esi EditBitTable
        .Else_If ebx = 2
            Move D$CheckingMask StaticCheckingMask
            Mov esi StaticBitTable
        .Else_If ebx = 3
            Move D$CheckingMask ListCheckingMask
            Mov esi ListBitTable
        .Else_If ebx = 4
            Move D$CheckingMask ScrollCheckingMask
            Mov esi ScrollBitTable
        .Else_If ebx = 5
            Move D$CheckingMask ComboCheckingMask
            Mov esi ComboBitTable
        .End_If
    ..Else                                      ; >>> class by Name
        .If ebx = 0
            Move D$CheckingMask UpDownCheckingMask
            Mov esi UpDownBitTable
        .Else_If ebx = 1
            ret                                 ; msctls_progress32 (no controls)
        .Else_If ebx = 2
            Move D$CheckingMask TrackCheckingMask
            Mov esi TrackBitTable
        .Else_If ebx = 3
            Move D$CheckingMask TreeCheckingMask
            Mov esi TreeBitTable
        .Else_If ebx = 4
            Move D$CheckingMask TabCheckingMask
            Mov esi TabBitTable
        .Else_If ebx = 5
            Move D$CheckingMask ListViewCheckingMask
            Mov esi ListViewBitTable
        .Else_If ebx = 6
            Move D$CheckingMask ToolBarCheckingMask
            Mov esi ToolBarBitTable
        .Else_If ebx = 7
            Move D$CheckingMask RichEdit20aCheckingMask
            Mov esi RichEdit20aBitTable
        .Else_If ebx = 8
            Move D$CheckingMask SysHeader32CheckingMask
            Mov esi SysHeader32BitTable
        .Else_If ebx = 9
            Move D$CheckingMask ReBarWindow32CheckingMask
            Mov esi ReBarWindow32BitTable
        .Else_If ebx = 10
            Move D$CheckingMask Tooltips_class32CheckingMask
            Mov esi Tooltips_class32BitTable
        .Else_If ebx = 11
            Move D$CheckingMask msctls_statusbar32CheckingMask
            Mov esi msctls_statusbar32BitTable
        .Else_If ebx = 12
            Move D$CheckingMask msctls_hotkey32CheckingMask
            Mov esi msctls_hotkey32BitTable
        .Else_If ebx = 13
            Move D$CheckingMask ComboBoxEx32CheckingMask
            Mov esi ComboBoxEx32BitTable
        .Else_If ebx = 14
            Move D$CheckingMask SysAnimate32CheckingMask
            Mov esi SysAnimate32BitTable
        .Else_If ebx = 15
            Move D$CheckingMask SysMonthCal32CheckingMask
            Mov esi SysMonthCal32BitTable
        .Else_If ebx = 16
            Move D$CheckingMask SysDateTimePick32CheckingMask
            Mov esi SysDateTimePick32BitTable
        .Else_If ebx = 17
            Move D$CheckingMask SysIPAddress32CheckingMask
            Mov esi SysIPAddress32BitTable
        .Else_If ebx = 18
            Move D$CheckingMask SysPagerCheckingMask
            Mov esi SysPagerBitTable
        .Else_If ebx = 19
            Move D$CheckingMask SysLinkCheckingMask
            Mov esi SysLinkBitTable
        .End_If
    ..End_If

    Call CheckControlStyles
ret

_______________________________________________________________________________________

[XdimText: B$ 'X position:' EOS]
[YdimText: B$ 'Y position:' EOS]
[WdimText: B$ 'Width:' EOS]
[HdimText: B$ 'Hight:' EOS]

; This Table is an array of 2 UDACCEL structures. First dWord is the delay before next
; Array runing, dWord 2 is the displacement:
[UDACCEL:
 n1Sec: D$ 0
 n1Inc: D$ 1
 n2Sec: D$ 2
 n2Inc: D$ 5]

[DialogDimTable:
 XdimText D$ 200 D$ 0
 YdimText D$ 240 D$ 0
 WdimText D$ 280 D$ 0
 HdimText D$ 320 D$ 0]

;;
 In "display edit box", ES_RIGHT (to align text number to the left, works only with
 ES_MULTILINE edit control style. (the box is de facto mono-line, of course). Without
 this, it sends a EN_CHANGE message at first display.
;;
ShowDimControls:
  ; First retrieve values from user text dim:
    Call SearchDialogLine
    Mov al SPC | Mov ecx 200 | repne scasb  ; space used as separator > count of data
    If D$DimIsForDialogWindow = &TRUE
        repne scasb                         ; jump over n (number of controls)
    End_If
    Mov esi edi | inc esi
    Call TranslateDialogHexa | Mov D$DialogDimTable+ 8 ebx

    Call TranslateDialogHexa | Mov D$DialogDimTable+20 ebx

    Call TranslateDialogHexa | Mov D$DialogDimTable+32 ebx

    Call TranslateDialogHexa | Mov D$DialogDimTable+44 ebx

    Mov ecx 4, esi DialogDimTable, ebx 0
L0: Push ecx
      ; display text:
        Push esi, ebx
            Mov eax D$esi+4 | add eax 6

            Call 'USER32.CreateWindowExA'  &WS_EX_LEFT,
                                           StaticClassName,
                                           &NULL,
                                           &WS_CHILD+&WS_VISIBLE+&SS_SIMPLE,
                                           2,
                                           eax,
                                           120,
                                           20,
                                           D$H.DialogEditor,
                                           &NULL,
                                           D$H.Instance,
                                           0

        Pop ebx, esi
        Mov D$TABLE.H.DialogControls+ebx eax
        Push esi, ebx
            Call 'USER32.SendMessageA' eax &WM_SETFONT D$H.MyFont &FALSE
            Call 'USER32.SetWindowTextA' D$TABLE.H.DialogControls+ebx D$esi
        Pop ebx esi

      ; display edit box:
        Push esi, ebx

            Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                          EditClass,
                                          &NULL,
                                          &WS_CHILD+&WS_VISIBLE+&WS_BORDER+&ES_NUMBER+&ES_RIGHT+&ES_MULTILINE,
                                          80,
                                          D$esi+4,
                                          45,
                                          20,
                                          D$H.DialogEditor,
                                          &NULL,
                                          D$H.Instance,
                                          0
        Pop ebx, esi
        Mov D$TABLE.H.DialogControls+ebx+4 eax

      ; display Up and down control:                     +&UDS_ARROWKEYS
        Push esi, ebx
            Call 'Comctl32.CreateUpDownControl',
              &WS_CHILD+&WS_BORDER+&WS_VISIBLE+&UDS_SETBUDDYINT+&UDS_NOTHOUSANDS+&UDS_HORZ,
                      130 D$esi+4 20 20, D$H.DialogEditor, 102, D$H.Instance,
                      eax  2000 0  D$esi+8
        Pop ebx, esi
        Mov D$TABLE.H.DialogControls+ebx+8 eax

      ; Set the speed (repeat speeds) of the UpDown Controls:
        Push esi, eax, ebx
            Call 'USER32.SendMessageA' eax &UDM_SETACCEL 2 UDACCEL
        Pop ebx, eax, esi

        Push esi, ebx
            Call 'USER32.SendMessageA' eax &UDM_SETBUDDY D$TABLE.H.DialogControls+ebx+4 0
        Pop ebx, esi

        add ebx 12 | add esi 12
    Pop ecx

    sub ecx 1 | jnz L0<<
ret

____________________________________________________________________________________

[ActualFontName: B$ ? # 20]
[ActualFontSize: D$ ? ?]

; +ES_MULTILINE because it work better at message flow point of view.
; (used as mono-line, of course). Without this, it sends a EN_CHANGE message
; at first display.

ShowTitleControl:

    Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  EditClass,
                                  0,
                                  &WS_CHILD+&WS_VISIBLE+&WS_BORDER+&ES_AUTOHSCROLL+&ES_MULTILINE,
                                  2,
                                  200,
                                  145,
                                  20,
                                  D$H.DialogEditor,
                                  &NULL,
                                  D$H.Instance,
                                  0

    Mov D$TABLE.H.DialogControls eax

  ; Copy data title (without quotes and comments) in TitleEditText:
    Call SearchDialogLine | inc edi | Mov esi edi
    Mov al '"', ecx 200, ebx 200 | repne scasb                      ; search "Text lenght"
    sub ebx ecx | xchg ecx ebx | dec ecx
    Mov edi ActualFontName | rep movsb | Mov al 0 | stosb           ; copy + end mark

    Call 'USER32.GetDlgCtrlID' D$TABLE.H.DialogControls
    Call 'USER32.SetDlgItemTextA' D$H.DialogEditor eax ActualFontName  ; show edition text
    Call 'USER32.SetFocus' D$TABLE.H.DialogControls
    Call 'USER32.SendMessageA' D$TABLE.H.DialogControls  &EM_SETSEL  0  0-1
ret

[ComboClass: B$ 'COMBOBOX' EOS]  ; EditClass


; table of fonts names for dialog fonts:

[DialogFonts:
 B$ 'Arial' EOS
 B$ 'Arial Black' EOS
 B$ 'Comic Sans MS' EOS
 B$ 'Courier' EOS
 B$ 'Courier New' EOS
 B$ 'Fixedsys' EOS
 B$ 'Helv' EOS
 B$ 'Impact' EOS
 B$ 'Marlett' EOS
 B$ 'Modern' EOS
 B$ 'MS Sans Serif' EOS
 B$ 'MS Serif' EOS
 B$ 'Small Fonts' EOS
 B$ 'Symbol' EOS
 B$ 'System' EOS
 B$ 'Terminal' EOS
 B$ 'Times New Roman' EOS
 B$ 'Verdana' EOS
 B$ 'Webdings' EOS
 B$ 'Wingdings' EOS
 0]

; Differents set of fonts sizes available for upper fonts list:

[T1F:
 B$ '08' EOS
 B$ '09' EOS
 B$ '0A' EOS
 B$ '0B' EOS
 B$ '0C' EOS
 B$ '0E' EOS
 B$ '10' EOS
 B$ '12' EOS
 B$ '14' EOS
 B$ '16' EOS
 B$ '18' EOS
 B$ '1A' EOS
 B$ '1C' EOS
 B$ '24' EOS
 B$ '30' EOS
 B$ '48' EOS
 B$ 0]

[T2F:
 B$ '0A' EOS
 B$ '0C' EOS
 B$ '0F' EOS
 B$ 0]

[T3F:
 B$ '09' EOS
 B$ 0]

[T4F:
 B$ '08' EOS
 B$ '0A' EOS
 B$ '0C' EOS
 B$ '0E' EOS
 B$ '12' EOS
 B$ '18' EOS
 B$ 0]

[T5F:
 B$ '06' EOS
 B$ '07' EOS
 B$ '08' EOS
 B$ '0A' EOS
 B$ '0C' EOS
 B$ '0E' EOS
 B$ '12' EOS
 B$ '18' EOS
 B$ 0]

[T6F:
 B$ '02' EOS
 B$ '03' EOS
 B$ '04' EOS
 B$ '05' EOS
 B$ '06' EOS
 B$ '07' EOS
 B$ 0]

[T7F:
 B$ '0A' EOS
 0]

; Table of pointers: font index of upper names >>> pointer to upper sizes table:

[FontSizesTable:
 D$ T1F
 D$ T1F
 D$ T1F
 D$ T2F
 D$ T1F
 D$ T3F
 D$ T4F
 D$ T1F
 D$ T1F
 D$ T1F
 D$ T4F
 D$ T5F
 D$ T6F
 D$ T7F
 D$ T3F
 D$ T1F
 D$ T1F
 D$ T1F
 D$ T1F]

; Search for the actual font in font name list to retrieve the index for sizes table:

SearchFontIndex:
    Mov edi DialogFonts, edx 0-4

    While B$edi > 0
        Mov esi ActualFontName | add edx 4
        Push edi
            Mov ecx 200, ebx 200, al 0 | repne scasb | sub ebx ecx | dec ebx
        Pop edi
        If ebx = D$ComboFontNameLenght
            Mov esi edi, ecx ebx | repe cmpsb | je L9>
                Mov edi esi | add edi ebx | inc edi
        Else
            add edi ebx | inc edi
        End_If
    End_While
    Mov eax 0 | ret

L9: Mov eax edx | ret


[FontComboID: D$ ?
 ComboFontNameLenght: D$ ?]

ShowFontControls:
  ; Font Type edition combo box:

    Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  ComboClass,
                                  &NULL,
                                  &WS_CHILD+&WS_VISIBLE+&WS_BORDER+&CBS_HASSTRINGS+&CBS_AUTOHSCROLL+&CBS_DROPDOWNLIST+&WS_VSCROLL+&ES_AUTOVSCROLL,
                                  2,
                                  240,
                                  145,
                                  220,
                                  D$H.DialogEditor,
                                  &NULL,
                                  D$H.Instance,
                                  0

    Mov D$TABLE.H.DialogControls eax

  ; Copy data font text (without quotes and comments) in TitleEditText:
    Call SearchDialogLine
    Mov esi edi, edi ActualFontSize
    while B$esi > SPC
        movsb
    End_While
    Mov al 0 | stosb
    add esi 2 | Mov edi esi
    Mov al '"', ecx 200, ebx 200 | repne scasb                      ; search "Text lenght"
    sub ebx ecx | xchg ecx ebx | dec ecx
    Mov D$ComboFontNameLenght ecx
    Mov edi ActualFontName | rep movsb | Mov al 0 | stosb           ; copy + end mark

    Mov edi DialogFonts
    While B$edi > 0
        Push edi
            Call 'USER32.SendMessageA' D$TABLE.H.DialogControls &CB_ADDSTRING 0  edi
        Pop edi
        Mov al 0, ecx 200 | repne scasb
    End_While

    Call 'USER32.SendMessageA' D$TABLE.H.DialogControls &CB_SELECTSTRING  0  ActualFontName

  ; Font size edition box:

    Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  ComboClass,
                                  0,
                                  &WS_CHILD+&WS_VISIBLE+&WS_BORDER+&CBS_HASSTRINGS+&CBS_AUTOHSCROLL+&CBS_DROPDOWNLIST+&WS_VSCROLL+&ES_AUTOVSCROLL,
                                  100,
                                  200,
                                  47,
                                  150,
                                  D$H.DialogEditor,
                                  &NULL,
                                  D$H.Instance,
                                  0

    Mov D$TABLE.H.DialogControls+4 eax

    Call SearchFontIndex

    If eax > 0
        Mov edi D$FontSizesTable+eax
        While B$edi > 0
            Push edi
                Call 'USER32.SendMessageA' D$TABLE.H.DialogControls+4 &CB_ADDSTRING 0  edi
            Pop edi
            Mov al 0, ecx 200 | repne scasb
        End_While
    End_If

    Call 'USER32.SendMessageA' D$TABLE.H.DialogControls+4  &CB_SELECTSTRING  0  ActualFontSize
ret

________________________________________________________________________________________
________________________________________________________________________________________
;;
 All the controls specificaly used for one record edition have these data tables
 associated with: (one for equates), one for texts proposed as different choices, one
 for the bit values of each possible choice, one for the bit values that can not be
 fitting with a choice and one for the bits that must be associated with one choice.

 Some win equates are not bit values but ordinals values (1, 2, 3, ..., 0100, 0200, 0300,...)
 in one same set of equates. This is to say that, having the binary value for some 'Style',
 it is impossible to find the signification by simply analysing each bit of 'Style' value.
 So must we have an additionnal set of mask to be able to Check / UnCheck the check
 boxes, either at first screen show or after user modifications, according with
 exclusions and 'must have' rules. These additionnal tables are named 'xxxxCheckingMask'.

 "xxxxExcludeBitTable" is used by 'WriteStyle' to compute the style value (only one value
 per operation)
 "xxxxCheckingMask" is used by 'CheckControlStyles' to set the check boxes (all the set
 per operation).
;;
________________________________________________________________________________________
________________________________________________________________________________________
; Button tables:


[ButtonCheckingMask:
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F0
 D$ &BS_CENTER
 D$ &BS_CENTER
 D$ &BS_CENTER
 D$ &BS_VCENTER
 D$ &BS_VCENTER
 D$ &BS_VCENTER
 D$ 0F
 D$ 0F0
 D$ 0F0
 D$ &BS_MULTILINE
 D$ &BS_NOTIFY
 D$ &BS_PUSHLIKE
 D$ &BS_FLAT
 D$ 010000
 D$ 020000
 D$ 08000000
 D$ &WS_CHILD
 D$ &WS_VISIBLE
 D$ &WS_BORDER
 D$ &WS_CLIPSIBLINGS]

[ButtonExcludeBitTable:
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0
 D$ &BS_CENTER
 D$ &BS_CENTER
 D$ &BS_CENTER
 D$ &BS_VCENTER
 D$ &BS_VCENTER
 D$ &BS_VCENTER
 D$ 0F
 D$ 0F0
 D$ 0F0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[ButtonTextTable:
 B$ 'BS_PUSHBUTTON' EOS
 B$ 'BS_DEFPUSHBUTTON' EOS
 B$ 'BS_CHECKBOX' EOS
 B$ 'BS_3STATE' EOS
 B$ 'BS_AUTOCHECKBOX' EOS
 B$ 'BS_AUTO3STATE' EOS
 B$ 'BS_RADIOBUTTON' EOS
 B$ 'BS_AUTORADIOBUTTON' EOS
 B$ 'BS_GROUPBOX' EOS
 B$ 'BS_LEFTTEXT' EOS
 B$ 'BS_LEFT' EOS
 B$ 'BS_RIGHT' EOS
 B$ 'BS_CENTER' EOS
 B$ 'BS_BOTTOM' EOS
 B$ 'BS_TOP' EOS
 B$ 'BS_VCENTER' EOS
 B$ 'BS_OWNERDRAW' EOS
 B$ 'BS_BITMAP' EOS
 B$ 'BS_ICON' EOS
 B$ 'BS_MULTILINE' EOS
 B$ 'BS_NOTIFY' EOS
 B$ 'BS_PUSHLIKE' EOS
 B$ 'BS_FLAT' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 'WS_GROUP' EOS
 B$ 'WS_DISABLED' EOS
 B$ 'WS_CHILD' EOS
 B$ 'WS_VISIBLE' EOS
 B$ 'WS_BORDER' EOS
 B$ 'WS_CLIPSIBLINGS' EOS
 B$ 0]

[ButtonBitTable:
 D$ &BS_PUSHBUTTON
 D$ &BS_DEFPUSHBUTTON
 D$ &BS_CHECKBOX
 D$ &BS_3STATE
 D$ &BS_AUTOCHECKBOX
 D$ &BS_AUTO3STATE
 D$ &BS_RADIOBUTTON
 D$ &BS_AUTORADIOBUTTON
 D$ &BS_GROUPBOX
 D$ &BS_LEFTTEXT
 D$ &BS_LEFT
 D$ &BS_RIGHT
 D$ &BS_CENTER
 D$ &BS_BOTTOM
 D$ &BS_TOP
 D$ &BS_VCENTER
 D$ &BS_OWNERDRAW
 D$ &BS_BITMAP
 D$ &BS_ICON
 D$ &BS_MULTILINE
 D$ &BS_NOTIFY
 D$ &BS_PUSHLIKE
 D$ &BS_FLAT
 D$ &WS_TABSTOP
 D$ &WS_GROUP
 D$ &WS_DISABLED
 D$ &WS_CHILD
 D$ &WS_VISIBLE
 D$ &WS_BORDER
 D$ &WS_CLIPSIBLINGS] ; &BS_RIGHTBUTTON &BS_TEXT

[ButtonMustHaveBitTable: D$ ? # 30]

[BS_PUSHBUTTONhelp: B$ "BS_PUSHBUTTON:

The button posts a WM_COMMAND message to its parent when pushed." EOS]

[BS_DEFPUSHBUTTONhelp: B$ "BS_DEFPUSHBUTTON:

The button behaves like with BS_PUSHBUTTON, but has a enhanced border and reacts as default OK, when user hits Return." EOS]

[BS_CHECKBOXhelp: B$ "BS_CHECKBOX: 

Empty check box with text.

The default text Pos (at right) may be changed with BS_LEFTTEXT." EOS]

[BS_3STATEhelp: B$ "BS_3STATE:

Same as check box, but it can be grayed, checked or unchecked." EOS]

[BS_AUTOCHECKBOXhelp: B$ "BS_AUTOCHECKBOX:

Same as check box, but the state automatically toggles on user action." EOS]

[BS_AUTO3STATEhelp: B$ "BS_AUTO3STATE:

Same as Three-state check box, but the state automatically toggles on user action." EOS]

[BS_RADIOBUTTONhelp: B$ "BS_RADIOBUTTON:

Small circle with text. The default text Pos (at right) may be changed with BS_LEFTTEXT." EOS]

[BS_AUTORADIOBUTTONhelp: B$ "BS_AUTORADIOBUTTON:

Same as a radio button, but the state automatically toggles on user action." EOS]

[BS_GROUPBOXhelp: B$ "BS_GROUPBOX:

Not really a button. Creates a rectangle in which other controls can be grouped, with eventually a common title.

If you want, for example to have several sets of Auto-Radio-Buttons, you may also need the WS_GROUP style" EOS]

[BS_LEFTTEXThelp: B$ "BS_LEFTTEXT:

Places text on the left side of the radio button or check box." EOS]

[BS_LEFThelp: B$ "BS_LEFT:

Left-justified the text in the button rectangle.

Note: If the button is a check box or radio button without BS_RIGHTBUTTON, the text is left justified on the right side of the control." EOS]

[BS_RIGHThelp: B$ "BS_RIGHT:

Right-justified text in the button rectangle.

Note: If the button is a check box or radio button without BS_RIGHTBUTTON, the text is right justified on the right side of the control." EOS]

[BS_CENTERhelp: B$ "BS_CENTER:

Centers the text horizontally in the button rectangle." EOS]

[BS_BOTTOMhelp: B$ "BS_BOTTOM:

Places the text at the bottom of the button rectangle." EOS]

[BS_TOPhelp: B$ "BS_TOP:

Places the text at the top of the button rectangle." EOS]

[BS_VCENTERhelp: B$ "BS_VCENTER:

Centers vertically the text on the button rectangle." EOS]

[BS_OWNERDRAWhelp: B$ "BS_OWNERDRAW:

The owner window will receives a WM_MEASUREITEM message at creation time and a WM_DRAWITEM message after the visual aspect change is done.

Can't be combined with any other button styles." EOS]

[BS_BITMAPhelp: B$ "BS_BITMAP:

The button will display a bitmap. You set it in your Proc." EOS]

[BS_ICONhelp: B$ "BS_ICON:

The button will display an Icon. You set it in your Proc." EOS]

[BS_MULTILINEhelp: B$ "BS_MULTILINE:

Allows multiple lines if the text string is too long to fit with the button width." EOS]

[BS_NOTIFYhelp: B$ "BS_NOTIFY:

The button will send BN_DBLCLK / BN_KILLFOCUS / BN_SETFOCUS notification messages to its parent. 

Note: BN_CLICKED is always send, even without this style. " EOS]

[BS_PUSHLIKEhelp: B$ "BS_PUSHLIKE:

Makes a check box or a radio button look like a button, with a state holding behaviour." EOS]

[BS_FLAThelp: B$ "BS_FLAT:

The button borders are visible only when activated" EOS]

[WS_GROUPhelp: B$ "WS_GROUP:

To be used with BS_GROUPBOX.

The next coming Controls, for example, a serie of Radio Buttons will behave as a group" EOS]

[ButtonStylesHelp:
 D$ BS_PUSHBUTTONhelp
 D$ BS_DEFPUSHBUTTONhelp
 D$ BS_CHECKBOXhelp
 D$ BS_3STATEhelp
 D$ BS_AUTOCHECKBOXhelp
 D$ BS_AUTO3STATEhelp
 D$ BS_RADIOBUTTONhelp
 D$ BS_AUTORADIOBUTTONhelp
 D$ BS_GROUPBOXhelp
 D$ BS_LEFTTEXThelp
 D$ BS_LEFThelp
 D$ BS_RIGHThelp
 D$ BS_CENTERhelp
 D$ BS_BOTTOMhelp
 D$ BS_TOPhelp
 D$ BS_VCENTERhelp
 D$ BS_OWNERDRAWhelp
 D$ BS_BITMAPhelp
 D$ BS_ICONhelp
 D$ BS_MULTILINEhelp
 D$ BS_NOTIFYhelp
 D$ BS_PUSHLIKEhelp
 D$ BS_FLAThelp
 D$ WS_TABSTOPhelp
 D$ WS_GROUPhelp
 D$ WS_DISABLEDhelp
 D$ WS_CHILDhelp
 D$ WS_VISIBLEhelp
 D$ WS_BORDERhelp
 D$ WS_CLIPSIBLINGShelp]
____________________________________

; Edit control tables:

[EditCheckingMask:
 D$ 03
 D$ 03
 D$ 03
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[EditTextTable:
 B$ 'ES_LEFT' EOS
 B$ 'ES_CENTER' EOS
 B$ 'ES_RIGHT' EOS
 B$ 'ES_MULTILINE' EOS
 B$ 'ES_AUTOVSCROLL' EOS
 B$ 'ES_AUTOHSCROLL' EOS
 B$ 'ES_LOWERCASE' EOS
 B$ 'ES_UPPERCASE' EOS
 B$ 'ES_PASSWORD' EOS
 B$ 'ES_OEMCONVERT' EOS
 B$ 'ES_NOHIDESEL' EOS
 B$ 'ES_READONLY' EOS
 B$ 'ES_NUMBER' EOS
 B$ 'ES_WANTRETURN' EOS
 B$ 'WS_VSCROLL' EOS
 B$ 'WS_HSCROLL' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 'WS_BORDER' EOS
 B$ 0]

[EditBitTable:
 D$ &ES_LEFT
 D$ &ES_CENTER
 D$ &ES_RIGHT
 D$ &ES_MULTILINE
 D$ &ES_AUTOVSCROLL
 D$ &ES_AUTOHSCROLL
 D$ &ES_LOWERCASE
 D$ &ES_UPPERCASE
 D$ &ES_PASSWORD
 D$ &ES_OEMCONVERT
 D$ &ES_NOHIDESEL
 D$ &ES_READONLY
 D$ &ES_NUMBER
 D$ &ES_WANTRETURN
 D$ &WS_VSCROLL
 D$ &WS_HSCROLL
 D$ &WS_TABSTOP
 D$ &WS_BORDER]

[EditExcludeBitTable:
 D$ 03
 D$ 02
 D$ 01
 D$ 0
 D$ 0
 D$ 0
 D$ 08
 D$ 010
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[EditMustHaveBitTable:
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ &WS_VSCROLL
 D$ &WS_HSCROLL
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[ES_LEFThelp: B$ "ES_LEFT:

Text will be Left-aligned." EOS]

[ES_CENTERhelp: B$ "ES_CENTER:

For multiline edit control:

Text will be centered" EOS]

[ES_RIGHThelp: B$ "ES_RIGHT:

For multiline edit control:

Text wil be right-aligned." EOS]

[ES_MULTILINEhelp: B$ "ES_MULTILINE:

Multiline edit control. Default is single-line. Whith multi-lines, by default, when pressing ENTER, the default button is activated. To use the ENTER key as a real carriage return, add the ES_WANTRETURN style.

With the ES_AUTOHSCROLL style, the multiline edit control automatically scrolls horizontally when the caret reaches the right edge. To start a new line, the user must press the ENTER key. Without ES_AUTOHSCROLL, the control automatically wraps the words to the beginning of the next line when needed. A new line is also started if the user presses the ENTER key. The word wrap vary with the window size.

With multiline edit you can have scroll bars, and the edit control will process its own scroll bar messages." EOS]

[ES_AUTOVSCROLLhelp: B$ "ES_AUTOVSCROLL:

Automatically scrolls text up or down on user action." EOS]

[ES_AUTOHSCROLLhelp: B$ "ES_AUTOHSCROLL:

Automatically scrolls text to the right by 10 chars when needed (on user action)." EOS]

[ES_LOWERCASEhelp: B$ "ES_LOWERCASE:

Converts all imputs to lowercase." EOS]

[ES_UPPERCASEhelp: B$ "ES_UPPERCASE:

Converts all Inputs to uppercase." EOS]

[ES_PASSWORDhelp: B$ "ES_PASSWORD:

Shows asterisks instead of imputed chars. You can change the (*)s to something Else with the EM_SETPASSWORDCHAR message." EOS]

[ES_OEMCONVERThelp: B$ "ES_OEMCONVERT:

Converts text entered, from the Windows character set to the OEM character set and then back to the Windows set. This ensures proper character conversion when the application calls the CharToOem function to convert a string to OEM. Useful for retrieving filenames." EOS]

[ES_NOHIDESELhelp: B$ "ES_NOHIDESEL:

The selected text will remain selected, even if the control looses the focus. (as opposed to the default behaviour)." EOS]

[ES_READONLYhelp: B$ "ES_READONLY:

User can't modify the actual text." EOS]

[ES_NUMBERhelp: B$ "ES_NUMBER:

Allows only decimal digits inputs." EOS]

[ES_WANTRETURNhelp: B$ "ES_WANTRETURN:

For multi-lines edit: The carriage return be inserted in response to ENTER.

Otherwise, ENTER would be considered as a default Push button actions." EOS]
; WS_VSCROLLhelp: '' EOS
; WS_HSCROLLhelp: '' EOS

[WS_TABSTOPhelp: B$ "WS_TABSTOP:

Pressing the TAB key changes the keyboard focus to the next control having this style. 

The order of Controls is the one in the Dialog Data." EOS]

; WS_BORDERhelp: '' EOS


[EditStylesHelp:
 D$ ES_LEFThelp
 D$ ES_CENTERhelp
 D$ ES_RIGHThelp
 D$ ES_MULTILINEhelp
 D$ ES_AUTOVSCROLLhelp
 D$ ES_AUTOHSCROLLhelp
 D$ ES_LOWERCASEhelp
 D$ ES_UPPERCASEhelp
 D$ ES_PASSWORDhelp
 D$ ES_OEMCONVERThelp
 D$ ES_NOHIDESELhelp
 D$ ES_READONLYhelp
 D$ ES_NUMBERhelp
 D$ ES_WANTRETURNhelp
 D$ WS_VSCROLLhelp
 D$ WS_HSCROLLhelp
 D$ WS_TABSTOPhelp
 D$ WS_BORDERhelp]
____________________________________


; Static Controls tables:

[StaticCheckingMask:
 D$ 0F
 D$ 0FFF
 D$ 0FFF
 D$ 0FFF
 D$ 0FFF
 D$ 0FFF
 D$ 0FFF
 D$ 0FFF
 D$ 0FFF
 D$ 03
 D$ 03
 D$ 03
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[StaticTextTable:
 B$ 'SS_SIMPLE' EOS
 B$ 'SS_BITMAP' EOS
 B$ 'SS_ICON' EOS
 B$ 'SS_BLACKRECT' EOS
 B$ 'SS_GRAYRECT' EOS
 B$ 'SS_WHITERECT' EOS
 B$ 'SS_BLACKFRAME' EOS
 B$ 'SS_GRAYFRAME' EOS
 B$ 'SS_WHITEFRAME' EOS
 B$ 'SS_CENTER' EOS
 B$ 'SS_LEFT' EOS
 B$ 'SS_RIGHT' EOS
 B$ 'SS_CENTERIMAGE' EOS
 B$ 'SS_LEFTNOWORDWRAP' EOS
 B$ 'SS_NOPREFIX' EOS
 B$ 'SS_NOTIFY' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 'WS_BORDER' EOS
 B$ 0]

[StaticBitTable:
 D$ &SS_SIMPLE
 D$ &SS_BITMAP
 D$ &SS_ICON
 D$ &SS_BLACKRECT
 D$ &SS_GRAYRECT
 D$ &SS_WHITERECT
 D$ &SS_BLACKFRAME
 D$ &SS_GRAYFRAME
 D$ &SS_WHITEFRAME
 D$ &SS_CENTER
 D$ &SS_LEFT
 D$ &SS_RIGHT
 D$ &SS_CENTERIMAGE
 D$ &SS_LEFTNOWORDWRAP
 D$ &SS_NOPREFIX
 D$ &SS_NOTIFY
 D$ &WS_TABSTOP
 D$ &WS_BORDER] ; &SS_METAPICT &SS_RIGHTIMAGE

[StaticExcludeBitTable:
 D$ 0FFF
 D$ 0FF
 D$ 0FFF
 D$ 0FF
 D$ 0FF
 D$ 0FF
 D$ 0FF
 D$ 0FF
 D$ 0FF
 D$ 03
 D$ 03
 D$ 03
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[StaticMustHaveBitTable: D$ ? # 18]

[SS_SIMPLEhelp: B$ "SS_SIMPLE:

A simple short line of left-aligned text in a rectangle." EOS]

[SS_BITMAPhelp: B$ "SS_BITMAP:

Room where to display a bitmap.

The Name (Title), when given is for targetting a Bitmap Resource save by Name. Actually RosAsm does not assume this way for saving Resources (only Resources by Number).

You have to defined your BitMap at Run Time from your CallBack Initialisation.

The width and Hight of the Control are dummy: The control automatically resizes to accommodate to the bitmap size." EOS]

[SS_ICONhelp: B$ "SS_ICON:

Room where to display an icon.

The Name (Title), when given is for targetting a Bitmap Resource save by Name. Actually RosAsm does not assume this way for saving Resources (only Resources by Number).

You have to defined your BitMap at Run Time from your CallBack Initialisation.

The width and Hight of the Control are dummy: The control automatically resizes to accommodate to the bitmap size" EOS]

[SS_BLACKRECThelp: B$ "SS_BLACKRECT:

A rectangle filled with the current window frame color.

Default is black." EOS]

[SS_GRAYRECThelp: B$ "SS_GRAYRECT:

A rectangle filled with the current screen background color.

Default is gray." EOS]

[SS_WHITERECThelp: B$ "SS_WHITERECT:

A rectangle filled with the current window background color.

Defauilt is white." EOS]

[SS_BLACKFRAMEhelp: B$ "SS_BLACKFRAME:

A box with a frame drawn in the color of window frames.

Default is black." EOS]

[SS_GRAYFRAMEhelp: B$ "SS_GRAYFRAME:

A box with a frame drawn in the color of screen background.

Default is gray." EOS]

[SS_WHITEFRAMEhelp: B$ "SS_WHITEFRAME:

A box with a frame drawn with the color of window backgrounds.

Default is white." EOS]

[SS_CENTERhelp: B$ "SS_CENTER: 

A rectangle with centered text inside.

Includes wordwrap." EOS]

[SS_LEFThelp: B$ "SS_LEFT:

A rectangle with left-aligned text inside.

Includes wordwrap." EOS]

[SS_RIGHThelp: B$ "SS_RIGHT:

A rectangle with right-aligned text inside.

Includes wordwrap." EOS]

[SS_CENTERIMAGEhelp: B$ "SS_CENTERIMAGE:

With SS_BITMAP or SS_ICON, when resizing to accomodate the image size, the control pos will refer to its center instead of its upper rigth corner." EOS]

[SS_LEFTNOWORDWRAPhelp: B$ "SS_LEFTNOWORDWRAP:

Clips past extended text instead of wrapping when too long" EOS]

[SS_NOPREFIXhelp: B$ "SS_NOPREFIX:

By default ampersand (&) indicates the next Char as a hot key (accelerator). This style negates this behaviour.

You can do the same by simply stating 2 ampersands (&&) instead." EOS]

[SS_NOTIFYhelp: B$ "SS_NOTIFY:

Sends STN_CLICKED and STN_DBLCLK notification messages to the parent when the user clicks or double clicks the control." EOS]
 ;WS_BORDERhelp: '' EOS
 ;
[StaticStylesHelp:
 D$ SS_SIMPLEhelp
 D$ SS_BITMAPhelp
 D$ SS_ICONhelp
 D$ SS_BLACKRECThelp
 D$ SS_GRAYRECThelp
 D$ SS_WHITERECThelp
 D$ SS_BLACKFRAMEhelp
 D$ SS_GRAYFRAMEhelp
 D$ SS_WHITEFRAMEhelp
 D$ SS_CENTERhelp
 D$ SS_LEFThelp
 D$ SS_RIGHThelp
 D$ SS_CENTERIMAGEhelp
 D$ SS_LEFTNOWORDWRAPhelp
 D$ SS_NOPREFIXhelp
 D$ SS_NOTIFYhelp
 D$ WS_TABSTOPhelp
 D$ WS_BORDERhelp]
____________________________________


; List box tables:

[ListCheckingMask:
 D$ 0
 D$ 0
 D$ 0
 D$ 0_FF_FFFF
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 030
 D$ 030
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[ListTextTable:
 B$ 'LBS_HASSTRINGS' EOS
 B$ 'LBS_NOTIFY' EOS
 B$ 'LBS_SORT' EOS
 B$ 'LBS_STANDARD' EOS
 B$ 'LBS_USETABSTOPS' EOS
 B$ 'LBS_WANTKEYBOARDINPUT' EOS
 B$ 'LBS_DISABLENOSCROLL' EOS
 B$ 'LBS_EXTENDEDSEL' EOS
 B$ 'LBS_MULTICOLUMN' EOS
 B$ 'LBS_MULTIPLESEL' EOS
 B$ 'LBS_NODATA' EOS
 B$ 'LBS_NOINTEGRALHEIGHT' EOS
 B$ 'LBS_NOSEL' EOS
 B$ 'LBS_NOREDRAW' EOS
 B$ 'LBS_OWNERDRAWFIXED' EOS
 B$ 'LBS_OWNERDRAWVARIABLE' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 'WS_BORDER' EOS
 B$ 'WS_VSCROLL' EOS
 B$ 'WS_HSCROLL' EOS
 B$ 0]

[ListBitTable:
 D$ &LBS_HASSTRINGS
 D$ &LBS_NOTIFY
 D$ &LBS_SORT
 D$ &LBS_STANDARD
 D$ &LBS_USETABSTOPS
 D$ &LBS_WANTKEYBOARDINPUT
 D$ &LBS_DISABLENOSCROLL
 D$ &LBS_EXTENDEDSEL
 D$ &LBS_MULTICOLUMN
 D$ &LBS_MULTIPLESEL
 D$ &LBS_NODATA
 D$ &LBS_NOINTEGRALHEIGHT
 D$ &LBS_NOSEL
 D$ &LBS_NOREDRAW
 D$ &LBS_OWNERDRAWFIXED
 D$ &LBS_OWNERDRAWVARIABLE
 D$ &WS_TABSTOP
 D$ &WS_BORDER
 D$ &WS_VSCROLL
 D$ &WS_HSCROLL]

[ListExcludeBitTable:
 D$ 02000
 D$ 0
 D$ 0
 D$ 0FF_FFFF
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 042
 D$ 0
 D$ 0808
 D$ 0
 D$ 030
 D$ 030
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[ListMustHaveBitTable: D$ ? # 20]

[LBS_HASSTRINGShelp: B$ "LBS_HASSTRINGS:

List of strings." EOS]

[LBS_NOTIFYhelp: B$ "LBS_NOTIFY:

Notifies the parent, when the user clicks or double-clicks on a listed item." EOS]

[LBS_SORThelp: B$ "LBS_SORT:

Auto-Sorts the strings in alphabetic order." EOS]

[LBS_STANDARDhelp: B$ "LBS_STANDARD:

Auto-Sorts the strings in alphabetic order.

Notify parent of user cicks and double-clicks.

Plus side borders" EOS]

[LBS_USETABSTOPShelp: B$ "LBS_USETABSTOPS:

Tab are drawn with the string, if any." EOS]

[LBS_WANTKEYBOARDINPUThelp: B$ "LBS_WANTKEYBOARDINPUT:

Send the owner of the box a WM_VKEYTOITEM messages when user depresses a key.

Enables an application to control keyboard inputs." EOS]

[LBS_DISABLENOSCROLLhelp: B$ "LBS_DISABLENOSCROLL:

When the vertical scroll bar is not required for viewing all of the ListBox Items, this style negates the default behaviour, which is to *not* display the scroll bar." EOS]

[LBS_EXTENDEDSELhelp: B$ "LBS_EXTENDEDSEL:

Allows multiple items selections by use of the SHIFT key or of the mouse." EOS]

[LBS_MULTICOLUMNhelp: B$ "LBS_MULTICOLUMN:

Multicolumn list box, scrolled horizontally.

You set the columns width with LB_SETCOLUMNWIDTH message." EOS]

[LBS_MULTIPLESELhelp: B$ "LBS_MULTIPLESEL:

Turns string selection on or off at each clicks on a string. 

The user can select several strings." EOS]

[LBS_NODATAhelp: B$ "LBS_NODATA:

Does not work with LBS_SORT or LBS_HASSTRINGS style. A no-data list box is to be used when the number of items is over 1000. Must have the LBS_OWNERDRAWFIXED style. 

See Win Help for more information. You should not use this." EOS]

[LBS_NOINTEGRALHEIGHThelp: B$ "LBS_NOINTEGRALHEIGHT:

Block the adaption of the list box size on the Items length." EOS]

[LBS_NOSELhelp: B$ "LBS_NOSEL:
 
 The Items viewed in list box cannot be selected" EOS]

[LBS_NOREDRAWhelp: B$ "LBS_NOREDRAW:

Prevents from updating when changes are made. May be changed with WM_SETREDRAW message." EOS]

[LBS_OWNERDRAWFIXEDhelp: B$ "LBS_OWNERDRAWFIXED:

The items are fixed height.

The owner of the list box redraws its contents when it receives the WM_MEASUREITEM (on creation) or WM_DRAWITEM (on change) messages." EOS]

[LBS_OWNERDRAWVARIABLEhelp: B$ "LBS_OWNERDRAWVARIABLE:

Items are variable heights.

The owner of the list box redraws its contents when it receives the WM_MEASUREITEM (on creation) or WM_DRAWITEM (on change) messages." EOS]
; WS_BORDERhelp: SPC EOS
; WS_VSCROLLhelp: SPC EOS
; WS_HSCROLLhelp: SPC EOS

[ListBoxStyles:
 D$ LBS_HASSTRINGShelp
 D$ LBS_NOTIFYhelp
 D$ LBS_SORThelp
 D$ LBS_STANDARDhelp
 D$ LBS_USETABSTOPShelp
 D$ LBS_WANTKEYBOARDINPUThelp
 D$ LBS_DISABLENOSCROLLhelp
 D$ LBS_EXTENDEDSELhelp
 D$ LBS_MULTICOLUMNhelp
 D$ LBS_MULTIPLESELhelp
 D$ LBS_NODATAhelp
 D$ LBS_NOINTEGRALHEIGHThelp
 D$ LBS_NOSELhelp
 D$ LBS_NOREDRAWhelp
 D$ LBS_OWNERDRAWFIXEDhelp
 D$ LBS_OWNERDRAWVARIABLEhelp
 D$ WS_TABSTOPhelp
 D$ WS_BORDERhelp
 D$ WS_VSCROLLhelp
 D$ WS_HSCROLLhelp]
____________________________________

; Scroll Controls tables:

[ScrollCheckingMask:
 D$ 01
 D$ 06
 D$ 06
 D$ 01
 D$ 06
 D$ 06
 D$ 010
 D$ 08
 D$ 06
 D$ 06
 D$ 0]

[ScrollTextTable:
 B$ 'SBS_HORZ' EOS
 B$ 'SBS_TOPALIGN' EOS
 B$ 'SBS_BOTTOMALIGN' EOS
 B$ 'SBS_VERT' EOS
 B$ 'SBS_LEFTALIGN' EOS
 B$ 'SBS_RIGHTALIGN' EOS
 B$ 'SBS_SIZEBOX' EOS
 B$ 'SBS_SIZEGRIP' EOS
 B$ 'SBS_SIZEBOXBOTTOMRIGHTALIGN' EOS
 B$ 'SBS_SIZEBOXTOPLEFTALIGN' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 0]

[ScrollBitTable:
 D$ &SBS_HORZ
 D$ &SBS_TOPALIGN
 D$ &SBS_BOTTOMALIGN
 D$ &SBS_VERT
 D$ &SBS_LEFTALIGN
 D$ &SBS_RIGHTALIGN
 D$ &SBS_SIZEBOX
 D$ &SBS_SIZEGRIP
 D$ &SBS_SIZEBOXBOTTOMRIGHTALIGN
 D$ &SBS_SIZEBOXTOPLEFTALIGN
 D$ &WS_TABSTOP]

[ScrollExcludeBitTable:
 D$ 019
 D$ 019
 D$ 019
 D$ 018
 D$ 018
 D$ 018
 D$ 010
 D$ 08
 D$ 0
 D$ 0
 D$ 0]

[ScrollMustHaveBitTable:
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 1
 D$ 1
 D$ 0
 D$ 0
 D$ 8
 D$ 8
 D$ 0]

[SBS_HORZhelp: B$ "SBS_HORZ:

The scroll bar will be horizontal." EOS]

[SBS_TOPALIGNhelp: B$ "SBS_TOPALIGN:

To be used with SBS_HORZ, to align the scroll bar top edge with the bounding rectangle." EOS]

[SBS_BOTTOMALIGNhelp: B$ "SBS_BOTTOMALIGN:

To be used with SBS_HORZ, to align the scroll bar bottom edge with the bounding rectangle." EOS]

[SBS_VERThelp: B$ "SBS_VERT:

The scroll bar will be vertical." EOS]

[SBS_LEFTALIGNhelp: B$ "SBS_LEFTALIGN:

To be used with SBS_VERT, to align the scroll bar left edge with the bounding rectangle." EOS]

[SBS_RIGHTALIGNhelp: B$ "SBS_RIGHTALIGN:

To be used with SBS_VERT, to align the scroll bar right edge with the bounding rectangle." EOS]

[SBS_SIZEBOXhelp: B$ "SBS_SIZEBOX:

Features the scroll bar with a size box style.

Does not seem to work, at least under 95.

See SBS_SIZEGRIP." EOS]

[SBS_SIZEGRIPhelp: B$ "SBS_SIZEGRIP:

Adds a resize bitmap left bottom corner of the scroll bar." EOS]

[SBS_SIZEBOXBOTTOMRIGHTALIGNhelp: B$ "SBS_SIZEBOXBOTTOMRIGHTALIGN:

to be used with SBS_SIZEBOX.

Like SBS_SIZEBOX, does not seem to work." EOS]

[SBS_SIZEBOXTOPLEFTALIGNhelp: B$ "SBS_SIZEBOXBOTTOMRIGHTALIGN:

To be used with SBS_SIZEBOX.

Like SBS_SIZEBOX, does not seem to work." EOS]

[ScrollStylesHelp:
 D$ SBS_HORZhelp
 D$ SBS_TOPALIGNhelp
 D$ SBS_BOTTOMALIGNhelp
 D$ SBS_VERThelp
 D$ SBS_LEFTALIGNhelp
 D$ SBS_RIGHTALIGNhelp
 D$ SBS_SIZEBOXhelp
 D$ SBS_SIZEGRIPhelp
 D$ SBS_SIZEBOXBOTTOMRIGHTALIGNhelp
 D$ SBS_SIZEBOXTOPLEFTALIGNhelp
 D$ WS_TABSTOPhelp]
____________________________________

; Combo Box tables:

[ComboCheckingMask:
 D$ &CBS_DROPDOWNLIST
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0F000
 D$ 0F000
 D$ &CBS_DROPDOWNLIST
 D$ &CBS_DROPDOWNLIST
 D$ 0
 D$ 0
 D$ 0F0
 D$ 0F0
 D$ 0
 D$ 0
 D$ 0]

[ComboTextTable:
 B$ 'CBS_SIMPLE' EOS
 B$ 'CBS_HASSTRINGS' EOS
 B$ 'CBS_SORT' EOS
 B$ 'CBS_AUTOHSCROLL' EOS
 B$ 'CBS_DISABLENOSCROLL' EOS
 B$ 'CBS_LOWERCASE' EOS
 B$ 'CBS_UPPERCASE' EOS
 B$ 'CBS_DROPDOWN' EOS
 B$ 'CBS_DROPDOWNLIST' EOS
 B$ 'CBS_NOINTEGRALHEIGHT' EOS
 B$ 'CBS_OEMCONVERT' EOS
 B$ 'CBS_OWNERDRAWFIXED' EOS
 B$ 'CBS_OWNERDRAWVARIABLE' EOS
 B$ 'WS_VSCROLL' EOS
 B$ 'WS_HSCROLL' EOS
 B$ 'WS_TABSTOP' EOS 0]

[ComboBitTable:
 D$ &CBS_SIMPLE
 D$ &CBS_HASSTRINGS
 D$ &CBS_SORT
 D$ &CBS_AUTOHSCROLL
 D$ &CBS_DISABLENOSCROLL
 D$ &CBS_LOWERCASE
 D$ &CBS_UPPERCASE
 D$ &CBS_DROPDOWN
 D$ &CBS_DROPDOWNLIST
 D$ &CBS_NOINTEGRALHEIGHT
 D$ &CBS_OEMCONVERT
 D$ &CBS_OWNERDRAWFIXED
 D$ &CBS_OWNERDRAWVARIABLE
 D$ &WS_VSCROLL
 D$ &WS_HSCROLL
 D$ &WS_TABSTOP]

[ComboExcludeBitTable:
 D$ 0FF
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ &CBS_UPPERCASE
 D$ &CBS_LOWERCASE
 D$ &CBS_DROPDOWNLIST
 D$ 0
 D$ 0
 D$ 0
 D$ &CBS_OWNERDRAWVARIABLE
 D$ &CBS_OWNERDRAWFIXED
 D$ 0
 D$ 0
 D$ 0]

[ComboMustHaveBitTable:
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ &CBS_AUTOHSCROLL
 D$ &CBS_HASSTRINGS
 D$ &CBS_HASSTRINGS
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[CBS_SIMPLEhelp: B$ "CBS_SIMPLE:

The list box is always displayed.

The current selection is displayed in the edit control." EOS]

[CBS_HASSTRINGShelp: B$ "CBS_HASSTRINGS:

The combo box items are strings.

The application makes use of CB_GETLBTEXT for retrieving one text item." EOS]

[CBS_SORThelp: B$ "CBS_SORT:

When you will fill the list box, the items will be Automatically sorted." EOS]

[CBS_AUTOHSCROLLhelp: B$ "CBS_AUTOHSCROLL:

In the edit control, the text will scroll Automatically when needed." EOS]

[CBS_DISABLENOSCROLLhelp: B$ "CBS_DISABLENOSCROLL:

Does not hide the scroll bar when it is useless." EOS]

[CBS_LOWERCASEhelp: B$ "CBS_LOWERCASE:

All Chars entered in the edit control will be turned lower case." EOS]

[CBS_UPPERCASEhelp: B$ "CBS_UPPERCASE:

All Chars entered in the edit control will be turned upper case." EOS]

[CBS_DROPDOWNhelp: B$ "CBS_DROPDOWN:

Same as CBS_SIMPLE, but the list box will only be displayed after the user selects the down arrow of the edit control." EOS]

[CBS_DROPDOWNLISThelp: B$ "CBS_DROPDOWNLIST:

Same as CBS_SIMPLE, but the list box will only be displayed after the user selects the down arrow of the edit control, just like with CBS_DROPDOWN, but the Edit Control is replaced by a static control." EOS]

[CBS_NOINTEGRALHEIGHThelp: B$ "CBS_NOINTEGRALHEIGHT:

Blocks automatic resizing depending on the size of items." EOS]

[CBS_OEMCONVERThelp: B$ "CBS_OEMCONVERT:

Converts text entered, from the Windows character set to the OEM character set and then back to the Windows set. This ensures proper character conversion when the application calls the CharToOem function to convert a string to OEM.

Useful for retrieving filenames." EOS]

[CBS_OWNERDRAWFIXEDhelp: B$ "CBS_OWNERDRAWFIXED:

All items are same hight.

The owner of the list box redraws its contents when it receives the WM_MEASUREITEM (on creation) or WM_DRAWITEM (on change) messages." EOS]

[CBS_OWNERDRAWVARIABLEhelp: B$ "CBS_OWNERDRAWVARIABLE:

The items are variable hights.

The owner of the list box redraws its contents when it receives the WM_MEASUREITEM (on creation) or WM_DRAWITEM (on change) messages." EOS]
; WS_VSCROLLhelp:
; WS_HSCROLLhelp:

[ComboStylesHelp:
 D$ CBS_SIMPLEhelp
 D$ CBS_HASSTRINGShelp
 D$ CBS_SORThelp
 D$ CBS_AUTOHSCROLLhelp
 D$ CBS_DISABLENOSCROLLhelp
 D$ CBS_LOWERCASEhelp
 D$ CBS_UPPERCASEhelp
 D$ CBS_DROPDOWNhelp
 D$ CBS_DROPDOWNLISThelp
 D$ CBS_NOINTEGRALHEIGHThelp
 D$ CBS_OEMCONVERThelp
 D$ CBS_OWNERDRAWFIXEDhelp
 D$ CBS_OWNERDRAWVARIABLEhelp
 D$ WS_VSCROLLhelp
 D$ WS_HSCROLLhelp
 D$ WS_TABSTOPhelp]
____________________________________

; UpDown controls tables:

[UpDownCheckingMask: D$ ? # 9]

[UpDownTextTable:
 B$ 'UDS_ALIGNLEFT' EOS
 B$ 'UDS_ALIGNRIGHT' EOS
 B$ 'UDS_SETBUDDYINT' EOS
 B$ 'UDS_NOTHOUSANDS' EOS
 B$ 'UDS_ARROWKEYS' EOS
 B$ 'UDS_HORZ' EOS
 B$ 'UDS_WRAP' EOS
 B$ 'UDS_AUTOBUDDY' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 0]

[UpDownBitTable:
 D$ &UDS_ALIGNLEFT
 D$ &UDS_ALIGNRIGHT
 D$ &UDS_SETBUDDYINT
 D$ &UDS_NOTHOUSANDS
 D$ &UDS_ARROWKEYS
 D$ &UDS_HORZ
 D$ &UDS_WRAP
 D$ &UDS_AUTOBUDDY
 D$ &WS_TABSTOP]

[UpDownExcludeBitTable:
 D$ 4
 D$ 8
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[UpDownMustHaveBitTable: D$ ? # 9]

[UDS_ALIGNLEFThelp: B$ "UDS_ALIGNLEFT:

The up-down control will be at the left edge of the buddy window." EOS]

[UDS_ALIGNRIGHThelp: B$ "UDS_ALIGNRIGHT:

The up-down control will be at the right edge of the buddy window." EOS]

[UDS_SETBUDDYINThelp: B$ "UDS_SETBUDDYINT:

The up-down control will set the text of the buddy window when needed (numbers only).

The buddy window may be set by UDS_AUTOBUDDY." EOS]

[UDS_NOTHOUSANDShelp: B$ "UDS_NOTHOUSANDS:

Reverses default, which is to insert a thousands'separator between each three digits." EOS]

[UDS_ARROWKEYShelp: B$ "UDS_ARROWKEYS:

Allows keyboard UP ARROW and DOWN ARROW keys alternate inputs." EOS]

[UDS_HORZhelp: B$ "UDS_HORZ:

Default is vertical..." EOS]

[UDS_WRAPhelp: B$ "UDS_WRAP:

Restarts at other end, when moving out of range." EOS]

[UDS_AUTOBUDDYhelp: B$ "UDS_AUTOBUDDY:
 
Automatically selcts the previous Z order window as the associated buddy window.
The 'previous Z order Window' is the previous one in the Template." EOS]

[UpDownStylesHelp:
 D$ UDS_ALIGNLEFThelp
 D$ UDS_ALIGNRIGHThelp
 D$ UDS_SETBUDDYINThelp
 D$ UDS_NOTHOUSANDShelp
 D$ UDS_ARROWKEYShelp
 D$ UDS_HORZhelp
 D$ UDS_WRAPhelp
 D$ UDS_AUTOBUDDYhelp
 D$ WS_TABSTOPhelp]
____________________________________

; Progress bar have nothing to edit:

[ProgressTextTable: D$ ? ?]
____________________________________

; Track Bars controls tables:

[TrackCheckingMask: D$ ? # 13]

[TrackTextTable:
 B$ 'TBS_HORZ' EOS
 B$ 'TBS_BOTTOM' EOS
 B$ 'TBS_TOP' EOS
 B$ 'TBS_VERT' EOS
 B$ 'TBS_RIGHT' EOS
 B$ 'TBS_LEFT' EOS
 B$ 'TBS_AUTOTICKS' EOS
 B$ 'TBS_NOTICKS' EOS
 B$ 'TBS_BOTH' EOS
 B$ 'TBS_ENABLESELRANGE' EOS
 B$ 'TBS_FIXEDLENGTH' EOS
 B$ 'TBS_NOTHUMB' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 0]

[TrackBitTable:
 D$ &TBS_HORZ
 D$ &TBS_BOTTOM
 D$ &TBS_TOP
 D$ &TBS_VERT
 D$ &TBS_RIGHT
 D$ &TBS_LEFT
 D$ &TBS_AUTOTICKS
 D$ &TBS_NOTICKS
 D$ &TBS_BOTH
 D$ &TBS_ENABLESELRANGE
 D$ &TBS_FIXEDLENGTH
 D$ &TBS_NOTHUMB
 D$ &WS_TABSTOP]

[TrackExcludeBitTable:
 D$ 2
 D$ 2
 D$ 2
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[TrackMustHaveBitTable:
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 2
 D$ 2
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[TBS_HORZhelp: B$ "TBS_HORZ:

The trackbar will be horizontal." EOS]

[TBS_BOTTOMhelp: B$ "TBS_BOTTOM:

Position of the tick marks in an horizontal trackbar." EOS]

[TBS_TOPhelp: B$ "TBS_TOP:

Position of the tick marks in an horizontal trackbar." EOS]

[TBS_VERThelp: B$ "TBS_VERT:

The trackbar will be vertical." EOS]

[TBS_RIGHThelp: B$ "TBS_RIGHT:

Position of the tick marks in a vertical trackbar." EOS]

[TBS_LEFThelp: B$ "TBS_LEFT:

Position of the tick marks in a vertical trackbar." EOS]

[TBS_AUTOTICKShelp: B$ "TBS_AUTOTICKS:

The tick mark will be added automatically when your application will send the TBM_SETRANGE message.

Otherwise, you can send the TBM_SETTIC and TBM_SETTICFREQ, to specify these positions." EOS]

[TBS_NOTICKShelp: B$ "TBS_NOTICKS:

Does not display the tick marks." EOS]

[TBS_BOTHhelp: B$ "TBS_BOTH:

Displays tick marks on both side of the track bar (left/right with TBS_VERT.Down top with TBS_HORZ)." EOS]

[TBS_ENABLESELRANGEhelp: B$ "TBS_ENABLESELRANGE:

Displays edge ticks as triangles and the selection range is highlighted." EOS]

[TBS_FIXEDLENGTHhelp: B$ "TBS_FIXEDLENGTH:

The slider size will not vary with the size of range, as it does by default." EOS]

[TBS_NOTHUMBhelp: B$ "TBS_NOTHUMB:

Disables the slider." EOS]

[TrackStylesHelp:
 D$ TBS_HORZhelp
 D$ TBS_BOTTOMhelp
 D$ TBS_TOPhelp
 D$ TBS_VERThelp
 D$ TBS_RIGHThelp
 D$ TBS_LEFThelp
 D$ TBS_AUTOTICKShelp
 D$ TBS_NOTICKShelp
 D$ TBS_BOTHhelp
 D$ TBS_ENABLESELRANGEhelp
 D$ TBS_FIXEDLENGTHhelp
 D$ TBS_NOTHUMBhelp
 D$ WS_TABSTOPhelp]
____________________________________

; TreeView Controls tables:

[TreeCheckingMask: D$ ? # 17]

[TreeTextTable:
 B$ 'TVS_HASLINES' EOS
 B$ 'TVS_LINESATROOT' EOS
 B$ 'TVS_HASBUTTONS' EOS
 B$ 'TVS_EDITLABELS' EOS
 B$ 'TVS_SHOWSELALWAYS' EOS
 B$ 'TVS_DISABLEDRAGDROP ' EOS
 B$ 'TVS_RTLREADING ' EOS
 B$ 'TVS_NOTOOLTIPS ' EOS
 B$ 'TVS_CHECKBOXES ' EOS
 B$ 'TVS_TRACKSELECT ' EOS
 B$ 'TVS_SINGLEEXPAND ' EOS
 B$ 'TVS_INFOTIP ' EOS
 B$ 'TVS_FULLROWSELECT ' EOS
 B$ 'TVS_NOSCROLL ' EOS
 B$ 'TVS_NONEVENHEIGHT ' EOS
 B$ 'TVS_NOHSCROLL ' EOS
 B$ 'WS_TABSTOP' EOS
 B$  0]

[TreeBitTable:
 D$ &TVS_HASLINES
 D$ &TVS_LINESATROOT
 D$ &TVS_HASBUTTONS
 D$ &TVS_EDITLABELS
 D$ &TVS_SHOWSELALWAYS
 D$ &TVS_DISABLEDRAGDROP
 D$ &TVS_RTLREADING
 D$ &TVS_NOTOOLTIPS
 D$ &TVS_CHECKBOXES
 D$ &TVS_TRACKSELECT
 D$ &TVS_SINGLEEXPAND
 D$ &TVS_INFOTIP
 D$ &TVS_FULLROWSELECT
 D$ &TVS_NOSCROLL
 D$ &TVS_NONEVENHEIGHT
 D$ &TVS_NOHSCROLL
 D$ &WS_TABSTOP]

[TreeExcludeBitTable:
 D$ &TVS_FULLROWSELECT
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ &TVS_INFOTIP
 D$ 0
 D$ 0
 D$ 0
 D$ &TVS_NOTOOLTIPS
 D$ &TVS_HASLINES
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[TreeMustHaveBitTable: D$ ? # 17]

[TVS_HASLINEShelp: B$ "TVS_HASLINES:

Draws lines showing the items hierarchy." EOS]

[TVS_LINESATROOThelp: B$ "TVS_LINESATROOT:

Draws lines between root and items.

Does not work with TVS_HASLINES." EOS]

[TVS_HASBUTTONShelp: B$ "TVS_HASBUTTONS:

Displays plus and minus buttons aside the parent items.

They are used to open / close the lists of  child items." EOS]

[TVS_EDITLABELShelp: B$ "TVS_EDITLABELS:

Allows edition of the items by user." EOS]

[TVS_SHOWSELALWAYShelp: B$ "TVS_SHOWSELALWAYS:

When lossing the focus, the tree-view will maintain the actual selection." EOS]

[TVS_DISABLEDRAGDROPhelp: B$ "TVS_DISABLEDRAGDROP:

Prevents the tree-view control from sending TVN_BEGINDRAG notification messages. " EOS]

[TVS_RTLREADINGhelp: B$ "TVS_RTLREADING:

Version 4.70. Causes text to be displayed from right-to-left (RTL).
Usually, windows display text left-to-right (LTR). Windows can be mirrored to display languages such
as Hebrew or Arabic that read RTL. Typically, tree-view text is displayed in the same direction as the text
in its parent window. If TVS_RTLREADING is set, tree-view text reads in the opposite direction from the text in
the parent window." EOS]

[TVS_NOTOOLTIPShelp: B$ "TVS_NOTOOLTIPS:

Version 4.70. Disables ToolTips." EOS]

[TVS_CHECKBOXEShelp: B$ "TVS_CHECKBOXES:

Version 4.70. Enables check boxes for items in a tree-view control. A check box is displayed only if an image is
associated with the item. When set to this style, the control effectively uses DrawFrameControl to create and set
a state image list containing two images. State image 1 is the unchecked box and state image 2 is the checked box.
Setting the state image to zero removes the check box altogether.

Version 5.80. Displays a check box even if no image is associated with the item. Once a tree-view control is created
with this style, the style cannot be removed. Instead, you must destroy the control and create a new one in its place.
Destroying the tree-view control does not destroy the check box state image list. You must destroy it explicitly.
Get the handle to the state image list by sending the tree-view control a TVM_GETIMAGELIST message.
Then destroy the image list with ImageList_Destroy.
     
If you want to use this style, you must set the TVS_CHECKBOXES style with SetWindowLong after you create the
treeview control, and before you populate the tree. Otherwise, the checkboxes might appear unchecked, depending
on timing issues." EOS]

[TVS_TRACKSELECThelp: B$ "TVS_TRACKSELECT:

Version 4.70. Enables hot tracking in a tree-view control." EOS]

[TVS_SINGLEEXPANDhelp: B$ "TVS_SINGLEEXPAND:

Version 4.71. Causes the item being selected to expand and the item being unselected to collapse upon selection
in the tree view. If the mouse is used to single-click the selected item and that item is closed, it will be expanded.
If the user holds down the CTRL key while selecting an item, the item being unselected will not be collapsed.

Version 5.80. Causes the item being selected to expand and the item being unselected to collapse upon selection in
the tree view. If the user holds down the CTRL key while selecting an item, the item being unselected will not be
collapsed." EOS]

[TVS_INFOTIPhelp: B$ "TVS_INFOTIP:

Version 4.71. Obtains ToolTip information by sending the TVN_GETINFOTIP notification." EOS]

[TVS_FULLROWSELECThelp: B$ "TVS_FULLROWSELECT:

Version 4.71. Enables full-row selection in the tree view. The entire row of the selected item is highlighted,
and clicking anywhere on an item's row causes it to be selected. This style cannot be used in conjunction with
the TVS_HASLINES style." EOS]

[TVS_NOSCROLLhelp: B$ "TVS_NOSCROLL:

Version 5.80. Disables horizontal scrolling in the control. The control will not display any horizontal scroll bars." EOS]

[TVS_NONEVENHEIGHThelp: B$ "TVS_NONEVENHEIGHT:

Version 4.71 Sets the height of the items to an odd height with the TVM_SETITEMHEIGHT message. By default, the height
of items must be an even value." EOS]

[TVS_NOHSCROLLhelp: B$ "TVS_NOHSCROLL:

Version 5.80. Disables horizontal scrolling in the control. The control will not display any horizontal scroll bars." EOS]

[TreeViewStylesHelp:
 D$ TVS_HASLINEShelp
 D$ TVS_LINESATROOThelp
 D$ TVS_HASBUTTONShelp
 D$ TVS_EDITLABELShelp
 D$ TVS_SHOWSELALWAYShelp
 D$ TVS_DISABLEDRAGDROPhelp
 D$ TVS_RTLREADINGhelp
 D$ TVS_NOTOOLTIPShelp
 D$ TVS_CHECKBOXEShelp
 D$ TVS_TRACKSELECThelp
 D$ TVS_SINGLEEXPANDhelp
 D$ TVS_INFOTIPhelp
 D$ TVS_FULLROWSELECThelp
 D$ TVS_NOSCROLLhelp
 D$ TVS_NONEVENHEIGHThelp
 D$ TVS_NOHSCROLLhelp
 D$ WS_TABSTOPhelp]
____________________________________

; Tabs Controls tables:

[TabCheckingMask: D$ ? # 19]

[TabTextTable:
 B$ 'TCS_SCROLLOPPOSITE' EOS
 B$ 'TCS_RIGHT/TCS_BOTTOM' EOS
 B$ 'TCS_MULTISELECT' EOS
 B$ 'TCS_FLATBUTTONS' EOS
 B$ 'TCS_FORCEICONLEFT' EOS
 B$ 'TCS_FORCELABELLEFT' EOS
 B$ 'TCS_HOTTRACK' EOS
 B$ 'TCS_VERTICAL' EOS
 B$ 'TCS_BUTTONS' EOS
 B$ 'TCS_MULTILINE' EOS
 B$ 'TCS_FIXEDWIDTH' EOS
 B$ 'TCS_RAGGEDRIGHT' EOS
 B$ 'TCS_FOCUSONBUTTONDOWN' EOS
 B$ 'TCS_OWNERDRAWFIXED' EOS
 B$ 'TCS_TOOLTIPS' EOS
 B$ 'TCS_FOCUSNEVER' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 'WS_CHILD' EOS
 B$ 'WS_VISIBLE' EOS
 B$ 0]

[TabBitTable:
 D$ &TCS_SCROLLOPPOSITE
 D$ &TCS_RIGHT
 D$ &TCS_MULTISELECT
 D$ &TCS_FLATBUTTONS
 D$ &TCS_FORCEICONLEFT
 D$ &TCS_FORCELABELLEFT
 D$ &TCS_HOTTRACK
 D$ &TCS_VERTICAL
 D$ &TCS_BUTTONS
 D$ &TCS_MULTILINE
 D$ &TCS_FIXEDWIDTH
 D$ &TCS_RAGGEDRIGHT
 D$ &TCS_FOCUSONBUTTONDOWN
 D$ &TCS_OWNERDRAWFIXED
 D$ &TCS_TOOLTIPS
 D$ &TCS_FOCUSNEVER
 D$ &WS_TABSTOP
 D$ &WS_CHILD
 D$ &WS_VISIBLE]

[TabExcludeBitTable:
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ &TCS_FORCELABELLEFT
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ &TCS_FOCUSNEVER
 D$ 0
 D$ 0
 D$ &TCS_FOCUSONBUTTONDOWN
 D$ 0
 D$ 0
 D$ 0]

[TabMustHaveBitTable:
 D$ 0
 D$ 0
 D$ 0
 D$ &TCS_BUTTONS
 D$ &TCS_FIXEDWIDTH
 D$ &TCS_FIXEDWIDTH+&TCS_FORCEICONLEFT
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ &TCS_FORCELABELLEFT
 D$ 0
 D$ &TCS_BUTTONS
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[TCS_SCROLLOPPOSITEhelp: B$ " TCS_SCROLLOPPOSITE:

Scrolls the Tab Control" EOS]

[TCS_RIGHThelp: B$ "TCS_RIGHT:

Places the Tab control vertically at the right corner of the control. It must be used with TCS_VERTICAL to display the tabs at the right corner.

This equate is the same as TCS_BOTTOM. But if it is used alone, the tab itens are placed horizontally at the bottom of the control." EOS]

[TCS_MULTISELECThelp: B$ "TCS_MULTISELECT:

Enables multi-selection mode of the tab itens" EOS]

[TCS_FLATBUTTONShelp: B$ "TCS_FLATBUTTONS:

The tab itens are in Flat mode" EOS]

[TCS_FORCEICONLEFThelp: B$ "TCS_FORCEICONLEFT:

Only with TCS_FIXEDWIDTH.

Icons will be aligned to the left of each tab." EOS]

[TCS_FORCELABELLEFThelp: B$ "TCS_FORCELABELLEFT:

Only with TCS_FIXEDWIDTH.

Labels will be be aligned to the left of each tab." EOS]

[TCS_HOTTRACKhelp: B$ "TCS_HOTTRACK:

Highlite the Text Label of the Tab." EOS]

[TCS_VERTICALhelp: B$ "TCS_VERTICAL:

Places the Tab itens vertically. By default the itens are places in the left corner of the Control. To display them in the right corner, use this equate with TCS_RIGHT." EOS]

[TCS_BUTTONShelp: B$ "TCS_BUTTONS:

Tabs are shown in the form of buttons." EOS]

[TCS_MULTILINEhelp: B$ "TCS_MULTILINE:

Allows multiple rows of tabs, when needed." EOS]

[TCS_FIXEDWIDTHhelp: B$ "TCS_FIXEDWIDTH:

Cannot be combined with TCS_RIGHTJUSTIFY.

All tabs will be the same width." EOS]

[TCS_RAGGEDRIGHThelp: B$ "TCS_RAGGEDRIGHT:

By default, Tabs are extended to entire fill the with of their control.

TCS_RAGGEDRIGHT prevents from this if unwanted." EOS]

[TCS_FOCUSONBUTTONDOWNhelp: B$ "CS_FOCUSONBUTTONDOWN:

Tabs receive the focus when clicked." EOS]

[TCS_OWNERDRAWFIXEDhelp: B$ "TCS_OWNERDRAWFIXED:

The parent window will draw the tabs." EOS]

[TCS_TOOLTIPShelp: B$ "TCS_TOOLTIPS:

The tabs control will display tooltips.

See Tooltip Controls in Win Help." EOS]

[TCS_FOCUSNEVERhelp: B$ "TCS_FOCUSNEVER:

Tabs never receive the focus (the Dialog does)." EOS]

[CCS_BOTTOMhelp: B$ "CCS_BOTTOM:

Common Controls style:

Inverts the vertical apearance of the control." EOS]


[TabStylesHelp:
 D$ TCS_SCROLLOPPOSITEhelp
 D$ TCS_RIGHThelp
 D$ TCS_MULTISELECThelp
 D$ TCS_FLATBUTTONShelp
 D$ TCS_FORCEICONLEFThelp
 D$ TCS_FORCELABELLEFThelp
 D$ TCS_HOTTRACKhelp
 D$ TCS_VERTICALhelp
 D$ TCS_BUTTONShelp
 D$ TCS_MULTILINEhelp
 D$ TCS_FIXEDWIDTHhelp
 D$ TCS_RAGGEDRIGHThelp
 D$ TCS_FOCUSONBUTTONDOWNhelp
 D$ TCS_OWNERDRAWFIXEDhelp
 D$ TCS_TOOLTIPShelp
 D$ TCS_FOCUSNEVERhelp
 D$ WS_TABSTOPhelp
 D$ WS_CHILDhelp
 D$ WS_VISIBLEhelp]
____________________________________


; ListView Controls tables:

;[ListViewCheckingMask: &LVS_LIST &LVS_LIST &LVS_LIST &LVS_LIST 0
[ListViewCheckingMask:
 D$ 0
 D$ 03
 D$ 03
 D$ 03
 D$ 0
 D$ 0F0
 D$ 0F0
 D$ 0
 D$ 0F000
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 ;0F0 0
 D$ 0
 D$ 0F000
 D$ 0]

[ListViewTextTable:
 B$ 'LVS_ICON' EOS
 B$ 'LVS_SMALLICON' EOS
 B$ 'LVS_LIST' EOS
 B$ 'LVS_REPORT' EOS
 B$ 'LVS_NOCOLUMNHEADER' EOS
 B$ 'LVS_SORTASCENDING' EOS
 B$ 'LVS_SORTDESCENDING' EOS
 B$ 'LVS_NOSORTHEADER' EOS
 B$ 'LVS_AUTOARRANGE' EOS
 B$ 'LVS_OWNERDRAWFIXED' EOS
 B$ 'LVS_OWNERDATA' EOS
 B$ 'LVS_EDITLABELS' EOS
 B$ 'LVS_NOLABELWRAP' EOS
 B$ 'LVS_NOSCROLL' EOS
 B$ 'LVS_SINGLESEL' EOS
 B$ 'LVS_SHOWSELALWAYS' EOS
 B$ 'LVS_SHAREIMAGELISTS' EOS
 B$ 'LVS_ALIGNLEFT' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 0]

[ListViewBitTable:
 D$ &LVS_ICON
 D$ &LVS_SMALLICON
 D$ &LVS_LIST
 D$ &LVS_REPORT
 D$ &LVS_NOCOLUMNHEADER
 D$ &LVS_SORTASCENDING
 D$ &LVS_SORTDESCENDING
 D$ &LVS_NOSORTHEADER
 D$ &LVS_AUTOARRANGE
 D$ &LVS_OWNERDRAWFIXED
 D$ &LVS_OWNERDATA
 D$ &LVS_EDITLABELS
 D$ &LVS_NOLABELWRAP
 D$ &LVS_NOSCROLL
 D$ &LVS_SINGLESEL
 D$ &LVS_SHOWSELALWAYS
 D$ &LVS_SHAREIMAGELISTS
 D$ &LVS_ALIGNLEFT
 D$ &WS_TABSTOP]
; &LVS_ALIGNLEFT &LVS_ALIGNTOP &LVS_AUTOARRANGE &LVS_BUTTON &LVS_EDITLABELS &LVS_NOLABELWRAP
; &LVS_NOSCROLL &LVS_NOSORTHEADER &LVS_OWNERDRAWFIXED &LVS_SHAREIMAGELISTS &LVS_SHOWSELALWAYS
; &LVS_SINGLESEL &LVS_SORTASCENDING &LVS_SORTDESCENDING

;[ListViewExcludeBitTable: &LVS_LIST  &LVS_LIST  &LVS_LIST  &LVS_LIST__&LVS_ALIGNLEFT  0
[ListViewExcludeBitTable:
 D$ &LVS_SMALLICON+&LVS_LIST+&LVS_REPORT
 D$ &LVS_LIST+&LVS_REPORT+&LVS_ICON
 D$ &LVS_SMALLICON+&LVS_REPORT+&LVS_ICON
 D$ &LVS_ICON+&LVS_SMALLICON+&LVS_LIST+&LVS_ALIGNLEFT
 D$ 0
 D$ &LVS_SORTDESCENDING+&LVS_NOSORTHEADER+&LVS_AUTOARRANGE
 D$ &LVS_SORTASCENDING+&LVS_NOSORTHEADER+&LVS_AUTOARRANGE
 D$ &LVS_SORTASCENDING+&LVS_SORTDESCENDING
 D$ &LVS_SORTASCENDING+&LVS_SORTDESCENDING
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ &LVS_REPORT
 D$ 0]

[ListViewMustHaveBitTable: D$ ? # 19]

[LVS_ICONhelp: B$ "LVS_ICON:

Indicates an Icon view." EOS]

[LVS_SMALLICONhelp: B$ "LVS_SMALLICON:

Indicates a small Icon view." EOS]

[LVS_LISThelp: B$ "LVS_LIST:

Simple list view." EOS]

[LVS_REPORThelp: B$ "LVS_REPORT:

Report view: The first column is always left-aligned, and you cannot use LVCFMT_RIGHT to change this." EOS]

[LVS_NOCOLUMNHEADERhelp: B$ "LVS_NOCOLUMNHEADER:

A column header is not displayed (as it would be by default)." EOS]

[LVS_SORTASCENDINGhelp: B$ "LVS_SORTASCENDING:

Sorts the List View in ascendent order accordying to the 1st item of your list view." EOS]

[LVS_SORTDESCENDINGhelp: B$ "LVS_SORTDESCENDING:

Sorts the List View in descendent order." EOS]

[LVS_NOSORTHEADERhelp: B$ "LVS_NOSORTHEADER:

Disables the Sort ordering of the header of the ListView Control." EOS]

[LVS_AUTOARRANGEhelp: B$ "LVS_AUTOARRANGE:

Auto-Organize the List View Control Itens. This is used by default." EOS]

[LVS_OWNERDRAWFIXEDhelp: B$ "LVS_OWNERDRAWFIXED:
The items are fixed height.

The owner of the list box redraws its contents when it receives the WM_MEASUREITEM (on creation) or WM_DRAWITEM (on change) messages." EOS]

[LVS_OWNERDATAhelp: B$ "LVS_OWNERDATA:
The owner of the list box redraws its contents when it receives the WM_MEASUREITEM (on creation) or WM_DRAWITEM (on change) messages." EOS]

[LVS_EDITLABELShelp: B$ "LVS_EDITLABELS:
Allows editing the item Label. To allow edition all you have to do is press the left button of the mouse over a selected item for a couple of seconds to allow the edition mode" EOS]

[LVS_NOLABELWRAPhelp: B$ "LVS_NOLABELWRAP:
Disables the wrapping mode of the item label" EOS]

[LVS_NOSCROLLhelp: B$ "LVS_NOSCROLL:
Disables the scrollbar of the ListView Control" EOS]

[LVS_SINGLESELhelp: B$ "LVS_SINGLESEL:
Allows selection of a row" EOS]

[LVS_SHOWSELALWAYShelp: B$ "LVS_SHOWSELALWAYS:
Always displays the selection mode" EOS]

[LVS_SHAREIMAGELISTShelp: B$ "LVS_SHAREIMAGELISTS:
Allows using images or icons with the ListView Control" EOS]

[ListViewStylesHelp:
 D$ LVS_ICONhelp
 D$ LVS_SMALLICONhelp
 D$ LVS_LISThelp
 D$ LVS_REPORThelp
 D$ LVS_NOCOLUMNHEADERhelp
 D$ LVS_SORTASCENDINGhelp
 D$ LVS_SORTDESCENDINGhelp
 D$ LVS_NOSORTHEADERhelp
 D$ LVS_AUTOARRANGEhelp
 D$ LVS_OWNERDRAWFIXEDhelp
 D$ LVS_OWNERDATAhelp
 D$ LVS_EDITLABELShelp
 D$ LVS_NOLABELWRAPhelp
 D$ LVS_NOSCROLLhelp
 D$ LVS_SINGLESELhelp
 D$ LVS_SHOWSELALWAYShelp
 D$ LVS_SHAREIMAGELISTShelp
 D$ WS_TABSTOPhelp]
____________________________________

; ToolBar Control Tables:

[ToolBarCheckingMask:
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F0
 D$ 0F0
 D$ 0F0
 D$ 0F0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0F
 D$ 0F
 D$ 0F0
 D$ 0F0
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0F
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[ToolBarTextTable:
 B$ 'TBSTYLE_BUTTON' EOS
 B$ 'TBSTYLE_SEP' EOS
 B$ 'TBSTYLE_CHECK' EOS
 B$ 'TBSTYLE_GROUP' EOS
 B$ 'TBSTYLE_CHECKGROUP' EOS
 B$ 'TBSTYLE_DROPDOWN' EOS
 B$ 'TBSTYLE_AUTOSIZE' EOS
 B$ 'TBSTYLE_NOPREFIX' EOS
 B$ 'BTNS_SHOWTEXT' EOS
 B$ 'BTNS_WHOLEDROPDOWN' EOS
 B$ 'TBSTYLE_TOOLTIPS' EOS
 B$ 'TBSTYLE_WRAPABLE' EOS
 B$ 'TBSTYLE_ALTDRAG' EOS
 B$ 'TBSTYLE_FLAT' EOS
 B$ 'TBSTYLE_LIST' EOS
 B$ 'TBSTYLE_CUSTOMERASE' EOS
 B$ 'TBSTYLE_REGISTERDROP' EOS
 B$ 'TBSTYLE_TRANSPARENT' EOS
 B$ 'TBSTYLE_EX_DRAWDDARROWS' EOS
 B$ 'TBSTYLE_EX_MIXEDBUTTONS' EOS
 B$ 'TBSTYLE_EX_HIDECLIPPEDBUTTONS' EOS
 B$ 'TBSTYLE_EX_DOUBLEBUFFER' EOS
 B$ 'CCS_TOP' EOS
 B$ 'CCS_NOMOVEY' EOS
 B$ 'CCS_BOTTOM' EOS
 B$ 'CCS_NORESIZE' EOS
 B$ 'CCS_NOPARENTALIGN' EOS
 B$ 'CCS_ADJUSTABLE' EOS
 B$ 'CCS_NODIVIDER' EOS
 B$ 'CCS_VERT' EOS
 B$ 'CCS_LEFT' EOS
 B$ 'CCS_NOMOVEX' EOS
 B$ 'CCS_RIGHT' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 'WS_GROUP' EOS
 B$ 'WS_DISABLED' EOS
 B$ 'WS_CHILD' EOS
 B$ 'WS_VISIBLE' EOS
 B$ 'WS_BORDER' EOS
 B$ 'WS_CLIPSIBLINGS' EOS
 B$ 0]

[ToolBarBitTable:
 D$ &TBSTYLE_BUTTON
 D$ &TBSTYLE_SEP
 D$ &TBSTYLE_CHECK
 D$ &TBSTYLE_GROUP
 D$ &TBSTYLE_CHECKGROUP
 D$ &TBSTYLE_DROPDOWN
 D$ &TBSTYLE_AUTOSIZE
 D$ &TBSTYLE_NOPREFIX
 D$ &BTNS_SHOWTEXT
 D$ &BTNS_WHOLEDROPDOWN
 D$ &TBSTYLE_TOOLTIPS
 D$ &TBSTYLE_WRAPABLE
 D$ &TBSTYLE_ALTDRAG
 D$ &TBSTYLE_FLAT
 D$ &TBSTYLE_LIST
 D$ &TBSTYLE_CUSTOMERASE
 D$ &TBSTYLE_REGISTERDROP
 D$ &TBSTYLE_TRANSPARENT
 D$ &TBSTYLE_EX_DRAWDDARROWS
 D$ &TBSTYLE_EX_MIXEDBUTTONS
 D$ &TBSTYLE_EX_HIDECLIPPEDBUTTONS
 D$ &TBSTYLE_EX_DOUBLEBUFFER
 D$ &CCS_TOP
 D$ &CCS_NOMOVEY
 D$ &CCS_BOTTOM
 D$ &CCS_NORESIZE
 D$ &CCS_NOPARENTALIGN
 D$ &CCS_ADJUSTABLE
 D$ &CCS_NODIVIDER
 D$ &CCS_VERT
 D$ &CCS_LEFT
 D$ &CCS_NOMOVEX
 D$ &CCS_RIGHT
 D$ &WS_TABSTOP
 D$ &WS_GROUP
 D$ &WS_DISABLED
 D$ &WS_CHILD
 D$ &WS_VISIBLE
 D$ &WS_BORDER
 D$ &WS_CLIPSIBLINGS]

[ToolBarExcludeBitTable:
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0FFFF
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[ToolBarMustHaveBitTable:
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ &TBSTYLE_LIST+&TBSTYLE_EX_MIXEDBUTTONS
 D$ 0
 D$ 0
 D$ 0
 D$ &CCS_ADJUSTABLE
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]


[TBSTYLE_BUTTONhelp: B$ "TBSTYLE_BUTTON:

Default ToolBar Style. Initialize the Control as a Button." EOS]

[CCS_TOPhelp: B$ "CCS_TOP:

Common Controls style:

Inverts the Horizontal appearance of the control." EOS]

[TBSTYLE_CHECKhelp: B$ "TBSTYLE_CHECK:

Allows using a CheckBox on the ToolBar." EOS]
;;
CCS_BOTTOMhelp: B$ "CCS_BOTTOM:
 
Common Controls style:

Inverts the vertical appearance of the control." EOS
;;
[TBSTYLE_GROUPhelp: B$ "TBSTYLE_GROUP:

Uses the ToolBar as Group." EOS]

[TBSTYLE_CHECKGROUPhelp: B$ "TBSTYLE_CHECKGROUP:

Uses the ToolBar as a CheckBox Group." EOS]

[TBSTYLE_DROPDOWNhelp: B$ "TBSTYLE_DROPDOWN:

 Enables the dropdown mode on the ToolBar." EOS]

[TBSTYLE_AUTOSIZEhelp: B$ "TBSTYLE_AUTOSIZE:

AutoSize the ToolBar to fit the window width." EOS]

[CCS_ADJUSTABLEhelp: B$ "CCS_ADJUSTABLE:
 
Common Controls style:

Enables the customization mode of the control." EOS]

[CCS_LEFThelp: B$ "CCS_LEFT:
 
Common Controls style:

Place the Control at the Left Corner of the window." EOS]

[CCS_RIGHThelp: B$ "CCS_RIGHT:
 
Common Controls style:

Place the Control at the Right Corner of the window." EOS]

[TBSTYLE_TOOLTIPShelp: B$ "TBSTYLE_TOOLTIPS:
 
Enables showing tooltips on the ToolBar." EOS]

[TBSTYLE_WRAPABLEhelp: B$ "TBSTYLE_WRAPABLE:
 
Allows warp mode on the ToolBar." EOS]

[TBSTYLE_ALTDRAGhelp: B$ "TBSTYLE_ALTDRAG:
 
Allows dragging and drop the ToolBar.

To do this, press the key Alt and the left mouse button and drag a certain Button in the ToolBar." EOS]

[TBSTYLE_FLAThelp: B$ "TBSTYLE_FLAT:

Shows the ToolBar as Flat." EOS]

[TBSTYLE_LISThelp: B$ "TBSTYLE_LIST:
 
Enables using a List Control on the ToolBar." EOS]

[TBSTYLE_CUSTOMERASEhelp: B$ "TBSTYLE_CUSTOMERASE:
 
Allows customizatino of the ToolBar." EOS]

[TBSTYLE_REGISTERDROPhelp: B$ "TBSTYLE_REGISTERDROP:

Allows registering the drag and drop mode." EOS]

[TBSTYLE_TRANSPARENThelp: B$ "TBSTYLE_TRANSPARENT:
 
Shows the toolbar button as transparent to insert masked images." EOS]

[TBSTYLE_SEPhelp: B$ "TBSTYLE_SEP:

version 4.72 and earlier. This is equivalent to BTNS_SEP (for version 5.80 and later).

Creates a separator, providing a small gap between button groups.

A button that has this style does not receive user input." EOS]

[TBSTYLE_NOPREFIXhelp: B$ "TBSTYLE_NOPREFIX:

version 4.72 and earlier. This is equivalent to BTNS_NOPREFIX  (for version 5.80 and later).

Specifies that the button text will not have an accelerator prefix associated with it." EOS]

[BTNS_SHOWTEXThelp: B$ "BTNS_SHOWTEXT:

Version 5.81. Specifies that button text should be displayed.

All buttons can have text, but only those buttons with the BTNS_SHOWTEXT button style will display it.

This button style must be used with the TBSTYLE_LIST style and the TBSTYLE_EX_MIXEDBUTTONS extended style.

If you set text for buttons that do not have the BTNS_SHOWTEXT style, the toolbar control will automatically display it as a ToolTip when the cursor hovers over the button.

This feature allows your application to avoid handling the TBN_GETINFOTIP notification for the toolbar." EOS]

[BTNS_WHOLEDROPDOWNhelp: B$ "BTNS_WHOLEDROPDOWN:

Version 5.80. Specifies that the button will have a drop-down arrow, but not as a separate section.

Buttons with this style behave the same, regardless of whether the TBSTYLE_EX_DRAWDDARROWS extended style is set." EOS]

[TBSTYLE_EX_DRAWDDARROWShelp: B$ "TBSTYLE_EX_DRAWDDARROWS:

Version 4.71. This style allows buttons to have a separate dropdown arrow.

Buttons that have the BTNS_DROPDOWN style will be drawn with a drop-down arrow in a separate section, to the right of the button.

If the arrow is clicked, only the arrow portion of the button will depress, and the toolbar control will send a TBN_DROPDOWN notification to prompt the application to display the dropdown menu.

If the main part of the button is clicked, the toolbar control sends a WM_COMMAND message with the button's ID. The application normally responds by launching the first command on the menu.

There are many situations where you may want to have only some of the dropdown buttons on a toolbar with separated arrows. To do so, set the TBSTYLE_EX_DRAWDDARROWS extended style. Give those buttons that will not have separated arrows the BTNS_WHOLEDROPDOWN style. Buttons with this style will have an arrow displayed next to the image.

However, the arrow will not be separate and when any part of the button is clicked, the toolbar control will send a TBN_DROPDOWN notification.

To prevent repainting problems, this style should be set before the toolbar control becomes visible.

Note :

    To set an extended style, send the toolbar control a TB_SETEXTENDEDSTYLE message. To determine what extended styles are currently set, send a TB_GETEXTENDEDSTYLE message." EOS]

[TBSTYLE_EX_HIDECLIPPEDBUTTONShelp: B$ "TBSTYLE_EX_HIDECLIPPEDBUTTONS:

Version 5.81. This style hides partially clipped buttons.

The most common use of this style is for toolbars that are part of a rebar control. If an adjacent band covers part of a button, the button will not be displayed.
However, if the rebar band has the RBBS_USECHEVRON style, the button will be displayed on the chevron's dropdown menu.

Note :

    To set an extended style, send the toolbar control a TB_SETEXTENDEDSTYLE message. To determine what extended styles are currently set, send a TB_GETEXTENDEDSTYLE message." EOS]

[TBSTYLE_EX_DOUBLEBUFFERhelp: B$ "TBSTYLE_EX_DOUBLEBUFFER:

Version 6. This style requires the toolbar to be double buffered. Double buffering is a mechanism that detects when the toolbar has changed.
Comctl32.dll version 6 is not redistributable but it is included in Microsoft Windows XP or later. To use Comctl32.dll version 6, specify it in a manifest.

Note :

    To set an extended style, send the toolbar control a TB_SETEXTENDEDSTYLE message. To determine what extended styles are currently set, send a TB_GETEXTENDEDSTYLE message." EOS]

[TBSTYLE_EX_MIXEDBUTTONShelp: B$ "TBSTYLE_EX_MIXEDBUTTONS:

Version 5.81. This style allows you to set text for all buttons, but only display it for those buttons with the BTNS_SHOWTEXT button style.

The TBSTYLE_LIST style must also be set. Normally, when a button does not display text, your application must handle TBN_GETINFOTIP to display a ToolTip.

With the TBSTYLE_EX_MIXEDBUTTONS extended style, text that is set but not displayed on a button will automatically be used as the button's ToolTip text.

Your application only needs to handle TBN_GETINFOTIP if it needs more flexibility in specifying the ToolTip text.

Note :

    To set an extended style, send the toolbar control a TB_SETEXTENDEDSTYLE message. To determine what extended styles are currently set, send a TB_GETEXTENDEDSTYLE message." EOS]

[CCS_NOMOVEYhelp: B$ "CCS_NOMOVEY:

Causes the control to resize and Move itself horizontally, but not vertically, in response to a WM_SIZE message.

Header windows have this style by default. This style does not apply if your control has the CCS_NORESIZE style." EOS]

[CCS_NORESIZEhelp: B$ "CCS_NORESIZE:
 
Prevents the control from using the default width and height when setting its initial size or a new size.

Instead, the control uses the width and height that is specified in the request for creation or sizing." EOS]

[CCS_NOPARENTALIGNhelp: B$ "CCS_NOPARENTALIGN:
 
Prevents the control from automatically moving to the top or bottom of the parent window.

Instead, the control keeps its position within the parent window despite changes to the size of the parent.
If the application also uses the CCS_TOP or CCS_BOTTOM styles, it adjusts the height to the default, but does not change the position and width of the control." EOS]

[CCS_NODIVIDERhelp: B$ "CCS_NODIVIDER:

Prevents a 2-pixel highlight from being drawn at the top of the control." EOS]

[CCS_VERThelp: B$ "CCS_VERT:

Causes the control to display vertically." EOS]

[CCS_NOMOVEXhelp: B$ "CCS_NOMOVEX:

Version 4.70. Causes the control to resize and Move itself vertically, but not horizontally, in response to a WM_SIZE message.

If CCS_NORESIZE is used, this style does not apply." EOS]

[WS_CLIPSIBLINGShelp: B$ "WS_CLIPSIBLINGS:

Clips child windows relative to each other; that is, when a particular child window receives a paint message, the WS_CLIPSIBLINGS style clips all other overlapped child windows out of the region of the child window to be updated.
(If WS_CLIPSIBLINGS is not given and child windows overlap, when you draw within the client area of a child window, it is possible to draw within the client area of a neighboring child window.) For use with the WS_CHILD style only." EOS]

[ToolBarStylesHelp:
 D$ TBSTYLE_BUTTONhelp
 D$ TBSTYLE_SEPhelp
 D$ TBSTYLE_CHECKhelp
 D$ TBSTYLE_GROUPhelp
 D$ TBSTYLE_CHECKGROUPhelp
 D$ TBSTYLE_DROPDOWNhelp
 D$ TBSTYLE_AUTOSIZEhelp
 D$ TBSTYLE_NOPREFIXhelp
 D$ BTNS_SHOWTEXThelp
 D$ BTNS_WHOLEDROPDOWNhelp
 D$ TBSTYLE_TOOLTIPShelp
 D$ TBSTYLE_WRAPABLEhelp
 D$ TBSTYLE_ALTDRAGhelp
 D$ TBSTYLE_FLAThelp
 D$ TBSTYLE_LISThelp
 D$ TBSTYLE_CUSTOMERASEhelp
 D$ TBSTYLE_REGISTERDROPhelp
 D$ TBSTYLE_TRANSPARENThelp
 D$ TBSTYLE_EX_DRAWDDARROWShelp
 D$ TBSTYLE_EX_MIXEDBUTTONShelp
 D$ TBSTYLE_EX_HIDECLIPPEDBUTTONShelp
 D$ TBSTYLE_EX_DOUBLEBUFFERhelp
 D$ CCS_TOPhelp
 D$ CCS_NOMOVEYhelp
 D$ CCS_BOTTOMhelp
 D$ CCS_NORESIZEhelp
 D$ CCS_NOPARENTALIGNhelp
 D$ CCS_ADJUSTABLEhelp
 D$ CCS_NODIVIDERhelp
 D$ CCS_VERThelp
 D$ CCS_LEFThelp
 D$ CCS_NOMOVEXhelp
 D$ CCS_RIGHThelp
 D$ WS_TABSTOPhelp
 D$ WS_GROUPhelp
 D$ WS_DISABLEDhelp
 D$ WS_CHILDhelp
 D$ WS_VISIBLEhelp
 D$ WS_BORDERhelp
 D$ WS_CLIPSIBLINGShelp]

; &CCS_VERT We don't need this because when it is set to Left or Right, it already uses the CCS_VERT
; &TBSTYLE_SEP The same as TBSTYLE_SEP
; &TBSTYLE_NOPREFIX iS THE SAME AS &CCS_ADJUSTABLE
____________________________________

; RichEdit20a control tables:


[RichEdit20aCheckingMask:
 D$ 03
 D$ 03
 D$ 03
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[RichEdit20aTextTable:
 B$ 'ES_LEFT' EOS
 B$ 'ES_CENTER' EOS
 B$ 'ES_RIGHT' EOS
 B$ 'ES_MULTILINE' EOS
 B$ 'ES_AUTOVSCROLL' EOS
 B$ 'ES_AUTOHSCROLL' EOS
 B$ 'ES_LOWERCASE' EOS
 B$ 'ES_UPPERCASE' EOS
 B$ 'ES_PASSWORD' EOS
 B$ 'ES_OEMCONVERT' EOS
 B$ 'ES_NOHIDESEL' EOS
 B$ 'ES_READONLY' EOS
 B$ 'ES_NUMBER' EOS
 B$ 'ES_WANTRETURN' EOS
 B$ 'WS_VSCROLL' EOS
 B$ 'WS_HSCROLL' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 'WS_BORDER' EOS
 B$ 'WS_VISIBLE' EOS
 B$ 'WS_CHILD' EOS
 B$ 0]

[RichEdit20aBitTable:
 D$ &ES_LEFT
 D$ &ES_CENTER
 D$ &ES_RIGHT
 D$ &ES_MULTILINE
 D$ &ES_AUTOVSCROLL
 D$ &ES_AUTOHSCROLL
 D$ &ES_LOWERCASE
 D$ &ES_UPPERCASE
 D$ &ES_PASSWORD
 D$ &ES_OEMCONVERT
 D$ &ES_NOHIDESEL
 D$ &ES_READONLY
 D$ &ES_NUMBER
 D$ &ES_WANTRETURN
 D$ &WS_VSCROLL
 D$ &WS_HSCROLL
 D$ &WS_TABSTOP
 D$ &WS_BORDER
 D$ &WS_VISIBLE
 D$ &WS_CHILD]

[RichEdit20aExcludeBitTable:
 D$ 03
 D$ 02
 D$ 01
 D$ 0
 D$ 0
 D$ 0
 D$ 08
 D$ 010
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[RichEdit20aMustHaveBitTable:
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ &WS_VSCROLL
 D$ &WS_HSCROLL
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[RichEdit20aStylesHelp:
 D$ ES_LEFThelp
 D$ ES_CENTERhelp
 D$ ES_RIGHThelp
 D$ ES_MULTILINEhelp
 D$ ES_AUTOVSCROLLhelp
 D$ ES_AUTOHSCROLLhelp
 D$ ES_LOWERCASEhelp
 D$ ES_UPPERCASEhelp
 D$ ES_PASSWORDhelp
 D$ ES_OEMCONVERThelp
 D$ ES_NOHIDESELhelp
 D$ ES_READONLYhelp
 D$ ES_NUMBERhelp
 D$ ES_WANTRETURNhelp
 D$ WS_VSCROLLhelp
 D$ WS_HSCROLLhelp
 D$ WS_TABSTOPhelp
 D$ WS_BORDERhelp
 D$ WS_VISIBLEhelp
 D$ WS_CHILDhelp]

____________________________________

; SysHeader32 control tables:

; use this on: ShowControlStyles ,  ShowControlStyleControl , ControlClassByNames , SearchWhatControlClass , WriteStyle,
; ShowStyleInfo
  ;&WS_GROUP &WS_DISABLED
  ; &BS_RIGHTBUTTON &BS_TEXT

[SysHeader32CheckingMask: D$ ? # 19]


[SysHeader32TextTable:
 B$ 'HDS_BUTTONS' EOS
 B$ 'HDS_DRAGDROP' EOS
 B$ 'HDS_FILTERBAR' EOS
 B$ 'HDS_FLAT' EOS
 B$ 'HDS_FULLDRAG' EOS
 B$ 'HDS_HIDDEN' EOS
 B$ 'HDS_HORZ' EOS
 B$ 'HDS_HOTTRACK' EOS
 B$ 'HDS_CHECKBOXES' EOS
 B$ 'HDS_NOSIZING' EOS
 B$ 'HDS_OVERFLOW' EOS
 B$ 'WS_VSCROLL' EOS
 B$ 'WS_HSCROLL' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 'WS_BORDER' EOS
 B$ 'WS_VISIBLE' EOS
 B$ 'WS_CHILD' EOS
 B$ 'WS_GROUP' EOS
 B$ 'WS_DISABLED' EOS
 B$ 0]

[SysHeader32BitTable:
 D$ &HDS_BUTTONS
 D$ &HDS_DRAGDROP
 D$ &HDS_FILTERBAR
 D$ &HDS_FLAT
 D$ &HDS_FULLDRAG
 D$ &HDS_HIDDEN
 D$ &HDS_HORZ
 D$ &HDS_HOTTRACK
 D$ &HDS_CHECKBOXES
 D$ &HDS_NOSIZING
 D$ &HDS_OVERFLOW
 D$ &WS_VSCROLL
 D$ &WS_HSCROLL
 D$ &WS_TABSTOP
 D$ &WS_BORDER
 D$ &WS_VISIBLE
 D$ &WS_CHILD
 D$ &WS_GROUP
 D$ &WS_DISABLED]

[SysHeader32ExcludeBitTable:
 D$ &HDS_CHECKBOXES
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ &HDS_BUTTONS
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[SysHeader32MustHaveBitTable: D$ ? # 19]

[SysHeader32StylesHelp:
 D$ HDS_BUTTONShelp
 D$ HDS_DRAGDROPhelp
 D$ HDS_FILTERBARhelp
 D$ HDS_FLAThelp
 D$ HDS_FULLDRAGhelp
 D$ HDS_HIDDENhelp
 D$ HDS_HORZhelp
 D$ HDS_HOTTRACKhelp
 D$ HDS_CHECKBOXEShelp
 D$ HDS_NOSIZINGhelp
 D$ HDS_OVERFLOWhelp
 D$ WS_VSCROLLhelp
 D$ WS_HSCROLLhelp
 D$ WS_TABSTOPhelp
 D$ WS_BORDERhelp
 D$ WS_VISIBLEhelp
 D$ WS_CHILDhelp
 D$ WS_GROUPhelp
 D$ WS_DISABLEDhelp]

[HDS_BUTTONShelp: B$ "HDS_BUTTONS:

Each item in the control looks and behaves like a Push button.
This style is useful if an application carries out a task when the user clicks an item in the header control.
For example, an application could sort information in the columns differently depending on which item the user clicks." EOS]

[HDS_DRAGDROPhelp: B$ "HDS_DRAGDROP:

Version 4.70. Allows drag-and-drop reordering of header items." EOS]

[HDS_FILTERBARhelp: B$ "HDS_FILTERBAR:

Version 5.80. Include a filter bar as part of the standard header control.
This bar allows users to conveniently apply a filter to the display.
Calls to HDM_LAYOUT will yield a new size for the control and cause the list view to update." EOS]

[HDS_FLAThelp: B$ "HDS_FLAT:

Version 6.0. Causes the header control to be drawn flat when Microsoft Windows XP is running in classic mode.

Note:  Comctl32.dll version 6 is not redistributable but it is included in Windows XP or later.
To use Comctl32.dll version 6, specify it in a manifest windows style" EOS]

[HDS_FULLDRAGhelp: B$ "HDS_FULLDRAG:

Version 4.70. Causes the header control to display column contents even while the user resizes a column." EOS]

[HDS_HIDDENhelp: B$ "HDS_HIDDEN:

Indicates a header control that is intended to be hidden.
This style does not hide the control.
Instead, when you send the HDM_LAYOUT message to a header control with the HDS_HIDDEN style, the control returns zero in the cy member of the WINDOWPOS structure.
You would then hide the control by setting its height to zero.
This can be useful when you want to use the control as an information container instead of a visual control." EOS]

[HDS_HORZhelp: B$ "HDS_HORZ:

Creates a header control with a horizontal orientation." EOS]

[HDS_HOTTRACKhelp: B$ "HDS_HOTTRACK:

Version 4.70. Enables hot tracking." EOS]

[HDS_CHECKBOXEShelp: B$ "HDS_CHECKBOXES:

(0x0400)" EOS]

[HDS_NOSIZINGhelp: B$ "HDS_NOSIZING:

(0x0800)" EOS]

[HDS_OVERFLOWhelp: B$ "HDS_OVERFLOW:

(0x1000)" EOS]

____________________________________

; ReBarWindow32 controls

[ReBarWindow32CheckingMask:
 D$ &CCS_BOTTOM
 D$ &CCS_VERT+&CCS_BOTTOM
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ &CCS_VERT
 D$ &CCS_RIGHT
 D$ &CCS_LEFT
 D$ 0
 D$ 0
 D$ 0]


[ReBarWindow32TextTable:
 B$ 'CCS_TOP' EOS
 B$ 'CCS_BOTTOM' EOS
 B$ 'RBS_AUTOSIZE' EOS
 B$ 'CCS_ADJUSTABLE' EOS
 B$ 'RBS_BANDBORDERS' EOS
 B$ 'RBS_DBLCLKTOGGLE' EOS
 B$ 'RBS_FIXEDORDER' EOS
 B$ 'RBS_REGISTERDROP' EOS
 B$ 'RBS_TOOLTIPS' EOS
 B$ 'RBS_VARHEIGHT' EOS
 B$ 'RBS_VERTICALGRIPPER' EOS
 B$ 'CCS_LEFT' EOS
 B$ 'CCS_RIGHT' EOS
 B$ 'WS_BORDER' EOS
 B$ 'WS_VISIBLE' EOS
 B$ 'WS_CHILD' EOS
 B$ 0]

[ReBarWindow32BitTable:
 D$ &CCS_TOP
 D$ &CCS_BOTTOM
 D$ &RBS_AUTOSIZE
 D$ &CCS_ADJUSTABLE
 D$ &RBS_BANDBORDERS
 D$ &RBS_DBLCLKTOGGLE
 D$ &RBS_FIXEDORDER
 D$ &RBS_REGISTERDROP
 D$ &RBS_TOOLTIPS
 D$ &RBS_VARHEIGHT
 D$ &RBS_VERTICALGRIPPER
 D$ &CCS_LEFT
 D$ &CCS_RIGHT
 D$ &WS_BORDER
 D$ &WS_VISIBLE &WS_CHILD]

[ReBarWindow32ExcludeBitTable:
 D$ &CCS_VERT+&CCS_BOTTOM
 D$ &CCS_VERT+&CCS_TOP
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ &CCS_RIGHT+&CCS_BOTTOM+&CCS_TOP
 D$ &CCS_LEFT+&CCS_TOP+&CCS_BOTTOM
 D$ 0
 D$ 0
 D$ 0]

[ReBarWindow32MustHaveBitTable: D$ ? # 16]

[ReBarWindow32StylesHelp:
 D$ CCS_TOPhelp
 D$ CCS_BOTTOMhelp
 D$ RBS_AUTOSIZEhelp
 D$ CCS_ADJUSTABLEhelp
 D$ RBS_BANDBORDERShelp
 D$ RBS_DBLCLKTOGGLEhelp
 D$ RBS_FIXEDORDERhelp
 D$ RBS_REGISTERDROPhelp
 D$ RBS_TOOLTIPShelp
 D$ RBS_VARHEIGHThelp
 D$ RBS_VERTICALGRIPPERhelp
 D$ CCS_LEFThelp
 D$ CCS_RIGHThelp
 D$ WS_BORDERhelp
 D$ WS_VISIBLEhelp
 D$ WS_CHILDhelp]


[RBS_AUTOSIZEhelp: B$ "RBS_AUTOSIZE:

Version 4.71. The rebar control will automatically change the layout of the bands when the size or position of the control changes. An RBN_AUTOSIZE notification will be sent when this occurs." EOS]

[RBS_BANDBORDERShelp: B$ "RBS_BANDBORDERS:

Version 4.71. The rebar control displays narrow lines to separate adjacent bands." EOS]

[RBS_DBLCLKTOGGLEhelp: B$ "RBS_DBLCLKTOGGLE:

Version 4.71. The rebar band will toggle its maximized or minimized state when the user double-clicks the band. Without this style, the maximized or minimized state is toggled when the user single-clicks on the band." EOS]

[RBS_FIXEDORDERhelp: B$ "RBS_FIXEDORDER:

Version 4.70. The rebar control always displays bands in the same order. You can Move bands to different rows, but the band order is static." EOS]

[RBS_REGISTERDROPhelp: B$ "RBS_REGISTERDROP:

Version 4.71. The rebar control generates RBN_GETOBJECT notification messages when an object is dragged over a band in the control. To receive the RBN_GETOBJECT notifications, initialize OLE with a Call to OleInitialize or CoInitialize." EOS]

[RBS_TOOLTIPShelp: B$ "RBS_TOOLTIPS:

Version 4.71. Not yet supported." EOS]

[RBS_VARHEIGHThelp: B$ "RBS_VARHEIGHT:

Version 4.71. The rebar control displays bands at the minimum required height, when possible. Without this style, the rebar control displays all bands at the same height, using the height of the tallest visible band to determine the height of other bands." EOS]

[RBS_VERTICALGRIPPERhelp: B$ "RBS_VERTICALGRIPPER:

Version 4.71. The size grip will be displayed vertically instead of horizontally in a vertical rebar control. This style is ignored for rebar controls that do not have the CCS_VERT style." EOS]

____________________________________

; tooltips_class32
;note: A ToolTip control always has the WS_POPUP and WS_EX_TOOLWINDOW window styles,
; regardless of whether you specify them when creating the control.

[Tooltips_class32CheckingMask: D$ ? # 12]
 ;&WS_POPUP  &WS_EX_TOOLWINDOW]

[Tooltips_class32TextTable:
 B$ 'TTS_ALWAYSTIP' EOS
 B$ 'TTS_BALLOON' EOS
 B$ 'TTS_NOANIMATE' EOS
 B$ 'TTS_NOFADE' EOS
 B$ 'TTS_NOPREFIX' EOS
 B$ 'TTS_USEVISUALSTYLE' EOS
 B$ 'TTS_CLOSE' EOS
 B$ 'WS_BORDER' EOS
 B$ '&WS_VISIBLE' EOS
 B$ 'WS_CHILD' EOS
 B$ 'WS_POPUP' EOS
 B$ 'WS_EX_TOOLWINDOW' EOS
 B$ 0]

[Tooltips_class32BitTable:
 D$ &TTS_ALWAYSTIP
 D$ &TTS_BALLOON
 D$ &TTS_NOANIMATE
 D$ &TTS_NOFADE
 D$ &TTS_NOPREFIX
 D$ &TTS_USEVISUALSTYLE
 D$ &TTS_CLOSE
 D$ &WS_BORDER
 D$ &WS_VISIBLE
 D$ &WS_CHILD
 D$ &WS_POPUP
 D$ &WS_EX_TOOLWINDOW]

[Tooltips_class32ExcludeBitTable: D$ ? # 12]


[Tooltips_class32MustHaveBitTable:
 D$ &WS_POPUP+&WS_EX_TOOLWINDOW
 D$ &WS_POPUP+&WS_EX_TOOLWINDOW
 D$ &WS_POPUP+&WS_EX_TOOLWINDOW
 D$ &WS_POPUP+&WS_EX_TOOLWINDOW
 D$ &WS_POPUP+&WS_EX_TOOLWINDOW
 D$ &WS_POPUP+&WS_EX_TOOLWINDOW
 D$ &WS_POPUP+&WS_EX_TOOLWINDOW
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0];&WS_POPUP  &WS_EX_TOOLWINDOW];  0]

[Tooltips_class32StylesHelp:
 D$ TTS_ALWAYSTIPHelp
 D$ TTS_BALLOONHelp
 D$ TTS_NOANIMATEHelp
 D$ TTS_NOFADEHelp
 D$ TTS_NOPREFIXHelp
 D$ TTS_USEVISUALSTYLEHelp
 D$ TTS_CLOSEHelp
 D$ WS_BORDERhelp
 D$ WS_VISIBLEhelp
 D$ WS_CHILDhelp
 D$ WS_POPUPhelp
 D$ WS_EX_TOOLWINDOWhelp]

[TTS_ALWAYSTIPHelp: B$ "TTS_ALWAYSTIP:

Indicates that the ToolTip control appears when the cursor is on a tool, even if the ToolTip control's owner window is inactive.
Without this style, the ToolTip appears only when the tool's owner window is active." EOS]

[TTS_BALLOONHelp: B$ "TTS_BALLOON:

Version 5.80. Indicates that the ToolTip control has the appearance of a cartoon 'balloon,' with rounded corners and a stem pointing to the item." EOS]

[TTS_NOANIMATEHelp: B$ "TTS_NOANIMATE:

Version 5.80. Disables sliding ToolTip animation on Microsoft Windows 98 and Windows 2000 systems.
This style is ignored on earlier systems." EOS]

[TTS_NOFADEHelp: B$ "TTS_NOFADE:

Version 5.80. Disables fading ToolTip animation on Windows 2000 systems.
This style is ignored on earlier Microsoft Windows NT systems, and on Windows 95 and Windows 98." EOS]

[TTS_NOPREFIXHelp: B$ "TTS_NOPREFIX:

Prevents the system from stripping the ampersand character from a string.
Without this style, the system automatically strips ampersand characters.
This allows an application to use the same string as both a menu item and as text in a ToolTip control." EOS]

[TTS_USEVISUALSTYLEHelp: B$ "TTS_USEVISUALSTYLE:

Uses themed hyperlinks. The theme will define the styles for any links in the tooltip.
This style always requires TTF_PARSELINKS to be set." EOS]

[TTS_CLOSEHelp: B$ "TTS_CLOSE:

Displays a Close button on the tooltip." EOS]

[WS_EX_TOOLWINDOWhelp: B$ "WS_EX_TOOLWINDOW:

Creates a tool window; that is, a window intended to be used as a floating toolbar.
A tool window has a title bar that is shorter than a normal title bar, and the window title is drawn using a smaller font.
A tool window does not appear in the taskbar or in the dialog box that appears when the user presses ALT+TAB.
If a tool window has a system menu, its icon is not displayed on the title bar.
However, you can display the system menu by typing ALT+SPACE." EOS]
____________________________________

; msctls_statusbar32

[msctls_statusbar32CheckingMask: D$ ? # 6]

[msctls_statusbar32TextTable:
 B$ 'SBARS_SIZEGRIP' EOS
 B$ 'SBT_TOOLTIPS' EOS
 B$ 'WS_BORDER' EOS
 B$ '&WS_VISIBLE' EOS
 B$ 'WS_CHILD' EOS
 B$ 'WS_POPUP' EOS
 B$ 0]



[msctls_statusbar32BitTable:
 D$ &SBARS_SIZEGRIP
 D$ &SBT_TOOLTIPS
 D$ &WS_BORDER
 D$ &WS_VISIBLE
 D$ &WS_CHILD
 D$ &WS_POPUP]


[msctls_statusbar32ExcludeBitTable: D$ ? # 6]

[msctls_statusbar32MustHaveBitTable: D$ ? # 6]

[msctls_statusbar32StylesHelp:
 D$ SBARS_SIZEGRIPHelp
 D$ SBT_TOOLTIPSHelp
 D$ WS_BORDERhelp
 D$ WS_VISIBLEhelp
 D$ WS_CHILDhelp
 D$ WS_POPUPhelp]

[SBARS_SIZEGRIPHelp: B$ "SBARS_SIZEGRIP:

The status bar control will include a sizing grip at the right end of the status bar.
A sizing grip is similar to a sizing border; it is a rectangular area that the user can click and drag to resize the parent window." EOS]

[SBT_TOOLTIPSHelp: B$ "SBT_TOOLTIPS:

Version 4.71.Use this style to enable ToolTips.
This is exactly the same as SBARS_TOOLTIPS" EOS]
____________________________________

; msctls_hotkey32

[msctls_hotkey32CheckingMask: D$ ? # 2]

[msctls_hotkey32TextTable:
 B$ 'WS_BORDER' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 0]

[msctls_hotkey32BitTable:
 D$ &WS_BORDER
 D$ &WS_TABSTOP]


[msctls_hotkey32ExcludeBitTable: D$ ? # 2]

[msctls_hotkey32MustHaveBitTable: D$ ? # 2]

[msctls_hotkey32StylesHelp:
 D$ WS_BORDERhelp
 D$ WS_TABSTOPhelp]
____________________________________

; ComboBoxEx32

[ComboBoxEx32CheckingMask: D$ ? # 9]

[ComboBoxEx32TextTable:
 B$ 'CBS_OWNERDRAWFIXED' EOS
 B$ 'CBES_EX_CASESENSITIVE' EOS
 B$ 'CBES_EX_NOEDITIMAGE' EOS
 B$ 'CBES_EX_NOEDITIMAGEINDENT' EOS
 B$ 'CBES_EX_NOSIZELIMIT' EOS
 B$ 'CBES_EX_PATHWORDBREAKPROC' EOS         ;'CBES_EX_TEXTENDELLIPSIS' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 'WS_VSCROLL' EOS
 B$ 'WS_HSCROLL' EOS
 B$ 0]


[ComboBoxEx32BitTable:
 D$ &CBS_OWNERDRAWFIXED
 D$ &CBES_EX_CASESENSITIVE
 D$ &CBES_EX_NOEDITIMAGE
 D$ &CBES_EX_NOEDITIMAGEINDENT
 D$ &CBES_EX_NOSIZELIMIT
 D$ &CBES_EX_PATHWORDBREAKPROC  ;  &CBES_EX_TEXTENDELLIPSIS (For Windows Vista. we need this equate later :) )
 D$ &WS_TABSTOP
 D$ &WS_VSCROLL
 D$ &WS_HSCROLL]

[ComboBoxEx32ExcludeBitTable: D$ ? # 9]

[ComboBoxEx32MustHaveBitTable: D$ ? # 9]

[ComboBoxEx32StylesHelp:
 D$ CBS_OWNERDRAWFIXEDhelp
 D$ CBES_EX_CASESENSITIVEhelp
 D$ CBES_EX_NOEDITIMAGEhelp
 D$ CBES_EX_NOEDITIMAGEINDENThelp
 D$ CBES_EX_NOSIZELIMIThelp
 D$ CBES_EX_PATHWORDBREAKPROChelp ; CBES_EX_TEXTENDELLIPSIShelp
 D$ WS_TABSTOPhelp
 D$ WS_VSCROLLhelp
 D$ WS_HSCROLLhelp]

[CBES_EX_CASESENSITIVEhelp: B$ "CBES_EX_CASESENSITIVE:

BSTR searches in the list will be case sensitive.
This includes searches as a result of text being typed in the edit box and the CB_FINDSTRINGEXACT message." EOS]

[CBES_EX_NOEDITIMAGEhelp: B$ "CBES_EX_NOEDITIMAGE:

The edit box and the dropdown list will not display item images." EOS]

[CBES_EX_NOEDITIMAGEINDENThelp: B$ "CBES_EX_NOEDITIMAGEINDENT:

The edit box and the dropdown list will not display item images." EOS]

[CBES_EX_NOSIZELIMIThelp: B$ "CBES_EX_NOSIZELIMIT:

Allows the ComboBoxEx control to be vertically sized smaller than its contained combo box control.
If the ComboBoxEx is sized smaller than the combo box, the combo box will be clipped." EOS]

[CBES_EX_PATHWORDBREAKPROChelp: B$ "CBES_EX_PATHWORDBREAKPRO:

Microsoft Windows NT only.
The edit box will use the slash (/), backslash (\), and period (.) characters as word delimiters.
This makes keyboard shortcuts for word-by-word cursor movement () effective in path names and URLs." EOS]

[CBES_EX_TEXTENDELLIPSIShelp: B$ "CBES_EX_TEXTENDELLIPSIS:

Windows Vista and later.
Causes items in the drop-down list and the edit box (when the edit box is read only) to be truncated with an ellipsis ('...') rather than just clipped by the edge of the control.
This is useful when the control needs to be set to a fixed width, yet the entries in the list may be long." EOS]

____________________________________

;SysAnimate32


[SysAnimate32CheckingMask: D$ ? # 8]

[SysAnimate32TextTable:
 B$ 'ACS_AUTOPLAY' EOS
 B$ 'ACS_CENTER' EOS
 B$ 'ACS_TIMER' EOS
 B$ 'ACS_TRANSPARENT' EOS
 B$ 'WS_BORDER' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 'WS_VSCROLL' EOS
 B$ 'WS_HSCROLL' EOS
 B$ 0]

[SysAnimate32BitTable:
 D$ &ACS_AUTOPLAY
 D$ &ACS_CENTER
 D$ &ACS_TIMER
 D$ &ACS_TRANSPARENT
 D$ &WS_BORDER
 D$ &WS_TABSTOP
 D$ &WS_VSCROLL
 D$ &WS_HSCROLL]

[SysAnimate32ExcludeBitTable: D$ ? # 8]

[SysAnimate32MustHaveBitTable: D$ ? # 8]

[SysAnimate32StylesHelp:
 D$ ACS_AUTOPLAYhelp
 D$ ACS_CENTERhelp
 D$ ACS_TIMERhelp
 D$ ACS_TRANSPARENThelp
 D$ WS_BORDERhelp
 D$ WS_TABSTOPhelp
 D$ WS_VSCROLLhelp
 D$ WS_HSCROLLhelp]

[ACS_AUTOPLAYhelp: B$ "ACS_AUTOPLAY:

Starts playing the animation as soon as the AVI clip is opened." EOS]

[ACS_CENTERhelp: B$ "ACS_CENTER:

Centers the animation in the animation control's window." EOS]

[ACS_TIMERhelp: B$ "ACS_TIMER:

By default, the control creates a thread to play the AVI clip.
If you set this flag, the control plays the clip without creating a thread; internally the control uses a Win32 timer to synchronize playback.
Comctl32.dll version 6 and later: This style is not supported. By default, the control plays the AVI clip without creating a thread.
Note:  Comctl32.dll version 6 is not redistributable, but it is included in Microsoft Windows XP.
To use Comctl32.dll version 6, specify it in a manifest." EOS]

[ACS_TRANSPARENThelp: B$ "ACS_TRANSPARENT:

Allows you to match an animation's background color to that of the underlying window, creating a 'transparent' background.
The parent of the animation control must not have the WS_CLIPCHILDREN style.
The control sends a WM_CTLCOLORSTATIC message to its parent.
Use SetBkColor to set the background color for the device context to an appropriate value.
The control interprets the upper-left pixel of the first frame as the animation's default background color.
It will remap all pixels with that color to the value you supplied in response to WM_CTLCOLORSTATIC." EOS]
____________________________________

; SysMonthCal32

[SysMonthCal32CheckingMask: D$ ? # 9]

[SysMonthCal32TextTable:
 B$ 'MCS_DAYSTATE' EOS
 B$ 'MCS_MULTISELECT' EOS
 B$ 'MCS_WEEKNUMBERS' EOS
 B$ 'MCS_NOTODAYCIRCLE' EOS
 B$ 'MCS_NOTODAY' EOS   ; 'MCS_NOTRAILINGDATES' EOS 'MCS_SHORTDAYSOFWEEK' EOS     'MCS_NOSELCHANGEONNAV' EOS
 B$ 'WS_BORDER' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 'WS_VSCROLL' EOS
 B$ 'WS_HSCROLL' EOS
 B$ 0]


[SysMonthCal32BitTable:
 D$ &MCS_DAYSTATE
 D$ &MCS_MULTISELECT
 D$ &MCS_WEEKNUMBERS
 D$ &MCS_NOTODAYCIRCLE
 D$ &MCS_NOTODAY   ; &MCS_NOTRAILINGDATES &MCS_SHORTDAYSOFWEEK     &MCS_NOSELCHANGEONNAV (these are Vista equates.. need to get them later)
 D$ &WS_BORDER
 D$ &WS_TABSTOP
 D$ &WS_VSCROLL
 D$ &WS_HSCROLL]

[SysMonthCal32ExcludeBitTable: D$ ? # 9]

[SysMonthCal32MustHaveBitTable: D$ ? # 9]

[SysMonthCal32StylesHelp:
 D$ MCS_DAYSTATEhelp
 D$ MCS_MULTISELECThelp
 D$ MCS_WEEKNUMBERShelp
 D$ MCS_NOTODAYCIRCLEhelp
 D$ MCS_NOTODAYhelp    ;MCS_NOTRAILINGDATEShelp MCS_SHORTDAYSOFWEEKhelp     MCS_NOSELCHANGEONNAVhelp
 D$ WS_BORDERhelp
 D$ WS_TABSTOPhelp
 D$ WS_VSCROLLhelp
 D$ WS_HSCROLLhelp]

[MCS_DAYSTATEhelp: B$ "MCS_DAYSTATE:

Version 4.70. The month calendar will send MCN_GETDAYSTATE notifications to request information about which days should be displayed in bold." EOS]

[MCS_MULTISELECThelp: B$ "MCS_MULTISELECT:

Version 4.70. The month calendar will allow the user to select a range of dates within the control.
By default, the maximum range is one week. You can change the maximum range that can be selected by using the MCM_SETMAXSELCOUNT message." EOS]

[MCS_WEEKNUMBERShelp: B$ "MCS_WEEKNUMBERS:

Version 4.70. The month calendar control will display week numbers (1-52) to the left of each row of days.
Week 1 is defined as the first week that contains at least four days." EOS]

[MCS_NOTODAYCIRCLEhelp: B$ "MCS_NOTODAYCIRCLE:

Version 4.70. The month calendar control will not circle the 'today' date." EOS]

[MCS_NOTODAYhelp: B$ "MCS_NOTODAY:

Version 4.70.The month calendar control will not display the 'today' date at the bottom of the control." EOS]

[MCS_NOTRAILINGDATEShelp: B$ "MCS_NOTRAILINGDATES:

Microsoft Windows Vista. This flag disables displaying the dates from the previous/next month in the current calendar." EOS]

[MCS_SHORTDAYSOFWEEKhelp: B$ "MCS_SHORTDAYSOFWEEK:

Microsoft Windows Vista. This flag uses the CAL_SSHORTESTDAYNAME* names to display for the day of the week column header." EOS]

[MCS_NOSELCHANGEONNAVhelp: B$ "MCS_NOSELCHANGEONNAV:

Microsoft Windows Vista. This flag does not change the selection when the user navigates next or previous in the calendar.
This allows the user to select a range larger than what they can currently see." EOS]
____________________________________

; SysDateTimePick32

[SysDateTimePick32CheckingMask:
 D$ &DTS_APPCANPARSE
 D$ &DTS_LONGDATEFORMAT
 D$ &DTS_RIGHTALIGN
 D$ &DTS_SHOWNONE
 D$ &DTS_SHORTDATEFORMAT
 D$ &DTS_SHORTDATECENTURYFORMAT
 D$ &DTS_TIMEFORMAT
 D$ &DTS_UPDOWN
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[SysDateTimePick32TextTable:
 B$ 'DTS_APPCANPARSE' EOS
 B$ 'DTS_LONGDATEFORMAT' EOS
 B$ 'DTS_RIGHTALIGN' EOS
 B$ 'DTS_SHOWNONE' EOS
 B$ 'DTS_SHORTDATEFORMAT' EOS
 B$ 'DTS_SHORTDATECENTURYFORMAT' EOS
 B$ 'DTS_TIMEFORMAT' EOS
 B$ 'DTS_UPDOWN' EOS
 B$ 'WS_BORDER' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 'WS_VSCROLL' EOS
 B$ 'WS_HSCROLL' EOS
 B$ 0]

[SysDateTimePick32BitTable:
 D$ &DTS_APPCANPARSE
 D$ &DTS_LONGDATEFORMAT
 D$ &DTS_RIGHTALIGN
 D$ &DTS_SHOWNONE
 D$ &DTS_SHORTDATEFORMAT
 D$ &DTS_SHORTDATECENTURYFORMAT
 D$ &DTS_TIMEFORMAT
 D$ &DTS_UPDOWN
 D$ &WS_BORDER
 D$ &WS_TABSTOP
 D$ &WS_VSCROLL
 D$ &WS_HSCROLL]

[SysDateTimePick32ExcludeBitTable:
 D$ &DTS_LONGDATEFORMAT+&DTS_RIGHTALIGN+&DTS_SHOWNONE+&DTS_SHORTDATEFORMAT+&DTS_SHORTDATECENTURYFORMAT+&DTS_TIMEFORMAT+&DTS_UPDOWN
 D$ &DTS_APPCANPARSE+&DTS_RIGHTALIGN+&DTS_SHOWNONE+&DTS_SHORTDATEFORMAT+&DTS_SHORTDATECENTURYFORMAT+&DTS_TIMEFORMAT+&DTS_UPDOWN
 D$ &DTS_APPCANPARSE+&DTS_LONGDATEFORMAT+&DTS_SHOWNONE+&DTS_SHORTDATEFORMAT+&DTS_SHORTDATECENTURYFORMAT+&DTS_TIMEFORMAT+&DTS_UPDOWN
 D$ &DTS_APPCANPARSE+&DTS_LONGDATEFORMAT+&DTS_RIGHTALIGN+&DTS_SHORTDATEFORMAT+&DTS_SHORTDATECENTURYFORMAT+&DTS_TIMEFORMAT+&DTS_UPDOWN
 D$ &DTS_APPCANPARSE+&DTS_LONGDATEFORMAT+&DTS_RIGHTALIGN+&DTS_SHOWNONE+&DTS_SHORTDATECENTURYFORMAT+&DTS_TIMEFORMAT+&DTS_UPDOWN
 D$ &DTS_APPCANPARSE+&DTS_LONGDATEFORMAT+&DTS_RIGHTALIGN+&DTS_SHOWNONE+&DTS_SHORTDATEFORMAT+&DTS_TIMEFORMAT+&DTS_UPDOWN
 D$ &DTS_APPCANPARSE+&DTS_LONGDATEFORMAT+&DTS_RIGHTALIGN+&DTS_SHOWNONE+&DTS_SHORTDATEFORMAT+&DTS_SHORTDATECENTURYFORMAT+&DTS_UPDOWN
 D$ &DTS_APPCANPARSE+&DTS_LONGDATEFORMAT+&DTS_RIGHTALIGN+&DTS_SHOWNONE+&DTS_SHORTDATEFORMAT+&DTS_SHORTDATECENTURYFORMAT+&DTS_TIMEFORMAT
 D$ 0
 D$ 0
 D$ 0
 D$ 0]

[SysDateTimePick32MustHaveBitTable: D$ ? # 12]

[SysDateTimePick32StylesHelp:
 D$ DTS_APPCANPARSEhelp
 D$ DTS_LONGDATEFORMAThelp
 D$ DTS_RIGHTALIGNhelp
 D$ DTS_SHOWNONEhelp
 D$ DTS_SHORTDATEFORMAThelp
 D$ DTS_SHORTDATECENTURYFORMAThelp
 D$ DTS_TIMEFORMAThelp
 D$ DTS_UPDOWNhelp
 D$ WS_BORDERhelp
 D$ WS_TABSTOPhelp
 D$ WS_VSCROLLhelp
 D$ WS_HSCROLLhelp]

[DTS_APPCANPARSEhelp: B$ "DTS_APPCANPARSE:

Allows the owner to parse user input and take necessary action.
It enables users to edit within the client area of the control when they press the F2 key.
The control sends DTN_USERSTRING notification messages when users are finished." EOS]

[DTS_LONGDATEFORMAThelp: B$ "DTS_LONGDATEFORMAT:

Displays the date in long format.
The default format string for this style is defined by LOCALE_SLONGDATEFORMAT, which produces output like 'Friday, April 19, 1996'." EOS]

[DTS_RIGHTALIGNhelp: B$ "DTS_RIGHTALIGN:

The drop-down month calendar will be right-aligned with the control instead of left-aligned, which is the default." EOS]

[DTS_SHOWNONEhelp: B$ "DTS_SHOWNONE:

It is possible to have no date currently selected in the control.
With this style, the control displays a check box that users can check once they have entered or selected a date.
Until this check box is checked, the application will not be able to retrieve the date from the control because, in essence, the control has no date.
This state can be set with the DTM_SETSYSTEMTIME message or queried with the DTM_GETSYSTEMTIME message." EOS]

[DTS_SHORTDATEFORMAThelp: B$ "DTS_SHORTDATEFORMAT:

Displays the date in short format.
The default format string for this style is defined by LOCALE_SSHORTDATE, which produces output like '4/19/96'." EOS]

[DTS_SHORTDATECENTURYFORMAThelp: B$ "DTS_SHORTDATECENTURYFORMAT:

Version 5.80. Similar to the DTS_SHORTDATEFORMAT style, except the year is a four-digit field.
The default format string for this style is based on LOCALE_SSHORTDATE. The output looks like: '4/19/1996'." EOS]

[DTS_TIMEFORMAThelp: B$ "DTS_TIMEFORMAT:

Displays the time. The default format string for this style is defined by LOCALE_STIMEFORMAT, which produces output like '5:31:42 PM'." EOS]

[DTS_UPDOWNhelp: B$ "DTS_UPDOWN:

Places an up-down control to the right of the DTP control to modify date-time values.
This style can be used in place of the drop-down month calendar, which is the default style." EOS]
____________________________________

; SysIPAddress32 . No ipaddress style exists.. we must set the minimum of WS_CHILD.

[SysIPAddress32CheckingMask: D$ ? # 5]

[SysIPAddress32TextTable:
 B$ 'WS_CHILD' EOS
 B$ 'WS_BORDER' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 'WS_VSCROLL' EOS
 B$ 'WS_HSCROLL' EOS
 B$ 0]


[SysIPAddress32BitTable:
 D$ &WS_CHILD
 D$ &WS_BORDER
 D$ &WS_TABSTOP
 D$ &WS_VSCROLL
 D$ &WS_HSCROLL]

[SysIPAddress32ExcludeBitTable: D$ ? # 5]

[SysIPAddress32MustHaveBitTable: D$ ? # 5]

[SysIPAddress32StylesHelp:
 D$ WS_CHILDhelp
 D$ WS_BORDERhelp
 D$ WS_TABSTOPhelp
 D$ WS_VSCROLLhelp
 D$ WS_HSCROLLhelp]
____________________________________

; SysPager

[SysPagerCheckingMask: D$ ? # 9]

[SysPagerTextTable:
 B$ 'PGS_AUTOSCROLL' EOS
 B$ 'PGS_DRAGNDROP' EOS
 B$ 'PGS_HORZ' EOS
 B$ 'PGS_VERT' EOS
 B$ 'WS_CHILD' EOS
 B$ 'WS_BORDER' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 'WS_VSCROLL' EOS
 B$ 'WS_HSCROLL' EOS
 B$ 0]

[SysPagerBitTable:
 D$ &PGS_AUTOSCROLL
 D$ &PGS_DRAGNDROP
 D$ &PGS_HORZ
 D$ &PGS_VERT
 D$ &WS_CHILD
 D$ &WS_BORDER
 D$ &WS_TABSTOP
 D$ &WS_VSCROLL
 D$ &WS_HSCROLL]

[SysPagerExcludeBitTable:
 D$ 0
 D$ 0
 D$ &PGS_VERT
 D$ &PGS_HORZ
 D$ 0
 D$ 0
 D$ 0
 D$ 0
 D$ 0]


[SysPagerMustHaveBitTable: D$ ? # 9]

[SysPagerStylesHelp:
 D$ PGS_AUTOSCROLLhelp
 D$ PGS_DRAGNDROPhelp
 D$ PGS_HORZhelp
 D$ PGS_VERThelp
 D$ WS_CHILDhelp
 D$ WS_BORDERhelp
 D$ WS_TABSTOPhelp
 D$ WS_VSCROLLhelp
 D$ WS_HSCROLLhelp]

[PGS_AUTOSCROLLhelp: B$ "PGS_AUTOSCROLL:

The pager control will scroll when the user hovers the mouse over one of the scroll buttons." EOS]

[PGS_DRAGNDROPhelp: B$ "PGS_DRAGNDROP:

The contained window can be a drag-and-drop target.
The pager control will automatically scroll if an item is dragged from outside the pager over one of the scroll buttons." EOS]

[PGS_HORZhelp: B$ "PGS_HORZ:

Creates a pager control that can be scrolled horizontally.
This style and the PGS_VERT style are mutually exclusive and cannot be combined." EOS]

[PGS_VERThelp: B$ "PGS_VERT:

Creates a pager control that can be scrolled vertically.
This is the default direction if no direction style is specified.
This style and the PGS_HORZ style are mutually exclusive and cannot be combined." EOS]

____________________________________

; SysLink

; Guga note:
; missing the &ICC_LINK_CLASS ? . The dialog is not showing.
; InitDialogEdition:

[SysLinkCheckingMask: D$ ? # 6]

[SysLinkTextTable:
; 'LIS_FOCUSED' EOS  'LIS_ENABLED' EOS   'LIS_VISITED' EOS
 B$ 'WS_VISIBLE' EOS
 B$ 'WS_CHILD' EOS
 B$ 'WS_BORDER' EOS
 B$ 'WS_TABSTOP' EOS
 B$ 'WS_VSCROLL' EOS
 B$ 'WS_HSCROLL' EOS
 B$ 0]

[SysLinkBitTable:
 ;&LIS_FOCUSED   &LIS_ENABLED   &LIS_VISITED
 D$ &WS_VISIBLE
 D$ &WS_CHILD
 D$ &WS_BORDER
 D$ &WS_TABSTOP
 D$ &WS_VSCROLL
 D$ &WS_HSCROLL]

[SysLinkExcludeBitTable: D$ ? # 6]

[SysLinkMustHaveBitTable: D$ ? # 6]

[SysLinkStylesHelp:
 ;LIS_FOCUSEDhelp     LIS_ENABLEDhelp     LIS_VISITEDhelp
 D$ WS_VISIBLEhelp
 D$ WS_CHILDhelp
 D$ WS_BORDERhelp
 D$ WS_TABSTOPhelp
 D$ WS_VSCROLLhelp
 D$ WS_HSCROLLhelp]

[LIS_FOCUSEDhelp: B$ "LIS_FOCUSED:

The link is surrounded by a dashed box. Pressing ENTER launches the link." EOS]

[LIS_ENABLEDhelp: B$ "LIS_ENABLED:

 The link is displayed in blue or purple text, depending on LIS_VISITED. Clicking the link launches it." EOS]

[LIS_VISITEDhelp: B$ "LIS_VISITED:
 
 The link is displayed in purple text. The user has already visited the URL represented by the link." EOS]
____________________________________

[QuestionMark: B$ '?' EOS]
; General routine for showing the checkboxes under the main edition:

SSCkeckBoxes:
    Mov eax 170, ebx 0

    .While B$esi > 0
        Push eax, ebx, esi

            Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                          ButtonClassName,
                                          esi,
                                          &WS_CHILD+&WS_VISIBLE+&BS_AUTOCHECKBOX,
                                          16,
                                          eax,
                                          140,
                                          10,
                                          D$H.DialogEditor,
                                          &NULL, D$H.Instance,
                                          0

            Pop esi, ebx | Push ebx, esi
            Mov D$TABLE.H.DialogControls+ebx eax
            Call 'USER32.SendMessageA' eax &WM_SETFONT D$H.MyFont &TRUE
        Pop esi, ebx, eax

        Push eax, ebx, esi
          ; This is the little Style [?] Buttons:

            Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                          ButtonClassName,
                                          QuestionMark,
                                          &WS_CHILD+&WS_VISIBLE,
                                          2,
                                          eax,
                                          12,
                                          12,
                                          D$H.DialogEditor,
                                          &NULL,
                                          D$H.Instance,
                                          0

            Mov D$TABLE.H.StyleHelpButtons+ebx eax

            Call 'USER32.SendMessageA' eax &WM_SETFONT D$H.MyFont &TRUE
        Pop edi
            Mov al 0, ecx 200 | repne scasb | Mov esi edi
        Pop ebx, eax

        add eax 14 | add ebx 4
    .End_While
ret

____________________________________________________________________________________________

; Little Dialog for viewing the Styles Help:

[HelpDialog: D$ 090CC00C2 0   ;  090CC00C2 0 ; Style
 U$ 01 09 08 0DB 033        ; Dim 01 040 090
 0                          ;      no Menu
 0                          ; Class
 0                          ; Title
 08 'Helv' 0]               ; Font

[HelpDialogEdit: D$
 &WS_VISIBLE__&WS_CHILD__&ES_CENTER__&ES_MULTILINE__&ES_AUTOVSCROLL__&WS_VSCROLL__&WS_BORDER
 0

 U$ 0 0 0DB 033             ; Dim
 01                         ; ID
 0FFFF 081                  ; Class
 0                          ; TitleV.
 0]                         ; No creation data

____________________________________________________________________________________________

Proc HelpDialogProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    .If D@msg = &WM_INITDIALOG
      ; lParam >>> InitValue >>> set Text...
        Call 'USER32.SetDlgItemTextA' D@hwnd, 1, D@lParam

    .Else_If D@msg = &WM_CTLCOLOREDIT
        Call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        Call 'USER32.SendMessageA' D@lParam &EM_SETSEL 0 0
        popad | Mov eax D$H.DialogsBackGroundBrush | jmp L9>>

    .Else_If D@msg = &WM_PARENTNOTIFY
        jmp L4>

    .Else_If D@msg = &WM_LBUTTONDOWN
        jmp L4>

    .Else_If D@msg = &WM_RBUTTONDOWN
        jmp L4>

    .Else_If D@msg = &WM_COMMAND
        Mov eax D@wParam, ebx eax | shr eax 16 | and ebx 0FFFF

        If ebx = &IDCANCEL
            jmp L4>
        Else_If eax = &EN_CHANGE
            jmp L4>
        Else_If eax = &EN_UPDATE
L4:         Call 'USER32.SetFocus' D$H.DialogList
            jmp L5>
        Else_If eax = &EN_KILLFOCUS
L5:         Call 'USER32.EndDialog' D@hwnd 0
        End_If

    .Else
L8:     popad | Mov eax &FALSE | jmp L9>
    .End_If

    popad | Mov eax &TRUE

L9: EndP


[ShowSetOfCheckBoxes | Mov esi #1 | Call SSCkeckBoxes]

[CCFstring: B$
"

  Style settings depend on what Class you choose    
  (line 4 of each control).

  Set the Class first

  " EOS]

[CCFtitle: B$ "Arghhhhhh!!!!... " EOS]


ControlClassFirst:
    Call 'USER32.MessageBoxA' D$H.MainWindow, CCFstring, CCFtitle, &MB_SYSTEMMODAL
ret

; from line pointed by user, searches what class and return in edi:ebx
; adress_of_table: index_of_string:

ShowControlStyleControl:
    Push D$DialogListIndex
       add D$DialogListIndex Line_Class | Call SearchWhatControlClass

       ..If edi = ControlClassByNumber
            If ebx = 0      | ShowSetOfCheckBoxes ButtonTextTable
            Else_If ebx = 1 | ShowSetOfCheckBoxes EditTextTable
            Else_If ebx = 2 | ShowSetOfCheckBoxes StaticTextTable
            Else_If ebx = 3 | ShowSetOfCheckBoxes ListTextTable
            Else_If ebx = 4 | ShowSetOfCheckBoxes ScrollTextTable
            Else_If ebx = 5 | ShowSetOfCheckBoxes ComboTextTable
            Else
                Call ControlClassFirst
            End_If
       ..Else                                                   ; (edi = ControlClassByNames)
            .If ebx = 0      | ShowSetOfCheckBoxes UpDownTextTable
            .Else_If ebx = 1 | ShowSetOfCheckBoxes ProgressTextTable
            .Else_If ebx = 2 | ShowSetOfCheckBoxes TrackTextTable
            .Else_If ebx = 3 | ShowSetOfCheckBoxes TreeTextTable
            .Else_If ebx = 4 | ShowSetOfCheckBoxes TabTextTable
            .Else_If ebx = 5 | ShowSetOfCheckBoxes ListViewTextTable
            .Else_If ebx = 6 | ShowSetOfCheckBoxes ToolBarTextTable
            .Else_If ebx = 7 | ShowSetOfCheckBoxes RichEdit20aTextTable
            .Else_If ebx = 8 | ShowSetOfCheckBoxes SysHeader32TextTable
            .Else_If ebx = 9 | ShowSetOfCheckBoxes ReBarWindow32TextTable
            .Else_If ebx = 10 | ShowSetOfCheckBoxes Tooltips_class32TextTable
            .Else_If ebx = 11 | ShowSetOfCheckBoxes msctls_statusbar32TextTable
            .Else_If ebx = 12 | ShowSetOfCheckBoxes msctls_hotkey32TextTable
            .Else_If ebx = 13 | ShowSetOfCheckBoxes ComboBoxEx32TextTable
            .Else_If ebx = 14 | ShowSetOfCheckBoxes SysAnimate32TextTable
            .Else_If ebx = 15 | ShowSetOfCheckBoxes SysMonthCal32TextTable
            .Else_If ebx = 16 | ShowSetOfCheckBoxes SysDateTimePick32TextTable
            .Else_If ebx = 17 | ShowSetOfCheckBoxes SysIPAddress32TextTable
            .Else_If ebx = 18 | ShowSetOfCheckBoxes SysPagerTextTable
            .Else_If ebx = 19 | ShowSetOfCheckBoxes SysLinkTextTable
            .End_If
       ..End_If

    Pop D$DialogListIndex
ret

[ID_Message: B$ "==== Set the ID first ====





==== Mouse Edition ====


    Move: Left button.

    Resize: Right Button." EOS]

ShowIDcontrols:
    Call SearchDialogLine | Mov esi edi | Call TranslateDialogHexa
    Push ebx

        Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                      EditClass,
                                      &NULL,
                                      &WS_CHILD+&WS_VISIBLE+&WS_BORDER+&ES_NUMBER+&ES_RIGHT+&ES_MULTILINE,
                                      80,
                                      200,
                                      45,
                                      20,
                                      D$H.DialogEditor,
                                      &NULL,
                                      D$H.Instance,
                                      0

        Mov D$TABLE.H.DialogControls eax

        Call 'USER32.GetDlgCtrlID' eax

    Pop ebx

    Call 'USER32.SetDlgItemInt' D$H.DialogEditor, eax, ebx, &FALSE

    Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  EditClass,
                                  &NULL,
                                  &WS_CHILD+&WS_VISIBLE+&ES_MULTILINE+&ES_READONLY,
                                  4,
                                  250,
                                  140,
                                  200,
                                  D$H.DialogEditor,
                                  &NULL,
                                  D$H.Instance,
                                  0

    Mov D$TABLE.H.DialogControls+4 eax

    Call 'USER32.SendMessageA' eax, &WM_SETTEXT, 0, ID_Message

    Call 'USER32.SendMessageA' D$TABLE.H.DialogControls+4, &WM_SETFONT, D$H.MyFont, &TRUE


    Call 'USER32.SetFocus' D$TABLE.H.DialogControls

    Call 'USER32.SendMessageA' D$TABLE.H.DialogControls ,&EM_SETSEL, 0, 0-1
ret

[ControlClassByNumber:
 B$ 'Button' EOS
 B$ 'Edit control' EOS
 B$ 'Static Control' EOS
 B$ 'ListBox' EOS
 B$ 'ScrollBar' EOS
 B$ 'ComboBox' EOS
 B$ 0]

;[D_button 080  D_Edit 081  D_Static 082  D_ListBox 083  D_ScrollBar 084  D_ComboBox 085]

[ControlClassByNames:
 B$ 'msctls_updown32' EOS
 B$ 'msctls_progress32' EOS
 B$ 'msctls_trackbar32' EOS
 B$ 'SysTreeView32' EOS
 B$ 'SysTabControl32' EOS
 B$ 'SysListView32' EOS
 B$ 'ToolbarWindow32' EOS,
 B$ 'RichEdit20A' EOS
 B$ 'SysHeader32' EOS
 B$ 'ReBarWindow32' EOS
 B$ 'tooltips_class32' EOS
 B$ 'msctls_statusbar32' EOS
 B$ 'msctls_hotkey32' EOS
 B$ 'ComboBoxEx32' EOS
 B$ 'SysAnimate32' EOS
 B$ 'SysMonthCal32' EOS
 B$ 'SysDateTimePick32' EOS
 B$ 'SysIPAddress32' EOS
 B$ 'SysPager' EOS
 B$ 'SysLink' EOS
 B$ 0]

[ActualClassName: D$ ? # 10]

; From line pointed by user, searches what class and return in edi:ebx
; adress_of_table: index_of_string. ebx value is 0 to 5, edi either "ControlClassByNumber"
; or "ControlClassByNumber" (6 classes 'by number', 6 classes 'by name'):

SearchWhatControlClass:
    Call SearchDialogLine
    ..If D$edi = 'FFFF'
        Mov esi edi | add esi 5 | Call TranslateDialogHexa  ; > ebx = 080 / 081 / ...
        sub ebx 080 | Mov edi ControlClassByNumber          ; > ebx = 0 / 1 / 2...
    ..Else
        Mov eax D$edi+8
        .If D$edi+8 = 'updo'         ; msctls_updown32
            Mov ebx 0
        .Else_If D$edi+8 = 'prog'    ; msctls_progress32
            Mov ebx 1
        .Else_If D$edi+8 = 'trac'    ; msctls_trackbar32
            Mov ebx 2
        .Else_If D$edi+4 = 'Tree'    ; SysTreeView32
            Mov ebx 3
        .Else_If D$edi+4 = 'List'    ; SysListView32
            Mov ebx 5
        .Else_If D$edi+8 = 'ontr'    ; SysTabControl32
            Mov ebx 4
        .Else_If D$edi+1 = 'Tool'    ; ToolbarWindow32
            Mov ebx 6
        .Else_If D$edi+8 = 't20A'    ; RichEdit20a
            Mov ebx 7
        .Else_If D$edi+1 = 'SysH'    ; SysHeader32
            Mov ebx 8
        .Else_If D$edi+1 = 'ReBa'   ; ReBarWindow32
            Mov ebx 9
        .Else_If D$edi+5 = 'tips'  ;tooltips_class32
            Mov ebx 10
        .Else_If D$edi+8 = 'stat' ; msctls_statusbar32
            Mov ebx 11
        .Else_If D$edi+8 = 'hotk'; msctls_hotkey32
            Mov ebx 12
        .Else_If D$edi+1 = 'Comb' ;ComboBoxEx32
            Mov ebx 13
        .Else_If D$edi+1 = 'SysA'; SysAnimate32
            Mov ebx 14
        .Else_If D$edi+1 = 'SysM' ;SysMonthCal32
            Mov ebx 15
        .Else_If D$edi+1 = 'SysD' ;SysDateTimePick32
            Mov ebx 16
        .Else_If D$edi+1 = 'SysI' ;SysIPAddress32
            Mov ebx 17
        .Else_If D$edi+1 = 'SysP'  ;SysPager
            Mov ebx 18
        .Else_If D$edi+1 = 'SysL'  ; SysLink
            Mov ebx 19
        .End_If

      Mov edi ControlClassByNames
    ..End_If
ret


ShowClassControls:

    Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  ComboClass,
                                  &NULL,
                                  &WS_CHILD+&WS_VISIBLE+&WS_BORDER+&CBS_HASSTRINGS+&CBS_AUTOHSCROLL+&CBS_DROPDOWNLIST+&WS_VSCROLL+&ES_AUTOVSCROLL,
                                  2,
                                  200,
                                  145,
                                  320,
                                  D$H.DialogEditor,
                                  &NULL,
                                  D$H.Instance,
                                  0

    Mov D$TABLE.H.DialogControls eax

    ; Copy data font text (without quotes and comments) in TitleEditText:

    Call SearchWhatControlClass

    Mov al 0, ecx 0FFFF
    While ebx > 0                       ; setting edi > start of whatever class
        repne scasb | dec ebx           ; name, either by number or by name
    End_While
    Mov esi edi, ecx 200 , al 0
    Mov edi ActualClassName

L0: lodsb | stosb | cmp al 0 | ja L0<   ; copying actual name for next 'CB_SELECTSTRING'

    Mov edi ControlClassByNumber        ; build list name from class by numbers:
    While B$edi > 0
        Push edi
            Call 'USER32.SendMessageA' D$TABLE.H.DialogControls &CB_ADDSTRING 0  edi
        Pop edi
        Mov al 0, ecx 200 | repne scasb
    End_While

    Mov edi ControlClassByNames         ; build list name from class by names:
    While B$edi > 0
        Push edi
            Call 'USER32.SendMessageA' D$TABLE.H.DialogControls &CB_ADDSTRING 0  edi
        Pop edi
        Mov al 0, ecx 200 | repne scasb
    End_While

  ; setting actual choice in edit control of ComboBox:
    Call 'USER32.SendMessageA' D$TABLE.H.DialogControls &CB_SELECTSTRING  0  ActualClassName
ret
____________________________________________________________________________________

; used by many 'user modifications' writting inside the edited template:

SearchDialogLine:
    Mov ecx MaxTemplateText, edi D$NewDialogTemplateText, ebx 0, al 0

    While ebx < D$DialogListIndex
        repne scasb | inc ebx
    End_While
ret


NoDialogMenu:
    Call 'USER32.MessageBoxA' EOS, {B$ 'There is no Menu in this Dialog' EOS},
                              {B$ ' Dialog Menu' EOS}, &MB_SYSTEMMODAL
ret


; At same time: reset the table of controls handles and close these controls:

KillPreviousDialogControls:
    Mov esi TABLE.H.DialogControls
    While D$esi > 0
        lodsd | Mov D$esi-4 0
        Push esi
            Call 'USER32.DestroyWindow' eax
        Pop esi
    End_While

    Mov esi TABLE.H.StyleHelpButtons
    While D$esi > 0
        lodsd | Mov D$esi-4 0
        Push esi
            Call 'USER32.DestroyWindow' eax
        Pop esi
    End_While
ret

;;
;[NewMenuForDialog: B$ "
;    Do you want to create a new menu?    
;     " EOS]

OldAddMenuToDialog:
    Mov esi MenuList

   ..If D$esi = 0                               ; no Menu:
        Call 'USER32.MessageBoxA'  0  NewMenuForDialog  argh,
                                &MB_SYSTEMMODAL+&MB_ICONQUESTION+&MB_YESNO
        If eax = &IDYES
            Call NewMenu
            Mov eax D$MenuListPtr               ; either 0 or ID
        Else
            Mov eax 0
        End_If
   ..Else                                       ; menu(s) exist in rsrc:
        Call 'USER32.MessageBoxA'  0  NewMenuForDialog  argh,
                                &MB_SYSTEMMODAL+&MB_ICONQUESTION+&MB_YESNO
        .If eax = &IDYES
            Call NewMenu | Mov D$ActualMenutestID 0
            Mov esi D$MenuListPtr
            If D$esi = 0
                Mov eax 0
            Else
                Mov eax D$MenuListPtr
            End_If
        .Else
            Mov D$MenuListPtr MenuList,  B$UserTellWhatMenu &FALSE
            While B$UserTellWhatMenu = &FALSE   ; even if only 1 menu because we do
                Call WhatMenu                   ; not set here the true user menu ID but
            End_While                           ; intead the 'D$ActualMenutestID'
            If D$MenuListPtr = 0
                Mov eax 0
            Else
                Mov eax D$MenuListPtr
            End_If
        .End_If
   ..End_If

   ; now, eax = Menu ID or 0.
   .If eax = 0
        Mov D$ActualMenutestID 0, D$DialogMenuTrueID 0
   .Else
        Move D$DialogMenuTrueID D$eax
        If D$DialogMenuTrueID <> 0              ; happends if user abort menu edition
            add eax 4 | Call 'USER32.LoadMenuIndirectA' D$eax ; menu Id (from resources)
            Mov D$ActualMenutestID eax
            Call SearchDialogLine
            Mov eax 'FFFF' | stosd | Mov al SPC | stosb
            Mov ebx D$DialogMenuTrueID
            Call TranslateDialogText4 | Mov al SPC | stosb | Mov ax '; ' | stosw
            Call FromTextToBinTemplate | Call ShowDialogResult | Call FillDialogListBox
        End_If
   .End_If
ret
;;

AddMenuToDialog:
    Call 'USER32.DialogBoxParamA' D$H.Instance 31555 &NULL NewOrExistingMenuProc &NULL

    .If B$NewOrExistingMenuChoice = 6           ; New.
        Call NewMenu | Mov D$ActualMenutestID 0
        Mov esi D$MenuListPtr
        If D$esi = 0
            Mov eax 0
        Else
            Mov eax D$MenuListPtr
        End_If

    .Else_If B$NewOrExistingMenuChoice = 5       ; Existing.
        Mov D$MenuListPtr MenuList,  B$UserTellWhatMenu &FALSE
        While B$UserTellWhatMenu = &FALSE   ; even if only 1 menu because we do
            Call WhatMenu                   ; not set here the true user menu ID but
        End_While                           ; intead the 'D$ActualMenutestID'
        If D$MenuListPtr = 0
            Mov eax 0
        Else
            Mov eax D$MenuListPtr
        End_If

    .End_If

 ; now, eax = Menu ID or 0.
   .If eax = 0
        Mov D$ActualMenutestID 0, D$DialogMenuTrueID 0
   .Else
        Move D$DialogMenuTrueID D$eax
        If D$DialogMenuTrueID <> 0              ; happends if user abort menu edition
            add eax 4 | Call 'USER32.LoadMenuIndirectA' D$eax ; menu Id (from resources)
            Mov D$ActualMenutestID eax
            Call SearchDialogLine
            Mov eax 'FFFF' | stosd | Mov al SPC | stosb
            Mov ebx D$DialogMenuTrueID
            Call TranslateDialogText4 | Mov al SPC | stosb | Mov ax '; ' | stosw
            Call FromTextToBinTemplate | Call ShowDialogResult | Call FillDialogListBox
        End_If
   .End_If
ret


[NewOrExistingMenuChoice: D$ ?]

Proc NewOrExistingMenuProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_INITDIALOG
      ; If no Menu >>> Disable the [Load Existing] Button:
        Mov esi MenuList
        If D$esi = 0
            Call 'USER32.GetDlgItem' D@hwnd, 5
            Call 'USER32.EnableWindow' eax &FALSE
        End_If

    ...Else_If D@msg = &WM_COMMAND
        Mov eax D@wParam | and D@wParam 0FFFF | shr eax 16

        If D@wParam = &IDCANCEL
            Mov B$NewOrExistingMenuChoice 0
            Call 'USER32.DestroyWindow' D@hwnd

        Else_If D@wParam = 5            ; Existing.
            Mov B$NewOrExistingMenuChoice 5
            Call 'USER32.DestroyWindow' D@hwnd

        Else_If D@wParam = 6            ; New.
            Mov B$NewOrExistingMenuChoice 6
            Call 'USER32.DestroyWindow' D@hwnd

        Else_If D@wParam = &IDHELP
            Call Help, B_U_AsmName, DialogHelp, ContextHlpMessage

        End_If

    ...Else
L8:     popad | Mov eax &FALSE | jmp L9>

    ...End_If

    popad | Mov eax &TRUE
L9: EndP
____________________________________________________________________________________________



EditDialogMenu:
    Push D$DialogMenuTrueID
        Mov esi MenuList, eax D$DialogMenuTrueID
        While D$esi <> eax
            add esi 12
        End_While
        Mov D$MenuListPtr esi | Call ReEditExistingMenu
    Pop eax

    If D$uMenu_ID <> eax
      Move D$DialogMenuTrueID D$uMenu_ID
    End_If

    Mov eax D$MenuListPtr | add eax 4          ; ptr to menu data
    Call 'USER32.LoadMenuIndirectA' D$eax | Mov D$ActualMenutestID eax
    Call SearchDialogLine
    Mov eax 'FFFF' | stosd | Mov al SPC | stosb
    Mov ebx D$DialogMenuTrueID
    Call TranslateDialogText4 | Mov al SPC | stosb | Mov ax '; ' | stosw
    Call FromTextToBinTemplate | Call ShowDialogResult | Call FillDialogListBox
ret

______________________________________________________________________________________
;;
 Setting of the wished controls under main list box for editing user dialog. (User
 have click on some item >>> which one >>> what controls to be set). As these controls
 may be many or few (or none), i choose a dynamic storage for the handles, whatever
 they are: One simgle table ("TABLE.H.DialogControls") is used to store all the handles
 as they come, without any visible symbolic name for further identification. To know
 what user is doing, we use the index (selected item) of the main list. In some cases
 (Dialog Callback checking), we scan the number of handles written in the List.
;;

[DialogListIndex: D$ ?           ; true 0 based line number in main list
 ControlIndex: D$ ?]             ; 0 based index of line in one control

[NoDialogControl | Mov D$DialogListIndex 0_FFFF_FFFF | Mov D$ControlIndex 0_FFFF_FFFF]

[EditingMenuFromDialog: D$ ?]

SetDialogTools:
    Call KillPreviousDialogControls
    NoDialogControl
    Call 'USER32.SendMessageA' D$H.DialogList, &LB_GETCURSEL, eax, 0
    Mov D$DialogListIndex eax

    ..If eax < Line_empty
        Mov D$ControlIndex eax
        .If eax = Line_Style
            Call ShowDialogStyleControl | Call ShowDialogStyles
        .Else_If eax = Line_Dim
            Mov D$DimIsForDialogWindow &TRUE | Call ShowDimControls
        .Else_If eax = Line_ID
            Call SearchDialogLine
            If D$edi = 'FFFF'
                Call EditDialogMenu
            Else
                Call AddMenuToDialog
            End_If
        .Else_If eax = Line_Class
            Call ShowTitleControl          ; reuse for dialog class
         ;  NoDialogControl  ; not yet
        .Else_If eax = Line_Class
            NoDialogControl  ; not yet
        .Else_If eax = Line_Title
            Call ShowTitleControl
        .Else_If eax = Line_Font
            Call ShowFontControls
        .End_If
    ..Else_If eax = Line_empty
        NoDialogControl    ; separator
    ..Else_If eax < D$LastDialogListItem
L1:     sub eax Line_empty+1 | cmp eax Line_empty+1 | jae L1<
        Mov D$ControlIndex eax
        If eax = Line_Style
            Call ShowControlStyleControl | Call ShowControlStyles
        Else_If eax = Line_ExStyle
            NoDialogControl   ; not yet
        Else_If eax = Line_Dim
            Mov D$DimIsForDialogWindow &FALSE | Call ShowDimControls
        Else_If eax = Line_ID
            Call ShowIDcontrols
        Else_If eax = Line_Class
            Call ShowClassControls
        Else_If eax = Line_Title
            Call ShowTitleControl
        Else_If eax = Line_Font
            NoDialogControl   ; not yet
        End_If
    ..End_If
ret

____________________________________________________________________________________
____________________________________________________________________________________

; Showing user modifications result:
____________________________________________________________________________________
____________________________________________________________________________________

[NextTemplateLine | xchg esi edi | Mov al 0 | Mov ecx 200 | repne scasb | xchg esi edi]

[ControlNumberPtr: D$ ?]            ; user doesn't have to set this record (before Dims)

; Translate the text template (visible in Editor Edit Control) into a Binary Template
; run by RosAsm (without the menu and the class, if any). (see 'DialogMenuComment')
;
; 'FromBinToTextTemplate' doesn't do the reverse operation...

FromTextToBinTemplate:
    Mov esi D$NewDialogTemplateText, edi D$EditedDialogBoxData

    add esi 3 | Call TranslateDialogHexa | Mov eax ebx | stosd  ; Style
    NextTemplateLine
    add esi 3 | Call TranslateDialogHexa | Mov eax ebx | stosd  ; extended style
    NextTemplateLine | add esi 3
    Mov D$ControlNumberPtr edi                                  ; used down there to
    Call TranslateDialogHexa | Mov eax 0 | stosw                ; set dummy control number
    Call TranslateDialogHexa | Mov eax ebx | stosw              ; X pos
    Call TranslateDialogHexa | Mov eax ebx | stosw              ; Y pos
    Call TranslateDialogHexa | Mov eax ebx | stosw              ; width
    Call TranslateDialogHexa | Mov eax ebx | stosw              ; hight
    NextTemplateLine | Call TranslateDialogHexa
  ; Why the commented out lines? >>> DialogMenuComment
  ;  If ebx = 0
      Mov ax 0 | stosw                                          ; no menu
  ;  Else
  ;    Mov eax ebx | stosw                                      ; 0FFFF in ebx > menu
  ;    Call TranslateDialogHexa
  ;    Mov eax ebx | stosw                                      ; menu ID
  ;  End_If
    NextTemplateLine | inc esi
    Mov eax 0 | stosw                                           ; no class in edition
  ;  Mov al 0FF
    Push edi
        Mov edi ClassRecord
L0:     cmp B$esi '"' | je L1>
        cmp B$esi "'" | je L1>
            lodsb | stosb                                       ; preserve Class for savings
        jmp L0<
L1:     Mov eax 0 | stosd                                       ; end mark at our string
    Pop edi
    NextTemplateLine | inc esi | Mov eax 0
L0: cmp B$esi '"' | je L1>
    cmp B$esi "'" | je L1>
        lodsb | stosw                                            ; title
    jmp L0<
L1: Mov ax 0 | stosw |  NextTemplateLine
    Call TranslateDialogHexa | Mov eax ebx | stosw               ; font size
    inc esi | Mov ax 0
L0: cmp B$esi '"' | je L1>
    cmp B$esi "'" | je L1>
        lodsb | stosw                                            ; font name
    jmp L0<
L1: Mov ax 0 | stosw
    Test edi 00_11 ZERO L1>                                      ; Dword aligned?
        stosw                                                    ; no > align!
L1: NextTemplateLine | NextTemplateLine                          ; + 1 blank line in text

    .While B$esi <> 255                                          ; Controls data:
        Mov ebx D$ControlNumberPtr | inc W$ebx
        add esi 3
        Call TranslateDialogHexa | Mov eax ebx | stosd           ; style
        NextTemplateLine | add esi 3
        Call TranslateDialogHexa | Mov eax ebx | stosd           ; extended style
        NextTemplateLine | add esi 3

        Call TranslateDialogHexa | Mov eax ebx | stosw           ; X pos
        Call TranslateDialogHexa | Mov eax ebx | stosw           ; Y pos
        Call TranslateDialogHexa | Mov eax ebx | stosw           ; width
        Call TranslateDialogHexa | Mov eax ebx | stosw           ; hight
        NextTemplateLine
        Call TranslateDialogHexa | Mov eax ebx | stosw           ; ID
        NextTemplateLine
        If D$esi = 'FFFF'                                        ; class by number:
            Mov ax 0FFFF | stosw | add  esi 5
            Call TranslateDialogHexa | Mov eax ebx | stosw       ; class
        Else
            inc esi | Mov eax 0
L0:         cmp B$esi '"' | je L1>
            cmp B$esi "'" | je L1>
            lodsb | stosw
            jmp L0<
L1:         Mov ax 0 | stosw
        End_If
        NextTemplateLine | inc esi | Mov ax 0
L0:     cmp B$esi '"' | je L1>
        cmp B$esi "'" | je L1>
            lodsb | stosw                                       ; title
        jmp L0<
L1:     Mov ax 0 | stosw                                        ; end mark
        NextTemplateLine | stosw                                ; no creation dat
        Test edi 00_11 ZERO L1>                                 ; Dword aligned?
            stosw                                               ; no > align!
L1:     NextTemplateLine  | NextTemplateLine                    ; + 1 blank line in text

    .End_While
ret
______________________________________________________________________________________

; User have click some 'Style' CheckBox. Called by 'EditDialogBoxProc' with
;  buttons handles in ecx, tables index in ebx

[H.ClickedCheckBox: D$ ?
 FlagCheck: D$ ?]

WriteStyle:
    Mov eax D$DialogListIndex, D$H.ClickedCheckBox ecx

    ...If eax = D$ControlIndex              ; >>> this is for Dialog Style:
        Move D$CheckMust D$DialogMustHaveBitTable+ebx
        Move D$CheckExclude D$DialogExcludeBitTable+ebx
        Move D$CheckingMask DialogCheckingMask
        Move D$CheckBit D$DialogBitTable+ebx
        Mov esi DialogBitTable

    ...Else                                  ; >>> this is for Control Style:
        Push ebx                                ; table indice (0 / 4 / 8 / 12...)
            add D$DialogListIndex Line_Class
            Call SearchWhatControlClass             ; ebx = indice of class (0 / 1 / 2 / 3...)
            sub D$DialogListIndex Line_Class
        Pop eax                                       ; table indice in eax
        ..If edi = ControlClassByNumber               ; >>> class by Number
            .If ebx = 0
                Move D$CheckMust D$ButtonMustHaveBitTable+eax      ; Button
                Move D$CheckExclude D$ButtonExcludeBitTable+eax
                Move D$CheckingMask ButtonCheckingMask
                Move D$CheckBit D$ButtonBitTable+eax
                Mov esi ButtonBitTable
            .Else_If ebx = 1
                Move D$CheckMust D$EditMustHaveBitTable+eax        ; Edit control
                Move D$CheckExclude D$EditExcludeBitTable+eax
                Move D$CheckingMask EditCheckingMask
                Move D$CheckBit D$EditBitTable+eax
                Mov esi EditBitTable
            .Else_If ebx = 2
                Move D$CheckMust D$StaticMustHaveBitTable+eax      ; Static Control
                Move D$CheckExclude D$StaticExcludeBitTable+eax
                Move D$CheckingMask StaticCheckingMask
                Move D$CheckBit D$StaticBitTable+eax
                Mov esi StaticBitTable
            .Else_If ebx = 3
                Move D$CheckMust D$ListMustHaveBitTable+eax        ; ListBox
                Move D$CheckExclude D$ListExcludeBitTable+eax
                Move D$CheckingMask ListCheckingMask
                Move D$CheckBit D$ListBitTable+eax
                Mov esi ListBitTable
            .Else_If ebx = 4
                Move D$CheckMust D$ScrollMustHaveBitTable+eax      ; ScrollBar
                Move D$CheckExclude D$ScrollExcludeBitTable+eax
                Move D$CheckingMask ScrollCheckingMask
                Move D$CheckBit D$ScrollBitTable+eax
                Mov esi ScrollBitTable
            .Else_If ebx = 5
                Move D$CheckMust D$ComboMustHaveBitTable+eax       ; ComboBox
                Move D$CheckExclude D$ComboExcludeBitTable+eax
                Move D$CheckingMask ComboCheckingMask
                Move D$CheckBit D$ComboBitTable+eax
                Mov esi ComboBitTable
            .End_If
        ..Else                                         ; >>> class by Name
            .If ebx = 0
                Move D$CheckMust D$UpDownMustHaveBitTable+eax      ; msctls_updown32
                Move D$CheckExclude D$UpDownExcludeBitTable+eax
                Move D$CheckingMask UpDownCheckingMask
                Move D$CheckBit D$UpDownBitTable+eax
                Mov esi UpDownBitTable
            .Else_If ebx = 1
                ret                                    ; msctls_progress32 (no controls)
            .Else_If ebx = 2
                Move D$CheckMust D$TrackMustHaveBitTable+eax       ; msctls_trackbar32
                Move D$CheckExclude D$TrackExcludeBitTable+eax
                Move D$CheckingMask TrackCheckingMask
                Move D$CheckBit D$TrackBitTable+eax
                Mov esi TrackBitTable
            .Else_If ebx = 3
                Move D$CheckMust D$TreeMustHaveBitTable+eax        ; SysTreeView32
                Move D$CheckExclude D$TreeExcludeBitTable+eax
                Move D$CheckingMask TreeCheckingMask
                Move D$CheckBit D$TreeBitTable+eax
                Mov esi TreeBitTable
            .Else_If ebx = 4
                Move D$CheckMust D$TabMustHaveBitTable+eax         ; SysTabControl32
                Move D$CheckExclude D$TabExcludeBitTable+eax
                Move D$CheckingMask TabCheckingMask
                Move D$CheckBit D$TabBitTable+eax
                Mov esi TabBitTable
            .Else_If ebx = 5
                Move D$CheckMust D$ListViewMustHaveBitTable+eax    ; SysListView32
                Move D$CheckExclude D$ListViewExcludeBitTable+eax
                Move D$CheckingMask ListViewCheckingMask
                Move D$CheckBit D$ListViewBitTable+eax
                Mov esi ListViewBitTable
            .Else_If ebx = 6
                Move D$CheckMust D$ToolBarMustHaveBitTable+eax    ; ToolbarWindow32
                Move D$CheckExclude D$ToolBarExcludeBitTable+eax
                Move D$CheckingMask ToolBarCheckingMask
                Move D$CheckBit D$ToolBarBitTable+eax
                Mov esi ToolBarBitTable

            .Else_If ebx = 7
                Move D$CheckMust D$RichEdit20aMustHaveBitTable+eax    ; RichEdit20a
                Move D$CheckExclude D$RichEdit20aExcludeBitTable+eax
                Move D$CheckingMask RichEdit20aCheckingMask
                Move D$CheckBit D$RichEdit20aBitTable+eax
                Mov esi RichEdit20aBitTable

            .Else_If ebx = 8
                Move D$CheckMust D$SysHeader32MustHaveBitTable+eax    ; SysHeader32
                Move D$CheckExclude D$SysHeader32ExcludeBitTable+eax
                Move D$CheckingMask SysHeader32CheckingMask
                Move D$CheckBit D$SysHeader32BitTable+eax
                Mov esi SysHeader32BitTable

            .Else_If ebx = 9
                Move D$CheckMust D$ReBarWindow32MustHaveBitTable+eax    ; ReBarWindow32
                Move D$CheckExclude D$ReBarWindow32ExcludeBitTable+eax
                Move D$CheckingMask ReBarWindow32CheckingMask
                Move D$CheckBit D$ReBarWindow32BitTable+eax
                Mov esi ReBarWindow32BitTable

            .Else_If ebx = 10
                Move D$CheckMust D$tooltips_class32MustHaveBitTable+eax    ; tooltips_class32
                Move D$CheckExclude D$tooltips_class32ExcludeBitTable+eax
                Move D$CheckingMask tooltips_class32CheckingMask
                Move D$CheckBit D$tooltips_class32BitTable+eax
                Mov esi tooltips_class32BitTable

            .Else_If ebx = 11
                Move D$CheckMust D$msctls_statusbar32MustHaveBitTable+eax    ; msctls_statusbar32
                Move D$CheckExclude D$msctls_statusbar32ExcludeBitTable+eax
                Move D$CheckingMask msctls_statusbar32CheckingMask
                Move D$CheckBit D$msctls_statusbar32BitTable+eax
                Mov esi msctls_statusbar32BitTable

            .Else_If ebx = 12
                Move D$CheckMust D$msctls_hotkey32MustHaveBitTable+eax    ; msctls_hotkey32
                Move D$CheckExclude D$msctls_hotkey32ExcludeBitTable+eax
                Move D$CheckingMask msctls_hotkey32CheckingMask
                Move D$CheckBit D$msctls_hotkey32BitTable+eax
                Mov esi msctls_hotkey32BitTable

            .Else_If ebx = 13
                Move D$CheckMust D$ComboBoxEx32MustHaveBitTable+eax    ; ComboBoxEx32
                Move D$CheckExclude D$ComboBoxEx32ExcludeBitTable+eax
                Move D$CheckingMask ComboBoxEx32CheckingMask
                Move D$CheckBit D$ComboBoxEx32BitTable+eax
                Mov esi ComboBoxEx32BitTable

            .Else_If ebx = 14
                Move D$CheckMust D$SysAnimate32MustHaveBitTable+eax    ; SysAnimate32
                Move D$CheckExclude D$SysAnimate32ExcludeBitTable+eax
                Move D$CheckingMask SysAnimate32CheckingMask
                Move D$CheckBit D$SysAnimate32BitTable+eax
                Mov esi SysAnimate32BitTable

            .Else_If ebx = 15
                Move D$CheckMust D$SysMonthCal32MustHaveBitTable+eax    ; SysMonthCal32
                Move D$CheckExclude D$SysMonthCal32ExcludeBitTable+eax
                Move D$CheckingMask SysMonthCal32CheckingMask
                Move D$CheckBit D$SysMonthCal32BitTable+eax
                Mov esi SysMonthCal32BitTable

            .Else_If ebx = 16
                Move D$CheckMust D$SysDateTimePick32MustHaveBitTable+eax    ; SysDateTimePick32
                Move D$CheckExclude D$SysDateTimePick32ExcludeBitTable+eax
                Move D$CheckingMask SysDateTimePick32CheckingMask
                Move D$CheckBit D$SysDateTimePick32BitTable+eax
                Mov esi SysDateTimePick32BitTable

            .Else_If ebx = 17
                Move D$CheckMust D$SysIPAddress32MustHaveBitTable+eax    ; SysIPAddress32
                Move D$CheckExclude D$SysIPAddress32ExcludeBitTable+eax
                Move D$CheckingMask SysIPAddress32CheckingMask
                Move D$CheckBit D$SysIPAddress32BitTable+eax
                Mov esi SysIPAddress32BitTable

            .Else_If ebx = 18
                Move D$CheckMust D$SysPagerMustHaveBitTable+eax    ; SysPager
                Move D$CheckExclude D$SysPagerExcludeBitTable+eax
                Move D$CheckingMask SysPagerCheckingMask
                Move D$CheckBit D$SysPagerBitTable+eax
                Mov esi SysPagerBitTable

            .Else_If ebx = 19
                Move D$CheckMust D$SysLinkMustHaveBitTable+eax    ; SysLink
                Move D$CheckExclude D$SysLinkExcludeBitTable+eax
                Move D$CheckingMask SysLinkCheckingMask
                Move D$CheckBit D$SysLinkBitTable+eax
                Mov esi SysLinkBitTable
            .End_If
        ..End_If

    ...End_If

    Push esi
        Call SearchDialogLine | add edi 3           ; edi > "NewDialogTemplateText": Style
        Mov esi edi | Call TranslateDialogHexa      ; Style value in ebx
    Pop esi
;;
 Now: D$CheckBit = bit value of the clicked checkbox
      ebx            = previous value for style
      D$CheckExclude = Excluded Bits
      D$CheckMust    = Must Have Bits
      esi            = ptr to Bit table
      edi            = "NewDialogTemplateText" concerned line +3 ("D$ "xxxxxxxx)

 We take care of "required" / "exclude" bits only if we are setting the bits:
;;
    Push esi, edi, ebx
        Call 'USER32.SendMessageA' D$H.ClickedCheckBox &BM_GETCHECK 0 0
        Mov D$FlagCheck eax
    Pop ebx, edi, esi


    On B$FlagCheck = &FALSE, jmp L1>                    ; Only if user is "checking"
        or ebx D$CheckExclude | xor ebx D$CheckExclude  ; excluded bit(s)
        or ebx D$CheckMust                              ; required bit(s)
L1: xor ebx D$CheckBit                                  ; new set bit

; or ebx &WS_THICKFRAME  (for my tests)

    Mov D$CheckActual ebx
    pushad
        Call TranslateDialogText8  ; write ebx (text) at "NewDialogTemplateText": Style
    popad

    Call CheckControlStyles

    Call FromTextToBinTemplate | Call ShowDialogResult | Call FillDialogListBox
ret

____________________________________________________________________________________________

; esi just found a valid [?] Help Butoon Handle:

ShowStyleInfo:
    .If D$CheckingMask = DialogCheckingMask
        Mov eax DialogStylesHelp
    .Else_If D$CheckingMask = ButtonCheckingMask
        Mov eax ButtonStylesHelp
    .Else_If D$CheckingMask = EditCheckingMask
        Mov eax EditStylesHelp
    .Else_If D$CheckingMask = StaticCheckingMask
        Mov eax StaticStylesHelp
    .Else_If D$CheckingMask = ListCheckingMask
        Mov eax ListBoxStyles
    .Else_If D$CheckingMask = ScrollCheckingMask
        Mov eax ScrollStylesHelp
    .Else_If D$CheckingMask = ComboCheckingMask
        Mov eax ComboStylesHelp
    .Else_If D$CheckingMask = UpDownCheckingMask
        Mov eax UpDownStylesHelp
    .Else_If D$CheckingMask = TrackCheckingMask
        Mov eax TrackStylesHelp
    .Else_If D$CheckingMask = TreeCheckingMask
        Mov eax TreeViewStylesHelp
    .Else_If D$CheckingMask = TabCheckingMask
        Mov eax TabStylesHelp
    .Else_If D$CheckingMask = ListViewCheckingMask
        Mov eax ListViewStylesHelp
    .Else_If D$CheckingMask = ToolBarCheckingMask
        Mov eax ToolBarStylesHelp
    .Else_If D$CheckingMask = RichEdit20aCheckingMask
        Mov eax RichEdit20aStylesHelp
    .Else_If D$CheckingMask = SysHeader32CheckingMask
        Mov eax SysHeader32StylesHelp
    .Else_If D$CheckingMask = ReBarWindow32CheckingMask
        Mov eax ReBarWindow32StylesHelp
    .Else_If D$CheckingMask = tooltips_class32CheckingMask
        Mov eax tooltips_class32StylesHelp
    .Else_If D$CheckingMask = msctls_statusbar32CheckingMask
        Mov eax msctls_statusbar32StylesHelp
    .Else_If D$CheckingMask = msctls_hotkey32CheckingMask
        Mov eax msctls_hotkey32StylesHelp
    .Else_If D$CheckingMask = ComboBoxEx32CheckingMask
        Mov eax ComboBoxEx32StylesHelp
    .Else_If D$CheckingMask = SysAnimate32CheckingMask
        Mov eax SysAnimate32StylesHelp
    .Else_If D$CheckingMask = SysMonthCal32CheckingMask
        Mov eax SysMonthCal32StylesHelp
    .Else_If D$CheckingMask = SysDateTimePick32CheckingMask
        Mov eax SysDateTimePick32StylesHelp
    .Else_If D$CheckingMask = SysIPAddress32CheckingMask
        Mov eax SysIPAddress32StylesHelp
    .Else_If D$CheckingMask = SysPagerCheckingMask
        Mov eax SysPagerStylesHelp
    .Else_If D$CheckingMask = SysLinkCheckingMask
        Mov eax SysLinkStylesHelp
    .End_If

    sub esi 4 | sub esi TABLE.H.StyleHelpButtons | add esi eax

    Call 'USER32.CreateDialogIndirectParamA' D$H.Instance, HelpDialog, ecx,
                                             HelpDialogProc, D$esi

ret
____________________________________________________________________________________

; Called by next routine only (to suppress the class by number before wirting a new
; class by name. edi, ecx set by caller:

StripFFFF0080:
    Mov B$edi '"' | inc edi
    Push edi
        add edi 8                                   ; edi after 'FFFF 0080'
        Push edi
            Mov al 255 | repne scasb | Mov ecx edi  ; edi > End of template
        Pop esi
        sub ecx esi                                 ; how many char to Move backward
    Pop edi                                         ; esi > first char
    rep movsb
ret


[DefaultNumberClass: B$ 'xxxx' EOS]

WriteClass: ;need LINES_!!
    Call 'USER32.SendMessageA' D$TABLE.H.DialogControls, &CB_GETCURSEL, 0, 0 | Mov ebx eax

    Mov edx D$DialogListIndex, edi D$NewDialogTemplateText, al 0, ecx MaxTemplateText
    sub edx Line_Class

L0: repne scasb | dec edx | jnz L0<                          ; to point to upper Style
    add edi 4 | Mov B$edi-1 '5', al '0', ecx 7 | rep stosb   ; reset Style record to default
    Mov edx Line_Class, al 0, ecx 0FFFFFFFF
L0: repne scasb | dec edx | jnz L0<                          ; point to Class record

    ..If D$edi = 'FFFF'
        .If ebx < 6
            add edi 8 | Mov B$edi '0' | add B$edi bl         ; new 'FFFF 008x' value
        .Else
          ; strip "text", make 9 chars room, copy 'FFFF 008x' value:
            Push edi
                Push ebx
                    Call StripFFFF0080
                Pop ebx
                sub ebx 6
                Mov edi ControlClassByNames, ecx 0FFFF, al 0
                While ebx > 0
                    repne scasb | dec ebx
                End_While                                ; edi > choosen Class by name text
            Pop esi
            inc esi                     ; "StripFFFF0080" have written first quote
            Call ResetTemplateClass
        .End_If
    ..Else                              ; edi point to "msctls_...."
      ; Strip "text":
        Push ebx, edi
            inc edi | Call StripTemplateText
        Pop edi, ebx
        .If ebx < 6
          ; make 9 chars room (including <" 0>, copy 'FFFF 008x' value):
            Push ebx
                Mov esi edi, edi DefaultNumberClass | Call ResetTemplateClass
                sub edi 7 | Mov D$edi 'FFFF', D$edi+4 ' 008'
            Pop ebx
            add bl '0' | Mov B$edi+8 bl, B$edi+9 SPC
        .Else
            Push edi
                sub ebx 6
              ; make whished lenght room, copy new Class text
                Mov edi ControlClassByNames, ecx 0FFFF, al 0
                While ebx > 0
                    repne scasb | dec ebx
                End_While
            Pop esi
            inc esi
            Call ResetTemplateClass
        .End_If
    ..End_If

    Call FromTextToBinTemplate | Call ShowDialogResult | Call FillDialogListBox
ret
_________________________________________________________________________________

WriteFontSize:
    Mov edi D$NewDialogTemplateText al 0, ecx MaxTemplateText
    repne scasb | repne scasb | repne scasb | repne scasb | repne scasb
    Push edi
        Call 'USER32.SendMessageA' D$TABLE.H.DialogControls+4  &CB_GETCURSEL  0  0
    Pop edi
    Push edi
        Call 'USER32.SendMessageA' D$TABLE.H.DialogControls+4 &CB_GETLBTEXT eax edi
    Pop edi
    Mov B$edi+2 SPC
    Call FromTextToBinTemplate | Call ShowDialogResult | Call FillDialogListBox
ret
_____________________________________________________________________________________

; 2 routines to replace some text inside template:

StripTemplateText:
    Push edi
        Mov al '"', ecx MaxTemplateText | repne scasb | dec edi ; edi > lasting '"'
        Push edi
            Mov al 255 | repne scasb                            ; edi > End of template
            Mov ecx edi
        Pop esi
        sub ecx esi                                    ; how many char to Move backward
    Pop edi                                                     ; esi > first char
    rep movsb
ret


ResetTemplateText:
    Push edi
        Push esi                                                ; > where to write
            Mov al 0, ecx 120, ebx 120 | repne scasb
            sub ecx 120 | neg ecx | Mov ebx ecx                 ; new string lenght
            Mov edi D$NewDialogTemplateText, al 255, ecx MaxTemplateText
            repne scasb | Mov esi edi | add edi ebx             ; edi, esi ready for room
        Pop eax
        Push eax
            Mov ecx esi | sub ecx eax | dec edi
            std
                rep movsb                   ; make empty room
            cld
        Pop edi
    Pop esi                                 ; adress of new string

L0: lodsb | cmp al 0 | je L9>
    stosb | jmp L0<                         ; write new string

L9: Mov al '"' | stosb
ret
______________________________________________________________________________________

ResetTemplateClass:
    Push edi
        Push esi                                                   ; > where to write
            Mov al 0, ecx 120, ebx 120 | repne scasb
            sub ecx 120 | neg ecx | Mov ebx ecx                    ; new string lenght
            add ebx 2                                              ; for ' 0'
            Mov edi D$NewDialogTemplateText, al 255, ecx MaxTemplateText
            repne scasb | Mov esi edi | add edi ebx                ; edi, esi ready for room
        Pop eax
        Push eax
            Mov ecx esi | sub ecx eax | dec edi
            std
                rep movsb                ; make empty room
            cld
        Pop edi
    Pop esi                              ; adress of new string set in edi by caller.

L0: lodsb | cmp al 0 | je L9>
    stosb | jmp L0<                      ; write new string

L9: Mov al '"' | stosb
    Mov ax ' 0' | stosw
ret

__________________________________________________________________________________

[LBbuffer: D$ ? # 30] ;[LBbufferLen 120]

ClearLBbuffer:
    Mov eax 0, ecx 30, edi LBbuffer | rep stosd
ret


WriteFontType:
    Call ClearLBbuffer
    Mov edi D$NewDialogTemplateText al 0, ecx MaxTemplateText
    repne scasb | repne scasb | repne scasb | repne scasb | repne scasb
    add edi 4
    Push edi
        Call StripTemplateText
        Call 'USER32.SendMessageA' D$TABLE.H.DialogControls  &CB_GETCURSEL  0  0
        Call 'USER32.SendMessageA' D$TABLE.H.DialogControls &CB_GETLBTEXT eax LBbuffer
    Pop esi
    Mov edi LBbuffer | Call ResetTemplateText
    Call FromTextToBinTemplate | Call ShowDialogResult | Call FillDialogListBox
ret
________________________________________________________________________________________
;;
 For some unknown reason, none of the usual working messages to retrieve text or
 value from an edit control (what work everywhere Else in this source) doesn't
 work with these boxes. So i have to use "WM_GETTEXT" instead, translate it to Bin,
 then to hexa, then to hexa text !!!!!!!!............ These Edit boxes no not seam to
 have a valid ID. They are created by:

     Call 'USER32.CreateWindowExA'  0  EditClass  0,
             WS_CHILD+WS_VISIBLE+WS_BORDER+ES_NUMBER+ES_RIGHT+ES_MULTILINE,
             80 D$esi+4  45 20, D$EDBPadressee 0 D$H.Instance 0

 ... just like any other ones...
;;

[DimIsForDialogWindow: D$ ?] ; used as +0 / +1 (true / false) to jump over n (number of controls)

WriteDimOnly:
    Push ecx                          ; edit control handle ( > edx )
        Call 'USER32.SendMessageA' ecx, &WM_GETTEXT, 0F, LBbuffer
    Pop edx
    Call SearchDialogLine           ; Edi is set there.
    Mov esi TABLE.H.DialogControls, ebx 1
    while D$esi <> edx | add esi 4 | inc ebx | End_While  ; search for what control
  ; 3 handles (text, edit, UpDown). So: 2 > 1   5 > 2   8 > 3   11 > 4
    shr ebx 1 | On ebx > 2, dec ebx
    add ebx D$DimIsForDialogWindow                       ; jump over n (number of controls)
    Mov al SPC
    While ebx > 0 | repne scasb | dec ebx | End_While    ; edi > dim to overwrite
    Push edi

      ; Translate decimal to binary (simplified version):
        Mov eax 0, ecx 0, esi LBbuffer
L2:     Mov cl B$esi | inc  esi              ; (eax used for result > no lodsb)
        cmp cl 0 | jbe  L9>
            Mov edx 10 | mul edx | sub  ecx '0'
            add  eax ecx | jmp  L2<          ; >>> number in EAX
L9:         Mov ebx eax
            add ebx D$ProposedUpDowmChange | Mov D$ProposedUpDowmChange 0
      ; Store text hexa at "DimTempo":
        Mov edi LBbuffer | Call TranslateDialogText8 | Mov al 0 | stosb
    Pop edi
    Mov eax '0000' | stosd | dec edi | Mov esi LBbuffer
    While B$esi+1 > 0 | inc esi | End_While
    std | Mov ecx 4
L0:     lodsb
        stosb | On esi < LBbuffer, jmp L9>
        loop L0<
L9: cld
ret

WriteDim:
    Call WriteDimOnly
    Call FromTextToBinTemplate | Call ShowDialogResult | Call FillDialogListBox
ret
__________________________________________________________________________________

WriteTitle:
    Call SearchDialogLine | inc edi        ; points to Class record
    Push edi
        Call StripTemplateText
        Call 'USER32.GetDlgCtrlID' D$TABLE.H.DialogControls
        Call 'USER32.GetDlgItemTextA' D$H.DialogEditor eax LBbuffer 100
                                        ; fix limit (100) here
                                        ; (sendmessage for text limitation doesn't work)
    Pop esi

    Mov edi LBbuffer | Call ResetTemplateText

    Call FromTextToBinTemplate | Call ShowDialogResult | Call FillDialogListBox
ret
_____________________________________________________________________________________

WriteID:
    Call SearchDialogLine

    Push edi
      Call 'USER32.GetDlgCtrlID' D$TABLE.H.DialogControls
      Call 'USER32.GetDlgItemInt' D$H.DialogEditor eax  0  0
    Pop edi

    Mov ebx eax, ecx 4  | On eax > 0, Mov W$PreviousControlID ax

L0: Mov eax ebx | and eax 0_F000 | shr eax 12
    add al '0' | On al > '9', add al 7
    shl ebx 4
    stosb | loop L0<

    Call FromTextToBinTemplate | Call ShowDialogResult | Call FillDialogListBox
ret
____________________________________________________________________________________

[DelMessage: B$ "

    To delete a control:

      Select any record in the control you want to delete.       
      Do not select a separator empty line.
      Do not select a Dialog box record.
    " EOS]

[Wahoo: B$ ' Whahooo!!!...' EOS]

OutDelOneControl:
    Call 'USER32.MessageBoxA' D$H.MainWindow, DelMessage, Wahoo, &MB_SYSTEMMODAL
ret


DelOneControl:
    Call 'USER32.SendMessageA' D$H.DialogList &LB_GETCURSEL eax 0
    Mov D$DialogListIndex eax

    .If D$DialogListIndex < Line_empty+1
        jmp OutDelOneControl

    .Else_If D$DialogListIndex > 0FFFF
        jmp OutDelOneControl

    .Else
        Call SearchDialogLine
        On B$edi = 0, jmp OutDelOneControl

        Mov al 0, ecx MaxTemplateText          ; edi > one record inside one control
        While B$edi > 0
            repne scasb
        End_While
        Mov esi edi | sub edi 2                ; esi > end of control records
        std
            While B$edi > 0
                repne scasb
            End_While
        cld
        add edi 2                              ; edi > start of control records

; If user delete some control before defining the ID, we reset the flag for new "Add":
        Push edi
            repne scasb | repne scasb
            On D$edi = '0000', Mov W$PreviousControlID 0FFFF  ; ID at +52 octets from start

            Mov al 255, edi esi | repne scasb  ; search for the end
            Mov ecx edi
        Pop edi | dec edi | sub ecx edi        ; ecx = lenght of data to Move upward
        rep movsb
        Call FromTextToBinTemplate | Call ShowDialogResult | Call FillDialogListBox

    .End_If
ret
____________________________________________________________________________________

; Called by Dialog Edition CallBack with buttons handles in ecx, controls table index in ebx:

ExitDialog:
    .If ebx = 0
        Call SaveClipDialog | Call CloseDialogEdition
    .Else_If ebx = 4
        If B$DialogLoadedFromResources = &TRUE
            Call SaveResourceDialog | Call CloseDialogEdition
        Else
            Call SaveNewResourceDialog | Call CloseDialogEdition
        End_If
        Mov D$FL.SourceHasChanged &TRUE
    .Else_If ebx = 8
       Call CloseDialogEdition |  Call SaveDialogToDisk
    .Else   ;_If ebx = 12
       Call CloseDialogEdition
    .End_If
ret

CloseDialogEdition:
    Call 'USER32.ClipCursor' &NULL
    Call UninstallHook
    Call 'USER32.EndDialog' D$H.DialogEditor 0
    Call 'USER32.DestroyWindow' D$H.EditedDialog
    Mov D$H.DialogEditor 0, D$H.EditedDialog 0
    Mov D$FL.DialogEdition &FALSE, B$DialogLoadedFromResources &FALSE
ret

_______________________________________________________________________________________
;;
 saving the template in ClipBoard. The main job is to make it clean and pretty source.
 (aligned comments, formated hexa numbers, ...). "B$InsideText" is used to know where
 to set leading hexa zeros (to preserve both texts and comments).
;;

[ClipTemplate: D$ ?
 ClipTemplateLength: D$ ?]

[DialogName: B$ 'Dialog: ' EOS]
[ControlName: B$ 'Control0' EOS]


BuildDialogTemplate:

    Call VirtualAlloc ClipTemplate,
                      60_000

    Mov edi D$ClipTemplate , esi D$NewDialogTemplateText
    Mov al '[' | stosb
    Mov eax 'Dial' | stosd | Mov eax 'og: ' | stosd
    Mov ebx 0, edx 8, B$InsideText &FALSE

L0: lodsb
    ..If al = 0                                     ; line end:
        Mov edx 0
        .If B$esi = 0                               ; block end (dialog or control)
            inc esi
            Push edi
                std
                    Mov al ';', ecx 200 | repne scasb      ; write ']' before comment
                    Mov al SPC | repe scasb | add edi 2    ; search last non space char
                    Mov al ']' | stosb
                cld
            Pop edi
            Mov al CR, ah LF | stosw | stosw
            On B$esi = 255, jmp L9>>
            Mov al '[' | stosb
            Mov eax 'Cont' | stosd | Mov eax 'rol0' | stosd | dec edi
            Push ebx
                Call TranslateDialogText4
            Pop ebx
            inc ebx
            Mov ax ': ' | stosw | add edx 10
        .Else
            Mov al CR, ah LF | stosw | Mov al SPC | stosb
        .End_If
        Mov B$InsideText &FALSE

    ..Else
        .If al = '"'
            Mov al "'" | Mov B$InsideText &TRUE
        .End_If
        .If al <> '0'
            If al = ';'
                Mov ecx 30 | cmp edx 28 | ja L2>
                sub ecx edx | Mov al SPC | rep stosb
                Mov al ';', B$InsideText &TRUE
            End_If
            On B$InsideText = &TRUE, jmp L2>
            If B$esi <> '$'                         ; set a leading '0'
                cmp B$edi-1 SPC | jne L2>           ; for naked hexa
                cmp al '1' | jb L2>
                cmp al 'F' | ja L2>
                    Mov B$edi '0' | inc edi | inc edx  ; numbers (not before 'U$'/'D$')
            End_If
L2:         stosb | inc edx
        .Else
            If W$edi-2 <> ' 0'
                stosb | inc edx                     ; no double zeros for hexa numbers
            End_If
        .End_If

    ..End_If
    jmp L0<<

L9: sub edi D$ClipTemplate | Mov D$ClipTemplateLength edi
ret


SaveClipDialog:
    Push D$BlockStartTextPtr, D$BlockEndTextPtr, D$FL.BlockInside

        Call BuildDialogTemplate
        Move D$BlockStartTextPtr D$ClipTemplate

L9:     Mov eax D$BlockStartTextPtr | add eax D$ClipTemplateLength | dec eax
        Mov D$BlockEndTextPtr eax

        Mov D$FL.BlockInside &TRUE | Call ControlC | Mov D$FL.BlockInside &FALSE

        Call VirtualFree ClipTemplate

        Mov B$InsideText &FALSE

    Pop D$FL.BlockInside, D$BlockEndTextPtr, D$BlockStartTextPtr
ret


[DlgFilesFilters: B$ 'RosAsm Dialog Template' EOS '*.dlgl' EOS 0]
[RcFilesFilters: B$ 'RosAsm RC Template' EOS '*.*' EOS, 0]
[SaveDlgFilter: B$ ? # &MAX_PATH]
[ChoosenDlgFile: B$ ? # &MAX_PATH]

[OpenDlg:
 OpenDlg.lStructSize: D$ len
 OpenDlg.hwndOwner: D$ 0
 OpenDlg.hInstance: D$ 0
 OpenDlg.lpstrFilter: D$ DlgFilesFilters

 OpenDlg.lpstrCustomFilter: D$ &NULL ; uFileFilter
 OpenDlg.nMaxCustFilter: D$ 0 ; &MAX_PATH
 OpenDlg.nFilterIndex: D$ 1
 OpenDlg.lpstrFile: D$ SaveDlgFilter
 OpenDlg.nMaxFile: D$ &MAX_PATH
 OpenDlg.lpstrFileTitle: D$ ChoosenDlgFile
 OpenDlg.nMaxFileTitle: D$ &MAX_PATH
 OpenDlg.lpstrInitialDir: D$ 0
 OpenDlg.lpstrTitle: D$ SaveDlgNameTitle
 OpenDlg.Flags: D$ &OFN_CREATEPROMPT__&OFN_EXPLORER__&OFN_HIDEREADONLY__&OFN_LONGNAMES__&OFN_NONETWORKBUTTON__&OFN_OVERWRITEPROMPT__&OFN_PATHMUSTEXIST
 ; 0281804
 ; &OFN_CREATEPROMPT__&OFN_EXPLORER__&OFN_HIDEREADONLY__&OFN_LONGNAMES
 ; &OFN_NONETWORKBUTTON__&OFN_OVERWRITEPROMPT__&OFN_PATHMUSTEXIST
 ;  0_2000
 ; 08_0000
 ;       4
 ; 20_0000
 ;  2_0000
 ;       2
 ;     800
 OpenDlg.nFileOffset: W$ 0
 OpenDlg.nFileExtension: W$ 0
 OpenDlg.lpstrDefExt: D$ 0
 OpenDlg.lCustData: D$ 0
 OpenDlg.lpfnHook: D$ 0
 OpenDlg.lpTemplateName: D$ 0]

;[OpenPEStruc: len
; hwndPEFileOwner: 0  OPESInstance: 0  PEFilesFilters  uFileFilter  260
; 1 SaveFilter  260  ChoosenFile  260  0
; OpenPEFileTitle  OpenPEStrucFlags: 0281804
; 0  0  0  0  0]

SaveDialogToDisk:
    Call BuildDialogTemplate

    Mov edi SaveFilter, eax 0, ecx 65 | rep stosd
    Mov D$SaveDlgFilter 'New.', D$SaveDlgFilter+3 '.dlg', D$SaveDlgFilter+7 0

    Call 'Comdlg32.GetSaveFileNameA' OpenDlg | On eax = &FALSE, jmp L9>>

    Call ForceExtension SaveDlgFilter, '.dlg'

    Call 'KERNEL32.CreateFileA' SaveDlgFilter &GENERIC_WRITE,
                               &FILE_SHARE_READ__&FILE_SHARE_WRITE, 0,
                               &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE

        Call MessageBox D$BusyFilePtr | jmp L9>>

    End_If

    Mov D$H.Destination eax, D$NumberOfReadBytes 0

    Call 'KERNEL32.WriteFile' D$H.Destination, D$ClipTemplate, D$ClipTemplateLength,
                              NumberOfReadBytes  0

    Call 'KERNEL32.CloseHandle' D$H.Destination | Mov D$H.Destination 0

L9: Call VirtualFree ClipTemplate

    Mov B$InsideText &FALSE

ret


Proc ForceExtension:
    Argument @FileName, @Ext

        Mov eax D@FileName, ebx D@Ext
        While B$eax <> 0 | inc eax | End_While | sub eax 4

        ..If D$eax <> ebx
            .If B$eax = '.'
L1:             Mov D$eax ebx, B$eax+4 0
            .Else
                If B$eax+1 = '.'
                    inc eax | jmp L1<
                Else_If B$eax+2 = '.'
                    add eax 2 | jmp L1<
                Else_If B$eax+3 = '.'
                    add eax 3 | jmp L1<
                Else
                    add eax 4 | jmp L1<
                End_If
            .End_If
        ..End_If
EndP

; Reuse of the ClipBoard Naming because reuse of the Routine for Loading from ClipBoard.

OpenDlgFile:
    Mov D$OtherFilesFilters DialogFilesFilters
    Mov D$OpenOtherFileTitle DialogFilesTitle

    Move D$OtherhwndFileOwner D$H.MainWindow, D$OtherhInstance D$H.Instance

    Mov edi OtherSaveFilter, ecx 260, eax 0 | rep stosd
    Call 'Comdlg32.GetOpenFileNameA' OtherOpenStruc

    If D$OtherSaveFilter = 0
        Pop eax | ret
    End_If

    Call 'KERNEL32.CreateFileA' OtherSaveFilter &GENERIC_READ,
                                &FILE_SHARE_READ+&FILE_SHARE_WRITE, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE

      Call MessageBox D$BusyFilePtr

    Pop eax

ret  ; return to caller of caller

    Else
      Mov D$H.OtherSource eax
    End_If

    Call 'KERNEL32.GetFileSize'  eax 0 | Mov D$ClipBoardLen eax

    If eax > 0

        Call VirtualAlloc ClipBoardPTR,
                          eax

        Mov eax D$ClipBoardPTR | add eax D$ClipBoardLen | Mov D$ClipBoardEnd eax


        Call 'KERNEL32.ReadFile' D$H.OtherSource D$ClipBoardPTR,
                                 D$ClipBoardLen NumberOfReadBytes 0
    End_If

    Call 'KERNEL32.CloseHandle' D$H.OtherSource
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[argh: B$ 'Arghhh!!!...' EOS]

[ClipBoardEnd: D$ ?
 OnClipDialog: D$ ?]
___________________________________________________________________________________
;;
 Saving the template for .rsrc:

 'DialogList' table holds the records for each Dialog resource (ID / Ptr / Size). This
 table is used at building time by 'TemporaryFillRsrcList' to prepare the Resource tree
 construction.
;;

SearchLenghtOfDialogData:
    Mov ebx D$LastDialogListItem, esi D$EditedDialogBoxData
    inc ebx                                     ; ex: 7+7+7+6 (last line not account)
    lodsd | lodsd                               ; styles
    lodsw | lodsw | lodsw | lodsw | lodsw       ; n, X, Y, W, H
    lodsw                                       ; menu?
    On ax > 0, lodsw                            ; menu ID
    lodsw                                       ; class? > 0 for edition

    Mov edi esi
        Mov ax 0, ecx 200 | repne scasw | repne scasw   ; title / font
    Mov esi edi

    sub ebx Line_empty+1
    while ebx > 0
        test esi 00_11 ZERO L1>
            lodsw                               ; align
L1:     lodsd | lodsd                           ; styles
        lodsw | lodsw | lodsw | lodsw           ; X, Y, W, H
        lodsw | lodsw                           ; ID / Class
        If ax <> 0FFFF
            Mov edi esi
                Mov ax 0, ecx 200 | repne scasw ; Text form Class (zero ended)
            Mov esi edi
        End_If
        Mov edi esi
            Mov ax 0, ecx 200 | repne scasw     ; title
        Mov esi edi
        lodsw

        sub ebx Line_empty+1
    End_While

    Mov ecx esi | sub ecx D$EditedDialogBoxData ; lenght of dialog data

    Mov esi ClassRecord
    While B$esi > 0
        lodsb | add ecx 2
    End_While
ret


[WhatDialogIDData: D$ 090C408C2 0  ; Style
 U$ 03 0 0 09C 01A             ; Dim
 0                             ; Menu
 0                             ; Class
 'Give an ID number for this Dialog' 0 ; Title
 08 'Helv' 0]                  ; Font

[WDIDC1: D$ 050042000 0        ; Style
 U$ 02A 05 049 0F              ; Dim
 0DE                           ; ID
 0FFFF 081                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data

[WDIDC2: D$ 050000001 0        ; Style
 U$ 075 05 023 0E              ; Dim
 0DF                           ; ID
 0FFFF 080                     ; Class
 'OK' 0                        ; Title
 0]                            ; No creation data

[WDIDC3: D$ 050000000 0        ; Style
 U$ 03 05 024 0F               ; Dim
 0E0                           ; ID
 0FFFF 080                     ; Class
 'Abort' 0                     ; Title
 0]                            ; No creation data


[OK_ID 0DF  IDNumberEdit 0DE  Abort_ID 0E0]


[EIDNMessage: B$ "

    Dialog ID numbers can't be greater than 0FFFF (65535)    

" EOS]


ErrorIDnumber:
    Call 'USER32.MessageBoxA' D$H.MainWindow, EIDNMessage, Wahoo, &MB_SYSTEMMODAL
ret


[UserAbortID: D$ ?]
[IDstring: B$ '        ' EOS]

Proc WhatDialogIdProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_INITDIALOG
        Mov B$UserAbortID &FALSE
        Call 'USER32.GetDlgItem' D@hwnd, IDNumberEdit
        Push eax
            Call 'USER32.SendMessageA' eax, &EM_SETLIMITTEXT, 5, 0
        Pop eax
        If B$DialogLoadedFromResources = &TRUE
            Mov eax D$WhatDialogListPtr, edi IDString, ebx 10          ; translate ID to
            sub eax 4 | Mov eax D$eax
            add edi 9                                                    ; decimal text
L0:         Mov edx 0 | div ebx
            dec edi | add dl '0' | Mov B$edi dl | cmp eax 0 | ja L0<
            Call 'USER32.SetDlgItemTextA' D@hwnd, IDNumberEdit, edi

        Else
            Mov esi DialogList
            While D$esi+12 > 0
                add esi 12
            End_While
            Mov D$DialogListPtr esi | On D$esi <> 0, add D$DialogListPtr 12

            lodsd | add eax 10
            Mov edi IDString, ebx 10 | add edi 9 | jmp L0<
        End_If

        Call 'USER32.SetClassLongA' D@hwnd, &GCL_HICON, D$STRUC.WINDOWCLASS@hIcon

    ...Else_If D@msg = &WM_COMMAND
        ..If W@wParam = OK_ID
            Call 'USER32.GetDlgItemInt' D@hwnd, IDNumberEdit, 0, 0

            .If eax > 0FFFF
                Call ErrorIDnumber
            .Else
                If B$DialogLoadedFromResources = &FALSE
                    Push eax
                        Call CheckNotUsedId eax, D@hwnd
                    Pop ebx
                    On eax = &IDCANCEL, jmp L5>
                    Mov eax ebx
                End_If

                If B$DialogLoadedFromResources = &TRUE
                    Mov edi D$WhatDialogListPtr | sub edi 4 | stosd
                Else
                    Mov edi D$DialogListPtr | stosd | add D$DialogListPtr 4
                End_If
                Call 'USER32.EndDialog' D@hwnd, 0
            .End_If

        ..Else_If W@wParam = Abort_ID
            Mov B$UserAbortID &TRUE
            Call 'USER32.EndDialog' D@hwnd, 0
        ..End_If

    ...Else
        popad | Mov eax &FALSE | jmp L9>

    ...End_If

L5: popad | Mov eax &TRUE

L9: EndP
____________________________________________________________________________________________
;;
 this does only save temporary a resource template in a fitting memory. It will
 remain there until compile time fills the resource tree. "ExitDialog" clear the
 editions chunks of memory. "ReleaseResourceDialogMemory" will be called by "MainWindowProc"
 exit case
;;

SaveNewResourceDialog:
    Call SearchLenghtOfDialogData
    Push ecx

        Call VirtualAlloc TempoMemPointer,
                          ecx                  ; new memory for dialog data

        Call 'USER32.DialogBoxIndirectParamA' D$H.Instance,
               WhatDialogIDData, 0, WhatDialogIdProc, 0 ; write ID in list <<<<<<<<

        If B$UserAbortID = &TRUE
            Call VirtualFree TempoMemPointer

Pop eax, eax  | ret                 ; return to caller of 'ExitDialog'

        End_If

        Mov eax D$TempoMemPointer
        Mov edi D$DialogListPtr | stosd         ; write ptr to data in list <<<<<<<<
        add D$DialogListPtr 4
        Mov edi eax                             ; ready for fill
    Pop ecx
    Mov esi D$DialogListPtr | Mov D$esi ecx     ; write size in list <<<<<<<<
    add D$DialogListPtr 4
    Call SaveDialogDataToResources
ret

____________________________________________________________________________________________
; In with ecx = Length as given by 'SearchLenghtOfDialogData'.
;         edi = Ptr to a Mem in DialogList.

SaveDialogDataToResources:
    Mov esi D$EditedDialogBoxData

    movsd | movsd                           ; Style / Ext.Style

    movsw | movsw | movsw | movsw | movsw   ; n / X / Y / W / H

    lodsw                                   ; +2 for '0' menu in edition

    sub ecx 20                              ; >>> 20 Bytes read / 18 Bytes written

    If D$DialogMenuTrueID = 0
        stosw                               ; '0' menu
    Else
        Mov eax 0FFFF | stosw
        Mov eax D$DialogMenuTrueID | stosw  ; See comments at 'DialogMenuComment'
    End_If

    lodsw
    Push esi
        Mov esi ClassRecord, eax 0
        Do
            lodsb | stosw                   ; write preserved Class
        loop_Until al = 0
    Pop esi
    sub ecx 2

    Do
        lodsw | stosw | sub ecx 2           ; write Title
    loop_Until ax = 0

    Do
        lodsw | stosw | sub ecx 2           ; write Font
    loop_Until ax = 0

;;
    Now, if edi is un-aligned, we have to align for the next Control.

    But: If esi is also un-aligned, the required alignement has already be done previously
        (at least for viewing the Dialog, when Editing). So:

    * esi un-aligned // edi un-aligned  >>> do nothing.
    * esi aligned    // edi aligned     >>> do nothing.
    * esi unaligned  // edi aligned     >>> kill previous alignment.
    * esi aligned    // edi un-aligned  >>> do the alignment.

   (The difference between esi and edi may come from addition of the Class
    Record, which is cleared off from the 'EditedDialogBoxData' for edition).
;;
    jecxz L9>

    Mov eax esi, ebx edi | and eax 00_11 | and ebx 00_11
    .If eax <> ebx
        If eax = 0
            Mov W$edi 0 | add edi 2
        Else
            lodsw | sub ecx 2 | jecxz L9>
        End_If
    .End_If

    rep movsb                               ; Write all Controls at once.
L9: ret

____________________________________________________________________________________________


SaveResourceDialog:

    Call 'USER32.DialogBoxIndirectParamA' D$H.Instance,
                                          WhatDialogIDData,
                                          0,
                                          WhatDialogIdProc,
                                          0   ; write ID in list <<<<<<<<

    If B$UserAbortID = &TRUE

          Pop eax | ret

    End_If

    Mov eax D$WhatDialogListPtr, eax D$eax

    Call VirtualFree eax                                     ; free previous mem ptr in List

    Call SearchLenghtOfDialogData

    Push ecx

        Call VirtualAlloc TempoMemPointer,
                          ecx

        Mov eax D$TempoMemPointer

        Mov edi D$WhatDialogListPtr | stosd             ; write ptr to data in list <<<<<<<<

        Mov edi eax                                     ; ready for fill

    Pop ecx

    Mov esi D$WhatDialogListPtr | add esi 4

    Mov D$esi ecx                                       ; write size in list <<<<<<<<

    Call SaveDialogDataToResources
ret


ReleaseResourceMemory:

    Call 'GDI32.DeleteObject' D$H.TabFont

  ; All Lists are: ID / Pointer to Mem / Size.

    Push esi,
         edi

        Mov edx DialogList,
            D$DialogListPtr DialogList

        Call ReleaseOneResourceMemory

    INT3 | NOP ; Debug à partir d'ici !!!

        Mov edx MenuList,
            D$MenuListPtr MenuList

        Call ReleaseOneResourceMemory

        Mov edx IconList,
            D$IconListPtr IconList

        Call ReleaseOneResourceMemory

        Mov edx GroupIconList,
            D$GroupIconListPtr GroupIconList

        Call ReleaseOneResourceMemory

        Mov edx CursorList,
            D$CursorListPtr CursorList

        Call ReleaseOneResourceMemory

        Mov edx GroupCursorList,
            D$GroupCursorListPtr GroupCursorList

        Call ReleaseOneResourceMemory

        Mov edx BitMapList,
            D$BitMapListPtr BitMapList

        Call ReleaseOneResourceMemory

        Mov edx WaveList,
            D$WaveListPtr WaveList

        Call ReleaseOneResourceMemory

        Mov edx AviList,
            D$AviListPtr AviList

        Call ReleaseOneResourceMemory

        Mov edx RcDataList,
            D$RcDataListPtr RcDataList

        Call ReleaseOneResourceMemory

 ;   Mov esi OtherList, D$OtherListPtr OtherList
 ;   Call ReleaseOneResourceMemory

        Mov edi uRsrcList,
            D$uRsrcListPtr uRsrcList,
            ecx 1000

        ; Clear uRsrcList
        xor eax eax | rep | stosd

    pop edi,
        esi

ret

ReleaseOneResourceMemory:

L1: Test D$edx NA ZERO S1>   ; ID

        Mov D$edx &NULL       ; Clear BitMapList table

        Call VirtualFree D$edx+4 ; ptr

        Mov D$edx+8 &NULL

    jmp L1< ; size

S1:

ret

;;

ReleaseOneResourceMemory:

L1: lodsd
    
        Test eax eax ZERO S1>   ; ID
    
        Mov D$esi-4 &NULL       ; Clear BitMapList table
        
        lodsd
        
        Mov D$esi-4 &NULL       ; ptr

        Call VirtualFree eax

        lodsd
    
        Mov D$esi-4 &NULL
        
    jmp L1< ; size

S1:

ret
;;
____________________________________________________________________________________

SearchResourceType:  ; in:  ebx = type (&RT_DIALOG, &RT_MENU, ...)
    Mov esi D$UserPEStartOfResources | cmp esi 0 | je L8>

    add esi 14                          ; > points to number of resources
    Mov eax 0 | lodsw | Mov ecx eax     ; > in ecx
    On eax = 0, ret                     ; if no resources at all

  ; search RT_MENU, ... in resource general header:

L0: lodsd | cmp eax ebx | je L1>
        lodsd | loop L0<
            jmp L8>                     ; no whished resource found (possible naked PE)

L1: lodsd                               ; menu "Level2Rt_Menu-StartOfRsrc+NodeFlag" in eax
    and eax 0FFFFFFF                    ; strip node flag (0_80000000)
    add eax D$UserPEStartOfResources

  ; edx will be the Number of Resources in a Type:
    add eax 14 | Mov esi eax | movzx edx W$esi
  ; If the Resource is registered by name instead of by ID:

    ;.If edx = 0

    .If W$esi-2 <> 0
        Call StoreNameToID esi
        Mov dx W$esi-2      ; Number of Resource for a given Type
        Push esi, edx
            dec dx | add esi 8
            While dx > 0 | Call StoreNameToID esi | dec dx | add esi 8 | End_While
        Pop edx, esi

        movzx edx W$esi-2 | add dx W$esi
  ;  .Else
  ;      If ebx = &RT_ICON
  ;          On B$FirstIconDone = &FALSE, Call StoreNameToID esi
  ;      End_If

    .End_If
    add esi 2 | ret

L8: Mov eax 0 | ret


; 'NamedIdList' Records are: [NumberID / StringLength with 2 "'" / 'String'] / [...] / ...

[NamedIdList: D$ ?]
[NamedIdListPtr: D$ ?
 FirstIconDone: D$ ?]

Proc StoreNameToID:
    Argument @Pointer
    Uses esi, edi, ebx, ecx, edx

        If D$NamedIDList = 0

           Call VirtualAlloc NamedIDList,
                             D$ResourcesSize

           Move D$NamedIdListPtr D$NamedIDList

        End_If

        Mov eax D@Pointer | add eax 2 ; Points now to Name Pointer (+ 0_8000_0000 mark)
        Mov eax D$eax | and eax 0_FFFF
       ; .If ebx = RT_ICON
       ;     If B$FirstIconDone = &FALSE
       ;         Mov eax 1, B$FirstIconDone &TRUE
       ;     End_If
       ; .End_If
        Mov edi D$NamedIdListPtr | stosd                        ; ID


        Mov esi eax | add esi D$UserPEStartOfResources

        lodsw | movzx eax ax | Mov ecx eax | add eax 2 | stosd  ; Length

        Mov B$edi '"' | inc edi

L0:     lodsw | stosb | loop L0<
        Mov B$edi '"' | inc edi | Mov D$edi 0                   ; String

        Mov D$NamedIdListPtr edi
EndP

____________________________________________________________________________________________

;;
  1) Search the Data 'Name'
  2) Get Back and read the Data Label
  3) Replace all Evocations of this Label by the Direct Numbered ID
;;
NamedIdSubstitution:  ; 'SymbolicAnalyzes' 'FRproc' 'StringReplaceAll'
    ...If D$NamedIdList <> 0
        Mov esi D$NamedIdList
        Mov B$DownSearch &TRUE, B$CaseSearch &FALSE, B$SilentSearch &TRUE

        ..While D$esi <> 0
          ; Create a Copy of the NumberID in Text form:
            lodsd | Mov edi ReplaceWithString | Call WriteEax | Mov B$edi 0

          ; Copy the User Source IDName into the 'SearchString' Buffer:
            lodsd | Mov D$LenOfSearchedString eax, ecx eax
            Mov edi SearchString | rep movsb | Mov al 0 | stosb

          ; esi ready for next Record:
            add esi ebx
            Push esi
                Mov D$NextSearchPos 0
                Move D$STRUCT.EditData@CurrentWritingPos D$CodeSource
                Call StringSearch
                ..If D$FL.BlockInside = &TRUE
                  ; The Text of the IdName has been found. Search back for the Data Label:
                    Mov esi D$BlockStartTextPtr
                    While D$esi <> 'Data' | dec esi | End_While

                  ; Copy the 'DATAXXXXXX' into the 'SearchString' Buffer:
                    Mov edi SearchString, ecx 0
                    While B$esi <> ':' | movsb | inc ecx | End_While | Mov al 0 | stosb
                    Mov D$LenOfSearchedString ecx

                  ; Search Evocations of 'DATAXXXXXX', replace by the ID Number Text:
L0:                 Call StringSearch
                    If D$FL.BlockInside = &TRUE
                        Mov eax D$BlockEndTextPtr
                        On B$eax+1 <> ':', Call IDReplace
                        jmp L0<
                    End_If

                ..End_If
            Pop esi
        ..End_While

        Call VirtualFree NamedIDList

    ...End_If

ret


IDReplace:
    Mov esi ReplaceWithString, edi D$BlockStartTextPtr

    While B$esi <> 0 | movsb | End_While
    While B$edi > SPC | Mov B$edi SPC | inc edi | End_While
ret


SearchResourceNamedType:   ; in:  edi = Named Type pointer ('WAVE', 'AVI', ...), edx = len
    Mov esi D$UserPEStartOfResources | cmp esi 0 | je L8>>  ; retire >>

    add esi 14                            ; > points to number of resources
    Mov eax 0 | lodsw | Mov ecx eax       ; > in ecx
    On eax = 0, ret                    ; if no resources at all

  ; search Ptr to Name, ... in resource general header:

L0: lodsd | test eax 0_8000_0000 ZERO L2>
    pushad
        xor eax 0_8000_0000 | add eax D$UserPEStartOfResources
        Mov ecx edx, esi eax | lodsw | cmp al dl | jne L1>
        repe cmpsw | je L3>
L1: popad
L2: lodsd | loop L0<
      jmp L8>                             ; no whished resource found (possible naked PE)

L3: popad
    lodsd                                 ; menu "Level2Rt_Menu-StartOfRsrc+NodeFlag" in eax
    and eax 0FFFFFFF                      ; strip node flag (0_80000000)
    add eax D$UserPEStartOfResources

    add eax 14 | Mov esi eax, edx 0, dx W$esi
    If edx = 0
        Call StoreNameToID esi
        Mov dx W$esi-2
    End_If
    add esi 2 | ret

L8: Mov eax 0 | ret


; Read all Dialogs in RosAsm PE. Same routine as the ones for icon / menu.

ReadRosAsmPeDialogs:
    Mov edi DialogList, eax 0, ecx 300 | rep stosd
    Mov ebx RT_DIALOG | Call SearchResourceType | On eax = 0, ret
    Mov D$DialogListPtr DialogList,  ebx DialogListPtr | Call ReadResourcesRecord
  ret

 ______________________________________

  ; resource TYPEs dir:

  ; Resources (ID / Ptr / Size)

; in: ebx = Prt variable to a List (DialogListPtr, MenuListPtr, ...)
; (ebx is the adress, not the value -we Move it in here-)
; edx is the Number of Resources in a Type

ReadResourcesRecord:
    lodsd | and eax 0FFFF | Mov edi D$ebx | stosd   ; Write ID.
    Mov D$ebx edi                                   ; Adjust D$XxxxxxListPtr.
    lodsd                                  ; "Level3Rt_Menu-StartOfRsrc+NodeFlag" in eax

    Push esi, edx
        test eax 08000_0000 NOT_ZERO L1>
            add eax D$UserPEStartOfResources | Mov esi eax | jmp L5>>

L1:     and eax 0FFFFFFF | add eax D$UserPEStartOfResources | add eax 20 | Mov esi eax
      ; Language. dir:
        lodsd                    ; "Level4Rt_Menu-StartOfRsrc" in eax (no NodeFlag here
                                 ; next one is leafe ptr to true resources)
        add eax D$UserPEStartOfResources
        Mov esi eax

      ; Records of each resource:
L5:     lodsd                                   ; ptr to menu data (but RVA - startOfResource)
L1:     Mov ecx D$esi
        sub eax D$ResourcesRVA                  ; - RVA
        add eax D$UserPEStartOfResources        ; eax now points to true menu data

        If eax < D$UserPeStart
            jmp DisFail
        Else_If eax > D$UserPeEnd
            jmp DisFail
        End_If

        Push eax

            Call VirtualAlloc TempoMemPointer,
                              ecx

        Pop esi

        Mov edi D$TempoMemPointer
        Push edi, ecx
            rep movsb                           ; copy bin template to temporary mem
        Pop ecx, eax

        Mov edi D$ebx | stosd
        Mov eax ecx | stosd | Mov D$ebx edi

    Pop edx, esi
    dec edx | cmp edx 0 | ja ReadResourcesRecord; next resource TYPEs dir record > ID ready
L9: Mov eax &TRUE
ret
_____________________________________________________________________________________
;;
 All routines for dialog edition are first based on text version of templates. Here,
 we got first a bin template > translation back to text needed. All dialog resources
 have been uploaded in memories chuncks stored in "DialogList" (/ID/Ptr/Size). The
 Naming "LoadFromResouce" is only to echoe what user clicked on in main menu, but,
 in fact, upload job was previously done by "ReadRosAsmPeDialogs" when file is open.
 We now simply translate to text version before runing edition:
;;

[NoResourceDialog: B$ "

    There is no Resources Dialog in this PE      

" EOS]

LoadFromResources:
    If D$FL.DialogEdition = &TRUE
        Call Beep | ret  ; prevents from multi-runs
    End_If
    Call InitDialogMemory

    Mov esi DialogList
    .If D$esi  = 0                    ; empty? > out
        Call 'USER32.MessageBoxA' D$H.MainWindow, NoResourceDialog, Argh, &MB_SYSTEMMODAL | ret

    .Else_If D$esi+12 = 0               ; only one resource? > OK
        Mov D$WhatDialogListPtr DialogList+4

        Mov ebx D$WhatDialogListPtr, ebx D$ebx
        If W$ebx+18 = 0
            Mov D$ActualMenutestID 0, D$DialogMenuTrueID 0
        Else
            movzx eax W$ebx+20 | Mov D$DialogMenuTrueID eax
            Mov esi MenuList
            While D$esi <> eax
                add esi 12
            End_While
            Mov D$MenuListPtr esi
            add esi 4
            Call 'USER32.LoadMenuIndirectA' D$esi | Mov D$ActualMenutestID eax
        End_If

    .Else                               ; several resources? > wich one?
        Mov D$H.DialogList 0, D$H.DialogEditor 0
        Call WhatResourceTemplate       ; > D$WhatDialogListPtr > Data pointer

    .End_If

    Mov B$DialogLoadedFromResources &TRUE
    Call FromBinToTextTemplate
    Call ReInitDialogEdition
ret

____________________________________________________________________________________________

; The following routines have been rewritten to loose the dependency on the old routines.
; They are not rewritten in an intelligent way. They are just rewritten so they will do
; exactly the same thing as before (no matter how stupid that was/is).
____________________________________________________________________________________________

[DialogFromFile: D$ ?]

LoadDialogFromFile:
    If D$FL.DialogEdition = &TRUE
        Call Beep | ret
    End_If

    Mov B$DialogFromFile &TRUE

    Call InitDialogMemory

    Call OpenDlgFile | jmp L0>


LoadDialogFromClipBoard:
    If D$FL.DialogEdition = &TRUE
        Call Beep | ret
    End_If

    Mov B$DialogFromFile &FALSE

    Call InitDialogMemory

    Call OpenClipBoard


L0: On D$ClipBoardPtr = 0, ret
    On D$ClipBoardlen = 0, ret

    Mov B$WeAreInTheCodeBox &TRUE
    Mov eax esp, D$OldStackPointer eax

    Push D$CodeSourceA, D$CodeSourceB

        Mov ecx D$ClipBoardlen | add ecx 010

        Call VirtualAlloc CodeSourceA,
                          ecx

        add D$CodeSourceA 010

        Mov ecx D$ClipBoardlen | add ecx 010

        Call VirtualAlloc CodeSourceB,
                          ecx

        add D$CodeSourceB 010

        Call NewCopyToCodeSourceA D$ClipBoardPtr, D$ClipBoardlen

        If B$DialogFromFile = &TRUE

            Call VirtualFree ClipBoardPTR

        Else
            Call CloseClipBoard
        End_If

        Mov esi D$CodeSourceA | While B$esi > 0 | inc esi | End_While
        Mov W$esi CRLF | add esi 2 | add D$StripLen 2

        Call ClearQwordCheckSum

        Call CoolParsers

        Call NewCountStatements | On B$CompileErrorHappend = &TRUE, jmp L9>>

        Call HotParsers | On B$CompileErrorHappend = &TRUE, jmp L9>>

        Call InitIndex1 | Call InitIndex2

        Exchange D$CodeSourceA D$CodesourceB

        Push D$SourceLen
            Move D$SourceLen D$StripLen
            Move D$AsmTablesLength D$SourceLen
            Call ReuseSourceAForCodeList
        Pop D$SourceLen

        ;Call ClearQwordCheckSum

        Call InitIndex3

        Call SaveCheckSumTable

        Mov eax D$CodeListPtr | Mov D$DataList eax | Mov D$DataListPtr eax
        Call StoreDatas

        Call RestoreCheckSumTable

L9:     Mov B$WeAreInTheCodeBox &FALSE | On B$CompileErrorHappend = &TRUE, jmp L9>>

        Mov B$OnClipDialog &TRUE

        Mov esi D$DataList, edi D$NewDialogTemplateText, D$ActualEditedDialogID 0
        Mov eax D$DataListPtr | sub eax D$DataList | inc eax
        Mov D$ResourceDialogSize eax

        .If W$esi+18 = 0FFFF
            movzx eax W$esi+20 | Mov D$DialogMenuTrueID eax
            Push esi
                Mov esi MenuList
                While D$esi <> eax
                    add esi 12
                    If D$esi = 0
                        Pop esi | Call NoSuchMenu | jmp L9>
                    End_If
                End_While
            Pop esi
        .End_If

        Call FromClipBoardBinToText | Call ReInitDialogEdition

L9:     Call VirtualFree CodeSourceA

        Call VirtualFree CodeSourceB

    Pop D$CodeSourceB, D$CodeSourceA

ret
____________________________________________________________________________________________

[NoMenuMessage: B$ "
This Dialog Template cannot be loaded because it    
includes a Menu that is not found in the actual
Resources.

                            Menu"

MissingMenuID: B$ "         " EOS]

NoSuchMenu:
    Call WriteDecimalID eax, MissingMenuID
    Call 'USER32.MessageBoxA' D$H.MainWindow, NoMenuMessage, Argh, &MB_OK
ret
____________________________________________________________________________________________


[WhatDialogListPtr: D$ ?
 OkDialogFlag: D$ ?
 H.ChoiceDialog: D$ ?]

WhatResourceTemplate:
  ; 'DialogList' structure is (dWords): ID / Ptr / Size // ...
  ; D$WhatDialogListPtr >>> Ptr to Dialog Mem in the form of 'DefaultDialogTemplateText').
    Mov D$OkDialogFlag &FALSE, D$WhatDialogListPtr DialogList+4
    .While B$OkDialogFlag = &FALSE

        Mov ebx D$WhatDialogListPtr, ebx D$ebx
        If W$ebx+18 = 0
            Mov D$ActualMenutestID 0, D$DialogMenuTrueID 0
        Else
          ; If Menu, W$ebx+18 = 0FFFF // W$ebx+20 = ID Number.
            movzx eax W$ebx+20 | Mov D$DialogMenuTrueID eax
            Mov esi MenuList
            While D$esi <> eax
                add esi 12
            End_While
            Mov D$MenuListPtr esi | add esi 4
            Call 'USER32.LoadMenuIndirectA' D$esi | Mov D$ActualMenutestID eax
        End_If

        Call FromBinToTextTemplate | Call FromTextToBinTemplate

        Call 'USER32.CreateDialogIndirectParamA' D$H.Instance, D$EditedDialogBoxData,
                                                 D$H.EditWindow, EditedDialogBoxProc, 0
        Mov D$H.ChoiceDialog eax

        Call SetNextChoiceID
        Call 'USER32.DialogBoxIndirectParamA' D$H.Instance, ChoiceBar, D$H.MainWindow, ChoiceDialogBoxProc, DialogList
      ; This is the deletion of the viewed Dialog (not of the Choice Bar Dialog):
        Call 'USER32.EndDialog' D$H.ChoiceDialog 0

        .If B$OkDialogFlag = &VK_ESCAPE
            Pop eax | ret                          ; abort "LoadFromResources" caller
        .Else_If D$WhatDialogListPtr < DialogList
            Mov D$WhatDialogListPtr DialogList+4
            Call SetNextChoiceID
        .Else
            Mov esi D$WhatDialogListPtr
            If D$esi = 0
                sub D$WhatDialogListPtr 12 | Mov esi D$WhatDialogListPtr
                Call SetNextChoiceID
            End_If
        .End_If
    .End_While
ret
____________________________________________________________________________________________
;                       _______   _______   _______
; The tool:            [  <<<  ] [  OK   ] [  >>>  ]       at 'ChoiceBar'
;                       -------   -------   -------

[H.ChoiceDialogBox: D$ ?
 UserIsChoosing: D$ ?]

SetNextChoiceID:
L1: Mov eax D$WhatDialogListPtr, eax D$eax-4
    Mov ecx 10, edi ChoiceDecimalID+10, D$edi 0200020, D$edi+4 0200020,
    D$edi+8 0200020, D$edi+12 0200020
    add edi 18
L2: Mov edx 0 | div ecx | dec edi | add dl '0'
    Mov B$edi 0 | dec edi | Mov B$edi dl | cmp eax 0 | ja L2<
ret

[ListOrigin: D$ ?]
Proc ChoiceDialogBoxProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_COMMAND
        Mov eax D@wParam | and eax 0FFFF
        .If eax = CHOICEFORTH
            add D$WhatDialogListPtr 12 | Call SetNextChoiceID

        .Else_If eax = CHOICEBACK
            sub D$WhatDialogListPtr 12 | Call SetNextChoiceID

        .Else_If eax = CHOICEFIRST
            Move D$WhatDialogListPtr D$ListOrigin  ;DialogList+4 |
            add D$WhatDialogListPtr 4
            Call SetNextChoiceID

        .Else_If eax = CHOICELAST
L0:         add D$WhatDialogListPtr 12 | Mov eax D$WhatDialogListPtr
            cmp D$eax 0 | ja L0<
            sub D$WhatDialogListPtr 12 | Call SetNextChoiceID

        .Else_If eax = CHOICEOK
            Mov B$OkDialogFlag &TRUE

        .Else_If eax = &IDCANCEL
            Mov B$OkDialogFlag &VK_ESCAPE

        .Else
            jmp L8>>
        .End_If

        Mov D$H.ChoiceDialogBox 0
        Call 'USER32.EndDialog' D@hwnd 0

    ...Else_If D@msg = &WM_INITDIALOG
        Move D$H.ChoiceDialogBox D@hwnd, D$ListOrigin D@lParam

        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$STRUC.WINDOWCLASS@hIcon
        Call 'USER32.GetWindowPlacement' D@hwnd Control
        Mov eax D$Control.rcNormalPosition.top | shr eax 2
        add D$Control.rcNormalPosition.top eax
        add D$Control.rcNormalPosition.bottom eax
        Call 'USER32.SetWindowPlacement' D@hwnd Control

    ...Else
        If D$H.ChoiceDialogBox <> 0
            Call 'USER32.GetFocus'
            On eax <> D@hwnd, Call 'USER32.SetForegroundWindow' D@hwnd
        End_If
        popad | Mov eax &FALSE | jmp L9>

    ...End_If

L8: popad | Mov eax &TRUE

L9: EndP

[SureDeleteDialog: B$ 'Kill this Dialog?' EOS]

DeleteDialog:
    If D$FL.DialogEdition = &TRUE
        Call Beep | ret                    ; prevents from multi-runs
    End_If
    Call InitDialogMemory
    Mov esi DialogList
    .If D$esi  = 0                                          ; empty? > out
        Call 'USER32.MessageBoxA' D$H.MainWindow, NoResourceDialog, Argh, &MB_SYSTEMMODAL | ret
    .Else                                                   ; Wich resources to kill?
        Call WhatResourceTemplate
    .End_If
    Mov eax D$WhatDialogListPtr
    Call 'USER32.CreateDialogIndirectParamA' D$H.Instance, D$eax, D$H.MainWindow, EditedDialogBoxProc 0
        Mov D$H.ChoiceDialog eax

    Call 'USER32.MessageBoxA' D$H.MainWindow, SureDeleteDialog, Argh, &MB_SYSTEMMODAL+&MB_YESNO
    Push eax
        Call 'USER32.DestroyWindow' D$H.ChoiceDialog
    Pop eax

    If eax = &IDYES
        sub D$WhatDialogListPtr 4                       ; > ID / Ptr /Size
        Mov esi D$WhatDialogListPtr, edi esi
        add esi 12
        Mov eax esi | sub eax DialogList | shr eax 2

        Mov ecx MAXDIALOG | sub ecx eax                 ; tail to Move
        rep movsd                                       ; scratch
    End_If

    Call VirtualFree NewDialogTemplateText

    Call VirtualFree EditedDialogBoxData
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

SimplyGetDialog:
    Call InitDialogMemory

    Mov esi DialogList
    If D$esi  = 0
        Call 'USER32.MessageBoxA' D$H.MainWindow, NoResourceDialog, Argh, &MB_SYSTEMMODAL
        Mov eax 0 | ret
    Else
        Call WhatResourceTemplate
    End_If
ret


SaveToBinaryFile:
  ; Prevent from Multiple Instances:
    If D$FL.DialogEdition = &TRUE
        Call Beep | ret
    End_If

    Call SimplyGetDialog | On B$OkDialogFlag = &VK_ESCAPE, ret

  ; Similar to SaveDialogToDisk:

    Mov edi SaveFilter, eax 0, ecx 65 | rep stosd
    Mov D$SaveDlgFilter 'New.', D$SaveDlgFilter+3 '.dlf', D$SaveDlgFilter+7 0

    Call 'Comdlg32.GetSaveFileNameA' OpenDlg | On eax = &FALSE, ret

    Call ForceExtension SaveDlgFilter, '.bdf'

    Call 'KERNEL32.CreateFileA' SaveDlgFilter &GENERIC_WRITE,
                               &FILE_SHARE_READ__&FILE_SHARE_WRITE, 0,
                               &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0

    If eax = &INVALID_HANDLE_VALUE

        Call MessageBox D$BusyFilePtr

ret

    End_If

    Mov D$H.Destination eax, D$NumberOfReadBytes 0

    Mov esi D$WhatDialogListPtr | sub esi 4 | Mov ecx D$esi+8, esi D$esi+4

    Call 'KERNEL32.WriteFile' D$H.Destination, esi, ecx, NumberOfReadBytes  0

    Call 'KERNEL32.CloseHandle' D$H.Destination | Mov D$H.Destination 0
ret


[BinDialogMemory: D$ ?
 BinDialogLength: D$ ?]

LoadFromBinaryFile:
    If D$FL.DialogEdition = &TRUE
        Call Beep | ret
    End_If

    Mov D$OtherFilesFilters BinDialogFilesFilters
    Mov D$OpenOtherFileTitle DialogFilesTitle

    Move D$OtherhwndFileOwner D$H.MainWindow, D$OtherhInstance D$H.Instance

    Mov edi OtherSaveFilter, ecx 260, eax 0 | rep stosd
    Call 'Comdlg32.GetOpenFileNameA' OtherOpenStruc

    On D$OtherSaveFilter = 0, ret

    Call 'KERNEL32.CreateFileA' OtherSaveFilter &GENERIC_READ,
                                &FILE_SHARE_READ+&FILE_SHARE_WRITE, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE

      Call MessageBox D$BusyFilePtr

ret  ; return to caller of caller

    Else
      Mov D$H.OtherSource eax
    End_If

    Call 'KERNEL32.GetFileSize' eax, 0 | Mov D$BinDialogLength eax

    If eax > 0

        Call VirtualAlloc BinDialogMemory,
                          eax


        Call 'KERNEL32.ReadFile' D$H.OtherSource, D$BinDialogMemory,
                                 eax, NumberOfReadBytes, 0
    Else
        ret
    End_If

    Call 'KERNEL32.CloseHandle' D$H.OtherSource

    Mov esi DialogList | While D$esi <> 0 | add esi 12 | End_While
    Mov D$DialogListPtr esi
    Mov eax D$BinDialogMemory, D$esi+4 eax
    Mov eax D$BinDialogLength, D$esi+8 eax

    Mov B$DialogLoadedFromResources &FALSE
    Call 'USER32.DialogBoxIndirectParamA' D$H.Instance, WhatDialogIDData, 0,
                                          WhatDialogIdProc, 0

    If B$UserAbortID = &TRUE
        Mov esi DialogList | While D$esi+4 <> 0 | add esi 12 | End_While
        Mov D$esi-4 0, D$esi-8 0, D$esi-12 0

        Call VirtualFree BinDialogMemory

    End_If

ret


ReplaceFromBinaryFile:
    If D$FL.DialogEdition = &TRUE
        Call Beep | ret
    End_If

    Call SimplyGetDialog | On B$OkDialogFlag = &VK_ESCAPE, ret

    Mov D$OtherFilesFilters BinDialogFilesFilters
    Mov D$OpenOtherFileTitle DialogFilesTitle

    Move D$OtherhwndFileOwner D$H.MainWindow, D$OtherhInstance D$H.Instance

    Mov edi OtherSaveFilter, ecx 260, eax 0 | rep stosd
    Call 'Comdlg32.GetOpenFileNameA' OtherOpenStruc

    On D$OtherSaveFilter = 0, ret

    Call 'KERNEL32.CreateFileA' OtherSaveFilter &GENERIC_READ,
                                &FILE_SHARE_READ+&FILE_SHARE_WRITE, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE

      Call MessageBox D$BusyFilePtr

ret  ; return to caller of caller

    Else
      Mov D$H.OtherSource eax
    End_If

    Call 'KERNEL32.GetFileSize' eax, 0 | Mov D$BinDialogLength eax

    If eax > 0

        Call VirtualAlloc BinDialogMemory,
                          eax


        Call 'KERNEL32.ReadFile' D$H.OtherSource, D$BinDialogMemory,
                                 eax, NumberOfReadBytes, 0
    Else
        ret
    End_If

    Call 'KERNEL32.CloseHandle' D$H.OtherSource

    Mov edi D$WhatDialogListPtr | sub edi 4

    lea eax D$edi+4 | Call VirtualFree eax

    Mov eax D$BinDialogMemory, D$edi+4 eax

    Mov eax D$BinDialogLength, D$edi+8 eax

ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[SureReplaceDialog: B$ 'Replace with this Dialog ?' EOS]

ReplaceDialogFromFile:  ; DeleteDialog // LoadDialogFromFile


    ____________________________

  ; LoadDialogFromFile

    Call OpenDlgFile | On D$ClipBoardPtr = 0, ret
                       On D$ClipBoardlen = 0, ret

    Call 'USER32.MessageBoxA' D$H.MainWindow, SureReplaceDialog, Argh, &MB_SYSTEMMODAL+&MB_YESNO

    If eax = &IDYES
        Mov esi D$ClipBoardPtr, edi D$WhatDialogListPtr, ecx D$ClipBoardlen
        Mov D$edi+4 ecx, edi D$edi
        rep movsb
    End_If

    Call CloseClipBoard

    Mov B$DialogLoadedFromResources &TRUE
    Call FromBinToTextTemplate
    Call ReInitDialogEdition
ret
____________________________________________________________________________________________


[CHOICEBACK 0136  CHOICEOK 01  CHOICEFORTH 0138  CHOICEID 010   CHOICEFIRST 037
 CHOICELAST 038]

[ChoiceBar: D$ 0900408C2 0        ; Style
 U$ 06 0 0 0C7 017             ; Dim
 0                             ;      no Menu
 '' 0                          ; Class
 'New Dialog' 0                ; Title
 08 'Helv' 0]                  ; Font

[CGroup: D$ 050000307 0      ; Style
 U$ 0 0 0C8 019                ; Dim
 010                           ; ID
 0FFFF 080                     ; Class
 ChoiceDecimalID:
 'ID =          ' 0            ; Title
 0]                            ; No creation data

[CBack: D$ 050000000 0      ; Style
 U$ 0E 09 038 0E               ; Dim
 0136                          ; ID
 0FFFF 080                     ; Class
 '<<<<<' 0                     ; Title
 0]                            ; No creation data

[COK: D$ 050000000 0      ; Style
 U$ 047 09 038 0E              ; Dim
 01                            ; ID
 0FFFF 080                     ; Class
 'OK' 0                        ; Title
 0]                            ; No creation data

[CForth: D$ 050000000 0      ; Style
 U$ 080 09 038 0E              ; Dim
 0138                          ; ID
 0FFFF 080                     ; Class
 '>>>>>' 0                     ; Title
 0]                            ; No creation data

[Cfirst: D$ 050000000 0      ; Style
 U$ 02 09 0B 0E                ; Dim
 037                           ; ID
 0FFFF 080                     ; Class
 '[<' 0                        ; Title
 0]                            ; No creation data

[Clast: D$ 050000000 0      ; Style
 U$ 0B9 09 0B 0E               ; Dim
 038                           ; ID
 0FFFF 080                     ; Class
 '>]' 0                        ; Title
 0]                            ; No creation data


;;
"Load_Dialog_from_Resources" effective Translation for bin to text template.
 Called only in case of 'Load Existing Resource Dialog' and 'Search for what Dialog '.
 So we can find in the Dialog data some menu ID. We set it in 'D$DialogMenuTrueID'
 (to see DialogMenuComment, < Right Click).

 'FromTextToBinTemplate' doesn't do the reverse operation...
;;

[ActualEditedDialogID: D$ ?
 ResourceDialogSize: D$ ?
 DialogNcontrol: D$ ?]

[isDLGEX: D$ ?
 InjectFont: D$ ?]

FromBinToTextTemplate:
    Call InitDialogMemory
    Mov B$OnClipDialog &TRUE              ; reuse of this flag for initialisation in proc

    Mov eax D$WhatDialogListPtr, esi D$eax, edi D$NewDialogTemplateText  ; esi > ptr
    Move D$ActualEditedDialogID D$eax-4                                  ; ID
    Move D$ResourceDialogSize D$eax+4                                    ; size

FromClipBoardBinToText:
    cmp D$esi 0FFFF0001 | setz B$isDLGEX

    Mov eax 'D$  ' | stosd | dec edi
    If B$isDLGEX = &TRUE
       Mov ebx D$esi+0C
    Else
       Mov ebx D$esi
    End_If

    Test ebx &DS_SETFONT NOT_ZERO S1>

    or B$InjectFont 1

    S1: or ebx &DS_SETFONT | Call TranslateDialogText8
    Mov eax ' ; S' | stosd | Mov eax 'tyle' | stosd | Mov al 0 | stosb
    Mov eax 'D$  ' | stosd | dec edi
    Mov ebx D$esi+08
    If B$isDLGEX = &TRUE
       Mov ebx D$esi+08
    Else
       Mov ebx D$esi+04
    End_If
    Call TranslateDialogText8
    Mov eax ' ; E' | stosd | Mov eax 'xSty' | stosd |Mov eax 'le' | stosd | dec edi
    If B$isDLGEX = &TRUE
;      Mov eax 'D$ 0' | stosd | dec edi
;      Mov ebx D$esi+04 | Call TranslateDialogText8; HelpID
;      Mov eax ' ; H' | stosd | Mov eax 'elpI' | stosd| Mov ax 'D' | stosw
       add esi 08
    End_If
    add esi 08

    Mov eax 'U$  ' | stosd | dec edi
    lodsw | Mov W$DialogNcontrol ax
            Mov ebx eax | Call TranslateDialogText4 | Mov al SPC| stosb  ; n
    lodsw | Mov ebx eax | Call TranslateDialogText4 | Mov al SPC| stosb  ; X
    lodsw | Mov ebx eax | Call TranslateDialogText4 | Mov al SPC| stosb  ; Y
    lodsw | Mov ebx eax | Call TranslateDialogText4 | Mov al SPC| stosb  ; W
    lodsw | Mov ebx eax | Call TranslateDialogText4 | Mov al SPC| stosb  ; H
    Mov eax ' ; D' | stosd | Mov ax 'im' | stosw | Mov al 0 | stosb

    lodsw    ; always 0 in RosAsm edition but 0 or 'FFFF ID' in resources:

    If D$DialogMenuTrueID = 0
        Mov eax '0 ; ' | stosd | Mov eax '    ' | stosd |
        Mov eax ' no ' | stosd | Mov eax 'Menu' | stosd | Mov al 0 | stosb  ; No menu
      ; beware: this is room for "FFFF ID ; menu" when menu added!
    Else
        Mov ebx 0FFFF | Call TranslateDialogText4 | Mov al SPC | stosb       ; Menu
        lodsw
        Mov ebx D$DialogMenuTrueID |  Call TranslateDialogText4
        Mov eax ' ; M' | stosd | Mov ax 'en' | stosw | Mov al 'u' | stosb | Mov al 0 | stosb
    End_If

    Mov al '"' | stosb | Mov bx W$esi | inc bx | jne L0> | add esi 4 | jmp L1> ; can Id
L0: lodsw | cmp ax 0 | je L1>
        stosb | jmp L0<
L1: Mov al '"' | stosb
    Mov eax ' 0 ;' | stosd | Mov eax ' Cla' | stosd | Mov ax 'ss' | stosw
    Mov al 0 | stosb

    Mov al '"' | stosb
L0: lodsw | cmp ax 0 | je L1>
        stosb | jmp L0<
L1: Mov al '"' | stosb
    Mov eax ' 0 ;' | stosd | Mov eax ' Tit' | stosd | Mov ax 'le' | stosw
    Mov al 0 | stosb

    cmp B$InjectFont 0 | je F0>
    Mov eax '08 "' |stosd| Mov eax 'Helv' |stosd| jmp L1> ; Inject font!
F0:
    lodsw | Mov ebx eax | Call TranslateDialogText2 ;pointsize
    If B$isDLGEX = &TRUE
;       Mov al SPC | stosb
;       lodsw | Mov ebx eax | Call TranslateDialogText4 ;weight
;       Mov al SPC | stosb
;       lodsw | Mov ebx eax | Call TranslateDialogText4 ;bItalic
       add esi 04 ; or above
    End_If
    Mov ax ' "' | stosw
L0: lodsw | cmp ax 0 | je L1>
        stosb | jmp L0<
L1: Mov al '"' | stosb
    Mov eax ' 0 ;' | stosd | Mov eax ' Fon' | stosd | Mov al 't' | stosb
    Mov al 0 | stosb | stosb

    AlignOn 4 esi

L1: .While W$DialogNcontrol > 0
       dec W$DialogNcontrol
       Mov eax 'D$  ' | stosd | dec edi
       If B$isDLGEX = &TRUE
          Mov ebx D$esi+08
       Else
          Mov ebx D$esi
       End_If
       Call TranslateDialogText8
       Mov eax ' ; S' | stosd | Mov eax 'tyle' | stosd | Mov al 0 | stosb
       Mov eax 'D$  ' | stosd | dec edi
       Mov ebx D$esi+04
       Call TranslateDialogText8
       Mov eax ' ; E' | stosd | Mov eax 'xSty' | stosd |Mov eax 'le' | stosd | dec edi
       If B$isDLGEX = &TRUE
;         Mov eax 'D$  ' | stosd | dec edi
;         Mov ebx D$esi | Call TranslateDialogText8; HelpID
;         Mov eax ' ; H' | stosd | Mov eax 'elpI' | stosd| Mov ax 'D' | stosw
          add esi 04
       End_If
       add esi 08

       Mov eax 'U$  ' | stosd | dec edi
       lodsw | Mov ebx eax | Call TranslateDialogText4 | Mov al SPC| stosb ; X
       lodsw | Mov ebx eax | Call TranslateDialogText4 | Mov al SPC| stosb ; Y
       lodsw | Mov ebx eax | Call TranslateDialogText4 | Mov al SPC| stosb ; W
       lodsw | Mov ebx eax | Call TranslateDialogText4 | Mov al SPC| stosb ; H
       Mov eax ' ; D' | stosd | Mov eax 'im' | stosd | dec edi

       lodsw | Mov ebx eax | Call TranslateDialogText4
       If B$isDLGEX = &TRUE
          add esi 2
       End_If
       Mov eax ' ; I' | stosd | Mov ax 'D' | stosw

       lodsw
       If ax = 0FFFF
          Mov ebx eax | Call TranslateDialogText4 | Mov al SPC| stosb
          lodsw | Mov ebx eax | Call TranslateDialogText4
       Else
          sub esi 2 | Mov al '"' | stosb
L0:       lodsw | cmp ax 0 | je L1>
          stosb | jmp L0<
L1:       Mov al '"' | stosb | Mov ax ' 0' | stosw
       End_If
       Mov eax ' ; C' | stosd | Mov eax 'lass' | stosd | Mov al 0 | stosb

      Mov al '"' | stosb | Mov bx W$esi | inc bx | jne L0> | add esi 4 | jmp L1> ; can ResId
L0:   lodsw | cmp ax 0 | je L1>
      stosb | jmp L0<
L1:   Mov al '"' | stosb
      Mov eax ' 0 ;' | stosd | Mov eax ' Tit' | stosd | Mov ax 'le' | stosw
      Mov al 0 | stosb

      movzx eax W$esi
      If B$isDLGEX = &FALSE
         Mov ah 0
      End_If
      add esi eax ; skip CreationData; NoAlign!
L1:
      Mov eax '0 ; ' | stosd | Mov eax 'No c' | stosd | Mov eax 'reat' | stosd
      Mov eax 'ion ' | stosd | Mov eax 'data' | stosd | Mov al 0 | stosb | stosb

      add esi 5 | and esi 0-4;AlignOn 4 esi
L1:
    .End_While
    Mov al 255 | stosb
ret
;DLGTEMPLATEEX
;[0FFFF0001, D$helpID, D$EXSTYLE, D$STYLE, W$Ncntrls, W$X, W$Y, W$Width, W$Height
;{W$0 | W$-1_W$MENUID | uMENU} {W$0 | W$-1_W$CLASS | uSTRING} {W$0 | uTITLE}
;if &DS_SETFONT {W$pointsize, W$weight, W$bItalic, uFontName}] ALIGN 4
;DLGITEMTEMPLATEEX
;[D$helpID, D$exStyle, D$style, W$x, W$Y, W$w, W$h, W$id
;{W$-1_W$CLASS | uSTRING} {W$0 | W$-1_W$ResID | uTITLE}
;W$extraCount] ALIGN 4


; Set a working ID for a Menu in Resources Upload Menu (see: DialogMenuComment):

SetMenuToLoadWithDialog:
    pushad
        Mov eax D$DialogMenuTrueID, esi MenuList
        While D$esi <> eax
            If D$esi = 0
                Mov D$DialogMenuTrueID 0, D$ActualMenutestID 0
                popad | Mov eax 0 | ret
            End_If
            add esi 12
        End_While
        add esi 4
        Call 'USER32.LoadMenuIndirectA' D$esi
        Mov D$ActualMenutestID eax
    popad
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
