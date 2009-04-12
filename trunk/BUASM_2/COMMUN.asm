TITLE COMMUN         ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

[HIDE_PUSHBUTTON_OK 1]

SetIconDialog:
    ______________________________________________

    ; Attribue l'icône de BUAsm au dialog courant
    ______________________________________________

    Call 'USER32.SetClassLongA' D$HWND,
                                &GCL_HICON,
                                D$H.MainIcon

ret
____________________________________________________________________________________________

WM_CTLCOLOREDIT:
    __________________________________________________

    ; Attribue RVB fond utilisateur au dialog courant
    __________________________________________________

  ;  Call 'USER32.SendMessageA' D$LPARAM,
  ;                             &EM_SETSEL,
  ;                             0-1,
  ;                             0

     Call 'GDI32.SetTextColor' D$WPARAM,
                               D$RVBA.Normal

    Call 'GDI32.SetBkColor' D$WPARAM,
                            D$RVBA.DialogsBackgnd

    ; Eax doit être rtourné à l'OS dans le dialog
    Mov eax D$H.DialogsBackGroundBrush

ret
____________________________________________________________________________________________

WM_CLOSE:
    ______________________________

    ; Fermeture du dialog courant
    ______________________________

    Call 'USER32.EndDialog' D$HWND,
                            0

ret
____________________________________________________________________________________________

[STRUC.MSGBOXPARAMS:
 @cbSize: D$ len
 @hwndOwner: D$ 0
 @hInstance: D$ 0
 @lpszText: D$ 0
 @lpszCaption: D$ 0
 @dwStyle: D$ 0
 @lpszIcon: D$ 1
 @dwContextHelpId: D$ 0
 @lpfnMsgBoxCallback: D$ 0
 @dwLanguageId: D$ 0]

[FL.MsgBoxReturn: D$ ? ]

Proc MessageBox:
    ___________________________________________________

    ; Argument 1 -> STR.A.Title
    ; Argument 2 -> STR.A.Text,

    ; Argument 3 ->

    ;  Simple information + OK:
    ;    &MB_USERICON

    ;  Information complexe:
    ;    &MB_SYSTEMMODAL+&MB_ICONEXCLAMATION+&MB_YESNO
    ___________________________________________________

    Arguments @STR.A.Caption,
              @STR.A.Text,
              @DU.Style

    pushad

        Mov eax D@STR.A.Caption,
            D$STRUC.MSGBOXPARAMS@lpszCaption eax,
            eax D@STR.A.Text,
            D$STRUC.MSGBOXPARAMS@lpszText eax,
            eax D@DU.Style,
            D$STRUC.MSGBOXPARAMS@dwStyle eax

        Call 'USER32.MessageBoxIndirectA' STRUC.MSGBOXPARAMS

        Mov D$FL.MsgBoxReturn eax

    popad

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________
;EOT



