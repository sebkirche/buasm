TITLE CoolControls    ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;              Cool Controls are a set of functinos that enhaces the Look of
;              RosAsm Controls, like Menus, ToolBars, Dialogs, Images etc

;              Author: Guga  - January/2.006
____________________________________________________________________________________________
____________________________________________________________________________________________

; ToolBar Controls
____________________________________________________________________________________________
____________________________________________________________________________________________

; CharMap dialog image list initialisation & finalization.

;;
Proc CoolControlTB_CreateImageList:
    Arguments @H.Output, @EnabledImage, @DisabledImage, @Cx, @Cy, @Flags, @CInitial, @CGrow
    Local @Image, @Mask, @TempBuff


    lea edi D@H.Output
    Move D@TempBuff D$edi

  ; Create the images
    Call 'USER32.LoadImageA' D$H.Instance, D@EnabledImage, &IMAGE_BITMAP, 0, 0, 0
    If eax = 0
        Call ReportWinError {B$ 'CoolControl ToolBar CreateImageList: LoadImage (1 - Enabled)' EOS}
    End_If
    Mov D@Image eax

    Call 'USER32.LoadImageA' D$H.Instance, D@DisabledImage, &IMAGE_BITMAP, 0, 0, 0
    If eax = 0
        Call ReportWinError {B$ 'CoolControl ToolBar CreateImageList: LoadImage (2 - Disabled)' EOS}
    End_If
    Mov D@Mask eax

    Call 'ComCtl32.ImageList_Create' D@Cx, D@Cy, D@Flags, D@CInitial, D@CGrow
    Mov D$edi eax       ; Copy the Value store in eax to the Data in Edi (That have the address of the Outputed Buffer)
    Mov eax D@TempBuff  ; Copy our previously stored address of the outputed Buffer to eax
    Move D$eax D$edi    ; Save the Data to be outputed in the outputed Buffer at the H.Output that now is stored in eax

    Call 'ComCtl32.ImageList_Add' D@H.Output, D@Image, D@Mask
    If eax = 0-1
        Call ReportWinError {B$ 'CoolControl ToolBar: ImageList_Add' EOS}
    End_If

    Call 'GDI32.DeleteObject' D@Image
    Call 'GDI32.DeleteObject' D@Mask
EndP
;;
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
Proc CoolControlWin_CreateToolbar:
    Arguments @Addresse, @H.Output, @ToolStructure, @TotalButtons, @ToolTipArray, @tbCmdStruc
    Local @TempBuff
    pushad

    Call 'ComCtl32.InitCommonControlsEx' Init_Common_Controls

    ; Always initialize the ToolBar handle with 0

    lea edi D@H.Output
    Move D@TempBuff D$edi
    Mov eax D@TempBuff
    Mov D$eax 0

    ; Always initialize the Text Flag to FALSE

    Mov edx D@tbCmdStruc
    Mov edx D$edx+TBWIN_CMD.ShowTxtFlagDis
    Mov D$edx &FALSE

    Call CoolControlWin_CreateCommandTB D@Addresse, eax, D@ToolStructure, D@TotalButtons, D@ToolTipArray, D@tbCmdStruc
    popad
EndP
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
    CoolControlWin_CreateCommandTB
    
    This function builds the toolbar located inside a window handle.
    
    Parameters:
    
        Addresse: Address of the window handle
        
        H.Output: Pointer to the handle of the ToolBar. When the function exits, it will output to the handle
                      of the toolbar.
                      
        ToolStructure: Pointer to a array of TBBUTTON Structure containing the Data to be used for the toolbar.
                       Each element of the array is a TBBUTTON Structure related to each one of button the user wants that
                       his toolbar to have.
        
        TotalButtons:  Total amount of Buttons of the created ToolBar.
        
        ToolTipArray:   Pointer to a array of Pointers (Dwords) to Null-Terminated Strings used for ToolTips or for displaying inside
                        each Button. The amount of elements of the array needs to be the same as the amount of buttons
                        to allow that each button have a ToolTip Text String.
        
        tbCmdStruc:     Pointer to the address of a TBWin_Cmd (ToolBar windows Command) Structure that contains the initialization values
                        of the toolbar to be created.
                        
                        
                        The TBWin_Cmd Structure have the following format:
                        
                        [TBWin_Cmd:
                            bWidth: D$ 0
                            dwStyle: D$ 0
                            hMenu: D$ 0
                            hIml: D$ 0
                            ShowTxtFlag: D$ 0]
                        
                        It member's information are related below:
                        
                            bWidth: Value of the width of each Button of the ToolBar.
                            
                            dwStyle: Styles of the ToolBar. The same ones defined for usage in CreateWindowExA (User32)
                                     when you are creating a 'ToolbarWindow32' class.
                                    
                            hMenu: Menu handle or child identifier of the window
                            
                            hIml: Pointer to a Buffer containing ImageList handle for this toolBar.
                                  The handle must be previously created from the function CoolControlTB_CreateImageList.
                            
                            ShowTxtFlag: Pointer to a Buffer that contains a Flag to display or not the Text inside a Button.
                                         If the Flag is settled to &TRUE the ToolBar displays the text inside the Button,
                                         otherwise it does not display the text.
;;

;;
[CHARMAP_TOOLBAR_STYLE &WS_CHILD__&WS_VISIBLE__&TBSTYLE_FLAT__&TBSTYLE_LIST__&TBSTYLE_AUTOSIZE__&TBSTYLE_TRANSPARENT__&TBSTYLE_TOOLTIPS__&CCS_TOP]

[CHARMAPDLG_TOOLBAR 432]

[CharMapDialog_ImageList: D$ ?]

[CharMapShowTBText: D$ ?]


; Based on DebugDialog_CreateCommandTB
; CharMapToolbarHandle
[TBWin_Cmd:
 TBWin_Cmd.bWidth: D$ 20
 TBWin_Cmd.dwStyle: D$ CHARMAP_TOOLBAR_STYLE
 TBWin_Cmd.hMenu: D$ CHARMAPDLG_TOOLBAR
 TBWin_Cmd.hIml: D$ CharMapDialog_ImageList
 TBWin_Cmd.ShowTxtFlag: D$ CharMapShowTBText]

[TBWIN_CMD.bWidthDis 0
 TBWIN_CMD.dwStyleDis 4
 TBWIN_CMD.hMenuDis 8
 TBWIN_CMD.hImlDis 12
 TBWIN_CMD.ShowTxtFlagDis 16

 SizeOf_TBWIN_CMD 20]

[TBBUTTON.iBitmapDis 0
 TBBUTTON.idCommandDis 4
 TBBUTTON.fsStateDis 8
 TBBUTTON.fsStyleDis 9
 TBBUTTON._wPad1Dis 10
 TBBUTTON.dwDataDis 12
 TBBUTTON.iStringDis 16

 SizeOf_TBBUTTON 20]

Proc CoolControlWin_CreateCommandTB:

    Arguments @hwnd,
              @H.Output,
              @ToolStructure,
              @TotalButtons,
              @ToolTipArray,
              @tbCmdStruc

    Local @TBWidth

    pushad

    Mov ebx D@ToolStructure
    Mov esi 0

    Mov ecx D@H.Output ; We need to keep track on ecx because this is the output handle.

    ; Save states & clear if toolbar is Recreated
    .If D$ecx <> 0
        Do
            Push ecx | SendMessage D$ecx, &TB_GETSTATE, D$ebx+TBBUTTON.idCommandDis, 0 | Pop ecx
            Mov B$ebx+TBBUTTON.fsStateDis al
            inc esi
            add ebx SizeOf_TBBUTTON
        Loop_Until esi = D@TotalButtons

        Push ecx | Call 'USER32.DestroyWindow' D$ecx | Pop ecx
    .End_If

    ; Create toolbar
    Push ecx
        Mov edx D@tbCmdStruc
        Mov esi D@tbCmdStruc
        ; eax = D@TBWidth * D@TotalButtons
        Move D@TBWidth D$edx
        Mov eax D@TotalButtons
        mul D@TBWidth

        Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                      {B$ 'ToolbarWindow32' EOS},
                                      &NULL,
                                      D$esi+TBWIN_CMD.dwStyleDis,
                                      0,
                                      0,
                                      eax,
                                      0,
                                      D@hwnd,
                                      D$esi+TBWIN_CMD.hMenuDis,
                                      D$H.Instance,
                                      0

    Pop ecx

    Mov D$ecx eax
    Push ecx | SendMessage D$ecx, &TB_BUTTONSTRUCTSIZE, SizeOf_TBBUTTON, 0 | Pop ecx

    Mov eax D$esi+TBWIN_CMD.hImlDis
    Push ecx | SendMessage D$ecx, &TB_SETIMAGELIST, 0, D$eax | Pop ecx

    Mov ebx D@ToolStructure
    Mov edx D@tbCmdStruc
    Mov edx D$edx+TBWIN_CMD.ShowTxtFlagDis
    Mov esi 0
    Mov edi D@ToolTipArray

  ; Activate / Deactivate Text
    .If D$edx = &TRUE
        Do
            Move D$ebx+TBBUTTON.iStringDis D$edi
            inc esi
            add ebx SizeOf_TBBUTTON
            add edi 4
        Loop_Until esi = D@TotalButtons

    .Else
        Do
            Mov D$ebx+TBBUTTON.iStringDis 0
            inc esi
            add ebx SizeOf_TBBUTTON
        Loop_Until esi = D@TotalButtons

    .End_If

    Mov ebx D@ToolStructure
    Push ecx | SendMessage D$ecx, &TB_ADDBUTTONS, D@TotalButtons, ebx | Pop ecx

    popad
EndP
;;
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
Proc CoolControlTB_OnNotify:
    Arguments @hwnd, @Notification, @ToolStructure, @TotalButtons, @ToolTipArray
    Uses ebx, edx, ecx, edi

        Mov ebx D@Notification
        Mov edx D$ebx+NMHDR.idFromDis
        Mov eax D$ebx+NMHDR.codeDis

        Mov edi D@ToolStructure

        ..If eax = &TTN_NEEDTEXT
            Mov eax D$ebx+NMHDR.idFromDis
          ; Pointing with esi to the Buttons List IDs:
            lea esi D$edi+TBBUTTON.idCommandDis

            Mov ecx 0
            While D$esi <> eax
                add esi SizeOf_TBBUTTON | inc ecx
                If ecx > D@TotalButtons
                    Mov eax 0   ; mandatory for TCN_SELCHANGING !!!
                    ExitP
                End_If
            End_While

            Mov edi D@ToolTipArray
            Mov eax D$edi+ecx*4
            Mov D$ebx+TOOLTIPTEXT_lpszText eax

        ..End_If

        Mov eax 0 ; mandatory for TCN_SELCHANGING !!!
EndP
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
Proc CoolControlTabToolTip_OnNotify:
    Arguments @Notification, @TotalButtons, @ToolTipArray
    Uses ebx, edx, ecx, edi

        Mov ebx D@Notification
        Mov edx D$ebx+NMHDR.idFromDis
        Mov eax D$ebx+NMHDR.codeDis

        Mov esi 0 ; The Tab Item always starts with 0

        ..If eax = &TTN_NEEDTEXT
            Mov eax D$ebx+NMHDR.idFromDis
            Mov ecx 0
            While esi <> eax
                inc esi | inc ecx
                If ecx > D@TotalButtons
                    Mov eax 0   ; mandatory for TCN_SELCHANGING !!!
                    ExitP
                End_If
            End_While

            Mov edi D@ToolTipArray
            Mov eax D$edi+ecx*4
            Mov D$ebx+TOOLTIPTEXT_lpszText eax

        ..End_If

        Mov eax 0 ; mandatory for TCN_SELCHANGING !!!
EndP
;;
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
Proc CoolControlTabChange_OnNotify:
    Arguments @Notification, @TabSelected, @MainTab

        Mov ebx D@Notification
        Mov eax D$ebx+NMHDR.codeDis
        lea edi D@TabSelected
        Mov edi D$edi

        ..If eax = &TCN_SELCHANGE

            ;Tab selection. We are sending the message on the Main Tab Control, in the main dialog.
            ; At ebx we have the handle of the MainTab = htab
            Call 'USER32.SendMessageA' D$ebx &TCM_GETCURSEL 0 0
            .If eax <> D$edi

                Push eax
                Mov eax D$edi
                Call 'USER32.ShowWindow' D$eax*4+H.TabDlg1 &SW_HIDE
                Pop eax
                Mov D$edi eax
                Call 'USER32.ShowWindow' D$eax*4+H.TabDlg1 &SW_SHOWDEFAULT

            .End_If

        ..End_If
EndP
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

; Tab Controls
____________________________________________________________________________________________
____________________________________________________________________________________________

[RECT.leftDis 0
 RECT.topDis 4
 RECT.rightDis 8
 RECT.bottomDis 12]

;[Size_Of_RECT 16]
;;
Proc CoolControlDialog_AdjustDataTabSize:
    Arguments @hwnd, @nIDDlgItem, @hWndPage, @Rect

    Local @H.Tab

    pushad

    Call 'USER32.GetDlgItem' D@hwnd, D@nIDDlgItem
    Mov D@H.Tab eax

    Mov esi D@Rect
    Call 'USER32.GetClientRect' D@H.Tab, esi
    SendMessage D@H.Tab, &TCM_ADJUSTRECT, &FALSE, esi
    Mov eax D$esi+RECT.leftDis
    Mov ebx D$esi+RECT.topDis
    Mov ecx D$esi+RECT.rightDis
    Mov edx D$esi+RECT.bottomDis
    sub ecx eax
    sub edx ebx
    Call 'USER32.SetWindowPos' D@hWndPage, 0, eax, ebx, ecx, edx, &SWP_NOZORDER

    popad
EndP
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

; ToolBar Controls
____________________________________________________________________________________________
____________________________________________________________________________________________

[NM_LISTVIEW.hdr.hwndFromDis 0
 NM_LISTVIEW.hdr.idFromDis 4
 NM_LISTVIEW.hdr.codeDis 8
 NM_LISTVIEW.iItemDis 12
 NM_LISTVIEW.iSubItemDis 16
 NM_LISTVIEW.uNewStateDis 20
 NM_LISTVIEW.uOldStateDis 24
 NM_LISTVIEW.uChangedDis 28
 NM_LISTVIEW.ptAction.xDis 32
 NM_LISTVIEW.ptAction.yDis 36
 NM_LISTVIEW.lParamDis 40]

;;

    CoolControl_ListViewAlternateSort
    
    This function reorganizes (sorts) each one of the columns on a ListView Control. 
    
    Parameters:
    
        hLView:         Address of a Sort function where the user defines how each one of the ListView itens is displayed.
        
        Notification:   Pointer to the Notification message handle (lParam, for example)
                      
        SortItem:       Address of the Buffer responsable for the Sorting Itens
        
        H.List:          Pointer to the handle of the ListView Control.
        
        LViewNumber:    Total amount of Columns used on the listview.


    Returned values: This function does not returns any value.

    Usage Example:
        Call CoolControl_ListViewAlternateSort ListViewSort, D@Notification, SortDecimal, D$hCharMapList, 5    

    Note: The functionality of this function is exactly the same as if we defined each listview item by hand, like
          if when we have, for example 5 itens (columns) on a listview, defined as:
          
                .If D$ebx+NM_LISTVIEW.iSubItemDis = 0 ; decimal
                    Call AlternateSorting ListViewSort, SortDecimal, D$hCharMapList, 1, 2
                .Else_If D$ebx+NM_LISTVIEW.iSubItemDis = 1 ; hexadecimal
                    Call AlternateSorting ListViewSort, SortDecimal, D$hCharMapList, 3, 4
                .Else_If D$ebx+NM_LISTVIEW.iSubItemDis = 2 ; Char
                    Call AlternateSorting ListViewSort, SortDecimal, D$hCharMapList, 5, 6
                .Else_If D$ebx+NM_LISTVIEW.iSubItemDis = 3 ; Count
                    Call AlternateSorting ListViewSort, SortDecimal, D$hCharMapList, 7, 8
                .Else_If D$ebx+NM_LISTVIEW.iSubItemDis = 4 ; Percent
                    Call AlternateSorting ListViewSort, SortDecimal, D$hCharMapList, 9, 10
                .End_If          
;;
;;
Proc CoolControl_ListViewAlternateSort:
    Arguments @hLView, @Notification, @SortItem, @H.List, @LViewNumber
    Local @InitValue, @NextValue

    pushad

    Mov ebx D@Notification
    Mov edx D$ebx+NMHDR.idFromDis
    Mov eax D$ebx+NMHDR.codeDis

    Mov D@InitValue 1
    Mov D@NextValue 2

    Mov ecx 0 ; Column Counter. Always starts at the 1st column.

    .While ecx <> D@LViewNumber
        If D$ebx+NM_LISTVIEW.iSubItemDis = ecx
            Call AlternateSorting D@hLView, D@SortItem, D@H.List, D@InitValue, D@NextValue
            jmp L1> ; Once we identify wich column we are working with, we go out of the loop
        End_If
        inc ecx
        add D@InitValue 2
        add D@NextValue 2
    .End_While

L1: popad
EndP
;;
________________________________________________________________________________________

Proc AlternateSorting:
    Arguments @hLView, @SortItem, @H.List, @InitValue, @NextValue
    pushad

    Mov edi D@SortItem
    If D$edi = &FALSE
        Call 'USER32.SendMessageA' D@H.List, &LVM_SORTITEMS, D@InitValue, D@hLView
        Call Resequence D@H.List
        Mov edi D@SortItem
        Mov D$edi &TRUE
    Else
        Call 'USER32.SendMessageA' D@H.List, &LVM_SORTITEMS, D@NextValue, D@hLView
        Call Resequence D@H.List
        Mov edi D@SortItem
        Mov D$edi &FALSE
    End_If

    popad
EndP

________________________________________________________________________________________

Proc Resequence:
    Arguments @H.List
    Structure @lviResequence 40, lviResequence.imaskDis 0, lviResequence.iItemDis 4, lviResequence.iSubItemDis 8, lviResequence.stateDis 12, lviResequence.stateMaskDis 16, lviResequence.pszTextDis 20, lviResequence.cchTextMaxDis 24, lviResequence.iImageDis 28, lviResequence.lParamDis 32, lviResequence.iIndentDis 36

    pushad

    Call 'USER32.SendMessageA' D@H.List, &LVM_GETITEMCOUNT, 0, 0
    Mov edi eax
    Mov D$lviResequence.imaskDis &LVIF_PARAM
    Mov D$lviResequence.iSubItemDis 0
    Mov D$lviResequence.iItemDis 0

    While edi <> 0
        Push D$lviResequence.iItemDis
        Pop D$lviResequence.lParamDis
        Call 'USER32.SendMessageA' D@H.List, &LVM_SETITEM, 0, D@lviResequence
        inc D$lviResequence.iItemDis
        dec edi
    End_While

    popad

EndP
________________________________________________________________________________________

;;

    CoolControl_LVBeginSort
    
    This function initializes the sorting state reorganizes (sorts) each one of the columns on a ListView Control. 
    
    Parameters:
    
        hLView:         Address of a Sort function where the user defines how each one of the ListView itens is displayed.
        
        SortItem:       Address of the Buffer responsable for the Sorting Itens
        
        H.List:          Pointer to the handle of the ListView Control.
        
        InitValue:      Initial value of the item the user wants to start first. If the user wants to 
                        start sorting biased on the 1st column (value = 0), theInitValue is equal to 1.
                        If it is biased on the 2nd column, the initvalue is 3.
                        If it is biased on the 3rd column, the initvalue is 5
                        If it is biased on the 4th column, the initvalue is 7, and so on.
                        
                        Note: By default, the InitValue should be settled to 1.

    Returned values: This function does not returns any value.

    Usage Example:
        Call CoolControl_LVBeginSort ListViewSort, SortDecimal, D$hCharMapList, 1

;;
;;
Proc CoolControl_LVBeginSort:
    Arguments @hLview, @SortItem, @H.List, @InitValue

    pushad
    Call 'USER32.SendMessageA' D@H.List, &LVM_SORTITEMS, D@InitValue, D@hLview
    Call Resequence D@H.List
    Mov edi D@SortItem
    Mov D$edi &TRUE
    popad

EndP
;;
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
