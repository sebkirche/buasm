TITLE MainProc        ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

; Equates given by the Menu Editor:

; Tag Menu 1000
[M00_Menu  1000                  M00_Open  1001                  M00_New  1002
 M00_New_Model  1003             M00_Paste_at_Pos  1004          M00_Change_Compile_Name  1005
 M00_Open_Source_Only  1006      M00_Replace_Source_Only  1007   M00_Save_Source_Only  1008
 M00_Print  1009                 M00_Output  1010                M00_Exit  1011
 M00_Tree  1012                  M00_Import  1013                M00_Export  1014
 M00_Find  1015                  M00_Replace  1016               M00_Undo  1017
 M00_Redo  1018                  M00_Copy  1019                  M00_Paste  1020
 M00_Delete  1021                M00_Cut  1022                   M00_Compile  1023
 M00_Run  1024                   M00_Optimize_Jumps_Sizes  1025  M00_Calc  1026
 M00_Ascii_Table  1027           M00_Show_BUAsm_Mems  1028       M00_Show_Symbols_Repartition  1029
 M00_Serial_Compilations  1030   M00_Data_to_Equates  1031       M00_Encoding  1032
 M00_DLLs_Scanner  1033          M00_Configuration  1034         M00_Create_Config_bin  1035
 M00_Main_Icon  1036             M00_Load_Icon  1037             M00_Delete_Icon  1038
 M00_Icon_IDs  1039              M00_Save_Icon  1040             M00_Load_BitMap  1041
 M00_Delete_BitMap  1042         M00_BitMaps_IDs  1043           M00_Save_BitMap  1044
 M00_Load_Cursor  1045           M00_Delete_Cursor  1046         M00_Cursors_IDs  1047
 M00_Save_Cursor  1048           M00_Load_Wave  1049             M00_Delete_Wave  1050
 M00_Waves_IDs  1051             M00_Save_Wave  1052             M00_Load_Avi  1053
 M00_Delete_Avi  1054            M00_Avi_IDs  1055               M00_Save_Avi  1056
 M00_Load_RC  1057               M00_Delete_RC  1058             M00_RCs_IDs  1059
 M00_Save_RC  1060               M00_New_Dialog  1061            M00_Load_from_Resources  1062
 M00_Load_from_ClipBoard  1063   M00_Load_from_File  1064        M00_Save_to_Binary_File  1065
 M00_Load_from_Binary_File  1066 M00_Replace_from_Binary_File  1067
 M00_Delete_Resources_Dialog  1068                               M00_Strings  1069
 M00_New_Menu  1070              M00_Existing_Menu  1071         M00_Delete_a_Menu  1072
 M00_Save_to_Binary_Menu_File  1073                              M00_Load_Binary_Menu_File  1074
 M00_Replace_from_Binary_Menu_File  1075                         M00_Clip_File  1076
 M00_Structures  1077            M00_Api_Functions  1078         M00_Sys_Resources  1079
 M00_GUIDs  1080                 M00_Win32_Equates  1081         M00_Win32_Data_Types  1082
 M00_Wizards  1083               M00_B_U_Asm  1084               M00_Sources_Editor  1085
 M00_Visual_Tuts  1086           M00_Win32_hlp  1087             M00_Mmedia_hlp  1088
 M00_OpenGl_hlp  1089            M00_WinSock_hlp  1090           M00_Dx_hlp  1091
 M00_SDL  1092                   M00_sqlite  1093                M00_DevIl  1094
 M00_About  1095                 M00_GPL_License  1096           M00_RosAsm_License  1097
 M00_<_  1098                    M00_>  1099]

[M00_About_ToolBar 1200]
____________________________________________________________________________________________

[FL.ShowStats: D$ &TRUE]

[FL.Redraw: D$ ?
 FL.ReadyToRun: D$ ?
 FL.UnusedSymbolsDialogWanted: D$ ?]

; Buffer for returned filename
[LP.File: B$ ? # &MAX_PATH]

[STR.A.ApplicationName: B$ ' Bottom Up Assembler: V. 03.00.004A' EOS]

Proc MainWindowProc:

; [TOPCODE]

;;
    
    TEXT
    –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
    TEXT
    ___________________________________________________________________________
    ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
    TEXT

    ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
    ___________________________________________________________________________
    ¦TEXT¦TEXT¦ ...  *** /// <TEXT> | TEXT |^^^^^|----| (0123456789) |{texte}|
    ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
   
    TEXT
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯
    TEXT
    _ _ _ _ _ _ _ _ _ 
     ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯
    TEXT
     
    SYMBOLS: 45×6= ¼½¾  ÷ ± ø ^² ^³ ^¹ ^° º

    
    
    
    
    [MAIN]
    
    

    MAJ:
    
    Bp                                  ->  BreakPoint

    Macros évocation                    ->  mov/Mov pus/Push pop/Pop Move/Move If Else_IF End_If
    Début révision cursor               ->  ID_TIMER_CURSOR

    Début révision [CreateTitleTab]     ->  Changement des propriétés de la fonte et maintient
                                            du scroll molette pendant les changements de TITLEs.
                                            Affichage en une seule fois sans clignotement. 
[GetActualPartFromPos]
[ExtendMemory]

[OpenBUAsmPE]
[StoreChoosenName]

    Début révision [MainWindowProc]     ->  Restructuration avant démontage
 
    Révision [StatusBar]
 
    [ResizeScrollBar] refaire au model les autres fenêtres !!! -> MainResize: CreateEditWindow

;;

;;
    
  [CreateTitleTab]
    
  [NewReplaceMacAndEqu] [GetFileNameFromPath]
  
  'CheckMRUFile', 'RightClick','WheelMsg'
  'ShowUnfoldMacro', 'UnfoldMacro', 'ShowUnfoldDialog'
  'StructDialog'; 'NewFileNameDialog'
  'EncodeLines', 'StoreFlatData', 'UpdateTitlesFromIncludeFiles'
  
  'Main', 'AsmMain, 'checksum64' 'OutOnError'
  
  'NewReplaceMacAndEqu', 'zReplaceEquates'
  
  'KillTrailingSpaces', 'NewFileNameDialog', 'DataToStructureProc'

;;

    Arguments @hwnd,
              @msg,
              @wParam,
              @lParam

    Mov eax D@hwnd

    Comp eax D$H.MainWindow = S3>

    Comp eax D$H.EditWindow <> S1>

        Mov edx D$STRUC.WINDOWCLASS@hCursor | Comp D$H.CurrentCursor edx = S3> | jmp S2>

S1: Comp eax D$H.ScrollBarWindow = S3>

    Comp eax D$H.BreakPointWindow <> S1>

        Mov edx D$H.CursorARROW | Comp D$H.CurrentCursor edx = S3> | jmp S2>

    ; This may happend when [Run]ing. This works because we do not hold anything at creation time
S1: Call 'USER32.DefWindowProcA' D@hwnd,
                                 D@msg,
                                 D@wParam,
                                 D@lParam

EndP
    __________________

    ; MAJ du curseur: TODO déclarer les Curseurs dans la classe ?!?
    __________________

S2: Mov D$H.CurrentCursor edx

    Call 'USER32.SetClassLongA' eax,
                                &GCL_HCURSOR,
                                edx
    ____________________________

    ; General purpose Callback:
    ____________________________

S3: Mov eax D@msg

    ...If eax = &WM_KEYDOWN
        .If D$FL.SourceReady = &TRUE
            movzx eax B@Wparam | Mov B$Keys+eax 1

            Call KeyMessage ; CharMessage

            On D$FL.BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
            If B$KeyHasModifedSource = &TRUE

                Mov D$FL.SourceHasChanged &TRUE
                Call AskForRedraw |
              jmp @NotReadyToRun
           ; Else_If B$KeyHasMovedCaret = &TRUE
           ;     Call AskForRedraw
            End_If

        .Else_If D@wParam = &VK_F1
            Call BUAsmHelp

        .Else_If D@wParam = &VK_F2
            Call F2Help

        .End_If

    ...Else_If eax = &WM_KEYUP
        movzx eax B@Wparam | Mov B$Keys+eax 0
        On eax = &VK_MENU, Mov B$keys+&VK_CONTROL &FALSE

    ...Else_If eax = &WM_GETMINMAXINFO

            ; Dimensions minimum de la MainWindow TODO aligner la toolbar dessus !!!
            Mov eax D@lParam,
                D$eax+MINMAXINFO.ptTrackSize+POINTX 500,
                D$eax+MINMAXINFO.ptTrackSize+POINTY 300

        jmp @ExitFalse ; !!!

    ...Else_If eax = &WM_SIZE

        Call MainResize

    ...Else_If eax = &WM_WINDOWPOSCHANGED

        Call TabResize

    ...Else_If eax = &WM_DROPFILES

        Call 'SHELL32.DragQueryFile' D@wParam,
                                     0,
                                     LP.File,
                                     &MAX_PATH

        Mov esi LP.File,
                edi SaveFilter

        while B$esi <> 0 | movsb | End_While | movsb
        Call DirectLoad
        Call StartEdition
        Call ReInitUndo
        Call SetPartialEditionFromPos
        Call EnableMenutems
        Call LoadBookMarks
        Call AskForRedraw

    ...Else_If eax = &WM_DESTROY
        On D$FL.DialogEdition = &TRUE, Call CloseDialogEdition
        Call DeleteUndoFiles | Call KillUndo
        Call ReleaseResourceMemory
        Call 'USER32.PostQuitMessage' &WM_NULL

        jmp @ExitFalse ; !!!

    ...Else_If D@msg = &WM_NOTIFY

        Call KillCompletionList ; !!! ???

        ; Edx > NMHDR // eax > hwndFrom.
        Mov edx D@lParam, eax D$edx+TOOLTIPTEXT_NMHDR_hwndFrom

        ; Tab (TITLEs)
        .If eax = D$H.TabWindow

            If D$edx+TOOLTIPTEXT_NMHDR_code = &TCN_SELCHANGE

                Call WM_BUTTON_Tab

            End_If

        .Else_If eax = D$H.StatusBar

            If D$edx+TOOLTIPTEXT_NMHDR_code = &NM_CLICK

                Mov eax D$edx+NMMOUSE_HitInfo

                ; Ecx = Nombre byte dans table StatusPartsPos
                Mov edx TABLE.StatusPartsPos,
                    ecx D$edx-(1*DWORD)

                sub ecx (2*DWORD)

                jmp S1>

            L0: sub ecx DWORD

            S1: Comp eax D$edx+ecx < L0<

                ; Si selection de l'ID 1 ouvrir la fenêtre TITLEs
                On ecx = ((ID_STATUS_BAR_TITLE-1)*DWORD) Call TITLESOnOff

            End_If

        ; ToolBar Messages:
        .Else_If D$edx+TOOLTIPTEXT_NMHDR_code = &TTN_NEEDTEXT
            Mov eax D$edx+TOOLTIPTEXT_NMHDR_idfrom
          ; Pointing with esi to the Buttons List IDs:
            lea esi D$ToolBarButtons+4
            Mov ecx 0
            While D$esi <> eax
                add esi 20 | inc ecx | On ecx > TOOLBUTTONS_NUMBER jmp @ExitFalse ; !!!ExitP
            End_While
            Mov eax D$PointersToToolTipsStrings+ecx*4
            Mov D$edx+TOOLTIPTEXT_lpszText eax

        .Else_If D$edx+TOOLTIPTEXT_NMHDR_code = &TBN_QUERYINSERT ; May be inserted ?

            Mov eax &TRUE  ; > yes for all.

EndP

        .Else_If D$edx+TOOLTIPTEXT_NMHDR_code = &TBN_QUERYDELETE ; May be deleted?

                        Mov eax &TRUE         ; > yes for all.

EndP

        .Else_If D$edx+TOOLTIPTEXT_NMHDR_code = &TBN_FIRST  ; = &TBN_GETBUTTONINFO (missing?)
            Mov ecx D$edx+TB_NOTIFY_Item ;, edx ecx
            If ecx > TOOLBUTTONS_NUMBER

                jmp @ExitFalse ; !!!

            End_If
            lea ecx D$ecx*4+ecx | shl ecx 2                 ; ecx = ecx * 20 >>>
            add ecx ToolBarButtons                          ; Pointer to whished Button Data
            Move D$edx+TB_NOTIFY_TBBUTTON_iBitmap D$ecx
            Move D$edx+TB_NOTIFY_TBBUTTON_idCommand D$ecx+4
            Move D$edx+TB_NOTIFY_TBBUTTON_fsState D$ecx+8
            Move D$edx+TB_NOTIFY_TBBUTTON_dwData D$ecx+12
            Move D$edx+TB_NOTIFY_TBBUTTON_iString D$ecx+16
            Mov edi D$edx+TB_NOTIFY_TextPtr
            Mov ecx D$edx+TB_NOTIFY_Item | shl ecx 2        ; Displacement to pointers
            Mov eax PointersToToolTipsStrings | add eax ecx ; Pointer
            Mov esi D$eax                                   ; Source
            Mov ecx 0-1                                     ; Counter for Non-zero-ended text
            Do
                lodsb | stosb | inc ecx
            Loop_Until al = 0
            Mov D$edx+TB_NOTIFY_CharCount ecx               ; Lenght of String    (+36)

            Mov eax &TRUE

EndP

        .Else_If D$edx+TOOLTIPTEXT_NMHDR_code = &TBN_TOOLBARCHANGE
            Mov B$ToolBarChange &TRUE                       ; For Saving at Exit if TRUE.

            ; Next instruction of no use usually, but, under some unknown circumstances
            ; the Buttons Size may be changed (Win98 without IE). So, as it can't hurt:
            Call 'USER32.SendMessageA' D$H.ToolBar, &TB_SETBUTTONSIZE, 0, 014_0014

        .Else_If D$edx+TOOLTIPTEXT_NMHDR_code = &TBN_RESET
            Call 'USER32.SendMessageA' D$H.ToolBar, &TB_SAVERESTORE, &FALSE,
                                    TOOLBAR_REGISTRY        ; FALSE > restore

        .Else_If D$edx+TOOLTIPTEXT_NMHDR_code = &TBN_CUSTHELP
            Call 'USER32.MessageBoxA' D$H.MainWindow, HelpToolBar, HelpToolBarTitle,
                                    &MB_OK__&MB_SYSTEMMODAL
        .End_If
    ____________________________________________________________

    ; Main window CallBack: User choices open-file independant:
    ____________________________________________________________

    ...Else_If eax = &WM_COMMAND

        Call KillCompletionList

       Mov eax D@wParam | and eax 0FFFF

        If D$FL.Includes = &FALSE

            Call Configuration | Call 'USER32.SendMessageA' D$H.MainWindow,
                                                            &WM_CLOSE,
                                                            0,
                                                            0

        End_If

        ..If eax = M00_Open

            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse

            Call Security | On eax = &IDCANCEL jmp @ExitFalse

            Call OpenBUAsmPE

            Call UpdateTitlesFromIncludeFiles

            If D$SaveFilter <> 0

                Call ReInitUndo

                Call SetPartialEditionFromPos | Call EnableMenutems

                Call LoadBookMarks

            End_If

        ..Else_If eax = M00_Open_Source_Only

            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse ; !!!

            Call Security | On eax = &IDCANCEL, jmp @ExitFalse

            Call LooseResources
            .If B$KeepResources = &FALSE
                Call ReInitUndo | Call OpenSourceOnly
                Call UpdateTitlesFromIncludeFiles
                If D$SourceLen > 0
                    Call SetPartialEditionFromPos | Call EnableMenutems
                    Call LoadBookMarks
                End_If
            .End_If

        ..Else_If eax = M00_Exit

            Call 'USER32.SendMessageA' D$H.MainWindow &WM_CLOSE 0 0

        ..Else_If eax = M00_Sources_Editor
            Call Help, B_U_AsmName, SourceEditor, ContextHlpMessage

        ..Else_If eax = M00_Main_Icon
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call IconEdition | jmp @NotReadyToRun

        ..Else_If eax = M00_New_Menu
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call NewMenu | Mov D$ActualMenutestID 0 | Call EnableMenutems | jmp @NotReadyToRun

        ..Else_If eax = M00_Existing_Menu
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call ExistingMenu | Mov D$ActualMenutestID 0 | jmp @NotReadyToRun

        ..Else_If eax = M00_Delete_a_Menu
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call DeleteMenu | Mov D$ActualMenutestID 0 | Call EnableMenutems | jmp @NotReadyToRun

        ..Else_If eax = M00_Save_to_Binary_Menu_File
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call SaveMenuBinaryFile

        ..Else_If eax = M00_Load_Binary_Menu_File
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call LoadMenuBinaryFile | jmp @NotReadyToRun

        ..Else_If eax = M00_Replace_from_Binary_Menu_File
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call ReplaceMenuBinaryFile | jmp @NotReadyToRun

        ..Else_If eax = M00_About | Call AboutBox

        ..Else_If eax = M00_About_ToolBar | Call About_ToolBar

        ..Else_If eax = M00_GPL_License | Call GPLView

        ..Else_If eax = M00_RosAsm_License | Call LicenseView

        ..Else_If eax = M00_B_U_Asm | Call BUAsmHelp

        ..Else_If eax = M00_Configuration | Call Configuration

        ..Else_If eax = M00_Create_Config_bin | Call Create_Config_bin

        ..Else_If eax = M00_Api_Functions | Call ViewApiList

        ..Else_If eax = M00_New
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call Security | On eax = &IDCANCEL jmp @ExitFalse ; !!!
            Call AutoNew
            and D$FL.RelocsWanted 0 ; jE!
            Call ChangeName
            cmp D$SavingExtension '.DLL' | setz B$FL.RelocsWanted ; jE! Relocs for Dll
            Call StartNewFile

        ..Else_If eax = M00_New_Model
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call Security | On eax = &IDCANCEL jmp @ExitFalse ; !!!
            Call NewFileNameDialog

        ..Else_If eax = M00_Change_Compile_Name | Call ChangeName

        ..Else_If eax = M00_Calc | Call Calc

        ..Else_If eax = M00_DLLs_Scanner | Call ExportScanner

      ;  ..Else_If eax = M00_Libs_Scanner | Call LibScanner

        ..Else_If eax = M00_New_Dialog
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            If D$FL.DialogEdition = &FALSE
                Mov B$SameIdAllowed &FALSE
                On D$FL.BlinkingCaretWanted = &TRUE, Call KillBlinkCursor
                    Call InitDialogEdition | Call ReleaseDialogMemories
                On D$FL.BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
                Call EnableMenutems | jmp @NotReadyToRun
            End_If

        ..Else_If eax = M00_Load_from_ClipBoard
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            If D$FL.DialogEdition = &FALSE
                Mov B$SameIdAllowed &FALSE
                On D$FL.BlinkingCaretWanted = &TRUE, Call KillBlinkCursor
                    Call ReleaseDialogMemories | Call LoadDialogFromClipBoard
                On D$FL.BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
                Call EnableMenutems | jmp @NotReadyToRun
            End_If

        ..Else_If eax = M00_Load_from_File
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            If D$FL.DialogEdition = &FALSE
                Mov B$SameIdAllowed &FALSE
                On d$FL.BlinkingCaretWanted = &TRUE, Call KillBlinkCursor
                    Call ReleaseDialogMemories | Call LoadDialogFromFile
                On D$FL.BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
                Call EnableMenutems | jmp @NotReadyToRun
            End_If

        ..Else_If eax = M00_Load_from_Resources
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            If D$FL.DialogEdition = &FALSE
                Mov B$SameIdAllowed &TRUE
                On D$FL.BlinkingCaretWanted = &TRUE, Call KillBlinkCursor
                    Call ReleaseDialogMemories | Call LoadFromResources
                On D$FL.BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
                jmp @NotReadyToRun
            End_If

        ..Else_If eax = M00_Save_to_Binary_File
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call SaveToBinaryFile

        ..Else_If eax = M00_Load_from_Binary_File
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Mov B$SameIdAllowed &TRUE
            Call LoadFromBinaryFile | Call EnableMenutems | jmp @NotReadyToRun

        ..Else_If eax = M00_Replace_from_Binary_File
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call ReplaceFromBinaryFile | jmp @NotReadyToRun

        ..Else_If eax = M00_Delete_Resources_Dialog
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            On D$FL.BlinkingCaretWanted = &TRUE, Call KillBlinkCursor
                Call DeleteDialog | Call EnableMenutems
            On D$FL.BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
            jmp @NotReadyToRun

        ..Else_If eax = M00_Load_BitMap
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call LoadBitMap | Call EnableMenutems | jmp @NotReadyToRun

        ..Else_If eax = M00_Delete_BitMap
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call DeleteBitMap | Call EnableMenutems | jmp @NotReadyToRun

        ..Else_If eax = M00_BitMaps_IDs | Call ShowBitMapsIds

        ..Else_If eax = M00_Load_Wave
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call ReadWaveFile | Call EnableMenutems | jmp @NotReadyToRun

        ..Else_If eax = M00_Delete_Wave
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call DeleteWave | Call EnableMenutems | jmp @NotReadyToRun

        ..Else_If eax = M00_Load_Avi
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call ReadAviFile | Call EnableMenutems | jmp @NotReadyToRun

        ..Else_If eax = M00_Delete_Avi
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call DeleteAviFile | Call EnableMenutems | jmp @NotReadyToRun

        ..Else_If eax = M00_Load_RC
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call ReadRcData | Call EnableMenutems | jmp @NotReadyToRun

        ..Else_If eax = M00_Save_RC
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call SaveRcData | jmp @NotReadyToRun

        ..Else_If eax = M00_Delete_RC
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call DeleteRcData | Call EnableMenutems | jmp @NotReadyToRun

        ..Else_If eax = M00_Ascii_Table | Call AsciiTable

        ..Else_If eax = M00_Win32_hlp | Call Win32_Hlp

        ..Else_If eax = M00_Mmedia_hlp | Call Mmedia_Hlp

        ..Else_If eax = M00_OpenGl_hlp | Call OpenGl_Hlp

        ..Else_If eax = M00_WinSock_hlp | Call WinSock_Hlp

        ..Else_If eax = M00_Dx_hlp | Call Dx_Hlp

        ..Else_If eax = M00_SDL | Call SDL_hlp

        ..Else_If eax = M00_sqlite | Call sqlite_Hlp

        ..Else_If eax = M00_DevIl | Call DevIL_Hlp

        ..Else_If eax = M00_Win32_Data_Types | Call ShowTypes

        ..Else_If eax = M00_Win32_Equates | Call ShowEquates

        ..Else_If eax = M00_Strings
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call StringsResources | jmp @NotReadyToRun

        ..Else_If eax = M00_Load_Cursor
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call ReadCursor | Call EnableMenutems | jmp @NotReadyToRun

        ..Else_If eax = M00_Delete_Cursor
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call DeleteCursor | Call EnableMenutems | jmp @NotReadyToRun

        ..Else_If eax = M00_Load_Icon
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call ReadIcon | Call EnableMenutems | jmp @NotReadyToRun

        ..Else_If eax = M00_Delete_Icon
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse
            Call DeleteIcon | Call EnableMenutems | jmp @NotReadyToRun

        ..Else_If eax = M00_Clip_File | Call Templates
           ; Call SetQwordCheckSum String2

        ..Else_If eax = M00_Data_to_Equates | Call DataToStructure

        ..Else_If eax = M00_Structures | Call StructDialog

        ..Else_If eax = M00_Sys_Resources | Call ViewSysResources

        ..Else_If eax = M00_GUIDs | Call ViewGUIDs

        ..Else_If eax = M00_Show_BUAsm_Mems | Call ViewBUAsmMems

        ..Else_If eax = M00_Show_Symbols_Repartition | Call TestRepartition

      ;  ..Else_If eax = M00_Local_mem_Tests | Call TestLocals

        ..Else_If eax = M00_Serial_Compilations | Call MultipleCompileTests

        ..Else_If eax = M00_Encoding
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse

            Call ViewEncoding

            jmp @NotReadyToRun

        ..Else
            .If eax > 6999                      ; Clip Files
                Call Templates

            .Else_If eax > 5999                  ; Wizards
                Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse

                On D$FL.SourceReady = &FALSE,
                    Call 'USER32.SendMessageA', D$H.MainWindow, &WM_COMMAND, M00_New, 0

                On D$FL.SourceReady = &TRUE Call NewWizardForm

            .Else_If eax > 4999                  ; Visual Tuts
                If eax < 5100
                   Call VisualTuts
                Else
                    jmp L5>>
                End_If

            .Else_If eax > 3999             ; Added FloatMenu under [Struct].
                If eax < 4010
                    Mov D$MenuID eax | Call StructDialog ; OpenStructureFile
                Else
                    jmp L5>>
                End_If

            .Else_If eax > 3000             ; Most Recently Used Files.
                If eax < 3005
                    Push eax
                    Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse

                        Call Security       ; 'Security' also uses eax as return Value.
                    Pop ebx
                    On eax = &IDCANCEL jmp @ExitFalse  ; !!!
                        Mov eax ebx         ; ... and 'LoadRMUfile' as input.
                        Call LoadRMUfile
                        On eax = &FALSE, jmp L5>
                        Call SetPartialEditionFromPos
                        Call LoadBookMarks | Call EnableMenutems | Call LoadBookMarks
                Else
                    jmp L5>
                End_If
            .Else_If eax > 1999             ; User defined menu:
                If eax < 2009
                    Mov eax D@wParam | Call UserDefinedAction
                Else
                    jmp L5>
                End_If
            .Else
                jmp L5>
            .End_If
        ..End_If

        jmp @ExitFalse ; !!!

    ...Else_If eax = &WM_CLOSE

        Call WM_CLOSE_MainWindow

        jmp @ExitFalse ; !!!

    ...End_If

L5: test D$FL.SourceReady &TRUE ZERO @NoClient
    ________________________________________________________

    ; Main window CallBack going on: With opened file only:
    ________________________________________________________

    Mov eax D@msg

    ...If eax = &WM_PAINT

            .If D$CodeSource > 0

                Call TextPos

                Mov eax D@hwnd

                If eax = D$H.EditWindow

                    Call PrintColorText

                Else_If eax = D$H.BreakPointWindow

                    Call PrintBreakPoint

                End_If

                If B$MovingBlock = &TRUE

                    Call SetBlock | Call AskForRedraw

                End_If

            Call StatusBar

                Mov D$FL.Redraw &FALSE

            .End_If

        Mov eax D$H.EditWindow | On D@hwnd <> eax jmp @NoClient

    ...Else_If eax = &WM_CHAR
        ..If D$FL.Redraw = &FALSE
            Mov D$FL.Redraw &TRUE

            Call CharMessage D@wParam | or D$FL.SourceHasChanged eax

            Call AskForRedraw | jmp @NotReadyToRun
        ..End_If
;;
   Note from Betov: This 'WM_IME_CHAR' case and according Routine have been implemented
   by Liang Xianning. I am pretty sure that holding unicode oriental KeyBoards input
   can *not* be done this way for many reasons (what if one byte inside the unicode
   string is equal to Quote, Double-Quote,... and other critical Bytes values? What of
   BackSpace upon such a String, and so on. Also, all of the innumerous RosAsm Routines
   that perform any action on Source Text should be re-written too... What i would
   clearly refuse 'for many reasons...' too).

   The only solution i can imagine for inputing Oriental Unicode Strings would be to
   write an external little tool (a simple EditControl + a simple Routine to save the
   String Data in U$ RosAsm Format -in hexaDecimal Numbers- to be pasted in the Source
   Data through the ClipBoard. Very easy to do, but i can't do it. (I can't see any
   Oriental Char on my PC...).

   Another thing is that fully holding Oriental Char *inside* an Asm Source Editor
   does not make much sense, as the source would then be turned of no use for all other
   programmers. Do i write in french??? Oh yes,... Sometimes, when i am ashamed... :))
;;
    ...Else_If eax = &WM_IME_CHAR
        .If D$FL.Redraw = &FALSE
            Call CloseDebuggerOrIgnore | Comp eax &IDNO <> @ExitFalse

            Mov D$FL.Redraw &TRUE
            Mov eax D@Wparam | Call InsertDoubleByte | Call AskForRedraw
            jmp @NotReadyToRun
        .End_If

    ...Else_If eax = &WM_MOUSEWHEEL
            Call WheelMsg

    ...Else_If eax = &WM_XBUTTONDOWN
        ..If W@Wparam+2 = &XBUTTON1
            Mov D$FL.BlockInside &FALSE
            Call RestoreRealSource | Call BackClick | Call SetPartialEditionFromPos

        ..Else_If W@Wparam+2 = &XBUTTON2
            Mov D$FL.BlockInside &FALSE
            Call RestoreRealSource | Call ForwardClick | Call SetPartialEditionFromPos
        ..End_If

        Mov eax &TRUE

EndP

    ...Else_If eax = &WM_COMMAND
        Mov eax D@Wparam | and eax 0FFFF

        ..If eax = M00_<_
            Mov D$FL.BlockInside &FALSE
            Call RestoreRealSource | Call BackClick | Call SetPartialEditionFromPos

        ..Else_If eax = M00_>
            Mov D$FL.BlockInside &FALSE
            Call RestoreRealSource | Call ForwardClick | Call SetPartialEditionFromPos

        ..Else_If eax = M00_Compile
            Mov D$FL.ShowStats &TRUE
            Call Compile

        ..Else_If eax = M00_Run
            Call Run

        ..Else_If eax = M00_Optimize_Jumps_Sizes
            Call Optimize

       ; ..Else_If eax = M00_Profile | Call Profiler

        ..Else_If eax = M00_Paste_at_Pos
         ;   On D$IsDebugging = &TRUE, jmp @ExitFalse
            Call RestoreRealSource | Call IncludeSource | Call SetPartialEditionFromPos
            Mov D$FL.SourceHasChanged &TRUE, D$FL.SourceHasChanged &FALSE
            Call SetCaret D$STRUCT.EditData@CurrentWritingPos

        ..Else_If eax = M00_Find
            Mov D$NextSearchPos 0 | Call SetSimpleSearchBox

        ..Else_If eax = M00_Replace
            Mov D$NextSearchPos 0 | On D$FL.IsDebugging = &FALSE, Call SetFindReplaceBox

        ..Else_If eax = M00_Undo
            Call ControlZ

        ..Else_If eax = M00_Redo
            Call ControlShiftZ

        ..Else_If eax = M00_Copy
            Call ControlC

        ..Else_If eax = M00_Paste
            Call ControlV | Call AskForRedraw

        ..Else_If eax = M00_Delete
            Call ControlD | Call AskForRedraw

        ..Else_If eax = M00_Cut
            Call ControlX | Call AskForRedraw

        ..Else_If eax = M00_Save_Source_only
            Call ControlS

        ..Else_If eax = M00_Replace_Source_Only
            On D$FL.IsDebugging = &TRUE, jmp @ExitFalse
            Call ReInitUndoOnly
            Call ReplaceSourceOnly
            Call UpdateTitlesFromIncludeFiles
            On D$SourceLen > 0, Call SetPartialEditionFromPos

        ..Else_If eax = M00_Tree
            Call CreateTreeViewList | Call SetTreeDialogPos

        ..Else_If eax = M00_Import
           Call ShowSourceImports

        ..Else_If eax = M00_Export
           Call ShowSourceExports

        ..Else_If eax = M00_Print | Call Print

        ..Else_If eax = Float_Copy
            Call CopyFromFloatMenu | Call AskForRedraw
            Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint

        ..Else_If eax = Float_Delete
            Call ControlX | Call AskForRedraw
            Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint

        ..Else_If eax = Float_Replace
            Call ControlD | Call ControlV | Call AskForRedraw
            Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint

        ..Else_If eax = Float_SearchUp
            Call RestoreRealSource
            Call StorePosInBackTable | Call SearchUpFromFloatMenu
            Call SetPartialEditionFromPos
            Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint

        ..Else_If eax = Float_SearchDown
            Call RestoreRealSource
            Call StorePosInBackTable | Call SearchDownFromFloatMenu
            Call SetPartialEditionFromPos
            Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint

        ..Else_If eax = Float_SearchFromTop
            Call RestoreRealSource
            Call StorePosInBackTable | Call SearchFromTopFromFloatMenu
            Call SetPartialEditionFromPos
            Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint

        ..Else_If eax = Float_Unfold
            Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint
            Call RestoreRealSource | Call ShowUnfoldMacro | Call SetPartialEditionFromPos

        ..Else_If eax = Float_BookMark | Call StoreBookMark
            Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint

        ..Else_If eax = Float_UnBookMark | Call DeleteBookMark
            Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint

        ..Else_If eax = Float_Number | Call ViewClickedNumber
            Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint

        ..Else_If eax = Float_SelReplace
            Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint | Call BlockReplaceAll

        ..Else_If eax = MenuFloatSetBreakPoint
            Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint
            Call SetBreakPoint | Call DoStoreBreakPoint | jmp @NotReadyToRun

        ..Else_If eax = MenuFloatDelBreakPoint
            Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint
            Call DeleteBreakPoint | Call DoStoreRemoveBreakPoint | jmp @NotReadyToRun

        ..Else_If eax = MenuFloatDelAllBreakPoint
            Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint
            Call DeleteAllBreakpoints | jmp @NotReadyToRun

        ..Else_If eax = Float_BadDisLabel
            Call 'USER32.DestroyMenu' D$H.MenuFloatBreakPoint
            Call ForcedFlags

        ..Else_If eax = M00_Output
            Call OutputFormat | Call EnableMenutems | jmp @NotReadyToRun

        ..End_If

    ...Else_If eax = &WM_MOUSEMOVE

        .If D@Wparam <> &MK_LBUTTON

            Call 'USER32.ClipCursor' &NULL ; !!! TODO à revoir pour éviter de le faire à chaque déplacement de curseur !!!

        .Else

            On B$UserHaveClickDown = &FALSE, jmp @NoClient

            On B$UserClickAfterEnd = &TRUE, jmp @NoClient

            Push D@Lparam | Pop W$MousePosX, W$MousePosY

            Call SetBlock

            If B$FirstBlockDraw = &FALSE

                On B$BlockRedraw = &TRUE, Call AskForRedrawNow

            End_If

        .End_If

    ...Else_If eax = &WM_LBUTTONDOWN

        Mov B$ShiftBlockInside &FALSE

        Call KillCompletionList

        On D$FL.BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
        Push D@Lparam | Pop W$MousePosX, W$MousePosY

        Mov D$FL.BlockInside &FALSE, eax D@hwnd | Call LeftButton

        Call AskForRedrawNow
        Mov D$NextSearchPos 0, B$UserHaveClickDown &TRUE

    ...Else_If eax = &WM_LBUTTONUP

        On D$FL.BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
        On B$FirstBlockDraw = &TRUE, Mov D$FL.BlockInside &FALSE
        Call LeftButtonUp | Mov B$UserHaveClickDown &FALSE | Call AskForRedrawNow

    ...Else_If eax = &WM_RBUTTONUP
        Call KillCompletionList
        On D$FL.BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor

        Mov eax D@hwnd
        .If eax = D$H.BreakPointWindow
            If D$DBPMenuOn = RIGHT_CLICK_ACTION
                Mov B$ShiftBlockInside &FALSE

                Call KillCompletionList

                On D$FL.BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
                Call RestoreRealSource
                    Push D@Lparam | Pop W$MousePosX, W$MousePosY
                    Call MarginRightClick
                Call SetPartialEditionFromPos
            End_If

            jmp @NoClient
        .End_If

        Call RestoreRealSource
            Push D@Lparam | Pop W$MousePosX, W$MousePosY
            Call RightClick
        Call SetPartialEditionFromPos

    ...Else_If eax = &WM_LBUTTONDBLCLK
        Mov B$ShiftBlockInside &FALSE

        Call KillCompletionList

        On D$FL.BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
        Call RestoreRealSource
            Push D@Lparam | Pop W$MousePosX, W$MousePosY
            Call DoubleClick
        Call SetPartialEditionFromPos

    ...Else_If eax = D$FindStringMessage
        Call KillCompletionList

        Call StringSearch | Call AskForRedraw

    ...Else
        jmp @NoClient

    ...End_If

@ExitFalse:

      Mov eax &FALSE | jmp P9>>

@NotReadyToRun:

    If D$FL.ReadyToRun = &TRUE         ; (Something has just been modified).
        Mov D$FL.ReadyToRun &FALSE

        Call VirtualFree IpTable

        Call VirtualFree StatementsTable

        Call VirtualFree StatementsTable2

    End_If

    Mov D$FL.SourceHasChanged &TRUE

    Mov eax &FALSE | jmp P9>

@NoClient:

    Call 'USER32.DefWindowProcA' D@hwnd,
                                 D@msg,
                                 D@wParam,
                                 D@lParam

EndP
____________________________________________________________________________________________

WM_CLOSE_MainWindow:

        Call CloseDebuggerOrIgnore | Comp eax &IDNO = S1>

ret

S1:     On D$FL.DialogEdition = &TRUE Call CloseDialogEdition

        Call Security | On eax = &IDCANCEL ret

        If D$FL.SaveMainPos = &TRUE

            Call 'USER32.ShowWindow' D$H.MainWindow,
                                     &SW_RESTORE

            Call 'USER32.GetWindowRect' D$H.MainWindow,
                                        STRUC.RECT.MainWindow

            Mov eax D$WindowX | sub D$WindowW eax

            Mov eax D$WindowY | sub D$WindowH eax

            Call UpdateRegistry

        End_If

        On D$ToolBarChange = &TRUE Call SaveToolBar

        Call LeftButtonUp                            ; ensure no confined mouse

        Call CloseHelp

        On D$H.EditedDialog <> 0 Call 'USER32.DestroyWindow' D$H.EditedDialog

        On D$H.DialogEditor <> 0 Call 'USER32.EndDialog' D$H.DialogEditor

        Call 'USER32.DestroyWindow' D$H.MainWindow

ret
____________________________________________________________________________________________

Compile:

    If D$H.UnusedCodeAndDataDialog = &FALSE

        test D$FL.Compiling &TRUE NOT_ZERO S0>>

        test D$FL.IsDebugging &TRUE NOT_ZERO S0>

    Else

        Mov D$FL.ShowStats &FALSE

    End_If

    Mov B$RecompileWanted &FALSE, D$FL.Compiling &TRUE

    Call RestoreRealSource

    On D$H.UnusedCodeAndDataDialog <> 0 Call ReInitUnusedDialog

    Call AsmMain | Mov D$OldStackPointer &NULL

    If D$FL.CompileErrorHappend = &FALSE

        Mov D$FL.ReadyToRun &TRUE, D$FL.SourceHasChanged &FALSE

    End_If

    Call SetPartialEditionFromPos

    Call AskForRedraw

    Mov D$FL.Compiling &FALSE | Call ResetKeys

S0:
ret


Run:
    If D$H.UnusedCodeAndDataDialog <> &FALSE
      ; 'UnusedCodeAndDataDialogCallBack'
        Call 'USER32.EndDialog' D$H.UnusedCodeAndDataDialog, 0
        Mov D$H.UnusedCodeAndDataDialog 0
        Mov D$FL.Compiling &FALSE, D$FL.UnusedSymbolsDialogWanted &FALSE, D$FL.ShowStats &FALSE
        Mov D$FL.UnusedSymbolsDialogWanted &FALSE
    End_If

    On D$FL.Compiling = &TRUE, ret
    On D$FL.IsDebugging = &TRUE, ret

    Mov D$FL.ShowStats &FALSE, D$FL.Compiling &TRUE, B$RecompileWanted &FALSE

    Call RestoreRealSource

    If D$FL.ReadyToRun = &FALSE
        Mov D$FL.ShowStats &FALSE
        Call AsmMain
        Mov D$OldStackPointer &NULL
        Mov D$FL.UnusedSymbolsDialogWanted &FALSE

        On D$FL.CompileErrorHappend = &FALSE Mov D$FL.ReadyToRun &TRUE

    End_If

    Call SetPartialEditionFromPos

    If D$FL.CompileErrorHappend = &FALSE

        Mov D$FL.SourceHasChanged &FALSE

        Call Debugger

    End_If

    Mov D$FL.Compiling &FALSE | Call ResetKeys
ret


Optimize:
    Mov B$AlignFound &FALSE

    If D$H.UnusedCodeAndDataDialog <> &FALSE
      ; 'UnusedCodeAndDataDialogCallBack'
        Call 'USER32.EndDialog' D$H.UnusedCodeAndDataDialog, 0
        Mov D$H.UnusedCodeAndDataDialog 0
        Mov D$FL.Compiling &FALSE, D$FL.UnusedSymbolsDialogWanted &FALSE, D$FL.ShowStats &FALSE
    End_If

    If D$FL.ReadyToRun = &FALSE
        Mov D$FL.ShowStats &FALSE
        Call Compile
    End_If

    .If D$FL.ReadyToRun = &TRUE
        If B$AlignFound = &TRUE
            Call 'USER32.MessageBoxA' &NULL, NoptimizeMessage,
                                      NoptimizeTitle, &MB_SYSTEMMODAL
        Else
            Mov B$ShortenJumpsWanted &TRUE
            Mov D$FL.ShowStats &TRUE
            Call Compile
            Mov B$ShortenJumpsWanted &FALSE
        End_If
    .End_If
ret

[NoptimizeTitle: B$ 'Optimizer' EOS
 NoptimizeMessage: B$ "The Assembler can not yet optimize the Jump Sizes  
 on a Source making use of the Align Statement:
 
 The Alignments would be broken." EOS]
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc ScrollBarProc:

    Arguments @hwnd,
              @msg,
              @wParam,
              @lParam

    On D$FL.SourceReady = &FALSE, jmp S1>>

    ...If D@msg = &WM_VSCROLL

        Call KillCompletionList

        Call 'USER32.IsMenu' D$H.MenuFloatBreakPoint

        ..If eax = &FALSE

            .If W@wParam = &SB_THUMBTRACK

                If D$TotalNumberOfLines < 0FFFF

                    movzx edx W@wParam+2

                Else

                    Call 'USER32.GetScrollInfo' D$H.ScrollBarWindow,
                                                &SB_VERT,
                                                VScroll

                    Mov edx D$VScroll.nTrackPos

                End_If

                Call RePosFromScroll

            .Else_If W@wParam = &SB_LINEDOWN

                Call DownOneLine

            .Else_If W@wParam = &SB_LINEUP

                Call UpOneLine

            .Else_If W@wParam = &SB_PAGEDOWN

                Call DownOneLine

            .Else_If W@wParam = &SB_PAGEUP

                Call UpOneLine

            .End_If

            Call AskForRedraw

        ..End_If

        Mov eax &FALSE

     ...Else

S1:     Call 'USER32.DefWindowProcA' D@hwnd,
                                     D@msg,
                                     D@wParam,
                                     D@lParam

     ...End_If

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[QuitTitle: B$ 'Sure?' EOS]
[QuitMessage: B$ 'Source has changed. Save/Compile now?' EOS]

Security:

    Mov eax &IDNO

    On D$CodeSource = 0 ret

    ...If D$FL.SecurityWanted = &TRUE

        ..If D$FL.SourceHasChanged = &TRUE

            Call 'MessageBoxA' D$H.MainWindow,
                               QuitMessage,
                               QuitTitle,
                               &MB_YESNOCANCEL

            .If eax = &IDYES

                Call RestoreRealSource

                Call AsmMain | Mov D$OldStackPointer &NULL

                If D$FL.CompileErrorHappend = &FALSE

                    Mov D$FL.ReadyToRun &TRUE,
                        D$FL.SourceHasChanged &FALSE,
                        eax &IDNO

                Else

                    Call SetPartialEditionFromPos | Mov eax &IDCANCEL

                End_If

            .End_If

        ..End_If

    ...End_If

ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[OldBlockInside: D$ ?
 DollarOnly: D$ ?]

Proc CharMessage:
    Argument @wParam

    Mov eax D@wParam | On eax = &VK_ESCAPE EndP

    On B$keys+&VK_CONTROL = &TRUE, jmp L2>>

    .If D$H.DebugDialog <> 0
        Push eax
        Call CloseDebuggerOrIgnore
            If eax = &IDNO
                Mov eax &FALSE

EndP

            End_If
        Pop eax
    .End_If

    Move D$OldBlockInside D$FL.BlockInside | Mov D$FL.BlockInside &FALSE

    If B$WriteCheckerWanted = &TRUE
        cmp eax 32 | je L1>
        cmp eax CR | je L1>
        cmp eax ',' | je L1>
        cmp eax 8 | je L1>
       ; cmp eax ':' | je L1>
        cmp eax TAB | jne L0>
L1:     Call WriteChecker D$STRUCT.EditData@CurrentWritingPos, eax
    End_If

L0: cmp eax CR | jne L1>
        If D$H.CompletionList <> 0
            Call ToCompletionList CR

EndP

        End_If
        Call CarriageReturn | jmp P8>>
L1: cmp eax 8 | jne L1>
        Call BackSpace | jmp P8>>
L1: cmp al 167 | jne L1>   ; 167 = Paragraph Char (no more available).
        On B$DollarOnly = &TRUE, Mov al '$'
L1: Mov B$SimulatedBlock &FALSE
    cmp B$keys+&VK_SHIFT &TRUE | jne L1>
        On B$OldBlockInside = &FALSE, Call SimulateBlockForBackIndent
L1: cmp B$OldBlockInside &TRUE | jne L1>
        cmp al TAB | jne L1>
            Call IsItBlockIndent
            .If B$SimulatedBlock = &TRUE
                Mov D$FL.BlockInside &FALSE
                Mov esi D$STRUCT.EditData@CurrentWritingPos
                If B$esi-1 = LF
                    Call StartOfLine
                Else
                    Call StartOfLine | Call AskForRedrawNow | Call StartOfLine
                End_If
            .End_If
            On B$BlockIndent = &TRUE, jmp P8>>
L1: ;Mov D$OldBlockInside &FALSE
    cmp D$STRUCT.EditData@Overwrite &TRUE | jne L1>
        Call OverwriteSource | jmp P8>>
L1:     Call InsertSource | jmp P8>>

L2: Call GetCtrlKeyState

    .If eax = 03
        Call ControlC | Mov eax &FALSE
    .Else_If eax = 016
        ;Mov D$FL.BlockInside &FALSE
        Call ControlV | Mov eax &TRUE
        Mov D$FL.BlockInside &FALSE
    .Else_If eax = 018
        Call ControlX | Mov eax &TRUE
    .Else_If eax = 04
        Call ControlD | Mov eax &TRUE
    .Else_If eax = 01A
        If B$Keys+&VK_SHIFT = &TRUE
            Call ControlShiftZ | Mov eax &FALSE ; ??? What use is this, here ???
        Else
            Call ControlZ | Mov eax &TRUE  ; ???
        End_If
    .Else_If eax = 1
        Call ControlA | Mov eax &FALSE
    .Else_If eax = 010
        Call ControlP | Mov eax &FALSE
    .Else_If eax = 012
        Mov D$NextSearchPos 0 | Call SetFindReplaceBox
        Mov B$keys+&VK_CONTROL &FALSE, eax &TRUE
    .Else_If eax = 013
        Call ControlS | Mov eax &FALSE
        Mov B$keys+&VK_CONTROL &FALSE
    .Else_If eax = 0B
        Call ControlK | Mov eax &FALSE
        Mov B$keys+&VK_CONTROL &FALSE
    .Else_If eax = 06
        Mov D$NextSearchPos 0 | Call SetSimpleSearchBox
        Mov B$keys+&VK_CONTROL &FALSE
        Mov eax &FALSE
        Mov B$keys+&VK_CONTROL &FALSE
    .Else_If eax = 019
        If B$CtrlYFlag = &TRUE
            Call ControlY | Mov eax &TRUE
        End_If
    .Else_If eax = SPC
        On B$Underline = &TRUE, Call Completion
        Mov eax &TRUE
        Mov B$keys+&VK_CONTROL &FALSE
    .Else
        Mov eax &FALSE
    .End_If

    Push eax | Call CheckCtrlKeyState | Pop eax | Mov D$OldBlockInside &FALSE | ExitP

P8: Mov D$OldBlockInside &FALSE, eax &TRUE
EndP
____________________________________________________________________________________________

[CtrlKeyState: D$ ?]

; The "and eax 0FFFF" makes it work as well under 95 and 2000

GetCtrlKeyState:
    Push eax
        Call 'USER32.GetKeyState' &VK_CONTROL
        and eax 0FFFF | Mov D$CtrlKeyState eax
    Pop eax
ret


CheckCtrlKeyState:
    Call 'USER32.GetKeyState' &VK_CONTROL | and eax 0_FFFF
    On eax <> D$CtrlKeyState, Mov B$keys+&VK_CONTROL &FALSE
ret
____________________________________________________________________________________________

[SimulatedBlock: D$ ?]

SimulateBlockForBackIndent:
    Push esi
        Mov esi D$STRUCT.EditData@CurrentWritingPos
        While B$esi-1 <> LF | dec esi | End_While
        Mov D$LP.BlockStartText esi

        Mov esi D$STRUCT.EditData@CurrentWritingPos
        While B$esi <> CR | inc esi | End_While | dec esi

        Mov D$LP.BlockEndText esi

        Mov B$OldBlockInside &TRUE, B$SimulatedBlock &TRUE
    Pop esi
ret


[BlockIndent: D$ ?]

IsItBlockIndent:
    Mov B$BlockIndent &FALSE

  ; Verify that the Block includes a start of Line (accept non included Labels):
    Mov esi D$LP.BlockStartText
    While B$esi <> LF
        dec esi
        If B$esi = ':'
            jmp L2>
        Else_If B$esi > SPC
            jmp L9>>
        End_If
    End_While

  ; Verify that the Block is not empty:
L2: Mov esi D$LP.BlockStartText

    While esi < D$LP.BlockEndText

        On B$esi > SPC jmp L2>

        add esi (1*ASCII)

    End_While

    jmp L9>>

  ; Verify that the last selected line is complete (does not stop before CRLF):
L2: Mov esi D$LP.BlockEndText

    While W$esi+1 <> CRLF
        On esi < D$LP.BlockStartText jmp L9>>
        dec esi
    EndWhile

  ; OK, Block holds a full Lines content.  ; 'InsertSource'
  ; Insert or retrieve as many TAB as Lines (but preserve Labels):
L2: On B$keys+&VK_SHIFT = &TRUE, jmp RetrieveBlockIndent

L2: Mov esi D$LP.BlockStartText, B$FirstBlockLine &TRUE

    ..While esi < D$LP.BlockEndText

      ; Go to Start of Line:
        Mov ebx esi | While B$ebx <> LF | dec ebx | End_While
      ; Go to Start of first member, and save in eax:
        While B$ebx <= SPC | inc ebx | End_While
        Mov eax ebx
        .While B$ebx > SPC
            inc ebx
            If B$ebx = ':'
                inc ebx
                While B$ebx <= SPC
                    inc ebx | On B$ebx = CR, jmp L8>>
                End_While
          ; New Start of first memeber if first one was a label:
            Mov eax ebx
            End_If
        .End_While
      ; eax > real first member to move. Adjust Block Start if necessary:
        If B$FirstBlockLine = &TRUE
            Mov D$LP.BlockStartText eax
            Mov B$FirstBlockLine &FALSE
        End_If
        Push eax, D$SourceLen
            Call SetCaret eax | dec D$STRUCT.EditData@CaretRow | Move D$STRUCT.EditData@PhysicalCaretRow D$STRUCT.EditData@CaretRow
            Mov al TAB | Call InsertSourceOnBlockIndent
        Pop ecx, esi
      ; Next Line:
        While B$esi <> CR | inc esi | End_While | inc esi

        Mov eax D$SourceLen | sub eax ecx | add D$LP.BlockEndText eax

    ..End_While

    Mov eax D$LP.BlockStartText
    While B$eax = SPC | inc eax | End_While
    Mov D$LP.BlockStartText eax

    Call SetCaret D$LP.BlockEndText | Mov D$STRUCT.EditData@RightScroll 0

  ; 'KeyMessage'
  ;  Move D$ShiftBlockCol D$CaretRow
  ;  Move D$ShiftBlockLine D$CaretLine
  ;  Move D$PhysicalCaretRow D$CaretRow
  ;  Mov D$CaretEndOfLine &TRUE
  ;  Call KeyMessage

L8: Mov B$BlockIndent &TRUE, D$FL.BlockInside &TRUE
L9: ret


[FirstBlockLine: D$ ?
 NewCaretBlockPos: D$ ?]

RetrieveBlockIndent:
    Mov B$BlockIndent &TRUE, D$NewCaretBlockPos 0
  ; Are there enough free Spaces to be deleted on each Line? If not, abort:
    Mov esi D$LP.BlockStartText

    ..While esi < D$LP.BlockEndText

      ; Go to Start of Line:
        Mov ebx esi | While B$ebx <> LF | dec ebx | End_While
      ; Go to Start of first member, and save in eax:
        While B$ebx <= SPC | inc ebx | End_While
        Mov eax ebx
      ; If Label, jmp over it:
        .While B$ebx > SPC
            inc ebx
            If B$ebx = ':'
                inc ebx
                While B$ebx <= SPC
                    inc ebx | On B$ebx = CR, jmp L8>>
                End_While
              ; New Start of first memeber if first one was a label:
                Mov eax ebx
            End_If
        .End_While

      ; eax > now real first member to move. Are there enough Spaces in front of it?
        Push eax
            Mov ecx D$TabIs
L0:         dec eax
            If B$eax <> SPC
                Pop eax | jmp L8>>
            End_If
            loop L0<
        Pop esi
      ; OK, enough spaces > Next Line:
        While B$esi <> CR | inc esi | End_While | inc esi
    ..End_While

  ; OK, enough Spaces on each Line > Delete:
    Mov esi D$LP.BlockStartText, B$FirstBlockLine &TRUE

    ..While esi < D$LP.BlockEndText

      ; Go to Start of Line:
        Mov ebx esi | While B$ebx <> LF | dec ebx | End_While
      ; Go to Start of first member, and save in eax:
        While B$ebx <= SPC | inc ebx | End_While
        Mov eax ebx
        .While B$ebx > SPC
            inc ebx
            If B$ebx = ':'
                inc ebx
                While B$ebx <= SPC
                    inc ebx | On B$ebx = CR, jmp L8>>
                End_While
              ; New Start of first member if first one was a label:
                Mov eax ebx
            End_If
        .End_While
      ; eax > real first member to Move:
        On D$NewCaretBlockPos = 0, Mov D$NewCaretBlockPos eax

        If B$FirstBlockLine = &TRUE
            Mov D$LP.BlockStartText eax, B$FirstBlockLine &FALSE
        End_If
        Push eax, D$SourceLen
            Call SetCaret eax | sub D$STRUCT.EditData@CaretRow ASCII | Move D$STRUCT.EditData@PhysicalCaretRow D$STRUCT.EditData@CaretRow
            Mov ecx D$TabIs
L0:         Push ecx

                Mov al TAB | Call BackSpace | sub D$LP.BlockEndText (1*ASCII)

            Pop ecx | loop L0<
        Pop ecx, esi
      ; Next Line:
        sub esi D$TabIs
        While B$esi <> CR | inc esi | End_While | inc esi
    ..End_While

    Mov eax D$TabIs | sub D$LP.BlockStartText eax
    Mov eax D$NewCaretBlockPos | sub eax D$TabIs | dec eax | Call SetCaret eax

   ; Move D$ShiftBlockCol D$CaretRow
   ; Move D$ShiftBlockLine D$CaretLine
   ; Move D$PhysicalCaretRow D$CaretRow
   ; Mov D$CaretEndOfLine &FALSE

L8: Mov B$BlockIndent &TRUE, D$FL.BlockInside &TRUE
L9: ret



[ShiftDown: D$ ?
 ShiftBlockCol: D$ ?
 ShiftBlockLine: D$ ?]

[keys: B$ ? # 0100]

[KeyHasModifedSource: D$ ?
 KeyHasMovedCaret: D$ ?
 FL.SourceHasChanged: D$ ?]

KeyMessage:
    On D$FL.SourceReady = &FALSE, ret

    Mov B$KeyHasModifedSource &FALSE, B$KeyHasMovedCaret &FALSE

    ..If D$FL.BlockInside = &TRUE
        .If B$ShiftBlockInside = &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                Move D$STRUCT.EditData@CaretRow D$ShiftBlockCol
                Move D$STRUCT.EditData@CaretLine D$ShiftBlockLine
                Move D$STRUCT.EditData@PhysicalCaretRow D$STRUCT.EditData@CaretRow
                Mov D$STRUCT.EditData@CaretEndOfLine &FALSE
            End_If
        .Else
            Mov D$ShiftDown 0
        .End_If
    ..End_If

  ; Under some configurations, it appears that [Atl-Gr] (Right [Alt]), may generate
  ; a Key Down Message not followed by a Key Up message, for the Control Key... So:
    ...If B$keys+&VK_MENU = &TRUE
        Mov B$keys+&VK_MENU &FALSE, B$keys+&VK_CONTROL &FALSE

    ...Else_If B$keys+&VK_CONTROL = &TRUE
        .If B$keys+&VK_PGDN = &TRUE
            Call FullDown | Mov D$FL.BlockInside &FALSE
            Mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                Call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, Mov D$FL.BlockInside &FALSE, B$ShiftBlockInside &FALSE

        .Else_If B$keys+&VK_PGUP = &TRUE
            Call FullUp | Mov D$FL.BlockInside &FALSE
            Mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                Call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, Mov D$FL.BlockInside &FALSE, B$ShiftBlockInside &FALSE

        .Else_If B$keys+&VK_LEFT = &TRUE
            Call StartOfWord | Mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                Call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, Mov D$FL.BlockInside &FALSE, B$ShiftBlockInside &FALSE
            Mov B$keys+&VK_LEFT &FALSE

        .Else_If B$keys+&VK_RIGHT = &TRUE
            Call EndOfWord | Mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                Call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, Mov D$FL.BlockInside &FALSE, B$ShiftBlockInside &FALSE
            Mov B$keys+&VK_RIGHT &FALSE

        .Else_If B$keys+&VK_DOWN = &TRUE
            Call DownOneLine | Mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                Call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, Mov D$FL.BlockInside &FALSE, B$ShiftBlockInside &FALSE
            Mov B$keys+&VK_DOWN &FALSE

        .Else_If B$keys+&VK_UP = &TRUE
            Call UpOneLine | Mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                Call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, Mov D$FL.BlockInside &FALSE, B$ShiftBlockInside &FALSE
            Mov B$keys+&VK_UP &FALSE

        .Else_If B$keys+&VK_DELETE = &TRUE
            On D$H.DebugDialog <> 0, ret
            Call ControlD | Mov B$keys+&VK_DELETE &FALSE, B$KeyHasModifedSource &TRUE

        .Else_If B$keys+&VK_INSERT = &TRUE
            On D$H.DebugDialog <> 0, ret
            Call ControlC | Mov B$keys+&VK_INSERT &FALSE, B$KeyHasModifedSource &TRUE

        .Else_If B$keys+&VK_BACK = &TRUE
            On D$H.DebugDialog <> 0, ret
            Call ControlX | Mov B$keys+&VK_BACK &FALSE, B$KeyHasModifedSource &TRUE
            Mov D$FL.Redraw &TRUE
          ; This 'FL.Redraw' is to kill the next coming WM_CHAR holding
          ; ([Ctrl][Back] sends a char).

        .Else_If B$keys+&VK_F4 = &TRUE
            Call RestoreRealSource
                Call SetCaret D$STRUCT.EditData@CurrentWritingPos
                Mov eax 0, ebx D$STRUCT.EditData@CaretLine | Call MarginAction
            Call SetPartialEditionFromPos
            Mov B$keys+&VK_F4 0

        .Else
            On B$keys+&VK_SHIFT = &TRUE, jmp L1>

        .End_If



    ...Else_If B$keys+&VK_SHIFT = &TRUE
L1:     .If B$keys+&VK_LEFT = &TRUE
            Call SetPhysicalCaretRow
            On B$keys+&VK_CONTROL = &FALSE, Call KeyLeft
            If D$ShiftDown <> 0
                Call SetShiftBlock
            Else
                Mov D$FL.BlockInside &FALSE
            End_If
            Mov B$KeyHasMovedCaret &TRUE
            Move D$STRUCT.EditData@PhysicalCaretRow D$STRUCT.EditData@CaretRow

        .Else_If B$keys+&VK_RIGHT = &TRUE
            On B$keys+&VK_CONTROL = &FALSE, Call KeyRight
            If D$ShiftDown <> 0
                Call SetShiftBlock
            Else
                Mov D$FL.BlockInside &FALSE
            End_If
            Mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_UP = &TRUE
            On B$keys+&VK_CONTROL = &FALSE, Call KeyUp
            If D$ShiftDown <> 0
                Call SetShiftBlock
            Else
                Mov D$FL.BlockInside &FALSE
            End_If
            Mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_DOWN = &TRUE
            On B$keys+&VK_CONTROL = &FALSE, Call KeyDown
            If D$ShiftDown <> 0
                Call SetShiftBlock
            Else
                Mov D$FL.BlockInside &FALSE
            End_If
            Mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_PGUP = &TRUE
            On B$keys+&VK_CONTROL = &FALSE, Call OnlyOnePageUp
            If D$ShiftDown <> 0
                Call SetShiftBlock
            Else
                Mov D$FL.BlockInside &FALSE
            End_If
            Mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_PGDN = &TRUE
            On B$keys+&VK_CONTROL = &FALSE, Call OnlyOnePageDown
            If D$ShiftDown <> 0
                Call SetShiftBlock
            Else
                Mov D$FL.BlockInside &FALSE
            End_If
            Mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_HOME = &TRUE
            Call StartOfLine
            If D$ShiftDown <> 0
                Call SetShiftBlock
            Else
                Mov D$FL.BlockInside &FALSE
            End_If
            Mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_END = &TRUE
            Call EndOfLine
            If D$ShiftDown <> 0
                Call SetShiftBlock
            Else
                Mov D$FL.BlockInside &FALSE
            End_If
            Mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_INSERT = &TRUE
            On D$H.DebugDialog <> 0, ret
            Call ControlV | Mov B$keys+&VK_INSERT &FALSE, B$KeyHasModifedSource &TRUE

        .Else_If B$keys+&VK_DELETE = &TRUE
            On D$H.DebugDialog <> 0, ret
            Call ControlX | Mov B$keys+&VK_DELETE &FALSE, B$KeyHasModifedSource &TRUE

        .Else_If B$keys+&VK_F4 = &TRUE
            Call RestoreRealSource
                Call SetCaret D$STRUCT.EditData@CurrentWritingPos
                Mov eax 0, ebx D$STRUCT.EditData@CaretLine | Call MarginAction
            Call SetPartialEditionFromPos
            Mov B$keys+&VK_F4 0

        .Else
            If D$FL.BlockInside = &FALSE
                Move D$ShiftDown D$STRUCT.EditData@CurrentWritingPos | ret
           ; Else
           ;     Mov D$FL.BlockInside &FALSE | Call AskForRedraw | ret
            End_If

        .End_If

    ...Else
        ..If eax = &VK_PGDN
            Call OnlyOnePageDown | Mov D$FL.BlockInside &FALSE

        ..Else_If eax = &VK_PGUP
            Call OnlyOnePageUp | Mov D$FL.BlockInside &FALSE

        ..Else_If eax = &VK_DOWN
            If D$H.CompletionList <> 0
                Mov B$Keys+eax 0
                Call ToCompletionList &VK_DOWN
              ; To save from downward 'KeyHasMovedCaret' Flag modification:
                ret
            Else
                Call KeyDown | Mov D$FL.BlockInside &FALSE
            End_If

        ..Else_If eax = &VK_UP
            If D$H.CompletionList <> 0
                Mov B$Keys+eax 0
                Call ToCompletionList &VK_UP
              ; To save from downward 'KeyHasMovedCaret' Flag modification:
                ret
            Else
                Call KeyUp | Mov D$FL.BlockInside &FALSE
            End_If

        ..Else_If eax = &VK_LEFT
            Call KeyLeft | Mov D$FL.BlockInside &FALSE

        ..Else_If eax = &VK_RIGHT
            Call KeyRight | Mov D$FL.BlockInside &FALSE

        ..Else_If eax = &VK_INSERT
            Call KeyInsert | Mov D$FL.BlockInside &FALSE

        ..Else_If eax = &VK_DELETE
            .If D$H.DebugDialog <> 0
                Mov B$Keys+eax 0
                Call CloseDebuggerOrIgnore | On eax = &IDNO ret
            .End_If
            Call KeyDelete | Mov D$FL.BlockInside &FALSE, B$KeyHasModifedSource &TRUE

        ..Else_If eax = &VK_END
            Call EndOfLine | Mov D$FL.BlockInside &FALSE

        ..Else_If eax = &VK_HOME
            Call StartOfLine | Mov D$FL.BlockInside &FALSE

        ..Else_If eax = &VK_ESCAPE
            If D$H.CompletionList <> 0
                Call 'USER32.SendMessageA' D$H.CompletionList, &WM_COMMAND, &IDCANCEL, 0
                Mov B$keys+&VK_ESCAPE &FALSE | ret
            End_If

        ..Else_If eax = &VK_F1
            Call BUAsmHelp | Mov B$keys+&VK_F1 &FALSE

        ..Else_If eax = &VK_F2
            Call F2Help | Mov B$keys+&VK_F2 &FALSE

        ..Else_If eax = &VK_F3
            Call RestoreRealSource | Call StringSearch | Call SetPartialEditionFromPos
            Mov B$keys+&VK_F3 0

        ..Else_If eax = &VK_F4  ; BpMenu / SetBreakPoint / DeleteBreakpoint
            Call RestoreRealSource
                Call SetCaret D$STRUCT.EditData@CurrentWritingPos
                Mov eax 0, ebx D$STRUCT.EditData@CaretLine | Call MarginAction
            Call SetPartialEditionFromPos
            Mov B$keys+&VK_F4 0

        ..Else_If eax = &VK_F5
            Mov B$keys+&VK_F5 &FALSE
            Mov D$FL.ShowStats &TRUE
            Call Compile

        ..Else_If eax = &VK_F6
            Mov B$keys+&VK_F6 &FALSE

            If D$H.DebugDialog <> 0
                ret
            Else
                Call Run
            End_If

        ..Else_If eax = &VK_F8
            If D$H.DebugDialog <> 0
                Mov B$keys+&VK_F2 &FALSE
                Call CloseDebuggerOrIgnore | On eax = &IDNO ret
            End_If
            Call DrawOneLine

        ..Else_If eax = &VK_F9
            Mov B$Keys+eax 0
            ;Mov eax D$BreakPointsTables | int3
            ret
;;
; Problem if someone want to implement a Key doinf the same job as Right-Click:
; How to get back after Editor moves?

            Call KillCompletionList
  
            Call RowToX D$CaretRow | Mov D$MousePosY eax
            Call LineToY D$CaretLine | Mov D$MousePosY eax
            Call 'USER32.SetCursorPos' D$MousePosX, D$MousePosY

            Call RestoreRealSource
                Call RightClick
            Call SetPartialEditionFromPos
            Mov B$keys+&VK_F9 &FALSE
;;
        ..Else_If eax = &VK_F11
            Call SavePosOnF11

        ..Else_If eax = &VK_F12
            Call SetPosOnF12

        ..Else
            Mov B$Keys+eax 0 | ret

        ..End_If
      ; Necessity for clearing by hand because, with lengthy case (like upper 'StringSearch'),
      ; the OS clear the remaining Messages (here WM_KEYUP!!!) for the Message Flow.
       ; Pop eax | Mov B$Keys+eax 0
        Mov B$KeyHasMovedCaret &TRUE

    ...End_If


    On B$KeyHasMovedCaret = &TRUE, Call KillCompletionList
ret
____________________________________________________________________________________________

ResetKeys:
    Mov edi keys, eax 0, ecx (0100/4) | rep stosd
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

EnableMenutems:
   ; Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Profile, &MF_GRAYED

    .If D$FL.SourceReady = &FALSE
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Tree, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Paste_at_Pos, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Change_Compile_Name, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Replace_Source_Only, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_Source_Only, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Output, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Print, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Find, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Replace, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Replace, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Undo, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Redo, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Copy, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Paste, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Cut, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Compile, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Run, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Import, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Export, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_GUIDs, &MF_GRAYED

    .Else
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Tree, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Paste_at_Pos, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Change_Compile_Name, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Replace_Source_Only, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_Source_Only, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Output, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Print, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Find, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Replace, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Replace, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Undo, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Redo, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Copy, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Paste, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Cut, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Compile, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Run, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Import, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Export, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_GUIDs, &MF_ENABLED

    .End_If

    If D$SavingExtension = '.SYS'
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Run, &MF_GRAYED
    Else_If D$FL.SourceReady = &TRUE
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Run, &MF_ENABLED
    End_If

    If D$IconList = 0
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete_Icon, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Icon_IDs, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_Icon, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete_Icon, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Icon_IDs, &MF_GRAYED ; &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_Icon, &MF_GRAYED ;&MF_ENABLED
    End_If

    If D$BitMapList = 0
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete_BitMap, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_BitMaps_IDs, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_BitMap, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete_BitMap, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_BitMaps_IDs, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_BitMap, &MF_GRAYED ;&MF_ENABLED
    End_If

    If D$CursorList = 0
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete_Cursor, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Cursors_IDs, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_Cursor, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete_Cursor, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Cursors_IDs, &MF_GRAYED ;&MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_Cursor, &MF_GRAYED ;&MF_ENABLED
    End_If

    If D$WaveList = 0
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete_Wave, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Waves_IDs, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_Wave, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete_Wave, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Waves_IDs, &MF_GRAYED ;&MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_Wave, &MF_GRAYED ;&MF_ENABLED
    End_If

    If D$AviList = 0
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete_Avi, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Avi_IDs, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_Avi, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete_Avi, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Avi_IDs, &MF_GRAYED ;&MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_Avi, &MF_GRAYED ;&MF_ENABLED
    End_If

    If D$RCDataList = 0
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete_RC, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_RCs_IDs, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_RC, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete_RC, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_RCs_IDs, &MF_GRAYED ;&MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_RC, &MF_ENABLED
    End_If

    If D$DialogList = 0
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Load_from_Resources, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete_Resources_Dialog, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_to_Binary_File, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Replace_from_Binary_File, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Load_from_Resources, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete_Resources_Dialog, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_to_Binary_File, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Replace_from_Binary_File, &MF_ENABLED
    End_If

    If D$FL.SourceReady = &FALSE
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Load_from_Binary_File, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Load_from_Binary_File, &MF_ENABLED
    End_If

    If D$MenuList = 0
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Existing_Menu, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete_a_Menu, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_to_Binary_Menu_File, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Load_Binary_Menu_File, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Replace_from_Binary_Menu_File, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Existing_Menu, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Delete_a_Menu, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Save_to_Binary_Menu_File, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Load_Binary_Menu_File, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Replace_from_Binary_Menu_File, &MF_ENABLED
    End_If

    Call 'USER32.DrawMenuBar' D$H.MainWindow

ret


[H.VisualTutsFind: D$ ?
 H.VisualTutsMenu: D$ ?
 VisualTutMenuID: D$ ?]
[VisualTutsItem: B$ 'Visual Tuts' EOS]
[VisualTutPath: B$ ? # &MAX_PATH]
[WizardPath: B$ ? # &MAX_PATH]


EnableHelpMenutems:
    Call EnableHelpMenutem B_U_AsmName, M00_B_U_Asm
    Call EnableHelpMenutem Win32HlpName, M00_Win32_hlp
    Call EnableHelpMenutem MmediaHlpName, M00_Mmedia_hlp
    Call EnableHelpMenutem OpenGlHlpName, M00_OpenGl_hlp
    Call EnableHelpMenutem DxHlpName, M00_Dx_hlp
    Call EnableHelpMenutem WinsockHlpName, M00_WinSock_hlp
    Call EnableHelpMenutem SDLRefName, M00_SDL
    Call EnableHelpMenutem sqliteName, M00_sqlite
    Call EnableHelpMenutem DevILName, M00_DevIL
ret


[PreviousIVT: D$ ?]
[IVTFilesNames1 Trash1]
[IVTFilesNames2 Trash2]

EnableVisualTutsMenu:
  ; Enable the 'Visual Tuts' Item and create the Pop-Up if some Visual Tuts are there:
    Mov esi EquatesName, edi VisualTutPath, ecx (&MAX_PATH/DWORD) | rep movsd

    Mov esi VisualTutPath | While B$esi <> 0 | inc esi | End_While
    While B$esi <> '\' | dec esi | End_While
    Mov D$esi+1 'IVT*', D$esi+5 '.exe', B$esi+9 0

    Call 'KERNEL32.FindFirstFileA' VisualTutPath, FindFile

    .If eax = &INVALID_HANDLE_VALUE
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Visual_Tuts, &MF_GRAYED

    .Else
        Mov D$H.VisualTutsFind eax

        Call 'USER32.CreatePopupMenu' | Mov D$H.VisualTutsMenu eax

        Mov edi IVTFilesNames1, ecx 0

L1:     Mov esi FindFile.cFileName | inc ecx

        While B$esi <> 0 | movsb | End_While | Mov B$edi 0 | inc edi

        Push ecx
            Call 'KERNEL32.FindNextFileA' D$H.VisualTutsFind, FindFile
        Pop ecx

        On eax = &TRUE, jmp L1<

        Call zStringsSort IVTFilesNames1, IVTFilesNames2, ecx
        ____________________________________________________

        Mov B$PreviousIVT '0', D$VisualTutMenuID 5000

        Call 'USER32.CreatePopupMenu' | Mov D$H.VisualTutsMenu eax

        Mov esi IVTFilesNames2

L1:     add esi 6

        Push esi
            Mov al B$esi-3
            If al <> B$PreviousIVT
                Mov B$PreviousIVT al
                Call 'USER32.AppendMenuA' D$H.VisualTutsMenu, &MF_SEPARATOR, 0, 0
            End_If

            Call 'USER32.AppendMenuA' D$H.VisualTutsMenu, &MF_ENABLED__&MF_STRING,
                                      D$VisualTutMenuID, esi
        Pop esi

        While B$esi <> 0 | inc esi | End_While | inc esi

        If B$esi <> 0
            inc D$VisualTutMenuID | jmp L1<
        End_If

        Call 'USER32.InsertMenuA' D$H.MenuMain, M00_Visual_Tuts,
                                  &MF_BYCOMMAND__&MF_POPUP__&MF_STRING,
                                  D$H.VisualTutsMenu, VisualTutsItem

        Call 'USER32.DeleteMenu' D$H.MenuMain, M00_Visual_Tuts, &MF_BYCOMMAND

        Call 'KERNEL32.FindClose' D$H.VisualTutsFind
    .End_If
ret


VisualTuts:
    Push eax
        .If D$H.DebugDialog <> 0
            Call CloseDebuggerOrIgnore
            If eax = &IDNO
                Pop eax | ret
            End_If
        .End_If

        ...If D$FL.SourceReady = &TRUE
            ..If D$FL.ReadyToRun = &FALSE
                .If D$FL.SecurityWanted = &TRUE
                    Call 'USER32.MessageBoxA' D$H.MainWindow, {B$ 'Close the Actual File ?' EOS},
                                              {B$ 'Visual Tutorial' EOS}, &MB_YESNO
                    If eax = &IDNO
                        Pop eax | ret
                    End_If
                .End_If
            ..End_If
        ...End_If
    Pop eax

    Mov esi VisualTutPath | While D$esi <> '\IVT' | inc esi | End_While
    add esi 4 | Mov D$esi '???' | add esi 3
    Call 'USER32.GetMenuStringA' D$H.MenuMain, eax, esi, 100, &MF_BYCOMMAND

    Call 'KERNEL32.FindFirstFileA' VisualTutPath, FindFile

    .If eax <> &INVALID_HANDLE_VALUE
        Mov D$H.VisualTutsFind eax
        Call 'KERNEL32.FindClose' D$H.VisualTutsFind

        Mov esi VisualTutPath, edi SaveFilter
        While D$esi <> '\IVT' | movsb | End_While | movsb
        Mov esi FindFile.cFileName | While B$esi <> 0 | movsb | End_While | movsb

        Call DirectLoad

        Call ReInitUndo
        Call SetPartialEditionFromPos | Call EnableMenutems
        Call LoadBookMarks
    .End_If
ret


Proc EnableHelpMenutem:
    Argument @FileName, @Item

    Call 'KERNEL32.FindFirstFileA' D@FileName, FindFile
    Push eax
        If eax = &INVALID_HANDLE_VALUE
            Mov eax &MF_GRAYED
        Else
            Mov eax &MF_ENABLED
        End_If

        Call 'USER32.EnableMenuItem' D$H.MenuMain, D@Item, eax
    Pop eax
    Call 'KERNEL32.FindClose' eax
EndP
____________________________________________________________________________________________

[H.WizardsFind: D$ ?
 H.WizardsMenu: D$ ?
 WizardMenuID: D$ ?]

[PreviousWZRD: D$ ?]

[WizardsItem: B$ 'Wizards' EOS]

EnableWizardsMenu:

    Call ClearTrashTables

  ; Enable the 'Visual Tuts' Item and create the Pop-Up if some Visual Tuts are there:
    Mov esi EquatesName, edi WizardPath, ecx (&MAX_PATH/DWORD) | rep movsd

    Mov esi WizardPath | While B$esi <> 0 | inc esi | End_While
    While B$esi <> '\' | dec esi | End_While

    Mov D$esi+1 'WZRD', D$esi+5 '*.*'

    Call 'KERNEL32.FindFirstFileA' WizardPath, FindFile

    .If eax = &INVALID_HANDLE_VALUE
        Call 'USER32.EnableMenuItem' D$H.MenuMain, M00_Wizards, &MF_GRAYED

    .Else
        Mov D$H.WizardsFind eax

        Call 'USER32.CreatePopupMenu' | Mov D$H.WizardsMenu eax

        Mov edi Trash1, ecx 0

L1:     Mov esi FindFile.cFileName | inc ecx

        While B$esi <> 0 | movsb | End_While | Mov B$edi 0 | inc edi

        Push ecx
            Call 'KERNEL32.FindNextFileA' D$H.WizardsFind, FindFile
        Pop ecx

        On eax = &TRUE, jmp L1<

        Call zStringsSort Trash1, Trash2, ecx

        ____________________________________________________

        Mov B$PreviousWZRD '0', D$WizardMenuID 6000

        Call 'USER32.CreatePopupMenu' | Mov D$H.WizardsMenu eax

        Mov esi Trash2

L1:     add esi 4

        Push esi
           ; Mov al B$esi-3
           ; If al <> B$PreviousWZRD
           ;     Mov B$PreviousWZRD al
           ;     Call 'USER32.AppendMenuA' D$H.VisualTutsMenu, &MF_SEPARATOR, 0, 0
           ; End_If

            Call 'USER32.AppendMenuA' D$H.WizardsMenu, &MF_ENABLED__&MF_STRING,
                                      D$WizardMenuID, esi
        Pop esi

        While B$esi <> 0 | inc esi | End_While | inc esi

        If B$esi <> 0
            inc D$VisualTutMenuID | jmp L1<
        End_If

        Call 'USER32.InsertMenuA' D$H.MenuMain, M00_Wizards,
                                  &MF_BYCOMMAND__&MF_POPUP__&MF_STRING,
                                  D$H.WizardsMenu, WizardsItem

        Call 'USER32.DeleteMenu' D$H.MenuMain, M00_Wizards, &MF_BYCOMMAND

        Call 'KERNEL32.FindClose' D$H.WizardsFind
    .End_If
ret
____________________________________________________________________________________________

[ClipMenuID: 7000    NumberOfClipFiles: 0]

[H.FindClipFiles: D$ ?
 H.ClipFilePopUpMenu: D$ ?]

[ClipFilesPath: B$ ? # &MAX_PATH]

[ClipFilesItem: B$ 'Clip Files' EOS]

; Room for storing 20 Clip Files Names of 20 Chars each:

[ClipMenuStrings: D$ ? # 100]

Proc StoreClipMenuStrings:
    Argument @Source
    uses esi, edi

        Mov edi ClipMenuStrings
L1:     While B$edi <> 0 | inc edi | End_While
        If edi <> ClipMenuStrings
            inc edi | On B$edi <> 0, jmp L1<
        End_If
        Mov esi D@Source
        While B$esi <> 0 | movsb | End_While
EndP


EnableClipMenu: ; M00_Clip_File / 'LoadClipFile'
    Call 'KERNEL32.FindFirstFileA' ClipName, FindFile
;;
  Assuming Multiple Clip Files had never been planed, and the whole implementation
  is supposed to work withnone single 'Clip.txt', which Path is given in 'ClipName',
  from the Configuration.
  
  So, if no Clip File found, we simple leave, and let the default way:
;;
    ...If eax = &INVALID_HANDLE_VALUE
        jmp L9>>

    ...Else
        Call 'KERNEL32.FindClose' eax

        Mov esi ClipName, edi ClipFilesPath
        While B$esi <> 0 | movsb | End_While
        While B$edi <> '\' | dec edi | End_While | inc edi
        Mov D$edi '*Cli', D$edi+4 'p.tx', W$edi+8 't'

        Call 'KERNEL32.FindFirstFileA' ClipFilesPath, FindFile

        ..If eax = &INVALID_HANDLE_VALUE
            jmp L9>>

        ..Else
            Mov D$H.FindClipFiles eax

L1:         If D$H.ClipFilePopUpMenu = 0
                Call 'USER32.CreatePopupMenu' | Mov D$H.ClipFilePopUpMenu eax
            End_If

            inc D$NumberOfClipFiles

            Call 'USER32.AppendMenuA' D$H.ClipFilePopUpMenu,
                                      &MF_ENABLED__&MF_STRING,
                                      D$ClipMenuID, FindFile.cFileName

            Call StoreClipMenuStrings FindFile.cFileName

            inc D$ClipMenuID

            Call 'KERNEL32.FindNextFileA' D$H.FindClipFiles, FindFile
            On eax = &TRUE, jmp L1<<

            Call 'KERNEL32.FindClose' D$H.FindClipFiles

            .If D$H.ClipFilePopUpMenu <> 0
                If D$NumberOfClipFiles > 1
                    Call 'USER32.InsertMenuA' D$H.MenuMain, M00_ClipFile,
                                              &MF_BYCOMMAND__&MF_POPUP__&MF_STRING,
                                              D$H.ClipFilePopUpMenu, ClipFilesItem

                    Call 'USER32.DeleteMenu' D$H.MenuMain, M00_ClipFile, &MF_BYCOMMAND
                End_If
            .End_If

        ..End_If

    ...End_If
L9: ret
____________________________________________________________________________________________


SetShiftBlock:
    Mov eax D$STRUCT.EditData@CaretRow, ebx D$STRUCT.EditData@CaretLine
    Push eax, ebx

        Call SearchTxtPtr               ; >>> eax = new Pos.

        .If D$FL.BlockInside = &FALSE
            If eax > D$ShiftDown
                dec eax
            End_If

        .Else
            If eax = D$ShiftDown
                Mov D$FL.BlockInside &FALSE | jmp L9>
            Else_If eax > D$ShiftDown
                dec eax
            End_If

        .End_If

        If eax < D$ShiftDown

            Mov D$LP.BlockStartText eax

            Move D$LP.BlockEndText D$ShiftDown

            sub D$LP.BlockEndText (1*ASCII)

        Else

            Move D$LP.BlockStartText D$ShiftDown

            Mov D$LP.BlockEndText eax

        End_If

        Mov D$FL.BlockInside &TRUE, B$ShiftBlockInside &TRUE

L9: Pop D$ShiftBlockLine, D$ShiftBlockCol
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  BackUps.
;;

____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
