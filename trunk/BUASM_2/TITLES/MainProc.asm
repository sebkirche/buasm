TITLE MainProc

 ______________________________________________________________________________________
 ______________________________________________________________________________________

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
 M00_Ascii_Table  1027           M00_Show_RosAsm_Mems  1028      M00_Show_Symbols_Repartition  1029
 M00_Local_mem_Tests  1030       M00_Serial_Compilations  1031   M00_Data_to_Equates  1032
 M00_Encoding  1033              M00_DLLs_Scanner  1034          M00_Libs_Scanner  1035
 M00_Configuration  1036         M00_Create_Config_bin  1037     M00_Main_Icon  1038
 M00_Load_Icon  1039             M00_Delete_Icon  1040           M00_Icon_IDs  1041
 M00_Save_Icon  1042             M00_Load_BitMap  1043           M00_Delete_BitMap  1044
 M00_BitMaps_IDs  1045           M00_Save_BitMap  1046           M00_Load_Cursor  1047
 M00_Delete_Cursor  1048         M00_Cursors_IDs  1049           M00_Save_Cursor  1050
 M00_Load_Wave  1051             M00_Delete_Wave  1052           M00_Waves_IDs  1053
 M00_Save_Wave  1054             M00_Load_Avi  1055              M00_Delete_Avi  1056
 M00_Avi_IDs  1057               M00_Save_Avi  1058              M00_Load_RC  1059
 M00_Delete_RC  1060             M00_RCs_IDs  1061               M00_Save_RC  1062
 M00_New_Dialog  1063            M00_Load_from_Resources  1064   M00_Load_from_ClipBoard  1065
 M00_Load_from_File  1066        M00_Save_to_Binary_File  1067   M00_Load_from_Binary_File  1068
 M00_Replace_from_Binary_File  1069                              M00_Delete_Resources_Dialog  1070
 M00_Strings  1071               M00_New_Menu  1072              M00_Existing_Menu  1073
 M00_Delete_a_Menu  1074         M00_Save_to_Binary_Menu_File  1075
 M00_Load_Binary_Menu_File  1076 M00_Replace_from_Binary_Menu_File  1077
 M00_Clip_File  1078             M00_Structures  1079            M00_Api_Functions  1080
 M00_Sys_Resources  1081         M00_GUIDs  1082                 M00_Win32_Equates  1083
 M00_Win32_Data_Types  1084      M00_Wizards  1085               M00_B_U_Asm  1086
 M00_Sources_Editor  1087        M00_Visual_Tuts  1088           M00_Win32_hlp  1089
 M00_Mmedia_hlp  1090            M00_OpenGl_hlp  1091            M00_WinSock_hlp  1092
 M00_Dx_hlp  1093                M00_SDL  1094                   M00_sqlite  1095
 M00_DevIl  1096                 M00_About  1097                 M00_GPL_License  1098
 M00_RosAsm_License  1099        M00_<<<<  1100                  M00_>>>>  1101]

[M00_About_ToolBar 1200]
____________________________________________________________________________________________

[redrawFlag: 0, ReadyToRun: 0, ShowStats: &TRUE, UnusedSymbolsDialogWanted: &FALSE]

[NotReadyToRun L7>>    NoClient L8>>]

[CloseDebuggerOrIgnore
    If D$DebugDialogHandle <> 0
        Call KillDebugger | On eax = &IDNO, jmp L9>>
    End_If]

; buffer for returned filename:
[lpszFile: B$ 0 #&MAXPATH]

; size of buffer for filename:
[cch: D$ &MAXPATH]

[STR.A.AppName: "  BUAsm, The Bottom-Up Assembler -V.0.00.001-" EOS]

Proc MainWindowProc:
;;
    TODO Double left-clic -> Search from Top

    [TopOffCode] -> Right-clic
    
    [MAIN]
    
    AppName -> STR.A.AppName
    H.MainWindow -> H.MainWindow
    @Adressee -> @hwnd
    @Handle -> @hwnd
    @Message -> @msg
    
    call -> Call
    Mov -> Mov
    
    Call 'MODULE.ApiName' selon convention
    
    Implémentation d'une partie des nouvelles Macros conventionées
    
    implémentation des Equates HLL et [D$HWND] etc pour éviter les transmissions via arguments 
;;

;;
    At the attention of all RosAsm contributors ---> 'Rules'
    
  'CreateTitleTab'
    
  'NewReplaceMacAndEqu', 'GetFileNameFromPath'
  
  'CheckMRUFile', 'RightClick', 'WheelMsg'
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

    pushad

    If D@msg = &WM_CREATE
        Call 'SHELL32.DragAcceptFiles' D@hwnd, &TRUE | jmp L9>>

    Else_If D@msg = &WM_DROPFILES
        Call 'SHELL32.DragQueryFile' D@wParam, 0, lpszFile, D$cch
        Mov esi lpszFile, edi SaveFilter
        while B$esi <> 0 | movsb | End_While | movsb
        Call DirectLoad
        Call StartEdition
        Call ReInitUndo
        Call SetPartialEditionFromPos
        Call EnableMenutems
        Call LoadBookMarks
        Call AskForRedraw
    End_If

    Mov eax D@hwnd

    .If eax = D$H.MainWindow
        ; OK
    .Else_If eax = D$EditWindowHandle
        Mov eax D$wc_hCursor
        If D$ActualCursor <> eax
            Mov D$ActualCursor eax
            Call 'USER32.SetClassLongA' D$EditWindowHandle, &GCL_HCURSOR, eax
        End_If
    .Else_If eax = D$ScrollWindowHandle
        ; OK
    .Else_If eax = D$BpWindowHandle
        Mov eax D$Bp_hCursor
        If D$ActualCursor <> eax
            Mov D$ActualCursor eax
            Call 'USER32.SetClassLongA' D$BpWindowHandle, &GCL_HCURSOR, eax
        End_If
    .Else
        jmp NoClient
    .End_If
  ; Yes, this may happend when [Run]ing. And this works because we do not hold anything
  ; at creation time.
 ____________________________________


; General purpose Callback:

    Mov eax D@msg

    ...If eax = &WM_KEYDOWN
        .If B$SourceReady = &TRUE
            movzx eax B@Wparam | Mov B$Keys+eax 1

            Call KeyMessage ; CharMessage

            On B$BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
            If B$KeyHasModifedSource = &TRUE

                Mov B$SourceHasChanged &TRUE
                Call AskForRedraw |
              jmp NotReadyToRun
           ; Else_If B$KeyHasMovedCaret = &TRUE
           ;     Call AskForRedraw
            End_If

        .Else_If D@wParam = &VK_F1
            Call RosAsmHelp

        .Else_If D@wParam = &VK_F2
            Call F2Help

        .End_If

    ...Else_If eax = &WM_KEYUP
        movzx eax B@Wparam | Mov B$Keys+eax 0
        On eax = &VK_MENU, Mov B$keys+&VK_CONTROL &FALSE

    ...Else_If eax = &WM_CLOSE
        CloseDebuggerOrIgnore

        On B$OnDialogEdition = &TRUE, Call CloseDialogEdition

        Call Security | On eax = &IDCANCEL, jmp L9>>

        If B$SaveMainPosFlag = &TRUE
            Call 'USER32.ShowWindow' D$H.MainWindow, &SW_RESTORE
            Call 'USER32.GetWindowRect' D$H.MainWindow, WindowX
            Mov eax D$WindowX | sub D$WindowW eax
            Mov eax D$WindowY | sub D$WindowH eax
            Call UpdateRegistry
        End_If

        On B$ToolBarChange = &TRUE, Call SaveToolBar

        Call LeftButtonUp                            ; ensure no confined mouse
        Call CloseHelp

        On D$EditedDialogHandle <> 0, Call 'User32.DestroyWindow' D$EditedDialogHandle
        On D$DialogEditorHandle <> 0 Call 'User32.EndDialog' D$DialogEditorHandle

        Call 'USER32.DestroyWindow' D$H.MainWindow | jmp L9>> ; works too with 'jmp NoClient'

    ...Else_If eax = &WM_GETMINMAXINFO
        Mov eax D@lParam
        Mov D$eax+24 500, D$eax+28 300
        popad | Mov eax &FALSE | ExitP

    ...Else_If eax = &WM_SIZE
        On D$TitleWindowHandle > 0, Call KillTitleTab

        Call MainResize

    ...Else_If eax = &WM_MOVING
        On D$TitleWindowHandle <> 0, Call KillTitleTab

    ...Else_If eax = &WM_DESTROY
        On B$OnDialogEdition = &TRUE, Call CloseDialogEdition
        Call DeleteUndoFiles | Call KillUndo
        Call ReleaseResourceMemory
        Call 'User32.PostQuitMessage' 0 | jmp L9>>

    ...Else_If D@msg = &WM_NOTIFY
        Call KillCompletionList
      ; ebx > NMHDR // eax > hwndFrom.
        Mov ebx D@lParam, eax D$ebx
      ; ToolBar Messages:
        .If D$ebx+TOOLTIPTEXT_NMHDR_code = &TTN_NEEDTEXT
            Mov eax D$ebx+TOOLTIPTEXT_NMHDR_idfrom
          ; Pointing with esi to the Buttons List IDs:
            lea esi D$ToolBarButtons+4
            Mov ecx 0
            While D$esi <> eax
                add esi 20 | inc ecx | On ecx > TOOLBUTTONS_NUMBER, ExitP
            End_While
            Mov eax D$PointersToToolTipsStrings+ecx*4
            Mov D$ebx+TOOLTIPTEXT_lpszText eax

        .Else_If D$ebx+TOOLTIPTEXT_NMHDR_code = &TBN_QUERYINSERT ; May be inserted ?
            popad | Mov eax &TRUE | ExitP                   ; > yes for all.

        .Else_If D$ebx+TOOLTIPTEXT_NMHDR_code = &TBN_QUERYDELETE ; May be deleted?
                        popad | Mov eax &TRUE | ExitP                   ; > yes for all.

        .Else_If D$ebx+TOOLTIPTEXT_NMHDR_code = &TBN_FIRST  ; = &TBN_GETBUTTONINFO (missing?)
            Mov ecx D$ebx+TB_NOTIFY_Item, edx ecx
            If ecx a TOOLBUTTONS_NUMBER
                popad | Mov eax &FALSE | ExitP
            End_If
            lea ecx D$ecx*4+ecx | shl ecx 2                 ; ecx = ecx * 20 >>>
            add ecx ToolBarButtons                          ; Pointer to whished Button Data
            move D$ebx+TB_NOTIFY_TBBUTTON_iBitmap D$ecx
            move D$ebx+TB_NOTIFY_TBBUTTON_idCommand  D$ecx+4
            move D$ebx+TB_NOTIFY_TBBUTTON_fsState D$ecx+8
            move D$ebx+TB_NOTIFY_TBBUTTON_dwData D$ecx+12
            move D$ebx+TB_NOTIFY_TBBUTTON_iString D$ecx+16
            Mov edi D$ebx+TB_NOTIFY_TextPtr
            Mov edx D$ebx+TB_NOTIFY_Item | shl edx 2        ; Displacement to pointers
            Mov eax PointersToToolTipsStrings | add eax edx ; Pointer
            Mov esi D$eax                                   ; Source
            Mov ecx 0-1                                     ; Counter for Non-zero-ended text
            Do
                lodsb | stosb | inc ecx
            Loop_Until al = 0
            Mov D$ebx+TB_NOTIFY_CharCount ecx               ; Lenght of String    (+36)
            popad | Mov eax &TRUE | ExitP

        .Else_If D$ebx+TOOLTIPTEXT_NMHDR_code = &TBN_TOOLBARCHANGE
            Mov B$ToolBarChange &TRUE                       ; For Saving at Exit if TRUE.

            ; Next instruction of no use usually, but, under some unknown circumstances
            ; the Buttons Size may be changed (Win98 without IE). So, as it can't hurt:
            Call 'USER32.SendMessageA' D$ToolBarHandle, &TB_SETBUTTONSIZE, 0, 014_0014

        .Else_If D$ebx+TOOLTIPTEXT_NMHDR_code = &TBN_RESET
            Call 'USER32.SendMessageA' D$ToolBarHandle, &TB_SAVERESTORE, &FALSE,
                                    TOOLBAR_REGISTRY        ; FALSE > restore

        .Else_If D$ebx+TOOLTIPTEXT_NMHDR_code = &TBN_CUSTHELP
            Call 'USER32.MessageBoxA' D$H.MainWindow, HelpToolBar, HelpToolBarTitle,
                                    &MB_OK__&MB_SYSTEMMODAL
        .End_If


 ____________________________________

; Main window CallBack: User choices open-file independant:

    ...Else_If eax = &WM_COMMAND
        Call KillCompletionList

        Mov eax D@wParam | and eax 0FFFF

        If B$IncludesOK = &FALSE
            Call Configuration | Call 'User32.SendMessageA' D$H.MainWindow &WM_CLOSE 0 0
        End_If

        ..If eax = M00_Open
            CloseDebuggerOrIgnore

            Call Security | On eax = &IDCANCEL, jmp L9>>

            Call OpenRosAsmPE

            Call UpdateTitlesFromIncludeFiles

            If D$SaveFilter <> 0
                Call ReInitUndo
                Call SetPartialEditionFromPos | Call EnableMenutems
                Call LoadBookMarks
            End_If

        ..Else_If eax = M00_Open_Source_Only
            CloseDebuggerOrIgnore

            Call Security | On eax = &IDCANCEL, jmp L9>>

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
            Call 'User32.SendMessageA' D$H.MainWindow &WM_CLOSE 0 0

        ..Else_If eax = M00_Sources_Editor
            Call Help, B_U_AsmName, SourceEditor, ContextHlpMessage

        ..Else_If eax = M00_Main_Icon
            CloseDebuggerOrIgnore
            Call IconEdition | jmp NotReadyToRun

        ..Else_If eax = M00_New_Menu
            CloseDebuggerOrIgnore
            Call NewMenu | Mov D$ActualMenutestID 0 | Call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Existing_Menu
            CloseDebuggerOrIgnore
            Call ExistingMenu | Mov D$ActualMenutestID 0 | jmp NotReadyToRun

        ..Else_If eax = M00_Delete_a_Menu
            CloseDebuggerOrIgnore
            Call DeleteMenu | Mov D$ActualMenutestID 0 | Call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Save_to_Binary_Menu_File
            CloseDebuggerOrIgnore
            Call SaveMenuBinaryFile

        ..Else_If eax = M00_Load_Binary_Menu_File
            CloseDebuggerOrIgnore
            Call LoadMenuBinaryFile | jmp NotReadyToRun

        ..Else_If eax = M00_Replace_from_Binary_Menu_File
            CloseDebuggerOrIgnore
            Call ReplaceMenuBinaryFile | jmp NotReadyToRun

        ..Else_If eax = M00_About | Call AboutBox

        ..Else_If eax = M00_About_ToolBar | Call About_ToolBar

        ..Else_If eax = M00_GPL_License | Call GPLView

        ..Else_If eax = M00_RosAsm_License | Call LicenseView

        ..Else_If eax = M00_B_U_Asm | Call RosAsmHelp

        ..Else_If eax = M00_Configuration | Call Configuration

        ..Else_If eax = M00_Create_Config_bin | Call Create_Config_bin

        ..Else_If eax = M00_Api_Functions | Call ViewApiList

        ..Else_If eax = M00_New
            CloseDebuggerOrIgnore
            Call Security | On eax = &IDCANCEL, jmp L9>>
            Call AutoNew
            and D$RelocsWanted 0 ; jE!
            Call ChangeName
            cmp D$SavingExtension '.DLL' | setz B$RelocsWanted ; jE! Relocs for Dll
            Call StartNewFile

        ..Else_If eax = M00_New_Model
            CloseDebuggerOrIgnore
            Call Security | On eax = &IDCANCEL, jmp L9>>
            Call NewFileNameDialog

        ..Else_If eax = M00_Change_Compile_Name | Call ChangeName

        ..Else_If eax = M00_Calc | Call Calc

        ..Else_If eax = M00_DLLs_Scanner | Call ExportScanner

        ..Else_If eax = M00_Libs_Scanner | Call LibScanner

        ..Else_If eax = M00_New_Dialog
            CloseDebuggerOrIgnore
            If B$OnDialogEdition = &FALSE
                Mov B$SameIdAllowed &FALSE
                On B$BlinkingCaretWanted = &TRUE, Call KillBlinkCursor
                    Call InitDialogEdition | Call ReleaseDialogMemories
                On B$BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
                Call EnableMenutems | jmp NotReadyToRun
            End_If

        ..Else_If eax = M00_Load_from_ClipBoard
            CloseDebuggerOrIgnore
            If B$OnDialogEdition = &FALSE
                Mov B$SameIdAllowed &FALSE
                On B$BlinkingCaretWanted = &TRUE, Call KillBlinkCursor
                    Call ReleaseDialogMemories | Call LoadDialogFromClipBoard
                On B$BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
                Call EnableMenutems | jmp NotReadyToRun
            End_If

        ..Else_If eax = M00_Load_from_File
            CloseDebuggerOrIgnore
            If B$OnDialogEdition = &FALSE
                Mov B$SameIdAllowed &FALSE
                On B$BlinkingCaretWanted = &TRUE, Call KillBlinkCursor
                    Call ReleaseDialogMemories | Call LoadDialogFromFile
                On B$BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
                Call EnableMenutems | jmp NotReadyToRun
            End_If

        ..Else_If eax = M00_Load_from_Resources
            CloseDebuggerOrIgnore
            If B$OnDialogEdition = &FALSE
                Mov B$SameIdAllowed &TRUE
                On B$BlinkingCaretWanted = &TRUE, Call KillBlinkCursor
                    Call ReleaseDialogMemories | Call LoadFromResources
                On B$BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
                jmp NotReadyToRun
            End_If

        ..Else_If eax = M00_Save_to_Binary_File
            CloseDebuggerOrIgnore
            Call SaveToBinaryFile

        ..Else_If eax = M00_Load_from_Binary_File
            CloseDebuggerOrIgnore
            Mov B$SameIdAllowed &TRUE
            Call LoadFromBinaryFile | Call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Replace_from_Binary_File
            CloseDebuggerOrIgnore
            Call ReplaceFromBinaryFile | jmp NotReadyToRun

        ..Else_If eax = M00_Delete_Resources_Dialog
            CloseDebuggerOrIgnore
            On B$BlinkingCaretWanted = &TRUE, Call KillBlinkCursor
                Call DeleteDialog | Call EnableMenutems
            On B$BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
            jmp NotReadyToRun

        ..Else_If eax = M00_Load_BitMap
            CloseDebuggerOrIgnore
            Call LoadBitMap | Call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Delete_BitMap
            CloseDebuggerOrIgnore
            Call DeleteBitMap | Call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_BitMaps_IDs | Call ShowBitMapsIds

        ..Else_If eax = M00_Load_Wave
            CloseDebuggerOrIgnore
            Call ReadWaveFile | Call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Delete_Wave
            CloseDebuggerOrIgnore
            Call DeleteWave | Call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Load_Avi
            CloseDebuggerOrIgnore
            Call ReadAviFile | Call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Delete_Avi
            CloseDebuggerOrIgnore
            Call DeleteAviFile | Call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Load_RC
            CloseDebuggerOrIgnore
            Call ReadRcData | Call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Save_RC
            CloseDebuggerOrIgnore
            Call SaveRcData | jmp NotReadyToRun

        ..Else_If eax = M00_Delete_RC
            CloseDebuggerOrIgnore
            Call DeleteRcData | Call EnableMenutems | jmp NotReadyToRun

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
            CloseDebuggerOrIgnore
            Call StringsResources | jmp NotReadyToRun

        ..Else_If eax = M00_Load_Cursor
            CloseDebuggerOrIgnore
            Call ReadCursor | Call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Delete_Cursor
            CloseDebuggerOrIgnore
            Call DeleteCursor | Call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Load_Icon
            CloseDebuggerOrIgnore
            Call ReadIcon | Call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Delete_Icon
            CloseDebuggerOrIgnore
            Call DeleteIcon | Call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Clip_File | Call Templates
           ; Call SetQwordCheckSum String2

        ..Else_If eax = M00_Data_to_Equates | Call DataToStructure

        ..Else_If eax = M00_Structures | Call StructDialog

        ..Else_If eax = M00_Sys_Resources | Call ViewSysResources

        ..Else_If eax = M00_GUIDs | Call ViewGUIDs

        ..Else_If eax = M00_Show_RosAsm_Mems | Call ViewRosAsmMems

        ..Else_If eax = M00_Show_Symbols_Repartition | Call TestRepartition

        ..Else_If eax = M00_Local_mem_Tests | Call TestLocals

        ..Else_If eax = M00_Serial_Compilations | Call MultipleCompileTests

        ..Else_If eax = M00_Encoding
            CloseDebuggerOrIgnore

            Call ViewEncoding

            jmp NotReadyToRun

        ..Else
            .If eax > 6999                      ; Clip Files
                Call Templates

            .Else_If eax > 5999                  ; Wizards
                CloseDebuggerOrIgnore

                On B$SourceReady = &FALSE,
                    Call 'USER32.SendMessageA', D$H.MainWindow, &WM_COMMAND, M00_New, 0

                On B$SourceReady = &TRUE, Call NewWizardForm

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
                    CloseDebuggerOrIgnore
                    push eax
                        Call Security       ; 'Security' also uses eax as return Value.
                    pop ebx
                    On eax = &IDCANCEL, jmp L9>>
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
        ..End_if
        jmp L9>>

    ...End_If

L5: If B$SourceReady = &FALSE
        On D@msg = &WM_PAINT, Call SplashScreen
        jmp NoClient
    End_If
 __________________________________________

 ; Main window CallBack going on: With opened file only:

    Mov eax D@msg

    ...If eax = &WM_PAINT
        .If D$CodeSource > 0
            Call TextPos
            Mov eax D@hwnd
            If eax = D$EditWindowHandle
                Call PrintColorText
            Else_If eax = D$BpWindowHandle
                Call PrintBp
            End_If

            If B$MovingBlock = &TRUE
                Call SetBlock | Call AskForRedraw
            End_If

            Call StatusBar

            Mov B$RedrawFlag &FALSE
        .End_If

        Mov eax D$EditWindowHandle | On D@hwnd <> eax, jmp NoClient

    ...Else_If eax = &WM_CHAR
        ..If B$RedrawFlag = &FALSE
            Mov B$RedrawFlag &TRUE

            Call CharMessage D@wParam | or B$SourceHasChanged al

            Call AskForRedraw | jmp NotReadyToRun
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
        .If B$RedrawFlag = &FALSE
            CloseDebuggerOrIgnore

            Mov B$RedrawFlag &TRUE
            Mov eax D@Wparam | Call InsertDoubleByte | Call AskForRedraw
            jmp NotReadyToRun
        .End_If

    ...Else_If eax = &WM_MOUSEWHEEL
        Mov eax D@wParam | Call WheelMsg | Call KillCompletionList

    ...Else_If eax = &WM_XBUTTONDOWN
        ..If W@Wparam+2 = &XBUTTON1
            Mov B$BlockInside &FALSE
            Call RestoreRealSource | Call BackClick | Call SetPartialEditionFromPos

        ..Else_If W@Wparam+2 = &XBUTTON2
            Mov B$BlockInside &FALSE
            Call RestoreRealSource | Call ForwardClick | Call SetPartialEditionFromPos
        ..End_If

        popad | Mov eax &TRUE | ExitP

    ...Else_If eax = &WM_COMMAND
        Mov eax D@Wparam | and eax 0FFFF

        ..If eax = M00_<<<<_
            Mov B$BlockInside &FALSE
            Call RestoreRealSource | Call BackClick | Call SetPartialEditionFromPos

        ..Else_If eax = M00_>>>>_
            Mov B$BlockInside &FALSE
            Call RestoreRealSource | Call ForwardClick | Call SetPartialEditionFromPos

        ..Else_If eax = M00_Compile
            Mov D$ShowStats &TRUE
            Call Compile

        ..Else_If eax = M00_Run
            Call Run

        ..Else_If eax = M00_Optimize_Jumps_Sizes
            Call Optimize

       ; ..Else_If eax = M00_Profile | Call Profiler

        ..Else_If eax = M00_Paste_at_Pos
            On D$IsDebugging = &TRUE, jmp L9>>
            Call RestoreRealSource | Call IncludeSource | Call SetPartialEditionFromPos
            Mov B$SourceHasChanged &TRUE, B$FirstBlockDraw &FALSE
            Call SetCaret D$CurrentWritingPos

        ..Else_If eax = M00_Find
            Mov D$NextSearchPos 0 | Call SetSimpleSearchBox

        ..Else_If eax = M00_Replace
            Mov D$NextSearchPos 0 | On D$IsDebugging = &FALSE, Call SetFindReplaceBox

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
            On D$IsDebugging = &TRUE, jmp L9>>
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
            Call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_Delete
            Call ControlX | Call AskForRedraw
            Call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_Replace
            Call ControlD | Call ControlV | Call AskForRedraw
            Call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_SearchUp
            Call RestoreRealSource
            Call StorePosInBackTable | Call SearchUpFromFloatMenu
            Call SetPartialEditionFromPos
            Call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_SearchDown
            Call RestoreRealSource
            Call StorePosInBackTable | Call SearchDownFromFloatMenu
            Call SetPartialEditionFromPos
            Call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_SearchFromTop
            Call RestoreRealSource
            Call StorePosInBackTable | Call SearchFromTopFromFloatMenu
            Call SetPartialEditionFromPos
            Call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_Unfold
            Call 'USER32.DestroyMenu' D$FloatHandle
            Call RestoreRealSource | Call ShowUnfoldMacro | Call SetPartialEditionFromPos

        ..Else_If eax = Float_BookMark | Call StoreBookMark
            Call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_UnBookMark | Call DeleteBookMark
            Call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_Number | Call ViewClickedNumber
            Call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_SelReplace
            Call 'USER32.DestroyMenu' D$FloatHandle | Call BlockReplaceAll

        ..Else_If eax = FloatSetBp
            Call 'USER32.DestroyMenu' D$FloatHandle
            Call SetBreakPoint | Call DoStoreBP | jmp NotReadyToRun

        ..Else_If eax = FloatDelBp
            Call 'USER32.DestroyMenu' D$FloatHandle
            Call DeleteBreakPoint | Call DoStoreRemoveBP | jmp NotReadyToRun

        ..Else_If eax = FloatDelAllBp
            Call 'USER32.DestroyMenu' D$FloatHandle
            Call DeleteAllBreakpoints | jmp NotReadyToRun

        ..Else_If eax = Float_BadDisLabel
            Call 'USER32.DestroyMenu' D$FloatHandle
            Call ForcedFlags

        ..Else_If eax = M00_Output
            Call OutputFormat | Call EnableMenutems | jmp NotReadyToRun

        ..End_If

    ...Else_If eax = &WM_SETCURSOR
        Mov eax D@wParam
        .If eax = D$StatusBarHandle
            If D$TitleWindowHandle = 0
                Call ShowTitles
            End_If
        .End_If

        jmp NoClient

    ...Else_If eax = &WM_MOUSEMOVE
        .If D$TitleWindowHandle > 0
            Mov ebx D$TabWindowY
            On W@lParam+2 < bx, Call KillTitleTab
        .End_If

        If D@Wparam <> &MK_LBUTTON                          ; Is the Left Button Down?
            Call 'USER32.ClipCursor' &NULL | jmp NoClient
        End_If

        On B$UserHaveClickDown = &FALSE, jmp NoClient
            On B$UserClickAfterEnd = &TRUE, jmp NoClient
                push D@Lparam | pop W$MousePosX, W$MousePosY

                Call SetBlock

                If B$FirstBlockDraw = &FALSE
                    On B$BlockRedraw = &TRUE, Call AskForRedrawNow
                End_If

    ...Else_If eax = &WM_LBUTTONDOWN
        Mov B$ShiftBlockInside &FALSE

        Call KillCompletionList

        On B$BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
        push D@Lparam | pop W$MousePosX, W$MousePosY

        Mov B$BlockInside &FALSE, eax D@hwnd | Call LeftButton

        Call AskForRedrawNow
        Mov D$NextSearchPos 0, B$UserHaveClickDown &TRUE

    ...Else_If eax = &WM_LBUTTONUP
        On B$BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
        On B$FirstBlockDraw = &TRUE, Mov B$BlockInside &FALSE
        Call LeftButtonUp | Mov B$UserHaveClickDown &FALSE | Call AskForRedrawNow

    ...Else_If eax = &WM_RBUTTONUP
        Call KillCompletionList
        On B$BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor

        Mov eax D@hwnd
        .If eax = D$BpWindowHandle
            If D$DBPMenuOn = RIGHT_CLICK_ACTION
                Mov B$ShiftBlockInside &FALSE

                Call KillCompletionList

                On B$BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
                Call RestoreRealSource
                    push D@Lparam | pop W$MousePosX, W$MousePosY
                    Call MarginRightClick
                Call SetPartialEditionFromPos
            End_If

            jmp NoClient
        .End_If

        Call RestoreRealSource
            push D@Lparam | pop W$MousePosX, W$MousePosY
            Call RightClick
        Call SetPartialEditionFromPos

    ...Else_If eax = &WM_LBUTTONDBLCLK
        Mov B$ShiftBlockInside &FALSE

        Call KillCompletionList

        On B$BlinkingCaretWanted = &TRUE, Call ResetBlinkCursor
        Call RestoreRealSource
            push D@Lparam | pop W$MousePosX, W$MousePosY
            Call DoubleClick
        Call SetPartialEditionFromPos

    ...Else_If eax = D$FindStringMessage
        Call KillCompletionList

        Call StringSearch | Call AskForRedraw

    ...Else
        jmp NoClient

    ...End_If
 ____________________________________

L9: popad | Mov eax &FALSE | jmp P9>>

L7: ; NotReadyToRun:
    .If B$ReadyToRun = &TRUE         ; (Something has just been modified).
        Mov B$ReadyToRun &FALSE
        VirtualFree D$IpTable, D$StatementsTable, D$StatementsTable2
    .End_If

    Mov B$SourceHasChanged &TRUE

    popad | Mov eax &FALSE | jmp P9>

L8: ; NoClient:
    popad | Call 'User32.DefWindowProcA' D@hwnd D@msg D@wParam D@lParam
EndP

____________________________________________________________________________________________

Compile:
        If D$UnusedCodeAndDataDialogHandle = &FALSE
            On B$Compiling = &TRUE, ret
            On D$IsDebugging = &TRUE, ret
        Else
            Mov D$ShowStats &FALSE
        End_If

        Mov B$RecompileWanted &FALSE, B$Compiling &TRUE
        Call RestoreRealSource

        On D$UnusedCodeAndDataDialogHandle <> 0, Call ReInitUnusedDialog

        Call AsmMain | Mov D$OldStackPointer 0

        If B$CompileErrorHappend = &FALSE
            Mov B$ReadyToRun &TRUE, B$SourceHasChanged &FALSE
        End_If

        Call SetPartialEditionFromPos
        Call AskForRedraw

        Mov B$Compiling &FALSE | Call ResetKeys
ret


Run:
    If D$UnusedCodeAndDataDialogHandle <> &FALSE
      ; 'UnusedCodeAndDataDialogCallBack'
        Call 'USER32.EndDialog' D$UnusedCodeAndDataDialogHandle, 0
        Mov D$UnusedCodeAndDataDialogHandle 0
        Mov B$Compiling &FALSE, B$UnusedSymbolsDialogWanted &FALSE, B$ShowStats &FALSE
        Mov B$UnusedSymbolsDialogWanted &FALSE
    End_If

    On B$Compiling = &TRUE, ret
    On D$IsDebugging = &TRUE, ret

    Mov B$ShowStats &FALSE, B$Compiling &TRUE, B$RecompileWanted &FALSE

    Call RestoreRealSource

    If B$ReadyToRun = &FALSE
        Mov B$ShowStats &FALSE
        Call AsmMain
        Mov D$OldStackPointer 0
        Mov D$UnusedSymbolsDialogWanted &FALSE

        On B$CompileErrorHappend = &FALSE, Mov B$ReadyToRun &TRUE
    End_If

    Call SetPartialEditionFromPos

    If B$CompileErrorHappend = &FALSE
        Mov B$SourceHasChanged &FALSE
        Call Debugger
    End_If

    Mov B$Compiling &FALSE | Call ResetKeys
ret


Optimize:
    Mov B$AlignFound &FALSE

    If D$UnusedCodeAndDataDialogHandle <> &FALSE
      ; 'UnusedCodeAndDataDialogCallBack'
        Call 'USER32.EndDialog' D$UnusedCodeAndDataDialogHandle, 0
        Mov D$UnusedCodeAndDataDialogHandle 0
        Mov B$Compiling &FALSE, B$UnusedSymbolsDialogWanted &FALSE, B$ShowStats &FALSE
    End_If

    If B$ReadyToRun = &FALSE
        Mov D$ShowStats &FALSE
        Call Compile
    End_If

    .If B$ReadyToRun = &TRUE
        If B$AlignFound = &TRUE
            Call 'USER32.MessageBoxA' &NULL, NoptimizeMessage,
                                      NoptimizeTitle, &MB_SYSTEMMODAL
        Else
            Mov B$ShortenJumpsWanted &TRUE
            Mov D$ShowStats &TRUE
            Call Compile
            Mov B$ShortenJumpsWanted &FALSE
        End_If
    .End_If
ret

[NoptimizeTitle: 'Optimizer', 0
 NoptimizeMessage: "The Assembler can not yet optimize the Jump Sizes  
 on a Source making use of the Align Statement:
 
 The Alignments would be broken.", 0]
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc ScrollBarProc:
    Arguments @hwnd, @msg, @wParam, @lParam

    On B$SourceReady = &FALSE, jmp L8>>

    pushad

    ...If D@msg = &WM_VSCROLL
        Call KillCompletionList

        Call 'USER32.IsMenu' D$FloatHandle
        ..If eax = &FALSE
            .If W@wParam = &SB_THUMBTRACK
                If D$TotalNumberOfLines < 0FFFF
                    movzx edx W@wParam+2
                Else
                    Call 'USER32.GetScrollInfo' D$ScrollWindowHandle, &SB_VERT, VScroll
                    Mov edx D$VScroll.nTrackPos
                End_If
                Call RePosFromScroll

            .Else_If W@wParam = &SB_LINEDOWN    ; SB > ScrollBar.
                Call DownOneLine

            .Else_If W@wParam = &SB_LINEUP
                Call UpOneLine

            .Else_If W@wParam = &SB_PAGEDOWN
                Mov ecx D$LineNumber
L4:             push ecx | Call DownOneLine | pop ecx | loop L4<

            .Else_If W@wParam = &SB_PAGEUP
                Mov ecx D$LineNumber
L0:             Call UpOneLine | loop L0<

            .End_If
            Call AskForRedraw
        ..End_If

        popad | Mov eax &FALSE | ExitP

     ...End_If

     On D$TitleWindowHandle > 0, Call KillTitleTab

    popad
L8: Call 'User32.DefWindowProcA' D@hwnd D@msg D@wParam D@lParam
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

[QuitTitle: 'Sure?', 0
QuitMessage: 'Source has changed. Save/Compile now?', 0]

Security:
    Mov eax &IDNO

    On D$CodeSource = 0, ret

    ...If B$SecurityWanted = &TRUE
        ..If B$SourceHasChanged = &TRUE
            Call 'MessageBoxA' D$H.MainWindow, QuitMessage, QuitTitle, &MB_YESNOCANCEL
            .If eax = &IDYES
                Call RestoreRealSource
                Call AsmMain | Mov D$OldStackPointer 0
                If B$CompileErrorHappend = &FALSE
                    Mov B$ReadyToRun &TRUE, B$SourceHasChanged &FALSE, eax &IDNO
                Else
                    Call SetPartialEditionFromPos | Mov eax &IDCANCEL
                End_If
            .End_if
        ..End_If
    ...End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[OldBlockInside: ?]  [DollarOnly: &FALSE]

Proc CharMessage:
    Argument @wParam

    Mov eax D@wParam | On eax = &VK_ESCAPE, ExitP

    On B$keys+&VK_CONTROL = &TRUE, jmp L2>>

    .If D$DebugDialogHandle <> 0
        push eax
        Call KillDebugger
            If eax = &IDNO
                Mov eax &FALSE | ExitP
            End_If
        pop eax
    .End_If

    move D$OldBlockInside D$BlockInside | Mov B$BlockInside &FALSE

    If B$WriteCheckerWanted = &TRUE
        cmp eax 32 | je L1>
        cmp eax CR | je L1>
        cmp eax ',' | je L1>
        cmp eax 8 | je L1>
       ; cmp eax ':' | je L1>
        cmp eax Tab | jne L0>
L1:     Call WriteChecker D$CurrentWritingPos, eax
    End_If

L0: cmp eax CR | jne L1>
        If D$CompletionListHandle <> 0
            Call ToCompletionList CR | ExitP
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
        cmp al Tab | jne L1>
            Call IsItBlockIndent
            .If B$SimulatedBlock = &TRUE
                Mov B$BlockInside &FALSE
                Mov esi D$CurrentWritingPos
                If B$esi-1 = LF
                    Call StartOfLine
                Else
                    Call StartOfLine | Call AskForRedrawNow | Call StartOfLine
                End_If
            .End_If
            On B$BlockIndent = &TRUE, jmp P8>>
L1: ;mov D$OldBlockInside &FALSE
    cmp B$Overwrite &TRUE | jne L1>
        Call OverwriteSource | jmp P8>>
L1:     Call InsertSource | jmp P8>>

L2: Call GetCtrlKeyState

    .If eax = 03
        Call ControlC | Mov eax &FALSE
    .Else_If eax = 016
        ;mov B$BlockInside &FALSE
        Call ControlV | Mov eax &TRUE
        Mov B$BlockInside &FALSE
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
    .Else_If eax = 020
        On B$Underline = &TRUE, Call Completion
        Mov eax &TRUE
        Mov B$keys+&VK_CONTROL &FALSE
    .Else
        Mov eax &FALSE
    .End_If

    push eax | Call CheckCtrlKeyState | pop eax | Mov D$OldBlockInside &FALSE | ExitP

P8: Mov D$OldBlockInside &FALSE, eax &TRUE
EndP
____________________________________________________________________________________________

[CtrlKeyState: ?]

; The "and eax 0FFFF" makes it work as well under 95 and 2000

GetCtrlKeyState:
    push eax
        Call 'USER32.GetKeyState' &VK_CONTROL
        and eax 0FFFF | Mov D$CtrlKeyState eax
    pop eax
ret


CheckCtrlKeyState:
    Call 'USER32.GetKeyState' &VK_CONTROL | and eax 0_FFFF
    On eax <> D$CtrlKeyState, Mov B$keys+&VK_CONTROL &FALSE
ret
____________________________________________________________________________________________

[SimulatedBlock: ?]

SimulateBlockForBackIndent:
    push esi
        Mov esi D$CurrentWritingPos
        While B$esi-1 <> LF | dec esi | End_While
        Mov D$BlockStartTextPtr esi

        Mov esi D$CurrentWritingPos
        While B$esi <> CR | inc esi | End_While | dec esi
        Mov D$BlockEndTextPtr esi

        Mov B$OldBlockInside &TRUE, B$SimulatedBlock &TRUE
    pop esi
ret


[BlockIndent: ?]

IsItBlockIndent:
    Mov B$BlockIndent &FALSE

  ; Verify that the Block includes a start of Line (accept non included Labels):
    Mov esi D$BlockStartTextPtr
    While B$esi <> LF
        dec esi
        If B$esi = ':'
            jmp L2>
        Else_If B$esi > ' '
            jmp L9>>
        End_If
    End_While

  ; Verify that the Block is not empty:
L2: Mov esi D$BlockStartTextPtr
    While esi < D$BlockEndTextPtr
        On B$esi > ' ', jmp L2>
        inc esi
    End_While
    jmp L9>>

  ; Verify that the last selected line is complete (does not stop before CRLF):
L2: Mov esi D$BlockEndTextPtr

    While W$esi+1 <> CRLF
        On esi < D$BlockStartTextPtr, jmp L9>>
        dec esi
    EndWhile

  ; OK, Block holds a full Lines content.  ; 'InsertSource'
  ; Insert or retrieve as many Tab as Lines (but preserve Labels):
L2: On B$keys+&VK_SHIFT = &TRUE, jmp RetrieveBlockIndent

L2: Mov esi D$BlockStartTextPtr, B$FirstBlockLine &TRUE

    ..While esi < D$BlockEndTextPtr
      ; Go to Start of Line:
        Mov ebx esi | While B$ebx <> LF | dec ebx | End_While
      ; Go to Start of first member, and save in eax:
        While B$ebx <= ' ' | inc ebx | End_While
        Mov eax ebx
        .While B$ebx > ' '
            inc ebx
            If B$ebx = ':'
                inc ebx
                While B$ebx <= ' '
                    inc ebx | On B$ebx = CR, jmp L8>>
                End_While
          ; New Start of first memeber if first one was a label:
            Mov eax ebx
            End_If
        .End_While
      ; eax > real first member to move. Adjust Block Start if necessary:
        If B$FirstBlockLine = &TRUE
            Mov D$BlockStartTextPtr eax
            Mov B$FirstBlockLine &FALSE
        End_If
        push eax, D$SourceLen
            Call SetCaret eax | dec D$CaretRow | move D$PhysicalCaretRow D$CaretRow
            Mov al Tab | Call InsertSourceOnBlockIndent
        pop ecx, esi
      ; Next Line:
        While B$esi <> CR | inc esi | End_While | inc esi
        Mov eax D$SourceLen | sub eax ecx | add D$BlockEndTextPtr eax
    ..End_While

    Mov eax D$BlockStartTextPtr
    While B$eax = ' ' | inc eax | End_While
    Mov D$BlockStartTextPtr eax

    Call SetCaret D$BlockEndTextPtr | Mov B$RightScroll 0

  ; 'KeyMessage'
  ;  move D$ShiftBlockCol D$CaretRow
  ;  move D$ShiftBlockLine D$CaretLine
  ;  move D$PhysicalCaretRow D$CaretRow
  ;  Mov D$CaretEndOfLine &TRUE
  ;  Call KeyMessage

L8: Mov B$BlockIndent &TRUE, B$BlockInside &TRUE
L9: ret


[FirstBlockLine: ?    NewCaretBlockPos: ?]

RetrieveBlockIndent:
    Mov B$BlockIndent &TRUE, D$NewCaretBlockPos 0
  ; Are there enough free Spaces to be deleted on each Line? If not, abort:
    Mov esi D$BlockStartTextPtr
    ..While esi < D$BlockEndTextPtr
      ; Go to Start of Line:
        Mov ebx esi | While B$ebx <> LF | dec ebx | End_While
      ; Go to Start of first member, and save in eax:
        While B$ebx <= ' ' | inc ebx | End_While
        Mov eax ebx
      ; If Label, jmp over it:
        .While B$ebx > ' '
            inc ebx
            If B$ebx = ':'
                inc ebx
                While B$ebx <= ' '
                    inc ebx | On B$ebx = CR, jmp L8>>
                End_While
              ; New Start of first memeber if first one was a label:
                Mov eax ebx
            End_If
        .End_While

      ; eax > now real first member to move. Are there enough Spaces in front of it?
        push eax
            Mov ecx D$TabIs
L0:         dec eax
            If B$eax <> ' '
                pop eax | jmp L8>>
            End_If
            loop L0<
        pop esi
      ; OK, enough spaces > Next Line:
        While B$esi <> CR | inc esi | End_While | inc esi
    ..End_While

  ; OK, enough Spaces on each Line > Delete:
    Mov esi D$BlockStartTextPtr, B$FirstBlockLine &TRUE
    ..While esi < D$BlockEndTextPtr
      ; Go to Start of Line:
        Mov ebx esi | While B$ebx <> LF | dec ebx | End_While
      ; Go to Start of first member, and save in eax:
        While B$ebx <= ' ' | inc ebx | End_While
        Mov eax ebx
        .While B$ebx > ' '
            inc ebx
            If B$ebx = ':'
                inc ebx
                While B$ebx <= ' '
                    inc ebx | On B$ebx = CR, jmp L8>>
                End_While
              ; New Start of first member if first one was a label:
                Mov eax ebx
            End_If
        .End_While
      ; eax > real first member to move:
        On D$NewCaretBlockPos = 0, Mov D$NewCaretBlockPos eax

        If B$FirstBlockLine = &TRUE
            Mov D$BlockStartTextPtr eax, B$FirstBlockLine &FALSE
        End_If
        push eax, D$SourceLen
            Call SetCaret eax | dec D$CaretRow | move D$PhysicalCaretRow D$CaretRow
            Mov ecx D$TabIs
L0:         push ecx
                Mov al Tab | Call BackSpace | dec D$BlockEndTextPtr
            pop ecx | loop L0<
        pop ecx, esi
      ; Next Line:
        sub esi D$TabIs
        While B$esi <> CR | inc esi | End_While | inc esi
    ..End_While

    Mov eax D$TabIs | sub D$BlockStartTextPtr eax
    Mov eax D$NewCaretBlockPos | sub eax D$TabIs | dec eax | Call SetCaret eax

   ; move D$ShiftBlockCol D$CaretRow
   ; move D$ShiftBlockLine D$CaretLine
   ; move D$PhysicalCaretRow D$CaretRow
   ; Mov D$CaretEndOfLine &FALSE

L8: Mov B$BlockIndent &TRUE, B$BlockInside &TRUE
L9: ret



[ShiftDown: ?    ShiftBlockCol: ?    ShiftBlockLine: ?]

[keys: B$ ? #0100]

[KeyHasModifedSource: ?   KeyHasMovedCaret: ?   SourceHasChanged: ?]

KeyMessage:
    On B$SourceReady = &FALSE, ret

    Mov B$KeyHasModifedSource &FALSE, B$KeyHasMovedCaret &FALSE

    ..If B$BlockInside = &TRUE
        .If B$ShiftBlockInside = &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                move D$CaretRow D$ShiftBlockCol
                move D$CaretLine D$ShiftBlockLine
                move D$PhysicalCaretRow D$CaretRow
                Mov D$CaretEndOfLine &FALSE
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
            Call FullDown | Mov B$BlockInside &FALSE
            Mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                Call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, Mov B$BlockInside &FALSE, B$ShiftBlockInside &FALSE

        .Else_If B$keys+&VK_PGUP = &TRUE
            Call FullUp | Mov B$BlockInside &FALSE
            Mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                Call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, Mov B$BlockInside &FALSE, B$ShiftBlockInside &FALSE

        .Else_If B$keys+&VK_LEFT = &TRUE
            Call StartOfWord | Mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                Call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, Mov B$BlockInside &FALSE, B$ShiftBlockInside &FALSE
            Mov B$keys+&VK_LEFT &FALSE

        .Else_If B$keys+&VK_RIGHT = &TRUE
            Call EndOfWord | Mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                Call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, Mov B$BlockInside &FALSE, B$ShiftBlockInside &FALSE
            Mov B$keys+&VK_RIGHT &FALSE

        .Else_If B$keys+&VK_DOWN = &TRUE
            Call DownOneLine | Mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                Call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, Mov B$BlockInside &FALSE, B$ShiftBlockInside &FALSE
            Mov B$keys+&VK_DOWN &FALSE

        .Else_If B$keys+&VK_UP = &TRUE
            Call UpOneLine | Mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                Call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, Mov B$BlockInside &FALSE, B$ShiftBlockInside &FALSE
            Mov B$keys+&VK_UP &FALSE

        .Else_If B$keys+&VK_DELETE = &TRUE
            On D$DebugDialogHandle <> 0, ret
            Call ControlD | Mov B$keys+&VK_DELETE &FALSE, B$KeyHasModifedSource &TRUE

        .Else_If B$keys+&VK_INSERT = &TRUE
            On D$DebugDialogHandle <> 0, ret
            Call ControlC | Mov B$keys+&VK_INSERT &FALSE, B$KeyHasModifedSource &TRUE

        .Else_If B$keys+&VK_BACK = &TRUE
            On D$DebugDialogHandle <> 0, ret
            Call ControlX | Mov B$keys+&VK_BACK &FALSE, B$KeyHasModifedSource &TRUE
            Mov B$RedrawFlag &TRUE
          ; This 'RedrawFlag' is to kill the next coming WM_CHAR holding
          ; ([Ctrl][Back] sends a char).

        .Else_If B$keys+&VK_F4 = &TRUE
            Call RestoreRealSource
                Call SetCaret D$CurrentWritingPos
                Mov eax 0, ebx D$CaretLine | Call MarginAction
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
                Mov B$BlockInside &FALSE
            End_If
            Mov B$KeyHasMovedCaret &TRUE
            move D$PhysicalCaretRow D$CaretRow

        .Else_If B$keys+&VK_RIGHT = &TRUE
            On B$keys+&VK_CONTROL = &FALSE, Call KeyRight
            If D$ShiftDown <> 0
                Call SetShiftBlock
            Else
                Mov B$BlockInside &FALSE
            End_If
            Mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_UP = &TRUE
            On B$keys+&VK_CONTROL = &FALSE, Call KeyUp
            If D$ShiftDown <> 0
                Call SetShiftBlock
            Else
                Mov B$BlockInside &FALSE
            End_If
            Mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_DOWN = &TRUE
            On B$keys+&VK_CONTROL = &FALSE, Call KeyDown
            If D$ShiftDown <> 0
                Call SetShiftBlock
            Else
                Mov B$BlockInside &FALSE
            End_If
            Mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_PGUP = &TRUE
            On B$keys+&VK_CONTROL = &FALSE, Call OnlyOnePageUp
            If D$ShiftDown <> 0
                Call SetShiftBlock
            Else
                Mov B$BlockInside &FALSE
            End_If
            Mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_PGDN = &TRUE
            On B$keys+&VK_CONTROL = &FALSE, Call OnlyOnePageDown
            If D$ShiftDown <> 0
                Call SetShiftBlock
            Else
                Mov B$BlockInside &FALSE
            End_If
            Mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_HOME = &TRUE
            Call StartOfLine
            If D$ShiftDown <> 0
                Call SetShiftBlock
            Else
                Mov B$BlockInside &FALSE
            End_If
            Mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_END = &TRUE
            Call EndOfLine
            If D$ShiftDown <> 0
                Call SetShiftBlock
            Else
                Mov B$BlockInside &FALSE
            End_If
            Mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_INSERT = &TRUE
            On D$DebugDialogHandle <> 0, ret
            Call ControlV | Mov B$keys+&VK_INSERT &FALSE, B$KeyHasModifedSource &TRUE

        .Else_If B$keys+&VK_DELETE = &TRUE
            On D$DebugDialogHandle <> 0, ret
            Call ControlX | Mov B$keys+&VK_DELETE &FALSE, B$KeyHasModifedSource &TRUE

        .Else_If B$keys+&VK_F4 = &TRUE
            Call RestoreRealSource
                Call SetCaret D$CurrentWritingPos
                Mov eax 0, ebx D$CaretLine | Call MarginAction
            Call SetPartialEditionFromPos
            Mov B$keys+&VK_F4 0

        .Else
            If B$BlockInside = &FALSE
                move D$ShiftDown D$CurrentWritingPos | ret
           ; Else
           ;     Mov B$BlockInside &FALSE | Call AskForRedraw | ret
            End_If

        .End_If

    ...Else
        ..If eax = &VK_PGDN
            Call OnlyOnePageDown | Mov B$BlockInside &FALSE

        ..Else_If eax = &VK_PGUP
            Call OnlyOnePageUp | Mov B$BlockInside &FALSE

        ..Else_If eax = &VK_DOWN
            If D$CompletionListHandle <> 0
                Mov B$Keys+eax 0
                Call ToCompletionList &VK_DOWN
              ; To save from downward 'KeyHasMovedCaret' Flag modification:
                ret
            Else
                Call KeyDown | Mov B$BlockInside &FALSE
            End_If

        ..Else_If eax = &VK_UP
            If D$CompletionListHandle <> 0
                Mov B$Keys+eax 0
                Call ToCompletionList &VK_UP
              ; To save from downward 'KeyHasMovedCaret' Flag modification:
                ret
            Else
                Call KeyUp | Mov B$BlockInside &FALSE
            End_If

        ..Else_If eax = &VK_LEFT
            Call KeyLeft | Mov B$BlockInside &FALSE

        ..Else_If eax = &VK_RIGHT
            Call KeyRight | Mov B$BlockInside &FALSE

        ..Else_If eax = &VK_INSERT
            Call KeyInsert | Mov B$BlockInside &FALSE

        ..Else_If eax = &VK_DELETE
            .If D$DebugDialogHandle <> 0
                Mov B$Keys+eax 0
                Call KillDebugger | On eax = &IDNO, ret
            .End_If
            Call KeyDelete | Mov B$BlockInside &FALSE, B$KeyHasModifedSource &TRUE

        ..Else_If eax = &VK_END
            Call EndOfLine | Mov B$BlockInside &FALSE

        ..Else_If eax = &VK_HOME
            Call StartOfLine | Mov B$BlockInside &FALSE

        ..Else_If eax = &VK_ESCAPE
            If D$CompletionListHandle <> 0
                Call 'USER32.SendMessageA' D$CompletionListHandle, &WM_COMMAND, &IDCANCEL, 0
                Mov B$keys+&VK_ESCAPE &FALSE | ret
            End_If

        ..Else_If eax = &VK_F1
            Call RosAsmHelp | Mov B$keys+&VK_F1 &FALSE

        ..Else_If eax = &VK_F2
            Call F2Help | Mov B$keys+&VK_F2 &FALSE

        ..Else_If eax = &VK_F3
            Call RestoreRealSource | Call StringSearch | Call SetPartialEditionFromPos
            Mov B$keys+&VK_F3 0

        ..Else_If eax = &VK_F4  ; BpMenu / SetBreakPoint / DeleteBreakpoint
            Call RestoreRealSource
                Call SetCaret D$CurrentWritingPos
                Mov eax 0, ebx D$CaretLine | Call MarginAction
            Call SetPartialEditionFromPos
            Mov B$keys+&VK_F4 0

        ..Else_If eax = &VK_F5
            Mov B$keys+&VK_F5 &FALSE
            Mov D$ShowStats &TRUE
            Call Compile

        ..Else_If eax = &VK_F6
            Mov B$keys+&VK_F6 &FALSE

            If D$DebugDialogHandle <> 0
                ret
            Else
                Call Run
            End_If

        ..Else_If eax = &VK_F8
            If D$DebugDialogHandle <> 0
                Mov B$keys+&VK_F2 &FALSE
                Call KillDebugger | On eax = &IDNO, ret
            End_If
            Call DrawOneLine

        ..Else_If eax = &VK_F9
            Mov B$Keys+eax 0
            ;mov eax D$BreakPointsTables | int3
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
       ; pop eax | Mov B$Keys+eax 0
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
   ; Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Profile, &MF_GRAYED

    .If B$SourceReady = &FALSE
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Tree, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Paste_at_Pos, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Change_Compile_Name, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace_Source_Only, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Source_Only, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Output, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Print, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Find, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Undo, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Redo, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Copy, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Paste, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Cut, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Compile, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Run, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Import, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Export, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_GUIDs, &MF_GRAYED

    .Else
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Tree, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Paste_at_Pos, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Change_Compile_Name, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace_Source_Only, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Source_Only, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Output, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Print, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Find, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Undo, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Redo, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Copy, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Paste, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Cut, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Compile, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Run, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Import, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Export, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_GUIDs, &MF_ENABLED

    .End_If

    If D$SavingExtension = '.SYS'
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Run, &MF_GRAYED
    Else_If B$SourceReady = &TRUE
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Run, &MF_ENABLED
    End_If

    If D$IconList = 0
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Icon, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Icon_IDs, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Icon, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Icon, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Icon_IDs, &MF_GRAYED ; &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Icon, &MF_GRAYED ;&MF_ENABLED
    End_If

    If D$BitMapList = 0
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_BitMap, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_BitMaps_IDs, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_BitMap, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_BitMap, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_BitMaps_IDs, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_BitMap, &MF_GRAYED ;&MF_ENABLED
    End_If

    If D$CursorList = 0
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Cursor, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Cursors_IDs, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Cursor, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Cursor, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Cursors_IDs, &MF_GRAYED ;&MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Cursor, &MF_GRAYED ;&MF_ENABLED
    End_If

    If D$WaveList = 0
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Wave, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Waves_IDs, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Wave, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Wave, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Waves_IDs, &MF_GRAYED ;&MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Wave, &MF_GRAYED ;&MF_ENABLED
    End_If

    If D$AviList = 0
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Avi, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Avi_IDs, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Avi, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Avi, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Avi_IDs, &MF_GRAYED ;&MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Avi, &MF_GRAYED ;&MF_ENABLED
    End_If

    If D$RCDataList = 0
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_RC, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_RCs_IDs, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_RC, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_RC, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_RCs_IDs, &MF_GRAYED ;&MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_RC, &MF_ENABLED
    End_If

    If D$DialogList = 0
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Load_from_Resources, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Resources_Dialog, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_to_Binary_File, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace_from_Binary_File, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Load_from_Resources, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Resources_Dialog, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_to_Binary_File, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace_from_Binary_File, &MF_ENABLED
    End_If

    If B$SourceReady = &FALSE
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Load_from_Binary_File, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Load_from_Binary_File, &MF_ENABLED
    End_If

    If D$MenuList = 0
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Existing_Menu, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_a_Menu, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_to_Binary_Menu_File, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Load_Binary_Menu_File, &MF_GRAYED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace_from_Binary_Menu_File, &MF_GRAYED
    Else
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Existing_Menu, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_a_Menu, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_to_Binary_Menu_File, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Load_Binary_Menu_File, &MF_ENABLED
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace_from_Binary_Menu_File, &MF_ENABLED
    End_If

    Call 'USER32.DrawMenuBar' D$H.MainWindow
ret


[VisualTutsFindHandle: ?    VisualTutsMenuHandle: ?    VisualTutMenuID: ?]
[VisualTutsItem: 'Visual Tuts', 0]
[VisualTutPath: B$ ? #&MAXPATH]
[WizardPath: B$ ? #&MAXPATH]


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


[PreviousIVT: ?] [IVTFilesNames1 Trash1][IVTFilesNames2 Trash2]

EnableVisualTutsMenu:
  ; Enable the 'Visual Tuts' Item and create the Pop-Up if some Visual Tuts are there:
    Mov esi EquatesName, edi VisualTutPath, ecx (&MAXPATH / 4) | rep movsd

    Mov esi VisualTutPath | While B$esi <> 0 | inc esi | End_While
    While B$esi <> '\' | dec esi | End_While
    Mov D$esi+1 'IVT*', D$esi+5 '.exe', B$esi+9 0

    Call 'KERNEL32.FindFirstFileA' VisualTutPath, FindFile

    .If eax = &INVALID_HANDLE_VALUE
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Visual_Tuts, &MF_GRAYED

    .Else
        Mov D$VisualTutsFindHandle eax

        Call 'USER32.CreatePopupMenu' | Mov D$VisualTutsMenuHandle eax

        Mov edi IVTFilesNames1, ecx 0

L1:     Mov esi FindFile.cFileName | inc ecx

        While B$esi <> 0 | movsb | End_While | Mov B$edi 0 | inc edi

        push ecx
            Call 'KERNEL32.FindNextFileA' D$VisualTutsFindHandle, FindFile
        pop ecx

        On eax = &TRUE, jmp L1<

        Call zStringsSort IVTFilesNames1, IVTFilesNames2, ecx
        ____________________________________________________

        Mov B$PreviousIVT '0', D$VisualTutMenuID 5000

        Call 'USER32.CreatePopupMenu' | Mov D$VisualTutsMenuHandle eax

        Mov esi IVTFilesNames2

L1:     add esi 6

        push esi
            Mov al B$esi-3
            If al <> B$PreviousIVT
                Mov B$PreviousIVT al
                Call 'USER32.AppendMenuA' D$VisualTutsMenuHandle, &MF_SEPARATOR, 0, 0
            End_If

            Call 'USER32.AppendMenuA' D$VisualTutsMenuHandle, &MF_ENABLED__&MF_STRING,
                                      D$VisualTutMenuID, esi
        pop esi

        While B$esi <> 0 | inc esi | End_While | inc esi

        If B$esi <> 0
            inc D$VisualTutMenuID | jmp L1<
        End_If

        Call 'USER32.InsertMenuA' D$MenuHandle, M00_Visual_Tuts,
                                  &MF_BYCOMMAND__&MF_POPUP__&MF_STRING,
                                  D$VisualTutsMenuHandle, VisualTutsItem

        Call 'USER32.DeleteMenu' D$MenuHandle, M00_Visual_Tuts, &MF_BYCOMMAND

        Call 'KERNEL32.FindClose' D$VisualTutsFindHandle
    .End_If
ret


VisualTuts:
    push eax
        .If D$DebugDialogHandle <> 0
            Call KillDebugger
            If eax = &IDNO
                pop eax | ret
            End_If
        .End_If

        ...If B$SourceReady = &TRUE
            ..If B$ReadyToRun = &FALSE
                .If B$SecurityWanted = &TRUE
                    Call 'USER32.MessageBoxA' D$H.MainWindow, {'Close the Actual File ?', 0},
                                              {'Visual Tutorial', 0}, &MB_YESNO
                    If eax = &IDNO
                        pop eax | ret
                    End_If
                .End_If
            ..End_If
        ...End_If
    pop eax

    Mov esi VisualTutPath | While D$esi <> '\IVT' | inc esi | End_While
    add esi 4 | Mov D$esi '???' | add esi 3
    Call 'USER32.GetMenuStringA' D$MenuHandle, eax, esi, 100, &MF_BYCOMMAND

    Call 'KERNEL32.FindFirstFileA' VisualTutPath, FindFile

    .If eax <> &INVALID_HANDLE_VALUE
        Mov D$VisualTutsFindHandle eax
        Call 'KERNEL32.FindClose' D$VisualTutsFindHandle

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
    push eax
        If eax = &INVALID_HANDLE_VALUE
            Mov eax &MF_GRAYED
        Else
            Mov eax &MF_ENABLED
        End_If

        Call 'USER32.EnableMenuItem' D$MenuHandle, D@Item, eax
    pop eax
    Call 'KERNEL32.FindClose' eax
EndP
____________________________________________________________________________________________

[WizardsFindHandle: ?    WizardsMenuHandle: ?    WizardMenuID: ?]
[PreviousWZRD: ?]

[WizardsItem: 'Wizards', 0]

EnableWizardsMenu:
    Call ClearTrashTables

  ; Enable the 'Visual Tuts' Item and create the Pop-Up if some Visual Tuts are there:
    Mov esi EquatesName, edi WizardPath, ecx (&MAXPATH / 4) | rep movsd

    Mov esi WizardPath | While B$esi <> 0 | inc esi | End_While
    While B$esi <> '\' | dec esi | End_While

    Mov D$esi+1 'WZRD', D$esi+5 '*.*'

    Call 'KERNEL32.FindFirstFileA' WizardPath, FindFile

    .If eax = &INVALID_HANDLE_VALUE
        Call 'USER32.EnableMenuItem' D$MenuHandle, M00_Wizards, &MF_GRAYED

    .Else
        Mov D$WizardsFindHandle eax

        Call 'USER32.CreatePopupMenu' | Mov D$WizardsMenuHandle eax

        Mov edi Trash1, ecx 0

L1:     Mov esi FindFile.cFileName | inc ecx

        While B$esi <> 0 | movsb | End_While | Mov B$edi 0 | inc edi

        push ecx
            Call 'KERNEL32.FindNextFileA' D$WizardsFindHandle, FindFile
        pop ecx

        On eax = &TRUE, jmp L1<

        Call zStringsSort Trash1, Trash2, ecx

        ____________________________________________________

        Mov B$PreviousWZRD '0', D$WizardMenuID 6000

        Call 'USER32.CreatePopupMenu' | Mov D$WizardsMenuHandle eax

        Mov esi Trash2

L1:     add esi 4

        push esi
           ; Mov al B$esi-3
           ; If al <> B$PreviousWZRD
           ;     Mov B$PreviousWZRD al
           ;     Call 'USER32.AppendMenuA' D$VisualTutsMenuHandle, &MF_SEPARATOR, 0, 0
           ; End_If

            Call 'USER32.AppendMenuA' D$WizardsMenuHandle, &MF_ENABLED__&MF_STRING,
                                      D$WizardMenuID, esi
        pop esi

        While B$esi <> 0 | inc esi | End_While | inc esi

        If B$esi <> 0
            inc D$VisualTutMenuID | jmp L1<
        End_If

        Call 'USER32.InsertMenuA' D$MenuHandle, M00_Wizards,
                                  &MF_BYCOMMAND__&MF_POPUP__&MF_STRING,
                                  D$WizardsMenuHandle, WizardsItem

        Call 'USER32.DeleteMenu' D$MenuHandle, M00_Wizards, &MF_BYCOMMAND

        Call 'KERNEL32.FindClose' D$WizardsFindHandle
    .End_If
ret
____________________________________________________________________________________________

[ClipMenuID: 7000    NumberOfClipFiles: 0]

[FindClipFilesHandle: ?    ClipFilePopUpMenuHandle: ?
 ClipFilesPath: B$ ? #&MAXPATH]

[ClipFilesItem: 'Clip Files', 0]

; Room for storing 20 Clip Files Names of 20 Chars each:

[ClipMenuStrings: ? #100]

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
            Mov D$FindClipFilesHandle eax

L1:         If D$ClipFilePopUpMenuHandle = 0
                Call 'USER32.CreatePopupMenu' | Mov D$ClipFilePopUpMenuHandle eax
            End_If

            inc D$NumberOfClipFiles

            Call 'USER32.AppendMenuA' D$ClipFilePopUpMenuHandle,
                                      &MF_ENABLED__&MF_STRING,
                                      D$ClipMenuID, FindFile.cFileName

            Call StoreClipMenuStrings FindFile.cFileName

            inc D$ClipMenuID

            Call 'KERNEL32.FindNextFileA' D$FindClipFilesHandle, FindFile
            On eax = &TRUE, jmp L1<<

            Call 'KERNEL32.FindClose' D$FindClipFilesHandle

            .If D$ClipFilePopUpMenuHandle <> 0
                If D$NumberOfClipFiles > 1
                    Call 'USER32.InsertMenuA' D$MenuHandle, M00_ClipFile,
                                              &MF_BYCOMMAND__&MF_POPUP__&MF_STRING,
                                              D$ClipFilePopUpMenuHandle, ClipFilesItem

                    Call 'USER32.DeleteMenu' D$MenuHandle, M00_ClipFile, &MF_BYCOMMAND
                End_If
            .End_If

        ..End_If

    ...End_If
L9: ret
____________________________________________________________________________________________


SetShiftBlock:
    Mov eax D$CaretRow, ebx D$CaretLine
    push eax, ebx

        Call SearchTxtPtr               ; >>> eax = new Pos.

        .If B$BlockInside = &FALSE
            If eax > D$ShiftDown
                dec eax
            End_If

        .Else
            If eax = D$ShiftDown
                Mov B$BlockInside &FALSE | jmp L9>
            Else_If eax > D$ShiftDown
                dec eax
            End_If

        .End_If

        If eax < D$ShiftDown
            move D$BlockStartTextPtr eax, D$BlockEndTextPtr D$ShiftDown
            dec D$BlockEndTextPtr
        Else
            move D$BlockStartTextPtr D$ShiftDown, D$BlockEndTextPtr eax
        End_If

        Mov B$BlockInside &TRUE, B$ShiftBlockInside &TRUE

L9: pop D$ShiftBlockLine, D$ShiftBlockCol
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

SplashScreen:
    Call 'USER32.BeginPaint' D$EditWindowHandle, PAINTSTRUCT | Mov D$hdc eax

    Call 'User32.GetClientRect' D$EditWindowHandle, RECT

    shr D$RECTright 1 | shr D$RECTbottom 1
    sub D$RECTright 75 | sub D$RECTbottom 75

    Call 'USER32.LoadBitmapA' D$hInstance, 5 | Mov D$BitMapHandle eax

    Call 'GDI32.CreateCompatibleDC' D$hdc | Mov D$hMemDC eax

    Call 'GDI32.SelectObject' eax, D$BitMapHandle

    Call 'GDI32.BitBlt' D$hdc, D$RECTright, D$RECTbottom, 150, 150,
                        D$hMemDC, 0, 0, &SRCCOPY

    Call 'GDI32.DeleteDC' D$hMemDC

    Call 'GDI32.DeleteObject' D$BitMapHandle

    Call 'USER32.EndPaint' D$EditWindowHandle, PAINTSTRUCT
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
  BackUps.
;;

____________________________________________________________________________________________
____________________________________________________________________________________________






