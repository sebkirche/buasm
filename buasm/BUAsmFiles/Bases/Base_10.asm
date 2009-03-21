____________________________________________________________________________________________

;;
  Used Macros Strings Variables:
  
    &1, &2, &3              Proc
    &9, &10 to &19          For
 
  Used Macros Counters Variables:
 
    &&0, &&1                If
    &&2, &&3                While
    &&4, &&5                Do
    &&6, &&7                For
    
  Local Labels Attributions:
  
    O1                      On
    P9                      Proc
    I0,... I9, J0,... J9    If
    W0,... W9               While
    D0,... D9               Do
    F0,... F9               For
;;
____________________________________________________________________________________________

[= e   < b    > a    <s l    >s g    =< be    <= be    => ae    >= ae    <> ne]
____________________________________________________________________________________________

; Multi push, pop, mov, move, inc, and dec  Macros:

[push | push #1 | #+1]
[pop | pop #1 | #+1]
[mov | mov #1 #2 | #+2]
[move | push #2 | pop #1 | #+2]
[inc | inc #1 | #+1]
[dec | dec #1 | #+1]
____________________________________________________________________________________________

[Exchange | push #1 | push #2 | pop #1 | pop #2 | #+2]
____________________________________________________________________________________________

[On | cmp #1 #3 | jn#2 O1> | #4>L | O1:]
____________________________________________________________________________________________

[call | #If #1=str
            ApiPush #L>2
        #Else
            push #L>2
        #End_If
        call #1]

[ApiPush | #If #1=str
                push {#1, 0}
           #Else
                push #1
           #End_If
           #+1]
____________________________________________________________________________________________

; C calling convention:

[ccall
    push #L>2 | call #1
    #If #N>1
        add esp ((#N-1)*4)
    #EndIf]
____________________________________________________________________________________________

[.If
    #If &&0<>0
        #If &&0<> '0'
            #Error 'Unpaired If'
        #End_If
    #End_If
    &&0= '0' | &&1=Pos
    AndCmp I&&0>>, #1>L]

[.End_If | I&&0: | J&&0:
    #If &&0<> '0'
        #ErrorPos &&1 'Unpaired If'
    #End_If
    &&0=0]

[If
    #If &&0=0
        &&0= '0'
    #Else
        &&0=&&0+1
    #End_If

    AndCmp I&&0>>, #1>L]

[Else_If | jmp J&&0>> | I&&0:
    AndCmp I&&0>>, #1>L]

[Else | jmp J&&0>> | I&&0: ]

[End_If | I&&0: | J&&0:
    #If &&0= '0'
        &&0=0
    #Else
        &&0=&&0-1
    #End_If]

[AndCmp | cmp #2 #4 | jn#3 #F | #+3]
____________________________________________________________________________________________

[.While
    #If &&2<>0
        #If &&2<> '0'
                #Error 'Unpaired While'
        #End_If
    #End_If

    &&2= '0' | &&3=Pos

    W&&2: cmp #1 #3 | jn#2 W&&2>>]

[.End_While
    #If &&2<> '0'
        #ErrorPos  &&3 'Unpaired While'
    #End_If
    jmp W&&2<< | w&&2:
    &&2=0]

[While
    #If &&2=0
        &&2= '0'
    #Else
        &&2=&&2+1
    #End_If
    W&&2: cmp #1 #3 | jn#2 W&&2>>]

[End_While | jmp W&&2<< | w&&2:
             #If &&2= '0'
                &&2=0
             #Else
                &&2=&&2-1
             #End_If]
____________________________________________________________________________________________

[.Do
    #If &&4<>0
        #If &&4<> '0'
                #Error 'Unpaired Do'
        #End_If
    #End_If

    &&4= '0' | &&5=Pos

    D&&4:]

[.Loop_Until
    #If &&4<> '0'
        #ErrorPos  &&5 'Unpaired Do Until'
    #End_If

    cmp #1 #3 | jn#2 D&&4<<
    &&4=0]

[.Loop_While
    #If &&4<> '0'
        #ErrorPos  &&5 'Unpaired Do While'
    #End_If

    cmp #1 #3 | j#2 D&&4<<
    &&4=0]

[Do
    #If &&4= 0
        &&4= '0'
    #Else
        &&4=&&4+1
    #End_If

    D&&4: ]

[Loop_Until | cmp #1 #3 | jn#2 D&&4<<  | D&&4:
 #If &&4= '0'
    &&4=0
 #Else
    &&4=&&4-1
 #End_If]

[Loop_While | cmp #1 #3 | j#2 D&&4<<  | D&&4:
 #If &&4= '0'
    &&4=0
 #Else
    &&4=&&4-1
 #End_If]
____________________________________________________________________________________________

[.For
    #If &&6<>0
        #If &&6<> '0'
            #Error 'Unpaired For'
        #End_If
    #End_If
    &&6= '0' | &&7=Pos

    #If #3=imm
        mov #1 (#3-1)
    #Else
        mov #1 #3
        dec #1
    #EndIf
 F&&6:
    inc #1 | cmp #1 #5 | ja F&&6>> ]

[.Next | jmp F&&6<<
 F&&6:
    #If &&6<> '0'
        #ErrorPos &&7 'Unpaired For'
    #End_If
    &&6=0]

[For
    #If &&6=0
        &&6= '0'
    #Else
        &&6=&&6+1
    #EndIf

    #If #3=imm
        mov #1 (#3-1)
    #Else
        mov #1 #3
        dec #1
    #EndIf
 F&&6:
    inc #1 | cmp #1 #5 | ja F&&6>> ]


[Next | jmp F&&6<< | F&&6: &&6=&&6-1]

[Break | jmp F&&6>>]
[Continue | jmp F&&6<<]
[ExitF | jmp F0>>]
____________________________________________________________________________________________

;;
  &1 = Size of arguments 
  &2 = Size of local data (local+structures) 
  &3 = preserved regs 
;;

[Proc | &1=0 | &2=0 | &3= | #1 | push ebp | mov ebp esp]

[Arguments | {#1 ebp+((#x*4)+4)} | #+1 | &1=(#N*4)]

[Local | {#1 ebp-(#x*4)} | #+1 | &2=(#N*4) | sub esp &2]

[GetMember | {#3 ebp-(#F-#2)} | #+2]

[Structure | {#1 ebp-(&2+#2+4)} | sub esp #2 | push esp | GetMember &2+#2 #L>3 | &2=&2+#2+4]

[Uses | push #1>L | &3=pop #L>1]

[Return | #If #N=1 | mov eax #1 | #EndIf | jmp P9>>]

[ExitP | jmp P9>>]

[EndP | P9: | &3 | mov esp ebp | pop ebp | ret &1]

[.EndP | P9: | &3 | mov esp ebp | pop ebp | ret &1]

___________________________________________________________________________________________

; Data:

; For "GetMessage":

[FirstMessage: 0 #7]

; Window Class Structure:

[WindowClass:
 style: 3
 lpfnWndProc: MainWindowProc
 cbClsExtra: 0
 cbWndExtra: 0
 hInstance: 0
 hIcon: 0
 hCursor: 0
 hbrBackground: 6
 lpszMenuName: 0
 lpszClassName: WindowClassName]

[WindowHandle: 0   MenuHandle: 0]

[WindowClassName: B$ 'Application' 0    WindowCaption: 'Base App' 0]

[WindowX: 50  WindowY: 50  WindowW: 650  WindowH: 450]
___________________________________________________________________________________________

Main:
    call 'Kernel32.GetModuleHandleA' 0 | mov D$hInstance eax

    call 'User32.LoadIconA' D$hInstance, 1 | mov D$hIcon eax

    call 'User32.LoadCursorA' 0, &IDC_ARROW | mov D$hCursor eax

    call 'User32.RegisterClassA' WindowClass

    call 'User32.LoadMenuA' D$hInstance, M00_Menu | mov D$MenuHandle eax

    call 'User32.CreateWindowExA' &WS_EX_CLIENTEDGE, WindowClassName, WindowCaption,
                                 &WS_OVERLAPPEDWINDOW__&WS_VISIBLE,
                                 D$WindowX, D$WindowY, D$WindowW, D$WindowH, 0,
                                 D$MenuHandle, D$hInstance, 0
      mov D$WindowHandle eax

    call 'User32.ShowWindow' D$WindowHandle, &SW_SHOW
    call 'User32.UpdateWindow' D$WindowHandle


    jmp L1>

    While eax <> 0
        call 'User32.TranslateMessage' FirstMessage
        call 'User32.DispatchMessageA' FirstMessage
L1:     call 'USER32.GetMessageA' FirstMessage 0 0 0
    End_While

    call 'Kernel32.ExitProcess' &NULL

___________________________________________________________________________________________
; These menu equates are given by the menu editor ([ClipBoard]):

[M00_Menu  2000                  M00_New  2001                   M00_Open  2002
 M00_Save  2003                  M00_Save_As  2004               M00_Exit  2005
 M00_Undo  2006                  M00_Cut  2007                   M00_Copy  2008
 M00_Paste  2009                 M00_Delete  2010                M00_Select_All  2011
 M00_About  2012]
___________________________________________________________________________________________

Proc MainWindowProc:
    Arguments @Addressee @Message @wParam @lParam

        pushad

            .If D@Message = &WM_CLOSE
                call 'USER32.DestroyWindow' D@Addressee

            Else_If D@Message = &WM_DESTROY
                call 'User32.PostQuitMessage' 0

            Else_If D@Message = &WM_COMMAND
                If D@wParam = M00_Exit
                    call 'User32.SendMessageA' D@Addressee, &WM_CLOSE, 0, 0

           ;    Else_If  D@wParam = M00_...
                   ; ...

           ;    Else_If  D@wParam = M00_About
                   ; ...

                Else
                    call 'USER32.MessageBoxA' &NULL,
                                              "
   This file is not a Demo. It is a 'StartUp'
   Base You can use to develop your own work.   ",
                                              'Application Base',
                                              &MB_SYSTEMMODAL__&MB_OK

                End_If

            Else
                popad
                call 'User32.DefWindowProcA' D@Addressee, D@Message, D@wParam, D@lParam
                ExitP

            .End_If

    popad | mov eax &FALSE
EndP
______________________________________________________________________________________
; little message routines for values tests (dWords only / text pointer) to be called with:
; > Hexprint D$esi / showme esi+5, for example:

[InfoTitle: 'Application Base', 0]

[HexprintString: B$ '        h' 0
 MessageTitle:      'HihoHiho' 0]

Proc HexPrnt:
    Arguments @N

    pushad
        mov ebx D@N, ecx 8, edi HexPrintString | add edi 7
        std
L1:
                mov al bl | and al 0F | add al '0'
                On al > '9', add al 7
                stosb | shr ebx 4
            Loop L1<
        cld
        call 'USER32.MessageBoxA'  0, HexPrintString, MessageTitle, &MB_OK__&MB_SYSTEMMODAL
    popad
EndP

[Hexprint | call Hexprnt #1 | #+1]


Proc ShowYou:
    Arguments @Pointer

    pushad
        call 'USER32.MessageBoxA' &NULL, D@Pointer, MessageTitle, &MB_SYSTEMMODAL__&MB_OK
    popad
EndP

[Showme | push eax | lea eax D$#1 | call ShowYou eax | pop eax]

_______________________________________________________________________________________






