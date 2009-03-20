TITLE Icon            ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________
;;
                                      icon editor
 
 (To be entirely re-written by who wants to).

 _______________________________________________________________________________________
 _______________________________________________________________________________________
;;

; Dialog Box Template in memory.

[ID_Inew 301  ID_Ipeek 302  ID_Ipoke 303  ID_IfromIco 304  ID_ItoIco 305
 ID_Icancel 306  ID_Ikeep 307  ID_TrackBar 320]

 _______________________________
;;
 The icon editor is set upon a dialog box created at run time (when created
 no dialog resource editor in RosAsm). The only one difficulty i encounted is
 that 'title's must be dWords aligned (doc doesn't tell).

 The predefined classes values are:
 080=button / 081=Edit / 082=Static / 083=ListBox / 084=ScrollBar / 085=ComboBox
;;

[IconDialogData:

; Dialog Box template:

  D$ &DS_SETFONT+&DS_CENTER+&WS_CAPTION+&WS_VISIBLE+&DS_MODALFRAME+&DS_3DLOOK+&WS_SYSMENU  ; style
     0                                                                   ; ext. Style
  U$ 8  0  0  220  200     ; control-number, x, y, width, hight
     0                       ; no menu
     0                       ; class 0 > default
    'Icon Editor' 0 0        ; title
     10 'Helv' 0

; controls (7 button):

  D$ &WS_CHILD&WS_VISIBLE  0     ; style / ext.style
  U$ 10 15  40  8            ; x y w h
     ID_Inew                 ; ID
     0FFFF                   ; Predefined class
     080                     ; 080=button
    '> New >' 0                   ; button title
     0                       ; no creation data

  D$ &WS_CHILD&WS_VISIBLE  0     ; style / ext.style
  U$ 10 30  40  8            ; x y w h
     ID_Ipeek                ; ID
     0FFFF                   ; Predefined class
     080                     ; button
    '> .exe >' 0 0                  ; button title
     0                       ; no creation data

  D$ &WS_CHILD&WS_VISIBLE  0     ; style / ext.style
  U$ 10 45  40  8            ; x y w h
     ID_Ipoke                ; ID
     0FFFF                   ; Predefined class
     080                     ; button
    '< .exe <' 0  0                 ; button title
     0                       ; no creation data

  D$ &WS_CHILD&WS_VISIBLE  0     ; style / ext.style
  U$ 10 60  40  8            ; x y w h
     ID_IfromIco             ; ID
     0FFFF                   ; Predefined class
     080                     ; button
    '> .ico >' 0  0          ; button title
     0                       ; no creation data

  D$ &WS_CHILD&WS_VISIBLE  0     ; style / ext.style
  U$ 10 75  40  8            ; x y w h
     ID_ItoIco               ; ID
     0FFFF                   ; Predefined class
     080                     ; button
    '< .ico <' 0  0          ; button title
     0                       ; no creation data

  D$ &WS_CHILD&WS_VISIBLE  0     ; style / ext.style
  U$ 10 90  40  8           ; x y w h
     &IDCANCEL               ; ID
     0FFFF                   ; Predefined class
     080                     ; button
    '= Cancel =' 0  0        ; button title
     0                       ; no creation data

  D$ &WS_CHILD&WS_VISIBLE  0     ; style / ext.style
  U$ 10 105 40  8           ; x y w h
     ID_Ikeep                ; ID
     0FFFF                   ; Predefined class
     080                     ; button
    '* Keep *' 0  0          ; button title
     0]                      ; no creation data

 [AddToIcon: D$ &WS_CHILD&WS_VISIBLE  0     ; style / ext.style
  U$ 10 120 40  8           ; x y w h
     ID_Help                ; ID
     0FFFF                   ; Predefined class
     080                     ; button
    '  Help  ' 0  0          ; button title
     0]                      ; no creation data

 _______________________________

; Icon Edition data

[iIcoFileHeader:

 ; poor little thing for saving a poor little icon alone in a poor little ico file:

 W$    0   ; reserved
       1   ; Type 1
       1   ; entries number
B$   020   ; width
     020   ; hight
     010   ; color count
       0   ; reserved
W$     0   ; planes
       0   ; bits count
D$  02E8   ; size
     016   ; offset from .ico start


; All these data are not the table used to build a PE. Just a temporary table for
; icon edition. In case user compiles a source without having drawn any icon, this
; one is copyied to &TRUE buffer as default. Once user have compiled an application
; reloading it fills this table with previously defined icon.

iIcon:
iIconHeader:
B$ 028,0,0,0     ; size
   020,0,0,0     ; width
   040,0,0,0     ; height (maybe 40h because of the two masks)
   01,0          ; planes
   04,0          ; bit count
   0,0,0,0       ; compression 0
   080,02,0,0    ; 0280 > size of icon data
   0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0   ; (dummy)


iIconPalette:
;Blue,green,red,0    (16 color palette -values seams to be fixed ones-)

B$ 0,  0,  0,  0 ; color 0
   0,  0,080,  0 ;       1
   0,080,  0,  0 ;       2
   0,080,080,  0 ;       3
 080,  0,  0,  0 ;       4
 080,  0,080,  0 ;       5
 080,080,  0,  0 ;       6
 0C0,0C0,0C0,  0 ;       7
 080,080,080,  0 ;       8
   0,  0,0FF,  0 ;       9    rouge
   0,0FF,  0,  0 ;       A
   0,0FF,0FF,  0 ;       B
 0FF,  0,  0,  0 ;       C    bleu
 0FF,  0,0FF,  0 ;       D
 0FF,0FF,  0,  0 ;       E
 0FF,0FF,0FF,  0 ;       F


iIconXorMask:
; XOR color mask: (32*16 octets > 2 pixels / byte > 32*32 pixels)

B$    0    0
B$    0    0    0    0    0    0    0    0    0    0    0   0C    0    0   09  099
B$  099  099  099  099  099  099  099  099  099  099  099   0C  0C0    0   09  099
B$  099  099  099  099  099  099  099  099  099  099  099   0C  0CC    0    0    0
B$   09  099  099  099  099  099  099  099  099  099  099   0C  0CC  0C0   03  03B
B$   09  099  099  099  099  099  099  099  099  099  099   0C  0CC  0C0    0  033
B$   09  099  090    0    0    0    0  099  099  099  099   0C  0CC  0C0    0   03
B$   09  099  090  0B$  0BB  0BB  0B0  099  099  099  099   0C  0CC  0C0    0    0
B$   09  099  090  0DD  0BB  0BB  0B0  099  099  099  099   0C  0CC  0C0    0    0
B$   09  099  090  0DD  0B$  0BB  0B0  099  099  099  099   0C  0CC  0C0    0    0
B$   09  099  090  0DD  0DD  033  030  099  099  099  099   0C  0CC  0C0    0    0
B$    0    0    0  0DD  0DD    0    0  099  099  099  099   0C  0CC  0C0    0    0
B$   03  03B  0BB  0BD  0DD    0    0  099  099  099  099   0C  0CC  0C0    0    0
B$    0  033  0BB  0BB  0DD    0    0  099  099  099  099   0C  0CC  0C0    0    0
B$  0C0   03  03B  0BB  0BD    0    0    0    0    0    0   0C  0CC  0C0   0E  0E0
B$  0CC    0  033  033  033    0    0  033  0BB  0BB  0BB  0BB  0CC  0C0   0E  0E0
B$  0CC  0C0    0    0    0    0    0   03  03B  0BB  0BB  0BB  0BC  0C0   0E  0E0
B$  0CC  0CC    0    0    0    0   0C    0  033  0BB  0BB  0BB  0BB  0C0   0E  0E0
B$  0CC  0CC    0   0E  0EE  0EE   0C  0C0   03  033  033  033  033  030   0E  0E0
B$  0CC  0CC    0   0E  0EE  0EE   0C  0CC    0    0    0    0    0    0   0E  0E0
B$  0CC  0CC    0   0E  0EE  0EE   0C  0CC  0C0    0    0    0    0    0   0E  0E0
B$    0    0    0   0E  0EE  0EE   0C  0C0    0    0    0    0    0    0   0E  0EE
B$  0EE  0EE  0EE  0EE  0EE  0EE   0C  0C0  0AA  0AA  0AA  0AA   0C    0   0E  0EE
B$  0EE  0EE  0EE  0EE  0EE  0EE   0C  0C0  0AA  0AA  0AA  0AA   0C  0C0   0E  0EE
B$  0EE  0EE  0EE  0EE  0EE  0EE   0C  0C0  0AA  0AA  0AA  0AA   0C  0CC   0E  0EE
B$  0EE  0EE  0EE  0EE  0EE  0EE   0C  0C0  0AA  0AA  0AA  0AA   0C  0CC    0    0
B$    0    0    0    0    0    0   0C  0C0  0AA  0AA  0AA  0AA   0C  0CC   03  03B
B$  0BB  0BB  0BB  0BB  0BB  0BB  0BB  0C0  0AA  0AA  0AA  0AA   0C  0CC    0  033
B$  0BB  0BB  0BB  0BB  0BB  0BB  0BB  0B0  0AA  0AA  0AA  0AA   0C  0CC    0   03
B$  03B  0BB  0BB  0BB  0BB  0BB  0BB  0B0    0    0    0    0   0C  0CC    0    0
B$  033  033  033  033  033  033  033  033   03  0BB  0BB  0BB  0BB  0CC    0    0
B$    0    0    0    0    0    0    0    0    0  03B  0BB  0BB  0BB  0BC    0    0
B$    0    0    0    0    0    0    0    0    0   03  033  033  033  033


iIconAndMask:
; AND monochrome mask: (8*16 octects > 8 pixels / byte > 32*32 pixels)

B$    0    0
B$    0   0F    0    0    0   07    0    0    0   03    0    0    0   01  080    0
B$    0   01  0C0    0    0   01  0E0    0    0   01  0F0    0    0   01  0F0    0
B$    0   01  0F0    0    0   01  0F0   03  080   01  0F8   03  080   01  0FC   03
B$  080   01   06   03  080   01   03   03  0C0   01   01  0FF  0E0   01    0  0C0
B$  030   01    0  0C0  018   01    0  0C0   0F  0FF    0  0C0   07  0FF    0    0
B$    0   07    0    0    0   03    0    0    0   01    0    0    0    0    0    0
B$    0    0    0    0    0    0  080    0    0    0  0C0    0    0    0  0E0    0
B$    0    0  0F0    0    0    0  0FF  0FF  0F8    0  0FF  0FF  0FC    0

EndiIconAndMask:]

 ______________________________

; RGB (eax) <> RGBquad (eax) (red green blue <> blue red green):

reverseRGB:
    Push ecx, ebx
      Mov ebx eax, ecx eax
      and eax 0FF | and ebx 0FF00 | and ecx 0FF0000
      shl eax 16 | shr ecx 16
      add eax ebx | add eax ecx
    Pop ebx, ecx
ret

 _______________________________

; Routines for drawing on Icon Dialog Box.

[iRECT:
 iRECTx1: D$ 120
 iRECTy1: D$ 20
 iRECTx2: D$ 130
 iRECTy2: D$ 30]

[iPAINTSTRUCT:
 ihdc: D$ ?
 ifErase: D$ ?
 ircPaint: B$ ?
 ifRestore: D$ ?
 ifIncUpdate: D$ ?
 irgbReserved: D$ ? ? ? ?  ? ? ? ?]

[iXcount: D$ ?
 iYcount: D$ ?]

; drawing one little box (each 'pixel') in edit icon image. Calculations at
; 'IBrush index' are to translate the "handles'Brushes" (linear dWords) in
; a pointer to "Brushes'structures". DL is used to rotary point to AND mask bits.

iOneRect:
    Mov dh B$iIconAndMask+ecx | and dh dl
    pushad
      If dh <> 0
; blank area box:
        Call 'GDI32.Rectangle' D$ihdc  D$iRECTx1  D$iRECTy1  D$iRECTx2  D$iRECTy2
; blank of true size icon at bottom:
        Mov eax D$iRECTx1, ebx D$iRECTy1 ; | sub eax 140 | sub ebx 276
        shr eax 3 | shr ebx 3 | add eax 40 | add ebx 360
        Call 'GDI32.SetPixel' D$ihdc eax ebx D$iBackGroundColor
      Else
        Push eax
; One color pixel box of icon :
          Call 'USER32.FillRect' D$ihdc iRECT D$H.IBrush0+eax
        Pop ebx
; show true size icon pixel at bottom:
        shr ebx 2                                    ; IBrush index
        Mov ecx ebx | shl ebx 1 | add ecx ebx        ; * 3
        inc ecx                                      ; + 1
        shl ecx 2                                    ; * 4
        Mov ecx D$IBrush0+ecx
      ;  Mov eax D$iRECTx1, ebx D$iRECTy1 | sub eax 140 | sub ebx 276
      ;  shr eax 3 | shr ebx 3 | add eax 40 | add ebx 360

        Mov eax D$iRECTx1, ebx D$iRECTy1
        shr eax 3 | shr ebx 3 | add eax 28 | add ebx 320
      ; Toto fix

        Call 'GDI32.SetPixel' D$ihdc eax ebx ecx      ; handle, X, Y, color
; NT > problem: only bottom line drawn (ebx always the same value ???) don't find any
; reason for...
      End_If
    popad
    ror dl 1 | On dl = 00_1000_0000, inc ecx
ret


; Drawing of edition image of icon:

iDrawColorBox:
    Call 'USER32.BeginPaint' D$H.IconEditor, iPAINTSTRUCT
    Call 'USER32.GetDC' D$H.IconEditor | Mov D$ihdc eax
    Mov D$iRECTx1 135,  D$iRECTy1 305, D$iRECTx2 0192,  D$iRECTy2 016C
    Call 'USER32.FillRect' D$ihdc iRECT D$H.IBackBrush
    Mov D$iRECTx1 140,  D$iRECTy1 310, D$iRECTx2 160,  D$iRECTy2 330, ecx 0
L0: Push ecx
      dec D$iRECTx1 | dec D$iRECTy1 | inc D$iRECTx2 | inc D$iRECTy2
      Call 'GDI32.Rectangle' D$ihdc  D$iRECTx1  D$iRECTy1  D$iRECTx2  D$iRECTy2
      inc D$iRECTx1 | inc D$iRECTy1 | dec D$iRECTx2 | dec D$iRECTy2
    Pop ecx
    Push ecx
      Mov al cl | shr al 2 | cmp B$ActualColor, al | jne L1>
        pushad
          Mov eax D$iRECTx1, ebx D$iRECTy1, ecx D$iRECTx2, edx D$iRECTy2
          sub eax 3 | sub ebx 3 | add ecx 3 | add edx 3
          Call 'GDI32.Rectangle' D$ihdc  eax ebx ecx edx
        popad
L1:   Call 'USER32.FillRect' D$ihdc iRECT D$H.IBrush0+ecx
    Pop ecx
      add ecx 4 | cmp ecx 64 | jae L9>
        add D$iRECTx1 34 | add D$iRECTx2 34
        If ecx = 32
          Mov D$iRECTx1 140,  D$iRECTy1 340, D$iRECTx2 160,  D$iRECTy2 360
        End_If
      jmp L0<<
L9: Call 'USER32.ReleaseDC' D$H.IconEditor, D$ihdc
    Call 'USER32.EndPaint' D$H.IconEditor, iPAINTSTRUCT
 ret


; Main Construction of the editor: edit image + true size icon + color selection boxes:

[iBackGroundColor: D$ ?
 DrawIconMessage: D$ ?]

DrawIcon:
    Call 'USER32.BeginPaint' D$H.IconEditor, iPAINTSTRUCT
      Call 'USER32.GetDC' D$H.IconEditor | Mov D$ihdc eax
      Call 'GDI32.GetPixel' eax 5 5 | Mov D$iBackGroundColor eax, D$iBackBrush+4 eax
      Call DeleteIconBrushes | Call CreateIconBrushes
        Mov D$iXcount 0, D$iYcount 0
        Mov D$iRECTx1 140,  D$iRECTy1 276,  D$iRECTx2 149,  D$iRECTy2 285
        Mov esi iIconXorMask, ecx 0, edx 00_1000_0000

L0:     Mov ebx 0, eax 0 | lodsb | Mov bl al | and ebx 0F | and eax 0F0 | shr eax 4
          shl eax 2 | shl ebx 2
            Call iOneRect
              inc D$iXcount | add D$iRECTx1 8 | add D$iRECTx2 8
                Mov eax ebx
                  Call iOneRect
                    inc D$iXcount | cmp D$iXcount 32 | je L2>
        add D$iRECTx1 8 | add D$iRECTx2 8 | jmp L0<<

L2:     inc D$iYcount | cmp D$iYcount 32 | je L3>
          Mov D$iRECTx1 140,  D$iRECTx2 149, D$iXcount 0
            sub D$iRECTy1 8 | sub D$iRECTy2 8 | jmp L0<<

L3: Call 'USER32.ReleaseDC' D$H.IconEditor,  D$ihdc
    Call 'USER32.EndPaint' D$H.IconEditor, iPAINTSTRUCT
    On D$DrawIconMessage <> &WM_MOUSEMOVE, Call iDrawColorBox
ret

 ______________________________________

; Brushes used by icon editor. 'iBrush0', ... are brushes creation data;
; 'ihBrush0, ... are brushes handles.

; [BS_SOLID 0] = first of 3 members, second is the color

 ; brushes struc
[IBrush0: D$ ? ? ?
 IBrush1: D$ ? ? ?
 IBrush2: D$ ? ? ?
 IBrush3: D$ ? ? ?
 IBrush4: D$ ? ? ?
 IBrush5: D$ ? ? ?
 IBrush6: D$ ? ? ?
 IBrush7: D$ ? ? ?
 IBrush8: D$ ? ? ?
 IBrush9: D$ ? ? ?
 IBrushA: D$ ? ? ?
 IBrushB: D$ ? ? ?
 IBrushC: D$ ? ? ?
 IBrushD: D$ ? ? ?
 IBrushE: D$ ? ? ?
 IBrushF: D$ ? ? ?
 iBrushesEnd:]

; brushes handles
[H.IBrush0: D$ ?
 H.IBrush1: D$ ?
 H.IBrush2: D$ ?
 H.IBrush3: D$ ?
 H.IBrush4: D$ ?
 H.IBrush5: D$ ?
 H.IBrush6: D$ ?
 H.IBrush7: D$ ?
 H.IBrush8: D$ ?
 H.IBrush9: D$ ?
 H.IBrushA: D$ ?
 H.IBrushB: D$ ?
 H.IBrushC: D$ ?
 H.IBrushD: D$ ?
 H.IBrushE: D$ ?
 H.IBrushF: D$ ?]

[iBackBrush: D$ ? ? ?
 H.IBackBrush: D$ ?]

CreateIconBrushes:
    Mov edi iBrush0, esi iIconPalette | add edi 4        ; store icon palette colors
L0: lodsd | Call reverseRGB | stosd | add edi 8          ; in brushes structures
    cmp edi iBrushesEnd | jb L0<

    Call 'GDI32.CreateBrushIndirect' iBrush0 | Mov D$H.IBrush0 eax
    Call 'GDI32.CreateBrushIndirect' iBrush1 | Mov D$H.IBrush1 eax
    Call 'GDI32.CreateBrushIndirect' iBrush2 | Mov D$H.IBrush2 eax
    Call 'GDI32.CreateBrushIndirect' iBrush3 | Mov D$H.IBrush3 eax
    Call 'GDI32.CreateBrushIndirect' iBrush4 | Mov D$H.IBrush4 eax
    Call 'GDI32.CreateBrushIndirect' iBrush5 | Mov D$H.IBrush5 eax
    Call 'GDI32.CreateBrushIndirect' iBrush6 | Mov D$H.IBrush6 eax
    Call 'GDI32.CreateBrushIndirect' iBrush7 | Mov D$H.IBrush7 eax
    Call 'GDI32.CreateBrushIndirect' iBrush8 | Mov D$H.IBrush8 eax
    Call 'GDI32.CreateBrushIndirect' iBrush9 | Mov D$H.IBrush9 eax
    Call 'GDI32.CreateBrushIndirect' iBrushA | Mov D$H.IBrushA eax
    Call 'GDI32.CreateBrushIndirect' iBrushB | Mov D$H.IBrushB eax
    Call 'GDI32.CreateBrushIndirect' iBrushC | Mov D$H.IBrushC eax
    Call 'GDI32.CreateBrushIndirect' iBrushD | Mov D$H.IBrushD eax
    Call 'GDI32.CreateBrushIndirect' iBrushE | Mov D$H.IBrushE eax
    Call 'GDI32.CreateBrushIndirect' iBrushF | Mov D$H.IBrushF eax
    Call 'GDI32.CreateBrushIndirect' iBackBrush | Mov D$H.IBackBrush eax
ret


DeleteIconBrushes:
    Call 'GDI32.DeleteObject'  D$H.IBrush0
    Call 'GDI32.DeleteObject'  D$H.IBrush1
    Call 'GDI32.DeleteObject'  D$H.IBrush2
    Call 'GDI32.DeleteObject'  D$H.IBrush3
    Call 'GDI32.DeleteObject'  D$H.IBrush4
    Call 'GDI32.DeleteObject'  D$H.IBrush5
    Call 'GDI32.DeleteObject'  D$H.IBrush6
    Call 'GDI32.DeleteObject'  D$H.IBrush7
    Call 'GDI32.DeleteObject'  D$H.IBrush8
    Call 'GDI32.DeleteObject'  D$H.IBrush9
    Call 'GDI32.DeleteObject'  D$H.IBrushA
    Call 'GDI32.DeleteObject'  D$H.IBrushB
    Call 'GDI32.DeleteObject'  D$H.IBrushC
    Call 'GDI32.DeleteObject'  D$H.IBrushD
    Call 'GDI32.DeleteObject'  D$H.IBrushE
    Call 'GDI32.DeleteObject'  D$H.IBrushF
    Call 'GDI32.DeleteObject'  D$H.IBackBrush
ret
 ________________________________________

; calculation of icon editor position of some user click:

[InsideEditBox edx  InsideColorBox ecx]

iPos:
    Mov InsideEditBox &FALSE, InsideColorBox &FALSE
    Mov eax D$MousePosX, ebx D$MousePosY
  ;  Mov eax 0, ebx 0 | Push D$Lparam | Pop ax, bx      ; eax > x / ebx > y
    cmp eax 08D  | jbe L9>
    cmp eax 018D | jae L9>
    cmp ebx 01C  | jbe L9>
    cmp ebx 011C | jae L8>
      sub eax 08D | sub ebx 01C                         ; Pos - origin
      Mov InsideEditBox &TRUE | ret
L8: cmp ebx 0135 | jb L9>
    cmp ebx 0167  | ja L9>
      sub eax 08D | sub ebx 0135
      Mov InsideColorBox &TRUE | ret
L9: ret

; Drawing in icon image.

[ActualColor: D$ ?]  ; accessed as byte and as dword.

; In: eax / ebx = indexes x/y to on icon pixel. we really write at iIconXorMask,
; (iIconAndMask used as "iIconXorMaskEnd"). first line is last one.
; When back from iPos, if user clicked on left upper little box, eax=0 / ebx=0
; ... on right lower one: eax=01F / ebx=01F:

EditIcon:
    shr eax 3 | shr ebx 3                                  ; 8 pixels per little box
    Push eax, ebx
      Mov esi iIconAndMask | sub esi 16 | shl ebx 4 | sub esi ebx ; + lines
      Mov ebx eax                                                 ; keep for odd test
      shr eax 1 | add esi eax
      Mov al B$ActualColor | Mov ah 00_1111_0000
      Test ebx 1 NOT_ZERO L2>
        shl al 4 | Mov ah 00_1111
L2:   and B$esi ah | or B$esi al
    Pop ebx, eax
    Mov esi EndiIconAndMask | sub esi 4 | shl ebx 2 | sub esi ebx
    Mov ecx eax
    shr eax 3 | add esi eax
    and ecx 00_111
    Mov al 00_1000_0000 | shr al cl
    or B$esi al | xor B$esi al
    Call DrawIcon
  ret

; Clearing in icon image.

ClearIconPix:
    shr eax 3 | shr ebx 3                               ; 8 pixels per little box
    Push eax, ebx
      Mov esi iIconAndMask | sub esi 16 | shl ebx 4 | sub esi ebx  ; + lines
      Mov ebx eax                                     ; keep for odd test
      shr eax 1 | add esi eax
      Mov al B$ActualColor | Mov ah 00_1111_0000
      Test ebx 1 NOT_ZERO L2>
        shl al 4 | Mov ah 00_1111
L2:   and B$esi ah
    Pop ebx, eax
    Mov esi EndiIconAndMask | sub esi 4 | shl ebx 2 | sub esi ebx
    Mov ecx eax
    shr eax 3 | add esi eax
    and ecx 00_111
    Mov al 00_1000_0000 | shr al cl
    or B$esi al
    Call DrawIcon
ret


; Search what color choice box user clicked on.

WhatColor:
    shr eax 5 | shr ebx 5 | shl ebx 3
    add eax ebx | Mov B$ActualColor al
    Call DrawIcon
ret


; user left click on the square rainbow:

[NewColor: D$ ?]

WhatNewColor:
    Call 'GDI32.GetPixel' D$RainbowDC eax ebx | Call ReverseRGB
    Mov edi iIconPalette, ebx D$ActualColor | shl ebx 2 | add edi ebx | stosd
    Call DeleteIconBrushes | Call CreateIconBrushes
    Call iDrawColorBox
ret

; User left Click on selected Color Box while in 'Choose Color':

SetNewColor:
    shr eax 5 | shr ebx 5 | shl ebx 3
    add eax ebx | cmp al B$ActualColor | jne L9>
      Call 'USER32.DestroyWindow' D$H.GreenSlider
      Call DeleteRainbowDC
      Mov B$OnRainbow &FALSE
      Call DrawIcon
L9: ret


iLeft:
    Call iPos
    cmp B$OnRainbow &TRUE | jne L5>

    If InsideEditBox = &TRUE
      Call WhatNewColor
    Else_If InsideColorBox = &TRUE
      Call SetNewColor
    End_If
    ret

L5: If InsideEditBox = &TRUE
      Call EditIcon
    Else_If InsideColorBox = &TRUE
      Call WhatColor
    End_If
L9: ret

 ________________________________________

; Showing Square Rainbow for color choice:

[RedBit 1  BlueBit  00_00000001_00000000_00000000  GreenBit  00_00000001_00000000
 NO_RED 0FFFFFF00  NO_BLUE 0FFFF  NO_GREEN 0FF00FF]

; size, width, height, planes, bitCount, compression imagesize x/meter y/meter 0  0
[RainBowHeader:
 D$ 40 255 255
 W$ 1 32
 D$ 0  65025
 xMeter: D$ 0
 yMeter: D$ 0   0  0]

[SquareRainbowPtr: D$ ?
 H.RainBow: D$ ?
 RainbowDC: D$ ?
 RainData: D$ ?
 SlideGreen: D$ ?
 OldRainbowBitMaP: D$ ?]

; Build a 2 dimensions color table in memory (blue/red):

DeleteRainbowDC:
    Call 'GDI32.SelectObject' D$RainBowDC  D$OldRainbowBitMaP
    Call 'GDI32.DeleteObject' D$H.RainBow
    Call 'GDI32.DeleteDC' D$RainbowDC
ret


Rainbow:
    Call 'USER32.BeginPaint' D$H.IconEditor iPAINTSTRUCT

    Call 'USER32.GetDC' D$H.IconEditor | Mov D$ihdc eax

    Call 'GDI32.CreateCompatibleDC' D$ihdc | Mov D$RainbowDC eax

    Call 'GDI32.CreateDIBSection' D$RainBowDC RainBowHeader,
                                     &DIB_RGB_COLORS RainData 0 0
    Mov D$H.RainBow eax

; filling colors data:

    Mov eax D$SlideGreen, ecx 0FF, edi D$RainData
    shl eax 8
L0: Push ecx
        Mov ecx 0FF
L1:     stosd | add eax RED_BIT | loop L1<
            and eax NO_RED
    Pop ecx
    add eax BLUE_BIT | loop L0<

; Painting at screen (we do not release rainbow DC and object here -needed-):

    Call 'GDI32.SelectObject' D$RainBowDC  D$H.RainBow
        Mov D$OldRainbowBitMaP eax
    Call 'GDI32.BitBlt' D$ihdc 140 30  255 255 D$RainbowDC 0  0  &SRCCOPY

    Call 'USER32.ReleaseDC' D$H.IconEditor D$ihdc

    Call 'USER32.EndPaint' D$H.IconEditor, iPAINTSTRUCT
ret


; Icon edition area is a little bit larger than 255/255 because of little boxes edges.
; we clean that here, at first 'color box right click':

clearIconArea:
    Call 'USER32.BeginPaint' D$H.IconEditor, iPAINTSTRUCT
      Call 'USER32.GetDC' D$H.IconEditor | Mov D$ihdc eax
        Mov D$iRECTx1 135,  D$iRECTy1 25, D$iRECTx2 400,  D$iRECTy2 290
        Call 'USER32.FillRect' D$ihdc iRECT D$H.IBackBrush
      Call 'USER32.ReleaseDC' D$H.IconEditor  D$ihdc
    Call 'USER32.EndPaint' D$H.IconEditor, iPAINTSTRUCT
ret


[TrackClassName: B$ 'msctls_trackbar32' EOS]
[TrackTitle: B$ 'Green' EOS]
[H.GreenSlider: D$ ?]

GreenSlider:

    Call 'USER32.CreateWindowExA' &WS_EX_LEFT,
                                  TrackClassName,
                                  TrackTitle,
                                  &WS_CHILD+&WS_VISIBLE+&TBS_LEFT+&TBS_VERT,
                                  410,
                                  25,
                                  20,
                                  265,
                                  D$H.IconEditor,
                                  ID_TRACKBAR,
                                  D$H.Instance,
                                  0
    Mov D$H.GreenSlider eax
    Call 'USER32.SendMessageA' D$H.GreenSlider  &TBM_SETRANGE 1 0FF_0000
    Mov eax D$SlideGreen | not al
    Call 'USER32.SendMessageA' D$H.GreenSlider  &TBM_SETPOS 1  eax
ret


[OnRainbow: D$ ?
 OldSelectedColor: D$ ?]

RestorePreviousColor:
    Mov edi iIconPalette, eax D$ActualColor | shl eax 2
    add edi eax | Mov eax D$OldSelectedColor | stosd
    Call DeleteIconBrushes | Call CreateIconBrushes | Call DrawIcon
ret


SaveActualColor:
    Mov esi iIconPalette, eax D$ActualColor | shl eax 2
    add esi eax | lodsd | Mov D$OldSelectedColor eax
ret


iRight:
    If B$OnRainbow = &TRUE
      Call RestorePreviousColor
      Call 'USER32.DestroyWindow' D$H.GreenSlider
      Call deleteRainbowDC
      Mov B$OnRainbow &FALSE | ret
    End_If

    Call iPos
    If InsideEditBox = &TRUE
      Call ClearIconPix
    Else_If InsideColorBox = &TRUE
      Mov B$OnRainbow &TRUE
      Call WhatColor
      Call SaveActualColor
      Mov esi iIconPalette, eax D$ActualColor | shl eax 2
      add esi eax | inc esi | Mov eax 0, al B$esi | Mov D$SlideGreen eax
      Call clearIconArea | Call GreenSlider | Call RainBow
    End_If
L9: ret


; Retrieve the slider value on user Move and redraw rainbow:

iTrackMove:
      Call 'USER32.SendMessageA' D$H.GreenSlider &TBM_GETPOS 0 0
        not al | Mov D$SlideGreen eax
      Call DeleteRainbowDC | Call Rainbow
L9: ret

 ___________________________________________________________________________________
 ___________________________________________________________________________________

; reading and writing icons from/to files:

 ___________________________________________________________________________________

; Reading and writing an icon from/to a PE:
 ___________________________________________________________________________________

[iStartOfResources: D$ 0
 iExePtr: D$ 0
 iExeLen: D$ 0
 iResourceRVA: D$ 0
 H.ISource: D$ 0
 iSourceFilterPtr: D$ 1
 iPEFilesFilters:  B$ 'PE files' EOS '*.exe' EOS  0]

[iuFileFilter: B$ ? # &MAX_PATH]
[iSaveFilter: B$ ? # &MAX_PATH]
[iChoosenFile: B$ ? # &MAX_PATH]

[iOpenPEStruc: len
 ihwndPEFileOwner: D$ 0
 iOPESInstance: D$ 0
 D$ iPEFilesFilters
 D$ iuFileFilter
 D$ &MAX_PATH
 D$ iSourceFilterPtr
 D$ iSaveFilter
 D$ &MAX_PATH
 D$ iChoosenFile
 D$ &MAX_PATH
 D$ 0
 D$ 0 ;OpenPEFileTitle
 D$ 0281804 ; !!! WinEquate ?
 D$ 0 0 0 0 0]

[ResourcesRVA: D$ ?
 NoResourcesPE: D$ ?
 PeIconFound: D$ ?]

OpenPeForReadingIcon:
  ; Opening a file:
    Mov B$NoResourcesPE &FALSE

    Call ClearPATH iSaveFilter

    Call 'Comdlg32.GetOpenFileNameA' iOpenPEStruc

    On D$iSaveFilter = 0 ret
 ______________________________________

  ; Loading the entire file in memory:

    On D$H.ISource > 0 Call 'KERNEL32.CloseHandle' D$H.ISource

    Mov esi iSaveFilter

    Call 'KERNEL32.CreateFileA' esi &GENERIC_READ, &FILE_SHARE_READ+&FILE_SHARE_WRITE,
                                0, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE
        Call MessageBox D$BusyFilePtr ;;;;| Pop eax | ret  ; return to caller of caller
        Mov D$H.ISource 0
    Else
        Mov D$H.ISource eax

        Call 'KERNEL32.GetFileSize'  eax 0
        Mov D$iExeLen eax

        Call VirtualAlloc iExePtr,
                          eax


        Call 'KERNEL32.ReadFile' D$H.ISource D$iExePtr,
                                 eax NumberOfReadBytes 0      ; load headers
    End_If
ret


ReadRosAsmPeIcon:                     ; reused by general purpose RosAsm PE opening.
    Mov B$PeIconFound &FALSE

  ; read dos header:
    Mov esi D$iExePtr
    Mov eax 0 | add esi 8 | lodsw    ; parag. size of dos header end >PE header adress

    shl eax 4 | sub eax 4
    Mov esi D$iExePtr | add esi eax | lodsd      ; eax = PE header

    Mov esi D$iExePtr | add esi eax
    If W$esi <> 'PE'
        Mov esi D$iExePtr | add esi 03C | lodsd | add eax D$iExePtr | Mov esi eax
        cmp W$esi 'PE' | jne PeNotFound
    End_If

 ______________________________________

  ; read data in PE header:

    movzx ecx w$esi+6                     ; word record of section number
    add esi 136 | lodsd | Mov ebx eax     ; RVA of resources from "Image Data Dir..."
    Mov D$ResourcesRVA eax  ; jmp over general purpose headers and reach PE sections headers:

    add esi 120                           ; esi points to RVA of first section header

L0: lodsd | cmp eax ebx | je L1>
        add esi 36 | loop L0<
        On ebx = 0, jmp AbortIconSearch
          jmp SectNotFound

L1: On D$esi-16 <> '.rsr', jmp AbortIconSearch

 ______________________________________

  ; if here, '.rsrc' section found:

L1: add esi 4 | lodsd                            ; > app ptr to resources

    add eax D$iExePtr | Mov D$iStartOfResources eax

DisReadMainIcon:
    Mov B$PeIconFound &FALSE

    Mov esi eax | add esi 14                     ; > number of ID resources
    Mov eax 0 | lodsw | Mov ecx eax              ; > in ecx
    add cx W$esi-4                               ; add number of Named IDs
    On eax = 0, jmp AbortIconSearch      ; if no resources at all

  ; search RT_ICON in resource general header:

L0: lodsd | cmp eax RT_ICON | je L1>
    lodsd | loop L0<
      jmp AbortIconSearch                        ; no icon found (possible naked PE)

L1: lodsd                   ; icon found "Level2Rt_Icon-StartOfRsrc+NodeFlag" in eax
    and eax 0FFFFFFF        ; strip node flag (0_80000000)
    add eax D$iStartOfResources

    add eax 14 | Mov esi eax, edx 0, dx W$esi | sub  esi 2

 ______________________________________

  ; resource TYPEs dir:

NextiRecord:

    add esi 8
    Push esi

    lodsd                              ; "Level3Rt_Icon-StartOfRsrc+NodeFlag" in eax
    and eax 0FFFFFFF
    add eax D$iStartOfResources
    add eax 20 | Mov esi eax

  ; language. dir:
    lodsd                      ; "Level4Rt_Icon-StartOfRsrc" in eax (no NodeFlag here
                                       ; next one is leave ptr to true resources)
    add eax D$iStartOfResources
    Mov esi eax

  ; records of each resource:
    lodsd                              ; ptr to icon data (but RVA - startOfResource)
    Mov ecx D$esi

    If ecx <> 02E8                                       ; 2E8h = size of common icons
      Pop esi | dec edx | On edx > 0, jmp NextiRecord
        jmp BadIcoSize
    Else
      Pop esi
    End_If

    sub eax ebx                              ; - RVA
    add eax D$iStartOfResources              ; eax now points to true icon data
    Mov B$PeIconFound &TRUE
  ret


SectNotFound:    Mov eax SectionNotFound | jmp L9>
AbortIconSearch:

    If D$SavingExtension = '.DLL'
        jmp L7>
    Else_If D$SavingExtension = '.SYS'
        jmp L7>
    End_If

                    Mov eax NoIcon       | jmp L9>
BadIcoSize:      Mov eax BadIconSize     | jmp L9>
PeNotFound:      Mov eax NoPE

L9: ;If B$Disassembling = &TRUE
    ;    Mov D$iExePtr 0 | ret  ; restored by Disassembler Call (used as Flag, here).
    ;End_If
    Call MessageBox eax ;;;;| Call IconSearchOut ;| Pop eax |
    ret     ;;;; return to caller of caller

L7: Mov B$NoResourcesPE &TRUE ;;;;| Call IconSearchOut |
    ret


;IconSearchOut:
;    Mov eax D$UserPeStart | On D$iExePtr <> eax, VirtualFree D$iExePtr
;  ; Don't destroy User Source in any case, if the 'readRosAsmPeIcon' Routine is called
;  ; from some Open PE ,normal Functions.
ret

[SectionNotFound: B$ 'Section not found' EOS]
[BadIconSize: B$ 'Icon size not assumed' EOS]
[NoPE: B$ 'PE signature not found' EOS]
[NoIcon: B$ 'Icon not found in this file' EOS]


; peek from PE:

PeekIcon:
    Call OpenPeForReadingIcon

    .If D$H.ISource <> 0
        Call ReadRosAsmPeIcon                        ; eax > start of icon data

        If B$PeIconFound = &TRUE
            Mov esi eax | Mov edi iIcon | rep movsb  ; Copying to ower buffer
        End_If

        Call 'KERNEL32.CloseHandle' D$H.ISource | Mov D$H.ISource 0

        Call VirtualFree iExePtr

    .End_If
ret


[H.IDestination: D$ ?]
[PokeSure: B$ 'Ready to modify choosen PE?' EOS]
[NullTitle: B$ SPC EOS]

; Poke inside PE:

PokeIcon:
    Call OpenPeForReadingIcon                        ; eax > start of icon data

    ..If D$H.ISource <> 0
        Call ReadRosAsmPeIcon                        ; eax > start of icon data

        .If B$PeIconFound = &TRUE
            Mov edi eax | Mov esi iIcon | rep movsb  ; Copying from ower buffer
            Call 'KERNEL32.CloseHandle' D$H.ISource | Mov D$H.ISource 0
            Call 'USER32.MessageBoxA' D$H.MainWindow  PokeSure  NullTitle,
                                    &MB_YESNO+&MB_ICONEXCLAMATION +&MB_SYSTEMMODAL
            On eax = &IDNO, jmp L9>>

            Call 'KERNEL32.CreateFileA' iSaveFilter &GENERIC_WRITE,
                                        &FILE_SHARE_READ+&FILE_SHARE_WRITE, 0,
                                        &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
            If eax = &INVALID_HANDLE_VALUE

                Call MessageBox D$BusyFilePtr

ret

            Else
                Mov D$H.IDestination eax
            End_If

            Mov D$NumberOfReadBytes  0
            Call 'KERNEL32.WriteFile'   D$H.IDestination D$iExePtr D$iExeLen,
                                        NumberOfReadBytes  0
        .End_If

L9:     Call VirtualFree iExePtr

        Call 'KERNEL32.CloseHandle' D$H.IDestination

    ..End_If

ret

 _______________________________________________________________________________________

 ; reading and writting .ico files:
 _______________________________________________________________________________________

[FileIconDir: W$ 0 1  FIDcount: 1

FIDentriesTable: FIwidth: B$ 020   FIheight: 020    FIcolorCount: 010  0
                 FIplanes: W$ 0     FIbitCount: 0
             FIBytesInRes: D$ 02E8  FIdwImagePtr: 022    ; * by "FIDcount" number
             FileIconDirLen: len]

[IcoFilePtr: D$ ?
 IcoFileLen: D$ ?]

[icoFilesFilters:  B$ 'icon files' EOS '*.ico' EOS 0]
[OpenIcoFileTitle: B$ 'Choose an icon file' EOS]

[icoOpenStruc: len
 icohwndFileOwner: 0  icohInstance:0  icoFilesFilters  iuFileFilter  &MAX_PATH
 iSourceFilterPtr  iSaveFilter  &MAX_PATH  iChoosenFile  &MAX_PATH  0
 OpenIcoFileTitle  IOSflags: 0281804 ; for read
 0  0  0  0  0]            ; 0280006 : for write  .ico



[BadFIsiz: B$ 'No 36/36 icon in this file (or too much colors)' EOS]

BadFIsize: Call MessageBox BadFIsiz

ret


ReadIcoFile:
  ; Opening a .ico file:

    Call ClearPATH iSaveFilter

    Mov D$IOSflags 0281804 | Call 'Comdlg32.GetOpenFileNameA' icoOpenStruc

    On D$iSaveFilter = 0  ret

  ; Loading the entire file in memory:

    Call 'KERNEL32.CreateFileA' iSaveFilter &GENERIC_READ, &FILE_SHARE_READ+&FILE_SHARE_WRITE,
                                0, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
                                              ; hTemplateFile
    If eax = &INVALID_HANDLE_VALUE

      Call MessageBox D$BusyFilePtr

ret         ; return to caller of caller

    Else
      Mov D$H.ISource eax
    End_If

    Call 'KERNEL32.GetFileSize'  eax 0 | Mov D$icoFileLen eax

    Call VirtualAlloc icoFilePtr,
                      eax



    Call 'KERNEL32.ReadFile' D$H.ISource D$icoFilePtr,
                             eax NumberOfReadBytes 0

    Mov esi D$IcoFilePtr | add esi 4
    lodsw | movzx ecx ax                             ; icons number in ecx
L0: cmp B$esi 020 | je L1>                           ; first size
      add esi 16 | loop L0<                          ; next record
        jmp BadFIsize
L1: On D$esi+8 <> 02E8, jmp BadFIsize
    add esi 12                                       ; ptr to ico data
    Mov esi D$esi, edi iIcon, ecx 02E8
    add esi D$IcoFilePtr | rep movsb                 ; Copying to ower buffer

    Call 'KERNEL32.CloseHandle' D$H.ISource

    Call VirtualFree icoFilePtr

ret


[NewOnly: B$ 'This option saves only new files' EOS]

WriteIcoFile:
  ; Opening a .ico file:

    Call ClearPATH iSaveFilter

    Mov D$IOSflags 0288006 | Call 'Comdlg32.GetSaveFileNameA' icoOpenStruc

    On D$iSaveFilter = 0  ret

    Call 'KERNEL32.CreateFileA' iSaveFilter &GENERIC_WRITE, 0, 0,
                                &CREATE_NEW, &FILE_ATTRIBUTE_NORMAL, 0

    If eax = &INVALID_HANDLE_VALUE

      Call MessageBox eax NewOnly

ret  ; return to caller of caller

    Else
      Mov D$H.IDestination eax
    End_If

    Mov D$NumberOfReadBytes  0
    Call 'KERNEL32.WriteFile'   D$H.IDestination iIcoFileHeader 02FE,
                               NumberOfReadBytes  0
    Call 'KERNEL32.CloseHandle' D$H.IDestination
ret

 ____________________________________________________________________________________


; saving icon data in user stub data for effective compilation:

StoreIcon:
    Mov esi iIcon, edi uIcon, ecx 02E8 | rep movsb
ret

 ____________________________________________________________________________________
 ____________________________________________________________________________________

; Main of icon edition (dialog box Proc):

IconEdition:
    If D$H.IconEditor = 0
        Call 'USER32.DialogBoxIndirectParamA' D$H.Instance, IconDialogData, D$H.MainWindow,
                                              IconEditProc, 0
    Else
        Call Beep
    End_If
ret


[iDraw: D$ ?
 iArase: D$ ?]

[H.IconEditor: D$ ?]

Proc IconEditProc:
  Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    Move D$DrawIconMessage D@msg

    ...If D@msg = &WM_PAINT
        Call DrawIcon

    ...Else_If D@msg = &WM_VSCROLL
        Call iTrackMove

    ...Else_If D@msg = &WM_LBUTTONDOWN
        Mov B$iDraw &TRUE

    ...Else_If D@msg = &WM_LBUTTONUP
        Push D@Lparam | Pop W$MousePosX, W$MousePosY
        Mov B$idraw &FALSE | Call iLeft

    ...Else_If D@msg = &WM_RBUTTONDOWN
        Mov B$iArase &TRUE

    ...Else_If D@msg = &WM_RBUTTONUP
        Push D@Lparam | Pop W$MousePosX, W$MousePosY
        Mov B$iArase &FALSE | Call iRight

    ...Else_If D@msg = &WM_MOUSEMOVE
        Push D@Lparam | Pop W$MousePosX, W$MousePosY
        If B$iDraw = &TRUE
            Call iLeft
        Else_If B$iArase = &TRUE
            Call iRight
        End_If

    ...Else_If D@msg = &WM_COMMAND
        .If D@wParam = &IDCANCEL
            Mov D$H.IconEditor 0
            Call 'USER32.EndDialog' D@hwnd 0
        .Else_If D@wParam = ID_Inew
            Mov edi iIconAndMask, ecx 128, al 0FF | rep stosb
            Mov edi iIconXorMask, ecx 512, al 0   | rep stosb
            Call DrawIcon
        .Else_If D@wParam = ID_Ipeek
            Call PeekIcon | Call DrawIcon
        .Else_If D@wParam = ID_Ipoke
            Call PokeIcon
        .Else_If D@wParam = ID_IfromIco
            Call ReadIcoFile | Call DrawIcon
        .Else_If D@wParam = ID_ItoIco
            Call WriteIcoFile
        .Else_If D@wParam = ID_iKeep
            Move D$H.IconEditor 0
            Call StoreIcon | Call 'USER32.EndDialog' D@hwnd 0
        .Else_If D@wParam = ID_Help
            Call Help, B_U_AsmName, IconHelp, ContextHlpMessage
        .Else
            popad | Mov eax &FALSE | jmp L9>>
        .End_If

    ...Else_If D@msg = &WM_INITDIALOG
        Move D$H.IconEditor D@hwnd
        Move D$icohInstance D$H.Instance
        Move D$iOPESInstance D$H.Instance
        Move D$icohwndFileOwner D@hwnd
        Move D$ihwndPEFileOwner D@hwnd
        Call CreateIconBrushes
        Call 'USER32.SetClassLongA' D@hwnd &GCL_HICON D$STRUC.WINDOWCLASS@hIcon

    ...Else_If D@msg = &WM_Close
        Mov D$H.IconEditor 0
        If B$OnRainbow = &TRUE
            Call RestorePreviousColor
            Call 'USER32.DestroyWindow' D$H.GreenSlider
            Call deleteRainbowDC
            Mov B$OnRainbow &FALSE
        End_If
        popad | Call DeleteIconBrushes | Mov eax &FALSE | jmp L9>

    ...Else
        popad | Mov eax &FALSE | jmp L9>

    ...End_If

    popad | Mov eax &TRUE

L9: EndP
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
