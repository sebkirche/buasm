TITLE Libs            ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________
____________________________________________________________________________________________
____________________________________________________________________________________________

;              FIRST PART: TRANSLATING THE WHOLE LIBARY INTO ROSASM SYNTAX

;              Authors: Guga and Betov. August/September 2005?
____________________________________________________________________________________________
____________________________________________________________________________________________

;;

  * Main function:
        LibScanner

  * Main Procedure (Dialog): 'ScanLibFile'
  
  * GUI Functions:

    LibScanCleanUp, LibScanDialog_EnableContinueMenu, SaveLibFileAs, LibScanDialog_ToggleToolbarText,
    LibScanDialog_HoverToolbarText, LibScanDialog_EnableContinuePrevTabMenu, LibScanDialog_EnableContinueNextTabMenu,
    OpenLibFile, LibScanDisplayFileSize, WriteObjectTypeinTitle, CoolControl_LVBeginSort, LibScanDialog_OnNotify,
    CoolControlTB_CreateImageList, CoolControlDlg_CreateTab, Tab1Proc, Tab2Proc, CoolControlWin_CreateToolbar,
    SetupListview, CoolControl_LVBeginSort, ListViewLibSort

  * Library Parsing Routines:
    
    LibCoffMarking, GetLibListing, GetOneCoffListing
    >>> 'OpenLibFile', 'LibSignatureCheck'
  
    >>> either 'GetLibListing' or 'GetOneCoffListing'
    
    'ParseLibObj' calls for 'GetCoffListing', in case of Obj Files in .Lib.
    
    CodeView Checking:
        WriteRawDataDebugS


  * Routines in charge of the IMAGE_FILE_MACHINE Equates:
  
    'WriteIMAGE_FILE_MACHINE', 'IsIMAGE_FILE_MACHINE'
    
    
  * 'WriteAuxiliarySymbolsRecords'

    ToDo:
    
    -   Build the steps for the DIS System. Need to identify all symbols, accordying to the DataTypes. I mean,
        we need to interpret them when they are found inside a CodeView file or inside regular Symbol Structures
        (Auxiliary or not). This is a hell of a work, but it is worthfull, because they contains full info.

    -   Analyze and parse OMF Files (Borland and Microsoft, Libs and Objs). They are not too hard to parse, but there are
        minor differences between M$ and Borland ones.

    -   Analyze and Parse all Delphi DCU files. They are not too hard to parse (Check Dede and DMFtoINT), but
        we must take care of the different delphi versions, because the structures varies sometimes.

;;
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  File Jobs:
;;

;;
[LibsFileFilter: B$ 'Supported Files (*.lib, *.a, *.obj, *.o, *.dcu, *.pdb, *.dbg)' EOS  '*.lib;*.a;*.obj;*.o;*.dcu;*.pdb;*.dbg' EOS
                 B$ 'Library Files (*.lib)' EOS  '*.lib' EOS
                 B$ 'Library Files MingWin32/Gcc (*.a)' EOS  '*.a' EOS
                 B$ 'Object Files (*.obj)' EOS  '*.obj' EOS
                 B$ 'Object Files MingWin32/Gcc (*.o)' EOS  '*.o' EOS
                 B$ 'Delphi and Kilyx Control Unit Files  (*.dcu)' EOS  '*.dcu' EOS
                 B$ 'Program DataBase Files (*.pdb)' EOS  '*.pdb' EOS
                 B$ 'Debugging Files  (*.dbg)' EOS  '*.dbg' EOS
                 B$ 'All Files' EOS  '*.*' EOS 0]

[CustomLibsFileFilter:  D$ ? # &MAX_PATH]
[LibSaveFilter: D$ ? # &MAX_PATH]
[ChoosenLibFile: D$ ? # &MAX_PATH]
[OpenLibFileTitle: B$ 'Choose a Lib file...' EOS]

; OpenFile Structure for Libs:

[OPENLIB:
 @lStructSize: D$ Len
 @hwndOwner: D$ 0
 @hInstance: D$ 0
 @lpstrFilter: D$ LibsFileFilter
 @lpstrCustomFilter: D$ CustomLibsFileFilter
 @nMaxCustFilter: D$ 260
 @nFilterIndex: D$ 1
 @lpstrFile: D$ LibSaveFilter
 @nMaxFile: D$ 260
 @lpstrFileTitle: D$ ChoosenLibFile
 @nMaxFileTitle: D$ 260
 @lpstrInitialDir: D$ 0
 @lpstrTitle: D$ OpenLibFileTitle
 @Flags: D$ &OFN_EXPLORER__&OFN_FILEMUSTEXIST__&OFN_LONGNAMES__&OFN_PATHMUSTEXIST
 @nFileOffset: W$ 0
 @nFileExtension: W$ 0
 @lpstrDefExt: D$ 0
 @lCustData: D$ 0
 @lpfnHook: D$ 0
 @lpTemplateName: D$ 0]
____________________________________________________________________________________________

[H.LibFile: D$ ?
 LibFileLength: D$ ?
 LibFileMemory: D$ ?
 LibFileMemoryEnd: D$ ?]

Proc OpenLibFile:
    Arguments @hwnd

    Call 'KERNEL32.CreateFileA' LibSaveFilter, &GENERIC_READ,
                                &FILE_SHARE_READ+&FILE_SHARE_WRITE, 0, &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL, 0
    Mov D$H.LibFile eax

    Call 'KERNEL32.GetFileSize' eax, 0 | Mov D$LibFileLength eax

    Call LibScanDisplayFileSize D@hwnd

    If eax = 0
        Call LibScanDialog_EnableContinueMenu &FALSE
        Call 'USER32.MessageBoxA' 0, {B$ "Dumping process aborted !!!", D$ CRLF2, B$ "The file you are trying to load is empty (The size of the file is Zero)." EOS}, {B$ 'LibScan' EOS},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL
    Else

        add eax 10

        Call VirtualAlloc LibFileMemory,
                          eax

        add D$LibFileMemory 10

        Call 'KERNEL32.ReadFile' D$H.LibFile, D$LibFileMemory,
                                 D$LibFileLength, NumberOfReadBytes, 0

        Call 'KERNEL32.CloseHandle' D$H.LibFile

        Mov eax D$LibFileMemory | add eax D$LibFileLength | Mov D$LibFileMemoryEnd eax
    End_If
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
;;
  Called from 'ScanLibFile' Dialog Proc:
  
      Main Procedures for the RosAsm syntax translation of a Library
;;
;;
____________________________________________________________________________________________

; Object and Library Constants
[UNKNOWN_LIB_FILE 0]
[COFF_LIB_FILE 1]
[COFF_OBJ_FILE 2]
[DCU1_KILYX_OBJ_FILE 3]
[DCU2_KILYX_OBJ_FILE 4]
[DCU3_KILYX_OBJ_FILE 5]
[DCU2_OBJ_FILE 6]
[DCU3_OBJ_FILE 7]
[DCU4_OBJ_FILE 8]
[DCU5_OBJ_FILE 9]
[DCU6_OBJ_FILE 10]
[DCU7_OBJ_FILE 11]
[OMF_OBJ_FILE 12]
[PDB_OBJ_FILE 13]
[DBG_OBJ_FILE 14]

; Verification Buffer to hold the above constants
[ValidLib: D$ 0]

Proc LibSignatureCheck:
    pushad

  ; Valid Tag?
    Mov esi D$LibFileMemory

    .If D$esi+2 = 'arch'
        ; Valid .lib
        Mov D$ValidLib COFF_LIB_FILE

    .Else_If D$esi = 0F21F148C ; Borland Kylix 1.0 unit file (DCU). Ver=100
        Mov D$ValidLib DCU1_KILYX_OBJ_FILE

    .Else_If D$esi = 0E1011DD ; Borland Kylix 2.0 unit file (DCU). Ver=101
        Mov D$ValidLib DCU2_KILYX_OBJ_FILE

    .Else_If D$esi = 0E0001DD ; Borland Kylix 2.0 unit file (kind $00) (DCU). Ver=101
        Mov D$ValidLib DCU2_KILYX_OBJ_FILE

    .Else_If D$esi = 0F1001DD ; Borland Kylix 3.0 unit file (DCU). Ver=102
        Mov D$ValidLib DCU3_KILYX_OBJ_FILE

    .Else_If D$esi = 050505348 ; Borland Delphi 2.0 unit file (DCU). Ver=2
        Mov D$ValidLib DCU2_OBJ_FILE

    .Else_If D$esi = 044518641 ; Borland Delphi 3.0 unit file (DCU). Ver=3
        Mov D$ValidLib DCU3_OBJ_FILE

    .Else_If D$esi = 04768A6D8 ; Borland Delphi 4.0 unit file (DCU). Ver=4
        Mov D$ValidLib DCU4_OBJ_FILE

    .Else_If D$esi = 0F21F148B ; Borland Delphi 5.0 unit file (DCU). Ver=5
        Mov D$ValidLib DCU5_OBJ_FILE

    .Else_If D$esi = 0E0000DD ; Borland Delphi 6.0 unit file (DCU). Ver=6
        Mov D$ValidLib DCU6_OBJ_FILE

    .Else_If D$esi = 0E8000DD ; Borland Delphi 6.0 unit file (kind $80) (DCU). Ver=6
        Mov D$ValidLib DCU6_OBJ_FILE

    .Else_If D$esi = 0FF80FFFF ; Borland Delphi 7.0 (Free) (DCU). Ver=7
        Mov D$ValidLib DCU7_OBJ_FILE

    .Else_If D$esi = 0FF0000DF ; Borland Delphi 7.0 unit file (DCU). Ver=7
        Mov D$ValidLib DCU7_OBJ_FILE

    .Else_If D$esi = 0F0000DF ; Borland Delphi 7.0 unit file (DCU). Ver=7
        Mov D$ValidLib DCU7_OBJ_FILE

    .Else_If_Or W$esi = &IMAGE_FILE_MACHINE_UNKNOWN, W$esi = &IMAGE_FILE_MACHINE_ALPHA,
                W$esi = &IMAGE_FILE_MACHINE_ALPHA64, W$esi = &IMAGE_FILE_MACHINE_ARM,
                W$esi = &IMAGE_FILE_MACHINE_AXP64, W$esi = &IMAGE_FILE_MACHINE_CEF,
                W$esi = &IMAGE_FILE_MACHINE_I386, W$esi = &IMAGE_FILE_MACHINE_I486,
                W$esi = &IMAGE_FILE_MACHINE_I586, W$esi = &IMAGE_FILE_MACHINE_IA64,
                W$esi = &IMAGE_FILE_MACHINE_M68K, W$esi = &IMAGE_FILE_MACHINE_MIPS16,
                W$esi = &IMAGE_FILE_MACHINE_MIPSFPU, W$esi = &IMAGE_FILE_MACHINE_MIPSFPU16,
                W$esi = &IMAGE_FILE_MACHINE_POWERPC, W$esi = &IMAGE_FILE_MACHINE_R10000,
                W$esi = &IMAGE_FILE_MACHINE_R3000, W$esi = &IMAGE_FILE_MACHINE_R4000,
                W$esi = &IMAGE_FILE_MACHINE_R6000, W$esi = &IMAGE_FILE_MACHINE_SH3,
                W$esi = &IMAGE_FILE_MACHINE_SH3E, W$esi = &IMAGE_FILE_MACHINE_SH4,
                W$esi = &IMAGE_FILE_MACHINE_SH5, W$esi = &IMAGE_FILE_MACHINE_SH3DSP,
                W$esi = &IMAGE_FILE_MACHINE_THUMB, W$esi = &IMAGE_FILE_MACHINE_WCEMIPSV2,
                W$esi = &IMAGE_FILE_MACHINE_AM33, W$esi = &IMAGE_FILE_MACHINE_AMD64,
                W$esi = &IMAGE_FILE_MACHINE_CEE, W$esi = &IMAGE_FILE_MACHINE_EBC,
                W$esi = &IMAGE_FILE_MACHINE_M32R, W$esi = &IMAGE_FILE_MACHINE_POWERPCFP,
                W$esi = &IMAGE_FILE_MACHINE_TRICORE

        ; Valid .obj, if here:
        Mov D$ValidLib COFF_OBJ_FILE

    .Else_If_And B$esi = 080, W$esi+1 < 1024, W$esi+1 > 0 ; the W$esi+1 is the record lenght. The max size of the record len is 1024(The entire record, inclusind the magic signature), and it must not be 0
        ; Valid OMF .obj
        Mov D$ValidLib OMF_OBJ_FILE

    .Else_If D$esi+40 = 0474A
        ; Valid Pdb file
        Mov D$ValidLib PDB_OBJ_FILE

    .Else_If D$esi = 04944
        ; Possible dbg file found
        Mov D$ValidLib DBG_OBJ_FILE

    .Else
;;
;;
        cmp B$esi 0F0; LibHdr
        jnz @TestFileType2
        
            Mov eax eax
        @TestFileType2: ; Object File Found

        cmp B$esi 080; THEADDR ; Object OMF Sig
        jz @TestFileType3
        cmp B$esi 082; LHEADDR ; Library OMF Sig (Microsoft)
        jnz @TestFileType4

        @TestFileType3:
            movzx eax B$esi+2
            inc esi
            movzx eax B$esi+1 ; The Record length is a word, but the Low order must be 0, because the lenght must be only 255 bytes long
            movzx ecx al
            
@TestFileType4: ; Not OMF
;;
;;
        Mov D$ValidLib UNKNOWN_LIB_FILE
    .End_If
    popad
EndP
____________________________________________________________________________________________

; All Identified Libs and Objects are identified by this function.

Proc ParseIdentifiedLibs:
    Arguments @hwnd
    pushad

    Mov esi D$LibFileMemory

    .If D$ValidLib = COFF_LIB_FILE
        Call WriteObjectTypeinTitle D@hwnd, {B$ "Library COFF" EOS}
        Call CoffLibFound
        Call GetLibListing D@hwnd, hLibReportEdit, hLibReportEditLength

    .Else_If D$ValidLib = COFF_OBJ_FILE
         Call WriteObjectTypeinTitle D@hwnd, {B$ "Object COFF" EOS}
         Call GetOneCoffListing D@hwnd, hLibReportEdit, hLibReportEditLength


    .Else_If D$ValidLib = DCU1_KILYX_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        Call LibScanDialog_EnableContinueMenu &FALSE
        Call WriteObjectTypeinTitle D@hwnd, {B$ "Delphi Kilyx 1" EOS}
        Call 'USER32.MessageBoxA' D@hwnd, {B$ "Delphi Kilyx 1 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet." EOS}, {B$ 'LibScan' EOS},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DCU2_KILYX_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        Call LibScanDialog_EnableContinueMenu &FALSE
        Call WriteObjectTypeinTitle D@hwnd, {B$ "Delphi Kilyx 2" EOS}
        Call 'USER32.MessageBoxA' D@hwnd, {B$ "Delphi Kilyx2 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet." EOS}, {B$ 'LibScan' EOS},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DCU3_KILYX_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        Call LibScanDialog_EnableContinueMenu &FALSE
        Call WriteObjectTypeinTitle D@hwnd, {B$ "Delphi Kilyx 3" EOS}
        Call 'USER32.MessageBoxA' D@hwnd, {B$ "Delphi Kilyx3 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet." EOS}, {B$ 'LibScan' EOS},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DCU2_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        Call LibScanDialog_EnableContinueMenu &FALSE
        Call WriteObjectTypeinTitle D@hwnd, {B$ "Delphi 2" EOS}
        Call 'USER32.MessageBoxA' D@hwnd, {B$ "Delphi 2 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet." EOS}, {B$ 'LibScan' EOS},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DCU3_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        Call LibScanDialog_EnableContinueMenu &FALSE
        Call WriteObjectTypeinTitle D@hwnd, {B$ "Delphi 3" EOS}
        Call 'USER32.MessageBoxA' D@hwnd, {B$ "Delphi 3 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet." EOS}, {B$ 'LibScan' EOS},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DCU4_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        Call LibScanDialog_EnableContinueMenu &FALSE
        Call WriteObjectTypeinTitle D@hwnd, {B$ "Delphi 4" EOS}
        Call 'USER32.MessageBoxA' D@hwnd, {B$ "Delphi 4 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet." EOS}, {B$ 'LibScan' EOS},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DCU5_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        Call LibScanDialog_EnableContinueMenu &FALSE
        Call WriteObjectTypeinTitle D@hwnd, {B$ "Delphi 5" EOS}
        Call 'USER32.MessageBoxA' D@hwnd, {B$ "Delphi 5 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet." EOS}, {B$ 'LibScan' EOS},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DCU6_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        Call LibScanDialog_EnableContinueMenu &FALSE
        Call WriteObjectTypeinTitle D@hwnd, {B$ "Delphi 6" EOS}
        Call 'USER32.MessageBoxA' D@hwnd, {B$ "Delphi 6 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet." EOS}, {B$ 'LibScan' EOS},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DCU7_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        Call LibScanDialog_EnableContinueMenu &FALSE
        Call WriteObjectTypeinTitle D@hwnd, {B$ "Delphi 7" EOS}
        Call 'USER32.MessageBoxA' D@hwnd, {B$ "Delphi 7 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet." EOS}, {B$ 'LibScan' EOS},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = OMF_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        Call LibScanDialog_EnableContinueMenu &FALSE
        Call WriteObjectTypeinTitle D@hwnd, {B$ "OMF Object" EOS}
        Call 'USER32.MessageBoxA' D@hwnd, {B$ "Object Module Format (OMF) Signature was Found..", D$ CRLF2, B$ "Sorry, but OMF objects are not handled by this dumper yet." EOS}, {B$ 'LibScan' EOS},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = PDB_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        Call LibScanDialog_EnableContinueMenu &FALSE
        Call WriteObjectTypeinTitle D@hwnd, {B$ "Pdb File" EOS}
        Call 'USER32.MessageBoxA' D@hwnd, {B$ "Program DataBase (Pdb) Signature was Found..", D$ CRLF2, B$ "Sorry, but PDB files are not handled by this dumper yet." EOS}, {B$ 'LibScan' EOS},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DBG_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        Call LibScanDialog_EnableContinueMenu &FALSE
        Call WriteObjectTypeinTitle D@hwnd, {B$ "Dbg File" EOS}
        Call 'USER32.MessageBoxA' D@hwnd, {B$ "Degug Format (dbg) Signature was Found..", D$ CRLF2, B$ "Sorry, but dbg files are not handled by this dumper yet." EOS}, {B$ 'LibScan' EOS},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .End_If
    popad
EndP
____________________________________________________________________________________________

Proc CoffLibFound:

  ; Tag:
    add esi 8
   ____________________________________________
  ; First Lib Header:
    If W$esi = '/ '
        Push esi
            lea esi D$esi+48 | Call GetMemberSize
        Pop esi
        add esi 03C | add esi eax | On B$esi = 0A, inc esi
    End_If
   ____________________________________________
  ; Second Optional Lib Header:
    If W$esi = '/ '
        Push esi
            lea esi D$esi+48 | Call GetMemberSize
        Pop esi
        add esi 03C | add esi eax | On B$esi = 0A, inc esi
    End_If
   ____________________________________________
  ; Third optional Lib Header:
    If W$esi = '//'
        Push esi
            lea esi D$esi+48 | Call GetMemberSize
        Pop esi
        add esi 03C | add esi eax | On B$esi = 0A, inc esi
    End_If
   ____________________________________________
  ; Coff Files:
L5: Call InitCoffIndice
;;
;;
  We overwrite the "UserID", at +28. We will use this Ordinal to build Synthetic
  Labels (in replacement of the real Names), to save from Duplications, when we
  recompie the output, with RosAsm, for testing the validity of the Rendering
  (Asm Source Form version).

;;
;;
    While B$esi <> 0
        Mov eax D$LibObjIndice, D$esi+LIB_USER_ID eax
        Mov ax W$LibObjIndice+4, W$esi+LIB_USER_ID+4 ax

        Call IncrementLibCoffIndice

        Push esi
            lea esi D$esi+LIB_MEMBER_SIZE
            Call GetMemberSize
        Pop esi

        add esi COFF_HEADER_SIZE | add esi D$MemberSize
        On esi >= D$LibFileMemoryEnd, ExitP
        On B$esi = 0A, inc esi
    End_While

EndP

;;
;;
Proc LibCoffMarking:

  ; Valid Tag?
    Mov esi D$LibFileMemory

    .If D$esi+2 = 'arch'
        ; Valid .lib
        Mov D$ValidLib COFF_LIB_FILE

    .Else
        Call IsIMAGE_FILE_MACHINE D$esi

        If eax = &TRUE
          ; Valid .obj, if here:
            Mov D$ValidLib COFF_OBJ_FILE | ExitP

        Else
            Call 'USER32.MessageBoxA' 0, {B$ "The 'arch' Magic Signature Tag was not found.", D$ CRLF2, B$ "This is not a COFF Object File" EOS}, {B$ 'LibScan' EOS},
                                      &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL
            Mov D$ValidLib UNKNOWN_LIB_FILE | ExitP
        End_If

    .End_If
   ____________________________________________
  ; Tag:
    add esi 8
   ____________________________________________
  ; First Lib Header:
    If W$esi = '/ '
        Push esi
            lea esi D$esi+48 | Call GetMemberSize
        Pop esi
        add esi 03C | add esi eax | On B$esi = 0A, inc esi
    End_If
   ____________________________________________
  ; Second Optional Lib Header:
    If W$esi = '/ '
        Push esi
            lea esi D$esi+48 | Call GetMemberSize
        Pop esi
        add esi 03C | add esi eax | On B$esi = 0A, inc esi
    End_If
   ____________________________________________
  ; Third optional Lib Header:
    If W$esi = '//'
        Push esi
            lea esi D$esi+48 | Call GetMemberSize
        Pop esi
        add esi 03C | add esi eax | On B$esi = 0A, inc esi
    End_If
   ____________________________________________
  ; Coff Files:
L5: Call InitCoffIndice

;  We overwrite the "UserID", at +28. We will use this Ordinal to build Synthetic
 ; Labels (in replacement of the real Names), to save from Duplications, when we
  ;recompie the output, with RosAsm, for testing the validity of the Rendering
;  (Asm Source Form version).

    While B$esi <> 0
        Mov eax D$LibObjIndice, D$esi+LIB_USER_ID eax
        Mov ax W$LibObjIndice+4, W$esi+LIB_USER_ID+4 ax

        Call IncrementLibCoffIndice

        Push esi
            lea esi D$esi+LIB_MEMBER_SIZE
            Call GetMemberSize
        Pop esi

        add esi COFF_HEADER_SIZE | add esi D$MemberSize
        On esi >= D$LibFileMemoryEnd, ExitP
        On B$esi = 0A, inc esi
    End_While

EndP

;;

____________________________________________________________________________________________
;;
  To save from Duplications, the Ordinal Coff Indice are stored inside each Heasder
  upon the LIB_USER_ID.
  
  Before writing each Names, we Call for 'CopyCoffIndice'.
;;
;;
CopyCoffIndice:
    Mov D$edi 'Obj_'
    Mov eax D$esi+LIB_USER_ID, D$edi+4 eax,
        ax W$esi+LIB_USER_ID+4, W$edi+8 ax,
        D$edi+10 ' ; '
    add edi 13
ret

____________________________________________________________________________________________

;[@LibHexaMem: D$ ?    @COFFnumber: D$ ?    @Base: D$ ?]
Proc GetLibListing:
    Argument @hwnd, @OutPut, @OutputSize

    pushad

        On D$LookUpValidNameCharsTable = 0, Call InitLookUpValidNameCharsTable

        ; The Parsed Data will be stored in edi
        Mov edi D@OutPut

        Mov ecx D$LibFileLength | shl ecx 6

        Call VirtualAlloc edi,
                          ecx

        Call GetLongNamesBase

        Mov esi D$LibFileMemory, edi D$edi

        ____________________________________________
        ; RosAsm Notice
        Mov esi WriteNotice | While B$esi <> 0 | movsb | End_While
        ____________________________________________
        ; Tag:
        Call WriteLibTag

        Mov D$ARCHIVE_MEMBER_HEADER_Indice '0000', W$ARCHIVE_MEMBER_HEADER_Indice+4 '01'
       ____________________________________________
      ; First Lib Header:
        If W$esi = '/ '
            Call Write_IMAGE_ARCHIVE_MEMBER_HEADER_1
          ; First Lib Member:
            Push esi
                Call WriteHeaderMember1
            Pop esi
            add esi D$MemberSize | On B$esi = 0A, inc esi
        End_If
       ____________________________________________
      ; Second optional Lib Header:
        If W$esi = '/ '
            Call WriteSecondLibHeaderComment
            Call Write_IMAGE_ARCHIVE_MEMBER_HEADER_2
            Push esi
                Call WriteHeaderMember2
            Pop esi
            add esi D$MemberSize | On B$esi = 0A, inc esi
        End_If
       ____________________________________________
      ; Third optional Lib Header:
        If W$esi = '//'

            Call WriteThirdLibHeaderComment

            Call GetLongNamesPointer
            Call Write_IMAGE_ARCHIVE_MEMBER_HEADER_3
            Push esi
                Call WriteHeaderMember3
            Pop esi
            add esi D$MemberSize | On B$esi = 0A, inc esi
        End_If
       ____________________________________________
      ; Coff Files:
        Push esi
            Mov esi CoffComment
            While B$esi <> 0 | movsb | End_While
        Pop esi

        Call InitCoffIndice

        sub D$LibFileMemoryEnd 20

        While esi < D$LibFileMemoryEnd
            Call WriteCoffTitle
            Call Write_Obj_IMAGE_ARCHIVE_MEMBER_HEADER
            Call ParseLibObj
            ; To be used in the listview control
            Call AddListviewItem, D$H.List

            add esi D$MemberSize | On esi >= D$LibFileMemoryEnd, jmp L2>
            On B$esi = 0A, inc esi
            Call IncrementLibCoffIndice

        End_While

L2:
        ; The Parsed Data, must be displayed in the edit box
        Mov eax D@OutPut    ;   Initial Address of the parsed data
        Mov eax D$eax       ;   eax is now at the start of the address

        Call 'USER32.SetDlgItemTextA' D$H.TabDlg1, IDC_LIB_SOURCE, eax

        ; Computed and store the outputed size of the Parsed Data displayed in the edit control
        Mov eax D@OutputSize
        Mov ecx D@OutPut
        Mov ecx D$ecx
        sub edi ecx
        Mov D$eax edi

    popad

EndP

_________________________________________________________________________________________________

[hLibReportEdit: D$ ?];    @COFFnumber: D$ ?    @Base: D$ ?]
[hLibReportEditLength: D$ 0]

Proc GetOneCoffListing:
    Argument @hwnd, @OutPut, @OutputSize

    pushad

    ; The Parsed Data will be stored in edi
    Mov edi D@OutPut

    Call InitCoffIndice

    Mov ecx D$LibFileLength | shl ecx 6

    Call VirtualAlloc edi,
                      ecx

    Mov esi D$LibFileMemory, edi D$edi

    ____________________________________________
    ; RosAsm Notice
    Push esi
    Mov esi WriteNotice | While B$esi <> 0 | movsb | End_While
    Pop esi

    Call ParseLibObj

    Call ParseOpenedFilePath LibSaveFilter

    Move D$MemberSize D$LibFileLength
    Call AddListviewItem, D$H.List

    ; The Parsed Data, must be displayed in the edit box
    Mov eax D@OutPut    ;   Initial Address of the parsed data
    Mov eax D$eax       ;   eax is now at the start of the address
    Call 'USER32.SetDlgItemTextA' D$H.TabDlg1 , IDC_LIB_SOURCE, eax; SHOW ALL Parsed Data in the 1st Tab dialog...

    ; Computed and store the outpued size of the Parsed Data displayed in the edit control
    Mov eax D@OutputSize
    Mov ecx D@OutPut
    Mov ecx D$ecx
    sub edi ecx
    Mov D$eax edi

    popad
EndP

_________________________________________________________________________________________________

; This function is to be used for displaying the object file name in the list view

; Built this way to solve really weird file namings paths. This is similar to ParseLibObjectSymbolName


Proc ParseOpenedFilePath:
    Arguments @StartAddress
    Local @TempEndAddr, @TempStartAddr, @StartExportObjAddr, @EndObjPathAddr, @StartObjExtensionAddr, @UseExportedLibrary

    pushad

    ; Always clear the loaded data before use.
    Call ClearDwordBuffer ObjectName,
                          (256/DWORD)
    
    Call ClearDwordBuffer ObjExtensionStr,
                          (16/DWORD)
    
    Call ClearDwordBuffer ObjPathStr,
                          (256/DWORD)

    ; Initialize all the data
    Mov D$ObjFileNameType 0
    Mov D$UseObjPath 0
    Mov D$UseObjExtension 0
    Mov D@TempStartAddr 0
    Mov D@TempEndAddr 0
    Mov D@StartExportObjAddr 0
    Mov D@UseExportedLibrary 0

    Mov esi D@StartAddress

    While B$esi <> 0
        inc esi
    End_While
    Mov D@TempEndAddr esi

    ; 1st Step. Now that we have our full string, we need to see if it have any path inside.
    ; We must start from the end to the beginning of the string to find the path chars.
    Mov esi D@StartAddress
    Mov ecx D@TempEndAddr

    .While esi <> ecx

        If_Or B$ecx = '/', B$ecx = '\'

            Mov D@EndObjPathAddr ecx
            Mov D$UseObjPath 1
            jmp L1>
        End_If

        dec ecx
    .End_While

   L1:

    ; 2nd Step. If we have a Path, we will copy it to the proper Buffer (ObjPathStr)

    .If D$UseObjPath = 1
        ; Here esi is already defined as the starting point. We don't need to change it now.
        Mov ecx D@EndObjPathAddr
        Mov edi ObjPathStr

        .While esi <> ecx
            movsb
            ;inc esi
        .End_While
        inc esi ; Bypass the last "/" or "\" char preceeding the object name.
        Mov D@StartAddress esi ; Will points to the beginning of the object name only

    .End_If


    ; 3rd Step. Chekc the file name Type. If the len of the object name is equal or bigger then 16 bytes, it is a LongName.
    ; Otherwise it is short name.

    Mov eax D@TempEndAddr
    sub eax D@StartAddress

    If eax <= 16
        Mov D$ObjFileNameType 0
    Else
        Mov D$ObjFileNameType 1
    End_If



    ; 4th Step. At this point we have only the name of the object and it's extension (if any).
    ; So we must find and copy the object extension.

    Mov esi D@StartAddress
    Mov ecx D@TempEndAddr
    Mov edi ObjectName

    .Do

        .If B$esi = '.'
            Mov D$UseObjExtension 1
            Mov edi ObjExtensionStr
            inc esi ; Bypass the "." char
            While esi <> ecx
                movsb
            End_While
            jmp L2>
        .End_If

        movsb
    .Loop_Until esi = ecx

   L2:

    popad
EndP

_________________________________________________________________________________________________


[CoffListingTitle: B$ "
; -------------------- Coff Listing: -------------------------

[IMAGE_FILE_HEADER_" EOS]

[ObjMachineText: B$ 'Machine: W$ ' EOS]
[ObjNumberOfSectionsText: B$ 'NumberOfSections: W$ ' EOS]
[ObjTimeDateStampText: B$ 'TimeDateStamp: D$ ' EOS]
[ObjPointerToSymbolTableText: B$ 'PointerToSymbolTable: D$ ' EOS]
[ObjPbjNumberOfSymbolsText: B$ 'NumberOfSymbols: D$ ' EOS]
[ObjSizeOfOptionalHeaderText: B$ 'SizeOfOptionalHeader: W$ ' EOS]
[ObjCharacteristicsText: B$ 'Characteristics: W$ ' EOS]
[SymbolTableText: B$ 'ImgSym000001 - IMAGE_FILE_HEADER_' EOS]



[SizeOfOptionalHeaderInObj: D$ ?
 ObjNumberOfSections: D$ ?
 FirstSectionPointer: D$ ?]

GetCoffIMAGE_FILE_HEADER:
    Push esi
        Mov esi CoffListingTitle
        While B$esi <> 0 | movsb | End_While
        Call WriteIndiceOnly
        Mov B$edi ':', W$edi+1 CRLF | add edi 3
    Pop esi

    ; To be used in AddListviewItem
    Push esi
        Mov D$LvOffsetCOFF 0 ; always initialize at 0 1st, due to the several loopings for each object.
        sub esi D$LibFileMemory
        Mov D$LvOffsetCOFF esi
    Pop esi

    Call WriteLibImportObjItem ObjMachineText, &FALSE | sub edi 2
        ; To be used in AddListviewItem
        Mov W$CoffMachineType 0 ; always initialize at 0 1st, due to the several loopings for each object.
        Move W$CoffMachineType W$esi
    Call Write_IMAGE_FILE_MACHINE D$esi | Mov W$edi CRLF | add edi 2

    add esi 2 | movzx eax W$esi | Mov D$ObjNumberOfSections eax

    Call WriteLibImportObjItem ObjNumberOfSectionsText, &TRUE

    add esi 2 | Mov eax D$esi
    Call WriteLibImportObjItem ObjTimeDateStampText, &TRUE
    Call WriteLinkerMemberTimeDateStamp D$esi

    add esi 4 | Mov eax D$esi
    Mov D$CoffSymbolsPointer eax ; To be used in WriteImageSymbolTable
    Call WriteLibImportObjItem ObjPointerToSymbolTableText, &FALSE | sub edi 2

    If eax <> 0
        Call WriteLibImportObjItem SymbolTableText, &FALSE | sub edi 2
        Call WriteIndiceOnly
        Push esi
            Mov esi {B$ ' ; Hex Value:  ' EOS}
            While B$esi <> 0 | movsb | End_While
        Pop esi
    End_If

    Call Writeeax | Mov W$edi CRLF | add edi 2 ; Write the Value of the SymbolTable Pointer in Hexadecimal string


    add esi 4 | Mov eax D$esi
    Mov D$CoffSymbolsNumber eax ; To be used in WriteImageSymbolTable
    Mov D$ObjSymbolsNumber eax ; To be used in AddListviewItem
    Call WriteLibImportObjItem ObjPbjNumberOfSymbolsText, &TRUE

    add esi 4 | movzx eax W$esi | Mov D$SizeOfOptionalHeaderInObj eax
    Call WriteLibImportObjItem ObjSizeOfOptionalHeaderText, &TRUE

    add esi 2 | movzx eax W$esi
    Call WriteLibImportObjItem ObjCharacteristicsText, &TRUE | sub edi 2
    movzx eax W$esi
    If eax <> 0
        Call WriteObjCharacteristics
    End_If

    Mov W$edi CRLF
    Mov B$edi+2 ']'
    Mov D$edi+3 CRLF2 | add edi 7

    add esi 2
ret


[CoffListingIMAGE_OPTIONAL_HEADERTitle: B$ "[IMAGE_OPTIONAL_HEADER_" EOS

 ImgOptHdr.Magic: W$'ImgOptHdr.Magic: W$ ' EOS ; !!! W$ -> U$...
 ImgOptHdr.MajorLinkerVersion: 'ImgOptHdr.MajorLinkerVersion: B$ ' EOS
 ImgOptHdr.MinorLinkerVersion: 'ImgOptHdr.MinorLinkerVersion: B$ ' EOS
 ImgOptHdr.SizeOfCode: 'ImgOptHdr.SizeOfCode: D$ ' EOS
 ImgOptHdr.SizeOfInitializedData: 'ImgOptHdr.SizeOfInitializedData: D$ ' EOS
 ImgOptHdr.SizeOfUninitializedData: 'ImgOptHdr.SizeOfUninitializedData: D$ ' EOS
 ImgOptHdr.AddressOfEntryPoint: 'ImgOptHdr.AddressOfEntryPoint: D$ ' EOS
 ImgOptHdr.BaseOfCode: 'ImgOptHdr.BaseOfCode: D$ ' EOS
 ImgOptHdr.BaseOfData: 'ImgOptHdr.BaseOfData: D$ ' EOS
 ImgOptHdr.ImageBase: 'ImgOptHdr.ImageBase: D$ ' EOS
 ImgOptHdr.SectionAlignment: 'ImgOptHdr.SectionAlignment: D$ ' EOS
 ImgOptHdr.FileAlignment: 'ImgOptHdr.FileAlignment: D$ ' EOS
 ImgOptHdr.MajorOperatingSystemVersion: 'ImgOptHdr.MajorOperatingSystemVersion: W$ ' EOS
 ImgOptHdr.MinorOperatingSystemVersion: 'ImgOptHdr.MinorOperatingSystemVersion: W$ ' EOS
 ImgOptHdr.MajorImageVersion: 'ImgOptHdr.MajorImageVersion: W$ ' EOS
 ImgOptHdr.MinorImageVersion: 'ImgOptHdr.MinorImageVersion: W$ ' EOS
 ImgOptHdr.MajorSubsystemVersion: 'ImgOptHdr.MajorSubsystemVersion: W$ ' EOS
 ImgOptHdr.MinorSubsystemVersion: 'ImgOptHdr.MinorSubsystemVersion: W$ ' EOS
 ImgOptHdr.Win32VersionValue: 'ImgOptHdr.Win32VersionValue: D$ ' EOS
 ImgOptHdr.SizeOfImage: 'ImgOptHdr.SizeOfImage: D$ ' EOS
 ImgOptHdr.SizeOfHeaders: 'ImgOptHdr.SizeOfHeaders: D$ ' EOS
 ImgOptHdr.CheckSum: 'ImgOptHdr.CheckSum: D$ ' EOS
 ImgOptHdr.Subsystem: 'ImgOptHdr.Subsystem: W$ ' EOS
 ImgOptHdr.DllCharacteristics: 'ImgOptHdr.DllCharacteristics: W$ ' EOS
 ImgOptHdr.SizeOfStackReserve: 'ImgOptHdr.SizeOfStackReserve: D$ ' EOS
 ImgOptHdr.SizeOfStackCommit: 'ImgOptHdr.SizeOfStackCommit: D$ ' EOS
 ImgOptHdr.SizeOfHeapReserve: 'ImgOptHdr.SizeOfHeapReserve: D$ ' EOS
 ImgOptHdr.SizeOfHeapCommit: 'ImgOptHdr.SizeOfHeapCommit: D$ ' EOS
 ImgOptHdr.LoaderFlags: 'ImgOptHdr.LoaderFlags: D$ ' EOS
 ImgOptHdr.NumberOfRvaAndSizes: 'ImgOptHdr.NumberOfRvaAndSizes: D$ ' EOS

 ImgDataDir.Export: 'ImgDataDir.Export: D$ ' EOS
 ImgDataDir.ExportSize: 'ImgDataDir.ExportSize: D$ ' EOS
 ImgDataDir.Import: 'ImgDataDir.Import: D$ ' EOS
 ImgDataDir.ImportSize: 'ImgDataDir.ImportSize: D$ ' EOS
 ImgDataDir.Resource: 'ImgDataDir.Resource: D$ ' EOS
 ImgDataDir.ResourceSize: 'ImgDataDir.ResourceSize: D$ ' EOS
 ImgDataDir.Exception: 'ImgDataDir.Exception: D$ ' EOS
 ImgDataDir.ExceptionSize: 'ImgDataDir.ExceptionSize: D$ ' EOS
 ImgDataDir.Certificate: 'ImgDataDir.Certificate: D$ ' EOS
 ImgDataDir.CertificateSize: 'ImgDataDir.CertificateSize: D$ ' EOS
 ImgDataDir.Relocation: 'ImgDataDir.Relocation: D$ ' EOS
 ImgDataDir.RelocationSize: 'ImgDataDir.RelocationSize: D$ ' EOS
 ImgDataDir.Debug: 'ImgDataDir.Debug: DebugDir: D$ ' EOS
 ImgDataDir.DebugSize: 'ImgDataDir.DebugSize: D$ ' EOS
 ImgDataDir.Architecture: 'ImgDataDir.Architecture: D$ ' EOS
 ImgDataDir.ArchitectureSize: 'ImgDataDir.ArchitectureSize: D$ ' EOS
 ImgDataDir.GPReg: 'ImgDataDir.GPReg: D$ ' EOS
 ImgDataDir.GPRegSize: 'ImgDataDir.GPRegSize: D$ ' EOS
 ImgDataDir.Thread: 'ImgDataDir.Thread: D$ ' EOS
 ImgDataDir.ThreadSize: 'ImgDataDir.ThreadSize: D$ ' EOS
 ImgDataDir.ConfigTable: 'ImgDataDir.ConfigTable: D$ ' EOS
 ImgDataDir.ConfigTableSize: 'ImgDataDir.ConfigTableSize: D$ ' EOS
 ImgDataDir.BoundIAT: 'ImgDataDir.BoundIAT: D$ ' EOS
 ImgDataDir.BoundIATSize: 'ImgDataDir.BoundIATSize: D$ ' EOS
 ImgDataDir.IAT: 'ImgDataDir.IAT: D$ ' EOS
 ImgDataDir.IATSize: 'ImgDataDir.IATSize: D$ ' EOS
 ImgDataDir.DelayID: 'ImgDataDir.DelayID: D$ ' EOS
 ImgDataDir.DelayIDSize: 'ImgDataDir.DelayIDSize: D$ ' EOS
 ImgDataDir.COM: 'ImgDataDir.COM: D$ ' EOS
 ImgDataDir.COMSize: 'ImgDataDir.COMSize: D$ ' EOS
 ImgDataDir.Reserved: 'ImgDataDir.Reserved: D$ ' EOS
 ImgDataDir.ReservedSize: 'ImgDataDir.ReservedSize: D$ ' EOS]

[IMAGE_DATA_DIRECTORY_Title: B$ '; ---- IMAGE_DATA_DIRECTORY ----' EOS]


Write_IMAGE_DATA_DIRECTORY_Title:
    
    Mov W$edi CRLF | add edi 2

    Push esi | Mov esi IMAGE_DATA_DIRECTORY_Title | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi
                 
    Mov D$edi CRLF2 | add edi 4

ret


WriteObjMagic:
    Push esi
        If eax = &IMAGE_NT_OPTIONAL_HDR32_MAGIC

            Push esi | Mov esi {B$ ' ; &IMAGE_NT_OPTIONAL_HDR32_MAGIC' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Else_If eax = &IMAGE_NT_OPTIONAL_HDR64_MAGIC

            Push esi | Mov esi {B$ ' ; &IMAGE_NT_OPTIONAL_HDR64_MAGIC' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Else

            Push esi | Mov esi {B$ ' ; Likely corrupted unknown Magic Value' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        End_If
    Pop esi

    Mov W$edi CRLF | add edi 2
ret


WriteObjSubSystem:
    Push esi
        .If eax = &IMAGE_SUBSYSTEM_UNKNOWN
 
            Push esi | Mov esi {B$ ' ; &IMAGE_SUBSYSTEM_UNKNOWN' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SUBSYSTEM_NATIVE
  
            Push esi | Mov esi {B$ ' ; &IMAGE_SUBSYSTEM_NATIVE' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SUBSYSTEM_WINDOWS_GUI
 
            Push esi | Mov esi {B$ ' ; &IMAGE_SUBSYSTEM_WINDOWS_GUI' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SUBSYSTEM_WINDOWS_CUI
 
            Push esi | Mov esi {B$ ' ; &IMAGE_SUBSYSTEM_WINDOWS_CUI' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SUBSYSTEM_OS2_CUI
 
            Push esi | Mov esi {B$ ' ; &IMAGE_SUBSYSTEM_OS2_CUI' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SUBSYSTEM_POSIX_CUI
 
            Push esi | Mov esi {B$ ' ; &IMAGE_SUBSYSTEM_POSIX_CUI' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SUBSYSTEM_WINDOWS_CE_GUI
 
            Push esi | Mov esi {B$ ' ; &IMAGE_SUBSYSTEM_WINDOWS_CE_GUI' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SUBSYSTEM_EFI_APPLICATION

            Push esi | Mov esi {B$ ' ; &IMAGE_SUBSYSTEM_EFI_APPLICATION' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER
 
            Push esi | Mov esi {B$ ' ; &IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER

            Push esi | Mov esi {B$ ' ; &IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SUBSYSTEM_NATIVE_WINDOWS
  
            Push esi | Mov esi {B$ ' ; &IMAGE_SUBSYSTEM_NATIVE_WINDOWS' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else

            Push esi | Mov esi {B$ ' ; Likely corrupted unknown SubSystem Record' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .End_If
    Pop esi

    Mov W$edi CRLF | add edi 2
ret


WriteDllCharacteristics:
    .If eax <> 0
        Push esi
            Mov D$edi ' ; ' | add edi 3

            test eax &IMAGE_DLLCHARACTERISTICS_PPROCESS_LIB_INIT | jz L1>
               
                Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_PPROCESS_LIB_INIT__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1:         test eax &IMAGE_DLLCHARACTERISTICS_PPROCESS_LIB_TERM | jz L1>

                Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_PPROCESS_LIB_TERM__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1:         test eax &IMAGE_DLLCHARACTERISTICS_PTHREAD_LIB_INIT | jz L1>

                Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_PTHREAD_LIB_INIT__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1:         test eax &IMAGE_DLLCHARACTERISTICS_PTHREAD_LIB_TERM | jz L1>

                Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_PTHREAD_LIB_TERM__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1:         test eax &IMAGE_DLLCHARACTERISTICS_NO_BIND | jz L1>

                Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_NO_BIND__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1:         test eax &IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE | jz L1>

                Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1:         test eax &IMAGE_DLLCHARACTERISTICS_WDM_DRIVER | jz L1>

                Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1:         While B$edi = '_' | dec edi | End_While
        Pop esi
    .End_If

    Mov W$edi CRLF | add edi 2
ret


GetCoffIMAGE_OPTIONAL_HEADER:
    Push esi
        Mov esi CoffListingIMAGE_OPTIONAL_HEADERTitle
        While B$esi <> EOS | movsb | End_While
        Call WriteIndiceOnly
        Mov B$edi ':', W$edi+1 CRLF | add edi 3
    Pop esi

    movzx eax W$esi
    Push eax
    Call WriteLibImportObjItem ImgOptHdr.Magic, &TRUE | sub edi 2
    Pop eax
    Call WriteObjMagic

    add esi 2 | movzx eax B$esi
    Call WriteLibImportObjItem ImgOptHdr.MajorLinkerVersion, &TRUE

    inc esi | movzx eax B$esi
    Call WriteLibImportObjItem ImgOptHdr.MinorLinkerVersion, &TRUE

    inc esi
    lodsd | Call WriteLibImportObjItem ImgOptHdr.SizeOfCode, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.SizeOfInitializedData, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.SizeOfUninitializedData, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.AddressOfEntryPoint, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.BaseOfCode, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.BaseOfData, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.ImageBase, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.SectionAlignment, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.FileAlignment, &TRUE

    lodsw | and eax 0FFFF
    Call WriteLibImportObjItem ImgOptHdr.MajorOperatingSystemVersion, &TRUE

    lodsw | and eax 0FFFF
    Call WriteLibImportObjItem ImgOptHdr.MinorOperatingSystemVersion, &TRUE

    lodsw | and eax 0FFFF
    Call WriteLibImportObjItem ImgOptHdr.MajorImageVersion, &TRUE

    lodsw | and eax 0FFFF
    Call WriteLibImportObjItem ImgOptHdr.MinorImageVersion, &TRUE

    lodsw | and eax 0FFFF
    Call WriteLibImportObjItem ImgOptHdr.MajorSubsystemVersion, &TRUE

    lodsw | and eax 0FFFF
    Call WriteLibImportObjItem ImgOptHdr.MinorSubsystemVersion, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.Win32VersionValue, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.SizeOfImage, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.SizeOfHeaders, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.CheckSum, &TRUE

    lodsw | and eax 0FFFF
    Push eax
    Call WriteLibImportObjItem ImgOptHdr.Subsystem, &TRUE | sub edi 2
    Pop eax
    Call WriteObjSubSystem

    lodsw | and eax 0FFFF
    Push eax
    Call WriteLibImportObjItem ImgOptHdr.DllCharacteristics, &TRUE | sub edi 2
    Pop eax
    Call WriteDllCharacteristics

    lodsd | Call WriteLibImportObjItem ImgOptHdr.SizeOfStackReserve, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.SizeOfStackCommit, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.SizeOfHeapReserve, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.SizeOfHeapCommit, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.LoaderFlags, &TRUE

    lodsd | Call WriteLibImportObjItem ImgOptHdr.NumberOfRvaAndSizes, &TRUE


    Call Write_IMAGE_DATA_DIRECTORY_Title


    lodsd | Call WriteLibImportObjItem ImgDataDir.Export, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.ExportSize, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.Import, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.ImportSize, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.Resource, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.ResourceSize, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.Exception, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.ExceptionSize, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.Certificate, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.CertificateSize, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.Relocation, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.RelocationSize, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.Debug, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.DebugSize, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.Architecture, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.ArchitectureSize, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.GPReg, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.GPRegSize, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.Thread, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.ThreadSize, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.ConfigTable, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.ConfigTableSize, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.BoundIAT, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.BoundIATSize, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.IAT, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.IATSize, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.DelayID, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.DelayIDSize, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.COM, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.COMSize, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.Reserved, &TRUE

    lodsd | Call WriteLibImportObjItem ImgDataDir.ReservedSize, &TRUE

    sub edi 2 | Mov B$edi ']', D$edi+1 CRLF2 | add edi 5

ret
____________________________________________________________________________________________



[CoffSectionHeaderTitle: "
;;

;;
_____________________________________________________

The Section Header
_____________________________________________________
_____________________________________________________

IMAGE_SECTION_HEADER Structure

;;
;;

" EOS]

[SectionHeaderNumber: B$ '001' EOS]

InitSectionHeaderNumber:
    Mov D$SectionHeaderNumber '001'
ret



IncrementSectionHeaderNumber:

    Push ebx
        lea ebx D$SectionHeaderNumber+2

        inc B$ebx
        While B$ebx > '9'
            Mov B$ebx '0' | dec ebx | inc B$ebx
        End_While
    Pop ebx
ret

[CoffSectionHeaderName: B$ 'Name1' EOS]
[CoffSectionHeaderVirtualSize: B$ 'MiscVirtualSize' EOS]
[CoffSectionHeaderRVA: B$ 'VirtualAddress' EOS]
[CoffSectionHeaderSize: B$ 'SizeOfRawData' EOS]
[CoffSectionHeaderPointerToData: B$ 'PointerToRawData' EOS]
[CoffSectionHeaderPointerToReloc: B$ 'PointerToRelocations' EOS]
[CoffSectionHeaderPointerToLinesNumbers: B$ 'PointerToLinenumbers' EOS]
[CoffSectionHeaderNumberOfRelocations: B$ 'NumberOfRelocations' EOS]
[CoffSectionHeaderNumberOfLinesNumbers: B$ 'NumberOfLinenumbers' EOS]
[CoffSectionHeaderCharacteristics: B$ 'Characteristics' EOS]

Proc WriteObjSectionHeaderItem:
    Argument @Text1, @Text2

        Push esi
            Call WriteObjIndice
           
            Mov esi {B$ 'ImgSecHdr' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0:
            Mov esi SectionHeaderNumber | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0:

            Mov B$edi '.' | inc edi
        
            Mov esi D@Text1 | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0:

            Mov esi D@Text2 | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0:

        Pop esi

        Mov eax D@Text2, al B$eax+2

        ...If al = 'B'
            Mov edx esi | add edx 8
            Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:         Mov B$edi "'" | inc edi
            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ...Else_If al = 'W'
            lodsw | and eax 0FFFF | Call WriteEax

        ...Else

            ..If D$esi <> 0
                Mov eax D@Text1

                .If D$eax = 'Size' ; from "SizeOfRawData" string

                    Call WriteSectionHeaderRawSizeDiffLabel
                    
                    Push esi | Mov esi {B$ " ; Hex Value:  " EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

                .Else_If D$eax+8 = 'oRaw'; From "PointerToRawData" string
                    ;Mov eax D$esi-4
                    If D$esi-4 = 0
                        Mov D$esi 0
                    Else

                        Call WriteSectionHeaderPointerToDataDiffLabel
                
                        Push esi | Mov esi {B$ " ; Hex Value:  " EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

                    End_If
                .Else_If D$eax+9 = 'Relo'; From "PointerToRelocations" string
                    Call InitSectionRelocNumber
                    ;movzx eax W$esi+4+4
                    ; Fix the value at esi when the Reloc Number is 0, but it is pointing somewhere
                    If W$esi+4+4 = 0
                        Mov D$esi 0
                    Else
                        Call WriteSectionHeaderPointerToRelocDiffLabel
                
                        Push esi | Mov esi {B$ " ; Hex Value:  " EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

                    End_If
                .Else_If D$eax+9 = 'Line'; From "PointerToLinenumbers" string
                    Call InitSectionLineNumber
                    ;movzx eax W$esi+4+2
                    If W$esi+4+2 = 0
                        Mov D$esi 0
                    Else
                        Call WriteSectionHeaderPointerToLineNumberDiffLabel
                        
                        Push esi | Mov esi {B$ " ; Hex Value:  " EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

                    End_If

;;
;;
                Else_If D$eax = 'Misc'; From "MiscVirtualSize" string

                    Call WriteSectionHeaderVirtualSizeDiffLabel
                    Push esi | zcopy {B$ " ; Hex Value:  " EOS} | Pop esi
;;
;;

                .End_If

            ..End_If

            lodsd | Call WriteEax

        ...End_If

        Mov W$edi CRLF | add edi 2
EndP


WriteCharacteristicsEquates:

    If eax <> 0
        Mov D$edi ' ; ' | add edi 3
    End_If

    test eax &IMAGE_SCN_TYPE_REG | jz L1>
      
        Push esi | Mov esi {B$ '&IMAGE_SCN_TYPE_REG__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_TYPE_DSECT | jz L1>
       
        Push esi | Mov esi {B$ '&IMAGE_SCN_TYPE_DSECT__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_TYPE_NOLOAD | jz L1>
        
        Push esi | Mov esi {B$ '&IMAGE_SCN_TYPE_NOLOAD__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_TYPE_GROUP | jz L1>
        
        Push esi | Mov esi {B$ '&IMAGE_SCN_TYPE_GROUP__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_TYPE_NO_PAD | jz L1>
        
        Push esi | Mov esi {B$ '&IMAGE_SCN_TYPE_NO_PAD__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_TYPE_COPY | jz L1>
        
        Push esi | Mov esi {B$ '&IMAGE_SCN_TYPE_COPY__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_CNT_CODE | jz L1>
        
        Push esi | Mov esi {B$ '&IMAGE_SCN_CNT_CODE__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_CNT_INITIALIZED_DATA | jz L1>
        
        Push esi | Mov esi {B$ '&IMAGE_SCN_CNT_INITIALIZED_DATA__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_CNT_UNINITIALIZED_DATA | jz L1>
        
        Push esi | Mov esi {B$ '&IMAGE_SCN_CNT_UNINITIALIZED_DATA__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_LNK_OTHER | jz L1>
        
        Push esi | Mov esi {B$ '&IMAGE_SCN_LNK_OTHER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_LNK_INFO | jz L1>
        
        Push esi | Mov esi {B$ '&IMAGE_SCN_LNK_INFO__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_TYPE_OVER | jz L1>
        
        Push esi | Mov esi {B$ '&IMAGE_SCN_TYPE_OVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_LNK_REMOVE | jz L1>
        
        Push esi | Mov esi {B$ '&IMAGE_SCN_LNK_REMOVE__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_LNK_COMDAT | jz L1>
        zCopy {B$ '&IMAGE_SCN_LNK_COMDAT__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_MEM_FARDATA | jz L1>
        zCopy {B$ '&IMAGE_SCN_MEM_FARDATA__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_MEM_PURGEABLE | jz L1>
        zCopy {B$ '&IMAGE_SCN_MEM_PURGEABLE__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_MEM_16BIT | jz L1>
        zCopy {B$ '&IMAGE_SCN_MEM_16BIT__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_MEM_LOCKED | jz L1>
        zCopy {B$ '&IMAGE_SCN_MEM_LOCKED__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_MEM_PRELOAD | jz L1>
        zCopy {B$ '&IMAGE_SCN_MEM_PRELOAD__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: Mov ebx eax | and ebx 0FF_0000

    .If ebx = &IMAGE_SCN_ALIGN_1BYTES                ; 010_0000
        zCopy {B$ '&IMAGE_SCN_ALIGN_1BYTES__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If ebx = &IMAGE_SCN_ALIGN_2BYTES           ; 020_0000
        zCopy {B$ '&IMAGE_SCN_ALIGN_2BYTES__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If ebx = &IMAGE_SCN_ALIGN_4BYTES           ; 030_0000
        zCopy {B$ '&IMAGE_SCN_ALIGN_4BYTES__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If ebx = &IMAGE_SCN_ALIGN_8BYTES           ; 040_0000
        zCopy {B$ '&IMAGE_SCN_ALIGN_8BYTES__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If ebx = &IMAGE_SCN_ALIGN_16BYTES          ; 050_0000
        zCopy {B$ '&IMAGE_SCN_ALIGN_16BYTES__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If ebx = &IMAGE_SCN_ALIGN_32BYTES          ; 060_0000
        zCopy {B$ '&IMAGE_SCN_ALIGN_32BYTES__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If ebx = &IMAGE_SCN_ALIGN_64BYTES          ; 070_0000
        zCopy {B$ '&IMAGE_SCN_ALIGN_64BYTES__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If ebx = &IMAGE_SCN_ALIGN_128BYTES         ; 080_0000
        zCopy {B$ '&IMAGE_SCN_ALIGN_128BYTES__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If ebx = &IMAGE_SCN_ALIGN_256BYTES         ; 090_0000
        zCopy {B$ '&IMAGE_SCN_ALIGN_256BYTES__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If ebx = &IMAGE_SCN_ALIGN_512BYTES         ; 0A0_0000
        zCopy {B$ '&IMAGE_SCN_ALIGN_512BYTES__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If ebx = &IMAGE_SCN_ALIGN_1024BYTES        ; 0B0_0000
        zCopy {B$ '&IMAGE_SCN_ALIGN_1024BYTES__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If ebx = &IMAGE_SCN_ALIGN_2048BYTES        ; 0C0_0000
        zCopy {B$ '&IMAGE_SCN_ALIGN_2048BYTES__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If ebx = &IMAGE_SCN_ALIGN_4096BYTES        ; 0D0_0000
        zCopy {B$ '&IMAGE_SCN_ALIGN_4096BYTES__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If ebx = &IMAGE_SCN_ALIGN_8192BYTES        ; 0E0_0000
        zCopy {B$ '&IMAGE_SCN_ALIGN_8192BYTES__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .End_If

L1: test eax &IMAGE_SCN_LNK_NRELOC_OVFL | jz L1>
        zCopy {B$ '&IMAGE_SCN_LNK_NRELOC_OVFL__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_MEM_DISCARDABLE | jz L1>
        zCopy {B$ '&IMAGE_SCN_MEM_DISCARDABLE__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_MEM_NOT_CACHED | jz L1>
        zCopy {B$ '&IMAGE_SCN_MEM_NOT_CACHED__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_MEM_NOT_PAGED | jz L1>
        zCopy {B$ '&IMAGE_SCN_MEM_NOT_PAGED__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_MEM_SHARED | jz L1>
        zCopy {B$ '&IMAGE_SCN_MEM_SHARED__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_MEM_EXECUTE | jz L1>
        zCopy {B$ '&IMAGE_SCN_MEM_EXECUTE__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_MEM_READ | jz L1>
        zCopy {B$ '&IMAGE_SCN_MEM_READ__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1: test eax &IMAGE_SCN_MEM_WRITE | jz L1>
        zCopy {B$ '&IMAGE_SCN_MEM_WRITE__' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

L1:

    While B$edi-1 = '_' | dec edi | End_While
ret



____________________________________________________________________________________________

[MemberName1Offset: D$ ?]

Proc GetName1Offset:
    pushad
        ; edi  is used as a Byte Counter
        Mov edi 8 ; Size of the Name1 member
        inc esi ; ByPass the 1st '/'
        Mov ecx 10, ebx 0, eax 0

        Do
            On B$esi = 0, jmp L0> ; If we reach 0 jmp over
            On B$esi = SPC, jmp L0> ; If we reach SPC jmp over
            mul ecx
            Push eax
                lodsb | sub al '0' | Mov bl al
            Pop eax
            add eax ebx
            dec edi
        Loop_Until edi = 0

L0:

        Mov D$MemberName1Offset eax

    popad
EndP

____________________________________________________________________________________________



Proc WriteSectionNameStringTablePointerComment:
    Uses esi, eax, edx


    ; Initialize the String Record Counter
    Call InitStringTableRecord

    ; eax is pointing to the Offset of the String on the String Table
    Mov eax D$MemberName1Offset
    Mov edx D$PointerToStringTable
    Mov esi edx ; esi is pointing to the Beginning of the String Table
    add edx eax ; edx will be using as a counter of the total amount of strings untill we reach the String where we are.
    add esi 4   ; Bypass the String Table Size. and Point to the beginning of the 1st String

    .Do

        While B$esi <> 0 | inc esi | End_While | inc esi | On B$esi = 0, Mov edx 0

        Call IncrementStringTableRecord

    .Loop_Until esi >= edx


        Push esi
            Call WriteObjIndice
            zCopy {B$ 'StringData' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy StringTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi


    ; Restore the Value of StringTableRecord

    Call InitStringTableRecord


EndP

___________________________________________

[SectionNameIndirectMessage: B$ "________________________________________________________________________

; Section Name indirectly addressed in Name1 member.
; The name of the Section is: " EOS]


Proc WriteIndirectSectionName:
    Uses esi

        Call GetName1Offset ; convert the Offset to hexadecimal
        Call GetStringTablePointer

        ;zCopy SymbolTableTitle
        Mov esi D$CoffSectionBase
        add esi D$CoffSymbolsPointer

        Push esi
            zCopy SectionNameIndirectMessage
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi

            Mov ecx D$PointerToStringTable

        Push esi
            add ecx D$MemberName1Offset | Mov esi ecx
            While B$esi <> 0 | movsb | End_While
        Pop esi

        Push esi
            zCopy {W$ CRLF, B$ "; Referenced in String Table data at: " EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Call WriteSectionNameStringTablePointerComment
            zCopy {W$ CRLF, B$ "________________________________________________________________________", D$ CRLF2 0}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi

EndP

___________________________________________________
;;
;;

Section Name    Content                     Characteristics
.arch           Alpha architecture info     &IMAGE_SCN_MEM_READ__&IMAGE_SCN_CNT_INITIALIZED_DATA (...) &IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_DISCARDABLE
.bss            Uninitialized data          &IMAGE_SCN_CNT_UNINITIALIZED_DATA__&IMAGE_SCN_MEM_READ (...) &IMAGE_SCN_MEM_WRITE
.data           Initialized data            &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ (...) &IMAGE_SCN_MEM_WRITE
.edata          Export tables               &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ
.idata          Import tables               &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.pdata          Exception information       &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ
.rdata          Read-only initialized data  &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ
.reloc          Image relocations           &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ (...) &IMAGE_SCN_MEM_DISCARDABLE
.rsrc           Resource directory          &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ (...) &IMAGE_SCN_MEM_WRITE
.text           Executable code             &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_MEM_EXECUTE (...) &IMAGE_SCN_MEM_READ
.tls            Thread-local storage        &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ (...) &IMAGE_SCN_MEM_WRITE
.xdata          Exception information       &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ


&IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE = 0E0000040

&IMAGE_SCN_CNT_UNINITIALIZED_DATA__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE = 0E0000080

Some common Names and their Flag values:

.bss C0300080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.bss C0301080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.bss C0400080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA__&IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.bss C0401080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.bss C0500080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA__&IMAGE_SCN_ALIGN_16BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.CRT C0300040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.data C0300040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.data C0301040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.data C0400040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.data C0401040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.data C0500040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_16BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.debug 42000048 &IMAGE_SCN_TYPE_NO_PAD__&IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_DISCARDABLE__&IMAGE_SCN_MEM_READ
.debug 42100040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_1BYTES__&IMAGE_SCN_MEM_DISCARDABLE__&IMAGE_SCN_MEM_READ
.debug 42100048 &IMAGE_SCN_TYPE_NO_PAD__&IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_1BYTES__&IMAGE_SCN_MEM_DISCARDABLE__&IMAGE_SCN_MEM_READ
.debug 42101040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_1BYTES__&IMAGE_SCN_MEM_DISCARDABLE__&IMAGE_SCN_MEM_READ
.debug 42101048 &IMAGE_SCN_TYPE_NO_PAD__&IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_1BYTES__&IMAGE_SCN_MEM_DISCARDABLE__&IMAGE_SCN_MEM_READ
.debug 42400048 &IMAGE_SCN_TYPE_NO_PAD__&IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_DISCARDABLE__&IMAGE_SCN_MEM_READ
.drectve 100A00 &IMAGE_SCN_LNK_INFO__&IMAGE_SCN_LNK_REMOVE__&IMAGE_SCN_ALIGN_1BYTES
.idata C0000048 &IMAGE_SCN_TYPE_NO_PAD__&IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.idata C0100040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_1BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.idata C0200040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_2BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.idata C0201040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_2BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.idata C0300040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.idata C0301040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.orpc 60501020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_16BYTES__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ
.rdata 40300040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ
.rdata 40301040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ
.rdata 40400040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_READ
.rdata 40401040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_READ
.rdata 40500040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_16BYTES__&IMAGE_SCN_MEM_READ
.text 400020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_ALIGN_8BYTES
.text 60000020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ
.text 60101020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_1BYTES__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ
.text 60201020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_2BYTES__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ
.text 60300020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ
.text 60301020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ
.text 60500020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_ALIGN_16BYTES__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ
.text 60501020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_16BYTES__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ
.tls C0300040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.xdata 40401040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_READ


__________________________________

Detailed sections found on several Libraries and executables files

.text 400020 &IMAGE_SCN_CNT_CODE   &IMAGE_SCN_ALIGN_8BYTES  
.orpc 60501020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_16BYTES &IMAGE_SCN_LNK_COMDAT 
.text 60000020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ   
.text 60101020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_1BYTES &IMAGE_SCN_LNK_COMDAT 
.text 60201020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_2BYTES &IMAGE_SCN_LNK_COMDAT 
.text 60300020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_4BYTES  
.text 60301020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_4BYTES &IMAGE_SCN_LNK_COMDAT 
.text 60500020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_16BYTES  
.text 60501020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_16BYTES &IMAGE_SCN_LNK_COMDAT 

; Data Flags

.CRT C0300040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_4BYTES 
.data C0300040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_4BYTES 
.data C0301040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_4BYTES &IMAGE_SCN_LNK_COMDAT
.data C0400040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_8BYTES 
.data C0401040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_8BYTES &IMAGE_SCN_LNK_COMDAT
.data C0500040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_16BYTES 

.edata 040000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  
.rdata 040000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  
.rsrc 040000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  
.rdata 040000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  
.INIT 042000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  &IMAGE_SCN_MEM_DISCARDABLE
.reloc 042000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  &IMAGE_SCN_MEM_DISCARDABLE
.rdata 050000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  &IMAGE_SCN_MEM_SHARED
.reloc 050000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  &IMAGE_SCN_MEM_SHARED
.rsrc 050000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  &IMAGE_SCN_MEM_SHARED
BSS 0C0000000  &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE 
.tls 0C0000000  &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE 
.idata 0C0000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE 
DATA 0C0000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE 
.data 0C0000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE 
.shared 0D0000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_MEM_SHARED
.idata 040000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  
.bss 0C0000080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE 
.CRT 0C0000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE 

; Really weird sections at C:\WINNT\Driver Cache\i386 
; It is worthfull to analyze to fix them like I was doing for the Dumped files. (RosAsm crashes, for example at ftdisk.sys)

PAGE 060000020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ  
INIT 0E2000020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_MEM_DISCARDABLE

.rdata 048000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_NOT_PAGED  
.data 0C8000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_NOT_PAGED &IMAGE_SCN_MEM_WRITE 
.rsrc 042000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE  
.reloc 042000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE  
.rsrc 048000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_NOT_PAGED  


; Debug Data
; Name Types .debug$S; .debug$F; .debug$T; .debug$P

.debug$S 42000048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD  
.debug$F 42100040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE   &IMAGE_SCN_ALIGN_1BYTES
.debug$S 42100040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE   &IMAGE_SCN_ALIGN_1BYTES
.debug$T 42100040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE   &IMAGE_SCN_ALIGN_1BYTES
.debug$P 42100048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD  &IMAGE_SCN_ALIGN_1BYTES
.debug$S 42100048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD  &IMAGE_SCN_ALIGN_1BYTES
.debug$T 42100048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD  &IMAGE_SCN_ALIGN_1BYTES
.debug$S 42101040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE  &IMAGE_SCN_LNK_COMDAT &IMAGE_SCN_ALIGN_1BYTES
.debug$F 42101048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD &IMAGE_SCN_LNK_COMDAT &IMAGE_SCN_ALIGN_1BYTES
.debug$S 42101048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD &IMAGE_SCN_LNK_COMDAT &IMAGE_SCN_ALIGN_1BYTES
.debug$F 42400048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD  &IMAGE_SCN_ALIGN_8BYTES

.debug 42100040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE   &IMAGE_SCN_ALIGN_1BYTES
.debug 42101040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE  &IMAGE_SCN_LNK_COMDAT &IMAGE_SCN_ALIGN_1BYTES
.debug 42000048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD  
.debug 42100048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD  &IMAGE_SCN_ALIGN_1BYTES
.debug 42101048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD &IMAGE_SCN_LNK_COMDAT &IMAGE_SCN_ALIGN_1BYTES
.debug 42400048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD  &IMAGE_SCN_ALIGN_8BYTES

.idata C0100040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_1BYTES  
.idata C0200040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_2BYTES  
.idata C0201040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_2BYTES  &IMAGE_SCN_LNK_COMDAT
.idata C0300040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_4BYTES  
.idata C0301040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_4BYTES  &IMAGE_SCN_LNK_COMDAT
.idata C0000048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE  &IMAGE_SCN_TYPE_NO_PAD 

.rdata 040300040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_4BYTES 
.rdata 040301040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_4BYTES &IMAGE_SCN_LNK_COMDAT
.rdata 040400040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_8BYTES 
.rdata 040401040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_8BYTES &IMAGE_SCN_LNK_COMDAT
.rdata 040500040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_16BYTES 

.tls 0C0300040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_4BYTES


.xdata 040401040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_8BYTES &IMAGE_SCN_LNK_COMDAT

; Virtual Data Flags

.bss 0C0300080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_4BYTES 
.bss 0C0301080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_4BYTES &IMAGE_SCN_LNK_COMDAT
.bss 0C0400080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_8BYTES 
.bss 0C0401080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_8BYTES &IMAGE_SCN_LNK_COMDAT
.bss 0C0500080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_16BYTES 

; Linker Directive Flags

.drectve 0100A00 &IMAGE_SCN_LNK_INFO &IMAGE_SCN_LNK_REMOVE &IMAGE_SCN_ALIGN_1BYTES




;;
;;
[RawDataType: D$ 0]

; RawData Types Constants

[RDT_DATA 0] ; This is the default. If everything fails, it always set to DATA
[RDT_LNKDIRECTIVE 1]
[RDT_CODE 2]
[RDT_VIRTUALDATA 3]
[RDT_DEBUGS 4]
[RDT_DEBUGF 5]
[RDT_DEBUGT 6]
[RDT_DEBUGP 7]
[RDT_IDATA 8]
[RDT_RDATA 9]
[RDT_XDATA 10]
[RDT_PDATA 11]
[RDT_EDATA 12]
[RDT_TLSDATA 13]
[RDT_RELOCDATA 14]
[RDT_RSRCDATA 15]
[RDT_ERROR 16] ; This is DATA, but we set this Flag to identify some error cases messages
[RDT_STABSTR 17] ; .stabstr ; NetFramework stab string section. This section contains only strings.

Proc IdentifyRawDataType:
    Uses eax, esi, ecx

    Mov eax D$esi+36 ; eax points to the Characteristics member

    ; Reinitialize the Raw Data to make sure it always will be set to 0 when the below
    ; checkings fails.

    Mov D$RawDataType 0

    ; Check if it is .text or .code or any other section that is related to code only
    .Test_If eax &IMAGE_SCN_CNT_CODE
            jmp L0>
    .Test_Else_If eax &IMAGE_SCN_MEM_EXECUTE
    L0:
            Test_If eax &IMAGE_SCN_CNT_UNINITIALIZED_DATA
                Mov D$RawDataType RDT_VIRTUALDATA
            Test_Else
                Mov D$RawDataType RDT_CODE
            Test_End

    .Test_Else

        ; All the rest is Data. Let's analyze what kind of data it is.
        ; Find Virtual Data
        ; Guga Note: Look at CTLFWR32.LIB we will need a Virtual data. Like we did for the raw data.

        Test_If eax &IMAGE_SCN_CNT_UNINITIALIZED_DATA
            Mov D$RawDataType RDT_VIRTUALDATA

EndP

        Test_End

        ; Check for long section names (KSGUID.LIB)
        If B$esi = '/'
            ; Similar to what we found at WriteIndirectSectionName
            Call GetName1Offset ; convert the Offset to hexadecimal
            Call GetStringTablePointer
            Mov esi D$CoffSectionBase
            add esi D$CoffSymbolsPointer
            Mov ecx D$PointerToStringTable
            add ecx D$MemberName1Offset | Mov esi ecx
        End_If

        ; Find Virtual Data
        ; convert all section names to small caps and compare them.
        ; save the address of esi to be restored later
        Push D$esi, D$esi+4
        or D$esi 020202020
        or D$esi+4 020202020

        .If D$esi = 'bss' ; BSS or bss
            jmp L0>
        .Else_If D$esi = '.bss'

        L0:
            Mov D$RawDataType RDT_VIRTUALDATA

        ; Find Debug Data. It will use the proper Debug structures to display in the raw data.
        .Else_If D$esi = '.deb'

            If D$esi+4 = 'ug$s' ; .debug$s , .debug$S or Big caps
                Mov D$RawDataType RDT_DEBUGS
            Else_If D$esi+4 = 'ug$f' ; .debug$f , .debug$F or Big caps
                Mov D$RawDataType RDT_DEBUGF
            Else_If D$esi+4 = 'ug$t' ; .debug$t , .debug$T or Big caps
                Mov D$RawDataType RDT_DEBUGT
            Else_If D$esi+4 = 'ug$p' ; .debug$p , .debug$P or Big caps
                Mov D$RawDataType RDT_DEBUGP
            Else
                Mov D$RawDataType RDT_ERROR
            End_If

        ; Find NetFramework Stab Structure. It will use the proper Debug structures to display in the raw data.
        .Else_If_And D$esi = '.sta', D$esi+4 = 'bstr'
            Mov D$RawDataType RDT_STABSTR

        ; Check for Linker Directive data. esi points to the Characteristics member.

        .Else_If_And D$esi = '.dre', D$esi+4 = 'ctve' ; .drectve

            Test_If eax &IMAGE_SCN_LNK_INFO
                ; PointerToRelocations and PointerToLinenumbers must be 0, because
                ; the section must not have relocations or line numbers.
                If_And D$esi+24 = 0, D$esi+28 = 0
                    Mov D$RawDataType RDT_LNKDIRECTIVE
                Else
                    Mov D$RawDataType RDT_ERROR
                End_If

            Test_Else
                Mov D$RawDataType RDT_ERROR
            Test_End

        ; Find Common Data. (.data; .idata; .rdata; .xdata; .pdata; .edata)

        .Else_If_And D$esi = '.dat', B$esi+4 = 'a'
            Mov D$RawDataType RDT_DATA

        .Else_If D$esi+2 = 'data'

            If W$esi = '.i' ; Import Data Tables. Need to use proper structures or Data arrays.
                            ; Using IMAGE_IMPORT_DESCRIPTOR structure. Take a look at ACLCLS.LIB to we identify the proper structures and Data Bytes
                Mov D$RawDataType RDT_IDATA
            Else_If W$esi = '.r' ; Read Only Data. In general, it is a series of Dwords, but it can be anything, like a serie of structures etc.
                Mov D$RawDataType RDT_RDATA
            Else_If W$esi = '.x' ; Exception information Data Tables. Need to use proper structures EXCEPTION_RECORD. _msExcept _msExcInfo
                Mov D$RawDataType RDT_XDATA
            Else_If W$esi = '.p' ; Exception information Data Tables. Need to use proper structures (EXCEPTION_RECORD ?)
                Mov D$RawDataType RDT_PDATA
            Else_If W$esi = '.e' ; Export Data Tables. Need to use proper structures or Data arrays (IMAGE_EXPORT_DIRECTORY ?)
                Mov D$RawDataType RDT_EDATA
            Else
                Mov D$RawDataType RDT_ERROR
            End_If

        .Else_If D$esi = '.tls' ; Thread-local storage Data. Some section uses the IMAGE_TLS_DIRECTORY32 to point to
                                ; some data inside this .tls section. See MSVCRT.LIB, MSVCRTD.LIB, LIBCMT.LIB, LIBCMTD.LIB
            Mov D$RawDataType RDT_TLSDATA

        .Else_If_And D$esi = '.rel', W$esi+4 = 'oc' ; Image relocations. We must use the (IMAGE_RELOCATION ?) for this
            Mov D$RawDataType RDT_RELOCDATA

        .Else_If_And D$esi = '.rsr', B$esi+4 = 'c' ; Resource Section. We must use the IMAGE_RESOURCE_DIRECTORY structure.
            Mov D$RawDataType RDT_RSRCDATA

        .Else
            ; When everything Else fails or we have some unknown section names, we always set to DATA
            Mov D$RawDataType RDT_DATA
        .End_If

        Pop D$esi+4, D$esi

    .Test_End

EndP

___________________________________________________

GetCoffSectionHeader:

    Push esi
        zCopy CoffSectionHeaderTitle
    Pop esi

    ; The name of the section is indirected addressed on the String Table ?
    ; Yes, do next line. No, jmp over. Example: KSGUID.LIB

    If B$esi = '/'
        Call WriteIndirectSectionName
    End_If

    Push esi
        Mov B$edi '[' | inc edi
        Call WriteObjIndice
        zCopy {B$ 'ImgSecHdr' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Mov B$edi ':', W$edi+1 CRLF | add edi 3
    Pop esi

;    Call IdentifyRawDataType

    Call WriteObjSectionHeaderItem CoffSectionHeaderName, {B$ ': B$ ' EOS}

    If B$esi-8 = '/'
        Call WriteLinkerMemberSizeHex D$MemberName1Offset
    End_If

    Call WriteObjSectionHeaderItem CoffSectionHeaderVirtualSize, {B$ ': D$ ' EOS}
    Call WriteObjSectionHeaderItem CoffSectionHeaderRVA, {B$ ': D$ ' EOS}
    Call WriteObjSectionHeaderItem CoffSectionHeaderSize, {B$ ': D$ ' EOS}
    Call WriteObjSectionHeaderItem CoffSectionHeaderPointerToData, {B$ ': D$ ' EOS}
    Call WriteObjSectionHeaderItem CoffSectionHeaderPointerToReloc, {B$ ': D$ ' EOS}
    Call WriteObjSectionHeaderItem CoffSectionHeaderPointerToLinesNumbers, {B$ ': D$ ' EOS}
    Call WriteObjSectionHeaderItem CoffSectionHeaderNumberOfRelocations, {B$ ': W$ ' EOS}
    Call WriteObjSectionHeaderItem CoffSectionHeaderNumberOfLinesNumbers, {B$ ': W$ ' EOS}
    Call WriteObjSectionHeaderItem CoffSectionHeaderCharacteristics, {B$ ': D$ ' EOS}
    Push esi
        sub edi 2 | Mov eax D$esi-4 | Call WriteCharacteristicsEquates
        Mov W$edi CRLF, B$edi+2 ']', D$edi+3 CRLF2 | add edi 7
    Pop esi
ret
____________________________________________________________________________________________

WriteSectionHeaderVirtualSizeDiffLabel:
    Push esi

        Call WriteObjIndice
        zCopy {B$ "VirtualDataEnd" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Mov D$edi ' - ' | add edi 3

        Call WriteObjIndice
        zCopy {B$ "VirtualData" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    Pop esi
ret


WriteSectionHeaderRawSizeDiffLabel:

    If D$esi+4 = 0 ; Is PointerToRawData = 0 ? If so, it means we are dealing with Virtual data. Do next line
        Push esi | lodsd | Call WriteEax | Pop esi | ret
    End_If

    Push esi

        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy {B$ ".RawDataEnd" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Mov D$edi ' - ' | add edi 3

        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy {B$ ".RawData" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    Pop esi
ret

[IMAGE_FILE_HEADER_Comment1: B$ 'IMAGE_FILE_HEADER_' EOS]

WriteSectionHeaderPointerToDataDiffLabel:
    Push esi

        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy {B$ ".RawData" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Mov D$edi ' - ' | add edi 3

        zCopy IMAGE_FILE_HEADER_Comment1
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Call WriteIndiceOnly

    Pop esi
ret


Proc FixRelocDiffLabel:
    uses eax, ebx, ecx, esi, edi

    ; reinitialize the sectino counter
    Call InitSectionHeaderNumber

    movzx ecx W$ObjNumberOfSections
    Mov ebx D$FirstSectionPointer
    Mov edi D$esi ; edi hold the value of our PointerToRelocations to be compared

L0:

    ; Points to PointerToRelocations starting at the 1st section
   .If edi = D$ebx+018 ; is our relocs equal to the relocs on section where we are ?
        If W$ebx+020 <> 0 ; Is NumberOfRelocations = 0 ? Yes, we are analysing our own section
            ExitP
        End_If
   .End_If

   Call IncrementSectionHeaderNumber
   add ebx 028 ; go to the next section
   Loop L0<


EndP


WriteSectionHeaderPointerToRelocDiffLabel:

    Push D$SectionHeaderNumber

    If W$esi+4+4 = 0 ; Points to NumberOfRelocations. aclcls.lib
        ; Check how many relocs we have. If we have 0, it means that the pointer is being referenced by another section.
         Call FixRelocDiffLabel
    End_If

    Push esi

        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy {B$ ".ImgReloc" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy RelocNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Mov D$edi ' - ' | add edi 3

        zCopy IMAGE_FILE_HEADER_Comment1
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Call WriteIndiceOnly

    Pop esi

    Pop D$SectionHeaderNumber

ret


WriteSectionHeaderPointerToLineNumberDiffLabel:
    Push esi

        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy {B$ ".LineNumber" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy CoffLineNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Mov D$edi ' - ' | add edi 3

        zCopy IMAGE_FILE_HEADER_Comment1
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Call WriteIndiceOnly

    Pop esi
ret


; It should display for example: ; Stringdata15 - StringTableSize

Proc WriteSectionHeaderPointerToStringTableDiffLabel:
    Uses esi, eax, edx

    ; Initialize the String Record Counter

    Call InitStringTableRecord

    ; On this case, the Stringtable must be initialized with 0. Otherwise our counter will already start at 01
    ; So, we must reset the counter to '000000'.

    Mov D$StringTableRecord+4 '00'

    lodsd ; eax is pointing to the Offset of the String on the String Table
    Mov edx D$PointerToStringTable
    add edx 4 ; Bypass the String Table Size. and Point to the beginning of the 1st String
    Mov esi edx ; esi is pointing to the 1st string
    add edx eax ; edx will be using as a counter of the total amount of strings untill we reach the String where we are.

    .Do

        While B$esi <> 0 | inc esi | End_While | inc esi | On B$esi = 0, Mov edx 0

        Call IncrementStringTableRecord

    .Loop_Until esi >= edx


        Push esi
            Call WriteObjIndice
            zCopy {B$ 'StringData' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy StringTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi

        Mov D$edi ' - ' | add edi 3

        Call WriteObjIndice
        zCopy {B$ 'StringTableSize' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy {B$ ' ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi


EndP

____________________________________________________________________________________________


Proc WriteSectionHeaderSymbolConstantIndex:
    uses eax, ecx, ebx, esi

    Call InitSymbolIndexRecord
    Call WriteObjIndice

    ; Fix the label to show in Caps and replace the '." with an "_"
    ; The 1st char in "Obj000000." is in Caps, so we don't need to overwrite the "O" char

    Push edi
    Mov B$edi-1 '_', W$edi-9 'BJ'
    Pop edi

    Push esi
    zCopy {B$ 'SYMBOLINDEX' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    Pop esi

    Mov ecx D$esi

    ; Note to Ren�: Replacing this with a hex to decimal ascii string is better,
    ; but i couldn't make it be on the same style as SymbolTableIndex

    While ecx <> 0
        Call IncrementSymbolIndexRecord
        dec ecx
    End_While

    zCopy SymbolTableIndex
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    ; restore the SymbolTableIndex
    Call InitSymbolIndexRecord

EndP
____________________________________________________________________________________________



[CoffRelocHeaderTitle: B$ "; IMAGE_RELOCATION Structure, pointed at: " EOS]

[CoffRelocHeaderRelocRVA: B$ 'VirtualAddress' EOS]
[CoffRelocHeaderRelocCount: B$ 'RelocCount' EOS]
[CoffRelocHeaderSymbolTableIndex: B$ 'SymbolTableIndex' EOS]
[CoffRelocHeaderType: B$ 'Type1' EOS]

Proc WriteObjRelocHeaderItem:
    Argument @Text1, @Text2

        Push esi
            Call WriteObjIndice

            zCopy {B$ "Sec" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy {B$ '.ImgReloc' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy RelocNumber;SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Mov B$edi '.' | inc edi
            zCopy D@Text1
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy D@Text2
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi

        Mov eax D@Text2, al B$eax+2

        ..If al = 'D'

            .If D$esi <> 0

                Mov eax D@Text1

                If D$eax = 'Virt' ; from "VirtualAddress" string

                    lodsd | Call WriteEax
                    Push esi | zcopy {B$ " ; Hex Value:  " EOS} | Pop esi
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

                ;Else_If D$eax = 'Relo'; From "RelocCount" string

                    ;Call WriteSectionHeaderPointerToDataDiffLabel
                    ;Push esi | zcopy {B$ " ; Hex Value:  " EOS} | Pop esi

                Else_If D$eax = 'Symb'; From "SymbolTableIndex" string
                    Call WriteSectionHeaderSymbolConstantIndex
                    add esi 4 ; we need to increment esi by 4 to we adjust at the end of the End_If macro
                    Push esi | zcopy {B$ " ; Hex Value:  " EOS} | Pop esi

                End_If

            .Else
                add esi 4


            .End_If
            ; We need to subtract esi by 4 (01 Dword) to we get back to the path again. It was increased above.
            sub esi 4 | lodsd | Call WriteEax

        ..Else_If al = 'W'
            lodsw | and eax 0FFFF | Call WriteEax

        ;..Else

        ..End_If

        Mov W$edi CRLF | add edi 2
EndP

_________________________________________________________
;;
;;
a) When the  Machine member of IMAGE_FILE_HEADER is settlled to one of 
theses: IMAGE_FILE_MACHINE_I386, IMAGE_FILE_MACHINE_I486, 
IMAGE_FILE_MACHINE_I586,  IMAGE_FILE_MACHINE_IA64 (So, INtel compatible 
processors), the equates are:

&IMAGE_REL_I386_ABSOLUTE
&IMAGE_REL_I386_DIR16
&IMAGE_REL_I386_REL16
&IMAGE_REL_I386_DIR32
&IMAGE_REL_I386_DIR32NB
&IMAGE_REL_I386_SEG12
&IMAGE_REL_I386_SECTION
&IMAGE_REL_I386_SECREL
&IMAGE_REL_I386_REL32

b) When the  Machine member of IMAGE_FILE_HEADER is settlled to one of 
theses: IMAGE_FILE_MACHINE_MIPS16, IMAGE_FILE_MACHINE_MIPSFPU, 
IMAGE_FILE_MACHINE_MIPSFPU16 , IMAGE_FILE_MACHINE_R10000, 
IMAGE_FILE_MACHINE_R3000,
IMAGE_FILE_MACHINE_R4000, IMAGE_FILE_MACHINE_WCEMIPSV2
(So, MIPS compatible processors), the equates are:

&IMAGE_REL_MIPS_ABSOLUTE
&IMAGE_REL_MIPS_REFHALF
&IMAGE_REL_MIPS_REFWORD
&IMAGE_REL_MIPS_JMPADDR
&IMAGE_REL_MIPS_REFHI
&IMAGE_REL_MIPS_REFLO
&IMAGE_REL_MIPS_GPREL
&IMAGE_REL_MIPS_LITERAL
&IMAGE_REL_MIPS_SECTION
&IMAGE_REL_MIPS_SECREL
&IMAGE_REL_MIPS_SECRELLO
&IMAGE_REL_MIPS_SECRELHI
&IMAGE_REL_MIPS_JMPADDR16
&IMAGE_REL_MIPS_REFWORDNB
&IMAGE_REL_MIPS_PAIR

c) When the  Machine member of IMAGE_FILE_HEADER is settlled to one of 
theses: IMAGE_FILE_MACHINE_ALPHA, IMAGE_FILE_MACHINE_ALPHA64, 
IMAGE_FILE_MACHINE_AXP64 (So, Alpha compatible processors), the equates are:

&IMAGE_REL_ALPHA_ABSOLUTE
&IMAGE_REL_ALPHA_REFLONG
&IMAGE_REL_ALPHA_REFQUAD
&IMAGE_REL_ALPHA_GPREL32
&IMAGE_REL_ALPHA_LITERAL
&IMAGE_REL_ALPHA_LITUSE
&IMAGE_REL_ALPHA_GPDISP
&IMAGE_REL_ALPHA_BRADDR
&IMAGE_REL_ALPHA_HINT
&IMAGE_REL_ALPHA_INLINE_REFLONG
&IMAGE_REL_ALPHA_REFHI
&IMAGE_REL_ALPHA_REFLO
&IMAGE_REL_ALPHA_PAIR
&IMAGE_REL_ALPHA_MATCH
&IMAGE_REL_ALPHA_SECTION
&IMAGE_REL_ALPHA_SECREL
&IMAGE_REL_ALPHA_REFLONGNB
&IMAGE_REL_ALPHA_SECRELLO
&IMAGE_REL_ALPHA_SECRELHI
&IMAGE_REL_ALPHA_REFQ3
&IMAGE_REL_ALPHA_REFQ2
&IMAGE_REL_ALPHA_REFQ1
&IMAGE_REL_ALPHA_GPRELLO
&IMAGE_REL_ALPHA_GPRELHI

d) When the  Machine member of IMAGE_FILE_HEADER is settlled to one of 
theses: IMAGE_FILE_MACHINE_POWERPC, IMAGE_FILE_MACHINE_POWERPCFP, (So, IBM 
POwer PC compatible processors), the equates are:

&IMAGE_REL_PPC_ABSOLUTE
&IMAGE_REL_PPC_ADDR64
&IMAGE_REL_PPC_ADDR32
&IMAGE_REL_PPC_ADDR24
&IMAGE_REL_PPC_ADDR16
&IMAGE_REL_PPC_ADDR14
&IMAGE_REL_PPC_REL24
&IMAGE_REL_PPC_REL14
&IMAGE_REL_PPC_ADDR32NB
&IMAGE_REL_PPC_SECREL
&IMAGE_REL_PPC_SECTION
&IMAGE_REL_PPC_SECREL16
&IMAGE_REL_PPC_REFHI
&IMAGE_REL_PPC_REFLO
&IMAGE_REL_PPC_PAIR
&IMAGE_REL_PPC_SECRELLO
&IMAGE_REL_PPC_SECRELHI
&IMAGE_REL_PPC_GPREL

e) When the  Machine member of IMAGE_FILE_HEADER is settlled to one of 
theses: IMAGE_FILE_MACHINE_SH3, IMAGE_FILE_MACHINE_SH3DSP, 
IMAGE_FILE_MACHINE_SH3E, IMAGE_FILE_MACHINE_SH4, IMAGE_FILE_MACHINE_SH5,  
(So, Hitachi SuperH compatible processors), the equates are:

&IMAGE_REL_SH3_ABSOLUTE
&IMAGE_REL_SH3_DIRECT16
&IMAGE_REL_SH3_DIRECT32
&IMAGE_REL_SH3_DIRECT8
&IMAGE_REL_SH3_DIRECT8_WORD
&IMAGE_REL_SH3_DIRECT8_LONG
&IMAGE_REL_SH3_DIRECT4
&IMAGE_REL_SH3_DIRECT4_WORD
&IMAGE_REL_SH3_DIRECT4_LONG
&IMAGE_REL_SH3_PCREL8_WORD
&IMAGE_REL_SH3_PCREL8_LONG
&IMAGE_REL_SH3_PCREL12_WORD
&IMAGE_REL_SH3_STARTOF_SECTION
&IMAGE_REL_SH3_SIZEOF_SECTION
&IMAGE_REL_SH3_SECTION
&IMAGE_REL_SH3_SECREL
&IMAGE_REL_SH3_DIRECT32_NB

f) When the  Machine member of IMAGE_FILE_HEADER is settlled to one of 
theses: IMAGE_FILE_MACHINE_ARM,  (So, ARM compatible processors), the 
equates are:

&IMAGE_REL_ARM_ABSOLUTE
&IMAGE_REL_ARM_ADDR32
&IMAGE_REL_ARM_ADDR32NB
&IMAGE_REL_ARM_BRANCH24
&IMAGE_REL_ARM_BRANCH11
&IMAGE_REL_ARM_SECTION
&IMAGE_REL_ARM_SECREL
;;
;;
WriteRelocHeaderTypeEquate:
    Push esi

    movzx eax W$esi-2 | Mov ebx D$CoffSectionBase | movzx ebx W$ebx

    Mov D$edi ' ; ' | add edi 3

    ; Intel Compatible: IMAGE_FILE_MACHINE_I386, IMAGE_FILE_MACHINE_I486,
    ; IMAGE_FILE_MACHINE_I586,  IMAGE_FILE_MACHINE_IA64

    ..If ebx = &IMAGE_FILE_MACHINE_I386
L1:     .If eax = &IMAGE_REL_I386_ABSOLUTE
            zCopy {B$ '&IMAGE_REL_I386_ABSOLUTE' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_I386_DIR16
            zCopy {B$ '&IMAGE_REL_I386_DIR16' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_I386_REL16
            zCopy {B$ '&IMAGE_REL_I386_REL16' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_I386_DIR32
            zCopy {B$ '&IMAGE_REL_I386_DIR32' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_I386_DIR32NB
            zCopy {B$ '&IMAGE_REL_I386_DIR32NB' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_I386_SEG12
            zCopy {B$ '&IMAGE_REL_I386_SEG12' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_I386_SECTION
            zCopy {B$ '&IMAGE_REL_I386_SECTION' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_I386_SECREL
            zCopy {B$ '&IMAGE_REL_I386_SECREL' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_I386_REL32
            zCopy {B$ '&IMAGE_REL_I386_REL32' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .End_If

    ..Else_If ebx = &IMAGE_FILE_MACHINE_I486
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_I586
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_IA64
        jmp L1<<


    ; Mips Compatible: IMAGE_FILE_MACHINE_MIPS16, IMAGE_FILE_MACHINE_MIPSFPU,
    ; IMAGE_FILE_MACHINE_MIPSFPU16 , IMAGE_FILE_MACHINE_R10000,
    ; IMAGE_FILE_MACHINE_R3000, IMAGE_FILE_MACHINE_R4000, IMAGE_FILE_MACHINE_WCEMIPSV2

    ..Else_If ebx = &IMAGE_FILE_MACHINE_MIPS16
L1:     .If eax = &IMAGE_REL_MIPS_ABSOLUTE
            zCopy {B$ '&IMAGE_REL_I386_ABSOLUTE' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_MIPS_REFHALF
            zCopy {B$ '&IMAGE_REL_MIPS_REFHALF' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_MIPS_REFWORD
            zCopy {B$ '&IMAGE_REL_MIPS_REFWORD' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_MIPS_JMPADDR
            zCopy {B$ '&IMAGE_REL_MIPS_JMPADDR' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_MIPS_REFHI
            zCopy {B$ 'IMAGE_REL_MIPS_REFHI' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_MIPS_REFLO
            zCopy {B$ '&IMAGE_REL_MIPS_REFLO' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_MIPS_GPREL
            zCopy {B$ '&IMAGE_REL_MIPS_GPREL' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_MIPS_LITERAL
            zCopy {B$ '&IMAGE_REL_MIPS_LITERAL' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_MIPS_SECTION
            zCopy {B$ '&IMAGE_REL_MIPS_SECTION' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_MIPS_SECREL
            zCopy {B$ '&IMAGE_REL_MIPS_SECREL' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_MIPS_SECRELLO
            zCopy {B$ '&IMAGE_REL_MIPS_SECRELLO' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_MIPS_SECRELHI
            zCopy {B$ '&IMAGE_REL_MIPS_SECRELHI' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_MIPS_JMPADDR16
            zCopy {B$ '&IMAGE_REL_MIPS_JMPADDR16' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_MIPS_REFWORDNB
            zCopy {B$ '&IMAGE_REL_MIPS_REFWORDNB' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_MIPS_PAIR
            zCopy {B$ '&IMAGE_REL_MIPS_PAIR' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .End_If


    ..Else_If ebx = &IMAGE_FILE_MACHINE_MIPSFPU
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_MIPSFPU16
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_R10000
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_R3000
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_R4000
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_WCEMIPSV2
        jmp L1<<


    ; Alpha Compatible: IMAGE_FILE_MACHINE_ALPHA, IMAGE_FILE_MACHINE_ALPHA64, IMAGE_FILE_MACHINE_AXP64

    ..Else_If ebx = &IMAGE_FILE_MACHINE_ALPHA
L1:     .If eax = &IMAGE_REL_ALPHA_ABSOLUTE
            zCopy {B$ '&IMAGE_REL_ALPHA_ABSOLUTE' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_REFLONG
            zCopy {B$ '&IMAGE_REL_ALPHA_REFLONG' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_REFQUAD
            zCopy {B$ '&IMAGE_REL_ALPHA_REFQUAD' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_GPREL32
            zCopy {B$ '&IMAGE_REL_ALPHA_GPREL32' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_LITERAL
            zCopy {B$ '&IMAGE_REL_ALPHA_LITERAL' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_LITUSE
            zCopy {B$ '&IMAGE_REL_ALPHA_LITUSE' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_GPDISP
            zCopy {B$ '&IMAGE_REL_ALPHA_GPDISP' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_BRADDR
            zCopy {B$ '&IMAGE_REL_ALPHA_BRADDR' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_HINT
            zCopy {B$ '&IMAGE_REL_ALPHA_HINT' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_INLINE_REFLONG
            zCopy {B$ '&IMAGE_REL_ALPHA_INLINE_REFLONG' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_REFHI
            zCopy {B$ '&IMAGE_REL_ALPHA_REFHI' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_REFLO
            zCopy {B$ '&IMAGE_REL_ALPHA_REFLO' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_PAIR
            zCopy {B$ '&IMAGE_REL_ALPHA_PAIR' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_MATCH
            zCopy {B$ '&IMAGE_REL_ALPHA_MATCH' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_SECTION
            zCopy {B$ '&IMAGE_REL_ALPHA_SECTION' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_SECREL
            zCopy {B$ '&IMAGE_REL_ALPHA_SECREL' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_REFLONGNB
            zCopy {B$ '&IMAGE_REL_ALPHA_REFLONGNB' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_SECRELLO
            zCopy {B$ '&IMAGE_REL_ALPHA_SECRELLO' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_SECRELHI
            zCopy {B$ '&IMAGE_REL_ALPHA_SECRELHI' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_REFQ3
            zCopy {B$ '&IMAGE_REL_ALPHA_REFQ3' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_REFQ2
            zCopy {B$ '&IMAGE_REL_ALPHA_REFQ2' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_REFQ1
            zCopy {B$ '&IMAGE_REL_ALPHA_REFQ1' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_GPRELLO
            zCopy {B$ '&IMAGE_REL_ALPHA_GPRELLO' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ALPHA_GPRELHI
            zCopy {B$ '&IMAGE_REL_ALPHA_GPRELHI' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .End_If

    ..Else_If ebx = &IMAGE_FILE_MACHINE_ALPHA64
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_AXP64
        jmp L1<<

    ; IBM Power PC compatible: IMAGE_FILE_MACHINE_POWERPC, IMAGE_FILE_MACHINE_POWERPCFP

    ..Else_If ebx = &IMAGE_FILE_MACHINE_POWERPC
L1:     .If eax = &IMAGE_REL_PPC_ABSOLUTE
            zCopy {B$ '&IMAGE_REL_PPC_ABSOLUTE' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_ADDR64
            zCopy {B$ '&IMAGE_REL_PPC_ADDR64' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_ADDR32
            zCopy {B$ '&IMAGE_REL_PPC_ADDR32' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_ADDR24
            zCopy {B$ '&IMAGE_REL_PPC_ADDR24' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_ADDR16
            zCopy {B$ '&IMAGE_REL_PPC_ADDR16' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_ADDR14
            zCopy {B$ '&IMAGE_REL_PPC_ADDR14' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_REL24
            zCopy {B$ '&IMAGE_REL_PPC_REL24' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_REL14
            zCopy {B$ '&IMAGE_REL_PPC_REL14' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_ADDR32NB
            zCopy {B$ '&IMAGE_REL_PPC_ADDR32NB' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_SECREL
            zCopy {B$ '&IMAGE_REL_PPC_SECREL' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_SECTION
            zCopy {B$ '&IMAGE_REL_PPC_SECTION' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_SECREL16
            zCopy {B$ '&IMAGE_REL_PPC_SECREL16' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_REFHI
            zCopy {B$ '&IMAGE_REL_PPC_REFHI' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_REFLO
            zCopy {B$ '&IMAGE_REL_PPC_REFLO' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_PAIR
            zCopy {B$ '&IMAGE_REL_PPC_PAIR' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_SECRELLO
            zCopy {B$ '&IMAGE_REL_PPC_SECRELLO' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_SECRELHI
            zCopy {B$ '&IMAGE_REL_PPC_SECRELHI' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_PPC_GPREL
            zCopy {B$ '&IMAGE_REL_PPC_GPREL' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .End_If

    ..Else_If ebx = &IMAGE_FILE_MACHINE_POWERPCFP
        jmp L1<<

    ; Hitachi SuperH compatible: IMAGE_FILE_MACHINE_SH3, IMAGE_FILE_MACHINE_SH3DSP, IMAGE_FILE_MACHINE_SH3E, IMAGE_FILE_MACHINE_SH4, IMAGE_FILE_MACHINE_SH5

    ..Else_If ebx = &IMAGE_FILE_MACHINE_SH3
L1:     .If eax = &IMAGE_REL_SH3_ABSOLUTE
            zCopy {B$ '&IMAGE_REL_SH3_ABSOLUTE' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_SH3_DIRECT16
            zCopy {B$ '&IMAGE_REL_SH3_DIRECT16' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_SH3_DIRECT32
            zCopy {B$ '&IMAGE_REL_SH3_DIRECT32' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_SH3_DIRECT8
            zCopy {B$ '&IMAGE_REL_SH3_DIRECT8' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_SH3_DIRECT8_WORD
            zCopy {B$ '&IMAGE_REL_SH3_DIRECT8_WORD' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_SH3_DIRECT8_LONG
            zCopy {B$ '&IMAGE_REL_SH3_DIRECT8_LONG' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_SH3_DIRECT4
            zCopy {B$ '&IMAGE_REL_SH3_DIRECT4' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_SH3_DIRECT4_WORD
            zCopy {B$ '&IMAGE_REL_SH3_DIRECT4_WORD' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_SH3_DIRECT4_LONG
            zCopy {B$ '&IMAGE_REL_SH3_DIRECT4_LONG' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_SH3_PCREL8_WORD
            zCopy {B$ '&IMAGE_REL_SH3_PCREL8_WORD' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_SH3_PCREL8_LONG
            zCopy {B$ '&IMAGE_REL_SH3_PCREL8_LONG' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_SH3_PCREL12_WORD
            zCopy {B$ '&IMAGE_REL_SH3_PCREL12_WORD' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_SH3_STARTOF_SECTION
            zCopy {B$ '&IMAGE_REL_SH3_STARTOF_SECTION' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_SH3_SIZEOF_SECTION
            zCopy {B$ '&IMAGE_REL_SH3_SIZEOF_SECTION' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_SH3_SECTION
            zCopy {B$ '&IMAGE_REL_SH3_SECTION' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_SH3_SECREL
            zCopy {B$ '&IMAGE_REL_SH3_SECREL' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_SH3_DIRECT32_NB
            zCopy {B$ '&IMAGE_REL_SH3_DIRECT32_NB' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .End_If

    ..Else_If ebx = &IMAGE_FILE_MACHINE_SH3DSP
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_SH3E
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_SH4
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_SH5
        jmp L1<<

    ; ARM compatible: IMAGE_FILE_MACHINE_ARM

    ..Else_If ebx = &IMAGE_FILE_MACHINE_ARM
        .If eax = &IMAGE_REL_ARM_ABSOLUTE
            zCopy {B$ '&IMAGE_REL_ARM_ABSOLUTE' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ARM_ADDR32
            zCopy {B$ '&IMAGE_REL_ARM_ADDR32' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ARM_ADDR32NB
            zCopy {B$ '&IMAGE_REL_ARM_ADDR32NB' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ARM_BRANCH24
            zCopy {B$ '&IMAGE_REL_ARM_BRANCH24' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ARM_BRANCH11
            zCopy {B$ '&IMAGE_REL_ARM_BRANCH11' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ARM_SECTION
            zCopy {B$ '&IMAGE_REL_ARM_SECTION' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_REL_ARM_SECREL
            zCopy {B$ '&IMAGE_REL_ARM_SECREL' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .End_If

    ..End_If

    Pop esi


ret

; GugaNote
; Problem of multiple section with different relocs with DX9SDKSampleFramework.lib
; Most often, they may contains relocs offset, but size is 0 (ForceLibrary.lib)

Proc GetCoffRelocHeader:
    uses ecx, ebx

    Push esi
        zCopy CoffRelocHeaderTitle
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Call WriteObjIndice
        zCopy {B$ "ImgSecHdr" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy {B$ ".PointerToRelocations" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy {B$ " Member" D$ CRLF2 B$ "[" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi


        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy {B$ ".ImgReloc" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy RelocNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Mov B$edi ':', W$edi+1 CRLF | add edi 3
    Pop esi

    Call WriteObjRelocHeaderItem CoffRelocHeaderRelocRVA, {B$ ': D$ ' EOS}
    Call WriteObjRelocHeaderItem CoffRelocHeaderSymbolTableIndex, {B$ ': D$ ' EOS}
    Call WriteObjRelocHeaderItem CoffRelocHeaderType, {B$ ': W$ ' EOS}
    sub edi 2 | Call WriteRelocHeaderTypeEquate
    Mov W$edi CRLF | add edi 2

    Push esi
        Mov B$edi ']', D$edi+1 CRLF2 | add edi 5
    Pop esi
EndP
____________________________________________________________

[RelocNumber: B$ '000001' EOS]


InitSectionRelocNumber:
    Mov D$RelocNumber '0000', D$RelocNumber+4 '01'
ret


IncrementSectionRelocNumber:
    lea ebx D$RelocNumber+5 | inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret




____________________________________________________________

Proc WriteObjLineHeaderItem:
    Argument @Text1, @Text2
    Uses edx

        Push esi
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy {B$ ".LineNumber" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy CoffLineNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy D@Text1
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy D@Text2
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi


                Mov eax D@Text1

                .If D$eax = 'Symb' ; from "SymbolTableIndex" string
                    Call WriteSectionHeaderSymbolConstantIndex
                    Push esi | zcopy {B$ " ; Hex Value:  " EOS} | Pop esi
                    lodsd | Call WriteEax

                .Else_If D$eax = 'Virt'; From "VirtualAddress" string
                    lodsd | Call WriteEax

                .Else_If D$eax = 'Line'; From "Linenumber" string
                    xor eax eax
                    lodsw | Call WriteEax

                .End_If


        Mov W$edi CRLF | add edi 2
EndP
;;
;;
[IMAGE_LINENUMBER:
 SymbolTableIndex: VirtualAddress: D$ 0
 Linenumber: W$ 0]
;;
;;
[CoffLineNumberTitle: "
__________________________________________________

; IMAGE_LINENUMBER Structure, pointed at: " EOS]

[CoffLineHeaderSymbolTableIndex: B$ 'SymbolTableIndex' EOS]
[CoffLineHeaderVirtualAddress: B$ 'VirtualAddress' EOS]
[CoffLineHeaderLinenumber: B$ 'Linenumber' EOS]

Proc GetCoffLineHeader:
    uses ecx, ebx


    Push esi
        zCopy CoffLineNumberTitle
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Call WriteObjIndice
        zCopy {B$ "ImgSecHdr" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy {B$ ".PointerToLinenumbers Member" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    Pop esi

    Mov D$edi CRLF2 | add edi 4

    If W$esi+4 <> 0
        Push esi
        zCopy {B$ "; This Structure contains the correspondent line number on the original source specified at LineNumber member." EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy {W$ CRLF, B$ "; Also it is related only to Code and contains the Virtual Address of some referenced data that can" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy {W$ CRLF, B$ "; be a Local Variable, Argument, or a Data Pointer", D$ CRLF2, 0}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi
    End_If


    Push esi
        Mov B$edi "[" | inc edi
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy {B$ ".LineNumber" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy CoffLineNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Mov B$edi ':', W$edi+1 CRLF | add edi 3
    Pop esi

    ; problem in ATLDLOAD.LIB

    If W$esi+4 = 0

        Call WriteObjLineHeaderItem CoffLineHeaderSymbolTableIndex, {B$ ': D$ ' EOS}
    Else

        Call WriteObjLineHeaderItem CoffLineHeaderVirtualAddress, {B$ ': D$ ' EOS}

    End_If

    Call WriteObjLineHeaderItem CoffLineHeaderLinenumber, {B$ ': W$ ' EOS}

;    sub edi 2 | Call WriteRelocHeaderTypeEquate
 ;   Mov W$edi CRLF | add edi 2

    Push esi
        Mov B$edi ']', D$edi+1 CRLF2 | add edi 5
    Pop esi
EndP



[CoffLineNumber: B$ '000001' EOS]


InitSectionLineNumber:
    Mov D$CoffLineNumber '0000', D$CoffLineNumber+4 '01'
ret


IncrementSectionLineNumber:
    lea ebx D$CoffLineNumber+5 | inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret


____________________________________________________________

[CoffSectionSize: D$ ?
 CoffPointerToData: D$ ?
 CoffPointerToReloc: D$ ?
 NumberOfRelocations: D$ ?
 CoffPointerToLineNumber: D$ ?
 NumberOfLineNumbers: D$ ?]

GetCoffSectionsVariables:

    add esi 8   ;  CoffSectionHeaderName, {B$ ': B$ ' EOS}

    add esi 4   ; CoffSectionHeaderVirtualSize, {B$ ': D$ ' EOS}

    add esi 4   ; CoffSectionHeaderRVA, {B$ ': D$ ' EOS} / VirtualAddress

    Move D$CoffSectionSize D$esi
    add esi 4   ; CoffSectionHeaderSize, {B$ ': D$ ' EOS} / SizeOfRawData

    Move D$CoffPointerToData D$esi
    add esi 4   ;  CoffSectionHeaderPointerToData, {B$ ': D$ ' EOS} / PointerToRawData

    Move D$CoffPointerToReloc D$esi
    add esi 4   ; CoffSectionHeaderPointerToReloc, {B$ ': D$ ' EOS} / PointerToRelocations

    Move D$CoffPointerToLineNumber D$esi
    add esi 4   ; CoffSectionHeaderPointerToLinesNumbers, {B$ ': D$ ' EOS} / PointerToLinenumbers

    Move W$NumberOfRelocations W$esi
    add esi 2   ; CoffSectionHeaderNumberOfRelocations, {B$ ': W$ ' EOS}

    Move W$NumberOfLineNumbers W$esi
    add esi 2   ; CoffSectionHeaderNumberOfLinesNumbers, {B$ ': W$ ' EOS}


  ; CoffSectionHeaderNumberOfRelocations, {B$ ': W$ ' EOS}

  ; CoffSectionHeaderNumberOfLinesNumbers, {B$ ': W$ ' EOS}
  ; CoffSectionHeaderCharacteristics, {B$ ': D$ ' EOS}
ret


[CoffSymbolsPointer: D$ ?
 CoffSymbolsNumber: D$ ?
 PointerToSymbolsStringTable: D$ ?
 SizeOfSymbolsStringsTable: D$ ?
 EndOfSymbolsStringsTable: D$ ?]

GetImageSymbolsVariables:
    Push esi
        Mov esi D$CoffSectionBase
        Mov eax D$esi+8
        If eax <> 0
            add eax esi | Mov D$CoffSymbolsPointer eax
        Else
            Mov D$CoffSymbolsPointer 0
        End_If

        Move D$CoffSymbolsNumber D$esi+12

        If D$CoffSymbolsPointer <> 0
          ; Search the Pointer to the Symbols Strings Table (18 is the Size of one Symbol Record):
            Mov ecx D$CoffSymbolsNumber, eax 18
            mul ecx
            add eax D$CoffSymbolsPointer
            Mov D$PointerToSymbolsStringTable eax
            Move D$SizeOfSymbolsStringsTable D$eax
            add eax D$eax | Mov D$EndOfSymbolsStringsTable eax
        End_If
    Pop esi
ret
____________________________________________________________________________________________

Proc WriteAuxSymFmt1DiffLabel:
    Uses esi, ebx, ecx, eax, edx, D$SectionHeaderNumber, D$CoffLineNumber


    Push esi

    Mov ebx D$FirstSectionPointer

    movzx ecx W$esi-14 ; ecx is pointing to the previous section value
    ; we need to recompute the sectionheader number

    Call InitSectionHeaderNumber
    Call InitSectionLineNumber

    Mov edx edi ; for keeping the path to edi, let's save it at edx

    ; Guga note this is the errros on DX9SDKSampleFramework.lib We have an 0 previous section

    On ecx = 0 EndP

    If ecx <> 1 ; Are we pointing to thye 1st section ?
        Push ecx

            Do
                Call IncrementSectionHeaderNumber
                dec ecx
            Loop_Until ecx = 1

        Pop ecx

    End_If

    sub esi 18+8 ; esi is pointing to the beginning of previous Symbol

    Push edx

    If ecx <> 1     ; If we are pointing to the 1st Section, we don� need to add it to the offset of the IMAGE_SECTION_HEADER
        Mov eax 40  ; eax is the Size of the IMAGE_SECTON_HEADER
        imul ecx    ; multiply by the section where we must go
        add ebx eax
        sub ebx 40  ; need to subtract from the Size of IMAGE_SECTION_HEADER to get back to the proper path.
    End_If

    Pop edx


    Pop esi ; restore our value at esi

    ; we must point to the proper Section (IMAGE_SECTION_HEADER)
    add ebx 28 ; we are pointing to PointerToLinenumbers
    Mov eax D$ebx ; eax points to the value of linenumber in the ImgSec where we are

    Mov edi D$esi ; edi is pointing to the LineNumber value to be compare

    .If eax <> edi ; is the pointer where we are is equal to the value found in the linenumber ?
                   ; Yes, jmp over.


        movzx ecx W$ebx+6 ; ecx is our counter it points to the number of line numbers
        ; No. the values are different. We are dealing with several linenumbers in the same section.
        ; Let's check for their values.

        L0:

            On eax = edi, jmp @OutLoop
            add eax 6 ; we need to add the value in edi (IMAGE_SECTION_HEADER where we are) 6 that is the size of the line number structure
            Call IncrementSectionLineNumber
        Loop L0<

@OutLoop:


    .End_If


        Mov edi edx ; retore the path to edi

        Push esi

        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy {B$ ".LineNumber" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy CoffLineNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi


        Mov D$edi ' - ' | add edi 3

        zCopy IMAGE_FILE_HEADER_Comment1
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Call WriteIndiceOnly
        zcopy {B$ " ; Hex Value:  " EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi

EndP



Proc WriteAuxiliarySymbolsRecordsFormat1Item:
    Argument @Text1, @Text2
    Uses edx,  D$SymbolTableIndex , D$SymbolTableIndex+4

        Push esi
            Call WriteObjIndice
            zCopy {B$ 'ImgAuxFmt1.Ind' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy SymbolTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Mov B$edi '.' | inc edi
            zCopy D@Text1
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy D@Text2
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi


        Mov eax D@Text1

        .If D$eax = 'TagI' ; from "TagIndex" string
            ;Call WriteAuxSymFmt5DiffLabel
            Call WriteSectionHeaderSymbolConstantIndex
            Push esi | zcopy {B$ " ; Hex Value:  " EOS} | Pop esi
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            lodsd | Call WriteEax

        .Else_If D$eax = 'Tota'; From "TotalSize" string
            lodsd | Call WriteEax

        .Else_If D$eax+9 = 'Line'; From "PointerToLinenumber" string
            If D$esi <> 0
                Call WriteAuxSymFmt1DiffLabel
             End_If
            lodsd | Call WriteEax

        .Else_If D$eax+9 = 'Next'; From "PointerToNextFunction" string

            If D$esi <> 0
                Call WriteSectionHeaderSymbolConstantIndex
                Push esi | zcopy {B$ " ; Hex Value:  " EOS} | Pop esi
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            End_If

            lodsd | Call WriteEax

        .Else_If D$eax = 'Rese'; From "Reserved" string
            xor eax eax
            lodsw | Call WriteEax

        .End_If


        Mov W$edi CRLF | add edi 2


EndP


;[ImgAuxSym: B$ 'ImgAuxSym' EOS] ; Guga Note: Remove this data variable. It is not used anymore


[Fmt1TagIndex: B$ 'TagIndex' EOS]
[Fmt1TotalSize: B$ 'TotalSize' EOS]
[Fmt1PointerToLinenumber: B$ 'PointerToLinenumber' EOS]
[Fmt1PointerToNextFunction: B$ 'PointerToNextFunction' EOS]
[Fmt1Reserved: B$ 'Reserved' EOS]

Proc WriteAuxiliarySymbolsRecordsFormat1:

        Push esi
            Call WriteObjIndice
            zCopy {B$ 'ImgAuxFmt1.Ind' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy SymbolTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Mov B$edi ':', W$edi+1 CRLF | add edi 3
        Pop esi

        Call WriteAuxiliarySymbolsRecordsFormat1Item Fmt1TagIndex, {B$ ': D$ ' EOS}
        Call WriteAuxiliarySymbolsRecordsFormat1Item Fmt1TotalSize, {B$ ': D$ ' EOS}
        Call WriteAuxiliarySymbolsRecordsFormat1Item Fmt1PointerToLinenumber, {B$ ': D$ ' EOS}
        Call WriteAuxiliarySymbolsRecordsFormat1Item Fmt1PointerToNextFunction, {B$ ': D$ ' EOS}
        Call WriteAuxiliarySymbolsRecordsFormat1Item Fmt1Reserved, {B$ ': W$ ' EOS}
        sub edi 2

EndP

_________________________________________________________

Proc WriteAuxiliarySymbolsRecordsFormat2Item:
    Argument @Text1, @Text2
    Uses edx, ecx, D$SymbolTableIndex, D$SymbolTableIndex+4

        Push esi
            Call WriteObjIndice
            zCopy {B$ 'ImgAuxFmt2.Ind' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy SymbolTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Mov B$edi '.' | inc edi
            zCopy D@Text1
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy D@Text2
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi


        Mov eax D@Text1


        .If D$eax+5 = 'ved1' ; from "Reserved1" string
            lodsd | Call WriteEax

        .Else_If D$eax = 'Line'; From "LineNumber" string
            xor eax eax
            lodsw | Call WriteEax
            Push esi | zcopy {B$ " ; Base Line Number" EOS} | Pop esi
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If D$eax+5 = 'ved2'; From "Reserved2" string
            xor eax eax
            Mov ecx 6
            L0:
                lodsb | Call WriteEax
                Mov W$edi ", " | add edi 2
            Loop L0<
            sub edi 2

        .Else_If D$eax = 'Poin'; From "PointerToNextFunction" string

            If D$esi <> 0
                Call WriteSectionHeaderSymbolConstantIndex
                Push esi | zcopy {B$ " ; Hex Value:  " EOS} | Pop esi
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            End_If

            lodsd | Call WriteEax

        .Else_If D$eax+5 = 'ved3'; From "Reserved3" string
            xor eax eax
            lodsw | Call WriteEax

        .End_If

        Mov W$edi CRLF | add edi 2

EndP



[Fmt2Reserved1: B$ 'Reserved1' EOS]
[Fmt2LineNumber: B$ 'LineNumber' EOS]
[Fmt2Reserved2: B$ 'Reserved2' EOS]
[Fmt2PointerToNextFunction: B$ 'PointerToNextFunction' EOS]
[Fmt2Reserved3: B$ 'Reserved3' EOS]

Proc WriteAuxiliarySymbolsRecordsFormat2:

        Push esi
            Call WriteObjIndice
            zCopy {B$ 'ImgAuxFmt2.Ind' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy SymbolTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Mov B$edi ':', W$edi+1 CRLF | add edi 3
        Pop esi

        Call WriteAuxiliarySymbolsRecordsFormat2Item Fmt2Reserved1, {B$ ': D$ ' EOS}
        Call WriteAuxiliarySymbolsRecordsFormat2Item Fmt2LineNumber, {B$ ': W$ ' EOS}
        Call WriteAuxiliarySymbolsRecordsFormat2Item Fmt2Reserved2, {B$ ': B$ ' EOS}
        Call WriteAuxiliarySymbolsRecordsFormat2Item Fmt2PointerToNextFunction, {B$ ': D$ ' EOS}
        Call WriteAuxiliarySymbolsRecordsFormat2Item Fmt2Reserved3, {B$ ': W$ ' EOS}
        sub edi 2

EndP

_________________________________________________________

Proc WriteAuxSymFmt3CharacteristicEquates:
    Uses esi;, ebx

    ;Mov ebx esi

    .If D$esi = &IMAGE_WEAK_EXTERN_SEARCH_NOLIBRARY
        zCopy {B$ '&IMAGE_WEAK_EXTERN_SEARCH_NOLIBRARY ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If D$esi = &IMAGE_WEAK_EXTERN_SEARCH_LIBRARY
        zCopy {B$ '&IMAGE_WEAK_EXTERN_SEARCH_LIBRARY ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If D$esi = &IMAGE_WEAK_EXTERN_SEARCH_ALIAS
        zCopy {B$ '&IMAGE_WEAK_EXTERN_SEARCH_ALIAS ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .End_If


EndP

Proc WriteAuxiliarySymbolsRecordsFormat3Item:
    Argument @Text1, @Text2
    Uses edx, ecx, D$SymbolTableIndex, D$SymbolTableIndex+4

        Push esi
            Call WriteObjIndice
            zCopy {B$ 'ImgAuxFmt3.Ind' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy SymbolTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Mov B$edi '.' | inc edi
            zCopy D@Text1
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy D@Text2
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi


        Mov eax D@Text1


        .If D$eax = 'TagI' ; from "TagIndex" string
            If D$esi <> 0
               Call WriteSectionHeaderSymbolConstantIndex
               Push esi | zcopy {B$ " ; Hex Value:  " EOS} | Pop esi
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            End_If

            lodsd | Call WriteEax

        .Else_If D$eax = 'Char'; From "Characteristics" string
            ;lodsd | Call WriteEax
            Call WriteAuxSymFmt3CharacteristicEquates
            lodsd | Call WriteEax

        .Else_If D$eax = 'Rese'; From "Reserved" string
            xor eax eax
            Mov ecx 10
            L0:
                lodsb | Call WriteEax
                Mov W$edi ", " | add edi 2
            Loop L0<
            sub edi 2

        .End_If

        Mov W$edi CRLF | add edi 2

EndP



[Fmt3TagIndex: B$ 'TagIndex' EOS]
[Fmt3Characteristics: B$ 'Characteristics' EOS]
[Fmt3Reserved: B$ 'Reserved' EOS]

Proc WriteAuxiliarySymbolsRecordsFormat3:

        Push esi
            Call WriteObjIndice
            zCopy {B$ 'ImgAuxFmt3.Ind' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy SymbolTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Mov B$edi ':', W$edi+1 CRLF | add edi 3
        Pop esi

        Call WriteAuxiliarySymbolsRecordsFormat3Item Fmt3TagIndex, {B$ ': D$ ' EOS}
        Call WriteAuxiliarySymbolsRecordsFormat3Item Fmt3Characteristics, {B$ ': D$ ' EOS}
        Call WriteAuxiliarySymbolsRecordsFormat3Item Fmt3Reserved, {B$ ': B$ ' EOS}
        sub edi 2

EndP


_________________________________________________________


Proc WriteAuxiliarySymbolsRecordsFormat4:

        Push esi
            Call WriteObjIndice
            zCopy {B$ 'ImgAuxFmt4.Ind' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy SymbolTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Mov B$edi ':', W$edi+1 CRLF | add edi 3
            Call WriteObjIndice
            zCopy {B$ 'ImgAuxFmt4.Ind' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy SymbolTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy {B$ '.FileName: B$ ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi

        Mov edx esi | add edx 18

        .If B$esi = 0
            Mov B$edi '0' | inc edi | inc esi
        .Else

        Mov B$edi "'" | inc edi
L0:     lodsb
            If al = 0
                dec esi | jmp L1>
            End_If
        stosb | On esi < edx, jmp L0<
L1:     Mov B$edi "'" | inc edi

        .End_If

        While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ;Mov W$edi CRLF | add edi 2


EndP


__________________________________________________________



Proc WriteAuxSymFmt5DiffLabel:
    Uses esi, ebx, ecx, eax, edx

    Mov ebx D$FirstSectionPointer

    movzx ecx W$esi-18+12 ; ecx is pointing to the previous section value
    ; we need to recompute the sectionheader number

    Call InitSectionHeaderNumber

    Mov edx edi ; for keeping the path to edi, let's save it at edx

    ; Guga note this is the errros on DX9SDKSampleFramework.lib We have an 0 previous section

    On ecx = 0 , jmp C2>>

    If ecx <> 1 ; Are we pointing to thye 1st section ?
        Push ecx

            Do
                Call IncrementSectionHeaderNumber
                dec ecx
            Loop_Until ecx = 1

        Pop ecx

    End_If

    sub esi 18 ; esi is pointing to the beginning of previous Symbol

    Push edx

    If ecx <> 1     ; If we are pointing to the 1st Section, we don� need to add it to the offset of the IMAGE_SECTION_HEADER
        Mov eax 40  ; eax is the Size of the IMAGE_SECTON_HEADER
        imul ecx    ; multiply by the section where we must go
        add ebx eax
        sub ebx 40  ; need to subtract from the Size of IMAG_SECTION_HEADER to get back to the proper path.
    End_If

    Pop edx

    ; we must point to the proper Section (IMAGE_SECTION_HEADER)

    Mov eax ebx ; The 1st String where we are (The Section in IMAGE_SECTION_HEADER)
    Mov edi esi ; edi is pointing to the beginning of the previous section. (Our string to be compared)

    Mov ecx 8   ;how many bytes in the string...char= one byte
    repe cmpsb
    jne C2>

        Mov edi edx ; retore the path to edi

        add eax 20 ; eax is pointing to PointerToRawData on IMAGE_SECTION_HEADER from the structure we label
        Mov esi D$eax ; now we point it to esi

        .If D$eax <> 0 ; Is PointerToRawData = 0 ? Is it Virtual data ? No, do next line.

            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy {B$ ".RawDataEnd" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Mov D$edi ' - ' | add edi 3

            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy SectionHeaderNumber
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy {B$ ".RawData ; Hex Value:  " EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .End_If

        ExitP

    C2:

    Mov edi edx ; retore the path to edi

EndP


Proc WriteAuxSymFmt5TypeEquates:
    Uses esi, ebx

    Mov ebx esi


    .If B$ebx = &IMAGE_COMDAT_SELECT_NODUPLICATES
        zCopy {B$ '&IMAGE_COMDAT_SELECT_NODUPLICATES ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If B$ebx = &IMAGE_COMDAT_SELECT_ANY
        zCopy {B$ '&IMAGE_COMDAT_SELECT_ANY ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If B$ebx = &IMAGE_COMDAT_SELECT_SAME_SIZE
        zCopy {B$ '&IMAGE_COMDAT_SELECT_SAME_SIZE ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If B$ebx = &IMAGE_COMDAT_SELECT_EXACT_MATCH
        zCopy {B$ '&IMAGE_COMDAT_SELECT_EXACT_MATCH ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If B$ebx = &IMAGE_COMDAT_SELECT_ASSOCIATIVE
        zCopy {B$ '&IMAGE_COMDAT_SELECT_ASSOCIATIVE ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .Else_If B$ebx = &IMAGE_COMDAT_SELECT_LARGEST
        zCopy {B$ '&IMAGE_COMDAT_SELECT_LARGEST ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    .End_If


EndP


[Fmt5Length: B$ 'Length' EOS]
[Fmt5NumberOfRelocations: B$ 'NumberOfRelocations' EOS]
[Fmt5NumberOfLinenumbers: B$ 'NumberOfLinenumbers' EOS]
[Fmt5CheckSum: B$ 'CheckSum' EOS]
[Fmt5Number: B$ 'Number' EOS]
[Fmt5Selection: B$ 'Selection' EOS]
[Fmt5Reserved: B$ 'Reserved' EOS]

Proc WriteAuxiliarySymbolsRecordsFormat5Item:
    Argument @Text1, @Text2
    Uses edx, ecx;, D$SymbolTableIndex, D$SymbolTableIndex+4

        Push esi
            Call WriteObjIndice
            zCopy {B$ 'ImgAuxFmt5.Ind' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy SymbolTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Mov B$edi '.' | inc edi
            zCopy D@Text1
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy D@Text2
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi


                Mov eax D@Text1

                .If D$eax = 'Leng' ; from "Length" string
                    Call WriteAuxSymFmt5DiffLabel
                    lodsd | Call WriteEax

                .Else_If D$eax+8 = 'Relo'; From "NumberOfRelocations" string
                    xor eax eax
                    lodsw | Call WriteEax

                .Else_If D$eax+8 = 'Line'; From "NumberOfLinenumbers" string
                    xor eax eax
                    lodsw | Call WriteEax

                .Else_If D$eax = 'Chec'; From "CheckSum" string
                    lodsd | Call WriteEax

                .Else_If D$eax = 'Numb'; From "Number" string

                    xor eax eax
                    lodsw | Call WriteEax

                .Else_If D$eax = 'Sele'; From "Selection" string
                    Call WriteAuxSymFmt5TypeEquates
                    xor eax eax
                    lodsb | Call WriteEax

                .Else_If D$eax = 'Rese'; From "Reserved" string
                    xor eax eax
                    Mov ecx 3
                    L0:
                        xor eax eax
                        lodsb | Call WriteEax
                        Mov W$edi ", " | add edi 2
                    Loop L0<
                    sub edi 2

                .End_If


        Mov W$edi CRLF | add edi 2


EndP


Proc WriteAuxiliarySymbolsRecordsFormat5:

        Push esi
            Call WriteObjIndice
            zCopy {B$ 'ImgAuxFmt5.Ind' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy SymbolTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Mov B$edi ':', W$edi+1 CRLF | add edi 3
        Pop esi

        Call WriteAuxiliarySymbolsRecordsFormat5Item Fmt5Length, {B$ ': D$ ' EOS}
        Call WriteAuxiliarySymbolsRecordsFormat5Item Fmt5NumberOfRelocations, {B$ ': W$ ' EOS}
        Call WriteAuxiliarySymbolsRecordsFormat5Item Fmt5NumberOfLinenumbers, {B$ ': W$ ' EOS}
        Call WriteAuxiliarySymbolsRecordsFormat5Item Fmt5CheckSum, {B$ ': D$ ' EOS}
        Call WriteAuxiliarySymbolsRecordsFormat5Item Fmt5Number, {B$ ': W$ ' EOS}
        Call WriteAuxiliarySymbolsRecordsFormat5Item Fmt5Selection, {B$ ': B$ ' EOS}
        Call WriteAuxiliarySymbolsRecordsFormat5Item Fmt5Reserved, {B$ ': B$ ' EOS}
        sub edi 2

EndP


_______________________________________________________________


; Examples: frmMainNET.obj, program_PadraoCOM.obj, Hello World_PAdraoNET.obj
; GIven to me (Guga) by Fabricio

Proc WriteAuxiliarySymbolsRecordsFormatUnknown:
    ;Argument @Pointer

    Push esi
        Call WriteObjIndice
        zCopy {B$ 'ImgAuxSym' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy SymbolTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy {B$ 'FormatUnknwon: B$ ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    Pop esi

;        Mov esi D@Pointer, ecx 18
        Mov ecx 18

L0:     lodsb | and eax 0FF | Call WriteEax
        Mov W$edi ', ' | add edi 2 | loop L0<
        sub edi 2
        ;Mov B$edi-2 "'" | dec edi
EndP

_______________________________________________________________

Proc WriteAuxiliarySymbolsRecords:
    Arguments @Number
    Uses ecx, ebx

    .Do

        Call WriteSymbolConstantIndex
        Call IncrementSymbolIndexRecord

        Mov W$edi CRLF | add edi 2
        Mov B$edi '[' | inc edi

        Call IncrementSymbolTableRecord

            ..If D$SymbolStorageClass = &IMAGE_SYM_CLASS_EXTERNAL
            ; example at libgccguga.a

                .If B$esi-4 = (&IMAGE_SYM_DTYPE_FUNCTION shl 4) + &IMAGE_SYM_TYPE_NULL ; Type1Complex = 020
                    ;If W$esi-6 >= 0  ;
                    ; SectionNumber >= 0. Once again the documentation lied. We can have a 0 section number
                        Call WriteAuxiliarySymbolsRecordsFormat1
                    ;End_If
                .End_If

            ..Else_If D$SymbolStorageClass = &IMAGE_SYM_CLASS_FUNCTION
            ; example at shit.obj
                Call WriteAuxiliarySymbolsRecordsFormat2

            ..Else_If D$SymbolStorageClass = &IMAGE_SYM_CLASS_WEAK_EXTERNAL
            ; example at stats.lib ; Libdc.lib
                .If W$esi-6 = &IMAGE_SYM_UNDEFINED  ; SectionNumber = &IMAGE_SYM_UNDEFINED
                    If D$esi-10 = 0 ; Value member = 0
                        Call WriteAuxiliarySymbolsRecordsFormat3
                    End_If
                .End_If

            ..Else_If D$SymbolStorageClass = &IMAGE_SYM_CLASS_FILE
                Call WriteAuxiliarySymbolsRecordsFormat4

            ..Else_If D$SymbolStorageClass = &IMAGE_SYM_CLASS_STATIC
                Call WriteAuxiliarySymbolsRecordsFormat5

            ..Else
                Call WriteAuxiliarySymbolsRecordsFormatUnknown

            ..End_If

        Mov B$edi ']', D$edi+1 CRLF2 | add edi 5
        dec D$CoffSymbolsNumber ; decrement our counter from the total amount symbols
        dec D@Number ; decrement the total amount of Auxiliary Symbols

    .Loop_Until D@Number = 0

EndP

____________________________________________________________________________________________


[SymbolTableTitle: "
_________________________________________________________

; Symbols Table Structure IMAGE_SYMBOL
_________________________________________________________


" EOS]
[SymbolTableShortName: B$ 'NameShort' EOS]
[SymbolTableLongNameZero: B$ 'NameZero' EOS]
[SymbolTableLongNameOffset: B$ 'NameLongOffset' EOS]
[SymbolTableValue: B$ 'Value' EOS]
[SymbolTableSectionNumber: B$ 'SectionNumber' EOS]
[SymbolTableTypeComplex: B$ 'Type1Complex' EOS]
[SymbolTableTypeBase: B$ 'Type1Base' EOS]
[SymbolTableStorageClass: B$ 'StorageClass' EOS]
[SymbolTableNumberOfAuxSymbols: B$ 'NumberOfAuxSymbols' EOS]


;[PointerToSymbolsRecords: D$ ?]

[SymbolTableRecord: B$ '000001' EOS]


InitSymbolTableRecord:
    Mov D$SymbolTableRecord '0000', D$SymbolTableRecord+4 '01'
ret


IncrementSymbolTableRecord:
    lea ebx D$SymbolTableRecord+5 | inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret


Proc WriteImageSymbolTableFieldName:
    Argument @String1 @String2
    Uses esi, ecx, ebx

   ; If D@String2 < SymbolTableShortName1
   ;      Mov eax D@String1, ebx D@String2
   ;      int3
   ; End_If

        Call WriteObjIndice
        zCopy D@String1
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy SymbolTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        zCopy D@String2
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

EndP
;;

;;
&IMAGE_SYM_UNDEFINED 0 Symbol record is not yet assigned a section.
    If the value is 0 this indicates a references to an external symbol 
    defined Elsewhere. If the value is non-zero this is a common symbol 
    with a size specified by the value. 
&IMAGE_SYM_ABSOLUTE -1 The symbol has an absolute (non-relocatable) value 
    and is not an address. 
&IMAGE_SYM_DEBUG -2 The symbol provides general type or debugging information 
    but does not correspond to a section. Microsoft tools use this setting 
    along with .file records (storage class FILE). 
;;
;;
WriteRelocSectionNumberEquate:
    Push esi
        ;Mov ebx D$PointerToSymbolsRecords

        If W$esi = &IMAGE_SYM_UNDEFINED
            zCopy {B$ '&IMAGE_SYM_UNDEFINED ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Else_If W$esi = &IMAGE_SYM_ABSOLUTE
            zCopy {B$ '&IMAGE_SYM_ABSOLUTE ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Else_If W$esi = &IMAGE_SYM_DEBUG
            zCopy {B$ '&IMAGE_SYM_DEBUG ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        End_If
    Pop esi
ret
;;
;;
WriteRelocSectionNumberEquate_Old:
    Push esi
        Mov ebx D$PointerToSymbolsRecords

        If W$ebx = &IMAGE_SYM_UNDEFINED
            zCopy {B$ ' ; &IMAGE_SYM_UNDEFINED' EOS}
        Else_If W$ebx = &IMAGE_SYM_ABSOLUTE
            zCopy {B$ ' ; &IMAGE_SYM_ABSOLUTE' EOS}
        Else_If W$ebx = &IMAGE_SYM_DEBUG
            zCopy {B$ ' ; &IMAGE_SYM_DEBUG' EOS}
        End_If
    Pop esi
ret
;;

;;
The most significative byte equates are:

&IMAGE_SYM_DTYPE_NULL
&IMAGE_SYM_DTYPE_POINTER
&IMAGE_SYM_DTYPE_FUNCTION
&IMAGE_SYM_DTYPE_ARRAY


The less signfiicative byte are:

&IMAGE_SYM_TYPE_NULL
&IMAGE_SYM_TYPE_VOID
&IMAGE_SYM_TYPE_CHAR
&IMAGE_SYM_TYPE_SHORT
&IMAGE_SYM_TYPE_INT
&IMAGE_SYM_TYPE_LONG
&IMAGE_SYM_TYPE_FLOAT
&IMAGE_SYM_TYPE_DOUBLE
&IMAGE_SYM_TYPE_STRUCT
&IMAGE_SYM_TYPE_UNION
&IMAGE_SYM_TYPE_ENUM
&IMAGE_SYM_TYPE_MOE
&IMAGE_SYM_TYPE_BYTE
&IMAGE_SYM_TYPE_WORD
&IMAGE_SYM_TYPE_UINT
&IMAGE_SYM_TYPE_DWORD
;;
;;
[RelocTypeD: D$ ?
 RelocType: D$ ?]
;;
;;
        .If_And eax >= 32, eax <= 65535 ; Check for Corrupted library. Reserved Bits are not 0

            Call LibScan_ErrManager IMP_OBJ_RESBIT_ERR

            If eax = &FALSE ; The user pressed No
                ExitP

            Else_If eax = &TRUE ; The user pressed Yes. We will Zero all reserved bits for him continue the parsing.
                Mov eax D@Flags ; Restore the original value of eax to be fixed
                Mov ah 0 ; Clear High Bits Flags
                btr eax 5 | btr eax 6 | btr eax 7 ; Clear Bits 5 to 7

            End_If

        .End_If

;;
;;
;WriteRelocTypeEquates:
Proc WriteSymbolTypeEquates:
    Uses esi, ebx

        .If B$esi > 63 ; Check for Corrupted library. The actual maximum value is 63 (03F)

            Call LibScan_ErrManager IMP_OBJ_RESBIT_ERR

            If eax = &FALSE ; The user pressed No

EndP

            Else_If eax = &TRUE ; The user pressed Yes. We will Zero all reserved bits for him continue the parsing.

                ; Clear all high flags, returning only in the targeting Byte
                xor eax eax | lodsb | btr eax 6 | btr eax 7 ; Clear Bits 6 and , that are what exceed the limit of 63
                Mov ebx eax
            End_If

        .Else

            xor eax eax | lodsb | Mov ebx eax

        .End_If

        and eax 0F | shr ebx 4

        ;Mov D$RelocType eax, D$RelocTypeD ebx

        If ebx = &IMAGE_SYM_DTYPE_NULL
            zCopy {B$ '(&IMAGE_SYM_DTYPE_NULL shl 4)' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Else_If ebx = &IMAGE_SYM_DTYPE_POINTER
            zCopy {B$ '(&IMAGE_SYM_DTYPE_POINTER shl 4)' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Else_If ebx = &IMAGE_SYM_DTYPE_FUNCTION
            zCopy {B$ '(&IMAGE_SYM_DTYPE_FUNCTION shl 4)' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Else_If ebx = &IMAGE_SYM_DTYPE_ARRAY
            zCopy {B$ '(&IMAGE_SYM_DTYPE_ARRAY shl 4)' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        End_If

        .If eax = &IMAGE_SYM_TYPE_NULL
            zCopy {B$ ' + &IMAGE_SYM_TYPE_NULL' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_TYPE_VOID
            zCopy {B$ ' + &IMAGE_SYM_TYPE_VOID' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_TYPE_CHAR
            zCopy {B$ ' + &IMAGE_SYM_TYPE_CHAR' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_TYPE_SHORT
            zCopy {B$ ' + &IMAGE_SYM_TYPE_SHORT' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_TYPE_INT
            zCopy {B$ ' + &IMAGE_SYM_TYPE_INT' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_TYPE_LONG
            zCopy {B$ ' + &IMAGE_SYM_TYPE_LONG' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_TYPE_FLOAT
            zCopy {B$ ' + &IMAGE_SYM_TYPE_FLOAT' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_TYPE_DOUBLE
            zCopy {B$ ' + &IMAGE_SYM_TYPE_DOUBLE' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_TYPE_STRUCT
            zCopy {B$ ' + &IMAGE_SYM_TYPE_STRUCT' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_TYPE_UNION
            zCopy {B$ ' + &IMAGE_SYM_TYPE_UNION' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_TYPE_ENUM
            zCopy {B$ ' + &IMAGE_SYM_TYPE_ENUM' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_TYPE_MOE
            zCopy {B$ ' + &IMAGE_SYM_TYPE_MOE' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_TYPE_BYTE
            zCopy {B$ ' + &IMAGE_SYM_TYPE_BYTE' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_TYPE_WORD
            zCopy {B$ ' + &IMAGE_SYM_TYPE_WORD' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_TYPE_UINT
            zCopy {B$ ' + &IMAGE_SYM_TYPE_UINT' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_TYPE_DWORD
            zCopy {B$ ' + &IMAGE_SYM_TYPE_DWORD' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .End_If

        zCopy {B$ ' ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

EndP

;;
;;
&IMAGE_SYM_CLASS_END_OF_FUNCTION -1 Special symbol representing end of function, for debugging purposes. 
&IMAGE_SYM_CLASS_NULL 0 No storage class assigned. 
&IMAGE_SYM_CLASS_AUTOMATIC 1 Automatic (stack) variable. The Value field specifies stack frame offset. 
&IMAGE_SYM_CLASS_EXTERNAL 2 Used by Microsoft tools for external symbols. The Value field indicates the size if the section number is IMAGE_SYM_UNDEFINED (0). If the section number is not 0, then the Value field specifies the offset within the section. 
&IMAGE_SYM_CLASS_STATIC 3 The Value field specifies the offset of the symbol within the section. If the Value is 0, then the symbol represents a section name. 
&IMAGE_SYM_CLASS_REGISTER 4 Register variable. The Value field specifies register number. 
&IMAGE_SYM_CLASS_EXTERNAL_DEF 5 Symbol is defined externally. 
&IMAGE_SYM_CLASS_LABEL 6 Code label defined within the module. The Value field specifies the offset of the symbol within the section. 
&IMAGE_SYM_CLASS_UNDEFINED_LABEL 7 Reference to a code label not defined. 
&IMAGE_SYM_CLASS_MEMBER_OF_STRUCT 8 Structure member. The Value field specifies nth member. 
&IMAGE_SYM_CLASS_ARGUMENT 9 Formal argument (parameter)of a function. The Value field specifies nth argument. 
&IMAGE_SYM_CLASS_STRUCT_TAG 10 Structure tag-name entry. 
&IMAGE_SYM_CLASS_MEMBER_OF_UNION 11 Union member. The Value field specifies nth member. 
&IMAGE_SYM_CLASS_UNION_TAG 12 Union tag-name entry. 
&IMAGE_SYM_CLASS_TYPE_DEFINITION 13 Typedef entry. 
&IMAGE_SYM_CLASS_UNDEFINED_STATIC 14 Static data declaration. 
&IMAGE_SYM_CLASS_ENUM_TAG 15 Enumerated type tagname entry. 
&IMAGE_SYM_CLASS_MEMBER_OF_ENUM 16 Member of enumeration. Value specifies nth member. 
&IMAGE_SYM_CLASS_REGISTER_PARAM 17 Register parameter. 
&IMAGE_SYM_CLASS_BIT_FIELD 18 Bit-field reference. Value specifies nth bit in the bit field. 
&IMAGE_SYM_CLASS_BLOCK 100 A .bb (beginning of block) or .eb (end of block) record. Value is the relocatable address of the code location. 
&IMAGE_SYM_CLASS_FUNCTION 101 Used by Microsoft tools for symbol records that define the extent of a function: begin function (named .bf), end function (.ef), and lines in function (.lf). For .lf records, Value gives the number of source lines in the function. For .ef records, Value gives the size of function code. 
&IMAGE_SYM_CLASS_END_OF_STRUCT 102 End of structure entry. 
&IMAGE_SYM_CLASS_FILE 103 Used by Microsoft tools, as well as traditional COFF format, for the source-file symbol record. The symbol is followed by auxiliary records that name the file. 
&IMAGE_SYM_CLASS_SECTION 104 Definition of a section (Microsoft tools use STATIC storage class instead). 
&IMAGE_SYM_CLASS_WEAK_EXTERNAL 105 Weak external. See Section 5.5.3, �Auxiliary Format 3: Weak Externals,� for more information. 
;;
;;


[SymbolStorageClass: D$ 0]

WriteRelocStorageClassEquate:
    Push esi

        movzx eax B$esi | Mov D$SymbolStorageClass eax

        .If eax = &IMAGE_SYM_CLASS_ARGUMENT
            zCopy {B$ '&IMAGE_SYM_CLASS_ARGUMENT ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_AUTOMATIC
            zCopy {B$ '&IMAGE_SYM_CLASS_AUTOMATIC ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_BIT_FIELD
            zCopy {B$ '&IMAGE_SYM_CLASS_BIT_FIELD ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_BLOCK
            zCopy {B$ '&IMAGE_SYM_CLASS_BLOCK ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

         .Else_If eax = &IMAGE_SYM_CLASS_END_OF_FUNCTION
            zCopy {B$ '&IMAGE_SYM_CLASS_END_OF_FUNCTION ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

         .Else_If eax = &IMAGE_SYM_CLASS_END_OF_STRUCT
            zCopy {B$ '&IMAGE_SYM_CLASS_END_OF_STRUCT ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_ENUM_TAG
            zCopy {B$ '&IMAGE_SYM_CLASS_ENUM_TAG ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_EXTERNAL
            zCopy {B$ '&IMAGE_SYM_CLASS_EXTERNAL ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_EXTERNAL_DEF
            zCopy {B$ '&IMAGE_SYM_CLASS_EXTERNAL_DEF ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_FAR_EXTERNAL
            zCopy {B$ '&IMAGE_SYM_CLASS_FAR_EXTERNAL ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_FILE
            zCopy {B$ '&IMAGE_SYM_CLASS_FILE ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_FUNCTION
            zCopy {B$ '&IMAGE_SYM_CLASS_FUNCTION ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_LABEL
            zCopy {B$ '&IMAGE_SYM_CLASS_LABEL ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_MEMBER_OF_ENUM
            zCopy {B$ '&IMAGE_SYM_CLASS_MEMBER_OF_ENUM ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_MEMBER_OF_STRUCT
            zCopy {B$ '&IMAGE_SYM_CLASS_MEMBER_OF_STRUCT ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_MEMBER_OF_UNION
            zCopy {B$ '&IMAGE_SYM_CLASS_MEMBER_OF_UNION ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_NULL
            zCopy {B$ '&IMAGE_SYM_CLASS_NULL ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_REGISTER
            zCopy {B$ '&IMAGE_SYM_CLASS_REGISTER ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_REGISTER_PARAM
            zCopy {B$ '&IMAGE_SYM_CLASS_REGISTER_PARAM ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_SECTION
            zCopy {B$ '&IMAGE_SYM_CLASS_SECTION ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_STATIC
            zCopy {B$ '&IMAGE_SYM_CLASS_STATIC ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_STRUCT_TAG
            zCopy {B$ '&IMAGE_SYM_CLASS_STRUCT_TAG ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_TYPE_DEFINITION
            zCopy {B$ '&IMAGE_SYM_CLASS_TYPE_DEFINITION ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_UNDEFINED_LABEL
            zCopy {B$ '&IMAGE_SYM_CLASS_UNDEFINED_LABEL ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_UNDEFINED_STATIC
            zCopy {B$ '&IMAGE_SYM_CLASS_UNDEFINED_STATIC ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_UNION_TAG
            zCopy {B$ '&IMAGE_SYM_CLASS_UNION_TAG ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .Else_If eax = &IMAGE_SYM_CLASS_WEAK_EXTERNAL
            zCopy {B$ '&IMAGE_SYM_CLASS_WEAK_EXTERNAL ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        .End_If

    Pop esi
ret



Proc WriteSymbolTableHeaderItem:
    Argument @Text1, @Text2
    Uses edx;, eax

        Push esi
            Call WriteObjIndice
            zCopy {B$ 'ImgSym' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy SymbolTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Mov B$edi '.' | inc edi
            zCopy D@Text1
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy D@Text2
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi


                Mov eax D@Text1

                ..If D$eax+4 = 'Shor' ; from "NameShort" string

                    Mov edx esi | add edx 8
                    Mov B$edi "'" | inc edi
L0:                 lodsb
                    If al = 0
                        dec esi | jmp L1>
                    End_If
                    stosb | On esi < edx, jmp L0<
L1:                 Mov B$edi "'" | inc edi
                    While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

                ..Else_If D$eax+4 = 'Zero'; From "NameZero" string
                    lodsd | Call WriteEax

                ..Else_If D$eax+8 = 'Offs'; From "NameLongOffset" string

                    Call WriteSectionHeaderPointerToStringTableDiffLabel
                    lodsd | Call WriteEax

                ..Else_If D$eax = 'Valu'; From "Value" string

                    ;Call WriteCharacteristicsEquates
                    lodsd | Call WriteEax

                        .If D$esi-4 <> 0 ; If the Value member is not Zero, do next line
                            If B$esi+4 = &IMAGE_SYM_CLASS_SECTION ; If StorageClass member is IMAGE_SYM_CLASS_SECTION, do next line
                                Push esi
                                Push D$esi-4
                                Push eax
                                Mov eax D$esi-4
                                Call WriteCharacteristicsEquates
                                Pop eax
                                Pop D$esi-4
                                Pop esi

                            End_If
                        .End_If

                ..Else_If D$eax = 'Sect'; From "SectionNumber" string
                    Call WriteRelocSectionNumberEquate
                    xor eax eax
                    lodsw | Call WriteEax

                ..Else_If D$eax = 'Type'; From "Type1Complex" string
                    Call WriteSymbolTypeEquates
                    xor eax eax
                    lodsb | Call WriteEax

                ..Else_If D$eax = '1Bas'; From "Type1Base" string
                    Call WriteSymbolTypeEquates
                    xor eax eax
                    lodsb | Call WriteEax

                ..Else_If D$eax = 'Stor'; From "StorageClass" string

                    Call WriteRelocStorageClassEquate
                    xor eax eax
                    lodsb | Call WriteEax

                ..Else_If D$eax+8 = 'AuxS'; From "NumberOfAuxSymbols" string
                    xor eax eax
                    lodsb | Call WriteEax

;;
;;
                ..Else_If D$eax = 'Misc'; From "MiscVirtualSize" string

                    Call WriteSectionHeaderVirtualSizeDiffLabel
                    Push esi | zcopy {B$ " ; Hex Value:  " EOS} | Pop esi
;;
;;

                ;..Else

                    ;lodsd | Call WriteEax

                ..End_If


        Mov W$edi CRLF | add edi 2
EndP

____________________________________________________________

[SymbolTableIndex: B$ '000000' EOS]


InitSymbolIndexRecord:
    Mov D$SymbolTableIndex '0000', D$SymbolTableIndex+4 '00'
ret


IncrementSymbolIndexRecord:
    lea ebx D$SymbolTableIndex+5 | inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret

Proc WriteSymbolConstantIndex:
    Uses esi, ecx

            Mov B$edi '[' | inc edi
            Call WriteObjIndice

            ; Fix the label to show in Caps and replace the '." with an "_"
            ; The 1st char in "Obj000000." is in Caps, so we don't need to overwrite the "O" char

            Push edi
                Mov B$edi-1 '_', W$edi-9 'BJ'
            Pop edi

            zCopy {B$ 'SYMBOLINDEX' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy SymbolTableIndex
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Mov B$edi SPC | inc edi

            Mov esi SymbolTableIndex

            .If_And D$esi = '0000', D$SymbolTableIndex+4 = '00'

                Mov B$edi '0' | inc edi
            .Else

                Mov ecx 6 ; Size of the SymbolTable String
            L0:

                While B$esi <> '0'

                    L1:
                        movsb
                    Loop L1<
                    On ecx = 0, jmp @Out

                End_While

                inc esi
            Loop L0<

            .End_If
@Out:
            zCopy {B$ "] ; Symbol Index Constant used in IMAGE_RELOCATION Structure.", W$ CRLF, 0}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi


EndP

____________________________________________________________


; What do we need here ?
; CoffSymbolsPointer = Pointer to the Coff symbols Table
; CoffSymbolsNumber = Amount of Symbols Table structures

; The values where from GetCoffIMAGE_FILE_HEADER

[AuxSymbolCount: D$ 0]

[StringtableCheckFlag: D$ &FALSE]

[SymbolTableNameLongMessage: B$ "________________________________________________________________________

; Name Label indirectly addressed at NameLongOffset.
; The name of the Symbol is: " EOS]


WriteImageSymbolTable:

    ; Initialise the String Table Checking.

    Mov D$StringtableCheckFlag &FALSE

    Call InitSymbolTableRecord
    Call InitSymbolIndexRecord

    zCopy SymbolTableTitle
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    Mov esi D$CoffSectionBase
    add esi D$CoffSymbolsPointer

.Do

    If B$esi = 0

        Push esi
            zCopy SymbolTableNameLongMessage
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi

            Mov ecx D$PointerToStringTable

        Push esi
            add ecx D$esi+4 | Mov esi ecx
            While B$esi <> 0 | movsb | End_While
            zCopy {W$ CRLF, B$ "________________________________________________________________________", D$ CRLF2 0}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi

    End_If

        Call WriteSymbolConstantIndex
        Call IncrementSymbolIndexRecord ; Increment it for the next Normal Symbol or Auxiliary Symbol
        Mov W$edi CRLF | add edi 2

    .If_And W$esi+12 = &IMAGE_SYM_UNDEFINED, B$esi+16 = &IMAGE_SYM_CLASS_EXTERNAL
        Push esi
        zCopy {B$ "; This symbol is defined externally. It can be a Dll function or a runtime function", D$ CRLF2 0}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi

    .Else_If B$esi+16 = &IMAGE_SYM_CLASS_SECTION

        If W$esi+12 = &IMAGE_SYM_UNDEFINED
            Push esi
            zCopy {B$ "; This symbol represents the Section Name. It's not used because it is not defined in any Section." , W$ CRLF, B$ "; However, the value member is the Characteristics of a Section if existant." , D$ CRLF2 0}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Pop esi
        Else
            Push esi
            zCopy {B$ "; This symbol represents the Section Name. The value member is the Characteristics of the Section," , W$ CRLF, B$ "; but it may not be the same as what is found on the Section Structure.", D$ CRLF2 0}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Pop esi
        End_If
    .End_If

        Push esi
            Mov B$edi '[' | inc edi
            Call WriteObjIndice
            zCopy {B$ 'ImgSym' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy SymbolTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            Mov B$edi ':', W$edi+1 CRLF | add edi 3
        Pop esi

    .If B$esi = 0

        If D$esi+4 <> 0 ; need this flag to check if we have String Table pointer or not.
            Mov D$StringtableCheckFlag &TRUE
        End_If

        Call WriteSymbolTableHeaderItem SymbolTableLongNameZero, {B$ ': D$ ' EOS}
        Call WriteSymbolTableHeaderItem SymbolTableLongNameOffset, {B$ ': D$ ' EOS}

    .Else

        Call WriteSymbolTableHeaderItem SymbolTableShortName, {B$ ': B$ ' EOS}

    .End_If


    Call WriteSymbolTableHeaderItem SymbolTableValue, {B$ ': D$ ' EOS}
    Call WriteSymbolTableHeaderItem SymbolTableSectionNumber, {B$ ': W$ ' EOS}
    Call WriteSymbolTableHeaderItem SymbolTableTypeComplex, {B$ ': B$ ' EOS}
    Call WriteSymbolTableHeaderItem SymbolTableTypeBase, {B$ ': B$ ' EOS}
    Call WriteSymbolTableHeaderItem SymbolTableStorageClass, {B$ ': B$ ' EOS}
    Call WriteSymbolTableHeaderItem SymbolTableNumberOfAuxSymbols, {B$ ': B$ ' EOS}

    sub edi 2
    Mov B$edi ']' | inc edi ; Close the bracket

    dec D$CoffSymbolsNumber ; decrement our counter

    If B$esi-1 <> 0 ; Is Auxiliary Value = 0 ? No, do next line.

        movzx eax B$esi-1 | Mov D$AuxSymbolCount eax
        Mov D$edi CRLF2 | add edi 4
        Call WriteAuxiliarySymbolsRecords eax

    Else
        ; To prevent adding extra Paragraphs (8 Bytes) we need this Else macro, because the end of the
        ; auxiliary record before, already is adding 4 bytes (CRLF2) on exit.
        Mov D$edi CRLF2 | add edi 4

    End_If

        Call IncrementSymbolTableRecord

    .Loop_Until D$CoffSymbolsNumber = 0

ret


[PointerToStringTable: D$ 0]

; This function is used to calculate the Pointer to the String table. All we are doing is compute
; the real size of the Symbols Structures (With the Auxiliary Symbols) and adding it to the Coff Base

GetStringTablePointer:
    pushad

    Mov esi D$CoffSectionBase
    add esi D$CoffSymbolsPointer
    Mov ecx D$CoffSymbolsNumber

    .Do
        add esi 18 ; The size of each IMAGE_SYMBOL Structure
        dec ecx

        If B$esi-1 <> 0 ; Is Auxiliary Value = 0 ? No, do next line.
            movzx eax B$esi-1; | Mov D$AuxSymbolCount eax The amount of auxiliary symbols found on this Symbol structure
                ;Call WriteAuxiliarySymbolsRecords eax

            Do
                add esi 18 ; Add to the size of each Auxiliary Symbol
                dec ecx ;decrement our counter from the total amount of Symbols
                dec eax ; decrement the total amount of Auxiliary Symbols
            Loop_Until eax = 0 ; Did we finished all Auxiliary symbols for this Symbol ?

        End_If

    .Loop_Until ecx = 0

    Mov D$PointerToStringTable esi

    popad

ret



[StringTableRecord: B$ '000001' EOS]


InitStringTableRecord:
    Mov D$StringTableRecord '0000', D$StringTableRecord+4 '01'
ret


IncrementStringTableRecord:
    lea ebx D$StringTableRecord+5 | inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret


[StringTableTitle: "
_________________________________________________________

; Strings Table
_________________________________________________________


" EOS]


WriteImageSymbolStringsTable:

    ; Initialize the String Record Counter

    Call InitStringTableRecord

    Push esi
        zCopy StringTableTitle
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Mov B$edi '[' | inc edi
        Call WriteObjIndice
        zCopy {B$ 'StringTableSize: D$ ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Call WriteObjIndice
        zCopy {B$ 'StringDataEnd - ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Call WriteObjIndice
        zCopy {B$ 'StringTableSize] ; Hex Value:  ' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

    Pop esi

    Mov edx esi | add edx D$esi ; We will use edx as a counter delimiter for the strings

    lodsd | Call WriteEax
    Push esi | zCopy {D$ CRLF2, B$ "; Strings Array", D$ CRLF2 0} | Pop esi
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi


    Push esi
        Mov B$edi '[' | inc edi
        Call WriteObjIndice
        zCopy {B$ 'StringData' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Mov B$edi ':', W$edi+1 CRLF | add edi 3
    Pop esi


   .Do

        Push esi
            Call WriteObjIndice
            zCopy {B$ 'StringData' EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy StringTableRecord
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

            zCopy {B$ ": B$ '" EOS}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi


        While B$esi <> 0 | movsb | End_While | inc esi | On B$esi = 0, Mov edx 0

        Mov D$edi "', 0" | add edi 4
        Mov W$edi CRLF | add edi 2

        Call IncrementStringTableRecord

    .Loop_Until esi >= edx

    sub edi 2

       Push esi
            Mov W$edi CRLF | add edi 2
            Call WriteObjIndice
            zCopy {B$ "StringDataEnd:]", W$ CRLF, 0}
        Push esi | Mov esi {B$ '&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__' EOS} | L0: test B$esi 0_FF | jz P0> | movsb | jmp L0< | P0: Pop esi

        Pop esi

ret


____________________________________________________________________________________________

[CoffSectionBase: D$ ?]

GetCoffListing:
    Mov D$CoffSectionBase esi

    Call GetCoffIMAGE_FILE_HEADER

    If D$SizeOfOptionalHeaderInObj <> 0
        Push esi
            Call GetCoffIMAGE_OPTIONAL_HEADER
        Pop esi
        add esi D$SizeOfOptionalHeaderInObj
    End_If

    Call InitSectionHeaderNumber
    Mov ecx D$ObjNumberOfSections

    .If ecx <> 0 ; If Section NUmber is not 0, do all checkings

        Mov D$FirstSectionPointer esi

        Push esi, ecx
        L0:
            Push ecx
            Call GetCoffSectionHeader
            Call IncrementSectionHeaderNumber
            Pop ecx
        loop L0<
        Pop ecx, esi

        Call InitSectionHeaderNumber
        Call InitSectionRelocNumber
        Mov esi D$FirstSectionPointer

        If D$ObjNumberOfSections <> 0
            L0:
                Push ecx, esi
                Call IdentifyRawDataType
                Call GetCoffSectionsVariables
                Call WriteCoffSectionData
                On W$NumberOfRelocations <> 0, Call WriteCoffReloc
                On W$NumberOfLineNumbers <> 0, Call WriteCoffLineNumber
                Call IncrementSectionHeaderNumber
                Pop esi, ecx
                add esi 40
            loop L0<
        End_If

        If D$CoffSymbolsPointer <> 0
            Call GetStringTablePointer
            Call WriteImageSymbolTable
            ; When we don�t have any StringTable pointers, we bypass the next Call
            On D$StringtableCheckFlag = &TRUE, Call WriteImageSymbolStringsTable
        End_If

    .Else ; Else, if section number is 0, perform only the checkings for the Symbol Pointer. Only this because the Optional
          ; Header check were already performed. This was because a problem like this in corelibc.lib (Pelles file)

        If D$CoffSymbolsPointer <> 0
            Call GetStringTablePointer
            Call WriteImageSymbolTable
            ; When we don�t have any StringTable pointers, we bypass the next Call
            On D$StringtableCheckFlag = &TRUE, Call WriteImageSymbolStringsTable
        End_If


    .End_If

ret
____________________________________________________________________________________________
;;
;;
  Values set in 'FirstSectionPointer', 'CoffSectionSize', 'CoffPointerToData'
  by 'GetCoffSectionsVariables'.
;;
;;

__________________________________________________________________

Proc WriteRawDataLinkerDirectiveReport:
    Uses esi, eax, edx

        Mov edx esi | add edx ecx

        .If B$esi = 0
            Mov B$edi '0' | inc edi | inc esi
        .Else

            Mov D$edi '    ' | add edi 4
L0:         lodsb
            If al = 0
                dec esi | jmp L1>

            Else_If al = SPC
                Mov W$edi CRLF, D$edi+2 '    ' | add edi 6

            Else
                stosb
            End_If
            On esi < edx, jmp L0<

L1:

        .End_If
;ret
EndP

__________________________________________________________________

;WriteRelocStorageClassEquate

[DebugSIndexValue: D$ 0]

WriteDebugSIndexEquate:
    Push esi

        movzx eax W$esi | Mov D$DebugSIndexValue eax

        .If eax = &S_COMPILE
            zCopy {B$ '&S_COMPILE ; Hex Value:  ' EOS}
        .Else_If eax = &S_REGISTER
            zCopy {B$ '&S_REGISTER ; Hex Value:  ' EOS}
        .Else_If eax = &S_CONSTANT
            zCopy {B$ '&S_CONSTANT ; Hex Value:  ' EOS}
        .Else_If eax = &S_UDT
            zCopy {B$ '&S_UDT ; Hex Value:  ' EOS}
        .Else_If eax = &S_SSEARCH
            zCopy {B$ '&S_SSEARCH ; Hex Value:  ' EOS}
        .Else_If eax = &S_END
            zCopy {B$ '&S_END ; Hex Value:  ' EOS}
        .Else_If eax = &S_SKIP
            zCopy {B$ '&S_SKIP ; Hex Value:  ' EOS}
        .Else_If eax = &S_CVRESERVE
            zCopy {B$ '&S_CVRESERVE ; Hex Value:  ' EOS}
        .Else_If eax = &S_OBJNAME
            zCopy {B$ '&S_OBJNAME ; Hex Value:  ' EOS}
        .Else_If eax = &S_ENDARG
            zCopy {B$ '&S_ENDARG ; Hex Value:  ' EOS}
        .Else_If eax = &S_COBOLUDT
            zCopy {B$ '&S_COBOLUDT ; Hex Value:  ' EOS}
        .Else_If eax = &S_MANYREG
            zCopy {B$ '&S_MANYREG ; Hex Value:  ' EOS}
        .Else_If eax = &S_RETURN
            zCopy {B$ '&S_RETURN ; Hex Value:  ' EOS}
        .Else_If eax = &S_ENTRYTHIS
            zCopy {B$ '&S_ENTRYTHIS ; Hex Value:  ' EOS}
        .Else_If eax = &S_BPREL16
            zCopy {B$ '&S_BPREL16 ; Hex Value:  ' EOS}
        .Else_If eax = &S_LDATA16
            zCopy {B$ '&S_LDATA16 ; Hex Value:  ' EOS}
        .Else_If eax = &S_GDATA16
            zCopy {B$ '&S_GDATA16 ; Hex Value:  ' EOS}
        .Else_If eax = &S_PUB16
            zCopy {B$ '&S_PUB16 ; Hex Value:  ' EOS}
        .Else_If eax = &S_LPROC16
            zCopy {B$ '&S_LPROC16 ; Hex Value:  ' EOS}
        .Else_If eax = &S_GPROC16
            zCopy {B$ '&S_GPROC16 ; Hex Value:  ' EOS}
        .Else_If eax = &S_THUNK16
            zCopy {B$ '&S_THUNK16 ; Hex Value:  ' EOS}
        .Else_If eax = &S_BLOCK16
            zCopy {B$ '&S_BLOCK16 ; Hex Value:  ' EOS}
        .Else_If eax = &S_WITH16
            zCopy {B$ '&S_WITH16 ; Hex Value:  ' EOS}
        .Else_If eax = &S_LABEL16
            zCopy {B$ '&S_LABEL16 ; Hex Value:  ' EOS}
        .Else_If eax = &S_CEXMODEL16
            zCopy {B$ '&S_CEXMODEL16 ; Hex Value:  ' EOS}
        .Else_If eax = &S_VFTPATH16
            zCopy {B$ '&S_VFTPATH16 ; Hex Value:  ' EOS}
        .Else_If eax = &S_REGREL16
            zCopy {B$ '&S_REGREL16 ; Hex Value:  ' EOS}
        .Else_If eax = &S_BPREL32
            zCopy {B$ '&S_BPREL32 ; Hex Value:  ' EOS}
        .Else_If eax = &S_LDATA32
            zCopy {B$ '&S_LDATA32 ; Hex Value:  ' EOS}
        .Else_If eax = &S_GDATA32
            zCopy {B$ '&S_GDATA32 ; Hex Value:  ' EOS}
        .Else_If eax = &S_PUB32
            zCopy {B$ '&S_PUB32 ; Hex Value:  ' EOS}
        .Else_If eax = &S_LPROC32
            zCopy {B$ '&S_LPROC32 ; Hex Value:  ' EOS}
        .Else_If eax = &S_GPROC32
            zCopy {B$ '&S_GPROC32 ; Hex Value:  ' EOS}
        .Else_If eax = &S_THUNK32
            zCopy {B$ '&S_THUNK32 ; Hex Value:  ' EOS}
        .Else_If eax = &S_BLOCK32
            zCopy {B$ '&S_BLOCK32 ; Hex Value:  ' EOS}
        .Else_If eax = &S_VFTPATH32
            zCopy {B$ '&S_VFTPATH32 ; Hex Value:  ' EOS}
        .Else_If eax = &S_REGREL32
            zCopy {B$ '&S_REGREL32 ; Hex Value:  ' EOS}
        .Else_If eax = &S_LTHREAD32
            zCopy {B$ '&S_LTHREAD32 ; Hex Value:  ' EOS}
        .Else_If eax = &S_GTHREAD32
            zCopy {B$ '&S_GTHREAD32 ; Hex Value:  ' EOS}
        .Else_If eax = &S_LPROCMIPS
            zCopy {B$ '&S_LPROCMIPS ; Hex Value:  ' EOS}
        .Else_If eax = &S_GPROCMIPS
            zCopy {B$ '&S_GPROCMIPS ; Hex Value:  ' EOS}
        .Else_If eax = &S_PROCREF
            zCopy {B$ '&S_PROCREF ; Hex Value:  ' EOS}
        .Else_If eax = &S_DATAREF
            zCopy {B$ '&S_DATAREF ; Hex Value:  ' EOS}
        .Else_If eax = &S_ALIGN
            zCopy {B$ '&S_ALIGN ; Hex Value:  ' EOS}
        .Else_If eax = &S_BLOCK_CV2
            zCopy {B$ '&S_BLOCK_CV2; Hex Value:  ' EOS}
        .Else_If eax = &S_BPREL_CV2
            zCopy {B$ '&S_BPREL_CV2; Hex Value:  ' EOS}
        .Else_If eax = &S_BPREL32_CV3
            zCopy {B$ '&S_BPREL32_CV3; Hex Value:  ' EOS}
        .Else_If eax = &S_COBOLUDT_CV3
            zCopy {B$ '&S_COBOLUDT_CV3; Hex Value:  ' EOS}
        .Else_If eax = &S_COMPILE_CV2
            zCopy {B$ '&S_COMPILE_CV2; Hex Value:  ' EOS}
        .Else_If eax = &S_COMPILE_CV3
            zCopy {B$ '&S_COMPILE_CV3; Hex Value:  ' EOS}
        .Else_If eax = &S_CONSTANT_CV2
            zCopy {B$ '&S_CONSTANT_CV2; Hex Value:  ' EOS}
        .Else_If eax = &S_CONSTANT_CV3
            zCopy {B$ '&S_CONSTANT_CV3; Hex Value:  ' EOS}
        .Else_If eax = &S_GDATA_CV2
            zCopy {B$ '&S_GDATA_CV2; Hex Value:  ' EOS}
        .Else_If eax = &S_GDATA_CV3
            zCopy {B$ '&S_GDATA_CV3; Hex Value:  ' EOS}
        .Else_If eax = &S_GPROC_CV2
            zCopy {B$ '&S_GPROC_CV2; Hex Value:  ' EOS}
        .Else_If eax = &S_GPROC32_CV3
            zCopy {B$ '&S_GPROC32_CV3; Hex Value:  ' EOS}
        .Else_If eax = &S_GTHREAD_CV3
            zCopy {B$ '&S_GTHREAD_CV3; Hex Value:  ' EOS}
        .Else_If eax = &S_LABEL_CV2
            zCopy {B$ '&S_LABEL_CV2; Hex Value:  ' EOS}
        .Else_If eax = &S_LDATA_CV2
            zCopy {B$ '&S_LDATA_CV2; Hex Value:  ' EOS}
        .Else_If eax = &S_LDATA_CV3
            zCopy {B$ '&S_LDATA_CV3; Hex Value:  ' EOS}
        .Else_If eax = &S_LPROC_CV2
            zCopy {B$ '&S_LPROC_CV2; Hex Value:  ' EOS}
        .Else_If eax = &S_LPROC_CV3
            zCopy {B$ '&S_LPROC_CV3; Hex Value:  ' EOS}
        .Else_If eax = &S_LTHREAD_CV3
            zCopy {B$ '&S_LTHREAD_CV3; Hex Value:  ' EOS}
        .Else_If eax = &S_MANYREG_CV3
            zCopy {B$ '&S_MANYREG_CV3; Hex Value:  ' EOS}
        .Else_If eax = &S_MSTOOL_CV2
            zCopy {B$ '&S_MSTOOL_CV2; Hex Value:  ' EOS}
        .Else_If eax = &S_PUB_CV3
            zCopy {B$ '&S_PUB_CV3; Hex Value:  ' EOS}
        .Else_If eax = &S_PUB_DATA_CV2
            zCopy {B$ '&S_PUB_DATA_CV2; Hex Value:  ' EOS}
        .Else_If eax = &S_PUB_FUNC1_CV2
            zCopy {B$ '&S_PUB_FUNC1_CV2; Hex Value:  ' EOS}
        .Else_If eax = &S_PUB_FUNC2_CV2
            zCopy {B$ '&S_PUB_FUNC2_CV2; Hex Value:  ' EOS}
        .Else_If eax = &S_REGISTER_CV3
            zCopy {B$ '&S_REGISTER_CV3; Hex Value:  ' EOS}
        .Else_If eax = &S_REGREL_CV3
            zCopy {B$ '&S_REGREL_CV3; Hex Value:  ' EOS}
        .Else_If eax = &S_THUNK_CV2
            zCopy {B$ '&S_THUNK_CV2; Hex Value:  ' EOS}
        .Else_If eax = &S_UDT_CV2
            zCopy {B$ '&S_UDT_CV2; Hex Value:  ' EOS}
        .Else_If eax = &S_UDT_CV3
            zCopy {B$ '&S_UDT_CV3; Hex Value:  ' EOS}
        .Else_If eax = &S_VFTTABLE_CV3
            zCopy {B$ '&S_VFTTABLE_CV3; Hex Value:  ' EOS}
        .End_If
    Pop esi
ret


__________________________________________________________________

Proc WriteRawDataDebugSItem:
    Argument @Text1, @Text2
    uses eax

        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice

            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy D@Text1
            zCopy D@Text2
        Pop esi

        Mov eax D@Text1

        If D$eax = 'Leng' ; from "Length" string
            xor eax eax
            lodsw | Call WriteEax

        Else_If D$eax = 'Inde'; From "Index" string
            Call WriteDebugSIndexEquate
            xor eax eax
            lodsw | Call WriteEax

        End_If

        Mov W$edi CRLF | add edi 2
EndP

__________________________________________________________________


Proc WriteCVdataTitle:
    Uses esi, ecx

    Mov D$edi '    ' | add edi 4
    Call WriteObjIndice
    zCopy {B$ "Sec" EOS}
    zCopy SectionHeaderNumber
    zCopy {B$ ".Index" EOS}
    zCopy DebugNumber
    zCopy D$CVLabel
    zCopy {B$ 'DummyBytes' EOS}
    If D$NestedLeafType = &TRUE
        zCopy {B$ '.Arr' EOS}
        zCopy LeafTypeArrayObjIndice
    End_If
    Mov D$edi ': B$', B$edi+4 SPC | add edi 5

EndP
__________________________________________________________________

[DebugNumber: B$ '000001' EOS]


InitSectionDebugNumber:
    Mov D$DebugNumber '0000', D$DebugNumber+4 '01'
ret


IncrementSectionDebugNumber:
    lea ebx D$DebugNumber+5 | inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret


__________________________________________________________________


__________________________________________________________________
;;
;;
Flag1 Member:

The Flag1 member is a Byte value responsable for several different computatiions accordying to specified bits.
We need to shr the founded values to find the proper equates used.


PCodePresent : 1 Uses Bit 0. When the bit is flagged the file uses PCode. Otherwise it don't use.
                Equates :   CV4_PCODE_ENABLED 01
                            CV4_PCODE_DISABLED 0

FloatPrecision :2   Uses Bits 1 and 2. The FloatPrecision flag is set to 1 if the compiler follows the
                    ANSI C floating-point precision rules. This is specified for Microsoft C compilers
                    by setting the -Op option. So, i builded 4 equates:
    
                    CV4_FP_PRECISION_RESERVED1 0
                    CV4_FP_PRECISION_ANSI 1
                    CV4_FP_PRECISION_RESERVED2 2
                    CV4_FP_PRECISION_RESERVED3 3

FloatPackage :2     Uses bits 3 and 4. The equates used are:

                    CV4_FP8087_PROCESSOR 0 Hardware processor (80x87 for Intel 80x86 processors)
                    CV4_FP8087_EMULATOR 1 Emulator
                    CV4_FP8087_ALTMATH 2 Altmath
                    CV4_FP8087_RESERVED 3 Reserved

AmbientData :3      Uses bits 5, 6, 7 . The equates used are:

                    Ambient code and data memory model enumeration:
                    CV4_AMBIENTDATA_NEAR 0 Near
                    CV4_AMBIENTDATA_FAR 1 Far
                    CV4_AMBIENTDATA_HUGE 2 Huge
                    CV4_AMBIENTDATA_RESERVED3 3 Reserved
                    CV4_AMBIENTDATA_RESERVED4 4 Reserved
                    CV4_AMBIENTDATA_RESERVED5 5 Reserved
                    CV4_AMBIENTDATA_RESERVED6 6 Reserved
                    CV4_AMBIENTDATA_RESERVED7 7 Reserved

                    Note: Values  3 - 7 Reserved
;;

;;
Proc WriteCVCompileFlag1Equates:
    Uses esi, ebx, eax

    xor eax eax | lodsb | Mov ebx eax

    Push ebx
    Push eax

    shr ebx 5 ; This is to calculate AmbientData. Shr by 5 because the Bit 5 is the one that is starting to be flagged.

    .If ebx = &CV4_AMBIENTDATA_NEAR
        zCopy {B$ '(&CV4_AMBIENTDATA_NEAR shl 5)' EOS}
    .Else_If ebx = &CV4_AMBIENTDATA_FAR
        zCopy {B$ '(&CV4_AMBIENTDATA_FAR shl 5)' EOS}
    .Else_If ebx = &CV4_AMBIENTDATA_HUGE
        zCopy {B$ '(&CV4_AMBIENTDATA_HUGE shl 5)' EOS}
    .Else_If ebx = &CV4_AMBIENTDATA_RESERVED3
        zCopy {B$ '(&CV4_AMBIENTDATA_RESERVED3 shl 5)' EOS}
    .Else_If ebx = &CV4_AMBIENTDATA_RESERVED4
        zCopy {B$ '(&CV4_AMBIENTDATA_RESERVED4 shl 5)' EOS}
    .Else_If ebx = &CV4_AMBIENTDATA_RESERVED5
        zCopy {B$ '(&CV4_AMBIENTDATA_RESERVED5 shl 5)' EOS}
    .Else_If ebx = &CV4_AMBIENTDATA_RESERVED6
        zCopy {B$ '(&CV4_AMBIENTDATA_RESERVED6 shl 5)' EOS}
    .Else
        zCopy {B$ '(&CV4_AMBIENTDATA_RESERVED7 shl 5)' EOS}
    .End_If

        zCopy {B$ ' + ' EOS}
        ; Now we must Zero bits 5 to 7 to shr again to find the next records for FloatPackage (Bits 3-4)
        btr eax 5 | btr eax 6 | btr eax 7
        ; After clearing the bits, we need to compute only the needed bits to be shred
        shr eax 3 ; This is to calculate FloatPackage. Shr by 3 because the Bit 3 is the one that is starting to be flagged.

    .If eax = &CV4_FP8087_PROCESSOR
        zCopy {B$ '(&CV4_FP8087_PROCESSOR shl 3)' EOS}
    .Else_If eax = &CV4_FP8087_EMULATOR
        zCopy {B$ '(&CV4_FP8087_EMULATOR shl 3)' EOS}
    .Else_If eax = &CV4_FP8087_ALTMATH
        zCopy {B$ '(&CV4_FP8087_ALTMATH shl 3)' EOS}
    .Else
        zCopy {B$ '(&CV4_FP8087_RESERVED shl 3)' EOS}
    .End_If

    Pop eax
    Pop ebx

        zCopy {B$ ' + ' EOS}

    ; Now we restored the values of ebx and eax. We will going to find the values for
    ; PCodePresent (Bit 0) and FloatPrecision (Bits 1 and 2)
    ; Since we restored the values we must clear bits 3 to 7

    ; zeroes bits 3 to 7
    btr eax 3 | btr eax 4 | btr eax 5 | btr eax 6 | btr eax 7


    shr eax 1 ; This is to calculate FloatPrecision. Shr by 1 because the Bit 1 is the one that is starting to be flagged.

    .If eax = &CV4_FP_PRECISION_RESERVED1
        zCopy {B$ '(&CV4_FP_PRECISION_RESERVED1 shl 1)' EOS}
    .Else_If eax = &CV4_FP_PRECISION_ANSI
        zCopy {B$ '(&CV4_FP_PRECISION_ANSI shl 1)' EOS}
    .Else_If eax = &CV4_FP_PRECISION_RESERVED2
        zCopy {B$ '(&CV4_FP_PRECISION_RESERVED2 shl 1)' EOS}
    .Else
        zCopy {B$ '(&CV4_FP_PRECISION_RESERVED3 shl 1)' EOS}
    .End_If

        zCopy {B$ ' + ' EOS}

    and ebx 01 ; This is to calculate PCodePresent.
               ; The value of 1 is because it is the maximum value of all bits flagged. (Bit 0)

    .If ebx = &CV4_PCODE_ENABLED
        zCopy {B$ '&CV4_PCODE_ENABLED' EOS}
    .Else
        zCopy {B$ '&CV4_PCODE_DISABLED' EOS}
    .End_If

        zCopy {B$ ' ; Hex Value:  ' EOS}

EndP

;;
;;
Flag2 Member:

The Flag2 member is a Byte value responsable for several different computatiions accordying to specified bits.
We need to shr the founded values to find the proper equates used.


AmbientCode :3  Uses bits 0, 1 , 2. The equates used are:

                CV4_AMBIENTCODE_NEAR 0 Near
                CV4_AMBIENTCODE_FAR 1 Far
                CV4_AMBIENTCODE_HUGE 2 Huge
                CV4_AMBIENTCODE_RESERVED3 3 Reserved
                CV4_AMBIENTCODE_RESERVED4 4 Reserved
                CV4_AMBIENTCODE_RESERVED5 5 Reserved
                CV4_AMBIENTCODE_RESERVED6 6 Reserved
                CV4_AMBIENTCODE_RESERVED7 7 Reserved

                Note: Values  3 - 7 Reserved

Mode32 :1       Uses Bit 3. When the bit is flagged the file is compiled for 32-bit addresses, otherwise it is not.
                Equates used are:
                
                CV4_MODE32_ENABLED 1 ; Compiled for 32-bit addresses
                CV4_MODE32_DISABLED 0  ; Not compiled for 32-bit addresses

Reserved :4     Used Bits 4,5,6,7. Those bits are reserved. So, they shoulnd't be used, we can set them all to 0.

;;
;;
_______________________________________________

Proc WriteCVCompileFlag2Equates:
    Uses esi, ebx, eax

    If B$esi > 15
        xor eax eax
        lodsb | btr eax 4 | btr eax 5 | btr eax 6 | btr eax 7 ; Clear Bits 4 to 7, that are what exceed the limit of 15
        Mov ebx eax
    Else
        xor eax eax | lodsb | Mov ebx eax
    End_If

    and eax 07 ; This is to calculate AmbientCode.
               ; The value of 7 is because it is the maximum value of all bits flagged. (0 to 3)

    shr ebx 3 ; This is to calculate Mode32. Shr by 3 because the Bit 3 is the one that is flagged.

    If ebx = &CV4_MODE32_ENABLED
        zCopy {B$ '(&CV4_MODE32_ENABLED shl 3)' EOS}
    Else
        zCopy {B$ '(&CV4_MODE32_DISABLED shl 3)' EOS}
    End_If

    .If eax = &CV4_AMBIENTCODE_NEAR
        zCopy {B$ ' + &CV4_AMBIENTCODE_NEAR' EOS}
    .Else_If eax = &CV4_AMBIENTCODE_FAR
        zCopy {B$ ' + &CV4_AMBIENTCODE_FAR' EOS}
    .Else_If eax = &CV4_AMBIENTCODE_HUGE
        zCopy {B$ ' + &CV4_AMBIENTCODE_HUGE' EOS}
    .Else_If eax = &CV4_AMBIENTCODE_RESERVED3
        zCopy {B$ ' + &CV4_AMBIENTCODE_RESERVED3' EOS}
    .Else_If eax = &CV4_AMBIENTCODE_RESERVED4
        zCopy {B$ ' + &CV4_AMBIENTCODE_RESERVED4' EOS}
    .Else_If eax = &CV4_AMBIENTCODE_RESERVED5
        zCopy {B$ ' + &CV4_AMBIENTCODE_RESERVED5' EOS}
    .Else_If eax = &CV4_AMBIENTCODE_RESERVED6
        zCopy {B$ ' + &CV4_AMBIENTCODE_RESERVED6' EOS}
    .Else
        zCopy {B$ ' + &CV4_AMBIENTCODE_RESERVED7' EOS}
    .End_If
        zCopy {B$ ' ; Hex Value:  ' EOS}

EndP
_______________
;;
;;
Used Equates. All that is not listed is reserved

&CV4_COMPILE_LANG_C 0 ; C
&CV4_COMPILE_LANG_CPLUS 01 ; C++
&CV4_COMPILE_LANG_FORTRAN 02; Fortran
&CV4_COMPILE_LANG_MASM 03; Masm
&CV4_COMPILE_LANG_PASCAL 04; Pascal
&CV4_COMPILE_LANG_BASIC 05; Basic
&CV4_COMPILE_LANG_COBOL 06; Cobol

;;
;;
Proc WriteCVCompileLanguageEquates:
    Uses esi

    .If B$esi = &CV4_COMPILE_LANG_C
        zCopy {B$ '&CV4_COMPILE_LANG_C ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_LANG_CPLUS
        zCopy {B$ '&CV4_COMPILE_LANG_CPLUS ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_LANG_FORTRAN
        zCopy {B$ '&CV4_COMPILE_LANG_FORTRAN ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_LANG_MASM
        zCopy {B$ '&CV4_COMPILE_LANG_MASM ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_LANG_PASCAL
        zCopy {B$ '&CV4_COMPILE_LANG_PASCAL ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_LANG_BASIC
        zCopy {B$ '&CV4_COMPILE_LANG_BASIC ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_LANG_COBOL
        zCopy {B$ '&CV4_COMPILE_LANG_COBOL ; Hex Value:  ' EOS}
    .End_If


EndP

_______________
;;
;;

Used Equates. All that is not listed is reserved

&CV4_COMPILE_MACHINE_I80 ; Intel 8080
&CV4_COMPILE_MACHINE_I86 ; Intel 8086
&CV4_COMPILE_MACHINE_I286 ; Intel 80286
&CV4_COMPILE_MACHINE_I386 ; Intel 80386
&CV4_COMPILE_MACHINE_I486 ; Intel 80486
&CV4_COMPILE_MACHINE_I586 ; Intel Pentium
&CV4_COMPILE_MACHINE_R4000 ; MIPS R4000
&CV4_COMPILE_MACHINE_MIPSRESERVED1 ; Reserved for future MIPS processor
&CV4_COMPILE_MACHINE_MIPSRESERVED2 ; Reserved for future MIPS processor
&CV4_COMPILE_MACHINE_MC68000 ; MC68000
&CV4_COMPILE_MACHINE_MC68010 ; MC68010
&CV4_COMPILE_MACHINE_MC68020 ; MC68020
&CV4_COMPILE_MACHINE_MC68030 ; MC68030
&CV4_COMPILE_MACHINE_MC68040 ; MC68040
&CV4_COMPILE_MACHINE_ALPHA ; DEC Alpha
;;
;;

Proc WriteCVCompileMachineEquates:
    Uses esi

    .If B$esi = &CV4_COMPILE_MACHINE_I80
        zCopy {B$ '&CV4_COMPILE_MACHINE_I80 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_I86
        zCopy {B$ '&CV4_COMPILE_MACHINE_I86 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_I286
        zCopy {B$ '&CV4_COMPILE_MACHINE_I286 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_I386
        zCopy {B$ '&CV4_COMPILE_MACHINE_I386 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_I486
        zCopy {B$ '&CV4_COMPILE_MACHINE_I486 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_I586
        zCopy {B$ '&CV4_COMPILE_MACHINE_I586 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_R4000
        zCopy {B$ '&CV4_COMPILE_MACHINE_R4000 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_MIPSRESERVED1
        zCopy {B$ '&CV4_COMPILE_MACHINE_MIPSRESERVED1 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_MIPSRESERVED2
        zCopy {B$ '&CV4_COMPILE_MACHINE_MIPSRESERVED2 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_MC68000
        zCopy {B$ '&CV4_COMPILE_MACHINE_MC68000 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_MC68010
        zCopy {B$ '&CV4_COMPILE_MACHINE_MC68010 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_MC68020
        zCopy {B$ '&CV4_COMPILE_MACHINE_MC68020 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_MC68030
        zCopy {B$ '&CV4_COMPILE_MACHINE_MC68030 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_MC68040
        zCopy {B$ '&CV4_COMPILE_MACHINE_MC68040 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_ALPHA
        zCopy {B$ '&CV4_COMPILE_MACHINE_ALPHA ; Hex Value:  ' EOS}
    .End_If


EndP
________________

Proc WriteRawDataDebugSCompileItem:
    Argument @Text1, @Text2, @DataSize
    uses eax, ecx

        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'Compile.' EOS}
            zCopy D@Text1
            zCopy D@Text2
        Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Mach' ; from "Machine" string
            Call WriteCVCompileMachineEquates
            If D@DataSize = 4
                lodsd | Call WriteEax
            Else
                xor eax eax
                lodsb | Call WriteEax
            End_If
        ..Else_If D$eax = 'Lang'; From "Language" string
            Call WriteCVCompileLanguageEquates
            If D@DataSize = 4
                lodsd | Call WriteEax
            Else
                xor eax eax
                lodsb | Call WriteEax
            End_If

        ..Else_If D$eax+2 = 'ags1'; From "Flags1" string
            Call WriteCVCompileFlag1Equates
            If D@DataSize = 4
                lodsd | Call WriteEax
            Else
                xor eax eax
                lodsb | Call WriteEax
            End_If

        ..Else_If D$eax+2 = 'ags2'; From "Flags2" string
            Call WriteCVCompileFlag2Equates
            If D@DataSize = 4
                lodsd | Call WriteEax
            Else
                xor eax eax
                lodsb | Call WriteEax
            End_If

        ..Else_If D$eax = 'Unkn'; From "Unknown" string
            If D@DataSize = 4
                lodsd | Call WriteEax
            Else
                xor eax eax
                lodsw | Call WriteEax
            End_If

        ..Else_If D$eax = 'Comp'; From "CompilerID" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            If D@DataSize = 2 ; this happens only in S_COMPILE_CV2. It�' size is not 2, but we use this to identify this type.
                Push eax
                    Call StrLenProc esi
                    Mov ecx eax
                    inc ecx ; The size is increased to we alow including the zero byte
                Pop eax
            Else
                movzx ecx B$esi-1
            End_If

            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2
EndP

_________________________________________

[CVNonLeafIndexPad03Unknown01: B$ 'Unknown01' EOS]
[CVNonLeafIndexPad03Unknown02: B$ 'Unknown02' EOS]
[CVNonLeafIndexPad03Unknown03: B$ 'Unknown03' EOS]
[CVNonLeafIndexPad03NameLenght: B$ 'NameLenght' EOS]
[CVNonLeafIndexPad03Name: B$ 'Name' EOS]

Proc WriteRawDataDebugSNonLeafIndexPad03:
    Uses ecx, eax, ebx
    ; This is to avoid that the Structure have dummy bytes at the end.
   ; Mov ebx esi
  ;  sub ebx 2
 ;   Mov ecx D$SizeAdd
;    add ebx ecx

;    Push ebx

    Call WriteRawDataDebugSNonLeafIndexPad03Item CVNonLeafIndexPad03Unknown01, {B$ ': B$ ' EOS}
    Call WriteRawDataDebugSNonLeafIndexPad03Item CVNonLeafIndexPad03Unknown02, {B$ ': B$ ' EOS}
    Call WriteRawDataDebugSNonLeafIndexPad03Item CVNonLeafIndexPad03Unknown03, {B$ ': B$ ' EOS}
    Call WriteRawDataDebugSNonLeafIndexPad03Item CVNonLeafIndexPad03NameLenght, {B$ ': D$ ' EOS}

    Mov ebx esi
    Mov ecx D$SizeAdd
    add ebx ecx
    add ebx D$esi-4

    If D$esi-4 <> 0 ; is the Name Lenght = 0 ? jmp over.

        Push ebx
            Call WriteRawDataDebugSNonLeafIndexPad03Item CVNonLeafIndexPad03Name, {B$ ': B$ ' EOS}
        Pop ebx

    End_If
    ; Is the end of this structure ends on the proper place ?

    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________

Proc WriteRawDataDebugSNonLeafIndexPad03Item:
    Argument @Text1, @Text2
    uses eax, ecx, ebx

        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'NoIndexLeafPad03.' EOS}
            zCopy D@Text1
            zCopy D@Text2
        Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Unkn' ; from "Unknown01", "Unknown02", "Unknown03" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            lodsd | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-4
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2
EndP

________________________

[CVCompileMachine: B$ 'Machine' EOS]
[CVCompileLanguage: B$ 'Language' EOS]
[CVCompileFlags1: B$ 'Flags1' EOS]
[CVCompileFlags2: B$ 'Flags2' EOS]
[CVCompileUnknown: B$ 'Unknown' EOS]
[CVCompileCompilerID: B$ 'CompilerID' EOS]
[CVCompileNameLenght: B$ 'NameLenght' EOS]
[CVCompileName: B$ 'Name' EOS]


Proc WriteRawDataDebugSCompile:
    Uses ecx, eax, ebx
    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx

    .If W$esi-2 = &S_COMPILE
        Call WriteRawDataDebugSCompileItem CVCompileMachine, {B$ ': B$ ' EOS}, 1
        Call WriteRawDataDebugSCompileItem CVCompileLanguage, {B$ ': B$ ' EOS}, 1
        Call WriteRawDataDebugSCompileItem CVCompileFlags1, {B$ ': B$ ' EOS}, 1
        Call WriteRawDataDebugSCompileItem CVCompileFlags2, {B$ ': B$ ' EOS}, 1
        Call WriteRawDataDebugSCompileItem CVCompileNameLenght, {B$ ': B$ ' EOS}, 1
        Call WriteRawDataDebugSCompileItem CVCompileName, {B$ ': B$ ' EOS}, 1
    .Else_If W$esi-2 = &S_COMPILE_CV3
        Call WriteRawDataDebugSCompileItem CVCompileLanguage, {B$ ': D$ ' EOS}, 4
        Call WriteRawDataDebugSCompileItem CVCompileMachine, {B$ ': D$ ' EOS}, 4
        Call WriteRawDataDebugSCompileItem CVCompileFlags2, {B$ ': D$ ' EOS}, 4
        Call WriteRawDataDebugSCompileItem CVCompileFlags1, {B$ ': D$ ' EOS}, 4
        Call WriteRawDataDebugSCompileItem CVCompileCompilerID, {B$ ': W$ ' EOS}, 2
        Call WriteRawDataDebugSCompileItem CVCompileNameLenght, {B$ ': B$ ' EOS}, 1
        Call WriteRawDataDebugSCompileItem CVCompileName, {B$ ': B$ ' EOS}, 1

    .Else_If W$esi-2 = &S_COMPILE_CV2
        ; The members below seems to be a variation of the S_OBJNAME structured data.
        ; The Unknown seems to be the Signature
        ; The name is the object name, with full path. (And not the name of the object only as in S_OBJECT)
        Call WriteRawDataDebugSCompileItem CVCompileUnknown, {B$ ': D$ ' EOS}, 4
        Call WriteRawDataDebugSCompileItem CVCompileName, {B$ ': B$ ' EOS}, 2
    .End_If

    Pop ebx

    ; Is the end of this structure ends on the proper place ?

    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP


________________________________________________________________________________

[CVMsToolMachine: B$ 'Machine' EOS]
[CVMsToolLanguage: B$ 'Language' EOS]
[CVMsToolUnknown1: B$ 'Unknown1' EOS]
[CVMsToolUnknown2: B$ 'Unknown2' EOS]
[CVMsToolUnknown3: B$ 'Unknown3' EOS]
[CVMsToolUnknown4: B$ 'Unknown4' EOS]
[CVMsToolUnknown5: B$ 'Unknown5' EOS]
[CVMsToolUnknown6: B$ 'Unknown6' EOS]
[CVMsToolUnknown7: B$ 'Unknown7' EOS]
[CVMsToolUnknown8: B$ 'Unknown8' EOS]
[CVMsToolName: B$ 'Name' EOS]

Proc WriteRawDataDebugSMsTool:
    Uses ecx, eax, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx

    .If W$esi-2 = &S_MSTOOL_CV2
        Call WriteRawDataDebugSMsToolItem CVMsToolMachine, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSMsToolItem CVMsToolLanguage, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSMsToolItem CVMsToolUnknown1, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSMsToolItem CVMsToolUnknown2, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSMsToolItem CVMsToolUnknown3, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSMsToolItem CVMsToolUnknown4, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSMsToolItem CVMsToolUnknown5, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSMsToolItem CVMsToolUnknown6, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSMsToolItem CVMsToolUnknown7, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSMsToolItem CVMsToolUnknown8, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSMsToolItem CVMsToolName, {B$ ': B$ ' EOS}
    .End_If

    Pop ebx

    ; Is the end of this structure ends on the proper place ?

    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP
_________________________________

Proc WriteRawDataDebugSMsToolItem:
    Argument @Text1, @Text2
    uses eax, ecx

        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'MsTool.' EOS}
            zCopy D@Text1
            zCopy D@Text2
        Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Mach' ; from "Machine" string
            Call WriteCVCompileMachineEquates
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Lang'; From "Language" string
            Call WriteCVCompileLanguageEquates
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax+4 = 'own1'; From "Unknown1" string
                xor eax eax
                lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'own2'; From "Unknown2" string
                xor eax eax
                lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'own3'; From "Unknown3" string
                xor eax eax
                lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'own4'; From "Unknown4" string
                xor eax eax
                lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'own5'; From "Unknown5" string
                xor eax eax
                lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'own6'; From "Unknown6" string
                xor eax eax
                lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'own7'; From "Unknown7" string
                xor eax eax
                lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'own8'; From "Unknown8" string
                xor eax eax
                lodsw | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name. On this case (S_MSTOOL_CV2) The name is a null terminated string
                Push eax
                    Call StrLenProc esi
                    Mov ecx eax
                    inc ecx ; The size is increased to we alow including the zero byte
                Pop eax

            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2
EndP

________________________________________________________________________________

[CVRegisterType: B$ 'Type' EOS]
[CVRegisterOldCVType: B$ 'OldCVType' EOS]
[CVRegisterRegister: B$ 'Register' EOS]
[CVRegisterNameLenght: B$ 'NameLenght' EOS]
[CVRegisterName: B$ 'Name' EOS]
[CVRegisterTrackingInfo: B$ 'TrackingInfo' EOS]

Proc WriteRawDataDebugSRegister:
    Local @EndPos, @OldCodeView
    Uses ecx, eax

    Mov D@OldCodeView &FALSE
    If W$esi-2 = &S_REGISTER_CV3
        Mov D@OldCodeView &TRUE
    End_If


    ; 1st find start of symbol (Address of index -> S_REGISTER)
    Mov ecx esi
    sub ecx 2 ; at ecx we point to the Index value
    movzx eax W$esi-4 ; find the len of the symbol
    add ecx eax
    Mov D@EndPos ecx

    If D@OldCodeView = &TRUE
        Call WriteRawDataDebugSRegisterItem CVRegisterOldCVType , {B$ ': W$ ' EOS}
    End_If
    Call WriteRawDataDebugSRegisterItem CVRegisterType, {B$ ': W$ ' EOS}
    Call WriteRawDataDebugSRegisterItem CVRegisterRegister, {B$ ': W$ ' EOS}
    Call WriteRawDataDebugSRegisterItem CVRegisterNameLenght, {B$ ': B$ ' EOS}
    Call WriteRawDataDebugSRegisterItem CVRegisterName, {B$ ': B$ ' EOS}

    ..If esi <> D@EndPos ; is esi ending at EndPos ? No, do next line. Yes, jmp over

        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'Register.TrackingInfo: B$ ' EOS}
        Pop esi

        Mov ecx D@EndPos
        sub ecx esi

        .Do
            Push ecx
            xor eax eax
            lodsb | Call WriteEax
            Mov W$edi ', ' | add edi 2
            Pop ecx
            dec ecx
        .Loop_Until ecx = 0
            sub edi 2
            Mov W$edi CRLF | add edi 2
    ..End_If

    ; bypass esi. Need to review this stuff
;    Call WriteRawDataDebugSRegisterItem CVRegisterTrackingInfo, {B$ ': B$ ' EOS}

; type equates here: WriteCVBPRel32TypeEquates
; Call Write_IMAGE_FILE_MACHINE D$CoffMachineType
EndP
________________________________________________________________________________

Proc WriteRawDataDebugSRegisterItem:
    Argument @Text1, @Text2
    uses eax, ecx

        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'Register.' EOS}
            zCopy D@Text1
            zCopy D@Text2
        Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Type' ; from "Type" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'OldC' ; from "OldCVType" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Regi'; From "Register" string
            .If_Or D$CoffMachineType = &IMAGE_FILE_MACHINE_I386, D$CoffMachineType = &IMAGE_FILE_MACHINE_I486, D$CoffMachineType = &IMAGE_FILE_MACHINE_I586
                Call WriteCVRegisterIntelRegEquates
            .Else_If D$CoffMachineType = &IMAGE_FILE_MACHINE_M68K
                Call WriteCVRegisterM68KRegEquates
            .Else_If_Or D$CoffMachineType = &IMAGE_FILE_MACHINE_MIPS16, D$CoffMachineType = &IMAGE_FILE_MACHINE_MIPSFPU, D$CoffMachineType = &IMAGE_FILE_MACHINE_MIPSFPU16
                Call WriteCVRegisterMipsRegEquates
            .End_If
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2
EndP

________________________________________________________________________________

Proc WriteCVRegisterIntelRegEquates:
    Uses esi

    .If B$esi = &CV4_REGISTER_INTEL_REG_NONE
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_NONE ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_AL
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_AL ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_CL
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_CL ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_DL
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_DL ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_BL
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_BL ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_AH
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_AH ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_CH
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_CH ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_DH
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_DH ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_BH
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_BH ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_AX
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_AX ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_CX
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_CX ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_DX
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_DX ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_BX
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_BX ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_SP
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_SP ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_BP
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_BP ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_SI
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_SI ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_DI
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_DI ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_EAX
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_EAX ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_ECX
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_ECX ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_EDX
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_EDX ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_EBX
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_EBX ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_ESP
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_ESP ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_EBP
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_EBP ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_ESI
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_ESI ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_EDI
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_EDI ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_ES
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_ES ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_CS
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_CS ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_SS
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_SS ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_DS
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_DS ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_FS
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_FS ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_GS
        zCopy {B$ '&CV4_REGISTER_INTEL_REG_GS ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SPECIALREGCASE_IP
        zCopy {B$ '&CV4_REGISTER_INTEL_SPECIALREGCASE_IP ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SPECIALREGCASE_FLAGS
        zCopy {B$ '&CV4_REGISTER_INTEL_SPECIALREGCASE_FLAGS ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SPECIALREGCASE_EIP
        zCopy {B$ '&CV4_REGISTER_INTEL_SPECIALREGCASE_EIP ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SPECIALREGCASE_EFLAGS
        zCopy {B$ '&CV4_REGISTER_INTEL_SPECIALREGCASE_EFLAGS ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_PCODEREG_TEMP
        zCopy {B$ '&CV4_REGISTER_INTEL_PCODEREG_TEMP ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_PCODEREG_TEMPH
        zCopy {B$ '&CV4_REGISTER_INTEL_PCODEREG_TEMPH ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_PCODEREG_QUOTE
        zCopy {B$ '&CV4_REGISTER_INTEL_PCODEREG_QUOTE ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_PCODEREG_RESERVED1
        zCopy {B$ '&CV4_REGISTER_INTEL_PCODEREG_RESERVED1 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_PCODEREG_RESERVED2
        zCopy {B$ '&CV4_REGISTER_INTEL_PCODEREG_RESERVED2 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_PCODEREG_RESERVED3
        zCopy {B$ '&CV4_REGISTER_INTEL_PCODEREG_RESERVED3 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_PCODEREG_RESERVED4
        zCopy {B$ '&CV4_REGISTER_INTEL_PCODEREG_RESERVED4 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_PCODEREG_RESERVED5
        zCopy {B$ '&CV4_REGISTER_INTEL_PCODEREG_RESERVED5 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_CR0
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREG_CR0 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_CR1
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREG_CR1 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_CR2
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREG_CR2 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_CR3
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREG_CR3 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_DR0
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREG_DR0 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_DR1
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREG_DR1 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_DR2
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREG_DR2 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_DR3
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREG_DR3 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_DR4
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREG_DR4 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_DR5
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREG_DR5 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_DR6
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREG_DR6 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_DR7
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREG_DR7 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_ST0
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_ST0 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_ST2
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_ST2 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_ST3
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_ST3 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_ST4
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_ST4 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_ST5
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_ST5 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_ST6
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_ST6 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_ST7
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_ST7 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_CONTROL
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_CONTROL ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_STATUS
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_STATUS ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_TAG
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_TAG ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_FPIP
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_FPIP ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_FPCS
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_FPCS ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_FPDO
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_FPDO ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_FPDS
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_FPDS ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_ISEM
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_ISEM ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_FPEIP
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_FPEIP ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_FPEDO
        zCopy {B$ '&CV4_REGISTER_INTEL_SYSREGEXTENSION_FPEDO ; Hex Value:  ' EOS}
    .End_If

EndP
________________________________________________________________________________

Proc WriteCVRegisterM68KRegEquates:
    Uses esi

    .If B$esi = &CV4_REGISTER_M68K_DATA_REG0
        zCopy {B$ '&CV4_REGISTER_M68K_DATA_REG0 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_DATA_REG1
        zCopy {B$ '&CV4_REGISTER_M68K_DATA_REG1 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_DATA_REG2
        zCopy {B$ '&CV4_REGISTER_M68K_DATA_REG2 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_DATA_REG3
        zCopy {B$ '&CV4_REGISTER_M68K_DATA_REG3 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_DATA_REG4
        zCopy {B$ '&CV4_REGISTER_M68K_DATA_REG4 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_DATA_REG5
        zCopy {B$ '&CV4_REGISTER_M68K_DATA_REG5 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_DATA_REG6
        zCopy {B$ '&CV4_REGISTER_M68K_DATA_REG6 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_DATA_REG7
        zCopy {B$ '&CV4_REGISTER_M68K_DATA_REG7 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_ADDRESS_REG0
        zCopy {B$ '&CV4_REGISTER_M68K_ADDRESS_REG0 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_ADDRESS_REG1
        zCopy {B$ '&CV4_REGISTER_M68K_ADDRESS_REG1 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_ADDRESS_REG2
        zCopy {B$ '&CV4_REGISTER_M68K_ADDRESS_REG2 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_ADDRESS_REG3
        zCopy {B$ '&CV4_REGISTER_M68K_ADDRESS_REG3 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_ADDRESS_REG4
        zCopy {B$ '&CV4_REGISTER_M68K_ADDRESS_REG4 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_ADDRESS_REG5
        zCopy {B$ '&CV4_REGISTER_M68K_ADDRESS_REG5 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_ADDRESS_REG6
        zCopy {B$ '&CV4_REGISTER_M68K_ADDRESS_REG6 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_ADDRESS_REG7
        zCopy {B$ '&CV4_REGISTER_M68K_ADDRESS_REG7 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_CCR
        zCopy {B$ '&CV4_REGISTER_M68K_R68_CCR ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_SR
        zCopy {B$ '&CV4_REGISTER_M68K_R68_SR ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_USP
        zCopy {B$ '&CV4_REGISTER_M68K_R68_USP ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_MSP
        zCopy {B$ '&CV4_REGISTER_M68K_R68_MSP ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_SFC
        zCopy {B$ '&CV4_REGISTER_M68K_R68_SFC ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_DFC
        zCopy {B$ '&CV4_REGISTER_M68K_R68_DFC ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_CACR
        zCopy {B$ '&CV4_REGISTER_M68K_R68_CACR ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_VBR
        zCopy {B$ '&CV4_REGISTER_M68K_R68_VBR ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_CAAR
        zCopy {B$ '&CV4_REGISTER_M68K_R68_CAAR ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_ISP
        zCopy {B$ '&CV4_REGISTER_M68K_R68_ISP ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_PC
        zCopy {B$ '&CV4_REGISTER_M68K_R68_PC ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED1
        zCopy {B$ '&CV4_REGISTER_M68K_RESERVED1 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_FPCR
        zCopy {B$ '&CV4_REGISTER_M68K_R68_FPCR ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_FPSR
        zCopy {B$ '&CV4_REGISTER_M68K_R68_FPSR ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_FPIAR
        zCopy {B$ '&CV4_REGISTER_M68K_R68_FPIAR ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED2
        zCopy {B$ '&CV4_REGISTER_M68K_RESERVED2 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_FLOATING_POINT0
        zCopy {B$ '&CV4_REGISTER_M68K_FLOATING_POINT0 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_FLOATING_POINT1
        zCopy {B$ '&CV4_REGISTER_M68K_FLOATING_POINT1 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_FLOATING_POINT2
        zCopy {B$ '&CV4_REGISTER_M68K_FLOATING_POINT2 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_FLOATING_POINT3
        zCopy {B$ '&CV4_REGISTER_M68K_FLOATING_POINT3 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_FLOATING_POINT4
        zCopy {B$ '&CV4_REGISTER_M68K_FLOATING_POINT4 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_FLOATING_POINT5
        zCopy {B$ '&CV4_REGISTER_M68K_FLOATING_POINT5 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_FLOATING_POINT6
        zCopy {B$ '&CV4_REGISTER_M68K_FLOATING_POINT6 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_FLOATING_POINT7
        zCopy {B$ '&CV4_REGISTER_M68K_FLOATING_POINT7 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED3
        zCopy {B$ '&CV4_REGISTER_M68K_RESERVED3 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED4
        zCopy {B$ '&CV4_REGISTER_M68K_RESERVED4 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED5
        zCopy {B$ '&CV4_REGISTER_M68K_RESERVED5 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED6
        zCopy {B$ '&CV4_REGISTER_M68K_RESERVED6 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED7
        zCopy {B$ '&CV4_REGISTER_M68K_RESERVED7 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED8
        zCopy {B$ '&CV4_REGISTER_M68K_RESERVED8 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED9
        zCopy {B$ '&CV4_REGISTER_M68K_RESERVED9 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED10
        zCopy {B$ '&CV4_REGISTER_M68K_RESERVED10 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED11
        zCopy {B$ '&CV4_REGISTER_M68K_RESERVED11 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED12
        zCopy {B$ '&CV4_REGISTER_M68K_RESERVED12 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED13
        zCopy {B$ '&CV4_REGISTER_M68K_RESERVED13 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_CV_R68_PSR
        zCopy {B$ '&CV4_REGISTER_M68K_CV_R68_PSR ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_M68K_CV_R68_PCSR
        zCopy {B$ '&CV4_REGISTER_M68K_CV_R68_PCSR ; Hex Value:  ' EOS}
    .End_If

EndP
________________________________________________________________________________

Proc WriteCVRegisterMipsRegEquates:
    Uses esi

    .If B$esi = &CV4_REGISTER_MIPS_NOREGISTER
        zCopy {B$ '&CV4_REGISTER_MIPS_NOREGISTER ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_ZERO
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_ZERO ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_AT
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_AT ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_V0
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_V0 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_V1
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_V1 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_A0
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_A0 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_A1
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_A1 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_A2
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_A2 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_A3
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_A3 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T0
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_T0 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T1
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_T1 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T2
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_T2 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T3
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_T3 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T4
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_T4 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T5
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_T5 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T6
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_T6 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T7
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_T7 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S0
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_S0 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S1
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_S1 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S2
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_S2 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S3
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_S3 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S4
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_S4 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S5
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_S5 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S6
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_S6 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S7
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_S7 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T8
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_T8 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T9
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_T9 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_KT0
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_KT0 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_KT1
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_KT1 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_GP
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_GP ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_SP
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_SP ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S8
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_S8 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_RA
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_RA ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_LO
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_LO ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_HI
        zCopy {B$ '&CV4_REGISTER_MIPS_INT_HI ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FIR
        zCopy {B$ '&CV4_REGISTER_MIPS_FIR ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_PSR
        zCopy {B$ '&CV4_REGISTER_MIPS_PSR ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT0
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT0 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT1
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT1 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT2
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT2 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT3
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT3 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT4
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT4 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT5
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT5 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT6
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT6 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT7
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT7 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT8
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT8 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT9
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT9 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT10
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT10 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT11
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT11 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT12
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT12 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT13
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT13 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT14
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT14 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT15
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT15 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT16
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT16 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT17
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT17 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT18
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT18 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT19
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT19 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT20
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT20 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT21
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT21 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT22
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT22 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT23
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT23 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT24
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT24 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT25
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT25 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT26
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT26 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT27
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT27 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT28
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT28 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT29
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT29 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT30
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT30 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT31
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT31 ; Hex Value:  ' EOS}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT_STATUS_REG
        zCopy {B$ '&CV4_REGISTER_MIPS_FLOATING_POINT_STATUS_REG ; Hex Value:  ' EOS}
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeStructureItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.VarStr' EOS}
        zCopy D@Text1
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Coun'; From "Count" string
            xor eax eax
            lodsw | Call WriteEax
        ..Else_If D$eax = 'Fiel'; From "Field" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'OldC'; From "OldCVType" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax
        ..Else_If D$eax+13 = 'own1'; From "OldCVTypeUnknown1" string
            xor eax eax
            lodsw | Call WriteEax
        ..Else_If D$eax+13 = 'own2'; From "OldCVTypeUnknown2" string
            xor eax eax
            lodsw | Call WriteEax
         ..Else_If D$eax = 'Prop'; From "Property" string
            xor eax eax
            lodsw | Call WriteEax
        ..Else_If D$eax = 'dLis'; From "dList" string
            xor eax eax
            lodsw | Call WriteEax
        ..Else_If D$eax = 'vSha'; From "vShape" string
            xor eax eax
            lodsw | Call WriteEax
        ..Else_If D$eax = 'Stru'; From "StructLen" string
            xor eax eax
            lodsw | Call WriteEax


        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP
___________________________________________________

[CVLeafTypeStructureCount: B$ 'Count' EOS]
[CVLeafTypeStructureField: B$ 'Field' EOS]
[CVLeafTypeStructureOldCVType: B$ 'OldCVType' EOS]
[CVLeafTypeStructureOldCVTypeUnkn1: B$ 'OldCVTypeUnknown1' EOS]
[CVLeafTypeStructureOldCVTypeUnkn2: B$ 'OldCVTypeUnknown2' EOS]
[CVLeafTypeStructureProperty: B$ 'Property' EOS]
[CVLeafTypeStructuredList: B$ 'dList' EOS]
[CVLeafTypeStructurevshape: B$ 'vShape' EOS]
[CVLeafTypeStructuredStructLen: B$ 'StructLen' EOS]

Proc WriteRawDataDebugSContantLeafTypeStructure:
    Local @OldCodeView
    Uses eax, ecx

    Mov D@OldCodeView &FALSE
    If W$esi-2 = &LF_STRUCTURE_CV3
        Mov D@OldCodeView &TRUE
    End_If

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx

        Call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeStructureCount, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeStructureField, {B$ ': W$ ' EOS}
        If D@OldCodeView = &TRUE
            Call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeUnionOldCVType, {B$ ': W$ ' EOS}
            Call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeStructureOldCVTypeUnkn1, {B$ ': W$ ' EOS}
            Call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeStructureOldCVTypeUnkn2, {B$ ': W$ ' EOS}
        End_If
        Call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeStructureProperty, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeStructuredList, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeStructurevshape, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeStructuredStructLen, {B$ ': W$ ' EOS}

        Call WriteRawDataDebugSContantLeafTypeStructureItem CVCompileNameLenght, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeStructureItem CVCompileName, {B$ ': B$ ' EOS}
        sub edi 2

    Pop ebx

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If


EndP
________________________________________________________________________________

[CVLeafTypeUnionCount: B$ 'Count' EOS]
[CVLeafTypeUnionField: B$ 'Field' EOS]
[CVLeafTypeUnionOldCVType: B$ 'OldCVType' EOS]
[CVLeafTypeUnionProperty: B$ 'Property' EOS]
[CVLeafTypeUnionUnionLen: B$ 'UnionLength' EOS]
;LF_UNION count @field property length
; WriteRawDataDebugSContantLeafTypeMember
Proc WriteRawDataDebugSContantLeafTypeUnion:
    Local @OldCodeView
    Uses eax, ecx


    Mov D@OldCodeView &FALSE
    If W$esi-2 = &LF_UNION_CV3
        Mov D@OldCodeView &TRUE
    End_If

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx

        Call WriteRawDataDebugSContantLeafTypeUnionItem CVLeafTypeUnionCount, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeUnionItem CVLeafTypeUnionField, {B$ ': W$ ' EOS}
        If D@OldCodeView = &TRUE
            Call WriteRawDataDebugSContantLeafTypeUnionItem CVLeafTypeUnionOldCVType, {B$ ': W$ ' EOS}
        End_If
        Call WriteRawDataDebugSContantLeafTypeUnionItem CVLeafTypeUnionProperty, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeUnionItem CVLeafTypeUnionUnionLen, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeUnionItem CVCompileNameLenght, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeUnionItem CVCompileName, {B$ ': B$ ' EOS}
        sub edi 2

    Pop ebx

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If


EndP
________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeUnionItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.Union.' EOS}
        zCopy D@Text1
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Coun'; From "Count" string
            xor eax eax
            lodsw | Call WriteEax
        ..Else_If D$eax = 'Fiel'; From "Field" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax
        ..Else_If D$eax = 'OldC'; From "OldCVType" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax
        ..Else_If D$eax = 'Prop'; From "Property" string
            xor eax eax
            lodsw | Call WriteEax
        ..Else_If D$eax = 'Unio'; From "UnionLength" string
            xor eax eax
            lodsw | Call WriteEax


        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP
________________________________________________________________________________


Proc WriteRawDataDebugSContantLeafTypePointerItem:
   Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.Pointer.' EOS}
        zCopy D@Text1
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        ..If D$eax = 'OldC'; From "OldCVType" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Attr'; From "Attribute" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Type'; From "Type" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP
_______________________________________________________________________

[CVLeafTypeModifierOldCVType: B$ 'OldCVType' EOS]
[CVLeafTypeModifierAttribute: B$ 'Attribute' EOS]
[CVLeafTypeModifierIndex: B$ 'Index' EOS]

Proc WriteRawDataDebugSContantLeafTypeModifier:
    Local @OldCodeView
    Uses eax, ecx

    Mov D@OldCodeView &FALSE
    If W$esi-2 = &LF_MODIFIER_CV3
        Mov D@OldCodeView &TRUE
    End_If


    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        If D@OldCodeView = &TRUE
            Call WriteRawDataDebugSContantLeafTypeModifierItem CVLeafTypeModifierOldCVType, {B$ ': W$ ' EOS}
        End_If
        Call WriteRawDataDebugSContantLeafTypeModifierItem CVLeafTypeModifierAttribute, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeModifierItem CVLeafTypeModifierIndex, {B$ ': W$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If


EndP
_________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeModifierItem:
   Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.Modifier.' EOS}
        zCopy D@Text1
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        ..If D$eax = 'OldC'; From "OldCVType" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Attr'; From "Attribute" string
            ; same as in WriteCVCompileFlag2Equates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Inde'; From "Type" string
            xor eax eax
            lodsw | Call WriteEax

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

_________________________________________________________________________________


[CVLeafTypePointerOldCVType: B$ 'OldCVType' EOS]
[CVLeafTypePointerAttribute: B$ 'Attribute' EOS]
[CVLeafTypePointerType: B$ 'Type' EOS]

Proc WriteRawDataDebugSContantLeafTypePointer:
    Local @OldCodeView
    Uses eax, ecx

    Mov D@OldCodeView &FALSE
    If W$esi-2 = &LF_POINTER_CV3
        Mov D@OldCodeView &TRUE
    End_If


    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        If D@OldCodeView = &TRUE
            Call WriteRawDataDebugSContantLeafTypePointerItem CVLeafTypePointerOldCVType, {B$ ': W$ ' EOS}
        End_If
        Call WriteRawDataDebugSContantLeafTypePointerItem CVLeafTypePointerAttribute, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypePointerItem CVLeafTypePointerType, {B$ ': W$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If


EndP
________________________________________________________________________________

            ;WriteRawDataDebugSBPRel32
[CVLeafTypeArrayElemType: B$ 'ElemType' EOS]
[CVLeafTypeArrayIdxType: B$ 'IdxType' EOS]
[CVLeafTypeArrayLength: B$ 'Length' EOS]
[CVLeafTypeArrayNamelenght: B$ 'NameLenght' EOS]
[CVLeafTypeArrayName: B$ 'Name' EOS]

Proc WriteRawDataDebugSContantLeafTypeArray:
    Uses eax, ecx


    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayElemType , {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayIdxType , {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayLength, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayNamelenght, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayName, {B$ ': B$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP
________________________________________________________________________________
Proc WriteRawDataDebugSContantLeafTypeArrayItem:
    Argument @Text1, @Text2
    uses eax, ecx

        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'Constant.LeafType.Array.' EOS}
            zCopy D@Text1
            zCopy D@Text2
        Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Elem' ; from "ElemType" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'IdxT'; From "IdxType" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Leng'; From "Lenght" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

________________________________________________________________________________
Proc WriteRawDataDebugSContantLeafTypeProcedureItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.Procedure.' EOS}
        zCopy D@Text1
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

         ..If D$eax = 'rvTy'; From "rvType" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Call'; From "Call" string
            Call WriteCVLF_ProcCallConvTypeEquates
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Rese'; From "Reserved" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Parm'; From "Parms" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'ArgL'; From "ArgList" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

_________________________________________

[CVLeafTypeProcrvType: B$ 'rvType' EOS]
[CVLeafTypeProcCall: B$ 'Call' EOS]
[CVLeafTypeProcrvReserved: B$ 'Reserved' EOS]
[CVLeafTypeProcrvParms: B$ 'Parms' EOS]
[CVLeafTypeProcrvArgList: B$ 'ArgList' EOS]

Proc WriteRawDataDebugSContantLeafTypeProcedure:
    Uses eax, ecx


    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeProcedureItem CVLeafTypeProcrvType, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeProcedureItem CVLeafTypeProcCall, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeProcedureItem CVLeafTypeProcrvReserved, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeProcedureItem CVLeafTypeProcrvParms, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeProcedureItem CVLeafTypeProcrvArgList, {B$ ': W$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

[CVLeafTypeMFunctionRvType: B$ 'RvType' EOS]
[CVLeafTypeMFunctionClass: B$ 'Class' EOS]
[CVLeafTypeMFunctionThis: B$ 'This' EOS]
[CVLeafTypeMFunctionCall: B$ 'Call' EOS]
[CVLeafTypeMFunctionRes: B$ 'Res' EOS]
[CVLeafTypeMFunctionParms: B$ 'Parms' EOS]
[CVLeafTypeMFunctionOldCVType: B$ 'OldCVType' EOS]
[CVLeafTypeMFunctionArglist: B$ 'Arglist' EOS]
[CVLeafTypeMFunctionThisAdjust: B$ 'ThisAdjust' EOS]

Proc WriteRawDataDebugSContantLeafTypeMFunction:
    Local @OldCodeView
    Uses eax, ecx


    Mov D@OldCodeView &FALSE
    If W$esi-2 = &LF_MFUNCTION_CV3
        Mov D@OldCodeView &TRUE
    End_If

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx

        If D@OldCodeView = &TRUE
            Call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionRvType, {B$ ': D$ ' EOS}, &TRUE
            Call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionClass, {B$ ': D$ ' EOS}, &TRUE
            Call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionThis, {B$ ': D$ ' EOS}, &TRUE
        Else
            Call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionRvType, {B$ ': W$ ' EOS}, &FALSE
            Call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionClass, {B$ ': W$ ' EOS}, &FALSE
            Call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionThis, {B$ ': W$ ' EOS}, &FALSE

        End_If
        Call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionCall, {B$ ': B$ ' EOS}, &FALSE
        Call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionRes, {B$ ': B$ ' EOS}, &FALSE
        Call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionParms, {B$ ': W$ ' EOS}, &FALSE
        If D@OldCodeView = &TRUE
            Call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionOldCVType, {B$ ': W$ ' EOS}, &TRUE
        End_If
        Call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionArglist, {B$ ': W$ ' EOS}, &FALSE
        Call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionThisAdjust, {B$ ': D$ ' EOS}, &FALSE
        sub edi 2

    Pop ebx

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP
________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeMFunctionItem:
    Argument @Text1, @Text2, @UseOldCVType
    uses eax, ecx, ebx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.MFunction.' EOS}
        zCopy D@Text1
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        ..If D$eax = 'RvTy'; From "RvType" string
            Call WriteCVBPRel32TypeEquates
            If D@UseOldCVType = &TRUE
                lodsd | Call WriteEax
            Else
                xor eax eax
                lodsw | Call WriteEax
            End_If
        ..Else_If D$eax = 'Clas'; From "Class" string
            Call WriteCVBPRel32TypeEquates
            If D@UseOldCVType = &TRUE
                lodsd | Call WriteEax
            Else
                xor eax eax
                lodsw | Call WriteEax
            End_If
        ..Else_If D$eax = 'This'; From "This" string
            .If D$eax+4 = 'Adju'; From "ThisAdjust" string
                lodsd | Call WriteEax
            .Else   ; From "This" string
                Call WriteCVBPRel32TypeEquates
                If D@UseOldCVType = &TRUE
                    lodsd | Call WriteEax
                Else
                    xor eax eax
                    lodsw | Call WriteEax
                End_If
            .End_If
        ..Else_If D$eax = 'Call'; From "Call" string
            Call WriteCVLF_ProcCallConvTypeEquates
            xor eax eax
            lodsb | Call WriteEax
        ..Else_If W$eax = 'Re'; From "Res" string
            xor eax eax
            lodsb | Call WriteEax
        ..Else_If D$eax = 'Parm'; From "Parms" string
            xor eax eax
            lodsw | Call WriteEax
        ..Else_If D$eax = 'OldC'; From "OldCVType" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax
        ..Else_If D$eax = 'Argl'; From "Arglist" string
            xor eax eax
            lodsw | Call WriteEax
        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

________________________________________________________________________________

Proc WriteCVLF_ProcCallConvTypeEquates:
    Uses esi

    ..If W$esi = &CV4_CALL_NEAR_C
        zCopy {B$ '&CV4_CALL_NEAR_C; Near C (arguments pushed right to left, caller pops arguments) - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_FAR_C
        zCopy {B$ '&CV4_CALL_FAR_C; Far C - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_NEAR_PASCAL
        zCopy {B$ '&CV4_CALL_NEAR_PASCAL; Near Pascal (arguments pushed left to right, callee pops arguments) - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_FAR_PASCAL
        zCopy {B$ '&CV4_CALL_FAR_PASCAL; Far Pascal - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_NEAR_FASTCALL
        zCopy {B$ '&CV4_CALL_NEAR_FASTCALL; Near fastcall - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_FAR_FASTCALL
        zCopy {B$ '&CV4_CALL_FAR_FASTCALL; Far fastcall - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED6
        zCopy {B$ '&CV4_CALL_RESERVED6; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_NEAR_STDCALL
        zCopy {B$ '&CV4_CALL_NEAR_STDCALL; Near stdcall - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_FAR_STDCALL
        zCopy {B$ '&CV4_CALL_FAR_STDCALL; Far stdcall - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_NEAR_SYSCALL
        zCopy {B$ '&CV4_CALL_NEAR_SYSCALL; Near syscall - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_FAR_SYSCALL
        zCopy {B$ '&CV4_CALL_FAR_SYSCALL; Far syscall - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_THIS_CALL
        zCopy {B$ '&CV4_CALL_THIS_CALL; This Call - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_MIPS_CALL
        zCopy {B$ '&CV4_CALL_MIPS_CALL; MIPS Call - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_GENERIC
        zCopy {B$ '&CV4_CALL_GENERIC; Generic - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED14
        zCopy {B$ '&CV4_CALL_RESERVED14; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED15
        zCopy {B$ '&CV4_CALL_RESERVED15; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED16
        zCopy {B$ '&CV4_CALL_RESERVED16; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED17
        zCopy {B$ '&CV4_CALL_RESERVED17; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED18
        zCopy {B$ '&CV4_CALL_RESERVED18; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED19
        zCopy {B$ '&CV4_CALL_RESERVED19; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED20
        zCopy {B$ '&CV4_CALL_RESERVED20; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED21
        zCopy {B$ '&CV4_CALL_RESERVED21; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED22
        zCopy {B$ '&CV4_CALL_RESERVED22; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED23
        zCopy {B$ '&CV4_CALL_RESERVED23; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED24
        zCopy {B$ '&CV4_CALL_RESERVED24; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED25
        zCopy {B$ '&CV4_CALL_RESERVED25; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED26
        zCopy {B$ '&CV4_CALL_RESERVED26; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED27
        zCopy {B$ '&CV4_CALL_RESERVED27; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED28
        zCopy {B$ '&CV4_CALL_RESERVED28; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED29
        zCopy {B$ '&CV4_CALL_RESERVED29; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED30
        zCopy {B$ '&CV4_CALL_RESERVED30; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED31
        zCopy {B$ '&CV4_CALL_RESERVED31; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED32
        zCopy {B$ '&CV4_CALL_RESERVED32; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED33
        zCopy {B$ '&CV4_CALL_RESERVED33; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED34
        zCopy {B$ '&CV4_CALL_RESERVED34; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED35
        zCopy {B$ '&CV4_CALL_RESERVED35; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED36
        zCopy {B$ '&CV4_CALL_RESERVED36; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED37
        zCopy {B$ '&CV4_CALL_RESERVED37; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED38
        zCopy {B$ '&CV4_CALL_RESERVED38; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED39
        zCopy {B$ '&CV4_CALL_RESERVED39; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED40
        zCopy {B$ '&CV4_CALL_RESERVED40; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED41
        zCopy {B$ '&CV4_CALL_RESERVED41; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED42
        zCopy {B$ '&CV4_CALL_RESERVED42; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED43
        zCopy {B$ '&CV4_CALL_RESERVED43; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED44
        zCopy {B$ '&CV4_CALL_RESERVED44; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED45
        zCopy {B$ '&CV4_CALL_RESERVED45; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED46
        zCopy {B$ '&CV4_CALL_RESERVED46; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED47
        zCopy {B$ '&CV4_CALL_RESERVED47; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED48
        zCopy {B$ '&CV4_CALL_RESERVED48; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED49
        zCopy {B$ '&CV4_CALL_RESERVED49; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED50
        zCopy {B$ '&CV4_CALL_RESERVED50; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED51
        zCopy {B$ '&CV4_CALL_RESERVED51; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED52
        zCopy {B$ '&CV4_CALL_RESERVED52; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED53
        zCopy {B$ '&CV4_CALL_RESERVED53; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED54
        zCopy {B$ '&CV4_CALL_RESERVED54; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED55
        zCopy {B$ '&CV4_CALL_RESERVED55; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED56
        zCopy {B$ '&CV4_CALL_RESERVED56; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED57
        zCopy {B$ '&CV4_CALL_RESERVED57; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED58
        zCopy {B$ '&CV4_CALL_RESERVED58; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED59
        zCopy {B$ '&CV4_CALL_RESERVED59; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED60
        zCopy {B$ '&CV4_CALL_RESERVED60; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED61
        zCopy {B$ '&CV4_CALL_RESERVED61; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED62
        zCopy {B$ '&CV4_CALL_RESERVED62; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED63
        zCopy {B$ '&CV4_CALL_RESERVED63; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED64
        zCopy {B$ '&CV4_CALL_RESERVED64; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED65
        zCopy {B$ '&CV4_CALL_RESERVED65; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED66
        zCopy {B$ '&CV4_CALL_RESERVED66; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED67
        zCopy {B$ '&CV4_CALL_RESERVED67; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED68
        zCopy {B$ '&CV4_CALL_RESERVED68; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED69
        zCopy {B$ '&CV4_CALL_RESERVED69; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED70
        zCopy {B$ '&CV4_CALL_RESERVED70; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED71
        zCopy {B$ '&CV4_CALL_RESERVED71; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED72
        zCopy {B$ '&CV4_CALL_RESERVED72; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED73
        zCopy {B$ '&CV4_CALL_RESERVED73; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED74
        zCopy {B$ '&CV4_CALL_RESERVED74; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED75
        zCopy {B$ '&CV4_CALL_RESERVED75; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED76
        zCopy {B$ '&CV4_CALL_RESERVED76; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED77
        zCopy {B$ '&CV4_CALL_RESERVED77; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED78
        zCopy {B$ '&CV4_CALL_RESERVED78; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED79
        zCopy {B$ '&CV4_CALL_RESERVED79; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED80
        zCopy {B$ '&CV4_CALL_RESERVED80; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED81
        zCopy {B$ '&CV4_CALL_RESERVED81; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED82
        zCopy {B$ '&CV4_CALL_RESERVED82; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED83
        zCopy {B$ '&CV4_CALL_RESERVED83; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED84
        zCopy {B$ '&CV4_CALL_RESERVED84; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED85
        zCopy {B$ '&CV4_CALL_RESERVED85; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED86
        zCopy {B$ '&CV4_CALL_RESERVED86; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED87
        zCopy {B$ '&CV4_CALL_RESERVED87; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED88
        zCopy {B$ '&CV4_CALL_RESERVED88; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED89
        zCopy {B$ '&CV4_CALL_RESERVED89; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED90
        zCopy {B$ '&CV4_CALL_RESERVED90; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED91
        zCopy {B$ '&CV4_CALL_RESERVED91; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED92
        zCopy {B$ '&CV4_CALL_RESERVED92; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED93
        zCopy {B$ '&CV4_CALL_RESERVED93; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED94
        zCopy {B$ '&CV4_CALL_RESERVED94; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED95
        zCopy {B$ '&CV4_CALL_RESERVED95; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED96
        zCopy {B$ '&CV4_CALL_RESERVED96; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED97
        zCopy {B$ '&CV4_CALL_RESERVED97; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED98
        zCopy {B$ '&CV4_CALL_RESERVED98; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED99
        zCopy {B$ '&CV4_CALL_RESERVED99; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED100
        zCopy {B$ '&CV4_CALL_RESERVED100; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED101
        zCopy {B$ '&CV4_CALL_RESERVED101; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED102
        zCopy {B$ '&CV4_CALL_RESERVED102; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED103
        zCopy {B$ '&CV4_CALL_RESERVED103; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED104
        zCopy {B$ '&CV4_CALL_RESERVED104; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED105
        zCopy {B$ '&CV4_CALL_RESERVED105; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED106
        zCopy {B$ '&CV4_CALL_RESERVED106; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED107
        zCopy {B$ '&CV4_CALL_RESERVED107; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED108
        zCopy {B$ '&CV4_CALL_RESERVED108; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED109
        zCopy {B$ '&CV4_CALL_RESERVED109; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED110
        zCopy {B$ '&CV4_CALL_RESERVED110; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED111
        zCopy {B$ '&CV4_CALL_RESERVED111; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED112
        zCopy {B$ '&CV4_CALL_RESERVED112; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED113
        zCopy {B$ '&CV4_CALL_RESERVED113; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED114
        zCopy {B$ '&CV4_CALL_RESERVED114; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED115
        zCopy {B$ '&CV4_CALL_RESERVED115; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED116
        zCopy {B$ '&CV4_CALL_RESERVED116; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED117
        zCopy {B$ '&CV4_CALL_RESERVED117; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED118
        zCopy {B$ '&CV4_CALL_RESERVED118; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED119
        zCopy {B$ '&CV4_CALL_RESERVED119; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED120
        zCopy {B$ '&CV4_CALL_RESERVED120; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED121
        zCopy {B$ '&CV4_CALL_RESERVED121; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED122
        zCopy {B$ '&CV4_CALL_RESERVED122; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED123
        zCopy {B$ '&CV4_CALL_RESERVED123; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED124
        zCopy {B$ '&CV4_CALL_RESERVED124; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED125
        zCopy {B$ '&CV4_CALL_RESERVED125; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED126
        zCopy {B$ '&CV4_CALL_RESERVED126; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED127
        zCopy {B$ '&CV4_CALL_RESERVED127; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED128
        zCopy {B$ '&CV4_CALL_RESERVED128; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED129
        zCopy {B$ '&CV4_CALL_RESERVED129; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED130
        zCopy {B$ '&CV4_CALL_RESERVED130; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED131
        zCopy {B$ '&CV4_CALL_RESERVED131; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED132
        zCopy {B$ '&CV4_CALL_RESERVED132; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED133
        zCopy {B$ '&CV4_CALL_RESERVED133; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED134
        zCopy {B$ '&CV4_CALL_RESERVED134; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED135
        zCopy {B$ '&CV4_CALL_RESERVED135; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED136
        zCopy {B$ '&CV4_CALL_RESERVED136; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED137
        zCopy {B$ '&CV4_CALL_RESERVED137; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED138
        zCopy {B$ '&CV4_CALL_RESERVED138; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED139
        zCopy {B$ '&CV4_CALL_RESERVED139; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED140
        zCopy {B$ '&CV4_CALL_RESERVED140; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED141
        zCopy {B$ '&CV4_CALL_RESERVED141; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED142
        zCopy {B$ '&CV4_CALL_RESERVED142; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED143
        zCopy {B$ '&CV4_CALL_RESERVED143; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED144
        zCopy {B$ '&CV4_CALL_RESERVED144; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED145
        zCopy {B$ '&CV4_CALL_RESERVED145; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED146
        zCopy {B$ '&CV4_CALL_RESERVED146; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED147
        zCopy {B$ '&CV4_CALL_RESERVED147; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED148
        zCopy {B$ '&CV4_CALL_RESERVED148; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED149
        zCopy {B$ '&CV4_CALL_RESERVED149; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED150
        zCopy {B$ '&CV4_CALL_RESERVED150; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED151
        zCopy {B$ '&CV4_CALL_RESERVED151; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED152
        zCopy {B$ '&CV4_CALL_RESERVED152; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED153
        zCopy {B$ '&CV4_CALL_RESERVED153; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED154
        zCopy {B$ '&CV4_CALL_RESERVED154; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED155
        zCopy {B$ '&CV4_CALL_RESERVED155; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED156
        zCopy {B$ '&CV4_CALL_RESERVED156; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED157
        zCopy {B$ '&CV4_CALL_RESERVED157; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED158
        zCopy {B$ '&CV4_CALL_RESERVED158; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED159
        zCopy {B$ '&CV4_CALL_RESERVED159; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED160
        zCopy {B$ '&CV4_CALL_RESERVED160; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED161
        zCopy {B$ '&CV4_CALL_RESERVED161; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED162
        zCopy {B$ '&CV4_CALL_RESERVED162; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED163
        zCopy {B$ '&CV4_CALL_RESERVED163; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED164
        zCopy {B$ '&CV4_CALL_RESERVED164; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED165
        zCopy {B$ '&CV4_CALL_RESERVED165; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED166
        zCopy {B$ '&CV4_CALL_RESERVED166; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED167
        zCopy {B$ '&CV4_CALL_RESERVED167; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED168
        zCopy {B$ '&CV4_CALL_RESERVED168; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED169
        zCopy {B$ '&CV4_CALL_RESERVED169; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED170
        zCopy {B$ '&CV4_CALL_RESERVED170; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED171
        zCopy {B$ '&CV4_CALL_RESERVED171; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED172
        zCopy {B$ '&CV4_CALL_RESERVED172; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED173
        zCopy {B$ '&CV4_CALL_RESERVED173; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED174
        zCopy {B$ '&CV4_CALL_RESERVED174; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED175
        zCopy {B$ '&CV4_CALL_RESERVED175; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED176
        zCopy {B$ '&CV4_CALL_RESERVED176; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED177
        zCopy {B$ '&CV4_CALL_RESERVED177; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED178
        zCopy {B$ '&CV4_CALL_RESERVED178; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED179
        zCopy {B$ '&CV4_CALL_RESERVED179; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED180
        zCopy {B$ '&CV4_CALL_RESERVED180; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED181
        zCopy {B$ '&CV4_CALL_RESERVED181; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED182
        zCopy {B$ '&CV4_CALL_RESERVED182; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED183
        zCopy {B$ '&CV4_CALL_RESERVED183; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED184
        zCopy {B$ '&CV4_CALL_RESERVED184; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED185
        zCopy {B$ '&CV4_CALL_RESERVED185; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED186
        zCopy {B$ '&CV4_CALL_RESERVED186; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED187
        zCopy {B$ '&CV4_CALL_RESERVED187; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED188
        zCopy {B$ '&CV4_CALL_RESERVED188; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED189
        zCopy {B$ '&CV4_CALL_RESERVED189; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED190
        zCopy {B$ '&CV4_CALL_RESERVED190; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED191
        zCopy {B$ '&CV4_CALL_RESERVED191; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED192
        zCopy {B$ '&CV4_CALL_RESERVED192; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED193
        zCopy {B$ '&CV4_CALL_RESERVED193; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED194
        zCopy {B$ '&CV4_CALL_RESERVED194; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED195
        zCopy {B$ '&CV4_CALL_RESERVED195; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED196
        zCopy {B$ '&CV4_CALL_RESERVED196; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED197
        zCopy {B$ '&CV4_CALL_RESERVED197; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED198
        zCopy {B$ '&CV4_CALL_RESERVED198; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED199
        zCopy {B$ '&CV4_CALL_RESERVED199; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED200
        zCopy {B$ '&CV4_CALL_RESERVED200; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED201
        zCopy {B$ '&CV4_CALL_RESERVED201; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED202
        zCopy {B$ '&CV4_CALL_RESERVED202; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED203
        zCopy {B$ '&CV4_CALL_RESERVED203; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED204
        zCopy {B$ '&CV4_CALL_RESERVED204; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED205
        zCopy {B$ '&CV4_CALL_RESERVED205; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED206
        zCopy {B$ '&CV4_CALL_RESERVED206; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED207
        zCopy {B$ '&CV4_CALL_RESERVED207; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED208
        zCopy {B$ '&CV4_CALL_RESERVED208; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED209
        zCopy {B$ '&CV4_CALL_RESERVED209; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED210
        zCopy {B$ '&CV4_CALL_RESERVED210; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED211
        zCopy {B$ '&CV4_CALL_RESERVED211; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED212
        zCopy {B$ '&CV4_CALL_RESERVED212; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED213
        zCopy {B$ '&CV4_CALL_RESERVED213; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED214
        zCopy {B$ '&CV4_CALL_RESERVED214; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED215
        zCopy {B$ '&CV4_CALL_RESERVED215; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED216
        zCopy {B$ '&CV4_CALL_RESERVED216; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED217
        zCopy {B$ '&CV4_CALL_RESERVED217; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED218
        zCopy {B$ '&CV4_CALL_RESERVED218; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED219
        zCopy {B$ '&CV4_CALL_RESERVED219; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED220
        zCopy {B$ '&CV4_CALL_RESERVED220; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED221
        zCopy {B$ '&CV4_CALL_RESERVED221; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED222
        zCopy {B$ '&CV4_CALL_RESERVED222; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED223
        zCopy {B$ '&CV4_CALL_RESERVED223; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED224
        zCopy {B$ '&CV4_CALL_RESERVED224; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED225
        zCopy {B$ '&CV4_CALL_RESERVED225; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED226
        zCopy {B$ '&CV4_CALL_RESERVED226; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED227
        zCopy {B$ '&CV4_CALL_RESERVED227; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED228
        zCopy {B$ '&CV4_CALL_RESERVED228; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED229
        zCopy {B$ '&CV4_CALL_RESERVED229; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED230
        zCopy {B$ '&CV4_CALL_RESERVED230; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED231
        zCopy {B$ '&CV4_CALL_RESERVED231; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED232
        zCopy {B$ '&CV4_CALL_RESERVED232; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED233
        zCopy {B$ '&CV4_CALL_RESERVED233; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED234
        zCopy {B$ '&CV4_CALL_RESERVED234; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED235
        zCopy {B$ '&CV4_CALL_RESERVED235; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED236
        zCopy {B$ '&CV4_CALL_RESERVED236; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED237
        zCopy {B$ '&CV4_CALL_RESERVED237; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED238
        zCopy {B$ '&CV4_CALL_RESERVED238; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED239
        zCopy {B$ '&CV4_CALL_RESERVED239; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED240
        zCopy {B$ '&CV4_CALL_RESERVED240; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED241
        zCopy {B$ '&CV4_CALL_RESERVED241; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED242
        zCopy {B$ '&CV4_CALL_RESERVED242; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED243
        zCopy {B$ '&CV4_CALL_RESERVED243; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED244
        zCopy {B$ '&CV4_CALL_RESERVED244; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED245
        zCopy {B$ '&CV4_CALL_RESERVED245; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED246
        zCopy {B$ '&CV4_CALL_RESERVED246; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED247
        zCopy {B$ '&CV4_CALL_RESERVED247; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED248
        zCopy {B$ '&CV4_CALL_RESERVED248; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED249
        zCopy {B$ '&CV4_CALL_RESERVED249; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED250
        zCopy {B$ '&CV4_CALL_RESERVED250; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED251
        zCopy {B$ '&CV4_CALL_RESERVED251; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED252
        zCopy {B$ '&CV4_CALL_RESERVED252; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED253
        zCopy {B$ '&CV4_CALL_RESERVED253; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED254
        zCopy {B$ '&CV4_CALL_RESERVED254; Reserved - Hex Value:  ' EOS}
    ..Else_If W$esi = &CV4_CALL_RESERVED255
        zCopy {B$ '&CV4_CALL_RESERVED255; Reserved - Hex Value:  ' EOS}
    ..End_If

EndP
________________________________________________________________________________


Proc WriteRawDataDebugSContantLeafTypeArgListItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.ArgList' EOS}
        zCopy D@Text1
        If D$ArgListIndexCount <> 0
            ZCopy LeafTypeArgListIndice
        End_If
        zCopy D@Text2
    Pop esi

    Call WriteCVBPRel32TypeEquates
    xor eax eax
    lodsw | Call WriteEax
    Mov W$edi CRLF | add edi 2

EndP

_________________________________________________

[CVLeafTypeArgListIndice: B$ 'Indice' EOS]
[CVLeafTypeArgListOldCVType: B$ 'OldCVType' EOS]


;    Mov D$LeafTypeArgListIndice '0000', D$LeafTypeArgListIndice+4 '01'
;            Call IncrementLeafTypeArgListIndex

[LeafTypeArgListIndice: B$ '000001' EOS]

IncrementLeafTypeArgListIndex:
    lea ebx D$LeafTypeArgListIndice+5 | inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret

[ArgListIndexCount: D$ 0]

Proc WriteRawDataDebugSContantLeafTypeArgList:
    Local @OldCodeView
    Uses eax, ecx, ebx

    Mov D@OldCodeView &FALSE
    If W$esi-2 = &LF_ARGLIST_CV3
        Mov D@OldCodeView &TRUE
    End_If

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        Push ecx
        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'Constant.LeafType.ArgList.' EOS}
            zCopy {B$ 'ArgCount: W$ ' EOS}
        Pop esi
        xor eax eax
        lodsw | Call WriteEax
        Mov W$edi CRLF | add edi 2
        Pop ecx

        movzx ecx W$esi-2

    Mov D$ArgListIndexCount ecx
    Mov D$LeafTypeArgListIndice '0000', D$LeafTypeArgListIndice+4 '01'

    .If ecx <> 0 ; if the amount of bytes is 0 jmp over

        L0:
            Call WriteRawDataDebugSContantLeafTypeArgListItem CVLeafTypeArgListIndice, {B$ ': W$ ' EOS}
            If D@OldCodeView = &TRUE
                Call WriteRawDataDebugSContantLeafTypeArgListItem CVLeafTypeArgListOldCVType, {B$ ': W$ ' EOS}
            End_If

            Call IncrementLeafTypeArgListIndex
        Loop L0<

    .End_If

    Pop ebx

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP


________________________________________________________________________________

[NestedLeafType: D$ 0]

Proc WriteRawDataDebugSContantLeafTypeFieldList:
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx
    ;Mov D@EndAddr ebx


    Mov D$NestedLeafType &TRUE
    Mov D$LeafTypeArrayObjIndice '0000', D$LeafTypeArrayObjIndice+4 '01'

    .Do

        Push esi
            Mov W$edi CRLF | add edi 2
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'Constant.LeafType.Descriptor' EOS}
        Pop esi

        ; from here we must insert a Buffer flag saying it is nested to we use the array indexs
;            Push ebx

        ..If W$esi = &LF_BCLASS
            Push esi | zCopy {B$ '.BClass.Arr' EOS} | Pop esi
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_BCLASS' EOS}
            Call WriteRawDataDebugSContantLeafTypeBClass &FALSE

        ..Else_If W$esi = &LF_VBCLASS
            Push esi | zCopy {B$ '.VBClass.Arr' EOS} | Pop esi
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_VBCLASS' EOS}
            Call WriteRawDataDebugSContantLeafTypeVBClass &FALSE

        ..Else_If W$esi = &LF_IVBCLASS
            Push esi | zCopy {B$ '.IVBClass.Arr' EOS} | Pop esi
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_IVBCLASS' EOS}
            Call WriteRawDataDebugSContantLeafTypeIVBClass &FALSE

        ..Else_If W$esi = &LF_ENUMERATE
            Push esi | zCopy {B$ '.Enumerate.Arr' EOS} | Pop esi
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_ENUMERATE' EOS}
            Call WriteRawDataDebugSContantLeafTypeEnumerate &FALSE

        ..Else_If W$esi = &LF_FRIENDFCN
            Push esi | zCopy {B$ '.FriendFcn.Arr' EOS} | Pop esi
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_FRIENDFCN' EOS}
            Call WriteRawDataDebugSContantLeafTypeFriendFcn &FALSE

        ..Else_If W$esi = &LF_INDEX
            Push esi | zCopy {B$ '.Index.Arr' EOS} | Pop esi
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_INDEX' EOS}
            Call WriteRawDataDebugSContantLeafTypeIndex &FALSE

        ..Else_If W$esi = &LF_MEMBER;, W$esi = 01405
            ;WriteRawDataDebugSRegister
            ;Push ebx
            Push esi | zCopy {B$ '.Member.Arr' EOS} | Pop esi
                If W$esi = &LF_MEMBER
                    Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_MEMBER' EOS}
                Else
                    ;Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_MEMBER_CV3' EOS}
                End_If
                Call WriteRawDataDebugSContantLeafTypeMember &FALSE
            ;Pop ebx
        ..Else_If W$esi = &LF_STMEMBER
            Push esi | zCopy {B$ '.STMember.Arr' EOS} | Pop esi
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_STMEMBER' EOS}
            Call WriteRawDataDebugSContantLeafTypeSTMember &FALSE

        ..Else_If W$esi = &LF_METHOD
            Push esi | zCopy {B$ '.Method.Arr' EOS} | Pop esi
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_METHOD' EOS}
            Call WriteRawDataDebugSContantLeafTypeMethod &FALSE

        ..Else_If W$esi = &LF_NESTTYPE
            Push esi | zCopy {B$ '.NestType.Arr' EOS} | Pop esi
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_NESTTYPE' EOS}
            Call WriteRawDataDebugSContantLeafTypeNestType &FALSE

        ..Else_If W$esi = &LF_VFUNCTAB
            Push esi | zCopy {B$ '.VFuncTab.Arr' EOS} | Pop esi
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_VFUNCTAB' EOS}
            Call WriteRawDataDebugSContantLeafTypeVFuncTab &FALSE

        ..Else_If W$esi = &LF_FRIENDCLS
            Push esi | zCopy {B$ '.FriendCls.Arr' EOS} | Pop esi
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_FRIENDCLS' EOS}
            Call WriteRawDataDebugSContantLeafTypeFriendCls &FALSE

        ..Else_If W$esi = &LF_ONEMETHOD
            Push esi | zCopy {B$ '.OneMethod.Arr' EOS} | Pop esi
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_ONEMETHOD' EOS}
            Call WriteRawDataDebugSContantLeafTypeOneMethod &FALSE

        ..Else_If W$esi = &LF_VFUNCOFF
            Push esi | zCopy {B$ '.VFuncOffset.Arr' EOS} | Pop esi
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_VFUNCOFF' EOS}
            Call WriteRawDataDebugSContantLeafTypeVFuncOffset &FALSE

        ..End_If
;            Push ebx
            .If_And B$esi > &LF_PAD0, B$esi =< &LF_PAD15

                movzx ecx B$esi
                sub ecx 0F0
                Mov W$edi CRLF | add edi 2
                Call WriteCVdataTitle

                    Mov edx 0
                    xor eax eax

                L0:

                    movzx eax B$esi
                    Push ebx | Call WriteEax | Pop ebx

                    Mov W$edi ', ' | add edi 2 | inc esi
                    inc edx
                    If edx > 15
                        Mov W$edi CRLF | add edi 2 | Mov edx 0
                        Mov D$edi '    ', D$edi+4 '    ' | add edi 8
                    End_If
                Loop L0<
                sub edi 2
                dec edi
                Mov W$edi+1 CRLF | add edi 2
                inc edi

            .End_If

            Call IncrementLeafTypeArrayIndex
;            Pop ebx
            ;Pop ebx
        .Loop_Until esi = ebx
;L2:
    Mov D$NestedLeafType &FALSE
    Mov W$edi CRLF | add edi 2
;    sub edi 2
 ;   dec edi
  ;  Mov W$edi+1 CRLF | add edi 2
   ; inc edi

EndP


________________________________________________________________________________

[LeafTypeArrayObjIndice: B$ '000001' EOS]

Proc IncrementLeafTypeArrayIndex:

    pushad

    lea ebx D$LeafTypeArrayObjIndice+5 | inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx | inc B$ebx
    End_While

    popad

EndP
____________________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeBClassItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.BClass.' EOS}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {B$ '.Arr' EOS}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Type'; From "Type" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Attr'; From "Attribute" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Offs'; From "Offset" string
            xor eax eax
            lodsw | Call WriteEax

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeBClassType: B$ 'Type' EOS]
[CVLeafTypeVFuncTypeBClassAttribute: B$ 'Attribute' EOS]
[CVLeafTypeVFuncTypeBClassOffset: B$ 'Offset' EOS]

Proc WriteRawDataDebugSContantLeafTypeBClass:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeBClassItem CVLeafTypeVFuncTypeBClassType, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeBClassItem CVLeafTypeVFuncTypeBClassAttribute, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeBClassItem CVLeafTypeVFuncTypeBClassOffset, {B$ ': W$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeVBClassItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.VBClass.' EOS}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {B$ '.Arr' EOS}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Inde'; From "Index" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'bTyp'; From "bType" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'VbTy'; From "VbType" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Attr'; From "Attribute" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Vbpo'; From "Vbpoff" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Vbof'; From "Vboff" string
            xor eax eax
            lodsw | Call WriteEax

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeVBClassType: B$ 'Index' EOS]
[CVLeafTypeVFuncTypeVBClassbType: B$ 'bType' EOS]
[CVLeafTypeVFuncTypeVBClassvbType: B$ 'VbType' EOS]
[CVLeafTypeVFuncTypeVBClassAttribute: B$ 'Attribute' EOS]
[CVLeafTypeVFuncTypeVBClassvbpoff: B$ 'Vbpoff' EOS]
[CVLeafTypeVFuncTypeVBClassvboff: B$ 'Vboff' EOS]

Proc WriteRawDataDebugSContantLeafTypeVBClass:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeVBClassItem CVLeafTypeVFuncTypeVBClassType, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeVBClassItem CVLeafTypeVFuncTypeVBClassbType, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeVBClassItem CVLeafTypeVFuncTypeVBClassvbType, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeVBClassItem CVLeafTypeVFuncTypeVBClassAttribute, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeVBClassItem CVLeafTypeVFuncTypeVBClassvbpoff, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeVBClassItem CVLeafTypeVFuncTypeVBClassvboff, {B$ ': W$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeIVBClassItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.IVBClass.' EOS}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {B$ '.Arr' EOS}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Inde'; From "Index" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'bTyp'; From "bType" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'VbTy'; From "VbType" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Attr'; From "Attribute" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Vbpo'; From "Vbpoff" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Vbof'; From "Vboff" string
            xor eax eax
            lodsw | Call WriteEax

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeIVBClassType: B$ 'Index' EOS]
[CVLeafTypeVFuncTypeIVBClassbType: B$ 'bType' EOS]
[CVLeafTypeVFuncTypeIVBClassvbType: B$ 'VbType' EOS]
[CVLeafTypeVFuncTypeIVBClassAttribute: B$ 'Attribute' EOS]
[CVLeafTypeVFuncTypeIVBClassvbpoff: B$ 'Vbpoff' EOS]
[CVLeafTypeVFuncTypeIVBClassvboff: B$ 'Vboff' EOS]


Proc WriteRawDataDebugSContantLeafTypeIVBClass:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeIVBClassItem CVLeafTypeVFuncTypeIVBClassType, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeIVBClassItem CVLeafTypeVFuncTypeIVBClassbType, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeIVBClassItem CVLeafTypeVFuncTypeIVBClassvbType, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeIVBClassItem CVLeafTypeVFuncTypeIVBClassAttribute, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeIVBClassItem CVLeafTypeVFuncTypeIVBClassvbpoff, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeIVBClassItem CVLeafTypeVFuncTypeIVBClassvboff, {B$ ': W$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeEnumerateItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.Enumerate.' EOS}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {B$ '.Arr' EOS}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1


        ..If D$eax = 'Attr'; From "Attribute" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Valu'; From "Value" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeEnumerateAttribute: B$ 'Attribute' EOS]
[CVLeafTypeVFuncTypeEnumerateValue: B$ 'Value' EOS]
[CVLeafTypeVFuncTypeEnumerateNameLenght: B$ 'NameLenght' EOS]
[CVLeafTypeVFuncTypeEnumerateName: B$ 'Name' EOS]

Proc WriteRawDataDebugSContantLeafTypeEnumerate:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeEnumerateItem CVLeafTypeVFuncTypeEnumerateAttribute, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeEnumerateItem CVLeafTypeVFuncTypeEnumerateValue, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeEnumerateItem CVLeafTypeVFuncTypeEnumerateNameLenght, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeEnumerateItem CVLeafTypeVFuncTypeEnumerateName, {B$ ': B$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeFriendFcnItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.FriendFcn.' EOS}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {B$ '.Arr' EOS}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Type'; From "Type" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeFriendFcnType: B$ 'Type' EOS]
[CVLeafTypeVFuncTypeFriendFcnNameLenght: B$ 'NameLenght' EOS]
[CVLeafTypeVFuncTypeFriendFcnName: B$ 'Name' EOS]

Proc WriteRawDataDebugSContantLeafTypeFriendFcn:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeFriendFcnItem CVLeafTypeVFuncTypeFriendFcnType, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeFriendFcnItem CVLeafTypeVFuncTypeFriendFcnNameLenght, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeFriendFcnItem CVLeafTypeVFuncTypeFriendFcnName, {B$ ': B$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeIndexItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.Type.' EOS}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {B$ '.Arr' EOS}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        xor eax eax
        lodsw | Call WriteEax

        Mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeTypeIndex: B$ 'Index' EOS]

Proc WriteRawDataDebugSContantLeafTypeIndex:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeIndexItem CVLeafTypeVFuncTypeTypeIndex, {B$ ': W$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeMemberItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.Member.' EOS}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {B$ '.Arr' EOS}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        ..If D$eax = 'OldC' ; from "OldCVType" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Type'; From "Type" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Attr'; From "Attribute" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Offs'; From "Offset" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeMemberOldCVType: B$ 'OldCVType' EOS]
[CVLeafTypeVFuncTypeMemberType: B$ 'Type' EOS]
[CVLeafTypeVFuncTypeMemberAttribute: B$ 'Attribute' EOS]
[CVLeafTypeVFuncTypeMemberOffset: B$ 'Offset' EOS]
[CVLeafTypeVFuncTypeMemberNameLenght: B$ 'NameLenght' EOS]
[CVLeafTypeVFuncTypeMemberName: B$ 'Name' EOS]
;WriteRawDataDebugSRegister
Proc WriteRawDataDebugSContantLeafTypeMember:
    Arguments @Nested
    Local @OldCodeView
    Uses eax, ecx, ebx

    Mov D@OldCodeView &FALSE
    If W$esi-2 = 01405
        Mov D@OldCodeView &TRUE
    End_If
    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        If D@OldCodeView = &TRUE
            Call WriteRawDataDebugSContantLeafTypeMemberItem CVLeafTypeVFuncTypeMemberOldCVType , {B$ ': W$ ' EOS}
        End_If
        Call WriteRawDataDebugSContantLeafTypeMemberItem CVLeafTypeVFuncTypeMemberType, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeMemberItem CVLeafTypeVFuncTypeMemberAttribute, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeMemberItem CVLeafTypeVFuncTypeMemberOffset, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeMemberItem CVLeafTypeVFuncTypeMemberNameLenght, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeMemberItem CVLeafTypeVFuncTypeMemberName, {B$ ': B$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeSTMemberItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.StMember.' EOS}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {B$ '.Arr' EOS}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Type'; From "Type" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Attr'; From "Attribute" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeStMemberType: B$ 'Type' EOS]
[CVLeafTypeVFuncTypeStMemberAttribute: B$ 'Attribute' EOS]
[CVLeafTypeVFuncTypeStMemberNameLenght: B$ 'NameLenght' EOS]
[CVLeafTypeVFuncTypeStMemberName: B$ 'Name' EOS]

Proc WriteRawDataDebugSContantLeafTypeSTMember:
    Arguments @Nested
    Uses eax, ecx

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeSTMemberItem CVLeafTypeVFuncTypeStMemberType, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeSTMemberItem CVLeafTypeVFuncTypeStMemberAttribute, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeSTMemberItem CVLeafTypeVFuncTypeStMemberNameLenght, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeSTMemberItem CVLeafTypeVFuncTypeStMemberName, {B$ ': B$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeMethodItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.Method.' EOS}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {B$ '.Arr' EOS}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Coun'; From "Count" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'mLis'; From "mList" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeMethodCount: B$ 'Count' EOS]
[CVLeafTypeVFuncTypeMethodmList: B$ 'mList' EOS]
[CVLeafTypeVFuncTypeMethodNameLenght: B$ 'NameLenght' EOS]
[CVLeafTypeVFuncTypeMethodTypeName: B$ 'Name' EOS]

Proc WriteRawDataDebugSContantLeafTypeMethod:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeMethodItem CVLeafTypeVFuncTypeMethodCount, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeMethodItem CVLeafTypeVFuncTypeMethodmList, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeMethodItem CVLeafTypeVFuncTypeMethodNameLenght, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeMethodItem CVLeafTypeVFuncTypeMethodTypeName, {B$ ': B$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP
_________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeNestTypeItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.NestedType.' EOS}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {B$ '.Arr' EOS}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Inde'; From "Index" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeNestTypeIndex: B$ 'Index' EOS]
[CVLeafTypeVFuncTypeNestTypeNameLenght: B$ 'NameLenght' EOS]
[CVLeafTypeVFuncTypeNestTypeName: B$ 'Name' EOS]

Proc WriteRawDataDebugSContantLeafTypeNestType:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeNestTypeItem CVLeafTypeVFuncTypeNestTypeIndex, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeNestTypeItem CVLeafTypeVFuncTypeNestTypeNameLenght, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeNestTypeItem CVLeafTypeVFuncTypeNestTypeName, {B$ ': B$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP
_________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeVFuncTabItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.VFuncTab.' EOS}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {B$ '.Arr' EOS}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        xor eax eax
        lodsw | Call WriteEax

        Mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeVFuncTabType: B$ 'Type' EOS]

Proc WriteRawDataDebugSContantLeafTypeVFuncTab:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeVFuncTabItem CVLeafTypeVFuncTypeVFuncTabType, {B$ ': W$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeFriendClsItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.FriendClass.' EOS}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {B$ '.Arr' EOS}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        xor eax eax
        lodsw | Call WriteEax

        Mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeFriendClsType: B$ 'Type' EOS]

Proc WriteRawDataDebugSContantLeafTypeFriendCls:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeFriendClsItem CVLeafTypeVFuncTypeFriendClsType, {B$ ': W$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP

__________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeOneMethodItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.OneMethod.' EOS}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {B$ '.Arr' EOS}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Attr'; From "Attribute" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Type'; From "Type" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'vBas'; From "vBaseOffset" string
            lodsd | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeOneMethodattribute: B$ 'Attribute' EOS]
[CVLeafTypeVFuncTypeOneMethodType: B$ 'Type' EOS]
[CVLeafTypeVFuncTypeOneMethodvBaseOffset: B$ 'vBaseOffset' EOS]
[CVLeafTypeVFuncTypeOneMethodNameLenght: B$ 'NameLenght' EOS]
[CVLeafTypeVFuncTypeOneMethodName: B$ 'Name' EOS]

Proc WriteRawDataDebugSContantLeafTypeOneMethod:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeOneMethodItem CVLeafTypeVFuncTypeOneMethodattribute, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeOneMethodItem CVLeafTypeVFuncTypeOneMethodType, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeOneMethodItem CVLeafTypeVFuncTypeOneMethodvBaseOffset, {B$ ': D$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeOneMethodItem CVLeafTypeVFuncTypeOneMethodNameLenght, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeOneMethodItem CVLeafTypeVFuncTypeOneMethodName, {B$ ': B$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeVFuncOffsetItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.VFunc.' EOS}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {B$ '.Arr' EOS}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

         ..If D$eax = 'Type'; From "Type" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Offs'; From "Offset" string
            lodsd | Call WriteEax

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncType: B$ 'Type' EOS]
[CVLeafTypeVFuncOffset: B$ 'Offset' EOS]

Proc WriteRawDataDebugSContantLeafTypeVFuncOffset:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeVFuncOffsetItem CVLeafTypeVFuncType, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeVFuncOffsetItem CVLeafTypeVFuncOffset, {B$ ': D$ ' EOS}
        sub edi 2
    Pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeComplexNumber:
    Argument @Text, @Size
    Uses eax, ecx

    Mov ebx esi
    Mov ecx D@Size
    add ebx ecx

    .If esi <> ebx
        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'Constant.LeafType.' EOS}
            zCopy D@Text
            zCopy {B$ ': B$ ' EOS}
        Pop esi

        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

        sub edi 2
        dec edi
        Mov W$edi+1 CRLF | add edi 2
        Pop ecx
        inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP
________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeVarStrItem:
    Argument @Text1, @Text2
    uses eax, ecx

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'Constant.LeafType.VarStr' EOS}
        zCopy D@Text1
        zCopy D@Text2
    Pop esi

        Mov eax D@Text1

        ..If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx W$esi-2
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

______________________________________________

Proc WriteRawDataDebugSContantLeafTypeVarStr:
    Uses eax, ecx

    Call WriteRawDataDebugSContantLeafTypeVarStrItem CVCompileNameLenght, {B$ ': W$ ' EOS}
    Call WriteRawDataDebugSContantLeafTypeVarStrItem CVCompileName, {B$ ': B$ ' EOS}
    sub edi 2

EndP
______________________________________________

Proc WriteRawDataDebugSContantLeafTypeEquate:
    Argument @Text
    uses eax, ecx, ebx


    If D$NestedLeafType = &TRUE
        Push esi | zCopy LeafTypeArrayObjIndice | Pop esi
    End_If
    Push esi
        zCopy {B$ ': W$ ' EOS}
        zCopy D@Text
        zCopy {B$ ' ; Hex Value:  ' EOS}
    Pop esi
    xor eax eax
    lodsw | Call WriteEax
    Mov W$edi CRLF | add edi 2

EndP

___________________________________________

[LeafTypeError: D$ &TRUE]

; The PDebug section is nothing but a Type.
; Note: Check the file SDKUTIL from lcc library

Proc WriteRawDataDebugSContantLeafTypeItem:
    Uses eax, ecx

    Mov D$LeafTypeError &TRUE ; In Any case, the default is True, meaning that we have no errors.

        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'Constant.LeafType' EOS}
        Pop esi

    ; 1st we need to identify the Undefined Leaf Types. Accordying to the documentation, we have:

    ; a) No LF_... index can have a value of 0x0000.
    ; b) Because of the method used to maintain natural alignment in complex lists, no leaf index can
    ;    have a value greater than or equal to 0xf000.
    ; c) Also, no leaf index can have a value such that the least significant 8 bits of the value
    ;    is greater than or equal to 0xf0.

    ...If W$esi = 0
        jmp L0>
    ...Else_If W$esi >= 0F000
L0:     Push esi | zCopy {B$ ': W$ ' EOS} | Pop esi
        xor eax eax
        lodsw | Call WriteEax
        zCopy {B$ ' ; No Defined Type' EOS}

        Mov D$LeafTypeError &FALSE ; Set the error case

    ...Else_If B$esi >= 0F0

        Push esi | zCopy {B$ '.Padding: B$ ' EOS} | Pop esi

        Push esi

        .If B$esi = &LF_PAD0
            zCopy {B$ '&LF_PAD0 ; Hex Value:  ' EOS}
        .Else_If B$esi = &LF_PAD1
            zCopy {B$ '&LF_PAD1 ; Hex Value:  ' EOS}
        .Else_If B$esi = &LF_PAD2
            zCopy {B$ '&LF_PAD2 ; Hex Value:  ' EOS}
        .Else_If B$esi = &LF_PAD3
            zCopy {B$ '&LF_PAD3 ; Hex Value:  ' EOS}
        .Else_If B$esi = &LF_PAD4
            zCopy {B$ '&LF_PAD4 ; Hex Value:  ' EOS}
        .Else_If B$esi = &LF_PAD5
            zCopy {B$ '&LF_PAD5 ; Hex Value:  ' EOS}
        .Else_If B$esi = &LF_PAD6
            zCopy {B$ '&LF_PAD6 ; Hex Value:  ' EOS}
        .Else_If B$esi = &LF_PAD7
            zCopy {B$ '&LF_PAD7 ; Hex Value:  ' EOS}
        .Else_If B$esi = &LF_PAD8
            zCopy {B$ '&LF_PAD8 ; Hex Value:  ' EOS}
        .Else_If B$esi = &LF_PAD9
            zCopy {B$ '&LF_PAD9 ; Hex Value:  ' EOS}
        .Else_If B$esi = &LF_PAD10
            zCopy {B$ '&LF_PAD10 ; Hex Value:  ' EOS}
        .Else_If B$esi = &LF_PAD11
            zCopy {B$ '&LF_PAD11 ; Hex Value:  ' EOS}
        .Else_If B$esi = &LF_PAD12
            zCopy {B$ '&LF_PAD12 ; Hex Value:  ' EOS}
        .Else_If B$esi = &LF_PAD13
            zCopy {B$ '&LF_PAD13 ; Hex Value:  ' EOS}
        .Else_If B$esi = &LF_PAD14
            zCopy {B$ '&LF_PAD14 ; Hex Value:  ' EOS}
        .Else_If B$esi = &LF_PAD15
            zCopy {B$ '&LF_PAD15 ; Hex Value:  ' EOS}
        .End_If

        Pop esi

        xor eax eax
        lodsb | Call WriteEax

        Push esi
            zCopy {B$ ' - No Defined Type' EOS}
            Mov W$edi CRLF | add edi 2
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ '.Undefined: B$ ' EOS}
        Pop esi

        xor eax eax
        lodsb | Call WriteEax
        zCopy {B$ ' ; No Defined Type' EOS}

        Mov D$LeafTypeError &FALSE ; Set the error case

    ; 2nd We must now identify the known defined Types.

    ...Else

        ..If_Or W$esi = &LF_MODIFIER, W$esi = &LF_MODIFIER_CV3
            If W$esi = &LF_MODIFIER
                Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_MODIFIER' EOS}
            Else
                Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_MODIFIER_CV3' EOS}
            End_If
            Call WriteRawDataDebugSContantLeafTypeModifier

        ..Else_If_Or W$esi = &LF_POINTER, W$esi = &LF_POINTER_CV3 ; see sdkutil.lib to we handle the equates for the different Bits (A true hell)
            If W$esi = &LF_POINTER
                Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_POINTER' EOS}
            Else
                Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_POINTER_CV3' EOS}
            End_If
            Call WriteRawDataDebugSContantLeafTypePointer

        ..Else_If W$esi = &LF_ARRAY
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_ARRAY' EOS}
            Call WriteRawDataDebugSContantLeafTypeArray

        ..Else_If W$esi = &LF_CLASS
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_CLASS' EOS}
            Call WriteRawDataDebugSContantLeafTypeStructure

        ..Else_If_Or W$esi = &LF_STRUCTURE, W$esi = &LF_STRUCTURE_CV3
            If W$esi = &LF_STRUCTURE
                Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_STRUCTURE' EOS}
            Else
                Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_STRUCTURE_CV3' EOS}
            End_If
            Call WriteRawDataDebugSContantLeafTypeStructure

        ..Else_If_Or W$esi = &LF_UNION, W$esi = &LF_UNION_CV3
            If W$esi = &LF_UNION
                Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_UNION' EOS}
            Else
                Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_UNION_CV3' EOS}
            End_If
            Call WriteRawDataDebugSContantLeafTypeUnion

        ..Else_If W$esi = &LF_ENUM
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_ENUM' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_PROCEDURE
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_PROCEDURE' EOS}
            Call WriteRawDataDebugSContantLeafTypeProcedure

        ..Else_If_Or W$esi = &LF_MFUNCTION, W$esi = &LF_MFUNCTION_CV3
            If W$esi = &LF_MFUNCTION
                Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_MFUNCTION' EOS}
            Else
                Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_MFUNCTION_CV3' EOS}
            End_If
                Call WriteRawDataDebugSContantLeafTypeMFunction

        ..Else_If W$esi = &LF_VTSHAPE
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_VTSHAPE' EOS}
            sub esi 2
            jmp L0>>
            ; Note to Guga. The error in sdkutil is here. Missing this parse

        ..Else_If W$esi = &LF_COBOL0
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_COBOL0' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_COBOL1
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_COBOL1' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_BARRAY
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_BARRAY' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_LABEL
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_LABEL' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_NULL
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_NULL' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_NOTTRAN
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_NOTTRAN' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_DIMARRAY
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_DIMARRAY' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_VFTPATH
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_VFTPATH' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_PRECOMP
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_PRECOMP' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_ENDPRECOMP
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_ENDPRECOMP' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_OEM
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_OEM' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_RESERVED
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_RESERVED' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_SKIP
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_SKIP' EOS}
            jmp L0>>

        ..Else_If_Or W$esi = &LF_ARGLIST, W$esi = &LF_ARGLIST_CV3
            If W$esi = &LF_ARGLIST
                Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_ARGLIST' EOS}
            Else
                Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_ARGLIST_CV3' EOS}
            End_If
            Call WriteRawDataDebugSContantLeafTypeArgList

        ..Else_If W$esi = &LF_DEFARG
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_DEFARG' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_LIST
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_LIST' EOS}
            jmp L0>>

        ..Else_If_Or W$esi = &LF_FIELDLIST;, W$esi = 01203
        ;WriteRawDataDebugSRegister
            If W$esi = &LF_FIELDLIST
                Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_FIELDLIST' EOS}
            Else
                ;Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_FIELDLIST_CV3' EOS}
            End_If
            Call WriteRawDataDebugSContantLeafTypeFieldList

        ..Else_If W$esi = &LF_DERIVED
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_DERIVED' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_BITFIELD
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_BITFIELD' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_METHODLIST
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_METHODLIST' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_DIMCONU
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_DIMCONU' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_DIMCONLU
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_DIMCONLU' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_DIMVARU
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_DIMVARU' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_DIMVARLU
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_DIMVARLU' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_REFSYM
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_REFSYM' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_BCLASS
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_BCLASS' EOS}
            Call WriteRawDataDebugSContantLeafTypeBClass &FALSE

        ..Else_If W$esi = &LF_VBCLASS
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_VBCLASS' EOS}
            Call WriteRawDataDebugSContantLeafTypeVBClass &FALSE

        ..Else_If W$esi = &LF_IVBCLASS
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_IVBCLASS' EOS}
            Call WriteRawDataDebugSContantLeafTypeIVBClass &FALSE

        ..Else_If W$esi = &LF_ENUMERATE
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_ENUMERATE' EOS}
            Call WriteRawDataDebugSContantLeafTypeEnumerate &FALSE

        ..Else_If W$esi = &LF_FRIENDFCN
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_FRIENDFCN' EOS}
            Call WriteRawDataDebugSContantLeafTypeFriendFcn &FALSE

        ..Else_If W$esi = &LF_INDEX
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_INDEX' EOS}
            Call WriteRawDataDebugSContantLeafTypeIndex &FALSE

        ..Else_If W$esi = &LF_MEMBER
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_MEMBER' EOS}
            Call WriteRawDataDebugSContantLeafTypeMember &FALSE

        ..Else_If W$esi = &LF_STMEMBER
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_STMEMBER' EOS}
            Call WriteRawDataDebugSContantLeafTypeSTMember &FALSE

        ..Else_If W$esi = &LF_METHOD
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_METHOD' EOS}
            Call WriteRawDataDebugSContantLeafTypeMethod &FALSE

        ..Else_If W$esi = &LF_NESTTYPE
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_NESTTYPE' EOS}
            Call WriteRawDataDebugSContantLeafTypeNestType &FALSE

        ..Else_If W$esi = &LF_VFUNCTAB
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_VFUNCTAB' EOS}
            Call WriteRawDataDebugSContantLeafTypeVFuncTab &FALSE

        ..Else_If W$esi = &LF_FRIENDCLS
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_FRIENDCLS' EOS}
            Call WriteRawDataDebugSContantLeafTypeFriendCls &FALSE

        ..Else_If W$esi = &LF_ONEMETHOD
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_ONEMETHOD' EOS}
            Call WriteRawDataDebugSContantLeafTypeOneMethod &FALSE

        ..Else_If W$esi = &LF_VFUNCOFF
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_VFUNCOFF' EOS}
            Call WriteRawDataDebugSContantLeafTypeVFuncOffset &FALSE

        ..Else_If W$esi = &LF_NUMERIC
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_NUMERIC' EOS}
            jmp L0>>

        ..Else_If W$esi = &LF_CHAR
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_CHAR' EOS}
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Char' EOS}, 1

        ..Else_If W$esi = &LF_SHORT
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_SHORT' EOS}
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Short' EOS}, 2

        ..Else_If W$esi = &LF_USHORT
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_USHORT' EOS}
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'UShort' EOS}, 2

        ..Else_If W$esi = &LF_LONG
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_LONG' EOS}
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Long' EOS}, 4

        ..Else_If W$esi = &LF_ULONG
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_ULONG' EOS}
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'ULong' EOS}, 4

        ..Else_If W$esi = &LF_REAL32
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_REAL32' EOS}
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Float32Bit' EOS}, 4

        ..Else_If W$esi = &LF_REAL64
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_REAL64' EOS}
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Float64Bit' EOS}, 8

        ..Else_If W$esi = &LF_REAL80
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_REAL80' EOS}
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Float80Bit' EOS}, 10

        ..Else_If W$esi = &LF_REAL128
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_REAL128' EOS}
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Float128Bit' EOS}, 16

        ..Else_If W$esi = &LF_QUADWORD
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_QUADWORD' EOS}
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Quadword' EOS}, 8

        ..Else_If W$esi = &LF_UQUADWORD
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_UQUADWORD' EOS}
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'UQuadword' EOS}, 8

        ..Else_If W$esi = &LF_REAL48
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_REAL48' EOS}
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Real48Bit' EOS}, 6

        ..Else_If W$esi = &LF_COMPLEX32
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_COMPLEX32' EOS}
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Complex32BitReal' EOS}, 4
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Complex32BitImaginary' EOS}, 4

        ..Else_If W$esi = &LF_COMPLEX64
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_COMPLEX64' EOS}
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Complex64BitReal' EOS}, 8
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Complex64BitImaginary' EOS}, 8

        ..Else_If W$esi = &LF_COMPLEX80
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_COMPLEX80' EOS}
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Complex80BitReal' EOS}, 10
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Complex80BitImaginary' EOS}, 10

        ..Else_If W$esi = &LF_COMPLEX128
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_COMPLEX128' EOS}
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Complex128BitReal' EOS}, 16
            Call WriteRawDataDebugSContantLeafTypeComplexNumber {B$ 'Complex128BitImaginary' EOS}, 16

        ..Else_If W$esi = &LF_VARSTRING
            Call WriteRawDataDebugSContantLeafTypeEquate {B$ '&LF_VARSTRING' EOS}
            Call WriteRawDataDebugSContantLeafTypeVarStr

        ..Else
L0:

            Push ecx
            Push eax
            Push esi
                If D$NestedLeafType = &TRUE
                    zCopy LeafTypeArrayObjIndice
                End_If
                zCopy {B$ '.TypeUnknown: W$ ' EOS}
            Pop esi
            xor eax eax
            lodsw | Call WriteEax
            Mov W$edi CRLF | add edi 2
            Pop eax
            Pop ecx
            sub D$SizeAdd 2


        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'Constant.LeafType' EOS}
        Pop esi


            Call WriteRawDataDebugSContantLeafTypeUnkown
            Mov D$LeafTypeError &FALSE ; Set the error case

        ..End_If

    ...End_If

    Mov W$edi CRLF | add edi 2

EndP
__________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeUnkown:
    Uses eax, ecx


    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
;    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx
;;
;;
    Push ebx

                    Push esi
                        Mov D$edi '    ' | add edi 4
                        Call WriteObjIndice
                        zCopy {B$ "Sec" EOS}
                        zCopy SectionHeaderNumber
                        zCopy {B$ ".Index" EOS}
                        zCopy DebugNumber
                        zCopy D$CVLabel
                        zCopy {B$ 'Constant.LeafType.Unknown: B$ ' EOS}
                    Pop esi
    Pop ebx
;;
;;

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayElemType , {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayIdxType , {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayLength, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayNamelenght, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayName, {B$ ': B$ ' EOS}
        sub edi 2
    Pop ebx
;;

;;
    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
;;
;;
        Mov W$edi CRLF | add edi 2
        ;Call WriteCVdataTitle
                    Push esi
                        Mov D$edi '    ' | add edi 4
                        Call WriteObjIndice
                        zCopy {B$ "Sec" EOS}
                        zCopy SectionHeaderNumber
                        zCopy {B$ ".Index" EOS}
                        zCopy DebugNumber
                        zCopy D$CVLabel
                        zCopy {B$ 'Constant.LeafType.Unknown: B$ ' EOS}
                    Pop esi

;;
;;
        Push esi | ZCopy {B$ '.Unknown: B$ ' EOS} | Pop esi

        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP
__________________________________________________________________

Proc WriteRawDataDebugSContantItem:
    Argument @Text1, @Text2, @UseLen
    uses eax, ecx, edx

        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'Constant.' EOS}
            zCopy D@Text1
            zCopy D@Text2
        Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Type' ; from "Type" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'OldC' ; from "OldCVType" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            If D@UseLen = &TRUE ; this happens only in S_CONSTANT_CV2.
                Push eax
                    Call StrLenProc esi
                    Mov ecx eax
                    inc ecx ; The size is increased to we alow including the zero byte
                Pop eax
            Else
                movzx ecx B$esi-1
            End_If

            Mov edx esi | add edx ecx


            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP
_______________________________________
;;

;;
Structures used:

. Note, in all equates used to dirrefernciate the structures, when Type member is 0 (&T_NOTYPE), the structure
is related to a enumeration, and the leaf member types (achieved from WriteRawDataDebugSContantLeafTypeItem) are not
used. Instead, the Constant.LeafType is replaced by an EnumerationValue member

 - S_CONSTANT_CV3 uses:
 
     CV3.Length: W$ 015
     CV3.Index: W$ &S_CONSTANT_CV3; Hex Value:  01002
     CV3.Constant.OldCVType: W$ 01058
     CV3.Constant.Type: W$ &T_NOTYPE ; Uncharacterized type (no type) - Hex Value:  0
     CV3.Constant.EnumerationValue: W$ 02 ; This symbol is an enumeration constant type.
     CV3.Constant.NameLenght: B$ 0C
     CV3.Constant.Name: B$ 'STUB_MARSHAL'

 - S_CONSTANT_CV2 uses:
 
     CV3.Length: W$ 015
     CV3.Index: W$ &S_CONSTANT_CV2; Hex Value:  01107
     CV3.Constant.OldCVType: W$ 01058
     CV3.Constant.Type: W$ &T_NOTYPE ; Uncharacterized type (no type) - Hex Value:  0
     CV3.Constant.EnumerationValue: W$ 02 ; This symbol is an enumeration constant type.
     CV3.Constant.NameLenght: B$ 0C
     CV3.Constant.Name: B$ 'STUB_MARSHAL'

;;
;;
[CVConstantOldCVType: B$ 'OldCVType' EOS]
[CVConstantType: B$ 'Type' EOS]
[CVConstantNameLenght: B$ 'NameLenght' EOS]
[CVConstantName: B$ 'Name' EOS]

Proc WriteRawDataDebugSContant:
    Local @OldCodeView
    Uses ecx, eax


    Mov D@OldCodeView &FALSE
    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx

    If W$esi-2 = &S_CONSTANT_CV3
        Call WriteRawDataDebugSContantItem CVConstantOldCVType, {B$ ': W$ ' EOS}, &FALSE
    Else_If W$esi-2 = &S_CONSTANT_CV2
        Call WriteRawDataDebugSContantItem CVConstantOldCVType, {B$ ': W$ ' EOS}, &FALSE
        Mov D@OldCodeView &TRUE
    End_If

    Call WriteRawDataDebugSContantItem CVConstantType, {B$ ': W$ ' EOS}, &FALSE

    .If W$esi-2 = &T_NOTYPE
        Push ecx
        Push eax
        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'Constant.EnumerationValue: W$ ' EOS}
        Pop esi
        xor eax eax
        lodsw | Call WriteEax

        Push esi | zCopy {B$ ' ; This symbol is an enumeration constant type.' EOS} | Pop esi
        Mov W$edi CRLF | add edi 2
        Pop eax
        Pop ecx
    .Else
        Call WriteRawDataDebugSContantLeafTypeItem
    .End_If

    .If D$LeafTypeError <> &FALSE ; No Errors. Do next Line. Jmp otherwise
        If D@OldCodeView = &FALSE
            Call WriteRawDataDebugSContantItem CVCompileNameLenght, {B$ ': B$ ' EOS}, &FALSE
            Call WriteRawDataDebugSContantItem CVCompileName, {B$ ': B$ ' EOS}, &FALSE
        Else
            Call WriteRawDataDebugSContantItem CVCompileName, {B$ ': B$ ' EOS}, &TRUE
        End_If
    .End_If


    Pop ebx

    ; Is the end of this structure ends on the proper place ?

    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP

__________________________________________________________________

[CVUDTOldCVType: B$ 'OldCVType' EOS]
[CVUDTType: B$ 'Type' EOS]
[CVUDTNamelenght: B$ 'NameLenght' EOS]
[CVUDTName: B$ 'Name' EOS]

Proc WriteRawDataDebugSUDT:
    Local @OldCodeView
    Uses ecx

    Mov D@OldCodeView 0
    If W$esi-2 = &S_UDT_CV3
        Mov D@OldCodeView 1
    Else_If W$esi-2 = &S_UDT_CV2
        Mov D@OldCodeView 2
    End_If

    If D@OldCodeView <> 0
        Call WriteRawDataDebugSUDTItem CVUDTOldCVType , {B$ ': W$ ' EOS}, &FALSE
    End_If

    Call WriteRawDataDebugSUDTItem CVUDTType , {B$ ': W$ ' EOS}, &FALSE

    If D@OldCodeView = 2
        Call WriteRawDataDebugSUDTItem CVUDTName , {B$ ': B$ ' EOS}, &TRUE
    Else
        Call WriteRawDataDebugSUDTItem CVUDTNamelenght , {B$ ': B$ ' EOS}, &FALSE
        Call WriteRawDataDebugSUDTItem CVUDTName , {B$ ': B$ ' EOS}, &FALSE
    End_If
EndP
__________________________________________________________________

Proc WriteRawDataDebugSUDTItem:
    Argument @Text1, @Text2, @UseLen
    uses eax, ecx

        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'UDT.' EOS}
            zCopy D@Text1
            zCopy D@Text2
        Pop esi

        Mov eax D@Text1

        ..If D$eax = 'OldC'; From "OldCVType" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Type'; From "Type" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            If D@UseLen = &TRUE ; this happens only in S_CONSTANT_CV2.
                Push eax
                    Call StrLenProc esi
                    Mov ecx eax
                    inc ecx ; The size is increased to we alow including the zero byte
                Pop eax
            Else
                movzx ecx B$esi-1
            End_If

            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP
__________________________________________________________________



;WriteCVCompileFlag1Equates
;WriteCVGlobalProcFlagEquates
;;
;;
Flag Member:

The Flag member is a Byte value responsable for several different computations accordying to specified bits.
We need to shr the founded values to find the proper equates used. This member is the Flags for the specified
procedure.

Fpo :1  Uses Bit 0. When this bit is flagged the function has frame pointer omitted. Otherwise it don't use.
        Equates:    CV4_FUNCTION_FPO_ENABLED 01
                    CV4_FUNCTION_FPO_DISABLED 0

Interrupt :1  Uses Bit 1. When this bit is flagged the function is interrupt routine. Otherwise it is not.
              Equates:    CV4_FUNCTION_INT_ENABLED 01
                          CV4_FUNCTION_INT_DISABLED 0

Return :1  Uses Bit 2. When this bit is flagged the function performs Far return. Otherwise it doesn't.
            Equates:    CV4_FUNCTION_RET_ENABLED 01
                        CV4_FUNCTION_RET_DISABLED 0

Never :1  Uses Bit 3. When this bit is flagged the function never returns. Otherwise it does.
            Equates:    CV4_FUNCTION_NORET_ENABLED 01
                        CV4_FUNCTION_NORET_DISABLED 0

Bits 4 to 7 are not used. They must be settled to 0.

;;
;;
Proc WriteCVGlobalProcFlagEquates:
    Uses esi, ebx, eax

    xor eax eax | lodsb
   ; We must clear Bits 4 to 7 because they are not used
    btr eax 4 | btr eax 5 | btr eax 6 | btr eax 7
    Mov ebx eax

    Push ebx
    Push eax

    shr ebx 3 ; This is to calculate Never member. Shr by 3 because the Bit 3 is the one that is starting to be flagged.

    .If ebx = &CV4_FUNCTION_NORET_ENABLED
        zCopy {B$ '(&CV4_FUNCTION_NORET_ENABLED shl 3)' EOS}
    .Else;_If ebx = CV4_FUNCTION_NORET_DISABLED
        zCopy {B$ '(&CV4_FUNCTION_NORET_DISABLED shl 3)' EOS}
    .End_If

        zCopy {B$ ' + ' EOS}
        ; Now we must Zero bit 3 to shr again to find the next records for Return Member (Bit 2)
        btr eax 3
        ; After clearing the bits, we need to compute only the needed bits to be shred
        shr eax 2 ; This is to calculate Return member. Shr by 2 because the Bit 2 is the one that is starting to be flagged.

    .If eax = &CV4_FUNCTION_RET_ENABLED
        zCopy {B$ '(&CV4_FUNCTION_RET_ENABLED shl 2)' EOS}
    .Else;_If eax = CV4_FUNCTION_RET_DISABLED
        zCopy {B$ '(&CV4_FUNCTION_RET_DISABLED shl 2)' EOS}
    .End_If

    Pop eax
    Pop ebx

        zCopy {B$ ' + ' EOS}

    ; Now we restored the values of ebx and eax. We will going to find the values for
    ; Interrupt (Bit 1) and Fpo (Bit 0)
    ; Since we restored the values we must clear bits 2 to 3 (Rememeber that bits 4 to 7 are already cleared)

    ; zeroes bits 2 to 3
    btr eax 2 | btr eax 3

    shr eax 1 ; This is to calculate Interrupt. Shr by 1 because the Bit 1 is the one that is starting to be flagged.

    .If eax = &CV4_FUNCTION_INT_ENABLED
        zCopy {B$ '(&CV4_FUNCTION_INT_ENABLED shl 1)' EOS}
    .Else;_If eax = CV4_FUNCTION_INT_DISABLED
        zCopy {B$ '(&CV4_FUNCTION_INT_DISABLED shl 1)' EOS}
    .End_If

        zCopy {B$ ' + ' EOS}

    and ebx 01 ; This is to calculate Fpo.
               ; The value of 1 is because it is the maximum value of all bits flagged. (Bit 0)

    .If ebx = &CV4_FUNCTION_FPO_ENABLED
        zCopy {B$ '&CV4_FUNCTION_FPO_ENABLED' EOS}
    .Else
        zCopy {B$ '&CV4_FUNCTION_FPO_DISABLED' EOS}
    .End_If

        zCopy {B$ ' ; Hex Value:  ' EOS}

EndP

______________________________________

Proc WriteRawDataDebugSGlobalProcItem:
    Argument @Text1, @Text2
    uses eax, ecx

        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy D$CVGlobalProcLabel;{B$ 'GlobalProcedure.' EOS}
            zCopy D@Text1
            zCopy D@Text2
        Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Ppar' ; from "Pparent" string
            lodsd | Call WriteEax

        ..Else_If D$eax = 'Pend'; From "Pend" string
            lodsd | Call WriteEax

        ..Else_If D$eax = 'Pnex'; From "Pnext" string
            lodsd | Call WriteEax

        ..Else_If D$eax+2 = 'ocLe'; From "ProcLength" string
            lodsd | Call WriteEax

        ..Else_If D$eax+4 = 'gSta'; From "DebugStart" string
            lodsd | Call WriteEax

        ..Else_If D$eax+4 = 'gEnd'; From "DebugEnd" string
            lodsd | Call WriteEax

        ..Else_If D$eax = 'OldC'; From "OldCVType" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Offs'; From "Offset" string
            lodsd | Call WriteEax

        ..Else_If D$eax = 'Segm'; From "Segment" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+2 = 'octy'; From "Proctype" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax
            .If W$esi-2 >= &CV_FIRST_NONPRIM
                Push esi | ZCopy {B$ "; This is a Non-Primitive Type. The value is: " EOS} | Pop esi
                movzx eax W$esi-2
                sub eax &CV_FIRST_NONPRIM
                Call WriteEax
            .End_If

        ..Else_If D$eax = 'Flag'; From "Flags" string
            Call WriteCVGlobalProcFlagEquates
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                ;Mov B$edi "'" | inc edi
                Mov B$edi '"' | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             ;Mov B$edi "'" | inc edi
                Mov B$edi '"' | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

________________________

[CVGlobalProcPparent: B$ 'Pparent' EOS]
[CVGlobalProcPend: B$ 'Pend' EOS]
[CVGlobalProcPnext: B$ 'Pnext' EOS]
[CVGlobalProcLength: B$ 'ProcLength' EOS]
[CVGlobalProcDebugStart: B$ 'DebugStart' EOS]
[CVGlobalProcDebugEnd: B$ 'DebugEnd' EOS]
[CVGlobalProcOldCVType: B$ 'OldCVType' EOS] ; for obsolete versinos of CodeView (V 3 and earlier)
[CVGlobalProcOffset: B$ 'Offset' EOS]
[CVGlobalProcSegment: B$ 'Segment' EOS]
[CVGlobalProcProctype: B$ 'Proctype' EOS]
[CVGlobalProcFlags: B$ 'Flags' EOS]
[CVGlobalProcNamelenght: B$ 'NameLenght' EOS]
[CVGlobalProcName: B$ 'Name' EOS]

[CVGlobalProcLabel: B$ 0 # 20]

Proc WriteRawDataDebugSGlobalProc:
    Local @OldCodeView
    Uses ecx

    Mov D@OldCodeView &FALSE

    If W$esi-2 = &S_LPROC32
        Move D$CVGlobalProcLabel {B$ 'LocalProcedure.' EOS}
    Else
        Move D$CVGlobalProcLabel {B$ 'GlobalProcedure.' EOS}
    End_If

    If W$esi-2 = &S_GPROC32_CV3
        Mov D@OldCodeView &TRUE
    End_If

    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcPparent , {B$ ': D$ ' EOS}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcPend , {B$ ': D$ ' EOS}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcPnext , {B$ ': D$ ' EOS}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcLength , {B$ ': D$ ' EOS}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcDebugStart , {B$ ': D$ ' EOS}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcDebugEnd , {B$ ': D$ ' EOS}

    If D@OldCodeView = &TRUE
        Call WriteRawDataDebugSGlobalProcItem CVGlobalProcOldCVType , {B$ ': W$ ' EOS}
    End_If

    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcOffset , {B$ ': D$ ' EOS}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcSegment , {B$ ': W$ ' EOS}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcProctype , {B$ ': W$ ' EOS}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcFlags , {B$ ': B$ ' EOS}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcNamelenght , {B$ ': B$ ' EOS}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcName , {B$ ': B$ ' EOS}

EndP

__________________________________________________________________

Proc WriteRawDataDebugSObjectName:
;    Uses ecx
    Uses ecx, eax, ebx
    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx



    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'ObjName.Signature: D$ ' EOS}
    Pop esi

        lodsd | Call WriteEax
        Mov W$edi CRLF | add edi 2

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'ObjName.NameLenght: B$ ' EOS}
    Pop esi

        xor eax eax
        lodsb | Call WriteEax

        Mov W$edi CRLF | add edi 2

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ ".Index" EOS}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {B$ 'ObjName.Name: B$ ' EOS}
    Pop esi

        ; ecx points to the size of the Name
        movzx ecx B$esi-1
        Mov edx esi | add edx ecx

        .If B$esi = 0
            Mov B$edi '0' | inc edi | inc esi
        .Else

        Mov B$edi "'" | inc edi
L0:     lodsb
            If al = 0
                dec esi | jmp L1>
            End_If
        stosb | On esi < edx, jmp L0<
L1:     Mov B$edi "'" | inc edi

        .End_If

        While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        Mov W$edi CRLF | add edi 2

    Pop ebx

    ; Is the end of this structure ends on the proper place ?

    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If


EndP

__________________________________________________________________

[CVThunkStart1632Pparent: B$ 'Pparent' EOS]
[CVThunkStart1632Pend: B$ 'Pend' EOS]
[CVThunkStart1632Pnext: B$ 'Pnext' EOS]
[CVThunkStart1632Offset: B$ 'Offset' EOS]
[CVThunkStart1632Segment: B$ 'Segment' EOS]
[CVThunkStart1632ThunkLength: B$ 'ThunkLength' EOS]
[CVThunkStart1632Ordinal: B$ 'Ordinal' EOS]
[CVThunkStart1632Namelenght: B$ 'NameLenght' EOS]
[CVThunkStart1632Name: B$ 'Name' EOS]
[CVThunkStart1632VariantAdjustorDelta: B$ 'VariantAdjustor.Delta' EOS]
[CVThunkStart1632VariantAdjustorTargetFunctionNameLen: B$ 'VariantAdjustor.TargetFunctionNameLenght' EOS]
[CVThunkStart1632VariantAdjustorTargetFunctionName: B$ 'VariantAdjustor.TargetFunctionName' EOS]
[CVThunkStart1632VariantVCallVTableDisplacement: B$ 'VariantVCall.VTableDisplacement' EOS]
[CVThunkStart1632VariantPCodeSegment: B$ 'VariantPCode.Segment' EOS]
[CVThunkStart1632VariantPCodeOffset: B$ 'VariantPCode.Offset' EOS]

Proc WriteRawDataDebugSThunkStart1632:
    Local @OrdinalValue
    Uses ecx, eax

    Mov D@OrdinalValue 0

    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632Pparent , {B$ ': D$ ' EOS}
    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632Pend , {B$ ': D$ ' EOS}
    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632Pnext , {B$ ': D$ ' EOS}
    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632Offset , {B$ ': D$ ' EOS}
    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632Segment , {B$ ': W$ ' EOS}
    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632ThunkLength , {B$ ': W$ ' EOS}
    movzx eax B$esi
    Mov D@OrdinalValue eax
    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632Ordinal , {B$ ': B$ ' EOS}
    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632Namelenght , {B$ ': B$ ' EOS}
    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632Name , {B$ ': B$ ' EOS}

    ; Below are some members that i have no files to check, but accordying to the documentation, it seems correct.
    .If D@OrdinalValue = &CV4_THUNK32_ADJUSTOR
        Push edi | Push esi | Call 'USER32.MessageBoxA' 0, {B$ "CV4 - S_THUNK32 / Variant. Please, send this file to us to we check if the CV4 structure is correct" EOS}, {B$ "Attention !!!!" EOS}, &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL | Pop esi | Pop edi
        Call WriteRawDataDebugSThunkStart1632VariantItem CVThunkStart1632VariantAdjustorDelta, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSThunkStart1632VariantItem CVThunkStart1632VariantAdjustorTargetFunctionNameLen, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSThunkStart1632VariantItem CVThunkStart1632VariantAdjustorTargetFunctionName, {B$ ': B$ ' EOS}
    .Else_If D@OrdinalValue = &CV4_THUNK32_VCALL
        Push edi | Push esi | Call 'USER32.MessageBoxA' 0, {B$ "CV4 - S_THUNK32 / Variant. Please, send this file to us to we check if the CV4 structure is correct" EOS}, {B$ "Attention !!!!" EOS}, &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL | Pop esi | Pop edi
        Call WriteRawDataDebugSThunkStart1632VariantItem CVThunkStart1632VariantVCallVTableDisplacement, {B$ ': W$ ' EOS}
    .Else_If D@OrdinalValue = &CV4_THUNK32_PCODE
        Push edi | Push esi | Call 'USER32.MessageBoxA' 0, {B$ "CV4 - S_THUNK32 / Variant. Please, send this file to us to we check if the CV4 structure is correct" EOS}, {B$ "Attention !!!!" EOS}, &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL | Pop esi | Pop edi
        Call WriteRawDataDebugSThunkStart1632VariantItem CVThunkStart1632VariantPCodeSegment, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSThunkStart1632VariantItem CVThunkStart1632VariantPCodeOffset, {B$ ': D$ ' EOS}
    .End_If

EndP
__________________________________________________________________

Proc WriteRawDataDebugSThunkStart1632Item:
    Argument @Text1, @Text2
    uses eax, ecx

        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'ThunkStart1632.' EOS}
            zCopy D@Text1
            zCopy D@Text2
        Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Ppar' ; from "Pparent" string
            lodsd | Call WriteEax

        ..Else_If D$eax = 'Pend'; From "Pend" string
            lodsd | Call WriteEax

        ..Else_If D$eax = 'Pnex'; From "Pnext" string
            lodsd | Call WriteEax

        ..Else_If D$eax = 'Offs'; From "Offset" string
            lodsd | Call WriteEax

        ..Else_If D$eax = 'Segm'; From "Segment" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Thun'; From "ThunkLength" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Ordi'; From "Ordinal" string
            Call WriteCVThunkStart1632OrdinalEquates
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

__________________________________________________________________

Proc WriteRawDataDebugSThunkStart1632VariantItem:
    Argument @Text1, @Text2
    uses eax, ecx

        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'ThunkStart1632.' EOS}
            zCopy D@Text1
            zCopy D@Text2
        Pop esi

        Mov eax D@Text1


        ..If D$eax+16 = 'Delt' ; from "VariantAdjustor.Delta" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+34 = 'Leng'; From "VariantAdjustor.TargetFunctionNameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax+30 = 'Name'; From "VariantAdjustor.TargetFunctionName" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..Else_If D$eax+13 = 'VTab'; From "VariantVCall.VTableDisplacement" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+13 = 'Segm'; From "VariantPCode.Segment" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+13 = 'Offs'; From "VariantPCode.Offset" string
            lodsd | Call WriteEax

         ..End_If

        Mov W$edi CRLF | add edi 2

EndP

__________________________________________________________________

Proc WriteCVThunkStart1632OrdinalEquates:
    Uses esi;, ebx, eax

    .If B$esi = &CV4_THUNK32_NOTYPE
        zCopy {B$ '&CV4_THUNK32_NOTYPE' EOS}
    .Else_If B$esi = &CV4_THUNK32_ADJUSTOR
        zCopy {B$ '&CV4_THUNK32_ADJUSTOR' EOS}
    .Else_If B$esi = &CV4_THUNK32_VCALL
        zCopy {B$ '&CV4_THUNK32_VCALL' EOS}
    .Else_If B$esi = &CV4_THUNK32_PCODE
        zCopy {B$ '&CV4_THUNK32_PCODE' EOS}

    .End_If

        zCopy {B$ ' ; Hex Value:  ' EOS}

EndP
__________________________________________________________________
;;

;;
These are the primitives Types Listings. All values not used below are unknown or not specified.

Equates used:

Special Types

T_NOTYPE 0x0000 Uncharacterized type (no type)
T_ABS 0x0001 Absolute symbol
T_SEGMENT 0x0002 Segment type
T_VOID 0x0003 Void
T_PVOID 0x0103 Near pointer to void
T_PFVOID 0x0203 Far pointer to void
T_PHVOID 0x0303 Huge pointer to void
T_32PVOID 0x0403 32-bit near pointer to void
T_32PFVOID 0x0503 32-bit far pointer to void
T_CURRENCY 0x0004 Basic 8-byte currency value
T_NBASICSTR 0x0005 Near Basic string
T_FBASICSTR 0x0006 Far Basic string
T_NOTTRANS 0x0007 Untranslated type record from Microsoft symbol format
T_BIT 0x0060 Bit
T_PASCHAR 0x0061 Pascal CHAR

Character Types

T_CHAR 0x0010 8-bit signed
T_UCHAR 0x0020 8-bit unsigned
T_PCHAR 0x0110 Near pointer to 8-bit signed
T_PUCHAR 0x0120 Near pointer to 8-bit unsigned
T_PFCHAR 0x0210 Far pointer to 8-bit signed
T_PFUCHAR 0x0220 Far pointer to 8-bit unsigned
T_PHCHAR 0x0310 Huge pointer to 8-bit signed
T_PHUCHAR 0x0320 Huge pointer to 8-bit unsigned
T_32PCHAR 0x0410 16:32 near pointer to 8-bit signed
T_32PUCHAR 0x0420 16:32 near pointer to 8-bit unsigned
T_32PFCHAR 0x0510 16:32 far pointer to 8-bit signed
T_32PFUCHAR 0x0520 16:32 far pointer to 8-bit unsigned

Real Character Types

T_RCHAR 0x0070 Real char
T_PRCHAR 0x0170 Near pointer to a real char
T_PFRCHAR 0x0270 Far pointer to a real char
T_PHRCHAR 0x0370 Huge pointer to a real char
T_32PRCHAR 0x0470 16:32 near pointer to a real char
T_32PFRCHAR 0x0570 16:32 far pointer to a real char

Wide Character Types

T_WCHAR 0x0071 Wide char
T_PWCHAR 0x0171 Near pointer to a wide char
T_PFWCHAR 0x0271 Far pointer to a wide char
T_PHWCHAR 0x0371 Huge pointer to a wide char
T_32PWCHAR 0x0471 16:32 near pointer to a wide char
T_32PFWCHAR 0x0571 16:32 far pointer to a wide char

Real 16-bit Integer Types

T_INT2 0x0072 Real 16-bit signed int
T_UINT2 0x0073 Real 16-bit unsigned int
T_PINT2 0x0172 Near pointer to 16-bit signed int
T_PUINT2 0x0173 Near pointer to 16-bit unsigned int
T_PFINT2 0x0272 Far pointer to 16-bit signed int
T_PFUINT2 0x0273 Far pointer to 16-bit unsigned int
T_PHINT2 0x0372 Huge pointer to 16-bit signed int
T_PHUINT2 0x0373 Huge pointer to 16-bit unsigned int
T_32PINT2 0x0472 16:32 near pointer to 16-bit signed int
T_32PUINT2 0x0473 16:32 near pointer to 16-bit unsigned int
T_32PFINT2 0x0572 16:32 far pointer to 16-bit signed int
T_32PFUINT2 0x0573 16:32 far pointer to 16-bit unsigned int

16-bit Short Types

T_SHORT 0x0011 16-bit signed
T_USHORT 0x0021 16-bit unsigned
T_PSHORT 0x0111 Near pointer to 16-bit signed
T_PUSHORT 0x0121 Near pointer to 16-bit unsigned
T_PFSHORT 0x0211 Far pointer to 16-bit signed
T_PFUSHORT 0x0221 Far pointer to 16-bit unsigned
T_PHSHORT 0x0311 Huge pointer to 16-bit signed
T_PHUSHORT 0x0321 Huge pointer to 16-bit unsigned
T_32PSHORT 0x0411 16:32 near pointer to 16-bit signed
T_32PUSHORT 0x0421 16:32 near pointer to 16-bit unsigned
T_32PFSHORT 0x0511 16:32 far pointer to 16-bit signed
T_32PFUSHORT 0x0521 16:32 far pointer to 16-bit unsigned

Real 32-bit Integer Types

T_INT4 0x0074 Real 32-bit signed int
T_UINT4 0x0075 Real 32-bit unsigned int
T_PINT4 0x0174 Near pointer to 32-bit signed int
T_PUINT4 0x0175 Near pointer to 32-bit unsigned int
T_PFINT4 0x0274 Far pointer to 32-bit signed int
T_PFUINT4 0x0275 Far pointer to 32-bit unsigned int
T_PHINT4 0x0374 Huge pointer to 32-bit signed int
T_PHUINT4 0x0375 Huge pointer to 32-bit unsigned int
T_32PINT4 0x0474 16:32 near pointer to 32-bit signed int
T_32PUINT4 0x0475 16:32 near pointer to 32-bit unsigned int
T_32PFINT4 0x0574 16:32 far pointer to 32-bit signed int
T_32PFUINT4 0x0575 16:32 far pointer to 32-bit unsigned int

32-bit Long Types

T_LONG 0x0012 32-bit signed
T_ULONG 0x0022 32-bit unsigned
T_PLONG 0x0112 Near pointer to 32-bit signed
T_PULONG 0x0122 Near pointer to 32-bit unsigned
T_PFLONG 0x0212 Far pointer to 32-bit signed
T_PFULONG 0x0222 Far pointer to 32-bit unsigned
T_PHLONG 0x0312 Huge pointer to 32-bit signed
T_PHULONG 0x0322 Huge pointer to 32-bit unsigned
T_32PLONG 0x0412 16:32 near pointer to 32-bit signed
T_32PULONG 0x0422 16:32 near pointer to 32-bit unsigned
T_32PFLONG 0x0512 16:32 far pointer to 32-bit signed
T_32PFULONG 0x0522 16:32 far pointer to 32-bit unsigned

Real 64-bit int Types

T_INT8 0x0076 64-bit signed int
T_UINT8 0x0077 64-bit unsigned int
T_PINT8 0x0176 Near pointer to 64-bit signed int
T_PUINT8 0x0177 Near pointer to 64-bit unsigned int
T_PFINT8 0x0276 Far pointer to 64-bit signed int
T_PFUINT8 0x0277 Far pointer to 64-bit unsigned int
T_PHINT8 0x0376 Huge pointer to 64-bit signed int
T_PHUINT8 0x0377 Huge pointer to 64-bit unsigned int
T_32PINT8 0x0476 16:32 near pointer to 64-bit signed int
T_32PUINT8 0x0477 16:32 near pointer to 64-bit unsigned int
T_32PFINT8 0x0576 16:32 far pointer to 64-bit signed int
T_32PFUINT8 0x0577 16:32 far pointer to 64-bit unsigned int

64-bit Integral Types

T_QUAD 0x0013 64-bit signed
T_UQUAD 0x0023 64-bit unsigned
T_PQUAD 0x0113 Near pointer to 64-bit signed
T_PUQUAD 0x0123 Near pointer to 64-bit unsigned
T_PFQUAD 0x0213 Far pointer to 64-bit signed
T_PFUQUAD 0x0223 Far pointer to 64-bit unsigned
T_PHQUAD 0x0313 Huge pointer to 64-bit signed
T_PHUQUAD 0x0323 Huge pointer to 64-bit unsigned
T_32PQUAD 0x0413 16:32 near pointer to 64-bit signed
T_32PUQUAD 0x0423 16:32 near pointer to 64-bit unsigned
T_32PFQUAD 0x0513 16:32 far pointer to 64-bit signed
T_32PFUQUAD 0x0523 16:32 far pointer to 64-bit unsigned

32-bit Real Types

T_REAL32 0x0040 32-bit real
T_PREAL32 0x0140 Near pointer to 32-bit real
T_PFREAL32 0x0240 Far pointer to 32-bit real
T_PHREAL32 0x0340 Huge pointer to 32-bit real
T_32PREAL32 0x0440 16:32 near pointer to 32-bit real
T_32PFREAL32 0x0540 16:32 far pointer to 32-bit real

48-bit Real Types

T_REAL48 0x0044 48-bit real
T_PREAL48 0x0144 Near pointer to 48-bit real
T_PFREAL48 0x0244 Far pointer to 48-bit real
T_PHREAL48 0x0344 Huge pointer to 48-bit real
T_32PREAL48 0x0444 16:32 near pointer to 48-bit real
T_32PFREAL48 0x0544 16:32 far pointer to 48-bit real

64-bit Real Types

T_REAL64 0x0041 64-bit real
T_PREAL64 0x0141 Near pointer to 64-bit real
T_PFREAL64 0x0241 Far pointer to 64-bit real
T_PHREAL64 0x0341 Huge pointer to 64-bit real
T_32PREAL64 0x0441 16:32 near pointer to 64-bit real
T_32PFREAL64 0x0541 16:32 far pointer to 64-bit real

80-bit Real Types

T_REAL80 0x0042 80-bit real
T_PREAL80 0x0142 Near pointer to 80-bit real
T_PFREAL80 0x0242 Far pointer to 80-bit real
T_PHREAL80 0x0342 Huge pointer to 80-bit real
T_32PREAL80 0x0442 16:32 near pointer to 80-bit real
T_32PFREAL80 0x0542 16:32 far pointer to 80-bit real

128-bit Real Types
T_REAL128 0x0043 128-bit real
T_PREAL128 0x0143 Near pointer to 128-bit real
T_PFREAL128 0x0243 Far pointer to 128-bit real
T_PHREAL128 0x0343 Huge pointer to 128-bit real
T_32PREAL128 0x0443 16:32 near pointer to 128-bit real
T_32PFREAL128 0x0543 16:32 far pointer to 128-bit real

32-bit Complex Types

T_CPLX32 0x0050 32-bit complex
T_PCPLX32 0x0150 Near pointer to 32-bit complex
T_PFCPLX32 0x0250 Far pointer to 32-bit complex
T_PHCPLX32 0x0350 Huge pointer to 32-bit complex
T_32PCPLX32 0x0450 16:32 near pointer to 32-bit complex
T_32PFCPLX32 0x0550 16:32 far pointer to 32-bit complex

64-bit Complex Types

T_CPLX64 0x0051 64-bit complex
T_PCPLX64 0x0151 Near pointer to 64-bit complex
T_PFCPLX64 0x0251 Far pointer to 64-bit complex
T_PHCPLX64 0x0351 Huge pointer to 64-bit complex
T_32PCPLX64 0x0451 16:32 near pointer to 64-bit complex
T_32PFCPLX64 0x0551 16:32 far pointer to 64-bit complex

80-bit Complex Types

T_CPLX80 0x0052 80-bit complex
T_PCPLX80 0x0152 Near pointer to 80-bit complex
T_PFCPLX80 0x0252 Far pointer to 80-bit complex
T_PHCPLX80 0x0352 Huge pointer to 80-bit complex
T_32PCPLX80 0x0452 16:32 near pointer to 80-bit complex
T_32PFCPLX80 0x0552 16:32 far pointer to 80-bit complex

128-bit Complex Types

T_CPLX128 0x0053 128-bit complex
T_PCPLX128 0x0153 Near pointer to 128-bit complex
T_PFCPLX128 0x0253 Far pointer to 128-bit complex
T_PHCPLX128 0x0353 Huge pointer to 128-bit real
T_32PCPLX128 0x0453 16:32 near pointer to 128-bit complex
T_32PFCPLX128 0x0553 16:32 far pointer to 128-bit complex

Boolean Types

T_BOOL08 0x0030 8-bit Boolean
T_BOOL16 0x0031 16-bit Boolean
T_BOOL32 0x0032 32-bit Boolean
T_BOOL64 0x0033 64-bit Boolean
T_PBOOL08 0x0130 Near pointer to 8-bit Boolean
T_PBOOL16 0x0131 Near pointer to 16-bit Boolean
T_PBOOL32 0x0132 Near pointer to 32-bit Boolean
T_PBOOL64 0x0133 Near pointer to 64-bit Boolean
T_PFBOOL08 0x0230 Far pointer to 8-bit Boolean
T_PFBOOL16 0x0231 Far pointer to 16-bit Boolean
T_PFBOOL32 0x0232 Far pointer to 32-bit Boolean
T_PFBOOL32 0x0233 Far pointer to 64-bit Boolean
T_PHBOOL08 0x0330 Huge pointer to 8-bit Boolean
T_PHBOOL16 0x0331 Huge pointer to 16-bit Boolean
T_PHBOOL32 0x0332 Huge pointer to 32-bit Boolean
T_PHBOOL64 0x0333 Huge pointer to 64-bit Boolean
T_32PBOOL08 0x0430 16:32 near pointer to 8-bit Boolean
T_32PBOOL16 0x0431 16:32 near pointer to 16-bit Boolean
T_32PBOOL32 0x0432 16:32 near pointer to 32-bit Boolean
T_32PBOOL64 0x0433 16:32 near pointer to 64-bit Boolean
T_32PFBOOL08 0x0530 16:32 far pointer to 8-bit Boolean
T_32PFBOOL16 0x0531 16:32 far pointer to 16-bit Boolean
T_32PFBOOL32 0x0532 16:32 far pointer to 32-bit Boolean
T_32PFBOOL64 0x0533 16:32 far pointer to 64-bit Boolean
;;
;;
Proc WriteCVBPRel32TypeEquates:
    Uses esi

    ..If W$esi = &T_NOTYPE
        zCopy {B$ '&T_NOTYPE ; Uncharacterized type (no type) - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_ABS
        zCopy {B$ '&T_ABS ; Absolute symbol - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_SEGMENT
        zCopy {B$ '&T_SEGMENT ; Segment type - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_VOID
        zCopy {B$ '&T_VOID ; Void - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PVOID
        zCopy {B$ '&T_PVOID ; Near pointer to void - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFVOID
        zCopy {B$ '&T_PFVOID ; Far pointer to void - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHVOID
        zCopy {B$ '&T_PHVOID ; Huge pointer to void - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PVOID
        zCopy {B$ '&T_32PVOID ; 32-bit near pointer to void - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFVOID
        zCopy {B$ '&T_32PFVOID ; 32-bit far pointer to void - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_CURRENCY
        zCopy {B$ '&T_CURRENCY ; Basic 8-byte currency value - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_NBASICSTR
        zCopy {B$ '&T_NBASICSTR ; Near Basic string - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_FBASICSTR
        zCopy {B$ '&T_FBASICSTR ; Far Basic string - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_NOTTRANS
        zCopy {B$ '&T_NOTTRANS ; Untranslated type record from Microsoft symbol format - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_BIT
        zCopy {B$ '&T_BIT ; Bit - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PASCHAR
        zCopy {B$ '&T_PASCHAR ; Pascal CHAR - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_CHAR
        zCopy {B$ '&T_CHAR ; 8-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_UCHAR
        zCopy {B$ '&T_UCHAR ; 8-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PCHAR
        zCopy {B$ '&T_PCHAR ; Near pointer to 8-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PUCHAR
        zCopy {B$ '&T_PUCHAR ; Near pointer to 8-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFCHAR
        zCopy {B$ '&T_PFCHAR ; Far pointer to 8-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFUCHAR
        zCopy {B$ '&T_PFUCHAR ; Far pointer to 8-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHCHAR
        zCopy {B$ '&T_PHCHAR ; Huge pointer to 8-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHUCHAR
        zCopy {B$ '&T_PHUCHAR ; Huge pointer to 8-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PCHAR
        zCopy {B$ '&T_32PCHAR ; 16:32 near pointer to 8-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PUCHAR
        zCopy {B$ '&T_32PUCHAR ; 16:32 near pointer to 8-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFCHAR
        zCopy {B$ '&T_32PFCHAR ; 16:32 far pointer to 8-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFUCHAR
        zCopy {B$ '&T_32PFUCHAR ; 16:32 far pointer to 8-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_RCHAR
        zCopy {B$ '&T_RCHAR ; Real char - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PRCHAR
        zCopy {B$ '&T_PRCHAR ; Near pointer to a real char - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFRCHAR
        zCopy {B$ '&T_PFRCHAR ; Far pointer to a real char - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHRCHAR
        zCopy {B$ '&T_PHRCHAR ; Huge pointer to a real char - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PRCHAR
        zCopy {B$ '&T_32PRCHAR ; 16:32 near pointer to a real char - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFRCHAR
        zCopy {B$ '&T_32PFRCHAR ; 16:32 far pointer to a real char - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_WCHAR
        zCopy {B$ '&T_WCHAR ; Wide char - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PWCHAR
        zCopy {B$ '&T_PWCHAR ; Near pointer to a wide char - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFWCHAR
        zCopy {B$ '&T_PFWCHAR ; Far pointer to a wide char - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHWCHAR
        zCopy {B$ '&T_PHWCHAR ; Huge pointer to a wide char - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PWCHAR
        zCopy {B$ '&T_32PWCHAR ; 16:32 near pointer to a wide char - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFWCHAR
        zCopy {B$ '&T_32PFWCHAR ; 16:32 far pointer to a wide char - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_INT2
        zCopy {B$ '&T_INT2 ; Real 16-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_UINT2
        zCopy {B$ '&T_UINT2 ; Real 16-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PINT2
        zCopy {B$ '&T_PINT2 ; Near pointer to 16-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PUINT2
        zCopy {B$ '&T_PUINT2 ; Near pointer to 16-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFINT2
        zCopy {B$ '&T_PFINT2 ; Far pointer to 16-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFUINT2
        zCopy {B$ '&T_PFUINT2 ; Far pointer to 16-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHINT2
        zCopy {B$ '&T_PHINT2 ; Huge pointer to 16-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHUINT2
        zCopy {B$ '&T_PHUINT2 ; Huge pointer to 16-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PINT2
        zCopy {B$ '&T_32PINT2 ; 16:32 near pointer to 16-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PUINT2
        zCopy {B$ '&T_32PUINT2 ; 16:32 near pointer to 16-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFINT2
        zCopy {B$ '&T_32PFINT2 ; 16:32 far pointer to 16-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFUINT2
        zCopy {B$ '&T_32PFUINT2 ; 16:32 far pointer to 16-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_SHORT
        zCopy {B$ '&T_SHORT ; 16-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_USHORT
        zCopy {B$ '&T_USHORT ; 16-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PSHORT
        zCopy {B$ '&T_PSHORT ; Near pointer to 16-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PUSHORT
        zCopy {B$ '&T_PUSHORT ; Near pointer to 16-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFSHORT
        zCopy {B$ '&T_PFSHORT ; Far pointer to 16-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFUSHORT
        zCopy {B$ '&T_PFUSHORT ; Far pointer to 16-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHSHORT
        zCopy {B$ '&T_PHSHORT ; Huge pointer to 16-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHUSHORT
        zCopy {B$ '&T_PHUSHORT ; Huge pointer to 16-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PSHORT
        zCopy {B$ '&T_32PSHORT ; 16:32 near pointer to 16-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PUSHORT
        zCopy {B$ '&T_32PUSHORT ; 16:32 near pointer to 16-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFSHORT
        zCopy {B$ '&T_32PFSHORT ; 16:32 far pointer to 16-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFUSHORT
        zCopy {B$ '&T_32PFUSHORT ; 16:32 far pointer to 16-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_INT4
        zCopy {B$ '&T_INT4 ; Real 32-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_UINT4
        zCopy {B$ '&T_UINT4 ; Real 32-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PINT4
        zCopy {B$ '&T_PINT4 ; Near pointer to 32-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PUINT4
        zCopy {B$ '&T_PUINT4 ; Near pointer to 32-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFINT4
        zCopy {B$ '&T_PFINT4 ; Far pointer to 32-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFUINT4
        zCopy {B$ '&T_PFUINT4 ; Far pointer to 32-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHINT4
        zCopy {B$ '&T_PHINT4 ; Huge pointer to 32-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHUINT4
        zCopy {B$ '&T_PHUINT4 ; Huge pointer to 32-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PINT4
        zCopy {B$ '&T_32PINT4 ; 16:32 near pointer to 32-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PUINT4
        zCopy {B$ '&T_32PUINT4 ; 16:32 near pointer to 32-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFINT4
        zCopy {B$ '&T_32PFINT4 ; 16:32 far pointer to 32-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFUINT4
        zCopy {B$ '&T_32PFUINT4 ; 16:32 far pointer to 32-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_LONG
        zCopy {B$ '&T_LONG ; 32-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_ULONG
        zCopy {B$ '&T_ULONG ; 32-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PLONG
        zCopy {B$ '&T_PLONG ; Near pointer to 32-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PULONG
        zCopy {B$ '&T_PULONG ; Near pointer to 32-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFLONG
        zCopy {B$ '&T_PFLONG ; Far pointer to 32-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFULONG
        zCopy {B$ '&T_PFULONG ; Far pointer to 32-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHLONG
        zCopy {B$ '&T_PHLONG ; Huge pointer to 32-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHULONG
        zCopy {B$ '&T_PHULONG ; Huge pointer to 32-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PLONG
        zCopy {B$ '&T_32PLONG ; 16:32 near pointer to 32-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PULONG
        zCopy {B$ '&T_32PULONG ; 16:32 near pointer to 32-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFLONG
        zCopy {B$ '&T_32PFLONG ; 16:32 far pointer to 32-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFULONG
        zCopy {B$ '&T_32PFULONG ; 16:32 far pointer to 32-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_INT8
        zCopy {B$ '&T_INT8 ; 64-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_UINT8
        zCopy {B$ '&T_UINT8 ; 64-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PINT8
        zCopy {B$ '&T_PINT8 ; Near pointer to 64-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PUINT8
        zCopy {B$ '&T_PUINT8 ; Near pointer to 64-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFINT8
        zCopy {B$ '&T_PFINT8 ; Far pointer to 64-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFUINT8
        zCopy {B$ '&T_PFUINT8 ; Far pointer to 64-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHINT8
        zCopy {B$ '&T_PHINT8 ; Huge pointer to 64-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHUINT8
        zCopy {B$ '&T_PHUINT8 ; Huge pointer to 64-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PINT8
        zCopy {B$ '&T_32PINT8 ; 16:32 near pointer to 64-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PUINT8
        zCopy {B$ '&T_32PUINT8 ; 16:32 near pointer to 64-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFINT8
        zCopy {B$ '&T_32PFINT8 ; 16:32 far pointer to 64-bit signed int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFUINT8
        zCopy {B$ '&T_32PFUINT8 ; 16:32 far pointer to 64-bit unsigned int - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_QUAD
        zCopy {B$ '&T_QUAD ; 64-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_UQUAD
        zCopy {B$ '&T_UQUAD ; 64-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PQUAD
        zCopy {B$ '&T_PQUAD ; Near pointer to 64-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PUQUAD
        zCopy {B$ '&T_PUQUAD ; Near pointer to 64-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFQUAD
        zCopy {B$ '&T_PFQUAD ; Far pointer to 64-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFUQUAD
        zCopy {B$ '&T_PFUQUAD ; Far pointer to 64-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHQUAD
        zCopy {B$ '&T_PHQUAD ; Huge pointer to 64-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHUQUAD
        zCopy {B$ '&T_PHUQUAD ; Huge pointer to 64-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PQUAD
        zCopy {B$ '&T_32PQUAD ; 16:32 near pointer to 64-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PUQUAD
        zCopy {B$ '&T_32PUQUAD ; 16:32 near pointer to 64-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFQUAD
        zCopy {B$ '&T_32PFQUAD ; 16:32 far pointer to 64-bit signed - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFUQUAD
        zCopy {B$ '&T_32PFUQUAD ; 16:32 far pointer to 64-bit unsigned - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_REAL32
        zCopy {B$ '&T_REAL32 ; 32-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PREAL32
        zCopy {B$ '&T_PREAL32 ; Near pointer to 32-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFREAL32
        zCopy {B$ '&T_PFREAL32 ; Far pointer to 32-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHREAL32
        zCopy {B$ '&T_PHREAL32 ; Huge pointer to 32-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PREAL32
        zCopy {B$ '&T_32PREAL32 ; 16:32 near pointer to 32-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFREAL32
        zCopy {B$ '&T_32PFREAL32 ; 16:32 far pointer to 32-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_REAL48
        zCopy {B$ '&T_REAL48 ; 48-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PREAL48
        zCopy {B$ '&T_PREAL48 ; Near pointer to 48-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFREAL48
        zCopy {B$ '&T_PFREAL48 ; Far pointer to 48-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHREAL48
        zCopy {B$ '&T_PHREAL48 ; Huge pointer to 48-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PREAL48
        zCopy {B$ '&T_32PREAL48 ; 16:32 near pointer to 48-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFREAL48
        zCopy {B$ '&T_32PFREAL48 ; 16:32 far pointer to 48-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_REAL64
        zCopy {B$ '&T_REAL64 ; 64-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PREAL64
        zCopy {B$ '&T_PREAL64 ; Near pointer to 64-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFREAL64
        zCopy {B$ '&T_PFREAL64 ; Far pointer to 64-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHREAL64
        zCopy {B$ '&T_PHREAL64 ; Huge pointer to 64-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PREAL64
        zCopy {B$ '&T_32PREAL64 ; 16:32 near pointer to 64-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFREAL64
        zCopy {B$ '&T_32PFREAL64 ; 16:32 far pointer to 64-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_REAL80
        zCopy {B$ '&T_REAL80 ; 80-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PREAL80
        zCopy {B$ '&T_PREAL80 ; Near pointer to 80-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFREAL80
        zCopy {B$ '&T_PFREAL80 ; Far pointer to 80-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHREAL80
        zCopy {B$ '&T_PHREAL80 ; Huge pointer to 80-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PREAL80
        zCopy {B$ '&T_32PREAL80 ; 16:32 near pointer to 80-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFREAL80
        zCopy {B$ '&T_32PFREAL80 ; 16:32 far pointer to 80-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_REAL128
        zCopy {B$ '&T_REAL128 ; 128-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PREAL128
        zCopy {B$ '&T_PREAL128 ; Near pointer to 128-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFREAL128
        zCopy {B$ '&T_PFREAL128 ; Far pointer to 128-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHREAL128
        zCopy {B$ '&T_PHREAL128 ; Huge pointer to 128-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PREAL128
        zCopy {B$ '&T_32PREAL128 ; 16:32 near pointer to 128-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFREAL128
        zCopy {B$ '&T_32PFREAL128 ; 16:32 far pointer to 128-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_CPLX32
        zCopy {B$ '&T_CPLX32 ; 32-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PCPLX32
        zCopy {B$ '&T_PCPLX32 ; Near pointer to 32-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFCPLX32
        zCopy {B$ '&T_PFCPLX32 ; Far pointer to 32-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHCPLX32
        zCopy {B$ '&T_PHCPLX32 ; Huge pointer to 32-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PCPLX32
        zCopy {B$ '&T_32PCPLX32 ; 16:32 near pointer to 32-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFCPLX32
        zCopy {B$ '&T_32PFCPLX32 ; 16:32 far pointer to 32-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_CPLX64
        zCopy {B$ '&T_CPLX64 ; 64-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PCPLX64
        zCopy {B$ '&T_PCPLX64 ; Near pointer to 64-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFCPLX64
        zCopy {B$ '&T_PFCPLX64 ; Far pointer to 64-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHCPLX64
        zCopy {B$ '&T_PHCPLX64 ; Huge pointer to 64-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PCPLX64
        zCopy {B$ '&T_32PCPLX64 ; 16:32 near pointer to 64-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFCPLX64
        zCopy {B$ '&T_32PFCPLX64 ; 16:32 far pointer to 64-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_CPLX80
        zCopy {B$ '&T_CPLX80 ; 80-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PCPLX80
        zCopy {B$ '&T_PCPLX80 ; Near pointer to 80-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFCPLX80
        zCopy {B$ '&T_PFCPLX80 ; Far pointer to 80-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHCPLX80
        zCopy {B$ '&T_PHCPLX80 ; Huge pointer to 80-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PCPLX80
        zCopy {B$ '&T_32PCPLX80 ; 16:32 near pointer to 80-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFCPLX80
        zCopy {B$ '&T_32PFCPLX80 ; 16:32 far pointer to 80-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_CPLX128
        zCopy {B$ '&T_CPLX128 ; 128-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PCPLX128
        zCopy {B$ '&T_PCPLX128 ; Near pointer to 128-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFCPLX128
        zCopy {B$ '&T_PFCPLX128 ; Far pointer to 128-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHCPLX128
        zCopy {B$ '&T_PHCPLX128 ; Huge pointer to 128-bit real - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PCPLX128
        zCopy {B$ '&T_32PCPLX128 ; 16:32 near pointer to 128-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFCPLX128
        zCopy {B$ '&T_32PFCPLX128 ; 16:32 far pointer to 128-bit complex - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_BOOL08
        zCopy {B$ '&T_BOOL08 ; 8-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_BOOL16
        zCopy {B$ '&T_BOOL16 ; 16-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_BOOL32
        zCopy {B$ '&T_BOOL32 ; 32-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_BOOL64
        zCopy {B$ '&T_BOOL64 ; 64-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PBOOL08
        zCopy {B$ '&T_PBOOL08 ; Near pointer to 8-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PBOOL16
        zCopy {B$ '&T_PBOOL16 ; Near pointer to 16-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PBOOL32
        zCopy {B$ '&T_PBOOL32 ; Near pointer to 32-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PBOOL64
        zCopy {B$ '&T_PBOOL64 ; Near pointer to 64-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFBOOL08
        zCopy {B$ '&T_PFBOOL08 ; Far pointer to 8-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFBOOL16
        zCopy {B$ '&T_PFBOOL16 ; Far pointer to 16-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFBOOL32
        zCopy {B$ '&T_PFBOOL32 ; Far pointer to 32-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PFBOOL32
        zCopy {B$ '&T_PFBOOL32 ; Far pointer to 64-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHBOOL08
        zCopy {B$ '&T_PHBOOL08 ; Huge pointer to 8-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHBOOL16
        zCopy {B$ '&T_PHBOOL16 ; Huge pointer to 16-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHBOOL32
        zCopy {B$ '&T_PHBOOL32 ; Huge pointer to 32-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_PHBOOL64
        zCopy {B$ '&T_PHBOOL64 ; Huge pointer to 64-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PBOOL08
        zCopy {B$ '&T_32PBOOL08 ; 16:32 near pointer to 8-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PBOOL16
        zCopy {B$ '&T_32PBOOL16 ; 16:32 near pointer to 16-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PBOOL32
        zCopy {B$ '&T_32PBOOL32 ; 16:32 near pointer to 32-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PBOOL64
        zCopy {B$ '&T_32PBOOL64 ; 16:32 near pointer to 64-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFBOOL08
        zCopy {B$ '&T_32PFBOOL08 ; 16:32 far pointer to 8-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFBOOL16
        zCopy {B$ '&T_32PFBOOL16 ; 16:32 far pointer to 16-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFBOOL32
        zCopy {B$ '&T_32PFBOOL32 ; 16:32 far pointer to 32-bit Boolean - Hex Value:  ' EOS}
    ..Else_If W$esi = &T_32PFBOOL64
        zCopy {B$ '&T_32PFBOOL64 ; 16:32 far pointer to 64-bit Boolean - Hex Value:  ' EOS}
    ..End_If

EndP
_____________________________________
; Non Primitive Types, can be interpreted as the leaf types, for values bigger then 01000
Proc WriteCVNonPrimitiveTypeEquates:
    Uses esi

        ; For values bigger or equal to 01000, it is probably that they are related to leaf type indices, because
        ; they are related to Non-Primitive types

        .If W$esi = &LF_ENUMERATE_CV2
            zCopy {B$ '&LF_ENUMERATE_CV2; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_ARRAY_CV2
            zCopy {B$ '&LF_ARRAY_CV2; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_CLASS_CV2
            zCopy {B$ '&LF_CLASS_CV2; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_STRUCTURE_CV2
            zCopy {B$ '&LF_STRUCTURE_CV2; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_UNION_CV2
            zCopy {B$ '&LF_UNION_CV2; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_ENUM_CV2
            zCopy {B$ '&LF_ENUM_CV2; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_MEMBER_CV2
            zCopy {B$ '&LF_MEMBER_CV2; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_MODIFIER_CV3
            zCopy {B$ '&LF_MODIFIER_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_POINTER_CV3
            zCopy {B$ '&LF_POINTER_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_ARRAY_CV3
            zCopy {B$ '&LF_ARRAY_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_CLASS_CV3
            zCopy {B$ '&LF_CLASS_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_STRUCTURE_CV3
            zCopy {B$ '&LF_STRUCTURE_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_UNION_CV3
            zCopy {B$ '&LF_UNION_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_ENUM_CV3
            zCopy {B$ '&LF_ENUM_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_PROCEDURE_CV3
            zCopy {B$ '&LF_PROCEDURE_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_MFUNCTION_CV3
            zCopy {B$ '&LF_MFUNCTION_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_COBOL0_CV3
            zCopy {B$ '&LF_COBOL0_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_BARRAY_CV3
            zCopy {B$ '&LF_BARRAY_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_DIMARRAY_CV3
            zCopy {B$ '&LF_DIMARRAY_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_VFTPATH_CV3
            zCopy {B$ '&LF_VFTPATH_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_PRECOMP_CV3
            zCopy {B$ '&LF_PRECOMP_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_OEM_CV3
            zCopy {B$ '&LF_OEM_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_SKIP_CV3
            zCopy {B$ '&LF_SKIP_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_ARGLIST_CV3
            zCopy {B$ '&LF_ARGLIST_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_DEFARG_CV3
            zCopy {B$ '&LF_DEFARG_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_FIELDLIST_CV3
            zCopy {B$ '&LF_FIELDLIST_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_DERIVED_CV3
            zCopy {B$ '&LF_DERIVED_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_BITFIELD_CV3
            zCopy {B$ '&LF_BITFIELD_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_METHODLIST_CV3
            zCopy {B$ '&LF_METHODLIST_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_DIMCONU_CV3
            zCopy {B$ '&LF_DIMCONU_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_DIMCONLU_CV3
            zCopy {B$ '&LF_DIMCONLU_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_DIMVARU_CV3
            zCopy {B$ '&LF_DIMVARU_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_DIMVARLU_CV3
            zCopy {B$ '&LF_DIMVARLU_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_BCLASS_CV3
            zCopy {B$ '&LF_BCLASS_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_VBCLASS_CV3
            zCopy {B$ '&LF_VBCLASS_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_IVBCLASS_CV3
            zCopy {B$ '&LF_IVBCLASS_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_FRIENDFCN_CV3
            zCopy {B$ '&LF_FRIENDFCN_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_INDEX_CV3
            zCopy {B$ '&LF_INDEX_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_MEMBER_CV3
            zCopy {B$ '&LF_MEMBER_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_STMEMBER_CV3
            zCopy {B$ '&LF_STMEMBER_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_METHOD_CV3
            zCopy {B$ '&LF_METHOD_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_NESTTYPE_CV3
            zCopy {B$ '&LF_NESTTYPE_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_VFUNCTAB_CV3
            zCopy {B$ '&LF_VFUNCTAB_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_FRIENDCLS_CV3
            zCopy {B$ '&LF_FRIENDCLS_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_ONEMETHOD_CV3
            zCopy {B$ '&LF_ONEMETHOD_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_VFUNCOFF_CV3
            zCopy {B$ '&LF_VFUNCOFF_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_NESTTYPEEX_CV3
            zCopy {B$ '&LF_NESTTYPEEX_CV3; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_CHAR
            zCopy {B$ '&LF_CHAR; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_NUMERIC
            zCopy {B$ '&LF_NUMERIC; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_SHORT
            zCopy {B$ '&LF_SHORT; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_USHORT
            zCopy {B$ '&LF_USHORT; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_LONG
            zCopy {B$ '&LF_LONG; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_ULONG
            zCopy {B$ '&LF_ULONG; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_REAL32
            zCopy {B$ '&LF_REAL32; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_REAL64
            zCopy {B$ '&LF_REAL64; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_REAL80
            zCopy {B$ '&LF_REAL80; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_REAL128
            zCopy {B$ '&LF_REAL128; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_QUADWORD
            zCopy {B$ '&LF_QUADWORD; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_UQUADWORD
            zCopy {B$ '&LF_UQUADWORD; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_REAL48
            zCopy {B$ '&LF_REAL48; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_COMPLEX32
            zCopy {B$ '&LF_COMPLEX32; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_COMPLEX64
            zCopy {B$ '&LF_COMPLEX64; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_COMPLEX80
            zCopy {B$ '&LF_COMPLEX80; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_COMPLEX128
            zCopy {B$ '&LF_COMPLEX128; - Hex Value:  ' EOS}
        .Else_If W$esi = &LF_VARSTRING
            zCopy {B$ '&LF_VARSTRING; - Hex Value:  ' EOS}
        .Else_If W$esi = &CV_FIRST_NONPRIM
            zCopy {B$ '&CV_FIRST_NONPRIM; - Hex Value:  ' EOS}
        .End_If

EndP

[MemberSize2: D$ ?]

Proc WriteCVBPRel32OffsetComment:
    Arguments @dwValue
    Uses esi, eax, ebx, edx, ecx

    Mov eax D@dwValue

    If eax = 0
        zCopy {B$ " ; This symbol cannot be evaluated because its location is unknown." EOS}
    End_If

    jns @sign
        Push eax
            zCopy {B$ " ; This is a Local variable: ebp-" EOS}
            neg eax
            Call WriteEax
        Pop eax
            zCopy {B$ " , ebp+" EOS}
            Call WriteEax

EndP

@sign:

        ..If eax < 8
            zCopy {B$ " ; Incorrect Symbol Allocation. This can be a Data Pointer, and not a Local Var or a Argument for this function." EOS}
        ..Else
            Mov ecx eax
            sub eax 8

            ..If eax = 0
                zCopy {B$ " ; This is a Parameter: ebp+08. Argument = 1. RosAsm Style: Arg1" EOS}
            ..Else

                ; divide eax by 4 to calculate how many parameters we have, and if it is a multiple of 4 or not.
                Mov edx 0, ebx 4
                div ebx

                If edx = 0

                    inc eax ; Increment eax to display the proper amount of Arguments.

                    Push eax
                        zCopy {B$ " ; This is a Parameter: ebp+" EOS}
                        Mov eax ecx | Call WriteEax
                    Pop eax

                    zCopy {B$ ". Argument = " EOS}

                    Push esi
                    Push edi
                    Push eax
                        ; Calculate and Display Size (Convert Hexa do Decimal String)
                        Mov D$membersize2 eax
                        Mov esi membersize2, ecx 4
                        Call toUDword
                        Mov esi edi, edi DecimalBuffer
                        Do | movsb | LoopUntil B$esi-1 = 0
                    Pop eax
                    Pop edi
                    Pop esi

                    zCopy DecimalBuffer
                    zCopy {B$ ". RosAsm Style: Arg" EOS}
                    zCopy DecimalBuffer
                Else
                    zCopy {B$ " ; Incorrect Symbol Allocation. This can be a Data Pointer, and not a Local Var or a Argument for this function." EOS}
                End_If

            ..End_If

        ..End_If

EndP
_____________________________________

Proc WriteRawDataDebugSBPRel32Item:
    Argument @Text1, @Text2
    uses eax, ecx

        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {B$ 'BPRel32.' EOS}
            zCopy D@Text1
            zCopy D@Text2
        Pop esi

        Mov eax D@Text1

        ..If D$eax = 'Offs' ; from "Offset" string
            lodsd | Call WriteEax
            Call WriteCVBPRel32OffsetComment D$esi-4

        ..Else_If D$eax = 'OldC'; From "OldCVType" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Type'; From "Type" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP

________________

[CVBPRel32Offset: B$ 'Offset' EOS]
[CVBPRel32OldCVType: B$ 'OldCVType' EOS]
[CVBPRel32Type: B$ 'Type' EOS]
[CVBPRel32Namelenght: B$ 'NameLenght' EOS]
[CVBPRel32Name: B$ 'Name' EOS]


Proc WriteRawDataDebugSBPRel32:
    Local @OldCodeView
    Uses ecx

    Mov D@OldCodeView &FALSE
    If W$esi-2 = &S_BPREL32_CV3
        Mov D@OldCodeView &TRUE
    End_If


    Call WriteRawDataDebugSBPRel32Item CVBPRel32Offset , {B$ ': D$ ' EOS}
    If D@OldCodeView = &TRUE
        Call WriteRawDataDebugSBPRel32Item CVBPRel32OldCVType , {B$ ': W$ ' EOS}
    End_If
    Call WriteRawDataDebugSBPRel32Item CVBPRel32Type , {B$ ': W$ ' EOS}
    Call WriteRawDataDebugSBPRel32Item CVBPRel32Namelenght , {B$ ': B$ ' EOS}
    Call WriteRawDataDebugSBPRel32Item CVBPRel32Name , {B$ ': B$ ' EOS}

EndP

__________________________________________________________________

[CVLocalData32OldCVType: B$ 'OldCVType' EOS]
[CVLocalData32Offset: B$ 'Offset' EOS]
[CVLocalData32Segment: B$ 'Segment' EOS]
[CVLocalData32Type: B$ 'Type' EOS]
[CVLocalData32Namelenght: B$ 'NameLenght' EOS]
[CVLocalData32Name: B$ 'Name' EOS]

[CVLocalData32Label: B$ 0 # 20]

Proc WriteRawDataDebugSLData32:
    Local @OldCodeView
;    Uses ecx
    Uses ecx, eax, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx

    Push ebx

    If W$esi-2 = &S_LDATA32
        Move D$CVLocalData32Label {B$ 'LocalData32.' EOS}
    Else_If_Or W$esi-2 = &S_GDATA32, W$esi-2 = &S_GDATA_CV3
        Move D$CVLocalData32Label {B$ 'GlobalData32.' EOS}
    Else
        Move D$CVLocalData32Label {B$ 'PublicData32.' EOS}
    End_If

    If W$esi-2 = &S_GDATA_CV3
        Mov D@OldCodeView &TRUE
    End_If

    If D@OldCodeView = &TRUE
        Call WriteRawDataDebugSLData32Item CVLocalData32OldCVType , {B$ ': D$ ' EOS}
    End_If

    Call WriteRawDataDebugSLData32Item CVLocalData32Offset , {B$ ': D$ ' EOS}
    Call WriteRawDataDebugSLData32Item CVLocalData32Segment , {B$ ': W$ ' EOS}
    Call WriteRawDataDebugSLData32Item CVLocalData32Type , {B$ ': W$ ' EOS}
    Call WriteRawDataDebugSLData32Item CVLocalData32Namelenght , {B$ ': B$ ' EOS}
    Call WriteRawDataDebugSLData32Item CVLocalData32Name , {B$ ': B$ ' EOS}

    Pop ebx

    ; Is the end of this structure ends on the proper place ?

    .If esi <> ebx
        Mov W$edi CRLF | add edi 2
        Call WriteCVdataTitle
        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If


EndP

______________________________________

Proc WriteRawDataDebugSLData32Item:
    Argument @Text1, @Text2
    uses eax, ecx

        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ ".Index" EOS}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy D$CVLocalData32Label
            zCopy D@Text1
            zCopy D@Text2
        Pop esi

        Mov eax D@Text1


        ..If D$eax = 'OldC'; From "OldCVType" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Offs'; From "Offset" string
            lodsd | Call WriteEax

        ..Else_If D$eax = 'Segm'; From "Segment" string
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax = 'Type'; From "Type" string
            Call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | Call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | Call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            Mov edx esi | add edx ecx

            .If B$esi = 0
                Mov B$edi '0' | inc edi | inc esi
            .Else

                Mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             Mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        Mov W$edi CRLF | add edi 2

EndP


__________________________________________________________________


[CVDataLenght: B$ 'Length' EOS]
[CVDataIndex: B$ 'Index' EOS]

[SizeAdd: D$ 0]
[CVLabel: B$ 0 # 6]
[CV5Label: B$ '.CV5.' EOS] ; found in some .NET Object Files
[CV4Label: B$ '.CV4.' EOS]
[CV3Label: B$ '.CV3.' EOS]
[CV2Label: B$ '.CV2.' EOS] ; Unknown CV Signature

Proc WriteRawDataDebugS:
    Local @CVSize, @RawDataEnd
    Uses ecx, esi, eax

    Mov ecx esi
    add ecx D$CoffSectionSize
    Mov D@RawDataEnd ecx

    ; If Signature is not 1. Do a byte chain. Signature = 1 is Code or CodeView 4 ?. Other values are data or CodeView older versions ?

    .If D$esi = 1
        Move D$CVLabel CV4Label
        jmp L0>>
    .Else_If D$esi = 2
        Move D$CVLabel CV3Label
        jmp L0>>
    .Else_If D$esi = 4
        sub edi 4
        Push esi | ZCopy {D$ CRLF2, B$ '; This Codeview format is unespecified. It is common for Dot NET files. ' D$ CRLF2 B$ '    B$: B$ ' EOS} | Pop esi
        Mov ecx D$CoffSectionSize
        Call WriteRawDataDataSection

EndP

    .Else    ;   Sometimes they do not contain signatures like in: clusapi2.obj
        Move D$CVLabel CV2Label
        Mov D$SizeAdd 0
        Call InitSectionDebugNumber
        Move D@CVSize D$CoffSectionSize
        sub edi 4
        add D@CVSize esi ; CVSize points to the beginning of the Debug Raw Data
        Mov W$edi CRLF | add edi 2
        Mov D$edi '    ' | add edi 4
        Push esi | zCopy {B$ '; This Debug Section have no signature.' EOS} | Pop esi
        Mov W$edi CRLF | add edi 2
        jmp L1>>

    .End_If

L0:

    Mov D$SizeAdd 0

    Call InitSectionDebugNumber

    Move D@CVSize D$CoffSectionSize
    sub edi 4

    Mov D$edi CRLF2 | add edi 4

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy D$CVLabel
        zCopy {B$ 'Signature: D$ ' EOS}
    Pop esi


        add D@CVSize esi ; CVSize points to the beginning of the Debug Raw Data

        lodsd | Call WriteEax

        If D$esi-4 = 1
            Push esi | ZCopy {B$ " ; The compiler that made this Object File, emitted OMF (Object Module Format) types on it's symbols and Types below" EOS} | Pop esi
        End_If

        Mov W$edi CRLF | add edi 2

   L1:

    .While esi <> D@RawDataEnd

        movzx eax W$esi
        Mov D$SizeAdd eax

        ...If_Or W$esi = 0, W$esi+2 = 0 ; If lenght = 0 , or index = 0 do next line
            add esi 2 ; only increment esi by 2
        ...Else

            .If_Or W$esi+2 = &S_GPROC32, W$esi+2 = &S_LPROC32, W$esi+2 = &S_THUNK32, W$esi+2 = &S_GPROC32_CV3
                Push esi | ZCopy {D$ CRLF2, B$ '; ---------------------- Procedure Start  ---------------------- ', W$ CRLF, 0} | Pop esi
            .End_If

            Call WriteRawDataDebugSItem CVDataLenght, {B$ ': W$ ' EOS}
            Call WriteRawDataDebugSItem CVDataIndex, {B$ ': W$ ' EOS}

            ..If_Or W$esi-2 = &S_COMPILE, W$esi-2 = &S_COMPILE_CV3, W$esi-2 = &S_COMPILE_CV2
                Call WriteRawDataDebugSCompile

            ..Else_If W$esi-2 = &S_MSTOOL_CV2 ; found in acos.obj This is a variation of S_COMPILE found in older versions of codeview
                Call WriteRawDataDebugSMsTool

            ..Else_If_Or W$esi-2 = &S_REGISTER, W$esi-2 = &S_REGISTER_CV3; Register variable
                Call WriteRawDataDebugSRegister

            ..Else_If_Or W$esi-2 = &S_CONSTANT, W$esi-2 = &S_CONSTANT_CV3, W$esi-2 = &S_CONSTANT_CV2 ; Constant symbol (frmMainNET.obj)
                Call WriteRawDataDebugSContant

            ..Else_If_Or W$esi-2 = &S_UDT, W$esi-2 = &S_UDT_CV3, W$esi-2 = &S_UDT_CV2; User-defined Type (frmMainNET.obj)
                Call WriteRawDataDebugSUDT

            ..Else_If W$esi-2 = &S_SSEARCH ; Start search
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_END ; End block, procedure, with, or thunk
                Push esi | ZCopy {W$ CRLF, B$ '; ---------------------- Procedure End  ---------------------- ', D$ CRLF2, 0} | Pop esi

            ..Else_If W$esi-2 = &S_SKIP ; Skip - Reserve symbol space
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_CVRESERVE ; Reserved for internal use by the Microsoft debugger
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_OBJNAME ; Specify name of object file
                Call WriteRawDataDebugSObjectName

            ..Else_If W$esi-2 = &S_ENDARG ; Specify end of arguments in function symbols
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_COBOLUDT ; Microfocus COBOL user-defined type
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_MANYREG ; Many register symbol
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_RETURN ; Function return description
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_ENTRYTHIS ; Description of this pointer at entry
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_BPREL16 ; BP relative 16:16
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_LDATA16 ; Local data 16:16
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_GDATA16 ; Global data 16:16
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_PUB16 ; Public symbol 16:16
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_LPROC16 ; Local procedure start 16:16
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_GPROC16 ; Global procedure start 16:16
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_THUNK16 ; Thunk start 16:16
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_BLOCK16 ; Block start 16:16
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_WITH16 ; With start 16:16
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_LABEL16 ; Code label 16:16
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_CEXMODEL16 ; Change execution model 16:16
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_VFTPATH16 ; Virtual function table path descriptor 16:16
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_REGREL16 ; Specify 16:16 offset relative to arbitrary register
                Mov eax eax
                jmp L0>>

            ..Else_If_Or W$esi-2 = &S_BPREL32, W$esi-2 = &S_BPREL32_CV3 ; BP relative 16:32
                Call WriteRawDataDebugSBPRel32
            ..Else_If W$esi-2 = &S_LDATA32 ; Local data 16:32
                Call WriteRawDataDebugSLData32

            ..Else_If_Or W$esi-2 = &S_GDATA32, W$esi-2 = &S_GDATA_CV3 ; Global data 16:32
                Call WriteRawDataDebugSLData32

            ..Else_If W$esi-2 = &S_PUB32 ; Public symbol 16:32
                Call WriteRawDataDebugSLData32

            ..Else_If W$esi-2 = &S_LPROC32 ; Local (Static) procedure start 16:32
                Call WriteRawDataDebugSGlobalProc
            ..Else_If_Or W$esi-2 = &S_GPROC32, W$esi-2 = &S_GPROC32_CV3 ; Global procedure start 16:32
                Call WriteRawDataDebugSGlobalProc
            ..Else_If W$esi-2 = &S_THUNK32 ; Thunk start 16:32
                Call WriteRawDataDebugSThunkStart1632

            ..Else_If W$esi-2 = &S_BLOCK32 ; Block start 16:32
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_VFTPATH32 ; Virtual function table path descriptor 16:32
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_REGREL32 ; 16:32 offset relative to arbitrary register
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_LTHREAD32 ; Local Thread Storage data
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_GTHREAD32 ; Global Thread Storage data
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_LPROCMIPS ; Local procedure start MIPS
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_GPROCMIPS ; Global procedure start MIPS
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_PROCREF ; Reference to a procedure
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_DATAREF ; Reference to data
                Mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_ALIGN ; Page align symbols
                Mov eax eax
                jmp L0>>

            ..Else_If_Or W$esi-2 >= 01000;, W$esi-2 >= 0
L0:
                Push esi
                    Mov D$edi '    ' | add edi 4
                    Call WriteObjIndice
                    zCopy {B$ "Sec" EOS}
                    zCopy SectionHeaderNumber
                    zCopy {B$ ".Index" EOS}
                    zCopy DebugNumber
                    zCopy D$CVLabel
                    zCopy {B$ 'Unknown' EOS}
                Pop esi
                Call WriteRawDataDebugSUnknown

        ..End_If

    ...End_If

        Call IncrementSectionDebugNumber
        Mov eax D@CVSize
        Mov W$edi CRLF | add edi 2

;    .Loop_Until esi >= eax
    .End_While
EndP
__________________________________________________________________

Proc WriteRawDataDebugSUnknown:
    Uses eax, ecx


    ; This is to avoid that the Structure have dummy bytes at the end.
    Mov ebx esi
    sub ebx 2
    Mov ecx D$SizeAdd
    add ebx ecx
;;
;;
    Push ebx

                    Push esi
                        Mov D$edi '    ' | add edi 4
                        Call WriteObjIndice
                        zCopy {B$ "Sec" EOS}
                        zCopy SectionHeaderNumber
                        zCopy {B$ ".Index" EOS}
                        zCopy DebugNumber
                        zCopy D$CVLabel
                        zCopy {B$ 'Constant.LeafType.Unknown: B$ ' EOS}
                    Pop esi
    Pop ebx
;;
;;

    Push ebx
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayElemType , {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayIdxType , {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayLength, {B$ ': W$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayNamelenght, {B$ ': B$ ' EOS}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayName, {B$ ': B$ ' EOS}
        sub edi 2
    Pop ebx
;;
;;
    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
;;
;;
        Mov W$edi CRLF | add edi 2
        ;Call WriteCVdataTitle
                    Push esi
                        Mov D$edi '    ' | add edi 4
                        Call WriteObjIndice
                        zCopy {B$ "Sec" EOS}
                        zCopy SectionHeaderNumber
                        zCopy {B$ ".Index" EOS}
                        zCopy DebugNumber
                        zCopy D$CVLabel
                        zCopy {B$ 'Constant.LeafType.Unknown: B$ ' EOS}
                    Pop esi

;;
;;
        Push esi | ZCopy {B$ '.Unknown: B$ ' EOS} | Pop esi

        Push ecx

        Mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            Push ebx | Call WriteEax | Pop ebx

            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            Mov W$edi+1 CRLF | add edi 2
            Pop ecx
            inc edi
    .Else
        Mov W$edi CRLF | add edi 2
    .End_If

EndP

__________________________________________________________________



Proc WriteRawDataDebugT:
    Local @CVSize, @RawDataEnd
    Uses ecx, esi, eax

    Mov ecx esi
    add ecx D$CoffSectionSize
    Mov D@RawDataEnd ecx

    ; If Signature is not 1. Do a byte chain. Signature = 1 is Code or CodeView 4 ?. Other values are data or CodeView older versions ?
    If D$esi = 1
        Move D$CVLabel CV4Label
        jmp L0>
    Else_If D$esi = 2
        Move D$CVLabel CV3Label
        jmp L0>
    Else_If D$esi = 4
        Move D$CVLabel CV5Label ; found in NET files. This is really really painfull.
        jmp L0>
    Else
        Call WriteRawDataDataSection

EndP

    End_If

L0:

    Mov D$SizeAdd 0

    Call InitSectionDebugNumber

    Move D@CVSize D$CoffSectionSize
    sub edi 4

    Mov D$edi CRLF2 | add edi 4

    Push esi
        Mov D$edi '    ' | add edi 4
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy D$CVLabel
        zCopy {B$ 'Signature: D$ ' EOS}
    Pop esi

        add D@CVSize esi ; CVSize points to the beginning of the Debug Raw Data

        lodsd | Call WriteEax
        If D$esi-4 = 1
            Push esi | ZCopy {B$ " ; The compiler that made this Object File, emitted OMF (Object Module Format) types on it's symbols and Types below" EOS} | Pop esi
        End_If

        Mov W$edi CRLF | add edi 2

        ; sometimes we may have a file with only the Signature Byte. (Size of raw data is 4 in VBAEXE6.LIB)
        On D@RawDataEnd = esi EndP


    .Do
        movzx eax W$esi
        Mov D$SizeAdd eax

        ...If W$esi = 0 ; If lenght = 0 do next line
            add esi 2 ; only increment esi by 2

        ...Else

            Push eax

            Push esi
                Mov D$edi '    ' | add edi 4
                Call WriteObjIndice

                zCopy {B$ "Sec" EOS}
                zCopy SectionHeaderNumber
                zCopy {B$ ".Index" EOS}
                zCopy DebugNumber
                zCopy D$CVLabel
                zCopy {B$ "Length: W$ " EOS}
            Pop esi

                xor eax eax
                lodsw | Call WriteEax

            Mov W$edi CRLF | add edi 2
            Pop eax

            Call WriteRawDataDebugSContantLeafTypeItem

    ...End_If

        Call IncrementSectionDebugNumber
        Mov eax D@CVSize
        Mov W$edi CRLF | add edi 2

    .Loop_Until esi >= eax

EndP

__________________________________________________________________

Proc WriteRawDataDebugFItem:
    Argument @Text1, @Text2
    uses eax

        Push esi
            Mov D$edi '    ' | add edi 4
            Call WriteObjIndice

            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber

            zCopy {B$ '.FPOData' EOS}
            Mov B$edi '.' | inc edi
            zCopy D@Text1
            zCopy D@Text2
        Pop esi

                Mov eax D@Text1

                If D$eax = 'ulOf' ; from "ulOffStart" string
                    lodsd | Call WriteEax
                    ;Call WriteSectionHeaderRawSizeDiffLabel
                    ;Push esi | zcopy {B$ " ; Hex Value:  " EOS} | Pop esi

                Else_If D$eax+2 = 'Proc'; From "cbProcSize" string
                    lodsd | Call WriteEax
                    ;Call WriteSectionHeaderPointerToDataDiffLabel
                    ;Push esi | zcopy {B$ " ; Hex Value:  " EOS} | Pop esi

                Else_If D$eax = 'cdwL'; From "cdwLocals" string
                    lodsd | Call WriteEax
                    ;Call InitSectionRelocNumber
                    ;Call WriteSectionHeaderPointerToRelocDiffLabel
                    ;Push esi | zcopy {B$ " ; Hex Value:  " EOS} | Pop esi

                Else_If D$eax = 'cdwP'; From "cdwParams" string
                    xor eax eax
                    lodsw | Call WriteEax
                    ;Call InitSectionLineNumber
                    ;Call WriteSectionHeaderPointerToLineNumberDiffLabel
                    ;Push esi | zcopy {B$ " ; Hex Value:  " EOS} | Pop esi

                Else_If D$eax+2 = 'Prol'; From "cbProlog" string
                    xor eax eax
                    lodsb | Call WriteEax
                    ;Call InitSectionLineNumber
                    ;Call WriteSectionHeaderPointerToLineNumberDiffLabel
                    ;Push esi | zcopy {B$ " ; Hex Value:  " EOS} | Pop esi

                Else_If D$eax = 'Func'; From "FunctionSpec" string
                    xor eax eax
                    lodsb | Call WriteEax
                    ;Call InitSectionLineNumber
                    ;Call WriteSectionHeaderPointerToLineNumberDiffLabel
                    ;Push esi | zcopy {B$ " ; Hex Value:  " EOS} | Pop esi

                End_If

        Mov W$edi CRLF | add edi 2
EndP

;;
;;
 similar to WriteImageSymbolTable and GetCoffSectionHeader
 Uses FPO_DATA Structure. The FPO_DATA in the structures.str is wrong, the correct is:

[FPOData:
 FPOData.ulOffStart: D$ 0
 FPOData.cbProcSize: D$ 0
 FPOData.cdwLocals: D$ 0
 FPOData.cdwParams: W$ 0
 FPOData.cbProlog: B$ 0
 FPOData.FunctionSpec: B$ 0]
;;
;;
[RawDataulOffStart: 'ulOffStart' EOS]
[RawDatacbProcSize: B$ 'cbProcSize' EOS]
[RawDatacdwLocals: B$ 'cdwLocals' EOS]
[RawDatacdwParams: B$ 'cdwParams' EOS]
[RawDatacbProlog: B$ 'cbProlog' EOS]
[RawDataFunctionSpec: B$ 'FunctionSpec' EOS]


WriteRawDataDebugF:

    sub edi 4

    Mov D$edi CRLF2 | add edi 4
    Call WriteRawDataDebugFItem RawDataulOffStart, {B$ ': D$ ' EOS}
    Call WriteRawDataDebugFItem RawDatacbProcSize, {B$ ': D$ ' EOS}
    Call WriteRawDataDebugFItem RawDatacdwLocals, {B$ ': D$ ' EOS}
    Call WriteRawDataDebugFItem RawDatacdwParams, {B$ ': W$ ' EOS}
    Call WriteRawDataDebugFItem RawDatacbProlog, {B$ ': B$ ' EOS}
    Call WriteRawDataDebugFItem RawDataFunctionSpec, {B$ ': B$ ' EOS}

ret
__________________________________________________________________

WriteRawDataOnlyStringsSection:

    ; Initialize the String Record Counter

    Call InitStringTableRecord
    Push esi
        Call WriteObjIndice
        zCopy {B$ "Sec" EOS}
        zCopy SectionHeaderNumber
        zCopy {B$ '.StringData.Signature' EOS}
        Mov D$edi ': B$', B$edi+4 SPC | add edi 5
    Pop esi
        While B$esi <> 0 | movsb | End_While
    xor eax eax
    lodsb | Call WriteEax

    Mov W$edi CRLF | add edi 2

    Mov edx D$CoffSectionSize
    dec edx
    add edx, esi ; We will use edx as a counter delimiter for the strings

   .Do

        Push esi
            Call WriteObjIndice
            zCopy {B$ "Sec" EOS}
            zCopy SectionHeaderNumber
            zCopy {B$ '.StringData.Arr' EOS}
            zCopy StringTableRecord
            zCopy {B$ ": B$ '" EOS}
        Pop esi


        While B$esi <> 0 | movsb | End_While | inc esi | On B$esi = 0, Mov edx 0

        Mov D$edi "', 0" | add edi 4
        Mov W$edi CRLF | add edi 2

        Call IncrementStringTableRecord

    .Loop_Until esi >= edx

    sub edi 2
    Mov W$edi CRLF | add edi 2
ret

__________________________________________________________________

WriteRawDataDataSection:

L0:         movzx eax B$esi | Call WriteEax
            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If
            loop L0<

            sub edi 2
ret
__________________________________________________________________

WriteRawDataCodeSection:

L0:         movzx eax B$esi | Call WriteEax
            Mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                Mov W$edi CRLF | add edi 2 | Mov edx 0
                Mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If
            loop L0<

            sub edi 2

ret
__________________________________________________________________

WriteRawDataLinkerDirective:

    Mov edx esi | add edx ecx

    .If B$esi = 0
        Mov B$edi '0' | inc edi | inc esi
    .Else

        Mov B$edi "'" | inc edi
L0:     lodsb
        If al = 0
            dec esi | jmp L1>
        End_If
        stosb | On esi < edx, jmp L0<
L1:     Mov B$edi "'" | inc edi

    .End_If

    While esi < edx | lodsb | Mov D$edi ', 0' | add edi 3 | End_While

ret
__________________________________________________________________

WriteRawdataEnd:

    Mov W$edi CRLF | add edi 2
    Call WriteObjIndice
    zCopy {B$ "Sec" EOS}
    zCopy SectionHeaderNumber
    zCopy {B$ ".RawDataEnd" EOS}
    zCopy {B$ ":]" D$ CRLF2} ; EOS ?
ret
__________________________________________________________________

Proc WriteRawdataTitle:
    Uses esi, ecx

    zCopy CoffRawDataSectionTitle
    Call WriteObjIndice
    zCopy {B$ "ImgSecHdr" EOS}
    zCopy SectionHeaderNumber
    zCopy {B$ ".PointerToRawData", W$ CRLF, 0}

    Mov W$edi CRLF, B$edi+2 '[' | add edi 3
    Call WriteObjIndice
    zCopy {B$ "Sec" EOS}
    zCopy SectionHeaderNumber
    zCopy {B$ '.RawData' EOS}
    Mov D$edi ': B$', B$edi+4 SPC | add edi 5

EndP
__________________________________________________________________

[CoffRawDataInfoTitle: B$ "_________________________________________________________

; Raw Data Info
_________________________________________________________
" EOS]

[CoffRawDataSectionTitle: "
; These are the Raw Data Values referenced at: " EOS]

Proc WriteCoffSectionData:
    Uses esi, eax, ecx, edx


    ...If D$CoffSectionSize = 0

    ...Else_If D$CoffPointerToData = 0 ; Is the Pointer to Raw Data = 0 ? Yes, exit

    ...Else

        Push esi | zCopy CoffRawDataInfoTitle | Pop esi
        ; esi points to the Begin of the Raw data section, ecx points to the size of the raw data
        Mov esi D$CoffSectionBase | add esi D$CoffPointerToData
        Mov ecx D$CoffSectionSize, edx 0

        ..If D$RawDataType = RDT_LNKDIRECTIVE
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    Linker Directive Reports:", D$ CRLF2, 0} | Pop esi
                Call WriteRawDataLinkerDirectiveReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle

            Call WriteRawDataLinkerDirective

        ..Else_If D$RawDataType = RDT_CODE
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    This Raw Data is related only to CODE", D$ CRLF2, 0} | Pop esi
                ; Call WriteRawDataCodeSectionReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle

            Call WriteRawDataCodeSection

        ..Else_If D$RawDataType = RDT_VIRTUALDATA
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    This Raw Data is related only to Virtual Data", D$ CRLF2, 0} | Pop esi
                ; Call WriteRawDataDataSectionReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle

            Call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_DEBUGS
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    This Raw Data is related only to Debug$S", D$ CRLF2, 0} | Pop esi
                ; Call WriteRawDataDataSectionReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle

            ;Call WriteRawDataDebugS
            Call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_DEBUGF
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    This Raw Data is related only to Debug$F - Frame Point Omission", D$ CRLF2, 0} | Pop esi
            ; Call WriteRawDataDebugFReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle

            Call WriteRawDataDebugF

        ..Else_If D$RawDataType = RDT_DEBUGT
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    This Raw Data is related only to Debug$T - Debug Type", D$ CRLF2, 0} | Pop esi
            ; Call WriteRawDataDataSectionReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle

            Call WriteRawDataDataSection
            ;Call WriteRawDataDebugT

        ..Else_If D$RawDataType = RDT_DEBUGP
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    This Raw Data is related only to Debug$P", D$ CRLF2, 0} | Pop esi
            ; Call WriteRawDataDataSectionReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle
            Call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_STABSTR
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    This Raw Data is related only to NetFramework strings section - stabstr", D$ CRLF2, 0} | Pop esi
            ; Call WriteRawDataDataSectionReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle
            sub edi 4
            Mov W$edi CRLF | add edi 2
            Call WriteRawDataOnlyStringsSection

        ..Else_If D$RawDataType = RDT_IDATA
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    This Raw Data is related only to Import Table Data - idata", D$ CRLF2, 0} | Pop esi
            ; Call WriteRawDataDataSectionReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle

            Call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_RDATA
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    This Raw Data is related only to Read-Only Data - rdata", D$ CRLF2, 0} | Pop esi
            ; Call WriteRawDataDataSectionReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle

            Call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_XDATA
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    This Raw Data is related only to Exception information - xdata", D$ CRLF2, 0} | Pop esi
            ; Call WriteRawDataDataSectionReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle

            Call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_PDATA
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    This Raw Data is related only to Exception information - pdata", D$ CRLF2, 0} | Pop esi
            ; Call WriteRawDataDataSectionReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle

            Call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_EDATA
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    This Raw Data is related only to Export tables - edata", D$ CRLF2, 0} | Pop esi
            ; Call WriteRawDataDataSectionReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle

            Call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_TLSDATA
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    This Raw Data is related only to Thread-local storage - tls", D$ CRLF2, 0} | Pop esi
            ; Call WriteRawDataDataSectionReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle

            Call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_RELOCDATA
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    This Raw Data is related only to Image relocations - reloc", D$ CRLF2, 0} | Pop esi
            ; Call WriteRawDataDataSectionReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle

            Call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_RSRCDATA
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    This Raw Data is related only to resources - rsrc", D$ CRLF2, 0} | Pop esi
            ; Call WriteRawDataDataSectionReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle

            Call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_ERROR
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    There was an error on the debug or the data section. These raw values will be assumed as DATA", D$ CRLF2, 0} | Pop esi
            ; Call WriteRawDataDataSectionReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle

            Call WriteRawDataDataSection

        ..Else
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Push esi | zCopy {B$ "    This Raw Data is related only to DATA", D$ CRLF2, 0} | Pop esi
            ; Call WriteRawDataDataSectionReport
            Mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            Call WriteRawdataTitle

            Call WriteRawDataDataSection

        ..End_If

        Call WriteRawdataEnd

    ...End_If

EndP


;;
;;
  Values set in 'FirstSectionPointer', 'NumberOfRelocations', 'CoffPointerToReloc'
  by 'GetCoffSectionsVariables'.
;;
;;
WriteCoffReloc:

    Call InitSectionRelocNumber

    Push esi
        Mov esi D$CoffSectionBase | add esi D$CoffPointerToReloc
        Mov ecx D$NumberOfRelocations

L0:     Push ecx
            Call GetCoffRelocHeader
            Call IncrementSectionRelocNumber
        Pop ecx

        loop L0<
    Pop esi
ret



WriteCoffLineNumber:
    Call InitSectionLineNumber
    Push esi
        Mov esi D$CoffSectionBase | add esi D$CoffPointerToLineNumber
        Mov ecx D$NumberOfLineNumbers

L0:     Push ecx
            Call GetCoffLineHeader
            Call IncrementSectionLineNumber
        Pop ecx

        loop L0<
    Pop esi
ret


____________________________________________________________________________________________




[ImageFileEquate: B$ '&IMAGE_FILE_' EOS]
[IMAGE_FILE_16BIT_MACHINE: B$ '16BIT_MACHINE' EOS]
[IMAGE_FILE_32BIT_MACHINE: B$ '32BIT_MACHINE__' EOS]
[IMAGE_FILE_AGGRESSIVE_WS_TRIM: B$ 'AGGRESSIVE_WS_TRIM' EOS]
[IMAGE_FILE_BYTES_REVERSED_HI: B$ 'BYTES_REVERSED_HI__' EOS]
[IMAGE_FILE_BYTES_REVERSED_LO: B$ 'BYTES_REVERSED_LO__' EOS]
[IMAGE_FILE_DEBUG_STRIPPED: B$ 'DEBUG_STRIPPED__' EOS]
[IMAGE_FILE_DLL: B$ 'DLL__' EOS]
[IMAGE_FILE_EXECUTABLE_IMAGE: B$ 'EXECUTABLE_IMAGE__' EOS]
[IMAGE_FILE_LARGE_ADDRESS_AWARE: B$ 'LARGE_ADDRESS_AWARE__' EOS]
[IMAGE_FILE_LINE_NUMS_STRIPPED: B$ 'LINE_NUMS_STRIPPED__' EOS]
[IMAGE_FILE_LOCAL_SYMS_STRIPPED: B$ 'LOCAL_SYMS_STRIPPED__' EOS]
[IMAGE_FILE_NET_RUN_FROM_SWAP: B$ 'NET_RUN_FROM_SWAP__' EOS]
[IMAGE_FILE_RELOCS_STRIPPED: B$ 'RELOCS_STRIPPED__' EOS]
[IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP: B$ 'REMOVABLE_RUN_FROM_SWAP__' EOS]
[IMAGE_FILE_SYSTEM: B$ 'SYSTEM__' EOS]
[IMAGE_FILE_UP_SYSTEM_ONLY: B$ 'UP_SYSTEM_ONLY__' EOS]


WriteObjCharacteristics:
    Push esi

    Mov D$edi ' ; ' | add edi 3

    test eax &IMAGE_FILE_32BIT_MACHINE | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_32BIT_MACHINE

L1: test eax &IMAGE_FILE_16BIT_MACHINE | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_16BIT_MACHINE

L1: test eax &IMAGE_FILE_BYTES_REVERSED_HI | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_BYTES_REVERSED_HI

L1: test eax &IMAGE_FILE_AGGRESSIVE_WS_TRIM | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_AGGRESSIVE_WS_TRIM

L1: test eax &IMAGE_FILE_BYTES_REVERSED_LO | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_BYTES_REVERSED_LO

L1: test eax &IMAGE_FILE_DEBUG_STRIPPED | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_DEBUG_STRIPPED

L1: test eax &IMAGE_FILE_DLL | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_DLL

L1: test eax &IMAGE_FILE_EXECUTABLE_IMAGE | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_EXECUTABLE_IMAGE

L1: test eax &IMAGE_FILE_LARGE_ADDRESS_AWARE | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_LARGE_ADDRESS_AWARE

L1: test eax &IMAGE_FILE_LINE_NUMS_STRIPPED | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_LINE_NUMS_STRIPPED

L1: test eax &IMAGE_FILE_LOCAL_SYMS_STRIPPED | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_LOCAL_SYMS_STRIPPED

L1: test eax &IMAGE_FILE_NET_RUN_FROM_SWAP | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_NET_RUN_FROM_SWAP

L1: test eax &IMAGE_FILE_RELOCS_STRIPPED | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_RELOCS_STRIPPED

L1: test eax &IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP

L1: test eax &IMAGE_FILE_SYSTEM | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_SYSTEM

L1: test eax &IMAGE_FILE_UP_SYSTEM_ONLY | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_UP_SYSTEM_ONLY

L1: Pop esi

    While B$edi-1 = '_' | dec edi | End_While
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
;;
  Data, Equates used more than once
;;
;;

[COFF_HEADER_SIZE 03C    ARCH_TAG 8
 LIB_USER_ID 01C         LIB_MEMBER_SIZE 030]

[IMAGE_ARCHIVE_MEMBER_HEADERstring: B$ ' Structure: IMAGE_ARCHIVE_MEMBER_HEADER' EOS]
____________________________________________________________________________________________

;;
;;
  This is called multiple times
;;
;;

[MemberSize: D$ ?]

GetMemberSize:
    Push esi
        Mov ecx 10, ebx 0, eax 0
        While B$esi > SPC
            mul ecx
            Push eax
                lodsb | sub al '0' | Mov bl al
            Pop eax
            add eax ebx
        End_While
        Mov D$MemberSize eax
    Pop esi
ret

______________________________

[MemberDate: D$ ?]

GetMemberDate:
    Push esi
        Mov ecx 10, ebx 0, eax 0
        While B$esi > SPC
            mul ecx
            Push eax
                lodsb | sub al '0' | Mov bl al
            Pop eax
            add eax ebx
        End_While
        Mov D$MemberDate eax
    Pop esi
ret

______________________________

Proc WriteNamesLinkerMemberX:
    Argument @Name, @n, @LinkerMember
    Local @Item

        Push esi
            Mov esi D@LinkerMember
            While B$esi <> 0 | movsb | End_While

            Mov B$edi '.' | inc edi
            Mov esi D@Name

            Move D@Item D$esi

            If D$esi = 'Size'
                Pop esi | Push esi
                Call GetMemberSize
                Mov esi D@Name

            Else_If D$esi = 'Date'
                Pop esi | Push esi
                Call GetMemberDate
                Mov esi D@Name
            End_If

            While B$esi <> 0 | movsb | End_While
        Pop esi

        Mov D$edi ': B$' | add edi 4

        .If D@n <> 0-2

            Mov W$edi " '" | add edi 2
            Mov ecx D@n

            If D@Item = 'User'
                Mov D$edi '****', W$edi+4 '**' | add edi 6 | sub ecx 6 | add esi 6
            End_If

            rep movsb

            While B$edi-1 = 0 | dec edi | End_While

            Mov B$edi "'" | inc edi
            Mov W$edi CRLF | add edi 2

        .Else
            Mov B$edi SPC | inc edi
            Mov ecx D@n | neg ecx
L0:         lodsb | and eax 0FF | Call WriteEax | Mov W$edi ', ' | add edi 2 | loop L0<
            sub edi 2

        .End_If
EndP

____________________________________________________________________________________________

[LibObjIndice: B$ '000001' EOS]

InitCoffIndice:
    Mov D$LibObjIndice '0000', D$LibObjIndice+4 '01'
ret

IncrementLibCoffIndice:
    lea ebx D$LibObjIndice+5 | inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret

WriteCoffIndice:
    Push esi
        Mov D$edi 'Obj_' | add edi 4
        Mov eax D$LibObjIndice, D$edi eax
        Mov ax W$LibObjIndice+4, W$edi+4 ax
        Mov D$edi+6 ': ; ' | add edi 10
    Pop esi
ret


_________________________________________________________

[szDateString: B$ ? # 64]
[szTimeString: B$ ? # 64]

Proc WriteLinkerMemberTimeDateStamp:
    Arguments @TimeDate
    Uses esi

        ; Write the Value of the Date in Hexadecimal string
        Mov esi {B$ ' ; Hex Value:  ' EOS}
        sub edi 2 ; We need to subtract 02 Bytes, to bypass the CRLF
        While B$esi <> 0 | movsb | End_While
        Mov eax D@TimeDate
        Call Writeeax

        Mov esi {B$ ' - TimeDate Stamp: ' EOS}
        While B$esi <> 0 | movsb | End_While

      ; Get the DateTime Stamp

      ; Time date stamp to string Function
        Call TimeDateStampToString D@TimeDate, {B$ "yyyy/MM/dd ddd " EOS},
                                   {B$ "HH:mm:ss UTC" EOS}, szDateString, szTimeString

      ; Write the TimeDate Stamp strings
        Mov esi szDateString
        While B$esi <> 0 | movsb | End_While

        Mov esi szTimeString
        While B$esi <> 0 | movsb | End_While

      ; We now Add the paragraphs marks, and add 02 Bytes at edi:
        Mov W$edi CRLF | add edi 2


EndP

_________________________________________________________

Proc WriteLinkerMemberSizeHex:
    Arguments @SizeValue
    Uses esi
    ; Write the Value of the Size in Hexadecimal string

        Mov esi {B$ ' ; Hex Value:  ' EOS}
        sub edi 2 ; We need to subtract 02 Bytes, to bypass the CRLF
        While B$esi <> 0 | movsb | End_While
        Mov eax D@SizeValue
        Call Writeeax
        Mov W$edi CRLF | add edi 2 ; We now Add the paragraphs marks, and add 02 Bytes at edi

EndP
_________________________________________________________


[NamesLinkerMember: B$ 'NamesLinkerMember'
 ARCHIVE_MEMBER_HEADER_Indice: B$ '000001' EOS]


[NamesLinkerMember1: B$ 'NamesLinkerMember_1' EOS]

Write_IMAGE_ARCHIVE_MEMBER_HEADER_1:
    Mov B$edi '[' | inc edi

    Call WriteNamesLinkerMemberX {B$ 'Name1' EOS}, 16, NamesLinkerMember1
    Call WriteNamesLinkerMemberX {B$ 'Date' EOS}, 12, NamesLinkerMember1
    Call WriteLinkerMemberTimeDateStamp D$MemberDate
    Call WriteNamesLinkerMemberX {B$ 'UserID' EOS}, 6, NamesLinkerMember1
    Call WriteNamesLinkerMemberX {B$ 'GroupID' EOS}, 6, NamesLinkerMember1
    Call WriteNamesLinkerMemberX {B$ 'Mode' EOS}, 8, NamesLinkerMember1
    Call WriteNamesLinkerMemberX {B$ 'Size1' EOS}, 10, NamesLinkerMember1
    Call WriteLinkerMemberSizeHex D$MemberSize
    Call WriteNamesLinkerMemberX {B$ 'EndHeader' EOS}, 0-2, NamesLinkerMember1

    Mov B$edi ']', D$edi+1 CRLF2 | add edi 5

    lea ebx D$ARCHIVE_MEMBER_HEADER_Indice+5 | inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret


[NamesLinkerMember2: B$ 'NamesLinkerMember_2' EOS]

Write_IMAGE_ARCHIVE_MEMBER_HEADER_2:
    Mov B$edi '[' | inc edi

    Call WriteNamesLinkerMemberX {B$ 'Name1' EOS}, 16, NamesLinkerMember2
    Call WriteNamesLinkerMemberX {B$ 'Date' EOS}, 12, NamesLinkerMember2
    Call WriteLinkerMemberTimeDateStamp D$MemberDate
    Call WriteNamesLinkerMemberX {B$ 'UserID' EOS}, 6, NamesLinkerMember2
    Call WriteNamesLinkerMemberX {B$ 'GroupID' EOS}, 6, NamesLinkerMember2
    Call WriteNamesLinkerMemberX {B$ 'Mode' EOS}, 8, NamesLinkerMember2
    Call WriteNamesLinkerMemberX {B$ 'Size1' EOS}, 10, NamesLinkerMember2
    Call WriteLinkerMemberSizeHex D$MemberSize
    Call WriteNamesLinkerMemberX {B$ 'EndHeader' EOS}, 0-2, NamesLinkerMember2

    Mov B$edi ']', D$edi+1 CRLF2 | add edi 5

    lea ebx D$ARCHIVE_MEMBER_HEADER_Indice+3 | inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret


[NamesLinkerMember3: B$ 'NamesLinkerMember_3' EOS]

Write_IMAGE_ARCHIVE_MEMBER_HEADER_3:
    Mov B$edi '[' | inc edi

    Call WriteNamesLinkerMemberX {B$ 'Name1' EOS}, 16, NamesLinkerMember3
    Call WriteNamesLinkerMemberX {B$ 'Date' EOS}, 12, NamesLinkerMember3
    Call WriteLinkerMemberTimeDateStamp D$MemberDate
    Call WriteNamesLinkerMemberX {B$ 'UserID' EOS}, 6, NamesLinkerMember3
    Call WriteNamesLinkerMemberX {B$ 'GroupID' EOS}, 6, NamesLinkerMember3
    Call WriteNamesLinkerMemberX {B$ 'Mode' EOS}, 8, NamesLinkerMember3
    Call WriteNamesLinkerMemberX {B$ 'Size1' EOS}, 10, NamesLinkerMember3
    Call WriteLinkerMemberSizeHex D$MemberSize
    Call WriteNamesLinkerMemberX {B$ 'EndHeader' EOS}, 0-2, NamesLinkerMember3

    Mov B$edi ']', D$edi+1 CRLF2 | add edi 5

    lea ebx D$ARCHIVE_MEMBER_HEADER_Indice+5 | inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret

[CoffTitle: B$ "

_____________________________________________________________________________________________
_____________________________________________________________________________________________

; The Coff File Format Starts here. Indice: " EOS]

WriteCoffTitle:
    Push esi
        Mov esi CoffTitle | While B$esi <> 0 | movsb | End_While
        Mov eax D$LibObjIndice, D$edi eax,
            ax W$LibObjIndice+4, W$edi+4 ax,
            D$edi+6 CRLF2 | add edi 10
    Pop esi
ret


[Obj_LIB_USER_ID: B$ 'Obj_' CopyOf_LIB_USER_ID: B$ '000001' EOS]

Proc Write_Obj_IMAGE_ARCHIVE_MEMBER_HEADER:
    Local @StartAddress

    Mov B$edi '[' | inc edi

    ...If B$esi = '/'
      ; First or Second Header: nop
        ..If B$esi+1 <> SPC
          ; Case of Obj with LongNames Table in the Third Header:
            Call WriteCoffIndice
            Push esi
                inc esi
                If D$LongNamesBase <> 0
                    Call CheckLongNameDisplacement | On eax = &FALSE, jmp L1>
                    ; to be used in AddListviewItem
                    Mov D$StartObjNameAddress edi
                    Call WriteLongName esi
                Else

   L1:
                    ; to be used in AddListviewItem
                    Mov D$StartObjNameAddress edi
                    Call CopyObjSymbolName

                End_If
                Mov esi IMAGE_ARCHIVE_MEMBER_HEADERstring
                While B$esi <> 0 | movsb | End_While
                Mov W$edi CRLF | add edi 2
            Pop esi
        ..Else
            ; There seem to exist case of Functions called by Number, here
            Mov D$StartObjNameAddress edi

            Call WriteCoffIndice
            While B$esi <> 0 | movsb | End_While
            Push esi
                Mov esi IMAGE_ARCHIVE_MEMBER_HEADERstring
                While B$esi <> 0 | movsb | End_While
                Mov W$edi CRLF | add edi 2
            Pop esi

        ..End_If

    ...Else
      ; Case of Obj without LongNames Table (No Third Header) Ex.: UUID.LIB ; WLDAP32.LIB:
        Call WriteCoffIndice

        Mov D$StartObjNameAddress edi
        Push esi

            Call CopyObjSymbolName
            Mov esi IMAGE_ARCHIVE_MEMBER_HEADERstring
            While B$esi <> 0 | movsb | End_While
            Mov W$edi CRLF | add edi 2
        Pop esi

    ...End_If

    Call ParseLibObjectSymbolName D$StartObjNameAddress

    Mov eax D$esi+LIB_USER_ID, D$CopyOf_LIB_USER_ID eax,
        ax W$esi+LIB_USER_ID+4, W$CopyOf_LIB_USER_ID+4 ax

    Call WriteNamesLinkerMemberX {B$ 'Name1' EOS}, 16, Obj_LIB_USER_ID
    Call WriteNamesLinkerMemberX {B$ 'Date' EOS}, 12, Obj_LIB_USER_ID
    Call WriteLinkerMemberTimeDateStamp D$MemberDate
    Call WriteNamesLinkerMemberX {B$ 'UserID' EOS}, 6, Obj_LIB_USER_ID
    Call WriteNamesLinkerMemberX {B$ 'GroupID' EOS}, 6, Obj_LIB_USER_ID
    Call WriteNamesLinkerMemberX {B$ 'Mode' EOS}, 8, Obj_LIB_USER_ID
    Call WriteNamesLinkerMemberX {B$ 'Size1' EOS}, 10, Obj_LIB_USER_ID
    Call WriteLinkerMemberSizeHex D$MemberSize
    Call WriteNamesLinkerMemberX {B$ 'EndHeader' EOS}, 0-2, Obj_LIB_USER_ID

    Mov B$edi ']', D$edi+1 CRLF2 | add edi 5

    lea ebx D$ARCHIVE_MEMBER_HEADER_Indice+5 | inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx | inc B$ebx
    End_While

EndP
____________________________________________________________________________________________


; This function is to be used for displaying the object file name in the list view

; Built this way to solve really weird file nameings paths like:

;;
;;
[Obj_000009: ; Long Name: wizard\wizard.obj Structure: IMAGE_ARCHIVE_MEMBER_HEADER
[Obj_000001: ; Short Name: poide.exe Structure: IMAGE_ARCHIVE_MEMBER_HEADER
[Obj_000004: ; Short Name: poide.exe Function Name: _WizAddProjectFileA@4 Structure: IMAGE_ARCHIVE_MEMBER_HEADER
[Obj_000050: ; Long Name: unwind-dw2-fde.o Structure: IMAGE_ARCHIVE_MEMBER_HEADER
[Obj_000235 ; Short Name: $UWD\except.obj Structure: IMAGE_ARCHIVE_MEMBER_HEADER
[Obj_000235 ; Long Name: $UWD\validadd.obj Structure: IMAGE_ARCHIVE_MEMBER_HEADER
[Obj_000135 ; Long Name: ../XXX\nanonano.obj Structure: IMAGE_ARCHIVE_MEMBER_HEADER
[Obj_000134 ; Long Name: ./.XXX/nanonano.obj Structure: IMAGE_ARCHIVE_MEMBER_HEADER
;;
;;

[ObjectName: B$ ? # 256]
[StartObjNameAddress: D$ ?]
[EndObjNameAddress: D$ ?]
[ObjExtensionStr: B$ ? # 16]
[UseObjExtension: D$ ?]
[ExportedLibrary: D$ ?]
[ExportedLibraryStr: B$ ? # 256]
[UseObjPath: D$ ?]
[ObjPathStr: B$ ? # 256]

Proc ParseLibObjectSymbolName:
    Arguments @StartAddress
    Local @TempEndAddr, @TempStartAddr, @StartExportObjAddr, @EndObjPathAddr, @StartObjExtensionAddr, @UseExportedLibrary

    pushad

    ; Always clear the loaded data before use.
    Call ClearDwordBuffer ObjectName,
                          (256/DWORD)
    
    Call ClearDwordBuffer ObjExtensionStr,
                          (16/DWORD)
    
    Call ClearDwordBuffer ObjPathStr,
                          (256/DWORD)

    ; Initialize all the data
    Mov D$ObjFileNameType 0
    Mov D$UseObjPath 0
    Mov D$UseObjExtension 0
    Mov D@TempStartAddr 0
    Mov D@TempEndAddr 0
    Mov D@StartExportObjAddr 0
    Mov D@UseExportedLibrary 0

    Mov esi D@StartAddress

    ; 1St Step. Locate the start and the end of the String, and also get the FileNameType and the check Exported Library
    Mov ecx 0 ; we will use ecx as a counter.
              ; The maximum size allowed for this is 784 bytes (Max path + Max Object Name + Max exported Lib string + Max extension)
    .While ecx <> (256*3+16)

        .If_And D$esi = 'Long', D$esi+4 = ' Nam' ; is it a Long Name string type ?

            ; to be used in AddListviewItem
            Mov D$ObjFileNameType 1
            add D@StartAddress 11 ; (String len) "Long Name: "

        .Else_If_And D$esi = 'Shor', D$esi+4 = 't Na' ; is it a Short Name string type ?

            ; to be used in AddListviewItem
            Mov D$ObjFileNameType 0
            add D@StartAddress 12 ; (String len) "Short Name: "

        .Else_If_And D$esi = ' Fun', D$esi+4 = 'ctio', D$esi+8 = 'n Na', D$esi+12 = 'me: '

            ; to be used in AddListviewItem
            Mov D@StartExportObjAddr esi
            Mov D@UseExportedLibrary 1

        .Else_If_And D$esi = ' Str', D$esi+4 = 'uctu', D$esi+8 = 're: ' ; Did we reached the end of the string ?
                                                                        ; Our string always ends with " Structure: "
            Mov D@TempEndAddr esi
            jmp L1>

        .End_If
        inc ecx
        inc esi
    .End_While

   L1:


    ; 2nd Step. Now that we have the start and ending address, we must check if we are dealing with a non-common format.
    ; These formats always have the String " Function Name: " on it. Copy it to the proper buffer (ExportedLibraryStr)

    .If D@UseExportedLibrary = 1
        Mov esi D@StartExportObjAddr
        Move D@TempEndAddr D@StartExportObjAddr
    .End_If


    ; 3rd Step. Now that we have our full string, we need to see if it have any path inside.
    ; We must start from the end to the beginning of the string to find the path chars.
    Mov esi D@StartAddress
    Mov ecx D@TempEndAddr

    .While esi <> ecx

        If_Or B$ecx = '/', B$ecx = '\'

            Mov D@EndObjPathAddr ecx
            Mov D$UseObjPath 1
            jmp L1>
        End_If

        dec ecx
    .End_While

   L1:

    ; 4th Step. If we have a Path, we will copy it to the proper Buffer (ObjPathStr)

    .If D$UseObjPath = 1
        ; Here esi is already defined as the starting point. We don't need to change it now.
        Mov ecx D@EndObjPathAddr
        Mov edi ObjPathStr

        .While esi <> ecx
            movsb
            ;inc esi
        .End_While
        inc esi ; Bypass the last "/" or "\" char preceeding the object name.
        Mov D@StartAddress esi ; Will points to the beginning of the object name only

    .End_If

    ; 5th Step. At this point we have only the name of the object and it's extension (if any).
    ; So we must find and copy the object extension.

    Mov esi D@StartAddress
    Mov ecx D@TempEndAddr
    Mov edi ObjectName

    On esi = ecx, jmp L2> ; If the end and starting address is teh same ones, it is an empty string

    .Do

        .If B$esi = '.'
            Mov D$UseObjExtension 1
            Mov edi ObjExtensionStr
            inc esi ; Bypass the "." char
            While esi <> ecx
                movsb
            End_While
            jmp L2>
        .End_If

        movsb
    .Loop_Until esi = ecx

   L2:

    popad
EndP

____________________________________________________________________________________________


ParseLibObj:
    Push esi
        ...If W$esi = &IMAGE_FILE_MACHINE_UNKNOWN
            ..If W$esi+2 = &IMPORT_OBJECT_HDR_SIG2
              ; +12 is the 'NameType'. 0 >>> Ordinal for the Function,
              ; that should be at +10...
                .If W$esi+012 <> 0
                    Call WriteLibImportObj
                    add esi 014
                    Call WriteImportObjStrings

                .Else
                    ;Call SendMeThisLib
                    Call WriteLibImportObj

                    movzx eax W$esi+010
                    Mov edx D$esi+0C
                    add esi 014
                    add edx esi
                    Mov ebx esi | While B$ebx <> 0 | inc ebx | End_While | inc ebx
                    If ebx < edx
                        Call WriteImportObjStrings
                    Else
                        Call WriteImportObjStringAndOrdinal eax
                    End_If
                .End_If

            ..Else
                ; to be added in AddListviewItem
                ; Always clear the loaded data before use.
                Call ClearDwordBuffer ExportedLibraryStr,
                                      (256/DWORD)
                
                Mov D$ExportedLibrary 0
                
                Call GetCoffListing

            ..End_If

        ...Else
            ; to be added in AddListviewItem
            ; Always clear the loaded data before use.
            Call ClearDwordBuffer ExportedLibraryStr,
                                  (256/DWORD)
   
            Mov D$ExportedLibrary 0
   
            Call GetCoffListing

        ...End_If
    Pop esi
ret
____________________________________________________________________________________________

[IMAGE_FILE_MACHINE_Text:
 &IMAGE_FILE_MACHINE_UNKNOWN, '&IMAGE_FILE_MACHINE_UNKNOWN' EOS
 &IMAGE_FILE_MACHINE_ALPHA, '&IMAGE_FILE_MACHINE_ALPHA' EOS
 &IMAGE_FILE_MACHINE_ALPHA64, '&IMAGE_FILE_MACHINE_ALPHA64' EOS
 &IMAGE_FILE_MACHINE_ARM, '&IMAGE_FILE_MACHINE_ARM' EOS
 &IMAGE_FILE_MACHINE_AXP64, '&IMAGE_FILE_MACHINE_AXP64' EOS
 &IMAGE_FILE_MACHINE_CEF, '&IMAGE_FILE_MACHINE_CEF' EOS
 &IMAGE_FILE_MACHINE_I386, '&IMAGE_FILE_MACHINE_I386' EOS
 &IMAGE_FILE_MACHINE_I486, '&IMAGE_FILE_MACHINE_I486' EOS
 &IMAGE_FILE_MACHINE_I586, '&IMAGE_FILE_MACHINE_I586' EOS
 &IMAGE_FILE_MACHINE_IA64, '&IMAGE_FILE_MACHINE_IA64' EOS

 &IMAGE_FILE_MACHINE_M68K, '&IMAGE_FILE_MACHINE_M68K' EOS
 &IMAGE_FILE_MACHINE_MIPS16, '&IMAGE_FILE_MACHINE_MIPS16' EOS
 &IMAGE_FILE_MACHINE_MIPSFPU, '&IMAGE_FILE_MACHINE_MIPSFPU' EOS
 &IMAGE_FILE_MACHINE_MIPSFPU16, '&IMAGE_FILE_MACHINE_MIPSFPU16' EOS
 &IMAGE_FILE_MACHINE_POWERPC, '&IMAGE_FILE_MACHINE_POWERPC' EOS
 &IMAGE_FILE_MACHINE_R10000, '&IMAGE_FILE_MACHINE_R10000' EOS
 &IMAGE_FILE_MACHINE_R3000, '&IMAGE_FILE_MACHINE_R3000' EOS
 &IMAGE_FILE_MACHINE_R4000, '&IMAGE_FILE_MACHINE_R4000' EOS
 &IMAGE_FILE_MACHINE_R6000, '&IMAGE_FILE_MACHINE_R6000' EOS
 &IMAGE_FILE_MACHINE_SH3, '&IMAGE_FILE_MACHINE_SH3' EOS

 &IMAGE_FILE_MACHINE_SH3E, '&IMAGE_FILE_MACHINE_SH3E' EOS
 &IMAGE_FILE_MACHINE_SH4, '&IMAGE_FILE_MACHINE_SH4' EOS
 &IMAGE_FILE_MACHINE_SH5, '&IMAGE_FILE_MACHINE_SH5' EOS
 &IMAGE_FILE_MACHINE_SH3DSP, '&IMAGE_FILE_MACHINE_SH3DSP' EOS
 &IMAGE_FILE_MACHINE_THUMB, '&IMAGE_FILE_MACHINE_THUMB' EOS
 &IMAGE_FILE_MACHINE_WCEMIPSV2, '&IMAGE_FILE_MACHINE_WCEMIPSV2' EOS
 &IMAGE_FILE_MACHINE_AM33, '&IMAGE_FILE_MACHINE_AM33' EOS
 &IMAGE_FILE_MACHINE_AMD64, '&IMAGE_FILE_MACHINE_AMD64' EOS
 &IMAGE_FILE_MACHINE_CEE, '&IMAGE_FILE_MACHINE_CEE' EOS
 &IMAGE_FILE_MACHINE_EBC, '&IMAGE_FILE_MACHINE_EBC' EOS

 &IMAGE_FILE_MACHINE_M32R, '&IMAGE_FILE_MACHINE_M32R' EOS
 &IMAGE_FILE_MACHINE_POWERPCFP, '&IMAGE_FILE_MACHINE_POWERPCFP' EOS
 &IMAGE_FILE_MACHINE_TRICORE, '&IMAGE_FILE_MACHINE_TRICORE' EOS, 0-1, 0-1]

[UnKown_IMAGE_FILE_MACHINE: B$ ' ; Unknown or corrupted IMAGE_FILE_MACHINE' EOS]

Proc WriteIMAGE_FILE_MACHINE:
    Argument @Type
    Uses esi

        Mov esi IMAGE_FILE_MACHINE_Text, eax D@Type | and eax 0FFFF

        ..While D$esi <> 0-1
            .If D$esi = eax
                add esi 4 | While B$esi <> 0 | movsb | End_While | ExitP
            .Else
                add esi 5
                .While B$esi <> '&'
                    inc esi
                    If D$esi = 0-1
                        Call WriteEax
                        Mov esi UnKown_IMAGE_FILE_MACHINE
                        While B$esi <> 0 | movsb | End_While | ExitP
                    End_If
                .End_While
                sub esi 4
            .End_If
        ..End_While
EndP


Proc IsIMAGE_FILE_MACHINE:
    Argument @Type
    Uses esi

        Mov esi IMAGE_FILE_MACHINE_Text, eax D@Type | and eax 0FFFF

        .While D$esi <> 0-1
            .If D$esi = eax
                Mov eax &TRUE | ExitP
            .Else
                add esi 5
                While B$esi <> '&'
                    inc esi
                    If D$esi = 0-1
                        Mov eax &FALSE | ExitP
                    End_If
                End_While
                sub esi 4
            .End_If
        .End_While
EndP

____________________________________________________________________________________________

;;

;;
 
 
 LibScan_ErrManager
 _____________________
 
    This function manages the error cases of the LIbScanner system
 
 Parameters: 
 
    ErrFlag: Handle to the error message. The equates used on this function are:

    IMP_OBJ_RESBIT_ERR = 01 ; Reserved Bits are not Zero
    IMP_OBJ_UNKTYPE_ERR = 02 ; Unknown Type
    

        PE_DATADIRSMALL 01 = The Directory Entry is located before the 1st section, on the DosHeader or in the PEHeader.
 
 Return values:
 
    Return &TRUE if  the user wants to contine analysing the file.
    Return &FALSE if he wants to stop the analysis.
 
 
;;

;[PE_DATADIRSMALL 01]
;[PE_DIRDATADOS 02]
;[PE_DIRDATASIZE 03]
;[PE_DIRDATAUNKNOWN 04]
;;

[IMP_OBJ_RESBIT_ERR 01]
[IMP_OBJ_UNKTYPE_ERR 02]


Proc LibScan_ErrManager:
    Arguments @ErrFlag
    Uses edx ; messagebox function changes the value of edx...So we need to preserve it, because we will use it.

    Mov eax D@ErrFlag

    .If eax = IMP_OBJ_RESBIT_ERR


        Call 'USER32.MessageBoxA' 0, {B$ "CORRUPTED LIBRARY !!!

The reserved Bits for the ImpObjHdr.Type member of the structure IMPORT_OBJECT_HEADER
on this Library are not Zero.

RosAsm can continue loading this file, but we will set the reserved Bits to ZERO to you be able to load this safelly.

* Press the YES button to set the necessary bits to Zero.
* Press the NO button to end analysing this file.
  
If after the automatically fix, this file still have errors, please, report this error to the author at:
  Betov@free.fr

Or report this error to the Development Team at the main RosAsm Forum.

Many thanks in advance. Betov.
  
" EOS}, {B$ 'Corrupted Library' EOS}, &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION__&MB_YESNO

            If eax = &IDNO
                Mov eax &FALSE
            Else_If eax = &IDYES
                Mov eax &TRUE
            End_If

    .Else_If eax = IMP_OBJ_UNKTYPE_ERR

          Call 'USER32.MessageBoxA' D$H.MainWindow {B$ "UNKNOWN TYPE !!!.

The reserved Bits for the ImpObjHdr.Type member of the structure IMPORT_OBJECT_HEADER
on this Library are of a Unknown Type.

The regular Type values of this member can not be 3, 7, 11, 15 to 31.

Your file have one of those unknown types.

RosAsm can't continue loading this file.

Please, report this error to the author, and send this file to the author at:
  Betov@free.fr

Or report this error to the Development Team at the main RosAsm Forum.

Many thanks in advance. Betov." EOS} {B$ "Unknown Type" EOS} &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION

            Mov eax &FALSE

    .End_If

EndP


____________________________________________________________________________________________

[ImpObjHdrSig1Text: B$ 'ImpObjHdr.Sig1: W$ &IMAGE_FILE_MACHINE_UNKNOWN' EOS]
[ImpObjHdrSig2Text: B$ 'ImpObjHdr.Sig2: W$ &IMPORT_OBJECT_HDR_SIG2' EOS]
[ImpObjHdrVersionText: B$ 'ImpObjHdr.Version: W$ ' EOS]
[ImpObjHdrMachineText: B$ 'ImpObjHdr.Machine: W$ ' EOS]
[ImpObjHdrTimeDateStampText: B$ 'ImpObjHdr.TimeDateStamp: D$ ' EOS]
[ImpObjHdrSizeOfDataText: B$ 'ImpObjHdr.SizeOfData: D$ ' EOS]
[ImpObjHdrOrdinalText: B$ 'ImpObjHdr.Ordinal.Hint: W$ ' EOS]
[ImpObjHdrType: B$ 'ImpObjHdr.Type: W$ ' EOS]

WriteObjIndice:
    Mov D$edi 'Obj',
        ecx D$LibObjIndice, D$edi+3 ecx,
        cx W$LibObjIndice+4, W$edi+7 cx

    add edi 9

    Mov B$edi '.' | inc edi
ret


WriteIndiceOnly:
    Mov ecx D$LibObjIndice, D$edi ecx, cx W$LibObjIndice+4, W$edi+4 cx
    add edi 6
ret


Proc WriteLibImportObjItem:
    Arguments @Text, @Flag
    Uses esi

        Call WriteObjIndice

        Mov esi D@Text | While B$esi <> 0 | movsb | End_While

        If D@Text = ImpObjHdrMachineText
            Call WriteIMAGE_FILE_MACHINE eax

        Else_If D@Text = ImpObjHdrType
            ; Need to increase edi with CRLF at the end, because when  the function returns, it
            ; decreases edi by 2. And we need to close the bracket after the paragraph, due to the commented
            ; Hexadecimal Value below.
            Push eax
            Call WriteLibImportObjType eax
            Pop eax
            zCopy {B$ ' ; Hex Value:  ' EOS}
            Call WriteEax
            Mov W$edi CRLF | add edi 2

        Else_If D@Text = ImpObjHdrSizeOfDataText

            Push esi
                Call WriteObjIndice
                zCopy {B$ 'ImportStringsEnd' EOS}
            Pop esi

            Mov D$edi ' - ' | add edi 3
            Push esi
                Call WriteObjIndice
                zCopy {B$ 'ImportStrings ; Hex Value:  ' EOS}
            Pop esi
            Call WriteEax

        Else_If B@Flag = &TRUE
            Call WriteEax

        End_If

        Mov W$edi CRLF | add edi 2
EndP

____________________________________________________________________________________________



;LibImportObjType:
; Bits 0 and 1:
[IMPORT_OBJECT_CODE: B$ '&IMPORT_OBJECT_CODE' EOS]
[IMPORT_OBJECT_DATA: B$ '&IMPORT_OBJECT_DATA' EOS]
[IMPORT_OBJECT_CONST: B$ '&IMPORT_OBJECT_CONST' EOS]

; Bits 2 to 3 (SHRed) >>> 0 to 1
[IMPORT_OBJECT_NAME: B$ '&IMPORT_OBJECT_NAME' EOS]
[IMPORT_OBJECT_NAME_NO_PREFIX: B$ '&IMPORT_OBJECT_NAME_NO_PREFIX' EOS]
[IMPORT_OBJECT_NAME_UNDECORATE: B$ '&IMPORT_OBJECT_NAME_UNDECORATE' EOS]
[IMPORT_OBJECT_ORDINAL: B$ '&IMPORT_OBJECT_ORDINAL' EOS]
;;
;;
  Example, if a Type Word would be:
  
  '&IMPORT_OBJECT_DATA' and '&IMPORT_OBJECT_NAME_NO_PREFIX'
  
  The Assembly representation would be:
  
  &IMPORT_OBJECT_DATA + (&IMPORT_OBJECT_NAME_NO_PREFIX shl 2)
;;

;;

    We have at eax the ImpObjHdr.Type member that is a WORD data. Accordying to the documentation, we only
    are interested in Bits 0 to 3 to use for Type and NameType members interpretations.
    Bit4 is for Unknown Type and should not be considered to analysis at the present moment.
    Bits 5 to 15 are reserved and should be setlled to 0 accordying to the docs, because they are not
    interpreteded and are not used whatsoever.
    
    A Word data have a maximum value of 0FFFF (65535), meaning that bits 0 to 15 are all settled to 1.
    
    Since bits 5 to 15 are not used, we can set them all to 0, that will result on a maximum value for this member of
    31 (Bits 0 to 4 are all settled to 1, then).
    
    Now, our WORD have a maximum range of 31.
    
    Accordying to the docs, we can have a possible set of Equates under the following form:
 
    EQUATES Forms                                               Value   Bits Sets                       Union
    
    &IMPORT_OBJECT_CODE+(&IMPORT_OBJECT_ORDINAL shl 2)          = 0     No Bits are Flaged              Ordinal
    &IMPORT_OBJECT_CODE+(&IMPORT_OBJECT_NAME shl 2)             = 4     Only Bit2 is flaged             Hint
    &IMPORT_OBJECT_CODE+(&IMPORT_OBJECT_NAME_NO_PREFIX shl 2)   = 8     Only Bit3 is flaged             Hint
    &IMPORT_OBJECT_CODE+(&IMPORT_OBJECT_NAME_UNDECORATE shl 2)  = 12    Only Bit2 and Bit3 are flaged   Hint
     
    &IMPORT_OBJECT_DATA+(&IMPORT_OBJECT_ORDINAL shl 2)          = 1     Only Bit0 is flaged             Ordinal
    &IMPORT_OBJECT_DATA+(&IMPORT_OBJECT_NAME shl 2)             = 5     Only Bit0 and Bit2 are flaged   Hint
    &IMPORT_OBJECT_DATA+(&IMPORT_OBJECT_NAME_NO_PREFIX shl 2)   = 9     Only Bit0 and Bit3 are flaged   Hint
    &IMPORT_OBJECT_DATA+(&IMPORT_OBJECT_NAME_UNDECORATE shl 2)  = 13    Only Bit0,Bit2,Bit3 are flaged  Hint
 
    &IMPORT_OBJECT_CONST+(&IMPORT_OBJECT_ORDINAL shl 2)         = 2     Only Bit1 is flaged             Ordinal
    &IMPORT_OBJECT_CONST+(&IMPORT_OBJECT_NAME shl 2)            = 6     Only Bit1 and Bit2 are flaged   Hint
    &IMPORT_OBJECT_CONST+(&IMPORT_OBJECT_NAME_NO_PREFIX shl 2)  = 10    Only Bit1 and Bit3 are flaged   Hint
    &IMPORT_OBJECT_CONST+(&IMPORT_OBJECT_NAME_UNDECORATE shl 2) = 14    Only Bit1,Bit2,Bit3 are flaged  Hint

    
    So, All values equal or above 14 are not allowed and also we can not have the following values and interpretations:
    So, from values between 31 and 14 we have an Unknown Type. All Values bigger or equal then 32 we have a corrupted
    Library because the reserved Fields are not 0.
    
    The list of Uknown Types are:
    03; 07; 11, 15 to 31

   Ex.: We have a WORD with the value of 4073. So we clean the high bits resulting, like this:
                Mov eax 4073 | Mov ah 0 ; Bits 8 to 15 settled to 0)
        
        Now we must clear the bits 5 to 7, because they are unused. Like this:
         btr eax 5 | btr eax 6 | btr eax 7 ; I�m using this, because it is probably faster on big files.
         
        Or we can also do this:
        shl eax 3 | Mov ah 0 | shr eax 3
        
   
   
;;
;;
[SymNameType: B$ 0] ; Holds the NameType value to be used to Skip the leading chars on NameType member.

Proc WriteLibImportObjType:
    Argument @Flags
    Uses esi, ebx

        Mov eax D@Flags

        .If_And eax >= 32, eax <= 65535 ; Check for Corrupted library. Reserved Bits are not 0

            Call LibScan_ErrManager IMP_OBJ_RESBIT_ERR

            If eax = &FALSE ; The user pressed No

EndP

            Else_If eax = &TRUE ; The user pressed Yes. We will Zero all reserved bits for him continue the parsing.
                Mov eax D@Flags ; Restore the original value of eax to be fixed
                Mov ah 0 ; Clear High Bits Flags
                btr eax 5 | btr eax 6 | btr eax 7 ; Clear Bits 5 to 7

            End_If

        .End_If

        ; Check for Unknown Types


        test eax 010 | jz A1> ; Is Bit4 Flagged ? So, is eax = 16 to 31 ? No, jmp over
            Call LibScan_ErrManager IMP_OBJ_UNKTYPE_ERR

EndP

A1:
        test eax 1 | jz A2> ; Is bit0 flagged ? No...Ok, we don� have any unknown types (15, 11, 7, 3)
            test eax 2 | jz A2> ; is bit1 also Flagged ? No...Ok, we don� have any unknown types (15, 11, 7, 3) jmp over.

            Call LibScan_ErrManager IMP_OBJ_UNKTYPE_ERR

            ; Yes bit0 and bit1 are flagged, we have unknown types (15, 11, 7, 3)
EndP

A2:


    ; We are Ok. We have only the values 0,1,2,4,5,6,8,9,10,12,13,14 on eax

          ; First lower two Bits (Code / Data / Constant). We can only have one of the 02 bits set, or none of them.

            test eax &IMPORT_OBJECT_DATA | jz L1> ; Is Bit0 Flaged ? Yes, do next line. Values = 1,5,9,13
                Mov esi IMPORT_OBJECT_DATA
                jmp L2>

L1:         test eax &IMPORT_OBJECT_CONST | jz L1> ; Is Bit1 Flaged ? Yes, do next line. Values = 2,6,10,14
                Mov esi IMPORT_OBJECT_CONST
                jmp L2>

L1:         ; we don� have bit0 and bit1 flagged. Values = 0,4,8,12
                Mov esi IMPORT_OBJECT_CODE

L2:
                While B$esi <> 0 | movsb | End_While
                Mov W$edi '+(' | add edi 2


          ; Second Part of the Record of Bits 2,3 (Ordinal, Name, NoPrefix, Undecorated)
          ; We can have all of the 02 bits set, or none of them.

                shr eax 2


            cmp eax &IMPORT_OBJECT_NAME_UNDECORATE | jnz L1> ; Is Bit0 and 1 Flaged ? Yes, do next line
                                                            ; Values = 12, 13, 14
                Mov esi IMPORT_OBJECT_NAME_UNDECORATE
                Mov B$SymNameType &IMPORT_OBJECT_NAME_UNDECORATE ; Save for later skip leading chars
                jmp L2>


L1:         test eax &IMPORT_OBJECT_NAME | jz L1> ; Is Bit0 Flaged ? Yes, do next line. Values = 4,5,6
                Mov esi IMPORT_OBJECT_NAME
                jmp L2>

L1:         test eax &IMPORT_OBJECT_NAME_NO_PREFIX | jz L1> ; Is Bit1 Flaged ? Yes, do next line. Values = 8,9,10
                Mov esi IMPORT_OBJECT_NAME_NO_PREFIX
                Mov B$SymNameType &IMPORT_OBJECT_NAME_NO_PREFIX ; Save for later skip leading chars
                jmp L2>

L1:         ; we don't have bit0 and bit1 flagged. Values = 0,1,2
                Mov esi IMPORT_OBJECT_ORDINAL

L2:
                While B$esi <> 0 | movsb | End_While
                Mov D$edi ' shl', D$edi+4 ' 2)' | add edi 7

EndP


____________________________________________________________________________________________

Proc WriteImportObjHdrTimeDateStamp:
    Arguments @TimeDate
    Uses esi

        sub edi 2 ; We need to subtract 02 Bytes, to bypass the previous CRLF
        Mov esi {B$ ' ; - TimeDate Stamp: ' EOS}
        While B$esi <> 0 | movsb | End_While

        ; Get the DateTime Stamp

        ; Time date stamp to string Function
        Call TimeDateStampToString D@TimeDate {B$ "yyyy/MM/dd ddd " EOS} {B$ "HH:mm:ss UTC" EOS} szDateString szTimeString

        ; Write the TimeDate Stamp strings
        Mov esi szDateString
        While B$esi <> 0 | movsb | End_While

        Mov esi szTimeString
        While B$esi <> 0 | movsb | End_While

        Mov W$edi CRLF | add edi 2 ; We now Add the paragraphs marks, and add 02 Bytes at edi

EndP
____________________________________________________________________________________________

[ImpObjDateStamp: D$ ?]

WriteLibImportObj:

    Push esi
        Mov esi CoffListingTitle
        While B$esi <> 0 | movsb | End_While
        Call WriteIndiceOnly
        Mov B$edi ':', W$edi+1 CRLF | add edi 3
    Pop esi

    ; To be used in AddListviewItem
    Push esi
        Mov D$LvOffsetCOFF 0 ; always initialize at 0 1st, due to the several loopings for each object.
        sub esi D$LibFileMemory
        Mov D$LvOffsetCOFF esi
    Pop esi

    Push esi
;        Mov B$edi '[' | inc edi

        Call WriteLibImportObjItem ImpObjHdrSig1Text, &FALSE | add esi 2

        Call WriteLibImportObjItem ImpObjHdrSig2Text, &FALSE | add esi 2

        movzx eax W$esi | add esi 2
        Call WriteLibImportObjItem ImpObjHdrVersionText, &TRUE

        ; To be used in AddListviewItem
        Mov W$CoffMachineType 0 ; always initialize at 0 1st, due to the several loopings for each object.
        Move W$CoffMachineType W$esi

        movzx eax W$esi | add esi 2
        Call WriteLibImportObjItem ImpObjHdrMachineText, &TRUE

        lodsd
        Mov D$ImpObjDateStamp eax
        Call WriteLibImportObjItem ImpObjHdrTimeDateStampText, &TRUE
        Call WriteImportObjHdrTimeDateStamp D$ImpObjDateStamp

        lodsd
        If D$esi-4 <> 0
            Call WriteLibImportObjItem ImpObjHdrSizeOfDataText, &TRUE
        Else

            Push esi
                Call WriteObjIndice
                zCopy ImpObjHdrSizeOfDataText
            Pop esi
            Call WriteEax
            Mov W$edi CRLF | add edi 2
        End_If

        movzx eax W$esi | add esi 2
        Call WriteLibImportObjItem ImpObjHdrOrdinalText, &TRUE

        movzx eax W$esi | add esi 2
        Call WriteLibImportObjItem ImpObjHdrType, &TRUE

        Mov B$edi-2 ']' | dec edi | Mov D$edi CRLF2 | add edi 4
    Pop esi
ret


[ImportStringsText: B$ "ImportStrings: B$ '" EOS]
[RosAsmInterpretation: B$ "; RosAsm Interpretation: '" EOS]
[RosAsmUndecoratedName: B$ "; Full Undecorated Name: " EOS]

;[UndecNameBuffer: D$ 0 # 4000]
;[Size_Of_UndecNameBuffer 4000]

Proc WriteImportObjStrings:
    Uses ecx, esi

    Mov D$edi '[' | inc edi
    Mov D$edi 'Obj',
        eax D$LibObjIndice, D$edi+3 eax,
        ax W$LibObjIndice+4, W$edi+7 ax,
        B$edi+9 '.' | add edi 10
    Push esi
        Mov esi ImportStringsText | While B$esi <> 0 | movsb | End_While
    Pop esi

    Push esi
        ; to be added in AddListviewItem
        Push esi
        Push edi
            ; Always clear the loaded data before use.
            Call ClearDwordBuffer ExportedLibraryStr,
                                  (256/DWORD)
            
            Mov D$ExportedLibrary 1
            Mov edi ExportedLibraryStr
            While B$esi <> 0 | movsb | End_While
        Pop edi
        Pop esi


        ; Copy the Function Name:
        While B$esi <> 0 | movsb | End_While
        Mov D$edi "', 0", D$edi+4 ", '" | add edi 7
        While B$esi = 0 | inc esi | End_While

        ; Copy the Module Name:
        Push esi
            While B$esi <> 0 | movsb | End_While
            Mov D$edi "', 0" | add edi 4
            Mov W$edi CRLF | add edi 2
            Push esi
                Call WriteObjIndice
                zCopy {B$ 'ImportStringsEnd:]' EOS}
            Pop esi
            Mov D$edi CRLF2 | add edi 4

            Mov esi RosAsmInterpretation | While B$esi <> 0 | movsb | End_While
        Pop esi

        ; Copy the Commented Module Name:
        While B$esi <> 0 | movsb | End_While
        Mov eax D$edi-4 | or eax 020202000
        If eax = '.dll'
            sub edi 4
        End_If

        Mov B$edi '.' | inc edi
    Pop esi

    Mov ecx esi ; Copy esi String to be used for name undecoration

  ; Copy the Commented Function Name:

    ; Remove 1st '_'; '@'; '?'
    .If B$SymNameType = &IMPORT_OBJECT_NAME_NO_PREFIX

        On B$esi = '_', inc esi
        On B$esi = '@', inc esi
        On B$esi = '?', inc esi

       While B$esi <> 0 | movsb | End_While

    ; Remove 1st '_'; '@'; '?' and truncate after next '@'.
    ; Ex:   _AbortPath@4 to AbortPath
    ;       ?WSCWriteProviderOrder@@YGHPAKK@Z to WSCWriteProviderOrder WS2_32.lib
    ;       ___CxxLongjmpUnwind@4 to __CxxLongjmpUnwind (MSVCRT.LIB)

    .Else_If B$SymNameType = &IMPORT_OBJECT_NAME_UNDECORATE

        On B$esi = '_', inc esi
        On B$esi = '@', inc esi
        On B$esi = '?', inc esi
        ; Ok, removed the above chars. No we need to truncate after findng the @
        While B$esi <> '@'

            On B$esi = 0, jmp L0> ; Sometimes we have No @ char here (Pelles\advapi32.lib)
                                  ; [Obj000397.ImportStrings: B$ '_SetServiceBits' EOS, 'ADVAPI32.dll' EOS]
            movsb

        End_While

        L0:

    .Else
        ; For all other cases, keep the lib name intact
       While B$esi <> 0 | movsb | End_While

    .End_If

    Mov B$edi "'" | inc edi

    Mov D$edi CRLF2 | add edi 4

    ; Write the full Undecorated name. Ecx ecx points to {B$ '?wndTopMost@CWnd@@2V1@B' EOS}

    Push esi
        Mov esi RosAsmUndecoratedName | While B$esi <> 0 | movsb | End_While
    Pop esi

    Push ecx
    Call 'IMAGEHLP.UnDecorateSymbolName' ecx, Trash, 4000, &UNDNAME_COMPLETE
    Pop ecx

    Call StrCmp ecx Trash

    ; If the strings are the same it means that the above function failed, because the undecoretated name is not the same
    ; as the original loaded name.
    If eax = 0
        Call Simple_UndecorateSymbolName ecx, Trash
    End_If

    Push esi
        Mov esi Trash | While B$esi <> 0 | movsb | End_While
    Pop esi


    Mov D$edi CRLF2 | add edi 4
EndP
___________________________________________________________________________
;;
;;
Bad Values

[Obj000004.ImportStrings: B$ '?ClearFontManager@@YAXXZ' EOS, 'AYGSHELL.dll' EOS
Obj000004.ImportStringsEnd:]

; RosAsm Interpretation: 'AYGSHELL.?ClearFontManager@@YAXXZ'

; Full Undecorated Name: void __cdecl ClearFontManager(void)


Good values


; RosAsm Interpretation: B$ 'ACLUI.EditSecurity'

; Full Undecorated Name: _EditSecurity@8
;;
;;
Proc Simple_UndecorateSymbolName:
    Arguments @Input, @Output
    Local @ParamCount

    pushad

    Mov D@ParamCount 0
    Mov edi D@Input

    While B$edi <> 0
        On B$edi = '?', jmp L4>> ; On this simplified versin we cannot have this char
        On B$edi = '$', jmp L4>> ; On this simplified versin we cannot have this char
        On B$edi = '@', jmp L1> ; This is possible to have, but a good symbols can only have this once.
        inc edi
    End_While

    jmp L2>
L1:
   inc edi

    While B$edi <> 0
        On B$edi = '@', jmp L4>> ; If we found this value again, we have a bad symbol
        If_And B$edi < '0', B$edi > '9' ; the values after the '@' char must be numbers from 0 to 9. Otherwise it is a bad symbol
            jmp L4>
        End_If
        inc edi
    End_While

    ; If we reach here we have a Good Symbol.
L2:

    Mov esi D@Input
    Mov edi D@Output

    If B$esi = '_'
        inc esi
    End_If

    If B$esi = 0
        Push esi | ZCopy {B$ "Invalid ! Null Symbol Name." EOS} | Pop esi
        jmp L4>
    End_If

    .While B$esi <> 0

        .If B$esi = '@' ; We found our delimiter for the Parameters amount.
            inc esi ; Bypass the '@' char.
            Call DecimalStringToDword esi
            shr eax 2 ; divide the result by 4
            Push esi | ZCopy {B$ " - Amount of Parameters: " EOS} | Pop esi

            ; Convert Dword to Decimal String
            Push edi
                Mov D@ParamCount eax
                lea esi D@ParamCount
                Mov ecx 4
                Call toUDword
                Mov esi edi
            Pop edi

            Do | movsb | LoopUntil B$esi-1 = 0
            dec edi
            jmp L3>
        .End_If

        movsb
    .End_While

L3:
    Mov B$edi 0

   L4:

    popad

EndP

___________________________________________________________________________
___________________________________________________________________________

; based on GetMemberSize

Proc DecimalStringToDword:
    Arguments @String
    Uses, esi, ebx, ecx, edx

    Mov esi D@String
    Mov ecx 10, ebx 0, eax 0

    .While B$esi <> 0
        on B$esi = SPC, ExitP
        mul ecx
        Push eax
            lodsb | sub al '0' | Mov bl al
        Pop eax
        add eax ebx
    .End_While

EndP
___________________________________________________________________________
___________________________________________________________________________

Proc WriteImportObjStringAndOrdinal:
    Argument @Ordinal

        Mov D$edi '[' | inc edi
        Mov D$edi 'Obj',
            eax D$LibObjIndice, D$edi+3 eax,
            ax W$LibObjIndice+4, W$edi+7 ax,
            B$edi+9 '.' | add edi 10
        Push esi
            Mov esi ImportStringsText | While B$esi <> 0 | movsb | End_While
        Pop esi


        ; to be added in AddListviewItem
        Push esi,
             edi

            ; Always clear the loaded data before use.
            Call ClearDwordBuffer ExportedLibraryStr,
                                  (256/DWORD)
            
            Mov D$ExportedLibrary 1
            
            Mov edi ExportedLibraryStr
            
            While B$esi <> 0 | movsb | End_While
        
        Pop edi,
            esi


        ; Copy the Module Name:
        Push esi
            While B$esi <> 0 | movsb | End_While
            Mov D$edi "', 0" | add edi 4
            Mov W$edi CRLF | add edi 2
            Push esi
                Call WriteObjIndice
                zCopy {B$ 'ImportStringsEnd:]' EOS}
            Pop esi
            Mov D$edi CRLF2 | add edi 4

            Mov esi RosAsmInterpretation | While B$esi <> 0 | movsb | End_While

        Pop esi

      ; Copy the Commented Module Name:
        While B$esi <> 0 | movsb | End_While
        Mov eax D$edi-4 | or eax 020202000
        If eax = '.dll'
            sub edi 4
        End_If

        Mov B$edi '.' | inc edi
    Pop esi
  ; Copy the Commented Function Ordinal:
    Mov eax D@Ordinal | Call WriteEax

    Mov D$edi CRLF2 | add edi 4
EndP



[LookUpValidNameCharsTable: B$ ? # 0100]

InitLookUpValidNameCharsTable:
    Mov edi LookUpValidNameCharsTable, eax 0, ecx 0100

  ; Build a normal Asicii Table:
L0: Mov B$edi+eax al | inc eax | loop L0<
ret

  ; Replace everything above 127 by '_'
    Mov ecx 080, ebx 080, al '_'
L0: Mov B$edi+ebx al | inc ebx | loop L0<

  ; Replace Ascii 127 by '_'
    Mov B$edi+07F '_'

    Mov eax '\' | Mov B$edi+eax '.'

    Mov eax '?' | Mov B$edi+eax '.'

    Mov eax '_' | Mov B$edi+eax '.'

    Mov eax '$' | Mov B$edi+eax '.'

    Mov eax '|' | Mov B$edi+eax '.'
ret
;;
  ; Other possibility:
;;
  
    Mov al '_', ecx 0100, edi LookUpValidNameCharsTable | rep stosb
    
    Mov edi LookUpValidNameCharsTable, eax '0'
    While eax <= '9'
        Mov B$edi+eax al | inc al
    End_While
    
    Mov eax 'A'
    While eax <= 'Z'
        Mov B$edi+eax al | inc al
    End_While
    
    Mov eax 'a'
    While eax <= 'z'
        Mov B$edi+eax al | inc al
    End_While
ret
;;
;;
[LookUpValidNameChars | lodsb | and eax 0FF | Mov al B$LookUpValidNameCharsTable+eax | stosb]

[IMAGE_ARCHIVE_MEMBER_SIZE 030]

GetLongNamesPointer:  ; GetMemberSize
    ...If D$LongNamesBase = 0
        pushad
          ;  Mov esi D$LibFileMemory | add esi 8

L0:         If W$esi = '//'
                add esi 03C
                Mov D$LongNamesBase esi

            Else_If B$esi = '/'
                Push D$MemberSize
                    add esi IMAGE_ARCHIVE_MEMBER_SIZE
                    Call GetMemberSize
                    sub esi IMAGE_ARCHIVE_MEMBER_SIZE
                    add esi 03C | add esi D$MemberSize
                Pop D$MemberSize | jmp L0<

            End_If
        popad
    ...End_If
ret


CheckLongNameDisplacement:
    Push esi
        While B$esi <> SPC
            lodsb
            If al < '0'
                Mov eax &FALSE | jmp L9>
            Else_If al > '9'
                Mov eax &FALSE | jmp L9>
            End_If
        End_While
        Mov eax &TRUE
L9: Pop esi
ret


Proc WriteLongName:  ; GetMemberSize
    Argument @Dis
    Uses ebx, ecx, edx

    Push esi
        Mov esi {B$ 'Long Name: ' EOS};IMAGE_ARCHIVE_MEMBER_HEADERstring
        While B$esi <> 0 | movsb | End_While
    Pop esi

        Mov esi D@Dis, ecx 10, ebx 0, eax 0

        While B$esi > SPC
            mul ecx
            Push eax
                lodsb | sub al '0' | Mov bl al
            Pop eax
            add eax ebx
        End_While

        Mov esi D$LongNamesBase | add esi eax

        While B$esi >= SPC | movsb | End_While
        On B$edi-1 = '/', dec edi
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[LongNamesBase: D$ ?]

GetLongNamesBase:
    Mov D$LongNamesBase 0
   ____________________________________________
  ; Lib Tag:
    Mov esi D$LibFileMemory | add esi 8

    If W$esi = '/ '
        Push esi
            add esi LIB_MEMBER_SIZE | Call GetMemberSize
        Pop esi
        add esi COFF_HEADER_SIZE
        add esi D$MemberSize | On B$esi = 0A, inc esi
    End_If
   ____________________________________________
  ; Second optional Lib Header:
    If W$esi = '/ '
        Push esi
            add esi LIB_MEMBER_SIZE | Call GetMemberSize
        Pop esi
        add esi COFF_HEADER_SIZE
        add esi D$MemberSize | On B$esi = 0A, inc esi
    End_If
   ____________________________________________
  ; Third optional Lib Header:
    If W$esi = '//'
        add esi COFF_HEADER_SIZE
        Mov D$LongNamesBase esi
    End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
;;
                      Write the Tag and first Library Header
;;
;;
____________________________________________________________________________________________

[WriteNotice: ";;

RosAsm Development Team presents:
________________________________________________

______________ Lib Scanner v 1.0  ______________
__________________  Set/05  ____________________
________________________________________________

This file was generated by RosAsm Library File Dumper,
aka known as Lib Scanner v 1.0.

http://www.rosasm.org
http://rosasm.org
http://betov.free.fr

If you have any questions about this file, read RosAsm's
Help file (B_U_Asm.exe), or feel free to contact us at:

http://www.quanta-it.com/easbell/RosAsmForum/index.php

_______________________

Author(s):

Ren� Tournois (Betov)
Gustavo Trigueiros (Beyond2000!)

;;
________________________________________________

;;
" EOS]


[LibTag: ";;

________________________________________________

    Tag Data. The Magic String
________________________________________________
________________________________________________
;;
;;
[Magic: B$   '!<arch>', 0A]


;;
;;
________________________________________________

    First Linker Header - Names Linker Member
________________________________________________
________________________________________________
;;
;;

; This is the IMAGE_ARCHIVE_MEMBER_HEADER Structure.

" EOS]
____________________________________________________________________________________________

WriteLibTag:
  ; (Includes the First Headers comments at once):
    Mov esi LibTag | While B$esi <> 0 | movsb | End_While

    Mov esi D$LibFileMemory | add esi 8
ret
____________________________________________________________________________________________

[AmountOfMembers: B$ "; Amount of Members on the file.

[PublicSymbols1: D$ " EOS]

[NumberOfSymbols: D$ ?]

ShowAmountOfSymbols1:
;;
;;
  Show the Amount of Symbols (a dWord stored the other way round):
;;
;;
    Push esi
        Mov esi AmountOfMembers | While B$esi <> 0 | movsb | End_While
    Pop esi
    lodsd | bswap eax | Mov D$NumberOfSymbols eax | Call WriteEax

    Mov B$edi ']', D$edi+1 CRLF2 | add edi 5
ret
____________________________________________________________________________________________

[AmountOfOffsets1: B$ "; Amount of Offsets of the Public Symbols.

[SymOffset:
 " EOS]

ShowOffsetTable1: ; 'ShowOffsetTable2'
;;
;;
  Show the Offset Table (where each dWord is a Displacement from Top of File,
  to the IMAGE_ARCHIVE_MEMBER_HEADER of the .Obj File concerned with the
  matching Name found in the next "Symbols Table" String):
;;
;;
    Push esi
        Mov esi AmountOfOffsets1 | While B$esi <> 0 | movsb | End_While
    Pop esi

    Mov ecx D$NumberOfSymbols

L0: lodsd
    Push esi
    ; Eax: Displacement to the SymOffset array Data. If it is 0, jmp over.
        ..If eax = 0
            Mov esi {B$ '&NULL ; Null Pointer. Does not point to any Object in this file.' EOS}

        ..Else
            Mov esi D$LibFileMemory | bswap eax | add esi eax

            .If B$esi = '/'
L1:             Push ecx
                    Call GetLongNamesPointer
                    Call CopyCoffIndice | inc esi
                    If D$LongNamesBase <> 0
                       ;                     Mov esi D$LongNamesBase
                        Call CheckLongNameDisplacement | On eax = &FALSE, jmp L1>
                        Call WriteLongName esi
                    Else
L1:                     Call CopySymbolName
                    End_If
                Pop ecx

            .Else
                Call CopyCoffIndice | Call CopySymbolName

            .End_If

            Mov esi IMAGE_ARCHIVE_MEMBER_HEADERstring

        ..End_If

        While B$esi <> 0 | movsb | End_While
    Pop esi
    Mov W$edi CRLF, B$edi+2 SPC | add edi 3 | dec ecx | jnz L0<<
ret


CopySymbolName:
  ; Copy the ARCHIVE_MEMBER_HEADER Name:
  Push ecx
  Mov ecx 1 ; It is used as a counter. Starting with 1 (The 1st byte)


    Push esi
        Mov esi {B$ 'Short Name: ' EOS}
        While B$esi <> 0 | movsb | End_While
    Pop esi

    Push esi

    While B$esi > SPC
        movsb
            ; Short Names always ends with an "/ ". Or sometimes with an "/"
            If W$esi+1 = '/ '
                movsW
                jmp L2>
            Else_If ecx = 16; Did we reached the 16th byte ? ON that case we may still have "/" as the last char
                jmp L2>
            End_If
        inc ecx
    End_While


L2:     On B$edi-1 = '/', dec edi ; Clean Up the last "/" char

    Pop esi

    ; We need to retrieve the Function Name in case for the Import Header (The non common Coff format)
    ; To avoid inserting the ".directive" strings etc. We need only the real function name for this type
    ; of structure.

    ..If W$esi+58+2 = &IMAGE_FILE_MACHINE_UNKNOWN
        .If W$esi+58+2+2 = &IMPORT_OBJECT_HDR_SIG2

            Push esi
                Mov esi {B$ ' Function Name: ' EOS}
                While B$esi <> 0 | movsb | End_While
            Pop esi

            lea esi D$esi+60+20
            On B$esi = '/', inc esi
            While B$esi > '/' | LookUpValidNameChars | End_While

        .End_If
    ..End_If

   ; If this is a common Coff Format, we compute the Function name it follows it here
    Pop ecx
ret


CopyObjSymbolName:
  ; Copy the ARCHIVE_MEMBER_HEADER Name:
  Push ecx
  Mov ecx 1 ; It is used as a counter. Starting with 1 (The 1st byte)

    Push esi
        Mov esi {B$ 'Short Name: ' EOS}
        While B$esi <> 0 | movsb | End_While
    Pop esi

    Push esi

    While B$esi > SPC
        movsb
            ; Short Names always ends with an "/ ". Or sometimes with an "/"
            If W$esi+1 = '/ '
                movsW
                jmp L2>
            Else_If ecx = 16; Did we reached the 16th byte ? ON that case we may still have "/" as the last char
                jmp L2>
            End_If
        inc ecx
    End_While


L2:     On B$edi-1 = '/', dec edi ; Clean Up the last "/" char

    Pop esi
    Mov D$ExportedLibrary 0
    ; We need to retrieve the Function Name in case for the Import Header (The non common Coff format)
    ; To avoid inserting the ".directive" strings etc. We need only the real function name for this type
    ; of structure.

    ..If W$esi+58+2 = &IMAGE_FILE_MACHINE_UNKNOWN
        .If W$esi+58+2+2 = &IMPORT_OBJECT_HDR_SIG2

        Mov D$ExportedLibrary edi

        Push esi
            Mov esi {B$ ' Function Name: ' EOS}
            While B$esi <> 0 | movsb | End_While
        Pop esi

        lea esi D$esi+60+20
        While B$esi >= SPC | LookUpValidNameChars | End_While

        .End_If

    ..End_If

   ; If this is a common Coff Format, we compute the Function Name it follows it here
    Pop ecx

ret
____________________________________________________________________________________________

[StringsArrayComments1: "]

; Array of null terminated strings that are the name of
;the Symbols List (The value of the array is defined by PublicSymbols member).

[Symname1:
 " EOS]

[Symname1.Data: B$   "Symname1.Data"
 SymName1Counter: "000001: B$   '" EOS]

ShowSymbolsTable1:
;;
;;

  Show the Symbols Table, that is a simple zero-ended string array. Each String
  is a Symbol used, in order, by the .Obj File, which the IMAGE_ARCHIVE_MEMBER_HEADER
  is pointed to, by the above Offset Table:
;;
;;
    Push esi
        Mov esi StringsArrayComments1
        While B$esi <> 0 | movsb | End_While
    Pop esi

    Mov D$SymName1Counter '0000', W$SymName1Counter+4 '01', ecx D$NumberOfSymbols

L0: Push esi
        Mov esi Symname1.Data
        While B$esi <> 0 | movsb | End_While
    Pop esi
    While B$esi <> 0 | LookUpValidNameChars | End_While | inc esi
    Mov D$edi "', 0", W$edi+4 CRLF, B$edi+6 SPC | add edi 7
    lea ebx D$SymName1Counter+5
    inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx
        inc B$ebx
    End_While
    loop L0<
    sub edi 3
    Mov B$edi ']', D$edi+1 CRLF2 | add edi 5
ret
____________________________________________________________________________________________

WriteHeaderMember1:
    Call ShowAmountOfSymbols1
    If D$NumberOfSymbols > 0
        Call ShowOffsetTable1
        Call ShowSymbolsTable1
    End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
;;

                              Write the Second Library Header
;;
;;
____________________________________________________________________________________________

[SecondLibHeaderComment: ";;
_____________________________________________________

    Secondary Linker Header - Names Linker Member
_____________________________________________________
_____________________________________________________
;;
;;
; This is the IMAGE_ARCHIVE_MEMBER_HEADER Structure.

" EOS]
____________________________________________________________________________________________

WriteSecondLibHeaderComment:
    Push esi
        Mov esi SecondLibHeaderComment
        While B$esi <> 0 | movsb | End_While
    Pop esi
ret
____________________________________________________________________________________________

[AmountOfSymbols2: "; Total amount of Objects files.

[PublicSymbols2: D$ " EOS]

ShowAmountOfSymbols2:
;;
;;
  Show the Amount of Symbols (a dWord stored the other way round):
;;
;;
    Push esi
        Mov esi AmountOfSymbols2 | While B$esi <> 0 | movsb | End_While
    Pop esi
    lodsd | Mov D$NumberOfSymbols eax | Call WriteEax

    Mov B$edi ']', D$edi+1 CRLF2 | add edi 5
ret
____________________________________________________________________________________________

[AmountOfOffsets2: "; Amount of Offsets of the Object Files Found.

[SymOffset2:
 " EOS]

ShowOffsetTable2:  ; 'ShowOffsetTable1'
;;

;;
  Show the Offset Table (where each dWord is a Displacement from Top of File,
  to the IMAGE_ARCHIVE_MEMBER_HEADER of the .Obj File concerned with the
  matching Name found in the next "Symbols Table" String):
;;
;;
    Push esi
        Mov esi AmountOfOffsets2 | While B$esi <> 0 | movsb | End_While
    Pop esi

    Mov ecx D$NumberOfSymbols

L0: lodsd
    Push esi
  ; Eax: Displacement to the SymOffset array Data. If it is 0, jmp over.

        ..If eax = 0
            Mov esi {B$ '&NULL ; Null Pointer. Does not point to any Object in this file.' EOS}

        ..Else
            Mov esi D$LibFileMemory | add esi eax
            .If B$esi = '/'
                Call GetLongNamesPointer
                Call CopyCoffIndice | inc esi
                If D$LongNamesBase <> 0
                    Call CheckLongNameDisplacement | On eax = &FALSE, jmp L1>
                    Call WriteLongName esi
                Else
L1:                 Call CopySymbolName
                End_If

            .Else
                Call CopyCoffIndice | Call CopySymbolName

            .End_If

            Mov esi IMAGE_ARCHIVE_MEMBER_HEADERstring

        ..End_If

        While B$esi <> 0 | movsb | End_While

    Pop esi
    Mov W$edi CRLF, B$edi+2 SPC | add edi 3 | dec ecx | jnz L0<< ;loop L0<
    Mov B$edi ']', D$edi+1 CRLF2 | add edi 5
ret
____________________________________________________________________________________________

[AmountOfSymbols3: "; Amount of Public Symbols on the file.

[PublicSymbols3: D$ " EOS]

ShowAmountOfSymbols2Bis:
;;
;;
  Show the Amount of Members (a dWord stored the other way round), for Second Header:
;;
;;
    Push esi
        Mov esi AmountOfSymbols3 | While B$esi <> 0 | movsb | End_While
    Pop esi
    lodsd | Mov D$NumberOfSymbols eax | Call WriteEax

    Mov B$edi ']', D$edi+1 CRLF2 | add edi 5
ret
____________________________________________________________________________________________

[IndexString2: "; Array of the Word data, representing the Index of the exported functions of the Symbols List.
; It points to the object files inside the lib.
; (The amount of the elements in the array is defined by the PublicSymbols2 member)

[SymIndex:
 " EOS]

[SymIndex2.Data: "SymIndex.Data"
 SymIndex2Counter: "000001:  W$    " EOS]


ShowIndexTable2:
;;
;;

  Flow of reversed Words. They are indexes to point out what String goes with what Obj File:
;;
;;
    Push esi
        Mov esi IndexString2 | While B$esi <> 0 | movsb | End_While
    Pop esi

    Mov ecx D$NumberOfSymbols, D$SymIndex2Counter '0000', W$SymIndex2Counter+4 '01'

L0: Push esi
        Mov esi SymIndex2.Data
        While B$esi <> 0 | movsb | End_While
    Pop esi
  ; This one is not reversed:
    lodsw | and eax 0FFFF | Call WriteEax



    lea ebx D$SymIndex2Counter+5
    inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx
        inc B$ebx
    End_While

    Mov W$edi CRLF, B$edi+2 SPC | add edi 3 | loop L0<

    sub edi 3
ret
____________________________________________________________________________________________

[StringsArrayComments2: "]

; Array of null terminated strings that are the name of
;the Symbols List (The value of the array is defined by PublicSymbols member).

[Symname2:
 " EOS]

[Symname2.Data: B$   "Symname2.Data"
 SymName2Counter: "000001: B$   '" EOS]

ShowSymbolsTable2:
;;
;;

  Show the Symbols Table, that is a simple zero-ended string array. Each String
  is a Symbol used, in order, by the .Obj File, which the IMAGE_ARCHIVE_MEMBER_HEADER
  is pointed to, by the above Offset Table:

;;
;;
    Push esi
        Mov esi StringsArrayComments2
        While B$esi <> 0 | movsb | End_While
    Pop esi

    Mov D$SymName2Counter '0000', W$SymName2Counter+4 '01', ecx D$NumberOfSymbols

L0: Push esi
        Mov esi Symname2.Data
        While B$esi <> 0 | movsb | End_While
    Pop esi
    While B$esi <> 0 | LookUpValidNameChars | End_While | inc esi
    Mov D$edi "', 0", W$edi+4 CRLF, B$edi+6 SPC | add edi 7
    lea ebx D$SymName2Counter+5
    inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx
        inc B$ebx
    End_While
    loop L0<
    sub edi 3
    Mov B$edi ']', D$edi+1 CRLF2 | add edi 5
ret
____________________________________________________________________________________________

WriteHeaderMember2:
    Call ShowAmountOfSymbols2
    On D$NumberOfSymbols > 0, Call ShowOffsetTable2
    Call ShowAmountOfSymbols2Bis
    If D$NumberOfSymbols > 0
        Call ShowIndexTable2
        Call ShowSymbolsTable2
    End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
;;
   Write the third Library Header
;;
;;
____________________________________________________________________________________________


[ThirdLibHeaderComment: ";;
_____________________________________________________

    Tertiary Linker Header - Names Linker Member
_____________________________________________________
_____________________________________________________
;;
;;
; This is the IMAGE_ARCHIVE_MEMBER_HEADER Structure.

" EOS]
____________________________________________________________________________________________

WriteThirdLibHeaderComment:
    Push esi
        Mov esi ThirdLibHeaderComment
        While B$esi <> 0 | movsb | End_While
    Pop esi
ret
____________________________________________________________________________________________

[LongNamesTableComment3:
"; Amount of Null Terminated String related to the Objects with LongNames.

[" EOS]

[LongObjectNames: "LongObjectNames.data"

LongObjectNamesCounter: "000001: B$ '" EOS]
____________________________________________________________________________________________

WriteLongNameTableComment3:
    Mov D$LongObjectNamesCounter '0000', W$LongObjectNamesCounter+4 '01'

    Push esi
        Mov esi LongNamesTableComment3
        While B$esi <> 0 | movsb | End_While
    Pop esi
ret

ShowLongNamesTable3:
    Mov edx esi | add edx D$MemberSize

    ..While esi < edx
        Push esi
            Mov esi LongObjectNames
            While B$esi <> 0 | movsb | End_While

            lea ebx D$LongObjectNamesCounter+5
            inc B$ebx
            While B$ebx > '9'
                Mov B$ebx '0' | dec ebx | inc B$ebx
            End_While
        Pop esi

        While B$esi >= SPC | LookUpValidNameChars | End_While | inc esi
        While B$esi < SPC | inc esi | End_While
        Mov D$edi "', 0", W$edi+4 CRLF | add edi 6
    ..End_While

    sub edi 2 | Mov B$edi ']', D$edi+1 CRLF2 | add edi 5
ret


WriteHeaderMember3:
    If D$MemberSize > 0
        Call WriteLongNameTableComment3
        Call ShowLongNamesTable3
    End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; Coff

[CoffComment: ";;
_____________________________________________________

Coff File Headers:
_____________________________________________________
_____________________________________________________
;;
;;
" EOS]

____________________________________________________________________________________________
____________________________________________________________________________________________
____________________________________________________________________________________________
____________________________________________________________________________________________
____________________________________________________________________________________________
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
;;
                                      Lib jobs
;;
;;
[LibSymbolsMap: D$ ?]

[SymbolsNumber: D$ ?
 CoffIndice: D$ ?]

GetLibCode:
    Mov D$SymbolsNumber 0, D$LibNumberOfBytes 0
    Call GetLibSymbolsNumber | On D$SymbolsNumber = 0, jmp L9>>

    Call CreateLibBuffers
    Move D$LibDisassemblyPtr D$LibDisassembly, D$LibBytesCopyPtr D$LibBytesCopy
    Mov D$CoffIndice 0

L0: Call GetCoffBase D$CoffIndice
    .If W$esi <> 0-1
        If W$esi+2 <> 0-1
            Call ScanCoff
        End_If
    .End_If
    inc D$CoffIndice | Mov eax D$CoffIndice | cmp eax D$SymbolsNumber | jb L0<<

    Call DecodeLib
L9: ret


GetLibSymbolsNumber:
    Mov esi D$LibFileMemory

  ; Number of SYMBOLs in ecx:
    Mov eax D$esi+COFF_HEADER_SIZE+ARCH_TAG | bswap eax
    Mov D$SymbolsNumber eax
ret

; esi point to Coff Base (014C):

[CoffHeaderBase: D$ ?]

[LibNumberOfBytes: D$ ?]

ScanCoff:
    ..If W$esi <> 0-1 ; 'PeHeader'
        Mov W$esi 0-1
        Mov D$CoffHeaderBase esi
      ; Number of Coff Sections in ecx:
        movzx ecx W$esi+2
        add esi 014 ; COFF_HEADER_SIZE ; example: '.text'

L0:     and D$esi+SECTION_FLAG &IMAGE_SCN_CNT_CODE

        .If D$esi+SECTION_FLAG = &IMAGE_SCN_CNT_CODE
            Push ecx, esi
                Mov ecx D$esi+SECTION_FILESIZE, esi D$esi+SECTION_FILEPOINTER

                If ecx <> 0
                    add esi D$CoffHeaderBase
                    Mov edi D$LibBytesCopyPtr
                    Mov D$edi ecx | add edi 4
                    add D$LibNumberOfBytes ecx
                    rep movsb
                    Mov D$LibBytesCopyPtr edi
                End_If
            Pop esi, ecx
        .End_If

        add esi SECTIONHEADERSIZE | loop L0<
    ..End_If
ret
;;
;;
B$ '.text' EOS, 0, 0
AppTrueCodeSize: D$   0     ; true size of code in file
AppCodeRVAoffset: D$   0    ; RVA offset (aligned on 01000 boundary)
AppFileSizeOfCode: D$   0   ; file aligned size of code (0200 aligned)
AppStartOfCode: D$   00     ; pointer to code (true first code in file - not entry point-)
D$   00                     ; dummy reloc ptr
D$   00                     ; dummy line number ptr
W$   00                     ; dummy reloc number
W$   00                     ; dummy number of line number
CodeCharacteristics:
D$   0_60000020             ; characteristics (readable, runable, code)
;;
;;
[LibBytesCopy: D$ ?
 LibBytesCopyPtr: D$ ?
 LibDisassembly: D$ ?
 LibDisassemblyPtr: D$ ?]

CreateLibBuffers:

    Call VirtualAlloc LibBytesCopy,
                      D$LibFileLength

  ; VirtualAlloc LibSymbolsMap, D$LibFileLength

    Mov ecx D$LibFileLength | shl ecx 5

    Call VirtualAlloc LibDisassembly,
                      ecx

    Move D$LibDisassemblyPtr D$LibDisassembly

    add D$LibDisassembly 4

ret


Proc GetCoffBase:
    Argument @Indice

        Mov esi D$LibFileMemory, eax D@Indice
        Mov esi D$esi+eax*4+COFF_HEADER_SIZE+ARCH_TAG+4
        bswap esi | add esi D$LibFileMemory

      ; esi now points to a COFF Object:
        add esi COFF_HEADER_SIZE

      ; esi now point, typically, to 014C
EndP



Proc SetStringPointer:
    Arguments @Base, @Offset, @String
    Uses edi
      ; Write the Pointer to String directly on the Code "0, 0, 0, 0":
        Mov edi D@Base
        add edi D$esi+SECTION_FILEPOINTER
        add edi D@Offset
        Move D$edi D@String
EndP

____________________________________________________________________________________________



__________________________________________________________________________________________________________________
__________________________________________________________________________________________________________________
;;
;;

----------------------------------------------------------------------
        Data used to build the Main dialog and Tab controls.
----------------------------------------------------------------------
----------------------------------------------------------------------

;;

__________________________________________________________________________________________________________________

;;
Proc WriteSectionHeaderSymbolConstantIndex:
    uses eax, ecx, ebx, esi

    Call InitSymbolIndexRecord
    Call WriteObjIndice

    ; Fix the label to show in Caps and replace the '." with an "_"
    ; The 1st char in "Obj000000." is in Caps, so we don't need to overwrite the "O" char

    Push edi
    Mov B$edi-1 '_', W$edi-9 'BJ'
    Pop edi

    Push esi
    zCopy {B$ 'SYMBOLINDEX' EOS}
    Pop esi

    Mov ecx D$esi

    ; Note to Ren�: Replacing this with a hex to decimal ascii string is better,
    ; but i couldn't make it be on the same style as SymbolTableIndex

    While ecx <> 0
        Call IncrementSymbolIndexRecord
        dec ecx
    End_While

    zCopy SymbolTableIndex

    ; restore the SymbolTableIndex
    Call InitSymbolIndexRecord

EndP
;;

;;
; LV_ITEM Structure

[buffer2: B$ ? # &MAX_PATH]

[lvi:
 lvi.imask: D$ &LVIF_TEXT
 lvi.iItem: D$ 0
 lvi.iSubItem: D$ 0
 lvi.state: D$ &LVIS_FOCUSED
 lvi.stateMask: D$ 0
 lvi.pszText: D$ buffer2
 lvi.cchTextMax: D$ &MAX_PATH
 lvi.iImage: D$ 0
 lvi.lParam: D$ 0
 lvi.iIndent: D$ 0]

;&LVCF_ORDER

[LvOffsetCOFF: D$ 0]
;GetCoffIMAGE_FILE_HEADER

[DecimalBuffer: B$ ? # 16];B$ 020 #10 0]
[HexaDecimalBuffer: B$ ? # 16]

[ObjSymbolsNumber: D$ ?]

[CoffMachineType: D$ ?]

[ObjFileNameType: D$ ?]


; The decimal and hexadecimal convertion routines comes from MouseHintDrawWindow

Proc AddListviewItem:
    Arguments @h2List
    pushad


    ; Calculate and Display Index Value
    Mov edi buffer2
    Mov esi LibObjIndice
    While B$esi <> 0 | movsb | End_While

    sub edi 6
    Mov D$lvi.pszText edi
    Mov D$lvi.iSubItem, 0
    Call 'USER32.SendMessageA', D@h2list, &LVM_INSERTITEM, 0, lvi

    ; Calculate and Display FileName

    Move D$lvi.pszText ObjectName
    inc D$lvi.iSubItem
    Call 'USER32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

    ; Calculate and Display Extension

    If D$UseObjExtension = 0
        Mov D$lvi.pszText {B$ "No extension" EOS}
    Else
        Move D$lvi.pszText ObjExtensionStr
    End_If
    inc D$lvi.iSubItem
    Call 'USER32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi


    ; Display File Name Type
    ; Definition of values in Write_Obj_IMAGE_ARCHIVE_MEMBER_HEADER

    If D$ObjFileNameType = 1
        Mov D$lvi.pszText {B$ "Long Name" EOS}
    Else
        Mov D$lvi.pszText {B$ "Short Name" EOS}
    End_If
    inc D$lvi.iSubItem
    Call 'USER32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi


    ; Display the Path of the Object file
    If D$UseObjPath = 0
        Mov D$lvi.pszText {B$ "Not used" EOS}
    Else
        Mov D$lvi.pszText ObjPathStr
    End_If

    inc D$lvi.iSubItem
    Call 'USER32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

   ; Calculate and Display Offset (Convert hexa dword to string)

    Mov edi HexaDecimalBuffer
    DwordToHex D$LvOffsetCOFF
    Mov B$edi 0

    Move D$lvi.pszText HexaDecimalBuffer
    inc D$lvi.iSubItem
    Call 'USER32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

    ; Calculate and Display Size (Convert Hexa do Decimal String)

    Mov esi MemberSize, ecx 4
    Call toUDword
    Mov esi edi, edi DecimalBuffer
    Do | movsb | LoopUntil B$esi-1 = 0

    Move D$lvi.pszText DecimalBuffer
    inc D$lvi.iSubItem
    Call 'USER32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

    ; Display Object Type
    If D$ExportedLibrary = 0
        Mov D$lvi.pszText {B$ "Runtime Object" EOS}
    Else
        Mov D$lvi.pszText {B$ "Exported Object" EOS}
    End_If

    inc D$lvi.iSubItem
    Call 'USER32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

    ; Display Function Name used in Exported Library
    If D$ExportedLibrary = 0
        Mov D$lvi.pszText {B$ "Not used" EOS}
    Else
        Move D$lvi.pszText ExportedLibraryStr
    End_If

    inc D$lvi.iSubItem
    Call 'USER32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi


    ; Display the Machine Type

    Mov D$buffer2 edi ; eax holds the address of the initial Equate String

    Call Write_IMAGE_FILE_MACHINE D$CoffMachineType
    Mov B$edi 0 ; Fix the end of edi in all cases. Force Null Terminated String
    Mov eax D$buffer2

    If B$eax = '&'
        inc D$buffer2
        Move D$lvi.pszText D$buffer2
    Else
        Mov D$lvi.pszText {B$ "Unknown or Corrupted Machine Type" EOS}
    End_If
    inc D$lvi.iSubItem
    Call 'USER32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

    ;  Calculate and Display Number of Sections (Convert Hexa do Decimal String)

    .If D$ExportedLibrary = 0
        Mov esi ObjNumberOfSections, ecx 4
        Call toUDword
        Mov esi edi, edi DecimalBuffer
        Do | movsb | LoopUntil B$esi-1 = 0

        Mov D$lvi.pszText DecimalBuffer
    .Else
        Mov D$lvi.pszText {B$ "No Sections" EOS}
    .End_If

    inc D$lvi.iSubItem
    Call 'USER32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

    ; Display Time and Date Stamp
    ; This came from WriteImportObjHdrTimeDateStamp
    ;TimeDateStringtoDword
    Call Concatenation szDateString, szTimeString, buffer2
    Move D$lvi.pszText buffer2
    inc D$lvi.iSubItem
    Call 'USER32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi


    ;  Calculate and Display Number of Symbols (Convert Hexa do Decimal String) CoffSymbolsNumber
    .If D$ExportedLibrary = 0
        Mov esi ObjSymbolsNumber, ecx 4
        Call toUDword
        Mov esi edi, edi DecimalBuffer
        Do | movsb | Loop_Until B$esi-1 = 0

        Mov D$lvi.pszText DecimalBuffer
    .Else
        Mov D$lvi.pszText {B$ "No Symbols" EOS}
    .End_If

    inc D$lvi.iSubItem
    Call 'USER32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

    ; Show Optional Header Options (False if it don't use Optional Header, True otherwise)

    If D$SizeOfOptionalHeaderInObj = 0
        Mov D$lvi.pszText {B$ "False" EOS}
    Else
        Mov D$lvi.pszText {B$ "True" EOS}
    End_If
    inc D$lvi.iSubItem
    Call 'USER32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

    ; Clear the Buffer before exit

    Mov ecx &MAX_PATH
    Mov edi 0
    L0:
        Mov B$buffer2+edi 0
        inc edi
    Loop L0<

    popad

EndP


___________________________________________

; Used Macros


; Amount of Columns of the ListView
[Lib_LVTotalCol 14]

; String Data

[Header1: B$ 'Index' EOS]
[Header2: B$ 'File Name' EOS]
[Header3: B$ 'Extension' EOS]
[Header4: B$ 'FileName Type' EOS]
[Header5: B$ 'Path' EOS]
[Header6: B$ 'Offset' EOS]
[Header7: B$ 'Size' EOS]
[Header8: B$ 'Object Type' EOS]
[Header9: B$ 'Exported Function' EOS]
[Header10: B$ 'Machine Type' EOS]
[Header11: B$ 'Sections' EOS]
[Header12: B$ 'Time and Date' EOS]
[Header13: B$ 'Symbols' EOS]
[Header14: B$ 'Optional Header' EOS]

; Our used Constants to identify what field is what
[LVIEW_INDEX 0]
[LVIEW_FILENAME 01]
[LVIEW_EXTENSION 02]
[LVIEW_NAMETYPE 03]
[LVIEW_PATH 04]
[LVIEW_OFFSET 05]
[LVIEW_SIZE 06]
[LVIEW_OBJTYPE 07]
[LVIEW_EXPORTEDFUNCTION 08]
[LVIEW_MACHINE 09]
[LVIEW_SECTION 10]
[LVIEW_TIME 11]
[LVIEW_SYMBOL 12]
[LVIEW_OPTIONAL 13]

[H.Header: D$ ?]
[H.List: D$ ?]

; LV_COLUMN Structure

[lvc:
 lvc.imask: D$ 0
 lvc.fmt: D$ &LVCFMT_LEFT
 lvc.lx: D$ 80  ; columnwidth
 lvc.pszText: D$ 0
 lvc.cchTextMax: D$ 0
 lvc.iSubItem: D$ 0
 lvc.iImage: D$ 0
 lvc.iOrder: D$ 0]

Proc SetupListview:
    Arguments @h2List

    ; /*Listview setup */
    Mov D$lvc.imask, &LVCF_TEXT+&LVCF_WIDTH
    Mov D$lvc.pszText, Header1
    Call 'USER32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_INDEX, lvc
    or D$lvc.imask, &LVCF_FMT
    Mov D$lvc.pszText, Header2
    Call 'USER32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_FILENAME, lvc
    or D$lvc.imask, &LVCF_FMT
    Mov D$lvc.pszText, Header3
    Call 'USER32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_EXTENSION, lvc
    or D$lvc.imask, &LVCF_FMT
    Mov D$lvc.pszText, Header4
    Call 'USER32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_NAMETYPE, lvc
    or D$lvc.imask, &LVCF_FMT
    Mov D$lvc.pszText, Header5
    Call 'USER32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_PATH, lvc
    or D$lvc.imask, &LVCF_FMT
    Mov D$lvc.pszText, Header6
    Call 'USER32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_OFFSET, lvc
    or D$lvc.imask, &LVCF_FMT
    Mov D$lvc.pszText, Header7
    Call 'USER32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_SIZE, lvc
    or D$lvc.imask, &LVCF_FMT
    Mov D$lvc.pszText, Header8
    Call 'USER32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_OBJTYPE, lvc
    or D$lvc.imask, &LVCF_FMT
    Mov D$lvc.pszText, Header9
    Call 'USER32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_EXPORTEDFUNCTION, lvc
    or D$lvc.imask, &LVCF_FMT
    Mov D$lvc.pszText, Header10
    Call 'USER32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_MACHINE, lvc
    or D$lvc.imask, &LVCF_FMT
    Mov D$lvc.pszText, Header11
    Call 'USER32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_SECTION, lvc
    or D$lvc.imask, &LVCF_FMT
    Mov D$lvc.pszText, Header12
    Call 'USER32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_TIME, lvc
    or D$lvc.imask, &LVCF_FMT
    Mov D$lvc.pszText, Header13
    Call 'USER32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_SYMBOL, lvc
    or D$lvc.imask, &LVCF_FMT
    Mov D$lvc.pszText, Header14
    Call 'USER32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_OPTIONAL, lvc

    ;/* these 5 lines create a FLAT columnheader */
    Call 'USER32.SendMessageA', D@h2List, &LVM_GETHEADER__&LVM_ENSUREVISIBLE__&LVM_SETCOLUMNORDERARRAY, 0, 0 ;// get handle to header;&LVM_GETHEADER, 0, 0 ;// get handle to header
    Mov D$H.Header, eax ;// preserve header handle
    Call 'USER32.GetWindowLongA', D$H.Header, &GWL_STYLE ;// get current window styles
    xor eax, &HDS_BUTTONS
    Call 'USER32.SetWindowLongA', D$H.Header, &GWL_STYLE, eax ;// set the new header styles

    ;/* Setup extended styles like gridlines, back-foregroundcolors */
    Call 'USER32.SendMessageA', D@h2List, &LVM_SETEXTENDEDLISTVIEWSTYLE, 0, &LVS_EX_FULLROWSELECT__&LVS_EX_HEADERDRAGDROP__&LVS_EX_SUBITEMIMAGES__&LVS_EX_GRIDLINES__&LVS_EX_FLATSB
    Call 'USER32.SendMessageA', D@h2List, &LVM_SETTEXTCOLOR, 0, {RGB 0_BA 0_30 0_26 0_00}
    Call 'USER32.SendMessageA', D@h2List, &LVM_SETBKCOLOR, 0, {RGB 0_FF 0_FF 0FF 0_00}
    Call 'USER32.SendMessageA', D@h2List, &LVM_SETTEXTBKCOLOR, 0, {RGB 0_F0 0_F7 0_A6 0_00}

EndP

___________________________________________________________________________________________________________

; ----------------------------------------------
; 2nd Dialog Tab Frame Procedure
; This is to show the Object Listing
; ----------------------------------------------


Proc Tab2Proc:
    Arguments @hWin, @uMsg, @wParam, @lParam

    pushad

    If D@uMsg = &WM_INITDIALOG
        Call 'USER32.SendMessageW' D@lParam, &EM_SETSEL, 0-1, 0
        Call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | Mov eax D$H.DialogsBackGroundBrush | ExitP
    Else_If D@uMsg = &WM_NOTIFY
        Call LibScanDialog_OnNotify D@hWin, D@lParam
    Else_If D@uMsg = &WM_CTLCOLOREDIT
        Call 'GDI32.SetBkColor' D@wParam, D$DialogsBackColor
        popad | Mov eax D$H.DialogsBackGroundBrush | ExitP
    Else
        popad | Mov eax &FALSE | ExitP
    End_If

    popad | Mov eax &TRUE
EndP


; ----------------------------------------------
; 1st Dialog Tab Frame Procedure
; This is where the MZ Header dialog is used
; ----------------------------------------------


Proc Tab1Proc:
    Arguments @hWin, @uMsg, @wParam, @lParam

    pushad

    .If D@uMsg = &WM_INITDIALOG
        Call 'USER32.SendMessageW' D@lParam, &EM_SETSEL, 0-1, 0
        Call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | Mov eax D$H.DialogsBackGroundBrush | ExitP
    .Else_If D@uMsg = &WM_NOTIFY
        Call LibScanDialog_OnNotify D@hWin, D@lParam
    .Else_If D@uMsg = &WM_CTLCOLOREDIT
        Call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | Mov eax D$H.DialogsBackGroundBrush | ExitP
    .Else
        popad | Mov eax &FALSE | ExitP
    .End_If

    popad | Mov eax &TRUE
EndP




______________________________________________________________________________________________

; constants for the library dialogs

; Dialog Controls

[IDD_MAINLIB 20] ; Main Dialog where it will hold the tabs.

; Internal controls for this dialog

[IDC_LIBFILESIZE 4] ; The Edit Control where the path name is displayed
[IDC_OPENLIBFILE 5] ; The Edit Control where the path name is displayed
[IDC_TABCTRL 38]  ; Main Tab Control Resource.
[IDC_LOADLIB 3]     ; The "Open" Button.

[IDD_TAB1 21] ; Id of the 1st Dialog "Library Structure"

; Internal controls for this dialog

[IDC_LIB_TARGET 11] ; 1 59 199 188 . ES_MULTI ES_AUTOVS ES_AUTOH WS_VS WS_HSC WS_TAB WS_BO
[IDC_LIB_SOURCE 10] ; 202 57 178 188. ES_MULTI ES_AUTOVS WS_VS WS_BO


[IDD_TAB2 22]  ; Id of the 2nd Dialog "Object Listing"
; Internal controls for this dialog
[IDC_LVIEW 30] ; Id for the listview control

[IDD_TAB3 23] ; Id of the 3rd Dialog other Header
[IDD_TAB4 24] ; Id of the 4th Dialog Section Images


; Menu Equates

[M03_Menu  4000                  M03_Open  4001                  M03_Close  4002
 M03_Save_Report  4003           M03_Exit  4004                  M03_Single_Object_File  4005
 M03_Runtime_Objects_Only  4006  M03_Exported_Objects_Only  4007 M03_All_Object_Files  4008
 M03_Build_Dis_File  4009        M03_Next_Tab  4010              M03_Previous_Tab  4011
 M03_First_Tab  4012             M03_Last_Tab  4013              M03_Show_ToolBarText  4014
 M03_Hover_ToolBar  4015         M03_About  4016]

; Structures used

[NMHDR.hwndFromDis 0
 NMHDR.idFromDis 4
 NMHDR.codeDis 8]


[hInst: D$ ?]

[SortDecimal: D$ ?] ; Sort Buffer

[flag_TB_00: D$ ?]

; Tab Selection
[LibScanSelTabStruc 0]
[LibScanSelTabObj 1]
[LibScanSelTabDisasm 2]
[LibScanSelTabDisBuild 3]

[LibScanTab_ImageList: D$ ?]
; Tag Dialog 20

Proc ScanLibFile:
    Arguments @hwnd, @msg, @wParam, @lParam

     pushad

    ...If D@msg = &WM_COMMAND                  ; User action

        ..If_Or D@wParam = &IDCANCEL, D@wParam = M03_Exit  ; User clicks on upper right [X] or Exited through the menu
            Call LibScanCleanUp D@hwnd
            Call 'COMCTL32.ImageList_Destroy' D$LibScanDialog_ImageList
            Call 'COMCTL32.ImageList_Destroy' D$LibScanTab_ImageList
            Call 'USER32.EndDialog' D@hwnd 0
            Mov D$LibScanToolbarHandle 0
            Mov B$LibScanIsFileOpen &FALSE

        ..Else_If D@wParam = M03_Close
            Call LibScanCleanUp D@hwnd
            Mov B$LibScanIsFileOpen &FALSE
            Call LibScanDialog_EnableContinueMenu &FALSE
            jmp L5>>

        ..Else_If D@wParam = M03_Save_Report
            Call SaveLibFileAs D@hwnd, D$hLibReportEdit, D$hLibReportEditLength

        ..Else_If D@wParam = M03_Single_Object_File
            Call SaveOneObjectFile D@hwnd

        ..Else_If D@wParam = M03_Runtime_Objects_Only
            Call SaveAllObjectFile D@hwnd, EXPORT_RUNTIME_OBJECTS

        ..Else_If D@wParam = M03_Exported_Objects_Only
            Call SaveAllObjectFile D@hwnd, EXPORT_EXPORTED_OBJECTS

        ..Else_If D@wParam = M03_All_Object_Files
            Call SaveAllObjectFile D@hwnd, EXPORT_ALL_OBJECTS

        ..Else_If D@wParam = M03_Show_ToolBarText
            Call LibScanDialog_ToggleToolbarText D@hwnd

        ..Else_If D@wParam = M03_Hover_ToolBar
            Call LibScanDialog_HoverToolbarText D@hwnd

        ..Else_If D@wParam = M03_Next_Tab
            Call LibScanDialog_EnableContinuePrevTabMenu &TRUE
            Call 'USER32.SendMessageA' D$hTab &TCM_GETCURSEL 0 0
            .If eax <> LibScanSelTabDisBuild
                inc eax
                Call 'USER32.SendMessageA' D$hTab &TCM_SETCURSEL eax eax
                inc eax

                If eax <> D$SelTab
                    Push eax
                    Mov eax D$SelTab
                    Call 'USER32.ShowWindow' D$eax*4+H.TabDlg1 &SW_HIDE
                    Pop eax
                    Mov D$SelTab eax
                    Call 'USER32.ShowWindow' D$eax*4+H.TabDlg1 &SW_SHOWDEFAULT
                End_If
            .End_If

            If D$SelTab = LibScanSelTabDisBuild
                Call LibScanDialog_EnableContinueNextTabMenu &FALSE
            End_If

        ..Else_If D@wParam = M03_Previous_Tab

            Call 'USER32.SendMessageA' D$hTab &TCM_GETCURSEL 0 0
            .If eax <> LibScanSelTabStruc
                dec eax
                Call 'USER32.SendMessageA' D$hTab &TCM_SETCURSEL eax eax
                dec eax

                If eax <> D$SelTab
                    Push eax
                    Mov eax D$SelTab
                    Call 'USER32.ShowWindow' D$eax*4+H.TabDlg1 &SW_HIDE
                    Pop eax
                    Mov D$SelTab eax
                    Call 'USER32.ShowWindow' D$eax*4+H.TabDlg1 &SW_SHOWDEFAULT
                End_If
                Call LibScanDialog_EnableContinueNextTabMenu &TRUE
            .End_If

            If D$SelTab = LibScanSelTabStruc
                Call LibScanDialog_EnableContinuePrevTabMenu &FALSE
            End_If

        ..Else_If D@wParam = M03_First_Tab
L5:
                Call 'USER32.SendMessageA' D$hTab &TCM_SETCURSEL LibScanSelTabStruc LibScanSelTabStruc
                Mov eax LibScanSelTabStruc
                Mov D$SelTab LibScanSelTabObj

                If eax <> D$SelTab
                    Push eax
                    Mov eax D$SelTab
                    Call 'USER32.ShowWindow' D$eax*4+H.TabDlg1 &SW_HIDE
                    Pop eax
                    Mov D$SelTab eax
                    Call 'USER32.ShowWindow' D$eax*4+H.TabDlg1 &SW_SHOWDEFAULT
                End_If
                Call LibScanDialog_EnableContinueNextTabMenu &TRUE
                Call LibScanDialog_EnableContinuePrevTabMenu &FALSE

        ..Else_If D@wParam = M03_Last_Tab

                Call 'USER32.SendMessageA' D$hTab &TCM_SETCURSEL LibScanSelTabDisBuild LibScanSelTabDisBuild
                Mov ecx LibScanSelTabDisBuild
                Mov eax D$SelTab
                If eax = D$SelTab
                    Push eax
                    Mov eax D$SelTab
                    Call 'USER32.ShowWindow' D$eax*4+H.TabDlg1 &SW_HIDE
                    Pop eax
                    Mov eax LibScanSelTabDisBuild
                    Mov D$SelTab eax
                    Call 'USER32.ShowWindow' D$eax*4+H.TabDlg1 &SW_SHOWDEFAULT
                End_If
                Call LibScanDialog_EnableContinueNextTabMenu &FALSE
                Call LibScanDialog_EnableContinuePrevTabMenu &TRUE

        ..Else_If D@wParam = M03_Open

            Mov D$ChoosenLibFile 0
            Move D$OPENLIB@hwndOwner D@hwnd
            Move D$OPENLIB@hInstance D$H.Instance
            Move D$OPENLIB@lpstrFilter LibsFileFilter
            Call 'COMDLG32.GetOpenFileNameA' OPENLIB

            .If D$ChoosenLibFile <> 0
                Call LibScanCleanUp D@hwnd
                Call LibScanDialog_EnableContinueMenu &TRUE

                Call 'USER32.SendDlgItemMessageA' D@hwnd IDC_OPENLIBFILE &WM_SETTEXT 0 LibSaveFilter
                Call 'USER32.SendDlgItemMessageA' D@hwnd IDC_OPENLIBFILE &WM_GETTEXT 0 LibSaveFilter ; GET THE FILENAME

                ; The user is opening the file, Clean the list view.
                Call 'USER32.SendMessageA', D$H.List, &LVM_DELETEALLITEMS, 0, lvi

                Call OpenLibFile D@hwnd
                On D$LibFileLength = 0, jmp L9>> ; Exit when the file size is 0.

                Call LibSignatureCheck

                If D$ValidLib = UNKNOWN_LIB_FILE
                    ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
                    Call LibScanDialog_EnableContinueMenu &FALSE
                    Call 'USER32.DialogBoxParamA' D$H.Instance, IDD_LIBSCANWARNINGMSG, D@hwnd, LibScanWarning, &NULL
                    jmp L9>>
                End_If

                Call ParseIdentifiedLibs D@hwnd

                ; Below we can only remove after we build the parser for each one of these libraries.
                ; But we can leave this lines here, in the meanwhile
                If_Or D$ValidLib = DCU1_KILYX_OBJ_FILE, D$ValidLib = DCU2_KILYX_OBJ_FILE,
                      D$ValidLib = DCU3_KILYX_OBJ_FILE, D$ValidLib = DCU2_OBJ_FILE,
                      D$ValidLib = DCU3_OBJ_FILE, D$ValidLib = DCU4_OBJ_FILE,
                      D$ValidLib = DCU5_OBJ_FILE, D$ValidLib = DCU6_OBJ_FILE,
                      D$ValidLib = DCU7_OBJ_FILE, D$ValidLib = OMF_OBJ_FILE,
                      D$ValidLib = PDB_OBJ_FILE, D$ValidLib = DBG_OBJ_FILE
                    jmp L9>>
                End_If
            .End_If

            Call CoolControl_LVBeginSort ListViewLibSort, SortDecimal, D$H.List, 1
      ;  ..Else_If D@wParam = &IDHELP
      ;      Call Help, B_U_AsmName, DisassemblerHelp, ContextHlpMessage

        ..End_If


    ...Else_If D@msg = &WM_NOTIFY
        Call LibScanDialog_OnNotify D@hwnd, D@lParam

    ...Else_If D@msg = &WM_NCMOUSEMOVE
        ..If B$LibScanHoverTBText <> &MF_UNCHECKED
            .If D$flag_TB_00 = 0                    ;check flag toolbar
                Call 'USER32.ShowWindow' D$LibScanToolbarHandle &SW_SHOW
                Mov D$flag_TB_00 1                  ;set flag toolbar
            .End_If
        ..End_If
    ...Else_If D@msg = &WM_MOUSEMOVE
        ..If B$LibScanHoverTBText <> &MF_UNCHECKED
            .If D$flag_TB_00 = 1                    ;check flag toolbar
                Call 'USER32.ShowWindow' D$LibScanToolbarHandle &SW_HIDE
                Mov D$flag_TB_00 0                  ;set flag toolbar
            .End_If
        ..End_If

    ...Else_If D@msg = &WM_INITDIALOG

        Call 'USER32.GetMenu' D@hwnd | Mov D$LibScanMenuHandle eax

        ; Create the image list
        Call CoolControlTB_CreateImageList LibScanDialog_ImageList, IDB_LibScanEnableTB, IDB_LibScanDisableTB, 20, 20, &ILC_COLOR32+&ILC_MASK, LibScanToolButtonsNumber, LibScanToolButtonsNumber

        Call CoolControlTB_CreateImageList LibScanTab_ImageList, IDB_LibScanTABEnable, IDB_LibScanTABDisable, 20, 20, &ILC_COLOR32+&ILC_MASK, LibScanTabControlsNumber, LibScanTabControlsNumber

        ;Create the tabs
        Call CoolControlDlg_CreateTab D@hwnd, IDC_TABCTRL, hTab, LibScanTabControl

        ;Create the tab dialogs
        Call 'USER32.CreateDialogParamA' D$H.Instance IDD_TAB1 D$hTab Tab1Proc 0
        Mov D$H.TabDlg1 eax

        Call 'USER32.CreateDialogParamA' D$H.Instance IDD_TAB2 D$hTab Tab2Proc 0
        Mov D$H.TabDlg2 eax


        Call 'USER32.GetDlgItem' D$H.TabDlg2 IDC_LVIEW ; Get the ListView Control in the 2nd TAB resources, and
                                                    ; use it on the handle of the main window
        Mov D$H.List eax                              ; Now we return the result (found in eax), copying it to
                                                    ; the Handle of the Tab Control in the resource.
                                                    ; So, all we did was get the TAB Control and save it to the Tab handle.

        ; Create the toolbar
        Call CoolControlWin_CreateToolbar D@hwnd, LibScanToolbarHandle, LibScanToolbarButtons,
        LibScanToolButtonsNumber, LibScanToolTipsStrings, LSTBWin_Cmd

        ; Initialize the Menu state
        Call LibScanDialog_EnableContinueMenu &FALSE
        Call LibScanDialog_EnableContinuePrevTabMenu &FALSE

        ;/* Setup listview */
        Call SetupListview, D$H.List
        Call CoolControl_LVBeginSort ListViewLibSort, SortDecimal, D$H.List, 1
;;
;;
        Call 'USER32.CreateDialogParamA' D$H.Instance IDD_TAB3 D$hTab Tab3Proc 0
        Mov D$H.TabDlg3 eax
        Call 'USER32.CreateDialogParamA' D$H.Instance IDD_TAB4 D$hTab Tab4Proc 0
        Mov D$H.TabDlg4 eax
;;
;;

    ...Else_If D@msg = &WM_CLOSE
        Call LibScanCleanUp D@hwnd
        Call 'COMCTL32.ImageList_Destroy' D$LibScanDialog_ImageList
        Call 'COMCTL32.ImageList_Destroy' D$LibScanTab_ImageList
        Call 'USER32.EndDialog' D@hwnd &NULL
        Mov D$LibScanToolbarHandle 0
        Mov B$LibScanIsFileOpen &FALSE

    ...Else_If D@msg = &WM_CTLCOLOREDIT
        Call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | Mov eax D$H.DialogsBackGroundBrush | ExitP

    ...Else
        popad | Mov eax &FALSE | ExitP

    ...End_If

L9: popad | Mov eax &TRUE
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; This function handles all Notifications messages from all controls in the different Tabs
; It is mandatory to they be called inside each Tab Procedure (Tab1Proc, Tab2Proc ....)
; because we need they behave as a callback to handle the proper messages from each control Tab

Proc LibScanDialog_OnNotify:
    Arguments @hwnd, @Notification

    Mov ebx D@Notification
    Mov edx D$ebx+NMHDR.idFromDis
    Mov eax D$ebx+NMHDR.codeDis

    .If edx = IDC_TABCTRL
        If eax = &TCN_SELCHANGE
            Call CoolControlTabChange_OnNotify D@Notification, SelTab, D$hTab
        End_If

    .Else_If edx = IDC_LVIEW
        If eax = &LVN_COLUMNCLICK
            Call CoolControl_ListViewAlternateSort ListViewLibSort, D@Notification, SortDecimal, D$H.List, Lib_LVTotalCol
        Else_If eax = &NM_DBLCLK
            Call SaveOneObjectFile D@hwnd
        End_If

    .Else_If eax = &TTN_NEEDTEXT

        If_And edx >= D$LSTB01.idCommand, edx <= D$LSTB10.idCommand
            Call CoolControlTB_OnNotify D@hwnd, D@Notification, LibScanToolbarButtons, LibScanToolButtonsNumber, LibScanToolTipsStrings
        Else_If_And edx >= LibScanSelTabStruc, edx <= LibScanSelTabDisBuild
            Call CoolControlTabToolTip_OnNotify D@Notification, LibScanTabNumber, LibScanTabToolTipStrings
        End_If

    .End_If

EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc SaveOneObjectFile:
    Arguments @hwnd

    pushad

    Call 'USER32.SendMessageA' D$H.List, &LVM_GETNEXTITEM, 0-1, &LVNI_SELECTED
    If eax <> 0-1
        Call SaveSingleObjectFileAs D@hwnd, eax
    Else
        Call 'USER32.MessageBoxA' D@hwnd, {B$ "No field selected or empty fields.", D$ CRLF2, B$ "You must select one field in the ListControl to allow exporting the Selected Object file." EOS}, {B$ "Attention !" EOS}, &MB_ICONEXCLAMATION
    End_If

    popad
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________


[ObjSaveFileFilter: B$ 'Object File (*.obj)' EOS  '*.obj' EOS 0]
[ObjSaveFilter: D$ ? # &MAX_PATH]

Proc SaveSingleObjectFileAs:
    Arguments @hwnd, @SelectedItem
    pushad

    ; 1st we get the FileName

    Move D$lvi.iItem D@SelectedItem
    Mov D$lvi.iSubItem 1 ; [Header2: B$ 'File Name' EOS] ; string
    Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEM, D@SelectedItem, lvi
    ; the result is in szBuff0

    Mov edi ObjSaveFilter
    Push esi | ZCopy szBuff0 | Pop esi
    Mov B$edi 0

    Mov D$ChoosenLibFile 0
    Move D$SAVELIB.lpstrFile ObjSaveFilter
    Move D$SAVELIB.hwndOwner D@hwnd
    Move D$SAVELIB.hInstance D$H.Instance
    Move D$SAVELIB.lpstrFilter ObjSaveFileFilter
    Call 'Comdlg32.GetSaveFileNameA' SAVELIB

    ..If eax <> 0
        .If D$ChoosenLibFile <> 0
            Call ForceExtension ObjSaveFilter, '.obj'

            ; Get the Offset
            Move D$lvi.iItem D@SelectedItem
            Mov D$lvi.iSubItem 5 ; [Header6: B$ 'Offset' EOS] ; hexadecimal value string
            Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEM, D@SelectedItem, lvi
            Call AsciiBase szBuff0 BASE_HEX
            Mov edi eax

            ; Get the size
            Move D$lvi.iItem D@SelectedItem
            Mov D$lvi.iSubItem 6 ; [Header7: B$ 'Size' EOS] ; decimal value
            Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEM, D@SelectedItem, lvi
            Call String2Dword szBuff0

            Mov esi D$LibFileMemory
            add esi edi
            Call SaveLibFile, esi, eax, ObjSaveFilter
        .End_If
    ..End_If

    popad
EndP


____________________________________________________________________________________________
____________________________________________________________________________________________

[ObjPathTitle: B$ 'For the Exported Object Files' EOS]
[MultipleObjectPath: B$ ? # &MAX_PATH]

[EXPORT_ALL_OBJECTS 0]
[EXPORT_RUNTIME_OBJECTS 1]
[EXPORT_EXPORTED_OBJECTS 2]
; Flag = 0 (Save all Objects)
; Flag = 1 (Save all Runtime Objects)
; Flag = 2 (Save all Exported Objects)
Proc SaveAllObjectFile:
    Arguments @hwnd, @Flag
    Local @SelectedItem, @CountItens, @FolderPathStart, @RepeatedName

; More examples here MultipleCompileTests SaveSingleObjectFileAs
    pushad

    Call BrowseForFolder D@hwnd, ObjPathTitle
    ...If B$BrowseForFolderAborted <> &TRUE

        ; Initialize all revelant Data
        Mov D@RepeatedName 0
        Mov D@CountItens 0
        Mov D$RepeatedObjIndice '0000', D$RepeatedObjIndice+4 '00'
        ; 1st we calculate the amount of objectsinside the lib
        Call String2Dword LibObjIndice
        Mov D@CountItens eax

        Mov D@SelectedItem 0 ; always starts at the 1st item (that have the value of 0)
        Mov esi FolderPath, edi MultipleObjectPath
        While B$esi <> 0 | movsb | End_While | Mov B$edi '\' | inc edi
        Mov D@FolderPathStart edi

        .Do

            .If D@Flag = EXPORT_RUNTIME_OBJECTS
                Move D$lvi.iItem D@SelectedItem
                Mov D$lvi.iSubItem 7 ; [Header8: B$ 'Object Type' EOS] ; string
                Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEM, D@SelectedItem, lvi
                On D$szBuff0 <> 'Runt', jmp L1>> ; Not a runtime Object, jmp over it
            .Else_If D@Flag = EXPORT_EXPORTED_OBJECTS
                Move D$lvi.iItem D@SelectedItem
                Mov D$lvi.iSubItem 7 ; [Header8: B$ 'Object Type' EOS] ; string
                Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEM, D@SelectedItem, lvi
                On D$szBuff0 <> 'Expo', jmp L1>> ; Not a Export Object, jmp over it
            .End_If
            ; 1st we get the FileName

            Move D$lvi.iItem D@SelectedItem
            Mov D$lvi.iSubItem 1 ; [Header2: B$ 'File Name' EOS] ; string
            Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEM, D@SelectedItem, lvi

            ; Copy the filename to the path and append the '.obj' extension
            ; the result is in szBuff0
            Mov esi szBuff0
            While B$esi <> 0 | movsb | End_While
            Mov D$edi '.obj' | add edi 4 | Mov B$edi 0

            Call 'KERNEL32.FindFirstFileA' MultipleObjectPath, FindFile

            If eax <> &INVALID_HANDLE_VALUE ; do we have an existing file with this name ?
                ; Yes...append a new name for it
                Mov D$edi-4 '_New'
                Call IncrementObjExportNameIndex
                Mov esi RepeatedObjIndice
                While B$esi <> 0 | movsb | End_While
                Mov D$edi '.obj' | add edi 4 | Mov B$edi 0

            End_If

            ; Get the Offset
            Move D$lvi.iItem D@SelectedItem
            Mov D$lvi.iSubItem 5 ; [Header6: B$ 'Offset' EOS] ; hexadecimal value string
            Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEM, D@SelectedItem, lvi
            Call AsciiBase szBuff0 BASE_HEX
            Mov edi eax

            ; Get the size
            Move D$lvi.iItem D@SelectedItem
            Mov D$lvi.iSubItem 6 ; [Header7: B$ 'Size' EOS] ; decimal value
            Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEM, D@SelectedItem, lvi
            Call String2Dword szBuff0

            Mov esi D$LibFileMemory
            add esi edi
            Call SaveLibFile, esi, eax, MultipleObjectPath
L1:
            Mov edi D@FolderPathStart
            inc D@SelectedItem
            dec D@CountItens
        .Loop_Until D@CountItens = 0

    ...End_If
    popad
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[RepeatedObjIndice: B$ '000000' EOS]

IncrementObjExportNameIndex:
    lea ebx D$RepeatedObjIndice+5 | inc B$ebx
    While B$ebx > '9'
        Mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret

____________________________________________________________________________________________
____________________________________________________________________________________________



[LibSaveFileFilter: B$ 'Text File (*.txt)' EOS  '*.txt' EOS
                    B$ 'Assembly File (*.asm)' EOS  '*.asm' EOS
                    B$ 'All Files' EOS  '*.*' EOS 0]

[CustomLibFileFilter:  ? # &MAX_PATH]

[SaveLibFileTitle: B$ 'Save Library Structure report as...' EOS]

; OPENFILENAMEA Structure

[SAVELIB:
 SAVELIB.lStructSize: D$ Len
 SAVELIB.hwndOwner: D$ 0
 SAVELIB.hInstance: D$ 0
 SAVELIB.lpstrFilter: D$ LibSaveFileFilter
 SAVELIB.lpstrCustomFilter: D$ CustomLibFileFilter
 SAVELIB.nMaxCustFilter: D$ 260
 SAVELIB.nFilterIndex: D$ 1
 SAVELIB.lpstrFile: D$ LibSaveFilter
 SAVELIB.nMaxFile: D$ 260
 SAVELIB.lpstrFileTitle: D$ ChoosenLibFile
 SAVELIB.nMaxFileTitle: D$ 260
 SAVELIB.lpstrInitialDir: D$ 0
 SAVELIB.lpstrTitle: D$ SaveLibFileTitle
 SAVELIB.Flags: D$ &OFN_EXPLORER__&OFN_FILEMUSTEXIST__&OFN_LONGNAMES__&OFN_PATHMUSTEXIST
 SAVELIB.nFileOffset: W$ 0
 SAVELIB.nFileExtension: W$ 0
 SAVELIB.lpstrDefExt: D$ 0
 SAVELIB.lCustData: D$ 0
 SAVELIB.lpfnHook: D$ 0
 SAVELIB.lpTemplateName: D$ 0]

[LibScanIsFileOpen: B$ &FALSE]

Proc SaveLibFileAs:
    Arguments @hwnd, @OutPut, @OutPutSize
    uses eax

    ; To Insert the Appended String. We inset it, when the user opens a new file only. On case the user presses Clear Button
    ; the Buffer is emptyed.
    If_And B$LibScanIsFileOpen = &FALSE, D$hLibReportEdit <> 0
        Call LibScanAppendReportName LibSaveFilter
    End_If

    Mov D$ChoosenLibFile 0
    Move D$SAVELIB.lpstrFile LibSaveFilter
    Move D$SAVELIB.hwndOwner D@hwnd
    Move D$SAVELIB.hInstance D$H.Instance
    Move D$SAVELIB.lpstrFilter LibSaveFileFilter
    Call 'Comdlg32.GetSaveFileNameA' SAVELIB

    On eax = 0, ExitP
    .If D$ChoosenLibFile <> 0
        If D$SAVELIB.nFilterIndex = 1 ; extension .txt
            Call ForceExtension LibSaveFilter, '.txt'
        Else_If D$SAVELIB.nFilterIndex = 2 ; extension .asm
            Call ForceExtension LibSaveFilter, '.asm'
        Else ; Force txt extensino if the user is trying to save as "all"
            Call ForceExtension LibSaveFilter, '.txt'
        End_If
        Call SaveLibFile, D@OutPut, D@OutPutSize, LibSaveFilter
    .End_If

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc LibScanAppendReportName:
    Arguments @String
    Uses eax, ecx, edi

    Mov edi D@String
    ;sub edi 2     ; edi points to the end of the string 2

    .While edi >= D@String

        .If_Or B$edi = ':', B$edi = '/', B$edi = '\'

            Do
                On B$edi = '.', jmp L1>
                inc edi
            Loop_Until B$edi = 0
            jmp L1>
        .End_If

        ;dec edi
        inc edi
    .End_While
    L1:
        Push esi | ZCopy {B$ "Report.txt" EOS} | Pop esi
        Mov D$edi 0
    Mov B$LibScanIsFileOpen &TRUE
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc SaveLibFile:
    Arguments @OutPut, @OutPutSize, @FileFilter

    pushad

    Call 'KERNEL32.CreateFileA' D@FileFilter, &GENERIC_WRITE,
                                &FILE_SHARE_READ__&FILE_SHARE_WRITE, &NULL, &CREATE_ALWAYS,
                                &FILE_ATTRIBUTE_NORMAL, &NULL

    Mov D$H.LibFile eax

    If eax = &INVALID_HANDLE_VALUE
        Mov eax D$BusyFilePtr | Call MessageBox | popad | ret
    End_If

    Mov D$H.Destination eax, D$NumberOfReadBytes 0

    Call 'KERNEL32.WriteFile' D$H.Destination, D@OutPut, D@OutPutSize, NumberOfReadBytes  0
    Call 'KERNEL32.CloseHandle' D$H.Destination | Mov D$H.Destination 0

    popad

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; This function will cleanup all previously loaded Data in case the user reopens the file without
; closing the CharMap Dialog

Proc LibScanCleanUp:

    Arguments @hwnd

    ; Free LibFileMemory

    Call VirtualFree LibFileMemory

    ; Free hLibReportEdit Report
    Call VirtualFree hLibReportEdit

    Call 'USER32.SendDlgItemMessageA' D@hwnd, IDC_OPENLIBFILE, &WM_SETTEXT, 0, &NULL

    Call 'USER32.SendDlgItemMessageA' D@hwnd, IDC_LIBFILESIZE, &WM_SETTEXT, 0, &NULL

    Call 'USER32.SendDlgItemMessageA' D$H.TabDlg1, IDC_LIB_SOURCE, &WM_SETTEXT, 0, &NULL

    Call 'USER32.SendMessageA', D$H.List, &LVM_DELETEALLITEMS, 0, lvi

    Call 'USER32.SendMessageA' D@hwnd, &WM_SETTEXT, 0, {B$ "ScanLib v 1.0" EOS}

    Call ClearDwordBuffer szBuff0,
                          (256/DWORD)

    Call ClearDwordBuffer szBuff1,
                          (256/DWORD)

    ; Must clean these listview itens to avoid that we have errors on the resequence
    Mov D$lvi.iItem 0

    Mov D$lvi.iSubItem 0

EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc CoolControlDlg_CreateTab:
    Arguments @hwnd, @nIDDlgItem, @H.Output, @CCTabStructure
    Local @TabsAmount, @UseImage


    Mov D@TabsAmount 0
    Mov D@UseImage 0

    Call 'USER32.GetDlgItem' D@hwnd, D@nIDDlgItem ; Get the Tab Control in the resources, and
                                                    ; use it on the handle of the main window
                                                    ; of this procedure.
    lea edi D@H.Output
    Mov edi D$edi
    Mov D$edi eax                                   ; Now we return the result (found in eax), copying it to
                                                    ; the Handle of the Tab Control in the resource.
                                                    ; So, all we did was get the TAB Control and save it to the Tab handle.

    Mov ecx D@CCTabStructure
    Mov eax D$ecx+CCTabOrganize.ImageListDis

    If eax = 0
        Mov D@UseImage &FALSE
    Else
        Mov D@UseImage &TRUE
        Push ecx | SendMessage D$edi, &TCM_SETIMAGELIST, 0, D$eax | Pop ecx
    End_If

    Mov edx D$ecx+CCTabOrganize.TCITEMDis
    Move D$edx+TC_ITEM.imaskDis D$ecx+CCTabOrganize.iMaskFlagDis

    Mov eax D$ecx+CCTabOrganize.TabsAmountDis
    Move D@TabsAmount D$eax

    Mov ebx 0

    .Do

        ; TabTitle copied to ts.pszText
        Mov esi D$ecx+CCTabOrganize.StringArrayDis
        Mov esi D$esi+ebx*4
        Mov D$edx+TC_ITEM.pszTextDis esi

        ; TabTitleLen copied to ts.cchTextMax
        Mov esi D$ecx+CCTabOrganize.StringLenArrayDis
        Mov esi D$esi+ebx*4
        Move D$edx+TC_ITEM.cchTextMaxDis D$esi

        If D@UseImage = &TRUE
            ; Image ID copied to ts.iImage
            Mov D$edx+TC_ITEM.iImageDis ebx
        End_If

        Push edx
        Push ecx
        Call 'USER32.SendMessageA' D$edi &TCM_INSERTITEM ebx edx
        Pop ecx
        Pop edx

        inc ebx
    .Loop_Until ebx = D@TabsAmount
;;

;;
[TC_ITEM.imaskDis 0
 TC_ITEM.lpReserved1Dis 4
 TC_ITEM.lpReserved2Dis 8
 TC_ITEM.pszTextDis 12
 TC_ITEM.cchTextMaxDis 16
 TC_ITEM.iImageDis 20
 TC_ITEM.lParamDis 24]
     
;;
;;

        Mov D$ts.imask &TCIF_TEXT__&TCIF_IMAGE ; Mask text & image
        Mov D$ts.pszText TabTitle1
        Mov D$ts.cchTextMax TabTitle1Len
        Mov D$ts.iImage 0
        Call 'USER32.SendMessageA' D$edi &TCM_INSERTITEM 0 ts

        Mov D$ts.imask &TCIF_TEXT__&TCIF_IMAGE ; Mask text & image
        Mov D$ts.pszText TabTitle2
        Mov D$ts.cchTextMax TabTitle2Len
        Mov D$ts.iImage 1
        Call 'USER32.SendMessageA' D$edi &TCM_INSERTITEM 1 ts

        Mov D$ts.imask &TCIF_TEXT__&TCIF_IMAGE ; Mask text & image
        Mov D$ts.pszText TabTitle3
        Mov D$ts.cchTextMax TabTitle3Len
        Mov D$ts.iImage 2
        Call 'USER32.SendMessageA' D$edi &TCM_INSERTITEM 2 ts


        Mov D$ts.imask &TCIF_TEXT__&TCIF_IMAGE ; Mask text & image
        Mov D$ts.pszText TabTitle4
        Mov D$ts.cchTextMax TabTitle4Len
        Mov D$ts.iImage 3
        Call 'USER32.SendMessageA' D$edi &TCM_INSERTITEM 3 ts
;;
;;
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________


[LibScannerTitle: B$ ? # 100]

Proc WriteObjectTypeinTitle:
    Arguments @hwnd, @TextTitle

    pushad

    Mov eax D@TextTitle
    Mov edi LibScannerTitle
    Push esi | Zcopy {B$ "ScanLib v 1.0 - " EOS} | Pop esi

    Push esi | ZCopy eax | Pop eax
    Push esi | Zcopy {B$ " Format" EOS} | Pop esi

    Mov B$edi 0
    Call 'USER32.SendMessageA' D@hwnd, &WM_SETTEXT, 0, LibScannerTitle

    popad

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; Enable / Gray-out execution control commands.

Proc LibScanDialog_EnableContinueMenu:
    Arguments @Enable

    If D@Enable = 1
        Mov ebx &MF_ENABLED
    Else
        Mov ebx &MF_GRAYED
    End_If

    Call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Close, ebx
    Call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Save_Report, ebx
    Call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Single_Object_File, ebx
    Call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_All_Object_Files, ebx
    Call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Runtime_Objects_Only, ebx
    Call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Exported_Objects_Only, ebx
    Call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Build_Dis_File, ebx

    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Close, D@Enable
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Save_Report, D@Enable
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Single_Object_File, D@Enable
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_All_Object_Files, D@Enable
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Runtime_Objects_Only, D@Enable
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Exported_Objects_Only, D@Enable
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Build_Dis_File, D@Enable

  ; Invert
    If D@Enable = 1
        Mov ebx &MF_GRAYED
        Mov D@Enable 0
    Else
        Mov ebx &MF_ENABLED
        Mov D@Enable 1
    End_If

EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc LibScanDialog_EnableContinueNextTabMenu:
    Arguments @Enable

    If D@Enable = 1
        Mov ebx &MF_ENABLED
    Else
        Mov ebx &MF_GRAYED
    End_If

    Call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Next_Tab, ebx
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Next_Tab, D@Enable

    Call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Last_Tab, ebx
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Last_Tab, D@Enable
  ; Invert
    If D@Enable = 1
        Mov ebx &MF_GRAYED
        Mov D@Enable 0
    Else
        Mov ebx &MF_ENABLED
        Mov D@Enable 1
    End_If

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc LibScanDialog_EnableContinuePrevTabMenu:
    Arguments @Enable
    uses eax, ecx, edx

    If D@Enable = 1
        Mov ebx &MF_ENABLED
    Else
        Mov ebx &MF_GRAYED
    End_If

    Call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Previous_Tab, ebx
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Previous_Tab, D@Enable

    Call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_First_Tab, ebx
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_First_Tab, D@Enable


  ; Invert
    If D@Enable = 1
        Mov ebx &MF_GRAYED
        Mov D@Enable 0
    Else
        Mov ebx &MF_ENABLED
        Mov D@Enable 1
    End_If

EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc LibScanDisplayFileSize:
    Arguments @hwnd

    pushad
    ; Calculate and Display File Size
    Mov edi buffer2
    lea esi D$LibFileLength

    Mov ecx 4
    Call toUDword
    Mov esi edi, edi DecimalBuffer
    Do | movsb | LoopUntil B$esi = 0
    Push esi | ZCopy {B$ ' Bytes' EOS} | Pop esi
    Mov B$edi 0

    Call 'USER32.SendDlgItemMessageA' D@hwnd IDC_LIBFILESIZE &WM_SETTEXT 0 DecimalBuffer
    Call 'USER32.SendDlgItemMessageA' D@hwnd IDC_LIBFILESIZE &WM_GETTEXT 0 DecimalBuffer ; GET THE FILENAME
    popad
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc LibScanDialog_ToggleToolbarText:
    Arguments @Addresse
    Structure @TBButtonInfo 32, @Size 0, @Mask 4, @Text 24

    Mov D@Size 32, D@Mask &TBIF_TEXT

  ; get check state of menu item, invert, toggle band and set inverted check state
    Call 'USER32.GetMenuState' D$LibScanMenuHandle, M03_Show_ToolBarText, &MF_BYCOMMAND
    Push eax
        xor edx edx | test eax &MF_CHECKED | setz dl
        Mov B$LibScanShowTBText dl
        Call CoolControlWin_CreateCommandTB D@Addresse, LibScanToolbarHandle, LibScanToolbarButtons, LibScanToolButtonsNumber, LibScanToolTipsStrings, LSTBWin_Cmd
    Pop eax
    xor eax &MF_CHECKED | and eax &MF_CHECKED
    Call 'USER32.CheckMenuItem' D$LibScanMenuHandle, M03_Show_ToolBarText, eax
    SendMessage D$LibScanMenuHandle, &TB_AUTOSIZE, 0, 0

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[LibScanHoverTBText: D$ ?]

Proc LibScanDialog_HoverToolbarText:
    Arguments @Addresse
    Structure @TBButtonInfo 32, @Size 0, @Mask 4, @Text 24

    Mov D@Size 32, D@Mask &TBIF_TEXT

  ; get check state of menu item, invert, toggle band and set inverted check state
    Call 'USER32.GetMenuState' D$LibScanMenuHandle, M03_Hover_ToolBar, &MF_BYCOMMAND
    Push eax
        xor edx edx | test eax &MF_CHECKED | setz dl
        Mov B$LibScanHoverTBText dl
        ;Call CoolControlWin_CreateCommandTB D@Addresse, LibScanToolbarHandle, LibScanToolbarButtons, LibScanToolButtonsNumber, LibScanToolTipsStrings, LSTBWin_Cmd
    Pop eax
    xor eax &MF_CHECKED | and eax &MF_CHECKED
    Call 'USER32.CheckMenuItem' D$LibScanMenuHandle, M03_Hover_ToolBar, eax
    SendMessage D$LibScanMenuHandle, &TB_AUTOSIZE, 0, 0

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

DecodeLib:   ; EncodeDecode
    Mov B$WeAreInTheCodeBox &TRUE

        Mov esi D$LibBytesCopy, edi D$LibDisassemblyPtr

      ; Disassemble: (DisMain)
        .While D$esi <> 0
            lodsd | Mov edx esi | add edx eax

            While esi < edx
                Push edx
                    Mov B$DisFlag 0, D$SegmentOverride 0, B$AddressSizeOverride 0
                    Mov B$OperandSizeOverride 0, W$DisSizeMarker 'D$'
                    Mov B$DisCodeDisplacement &FALSE, B$EscapePrefix &FALSE

L1:                 movzx eax B$esi | inc esi | Call D$DisOp1+eax*4
                    On B$DisFlag = DISDONE, jmp L1<

                    Mov W$edi CRLF | add edi 2
                Pop edx
            End_While

            Mov D$edi CRLF2 | add edi 4 | Mov esi edx
        .End_While

        Mov D$LibDisassemblyPtr edi

    Mov B$WeAreInTheCodeBox &FALSE
ret




__________________________________________________________________________________________________________________



; Tag Dialog 20

LibScanner:
    Call 'USER32.DialogBoxParamA' D$H.Instance, IDD_MAINLIB, &NULL, ScanLibFile, &NULL
ret

____________________________________________________________________________________________

[IDD_LIBSCANWARNINGMSG 23]
[IDC_DISPLAYWARNINGMSG 10]
[IDC_DISPLAYWARNINGHEXA 20]

[IDB_LIBWARNING_BITMAP 200]
[IDC_LIBWARNING_SHOWICON 6]

[LibScanWarnMsg: B$ "The Magic Signature Tag was not found.

This is not a Object File supported by this dumper
__________________________________________________" EOS]

[LibScanHexaMsg: D$ ? # 256]
[LibWarningStaticImage: D$ ?]
[LibScanhIcon: D$ ?]

Proc LibScanWarning:
    Arguments @hwnd, @msg, @wParam, @lParam

     pushad


    ..If D@msg = &WM_INITDIALOG

        Call 'USER32.GetDlgItem' D@hwnd IDC_LIBWARNING_SHOWICON ; Get the handle of the Static Image
        Mov D$LibWarningStaticImage eax
        Call 'USER32.LoadBitmapA' D$H.Instance, IDB_LIBWARNING_BITMAP
        Mov D$LibScanhIcon eax

        Call 'USER32.SendMessageA' D$LibWarningStaticImage, &STM_SETIMAGE, &IMAGE_BITMAP, D$LibScanhIcon
        Call 'USER32.SendDlgItemMessageA' D@hwnd IDC_DISPLAYWARNINGMSG &WM_SETTEXT 0 LibScanWarnMsg
        Call 'USER32.SendDlgItemMessageA' D@hwnd IDC_DISPLAYWARNINGMSG &WM_GETTEXT 0 LibScanWarnMsg

        ; Calculate and Display File Size
        Mov edi LibScanHexaMsg
        Mov ecx D$LibFileLength
        Mov esi D$LibFileMemory

        If D$LibFileLength < 80  ; Maximum amount of bytes
            Mov ecx D$LibFileLength
        Else_If ecx = 0 ; This shouldn�t happen, because we already have a Zero Size check, but...security is never too much ;)
            Mov ecx 1
        Else
            Mov ecx 80
        End_If

        ; Convert the 1st 80 Bytes to Decimal String notation.
        xor eax eax
        Mov edx 0
        Do
            lodsb
            Call writeeax
            xor eax eax
            Mov B$edi SPC | inc edi
            dec ecx
        Loop_Until ecx = 0
        Mov B$edi 0

        Call 'USER32.SetDlgItemTextA' D@hwnd, IDC_DISPLAYWARNINGHEXA, LibScanHexaMsg

    ..Else_If D@msg = &WM_CLOSE
        
        Call ClearDwordBuffer LibScanHexaMsg,
                              (256/DWORD)
        
        Call 'USER32.EndDialog' D@hwnd &NULL

    ..Else_If D@msg = &WM_COMMAND
        .If D@wParam = &IDOK

            Call ClearDwordBuffer LibScanHexaMsg,
                                  (256/DWORD)

            Call 'USER32.EndDialog' D@hwnd &NULL

         .End_If

    ..Else

       popad | Mov eax &FALSE | ExitP

    ..End_If

    popad
    Mov eax &TRUE
EndP

____________________________________________________________

[szBuff1: B$ ? # 256]

Proc ListViewLibSort:
    Arguments @lParam1, @lParam2, @lParamSort
    Local @Dir1, @Dir2
    Uses edi, ebx, esi, ecx, edx

;    Call ClearCharMapData szBuff0, 256
;    Call ClearCharMapData szBuff1, 256

    and D@Dir1 0
    and D@Dir2 0
    Mov D$lvi.imask &LVIF_TEXT
    lea eax D$szBuff0
    Mov D$lvi.pszText eax
    Mov D$lvi.cchTextMax 256

    Mov D$lvi.iSubItem 01
    Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam1, lvi
    xor eax eax
    Mov al B$szBuff0
    Mov D@Dir1 eax
    Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam2, lvi
    xor eax eax
    Mov al B$szBuff0
    Mov D@Dir2 eax

    ; Decimal Value [Header1: B$ 'Index' EOS]
    ..If_Or D@lParamSort = 01, D@lParamSort = 02

        Mov D$lvi.iSubItem 0
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam1, lvi
        Call String2Dword szBuff0
        Mov edi eax
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam2, lvi
        Call String2Dword szBuff0

        If D@lParamSort = 1
            sub edi eax
            Mov eax edi
        Else
            sub eax edi
        End_If

    ; Hexadecimal Value [Header2: B$ 'File Name' EOS] ; string
    ..Else_If_Or D@lParamSort = 3, D@lParamSort = 4

        Mov D$lvi.iSubItem 1
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam1, lvi
        Call strcpy szBuff1, szBuff0
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam2, lvi

        If D@lParamSort = 3
            Call lstrcmpi szBuff1, szBuff0
        Else
            Call lstrcmpi szBuff0, szBuff1
        End_If

    ; [Header3: B$ 'Extension' EOS] ; string
    ..Else_If_Or D@lParamSort = 5, D@lParamSort = 6

        Mov D$lvi.iSubItem 2
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam1, lvi
        Call strcpy szBuff1, szBuff0
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam2, lvi

        If D@lParamSort = 5
            Call stricmp szBuff1, szBuff0
        Else
            Call stricmp szBuff0, szBuff1
        End_If

    ; [Header4: B$ 'FileName Type' EOS] ; string
    ..Else_If_Or D@lParamSort = 7, D@lParamSort = 8

        Mov D$lvi.iSubItem 3
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam1, lvi
        Call strcpy szBuff1, szBuff0
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam2, lvi

        If D@lParamSort = 7
            Call stricmp szBuff1, szBuff0
        Else
            Call stricmp szBuff0, szBuff1
        End_If

    ; [Header5: B$ 'Path' EOS] ; string
    ..Else_If_Or D@lParamSort = 9, D@lParamSort = 10

        Mov D$lvi.iSubItem 4
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam1, lvi
        Call strcpy szBuff1, szBuff0
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam2, lvi

        If D@lParamSort = 9
            Call lstrcmpi szBuff1, szBuff0
        Else
            Call lstrcmpi szBuff0, szBuff1
        End_If

    ; [Header6: B$ 'Offset' EOS] ; hexadecimal value string
    ..Else_If_Or D@lParamSort = 11, D@lParamSort = 12

        Mov D$lvi.iSubItem 5
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam1, lvi
        Call AsciiBase szBuff0 BASE_HEX
        Mov edi eax
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam2, lvi
        Call AsciiBase szBuff0 BASE_HEX

        If D@lParamSort = 11
            sub edi eax
            Mov eax edi
        Else
            sub eax edi
        End_If


    ; [Header7: B$ 'Size' EOS] ; decimal value
    ..Else_If_Or D@lParamSort = 13, D@lParamSort = 14

        Mov D$lvi.iSubItem 6
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam1, lvi
        Call String2Dword szBuff0
        Mov edi eax
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam2, lvi
        Call String2Dword szBuff0

        If D@lParamSort = 13
            sub edi eax
            Mov eax edi
        Else
            sub eax edi
        End_If

    ; [Header8: B$ 'Object Type' EOS] ; string
    ..Else_If_Or D@lParamSort = 15, D@lParamSort = 16

        Mov D$lvi.iSubItem 7
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam1, lvi
        Call strcpy szBuff1, szBuff0
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam2, lvi

        If D@lParamSort = 15
            Call stricmp szBuff1, szBuff0
        Else
            Call stricmp szBuff0, szBuff1
        End_If

    ; [Header9: B$ 'Exported Function' EOS] ; string
    ..Else_If_Or D@lParamSort = 17, D@lParamSort = 18

        Mov D$lvi.iSubItem 8
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam1, lvi
        Call strcpy szBuff1, szBuff0
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam2, lvi

        If D@lParamSort = 17
            Call lstrcmpi szBuff1, szBuff0
        Else
            Call lstrcmpi szBuff0, szBuff1
        End_If

    ; [Header10: B$ 'Machine Type' EOS] ; string
    ..Else_If_Or D@lParamSort = 19, D@lParamSort = 20

        Mov D$lvi.iSubItem 9
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam1, lvi
        Call strcpy szBuff1, szBuff0
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam2, lvi

        If D@lParamSort = 19
            Call lstrcmpi szBuff1, szBuff0
        Else
            Call lstrcmpi szBuff0, szBuff1
        End_If

    ; [Header11: B$ 'Sections' EOS] ; decimal value
    ..Else_If_Or D@lParamSort = 21, D@lParamSort = 22

        Mov D$lvi.iSubItem 10
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam1, lvi
            If_And D$szBuff0 = 'No S', D$szBuff0+4 = 'ecti' ; "No Sections" string = 0
                Mov eax 0
            Else
                Call String2Dword szBuff0
            End_If
        Mov edi eax
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam2, lvi
            If_And D$szBuff0 = 'No S', D$szBuff0+4 = 'ecti' ; "No Sections" string = 0
                Mov eax 0
            Else
                Call String2Dword szBuff0
            End_If
        If D@lParamSort = 21
            sub edi eax
            Mov eax edi
        Else
            sub eax edi
        End_If

    ; [Header12: B$ 'Time and Date' EOS] ; time and date string - convert it to dword
    ..Else_If_Or D@lParamSort = 23, D@lParamSort = 24

        Mov D$lvi.iSubItem 11
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam1, lvi
        ;TimeDateStringtoDword
        Call strcpy szBuff1, szBuff0
        Mov edi eax
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam2, lvi
        Call String2Dword szBuff0

        If D@lParamSort = 23
            Call lstrcmpi szBuff1, szBuff0
        Else
            Call lstrcmpi szBuff0, szBuff1
        End_If

    ; [Header13: B$ 'Symbols' EOS] ; decimal value
    ..Else_If_Or D@lParamSort = 25, D@lParamSort = 26
        Mov D$lvi.iSubItem 12
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam1, lvi
            If_And D$szBuff0 = 'No S', D$szBuff0+4 = 'ymbo' ; "No Symbols" String = 0
                Mov eax 0
            Else
                Call String2Dword szBuff0
            End_If
        Mov edi eax
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam2, lvi
            If_And D$szBuff0 = 'No S', D$szBuff0+4 = 'ymbo' ; "No Symbols" String = 0
                Mov eax 0
            Else
                Call String2Dword szBuff0
            End_If
        If D@lParamSort = 25
            sub edi eax
            Mov eax edi
        Else
            sub eax edi
        End_If

    ; [Header14: B$ 'Optional Header' EOS] ; string
    ..Else_If_Or D@lParamSort = 27, D@lParamSort = 28
        ; Optional Header can be only True or False.
        Mov D$lvi.iSubItem 13
        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam1, lvi
            If D$szBuff0 = 'True'
                Mov eax 1
            Else
                Mov eax 0
            End_If
        Mov edi eax

        Call 'USER32.SendMessageA' D$H.List, &LVM_GETITEMTEXT, D@lParam2, lvi
            If D$szBuff0 = 'True'
                Mov eax 1
            Else
                Mov eax 0
            End_If

        If D@lParamSort = 27
            sub edi eax
            Mov eax edi
        Else
            sub eax edi
        End_If



    ..End_If
;---------- [Keep the folders on top] ----------

    .If_And B@Dir1 = '<', B@Dir2 <> '<'
        xor eax eax
        dec eax
        ExitP
    .Else_If_And B@Dir2 = '<', B@Dir1 <> '<'
        xor eax eax
        inc eax
        ExitP
    .Else_If_And B@Dir1 = '<', B@Dir2 = '<'
        xor eax eax
    .End_If

EndP


________________________________________________________________________________________


; ToolBar Data and Functions

[LibScanMenuHandle: D$ ?]
[LibScanDialog_ImageList: D$ ?]
[IDB_LibScanEnableTB 30]
[IDB_LibScanDisableTB 31]
[LibScanToolbarHandle: D$ 0]

[LIBSCANDLG_TOOLBAR 400]

; Flow control buttons ; TBBUTTON Structure

[LibScanToolbarButtons:
    LSTB01.iBitmap: D$ 0
    LSTB01.idCommand: D$ M03_Open
    LSTB01.fsState: B$ &TBSTATE_ENABLED
    LSTB01.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB01._wPad1: W$ 0
    LSTB01.dwData: D$ 0
    LSTB01.iString: D$ 0

    LSTB02.iBitmap: D$ 1
    LSTB02.idCommand: D$ M03_Close
    LSTB02.fsState: B$ &TBSTATE_ENABLED
    LSTB02.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB02._wPad1: W$ 0
    LSTB02.dwData: D$ 0
    LSTB02.iString: D$ 0

    LSTB03.iBitmap: D$ 2
    LSTB03.idCommand: D$ M03_Save_Report
    LSTB03.fsState: B$ &TBSTATE_ENABLED
    LSTB03.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB03._wPad1: W$ 0
    LSTB03.dwData: D$ 0
    LSTB03.iString: D$ 0

    LSTB04.iBitmap: D$ 3
    LSTB04.idCommand: D$ M03_Single_Object_File
    LSTB04.fsState: B$ &TBSTATE_ENABLED
    LSTB04.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB04._wPad1: W$ 0
    LSTB04.dwData: D$ 0
    LSTB04.iString: D$ 0

    LSTB05.iBitmap: D$ 4
    LSTB05.idCommand: D$ M03_All_Object_Files
    LSTB05.fsState: B$ &TBSTATE_ENABLED
    LSTB05.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB05._wPad1: W$ 0
    LSTB05.dwData: D$ 0
    LSTB05.iString: D$ 0

    LSTB06.iBitmap: D$ 5
    LSTB06.idCommand: D$ M03_Build_Dis_File
    LSTB06.fsState: B$ &TBSTATE_ENABLED
    LSTB06.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB06._wPad1: W$ 0
    LSTB06.dwData: D$ 0
    LSTB06.iString: D$ 0

    LSTB07.iBitmap: D$ 6
    LSTB07.idCommand: D$ M03_Next_Tab
    LSTB07.fsState: B$ &TBSTATE_ENABLED
    LSTB07.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB07._wPad1: W$ 0
    LSTB07.dwData: D$ 0
    LSTB07.iString: D$ 0

    LSTB08.iBitmap: D$ 7
    LSTB08.idCommand: D$ M03_Previous_Tab
    LSTB08.fsState: B$ &TBSTATE_ENABLED
    LSTB08.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB08._wPad1: W$ 0
    LSTB08.dwData: D$ 0
    LSTB08.iString: D$ 0

    LSTB09.iBitmap: D$ 8
    LSTB09.idCommand: D$ M03_First_Tab
    LSTB09.fsState: B$ &TBSTATE_ENABLED
    LSTB09.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB09._wPad1: W$ 0
    LSTB09.dwData: D$ 0
    LSTB09.iString: D$ 0

    LSTB10.iBitmap: D$ 9
    LSTB10.idCommand: D$ M03_Last_Tab
    LSTB10.fsState: B$ &TBSTATE_ENABLED
    LSTB10.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB10._wPad1: W$ 0
    LSTB10.dwData: D$ 0
    LSTB10.iString: D$ 0]


[LIBSCAN_TOOLBAR_STYLE &WS_CHILD__&WS_VISIBLE__&TBSTYLE_FLAT__&TBSTYLE_LIST__&TBSTYLE_AUTOSIZE__&TBSTYLE_TRANSPARENT__&TBSTYLE_TOOLTIPS__&CCS_TOP]

[LibScanShowTBText: D$ ?]

[LibScanToolButtonsNumber 10]

[LibScanToolTipsStrings:
 StrLibScanOpen
 StrLibScanClose
 StrLibScanSave
 StrLibScanSingleObj
 StrLibScanAllObj
 StrLibScanDIS
 StrLibScanNextTab
 StrLibScanPrevTab
 StrLibScanFirstTab
 StrLibScanLastTab]

[StrLibScanOpen: B$ 'Open' EOS]
[StrLibScanClose: B$ 'Close' EOS]
[StrLibScanSave: B$ 'Save Report' EOS]
[StrLibScanSingleObj: B$ 'Extract Single Object' EOS]
[StrLibScanAllObj: B$ 'Extract All Objects' EOS]
[StrLibScanDIS: B$ 'Build DIS File' EOS]
[StrLibScanNextTab: B$ 'Goto Next tab' EOS]
[StrLibScanPrevTab: B$ 'Goto Previous tab' EOS]
[StrLibScanFirstTab: B$ 'Goto First tab' EOS]
[StrLibScanLastTab: B$ 'Goto Last tab' EOS]

[LSTBWin_Cmd:
 LSTBWin_Cmd.bWidth: D$ 20
 LSTBWin_Cmd.dwStyle: D$ LIBSCAN_TOOLBAR_STYLE
 LSTBWin_Cmd.hMenu: D$ LIBSCANDLG_TOOLBAR
 LSTBWin_Cmd.hIml: D$ LibScanDialog_ImageList
 LSTBWin_Cmd.ShowTxtFlag: D$ LibScanShowTBText]

__________________________________________________________________________________________

; Tab Control Data and Functions

[IDB_LibScanTABEnable 40]
[IDB_LibScanTABDisable 41]

[LibScanTabControlsNumber 4]

; This structure is of the type TC_ITEM
[ts:
 ts.imask: D$ &TCIF_TEXT
 ts.lpReserved1: D$ 0
 ts.lpReserved2: D$ 0
 ts.pszText: D$ 0
 ts.cchTextMax: len
 ts.iImage: D$ 0-01
 ts.lParam: D$ 0]

[TC_ITEM.imaskDis 0
 TC_ITEM.lpReserved1Dis 4
 TC_ITEM.lpReserved2Dis 8
 TC_ITEM.pszTextDis 12
 TC_ITEM.cchTextMaxDis 16
 TC_ITEM.iImageDis 20
 TC_ITEM.lParamDis 24]

; Tab Dialog Messages and Data

[TabTitle1: B$ "Library Structure" EOS
TabTitle1Len: D$ len]

[TabTitle2: B$ "Object Files" EOS
TabTitle2Len: D$ len]

[TabTitle3: B$ "Disassembled Display" EOS
TabTitle3Len: D$ len]

[TabTitle4: B$ "DIS Builder" EOS
TabTitle4Len: D$ len]

[LibScanTabToolTipStrings: TabTitle1 TabTitle2 TabTitle3 TabTitle4]
[LibScanTabToolTipStringsLen: TabTitle1Len TabTitle2Len TabTitle3Len TabTitle4Len]

; CCTabOrganize Structure

[LibScanTabControl:
 LibScanTabControl.TCITEM: D$ ts
 LibScanTabControl.iMaskFlag: D$ &TCIF_TEXT__&TCIF_IMAGE
 LibScanTabControl.ImageList: D$ LibScanTab_ImageList
 LibScanTabControl.TabsAmount: D$ LibScanTabNumber
 LibScanTabControl.StringArray: D$ LibScanTabToolTipStrings
 LibScanTabControl.StringLenArray: D$ LibScanTabToolTipStringsLen]

[CCTabOrganize.TCITEMDis 0
 CCTabOrganize.iMaskFlagDis 4
 CCTabOrganize.ImageListDis 8
 CCTabOrganize.TabsAmountDis 12
 CCTabOrganize.StringArrayDis 16
 CCTabOrganize.StringLenArrayDis 20]

[Size_Of_CCTabOrganize 24]


[LibScanTabNumber: 4]

[hTab: D$ 0] ; Handle of the Tab Control in the resource, that is inside the main Dialog

[SelTab: D$ 0] ; Selection Tab Flag.

[H.TabDlg1: D$ 0 ; Handle of the 1st Dialog MZ Header !!! Attention, utilis� en tant que Lp -> TABLE.H.TabDlg...
 H.TabDlg2: D$ 0 ; Handle of the 2nd Dialog PE Header
 H.TabDlg3: D$ 0 ; Handle of the 3rd Dialog PE DataDirectory Header
 H.TabDlg4: D$ 0] ; Handle of the 4th Dialog Image Section Headers

__________________________________________________________________________________________


; RunTime Functions


;;
;;
    Concatenation
    
    This function concatenates (join) 02 strings, and save the result on a buffer.

 Parameters:
 
    Source1: The 1st string to be joined
  
    Source2: The 2nd string to be joined
    
    Destination: The outputed buffer where the new string is stored
 

 Usage Example:
 
 
    Call Concatenation {B$ "HY ! My name is" EOS} {B$ "Guga" EOS}  edi

_______________________________________________________________

  Author:
    
    RobotBob (Eric) - Original Author

;;
;;

Proc Concatenation:
    Arguments @Source1, @Source2, @Destination
    Uses esi, edi

        Mov esi D@Source1, edi D@Destination
        While B$esi <> 0 | movsb | End_While

        Mov esi D@Source2
        While B$esi <> 0 | movsb | End_While

        movsb
EndP
____________________________________________________________________________________________________________

; Guga CodeView Equates


;;
;;
#define MAKESIG(a,b,c,d)        ((a) | ((b) << 8) | ((c) << 16) | ((d) << 24))
#define CODEVIEW_NB09_SIG       MAKESIG('N','B','0','9')
#define CODEVIEW_NB10_SIG       MAKESIG('N','B','1','0')
#define CODEVIEW_NB11_SIG       MAKESIG('N','B','1','1')
#define CODEVIEW_RSDS_SIG       MAKESIG('R','S','D','S')
;;
;;

[MAKESIG | (#1 or (#2 shl 8) or (#3 shl 16) or (#4 shl 24))]

[CODEVIEW_NB09_SIG 03930424E] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','9'] ; The result is reversed NB09
[CODEVIEW_NB10_SIG 03031424E] ; [CODEVIEW_NB10_SIG  MAKESIG 'N','B','1','0'] ; The result is reversed NB10
[CODEVIEW_NB11_SIG 03131424E] ; [CODEVIEW_NB11_SIG  MAKESIG 'N','B','1','1'] ; The result is reversed NB11
[CODEVIEW_RSDS_SIG 053445352] ; [CODEVIEW_RSDS_SIG  MAKESIG 'R','S','D','S'] ; The result is reversed RSDS

;;
; weird. The real signature (pdb files) in hexadecimal is not reversed. It is the regular order, Like:
;;
[CODEVIEW_NB00_SIG 04E423030] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','0'] ; The result is NB00
[CODEVIEW_NB01_SIG 04E423031] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','1'] ; The result is NB01
[CODEVIEW_NB02_SIG 04E423032] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','2'] ; The result is NB02
[CODEVIEW_NB03_SIG 04E423033] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','3'] ; The result is NB03
[CODEVIEW_NB04_SIG 04E423034] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','4'] ; The result is NB04
[CODEVIEW_NB05_SIG 04E423035] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','5'] ; The result is NB05
[CODEVIEW_NB06_SIG 04E423036] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','6'] ; The result is NB06
[CODEVIEW_NB07_SIG 04E423037] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','7'] ; The result is NB07
[CODEVIEW_NB08_SIG 04E423038] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','8'] ; The result is NB08
[CODEVIEW_NB09_SIG 04E423039] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','9'] ; The result is NB09
[CODEVIEW_NB10_SIG 04E423130] ; [CODEVIEW_NB10_SIG  MAKESIG 'N','B','1','0'] ; The result is NB10
[CODEVIEW_NB11_SIG 04E423131] ; [CODEVIEW_NB11_SIG  MAKESIG 'N','B','1','1'] ; The result is NB11
[CODEVIEW_RSDS_SIG 052534453] ; [CODEVIEW_RSDS_SIG  MAKESIG 'R','S','D','S'] ; The result is RSDS
;;


;;

Note to Guga:

It seems that when a Lib file is used by the linker, it inserts the whole object file that
uses a specific function that was called by the source.

For example, if we have a source file with a Call to a external lib containing "Function01", and this lib file
have 45 different Object files. The only Object file that will be used and "linked" is the one containing the 
"Functino01" function. (Example, if this function exists only in Object 32.

What happens is that the whole Object32.obj (Inside that lib) is used and linked/injected inside to produce the new
application. This explains why some applications have too many nested data inside code. Because the Object file that
is injected is fixed. And it contains theit own Code and Data sectinos, but the final application interprets it as 
Code only to inject the function on the final code section.

Sometimes it don� happens, because when you build your file, the 1st to be done is to use the linked object/libs,
and after it is used, the compiler translates the Source code to produce the new object file. So this new object
file is surrounding the fixed object files. Like:


1st step:

Source Code
 --- Fixed Object File (Code Section)
 ----Fixed Object File (Data Section)
 
 2nd step , Linking
 
 Resultant Object File from source file. (Code Section from the new Object file) --|
 --- Fixed Object File (Code Section)                                              |
 ----Fixed Object File (Data Section)                                              |
 Resultant Object File from source file. (Data Section from the new Object file) --|
 
 Or something Like this.
 
 So, what we can do for the DIS ?
 
 Maybe the better is we once we identify a specific function, we check which Object File (Inside or not a lib)
 it belongs to, and then we just need to disassemble that whole object file that is evidently inside the exe.

Bingo !!!
This is exactly this :):):)
Check on ml.exe

We will see this function:
_flsall at 0043E359

It belongs to a specific object file 394 inside that consists in only this function.

Immediatelly after this function we have another one:

__mbsdec at 0043E3C6

This one belongs to other object file. (On the same lib)

Also, check the function _memcpy it contains nested code and data, and no debug info, but the relocs, and symbols seems
to be enough to find what are the data and code part of it.

;;



;;
 Note to Ren�

a) Assembler Errors

        Call 'KERNEL32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        Mov edi RegContent | Mov W$edi+1 'DX' | add edi 4
"       DwordToHex D$ebx+0A8
        Call 'KERNEL32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0


When i try to assemble a copy of RoAsm2028b with RosAsm2028b, it keeps inserting a " char like above.
It causes rosasm to now assemble the file. The reason is because after assemblying it and RosAsm do
his job, it changes the lines of the source code and adds this extra ".


LibScannner

a) Errors: 

In several places of the whole RosAsm source i found a mismatch of size in CRLF displacement.
It was displaying this:

Mov D$edi CRLF | add edi 2

When the correct is:

Mov W$edi CRLF | add edi 2

Like in the Error Tab, here:

Mov esi D@Text1, edi Trash1, ecx 0 | Mov D$edi CRLF | add edi 2
(...)
Mov esi D@Text2, edi Trash2, ecx 0 | Mov D$edi CRLF | add edi 2


b) On file libgccguga.a, and on libgcc.a it was crashing due to an memory error.


I Fixed it increasing allocated memory here:

  ; Allocate mem for Strings ('CompletionTable')
  ; and for list of pointers to Strings ('CompletionPointers'):

    Push ecx
        ;inc ecx | shl ecx 2 | or ecx 1
        inc ecx | shl ecx 5 | or ecx 1 ; -----------> Added this
        VirtualAlloc CompletionPointers ecx
    Pop ecx

    ;shl ecx 5 | or ecx 1
    shl ecx 8 | or ecx 1 ; -------------------------> Added this
    VirtualAlloc CompletionTable ecx | inc D$CompletionTable

  ; Fill the Tables:
    Mov edi D$CompletionTable, ebx D$CompletionPointers


Note: Although it is fixed, the same memory problem hapens on UAFXCWD.LIB
The file does not load completelly on the object listing and also don't load on the structure editor.
So, maybe we need to calculate and extend memory like in ReMapSourceMemoryIfNeeded
The reason is because the generated text file is something around 0C0D3407 bytes (202,20 Mb)

c) Even fixing the above memory problem. It still crashes on LIBCMTD.LIB
THis one i couldn't fix 

Proc SetQwordCheckSum:
    Argument @Pointer

    pushad

        Mov esi D@Pointer

        If B$esi < '0'
            ;
        Else_If B$esi <= '9'
            error D$NumerAsSymbolPtr
        End_If

        Call CheckSum64 | Call NoDuplication D@Pointer | Call CheckSum16

      ; The List Pointer is used to test empty Records (Lists Pointers can never be zero):
        .If D$CheckSumsRecords+ecx = 0
            On D$CheckSumsRecords+ecx+4 <> 0, jmp L1>
            Mov D$CheckSumsRecords+ecx eax
            Mov D$CheckSumsRecords+ecx+4 ebx
            Move D$CheckSumsRecords+ecx+8 D@Pointer
          ; D$CheckSumsRecords+ecx+12 = 0
        .Else
L1:         If D$CheckSumsRecords+ecx+12 = 0
                Move D$CheckSumsRecords+ecx+12 D$PointerToCheckSumsLinkedRecords
            Else
                Mov edi D$CheckSumsRecords+ecx+12
                While D$edi+12 <> 0 | Mov edi D$edi+12 | End_While
                Move D$edi+12 D$PointerToCheckSumsLinkedRecords
            End_If

            Mov edi D$PointerToCheckSumsLinkedRecords
            Mov D$edi eax ; -----------------------------> Crashes here

d) Fixed Loading BookMarks when Loading the MRU files.

When i loaded a file through the MRU List, the bookmarks where not being loaded.
I fixed here (MainProc TAB):



            .Else_If eax > 3000             ; Most Recently Used Files.
                If eax < 3005
                    Push eax
                        Call Security       ; 'Security' also uses eax as return Value.
                    Pop ebx
                    On eax = &IDCANCEL, jmp L9>>
                        Mov eax ebx         ; ... and 'LoadRMUfile' as input.
                        Call LoadRMUfile | Call EnableMenutems | Call LoadBookMarks ; added the Call to LoadBookmarks


e) Small codecomplete error.

I added some new equates for CodeView 4, but if i start typing:

"&CV4_" and press control space an error messagebox shows up saying that it can� find the equate and ask me to build a list.

But, if i type only: "&CV" and press control+space, the list displays properly

I added some equates like: &CV4_COMPILE_MACHINE_I80


f) RosAsm is crashing on Preparse Equal

If i write at the top of this lib "PREPARSE Equal" (After the Title) RosAsm crashes badly

g) Disassembler error on Floating Point Data.

Here:

[Data04D17F4: T$ 1.414214]
[<Data04D17FE: B$ 0, 0, 080, 07F  B$ 0, 0]
[Data04D1804: B$ 0, 048, 0C0, 0FF, F$ 0.5, F$ -1.443411e+035, F$ -4.952773e-007
                 F$ 2.295747e-041, F$ 4.573838e-041]
                 
Should be:

[Data04D17F4: T$ 1.414214]
[Data04D17FE: F$ 3.4028237e+038]
[Data04D1804: F$ -5.1117124e+038, F$ 0.5, F$ -1.443411e+035, F$ -4.952773e-007
                 F$ 2.295747e-041, F$ 4.573838e-041]

This is because all of them points to Fld instructions, like:

here:
    fld F$Data04D17FE
jmp E4>  ; Code04AAF8C
    
Code04AAF1B: J1:


and here:
    
Code04AAFD2: L4:
    fstp ST0
    fld F$Data04D1804
    
Code04AAFDA: M2:

h) Disassembler error on MMX mnemonic.

On testing.exe from C:\Guga\EvilHomerNewSite\site\stig.servehttp.com\homer\DirectX\Example1, ehit is being disasembled:

    cvtpq2ps XMM0 XMM2                ; 0F 5B C2 
    cvtpq2ps XMM2 XMM7                ; 0F 5B D7 
    mulps XMM0 XMM1                   ; 0F 59 C1 
    mulps XMM3 XMM2                   ; 0F 59 DA 
    Pop ecx                           ; 59 
    ret                               ; C3 
    
Code040A4CC: B2:

But the correct is cvtdq2ps, like:

    cvtdq2ps XMM0 XMM2                ; 0F 5B C2 
    cvtdq2ps XMM2 XMM7                ; 0F 5B D7 
    mulps XMM0 XMM1                   ; 0F 59 C1 
    mulps XMM3 XMM2                   ; 0F 59 DA 
    Pop ecx                           ; 59 
    ret                               ; C3 
    
Code040A4CC: B2:

I changed here:


cvtdq2ps

Op5B:
    .If B$EscapePrefix = &FALSE
        Mov D$edi 'Pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            Mov D$edi 'ebx ' | add edi 3
        Else
           ; inc D$UnLikelyCode
            Mov W$edi 'bx' | add edi 2
        End_If

    .Else

        ;inc D$UnLikelyCode
;        Mov D$edi 'cvtp'
        If B$OperandSizeOverride = &TRUE ; CVTPS2DQ xmm1, xmm2/m128
            Mov D$edi 'cvtp'
            Mov D$edi+4 's2dq'
        Else                             ; CVTDQ2PS xmm1, xmm2/m128
            Mov D$edi 'cvtd'            ; Guga fix. It must be cvtd
            Mov D$edi+4 'q2ps'          ; Guga Error Here. It is d2ps and not q2ps
        End_If
        Mov B$edi+8 SPC | add edi 9 | jmp Dis_xmm1__xmm2_m128

Also on this same file we have several problems of bad interpretations of code as data. It comes for example from here:

All of the Pointers are to Code and Not data, like "Mov D$ecx  Code04124BB" ; "Mov D$ecx+04 Code0411707" etc etc

Code0406E53: O7:
    Call Code0406D6F
    test al 020
    Mov ecx D$esp+04 | je Code040708C
    test al al | jns Code040708C
    test ah 01
    Mov D$ecx  Data04124BB ; error here it is a pointer to code and not to data
    Mov D$ecx+04 Data0411707 ; error here it is a pointer to code and not to data
    Mov D$ecx+08 Data041277C ; error here it is a pointer to code and not to data
    Mov D$ecx+0C Data040D957 ; error here it is a pointer to code and not to data
    Mov D$ecx+014 Data0412510 ; error here it is a pointer to code and not to data
    Mov D$ecx+018 Data0411778 ; error here it is a pointer to code and not to data
    Mov D$ecx+01C Code04117DF
    Mov D$ecx+020 Data04127FE ; error here it is a pointer to code and not to data
    Mov D$ecx+024 Data0412549 ; error here it is a pointer to code and not to data
    Mov D$ecx+028 Code0411CBF
    Mov D$ecx+02C Data040DB89 ; error here it is a pointer to code and not to data
    Mov D$ecx+030 Data0412BA6 ; error here it is a pointer to code and not to data
    Mov D$ecx+034 Data0412BE9 ; error here it is a pointer to code and not to data
    Mov D$ecx+038 Code0411CC4
    Mov D$ecx+03C Code0411E4C
    Mov D$ecx+040 Data041285D ; error here it is a pointer to code and not to data
    Mov D$ecx+044 Data04125AD ; error here it is a pointer to code and not to data
    Mov D$ecx+048 Data04116EB ; error here it is a pointer to code and not to data
    Mov D$ecx+04C Data040C47C ; error here it is a pointer to code and not to data
    Mov D$ecx+050 Data040C515 ; error here it is a pointer to code and not to data
    Mov D$ecx+054 Code0412D8B
    Mov D$ecx+058 Data0412CCD ; error here it is a pointer to code and not to data
    Mov D$ecx+05C Data040C572 ; error here it is a pointer to code and not to data
    Mov D$ecx+060 Data040C5B6 ; error here it is a pointer to code and not to data
    Mov D$ecx+064 Data040C9E7 ; error here it is a pointer to code and not to data
    Mov D$ecx+068 Data040DC58 ; error here it is a pointer to code and not to data
    Mov D$ecx+06C Data040DCA8 ; error here it is a pointer to code and not to data
    Mov D$ecx+070 Data040DD93 ; error here it is a pointer to code and not to data
    Mov D$ecx+074 Data040CAAC ; error here it is a pointer to code and not to data
    Mov D$ecx+078 Data040DE6F ; error here it is a pointer to code and not to data
    Mov D$ecx+07C Data040DED2 ; error here it is a pointer to code and not to data
    Mov D$ecx+080 Data040DF30 ; error here it is a pointer to code and not to data
    Mov D$ecx+084 Data040DF89 ; error here it is a pointer to code and not to data
    Mov D$ecx+088 Data0412DEF ; error here it is a pointer to code and not to data
    Mov D$ecx+08C Code040CB18
    Mov D$ecx+090 Data040CC4F ; error here it is a pointer to code and not to data
    Mov D$ecx+094 Data040DFD0 ; error here it is a pointer to code and not to data
    Mov D$ecx+098 Data040E078 ; error here it is a pointer to code and not to data
    Mov D$ecx+09C Code040E1FF
    Mov D$ecx+0A0 Data040F397 ; error here it is a pointer to code and not to data
    Mov D$ecx+0A4 Data040F53C ; error here it is a pointer to code and not to data
    Mov D$ecx+0A8 Code040FA70
    Mov D$ecx+0AC Data040CCDC ; error here it is a pointer to code and not to data
    Mov D$ecx+0B0 Data040CD6E ; error here it is a pointer to code and not to data
    Mov D$ecx+0D8 Data041298F ; error here it is a pointer to code and not to data
    Mov D$ecx+0CC Data041183E ; error here it is a pointer to code and not to data
    Mov D$ecx+0C0 Data0412645 ; error here it is a pointer to code and not to data
    Mov D$ecx+0B8 Data041268E ; error here it is a pointer to code and not to data
    Mov D$ecx+0D0 Data0412A00 ; error here it is a pointer to code and not to data
    Mov D$ecx+0C4 Data04118AF ; error here it is a pointer to code and not to data
    Mov D$ecx+0DC Data040CDD2 ; error here it is a pointer to code and not to data
    Mov D$ecx+0E0 Data040CE79 ; error here it is a pointer to code and not to data
    Mov D$ecx+0D4 Data0412AC8 ; error here it is a pointer to code and not to data
    Mov D$ecx+0BC Data04126D1 ; error here it is a pointer to code and not to data
    Mov D$ecx+0C8 Data0411983 ; error here it is a pointer to code and not to data
    Mov D$ecx+010 Data0410FBF ; error here it is a pointer to code and not to data
    Mov D$ecx+0B4 Code040CF1E
    Mov D$ecx+0E8 Code0412200
    Mov D$ecx+0E4 Code0412340 | je H6>  ; Code040708C
    test ah 02 | je H6>  ; Code040708C
    Mov D$ecx+03C Code0412072
    Mov D$ecx+048 Code0410CDD
    Mov D$ecx+098 Data040E14A ; error here it is a pointer to code and not to data
    Mov D$ecx+09C Code040FB89
    Mov D$ecx+060 Data040C7D2 ; error here it is a pointer to code and not to data
    
Code040708C: H6:
    test al 040 | je A0>  ; Code04070E0
    Mov D$ecx+0F8 Code040BE40
    Mov D$ecx+0FC Code040BEE0
    Mov D$ecx+0100 Data040C200 ; error here it is a pointer to code and not to data
    Mov D$ecx+0EC Data040B7C0 ; error here it is a pointer to code and not to data
    Mov D$ecx+0F0 Data040BA20 ; error here it is a pointer to code and not to data
    Mov D$ecx+0F4 Data040BC80 ; error here it is a pointer to code and not to data
    Mov D$ecx+0104 Code040B700
    Mov D$ecx+0110 Code040B640
    
Code04070E0: A0:
    ret


You will also note that even fixing this we have other errors like it may result in this (After fixing the Data to Code problem):

Code040C47C: M4:
    Mov edx D$esp+0C
    Mov eax D$esp+08
    movq MM2 Q$edx 
    movq MM3 Q$edx+08
    movq MM4 MM2
    movq MM5 MM3
    movq MM0 Q$eax 
    punpckhdq MM4 MM2
    punpckhdq MM5 MM3
    movq MM1 Q$eax+08
    punpckldq MM4 MM2
    punpckldq MM5 MM3
    Mov eax D$esp+04
    movq MM6 MM4
    movq MM7 MM5
    psrlw MM6 Q$edi+ecx+089E02DEF ; Error Here
    inc ecx
    add B$edi  cl
    psubusb MM6 Q$edi+ecx+089E015EF
    inc ecx
    add B$edi  cl
    psubsb MM6 Q$edi+ecx+0FB4E10F
    psrlq MM3 Q$esi+0F1A6F0F
    pmulhw MM3 Q$edx+086A6F0F
    psubw MM6 Q$edi+ecx+089E035EF
    inc ecx
    add B$edi  cl
    psrad MM5 Q$esi+0B4F00F0F
    pxor MM5 Q$Data04189E0
    psubusb MM6 Q$edi+ecx+0F0F207F
jmp 0F74FD4B7

But the correct is:
Code040C47C: M4:

.text:0040C47C                 Mov     edx, [esp+0Ch]
.text:0040C480                 Mov     eax, [esp+8]
.text:0040C484                 movq    mm2, qword ptr [edx]
.text:0040C487                 movq    mm3, qword ptr [edx+8]
.text:0040C48B                 movq    mm4, mm2
.text:0040C48E                 movq    mm5, mm3
.text:0040C491                 movq    mm0, qword ptr [eax]
.text:0040C494                 punpckhdq mm4, mm2
.text:0040C497                 punpckhdq mm5, mm3
.text:0040C49A                 movq    mm1, qword ptr [eax+8]
.text:0040C49E                 punpckldq mm4, mm2
.text:0040C4A1                 punpckldq mm5, mm3
.text:0040C4A4                 Mov     eax, [esp+4]
.text:0040C4A8                 movq    mm6, mm4
.text:0040C4AB                 movq    mm7, mm5
.text:0040C4AE                 pfmul   mm2, mm1
.text:0040C4B2                 pxor    mm5, ds:qword_4189E0
.text:0040C4B9                 pfmul   mm3, mm0
.text:0040C4BD                 pxor    mm2, ds:qword_4189E0
.text:0040C4C4                 pfmul   mm5, mm0
.text:0040C4C8                 pfmul   mm4, mm1
.text:0040C4CC                 pfadd   mm2, mm3
.text:0040C4D0                 movq    mm3, qword ptr [edx]
.text:0040C4D3                 pfsub   mm4, mm5
.text:0040C4D7                 movq    mm5, qword ptr [edx+8]
.text:0040C4DB                 pfmul   mm7, mm1
.text:0040C4DF                 pxor    mm6, ds:qword_4189E0
.text:0040C4E6                 pfacc   mm4, mm2
.text:0040C4EA                 pfmul   mm6, mm0
.text:0040C4EE                 pxor    mm5, ds:qword_4189E0
.text:0040C4F5                 pfmul   mm3, mm0
.text:0040C4F9                 movq    qword ptr [eax], mm4
.text:0040C4FC                 pfmul   mm5, mm1
.text:0040C500                 pfadd   mm6, mm7
.text:0040C504                 pfsub   mm5, mm3
.text:0040C508                 pfacc   mm6, mm5
.text:0040C50C                 movq    qword ptr [eax+8], mm6
.text:0040C510                 femms
.text:0040C512                 retn    0Ch
.text:0040C515 ; ---------------------------------------------------------------------------


;;




;;

Guga changes
a)

                    Else_If B$esi-1 = 0E9
                        test B$eax EVOCATED | jz L1>
                      ; 2 or more JMP Instructions to the same location:
                        ;or B$eax PUSH_EBP+NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL
                        ; GugaNote This is a jmp Call
                        or B$eax NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL
                        Mov ebx eax | sub ebx D$RoutingMap | add ebx D$SectionsMap
                        Mov B$ebx CODEFLAG
                        jmp L5>
                        
b)

This is for the JCC
    While edi < edx
        If B$esi > 3
            On B$edi = 0, Mov B$edi CODEFLAG
            On B$edi = CODEFLAG, Mov B$ebx INSTRUCTION+EVOCATED+LABEL+ACCESSED;+PUSH_EBP
        End_If
        inc esi, edi, ebx
    End_While
;;


;;

More errors.

On ntdll.dll (D:\RosAsm\dlls\NewDlls)

Here:

    Mov D$eax+0BC es                  ; 8C 88 BC 00 00 00 
    Mov D$eax+098 es                  ; 8C 98 98 00 00 00 
    Mov D$eax+094 es                  ; 8C 80 94 00 00 00 
    Mov D$eax+090 es                  ; 8C A0 90 00 00 00 
    Mov D$eax+08C es                  ; 8C A8 8C 00 00 00 
    Mov D$eax+0C8 es                  ; 8C 90 C8 00 00 00 
    Mov D$eax  010007                 ; C7 00 07 00 01 00 
    Push 01                           ; 6A 01 
    Push eax                          ; 50 
    Push D$ebp+08                     ; FF 75 08 
    Call Code078468A5C                ; E8 29 C5 FD FF 
(...)
    Call Code07848C4A0                ; E8 48 FF FF FF 
RtlRaiseStatus:: Code07848C558: P2:

The "es" is repeated. It dhould be:

    Mov D$eax+0BC cs                  ; 8C 88 BC 00 00 00 
    Mov D$eax+098 ds                  ; 8C 98 98 00 00 00 
    Mov D$eax+094 es                  ; 8C 80 94 00 00 00 
    Mov D$eax+090 fs                  ; 8C A0 90 00 00 00 
    Mov D$eax+08C gs                  ; 8C A8 8C 00 00 00 
    Mov D$eax+0C8 ss                  ; 8C 90 C8 00 00 00 
    Mov D$eax  010007                 ; C7 00 07 00 01 00 
    Push 01                           ; 6A 01 
    Push eax                          ; 50 
    Push D$ebp+08                     ; FF 75 08 
    Call Code078468A5C                ; E8 29 C5 FD FF 
(...)
    Call Code07848C4A0                ; E8 48 FF FF FF 
RtlRaiseStatus:: Code07848C558: P2:


The error was here:

WriteEffectiveAddressFromModRm:  ; 044 00_100_100
(...)

    .Else_If al = 2
        Call StartEffectiveAddress | RmMask bl To al

        If al = 0      | Mov D$edi 'eax+' | add edi 4
        Else_If al = 1 | Mov D$edi 'ecx+' | add edi 4
        Else_If al = 2 | Mov D$edi 'edx+' | add edi 4
        Else_If al = 3 | Mov D$edi 'ebx+' | add edi 4
        Else_If al = 4 | Call WriteFromSib
            ..If B$edi-1 <> '+'
                Mov B$edi '+' | inc edi
            ..End_If
        Else_If al = 5 | Mov D$edi 'ebp+' | add edi 4
        Else_If al = 6 | Mov D$edi 'esi+' | add edi 4
        Else           | Mov D$edi 'edi+' | add edi 4
        End_If

        ; Call Writedis32 ; ----> Here was the error. The values of eax and ebx was changing
        Push ebx | Push eax | Call Writedis32 | Pop eax | Pop ebx ; --> This was my preliminary fix.

    .Else ; bl = 3

The 1st fix i made was only a palleative way to retrieve back the values of eax and ebx in order to don�t change
the values of bl and al, but this is not the proper way to go, because we have several other functions that Call to
this Writedis32 and also in some functions below or after it.

All of those surrounding functions changes the values, because they are not preserved. The functions are top-down, like:

WriteBase5dis32: ; starting address
    If B$edi-1 <> '+'
        Mov B$edi '+' | inc edi
    End_If

WriteDis32:
    If B$AddressSizeOverride = &FALSE
        lodsd
    Else
        lodsw | and eax 0FFFF | Call WriteEax | ret
    End_If

WriteDisRelative:
    Mov D$LastCodeRef eax | On eax = 0, jmp L8>>

L0: If B$SimpleScan = &TRUE
        Mov B$DisFlag DISDONE+DISLINEOVER | ret
    End_If

So i fix it all only adding a Proc macro and a use macro in all functinos and adding a Call to the next function in sequence,
like this:


Proc WriteBase5dis32:
    Uses eax, ebx, ecx, edx
    
    If B$edi-1 <> '+'
        Mov B$edi '+' | inc edi
    End_If
    Call WriteDis32
EndP

Proc WriteDis32:
    Uses eax, ebx, ecx, edx

    If B$AddressSizeOverride = &FALSE
        lodsd
    Else
        lodsw | and eax 0FFFF | Call WriteEax | ExitP;ret
    End_If
    Call WriteDisRelative
EndP

Proc WriteDisRelative:
    ;Uses eax, ebx, ecx, edx ; Maybe not use eax here bccause of  | jnz WriteDisRelative
    Uses ebx, ecx, edx ; Maybe not use eax here

    Mov D$LastCodeRef eax | On eax = 0, jmp L8>>

L0: If B$SimpleScan = &TRUE
        Mov B$DisFlag DISDONE+DISLINEOVER | ExitP;ret
    End_If

L0: On B$WeAreInTheCodeBox = &TRUE, jmp L8>>

    sub eax D$DisImageBase | add eax D$SectionsMap

    On eax >= D$EndOfSectionsMap, jmp L8>>
    On eax <= D$SectionsMap, jmp L8>>

    Mov B$ToJumpsTable &FALSE
    Mov al B$eax | and al DATAFLAG+VIRTUALFLAG+IMPORTFLAG+CODEFLAG+CONSTANTFLAG

    ..If al = 0
        Mov eax D$LastCodeRef

        Mov ebx eax | sub ebx D$DisImageBase | add ebx D$SizesMap

            .If B$LeaInstruction = &TRUE
               ; or B$ebx POINTER | Mov B$LeaInstruction &FALSE
            .Else_If W$edi-2 = 'B$'
                or B$ebx BYTE
                sub ebx D$SizesMap | add ebx D$SectionsMap | Mov B$ebx DATAFLAG
            .Else_If W$edi-2 = 'W$'
                or B$ebx WORD
                sub ebx D$SizesMap | add ebx D$SectionsMap
                Mov B$ebx DATAFLAG, B$ebx+1 DATAFLAG
            .Else_If W$edi-2 = 'D$'
                or B$ebx DWORD
                sub ebx D$SizesMap | add ebx D$SectionsMap
                Mov D$ebx FOURDATAFLAGS
            .Else_If W$edi-2 = 'F$'
                or B$ebx FP4
                sub ebx D$SizesMap | add ebx D$SectionsMap
                Mov D$ebx FOURDATAFLAGS
            .Else_If W$edi-2 = 'R$'
                or B$ebx FP8
                sub ebx D$SizesMap | add ebx D$SectionsMap
                Mov D$ebx FOURDATAFLAGS, D$ebx+4 FOURDATAFLAGS
            .Else_If W$edi-2 = 'T$'
                or B$ebx FP10
                sub ebx D$SizesMap | add ebx D$SectionsMap
                Mov D$ebx FOURDATAFLAGS, D$ebx+4 FOURDATAFLAGS, D$ebx+6 FOURDATAFLAGS
            .Else
                jmp L8>>
               ; or B$ebx POINTER
            .End_If

    ..Else_If al = DATAFLAG
        Mov eax D$LastCodeRef | Call StoreDisSize
        sub eax D$DisImageBase | add eax D$RoutingMap | or B$eax LABEL+EVOCATED
        Mov D$edi 'Data' | add edi 4 | jmp L8>>

    ..Else_If al = CODEFLAG
      ; Is it a Call to a Jumps Table?
        Mov eax D$LastCodeRef
        sub eax D$DisImageBase | add eax D$UserPeStart

        If W$eax = 025FF ; Code of jmp relative long
            Mov ebx D$eax+2 | sub ebx D$DisImageBase | add ebx D$SectionsMap
            On ebx > D$EndOfSectionsMap, jmp L1>
            On ebx < D$SectionsMap, jmp L1>
                On B$ebx <> IMPORTFLAG, jmp L1>

                    Mov B$ApiCommentWanted &TRUE, ebx D$eax+2, D$PtrToApiName ebx

                  ; On B$WithSymbolicsAnalyzes = &FALSE, Mov B$ToJumpsTable &TRUE
;                    Push eax
;                        Mov al B$WithSymbolicsAnalyzes | xor al &TRUE
;                        Mov B$ToJumpsTable al
;                        On B$WithoutJumpsTableCalls = &TRUE, Mov B$ToJumpsTable &FALSE
;                    Pop eax
;                    Mov ebx D$eax+2 | jmp L5>>
        End_If

L1:     Mov eax D$LastCodeRef
        sub eax D$DisImageBase | add eax D$RoutingMap
        test B$eax INSTRUCTION | jz L8>>
        or B$eax NODE+LABEL | Mov D$edi 'Code' | add edi 4 | jmp L8>>

    ..Else_If al = VIRTUALFLAG
        Mov eax D$LastCodeRef | Call StoreDisSize
        sub eax D$DisImageBase | add eax D$RoutingMap | or B$eax LABEL+EVOCATED
        Mov D$edi 'Virt', D$edi+4 'ual ' | add edi 7 | jmp L8>>

    ..Else_If al = CONSTANTFLAG
L1:     Mov eax D$LastCodeRef | Call StoreDisSize
        sub eax D$DisImageBase | add eax D$RoutingMap | or B$eax LABEL+EVOCATED
        Mov D$edi 'Cons', D$edi+4 'tant', B$edi+8 SPC | add edi 8 | jmp L8>>

    ..Else_If al = IMPORTFLAG
        Mov ebx D$LastCodeRef
L5:     sub ebx D$DisImageBase | add ebx D$UserPeStart | Mov ebx D$ebx
      ; May be a wrong pointing inside the .Import!
      ; Add a Pointer test!
        On ebx < D$ApiBuffer, ExitP;ret
        On ebx >= D$EndOfApiBuffer, ExitP;ret

        Push esi

            On W$edi-2 = 'D$', sub edi 2

            Mov esi ebx

            .If D$edi-4 = 'jmp '    ; Jumps Table?
                Call WriteApiJmpTableLabel

                While B$esi <> 0 | movsb | End_While

            .Else_If D$edi-4 = 'all '  ; Call api?
                Call FlagNoReturnApi
                Call FlagApiProcedures
                While B$esi <> 0 | movsb | End_While

          ; Other case: Either "Mov eax D$ApiCall" or "Mov D$eax ApiCall"
            .Else
                Push edi
                    Mov al "'"
                    While B$edi > LF
                        dec edi
                        On B$edi = '$', Mov al 0
                    End_While
                Pop edi
                Mov esi ebx
                If al = 0
                    While B$esi <> '.' | inc esi | End_While | inc esi
                End_If
                While B$esi <> 0 | movsb | End_While
                On al = 0, dec edi

            .End_If
        Pop esi

     ..End_If

    ExitP;ret
______________________________________________
; Note to Ren�: All of this is for your Tests ?

L7:         Pop ebx, eax, esi
            Mov B$edi ':' | inc edi | NextDisLine

            Mov D$edi 'jmp ' | add edi 4
        .End_If
ExitP;ret

       ; If B$ToJumpsTable = &FALSE
       ;     Mov B$edi "'" | inc edi
       ; End_If
        Push esi
            Mov esi ebx | While B$esi <> 0 | movsb | End_While
        Pop esi
      ;  Mov eax D$edi-4 | or eax 0202020 | On eax = '.dll' | sub edi 4
      ;  Mov B$edi '.' | inc edi
;      ; Switch to the real Import for reading the Pointer to Function Name:
;        sub ebx D$RoutingMap | add ebx D$UserPeStart
;        Test D$ebx 0_8000_0000 | jnz L6>
;            Push esi
;                Mov esi D$ebx | add esi 2 | add esi D$UserPeStart
;                If esi < D$UserPeStart
;D0:                 Mov B$DisFlag DISFAILED | Pop esi | ret
;                Else_If esi > D$UserPeEnd
;                    jmp D0<
;                End_If
;                While B$esi <> 0 | movsb | End_While
;            Pop esi
;            If B$ToJumpsTable = &FALSE
;                Mov B$edi "'" | inc edi
;            End_If
            ExitP;ret

;L6:     Mov eax D$ebx | xor eax 0_8000_0000 | Call WriteEax
;        If B$ToJumpsTable = &FALSE
;            Mov B$edi "'" | inc edi
;        End_If
        ExitP;ret

    ..Else_If al = 0
        Mov ebx D$LastCodeRef
        sub ebx D$DisImageBase | add ebx D$RoutingMap
        Mov eax D$ebx
        If eax = 0
            sub ebx D$RoutingMap | add ebx D$SectionsMap
            Mov B$ebx CONSTANTFLAG | jmp L1<<
        End_If
    ..End_If
_______________________________________

L8: On B$WeAreInTheCodeBox = &FALSE, jmp L8>
    On D$LibFileMemory = 0, jmp L8>
        Push esi
            Mov esi D$LastCodeRef
            .If esi > D$LibFileMemory
                Mov eax D$LibFileMemory | add eax D$LibFileLength
                If esi < eax
                    While B$esi <> 0 | movsb | End_While
                   ; Mov D$edi ' ; <', D$edi+4 '<<<<' | add edi 8
                Else
                    Pop esi | jmp L8>
                End_If
            .Else
                Pop esi | jmp L8>
            .End_If
        Pop esi
        ExitP;ret

L8: ;On D$LastCodeRef = 0438E28, int3

    .If B$edi-1 = '+'
        If W$edi-3 = '*2'
            Call TryWithIndice 2
        Else_If W$edi-3 = '*4'
            Call TryWithIndice 4
        Else_If W$edi-3 = '*8'
            Call TryWithIndice 8
        End_If
    .End_If

    If W$DisplacementFromLabel = 0
        Mov eax D$LastCodeRef | sub eax D$DisImageBase | add eax D$SizesMap
        Mov ebx D$SizesMap | add ebx 4
        On eax < ebx, jmp L8>
        On eax > D$EndOfSizesMap, jmp L8>
        test B$eax-4 FP8 | jz L8>
            sub D$LastCodeRef 4 | Mov W$DisplacementFromLabel '+4'
    End_If

L8: Push 0-1

    Mov ebx D$LastCodeRef

L0: Mov eax ebx | shr ebx 4 | and eax 0F
    add eax '0' | On eax > '9', add eax 7
    Push eax
    cmp ebx 0 | ja L0<

    Mov B$edi '0' | inc edi
L0: Pop eax | cmp eax 0-1 | je L9>
    Mov B$edi al | inc edi | jmp L0<

L9: If W$DisplacementFromLabel <> 0
        Mov ax W$DisplacementFromLabel | stosw
        Mov W$DisplacementFromLabel 0
    End_If

    Mov eax D$LastCodeRef | sub eax D$DisImageBase | add eax D$SectionsMap

    .If eax < D$SectionsMap
        ;
    .Else_If eax < D$EndOfSectionsMap
        If B$eax = DATAFLAG
            sub eax D$SectionsMap | add eax D$RoutingMap | or B$eax LABEL+EVOCATED
        End_If
    .End_If

    On B$ApiCommentWanted = &TRUE, Call WriteApiLabelComment
;ret
EndP



------------------------

i Also changed here:


BuildTruthAsciiTable:
    VirtualAlloc TruthAsciiTable 256
    Mov edi D$TruthAsciiTable, al BADASCII, ecx 256 | rep stosb

    Mov edi D$TruthAsciiTable, B$edi 0
    Mov B$edi+LF GOODASCII, B$edi+CR GOODASCII,
        B$edi+0A7 GOODASCII,    ; $
        B$edi+025 GOODASCII,    ; %
        B$edi+0A9 GOODASCII,    ; �
        B$edi+02F GOODASCII     ; /

    Mov ebx 32, ecx 127;8
    While ebx =< ecx
        Mov B$edi+ebx GOODASCII | inc ebx
    End_While

  ; 128 - 32 + 6 > 102
ret

Chars 127 and 128 are not Good Ascii
;;







;;
    lstrcmpi

    This function compare two Null Terminated Ascii strings (With the same size or not). It looks for the
    Language version of your system to order the Strings.
    The functionality is exactly the same as on lstrcmpiA inside Kernel32.dll.
    
    Arguments:
    
    String1:    First ascii null terminated to be inputed
    String2:    Second ascii null terminated to be inputed
    
    Return Values:
        If the Strings are the same, eax returns 0
        If the 1st String alphabetically preceeds the 2nd String, eax returns 0-1.
        If the 1st String is alphabetically located after the 2nd String, eax returns 1.
        
    Example of usage and their return values:

    Call lstrcmpi {B$ "AB" EOS} {B$ "AB" EOS} ; Eax = 0
    Call lstrcmpi {B$ "AB" EOS} {B$ "CD" EOS} ; Eax = 0-1
    Call lstrcmpi {B$ "CD" EOS} {B$ "AB" EOS} ; Eax = 1
    Call lstrcmpi {B$ "RosAsm" EOS} {B$ "Assembly" EOS} ; Eax = 1
    Call lstrcmpi {B$ "Assembly" EOS} {B$ "RosAsm" EOS} ; Eax = 0-1
    
    Note: If you are using this function to organize (list) strings. The starting order in crescent sttrings.
    Like the following list of strings:
        
        Home, House, Indian, RosAsm, Slovak, Story

    If you are trying to build a list of strings without having to use the Languiage definitions of your system, use
    the functions:
    
    strcmp - The same functionality as this lstrcmpi, but built on a simplified way to not use the Lang definitions.
             It will displays the order of strings in crescent order.

    stricmp - The same functionality as this lstrcmpi, but built on a simplified way to not use the Lang definitions.
             It will displays the order of strings in decrescent order.
;;
;;
Proc lstrcmpi:
    Arguments @String1, @String2
    uses ebx, esi, ecx, edx

    Call 'KERNEL32.GetThreadLocale'
    Call 'KERNEL32.CompareStringA' eax, &LOCALE_ILANGUAGE__&LOCALE_USE_CP_ACP, D@String1, 0-01, D@String2, 0-1

    ...If eax = 0

        Call 'KERNEL32.GetSystemDefaultLCID'
        Call 'KERNEL32.CompareStringA' eax, &LOCALE_ILANGUAGE__&LOCALE_USE_CP_ACP, D@String1, 0-01, D@String2, 0-1

        ..If eax = 0

            .If D@String1 <> 0

                If D@String2 <> 0
                    Call stricmp D@String1, D@String2
                Else_If D@String1 <> 0 ; There is an error on the original dll. This may be eax and not String1
                    Mov eax 1
                Else
                    Mov eax D@String2
                    neg eax
                    sbb eax eax
                End_If
                ExitP

            .End_If

            Mov eax D@String2
            neg eax
            sbb eax eax
            ExitP

        ..End_If

    ...End_If

    add eax 0-02

EndP




;;
;;
    Stricmp

    This function compare two Null Terminated Ascii strings (With the same size or not). It is a simplified version
    of lstrcmpi from kernel32.dll, with the diference that in this function it does not look for the
    Language version of your system, and also it reverses the order if a user is building a List of Strings.
    For example, on lstrcmpi the initial order of a String Listings is crescent. On this function the starting order
    is decrescent.
    The functionality is exactly the same as on _stricmp and _strcmpi inside ntdll.dll.
    It handle only latin alphabet without accents. So, only vowels and consonants from A to Z.
    
    To display a list of files in crescent alphabetical order, use Strcmp function.
    
    Arguments:
    
    String1:    First ascii null terminated to be inputed
    String2:    Second ascii null terminated to be inputed
    
    Return Values:
        If the Strings are the same, eax returns 0
        If the 1st String alphabetically preceeds the 2nd String, eax returns 1.
        If the 1st String is alphabetically located after the 2nd String, eax returns 0-1.
        
    Example of usage and their return values:
        
    Call stricmp {B$ "AB" EOS} {B$ "AB" EOS} ; Eax = 0
    Call stricmp {B$ "AB" EOS} {B$ "CD" EOS} ; Eax = 1
    Call stricmp {B$ "CD" EOS} {B$ "AB" EOS} ; Eax = 0-1
    Call stricmp {B$ "RosAsm" EOS} {B$ "Assembly" EOS} ; Eax = 0-1
    Call stricmp {B$ "Assembly" EOS} {B$ "RosAsm" EOS} ; Eax = 1
    
    Note: Used Macros. Default RosAsm macro system, plus:
    
    [If_And    | If #1 #2 #3    | #+3]
    [.If_And   | .If #1 #2 #3   | #+3]
    [..If_And  | ..If #1 #2 #3  | #+3]
    [...If_And | ...If #1 #2 #3 | #+3]

    [Else_If_And    | Else    | If_And    #F>L]
    [.Else_If_And   | .Else   | .If_And   #F>L]
    [..Else_If_And  | ..Else  | ..If_And  #F>L]
    [...Else_If_And | ...Else | ...If_And #F>L]

    If you are using this function to organize (list) strings. The starting order in decrescent strings.
    Like the following list of strings:
        
        Story, Slovak, RosAsm, Indian, House, Home

;;
;;
Proc stricmp:
    Arguments @String1, @String2
    Uses edi, esi, ebx

    Mov esi D@String2
    Mov edi D@String1
    Mov al 0FF


    .Do
        Do
            On al = 0, jmp L2>
            Mov al B$esi
            inc esi
            Mov ah B$edi
            inc edi
        Loop_Until ah <> al

        ; Lower case convertion
        If_And al >= 'A', al <= 'Z'
            xor al 32
        End_If

        If_And ah >= 'A', ah <= 'Z'
            xor ah 32
        End_If

    .Loop_Until al <> ah
    sbb al al
    sbb al 0-1
L2:
    movsx eax al

EndP
;;
____________________________________________________________________________________________

;;
    String2Dword

    This function converts a Null Terminated Decimal Ascii String to a 32 Bits Dword Value.
    
    Arguments:
    
    String:    The inputed String to be converted
    
    Return Values:
        If the function suceeds eax returns the value of the dword string.
        If the function fails eax returns 0.
;;
;;
Proc String2Dword:
    Arguments @String
    Local @Result
    Uses ecx, edi, edx, esi

    Mov D@Result 0
    Mov edi D@String
    Mov ecx D@String
    On ecx = 0, ExitP

    Call StrLenProc edi

    .While B$ecx <> 0

        xor edx edx
        Mov dl B$edi
        sub dl '0'  ; subtrack each digit with "0" to convert it to hex value
        Mov esi eax
        dec esi
        Push eax
            Mov eax edx
            Push ebx
                Mov ebx 10;0A
                While esi > 0
                    mul ebx
                    dec esi
                End_While
            Pop ebx
            add D@Result eax
        Pop eax
        inc edi
        dec eax
        inc ecx
    .End_While
    Mov eax D@Result

EndP
;;
____________________________________________________________________________________________

;;
    Strcmp

    This function compare two Null Terminated Ascii strings (With the same size or not). It is a simplified version
    of lstrcmpi from kernel32.dll, with the diference that in this function it does not look for the
    Language version of your system. It also displays the same order if a user is building a List of Strings.
    For example, on lstrcmpi the initial order of a String Listings is crescent. On this function the starting order
    is also crescent.
    It handle only latin alphabet without accents. So, only vowels and consonants from A to Z.
    
    To display a list of files in decrescent alphabetical order, use Stricmp function.
        
    Arguments:
    
    String1:    First ascii null terminated to be inputed
    String2:    Second ascii null terminated to be inputed
    
    Return Values:
        If the Strings are the same, eax returns 0
        If the 1st String alphabetically preceeds the 2nd String, eax returns 0-1.
        If the 1st String is alphabetically located after the 2nd String, eax returns 1.
        
    Example of usage and their return values:
        
    Call strcmp {B$ "AB" EOS} {B$ "AB" EOS} ; Eax = 0
    Call strcmp {B$ "AB" EOS} {B$ "CD" EOS} ; Eax = 0-1
    Call strcmp {B$ "CD" EOS} {B$ "AB" EOS} ; Eax = 1
    Call strcmp {B$ "RosAsm" EOS} {B$ "Assembly" EOS} ; Eax = 1
    Call strcmp {B$ "Assembly" EOS} {B$ "RosAsm" EOS} ; Eax = 0-1
    
    Note: Used Macros. Default RosAsm macro system, plus:
    
    [If_And    | If #1 #2 #3    | #+3]
    [.If_And   | .If #1 #2 #3   | #+3]
    [..If_And  | ..If #1 #2 #3  | #+3]
    [...If_And | ...If #1 #2 #3 | #+3]

    [Else_If_And    | Else    | If_And    #F>L]
    [.Else_If_And   | .Else   | .If_And   #F>L]
    [..Else_If_And  | ..Else  | ..If_And  #F>L]
    [...Else_If_And | ...Else | ...If_And #F>L]

    Note: If you are using this function to organize (list) strings. The starting order in crescent sttrings.
    Like the following list of strings:
        
        Home, House, Indian, RosAsm, Slovak, Story

;;
;;
Proc strcmp:
    Arguments @String1, @String2
    Uses edi, esi, ebx

    Mov esi D@String1
    Mov edi D@String2
    Mov al 0FF


    .Do
        Do
            On al = 0, jmp L2>
            Mov al B$esi
            inc esi
            Mov ah B$edi
            inc edi
        Loop_Until ah <> al

        ; Lower case convertion
        If_And al >= 'A', al <= 'Z'
            xor al 32
        End_If

        If_And ah >= 'A', ah <= 'Z'
            xor ah 32
        End_If

    .Loop_Until al <> ah
    sbb al al
    sbb al 0-1
L2:
    movsx eax al

EndP


[szBuff0: B$ ? # &MAX_PATH]

[BASE_HEX 16, BASE_DEC 10, BASE_OCT 8, BASE_FOUR 4, BASE_BIN 2]

Proc AsciiBase:
    Arguments @String, @Base
    Uses esi, ebx, edx

        Mov esi D@String, eax 0, ebx 0

        While B$esi <> 0
            Mov eax ebx | mul D@Base | Mov ebx eax, eax 0
            Mov al B$esi | sub al '0'
          ; Cases of Hexa Notation:
            On al > 9, sub al 7
            add ebx eax | inc esi
        End_While

        Mov eax ebx
EndP
;;
;;
    Strcpy

    This function copies a Null Terminated String to a Buffer. The size of the Buffer must be bigger or equal to the
    size of the string.
    It have the same functionality as lstrcpyA in kernel32.dll
    
    Arguments:
    
    InBound:    The inputed String
    
    OutBound:   The Buffer which will hold the String.
    
;;
;;
Proc strcpy:
    Arguments @OutBound, @InBound
    Uses ecx, esi, edi

    xor ecx ecx
    Mov esi D@InBound

    Do
        inc ecx
        inc esi
    Loop_Until B$esi = 0

    Mov esi D@InBound | Mov edi D@OutBound | rep movsb
    Mov B$edi 0

EndP
;;
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
