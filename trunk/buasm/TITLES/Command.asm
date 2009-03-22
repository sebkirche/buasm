TITLE Command         ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________
;;
  Allows to pass commandlines to the debuggee. Command line options are loaded from text 
  files that have the same path and filename (with the extension '.CLO') as the destination 
  file. If there is no such file the app path is just copied to the buffer. 

  e.g. If DestinationFile is "D:\ping.EXE" and 'D:\ping.CLO' contains 
  "111.42.23.17" then CommandLine is set to "D:\ping.EXE 111.42.23.17". 
  If no CLO file exists CommandLine is set to "D:\ping.EXE".
  
  23-Oct-03 Ludwig Haehne - wkx@gmx.li
  
  28-Jun-04 - ExeName put in brackets
  
  11-Nov-04 - Complete rewrite - no 16k limit, deals with dll's
  
  26-Dec-04 - Separate the command line from the executable path and filename
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

; Setup the command line. The address of the string or zero is returned in eax.
; The buffer for the command line is allocated by SetupCommandLine and must be freed by the
; caller after use. Attention: Only free CommandLinePtr! The address returned in eax
; might point to a static buffer!

[CommandLinePtr: D$ ?]
[ScreensaverCommandLine: B$ '/S' EOS]
[BytesTransfered: D$ ?]

Proc SetupCommandLine:

    Local @H.File

    Uses esi edi

        Call GetCLOFilename
        Call 'KERNEL32.CreateFileA' CommandLineFile, &GENERIC_READ, &FILE_SHARE_READ,
                        &NULL, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, &NULL

        ; In general no clo file will be provided, if we deal with a screensaver put
        ; a /S command as a default command and exit proc
        .If eax = &INVALID_HANDLE_VALUE
            If D$SavingExtension = '.SCR'
                Mov eax ScreensaverCommandLine
            Else
                Mov eax 0
            End_If
            ExitP
        .End_If

        Mov D@H.File eax

        ; Allocate buffer for commandline (must be freed by caller)
        Call 'KERNEL32.GetFileSize' D@H.File, 0
        If eax = 0_FFFF_FFFF
            Call ReportWinError {B$ 'SetupCommandLine: GetFileSize' EOS}
            Call 'KERNEL32.CloseHandle' D@H.File
            Mov eax 0
            ExitP
        End_If

        Call VirtualAlloc CommandLinePtr,
                          eax

        Mov edi D$CommandLinePtr

        ; Read the command line parameters from the file
        Call 'KERNEL32.ReadFile' D@H.File, edi, eax, BytesTransfered, &NULL
        add edi D$BytesTransfered
        Mov B$edi EOS

        Call 'KERNEL32.CloseHandle' D@H.File
        Mov eax D$CommandLinePtr
EndP
____________________________________________________________________________________________

[CommandLineFile: B$ ? # &MAX_PATH]

GetCLOFilename:

    Mov esi MainName,
        edi CommandLineFile

    While B$esi <> EOS

        movsb

    End_While

    Mov D$edi '.clo', B$edi+(4*ASCII) EOS ; append extension
ret
____________________________________________________________________________________________

; To allow to set the commandline inside the IDE I added an edit control in the output
; dialog and this code in section Format.
;
; These two are called at 'InitOutputDialog' and 'SaveOutputFormat':

Proc LoadCommandLine:

    Local @H.File

        Call GetCLOFilename ; Copy filename to buffer
        Call 'KERNEL32.CreateFileA' CommandLineFile, &GENERIC_READ, &FILE_SHARE_READ,
                        &NULL, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, &NULL
        Mov D@H.File eax
        .If eax = &INVALID_HANDLE_VALUE
            Call 'USER32.SetDlgItemTextA' D$H.Output, 214, 0 ; clear edit
        .Else
            Call 'KERNEL32.GetFileSize' D@H.File, 0
            On eax = 0_FFFF_FFFF ExitP

            Call VirtualAlloc CommandLinePtr,
                              eax

                Call 'KERNEL32.ReadFile' D@H.File, D$CommandLinePtr, eax, BytesTransfered, &NULL
                Mov eax D$CommandLinePtr | add eax D$BytesTransfered | Mov B$eax 0
                Call 'KERNEL32.CloseHandle' D@H.File
                Call 'USER32.SetDlgItemTextA' D$H.Output, 214, D$CommandLinePtr

                Call VirtualFree CommandLinePtr

        .End_If

EndP
____________________________________________________________________________________________

Proc SaveCommandLine:

    Local @H.File, @Size

        ; If string is empty, delete CLO file if it exists
        Call 'USER32.SendDlgItemMessageA' D$H.Output, 214, &WM_GETTEXTLENGTH, 0, 0
        Mov D@Size eax
        If eax = 0
            Call GetCLOFilename
            Call 'KERNEL32.DeleteFileA' CommandLineFile

EndP

        End_If

        Call VirtualAlloc CommandLinePtr,
                          D@Size

        Call GetCLOFilename ; Copy filename
        Call 'KERNEL32.CreateFileA' CommandLineFile, &GENERIC_WRITE, &FILE_SHARE_READ,
                        &NULL, &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, &NULL
        Mov D@H.File eax
        If eax <> &INVALID_HANDLE_VALUE
            ; Copy commandline from edit to buffer and write it into the file
            Mov eax D@Size | add eax (1*ASCII)
            Call 'USER32.GetDlgItemTextA' D$H.Output, 214, D$CommandLinePtr, eax
            Call 'KERNEL32.WriteFile' D@H.File, D$CommandLinePtr, eax, BytesTransfered, &NULL
            Call 'KERNEL32.CloseHandle' D@H.File
        End_If

        Call VirtualFree CommandLinePtr

EndP

____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
