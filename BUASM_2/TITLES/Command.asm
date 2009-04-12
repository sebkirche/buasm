TITLE Command

____________________________________________________________________________________________
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

[CommandLinePtr: ?]
[ScreensaverCommandLine: '/S' 0]
[BytesTransfered: ?]

Proc SetupCommandLine:
    Local @hFile, @Size
    Uses esi edi

        Call GetCLOFilename
        Call 'Kernel32.CreateFileA' CommandLineFile, &GENERIC_READ, &FILE_SHARE_READ,
                        &NULL, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, &NULL

        ; In general no clo file will be provided, if we deal with a screensaver put
        ; a /S command as a default command and exit proc
        .If eax = &INVALID_HANDLE_VALUE
            If D$SavingExtension = '.SCR'
                Mov eax ScreensaverCommandLine
            Else
                Mov eax 0
            EndIf
            ExitP
        .EndIf

        Mov D@hFile eax

        ; Allocate buffer for commandline (must be freed by caller)
        Call 'KERNEL32.GetFileSize' D@hFile, 0
        If eax = 0_FFFF_FFFF
            Call ReportWinError {'SetupCommandLine: GetFileSize' 0}
            Call 'Kernel32.CloseHandle' D@hFile
            Mov eax 0
            ExitP
        EndIf
        Mov D@Size eax

        VirtualAlloc CommandLinePtr D@Size
        Mov edi D$CommandLinePtr

        ; Read the command line parameters from the file
        Call 'Kernel32.ReadFile' D@hFile, edi, D@Size, BytesTransfered, &NULL
        add edi D$BytesTransfered
        Mov B$edi 0

        Call 'Kernel32.CloseHandle' D@hFile
        Mov eax D$CommandLinePtr
EndP
____________________________________________________________________________________________

[CommandLineFile: B$ ? #&MAXPATH]

GetCLOFilename:
    Mov esi MainName, edi CommandLineFile
    While B$esi <> 0
        movsb
    End_While
    Mov D$edi '.clo', B$edi+4 0 ; append extension
ret
____________________________________________________________________________________________

; To allow to set the commandline inside the IDE I added an edit control in the output
; dialog and this code in section Format.
;
; These two are called at 'InitOutputDialog' and 'SaveOutputFormat':

Proc LoadCommandLine:
    Local @hFile, @Size

        Call GetCLOFilename ; Copy filename to buffer
        Call 'Kernel32.CreateFileA' CommandLineFile, &GENERIC_READ, &FILE_SHARE_READ,
                        &NULL, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, &NULL
        Mov D@hFile eax
        .If eax = &INVALID_HANDLE_VALUE
            Call 'User32.SetDlgItemTextA' D$OutputHandle, 214, 0 ; clear edit
        .Else
            Call 'KERNEL32.GetFileSize' D@hFile, 0
            On eax = 0_FFFF_FFFF, ExitP
            Mov D@Size eax

            VirtualAlloc CommandLinePtr D@Size

                Call 'Kernel32.ReadFile' D@hFile, D$CommandLinePtr, D@Size, BytesTransfered, &NULL
                Mov eax D$CommandLinePtr | add eax D$BytesTransfered | Mov B$eax 0
                Call 'Kernel32.CloseHandle' D@hFile
                Call 'User32.SetDlgItemTextA' D$OutputHandle, 214, D$CommandLinePtr

            VirtualFree D$CommandLinePtr
        .End_If

EndP
____________________________________________________________________________________________

Proc SaveCommandLine:
    Local @hFile, @Size

        ; If string is empty, delete CLO file if it exists
        Call 'User32.SendDlgItemMessageA' D$OutputHandle, 214, &WM_GETTEXTLENGTH, 0, 0
        Mov D@Size eax
        If eax = 0
            Call GetCLOFilename
            Call 'Kernel32.DeleteFileA' CommandLineFile
            ExitP
        End_If

        VirtualAlloc CommandLinePtr D@Size

        Call GetCLOFilename ; Copy filename
        Call 'Kernel32.CreateFileA' CommandLineFile, &GENERIC_WRITE, &FILE_SHARE_READ,
                        &NULL, &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, &NULL
        Mov D@hFile eax
        If eax <> &INVALID_HANDLE_VALUE
            ; Copy commandline from edit to buffer and write it into the file
            Mov eax D@Size | inc eax
            Call 'User32.GetDlgItemTextA' D$OutputHandle, 214, D$CommandLinePtr, eax
            Call 'Kernel32.WriteFile' D@hFile, D$CommandLinePtr, eax, BytesTransfered, &NULL
            Call 'Kernel32.CloseHandle' D@hFile
        End_If

        VirtualFree D$CommandLinePtr
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________




