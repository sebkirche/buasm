TITLE Exception

;;
    General exception handler.
    
    October 2004 - Ludwig Haehne - wkx@gmx.li
    
    Reports a description of the exception that occurred, writes a crash.log file
    and tries to save the source before RosAsm is terminated by the system. 
    
    TODO Create a crash dump (data, stack, context) that can be loaded on a different 
    machine to find the problem.
;;
____________________________________________________________________________________________

Proc FinalExceptionHandler:
    Arguments @ExceptionInfo
    Uses ebx esi edi

  ; Save whole source
    On B$RealSourceRestored = &FALSE, Call RestoreRealSource
    Mov B$WeAreSavingPart &FALSE | Call SaveSource

    Mov eax D@ExceptionInfo | Call GetExceptionInfo D$eax
    Mov eax D@ExceptionInfo | Call WriteCrashLog D$eax D$eax+4

    Call MessageBox {B$ "BUAsm CRASHED:" EOS},
                    ExceptionMessage,
                    &MB_SYSTEMMODAL+&MB_ICONEXCLAMATION

    Call 'KERNEL32.SetErrorMode' &SEM_NOGPFAULTERRORBOX

    Mov eax &EXCEPTION_CONTINUE_SEARCH
EndP
____________________________________________________________________________________________


[ExceptionMessage: B$
"An exception occurred inside BUsm. It must terminate now.

                    YOUR WORK IS NOT LOST!
                
Your source has been saved at the path of your application.
To continue working restart BUAsm, open your application
and replace the source.

Please post a bug report describing how to reproduce this
problem along with the crash.log (in the applications folder)
at BUAsm board.

Thank you and sorry for the inconvenience.

" ExceptionInfo: B$ "Exception occurred at address " ExceptionAddress: B$ "########.
" ExceptionDesc: B$ 0 # &MAX_PATH]

[Exception_AV: B$ "Access Violation ! Attempt to " AV_ReadWrite: B$ "######### address " AV_Address: B$ "########." EOS]

[Exception_other: B$ "Unknown exception. Code " Exception_Code: B$ "########" EOS]

Proc GetExceptionInfo:
    Arguments @ExceptionRecord

    Mov ebx D@ExceptionRecord

    Mov edi ExceptionAddress
    DwordToHex D$ebx+12 ; Address

    Mov eax D$ebx ; ExceptionCode

    .If eax = &EXCEPTION_ACCESS_VIOLATION
        Mov eax D$ebx+20 ; read/write
        If eax = 0
            Mov D$AV_ReadWrite 'read', D$AV_ReadWrite+4 ' fro', B$AV_ReadWrite+8 'm'
        Else
            Mov D$AV_ReadWrite 'writ', D$AV_ReadWrite+4 'e at', B$AV_ReadWrite+8 ' '
        EndIf
        Mov edi AV_Address
        DwordToHex D$ebx+24 ; inaccessible address
        Mov esi Exception_AV
    .Else
        Mov edi ExceptionCode
        DwordToHex D$ebx ; exc. code
        Mov esi Exception_other
    .EndIf

    Mov edi ExceptionDesc
    Do
        movsb
    Loop_until B$esi-1 = 0
EndP
____________________________________________________________________________________________

; Write a log-file which is really helpful.
;   * dump exception information (exc code, inaccessible address, access type)
;   * rosasm version
;   * OS information (NT/9x)
;   * register contents

[NewLineSeq: W$ 0A0D]

Proc EmitNewLine:
    Arguments @File

    Call 'Kernel32.WriteFile' D@File, NewLineSeq, 2, BytesTransfered, 0
EndP

[RegContent: B$ 'Exx=12345678' 0D 0A]

Proc WriteCrashLog:
    Arguments @ExceptionRecord @Context
    Local @File

    Call 'Kernel32.CreateFileA' {'crash.log' 0}, &GENERIC_WRITE, &FILE_SHARE_READ, 0,
        &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
    Mov D@File eax

    .If D@File <> &INVALID_HANDLE_VALUE

      ; Output RosAsm version
        Mov edi STR.A.AppName | Call StrLen
        Mov edx STR.A.AppName | add edx 2 | sub eax 2
        Call 'Kernel32.WriteFile' D@File, edx, eax, BytesTransfered, 0
        Call EmitNewLine D@File

      ; Output Windows version
        Call GetWindowsVersionString
        Mov edi WindowsVersion | Call StrLen
        Call 'Kernel32.WriteFile' D@File, WindowsVersion, eax, BytesTransfered, 0
        Call EmitNewLine D@File
        Call EmitNewLine D@File

      ; Output exception info
        Mov edi ExceptionInfo | Call StrLen
        Call 'Kernel32.WriteFile' D@File, ExceptionInfo, eax, BytesTransfered, 0
        Call EmitNewLine D@File
        Call EmitNewLine D@File

      ; Output reg contents
        Mov ebx D@Context

        Mov edi RegContent | Mov W$edi+1 'AX' | add edi 4
        DwordToHex D$ebx+0B0
        Call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        Mov edi RegContent | Mov W$edi+1 'BX' | add edi 4
        DwordToHex D$ebx+0A4
        Call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        Mov edi RegContent | Mov W$edi+1 'CX' | add edi 4
        DwordToHex D$ebx+0AC
        Call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        Mov edi RegContent | Mov W$edi+1 'DX' | add edi 4
        DwordToHex D$ebx+0A8
        Call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        Mov edi RegContent | Mov W$edi+1 'SI' | add edi 4
        DwordToHex D$ebx+0A0
        Call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        Mov edi RegContent | Mov W$edi+1 'DI' | add edi 4
        DwordToHex D$ebx+09C
        Call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        Mov edi RegContent | Mov W$edi+1 'BP' | add edi 4
        DwordToHex D$ebx+0B4
        Call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        Mov edi RegContent | Mov W$edi+1 'SP' | add edi 4
        DwordToHex D$ebx+0C4
        Call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        Call 'Kernel32.CloseHandle' D@File
    .EndIf

EndP
____________________________________________________________________________________________

; Get windows version information. Original C code from MSDN converted to RosAsm.

[WindowsVersion: B$ ? #256]

[OSVersionInfo:
 OSVersionInfo.Size: D$ ?
 OSVersionInfo.MajorVersion: D$ ?
 OSVersionInfo.MinorVersion: D$ ?
 OSVersionInfo.BuildNumber: D$ ?
 OSVersionInfo.PlatformId: D$ ?
 OSVersionInfo.CSDVersion: B$ ? #128
 OSVersionInfo.ServicePackMajor: W$ ?
 OSVersionInfo.ServicePackMinor: W$ ?
 OSVersionInfo.SuiteMask: W$ ?
 OSVersionInfo.ProductType: B$ ?
 OSVersionInfo.Reserved: B$ ?]

[OSVI_SIZE 148 OSVI_EX_SIZE 156]

Win2003ServerProductType:
    Mov ax W$OSVersionInfo.SuiteMask
    test ax &VER_SUITE_DATACENTER | jz L0>
        Mov esi {'Datacenter Edition' 0} | ret
L0: test ax &VER_SUITE_ENTERPRISE | jz L0>
        Mov esi {'Enterprise Edition' 0} | ret
L0: test ax 0400 | jz L0> ;&VER_SUITE_BLADE | jz L0>
        Mov esi {'Web Edition' 0} | ret
L0: Mov esi {'Standard Edition' 0}
ret

Win2000ServerProductType:
    test ax &VER_SUITE_DATACENTER | jz L0>
        Mov esi {'Datacenter Server' 0} | ret
L0: test ax &VER_SUITE_ENTERPRISE | jz L0>
        Mov esi {'Advanced Server' 0} | ret
L0: Mov esi {'Server' 0}
ret

Proc TestWinNTSP6a:
    Local @Key

    .If D$OSVersionInfo.MajorVersion = 4
        lea eax D@Key
        Call 'ADVAPI32.RegOpenKeyExA' &HKEY_LOCAL_MACHINE,
            {'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Hotfix\Q246009' 0},
            0, &KEY_QUERY_VALUE, eax

        If eax = &ERROR_SUCCESS
            Mov al 'a' | stosb
        EndIf

        Call 'ADVAPI32.RegCloseKey' D@Key
    .EndIf
EndP

[WinNTProductType: B$ ? #80 WinNTPTLen: D$ ?]

Proc GetWindowsProductInfo:
    Local @Key

    Mov esi 0

    Mov D$OSVersionInfo.Size OSVI_EX_SIZE
    Call 'Kernel32.GetVersionExA' OSVersionInfo
    ...If eax = 1

      ; workstation
        ..If W$OsVersionInfo.ProductType = &VER_NT_WORKSTATION
            .If D$OSVersionInfo.MajorVersion = 4
                Mov esi {'Workstation 4.0' 0}
            .Else
                Mov ax W$OSVersionInfo.SuiteMask
                and ax 0200 ;&VER_SUITE_PERSONAL
                If ax <> 0
                    Mov esi {'Home Edition' 0}
                Else
                    Mov esi {'Professional' 0}
                EndIf
            .EndIf

       ; server
         ..Else
            .If D$OSVersionInfo.MajorVersion = 5
                If D$OSVersionInfo.MinorVersion = 2
                    Call Win2003ServerProductType
                ElseIf D$OSVersionInfo.MinorVersion = 0
                    Call Win2000ServerProductType
                EndIf
            .Else
                Mov ax W$OSVersionInfo.SuiteMask
                and ax &VER_SUITE_ENTERPRISE
                If ax <> 0
                    Mov esi {'Server 4.0 Enterprise' 0}
                Else
                    Mov esi {'Server 4.0' 0}
                EndIf
            .EndIf
         ..EndIf

    ...Else

        lea eax D@Key
        Call 'ADVAPI32.RegOpenKeyExA' &HKEY_LOCAL_MACHINE,
            {'SYSTEM\CurrentControlSet\Control\ProductOptions' 0},
            0, &KEY_QUERY_VALUE, eax

        On eax <> &ERROR_SUCCESS, ExitP

        Mov D$WinNTPTLen 80
        Call 'ADVAPI32.RegQueryValueExA' D@Key, {'ProductType' 0},
            0, 0, WinNTProductType, WinNTPTLen

        On eax <> &ERROR_SUCCESS, ExitP
        On D$WinNTPTLen > 80, ExitP

        Call 'ADVAPI32.RegCloseKey' D@Key

        If D$WinNTProductType = 'WINN'
            Mov esi {'Workstation' 0}
        ElseIf D$WinNTProductType = 'LANM'
            Mov esi {'Server' 0}
        ElseIf D$WinNTProductType = 'SERV'
            Mov esi {'Advanced Server' 0}
        EndIf

    ...EndIf
EndP

IntToStr:
    Mov dl 0FF | push edx                       ; Push stack end mark
    Mov ecx 10
L0: Mov edx 0
    div ecx | push edx | cmp eax 0 | ja L0<     ; Push remainders
L2: pop eax                                     ; Retrieve Backward
    cmp al 0FF | je L9>                         ; Over?
    add al '0' | stosb | jmp L2<             ; Write
L9: ret

Proc GetWindowsVersionString:

    Mov D$OSVersionInfo.Size OSVI_SIZE
    Call 'Kernel32.GetVersionExA' OSVersionInfo

    Mov edi WindowsVersion

    ..If D$OSVersionInfo.PlatformId = &VER_PLATFORM_WIN32_NT

      ; Major versions
        .If D$OSVersionInfo.MajorVersion = 5
            If D$OSVersionInfo.MinorVersion = 2
                Mov esi {'MS Windows Server 2003' 0}
            ElseIf D$OSVersionInfo.MinorVersion = 1
                Mov esi {'MS Windows XP' 0}
            ElseIf D$OSVersionInfo.MinorVersion = 0
                Mov esi {'MS Windows 2000' 0}
            EndIf
        .ElseIf D$OSVersionInfo.MajorVersion <= 4
            Mov esi {'MS Windows NT' 0}
        .EndIf

        While B$esi <> 0 | movsb | EndWhile
        Mov al ' ' | stosb

      ; Service pack number
        Mov esi OSVersionInfo.CSDVersion
        While B$esi <> 0 | movsb | EndWhile
        Call TestWinNTSP6a

      ; Build number
        Mov esi {' Build ' 0}
        While B$esi <> 0 | movsb | EndWhile
        movzx eax W$OSVersionInfo.BuildNumber
        Call IntToStr
        Mov al ' ' | stosb

      ; Home / Professional / ...
        Call GetWindowsProductInfo
        If esi <> 0
            While B$esi <> 0 | movsb | EndWhile
            Mov al ' ' | stosb
        EndIf

    ..ElseIf D$OSVersionInfo.PlatformId = &VER_PLATFORM_WIN32_WINDOWS

      ; Major versions
        .If D$OSVersionInfo.MajorVersion = 4
            Mov eax 0
            If D$OSVersionInfo.MinorVersion = 90
                Mov esi {'MS Windows ME' 0}
            ElseIf D$OSVersionInfo.MinorVersion = 10
                Mov esi {'MS Windows 98' 0}
                On B$OSVersionInfo.CSDVersion+1 = 'A', Mov eax ' SE'
            ElseIf D$OSVersionInfo.MinorVersion = 0
                Mov esi {'MS Windows 95 ' 0}
                On B$OSVersionInfo.CSDVersion+1 = 'B', Mov eax 'OSR2'
                On B$OSVersionInfo.CSDVersion+1 = 'C', Mov eax 'OSR2'
            EndIf
            While B$esi <> 0 | movsb | EndWhile
            stosd
        .EndIf

    ..ElseIf D$OSVersionInfo.PlatformId = &VER_PLATFORM_WIN32S

        Mov esi {'MS Win32s' 0}
        While B$esi <> 0 | movsb | EndWhile

    ..EndIf

    Mov B$edi 0
EndP

























