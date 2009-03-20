TITLE Exception       ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

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
    On D$FL.RealSourceRestored = &FALSE Call RestoreRealSource
    Mov B$WeAreSavingPart &FALSE | Call SaveSource

    Mov eax D@ExceptionInfo | Call GetExceptionInfo D$eax
    Mov eax D@ExceptionInfo | Call WriteCrashLog D$eax D$eax+4

    Call 'USER32.MessageBoxA' 0, ExceptionMessage,
        {B$ 'RosAsm crashed' EOS}, &MB_OK+&MB_ICONEXCLAMATION

    Call 'KERNEL32.SetErrorMode' &SEM_NOGPFAULTERRORBOX

    Mov eax &EXCEPTION_CONTINUE_SEARCH
EndP
____________________________________________________________________________________________


[ExceptionMessage:
"An exception occurred inside RosAsm. It must terminate now.

                    YOUR WORK IS NOT LOST!
                
Your source has been saved at the path of your application.
To continue working restart RosAsm, open your application
and replace the source.

Please post a bug report describing how to reproduce this
problem along with the crash.log (in the applications folder)
at RosAsm board.

Thank you and sorry for the inconvenience.

" ExceptionInfo: B$ "Exception occurred at address " ExceptionAddress: B$ "########.
" ExceptionDesc: B$ 0 # &MAX_PATH]

[Exception_AV: B$ 'Access Violation! Attempt to ' AV_ReadWrite: B$ '######### address ' AV_Address: B$ '########.' EOS]
[Exception_other: B$ 'Unknown exception. Code ' Exception_Code: B$ '########' EOS]

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
            Mov D$AV_ReadWrite 'writ', D$AV_ReadWrite+4 'e at', B$AV_ReadWrite+8 SPC
        End_If
        Mov edi AV_Address
        DwordToHex D$ebx+24 ; inaccessible address
        Mov esi Exception_AV
    .Else
        Mov edi ExceptionCode
        DwordToHex D$ebx ; exc. code
        Mov esi Exception_other
    .End_If

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

    Call 'KERNEL32.WriteFile' D@File, NewLineSeq, 2, BytesTransfered, 0
EndP

[RegContent: B$ 'Exx=12345678' 0D 0A] ; EOS

Proc WriteCrashLog:

    Arguments @ExceptionRecord @Context

    Local @H.File

    Call 'KERNEL32.CreateFileA' {B$ 'crash.log' EOS}, &GENERIC_WRITE, &FILE_SHARE_READ, 0,
        &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
    Mov D@H.File eax

    .If D@H.File <> &INVALID_HANDLE_VALUE

      ; Output RosAsm version
        Mov edi STR.A.ApplicationName | Call StrLen
        Mov edx STR.A.ApplicationName | add edx 2 | sub eax 2
        Call 'KERNEL32.WriteFile' D@H.File, edx, eax, BytesTransfered, 0
        Call EmitNewLine D@H.File

      ; Output Windows version
        Call GetWindowsVersionString
        Mov edi WindowsVersion | Call StrLen
        Call 'KERNEL32.WriteFile' D@H.File, WindowsVersion, eax, BytesTransfered, 0
        Call EmitNewLine D@H.File
        Call EmitNewLine D@H.File

      ; Output exception info
        Mov edi ExceptionInfo | Call StrLen
        Call 'KERNEL32.WriteFile' D@H.File, ExceptionInfo, eax, BytesTransfered, 0
        Call EmitNewLine D@H.File
        Call EmitNewLine D@H.File

      ; Output reg contents
        Mov ebx D@Context

        Mov edi RegContent | Mov W$edi+1 'AX' | add edi 4
        DwordToHex D$ebx+0B0
        Call 'KERNEL32.WriteFile' D@H.File, RegContent, 14, BytesTransfered, 0

        Mov edi RegContent | Mov W$edi+1 'BX' | add edi 4
        DwordToHex D$ebx+0A4
        Call 'KERNEL32.WriteFile' D@H.File, RegContent, 14, BytesTransfered, 0

        Mov edi RegContent | Mov W$edi+1 'CX' | add edi 4
        DwordToHex D$ebx+0AC
        Call 'KERNEL32.WriteFile' D@H.File, RegContent, 14, BytesTransfered, 0

        Mov edi RegContent | Mov W$edi+1 'DX' | add edi 4
        DwordToHex D$ebx+0A8
        Call 'KERNEL32.WriteFile' D@H.File, RegContent, 14, BytesTransfered, 0

        Mov edi RegContent | Mov W$edi+1 'SI' | add edi 4
        DwordToHex D$ebx+0A0
        Call 'KERNEL32.WriteFile' D@H.File, RegContent, 14, BytesTransfered, 0

        Mov edi RegContent | Mov W$edi+1 'DI' | add edi 4
        DwordToHex D$ebx+09C
        Call 'KERNEL32.WriteFile' D@H.File, RegContent, 14, BytesTransfered, 0

        Mov edi RegContent | Mov W$edi+1 'BP' | add edi 4
        DwordToHex D$ebx+0B4
        Call 'KERNEL32.WriteFile' D@H.File, RegContent, 14, BytesTransfered, 0

        Mov edi RegContent | Mov W$edi+1 'SP' | add edi 4
        DwordToHex D$ebx+0C4
        Call 'KERNEL32.WriteFile' D@H.File, RegContent, 14, BytesTransfered, 0

        Call 'KERNEL32.CloseHandle' D@H.File
    .End_If

EndP
____________________________________________________________________________________________

; Get windows version information. Original C code from MSDN converted to RosAsm.

[WindowsVersion: B$ ? # 256]

[OSVersionInfo:
 OSVersionInfo.Size: D$ ?
 OSVersionInfo.MajorVersion: D$ ?
 OSVersionInfo.MinorVersion: D$ ?
 OSVersionInfo.BuildNumber: D$ ?
 OSVersionInfo.PlatformId: D$ ?
 OSVersionInfo.CSDVersion: B$ ? # 128
 OSVersionInfo.ServicePackMajor: W$ ?
 OSVersionInfo.ServicePackMinor: W$ ?
 OSVersionInfo.SuiteMask: W$ ?
 OSVersionInfo.ProductType: B$ ?
 OSVersionInfo.Reserved: B$ ?]

[OSVI_SIZE 148 OSVI_EX_SIZE 156]

Win2003ServerProductType:
    Mov ax W$OSVersionInfo.SuiteMask
    test ax &VER_SUITE_DATACENTER ZERO L0>
        Mov esi {B$ 'Datacenter Edition' EOS} | ret
L0: test ax &VER_SUITE_ENTERPRISE ZERO L0>
        Mov esi {B$ 'Enterprise Edition' EOS} | ret
L0: test ax 0400 ZERO L0> ;&VER_SUITE_BLADE | jz L0>
        Mov esi {B$ 'Web Edition' EOS} | ret
L0: Mov esi {B$ 'Standard Edition' EOS}
ret

Win2000ServerProductType:
    test ax &VER_SUITE_DATACENTER ZERO L0>
        Mov esi {B$ 'Datacenter Server' EOS} | ret
L0: test ax &VER_SUITE_ENTERPRISE ZERO L0>
        Mov esi {B$ 'Advanced Server' EOS} | ret
L0: Mov esi {B$ 'Server' EOS}
ret

Proc TestWinNTSP6a:
    Local @Key

    .If D$OSVersionInfo.MajorVersion = 4
        lea eax D@Key
        Call 'ADVAPI32.RegOpenKeyExA' &HKEY_LOCAL_MACHINE,
            {B$ 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Hotfix\Q246009' EOS},
            0, &KEY_QUERY_VALUE, eax

        If eax = &ERROR_SUCCESS
            Mov al 'a' | stosb
        End_If

        Call 'ADVAPI32.RegCloseKey' D@Key
    .End_If
EndP

[WinNTProductType: B$ ? # 80]
[WinNTPTLen: D$ ?]

Proc GetWindowsProductInfo:
    Local @Key

    Mov esi 0

    Mov D$OSVersionInfo.Size OSVI_EX_SIZE
    Call 'KERNEL32.GetVersionExA' OSVersionInfo
    ...If eax = 1

      ; workstation
        ..If W$OsVersionInfo.ProductType = &VER_NT_WORKSTATION
            .If D$OSVersionInfo.MajorVersion = 4
                Mov esi {B$ 'Workstation 4.0' EOS}
            .Else
                Mov ax W$OSVersionInfo.SuiteMask
                and ax 0200 ;&VER_SUITE_PERSONAL
                If ax <> 0
                    Mov esi {B$ 'Home Edition' EOS}
                Else
                    Mov esi {B$ 'Professional' EOS}
                End_If
            .End_If

       ; server
         ..Else
            .If D$OSVersionInfo.MajorVersion = 5
                If D$OSVersionInfo.MinorVersion = 2
                    Call Win2003ServerProductType
                Else_If D$OSVersionInfo.MinorVersion = 0
                    Call Win2000ServerProductType
                End_If
            .Else
                Mov ax W$OSVersionInfo.SuiteMask
                and ax &VER_SUITE_ENTERPRISE
                If ax <> 0
                    Mov esi {B$ 'Server 4.0 Enterprise' EOS}
                Else
                    Mov esi {B$ 'Server 4.0' EOS}
                End_If
            .End_If
         ..End_If

    ...Else

        lea eax D@Key
        Call 'ADVAPI32.RegOpenKeyExA' &HKEY_LOCAL_MACHINE,
            {B$ 'SYSTEM\CurrentControlSet\Control\ProductOptions' EOS},
            0, &KEY_QUERY_VALUE, eax

        On eax <> &ERROR_SUCCESS, ExitP

        Mov D$WinNTPTLen 80
        Call 'ADVAPI32.RegQueryValueExA' D@Key, {B$ 'ProductType' EOS},
            0, 0, WinNTProductType, WinNTPTLen

        On eax <> &ERROR_SUCCESS, ExitP
        On D$WinNTPTLen > 80, ExitP

        Call 'ADVAPI32.RegCloseKey' D@Key

        If D$WinNTProductType = 'WINN'
            Mov esi {B$ 'Workstation' EOS}
        Else_If D$WinNTProductType = 'LANM'
            Mov esi {B$ 'Server' EOS}
        Else_If D$WinNTProductType = 'SERV'
            Mov esi {B$ 'Advanced Server' EOS}
        End_If

    ...End_If
EndP

IntToStr:
    Mov dl 0FF | Push edx                       ; Push stack end mark
    Mov ecx 10
L0: Mov edx 0
    div ecx | Push edx | cmp eax 0 | ja L0<     ; Push remainders
L2: Pop eax                                     ; Retrieve Backward
    cmp al 0FF | je L9>                         ; Over?
    add al '0' | stosb | jmp L2<             ; Write
L9: ret

Proc GetWindowsVersionString:

    Mov D$OSVersionInfo.Size OSVI_SIZE
    Call 'KERNEL32.GetVersionExA' OSVersionInfo

    Mov edi WindowsVersion

    ..If D$OSVersionInfo.PlatformId = &VER_PLATFORM_WIN32_NT

      ; Major versions
        .If D$OSVersionInfo.MajorVersion = 5
            If D$OSVersionInfo.MinorVersion = 2
                Mov esi {B$ 'MS Windows Server 2003' EOS}
            Else_If D$OSVersionInfo.MinorVersion = 1
                Mov esi {B$ 'MS Windows XP' EOS}
            Else_If D$OSVersionInfo.MinorVersion = 0
                Mov esi {B$ 'MS Windows 2000' EOS}
            End_If
        .Else_If D$OSVersionInfo.MajorVersion <= 4
            Mov esi {B$ 'MS Windows NT' EOS}
        .End_If

        While B$esi <> 0 | movsb | EndWhile
        Mov al SPC | stosb

      ; Service pack number
        Mov esi OSVersionInfo.CSDVersion
        While B$esi <> 0 | movsb | EndWhile
        Call TestWinNTSP6a

      ; Build number
        Mov esi {B$ ' Build ' EOS}
        While B$esi <> 0 | movsb | EndWhile
        movzx eax W$OSVersionInfo.BuildNumber
        Call IntToStr
        Mov al SPC | stosb

      ; Home / Professional / ...
        Call GetWindowsProductInfo
        If esi <> 0
            While B$esi <> 0 | movsb | EndWhile
            Mov al SPC | stosb
        End_If

    ..Else_If D$OSVersionInfo.PlatformId = &VER_PLATFORM_WIN32_WINDOWS

      ; Major versions
        .If D$OSVersionInfo.MajorVersion = 4
            Mov eax 0
            If D$OSVersionInfo.MinorVersion = 90
                Mov esi {B$ 'MS Windows ME' EOS}
            Else_If D$OSVersionInfo.MinorVersion = 10
                Mov esi {B$ 'MS Windows 98' EOS}
                On B$OSVersionInfo.CSDVersion+1 = 'A', Mov eax ' SE'
            Else_If D$OSVersionInfo.MinorVersion = 0
                Mov esi {B$ 'MS Windows 95 ' EOS}
                On B$OSVersionInfo.CSDVersion+1 = 'B', Mov eax 'OSR2'
                On B$OSVersionInfo.CSDVersion+1 = 'C', Mov eax 'OSR2'
            End_If
            While B$esi <> 0 | movsb | EndWhile
            stosd
        .End_If

    ..Else_If D$OSVersionInfo.PlatformId = &VER_PLATFORM_WIN32S

        Mov esi {B$ 'MS Win32s' EOS}
        While B$esi <> 0 | movsb | EndWhile

    ..End_If

    Mov B$edi 0
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
