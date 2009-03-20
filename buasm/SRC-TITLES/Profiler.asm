TITLE Profiler        ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

ProfileComments:
;;

____________________________________________________________________________________________

  ________
  Routines:
  
  From the Menu 'M00_Profile', Call to the Main Routine: 'Profiler'.
  
  From 'AsmMain':
  
  * 'InjectedCopyToCodeSourceA' injects additional Coding in the Timed App,
    instead of the normal 'NewCopyToCodeSourceA'. (Materials at 'InjectedTIME_COUNT')
    
  * In 'HotParsers', after the Call to 'StripUnderscore', turn the '.' of
    'InjectedTIME_COUNT' into '_', to build unique Symbols, by calling to
    'InjectDashLines'.
  
  * After execution of 'EncodeLines', Call to 'CreateProfilerTables'
  
  * 'FillCodeSymbols' takes in charge the filling of the Relays Table and
    does the substitutions. >>> 'TimingCalls'
  
  ______
  Plan_2:
  
  All calls are turned "Call Time_Count" So, we need a Table for storing the
  real calls Addresses. Same size as the CodeList Buffer. In 'FillCodeSymbols',
  "If B$ProfilerFlag = &TRUE", we do the substitution and save the Original Call
  Address in the Parallel Table.
  
  Inserted Routins, in the compiled Applicatinon, at 'InjectedTIME_COUNT'.
  There, the real Call will be in the form of "Call D$Address_Relay", which
  Variable will hold the Table Assress.
  
  This Table is created On RosAsm Side: Call for 'CreateMemoryMapFile'. This
  sould be done after the Call to EncodeLines, in 'AsmMain', by 'CreateProfilerTables'
  
  The 'Time_Count' inserted Routine must write the Timing in a second Table
  dedicated to these recordings.
  
  How to record the Timings?
  
  We need a qWords Table where what will be stored will be the difference
  between rdtsc, before and after the real call.
  
  Note: I do not see any Function associted to 'CreateFileMapping' that could
  tell the size of the File (?...). For now, simpler is to store this size in
  a Variable of 'InjectedTIME_COUNT'.
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[TimingMapName: B$ 'TestMem.tst' EOS]

[H.TimingMapFile: D$ ?
 OrigineOfTimingMap: D$ ?]

Proc CreateMemoryMapFile:
    Argument @Size
        Call 'KERNEL32.CreateFileMappingA' 0-1, &NULL, &PAGE_READWRITE,
                                        0, D@Size, TimingMapName
        Mov D$H.TimingMapFile eax

        Call 'KERNEL32.MapViewOfFile' D$H.TimingMapFile, &FILE_MAP_ALL_ACCESS,
                                      0, 0, D@Size
        Mov D$OrigineOfTimingMap eax
ret


ReadMemoryMapFile:
    Mov esi D$OrigineOfTimingMap, eax D$esi
    ;hexprint eax
ret


DeleteMemoryMapFile:
    Call 'KERNEL32.UnmapViewOfFile' D$OrigineOfTimingMap
    Call 'KERNEL32.CloseHandle' D$H.TimingMapFile
ret


OpenMemoryMapFile:
    Call 'KERNEL32.CreateFileMappingA' 0-1, &NULL, &PAGE_READWRITE,
                                       0, 100, TimingMapName
    Mov D$H.TimingMapFile eax

    Call 'KERNEL32.MapViewOfFile' D$H.TimingMapFile, &FILE_MAP_ALL_ACCESS,
                                  0, 0, 100
    Mov D$OrigineOfTimingMap eax
ret


WriteMemoryMapFile:
    Mov esi D$OrigineOfTimingMap

    Mov D$esi 012345678
ret


CloseMemoryMapFile:
    Call 'KERNEL32.UnmapViewOfFile' D$OrigineOfTimingMap
    Call 'KERNEL32.CloseHandle' D$H.TimingMapFile
ret

____________________________________________________________________________________________


[ProfileTable: D$ ?
 ProfilerFlag: D$ ?]

; ProfileComments

Profiler:
    Mov B$ProfilerFlag &TRUE
    Mov D$AddressOfTimeCount 0

    Mov D$FL.ShowStats &FALSE | Call AsmMain | Mov D$FL.ShowStats &TRUE
    Mov D$OldStackPointer 0
  ; Main Call >>> 'CreateProfilerTables'

  ret


    On B$CompileErrorHappend = &TRUE, ret

  ; Similar to 'ScanShortenJmpsTable'
    Mov ebx D$CodeRef | add ebx 5

  ; Case of Api Calls: ...|0FF dWordCodeAddress|:
  ;                   ....|.....|
    While B$ebx = 0FF | add ebx 6 | End_While

    Mov D$CodeRefScan ebx

    Mov B$ProfilerFlag &FALSE
ret


[ProfilerOriginalCalls: D$ ?
 ProfilerNumberOfCalls: D$ ?]

CreateProfilerTables:
;;
 What Table do we need in the MapFile?
 
 - Storing the Real Calls Addresses. Same size as the real Code.
 - Storing the Number of calls. Same size as the real Code.
 - Storing the Timings on the calls Rooms. One qWord each. Double Size.
 
;;
  ; (Size of Code) * 4:
    Mov ecx D$CodeListPtr | sub ecx D$CodeOrigine | shl ecx 2

    Call CreateMemoryMapFile ecx

  ; The Timed App needs to know of the Displacement...
  ; .. and 'FillCodeSymbols' must fill the proper Displacements...
ret


ReleaseProfilerTables:

    Call VirtualFree ProfilerOriginalCalls

    Call VirtualFree ProfilerNumberOfCalls

ret

____________________________________________________________________________________________

[AddressOfTimeCount: D$ ?]
[ModelOfTimeCount: B$ 'TIME_COUNT' EOS]

SetAddressOfTimeCount:
    pushad
      ; 'FillCodeSymbols'
        Call GetFromQwordCheckSum ModelOfTimeCount, D$LabelList, D$LabelListLimit
        While B$eax > LowSigns | inc eax | inc esi | End_While | inc eax | inc esi
        Mov edi eax, ecx 0
    popad
ret

[InjectedTIME_COUNT: B$ "
Time.Count:
  ; The 'Call' Return Address is at D$esp
    Pop D$ReturnAddressOfTimeCount

        Push eax, ebx, ecx, edx
            cpuid | rdtsc
            sub D$Duration eax | sbb D$Duration+4 edx
        Pop edx, ecx, ebx, eax

        Call D$CallAddress

        Push eax, ebx, ecx, edx
            cpuid | rdtsc
            add D$Duration eax | adc D$Duration+4 edx
        Pop edx, ecx, ebx, eax

        hexprint D$Duration+4, D$Duration

    Push D$ReturnAddressOfTimeCount
ret

; To be paste here: 'OpenMemoryMapFile', 'WriteMemoryMapFile', 'CloseMemoryMapFile'.

" InjectedTIME_COUNT_Len: D$ Len]


; Called from 'HotParsers': ; CoolParsers

InjectDashLines:
    Mov esi D$CodeSourceA, ecx 1
  ; ecx = Hard code number of '.' in 'InjectedTIME_COUNT'.
L0: If B$esi = '.'
        Mov B$esi '_' | loop L0<
    Else
        jmp L0<
    End_If
ret


[CallByte: 0E8]

Profile:
    Call 'KERNEL32.GetStartupInfoA' STARTUPINFO
    Call 'KERNEL32.CreateProcessA' DestinationFile, &NULL, &NULL, &NULL, &FALSE,
                                   &DEBUG_PROCESS__&DEBUG_ONLY_THIS_PROCESS,
                                   &NULL, &NULL, STARTUPINFO, PROCESS_INFORMATION

    .While eax = &TRUE
L0:     Call 'KERNEL32.WaitForDebugEvent' DEBUG_EVENT, &INFINITE

        .If D$DE.dwDebugEventCode = &EXIT_PROCESS_DEBUG_EVENT
            jmp L9>>

        .Else_If D$DE.dwDebugEventCode = &EXCEPTION_DEBUG_EVENT
            If D$E.ExceptionCode = &EXCEPTION_BREAKPOINT

                ; Debugger_OnException
                Mov D$C.ContextFlags &CONTEXT_FULL
                Call 'KERNEL32.GetThreadContext' D$PI.hThread, CONTEXT
                ;Mov ebx D$C.regEip

           ;     Mov D$C.ContextFlags &CONTEXT_CONTROL
           ;     Call 'KERNEL32.GetThreadContext' D$PI.hThread, CONTEXT
           ;     or D$C.regFlag 0100
           ;     Call 'KERNEL32.SetThreadContext' D$PI.hThread, CONTEXT
;Call 'KERNEL32.ReadProcessMemory' D$PI.hProcess, D$E.ExceptionAddress, Trash, 1, &NULL
;hexprint D$Trash
                ;Call 'KERNEL32.WriteProcessMemory' D$PI.hProcess, D$E.ExceptionAddress,
                ;                                   CallByte, 1, &NULL

                Call 'KERNEL32.ReadProcessMemory' D$PI.hProcess, D$C.regEip, LP.MemTrash, 5 ; !!! TODO !!!! Il manque le dernier paramètre !!!

                Mov eax LP.MemTrash | ;int3
;hexprint D$E.ExceptionAddress
                Call 'KERNEL32.ContinueDebugEvent' D$DE.dwProcessId, D$DE.dwThreadId,
                                                   &DBG_CONTINUE
              jmp L0<<

           ; Else_If D$E.ExceptionCode = &EXCEPTION_SINGLE_STEP
           ;     Call 'KERNEL32.GetThreadContext' D$PI.hThread, CONTEXT
           ;     or D$C.regFlag 0100
           ;     Call 'KERNEL32.SetThreadContext' D$PI.hThread, CONTEXT
           ;     Call 'KERNEL32.ContinueDebugEvent' D$DE.dwProcessId, D$DE.dwThreadId,
           ;                                        &DBG_CONTINUE
           ;   jmp L0<<

            End_If
        .End_If

        Call 'KERNEL32.ContinueDebugEvent' D$DE.dwProcessId, D$DE.dwThreadId,
                                           &DBG_EXCEPTION_NOT_HANDLED
    .End_While

L9: Call 'KERNEL32.CloseHandle' D$PI.hProcess
    Call 'KERNEL32.CloseHandle' D$PI.hThread
ret


ShowProfilerStats:
   ; Call 'USER32.MessageBoxA' 0, {B$ 'Hi' EOS}, {B$ 'Hi' EOS}, 0
ret


; Called from 'FillCodeSymbols':

TimingCalls:
  ; eax+4 = Original Displacement.

    On B$edi+1 <> 0E8, ret

    On D$AddressOfTimeCount = 0, Call SetAddressOfTimeCount
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
