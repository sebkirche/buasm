TITLE Local


;;
  Small Memory Chunks Management. Do not use: Under developement.
  
  
  The 'LocalChunksTable' is a Table of dWords. Each dWord is a Pointer to a
  Table of "Follow-Up Tables", one for each Size of Local Chunk:
  
  32, 64, 128, 256, 512
  020, 040, 080, 0100, 0200
  
  The Memories pointed by the 'LocalChunksTable' dWOrds are extended when
  required and never released. They are extended by 01000 steps (01000, 02000,
  03000).
  
  A Table of Follow-Up Tables is a flow of "Follow-Up Table" of one given Size.
  
  On single "Follow-Up Table" looks like this:
  
  [030510000, xxxxxxxx, xxxxxxxx, xxxxxxxx, xxxxxxxx]
  
  ... where 030510000 is the "Mother Chunk" of Memory, given by VirtualAlloc,
  and where the "xxxxxxxx"s are dWords, where each Bit represents one small
  Chunk inside the Mother Chunk.
  
  All of the "Follow-Up Tables" have 4 Follow-Up Bits Masks dWords, and the Size
  of the Mother Chunk is proportional to the size of the small Chunks. This is
  to say:
   __________            _______________
   Chunk Size            Mother Mem Size
  
   32 Bytes * 128 >>> 4096 Bytes (01000)
   64       * 128 >>>             02000
   128      * 128 >>>             04000
   256      * 128 >>>             08000
   512      * 128 >>>            010000
  1024      * 128 >>>            020000
  2048      * 128 >>>            040000
  
  So, all Mother-Memories hold 128 Local Chunks.
  
  The Mother Chunks (like the 'LocalChunksTable' Memories) are never released
  (no Call to VirtualFree). In cases of Call to 'LocalFree', the concerned Bit
  is set to zero.
  
  To fasten the LocalFree operation, each Local Chunk begins with two dWOrds:
  The Address, inside one of the "Follow-Up Tables", the bit position of the
  Local Chunk and the indice of chunk size(this last two in the last dword)
  
  So, when calling, for example, a Call for a 100 Bytes Local Chunk, the real
  size will be 108 Bytes. Then, the 'LocalAlloc' Routine will align that Value
  on the closer possible Size, that is, 128 Bytes, and return a to the caller
  a Pointer to the third dWord on that boundary.
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[LocalChunksTable:      ? ? ? ? ? ? ?]
[LocalChunksTablePages: ? ? ? ? ? ? ?]
[FreeLocalChunkPointer: ? ? ? ? ? ? ?]
; First Allocation for a Table of 'Follow-Up Tables':

Proc CreateLocalChunksTable:
    Argument @Indice

        VirtualAlloc NewMem, PAGESIZE
        Mov eax D@Indice, ebx D$NewMem
        Mov D$LocalChunksTable+eax*4 ebx
        Mov D$LocalChunksTablePages+eax*4 1
EndP

;;
  This Memory extension will only be run, in case when the size of the 'LocalChunksTable'
  dWOrds Memory is found too small. It will never be mad smaller during one execution.
  This is only to make the whole thing entirely dynamic, and should only be executed
  once per need. So, the stupid Copy is enough, as there is no speed problem, here.
;;
[NewMem: ?]

Proc ExtendLocalChunksTable:
    Argument @Indice
    Uses ecx, edx

        Mov edx D@Indice

        inc D$LocalChunksTablePages+edx*4
        Mov ecx D$LocalChunksTablePages+edx*4 | shl ecx 12 ; 001000000000000 = 01000
        push ecx edx
            VirtualAlloc NewMem ecx
        pop edx ecx
        sub ecx PAGESIZE
        shr ecx 2 | Mov esi D$LocalChunksTable+edx*4, edi D$NewMem | rep movsd
        VirtualFree D$LocalChunksTable+edx*4
        move D$FreeLocalChunkPointer+edx*4 D$NewMem | add D$FreeLocalChunkPointer+edx*4  PAGESIZE
        move D$LocalChunksTable+edx*4 D$NewMem

EndP
____________________________________________________________________________________________

[LocalAlloc | Call LocAlloc #1, #2 | #+2]
; Evocation: LocalAlloc Pointer, Size

Proc LocAlloc:
    Arguments @Pointer, @Size
    Local @FollowUpTableSize, @Indice
    Uses esi, edx, ecx, ebx

      ; Room for Address and Bit Map:
        add D@Size 8

        If D@Size < 32
            Mov D@Size 32
        Else_If D@Size > (PAGESIZE/2)
            VirtualAlloc D@Pointer, D@Size
            Mov D$eax eax, D$eax+4 0-1 | add eax 8
            Mov ecx D@Pointer | Mov D$ecx eax
            ExitP
        End_If

        Mov eax D@Size | bsr ecx eax | Mov eax 1 | shl eax cl
        If D@Size > eax
            shl eax 1 | inc ecx
        End_If
        sub ecx 5
      ; ecx = Zero Based Indice for the 'LocalChunksTable' // eax = Aligned Chunk Size
        shl eax 7 ; * 128
        Mov D@Indice ecx, D@FollowUpTableSize eax

        If D$FreeLocalChunkPointer+ecx*4 = 0
            Mov esi D$LocalChunksTable+ecx*4
        Else
            Mov esi D$FreeLocalChunkPointer+ecx*4
        End_If

        If esi = 0
            push ecx
                Call CreateLocalChunksTable ecx
            pop ecx
            Mov esi D$LocalChunksTable+ecx*4
        End_If

      ; esi = Follow-Up Tables List
L0:     lodsd | Mov ebx eax
        .If ebx = 0
            push esi ecx
              ; Allocation of a Mother Chunk. (Size * 128)
                VirtualAlloc NewMem, D@FollowUpTableSize
            pop ecx esi
            Mov D$esi-4 eax, ebx eax

            ; If at the end of the 'LocalChunksTable', extend it for next allocations:
            Mov edx D$LocalChunksTablePages+ecx*4 | shl edx 12 | add edx D$LocalChunksTable+ecx*4
            sub edx 40
            If esi >= edx
                sub esi D$LocalChunksTable+ecx*4
                push esi
                    Call ExtendLocalChunksTable ecx
                pop edx
                Mov esi D$LocalChunksTable+ecx*4 | add esi edx
            End_If
        .End_If

        If D$esi <> 0-1
            Mov eax D$esi, edx 0
        Else_If D$esi+4 <> 0-1
            Mov eax D$esi+4, edx 4
        Else_If D$esi+8 <> 0-1
            Mov eax D$esi+8, edx 8
        Else_If D$esi+12 <> 0-1
            Mov eax D$esi+12, edx 12
        Else
            add esi (4*4)
            Mov D$FreeLocalChunkPointer+ecx*4 esi
            jmp L0<<
        End_If

        not eax | bsf ecx eax
      ; edx = number of bytes of the shift (0,4,8 or 12 bytes after the first DWord of the Follow-Up table)
      ; ecx = Bit position in the (edx/4) DWord of the Follow-Up Table. (0 to 31)
      ; Bit Map is updated
        bts D$esi+edx ecx

        lea eax D$ecx+edx*8
      ; eax = bit position in the Follow-Up Table (0 to 127)

      ; Keep (esi+edx), eax and D@Indice value for LocFree
        add esi edx | Mov edx eax | shl edx 16 | Mov dx W@Indice
      ; Multiplication by ChunkSize
        Mov ecx D@Indice | add ecx 5 | shl eax cl | add eax ebx
      ; eax is now the Address of the new Local Chunk (example: 030510000+(BitIndice*ChunkSize)
        Mov D$eax esi, D$eax+4 edx | add eax 8
        Mov ecx D@Pointer | Mov D$ecx eax
EndP
____________________________________________________________________________________________

[LocalFree  | cmp #1 0 | je V9> | Mov eax #1
                    Call LocFree
               Mov #1 0 | V9: | #+1]
; Evocation: LocalFree D$Pointer (>>> D$Pointer = 0, when back).

LocFree: ; eax = Pointer to mem
    push ebx ecx edx
        If D$eax-4 <> 0-1
            movzx ecx W$eax-4 | movzx edx W$eax-2 | Mov ebx D$eax-8
            ; ebx: Follow-Up Table address of this chunk (points to the proper DWord in the table)
            ; ecx: Zero Based Indice for the 'LocalChunksTable'
            ; edx: bit position in the Follow-Up Table (0 to 127)
            ; Bit Map is updated
            and edx 31 | btr D$ebx edx
            ; we compute the byte in which this bit is from edx
            movzx edx W$eax-2 | and edx 0-32 | shr edx 3 | sub ebx edx | sub ebx 4
            Mov D$FreeLocalChunkPointer+ecx*4 ebx
        Else
            Mov eax D$eax-8 | Call VirtFree
        End_If
    pop edx ecx ebx
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[LocalTest1: ? LocalTest2: ? LocalTest3: ?]

; 'LocalChunksTable'
[MemAddrList: ? #5000 ]
TestLocals:

    Call MessageBox {B$ "TestLocals" EOS},
                    {B$ "Ready to test Local Engine" EOS},
                     &MB_SYSTEMMODAL+&MB_USERICON

    LocalAlloc LocalTest1, 25
    LocalAlloc LocalTest2, 250
    Mov ecx 5000
    .While ecx > 0
        LocalAlloc LocalTest3, 504
        Mov D$MemAddrList+ecx*4 eax
        Mov ebx 0
        While ebx < 504
            Mov B$eax cl
            inc eax
            inc ebx
        End_While
        dec ecx
    .End_While

   ; int3

    LocalFree D$LocalTest2
   ; int3
    Mov ecx 5000
    While ecx > 0
        push ecx | LocalFree D$MemAddrList+ecx*4 | pop ecx
        dec ecx
    End_While
   ; int3
    LocalFree D$LocalTest1
   ; int3

    Call MessageBox {B$ "TestLocals" EOS},
                    {B$ "test done" EOS},
                    &MB_SYSTEMMODAL+&MB_USERICON

ret


[TestLocalTableHandle: ?   TestLocalSize: ?]

; Tag Dialog 15

Proc TestLocalTables:
    Arguments @hwnd, @msg, @wParam, @lParam

    pushad

    ...If D@msg = &WM_COMMAND                  ; User action
        ..If D@wParam = &IDCANCEL                   ; User clicks on upper right [X]
            VirtualFree D$AllocTestMem
            Mov D$FileTypeChoice 0-1
            Call 'User32.EndDialog' D@hwnd 0

        ..Else_If D@wParam = &IDOK
            VirtualFree D$AllocTestMem
            Call 'User32.EndDialog' D@hwnd 0

      ;  ..Else_If D@wParam = &IDHELP
      ;      Call Help B_U_AsmName, DisassemblerHelp, ...

        ..Else
            movzx eax W@wParam
            If ax = 10
                Mov D$TestLocalSize 32
            Else_If ax = 11
                Mov D$TestLocalSize 64
            Else_If ax = 12
                Mov D$TestLocalSize 128

            Else_If ax = 200 ; Allocate
                Call AllocationTest

            Else_If ax = 201 ; Release
                Call ReleaseTest
            End_If

        ..End_If

    ...Else_If D@msg = &WM_INITDIALOG          ; Win ready to build the Dialog
       Call 'USER32.CheckDlgButton' D@hwnd, 10, &TRUE
       Mov D$TestLocalSize 32
       move D$TestLocalTableHandle D@hwnd
       VirtualAlloc AllocTestMem, 010000

  ;  ...Else_If D$Message = &WM_CTLCOLOREDIT        ; Win ready to paint the Dialog
          ;      ; Control of output

    ...Else
        popad | Mov eax &FALSE | ExitP               ; Non processed

    ...End_If

    popad | Mov eax &TRUE                           ; Processed
EndP


[AllocTestMem: ?]

AllocationTest:
    Call 'USER32.GetDlgItemInt' D$TestLocalTableHandle, 20, 0, 0

    If eax = 0

        Call MessageBox {B$ "Local tests" EOS},
                        {B$ "Set some number of Chunks (1 to 1024)" EOS},
                        &MB_SYSTEMMODAL+&MB_USERICON

    Else_If eax > (01000/4)

        Call MessageBox {B$ "Local tests" EOS},
                        {B$ "Set some number of Chunks (1 to 1024)" EOS},
                        &MB_SYSTEMMODAL+&MB_USERICON

    Else
        Mov ecx eax

L0:     push ecx
            LocalAlloc LocalTest1, D$TestLocalSize
            Mov edi D$AllocTestMem
            While D$edi <> 0 | add edi 4 | End_While
            move D$edi D$LocalTest1
        pop ecx | loop L0<
    End_If

    Call ShowLocalTest
ret


ReleaseTest:

ret


[ShowLocalTestMem: ?]

ShowLocalTest: ;ret ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    VirtualAlloc ShowLocalTestMem 0FFFF

        Mov edi D$ShowLocalTestMem

        Mov esi D$LocalChunksTable | On esi = 0, jmp L1>

        If D$esi = 0
L1:         Mov D$edi 'None', W$edi+4 CRLF | add edi 6
        Else
            While D$esi <> 0
                lodsd | Call WriteEax | Mov W$edi ': ' | add edi 2
                lodsd | Call WriteEax | Mov W$edi ', ' | add edi 2
                lodsd | Call WriteEax | Mov W$edi ', ' | add edi 2
                lodsd | Call WriteEax | Mov W$edi ', ' | add edi 2
                lodsd | Call WriteEax | Mov D$edi ' // ' | add edi 4
            End_While
            Mov W$edi CRLF | add edi 2
        End_If

        Mov esi D$LocalChunksTable+4 | On esi = 0, jmp L1>

        If D$esi = 0
L1:         Mov D$edi 'None', W$edi+4 CRLF | add edi 6
        Else
            While D$esi <> 0
                lodsd | Call WriteEax | Mov W$edi ': ' | add edi 2
                lodsd | Call WriteEax | Mov W$edi ', ' | add edi 2
                lodsd | Call WriteEax | Mov D$edi ' // ' | add edi 4
            End_While
            Mov W$edi CRLF | add edi 2
        End_If

        Mov esi D$LocalChunksTable+8 | On esi = 0, jmp L1>

        If D$esi = 0
L1:         Mov D$edi 'None', W$edi+4 CRLF | add edi 6
        Else
            While D$esi <> 0
                lodsd | Call WriteEax | Mov W$edi ': ' | add edi 2
                lodsd | Call WriteEax | Mov D$edi ' // ' | add edi 4
            End_While
            Mov W$edi CRLF | add edi 2
        End_If

        Mov esi D$LocalChunksTable+12 | On esi = 0, jmp L1>

        If D$esi = 0
L1:         Mov D$edi 'None', W$edi+4 CRLF | add edi 6
        Else
            While D$esi <> 0
                lodsd | Call WriteEax | Mov W$edi ': ' | add edi 2
                lodsd | Call WriteEax | Mov D$edi ' // ' | add edi 4
            End_While
            Mov W$edi CRLF | add edi 2
        End_If

    Call 'USER32.SendDlgItemMessageA' D$TestLocalTableHandle, 100,
                                      &WM_SETTEXT, 0, D$ShowLocalTestMem

    VirtualFree D$ShowLocalTestMem
ret













































