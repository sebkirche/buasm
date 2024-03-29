TITLE Mem
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
                                   Memory Management.
____________________________________________________________________________________________
____________________________________________________________________________________________
 Managements for VirtualAlloc:

 VirtualAlloc Reserves Memory Chunks with a 01_0000 alignement and Commit Chunks with
 a 0_1000 alignement. These Memory Manager Routines purpose is to completely secure
 and simplify the calls for Mems and to save all these rooms spoiled by the 01_0000
 alignement. To simplify things, let us Call 'Mother Chunks' the 01_0000 aligned ones
 (Reservations default) and 'Child Chunks' the 0_1000 aligned ones (Commition default).

 For developements, an Equate 'GUARDPAGE' may be set to 0_1000. This ensure that, in
 case of overflowing writes to a given Memory Chunk, even if 2 chunks are contiguous,
 Win will effectively raise a exception. With this Guard set, the Manager may eat up
 to 8 times less Mem (when dealing with small Chunks). Without Guard (GUARDPAGE set to
 zero), the eaten Mem may be up to 16 times smaller. (This 'GUARDPAGE' has no relation
 with what Win Doc calls so).

 When small Chunks are wanted, the Memory Manager reuse free Chunks that may be found
 inside already Reserved 01_0000 aligned Chunks.

 All Chunks are recorded in a 'TABLE.MemBUAsm'. Each Record is:

 Pointer / Size / Mother_Pointer

 Where 'Pointer' is the Pointer to the Committed Chunks, Size is the Size of the
 Committed Chunks eventually added of the GUARDPAGE (0_1000 or 0), and 'Mother_Pointer'
 is a Pointer to the 01_0000 aligned Chunk to which may belong the Committed Chunk. Even
 the Mother Chunk get this last Value pointing to itself. This last Value is used to
 control if and when, the Memory manager may effectively Release the whole Chunk. Until
 all of the nested and Mother Chunks are not Decommitted, the Memory Manager do not
 Release the whole Mother Chunk, but proper releasing will be done, whatever order of
 the 'VirtualFree' calls, even if the 'VirtualFree D$MotherChunk' is called first and
 the 'VirtualFree D$ThirdChildChunk' last.

 If more than expected (here ' MEM_TABLE_SIZE') Chunks are required, this is to say,
 if too much Mems are not released (in other words, if i have forgotten to release
 some Allocated Memories), i should also see an error Message ('NoMoreMem') if the
 'TABLE.MemBUAsm' is overflowed. Added to this i set a Menu Item in [Tools] to see how
 much Memory Chunks are activated at any time ('ViewRosAsmMems'). This does not tell
 how much Mem is used. This only tells how much Chunks are recorded in the 'TABLE.MemBUAsm',
 no matter what Size.

 When releasing, if the Pointer value is corrupted, we should also see an error
 Message for 'MemInternalAlert'.


 All this is much heavy, but gives full control on any Management problem, and the
 calls remain completly simple:

 > VirtualAlloc Pointer
 > VirtualFree D$Pointer

 When back from 'VirtualAlloc' Macro, the 'Pointer' holds the new Memory Pointer.
 'VirtualFree' reset 'D$Pointer' Value to zero. If called a second time (with a
 zeroed pointer, the Macro aborts without any error Message, because, in some
 complex cases organisation of the source flow, it may become difficult to know if,
 at some given time, the Mem has already be freed or not. This save from having to
 write:

 > On D$Pointer > 0, VirtualFree D$Pointer

 This is the only one case for which the Manager does not send an error Message
 in case of bad Call from the Application. If a wrong Address is sent to VirtualFree,
 for example an Application Table Address instead of a Pointer content, the proper
 error Message is sent.


 A 'LP.LastMemRecord' is also managed in order to save from scaning wide 'MenTable' for
 nop, as we cannot rely on zeroed Records to know if we have finished scaning.
;;
____________________________________________________________________________________________

[GUARDPAGE 01000]   ; Comment / unComment these 2 Equates for
;[GUARDPAGE 0]      ; having / not having hand made Guard Pages.

; TABLE.MemBUAsm is 3 dWords per record: pointer-to-Mem / MemChunkSize / Mother Pointer// ...
[MEM_LP             (0*DWORD)
 MEM_CHUNK_SIZE     (1*DWORD)
 MEM_MOTHER_LP      (2*DWORD)
 MEM_RECORD         (3*DWORD)
 MEM_TABLE_SIZE (((MAXRESOURCE+50)*3)*DWORD) ] ; room for 50 Chunks + Resources Chunks.

[TABLE.MemBUAsm: B$ ? # MEM_TABLE_SIZE] [MemTableEnd: ?    MemChunSize: ?]

[MemAlert: 'Memory attribution failure', 0
 MemInternalAlert: 'Error inside RosAsm Memory Management', 0
 MemAlertText: "   RosAsm is going to close. Shut down some Aplications and re-run      " 0]

[LP.LastMemRecord: D$ ?
 TempoMemPointer: D$ ?]    ; used when no named Pointer is used by
                                            ; caller (cases of Tables of Memory Pointers,
                                            ; for example).
[VirtAllocOverFlowString: 'VirtualAlloc Buffer overflowed', 0]
[VirtAllocFailureString:  'VirtualAlloc Function failure' 0]

Proc VirtAlloc:
   Arguments @sz, @ptr
   USES ecx edx ebx esi edi
        Mov edi TABLE.MemBUAsm                    ; Search an empty Record.
        While D$edi > 0
            add edi MEM_RECORD
          ; This Alert can only happend if i completly corrupt a Mem Pointer Value
          ; so that it is no more founded in the 'TABLE.MemBUAsm':
            If edi = MemTableEnd

                Call MessageBox MemAlert,
                                MemAlertText,
                                &MB_SYSTEMMODAL+&MB_ICONSTOP

                Call ViewBUAsmMems

                ShowMe VirtAllocOverFlowString | jmp END

            End_If
        End_While

      ; The 'add eax GUARDPAGE' is to reserve a never Committed Page (allow Win
      ; error if i overflow write to a Buffer):
        Mov eax D@sz | Align_On PAGESIZE eax | add eax GUARDPAGE
        Mov D$MemChunSize eax, D$edi+4 eax

      ; VirtualAlloc Reserves by 01_0000 Octets Block (16 Pages). So, as any previous
      ; Committed Block can not be smaller than 0_1000 or 0_2000 Octets (1 Page + 1 Page
      ; for 'Guard', the not used remaining space of any reservation cannot be bigger
      ; than 01_0000-(PAGESIZE+GUARDPAGE) (0_E000 or 0F000).
        If D$MemChunSize <= (01_0000-(PAGESIZE+GUARDPAGE))
            push edi | Call IsThereSomeRoom | pop edi   ; >>> eax= 0 or Pointer.
        Else                                            ; eax= Pointer >>> ebx = MotherPtr.

            Mov eax 0
        End_If

        If eax = 0
            push edi
                Mov eax D$MemChunSize | Align_On 010_000 eax

                Call 'KERNEL32.VirtualAlloc' &NULL,
                                             eax,
                                             &MEM_RESERVE+&MEM_TOP_DOWN,
                                             &PAGE_READWRITE

                On eax = 0, hexprint D$MemChunSize
                ;hexprint eax
            pop edi
            Mov D$edi eax, D$edi+8 eax      ; D$edi+8 = Pointer
        Else
            Mov D$edi eax, D$edi+8 ebx      ; D$edi+8 = MotherPointer ('IsThereSomeRoom' ebx)
        End_If

        On edi > D$LP.LastMemRecord, Mov D$LP.LastMemRecord edi
        Mov ecx D$MemChunSize | sub ecx GUARDPAGE
      ;  push eax, ecx                      ; For my Tests
        Call 'KERNEL32.VirtualAlloc' eax, ecx, &MEM_COMMIT, &PAGE_READWRITE
      ;  pop ecx, ebx                       ; For my Tests
        If eax = 0
      ;      hexprint ecx | hexprint ebx    ; For my Tests
            Call MessageBox MemAlert,
                            MemInternalAlert,
                            &MB_SYSTEMMODAL+&MB_ICONSTOP

            Call ViewBUAsmMems

            ShowMe VirtAllocFailureString | jmp END

        End_If
    Mov ebx D@ptr
    Mov D$ebx eax                           ; Writes value in pointer & in eax!
EndP

[VirtualAlloc | push #1 | push #2 | Call VirtAlloc | #+2]
; Evocation: VirtualAlloc Pointer, Size
____________________________________________________________________________________________

[VirtFreeOverFlowString: 'VirtualFree Buffer overflow' 0]

VirtFree:       ; eax = D$Pointer
    pushad

  ; I sometimes increase the original Pointer to set a security top margin allowing
  ; computation begining by 'If D$esi-4 = ...", for example >>> Restore origin:
    and eax 0_FFFF_F000                 ; (Chunks are always Page-aligned).

    Mov esi TABLE.MemBUAsm
    While D$esi <> eax

        add esi MEM_RECORD

        Comp esi MemTableEnd = S2>

    End_While

    push esi
        Mov ecx D$esi+4 | sub ecx GUARDPAGE
        Call 'KERNEL32.VirtualFree' D$esi, ecx, &MEM_DECOMMIT
    pop esi

  ; Now, we can Release the whole Memory Block only if no other Chunk is Committed.
  ; If so, no other Block whith the same origin (Third dWord of records) can be
  ; found in the 'TABLE.MemBUAsm':
    Mov ebx D$esi+8, ecx 0
    Mov D$esi 0, D$esi+4 0, D$esi+8 0       ; Clear the record, anyway.
    push esi
        Mov esi TABLE.MemBUAsm
        While esi <= D$LP.LastMemRecord
            If D$esi+8 = ebx
                inc ecx
            End_If
            add esi MEM_RECORD
        End_While
        On ecx = 0, Call 'KERNEL32.VirtualFree' ebx, &NULL, &MEM_RELEASE
    pop esi
    On esi = D$LP.LastMemRecord, sub D$LP.LastMemRecord MEM_RECORD
L9: popad
ret

S2: Call MessageBox MemInternalAlert,
                    MemInternalAlert,
                    &MB_SYSTEMMODAL+&MB_ICONSTOP

    Call ViewBUAsmMems

    ShowMe VirtFreeOverFlowString

    jmp END

[VirtualFree | cmp #1 0 | je V9> | Mov eax #1
                    Call VirtFree
               Mov #1 0 | V9: | #+1]

; Evocation: VirtualFree D$Pointer (>>> D$Pointer = 0, when back).
____________________________________________________________________________________________

; Search is some empty room is available inside already Reserved Chunks. 'D$MemChunSize'
; holds the size of the wanted Block + one Page:

IsThereSomeRoom:
    Mov esi TABLE.MemBUAsm
    .While esi <= D$LP.LastMemRecord
        ..If D$esi > 0
            Mov eax D$esi | add eax D$esi+4
            Mov ebx eax | Align_On 01_0000 eax | sub eax ebx

          ; eax = supposed free room in this 01_0000 aligned reserved Chunk or sub-Chunk.
            .If eax >= D$MemChunSize
                Mov eax D$esi | add eax D$esi+4
                Mov ebx eax | add ebx D$MemChunSize
              ; eax = Start // ebx = End of possible free Block.
                Call IsThisRoomFree
                If eax = &TRUE
                    Mov eax D$esi, ebx D$esi+8 | add eax D$esi+4 | jmp L9>>
                End_If
            .End_If
        ..End_If
        add esi MEM_RECORD
    .End_While
    Mov eax 0
L9: ret

;;
 in: eax = candidate Block start // ebx = end. Both must be outside of any other
 Declared Block. This is to say, down here,  (ebx <= ecx) OR (eax >= edx). Example:
 eax > edx >>> good:

 .....................................(eax)XXXXXXXXXXXXXXXXXXXXX(ebx)......
 ....(ecx)XXXXXXXXXXXXXXXXXXXXX(edx).......................................
;;
IsThisRoomFree:
    push esi
        Mov esi TABLE.MemBUAsm
        While esi <= D$LP.LastMemRecord
            Mov ecx D$esi, edx ecx | add edx D$esi+4
            If ebx <= ecx
                ; OK
            Else_If eax >= edx
                ; OK
            Else
                Mov eax &FALSE | jmp L9>
            End_If
            add esi MEM_RECORD
        End_While
        Mov eax &TRUE
L9: pop esi
ret

____________________________________________________________________________________________

____________________________________________________________________________________________
____________________________________________________________________________________________



[ExtendMemory | Call NewMemory #1, #2, #3 | #+3]

; AddMemory MemoryPointer, ActualSizePointer, SizeToAdd

Proc NewMemory:
    Arguments @Pointer, @SizePointer, @Added
    [@NewPointer: ?]

        pushad
          ; Compute the new Size:
            Mov ecx D@SizePointer, ecx D$ecx | add ecx D@Added

            VirtualAlloc @NewPointer, ecx

          ; Copy from Old to new memory:
            push ecx
                Mov esi D@Pointer, esi D$esi, edi eax, ecx D@SizePointer, ecx D$ecx
                shr ecx 2 | rep movsd
            pop ecx

          ; Adjust the new Size:
            Mov ebx D@SizePointer, D$ebx ecx

          ; Replace the old Memory Pointer by the new one:
            Mov ebx D@Pointer | Exchange D$ebx D@NewPointer

            VirtualFree D@NewPointer
        popad
EndP










