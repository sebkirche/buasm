TITLE Mem             ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

;;

    Memory Management
  
    VirtualAlloc Reserves Memory Chunks with a 01_0000 alignement and Commit Chunks with
    a 0_1000 alignement. These Memory Manager Routines purpose is to completely secure
    and simplify the calls for Mems and to save all these rooms spoiled by the 01_0000
    alignement. To simplify things, let us Call 'Mother Chunks' the 01_0000 aligned ones
    (Reservations default) and 'Child Chunks' the 0_1000 aligned ones (Commition default)

    For developements, an Equate [GUARD_PAGE] may be set to 0_1000. This ensure that, in
    case of overflowing writes to a given Memory Chunk, even if 2 chunks are contiguous,
    Win will effectively raise a exception. With this Guard set, the Manager may eat up
    to 8 times less Mem (when dealing with small Chunks). Without Guard ([GUARD_PAGE] set to
    zero), the eaten Mem may be up to 16 times smaller. (This [GUARD_PAGE] has no relation
    with what Win Doc calls so)

    When small Chunks are wanted, the Memory Manager reuse free Chunks that may be found
    inside already Reserved 01_0000 aligned Chunks

    All Chunks are recorded in a [MemTable]. Each Record is:

    Pointer / Size / Mother_Pointer

    Where 'Pointer' is the Pointer to the Committed Chunks, Size is the Size of the
    Committed Chunks eventually added of the GUARD_PAGE (0_1000 or 0), and 'Mother_Pointer'
    is a Pointer to the 01_0000 aligned Chunk to which may belong the Committed Chunk. Even
    the Mother Chunk get this last Value pointing to itself. This last Value is used to
    control if and when, the Memory manager may effectively Release the whole Chunk. Until
    all of the nested and Mother Chunks are not Decommitted, the Memory Manager do not
    Release the whole Mother Chunk, but proper releasing will be done, whatever order of
    the [VirtualFree] calls, even if the [VirtualFree] MotherChunk is called first and
    the [VirtualFree] ThirdChildChunk last

    If more than expected (here [MEM_TABLE_SIZE]) Chunks are required, this is to say,
    if too much Mems are not released (in other words, if i have forgotten to release
    some Allocated Memories), i should also see an error Message 'NoMoreMem' if the
    [MemTable] is overflowed. Added to this i set a Menu Item in Tools to see how
    much Memory Chunks are activated at any time [ViewBUAsmMems]. This does not tell
    how much Mem is used. This only tells how much Chunks are recorded in the [MemTable],
    no matter what Size

    When releasing, if the Pointer value is corrupted, we should also see an error
    Message for [MemInternalAlert]


    All this is much heavy, but gives full control on any Management problem, and the
    calls remain completly simple:

    VirtualAlloc Pointer,
                 Size

    VirtualFree Pointer

    When back from [VirtualAlloc], the Pointer holds the new Memory Pointer.
    [VirtualFree] reset D$Pointer Value to zero. If called a second time (with a nul
    pointer, the Proc aborts without any error Message, because, in some complex cases
    organisation of the source flow, it may become difficult to know if, at some given
    time, the Mem has already be freed or not. This save from having to write:

    On D$Pointer > 0 VirtualFree Pointer

    This is the only one case for which the Manager does not send an error Message
    in case of bad Call from the Application. If a wrong Address is sent to VirtualFree,
    for example an Application Table Address instead of a Pointer content, the proper
    error Message is sent

    A [LastMemRecord] is also managed in order to save from scaning wide [MemTable] for
    nop, as we cannot rely on zeroed Records to know if we have finished scaning

    !!! VirtualAlloc / VirtualFree protect ALL registers

;;
____________________________________________________________________________________________

[MAIN_BLOCK_SIZE (16*PAGE_SIZE)

 GUARD_PAGE       PAGE_SIZE ; GUARD_PAGE = PAGE_SIZE Protection débordement en écriture
; GUARD_PAGE       &FALSE    ; GUARD_PAGE = &FALSE pas de protection débordement en écriture
]

; MemTable is 3 DWORD per record: / LP -> Mem / MemChunkSize / Mother LP /...
[MEM_LP         (0*DWORD)
 MEM_CHUNK_SIZE (1*DWORD)
 MEM_MOTHER_LP  (2*DWORD)
 MEM_RECORD     (3*DWORD) ; !!! A changer en 4*DWORD pour simplifier les calculs et rajouter un flag
 MEM_TABLE_SIZE ((MAXRESOURCE+50)*3) ] ; room for 50 Chunks + Resources Chunks.

[MemChunSize: D$ ?
 LastMemRecord: D$ ?
 TempoMemPointer: D$ ?] ; Used when no named Pointer is used by caller
                        ; (cases of Tables of Memory Pointers, for example)

[TABLE.MemBUAsm: D$ ? # MEM_TABLE_SIZE BUAsmMemTableEnd:]


[MemAlert: B$ 'Memory attribution failure' EOS]
[MemInternalAlert: B$ 'Error inside BUAsm Memory Management' EOS]
[MemAlertText: B$ "   BUAsm is going to close. Shut down some Aplications and re-run      " EOS]

Proc VirtualAlloc:

    Arguments @Pointer,
              @Size

    pushad

        Mov edi TABLE.MemBUAsm

        ; Edi = Lp -> Enregistrement libre
        While D$edi+MEM_LP <> &NULL

            add edi MEM_RECORD | Comp edi BUAsmMemTableEnd = VirtualAllocERROR1

        End_While

        ; GUARD_PAGE = 01_000 entraîne une erreur en cas de débordement d'écriture dans un bloc
        Mov ecx D@Size

        AlignOn PAGE_SIZE ecx | add ecx GUARD_PAGE

        Mov D$MemChunSize ecx,
            D$edi+MEM_CHUNK_SIZE ecx

        ; La réservation mémoire se fait par blocs de 010_000 bytes [MAIN_BLOCK_SIZE]
        ; Aucun bloc n'est inférieur à 0_1000 bytes [PAGE_SIZE]
        ; ou 0_2000 bytes dans le cas de GUARD_PAGE = [PAGE_SIZE]
        ; Les emplacements libres à l'intérieur d'un bloc Main ne sont donc pas supérieurs à
        ; MAIN_BLOCK_SIZE-(PAGE_SIZE+GUARD_PAGE) soit 0_F000 ou 0_E000 si GUARD_PAGE = [PAGE_SIZE]

        Comp ecx (MAIN_BLOCK_SIZE-(PAGE_SIZE+GUARD_PAGE)) > S1>

        Call IsThereSomeRoom
        ; Eax = 0 ou Lp
        ; Eax= Lp / Ebx = Mother Lp

        Test eax eax NOT_ZERO S2>

            ; Bloc parent
S1:         Mov ecx D$MemChunSize | AlignOn MAIN_BLOCK_SIZE ecx

            Call 'KERNEL32.VirtualAlloc' &NULL,
                                         ecx,
                                         &MEM_RESERVE, ;+&MEM_TOP_DOWN,
                                         &PAGE_READWRITE

            On eax = &NULL Call HexPrint D$MemChunSize

            Mov D$edi+MEM_LP eax,        ; Lp
                D$edi+MEM_MOTHER_LP eax  ; Mother Lp = Lp

        jmp S3>

S2:     Mov D$edi+MEM_LP eax,        ; Lp
            D$edi+MEM_MOTHER_LP ebx  ; Mother Lp ([IsThereSomeRoom] ebx)

S3:     On edi > D$LastMemRecord Mov D$LastMemRecord edi

        ; Bloc enfant
        Mov ecx D$MemChunSize | sub ecx GUARD_PAGE

        Call 'KERNEL32.VirtualAlloc' eax,
                                     ecx,
                                     &MEM_COMMIT,
                                     &PAGE_READWRITE

        test eax eax ZERO VirtualAllocERROR2

        ; MAJ: Lp transmis
        Mov edx D@Pointer,
            D$edx eax

    popad

EndP


IsThereSomeRoom:

    ; Recherche si un Search is some empty room is available inside already Reserved Chunks. [MemChunSize]
    ; holds the size of the wanted Block + one Page:
    Mov esi TABLE.MemBUAsm

L1: Comp esi D$LastMemRecord > S4>

        test D$esi+MEM_LP NA ZERO S3>

            Mov eax D$esi+MEM_LP | add eax D$esi+MEM_CHUNK_SIZE

            Mov ebx eax | AlignOn MAIN_BLOCK_SIZE eax | sub eax ebx

        ; Eax = supposed free room in this 01_0000 aligned reserved Chunk or sub-Chunk.
        Comp eax D$MemChunSize < S3>

            Mov eax D$esi+MEM_LP | add eax D$esi+MEM_CHUNK_SIZE

            Mov ebx eax | add ebx D$MemChunSize

            ; Eax = Start // ebx = End of possible free Block.
            ; Both must be outside of any other Declared Block.
            ; This is to say, down here,  (ebx <= ecx) OR (eax >= edx). Example:
            ; eax > edx >>> good:

;    .........................................(eax)XXXXXXXXXXXXXXXXXXXXX(ebx)...
;    ...D$esi+LP_MEMXXXXXXXXXXXXXXXXXXXXX(edx)..................................

        Mov ecx TABLE.MemBUAsm

L2:     Comp ecx D$LastMemRecord > S2>

            Mov edx D$ecx+MEM_LP | add edx D$ecx+MEM_CHUNK_SIZE

            Comp ebx D$ecx+MEM_LP <= S1>

            Comp eax edx < S3>

S1:     add ecx MEM_RECORD | jmp L2<

S2:     Mov eax D$esi+MEM_LP,
            ebx D$esi+MEM_MOTHER_LP

        add eax D$esi+MEM_CHUNK_SIZE

ret

S3: add esi MEM_RECORD | jmp L1<

S4: xor eax eax

ret

VirtualAllocERROR1:

    popad

    ; This Alert can only happend if i completly corrupt a Mem Pointer Value
    ; so that it is no more founded in the [MemTable]

    Call 'USER32.MessageBoxA' &NULL,
                              MemAlertText,
                              MemAlert,
                              &MB_SYSTEMMODAL+&MB_ICONSTOP

    Call ViewBUAsmMems

    Call MessageBox {B$ 'VirtualAlloc Buffer overflowed' EOS}

    Call 'KERNEL32.ExitProcess' 0

VirtualAllocERROR2:

    popad

    Call 'USER32.MessageBoxA' &NULL,
                              MemInternalAlert,
                              MemAlert,
                              &MB_SYSTEMMODAL+&MB_ICONSTOP

    Call ViewBUAsmMems

    Call MessageBox {B$ 'VirtualAlloc Function failure' EOS}

    Call 'KERNEL32.ExitProcess' 0
____________________________________________________________________________________________

Proc VirtualFree:

    Argument @LP.VirtualMem

    pushad

        ; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ; I sometimes increase the original Pointer to set a security top margin allowing
        ; computation begining by 'If D$esi-4 = ...", for example >>> Restore origin:
        ; (Chunks are always Page-aligned)
        ; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        Mov eax D@LP.VirtualMem,
            eax D$eax

        ; !!! TODO !!!
        and eax 0_FFFF_F000 | jz S4>>

        Mov esi TABLE.MemBUAsm

        jmp S1>

L1:     add esi MEM_RECORD

            Comp esi D$LastMemRecord > VirtualFreeERROR

S1:     Comp D$esi+MEM_LP eax <> L1<

        ; Ne jamais détruire un parent directement, c'est la destruction de son dernier
        ; enfant qui lance sa destruction
        Comp D$esi+MEM_MOTHER_LP eax = S5>

        Mov ecx D$esi+MEM_CHUNK_SIZE | sub ecx GUARD_PAGE

        ; Détruire l'enfant
        Call 'KERNEL32.VirtualFree' eax,
                                    ecx,
                                    &MEM_DECOMMIT

        ; Effacer l'enregistrement de l'enfant
        Mov eax D$esi+MEM_MOTHER_LP,
            D$esi+MEM_LP &NULL,
            D$esi+MEM_CHUNK_SIZE &NULL,
            D$esi+MEM_MOTHER_LP &NULL

        ; Si aucun autre bloc enfant n'est présent dans le bloc parent
        ; nous pouvons détruire ce dernier

        Mov esi TABLE.MemBUAsm

L2:     Comp D$esi+MEM_MOTHER_LP eax = S3>

             add esi MEM_RECORD

        Comp esi D$LastMemRecord <= L2<

S2:     Call 'KERNEL32.VirtualFree' eax,
                                    &NULL,
                                    &MEM_RELEASE

S3:     Comp esi D$LastMemRecord <> S4>

        sub D$LastMemRecord MEM_RECORD

S4:     Mov eax D@LP.VirtualMem,
            D$eax &NULL

S5: popad

EndP


VirtualFreeERROR:

    popad

    Call 'USER32.MessageBoxA' &NULL,
                              MemInternalAlert,
                              MemInternalAlert,
                              &MB_SYSTEMMODAL+&MB_ICONSTOP

    Call ViewBUAsmMems

    Call 'USER32.MessageBoxA' &NULL,
                              {B$ 'VirtualFree Buffer overflow' EOS},
                              argh,
                              &MB_SYSTEMMODAL


    Call 'KERNEL32.ExitProcess' &NULL
____________________________________________________________________________________________
____________________________________________________________________________________________

;;

    Call ExtendMemory LP.ActualMemory,
                      LP.ActualSize,
                      DU.NbBytesToAdd

;;

Proc ExtendMemory:

    Arguments @LP.ActualMemory,
              @LP.ActualSize,
              @DU.NbBytesToAdd

    [@LP.NewMemory: D$ ?]

    pushad

        ; Compute the new size
        Mov eax D@LP.ActualSize,
            ecx D$eax

        add ecx D@DU.NbBytesToAdd

        Push ecx

            Mov edi @LP.NewMemory | Call VirtualAlloc edi,
                                                      ecx

            ; Copy from actual to new memory
            Mov esi D@LP.ActualMemory,
                esi D$esi+MEM_LP,
                ecx D$eax

            shr ecx 2 | rep movsd

        ; Replace size
        Pop D$eax

        ; Replace the old memory pointer by the new one
        Call VirtualFree D@LP.ActualMemory

        Mov eax D@LP.ActualMemory | Move D$eax D@LP.NewMemory

    popad

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT
