TITLE COMMUNS_PROCs   ; Version A.Bvvv DD.MM.YY maintainer email and version number go here
____________________________________________________________________________________________

[NMHDR.hwndFromDis 0
 NMHDR.idFromDis 4
 NMHDR.codeDis 8]

;;
    String2Dword

    This function converts a Null Terminated Decimal Ascii String to a 32 Bits Dword Value.
    
    Arguments:
    
    String:    The inputed String to be converted
    
    Return Values:
        If the function suceeds eax returns the value of the dword string.
        If the function fails eax returns 0.
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
                Mov ebx 10
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
____________________________________________________________________________________________
____________________________________________________________________________________________
; EOT

