title firstproj.asm							;DOS file name of program

.586                                    ;enable all pentium instructions
.model flat, stdcall                    ;memory model & calling convention
.stack 8192                             ;allocate 8k for stack

INCLUDELIB kernel32.lib                 ;Include the kernel 32 library
INCLUDE    \Masm32\Include\Masm32rt.inc
;----------------------------------------------------------
; Constant Definitions
;----------------------------------------------------------

STD_INPUT  equ -10d                     ;Function number for keyboard input
STD_OUTPUT equ -11d                     ;Function number for monitor output

LF equ 10d                              ;Line feed ascii constant
CR equ 13d                              ;Carriage return constant
NEWLINE equ CR,LF                       ;Combine CR and LF for carriage return

ENABLE_PROCESSED_INPUT  equ 1           ;Flag to turn off line buffering
ENABLE_PROCESSED_OUTPUT equ 1           ;Flag to turn off line bufferin
ENABLE_LINE_WRAP        equ 3           ;Flag to trun line wrap on
DISABLE_PROCESSED_INPUT equ 7           ;Flag to turn on line buffering

CREATE_NEW    EQU  1                    ;Parameter for creating a new file
CREATE_ALWAYS EQU  2                    ;Always create (overwrite existing)
OPEN_EXISTING EQU  3                    ;Parameter for opening an existing file
GENERIC_READ  EQU  80000000h            ;Parameter for reading a file
GENERIC_WRITE EQU  40000000h            ;Parameter for writing a file

FILE_SHARE_READ   equ 1
FILE_SHARE_WRITE  equ 2
FILE_SHARE_DELETE equ 4

FILE_ATTRIBUTE_NORMAL equ 80h

HANDLE equ dword
;----------------------------------------------------------
; prototype Declarations for libarary imports
;----------------------------------------------------------


ExitProcess proto,
    dwExitCode:dword				   ;The exit code for the process 

GetStdHandle proto, 
	nStdHandle: dword                  ;The standard device. -10=INPUT, -11=OUTPUT, -13=ERROR

SetConsoleMode proto,                  
    hConsoleHandle:dword,              ;A handle to the console input buffer or a console screen buffer
	dwMode:dword                       ;The input or output mode to be set. 

ReadFile proto,	
    hFile:dword,                       ;A handle to the device
	lpBuffer:near32,                   ;A pointer to the buffer that receives the data read 
    nNumberOfCharsToRead:dword,        ;The maximum number of bytes to be read.
    lpNumberOfbytesRead:near32,        ;A pointer to the variable that receives the number of bytes read
    lpOverlapped:near32                ;A pointer to an OVERLAPPED structure is required if the hFile parameter 
	                                   ;was opened with FILE_FLAG_OVERLAPPED, otherwise it can be NULL.

WriteFile proto,                  
    hFile:dword, lpBuffer:near32,      ;A handle to the device
    nNumberOfCharsToWrite:dword,       ;The maximum number of bytes to be written.
    lpNumberOfbytesWritten:near32,     ;A pointer to the variable that receives the number of bytes written
    lpOverlapped:near32                ;A pointer to an OVERLAPPED structure is required if the hFile parameter 
	                                   ;was opened with FILE_FLAG_OVERLAPPED, otherwise it can be NULL.


CloseHandle proto,                     ;Prototype for closing a file
    fHandle:dword

GetLastError proto                     ;Prototype for getting specific error


CreateFileA proto,                     ;Prototype for CreateFile, used for getting handle to new or existin file
    lpFileName:near32,
	dwDesiredAccess:dword,
	dwShareMode:dword,
	lpSecurityAttributes:near32,
	dwCreationDisposition:dword,
	dwFlagsAndAttributes:dword,
	hTemplateFile:dword

SetConsoleCursorPosition proto,        ;Prototype for setting cursor position
	hConsoleOutput:dword,
	dwCursorPosition:dword

GetConsoleScreenBufferInfo proto,      ;Prototype for getting console info
	hConsoleOutput:dword,
	lpConsoleScreenBufferInfo:near32

 FillConsoleOutputCharacterA proto,    ;Prototype for filling screen with character (used for clear screen)
	hConsoleOutput:dword,
	cCharacter:byte,
	nLength:dword,
	dwWriteCoord:dword,
	lpNumberOfCharsWritten:near32


;----------------------------------------------------------
; Data Segment -- Global Variables
;----------------------------------------------------------
.data
	strAddr			dd		?
	strLength		dd		?
	hStdOut			dd		?
	hStdIn			dd		?
	hFileOut        dd		?
	hFileIn         dd		?
	read			dd		?
	written			dd		?	
	fdata           db		1024 dup(0)
	inFilename      db		256 dup(0)
	filePrompt      db		"Enter filename: ",0
	currChar		dd		?						; Current character in process
	convertedValue	byte	?						; Result of various functions
	convValue		db		8 dup(?)				; Buffer for string to various things.
	HexChars		db		"0123456789ABCDEF",0	; Reference table for hexadecimal ASCII
	;------------------------------------------------------
	; Main Menu Variables
	;------------------------------------------------------
	MenuPrompt		db		"Assignment 2:SKAMAR",NEWLINE,	; Main Menu Prompt
							"1) Load data file",NEWLINE,
							"2) Search for bit sequence",
							NEWLINE,"3) Exit",NEWLINE,"::> ",0
	MMChoice		db		4 dup(?)				; Holds user's choice
	LoadedInfo		db		" bytes loaded",0		; Display number of bytes loaded.
;----------------------------------------------------------
; Code Segment
;----------------------------------------------------------
.code
main proc

				xor eax, eax					; Clear out EAX
				xor ebx, ebx					; Clear out EBX
				xor ecx, ecx					; Clear out ECX
				xor edx, edx					; Clear out EDX
				; ----- Menu Choice Input -----
MainMenu:
				lea		esi, MenuPrompt 		; Load address of Main Menu Prompt
				call	PrintString				; Display main menu
				lea		esi, MMChoice			; Load the address to hold the starting number
				call	GetString				; Get Starting number
				lea		esi, MMChoice			; Load address of where choice is held (ASCII)
				call	StrToDecimal			; Convert input to number
				mov		dl, convertedValue		; Load converted number into d register
				cmp		dl, 3d					; Check to see if we exit
				je		TotsFinished			; Jump To Exit
				cmp		dl, 1d					; Check to see if user wants to enter filename.	
				jne		BitPatternOpt			; Keep testing if it's not
				lea		esi, filePrompt			; Prompt for filename
				call	PrintString				; Display prompt
				lea		esi, inFilename			; Load address of memory holding filename
				call	GetString				; Get Filename.
				call	ReadFileContents		; Read contents of file into buffer
BitPatternOpt:
				mov		al, 2
ContTest:
			
				mov al, 0
TotsFinished:				
	invoke ExitProcess, 0			
main endp

;------------------------------------------------------------------------------
; Procedure to print a string to stdout
;
; Given   :  The Address of Null (0) terminated String to print in ESI register
; process :  Print the String using the kernel32.lib WriteFile to
;         :  Standard_Output function call.  No registers are changed and the
;         :  flags are not affected.
; Return  :  Nothing
;------------------------------------------------------------------------------
GatherInformation proc

GatherInformation endp

DecToIntStr proc
			push	ebp					; Save base pointer
			mov		ebp, esp			; Extablish stack frame
			mov		ax, [ebp+8]			; Move number into AX
			push	ecx					; Save the CX counter
			pushad
			pushfd
			mov		ecx, 2				; Counting backward, most num digits = 3	
			mov		byte ptr[esi+3], 0	; Null Terminator
			cmp		ax, 100d			; If number is less than 100,
			jl		PadOneZero			; add a zero to the start.
			jmp		StartSTIConv		; Continue to conversion loop
PadOneZero: mov		byte ptr[esi], 48d	; Add padding
			cmp		ax, 10d				; If less than 10, add another zero
			jl		PadAnotherZero		
			jmp		StartSTIConv
PadAnotherZero:
			mov		byte ptr [esi+1],48d; Add another zero padding
StartSTIConv:							; If there's more digits...
			xor		edx, edx			; Clear out EDX so DIV won't throw a fit
			mov		ebx, 10d			; Prepeare to divide by ten.
			div		ebx					; Divide EBX by ten
			add		dx, 48d				; Add 30 to the remainder to get resulting ASCII
			mov		[esi+ecx], dl		; Add the character to the string
			dec		ecx					; Descrement the counter
			cmp		ax, 0				; If there is no quotient, we have
			je		EndSTIConv			; reached the end so exit loop.
			jmp		StartSTIConv
EndSTIConv:
			nop
			popfd
			popad
			pop		ecx
			pop		ebp
			ret
DecToIntStr endp

StrToDecimal proc
			pushad
			pushfd
			xor eax, eax
			xor ebx, ebx
			xor ecx, ecx
			xor edx, edx
			
			mov cl, 0					; Set the counter to zero
LoopIn:		
			cmp byte ptr [esi], 0		; Check to see if we've reached the end of the string
			je	EndLoopIn				; If we have (i.e., '\0'), then jump to EndLoopIn
			mov bh, 0					; Set high bx register to 0
			mov bl, cl					; Set low bx register to counter so we can address array
			mov ah, 0					; CLear high AX register
			mov al, 10d					; Load 10 into al as multiplier
			mul edx						; Multiply current result by 10
			mov edx, eax
			mov al, byte ptr [esi]		; Move the next character into dl
			sub al, 48d					; Subtract 48 from the ASCII char to get the integer
			add eax, edx				; Add resultant to current number
			mov edx, eax				; Copy current number into D register
			add cl, 1					; Increment counter
			inc esi						; Increment the string pointer
			jmp LoopIn					; Jump to loop start
EndLoopIn:	
			mov convertedValue, dl		; Copy from D register to memory
			popfd
			popad
			ret
StrToDecimal endp

DecToHexStr proc
			push	ebp					; Save base pointer
			mov		ebp, esp			; Extablish stack freame
			mov		ax, [esp+8]			; Move number into AX
			pushad
			pushfd
			mov		bx, ax				; Copy number into bx, so we can 
			shr		bx, 4				; shift the lower nibble out
			shl		bx,	8				; Shift BX "bacK" so that the higher nibble -> BH
			mov		dx, ax				; Copy number into dx,
			and		dl, 15d				; and get the lower nibble
			xor		dx, bx				; and then recombine the two
			mov		bh, 0				; Zero-out lower bx register for index use
			mov		bl, dl				; Copy destination index into BX register
			push	eax					; Save the state of the AX register
			mov		ah, 0				; Preparing to copy first character
			mov		al, HexChars[bx]	; Fetch the corresponding hex character
			mov		byte ptr[esi+1], al	; Copy first character into memory
			mov		bh, 0
			mov		bl, dh				; About to get "higher" character
			mov		al, HexChars[bx]
			mov		byte ptr[esi], al	; Copy second character to memory
			mov		byte ptr[esi+2], 0	; Copy null terminator to the string.
			pop		eax
			popfd
			popad
			pop		ebp
			ret
DecToHexStr endp





;------------------------------------------------------------------------------
; Procedure to print a string to stdout
;
; Given   :  The Address of Null (0) terminated String to print in ESI register
; process :  Print the String using the kernel32.lib WriteFile to
;         :  Standard_Output function call.  No registers are changed and the
;         :  flags are not affected.
; Return  :  Nothing
;------------------------------------------------------------------------------
PrintString proc                       ; Define procedure
            pushad                     ; save registers
            pushfd                     ; save flags
            mov    strAddr, esi        ; copy string address
                                       ; find string length
            mov    strLength, 0        ; initialize string length
WhileChar:  cmp    byte ptr [esi], 0   ; character = null?
            jz     EndWhileChar        ; exit if so
            inc    strLength           ; increment character count
            inc    esi                 ; point at next character
            jmp    WhileChar           ; while more characters exist
EndWhileChar:
            invoke GetStdHandle,STD_OUTPUT ; get handle for console output
            mov    hStdOut, eax        ; copy file handle for screen
            invoke WriteFile,          ; invoke standard WriteFile with
              hStdOut,                 ;   file handle for screen
              strAddr,                 ;   address of string
              strLength,               ;   length of string
              near32 ptr written,      ;   variable for # bytes written
              0                        ;   overlapped mode
            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller
PrintString endp


;------------------------------------------------------------------------------
; Procedure to get a string from stdin
;
; Given   :  The Address of the String to fill in ESI register
; process :  Input the String using the kernel32.lib ReadFile from the
;         :  Standard_Input function call.  No registers are changed and the
;         :  flags are not affected.
; Return  :  The input string in the data segment
;------------------------------------------------------------------------------
GetString proc                         ; Define procedure
            pushad                     ; save all registers
            pushfd                     ; save flags

            invoke GetStdHandle,STD_INPUT  ; get handle for console
            mov    hStdIn, eax         ; save the handle
            invoke SetConsoleMode,     ; invoke standard console with
              hStdIn,                  ;   file handle for keyboard
              DISABLE_PROCESSED_INPUT  ;   turn line buffering on

            mov    ecx, 255d;MAXSTR    ; string length
            mov    strLength, ecx      ; maximum string to accept
            mov    strAddr, esi        ; save pointer to input string
            invoke ReadFile,           ; invoke standard ReadFile with
              hStdIn,                  ;   file handle for keyboard
              strAddr,                 ;   address of string
              strLength,               ;   length of string
              near32 ptr read,         ;   variable for # bytes read
              0                        ;   overlapped mode
            mov ecx, read              ; number of bytes read
            mov byte ptr [esi+ecx-2],0 ; replace CR/LF by trailing null

            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller
GetString   endp


;------------------------------------------------------------------------------
; Procedure to read file (filename in inFilename) into fdata buffer
;------------------------------------------------------------------------------
ReadFileContents  proc                 ; Define procedure
            pushad                     ; save all registers
            pushfd                     ; save flags

            invoke CreateFileA, near32 ptr inFilename, GENERIC_READ, FILE_SHARE_READ,
			   0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0

            mov hFileIn, eax           ; save the handle

            mov ecx, 1024d             ; string length
            mov strLength, ecx         ; maximum string to accept
            mov strAddr, esi           ; save pointer to input string
            invoke ReadFile,           ; invoke standard ReadFile with
              hFileIn,                 ;   file handle for keyboard
              strAddr,                 ;   address of string
              strLength,               ;   length of string
              near32 ptr read,         ;   variable for # bytes read
              0                        ;   overlapped mode
            mov ecx, read              ; number of bytes read

            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller
ReadFileContents   endp

end  ; end directive to compiler