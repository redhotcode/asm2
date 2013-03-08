title firstproj.asm							;DOS file name of program
;----------------------------------------------------------
; SHANE KAMAR - SECOND ASSIGNMENT - ASSEMBLY
;----------------------------------------------------------
.586                                    ;enable all pentium instructions
.model flat, stdcall                    ;memory model & calling convention
.stack 8192                             ;allocate 8k for stack

INCLUDELIB kernel32.lib                 ;Include the kernel 32 library
INCLUDE \masm32\include\masm32rt.inc		;Include windows functions.s
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
FULLMASK	  EQU  255d					;Full mask
FILE_SHARE_READ   equ 1
FILE_SHARE_WRITE  equ 2
FILE_SHARE_DELETE equ 4

FILE_ATTRIBUTE_NORMAL equ 80h

; Menu Selections (in decimal)
MENU_EXIT			equ	3
MENU_ENTER_FILENAME	equ	1
MENU_ENTER_PATTERN	equ	2

; Bases
BASE_TEN			equ	10d
BASE_BINARY			equ	2d

;----------------------------------------------------------
; prototype Declarations for libarary imports
;----------------------------------------------------------

system proto c :dword

ExitProcess proto,
    dwExitCode:dword				   ;The exit code for the process 

GetStdHandle proto, 
	nStdHandle: dword                  ;The standard device. -10=INPUT, -11=OUTPUT, -13=ERROR

SetConsoleMode proto,                  
    hConsoleHandle:dword,              ;A handle to the console input buffer or a console screen buffer
	dwMode:dword                       ;The input or output mode to be set. 



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
	fErrMsg			db		"File not found!",NEWLINE,0  ; Error Message
	MenuPrompt		db		"Assignment 2:SHANE KAMAR",NEWLINE,	; Main Menu Prompt
							"1) Load data file",NEWLINE,
							"2) Search for bit sequence",
							NEWLINE,"3) Exit",NEWLINE,"::> ",0
	PressPrompt		db		"Press any key to continue...",NEWLINE,0	; Prompt.
	BadInfoPrompt	db		"Too few or too many bits. Must be 3-8 bits.",NEWLINE,0 ; Error.
	cmdClearScr		db		"cls",0					; Holds constant for clearing the screen
	MenuChoice		db		4 dup(?)				; Holds user's choice
	LoadedInfo		db		" bytes loaded",NEWLINE,0; Display number of bytes loaded.
	lastError		dd		?						; Holds the error code of the last error
	bitpString		db		16 dup (0)				; The input string for the bit pattern.
	bitpPrompt		db		"Enter bit pattern: ",0 ; Prompt for bit pattern
	bitpMask		byte	0						; The actual bit pattern.
	bitpMatch		byte	0						; The unused bits to mask.
	digitCount		byte	0						; Bit Count on the pattern.
	BaseFactor		db		2						; Base for parsing and generating strings from numbers.
	matchCount		dword	0						; Counts number of matches.
	matchPrompt		db		" occurences found.",NEWLINE,0; Presents with info about matches.
;----------------------------------------------------------
; Code Segment
;----------------------------------------------------------
.code
main proc

				xor eax, eax					; Clear out EAX
				xor ebx, ebx					; Clear out EBX
				xor ecx, ecx					; Clear out ECX
				xor edx, edx					; Clear out EDX
MainMenu:
				invoke	system, addr cmdClearScr; Clear the screen.
				lea		esi, MenuPrompt 		; Load address of Main Menu Prompt
				call	PrintString				; Display main menu
				lea		esi, MenuChoice			; Load the address to hold the starting number
				call	GetString				; Get Starting number
				lea		esi, MenuChoice			; Load address of where choice is held (ASCII)
				mov		BaseFactor, BASE_TEN	; Base Ten for choosing a menu option.
				call	StrToDecimal			; Convert input to number
				mov		dl, convertedValue		; Load converted number into d register
				cmp		dl, MENU_EXIT			; Check to see if we exit
				je		TotsFinished			; Jump To Exit
				cmp		dl, MENU_ENTER_FILENAME	; Check to see if user wants to enter filename.	
				jne		BitPatternOpt			; Keep testing if it's not
FilePrompt:
				lea		esi, filePrompt			; Prompt for filename
				call	PrintString				; Display prompt
				lea		esi, inFilename			; Load address of memory holding filename
				call	GetString				; Get Filename and store in inFilename
				lea		esi, fdata				; Load address of file buffer into stack pointer
				call	ReadFileContents		; Read contents of file into buffer
		.IF lastError==ERROR_SUCCESS			; If no File IO errors occurred
				mov		eax, read				; Get the bytes read
				push	eax						; Push onto stack as first argument
				lea		esi, convValue			; Get the place to hold converted string.
				call	DecToIntStr				; Convert value to int string
				lea		esi, convValue			; Get the converted string again.
				call	PrintString				; Print the number of bytes
				lea		esi, LoadedInfo			; and now the rest of the line.
				call	PrintString				; Display.
				jmp		ReturnToTop				; Prompt to continue, clear screen.
		.ELSE
				jmp		FilePrompt
		.ENDIF
BitPatternOpt:
				lea		esi, bitpPrompt			; Prompt for bit pattern
				call	PrintString				; Display prompt
				lea		esi, bitpString			; Get ready to receive input
				call	GetString				; Get the bit prompt
				mov		BaseFactor, BASE_BINARY ; Working with binary.
				call	StrToDecimal			; Convert the string to a decimal.
				mov		al, convertedValue		; Copy resultant byte to register
				mov		bitpMask, al			; Copy to memory
				mov		bl, digitCount			; Check digit length
		.IF bl < 3
				jmp		BadInfSec
		.ELSEIF bl > 8
				jmp		BadInfSec
		.ENDIF
				jmp		DoRealWork

BadInfSec:		
				lea		esi, BadInfoPrompt		; Load "bad info" prompt
				call	PrintString				; Display
				jmp		ReturnToTop
ReturnToTop:	
				lea		esi, PressPrompt		; Ask to press any key
				call	PrintString				; Display prompt
				lea		esi, MenuChoice			; For the empty input
				call	GetString				; There we go.
				jmp		MainMenu				; Jump back to main menu.

DoRealWork:
				call	ScanFile				; Scan the file for the bit pattern.
				lea		esi, convValue			; Converting match count to string
				mov		eax, matchCount			; Get the match count
				push	eax						; Push match count on as an argument
				call	DecToIntStr				; Convert to string
				call	PrintString				; Display count
				lea		esi, matchPrompt		; Display matching prompt
				call	PrintString
				jmp		ReturnToTop				; Display the "press any key..." and loop.
TotsFinished:				
	invoke ExitProcess, 0			
main endp

;------------------------------------------------------------------------------
; Scans the file for the bit pattern.
;------------------------------------------------------------------------------
ScanFile proc
			pushad
			pushfd
			xor eax, eax
			xor ebx, ebx
			xor ecx, ecx
			xor edx, edx
			; Prepare bitmask
			mov	edx, FULLMASK			; Get ready to make mask.
			mov al, 8d					; Get ready to subtract.
			mov bl, digitCount			; Get digit count for bitmask
			sub ax, bx					; Subtract
			mov bx, ax					; Swap back AX
		.WHILE BX > 0					; Loop through and shift appropriately
			shr edx, 1
			dec ebx
		.ENDW
			mov bl, 0					; Now shift the mask "back"
		.WHILE BX > 0					; Loop through and shift appropriately
			shl edx, 1
			dec ebx
		.ENDW
			mov bl, bitpMask			; Load pattern
			and bl, dl					; Flip out unused part
			mov bitpMatch, bl			; Save pattern
			; Prepare file buffer
InitLoad:
			lea	esi, fdata				; Point to our file data
			cmp word ptr [esi], 0		; Check start of file.
			je	DoneWithScan			; If EOF, exit loop
			mov ax, word ptr [esi]		; Load initial word.
			inc esi						; Increment ESI
			
StartMatch:
			mov ecx, 8d					; Set counter to 8 bits	
	.WHILE ECX > 0
			push eax					; Save the state of the buffered data
			and ax, dx					; Mask out unused portion of a register
			cmp al, bl					; See if we are a match
			jnz NotMatched				; Skip the matching increment.
Matched:
			push ecx					; Save counter state
			mov ecx, matchCount			; Load match count from memory
			inc ecx						; Increase counter
			mov matchCount, ecx			; Push back to memory
			pop ecx						; Restore counter state.
NotMatched:
			pop eax						; Restore A register.
			shr eax, 1					; Shift left by one.
			dec ecx						; Decrement byte counter
	.ENDW
ScanByteLoop:
			cmp byte ptr [esi], 0		; See if we are null yet.
			je DoneWithScan				; Skip to done.
			mov ah, byte ptr [esi]	; Copy WORD into register. Now we have 16 bits again.
			inc esi						; Increment stack pointer
			mov	ecx, 8d					; Set counter to 8 bits to shift.			
			jmp StartMatch				; Match the next byte.
DoneWithScan:
			popfd
			popad
			ret
ScanFile endp

;------------------------------------------------------------------------------
; Convert an integer to a string.
;------------------------------------------------------------------------------
DecToIntStr proc
			push	ebp					; Save base pointer
			mov		ebp, esp			; Extablish stack frame
			mov		ax, [ebp+8]			; Move number into AX
			push	ecx					; Save the CX counter
			pushad
			pushfd
			mov		ecx, 0				; Counting upward
StartSTIConv:							; If there's more digits...
			xor		edx, edx			; Clear out EDX so DIV won't throw a fit
			mov		ebx, 10d			; Prepeare to divide by ten.
			div		ebx					; Divide EBX by ten
			add		dx, 48d				; Add 30 to the remainder to get resulting ASCII
			push	dx
			inc		ecx					; Descrement the counter
			cmp		ax, 0				; If there is no quotient, we have
			je		EndSTIConv			; reached the end so exit loop.
			jmp		StartSTIConv
EndSTIConv:
			nop
			mov ebx, ecx				; Copy counter into EBX
			xor ecx, ecx				; Zero-out counter
	.WHILE ECX < EBX					; Count up until at EBX
			pop		dx					; Pop another 'digit' off
			mov		[esi+ecx], dl		; Copy digit into memory
			inc		ecx					; Increase counter
	.ENDW
			xor		edx, edx			; Zero-out D register
			mov		[esi+ecx], dl		; Copy null terminator
			popfd
			popad
			pop		ecx
			pop		ebp
			ret
DecToIntStr endp

;------------------------------------------------------------------------------
; Convert a string to a decimal or integer.
;------------------------------------------------------------------------------
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
			mov al, BaseFactor			; Load 10 into al as multiplier
			mul edx						; Multiply current result by 10
			mov edx, eax
			mov al, byte ptr [esi]		; Move the next character into dl
			sub al, 48d					; Subtract 48 from the ASCII char to get the integer
			add eax, edx				; Add resultant to current number
			mov edx, eax				; Copy current number into D register
			add cl, 1					; Increment counter
			mov digitCount, cl			; Copy digit count to memory
			inc esi						; Increment the string pointer
			jmp LoopIn					; Jump to loop start
EndLoopIn:	
			mov convertedValue, dl		; Copy from D register to memory
			popfd
			popad
			ret
StrToDecimal endp

;------------------------------------------------------------------------------
; Convert integer to hexadecimal string
;------------------------------------------------------------------------------
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

            invoke CreateFileA, OFFSET inFilename, GENERIC_READ, FILE_SHARE_READ,
			   0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0

	.IF eax==INVALID_HANDLE_VALUE		; Check to see if handle is invalid
			push esi					; Save stack pointer
			lea esi, fErrMsg			; Prepare to show error
			call PrintString			; Show error.
			pop esi						; Restore stack pointer
			call GetLastError			; Get the error code behind what went wrong
			mov lastError, eax			; Copy the error code into a variable.
			mov read, 0					; Zero bytes read.
	.ELSE
			mov hFileIn, eax			; save the handle
            mov ecx, 1024d				; string length
            mov strLength, ecx			; maximum string to accept
            mov strAddr, esi			; save pointer to input string
            invoke ReadFile,			; invoke standard ReadFile with
              hFileIn,					;   file handle for keyboard
              strAddr,					;   address of string
              strLength,				;   length of string
              near32 ptr read,			;   variable for # bytes read
              0							;   overlapped mode
            mov ecx, read				; number of bytes read
			invoke GetLastError			; Check for reading errors.
			mov lastError, eax			; Copy result to memory.
	.ENDIF
            

            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller
ReadFileContents   endp

end  ; end directive to compiler