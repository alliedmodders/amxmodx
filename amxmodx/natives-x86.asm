;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (C)2005 by David "BAILOPAN" Anderson 			 ;
; register_native functions for x86    			 ;;;;;;
; Based on the concept by Julien "dJeyL" Laurent      ;
; Thanks to T(+)rget for pushing me to implement this ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Licensed under the GNU General Public License, version 2
;;This is a portion of AMX Mod X 
;; and is maintained by the AMX Mod X development team.

;;Initializes the global variable

global amxx_DynaInit, _amxx_DynaInit
;void amxx_DynaInit(void *ptr);
amxx_DynaInit:
_amxx_DynaInit:
	push	ebp
	mov		ebp, esp
	
	mov		eax, [ebp+8]		;get pointer
	mov		[GLOBAL_GATE], eax	;store
	
	mov		eax, 1
	pop		ebp
	ret
	
;;Assembles the gateway function
global amxx_DynaMake, _amxx_DynaMake
;int amxx_DynaMake(char *buffer, int id);
amxx_DynaMake:
_amxx_DynaMake:
	push	ebp
	mov		ebp, esp
	
	push	edi
	push	esi
	
	mov		edi, [ebp+8]	;buffer
	mov		esi, _amxx_DynaFuncStart
	mov		ecx, _amxx_DynaFuncEnd - _amxx_DynaFuncStart
	cld		;clear direction flag (just in case)
	rep		movsb
	
	mov		edi, [ebp+8]	;get buffer again
	;align us to mov eax, 1234
	add		edi, _amxx_DynaMoveOffset + 1
	mov		eax, [ebp+12]
	mov		[edi], eax
	
	pop		esi
	pop		edi
	
	mov		eax, 1
	pop		ebp
	ret

;;The gateway function we will re-assemble
;; This is similar to dJeyL's but a tad more elegant, as it's written in pure assembly
;; and NASM > GAS :')
global amxx_DynaFunc, _amxx_DynaFunc
;int amxx_DynaFunc(AMX *amx, cell *params);
amxx_DynaFunc:
_amxx_DynaFunc:
_amxx_DynaFuncStart:
	push	ebp
	mov		ebp, esp
	
	;we're given an amx and params... we're also hardcoded for this though:
_amxx_DynaMoveOffset:
	mov		eax, 12345678h		;this old trick, we'll move in the real pointer in a bit.
	push	dword [ebp+12]		;push params
	push	dword [ebp+8]		;push amx
	push	eax					;push the id
	call	[GLOBAL_GATE]		;pass through teh global gateway.
	
	pop		ebp
	ret
_amxx_DynaFuncEnd:

;;Just returns the buffer size required
global _amxx_DynaCodesize, amxx_DynaCodesize
;int amxx_DynaCodesize()
amxx_DynaCodesize:
_amxx_DynaCodesize:
	push	ebp
	mov		ebp, esp
	
	mov		eax, _amxx_DynaFuncEnd - _amxx_DynaFuncStart
	
	pop		ebp
	ret
	
GLOBAL_GATE		DD		0
