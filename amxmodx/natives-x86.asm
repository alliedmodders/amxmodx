; vim: set ts=4 sw=4 tw=99 noet ft=nasm:
;
; AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
; Copyright (C) The AMX Mod X Development Team.
;
; This software is licensed under the GNU General Public License, version 3 or higher.
; Additional exceptions apply. For full license details, see LICENSE.txt or visit:
;     https://alliedmods.net/amxmodx-license

;
; register_native functions for x86
;     Based on the concept by Julien "dJeyL" Laurent
;     Thanks to T(+)rget for pushing to implement this
;

;;Initializes the global variable

section .text

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
	;align us to mov eax, 1234 - on x86 this is 4 bytes
	add		edi, (_amxx_DynaMoveOffset-_amxx_DynaFuncStart) + 1
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
	add		esp, 12				;reset stack oops
	
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
	
	; on x86 is this 17 bytes
	mov		eax, _amxx_DynaFuncEnd - _amxx_DynaFuncStart
	
	pop		ebp
	ret
	
section .data
	
GLOBAL_GATE		DD		0
