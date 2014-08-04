; vim: set ts=4 sw=4 tw=99 noet ft=nasm:
;
; AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
; Copyright (C) The AMX Mod X Development Team.
;
; This software is licensed under the GNU General Public License, version 3 or higher.
; Additional exceptions apply. For full license details, see LICENSE.txt or visit:
;     https://alliedmods.net/amxmodx-license

section .text

global amxx_CpuSupport, _amxx_CpuSupport

amxx_CpuSupport:
_amxx_CpuSupport:
	push	ebp
	mov		ebp, esp
	
	push	ebx
	
	mov		eax, 0
	cpuid
	cmp		eax, 1
	jl		.fail
	
	mov		eax, 1
	cpuid
	;check if family == 5 or 4
	and		eax, 0780h	;family mask
	shr		eax, 7		;family shift
	cmp		eax, 5
	je		.fail
	cmp		eax, 4
	je		.fail
	;check if CMOV exists
	shr		edx, 15
	and		edx, 1
	cmp		edx, 0
	je		.fail
	
	mov		eax, 1
	jmp		.end

.fail:
	xor		eax, eax
	
.end:
	
	pop		ebx

	pop		ebp
	ret
