;(C)2004-2006 AMX Mod X Development Team
;Written by David "BAILOPAN" Anderson
;These routines were very hard to optimize.  
;They are basically unoptimizable as far as I can tell,
; but it's one of the most expensive operations in core.


section .text

%ifdef LINUX
%define __snprintf snprintf
%define _get_amxstring_r get_amxstring_r
%define _MNF_GetAmxString MNF_GetAmxString
%define _translate translate
%define _LogError LogError
%endif

extern _LogError, _get_amxstring_r, __snprintf, _MNF_GetAmxString
extern _translate

global do_amx_format, _do_amx_format, format_parameter
global init_format_jumps, _init_format_jumps

init_format_jumps:
_init_format_jumps:
	push	ebp
	mov	ebp, esp
	
	lea	edx, [g_jumptbl]
	mov	[edx+'c'*4], dword format_parameter.fmt_num
	mov	[edx+'d'*4], dword format_parameter.fmt_num
	mov	[edx+'f'*4], dword format_parameter.fmt_float
	mov	[edx+'g'*4], dword format_parameter.fmt_float
	mov	[edx+'i'*4], dword format_parameter.fmt_num
	mov	[edx+'L'*4], dword format_parameter.fmt_ml
	;mov	[edx+'p'*4], dword format_parameter.fmt_ptr
	mov	[edx+'s'*4], dword format_parameter.fmt_string
	mov	[edx+'x'*4], dword format_parameter.fmt_num

	pop	ebp
	ret
	

;size_t do_amx_format_parameter(AMX *amx, cell *params, int *param, size_t maxlen);
;REGISTER USAGE (fmt search)
;  edi - fmt output
;  esi - lexptr (given)
;  ebx - scrach address
;  eax - lexchar
;  edx - scratch
;  ecx - length
;REGISTER USAGE (format)
;  edi - real output
;  esi - lexptr (given)
format_parameter:
	push	ebp
	mov	ebp, esp
	
	push	edi
	push	ebx
	push	esi
	
	;len=30
	mov	ecx, 30
	;edi=char[32] (+1 for offset)
	sub	esp, 32
	mov	[esp], byte '%'
	lea	edi, [esp+1]
	;esi=lexptr already
	;eax=*lexptr
	xor	eax, eax
	mov	al, [esi]
	test	al, al
	jz	.abort
	;ebx=g_chartbl
	lea	ebx, [g_chartbl]
	;start looping
.fmtloop
	mov	edx, [ebx+eax*4] ;get char flag
	test	edx, edx	;is it zero?
	jnz	.fmtdone	;yes, we've got a format code
	test	ecx, ecx	;are we over maxlen?
	jz	.fmtdone	;yes, dump out
	mov	[edi], al	;copy into destination
	inc	edi		;dest++
	inc	esi		;src++
	dec	ecx		;len--
	mov	al, [esi]	;get next char
	test	al, al		;is it zero?
	jnz	.fmtloop	;no, continue
	
.fmtdone:
	;if there's no control code, we dumped out.
	;just abort in that case.
	test	al, al
	jz	.abort
	;terminate fmtptr
	mov	[edi], al
	mov	[edi+1], byte 0
	;sto fmtsrc back
	inc	esi
	;get output ptr
	mov	edi, [ebp-4]
	mov	edx, [ebp-12]
	lea	ebx, [g_jumptbl]
	jmp	[ebx+eax*4]	;LOLolOLoL.
	
.fmt_string
	;check parameter count
	mov	ebx, [ebp+12]	;params
	mov	eax, [ebx]	;params[0]
	shr	eax, 2		;params[0]/4
	mov	edx, [ebp+16]	;param
	mov	ecx, [edx]	;*param
	cmp	ecx, eax	;*param / params[0]/4 ?
	ja	.error
	;get the param - it's in eax
	add	dword [edx], 1
	mov	eax, ebx
	sub	esp, 2048
	mov	ebx, esp
	push	2047		;buffer size
	push	ebx
	push	dword [eax+ecx*4]
	push	dword [ebp+8]	;context
	call	_get_amxstring_r
	push	ebx		;push buffer
	lea	ebx, [ebp-44]
	push	ebx		;push format
	push	dword [ebp+28]	;push maxlen
	push	edi		;push output
	call	__snprintf
	add	esp, 4*8
	add	esp, 2048
	add	edi, eax
	jmp	.end
	
.fmt_num
	;check parameter count
	mov	ebx, [ebp+12]		;params
	mov	eax, [ebx]		;params[0]
	shr	eax, 2			;params[0]/4
	mov	edx, [ebp+16]		;param
	mov	ecx, [edx]		;*param
	cmp	ecx, eax		;*param / params[0]/4 ?
	ja	.error
	;get the param - it's in eax
	add	dword [edx], 1		;incr *param
	mov	edx, [ebp+8]		;get AMX into edx
	mov	edx, [edx]		;get AMX->base into edx
	mov	eax, [edx+16]		;get base->dat into eax
	add	edx, eax		;add dat to base
	add	edx, dword [ebx+ecx*4]	;add params[ecx]
	push	dword [edx]
	lea	ebx, [ebp-44]
	push	ebx
	push	dword [ebp+28]
	push	edi
	call	__snprintf
	add	esp, 4*4
	add	edi, eax
	jmp	.end
	
.fmt_float
	;check parameter count
	mov	ebx, [ebp+12]	;params
	mov	eax, [ebx]	;params[0]
	shr	eax, 2		;params[0]/4
	mov	edx, [ebp+16]	;param
	mov	ecx, [edx]	;*param
	cmp	ecx, eax	;*param / params[0]/4 ?
	ja	.error
	;get the param - it's in eax
	add	dword [edx], 1
	mov	edx, [ebp+8]
	mov	edx, [edx]
	mov	eax, [edx+16]
	add	edx, eax
	add	edx, dword [ebx+ecx*4]
	;load the float, convert to double
	fld	dword [edx]
	sub	esp, 8
	fstp	qword [esp]
	;it's already on the stack now, push rest
	lea	ebx, [ebp-44]
	push	ebx
	push	dword [ebp+28]
	push	edi
	call	__snprintf
	add	esp, 4*5
	add	edi, eax
	jmp	.end
	
.fmt_ml
	mov	ebx, [ebp+12]	;params
	mov	eax, [ebx]	;params[0]
	shr	eax, 2		;params[0]/4
	mov	edx, [ebp+16]	;param
	mov	ecx, [edx]	;*param
	inc	ecx
	cmp	ecx, eax	;*param / params[0]/4 ?
	ja	.error
	add	dword [edx], 2
	push	ecx
	push	dword 0		;NULL
	push	dword 3		;buffer 3
	push	dword [ebx+ecx*4]
	push	dword [ebp+8]
	call	_MNF_GetAmxString
	add	esp, 4*4
	pop	ecx
	dec	ecx
	push	eax
	push	eax			;key
	push	dword [ebx+ecx*4]	;lang_addr
	push	dword [ebp+8]		;AMX
	call	_translate
	add	esp, 4*3
	pop	ecx
	test	eax, eax
	je	.fmt_error
	
	;invoke the translator
	;;store this on the stack so we can pass the address
	push	eax
	mov	edx, esp
	push	1			;no reparse ^
	push	dword [ebp+20]		;maxlen
	push	edi			;output
	push	edx			;lexptr
	push	dword [ebp+16]		;param
	push	ebx			;params
	push	dword [ebp+8]		;amx
	call	do_amx_format
	add	esp, 4*8
	;we don't care about the return lex
	add	edi, eax
	jmp	.end
	
	
.fmt_error
	push	ecx
	lea	eax, [g_mlfmt]
	push	eax
	push	dword [ebp+20]
	push 	edi
	call	__snprintf
	add	esp, 4*4
	add	edi, eax
	jmp	.end
	
	
.fmt_default
	mov	esi, edx
	;store the % at least
	mov	[edi], byte '%'
	inc 	edi
	mov	eax, 1
	jmp	.end	

.error
	push	ecx
	push	eax
	push	g_errfmt
	push	10	;AMX_ERR_NATIVE
	push	dword [ebp+8]
	call 	_LogError
	add	esp, 4*5

.abort
	xor	eax, eax

.end
	add	esp, 32
	pop	ecx
	pop	ebx
	pop	ecx
	
	pop	ebp
	ret

;size_t do_amx_format(AMX *amx, cell *params, int *param, const char **lex, char *output, size_t maxlen, int level)
;REGISTER USAGE - 
;    esi=lex
;    edi=output
;    ecx=maxlen
;    eax=scratch
do_amx_format:
_do_amx_format:
	push	esi				;input
	push	edi				;output
	
	;current esp offset is 12 (0=edi,4=esi,8=ret)
	mov	esi, [esp+24]	;lex (dbl addr)
	mov	esi, [esi]
	mov	edi, [esp+28]	;get output
	mov	ecx, [esp+32]	;get maxlen

	;initial checks
	mov	al, [esi]
	test	al, al
	jz	.done
.loop:
	test	ecx, ecx
	jz 	.done
	cmp	al, '%'
	je	.perc
	cmp	al, '^'
	je	.esc
	
.copy:
	;*output++ = *lexptr++
	mov	[edi], al
	inc	esi
	inc	edi
	dec	ecx
.next
	mov	al, [esi]
	test	al, al
	jnz	.loop
	jmp	.done

;we got a '^'
.esc:
	cmp	dword [esp+36], 0
	je	.copy
	inc	esi
	mov	al, [esi]
	cmp	al, 'n'
	je	.escn
	cmp	al, 't'
	je	.esct

;*outptr++ = *lexptr
	mov	[edi], al
	inc	edi

;lexptr++
;maxlen--
.escdone
	inc	esi
	dec	ecx
	jmp	.next

.escn
	;*outptr++ = '\n'
	mov	[edi], byte 0xA	;'\n'
	inc	edi
	jmp	.escdone
	
.esct
	;*outptr++ = '\t'
	mov	[edi], byte 0x9	;'\t'
	inc	edi
	jmp	.escdone

;we got a '%'
.perc:
	inc	esi
	mov	al, [esi]	
	test	al, al		;'\0'
	je	.percatend
	cmp	al, '%'	
	je	.percnone
	jmp	.percfmt
.percatend
	dec	esi
	add	ecx, 1
.percnone:
	;*outptr++=*lexptr++; x2
	;maxlen -= 2
	;note we recalculate eax and then
	;only move once, since this is a 2byte move anyway.
	mov	ax, [esi]
	mov	[edi], ax
	add	esi, 2
	add	edi, 2
	sub	ecx, 2
	jmp	.next
	
.percfmt:
	;call do_amx_format_parameter.
	push	ecx
	push	ecx		;maxlen
	push	dword [esp+28]	;param
	push	dword [esp+28]	;params
	push	dword [esp+28]	;amx
	call	format_parameter	;will return edi adjusted for us
	add	esp, 4*4	;will also return esi adjusted for us
	pop	ecx
	sub	ecx, eax	;adjust maxlength
	mov	al, [esi]	;reiterate
	test	al, al
	jnz	.loop
	
.done:
	;end the string
	mov	[edi], dword 0
	mov	edi, [esp+24]	;get lexptr ref
	mov	[edi], esi	;sto into lexptr ref
	mov	eax, [esp+32]	;get maxlen
	sub	eax, ecx	;subtract what we did
	
	pop	edi
	pop	esi
	
	ret


section .data
	align 16
	g_errfmt	db "String formatted incorrectly - parameter %d (total %d)", 0
	g_mlfmt		db "ML_NOTFOUND: %s", 0
	;Stores whether a character is a letter or not.  hAxXx
	g_chartbl	times  65 dd 0
			times  26 dd 1
			times   6 dd 0
			times  26 dd 1
			times 133 dd 0
					
	g_jumptbl	times 256 dd format_parameter.fmt_default
					
;end

